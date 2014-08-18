/*
 * Copyright © 2007, 2013 Red Hat, Inc.
 * Copyright © 2008 Maarten Maathuis
 * Copyright © 2014 Tomasz Figa
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Authors:
 *    Dave Airlie <airlied@redhat.com>
 *
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "xorgVersion.h"

#include "of.h"
#include "xf86Crtc.h"
#include "xf86drmMode.h"
#include "drm_fourcc.h"
#include "xf86DDC.h"
#include "xf86Cursor.h"
#include "xf86RandR12.h"
#include "X11/Xatom.h"

#include "vbe.h"
#include "dixstruct.h"
#include "scrnintstr.h"
#include "fb.h"
#include "xf86cmap.h"
#include "shadowfb.h"

//#include "xf86xv.h"
//#include <X11/extensions/Xv.h>

#include "region.h"

#include <X11/extensions/randr.h>

#ifdef HAVE_XEXTPROTO_71
#include <X11/extensions/dpmsconst.h>
#else
#define DPMS_SERVER
#include <X11/extensions/dpms.h>
#endif

#include <sys/ioctl.h>
#include <errno.h>
#include <libudev.h>


static Bool drmmode_xf86crtc_resize(ScrnInfoPtr pScrn, int width, int height);

typedef struct {
	/* hardware cursor: */
	struct fd_bo *bo;
	int x, y;
	 /* These are used for HWCURSOR_API_PLANE */
	drmModePlane *ovr;
	uint32_t fb_id;
	/* This is used for HWCURSOR_API_STANDARD */
	uint32_t handle;
} drmmode_cursor_rec, *drmmode_cursor_ptr;

typedef struct {
	int fd;
	uint32_t fb_id;
	drmModeResPtr mode_res;
	int cpp;
	drmEventContext event_context;
	drmmode_cursor_ptr cursor;
#ifdef HAVE_LIBUDEV
	struct udev_monitor *uevent_monitor;
#endif
} drmmode_rec, *drmmode_ptr;

typedef struct {
	drmmode_ptr drmmode;
	drmModeCrtcPtr mode_crtc;
	struct fd_bo *rotate_bo;
	int rotate_pitch;
	PixmapPtr rotate_pixmap;
	uint32_t rotate_fb_id;
	Bool cursor_visible;
} drmmode_crtc_private_rec, *drmmode_crtc_private_ptr;

typedef struct {
	drmModePropertyPtr mode_prop;
	int index; /* Index within the kernel-side property arrays for
		    * this connector. */
	int num_atoms; /* if range prop, num_atoms == 1; if enum prop,
			* num_atoms == num_enums + 1 */
	Atom *atoms;
} drmmode_prop_rec, *drmmode_prop_ptr;

typedef struct {
	drmmode_ptr drmmode;
	int output_id;
	drmModeConnectorPtr mode_output;
	drmModeEncoderPtr mode_encoder;
	drmModePropertyBlobPtr edid_blob;
	int num_props;
	drmmode_prop_ptr props;
} drmmode_output_private_rec, *drmmode_output_private_ptr;

typedef struct {
	drmmode_ptr drmmode;
	unsigned old_fb_id;
	int flip_count;
	void *event_data;
	unsigned int fe_frame;
	unsigned int fe_tv_sec;
	unsigned int fe_tv_usec;
} drmmode_flipdata_rec, *drmmode_flipdata_ptr;

typedef struct {
	drmmode_flipdata_ptr flipdata;
	Bool dispatch_me;
} drmmode_flipevtcarrier_rec, *drmmode_flipevtcarrier_ptr;

static void drmmode_output_dpms(xf86OutputPtr output, int mode);

static drmmode_ptr
drmmode_from_scrn(ScrnInfoPtr scrn)
{
	if (scrn) {
		xf86CrtcConfigPtr conf = XF86_CRTC_CONFIG_PTR(scrn);
		drmmode_crtc_private_ptr crtc = conf->crtc[0]->driver_private;

		return crtc->drmmode;
	}

	return NULL;
}

static PixmapPtr
drmmode_pixmap_wrap(ScreenPtr pScreen, int width, int height, int depth,
		int bpp, int pitch, struct fd_bo *bo, void *data)
{
	OFPtr pOf = OFPTR_FROM_SCREEN(pScreen);
	PixmapPtr ppix;

	if (!pOf->NoAccel)
		data = NULL;

	ppix = pScreen->CreatePixmap(pScreen, 0, 0, depth, 0);
	if (!ppix)
		return NULL;

	pScreen->ModifyPixmapHeader(ppix, width, height, depth, bpp,
			pitch, data);
	of_set_pixmap_bo(ppix, bo);

	return ppix;
}

static void
drmmode_ConvertFromKMode(ScrnInfoPtr scrn, drmModeModeInfo *kmode,
		DisplayModePtr	mode)
{
	memset(mode, 0, sizeof(DisplayModeRec));
	mode->status = MODE_OK;

	mode->Clock = kmode->clock;

	mode->HDisplay = kmode->hdisplay;
	mode->HSyncStart = kmode->hsync_start;
	mode->HSyncEnd = kmode->hsync_end;
	mode->HTotal = kmode->htotal;
	mode->HSkew = kmode->hskew;

	mode->VDisplay = kmode->vdisplay;
	mode->VSyncStart = kmode->vsync_start;
	mode->VSyncEnd = kmode->vsync_end;
	mode->VTotal = kmode->vtotal;
	mode->VScan = kmode->vscan;

	mode->Flags = kmode->flags; //& FLAG_BITS;
	mode->name = strdup(kmode->name);

	if (kmode->type & DRM_MODE_TYPE_DRIVER)
		mode->type = M_T_DRIVER;
	if (kmode->type & DRM_MODE_TYPE_PREFERRED)
		mode->type |= M_T_PREFERRED;
	xf86SetModeCrtc (mode, scrn->adjustFlags);
}

static void
drmmode_ConvertToKMode(ScrnInfoPtr scrn, drmModeModeInfo *kmode,
		DisplayModePtr mode)
{
	memset(kmode, 0, sizeof(*kmode));

	kmode->clock = mode->Clock;
	kmode->hdisplay = mode->HDisplay;
	kmode->hsync_start = mode->HSyncStart;
	kmode->hsync_end = mode->HSyncEnd;
	kmode->htotal = mode->HTotal;
	kmode->hskew = mode->HSkew;

	kmode->vdisplay = mode->VDisplay;
	kmode->vsync_start = mode->VSyncStart;
	kmode->vsync_end = mode->VSyncEnd;
	kmode->vtotal = mode->VTotal;
	kmode->vscan = mode->VScan;

	kmode->flags = mode->Flags; //& FLAG_BITS;
	if (mode->name)
		strncpy(kmode->name, mode->name, DRM_DISPLAY_MODE_LEN);
	kmode->name[DRM_DISPLAY_MODE_LEN-1] = 0;

}

static void
drmmode_crtc_dpms(xf86CrtcPtr drmmode_crtc, int mode)
{

}

static Bool
drmmode_set_mode_major(xf86CrtcPtr crtc, DisplayModePtr mode,
		Rotation rotation, int x, int y)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	OFPtr pOf = OFPTR(pScrn);
	xf86CrtcConfigPtr   xf86_config = XF86_CRTC_CONFIG_PTR(crtc->scrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	uint32_t *output_ids;
	int output_count = 0;
	int ret = TRUE;
	int i;
	int fb_id;
	drmModeModeInfo kmode;

	if (drmmode->fb_id == 0)
		if (!drmmode_xf86crtc_resize(pScrn, mode->HDisplay, mode->VDisplay))
			return FALSE;

	if (!xf86CrtcRotate(crtc))
		return FALSE;

	output_ids = calloc(sizeof(uint32_t), xf86_config->num_output);
	if (!output_ids)
		return FALSE;

	for (i = 0; i < xf86_config->num_output; i++) {
		xf86OutputPtr output = xf86_config->output[i];
		drmmode_output_private_ptr drmmode_output;

		if (output->crtc != crtc)
			continue;

		drmmode_output = output->driver_private;
		output_ids[output_count] =
				drmmode_output->mode_output->connector_id;
		output_count++;
	}

	drmmode_ConvertToKMode(crtc->scrn, &kmode, mode);

	fb_id = drmmode->fb_id;
	if (drmmode_crtc->rotate_fb_id) {
		fb_id = drmmode_crtc->rotate_fb_id;
		x = 0;
		y = 0;
	}

	ret = drmModeSetCrtc(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id,
			fb_id, x, y, output_ids, output_count, &kmode);
	free(output_ids);

	if (ret) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				"failed to set mode: %s\n", strerror(-ret));
		return FALSE;
	}

	/* Work around some xserver stupidity */
	for (i = 0; i < xf86_config->num_output; i++) {
		xf86OutputPtr output = xf86_config->output[i];

		if (output->crtc != crtc)
			continue;

		drmmode_output_dpms(output, DPMSModeOn);
	}

	crtc->funcs->gamma_set(crtc, crtc->gamma_red, crtc->gamma_green,
			crtc->gamma_blue, crtc->gamma_size);

	if (pOf->HWCursor)
		xf86_reload_cursors(crtc->scrn->pScreen);

	return TRUE;
}

static void
drmmode_hide_cursor(xf86CrtcPtr crtc)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	drmmode_cursor_ptr cursor = drmmode->cursor;
	drmModeCrtcPtr mode_crtc = drmmode_crtc->mode_crtc;

	if (!cursor)
		return;

	drmmode_crtc->cursor_visible = FALSE;

	/* set plane's fb_id to 0 to disable it */
	drmModeSetPlane(drmmode->fd, cursor->ovr->plane_id,
			mode_crtc->crtc_id, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
}

#define OF_CURSOR_WIDTH		64
#define OF_CURSOR_HEIGHT	64
#define OF_CURSOR_PADDING	16

/*
 * The argument "update_image" controls whether the cursor image needs
 * to be updated by the HW or not. This is ignored by HWCURSOR_API_PLANE
 * which doesn't allow changing the cursor possition without updating
 * the image too.
 */
static void
drmmode_show_cursor_image(xf86CrtcPtr crtc, Bool update_image)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	drmmode_cursor_ptr cursor = drmmode->cursor;
	drmModeCrtcPtr mode_crtc = drmmode_crtc->mode_crtc;
	int crtc_x, crtc_y, src_x, src_y;
	int w, h, pad;

	if (!cursor)
		return;

	drmmode_crtc->cursor_visible = TRUE;

	w = OF_CURSOR_WIDTH;
	h = OF_CURSOR_HEIGHT;
	pad = OF_CURSOR_PADDING;

	/* get padded width */
	w = w + 2 * pad;
	/* get x of padded cursor */
	crtc_x = cursor->x - pad;
	crtc_y = cursor->y;

	src_x = 0;
	src_y = 0;

	/* calculate clipped x, y, w & h if cursor is off edges */
	if (crtc_x < 0) {
		src_x += -crtc_x;
		w -= -crtc_x;
		crtc_x = 0;
	}

	if (crtc_y < 0) {
		src_y += -crtc_y;
		h -= -crtc_y;
		crtc_y = 0;
	}

	if ((crtc_x + w) > crtc->mode.HDisplay)
		w = crtc->mode.HDisplay - crtc_x;

	if ((crtc_y + h) > crtc->mode.VDisplay)
		h = crtc->mode.VDisplay - crtc_y;

	/* note src coords (last 4 args) are in Q16 format */
	drmModeSetPlane(drmmode->fd, cursor->ovr->plane_id,
		mode_crtc->crtc_id, cursor->fb_id, 0,
		crtc_x, crtc_y, w, h, src_x<<16, src_y<<16,
		w<<16, h<<16);
}

static void
drmmode_show_cursor(xf86CrtcPtr crtc)
{
	drmmode_show_cursor_image(crtc, TRUE);
}

static void
drmmode_set_cursor_position(xf86CrtcPtr crtc, int x, int y)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	drmmode_cursor_ptr cursor = drmmode->cursor;

	if (!cursor)
		return;

	cursor->x = x;
	cursor->y = y;

	/*
	 * Show the cursor at a different possition without updating the image
	 * when possible (HWCURSOR_API_PLANE doesn't have a way to update
	 * cursor position without updating the image too).
	 */
	drmmode_show_cursor_image(crtc, FALSE);
}

/*
 * The cursor format is ARGB so the image can be copied straight over.
 * Columns of CURSORPAD blank pixels are maintained down either side
 * of the destination image. This is a workaround for a bug causing
 * corruption when the cursor reaches the screen edges in some DRM
 * drivers.
 */
static void set_cursor_image(xf86CrtcPtr crtc, uint32_t *d, CARD32 *s)
{
	int row;
	void *dst;
	const char *src_row;
	char *dst_row;
	uint32_t cursorh = OF_CURSOR_HEIGHT;
	uint32_t cursorw = OF_CURSOR_WIDTH;
	uint32_t cursorpad = OF_CURSOR_PADDING;

	dst = d;
	for (row = 0; row < cursorh; row += 1) {
		/* we're operating with ARGB data (4 bytes per pixel) */
		src_row = (const char *)s + row * 4 * cursorw;
		dst_row = (char *)dst + row * 4 * (cursorw + 2 * cursorpad);

		/* set first CURSORPAD pixels in row to 0 */
		memset(dst_row, 0, (4 * cursorpad));
		/* copy cursor image pixel row across */
		memcpy(dst_row + (4 * cursorpad), src_row, 4 * cursorw);
		/* set last CURSORPAD pixels in row to 0 */
		memset(dst_row + 4 * (cursorpad + cursorw),
				0, (4 * cursorpad));
	}
}

static void
drmmode_load_cursor_argb(xf86CrtcPtr crtc, CARD32 *image)
{
	uint32_t *d;
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	drmmode_cursor_ptr cursor = drmmode->cursor;
	int visible;

	if (!cursor)
		return;

	visible = drmmode_crtc->cursor_visible;

	if (visible)
		drmmode_hide_cursor(crtc);

	d = fd_bo_map(cursor->bo);
	if (!d) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
			"load_cursor_argb map failure\n");
		if (visible)
			drmmode_show_cursor_image(crtc, TRUE);
		return;
	}

	set_cursor_image(crtc, d, image);

	if (visible)
		drmmode_show_cursor_image(crtc, TRUE);
}

struct drm_exynos_plane_set_zpos {
	__u32 plane_id;
	__s32 zpos;
};
#define DRM_EXYNOS_PLANE_SET_ZPOS 0x06
#define DRM_IOCTL_EXYNOS_PLANE_SET_ZPOS DRM_IOWR(DRM_COMMAND_BASE + \
		DRM_EXYNOS_PLANE_SET_ZPOS, struct drm_exynos_plane_set_zpos)

static int init_plane_for_cursor(int drm_fd, uint32_t plane_id)
{
	int res = -1;
	drmModeObjectPropertiesPtr props;
	props = drmModeObjectGetProperties(drm_fd, plane_id,
			DRM_MODE_OBJECT_PLANE);

	if (props) {
		int i;

		for (i = 0; i < props->count_props; i++) {
			drmModePropertyPtr this_prop;
			this_prop = drmModeGetProperty(drm_fd, props->props[i]);

			if (this_prop) {
				if (!strncmp(this_prop->name, "zpos",
							DRM_PROP_NAME_LEN)) {
					res = drmModeObjectSetProperty(drm_fd,
							plane_id,
							DRM_MODE_OBJECT_PLANE,
							this_prop->prop_id,
							1);
					drmModeFreeProperty(this_prop);
					break;
				}
				drmModeFreeProperty(this_prop);
			}
		}
		drmModeFreeObjectProperties(props);
	}

	if (res) {
		/* Try the old method */
		struct drm_exynos_plane_set_zpos data;
		data.plane_id = plane_id;
		data.zpos = 1;

		res = ioctl(drm_fd, DRM_IOCTL_EXYNOS_PLANE_SET_ZPOS, &data);
	}

	return res;
}

Bool
drmmode_cursor_init(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	drmmode_ptr drmmode = drmmode_from_scrn(pScrn);
	drmmode_cursor_ptr cursor;
	drmModePlaneRes *plane_resources;
	drmModePlane *ovr;
	int w, h, pad;
	uint32_t handles[4], pitches[4], offsets[4]; /* we only use [0] */

	INFO_MSG("HW cursor init()");

	if (drmmode->cursor) {
		INFO_MSG("cursor already initialized");
		return TRUE;
	}

	if (!xf86LoaderCheckSymbol("drmModeGetPlaneResources")) {
		ERROR_MSG(
				"HW cursor not supported (needs libdrm 2.4.30 or higher)");
		return FALSE;
	}

	/* find an unused plane which can be used as a mouse cursor.  Note
	 * that we cheat a bit, in order to not burn one overlay per crtc,
	 * and only show the mouse cursor on one crtc at a time
	 */
	plane_resources = drmModeGetPlaneResources(drmmode->fd);
	if (!plane_resources) {
		ERROR_MSG("HW cursor: drmModeGetPlaneResources failed: %s",
						strerror(errno));
		return FALSE;
	}

	if (plane_resources->count_planes < 1) {
		ERROR_MSG("not enough planes for HW cursor");
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	ovr = drmModeGetPlane(drmmode->fd, plane_resources->planes[0]);
	if (!ovr) {
		ERROR_MSG("HW cursor: drmModeGetPlane failed: %s",
					strerror(errno));
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	if (init_plane_for_cursor(drmmode->fd, ovr->plane_id)) {
		ERROR_MSG("Failed driver-specific cursor initialization");
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	cursor = calloc(1, sizeof(drmmode_cursor_rec));
	if (!cursor) {
		ERROR_MSG("HW cursor: calloc failed");
		drmModeFreePlane(ovr);
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	cursor->ovr = ovr;

	w = OF_CURSOR_WIDTH;
	h = OF_CURSOR_HEIGHT;
	pad = OF_CURSOR_PADDING;

	/* allow for cursor padding in the bo */
	cursor->bo  = fd_bo_new(pOf->dev, 4 * (w + 2 * pad) * h, 0);

	if (!cursor->bo) {
		ERROR_MSG("HW cursor: buffer allocation failed");
		free(cursor);
		drmModeFreePlane(ovr);
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	handles[0] = fd_bo_handle(cursor->bo);
	pitches[0] = (32 * (w + 2 * pad) / 8);
	offsets[0] = 0;

	/* allow for cursor padding in the fb */
	if (drmModeAddFB2(drmmode->fd, w + 2 * pad, h, DRM_FORMAT_ARGB8888,
			handles, pitches, offsets, &cursor->fb_id, 0)) {
		ERROR_MSG("HW cursor: drmModeAddFB2 failed: %s",
					strerror(errno));
		fd_bo_del(cursor->bo);
		free(cursor);
		drmModeFreePlane(ovr);
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	if (!xf86_cursors_init(pScreen, w, h, HARDWARE_CURSOR_ARGB)) {
		ERROR_MSG("xf86_cursors_init() failed");
		if (drmModeRmFB(drmmode->fd, cursor->fb_id))
			ERROR_MSG("drmModeRmFB() failed");

		fd_bo_del(cursor->bo);
		free(cursor);
		drmModeFreePlane(ovr);
		drmModeFreePlaneResources(plane_resources);
		return FALSE;
	}

	INFO_MSG("HW cursor initialized");
	drmmode->cursor = cursor;
	drmModeFreePlaneResources(plane_resources);
	return TRUE;
}

void drmmode_cursor_fini(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	drmmode_ptr drmmode = drmmode_from_scrn(pScrn);
	drmmode_cursor_ptr cursor = drmmode->cursor;

	if (!cursor)
		return;

	drmmode->cursor = NULL;
	xf86_cursors_fini(pScreen);
	drmModeRmFB(drmmode->fd, cursor->fb_id);
	fd_bo_del(cursor->bo);
	drmModeFreePlane(cursor->ovr);
	free(cursor);
}

static void *
drmmode_crtc_shadow_allocate(xf86CrtcPtr crtc, int width, int height)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	OFPtr pOf = OFPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	int ret, pitch, size;
	void *ptr;

	pitch = OFAlignedStride(width, 32);
	size = pitch * height;

	drmmode_crtc->rotate_pitch = pitch;
	drmmode_crtc->rotate_bo = fd_bo_new(pOf->dev, size,
			DRM_FREEDRENO_GEM_TYPE_KMEM);
	if (!drmmode_crtc->rotate_bo) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				"Couldn't allocate shadow memory for rotated CRTC\n");
		return NULL;
	}

	ptr = fd_bo_map(drmmode_crtc->rotate_bo);
	if (!ptr) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				"Couldn't get virtual address of shadow scanout\n");
		fd_bo_del(drmmode_crtc->rotate_bo);
		drmmode_crtc->rotate_bo = NULL;
		return NULL;
	}

	ret = drmModeAddFB(drmmode->fd, width, height, crtc->scrn->depth,
			crtc->scrn->bitsPerPixel, drmmode_crtc->rotate_pitch,
			fd_bo_handle(drmmode_crtc->rotate_bo),
			&drmmode_crtc->rotate_fb_id);
	if (ret) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				"Error adding FB for shadow scanout: %s\n",
				strerror(-ret));
		fd_bo_del(drmmode_crtc->rotate_bo);
		drmmode_crtc->rotate_bo = NULL;
		return NULL;
	}

	return ptr;
}

static PixmapPtr
drmmode_crtc_shadow_create(xf86CrtcPtr crtc, void *data, int width, int height)
{
	ScrnInfoPtr pScrn = crtc->scrn;
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	PixmapPtr rotate_pixmap;

	if (!data)
		data = drmmode_crtc_shadow_allocate (crtc, width, height);

	rotate_pixmap = drmmode_pixmap_wrap(pScrn->pScreen, width, height,
			pScrn->depth, pScrn->bitsPerPixel,
			drmmode_crtc->rotate_pitch,
			drmmode_crtc->rotate_bo, data);

	drmmode_crtc->rotate_pixmap = rotate_pixmap;
	return drmmode_crtc->rotate_pixmap;
}

static void
drmmode_crtc_shadow_destroy(xf86CrtcPtr crtc, PixmapPtr rotate_pixmap, void *data)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;

	if (rotate_pixmap)
		FreeScratchPixmapHeader(rotate_pixmap);

	if (data) {
		drmModeRmFB(drmmode->fd, drmmode_crtc->rotate_fb_id);
		drmmode_crtc->rotate_fb_id = 0;
		fd_bo_del(drmmode_crtc->rotate_bo);
		drmmode_crtc->rotate_bo = NULL;
		drmmode_crtc->rotate_pixmap = NULL;
	}
}

static void
drmmode_gamma_set(xf86CrtcPtr crtc, CARD16 *red, CARD16 *green, CARD16 *blue,
		int size)
{
	drmmode_crtc_private_ptr drmmode_crtc = crtc->driver_private;
	drmmode_ptr drmmode = drmmode_crtc->drmmode;
	int ret;

	ret = drmModeCrtcSetGamma(drmmode->fd, drmmode_crtc->mode_crtc->crtc_id,
			size, red, green, blue);
	if (ret != 0) {
		xf86DrvMsg(crtc->scrn->scrnIndex, X_ERROR,
				"failed to set gamma: %s\n", strerror(-ret));
	}
}

static const xf86CrtcFuncsRec drmmode_crtc_funcs = {
		.dpms = drmmode_crtc_dpms,
		.set_mode_major = drmmode_set_mode_major,
		.set_cursor_position = drmmode_set_cursor_position,
		.show_cursor = drmmode_show_cursor,
		.hide_cursor = drmmode_hide_cursor,
		.load_cursor_argb = drmmode_load_cursor_argb,
		.shadow_create = drmmode_crtc_shadow_create,
		.shadow_allocate = drmmode_crtc_shadow_allocate,
		.shadow_destroy = drmmode_crtc_shadow_destroy,
		.gamma_set = drmmode_gamma_set,
};


static void
drmmode_crtc_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int num)
{
	xf86CrtcPtr crtc;
	drmmode_crtc_private_ptr drmmode_crtc;

	crtc = xf86CrtcCreate(pScrn, &drmmode_crtc_funcs);
	if (crtc == NULL)
		return;

	drmmode_crtc = xnfcalloc(sizeof(drmmode_crtc_private_rec), 1);
	drmmode_crtc->mode_crtc = drmModeGetCrtc(drmmode->fd,
			drmmode->mode_res->crtcs[num]);
	drmmode_crtc->drmmode = drmmode;

	crtc->driver_private = drmmode_crtc;

	return;
}

static xf86OutputStatus
drmmode_output_detect(xf86OutputPtr output)
{
	/* go to the hw and retrieve a new output struct */
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	xf86OutputStatus status;
	drmModeFreeConnector(drmmode_output->mode_output);

	drmmode_output->mode_output =
			drmModeGetConnector(drmmode->fd, drmmode_output->output_id);

	if (!drmmode_output->mode_output)
		return XF86OutputStatusDisconnected;

	switch (drmmode_output->mode_output->connection) {
	case DRM_MODE_CONNECTED:
		status = XF86OutputStatusConnected;
		break;
	case DRM_MODE_DISCONNECTED:
		status = XF86OutputStatusDisconnected;
		break;
	default:
	case DRM_MODE_UNKNOWNCONNECTION:
		status = XF86OutputStatusUnknown;
		break;
	}
	return status;
}

static Bool
drmmode_output_mode_valid(xf86OutputPtr output, DisplayModePtr mode)
{
	if (mode->type & M_T_DEFAULT)
		/* Default modes are harmful here. */
		return MODE_BAD;

	return MODE_OK;
}

static DisplayModePtr
drmmode_output_get_modes(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr koutput = drmmode_output->mode_output;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	int i;
	DisplayModePtr Modes = NULL, Mode;
	drmModePropertyPtr props;
	xf86MonPtr ddc_mon = NULL;

	if (!koutput)
		return NULL;

	/* look for an EDID property */
	for (i = 0; i < koutput->count_props; i++) {
		props = drmModeGetProperty(drmmode->fd, koutput->props[i]);
		if (!props || !(props->flags & DRM_MODE_PROP_BLOB))
			continue;

		if (!strcmp(props->name, "EDID")) {
			if (drmmode_output->edid_blob)
				drmModeFreePropertyBlob(drmmode_output->edid_blob);
			drmmode_output->edid_blob =
					drmModeGetPropertyBlob(drmmode->fd,
							koutput->prop_values[i]);
		}
		drmModeFreeProperty(props);
	}

	if (drmmode_output->edid_blob) {
		ddc_mon = xf86InterpretEDID(output->scrn->scrnIndex,
				drmmode_output->edid_blob->data);
		if (ddc_mon && drmmode_output->edid_blob->length > 128)
			ddc_mon->flags |= MONITOR_EDID_COMPLETE_RAWDATA;
	}
	xf86OutputSetEDID(output, ddc_mon);

	/* modes should already be available */
	for (i = 0; i < koutput->count_modes; i++) {
		Mode = xnfalloc(sizeof(DisplayModeRec));

		drmmode_ConvertFromKMode(output->scrn, &koutput->modes[i],
				Mode);
		Modes = xf86ModesAdd(Modes, Mode);

	}
	return Modes;
}

static void
drmmode_output_destroy(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	int i;

	if (drmmode_output->edid_blob)
		drmModeFreePropertyBlob(drmmode_output->edid_blob);
	for (i = 0; i < drmmode_output->num_props; i++) {
		drmModeFreeProperty(drmmode_output->props[i].mode_prop);
		free(drmmode_output->props[i].atoms);
	}
	drmModeFreeConnector(drmmode_output->mode_output);
	free(drmmode_output);
	output->driver_private = NULL;
}

static void
drmmode_output_dpms(xf86OutputPtr output, int mode)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr koutput = drmmode_output->mode_output;
	drmModePropertyPtr props;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	int mode_id = -1, i;

	for (i = 0; i < koutput->count_props; i++) {
		props = drmModeGetProperty(drmmode->fd, koutput->props[i]);
		if (props && (props->flags && DRM_MODE_PROP_ENUM)) {
			if (!strcmp(props->name, "DPMS")) {
				mode_id = koutput->props[i];
				drmModeFreeProperty(props);
				break;
			}
			drmModeFreeProperty(props);
		}
	}

	if (mode_id < 0)
		return;

	drmModeConnectorSetProperty(drmmode->fd, koutput->connector_id,
			mode_id, mode);
}

static Bool
drmmode_property_ignore(drmModePropertyPtr prop)
{
	if (!prop)
		return TRUE;
	/* ignore blob prop */
	if (prop->flags & DRM_MODE_PROP_BLOB)
		return TRUE;
	/* ignore standard property */
	if (!strcmp(prop->name, "EDID") ||
			!strcmp(prop->name, "DPMS"))
		return TRUE;

	return FALSE;
}

static void
drmmode_output_create_resources(xf86OutputPtr output)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmModeConnectorPtr mode_output = drmmode_output->mode_output;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	drmModePropertyPtr drmmode_prop;
	uint32_t value;
	int i, j, err;

	drmmode_output->props = calloc(mode_output->count_props, sizeof(drmmode_prop_rec));
	if (!drmmode_output->props)
		return;

	drmmode_output->num_props = 0;
	for (i = 0, j = 0; i < mode_output->count_props; i++) {
		drmmode_prop = drmModeGetProperty(drmmode->fd, mode_output->props[i]);
		if (drmmode_property_ignore(drmmode_prop)) {
			drmModeFreeProperty(drmmode_prop);
			continue;
		}
		drmmode_output->props[j].mode_prop = drmmode_prop;
		drmmode_output->props[j].index = i;
		drmmode_output->num_props++;
		j++;
	}

	for (i = 0; i < drmmode_output->num_props; i++) {
		drmmode_prop_ptr p = &drmmode_output->props[i];
		drmmode_prop = p->mode_prop;

		value = drmmode_output->mode_output->prop_values[p->index];

		if (drmmode_prop->flags & DRM_MODE_PROP_RANGE) {
			INT32 range[2];

			p->num_atoms = 1;
			p->atoms = calloc(p->num_atoms, sizeof(Atom));
			if (!p->atoms)
				continue;
			p->atoms[0] = MakeAtom(drmmode_prop->name, strlen(drmmode_prop->name), TRUE);
			range[0] = drmmode_prop->values[0];
			range[1] = drmmode_prop->values[1];
			err = RRConfigureOutputProperty(output->randr_output, p->atoms[0],
					FALSE, TRUE,
					drmmode_prop->flags & DRM_MODE_PROP_IMMUTABLE ? TRUE : FALSE,
							2, range);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
						"RRConfigureOutputProperty error, %d\n", err);
			}
			err = RRChangeOutputProperty(output->randr_output, p->atoms[0],
					XA_INTEGER, 32, PropModeReplace, 1,
					&value, FALSE, FALSE);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
						"RRChangeOutputProperty error, %d\n", err);
			}
		} else if (drmmode_prop->flags & DRM_MODE_PROP_ENUM) {
			p->num_atoms = drmmode_prop->count_enums + 1;
			p->atoms = calloc(p->num_atoms, sizeof(Atom));
			if (!p->atoms)
				continue;
			p->atoms[0] = MakeAtom(drmmode_prop->name, strlen(drmmode_prop->name), TRUE);
			for (j = 1; j <= drmmode_prop->count_enums; j++) {
				struct drm_mode_property_enum *e = &drmmode_prop->enums[j-1];
				p->atoms[j] = MakeAtom(e->name, strlen(e->name), TRUE);
			}
			err = RRConfigureOutputProperty(output->randr_output, p->atoms[0],
					FALSE, FALSE,
					drmmode_prop->flags & DRM_MODE_PROP_IMMUTABLE ? TRUE : FALSE,
							p->num_atoms - 1, (INT32 *)&p->atoms[1]);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
						"RRConfigureOutputProperty error, %d\n", err);
			}
			for (j = 0; j < drmmode_prop->count_enums; j++)
				if (drmmode_prop->enums[j].value == value)
					break;
			/* there's always a matching value */
			err = RRChangeOutputProperty(output->randr_output, p->atoms[0],
					XA_ATOM, 32, PropModeReplace, 1, &p->atoms[j+1], FALSE, FALSE);
			if (err != 0) {
				xf86DrvMsg(output->scrn->scrnIndex, X_ERROR,
						"RRChangeOutputProperty error, %d\n", err);
			}
		}
	}
}

static Bool
drmmode_output_set_property(xf86OutputPtr output, Atom property,
		RRPropertyValuePtr value)
{
	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	int i, ret;

	for (i = 0; i < drmmode_output->num_props; i++) {
		drmmode_prop_ptr p = &drmmode_output->props[i];

		if (p->atoms[0] != property)
			continue;

		if (p->mode_prop->flags & DRM_MODE_PROP_RANGE) {
			uint32_t val;

			if (value->type != XA_INTEGER || value->format != 32 ||
					value->size != 1)
				return FALSE;
			val = *(uint32_t *)value->data;

			ret = drmModeConnectorSetProperty(drmmode->fd, drmmode_output->output_id,
					p->mode_prop->prop_id, (uint64_t)val);

			if (ret)
				return FALSE;

			return TRUE;

		} else if (p->mode_prop->flags & DRM_MODE_PROP_ENUM) {
			Atom	atom;
			const char	*name;
			int		j;

			if (value->type != XA_ATOM || value->format != 32 || value->size != 1)
				return FALSE;
			memcpy(&atom, value->data, 4);
			name = NameForAtom(atom);

			/* search for matching name string, then set its value down */
			for (j = 0; j < p->mode_prop->count_enums; j++) {
				if (!strcmp(p->mode_prop->enums[j].name, name)) {
					ret = drmModeConnectorSetProperty(drmmode->fd,
							drmmode_output->output_id,
							p->mode_prop->prop_id,
							p->mode_prop->enums[j].value);

					if (ret)
						return FALSE;

					return TRUE;
				}
			}

			return FALSE;
		}
	}

	return TRUE;
}

static Bool
drmmode_output_get_property(xf86OutputPtr output, Atom property)
{

	drmmode_output_private_ptr drmmode_output = output->driver_private;
	drmmode_ptr drmmode = drmmode_output->drmmode;
	uint32_t value;
	int err, i;

	if (output->scrn->vtSema) {
		drmModeFreeConnector(drmmode_output->mode_output);
		drmmode_output->mode_output =
				drmModeGetConnector(drmmode->fd, drmmode_output->output_id);
	}

	if (!drmmode_output->mode_output)
		return FALSE;

	for (i = 0; i < drmmode_output->num_props; i++) {
		drmmode_prop_ptr p = &drmmode_output->props[i];
		if (p->atoms[0] != property)
			continue;

		value = drmmode_output->mode_output->prop_values[p->index];

		if (p->mode_prop->flags & DRM_MODE_PROP_RANGE) {
			err = RRChangeOutputProperty(output->randr_output,
					property, XA_INTEGER, 32,
					PropModeReplace, 1, &value,
					FALSE, FALSE);

			return !err;
		} else if (p->mode_prop->flags & DRM_MODE_PROP_ENUM) {
			int		j;

			/* search for matching name string, then set its value down */
			for (j = 0; j < p->mode_prop->count_enums; j++) {
				if (p->mode_prop->enums[j].value == value)
					break;
			}

			err = RRChangeOutputProperty(output->randr_output, property,
					XA_ATOM, 32, PropModeReplace, 1,
					&p->atoms[j+1], FALSE, FALSE);

			return !err;
		}
	}

	return FALSE;
}

static const xf86OutputFuncsRec drmmode_output_funcs = {
		.create_resources = drmmode_output_create_resources,
		.dpms = drmmode_output_dpms,
		.detect = drmmode_output_detect,
		.mode_valid = drmmode_output_mode_valid,
		.get_modes = drmmode_output_get_modes,
		.set_property = drmmode_output_set_property,
		.get_property = drmmode_output_get_property,
		.destroy = drmmode_output_destroy
};

static int subpixel_conv_table[7] = {
		0, SubPixelUnknown,
		SubPixelHorizontalRGB,
		SubPixelHorizontalBGR,
		SubPixelVerticalRGB,
		SubPixelVerticalBGR,
		SubPixelNone };

const char *output_names[] = {
		"None",
		"VGA",
		"DVI-I",
		"DVI-D",
		"DVI-A",
		"Composite",
		"SVIDEO",
		"LVDS",
		"CTV",
		"DIN",
		"DP",
		"HDMI",
		"HDMI",
		"TV",
		"eDP",
};
#define NUM_OUTPUT_NAMES (sizeof(output_names) / sizeof(output_names[0]))

static void
drmmode_output_init(ScrnInfoPtr pScrn, drmmode_ptr drmmode, int num)
{
	xf86OutputPtr output;
	drmModeConnectorPtr koutput;
	drmModeEncoderPtr kencoder;
	drmmode_output_private_ptr drmmode_output;
	char name[32];

	koutput = drmModeGetConnector(drmmode->fd,
			drmmode->mode_res->connectors[num]);
	if (!koutput)
		return;

	kencoder = drmModeGetEncoder(drmmode->fd, koutput->encoders[0]);
	if (!kencoder) {
		drmModeFreeConnector(koutput);
		return;
	}

	if (koutput->connector_type >= NUM_OUTPUT_NAMES)
		snprintf(name, 32, "Unknown%d-%d", koutput->connector_type,
				koutput->connector_type_id);
	else
		snprintf(name, 32, "%s-%d",
				output_names[koutput->connector_type],
				koutput->connector_type_id);

	output = xf86OutputCreate (pScrn, &drmmode_output_funcs, name);
	if (!output) {
		drmModeFreeEncoder(kencoder);
		drmModeFreeConnector(koutput);
		return;
	}

	drmmode_output = calloc(sizeof(drmmode_output_private_rec), 1);
	if (!drmmode_output) {
		xf86OutputDestroy(output);
		drmModeFreeConnector(koutput);
		drmModeFreeEncoder(kencoder);
		return;
	}

	drmmode_output->output_id = drmmode->mode_res->connectors[num];
	drmmode_output->mode_output = koutput;
	drmmode_output->mode_encoder = kencoder;
	drmmode_output->drmmode = drmmode;
	output->mm_width = koutput->mmWidth;
	output->mm_height = koutput->mmHeight;

	output->subpixel_order = subpixel_conv_table[koutput->subpixel];
	output->driver_private = drmmode_output;

	output->possible_crtcs = kencoder->possible_crtcs;
	output->possible_clones = kencoder->possible_clones;

	output->interlaceAllowed = TRUE;
	output->doubleScanAllowed = TRUE;
}

static Bool
drmmode_xf86crtc_resize(ScrnInfoPtr pScrn, int width, int height)
{
	xf86CrtcConfigPtr xf86_config = XF86_CRTC_CONFIG_PTR(pScrn);
	ScreenPtr screen = xf86ScrnToScreen(pScrn);
	OFPtr pOf = OFPTR(pScrn);
	drmmode_crtc_private_ptr drmmode_crtc = NULL;
	drmmode_ptr drmmode = NULL;
	uint32_t old_width, old_height, old_pitch, old_fb_id = 0;
	struct fd_bo *old_bo = NULL;
	int ret, i, pitch, size;
	PixmapPtr ppix;
	void *ptr;

	pitch = OFAlignedStride(width, 32);
	size = pitch * height;

	if (xf86_config->num_crtc) {
		drmmode_crtc = xf86_config->crtc[0]->driver_private;
		drmmode = drmmode_crtc->drmmode;
	}

	DEBUG_MSG("resize called %d %d\n", width, height);

	if ((pScrn->virtualX == width) &&
			(pScrn->virtualY == height) &&
			pOf->scanout)
		return TRUE;

	old_width = pScrn->virtualX;
	old_height = pScrn->virtualY;
	old_pitch = pScrn->displayWidth;
	if (drmmode)
		old_fb_id = drmmode->fb_id;
	old_bo = pOf->scanout;

	pOf->scanout = fd_bo_new(pOf->dev, size,
			DRM_FREEDRENO_GEM_TYPE_KMEM);

	if (!pOf->scanout)
		goto fail;

	pScrn->virtualX = width;
	pScrn->virtualY = height;
	pScrn->displayWidth = pitch / (pScrn->bitsPerPixel >> 3);

	ptr = fd_bo_map(pOf->scanout);

	if (drmmode) {
		ret = drmModeAddFB(drmmode->fd, width, height, pScrn->depth,
				pScrn->bitsPerPixel, pitch, fd_bo_handle(pOf->scanout),
				&drmmode->fb_id);
		if (ret)
			goto fail;
	}

	/* NOTE do everything that could fail before this point,
	 * otherwise you could end up w/ screen pixmap pointing
	 * at the wrong scanout bo
	 */
	ppix = screen->GetScreenPixmap(screen);
	if (ppix) {
		screen->ModifyPixmapHeader(ppix, width, height, -1, -1, pitch, ptr);
		of_set_pixmap_bo(ppix, pOf->scanout);
#if GET_ABI_MAJOR(ABI_VIDEODRV_VERSION) < 9
		pScrn->pixmapPrivate.ptr = ppix->devPrivate.ptr;
#endif
	}

	memset(ptr, 0x00, fd_bo_size(pOf->scanout));

	for (i = 0; i < xf86_config->num_crtc; i++) {
		xf86CrtcPtr crtc = xf86_config->crtc[i];

		if (!crtc->enabled)
			continue;

		drmmode_set_mode_major(crtc, &crtc->mode,
				crtc->rotation, crtc->x, crtc->y);
	}

	if (old_fb_id)
		drmModeRmFB(drmmode->fd, old_fb_id);
	if (old_bo)
		fd_bo_del(old_bo);

	return TRUE;

 fail:
	pOf->scanout = old_bo;
	pScrn->virtualX = old_width;
	pScrn->virtualY = old_height;
	pScrn->displayWidth = old_pitch;
	if (drmmode)
		drmmode->fb_id = old_fb_id;

	return FALSE;
}

static const xf86CrtcConfigFuncsRec drmmode_xf86crtc_config_funcs = {
		drmmode_xf86crtc_resize
};

Bool drmmode_pre_init(ScrnInfoPtr pScrn, int fd, int cpp)
{
	drmmode_ptr drmmode;
	int i;

	drmmode = xnfcalloc(sizeof(*drmmode), 1);
	drmmode->fd = fd;
	drmmode->fb_id = 0;

	xf86CrtcConfigInit(pScrn, &drmmode_xf86crtc_config_funcs);

	drmmode->cpp = cpp;
	drmmode->mode_res = drmModeGetResources(drmmode->fd);
	if (!drmmode->mode_res)
		return FALSE;

	xf86CrtcSetSizeRange(pScrn, 320, 200, drmmode->mode_res->max_width,
			drmmode->mode_res->max_height);

	if (!xf86SetDepthBpp(pScrn, 0, 0, 0,
			Support24bppFb | Support32bppFb |
			SupportConvert32to24 | SupportConvert24to32)) {
		ERROR_MSG("Unable to set bitdepth");
		free(drmmode);
		return FALSE;
	}

	if (!drmmode->mode_res->count_connectors ||
			!drmmode->mode_res->count_crtcs) {
		drmModeFreeResources(drmmode->mode_res);
		free(drmmode);
		goto done;
	}

	for (i = 0; i < drmmode->mode_res->count_crtcs; i++) {
		if (!xf86IsEntityShared(pScrn->entityList[0]) ||
				(pScrn->confScreen->device->screen == i))
			drmmode_crtc_init(pScrn, drmmode, i);
	}

	for (i = 0; i < drmmode->mode_res->count_connectors; i++)
		drmmode_output_init(pScrn, drmmode, i);

done:

	xf86InitialConfiguration(pScrn, TRUE);

	return TRUE;
}

void
drmmode_adjust_frame(ScrnInfoPtr scrn, int x, int y)
{
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(scrn);
	xf86OutputPtr output = config->output[config->compat_output];
	xf86CrtcPtr crtc = output->crtc;

	if (!crtc || !crtc->enabled)
		return;

	drmmode_set_mode_major(crtc, &crtc->mode, crtc->rotation, x, y);
}

void
drmmode_remove_fb(ScrnInfoPtr pScrn)
{
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(pScrn);
	xf86CrtcPtr crtc = NULL;
	drmmode_crtc_private_ptr drmmode_crtc;
	drmmode_ptr drmmode;

	if (config && config->num_crtc)
		crtc = config->crtc[0];
	if (!crtc)
		return;

	drmmode_crtc = crtc->driver_private;
	drmmode = drmmode_crtc->drmmode;

	if (drmmode->fb_id)
		drmModeRmFB(drmmode->fd, drmmode->fb_id);
	drmmode->fb_id = 0;
}

Bool
drmmode_page_flip(DrawablePtr draw, PixmapPtr back, void *priv)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(draw->pScreen);
	OFPtr pOf = OFPTR(pScrn);
	struct fd_bo *back_bo = of_get_pixmap_bo(back);
	xf86CrtcConfigPtr config = XF86_CRTC_CONFIG_PTR(pScrn);
	drmmode_crtc_private_ptr crtc = config->crtc[0]->driver_private;
	drmmode_ptr mode = crtc->drmmode;
	drmmode_flipdata_ptr flipdata;
	drmmode_flipevtcarrier_ptr flipcarrier;
	unsigned int ref_crtc_hw_id = 0;
	int ret, i, old_fb_id, emitted = 0;

	old_fb_id = mode->fb_id;
	ret = drmModeAddFB(mode->fd, pScrn->virtualX, pScrn->virtualY,
			pScrn->depth, pScrn->bitsPerPixel,
			pScrn->displayWidth * pScrn->bitsPerPixel / 8,
			fd_bo_handle(back_bo), &mode->fb_id);
	if (ret) {
		xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
				"add fb failed: %s\n", strerror(errno));
		return FALSE;
	}

	flipdata = calloc(1, sizeof(drmmode_flipdata_rec));
	if (!flipdata) {
		xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
				"flip queue: data alloc failed.\n");
		goto error_undo;
	}

	flipdata->event_data = priv;
	flipdata->drmmode = mode;

	for (i = 0; i < config->num_crtc; i++) {
		crtc = config->crtc[i]->driver_private;

		if (!config->crtc[i]->enabled)
			continue;

		if (ref_crtc_hw_id == 0)
			ref_crtc_hw_id = (1 << i);

		flipdata->flip_count++;

		flipcarrier = calloc(1, sizeof(drmmode_flipevtcarrier_rec));
		if (!flipcarrier) {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
					"flip queue: carrier alloc failed.\n");
			if (emitted == 0)
				free(flipdata);
			goto error_undo;
		}

		/* Only the reference crtc will finally deliver its page flip
		 * completion event. All other crtc's events will be discarded.
		 */
		flipcarrier->dispatch_me = ((1 << i) == ref_crtc_hw_id);
		flipcarrier->flipdata = flipdata;

		ret = drmModePageFlip(mode->fd, crtc->mode_crtc->crtc_id,
				mode->fb_id, DRM_MODE_PAGE_FLIP_EVENT,
				flipcarrier);
		if (ret) {
			xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
					"flip queue failed: %s\n", strerror(errno));

			free(flipcarrier);
			if (emitted == 0)
				free(flipdata);
			goto error_undo;
		}

		emitted++;
	}

	/* Will release old fb after all crtc's completed flip. */
	flipdata->old_fb_id = old_fb_id;

	pOf->scanout = back_bo;

	return TRUE;

error_undo:
	drmModeRmFB(mode->fd, mode->fb_id);
	mode->fb_id = old_fb_id;
	return FALSE;
}

#ifdef HAVE_LIBUDEV
static void
drmmode_handle_uevents(ScrnInfoPtr scrn)
{
	drmmode_ptr drmmode = drmmode_from_scrn(scrn);
	struct udev_device *dev;

	dev = udev_monitor_receive_device(drmmode->uevent_monitor);
	if (!dev)
		return;

	RRGetInfo(xf86ScrnToScreen(scrn), TRUE);
	udev_device_unref(dev);
}
#endif

static void
drmmode_uevent_init(ScrnInfoPtr scrn)
{
#ifdef HAVE_LIBUDEV
	drmmode_ptr drmmode = drmmode_from_scrn(scrn);
	struct udev *u;
	struct udev_monitor *mon;

	u = udev_new();
	if (!u)
		return;
	mon = udev_monitor_new_from_netlink(u, "udev");
	if (!mon) {
		udev_unref(u);
		return;
	}

	if (udev_monitor_filter_add_match_subsystem_devtype(mon,
			"drm",
			"drm_minor") < 0 ||
			udev_monitor_enable_receiving(mon) < 0) {
		udev_monitor_unref(mon);
		udev_unref(u);
		return;
	}

	AddGeneralSocket(udev_monitor_get_fd(mon));
	drmmode->uevent_monitor = mon;
#endif
}

static void
drmmode_uevent_fini(ScrnInfoPtr scrn)
{
#ifdef HAVE_LIBUDEV
	drmmode_ptr drmmode = drmmode_from_scrn(scrn);

	if (drmmode->uevent_monitor) {
		struct udev *u = udev_monitor_get_udev(drmmode->uevent_monitor);

		RemoveGeneralSocket(udev_monitor_get_fd(drmmode->uevent_monitor));
		udev_monitor_unref(drmmode->uevent_monitor);
		udev_unref(u);
	}
#endif
}

static void
drmmode_flip_handler(int fd, unsigned int frame, unsigned int tv_sec,
		unsigned int tv_usec, void *event_data)
{
	drmmode_flipevtcarrier_ptr flipcarrier = event_data;
	drmmode_flipdata_ptr flipdata = flipcarrier->flipdata;
	drmmode_ptr drmmode = flipdata->drmmode;

	/* Is this the event whose info shall be delivered to higher level? */
	if (flipcarrier->dispatch_me) {
		/* Yes: Cache msc, ust for later delivery. */
		flipdata->fe_frame = frame;
		flipdata->fe_tv_sec = tv_sec;
		flipdata->fe_tv_usec = tv_usec;
	}
	free(flipcarrier);

	/* Last crtc completed flip? */
	flipdata->flip_count--;
	if (flipdata->flip_count > 0)
		return;

	/* Release framebuffer */
	drmModeRmFB(drmmode->fd, flipdata->old_fb_id);

	if (flipdata->event_data) {
		/* Deliver cached msc, ust from reference crtc to flip event handler */
		OFDRI2SwapComplete(flipdata->event_data, flipdata->fe_frame,
				flipdata->fe_tv_sec, flipdata->fe_tv_usec);
	}

	free(flipdata);
}

static void
drmmode_wakeup_handler(pointer data, int err, pointer p)
{
	ScrnInfoPtr scrn = data;
	drmmode_ptr drmmode = drmmode_from_scrn(scrn);
	fd_set *read_mask = p;

	if (scrn == NULL || err < 0)
		return;

	if (FD_ISSET(drmmode->fd, read_mask))
		drmHandleEvent(drmmode->fd, &drmmode->event_context);

#ifdef HAVE_LIBUDEV
	if (FD_ISSET(udev_monitor_get_fd(drmmode->uevent_monitor), read_mask))
		drmmode_handle_uevents(scrn);
#endif
}

void
drmmode_wait_for_event(ScrnInfoPtr pScrn)
{
	drmmode_ptr drmmode = drmmode_from_scrn(pScrn);
	drmHandleEvent(drmmode->fd, &drmmode->event_context);
}

void
drmmode_screen_init(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	drmmode_ptr drmmode = drmmode_from_scrn(pScrn);

	drmmode_uevent_init(pScrn);

	/* Plug in a pageflip completion event handler */
	drmmode->event_context.version = DRM_EVENT_CONTEXT_VERSION;
	drmmode->event_context.page_flip_handler = drmmode_flip_handler;

	AddGeneralSocket(drmmode->fd);

	/* Register a wakeup handler to get informed on DRM events */
	RegisterBlockAndWakeupHandlers((BlockHandlerProcPtr)NoopDDA,
			drmmode_wakeup_handler, pScrn);
}

void
drmmode_screen_fini(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	drmmode_ptr drmmode = drmmode_from_scrn(pScrn);

	drmmode_uevent_fini(pScrn);

	/* Register a wakeup handler to get informed on DRM events */
	RemoveBlockAndWakeupHandlers((BlockHandlerProcPtr)NoopDDA,
			drmmode_wakeup_handler, pScrn);
	RemoveGeneralSocket(drmmode->fd);

	fd_bo_del(pOf->scanout);
	pOf->scanout = NULL;
}
