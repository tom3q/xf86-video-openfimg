/* of-driver.c
 *
 * Copyright (c) 2009-2010, Code Aurora Forum. All rights reserved.
 * Copyright (c) 2014, Tomasz Figa
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Code Aurora nor
 *       the names of its contributors may be used to endorse or promote
 *       products derived from this software without specific prior written
 *       permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON-INFRINGEMENT ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <string.h>
#include <sys/types.h>
#include <grp.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/mman.h>
#include <stdint.h>

#include "xf86.h"
#include "damage.h"
#include "xf86_OSlib.h"
#include "xf86Crtc.h"

#include "mipointer.h"
#include "micmap.h"
#include "fb.h"
#include "dixstruct.h"

#include "of.h"
#include "compat-api.h"

#include <drm.h>
#include "xf86drm.h"

#ifdef XSERVER_PLATFORM_BUS
#include "xf86platformBus.h"
#endif

#define OF_NAME        "openfimg"
#define OF_DRIVER_NAME "openfimg"

#define OF_VERSION_MAJOR PACKAGE_VERSION_MAJOR
#define OF_VERSION_MINOR PACKAGE_VERSION_MINOR
#define OF_VERSION_PATCH PACKAGE_VERSION_PATCHLEVEL

#define OF_VERSION_CURRENT \
		((OF_VERSION_MAJOR << 20) |\
				(OF_VERSION_MINOR << 10) | \
				(OF_VERSION_PATCH))


/* An aray containing the options that the user can
   configure in xorg.conf
 */

static const OptionInfoRec OFOptions[] = {
		{OPTION_FB, "fb", OPTV_STRING, {0}, FALSE},
		{OPTION_NOACCEL, "NoAccel", OPTV_BOOLEAN, {0}, FALSE},
		{OPTION_SWCURSOR, "SWCursor", OPTV_BOOLEAN, {0}, FALSE},
		{OPTION_EXAMASK, "examask", OPTV_INTEGER, {0}, FALSE},
		{OPTION_VSYNC, "DefaultVsync", OPTV_INTEGER, {0}, FALSE},
		{OPTION_DEBUG, "Debug", OPTV_BOOLEAN, {0}, FALSE},
		{-1, NULL, OPTV_NONE, {0}, FALSE}
};


static Bool OFEnterVT(VT_FUNC_ARGS_DECL);
static void OFLeaveVT(VT_FUNC_ARGS_DECL);

Bool ofDebug = TRUE;

static void
OFBlockHandler (BLOCKHANDLER_ARGS_DECL)
{
	SCREEN_PTR(arg);
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);

	pScreen->BlockHandler = pOf->BlockHandler;
	(*pScreen->BlockHandler) (BLOCKHANDLER_ARGS);
	pScreen->BlockHandler = OFBlockHandler;

	if (pScrn->vtSema)
		OFFlushAccel(pScreen);
}

/*
 * Because we don't use DRI1:
 */

static int
dri_drm_debug_print(const char *format, va_list ap)
{
	xf86VDrvMsgVerb(-1, X_NONE, 3, format, ap);
	return 0;
}

static void
dri_drm_get_perms(gid_t * group, mode_t * mode)
{
	*group = -1;
	*mode = 0666;
}

static drmServerInfo drm_server_info = {
	dri_drm_debug_print,
	xf86LoadKernelModule,
	dri_drm_get_perms,
};

static int
fd_is_server_managed(OFPtr pOf)
{
#ifdef XF86_PDEV_SERVER_FD
	if (!(pOf->pEnt->location.type == BUS_PLATFORM
	    && (pOf->pEnt->location.id.plat->flags & XF86_PDEV_SERVER_FD)))
		return 1;
#endif
	return 0;
}

static void
free_of(OFPtr pOf)
{
	if (pOf->drmFD && !fd_is_server_managed(pOf))
		drmClose(pOf->drmFD);
	free(pOf);
}

static Bool
OFInitDRM(ScrnInfoPtr pScrn)
{
	OFPtr pOf = OFPTR(pScrn);

	drmSetServerInfo(&drm_server_info);

	pOf->drmFD = drmOpen("exynos", NULL);
	if (pOf->drmFD < 0) {
		xf86DrvMsg(pScrn->scrnIndex, X_WARNING,
				"Unable to open a DRM device\n");
		return FALSE;
	}

	pOf->dev = fd_device_new(pOf->drmFD);

	pOf->deviceName = drmGetDeviceNameFromFd(pOf->drmFD);

	return TRUE;
}

/* This is the main initialization function for the screen */

static Bool
OFPreInit(ScrnInfoPtr pScrn, int flags)
{
	OFPtr pOf;
	rgb defaultWeight = { 0, 0, 0 };
	Gamma zeros = { 0.0, 0.0, 0.0 };
	unsigned long val;

	DEBUG_MSG("pre-init");

	/* Omit ourselves from auto-probing (which is bound to
	 * fail on our hardware anyway)
	 *
	 * TODO we could probe for drm device..
	 */

	if (flags & PROBE_DETECT) {
		DEBUG_MSG("probe not supported");
		return FALSE;
	}

	if (pScrn->numEntities != 1) {
		DEBUG_MSG("numEntities=%d", pScrn->numEntities);
		return FALSE;
	}

	/* Just use the current monitor specified in the
	 * xorg.conf.  This really means little to us since
	 * we have no choice over which monitor is used,
	 * but X needs this to be set
	 */

	pScrn->monitor = pScrn->confScreen->monitor;

	/* Allocate room for our private data */
	if (pScrn->driverPrivate == NULL)
		pScrn->driverPrivate = xnfcalloc(sizeof(OFRec), 1);

	pOf = OFPTR(pScrn);

	if (pOf == NULL) {
		ERROR_MSG("Unable to allocate memory");
		return FALSE;
	}

	xf86PrintDepthBpp(pScrn);
	pScrn->rgbBits = 8;

	pScrn->progClock = TRUE;
	pScrn->chipset = OF_DRIVER_NAME;

	INFO_MSG("OpenFIMG driver running on Samsung SoC");

	pOf->pEnt = xf86GetEntityInfo(pScrn->entityList[0]);

	if (!OFInitDRM(pScrn)) {
		ERROR_MSG("Unable to open DRM");
		return FALSE;
	}

	if (!drmmode_pre_init(pScrn, pOf->drmFD, pScrn->bitsPerPixel >> 3)) {
		ERROR_MSG("Kernel modesetting failed to initialize");
		return FALSE;
	}

	xf86CollectOptions(pScrn, NULL);

	pOf->options = malloc(sizeof(OFOptions));

	if (pOf->options == NULL) {
		free_of(pOf);
		return FALSE;
	}

	memcpy(pOf->options, OFOptions, sizeof(OFOptions));
	xf86ProcessOptions(pScrn->scrnIndex, pScrn->options, pOf->options);

	/* Determine if the user wants debug messages turned on: */
	ofDebug = xf86ReturnOptValBool(pOf->options, OPTION_DEBUG, FALSE);

	/* NoAccel - default FALSE */
	pOf->NoAccel = xf86ReturnOptValBool(pOf->options, OPTION_NOACCEL, FALSE);

	/* SWCursor - default FALSE */
	pOf->HWCursor = !xf86ReturnOptValBool(pOf->options, OPTION_SWCURSOR, FALSE);

	if (xf86GetOptValULong(pOf->options, OPTION_EXAMASK, &val))
		pOf->examask = val;
	else
		pOf->examask = ACCEL_DEFAULT;

	xf86PrintModes(pScrn);

	/* FIXME:  We will probably need to be more exact when setting
	 * the DPI.  For now, we just use the default (96,96 I think) */

	xf86SetDpi(pScrn, 0, 0);

	if (!xf86SetWeight(pScrn, defaultWeight, defaultWeight)) {
		free_of(pOf);
		return FALSE;
	}

	/* Initialize default visual */
	if (!xf86SetDefaultVisual(pScrn, -1)) {
		free_of(pOf);
		return FALSE;
	}

	if (!xf86SetGamma(pScrn, zeros)) {
		free_of(pOf);
		return FALSE;
	}

	INFO_MSG("OF Options:");
	INFO_MSG("  HW Cursor: %s", pOf->HWCursor ? "Enabled" : "Disabled");
	INFO_MSG("  examask:   %d", pOf->examask);

	return TRUE;
}

static Bool
OFSaveScreen(ScreenPtr pScreen, int mode)
{
	/* Nothing to do here, yet */
	return TRUE;
}

static Bool
OFCreateScreenResources(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	PixmapPtr ppix;

	pScreen->CreateScreenResources = pOf->CreateScreenResources;
	if (!(*pScreen->CreateScreenResources)(pScreen))
		return FALSE;
	pScreen->CreateScreenResources = OFCreateScreenResources;

	if (!OFEnterVT(VT_FUNC_ARGS(0)))
		return FALSE;

	ppix = pScreen->GetScreenPixmap(pScreen);
	if (ppix) {
		int pitch = OFAlignedStride(ppix->drawable.width,
						ppix->drawable.bitsPerPixel);

		pScreen->ModifyPixmapHeader(ppix, ppix->drawable.width,
						ppix->drawable.height,
						ppix->drawable.depth,
						ppix->drawable.bitsPerPixel,
						pitch, NULL);
		of_set_pixmap_bo(ppix, pOf->scanout);
	}

	return TRUE;
}

static Bool
OFCloseScreen(CLOSE_SCREEN_ARGS_DECL)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);

	DEBUG_MSG("close screen");

	/* Close DRI2 */
	if (pOf->dri) {
		OFDRI2CloseScreen(pScreen);
	}

	/* Close EXA */
	if (pOf->pExa) {
		exaDriverFini(pScreen);
		free(pOf->pExa);
		pOf->pExa = NULL;
	}

	if (pScrn->vtSema) {
		OFLeaveVT(VT_FUNC_ARGS(0));
		pScrn->vtSema = FALSE;
	}

	drmmode_screen_fini(pScreen);

	pScreen->BlockHandler = pOf->BlockHandler;
	pScreen->CloseScreen = pOf->CloseScreen;

	return (*pScreen->CloseScreen)(CLOSE_SCREEN_ARGS);
}

static Bool
OFScreenInit(SCREEN_INIT_ARGS_DECL)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	int displayWidth;

	DEBUG_MSG("screen-init");

	/* Set up the X visuals */
	miClearVisualTypes();

	/* We only support TrueColor at the moment, and I suspect that is all
	 * we will ever support */

	if (!miSetVisualTypes(pScrn->depth, TrueColorMask,
			pScrn->rgbBits, TrueColor)) {
		ERROR_MSG("Unable to set up the visual for %d BPP", pScrn->bitsPerPixel);
		return FALSE;
	}

	if (!miSetPixmapDepths()) {
		ERROR_MSG("Unable to set the pixmap depth");
		return FALSE;
	}

	/* Set up the X drawing area */

	displayWidth = pScrn->displayWidth;
	if (!displayWidth)
		displayWidth = pScrn->virtualX;

	xf86LoadSubModule(pScrn, "fb");

	if (!fbScreenInit(pScreen, NULL,
			pScrn->virtualX, pScrn->virtualY,
			pScrn->xDpi, pScrn->yDpi,
			displayWidth, pScrn->bitsPerPixel)) {
		ERROR_MSG("fbScreenInit failed");
		return FALSE;
	}

	/* Set up the color information for the visual(s) */

	if (pScrn->bitsPerPixel > 8) {
		VisualPtr visual = pScreen->visuals + pScreen->numVisuals;

		while (--visual >= pScreen->visuals) {
			if ((visual->class | DynamicClass) == DirectColor) {
				visual->offsetRed = pScrn->offset.red;
				visual->offsetGreen = pScrn->offset.green;
				visual->offsetBlue = pScrn->offset.blue;
				visual->redMask = pScrn->mask.red;
				visual->greenMask = pScrn->mask.green;
				visual->blueMask = pScrn->mask.blue;
			}
		}
	}

	/* Set up the Render fallbacks */
	if (!fbPictureInit(pScreen, NULL, 0)) {
		ERROR_MSG("fbPictureInit failed");
		return FALSE;
	}

	/* Set default colors */
	xf86SetBlackWhitePixels(pScreen);

	/* Set up the backing store */
	xf86SetBackingStore(pScreen);

	/* Set up EXA */
	xf86LoadSubModule(pScrn, "exa");

	if (!OFSetupAccel(pScreen))
		ERROR_MSG("Unable to setup EXA");

	/* Enable cursor position updates by mouse signal handler: */
	xf86SetSilkenMouse(pScreen);

	/* Set up the software cursor */
	miDCInitialize(pScreen, xf86GetPointerScreenFuncs());

	/* Try to set up the HW cursor */
	if (pOf->HWCursor) {
		pOf->HWCursor = drmmode_cursor_init(pScreen);
		if (!pOf->HWCursor)
			ERROR_MSG("Hardware cursor initialization failed");
	}

	/* Set up the default colormap */

	if (!miCreateDefColormap(pScreen)) {
		ERROR_MSG("miCreateDefColormap failed");
		return FALSE;
	}

	pScreen->SaveScreen = OFSaveScreen;

	pOf->CloseScreen = pScreen->CloseScreen;
	pScreen->CloseScreen = OFCloseScreen;

	pOf->CreateScreenResources = pScreen->CreateScreenResources;
	pScreen->CreateScreenResources = OFCreateScreenResources;

	pOf->BlockHandler = pScreen->BlockHandler;
	pScreen->BlockHandler = OFBlockHandler;

	if (!xf86CrtcScreenInit(pScreen)) {
		ERROR_MSG("CRTCScreenInit failed");
		return FALSE;
	}

	if (!drmmode_screen_init(pScreen)) {
		ERROR_MSG("drmmode_screen_init failed");
		return FALSE;
	}

	return TRUE;
}

static Bool
OFSwitchMode(SWITCH_MODE_ARGS_DECL)
{
	/* FIXME:  We should only have the one mode, so we shouldn't ever call
	 * this function - regardless, it needs to be stubbed - so what
	 * do we return, TRUE or FALSE? */

	return TRUE;
}

static Bool
OFEnterVT(VT_FUNC_ARGS_DECL)
{
	SCRN_INFO_PTR(arg);
	OFPtr pOf = OFPTR(pScrn);
	int ret;

	DEBUG_MSG("enter-vt");

	if (!fd_is_server_managed(pOf)) {
		ret = drmSetMaster(pOf->drmFD);
		if (ret)
			ERROR_MSG("Unable to get master: %s", strerror(errno));
	}

	/* Set up the mode - this doesn't actually touch the hardware,
	 * but it makes RandR all happy */

	if (!xf86SetDesiredModes(pScrn)) {
		ERROR_MSG("Unable to set the mode");
		return FALSE;
	}

	return TRUE;
}

static void
OFLeaveVT(VT_FUNC_ARGS_DECL)
{
	SCRN_INFO_PTR(arg);
	OFPtr pOf = OFPTR(pScrn);
	int ret;

	DEBUG_MSG("leave-vt");

	if (!fd_is_server_managed(pOf)) {
		ret = drmDropMaster(pOf->drmFD);
		if (ret)
			ERROR_MSG("Unable to drop master: %s", strerror(errno));
	}
}

static void
OFFreeScreen(FREE_SCREEN_ARGS_DECL)
{
	SCRN_INFO_PTR(arg);
	OFPtr pOf = OFPTR(pScrn);
	free_of(pOf);
}

/* ------------------------------------------------------------ */
/* Following is the standard driver setup that probes for the   */
/* hardware and sets up the structures.                         */

static const OptionInfoRec *
OFAvailableOptions(int chipid, int busid)
{
	return OFOptions;
}

static void
OFIdentify(int flags)
{
	xf86Msg(X_INFO, "%s: Video driver for Samsung processors\n", OF_NAME);
}

static Bool
OFProbe(DriverPtr drv, int flags)
{
	GDevPtr *sections;
	int i, nsects;
	Bool foundScreen = FALSE;
	ScrnInfoPtr pScrn = NULL;

	/* For now, just return false during a probe */

	if (flags & PROBE_DETECT) {
		ErrorF("probe not supported\n");
		return FALSE;
	}

	/* Find all of the device sections in the config */

	nsects = xf86MatchDevice(OF_NAME, &sections);
	if (nsects <= 0) {
		xf86Msg(X_INFO, "Did not find any matching device "
				"section in configuration file\n");
		if (flags & PROBE_DETECT) {
			/* if we are probing, assume one and lets see if we can
			 * open the device to confirm it is there:
			 */
			nsects = 1;
		} else {
			return FALSE;
		}
	}

	for (i = 0; i < nsects; i++) {
		int entity, drmfd;

		drmfd = drmOpen("exynos", NULL);
		if (drmfd < 0) {
			xf86Msg(X_WARNING, "Could not open drm: %s\n",
					strerror(errno));
			continue;
		}

		close(drmfd);

		foundScreen = TRUE;

		if (flags & PROBE_DETECT) {
			/* just add the device.. we aren't a PCI device, so
			 * call xf86AddBusDeviceToConfigure() directly
			 */
			xf86AddBusDeviceToConfigure(OF_DRIVER_NAME,
					BUS_NONE, NULL, i);
			continue;
		}

		entity = xf86ClaimFbSlot(drv, 0, sections[i], TRUE);
		pScrn = xf86ConfigFbEntity(NULL, 0, entity, NULL, NULL, NULL, NULL);

		/* Set up the hooks for the screen */

		pScrn->driverVersion = OF_VERSION_CURRENT;
		pScrn->driverName = OF_NAME;
		pScrn->name = OF_NAME;
		pScrn->Probe = OFProbe;
		pScrn->PreInit = OFPreInit;
		pScrn->ScreenInit = OFScreenInit;
		pScrn->SwitchMode = OFSwitchMode;
		pScrn->EnterVT = OFEnterVT;
		pScrn->LeaveVT = OFLeaveVT;
		pScrn->FreeScreen = OFFreeScreen;
	}

	free(sections);
	return foundScreen;
}

static Bool
OFDriverFunc(ScrnInfoPtr scrn, xorgDriverFuncOp op, void *data)
{
	xorgHWFlags *flag;

	switch (op) {
	case GET_REQUIRED_HW_INTERFACES:
		flag = (CARD32 *)data;
		(*flag) = 0;
		return TRUE;
#ifdef XF86_PDEV_SERVER_FD
	case SUPPORTS_SERVER_FDS:
		return TRUE;
#endif
	default:
		return FALSE;
	}
}

#ifdef XSERVER_PLATFORM_BUS
static Bool probe_hw(struct xf86_platform_device *dev)
{
	int fd;

	/* NOTE: for kgsl we still need config file to find fbdev device,
	 * so it will always be probed through the old OFProbe path.  So
	 * only look for drm/msm here:
	 */

#ifdef XF86_PDEV_SERVER_FD
	if (dev && (dev->flags & XF86_PDEV_SERVER_FD)) {
		drmVersionPtr version;

		fd = xf86_get_platform_device_int_attrib(dev, ODEV_ATTRIB_FD,
								-1);
		if (fd == -1)
			return FALSE;

		version = drmGetVersion(fd);
		/* make sure we have the right device: */
		if (version && (strcmp(version->name, "exynos") == 0)) {
			drmFreeVersion(version);
			return TRUE;
		}

		drmFreeVersion(version);
		return FALSE;
	}
#endif

	fd = drmOpen("exynos", NULL);
	if (fd != -1) {
		close(fd);
		return TRUE;
	}

	return FALSE;
}

static Bool
OFPlatformProbe(DriverPtr driver,
		int entity_num, int flags, struct xf86_platform_device *dev,
		intptr_t match_data)
{
	ScrnInfoPtr pScrn = NULL;
	int scr_flags = 0;

	/* Note: at least for now there is no point in gpu screens.. and
	 * allowing them exposes a bug in older xservers that would result
	 * in the device probed as a gpu screen rather than regular screen
	 * resulting in "No screens found".
	 *
	 * If later there is actually reason to support GPU screens, track
	 * down the first xorg ABI # that contains the fix, and make this
	 * conditional on that or later ABI versions.  Otherwise you will
	 * break things for people with older xservers.
	 *
	if (flags & PLATFORM_PROBE_GPU_SCREEN)
		scr_flags = XF86_ALLOCATE_GPU_SCREEN;
	 */

	if (probe_hw(dev)) {
		pScrn = xf86AllocateScreen(driver, scr_flags);
		xf86AddEntityToScreen(pScrn, entity_num);

		pScrn->driverVersion = OF_VERSION_CURRENT;
		pScrn->driverName = OF_NAME;
		pScrn->name = OF_NAME;
		pScrn->Probe = OFProbe;
		pScrn->PreInit = OFPreInit;
		pScrn->ScreenInit = OFScreenInit;
		pScrn->SwitchMode = OFSwitchMode;
		pScrn->EnterVT = OFEnterVT;
		pScrn->LeaveVT = OFLeaveVT;
		pScrn->FreeScreen = OFFreeScreen;
	}

	return pScrn != NULL;
}
#endif

_X_EXPORT DriverRec openfimgDriver = {
		OF_VERSION_CURRENT,
		OF_DRIVER_NAME,
		OFIdentify,
		OFProbe,
		OFAvailableOptions,
		NULL,
		0,
		OFDriverFunc,
		NULL,
		NULL,  /* pci_probe */
#ifdef XSERVER_PLATFORM_BUS
		OFPlatformProbe,
#endif
};

MODULESETUPPROTO(openfimgSetup);

/* Versioning information for the module - most of these variables will
   come from config.h generated by ./configure
 */

static XF86ModuleVersionInfo openfimgVersRec = {
		OF_DRIVER_NAME,
		MODULEVENDORSTRING,
		MODINFOSTRING1,
		MODINFOSTRING2,
		XORG_VERSION_CURRENT,
		OF_VERSION_MAJOR, OF_VERSION_MINOR, OF_VERSION_PATCH,
		ABI_CLASS_VIDEODRV,
		ABI_VIDEODRV_VERSION,
		NULL,
		{0, 0, 0, 0},
};

_X_EXPORT XF86ModuleData openfimgModuleData = { &openfimgVersRec, openfimgSetup, NULL };

pointer
openfimgSetup(pointer module, pointer ops, int *errmaj, int *errmin)
{
	static Bool initDone = FALSE;

	if (initDone == FALSE) {
		initDone = TRUE;
		xf86AddDriver(&openfimgDriver, module, HaveDriverFuncs);

		/* FIXME: Load symbol references here */
		return (pointer) 1;
	} else {
		if (errmaj)
			*errmaj = LDR_ONCEONLY;
		return NULL;
	}
}
