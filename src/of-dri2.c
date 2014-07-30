/*
 * Copyright © 2012 Rob Clark <robclark@freedesktop.org>
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
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "of.h"

#include "xf86drmMode.h"
#include "dri2.h"

#include <errno.h>

typedef struct {
	DRI2BufferRec base;

	/**
	 * Pixmap that is backing the buffer
	 *
	 * NOTE: don't track the pixmap ptr for the front buffer if it is
	 * a window.. this could get reallocated from beneath us, so we should
	 * always use draw2pix to be sure to have the correct one
	 */
	PixmapPtr pPixmap;

	/**
	 * The DRI2 buffers are reference counted to avoid crashyness when the
	 * client detaches a dri2 drawable while we are still waiting for a
	 * page_flip event.
	 */
	int refcnt;

} OFDRI2BufferRec, *OFDRI2BufferPtr;

#define OFBUF(p)	((OFDRI2BufferPtr)(p))
#define DRIBUF(p)	((DRI2BufferPtr)(&(p)->base))

static void OFDRI2DestroyBuffer(DrawablePtr pDraw, DRI2BufferPtr buffer);

#define DRI2BufferThirdLeft       (DRI2BufferBackLeft | 0x00008000)

static DevPrivateKeyRec           OFDRI2WindowPrivateKeyRec;
#define OFDRI2WindowPrivateKey  (&OFDRI2WindowPrivateKeyRec)
static DevPrivateKeyRec           OFDRI2PixmapPrivateKeyRec;
#define OFDRI2PixmapPrivateKey  (&OFDRI2PixmapPrivateKeyRec)
static RESTYPE                    OFDRI2DrawableRes;

typedef struct {
	DrawablePtr pDraw;

	/* keep track of the third buffer, if created:
	 */
	DRI2BufferPtr pThirdBuffer;

	/* in case of triple buffering, we can get another swap request
	 * before the previous has completed, so queue up the next one:
	 */
	OFDRISwapCmd *cmd;

	/* pending swaps on this drawable (which might or might not be flips) */
	int pending_swaps;

	/* timestamp from last pageflip event (we cheat a bit in case
	 * of triple-buffering and send these cached values back to the
	 * client)
	 */
	uint32_t frame, tv_sec, tv_usec;

} OFDRI2DrawableRec, *OFDRI2DrawablePtr;

static int
OFDRI2DrawableGone(pointer p, XID id)
{
	OFDRI2DrawablePtr pPriv = p;
	DrawablePtr pDraw = pPriv->pDraw;

	if (pPriv->pThirdBuffer)
		OFDRI2DestroyBuffer(pDraw, pPriv->pThirdBuffer);

	if (pDraw->type == DRAWABLE_WINDOW) {
		dixSetPrivate(&((WindowPtr)pDraw)->devPrivates,
				OFDRI2WindowPrivateKey, NULL);
	} else {
		dixSetPrivate(&((PixmapPtr)pDraw)->devPrivates,
				OFDRI2PixmapPrivateKey, NULL);
	}

	free(pPriv);

	return Success;
}

static OFDRI2DrawablePtr
OFDRI2GetDrawable(DrawablePtr pDraw)
{
	OFDRI2DrawablePtr pPriv;

	if (pDraw->type == DRAWABLE_WINDOW) {
		pPriv = dixLookupPrivate(&((WindowPtr)pDraw)->devPrivates,
				OFDRI2WindowPrivateKey);
	} else {
		pPriv = dixLookupPrivate(&((PixmapPtr)pDraw)->devPrivates,
				OFDRI2PixmapPrivateKey);
	}

	if (!pPriv) {
		pPriv = calloc(1, sizeof(*pPriv));
		pPriv->pDraw = pDraw;

		if (pDraw->type == DRAWABLE_WINDOW) {
			dixSetPrivate(&((WindowPtr)pDraw)->devPrivates,
					OFDRI2WindowPrivateKey, pPriv);
		} else {
			dixSetPrivate(&((PixmapPtr)pDraw)->devPrivates,
					OFDRI2PixmapPrivateKey, pPriv);
		}

		if (!AddResource(pDraw->id, OFDRI2DrawableRes, pPriv)) {
			OFDRI2DrawableGone(pPriv, pDraw->id);
			pPriv = NULL;
		}
	}

	return pPriv;
}

static inline DrawablePtr
dri2draw(DrawablePtr pDraw, DRI2BufferPtr buf)
{
	if (buf->attachment == DRI2BufferFrontLeft) {
		return pDraw;
	} else {
		return &(OFBUF(buf)->pPixmap->drawable);
	}
}

static inline PixmapPtr
draw2pix(DrawablePtr pDraw)
{
	if (!pDraw) {
		return NULL;
	} else if (pDraw->type == DRAWABLE_WINDOW) {
		return pDraw->pScreen->GetWindowPixmap((WindowPtr)pDraw);
	} else {
		return (PixmapPtr)pDraw;
	}
}

static inline Bool
canexchange(DrawablePtr pDraw, DRI2BufferPtr a, DRI2BufferPtr b)
{
	DrawablePtr da = dri2draw(pDraw, a);
	DrawablePtr db = dri2draw(pDraw, b);

	return DRI2CanFlip(pDraw) &&
			(da->width == db->width) &&
			(da->height == db->height) &&
			(da->depth == db->depth);
}

static Bool
canflip(DrawablePtr pDraw)
{
	return (pDraw->type == DRAWABLE_WINDOW) &&
			DRI2CanFlip(pDraw);
}

static inline Bool
exchangebufs(DrawablePtr pDraw, DRI2BufferPtr a, DRI2BufferPtr b)
{
	of_pixmap_exchange(draw2pix(dri2draw(pDraw, a)),
			draw2pix(dri2draw(pDraw, b)));
	exchange(a->name, b->name);
	return TRUE;
}

static PixmapPtr
createpix(DrawablePtr pDraw)
{
	ScreenPtr pScreen = pDraw->pScreen;
	return pScreen->CreatePixmap(pScreen,
			pDraw->width, pDraw->height, pDraw->depth,
			CREATE_PIXMAP_USAGE_DRI2);
}

/**
 * Create Buffer.
 */
static DRI2BufferPtr
OFDRI2CreateBuffer(DrawablePtr pDraw, unsigned int attachment,
		unsigned int format)
{
	ScreenPtr pScreen = pDraw->pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFDRI2BufferPtr buf = calloc(1, sizeof(*buf));
	PixmapPtr pPixmap;
	int ret;

	DEBUG_MSG("pDraw=%p, attachment=%d, format=%08x",
			pDraw, attachment, format);

	if (!buf) {
		return NULL;
	}

	if (attachment == DRI2BufferFrontLeft) {
		pPixmap = draw2pix(pDraw);
		pPixmap->refcnt++;
	} else {
		pPixmap = createpix(pDraw);
	}

	DRIBUF(buf)->attachment = attachment;
	DRIBUF(buf)->cpp = pPixmap->drawable.bitsPerPixel / 8;
	DRIBUF(buf)->format = format;
	buf->refcnt = 1;
	buf->pPixmap = pPixmap;

	ret = of_get_pixmap_name(pPixmap, &DRIBUF(buf)->name,
			&DRIBUF(buf)->pitch);
	if (ret) {
		ERROR_MSG("could not get buffer name: %d", ret);
		OFDRI2DestroyBuffer(pDraw, DRIBUF(buf));
		return NULL;
	}

	return DRIBUF(buf);
}

/**
 * Destroy Buffer
 */
static void
OFDRI2DestroyBuffer(DrawablePtr pDraw, DRI2BufferPtr buffer)
{
	OFDRI2BufferPtr buf = OFBUF(buffer);
	ScreenPtr pScreen = buf->pPixmap->drawable.pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);

	if (--buf->refcnt)
		return;

	DEBUG_MSG("pDraw=%p, buffer=%p, attachment=%d",
			pDraw, buffer, buffer->attachment);

	/* if drawable has already gone away, 3rd buf is cleaned
	 * up in OFDRI2DrawableGone()
	 */
	if ((buffer->attachment == DRI2BufferBackLeft) && pDraw) {
		OFDRI2DrawablePtr pPriv = OFDRI2GetDrawable(pDraw);
		if (pPriv->pThirdBuffer) {
			OFDRI2DestroyBuffer(pDraw, pPriv->pThirdBuffer);
			pPriv->pThirdBuffer = NULL;
		}
	}

	pScreen->DestroyPixmap(buf->pPixmap);

	free(buf);
}

static void
OFDRI2ReferenceBuffer(DRI2BufferPtr buffer)
{
	OFDRI2BufferPtr buf = OFBUF(buffer);
	buf->refcnt++;
}

/**
 *
 */
static void
OFDRI2CopyRegion(DrawablePtr pDraw, RegionPtr pRegion,
		DRI2BufferPtr pDstBuffer, DRI2BufferPtr pSrcBuffer)
{
	ScreenPtr pScreen = pDraw->pScreen;
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	DrawablePtr pSrcDraw = dri2draw(pDraw, pSrcBuffer);
	DrawablePtr pDstDraw = dri2draw(pDraw, pDstBuffer);
	RegionPtr pCopyClip;
	GCPtr pGC;

	DEBUG_MSG("pDraw=%p, pDstBuffer=%p (%p), pSrcBuffer=%p (%p)",
			pDraw, pDstBuffer, pSrcDraw, pSrcBuffer, pDstDraw);

	/* hack.. since we don't have proper fencing / kernel synchronization
	 * we can get in a scenario where we get many frames ahead of the gpu,
	 * with queued up cmd sequence like: render -> blit -> render -> blit ..
	 * This hack makes sure the previous blit has completed.
	 */
	{
	OFPtr pOF = OFPTR(pScrn);
	OFDRI2BufferPtr buf = OFBUF(pDstBuffer);
	pOF->pExa->PrepareAccess(buf->pPixmap, 0);
	pOF->pExa->FinishAccess(buf->pPixmap, 0);
	}

	pGC = GetScratchGC(pDstDraw->depth, pScreen);
	if (!pGC) {
		return;
	}

	pCopyClip = REGION_CREATE(pScreen, NULL, 0);
	RegionCopy(pCopyClip, pRegion);
	(*pGC->funcs->ChangeClip) (pGC, CT_REGION, pCopyClip, 0);
	ValidateGC(pDstDraw, pGC);

	/* If the dst is the framebuffer, and we had a way to
	 * schedule a deferred blit synchronized w/ vsync, that
	 * would be a nice thing to do utilize here to avoid
	 * tearing..  when we have sync object support for GEM
	 * buffers, I think we could do something more clever
	 * here.
	 */

	pGC->ops->CopyArea(pSrcDraw, pDstDraw, pGC,
			0, 0, pDraw->width, pDraw->height, 0, 0);

	FreeScratchGC(pGC);

	OFFlushAccel(pScreen);
}

/**
 * Get current frame count and frame count timestamp, based on drawable's
 * crtc.
 */
static int
OFDRI2GetMSC(DrawablePtr pDraw, CARD64 *ust, CARD64 *msc)
{
	ScreenPtr pScreen = pDraw->pScreen;
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	OFPtr pOF = OFPTR(pScrn);
	drmVBlank vbl = { .request = {
		.type = DRM_VBLANK_RELATIVE,
		.sequence = 0,
	} };
	int ret;

	ret = drmWaitVBlank(pOF->drmFD, &vbl);
	if (ret) {
		static int limit = 5;
		if (limit) {
			WARNING_MSG("get vblank counter failed: %s", strerror(errno));
			limit--;
		}
		return FALSE;
	}

	if (ust) {
		*ust = ((CARD64)vbl.reply.tval_sec * 1000000) + vbl.reply.tval_usec;
	}
	if (msc) {
		*msc = vbl.reply.sequence;
	}

	return TRUE;
}

struct _OFDRISwapCmd {
	int type;
	ClientPtr client;
	ScreenPtr pScreen;

	/* Note: store drawable ID, rather than drawable.  It's possible that
	 * the drawable can be destroyed while we wait for page flip event:
	 */
	XID draw_id;
	DRI2BufferPtr pDstBuffer;
	DRI2BufferPtr pSrcBuffer;
	DRI2SwapEventPtr func;
	void *data;
};

static const char *swap_names[] = {
		[DRI2_EXCHANGE_COMPLETE] = "exchange",
		[DRI2_BLIT_COMPLETE] = "blit",
		[DRI2_FLIP_COMPLETE] = "flip,"
};

static void
OFDRI2SwapDispatch(DrawablePtr pDraw, OFDRISwapCmd *cmd)
{
	ScreenPtr pScreen = pDraw->pScreen;
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	OFDRI2DrawablePtr pPriv = OFDRI2GetDrawable(pDraw);
	OFDRI2BufferPtr src = OFBUF(cmd->pSrcBuffer);

	/* if we can flip, do so: */
	if (canflip(pDraw) &&
			drmmode_page_flip(pDraw, src->pPixmap, cmd)) {
		OFPTR(pScrn)->pending_page_flips++;
		cmd->type = DRI2_FLIP_COMPLETE;
	} else if (canexchange(pDraw, cmd->pSrcBuffer, cmd->pDstBuffer)) {
		/* we can get away w/ pointer swap.. yah! */
		cmd->type = DRI2_EXCHANGE_COMPLETE;
	} else {
		/* fallback to blit: */
		BoxRec box = {
				.x1 = 0,
				.y1 = 0,
				.x2 = pDraw->width,
				.y2 = pDraw->height,
		};
		RegionRec region;
		RegionInit(&region, &box, 0);
		OFDRI2CopyRegion(pDraw, &region,
				cmd->pDstBuffer, cmd->pSrcBuffer);
		RegionUninit(&region);
		cmd->type = DRI2_BLIT_COMPLETE;
	}

	DEBUG_MSG("%s dispatched: %d -> %d", swap_names[cmd->type],
			cmd->pSrcBuffer->attachment, cmd->pDstBuffer->attachment);

	/* for flip/exchange, cycle buffers now, so no next DRI2GetBuffers
	 * gets the new buffer names:
	 */
	switch (cmd->type) {
	case DRI2_FLIP_COMPLETE:
		/* allocate 3rd buffer if needed: */
		if (!pPriv->pThirdBuffer) {
			pPriv->pThirdBuffer = OFDRI2CreateBuffer(pDraw,
					DRI2BufferThirdLeft, cmd->pSrcBuffer->format);
		}
		exchangebufs(pDraw, cmd->pDstBuffer, pPriv->pThirdBuffer);
		/* fallthrough */
	case DRI2_EXCHANGE_COMPLETE:
		exchangebufs(pDraw, cmd->pSrcBuffer, cmd->pDstBuffer);
	}

	/* if we are triple buffering, send event back to client
	 * now, rather than waiting for vblank, so client can
	 * immediately request the new back buffer:
	 */
	if (pPriv->pThirdBuffer) {
		DRI2SwapComplete(cmd->client, pDraw,
				pPriv->frame, pPriv->tv_sec, pPriv->tv_usec,
				cmd->type, cmd->func, cmd->data);
	}

	/* for exchange/blit, there is no page_flip event to wait for:
	 */
	if (cmd->type != DRI2_FLIP_COMPLETE) {
		OFDRI2SwapComplete(cmd, 0, 0, 0);
	}
}

void
OFDRI2SwapComplete(OFDRISwapCmd *cmd, uint32_t frame,
		uint32_t tv_sec, uint32_t tv_usec)
{
	ScreenPtr pScreen = cmd->pScreen;
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	DrawablePtr pDraw = NULL;
	int status;

	DEBUG_MSG("%s complete: %d -> %d", swap_names[cmd->type],
			cmd->pSrcBuffer->attachment, cmd->pDstBuffer->attachment);

	if (cmd->type == DRI2_FLIP_COMPLETE)
		OFPTR(pScrn)->pending_page_flips--;

	status = dixLookupDrawable(&pDraw, cmd->draw_id, serverClient,
			M_ANY, DixWriteAccess);

	if (status == Success) {
		OFDRI2DrawablePtr pPriv = OFDRI2GetDrawable(pDraw);
		if (pPriv->pThirdBuffer) {
			pPriv->frame = frame;
			pPriv->tv_sec = tv_sec;
			pPriv->tv_usec = tv_usec;
		} else {
			DRI2SwapComplete(cmd->client, pDraw,
					frame, tv_sec, tv_usec, cmd->type,
					cmd->func, cmd->data);
		}
		if (pPriv->cmd) {
			/* dispatch queued flip: */
			OFDRISwapCmd *cmd = pPriv->cmd;
			pPriv->cmd = NULL;
			OFDRI2SwapDispatch(pDraw, cmd);
		}
		pPriv->pending_swaps--;
	}

	/* drop extra refcnt we obtained prior to swap:
	 */
	OFDRI2DestroyBuffer(pDraw, cmd->pSrcBuffer);
	OFDRI2DestroyBuffer(pDraw, cmd->pDstBuffer);

	free(cmd);
}

/**
 * ScheduleSwap is responsible for requesting a DRM vblank event for the
 * appropriate frame.
 *
 * In the case of a blit (e.g. for a windowed swap) or buffer exchange,
 * the vblank requested can simply be the last queued swap frame + the swap
 * interval for the drawable.
 *
 * In the case of a page flip, we request an event for the last queued swap
 * frame + swap interval - 1, since we'll need to queue the flip for the frame
 * immediately following the received event.
 */
static int
OFDRI2ScheduleSwap(ClientPtr client, DrawablePtr pDraw,
		DRI2BufferPtr pDstBuffer, DRI2BufferPtr pSrcBuffer,
		CARD64 *target_msc, CARD64 divisor, CARD64 remainder,
		DRI2SwapEventPtr func, void *data)
{
	ScreenPtr pScreen = pDraw->pScreen;
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	OFDRI2DrawablePtr pPriv = OFDRI2GetDrawable(pDraw);
	OFDRISwapCmd *cmd = calloc(1, sizeof(*cmd));

	cmd->client = client;
	cmd->pScreen = pScreen;
	cmd->draw_id = pDraw->id;
	cmd->pSrcBuffer = pSrcBuffer;
	cmd->pDstBuffer = pDstBuffer;
	cmd->func = func;
	cmd->data = data;

	/* obtain extra ref on buffers to avoid them going away while we await
	 * the page flip event:
	 */
	OFDRI2ReferenceBuffer(pSrcBuffer);
	OFDRI2ReferenceBuffer(pDstBuffer);

	pPriv->pending_swaps++;

	if (pPriv->pending_swaps > 1) {
		/* if we already have a pending swap, then just queue this
		 * one up:
		 */
		if (pPriv->cmd) {
			ERROR_MSG("already pending a flip!");
			pPriv->pending_swaps--;
			return FALSE;
		}
		pPriv->cmd = cmd;
	} else {
		OFDRI2SwapDispatch(pDraw, cmd);
	}

	return TRUE;
}

/**
 * Request a DRM event when the requested conditions will be satisfied.
 *
 * We need to handle the event and ask the server to wake up the client when
 * we receive it.
 */
static int
OFDRI2ScheduleWaitMSC(ClientPtr client, DrawablePtr pDraw, CARD64 target_msc,
		CARD64 divisor, CARD64 remainder)
{
	/* TODO */
	DRI2WaitMSCComplete(client, pDraw, target_msc, 0, 0);
	return TRUE;
}

/**
 * The DRI2 ScreenInit() function.. register our handler fxns w/ DRI2 core
 */
Bool
OFDRI2ScreenInit(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOF = OFPTR(pScrn);
	DRI2InfoRec info = {
			.fd 			= pOF->drmFD,
			.deviceName		= pOF->deviceName,
			.CreateBuffer		= OFDRI2CreateBuffer,
			.DestroyBuffer		= OFDRI2DestroyBuffer,
			.CopyRegion		= OFDRI2CopyRegion,
			.AuthMagic		= drmAuthMagic,
	};
	int minor = 1, major = 0;

	info.driverName      = "exynos";
	info.version         = 6;
	info.ScheduleSwap    = OFDRI2ScheduleSwap;
	info.ScheduleWaitMSC = OFDRI2ScheduleWaitMSC;
	info.GetMSC          = OFDRI2GetMSC;

	if (xf86LoaderCheckSymbol("DRI2Version")) {
		DRI2Version(&major, &minor);
	}

	if (minor < 1) {
		WARNING_MSG("DRI2 requires DRI2 module version 1.1.0 or later");
		return FALSE;
	}

	OFDRI2DrawableRes = CreateNewResourceType(
			OFDRI2DrawableGone, (char *)"OFDRI2Drawable");
	if (!OFDRI2DrawableRes)
		return FALSE;

	if (!dixRegisterPrivateKey(&OFDRI2WindowPrivateKeyRec, PRIVATE_WINDOW, 0))
		return FALSE;

	if (!dixRegisterPrivateKey(&OFDRI2PixmapPrivateKeyRec, PRIVATE_PIXMAP, 0))
		return FALSE;

	return DRI2ScreenInit(pScreen, &info);
}

/**
 * The DRI2 CloseScreen() function.. unregister ourself w/ DRI2 core.
 */
void
OFDRI2CloseScreen(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86Screens[pScreen->myNum];
	OFPtr pOF = OFPTR(pScrn);
	while (pOF->pending_page_flips > 0) {
		DEBUG_MSG("waiting..");
		drmmode_wait_for_event(pScrn);
	}
	DRI2CloseScreen(pScreen);
}
