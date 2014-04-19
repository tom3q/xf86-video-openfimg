/*
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

#include <errno.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/mman.h>

#include "of.h"

#include <xa_tracker.h>

Bool
OFSetupAccel(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	Bool ret, softexa = FALSE;
	struct fd_pipe *p;

	if (pOf->NoAccel) {
		INFO_MSG("Acceleration disabled in config file");
		softexa = TRUE;
		goto out;
	}

	INFO_MSG("trying 3D/XA");

	p = fd_pipe_new(pOf->dev, FD_PIPE_3D);
	if (!p) {
		ERROR_MSG("no 3D pipe");
		softexa = TRUE;
		goto out;
	}

	pOf->xa = xa_tracker_create(pOf->drmFD);
	if (!pOf->xa) {
		ERROR_MSG("could not setup XA");
		softexa = TRUE;
		goto out;
	}

	pOf->pipe = p;

	ret = OFSetupExaXA(pScreen);
	if (ret) {
		INFO_MSG("using 3D/XA");
	} else {
		ERROR_MSG("3D/XA setup failed");
		softexa = TRUE;
	}

out:
	if (softexa)
		ret = OFSetupExaSW(pScreen);

	if (ret)
		pOf->dri = OFDRI2ScreenInit(pScreen);

	return ret;
}

void
OFFlushAccel(ScreenPtr pScreen)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	OFPtr pOf = OFPTR(pScrn);
	if (pOf->xa)
		OFFlushXA(pOf);
}
