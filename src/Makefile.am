AM_CFLAGS = \
	@XORG_CFLAGS@ \
	@XATRACKER_CFLAGS@ \
	-Wall \
	-Werror \
	-I$(top_srcdir)/system-includes/ \
	-I$(top_builddir)/

openfimg_drv_la_LTLIBRARIES = openfimg_drv.la
openfimg_drv_la_LDFLAGS = -module -avoid-version
openfimg_drv_la_LIBADD = @XORG_LIBS@ @XATRACKER_LIBS@
openfimg_drv_ladir = @moduledir@/drivers

openfimg_drv_la_SOURCES = \
	compat-api.h \
	drmmode_display.c \
	of-driver.c \
	of-accel.c \
	of-accel.h \
	of-dri2.c \
	of-pixmap.c \
	of-exa-xa.c

EXTRA_DIST = \
	of.h
