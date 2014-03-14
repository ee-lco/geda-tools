## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libgedaruby.la

# don't forget all *.h files */
libgedaruby_la_SOURCES = \
	geda-ruby.c

libgedaruby_la_CPPFLAGS = $(RUBY_CPPFLAGS)
libgedaruby_la_CFLAGS = $(GCC_CFLAGS) $(GLIB_CFLAGS) $(GDK_PIXBUF_CFLAGS)
libgedaruby_la_LDFLAGS = -module -avoid-version -shared
libgedaruby_la_LIBADD = -lgeda $(GLIB_LIBS) $(GDK_PIXBUF_LIBS) $(RUBY_LDFLAGS) $(RUBY_EXTRA_LIBS)

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in

