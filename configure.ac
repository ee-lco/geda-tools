AC_INIT([geda-ruby], [1.0])
AC_PREREQ([2.60])

AC_CONFIG_SRCDIR([src/geda-ruby.c])
AC_CONFIG_AUX_DIR([build-tools])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_HEADER([config.h])

AM_INIT_AUTOMAKE([parallel-tests color-tests])
AM_SILENT_RULES

LT_INIT


AC_PROG_CC
AC_PROG_CPP

AM_PROG_CC_C_O

AX_WITH_PROG([RUBY], [ruby])
AX_RUBY_DEVEL


PKG_PROG_PKG_CONFIG

PKG_CHECK_MODULES(GLIB, [glib-2.0 >= 2.20.0], ,
  AC_MSG_ERROR([GLib 2.20.0 or later is required.]))

#PKG_CHECK_MODULES(GTK, [gtk+-2.0 >= 2.18.0], ,
#  AC_MSG_ERROR([GTK+ 2.18.0 or later is required.]))

#PKG_CHECK_MODULES(GDK, [gdk-2.0 >= 2.18.0], ,
#  AC_MSG_ERROR([GDK 2.18.0 or later is required.]))

PKG_CHECK_MODULES(GDK_PIXBUF, [gdk-pixbuf-2.0 >= 2.18.0], ,
  AC_MSG_ERROR([GDK_PIXBUF 2.18.0 or later is required.]))


AC_CONFIG_FILES([
    Makefile

    src/Makefile
])

AC_OUTPUT

