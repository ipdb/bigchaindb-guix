dnl                                               -*- Autoconf -*-
dnl Process this file with autoconf to produce a configure script.

AC_INIT([bigchaindb-guix], [0.1.0])
AC_CONFIG_SRCDIR([bigchaindb-guix/os/node.scm])
AC_CONFIG_AUX_DIR([build-aux])
AC_REQUIRE_AUX_FILE([test-driver.scm], [pre-inst-env.in])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([-Wall -Werror foreign subdir-objects])

GUILE_PKG([3.0])
GUILE_PROGS
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(
        ['guild' binary not found; please check your guile installation.])
fi
GUILE_MODULE_REQUIRED([guix])
GUILE_MODULE_REQUIRED([json])
dnl GUILE_MODULE_REQUIRED([gcrypt])
dnl FIXME gcrypt check fails

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([pre-inst-env:build-aux/pre-inst-env.in],
        [chmod +x pre-inst-env])

AC_CONFIG_HEADER([extensions/config.h])
AC_CONFIG_FILES([extensions/Makefile])

AC_CHECK_HEADERS([sodium.h])

AC_PROG_CC
AM_PROG_AR
LT_INIT

GUILE_CFLAGS=`guile-config compile`
GUILE_LIBS=`guile-config link`
AC_SUBST(GUILE_CFLAGS)
AC_SUBST(GUILE_LIBS)

AC_OUTPUT
