# bigchainb-guix - Deployment and testing infrastructure for BigchainDB
# using GNU Guix - Functional package management for GNU and Guile,
# Project GNU's extension language.

# Parts of this automake recipe borrowed from: GNU Guix and from
# Erik Edrosa <erik.edrosa@gmail.com> writings.
#
# Copyright (C) 2020 David Dashyan <mail@davie.li>
#
# This file is part of bigchaindb-guix
#
# bigchaindb-guix is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# bigchaindb-guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with bigchaindb-guix. If not, see https://www.gnu.org/licenses/.

## Guile related stuff
moddir=$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

GOBJECTS = $(SOURCES:%.scm=%.go)

nobase_dist_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_dist_modDATA

GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILD)	\
	compile $(GUILE_WARNINGS) -o "$@" "$<"

clean-go:
	-$(RM) $(GOBJECTS)

.PHONY: clean-go

## Dist

SOURCES =						\
  bigchaindb-guix/deployment/tendermint-init.scm	\
  bigchaindb-guix/os/node.scm				\
  bigchaindb-guix/services/bigchaindb.scm		\
  bigchaindb-guix/packages/bigchaindb.scm		\
  bigchaindb-guix/packages/tendermint.scm

TESTS =						\
	tests/services.scm

TEST_EXTENSIONS = .scm

AM_TESTS_ENVIRONMENT =					\
  abs_top_srcdir="$(abs_top_srcdir)"			\
  LD_LIBRARY_PATH="$(abs_top_srcdir)/extensions"	\
  GUILE_AUTO_COMPILE=0

# SCM_LOG_COMPILER = $(GUILE)
# AM_SCM_LOG_FLAGS = --no-auto-compile -L $(top_srcdir)

SCM_LOG_DRIVER =				\
  $(top_builddir)/pre-inst-env			\
  $(GUILE) --no-auto-compile -e main		\
  $(top_srcdir)/build-aux/test-driver.scm

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)			\
  *.log *.tar.g

EXTRA_DIST =					\
  $(SOURCES)					\
  $(TESTS)					\
  README					\
  activate					\
  bootstrap					\
  build-aux/pre-inst-env.in			\
  build-aux/test-driver.scm			\
  guix.scm					\
  m4/guile.m4					\
  system.scm

SUBDIRS = extensions
