# bigchainb-guix - Deployment and testing infrastructure for BigchainDB using
# GNU Guix - Functional package management for GNU and
# Guile, Project GNU's extension language

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

extlibdir = $(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions
extlib_LTLIBRARIES = libguile-tendermint-init.la

libguile_tendermint_init_la_SOURCES = tendermint_genkey.c
libguile_tendermint_init_la_CFLAGS = $(GUILE_CFLAGS)
libguile_tendermint_init_la_LIBADD = -lsodium
