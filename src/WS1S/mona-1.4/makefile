#
# MONA
# Copyright (C) 1997-2000 BRICS.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the  Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.
#

include config

OBJ	= Front/front.a GTA/gta.a DFA/dfa.a BDD/bdd.a Mem/mem.a

mona:	FORCE
	@$(MAKE) -C Mem
	@$(MAKE) -C Front
	@$(MAKE) -C BDD
	@$(MAKE) -C DFA
	@$(MAKE) -C GTA
	@$(MAKE) -C Lib
	@$(MAKE) -C Examples
ifeq ($(PURIFY),yes)
	purify $(CXX) $(LDFLAGS) $(OBJ) $(CFLAGS) -lstdc++ -o $@
else 
	$(CXX) $(LDFLAGS) $(OBJ) $(CFLAGS) -lstdc++ -o $@
endif
ifneq ($(DEVEL),yes)
	strip $@
endif

depend clean: FORCE 
	@$(MAKE) -C Mem $@
	@$(MAKE) -C Front $@
	@$(MAKE) -C BDD $@
	@$(MAKE) -C DFA $@
	@$(MAKE) -C GTA $@
	@$(MAKE) -C Lib $@
	@$(MAKE) -C Examples $@

backup:	FORCE 
	cd ..;\
	tar cvf m.tar mona-$(VERSION)/Examples/*.mona \
	`find mona-$(VERSION) \
	\( -name '*.h' -o \
	-name '*.cpp' -o \
	-name '*.c' -o \
	-name '*.l' -o \
	-name '*.y' -o \
	-name 'README' -o \
	-name 'COPYING' -o \
	-name dependencies -o \
	-name makefile -o \
	-name 'mona-mode.el' -o \
	-name 'bdd_volatility' -o \
	-name 'mona.spec' -o \
	-name config \) -a \
	! -name '*~' -a \
	! -name 'scanner.cpp' -a \
	! -name 'parser.h' -a \
	! -name 'parser.cpp'` ;\
	gzip m.tar ;\
	chmod 644 m.tar.gz ;\
	mv m.tar.gz mona-$(VERSION)-$(RELEASE).tar.gz

rpm:	backup
	cd ..; rpm -ta mona-$(VERSION)-$(RELEASE).tar.gz

wc:	FORCE
	wc -l \
	`find . \
	\( -name '*.h' -o \
	-name '*.cpp' -o \
	-name '*.c' -o \
	-name '*.l' -o \
	-name '*.y' -o \
	-name makefile -o \
	-name config \) -a \
	! -name 'scanner.cpp' -a \
	! -name 'parser.h' -a \
	! -name 'parser.cpp' `

FORCE:
