include config

OBJ	= Front/ast.o Front/code.o Front/offsets.o Front/pragma.o \
	  Front/st_dfa.o Front/ident.o Front/lib.o \
	  Front/predlib.o Front/signature.o Front/printline.o \
	  Front/symboltable.o Front/untyped.o Front/timer.o \
	  Front/st_gta.o Front/scanner.o Front/parser.o Front/mona.o \
	  DFA/dfa.o DFA/basic.o DFA/product.o DFA/project.o \
	  DFA/minimize.o DFA/quotient.o DFA/prefix.o DFA/analyze.o \
	  DFA/printdfa.o DFA/makebasic.o DFA/external.o \
	  BDD/bdd.o BDD/bdd_cache.o BDD/bdd_double.o BDD/bdd_manager.o \
	  BDD/bdd_trace.o BDD/hash.o \
	  BDD/long_mem/memrec.o BDD/long_mem/memblock.o \
	  BDD/bdd_external.o BDD/bdd_dump.o \
	  GTA/external.o GTA/analyze.o GTA/restrict.o \
	  GTA/negation.o GTA/copy.o GTA/replace_indices.o \
	  GTA/printgta.o GTA/gta.o GTA/product.o GTA/pairhash.o \
	  GTA/project.o GTA/minimize.o GTA/subsets.o GTA/projset.o \
	  GTA/basic.o GTA/makebasic.o GTA/reachable.o GTA/dyn.o \
	  GTA/analyze_acceptance.o

mona:	FORCE
	(cd Front; $(MAKE))
	(cd BDD; $(MAKE))
	(cd DFA; $(MAKE))
	(cd GTA; $(MAKE))
	(cd Lib; $(MAKE))
	(cd Examples; $(MAKE))
ifeq ($(PURIFY),yes)
	purify $(CXX) $(LDFLAGS) $(OBJ) $(CFLAGS) -lstdc++ -o $@
else
	$(CXX) $(LDFLAGS) $(OBJ) $(CFLAGS) -lstdc++ -o $@
endif
ifneq ($(DEVEL),yes)
	strip $@
endif

depend clean: FORCE 
	(cd Front; $(MAKE) $@)
	(cd BDD; $(MAKE) $@)	
	(cd DFA; $(MAKE) $@)
	(cd GTA; $(MAKE) $@)
	(cd Lib; $(MAKE) $@)
	(cd Examples; $(MAKE) $@)

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
	-name 'COPYRIGHT' -o \
	-name dependencies -o \
	-name makefile -o \
	-name 'mona-mode.el' -o \
	-name 'bdd_volatility' -o \
	-name config \) -a \
	! -name '*~' -a \
	! -name 'scanner.cpp' -a \
	! -name 'parser.h' -a \
	! -name 'parser.cpp'` ;\
	gzip m.tar ;\
	chmod 644 m.tar.gz ;\
	mv m.tar.gz mona-$(VERSION).tar.gz

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
