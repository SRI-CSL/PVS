########################### -*- Mode: Makefile -*- ###########################
## Makefile -- Main Makefile for PVS; requires GNU Make
## Author          : Sam Owre
## Created On      : Wed Dec 30 19:29:47 1998
## Last Modified By: Sam Owre
## Last Modified On: Sun Jan  3 03:00:40 1999
## Update Count    : 21
## Status          : Alpha test
###############################################################################
ifneq (,)
This makefile requires GNU Make.
endif

.SUFFIXES:
.SUFFIXES: .c .o
SHELL=/bin/sh
ifeq ($(origin PVSPATH), undefined)
  PVSPATH = `pwd`
endif

LISPDIR = /csl/allegro/allegro5.0.1
lisp = allegro5.0
TAR = tar
SED = sed
TARFLAGS = -cvohz --mode="u+rw" --atime-preserve --exclude=CVS
PLATFORM = $(shell bin/pvs-platform)
export PLATFORM
ifeq ($(PLATFORM),sun4-SunOS5)
	ifeq ($(origin PVSLISP), undefined)
	  PVSLISP = $(LISPDIR)/lisp
	endif
	LISPEXT=fasl
else
ifeq ($(PLATFORM),ix86-redhat4)
	ifeq ($(origin PVSLISP), undefined)
	  PVSLISP = $(LISPDIR)/lisp
	endif
	LISPEXT=lfasl
else
ifeq ($(PLATFORM),ix86-redhat5)
	ifeq ($(origin PVSLISP), undefined)
	  PVSLISP = $(LISPDIR)/lisp
	endif
	LISPEXT=lfasl
endif
endif
endif

# Determine which versions of Emacs are available
EMACS = "$(shell which emacs)"
XEMACS = "$(shell which xemacs)"

emacsversion = $(shell expr "`emacs --version 2> /dev/null`" \
                            : '[^X]*\(X\?Emacs [0-9]\+\)')
xemacsversion = $(shell expr "`xemacs --version 2> /dev/null`" \
                            : '[^X]*\(X\?Emacs [0-9]\+\)')

ifeq ("$(emacsversion)", "Emacs 20")
EMACS20 = "$(shell which emacs)"
else
emacs20version = $(shell expr "`emacs-20.3 --version 2> /dev/null`" \
                              : '[^X]*\(X\?Emacs [0-9]\+\)')
ifeq ("$(emacs20version)", "Emacs 20")
EMACS20 = "$(shell which emacs-20.3)"
endif
endif

ifeq ("$(emacsversion)", "Emacs 19")
EMACS19 = "$(shell which emacs)"
else
emacs19version = $(shell expr "`emacs-19.34 --version 2> /dev/null`" \
                              : '[^X]*\(X\?Emacs [0-9]\+\)')
ifeq ("$(emacs19version)", "Emacs 19")
EMACS19 = "$(shell which emacs-19.34)"
endif
endif

ifeq ("$(emacsversion)", "XEmacs 20")
XEMACS20 = "$(shell which emacs)"
else
ifeq ("$(xemacsversion)", "XEmacs 20")
XEMACS20 = "$(shell which xemacs)"
else
xemacs20version = $(shell expr "`xemacs-20.4 --version 2> /dev/null`" \
                               : '[^X]*\(X\?Emacs [0-9]\+\)')
ifeq ("$(xemacs20version)", "XEmacs 20")
XEMACS20 = "$(shell which xemacs-20.4)"
endif
endif
endif

ifeq ("$(emacsversion)", "XEmacs 19")
XEMACS19 = "$(shell which emacs)"
else
ifeq ("$(xemacsversion)", "XEmacs 19")
XEMACS19 = "$(shell which xemacs)"
else
xemacs19version = $(shell expr "`xemacs-19.16 --version 2> /dev/null`" \
                               : '[^X]*\(X\?Emacs [0-9]\+\)')
ifeq ("$(xemacs19version)", "XEmacs 19")
XEMACS19 = "$(shell which xemacs-19.16)"
endif
endif
endif

bindir = bin/$(PLATFORM)
pvsfull = ${bindir}/full/pvs-${lisp}
pvsrt = ${bindir}/runtime/pvs-${lisp}

pvs-make-files = pvs.system src/defsystem.lisp src/make-pvs.lisp \
		 src/make-allegro-pvs.lisp src/closopt.lisp

ff-files = src/utils/$(PLATFORM)/file_utils.so \
           BDD/$(PLATFORM)/mu.so \
           src/decision-procedures/polylib/$(PLATFORM)/polylib.so \
           src/WS1S/$(PLATFORM)/ws1s.so \
	   src/ics/$(PLATFORM)/ics.so

ess = ess/dist-ess.lisp \
	ess/init-load.lisp \
	ess/sys/ergolisp/rel/ergolisp.lisp \
	ess/sys/ergolisp/rel/ergolisp-exports.lisp \
	ess/sys/ergolisp/rel/ergo-system.lisp \
	ess/sys/tools/rel/retry.lisp \
	ess/sys/tools/rel/box-system.lisp \
	ess/sys/tools/rel/box.lisp \
	ess/box-defs.lisp \
	ess/sys/ergolisp/rel/ergo-types.lisp \
	ess/sys/ergolisp/rel/type-check.lisp \
	ess/sys/tools/rel/regression-test.lisp \
	ess/sys/tools/rel/clet.lisp \
	ess/sys/tools/rel/box-lib.lisp \
	ess/sys/tools/rel/print-utils.lisp \
	ess/sys/ergolisp/rel/dlambda.lisp \
	ess/sys/ergolisp/rel/dlambda-lib.lisp \
	ess/term/language/rel/languages.lisp \
	ess/term/terms/rel/opers.lisp \
	ess/term/terms/rel/occur.lisp \
	ess/term/terms/rel/sorts.lisp \
	ess/term/trep/rel/attr-prims.lisp \
	ess/term/trep/rel/gterm.lisp \
	ess/term/terms/rel/terms.lisp \
	ess/term/terms/rel/termop.lisp \
	ess/lang/sb-term/rel/sbrt-lang-def.lisp \
	ess/lang/sb-term/rel/sbrt-sorting.lisp \
	ess/lang/sb-term/rel/rt-structs.lisp \
	ess/lang/sb-term/rel/rt-unp-structs.lisp \
	ess/lang/sb-term/rel/rt-lex.lisp \
	ess/lang/sb-term/rel/rt-term.lisp \
	ess/lang/sb-term/rel/rt-parse-mac.lisp \
	ess/lang/sb-term/rel/rt-parse.lisp \
	ess/lang/sb-term/rel/rt-unparse.lisp \
	ess/term/attr/rel/attr-lang-lib.lisp \
	ess/term/attr/rel/attr-sort.lisp \
	ess/term/attr/rel/attr-lang.lisp \
	ess/term/attr/rel/attr-global.lisp \
	ess/lang/sb-term/rel/rt-unp-attr.lisp \
	ess/lang/sb-term/rel/rt-format.lisp \
	ess/lang/sb-term/rel/rt-unp-tex.lisp \
	ess/lang/sb-term/rel/rt-unp-top.lisp \
	ess/term/attr/rel/attr-lib.lisp \
	ess/term/attr/rel/attr-gsort.lisp \
	ess/term/attr/rel/attr-occ.lisp \
	ess/lang/sb-term/rel/sbrt-lang-def.lisp \
	ess/lang/sb-term/rel/sbrt-sorting.lisp \
	ess/lang/sb-term/rel/rt-structs.lisp \
	ess/lang/sb-term/rel/rt-unp-structs.lisp \
	ess/lang/sb-term/rel/rt-lex.lisp \
	ess/lang/sb-term/rel/rt-term.lisp \
	ess/lang/sb-term/rel/rt-parse-mac.lisp \
	ess/lang/sb-term/rel/rt-parse.lisp \
	ess/lang/sb-term/rel/rt-unparse.lisp \
	ess/lang/sb-term/rel/rt-unp-attr.lisp \
	ess/lang/sb-term/rel/rt-format.lisp \
	ess/lang/sb-term/rel/rt-unp-tex.lisp \
	ess/lang/sb-term/rel/rt-unp-top.lisp \
	ess/lang/ab-term/rel/af-runtime.lisp \
	ess/sys/constr/rel/constr.lisp \
	ess/sys/constr/rel/constr-term-rep.lisp \
	ess/sys/constr/rel/constr-sorts.lisp \
	ess/sys/constr/rel/constr-lexer.lisp \
	ess/sys/constr/rel/constr-parser.lisp \
	ess/sys/constr/rel/defsconstr.lisp \
	ess/sys/ergolisp/rel/tdefun.lisp

pvs-parser-in = src/make-pvs-parser.lisp \
             src/pvs-gr.txt \
             src/ergo-runtime-fixes.lisp \
             src/pvs-lang-def.lisp \
             src/pvs-parse-fixes.lisp

pvs-parser-out = src/pvs-lexer.lisp \
             src/pvs-parser.lisp \
             src/pvs-sorts.lisp

ground-prover = src/ground-prover/prmacros.lisp \
		src/ground-prover/interface.lisp \
		src/ground-prover/prglobals.lisp \
		src/ground-prover/process.lisp \
		src/ground-prover/arrays.lisp \
		src/ground-prover/tuples.lisp \
		src/ground-prover/arith.lisp \
		src/ground-prover/q.lisp \
		src/ground-prover/nonlin.lisp \
		src/ground-prover/pvs-patch.lisp \

pvs-src = src/closopt.lisp \
          src/store-object.lisp \
	  src/defcl.lisp \
          src/classes-expr.lisp \
          src/classes-decl.lisp \
          src/prover/estructures.lisp \
          src/pvs-methods.lisp \
          src/utils/hashfn.lisp \
          src/utils/file-utils.lisp \
          src/macros.lisp \
          src/globals.lisp \
          src/optimize.lisp \
	  src/makes.lisp \
	  src/parse.lisp \
	  src/pp.lisp \
          src/pp-tex.lisp \
          src/print-object.lisp \
	  src/equalities.lisp \
          src/utils.lisp \
          src/gensubst.lisp \
	  src/substit.lisp \
	  src/freeparams.lisp \
	  src/subst-mod-params.lisp \
	  src/tc-unify.lisp \
	  src/resolve.lisp \
	  src/tcc-gen.lisp \
	  src/set-type.lisp \
	  src/check-for-tccs.lisp \
          src/tcexprs.lisp \
	  src/tcdecls.lisp \
	  src/judgements.lisp \
	  src/xref.lisp \
	  src/occurs-in.lisp \
	  src/context.lisp \
	  src/datatype.lisp \
	  src/typecheck.lisp \
	  src/tclib.lisp \
          src/compare.lisp \
	  src/untypecheck.lisp \
	  src/copy-lex.lisp \
	  src/save-theories.lisp \
          src/pvs.lisp \
          src/status-cmds.lisp \
	  src/add-decl.lisp \
	  src/list-decls.lisp \
	  src/tex-support.lisp

emacs-interface = src/interface/pvs-emacs.lisp \
		  src/interface/emacs-calls.lisp \
		  src/interface/cl-ilisp.lisp \
		  src/interface/allegro.lisp \
		  src/interface/ilisp-pkg.lisp

pvs-prover = src/prover/checker-macros.lisp \
         src/prover/decision-procedure-interface.lisp \
         src/prover/translate-to-prove.lisp \
         src/prover/eproofcheck.lisp \
         src/prover/proofrules.lisp \
	 src/prover/equantifiers.lisp \
	 src/prover/freevars.lisp \
	 src/prover/rewrites.lisp \
	 src/prover/assert.lisp \
	 src/prover/beta-reduce.lisp \
	 src/prover/replace.lisp \
	 src/prover/expand.lisp \
	 src/prover/match.lisp \
	 src/prover/rules.lisp \
         src/prover/strategies.lisp \
         src/prover/wish.lisp

bddlisp = BDD/bdd.lisp BDD/mu.lisp

decision-procedures = src/decision-procedures/types.lisp \
                      src/decision-procedures/node-structures.lisp \
                      src/decision-procedures/interpreted-node-structures.lisp \
                      src/decision-procedures/cong-state.lisp \
                      src/decision-procedures/translate-to-dc.lisp \
                      src/decision-procedures/translate-from-dc.lisp \
                      src/decision-procedures/polylib/foreign.lisp \
                      src/decision-procedures/integer.lisp \
                      src/decision-procedures/arithmetic.lisp \
                      src/decision-procedures/arrays.lisp \
                      src/decision-procedures/shostak.lisp \
                      src/decision-procedures/shostak-interp.lisp \
                      src/decision-procedures/boolean.lisp \
                      src/decision-procedures/arith-solve.lisp \
                      src/decision-procedures/polyhedron.lisp \
                      src/decision-procedures/fourier-motzkin.lisp \
                      src/decision-procedures/rewrite.lisp \
                      src/decision-procedures/rewrite-pvs.lisp \
                      src/decision-procedures/datatypes-pvs.lisp \
                      src/decision-procedures/datatypes.lisp \
                      src/decision-procedures/records.lisp \
                      src/decision-procedures/rewrite-optimize.lisp \
                      src/decision-procedures/simplify.lisp \
                      src/decision-procedures/forward-chain-pvs.lisp \
                      src/decision-procedures/forward-chain.lisp \
                      src/decision-procedures/bit-vectors/bvec_structure.lisp \
                      src/decision-procedures/bit-vectors/bvec_arith.lisp \
                      src/decision-procedures/bit-vectors/bvec_fixed_solver.lisp \
                      src/decision-procedures/bit-vectors/bvec_bdd_solve.lisp \
                      src/decision-procedures/bit-vectors/bvec_slicing.lisp \
                      src/decision-procedures/polylib/dp-foreign.lisp

ws1slisp = src/WS1S/lisp/dfa-foreign.lisp src/WS1S/lisp/dfa.lisp \
           src/WS1S/lisp/pvs-utils.lisp src/WS1S/lisp/symtab.lisp \
           src/WS1S/lisp/signature.lisp src/WS1S/lisp/pvs2dfa.lisp \
           src/WS1S/lisp/ws1s-strategy.lisp

qelisp = src/qe/qe-utils.lisp \
         src/qe/dp-interface.lisp \
         src/qe/dnf.lisp \
         src/qe/qe.lisp

follisp = src/fol/fol.lisp \
          src/fol/fol-top.lisp

abstractionlisp = src/abstraction/abstract.lisp

groundevallisp = src/groundeval/eval-macros.lisp \
		 src/groundeval/eval-utils.lisp \
		 src/groundeval/ground-expr.lisp \
		 src/groundeval/static-update.lisp \
		 src/groundeval/pvseval-update.lisp \
		 src/groundeval/cl2pvs.lisp \
		 src/groundeval/generate-lisp-for-theory.lisp

inst-by-unif-lisp = src/inst-by-unif/unify.lisp \
                    src/inst-by-unif/herbrandize.lisp \
                    src/inst-by-unif/gensubsts.lisp \
                    src/inst-by-unif/inst-by-unif.lisp

ics-lisp = src/ics/ics.lisp src/ics/shostak.lisp

lisp-files = ${pvs-parser-out} ${ground-prover} ${pvs-src} \
	     ${emacs-interface} ${pvs-prover} ${bddlisp} \
	     ${decision-procedures} ${ws1slisp} ${groundevallisp} \
             ${inst-by-unif-lisp} ${qelisp} ${follisp} ${abstractionlisp} \
	     ${ics-lisp}

templatedeps = pvs src/make-allegro-pvs.lisp pvs.system \
               ess/dist-ess.lisp ess/allegro-runtime.lisp

all-lisp-files = ${ess} ${pvs-parser-in} ${lisp-files}

fasl-files = $(lisp-files:.lisp=.$(LISPEXT))

all-fasl-files = $(filter %.$(LISPEXT), $(all-lisp-files:.lisp=.$(LISPEXT)))

ifneq ($(strip $(EMACS20)),)
emacs20-elc = $(pvs-emacs20-src:.el=.elc) $(ilisp-emacs20-src:.el=.elc)
emacs += emacs/emacs20/pvs-load.elc
endif

ifneq ($(strip $(EMACS19)),)
emacs19-elc = $(pvs-emacs19-src:.el=.elc) $(ilisp-emacs19-src:.el=.elc)
emacs += emacs/emacs19/pvs-load.elc
endif

ifneq ($(strip $(XEMACS20)),)
xemacs20-elc = $(pvs-xemacs20-src:.el=.elc) $(ilisp-xemacs20-src:.el=.elc)
emacs += emacs/xemacs20/pvs-load.elc
endif

ifneq ($(strip $(XEMACS19)),)
xemacs19-elc = $(pvs-xemacs19-src:.el=.elc) $(ilisp-xemacs19-src:.el=.elc)
emacs += emacs/xemacs19/pvs-load.elc
endif

all : ${templatedeps} full

pvs : pvs.template
	@echo "Run make install [PVSPATH=cdir]"
	@echo "where cdir is the current directory (this is not done automatically"
	@echo "since pwd may not give the right values for mounted directories)"
	rm -f pvs
	exit 1

src/make-allegro-pvs.lisp : src/make-allegro-pvs.lisp.template
	@echo "Run make install [PVSPATH=cdir]"
	@echo "where cdir is the current directory (this is not done automatically"
	@echo "since pwd may not give the right values for mounted directories)"
	rm src/make-allegro-pvs.lisp
	exit 1

pvs.system : pvs.system.template
	@echo "Run make install [PVSPATH=cdir]"
	@echo "where cdir is the current directory (this is not done automatically"
	@echo "since pwd may not give the right values for mounted directories)"
	rm -f pvs.system
	exit 1

ess/dist-ess.lisp : ess/dist-ess.lisp.template
	@echo "Run make install [PVSPATH=cdir]"
	@echo "where cdir is the current directory (this is not done automatically"
	@echo "since pwd may not give the right values for mounted directories)"
	rm ess/dist-ess.lisp
	exit 1

ess/allegro-runtime.lisp : ess/allegro-runtime.lisp.template
	@echo "Run make install [PVSPATH=cdir]"
	@echo "where cdir is the current directory (this is not done automatically"
	@echo "since pwd may not give the right values for mounted directories)"
	rm ess/allegro-runtime.lisp
	exit 1

full : makefileutils makebdd makepolylib makews1s ${pvsfull} ${emacs}

runtime : makefileutils makebdd makepolylib makews1s ${pvsrt} ${emacs}

${pvsfull} : ${pvs-make-files} ${ess} ${ff-files} ${lisp-files} \
             lib/prelude.pvs lib/prelude.prf
	rm -rf ${bindir}/full
	umask 002; \
	$(PVSLISP) -e '(load "pvs.system")' \
		-e "(let ((code 1)) \
                      (unwind-protect \
                          (multiple-value-bind (v err) \
                            (ignore-errors (operate-on-system :pvs :compile)) \
                               (if err \
                                   (let ((*print-readably* nil)) \
                                     (format t \"~a\" err)) \
                                   (setq code 0)))) \
                        (excl:exit code)))"
	umask 002; etags ${lisp-files} ${pvs-emacs-src}
	touch src/pvs-lexer.${LISPEXT}
	umask 002; \
	$(PVSLISP) -e '(defvar *runtime* nil)' \
		-L src/make-pvs.lisp
	touch ${pvsfull}

${pvsrt} : ${pvs-make-files} ${ess} ${ff-files} ${lisp-files} \
           lib/prelude.pvs lib/prelude.prf
	rm -rf ${bindir}/runtime
	umask 002; \
	$(PVSLISP) -e '(load "pvs.system")' \
		-e "(let ((code 1)) \
                      (unwind-protect \
                          (multiple-value-bind (v err) \
                            (ignore-errors (operate-on-system :pvs :compile)) \
                               (if err \
                                   (let ((*print-readably* nil)) \
                                     (format t \"~a\" err)) \
                                   (setq code 0)))) \
                        (excl:exit code)))"
	umask 002; etags ${lisp-files} ${pvs-emacs-src}
	touch src/pvs-lexer.${LISPEXT}
	umask 002; \
	$(PVSLISP) -e '(defvar *runtime* t)' \
		-L src/make-pvs.lisp
	touch ${pvsrt}

src/pvs-lexer.lisp : ${pvs-parser-in}
	umask 002; \
	$(PVSLISP) -L src/make-pvs-parser

src/pvs-methods.lisp : src/make-pvs-methods.lisp src/defcl.lisp \
                       src/classes-expr.lisp src/classes-decl.lisp
	umask 002; \
	$(PVSLISP) -e '(defvar *pvs-path* "${PWD}")' \
		-L src/make-pvs-methods.lisp

${LISP} :
	@echo "Need to set the LISP variable in Makefile to a valid"
	@echo "allegro 5.0 lisp executable for this platform"

.PHONY: all makefileutils makebdd makepolylib makews1s makeics install clean
makefileutils :
	umask 002; \
	$(MAKE) -C src/utils/$(PLATFORM)
makebdd :
	umask 002; \
	$(MAKE) -C BDD/$(PLATFORM)
makepolylib :
	umask 002; \
	$(MAKE) -C src/decision-procedures/polylib/$(PLATFORM)
makews1s :
	umask 002; \
	$(MAKE) -C src/WS1S/$(PLATFORM)

makeics :
	umask 002; \
	$(MAKE) -C src/ics/$(PLATFORM)

image-tar = pvs-$(PLATFORM).tgz

system-tar = pvs-system.tgz

emacs19-tar = pvs-emacs19.tgz

tarfiles : ${image-tar} ${system-tar} ${emacs19-tar}

image-files = $(wildcard ${bindir}/runtime/*)

system-files = README pvs pvs-tex.sub pvs.sty bin/relocate bin/pvs-platform \
	       Examples \
               lib/prelude.pvs lib/prelude.prf lib/list_adt.pvs \
               lib/ordstruct_adt.pvs lib/character_adt.pvs \
               lib/pvs-language.help lib/pvs-prover.help lib/pvs.help \
               lib/pvs.grammar lib/pvs.bnf wish/pvs-support.tcl \
               wish/gray.xbm wish/sequent.xbm \
               emacs/README emacs/go-pvs.el $(pvs-emacs-src) \
               $(ilisp-emacs-src) emacs/emacs20 emacs/xemacs20

emacs19-files = emacs/emacs19 emacs/xemacs19

${image-tar} : ${pvsrt}
	$(TAR) $(TARFLAGS) -f ${image-tar} ${image-files}

${system-tar} : ${system-files}
	$(TAR) $(TARFLAGS) -f ${system-tar} ${system-files}

${emacs19-tar} : $(emacs19-files)
	$(TAR) $(TARFLAGS) -f ${emacs19-tar} ${emacs19-files}

install :
	# Relocate PVSPATH in the \"pvs\" shell script to $(PVSPATH)
	if [ -f pvs.template ]; then \
	  $(SED) -e "s,^PVSPATH=.*$$,PVSPATH=$(PVSPATH)," < pvs.template > pvs; \
	else \
	  $(SED) -e "s,^PVSPATH=.*$$,PVSPATH=$(PVSPATH)," < pvs > /tmp/pvs.tmp; \
	  mv /tmp/pvs.tmp pvs; \
	fi
	chmod a+x pvs
	# Relocate *pvs-path* in src/make-allegro-pvs.lisp to $(PVSPATH)
	if [ -f src/make-allegro-pvs.lisp.template ]; then \
	  $(SED) -e "/^(defvar \*pvs-path\*/s,\"[^\"]*\",\"$(PVSPATH)\"," \
	      src/make-allegro-pvs.lisp.template > src/make-allegro-pvs.lisp; \
	fi
	# Relocate *pvs-path* in pvs.system to $(PVSPATH)
	if [ -f pvs.system.template ]; then \
	  $(SED) -e "/(defparameter \*pvs-path\*/s,\"[^\"]*\",\"$(PVSPATH)\"," \
	      pvs.system.template > pvs.system; \
	fi
	# Relocate *ess-path* in ess/dist-ess.lisp to $(PVSPATH)/ess
	if [ -f ess/dist-ess.lisp.template ]; then \
	  $(SED) -e "/(defvar \*ess-path\*/s,\"[^\"]*\",\"$(PVSPATH)/ess\"," \
	      ess/dist-ess.lisp.template > ess/dist-ess.lisp; \
	fi
	# Relocate *ess-path* in ess/allegro-runtime.lisp to $(PVSPATH)/ess
	if [ -f ess/allegro-runtime.lisp.template ]; then \
	  $(SED) -e "/(defvar \*ess-path\*/s,\"[^\"]*\",\"$(PVSPATH)/ess\"," \
	      ess/allegro-runtime.lisp.template > ess/allegro-runtime.lisp; \
	fi

clean : 
	rm -f ${all-fasl-files}
	$(MAKE) -C src/utils/$(PLATFORM) clean
	$(MAKE) -C BDD/$(PLATFORM) clean
	$(MAKE) -C src/decision-procedures/polylib/$(PLATFORM) clean
	$(MAKE) -C src/WS1S/$(PLATFORM) clean
	$(MAKE) -C src/ics/$(PLATFORM) clean

pvs-emacs-src = $(wildcard emacs/emacs-src/*.el)
ilisp-emacs-src = $(wildcard emacs/emacs-src/ilisp/*.el)

pvs-emacs20-src = $(addprefix emacs/emacs20/,$(notdir ${pvs-emacs-src}))
pvs-emacs19-src = $(addprefix emacs/emacs19/,$(notdir ${pvs-emacs-src}))
pvs-xemacs20-src = $(addprefix emacs/xemacs20/,$(notdir ${pvs-emacs-src}))
pvs-xemacs19-src = $(addprefix emacs/xemacs19/,$(notdir ${pvs-emacs-src}))

ilisp-emacs20-src = $(addprefix emacs/emacs20/,$(notdir ${ilisp-emacs-src}))
ilisp-emacs19-src = $(addprefix emacs/emacs19/,$(notdir ${ilisp-emacs-src}))
ilisp-xemacs20-src = $(addprefix emacs/xemacs20/,$(notdir ${ilisp-emacs-src}))
ilisp-xemacs19-src = $(addprefix emacs/xemacs19/,$(notdir ${ilisp-emacs-src}))

emacs/emacs20/%.el : emacs/emacs-src/%.el
	(cd emacs/emacs20; ln -sf ../emacs-src/$(@F) .)

emacs/emacs19/%.el : emacs/emacs-src/%.el
	(cd emacs/emacs19; ln -sf ../emacs-src/$(@F) .)

emacs/xemacs20/%.el : emacs/emacs-src/%.el
	(cd emacs/xemacs20; ln -sf ../emacs-src/$(@F) .)

emacs/xemacs19/%.el : emacs/emacs-src/%.el
	(cd emacs/xemacs19; ln -sf ../emacs-src/$(@F) .)

emacs/emacs20/%.el : emacs/emacs-src/ilisp/%.el
	(cd emacs/emacs20; ln -sf ../emacs-src/ilisp/$(@F) .)

emacs/emacs19/%.el : emacs/emacs-src/ilisp/%.el
	(cd emacs/emacs19; ln -sf ../emacs-src/ilisp/$(@F) .)

emacs/xemacs20/%.el : emacs/emacs-src/ilisp/%.el
	(cd emacs/xemacs20; ln -sf ../emacs-src/ilisp/$(@F) .)

emacs/xemacs19/%.el : emacs/emacs-src/ilisp/%.el
	(cd emacs/xemacs19; ln -sf ../emacs-src/ilisp/$(@F) .)

ifeq ($(shell if [ -x ${emacs20} ] ; then echo OK ; fi), OK)
emacs/emacs20/pvs-load.elc : ${pvs-emacs20-src} ${ilisp-emacs20-src}
	(cd emacs/emacs20; $(EMACS20) -batch -l pvs-byte-compile.el)
endif

ifeq ($(shell if [ -x ${emacs19} ] ; then echo OK ; fi), OK)
emacs/emacs19/pvs-load.elc : ${pvs-emacs19-src} ${ilisp-emacs19-src}
	(cd emacs/emacs19; $(EMACS19) -batch -l pvs-byte-compile.el)
endif

ifeq ($(shell if [ -x ${xemacs20} ] ; then echo OK ; fi), OK)
emacs/xemacs20/pvs-load.elc : ${pvs-xemacs20-src} ${ilisp-xemacs20-src}
	(cd emacs/xemacs20; $(XEMACS20) -batch -l pvs-byte-compile.el)
endif

ifeq ($(shell if [ -x ${xemacs19} ] ; then echo OK ; fi), OK)
emacs/xemacs19/pvs-load.elc : ${pvs-xemacs19-src} ${ilisp-xemacs19-src}
	(cd emacs/xemacs19; $(XEMACS19) -batch -l pvs-byte-compile.el)
endif
