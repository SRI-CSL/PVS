############################ -*- Mode: Makefile -*- ###########################
## Makefile -- Main Makefile for PVS; requires GNU Make
## Author          : Sam Owre
## Created On      : Wed Dec 30 19:29:47 1998
## Last Modified By: Sam Owre
## Last Modified On: Thu Dec 31 21:12:20 1998
## Update Count    : 16
## Status          : Alpha test
###############################################################################
.SUFFIXES:
.SUFFIXES: .c .o
SHELL=/bin/sh
lisp = allegro5.0
PLATFORM = $(shell bin/pvs-platform)
export PLATFORM
ifeq ($(PLATFORM),sun4-SunOS5)
	LISP = /pkg/allegro5.0/sol24/lisp
else
ifeq ($(PLATFORM),ix86-redhat4)
	LISP=/pkg/allegro5.0/redhat4/lisp
else
ifeq ($(PLATFORM),ix86-redhat5)
	LISP=/pkg/allegro5.0/redhat5/lisp
else
	LISP = lisp
endif
endif
endif
bindir = bin/$(PLATFORM)
binfiles = pvs-${lisp}-full pvs-${lisp} atob btoa chunk
pvsfull = ${bindir}/full/pvs-${lisp}
pvsrt = ${bindir}/runtime/pvs-${lisp}

pvs-make-files = pvs.system src/defsystem.lisp src/make-pvs.lisp \
		 src/closopt.lisp

ff-files = BDD/$(PLATFORM)/bdd.so BDD/$(PLATFORM)/mu.so \
           src/decision-procedures/polylib/$(PLATFORM)/polylib.so

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
             src/ergo-tex-fixes.lisp \
             src/pvs-lang-def.lisp \
             src/pvs-parse-fixes.lisp

pvs-parser-out = src/pvs-lexer.lisp \
             src/pvs-parser.lisp \
             src/pvs-unparser.lisp \
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

pvs-src = src/store-object.lisp \
	  src/defcl.lisp \
          src/classes-expr.lisp \
          src/classes-decl.lisp \
          src/prover/estructures.lisp \
          src/pvs-methods.lisp \
          src/utils/hashfn.lisp \
          src/macros.lisp \
          src/globals.lisp \
          src/optimize.lisp \
	  src/makes.lisp \
	  src/parse.lisp \
	  src/unparse.lisp \
	  src/pp.lisp \
          src/print-object.lisp \
	  src/pvs-emacs.lisp \
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
	  src/tex-support.lisp \

pvs-prover = src/prover/checker-macros.lisp \
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

bddlisp = BDD/bdd.lisp BDD/mu.lisp BDD/strategies.lisp

decision-procedures = src/decision-procedures/types.lisp \
                      src/decision-procedures/node-structures.lisp \
                      src/decision-procedures/translate-to-dc.lisp \
                      src/decision-procedures/translate-from-dc.lisp \
                      src/decision-procedures/polylib/foreign.lisp \
                      src/decision-procedures/integer.lisp \
                      src/decision-procedures/arithmetic.lisp \
                      src/decision-procedures/arrays.lisp \
                      src/decision-procedures/shostak.lisp \
                      src/decision-procedures/shostak-interp.lisp \
                      src/decision-procedures/arith-solve.lisp \
                      src/decision-procedures/polyhedron.lisp \
                      src/decision-procedures/rewrite.lisp \
                      src/decision-procedures/rewrite-pvs.lisp \
                      src/decision-procedures/datatypes-pvs.lisp \
                      src/decision-procedures/records.lisp \
                      src/decision-procedures/rewrite-optimize.lisp \
                      src/decision-procedures/simplify.lisp \
                      src/decision-procedures/forward-chain-pvs.lisp \
                      src/decision-procedures/forward-chain.lisp

all : makebdd makepolylib ${pvsfull} ${pvsrt}

${pvsfull} : ${pvs-make-files} ${ff-files} ${ess} ${pvs-parser} \
             ${ground-prover} ${pvs-src} ${pvs-prover} ${bddlisp} \
             ${decision-procedures}
	rm -rf ${bindir}/full
	$(LISP) -e '(defvar *runtime* nil)' \
		-L src/make-pvs.lisp
	touch ${pvsfull}

${pvsrt} : ${pvs-make-files} ${ff-files} ${ess} ${pvs-parser} \
           ${ground-prover} ${pvs-src} ${pvs-prover} ${bddlisp} \
           ${decision-procedures}
	rm -rf ${bindir}/runtime
	$(LISP) -e '(defvar *runtime* t)' \
		-L src/make-pvs.lisp
	touch ${pvsrt}

${pvs-parser-out} : ${pvs-parser-in}
	$(LISP) -e '(load "src/make-pvs-parser")'

src/pvs-methods.lisp : src/make-pvs-methods.lisp src/defcl.lisp \
                       src/classes-expr.lisp src/classes-decl.lisp
	$(LISP) -e '(defvar *pvs-path* "${PWD}")' \
		-L src/make-pvs-methods.lisp

.PHONY: all makebdd makepolylib
makebdd :
	$(MAKE) -C BDD/$(PLATFORM)
makepolylib :
	$(MAKE) -C src/decision-procedures/polylib/$(PLATFORM)
