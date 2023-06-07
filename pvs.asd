;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs.asd
;; Author          : Sam Owre
;; The ASDF file for building PVS; ASDF is the de facto standard build
;; facility for Common Lisp see https://common-lisp.net/project/asdf/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) 2006-2021, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

;; The asdf file for PVS

;; To find PVS locally, create the file
;;     ~/.config/common-lisp/source-registry.conf.d/20-pvs.conf
;; with contents
;;   (:tree "PVS DIRECTORY")

;; Install quicklisp; let it add the following to your ~/.sbclrc:
;;  ;;; The following lines added by ql:add-to-init-file:
;;  #-quicklisp
;;  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
;;                                         (user-homedir-pathname))))
;;    (when (probe-file quicklisp-init)
;;      (load quicklisp-init)))

;; Note that this relies on a source-registry, which must be cleared before
;; dumping an image using clear-configuration.

;; (require "asdf")
;; (asdf:operate :load-op :pvs) same as (asdf:load-system :pvs)
;; (asdf:operate :program-op :pvs)

;; (declaim (optimize (compilation-speed 0) (space 1) (safety 1) (speed 3) (cl:debug 0)))
;; (sb-ext:describe-compiler-policy)

(in-package :cl-user)

(defpackage :pvs-asd
  (:use :cl :cl-user))

(in-package :pvs-asd)

(asdf:defsystem "pvs"
  :description "PVS is a verification system; see http://pvs.csl.sri.com"
  :version "8.0"
  :author "Sam Owre, N. Shankar, and others"
  :maintainer "Sam Owre <owre@csl.sri.com>"
  :license "GPL"
  :entry-point "pvs::startup-pvs"
  ;;:defsystem-depends-on (#:asdf-shared-library)
  :depends-on (#:babel #:clack #:websocket-driver
		       #:hunchentoot #:anaphora #:lparallel #:cl-json #:cffi)
  :serial t
  :perform (asdf:load-op :after (op cmp)
			 (funcall (intern (string :finally-do) :pvs)))
  ;; Builds program, imports image-op; invoked with (asdf:oos :program-op :pvs)
  :perform (asdf:program-op :around (op cmp)
			    (funcall (intern (string :make-pvs-program) :pvs)))
  ;; Need to understand why the following doesn't work.
  ;; :output-files (list "pvs-allegro.dxl" "files.bu" "pvs-allegro" "pvs-allegro.dxl"
  ;; 		      "pvs-allegro.lic")
  ;; Should add .so / .dylib files if we make this work.
  :components
  ((:file "packages")
   (:module :ess ;; Needed for pvs-parser
     :pathname "ess/"
     :components ((:file "dist-ess")
		  ;; Loads ess/init-load.lisp
		  ;;   Loads ess/sys/ergolisp/rel/ergo-system.lisp
		  ;;   Loads ess/sys/tools/rel/retry.lisp
		  ;;   Loads ess/sys/tools/rel/box-system.lisp
		  ;;   Loads ess/sys/tools/rel/box.lisp
		  ;;   Loads ess/box-defs.lisp
		  (:file "sys/ergolisp/rel/ergolisp")
		  (:file "sys/ergolisp/rel/ergolisp-exports")
		  (:file "sys/ergolisp/rel/ergo-system")
		  (:file "sys/ergolisp/rel/ergo-types")
		  (:file "sys/ergolisp/rel/type-check")
		  (:file "sys/tools/rel/regression-test")
		  (:file "sys/tools/rel/clet")
		  (:file "sys/tools/rel/retry")
		  (:file "sys/tools/rel/box-system")
		  (:file "sys/tools/rel/box")
		  (:file "sys/tools/rel/box-lib")
		  (:file "sys/tools/rel/print-utils")
		  (:file "sys/ergolisp/rel/dlambda")
		  (:file "sys/ergolisp/rel/dlambda-lib")
		  (:file "term/language/rel/languages")
		  (:file "term/terms/rel/opers")
		  (:file "term/terms/rel/occur")
		  (:file "term/terms/rel/sorts")
		  (:file "term/trep/rel/attr-prims")
		  (:file "term/trep/rel/gterm")
		  (:file "term/terms/rel/terms")
		  (:file "term/terms/rel/termop")
		  (:file "lang/sb-term/rel/sbrt-lang-def")
		  (:file "lang/sb-term/rel/sbrt-sorting")
		  (:file "lang/sb-term/rel/rt-structs")
		  (:file "lang/sb-term/rel/rt-unp-structs")
		  (:file  "lang/sb-term/rel/rt-lex")
		  (:file "lang/sb-term/rel/rt-term")
		  (:file "lang/sb-term/rel/rt-parse-mac")
		  (:file "lang/sb-term/rel/rt-parse")
		  (:file "lang/sb-term/rel/rt-unparse")
		  (:file "term/attr/rel/attr-lang-lib") ;; Loads
		  (:file "term/attr/rel/attr-sort") ;; Loads
		  (:file "term/attr/rel/attr-lang") ;; Loads
		  (:file "term/attr/rel/attr-global") ;; Loads
		  (:file "lang/sb-term/rel/rt-unp-attr") ;; Loads
		  (:file "lang/sb-term/rel/rt-format") ;; Loads
		  (:file "lang/sb-term/rel/rt-unp-tex") ;; Loads
		  (:file "lang/sb-term/rel/rt-unp-top") ;; Loads
		  (:file "term/attr/rel/attr-lib") ;; Loads
		  (:file "term/attr/rel/attr-gsort") ;; Loads
		  (:file "term/attr/rel/attr-occ") ;; Loads
		  (:file "lang/ab-term/rel/af-runtime")
		  (:file "sys/constr/rel/constr")
		  (:file "sys/constr/rel/constr-term-rep")
		  (:file "sys/constr/rel/constr-sorts")
		  (:file "sys/constr/rel/constr-lexer")
		  (:file "sys/constr/rel/constr-parser")
		  (:file "sys/constr/rel/defsconstr")
		  (:file "sys/ergolisp/rel/tdefun")
		  (:file "lang/sb-term/rel/access")
		  (:file "lang/sb-term/rel/access-par")
		  (:file "lang/sb-term/rel/aux-funs")
		  (:file "lang/sb-term/rel/sb-lexer")
		  (:file "lang/sb-term/rel/sb-parser")
		  (:file "lang/sb-term/rel/sb-sorts")
		  (:file "lang/sb-term/rel/sb-unparser")
		  (:file "lang/sb-term/rel/sb-unparsing-aux")
		  (:file "lang/sb-term/rel/pre-process")
		  (:file "lang/sb-term/rel/inter-phase")
		  (:file "lang/sb-term/rel/sort-gen")
		  (:file "lang/sb-term/rel/lexer-gen")
		  (:file "lang/sb-term/rel/flatten")
		  (:file "lang/sb-term/rel/look-ahead")
		  (:file "lang/sb-term/rel/compare")
		  (:file "lang/sb-term/rel/collapse")
		  (:file "lang/sb-term/rel/phase-three")
		  (:file "lang/sb-term/rel/top-parse")
		  (:file "lang/sb-term/rel/unp-code-revise")
		  (:file "lang/sb-term/rel/unparse-gen")
		  (:file "lang/sb-term/rel/top"))
     ;; :perform (asdf:load-op
     ;; 	       :after (op c)
     ;; 	       ;; (generate-ess 'ergolisp 'sb)
     ;; 	       (when t ;; (parser-needs-rebuilding?)
     ;; 		 (funcall (intern (string :generate-ess) :cl-user)
     ;; 			  (intern (string :ergolisp) :cl-user)
     ;; 			  (intern (string :sb) :cl-user))))
     )
   (:file "pvs-config" :depends-on ("packages"))
   (:module :pvs-parser
     ;; Makes the pvs-parser if needed; it's loaded in the :language module
     :pathname "src/"
     :depends-on ("ess")
     :components ((:file "ergo-gen-fixes")
		  ;;(:file "pvs-lang-def")
		  )
     :perform (asdf:load-op
	       :after (op c)
	       ;;(funcall (intern (string :make-pvs-parser) :pvs))
	       ;; (sb:sb-make :language "pvs" :working-dir "./src/" :unparser? nil)
	       (when t ;(funcall (intern (string :parser-needs-rebuilding?) :pvs))
		 (funcall (intern (string :sb-make) :sb)
			  :language "pvs"
			  :working-dir "./src/"
			  :unparser? nil)
		 ))
     ;; :output-files (asdf:load-op (op c)
     ;; 			    (list "pvs-lexer.lisp"
     ;; 				  "pvs-parser.lisp"
     ;; 				  "pvs-sorts.lisp"))
     )
   (:module :ground-prover
     :pathname "src/ground-prover/"
     :components ((:file "prmacros")
		  (:file "interface" :depends-on ("prmacros"))
		  (:file "prglobals" :depends-on ("prmacros"))
		  (:file "process" :depends-on ("prmacros"))
		  (:file "arrays" :depends-on ("prmacros"))
		  (:file "tuples" :depends-on ("prmacros"))
		  (:file "arith" :depends-on ("prmacros"))
		  (:file "q" :depends-on ("prmacros"))))
   (:module :classes
     :pathname "src/"
     :components ((:file "linked-hash-table")
		  (:file "store-object")
		  (:file "defcl")
		  (:file "classes-expr" :depends-on ("defcl"))
		  (:file "pvs-threads" :depends-on ("defcl"))
		  (:file "classes-decl" :depends-on ("defcl"))
		  (:file "prover/estructures" :depends-on ("defcl"))
		  (:file "groundeval/pvs2c-utils" :depends-on ("defcl"))
		  (:file "groundeval/pvs2ir-classes" :depends-on ("defcl"))))
   (:module :macros
     :pathname "src/"
     :components ((:file "macros")
		  (:file "prover/checker-macros")
		  (:file "prover/decision-procedure-interface")
		  (:file "globals")
		  (:file "groundeval/eval-macros")
		  (:file "optimize")
		  (:file "makes" :depends-on ("macros"))))
   (:module :utils
     :pathname "src/utils"
     :perform (asdf:load-op :before (op c)
			    (format t "~%Making and loading file_utils.so")
			    (funcall (intern (string :make-in-platform) :pvs)
				     (asdf:component-pathname c) "file_utils" "b64"))
     :components ((:file "hashfn")
		  (:file "file-utils")))
   (:module :pvs2c
     :pathname "src/groundeval"
     :components((:file "pvs2c-types")
		 (:file "pvs2c-code")
		 (:file "pvs2c-analysis")
		 (:file "pvs2ir")))
   (:module :pvs-methods
     :pathname "src/"
     :components ((:file "pvs-methods"
			 :perform (asdf:compile-op
				   :before (op c)
				   (let ((*package* (find-package :pvs)))
				     (funcall (intern (string :write-deferred-methods-to-file) :pvs)
					      t))))))
   (:module :language
     :pathname "src/"
     :components ((:file "ergo-runtime-fixes")
		  ;;(:file "ergo-tex-fixes")
		  (:file "pvs-lang-def")
		  (:file "pvs-lexer")
		  (:file "pvs-parser" :depends-on ("ergo-runtime-fixes"))
		  ;;(:file "pvs-unparser")
		  (:file "pvs-sorts")
		  (:file "pvs-parse-fixes"))
     :depends-on (classes))
   (:module :basic
     :pathname "src/"
     :components (;;(:file "makes")
		  (:file "parse")
		  #-(or allegro sbcl)
		  (:file "md5")
		  (:file "context")
		  (:file "workspaces")
		  (:file "pp")
		  (:file "print-object")
		  (:file "equalities")
		  (:file "utils")
		  (:file "gensubst")
		  (:file "substit"))
     :depends-on (language macros))
   (:module :interface
     :pathname "src/interface"
     :components ((:file "ilisp-pkg")
		  (:file "pvs-emacs")
		  (:file "emacs-calls")
		  (:file "cl-ilisp")
		  #+allegro
		  (:file "allegro")
		  #+cmu
		  (:file "cmulisp")
		  ))
   (:module typechecker
     :pathname "src/"
     :components ((:file "freeparams")
		  (:file "subst-mod-params")
		  (:file "tc-unify")
		  (:file "resolve")
		  (:file "tcc-gen")
		  (:file "set-type")
		  (:file "check-for-tccs")
		  (:file "tcexprs")
		  (:file "tcdecls")
		  (:file "conversions")
		  (:file "judgements")
		  (:file "xref")
		  (:file "occurs-in")
		  (:file "datatype")
		  (:file "typecheck")
		  (:file "tclib"))
     :depends-on (basic))
   (:module user-interface
     :pathname "src/"
     :components (;;(:file "restore-types")
		  (:file "compare")
		  (:file "untypecheck")
		  (:file "copy-lex")
		  (:file "save-theories")
		  (:file "pvs")
		  (:file "status-cmds")
		  (:file "add-decl")
		  (:file "list-decls")
		  (:file "pp-tex")
		  (:file "tex-support")
		  (:file "pp-html")
		  (:file "pp-xml")
		  (:file "pp-json-ml")
		  (:file "raw-api"))
     :depends-on (basic))
   (:module proof-checker
     :pathname "src/prover/"
     :components ((:file "translate-to-prove")
		  (:file "eproofcheck")
		  (:file "proofrules")
		  (:file "equantifiers")
		  (:file "freevars")
		  (:file "rewrites")
		  (:file "assert")
		  (:file "beta-reduce")
		  (:file "replace")
		  (:file "expand")
		  (:file "match")
		  (:file "rules")
		  (:file "strategies")
		  (:file "wish")
		  (:file "translate-to-yices")
		  (:file "translate-to-yices2"))
     :depends-on (typechecker))
   (:module bddlib
     :pathname "src/BDD"
     :perform (asdf:load-op :before (op c)
		       (funcall (intern (string :make-in-platform) :pvs)
				(asdf:component-pathname c) "mu")))
   (:module bdd
     :pathname "src/BDD"
     :components ((:file "bdd")
		  (:file "bdd-cffi"))
     :depends-on (:proof-checker :bddlib))
   (:module mu-simplifier
     :pathname "src/BDD"
     :components ((:file "mu")
		  (:file "mu-cffi"))
     :depends-on (:bdd))
   (:module groundeval
     :pathname "src/groundeval"
     :components ((:file "eval-utils")
		  (:file "ground-expr")
		  (:file "static-update")
		  (:file "pvseval-update")
		  (:file "cl2pvs")
		  (:file "generate-lisp-for-theory")
		  (:file "random-test")
		  (:file "pvs2clean")
		  (:file "c-primitive-attachments"))
     :depends-on (basic))
   (:module Field
     :pathname "src/Field"
     :components ((:file "decimals")
		  (:file "extrategies")
		  (:file "field")))
   (:module PVSio
     :pathname "src/PVSio"
     :components ((:file "pvs-lib")
		  (:file "defattach")
		  (:file "pvsio")
		  (:file "prelude-attachments")
		  ))
   (:module Manip
     :pathname "src/Manip"
     :components ((:file "pregexp")
		  (:file "manip-utilities")
		  (:file "extended-expr")
		  (:file "syntax-matching")
		  (:file "manip-strategies")
					;(:file "debug-utils")
		  ))
   (:module ProofLite
     :pathname "src/ProofLite"
     :components (			;(:file "pregexp")
		  (:file "prooflite")
		  (:file "proveit-init")))
   (:module ws1slib
     :pathname "src/WS1S"
     :perform (asdf:load-op :before (op c)
			    (funcall (intern (string :make-in-platform) :pvs)
				     (asdf:component-pathname c) "ws1s")))
   (:module ws1s
     :pathname "src/WS1S/lisp"
     :components (#+allegro
		  (:file "dfa-foreign")
		  #+sbcl
		  (:file "dfa-foreign-sbcl")
		  (:file "dfa")
		  (:file "pvs-utils")
		  (:file "symtab")
		  (:file "signature")
		  (:file "pvs2dfa")
		  (:file "ws1s-strategy")
		  (:file "presburger")))
   (:module abstraction
     :pathname "src/abstraction"
     :components ((:file "abstract")))
   (:module rahd
     :pathname "src/rahd"
     :components ((:file "rahd")
		  (:file "polyalg")
		  (:file "polyeval")
		  (:file "polyconv")
		  (:file "sturm")
		  (:file "sturmineq")
		  (:file "strings")
		  (:file "ineqfert")
		  (:file "canonizer")
		  (:file "ideals")
		  (:file "opencad")
		  (:file "cocoa")
		  (:file "cases")
		  (:file "realnull")
		  (:file "cauchyeval")
		  (:file "intgrldom")
		  (:file "demodlin")
		  (:file "demodnl")
		  (:file "plinsolver")
		  (:file "intsplit")
		  (:file "interval")
		  (:file "intvlcp")
		  (:file "gbrnull")
		  (:file "cnf")
		  (:file "division")
		  (:file "quicksat")
		  (:file "prover")
		  (:file "abbrevs")
		  (:file "regression")
		  (:file "debug")
		  (:file "prfanal")
		  (:file "rahd-pvs")))
   #+allegro
   (:module nlyices
     :pathname "src/nlyices"
     :components ((:file "polyrep-totdeglex")
		  (:file "decide3_2a")
		  (:file "vts")
		  (:file "nlsolver")
		  (:file "named-callbacks")
		  (:file "fcpo")
		  (:file "nlyices")))
   (:module rpc
     :pathname "src/interface"
     :components ((:file "pvs-json")
		  (:file "pvs-json-rpc")
		  (:file "pvs-json-methods")
		  (:file "pvs-websocket")
		  ;;#+allegro
		  ;;(:file "pvs-xml-rpc")
		  ))
   ))
