;; -*- Mode: lisp; Package: TOOLS -*-
;;;
;;; This file is the central clearinghouse for tools and
;;; applications that are available in the Ergo Support System.
;;;
;;; NOTE:  This file should contain only exported boxes!
;;;        New or obsolete boxes should go into internal-boxes.lisp.
;;;
;;; Sccs Id @(#)box-defs.lisp	1.46 11/16/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(in-package "TOOLS") (use-package :ergolisp)

(export '(*plain-readtable*))

(defparameter *plain-readtable* (copy-readtable nil))

(defbox *default-box*
  "Box to use if a file is unknown."
  (:maintainers :all)
  (:path "./")				; Always look in the current directory.
  (:files)				; No files predefined.
  )

#-gcl
(defbox lisp-loader
  "The Lisp loader.  This box does only loads compiled files to avoid
a conflict with the lisp-processor for files with .lisp extension."
  (:maintainers)
  (:needs)
  (:path)
  (:generates "tools::lisp-load" &key
	      (file #,(concatenate 'string "*" *lisp-compiled-suffix-string*)
		    :input)))

#+gcl
(defbox lisp-loader
  "The Lisp loader.  This box does only loads compiled files to avoid
a conflict with the lisp-processor for files with .lisp extension."
  (:maintainers)
  (:needs)
  (:path)
  (:generates "tools::lisp-load" &key
	      (file #.(concatenate 'string "*" *lisp-compiled-suffix-string*)
		    :input)))

#-gcl
(defbox lisp-source-loader
  (:generates "tools::lisp-load" &key
	      (file #,(concatenate 'string "*" *lisp-source-suffix-string*)
		    :input)))

#+gcl
(defbox lisp-source-loader
  (:generates "tools::lisp-load" &key
	      (file #.(concatenate 'string "*" *lisp-source-suffix-string*)
		    :input)))

#-gcl
(defbox ccom
  "The C compiler."
  (:maintainers "tsf")
  (:generates c-make &key
	      (source-file "*.c" :input)
	      (object-file "*.o" :output)
	      ;; Problem is, if there is a file foo.c, then this
	      ;; insists that there must be a header file named foo.h.
	      #|(header-file "*.h" :input)|#))


;;; The function lisp-load and global variables are now in box-system.lisp.

#-gcl
(defbox lisp-compiler
  "The Lisp compiler."
  (:needs)
  (:path)
  (:generates "tools::lisp-compile" &key
	      (source-file #,(concatenate 'string "*" *lisp-source-suffix-string*) :input)
	      (compiled-file #,(concatenate 'string "*" *lisp-compiled-suffix-string*)
			     :output)))

#+gcl
(defbox lisp-compiler
  "The Lisp compiler."
  (:needs)
  (:path)
  (:generates "tools::lisp-compile" &key
	      (source-file #.(concatenate 'string "*" *lisp-source-suffix-string*) :input)
	      (compiled-file #.(concatenate 'string "*" *lisp-compiled-suffix-string*)
			     :output)))

#-gcl
(defbox cload
  "The foreign function loader."
  (:maintainers "tsf")
  (:generates c-load &key
	      (object-file "*.o" :input)
	      (libraries nil :lisp)))

(defbox install-utils
  (:maintainers)
  (:path "/homes/EHDM/systems/ess/bin/")
  (:readme "relocate"))

(defbox initload
  "Files that are loaded initially, but should not be loaded again
afterwards.  Thus this box SHOULD NEVER BE LOADED."
  (:maintainers)
  (:path "/homes/EHDM/systems/ess/")
  (:readme "README" "INSTALL" "LICENSE")
  (:files "dist-ess.lisp"
	  "init-load.lisp"
	  "box-defs.lisp"))

(defbox ess-doc
  "General documentation for the Ergo Support System."
  (:maintainers)
  (:path "/homes/EHDM/systems/ess/doc/")
  (:readme "README")
  (:files "elpbib.ps" "penn-trs.txt"))

(defbox gnu-support
  "Support files for gnu-emacs that help to interact with the ESS."
  (:maintainers)
  (:path "/homes/EHDM/systems/ess/gnu/")
  (:readme "README")
  (:files "emacs.el"
	  "funkeys.el"
	  "shell-adds.el"
	  "sticky-filter.el"))

(defbox ergolisp
  "Support for making general-purpose Lisp extensions for the ERGO
Lisp programming environment." 
  (:maintainers conal fp)
  (:path "/homes/EHDM/systems/ess/sys/ergolisp/rel/")
  (:readme "/homes/EHDM/systems/ess/sys/ergolisp/README")
  (:files
   "ergolisp-exports"
   "ergolisp"
   ))

(defbox ergomisc
  "Miscellaneous stuff that becomes part of ERGOLISP"
  (:maintainers conal fp)
  (:needs ergolisp)
  (:path "/homes/EHDM/systems/ess/sys/ergolisp/rel/")
  (:readme "/homes/EHDM/systems/ess/sys/ergolisp/doc/type-check.doc")
  (:files
   "type-check"
   "ergo-types"
   "ergo-system"
   ))

(defbox tools
  "Basic Lisp tools."
  (:maintainers fp tsf conal)
  (:needs ergomisc)
  (:path "/homes/EHDM/systems/ess/sys/tools/rel/")
  (:files
   "print-utils"		; Used by the SB.
   "box-lib"
   ;; Moved up to the initload box.
   ;;("box-defs.lisp" :compile "box" :source t)
   ("box" :compile "retry")
   "box-system"
   "retry"				; ==> ergomisc
   "clet"				; ==> ergomisc
   "regression-test"
   )
  (:load-hook (use-package "TOOLS")))

(defbox internal-boxes
  "Box with one file: the boxes that are not exported."
  (:maintainers :all)
  (:path "/homes/EHDM/systems/ess/sys/tools/rel/")
  (:files ("internal-box-defs.lisp" :compile "box" :source t)))

(defbox dlambda
  "Destructuring support"
  (:maintainers conal)
  (:needs ergolisp)
  (:readme "/homes/EHDM/systems/ess/sys/ergolisp/doc/dlambda.doc")
  (:path "/homes/EHDM/systems/ess/sys/ergolisp/rel/")
  (:files
   "dlambda-lib"
   "dlambda"
   ))

(defbox summarize
  "Summarizer of files and boxes"
  (:maintainers fp)
  (:needs ergolisp dlambda)
  (:path "/homes/EHDM/systems/ess/sys/tools/rel/")
  (:files
   ("summarize-actions" :compile "summarize")
   "summarize"
   ))

(defbox profilers
  "Profiling tools."
  (:maintainers fp tsf conal)
  (:path "/homes/EHDM/systems/ess/sys/tools/rel/")
  (:files  #+lucid "monitor"   ; Lucid profiler.  See doc.  No source avail.
	   ))


(defbox languages
  "Language information"
  (:maintainers "srd" "fp")
  (:path "/homes/EHDM/systems/ess/term/language/rel/")
  (:readme "languages-doc.txt")
  (:files "languages")
  )

(defbox operators
  "Operators for terms"
  (:maintainers "srd")
  (:needs dlambda)			; declare-constructor
  (:path "/homes/EHDM/systems/ess/term/terms/rel/")
  (:readme "oper-doc.txt")
  (:files "opers")
  )

(defbox occur
  "Occurrences for terms"
  (:maintainers "srd")
  (:path "/homes/EHDM/systems/ess/term/terms/rel/")
  (:readme "occ-doc.txt")
  (:files "occur")
  )

(defbox sorts
  "Sort ADT"
  (:maintainers "srd")
  (:needs operators)
  (:path "/homes/EHDM/systems/ess/term/terms/rel/")
  (:readme "sort-doc.txt")
  (:files "sorts")
  )

(defbox gterm
  "Generic term primitives, supporting multiple representations."
  (:maintainers conal tsf srd)
  (:needs operators sorts dlambda)	; dlambda for declare-constructor
  (:path "/homes/EHDM/systems/ess/term/trep/rel/")
  (:readme "/homes/EHDM/systems/ess/term/trep/doc/gterm-doc.txt")
  (:files
   ;; "attributable-term"
   "gterm"				; mk-term, etc.  Rep installation.
   "attr-prims"				; Attribute caching.
   ))

(defbox terms
  "Generic Term ADT"
  (:maintainers "srd")
  (:needs operators occur dlambda gterm) ; dlambda for declare-constructor
  (:path "/homes/EHDM/systems/ess/term/terms/rel/")
  (:readme "term-doc.txt"
	   "termop-doc.txt")
  (:files
   ("termop" :compile "terms")
   ("terms"))
  )

(defbox attr
  "Attribute ADT compatible with  SB and term package."
  (:needs terms languages occur operators sorts (:boot sb-support)); sb-support
  (:maintainers "fp" "rln" "srd")
  (:path "/homes/EHDM/systems/ess/term/attr/rel/")
  (:files ("attr-occ" :compile "attr-global")
	  ("attr-gsort" :compile "attr-global")
	  ("attr-lib" :compile "attr-global")
	  ("attr-global" :compile "attr-lang")
	  ("attr-lang" :compile "attr-sort")
	  "attr-sort"
	  "attr-lang-lib"))


(defbox sb-support
  "The run-time system for the files generated by the sb."
  (:maintainers "srd" "tsf")
  ;; The dependency on attr is circular.  :boot will load the sources
  ;; in the box attr, then generate sb-support (if necessary).
  (:needs ergolisp ergomisc
	  operators occur terms sorts languages attr) ; (:boot attr)
  (:readme "/homes/EHDM/systems/ess/lang/sb-term/README")
  (:path "/homes/EHDM/systems/ess/lang/sb-term/rel/")
  (:generates)
  (:files
   ("rt-unp-top"   :load "rt-format" :load "rt-unparse")
   ("rt-unp-tex")
   ("rt-format"    :load "rt-term" :compile "rt-unp-structs"
		   :load "rt-unp-attr")
   ("rt-unp-attr"  :load "attr-global"
		   :load "attr-lang"
		   :load "attr-sort"
		   :load "attr-lang-lib"
		   :boot t		; because of missing eval-when's
		   )
   ("rt-unparse"   :load "rt-term" :compile "rt-unp-structs")
   ("rt-parse"     :load "rt-term" :compile "rt-structs")
   ("rt-parse-mac" :load "rt-term")
   ("rt-term"      :load "rt-lex")
   ("rt-lex"       :compile "rt-structs" :compile "rt-unp-structs")
   ("rt-unp-structs")
   ("rt-structs")  
   ("sbrt-sorting")
   ("sbrt-lang-def")
   )
  )

(defbox sb
  "The syntax-box parser, unparser, etc. generator."  
  (:maintainers "srd" "tsf")
  (:needs tools operators occur terms sorts languages sb-support attr)
  (:readme "/homes/EHDM/systems/ess/lang/sb-term/README")
  (:path "/homes/EHDM/systems/ess/lang/sb-term/rel/")
  (:generates "sb:sb-make" &key
	
	      (grammar-file "*-gr.txt" :input)
	      (lexer-file "*-lexer.lisp" :output)
	      (parser-file "*-parser.lisp" :output)
	      (unparser-file "*-unparser.lisp" :output)
	      (info-file "*-info.lisp" :output)
	      (sorts-file "*-sorts.lisp" :output)

	      (language nil :lisp)
	      (lang-struct nil :lisp)
	      (conc-name "" :lisp)
	      (working-dir nil :relevant-directory)
	      ;; (lang-file "*-lang-def.lisp" :input) ; obsolete.

	      (abs-syn-package "" :lisp)
	      (code-package "" :lisp)
	      (use-packages () :lisp)
	      (sub-languages () :lisp)

	      (unparse-nts t :lisp)
	      (parse-routine-name nil :lisp)
	      (unparse-routine-name nil :lisp)
	      (win-unparse-routine-name nil :lisp)

	      (info? t :lisp)
	      (lexer? t :lisp)
	      (parser? t :lisp)
	      (unparser? t :lisp)
	      (sorts? t :lisp)

	      (suppress-sort-errors nil :lisp)
	      (no-sort-phase nil :lisp)
	      (processing-hook nil :lisp)

	      )

  (:files
   ("top"		:load "unparse-gen" :load "top-parse"
			:load "inter-phase" :load "sb-parser"
			:load "sort-gen"    :compile "access"
			:compile "aux-funs")

   ("unparse-gen"       :compile "access" :compile "aux-funs"
			:load "sort-gen"  :load "top-parse")

   ("unp-code-revise"   :compile "access" :compile "aux-funs")

   ("top-parse"	        :compile "access" :compile "access-par"
			:compile "aux-funs"
			:load "collapse" :load "phase-three"
			:load "look-ahead" :load "flatten" :load "compare")

   ("phase-three"       :compile "access" :compile "access-par"
			:compile "aux-funs" :load "collapse"
			:load "sort-gen")
   ("collapse"	        :compile "access" :compile "access-par"
			:compile "aux-funs" :load "look-ahead")
   ("compare"	        :compile "access" :compile "access-par"
			:compile "aux-funs" :load "look-ahead")
   ("look-ahead"	:compile "access" :compile "access-par"
			:compile "aux-funs" :load "flatten")
   ("flatten"	        :compile "access" :compile "access-par"
			:compile "aux-funs") 

   ("lexer-gen"	     	:compile "access" :compile "aux-funs")

   ("sort-gen"		:compile "access")

   ("inter-phase"    	:compile "access" :compile "aux-funs")

   ("pre-process" 	:compile "access")
   ("sb-unparsing-aux"  :compile "sb-unparser") ; reloaded after sb-unparser


   ("sb-unparser"	:compile "rt-unparse")
   ("sb-sorts")
   ("sb-parser"  	:compile "rt-parse-mac") 
   ("sb-lexer"          :compile "access"
			:compile "rt-term")
   ("sb-gr.txt"         :language "meta-grammar" :conc-name "sb"
                        :abs-syn-package "SYNTAX-BOX" :code-package "SYNTAX-BOX"
			:suppress-sort-errors t)

   ("aux-funs"		:compile "access" :compile "access-par")
   ("access-par"     	:compile "access")
   ("access")
   ;; ("sb-lang-def")
   )
  )



(defbox sb-doc
  "Documentation for the SB (TeX and PostScript)."
  (:maintainers srd)			; (not really)
  (:path "/homes/EHDM/systems/ess/lang/sb-term/doc/")
  (:files
   "sb-manual.PS"			; postscript output.
;   "sb-manual.tex"
;   "sb-cover.tex"
;   "sb-intro.tex"
;   "sb-syntax.tex"
;   "sb-semantics.tex"
;   "sb-grammar.tex"
;   "example-gr.tex"
;   "sb-sorting.tex"
;   "sb-parsing.tex"
;   "sb-unparsing.tex"
;   "sb-restrictions.tex"
;   "sb-formatting.tex"
;   "sb-system.tex"
;   "sb-errors.tex"
;   "appendices.tex"
;   "sb-ack.tex"
;   "sb-bib.tex"
;   "sb-gr.tex"
;   "sb-gr-keywords.tex"
;   "doc-macros.tex"
;   "latex-1.tex"
;   "prog-mode.tex"
;   "/homes/EHDM/systems/ess/lang/sb-term/rel/sb-tabbing.tex"
;   "/homes/EHDM/systems/ess/lang/sb-term/rel/sb-prog-mode.tex"
;   "tex-example.tex"
;   "makefile"
   )
  )



;;; SB examples

(defbox pascal
  (:maintainers "srd")
  (:path "/homes/EHDM/systems/ess/lang/sb-term/examples/pascal/")
  (:needs sb-support)
  (:files "pascal-unparser"
	  "pascal-parser"
	  "pascal-lexer"
	  "pascal-sorts"
	  "ex1.pas"			; example text
	  ("pascal-gr.txt"         
	   :language "pascal" :conc-name "pascal"
	   :abs-syn-package "PASCAL" :code-package "PASCAL"
	   :suppress-sort-errors t)
	  ))


(defbox formtest
  "Test cases for the formatting language."
  (:maintainers "srd")
  (:path "/homes/EHDM/systems/ess/lang/sb-term/examples/formtest/")
  (:needs sb-support)
  (:files "form-unparser"
	  "form-parser"
	  "form-lexer"
	  "form-sorts"
	  "ex1.txt"			; example text
	  "ex2.txt"
	  ("form-gr.txt"         
	   :language "formtest" :conc-name "form"
	   :abs-syn-package "FORM" :code-package "FORM"
	   :suppress-sort-errors t)
	  ))






(defbox ab
  "Analysis Facility generator."
  (:maintainers rln)
  (:needs ab-support sb-support)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/rel/")
  (:readme "release-doc.txt")
  (:generates "ab:ab-make" &key
	      (grammar "*-agr.txt" :input)
	      (analyzer "*-analyzer.lisp" :output)
	      (package nil :lisp)	; package for resulting files.
	      (use-packages nil :lisp)	; packages to be used by package.
	      )
  (:files ("af-gr.txt" :language "agg"
	   :conc-name "af"
	   :code-package "ANALYSIS-FACILITY"
	   :abs-syn-package "ANALYSIS-FACILITY"
	   :suppress-sort-errors t)
	  ("af-agr.txt" :package "ANALYSIS-FACILITY"
			:analyzer "af-analyzer.lisp")
	  "af-top"
	  "af-analyzer"
	  "code-emitters"
	  "af-dependency"
	  "af-aux" "af-structs"
	  "af-sorts" "af-parser" "af-lexer"
	  )
  )

(defbox ab-support
  "Analysis Facility runtime system."
  (:maintainers rln)
  (:needs terms sorts attr)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/rel/")
  (:files "af-runtime"))

;;; Examples for the Analysis Facility

(defbox ab-doc
  "Documentation for the Analysis Facility"
  (:maintainers rln)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/doc/")
  (:readme "README")
  (:files "abox-doc.ps"))

(defbox calc
  "Simple calculator"
  (:maintainers rln)
  (:needs sb-support ab-support)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/examples/calc/")
  (:readme "/homes/EHDM/systems/ess/lang/ab-term/examples/README")
  (:files
   "calc-test" "calc-top"
   ("calc-agr.txt" :package "CALC" :analyzer "calc-analyzer")
   "calc-analyzer" "calc-aux"
   ("calc-gr.txt" :language "CALC")
   "calc-parser" "calc-lexer"
   "calc-sorts"))

(defbox tl
  "Trivial language with statements and expressions."
  (:maintainers rln)
  (:needs sb-support)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/examples/tl/")
  (:files
   ("tl-gr.txt" :language "TL")
   "tl-unparser" "tl-parser" "tl-lexer"
   "tl-sorts"))
   
(defbox dv
  "Dead variable analysis"
  (:maintainers rln)
  (:needs tl ab-support)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/examples/dv/")
  (:files
   "dv-test"
   ("dv-agr.txt" :package "TL" :analyzer "dv-analyzer")
   "dv-analyzer" "dv-aux"))

(defbox sf
  "Safety conditions"
  (:maintainers rln)
  (:needs sb-support ab-support)
  (:path "/homes/EHDM/systems/ess/lang/ab-term/examples/sf/")
  (:files
   "sf-test"
   ("sf-agr.txt" :package "SF" :analyzer "sf-analyzer")
   "sf-analyzer" "sf-aux"
   ("sf-gr.txt" :language "SF")
   "sf-unparser" "sf-parser" "sf-lexer"
   "sf-sorts"))

(defbox constr
  "Support for defining structural data-types.  Both a s-expression and a
ML-like concrete syntax.  The Lisp type syntax should be factored out someday."
  (:maintainers conal)
  (:needs dlambda gterm sb-support)
  (:path "/homes/EHDM/systems/ess/sys/constr/rel/")
  ;; We never (and can't) unparse.
  (:local-opts :unparser? nil :suppress-sort-errors t)
  (:readme "/homes/EHDM/systems/ess/sys/constr/doc/constructure.doc")
  (:files
   "defsconstr"
   ("constr-gr.txt" :language "constr"
    :abs-syn-package "CONSTR-TERM-REP"
    :code-package "CONSTRG"
    :unparser? nil)
   "constr-parser"
   "constr-lexer"
   "constr-sorts"
   "constr-term-rep"
   "constr"
   ))

(defbox tdefun
  "Nice variant of defun with destructuring and concrete syntax type
specification.  Also similar variants of deftype, defvar, etc."
  (:maintainers conal fp)
  ;; Uses only type concrete syntax from constr box
  (:needs constr dlambda)
  (:path "/homes/EHDM/systems/ess/sys/ergolisp/rel/")
  (:files
   "tdefun"
   ))

;;;; eLP boxes.
;;; First LP, the logic programming interpreter.

(defbox lp-interface
  "The interface of the LP interpreter to the term language."
  (:maintainers conal fp dmiller srd)
  (:needs constr)
  (:path "/homes/EHDM/systems/ess/elp/lp/rel/")
  (:files
   "lp-form"
   "lp-id"
   "lp-interface"
   ))

(defbox smodule
  "Syntactic form of lp modules."
  (:maintainers conal fp dmiller)
  (:needs sb-support constr)
  (:path "/homes/EHDM/systems/ess/elp/lp/rel/")
  (:files
   "smodule"
   ))

(defbox lp
  "The LP interpreter."
  (:maintainers conal fp dmiller srd)
  (:needs constr lp-interface ident smodule)
  (:path "/homes/EHDM/systems/ess/elp/lp/rel/")
  (:files
   "lp-top"
   "lp-evaluate"
   "lp-special"
   "lp-solve"
   "lp-from-smodule"
   "lp-program"
   "lp-module"
   ))

(defbox ident
  "Association of unique identifiers with objects, e.g., uvars, evars, and
tevars in lp terms."
  (:maintainers conal fp dmiller)
  (:needs)
  (:path "/homes/EHDM/systems/ess/elp/lp/rel/")
  (:files
   "ident"
   ))

(defbox elp-loader
  "The ELP module loader."
  (:maintainers conal fp dmiller)
  (:needs lp)
  ;; box-module-load is defined in lp-interface.lisp, since it uses tlang-call.
  ;; This box should not be included in a crate definition, since
  ;; it doesn't have path (or file) associated with it.
  (:generates "elp::box-module-load" &key
	      (file "*.mod" :input)))

;;;; Now the term language of lterms.
;;; The grammar lives in a separate directory for easy copying.

(defbox slterm-lang
  "SB support for slterm.  NOTE THE PATH."
  (:maintainers conal fp dmiller srd)
  (:needs sb-support)
  (:path "/homes/EHDM/systems/ess/elp/grammar/rel/")
  (:files
   ("slterm-gr.txt" :language "slterm"
    :abs-syn-package "SLTERM"
    :code-package "SLTERMG"
    :suppress-sort-errors t)
   "slterm-unparser"
   "slterm-parser"
   "slterm-lexer"
   "slterm-sorts.lisp"
   ))

(defbox slterm
  "The ADTs for constructing and using the `syntactic form' of lterms."
  (:maintainers conal fp dmiller srd)
  (:needs constr)
  (:path "/homes/EHDM/systems/ess/elp/lterm/rel/")
  (:files
   "slterm"
   ))

(defbox slterm-io
  "Concrete syntax support for slterms."
  (:maintainers conal fp dmiller srd)
  (:needs slterm-lang terms slterm lp-interface)
  (:path "/homes/EHDM/systems/ess/elp/lterm/rel/")
  (:files
   "slterm-system"			; Lisp impl. dependent.
   "slterm-io"
   "slterm-term-rep"
   ))

(defbox lterm
  "Lterms, i.e., the ADTs of simply typed lambda terms, their types, etc."
  (:maintainers conal fp dmiller srd)
  (:needs ident constr slterm lp-interface tdefun slterm-io)
  (:path "/homes/EHDM/systems/ess/elp/lterm/rel/")
  (:files
   "lterm-tlang"
   ("lterm-cterm" :compile "lterm-adts") ; For print functions.

   "lterm-prune"
   "lterm-gvar-gvar"
   "lterm-unify"
   "lterm-vtopt"

   "lterm-from-slterm"

   "lterm-exp"
   "lterm-form"
   "lterm-convert"

   "lterm-tunify"
   "lterm-to-slterm"
   "lterm-translator"

   ;; :boot because of missing eval-when's.  Fix sometime
   ;; ("lterm-adts" :boot t) ; hopefully fixed now.
   "lterm-adts"
   ))

(defbox elp-lterm
  "The lterm version of eLP.  This is the lterm side of the interface to LP."
  (:maintainers conal fp dmiller srd)
  (:needs lp lterm)
  (:path "/homes/EHDM/systems/ess/elp/lterm/rel/")
  (:files
   "lterm-special"))

(defbox elp-modules
  "The system modules used by eLP."
  (:maintainers conal fp dmiller srd)
  (:needs elp-lterm)
  (:path "/homes/EHDM/systems/ess/elp/lib/rel/")
  (:files
   "maps.mod"
   "lists.mod"
   "help.mod"
   "elp.mod"))

(defbox polylam
  "A box with an eLP example of the polymorphic Lambda calculus, including
type inference and interpretation."
  (:maintainers fp)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/polylam/")
  (:files
   "lameval.mod"
   "lamsig.mod"
   "lamsubst.mod"
   "polycompile.mod"
   "polydiag.mod"
   "polyinf.mod"
   "polysig.mod"))

(defbox meta88
  "A box with the eLP code by John Hannan from a paper at the workshop
on Metaprogramming in Logic Programming in June 1988.  This is a mini-ML
interpreter, compiler to CAM code, and CAM code evaluator."
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/meta88/")
  (:readme "README")
  (:files "mldecl.mod"
	  "mltype.mod" "mleval.mod"
	  "mlexam.mod" "mltest.mod" "mlindex.mod"
	  "camdecl.mod"
	  "cameval.mod" "camml.mod"
	  "camtest.mod" "camindex.mod"))

(defbox metaint
  "A box with the code for various eLP meta interpreters."
  (:maintainers fp)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/metaint/")
  (:readme "README" "meta_ex.rec")
  (:files "meta.mod"
	  "meta_ex.mod"
	  "metacut.mod"
	  "metacut_ex.mod"
	  "metaelf.mod"
	  "metaproof.mod"))

(defbox subst
  "A box with the eLP code by John Hannan which implements a variety
of substitution predicates."
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/subst/")
  (:readme "README")
  (:files "director.mod"
	  "examples.mod"
	  "hftrans.mod"
	  "sub0.mod"
	  "sub1.mod"
	  "sub2.mod"
	  "sub3.mod"
	  "sub4.mod"
	  "sub5.mod"
	  "suba.mod"
	  "subb.mod"
	  "subdb.mod"
	  "substaux.mod"))

(defbox church
  "A box with the eLP code for a little calculator on Church numerals."
  (:maintainers fp)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/church/")
  (:readme "README")
  (:files "church.mod"))

(defbox deduction
  "Examples of deductive systems and theorem provers in eLP from Amy Felty."
  (:maintainers felty fp)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/deduction/")
  (:readme "README"
	   ;; The following grammar files are listed here since the current
	   ;; version of eLP requires them to have the same name as
	   ;; already existing files (the standard grammar for eLP).
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-gr.txt"
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-info.lisp"
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-lexer.lisp"
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-parser.lisp"
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-sorts.lisp"
	   "/homes/EHDM/systems/ess/elp/examples/deduction/grammar/slterm-unparser.lisp")
  (:files 
   "convert.mod"
   "copy.mod"
   "dfs.mod"
   "dfstest.mod"
   "fol.mod"
   "formulas.mod"
   "goalred.mod"
   "goals.mod"
   "hol.mod"
   "idfs.mod"
   "idfstest.mod"
   "inter_tacs.mod"
   "lc_auto.mod"
   "lc_iter.mod"
   "lc_prove.mod"
   "lc_top.mod"
   "lcprfchk.mod"
   "lcprover.mod"
   "lf_fol.mod"
   "lf_ni.mod"
   "lfconv.mod"
   "lfnorm.mod"
   "lfsig.mod"
   "liprfchk.mod"
   "liprover.mod"
   ;; "lists.mod"
   "lni_top.mod"
   "lniprover.mod"
   "lprf.mod"
   "lprfex.mod"
   "mapcopy.mod"
   "maptac.mod"
   "ncprfchk.mod"
   "ncprover.mod"
   "nd.mod"
   "nd_top.mod"
   "ndc.mod"
   "ndcopy.mod"
   "ndgoal.mod"
   "ndnorm.mod"
   "ndnormalize.mod"
   "ndprint.mod"
   "ndredex.mod"
   "ndtac.mod"
   "ninormal.mod"
   "ninormal1.mod"
   "niprfchk.mod"
   "niprover.mod"
   "nprf.mod"
   "nprfex.mod"
   "st.mod"
   "sttest.mod"
   "tacticals.mod"))

(defbox misc-elp-examples
  "A box with miscellaneous eLP examples."
  (:maintainers conal fp dmiller)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/elp/examples/misc/")
  (:files
   "norm_dform.mod"
   "prims.mod"
   "top_utils.mod"))

(defbox misc-elp-files
  "A box with miscellaneous unloadable files for eLP."
  (:path "/homes/EHDM/systems/ess/elp/examples/")
  (:files "elp.init"))

(defbox misc-elp-doc
  "An unloadable box with miscellaneous documentation files for eLP."
  (:path "/homes/EHDM/systems/ess/elp/doc/")
  (:readme "WELCOME")
  (:files "help.list"
	  "document.list"
	  "initialization.doc"
	  "lisp-interface.doc"))

(defbox elp-topics-doc
  "An unloadable box with the documentation for eLP organized by topics."
  (:path "/homes/EHDM/systems/ess/elp/doc/topics/")
  (:readme "README")
  (:files "arithmetic.doc"
	  "help.doc"
	  "input.doc"
	  "interpreter.doc"
	  "lists.doc"
	  "logprim.doc"
	  "modules.doc"
	  "output.doc"
	  "parsing.doc"
	  "switches.doc"
	  "toplevel.doc"
	  "tracing.doc"
	  "warnings.doc"))



(defbox ebg
  "eLP code for EBG (explanation-based generalization)"
  (:maintainers srd fp)
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/")
  (:files
   "metaebg.mod"
   ))

(defbox ebg-doc
  "Documentation for the EBG system (under ELP) (PostScript)."
  (:maintainers srd fp)		
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/doc/")
  (:files
   "ebg.PS"			; postscript output.
   ))

(defbox suicide
  "suicide ebg example"
  (:maintainers srd fp)
  (:needs ebg)
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/suicide/")
  (:files
   "suicide.mod"
   ))

(defbox intgr
  "integration ebg example"
  (:maintainers srd fp)
  (:needs ebg)
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/intgr/")
  (:files
   "integrate.mod"
   ))

(defbox proofck
  "Proof checking ebg example"
  (:maintainers srd fp)
  (:needs ebg slterm-lang)
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/proofck/")
  (:files
   "nj_ebg.mod" 
   "nproofex.mod"
   "ndproofs.mod"
   "form.mod"
   "ebg_fol.mod"
   "grammar.lisp"
   ))

(defbox tactic
  "Tactical theorem proving ebg example"
  (:maintainers srd fp)
  ;; (:needs ebg)     ; uses an extended another version.
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/tactic/")
  (:files
   "tac_integrate.mod"
   "ebg_tacticals.mod"
   "metaebg_ext.mod"
   ))

(defbox tail_rec
  "Tail recursion ebg example"
  (:maintainers srd fp)
  (:needs ebg)
  (:path "/homes/EHDM/systems/ess/elp/examples/ebg/tail_rec/")
  (:files
   "tail_rec_ebg.mod"
   ))






;;; We steal parts of the pcl box instead of putting the entire pcl
;;; box in the :needs list because we don't need the whole thing.
(defbox adt-obj
  "ADT-OBJ (object system for DISPLAY)"
  (:maintainers tsf)
  (:needs dlambda)
  (:path "/homes/EHDM/systems/ess/sys/adt-obj/rel/")
  (:files
   "tracer"
   ("adt-obj" :compile ("pcl-walk" "pcl-macros"))
   "pcl-walk"		; This is really from the pcl box.
   ("pcl-macros" :compile "pcl-macros.lisp") ; so's this.
   ;; For repairing package problems, in lucid, perhaps in others.
   ("pcl-defsys" :source t)))

(defbox adt-obj-doc
  "Documentation for adt-obj"
  (:maintainers tsf)
  (:path "/homes/EHDM/systems/ess/sys/adt-obj/doc/")
  (:files
   "design.txt"
   "adt-obj.PS"))
 
(case (windowsystem)
  (:x11 (push :x11 *features*)
	(push :twm-bug-workaround *features*)))

(defbox low-display
  "New Object Oriented Interaction Facility (different object system)"
  (:maintainers tsf)
  (:readme "/homes/EHDM/systems/ess/if/display/INSTALL")
  (:needs xint sb-support adt-obj #+x11 clx)
  (:path
   ;; See $ess/if/display/INSTALL for instructions explaining how to
   ;; run DISPLAY under X10.  Those instructions refer to the 
   ;; path of the low-display box, which is the following:
   #-x11 "/homes/EHDM/systems/ess/if/display/rel-x10/"
   "/homes/EHDM/systems/ess/if/display/rel/"
   ;; end path of the low-display box.
   )
  (:files
   ;; The following three are not ported to X11 yet due to mysterious
   ;; clx color problems. 
   "newmenu"      
   "dispinput"    
   "titlebar" 
   "mouse"
   "pseudowin"
   "wire"
   "editor"
   "predicate"
   "sbdts"   
   "string"
   "dt-mixins"
   "cache"
   "highlight"
   "know"
   "parent"
   "hyster"
   "point"
   "term-win"
   "async"
   "stacking"
   "scroll"
   "subwin"
   "div"
   "movehere"
   "load-shed"
   "optimize"
   "dispwin2"
   "dispwin1"
   "writegcon"
   "xorgcon"
   "gcon"
   "default2"
   "default1"
   "event"))

(defbox color-display
  "New Object Oriented Interaction Facility (different object system)"
  (:maintainers tsf)
  (:needs low-display)
  (:path
   ;; See $ess/if/display/INSTALL for instructions explaining how to
   ;; run DISPLAY under X10.  Those instructions refer to the 
   ;; path of the color-display box, which is the following:
   #-x11 "/homes/EHDM/systems/ess/if/display/rel-x10/"
   "/homes/EHDM/systems/ess/if/display/rel/"
   ;; end path of the color-display box.
   )
  (:files
   "floatlist"
   "threedee"
   "palette"))

(defbox display
  "New Object Oriented Interaction Facility (different object system)"
  (:maintainers tsf)
  (:needs #+x11 low-display #-x11 color-display))  

(defbox display-doc
  "Documentation for display"
  (:maintainers tsf)
  (:path "/homes/EHDM/systems/ess/if/display/doc/")
  (:files
   "display.PS"
   ))

(defbox xint-x10 
  "Minimal X-C interface, ought to be quite robust."
  (:maintainers "tsf")
  (:path "/homes/EHDM/systems/ess/sys/xint/rel/")
  (:files
   ("dispopen.o" :libraries ("-lX" "-lc"))
   "xint"
   ("boring.o" :libraries ("-lX" "-lc"))
   ("event.o" :libraries ("-lX" "-lc"))))

(defbox xint
  "Minimal X-C interface, ought to be quite robust."
  (:maintainers "tsf")
  (:needs #-x11 xint-x10)
  (:path "/homes/EHDM/systems/ess/sys/xint/rel/")
  (:files
   "xblock"
   ("block.o" :libraries ("-lc"))))

#+x11
;;; To compile this, you'll have to load defsystem.lisp and give the command
;;; (compile-clx).  
(defbox clx
  (:path "/homes/EHDM/systems/ess/if/clx/rel/")
  (:load-hook
   (progn
     (let ((*default-pathname-defaults* (pathname "/homes/EHDM/systems/ess/if/clx/rel/")))
       (load "defsystem.lisp")
       (funcall (intern "LOAD-CLX" (find-package "USER")))))))

(defbox clx-files
  "References all files in clx box.
   Don't boxload or boxgen this box, boxload the clx box or use clx's
   compile-clx function instead."
  (:path "/homes/EHDM/systems/ess/if/clx/rel/")
  (:readme "README" "NEWCHANGES" "exclREADME" "exclMakefile" "Makefile")
  (:files 
   ("attributes.lisp" :lisp-loader nil :lisp-compiler nil)
   ("buffer.lisp" :lisp-loader nil :lisp-compiler nil)
   ("bufmac.lisp" :lisp-loader nil :lisp-compiler nil)
   ("clx.lisp" :lisp-loader nil :lisp-compiler nil)
   ("defsystem.lisp" :lisp-loader nil :lisp-compiler nil)
   ("depdefs.lisp" :lisp-loader nil :lisp-compiler nil)
   ("dependent.lisp" :lisp-loader nil :lisp-compiler nil)
   ("display.lisp" :lisp-loader nil :lisp-compiler nil)
   ("doc.lisp" :lisp-loader nil :lisp-compiler nil)
   ("excldefsys.lisp" :lisp-loader nil :lisp-compiler nil)
   ("excldep.lisp" :lisp-loader nil :lisp-compiler nil)
   ("fonts.lisp" :lisp-loader nil :lisp-compiler nil)
   ("gcontext.lisp" :lisp-loader nil :lisp-compiler nil)
   ("graphics.lisp" :lisp-loader nil :lisp-compiler nil)
   ("image.lisp" :lisp-loader nil :lisp-compiler nil)
   ("input.lisp" :lisp-loader nil :lisp-compiler nil)
   ("keysyms.lisp" :lisp-loader nil :lisp-compiler nil)
   ("macros.lisp" :lisp-loader nil :lisp-compiler nil)
   ("manager.lisp" :lisp-loader nil :lisp-compiler nil)
   ("requests.lisp" :lisp-loader nil :lisp-compiler nil)
   ("resource.lisp" :lisp-loader nil :lisp-compiler nil)
   ("tcp.lisp" :lisp-loader nil :lisp-compiler nil)
   ("tcpinit.lisp" :lisp-loader nil :lisp-compiler nil)
   ("text.lisp" :lisp-loader nil :lisp-compiler nil)
   ("translate.lisp" :lisp-loader nil :lisp-compiler nil)
   ("excldep.c" :ccom nil :cload nil)
   ("kcltcp.c" :ccom nil :cload nil)
   ("socket.c" :ccom nil :cload nil)
   ))

(defbox clx-test
  "References all test files for clx box."
  (:path "/homes/EHDM/systems/ess/if/clx/rel/test/")
  (:needs clx)
  (:files 
   ("trivial"
    #-x11 :lisp-loader #-x11 nil #-x11 :lisp-compiler #-x11 nil)
   ("chileshacks"
    #-x11 :lisp-loader #-x11 nil #-x11 :lisp-compiler #-x11 nil)))

(defbox elp-programming-aides
  "Helps for working with elp from lisp."
  (:maintainers tsf)
  (:needs elp-modules)
  (:path "/homes/EHDM/systems/ess/if/mellowcard/rel/")
  (:files "elp-hacks"))

(defbox mellowcard
  "Cheap hypertext."
  (:maintainers tsf)
  (:needs display lp)
  (:readme "/homes/EHDM/systems/ess/if/mellowcard/README.txt")
  (:path "/homes/EHDM/systems/ess/if/mellowcard/rel/")
  (:files
   "hyperhack"))

(defbox mellowlam
  "Cheap hypertext for lambda prolog self-documentation."
  (:maintainers tsf)
  (:needs display mellowcard elp-programming-aides)
  (:path "/homes/EHDM/systems/ess/if/mellowcard/rel/")
  (:files
   "mellowlam"))

(defbox mellowcard-modules
  (:maintainers tsf)
  (:needs mellowlam)
  (:path "/homes/EHDM/systems/ess/if/mellowcard/rel/")
  (:files
   "mellowdb.mod"
   "mellowdesc.mod"
   "mellowscreen.mod"
   "mellowlow.mod"))

;;;
;;; Now various crate definitions which should always be at the end
;;; of this file.
;;;

(defcrate basics
  (:boxes ergolisp ergomisc tools dlambda))

(defcrate internals
  (:boxes internal-boxes))

(defcrate term-support
  ;;Basic support for terms, including parser and unparsers generated by
  ;; the SB and attribute evaluators generated by the AB.
  (:boxes languages operators occur sorts gterm terms
	  ;; attr and sb-support are mutually dependent!
	  sb-support attr
	  ab-support))

(defcrate ergolisp-with-term-support
  ;; Fill out the ERGOLISP environment with extensions that depend on the SB.
  "Ergolisp"
  (:boxes constr tdefun))

(defcrate ess-no-load
  ;; Not to be loaded files which are part of the basic ESS.
  (:boxes initload gnu-support install-utils ess-doc))

(defcrate syntax-facility
  "The Syntax Facility"
  (:boxes sb))

(defcrate sb-examples
  (:boxes sb-doc pascal formtest))

(defcrate analysis-facility
  "The Analysis Facility"
  (:boxes ab))

(defcrate ab-examples
  (:boxes ab-doc calc sf tl dv))

(defcrate elp
  "eLP"
  (:boxes
	  ;; system boxes
	  lp-interface smodule ident lp
	  ;; elp-loader ; not included, since contains no files.
	  ;; lterm boxes
	  slterm slterm-lang slterm-io lterm elp-lterm elp-modules
    ))

;; the examples and documentation crate for eLP should not be loaded, merely
;; listed when making the tar tape.  See instructions in
;; /homes/EHDM/systems/ess/TAR-README

(defcrate elp-examples
  (:boxes polylam meta88 subst misc-elp-examples
	  church metaint deduction
	  misc-elp-files
	  misc-elp-doc elp-topics-doc))

(defcrate ebg-system
  (:boxes ebg))

(defcrate ebg-examples
  (:boxes ebg-doc suicide intgr proofck tactic tail_rec))


(defcrate display
  (:boxes #-x11 xint-x10 xint adt-obj adt-obj-doc low-display
	  #-x11 color-display display display-doc))

(defcrate display-doc
  (:boxes adt-obj-doc display-doc #+x11 xint-x10 #+x11 color-display))

(defcrate clx
  (:boxes #+x11 clx))

(defcrate clx-source
  (:boxes clx-files))

(defcrate clx-examples
  (:boxes clx-test))

(defcrate mellowcard
  (:boxes elp-programming-aides mellowcard mellowlam mellowcard-modules))
