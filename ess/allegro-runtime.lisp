#+allegro
(eval-when (eval load)
  (setq system:*load-search-list*
	(list #p"" #p(:type "lfasl") #p(:type "cl") #p(:type "lisp"))))
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/ergolisp")
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/ergolisp-exports")
;;[Regenerating and Loading box #>B"ERGOMISC".]
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/ergo-system")
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/ergo-types")
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/type-check")
;;[Regenerating and Loading box #>B"TOOLS".]
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/regression-test")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/clet")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/retry")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/box-system")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/box")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/box-lib")
(load "/project/pvs/pvs2.3/ess/sys/tools/rel/print-utils")
;;[Regenerating and Loading box #>B"DLAMBDA".]
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/dlambda")
(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/dlambda-lib")
;;[Regenerating and Loading box #>B"LANGUAGES".]
(load "/project/pvs/pvs2.3/ess/term/language/rel/languages")
;;[Regenerating and Loading box #>B"OPERATORS".]
(load "/project/pvs/pvs2.3/ess/term/terms/rel/opers")
;;[Regenerating and Loading box #>B"OCCUR".]
(load "/project/pvs/pvs2.3/ess/term/terms/rel/occur")
;;[Regenerating and Loading box #>B"SORTS".]
;(load "/project/pvs/pvs2.3/ess/term/terms/rel/sorts")
;;;[Regenerating and Loading box #>B"GTERM".]
;(load "/project/pvs/pvs2.3/ess/term/trep/rel/attr-prims")
;(load "/project/pvs/pvs2.3/ess/term/trep/rel/gterm")
;;;[Regenerating and Loading box #>B"TERMS".]
;(load "/project/pvs/pvs2.3/ess/term/terms/rel/terms")
;(load "/project/pvs/pvs2.3/ess/term/terms/rel/termop")
;;[Loading box #>B"SB-SUPPORT".]
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/sbrt-lang-def")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/sbrt-sorting")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-structs")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-structs")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-lex.lisp")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-lex")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-term")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-parse-mac")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-parse")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unparse")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-lang-lib")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-sort")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-lang")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-global")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-attr")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-format")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-tex")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-top")
;;;[Loading box #>B"ATTR".]
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-lib")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-gsort")
;(load "/project/pvs/pvs2.3/ess/term/attr/rel/attr-occ")
;;;[Regenerating and Loading box #>B"SB-SUPPORT".]
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/sbrt-lang-def")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/sbrt-sorting")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-structs")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-structs")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-lex")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-term")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-parse-mac")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-parse")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unparse")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-attr")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-format")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-tex")
;(load "/project/pvs/pvs2.3/ess/lang/sb-term/rel/rt-unp-top")
;;;[Regenerating and Loading box #>B"ATTR".]
;;;[Regenerating and Loading box #>B"AB-SUPPORT".]
;(load "/project/pvs/pvs2.3/ess/lang/ab-term/rel/af-runtime")
;;;[Regenerating and Loading box #>B"CONSTR".]
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/constr")
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/constr-term-rep")
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/constr-sorts")
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/constr-lexer")
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/constr-parser")
;(load "/project/pvs/pvs2.3/ess/sys/constr/rel/defsconstr")
;;;[Regenerating and Loading box #>B"TDEFUN".]
;(load "/project/pvs/pvs2.3/ess/sys/ergolisp/rel/tdefun")
;(excl:dumplisp :name "/project/pvs/pvs2.3/ess/bin/ess-allegro-sun4"
;	       :checkpoint t)
