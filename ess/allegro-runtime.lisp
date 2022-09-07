#+allegro
(eval-when (:execute :load-toplevel)
  (setq system:*load-search-list*
	(list #p"" #p(:type "lfasl") #p(:type "cl") #p(:type "lisp"))))
(defvar *ess-path*
  (format nil "~a/ess" (or #+allegro (sys:getenv "PVSPATH")
			   #+gcl (si:getenv "PVSPATH")
			   #+cmu (cdr (assoc :PVSPATH
					     extensions::*environment-list*))
			   ".")))
;; (load (format nil "~a/sys/ergolisp/rel/ergolisp" *ess-path*))
;; (load (format nil "~a/sys/ergolisp/rel/ergolisp-exports" *ess-path*))
;;[Regenerating and Loading box #>B"ERGOMISC".]
(load (format nil "~a/sys/ergolisp/rel/ergo-system" *ess-path*))
(load (format nil "~a/sys/ergolisp/rel/ergo-types" *ess-path*))
(load (format nil "~a/sys/ergolisp/rel/type-check" *ess-path*))
;;[Regenerating and Loading box #>B"TOOLS".]
(load (format nil "~a/sys/tools/rel/regression-test" *ess-path*))
(load (format nil "~a/sys/tools/rel/clet" *ess-path*))
(load (format nil "~a/sys/tools/rel/retry" *ess-path*))
(load (format nil "~a/sys/tools/rel/box-system" *ess-path*))
(load (format nil "~a/sys/tools/rel/box" *ess-path*))
(load (format nil "~a/sys/tools/rel/box-lib" *ess-path*))
(load (format nil "~a/sys/tools/rel/print-utils" *ess-path*))
;;[Regenerating and Loading box #>B"DLAMBDA".]
(load (format nil "~a/sys/ergolisp/rel/dlambda" *ess-path*))
(load (format nil "~a/sys/ergolisp/rel/dlambda-lib" *ess-path*))
;;[Regenerating and Loading box #>B"LANGUAGES".]
(load (format nil "~a/term/language/rel/languages" *ess-path*))
;;[Regenerating and Loading box #>B"OPERATORS".]
(load (format nil "~a/term/terms/rel/opers" *ess-path*))
;;[Regenerating and Loading box #>B"OCCUR".]
(load (format nil "~a/term/terms/rel/occur" *ess-path*))
;;[Regenerating and Loading box #>B"SORTS".]
;(load (format nil "~a/term/terms/rel/sorts" *ess-path*))
;;;[Regenerating and Loading box #>B"GTERM".]
;(load (format nil "~a/term/trep/rel/attr-prims" *ess-path*))
;(load (format nil "~a/term/trep/rel/gterm" *ess-path*))
;;;[Regenerating and Loading box #>B"TERMS".]
;(load (format nil "~a/term/terms/rel/terms" *ess-path*))
;(load (format nil "~a/term/terms/rel/termop" *ess-path*))
;;[Loading box #>B"SB-SUPPORT".]
;(load (format nil "~a/lang/sb-term/rel/sbrt-lang-def" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/sbrt-sorting" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-structs" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-structs" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-lex.lisp" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-lex" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-term" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-parse-mac" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-parse" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unparse" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-lang-lib" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-sort" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-lang" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-global" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-attr" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-format" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-tex" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-top" *ess-path*))
;;;[Loading box #>B"ATTR".]
;(load (format nil "~a/term/attr/rel/attr-lib" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-gsort" *ess-path*))
;(load (format nil "~a/term/attr/rel/attr-occ" *ess-path*))
;;;[Regenerating and Loading box #>B"SB-SUPPORT".]
;(load (format nil "~a/lang/sb-term/rel/sbrt-lang-def" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/sbrt-sorting" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-structs" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-structs" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-lex" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-term" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-parse-mac" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-parse" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unparse" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-attr" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-format" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-tex" *ess-path*))
;(load (format nil "~a/lang/sb-term/rel/rt-unp-top" *ess-path*))
;;;[Regenerating and Loading box #>B"ATTR".]
;;;[Regenerating and Loading box #>B"AB-SUPPORT".]
;(load (format nil "~a/lang/ab-term/rel/af-runtime" *ess-path*))
;;;[Regenerating and Loading box #>B"CONSTR".]
;(load (format nil "~a/sys/constr/rel/constr" *ess-path*))
;(load (format nil "~a/sys/constr/rel/constr-term-rep" *ess-path*))
;(load (format nil "~a/sys/constr/rel/constr-sorts" *ess-path*))
;(load (format nil "~a/sys/constr/rel/constr-lexer" *ess-path*))
;(load (format nil "~a/sys/constr/rel/constr-parser" *ess-path*))
;(load (format nil "~a/sys/constr/rel/defsconstr" *ess-path*))
;;;[Regenerating and Loading box #>B"TDEFUN".]
;(load (format nil "~a/sys/ergolisp/rel/tdefun" *ess-path*))
;(excl:dumplisp :name (format nil "~a/ess/bin/ess-allegro-sun4" *ess-path*)
;	       :checkpoint t)
