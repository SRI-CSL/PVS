;;; Added the use-package of :ergolisp, since this is the first time
;;; package SB-RUNTIME is seen.  fp, Mon Jan  2 11:07:17 1989.
#-gcl
(defpackage "SB-RUNTIME")
(in-package "SB-RUNTIME") (use-package :ergolisp)

(use-package '("OPER" "OCC" "TERM" "SORT" "LANG"))

(lang:lang-define 
:name "lexical-terminals"
:conc-name "sbrt"
:working-dir "/usr/ergo/ess/lang/sb-term/rel/"
:code-package "SB-RUNTIME"
:abs-syn-package "SB-RUNTIME"
:use-packages '("OPER" "OCC" "TERM" "SORT" "LANG")
:grammar-file ""
:sub-languages 'nil
:lexer-file ""
:parser-file ""
:unparse-nts 't
:unparser-file ""
:info-file ""
:sorts-file "/usr/ergo/ess/lang/sb-term/rel/sbrt-sorts.lisp"
:parse-routine-name 'SB-RUNTIME::SBRT-PARSE
:unparse-routine-name 'SB-RUNTIME::SBRT-UNPARSE
:win-unparse-routine-name 'SB-RUNTIME::SBRT-WIN-UNPARSE
:sort-table-name 'SB-RUNTIME::*SBRT-SORT-TABLE*
:opsig-table-name 'SB-RUNTIME::*SBRT-OPSIG-TABLE*
:lang-def-file "/usr/ergo/ess/lang/sb-term/rel/sbrt-lang-def.lisp"
)


;; For all others, lexical-terminals is a default sub-language.  This avoids
;; infinite loops. 

(setf (lang:lang-sub-languages
         (lang:coerce-find-lang "lexical-terminals"))
      nil)
