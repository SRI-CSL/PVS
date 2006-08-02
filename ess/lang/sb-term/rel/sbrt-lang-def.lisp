;;; Added the use-package of :ergolisp, since this is the first time
;;; package SB-RUNTIME is seen.  fp, Mon Jan  2 11:07:17 1989.
#-gcl
(defpackage :sb-runtime
  #+sbcl (:use :common-lisp :ergolisp :oper :occ :term :sort :lang))
(in-package :sb-runtime)
#-sbcl (use-package :ergolisp)
#-sbcl (use-package '(:oper :occ :term :sort :lang))

(lang:lang-define 
:name "lexical-terminals"
:conc-name "sbrt"
:working-dir "/usr/ergo/ess/lang/sb-term/rel/"
:code-package "sb-runtime"
:abs-syn-package "sb-runtime"
:use-packages '(:oper :occ :term :sort :lang)
:grammar-file ""
:sub-languages 'nil
:lexer-file ""
:parser-file ""
:unparse-nts 't
:unparser-file ""
:info-file ""
:sorts-file "/usr/ergo/ess/lang/sb-term/rel/sbrt-sorts.lisp"
:parse-routine-name 'sb-runtime::sbrt-parse
:unparse-routine-name 'sb-runtime::sbrt-unparse
:win-unparse-routine-name 'sb-runtime::sbrt-win-unparse
:sort-table-name 'sb-runtime::*sbrt-sort-table*
:opsig-table-name 'sb-runtime::*sbrt-opsig-table*
:lang-def-file "/usr/ergo/ess/lang/sb-term/rel/sbrt-lang-def.lisp"
)


;; For all others, lexical-terminals is a default sub-language.  This avoids
;; infinite loops. 

(setf (lang:lang-sub-languages
         (lang:coerce-find-lang "lexical-terminals"))
      nil)
