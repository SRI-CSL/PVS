(in-package "PVS") ;; package for abstract syntax.

(in-package "PVS")  ;; package for generated code.  

(use-package '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))

(lang:lang-define 
:name "PVS2.3"
:conc-name "pvs2.3"
:working-dir (format nil "~a/src/" *pvs-path*)
:code-package "PVS"
:abs-syn-package "PVS"
:use-packages '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR")
:grammar-file (format nil "~a/src/pvs-gr.txt")
:sub-languages '("LEXICAL-TERMINALS")
:lexer-file (format nil "~a/src/new-pvs-lexer.lisp" *pvs-path*)
:parser-file (format nil "~a/src/new-pvs-parser.lisp" *pvs-path*)
:unparse-nts 'T
:unparser-file (format nil "~a/src/new-pvs-unparser.lisp" *pvs-path*)
:info-file (format nil "~a/src/new-pvs-info.lisp" *pvs-path*)
:sorts-file (format nil "~a/src/new-pvs-sorts.lisp" *pvs-path*)
:parse-routine-name 'PVS::PVS-PARSE
:unparse-routine-name 'PVS::PVS-UNPARSE
:win-unparse-routine-name 'PVS::PVS-WIN-UNPARSE
:sort-table-name 'PVS::*PVS-SORT-TABLE*
:opsig-table-name 'PVS::*PVS-OPSIG-TABLE*
)
