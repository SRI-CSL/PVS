;;; -*- Mode: Lisp; Package: CONSTRG -*-
(in-package "CONSTR-TERM-REP")  ;; creates package for abstract syntax. 

(in-package "CONSTRG")  ;; enters package for generated code.  

(use-package '("NEWATTR" "LANG" "SB-RUNTIME" "SORT" "TERM" "OCC" "OPER"
               "ERGOLISP"))


(export '())

(DEFPARAMETER *CONSTR-OPSIG-TABLE*
    (MAKE-OPSIG-TABLE
     '((#^CONSTR-TERM-REP::PART
        . #@("constr" :=
             (#t(:SORT SB-RUNTIME::ID) #t(:SORT CONSTR-TERM-REP::PTYPE)) :->
             #t(:SORT CONSTR-TERM-REP::PART)))
       (#^CONSTR-TERM-REP::PARTS
        . #@("constr" := (#t(:LIST (:SORT CONSTR-TERM-REP::PART))) :->
             #t(:SORT CONSTR-TERM-REP::PARTS)))
       (#^CONSTR-TERM-REP::MCONSTR-ITEM
        . #@("constr" := (#t(:SORT CONSTR-TERM-REP::DATATYPE)) :->
             #t(:SORT CONSTR-TERM-REP::ITEM)))
       (#^CONSTR-TERM-REP::ABBREV
        . #@("constr" :=
             (#t(:SORT SB-RUNTIME::ID) #t(:SORT CONSTR-TERM-REP::PTYPE)) :->
             #t(:SORT CONSTR-TERM-REP::ABBREV)))
       (#^CONSTR-TERM-REP::CONSTRS
        . #@("constr" := (#t(:LIST (:SORT CONSTR-TERM-REP::DTARM))) :->
             #t(:SORT CONSTR-TERM-REP::CONSTRS)))
       (#^CONSTR-TERM-REP::PTYPE-TV
        . #@("constr" := (#t(:SORT SB-RUNTIME::ID)) :->
             #t(:SORT CONSTR-TERM-REP::PTYPE)))
       (#^CONSTR-TERM-REP::DTARM
        . #@("constr" :=
             (#t(:SORT SB-RUNTIME::ID) #t(:SORT CONSTR-TERM-REP::PARTS)) :->
             #t(:SORT CONSTR-TERM-REP::DTARM)))
       (#^CONSTR-TERM-REP::MCONSTR
        . #@("constr" :=
             (#t(:SORT SB-RUNTIME::ID) #t(:SORT CONSTR-TERM-REP::CONSTRS)) :->
             #t(:SORT CONSTR-TERM-REP::DATATYPE)))
       (#^CONSTR-TERM-REP::ITEMS
        . #@("constr" := (#t(:LIST (:SORT CONSTR-TERM-REP::ITEM))) :->
             #t(:SORT CONSTR-TERM-REP::ITEMS)))
       (#^CONSTR-TERM-REP::ABBREV-ITEM
        . #@("constr" := (#t(:SORT CONSTR-TERM-REP::ABBREV)) :->
             #t(:SORT CONSTR-TERM-REP::ITEM)))
       (#^CONSTR-TERM-REP::PTYPE-APPL
        . #@("constr" :=
             (#t(:SORT CONSTR-TERM-REP::PTYPE)
              #t(:SORT CONSTR-TERM-REP::PTYPE))
             :-> #t(:SORT CONSTR-TERM-REP::PTYPE)))
       (#^CONSTR-TERM-REP::PTYPE-CON
        . #@("constr" := (#t(:SORT SB-RUNTIME::ID)) :->
             #t(:SORT CONSTR-TERM-REP::PTYPE)))
       (#^CONSTR-TERM-REP::CONSTR
        . #@("constr" := (#t(:SORT CONSTR-TERM-REP::DTARM)) :->
             #t(:SORT CONSTR-TERM-REP::DATATYPE)))
       (#^CONSTR-TERM-REP::PTYPE-MEMBERS
        . #@("constr" := (#t(:LIST (:SORT CONSTR-TERM-REP::PTYPE))) :->
             #t(:SORT CONSTR-TERM-REP::PTYPE))))))


(DEFPARAMETER *CONSTR-SORT-TABLE*
    (MAKE-SORT-TABLE
     '((#t(:SORT CONSTR-TERM-REP::DATATYPE)
        . #t(:UNION (:OP #^CONSTR-TERM-REP::CONSTR)
             (:OP #^CONSTR-TERM-REP::MCONSTR)))
       (#t(:SORT CONSTR-TERM-REP::PART) . #t(:OP #^CONSTR-TERM-REP::PART))
       (#t(:SORT CONSTR-TERM-REP::PARTS) . #t(:OP #^CONSTR-TERM-REP::PARTS))
       (#t(:SORT CONSTR-TERM-REP::ABBREV) . #t(:OP #^CONSTR-TERM-REP::ABBREV))
       (#t(:SORT CONSTR-TERM-REP::PTYPE)
        . #t(:UNION (:OP #^CONSTR-TERM-REP::PTYPE-APPL)
             (:OP #^CONSTR-TERM-REP::PTYPE-CON)
             (:OP #^CONSTR-TERM-REP::PTYPE-TV)
             (:OP #^CONSTR-TERM-REP::PTYPE-MEMBERS)))
       (#t(:SORT CONSTR-TERM-REP::CONSTRS)
        . #t(:OP #^CONSTR-TERM-REP::CONSTRS))
       (#t(:SORT CONSTR-TERM-REP::DTARM) . #t(:OP #^CONSTR-TERM-REP::DTARM))
       (#t(:SORT CONSTR-TERM-REP::ITEM)
        . #t(:UNION (:OP #^CONSTR-TERM-REP::MCONSTR-ITEM)
             (:OP #^CONSTR-TERM-REP::ABBREV-ITEM)))
       (#t(:SORT CONSTR-TERM-REP::ITEMS) . #t(:OP #^CONSTR-TERM-REP::ITEMS))
       (#t(:SORT CONSTR-TERM-REP::PTYPE-APPL)
        . #t(:OP #^CONSTR-TERM-REP::PTYPE-APPL))
       (#t(:SORT CONSTR-TERM-REP::PTYPE-CON)
        . #t(:OP #^CONSTR-TERM-REP::PTYPE-CON)))))

(lang:lang-define 
:name "constr"
:conc-name "constr"
:code-package "CONSTRG"
:abs-syn-package "CONSTR-TERM-REP"
:use-packages '("NEWATTR" "LANG" "SB-RUNTIME" "SORT" "TERM" "OCC" "OPER"
                "ERGOLISP")
:sub-languages '("LEXICAL-TERMINALS")
:unparse-nts 'T
:parse-routine-name 'CONSTRG::CONSTR-PARSE
:unparse-routine-name 'CONSTRG::CONSTR-UNPARSE
:win-unparse-routine-name 'CONSTRG::CONSTR-WIN-UNPARSE
:sort-table-name 'CONSTRG::*CONSTR-SORT-TABLE*
:opsig-table-name 'CONSTRG::*CONSTR-OPSIG-TABLE*
)

