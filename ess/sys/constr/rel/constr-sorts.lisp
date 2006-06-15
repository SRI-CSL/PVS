;;; -*- Mode: Lisp; Package: CONSTRG -*-
(in-package :constr-term-rep)  ;; creates package for abstract syntax. 
#+cmu
(defpackage :constrg)
(in-package :constrg)  ;; enters package for generated code.  

(use-package '(:newattr :lang :sb-runtime :sort :term :occ :oper :ergolisp))


(export '())

(defparameter *constr-opsig-table*
    (make-opsig-table
     '((#^constr-term-rep::part
        . #@("constr" :=
             (#t(:sort sb-runtime::id) #t(:sort constr-term-rep::ptype)) :->
             #t(:sort constr-term-rep::part)))
       (#^constr-term-rep::parts
        . #@("constr" := (#t(:list (:sort constr-term-rep::part))) :->
             #t(:sort constr-term-rep::parts)))
       (#^constr-term-rep::mconstr-item
        . #@("constr" := (#t(:sort constr-term-rep::datatype)) :->
             #t(:sort constr-term-rep::item)))
       (#^constr-term-rep::abbrev
        . #@("constr" :=
             (#t(:sort sb-runtime::id) #t(:sort constr-term-rep::ptype)) :->
             #t(:sort constr-term-rep::abbrev)))
       (#^constr-term-rep::constrs
        . #@("constr" := (#t(:list (:sort constr-term-rep::dtarm))) :->
             #t(:sort constr-term-rep::constrs)))
       (#^constr-term-rep::ptype-tv
        . #@("constr" := (#t(:sort sb-runtime::id)) :->
             #t(:sort constr-term-rep::ptype)))
       (#^constr-term-rep::dtarm
        . #@("constr" :=
             (#t(:sort sb-runtime::id) #t(:sort constr-term-rep::parts)) :->
             #t(:sort constr-term-rep::dtarm)))
       (#^constr-term-rep::mconstr
        . #@("constr" :=
             (#t(:sort sb-runtime::id) #t(:sort constr-term-rep::constrs)) :->
             #t(:sort constr-term-rep::datatype)))
       (#^constr-term-rep::items
        . #@("constr" := (#t(:list (:sort constr-term-rep::item))) :->
             #t(:sort constr-term-rep::items)))
       (#^constr-term-rep::abbrev-item
        . #@("constr" := (#t(:sort constr-term-rep::abbrev)) :->
             #t(:sort constr-term-rep::item)))
       (#^constr-term-rep::ptype-appl
        . #@("constr" :=
             (#t(:sort constr-term-rep::ptype)
              #t(:sort constr-term-rep::ptype))
             :-> #t(:sort constr-term-rep::ptype)))
       (#^constr-term-rep::ptype-con
        . #@("constr" := (#t(:sort sb-runtime::id)) :->
             #t(:sort constr-term-rep::ptype)))
       (#^constr-term-rep::constr
        . #@("constr" := (#t(:sort constr-term-rep::dtarm)) :->
             #t(:sort constr-term-rep::datatype)))
       (#^constr-term-rep::ptype-members
        . #@("constr" := (#t(:list (:sort constr-term-rep::ptype))) :->
             #t(:sort constr-term-rep::ptype))))))


(defparameter *constr-sort-table*
    (make-sort-table
     '((#t(:sort constr-term-rep::datatype)
        . #t(:union (:op #^constr-term-rep::constr)
             (:op #^constr-term-rep::mconstr)))
       (#t(:sort constr-term-rep::part) . #t(:op #^constr-term-rep::part))
       (#t(:sort constr-term-rep::parts) . #t(:op #^constr-term-rep::parts))
       (#t(:sort constr-term-rep::abbrev) . #t(:op #^constr-term-rep::abbrev))
       (#t(:sort constr-term-rep::ptype)
        . #t(:union (:op #^constr-term-rep::ptype-appl)
             (:op #^constr-term-rep::ptype-con)
             (:op #^constr-term-rep::ptype-tv)
             (:op #^constr-term-rep::ptype-members)))
       (#t(:sort constr-term-rep::constrs)
        . #t(:op #^constr-term-rep::constrs))
       (#t(:sort constr-term-rep::dtarm) . #t(:op #^constr-term-rep::dtarm))
       (#t(:sort constr-term-rep::item)
        . #t(:union (:op #^constr-term-rep::mconstr-item)
             (:op #^constr-term-rep::abbrev-item)))
       (#t(:sort constr-term-rep::items) . #t(:op #^constr-term-rep::items))
       (#t(:sort constr-term-rep::ptype-appl)
        . #t(:op #^constr-term-rep::ptype-appl))
       (#t(:sort constr-term-rep::ptype-con)
        . #t(:op #^constr-term-rep::ptype-con)))))

(lang:lang-define 
:name "constr"
:conc-name "constr"
:code-package "constrg"
:abs-syn-package "constr-term-rep"
:use-packages '(:newattr :lang :sb-runtime :sort :term :occ :oper
                :ergolisp)
:sub-languages '("lexical-terminals")
:unparse-nts 't
:parse-routine-name 'constrg::constr-parse
:unparse-routine-name 'constrg::constr-unparse
:win-unparse-routine-name 'constrg::constr-win-unparse
:sort-table-name 'constrg::*constr-sort-table*
:opsig-table-name 'constrg::*constr-opsig-table*
)

