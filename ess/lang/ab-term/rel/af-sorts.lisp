;;; -*- Mode: Lisp; Package: ANALYSIS-FACILITY -*-
(in-package "ANALYSIS-FACILITY")  ;; creates package for abstract syntax. 

(in-package "ANALYSIS-FACILITY")  ;; enters package for generated code.  

(use-package '(:ergolisp))  ;; standard primitives 
(use-package '("OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '())

(DEFPARAMETER *AF-OPSIG-TABLE*
              (MAKE-OPSIG-TABLE
                '((#^CHILD-SORT-INSTANCE-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT CHILD-SORT-INSTANCE-OPT-0)))
                  (#^EMBEDDED-OP
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT EMB-CSORTL) #t(:SORT CHILD-SORT-INSTANCE-OPT-0)) :-> #t(:SORT CHILD-SORT-INSTANCE)))
                  (#^ITERLIST
                   . #@("agg" := (#t(:LIST (:SORT ITER))) :-> #t(:SORT ITERS)))
                  (#^CONSTRAINT
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT CONSTRAINT)))
                  (#^TLIST-CONSTITUENT
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT TLIST-ATTRL)) :-> #t(:SORT CHILD-SORT-INSTANCE)))
                  (#^TID
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT TAG)))
                  (#^ALT-CSORTL
                   . #@("agg" := (#t(:LIST (:SORT CHILD-SORT))) :-> #t(:SORT ALT-CSORTL)))
                  (#^NULL-CONSTITUENT . #@("agg" := NIL :-> #t(:SORT CHILD-SORT)))
                  (#^EXT-GRAM-NULL-2
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT EXT-GRAM-OPT-1)))
                  (#^EXT-GRAM-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT EXT-GRAM-OPT-0)))
                  (#^INH-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT LHS-ATTR)))
                  (#^EXT-GRAMLIST
                   . #@("agg" := (#t(:LIST (:SORT EXT-GRAM))) :-> #t(:SORT EXT-GRAMMARS)))
                  (#^INHF-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^DEF
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT EXP)) :-> #t(:SORT DEF)))
                  (#^EXT-GRAM
                   . #@("agg" := (#t(:SORT EXT-GRAM-OPT-0) #t(:SORT SB-RUNTIME::ID) #t(:SORT EXT-GRAM-OPT-1)) :-> #t(:SORT EXT-GRAM)))
                  (#^CONST-ATTRL
                   . #@("agg" := (#t(:LIST (:SORT RHS-ATTR))) :-> #t(:SORT CONST-ATTRL)))
                  (#^NUMBER . #@("agg" := (#t(:SORT NUMBER)) :-> #t(:SORT EXP)))
                  (#^EXT-CONSTITUENT
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID) #t(:SORT ECONST-ATTRL)) :-> #t(:SORT CHILD-SORT-INSTANCE)))
                  (#^AGG-NULL-2
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT AGG-OPT-1)))
                  (#^AGG-NULL-3
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT AGG-OPT-2)))
                  (#^SYNR-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^ETLIST-ATTRL
                   . #@("agg" := (#t(:LIST (:SORT RHS-ATTR))) :-> #t(:SORT ETLIST-ATTRL)))
                  (#^RULE-ATTRL
                   . #@("agg" := (#t(:LIST (:SORT LHS-ATTR))) :-> #t(:SORT RULE-ATTRL)))
                  (#^AGG-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT AGG-OPT-0)))
                  (#^INHL-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^INHL-ATTRL
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^MULT
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^DECL-FINAL
                   . #@("agg" := (#t(:LIST (:SORT SB-RUNTIME::ID))) :-> #t(:SORT DECLARATION)))
                  (#^NT-RULE
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT RULE-ATTRL) #t(:SORT RULE-ALTL) #t(:SORT RULE-OPT-0)) :-> #t(:SORT RULE)))
                  (#^ECONST-ATTRL
                   . #@("agg" := (#t(:LIST (:SORT RHS-ATTR))) :-> #t(:SORT ECONST-ATTRL)))
                  (#^ITE
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^INHR-ATTRL
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^EXT-TLIST-CONSTITUENT
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID) #t(:SORT ETLIST-ATTRL)) :-> #t(:SORT CHILD-SORT-INSTANCE)))
                  (#^INHR-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^VARIABLE
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT EXP)))
                  (#^CONSTRLIST
                   . #@("agg" := (#t(:LIST (:OP #^CONSTRAINT))) :-> #t(:SORT CONSTRAINTS)))
                  (#^SYN-ATTRL
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT LHS-ATTR)))
                  (#^ABSY . #@("agg" := NIL :-> #t(:SORT EXP)))
                  (#^DEFLIST
                   . #@("agg" := (#t(:LIST (:OP #^DEF))) :-> #t(:SORT DEFS)))
                  (#^ELIST
                   . #@("agg" := (#t(:LIST (:SORT EXP))) :-> #t(:SORT EXPLIST)))
                  (#^DIV
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^RULE-ALTL
                   . #@("agg" := (#t(:LIST (:SORT ALTERNATIVE))) :-> #t(:SORT RULE-ALTL)))
                  (#^ALTERNATIVE-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT ALTERNATIVE-OPT-0)))
                  (#^ALTERNATIVE-NULL-2
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT ALTERNATIVE-OPT-1)))
                  (#^ALTERNATIVE-NULL-3
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT ALTERNATIVE-OPT-2)))
                  (#^CONSTITUENT
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT CONST-ATTRL)) :-> #t(:SORT CHILD-SORT-INSTANCE)))
                  (#^CHILD-SORT-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT CHILD-SORT-OPT-0)))
                  (#^OPTSORT
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP) #t(:SORT EXP-OPT-0)) :-> #t(:SORT EXP)))
                  (#^ADD
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^EXP-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT EXP-OPT-0)))
                  (#^FUNCT
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT EXPLIST)) :-> #t(:SORT EXP)))
                  (#^ALTERNATIVE
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT ALT-CSORTL) #t(:SORT ALTERNATIVE-OPT-0) #t(:SORT ALTERNATIVE-OPT-1) #t(:SORT ALTERNATIVE-OPT-2)) :-> #t(:SORT ALTERNATIVE)))
                  (#^INHF-ATTRL
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^TLIST-ATTRL
                   . #@("agg" := (#t(:LIST (:SORT RHS-ATTR))) :-> #t(:SORT TLIST-ATTRL)))
                  (#^SYNL-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^NEG . #@("agg" := (#t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^STRING . #@("agg" := (#t(:SORT STRING)) :-> #t(:SORT EXP)))
                  (#^OPT-CONSTITUENT
                   . #@("agg" := (#t(:SORT CHILD-SORT-INSTANCE) #t(:SORT CHILD-SORT-OPT-0)) :-> #t(:SORT CHILD-SORT)))
                  (#^REG-CONSTITUENT
                   . #@("agg" := (#t(:SORT CHILD-SORT-INSTANCE)) :-> #t(:SORT CHILD-SORT)))
                  (#^MINUS
                   . #@("agg" := (#t(:SORT EXP) #t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^SYN-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT LHS-ATTR)))
                  (#^RULE-NULL-1
                   . #@("agg" := (#t(:NULL)) :-> #t(:SORT RULE-OPT-0)))
                  (#^ATTRIBUTE
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT EXP)))
                  (#^DECL-ITERATE
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT EXP) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT ITER)))
                  (#^ATTRLIST . #@("agg" := (#t(:SORT EXP)) :-> #t(:SORT EXP)))
                  (#^AGG
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT AGG-OPT-0) #t(:SORT AGG-OPT-1) #t(:SORT AGG-OPT-2) #t(:SORT RULES)) :-> #t(:SORT AGG)))
                  (#^SYNF-ATTR
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID) #t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT RHS-ATTR)))
                  (#^SYMBOL
                   . #@("agg" := (#t(:SORT SB-RUNTIME::ID)) :-> #t(:SORT EXP)))
                  (#^EMB-CSORTL
                   . #@("agg" := (#t(:LIST (:SORT CHILD-SORT))) :-> #t(:SORT EMB-CSORTL)))
                  (#^ITERMS
                   . #@("agg" := (#t(:LIST (:SORT SB-RUNTIME::ID))) :-> #t(:SORT INTRINSIC-TERMS)))
                  (#^RULELIST
                   . #@("agg" := (#t(:LIST (:SORT RULE))) :-> #t(:SORT RULES)))
                  (#^TNUM . #@("agg" := (#t(:SORT NUMBER)) :-> #t(:SORT TAG))))))


(DEFPARAMETER *AF-SORT-TABLE*
              (MAKE-SORT-TABLE
                '((#t(:SORT RHS-ATTR)
                   . #t(:UNION (:OP #^SYNL-ATTR) (:OP #^SYNF-ATTR) (:OP #^SYNR-ATTR) (:OP #^INHL-ATTRL) (:OP #^INHL-ATTR) (:OP #^INHF-ATTR) (:OP #^INHF-ATTRL) (:OP #^INHR-ATTRL) (:OP #^INHR-ATTR)))
                  (#t(:SORT CHILD-SORT-OPT-0)
                   . #t(:UNION (:SORT TAG) (:OP #^CHILD-SORT-NULL-1)))
                  (#t(:SORT ECONST-ATTRL) . #t(:OP #^ECONST-ATTRL))
                  (#t(:SORT EXP)
                   . #t(:UNION (:OP #^NEG) (:OP #^MINUS) (:OP #^ADD) (:OP #^DIV) (:OP #^MULT) (:OP #^ATTRLIST) (:OP #^ABSY) (:OP #^FUNCT) (:OP #^OPTSORT) (:OP #^ITE) (:OP #^STRING) (:OP #^NUMBER) (:OP #^SYMBOL) (:OP #^VARIABLE) (:OP #^ATTRIBUTE)))
                  (#t(:SORT RULE-ALTL) . #t(:OP #^RULE-ALTL))
                  (#t(:SORT ITER) . #t(:OP #^DECL-ITERATE))
                  (#t(:SORT ALT-CSORTL) . #t(:OP #^ALT-CSORTL))
                  (#t(:SORT CONSTRAINTS) . #t(:OP #^CONSTRLIST))
                  (#t(:SORT CHILD-SORT-INSTANCE)
                   . #t(:UNION (:OP #^EXT-TLIST-CONSTITUENT) (:OP #^EXT-CONSTITUENT) (:OP #^EMBEDDED-OP) (:OP #^TLIST-CONSTITUENT) (:OP #^CONSTITUENT)))
                  (#t(:SORT CHILD-SORT)
                   . #t(:UNION (:OP #^NULL-CONSTITUENT) (:OP #^OPT-CONSTITUENT) (:OP #^REG-CONSTITUENT)))
                  (#t(:SORT AGG-OPT-0)
                   . #t(:UNION (:SORT SB-RUNTIME::ID) (:OP #^AGG-NULL-1)))
                  (#t(:SORT CONST-ATTRL) . #t(:OP #^CONST-ATTRL))
                  (#t(:SORT AGG-OPT-2)
                   . #t(:UNION (:SORT INTRINSIC-TERMS) (:OP #^AGG-NULL-3)))
                  (#t(:SORT AGG-OPT-1)
                   . #t(:UNION (:SORT EXT-GRAMMARS) (:OP #^AGG-NULL-2)))
                  (#t(:SORT ALTERNATIVE) . #t(:OP #^ALTERNATIVE))
                  (#t(:SORT INTRINSIC-TERMS) . #t(:OP #^ITERMS))
                  (#t(:SORT EXT-GRAMMARS) . #t(:OP #^EXT-GRAMLIST))
                  (#t(:SORT TAG) . #t(:UNION (:OP #^TNUM) (:OP #^TID)))
                  (#t(:SORT DEFS) . #t(:OP #^DEFLIST))
                  (#t(:SORT TLIST-ATTRL) . #t(:OP #^TLIST-ATTRL))
                  (#t(:SORT EMB-CSORTL) . #t(:OP #^EMB-CSORTL))
                  (#t(:SORT EXPLIST) . #t(:OP #^ELIST))
                  (#t(:SORT LHS-ATTR)
                   . #t(:UNION (:OP #^SYN-ATTRL) (:OP #^SYN-ATTR) (:OP #^INH-ATTR)))
                  (#t(:SORT RULE) . #t(:OP #^NT-RULE))
                  (#t(:SORT RULES) . #t(:OP #^RULELIST))
                  (#t(:SORT EXP-OPT-0)
                   . #t(:UNION (:SORT TAG) (:OP #^EXP-NULL-1)))
                  (#t(:SORT ALTERNATIVE-OPT-2)
                   . #t(:UNION (:SORT ITERS) (:OP #^ALTERNATIVE-NULL-3)))
                  (#t(:SORT ALTERNATIVE-OPT-1)
                   . #t(:UNION (:SORT DEFS) (:OP #^ALTERNATIVE-NULL-2)))
                  (#t(:SORT ALTERNATIVE-OPT-0)
                   . #t(:UNION (:SORT CONSTRAINTS) (:OP #^ALTERNATIVE-NULL-1)))
                  (#t(:SORT AGG) . #t(:OP #^AGG))
                  (#t(:SORT DEF) . #t(:OP #^DEF))
                  (#t(:SORT RULE-ATTRL) . #t(:OP #^RULE-ATTRL))
                  (#t(:SORT RULE-OPT-0)
                   . #t(:UNION (:SORT DECLARATION) (:OP #^RULE-NULL-1)))
                  (#t(:SORT CHILD-SORT-INSTANCE-OPT-0)
                   . #t(:UNION (:SORT DEFS) (:OP #^CHILD-SORT-INSTANCE-NULL-1)))
                  (#t(:SORT EXT-GRAM-OPT-0)
                   . #t(:UNION (:SORT SB-RUNTIME::ID) (:OP #^EXT-GRAM-NULL-1)))
                  (#t(:SORT ETLIST-ATTRL) . #t(:OP #^ETLIST-ATTRL))
                  (#t(:SORT EXT-GRAM-OPT-1)
                   . #t(:UNION (:SORT SB-RUNTIME::ID) (:OP #^EXT-GRAM-NULL-2)))
                  (#t(:SORT CONSTRAINT) . #t(:OP #^CONSTRAINT))
                  (#t(:SORT DECLARATION) . #t(:OP #^DECL-FINAL))
                  (#t(:SORT EXT-GRAM) . #t(:OP #^EXT-GRAM))
                  (#t(:SORT ITERS) . #t(:OP #^ITERLIST)))))

(lang:lang-define 
:name "agg"
:conc-name "af"
:code-package "ANALYSIS-FACILITY"
:abs-syn-package "ANALYSIS-FACILITY"
:use-packages '("OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR")
:sub-languages '("LEXICAL-TERMINALS")
:unparse-nts 'T
:parse-routine-name 'ANALYSIS-FACILITY::AF-PARSE
:unparse-routine-name 'ANALYSIS-FACILITY::AF-UNPARSE
:win-unparse-routine-name 'ANALYSIS-FACILITY::AF-WIN-UNPARSE
:sort-table-name 'ANALYSIS-FACILITY::*AF-SORT-TABLE*
:opsig-table-name 'ANALYSIS-FACILITY::*AF-OPSIG-TABLE*
)

