;;; -*- mode: lisp; package: syntax-box -*-
(in-package :syntax-box)  ;; creates package for abstract syntax. 

(in-package :syntax-box)  ;; enters package for generated code.  

(use-package '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr))


(export '())

(defparameter *sb-opsig-table*
              (make-opsig-table
               '((#^string-aug .
                  #@("meta-grammar" :=
                                                           (#t(:sort string))
                                                           :->
                                                           #t(:sort augment-alt-0)))
                (#^bracket-entry .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort keyword)
                                                                      #t(:sort keyword))
                                                                      :->
                                                                      #t(:sort
                                                                      bracket-entry)))
                (#^ext-gram .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort sb-runtime::id)))
                                                                      :->
                                                                      #t(:sort external-grammars)))
                (#^number-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort number))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^opt-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^alt-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort augment)))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^push-tab-right .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^upats .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:op #^upat)))
                                                                      :->
                                                                      #t(:sort upats)))
                (#^sp .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort number))
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^jux .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort pattern)
                                                                      #t(:sort ws-specs))
                                                                      :->
                                                                      #t(:sort
                                                                      pattern)))
                (#^id-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort augment-alt-0)))
                (#^doublestar .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort format))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^plus-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^prec-entries .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:op #^prec-entry)))
                                                                      :->
                                                                      #t(:sort precedence-information)))
                (#^incr-bp .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort number))
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^alt .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort pattern)))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^opt .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort seq))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^augment .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort augment))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^nts .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort nonterminal-definition)))
                                                                      :->
                                                                      #t(:sort nts)))
                (#^delimiter .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort keyword))
                                                                      :->
                                                                      #t(:sort
                                                               lexical-terminals-opt-0)))
                (#^keyword-op .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort keyword))
                                                                      :->
                                                                      #t(:sort single-op-precedence-alt-0)))
                (#^unite .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern-alt-0))
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^prec-levels .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort single-level)))
                                                                      :->
                                                                      #t(:sort multiple-levels)))
                (#^comment-character .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort comment-character-opt-0)
                                                                      #t(:sort
                                                    comment-character-opt-0)
                                                                      #t(:sort
                                                                      comment-character-opt-0))
                                                                      :->
                                                                      #t(:sort
                                                                      comment-character)))
                (#^space-com .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort spacing-information-alt-0)
                                                                      #t(:sort ws-specs)
                                                                      #t(:sort
                                                                      spacing-information-alt-0))
                                                                      :->
                                                                      #t(:sort
                                                                      space-com)))
                (#^meta-grammar-null-4 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-8)))
                (#^meta-grammar-null-9 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-7)))
                (#^meta-grammar-null-8 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-6)))
                (#^meta-grammar-null-7 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-5)))
                (#^meta-grammar-null-6 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-4)))
                (#^meta-grammar-null-5 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-3)))
                (#^meta-grammar-null-3 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-2)))
                (#^meta-grammar-null-2 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-1)))
                (#^meta-grammar-null-1 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:null))
                                                                      :->
                                                                      #t(:sort meta-grammar-opt-0)))
                (#^append .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment)
                                                                      #t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^grammar .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort meta-grammar-opt-0)
                                                                      #t(:sort meta-grammar-opt-1)
                                                                      #t(:sort
                                                                      meta-grammar-opt-2)
                                                                      #t(:sort
                                                                      meta-grammar-opt-3)
                                                                      #t(:sort
                                                                      meta-grammar-opt-4)
                                                                      #t(:sort
                                                                      meta-grammar-opt-5)
                                                                      #t(:sort
                                                                      meta-grammar-opt-6)
                                                                      #t(:sort
                                                                      meta-grammar-opt-7)
                                                                      #t(:sort
                                                                      nts)
                                                                      #t(:sort
                                                                      sb-runtime::id)
                                                                      #t(:sort
                                                                      meta-grammar-opt-8))
                                                                      :->
                                                                      #t(:sort
                                                                      meta-grammar)))
                (#^upattern .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort upats))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^nt-def .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort pattern))
                                                                      :->
                                                                      #t(:sort
                                                               nonterminal-definition)))
                (#^op-info .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort keyword)))
                                                                      :->
                                                                      #t(:sort operator-information)))
                (#^list .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort augment)))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^star .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^tag-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment)
                                                                      #t(:sort pattern-alt-0))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^decr-bp .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort number))
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^name .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^term-const .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list
                   (:union (:op #^string-aug) (:op #^literal-aug)
                    (:op #^id-aug) (:sort augment))))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^ws-specs .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort format-command)))
                                                                      :->
                                                                      #t(:sort ws-specs)))
                (#^push-tab-left .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^escape-character .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort keyword))
                                                                      :->
                                                                      #t(:sort escape-character)))
                (#^nonterminal .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^prec-entry .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort multiple-levels))
                                                                      :->
                                                                      #t(:sort
                                                                      prec-entry)))
                (#^literal-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::literal))
                                                                      :->
                                                                      #t(:sort augment-alt-0)))
                (#^upat .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort uids)
                                                                      #t(:sort seq))
                                                                      :->
                                                                      #t(:sort upat)))
                (#^star-aug .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^pop-indent .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^bcons .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment)
                                                                      #t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^lex-terms .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:union (:sort sb-runtime::id) (:op #^delimiter))))
                                                                      :->
                                                                      #t(:sort
                                                                      lexical-terminals)))
                (#^doubleplus .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort format))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^ukeyword .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort keyword))
                                                                      :->
                                                                      #t(:sort ukeyword)))
                (#^prec-level .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort single-op-precedence)))
                                                                      :->
                                                                      #t(:sort single-level)))
                (#^seq .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort pattern)))
                                                                      :->
                                                                      #t(:sort seq)))
                (#^initial .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort single-op-precedence-alt-0))
                                                                      :->
                                                                      #t(:sort
                                                          single-op-precedence)))
                (#^format .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort ws-specs))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^tag .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern)
                                                                      #t(:sort pattern-alt-0))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^aggregate .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort single-op-precedence-alt-0))
                                                                      :->
                                                                      #t(:sort
                                                          single-op-precedence)))
                (#^pop-tab .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^null .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort augment)))
                (#^uids .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:sort sb-runtime::id)))
                                                                      :->
                                                                      #t(:sort uids)))
                (#^cons .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort augment)
                                                                      #t(:sort augment))
                                                                      :->
                                                                      #t(:sort augment)))
                (#^ext-nonterminal .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort
                                                                      pattern)))
                (#^medial .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort single-op-precedence-alt-0)
                                                                      #t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort
                                                                      single-op-precedence)))
                (#^push-indent .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort number))
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^bracket-entries .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:op #^bracket-entry)))
                                                                      :->
                                                                      #t(:sort
                                                     bracketing-information)))
                (#^plus .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort pattern))
                                                                      :->
                                                                      #t(:sort pattern)))
                (#^cr .
                 #@("meta-grammar"
                                                                      :=
                                                                      nil
                                                                      :->
                                                                      #t(:sort format-command)))
                (#^ext-name .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort sb-runtime::id)
                                                                      #t(:sort sb-runtime::id))
                                                                      :->
                                                                      #t(:sort
                                                                      augment)))
                (#^spacing .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:list (:op #^space-com)))
                                                                      :->
                                                                      #t(:sort spacing-information)))
                (#^jux-op .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:sort spacing-information-opt-0))
                                                                      :->
                                                                      #t(:sort
                                                         single-op-precedence-alt-0)))))) 


(defparameter *sb-sort-table*
              (make-sort-table
               '((#t(:sort spacing-information-opt-0) .
                  #t(:union
                                                                      (:sort
                                                                      number)
                                                                      (:sort
                                                                      sb-runtime::id)
                                                                      (:op
                                                                      #^null)))
                (#t(:sort
                                                                      pattern)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^upattern)
                                                                      (:op
                                                                      #^augment)
                                                                      (:op
                                                                      #^format)
                                                                      (:op
                                                                      #^tag)
                                                                      (:op
                                                                      #^doublestar)
                                                                      (:op
                                                                      #^doubleplus)
                                                                      (:op
                                                                      #^plus)
                                                                      (:op
                                                                      #^star)
                                                                      (:op
                                                                      #^alt)
                                                                      (:op
                                                                      #^seq)
                                                                      (:op
                                                                      #^opt)
                                                                      (:op
                                                                      #^jux)
                                                                      (:op
                                                                      #^ukeyword)
                                                                      (:op
                                                                      #^ext-nonterminal)
                                                                      (:op
                                                                      #^nonterminal)))
                (#t(:sort
                                                                      meta-grammar-opt-4)
                 .
                 #t(:union
                                                                      (:sort
                                                                      lexical-terminals)
                                                                      (:op
                                                                      #^meta-grammar-null-6)))
                (#t(:sort
                                                                      comment-character)
                 .
                 #t(:op
                                                                      #^comment-character))
                (#t(:sort
                                                                      format)
                 .
                 #t(:op
                                                                      #^format))
                (#t(:sort
                                                                      nts)
                 .
                 #t(:op
                                                                      #^nts))
                (#t(:sort
                                                                      space-com)
                 .
                 #t(:op
                                                                      #^space-com))
                (#t(:sort
                                                                      multiple-levels)
                 .
                 #t(:op
                                                                      #^prec-levels))
                (#t(:sort
                                                                      meta-grammar-opt-7)
                 .
                 #t(:union
                                                                      (:sort
                                                                      spacing-information)
                                                                      (:op
                                                                      #^meta-grammar-null-9)))
                (#t(:sort
                                                                      escape-character)
                 .
                 #t(:op
                                                                      #^escape-character))
                (#t(:sort
                                                                      single-op-precedence)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^aggregate)
                                                                      (:op
                                                                      #^medial)
                                                                      (:op
                                                                      #^initial)))
                (#t(:sort
                                                                      bracket-entry)
                 .
                 #t(:op
                                                                      #^bracket-entry))
                (#t(:sort
                                                                      lexical-terminals)
                 .
                 #t(:op
                                                                      #^lex-terms))
                (#t(:sort
                                                                      uids)
                 .
                 #t(:op
                                                                      #^uids))
                (#t(:sort
                                                                      single-level)
                 .
                 #t(:op
                                                                      #^prec-level))
                (#t(:sort
                                                                      meta-grammar-opt-2)
                 .
                 #t(:union
                                                                      (:sort
                                                                      comment-character)
                                                                      (:op
                                                                      #^meta-grammar-null-3)))
                (#t(:sort
                                                                      meta-grammar)
                 .
                 #t(:op
                                                                      #^grammar))
                (#t(:sort
                                                                      precedence-information)
                 .
                 #t(:op
                                                                      #^prec-entries))
                (#t(:sort
                                                                      spacing-information-alt-0)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^jux-op)
                                                                      (:sort
                                                                      sb-runtime::id)
                                                                      (:sort
                                                                      keyword)))
                (#t(:sort
                                                                      ukeyword)
                 .
                 #t(:op
                                                                      #^ukeyword))
                (#t(:sort
                                                                      ws-specs)
                 .
                 #t(:op
                                                                      #^ws-specs))
                (#t(:sort
                                                                      meta-grammar-opt-5)
                 .
                 #t(:union
                                                                      (:sort
                                                                      bracketing-information)
                                                                      (:op
                                                                      #^meta-grammar-null-7)))
                (#t(:sort
                                                                      operator-information)
                 .
                 #t(:op
                                                                      #^op-info))
                (#t(:sort
                                                                      bracketing-information)
                 .
                 #t(:op
                                                                      #^bracket-entries))
                (#t(:sort
                                                                      meta-grammar-opt-8)
                 .
                 #t(:union
                                                                      (:sort
                                                                      escape-character)
                                                                      (:op
                                                                      #^meta-grammar-null-4)))
                (#t(:sort
                                                                      upats)
                 .
                 #t(:op
                                                                      #^upats))
                (#t(:sort
                                                                      format-command)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^pop-tab)
                                                                      (:op
                                                                      #^push-tab-right)
                                                                      (:op
                                                                      #^push-tab-left)
                                                                      (:op
                                                                      #^pop-indent)
                                                                      (:op
                                                                      #^push-indent)
                                                                      (:op
                                                                      #^unite)
                                                                      (:op
                                                                      #^decr-bp)
                                                                      (:op
                                                                      #^incr-bp)
                                                                      (:op
                                                                      #^cr)
                                                                      (:op
                                                                      #^sp)))
                (#t(:sort
                                                                      augment)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^tag-aug)
                                                                      (:op
                                                                      #^opt-aug)
                                                                      (:op
                                                                      #^alt-aug)
                                                                      (:op
                                                                      #^plus-aug)
                                                                      (:op
                                                                      #^star-aug)
                                                                      (:op
                                                                      #^term-const)
                                                                      (:op
                                                                      #^append)
                                                                      (:op
                                                                      #^bcons)
                                                                      (:op
                                                                      #^cons)
                                                                      (:op
                                                                      #^list)
                                                                      (:op
                                                                      #^null)
                                                                      (:op
                                                                      #^ext-name)
                                                                      (:op
                                                                      #^name)
                                                                      (:op
                                                                      #^literal-aug)
                                                                      (:op
                                                                      #^number-aug)
                                                                      (:op
                                                                      #^string-aug)))
                (#t(:sort
                                                                      meta-grammar-opt-0)
                 .
                 #t(:union
                                                                      (:sort
                                                                      sb-runtime::id)
                                                                      (:op
                                                                      #^meta-grammar-null-1)))
                (#t(:sort
                                                                      comment-character-opt-0)
                 .
                 #t(:union
                                                                      (:sort
                                                                      keyword)
                                                                      (:op
                                                                      #^null)))
                (#t(:sort
                                                                      meta-grammar-opt-3)
                 .
                 #t(:union
                                                                      (:sort
                                                                      operator-information)
                                                                      (:op
                                                                      #^meta-grammar-null-5)))
                (#t(:sort
                                                                      upat)
                 .
                 #t(:op
                                                                      #^upat))
                (#t(:sort
                                                                      pattern-alt-0)
                 .
                 #t(:union
                                                                      (:sort
                                                                      number)
                                                                      (:sort
                                                                      sb-runtime::id)))
                (#t(:sort
                                                                      spacing-information)
                 .
                 #t(:op
                                                                      #^spacing))
                (#t(:sort
                                                                      seq)
                 .
                 #t(:op
                                                                      #^seq))
                (#t(:sort
                                                                      meta-grammar-opt-6)
                 .
                 #t(:union
                                                                      (:sort
                                                                      precedence-information)
                                                                      (:op
                                                                      #^meta-grammar-null-8)))
                (#t(:sort
                                                                      prec-entry)
                 .
                 #t(:op
                                                                      #^prec-entry))
                (#t(:sort
                                                                      single-op-precedence-alt-0)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^jux-op)
                                                                      (:op
                                                                      #^keyword-op)))
                (#t(:sort
                                                                      external-grammars)
                 .
                 #t(:op
                                                                      #^ext-gram))
                (#t(:sort
                                                                      augment-alt-0)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^string-aug)
                                                                      (:op
                                                                      #^literal-aug)
                                                                      (:op
                                                                      #^id-aug)))
                (#t(:sort
                                                                      lexical-terminals-opt-0)
                 .
                 #t(:union
                                                                      (:op
                                                                      #^delimiter)
                                                                      (:sort
                                                                      sb-runtime::id)))
                (#t(:sort
                                                                      meta-grammar-opt-1)
                 .
                 #t(:union
                                                                      (:sort
                                                                      external-grammars)
                                                                      (:op
                                                                      #^meta-grammar-null-2)))
                (#t(:sort
                                                                      nonterminal-definition)
                 .
                 #t(:op
                                                                      #^nt-def))))) 

(lang:lang-define 
:name "meta-grammar"
:conc-name "sb"
:code-package "syntax-box"
:abs-syn-package "syntax-box"
:use-packages '("ergolisp" "oper" "occ" "term" "sort" "sb-runtime" "lang" "newattr")
:sub-languages '("lexical-terminals")
:unparse-nts 't
:parse-routine-name 'syntax-box::sb-parse
:unparse-routine-name 'syntax-box::sb-unparse
:win-unparse-routine-name 'syntax-box::sb-win-unparse
:sort-table-name 'syntax-box::*sb-sort-table*
:opsig-table-name 'syntax-box::*sb-opsig-table*
)

