;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package "SYNTAX-BOX")  ;; creates package for abstract syntax. 

(in-package "SYNTAX-BOX")  ;; enters package for generated code.  

(use-package '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '())

(DEFPARAMETER *SB-OPSIG-TABLE*
              (MAKE-OPSIG-TABLE
               '((#^STRING-AUG .
                  #@("meta-grammar" :=
                                                           (#t(:SORT STRING))
                                                           :->
                                                           #t(:SORT AUGMENT-ALT-0)))
                (#^BRACKET-ENTRY .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT KEYWORD)
                                                                      #t(:SORT KEYWORD))
                                                                      :->
                                                                      #t(:SORT
                                                                      BRACKET-ENTRY)))
                (#^EXT-GRAM .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT SB-RUNTIME::ID)))
                                                                      :->
                                                                      #t(:SORT EXTERNAL-GRAMMARS)))
                (#^NUMBER-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT NUMBER))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^OPT-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^ALT-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT AUGMENT)))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^PUSH-TAB-RIGHT .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^UPATS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:OP #^UPAT)))
                                                                      :->
                                                                      #t(:SORT UPATS)))
                (#^SP .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT NUMBER))
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^JUX .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT PATTERN)
                                                                      #t(:SORT WS-SPECS))
                                                                      :->
                                                                      #t(:SORT
                                                                      PATTERN)))
                (#^ID-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT AUGMENT-ALT-0)))
                (#^DOUBLESTAR .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT FORMAT))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^PLUS-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^PREC-ENTRIES .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:OP #^PREC-ENTRY)))
                                                                      :->
                                                                      #t(:SORT PRECEDENCE-INFORMATION)))
                (#^INCR-BP .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT NUMBER))
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^ALT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT PATTERN)))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^OPT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SEQ))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^AUGMENT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^NTS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT NONTERMINAL-DEFINITION)))
                                                                      :->
                                                                      #t(:SORT NTS)))
                (#^DELIMITER .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT KEYWORD))
                                                                      :->
                                                                      #t(:SORT
                                                               LEXICAL-TERMINALS-OPT-0)))
                (#^KEYWORD-OP .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT KEYWORD))
                                                                      :->
                                                                      #t(:SORT SINGLE-OP-PRECEDENCE-ALT-0)))
                (#^UNITE .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN-ALT-0))
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^PREC-LEVELS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT SINGLE-LEVEL)))
                                                                      :->
                                                                      #t(:SORT MULTIPLE-LEVELS)))
                (#^COMMENT-CHARACTER .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT COMMENT-CHARACTER-OPT-0)
                                                                      #t(:SORT
                                                    COMMENT-CHARACTER-OPT-0)
                                                                      #t(:SORT
                                                                      COMMENT-CHARACTER-OPT-0))
                                                                      :->
                                                                      #t(:SORT
                                                                      COMMENT-CHARACTER)))
                (#^SPACE-COM .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SPACING-INFORMATION-ALT-0)
                                                                      #t(:SORT WS-SPECS)
                                                                      #t(:SORT
                                                                      SPACING-INFORMATION-ALT-0))
                                                                      :->
                                                                      #t(:SORT
                                                                      SPACE-COM)))
                (#^META-GRAMMAR-NULL-4 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-8)))
                (#^META-GRAMMAR-NULL-9 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-7)))
                (#^META-GRAMMAR-NULL-8 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-6)))
                (#^META-GRAMMAR-NULL-7 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-5)))
                (#^META-GRAMMAR-NULL-6 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-4)))
                (#^META-GRAMMAR-NULL-5 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-3)))
                (#^META-GRAMMAR-NULL-3 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-2)))
                (#^META-GRAMMAR-NULL-2 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-1)))
                (#^META-GRAMMAR-NULL-1 .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:NULL))
                                                                      :->
                                                                      #t(:SORT META-GRAMMAR-OPT-0)))
                (#^APPEND .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT)
                                                                      #t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^GRAMMAR .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT META-GRAMMAR-OPT-0)
                                                                      #t(:SORT META-GRAMMAR-OPT-1)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-2)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-3)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-4)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-5)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-6)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-7)
                                                                      #t(:SORT
                                                                      NTS)
                                                                      #t(:SORT
                                                                      SB-RUNTIME::ID)
                                                                      #t(:SORT
                                                                      META-GRAMMAR-OPT-8))
                                                                      :->
                                                                      #t(:SORT
                                                                      META-GRAMMAR)))
                (#^UPATTERN .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT UPATS))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^NT-DEF .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT PATTERN))
                                                                      :->
                                                                      #t(:SORT
                                                               NONTERMINAL-DEFINITION)))
                (#^OP-INFO .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT KEYWORD)))
                                                                      :->
                                                                      #t(:SORT OPERATOR-INFORMATION)))
                (#^LIST .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT AUGMENT)))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^STAR .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^TAG-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT)
                                                                      #t(:SORT PATTERN-ALT-0))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^DECR-BP .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT NUMBER))
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^NAME .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^TERM-CONST .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST
                   (:UNION (:OP #^STRING-AUG) (:OP #^LITERAL-AUG)
                    (:OP #^ID-AUG) (:SORT AUGMENT))))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^WS-SPECS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT FORMAT-COMMAND)))
                                                                      :->
                                                                      #t(:SORT WS-SPECS)))
                (#^PUSH-TAB-LEFT .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^ESCAPE-CHARACTER .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT KEYWORD))
                                                                      :->
                                                                      #t(:SORT ESCAPE-CHARACTER)))
                (#^NONTERMINAL .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^PREC-ENTRY .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT MULTIPLE-LEVELS))
                                                                      :->
                                                                      #t(:SORT
                                                                      PREC-ENTRY)))
                (#^LITERAL-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::LITERAL))
                                                                      :->
                                                                      #t(:SORT AUGMENT-ALT-0)))
                (#^UPAT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT UIDS)
                                                                      #t(:SORT SEQ))
                                                                      :->
                                                                      #t(:SORT UPAT)))
                (#^STAR-AUG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^POP-INDENT .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^BCONS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT)
                                                                      #t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^LEX-TERMS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:UNION (:SORT SB-RUNTIME::ID) (:OP #^DELIMITER))))
                                                                      :->
                                                                      #t(:SORT
                                                                      LEXICAL-TERMINALS)))
                (#^DOUBLEPLUS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT FORMAT))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^UKEYWORD .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT KEYWORD))
                                                                      :->
                                                                      #t(:SORT UKEYWORD)))
                (#^PREC-LEVEL .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT SINGLE-OP-PRECEDENCE)))
                                                                      :->
                                                                      #t(:SORT SINGLE-LEVEL)))
                (#^SEQ .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT PATTERN)))
                                                                      :->
                                                                      #t(:SORT SEQ)))
                (#^INITIAL .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SINGLE-OP-PRECEDENCE-ALT-0))
                                                                      :->
                                                                      #t(:SORT
                                                          SINGLE-OP-PRECEDENCE)))
                (#^FORMAT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT WS-SPECS))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^TAG .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN)
                                                                      #t(:SORT PATTERN-ALT-0))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^AGGREGATE .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SINGLE-OP-PRECEDENCE-ALT-0))
                                                                      :->
                                                                      #t(:SORT
                                                          SINGLE-OP-PRECEDENCE)))
                (#^POP-TAB .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^NULL .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^UIDS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:SORT SB-RUNTIME::ID)))
                                                                      :->
                                                                      #t(:SORT UIDS)))
                (#^CONS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT AUGMENT)
                                                                      #t(:SORT AUGMENT))
                                                                      :->
                                                                      #t(:SORT AUGMENT)))
                (#^EXT-NONTERMINAL .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT
                                                                      PATTERN)))
                (#^MEDIAL .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SINGLE-OP-PRECEDENCE-ALT-0)
                                                                      #t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT
                                                                      SINGLE-OP-PRECEDENCE)))
                (#^PUSH-INDENT .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT NUMBER))
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^BRACKET-ENTRIES .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:OP #^BRACKET-ENTRY)))
                                                                      :->
                                                                      #t(:SORT
                                                     BRACKETING-INFORMATION)))
                (#^PLUS .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT PATTERN))
                                                                      :->
                                                                      #t(:SORT PATTERN)))
                (#^CR .
                 #@("meta-grammar"
                                                                      :=
                                                                      NIL
                                                                      :->
                                                                      #t(:SORT FORMAT-COMMAND)))
                (#^EXT-NAME .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SB-RUNTIME::ID)
                                                                      #t(:SORT SB-RUNTIME::ID))
                                                                      :->
                                                                      #t(:SORT
                                                                      AUGMENT)))
                (#^SPACING .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:LIST (:OP #^SPACE-COM)))
                                                                      :->
                                                                      #t(:SORT SPACING-INFORMATION)))
                (#^JUX-OP .
                 #@("meta-grammar"
                                                                      :=
                                                                      (#t(:SORT SPACING-INFORMATION-OPT-0))
                                                                      :->
                                                                      #t(:SORT
                                                         SINGLE-OP-PRECEDENCE-ALT-0)))))) 


(DEFPARAMETER *SB-SORT-TABLE*
              (MAKE-SORT-TABLE
               '((#t(:SORT SPACING-INFORMATION-OPT-0) .
                  #t(:UNION
                                                                      (:SORT
                                                                      NUMBER)
                                                                      (:SORT
                                                                      SB-RUNTIME::ID)
                                                                      (:OP
                                                                      #^NULL)))
                (#t(:SORT
                                                                      PATTERN)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^UPATTERN)
                                                                      (:OP
                                                                      #^AUGMENT)
                                                                      (:OP
                                                                      #^FORMAT)
                                                                      (:OP
                                                                      #^TAG)
                                                                      (:OP
                                                                      #^DOUBLESTAR)
                                                                      (:OP
                                                                      #^DOUBLEPLUS)
                                                                      (:OP
                                                                      #^PLUS)
                                                                      (:OP
                                                                      #^STAR)
                                                                      (:OP
                                                                      #^ALT)
                                                                      (:OP
                                                                      #^SEQ)
                                                                      (:OP
                                                                      #^OPT)
                                                                      (:OP
                                                                      #^JUX)
                                                                      (:OP
                                                                      #^UKEYWORD)
                                                                      (:OP
                                                                      #^EXT-NONTERMINAL)
                                                                      (:OP
                                                                      #^NONTERMINAL)))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-4)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      LEXICAL-TERMINALS)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-6)))
                (#t(:SORT
                                                                      COMMENT-CHARACTER)
                 .
                 #t(:OP
                                                                      #^COMMENT-CHARACTER))
                (#t(:SORT
                                                                      FORMAT)
                 .
                 #t(:OP
                                                                      #^FORMAT))
                (#t(:SORT
                                                                      NTS)
                 .
                 #t(:OP
                                                                      #^NTS))
                (#t(:SORT
                                                                      SPACE-COM)
                 .
                 #t(:OP
                                                                      #^SPACE-COM))
                (#t(:SORT
                                                                      MULTIPLE-LEVELS)
                 .
                 #t(:OP
                                                                      #^PREC-LEVELS))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-7)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      SPACING-INFORMATION)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-9)))
                (#t(:SORT
                                                                      ESCAPE-CHARACTER)
                 .
                 #t(:OP
                                                                      #^ESCAPE-CHARACTER))
                (#t(:SORT
                                                                      SINGLE-OP-PRECEDENCE)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^AGGREGATE)
                                                                      (:OP
                                                                      #^MEDIAL)
                                                                      (:OP
                                                                      #^INITIAL)))
                (#t(:SORT
                                                                      BRACKET-ENTRY)
                 .
                 #t(:OP
                                                                      #^BRACKET-ENTRY))
                (#t(:SORT
                                                                      LEXICAL-TERMINALS)
                 .
                 #t(:OP
                                                                      #^LEX-TERMS))
                (#t(:SORT
                                                                      UIDS)
                 .
                 #t(:OP
                                                                      #^UIDS))
                (#t(:SORT
                                                                      SINGLE-LEVEL)
                 .
                 #t(:OP
                                                                      #^PREC-LEVEL))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-2)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      COMMENT-CHARACTER)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-3)))
                (#t(:SORT
                                                                      META-GRAMMAR)
                 .
                 #t(:OP
                                                                      #^GRAMMAR))
                (#t(:SORT
                                                                      PRECEDENCE-INFORMATION)
                 .
                 #t(:OP
                                                                      #^PREC-ENTRIES))
                (#t(:SORT
                                                                      SPACING-INFORMATION-ALT-0)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^JUX-OP)
                                                                      (:SORT
                                                                      SB-RUNTIME::ID)
                                                                      (:SORT
                                                                      KEYWORD)))
                (#t(:SORT
                                                                      UKEYWORD)
                 .
                 #t(:OP
                                                                      #^UKEYWORD))
                (#t(:SORT
                                                                      WS-SPECS)
                 .
                 #t(:OP
                                                                      #^WS-SPECS))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-5)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      BRACKETING-INFORMATION)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-7)))
                (#t(:SORT
                                                                      OPERATOR-INFORMATION)
                 .
                 #t(:OP
                                                                      #^OP-INFO))
                (#t(:SORT
                                                                      BRACKETING-INFORMATION)
                 .
                 #t(:OP
                                                                      #^BRACKET-ENTRIES))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-8)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      ESCAPE-CHARACTER)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-4)))
                (#t(:SORT
                                                                      UPATS)
                 .
                 #t(:OP
                                                                      #^UPATS))
                (#t(:SORT
                                                                      FORMAT-COMMAND)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^POP-TAB)
                                                                      (:OP
                                                                      #^PUSH-TAB-RIGHT)
                                                                      (:OP
                                                                      #^PUSH-TAB-LEFT)
                                                                      (:OP
                                                                      #^POP-INDENT)
                                                                      (:OP
                                                                      #^PUSH-INDENT)
                                                                      (:OP
                                                                      #^UNITE)
                                                                      (:OP
                                                                      #^DECR-BP)
                                                                      (:OP
                                                                      #^INCR-BP)
                                                                      (:OP
                                                                      #^CR)
                                                                      (:OP
                                                                      #^SP)))
                (#t(:SORT
                                                                      AUGMENT)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^TAG-AUG)
                                                                      (:OP
                                                                      #^OPT-AUG)
                                                                      (:OP
                                                                      #^ALT-AUG)
                                                                      (:OP
                                                                      #^PLUS-AUG)
                                                                      (:OP
                                                                      #^STAR-AUG)
                                                                      (:OP
                                                                      #^TERM-CONST)
                                                                      (:OP
                                                                      #^APPEND)
                                                                      (:OP
                                                                      #^BCONS)
                                                                      (:OP
                                                                      #^CONS)
                                                                      (:OP
                                                                      #^LIST)
                                                                      (:OP
                                                                      #^NULL)
                                                                      (:OP
                                                                      #^EXT-NAME)
                                                                      (:OP
                                                                      #^NAME)
                                                                      (:OP
                                                                      #^LITERAL-AUG)
                                                                      (:OP
                                                                      #^NUMBER-AUG)
                                                                      (:OP
                                                                      #^STRING-AUG)))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-0)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      SB-RUNTIME::ID)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-1)))
                (#t(:SORT
                                                                      COMMENT-CHARACTER-OPT-0)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      KEYWORD)
                                                                      (:OP
                                                                      #^NULL)))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-3)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      OPERATOR-INFORMATION)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-5)))
                (#t(:SORT
                                                                      UPAT)
                 .
                 #t(:OP
                                                                      #^UPAT))
                (#t(:SORT
                                                                      PATTERN-ALT-0)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      NUMBER)
                                                                      (:SORT
                                                                      SB-RUNTIME::ID)))
                (#t(:SORT
                                                                      SPACING-INFORMATION)
                 .
                 #t(:OP
                                                                      #^SPACING))
                (#t(:SORT
                                                                      SEQ)
                 .
                 #t(:OP
                                                                      #^SEQ))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-6)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      PRECEDENCE-INFORMATION)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-8)))
                (#t(:SORT
                                                                      PREC-ENTRY)
                 .
                 #t(:OP
                                                                      #^PREC-ENTRY))
                (#t(:SORT
                                                                      SINGLE-OP-PRECEDENCE-ALT-0)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^JUX-OP)
                                                                      (:OP
                                                                      #^KEYWORD-OP)))
                (#t(:SORT
                                                                      EXTERNAL-GRAMMARS)
                 .
                 #t(:OP
                                                                      #^EXT-GRAM))
                (#t(:SORT
                                                                      AUGMENT-ALT-0)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^STRING-AUG)
                                                                      (:OP
                                                                      #^LITERAL-AUG)
                                                                      (:OP
                                                                      #^ID-AUG)))
                (#t(:SORT
                                                                      LEXICAL-TERMINALS-OPT-0)
                 .
                 #t(:UNION
                                                                      (:OP
                                                                      #^DELIMITER)
                                                                      (:SORT
                                                                      SB-RUNTIME::ID)))
                (#t(:SORT
                                                                      META-GRAMMAR-OPT-1)
                 .
                 #t(:UNION
                                                                      (:SORT
                                                                      EXTERNAL-GRAMMARS)
                                                                      (:OP
                                                                      #^META-GRAMMAR-NULL-2)))
                (#t(:SORT
                                                                      NONTERMINAL-DEFINITION)
                 .
                 #t(:OP
                                                                      #^NT-DEF))))) 

(lang:lang-define 
:name "meta-grammar"
:conc-name "sb"
:code-package "SYNTAX-BOX"
:abs-syn-package "SYNTAX-BOX"
:use-packages '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR")
:sub-languages '("LEXICAL-TERMINALS")
:unparse-nts 'T
:parse-routine-name 'SYNTAX-BOX::SB-PARSE
:unparse-routine-name 'SYNTAX-BOX::SB-UNPARSE
:win-unparse-routine-name 'SYNTAX-BOX::SB-WIN-UNPARSE
:sort-table-name 'SYNTAX-BOX::*SB-SORT-TABLE*
:opsig-table-name 'SYNTAX-BOX::*SB-OPSIG-TABLE*
)

