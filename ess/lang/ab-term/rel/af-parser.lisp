;;; -*- Mode: Lisp; Package: ANALYSIS-FACILITY -*-
(in-package "ANALYSIS-FACILITY")  ;; creates package for abstract syntax. 

(in-package "ANALYSIS-FACILITY")  ;; enters package for generated code.  

(use-package '(:ergolisp))  ;; standard primitives 
(use-package '("OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '( AF-PARSE ))

(DEFPARAMETER AF-ABS-SYN-PACKAGE
              (FIND-PACKAGE "ANALYSIS-FACILITY"))

(DEFUN AF-PARSE
       (&KEY (NT 'ANALYSIS-FACILITY::AGG)
             ERROR-THRESHOLD
             ASK-ABOUT-BAD-TOKENS
             (RETURN-ERRORS NIL)
             (STREAM NIL STREAMP)
             STRING
             FILE
             (EXHAUST-STREAM NIL))
       (COND (STREAM)
             (STRING (SETF STREAM
                           (MAKE-STRING-INPUT-STREAM STRING)))
             (FILE (SETF STREAM (OPEN FILE)))
             (T
              (ERROR
                "Must provide an input means -- either :stream, ~
			:string, or :file.")))
       (SETQ *AF-KEYWORD-TABLE* (INIT-KEYWORD-TABLE AF-KEYWORD-LIST))
       (LET ((*PARSER-ERROR* NIL)
             (*PARSER-ERROR-COUNT* 0)
             (*PARSER-RETURN-ERRORS* RETURN-ERRORS)
             (*PARSER-ABORT-THRESHOLD* ERROR-THRESHOLD)
             (*ASK-ABOUT-BAD-TOKENS* ASK-ABOUT-BAD-TOKENS)
             (*LEXICAL-STREAM* (OPEN-LEXICAL-STREAM (OPEN-PLACESTREAM STREAM)))
             (*HOLD-A1* NIL)
             (*HOLD-A2* NIL)
             (*HOLD-B1* NIL)
             (*HOLD-B2* NIL)
             (*ABS-SYN-PACKAGE* AF-ABS-SYN-PACKAGE)
             (*READER-FUN* #'READER)
             (*APPLY-LEX-TERM-CONSTR-FUN* #'APPLY-LEXICAL-TERMINAL-CONSTRUCTOR)
             (*KEYWORD-TABLE* *AF-KEYWORD-TABLE*)
             (*CLOSE-COMMENT-CHAR* AF-CLOSE-COMMENT-CHAR)
             (*CASE-SENSITIVE* AF-CASE-SENSITIVE))
         (INIT-LEXER *LEXICAL-STREAM*)
         (MULTIPLE-VALUE-BIND (ABS-SYNTAX ERROR-STRING
                                          ERROR-ARGS
                                          ERROR-PLACE)
                              (UNWIND-PROTECT
                                (CATCH 'PARSER-ABORT-CATCH
                                       (PROG1
                                         (CASE NT
                                               (ANALYSIS-FACILITY::AGG
                                                 (!AGG! 0))
                                               (ANALYSIS-FACILITY::EXT-GRAMMARS
                                                 (!EXT-GRAMMARS! 0))
                                               (ANALYSIS-FACILITY::EXT-GRAM
                                                 (!EXT-GRAM! 0))
                                               (ANALYSIS-FACILITY::INTRINSIC-TERMS
                                                 (!INTRINSIC-TERMS! 0))
                                               (ANALYSIS-FACILITY::RULES
                                                 (!RULES! 0))
                                               (ANALYSIS-FACILITY::RULE
                                                 (!RULE! 0))
                                               (ANALYSIS-FACILITY::ALTERNATIVE
                                                 (!ALTERNATIVE! 0))
                                               (ANALYSIS-FACILITY::LHS-ATTR
                                                 (!LHS-ATTR! 0))
                                               (ANALYSIS-FACILITY::CHILD-SORT
                                                 (!CHILD-SORT! 0))
                                               (ANALYSIS-FACILITY::CHILD-SORT-INSTANCE
                                                 (!CHILD-SORT-INSTANCE! 0))
                                               (ANALYSIS-FACILITY::RHS-ATTR
                                                 (!RHS-ATTR! 0))
                                               (ANALYSIS-FACILITY::DEFS
                                                 (!DEFS! 0))
                                               (ANALYSIS-FACILITY::CONSTRAINTS
                                                 (!CONSTRAINTS! 0))
                                               (EXP (!EXP! 0))
                                               (ANALYSIS-FACILITY::EXPLIST
                                                 (!EXPLIST! 0))
                                               (ANALYSIS-FACILITY::ITERS
                                                 (!ITERS! 0))
                                               (ANALYSIS-FACILITY::ITER
                                                 (!ITER! 0))
                                               (DECLARATION (!DECLARATION! 0))
                                               (ANALYSIS-FACILITY::TAG
                                                 (!TAG! 0))
                                               (T
                                                 (ERROR
                                                   "Unknown nonterminal ~A."
                                                   NT)))
                                         (WHEN
                                           (OR FILE STRING EXHAUST-STREAM)
                                           (CLET*
                                             (((TOKEN IGNORE PLACE)
                                                (PEEK-FIRST)))
                                             (UNLESS
                                               (OR (EQ TOKEN :EOF)
                                                   (EQ TOKEN 'EOF))
                                               (DO-SYNTAX-ERROR
                                                 "There is garbage at the end of your ~
			      file or string:~%~A"
                                                 PLACE))))))
                                (UNLESS STREAMP
                                        (CLOSE STREAM)))
                              (IF RETURN-ERRORS
                                  (VALUES ABS-SYNTAX
                                          *PARSER-ERROR*
                                          ERROR-PLACE
                                          ERROR-STRING
                                          ERROR-ARGS)
                                  (VALUES ABS-SYNTAX *PARSER-ERROR*)))))


(DEFUN !TAG!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1)
         (ERGO-IGNORE-IF-UNUSED V0 V1)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID!)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::TID
                                              (LIST V1)))))
           ((LA-MATCH ((SBST::!NUMBER!)))
             (PROGN (LAM ((SBST::!NUMBER! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::TNUM
                                              (LIST V1)))))
           (T
             (INITIAL-ERROR
               '((SBST::!NUMBER! EPSILON) (SBST::!ID! EPSILON)))))
         (VALUE-TO-SLOT V0 V1)
         V0))


(DEFUN !DECLARATION!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::FINAL)))
             (PROGN (LAM ((SBST::FINAL EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON))
                                      (LAM ((SBST::!ID! EPSILON))
                                           (GOBBLE-TO-SLOT V2))
                                      V1
                                      V2
                                      'SBST::|,|)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::DECL-FINAL
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T (INITIAL-ERROR '((SBST::FINAL SBST::!ID!)))))
         NIL
         V0))


(DEFUN !ITER!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::ITERATE)))
             (PROGN (LAM ((SBST::ITERATE EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (LAM ((SBST::INITIAL EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V2 (!EXP! 0))
                    (LAM ((SBST::TEST EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V3))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::DECL-ITERATE
                                              (LIST V1 V2 V3)))))
           (T (INITIAL-ERROR '((SBST::ITERATE SBST::!ID!)))))
         NIL
         V0))


(DEFUN !ITERS!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1)
         (ERGO-IGNORE-IF-UNUSED V0 V1)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::ITERATE)))
             (PROGN (CODE-TO-SLOT V1 (!ITER! 0))
                    (CONS-TO-SLOT V0 V1)
                    (GEN-STAR-PARSE ((SBST::|,| SBST::ITERATE))
                                    (PROGN
                                      (LAM ((SBST::|,| EPSILON))
                                           (GOBBLE-TOKEN))
                                      (CODE-TO-SLOT V1 (!ITER! 0)))
                                    V0
                                    V1)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ITERLIST
                                              (DS-TEMP-REPT (CK-REPT-REF V0))))))
           (T (INITIAL-ERROR '((SBST::ITERATE SBST::!ID!)))))
         NIL
         V0))


(DEFUN !EXPLIST!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::|(|)))
             (PROGN (LAM ((SBST::|(| EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLESTAR-PARSE
                      ((SBST::|(| SBST::|(|
                                  SBST::-
                                  SBST::AST
                                  SBST::[
                                  SBST::IF
                                  SBST::!STRING!
                                  SBST::!NUMBER!
                                  SBST::|'|
                                  SBST::|#|
                                  SBST::!ID!)
                        (SBST::- SBST::|(|
                                 SBST::-
                                 SBST::AST
                                 SBST::[
                                 SBST::IF
                                 SBST::!STRING!
                                 SBST::!NUMBER!
                                 SBST::|'|
                                 SBST::|#|
                                 SBST::!ID!)
                        (SBST::AST EPSILON
                                   SBST::-
                                   SBST::+
                                   SBST::/
                                   SBST::*)
                        (SBST::[ SBST::|(|
                                 SBST::-
                                 SBST::AST
                                 SBST::[
                                 SBST::IF
                                 SBST::!STRING!
                                 SBST::!NUMBER!
                                 SBST::|'|
                                 SBST::|#|
                                 SBST::!ID!)
                        (SBST::IF SBST::|(|
                                  SBST::-
                                  SBST::AST
                                  SBST::[
                                  SBST::IF
                                  SBST::!STRING!
                                  SBST::!NUMBER!
                                  SBST::|'|
                                  SBST::|#|
                                  SBST::!ID!)
                        (SBST::!STRING! EPSILON
                                        SBST::-
                                        SBST::+
                                        SBST::/
                                        SBST::*)
                        (SBST::!NUMBER! EPSILON
                                        SBST::-
                                        SBST::+
                                        SBST::/
                                        SBST::*)
                        (SBST::|'| SBST::!ID!)
                        (SBST::|#| SBST::!ID!)
                        (SBST::!ID! EPSILON
                                    SBST::|(|
                                    SBST::-
                                    SBST::+
                                    SBST::/
                                    SBST::*))
                      (CODE-TO-SLOT V2 (!EXP! 0))
                      V1
                      V2
                      'SBST::|,|)
                    (LAM ((SBST::|)| EPSILON))
                         (GOBBLE-TOKEN))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ELIST
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T
             (INITIAL-ERROR
               '((SBST::|(| SBST::|)| SBST::|(| SBST::- SBST::AST SBST::[
                  SBST::IF SBST::!STRING! SBST::!NUMBER! SBST::|'| SBST::|#|
                  SBST::!ID!)))))
         NIL
         V0))


(DEFUN !EXP!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (LET (MTEMP
             V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7)
         (STACK-BRACKETS SBST::|(| EXP)
         (INITIALS
           ((LA-MATCH ((SBST::-)))
             (PROGN (LAM ((SBST::- EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V2 (!EXP! 40))
                    (VALUE-TO-SLOT V0 13)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::NEG
                                              (LIST V2)))))
           ((LA-MATCH ((SBST::|#|)))
             (PROGN (LAM ((SBST::|#| EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (LAM ((SBST::|#| EPSILON))
                         (GOBBLE-TOKEN))
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'VARIABLE
                                              (LIST V2)))))
           ((LA-MATCH ((SBST::|'|)))
             (PROGN (LAM ((SBST::|'| EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (VALUE-TO-SLOT V0 2)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'SYMBOL
                                              (LIST V2)))))
           ((LA-MATCH ((SBST::IF)))
             (PROGN (LAM ((SBST::IF EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V2 (!EXP! 10))
                    (LAM ((SBST::THEN EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V3 (!EXP! 0))
                    (LAM ((SBST::ELSE EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V4 (!EXP! 0))
                    (VALUE-TO-SLOT V0 5)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ITE
                                              (LIST V2 V3 V4)))))
           ((LA-MATCH ((SBST::[)))
             (PROGN (LAM ((SBST::[ EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V2 (!EXP! 0))
                    (LAM ((SBST::\| EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V3 (!EXP! 0))
                    (LAM ((SBST::] EPSILON))
                         (GOBBLE-TOKEN))
                    (OPT-PARSE ((SBST::|:| SBST::!NUMBER!
                                           SBST::!ID!))
                               (PROGN
                                 (LAM ((SBST::|:| EPSILON))
                                      (GOBBLE-TOKEN))
                                 (CODE-TO-SLOT V7 (!TAG! 0))
                                 (VALUE-TO-SLOT V6 V7))
                               V4)
                    (AS-TO-SLOT V5
                                (IF (= V4 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXP-NULL-1
                                      NIL)
                                    V6))
                    (VALUE-TO-SLOT V0 6)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::OPTSORT
                                              (LIST V2 V3 V5)))))
           ((LA-MATCH ((SBST::!ID! SBST::|(|)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (CODE-TO-SLOT V3 (!EXPLIST! 0))
                    (VALUE-TO-SLOT V0 7)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::FUNCT
                                              (LIST V2 V3)))))
           ((LA-MATCH ((SBST::!ID! EPSILON)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ATTRIBUTE
                                              (LIST V1)))))
           ((LA-MATCH ((SBST::!NUMBER!)))
             (PROGN (LAM ((SBST::!NUMBER! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (VALUE-TO-SLOT V0 3)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'NUMBER
                                              (LIST V1)))))
           ((LA-MATCH ((SBST::!STRING!)))
             (PROGN (LAM ((SBST::!STRING! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (VALUE-TO-SLOT V0 4)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'STRING
                                              (LIST V1)))))
           ((LA-MATCH ((SBST::AST)))
             (PROGN (LAM ((SBST::AST EPSILON))
                         (GOBBLE-TOKEN))
                    (VALUE-TO-SLOT V0 8)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ABSY
                                              (LIST)))))
           (T
             (INITIAL-ERROR
               '((SBST::AST EPSILON) (SBST::!ID! SBST::|(| EPSILON)
                 (SBST::[ SBST::!ID! SBST::|#| SBST::|'| SBST::!NUMBER!
                  SBST::!STRING! SBST::IF SBST::[ SBST::AST SBST::- SBST::|(|)
                 (SBST::IF SBST::!ID! SBST::|#| SBST::|'| SBST::!NUMBER!
                  SBST::!STRING! SBST::IF SBST::[ SBST::AST SBST::- SBST::|(|)
                 (SBST::!STRING! EPSILON) (SBST::!NUMBER! EPSILON)
                 (SBST::|'| SBST::!ID!) (SBST::|#| SBST::!ID!)
                 (SBST::- SBST::|(| SBST::- SBST::AST SBST::[ SBST::IF
                  SBST::!STRING! SBST::!NUMBER! SBST::|'| SBST::|#| SBST::!ID!)
                 (SBST::|(| SBST::|(| SBST::- SBST::AST SBST::[ SBST::IF
                  SBST::!STRING! SBST::!NUMBER! SBST::|'| SBST::|#| SBST::!ID!)))))
         (VALUE-TO-SLOT V0 V1)
         (EAT-BRACKETS (SBST::|)| SBST::|(|) EXP)
         (COND ((>
                  (COND ((MEMBER (PEEK-FIRST)
                                 '(SBST::* SBST::/ SBST::+ SBST::-))
                         (SETQ MTEMP (PEEK-FIRST))
                         (CADR
                           (ASSOC (PEEK-FIRST)
                                  '((SBST::/ 30) (SBST::* 30) (SBST::- 20)
                                    (SBST::+ 20)))))
                        (T 0))
                  RBP)
                (LOOP (CLEAN-VARIABLES (V1 V2 V3 V4 V5 V6 V7))
                      (CASE MTEMP
                            (SBST::-
                              (PROGN (SLOT-TO-SLOT V2 V0)
                                     (LAM ((SBST::- EPSILON))
                                          (GOBBLE-TOKEN))
                                     (CODE-TO-SLOT V3 (!EXP! 21))
                                     (VALUE-TO-SLOT V0 12)
                                     (AS-TO-SLOT V1
                                                 (MAKE-SB-TERM
                                                   'ANALYSIS-FACILITY::MINUS
                                                   (LIST V2 V3)))))
                            (SBST::+
                              (PROGN (SLOT-TO-SLOT V2 V0)
                                     (LAM ((SBST::+ EPSILON))
                                          (GOBBLE-TOKEN))
                                     (CODE-TO-SLOT V3 (!EXP! 21))
                                     (VALUE-TO-SLOT V0 11)
                                     (AS-TO-SLOT V1
                                                 (MAKE-SB-TERM
                                                   'ANALYSIS-FACILITY::ADD
                                                   (LIST V2 V3)))))
                            (SBST::/
                              (PROGN (SLOT-TO-SLOT V2 V0)
                                     (LAM ((SBST::/ EPSILON))
                                          (GOBBLE-TOKEN))
                                     (CODE-TO-SLOT V3 (!EXP! 31))
                                     (VALUE-TO-SLOT V0 10)
                                     (AS-TO-SLOT V1
                                                 (MAKE-SB-TERM
                                                   'ANALYSIS-FACILITY::DIV
                                                   (LIST V2 V3)))))
                            (SBST::*
                              (PROGN (SLOT-TO-SLOT V2 V0)
                                     (LAM ((SBST::* EPSILON))
                                          (GOBBLE-TOKEN))
                                     (OPT-PARSE
                                       ((SBST::|(| SBST::|(|
                                                   SBST::-
                                                   SBST::AST
                                                   SBST::[
                                                   SBST::IF
                                                   SBST::!STRING!
                                                   SBST::!NUMBER!
                                                   SBST::|'|
                                                   SBST::|#|
                                                   SBST::!ID!)
                                         (SBST::- SBST::|(|
                                                  SBST::-
                                                  SBST::AST
                                                  SBST::[
                                                  SBST::IF
                                                  SBST::!STRING!
                                                  SBST::!NUMBER!
                                                  SBST::|'|
                                                  SBST::|#|
                                                  SBST::!ID!)
                                         (SBST::AST EPSILON
                                                    SBST::-
                                                    SBST::+
                                                    SBST::/
                                                    SBST::*)
                                         (SBST::[ SBST::|(|
                                                  SBST::-
                                                  SBST::AST
                                                  SBST::[
                                                  SBST::IF
                                                  SBST::!STRING!
                                                  SBST::!NUMBER!
                                                  SBST::|'|
                                                  SBST::|#|
                                                  SBST::!ID!)
                                         (SBST::IF SBST::|(|
                                                   SBST::-
                                                   SBST::AST
                                                   SBST::[
                                                   SBST::IF
                                                   SBST::!STRING!
                                                   SBST::!NUMBER!
                                                   SBST::|'|
                                                   SBST::|#|
                                                   SBST::!ID!)
                                         (SBST::!STRING! EPSILON
                                                         SBST::-
                                                         SBST::+
                                                         SBST::/
                                                         SBST::*)
                                         (SBST::!NUMBER! EPSILON
                                                         SBST::-
                                                         SBST::+
                                                         SBST::/
                                                         SBST::*)
                                         (SBST::|'| SBST::!ID!)
                                         (SBST::|#| SBST::!ID!)
                                         (SBST::!ID! EPSILON
                                                     SBST::|(|
                                                     SBST::-
                                                     SBST::+
                                                     SBST::/
                                                     SBST::*))
                                       (CODE-TO-SLOT V4 (!EXP! 31))
                                       V3)
                                     (VALUE-TO-SLOT V0 9)
                                     (AS-TO-SLOT V1
                                                 (IF (= V3 0)
                                                     (MAKE-SB-TERM
                                                       'ANALYSIS-FACILITY::ATTRLIST
                                                       (LIST V2))
                                                     (MAKE-SB-TERM
                                                       'ANALYSIS-FACILITY::MULT
                                                       (LIST V2 V4))))))
                            (T
                              (MEDIAL-ERROR
                                '(SBST::* SBST::/ SBST::+ SBST::-))))
                      (VALUE-TO-SLOT V0 V1)
                      (EAT-BRACKETS (SBST::|)| SBST::|(|) EXP)
                      (WHEN
                        (<=
                          (COND ((MEMBER (PEEK-FIRST)
                                         '(SBST::* SBST::/ SBST::+ SBST::-))
                                 (SETQ MTEMP (PEEK-FIRST))
                                 (CADR
                                   (ASSOC (PEEK-FIRST)
                                          '((SBST::/ 30) (SBST::* 30)
                                            (SBST::- 20) (SBST::+ 20)))))
                                (T 0))
                          RBP)
                        (RETURN NIL)))))
         V0))


(DEFUN !CONSTRAINTS!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::WITH)))
             (PROGN (LAM ((SBST::WITH EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE
                      ((SBST::!ID! SBST::|(|
                                   SBST::-
                                   SBST::+
                                   SBST::/
                                   SBST::*
                                   SBST::ELSE)
                        (SBST::|#| SBST::!ID!)
                        (SBST::|'| SBST::!ID!)
                        (SBST::!NUMBER! SBST::-
                                        SBST::+
                                        SBST::/
                                        SBST::*
                                        SBST::ELSE)
                        (SBST::!STRING! SBST::-
                                        SBST::+
                                        SBST::/
                                        SBST::*
                                        SBST::ELSE)
                        (SBST::IF SBST::|(|
                                  SBST::-
                                  SBST::AST
                                  SBST::[
                                  SBST::IF
                                  SBST::!STRING!
                                  SBST::!NUMBER!
                                  SBST::|'|
                                  SBST::|#|
                                  SBST::!ID!)
                        (SBST::[ SBST::|(|
                                 SBST::-
                                 SBST::AST
                                 SBST::[
                                 SBST::IF
                                 SBST::!STRING!
                                 SBST::!NUMBER!
                                 SBST::|'|
                                 SBST::|#|
                                 SBST::!ID!)
                        (SBST::AST SBST::- SBST::+ SBST::/ SBST::* SBST::ELSE)
                        (SBST::- SBST::|(|
                                 SBST::-
                                 SBST::AST
                                 SBST::[
                                 SBST::IF
                                 SBST::!STRING!
                                 SBST::!NUMBER!
                                 SBST::|'|
                                 SBST::|#|
                                 SBST::!ID!)
                        (SBST::|(| SBST::|(|
                                   SBST::-
                                   SBST::AST
                                   SBST::[
                                   SBST::IF
                                   SBST::!STRING!
                                   SBST::!NUMBER!
                                   SBST::|'|
                                   SBST::|#|
                                   SBST::!ID!))
                      (PROGN (CODE-TO-SLOT V3 (!EXP! 0))
                             (LAM ((SBST::ELSE EPSILON))
                                  (GOBBLE-TOKEN))
                             (CODE-TO-SLOT V4 (!EXP! 0))
                             (AS-TO-SLOT V2
                                         (MAKE-SB-TERM
                                           'ANALYSIS-FACILITY::CONSTRAINT
                                           (LIST V3 V4))))
                      V1
                      V2
                      'SBST::AND)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::CONSTRLIST
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T
             (INITIAL-ERROR
               '((SBST::WITH SBST::|(| SBST::- SBST::AST SBST::[ SBST::IF
                  SBST::!STRING! SBST::!NUMBER! SBST::|'| SBST::|#| SBST::!ID!)))))
         NIL
         V0))


(DEFUN !DEFS!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::WHERE)))
             (PROGN (LAM ((SBST::WHERE EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE ((SBST::!ID! SBST::=))
                                      (PROGN
                                        (LAM ((SBST::!ID! EPSILON))
                                             (GOBBLE-TO-SLOT V3))
                                        (LAM ((SBST::= EPSILON))
                                             (GOBBLE-TOKEN))
                                        (CODE-TO-SLOT V4 (!EXP! 0))
                                        (AS-TO-SLOT V2
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::DEF
                                                      (LIST V3 V4))))
                                      V1
                                      V2
                                      'SBST::AND)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::DEFLIST
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T (INITIAL-ERROR '((SBST::WHERE SBST::!ID!)))))
         NIL
         V0))


(DEFUN !RHS-ATTR!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!)))
             (PROGN (LAM ((SBST::! EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (OPT-PARSE
                      ((SBST::* EPSILON SBST::/) (SBST::/ SBST::!ID!)
                                                 (SBST::|\\| SBST::!ID!))
                      (ALT-PARSE
                        ((LA-MATCH ((SBST::/))) (VALUE-TO-SLOT V4 1)
                                                (PROGN
                                                  (LAM ((SBST::/ EPSILON))
                                                       (GOBBLE-TOKEN))
                                                  (LAM ((SBST::!ID! EPSILON))
                                                       (GOBBLE-TO-SLOT V5))))
                        ((LA-MATCH ((SBST::|\\|))) (VALUE-TO-SLOT V4 2)
                                                   (PROGN
                                                     (LAM
                                                       ((SBST::|\\| EPSILON))
                                                       (GOBBLE-TOKEN))
                                                     (LAM
                                                       ((SBST::!ID! EPSILON))
                                                       (GOBBLE-TO-SLOT V5))
                                                     (OPT-PARSE
                                                       ((SBST::* EPSILON))
                                                       (LAM
                                                         ((SBST::* EPSILON))
                                                         (GOBBLE-TOKEN))
                                                       V6)))
                        ((LA-MATCH ((SBST::*))) (VALUE-TO-SLOT V4 0)
                                                (PROGN
                                                  (LAM ((SBST::* EPSILON))
                                                       (GOBBLE-TOKEN))
                                                  (OPT-PARSE
                                                    ((SBST::/ SBST::!ID!))
                                                    (PROGN
                                                      (LAM
                                                        ((SBST::/ EPSILON))
                                                        (GOBBLE-TOKEN))
                                                      (LAM
                                                        ((SBST::!ID! EPSILON))
                                                        (GOBBLE-TO-SLOT V6)))
                                                    V5)))
                        (T
                          (INITIAL-ERROR
                            '((SBST::/ SBST::!ID!) (SBST::|\\| SBST::!ID!)
                              (SBST::* EPSILON SBST::/)))))
                      V3)
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (IF (= V3 0)
                                    (MAKE-SB-TERM 'ANALYSIS-FACILITY::INHR-ATTR
                                                  (LIST V2))
                                    (CASE V4
                                          (2
                                            (IF (= V6 0)
                                                (MAKE-SB-TERM
                                                  'ANALYSIS-FACILITY::INHL-ATTR
                                                  (LIST V2 V5))
                                                (MAKE-SB-TERM
                                                  'ANALYSIS-FACILITY::INHL-ATTRL
                                                  (LIST V2 V5))))
                                          (1
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::INHF-ATTR
                                              (LIST V2 V5)))
                                          (0
                                            (IF (= V5 0)
                                                (MAKE-SB-TERM
                                                  'ANALYSIS-FACILITY::INHR-ATTRL
                                                  (LIST V2))
                                                (MAKE-SB-TERM
                                                  'ANALYSIS-FACILITY::INHF-ATTRL
                                                  (LIST V2 V6)))))))))
           ((LA-MATCH ((SBST::^)))
             (PROGN (LAM ((SBST::^ EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (OPT-PARSE
                      ((SBST::|\\| SBST::!ID!) (SBST::/ SBST::!ID!))
                      (PROGN
                        (ALT-PARSE
                          ((LA-MATCH ((SBST::/))) (VALUE-TO-SLOT V4 0)
                                                  (LAM ((SBST::/ EPSILON))
                                                       (GOBBLE-TOKEN)))
                          ((LA-MATCH ((SBST::|\\|))) (VALUE-TO-SLOT V4 1)
                                                     (LAM
                                                       ((SBST::|\\| EPSILON))
                                                       (GOBBLE-TOKEN)))
                          (T
                            (INITIAL-ERROR
                              '((SBST::/ EPSILON) (SBST::|\\| EPSILON)))))
                        (LAM ((SBST::!ID! EPSILON))
                             (GOBBLE-TO-SLOT V5)))
                      V3)
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (IF (= V3 0)
                                    (MAKE-SB-TERM 'ANALYSIS-FACILITY::SYNR-ATTR
                                                  (LIST V2))
                                    (CASE V4
                                          (1
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::SYNL-ATTR
                                              (LIST V2 V5)))
                                          (0
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::SYNF-ATTR
                                              (LIST V2 V5))))))))
           (T (INITIAL-ERROR '((SBST::^ SBST::!ID!) (SBST::! SBST::!ID!)))))
         (VALUE-TO-SLOT V0 V1)
         V0))


(DEFUN !CHILD-SORT-INSTANCE!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7
             V8)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID! SBST::|(|)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (LAM ((SBST::|(| EPSILON))
                         (GOBBLE-TOKEN))
                    (ALT-PARSE
                      ((LA-MATCH ((SBST::!) (SBST::^))) (VALUE-TO-SLOT V3 0)
                                                        (PROGN
                                                          (DOUBLEPLUS-PARSE
                                                            ((SBST::^
                                                               SBST::!ID!)
                                                              (SBST::!
                                                                SBST::!ID!))
                                                            (CODE-TO-SLOT V5
                                                                      (!RHS-ATTR!
                                                                      0))
                                                            V4
                                                            V5
                                                            'SBST::|,|)
                                                          (LAM
                                                            ((SBST::|)| EPSILON))
                                                            (GOBBLE-TOKEN))
                                                          (OPT-PARSE
                                                            ((SBST::* EPSILON))
                                                            (LAM
                                                              ((SBST::* EPSILON))
                                                              (GOBBLE-TOKEN))
                                                            V6)))
                      ((LA-MATCH
                         ((SBST::!ID!) (SBST::[)
                                       (SBST::_)
                                       (SBST::|)|)))
                        (VALUE-TO-SLOT V3 1)
                        (PROGN
                          (DOUBLESTAR-PARSE
                            ((SBST::_ EPSILON) (SBST::[ SBST::!ID!)
                                               (SBST::!ID! SBST::|:|
                                                           SBST::|(|))
                            (CODE-TO-SLOT V5
                                          (!CHILD-SORT! 0))
                            V4
                            V5
                            'SBST::|,|)
                          (LAM ((SBST::|)| EPSILON))
                               (GOBBLE-TOKEN))
                          (OPT-PARSE ((SBST::WHERE SBST::!ID!))
                                     (CODE-TO-SLOT V8
                                                   (!DEFS! 0))
                                     V6)
                          (AS-TO-SLOT V7
                                      (IF (= V6 0)
                                          (MAKE-SB-TERM
                                            'ANALYSIS-FACILITY::CHILD-SORT-INSTANCE-NULL-1
                                            NIL)
                                          V8))))
                      (T
                        (INITIAL-ERROR
                          '((SBST::! SBST::!ID!) (SBST::^ SBST::!ID!)
                            (SBST::|)| EPSILON SBST::WHERE)
                            (SBST::_ SBST::|,| SBST::|)|)
                            (SBST::[ SBST::!ID!)
                            (SBST::!ID! SBST::|:| SBST::|(|)))))
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (CASE V3
                                      (1
                                        (MAKE-SB-TERM
                                          'ANALYSIS-FACILITY::EMBEDDED-OP
                                          (LIST V2
                                                (MAKE-SB-TERM
                                                  'ANALYSIS-FACILITY::EMB-CSORTL
                                                  (DS-TEMP-REPT
                                                    (CK-REPT-REF V4)))
                                                V7)))
                                      (0
                                        (IF (= V6 0)
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::CONSTITUENT
                                              (LIST V2
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::CONST-ATTRL
                                                      (DS-TEMP-REPT
                                                        (CK-REPT-REF V4)))))
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::TLIST-CONSTITUENT
                                              (LIST V2
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::TLIST-ATTRL
                                                      (DS-TEMP-REPT
                                                        (CK-REPT-REF V4)))))))))))
           ((LA-MATCH ((SBST::!ID! SBST::|:|)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (LAM ((SBST::|:| EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V3))
                    (LAM ((SBST::|(| EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE
                      ((SBST::^ SBST::!ID!) (SBST::! SBST::!ID!))
                      (CODE-TO-SLOT V5 (!RHS-ATTR! 0))
                      V4
                      V5
                      'SBST::|,|)
                    (LAM ((SBST::|)| EPSILON))
                         (GOBBLE-TOKEN))
                    (OPT-PARSE ((SBST::* EPSILON))
                               (LAM ((SBST::* EPSILON))
                                    (GOBBLE-TOKEN))
                               V6)
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (IF (= V6 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-CONSTITUENT
                                      (LIST V2
                                            V3
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::ECONST-ATTRL
                                              (DS-TEMP-REPT (CK-REPT-REF V4)))))
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-TLIST-CONSTITUENT
                                      (LIST V2
                                            V3
                                            (MAKE-SB-TERM
                                              'ANALYSIS-FACILITY::ETLIST-ATTRL
                                              (DS-TEMP-REPT (CK-REPT-REF V4)))))))))
           (T (INITIAL-ERROR '((SBST::!ID! SBST::|:| SBST::|(|)))))
         (VALUE-TO-SLOT V0 V1)
         V0))


(DEFUN !CHILD-SORT!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID!)))
             (PROGN (CODE-TO-SLOT V1
                                  (!CHILD-SORT-INSTANCE! 0))
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM
                                  'ANALYSIS-FACILITY::REG-CONSTITUENT
                                  (LIST V1)))))
           ((LA-MATCH ((SBST::[)))
             (PROGN (LAM ((SBST::[ EPSILON))
                         (GOBBLE-TOKEN))
                    (CODE-TO-SLOT V2
                                  (!CHILD-SORT-INSTANCE! 0))
                    (LAM ((SBST::] EPSILON))
                         (GOBBLE-TOKEN))
                    (OPT-PARSE ((SBST::|:| SBST::!NUMBER!
                                           SBST::!ID!))
                               (PROGN
                                 (LAM ((SBST::|:| EPSILON))
                                      (GOBBLE-TOKEN))
                                 (CODE-TO-SLOT V6 (!TAG! 0))
                                 (VALUE-TO-SLOT V5 V6))
                               V3)
                    (AS-TO-SLOT V4
                                (IF (= V3 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::CHILD-SORT-NULL-1
                                      NIL)
                                    V5))
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM
                                  'ANALYSIS-FACILITY::OPT-CONSTITUENT
                                  (LIST V2 V4)))))
           ((LA-MATCH ((SBST::_)))
             (PROGN (LAM ((SBST::_ EPSILON))
                         (GOBBLE-TOKEN))
                    (VALUE-TO-SLOT V0 2)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM
                                  'ANALYSIS-FACILITY::NULL-CONSTITUENT
                                  (LIST)))))
           (T
             (INITIAL-ERROR
               '((SBST::_ EPSILON) (SBST::[ SBST::!ID!)
                 (SBST::!ID! SBST::|:| SBST::|(|)))))
         (VALUE-TO-SLOT V0 V1)
         V0))


(DEFUN !LHS-ATTR!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!)))
             (PROGN (LAM ((SBST::! EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (VALUE-TO-SLOT V0 0)
                    (AS-TO-SLOT V1
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::INH-ATTR
                                              (LIST V2)))))
           ((LA-MATCH ((SBST::^)))
             (PROGN (LAM ((SBST::^ EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V2))
                    (OPT-PARSE ((SBST::* EPSILON))
                               (LAM ((SBST::* EPSILON))
                                    (GOBBLE-TOKEN))
                               V3)
                    (VALUE-TO-SLOT V0 1)
                    (AS-TO-SLOT V1
                                (IF (= V3 0)
                                    (MAKE-SB-TERM 'ANALYSIS-FACILITY::SYN-ATTR
                                                  (LIST V2))
                                    (MAKE-SB-TERM 'ANALYSIS-FACILITY::SYN-ATTRL
                                                  (LIST V2))))))
           (T (INITIAL-ERROR '((SBST::^ SBST::!ID!) (SBST::! SBST::!ID!)))))
         (VALUE-TO-SLOT V0 V1)
         V0))


(DEFUN !ALTERNATIVE!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7
             V8
             V9
             V10
             V11
             V12)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID!)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (LAM ((SBST::|(| EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLESTAR-PARSE
                      ((SBST::_ EPSILON) (SBST::[ SBST::!ID!)
                                         (SBST::!ID! SBST::|:|
                                                     SBST::|(|))
                      (CODE-TO-SLOT V3
                                    (!CHILD-SORT! 0))
                      V2
                      V3
                      'SBST::|,|)
                    (LAM ((SBST::|)| EPSILON))
                         (GOBBLE-TOKEN))
                    (OPT-PARSE
                      ((SBST::WITH SBST::|(|
                                   SBST::-
                                   SBST::AST
                                   SBST::[
                                   SBST::IF
                                   SBST::!STRING!
                                   SBST::!NUMBER!
                                   SBST::|'|
                                   SBST::|#|
                                   SBST::!ID!))
                      (CODE-TO-SLOT V6
                                    (!CONSTRAINTS! 0))
                      V4)
                    (AS-TO-SLOT V5
                                (IF (= V4 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::ALTERNATIVE-NULL-1
                                      NIL)
                                    V6))
                    (OPT-PARSE ((SBST::WHERE SBST::!ID!))
                               (CODE-TO-SLOT V9 (!DEFS! 0))
                               V7)
                    (AS-TO-SLOT V8
                                (IF (= V7 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::ALTERNATIVE-NULL-2
                                      NIL)
                                    V9))
                    (OPT-PARSE ((SBST::ITERATE SBST::!ID! SBST::!ID!))
                               (CODE-TO-SLOT V12 (!ITERS! 0))
                               V10)
                    (AS-TO-SLOT V11
                                (IF (= V10 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::ALTERNATIVE-NULL-3
                                      NIL)
                                    V12))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ALTERNATIVE
                                              (LIST V1
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::ALT-CSORTL
                                                      (DS-TEMP-REPT
                                                        (CK-REPT-REF V2)))
                                                    V5
                                                    V8
                                                    V11)))))
           (T (INITIAL-ERROR '((SBST::!ID! SBST::|(|)))))
         NIL
         V0))


(DEFUN !RULE!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7
             V8)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID!)))
             (PROGN (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (LAM ((SBST::|(| EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLESTAR-PARSE
                      ((SBST::^ SBST::!ID!) (SBST::! SBST::!ID!))
                      (CODE-TO-SLOT V3 (!LHS-ATTR! 0))
                      V2
                      V3
                      'SBST::|,|)
                    (LAM ((SBST::|)| EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::= EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE ((SBST::!ID! SBST::|(|))
                                      (CODE-TO-SLOT V5
                                                    (!ALTERNATIVE! 0))
                                      V4
                                      V5
                                      'SBST::\|)
                    (LAM ((SBST::|;| EPSILON))
                         (GOBBLE-TOKEN))
                    (OPT-PARSE ((SBST::FINAL SBST::!ID!))
                               (CODE-TO-SLOT V8
                                             (!DECLARATION! 0))
                               V6)
                    (AS-TO-SLOT V7
                                (IF (= V6 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::RULE-NULL-1
                                      NIL)
                                    V8))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::NT-RULE
                                              (LIST V1
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::RULE-ATTRL
                                                      (DS-TEMP-REPT
                                                        (CK-REPT-REF V2)))
                                                    (MAKE-SB-TERM
                                                      'ANALYSIS-FACILITY::RULE-ALTL
                                                      (DS-TEMP-REPT
                                                        (CK-REPT-REF V4)))
                                                    V7)))))
           (T (INITIAL-ERROR '((SBST::!ID! SBST::|(|)))))
         NIL
         V0))


(DEFUN !RULES!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::!ID!)))
             (PROGN (CODE-TO-SLOT V2 (!RULE! 0))
                    (LAM ((SBST::|;| EPSILON))
                         (GOBBLE-TOKEN))
                    (VALUE-TO-SLOT V1 V2)
                    (CONS-TO-SLOT V0 V1)
                    (GEN-STAR-PARSE ((SBST::!ID! SBST::|(|))
                                    (PROGN (CODE-TO-SLOT V2
                                                         (!RULE! 0))
                                           (LAM ((SBST::|;| EPSILON))
                                                (GOBBLE-TOKEN))
                                           (VALUE-TO-SLOT V1 V2))
                                    V0
                                    V1)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::RULELIST
                                              (DS-TEMP-REPT (CK-REPT-REF V0))))))
           (T (INITIAL-ERROR '((SBST::!ID! SBST::|(|)))))
         NIL
         V0))


(DEFUN !INTRINSIC-TERMS!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::TERMINALS)))
             (PROGN (LAM ((SBST::TERMINALS EPSILON))
                         (GOBBLE-TOKEN))
                    (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON))
                                      (LAM ((SBST::!ID! EPSILON))
                                           (GOBBLE-TO-SLOT V2))
                                      V1
                                      V2
                                      'SBST::|,|)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::ITERMS
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T (INITIAL-ERROR '((SBST::TERMINALS SBST::!ID!)))))
         NIL
         V0))


(DEFUN !EXT-GRAM!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7
             V8
             V9)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::PASS)))
             (PROGN (LAM ((SBST::PASS EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V5))
                    (OPT-PARSE ((SBST::PACKAGE SBST::!ID!))
                               (PROGN
                                 (LAM ((SBST::PACKAGE EPSILON))
                                      (GOBBLE-TOKEN))
                                 (LAM ((SBST::!ID! EPSILON))
                                      (GOBBLE-TO-SLOT V9))
                                 (VALUE-TO-SLOT V8 V9))
                               V6)
                    (AS-TO-SLOT V7
                                (IF (= V6 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-GRAM-NULL-2
                                      NIL)
                                    V8))
                    (VALUE-TO-SLOT V1 0)
                    (AS-TO-SLOT V2
                                (IF (= V1 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-GRAM-NULL-1
                                      NIL)
                                    V3))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::EXT-GRAM
                                              (LIST V2 V5 V7)))))
           ((LA-MATCH ((SBST::GRAMMAR)))
             (PROGN (LAM ((SBST::GRAMMAR EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V4))
                    (VALUE-TO-SLOT V1 1)
                    (VALUE-TO-SLOT V3 V4)
                    (AS-TO-SLOT V2
                                (IF (= V1 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-GRAM-NULL-1
                                      NIL)
                                    V3))
                    (LAM ((SBST::PASS EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V5))
                    (OPT-PARSE ((SBST::PACKAGE SBST::!ID!))
                               (PROGN
                                 (LAM ((SBST::PACKAGE EPSILON))
                                      (GOBBLE-TOKEN))
                                 (LAM ((SBST::!ID! EPSILON))
                                      (GOBBLE-TO-SLOT V9))
                                 (VALUE-TO-SLOT V8 V9))
                               V6)
                    (AS-TO-SLOT V7
                                (IF (= V6 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::EXT-GRAM-NULL-2
                                      NIL)
                                    V8))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::EXT-GRAM
                                              (LIST V2 V5 V7)))))
           (T
             (INITIAL-ERROR
               '((SBST::GRAMMAR SBST::!ID!) (SBST::PASS SBST::!ID!)))))
         NIL
         V0))


(DEFUN !EXT-GRAMMARS!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::EXTERNAL)))
             (PROGN (LAM ((SBST::EXTERNAL EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::GRAMMARS EPSILON))
                         (GOBBLE-TOKEN))
                    (PLUS-PARSE
                      ((SBST::PASS SBST::!ID!) (SBST::GRAMMAR SBST::!ID!))
                      (CODE-TO-SLOT V2 (!EXT-GRAM! 0))
                      V1
                      V2)
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::EXT-GRAMLIST
                                              (DS-TEMP-REPT (CK-REPT-REF V1))))))
           (T (INITIAL-ERROR '((SBST::EXTERNAL SBST::GRAMMARS)))))
         NIL
         V0))


(DEFUN !AGG!
       (RBP &OPTIONAL
            (BRACKET-LIST (EMPTY-BRACKET-LIST)))
       (DECLARE (IGNORE RBP))
       (DECLARE (IGNORE BRACKET-LIST))
       (LET (V0
             V1
             V2
             V3
             V4
             V5
             V6
             V7
             V8
             V9
             V10
             V11
             V12)
         (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12)
         (INITIALS-ONLY
           ((LA-MATCH ((SBST::GRAMMAR)))
             (PROGN (LAM ((SBST::GRAMMAR EPSILON))
                         (GOBBLE-TOKEN))
                    (LAM ((SBST::!ID! EPSILON))
                         (GOBBLE-TO-SLOT V1))
                    (OPT-PARSE ((SBST::PASS SBST::!ID!))
                               (PROGN
                                 (LAM ((SBST::PASS EPSILON))
                                      (GOBBLE-TOKEN))
                                 (LAM ((SBST::!ID! EPSILON))
                                      (GOBBLE-TO-SLOT V5))
                                 (VALUE-TO-SLOT V4 V5))
                               V2)
                    (AS-TO-SLOT V3
                                (IF (= V2 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::AGG-NULL-1
                                      NIL)
                                    V4))
                    (OPT-PARSE ((SBST::EXTERNAL SBST::GRAMMARS))
                               (CODE-TO-SLOT V8
                                             (!EXT-GRAMMARS! 0))
                               V6)
                    (AS-TO-SLOT V7
                                (IF (= V6 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::AGG-NULL-2
                                      NIL)
                                    V8))
                    (OPT-PARSE ((SBST::TERMINALS SBST::!ID!))
                               (CODE-TO-SLOT V11
                                             (!INTRINSIC-TERMS! 0))
                               V9)
                    (AS-TO-SLOT V10
                                (IF (= V9 0)
                                    (MAKE-SB-TERM
                                      'ANALYSIS-FACILITY::AGG-NULL-3
                                      NIL)
                                    V11))
                    (CODE-TO-SLOT V12 (!RULES! 0))
                    (AS-TO-SLOT V0
                                (MAKE-SB-TERM 'ANALYSIS-FACILITY::AGG
                                              (LIST V1 V3 V7 V10 V12)))))
           (T (INITIAL-ERROR '((SBST::GRAMMAR SBST::!ID!)))))
         NIL
         V0))

