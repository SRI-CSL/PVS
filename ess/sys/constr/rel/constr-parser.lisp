;;; -*- Mode: Lisp; Package: CONSTRG -*-
(in-package "CONSTR-TERM-REP")  ;; creates package for abstract syntax. 

(in-package "CONSTRG")  ;; enters package for generated code.  

(use-package '("NEWATTR" "LANG" "SB-RUNTIME" "SORT" "TERM" "OCC" "OPER"
               "ERGOLISP"))


(export '( CONSTR-PARSE ))

(DEFPARAMETER CONSTR-ABS-SYN-PACKAGE (FIND-PACKAGE "CONSTR-TERM-REP"))

(DEFUN CONSTR-PARSE
    (&KEY (NT 'CONSTR-TERM-REP::ITEMS) ERROR-THRESHOLD ASK-ABOUT-BAD-TOKENS
     (RETURN-ERRORS NIL) (STREAM NIL STREAMP) STRING FILE
     (EXHAUST-STREAM NIL))
  (COND (STREAM)
        (STRING (SETF STREAM (MAKE-STRING-INPUT-STREAM STRING)))
        (FILE (SETF STREAM (OPEN FILE)))
        (T
         (ERROR "Must provide an input means -- either :stream, ~
			:string, or :file.")))
  (SETQ *CONSTR-KEYWORD-TABLE* (INIT-KEYWORD-TABLE CONSTR-KEYWORD-LIST))
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
        (*ABS-SYN-PACKAGE* CONSTR-ABS-SYN-PACKAGE)
        (*READER-FUN* #'READER)
        (*APPLY-LEX-TERM-CONSTR-FUN* #'APPLY-LEXICAL-TERMINAL-CONSTRUCTOR)
        (*KEYWORD-TABLE* *CONSTR-KEYWORD-TABLE*)
        (*CLOSE-COMMENT-CHAR* CONSTR-CLOSE-COMMENT-CHAR)
        (*CASE-SENSITIVE* CONSTR-CASE-SENSITIVE))
    (INIT-LEXER *LEXICAL-STREAM*)
    (MULTIPLE-VALUE-BIND (ABS-SYNTAX ERROR-STRING ERROR-ARGS ERROR-PLACE)
        (UNWIND-PROTECT
            (CATCH 'PARSER-ABORT-CATCH
              (PROG1 (CASE NT
                       (CONSTR-TERM-REP::ITEMS (!ITEMS! 0))
                       (CONSTR-TERM-REP::ITEM (!ITEM! 0))
                       (CONSTR-TERM-REP::ABBREV (!ABBREV! 0))
                       (CONSTR-TERM-REP::DATATYPE (!DATATYPE! 0))
                       (CONSTR-TERM-REP::DTARM (!DTARM! 0))
                       (CONSTR-TERM-REP::PART (!PART! 0))
                       (CONSTR-TERM-REP::PTYPE (!PTYPE! 0))
                       (T (ERROR "Unknown nonterminal ~A." NT)))
                     (WHEN (OR FILE STRING EXHAUST-STREAM)
                       (CLET* (((TOKEN IGNORE PLACE) (PEEK-FIRST)))
                              (UNLESS (OR (EQ TOKEN :EOF) (EQ TOKEN 'EOF))
                                (DO-SYNTAX-ERROR "There is garbage at the end of your ~
			      file or string:~%~A" PLACE))))))
          (UNLESS STREAMP (CLOSE STREAM)))
      (IF RETURN-ERRORS
          (VALUES ABS-SYNTAX *PARSER-ERROR* ERROR-PLACE ERROR-STRING
                  ERROR-ARGS)
        (VALUES ABS-SYNTAX *PARSER-ERROR*)))))


(DEFUN !PTYPE! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (LET (MTEMP V0 V1 V2 V3)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3)
    (STACK-BRACKETS SBST::|(| PTYPE)
    (INITIALS ((LA-MATCH ((SBST::|'|)))
               (PROGN (LAM ((SBST::|'| EPSILON)) (GOBBLE-TOKEN))
                      (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V2))
                      (VALUE-TO-SLOT V0 11)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-TV
                                                (LIST V2)))))
              ((LA-MATCH ((SBST::{ SBST::})))
               (PROGN (LAM ((SBST::{ EPSILON)) (GOBBLE-TOKEN))
                      (LAM ((SBST::} EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 9)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-MEMBERS
                                                (DS-TEMP-REPT
                                                 (CK-REPT-REF V2))))))
              ((LA-MATCH ((SBST::{ SBST::NOT SBST::{ SBST::UNIT SBST::|'|
                           SBST::!ID! SBST::|(|)))
               (PROGN (LAM ((SBST::{ EPSILON)) (GOBBLE-TOKEN))
                      (CODE-TO-SLOT V3 (!PTYPE! 0)) (CONS-TO-SLOT V2 V3)
                      (GEN-STAR-PARSE ((SBST::|,|
                                        SBST::|(|
                                        SBST::!ID!
                                        SBST::|'|
                                        SBST::UNIT
                                        SBST::{
                                        SBST::NOT))
                                      (PROGN
                                       (LAM
                                        ((SBST::|,| EPSILON))
                                        (GOBBLE-TOKEN))
                                       (CODE-TO-SLOT V3 (!PTYPE! 0)))
                                      V2 V3)
                      (LAM ((SBST::} EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 9)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-MEMBERS
                                                (DS-TEMP-REPT
                                                 (CK-REPT-REF V2))))))
              ((LA-MATCH ((SBST::NOT)))
               (PROGN (LAM ((SBST::NOT EPSILON)) (GOBBLE-TOKEN))
                      (CODE-TO-SLOT V2 (!PTYPE! 50)) (VALUE-TO-SLOT V0 6)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-APPL
                                                (LIST
                                                 (MAKE-SB-TERM
                                                  'CONSTR-TERM-REP::PTYPE-CON
                                                  (LIST (MK-ID 'NOT)))
                                                 V2)))))
              ((LA-MATCH ((SBST::!ID!)))
               (PROGN (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
                      (VALUE-TO-SLOT V0 12)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-CON
                                                (LIST V1)))))
              ((LA-MATCH ((SBST::UNIT)))
               (PROGN (LAM ((SBST::UNIT EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 10)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'CONSTR-TERM-REP::PTYPE-CON
                                                (LIST
                                                 (MK-ID
                                                  'CONSTR-TERM-REP::UNIT))))))
              (T
               (INITIAL-ERROR '((SBST::NOT SBST::|(| SBST::!ID! SBST::|'|
                                 SBST::UNIT SBST::{ SBST::NOT)
                                (SBST::{ SBST::NOT SBST::{ SBST::UNIT
                                 SBST::|'| SBST::!ID! SBST::|(| SBST::})
                                (SBST::UNIT EPSILON) (SBST::|'| SBST::!ID!)
                                (SBST::!ID! EPSILON)
                                (SBST::|(| SBST::|(| SBST::!ID! SBST::|'|
                                 SBST::UNIT SBST::{ SBST::NOT)))))
    (VALUE-TO-SLOT V0 V1)
    (EAT-BRACKETS (SBST::|)| SBST::|(|) PTYPE)
    (COND ((> (COND ((MEMBER (PEEK-FIRST)
                             '(SBST::* SBST::? SBST::-> SBST::|#| SBST::AND
                               SBST::OR SBST::SATS SBST::NOT-SATS))
                     (SETQ MTEMP (PEEK-FIRST))
                     (CADR (ASSOC (PEEK-FIRST)
                                  '((SBST::NOT-SATS 80) (SBST::SATS 80)
                                    (SBST::? 70) (SBST::* 70) (SBST::JUX 60)
                                    (SBST::AND 41) (SBST::OR 31)
                                    (SBST::|#| 21) (SBST::-> 11)))))
                    ((MEMBER (PEEK-FIRST)
                             '(SBST::|(| SBST::!ID! SBST::|'| SBST::UNIT
                               SBST::{ SBST::NOT))
                     (SETQ MTEMP 'G45180)
                     60)
                    (T 0))
              RBP)
           (LOOP (CLEAN-VARIABLES (V1 V2 V3))
                 (CASE MTEMP
                   (G45180
                    (PROGN (SLOT-TO-SLOT V2 V0) (CODE-TO-SLOT V3 (!PTYPE! 61))
                           (VALUE-TO-SLOT V0 13)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST V2 V3)))))
                   (SBST::NOT-SATS
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::NOT-SATS EPSILON)) (GOBBLE-TOKEN))
                           (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
                           (VALUE-TO-SLOT V0 8)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'AND)))
                                           V2))
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'NOT)))
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-APPL
                                            (LIST
                                             (MAKE-SB-TERM
                                              'CONSTR-TERM-REP::PTYPE-CON
                                              (LIST (MK-ID 'SATISFIES)))
                                             (MAKE-SB-TERM
                                              'CONSTR-TERM-REP::PTYPE-CON
                                              (LIST V3)))))))))))
                   (SBST::SATS
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::SATS EPSILON)) (GOBBLE-TOKEN))
                           (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
                           (VALUE-TO-SLOT V0 7)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'AND)))
                                           V2))
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'SATISFIES)))
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST V3)))))))))
                   (SBST::OR
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::OR EPSILON)) (GOBBLE-TOKEN))
                           (CODE-TO-SLOT V3 (!PTYPE! 30)) (VALUE-TO-SLOT V0 5)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'OR)))
                                           V2))
                                         V3)))))
                   (SBST::AND
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::AND EPSILON)) (GOBBLE-TOKEN))
                           (CODE-TO-SLOT V3 (!PTYPE! 40)) (VALUE-TO-SLOT V0 4)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST (MK-ID 'AND)))
                                           V2))
                                         V3)))))
                   (SBST::|#|
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::|#| EPSILON)) (GOBBLE-TOKEN))
                           (CODE-TO-SLOT V3 (!PTYPE! 20)) (VALUE-TO-SLOT V0 3)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST
                                             (MK-ID 'CONSTR-TERM-REP::PROD)))
                                           V2))
                                         V3)))))
                   (SBST::->
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::-> EPSILON)) (GOBBLE-TOKEN))
                           (CODE-TO-SLOT V3 (!PTYPE! 10)) (VALUE-TO-SLOT V0 2)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-APPL
                                          (LIST
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::PTYPE-CON
                                            (LIST
                                             (MK-ID 'CONSTR-TERM-REP::ARROW)))
                                           V2))
                                         V3)))))
                   (SBST::?
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::? EPSILON)) (GOBBLE-TOKEN))
                           (VALUE-TO-SLOT V0 1)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-CON
                                          (LIST (MK-ID 'OPTIONAL)))
                                         V2)))))
                   (SBST::*
                    (PROGN (SLOT-TO-SLOT V2 V0)
                           (LAM ((SBST::* EPSILON)) (GOBBLE-TOKEN))
                           (VALUE-TO-SLOT V0 0)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PTYPE-APPL
                                        (LIST
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::PTYPE-CON
                                          (LIST (MK-ID 'LIST-OF)))
                                         V2)))))
                   (T
                    (MEDIAL-ERROR '(SBST::* SBST::? SBST::-> SBST::|#|
                                    SBST::AND SBST::OR SBST::SATS
                                    SBST::NOT-SATS SBST::|(| SBST::!ID!
                                    SBST::|'| SBST::UNIT SBST::{ SBST::NOT))))
                 (VALUE-TO-SLOT V0 V1)
                 (EAT-BRACKETS (SBST::|)| SBST::|(|) PTYPE)
                 (WHEN (<= (COND ((MEMBER (PEEK-FIRST)
                                          '(SBST::*
                                            SBST::?
                                            SBST::->
                                            SBST::|#|
                                            SBST::AND
                                            SBST::OR
                                            SBST::SATS
                                            SBST::NOT-SATS))
                                  (SETQ MTEMP (PEEK-FIRST))
                                  (CADR (ASSOC

                                         (PEEK-FIRST)
                                         '((SBST::NOT-SATS 80)
                                           (SBST::SATS 80)
                                           (SBST::? 70)
                                           (SBST::* 70)
                                           (SBST::JUX 60)
                                           (SBST::AND 41)
                                           (SBST::OR 31)
                                           (SBST::|#| 21)
                                           (SBST::-> 11)))))
                                 ((MEMBER (PEEK-FIRST)
                                          '(SBST::|(|
                                            SBST::!ID!
                                            SBST::|'|
                                            SBST::UNIT
                                            SBST::{
                                            SBST::NOT))
                                  (SETQ MTEMP 'G45180)
                                  60)
                                 (T 0))
                           RBP)
                   (RETURN NIL)))))
    V0))


(DEFUN !PART! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1 V2 V3)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3)
    (INITIALS-ONLY ((LA-MATCH ((SBST::!ID! SBST::|:|)))
                    (PROGN (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V2))
                           (LAM ((SBST::|:| EPSILON)) (GOBBLE-TOKEN))
                           (VALUE-TO-SLOT V1 1) (CODE-TO-SLOT V3 (!PTYPE! 0))
                           (AS-TO-SLOT V0
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PART
                                        (LIST
                                         (IF (= V1 0) (MK-ID 'NIL) V2)
                                         V3)))))
                   ((LA-MATCH ((SBST::NOT) (SBST::{) (SBST::UNIT) (SBST::|'|)
                               (SBST::!ID! EPSILON SBST::* SBST::? SBST::->
                                SBST::|#| SBST::AND SBST::OR SBST::SATS
                                SBST::NOT-SATS SBST::|(| SBST::!ID! SBST::|'|
                                SBST::UNIT SBST::{ SBST::NOT)
                               (SBST::|(|)))
                    (PROGN (CODE-TO-SLOT V3 (!PTYPE! 0)) (VALUE-TO-SLOT V1 0)
                           (AS-TO-SLOT V0
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::PART
                                        (LIST
                                         (IF (= V1 0) (MK-ID 'NIL) V2)
                                         V3)))))
                   (T
                    (INITIAL-ERROR '((SBST::!ID! SBST::|:| EPSILON SBST::*
                                      SBST::? SBST::-> SBST::|#| SBST::AND
                                      SBST::OR SBST::SATS SBST::NOT-SATS
                                      SBST::|(| SBST::!ID! SBST::|'|
                                      SBST::UNIT SBST::{ SBST::NOT)
                                     (SBST::|(| SBST::|(| SBST::!ID! SBST::|'|
                                      SBST::UNIT SBST::{ SBST::NOT)
                                     (SBST::|'| SBST::!ID!)
                                     (SBST::UNIT EPSILON SBST::* SBST::?
                                      SBST::-> SBST::|#| SBST::AND SBST::OR
                                      SBST::SATS SBST::NOT-SATS SBST::|(|
                                      SBST::!ID! SBST::|'| SBST::UNIT SBST::{
                                      SBST::NOT)
                                     (SBST::{ SBST::} SBST::|(| SBST::!ID!
                                      SBST::|'| SBST::UNIT SBST::{ SBST::NOT)
                                     (SBST::NOT SBST::|(| SBST::!ID! SBST::|'|
                                      SBST::UNIT SBST::{ SBST::NOT)))))
    NIL
    V0))


(DEFUN !DTARM! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1 V2 V3 V4)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
    (INITIALS-ONLY ((LA-MATCH ((SBST::!ID!)))
                    (PROGN (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
                           (OPT-PARSE ((SBST::OF
                                        EPSILON
                                        SBST::!ID!
                                        SBST::NOT
                                        SBST::{
                                        SBST::UNIT
                                        SBST::|'|
                                        SBST::|(|))
                                      (PROGN
                                       (LAM
                                        ((SBST::OF EPSILON))
                                        (GOBBLE-TOKEN))
                                       (DOUBLESTAR-PARSE
                                        ((SBST::|(|
                                          SBST::|(|
                                          SBST::!ID!
                                          SBST::|'|
                                          SBST::UNIT
                                          SBST::{
                                          SBST::NOT)
                                         (SBST::|'| SBST::!ID!)
                                         (SBST::UNIT
                                          EPSILON
                                          SBST::*
                                          SBST::?
                                          SBST::->
                                          SBST::|#|
                                          SBST::AND
                                          SBST::OR
                                          SBST::SATS
                                          SBST::NOT-SATS
                                          SBST::|(|
                                          SBST::!ID!
                                          SBST::|'|
                                          SBST::UNIT
                                          SBST::{
                                          SBST::NOT)
                                         (SBST::{
                                          SBST::}
                                          SBST::|(|
                                          SBST::!ID!
                                          SBST::|'|
                                          SBST::UNIT
                                          SBST::{
                                          SBST::NOT)
                                         (SBST::NOT
                                          SBST::|(|
                                          SBST::!ID!
                                          SBST::|'|
                                          SBST::UNIT
                                          SBST::{
                                          SBST::NOT)
                                         (SBST::!ID!
                                          SBST::|:|
                                          EPSILON
                                          SBST::*
                                          SBST::?
                                          SBST::->
                                          SBST::|#|
                                          SBST::AND
                                          SBST::OR
                                          SBST::SATS
                                          SBST::NOT-SATS
                                          SBST::|(|
                                          SBST::!ID!
                                          SBST::|'|
                                          SBST::UNIT
                                          SBST::{
                                          SBST::NOT))
                                        (PROGN
                                         (CODE-TO-SLOT V4 (!PART! 0))
                                         NIL)
                                        V3
                                        V4
                                        'SBST::|,|))
                                      V2)
                           (AS-TO-SLOT V0
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::DTARM
                                        (LIST
                                         V1
                                         (IF
                                          (= V2 0)
                                          (MAKE-SB-TERM
                                           'CONSTR-TERM-REP::PARTS
                                           NIL)
                                          (MAKE-SB-TERM
                                           'CONSTR-TERM-REP::PARTS
                                           (DS-TEMP-REPT
                                            (CK-REPT-REF V3)))))))))
                   (T (INITIAL-ERROR '((SBST::!ID! EPSILON SBST::OF)))))
    NIL
    V0))


(DEFUN !DATATYPE! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1 V2 V3 V4)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
    (INITIALS-ONLY ((LA-MATCH ((SBST::DATATYPE)))
                    (PROGN (LAM ((SBST::DATATYPE EPSILON)) (GOBBLE-TOKEN))
                           (ALT-PARSE ((LA-MATCH ((SBST::!ID! SBST::=)))
                                       (VALUE-TO-SLOT V1 0)
                                       (PROGN
                                        (LAM
                                         ((SBST::!ID! EPSILON))
                                         (GOBBLE-TO-SLOT V2))
                                        (LAM
                                         ((SBST::= EPSILON))
                                         (GOBBLE-TOKEN))
                                        (DOUBLESTAR-PARSE
                                         ((SBST::!ID! SBST::OF EPSILON))
                                         (PROGN
                                          (CODE-TO-SLOT V4 (!DTARM! 0))
                                          NIL)
                                         V3
                                         V4
                                         'SBST::|\||)))
                                      ((LA-MATCH
                                        ((SBST::!ID! SBST::OF EPSILON)))
                                       (VALUE-TO-SLOT V1 1)
                                       (CODE-TO-SLOT V2 (!DTARM! 0)))
                                      (T
                                       (INITIAL-ERROR
                                        '((SBST::!ID!
                                           SBST::=
                                           SBST::OF
                                           EPSILON)))))
                           (AS-TO-SLOT V0
                                       (CASE
                                        V1
                                        (1
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::CONSTR
                                          (LIST V2)))
                                        (0
                                         (MAKE-SB-TERM
                                          'CONSTR-TERM-REP::MCONSTR
                                          (LIST
                                           V2
                                           (MAKE-SB-TERM
                                            'CONSTR-TERM-REP::CONSTRS
                                            (DS-TEMP-REPT
                                             (CK-REPT-REF V3))))))))))
                   (T (INITIAL-ERROR '((SBST::DATATYPE SBST::!ID!)))))
    NIL
    V0))


(DEFUN !ABBREV! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1 V2 V3)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3)
    (INITIALS-ONLY ((LA-MATCH ((SBST::ABBREV)))
                    (PROGN (LAM ((SBST::ABBREV EPSILON)) (GOBBLE-TOKEN))
                           (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
                           (ALT-PARSE ((LA-MATCH ((SBST::==)))
                                       (VALUE-TO-SLOT V2 0)
                                       (LAM
                                        ((SBST::== EPSILON))
                                        (GOBBLE-TOKEN)))
                                      ((LA-MATCH ((SBST::=)))
                                       (VALUE-TO-SLOT V2 1)
                                       (LAM
                                        ((SBST::= EPSILON))
                                        (GOBBLE-TOKEN)))
                                      (T
                                       (INITIAL-ERROR
                                        '((SBST::== EPSILON)
                                          (SBST::= EPSILON)))))
                           (CODE-TO-SLOT V3 (!PTYPE! 0))
                           (AS-TO-SLOT V0
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::ABBREV
                                        (LIST V1 V3)))))
                   (T (INITIAL-ERROR '((SBST::ABBREV SBST::!ID!)))))
    NIL
    V0))


(DEFUN !ITEM! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (INITIALS-ONLY ((LA-MATCH ((SBST::ABBREV)))
                    (PROGN (CODE-TO-SLOT V1 (!ABBREV! 0)) (VALUE-TO-SLOT V0 0)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::ABBREV-ITEM
                                        (LIST V1)))))
                   ((LA-MATCH ((SBST::DATATYPE)))
                    (PROGN (CODE-TO-SLOT V1 (!DATATYPE! 0))
                           (VALUE-TO-SLOT V0 1)
                           (AS-TO-SLOT V1
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::MCONSTR-ITEM
                                        (LIST V1)))))
                   (T
                    (INITIAL-ERROR '((SBST::DATATYPE SBST::!ID! SBST::!ID!)
                                     (SBST::ABBREV SBST::!ID!)))))
    (VALUE-TO-SLOT V0 V1)
    V0))


(DEFUN !ITEMS! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0 V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (INITIALS-ONLY ((LA-MATCH ((SBST::DATATYPE) (SBST::ABBREV)))
                    (PROGN (CODE-TO-SLOT V1 (!ITEM! 0)) NIL
                           (CONS-TO-SLOT V0 V1)
                           (GEN-STAR-PARSE ((SBST::DATATYPE
                                             SBST::!ID!
                                             SBST::!ID!)
                                            (SBST::ABBREV SBST::!ID!))
                                           (PROGN
                                            (CODE-TO-SLOT V1 (!ITEM! 0))
                                            NIL)
                                           V0 V1)
                           (AS-TO-SLOT V0
                                       (MAKE-SB-TERM
                                        'CONSTR-TERM-REP::ITEMS
                                        (DS-TEMP-REPT (CK-REPT-REF V0))))))
                   (T
                    (INITIAL-ERROR '((SBST::ABBREV SBST::!ID!)
                                     (SBST::DATATYPE SBST::!ID!
                                      SBST::!ID!)))))
    NIL
    V0))

