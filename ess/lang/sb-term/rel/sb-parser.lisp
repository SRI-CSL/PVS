;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package "SYNTAX-BOX")  ;; creates package for abstract syntax. 

(in-package "SYNTAX-BOX")  ;; enters package for generated code.  

(use-package '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '( SB-PARSE ))

(DEFPARAMETER SB-ABS-SYN-PACKAGE (FIND-PACKAGE "SYNTAX-BOX")) 

(DEFUN SB-PARSE (&KEY (NT 'META-GRAMMAR) ERROR-THRESHOLD ASK-ABOUT-BAD-TOKENS
  (RETURN-ERRORS NIL) (STREAM NIL STREAMP) STRING FILE (EXHAUST-STREAM NIL))
  (COND (STREAM)
        (STRING
         (SETF STREAM (MAKE-STRING-INPUT-STREAM STRING)))
        (FILE
         (SETF STREAM (OPEN FILE)))
        (T
         (ERROR
          "Must provide an input means -- either :stream, ~
			:string, or :file.")))
  (SETQ *SB-KEYWORD-TABLE* (INIT-KEYWORD-TABLE SB-KEYWORD-LIST))
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
        (*ABS-SYN-PACKAGE* SB-ABS-SYN-PACKAGE)
        (*READER-FUN* #'READER)
        (*APPLY-LEX-TERM-CONSTR-FUN* #'APPLY-LEXICAL-TERMINAL-CONSTRUCTOR)
        (*KEYWORD-TABLE* *SB-KEYWORD-TABLE*)
        (*CLOSE-COMMENT-CHAR* SB-CLOSE-COMMENT-CHAR)
        (*CASE-SENSITIVE* SB-CASE-SENSITIVE))
    (INIT-LEXER *LEXICAL-STREAM*)
    (MULTIPLE-VALUE-BIND (ABS-SYNTAX ERROR-STRING ERROR-ARGS ERROR-PLACE) (UNWIND-PROTECT (CATCH
                      'PARSER-ABORT-CATCH
                      (PROG1
                        (CASE NT
                          (META-GRAMMAR (!META-GRAMMAR! 0))
                          (EXTERNAL-GRAMMARS (!EXTERNAL-GRAMMARS! 0))
                          (COMMENT-CHARACTER (!COMMENT-CHARACTER! 0))
                          (ESCAPE-CHARACTER (!ESCAPE-CHARACTER! 0))
                          (OPERATOR-INFORMATION (!OPERATOR-INFORMATION! 0))
                          (LEXICAL-TERMINALS (!LEXICAL-TERMINALS! 0))
                          (BRACKETING-INFORMATION (!BRACKETING-INFORMATION! 0))
                          (SPACING-INFORMATION (!SPACING-INFORMATION! 0))
                          (PRECEDENCE-INFORMATION (!PRECEDENCE-INFORMATION! 0))
                          (MULTIPLE-LEVELS (!MULTIPLE-LEVELS! 0))
                          (SINGLE-LEVEL (!SINGLE-LEVEL! 0))
                          (SINGLE-OP-PRECEDENCE (!SINGLE-OP-PRECEDENCE! 0))
                          (NONTERMINAL-DEFINITION (!NONTERMINAL-DEFINITION! 0))
                          (PATTERN (!PATTERN! 0))
                          (AUGMENT (!AUGMENT! 0))
                          (FORMAT-COMMAND (!FORMAT-COMMAND! 0))
                          (T (ERROR "Unknown nonterminal ~A." NT)))
                        (WHEN (OR FILE STRING EXHAUST-STREAM)
                          (CLET* (((TOKEN IGNORE PLACE) (PEEK-FIRST)))
                                 (UNLESS (OR (EQ TOKEN :EOF) (EQ TOKEN 'EOF))
                                   (DO-SYNTAX-ERROR
                                    "There is garbage at the end of your ~
			      file or string:~%~A"
                                    PLACE))))))
                     (UNLESS
                      STREAMP
                      (CLOSE STREAM)))
      (IF RETURN-ERRORS
          (VALUES ABS-SYNTAX *PARSER-ERROR* ERROR-PLACE ERROR-STRING ERROR-ARGS)
          (VALUES ABS-SYNTAX *PARSER-ERROR*))))) 


(DEFUN !FORMAT-COMMAND! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::/+)))
      (PROGN
        (LAM ((SBST::/+ EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V2))
        (VALUE-TO-SLOT V0 2)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'INCR-BP (LIST V2)))))
     ((LA-MATCH ((SBST::/-)))
      (PROGN
        (LAM ((SBST::/- EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V2))
        (VALUE-TO-SLOT V0 3)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'DECR-BP (LIST V2)))))
     ((LA-MATCH ((SBST::?)))
      (PROGN
        (LAM ((SBST::? EPSILON)) (GOBBLE-TOKEN))
        (ALT-PARSE
         ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V2 0)
          (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V4)))
         ((LA-MATCH ((SBST::!NUMBER!))) (VALUE-TO-SLOT V2 1)
          (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V4)))
         (T (INITIAL-ERROR '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
        (VALUE-TO-SLOT V3 V4)
        (VALUE-TO-SLOT V0 4)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'UNITE (LIST V3)))))
     ((LA-MATCH ((SBST::_)))
      (PROGN
        (LAM ((SBST::_ EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::!NUMBER! EPSILON))
                   (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V3)) V2)
        (VALUE-TO-SLOT V0 0)
        (AS-TO-SLOT V1
                    (IF (= V2 0) (MAKE-SB-TERM 'SP (LIST (MK-NUMBER 1)))
                        (MAKE-SB-TERM 'SP (LIST V3))))))
     ((LA-MATCH ((SBST::|#|)))
      (PROGN
        (LAM ((SBST::|#| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 1)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'CR (LIST)))))
     ((LA-MATCH ((SBST::!+)))
      (PROGN
        (LAM ((SBST::!+ EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::!NUMBER! EPSILON))
                   (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V3)) V2)
        (VALUE-TO-SLOT V0 5)
        (AS-TO-SLOT V1
                    (IF (= V2 0)
                        (MAKE-SB-TERM 'PUSH-INDENT (LIST (MK-NUMBER 2)))
                        (MAKE-SB-TERM 'PUSH-INDENT (LIST V3))))))
     ((LA-MATCH ((SBST::!-)))
      (PROGN
        (LAM ((SBST::!- EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 6)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'POP-INDENT (LIST)))))
     ((LA-MATCH ((SBST::@<)))
      (PROGN
        (LAM ((SBST::@< EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 7)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'PUSH-TAB-LEFT (LIST)))))
     ((LA-MATCH ((SBST::@>)))
      (PROGN
        (LAM ((SBST::@> EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 8)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'PUSH-TAB-RIGHT (LIST)))))
     ((LA-MATCH ((SBST::@^)))
      (PROGN
        (LAM ((SBST::@^ EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 9)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'POP-TAB (LIST)))))
     (T
      (INITIAL-ERROR
       '((SBST::@^ EPSILON) (SBST::@> EPSILON) (SBST::@< EPSILON)
        (SBST::!- EPSILON) (SBST::!+ EPSILON SBST::!NUMBER!)
        (SBST::? SBST::!ID! SBST::!NUMBER!) (SBST::/- SBST::!NUMBER!)
        (SBST::/+ SBST::!NUMBER!) (SBST::|#| EPSILON)
        (SBST::_ EPSILON SBST::!NUMBER!)))))
    (VALUE-TO-SLOT V0 V1)
    V0)) 


(DEFUN !AUGMENT! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (LET (MTEMP
        V0
        V1
        V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5)
    (STACK-BRACKETS SBST::{ AUGMENT)
    (INITIALS
     ((LA-MATCH ((SBST::[)))
      (PROGN
        (LAM ((SBST::[ EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V2 (!AUGMENT! 10))
        (LAM ((SBST::] EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 21)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'OPT-AUG (LIST V2)))))
     ((LA-MATCH ((SBST::!ID! SBST::@)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V2))
        (LAM ((SBST::@ EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V0 11)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'EXT-NAME (LIST V2 V3)))))
     ((LA-MATCH ((SBST::LIST)))
      (PROGN
        (LAM ((SBST::LIST EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (DOUBLESTAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::[ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|)
          (SBST::CONS SBST::|(|) (SBST::LIST SBST::|(|)
          (SBST::NULL EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::SEQ EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLESTAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::STAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLEPLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::PLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::ALT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::OPT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!ID! EPSILON SBST::@ SBST::|(| SBST::* SBST::+ SBST::\|
           SBST::^)
          (SBST::!LITERAL! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!NUMBER! EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!STRING! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^))
         (CODE-TO-SLOT V3 (!AUGMENT! 0)) V2 V3 'SBST::|,|)
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 13)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'LIST (DS-TEMP-REPT (CK-REPT-REF V2))))))
     ((LA-MATCH ((SBST::CONS)))
      (PROGN
        (LAM ((SBST::CONS EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V2 (!AUGMENT! 0))
        (LAM ((SBST::|,| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V3 (!AUGMENT! 0))
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 14)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'CONS (LIST V2 V3)))))
     ((LA-MATCH ((SBST::BCONS)))
      (PROGN
        (LAM ((SBST::BCONS EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V2 (!AUGMENT! 0))
        (LAM ((SBST::|,| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V3 (!AUGMENT! 0))
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 15)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'BCONS (LIST V2 V3)))))
     ((LA-MATCH ((SBST::APPEND)))
      (PROGN
        (LAM ((SBST::APPEND EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V2 (!AUGMENT! 0))
        (LAM ((SBST::|,| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V3 (!AUGMENT! 0))
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 16)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'APPEND (LIST V2 V3)))))
     ((LA-MATCH ((SBST::!STRING! SBST::|(|)))
      (PROGN
        (LAM ((SBST::!STRING! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V2 2)
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (DOUBLESTAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::[ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|)
          (SBST::CONS SBST::|(|) (SBST::LIST SBST::|(|)
          (SBST::NULL EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::SEQ EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLESTAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::STAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLEPLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::PLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::ALT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::OPT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!ID! EPSILON SBST::@ SBST::|(| SBST::* SBST::+ SBST::\|
           SBST::^)
          (SBST::!LITERAL! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!NUMBER! EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!STRING! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^))
         (CODE-TO-SLOT V5 (!AUGMENT! 0)) V4 V5 'SBST::|,|)
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 17)
        (AS-TO-SLOT V1
                    (MAKE-SB-TERM 'TERM-CONST
                                  (DS-TEMP-REPT
                                   (MK-TEMP-REPT
                                    (CONS
                                     (CASE V2
                                       (2 (MAKE-SB-TERM 'STRING-AUG (LIST V3)))
                                       (1 (MAKE-SB-TERM 'LITERAL-AUG (LIST V3)))
                                       (0 (MAKE-SB-TERM 'ID-AUG (LIST V3))))
                                     (DS-TEMP-REPT (CK-REPT-REF V4)))))))))
     ((LA-MATCH ((SBST::!LITERAL! SBST::|(|)))
      (PROGN
        (LAM ((SBST::!LITERAL! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V2 1)
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (DOUBLESTAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::[ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|)
          (SBST::CONS SBST::|(|) (SBST::LIST SBST::|(|)
          (SBST::NULL EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::SEQ EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLESTAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::STAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLEPLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::PLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::ALT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::OPT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!ID! EPSILON SBST::@ SBST::|(| SBST::* SBST::+ SBST::\|
           SBST::^)
          (SBST::!LITERAL! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!NUMBER! EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!STRING! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^))
         (CODE-TO-SLOT V5 (!AUGMENT! 0)) V4 V5 'SBST::|,|)
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 17)
        (AS-TO-SLOT V1
                    (MAKE-SB-TERM 'TERM-CONST
                                  (DS-TEMP-REPT
                                   (MK-TEMP-REPT
                                    (CONS
                                     (CASE V2
                                       (2 (MAKE-SB-TERM 'STRING-AUG (LIST V3)))
                                       (1 (MAKE-SB-TERM 'LITERAL-AUG (LIST V3)))
                                       (0 (MAKE-SB-TERM 'ID-AUG (LIST V3))))
                                     (DS-TEMP-REPT (CK-REPT-REF V4)))))))))
     ((LA-MATCH ((SBST::!ID! SBST::|(|)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V2 0)
        (LAM ((SBST::|(| EPSILON)) (GOBBLE-TOKEN))
        (DOUBLESTAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::[ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS
           SBST::LIST SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR
           SBST::DOUBLEPLUS SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
           SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
          (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|)
          (SBST::CONS SBST::|(|) (SBST::LIST SBST::|(|)
          (SBST::NULL EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::SEQ EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLESTAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::STAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::DOUBLEPLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::PLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::ALT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::OPT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!ID! EPSILON SBST::@ SBST::|(| SBST::* SBST::+ SBST::\|
           SBST::^)
          (SBST::!LITERAL! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!NUMBER! EPSILON SBST::* SBST::+ SBST::\| SBST::^)
          (SBST::!STRING! EPSILON SBST::|(| SBST::* SBST::+ SBST::\| SBST::^))
         (CODE-TO-SLOT V5 (!AUGMENT! 0)) V4 V5 'SBST::|,|)
        (LAM ((SBST::|)| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 17)
        (AS-TO-SLOT V1
                    (MAKE-SB-TERM 'TERM-CONST
                                  (DS-TEMP-REPT
                                   (MK-TEMP-REPT
                                    (CONS
                                     (CASE V2
                                       (2 (MAKE-SB-TERM 'STRING-AUG (LIST V3)))
                                       (1 (MAKE-SB-TERM 'LITERAL-AUG (LIST V3)))
                                       (0 (MAKE-SB-TERM 'ID-AUG (LIST V3))))
                                     (DS-TEMP-REPT (CK-REPT-REF V4)))))))))
     ((LA-MATCH ((SBST::!STRING! EPSILON)))
      (PROGN
        (LAM ((SBST::!STRING! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 0)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'STRING-AUG (LIST V1)))))
     ((LA-MATCH ((SBST::!NUMBER!)))
      (PROGN
        (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 1)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NUMBER-AUG (LIST V1)))))
     ((LA-MATCH ((SBST::!LITERAL! EPSILON)))
      (PROGN
        (LAM ((SBST::!LITERAL! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 2)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'LITERAL-AUG (LIST V1)))))
     ((LA-MATCH ((SBST::!ID! EPSILON)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 3)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST V1)))))
     ((LA-MATCH ((SBST::OPT)))
      (PROGN
        (LAM ((SBST::OPT EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 4)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'OPT))))))
     ((LA-MATCH ((SBST::ALT)))
      (PROGN
        (LAM ((SBST::ALT EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 5)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'ALT))))))
     ((LA-MATCH ((SBST::PLUS)))
      (PROGN
        (LAM ((SBST::PLUS EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 6)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'PLUS))))))
     ((LA-MATCH ((SBST::DOUBLEPLUS)))
      (PROGN
        (LAM ((SBST::DOUBLEPLUS EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 7)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'DOUBLEPLUS))))))
     ((LA-MATCH ((SBST::STAR)))
      (PROGN
        (LAM ((SBST::STAR EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 8)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'STAR))))))
     ((LA-MATCH ((SBST::DOUBLESTAR)))
      (PROGN
        (LAM ((SBST::DOUBLESTAR EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 9)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'DOUBLESTAR))))))
     ((LA-MATCH ((SBST::SEQ)))
      (PROGN
        (LAM ((SBST::SEQ EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 10)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NAME (LIST (MK-ID 'SEQ))))))
     ((LA-MATCH ((SBST::NULL)))
      (PROGN
        (LAM ((SBST::NULL EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 12)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NULL (LIST)))))
     (T
      (INITIAL-ERROR
       '((SBST::!ID! SBST::|(| SBST::@ EPSILON)
        (SBST::!LITERAL! SBST::|(| EPSILON) (SBST::!STRING! SBST::|(| EPSILON)
        (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|) (SBST::CONS SBST::|(|)
        (SBST::LIST SBST::|(|) (SBST::NULL EPSILON) (SBST::SEQ EPSILON)
        (SBST::DOUBLESTAR EPSILON) (SBST::STAR EPSILON)
        (SBST::DOUBLEPLUS EPSILON) (SBST::PLUS EPSILON) (SBST::ALT EPSILON)
        (SBST::OPT EPSILON) (SBST::!NUMBER! EPSILON)
        (SBST::[ SBST::!STRING! SBST::!NUMBER! SBST::!LITERAL! SBST::!ID!
         SBST::OPT SBST::ALT SBST::PLUS SBST::DOUBLEPLUS SBST::STAR
         SBST::DOUBLESTAR SBST::SEQ SBST::NULL SBST::LIST SBST::CONS
         SBST::BCONS SBST::APPEND SBST::[ SBST::{)
        (SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS SBST::CONS SBST::LIST
         SBST::NULL SBST::SEQ SBST::DOUBLESTAR SBST::STAR SBST::DOUBLEPLUS
         SBST::PLUS SBST::ALT SBST::OPT SBST::!ID! SBST::!LITERAL!
         SBST::!NUMBER! SBST::!STRING!)))))
    (VALUE-TO-SLOT V0 V1)
    (EAT-BRACKETS (SBST::} SBST::{) AUGMENT)
    (COND ((>
            (COND ((MEMBER (PEEK-FIRST) '(SBST::* SBST::+ SBST::\| SBST::^))
                   (SETQ MTEMP (PEEK-FIRST))
                   (CADR
                    (ASSOC (PEEK-FIRST)
                           '((SBST::^ 40) (SBST::+ 30) (SBST::* 30)
                            (SBST::\| 20)))))
                  (T
                   0))
            RBP)
           (LOOP (CLEAN-VARIABLES (V1 V2 V3 V4 V5))
                 (CASE MTEMP
                   (SBST::^
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::^ EPSILON)) (GOBBLE-TOKEN))
                      (ALT-PARSE
                       ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V3 0)
                        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V5)))
                       ((LA-MATCH ((SBST::!NUMBER!))) (VALUE-TO-SLOT V3 1)
                        (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V5)))
                       (T
                        (INITIAL-ERROR
                         '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
                      (VALUE-TO-SLOT V4 V5)
                      (VALUE-TO-SLOT V0 22)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'TAG-AUG (LIST V2 V4)))))
                   (SBST::\|
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::\| EPSILON)) (GOBBLE-TOKEN))
                      (DOUBLEPLUS-PARSE
                       ((SBST::{ SBST::{ SBST::[ SBST::APPEND SBST::BCONS
                         SBST::CONS SBST::LIST SBST::NULL SBST::SEQ
                         SBST::DOUBLESTAR SBST::STAR SBST::DOUBLEPLUS
                         SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
                         SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
                        (SBST::[ SBST::{ SBST::[ SBST::APPEND SBST::BCONS
                         SBST::CONS SBST::LIST SBST::NULL SBST::SEQ
                         SBST::DOUBLESTAR SBST::STAR SBST::DOUBLEPLUS
                         SBST::PLUS SBST::ALT SBST::OPT SBST::!ID!
                         SBST::!LITERAL! SBST::!NUMBER! SBST::!STRING!)
                        (SBST::APPEND SBST::|(|) (SBST::BCONS SBST::|(|)
                        (SBST::CONS SBST::|(|) (SBST::LIST SBST::|(|)
                        (SBST::NULL EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::SEQ EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::DOUBLESTAR EPSILON SBST::* SBST::+ SBST::\|
                         SBST::^)
                        (SBST::STAR EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::DOUBLEPLUS EPSILON SBST::* SBST::+ SBST::\|
                         SBST::^)
                        (SBST::PLUS EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::ALT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::OPT EPSILON SBST::* SBST::+ SBST::\| SBST::^)
                        (SBST::!ID! EPSILON SBST::@ SBST::|(| SBST::* SBST::+
                         SBST::\| SBST::^)
                        (SBST::!LITERAL! EPSILON SBST::|(| SBST::* SBST::+
                         SBST::\| SBST::^)
                        (SBST::!NUMBER! EPSILON SBST::* SBST::+ SBST::\|
                         SBST::^)
                        (SBST::!STRING! EPSILON SBST::|(| SBST::* SBST::+
                         SBST::\| SBST::^))
                       (CODE-TO-SLOT V4 (!AUGMENT! 21)) V3 V4 'SBST::\|)
                      (VALUE-TO-SLOT V0 20)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'ALT-AUG
                                                (DS-TEMP-REPT
                                                 (MK-TEMP-REPT
                                                  (CONS V2
                                                        (DS-TEMP-REPT
                                                         (CK-REPT-REF V3)))))))))
                   (SBST::+
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::+ EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 19)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'PLUS-AUG (LIST V2)))))
                   (SBST::*
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::* EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 18)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'STAR-AUG (LIST V2)))))
                   (T (MEDIAL-ERROR '(SBST::* SBST::+ SBST::\| SBST::^))))
                 (VALUE-TO-SLOT V0 V1) (EAT-BRACKETS (SBST::} SBST::{) AUGMENT)
                 (WHEN (<=
                   (COND ((MEMBER (PEEK-FIRST)
                                  '(SBST::* SBST::+ SBST::\| SBST::^))
                          (SETQ MTEMP (PEEK-FIRST))
                          (CADR
                           (ASSOC (PEEK-FIRST)
                                  '((SBST::^ 40) (SBST::+ 30) (SBST::* 30)
                                   (SBST::\| 20)))))
                         (T
                          0))
                   RBP)
                   (RETURN NIL)))))
    V0)) 


(DEFUN !PATTERN! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (MTEMP
        V0
        V1
        V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (INITIALS
     ((LA-MATCH ((SBST::{)))
      (PROGN
        (LAM ((SBST::{ EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V3 (!PATTERN! 10))
        (CONS-TO-SLOT V2 V3)
        (GEN-STAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
          (SBST::[ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
          (SBST::!KEYWORD! EPSILON SBST::JUX SBST::\| SBST::* SBST::+ SBST::++
           SBST::** SBST::^ SBST::< SBST::<<)
          (SBST::!ID! EPSILON SBST::@ SBST::JUX SBST::\| SBST::* SBST::+
           SBST::++ SBST::** SBST::^ SBST::< SBST::<<))
         (CODE-TO-SLOT V3 (!PATTERN! 0)) V2 V3)
        (LAM ((SBST::} EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 5)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'SEQ (DS-TEMP-REPT (CK-REPT-REF V2))))))
     ((LA-MATCH ((SBST::[)))
      (PROGN
        (LAM ((SBST::[ EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V3 (!PATTERN! 10))
        (CONS-TO-SLOT V2 V3)
        (GEN-STAR-PARSE
         ((SBST::{ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
          (SBST::[ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
          (SBST::!KEYWORD! EPSILON SBST::JUX SBST::\| SBST::* SBST::+ SBST::++
           SBST::** SBST::^ SBST::< SBST::<<)
          (SBST::!ID! EPSILON SBST::@ SBST::JUX SBST::\| SBST::* SBST::+
           SBST::++ SBST::** SBST::^ SBST::< SBST::<<))
         (CODE-TO-SLOT V3 (!PATTERN! 0)) V2 V3)
        (LAM ((SBST::] EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V0 4)
        (AS-TO-SLOT V1
                    (MAKE-SB-TERM 'OPT
                                  (LIST
                                   (MAKE-SB-TERM 'SEQ
                                                 (DS-TEMP-REPT (CK-REPT-REF V2))))))))
     ((LA-MATCH ((SBST::!ID! SBST::@)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V2))
        (LAM ((SBST::@ EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V0 1)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'EXT-NONTERMINAL (LIST V2 V3)))))
     ((LA-MATCH ((SBST::!ID! EPSILON)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 0)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'NONTERMINAL (LIST V1)))))
     ((LA-MATCH ((SBST::!KEYWORD!)))
      (PROGN
        (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V1))
        (VALUE-TO-SLOT V0 2)
        (AS-TO-SLOT V1 (MAKE-SB-TERM 'UKEYWORD (LIST V1)))))
     (T
      (INITIAL-ERROR
       '((SBST::!KEYWORD! EPSILON) (SBST::!ID! SBST::@ EPSILON)
        (SBST::[ SBST::!ID! SBST::!KEYWORD! SBST::[ SBST::{)
        (SBST::{ SBST::!ID! SBST::!KEYWORD! SBST::[ SBST::{)))))
    (VALUE-TO-SLOT V0 V1)
    (COND ((>
            (COND ((MEMBER (PEEK-FIRST)
                           '(SBST::JUX SBST::\| SBST::* SBST::+ SBST::++
                            SBST::** SBST::^ SBST::< SBST::<<))
                   (SETQ MTEMP (PEEK-FIRST))
                   (CADR
                    (ASSOC (PEEK-FIRST)
                           '((SBST::^ 80) (SBST::JUX 70) (SBST::++ 60)
                            (SBST::** 60) (SBST::+ 60) (SBST::* 60)
                            (SBST::< 50) (SBST::<< 40) (SBST::JUX 30)
                            (SBST::\| 20)))))
                  ((MEMBER (PEEK-FIRST)
                           '(SBST::_ SBST::|#| SBST::/+ SBST::/- SBST::?
                            SBST::!+ SBST::!- SBST::@< SBST::@> SBST::@^))
                   (SETQ MTEMP 'G24129)
                   70)
                  (T
                   0))
            RBP)
           (LOOP (CLEAN-VARIABLES (V1 V2 V3 V4 V5 V6 V7 V8 V9 V10))
                 (CASE MTEMP
                   (SBST::<<
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::<< EPSILON)) (GOBBLE-TOKEN))
                      (PLUS-PARSE
                       ((SBST::{ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                        (SBST::[ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                        (SBST::!KEYWORD! EPSILON SBST::JUX SBST::\| SBST::*
                         SBST::+ SBST::++ SBST::** SBST::^ SBST::< SBST::<<)
                        (SBST::!ID! EPSILON SBST::@ SBST::JUX SBST::\| SBST::*
                         SBST::+ SBST::++ SBST::** SBST::^ SBST::< SBST::<<))
                       (PROGN
                         (CODE-TO-SLOT V6 (!PATTERN! 41))
                         NIL) V5 V6)
                      (LAM ((SBST::> EPSILON)) (GOBBLE-TOKEN))
                      (OPT-PARSE ((SBST::|:| SBST::!ID!))
                                 (PROGN
                                   (LAM ((SBST::|:| EPSILON)) (GOBBLE-TOKEN))
                                   (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON))
                                                     (PROGN
                                                       (LAM
                                                        ((SBST::!ID! EPSILON))
                                                        (GOBBLE-TO-SLOT V9))
                                                       NIL)
                                                     V8 V9 'SBST::|,|))
                                 V7)
                      (LAM ((SBST::> EPSILON)) (GOBBLE-TOKEN))
                      (AS-TO-SLOT V10
                                  (MAKE-SB-TERM 'UPAT
                                                (LIST
                                                 (IF (= V7 0)
                                                     (MAKE-SB-TERM 'UIDS NIL)
                                                     (MAKE-SB-TERM 'UIDS
                                                                   (DS-TEMP-REPT
                                                                    (CK-REPT-REF
                                                                     V8))))
                                                 (MAKE-SB-TERM 'SEQ
                                                               (DS-TEMP-REPT
                                                                (CK-REPT-REF V5))))))
                      (VALUE-TO-SLOT V4 V10)
                      (CONS-TO-SLOT V3 V4)
                      (GEN-STAR-PARSE
                       ((SBST::<< SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!))
                       (PROGN
                         (LAM ((SBST::<< EPSILON)) (GOBBLE-TOKEN))
                         (PLUS-PARSE
                          ((SBST::{ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                           (SBST::[ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                           (SBST::!KEYWORD! EPSILON SBST::JUX SBST::\| SBST::*
                            SBST::+ SBST::++ SBST::** SBST::^ SBST::< SBST::<<)
                           (SBST::!ID! EPSILON SBST::@ SBST::JUX SBST::\|
                            SBST::* SBST::+ SBST::++ SBST::** SBST::^ SBST::<
                            SBST::<<))
                          (PROGN
                            (CODE-TO-SLOT V6 (!PATTERN! 41))
                            NIL) V5 V6)
                         (LAM ((SBST::> EPSILON)) (GOBBLE-TOKEN))
                         (OPT-PARSE ((SBST::|:| SBST::!ID!))
                                    (PROGN
                                      (LAM ((SBST::|:| EPSILON)) (GOBBLE-TOKEN))
                                      (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON))
                                                        (PROGN
                                                          (LAM
                                                           ((SBST::!ID! EPSILON))
                                                           (GOBBLE-TO-SLOT V9))
                                                          NIL)
                                                        V8 V9 'SBST::|,|))
                                    V7)
                         (LAM ((SBST::> EPSILON)) (GOBBLE-TOKEN))
                         (AS-TO-SLOT V10
                                     (MAKE-SB-TERM 'UPAT
                                                   (LIST
                                                    (IF (= V7 0)
                                                        (MAKE-SB-TERM 'UIDS NIL)
                                                        (MAKE-SB-TERM 'UIDS
                                                                      (DS-TEMP-REPT
                                                                      (CK-REPT-REF
                                                                      V8))))
                                                    (MAKE-SB-TERM 'SEQ
                                                                  (DS-TEMP-REPT
                                                                   (CK-REPT-REF
                                                                    V5))))))
                         (VALUE-TO-SLOT V4 V10))
                       V3 V4)
                      (VALUE-TO-SLOT V0 14)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'UPATTERN
                                                (LIST V2
                                                      (MAKE-SB-TERM 'UPATS
                                                                    (DS-TEMP-REPT
                                                                     (CK-REPT-REF
                                                                      V3))))))))
                   (SBST::<
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::< EPSILON)) (GOBBLE-TOKEN))
                      (CODE-TO-SLOT V3 (!AUGMENT! 0))
                      (LAM ((SBST::> EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 13)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'AUGMENT (LIST V2 V3)))))
                   (G24129
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (PLUS-PARSE
                       ((SBST::@^ EPSILON) (SBST::@> EPSILON)
                        (SBST::@< EPSILON) (SBST::!- EPSILON)
                        (SBST::!+ SBST::!NUMBER! EPSILON)
                        (SBST::? SBST::!ID! SBST::!NUMBER!)
                        (SBST::/- SBST::!NUMBER!) (SBST::/+ SBST::!NUMBER!)
                        (SBST::|#| EPSILON) (SBST::_ SBST::!NUMBER! EPSILON))
                       (CODE-TO-SLOT V4 (!FORMAT-COMMAND! 0)) V3 V4)
                      (VALUE-TO-SLOT V0 12)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'FORMAT
                                                (LIST V2
                                                      (MAKE-SB-TERM 'WS-SPECS
                                                                    (DS-TEMP-REPT
                                                                     (CK-REPT-REF
                                                                      V3))))))))
                   (SBST::^
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::^ EPSILON)) (GOBBLE-TOKEN))
                      (ALT-PARSE
                       ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V3 0)
                        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V5)))
                       ((LA-MATCH ((SBST::!NUMBER!))) (VALUE-TO-SLOT V3 1)
                        (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V5)))
                       (T
                        (INITIAL-ERROR
                         '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
                      (VALUE-TO-SLOT V4 V5)
                      (VALUE-TO-SLOT V0 11)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'TAG (LIST V2 V4)))))
                   (SBST::**
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::** EPSILON)) (GOBBLE-TOKEN))
                      (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V3))
                      (OPT-PARSE
                       ((SBST::_ SBST::!NUMBER! EPSILON SBST::@^ SBST::@>
                         SBST::@< SBST::!- SBST::!+ SBST::? SBST::/- SBST::/+
                         SBST::|#| SBST::_)
                        (SBST::|#| EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::/+ SBST::!NUMBER!) (SBST::/- SBST::!NUMBER!)
                        (SBST::? SBST::!ID! SBST::!NUMBER!)
                        (SBST::!+ SBST::!NUMBER! EPSILON SBST::@^ SBST::@>
                         SBST::@< SBST::!- SBST::!+ SBST::? SBST::/- SBST::/+
                         SBST::|#| SBST::_)
                        (SBST::!- EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@< EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@> EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@^ EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_))
                       (PLUS-PARSE
                        ((SBST::@^ EPSILON) (SBST::@> EPSILON)
                         (SBST::@< EPSILON) (SBST::!- EPSILON)
                         (SBST::!+ SBST::!NUMBER! EPSILON)
                         (SBST::? SBST::!ID! SBST::!NUMBER!)
                         (SBST::/- SBST::!NUMBER!) (SBST::/+ SBST::!NUMBER!)
                         (SBST::|#| EPSILON) (SBST::_ SBST::!NUMBER! EPSILON))
                        (CODE-TO-SLOT V6 (!FORMAT-COMMAND! 0)) V5 V6)
                       V4)
                      (VALUE-TO-SLOT V0 10)
                      (AS-TO-SLOT V1
                                  (IF (= V4 0)
                                      (MAKE-SB-TERM 'DOUBLESTAR
                                                    (LIST V2
                                                          (MAKE-SB-TERM
                                                           'UKEYWORD (LIST V3))))
                                      (MAKE-SB-TERM 'DOUBLESTAR
                                                    (LIST V2
                                                          (MAKE-SB-TERM 'FORMAT
                                                                      (LIST
                                                                      (MAKE-SB-TERM
                                                                      'UKEYWORD
                                                                      (LIST
                                                                      V3))
                                                                      (MAKE-SB-TERM
                                                                      'WS-SPECS
                                                                      (DS-TEMP-REPT
                                                                      (CK-REPT-REF
                                                                      V5)))))))))))
                   (SBST::++
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::++ EPSILON)) (GOBBLE-TOKEN))
                      (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V3))
                      (OPT-PARSE
                       ((SBST::_ SBST::!NUMBER! EPSILON SBST::@^ SBST::@>
                         SBST::@< SBST::!- SBST::!+ SBST::? SBST::/- SBST::/+
                         SBST::|#| SBST::_)
                        (SBST::|#| EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::/+ SBST::!NUMBER!) (SBST::/- SBST::!NUMBER!)
                        (SBST::? SBST::!ID! SBST::!NUMBER!)
                        (SBST::!+ SBST::!NUMBER! EPSILON SBST::@^ SBST::@>
                         SBST::@< SBST::!- SBST::!+ SBST::? SBST::/- SBST::/+
                         SBST::|#| SBST::_)
                        (SBST::!- EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@< EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@> EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_)
                        (SBST::@^ EPSILON SBST::@^ SBST::@> SBST::@< SBST::!-
                         SBST::!+ SBST::? SBST::/- SBST::/+ SBST::|#| SBST::_))
                       (PLUS-PARSE
                        ((SBST::@^ EPSILON) (SBST::@> EPSILON)
                         (SBST::@< EPSILON) (SBST::!- EPSILON)
                         (SBST::!+ SBST::!NUMBER! EPSILON)
                         (SBST::? SBST::!ID! SBST::!NUMBER!)
                         (SBST::/- SBST::!NUMBER!) (SBST::/+ SBST::!NUMBER!)
                         (SBST::|#| EPSILON) (SBST::_ SBST::!NUMBER! EPSILON))
                        (CODE-TO-SLOT V6 (!FORMAT-COMMAND! 0)) V5 V6)
                       V4)
                      (VALUE-TO-SLOT V0 9)
                      (AS-TO-SLOT V1
                                  (IF (= V4 0)
                                      (MAKE-SB-TERM 'DOUBLEPLUS
                                                    (LIST V2
                                                          (MAKE-SB-TERM
                                                           'UKEYWORD (LIST V3))))
                                      (MAKE-SB-TERM 'DOUBLEPLUS
                                                    (LIST V2
                                                          (MAKE-SB-TERM 'FORMAT
                                                                      (LIST
                                                                      (MAKE-SB-TERM
                                                                      'UKEYWORD
                                                                      (LIST
                                                                      V3))
                                                                      (MAKE-SB-TERM
                                                                      'WS-SPECS
                                                                      (DS-TEMP-REPT
                                                                      (CK-REPT-REF
                                                                      V5)))))))))))
                   (SBST::+
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::+ EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 8)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'PLUS (LIST V2)))))
                   (SBST::*
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::* EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V0 7)
                      (AS-TO-SLOT V1 (MAKE-SB-TERM 'STAR (LIST V2)))))
                   (SBST::\|
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::\| EPSILON)) (GOBBLE-TOKEN))
                      (DOUBLEPLUS-PARSE
                       ((SBST::{ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                        (SBST::[ SBST::{ SBST::[ SBST::!KEYWORD! SBST::!ID!)
                        (SBST::!KEYWORD! EPSILON SBST::JUX SBST::\| SBST::*
                         SBST::+ SBST::++ SBST::** SBST::^ SBST::< SBST::<<)
                        (SBST::!ID! EPSILON SBST::@ SBST::JUX SBST::\| SBST::*
                         SBST::+ SBST::++ SBST::** SBST::^ SBST::< SBST::<<))
                       (CODE-TO-SLOT V4 (!PATTERN! 21)) V3 V4 'SBST::\|)
                      (VALUE-TO-SLOT V0 6)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'ALT
                                                (DS-TEMP-REPT
                                                 (MK-TEMP-REPT
                                                  (CONS V2
                                                        (DS-TEMP-REPT
                                                         (CK-REPT-REF V3)))))))))
                   (SBST::JUX
                    (PROGN
                      (SLOT-TO-SLOT V2 V0)
                      (LAM ((SBST::JUX EPSILON)) (GOBBLE-TOKEN))
                      (CODE-TO-SLOT V3 (!PATTERN! 31))
                      (OPT-PARSE
                       ((SBST::JUXFORM SBST::_ SBST::|#| SBST::/+ SBST::/-
                         SBST::? SBST::!+ SBST::!- SBST::@< SBST::@> SBST::@^))
                       (PROGN
                         (LAM ((SBST::JUXFORM EPSILON)) (GOBBLE-TOKEN))
                         (PLUS-PARSE
                          ((SBST::@^ EPSILON) (SBST::@> EPSILON)
                           (SBST::@< EPSILON) (SBST::!- EPSILON)
                           (SBST::!+ SBST::!NUMBER! EPSILON)
                           (SBST::? SBST::!ID! SBST::!NUMBER!)
                           (SBST::/- SBST::!NUMBER!) (SBST::/+ SBST::!NUMBER!)
                           (SBST::|#| EPSILON) (SBST::_ SBST::!NUMBER! EPSILON))
                          (CODE-TO-SLOT V6 (!FORMAT-COMMAND! 0)) V5 V6))
                       V4)
                      (VALUE-TO-SLOT V0 3)
                      (AS-TO-SLOT V1
                                  (MAKE-SB-TERM 'JUX
                                                (LIST V2 V3
                                                      (IF (= V4 0)
                                                          (MAKE-SB-TERM
                                                           'WS-SPECS NIL)
                                                          (MAKE-SB-TERM
                                                           'WS-SPECS
                                                           (DS-TEMP-REPT
                                                            (CK-REPT-REF V5)))))))))
                   (T
                    (MEDIAL-ERROR
                     '(SBST::JUX SBST::\| SBST::* SBST::+ SBST::++ SBST::**
                      SBST::^ SBST::_ SBST::|#| SBST::/+ SBST::/- SBST::?
                      SBST::!+ SBST::!- SBST::@< SBST::@> SBST::@^ SBST::<
                      SBST::<<))))
                 (VALUE-TO-SLOT V0 V1)
                 (WHEN (<=
                   (COND ((MEMBER (PEEK-FIRST)
                                  '(SBST::JUX SBST::\| SBST::* SBST::+ SBST::++
                                   SBST::** SBST::^ SBST::< SBST::<<))
                          (SETQ MTEMP (PEEK-FIRST))
                          (CADR
                           (ASSOC (PEEK-FIRST)
                                  '((SBST::^ 80) (SBST::JUX 70) (SBST::++ 60)
                                   (SBST::** 60) (SBST::+ 60) (SBST::* 60)
                                   (SBST::< 50) (SBST::<< 40) (SBST::JUX 30)
                                   (SBST::\| 20)))))
                         ((MEMBER (PEEK-FIRST)
                                  '(SBST::_ SBST::|#| SBST::/+ SBST::/- SBST::?
                                   SBST::!+ SBST::!- SBST::@< SBST::@> SBST::@^))
                          (SETQ MTEMP 'G24129)
                          70)
                         (T
                          0))
                   RBP)
                   (RETURN NIL)))))
    V0)) 


(DEFUN !NONTERMINAL-DEFINITION! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::!ID!)))
      (PROGN
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V1))
        (LAM ((SBST::|::=| EPSILON)) (GOBBLE-TOKEN))
        (CODE-TO-SLOT V2 (!PATTERN! 0))
        (AS-TO-SLOT V0 (MAKE-SB-TERM 'NT-DEF (LIST V1 V2)))))
     (T (INITIAL-ERROR '((SBST::!ID! SBST::|::=|)))))
    NIL
    V0)) 


(DEFUN !SINGLE-OP-PRECEDENCE! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
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
     ((LA-MATCH ((SBST::JUX)))
      (PROGN
        (LAM ((SBST::JUX EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::^ SBST::!ID! SBST::!NUMBER!))
                   (PROGN
                     (LAM ((SBST::^ EPSILON)) (GOBBLE-TOKEN))
                     (ALT-PARSE
                      ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V4 0)
                       (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V6)))
                      ((LA-MATCH ((SBST::!NUMBER!))) (VALUE-TO-SLOT V4 1)
                       (LAM ((SBST::!NUMBER! EPSILON)) (GOBBLE-TO-SLOT V6)))
                      (T
                       (INITIAL-ERROR
                        '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
                     (VALUE-TO-SLOT V5 V6))
                   V3)
        (VALUE-TO-SLOT V1 1)
        (AS-TO-SLOT V2
                    (CASE V1
                      (1
                       (MAKE-SB-TERM 'JUX-OP
                                     (LIST
                                      (IF (= V3 0) (MAKE-SB-TERM 'NULL (LIST))
                                          V5))))
                      (0 (MAKE-SB-TERM 'KEYWORD-OP (LIST V3)))))
        (ALT-PARSE
         ((LA-MATCH ((SBST::MEDIAL))) (VALUE-TO-SLOT V7 1)
          (PROGN
            (LAM ((SBST::MEDIAL EPSILON)) (GOBBLE-TOKEN))
            (ALT-PARSE
             ((LA-MATCH ((SBST::LEFT))) (VALUE-TO-SLOT V8 0)
              (LAM ((SBST::LEFT EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::RIGHT))) (VALUE-TO-SLOT V8 1)
              (LAM ((SBST::RIGHT EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::LBIND))) (VALUE-TO-SLOT V8 2)
              (LAM ((SBST::LBIND EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::RBIND))) (VALUE-TO-SLOT V8 3)
              (LAM ((SBST::RBIND EPSILON)) (GOBBLE-TOKEN)))
             (T
              (INITIAL-ERROR
               '((SBST::LEFT EPSILON) (SBST::RIGHT EPSILON)
                (SBST::LBIND EPSILON) (SBST::RBIND EPSILON)))))))
         ((LA-MATCH ((SBST::INITIAL))) (VALUE-TO-SLOT V7 0)
          (LAM ((SBST::INITIAL EPSILON)) (GOBBLE-TOKEN)))
         ((LA-MATCH ((SBST::AGGREGATE))) (VALUE-TO-SLOT V7 2)
          (LAM ((SBST::AGGREGATE EPSILON)) (GOBBLE-TOKEN)))
         (T
          (INITIAL-ERROR
           '((SBST::MEDIAL SBST::LEFT SBST::RIGHT SBST::LBIND SBST::RBIND)
            (SBST::INITIAL EPSILON) (SBST::AGGREGATE EPSILON)))))
        (AS-TO-SLOT V0
                    (CASE V7
                      (2 (MAKE-SB-TERM 'AGGREGATE (LIST V2)))
                      (1
                       (MAKE-SB-TERM 'MEDIAL
                                     (LIST V2
                                           (CASE V8
                                             (3 (MK-ID 'RBIND))
                                             (2 (MK-ID 'LBIND))
                                             (1 (MK-ID 'RIGHT))
                                             (0 (MK-ID 'LEFT))))))
                      (0 (MAKE-SB-TERM 'INITIAL (LIST V2)))))))
     ((LA-MATCH ((SBST::!KEYWORD!)))
      (PROGN
        (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V3))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2
                    (CASE V1
                      (1
                       (MAKE-SB-TERM 'JUX-OP
                                     (LIST
                                      (IF (= V3 0) (MAKE-SB-TERM 'NULL (LIST))
                                          V5))))
                      (0 (MAKE-SB-TERM 'KEYWORD-OP (LIST V3)))))
        (ALT-PARSE
         ((LA-MATCH ((SBST::MEDIAL))) (VALUE-TO-SLOT V7 1)
          (PROGN
            (LAM ((SBST::MEDIAL EPSILON)) (GOBBLE-TOKEN))
            (ALT-PARSE
             ((LA-MATCH ((SBST::LEFT))) (VALUE-TO-SLOT V8 0)
              (LAM ((SBST::LEFT EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::RIGHT))) (VALUE-TO-SLOT V8 1)
              (LAM ((SBST::RIGHT EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::LBIND))) (VALUE-TO-SLOT V8 2)
              (LAM ((SBST::LBIND EPSILON)) (GOBBLE-TOKEN)))
             ((LA-MATCH ((SBST::RBIND))) (VALUE-TO-SLOT V8 3)
              (LAM ((SBST::RBIND EPSILON)) (GOBBLE-TOKEN)))
             (T
              (INITIAL-ERROR
               '((SBST::LEFT EPSILON) (SBST::RIGHT EPSILON)
                (SBST::LBIND EPSILON) (SBST::RBIND EPSILON)))))))
         ((LA-MATCH ((SBST::INITIAL))) (VALUE-TO-SLOT V7 0)
          (LAM ((SBST::INITIAL EPSILON)) (GOBBLE-TOKEN)))
         ((LA-MATCH ((SBST::AGGREGATE))) (VALUE-TO-SLOT V7 2)
          (LAM ((SBST::AGGREGATE EPSILON)) (GOBBLE-TOKEN)))
         (T
          (INITIAL-ERROR
           '((SBST::MEDIAL SBST::LEFT SBST::RIGHT SBST::LBIND SBST::RBIND)
            (SBST::INITIAL EPSILON) (SBST::AGGREGATE EPSILON)))))
        (AS-TO-SLOT V0
                    (CASE V7
                      (2 (MAKE-SB-TERM 'AGGREGATE (LIST V2)))
                      (1
                       (MAKE-SB-TERM 'MEDIAL
                                     (LIST V2
                                           (CASE V8
                                             (3 (MK-ID 'RBIND))
                                             (2 (MK-ID 'LBIND))
                                             (1 (MK-ID 'RIGHT))
                                             (0 (MK-ID 'LEFT))))))
                      (0 (MAKE-SB-TERM 'INITIAL (LIST V2)))))))
     (T
      (INITIAL-ERROR
       '((SBST::!KEYWORD! SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)
        (SBST::JUX SBST::AGGREGATE SBST::MEDIAL SBST::INITIAL SBST::^)))))
    NIL
    V0)) 


(DEFUN !SINGLE-LEVEL! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::!KEYWORD!) (SBST::JUX)))
      (PROGN
        (CODE-TO-SLOT V1 (!SINGLE-OP-PRECEDENCE! 0))
        (CONS-TO-SLOT V0 V1)
        (GEN-STAR-PARSE ((SBST::|,| SBST::!KEYWORD! SBST::JUX))
                        (PROGN
                          (LAM ((SBST::|,| EPSILON)) (GOBBLE-TOKEN))
                          (CODE-TO-SLOT V1 (!SINGLE-OP-PRECEDENCE! 0)))
                        V0 V1)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'PREC-LEVEL (DS-TEMP-REPT (CK-REPT-REF V0))))))
     (T
      (INITIAL-ERROR
       '((SBST::JUX SBST::^ SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)
        (SBST::!KEYWORD! SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)))))
    NIL
    V0)) 


(DEFUN !MULTIPLE-LEVELS! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::!KEYWORD!) (SBST::JUX)))
      (PROGN
        (CODE-TO-SLOT V1 (!SINGLE-LEVEL! 0))
        (CONS-TO-SLOT V0 V1)
        (GEN-STAR-PARSE
         ((SBST::!KEYWORD! SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE
           SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)
          (SBST::JUX SBST::^ SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE SBST::^
           SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE))
         (CODE-TO-SLOT V1 (!SINGLE-LEVEL! 0)) V0 V1)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'PREC-LEVELS (DS-TEMP-REPT (CK-REPT-REF V0))))))
     (T
      (INITIAL-ERROR
       '((SBST::JUX SBST::^ SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE SBST::^
          SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)
        (SBST::!KEYWORD! SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE
         SBST::INITIAL SBST::MEDIAL SBST::AGGREGATE)))))
    NIL
    V0)) 


(DEFUN !PRECEDENCE-INFORMATION! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
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
     ((LA-MATCH ((SBST::PRECEDENCE)))
      (PROGN
        (LAM ((SBST::PRECEDENCE EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::INFORMATION EPSILON))
                   (LAM ((SBST::INFORMATION EPSILON)) (GOBBLE-TOKEN)) V1)
        (PLUS-PARSE ((SBST::!ID! SBST::!KEYWORD! SBST::JUX))
                    (PROGN
                      (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V4))
                      (CODE-TO-SLOT V5 (!MULTIPLE-LEVELS! 0))
                      (AS-TO-SLOT V6 (MAKE-SB-TERM 'PREC-ENTRY (LIST V4 V5)))
                      (VALUE-TO-SLOT V3 V6))
                    V2 V3)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'PREC-ENTRIES (DS-TEMP-REPT (CK-REPT-REF V2))))))
     (T (INITIAL-ERROR '((SBST::PRECEDENCE SBST::!ID! SBST::INFORMATION)))))
    NIL
    V0)) 


(DEFUN !SPACING-INFORMATION! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
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
        V12
        V13
        V14
        V15
        V16)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14
                           V15 V16)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::SPACING)))
      (PROGN
        (LAM ((SBST::SPACING EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::INFORMATION EPSILON))
                   (LAM ((SBST::INFORMATION EPSILON)) (GOBBLE-TOKEN)) V1)
        (PLUS-PARSE
         ((SBST::JUX SBST::^ SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_)
          (SBST::ARB SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_)
          (SBST::LT SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_)
          (SBST::OP SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_)
          (SBST::!ID! SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_)
          (SBST::!KEYWORD! SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
           SBST::!KEYWORD! SBST::@^ SBST::@> SBST::@< SBST::!- SBST::!+ SBST::?
           SBST::/- SBST::/+ SBST::|#| SBST::_))
         (PROGN
           (ALT-PARSE
            ((LA-MATCH ((SBST::!KEYWORD!))) (VALUE-TO-SLOT V4 0)
             (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V5)))
            ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V4 1)
             (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V5)))
            ((LA-MATCH ((SBST::OP))) (VALUE-TO-SLOT V4 2)
             (LAM ((SBST::OP EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::LT))) (VALUE-TO-SLOT V4 3)
             (LAM ((SBST::LT EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::ARB))) (VALUE-TO-SLOT V4 4)
             (LAM ((SBST::ARB EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::JUX))) (VALUE-TO-SLOT V4 5)
             (PROGN
               (LAM ((SBST::JUX EPSILON)) (GOBBLE-TOKEN))
               (OPT-PARSE ((SBST::^ SBST::!ID! SBST::!NUMBER!))
                          (PROGN
                            (LAM ((SBST::^ EPSILON)) (GOBBLE-TOKEN))
                            (ALT-PARSE
                             ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V6 0)
                              (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V8)))
                             ((LA-MATCH ((SBST::!NUMBER!)))
                              (VALUE-TO-SLOT V6 1)
                              (LAM ((SBST::!NUMBER! EPSILON))
                                   (GOBBLE-TO-SLOT V8)))
                             (T
                              (INITIAL-ERROR
                               '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
                            (VALUE-TO-SLOT V7 V8))
                          V5)))
            (T
             (INITIAL-ERROR
              '((SBST::!KEYWORD! EPSILON) (SBST::!ID! EPSILON)
               (SBST::OP EPSILON) (SBST::LT EPSILON) (SBST::ARB EPSILON)
               (SBST::JUX EPSILON SBST::^)))))
           (STAR-PARSE
            ((SBST::@^ EPSILON) (SBST::@> EPSILON) (SBST::@< EPSILON)
             (SBST::!- EPSILON) (SBST::!+ SBST::!NUMBER! EPSILON)
             (SBST::? SBST::!ID! SBST::!NUMBER!) (SBST::/- SBST::!NUMBER!)
             (SBST::/+ SBST::!NUMBER!) (SBST::|#| EPSILON)
             (SBST::_ SBST::!NUMBER! EPSILON))
            (CODE-TO-SLOT V10 (!FORMAT-COMMAND! 0)) V9 V10)
           (ALT-PARSE
            ((LA-MATCH ((SBST::!KEYWORD!))) (VALUE-TO-SLOT V11 0)
             (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V12)))
            ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V11 1)
             (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V12)))
            ((LA-MATCH ((SBST::OP))) (VALUE-TO-SLOT V11 2)
             (LAM ((SBST::OP EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::LT))) (VALUE-TO-SLOT V11 3)
             (LAM ((SBST::LT EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::ARB))) (VALUE-TO-SLOT V11 4)
             (LAM ((SBST::ARB EPSILON)) (GOBBLE-TOKEN)))
            ((LA-MATCH ((SBST::JUX))) (VALUE-TO-SLOT V11 5)
             (PROGN
               (LAM ((SBST::JUX EPSILON)) (GOBBLE-TOKEN))
               (OPT-PARSE ((SBST::^ SBST::!ID! SBST::!NUMBER!))
                          (PROGN
                            (LAM ((SBST::^ EPSILON)) (GOBBLE-TOKEN))
                            (ALT-PARSE
                             ((LA-MATCH ((SBST::!ID!))) (VALUE-TO-SLOT V13 0)
                              (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V15)))
                             ((LA-MATCH ((SBST::!NUMBER!)))
                              (VALUE-TO-SLOT V13 1)
                              (LAM ((SBST::!NUMBER! EPSILON))
                                   (GOBBLE-TO-SLOT V15)))
                             (T
                              (INITIAL-ERROR
                               '((SBST::!ID! EPSILON) (SBST::!NUMBER! EPSILON)))))
                            (VALUE-TO-SLOT V14 V15))
                          V12)))
            (T
             (INITIAL-ERROR
              '((SBST::!KEYWORD! EPSILON) (SBST::!ID! EPSILON)
               (SBST::OP EPSILON) (SBST::LT EPSILON) (SBST::ARB EPSILON)
               (SBST::JUX EPSILON SBST::^)))))
           (AS-TO-SLOT V16
                       (MAKE-SB-TERM 'SPACE-COM
                                     (LIST
                                      (CASE V4
                                        ((1 0) V5)
                                        (5
                                         (MAKE-SB-TERM 'JUX-OP
                                                       (LIST
                                                        (IF (= V5 0)
                                                            (MAKE-SB-TERM 'NULL
                                                                      (LIST))
                                                            V7))))
                                        (4 (MK-ID 'ARB))
                                        (3 (MK-ID 'LT))
                                        (2 (MK-ID 'OP)))
                                      (MAKE-SB-TERM 'WS-SPECS
                                                    (DS-TEMP-REPT
                                                     (CK-REPT-REF V9)))
                                      (CASE V11
                                        ((1 0) V12)
                                        (5
                                         (MAKE-SB-TERM 'JUX-OP
                                                       (LIST
                                                        (IF (= V12 0)
                                                            (MAKE-SB-TERM 'NULL
                                                                      (LIST))
                                                            V14))))
                                        (4 (MK-ID 'ARB))
                                        (3 (MK-ID 'LT))
                                        (2 (MK-ID 'OP))))))
           (VALUE-TO-SLOT V3 V16))
         V2 V3)
        (AS-TO-SLOT V0 (MAKE-SB-TERM 'SPACING (DS-TEMP-REPT (CK-REPT-REF V2))))))
     (T
      (INITIAL-ERROR
       '((SBST::SPACING SBST::JUX SBST::ARB SBST::LT SBST::OP SBST::!ID!
          SBST::!KEYWORD! SBST::INFORMATION)))))
    NIL
    V0)) 


(DEFUN !BRACKETING-INFORMATION! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2
        V3
        V4
        V5
        V6
        V7)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::BRACKETING)))
      (PROGN
        (LAM ((SBST::BRACKETING EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::INFORMATION EPSILON))
                   (LAM ((SBST::INFORMATION EPSILON)) (GOBBLE-TOKEN)) V1)
        (PLUS-PARSE ((SBST::!ID! SBST::!KEYWORD!))
                    (PROGN
                      (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V4))
                      (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V5))
                      (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V6))
                      (AS-TO-SLOT V7
                                  (MAKE-SB-TERM 'BRACKET-ENTRY (LIST V4 V5 V6)))
                      (VALUE-TO-SLOT V3 V7))
                    V2 V3)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'BRACKET-ENTRIES
                                  (DS-TEMP-REPT (CK-REPT-REF V2))))))
     (T (INITIAL-ERROR '((SBST::BRACKETING SBST::!ID! SBST::INFORMATION)))))
    NIL
    V0)) 


(DEFUN !LEXICAL-TERMINALS! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
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
     ((LA-MATCH ((SBST::LEXICAL)))
      (PROGN
        (LAM ((SBST::LEXICAL EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::TERMINALS EPSILON)) (GOBBLE-TOKEN))
        (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON SBST::!KEYWORD!))
                          (PROGN
                            (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V3))
                            (OPT-PARSE ((SBST::!KEYWORD! EPSILON))
                                       (LAM ((SBST::!KEYWORD! EPSILON))
                                            (GOBBLE-TO-SLOT V5))
                                       V4)
                            (AS-TO-SLOT V6
                                        (IF (= V4 0) V3
                                            (MAKE-SB-TERM 'DELIMITER
                                                          (LIST V3 V5))))
                            (VALUE-TO-SLOT V2 V6))
                          V1 V2 'SBST::|,|)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'LEX-TERMS (DS-TEMP-REPT (CK-REPT-REF V1))))))
     (T (INITIAL-ERROR '((SBST::LEXICAL SBST::TERMINALS)))))
    NIL
    V0)) 


(DEFUN !OPERATOR-INFORMATION! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::OPERATORS)))
      (PROGN
        (LAM ((SBST::OPERATORS EPSILON)) (GOBBLE-TOKEN))
        (STAR-PARSE ((SBST::!KEYWORD! EPSILON))
                    (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V2)) V1 V2)
        (AS-TO-SLOT V0 (MAKE-SB-TERM 'OP-INFO (DS-TEMP-REPT (CK-REPT-REF V1))))))
     (T (INITIAL-ERROR '((SBST::OPERATORS EPSILON SBST::!KEYWORD!)))))
    NIL
    V0)) 


(DEFUN !ESCAPE-CHARACTER! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::ESCAPE)))
      (PROGN
        (LAM ((SBST::ESCAPE EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::CHARACTER EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V1))
        (AS-TO-SLOT V0 (MAKE-SB-TERM 'ESCAPE-CHARACTER (LIST V1)))))
     (T (INITIAL-ERROR '((SBST::ESCAPE SBST::CHARACTER)))))
    NIL
    V0)) 


(DEFUN !COMMENT-CHARACTER! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::COMMENT)))
      (PROGN
        (LAM ((SBST::COMMENT EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::CHARACTER EPSILON)) (GOBBLE-TOKEN))
        (OPT-PARSE ((SBST::NEWLINE SBST::!KEYWORD!))
                   (PROGN
                     (LAM ((SBST::NEWLINE EPSILON)) (GOBBLE-TOKEN))
                     (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V2)))
                   V1)
        (OPT-PARSE ((SBST::DELIMITED SBST::!KEYWORD!))
                   (PROGN
                     (LAM ((SBST::DELIMITED EPSILON)) (GOBBLE-TOKEN))
                     (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V4))
                     (LAM ((SBST::!KEYWORD! EPSILON)) (GOBBLE-TO-SLOT V5)))
                   V3)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'COMMENT-CHARACTER
                                  (LIST
                                   (IF (= V1 0) (MAKE-SB-TERM 'NULL (LIST)) V2)
                                   (IF (= V3 0) (MAKE-SB-TERM 'NULL (LIST)) V4)
                                   (IF (= V3 0) (MAKE-SB-TERM 'NULL (LIST)) V5))))))
     (T (INITIAL-ERROR '((SBST::COMMENT SBST::CHARACTER)))))
    NIL
    V0)) 


(DEFUN !EXTERNAL-GRAMMARS! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
  (DECLARE (IGNORE RBP))
  (DECLARE (IGNORE BRACKET-LIST))
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::EXTERNAL)))
      (PROGN
        (LAM ((SBST::EXTERNAL EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::GRAMMARS EPSILON)) (GOBBLE-TOKEN))
        (DOUBLEPLUS-PARSE ((SBST::!ID! EPSILON))
                          (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V2)) V1
                          V2 'SBST::|,|)
        (AS-TO-SLOT V0 (MAKE-SB-TERM 'EXT-GRAM (DS-TEMP-REPT (CK-REPT-REF V1))))))
     (T (INITIAL-ERROR '((SBST::EXTERNAL SBST::GRAMMARS)))))
    NIL
    V0)) 


(DEFUN !META-GRAMMAR! (RBP &OPTIONAL (BRACKET-LIST (EMPTY-BRACKET-LIST)))
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
        V12
        V13
        V14
        V15
        V16
        V17
        V18
        V19
        V20
        V21
        V22
        V23
        V24
        V25
        V26
        V27
        V28
        V29
        V30
        V31
        V32)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10 V11 V12 V13 V14
                           V15 V16 V17 V18 V19 V20 V21 V22 V23 V24 V25 V26 V27
                           V28 V29 V30 V31 V32)
    (INITIALS-ONLY
     ((LA-MATCH ((SBST::GRAMMAR)))
      (PROGN
        (LAM ((SBST::GRAMMAR EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::!ID! EPSILON)) (GOBBLE-TO-SLOT V4))
        (VALUE-TO-SLOT V1 1)
        (VALUE-TO-SLOT V3 V4)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (OPT-PARSE ((SBST::EXTERNAL SBST::GRAMMARS))
                   (CODE-TO-SLOT V7 (!EXTERNAL-GRAMMARS! 0)) V5)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (OPT-PARSE ((SBST::CASE SBST::SENSITIVE))
                   (PROGN
                     (LAM ((SBST::CASE EPSILON)) (GOBBLE-TOKEN))
                     (LAM ((SBST::SENSITIVE EPSILON)) (GOBBLE-TOKEN)))
                   V8)
        (OPT-PARSE ((SBST::COMMENT SBST::CHARACTER))
                   (CODE-TO-SLOT V11 (!COMMENT-CHARACTER! 0)) V9)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (OPT-PARSE ((SBST::ESCAPE SBST::CHARACTER))
                   (CODE-TO-SLOT V14 (!ESCAPE-CHARACTER! 0)) V12)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (OPT-PARSE ((SBST::OPERATORS SBST::!KEYWORD!))
                   (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0)) V15)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::EXTERNAL)))
      (PROGN
        (CODE-TO-SLOT V7 (!EXTERNAL-GRAMMARS! 0))
        (VALUE-TO-SLOT V5 1)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (OPT-PARSE ((SBST::CASE SBST::SENSITIVE))
                   (PROGN
                     (LAM ((SBST::CASE EPSILON)) (GOBBLE-TOKEN))
                     (LAM ((SBST::SENSITIVE EPSILON)) (GOBBLE-TOKEN)))
                   V8)
        (OPT-PARSE ((SBST::COMMENT SBST::CHARACTER))
                   (CODE-TO-SLOT V11 (!COMMENT-CHARACTER! 0)) V9)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (OPT-PARSE ((SBST::ESCAPE SBST::CHARACTER))
                   (CODE-TO-SLOT V14 (!ESCAPE-CHARACTER! 0)) V12)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (OPT-PARSE ((SBST::OPERATORS SBST::!KEYWORD!))
                   (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0)) V15)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::CASE)))
      (PROGN
        (LAM ((SBST::CASE EPSILON)) (GOBBLE-TOKEN))
        (LAM ((SBST::SENSITIVE EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V8 1)
        (OPT-PARSE ((SBST::COMMENT SBST::CHARACTER))
                   (CODE-TO-SLOT V11 (!COMMENT-CHARACTER! 0)) V9)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (OPT-PARSE ((SBST::ESCAPE SBST::CHARACTER))
                   (CODE-TO-SLOT V14 (!ESCAPE-CHARACTER! 0)) V12)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (OPT-PARSE ((SBST::OPERATORS SBST::!KEYWORD!))
                   (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0)) V15)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::COMMENT)))
      (PROGN
        (CODE-TO-SLOT V11 (!COMMENT-CHARACTER! 0))
        (VALUE-TO-SLOT V9 1)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (OPT-PARSE ((SBST::ESCAPE SBST::CHARACTER))
                   (CODE-TO-SLOT V14 (!ESCAPE-CHARACTER! 0)) V12)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (OPT-PARSE ((SBST::OPERATORS SBST::!KEYWORD!))
                   (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0)) V15)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::ESCAPE)))
      (PROGN
        (CODE-TO-SLOT V14 (!ESCAPE-CHARACTER! 0))
        (VALUE-TO-SLOT V12 1)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (OPT-PARSE ((SBST::OPERATORS SBST::!KEYWORD!))
                   (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0)) V15)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::OPERATORS)))
      (PROGN
        (CODE-TO-SLOT V17 (!OPERATOR-INFORMATION! 0))
        (VALUE-TO-SLOT V15 1)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (OPT-PARSE ((SBST::LEXICAL SBST::TERMINALS))
                   (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0)) V18)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::LEXICAL)))
      (PROGN
        (CODE-TO-SLOT V20 (!LEXICAL-TERMINALS! 0))
        (VALUE-TO-SLOT V18 1)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (OPT-PARSE ((SBST::BRACKETING SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0)) V21)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V15 0)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::BRACKETING)))
      (PROGN
        (CODE-TO-SLOT V23 (!BRACKETING-INFORMATION! 0))
        (VALUE-TO-SLOT V21 1)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (OPT-PARSE ((SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!))
                   (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0)) V24)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V18 0)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (VALUE-TO-SLOT V15 0)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::PRECEDENCE)))
      (PROGN
        (CODE-TO-SLOT V26 (!PRECEDENCE-INFORMATION! 0))
        (VALUE-TO-SLOT V24 1)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (OPT-PARSE
         ((SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
           SBST::LT SBST::ARB SBST::JUX))
         (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0)) V27)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V21 0)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (VALUE-TO-SLOT V18 0)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (VALUE-TO-SLOT V15 0)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::SPACING)))
      (PROGN
        (CODE-TO-SLOT V29 (!SPACING-INFORMATION! 0))
        (VALUE-TO-SLOT V27 1)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (PLUS-PARSE ((SBST::!ID! SBST::|::=|))
                    (PROGN
                      (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                      (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                      (VALUE-TO-SLOT V31 V32))
                    V30 V31)
        (VALUE-TO-SLOT V24 0)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (VALUE-TO-SLOT V21 0)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (VALUE-TO-SLOT V18 0)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (VALUE-TO-SLOT V15 0)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     ((LA-MATCH ((SBST::!ID!)))
      (PROGN
        (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
        (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
        (VALUE-TO-SLOT V31 V32)
        (CONS-TO-SLOT V30 V31)
        (GEN-STAR-PARSE ((SBST::!ID! SBST::|::=|))
                        (PROGN
                          (CODE-TO-SLOT V32 (!NONTERMINAL-DEFINITION! 0))
                          (LAM ((SBST::|;| EPSILON)) (GOBBLE-TOKEN))
                          (VALUE-TO-SLOT V31 V32))
                        V30 V31)
        (VALUE-TO-SLOT V27 0)
        (AS-TO-SLOT V28
                    (IF (= V27 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-9 NIL) V29))
        (VALUE-TO-SLOT V24 0)
        (AS-TO-SLOT V25
                    (IF (= V24 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-8 NIL) V26))
        (VALUE-TO-SLOT V21 0)
        (AS-TO-SLOT V22
                    (IF (= V21 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-7 NIL) V23))
        (VALUE-TO-SLOT V18 0)
        (AS-TO-SLOT V19
                    (IF (= V18 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-6 NIL) V20))
        (VALUE-TO-SLOT V15 0)
        (AS-TO-SLOT V16
                    (IF (= V15 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-5 NIL) V17))
        (VALUE-TO-SLOT V12 0)
        (AS-TO-SLOT V13
                    (IF (= V12 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-4 NIL) V14))
        (VALUE-TO-SLOT V9 0)
        (AS-TO-SLOT V10
                    (IF (= V9 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-3 NIL) V11))
        (VALUE-TO-SLOT V8 0)
        (VALUE-TO-SLOT V5 0)
        (AS-TO-SLOT V6 (IF (= V5 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-2 NIL) V7))
        (VALUE-TO-SLOT V1 0)
        (AS-TO-SLOT V2 (IF (= V1 0) (MAKE-SB-TERM 'META-GRAMMAR-NULL-1 NIL) V3))
        (AS-TO-SLOT V0
                    (MAKE-SB-TERM 'GRAMMAR
                                  (LIST V2 V6 V10 V16 V19 V22 V25 V28
                                        (MAKE-SB-TERM 'NTS
                                                      (DS-TEMP-REPT
                                                       (CK-REPT-REF V30)))
                                        (IF (= V8 0) (MK-ID 'NOCASE)
                                            (MK-ID 'CASE))
                                        V13)))))
     (T
      (INITIAL-ERROR
       '((SBST::!ID! SBST::|::=|)
        (SBST::SPACING SBST::INFORMATION SBST::!KEYWORD! SBST::!ID! SBST::OP
         SBST::LT SBST::ARB SBST::JUX)
        (SBST::PRECEDENCE SBST::INFORMATION SBST::!ID!)
        (SBST::BRACKETING SBST::INFORMATION SBST::!ID!)
        (SBST::LEXICAL SBST::TERMINALS) (SBST::OPERATORS SBST::!KEYWORD!)
        (SBST::ESCAPE SBST::CHARACTER) (SBST::COMMENT SBST::CHARACTER)
        (SBST::CASE SBST::SENSITIVE) (SBST::EXTERNAL SBST::GRAMMARS)
        (SBST::GRAMMAR SBST::!ID!)))))
    NIL
    V0)) 

