;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package "SYNTAX-BOX")  ;; creates package for abstract syntax. 

(in-package "SYNTAX-BOX")  ;; enters package for generated code.  

(use-package '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '( SB-UNPARSE  SB-WIN-UNPARSE ))

(DEFVAR SB-BRACKET-INFO (MAKE-HASH-TABLE :TEST #'EQ)) 
(CLRHASH SB-BRACKET-INFO) 
(MAPC
 #'(LAMBDA (ENTRY)
     (SET-BRACKET-INFO (CAR ENTRY) (CADR ENTRY) (CADDR ENTRY) SB-BRACKET-INFO))
 '((AUGMENT SBST::{ SBST::}))) 

(DEFVAR SB-PREC-INFO (MAKE-HASH-TABLE :TEST #'EQ)) 
(CLRHASH SB-PREC-INFO) 
(MAPC #'(LAMBDA (NT)
          (INIT-PREC-INFO NT SB-PREC-INFO)) '(PATTERN AUGMENT)) 
(MAPC
 #'(LAMBDA (ENTRY)
     (SET-PREC-INFO (CAR ENTRY) (CADR ENTRY) (CADDR ENTRY) (CADDDR ENTRY)
                    SB-PREC-INFO))
 '((PATTERN SBST::{ 0 10) (PATTERN SBST::[ 0 10) (PATTERN SBST::^ 1 80)
  (PATTERN JUX 1 70) (PATTERN SBST::++ 1 60) (PATTERN SBST::** 1 60)
  (PATTERN SBST::+ 1 60) (PATTERN SBST::* 1 60) (PATTERN SBST::< 1 50)
  (PATTERN SBST::<< 1 40) (PATTERN SBST::JUX 1 30) (PATTERN SBST::\| 1 20)
  (PATTERN SBST::^ 2 81) (PATTERN JUX 2 71) (PATTERN SBST::++ 2 61)
  (PATTERN SBST::** 2 61) (PATTERN SBST::+ 2 61) (PATTERN SBST::* 2 61)
  (PATTERN SBST::< 2 51) (PATTERN SBST::<< 2 41) (PATTERN SBST::JUX 2 31)
  (PATTERN SBST::\| 2 21) (AUGMENT SBST::[ 0 10) (AUGMENT SBST::^ 1 40)
  (AUGMENT SBST::+ 1 30) (AUGMENT SBST::* 1 30) (AUGMENT SBST::\| 1 20)
  (AUGMENT SBST::^ 2 41) (AUGMENT SBST::+ 2 31) (AUGMENT SBST::* 2 31)
  (AUGMENT SBST::\| 2 21))) 

(DEFVAR SB-SPACING-INFO (MAKE-HASH-TABLE :TEST #'EQ)) 
(CLRHASH SB-SPACING-INFO) 
(MAPC
 #'(LAMBDA (T1
            T2
            BP)
     (COND ((ASSOC T2 (GETHASH T1 SB-SPACING-INFO)))
           (T
            (SETF (GETHASH T1 SB-SPACING-INFO)
                  (CONS (LIST T2 BP) (GETHASH T1 SB-SPACING-INFO))))))
 '(SBST::|(| SBST::|)| SBST::[ SBST::] SBST::{ SBST::} SBST::< SBST::> SBST::<<
  SBST::|,| SBST::|;| SBST::\| SBST::^ SBST::@ SBST::|:| SBST::|::=| SBST::+
  SBST::++ SBST::* SBST::** SBST::!- SBST::!+ SBST::_ SBST::|#| SBST::?
  SBST::/+ SBST::/- SBST::@> SBST::@< SBST::@^ SBST::ID SBST::NUMBER
  SBST::STRING SBST::LITERAL SBST::KEYWORD META-GRAMMAR EXTERNAL-GRAMMARS
  COMMENT-CHARACTER ESCAPE-CHARACTER OPERATOR-INFORMATION LEXICAL-TERMINALS
  BRACKETING-INFORMATION SPACING-INFORMATION PRECEDENCE-INFORMATION
  MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE NONTERMINAL-DEFINITION
  PATTERN AUGMENT FORMAT-COMMAND SBST::|(| SBST::|)| SBST::[ SBST::] SBST::{
  SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;| SBST::\| SBST::^ SBST::@
  SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::* SBST::** SBST::!- SBST::!+
  SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/- SBST::@> SBST::@< SBST::@^
  SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL SBST::KEYWORD META-GRAMMAR
  EXTERNAL-GRAMMARS COMMENT-CHARACTER ESCAPE-CHARACTER OPERATOR-INFORMATION
  LEXICAL-TERMINALS BRACKETING-INFORMATION SPACING-INFORMATION
  PRECEDENCE-INFORMATION MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE
  NONTERMINAL-DEFINITION PATTERN AUGMENT FORMAT-COMMAND SBST::|(| SBST::|)|
  SBST::[ SBST::] SBST::{ SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;|
  SBST::\| SBST::^ SBST::@ SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::*
  SBST::** SBST::!- SBST::!+ SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/-
  SBST::@> SBST::@< SBST::@^ SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL
  SBST::KEYWORD META-GRAMMAR EXTERNAL-GRAMMARS COMMENT-CHARACTER
  ESCAPE-CHARACTER OPERATOR-INFORMATION LEXICAL-TERMINALS
  BRACKETING-INFORMATION SPACING-INFORMATION PRECEDENCE-INFORMATION
  MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE NONTERMINAL-DEFINITION
  PATTERN AUGMENT FORMAT-COMMAND SBST::|(| SBST::|)| SBST::[ SBST::] SBST::{
  SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;| SBST::\| SBST::^ SBST::@
  SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::* SBST::** SBST::!- SBST::!+
  SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/- SBST::@> SBST::@< SBST::@^
  SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL SBST::KEYWORD META-GRAMMAR
  EXTERNAL-GRAMMARS COMMENT-CHARACTER ESCAPE-CHARACTER OPERATOR-INFORMATION
  LEXICAL-TERMINALS BRACKETING-INFORMATION SPACING-INFORMATION
  PRECEDENCE-INFORMATION MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE
  NONTERMINAL-DEFINITION PATTERN AUGMENT FORMAT-COMMAND SBST::^ SBST::|(|
  SBST::|)| SBST::[ SBST::] SBST::{ SBST::} SBST::< SBST::> SBST::<< SBST::|,|
  SBST::|;| SBST::\| SBST::^ SBST::@ SBST::|:| SBST::|::=| SBST::+ SBST::++
  SBST::* SBST::** SBST::!- SBST::!+ SBST::_ SBST::|#| SBST::? SBST::/+
  SBST::/- SBST::@> SBST::@< SBST::@^ SBST::ID SBST::NUMBER SBST::STRING
  SBST::LITERAL SBST::KEYWORD META-GRAMMAR EXTERNAL-GRAMMARS COMMENT-CHARACTER
  ESCAPE-CHARACTER OPERATOR-INFORMATION LEXICAL-TERMINALS
  BRACKETING-INFORMATION SPACING-INFORMATION PRECEDENCE-INFORMATION
  MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE NONTERMINAL-DEFINITION
  PATTERN AUGMENT FORMAT-COMMAND SBST::< SBST::<< SBST::|(| SBST::|)| SBST::[
  SBST::] SBST::{ SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;| SBST::\|
  SBST::^ SBST::@ SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::* SBST::**
  SBST::!- SBST::!+ SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/- SBST::@>
  SBST::@< SBST::@^ SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL
  SBST::KEYWORD META-GRAMMAR EXTERNAL-GRAMMARS COMMENT-CHARACTER
  ESCAPE-CHARACTER OPERATOR-INFORMATION LEXICAL-TERMINALS
  BRACKETING-INFORMATION SPACING-INFORMATION PRECEDENCE-INFORMATION
  MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE NONTERMINAL-DEFINITION
  PATTERN AUGMENT FORMAT-COMMAND SBST::|(| SBST::|)| SBST::[ SBST::] SBST::{
  SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;| SBST::\| SBST::^ SBST::@
  SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::* SBST::** SBST::!- SBST::!+
  SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/- SBST::@> SBST::@< SBST::@^
  SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL SBST::KEYWORD META-GRAMMAR
  EXTERNAL-GRAMMARS COMMENT-CHARACTER ESCAPE-CHARACTER OPERATOR-INFORMATION
  LEXICAL-TERMINALS BRACKETING-INFORMATION SPACING-INFORMATION
  PRECEDENCE-INFORMATION MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE
  NONTERMINAL-DEFINITION PATTERN AUGMENT FORMAT-COMMAND SBST::|(| SBST::|)|
  SBST::[ SBST::] SBST::{ SBST::} SBST::< SBST::> SBST::<< SBST::|,| SBST::|;|
  SBST::\| SBST::^ SBST::@ SBST::|:| SBST::|::=| SBST::+ SBST::++ SBST::*
  SBST::** SBST::!- SBST::!+ SBST::_ SBST::|#| SBST::? SBST::/+ SBST::/-
  SBST::@> SBST::@< SBST::@^ SBST::ID SBST::NUMBER SBST::STRING SBST::LITERAL
  SBST::KEYWORD META-GRAMMAR EXTERNAL-GRAMMARS COMMENT-CHARACTER
  ESCAPE-CHARACTER OPERATOR-INFORMATION LEXICAL-TERMINALS
  BRACKETING-INFORMATION SPACING-INFORMATION PRECEDENCE-INFORMATION
  MULTIPLE-LEVELS SINGLE-LEVEL SINGLE-OP-PRECEDENCE NONTERMINAL-DEFINITION
  PATTERN AUGMENT FORMAT-COMMAND SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::|#| SBST::_ SBST::/+ SBST::/- SBST::? SBST::!+)
 '(SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+
  SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+
  SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+
  SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+
  SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+
  SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::+ SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::* SBST::*
  SBST::* SBST::* SBST::* SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::++
  SBST::++ SBST::++ SBST::++ SBST::++ SBST::++ SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::**
  SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** SBST::** :ARB
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^
  SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ SBST::^ :ARB :ARB SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::> SBST::>
  SBST::> SBST::> SBST::> SBST::> SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::< SBST::<
  SBST::< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<< SBST::<<
  SBST::<< SBST::<< SBST::<< SBST::<< SBST::> SBST::} SBST::|;| SBST::|:| :ARB
  SBST::|#| SBST::NUMBER SBST::NUMBER SBST::NUMBER :ARB SBST::NUMBER)
 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 0 0 0)) 

(DEFVAR SB-KEY-TOKEN-MAP (MAKE-HASH-TABLE :TEST #'EQ)) 
(DEFVAR SB-KEY-ESC-TOKEN-MAP (IF SB-ESCAPE-CHAR (MAKE-HASH-TABLE :TEST #'EQ))) 
(DEFVAR SB-LT-TOKEN-MAP (MAKE-HASH-TABLE :TEST #'EQUAL)) 
(DEFVAR SB-LT-ESC-TOKEN-MAP (IF SB-ESCAPE-CHAR (MAKE-HASH-TABLE :TEST #'EQUAL))) 


(DEFUN SB-UNPARSE (NT AS &KEY (STYLE *UNPARSE-STYLE*))
  (LET* ((*UNPARSER-OP-LIST* SB-ALL-OPERATORS-LIST)
         (*KEY-TOKEN-MAP* SB-KEY-TOKEN-MAP)
         (*KEY-ESC-TOKEN-MAP* SB-KEY-ESC-TOKEN-MAP)
         (*LT-TOKEN-MAP* SB-LT-TOKEN-MAP)
         (*LT-ESC-TOKEN-MAP* SB-LT-ESC-TOKEN-MAP)
         (*APPLY-LT-DIS-FUN* #'APPLY-LEXICAL-TERMINAL-DISCRIMINATOR)
         (*APPLY-LT-DES-FUN* #'APPLY-LEXICAL-TERMINAL-DESTRUCTOR)
         (*APPLY-LT-TOKEN-CONS-FUN* #'APPLY-LT-TOKEN-CONSTRUCTOR)
         (*BRACKET-INFO* SB-BRACKET-INFO)
         (*PREC-INFO* SB-PREC-INFO)
         (SORT (OPSIG-OUTPUT (OPSIG-TABLE-LOOKUP (TERM-OP AS))))
         (NT-NAME
          (COND (NT)
                ((IS-SORT-TTYPE SORT)
                 (DS-SORT-TTYPE SORT))
                (T
                 'META-GRAMMAR)))
         (*CASE-SENSITIVE* SB-CASE-SENSITIVE)
         (*ESCAPE-CHARACTER* SB-ESCAPE-CHAR)
         (*RESTRICTED-CHARS* SB-RESTRICTED-CHARS)
         (*STRING-CHAR* SB-STRING-CHAR)
         (*LITERAL-CHAR* SB-LITERAL-CHAR)
         (*KEYWORD-CHAR* SB-KEYWORD-CHAR)
         (*UNPARSE-STYLE*
          (IF (AND STYLE (NOT (CONSP STYLE))) (LIST STYLE) STYLE))
         (*NO-ESCAPES* (OR *NO-ESCAPES* (NULL (INSERT-ESCAPES?))))
         (*CURRENT-PRINT-DEPTH* 1))
    (LET ((UTERM
           (CASE NT-NAME
             (META-GRAMMAR (SB-UNP-META-GRAMMAR AS :TOP-LEVEL? T))
             (EXTERNAL-GRAMMARS (SB-UNP-EXTERNAL-GRAMMARS AS :TOP-LEVEL? T))
             (COMMENT-CHARACTER (SB-UNP-COMMENT-CHARACTER AS :TOP-LEVEL? T))
             (ESCAPE-CHARACTER (SB-UNP-ESCAPE-CHARACTER AS :TOP-LEVEL? T))
             (OPERATOR-INFORMATION
              (SB-UNP-OPERATOR-INFORMATION AS :TOP-LEVEL? T))
             (LEXICAL-TERMINALS (SB-UNP-LEXICAL-TERMINALS AS :TOP-LEVEL? T))
             (BRACKETING-INFORMATION
              (SB-UNP-BRACKETING-INFORMATION AS :TOP-LEVEL? T))
             (SPACING-INFORMATION (SB-UNP-SPACING-INFORMATION AS :TOP-LEVEL? T))
             (PRECEDENCE-INFORMATION
              (SB-UNP-PRECEDENCE-INFORMATION AS :TOP-LEVEL? T))
             (MULTIPLE-LEVELS (SB-UNP-MULTIPLE-LEVELS AS :TOP-LEVEL? T))
             (SINGLE-LEVEL (SB-UNP-SINGLE-LEVEL AS :TOP-LEVEL? T))
             (SINGLE-OP-PRECEDENCE
              (SB-UNP-SINGLE-OP-PRECEDENCE AS :TOP-LEVEL? T))
             (NONTERMINAL-DEFINITION
              (SB-UNP-NONTERMINAL-DEFINITION AS :TOP-LEVEL? T))
             (PATTERN (SB-UNP-PATTERN AS :TOP-LEVEL? T))
             (AUGMENT (SB-UNP-AUGMENT AS :TOP-LEVEL? T))
             (FORMAT-COMMAND (SB-UNP-FORMAT-COMMAND AS :TOP-LEVEL? T))
             (T (UNPARSE-RUNTIME-ERROR
		 "Nonterminal not unparsable:" NT-NAME NIL)
		NIL))))
      UTERM))) 

(DEFUN SB-WIN-UNPARSE (UTERM WIDTH &KEY (FONTWIDTH SB-DEFFONTWIDTH) (FONTHEIGHT SB-DEFFONTHEIGHT))
  (LET* ((*CASE-SENSITIVE* SB-CASE-SENSITIVE)
         (*UNPARSER-OP-LIST* SB-ALL-OPERATORS-LIST)
         (*SPACING-INFO* SB-SPACING-INFO))
    (FORMAT-UTERM UTERM WIDTH :FONTWIDTH FONTWIDTH :FONTHEIGHT FONTHEIGHT))) 



(DEFUN SB-UNP-META-GRAMMAR (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-META-GRAMMAR-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-EXTERNAL-GRAMMARS (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-EXTERNAL-GRAMMARS-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-COMMENT-CHARACTER (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-COMMENT-CHARACTER-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-ESCAPE-CHARACTER (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-ESCAPE-CHARACTER-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-OPERATOR-INFORMATION (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-OPERATOR-INFORMATION-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-LEXICAL-TERMINALS (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-LEXICAL-TERMINALS-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-BRACKETING-INFORMATION (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-BRACKETING-INFORMATION-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-SPACING-INFORMATION (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-SPACING-INFORMATION-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-PRECEDENCE-INFORMATION (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-PRECEDENCE-INFORMATION-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-MULTIPLE-LEVELS (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-MULTIPLE-LEVELS-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-SINGLE-LEVEL (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-SINGLE-LEVEL-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-SINGLE-OP-PRECEDENCE (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-SINGLE-OP-PRECEDENCE-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-NONTERMINAL-DEFINITION (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-NONTERMINAL-DEFINITION-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-PATTERN (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-PATTERN-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-AUGMENT (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-AUGMENT-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-FORMAT-COMMAND (AS &KEY (TOP-LEVEL? NIL))
  (MEMO-UTERM AS #'SB-UNP-FORMAT-COMMAND-AUX :TOP-LEVEL? TOP-LEVEL?)) 



(DEFUN SB-UNP-META-GRAMMAR-AUX (AS)
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
    (SETF V0 AS)
    (NT-UNP 'META-GRAMMAR AS
            ((UPATS
              ((:ELIDED :PATS :UPATS :AUGS)
               (UNP-UTERM V0
                          ((UNP-BIND 'SEQ_ V0) (UNP-TERM-CONST 'GRAMMAR)
                           (UNP-NAME 'OPT V2) (TOSS-NAME 'OPT) (TOSS-NAME 'OPT)
                           (TOSS-NAME 'OPT) (TOSS-NAME 'OPT) (TOSS-NAME 'OPT)
                           (TOSS-NAME 'OPT) (TOSS-NAME 'OPT) (UNP-LIST-CONST)
                           (UNP-NAME (QUOTE NIL) V4)
                           (DIS-OPT VNIL (LAMBDA (X)
                                           (MATCH-ID 'NOCASE X))
                                    (LAMBDA (X)
                                      (MATCH-ID 'CASE X)))
                           (UNP-OPT-AUG VNIL ((UNP-BASE-LIT)) ((UNP-BASE-LIT)))
                           (TOSS-NAME 'OPT)
                           (UNP-SEQ
                            (((UNP-KEYWORD 'SBST::GRAMMAR))
                             ((UNP-BIND 'OPT_0 V2)
                              (DIS-OPT V1
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-10 X)))
                                       (LAMBDA (X)
                                         (DIS-LT 'SBST::ID X)))
                              (UNP-OPT-AUG V1 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V3)))
                              (UNP-OPT V1 ((UNP-LT 'SBST::ID V3))))
                             ((UNP-KEYWORD 'SBST::|...|))
                             ((UNP-UTERM V4
                                         ((UNP-REPT V4
                                                    ((UNP-UTERM V5
                                                                ((UNP-BIND
                                                                  'SEQ_ V5)
                                                                 (UNP-NAME
                                                                  'NONTERMINAL-DEFINITION
                                                                  V6)
                                                                 (UNP-SEQ
                                                                  (((UNP-NT
                                                                     SB-UNP-NONTERMINAL-DEFINITION
                                                                     V6))
                                                                   ((UNP-KEYWORD
                                                                     'SBST::|;|)))
                                                                  ((MAKE-BP
                                                                    :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                   (MAKE-BP
                                                                    :VALUE
                                                                    -268435456
                                                                    :UNITED-FLAG
                                                                    NIL :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      0
                                                                      :FORMAT
                                                                      (LIST
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1)
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1))))))))
                                                    V5
                                                    (MAKE-BP :VALUE 0
                                                             :UNITED-FLAG NIL
                                                             :SPACES NIL :CRS
                                                             NIL :FORMAT (LIST)))))))
                            ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL
                                      :SPACES NIL :CRS 0 :FORMAT
                                      (LIST
                                       (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND
                                                   :CR :VALUE 1)
                                       (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND
                                                   :CR :VALUE 1)))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST)))))))
              (T
               (UNP-UTERM V0
                          ((UNP-BIND 'SEQ_ V0) (UNP-TERM-CONST 'GRAMMAR)
                           (UNP-NAME 'OPT V2) (UNP-NAME 'OPT V6)
                           (UNP-NAME 'OPT V10) (UNP-NAME 'OPT V16)
                           (UNP-NAME 'OPT V19) (UNP-NAME 'OPT V22)
                           (UNP-NAME 'OPT V25) (UNP-NAME 'OPT V28)
                           (UNP-LIST-CONST) (UNP-NAME (QUOTE NIL) V30)
                           (DIS-OPT V8 (LAMBDA (X)
                                         (MATCH-ID 'NOCASE X))
                                    (LAMBDA (X)
                                      (MATCH-ID 'CASE X)))
                           (UNP-OPT-AUG V8 ((UNP-BASE-LIT)) ((UNP-BASE-LIT)))
                           (UNP-NAME 'OPT V13)
                           (UNP-SEQ
                            (((UNP-BIND 'OPT_0 V2)
                              (DIS-OPT V1
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-1 X)))
                                       (LAMBDA (X)
                                         (DIS-LT 'SBST::ID X)))
                              (UNP-OPT-AUG V1 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V3)))
                              (UNP-OPT V1
                                       ((UNP-BIND 'SEQ_ V3) (UNP-NAME 'ID V4)
                                        (UNP-SEQ
                                         (((UNP-KEYWORD 'SBST::GRAMMAR))
                                          ((UNP-LT 'SBST::ID V4)))
                                         ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                   :SPACES NIL :CRS NIL :FORMAT
                                                   (LIST))
                                          (MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_1 V6)
                              (DIS-OPT V5
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-2 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-EXTERNAL-GRAMMARS X)))
                              (UNP-OPT-AUG V5 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V7)))
                              (UNP-OPT V5
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-EXTERNAL-GRAMMARS V7)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-OPT V8
                                       ((UNP-SEQ
                                         (((UNP-KEYWORD 'SBST::CASE))
                                          ((UNP-KEYWORD 'SBST::SENSITIVE)))
                                         ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                   :SPACES NIL :CRS NIL :FORMAT
                                                   (LIST))
                                          (MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_2 V10)
                              (DIS-OPT V9
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-3 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-COMMENT-CHARACTER X)))
                              (UNP-OPT-AUG V9 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V11)))
                              (UNP-OPT V9
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-COMMENT-CHARACTER V11)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_9 V13)
                              (DIS-OPT V12
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-4 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-ESCAPE-CHARACTER X)))
                              (UNP-OPT-AUG V12 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V14)))
                              (UNP-OPT V12
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-ESCAPE-CHARACTER V14)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_3 V16)
                              (DIS-OPT V15
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-5 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-OPERATOR-INFORMATION X)))
                              (UNP-OPT-AUG V15 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V17)))
                              (UNP-OPT V15
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-OPERATOR-INFORMATION
                                                   V17)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_4 V19)
                              (DIS-OPT V18
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-6 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-LEXICAL-TERMINALS X)))
                              (UNP-OPT-AUG V18 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V20)))
                              (UNP-OPT V18
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-LEXICAL-TERMINALS V20)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_5 V22)
                              (DIS-OPT V21
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-7 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-BRACKETING-INFORMATION X)))
                              (UNP-OPT-AUG V21 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V23)))
                              (UNP-OPT V21
                                       ((UNP-SEQ
                                         (((UNP-NT
                                            SB-UNP-BRACKETING-INFORMATION V23)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_6 V25)
                              (DIS-OPT V24
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-8 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-PRECEDENCE-INFORMATION X)))
                              (UNP-OPT-AUG V24 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V26)))
                              (UNP-OPT V24
                                       ((UNP-SEQ
                                         (((UNP-NT
                                            SB-UNP-PRECEDENCE-INFORMATION V26)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-BIND 'OPT_7 V28)
                              (DIS-OPT V27
                                       (LAMBDA (X)
                                         (AND (DIS-OP 'META-GRAMMAR-NULL-9 X)))
                                       (LAMBDA (X)
                                         (SB-DIS-SPACING-INFORMATION X)))
                              (UNP-OPT-AUG V27 ((UNP-NULL-CONST))
                                           ((UNP-NAME (QUOTE NIL) V29)))
                              (UNP-OPT V27
                                       ((UNP-SEQ
                                         (((UNP-NT SB-UNP-SPACING-INFORMATION
                                                   V29)))
                                         ((MAKE-BP :VALUE -268435456
                                                   :UNITED-FLAG NIL :SPACES NIL
                                                   :CRS 0 :FORMAT
                                                   (LIST
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1)
                                                    (MAKE-TOKEN :KIND
                                                                :WHITESPACE
                                                                :SUBKIND :CR
                                                                :VALUE 1))))))))
                             ((UNP-UTERM V30
                                         ((UNP-REPT V30
                                                    ((UNP-UTERM V31
                                                                ((UNP-BIND
                                                                  'SEQ_ V31)
                                                                 (UNP-NAME
                                                                  'NONTERMINAL-DEFINITION
                                                                  V32)
                                                                 (UNP-SEQ
                                                                  (((UNP-NT
                                                                     SB-UNP-NONTERMINAL-DEFINITION
                                                                     V32))
                                                                   ((UNP-KEYWORD
                                                                     'SBST::|;|)))
                                                                  ((MAKE-BP
                                                                    :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                   (MAKE-BP
                                                                    :VALUE
                                                                    -268435456
                                                                    :UNITED-FLAG
                                                                    NIL :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      0
                                                                      :FORMAT
                                                                      (LIST
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1)
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1))))))))
                                                    V31
                                                    (MAKE-BP :VALUE 0
                                                             :UNITED-FLAG NIL
                                                             :SPACES NIL :CRS
                                                             NIL :FORMAT (LIST)))))))
                            ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST))
                             (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL
                                      :SPACES NIL :CRS 0 :FORMAT
                                      (LIST
                                       (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND
                                                   :CR :VALUE 1)))
                             (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                      :CRS NIL :FORMAT (LIST)))))))))))) 



(DEFUN SB-UNP-EXTERNAL-GRAMMARS-AUX (AS)
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (SETF V0 AS)
    (NT-UNP 'EXTERNAL-GRAMMARS AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME 'DOUBLEPLUS V1)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::EXTERNAL))
                           ((UNP-KEYWORD 'SBST::GRAMMARS))
                           ((UNP-UTERM V1
                                       ((UNP-DOUBLE-REPT V1
                                                         ((UNP-UTERM V2
                                                                     ((UNP-LT
                                                                      'SBST::ID
                                                                      V2))))
                                                         ((UNP-KEYWORD
                                                           'SBST::|,|))
                                                         V2
                                                         (MAKE-BP :VALUE 2
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                  NIL :CRS NIL
                                                                  :FORMAT
                                                                  (LIST))
                                                         (MAKE-BP :VALUE 0
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                  NIL :CRS NIL
                                                                  :FORMAT
                                                                  (LIST)))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-COMMENT-CHARACTER-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5)
    (SETF V0 AS)
    (NT-UNP 'COMMENT-CHARACTER AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0)
                         (UNP-TERM-CONST 'COMMENT-CHARACTER)
                         (DIS-OPT V1 (LAMBDA (X)
                                       (AND (DIS-OP 'NULL X)))
                                  (LAMBDA (X)
                                    (DIS-LT 'SBST::KEYWORD X)))
                         (UNP-OPT-AUG V1 ((UNP-TERM-CONST 'NULL))
                                      ((UNP-NAME 'KEYWORD V2)))
                         (DIS-OPT V3 (LAMBDA (X)
                                       (AND (DIS-OP 'NULL X)))
                                  (LAMBDA (X)
                                    (DIS-LT 'SBST::KEYWORD X)))
                         (UNP-OPT-AUG V3 ((UNP-TERM-CONST 'NULL))
                                      ((UNP-NAME 'KEYWORD V4)))
                         (UNP-OPT-AUG V3 ((UNP-TERM-CONST 'NULL))
                                      ((UNP-NAME 'KEYWORD V5)))
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::COMMENT))
                           ((UNP-KEYWORD 'SBST::CHARACTER))
                           ((UNP-OPT V1
                                     ((UNP-SEQ
                                       (((UNP-KEYWORD 'SBST::NEWLINE))
                                        ((UNP-LT 'SBST::KEYWORD V2)))
                                       ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                 :SPACES NIL :CRS NIL :FORMAT
                                                 (LIST))
                                        (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                 :SPACES NIL :CRS NIL :FORMAT
                                                 (LIST)))))))
                           ((UNP-OPT V3
                                     ((UNP-SEQ
                                       (((UNP-KEYWORD 'SBST::DELIMITED))
                                        ((UNP-LT 'SBST::KEYWORD V4))
                                        ((UNP-LT 'SBST::KEYWORD V5)))
                                       ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                 :SPACES NIL :CRS NIL :FORMAT
                                                 (LIST))
                                        (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                 :SPACES NIL :CRS NIL :FORMAT
                                                 (LIST))
                                        (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                 :SPACES NIL :CRS NIL :FORMAT
                                                 (LIST))))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-ESCAPE-CHARACTER-AUX (AS)
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (SETF V0 AS)
    (NT-UNP 'ESCAPE-CHARACTER AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-TERM-CONST 'ESCAPE-CHARACTER)
                         (UNP-NAME 'KEYWORD V1)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::ESCAPE))
                           ((UNP-KEYWORD 'SBST::CHARACTER))
                           ((UNP-LT 'SBST::KEYWORD V1)))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-OPERATOR-INFORMATION-AUX (AS)
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (SETF V0 AS)
    (NT-UNP 'OPERATOR-INFORMATION AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME 'STAR V1)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::OPERATORS))
                           ((UNP-UTERM V1
                                       ((UNP-REPT V1
                                                  ((UNP-UTERM V2
                                                              ((UNP-LT
                                                                'SBST::KEYWORD
                                                                V2))))
                                                  V2
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                          ((MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL :SPACES
                                    NIL :CRS 0 :FORMAT
                                    (LIST
                                     (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND :CR
                                                 :VALUE 1)))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-LEXICAL-TERMINALS-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4
        V5
        V6)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6)
    (SETF V0 AS)
    (NT-UNP 'LEXICAL-TERMINALS AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME (QUOTE NIL) V1)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::LEXICAL))
                           ((UNP-KEYWORD 'SBST::TERMINALS))
                           ((UNP-UTERM V1
                                       ((UNP-DOUBLE-REPT V1
                                                         ((UNP-UTERM V2
                                                                     ((UNP-BIND
                                                                      'SEQ_ V2)
                                                                      (UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V6)
                                                                      (UNP-BIND
                                                                      '#:G23695
                                                                      V6)
                                                                      (DIS-OPT
                                                                      V4
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (AND
                                                                      (DIS-OP
                                                                      'DELIMITER
                                                                      X))))
                                                                      (UNP-OPT-AUG
                                                                      V4
                                                                      ((UNP-NAME
                                                                      'ID V3))
                                                                      ((UNP-TERM-CONST
                                                                      'DELIMITER)
                                                                      (UNP-NAME
                                                                      'ID V3)
                                                                      (UNP-NAME
                                                                      'KEYWORD
                                                                      V5)))
                                                                      (UNP-SEQ
                                                                      (((UNP-LT
                                                                      'SBST::ID
                                                                      V3))
                                                                      ((UNP-OPT
                                                                      V4
                                                                      ((UNP-LT
                                                                      'SBST::KEYWORD
                                                                      V5)))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))
                                                         ((UNP-KEYWORD
                                                           'SBST::|,|))
                                                         V2
                                                         (MAKE-BP :VALUE 2
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                  NIL :CRS NIL
                                                                  :FORMAT
                                                                  (LIST))
                                                         (MAKE-BP :VALUE 0
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                  NIL :CRS NIL
                                                                  :FORMAT
                                                                  (LIST)))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL :SPACES
                                    NIL :CRS 0 :FORMAT
                                    (LIST
                                     (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND :CR
                                                 :VALUE 1)))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-BRACKETING-INFORMATION-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4
        V5
        V6
        V7)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7)
    (SETF V0 AS)
    (NT-UNP 'BRACKETING-INFORMATION AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME (QUOTE NIL) V2)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::BRACKETING))
                           ((UNP-OPT V1 ((UNP-KEYWORD 'SBST::INFORMATION))))
                           ((UNP-UTERM V2
                                       ((UNP-REPT V2
                                                  ((UNP-UTERM V3
                                                              ((UNP-BIND 'SEQ_
                                                                      V3)
                                                               (UNP-NAME
                                                                (QUOTE NIL) V7)
                                                               (UNP-BIND
                                                                '#:G23697 V7)
                                                               (UNP-TERM-CONST
                                                                'BRACKET-ENTRY)
                                                               (UNP-NAME 'ID V4)
                                                               (UNP-NAME
                                                                'KEYWORD V5)
                                                               (UNP-NAME
                                                                'KEYWORD V6)
                                                               (UNP-SEQ
                                                                (((UNP-LT
                                                                   'SBST::ID V4))
                                                                 ((UNP-LT
                                                                   'SBST::KEYWORD
                                                                   V5))
                                                                 ((UNP-LT
                                                                   'SBST::KEYWORD
                                                                   V6)))
                                                                ((MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                 (MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                 (MAKE-BP
                                                                  :VALUE
                                                                  -268435456
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      0
                                                                      :FORMAT
                                                                      (LIST
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1))))))))
                                                  V3
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL :SPACES
                                    NIL :CRS 0 :FORMAT
                                    (LIST
                                     (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND :CR
                                                 :VALUE 1)))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-SPACING-INFORMATION-AUX (AS)
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
    (SETF V0 AS)
    (NT-UNP 'SPACING-INFORMATION AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME (QUOTE NIL) V2)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::SPACING))
                           ((UNP-OPT V1 ((UNP-KEYWORD 'SBST::INFORMATION))))
                           ((UNP-UTERM V2
                                       ((UNP-REPT V2
                                                  ((UNP-UTERM V3
                                                              ((UNP-BIND 'SEQ_
                                                                      V3)
                                                               (UNP-NAME
                                                                (QUOTE NIL) V16)
                                                               (UNP-BIND
                                                                '#:G23699 V16)
                                                               (UNP-TERM-CONST
                                                                'SPACE-COM)
                                                               (DIS-ALT V4
                                                                      ((LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::KEYWORD
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'OP
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'LT
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'ARB
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (AND
                                                                      (DIS-OP
                                                                      'JUX-OP
                                                                      X)))))
                                                               (UNP-ALT-AUG V4
                                                                      (((UNP-NAME
                                                                      'KEYWORD
                                                                      V5))
                                                                      ((UNP-NAME
                                                                      'ID
                                                                      V5))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-TERM-CONST
                                                                      'JUX-OP)
                                                                      (DIS-OPT
                                                                      V5
                                                                      (LAMBDA (X)
                                                                      (AND
                                                                      (DIS-OP
                                                                      'NULL
                                                                      X)))
                                                                      (LAMBDA (X)
                                                                      (OR
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X)
                                                                      (DIS-LT
                                                                      'SBST::NUMBER
                                                                      X))))
                                                                      (UNP-OPT-AUG
                                                                      V5
                                                                      ((UNP-TERM-CONST
                                                                      'NULL))
                                                                      ((UNP-NAME
                                                                      'ALT
                                                                      V7))))))
                                                               (UNP-LIST-CONST)
                                                               (UNP-NAME 'STAR
                                                                      V9)
                                                               (DIS-ALT V11
                                                                      ((LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::KEYWORD
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'OP
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'LT
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (MATCH-ID
                                                                      'ARB
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (AND
                                                                      (DIS-OP
                                                                      'JUX-OP
                                                                      X)))))
                                                               (UNP-ALT-AUG V11
                                                                      (((UNP-NAME
                                                                      'KEYWORD
                                                                      V12))
                                                                      ((UNP-NAME
                                                                      'ID
                                                                      V12))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-BASE-LIT))
                                                                      ((UNP-TERM-CONST
                                                                      'JUX-OP)
                                                                      (DIS-OPT
                                                                      V12
                                                                      (LAMBDA (X)
                                                                      (AND
                                                                      (DIS-OP
                                                                      'NULL
                                                                      X)))
                                                                      (LAMBDA (X)
                                                                      (OR
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X)
                                                                      (DIS-LT
                                                                      'SBST::NUMBER
                                                                      X))))
                                                                      (UNP-OPT-AUG
                                                                      V12
                                                                      ((UNP-TERM-CONST
                                                                      'NULL))
                                                                      ((UNP-NAME
                                                                      'ALT
                                                                      V14))))))
                                                               (UNP-SEQ
                                                                (((UNP-ALT V4
                                                                      (((UNP-LT
                                                                      'SBST::KEYWORD
                                                                      V5))
                                                                      ((UNP-LT
                                                                      'SBST::ID
                                                                      V5))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::OP))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::LT))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::ARB))
                                                                      ((UNP-SEQ
                                                                      (((UNP-KEYWORD
                                                                      'SBST::JUX))
                                                                      ((UNP-OPT
                                                                      V5
                                                                      ((UNP-SEQ
                                                                      (((UNP-KEYWORD
                                                                      'SBST::^))
                                                                      ((UNP-BIND
                                                                      'ALT_TAG
                                                                      V7)
                                                                      (DIS-ALT
                                                                      V6
                                                                      ((LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::NUMBER
                                                                      X))))
                                                                      (UNP-ALT-AUG
                                                                      V6
                                                                      (((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V8))
                                                                      ((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V8))))
                                                                      (UNP-ALT
                                                                      V6
                                                                      (((UNP-LT
                                                                      'SBST::ID
                                                                      V8))
                                                                      ((UNP-LT
                                                                      'SBST::NUMBER
                                                                      V8))))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))))
                                                                 ((UNP-UTERM V9
                                                                      ((UNP-REPT
                                                                      V9
                                                                      ((UNP-UTERM
                                                                      V10
                                                                      ((UNP-NT
                                                                      SB-UNP-FORMAT-COMMAND
                                                                      V10))))
                                                                      V10
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))
                                                                 ((UNP-ALT V11
                                                                      (((UNP-LT
                                                                      'SBST::KEYWORD
                                                                      V12))
                                                                      ((UNP-LT
                                                                      'SBST::ID
                                                                      V12))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::OP))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::LT))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::ARB))
                                                                      ((UNP-SEQ
                                                                      (((UNP-KEYWORD
                                                                      'SBST::JUX))
                                                                      ((UNP-OPT
                                                                      V12
                                                                      ((UNP-SEQ
                                                                      (((UNP-KEYWORD
                                                                      'SBST::^))
                                                                      ((UNP-BIND
                                                                      'ALT_TAG
                                                                      V14)
                                                                      (DIS-ALT
                                                                      V13
                                                                      ((LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::NUMBER
                                                                      X))))
                                                                      (UNP-ALT-AUG
                                                                      V13
                                                                      (((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V15))
                                                                      ((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V15))))
                                                                      (UNP-ALT
                                                                      V13
                                                                      (((UNP-LT
                                                                      'SBST::ID
                                                                      V15))
                                                                      ((UNP-LT
                                                                      'SBST::NUMBER
                                                                      V15))))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))))
                                                                ((MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                 (MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                 (MAKE-BP
                                                                  :VALUE
                                                                  -268435456
                                                                  :UNITED-FLAG
                                                                  NIL :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      0
                                                                      :FORMAT
                                                                      (LIST
                                                                      (MAKE-TOKEN
                                                                      :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1))))))))
                                                  V3
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL :SPACES
                                    NIL :CRS 0 :FORMAT
                                    (LIST
                                     (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND :CR
                                                 :VALUE 1)))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-PRECEDENCE-INFORMATION-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4
        V5
        V6)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6)
    (SETF V0 AS)
    (NT-UNP 'PRECEDENCE-INFORMATION AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-LIST-CONST)
                         (UNP-NAME (QUOTE NIL) V2)
                         (UNP-SEQ
                          (((UNP-KEYWORD 'SBST::PRECEDENCE))
                           ((UNP-OPT V1 ((UNP-KEYWORD 'SBST::INFORMATION))))
                           ((UNP-UTERM V2
                                       ((UNP-REPT V2
                                                  ((UNP-UTERM V3
                                                              ((UNP-BIND 'SEQ_
                                                                      V3)
                                                               (UNP-NAME
                                                                (QUOTE NIL) V6)
                                                               (UNP-BIND
                                                                '#:G23701 V6)
                                                               (UNP-TERM-CONST
                                                                'PREC-ENTRY)
                                                               (UNP-NAME 'ID V4)
                                                               (UNP-NAME
                                                                'MULTIPLE-LEVELS
                                                                V5)
                                                               (UNP-SEQ
                                                                (((UNP-LT
                                                                   'SBST::ID V4))
                                                                 ((UNP-NT
                                                                   SB-UNP-MULTIPLE-LEVELS
                                                                   V5)))
                                                                ((MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                 (MAKE-BP
                                                                  :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))
                                                  V3
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL :SPACES
                                    NIL :CRS 0 :FORMAT
                                    (LIST
                                     (MAKE-TOKEN :KIND :WHITESPACE :SUBKIND :CR
                                                 :VALUE 1)))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-MULTIPLE-LEVELS-AUX (AS)
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (SETF V0 AS)
    (NT-UNP 'MULTIPLE-LEVELS AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'PLUS_ V0) (UNP-LIST-CONST)
                         (UNP-NAME 'PLUS V0)
                         (UNP-REPT V0
                                   ((UNP-UTERM V1
                                               ((UNP-SEQ
                                                 (((UNP-NT SB-UNP-SINGLE-LEVEL
                                                           V1)))
                                                 ((MAKE-BP :VALUE -268435456
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS 0
                                                           :FORMAT
                                                           (LIST
                                                            (MAKE-TOKEN :KIND
                                                                      :WHITESPACE
                                                                      :SUBKIND
                                                                      :CR
                                                                      :VALUE
                                                                      1))))))))
                                   V1
                                   (MAKE-BP :VALUE -268435456 :UNITED-FLAG NIL
                                            :SPACES NIL :CRS 0 :FORMAT
                                            (LIST
                                             (MAKE-TOKEN :KIND :WHITESPACE
                                                         :SUBKIND :CR :VALUE 1)))))))))) 



(DEFUN SB-UNP-SINGLE-LEVEL-AUX (AS)
  (LET (V0
        V1)
    (ERGO-IGNORE-IF-UNUSED V0 V1)
    (SETF V0 AS)
    (NT-UNP 'SINGLE-LEVEL AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'DOUBLEPLUS_ V0) (UNP-LIST-CONST)
                         (UNP-NAME 'DOUBLEPLUS V0)
                         (UNP-DOUBLE-REPT V0
                                          ((UNP-UTERM V1
                                                      ((UNP-NT
                                                        SB-UNP-SINGLE-OP-PRECEDENCE
                                                        V1))))
                                          ((UNP-KEYWORD 'SBST::|,|)) V1
                                          (MAKE-BP :VALUE 2 :UNITED-FLAG NIL
                                                   :SPACES NIL :CRS NIL :FORMAT
                                                   (LIST))
                                          (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                   :SPACES NIL :CRS NIL :FORMAT
                                                   (LIST))))))))) 



(DEFUN SB-UNP-SINGLE-OP-PRECEDENCE-AUX (AS)
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
    (SETF V0 AS)
    (NT-UNP 'SINGLE-OP-PRECEDENCE AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0)
                         (DIS-ALT V7
                                  ((LAMBDA (X)
                                     (AND (DIS-OP 'INITIAL X)))
                                   (LAMBDA (X)
                                     (AND (DIS-OP 'MEDIAL X)))
                                   (LAMBDA (X)
                                     (AND (DIS-OP 'AGGREGATE X)))))
                         (UNP-ALT-AUG V7
                                      (((UNP-TERM-CONST 'INITIAL)
                                        (UNP-NAME 'ALT V2))
                                       ((UNP-TERM-CONST 'MEDIAL)
                                        (UNP-NAME 'ALT V2)
                                        (DIS-ALT V8
                                                 ((LAMBDA (X)
                                                    (MATCH-ID 'LEFT X))
                                                  (LAMBDA (X)
                                                    (MATCH-ID 'RIGHT X))
                                                  (LAMBDA (X)
                                                    (MATCH-ID 'LBIND X))
                                                  (LAMBDA (X)
                                                    (MATCH-ID 'RBIND X))))
                                        (UNP-ALT-AUG V8
                                                     (((UNP-BASE-LIT))
                                                      ((UNP-BASE-LIT))
                                                      ((UNP-BASE-LIT))
                                                      ((UNP-BASE-LIT)))))
                                       ((UNP-TERM-CONST 'AGGREGATE)
                                        (UNP-NAME 'ALT V2))))
                         (UNP-SEQ
                          (((UNP-UTERM V2
                                       ((UNP-BIND 'ALT_KEY V2)
                                        (DIS-ALT V1
                                                 ((LAMBDA (X)
                                                    (AND (DIS-OP 'KEYWORD-OP X)))
                                                  (LAMBDA (X)
                                                    (AND (DIS-OP 'JUX-OP X)))))
                                        (UNP-ALT-AUG V1
                                                     (((UNP-TERM-CONST
                                                        'KEYWORD-OP)
                                                       (UNP-NAME 'KEYWORD V3))
                                                      ((UNP-TERM-CONST 'JUX-OP)
                                                       (DIS-OPT V3
                                                                (LAMBDA (X)
                                                                  (AND
                                                                   (DIS-OP
                                                                    'NULL X)))
                                                                (LAMBDA (X)
                                                                  (OR
                                                                   (DIS-LT
                                                                    'SBST::ID X)
                                                                   (DIS-LT
                                                                    'SBST::NUMBER
                                                                    X))))
                                                       (UNP-OPT-AUG V3
                                                                    ((UNP-TERM-CONST
                                                                      'NULL))
                                                                    ((UNP-NAME
                                                                      'ALT V5))))))
                                        (UNP-ALT V1
                                                 (((UNP-LT 'SBST::KEYWORD V3))
                                                  ((UNP-SEQ
                                                    (((UNP-KEYWORD 'SBST::JUX))
                                                     ((UNP-OPT V3
                                                               ((UNP-SEQ
                                                                 (((UNP-KEYWORD
                                                                    'SBST::^))
                                                                  ((UNP-BIND
                                                                    'ALT_TAG V5)
                                                                   (DIS-ALT V4
                                                                      ((LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::ID
                                                                      X))
                                                                      (LAMBDA (X)
                                                                      (DIS-LT
                                                                      'SBST::NUMBER
                                                                      X))))
                                                                   (UNP-ALT-AUG
                                                                    V4
                                                                    (((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V6))
                                                                     ((UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V6))))
                                                                   (UNP-ALT V4
                                                                      (((UNP-LT
                                                                      'SBST::ID
                                                                      V6))
                                                                      ((UNP-LT
                                                                      'SBST::NUMBER
                                                                      V6))))))
                                                                 ((MAKE-BP
                                                                   :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                  (MAKE-BP
                                                                   :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))))
                                                    ((MAKE-BP :VALUE 0
                                                              :UNITED-FLAG NIL
                                                              :SPACES NIL :CRS
                                                              NIL :FORMAT
                                                              (LIST))
                                                     (MAKE-BP :VALUE 0
                                                              :UNITED-FLAG NIL
                                                              :SPACES NIL :CRS
                                                              NIL :FORMAT
                                                              (LIST))))))))))
                           ((UNP-ALT V7
                                     (((UNP-KEYWORD 'SBST::INITIAL))
                                      ((UNP-SEQ
                                        (((UNP-KEYWORD 'SBST::MEDIAL))
                                         ((UNP-ALT V8
                                                   (((UNP-KEYWORD 'SBST::LEFT))
                                                    ((UNP-KEYWORD 'SBST::RIGHT))
                                                    ((UNP-KEYWORD 'SBST::LBIND))
                                                    ((UNP-KEYWORD 'SBST::RBIND))))))
                                        ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                  :SPACES NIL :CRS NIL :FORMAT
                                                  (LIST))
                                         (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                  :SPACES NIL :CRS NIL :FORMAT
                                                  (LIST)))))
                                      ((UNP-KEYWORD 'SBST::AGGREGATE))))))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-NONTERMINAL-DEFINITION-AUX (AS)
  (LET (V0
        V1
        V2)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2)
    (SETF V0 AS)
    (NT-UNP 'NONTERMINAL-DEFINITION AS
            ((UNP-UTERM V0
                        ((UNP-BIND 'SEQ_ V0) (UNP-TERM-CONST 'NT-DEF)
                         (UNP-NAME 'ID V1) (UNP-NAME 'PATTERN V2)
                         (UNP-SEQ
                          (((UNP-LT 'SBST::ID V1)) ((UNP-KEYWORD 'SBST::|::=|))
                           ((UNP-NT SB-UNP-PATTERN V2)))
                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-PATTERN-AUX (AS)
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
        V10)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (SETF V0 AS)
    (NT-UNP 'PATTERN AS
            ((UNP-BIND 'ALT_ V0)
             (DIS-ALT V0
                      ((LAMBDA (X)
                         (AND (DIS-OP 'NONTERMINAL X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'EXT-NONTERMINAL X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'UKEYWORD X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'JUX X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'OPT X)
                              (AND (DIS-OP 'SEQ (TERM-ARGN X 0)))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'SEQ X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'ALT X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'STAR X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'PLUS X)))
                       (LAMBDA (X)
                         (OR
                          (AND (DIS-OP 'DOUBLEPLUS X)
                               (AND (DIS-OP 'UKEYWORD (TERM-ARGN X 1))))
                          (AND (DIS-OP 'DOUBLEPLUS X)
                               (AND (DIS-OP 'FORMAT (TERM-ARGN X 1))
                                    (AND
                                     (DIS-OP 'UKEYWORD
                                             (TERM-ARGN (TERM-ARGN X 1) 0)))
                                    (AND
                                     (DIS-OP 'WS-SPECS
                                             (TERM-ARGN (TERM-ARGN X 1) 1)))))))
                       (LAMBDA (X)
                         (OR
                          (AND (DIS-OP 'DOUBLESTAR X)
                               (AND (DIS-OP 'UKEYWORD (TERM-ARGN X 1))))
                          (AND (DIS-OP 'DOUBLESTAR X)
                               (AND (DIS-OP 'FORMAT (TERM-ARGN X 1))
                                    (AND
                                     (DIS-OP 'UKEYWORD
                                             (TERM-ARGN (TERM-ARGN X 1) 0)))
                                    (AND
                                     (DIS-OP 'WS-SPECS
                                             (TERM-ARGN (TERM-ARGN X 1) 1)))))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'TAG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'FORMAT X)
                              (AND (DIS-OP 'WS-SPECS (TERM-ARGN X 1)))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'AUGMENT X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'UPATTERN X)
                              (AND (DIS-OP 'UPATS (TERM-ARGN X 1)))))))
             (UNP-NAME (QUOTE NIL) V1)
             (UNP-ALT V0
                      (((SB-UNP-PATTERN-AUX-ALT-1 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-2 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-3 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-4 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-5 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-6 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-7 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-8 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-9 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-10 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-11 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-12 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-13 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-14 V0 V1))
                       ((SB-UNP-PATTERN-AUX-ALT-15 V0 V1)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-1 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'NONTERMINAL)
                (UNP-NAME 'ID V1) (UNP-LT 'SBST::ID V1))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-2 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'EXT-NONTERMINAL)
                (UNP-NAME 'ID V2) (UNP-NAME 'ID V3)
                (UNP-SEQ
                 (((UNP-LT 'SBST::ID V2)) ((UNP-KEYWORD 'SBST::@))
                  ((UNP-LT 'SBST::ID V3)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-3 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'UKEYWORD)
                (UNP-NAME 'KEYWORD V1) (UNP-LT 'SBST::KEYWORD V1))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-4 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'JUX)
                (UNP-NAME 'PATTERN V2) (UNP-NAME 'PATTERN V3)
                (DIS-OPT V4
                         (LAMBDA (X)
                           (AND (DIS-OP 'WS-SPECS X) (NULL (GET-TERM-ARGS X))))
                         (LAMBDA (X)
                           (AND (DIS-OP 'WS-SPECS X))))
                (UNP-OPT-AUG V4 ((UNP-ELIST-CONST))
                             ((UNP-LIST-CONST) (UNP-NAME 'PLUS V5)))
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::JUX))
                  ((UNP-NT SB-UNP-PATTERN V3))
                  ((UNP-OPT V4
                            ((UNP-SEQ
                              (((UNP-KEYWORD 'SBST::JUXFORM))
                               ((UNP-UTERM V5
                                           ((UNP-REPT V5
                                                      ((UNP-UTERM V6
                                                                  ((UNP-NT
                                                                    SB-UNP-FORMAT-COMMAND
                                                                    V6))))
                                                      V6
                                                      (MAKE-BP :VALUE 0
                                                               :UNITED-FLAG NIL
                                                               :SPACES NIL :CRS
                                                               NIL :FORMAT
                                                               (LIST)))))))
                              ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                        :CRS NIL :FORMAT (LIST))
                               (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL
                                        :CRS NIL :FORMAT (LIST))))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-5 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'OPT) (UNP-LIST-CONST)
                (UNP-NAME 'PLUS V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::[))
                  ((UNP-UTERM V2
                              ((UNP-REPT V2
                                         ((UNP-UTERM V3
                                                     ((UNP-NT SB-UNP-PATTERN V3))))
                                         V3
                                         (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                  :SPACES NIL :CRS NIL :FORMAT
                                                  (LIST))))))
                  ((UNP-KEYWORD 'SBST::])))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-6 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-NAME 'PLUS V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::{))
                  ((UNP-UTERM V2
                              ((UNP-REPT V2
                                         ((UNP-UTERM V3
                                                     ((UNP-NT SB-UNP-PATTERN V3))))
                                         V3
                                         (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                  :SPACES NIL :CRS NIL :FORMAT
                                                  (LIST))))))
                  ((UNP-KEYWORD 'SBST::})))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-7 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UPATS
     ((:PARSE)
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-CONS)
                  (UNP-NAME 'PATTERN V2) (UNP-NAME 'DOUBLEPLUS V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::\|))
                    ((UNP-UTERM V3
                                ((UNP-DOUBLE-REPT V3
                                                  ((UNP-UTERM V4
                                                              ((UNP-NT
                                                                SB-UNP-PATTERN
                                                                V4))))
                                                  ((UNP-KEYWORD 'SBST::\|)) V4
                                                  (MAKE-BP :VALUE 2
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST))
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                   ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))
     (T
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-CONS)
                  (UNP-NAME 'PATTERN V2) (UNP-NAME 'DOUBLEPLUS V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::\|))
                    ((UNP-UTERM V3
                                ((UNP-DOUBLE-REPT V3
                                                  ((UNP-UTERM V4
                                                              ((UNP-NT
                                                                SB-UNP-PATTERN
                                                                V4))))
                                                  ((UNP-KEYWORD 'SBST::\|)) V4
                                                  (MAKE-BP :VALUE 2
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST))
                                                  (MAKE-BP :VALUE -500
                                                           :UNITED-FLAG
                                                           (LIST 1) :SPACES NIL
                                                           :CRS NIL :FORMAT
                                                           (LIST)))))))
                   ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE -500 :UNITED-FLAG (LIST 1) :SPACES NIL :CRS
                             NIL :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-8 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'STAR)
                (UNP-NAME 'PATTERN V2)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::*)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-9 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'PLUS)
                (UNP-NAME 'PATTERN V2)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::+)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-10 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1)
                (DIS-OPT V4
                         (LAMBDA (X)
                           (AND (DIS-OP 'DOUBLEPLUS X)
                                (AND (DIS-OP 'UKEYWORD (TERM-ARGN X 1)))))
                         (LAMBDA (X)
                           (AND (DIS-OP 'DOUBLEPLUS X)
                                (AND (DIS-OP 'FORMAT (TERM-ARGN X 1))
                                     (AND
                                      (DIS-OP 'UKEYWORD
                                              (TERM-ARGN (TERM-ARGN X 1) 0)))
                                     (AND
                                      (DIS-OP 'WS-SPECS
                                              (TERM-ARGN (TERM-ARGN X 1) 1)))))))
                (UNP-OPT-AUG V4
                             ((UNP-TERM-CONST 'DOUBLEPLUS)
                              (UNP-NAME 'PATTERN V2) (UNP-TERM-CONST 'UKEYWORD)
                              (UNP-NAME 'KEYWORD V3))
                             ((UNP-TERM-CONST 'DOUBLEPLUS)
                              (UNP-NAME 'PATTERN V2) (UNP-TERM-CONST 'FORMAT)
                              (UNP-TERM-CONST 'UKEYWORD) (UNP-NAME 'KEYWORD V3)
                              (UNP-LIST-CONST) (UNP-NAME 'PLUS V5)))
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::++))
                  ((UNP-LT 'SBST::KEYWORD V3))
                  ((UNP-OPT V4
                            ((UNP-UTERM V5
                                        ((UNP-REPT V5
                                                   ((UNP-UTERM V6
                                                               ((UNP-NT
                                                                 SB-UNP-FORMAT-COMMAND
                                                                 V6))))
                                                   V6
                                                   (MAKE-BP :VALUE 0
                                                            :UNITED-FLAG NIL
                                                            :SPACES NIL :CRS
                                                            NIL :FORMAT (LIST)))))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-11 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1)
                (DIS-OPT V4
                         (LAMBDA (X)
                           (AND (DIS-OP 'DOUBLESTAR X)
                                (AND (DIS-OP 'UKEYWORD (TERM-ARGN X 1)))))
                         (LAMBDA (X)
                           (AND (DIS-OP 'DOUBLESTAR X)
                                (AND (DIS-OP 'FORMAT (TERM-ARGN X 1))
                                     (AND
                                      (DIS-OP 'UKEYWORD
                                              (TERM-ARGN (TERM-ARGN X 1) 0)))
                                     (AND
                                      (DIS-OP 'WS-SPECS
                                              (TERM-ARGN (TERM-ARGN X 1) 1)))))))
                (UNP-OPT-AUG V4
                             ((UNP-TERM-CONST 'DOUBLESTAR)
                              (UNP-NAME 'PATTERN V2) (UNP-TERM-CONST 'UKEYWORD)
                              (UNP-NAME 'KEYWORD V3))
                             ((UNP-TERM-CONST 'DOUBLESTAR)
                              (UNP-NAME 'PATTERN V2) (UNP-TERM-CONST 'FORMAT)
                              (UNP-TERM-CONST 'UKEYWORD) (UNP-NAME 'KEYWORD V3)
                              (UNP-LIST-CONST) (UNP-NAME 'PLUS V5)))
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::**))
                  ((UNP-LT 'SBST::KEYWORD V3))
                  ((UNP-OPT V4
                            ((UNP-UTERM V5
                                        ((UNP-REPT V5
                                                   ((UNP-UTERM V6
                                                               ((UNP-NT
                                                                 SB-UNP-FORMAT-COMMAND
                                                                 V6))))
                                                   V6
                                                   (MAKE-BP :VALUE 0
                                                            :UNITED-FLAG NIL
                                                            :SPACES NIL :CRS
                                                            NIL :FORMAT (LIST)))))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-12 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'TAG)
                (UNP-NAME 'PATTERN V2) (UNP-NAME 'ALT V4)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::^))
                  ((UNP-BIND 'ALT_ V4)
                   (DIS-ALT V3
                            ((LAMBDA (X)
                               (DIS-LT 'SBST::ID X))
                             (LAMBDA (X)
                               (DIS-LT 'SBST::NUMBER X))))
                   (UNP-ALT-AUG V3
                                (((UNP-NAME (QUOTE NIL) V5))
                                 ((UNP-NAME (QUOTE NIL) V5))))
                   (UNP-ALT V3
                            (((UNP-LT 'SBST::ID V5))
                             ((UNP-LT 'SBST::NUMBER V5))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-13 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UPATS
     ((:PATS :AUGS :NOFORM :ELIDED)
      (UNP-UTERM V1
                 ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'FORMAT)
                  (UNP-NAME 'PATTERN V1) (UNP-TERM-CONST 'WS-SPECS)
                  (TOSS-NAME 'PLUS) (UNP-NT SB-UNP-PATTERN V1))))
     (T
      (UNP-UTERM V1
                 ((UNP-BIND 'JUX_ V1) (UNP-TERM-CONST 'FORMAT)
                  (UNP-NAME 'PATTERN V2) (UNP-LIST-CONST) (UNP-NAME 'PLUS V3)
                  (UNP-JUX ((UNP-NT SB-UNP-PATTERN V2)) 'JUX
                           ((UNP-UTERM V3
                                       ((UNP-REPT V3
                                                  ((UNP-UTERM V4
                                                              ((UNP-NT
                                                                SB-UNP-FORMAT-COMMAND
                                                                V4))))
                                                  V4
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST))))))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))
                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                    NIL :FORMAT (LIST))))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-14 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UPATS
     ((:PATS :UPATS :NOAUGS)
      (UNP-UTERM V1
                 ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'AUGMENT)
                  (UNP-NAME 'PATTERN V1) (TOSS-NAME 'AUGMENT)
                  (UNP-NT SB-UNP-PATTERN V1))))
     ((:AUGS)
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'AUGMENT)
                  (TOSS-NAME 'PATTERN) (UNP-NAME 'AUGMENT V2)
                  (UNP-SEQ
                   (((UNP-KEYWORD 'SBST::<)) ((UNP-NT SB-UNP-AUGMENT V2))
                    ((UNP-KEYWORD 'SBST::>)))
                   ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))
     (T
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'AUGMENT)
                  (UNP-NAME 'PATTERN V2) (UNP-NAME 'AUGMENT V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-PATTERN V2)) ((UNP-KEYWORD 'SBST::<))
                    ((UNP-NT SB-UNP-AUGMENT V3)) ((UNP-KEYWORD 'SBST::>)))
                   ((MAKE-BP :VALUE -200 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-PATTERN-AUX-ALT-15 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5
        V6
        V7
        V8
        V9
        V10)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5 V6 V7 V8 V9 V10)
    (UPATS
     ((:PATS :AUGS :NOUPATS :ELIDED :NOFORM)
      (UNP-UTERM V1
                 ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'UPATTERN)
                  (UNP-NAME 'PATTERN V1) (UNP-TERM-CONST 'UPATS) (TOSS-REPT)
                  (UNP-NT SB-UNP-PATTERN V1))))
     ((:UPATS)
      (UNP-UTERM V1
                 ((UNP-BIND 'PLUS_ V1) (UNP-TERM-CONST 'UPATTERN)
                  (TOSS-NAME 'PATTERN) (UNP-LIST-CONST)
                  (UNP-NAME (QUOTE NIL) V1)
                  (UNP-REPT V1
                            ((UNP-UTERM V2
                                        ((UNP-BIND 'SEQ_ V2)
                                         (UNP-NAME (QUOTE NIL) V8)
                                         (UNP-BIND '#:G23706 V8)
                                         (UNP-TERM-CONST 'UPAT)
                                         (DIS-OPT V5
                                                  (LAMBDA (X)
                                                    (AND (DIS-OP 'UIDS X)
                                                         (NULL
                                                          (GET-TERM-ARGS X))))
                                                  (LAMBDA (X)
                                                    (AND (DIS-OP 'UIDS X))))
                                         (UNP-OPT-AUG V5 ((UNP-ELIST-CONST))
                                                      ((UNP-LIST-CONST)
                                                       (UNP-NAME (QUOTE NIL) V6)))
                                         (UNP-LIST-CONST)
                                         (UNP-NAME (QUOTE NIL) V3)
                                         (UNP-SEQ
                                          (((UNP-KEYWORD 'SBST::<<))
                                           ((UNP-UTERM V3
                                                       ((UNP-REPT V3
                                                                  ((UNP-UTERM
                                                                    V4
                                                                    ((UNP-BIND
                                                                      'NONTERMINAL_
                                                                      V4)
                                                                     (UNP-NAME
                                                                      'PATTERN
                                                                      V4)
                                                                     (UNP-NT
                                                                      SB-UNP-PATTERN
                                                                      V4))))
                                                                  V4
                                                                  (MAKE-BP
                                                                   :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))
                                           ((UNP-KEYWORD 'SBST::>))
                                           ((UNP-OPT V5
                                                     ((UNP-SEQ
                                                       (((UNP-KEYWORD
                                                          'SBST::|:|))
                                                        ((UNP-UTERM V6
                                                                    ((UNP-DOUBLE-REPT
                                                                      V6
                                                                      ((UNP-UTERM
                                                                      V7
                                                                      ((UNP-BIND
                                                                      'NONTERMINAL_
                                                                      V7)
                                                                      (UNP-NAME
                                                                      'ID
                                                                      V7)
                                                                      (UNP-LT
                                                                      'SBST::ID
                                                                      V7))))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::|,|))
                                                                      V7
                                                                      (MAKE-BP
                                                                      :VALUE 2
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE 0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))
                                                       ((MAKE-BP :VALUE 0
                                                                 :UNITED-FLAG
                                                                 NIL :SPACES
                                                                 NIL :CRS NIL
                                                                 :FORMAT (LIST))
                                                        (MAKE-BP :VALUE 0
                                                                 :UNITED-FLAG
                                                                 NIL :SPACES
                                                                 NIL :CRS NIL
                                                                 :FORMAT (LIST)))))))
                                           ((UNP-KEYWORD 'SBST::>)))
                                          ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST))
                                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST))
                                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST))
                                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST))
                                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST)))))))
                            V2
                            (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS
                                     NIL :FORMAT (LIST))))))
     (T
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'UPATTERN)
                  (UNP-NAME 'PATTERN V2) (UNP-LIST-CONST)
                  (UNP-NAME (QUOTE NIL) V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-PATTERN V2))
                    ((UNP-UTERM V3
                                ((UNP-REPT V3
                                           ((UNP-UTERM V4
                                                       ((UNP-BIND 'SEQ_ V4)
                                                        (UNP-NAME (QUOTE NIL)
                                                                  V10)
                                                        (UNP-BIND '#:G23703 V10)
                                                        (UNP-TERM-CONST 'UPAT)
                                                        (DIS-OPT V7
                                                                 (LAMBDA (X)
                                                                   (AND
                                                                    (DIS-OP
                                                                     'UIDS X)
                                                                    (NULL
                                                                     (GET-TERM-ARGS
                                                                      X))))
                                                                 (LAMBDA (X)
                                                                   (AND
                                                                    (DIS-OP
                                                                     'UIDS X))))
                                                        (UNP-OPT-AUG V7
                                                                     ((UNP-ELIST-CONST))
                                                                     ((UNP-LIST-CONST)
                                                                      (UNP-NAME
                                                                      (QUOTE
                                                                      NIL)
                                                                      V8)))
                                                        (UNP-LIST-CONST)
                                                        (UNP-NAME (QUOTE NIL)
                                                                  V5)
                                                        (UNP-SEQ
                                                         (((UNP-KEYWORD
                                                            'SBST::<<))
                                                          ((UNP-UTERM V5
                                                                      ((UNP-REPT
                                                                      V5
                                                                      ((UNP-UTERM
                                                                      V6
                                                                      ((UNP-BIND
                                                                      'NONTERMINAL_
                                                                      V6)
                                                                      (UNP-NAME
                                                                      'PATTERN
                                                                      V6)
                                                                      (UNP-NT
                                                                      SB-UNP-PATTERN
                                                                      V6))))
                                                                      V6
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))))))
                                                          ((UNP-KEYWORD
                                                            'SBST::>))
                                                          ((UNP-OPT V7
                                                                    ((UNP-SEQ
                                                                      (((UNP-KEYWORD
                                                                      'SBST::|:|))
                                                                      ((UNP-UTERM
                                                                      V8
                                                                      ((UNP-DOUBLE-REPT
                                                                      V8
                                                                      ((UNP-UTERM
                                                                      V9
                                                                      ((UNP-BIND
                                                                      'NONTERMINAL_
                                                                      V9)
                                                                      (UNP-NAME
                                                                      'ID
                                                                      V9)
                                                                      (UNP-LT
                                                                      'SBST::ID
                                                                      V9))))
                                                                      ((UNP-KEYWORD
                                                                      'SBST::|,|))
                                                                      V9
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      2
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))
                                                                      ((MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST))
                                                                      (MAKE-BP
                                                                      :VALUE
                                                                      0
                                                                      :UNITED-FLAG
                                                                      NIL
                                                                      :SPACES
                                                                      NIL
                                                                      :CRS
                                                                      NIL
                                                                      :FORMAT
                                                                      (LIST)))))))
                                                          ((UNP-KEYWORD
                                                            'SBST::>)))
                                                         ((MAKE-BP :VALUE 0
                                                                   :UNITED-FLAG
                                                                   NIL :SPACES
                                                                   NIL :CRS NIL
                                                                   :FORMAT
                                                                   (LIST))
                                                          (MAKE-BP :VALUE 0
                                                                   :UNITED-FLAG
                                                                   NIL :SPACES
                                                                   NIL :CRS NIL
                                                                   :FORMAT
                                                                   (LIST))
                                                          (MAKE-BP :VALUE 0
                                                                   :UNITED-FLAG
                                                                   NIL :SPACES
                                                                   NIL :CRS NIL
                                                                   :FORMAT
                                                                   (LIST))
                                                          (MAKE-BP :VALUE 0
                                                                   :UNITED-FLAG
                                                                   NIL :SPACES
                                                                   NIL :CRS NIL
                                                                   :FORMAT
                                                                   (LIST))
                                                          (MAKE-BP :VALUE 0
                                                                   :UNITED-FLAG
                                                                   NIL :SPACES
                                                                   NIL :CRS NIL
                                                                   :FORMAT
                                                                   (LIST)))))))
                                           V4
                                           (MAKE-BP :VALUE 0 :UNITED-FLAG NIL
                                                    :SPACES NIL :CRS NIL
                                                    :FORMAT (LIST)))))))
                   ((MAKE-BP :VALUE -200 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-AUGMENT-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4 V5)
    (SETF V0 AS)
    (NT-UNP 'AUGMENT AS
            ((UNP-BIND 'ALT_ V0)
             (DIS-ALT V0
                      ((LAMBDA (X)
                         (AND (DIS-OP 'STRING-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NUMBER-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'LITERAL-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X) (MATCH-ID 'OPT (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X) (MATCH-ID 'ALT (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X) (MATCH-ID 'PLUS (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X)
                              (MATCH-ID 'DOUBLEPLUS (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X) (MATCH-ID 'STAR (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X)
                              (MATCH-ID 'DOUBLESTAR (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NAME X) (MATCH-ID 'SEQ (TERM-ARGN X 0))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'EXT-NAME X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'NULL X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'LIST X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'CONS X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'BCONS X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'APPEND X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'TERM-CONST X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'STAR-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'PLUS-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'ALT-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'OPT-AUG X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'TAG-AUG X)))))
             (UNP-NAME (QUOTE NIL) V1)
             (UNP-ALT V0
                      (((SB-UNP-AUGMENT-AUX-ALT-1 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-2 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-3 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-4 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-5 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-6 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-7 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-8 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-9 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-10 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-11 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-12 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-13 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-14 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-15 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-16 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-17 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-18 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-19 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-20 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-21 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-22 V0 V1))
                       ((SB-UNP-AUGMENT-AUX-ALT-23 V0 V1)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-1 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'STRING-AUG)
                (UNP-NAME 'STRING V1) (UNP-LT 'SBST::STRING V1))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-2 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'NUMBER-AUG)
                (UNP-NAME 'NUMBER V1) (UNP-LT 'SBST::NUMBER V1))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-3 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'LITERAL-AUG)
                (UNP-NAME 'LITERAL V1) (UNP-LT 'SBST::LITERAL V1))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-4 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'NONTERMINAL_ V1) (UNP-TERM-CONST 'NAME)
                (UNP-NAME 'ID V1) (UNP-LT 'SBST::ID V1))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-5 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::OPT))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-6 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::ALT))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-7 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::PLUS))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-8 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::DOUBLEPLUS))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-9 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::STAR))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-10 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::DOUBLESTAR))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-11 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NAME) (UNP-BASE-LIT)
                (UNP-KEYWORD 'SBST::SEQ))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-12 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'EXT-NAME)
                (UNP-NAME 'ID V2) (UNP-NAME 'ID V3)
                (UNP-SEQ
                 (((UNP-LT 'SBST::ID V2)) ((UNP-KEYWORD 'SBST::@))
                  ((UNP-LT 'SBST::ID V3)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-13 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'NULL)
                (UNP-KEYWORD 'SBST::NULL))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-14 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-NAME 'DOUBLESTAR V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::LIST)) ((UNP-KEYWORD 'SBST::|(|))
                  ((UNP-UTERM V2
                              ((UNP-DOUBLE-REPT V2
                                                ((UNP-UTERM V3
                                                            ((UNP-NT
                                                              SB-UNP-AUGMENT V3))))
                                                ((UNP-KEYWORD 'SBST::|,|)) V3
                                                (MAKE-BP :VALUE 2 :UNITED-FLAG
                                                         NIL :SPACES NIL :CRS
                                                         NIL :FORMAT (LIST))
                                                (MAKE-BP :VALUE 0 :UNITED-FLAG
                                                         NIL :SPACES NIL :CRS
                                                         NIL :FORMAT (LIST))))))
                  ((UNP-KEYWORD 'SBST::|)|)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-15 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'CONS)
                (UNP-NAME 'AUGMENT V2) (UNP-NAME 'AUGMENT V3)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::CONS)) ((UNP-KEYWORD 'SBST::|(|))
                  ((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::|,|))
                  ((UNP-NT SB-UNP-AUGMENT V3)) ((UNP-KEYWORD 'SBST::|)|)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-16 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'BCONS)
                (UNP-NAME 'AUGMENT V2) (UNP-NAME 'AUGMENT V3)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::BCONS)) ((UNP-KEYWORD 'SBST::|(|))
                  ((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::|,|))
                  ((UNP-NT SB-UNP-AUGMENT V3)) ((UNP-KEYWORD 'SBST::|)|)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-17 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'APPEND)
                (UNP-NAME 'AUGMENT V2) (UNP-NAME 'AUGMENT V3)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::APPEND)) ((UNP-KEYWORD 'SBST::|(|))
                  ((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::|,|))
                  ((UNP-NT SB-UNP-AUGMENT V3)) ((UNP-KEYWORD 'SBST::|)|)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-18 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-CONS)
                (DIS-ALT V2
                         ((LAMBDA (X)
                            (AND (DIS-OP 'ID-AUG X)))
                          (LAMBDA (X)
                            (AND (DIS-OP 'LITERAL-AUG X)))
                          (LAMBDA (X)
                            (AND (DIS-OP 'STRING-AUG X)))))
                (UNP-ALT-AUG V2
                             (((UNP-TERM-CONST 'ID-AUG) (UNP-NAME 'ID V3))
                              ((UNP-TERM-CONST 'LITERAL-AUG)
                               (UNP-NAME 'LITERAL V3))
                              ((UNP-TERM-CONST 'STRING-AUG)
                               (UNP-NAME 'STRING V3))))
                (UNP-NAME 'DOUBLESTAR V4)
                (UNP-SEQ
                 (((UNP-ALT V2
                            (((UNP-LT 'SBST::ID V3))
                             ((UNP-LT 'SBST::LITERAL V3))
                             ((UNP-LT 'SBST::STRING V3)))))
                  ((UNP-KEYWORD 'SBST::|(|))
                  ((UNP-UTERM V4
                              ((UNP-DOUBLE-REPT V4
                                                ((UNP-UTERM V5
                                                            ((UNP-NT
                                                              SB-UNP-AUGMENT V5))))
                                                ((UNP-KEYWORD 'SBST::|,|)) V5
                                                (MAKE-BP :VALUE 2 :UNITED-FLAG
                                                         NIL :SPACES NIL :CRS
                                                         NIL :FORMAT (LIST))
                                                (MAKE-BP :VALUE 0 :UNITED-FLAG
                                                         NIL :SPACES NIL :CRS
                                                         NIL :FORMAT (LIST))))))
                  ((UNP-KEYWORD 'SBST::|)|)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-19 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'STAR-AUG)
                (UNP-NAME 'AUGMENT V2)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::*)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-20 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'PLUS-AUG)
                (UNP-NAME 'AUGMENT V2)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::+)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-21 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UPATS
     ((:PARSE)
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-CONS)
                  (UNP-NAME 'AUGMENT V2) (UNP-NAME 'DOUBLEPLUS V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::\|))
                    ((UNP-UTERM V3
                                ((UNP-DOUBLE-REPT V3
                                                  ((UNP-UTERM V4
                                                              ((UNP-NT
                                                                SB-UNP-AUGMENT
                                                                V4))))
                                                  ((UNP-KEYWORD 'SBST::\|)) V4
                                                  (MAKE-BP :VALUE 2
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST))
                                                  (MAKE-BP :VALUE 0
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST)))))))
                   ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))
     (T
      (UNP-UTERM V1
                 ((UNP-BIND 'SEQ_ V1) (UNP-LIST-CONST) (UNP-CONS)
                  (UNP-NAME 'AUGMENT V2) (UNP-NAME 'DOUBLEPLUS V3)
                  (UNP-SEQ
                   (((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::\|))
                    ((UNP-UTERM V3
                                ((UNP-DOUBLE-REPT V3
                                                  ((UNP-UTERM V4
                                                              ((UNP-NT
                                                                SB-UNP-AUGMENT
                                                                V4))))
                                                  ((UNP-KEYWORD 'SBST::\|)) V4
                                                  (MAKE-BP :VALUE 2
                                                           :UNITED-FLAG NIL
                                                           :SPACES NIL :CRS NIL
                                                           :FORMAT (LIST))
                                                  (MAKE-BP :VALUE -500
                                                           :UNITED-FLAG
                                                           (LIST 1) :SPACES NIL
                                                           :CRS NIL :FORMAT
                                                           (LIST)))))))
                   ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST))
                    (MAKE-BP :VALUE -500 :UNITED-FLAG (LIST 1) :SPACES NIL :CRS
                             NIL :FORMAT (LIST))
                    (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                             :FORMAT (LIST)))))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-22 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'OPT-AUG)
                (UNP-NAME 'AUGMENT V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::[)) ((UNP-NT SB-UNP-AUGMENT V2))
                  ((UNP-KEYWORD 'SBST::])))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-AUGMENT-AUX-ALT-23 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4
        V5)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4 V5)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'TAG-AUG)
                (UNP-NAME 'AUGMENT V2) (UNP-NAME 'ALT V4)
                (UNP-SEQ
                 (((UNP-NT SB-UNP-AUGMENT V2)) ((UNP-KEYWORD 'SBST::^))
                  ((UNP-BIND 'ALT_ V4)
                   (DIS-ALT V3
                            ((LAMBDA (X)
                               (DIS-LT 'SBST::ID X))
                             (LAMBDA (X)
                               (DIS-LT 'SBST::NUMBER X))))
                   (UNP-ALT-AUG V3
                                (((UNP-NAME (QUOTE NIL) V5))
                                 ((UNP-NAME (QUOTE NIL) V5))))
                   (UNP-ALT V3
                            (((UNP-LT 'SBST::ID V5))
                             ((UNP-LT 'SBST::NUMBER V5))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX (AS)
  (LET (V0
        V1
        V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V0 V1 V2 V3 V4)
    (SETF V0 AS)
    (NT-UNP 'FORMAT-COMMAND AS
            ((UNP-BIND 'ALT_ V0)
             (DIS-ALT V0
                      ((LAMBDA (X)
                         (OR (AND (DIS-OP 'SP X) (MATCH-NUM 1 (TERM-ARGN X 0)))
                             (AND (DIS-OP 'SP X))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'CR X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'INCR-BP X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'DECR-BP X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'UNITE X)))
                       (LAMBDA (X)
                         (OR
                          (AND (DIS-OP 'PUSH-INDENT X)
                               (MATCH-NUM 2 (TERM-ARGN X 0)))
                          (AND (DIS-OP 'PUSH-INDENT X))))
                       (LAMBDA (X)
                         (AND (DIS-OP 'POP-INDENT X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'PUSH-TAB-LEFT X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'PUSH-TAB-RIGHT X)))
                       (LAMBDA (X)
                         (AND (DIS-OP 'POP-TAB X)))))
             (UNP-NAME (QUOTE NIL) V1)
             (UNP-ALT V0
                      (((SB-UNP-FORMAT-COMMAND-AUX-ALT-1 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-2 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-3 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-4 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-5 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-6 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-7 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-8 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-9 V0 V1))
                       ((SB-UNP-FORMAT-COMMAND-AUX-ALT-10 V0 V1)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-1 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1)
                (DIS-OPT V2
                         (LAMBDA (X)
                           (AND (DIS-OP 'SP X) (MATCH-NUM 1 (TERM-ARGN X 0))))
                         (LAMBDA (X)
                           (AND (DIS-OP 'SP X))))
                (UNP-OPT-AUG V2 ((UNP-TERM-CONST 'SP) (UNP-BASE-NUM))
                             ((UNP-TERM-CONST 'SP) (UNP-NAME 'NUMBER V3)))
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::_))
                  ((UNP-OPT V2 ((UNP-LT 'SBST::NUMBER V3)))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-2 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'CR)
                (UNP-KEYWORD 'SBST::|#|))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-3 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'INCR-BP)
                (UNP-NAME 'NUMBER V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::/+)) ((UNP-LT 'SBST::NUMBER V2)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-4 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'DECR-BP)
                (UNP-NAME 'NUMBER V2)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::/-)) ((UNP-LT 'SBST::NUMBER V2)))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-5 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1) (UNP-TERM-CONST 'UNITE) (UNP-NAME 'ALT V3)
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::?))
                  ((UNP-BIND 'ALT_ V3)
                   (DIS-ALT V2
                            ((LAMBDA (X)
                               (DIS-LT 'SBST::ID X))
                             (LAMBDA (X)
                               (DIS-LT 'SBST::NUMBER X))))
                   (UNP-ALT-AUG V2
                                (((UNP-NAME (QUOTE NIL) V4))
                                 ((UNP-NAME (QUOTE NIL) V4))))
                   (UNP-ALT V2
                            (((UNP-LT 'SBST::ID V4))
                             ((UNP-LT 'SBST::NUMBER V4))))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-6 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'SEQ_ V1)
                (DIS-OPT V2
                         (LAMBDA (X)
                           (AND (DIS-OP 'PUSH-INDENT X)
                                (MATCH-NUM 2 (TERM-ARGN X 0))))
                         (LAMBDA (X)
                           (AND (DIS-OP 'PUSH-INDENT X))))
                (UNP-OPT-AUG V2 ((UNP-TERM-CONST 'PUSH-INDENT) (UNP-BASE-NUM))
                             ((UNP-TERM-CONST 'PUSH-INDENT)
                              (UNP-NAME 'NUMBER V3)))
                (UNP-SEQ
                 (((UNP-KEYWORD 'SBST::!+))
                  ((UNP-OPT V2 ((UNP-LT 'SBST::NUMBER V3)))))
                 ((MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST))
                  (MAKE-BP :VALUE 0 :UNITED-FLAG NIL :SPACES NIL :CRS NIL
                           :FORMAT (LIST)))))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-7 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'POP-INDENT)
                (UNP-KEYWORD 'SBST::!-))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-8 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'PUSH-TAB-LEFT)
                (UNP-KEYWORD 'SBST::@<))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-9 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'PUSH-TAB-RIGHT)
                (UNP-KEYWORD 'SBST::@>))))) 



(DEFUN SB-UNP-FORMAT-COMMAND-AUX-ALT-10 (V0 V1)
  (ERGO-IGNORE-IF-UNUSED V0 V1)
  (LET (V2
        V3
        V4)
    (ERGO-IGNORE-IF-UNUSED V2 V3 V4)
    (UNP-UTERM V1
               ((UNP-BIND 'UKEYWORD_ V1) (UNP-TERM-CONST 'POP-TAB)
                (UNP-KEYWORD 'SBST::@^))))) 



(DEFUN SB-DIS-META-GRAMMAR (AS)
  (OR (IS-NT-OPSIG? 'META-GRAMMAR AS) ((LAMBDA (X)
                                         (DIS-OP 'GRAMMAR X)) AS))) 



(DEFUN SB-DIS-EXTERNAL-GRAMMARS (AS)
  (OR (IS-NT-OPSIG? 'EXTERNAL-GRAMMARS AS)
      ((LAMBDA (X)
         (DIS-OP 'EXT-GRAM X)) AS))) 



(DEFUN SB-DIS-COMMENT-CHARACTER (AS)
  (OR (IS-NT-OPSIG? 'COMMENT-CHARACTER AS)
      ((LAMBDA (X)
         (DIS-OP 'COMMENT-CHARACTER X)) AS))) 



(DEFUN SB-DIS-ESCAPE-CHARACTER (AS)
  (OR (IS-NT-OPSIG? 'ESCAPE-CHARACTER AS)
      ((LAMBDA (X)
         (DIS-OP 'ESCAPE-CHARACTER X)) AS))) 



(DEFUN SB-DIS-OPERATOR-INFORMATION (AS)
  (OR (IS-NT-OPSIG? 'OPERATOR-INFORMATION AS)
      ((LAMBDA (X)
         (DIS-OP 'OP-INFO X)) AS))) 



(DEFUN SB-DIS-LEXICAL-TERMINALS (AS)
  (OR (IS-NT-OPSIG? 'LEXICAL-TERMINALS AS)
      ((LAMBDA (X)
         (DIS-OP 'LEX-TERMS X)) AS))) 



(DEFUN SB-DIS-BRACKETING-INFORMATION (AS)
  (OR (IS-NT-OPSIG? 'BRACKETING-INFORMATION AS)
      ((LAMBDA (X)
         (DIS-OP 'BRACKET-ENTRIES X)) AS))) 



(DEFUN SB-DIS-SPACING-INFORMATION (AS)
  (OR (IS-NT-OPSIG? 'SPACING-INFORMATION AS)
      ((LAMBDA (X)
         (DIS-OP 'SPACING X)) AS))) 



(DEFUN SB-DIS-PRECEDENCE-INFORMATION (AS)
  (OR (IS-NT-OPSIG? 'PRECEDENCE-INFORMATION AS)
      ((LAMBDA (X)
         (DIS-OP 'PREC-ENTRIES X)) AS))) 



(DEFUN SB-DIS-MULTIPLE-LEVELS (AS)
  (OR (IS-NT-OPSIG? 'MULTIPLE-LEVELS AS)
      ((LAMBDA (X)
         (DIS-OP 'PREC-LEVELS X)) AS))) 



(DEFUN SB-DIS-SINGLE-LEVEL (AS)
  (OR (IS-NT-OPSIG? 'SINGLE-LEVEL AS) ((LAMBDA (X)
                                         (DIS-OP 'PREC-LEVEL X)) AS))) 



(DEFUN SB-DIS-SINGLE-OP-PRECEDENCE (AS)
  (OR (IS-NT-OPSIG? 'SINGLE-OP-PRECEDENCE AS)
      ((LAMBDA (X)
         (OR ((LAMBDA (X)
                (DIS-OP 'AGGREGATE X)) X)
             ((LAMBDA (X)
                (DIS-OP 'MEDIAL X)) X)
             ((LAMBDA (X)
                (DIS-OP 'INITIAL X)) X)))
       AS))) 



(DEFUN SB-DIS-NONTERMINAL-DEFINITION (AS)
  (OR (IS-NT-OPSIG? 'NONTERMINAL-DEFINITION AS)
      ((LAMBDA (X)
         (DIS-OP 'NT-DEF X)) AS))) 



(DEFUN SB-DIS-PATTERN (AS)
  (OR (IS-NT-OPSIG? 'PATTERN AS)
      ((LAMBDA (X)
         (OR ((LAMBDA (X)
                (DIS-OP 'UPATTERN X)) X)
             ((LAMBDA (X)
                (DIS-OP 'AUGMENT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'FORMAT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'TAG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'DOUBLESTAR X)) X)
             ((LAMBDA (X)
                (DIS-OP 'DOUBLEPLUS X)) X)
             ((LAMBDA (X)
                (DIS-OP 'PLUS X)) X)
             ((LAMBDA (X)
                (DIS-OP 'STAR X)) X) ((LAMBDA (X)
                                                  (DIS-OP 'ALT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'SEQ X)) X) ((LAMBDA (X)
                                                 (DIS-OP 'OPT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'JUX X)) X)
             ((LAMBDA (X)
                (DIS-OP 'UKEYWORD X)) X)
             ((LAMBDA (X)
                (DIS-OP 'EXT-NONTERMINAL X)) X)
             ((LAMBDA (X)
                (DIS-OP 'NONTERMINAL X)) X)))
       AS))) 



(DEFUN SB-DIS-AUGMENT (AS)
  (OR (IS-NT-OPSIG? 'AUGMENT AS)
      ((LAMBDA (X)
         (OR ((LAMBDA (X)
                (DIS-OP 'TAG-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'OPT-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'ALT-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'PLUS-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'STAR-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'TERM-CONST X)) X)
             ((LAMBDA (X)
                (DIS-OP 'APPEND X)) X)
             ((LAMBDA (X)
                (DIS-OP 'BCONS X)) X)
             ((LAMBDA (X)
                (DIS-OP 'CONS X)) X)
             ((LAMBDA (X)
                (DIS-OP 'LIST X)) X)
             ((LAMBDA (X)
                (DIS-OP 'NULL X)) X)
             ((LAMBDA (X)
                (DIS-OP 'EXT-NAME X)) X)
             ((LAMBDA (X)
                (DIS-OP 'NAME X)) X)
             ((LAMBDA (X)
                (DIS-OP 'LITERAL-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'NUMBER-AUG X)) X)
             ((LAMBDA (X)
                (DIS-OP 'STRING-AUG X)) X)))
       AS))) 



(DEFUN SB-DIS-FORMAT-COMMAND (AS)
  (OR (IS-NT-OPSIG? 'FORMAT-COMMAND AS)
      ((LAMBDA (X)
         (OR ((LAMBDA (X)
                (DIS-OP 'POP-TAB X)) X)
             ((LAMBDA (X)
                (DIS-OP 'PUSH-TAB-RIGHT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'PUSH-TAB-LEFT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'POP-INDENT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'PUSH-INDENT X)) X)
             ((LAMBDA (X)
                (DIS-OP 'UNITE X)) X)
             ((LAMBDA (X)
                (DIS-OP 'DECR-BP X)) X)
             ((LAMBDA (X)
                (DIS-OP 'INCR-BP X)) X)
             ((LAMBDA (X)
                (DIS-OP 'CR X)) X) ((LAMBDA (X)
                                                (DIS-OP 'SP X)) X)))
       AS))) 

