;;; -*- Mode: Lisp; Package: ANALYSIS-FACILITY -*-
(in-package "ANALYSIS-FACILITY")  ;; creates package for abstract syntax. 

(in-package "ANALYSIS-FACILITY")  ;; enters package for generated code.  

(use-package '(:ergolisp))  ;; standard primitives 
(use-package '("OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR"))


(export '())

(DEFPARAMETER AF-KEYWORD-LIST
              '(SBST::EXTERNAL SBST::GRAMMARS SBST::! SBST::* SBST::^
                SBST::|\\| SBST::|\\| SBST::ITERATE SBST::INITIAL SBST::TEST
                SBST::|;| SBST::FINAL SBST::WITH SBST::_ SBST::AND SBST::=
                SBST::WHERE SBST::AST SBST::[ SBST::\| SBST::] SBST::|:|
                SBST::IF SBST::THEN SBST::ELSE SBST::|'| SBST::|#| SBST::|#|
                SBST::/ SBST::+ SBST::- SBST::- SBST::|(| SBST::|)|
                SBST::TERMINALS SBST::|,| SBST::GRAMMAR SBST::PASS
                SBST::PACKAGE))
(DEFPARAMETER AF-SINGLE-CHAR-OP-LIST
              '(#\( #\) #\* #\/ #\+ #\! #\^ #\; #\: #\, #\' #\# #\= #\\ #\| #\[
                #\] #\_))
(DEFPARAMETER AF-MULTI-CHAR-OP-LIST NIL)
(DEFPARAMETER AF-ALL-OPERATORS-LIST
              '(SBST::|(| SBST::|)| SBST::* SBST::/ SBST::+ SBST::! SBST::^
                SBST::|;| SBST::|:| SBST::|,| SBST::|'| SBST::|#| SBST::=
                SBST::|\\| SBST::\| SBST::[ SBST::] SBST::_))
(DEFPARAMETER AF-NEW-LINE-COMMENT-CHAR #\%)
(DEFPARAMETER AF-OPEN-COMMENT-CHAR NIL)
(DEFPARAMETER AF-CLOSE-COMMENT-CHAR NIL)
(DEFPARAMETER AF-ESCAPE-CHAR NIL)
(DEFPARAMETER AF-CASE-SENSITIVE NIL)
(DEFPARAMETER AF-STRING-CHAR #\")
(DEFPARAMETER AF-KEYWORD-CHAR NIL)
(DEFPARAMETER AF-LITERAL-CHAR NIL)
(DEFPARAMETER AF-RESTRICTED-CHARS
              (REDUCE #'(LAMBDA (R S)
                                (UNION R S :TEST #'CHAR=))
                      (LIST AF-SINGLE-CHAR-OP-LIST
                            (IF AF-NEW-LINE-COMMENT-CHAR
                                (LIST AF-NEW-LINE-COMMENT-CHAR))
                            (IF AF-OPEN-COMMENT-CHAR
                                (LIST AF-OPEN-COMMENT-CHAR))
                            (IF AF-CLOSE-COMMENT-CHAR
                                (LIST AF-CLOSE-COMMENT-CHAR))
                            (IF AF-ESCAPE-CHAR
                                (LIST AF-ESCAPE-CHAR))
                            (IF AF-STRING-CHAR
                                (LIST AF-STRING-CHAR))
                            (IF AF-KEYWORD-CHAR
                                (LIST AF-KEYWORD-CHAR))
                            (IF AF-LITERAL-CHAR
                                (LIST AF-LITERAL-CHAR)))))
(DEFVAR *AF-KEYWORD-TABLE* NIL)
(DEFUN INIT-LEXER-AUX
       (LEXSTREAM)
       (INIT-LEXICAL-READTABLE LEXSTREAM
                               :SINGLE-CHAR-OP-LIST
                               AF-SINGLE-CHAR-OP-LIST
                               :NEW-LINE-COMMENT-CHAR
                               AF-NEW-LINE-COMMENT-CHAR
                               :OPEN-COMMENT-CHAR
                               AF-OPEN-COMMENT-CHAR
                               :ESCAPE-CHAR
                               AF-ESCAPE-CHAR
                               :MULTI-CHAR-OP-LIST
                               AF-MULTI-CHAR-OP-LIST))

(DEFUN INIT-LEXER
       (LEXSTREAM)
       (INIT-LEXER-AUX LEXSTREAM)
       (IF AF-STRING-CHAR
           (LEXICAL-MAKE-MACRO LEXSTREAM
                               AF-STRING-CHAR
                               #'READ-SB-STRING))
       (IF AF-KEYWORD-CHAR
           (LEXICAL-MAKE-MACRO LEXSTREAM
                               AF-KEYWORD-CHAR
                               #'READ-KEYWORD-STRING))
       (IF AF-LITERAL-CHAR
           (LEXICAL-MAKE-MACRO LEXSTREAM
                               AF-LITERAL-CHAR
                               #'READ-LITERAL)))

