;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package "SYNTAX-BOX")  (use-package :ergolisp)

;;; The following hacks revise the precedences in the unparser for grammars
;;; since there is some wierd anomaly with the parsing of |. 

(DEFVAR SB-BRACKET-INFO
        (MAKE-HASH-TABLE :TEST #'EQ))
;;(CLRHASH SB-BRACKET-INFO)
(MAPC
  #'(LAMBDA (ENTRY)
     (SET-BRACKET-INFO (CAR ENTRY) (CADR ENTRY) (CADDR ENTRY) SB-BRACKET-INFO))
  '((pattern sbst::{ sbst::})))


(defvar SB-PREC-INFO
        (MAKE-HASH-TABLE :TEST #'EQ))
;;(CLRHASH SB-PREC-INFO)
;;(MAPC #'(LAMBDA (NT)
;;          (INIT-PREC-INFO NT SB-PREC-INFO))
;;       '(PATTERN AUGMENT))
(MAPC
  #'(LAMBDA (ENTRY)
     (SET-PREC-INFO (CAR ENTRY) (CADR ENTRY) (CADDR ENTRY) (CADDDR ENTRY)
      SB-PREC-INFO))
  '((pattern sbst::\| 1 0)
    (pattern sbst::\| 2 0)))

