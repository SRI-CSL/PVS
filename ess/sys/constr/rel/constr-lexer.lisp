;;; -*- Mode: Lisp; Package: CONSTRG -*-
(in-package :constr-term-rep)  ;; creates package for abstract syntax. 

(in-package :constrg)  ;; enters package for generated code.  

(use-package '(:newattr :lang :sb-runtime :sort :term :occ :oper :ergolisp))


(export '())

(defparameter constr-keyword-list
    (quote
     (sbst::|:| sbst::|\|| sbst::= sbst::datatype sbst::abbrev sbst::==
      sbst::of sbst::|'| sbst::unit sbst::} sbst::{ sbst::} sbst::|,| sbst::{
      sbst::not-sats sbst::sats sbst::not sbst::or sbst::and sbst::|#|
      sbst::-> sbst::? sbst::* sbst::|(| sbst::|)|)))
(defparameter constr-single-char-op-list
    (quote (#\} #\{ #\# #\] #\[ #\* #\? #\' #\) #\( #\| #\,)))
(defparameter constr-multi-char-op-list (quote ((#\= . lex-=))))
(defparameter constr-all-operators-list
    (quote
     (sbst::} sbst::{ sbst::|#| sbst::] sbst::[ sbst::* sbst::? sbst::|'|
      sbst::|)| sbst::|(| sbst::|\|| sbst::= sbst::== sbst::|,|)))
(defparameter constr-new-line-comment-char #\%)
(defparameter constr-open-comment-char ())
(defparameter constr-close-comment-char ())
(defparameter constr-escape-char ())
(defparameter constr-case-sensitive ())
(defparameter constr-string-char #\")
(defparameter constr-keyword-char ())
(defparameter constr-literal-char ())
(defparameter constr-restricted-chars
    (reduce #'(lambda (r s) (union r s :test #'char=))
     (list constr-single-char-op-list
           (if constr-new-line-comment-char
               (list constr-new-line-comment-char))
           (if constr-open-comment-char (list constr-open-comment-char))
           (if constr-close-comment-char (list constr-close-comment-char))
           (if constr-escape-char (list constr-escape-char))
           (if constr-string-char (list constr-string-char))
           (if constr-keyword-char (list constr-keyword-char))
           (if constr-literal-char (list constr-literal-char)))))
(defvar *constr-keyword-table* ())
(defun init-lexer-aux (lexstream)
  (init-lexical-readtable lexstream
    :single-char-op-list constr-single-char-op-list
    :new-line-comment-char constr-new-line-comment-char
    :open-comment-char constr-open-comment-char
    :escape-char constr-escape-char
    :multi-char-op-list constr-multi-char-op-list))

(defun init-lexer (lexstream)
  (init-lexer-aux lexstream)
  (if constr-string-char
      (lexical-make-macro lexstream constr-string-char #'read-sb-string))
  (if constr-keyword-char
      (lexical-make-macro lexstream constr-keyword-char
                          #'read-keyword-string))
  (if constr-literal-char
      (lexical-make-macro lexstream constr-literal-char #'read-literal)))

(defun lex-= (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and constr-escape-char (char= holdchar constr-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\=) 'sbst::==)
          (t (lexical-unread-char stream) 'sbst::=))))
