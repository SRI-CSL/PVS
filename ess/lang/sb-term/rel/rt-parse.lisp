;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-parse.lisp	1.4 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Anne Rogers

;;; Parser Runtime System

(in-package :sb-runtime)  (use-package :ergolisp)

(export '(lbp lam-error
	      *parser-error-count* *parser-error* *parser-return-errors*
	      *ask-about-bad-tokens*
	      *parser-abort-threshold* parser-abort-catch initial-error
	      medial-error mk-null do-syntax-error))

(defvar *parser-error*)
(defvar *parser-return-errors*)
(defvar *parser-error-count*)
(defvar *parser-abort-threshold*)
(defvar *ask-about-bad-tokens*)
(defparameter *hard-syntax-errors* nil
  "If true, call error when a syntax error occurs.")

(defun lbp (prec-list)
  (let (temp)
    (cond ((setq temp (assoc (peek-first) prec-list))
	   (cadr temp))
	  (t 0))))

;;; This is the common code in all of the routines that do syntax errors.
(defun do-syntax-error (string &rest args)
  (incf *parser-error-count*)
  (setf *parser-error* t)
  (when *parser-return-errors*
    (lexical-clear-input *lexical-stream*)
    (flush-lexer)
    (throw 'parser-abort-catch
	   (values :parser-aborted-on-return-error
		   string
		   args
		   (car (last args)))))	; place
  (when (and *parser-abort-threshold*
	     (>= *parser-error-count* *parser-abort-threshold*))
    (lexical-clear-input *lexical-stream*)
    (flush-lexer)
    (throw 'parser-abort-catch :parse-aborted-on-threshold))
  (let ((*print-escape* nil))
    (format t "~?" string args))
  ;; Bomb if we are supposed to, otherwise return nil.
  (when *hard-syntax-errors* (error "*hard-syntax-errors* is true."))
  nil)

(defun lam-error (fs-list)
  (multiple-value-bind (type name place) (peek-first)
    (do-syntax-error "Look ahead set match error.~%~
		     Missing ~A inserted here:~%~A" (caar fs-list) place)
    (when *ask-about-bad-tokens*
      (format t "Bad token is ~A~:[~; ~:*~S~].~%~
	      Should it be removed from the token stream? [yes]" type name)
      (if (yesp) (gobble-token)))))

(defun yesp ()
  (member (string-trim '(#\space #\tab) (read-line))
	  '("" "y" "ye" "yes")
	  :test #'string-equal))

;;; fs-list appears to be a list of lists, each representing an expected token.
;;;  Any element of fs-list is of the form (first . possible-seconds)
;;;  where first is a token and possible-seconds is a list of tokens.
(defun initial-error (fs-list &aux (formatstr
				    "~&Initial error.~%~
				    Found ~A when looking for ~A here:~%~A"))
  (clet* (((first ignore place) (peek-first))
	  (temp (assoc first fs-list)))
	 (if (null temp)
	     (do-syntax-error formatstr
			      first
			      (orify (mapcar #'(lambda (x) (car x)) fs-list))
			      place)
	     (multiple-value-bind (second name place) (peek-second)
	       (declare (ignore name))
	       (do-syntax-error formatstr
				second
				(orify (cdr temp))
				place)))))

;;; fs-list appears to be a list of the tokens that were expected, unlike the 
;;; meaning for initial-error.
(defun medial-error (fs-list)
  (clet* (((first ignore place) (peek-first)))
	 (do-syntax-error "Medial error.~%~
			  Found ~A when looking for ~A here:~%~S"
			  first
			  (orify fs-list)
			  place)))

;;; This incantation is taken from the silver bible, page 402.  Purpose of this
;;; routine is to return a string which enumerates the items in the list in 
;;; grammatical English. 
(defun orify (list)
  (format nil "~{~#[nothing at all~;~A~;~A or ~A~:;~@{~#[~;or ~]~A~^, ~}~]~}"
	  list))
