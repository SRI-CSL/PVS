;;; -*- Mode: Lisp; Package: CONSTR -*-
;;;
;;; Define structure-based implementations of ADTs.
;;;
;;; Author: Conal.  Last Modified Mon Sep 25 09:51:20 1989
;;; 
;;; Sccs Id @(#)constr.lisp	1.10 9/25/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


(defpackage "CONSTR")
(in-package "CONSTR") (use-package :ergolisp)

(eexport '(defconstr defmulticonstr))

;; Control variables.  Note that these must be set at compile time.
;; This mechanism doesn't work in Lucid.
(export '(proclaim-constrs-inline))


(defvar *proclaim-constrs-inline* nil
  "Whether the functions produced should be inline expanded (default false).
WARNING:  Because of what I think is a Lisp bug, once a constr function is
proclaimed inline expanding, it can never be changed (in the current Lisp
image).  This doesn't work.  It always insists on inline expanding.")

(defmacro proclaim-constrs-inline (flag)
  `(eval-when (compile load eval)
     (setq *proclaim-constrs-inline* ,flag)))


;;; Names of various ADT funs.

(defun constr-fun (adt)
  ;; constructor
  adt)

(defun keyword-constr-fun (adt)
  ;; keyword-oriented constructor
  (cat-syms 'make adt))

(defun reconstr-fun (adt)
  ;; constructor
  (cat-syms 're adt))

(defun sel-fun (adt sel)
  ;; selector
  (cat-syms adt sel))

(defun disc-fun (adt)
  ;; discriminator
  (cat-syms 'is adt))

(defun print-fun (adt)
  ;; printer
  (cat-syms 'print adt))

(defun print-orig-fun (adt)
  ;; name of original, in case the print-fun is changed
  (cat-syms (print-fun adt) 'orig))


(defun cat-syms (first &rest rest)
  (intern (format nil
		  "~:@(~a~{-~a~}~)"
		  first rest)))


(defun defconstr-expand (adt part-specs)
  ;; Do the expansion work for defconstr
  (let ((part-names (gen-names (mapcar #'car part-specs) 1))
	(part-types (mapcar #'cdr part-specs)))
    (let ((sel-funs  (mapcar #'(lambda (part) (sel-fun adt part)) part-names)))
      `(progn (defstruct (,adt
			  (:print-function ,(print-fun adt))
			  (:predicate ,(disc-fun adt))
			  (:constructor)
			  (:constructor ,(constr-fun adt) ,part-names))
		,@part-names)
	      ;; Declare inline or not.  Must come here!
	      (proclaim
	       '(,(if *proclaim-constrs-inline*
		      'inline
		      'notinline)
		 ,(constr-fun adt)
		 ,(disc-fun adt)
		 ,@sel-funs))
	      ;; Declare the constructor info so we can use dlet, dcase, etc.
	      (declare-constructor ,adt ,sel-funs
				   ,(disc-fun adt))
	      ;; Define the original version of the print function.
	      (ddefun ,(print-orig-fun adt)
		  ((:as ,adt (,adt ,@part-names)) stream _)
		(print-constr ,adt ',adt (list ,@part-names) stream))
	      (setf (symbol-function ',(print-fun adt))
		    (symbol-function ',(print-orig-fun adt)))
	      (proclaim-ftype ,(constr-fun adt) ,part-types ,adt)
	      (progn ,@(mapcar #'(lambda (name type)
				   `(proclaim-ftype ,(sel-fun adt name)
						    (,adt) ,type))
			       part-names part-types))
	      (export '(,adt ,(constr-fun adt) ,(reconstr-fun adt)
			     ,(keyword-constr-fun adt)
			     ,(disc-fun adt)
			     ,@sel-funs))))))

#+harlequin-common-lisp ;; compiler doesn't detect GTERM as a pkg nickname
(defpackage "TERM" (:nicknames "GTERM"))

(defun print-constr (obj adt-name parts stream)
  "Print a constr object given the object, its constructor name, a list
of parts, and a stream.  Calls unparse-term if the object's type is known to
ds-term and if gterm:*disable-auto-unparsing* is nil.  If *print-escape* is
true, put `` .. '' around the unparsing."
  (if (or (eq (gterm:ds-term-impl adt-name)
	      gterm:*default-ds-term-impl*)
	  gterm:*disable-auto-unparsing*)
      (progn
	;; Do not use format because we want to preserve *print-escape*.
	(write-string "#<" stream)
	(write adt-name :stream stream :pretty nil)
	(dolist (part parts)
	  (write-string " " stream)
	  (write part :stream stream :pretty nil))
	(write-string ">" stream))
      (let ((*print-circle* nil))
	(when *print-escape*
	  (write-string "``" stream))
	;; Turn off pretty-printing to avoid nasty system printer bug.
	;; Turn on ds-term caching and turn off attribute
	;; caching (used by the unparser).
	;; Changed to turn *print-escape* on while unparsing here.    -fp.
	(let ((*print-pretty* nil)
	      (*print-escape* t)
	      (gterm:*cache-ds-term-p* t)
	      (gterm:*default-get-term-attr-impl*
	       #'gterm:nil-get-term-attr-impl)
	      (gterm:*default-set-term-attr-impl*
	       #'gterm:nil-set-term-attr-impl))
	  (sbrt:unparse-term obj :stream stream)
	  (gterm:clear-ds-term))
	(when *print-escape*
	  (write-string "''" stream)))))

(defun gen-names (given-names i)
  "Generate names from the ones given, replacing NILs by ordinals."
  (if (null given-names)
      ()
      (cons (or (first given-names)
		(intern (format nil "~:@(~:R~)" i)))
	    (gen-names (rest given-names) (1+ i)))))

	 


(defmacro defconstr (adt part-specs)
  (defconstr-expand adt part-specs))

(defmacro defmulticonstr (rectype alts)
  "Define a \"concrete type\", similar to ML"
  (let ((constrs (mapcar #'first alts))
	(disc (disc-fun rectype)))
    `(progn (export '(,rectype ,disc))
	    (deftype ,rectype ()
	      '(or ,@constrs))
	    (defun ,disc (obj)
	      (typep obj ',rectype))
	    ,@(mapcar #'(lambda (alt)
			  `(defconstr ,@alt))
		      alts))))

