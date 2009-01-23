;;; -*- Mode: Lisp; Package: GTERM -*-
;;; Sccs Id 9/21/89 @(#)attr-prims.lisp	1.4
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;;
;;; Attribute caching support for generic terms.
;;;
;;; Author: Conal.  Last Modified Sat Nov  5 13:19:02 1988
;;; Revised by TSF 13 March 1988
;;;
;;; This is one implementation of attribute slots which uses a hash
;;; table to store the attributes.  It doesn't require any special
;;; features from the terms, so it is the default method for storing
;;; attributes.

(eval-when (compile load eval)
  (unless (find-package "TERM")
    (make-package "TERM" :nicknames '("GTERM")
			 :use '(:cl-user :common-lisp :ergolisp))))
(in-package "TERM")

(export '(attr-clear-one attr-clear-all))

(defvar *attr-table*)			; The hash table

(defun get-hash-attribute (term)
  "Attribute information associated with a term.  Not for general use."
  (declare-ftype get-hash-attribute (Term) T)
  (gethash term *attr-table*))

(defun set-hash-attribute (term value)
  "Set attribute info associated with a term.  Not for general use."
  (declare-ftype set-hash-attribute (Term T) T)
  (setf (gethash term *attr-table*)
	value))

;;; Defsetf method established in terms.lisp

;;;; Clearing all or one cache.

(defun attr-clear-all ()
  "Clear out the attribute cache."
  (declare-ftype attr-clear-all () )
  (setq *attr-table* (make-hash-table :test 'eq))
  (values))

(defun attr-clear-one (term)
  "Remove the attribute cache for one term (pointer)"
  (declare-ftype attr-clear-one (Term) Boolean)
  (remhash term *attr-table*))


(attr-clear-all)			; Initialize the table.

