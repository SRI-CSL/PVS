;;; -*- Mode: Lisp; Package: TDEFUN -*-
;;;
;;; Alternative to defun, given an explicit function type and doing argument
;;; destructuring.  Also, similar alternatives to defvar and defstype.
;;;
;;; Author: Conal Elliott.  Last Modified Sat Sep 23 12:39:24 1989
;;;
;;; Sccs Id @(#)tdefun.lisp	1.3 9/23/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(defpackage "TDEFUN")
(in-package :TDEFUN) (use-package :ergolisp)

(eexport '(tdefun tdefvar defstype))

(defmacro tdefun (name arg-pats type-spec doc-string &rest rest)
  "Like ddefun, but list of arg patterns is followed by a type specification, as
used by defsconstr.  Doc-string is mandatory, and is supplemented by the
type-spec."
  ;; Until dlambda is optimized, this macro ought to use defun instead of
  ;; ddefun when possible.
  `(progn (proclaim-sftype ',name ,type-spec)
	  (ddefun ,name ,arg-pats
	    ,(concatenate 'string
	      doc-string "
Type is ``"
	      type-spec "''")
	    ,@rest)))

(defun proclaim-sftype (name type-spec)
  "Like proclaim-ftype, but the type-spec is in sconstr-style concrete syntax.
Doesn't really do anything yet."
  ;; I don't want to parse each time.  Parsing should be need driven (with
  ;; result cached). 
  (declare (ignore name type-spec))
  )

(defmacro tdefvar (name type-spec &optional (init-value nil init-p)
			(doc-string nil))
  "Like defvar, but after variable name, there is a mandatory type spec,
as in tdefun."
  (let ((doc (if doc-string
		 (concatenate 'string
		   doc-string "
Type is ``"
		   type-spec "''")
		 (concatenate 'string
		   "Undocumented.  Type is " type-spec))))
    (if init-p
	`(defvar ,name ,init-value ,doc)
	`(progn (defvar ,name)
		(setf (documentation ',name 'variable) ,doc)
		',name))))

;;; The syntactic types should be factored out of the sconstr grammar.

(defmacro defstype (name args &rest rest)
  "Like deftype, but type spec is in sconstr-style concrete syntax."
  (multiple-value-bind (doc-string type-spec)
      (case (length rest)
	(1 (values (first rest) (first rest)))
	(2 (values
	    (concatenate 'string
	      (first rest) "
Actual type is ``"
	      (second rest) "''")
	    (second rest)))
	(t (error "defstype must have three or four arguments")))
    `(deftype ,name ,args
       ,doc-string
       ',(constr-term-rep::fix-prods
	  (constrg:constr-parse :nt 'constr-term-rep::Ptype
				:string type-spec)))))
