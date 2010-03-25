;;; -*- Package: NEWATTR -*-
;;; Sccs Id 12/18/89 @(#)attr-lib.lisp	2.8
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; Attribute examples and extensions.
;;; Author: fp

(in-package "NEWATTR")  (use-package :ergolisp)

(export '(empty-gcon defemptysyn empty-gsyn))
(export '(termself))
(export '(undefined mvb mvs vls))


;;; Some useful macros.

(defconstant undefined 'undefined)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun is-underline (form)
  (and (symbolp form) (string-equal (symbol-name form) "_")))

(defun replace-underline (vars)
  "Returns 2 values: first is the list where each underscore has been replaced
by a temporary variable, second is the list of those temporary variables."
  (do ((vars vars (cdr vars))
       (newvars nil (cons nextvar newvars))
       (ignorevars nil (if (eq var nextvar)
			   ignorevars
			   (cons nextvar ignorevars)))
       (var)
       (nextvar))
      ((null vars) (values (nreverse newvars) ignorevars))
    (setq var (car vars))
    (setq nextvar (if (string-equal (symbol-name var) "_")
		      (gensym)
		      var))))

)

(defmacro mvb (vars values-form &rest body)
  "Like multiple-value-bind, except that it allows underscores in don't care
positions among the variables to be bound."
  (multiple-value-bind (newvars ignorevars)
      (replace-underline vars)
    `(multiple-value-bind ,newvars ,values-form
        (declare (ignore ,@ignorevars))
	,@body)))

(defmacro mvs (vars values-form)
  "Like multiple-value-setq, except that it allows underscores in don't care
positions among the variables to be bound."
  (multiple-value-bind (newvars ignorevars)
      (replace-underline vars)
    `(let (,@ignorevars)
       (declare (ignore ,@ignorevars))
       (multiple-value-setq ,newvars ,values-form))))


(defmacro vls (&rest args)
  "Like values, except that it allows underscores, which are replaced
by the predefined constant UNDEFINED."
  `(values ,@(substitute-if 'undefined #'is-underline args)))

;;; The empty context.

(defgcon empty-gcon
  "The empty global context.")

(defgcon-deltafun empty-gcon (n term) ()
  (declare (ignore n term))
  (vls))

;;; The empty syntext, given a context.

(defmacro defemptysyn (syn-name lang-name con-name)
  "Given a context, defines the empty syntext which depends on it."
  `(progn
     (defsyn ,syn-name ,lang-name ,con-name)
     (defsyn-compfun ,syn-name (term) (&rest con-vars)
       (declare (ignore term con-vars))
       (vls))))

(defmacro defemptygsyn (gsyn-name gcon-name)
  "Given a global context, defines the global empty syntext which
depends on it."
  `(defemptysyn ,gsyn-name :global ,gcon-name))

;;; The global empty syntext of the global empty context.

(defemptygsyn empty-gsyn empty-gcon)

;;; The trivial attribute: the term itself.

(defgattr termself empty-gsyn
  "The global attribute with the term itself.")

(defgattr-attrfun termself (term) () ()
  (vls term))
