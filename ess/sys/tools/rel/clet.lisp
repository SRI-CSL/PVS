;;; -*- Mode: Lisp; Package: clet; log: tools-changes.log -*-
;;; Sccs Id @(#)clet.lisp	2.4 9/26/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(defpackage "CLET")
(in-package "CLET") (use-package :ergolisp)

;;; Next two forms to be superseded by an eexport eventually.
(export '(lisp::clet*) :lisp)
(export '(clet*))

;;; Evaluate code in a peculiar environment.
;;; Var&vals is a list of things of the form (variable value . cleanup)
;;; Variable is a variable to bind, or a list thereof.
;;; Value is a value to bind variable to.  If variable is a list of variables, 
;;;    they are bound using multiple-value-bind to the values returned by value.
;;; Cleanup is a list of forms.
;;; Code is evaluated in an environment where variable is bound to value.
;;; After code terminates (whether normally or abnormally), the forms in
;;;    cleanup are evaluated sequentially in the same environment.
;;; Variable is allowed to be a list of variables, in which case they are
;;;    bound to the corresponding multiple values returned by value.
;;; It is also possible to have var&val be a symbol.  In that case, code is 
;;;    evaluated in an environment where value is bound to nil.
;;; ANY VARIABLES NAMED "IGNORE" ARE REPLACED BY GENSYM'S AND ARE DECLARED TO BE
;;;    IGNORED.
;;; Example:
;;; (clet* ((this (make-big-object 3) (cleanup-big-object this))
;;;	    ((that theother) (make-two-blue-objects 4)
;;;	     (cleanup-blue-object that)
;;;	     (cleanup-blue-object theother))
;;;	    (ignore (format t "This is evaluated for side effects.~%")
;;;		    (format t "This is evaluated on the way out.~%"))
;;;	   (format t "This form is in the body of the clet*.~%"))
;;; In this example, if the call to make-two-blue-objects fails, 
;;; cleanup-big-object will still be called.  make-two-blue-object should return
;;; two values.  Assuming that no errors occur early on, the cleanup forms will
;;; be evaluated in the following order:
;;;   (format t "This is evaluated on the way out.~%")
;;;   (cleanup-blue-object that)
;;;   (cleanup-blue-object theother)
;;;   (cleanup-big-object this)
	   
(defmacro clet* (vars&vals &body code)
    "Like let* but also does unwind-protect's.  C is for Cleanup."
    (labels ((ignorep (sym) (equal (symbol-name sym) "IGNORE"))
	     (make-ignores (ignore-list)
		 (if ignore-list
		     `((declare (ignore . ,ignore-list)))
		     nil))
	     (clet*1 (var&val code)
		     (let (var val cleanup (ignore-list '()))
			  (if (symbolp var&val)
			      (setq cleanup nil
				    var var&val
				    val nil)
			      (setq cleanup (cddr var&val)
				    var (car var&val)
				    val (cadr var&val)))
			  ;; Ignore some variables.
			  (if (symbolp var)
			      (when (ignorep var)
				    (setq var (gensym))
				    (push var ignore-list))
			      (setq var
				    (mapcar #'(lambda (var &aux (gensym nil))
						      (if (ignorep var)
							  (progn
							    (setq gensym
								  (gensym))
							    (push gensym
								  ignore-list)
							    gensym)
							  var))
					    var)))
			  ;; Make an unwind-protect if cleanup is nontrivial.
			  (when cleanup (setq code `((unwind-protect
							 (progn . ,code)
							 (progn . ,cleanup)))))
			  (if (listp var)
			      `(multiple-value-bind ,var ,val
				   ,@(make-ignores ignore-list) . ,code)
			      `(let ((,var ,val)) ,@(make-ignores ignore-list)
				    . ,code))))
	     (cletrec (vars&vals code)
		 (if (null vars&vals)
		     `(progn . ,code)
		     (clet*1 (car vars&vals)
			     (list (cletrec (cdr vars&vals) code))))))
	    (cletrec vars&vals code)))

#|
(progn
    (defun ignorep (sym) (equal (symbol-name sym) "IGNORE"))
    
    (defun make-ignores (ignore-list)
	   (if ignore-list
	       `((declare (ignore . ,ignore-list)))
	       nil))
    
    (defun clet*1 (var&val code &aux var val cleanup (ignore-list '()))
	   (if (symbolp var&val)
	       (setq cleanup nil
		     var var&val
		     val nil)
	       (setq cleanup (cddr var&val)
		     var (car var&val)
		     val (cadr var&val)))
	   ;; Ignore some variables.
	   (if (symbolp var)
	       (when (ignorep var)
		     (setq var (gensym))
		     (push var ignore-list))
	       (setq var
		     (mapcar #'(lambda (var &aux (gensym nil))
				       (if (ignorep var)
					   (progn
						 (setq gensym (gensym))
						 (push gensym ignore-list)
						 gensym)
					   var))
			     var)))
	   ;; Make an unwind-protect if cleanup is nontrivial.
	   (when cleanup (setq code `((unwind-protect
					  (progn . ,code)
					  (progn . ,cleanup)))))
	   (if (listp var)
	       `(multiple-value-bind ,var ,val
		    ,@(make-ignores ignore-list) . ,code)
	       `(let ((,var ,val)) ,@(make-ignores ignore-list) . ,code)))
    
    (defun cletrec (vars&vals code)
	   (if (null vars&vals)
	       `(progn . ,code)
	       (clet*1 (car vars&vals)
		       (list (cletrec (cdr vars&vals) code)))))
    
    (defmacro clet* (vars&vals &body code)
	"Like let* but also does unwind-protect's.  C is for Cleanup."
	(cletrec vars&vals code)))
|#
