;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; freevars.lisp -- 
;; Author          : Sam Owre & N. Shankar
;; Created On      : Wed Aug 17 00:48:46 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 21:26:13 2004
;; Update Count    : 3
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002-2004 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;collect free variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From Sam: 
;;; freevars - returns the free variables of an expression - a list of
;;; name-exprs.  The
;;; returned list has no duplication, and the original expression is not
;;; modified.  Note that the formula must be typechecked, as this is the
;;; only way to determine whether a name is a variable.

;;; This is faster than simple union, as (in allegro)
;;;   (union '(a b c) nil) returns '(c b a)
(defmacro fv-union (fv1 fv2)
  (let ((efv1 (gentemp))
	(efv2 (gentemp)))
    `(let ((,efv1 ,fv1)
	   (,efv2 ,fv2))
       (if ,efv1
	   (if ,efv2
	       (union ,efv1 ,efv2 :test #'same-declaration)
	       ,efv1)
	   ,efv2))))

(defun freevars (obj)
;;  (sort (freevars* obj  frees)
;;	#'(lambda (x y) (occurs-in x (type y))))
  (freevars* obj))

(defmethod freevars* ((theory datatype-or-module))
  nil)

(defmethod freevars* ((decl declaration))
  nil)

(defmethod freevars* ((exp exporting))
  nil)

(defmethod freevars* ((imp importing))
  nil)

(defmethod freevars* :around ((expr expr))
  (with-slots (free-variables) expr
    (if (eq free-variables 'unbound)
	(setf free-variables (call-next-method))
	free-variables)))

(defmethod freevars* :around ((texpr type-expr)) 
  (with-slots (free-variables) texpr
    (if (eq free-variables 'unbound)
	(setf free-variables (call-next-method))
	free-variables)))

(defmethod freevars* ((expr projection-application))
  (freevars* (argument expr)))

(defmethod freevars* ((expr injection-application))
  (freevars* (argument expr)))

(defmethod freevars* ((expr injection?-application))
  (freevars* (argument expr)))

(defmethod freevars* ((expr extraction-application))
  (freevars* (argument expr)))

(defmethod freevars* ((expr field-application))
  (freevars* (argument expr)))

(defmethod freevars* ((expr application))
  (let* ((ofrees (freevars* (operator expr)))
	 (afrees (freevars* (argument expr))))
    (fv-union ofrees afrees)))

(defmethod freevars* ((list list))
  (freevars-list (reverse list) nil))

(defun freevars-list (list result)
  (if (null list)
      result
      (let* ((frees-car (freevars* (car list)))
	     (ufrees (fv-union frees-car result)))
	#+pvsdebug (assert (not (and (binding? (car list))
				     (member (car list) frees-car
					     :test #'same-declaration))))
	(freevars-list
	 (cdr list)
	 (if (binding? (car list))
	     (remove (car list) ufrees :test #'same-declaration)
	     ufrees)))))

(defmethod freevars* ((conv conversion-result))
  (freevars* (expr conv)))

(defmethod declaration ((expr field-decl))
  expr)

(defmethod declaration ((expr dep-binding))
  expr)

(defmethod declaration ((expr bind-decl))
  expr)

(defmethod freevars* ((expr bind-decl))
  (freevars* (type expr)))

(defmethod no-freevars? ((expr bind-decl))
  (no-freevars? (type expr)))

(defmethod freevars* ((expr binding-expr))
  (let* ((frees-expression (freevars* (expression expr)))
	 (frees-bindlist (freevars* (bindings expr)))
	 (diff (set-difference frees-expression (bindings expr)
			       :test #'same-declaration)))
    (fv-union frees-bindlist diff)))

(defmethod freevars* ((expr name-expr))
  (with-slots (type resolutions free-variables) expr
    (if (variable? expr)
	(let ((tfrees (freevars* type)))
	  (cons (copy expr) tfrees))
	(freevars* (actuals (module-instance (car resolutions)))))))

(defmethod freevars* ((res resolution))
  (with-slots (module-instance) res
    (freevars* (actuals module-instance))))

(defmethod freevars* ((expr field-name-expr))
  (if (variable? expr)
      (let ((tfrees (freevars* (type expr))))
	(cons expr tfrees))
      (let* ((afrees (freevars* (actuals (module-instance
					  (resolution expr)))))
	     (tfrees (freevars* (type expr))))
	(fv-union afrees tfrees))))

(defmethod freevars* ((expr projection-expr))
  (freevars* (type expr)))

(defmethod freevars* ((expr injection-expr))
  (freevars* (type expr)))

(defmethod freevars* ((expr injection?-expr))
  (freevars* (type expr)))

(defmethod freevars* ((expr extraction-expr))
  (freevars* (type expr)))

(defmethod freevars* ((expr record-expr))
  (freevars* (assignments expr)))

(defmethod freevars* ((expr tuple-expr))
  (freevars* (exprs expr)))

(defmethod freevars* ((expr update-expr))
  (let* ((efrees (freevars* (expression expr)))
	 (afrees (freevars* (assignments expr)))
	 (tfrees (freevars* (type expr))))
    (fv-union efrees (fv-union afrees tfrees))))

(defmethod freevars* ((expr assignment))
  (fv-union (freevars* (expression expr))
	    (freevars* (arguments expr))))

(defmethod freevars* ((expr field-assignment-arg))
  nil)

(defmethod no-freevars? ((expr assignment))
  (and (no-freevars? (arguments expr))
       (no-freevars? (expression expr))))

(defmethod freevars* ((expr cases-expr))
  (let* ((tfrees (freevars* (type expr)))
	 (sfrees (freevars* (selections expr)))
	 (elfrees (freevars* (else-part expr)))
	 (exfrees (freevars* (expression expr))))
    (fv-union tfrees (fv-union sfrees (fv-union elfrees exfrees)))))

(defmethod freevars* ((expr selection))
  (let ((cfrees (freevars* (constructor expr))))
    (fv-union cfrees
	      (set-difference (freevars* (expression expr)) (args expr)
			      :test #'same-declaration))))

(defmethod no-freevars? ((expr selection))
  (no-freevars? (expression expr)))

(defmethod freevars* ((expr actual))
  (if (type-value expr)
      (freevars* (type-value expr))
      (freevars* (expr expr))))

(defmethod no-freevars? ((expr actual))
  (and (no-freevars? (expr expr))
       (no-freevars? (type-value expr))))

(defmethod freevars* ((map mapping-rhs))
  (if (type-value map)
      (freevars* (type-value map))
      (freevars* (expr map))))

(defmethod no-freevars? ((expr mapping-rhs))
  (and (no-freevars? (expr expr))
       (no-freevars? (type-value expr))))

(defmethod freevars* ((expr expr))
  nil)

(defmethod no-freevars? ((list list))
  (every #'no-freevars? list))

(defmethod freevars* ((mn modname))
  (freevars* (actuals mn)))

(defmethod freevars* ((texpr type-var))
  nil)

(defmethod freevars* ((texpr type-name))
  (freevars* (actuals (module-instance (resolution texpr)))))

(defmethod freevars* ((texpr type-application))
  (let* ((tfrees (freevars* (type texpr)))
	 (pfrees (freevars* (parameters texpr))))
    (fv-union tfrees pfrees)))

(defmethod freevars* ((texpr subtype))
  (let* ((tfrees (freevars* (supertype texpr)))
	 (pfrees (freevars* (predicate texpr))))
    (fv-union tfrees pfrees)))

(defmethod freevars* ((texpr expr-as-type))
  (if (predicate texpr)
      (call-next-method)
      (freevars* (expr texpr))))

(defmethod freevars* ((texpr funtype))
  (with-slots (domain range) texpr
    (let* ((dfrees (freevars* domain))
	   (rfrees (freevars* range)))
      (fv-union dfrees
		(if (typep domain 'dep-binding)
		    (remove-if #'(lambda (d) (eq (declaration d) domain))
		      rfrees)
		    rfrees)))))

(defmethod freevars* ((texpr tupletype))
  (freevars* (types texpr)))

(defmethod freevars* ((texpr cotupletype))
  (freevars* (types texpr)))

(defmethod freevars* ((texpr recordtype))
  (freevars* (fields texpr)))

(defmethod freevars* ((texpr field-decl))
  (freevars* (type texpr)))

(defmethod no-freevars? ((texpr field-decl))
  (no-freevars? (type texpr)))

(defmethod freevars* ((te dep-binding))
  (freevars* (type te)))

(defmethod freevars* ((sym symbol))
  nil)

(defmethod no-freevars? ((texpr dep-binding))
  (no-freevars? (type texpr)))

(defmethod no-freevars? ((texpr type-expr))
  (with-slots (free-variables) texpr
    (and (not (eq free-variables 'unbound))
	 (null free-variables))))

(defmethod no-freevars? ((expr expr))
  (with-slots (free-variables) expr
    (and (not (eq free-variables 'unbound))
	 (null free-variables))))

(defmethod freevars* ((expr template))
  (freevars* (template-expression expr)))

(defmethod freevars* ((expr simple-constructor))
  nil)
