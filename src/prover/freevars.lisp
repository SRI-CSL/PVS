;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; freevars.lisp -- 
;; Author          : Sam Owre & N. Shankar
;; Created On      : Wed Aug 17 00:48:46 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:38:17 1998
;; Update Count    : 2
;; Status          : Alpha test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defmacro fv-union (fv1 fv2)
  `(union ,fv1 ,fv2 :test #'same-declaration))

(defun freevars (obj)
;;  (sort (freevars* obj  frees)
;;	#'(lambda (x y) (occurs-in x (type y))))
  (freevars* obj nil))

(defmethod freevars* :around ((expr expr) frees)
  (with-slots (free-variables) expr
    (if (eq free-variables 'unbound)
	(call-next-method)
	(if frees
	    (append (set-difference free-variables frees
				    :test #'same-declaration)
		    frees)
	    free-variables))))

(defmethod freevars* :around ((texpr type-expr) frees) 
  (with-slots (free-variables) texpr
    (if (eq free-variables 'unbound)
	(call-next-method)
	(remove-duplicates (append free-variables frees)
	  :test #'same-declaration))))

(defmethod freevars* ((expr projection-application) frees)
  (let ((efrees (freevars* (argument expr) nil)))
    (setf (free-variables expr) efrees)
    (fv-union efrees frees)))

(defmethod freevars* ((expr field-application) frees)
  (let ((efrees (freevars* (argument expr) nil)))
    (setf (free-variables expr) efrees)
    (fv-union efrees frees)))

(defmethod freevars* ((expr application) frees)
  (let* ((ofrees (freevars* (operator expr) nil))
	 (afrees (freevars* (argument expr) nil))
	 (efrees (fv-union ofrees afrees)))
    (setf (free-variables expr) efrees)
    (fv-union efrees frees)))

(defmethod freevars* ((list list) frees)
  (freevars-list list frees))

(defun freevars-list (list frees)
  (if (null list)
      frees
      (let* ((frees-car (freevars* (car list) frees))
	     (frees-cdr (freevars-list (cdr list) frees-car)))
	(if (and (typep (car list) '(or binding field-decl))
		 (not (member (car list) frees-car :test #'same-declaration)))
	    (remove (car list) frees-cdr :test #'same-declaration)
	    frees-cdr))))


(defmethod declaration ((expr field-decl))
  expr)
(defmethod declaration ((expr dep-binding))
  expr)

(defmethod freevars* ((expr bind-decl) frees)
  (freevars* (type expr) frees))

(defmethod no-freevars? ((expr bind-decl))
  (no-freevars? (type expr)))

(defmethod freevars* ((expr binding-expr) frees)
  (let* ((frees-expression (freevars* (expression expr) nil))
	 (frees-bindlist (freevars* (bindings expr) nil))
	 (diff (set-difference frees-expression (bindings expr)
			       :test #'same-declaration))
	 (nfrees (fv-union frees-bindlist diff)))
    (setf (free-variables expr) nfrees)
    (fv-union nfrees frees)))

(defmethod freevars* ((expr name-expr) frees)
  (with-slots (kind type resolutions free-variables) expr
    (assert kind)
    (cond ((variable? expr)
	   (let* ((tfrees (freevars* type nil))
		  (ufrees (fv-union tfrees frees)))
	     (setf free-variables (cons (copy expr) tfrees))
	     (if (not (member expr ufrees :test #'same-declaration))
		 (cons (copy expr) ufrees)
		 ufrees)))
	  (t (let ((afrees (freevars* (actuals (module-instance
						(car resolutions)))
			     nil)))
	       (setf free-variables afrees)
	       (fv-union afrees frees))))))

(defmethod freevars* ((res resolution) frees)
  (with-slots (module-instance) res
    (fv-union (freevars* (actuals module-instance) nil) frees)))

(defmethod freevars* ((expr field-name-expr) frees)
  (assert (kind expr))
  (cond ((variable? expr)
	 (let* ((tfrees (freevars* (type expr) nil))
		(ufrees (fv-union tfrees frees)))
	   (setf (free-variables expr) (cons expr tfrees))
	   (if (not (member expr ufrees :test #'same-declaration))
	       (cons expr ufrees)
	       ufrees)))
	(t (let* ((afrees (freevars* (actuals (module-instance
					       (resolution expr)))
				     nil))
		  (tfrees (freevars* (type expr) nil))
		  (ufrees (fv-union afrees tfrees)))
	     (setf (free-variables expr) ufrees)
	     (fv-union frees ufrees)))))

(defmethod freevars* ((expr projection-expr) frees)
  (let ((tfrees (freevars* (type expr) nil)))
    (setf (free-variables expr) tfrees)
    (fv-union tfrees frees)))

(defmethod freevars* ((expr record-expr) frees)
  (let ((rfrees (freevars* (assignments expr) nil)))
    (setf (free-variables expr) rfrees)
    (fv-union rfrees frees)))

(defmethod freevars* ((expr tuple-expr) frees)
  (let ((efrees (freevars* (exprs expr) nil)))
    (setf (free-variables expr) efrees)
    (fv-union efrees frees)))

(defmethod freevars* ((expr update-expr) frees)
  (let* ((efrees (freevars* (expression expr) nil))
	 (afrees (freevars* (assignments expr) nil))
	 (tfrees (freevars* (type expr) nil))
	 (ufrees (fv-union efrees (fv-union afrees tfrees))))
    (setf (free-variables expr) ufrees)
    (fv-union ufrees frees)))

(defmethod freevars* ((expr assignment) frees)
  (fv-union (fv-union (freevars* (expression expr) nil)
		      (freevars* (arguments expr) nil))
	    frees))

(defmethod freevars* ((expr field-assignment-arg) frees)
  frees)

(defmethod no-freevars? ((expr assignment))
  (and (no-freevars? (arguments expr))
       (no-freevars? (expression expr))))

(defmethod freevars* ((expr cases-expr) frees)
  (let* ((tfrees (freevars* (type expr) nil))
	 (sfrees (freevars* (selections expr) nil))
	 (elfrees (freevars* (else-part expr) nil))
	 (exfrees (freevars* (expression expr) nil))
	 (efrees (fv-union tfrees
			   (fv-union sfrees (fv-union elfrees exfrees)))))
    (setf (free-variables expr) efrees)
    (fv-union efrees frees)))

(defmethod freevars* ((expr selection) frees)
  (set-difference (freevars* (expression expr) frees)
		  (args expr)
		  :test #'same-declaration))

(defmethod no-freevars? ((expr selection))
  (no-freevars? (expression expr)))

(defmethod freevars* ((expr actual) frees)
  (if (type-value expr)
      (freevars* (type-value expr) frees)
      (freevars* (expr expr) frees)))

(defmethod no-freevars? ((expr actual))
  (and (no-freevars? (expr expr))
       (no-freevars? (type-value expr))))

(defmethod freevars* ((expr expr) frees)
  frees)

(defmethod no-freevars? ((list list))
  (every@ #'no-freevars? list))

(defmethod freevars* ((mn modname) frees)
  (let ((afrees (freevars* (actuals mn) nil)))
    (fv-union afrees frees)))

(defmethod freevars* ((texpr type-name) frees)
  (let* ((act (actuals (module-instance (resolution texpr))))
	 (afrees (freevars* act nil)))
    (setf (free-variables texpr) afrees)
    (fv-union afrees frees)))

(defmethod freevars* ((texpr type-application) frees)
  (let* ((tfrees (freevars* (type texpr) nil))
	 (pfrees (freevars* (parameters texpr) nil))
	 (nfrees (fv-union tfrees pfrees)))
    (setf (free-variables texpr) nfrees)
    (fv-union nfrees frees)))

(defmethod freevars* ((texpr subtype) frees)
  (let* ((tfrees (freevars* (supertype texpr) nil))
	 (pfrees (freevars* (predicate texpr) nil))
	 (ufrees (fv-union tfrees pfrees)))
    (setf (free-variables texpr) ufrees)
    (fv-union ufrees frees)))

(defmethod freevars* ((texpr expr-as-type) frees)
  (if (predicate texpr)
      (call-next-method)
      (let ((tfrees (freevars* (expr texpr) nil)))
	(setf (free-variables texpr) tfrees)
	(fv-union tfrees frees))))

(defmethod freevars* ((texpr funtype) frees)
  (with-slots (domain range) texpr
    (let* ((dfrees (freevars* domain nil))
	   (rfrees (freevars* range nil))
	   (ufrees (fv-union dfrees rfrees))
	   (tfrees (if (typep domain 'dep-binding)
		       (remove-if #'(lambda (d) (eq (declaration d) domain))
			 ufrees)
		       ufrees)))
      (setf (free-variables texpr) tfrees)
      (fv-union tfrees frees))))

(defmethod freevars* ((texpr tupletype) frees)
  (let ((tfrees (freevars* (types texpr) nil)))
    (setf (free-variables texpr) tfrees)
    (fv-union tfrees frees)))

(defmethod freevars* ((texpr recordtype) frees)
  (let ((ffrees (freevars* (fields texpr) nil))) 
    (setf (free-variables texpr) ffrees)
    (fv-union ffrees frees)))

(defmethod freevars* ((texpr field-decl) frees)
  (freevars* (type texpr) frees))

(defmethod no-freevars? ((texpr field-decl))
  (no-freevars? (type texpr)))

(defmethod freevars* ((te dep-binding) frees)
  (freevars* (type te) frees))

(defmethod freevars* ((sym symbol) frees)
  frees)

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
