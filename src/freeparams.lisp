;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; freeparams.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Jun  4 01:33:50 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jun  9 21:24:13 1994
;; Update Count    : 6
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;;; Collect the free parameters of an object.
;;; Returns a list of formal decls.

(defun free-params (obj)
  (let ((frees (free-params* obj nil)))
    ;;(assert (null (set-exclusive-or frees (free-params* obj nil))))
    frees))

(defun no-free-params? (obj)
  (null (free-params obj)))

(defun fully-instantiated? (obj)
  (let ((frees (free-params obj)))
    (or (null frees)
	(let ((formals (formals-sans-usings (current-theory))))
	  (every #'(lambda (fp) (memq fp formals)) frees)))))

;;; Theory

(defmethod free-params* ((theory datatype-or-module) frees)
  (declare (ignore frees))
  (formals-sans-usings theory))

(defmethod free-params* ((decl declaration) frees)
  (free-params* (module decl) frees))

(defmethod free-params* ((importing importing) frees)
  (free-params* (theory-name importing) frees))

(defmethod free-params* ((exporting exporting) frees)
  (declare (ignore frees))
  nil)

;;; Type expressions

(defmethod free-params* :around ((texpr type-expr) frees) 
  (with-slots (free-parameters print-type) texpr
    (when (eq free-parameters 'unbound)
      (let ((pfrees (free-params* print-type nil)))
	(call-next-method)
	(dolist (pf pfrees)
	  (pushnew pf free-parameters :test #'eq))))
    (union free-parameters frees :test #'eq)))

(defmethod free-params* ((texpr type-application) frees)
  (let ((nfrees (free-params* (type texpr)
		  (free-params* (parameters texpr) nil))))
    (setf (free-parameters texpr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((texpr type-name) frees)
  (let ((nfrees (call-next-method texpr nil)))
    (setf (free-parameters texpr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((texpr subtype) frees)
  (let ((tfrees (free-params* (supertype texpr) 
		  (free-params* (predicate texpr) nil))))
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))

(defmethod free-params* ((texpr expr-as-type) frees)
  (call-next-method)
  (let ((efrees (free-params* (expr texpr) nil)))
    (setf (free-parameters texpr)
	  (union efrees (free-parameters texpr) :test #'eq)))
    (union frees (free-parameters texpr)))

(defmethod free-params* ((texpr funtype) frees)
  (let ((dfrees (free-params* (domain texpr) 
		  (free-params* (range texpr) nil))))
    (setf (free-parameters texpr) dfrees)
    (union dfrees frees :test #'eq)))

(defmethod free-params* ((texpr tupletype) frees)
  (let ((tfrees (free-params* (types texpr) nil)))
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))

(defmethod free-params* ((texpr cotupletype) frees)
  (let ((tfrees (free-params* (types texpr) nil)))
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))

(defmethod free-params* ((texpr recordtype) frees)
  (let ((ffrees (free-params* (fields texpr) nil))) 
    (setf (free-parameters texpr) ffrees)
    (union ffrees frees :test #'eq)))

(defmethod free-params* ((texpr field-decl) frees)
  (free-params* (type texpr) frees))

(defmethod free-params* ((te dep-binding) frees)
  (free-params* (type te) frees))


;;; Expressions

(defmethod free-params* :around ((expr expr) frees)
  (with-slots (free-parameters) expr
    (if (eq free-parameters 'unbound)
	(call-next-method)
	(remove-duplicates (append free-parameters frees)
	  :test #'same-declaration))))


(defmethod free-params* ((expr application) frees)
  (let ((efrees (free-params* (operator expr)
		  (free-params* (argument expr)
		    (free-params* (type expr) nil)))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((list list) frees)
  (free-params-list list frees))

(defun free-params-list (list frees)
  (if (null list)
      frees
      (free-params-list (cdr list) (free-params* (car list) frees))))

(defmethod free-params* ((expr bind-decl) frees)
  (let ((tfrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) tfrees)
    (union frees tfrees :test #'eq)))

(defmethod free-params* ((expr binding-expr) frees)
  (let* ((efrees (free-params* (expression expr) 
		   (free-params* (bindings expr) nil))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr name-expr) frees)
  (let ((nfrees (free-params* (type expr)
		  (when (constant? expr)
		    (call-next-method expr nil)))))
    (setf (free-parameters expr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((expr number-expr) frees)
  (setf (free-parameters expr) nil)
  frees)

(defmethod free-params* ((expr projection-expr) frees)
  (let ((afrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr injection-expr) frees)
  (let ((afrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr injection?-expr) frees)
  (let ((afrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr extraction-expr) frees)
  (let ((afrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr projection-application) frees)
  (let ((afrees (free-params* (argument expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr injection-application) frees)
  (let ((afrees (free-params* (argument expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr injection?-application) frees)
  (let ((afrees (free-params* (argument expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr extraction-application) frees)
  (let ((afrees (free-params* (argument expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr field-application) frees)
  (let ((afrees (free-params* (argument expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

(defmethod free-params* ((expr record-expr) frees)
  (let ((rfrees (free-params* (assignments expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) rfrees)
    (union rfrees frees :test #'eq)))

(defmethod free-params* ((expr tuple-expr) frees)
  (let ((efrees (free-params* (exprs expr)
		  (free-params* (type expr) nil))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr update-expr) frees)
  (let ((efrees (free-params* (type expr) 
		  (free-params* (assignments expr) 
		    (free-params* (expression expr) nil)))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr assignment) frees)
  (free-params* (expression expr)
    (free-params* (arguments expr) frees)))

(defmethod free-params* ((expr cases-expr) frees)
  (let ((efrees (free-params* (type expr) 
		  (free-params* (selections expr) 
		    (free-params* (else-part expr) 
		      (free-params* (expression expr) nil))))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr selection) frees)
  (free-params* (expression expr)
    (free-params* (constructor expr)
      (free-params* (args expr) frees))))


;;; Names

(defmethod free-params* ((mi modname) frees)
  (with-slots (actuals) mi
    (if actuals
	(let ((afrees (free-params* actuals nil)))
	  (union afrees frees :test #'eq))
	(let ((theory (get-theory mi)))
	  (dolist (x (formals-sans-usings theory))
	    (setq frees (pushnew x frees :test #'eq)))
	  frees))))

(defmethod free-params* ((map mapping-rhs) frees)
  (with-slots (expr type-value) map
    (if type-value
	(free-params* type-value frees)
	(free-params* expr frees))))

(defmethod free-params* ((name name) frees)
  (with-slots (resolutions) name
    (free-params* (car resolutions) frees)))

(defmethod free-params* ((res resolution) frees)
  (with-slots ((decl declaration) (mi module-instance) type) res
    (free-params-res decl mi type frees)))

(defmethod free-params-res ((decl formal-decl) mi type frees)
  (declare (ignore mi type))
  (if (memq decl frees)
      frees
      (cons decl frees)))

(defmethod free-params-res ((decl field-decl) mi type frees)
  (declare (ignore mi))
  (free-params* type frees))

(defmethod free-params-res (decl (mi modname) type frees)
  (declare (ignore decl type))
  (with-slots (actuals) mi
    (if actuals
	(let ((afrees (free-params* actuals nil)))
	  (union afrees frees :test #'eq))
	(let ((theory (get-theory mi)))
	  (when theory
	    (dolist (x (formals-sans-usings theory))
	      (setq frees (pushnew x frees :test #'eq))))
	  frees))))

(defmethod free-params* ((act actual) frees)
  (with-slots (expr type-value) act
    (if type-value
	(free-params* type-value frees)
	(free-params* expr frees))))

(defmethod free-params* ((s symbol) frees)
  frees)

(defun external-free-params (obj)
  (let ((formals (formals-sans-usings (current-theory))))
    (remove-if #'(lambda (fp) (memq fp formals))
      (free-params obj))))
