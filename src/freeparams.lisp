;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; freeparams.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Jun  4 01:33:50 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jun  9 21:24:13 1994
;; Update Count    : 6
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

(export '(free-params free-params*)) ; free-params

;;; Collect the free parameters of an object.
;;; Returns a list of formal decls.

(defun free-params (obj)
  (let ((frees (free-params* obj nil)))
    ;;(assert (null (set-exclusive-or frees (free-params* obj nil))))
    frees))

(defun no-free-params? (obj)
  (null (free-params obj)))

(defmethod formal-not-in-context? ((act actual))
  (formal-not-in-context? (actual-value act)))

(defmethod formal-not-in-context? ((ex name))
  (when (resolution ex)
    (formal-not-in-context? (declaration ex))))

(defmethod formal-not-in-context? ((decl formal-decl))
  (assert (current-theory))
  (not (memq decl (formals-sans-usings (current-theory)))))

(defmethod formal-not-in-context? ((decl decl-formal))
  (assert (current-declaration))
  (not (memq decl (decl-formals (current-declaration)))))

(defmethod formal-not-in-context? ((decl declaration))
  nil)

(defmethod formal-not-in-context? ((ex expr))
  nil)

(defmethod formal-not-in-context? ((te type-expr))
  nil)

(defun formals-not-in-context (obj)
  (remove-if-not #'formal-not-in-context? (free-params obj)))

(defun fully-instantiated? (obj)
  (not (some #'formal-not-in-context? (free-params obj))))

;; (defun old-fully-instantiated? (obj)
;;   (let ((frees (free-params obj)))
;;     (declare (type list frees))
;;     (or (null frees)
;; 	(let ((tformals (formals-sans-usings (current-theory)))
;; 	      (dformals (or *decl-bound-parameters*
;; 			    (when (current-declaration)
;; 			      (decl-formals (current-declaration))))))
;; 	  (declare (type list tformals dformals))
;; 	  (every #'(lambda (fp) (memq fp (append dformals tformals)))
;; 		 frees)))))

;;; Theory

(defmethod free-params* ((theory datatype-or-module) frees)
  (union (formals-sans-usings theory) frees :test #'eq))

(defmethod free-params* ((c simple-constructor) frees)
  (let ((efrees (free-params* (recognizer c)
		  (free-params* (arguments c) nil))))
    (union efrees frees :test #'eq)))

(defmethod free-params* ((decl declaration) frees)
  (free-params* (module decl) frees))

(defmethod free-params* ((importing importing) frees)
  (free-params* (theory-name importing) frees))

(defmethod free-params* ((exporting exporting) frees)
  frees)

(defmethod free-params* ((jdecl subtype-judgement) frees)
  (with-slots (free-parameters) jdecl
    (when (eq free-parameters 'unbound)
      (setf free-parameters
	    (free-params* (subtype jdecl)
	      (free-params* (declared-subtype jdecl)
		(free-params* (declared-type jdecl)
		  (free-params* (type jdecl) nil))))))
    (union free-parameters frees :test #'eq)))

(defmethod free-params* ((jdecl number-judgement) frees)
  (with-slots (free-parameters) jdecl
    (when (eq free-parameters 'unbound)
      (setf free-parameters
	    (free-params* (type jdecl)
	      (free-params* (declared-type jdecl) nil))))
    (union free-parameters frees :test #'eq)))

(defmethod free-params* ((jdecl name-judgement) frees)
  (with-slots (free-parameters) jdecl
    (when (eq free-parameters 'unbound)
      (setf free-parameters
	    (free-params* (name jdecl)
	      (free-params* (type jdecl)
		(free-params* (declared-type jdecl) nil)))))
    (union free-parameters frees :test #'eq)))

(defmethod free-params* ((jdecl application-judgement) frees)
  (with-slots (free-parameters) jdecl
    (when (eq free-parameters 'unbound)
      (setf free-parameters
	    (free-params* (name jdecl)
	      (free-params* (formals jdecl) 
		(free-params* (type jdecl)
		  (free-params* (declared-type jdecl)
		    (free-params* (judgement-type jdecl) nil)))))))
    (union free-parameters frees :test #'eq)))

(defmethod free-params* ((cdecl conversion-decl) frees)
  (with-slots (free-parameters) cdecl
    (when (eq free-parameters 'unbound)
      (setf free-parameters
	    (free-params* (expr cdecl) nil)))
    (union free-parameters frees :test #'eq)))

;;; Type expressions

(defmethod free-params* :around ((texpr type-expr) frees) 
  (with-slots (free-parameters) texpr
    (when (eq free-parameters 'unbound)
      (let ((pfrees (free-params* (print-type texpr) nil)))
	(call-next-method)
	(dolist (pf pfrees)
	  (pushnew pf free-parameters :test #'eq))))
    (union free-parameters frees :test #'eq)))

(defmethod free-parameters ((texpr dep-binding))
  (free-parameters (type texpr)))
    
(defmethod free-params* ((texpr type-application) frees)
  (let ((nfrees (free-params* (type texpr)
		  (free-params* (parameters texpr) nil))))
    (setf (free-parameters texpr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((texpr type-name) frees)
  (assert (resolution texpr))
  (let ((nfrees (call-next-method texpr nil)))
    #+pvs-debug (assert (subsetp (free-params (resolution texpr)) nfrees :test #'eq))
    (setf (free-parameters texpr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((texpr print-type-name) frees)
  (assert (resolution texpr))
  (assert (or (null (actuals texpr))
	      (eq (actuals texpr) (actuals (module-instance texpr)))))
  (let ((nfrees (free-params* (module-instance texpr) nil)))
    (setf (free-parameters texpr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((texpr setsubtype) frees)
  (let ((tfrees (free-params* (formals texpr)
		  (free-params* (formula texpr)
		    (free-params* (supertype texpr) 
		      (free-params* (predicate texpr) nil))))))
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))

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

(defmethod free-params* ((conv conversion-result) frees)
  (free-params* (expr conv) frees))

(defmethod free-params* ((texpr struct-sub-recordtype) frees)
  (let ((ffrees (free-params* (fields texpr) nil))) 
    (setf (free-parameters texpr) ffrees)
    (union ffrees frees :test #'eq)))

(defmethod free-params* ((texpr struct-sub-tupletype) frees)
  (let ((tfrees (free-params* (types texpr) nil))) 
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))

(defmethod free-params* ((texpr type-extension) frees)
  (let ((tfrees (free-params* (type texpr)
		  (free-params* (extension texpr) nil))))
    (setf (free-parameters texpr) tfrees)
    (union tfrees frees :test #'eq)))
    
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

(defmethod free-params* ((expr list-expr) frees)
  (let ((list-ex expr)
	(ofrees (free-params* (operator expr) nil))
	(cons-list nil))
    (loop while (list-expr? list-ex)
       do (let ((afrs (free-params* (args1 list-ex) nil)))
	    (assert (equal (free-parameters (args1 list-ex)) afrs))
	    (push list-ex cons-list)
	    (setq list-ex (args2 list-ex))))
    (assert (or (null-expr? list-ex)
		(and (constructor-name-expr? list-ex)
		     (eq (id list-ex) '|null|))))
    (setf (free-parameters list-ex) ofrees)
    ;; Now walk down the reversed list-exprs
    (dolist (cons-ex cons-list)
      (let ((a1frees (free-parameters (args1 cons-ex)))
	    (a2frees (free-parameters (args2 cons-ex))))
	(assert (listp a1frees))
	(assert (listp a2frees))
	(setf (free-parameters cons-ex)
	      (union a1frees a2frees :test #'eq))))
    (assert (listp (free-parameters expr)))
    (union (free-parameters expr) frees :test #'eq)))

(defmethod free-params* ((list list) frees)
  (free-params-list list frees))

(defun free-params-list (list frees)
  (if (null list)
      frees
      (let ((car-frees (free-params* (car list) frees)))
	(free-params-list (cdr list) car-frees))))

(defmethod free-params* ((expr bind-decl) frees)
  (let ((tfrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) tfrees)
    (union frees tfrees :test #'eq)))

(defmethod free-params* ((expr binding-expr) frees)
  (let* ((efrees (free-params* (expression expr) 
		   (free-params* (bindings expr)
		     (free-params* (type expr) nil)))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr lambda-expr-with-type) frees)
  (let* ((efrees (free-params* (expression expr) 
		   (free-params* (bindings expr)
		     (free-params* (type expr)
		       (free-params* (return-type expr) nil))))))
    (setf (free-parameters expr) efrees)
    (union efrees frees :test #'eq)))

(defmethod free-params* ((expr name-expr) frees)
  (let ((nfrees (free-params* (type expr)
		  (when (constant? expr)
		    (call-next-method expr nil)))))
    (setf (free-parameters expr) nfrees)
    (union nfrees frees :test #'eq)))

(defmethod free-params* ((expr rational-expr) frees)
  (setf (free-parameters expr) nil)
  frees)

(defmethod free-params* ((expr fieldex) frees)
  (let ((afrees (free-params* (type expr) nil)))
    (setf (free-parameters expr) afrees)
    (union afrees frees :test #'eq)))

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
  (with-slots (actuals dactuals mappings resolutions) mi
    (cond ((or actuals dactuals)
	   ;; In this case, the theory is at least partially instantiated,
	   ;; but there may be freevars in the actuals or mappings
	   (let ((afrees (free-params-acts actuals mi))
		 (dfrees (free-params-dacts dactuals))
		 (mfrees (free-params* mappings nil)))
	     (union mfrees (union dfrees (union afrees frees :test #'eq)
				  :test #'eq)
		    :test #'eq)))
	  (resolutions
	   (assert (null (cdr resolutions)))
	   ;; This case is for theory references, i.e., theory declarations or
	   ;; abbreviations
	   (free-params* (car resolutions) frees))
	  (t (let ((theory (get-theory mi))
		   (mfrees (free-params* mappings nil)))
	       (assert theory)
	       (union mfrees (formals-sans-usings theory) :test #'eq))))))

(defmethod free-params* ((map mapping) frees)
  (let* ((rmfrees (free-params* (rhs map) nil))
	 (mfrees (remove-if #'(lambda (mf)
				(memq mf (decl-formals (lhs map))))
		   rmfrees)))
    (union mfrees frees :test #'eq)))

(defmethod free-params* ((map mapping-rhs) frees)
  (with-slots (expr type-value) map
    (if type-value
	(free-params* type-value frees)
	(free-params* expr frees))))

(defmethod free-params* ((name name) frees)
  (with-slots (actuals dactuals print-type resolutions) name
    (let ((afrees (free-params* actuals nil))
	  (dafrees (free-params* dactuals nil))
	  (rfrees (free-params* (car resolutions) frees)))
      (union dafrees (union afrees (union rfrees frees
					  :test #'eq)
			    :test #'eq)
	     :test #'eq))))

(defmethod free-params* ((res resolution) frees)
  (with-slots ((decl declaration) (mi module-instance) type) res
    (free-params-res decl mi type frees)))

(defmethod free-params-res ((decl formal-decl) mi type frees)
  (declare (ignore mi type))
  (if (memq decl frees)
      frees
      (cons decl frees)))

(defmethod free-params-res ((decl binding) mi type frees)
  (declare (ignore mi))
  (free-params* type frees))

(defmethod free-params-res ((decl type-decl) (mi modname) type frees)
  (with-slots (actuals dactuals) mi
    (declare (ignore type)) ;; Can't go down type - recurses
    (if (or actuals dactuals)
	(let* ((afrees (free-params-acts actuals mi))
	       (dafrees (free-params-dacts dactuals))
	       ;; Can't go down type - recurses
	       (ufrees (union dafrees (union afrees frees :test #'eq) :test #'eq)))
	  ;;(assert (every #'(lambda (fp) (memq fp ufrees)) (free-params* type nil)))
	  ufrees)
	(let ((theory (if (typep decl '(and recursive-type
					    (not inline-recursive-type)))
			  decl
			  (module decl))))
	  (when theory
	    (dolist (x (formals-sans-usings theory))
	      (setq frees (pushnew x frees :test #'eq))))
	  (dolist (x (decl-formals decl))
	    (setq frees (pushnew x frees :test #'eq)))
	  frees))))

(defmethod free-params-res (decl (mi modname) type frees)
  ;; Should mappings be here?
  (with-slots (actuals dactuals) mi
    (let* ((afrees (if actuals
		       (free-params-acts actuals mi)
		       (free-params-from-theory decl)))
	   (dafrees (if dactuals
			(free-params-dacts dactuals)
			(free-params-from-decl decl)))
	   (tfrees (free-params type))
	   (ufrees (union dafrees
			  (union afrees
				 (union tfrees frees :test #'eq)
				 :test #'eq)
			  :test #'eq)))
      ;;(assert (every #'(lambda (fp) (memq fp ufrees)) (free-params* type nil)))
      ufrees)))

(defun free-params-from-theory (decl)
  (let ((theory (if (typep decl '(and recursive-type
				  (not inline-recursive-type)))
		    decl
		    (module decl))))
    (formals-sans-usings theory)))

(defun free-params-from-decl (decl)
  (decl-formals decl))

(defun free-params-acts (actuals mi)
  (if actuals
      (free-params* actuals nil)
      (let ((theory (get-theory mi)))
	(assert theory ()
		"free-params: get-theory failed for ~a" mi)
	(formals-sans-usings theory))))

(defun free-params-dacts (dactuals)
  (when dactuals
    (free-params* dactuals nil)))

(defmethod free-params-res ((theory module) (mi modname) type frees)
  (declare (ignore type))
  (with-slots (actuals dactuals) mi
    (if (or actuals dactuals)
	(let ((afrees (free-params* actuals nil))
	      (dfrees (free-params* dactuals nil)))
	  (union dfrees (union afrees frees :test #'eq) :test #'eq))
	(progn (dolist (x (formals-sans-usings theory))
		 (setq frees (pushnew x frees :test #'eq)))
	       frees))))

(defmethod free-params-res ((decl mapping) mi type frees)
  (declare (ignore type))
  (if (memq decl (mappings mi))
      frees
      (free-params* (lhs decl) (free-params* (rhs decl) frees))))

(defmethod free-params* ((act actual) frees)
  (free-params* (actual-value act) frees))

(defmethod free-params* ((s symbol) frees)
  frees)

(defun external-free-params (obj)
  (let ((formals (formals-sans-usings (current-theory)))
	(dformals (decl-formals (current-declaration))))
    (remove-if #'(lambda (fp) (or (memq fp formals) (memq fp dformals)))
      (free-params obj))))
