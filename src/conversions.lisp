;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conversions.lisp -- functions for creating and propagating conversions
;; Author          : Sam Owre
;; Created On      : Mon Apr 12 14:54:48 2004
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Apr 12 16:28:00 2004
;; Update Count    : 4
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

(export '(argument-conversions))

;;; resolve
(defun argument-conversion (name arguments)
  (let ((*found-one* nil)
	(reses (remove-if-not #'(lambda (r)
				  (typep (find-supertype (type r)) 'funtype))
		 (resolve name 'expr nil))))
    (declare (special *found-one*))
    (when (let ((*ignored-conversions*
		 (cons "K_conversion" *ignored-conversions*)))
	    (argument-conversions (mapcar #'type reses) arguments))
      (resolve name 'expr arguments))))

(defun argument-k-conversion (name arguments)
  (let ((*found-one* nil)
	(reses (remove-if-not #'(lambda (r)
				  (typep (find-supertype (type r)) 'funtype))
		 (resolve name 'expr nil))))
    (declare (special *found-one*))
    (unless (member "K_conversion" *ignored-conversions* :test #'string=)
      (let ((*only-use-conversions* (list "K_conversion")))
	(when (argument-conversions (mapcar #'type reses) arguments)
	  (resolve name 'expr arguments))))))

(defun argument-conversions (optypes arguments)
  (declare (special *found-one*))
  (if optypes
      (let* ((rtype (find-supertype (car optypes)))
	     (dtypes (when (typep rtype 'funtype)
		       (domain-types rtype)))
	     (dtypes-list (all-possible-instantiations dtypes arguments)))
	(when (length= arguments dtypes)
	  (argument-conversions* arguments dtypes-list))
	(argument-conversions (cdr optypes) arguments))
      *found-one*))

(defun argument-conversions* (arguments dtypes-list &optional aconversions)
  (declare (special *found-one*))
  (if (null dtypes-list)
      aconversions
      (let ((conversions (argument-conversions1 arguments (car dtypes-list)))
	    (found-one nil))
	(when conversions
	  (mapc #'(lambda (arg convs)
		    (when convs
		      (setq found-one t)
		      (setq *found-one* t)
		      (setf (types arg)
			    (remove-duplicates
				(append (types arg)
					(mapcar #'(lambda (c)
						    (get-conversion-range-type
						     c arg))
					  convs))
			      :test #'tc-eq :from-end t))))
		arguments conversions))
	(argument-conversions* arguments (cdr dtypes-list)
			       (if found-one
				   (cons conversions aconversions)
				   aconversions)))))

(defun argument-conversions1 (arguments dtypes &optional result)
  (if (null arguments)
      (unless (every #'null result)
	(nreverse result))
      (let* ((atypes (remove-if #'from-conversion (types (car arguments))))
	     (convs (unless (some #'(lambda (aty)
				      (or (tc-eq aty (car dtypes))
					  (and (compatible? aty (car dtypes))
					       (fully-instantiated?
						(car dtypes)))))
				  atypes)
		      (argument-conversions** atypes (car dtypes)))))
	(argument-conversions1 (cdr arguments) (cdr dtypes)
			       (cons convs result)))))

(defun argument-conversions** (atypes etype &optional result)
  (if (null atypes)
      (nreverse result)
      (let ((conv (find-conversions-for (car atypes) etype)))
	(argument-conversions** (cdr atypes) etype
				(append conv result)))))



;;; This is called by typecheck* (name) when the resolution is nil.  If there
;;; are arguments, it first checks whether there is a conversion available that
;;; would turn the given name into a function of the right type.  If not, then
;;; it looks for a k-conversion so that it can push the conversion down on the
;;; arguments.

(defun function-conversion (name arguments)
  (append (simple-function-conversion name arguments)
	  (resolutions-with-argument-conversions
	   (resolve (copy name 'resolutions nil) 'expr nil) arguments)))

(defun resolutions-with-argument-conversions (resolutions arguments
							  &optional result)
  (if (null resolutions)
      result
      (let ((rtype (find-supertype (type (car resolutions)))))
	(multiple-value-bind (compats k-conv-type)
	    (compatible-arg-conversions? rtype arguments)
	  (resolutions-with-argument-conversions
	   (cdr resolutions)
	   arguments
	   (if (and compats
		    (some #'(lambda (x) (not (eq x 't))) compats))
	       (cons (make-function-conversion-resolution
		      (car resolutions) arguments compats k-conv-type)
		     result)
	       result))))))

(defun make-function-conversion-resolution (res args compats k-conv-type)
  (declare (ignore args))
  (let ((theories (delete *current-theory*
			  (delete-duplicates (mapcar #'module
						     (free-params res)))))
	(dtypes (domain-types (type res)))
	(nres res))
    (dolist (th theories)
      (mapc #'(lambda (dt compat)
		(when (typep compat 'name-expr)
		  (let ((bindings (tc-match
				   (domain (type compat)) dt
				   (mapcar #'(lambda (x)
					       (cons x nil))
				     (formals-sans-usings th)))))
		    (when (and bindings (every #'cdr bindings))
		      (setq nres
			    (subst-mod-params
			     nres
			     (mk-modname (id th)
			       (mapcar #'(lambda (a)
					   (mk-res-actual (cdr a) th))
				 bindings))
			     th))))))
	    dtypes compats))
    (change-class nres 'lambda-conversion-resolution)
    (setf (k-conv-type nres) k-conv-type)
    (let* ((ktype (type (find-if #'name-expr? compats)))
	   (ftype (mk-funtype (domain ktype)
			      (mk-funtype (domain (range ktype))
					  (range (type nres))))))
      (setf (type nres) ftype))
    nres))

(defun compatible-arg-conversions? (rtype arguments)
  (and (typep rtype 'funtype)
       (let* ((dtypes (domain-types rtype))
	      (dtypes-list (all-possible-instantiations dtypes arguments)))
	 (and (length= dtypes arguments)
	      (let* ((compats (mapcan #'(lambda (dty)
					  (compatible-arguments-k-conversions
					   dty arguments))
				dtypes-list))
		     (k-convtype (get-k-conversion-type compats)))
		(when k-convtype
		  (let* ((ndtypes-list
			  (mapcar #'(lambda (dtypes)
				      (mapcar #'(lambda (dty)
						  (if (and (funtype? dty)
							   (tc-eq k-convtype
								  (domain dty)))
						      (range dty)
						      dty))
					dtypes))
			    dtypes-list))
			 (ncompats (mapcan #'(lambda (dty)
					       (compatible-arguments-k-conversions
						dty arguments))
				     ndtypes-list)))
		    (values (or ncompats compats) k-convtype))))))))

(defun get-k-conversion-type (compats &optional type)
  (if (null compats)
      type
      (if (eq (car compats) t)
	  (get-k-conversion-type (cdr compats) type)
	  (let ((k-type (domain (range (type (car compats))))))
	    (when (or (null type)
		      (tc-eq type k-type))
	      (get-k-conversion-type (cdr compats) k-type))))))

(defun compatible-arguments-k-conversions (dtypes arguments
						  &optional kconv result)
  (if (null dtypes)
      (nreverse result)
      (let ((conv (compatible-argument-k-conversions
		   (car dtypes) (car arguments))))
	(when conv
	  (assert (or (eq conv t)
		      (name-expr? conv)))
	  (when (or (null kconv)
		    (eq conv t)
		    ;; Do they have the same introduced type, e.g., state?
		    (tc-eq (cadr (actuals (module-instance kconv)))
			   (cadr (actuals (module-instance conv)))))
	    (compatible-arguments-k-conversions
	     (cdr dtypes) (cdr arguments)
	     (or kconv (and (name-expr? conv) conv))
	     (cons conv result)))))))

(defun compatible-argument-k-conversions (dtype argument)
  (or (some #'(lambda (pty)
		(compatible? dtype pty))
	    (ptypes argument))
      (compatible-argument-k-conversion dtype (ptypes argument))))

(defun compatible-argument-k-conversion (dtype ptypes)
  (when ptypes
    (let ((fty (find-supertype (car ptypes))))
      (or (and (typep fty 'funtype)
	       (compatible? (range fty) dtype)
	       (car (get-k-conversions fty)))	  
	  (compatible-argument-k-conversion dtype (cdr ptypes))))))

(defun k-conversion-resolutions (res kcs arguments &optional result)
  (if (null res)
      result
      (let ((nres (k-conversion-resolutions1 (car res) kcs arguments)))
	(k-conversion-resolutions (cdr res) kcs arguments
				(if nres (cons nres result) result)))))

(defun k-conversion-resolutions1 (res kcs arguments)
  (let ((rtype (find-supertype (type res))))
    (when (and (typep rtype 'funtype)
	       (length= (domain-types rtype) arguments))
      (let ((kc (find-if #'(lambda (kc)
			     (compatible-k-args kc (domain-types rtype) arguments))
		  kcs)))
	(when kc
	  (let ((sres (instantiate-resolution-from-k
		       res (domain (find-supertype (type kc))))))
	    (change-class sres 'conversion-resolution)
	  (setf (conversion sres) kc)
	  sres))))))

(defun instantiate-resolution-from-k (res domain)
  (if (fully-instantiated? (type res))
      res
      (let* ((theory (get-theory (module-instance res)))
	     (bindings (tc-match domain (domain (find-supertype (type res)))
				(mapcar #'(lambda (x) (cons x nil))
					(formals-sans-usings theory)))))
	(if (every #'cdr bindings)
	    (subst-mod-params res
			      (car (create-compatible-modinsts
				    (module-instance res)
				    (declaration res)
				    (list bindings)
				    nil))
			      theory)
	    res))))

(defun compatible-k-args (kc dtypes args)
  (let ((lt (car (domain-types (type kc))))
	(rt (range (type kc))))
    (compatible-k-args* lt rt dtypes args)))

(defun compatible-k-args* (lt rt dtypes args)
  (or (null dtypes)
      (and (some #'(lambda (pty)
		     (or (compatible? pty (car dtypes))
			 (and (compatible? pty rt)
			      (compatible? lt (car dtypes)))))
		 (ptypes (car args)))
	   (compatible-k-args* lt rt (cdr dtypes) (cdr args)))))

(defun matching-k-conversions (args &optional result)
  (if (null args)
      (remove-duplicates result :test #'tc-eq)
      (matching-k-conversions
       (cdr args)
       (append (matching-k-conversions1 (ptypes (car args)))
	       result))))

(defun matching-k-conversions1 (atypes &optional result)
  (if (null atypes)
      result
      (matching-k-conversions1
       (cdr atypes)
       (append (get-k-conversions (car atypes)) result))))

(defun simple-function-conversion (name arguments)
  (let ((reses (resolve name 'expr nil))
	(creses nil))
    (mapc #'(lambda (r)
	      (dolist (conv (get-resolution-conversions r arguments))
		(push (make-conversion-resolution r conv name)
		      creses)))
	  reses)
    (append (get-recordtype-conversion-resolutions name arguments)
	    (nreverse creses))))

(defun get-resolution-conversions (res arguments)
  (get-resolution-conversions*
   (remove-if #'k-combinator? (current-conversions))
   res arguments nil))

(defun get-resolution-conversions* (conversions res arguments result)
  (if (null conversions)
      result
      (let ((conv (compatible-resolution-conversion (car conversions)
						    res arguments)))
	(get-resolution-conversions* (cdr conversions) res arguments
				     (if conv
					 (cons conv result)
					 result)))))

(defun compatible-resolution-conversion (conversion res arguments)
  (unless (memq conversion (disabled-conversions *current-context*))
    (let ((nconv (compatible-operator-conversion
		  conversion (type res) arguments)))
      (when (and nconv
		 (not (member (expr nconv)
			      (disabled-conversions *current-context*)
			      :key #'expr :test #'tc-eq)))
	(make-instance 'conversion-result
	  :expr (copy (expr nconv))
	  :conversion nconv)))))

(defun compatible-resolution-conversion-args (bindings ctype arguments)
  (let* ((cran (find-supertype (range ctype)))
	 (dtypes (when (typep cran 'funtype)
		   (domain-types cran)))
	 (bindings-list (find-compatible-bindings arguments dtypes bindings)))
    (car bindings-list)))

(defun compatible-resolution-conversion? (ctype res arguments)
  (and (compatible? (domain ctype) (type res))
       (or (null arguments)
	   (let ((cran (find-supertype (range ctype))))
	     (and (typep cran 'funtype)
		  (let* ((dtypes (domain-types cran))
			 (args (if (and (singleton? dtypes)
					(not (singleton? arguments)))
				   (list (typecheck* (mk-arg-tuple-expr*
						      arguments)
						     nil nil nil))
				   arguments)))
		    (and (length= dtypes args)
			 (every #'(lambda (a dty)
				    (some #'(lambda (pty)
					      (compatible? pty dty))
					  (ptypes a)))
				args dtypes))))))))

(defun make-conversion-resolution (res conv name)
  (let* ((ctype (find-supertype (type conv)))
	 (dep? (typep (domain ctype) 'dep-binding))
	 (nname (when dep?
		  (copy name
		    'type (type res)
		    'resolutions (list res))))
	 (rtype (if dep?
		    (substit (range ctype)
		      (acons (domain ctype) nname nil))
		    (range ctype))))
    (assert (typep conv '(or conversion-result funtype-conversion
			     rectype-conversion tuptype-conversion)))
    (when (name-expr? (expr conv))
      (change-name-expr-class-if-needed (declaration (expr conv))
					(expr conv)))
    (make-instance 'conversion-resolution
      :module-instance (module-instance res)
      :declaration (declaration res)
      :type (copy rtype 'from-conversion conv)
      :conversion conv)))

(defun get-conversion-range-type (conv expr)
  (let* ((ctype (find-supertype (type conv)))
	 (dep? (typep (domain ctype) 'dep-binding))
	 (nexpr (when dep?
		  (typecheck* (copy-untyped expr) (domain ctype) nil nil)))
	 (rtype (if dep?
		    (substit (range ctype)
		      (acons (domain ctype) nexpr nil))
		    (range ctype))))
    (assert (typep conv '(or conversion-result funtype-conversion
			     rectype-conversion tuptype-conversion)))
    (when (name-expr? (expr conv))
      (change-name-expr-class-if-needed (declaration (expr conv))
					(expr conv)))
    (copy rtype 'from-conversion conv)))

;;; tcexprs

(defun find-application-conversion (expr)
  (let* ((op (operator expr))
	 (arg (argument expr))
	 (args (arguments expr))
	 (*found-one* nil))
    (declare (special *found-one*))
    (if (or (argument-conversions (types op) args)
	    (argument-conversions (types op) (list arg)))
	(set-possible-argument-types op (argument expr))
	(let ((conversions (unless *no-conversions-allowed*
			     (find-operator-conversions (types op) args))))
	  (if conversions
	      (let* ((conv (car conversions))
		     (ctype (type (expr conv)))
		     (dom (domain (find-supertype ctype)))
		     ;;(ran (range (find-supertype ctype)))
		     (nop (copy op)))
		(add-conversion-info (expr conv) op)
		(change-class op 'implicit-conversion)
		(setf (argument op) nop)
		(setf (types nop)
		      (list (if (typep dom 'dep-binding) (type dom) dom)))
		(setf (operator op) (copy (expr conv)))
		(setf (types op) (list ctype))
		(typecheck* op nil nil nil))
	      (type-mismatch-error expr))))))

(defun find-operator-conversions (optypes args &optional conversions)
  (if (null optypes)
      conversions
      (find-operator-conversions
       (cdr optypes) args
       (append conversions
	       (find-operator-conversion* (car optypes) args)))))

(defun find-operator-conversion* (optype args)
  (let ((conversions nil))
    (dolist (conv (current-conversions))
      (let ((nconv (compatible-operator-conversion conv optype args)))
	(when (and nconv
		   (not (member nconv (disabled-conversions *current-context*)
				:key #'expr :test #'tc-eq)))
	  (push nconv conversions))))
    (nreverse conversions)))

(defun compatible-operator-conversion (conversion optype args)
  (let* ((theory (module conversion))
	 (ctype (find-supertype (type (expr conversion))))
	 (fmls (formals-sans-usings theory)))
    (and (typep ctype 'funtype)
	 (typep (find-supertype (range ctype)) 'funtype)
	 (if (and (remove-if #'(lambda (fp)
				 (memq fp (formals-sans-usings
					   (current-theory))))
		    (free-params conversion))
		  fmls
		  (not (eq theory *current-theory*)))
	     (let* ((bindings1 (tc-match optype (domain ctype)
					 (mapcar #'list fmls)))
		    (dtypes (domain-types (find-supertype (range ctype))))
		    (bindings (when (and bindings1
					 (= (length args) (length dtypes)))
				(car (find-compatible-bindings args dtypes
							       bindings1)))))
	       (when (and bindings (every #'cdr bindings))
		 (let* ((acts (mapcar #'(lambda (a)
					  (mk-res-actual (cdr a) theory))
				      bindings))
			(nmi (mk-modname (id theory) acts)))
		   (and (with-no-type-errors
			 (check-compatible-params
			  (formals-sans-usings theory) acts nil))
			(check-operator-conversion
			 (subst-params-decl conversion nmi theory) args)))))
	     (when (compatible? optype (domain ctype))
	       (check-operator-conversion conversion args))))))

(defun check-operator-conversion (conversion args)
  (let* ((ftype (find-supertype (type (expr conversion))))
	 (rtype (find-supertype (range ftype))))
    (when (and ;;(valid-actuals (expr conversion))
	       (typep rtype 'funtype)
	       (length= args (domain-types rtype))
	       (every #'(lambda (a d)
			  (some #'(lambda (aty) (compatible? aty d))
				(ptypes a)))
		      args (domain-types rtype)))
      conversion)))

(defvar *in-application-conversion* nil)

(defun change-application-to-conversion (expr)
  #+pvsdebug (assert (and (typep (operator expr) 'name-expr)
			  (some #'(lambda (r)
				    (typep r 'lambda-conversion-resolution))
				(resolutions (operator expr)))))
  (unless (or *no-conversions-allowed*
	      *in-application-conversion*)
    (let* ((*in-application-conversion* t)
	   (op (operator expr))
	   (kres (find-if #'(lambda (r)
			      (typep r 'lambda-conversion-resolution))
		   (resolutions (operator expr))))
	   (kid (make-new-variable '|s| expr))
	   (kbd (make-bind-decl kid (k-conv-type kres)))
	   (*bound-variables* (cons kbd *bound-variables*))
	   (kvar (make-variable-expr kbd))
	   (args (application-conversion-arguments (arguments expr) kvar))
	   (orig-expr (copy expr)))
      (change-class expr 'lambda-conversion)
      (setf (bindings expr) (list kbd)
	    (types op) nil
	    (resolutions op) nil
	    (expression expr) (make-instance (class-of orig-expr)
				:operator op
				:argument (if (cdr args)
					      (make-instance 'arg-tuple-expr
						:exprs args)
					      (car args))))
      (add-conversion-info "LAMBDA conversion" orig-expr expr)
      (typecheck* expr nil nil nil))))

(defun application-conversion-arguments (arguments kvar)
  (let* ((*parsing-or-unparsing* t)
	 (*gensubst-reset-types* t)
	 (nargs (gensubst arguments
		  #'(lambda (ex)
		      (setf (types ex) nil)
		      (when (name-expr? ex) (setf (resolutions ex) nil))
		      (let ((ac (make-instance 'argument-conversion
				  :operator ex
				  :argument kvar)))
			(typecheck* ac nil nil nil)))
		  #'(lambda (ex)
		      (and (expr? ex)
			   (some #'(lambda (ty)
				     (and (type-expr? ty)
					  (not (from-conversion ty))
					  (let ((sty (find-supertype ty)))
					    (and (funtype? sty)
						 (compatible? (domain sty)
							      (type kvar))))))
				 (ptypes ex)))))))
    (typecheck* nargs nil nil nil)))
      

(defmethod application-conversion-argument (arg (conv name-expr) vars)
  (let ((var1 (find-if #'(lambda (v)
			   (tc-eq (type v) (domain (range (type conv)))))
		vars)))
    (if (some #'(lambda (ty)
		  (and (funtype? (find-supertype ty))
		       (compatible? (domain (find-supertype ty)) (type var1))))
	      (ptypes arg))
	(let ((ac (make-instance 'argument-conversion
		    :operator arg
		    :argument var1)))
	  (typecheck* ac nil nil nil))
	arg)))

;;; If the argument is a function whose domain matches one of the
;;; variables, create the application.  This fixes a problem with,
;;; e.g., IF a THEN b ELSE c ENDIF, where a: [T -> bool], b, c: [T -> int]
;;; being translated to
;;;  LAMBDA (x:T) IF a(x) THEN b ELSE c ENDIF : [T -> [T -> int]]
;;; rather than
;;;  LAMBDA (x:T): IF a(x) THEN b(x) ELSE c(x) ENDIF : [T -> int]
(defmethod application-conversion-argument (arg conv vars)
  (declare (ignore conv))
  (let ((var1 (find-if #'(lambda (v)
			   (every #'(lambda (ty)
				      (let ((sty (find-supertype ty)))
					(and (funtype? sty)
					     (tc-eq (type v) (domain sty)))))
				  (ptypes arg)))
		vars)))
    (if var1
	(let ((ac (make-instance 'argument-conversion
		    :operator arg
		    :argument var1)))
	  (typecheck* ac nil nil nil))
	arg))
  ;;arg
  )

;;; set-type
(defun look-for-conversion (expr expected)
  (let ((ctypes (argument-conversions** (types expr) expected)))
    (when ctypes
      (change-to-conversion expr expected
			    (get-conversion-range-type (car ctypes) expr)))))


(defun change-to-conversion (expr expected &optional convtype)
  (let* ((ctype1 (or convtype
		     (find-best-type-conversion (types expr) expected)))
	 (ctype (if (fully-instantiated? ctype1)
		    ctype1
		    (instantiate-from ctype1 expected expr))))
    (assert ctype)
    (cond
     ((and (typep expr 'application)
	   (typep (operator expr) 'name-expr)
	   (some #'(lambda (r)
		     (and (typep r 'lambda-conversion-resolution)
			  (compatible? (k-conv-type r) ctype)))
		 (resolutions (operator expr))))
      (change-application-to-conversion expr)
      (set-type expr expected)
      expr)
     ((and (typep expr 'application)
	   (typep (operator expr) 'name-expr)
	   (k-combinator? (from-conversion ctype)))
      (change-to-lambda-conversion expr ctype)
      (set-type expr expected)
      expr)
     (t
      #+pvsdebug (assert (typep (expr (from-conversion ctype)) 'expr))
      (add-conversion-info (from-conversion ctype) expr)
      (make-implicit-conversion (expr (from-conversion ctype)) ctype expr)
      ;; expr now has been modified to have the conversion applied
      (typecheck* expr expected nil nil)))))

;;; make-implicit-conversion converts the ex according to the following table:
;;;  field-name-expr --> field-conversion (fieldappl)
;;;  projection-expr --> projection-conversion (projappl)
;;;  injection-expr  --> injection-conversion (injection-application)
;;;  extraction-expr --> extraction-conversion (extraction-application)
;;; injection?-expr not included - hard to see how it is a useful conversion

(defmethod make-implicit-conversion ((conv fieldex) ctype ex)
  (let ((nexpr (copy ex))
	(dom (domain (type (from-conversion ctype)))))
    (change-class ex 'field-conversion
      'id (id conv)
      'actuals (actuals conv)
      'argument nexpr)
    (setf (types nexpr) (list (if (typep dom 'dep-binding) (type dom) dom)))
    (setf (types ex) (list ctype))
    (when (typep (argument ex) 'name-expr)
      (setf (resolutions (argument ex)) nil
	    (types (argument ex)) nil))))

(defmethod make-implicit-conversion ((conv projection-expr) ctype ex)
  (let ((nexpr (copy ex))
	(dom (domain (type (from-conversion ctype)))))
    (change-class ex 'projection-conversion
      'id (id conv)
      'index (index conv)
      'actuals (actuals conv)
      'argument nexpr)
    (setf (types nexpr) (list (if (typep dom 'dep-binding) (type dom) dom)))
    (setf (types ex) (list ctype))
    (when (typep (argument ex) 'name-expr)
      (setf (resolutions (argument ex)) nil
	    (types (argument ex)) nil))))

(defmethod make-implicit-conversion ((conv injection-expr) ctype ex)
  (let ((nexpr (copy ex))
	(dom (domain (type (from-conversion ctype)))))
    (change-class ex 'injection-conversion
      'id (id conv)
      'index (index conv)
      'actuals (actuals conv)
      'argument nexpr)
    (setf (types nexpr) (list (if (typep dom 'dep-binding) (type dom) dom)))
    (setf (types ex) (list ctype))
    (when (typep (argument ex) 'name-expr)
      (setf (resolutions (argument ex)) nil
	    (types (argument ex)) nil))))

(defmethod make-implicit-conversion ((conv extraction-expr) ctype ex)
  (let ((nexpr (copy ex))
	(dom (domain (type (from-conversion ctype)))))
    (change-class ex 'extraction-conversion
		  'id (id conv)
		  'index (index conv)
		  'actuals (actuals conv)
		  'argument nexpr)
    (setf (types nexpr) (list (if (typep dom 'dep-binding) (type dom) dom)))
    (setf (types ex) (list ctype))
    (when (typep (argument ex) 'name-expr)
      (setf (resolutions (argument ex)) nil
	    (types (argument ex)) nil))))

(defmethod make-implicit-conversion ((conv expr) ctype ex)
  (let ((nexpr (copy-untyped ex)))
    (change-class ex 'implicit-conversion)
    (setf (argument ex) nexpr)
    (setf (types nexpr) nil)
    (setf (operator ex)
	  (raise-actuals (copy (expr (from-conversion ctype))) 1))
    (typecheck* ex nil nil nil)))

(defun find-best-type-conversion (types expected &optional best type)
  (cond ((null types)
	 (assert type)
	 type)
	(t (let* ((conv (from-conversion (car types)))
		  (better?
		   (and conv
			(compatible? (car types) expected)
			(or (null best)
			    (memq (conversion best)
				  (memq (conversion conv)
					(current-conversions)))))))
	     (find-best-type-conversion
	      (cdr types)
	      expected
	      (if better? conv best)
	      (if better? (car types) type))))))


(defun change-to-lambda-conversion (expr ctype)
  (unless (or *no-conversions-allowed*
	      *in-application-conversion*)
    (let* ((*in-application-conversion* t)
	   (op (operator expr))
	   (id (make-new-variable '|x| expr))
	   (bd (make-bind-decl id (domain ctype)))
	   (*bound-variables* (cons bd *bound-variables*))
	   (varex (make-variable-expr bd))
	   (args (application-conversion-arguments (arguments expr) varex))
	   (orig-expr (copy expr)))
      (change-class expr 'lambda-conversion)
      (setf (bindings expr) (list bd))
      (setf (types op) nil)
      (when (name-expr? op)
	(setf (resolutions op) nil))
      (setf (expression expr) (make-instance (class-of orig-expr)
				:operator op
				:argument (if (cdr args)
					      (make-instance 'arg-tuple-expr
						:exprs args)
					      (car args))))
      (add-conversion-info "LAMBDA conversion" orig-expr expr) 
      (typecheck* expr nil nil nil))))

(defun add-conversion-info (conversion expr &optional new-expr)
  (reset-subst-mod-params-cache)
  (when (type expr)
    (remhash expr (judgement-types-hash (current-judgements))))
  (let ((origin
	 (if (or *in-checker* *in-evaluator*)
	     (if *in-typechecker*
		 (format nil "input ~s"
		   (if (stringp *in-typechecker*)
		       *in-typechecker*
		       (unparse *in-typechecker* :string t)))
		 "input")
	     (format nil "declaration ~a"
	       (if (typep (current-declaration) 'declaration)
		   (id (current-declaration))
		   (unparse (current-declaration) :string t))))))
    (if (stringp conversion)
	(pvs-conversion-msg "In ~a:~
                 ~%  added ~a, converting~
                 ~%    ~a~%  to~%    ~a"
	  origin
	  conversion
	  expr new-expr)
	(let ((conv-expr (if (typep conversion
				    '(or conversion-decl conversion-result))
			     (expr conversion)
			     conversion)))
	  (pvs-conversion-msg "In ~a:~
             ~%  added conversion ~a~
             ~@[~%  (involving the ~{~a recursive signature ~a~})~]
             ~%  to ~a, converting~
             ~%     ~a~%  to ~a"
	    origin
	    (raise-actuals conv-expr 1)
	    (when (and *recursive-subtype-term*
		       (occurs-in-eq *recursive-subtype-term*
				     (type conversion)))
	      (list (id (current-declaration))
		    (recursive-signature (current-declaration))))
	    expr
	    (if (recursive-defn-conversion? new-expr)
		(type expr)
		(domain (find-supertype (type conv-expr))))
	    (if (recursive-defn-conversion? new-expr)
		(type new-expr)
		(range (find-supertype (type conv-expr)))))))))

(defun find-funtype-conversion (type expected expr)
  (unless (or *no-conversions-allowed*
	      (typep expr '(or implicit-conversion binding)))
    (let* ((ftype (mk-funtype (list type) expected))
	   (conversions (get-conversions ftype)))
      (when conversions
	(let ((nexpr (copy expr)))
	  (add-conversion-info (car conversions) expr)
	  ;; Warn if more than one possible.
	  (change-class expr 'implicit-conversion)
	  ;;(setf (abstract-syntax expr) nil)
	  (setf (argument expr) nexpr)
	  (setf (operator expr)
		(raise-actuals (copy (expr (car conversions))) 1))
	  (setf (type expr) nil (types expr) nil)
	  (set-type-actuals-and-maps (operator expr))
	  (typecheck* expr expected nil nil)
	  expr)))))

;(defun find-funtype-conversion (type expected expr)
;  (unless (typep expr 'implicit-conversion)
;    (let* ((ftype (mk-funtype (list type) expected))
;	   (conversions (get-conversions ftype)))
;      (if conversions
;	  (let ((nexpr (copy expr)))
;	    (pvs-warning "Added conversion ~a to ~a, converting ~%     ~a~% to  ~a"
;			 (car conversions) expr type expected)
;	    ;; Warn if more than one possible.
;	    (change-class expr 'implicit-conversion)
;	    ;;(setf (abstract-syntax expr) nil)
;	    (setf (argument expr) nexpr)
;	
;	    (setf (operator expr) (copy (car conversions)))
;	    (typecheck* expr expected nil nil)
;	    expr)
;	  (let* ((stype (compatible-type (domain type) (domain expected)))
;		 (sftype (mk-funtype (list stype) (range type)))
;		 (ftype1 (mk-funtype (list type) sftype))
;		 (ftype2 (mk-funtype (list sftype) expected))
;		 (conversions1 (get-conversions ftype1))
;		 (conversions2 (get-conversions ftype2)))
;	    (when (and conversions1 conversions2)
;	      (let ((nexpr (copy expr)))
;		(change-class expr 'implicit-conversion)
;		)))))))



;;; Given the arguments of an application, and the possible operator types
;;; compute the best optypes choice based on a ranking number.
(defun preferred-argument-conversion (args optypes &optional best best-rank)
  (if (null optypes)
      (nreverse best)
      (let* ((atypes (mapcar #'(lambda (a dty)
				 (remove-if-not #'(lambda (aty)
						    (compatible? aty dty))
				   (types a)))
		       args (domain-types (car optypes))))
	     (rank (argtype-conversion-ranking atypes)))
	(preferred-argument-conversion
	 args (cdr optypes)
	 (cond ((or (null best-rank)
		    (< rank best-rank))
		(list (car optypes)))
	       ((= rank best-rank)
		(cons (car optypes) best))
	       (t best))
	 (if best-rank (min rank best-rank) rank)))))

;;; rank the list of argument types - these are the possible types for
;;; the arguments, and have already had the incompatible types
;;; filtered out.
(defun argtype-conversion-ranking (argtypes &optional (rank 0))
  (if (null argtypes)
      rank
      (argtype-conversion-ranking
       (cdr argtypes)
       (max rank (argtype-conversion-ranking*
		  (car argtypes) (1- (length argtypes)))))))

;;; 
(defun argtype-conversion-ranking* (argtypes num)
  (if (every #'from-conversion argtypes)
      (+ (* num .1)
	 (reduce #'min
		 (mapcar #'(lambda (aty)
			     (locality (from-conversion aty)))
		   argtypes)
		 :initial-value 0))
      0))

;;; utils
(defmethod get-conversions ((name name-expr))
  (assert (resolution name))
  (get-conversions (resolution name)))


(defmethod find-conversions-for ((atype recordtype) (etype recordtype))
  (append (call-next-method)
	  (when (and (= (length (fields atype)) (length (fields etype)))
		     (every #'(lambda (afld)
				(some #'(lambda (efld)
					  (eq (id afld) (id efld)))
				      (fields etype)))
			    (fields atype)))
	    ;; Create a conversion of the form
	    ;; LAMBDA (x: atype): (# f1 := conv(x`f1) ... #)
	    (let* ((aid (make-new-variable '|x| (list atype etype)))
		   (abd (make-bind-decl aid atype))
		   (avar (make-variable-expr abd)))
	      (multiple-value-bind (assigns convs)
		  (find-record-assignment-conversions
		   (fields atype) (fields etype) avar
		   (dependent? atype) (dependent? etype))
		(when assigns
		  (list (change-class
			 (make!-lambda-expr (list abd)
			   (make!-record-expr assigns etype))
			 'rectype-conversion
			 'conversions convs))))))))

(defun find-record-assignment-conversions (afields efields avar adep? edep?
						   &optional assigns convs)
  (if (null afields)
      (values (nreverse assigns) (nreverse convs))
      (let* ((afld (car afields))
	     (efld (find (id afld) efields :key #'id)))
	(multiple-value-bind (assign conv aarg earg)
	    (find-record-assignment-conversion afld efld avar adep? edep?)
	  (when assign
	    (find-record-assignment-conversions
	     (if aarg
		 (substit (cdr afields) (acons afld aarg nil))
		 (cdr afields))
	     (if earg
		 (substit efields (acons efld earg nil))
		 efields)
	     avar adep? edep?
	     (cons assign assigns)
	     (cons conv convs)))))))

(defun find-record-assignment-conversion (afld efld avar adep? edep?)
  (let* ((arg (list (list (make-instance 'field-assignment-arg
			    :id (id afld)))))
	 (fappl (make!-field-application efld avar))
	 (conv (unless (tc-eq (type afld) (type efld))
		 (find-most-recent-conversion
		  (find-conversions-for (type afld) (type efld)))))
	 (expr (if (tc-eq (type afld) (type efld))
		   fappl
		   (when conv
		     (make!-application conv fappl)))))
    (when expr
      (values (mk-assignment 'uni arg expr) conv
	      (when adep? fappl) (when edep? expr)))))

(defmethod find-conversions-for ((atype tupletype) (etype tupletype))
  (append (call-next-method)
	  (when (= (length (types atype)) (length (types etype)))
	    ;; Create a conversion of the form
	    ;; LAMBDA (x: atype): (conv(x`1) ... )
	    (let* ((aid (make-new-variable '|x| (list atype etype)))
		   (abd (make-bind-decl aid atype))
		   (avar (make-variable-expr abd)))
	      (multiple-value-bind (args convs)
		  (find-tupletype-conversions
		   (types atype) (types etype) 1 avar)
		(when args
		  (list (change-class
			 (make!-lambda-expr (list abd)
			   (make!-tuple-expr* args))
			 'tuptype-conversion
			 'conversions convs))))))))

(defun find-tupletype-conversions (atypes etypes index avar
					  &optional args convs)
  (if (null atypes)
      (values (nreverse args) (nreverse convs))
      (let* ((atype (car atypes))
	     (etype (car etypes)))
	(multiple-value-bind (arg conv aarg earg)
	    (find-tupletype-conversion atype etype index avar)
	  (when arg
	    (find-tupletype-conversions
	     (if aarg
		 (substit (cdr atypes) (acons (car atypes) aarg nil))
		 (cdr atypes))
	     (if earg
		 (substit (cdr etypes) (acons (car etypes) earg nil))
		 (cdr etypes))
	     (1+ index) avar
	     (cons arg args)
	     (cons conv convs)))))))

(defun find-tupletype-conversion (atype etype index avar)
  (let* ((pappl (make!-projection-application index avar))
	 (conv (unless (tc-eq atype etype)
		 (find-most-recent-conversion
		  (find-conversions-for atype etype))))
	 (expr (if (tc-eq atype etype)
		   pappl
		   (when conv
		     (make!-application (if (conversion-result? conv)
					    (expr conv)
					    conv)
		       pappl)))))
    (when expr
      (values expr conv
	      (when (dep-binding? atype) pappl)
	      (when (dep-binding? etype) expr)))))

(defmethod find-conversions-for ((atype funtype) (etype funtype))
  (let* ((nconvs (call-next-method))
	 (sconvs (remove-if
		     (complement
		      #'(lambda (c)
			  (let ((ctype (find-supertype (type c))))
			    (and (strict-compatible? (domain ctype) atype)
				 (strict-compatible? (range ctype) etype)))))
		   nconvs))
	 (fid (make-new-variable '|f| (list atype etype)))
	 (fbd (make-bind-decl fid atype))
	 (fvar (make-variable-expr fbd))
	 (datype (if (dep-binding? (domain atype))
		     (type (domain atype))
		     (domain atype)))
	 (detype (if (dep-binding? (domain etype))
		     (type (domain etype))
		     (domain etype)))
	 (daid (make-new-variable '|x| (list atype etype)))
	 (dabd (make-bind-decl daid detype))
	 (davar (make-variable-expr dabd))
	 (dconv (unless (tc-eq datype detype)
		  (find-most-recent-conversion
		   ;; Note contravariance
		   (find-conversions-for detype datype))))
	 (arg (if (tc-eq datype detype)
		  davar
		  (when dconv
		    (make!-application dconv davar))))
	 (ratype (if (dep-binding? (domain atype))
		     (when arg
		       (substit (range atype) (acons (domain atype) arg nil)))
		     (range atype)))
	 (retype (if (dep-binding? (domain etype))
		     (substit (range etype) (acons (domain etype) davar nil))
		     (range etype)))
	 (rconv (unless (or (null ratype)
			    (tc-eq ratype retype))
		  (find-most-recent-conversion
		   ;; covariant
		   (find-conversions-for ratype retype))))
	 (fconvs (when arg
		   (let* ((appl (make!-application fvar arg))
			  (expr (if (tc-eq ratype retype)
				    appl
				    (when rconv
				      (make!-application rconv appl)))))
		     (when expr
		       (list (change-class
			      (make!-lambda-expr (list fbd)
				(make!-lambda-expr (list dabd)
				  expr))
			      'funtype-conversion
			      'domain-conversion dconv
			      'range-conversion rconv)))))))
    (or sconvs
	nconvs
	fconvs)))

(defun find-most-recent-conversion (conversions &optional best)
  (assert (every #'(lambda (c)
		     (typep c '(or funtype-conversion rectype-conversion
				   tuptype-conversion conversion-result)))
		 conversions))
  (if (null conversions)
      (if (conversion-result? best)
	  (expr best)
	  best)
      (find-most-recent-conversion
       (cdr conversions)
       (if (or (null best)
	       (memq (conversion best)
		     (memq (conversion (car conversions))
			   (current-conversions))))
	   (car conversions)
	   best))))
  

(defmethod find-conversions-for (atype etype)
  (find-conversions* (current-conversions)
		     (mk-funtype atype etype)))

(defmethod expr ((ex tuptype-conversion))
  ex)

(defmethod expr ((ex rectype-conversion))
  ex)

(defmethod expr ((ex funtype-conversion))
  ex)

(defvar *only-use-conversions* nil)
(defvar *ignored-conversions* nil)

(defmethod conversions :around ((context context))
  (let ((convs (call-next-method)))
    (cond (*only-use-conversions*
	   (remove-if-not #'(lambda (x)
			      (member (string (id x)) *only-use-conversions*
				      :test #'string=))
	     convs))
	  (*ignored-conversions*
	   (remove-if #'(lambda (x)
			  (member (string (id x)) *ignored-conversions*
				  :test #'string=))
	     convs))
	  (t convs))))

(defun find-conversions* (conversions ftype &optional result)
  (if (null conversions)
      result
      (append (get-conversions ftype) result)))

;;; Given a type, find the set of compatible conversion names.  These are
;;; in the order of preference.  Note that the conversion name has the
;;; conversion-decl as its declaration.

(defmethod get-conversions ((type type-expr))
  (compatible-conversions (current-conversions)
			  type
			  (disabled-conversions *current-context*)))

(defmethod get-conversions ((type dep-binding))
  (get-conversions (type type)))

(defun compatible-conversions (conversions type disabled-convs
					   &optional result)
  (if (null conversions)
      (nreverse result)
      (let ((cos (compatible-conversion (car conversions) type)))
	(compatible-conversions
	 (cdr conversions)
	 type
	 disabled-convs
	 (if (and cos
		  (not (member cos disabled-convs :test #'tc-eq :key #'expr)))
	     (cons cos result)
	     result)))))

;; (defun disabled-conversion-test (conv disabled)
;;   (and (eq (id (expr conv)) (id (expr disabled)))
;;        (or (not (fully-instantiated? (expr disabled)))
;; 	   (tc-eq (expr conv) (expr disabled)))))

(defun compatible-conversion (conversion type)
  (let* ((ctype (find-supertype (type conversion)))
	 (fparams (remove-if #'(lambda (fp)
				 (memq fp (formals-sans-usings
					     (current-theory))))
		    (free-params ctype)))
	 (theory (when fparams (module (car fparams))))
	 (fmls (when theory (formals-sans-usings theory))))
    (if (and fmls
	     (not (eq theory (current-theory)))
	     (not (fully-instantiated? ctype)))
	(let ((bindings (tc-match-conversion conversion type ctype
					     (mapcar #'list fmls))))
	  (when (and bindings (every #'cdr bindings))
	    (let* ((acts (mapcar #'(lambda (a)
				     (mk-res-actual (cdr a) theory))
				 bindings))
		   (nmi (mk-modname (id theory) acts))
		   (*generate-tccs* 'none)
		   (*smp-include-actuals* t))
	      (and (with-no-type-errors
		    (let ((*current-context*
			   (if (free-params type)
			       (or (saved-context
				    (module (car (free-params type))))
				   *current-context*)
			       *current-context*)))
		      (subtypes-satisfied? acts fmls)))
		   (let ((sconv (subst-mod-params (expr conversion) nmi theory)))
		     (when (and (compatible? (type sconv) type)
				(check-conversion sconv))
		       (make-instance 'conversion-result
			 :expr (copy sconv)
			 :conversion conversion)))))))
	(when (compatible? ctype type)
	  (make-instance 'conversion-result
	    :expr (copy (expr conversion))
	    :conversion conversion)))))

(defmethod type ((conv-res conversion-result))
  (type (expr conv-res)))

(defmethod k-combinator? ((conv-res conversion-result))
  (k-combinator? (expr conv-res)))

(defun tc-match-conversion (conversion type ctype bindings)
  (let ((nbindings (tc-match type ctype bindings)))
    (when nbindings
      (if (every #'cdr nbindings)
	  nbindings
	  (tc-match-conversion* (expr conversion) nbindings)))))

(defmethod tc-match-conversion* ((expr application) bindings)
  (let ((nbindings (tc-match (type (argument expr))
			     (domain (type (operator expr)))
			     bindings)))
    (if nbindings
	(if (every #'cdr nbindings)
	    nbindings
	    (tc-match (domain (type (operator expr)))
		      (type (argument expr))
		      nbindings))
	(tc-match (domain (type (operator expr)))
		  (type (argument expr))
		  bindings))))

(defmethod tc-match-conversion* ((expr name-expr) bindings)
  bindings)

(defun subtypes-satisfied? (actuals formals &optional alist)
  (or (notany #'(lambda (fm) (typep fm 'formal-subtype-decl)) formals)
      (multiple-value-bind (nfml nalist)
	  (subst-actuals-in-next-formal (car actuals) (car formals) alist)
	(and (or (not (typep nfml 'formal-subtype-decl))
		 (subtype-of? (type-canon (type-value (car actuals)))
			      (type-canon (type-value nfml))))
	     (subtypes-satisfied? (cdr actuals) (cdr formals) nalist)))))

(defun check-conversion (name)
  (let ((type (find-supertype (type name))))
    (unless (strict-compatible? (domain type) (range type))
      (setf (types name) (list (type name)))
      name)))


;;; Given a type, find the set of compatible k-conversion names (properly
;;; instantiated).  This differs from get-conversions in that we are only using
;;; conversions that are k-combinators, and matching against the range rather
;;; than the domain of the conversion type.

(defmethod get-k-conversions ((type type-expr))
  (compatible-k-conversions (current-conversions) type
			    (disabled-conversions *current-context*)))

(defun compatible-k-conversions (conversions type disabled-convs
					     &optional result)
  (if (null conversions)
      (nreverse result)
      (compatible-k-conversions
       (cdr conversions)
       type
       disabled-convs
       (let ((cos (when (k-combinator? (car conversions))
		    (compatible-k-conversion (car conversions) type))))
	 (if (and cos
		  (not (member cos disabled-convs :test #'tc-eq :key #'expr)))
	     (cons cos result)
	     result)))))

(defun compatible-k-conversion (conversion type)
  (let* ((ctype (range (find-supertype (type conversion))))
	 (fparams (free-params ctype))
	 (ctheory (when fparams (module (car fparams))))
	 (fmls (when ctheory (formals-sans-usings ctheory))))
    (if (and fmls
	     (not (fully-instantiated? ctype)))
	(let ((bindings (tc-match type ctype (mapcar #'list fmls))))
	  (when (and bindings (every #'cdr bindings))
	    (let ((*smp-include-actuals* t)
		  (nmi (mk-modname (id ctheory)
			 (mapcar #'(lambda (a)
				     (mk-res-actual (cdr a) ctheory))
				 bindings))))
	      (subst-mod-params (expr conversion) nmi ctheory))))
	(when (compatible? ctype type)
	  (expr conversion)))))


(defun all-possible-instantiations (dtypes arguments)
  (when (length= dtypes arguments)
    (if (free-params dtypes)
	(let* ((bindings (instantiate-operator-bindings (free-params dtypes)))
	       (bindings-list (all-possible-bindings dtypes arguments bindings)))
	  (or (all-possible-instantiations* dtypes bindings-list)
	      (list dtypes)))
	(list dtypes))))

(defun all-possible-instantiations* (dtypes bindings-list &optional result)
  (cond ((null bindings-list)
	 (nreverse result))
	(t (assert (every #'cdr (car bindings-list)))
	   (let ((dtypes-inst (instantiate-operator-from-bindings
			       dtypes (car bindings-list))))
	     (all-possible-instantiations* dtypes (cdr bindings-list)
					   (pushnew dtypes-inst result
						    :test #'tc-eq))))))
      

(defun all-possible-bindings (dtypes arguments bindings)
  (cond ((every #'cdr bindings)
	 (list bindings))
	((null dtypes)
	 nil)
	(t (let ((abindings (all-possible-bindings*
			     (car dtypes) (types (car arguments)) bindings)))
	     (delete-duplicates
	      (mapcan #'(lambda (bnd)
			  (all-possible-bindings
			   (cdr dtypes) (cdr arguments) bnd))
		(if (member bindings abindings :test #'equal)
		    abindings
		    (nconc abindings (list bindings))))
	      :test #'equal)))))

(defun all-possible-bindings* (dtype atypes bindings)
  (if (free-params dtype)
      (possible-bindings dtype atypes bindings)
      (list bindings)))

(defun possible-bindings (dtype atypes bindings &optional bindings-list)
  (if (null atypes)
      (nreverse bindings-list)
      (let* ((nbindings (tc-match (car atypes) dtype (copy-tree bindings))))
	(possible-bindings dtype (cdr atypes) bindings
			   (if (and nbindings
				    (not (member nbindings bindings-list
						 :test #'equal)))
			       (cons nbindings bindings-list)
			       bindings-list)))))

(defmethod find-proj-application-conversion ((ex projection-application))
  (find-proj-application-conversion*
   (current-conversions) (argument ex) (index ex)))

(defun find-proj-application-conversion* (conversions ex index)
  (when conversions
    (let ((conv (compatible-tupletype-conversion
		 (car conversions) ex index)))
      (if (and conv
	       (not (member conv (disabled-conversions *current-context*)
			    :key #'expr :test #'tc-eq)))
	  (let* ((ctype (type conv))
		 (dom (domain (find-supertype ctype)))
		 (nex (copy ex)))
	    (add-conversion-info (expr conv) ex)
	    (change-class ex 'implicit-conversion)
	    (setf (argument ex) nex)
	    (setf (types nex)
		  (list (if (typep dom 'dep-binding) (type dom) dom)))
	    (setf (operator ex) (copy (expr conv)))
	    (setf (types ex) (list ctype))
	    (typecheck* ex nil nil nil))
	  (find-proj-application-conversion*
	   (cdr conversions) ex index)))))

(defun compatible-tupletype-conversion (conversion ex index)
  (let* ((theory (module conversion))
	 (ctype (find-supertype (type (expr conversion))))
	 (fmls (formals-sans-usings theory)))
    (and (typep ctype 'funtype)
	 (typep (find-supertype (range ctype)) 'tupletype)
	 (<= index (length (types (find-supertype (range ctype)))))
	 (if (and fmls
		  (not (eq theory (current-theory)))
		  (remove-if #'(lambda (fp)
				 (memq fp (formals-sans-usings
					   (current-theory))))
		    (free-params conversion)))
	     (compatible-tupletype-conversion*
	      (ptypes ex) conversion ex index)
	     (when (some #'(lambda (atype)
			     (compatible? atype (domain ctype)))
			 (ptypes ex))
	       conversion)))))

(defun compatible-recordtype-conversion* (types conversion ex index)
  (when types
    (let* ((theory (module conversion))
	   (ctype (find-supertype (type (expr conversion))))
	   (fmls (formals-sans-usings theory))
	   (bindings (tc-match (car types) (domain ctype)
			       (mapcar #'list fmls))))
      (if (and bindings (every #'cdr bindings))
	  (let* ((acts (mapcar #'(lambda (a)
				   (mk-res-actual (cdr a) theory))
			 bindings))
		 (nmi (mk-modname (id theory) acts))
		 (cdecl (when (with-no-type-errors
			       (check-compatible-params
				(formals-sans-usings theory) acts nil))
			  (subst-params-decl conversion nmi theory))))
	    (if cdecl
		(make-instance 'conversion-result
		  :expr (expr cdecl)
		  :conversion cdecl)
		(compatible-recordtype-conversion*
		 (cdr types) conversion ex index)))
	  (compatible-recordtype-conversion*
	   (cdr types) conversion ex index)))))


;;; Field applications - more difficult than projections because they can be
;;; applications of the form fld(ex).

(defmethod find-field-application-conversion ((ex field-application))
  (find-field-application-conversion*
   (current-conversions) (argument ex) (id ex)))

(defun find-field-application-conversion* (conversions ex field-id)
  (when conversions
    (let ((conv (compatible-recordtype-conversion
		 (car conversions) ex field-id)))
      (if (and conv
	       (not (member conv (disabled-conversions *current-context*)
			    :key #'expr :test #'tc-eq)))
	  (let* ((ctype (type conv))
		 (dom (domain (find-supertype ctype)))
		 (nex (copy ex)))
	    (add-conversion-info (expr conv) ex)
	    (change-class ex 'implicit-conversion)
	    (setf (argument ex) nex)
	    (setf (types nex)
		  (list (if (typep dom 'dep-binding) (type dom) dom)))
	    (setf (operator ex) (copy (expr conv)))
	    (setf (types ex) (list ctype))
	    (typecheck* ex nil nil nil))
	  (find-field-application-conversion*
	   (cdr conversions) ex field-id)))))

(defun compatible-recordtype-conversion (conversion ex field-id)
  (let* ((theory (module conversion))
	 (ctype (find-supertype (type (expr conversion))))
	 (fmls (formals-sans-usings theory)))
    (and (typep ctype 'funtype)
	 (typep (find-supertype (range ctype)) 'recordtype)
	 (member field-id (fields (find-supertype (range ctype))) :key #'id)
	 (if (and fmls
		  (not (eq theory (current-theory)))
		  (remove-if #'(lambda (fp)
				 (memq fp (formals-sans-usings
					   (current-theory))))
		    (free-params conversion)))
	     (compatible-recordtype-conversion*
	      (ptypes ex) conversion ex field-id)
	     (when (some #'(lambda (atype)
			     (compatible? atype (domain ctype)))
			 (ptypes ex))
	       conversion)))))

;; (defun compatible-recordtype-conversion* (types conversion ex field-id)
;;   (when types
;;     (let* ((theory (module conversion))
;; 	   (ctype (find-supertype (type (expr conversion))))
;; 	   (fmls (formals-sans-usings theory))
;; 	   (bindings (tc-match (car types) (domain ctype)
;; 			       (mapcar #'list fmls))))
;;       (if (and bindings (every #'cdr bindings))
;; 	  (let* ((acts (mapcar #'(lambda (a)
;; 				   (mk-res-actual (cdr a) theory))
;; 			 bindings))
;; 		 (nmi (mk-modname (id theory) acts)))
;; 	    (when (with-no-type-errors
;; 		   (check-compatible-params
;; 		    (formals-sans-usings theory) acts nil))
;; 	      (subst-params-decl conversion nmi theory) (car types)))
;; 	  (compatible-recordtype-conversion*
;; 	   (cdr types) conversion ex field-id)))))

(defun get-recordtype-conversion-resolutions (name arguments)
  (when (and (null (library name))
	     (null (mod-id name))
	     (null (actuals name)))
    (get-recordtype-conversion-resolutions*
     (current-conversions) (mk-arg-tuple-expr* arguments) (id name))))

(defun get-recordtype-conversion-resolutions* (conversions ex field-id
							   &optional reses)
  (if (null conversions)
      (nreverse reses)
      (let* ((conv (compatible-recordtype-conversion
		   (car conversions) ex field-id))
	     (res (when conv
		    (make-recordtype-conversion-resolution conv ex field-id))))
	(get-recordtype-conversion-resolutions*
	 (cdr conversions) ex field-id
	 (if res
	     (cons res reses)
	     reses)))))

(defun make-recordtype-conversion-resolution (conv ex field-id)
  (let* ((rtype (range (type (expr conv))))
	 (fdecl (find field-id (fields rtype) :key #'id))
	 (ftype
	  (if (and (dependent? rtype)
		   (some #'(lambda (x)
			     (and (not (eq x fdecl))
				  (occurs-in x fdecl)))
			 (fields rtype)))
	      (let* ((db (mk-dep-binding
			  (make-new-variable '|r| rtype)
			  rtype))
		     (ne (make-dep-field-name-expr
			  db rtype))
		     (ftype (field-application-type
			     (id fdecl) rtype ne)))
		(mk-funtype db ftype))
	      (mk-funtype (list rtype) (type fdecl)))))
    (change-class (make-resolution fdecl (current-theory-name) ftype)
	'conversion-resolution
      'conversion conv)))

(defmethod find-update-conversions (expr type)
  (typecheck-assignments (assignments expr) type))

(defmethod find-update-conversions (expr (rtype recordtype))
  (assert (update-expr? expr))
  (let* ((conversions (current-conversions))
	 (assns (assignments expr))
	 (rfields (fields rtype))
	 (conv (find-record-update-conversion conversions rtype assns)))
    (if conv
	(break "Need to finish this")
	(typecheck-assignments (assignments expr) rtype))))

(defun find-record-update-conversion (convs rtype assns)
  (when convs
    (if (compatible-record-update-conversion (car convs) rtype assns)
	(car convs)
	(find-record-update-conversion (cdr convs) rtype assns))))

(defun compatible-record-update-conversion (conv rtype assns)
  (let* ((theory (module conv))
	 (ctype (find-supertype (type (expr conv))))
	 (fmls (formals-sans-usings theory)))
    (and (typep ctype 'funtype)
	 (compatible? (domain ctype) rtype)
	 (recordtype? (find-supertype (range ctype)))
	 (every #'(lambda (nassn)
		    (member (caar (arguments nassn))
			    (fields rtype) :test #'same-id))
		assns))))
  
