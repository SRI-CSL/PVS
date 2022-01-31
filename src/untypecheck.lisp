;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; untypecheck.lisp -- Untypecheck-Theory removes types, resolutions,
;;                     and references to other declarations.
;; Author          : Sam Owre
;; Created On      : Mon Oct 25 23:18:47 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jan 28 14:16:59 1999
;; Update Count    : 20
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

;;; For now the entire theory will be untypechecked.

(in-package :pvs)

(export '(untypecheck-theory))

(defvar *untypechecking-theory* nil)

;;; Untypechecking a theory removes all types and resolutions, and goes
;;; up the using chain recursively.  The used-by slot is used for this
;;; purpose; it is set in xref.lisp.

(defmethod untypecheck-theory ((dorm datatype-or-module))
  (let ((fe (when (filename dorm)
	      (get-context-file-entry (filename dorm)))))
    (when (and fe
	       (ce-object-date fe))
      (setf (ce-object-date fe) nil)
      (setf (pvs-context-changed *workspace-session*) t)))
  (untypecheck-theory (formals dorm))
  (setf (assuming dorm)
	(remove-if #'(lambda (d) (and (declaration? d)
				      (generated-by d)))
	  (assuming dorm)))
  (untypecheck-theory (assuming dorm))
  (setf (status dorm) nil)
  (setf (tcc-comments dorm) nil)
  (setf (info dorm) nil)
  (setf (warnings dorm) nil)
  (setf (conversion-messages dorm) nil)
  (setf (all-declarations dorm) nil)
  (setf (all-imported-theories dorm) 'unbound)
  (setf (all-imported-names dorm) 'unbound))

(defmethod untypecheck-theory ((adt recursive-type))
  (let ((*untypechecking-theory* (if (inline-recursive-type? adt)
				     *untypechecking-theory*
				     (current-theory))))
    (untypecheck-theory (importings adt))
    (untypecheck-theory (constructors adt))
    (call-next-method)
    (when (typep adt 'inline-datatype)
      (setf (typechecked? adt) nil))
    (setf (adt-type-name adt) nil)
    (setf (adt-theory adt) nil)
    (setf (adt-map-theory adt) nil)
    (setf (adt-reduce-theory adt) nil)
    (when (inline-recursive-type? adt)
      (setf (generated adt) nil))
    (setf (generated-file-date adt) nil)
    ;;(setf (positive-types adt) nil)
    ;;(setf (saved-context adt) nil)
    ))

(defmethod untypecheck-theory ((con simple-constructor))
  (dolist (a (arguments con))
    (untypecheck-theory (declared-type a))
    (setf (type a) nil)
    (setf (bind-decl a) nil))
  (setf (con-decl con) nil)
  (setf (rec-decl con) nil)
  (setf (acc-decls con) nil))

(defmethod untypecheck-theory ((theory module))
  (let ((*untypechecking-theory* theory))
    (cond ((assq theory *tc-theories*)
	   (let ((cdecl (cdr (assq theory *tc-theories*))))
	     (untypecheck-theory-after-decl theory cdecl)))
	  ((eq theory (current-theory))
	   (untypecheck-theory-after-decl theory (current-declaration)))
	  (t
	   (tcdebug "~%Untypechecking theory ~a" (id theory))
	   ;;(setf (declarations theory) (make-hash-table :test #'eq :size 20))
	   (dolist (ty-decl (nonempty-types theory))
	     (setf (nonempty? (car ty-decl)) nil))
	   (setf (nonempty-types theory) nil)
	   (untypecheck-theory (exporting theory))
	   (setf (all-usings theory) nil)
	   (setf (immediate-usings theory) 'unbound)
	   (call-next-method)
	   (setf (instances-used theory) nil)
	   (setf (assuming-instances theory) nil)
	   (setf (used-by theory) nil)
	   (setf (theory theory) (remove-if #'(lambda (d)
						(and (declaration? d)
						     (generated-by d)))
				   (theory theory)))
	   (untypecheck-theory (theory theory))
	   (setf (tccs-tried? theory) nil)
	   (setf (modified-proof? theory) nil)
	   (remove-associated-buffers (id theory))
	   (setf (ppe-form theory) nil)
	   (setf (tcc-form theory) nil)
	   (setf (saved-context theory) nil)))))

(defun untypecheck-theory-after-decl (theory decl)
  ;; Need to only untypecheck declarations following the given one
  (dolist (ty-decl (nonempty-types theory))
    (setf (nonempty? (car ty-decl)) nil))
  (setf (nonempty-types theory) nil)
  (untypecheck-theory (exporting theory))
  (setf (all-usings theory) nil)
  (setf (immediate-usings theory) 'unbound)
  ;; next-method
  (setf (instances-used theory) nil)
  (setf (assuming-instances theory) nil)
  (setf (used-by theory) nil)
  (let ((rem-decls (cdr (memq decl (all-decls theory)))))
    ;; This doesn't quite work - see store-declref for the problem
;;     (when (eq theory (current-theory))
;;       (dolist (d (ldiff (all-decls theory) rem-decls))
;; 	(unless (or (not (typep d '(or declaration inline-recursive-type)))
;; 		    (eq (module d) (current-theory)))
;; 	  (setf (module d) (current-theory)))))
    (setf (warnings theory)
	  (remove-if #'(lambda (i) (memq (car i) rem-decls))
	    (warnings theory)))
    (setf (info theory)
	  (remove-if #'(lambda (i) (memq (car i) rem-decls)) (info theory)))
    (setf (conversion-messages theory)
	  (remove-if #'(lambda (i) (memq (car i) rem-decls))
	    (conversion-messages theory))))
  (let* ((fdecls (memq decl (formals theory)))
	 (adecls (unless fdecls (memq decl (assuming theory))))
	 (tdecls (unless (or fdecls adecls) (memq decl (theory theory)))))
    (cond (fdecls
	   (setf (formals theory)
		 (append (ldiff (formals theory) fdecls)
			 (remove-if #'(lambda (d)
					(and (declaration? d)
					     (generated-by d)))
			   fdecls)))
	   (setf (assuming theory)
		 (remove-if #'(lambda (d)
				(and (declaration? d)
				     (generated-by d)))
		   (assuming theory)))
	   (setf (theory theory)
		 (remove-if #'(lambda (d)
				(and (declaration? d)
				     (generated-by d)))
		   (theory theory))))
	  (adecls
	   (setf (assuming theory)
		 (append (ldiff (assuming theory) adecls)
			 (remove-if #'(lambda (d)
					(and (declaration? d)
					     (generated-by d)))
			   adecls)))
	   (setf (theory theory)
		 (remove-if #'(lambda (d)
				(and (declaration? d)
				     (generated-by d)))
		   (theory theory))))
	  (tdecls
	   (setf (theory theory)
		 (append (ldiff (theory theory) tdecls)
			 (remove-if #'(lambda (d)
					(and (declaration? d)
					     (generated-by d)))
			   tdecls))))
	  (t nil))
    (let* ((untc-decls (cond (fdecls
			      (append fdecls
				      (assuming theory) (theory theory)))
			     (adecls
			      (append adecls (theory theory)))
			     (tdecls tdecls))))
      (untypecheck-theory untc-decls)))
  (setf (all-declarations theory) nil)
  (setf (tccs-tried? theory) nil)
  (setf (modified-proof? theory) nil)
  (remove-associated-buffers (id theory))
  (setf (ppe-form theory) nil)
  (setf (tcc-form theory) nil)
  (setf (saved-context theory) nil))

(defmethod untypecheck-theory ((exp exporting))
  (if (memq (kind exp) '(all closure default))
      (setf (modules exp) nil)
      (untypecheck-theory (modules exp)))
  (unless (symbolp (names exp))
    (untypecheck-theory (names exp)))
  (unless (symbolp (but-names exp))
    (untypecheck-theory (but-names exp)))
  (setf (closure exp) nil))

(defmethod untypecheck-theory ((name expname))
  (setf (type name) nil))

(defmethod untypecheck-theory ((using importing))
  (setf (refers-to using) nil)
  (untypecheck-theory (theory-name using))
  (setf (generated using) nil)
  (setf (saved-context using) nil))

(defmethod untypecheck-theory ((list list))
  (mapc #'untypecheck-theory list))


;;; Untypechecking a decl clears out all of the type information of that
;;; decl, and then propagates to the referred-by declarations according
;;; to the change parameter.

(defmethod untypecheck-theory :around ((decl declaration))
  (when (and (module decl)
	     (not (generated-by decl)))
    (tcdebug "~%  Untypechecking ~a ~a!~a"
	     (type-of decl) (id (module decl)) (id decl))
    ;;(untypecheck-references decl)
    (when (module? (module decl))
      (setf (theory (module decl))
	    (delete-if #'(lambda (d) (memq d (generated decl)))
	      (theory (module decl)))))
;;     (mapc #'(lambda (rd)
;; 	      (unless (or (module? rd) (from-prelude? rd))
;; 		(setf (referred-by rd)
;; 		      (remove decl (referred-by rd)))))
;; 	  (refers-to decl))
;;     (mapc #'(lambda (d)
;; ; 	      (setf (gethash (id d) (declarations (module decl)))
;; ; 		    (remove d (gethash (id d) (declarations (module decl)))))
;; 	      (mapc #'(lambda (rd)
;; 			(unless (from-prelude? rd)
;; 			  (setf (referred-by rd)
;; 				(remove d (referred-by rd)))))
;; 		    (refers-to d)))
;; 	  (generated decl))
    (setf (status (module decl))
	  (remove 'typechecked (status (module decl))))
    (call-next-method)
    (untypecheck-theory (formals decl))
    (setf (module decl) nil
	  (visible? decl) nil
	  (refers-to decl) nil
	  ;;(referred-by decl) nil
	  (typechecked? decl) nil
	  (generated decl) nil)))

;;; Be careful of generated declarations.

;; (defun untypecheck-references (decl)
;;   (dolist (d (append (referred-by decl)
;; 		     (mapcan #'(lambda (d)
;; 				 (copy-list (referred-by d)))
;; 			     (generated decl))))
;;     (unless (eq d decl)
;;       (untypecheck-theory d))))

;; (defmethod referred-by ((decl field-decl))
;;   nil)

(defmethod refers-to ((decl field-decl))
  nil)

(defmethod untypecheck-theory ((decl typed-declaration))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (declared-type decl))
  (setf (type decl) nil))

(defmethod untypecheck-theory ((decl formal-type-decl))
  (when (next-method-p) (call-next-method))
  ;;(setf (nonempty? decl) nil)
  (setf (type decl) nil))

(defmethod untypecheck-theory ((decl formal-const-decl))
  (when (next-method-p) (call-next-method))
  (setf (type decl) nil)
  (untypecheck-theory (declared-type decl)))

(defmethod untypecheck-theory ((decl formal-theory-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (theory-name decl))
  (setf (saved-context decl) nil)
  (setf (theory-mappings decl) nil))

(defmethod untypecheck-theory ((decl lib-decl))
  (setf (lib-ref decl) nil))

(defmethod untypecheck-theory ((decl mod-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (actuals (modname decl)))
  (setf (saved-context decl) nil)
  (setf (theory-mappings decl) nil))

(defmethod untypecheck-theory ((decl theory-abbreviation-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (actuals (theory-name decl)))
  (setf (saved-context decl) nil))

(defmethod untypecheck-theory ((decl type-decl))
  (when (next-method-p) (call-next-method))
  (setf (type-value decl) nil))

(defmethod untypecheck-theory ((decl type-def-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (type-expr decl))
  (when (contains decl)
    (untypecheck-theory (contains decl))))

;; var-decl is a typed-declaration

(defmethod untypecheck-theory ((decl const-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (definition decl))
  (setf (def-axiom decl) nil)
  (when (eval-info decl) (remove-eval-info decl))
  ;;(setf (adt-kind decl) nil)
  )

;;; in-name etc are macros, defined in eval-macros.lisp
;;; only recognizer-decls should unintern - all others should undef
(defmethod remove-eval-info ((decl adt-recognizer-decl))
  (when (in-info decl)
    (eval-unintern (in-name   decl))
    (eval-unintern (in-name-m decl))
    (eval-unintern (in-name-d decl)))
  (when (ex-info decl)
    (eval-unintern (ex-name   decl))
    (eval-unintern (ex-name-m decl))
    (eval-unintern (ex-name-d decl)))
  (setf (eval-info decl) nil))

(defmethod remove-eval-info ((decl const-decl))
  (when (in-info decl)
    (eval-undef (in-name   decl))
    (eval-undef (in-name-m decl))
    (eval-undef (in-name-d decl)))
  (when (ex-info decl)
    (eval-undef (ex-name   decl))
    (eval-undef (ex-name-m decl))
    (eval-undef (ex-name-d decl)))
  (setf (eval-info decl) nil))

(defun eval-unintern (name)
  (when (and name (symbolp name))
    (unintern name)))

(defun eval-undef (name)
  (when (and name (symbolp name) (fboundp name))
    (fmakunbound name)))
   
(defmethod untypecheck-theory ((decl def-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (declared-measure decl))
  (setf (measure decl) nil)
  (untypecheck-theory (ordering decl))
  (setf (measure-depth decl) nil)
  (setf (recursive-signature decl) nil))
  

;;; inductive-decl is a const-decl

(defmethod untypecheck-theory ((decl formula-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (definition decl))
  (setf (kind decl) nil)
  (setf (proof-refers-to decl) nil)
  (setf (closed-definition decl) nil)
  (setf (default-proof decl) nil)
  (setf (proofs decl) nil))

(defmethod untypecheck-theory ((decl conversion-decl))
  (when (next-method-p) (call-next-method))
  (setf (k-combinator? decl) nil)
  (untypecheck-theory (expr decl)))

(defmethod untypecheck-theory ((decl assuming-decl))
  (setf (definition decl) (original-definition decl))
  (call-next-method))

(defmethod untypecheck-theory ((decl subtype-judgement))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (declared-subtype decl))
  (setf (subtype decl) nil))

(defmethod untypecheck-theory ((decl number-judgement))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (number-expr decl)))

(defmethod untypecheck-theory ((decl name-judgement))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (name decl)))

(defmethod untypecheck-theory ((decl application-judgement))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (name decl))
  (untypecheck-theory (formals decl)))

(defmethod untypecheck-theory ((decl auto-rewrite-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (rewrite-names decl)))


;;; Type Expressions

(defmethod untypecheck-theory ((te type-expr))
  (when (next-method-p) (call-next-method))
  (setf (from-conversion te) nil)
  (setf (nonempty? te) nil)
  (setf (print-type te) nil)
  ;;(setf (no-freevars? te) nil)
  (setf (free-variables te) 'unbound)
  (setf (free-parameters te) 'unbound))

(defmethod untypecheck-theory ((te type-name))
  (when (next-method-p) (call-next-method))
  ;;(setf (adt te) nil)
  )

(defmethod untypecheck-theory ((te adt-type-name))
  (call-next-method (change-class te 'type-name)))

(defmethod untypecheck-theory ((te type-application))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (type te))
  (untypecheck-theory (parameters te)))

(defmethod untypecheck-theory ((te subtype))
  (when (next-method-p) (call-next-method))
  (unless (typep te 'setsubtype)
    (setf (supertype te) nil))
  (untypecheck-theory (predicate te)))

(defmethod untypecheck-theory ((te setsubtype))
  (setf (predicate te) nil)
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (formals te))
  (untypecheck-theory (formula te)))

(defmethod untypecheck-theory ((te nsetsubtype))
  (setf (supertype te) nil)
  (when (next-method-p) (call-next-method)))

(defmethod untypecheck-theory ((te expr-as-type))
  (setf (supertype te) nil)
  (setf (predicate te) nil)
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (expr te)))

(defmethod untypecheck-theory ((te funtype))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (domain te))
  (untypecheck-theory (range te)))

(defmethod untypecheck-theory ((te tupletype))
  (assert (not (generated? te)))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (types te)))

(defmethod untypecheck-theory ((te cotupletype))
  (assert (not (generated? te)))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (types te)))

(defmethod untypecheck-theory ((te recordtype))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (fields te))
  (setf (dependent? te) nil))


;;; Expressions

(defmethod untypecheck-theory :around ((ex expr))
  (let* ((utheory (or *untypechecking-theory* (current-theory)))
	 (macro (when utheory (assq ex (macro-expressions utheory)))))
    (cond (macro
	   (untypecheck-macro ex macro utheory))
	  (t (call-next-method)))))

(defun untypecheck-macro (ex macro utheory)
  (change-class ex (class-of (cdr macro)))
  (copy-slots ex (cdr macro))
  (setf (macro-expressions utheory)
	(delete macro (macro-expressions utheory)))
  (untypecheck-theory ex))

(defmethod untypecheck-theory ((ex expr))
  (when (next-method-p) (call-next-method))
  (setf (type ex) nil)
  (setf (types ex) nil)
  ;;(setf (no-freevars? ex) nil)
  (setf (free-variables ex) 'unbound)
  (setf (free-parameters ex) 'unbound))

(defmethod untypecheck-theory ((ex name))
  (assert (not (memq ex (list *boolean* *number*))))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (actuals ex))
  (untypecheck-theory (mappings ex))
  (untypecheck-theory (target ex))
  (setf (resolutions ex) nil))

; (defmethod untypecheck-theory ((ex name-expr))
;   ;;(when (eq (id ex) '|real_pred|) (break))
;   (when (next-method-p) (call-next-method)))

(defmethod untypecheck-theory ((ex constructor-name-expr))
  (setf (accessor-names ex) nil)
  (setf (recognizer-name ex) nil)
  (when (next-method-p) (call-next-method)))

(defmethod untypecheck-theory ((ex recognizer-name-expr))
  (setf (constructor-name ex) nil)
  (setf (unit? ex) 'unbound)
  (when (next-method-p) (call-next-method)))

(defmethod untypecheck-theory ((ex adt-name-expr))
  (call-next-method)
  (unless (null-expr? ex)
    (change-class ex 'name-expr)))

(defmethod untypecheck-theory ((ex bind-decl))
  (when (next-method-p) (call-next-method))
  (when (declared-type ex)
    (untypecheck-theory (declared-type ex))))

;(defmethod untypecheck-theory ((ex projection-expr))
;  (when (next-method-p) (call-next-method))
;  ;;(setf (index ex) nil)
;  )

(defmethod untypecheck-theory ((ex tuple-expr))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (exprs ex)))

(defmethod untypecheck-theory ((ex record-expr))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (assignments ex)))

(defmethod untypecheck-theory ((ex cases-expr))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (expression ex))
  (untypecheck-theory (selections ex))
  (untypecheck-theory (else-part ex)))

(defmethod untypecheck-theory ((ex selection))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (constructor ex))
  (mapc #'(lambda (a)
	    (setf (declared-type a) nil)
	    (setf (type a) nil))
	(args ex))
  (untypecheck-theory (args ex))
  (untypecheck-theory (expression ex)))

(defmethod untypecheck-theory ((ex projection-expr))
  (call-next-method)
  (untypecheck-theory (actuals ex)))

(defmethod untypecheck-theory ((ex injection-expr))
  (call-next-method)
  (untypecheck-theory (actuals ex)))

(defmethod untypecheck-theory ((ex injection?-expr))
  (call-next-method)
  (untypecheck-theory (actuals ex)))

(defmethod untypecheck-theory ((ex extraction-expr))
  (call-next-method)
  (untypecheck-theory (actuals ex)))

(defmethod untypecheck-theory ((ex projection-application))
  (call-next-method)
  (untypecheck-theory (actuals ex))
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex injection-application))
  (call-next-method)
  (untypecheck-theory (actuals ex))
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex injection?-application))
  (call-next-method)
  (untypecheck-theory (actuals ex))
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex extraction-application))
  (call-next-method)
  (untypecheck-theory (actuals ex))
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex field-application))
  (if (fieldappl? ex)
      (call-next-method)
      (let ((op (mk-name-expr (id ex))))
	(call-next-method)
	(change-class ex 'application)
	(setf (operator ex) op)
	(untypecheck-theory (argument ex)))))

(defmethod untypecheck-theory ((ex fieldappl))
  (call-next-method)
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex fieldex-lambda-expr))
  (untypecheck-theory (actuals ex))
  (change-class ex 'fieldex 'id (id (expression ex))))

(defmethod untypecheck-theory ((ex application))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (operator ex))
  (untypecheck-theory (argument ex)))

(defmethod untypecheck-theory ((ex list-expr))
  (let ((list-ex ex))
    (loop while (list-expr? list-ex)
       do (progn (setf (type list-ex) nil)
		 (setf (types list-ex) nil)
		 (setf (type (argument list-ex)) nil)
		 (setf (types (argument list-ex)) nil)
		 (setf (free-variables list-ex) 'unbound)
		 (setf (free-parameters list-ex) 'unbound)
		 (untypecheck-theory (operator list-ex))
		 (untypecheck-theory (args1 list-ex))
		 (setq list-ex (args2 list-ex))))
    (untypecheck-theory list-ex)))

(defmethod untypecheck-theory ((ex argument-conversion))
  (let ((expr (operator ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex lambda-conversion))
  (let ((expr (expression ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex implicit-conversion))
  (let ((expr (argument ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex field-conversion))
  (let ((expr (argument ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex projection-conversion))
  (let ((expr (argument ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex injection-conversion))
  (let ((expr (argument ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex extraction-conversion))
  (let ((expr (argument ex)))
    (untypecheck-conversion ex expr)))

(defmethod untypecheck-theory ((ex recursive-defn-conversion))
  (if *dont-untypecheck-recdef-conversions*
      (call-next-method)
      (let ((expr (from-expr ex)))
	(untypecheck-conversion ex expr))))

(defun untypecheck-conversion (ex expr)
  (change-class ex (class-of expr))
  (copy-slots ex expr)
  (untypecheck-theory ex))

(defmethod untypecheck-theory ((ex set-list-expr))
  (setf (bindings ex) nil
	(expression ex) nil)
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (exprs ex)))

(defmethod untypecheck-theory ((ex binding-expr))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (bindings ex))
  (untypecheck-theory (expression ex)))

(defmethod untypecheck-theory ((ex update-expr))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (expression ex))
  (untypecheck-theory (assignments ex)))

(defmethod untypecheck-theory ((ex assignment))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (arguments ex))
  (untypecheck-theory (expression ex)))

(defmethod untypecheck-theory ((ex field-assignment-arg))
  (when (next-method-p) (call-next-method))
  (change-class ex 'name-expr))

(defmethod untypecheck-theory ((ex simple-decl))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (declared-type ex))
  (setf (type ex) nil))

(defmethod untypecheck-theory ((ex field-decl))
  (when (next-method-p) (call-next-method))
  ;;(setf (recordtype ex) nil)
  )

(defmethod untypecheck-theory ((act actual))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (expr act))
  (setf (type-value act) nil))

(defmethod untypecheck-theory ((map mapping-with-formals))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (formals map)))

(defmethod untypecheck-theory ((map mapping))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (declared-type map))
  (setf (type map) nil)
  (untypecheck-theory (lhs map))
  (untypecheck-theory (rhs map)))

(defmethod untypecheck-theory ((map mapping-rename))
  (when (next-method-p) (call-next-method))
  (setf (mapped-decl map) nil))

(defmethod untypecheck-theory ((rhs mapping-rhs))
  (when (next-method-p) (call-next-method))
  (untypecheck-theory (expr rhs))
  (setf (type-value rhs) nil))

;;; Untypecheck

(defun untypecheck (obj)
  (mapobject #'untypecheck* obj)
  obj)

;;; These must return NIL or mapobject won't recurse past them.

(defmethod untypecheck* (obj)
  (declare (ignore obj))
  nil)

(defmethod untypecheck* ((en expname))
  (setf (type en) nil)
  nil)

(defmethod untypecheck* ((td typed-declaration))
  (setf (type td) nil)
  (call-next-method)
  nil)

(defmethod untypecheck* ((decl declaration))
  (setf (theory (module decl))
	(remove-if #'(lambda (d) (member d (generated decl)))
	  (theory (module decl))))
  (setf (generated decl) nil)
  (call-next-method)
  nil)

(defmethod untypecheck* ((te type-expr))
  t)

(defmethod untypecheck* ((ex expr))
  (setf (type ex) nil
	(types ex) nil)
  nil)

(defmethod untypecheck* ((ex name-expr))
  (call-next-method)
  (setf (type ex) nil
	(types ex) nil)
  nil)

(defmethod untypecheck* ((n name))
  (setf (resolutions n) nil)
  nil)

(defmethod untypecheck* ((act actual))
  (untypecheck* (expr act))
  (setf (type-value act) nil)
  nil)


;;; copy-slots copies the slots of one expr into another.

(defmethod copy-slots :around ((ex1 syntax) (ex2 syntax))
  ;;(setf (newline-comment ex1) (newline-comment ex2))
  ;;(setf (abstract-syntax ex1) (abstract-syntax ex2))
  (setf (place ex1) (place ex2))
  (call-next-method))

(defmethod copy-slots :around ((ex1 expr) (ex2 expr))
  (setf (parens ex1) (parens ex2))
  (call-next-method))

(defmethod copy-slots ((ex1 name-expr) (ex2 name-expr))
  (setf (mod-id ex1) (mod-id ex2)
	(library ex1) (library ex2)
	(actuals ex1) (actuals ex2)
	(id ex1) (id ex2)
	(type ex1) (type ex2)
	(resolutions ex1) (resolutions ex2)))

(defmethod copy-slots ((ex1 projection-expr) (ex2 projection-expr))
  (setf (index ex1) (index ex2))
  (setf (actuals ex1) (actuals ex2)))

(defmethod copy-slots ((ex1 injection-expr) (ex2 injection-expr))
  (setf (index ex1) (index ex2))
  (setf (actuals ex1) (actuals ex2)))

(defmethod copy-slots ((ex1 injection?-expr) (ex2 injection?-expr))
  (setf (index ex1) (index ex2))
  (setf (actuals ex1) (actuals ex2)))

(defmethod copy-slots ((ex1 extraction-expr) (ex2 extraction-expr))
  (setf (index ex1) (index ex2))
  (setf (actuals ex1) (actuals ex2)))

(defmethod copy-slots ((ex1 projection-application) (ex2 projection-application))
  (setf (id ex1) (id ex2)
	(index ex1) (index ex2)
	(actuals ex1) (actuals ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 injection-application) (ex2 injection-application))
  (setf (id ex1) (id ex2)
	(index ex1) (index ex2)
	(actuals ex1) (actuals ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 injection?-application) (ex2 injection?-application))
  (setf (id ex1) (id ex2)
	(index ex1) (index ex2)
	(actuals ex1) (actuals ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 extraction-application) (ex2 extraction-application))
  (setf (id ex1) (id ex2)
	(index ex1) (index ex2)
	(actuals ex1) (actuals ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 field-application) (ex2 field-application))
  (setf (id ex1) (id ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 rational-expr) (ex2 rational-expr))
  (setf (number ex1) (number ex2)))

(defmethod copy-slots ((ex1 number-expr-with-radix) (ex2 number-expr-with-radix))
  (setf (number ex1) (number ex2)
	(radix ex1) (radix ex2)))

(defmethod copy-slots ((ex1 tuple-expr) (ex2 tuple-expr))
  (setf (exprs ex1) (exprs ex2)))

(defmethod copy-slots ((ex1 record-expr) (ex2 record-expr))
  (setf (assignments ex1) (assignments ex2)))

(defmethod copy-slots ((ex1 cases-expr) (ex2 cases-expr))
  (setf (expression ex1) (expression ex2)
	(selections ex1) (selections ex2)
	(else-part ex1) (else-part ex2)))

(defmethod copy-slots ((ex1 application) (ex2 application))
  (setf (operator ex1) (operator ex2)
	(argument ex1) (argument ex2)))

(defmethod copy-slots ((ex1 binding-expr) (ex2 binding-expr))
  (setf (bindings ex1) (bindings ex2)
	(expression ex1) (expression ex2)))

(defmethod copy-slots ((ex1 lambda-expr-with-type) (ex2 lambda-expr-with-type))
  (call-next-method)
  (setf (declared-ret-type ex1) (declared-ret-type ex2)
	(return-type ex1) (return-type ex2)))

(defmethod copy-slots ((ex1 update-expr) (ex2 update-expr))
  (setf (expression ex1) (expression ex2)
	(assignments ex1) (assignments ex2)))
