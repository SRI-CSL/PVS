;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; substit.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Oct 27 00:15:26 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 16:54:32 1998
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

(export '(substit make-new-bindings substit-binding))

;;; NOTE: Many attempts have been made to use hash tables to speed up this
;;; function, and for particular examples it may look like a win, but it
;;; often makes things slower.  The real slowness in this function is
;;; in the calls to pseudo-normalize, which are needed in subtypes,
;;; actuals, and type-applications.

;;; Here we are using hash tables, but we now make sure that substit* never
;;; directly calls substit.  Keep in mind that pseudo-normalize can make
;;; calls to substit.

(defvar *alist-freevars*)
(defvar *alist-boundvars*)
;; (defvar *substit-hash-heap* nil)

(defvar *substit-hash-list*)

(defvar *let-operators* nil
  "let-exprs have lambda expressions as operators, and we don't want
to set the declared-type for those bindings if already unset")

(defvar *leave-bindings-undeclared* nil
  "If set, then if declared-type of a binding is nil,
it is nil in the substituted binding")

(defmacro new-substit-hash (&rest forms)
  `(let* ((*alist-freevars* 'unbound)
	  (*alist-boundvars* 'unbound)
	  (next-hash (make-hash-table :test #'eq))
	  (*substit-hash-list* (cons next-hash nil)))
     (progn ,@forms)))

(defmacro add-substit-hash (&rest forms)
  `(let* ((*alist-freevars* 'unbound)
	  (*alist-boundvars* 'unbound)
	  (next-hash (make-hash-table :test #'eq))
	  (*substit-hash-list* (cons next-hash
				     (if (zerop (hash-table-count
						 (car *substit-hash-list*)))
					 (cdr *substit-hash-list*)
					 *substit-hash-list*))))
     (progn ,@forms)))

;; (defmacro add-substit-hash (&rest forms)
;;   `(let* ((next-hash (let ((ht (pop *substit-hash-heap*)))
;; 		       (if ht
;; 			   (clrhash ht)
;; 			   (make-hash-table :test #'eq))))
;; 	  (*substit-hash-list* (cons next-hash *substit-hash-list*)))
;;      (unwind-protect
;; 	 (progn ,@forms)
;;        (push next-hash *substit-hash-heap*))))

(defun get-substit-hash (obj)
  (get-substit-hash* obj *substit-hash-list*))

(defun get-substit-hash* (obj hashes)
  (when hashes
    (or (gethash obj (car hashes))
	(get-substit-hash* obj (cdr hashes)))))

(defsetf get-substit-hash (obj) (sobj)
  ;;(assert (or (eq obj sobj) (not (strong-tc-eq obj sobj))))
  `(setf (gethash ,obj (car *substit-hash-list*)) ,sobj))

(defun substit (obj alist)
  ;; At some point, should verify that car of every element of the
  ;; alist is a declaration.
  #+pvsdebug
  (assert (every #'(lambda (a)
		     (and (typep (car a)
				 '(or simple-decl declaration))
			  (eq (declaration (car a)) (car a))))
		 alist))
  (if (or (null obj) (null alist))
      obj
      (let* ((fvars (freevars obj))
	     (nalist (remove-if
			 #'(lambda (a)
			     (or (eq (car a) (cdr a))
				 (and (name-expr? (cdr a))
				      (eq (declaration (cdr a)) (car a)))
				 (not (member (declaration (car a)) fvars
					      :key #'declaration :test #'eq))))
		       alist)))
	#+pvsdebug ; Not quite right - dependent bindings are handled below
	(assert (every #'(lambda (a)
			   (every #'(lambda (fv)
				      (let ((fvelt (assq (declaration fv) nalist)))
					(or (null fvelt)
					    (eq fvelt a))))
				  (freevars (cdr a))))
		       nalist))
	(if (null nalist)
	    obj
	    (new-substit-hash
	     (let ((sobj (substit* obj nalist)))
	       sobj))))))

(defmethod substit* :around ((expr expr) alist)
  (declare (ignore alist))
  (if (null (freevars expr))
      expr
      (or (get-substit-hash expr)
	  (let ((nex (call-next-method)))
	    (setf (get-substit-hash expr) nex)))))

(defmethod substit* :around ((expr type-expr) alist)
  (if (some #'(lambda (fv)
		(assq (declaration fv) alist))
	    (freevars expr))
      (if *subst-type-hash*
	  (let ((result (lookup-subst-hash expr alist *subst-type-hash*)))
	    (if result
		(if (strong-tc-eq result expr)
		    expr
		    result)
		(let ((result (call-next-method)))
		  (install-subst-hash expr alist result *subst-type-hash*)
		  result)))
	  (or (get-substit-hash expr)
	      (setf (get-substit-hash expr) (call-next-method))))
      expr))

(defun lookup-subst-hash (expr alist hash)
  (gethash (cons expr
		 (pick-freevars-entries (freevars expr) alist))
	   hash))

(defun install-subst-hash (expr alist result hash)
  (let* ((fv (freevars expr))
	 (fv-subs (pick-freevars-entries fv alist)))
    (setf (gethash (cons expr fv-subs) hash)
	  result)
    t))

(defun pick-freevars-entries (freevars alist &optional entries)
  (if (null freevars)
      (nreverse entries)
      (let ((entry (assq (declaration (car freevars)) alist)))
	(pick-freevars-entries (cdr freevars) alist
			       (if entry
				   (cons (cdr entry) entries)
				   entries)))))


;(defmethod substit* :around (expr alist)
;  (if (null (freevars expr)) ;; (substit-possible? expr alist)
;      expr
;      (call-next-method)))

(defun substit-possible? (expr alist)
  (substit-possible*? (freevars expr) alist))

(defun substit-possible*? (freevars alist)
  (when freevars
    (or (assq (declaration (car freevars)) alist)
	(substit-possible*? (cdr freevars) alist))))

(defmethod substit* ((expr name-expr) alist)
  (with-slots (resolutions type actuals dactuals) expr
    (let* ((decl (declaration (car resolutions)))
	   (binding (assq decl alist)))
      (cond ((null binding)
	     (let* ((res (substit* resolutions alist))
		    (ntype (type (car res)))
		    (nexpr (lcopy expr
			     'type ntype
			     'actuals (substit* actuals alist)
			     'dactuals (substit* dactuals alist)
			     'resolutions res)))
	       #+pvsdebug
	       (assert (or (eq nexpr expr)
			   ;; nexpr and expr could be tc-eq, if constructor-name-exprs
			   (not (tc-eq (resolution nexpr) (resolution expr)))
			   (not (tc-eq (type (resolution nexpr))
				       (type (resolution expr))))))
	       nexpr))
	    ((typep (cdr binding) 'binding)
	     (if (eq (car binding) (cdr binding))
		 expr
		 (let ((nex (if (typep (cdr binding) 'field-decl)
				(change-class (copy (cdr binding))
				    'field-name-expr)
				(change-class (copy (cdr binding))
				    'name-expr))))
		   (setf (parens nex) 0)
		   (setf (resolutions nex)
			 (list (mk-resolution (cdr binding)
				 (current-theory-name)
				 (type (cdr binding)))))
		   #+pvsdebug
		   (assert (or (eq nex expr) (not (tc-eq nex expr))))
		   nex)))
	    (t (let ((nex (cdr binding)))
		 (if (strong-tc-eq nex expr)
		     expr
		     nex)))))))

(defmethod substit* ((expr adt-name-expr) alist)
  (let ((nex (call-next-method)))
    (if (eq nex expr)
	nex
	(copy nex 'adt-type (substit* (adt-type expr) alist)))))

(defmethod substit* ((expr projection-expr) alist)
  (with-slots (actuals dactuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'dactuals (substit* dactuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr injection-expr) alist)
  (with-slots (actuals dactuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'dactuals (substit* dactuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr injection?-expr) alist)
  (with-slots (actuals dactuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'dactuals (substit* dactuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr extraction-expr) alist)
  (with-slots (actuals dactuals type index) expr
    (lcopy expr
      'actuals (substit* actuals alist)
      'dactuals (substit* dactuals alist)
      'type (substit* type alist))))

(defmethod substit* ((expr projection-application) alist)
  (with-slots (argument actuals dactuals type index) expr
    (let ((narg (substit* argument alist)))
      (cond ((and (not *substit-dont-simplify*)
		  (tuple-expr? narg))
	     (nth (1- index) (exprs narg)))
	    ((eq argument narg)
	     expr)
	    (t (let ((ntype (substit* (type expr) alist)))
		 (lcopy expr
		   'argument narg
		   'actuals (substit* actuals alist)
		   'dactuals (substit* dactuals alist)
		   'type ntype)))))))

(defmethod substit* ((expr injection-application) alist)
  (with-slots (argument actuals dactuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'dactuals (substit* dactuals alist)
	'type ntype))))

(defmethod substit* ((expr injection?-application) alist)
  (with-slots (argument actuals dactuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'dactuals (substit* dactuals alist)
	'type ntype))))

(defmethod substit* ((expr extraction-application) alist)
  (with-slots (argument actuals dactuals type index) expr
    (let ((narg (substit* argument alist))
	  (ntype (substit* type alist)))
      (lcopy expr
	'argument narg
	'actuals (substit* actuals alist)
	'dactuals (substit* dactuals alist)
	'type ntype))))

(defmethod substit* ((expr field-application) alist)
  (with-slots (id argument type) expr
    (let ((narg (substit* argument alist)))
      (if (and (not *substit-dont-simplify*)
	       (record-expr? narg))
	  (let ((ass (find id (assignments narg)
			   :key #'(lambda (a) (id (caar (arguments a)))))))
	    (assert ass)
	    (expression ass))
	  (let ((ntype (substit* type alist)))
	    (lcopy expr 'argument narg 'type ntype))))))

(defmethod substit* ((res resolution) alist)
  (with-slots (module-instance declaration type) res
    (let* ((smodinst (substit* module-instance alist))
	   (stype (substit* type alist))
	   ;; Don't touch the declaration, even if a binding
	   (nres (if (and (eq smodinst module-instance)
			  (eq stype type))
		     res
		     (mk-resolution declaration smodinst stype))))
      #+pvsdebug
      (assert (or (eq res nres) (not (tc-eq type stype)) (not (tc-eq res nres))))
      nres)))

(defmethod substit* ((expr modname) alist)
  (lcopy expr
    'actuals (substit* (actuals expr) alist)
    'dactuals (substit* (dactuals expr) alist)))


(defmethod substit* ((act actual) alist)
  (with-slots (expr type-value) act
    (let ((ntype (when type-value
		   (substit* type-value alist))))
      (lcopy act
	'expr (cond ((and ntype (eq ntype type-value))
		     expr)
		    (type-value ntype)
		    (t (let ((nexpr (substit* expr alist)))
			 (pseudo-normalize nexpr))))
	'type-value ntype))))

;(defmethod substit* ( (expr if-expr) alist) ;;NSH(7-30)get rid
;  (copy expr
;	'arguments
;	(substit* (arguments expr) alist)))
;    'condition (substit*  (condition expr) alist)
;    'then-part (substit* (then-part expr) alist)
;    'else-part (substit*  (else-part expr) alist)))


(defmethod substit* ((expr application) alist)
  (with-slots (operator argument) expr
    (let ((op (substit* operator alist))
	  (arg (substit* argument alist)))
      (cond ((and (not *substit-dont-simplify*)
		  (lambda-expr? op)
		  (not (let-expr? expr)))
	     (let ((appl (make!-reduced-application* op arg)))
	       appl))
	    ((and (eq op operator)
		  (eq arg argument))
	     expr)
	    ((typep op '(or projection-expr injection-expr injection?-expr
			 extraction-expr))
	     (typecase op
	       (projection-expr
		(make!-projection-application (index op) arg (actuals op)))
	       (injection-expr
		(make!-injection-application
		 (index op) arg (range (type op)) (actuals op)))
	       (injection?-expr
		(make!-injection?-application (index op) arg (actuals op)))
	       (extraction-expr
		(make!-extraction-application (index op) arg (actuals op)))))
	    (t (let* ((stype (find-supertype (type op)))
		      (nex (simplify-or-copy-app
			    expr op arg
			    (if (typep (domain stype) 'dep-binding)
				(new-substit-hash
				 (substit* (range stype)
					   (acons (domain stype)
						  arg nil)))
				(range stype)))))
		 ;; Note: the copy :around (application) method takes care of
		 ;; changing the class if it is needed.
		 (if (strong-tc-eq nex expr)
		     expr
		     nex)))))))

(defmethod substit* :around ((expr let-expr) alist)
  (declare (ignore alist))
  (let ((*let-operators* (cons (operator expr) *let-operators*)))
    (call-next-method)))

(defmethod make!-reduced-application* ((op lambda-expr) (arg tuple-expr))
  (if (singleton? (bindings op))
      (call-next-method)
      (new-substit-hash
       (substit* (expression op)
		 (pairlis (bindings op) (exprs arg))))))

(defmethod make!-reduced-application* ((op lambda-expr) arg)
  (new-substit-hash
   (if (singleton? (bindings op))
       (substit* (expression op)
		 (acons (car (bindings op)) arg nil))
       (substit* (expression op)
		 (pairlis (bindings op) (make!-projections arg))))))

(defmethod substit* ((expr equation) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let* ((result (call-next-method)))
	(if (equation? result)
	    (let* ((arg1 (args1 result))
		   (arg2 (args2 result)))
	      (if (tc-eq arg1 arg2)
		  *true*
		  (if (iff-or-boolean-equation? result)
		      (if (tc-eq arg1 *true*)
			  arg2
			  (if (tc-eq arg2 *true*)
			      arg1
			      result))
		      result)))
	    result))))


(defmethod substit* ((expr conjunction) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (conjunction? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *true*)
		  arg2
		  (if (tc-eq arg2 *true*)
		      arg1
		      (if (or (tc-eq arg1 *false*)
			      (tc-eq arg2 *false*))
			  *false*
			  result))))
	    result))))

(defmethod substit* ((expr disjunction) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (disjunction? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *false*)
		  arg2
		  (if (tc-eq arg2 *false*)
		      arg1
		      (if (or (tc-eq arg1 *true*)
			      (tc-eq arg2 *true*))
			  *true*
			  result))))
	    result))))

(defmethod substit* ((expr implication) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (implication? result)
	    (let ((arg1 (args1 result))
		  (arg2 (args2 result)))
	      (if (tc-eq arg1 *true*)
		  arg2
		  (if (or (tc-eq arg1 *false*)
			  (tc-eq arg2 *true*))
		      *true*
		      (if (tc-eq arg2 *false*)
			  (negate! arg1)
			  result))))
	    result))))

(defmethod substit* ((expr negation) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (negation? result)
	    (let ((arg (argument result)))
	      (if (tc-eq arg *true*)
		  *false*
		  (if (tc-eq arg *false*)
		      *true*
		      result)))
	    result))))

(defmethod substit* ((expr branch) alist)
  (declare (ignore alist))
  (if *substit-dont-simplify*
      (call-next-method)
      (let ((result (call-next-method)))
	(if (branch? result)
	    (let ((cond (condition result))
		  (then (then-part result))
		  (else (else-part result)))
	      (if (or (tc-eq cond *true*)
		      (tc-eq then else))
		  then
		  (if (tc-eq cond *false*)
		      else
		      result)))
	    result))))

(defmethod change-to-infix-appl? ((expr infix-application))
  nil)

(defmethod change-to-infix-appl? ((expr application))
  (with-slots (operator argument) expr
    (and (infix-op? operator)
	 (= (the fixnum (arg-length argument)) 2))))

(defmethod infix-op? (expr)
  (declare (ignore expr))
  nil)

(defmethod infix-op? ((expr name-expr))
  (with-slots (id) expr
    (memq id *infix-operators*)))

(defmethod arg-length (expr)
  (declare (ignore expr))
  1)

(defmethod arg-length ((expr tuple-expr))
  (with-slots (exprs) expr
    (length exprs)))

(defmethod substit* :around ((expr table-expr) alist)
  (let ((nexpr (call-next-method)))
    (if (eq expr nexpr)
	expr
	(if (table-expr? nexpr)
	    (lcopy nexpr
	      'row-expr (substit* (row-expr nexpr) alist)
	      'col-expr (substit* (col-expr nexpr) alist)
	      'row-headings (substit* (row-headings nexpr) alist)
	      'col-headings (substit* (col-headings nexpr) alist)
	      'table-entries (substit* (table-entries nexpr) alist))
	    nexpr))))

(defmethod substit* ((expr record-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr tuple-expr) alist)
  (let ((nexprs (substit*-simple-list (exprs expr) alist)))
    (if (eq nexprs (exprs expr))
	expr
	(let ((ntype (if (every #'(lambda (nex ex) (eq (type nex) (type ex)))
				nexprs (exprs expr))
			 (type expr)
			 (mk-tupletype (mapcar #'type nexprs)))))
	  (copy expr
	    'exprs nexprs
	    'type ntype)))))

(defun substit*-simple-list (list alist)
  (let ((slist (substit*-simple-list* list alist nil)))
    (if (equal list slist) list slist)))

(defun substit*-simple-list* (list alist result)
  (if (null list)
      (nreverse result)
      (substit*-simple-list* (cdr list) alist
			     (cons (substit* (car list) alist) result))))

(defmethod substit* ((expr update-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'expression (substit* (expression expr) alist)
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr assignment) alist)
  (lcopy expr
    'arguments (substit* (arguments expr) alist)
    'expression (substit* (expression expr) alist)))

(defmethod substit* ((expr list) alist)
  (let ((nlist (substit*-list expr alist nil)))
    (if (equal nlist expr) expr nlist)))

(defun substit*-list (expr alist result)
  (cond ((null expr)
	 (nreverse result))
	((binding? (car expr))
	 (let ((newcar (substit-binding* (car expr) alist)))
	   (cond ((not (listp (cdr expr)))
		  (assert (null result))
		  (cons newcar (substit* (cdr expr) alist)))
		 ((eq newcar (car expr))
		  (substit*-list (cdr expr) alist (cons newcar result)))
		 ((null (cdr expr))
		  (nreverse (cons newcar result)))
		 (t (add-substit-hash
		     (substit*-list (cdr expr)
				    (acons (car expr) newcar alist)
				    (cons newcar result)))))))
	((listp (cdr expr))
	 (substit*-list (cdr expr) alist
			(cons (substit* (car expr) alist)
			      result)))
	(t (assert (null result))
	   (cons (substit* (car expr) alist) (substit* (cdr expr) alist)))))

(defun substit-binding (expr alist)
  (new-substit-hash
   (substit-binding* expr alist)))

(defun substit-binding* (expr alist)
  (let* ((newtype (substit* (type expr) alist))
	 (newdtype (if (tc-eq (print-type (type expr))
			      (declared-type expr))
		       (if (eq (print-type (type expr)) (print-type newtype))
			   (declared-type expr)
			   (print-type newtype))
		       (substit* (declared-type expr) alist)))
	 (nbind (lcopy expr
		  'type newtype
		  'declared-type newdtype)))
    #+pvsdebug
    (assert (or (eq nbind expr) (not (tc-eq nbind expr))))
    nbind))

(defmethod substit* ((expr binding-expr) alist)
  (if (not (substit-possible? expr alist))
      expr
      (let* ((*leave-bindings-undeclared* (memq expr *let-operators*))
	     (new-bindings-i (make-new-bindings-internal
			      (bindings expr) alist (expression expr)))
	     (new-bindings (if (equal new-bindings-i (bindings expr))
			       (bindings expr)
			       new-bindings-i))
	     (*bound-variables* (append new-bindings *bound-variables*))
	     (nalist (substit-pairlis (bindings expr) new-bindings alist))
	     (nexpr (add-substit-hash
		     (substit* (expression expr) nalist)))
	     (ntype (if (or (quant-expr? expr)
			    (and (equal new-bindings (bindings expr))
				 (eq nexpr (expression expr))))
			(type expr)
			(make-formals-funtype (list new-bindings)
					      (type nexpr))))
	     (nrettype (when (lambda-expr-with-type? expr)
			 (substit* (return-type expr) nalist)))
	     (ndrettype (when (lambda-expr-with-type? expr)
			  (substit* (declared-ret-type expr) alist)))
	     (nbexpr (if (lambda-expr-with-type? expr)
			 (lcopy expr
			   'bindings new-bindings
			   'type ntype
			   'expression nexpr
			   'parens 0
			   'return-type nrettype
			   'declared-ret-type ndrettype)
			 (lcopy expr
			   'bindings new-bindings
			   'type ntype
			   'expression nexpr
			   'parens 0))))
	(if (strong-tc-eq expr nbexpr)
	    expr
	    nbexpr))))

(defun substit-pairlis (bindings new-bindings alist)
  (if (null bindings)
      alist
      (substit-pairlis (cdr bindings) (cdr new-bindings)
		       (if (eq (car bindings) (car new-bindings))
			   alist
			   (acons (car bindings) (car new-bindings) alist)))))

;; Creates new bindings from old.

(defun make-new-bindings (old-bindings alist expr)
  (new-substit-hash
   (make-new-bindings* old-bindings alist expr)))

;; Uses existing substit-hash
(defun make-new-bindings-internal (old-bindings alist expr)
  (let ((nbindings (make-new-bindings* old-bindings alist expr)))
    (if (equal nbindings old-bindings)
	old-bindings
	nbindings)))

;; freevars are the free variables in the alist RHSs.
;; boundvars are the bindings found walking down the alist RHSs, excluding subtypes

(defun make-new-bindings* (old-bindings alist expr &optional nbindings)
  (if (null old-bindings)
      (nreverse nbindings)
      (let* ((bind (car old-bindings))
	     (btype (type bind))
	     (check (bindings-subst-clash bind expr alist))
	     (stype (substit* btype alist))
	     (dec-type (declared-type bind))
	     (new-binding
	      (if (not check)
		  (lcopy bind
		    'type stype
		    'declared-type (substit* dec-type alist))
		  (copy bind
		    'id (new-boundvar-id (id bind) expr)
		    'type stype
		    'declared-type (substit* dec-type alist)))))
	(unless (or *leave-bindings-undeclared*
		    (eq bind new-binding)
		    (declared-type new-binding))
	  (setf (declared-type new-binding) (or (print-type stype) stype)))
	(make-new-bindings*
	 (cdr old-bindings)
	 (acons bind new-binding alist)
	 (list new-binding expr)
	 (cons new-binding nbindings)))))

(defun bindings-subst-clash (bind expr alist)
  "Check if =bind= will clash with alist substition.  It clashes if there is
a free variable in =expr=, which is the car of an elt of =alist= (so it will
be substituted in =expr=), and the id of =bind= occurs unbound in the cdr of
that elt."
  (or (some #'(lambda (fv)
		(let ((abd (assq (declaration fv) alist)))
		  (and abd (var-occurs-in (id bind) (cdr abd)))))
	    (freevars expr))
      (member (id bind) (alist-boundvars alist) :key #'id)))

(defun alist-boundvars (alist)
  (if (eq *alist-boundvars* 'unbound)
      (let ((bvars nil))
	(dolist (acons alist)
	  (mapobject #'(lambda (ex)
			 (or (subtype? ex)
			     (when (binding-expr? ex)
			       (dolist (bd (bindings ex)) (pushnew bd bvars))
			       nil)))
		     (cdr acons)))
	(setq *alist-boundvars* bvars))
      *alist-boundvars*))

(defmethod substit* ((expr cases-expr) alist)
  (let* ((ntype (substit* (type expr) alist))
	 (nexpr (substit* (expression expr) alist)))
    (if (or *gensubst-subst-types*
	    (eq nexpr (expression expr))
	    (compatible? (type nexpr) (type (expression expr))))
	(let* ((nsels (mapcar #'(lambda (s) (substit* s alist))
		       (selections expr)))
	       (nex (lcopy expr
		      'expression nexpr
		      'selections (if (equal nsels (selections expr))
				      (selections expr)
				      nsels)
		      'else-part (substit* (else-part expr) alist)
		      'type ntype)))
	  #+pvsdebug
	  (assert (or (eq nex expr) (not (tc-eq nex expr))))
	  nex)
	(substit* (translate-cases-to-if expr) alist))))

(defmethod substit* ((expr selection) alist)
  (if (freevars expr)
      (let* ((new-bindings (make-new-bindings-internal
			    (args expr) alist (expression expr)))
	     (nexpr (add-substit-hash
		     (substit* (expression expr)
			       (nconc (pairlis (args expr)
					       new-bindings)
				      alist))))
	     (nsel (lcopy expr
		     'constructor (substit* (constructor expr) alist)
		     'args (if (eq (expression expr) nexpr)
			       (args expr)
			       new-bindings)
		     'expression nexpr)))
	#+pvsdebug
	(assert (or (eq nsel expr) (not (tc-eq nsel expr))))
	nsel)
      expr))

;(defmethod substit* ((expr coercion) alist)
;  (lcopy expr
;    'expression (substit* (expression expr) alist)
;    'declared-type (substit* (declared-type expr) alist)
;    'type (substit* (type expr) alist)))
  


;;2-21-91: remaining exprs are treated as unsubstitutable.
(defmethod substit* ((expr expr) alist)
  (declare (ignore alist))
  expr)

;;NSH:8-21-91: substit* for type-expressions.
(defmethod substit* ((texpr type-name) alist)
  (let ((nacts (substit* (actuals texpr) alist))
	(dacts (substit* (dactuals texpr) alist))
	(ptype (substit* (print-type texpr) alist))
	(mi (substit* (module-instance (resolution texpr)) alist)))
    (if (and (eq nacts (actuals texpr))
	     (eq dacts (dactuals texpr))
	     (eq ptype (print-type texpr))
	     (eq mi (module-instance (resolution texpr))))
	texpr
	(let ((nte (copy texpr
		     'actuals nacts
		     'dactuals dacts
		     'print-type ptype)))
	  (setf (get-substit-hash texpr) nte)
	  (setf (resolutions nte)
		(list
		 (if (eq texpr (type (resolution texpr)))
		     (mk-resolution (declaration (resolution texpr))
		       mi
		       nte)
		     (mk-resolution (declaration (resolution texpr))
		       mi
		       (substit* (type (resolution texpr)) alist)))))
	  nte))))

(defmethod substit* ((te print-type-name) alist)
  (assert (resolution te))
  (let ((mi (substit* (module-instance te) alist)))
    (if (eq mi (module-instance te))
	te
	(copy te
	  :actuals (when (actuals te) (actuals mi))
	  :dactuals (when (dactuals te) (dactuals mi))
	  :resolutions (list (mk-resolution (declaration te) mi nil))))))

(defmethod substit* ((texpr subtype) alist)
  (with-slots (supertype predicate print-type) texpr
    (let ((npred (substit* predicate alist)))
      (if (eq npred predicate)
	  texpr
	  (let* ((spred (pseudo-normalize npred))
		 (stype (domain (find-supertype (type spred))))
		 (subst-ptype (substit* print-type alist))
		 (cand-ptype (when subst-ptype
			       (or (print-type subst-ptype) subst-ptype)))
		 (ptype (when (typep cand-ptype
				     '(or null type-name expr-as-type
				       type-application))
			  (if (or (null cand-ptype)
				  (print-type-expr? cand-ptype))
			      cand-ptype
			      (change-class cand-ptype
				  (typecase cand-ptype
				    (type-name 'print-type-name)
				    (type-application 'print-type-application)
				    (expr-as-type 'print-expr-as-type))))))
		 (ntexpr (lcopy texpr
			   'supertype stype
			   'predicate spred
			   'print-type ptype)))
	    #+pvsdebug
	    (assert (or (eq texpr ntexpr) (not (strong-tc-eq texpr ntexpr))))
	    ntexpr)))))

(defmethod substit* ((texpr setsubtype) alist)
  (declare (ignore alist))
  (let ((nexpr (call-next-method)))
    (unless (or (not (typep nexpr 'setsubtype))
		(eq (predicate texpr) (predicate nexpr)))
      ;;(assert (lambda-expr? (predicate nexpr)))
      (setf (formals nexpr) (bindings (predicate nexpr)))
      (setf (formula nexpr) (expression (predicate nexpr))))
    nexpr))

(defmethod substit* ((texpr expr-as-type) alist)
  (let* ((sexpr (substit* (expr texpr) alist))
	 (bexpr (beta-reduce sexpr)))
    (if (everywhere-true? bexpr)
	(let* ((dty (domain (type bexpr)))
	       (pty (or (print-type dty) dty)))
	  pty)
	(lcopy texpr :expr bexpr))))

(defmethod substit* ((texpr datatype-subtype) alist)
  (lcopy (call-next-method)
    'declared-type (substit* (declared-type texpr) alist)))

(defmethod substit* ((texpr funtype) alist)
  (let* ((typelist (list (domain texpr) (range texpr)))
	 (ntypelist (substit* typelist alist))
	 (nptype (substit* (print-type texpr) alist))
	 (nftype (lcopy texpr
		   'domain (car ntypelist)
		   'range (cadr ntypelist)
		   'print-type nptype)))
    #+pvsdebug
    (assert (or (eq texpr nftype) (not (tc-eq texpr nftype))))
    nftype))

(defmethod substit* ((texpr tupletype) alist)
  (lcopy texpr
    'types (substit* (types texpr) alist)
    'print-type (substit* (print-type texpr) alist)))

(defmethod substit* ((texpr cotupletype) alist)
  (lcopy texpr
    'types (substit* (types texpr) alist)
    'print-type (substit* (print-type texpr) alist)))

(defmethod substit* ((te recordtype) alist)
  (let* ((fields (substit* (fields te) alist))
	 (sfields (sort-fields fields (dependent-fields? fields)))
	 (ptype (substit* (print-type te) alist))
	 (nte (lcopy te
		'fields (if (equal sfields fields) fields sfields)
		'print-type ptype)))
    #+pvsdebug
    (assert (or (eq te nte) (not (strong-tc-eq te nte))))
    nte))

(defmethod substit* ((te type-application) alist)
  (lcopy te
    'type (substit* (type te) alist)
    'parameters (let* ((nparms (substit* (parameters te) alist)))
		  (if (equal nparms (parameters te))
		      (parameters te)
		      (mapcar #'pseudo-normalize nparms)))
    'print-type (substit* (print-type te) alist)))

(defmethod substit* ((fd field-decl) alist)
  (let ((ntype (substit* (type fd) alist))
	(dtype (substit* (declared-type fd) alist)))
    (lcopy fd 'type ntype 'declared-type dtype)))

(defmethod substit* ((db dep-binding) alist)
  (let ((ntype (substit* (type db) alist))
	(ndtype (substit* (declared-type db) alist)))
    (lcopy db 'type ntype 'declared-type ndtype)))

(defmethod substit* ((bd binding) alist)
  (let ((ntype (substit* (type bd) alist))
	(ndtype (substit* (declared-type bd) alist)))
    (lcopy bd 'type ntype 'declared-type ndtype)))

(defmethod substit* ((seq vector) alist)
  (let* ((list (coerce seq 'list))
	 (slist (substit* list alist)))
    (if (equal list slist)
	seq
	(coerce slist 'vector))))

(defmethod substit* ((sym symbol) alist)
  (declare (ignore alist))
  sym)

;; (defun pseudo-normalize* (expr)
;;   (gensubst expr #'pseudo-normalize! #'pseudo-normalize?))

;; (defmethod pseudo-normalize? (ex)
;;   (declare (ignore ex))
;;   nil)

;; (defmethod pseudo-normalize? ((ty subtype))
;;   t)

;; (defmethod pseudo-normalize? ((act actual))
;;   (not (type-value act)))

;; (defmethod pseudo-normalize! ((ty subtype))
;;   (lcopy ty 'predicate (pseudo-normalize (predicate ty))))

;; (defmethod pseudo-normalize! ((act actual))
;;   (lcopy act 'expr (pseudo-normalize (expr act))))
