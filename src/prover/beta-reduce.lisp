;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; beta-reduce.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Oct 31 01:09:32 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 01:09:48 1998
;; Update Count    : 1
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

(export '(beta-reduce))

;;; Moved from proofrules.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;beta reduction

;(defun beta-rule (sformnums)
;  (make-instance 'rule
;		 'rule-part (beta-step sformnums)
;		 'rule-input `(beta ,sformnums)))

 

(defun beta-sform (sform &optional rewrite-flag (let-reduce? t))
  (let* ((fmla (formula sform))
	 (newfmla (if (memq rewrite-flag '(lr rl))
		      (if (and (negation? fmla)
			       (equation? (args1 fmla)))
			  (lcopy fmla
			    'argument
			    (lcopy (args1 fmla)
			      'argument
			      (make!-arg-tuple-expr*
			       (if (eq rewrite-flag 'rl)
				   (list (beta-reduce (args1 (args1 fmla))
						      let-reduce?)
					 (args2 (args1 fmla)))
				   (list (args1 (args1 fmla))
					 (beta-reduce (args2 (args1 fmla))
						      let-reduce?))))))
			  fmla)
		      (beta-reduce fmla let-reduce?)))
	 (new-sform (if (or (tc-eq fmla newfmla)
			    (tc-eq newfmla *false*) ;;NSH(4.26.95)
			    (and (negation? newfmla)
				 (tc-eq (args1 newfmla) *true*)))
			    sform
			(lcopy sform 'formula newfmla))))
    (if (s-form-equal? sform new-sform) (values 'X sform)
	(values '? new-sform))))

(defun beta-step (sformnums &optional rewrite-flag (let-reduce? t))
  #'(lambda (ps)
      (multiple-value-bind
	  (signal subgoal)
	  (sequent-reduce (current-goal ps)
			  #'(lambda (x) (beta-sform x rewrite-flag let-reduce?))
			  sformnums)
	(when (not (eq signal '?))
	  ;;	    (format-if "~%Beta reducing the formulas ~a," sformnums)
	  (error-format-if "~%No suitable redexes found."))
	(values signal (list subgoal) ))))


;;; Beta-reduce methods

(defvar *beta-cache* (make-hash-table :test #'eq :size 3250))

(defvar *let-reduce-beta-cache* (make-hash-table :test #'eq :size 3250))

(defun reset-beta-cache ()
  (clrhash *beta-cache*)
  (clrhash *let-reduce-beta-cache*))

(defun beta-reduce (obj &optional (let-reduce? t))
  (let ((*beta-cache* (if let-reduce?
			  *beta-cache*
			  *let-reduce-beta-cache*))
	(*let-reduce?* let-reduce?))
    (when (and *in-checker*
	       (> (hash-table-count *beta-cache*) 5000))
      (clrhash *beta-cache*))
    (beta-reduce* obj)))

(defmethod beta-reduce* :around (obj)
  (or (gethash obj *beta-cache*)
      (let ((nobj (call-next-method)))
	(setf (gethash obj *beta-cache*) nobj)
	nobj)))

(defmethod beta-reduce* ((te type-name))
  te)

(defmethod beta-reduce* ((te subtype))
  (lcopy te 'supertype (beta-reduce* (supertype te))
	 'predicate (beta-reduce* (predicate te))))

(defmethod beta-reduce* ((te funtype))
  (lcopy te
    'domain (beta-reduce* (domain te))
    'range (beta-reduce* (range te))))

(defmethod beta-reduce* ((te tupletype))
  (lcopy te 'types (beta-reduce* (types te))))

(defmethod beta-reduce* ((te cotupletype))
  (lcopy te 'types (beta-reduce* (types te))))

(defmethod beta-reduce* ((te recordtype))
  (let ((fields (beta-reduce* (fields te))))
    (if (eq fields (fields te))
	te
	(copy te
	  'fields (sort-fields fields
			       (dependent-fields? fields))))))

(defmethod beta-reduce* ((fd field-decl))
  (let ((te (beta-reduce* (type fd))))
    (if (eq te (type fd))
	fd
	(lcopy fd 'declared-type te 'type te))))

(defmethod beta-reduce* ((te dep-binding))
  (lcopy te 'type (beta-reduce* (type te))))

(defmethod beta-reduce* :around ((expr expr))
  (let ((nexpr (call-next-method)))
    (if (typep expr '(or bind-decl number-expr))
	nexpr
	(lcopy nexpr 'type (beta-reduce* (type nexpr))))))

(defmethod beta-reduce* ((expr expr))
  expr)

(defmethod beta-reduce* ((expr cases-expr))
  (let* ((expression (beta-reduce* (expression expr)))
	 (selections (beta-reduce* (selections expr)))
	 (else-part (beta-reduce* (else-part expr)))
	 (expr (lcopy expr
		 'expression expression
		 'selections selections
		 'else-part else-part)))
    (cond ((and (typep expression 'application)
		(typep (operator expression) 'name-expr)
		(constructor? (operator expression)))
	   (beta-reduce* (find-selection (id (operator expression))
			   (arguments expression)
			   expr)))
	  ((and (typep expression 'name-expr)
		(constructor? expression))
	   (beta-reduce* (find-selection (id expression) nil expr)))
	  ((typep expression 'injection-application)
	   (beta-reduce* (find-selection (id expression) (arguments expression)
					 expr)))
	  (t expr))))

(defmethod beta-reduce* ((expr selection))
  (lcopy expr
    'expression (beta-reduce* (expression expr))))

(defmethod beta-reduce* ((expr binding-expr))
  (let ((nbindings (beta-reduce* (bindings expr))))
    (lcopy expr
	   'expression (beta-reduce*
			(if (eq nbindings (bindings expr))
			    (expression expr)
			    (substit (expression expr)
				     (mapcan #'(lambda (x y)
						 (unless (eq x y)
						   (list (cons x y))))
					     (bindings expr) nbindings))))
	   'bindings nbindings)))

(defmethod beta-reduce* ((expr symbol))
  expr)

(defmethod beta-reduce* :around ((expr table-expr))
  (let ((nexpr (call-next-method)))
    (if (and (not (eq nexpr expr))
	     (table-expr? nexpr))
	(lcopy nexpr
	  'row-expr (beta-reduce* (row-expr nexpr))
	  'col-expr (beta-reduce* (col-expr nexpr))
	  'row-headings (beta-reduce* (row-headings nexpr))
	  'col-headings (beta-reduce* (col-headings nexpr))
	  'table-entries (beta-reduce* (table-entries nexpr)))
	nexpr)))

;(defmethod beta-reduce* ((bd bind-decl))
;  (let ((te (beta-reduce* (type bd))))
;    (if (eq te (type bd))
;	bd
;	(let ((nbd (copy bd 'declared-type te 'type te)))
;	  (setf (resolutions nbd)
;		(list (make-resolution nbd 
;			(module-instance nbd)
;			te)))
;	  nbd))))

(defmethod beta-reduce* ((bd bind-decl))
  (let ((te (beta-reduce* (type bd))))
    (if (eq te (type bd))
	bd
	(let ((nbd (copy bd 'declared-type te 'type te)))
	  (setf (resolutions nbd)
		(list (make-resolution nbd 
			(module-instance nbd)
			te)))
	  nbd))))

(defmethod beta-reduce* ((expr record-expr))
  (lcopy expr
	'assignments (beta-reduce* (assignments expr))))

(defmethod beta-reduce* ((expr assignment))
  (lcopy expr
    ;; Do *NOT* change this to argument - it is *NOT* an application!!
    'arguments (beta-reduce* (arguments expr))
    'expression  (beta-reduce* (expression expr))))

(defmethod beta-reduce* ((expr tuple-expr))
  (let ((bexprs (beta-reduce* (exprs expr))))
    (if (eq bexprs (exprs expr))
	expr
	(lcopy expr 
	  'exprs bexprs
	  'type (mk-tupletype (mapcar #'type bexprs))))))

(defmethod beta-reduce* ((expr update-expr))
  (lcopy expr
	'expression (beta-reduce* (expression expr))
	'assignments (beta-reduce* (assignments expr))))

(defun pairlis-args (formals actuals)
  (cond ((eql (length formals)(length actuals))
	 (pairlis-args* formals actuals))
	((and (singleton? actuals)
	      (typep (find-supertype (type (car actuals))) 'tupletype))
	 (pairlis-args* formals (make!-projections (car actuals))))
	((and (singleton? formals)
	      (typep (find-supertype (type (car formals))) 'tupletype))
	 (list (cons (car formals)
		     (make!-tuple-expr actuals))))
	(t (break "~%pairlis-args invoked with unsuitable formals/actuals."))))

(defun pairlis-args* (formals actuals &optional result)
  (if (null formals)
      (nreverse result)
      (pairlis-args* (cdr formals) (cdr actuals)
		     (if (or (eq (car formals) (car actuals))
			     (and (name-expr? (car actuals))
				  (eq (car formals)
				      (declaration (car actuals)))))
			 result
			 (acons (car formals) (car actuals) result)))))
  

;;; SO 8/31/94 - added following two methods, extracted from application
;;; method

(defun beta-reduce-tuple-update-redex (index arg)
  (let ((updates (loop for assn in (assignments arg)
		 when (eql index (number (caar (arguments assn))))
		 collect assn))
	(expr-of-arg (expression arg)))
    (if updates
	(if (every #'(lambda (x) (cdr (arguments x)))
		     updates) ;;;a curried update::
	                             ;;;PROJ_n(exp WITH [(n)(i):= e])
	    (let ((newexpr (make!-update-expr
			    (make-tuple-update-reduced-projection
			     index expr-of-arg)
			    (mapcar #'(lambda (x)
				    (lcopy x 'arguments
					   (cdr (arguments x))))
			      updates)
			    ;;(type (make!-projection-application index arg))
			    )))
	      newexpr)
	    (make-beta-update-expr-from-updates updates))
	(make-tuple-update-reduced-projection
	 index expr-of-arg))))

(defmethod beta-reduce* ((expr projection-application))
  (with-slots (index argument) expr
    (let ((narg (beta-reduce* argument)))
      (if (typep narg 'tuple-expr)
	  (nth (1- index) (exprs narg))
	  (if (typep narg 'update-expr)
	      (beta-reduce-tuple-update-redex index narg)
	  (lcopy expr 'argument narg))))))

(defmethod beta-reduce* ((expr injection-application))
  (with-slots (index argument) expr
    (let ((narg (beta-reduce* argument)))
      (if (and (extraction-application? narg)
	       (= (index expr) (index narg)))
	  (argument narg)
	  (lcopy expr 'argument narg)))))

(defmethod beta-reduce* ((expr injection?-application))
  (with-slots (index argument) expr
    (let ((narg (beta-reduce* argument)))
      (lcopy expr 'argument narg))))

(defmethod beta-reduce* ((expr extraction-application))
  (with-slots (index argument) expr
    (let ((narg (beta-reduce* argument)))
      (if (and (injection-application? narg)
	       (= (index expr) (index narg)))
	  (argument narg)
	  (lcopy expr 'argument narg)))))

(defun beta-reduce-record-update-redex (op arg)
  (let ((updates
	 (loop for assn in (assignments arg)
	       when (eq op (id (caar (arguments assn))))
	       collect assn))
	(expr-of-arg (expression arg)))
    (if updates
	(if (every #'(lambda (x) (cdr (arguments x)))
		   updates) ;;;a curried update::
	                             ;;;a(exp WITH [((a)(i)):= e])
	    (let ((newexpr;;NSH(9.15.94): otherwise TCCs
		   ;;are generated when domain is subtype.
		   ;;(let ((*generate-tccs* 'none)))
		   (make!-update-expr
		    (make-record-update-reduced-application
		     op expr-of-arg)
		    (mapcar #'(lambda (x)
				(lcopy x 'arguments
				       (cdr (arguments x))))
		      updates))))
	       newexpr)
	    (make-beta-update-expr-from-updates
	     updates))
	(make-record-update-reduced-application
			   op expr-of-arg))))

(defun make-beta-update-expr-from-updates (updates)
    (if (and (consp updates)
	   (null (cdr (arguments (car updates))))
	   (every #'(lambda (x) (cdr (arguments x)))
		  (cdr updates)))
      (if (null (cdr updates))
	  (expression (car updates))
	  (make!-update-expr
	    (expression (car updates))
	    (mapcar #'(lambda (x) (lcopy x 'arguments
					 (cdr (arguments x))))
	      (cdr updates))))
      (make-beta-update-expr-from-updates (cdr updates))))

(defmethod beta-reduce* ((expr field-application))
  (with-slots (id argument) expr
    (let* ((arg (beta-reduce* argument))
	   (expr (lcopy expr 'argument arg)))
      (cond ((record-redex? expr)
	     (beta-reduce*
	      (expression
	       (find id (assignments arg)
		     :test #'(lambda (x y)
			       (eq x (id (caar (arguments y)))))))))
	    ((record-update-redex? expr)
	     (beta-reduce-record-update-redex id arg))
	    (t (lcopy expr 'argument arg))))))

;;NSH(10.27.94): needed to avoid betareducing lets in TCCs.
(defmethod beta-reduce* ((expr let-expr))
  (if *let-reduce?*
      (call-next-method)
      (lcopy expr
	'operator (beta-reduce* (operator expr))
	'argument (beta-reduce* (argument expr)))))

(defmethod beta-reduce* ((expr application))
  (let* ((oper (if (lambda? (operator expr))
		   (operator expr);;since it will be reduced anyway.
		   (beta-reduce* (operator expr))))
	 (arg (beta-reduce* (argument expr)))
	 (newexpr (lcopy expr
		    'operator oper
		    'argument  arg)))
    (cond ((and (is-predicate? oper)
		(typep (type arg) 'subtype))
	   (cond ((member expr (type-constraints arg t)
			  :test #'tc-eq)
		  *true*)
		 ((and (adt? (supertype (type arg)))
		       (recognizer? oper)
		       (recognizer? (predicate (type arg))))
		  (if (tc-eq oper (predicate (type arg)))
		      *true*
		      *false*))
		 ((lambda? oper)
		  (beta-reduce* (substit (expression oper)
				  (pairlis-args (bindings oper)
						(arguments newexpr)))))
		 (t newexpr)))
	  ((lambda? oper)
	   (beta-reduce* (substit (expression oper)
			   (pairlis-args (bindings oper)
					 (arguments newexpr)))))
	  ((function-update-redex? newexpr)
	   (simplify-function-update-redex newexpr t))
	  ((accessor-update-redex? newexpr)
	   (beta-reduce-accessor-update-redex newexpr))
	  ((and (typep oper 'name-expr)
		(accessor? oper)
		(typep arg 'application)
		(typep (operator arg) 'name-expr)
		(member (operator arg) (constructor oper) :test #'same-id))
	   (let ((accessors (accessors (operator arg))))
	     (if (cdr accessors)
		 (let ((args (arguments arg))
		       (pos (position oper accessors :test #'same-id)))
		   (if (cdr args)
		       (nth pos args)
		       (make!-projection-application (1+ pos) (car args))))
		 (argument arg))))
	  (t (if (lambda? oper)
		 (beta-reduce* (substit (expression oper)
				 (pairlis-args (bindings oper)
					       (arguments newexpr))))
		 newexpr)))))

(defun beta-reduce-accessor-update-redex (expr)
  (let* ((op (operator expr))
	 (arg (argument expr))
	 (updates
	  (loop for assn in (assignments arg)
		when (eq (id op) (id (caar (arguments assn))))
		collect assn))
	 (expr-of-arg (expression arg)))
    (if updates
	(if (every #'(lambda (x) (cdr (arguments x)))
		   updates) ;;;a curried update::
	                             ;;;a(exp WITH [((a)(i)):= e])
	    (let ((newexpr;;NSH(9.15.94): otherwise TCCs
		   ;;are generated when domain is subtype.
		   ;;(let ((*generate-tccs* 'none)))
		   (make!-update-expr
		    (make-accessor-update-reduced-application
		     op expr-of-arg)
		    (mapcar #'(lambda (x)
				(lcopy x 'arguments
				       (cdr (arguments x))))
		      updates))))
	       newexpr)
	    (make-beta-update-expr-from-updates
	     updates))
	(make-accessor-update-reduced-application op expr-of-arg))))

(defmethod beta-reduce* ((list list))
  (let ((nlist (beta-reduce*-list list nil)))
    (if (equal nlist list) list nlist)))

(defun beta-reduce*-list (list result)
  (if (null list)
      (nreverse result)
      (let ((nelt (beta-reduce* (car list))))
	(if (or (eq nelt (car list))
		(not (typep (car list) 'binding)))
	    (beta-reduce*-list (cdr list) (cons nelt result))
	    (beta-reduce*-list (substit (cdr list)
				 (acons (car list) nelt nil))
			       (cons nelt result))))))

;(defmethod beta-reduce* ((expr coercion))
;  (lcopy  expr
;      'expression (beta-reduce* (expression expr))))
