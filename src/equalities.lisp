;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalities.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 00:37:50 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr  5 00:03:50 1998
;; Update Count    : 77
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This file provides the various equalities needed by the type system.
;;;  ps-eq is used to check for syntactic equality - useful when the
;;;        expressions have not yet been typechecked
;;;  tc-eq checks for equality, handling dependent bindings and alpha
;;;        equivalence
;;;  compatible? is a quick check whether two types are compatible
;;;  compatible-type is the least common supertype for compatible? types
;;;  compatible-preds is the list of predicates needed to coerce an
;;;                   element of the first type to the second type

(in-package 'pvs)

(defvar *dep-bindings* nil
  "An alist of dep-bindings: each elt is of the form (db db1 db2),
where db is to replace db1 and db2")

;;; ps-eq is used to determine whether two entities are syntactically
;;; equal; the types and resolutions are ignored.

(defun ps-eq (x y)
  (string= (unparse x :string t :char-width most-positive-fixnum)
	   (unparse y :string t :char-width most-positive-fixnum)))

;;; tc-eq determines when two entities are "the same".  This is defined
;;; only when both entities have been typechecked; for types this means
;;; that they are in canonical form.  Expressions must have a type, and
;;; names must have a resolution.  A new invariant has been added that
;;; needs to be maintained: if two expressions are tc-eq, then one must be
;;; substitutable for the other in any context.  This was violated in the
;;; past; e.g., tuple-exprs could be tc-eq to lists.

;(defun tc-eq (x y)
;  (or (eq x y)
;      (let ((pair (cons x y)))
;	(multiple-value-bind (value there?)
;	    (gethash pair *tc-eq-hash*)
;	  (if there?
;	      value
;	      (let ((nvalue (tc-eq* x y nil)))
;		(setf (gethash pair *tc-eq-hash*) nvalue)
;		nvalue))))))

;(defun tc-eq (x y)
;  (or (eq x y)
;      (multiple-value-bind (value1 there1?)
;	  (gethash x *tc-eq-hash*)
;	(if there1?
;	    (multiple-value-bind (value2 there2?)
;		(gethash y value1)
;	      (if there2?
;		  value2
;		  (let ((nvalue (tc-eq* x y nil)))
;		    (setf (gethash y value1) nvalue)
;		    nvalue)))
;	    (let ((nvalue (tc-eq* x y nil))
;		  (nhash (make-hash-table :test #'eq)))
;	      (setf (gethash x *tc-eq-hash*) nhash)
;	      (setf (gethash y nhash) nvalue)
;	      nvalue)))))

(defun tc-eq (x y)
  (or (eq x y)
      (tc-eq* x y nil)))

(defun tc-eq-with-bindings (x y bindings)
  (or (eq x y)
      (tc-eq* x y bindings)))

;;; strong-tc-eq is the same as tc-eq, except that it checks the types of
;;; projection and field application operators, not just the argument.
;;; See tc-eq* (application application).

(defvar *strong-tc-eq-flag* nil)

(defun strong-tc-eq (x y)
  (let ((*strong-tc-eq-flag* t))
    (tc-eq* x y nil)))

;;; If two objects aren't tc-eq* by more specific methods, then try eq.

(defmethod tc-eq* (x y bindings)
  (declare (ignore bindings))
  (eq x y))

(defmethod tc-eq* ((l1 cons) (l2 cons) bindings)
  (and (tc-eq* (car l1) (car l2) bindings)
       (tc-eq* (cdr l1) (cdr l2)
	       (new-tc-eq-list-bindings (car l1) (car l2) bindings))))

(defmethod tc-eq* ((l1 null) (l2 cons) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((l1 cons) (l2 null) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((l1 null) (l2 null) bindings)
  (declare (ignore bindings))
  t)

(defmethod new-tc-eq-list-bindings ((b1 binding) (b2 binding) bindings)
  (acons b1 b2 bindings))

(defmethod new-tc-eq-list-bindings (e1 e2 bindings)
  (declare (ignore e1 e2))
  bindings)

(defun type-binding? (te)
  (typep te '(or dep-binding field-decl)))

(defmethod tc-eq* ((t1 dep-binding) (t2 dep-binding) bindings)
  (declare (ignore bindings))
  (eq t1 t2))

(defmethod tc-eq* ((t1 type-expr) (t2 dep-binding) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((t1 dep-binding) (t2 type-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((t1 binding) (t2 binding) bindings)
  (declare (ignore bindings))
  (eq t1 t2))


;;; Type expressions - enumtypes are not handled since they are never
;;; the canonical form.

(defmethod tc-eq* :around ((t1 type-expr) (t2 type-expr) bindings)
   (or (eq t1 t2)
       (and (call-next-method)
	    (or (not *strong-tc-eq-flag*)
		(and (tc-eq* (print-type t1) (print-type t2) bindings)
		     (eq (from-conversion t1) (from-conversion t2)))))))

(defmethod tc-eq* ((t1 subtype) (t2 subtype) bindings)
  (with-slots ((st1 supertype) (p1 predicate)) t1
    (with-slots ((st2 supertype) (p2 predicate)) t2
      (and (tc-eq* st1 st2 bindings)
	   (tc-eq-ops p1 p2 bindings)))))


;;; This is needed since expr-as-type is used as a print-type, in which
;;; case the supertype slot will not be set and we need to look at the
;;; exprs instead.  Note that we can ignore the predicate slot, since the
;;; supertype will not be set without also setting the predicate.

(defmethod tc-eq* ((t1 expr-as-type) (t2 expr-as-type) bindings)
  (with-slots ((s1 supertype) (e1 expr)) t1
    (with-slots ((s2 supertype) (e2 expr)) t2
      (if (or s1 s2)
	  (call-next-method)
	  (tc-eq* e1 e2 bindings)))))


(defmethod tc-eq* ((t1 funtype) (t2 funtype) bindings)
  (with-slots ((d1 domain) (r1 range)) t1
    (with-slots ((d2 domain) (r2 range)) t2
      (or (eq t1 t2)
	  (tc-eq-types (list d1 r1) (list d2 r2) bindings)))))

(defmethod tc-eq* ((t1 tupletype) (t2 tupletype) bindings)
  (with-slots ((ty1 types)) t1
    (with-slots ((ty2 types)) t2
      (or (eq t1 t2)
	  (tc-eq-types ty1 ty2 bindings)))))

(defun tc-eq-types (types1 types2 bindings)
  (declare (list types1 types2))
  (cond ((null types1) (null types2))
	((null types2) nil)
	(t (and (tc-eq-bindings (car types1) (car types2) bindings)
		(tc-eq-types (cdr types1) (cdr types2)
			     (new-tc-eq-list-bindings (car types1) (car types2)
						      bindings))))))

(defmethod tc-eq-bindings ((b1 dep-binding) (b2 dep-binding) bindings)
  (tc-eq* (type b1) (type b2) bindings))

(defmethod tc-eq-bindings ((b1 dep-binding) b2 bindings)
  (tc-eq* (type b1) b2 bindings))

(defmethod tc-eq-bindings (b1 (b2 dep-binding) bindings)
  (tc-eq* b1 (type b2) bindings))

(defmethod tc-eq-bindings ((b1 field-decl) (b2 field-decl) bindings)
  (and (eq (id b1) (id b2))
	   (tc-eq* (type b1) (type b2) bindings)))

(defmethod tc-eq-bindings (e1 e2 bindings)
  (tc-eq* e1 e2 bindings))

(defmethod tc-eq* ((t1 recordtype) (t2 recordtype) bindings)
  (with-slots ((fields1 fields)) t1
    (with-slots ((fields2 fields)) t2
      (or (eq t1 t2)
	  (tc-eq-types fields1 fields2 bindings)))))

(defun tc-eq-fields-type (flds1 flds2 bindings)
  (declare (list flds1 flds2))
  (cond ((null flds1) (null flds2))
	((null flds2) nil)
	(t (let* ((fld1 (car flds1))
		  (fld2 (find fld1 flds2
			      :test #'(lambda (x y)
					(and (same-id x y)
					     (tc-eq* (type x) (type y)
						     bindings))))))
	     (and fld2
		  (tc-eq-fields-type (cdr flds1)
				     (remove fld2 flds2 :test #'eq)
				     (acons fld1 fld2 bindings)))))))

(defun tc-eq-fields (flds1 flds2 bindings)
  (cond ((null flds1) (null flds2))
	((null flds2) nil)
	(t (let ((fld2 (find (car flds1) flds2
			     :test #'(lambda (x y)
				       (same-id (caar (arguments x))
						(caar (arguments y)))))))
	     (and fld2
		  (tc-eq* (expression (car flds1)) (expression fld2) bindings)
		  (tc-eq-fields (cdr flds1) (remove fld2 flds2) bindings))))))

(defmethod tc-eq* ((f1 field-decl) (f2 field-decl) bindings)
  (declare (ignore bindings))
  (eq f1 f2))


;;; Expressions

(defmethod tc-eq* ((e1 field-name-expr) e2 bindings)
  (declare (ignore bindings e2))
  nil)

(defmethod tc-eq* ((e1 name-expr) (e2 field-name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 field-name-expr) (e2 field-name-expr) bindings)
  (with-slots ((id1 id) (res1 resolutions)) e1
    (with-slots ((id2 id) (res2 resolutions)) e2
      (or (eq e1 e2)
	  (and (eq id1 id2)
	       (tc-eq* (car res1) (car res2) bindings))))))

(defmethod tc-eq* ((e1 projection-application) (e2 projection-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 index) (arg1 argument)) e1
	(with-slots ((id2 index) (arg2 argument)) e2
	  (assert (and id1 id2))
	  (and (= id1 id2)
	       (tc-eq* arg1 arg2 bindings))))))

(defmethod tc-eq* ((e1 projection-application) (e2 name-expr) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 name-expr) (e2 projection-application) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 field-application) (e2 field-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 id) (arg1 argument)) e1
	(with-slots ((id2 id) (arg2 argument)) e2
	  (and (eq id1 id2)
	       (tc-eq* arg1 arg2 bindings))))))


(defmethod tc-eq* ((e1 number-expr) (e2 number-expr) bindings)
  (declare (ignore bindings))
  (with-slots ((n1 number)) e1
    (with-slots ((n2 number)) e2
      (= (the integer n1) (the integer n2)))))

(defmethod tc-eq* ((e1 record-expr) (e2 record-expr) bindings)
  (or (eq e1 e2)
      (with-slots ((ass1 assignments)) e1
	(with-slots ((ass2 assignments)) e2
	  (tc-eq-fields ass1 ass2 bindings)))))

(defmethod tc-eq* ((e1 tuple-expr) (e2 tuple-expr) bindings)
  (or (eq e1 e2)
      (with-slots ((ex1 exprs)) e1
	(with-slots ((ex2 exprs)) e2
	  (tc-eq* ex1 ex2 bindings)))))

(defmethod tc-eq* ((e1 tuple-expr) (e2 name-expr) bindings)
  (let ((bex (cdr (assoc e1 bindings :test #'tc-eq))))
    (and bex
	 (tc-eq* bex e2 bindings))))

(defmethod tc-eq* ((e1 name-expr) (e2 tuple-expr) bindings)
  (let ((bex (cdr (assoc e1 bindings :test #'tc-eq))))
    (and bex
	 (tc-eq* bex e2 bindings))))

(defmethod tc-eq* ((e1 cases-expr) (e2 cases-expr) bindings)
  (with-slots ((expr1 expression) (sel1 selections) (else1 else-part)) e1
    (with-slots ((expr2 expression) (sel2 selections) (else2 else-part)) e2
      (and (tc-eq* expr1 expr2 bindings)
	   (tc-eq-selections sel1 sel2 bindings)
	   (tc-eq* else1 else2 bindings)))))

(defun tc-eq-selections (sel1 sel2 bindings)
  (cond ((null sel1) (null sel2))
	((null sel2) nil)
	(t (let ((s2 (find (constructor (car sel1)) sel2
			   :test #'tc-eq :key #'constructor)))
	     (and s2
		  (tc-eq* (car sel1) s2 bindings)
		  (tc-eq-selections (cdr sel1) (remove s2 sel2) bindings))))))

(defmethod tc-eq* ((s1 selection) (s2 selection) bindings)
  (with-slots ((cons1 constructor) (args1 args) (ex1 expression)) s1
    (with-slots ((cons2 constructor) (args2 args) (ex2 expression)) s2
      (and (tc-eq* cons1 cons2 bindings)
	   (multiple-value-bind (eq? abindings)
	       (bindings-eq args1 args2 bindings)
	     (and eq?
		  (tc-eq* ex1 ex2 abindings)))))))

(defmethod tc-eq* ((e1 constructor-name-expr) (e2 constructor-name-expr)
		   bindings)
  #+lucid (restore-adt (adt e1))
  #+lucid (restore-adt (adt e2))
  (let* ((adt (adt (adt e1)))
	 (constr (find e1 (constructors adt) :test #'same-id)))
    (if (arguments constr)
	(call-next-method)
	(tc-eq-ops e1 e2 bindings))))

(defmethod tc-eq* ((e1 negation) (e2 negation) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 negation) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 negation) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 conjunction) (e2 conjunction) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 conjunction) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 conjunction) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 disjunction) (e2 disjunction) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 disjunction) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 disjunction) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 implication) (e2 implication) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 implication) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 implication) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 iff) (e2 iff) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 iff) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 iff) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 equation) (e2 equation) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 equation) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 equation) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 branch) (e2 branch) bindings)
  (or (eq e1 e2)
      (with-slots ((arg1 argument)) e1
	(with-slots ((arg2 argument)) e2
	  (tc-eq* arg1 arg2 bindings)))))

(defmethod tc-eq* ((e1 branch) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 branch) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 application) bindings)
  (or (eq e1 e2)
      (with-slots ((op1 operator) (arg1 argument)) e1
	(with-slots ((op2 operator) (arg2 argument)) e2
	  (and (tc-eq* arg1 arg2 bindings)
	       (tc-eq-ops op1 op2 bindings))))))

(defmethod tc-eq-ops ((op1 field-name-expr) (op2 field-name-expr)
		      &optional bindings)
  (with-slots ((id1 id) (ty1 type)) op1
    (with-slots ((id2 id) (ty2 type)) op2
      (and (eq id1 id2)
	   (tc-eq* ty1 ty2 bindings)))))

(defmethod tc-eq-ops ((op1 field-name-expr) (op2 name-expr)
		      &optional bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq-ops ((op1 name-expr) (op2 field-name-expr)
		      &optional bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq-ops ((op1 constructor-name-expr) (op2 constructor-name-expr)
		      &optional bindings)
  (tc-eq-adt-ops op1 op2 bindings))

(defmethod tc-eq-ops ((op1 recognizer-name-expr) (op2 recognizer-name-expr)
		      &optional bindings)
  (tc-eq-adt-ops op1 op2 bindings))

(defmethod tc-eq-ops ((op1 accessor-name-expr) (op2 accessor-name-expr)
		      &optional bindings)
  (tc-eq-adt-ops op1 op2 bindings))

(defmethod tc-eq-ops (op1 op2 &optional bindings)
  (tc-eq* op1 op2 bindings))

(defun tc-eq-adt-ops (op1 op2 bindings)
  (and (eq (id op1) (id op2))
       (let ((adt1 (adt op1))
	     (adt2 (adt op2)))
	 (and (eq (adt adt1) (adt adt2))
	      (let ((acts1 (actuals (module-instance
				     (resolution
				      (or (print-type adt1) adt1)))))
		    (acts2 (actuals (module-instance
				     (resolution
				      (or (print-type adt2) adt2))))))
		(or (null acts1)
		    (null acts2)
		    (tc-eq-adt-actuals acts1
				       acts2
				       bindings
				       (formals-sans-usings (adt adt1))
				       (positive-types (adt adt1)))))))))

(defun tc-eq-adt-actuals (acts1 acts2 bindings formals postypes)
  (or (null acts1)
      (and (if (member (car formals) postypes
		       :test #'same-id
		       :key #'(lambda (x) (or (print-type x) x)))
	       (compatible? (type-value (car acts1)) (type-value (car acts2)))
	       (tc-eq* (car acts1) (car acts2) bindings))
	   (tc-eq-adt-actuals (cdr acts1) (cdr acts2) bindings
			      (cdr formals) postypes))))

;;; Should consider whether all binding-exprs are over a single bound
;;; variable; i.e., the canonical form of FORALL (x:t1), (y:t2): p(x,y) is
;;; FORALL (z:[t1,t2]): p(PROJ_1(z),PROJ_2(z))

(defmethod tc-eq* ((e1 binding-expr) (e2 binding-expr) ibindings)
  (with-slots ((b1 bindings) (ex1 expression)) e1
    (with-slots ((b2 bindings) (ex2 expression)) e2
      (or (eq e1 e2)
	  (and (same-binding-op? e1 e2)
	       (multiple-value-bind (eq? abindings)
		   (bindings-eq b1 b2 ibindings)
		 (and eq?
		      (tc-eq* ex1 ex2 abindings))))))))

(defun bindings-eq (b1 b2 bindings)
  (declare (list b1 b2))
  (let ((nbindings (make-compatible-bindings b1 b2 bindings)))
    (if nbindings
	(values t nbindings)
	(bindings-eq* b1 b2 bindings))))

(defun bindings-eq* (b1 b2 bindings)
  (cond ((null b1)
	 (unless b2
	   (values t bindings)))
	((null b2) nil)
	(t (when (tc-eq* (type (car b1)) (type (car b2)) bindings)
	     (bindings-eq* (cdr b1) (cdr b2)
			   (cons (cons (car b1) (car b2)) bindings))))))

(defun bindings-to-types (bindings types &optional result)
  (if (null bindings)
      (nreverse result)
      (if (typep (car types) 'dep-binding)
	  (let ((db (mk-dep-binding (id (car bindings))
				    (type (car bindings)))))
	    (bindings-to-types (substit (cdr bindings)
				 (acons (car bindings) db nil))
			       (cdr types)
			       (cons db result)))
	  (bindings-to-types (cdr bindings) (cdr types)
			     (cons (type (car bindings)) result)))))

(defun make-compatible-bindings (b1 b2 bindings)
  (if (and (singleton? b1)
	   (typep (type (car b1)) 'tupletype))
      (let ((types1 (types (type (car b1)))))
	(when (and (length= (the list types1) b2)
		   (tc-eq-types (the list types1)
				(bindings-to-types b2 types1)
				bindings))
	  (make-compatible-bindings* b1 b2 bindings nil)))
      (when (and (singleton? b2)
		 (typep (type (car b2)) 'tupletype))
	(let ((types2 (types (type (car b2)))))
	  (when (and (length= b1 (the list types2))
		     (tc-eq-types (bindings-to-types b1 types2)
				  (the list types2)
				  bindings))
	    (make-compatible-bindings* b2 b1 bindings t))))))

(defun make-compatible-bindings* (b1 b2 bindings rev?)
  (let* ((projs (make!-projections (make-variable-expr (car b1))))
	 (vars (mapcar #'make-variable-expr b2)))
    (nconc (if rev?
	       (pairlis vars projs)
	       (pairlis projs vars))
	   bindings)))

	  

;(defun bindings-eq* (b1 b2 bindings result)
;  (if (null b1)
;      (values t result)
;      (when (tc-eq* (type (car b1)) (type (car b2)) bindings)
;	(bindings-eq* (cdr b1) (cdr b2) bindings
;		      (cons (cons (car b1) (car b2))
;			    result)))))

(defmethod same-binding-op? ((op1 lambda-expr) (op2 lambda-expr))
  t)

(defmethod same-binding-op? ((op1 forall-expr) (op2 forall-expr))
  t)

(defmethod same-binding-op? ((op1 exists-expr) (op2 exists-expr))
  t)

(defmethod same-binding-op? (op1 op2)
  (declare (ignore op1 op2))
  nil)

;;NSH: to ignore coercions;;;;;;
(defmethod tc-eq* ((A coercion) (B coercion) bindings)
  (or (eq A B)
      (tc-eq* (args1 A) (args1 B) bindings)))

(defmethod tc-eq* ((A coercion) (B expr) bindings)
  (tc-eq* (args1 A) B bindings))

(defmethod tc-eq* ((A expr) (B coercion) bindings)
  (tc-eq* A (args1 B) bindings))

(defmethod tc-eq* ((A name-expr) (B coercion) bindings)
  (tc-eq* A (args1 B) bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tc-eq* ((e1 update-expr) (e2 update-expr) bindings)
  (or (eq e1 e2)
      (with-slots ((ex1 expression) (ass1 assignments)) e1
	(with-slots ((ex2 expression) (ass2 assignments)) e2
	  (and (tc-eq* ex1 ex2 bindings)
	       (tc-eq* ass1 ass2 bindings))))))

(defmethod tc-eq* ((a1 assignment) (a2 assignment) bindings)
  (or (eq a1 a2)
      (with-slots ((args1 arguments) (ex1 expression)) a1
	(with-slots ((args2 arguments) (ex2 expression)) a2
	  (and (tc-eq* args1 args2 bindings)
	       (tc-eq* ex1 ex2 bindings))))))

;;; Make sure we don't allow bindings and names to be tc-eq*

(defmethod tc-eq* ((n1 binding) (n2 name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 binding) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 modname) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 modname) (n2 name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 name) bindings)
  (with-slots ((id1 id) (res1 resolutions)) n1
    (with-slots ((id2 id) (res2 resolutions)) n2
      (or (eq n1 n2)
	  (tc-eq* (car res1) (car res2) bindings)))))

(defmethod tc-eq* ((res1 resolution) (res2 resolution) bindings)
  (with-slots ((decl1 declaration) (mi1 module-instance)) res1
    (with-slots ((decl2 declaration) (mi2 module-instance)) res2
      (or (let ((bind (cdr (assq decl1 bindings))))
	    (and bind
		 (eq decl2 (if (consp bind) (car bind) bind))))
	  (and (eq decl1 decl2)
	       (or (binding? decl1)
		   (if mi1
		       (and mi2
			    (tc-eq* mi1 mi2 bindings))
		       (null mi2))))))))

(defmethod tc-eq* ((n1 modname) (n2 modname) bindings)
  (or (eq n1 n2)
      (with-slots ((id1 id) (act1 actuals)) n1
	(with-slots ((id2 id) (act2 actuals)) n2
	  (and (eq id1 id2)
	       (tc-eq* act1 act2 bindings))))))

(defmethod tc-eq* ((a1 actual) (a2 actual) bindings)
  (or (eq a1 a2)
      (with-slots ((tv1 type-value) (ex1 expr)) a1
	(with-slots ((tv2 type-value) (ex2 expr)) a2
	  (if tv1
	      (and tv2
		   (tc-eq* tv1 tv2 bindings))
	      (and (not tv2)
		   (tc-eq* ex1 ex2 bindings)))))))


;;; Compatible? for types checks whether two types have a common
;;; supertype.

(defmethod compatible? ((atype list) (etype list))
  (cond ((null atype) (null etype))
	((null etype) nil)
	(t (and (compatible? (car atype) (car etype))
		(compatible? (cdr atype) (cdr etype))))))

(defmethod compatible? (atype etype)
  (compatible?* (find-supertype atype) (find-supertype etype)))

(defmethod compatible? ((atype actual) (etype actual))
  (compatible?* atype etype))

(defmethod compatible?* :around ((atype type-expr) (etype type-expr))
  (or (tc-eq atype etype)
      (call-next-method)))

(defmethod compatible?* (atype etype)
  (compatible?* (find-supertype atype) (find-supertype etype)))

(defmethod compatible?* ((atype type-expr) (etype type-expr))
  nil)

(defmethod compatible?* ((atype dep-binding) etype)
  (compatible?* (type atype) etype))

(defmethod compatible?* (atype (etype dep-binding))
  (compatible?* atype (type etype)))

;;; We would like to use tc-eq for type-names, but since this can be
;;; called from tc-unify, the actuals may not be known yet.  Note that
;;; we return the instantiated type, if there is one.

(defmethod compatible?* ((atype type-name) (etype type-name))
  #+pvsdebug (assert (and (resolution atype) (resolution etype)))
  (or (declaration-outside-formals? atype)
      (declaration-outside-formals? etype)
      (let* ((mi1 (module-instance atype))
	     (mi2 (module-instance etype))
	     (a1 (actuals mi1))
	     (a2 (actuals mi2)))
	(and (eq (id atype) (id etype))
	     (eq (id mi1) (id mi2))
	     ;;(tc-eq (module-instance atype) (module-instance etype))
	     (or (null a1)
		 (null a2)
		 (actuals-are-outside-formals? a1)
		 (actuals-are-outside-formals? a2)
		 (compatible?* a1 a2))))))

(defmethod compatible?* ((atype type-name) etype)
  (declare (ignore etype))
  (or (declaration-outside-formals? atype)
      (call-next-method)))

(defmethod compatible?* ((atype type-expr) (etype type-name))
  (or (declaration-outside-formals? etype)
      (call-next-method)))

(defmethod compatible?* (atype (etype type-name))
  (declare (ignore atype))
  (or (declaration-outside-formals? etype)
      (call-next-method)))

(defun declaration-outside-formals? (type-name)
  (let ((decl (declaration type-name)))
    (and (typep decl 'formal-decl)
	 (not (memq decl (formals (current-theory)))))))

(defun actuals-are-outside-formals? (actuals)
  (let ((ex (actual-value (car actuals))))
    (and (typep ex 'name)
	 (typep (declaration ex) 'formal-decl)
	 (not (memq (declaration ex) (formals (current-theory)))))))

(defmethod compatible?* ((l1 list) (l2 list))
  (and (length= l1 l2)
       (every #'compatible?* l1 l2)))

(defmethod compatible?* ((a1 actual) (a2 actual))
  (if (type-value a1)
      (and (type-value a2)
	   (compatible?* (type-value a1) (type-value a2)))
      (and (not (type-value a2))
	   (some #'(lambda (aty)
		     (some #'(lambda (ety)
			       (compatible?* aty ety))
			   (ptypes (expr a2))))
		 (ptypes (expr a1))))))


;;; These two simply recurse up the supertype.  This will find the least
;;; compatible supertype since the case where they are both subtypes is
;;; another method.

(defmethod compatible?* ((atype subtype) (etype type-expr))
  (compatible?* (supertype atype) etype))

(defmethod compatible?* ((atype type-expr) (etype subtype))
  (compatible?* atype (supertype etype)))

(defmethod compatible?* ((atype subtype) (etype subtype))
  (compatible?* (supertype atype) (supertype etype)))


;;; Note: no distinction between functions and arrays.

(defmethod compatible?* ((atype funtype) (etype funtype))
  (and (compatible?* (domain atype) (domain etype))
       (compatible?* (range atype) (range etype))))

;(defmethod compatible?* ((atype list) (etype tupletype))
;  (and (length= atype (types etype))
;       (every #'tc-eq atype (types etype))))
;
;(defmethod compatible?* ((atype tupletype) (etype list))
;  (and (length= (types atype) etype)
;       (every #'tc-eq (types atype) etype)))

(defmethod compatible?* ((atype tupletype) (etype tupletype))
  (and (length= (types atype) (types etype))
       (every #'compatible?* (types atype) (types etype))))

(defmethod compatible?* ((atype recordtype) (etype recordtype))
  (and (length= (fields atype) (fields etype))
       (compatible-fields? (fields atype) (fields etype))))

(defun compatible-fields? (aflds eflds)
  (or (null aflds)
      (let* ((afld (car aflds))
	     (efld (find afld eflds :test #'same-id)))
	(and efld
	     (compatible?* (type afld) (type efld))
	     (compatible-fields? (cdr aflds) (remove efld eflds))))))


;;; Strict-Compatible? is like compatible?, but will fail for domains unless
;;; they are tc-eq.

(defun strict-compatible? (atype etype)
  (strict-compatible?* (find-supertype atype) (find-supertype etype) nil))

(defmethod strict-compatible?* :around ((atype type-expr) (etype type-expr)
					bindings)
  (or (tc-eq-with-bindings atype etype bindings)
      (call-next-method)))

(defmethod strict-compatible?* ((atype type-expr) (etype type-expr) bindings)
  (declare (ignore bindings))
  nil)

;;; We would like to use tc-eq for type-names, but since this can be
;;; called from tc-unify, the actuals may not be known yet.  Note that
;;; we return the instantiated type, if there is one.

(defmethod strict-compatible?* ((atype type-name) (etype type-name) bindings)
  #+pvsdebug (assert (and (resolution atype) (resolution etype)))
  (or (declaration-outside-formals? atype)
      (declaration-outside-formals? etype)
      (let* ((mi1 (module-instance atype))
	     (mi2 (module-instance etype))
	     (a1 (actuals mi1))
	     (a2 (actuals mi2)))
	(and (eq (id atype) (id etype))
	     (eq (id mi1) (id mi2))
	     ;;(tc-eq (module-instance atype) (module-instance etype))
	     (or (null a1)
		 (null a2)
		 (actuals-are-outside-formals? a1)
		 (actuals-are-outside-formals? a2)
		 (strict-compatible?* a1 a2 bindings))))))

(defmethod strict-compatible?* ((l1 list) (l2 list) bindings)
  (and (length= l1 l2)
       (every #'(lambda (x y) (strict-compatible?* x y bindings)) l1 l2)))

(defmethod strict-compatible?* ((a1 actual) (a2 actual) bindings)
  (if (type-value a1)
      (and (type-value a2)
	   (tc-eq-with-bindings (type-value a1) (type-value a2) bindings))
      (and (not (type-value a2))
	   (some #'(lambda (aty)
		     (some #'(lambda (ety)
			       (strict-compatible?* aty ety bindings))
			   (ptypes (expr a2))))
		 (ptypes (expr a1))))))


;;; These two simply recurse up the supertype.  This will find the least
;;; compatible supertype since the case where they are both subtypes is
;;; another method.

(defmethod strict-compatible?* ((atype subtype) (etype type-expr) bindings)
  (strict-compatible?* (supertype atype) etype bindings))

(defmethod strict-compatible?* ((atype type-expr) (etype subtype) bindings)
  (strict-compatible?* atype (supertype etype) bindings))

(defmethod strict-compatible?* ((atype subtype) (etype subtype) bindings)
  (strict-compatible?* (supertype atype) (supertype etype) bindings))


;;; Note: no distinction between functions and arrays.

(defmethod strict-compatible?* ((t1 funtype) (t2 funtype) bindings)
  (with-slots ((d1 domain) (r1 range)) t1
    (with-slots ((d2 domain) (r2 range)) t2
      ;; Use tc-eq on the domain
      (and (tc-eq-bindings d1 d2 bindings)
	   (strict-compatible?* r1 r2
				(new-tc-eq-list-bindings d1 d2 bindings))))))

(defmethod strict-compatible?* ((atype tupletype) (etype tupletype) bindings)
  (strict-compatible-types (types atype) (types etype) bindings))

(defun strict-compatible-types (types1 types2 bindings)
  (declare (list types1 types2))
  (cond ((null types1) (null types2))
	((null types2) nil)
	(t (and (strict-compatible-bindings (car types1) (car types2) bindings)
		(strict-compatible-types (cdr types1) (cdr types2)
					 (new-tc-eq-list-bindings
					  (car types1) (car types2)
					  bindings))))))

(defmethod strict-compatible-bindings ((b1 dep-binding) (b2 dep-binding)
				       bindings)
  (strict-compatible?* (type b1) (type b2) bindings))

(defmethod strict-compatible-bindings ((b1 dep-binding) b2 bindings)
  (strict-compatible?* (type b1) b2 bindings))

(defmethod strict-compatible-bindings (b1 (b2 dep-binding) bindings)
  (strict-compatible?* b1 (type b2) bindings))

(defmethod strict-compatible-bindings ((b1 field-decl) (b2 field-decl)
				       bindings)
  (and (eq (id b1) (id b2))
       (strict-compatible?* (type b1) (type b2) bindings)))

(defmethod strict-compatible-bindings (e1 e2 bindings)
  (strict-compatible?* e1 e2 bindings))


(defmethod strict-compatible?* ((atype recordtype) (etype recordtype) bindings)
  (strict-compatible-types (fields atype) (fields etype) bindings))


;;; Compatible-Type for types checks whether two types have a common
;;; supertype.  It returns the (least) common supertype.

(defun compatible-type (t1 t2)
  (when (compatible? t1 t2)
    (compatible-type* t1 t2)))

(defmethod compatible-type* :around ((atype type-expr) (etype type-expr))
  (if (tc-eq atype etype)
      atype
      (call-next-method)))

(defmethod compatible-type* ((atype dep-binding) etype)
  (compatible-type* (type atype) etype))

(defmethod compatible-type* (atype (etype dep-binding))
  (compatible-type* atype (type etype)))

;;; Note that we return the instantiated type, if there is one.

(defmethod compatible-type* ((atype type-name) (etype type-name))
  (let* ((a1 (actuals atype))
	 (a2 (actuals etype)))
    (if (or a1 (not a2)) atype etype)))

(defmethod compatible-type* ((atype type-name) etype)
  (if (declaration-outside-formals? atype)
      etype
      (call-next-method)))

(defmethod compatible-type* (atype (etype type-name))
  (if (declaration-outside-formals? etype)
      atype
      (call-next-method)))


;;; These two simply recurse up the supertype.  This will find the least
;;; compatible supertype since the case where they are both subtypes is
;;; another method.

(defmethod compatible-type* ((atype subtype) (etype type-expr))
  (compatible-type* (supertype atype) etype))

(defmethod compatible-type* ((atype type-expr) (etype subtype))
  (compatible-type* atype (supertype etype)))

;;; This is the tricky one, since we don't want to go higher than
;;; necessary.  For example, given posint as a subtype of nat, we don't
;;; want to return int as the least common supertype.  So we first do a
;;; direct check whether one is a subtype of the other.  If that fails,
;;; we can recurse on both supertypes at the same time.

(defmethod compatible-type* ((atype subtype) (etype subtype))
  (cond ((subtype-of? atype etype) etype)
	((subtype-of? etype atype) atype)
	(t (compatible-type* (supertype atype) (supertype etype)))))

(defun adt-compatible-type (acts1 acts2 formals type postypes compacts)
  (cond ((null acts1)
	 (typecheck* (mk-type-name (id type) (nreverse compacts))
				  nil nil nil))
	((tc-eq (car acts1) (car acts2))
	 (adt-compatible-type (cdr acts1) (cdr acts2) (cdr formals) type
			      postypes (cons (car acts1) compacts)))
	((member (car formals) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration y)))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (let ((ntype (compatible-type (type-value (car acts1))
				       (type-value (car acts2)))))
	   (adt-compatible-type (cdr acts1) (cdr acts2) (cdr formals) type
			      postypes (cons (mk-actual ntype) compacts))))))

(defmethod compatible-type* ((t1 datatype-subtype) (t2 datatype-subtype))
  (adt-compatible-type
   (actuals (module-instance (find-declared-adt-supertype t1)))
   (actuals (module-instance (find-declared-adt-supertype t2)))
   (formals-sans-usings (adt t1))
   t1
   (positive-types (adt t1))
   nil))


;;; An actual funtype is compatible with an expected funtype only if the
;;; actual is a subtype of the expected.  We don't distinguish the
;;; function types, so an array is compatible with a function of the same
;;; signature.

(defmethod compatible-type* ((atype funtype) (etype funtype))
  (with-slots ((adom domain) (arng range)) atype
    (with-slots ((edom domain) (erng range)) etype
      (if (or (fully-instantiated? adom)
	      (not (fully-instantiated? edom)))
	  (lcopy atype 'range (compatible-type* arng erng))
	  (lcopy etype 'range (compatible-type* arng erng))))))

(defmethod compatible-type* ((atype tupletype) (etype tupletype))
  (compatible-tupletypes (types atype) (types etype) nil))

(defun compatible-tupletypes (atypes etypes types)
  (if (null atypes)
      (mk-tupletype (nreverse types))
      (let* ((stype (compatible-type* (car atypes) (car etypes))))
	(compatible-tupletypes (cdr atypes) (cdr etypes)
			       (cons stype types)))))

(defmethod compatible-type* ((atype recordtype) (etype recordtype))
  (compatible-recordtypes (fields atype) (fields etype) atype nil))

(defun compatible-recordtypes (afields efields atype fields)
  (if (null afields)
      (let ((nfields (nreverse fields)))
	(if (equal nfields (fields atype))
	    atype
	    (make-instance 'recordtype
	      'fields nfields
	      'dependent? (dependent? atype))))
      (let* ((afld (car afields))
	     (efld (find afld efields :test #'same-id))
	     (stype (compatible-type* (type afld) (type efld)))
	     (nfield (if (eq stype (type afld))
			 afld
			 (mk-field-decl (id afld) stype stype)))
	     (cdrfields (if (or (eq nfield afld)
				(not (dependent? atype)))
			    (cdr afields)
			    (let* ((nres (make-resolution nfield
					   (current-theory-name)
					   (type afld)))
				   (fne (mk-field-name-expr (id afld) nres)))
			      (substit (cdr afields)
				(acons afld fne nil))))))
	(compatible-recordtypes cdrfields
				(remove efld efields :test #'eq)
				atype (cons nfield fields)))))


;;; Compatible-Preds takes an actual type, an expected type, and an expression
;;; of the actual type and generates a list of predicates whose truth
;;; guarantees that the expression is of the expected type.  We already know
;;; that the types are compatible. We will give
;;; derivations below of the form

;;;  CP(A,E,x) ==> {p1} U CP(A',E',e) U ...

(defun compatible-preds (atype etype aexpr)
  (unless (subtype-of? atype etype)
    (let ((*generate-tccs* 'none)
	  (ex (if (typep aexpr 'binding)
		  (mk-name-expr (id aexpr) nil nil
				(make-resolution aexpr
				  (current-theory-name) (type aexpr)))
		  aexpr)))
      (delete-if #'(lambda (inc) (eq inc *true*))
	(compatible-preds* atype etype ex nil)))))


;;; Checks for tc-eq types so that we can quit early on.

(defmethod compatible-preds* :around ((atype type-expr) (etype type-expr)
				     aexpr incs)
  (declare (ignore aexpr))
  (if (subtype-of? atype etype)
      incs
      (call-next-method)))


;;; Just get down to the types if dep-bindings are involved

(defmethod compatible-preds* ((atype dep-binding) etype aexpr incs)
  (compatible-preds* (type atype) etype aexpr incs))

(defmethod compatible-preds* (atype (etype dep-binding) aexpr incs)
  (compatible-preds* atype (type etype) aexpr incs))


;;; Type-names may refer to a datatype with positively occuring type
;;; parameters, in which case we need to check them.  Otherwise we simply
;;; generate the actual equalities.

;;; CP(T[t],T[s],x) ==> every(CP(t,s, ))(x)

(defmethod compatible-preds* ((atype type-name) (etype type-name)
			     aexpr incs)
  (if (adt? atype)
      (adt-compatible-preds atype etype aexpr incs)
      (append (actual-equalities (actuals (module-instance atype))
				 (actuals (module-instance etype)))
	      incs)))

(defun adt-compatible-preds (atype etype aexpr incs)
  #+pvsdebug (assert (adt? etype))
  (adt-compatible-pred-actuals (actuals (module-instance atype))
			       (actuals (module-instance etype))
			       (formals-sans-usings (adt atype))
			       atype
			       (positive-types (adt atype))
			       aexpr
			       incs))

(defun adt-compatible-pred-actuals (aacts eacts formals atype postypes
					  aexpr incs &optional pospreds)
  (cond ((null aacts)
	 (if (some #'cdr pospreds)
	     (cons (make-compatible-every-pred (reverse pospreds)
					       atype aacts aexpr)
		   incs)
	     incs))
	((null eacts) nil)
	((member (car formals) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration (or (print-type y) y))))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (adt-compatible-pred-actuals
	  (cdr aacts) (cdr eacts) (cdr formals) atype postypes aexpr incs
	  (adt-compatible-pred-actuals* (car aacts) (car eacts) pospreds)))
	((tc-eq (car aacts) (car eacts))
	 (adt-compatible-pred-actuals (cdr aacts) (cdr eacts) (cdr formals)
				      atype postypes aexpr incs pospreds))
	(t (adt-compatible-pred-actuals
	    (cdr aacts) (cdr eacts) (cdr formals) atype postypes aexpr
	    (cons (actual-equality (car aacts) (car eacts)) incs)
	    pospreds))))

(defun adt-compatible-pred-actuals* (aact eact pospreds)
  #+pvsdebug (assert (and (type-value aact) (type-value eact)))
  (let* ((atype (type-value aact))
	 (etype (type-value eact))
	 (stype (compatible-type atype etype))
	 (preds (nth-value 1 (subtype-preds etype stype))))
    (cons (cons aact preds) pospreds)))

(defun make-compatible-every-pred (pospreds atype aacts aexpr)
  (let ((every (make-every-name atype aacts))
	(*generate-tccs* 'none))
    (typecheck* (mk-application (mk-application* every
				  (mapcar #'make-compatible-every-pred*
					  pospreds))
		  (copy aexpr))
		*boolean* nil nil)))

(defun make-every-name (atype acts)
  (declare (ignore acts))
  (let* ((adtth (adt-theory (adt atype)))
	 (decl (find '|every| (theory adtth) :key #'id))
	 (modname (mk-modname (id adtth) (actuals atype)))
	 (res (make-resolution decl modname)))
    (mk-name-expr '|every| nil nil res)))

(defun make-compatible-every-pred* (pospred)
  (cond ((null (cdr pospred))
	 (mk-everywhere-true-function (type-value (car pospred))))
	((singleton? (cdr pospred))
	 (cadr pospred))
	(t (gen-lambda-expr '|x| (type-value (car pospred)) (cdr pospred)))))

(defun actual-equalities (aacts eacts &optional result)
  (if (null aacts)
      (nreverse result)
      (let ((equality (actual-equality (car aacts) (car eacts))))
	(actual-equalities (cdr aacts) (cdr eacts)
			   (if equality
			       (cons equality result)
			       result)))))

(defun actual-equality (aact eact)
  (cond ((type-value aact)
	 #+pvsdebug (assert (type-value eact))
	 (actual-equality-type (type-value aact) (type-value eact)))
	(t #+pvsdebug (assert (not (type-value eact)))
	   (actual-equality-expr (expr aact) (expr eact)))))

(defun actual-equality-type (atype etype)
  (unless (tc-eq atype etype)
    (let* ((stype (compatible-type atype etype))
	   (apred (nth-value 1 (subtype-preds atype stype)))
	   (epred (nth-value 1 (subtype-preds etype stype))))
      (cond ((null apred)
	     (when epred
	       (gen-forall-expr '|x| stype epred)))
	    ((null epred)
	     (gen-forall-expr '|x| stype apred))
	    (t (let* ((id (make-new-variable '|x| (list atype etype)))
		      (bd (typecheck* (mk-bind-decl id stype stype)
				      nil nil nil))
		      (var (mk-name-expr id nil nil
					 (make-resolution bd
					   (current-theory-name) stype)))
		      (aconj (make!-conjunction*
			      (mapcar #'(lambda (o)
					  (beta-reduce (make!-application o var)))
				apred)))
		      (econj (make!-conjunction*
			      (mapcar #'(lambda (o)
					  (beta-reduce (make!-application o var)))
				epred))))
		 (make!-forall-expr
		  (list bd)
		  (make!-iff aconj econj))))))))

(defun actual-equality-expr (aexpr eexpr)
  (unless (tc-eq aexpr eexpr)
    (make!-equation aexpr eexpr)))

;;; These two simply recurse up the supertype; in the latter case the
;;; incs is augmented with the predicate.  This will find the least
;;; compatible supertype since the case where they are both subtypes is
;;; another method.

(defmethod compatible-preds* ((atype subtype) (etype type-expr) aexpr incs)
  (compatible-preds* (supertype atype) etype aexpr incs))

(defmethod compatible-preds* ((atype type-expr) (etype subtype) aexpr incs)
  (compatible-preds* atype (supertype etype) aexpr
		     (cons (beta-reduce (make!-application (predicate etype)
					  aexpr))
			   incs)))

;;; This is the tricky one, since we don't want to go higher than
;;; necessary.  For example, given posint as a subtype of nat, we don't
;;; want to return int as the least common supertype.  So we first do a
;;; direct check whether one is a subtype of the other.  If that fails,
;;; we can recurse on both supertypes at the same time.

(defmethod compatible-preds* ((atype subtype) (etype subtype) aexpr incs)
  (if (subtype-of? atype etype)
      incs
      (compatible-preds* atype (supertype etype) aexpr
			 (cons (mk-red-appl (predicate etype) aexpr) incs))))

(defmethod compatible-preds* ((atype datatype-subtype) (etype datatype-subtype)
			      aexpr incs)
  (adt-compatible-preds atype etype aexpr incs))

(defun mk-red-appl (op &rest args)
  (beta-reduce (apply #'make!-application op args)))


;;; An actual funtype is compatible with a given funtype only if the
;;; actual is a subtype of the expected.  Subtype-compatible returns a
;;; predicate which ensures this.  Note that we use contravariance on
;;; the domain.  We don't distinguish the function types, so an array is
;;; compatible with a function of the same signature.

(defmethod compatible-preds* ((atype funtype) (etype funtype) aexpr incs)
  (with-slots ((adom domain) (arng range)) atype
    (with-slots ((edom domain) (erng range)) etype
      (multiple-value-bind (avar evar arange erange)
	  (make-funtype-vars atype aexpr adom arng etype edom erng)
	(let ((*bound-variables* (cons (declaration avar)
				       (cons (declaration evar)
					     *bound-variables*))))
	  (compatible-funtype-pred atype adom edom avar evar
				   aexpr arange erange incs))))))

(defmethod compatible-preds* ((atype funtype) (etype funtype)
			      (aexpr lambda-expr) incs)
  (with-slots ((adom domain) (arng range)) atype
    (with-slots ((edom domain) (erng range)) etype
      (with-slots (bindings expression) aexpr
	(if (cdr bindings)
	    (call-next-method)
	    (let* ((avar (make-variable-expr (car bindings)))
		   (eid (make-new-variable '|y| (list aexpr atype etype) 1))
		   (ebd (make-bind-decl eid edom))
		   (evar (make-variable-expr ebd))
		   (arange (subst-var-into-deptypes avar adom arng))
		   (erange (subst-var-into-deptypes avar edom erng))
		   (*bound-variables* (cons (declaration avar)
					    (cons (declaration evar)
						  *bound-variables*))))
	      (compatible-funtype-pred atype adom edom avar evar
				       aexpr arange erange incs)))))))

(defun make-funtype-vars (atype aexpr adom arng etype edom erng)
  (let* ((av (make-new-variable '|x| (list aexpr atype etype) 1))
	 (ev (make-new-variable '|y| (list aexpr atype etype) 1))
	 (ae (make-new-variable-name-expr av adom))
	 (ee (make-new-variable-name-expr ev edom)))
    (values ae ee
	    (subst-var-into-deptypes ae adom arng)
	    (subst-var-into-deptypes ae edom erng))))

(defun make-new-variable-name-expr (id type)
  (let* ((ty (if (typep type 'dep-binding) (type type) type))
	 (bd (typecheck* (mk-bind-decl id ty ty) nil nil nil))
	 (ne (mk-name-expr id nil nil (make-resolution bd
					(current-theory-name) ty))))
    ne))

(defun subst-var-into-deptypes (var type types)
  (if (dep-binding? type)
      (substit types (acons type var nil))
      types))

(defun compatible-funtype-pred (atype adom edom avar evar ;;apred epred
				      aexpr arng erng incs)
  (declare (ignore atype evar))
  (let* ((dpreds (equality-predicates adom edom))
	 (dpred (when dpreds
		   (typecheck* dpreds *boolean* nil nil)))
	 (*bound-variables* (append (when (dep-binding? adom) (list adom))
				    (when (dep-binding? edom) (list edom))
				    *bound-variables*))
	 (*dep-bindings* (if (and (dep-binding? adom)
				  (dep-binding? edom))
			     (acons (mk-dep-binding (id adom) (type adom))
				    (list adom edom)
				    *dep-bindings*)
			     *dep-bindings*))
	 (rpreds (compatible-preds* (subst-deps arng) (subst-deps erng)
				    (beta-reduce
				     (make!-application aexpr avar))
				    nil))
	 (rpred (when rpreds
		  (if (and (singleton? rpreds)
			   (forall-expr? (car rpreds)))
		      (make!-forall-expr (cons (declaration avar)
					       (bindings (car rpreds)))
					 (expression (car rpreds)))
		      (make!-forall-expr (list (declaration avar))
					 (make!-conjunction* rpreds)))))
	 (conj (if dpred
		   (if rpred
		       (make!-conjunction dpred rpred)
		       dpred)
		   rpred)))
    (if conj
	(cons conj incs)
	incs)))

(defmethod compatible-preds* ((atype tupletype) (etype tupletype)
			      (aexpr tuple-expr) incs)
  incs)

(defmethod compatible-preds* ((atype tupletype) (etype tupletype) aexpr incs)  
  (compatible-tupletype-preds (types atype) (types etype)
			      (cond ((tuple-expr? aexpr) (exprs aexpr))
				    ((listp aexpr) aexpr)
				    (t (make!-projections aexpr)))
			      incs))

(defun compatible-tupletype-preds (atypes etypes aexprs incs)
  (if (null atypes)
      incs
      (let* ((adep (car atypes))
	     (edep (car etypes))
	     (aexp (car aexprs))
	     ;;(aty (if (dep-binding? adep) (type adep) adep))
	     (ety (if (dep-binding? edep) (type edep) edep))
	     (preds (compatible-predicates (judgement-types+ aexp) ety aexp)))
	(compatible-tupletype-preds
	 (if (dep-binding? adep)
	     (substit (cdr atypes) (acons adep aexp nil))
	     (cdr atypes))
	 (if (dep-binding? edep)
	     (substit (cdr etypes) (acons edep aexp nil))
	     (cdr etypes))
	 (if (dep-binding? adep)
	     (substit (cdr aexprs) (acons adep aexp nil))
	     (cdr aexprs))
	 (append preds incs)))))

(defmethod compatible-preds* ((atype recordtype) (etype recordtype)
			      (aexpr record-expr) incs)
  incs)

(defmethod compatible-preds* ((atype recordtype) (etype recordtype) aexpr incs)
  (compatible-recordtype-preds (subst-fields atype aexpr)
			       (subst-fields etype aexpr)
			       aexpr incs))

(defun subst-fields (rectype expr)
  (if (dependent? rectype)
      (subst-fields* (fields rectype) expr)
      (fields rectype)))

(defun subst-fields* (fields expr &optional nfields)
  (if (null fields)
      (sort-fields nfields nil)
      (subst-fields* (subst-var-application (car fields) (cdr fields) expr)
		     expr
		     (cons (car fields) nfields))))

(defun compatible-recordtype-preds (aflds eflds aexpr incs)
  (assert (type aexpr))
  (cond ((null aflds)
	 (when (null eflds)
	   incs))
	((null eflds) nil)
	(t (let* ((efld (car eflds))
		  (afld (find efld aflds :test #'same-id))
		  (aex (unless (tc-eq (type afld) (type efld))
			 (get-field-application afld aexpr)))
		  (preds (when aex
			   (compatible-predicates
			    (judgement-types+ aex) (type efld) aex))))
	     (compatible-recordtype-preds
	      (remove afld aflds :test #'eq)
	      (cdr eflds)
	      aexpr
	      (append preds incs))))))

(defun get-field-application (field expr)
  (let ((appl (make!-field-application (id field) expr)))
    (beta-reduce appl)))

(defun subst-var-application (field fields aexpr)
  (let* ((ne aexpr)
	 (appl (make!-field-application (id field) ne)))
    (substit fields (list (cons field (beta-reduce appl))))))


;;; Strict-subtype-of?

(defmethod strict-subtype-of*? ((l1 list) (l2 list) dist)
  (multiple-value-bind (ndist strict?)
      (subtype-of*? l1 l2 dist)
    (when (and ndist strict?)
      ndist)))

(defmethod strict-subtype-of*? ((t1 subtype) (t2 type-expr) dist)
  (if (tc-eq (supertype t1) t2)
      (1+ dist)
      (strict-subtype-of*? (supertype t1) t2 (1+ dist))))

(defmethod strict-subtype-of*? ((t1 funtype) (t2 funtype) dist)
  (and (tc-eq (domain t1) (domain t2))
       (strict-subtype-of*? (range t1) (range t2) dist)))

(defmethod strict-subtype-of*? ((t1 tupletype) (t2 tupletype) dist)
  (strict-subtype-of*? (types t1) (types t2) dist))

(defmethod strict-subtype-of*? ((t1 dep-binding) (t2 dep-binding) dist)
  (strict-subtype-of*? (type t1) (type t2) dist))

(defmethod strict-subtype-of*? ((t1 dep-binding) (t2 type-expr) dist)
  (strict-subtype-of*? (type t1) t2 dist))

(defmethod strict-subtype-of*? ((t1 type-expr) (t2 dep-binding) dist)
  (strict-subtype-of*? t1 (type t2) dist))

(defmethod strict-subtype-of*? ((t1 type-expr) (t2 type-expr) dist)
  (declare (ignore dist))
  nil)


;;; Subtype-of? is used to determine if two types are in the subtype
;;; relation to each other.  It returns the distance from t1 to t2 if t1
;;; is a subtype of t2, otherwise it returns NIL.  t1 is a subtype of t2
;;; either directly through type declarations, or indirectly through
;;; judgements.

(defun subtype-of? (t1 t2)
  (subtype-of*? t1 t2))

(defun strict-subtype-of? (t1 t2)
  (and (not (tc-eq t1 t2))
       (subtype-of? t1 t2)))

(defmethod subtype-of*? :around (t1 t2)
  (or (tc-eq t1 t2)
      (if *subtype-of-hash*
	  (let ((subhash (gethash t1 *subtype-of-hash*)))
	    (if subhash
		(multiple-value-bind (st? there?)
		    (gethash t2 subhash)
		  (if there?
		      st?
		      (let ((nvalue (or (call-next-method)
					(known-subtype-of? t1 t2))))
			(setf (gethash t2 subhash) nvalue))))
		(let ((ht (make-hash-table :test #'eq)))
		  (setf (gethash t1 *subtype-of-hash*) ht)
		  (let ((nvalue (or (call-next-method)
				    (known-subtype-of? t1 t2))))
		    (setf (gethash t2 ht) nvalue)))))
	  (or (call-next-method)
	      (known-subtype-of? t1 t2)))))
	 

(defmethod subtype-of*? :around (t1 (t2 subtype))
   (if (everywhere-true? (predicate t2))
       (subtype-of*? t1 (supertype t2))
       (call-next-method)))

(defmethod subtype-of*? ((t1 dep-binding) t2)
  (subtype-of*? (type t1) t2))

(defmethod subtype-of*? (t1 (t2 dep-binding))
  (subtype-of*? t1 (type t2)))

(defmethod subtype-of*? ((t1 datatype-subtype) (t2 datatype-subtype))
  (adt-subtype-of?
   (actuals (module-instance t1))
   (actuals (module-instance t2))
   (formals-sans-usings (adt t1))
   (declared-type t1)
   (positive-types (adt t1))))

(defun adt-subtype-of? (acts1 acts2 formals type postypes)
  (cond ((null acts1) t)
	((null acts2) nil)
	((tc-eq (car acts1) (car acts2))
	 (adt-subtype-of? (cdr acts1) (cdr acts2) (cdr formals) type
			  postypes))
	((member (car formals) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration (or (print-type y) y))))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (and (subtype-of*? (type-value (car acts1))
			    (type-value (car acts2)))
	      (adt-subtype-of? (cdr acts1) (cdr acts2) (cdr formals) type
			       postypes)))
	(t nil)))

(defmethod subtype-of*? ((t1 subtype) (t2 subtype))
  (with-slots ((st1 supertype) (pr1 predicate)) t1
    (with-slots ((st2 supertype) (pr2 predicate)) t2
      (if (or (and (typep pr1 'recognizer-name-expr)
		   (typep pr2 'recognizer-name-expr)
		   (tc-eq-ops pr1 pr2))
	      (same-predicate? t1 t2 nil))
	  (subtype-of*? st1 st2)
	  (call-next-method)))))

(defmethod subtype-of*? ((t1 adt-type-name) (t2 subtype))
  (and (adt t1)
       (singleton? (constructors (adt t1)))
       (tc-eq t1 (supertype t2))
       (recognizer-name-expr? (predicate t2))))

(defmethod subtype-of*? ((t1 subtype) (t2 type-expr))
  (subtype-of*? (supertype t1) t2))

(defmethod subtype-of*? ((t1 funtype) (t2 funtype))
  (when (tc-eq (domain t1) (domain t2))
    (subtype-of*? (range t1) (range t2))))

(defmethod subtype-of*? ((t1 tupletype) (t2 tupletype))
  (when (length= (types t1) (types t2))
    (subtype-of-list (types t1) (types t2))))

(defun subtype-of-list (t1 t2)
  (or (null t1)
      (and (subtype-of*? (car t1) (car t2))
	   (subtype-of-list (cdr t1) (cdr t2)))))

(defmethod subtype-of*? ((r1 recordtype) (r2 recordtype))
  (when (length= (fields r1) (fields r2))
    (subtype-of-fields (fields r1) (fields r2))))

(defun subtype-of-fields (t1 t2)
  (or (null t1)
      (when (eq (id (car t1)) (id (car t2)))
	(and (subtype-of*? (type (car t1)) (type (car t2)))
	     (subtype-of-fields (cdr t1) (cdr t2))))))

(defmethod subtype-of*? ((t1 type-expr) (t2 type-expr))
  nil)


;;; Subtype-pred returns a single predicate that is the conjunction of the
;;; value returned by subtype-preds.

(defun subtype-pred (t1 t2)
  (multiple-value-bind (type preds)
      (subtype-preds t1 t2)
    (when type
      (make-single-subtype-pred type preds))))

(defun make-single-subtype-pred (type preds)
  (if (singleton? preds)
      (car preds)
      (gen-lambda-expr '|x| type preds)))


;;; Subtype-preds is used to return the predicates which make elements of the
;;; second type belong to the first type.  It returns multiple values, the
;;; first is either t2 or NIL, if t1 is/is not a subtype of t2.  The second
;;; value is the list of predicates.  Given an element of type t2, the
;;; conjunction of the applications of the predicates on that element
;;; guarantees that the element is of type t1.

(defmethod subtype-preds :around ((t1 type-expr) (t2 type-expr) &optional incs)
  (if (tc-eq t1 t2)
      (values t2 incs)
      (call-next-method)))

(defmethod subtype-preds ((t1 type-expr) (t2 type-expr) &optional incs)
  (declare (ignore incs))
  nil)

(defmethod subtype-preds ((t1 type-name) (t2 type-name) &optional incs)
  (let* ((vid (make-new-variable '|v| t2))
	 (vb (typecheck* (mk-bind-decl vid t2 t2) nil nil nil))
	 (svar (mk-name-expr vid nil nil
			     (make-resolution vb (current-theory-name) t2)))
	 (preds (compatible-preds t2 t1 svar)))
    (if preds
	(let ((lpred (make!-lambda-expr (list vb)
		       (make!-conjunction* preds))))
	  (values t2 (cons lpred incs)))
	(values t2 incs))))

(defmethod subtype-preds ((t1 dep-binding) (t2 dep-binding) &optional incs)
  (subtype-preds (type t1) (type t2) incs))

(defmethod subtype-preds ((t1 dep-binding) (t2 type-expr) &optional incs)
  (subtype-preds (type t1) t2 incs))

(defmethod subtype-preds ((t1 type-expr) (t2 dep-binding) &optional incs)
  (multiple-value-bind (ty preds)
      (subtype-preds t1 (type t2) incs)
    (when ty
      (values t2 preds))))

(defmethod subtype-preds ((t1 subtype) (t2 type-expr) &optional incs)
  (subtype-preds (supertype t1) t2 (cons (predicate t1) incs)))

(defmethod subtype-preds ((t1 subtype) (t2 subtype) &optional incs)
  (if (subtype-of? t1 t2)
      (subtype-preds (supertype t1) t2 (cons (predicate t1) incs))
      (multiple-value-bind (ty preds)
	  (subtype-preds t1 (compatible-type t2 t1) nil)
	(when ty
	  (if preds
	      (let* ((vid (make-new-variable '|v| t2))
		     (vb (typecheck* (mk-bind-decl vid t2 t2) nil nil nil))
		     (svar (mk-name-expr vid nil nil
					 (make-resolution vb
					   (current-theory-name) t2)))
		     (lpred (make!-lambda-expr (list vb)
			      (make!-conjunction*
			       (mapcar #'(lambda (p) (make!-application p svar))
				 preds)))))
		(values t2 (cons lpred incs)))
	      (values t2 incs))))))

(defmethod subtype-preds ((t1 datatype-subtype) (t2 datatype-subtype)
			  &optional incs)
  (let* ((id (make-new-variable '|x| (list t1 t2)))
	 (bd (typecheck* (mk-bind-decl id t2 t2)
			 nil nil nil))
	 (var (mk-name-expr id nil nil
			    (make-resolution bd (current-theory-name) t2)))
	 (preds (adt-compatible-preds t2 t1 var nil)))
    (values t2
	    (if preds
		(cons (make!-lambda-expr (list bd)
			(make!-conjunction* preds))
		      incs)
		incs))))


;;; Given [d1,...,dn -> t1], [d1,...,dn -> t2],
;;;   where (subtype-preds t1 t2) = p,
;;; Generates (LAMBDA (f:[d1,...,dn -> t2]):
;;;              (FORALL (x:[d1,...,dn]): p(f(x))))

(defmethod subtype-preds ((t1 funtype) (t2 funtype) &optional incs)
  (let ((types1 (extract-domain t1))
	(types2 (extract-domain t2)))
    (and (length= types1 types2)
	 (tc-eq types1 types2)
	 (multiple-value-bind (ty preds)
	     (subtype-preds (range t1) (range t2))
	   (when ty
	     (let* ((xid (make-new-variable '|x| (list t1 t2)))
		    (tupty (domain t2))
		    (xb (typecheck* (mk-bind-decl xid tupty tupty) nil nil nil))
		    (xvar (mk-name-expr xid nil nil
					(make-resolution xb
					  (current-theory-name) tupty)))
		    (vid (make-new-variable '|f| (list t1 t2)))
		    (vb (typecheck* (mk-bind-decl vid t2 t2) nil nil nil))
		    (var (mk-name-expr vid nil nil
				       (make-resolution vb
					 (current-theory-name) t2)))
		    (npred (make!-lambda-expr (list vb)
			     (make!-forall-expr (list xb)
			       (make!-conjunction*
				(mapcar #'(lambda (pred)
					    (beta-reduce
					      (make!-application pred
						(make!-application var
						  xvar))))
				  preds))))))
	       (values t2 (cons npred incs))))))))

(defun extract-domain (ftype)
  (let ((dom (domain ftype)))
    (if (and (singleton? dom)
	     (tupletype? (car dom)))
	(types (car dom))
	(list dom))))

(defmethod subtype-preds ((t1 tupletype) (t2 tupletype) &optional incs)
  (when (length= (types t1) (types t2))
    (let* ((vid (make-new-variable '|t| (list t1 t2)))
	   (vb (typecheck* (mk-bind-decl vid t2 t2) nil nil nil))
	   (var (mk-name-expr vid nil nil
			      (make-resolution vb
				(current-theory-name) t2))))
      (multiple-value-bind (ty pred)
	  (subtype-tuple-preds (types t1) (types t2) t2 vb var)
	(when ty
	  (values t2 (cons pred incs)))))))

;;; Given [s1,...,sn] \subtype [t1,...,tn],
;;;   where (subtype-preds si ti) = (pi1 ... pim)
;;; each element of preds is of the form pik(PROJ_i(t)),
;;; and the final result is of the form
;;;   (LAMBDA (t:[t1,...,tn]): p11(PROJ_1(t)) & ... & pnm(PROJ_n(t)))

(defun subtype-tuple-preds (types1 types2 type2 vb var &optional (num 1) preds)
  (if (null types1)
      (values type2
	      (make!-lambda-expr (list vb)
		(make!-conjunction* (nreverse (beta-reduce preds)))))
      (multiple-value-bind (ty cpreds)
	  (subtype-preds (car types1) (car types2))
	(when ty
	  (let ((npreds (subtype-tuple-preds* cpreds var num)))
	    (subtype-tuple-preds (if (typep (car types1) 'dep-binding)
				     (substit (cdr types1)
				       (acons (car types1)
					      (make!-projection-application
					       num var)
					      nil))
				     (cdr types1))
				 (cdr types2)
				 type2 vb var (1+ num)
				 (nconc npreds preds)))))))

(defun subtype-tuple-preds* (cpreds var num &optional preds)
  (if (null cpreds)
      preds
      (let ((npred (make!-application (car cpreds)
		     (make!-projection-application num var))))
	(subtype-tuple-preds* (cdr cpreds) var num (cons npred preds))))) 

(defmethod subtype-preds ((t1 recordtype) (t2 recordtype) &optional incs)
  (when (length= (fields t1) (fields t2))
    (let* ((vid (make-new-variable '|t| (list t1 t2)))
	   (vb (typecheck* (mk-bind-decl vid t2 t2) nil nil nil))
	   (var (mk-name-expr vid nil nil
			      (make-resolution vb
				(current-theory-name) t2))))
      (multiple-value-bind (ty pred)
	  (subtype-record-preds (fields t1) (fields t2) t2 vb var
				(dependent? t1))
	(when ty
	  (values t2 (cons pred incs)))))))

(defun subtype-record-preds (flds1 flds2 type2 vb var dep? &optional preds)
  (if (null flds1)
      (values type2
	      (make!-lambda-expr (list vb)
		(make!-conjunction* (nreverse (beta-reduce preds)))))
      (let* ((fld1 (car flds1))
	     (fld2 (find fld1 flds2 :test #'same-id)))
	(multiple-value-bind (ty cpreds)
	    (subtype-preds (type fld1) (type fld2))
	  (when ty
	    (let ((npreds (subtype-record-preds* cpreds var fld2)))
	      (subtype-record-preds (if dep?
					(substit (cdr flds1)
					  (acons (car flds1)
						 (make!-field-application
						  (id fld1) var)
						 nil))
					(cdr flds1))
				    flds2 type2 vb var dep?
				    (nconc npreds preds))))))))

(defun subtype-record-preds* (cpreds var fld &optional preds)
  (if (null cpreds)
      preds
      (let ((npred (make!-application (car cpreds)
		     (make!-field-application (id fld) var))))
	(subtype-record-preds* (cdr cpreds) var fld (cons npred preds)))))


(defun subst-deps (obj)
  (if (null *dep-bindings*)
      obj
      (gensubst obj #'subst-deps! #'subst-deps?)))

(defmethod subst-deps! ((te recordtype))
  te)

(defmethod subst-deps! ((ex name-expr))
  (let* ((deps (rassoc (declaration ex) *dep-bindings*
		       :test #'member))
	 (dep (caddr deps))
	 (type (if (binding? dep)
		   (type dep)
		   dep)))
    ;;(assert (id *current-theory*))
    (mk-name-expr (id dep)
      nil nil (make-resolution dep
		(if (binding? dep)
		    (current-theory-name)
		    (module-instance dep))
		type))))

(defmethod subst-deps? (ex)
  (declare (ignore ex))
  nil)

(defmethod subst-deps? ((ex name-expr))
  (and (rassoc (declaration ex) *dep-bindings* :test #'member)
       (or ;;(not (slot-exists-p (declaration ex) 'recordtype))
	   (not (funtype? (type ex)))
	   (not (singleton? (domain (type ex))))
	   ;;(not (eq (car (domain (type ex))) (recordtype (declaration ex))))
	   )))

;(defun make-funtype-pred-bindings (vars edom types result)
;  (if (null vars)
;      (nreverse result)
;      (let* ((dom (car edom))
;	     (vb (typecheck* (mk-bind-decl (id (car vars)) (car types)
;					  (car types))
;			     nil nil nil)))
;	(make-funtype-pred-bindings (cdr vars)
;				    (cdr edom)
;				    (if (binding? (car edom))
;					(substit (cdr types)
;					  (list (cons (car edom) vb)))
;					(cdr types))
;				    (cons vb result)))))

;;; Type-canon generally tries to push down subtypes.
;;; For example, it takes types of the form
;;;  {t: [T1,..,Tn] | p(proj_1(t),..,proj_n(t))}
;;; and pushes down all the projections it can.  It does this by splitting
;;; p into conjuncts, collecting those conjuncts in which t only occurs as
;;; the argument to a proj_i for some i (the same throughout the
;;; conjunct), and pushing those conjuncts down.  For example,
;;;  {t: [T1,..,Tn] | p(proj_1(t)) AND q(proj_2(t)) AND r(proj_1(t)) AND
;;;                   (s(proj_1(t)) OR u(proj_2(t)))}
;;;  yields
;;;  {t: [{x:T1|p(x) AND r(x)},{x:T2|q(x)},..,Tn] |
;;;                                      s(proj_1(t)) OR u(proj_2(t))}

(defvar *type-canon-hash* nil)

(defun reset-type-canon-cache ()
  (setq *type-canon-hash* nil)
;  (if *type-canon-hash*
;      (pvs-clrhash *type-canon-hash*)
;      (make-pvs-hash-table :hashfn #'pvs-sxhash :test #'tc-eq))
  )

(defun type-canon (te)
  (type-canon* te nil))

;(defmethod type-canon* :around (te predicate)
;  (let ((hobj nil ;(car (member te *type-canon-hash* :test #'tc-eq))
;	      ;;(pvs-gethash-eq te *type-canon-hash*)
;	      ))
;    (or hobj
;	(let ((nobj (call-next-method)))
;	  ;;(setf (pvs-gethash te *type-canon-hash*) nobj)
;	  (push nobj *type-canon-hash*)
;	  nobj))))

(defmethod type-canon* ((te type-expr) predicates)
  (if predicates
      (make-instance 'subtype
	'supertype te
	'predicate (make-lambda-conjunction te predicates))
      te))

(defun make-lambda-conjunction (te predicates)
  (if (cdr predicates)
      (multiple-value-bind (preds var)
	  (make-preds-with-same-binding te predicates)
	(make-lambda-expr-or-eta-equivalent (declaration var)
					    (make!-conjunction* preds)))
      (possibly-eta-reduce (car predicates))))

(defmethod possibly-eta-reduce (expr)
  expr)

(defmethod possibly-eta-reduce ((expr lambda-expr))
  (if (and (typep (expression expr) 'application)
	   (or (and (singleton? (bindings expr))
		    (typep (argument (expression expr)) 'name-expr)
		    (same-declaration (car (bindings expr))
				      (argument (expression expr))))
	       (and (typep (argument (expression expr)) 'tuple-expr)
		    (every #'name-expr? (exprs (argument (expression expr))))
		    (length= (bindings expr)
			     (exprs (argument (expression expr))))
		    (every #'same-declaration
			   (bindings expr)
			   (exprs (argument (expression expr)))))))
      (operator (expression expr))
      expr))

(defmethod make-lambda-expr-or-eta-equivalent (bind-decl expr)
  (make!-lambda-expr (list bind-decl) expr))

(defmethod make-lambda-expr-or-eta-equivalent (bind-decl (expr application))
  (if (and (typep (argument expr) 'name-expr)
	   (same-declaration (argument expr) bind-decl))
      (operator expr)
      (make!-lambda-expr (list bind-decl) expr)))

(defun make-preds-with-same-binding (te predicates)
  (let* ((id (make-new-variable '|x| predicates))
	 (bd (typecheck* (mk-bind-decl id te te) nil nil nil))
	 (nvar (mk-name-expr id nil nil (make-resolution bd
					  (current-theory-name) te))))
    (values (make-preds-with-same-binding* nvar predicates)
	    nvar)))

(defun make-preds-with-same-binding* (nvar predicates &optional result)
  (if (null predicates)
      result
      (let ((npreds (and+ (make-pred-with-same-binding
			   nvar (car predicates)))))
	(make-preds-with-same-binding* nvar (cdr predicates)
				       (nconc result npreds)))))

(defmethod make-pred-with-same-binding (nvar (pred lambda-expr))
  (substit (expression pred)
    (acons (car (bindings pred)) nvar nil)))

(defmethod make-pred-with-same-binding (nvar pred)
  (make!-application pred nvar))


(defmethod type-canon* ((te subtype) predicates)
  (type-canon* (supertype te) (cons (predicate te)  predicates)))

(defmethod type-canon* ((te dep-binding) predicates)
  (lcopy te 'type (type-canon* (type te) predicates)))

(defmethod type-canon* ((te tupletype) predicates)
  (type-canon-tupletype te predicates))

(defun type-canon-tupletype (te predicates)
  (multiple-value-bind (npreds var)
      (make-preds-with-same-binding te predicates)
    (let* ((parts (partition-projection-predicates
		   var (mapcan #'conjuncts npreds)))
	   (ntypes (add-tupletype-preds (types te) parts))
	   (ntuptype (make-instance 'tupletype 'types ntypes))
	   (toppreds (cdr (assq nil parts))))
      (if toppreds
	  (make-instance 'subtype
	    'supertype ntuptype
	    'predicate (make-lambda-expr-or-eta-equivalent (declaration var)
			 (make!-conjunction* toppreds)))
	  ntuptype))))

(defun add-tupletype-preds (types parts &optional (index 1) ntypes)
  (if (null types)
      (nreverse ntypes)
      (let* ((preds (assoc index parts
			   :test #'(lambda (x y)
				     (and y (= x (index y))))))
	     (npreds (when preds
		       (make-new-tupletype-preds (car types) preds)))
	     (ntype (type-canon* (car types) (when npreds (list npreds))))
	     (nrest (if (typep ntype 'dep-binding)
			(subst-for-binding (car types) ntype (cdr types))
			(cdr types))))
	(add-tupletype-preds nrest parts (1+ index)
			     (cons ntype ntypes)))))

(defun subst-for-binding (binding nbinding obj)
  (let* ((nres (make-resolution nbinding (current-theory-name)))
	 (nname (mk-name-expr nbinding nil nil nres)))
    (substit obj (list (cons binding nname)))))

(defun make-new-tupletype-preds (te predicates)
  (let* ((id (make-new-variable '|x| predicates))
	 (type (if (typep te 'dep-binding) (type te) te))
	 (bd (typecheck* (mk-bind-decl id type type) nil nil nil))
	 (nvar (mk-name-expr id nil nil (make-resolution bd
					  (current-theory-name) type)))
	 (conj (make!-conjunction* (gensubst (cdr predicates)
				   #'(lambda (ex) (declare (ignore ex)) nvar)
				   #'(lambda (ex)
				       (tc-eq ex (car predicates)))))))
    (make-new-tupletype-pred conj bd nvar)))

(defmethod make-new-tupletype-pred ((conj application) bd nvar)
  (if (and (tc-eq (argument conj) nvar)
	   (tc-eq (type bd) (domain (find-supertype (type (operator conj))))))
      (operator conj)
      (make!-lambda-expr (list bd) conj)))

(defmethod make-new-tupletype-pred ((conj expr) bd nvar)
  (declare (ignore nvar))
  (make!-lambda-expr (list bd) conj))

(defun partition-projection-predicates (var predicates &optional parts)
  (if (null predicates)
      parts
      (let* ((proj (which-projection-predicate var (car predicates)))
	     (part (assoc proj parts :test #'tc-eq)))
	(partition-projection-predicates
	 var
	 (cdr predicates)
	 (if part
	     (progn (nconc part (list (car predicates)))
		    parts)
	     (cons (list proj (car predicates)) parts))))))

(defun which-projection-predicate (var predicate)
  (let ((proj nil)
	(done nil))
    (mapobject #'(lambda (ex)
		   (cond (done nil)
			 ((and (projection-application? ex)
			       (tc-eq (args1 ex) var))
			  (cond ((null proj)
				 (setq proj ex))
				((not (tc-eq proj ex))
				 (setq done t))))
			 ((and (typep ex 'name-expr)
			       (tc-eq ex var))
			  (setq done t))))
	       predicate)
    (unless done
      proj)))
	       
(defmethod type-canon* ((te recordtype) predicates)
  (type-canon-recordtype te predicates))

(defun type-canon-recordtype (te predicates)
  (multiple-value-bind (npreds var)
      (make-preds-with-same-binding te predicates)
    (let* ((parts (partition-field-predicates
		   var (mapcan #'conjuncts npreds)))
	   (nfields (add-field-type-preds (fields te) parts))
	   (nrectype (make-instance 'recordtype 'fields nfields))
	   (toppreds (cdr (assq nil parts))))
      (if toppreds
	  (make-instance 'subtype
	    'supertype nrectype
	    'predicate (make-lambda-expr-or-eta-equivalent
			(declaration var)
			(make!-conjunction* toppreds)))
	  nrectype))))

(defun partition-field-predicates (var predicates &optional parts)
  (if (null predicates)
      parts
      (let* ((field (which-field-predicate var (car predicates)))
	     (part (assoc field parts :test #'tc-eq)))
	(partition-projection-predicates
	 var
	 (cdr predicates)
	 (if part
	     (progn (nconc part (list (car predicates)))
		    parts)
	     (cons (list field (car predicates)) parts))))))

(defun which-field-predicate (var predicate)
  (let ((field nil)
	(done nil))
    (mapobject #'(lambda (ex)
		   (cond (done nil)
			 ((and (field-application? ex)
			       (tc-eq (argument ex) var))
			  (cond ((null field)
				 (setq field ex))
				((not (tc-eq field ex))
				 (setq done t))))
			 ((and (typep ex 'name-expr)
			       (tc-eq ex var))
			  (setq done t))))
	       predicate)
    (unless done
      field)))

(defun add-field-type-preds (fields parts &optional nfields)
  (if (null fields)
      (nreverse nfields)
      (let* ((preds (assoc (car fields) parts
			   :test #'(lambda (x y)
				     (and y (eq (id x) (id y))))))
	     (npreds (when preds
		       (make-new-field-type-preds (car fields) preds)))
	     (ntype (type-canon* (type (car fields))
				 (when npreds (list npreds)))))
	(add-field-type-preds (cdr fields) parts
			      (cons (lcopy (car fields) 'type ntype)
				    nfields)))))

(defun make-new-field-type-preds (field predicates)
  (let* ((id (make-new-variable '|x| (cons field predicates)))
	 (te (type field))
	 (bd (typecheck* (mk-bind-decl id te te) nil nil nil))
	 (nvar (mk-name-expr id nil nil (make-resolution bd
					  (current-theory-name) te))))
    (make!-lambda-expr (list bd)
      (make!-conjunction* (gensubst (cdr predicates)
			  #'(lambda (ex) (declare (ignore ex)) nvar)
			  #'(lambda (ex) (tc-eq ex (car predicates))))))))
