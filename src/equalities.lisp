;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; equalities.lisp -- Gives various relations between PVS terms
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2018, SRI International.  All Rights Reserved.

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

;;; This file provides various term relations for PVS:

;;; ps-eq - checks for syntactic equality. This is useful when the
;;;    expressions have not yet been typechecked.  In general, it's
;;;    better to typecheck first, and use tc-eq.
;;; tc-eq - checks for equality, handling dependent bindings and alpha
;;;    equivalence.  It is the primary equality relation for PVS terms.  It
;;;    assumes the terms are typechecked and fully instantiated.
;;; strong-tc-eq - a stricter test than tc-eq, in that it checks tc-eq on the
;;;    print-types of type-exprs, and the parts of names (actuals, etc.)
;;;    instead of just their resolutions.
;;; tc-eq-with-bindings - tc-eq internally uses a bindings argument, used
;;;    for dealing with alpha-equivalence.  This gives the API to methods
;;;    that keep track of their own bindings.  The bindings must be an alist
;;;    with elements "(expr . expr)"  For example tc-eq between
;;;      λ (x: T, y: T'): f(x, y)
;;;      λ (z: [T, T']): f(z`1, z`2)
;;;    yields bindings of the form
;;;      (((x, y) . z) (y . z`2) (x . z`1))
;;;    Most of the functions described below use bindings in this way.
;;; compatible? - checks whether two types are compatible.  This is weaker
;;;    than whether they have a common supertype, as it allows for the
;;;    possibility that conversions or TCCs could allow these types to be
;;;    compared.  This is mostly in function domain types, and actual
;;;    parameters.
;;; strict-compatible? - like compatible? but requires domain types to be tc-eq.
;;; disjoint-types? - checks if two types are known to be disjoint.
;;; compatible-type - tries to return the compatible type.  If there is a
;;;    common supertype, it generally returns the least one.  But with the
;;;    caveats of compatible?.  And it tends to prefer the first
;;;    argument, e.g.,
;;;      (compatible-type set[T] set[(a)]) ==> set[T]
;;;      (compatible-type set[(a)] set[T]) ==> set[(a)]
;;;    and the result is eq to the first arg.
;;; compatible-preds is the list of predicates needed to coerce an
;;;    element of the first type to the second type.  Used primarily for TCC
;;;    generation.
;;; subtype-of? - checks whether one type is a subtype of another.  Uses
;;;    subtype judgements, but otherwise it's essentially syntactic, e.g.,
;;;    there is no relation between {i: int | i > 0} and {i: int | 0 < i}.
;;;    Though types are kept in canonical form (as given by
;;;    pseudonormalize); this example isn't taking that into account.
;;; strict-subtype-of? - like subtype-of?, but the types cannot be tc-eq.
;;; subtype-preds - returns the list of predicates that would have to hold
;;;    of an element of the second type in order for it to belong to the
;;;    first type.  For example,
;;;     (subtype-preds nat int) ==> ({i: int | i >= 0})
;;; subtype-pred - makes a single conjunction of the subtype-preds.
;;; type-canon - tries to canonicalize the types, by lifting predicates as
;;;    high as possible.  For example, [y: real, {x: real | x < y}] is
;;;    canonicalized to {z: [real, real] | z`1 < z`2}.  Note that predicates
;;;    cannot be lifted from domains of function types.  This function is
;;;    not yet completed.
;;; intersection-type - returns the largest type contained in the given types,
;;;    which must be compatible.  It can return the empty type.  Note that
;;;    this never returns a dep-binding.

(in-package :pvs)

(export '(ps-eq tc-eq tc-eq-with-bindings compatible? strict-compatible? compatible-type
	  compatible-preds subtype-of? strict-subtype-of? subtype-preds subtype-pred
	  type-canon intersection-type))

;;; ps-eq is used to determine whether two entities are syntactically equal;
;;; the types and resolutions are ignored.  This is not generally a good
;;; test, especially between a typed term and an untyped one, as the
;;; typechecker can modify the parse tree by adding conversions and macros,
;;; making it difficult to compare something that's merely parsed with
;;; something that's typechecked.  If both terms are typechecked, use tc-eq
;;; instead.  ps-eq basically just unparses both terms to strings, and
;;; compares them.

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

;; (defmethod tc-eq* :around ((x syntax) (y syntax) bindings)
;;   (with-slots ((xhash pvs-sxhash-value)) x
;;     (with-slots ((yhash pvs-sxhash-value)) y
;;       (when (or (null xhash)
;; 		(null yhash)
;; 		(= xhash yhash))
;; 	(call-next-method)))))


(defmethod tc-eq* (x y bindings)
  (or (eq x y)
      (values nil x y bindings)))

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
  (if (eq b1 b2)
      bindings
      (acons b1 b2 bindings)))

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
	       (let ((pt1 (print-type t1))
		     (pt2 (print-type t2)))
		 (and (if pt1
			  (and pt2
			       (tc-eq* pt1 pt2 bindings))
			  (null pt2))
		      (eq (from-conversion t1) (from-conversion t2))))))))

(defmethod tc-eq* ((t1 subtype) (t2 subtype) bindings)
  (with-slots ((st1 supertype) (p1 predicate)) t1
    (with-slots ((st2 supertype) (p2 predicate)) t2
      (if (everywhere-true? p1)
	  (if (everywhere-true? p2)
	      (tc-eq* st1 st2 bindings)
	      (unless *strong-tc-eq-flag*
		(tc-eq* st1 t2 bindings)))
	  (if (everywhere-true? p2)
	      (unless *strong-tc-eq-flag*
		(tc-eq* t1 st2 bindings))
	      (and (tc-eq* st1 st2 bindings)
		   (tc-eq-ops p1 p2 bindings)))))))

(defmethod tc-eq* ((t1 subtype) (t2 type-name) bindings)
  (when (or (everywhere-true? (predicate t1))
	    (and (adt-type-name? (supertype t1))
		 (single-constructor? (supertype t1))
		 (recognizer-name-expr? (predicate t1))))
    (tc-eq* (supertype t1) t2 bindings)))

(defmethod tc-eq* ((t1 type-name) (t2 subtype) bindings)
  (when (or (everywhere-true? (predicate t2))
	    (and (adt-type-name? (supertype t2))
		 (single-constructor? (supertype t2))
		 (recognizer-name-expr? (predicate t2))))
    (tc-eq* t1 (supertype t2) bindings)))

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

(defmethod tc-eq* ((t1 type-application) (t2 type-application) bindings)
  (with-slots ((ty1 type) (p1 parameters)) t1
    (with-slots ((ty2 type) (p2 parameters)) t2
      (and (tc-eq* ty1 ty2 bindings)
	   (tc-eq* p1 p2 bindings)))))


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

(defmethod tc-eq* ((t1 struct-sub-tupletype) (t2 struct-sub-tupletype) bindings)
  (with-slots ((types1 types)) t1
    (with-slots ((types2 types)) t2
      (or (eq t1 t2)
	  (tc-eq-types types1 types2 bindings)))))

(defmethod tc-eq* ((t1 cotupletype) (t2 cotupletype) bindings)
  (with-slots ((ty1 types)) t1
    (with-slots ((ty2 types)) t2
      (or (eq t1 t2)
	  (tc-eq-types ty1 ty2 bindings)))))

;; (defmethod tc-eq* ((t1 tupletype) (t2 subtype) bindings)
;;   ;; handles {z: [t1, t2] | p(z)}  [x: t1, {y: t2 | p(x, y)]
;;   (let ((stype (find-supertype t2)))
;;     (when (and (tupletype? stype)
;; 	       (length= (types stype) (types t1)))
;;       (let* ((nbds (make-new-bind-decls (types stype)))
;; 	     (nvars (mapcar #'make-variable-expr nbds))
;; 	     (ntupex (make!-arg-tuple-expr* nvars))
;; 	     (spred (make!-application (predicate t2) ntupex)))
;; 	(tc-eq-tuptypes (types t1) nbds bindings spred)))))

;; (defun tc-eq-tuptypes (types1 types2 bindings preds1 preds2)
;;   (and 

(defun tc-eq-types (types1 types2 bindings)
  (declare (list types1 types2))
  (cond ((null types1) (null types2))
	((null types2) nil)
	(t (and (tc-eq-bindings (car types1) (car types2) bindings)
		(tc-eq-types (cdr types1) (cdr types2)
			     (new-tc-eq-list-bindings (car types1) (car types2)
						      bindings))))))

(defmethod tc-eq-bindings ((b1 dep-binding) (b2 dep-binding) bindings)
  (and (or (not *strong-tc-eq-flag*)
	   (tc-eq* (declared-type b1) (declared-type b2) bindings))
       (tc-eq* (type b1) (type b2) bindings)))

(defmethod tc-eq-bindings ((b1 dep-binding) b2 bindings)
  (unless *strong-tc-eq-flag*
    (tc-eq* (type b1) b2 bindings)))

(defmethod tc-eq-bindings (b1 (b2 dep-binding) bindings)
  (unless *strong-tc-eq-flag*
    (tc-eq* b1 (type b2) bindings)))

(defmethod tc-eq-bindings ((b1 field-decl) (b2 field-decl) bindings)
  (and (eq (id b1) (id b2))
       (tc-eq* (type b1) (type b2) bindings)))

(defmethod tc-eq-bindings (e1 e2 bindings)
  (tc-eq* e1 e2 bindings))

(defmethod tc-eq* ((t1 recordtype) (t2 recordtype) bindings)
  (with-slots ((fields1 fields)) t1
    (with-slots ((fields2 fields)) t2
      (or (eq t1 t2)
	  (tc-eq-fields-type fields1 fields2 bindings)))))

(defmethod tc-eq* ((t1 struct-sub-recordtype) (t2 struct-sub-recordtype) bindings)
  (with-slots ((fields1 fields)) t1
    (with-slots ((fields2 fields)) t2
      (or (eq t1 t2)
	  (tc-eq-fields-type fields1 fields2 bindings)))))

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
  (declare (list flds1 flds2))
  (cond ((null flds1) (null flds2))
	((null flds2) nil)
	(t (flet ((ftest (x y)
		    (same-id (caar (arguments x)) (caar (arguments y)))))
	     (let ((fld2 (find (car flds1) flds2 :test #'ftest)))
	       (and fld2
		    (tc-eq* (expression (car flds1)) (expression fld2)
			    bindings)
		    (tc-eq-fields (cdr flds1) (remove fld2 flds2)
				  bindings)))))))

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

(defmethod tc-eq* ((e1 projection-expr) (e2 projection-expr) bindings)
  (with-slots ((id1 index) (ty1 type)) e1
    (with-slots ((id2 index) (ty2 type)) e2
      (or (eq e1 e2)
	  (and (= id1 id2)
	       (tc-eq* ty1 ty2 bindings))))))

(defmethod tc-eq* ((e1 injection-expr) (e2 injection-expr) bindings)
  (with-slots ((id1 index) (ty1 type)) e1
    (with-slots ((id2 index) (ty2 type)) e2
      (or (eq e1 e2)
	  (and (= id1 id2)
	       (tc-eq* ty1 ty2 bindings))))))

(defmethod tc-eq* ((e1 injection?-expr) (e2 injection?-expr) bindings)
  (with-slots ((id1 index) (ty1 type)) e1
    (with-slots ((id2 index) (ty2 type)) e2
      (or (eq e1 e2)
	  (and (= id1 id2)
	       (tc-eq* ty1 ty2 bindings))))))

(defmethod tc-eq* ((e1 extraction-expr) (e2 extraction-expr) bindings)
  (with-slots ((id1 index) (ty1 type)) e1
    (with-slots ((id2 index) (ty2 type)) e2
      (or (eq e1 e2)
	  (and (= id1 id2)
	       (tc-eq* ty1 ty2 bindings))))))

(defmethod tc-eq* ((e1 projection-application) (e2 projection-application)
		   bindings)
  (with-slots ((id1 index) (arg1 argument) (ty1 type)) e1
    (with-slots ((id2 index) (arg2 argument) (ty2 type)) e2
      (or (eq e1 e2)
	  (assert (and id1 id2))
	  (and (= id1 id2)
	       (tc-eq* ty1 ty2 bindings)
	       (tc-eq* arg1 arg2 bindings))))))

(defmethod tc-eq* ((e1 projection-application) (e2 name-expr) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 name-expr) (e2 projection-application) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 injection-application) (e2 injection-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 index) (arg1 argument)) e1
	(with-slots ((id2 index) (arg2 argument)) e2
	  (assert (and id1 id2))
	  (and (= id1 id2)
	       (tc-eq* arg1 arg2 bindings))))))

(defmethod tc-eq* ((e1 injection-application) (e2 name-expr) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 name-expr) (e2 injection-application) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 injection?-application) (e2 injection?-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 index) (arg1 argument)) e1
	(with-slots ((id2 index) (arg2 argument)) e2
	  (assert (and id1 id2))
	  (and (= id1 id2)
	       (tc-eq* arg1 arg2 bindings))))))

(defmethod tc-eq* ((e1 injection?-application) (e2 name-expr) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 name-expr) (e2 injection?-application) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 extraction-application) (e2 extraction-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 index) (arg1 argument)) e1
	(with-slots ((id2 index) (arg2 argument)) e2
	  (assert (and id1 id2))
	  (and (= id1 id2)
	       (tc-eq* arg1 arg2 bindings))))))

(defmethod tc-eq* ((e1 extraction-application) (e2 name-expr) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 name-expr) (e2 extraction-application) bindings)
  (let ((bind (assoc e1 bindings :test #'tc-eq)))
    (and bind
	 (tc-eq (cdr bind) e2))))

(defmethod tc-eq* ((e1 field-application) (e2 field-application)
		   bindings)
  (or (eq e1 e2)
      (with-slots ((id1 id) (arg1 argument) (ty1 type)) e1
	(with-slots ((id2 id) (arg2 argument) (ty2 type)) e2
	  (and (eq id1 id2)
	       (tc-eq* arg1 arg2 bindings)
	       ;; tc-eq* doesn't work - should chase down why
	       ;; see examples/dpll/dpll3_4.pvs for why next line is needed
	       (eq (fully-instantiated? ty1) (fully-instantiated? ty2))
	       (compatible? ty1 ty2))))))


(defmethod tc-eq* ((e1 rational-expr) (e2 rational-expr) bindings)
  (declare (ignore bindings))
  (with-slots ((n1 number)) e1
    (with-slots ((n2 number)) e2
      (= n1 n2))))

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
  (declare (list sel1 sel2))
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
  (let* ((stype (find-supertype (type e1))))
    (if (funtype? stype)
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
      (tc-eq* (collect-conjuncts e1)
	      (collect-conjuncts e2)
	      bindings)))

(defmethod collect-conjuncts ((ex conjunction))
  (nconc (collect-conjuncts (args1 ex))
	 (collect-conjuncts (args2 ex))))

(defmethod collect-conjuncts (ex)
  (list ex))

(defmethod tc-eq* ((e1 conjunction) (e2 expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 application) (e2 conjunction) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((e1 disjunction) (e2 disjunction) bindings)
  (or (eq e1 e2)
      (tc-eq* (collect-disjuncts e1)
	      (collect-disjuncts e2)
	      bindings)))

(defmethod collect-disjuncts ((ex disjunction))
  (nconc (collect-disjuncts (args1 ex))
	 (collect-disjuncts (args2 ex))))

(defmethod collect-disjuncts (ex)
  (list ex))

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
  (with-slots ((arg1 argument)) e1
    (with-slots ((arg2 argument)) e2
      (or (eq e1 e2)
	  (and (or (not *strong-tc-eq-flag*)
		   (tc-eq* (operator e1) (operator e2) bindings))
	       (tc-eq* arg1 arg2 bindings))))))

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
      (with-slots ((op1 operator) (arg1 argument) (ty1 type)) e1
	(with-slots ((op2 operator) (arg2 argument) (ty2 type)) e2
	  (assert ty1)
	  (and (tc-eq* arg1 arg2 bindings)
	       (if (funtype? (find-supertype ty1))
		   (tc-eq* op1 op2 bindings)
		   (tc-eq-ops op1 op2 bindings)))))))

(defmethod tc-eq* ((e1 list-expr) (e2 list-expr) bindings)
  (and (tc-eq* (args1 e1) (args1 e2) bindings)
       (tc-eq* (args2 e1) (args2 e2) bindings)))

(defmethod tc-eq* ((e1 application) (e2 rational-expr) bindings)
  (declare (ignore bindings))
  (and (not *use-rationals*) ;; Don't match if trying to convert in assert
       (is-division? e1)
       (number-expr? (args1 e1))
       (number-expr? (args2 e1))
       (not (zerop (number (args2 e1))))
       (= (/ (number (args1 e1)) (number (args2 e1))) (number e2))))

(defmethod tc-eq* ((e1 rational-expr) (e2 application) bindings)
  (tc-eq* e2 e1 bindings))

(defmethod tc-eq* ((e1 unary-application) (e2 rational-expr) bindings)
  (declare (ignore bindings))
  (and (name-expr? (operator e1))
       (eq (id (operator e1)) '-)
       (eq (id (module (declaration (operator e1)))) '|number_fields|)
       (number-expr? (argument e1))
       (= (number (argument e1)) (- (number e2)))))

(defmethod tc-eq* ((e1 rational-expr) (e2 unary-application) bindings)
  (tc-eq* e2 e1 bindings))

(defmethod tc-eq* ((e1 name-expr) (e2 name-expr-from-number) bindings)
  (declare (ignore bindings))
  (call-next-method))

(defmethod tc-eq* ((e1 name-expr-from-number) (e2 name-expr) bindings)
  (declare (ignore bindings))
  (call-next-method))

(defmethod tc-eq-ops ((op1 field-name-expr) (op2 field-name-expr)
		      &optional bindings)
  (with-slots ((id1 id) (ty1 type)) op1
    (with-slots ((id2 id) (ty2 type)) op2
      (and (eq id1 id2)
	   (or (null *strong-tc-eq-flag*)
	       (eq (declaration op1) (declaration op2)))
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

(defmethod tc-eq-ops ((op1 injection?-expr) (op2 injection?-expr)
		      &optional bindings)
  (tc-eq* op1 op2 bindings))

(defmethod tc-eq-ops ((op1 accessor-name-expr) (op2 accessor-name-expr)
		      &optional bindings)
  (tc-eq-adt-ops op1 op2 bindings))

(defmethod tc-eq-ops ((op1 name-expr) (op2 name-expr) &optional bindings)
  (let ((decl (declaration op1)))
    (if (and (eq decl (declaration op2))
	     (const-decl? decl)
	     (listp (positive-types decl))
	     (not (every #'null (positive-types decl))))
	(let* ((mi1 (module-instance (resolution op1)))
	       (mi2 (module-instance (resolution op2)))
	       (act1 (unless (eq (module decl) (current-theory)) (actuals mi1)))
	       (act2 (unless (eq (module decl) (current-theory)) (actuals mi2)))
	       (fmls (formals-sans-usings (module decl)))
	       (ptypes (unless *strong-tc-eq-flag* (positive-types decl))))
	  (and (tc-eq-adt-op-actuals act1 act2 bindings fmls ptypes)
	       (tc-eq-adt-op-actuals (dactuals mi1) (dactuals mi2) bindings
				     fmls ptypes)))
	(call-next-method))))

(defmethod tc-eq-ops ((op1 application) (op2 application) &optional bindings)
  (and (tc-eq* (argument op1) (argument op2) bindings)
       (tc-eq-ops (operator op1) (operator op2) bindings)))

(defmethod tc-eq-ops (op1 op2 &optional bindings)
  (tc-eq* op1 op2 bindings))

(defun tc-eq-adt-ops (op1 op2 bindings)
  (and (name-eq (id op1) (id op2))
       (let ((adt1 (adt op1))
	     (adt2 (adt op2)))
	 (and (eq (adt adt1) (adt adt2))
	      (let ((mi1 (module-instance
			  (resolution
			   (or (print-type adt1) adt1))))
		    (mi2 (module-instance
			  (resolution
			   (or (print-type adt2) adt2)))))
		(or (and (null (actuals mi1)) (null (actuals mi2)))
		    (let ((fmls (formals-sans-usings (adt adt1)))
			  (ptypes (unless *strong-tc-eq-flag*
				    (positive-types (adt adt1)))))
		      (and (tc-eq-adt-op-actuals (actuals mi1) (actuals mi2) bindings
						 fmls ptypes)
			   (tc-eq-adt-op-actuals (dactuals mi1) (dactuals mi2) bindings
						 fmls ptypes)))))))))

(defun tc-eq-adt-op-actuals (acts1 acts2 bindings fmls ptypes)
  (cond ((null acts1) (null acts2))
	((null acts2) nil)
	(t (tc-eq-adt-actuals acts1 acts2 bindings fmls ptypes))))

(defun tc-eq-adt-actuals (acts1 acts2 bindings formals postypes)
  (or (null acts1)
      (flet ((lkey (x) (or (print-type x) x)))
	(and (if (member (car formals) postypes :test #'name-eq :key #'lkey)
		 (compatible? (type-value (car acts1)) (type-value (car acts2)))
		 (tc-eq* (car acts1) (car acts2) bindings))
	     (tc-eq-adt-actuals (cdr acts1) (cdr acts2) bindings
				(cdr formals) postypes)))))

;;; Should consider whether all binding-exprs are over a single bound
;;; variable; i.e., the canonical form of FORALL (x:t1), (y:t2): p(x,y) is
;;; FORALL (z:[t1,t2]): p(PROJ_1(z),PROJ_2(z))

(defmethod tc-eq* ((e1 binding-expr) (e2 binding-expr) ibindings)
  (with-slots ((b1 bindings) (ex1 expression) (ty1 type)) e1
    (with-slots ((b2 bindings) (ex2 expression) (ty2 type)) e2
      (or (eq e1 e2)
	  (and (same-binding-op? e1 e2)
	       (multiple-value-bind (eq? abindings)
		   (bindings-eq b1 b2 ibindings)
		 (and eq?
		      (tc-eq* ex1 ex2 abindings)
		      (or (not *strong-tc-eq-flag*)
			  (tc-eq ty1 ty2)))))))))

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

;;; make-compatible-bindings takes two bindings lists b1 and b2 (e.g., from
;;; two lambda exprs) and a bindings alist, and updates the bindings alist
;;; accordingly.  The trickiness here is with things like
;;;   "λ (x: T, y: T'): f(x, y)", which is tc-eq to
;;;   "λ (z: [T, T']): f(z`1, z`2)".
;;; Dependent types 

(defun make-compatible-bindings (b1 b2 bindings)
  (declare (list b1 b2))
  (if (and (singleton? b1)
	   (typep (type (car b1)) 'tupletype))
      (let ((types1 (types (type (car b1)))))
	(declare (list types1))
	(when (and (length= types1 b2)
		   (tc-eq* (if (dep-binding? (car types1))
			       (type (car types1))
			       (car types1))
			   (type (car b2))
			   bindings)
		   (compatible-binding-types? (cdr b2) (cdr types1))
		   (tc-eq-types (the list types1)
				(bindings-to-types b2 types1)
				bindings))
	  (make-compatible-bindings* b1 b2 bindings nil)))
      (when (and (singleton? b2)
		 (typep (type (car b2)) 'tupletype))
	(let ((types2 (types (type (car b2)))))
	  (when (and (length= b1 (the list types2))
		     (tc-eq* (type (car b1))
			     (if (dep-binding? (car types2))
				 (type (car types2))
				 (car types2))
			     bindings)
		     (compatible-binding-types? (cdr b1) (cdr types2))
		     (tc-eq-types (bindings-to-types b1 types2)
				  (the list types2)
				  bindings))
	    (make-compatible-bindings* b2 b1 bindings t))))))

(defun compatible-binding-types? (bindings types)
  (or (null bindings)
      (and (compatible? (type (car bindings)) (car types))
	   (compatible-binding-types? (cdr bindings) (cdr types)))))

(defun make-compatible-bindings* (b1 b2 bindings rev?)
  (let* ((var (make-variable-expr (car b1)))
	 (projs (make!-projections var))
	 (vars (mapcar #'make-variable-expr b2))
	 (tup (make!-tuple-expr vars)))
    (nconc (if rev?
	       (acons tup var (pairlis vars projs))
	       (acons var tup (pairlis projs vars)))
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

;;; The name prover command introduces a skolem constant with
;;; a definition.  We check this in the next two methods
;;; However, when in replace, we don't want to do this, since
;;; the replace is not done if the replacement is tc-eq to the
;;; original.  Using *replace-cache* as a proxy for being in
;;; replace.  rewrite doesn't seem to do this, so no check for
;;; in rewrite.

;; (defmethod tc-eq* ((n1 name-expr) (e2 expr) bindings)
;;   (if (name-expr? e2)
;;       (call-next-method)
;;       ;; (when (and (not *replace-cache*)
;;       ;; 		 (skolem-const-decl? (declaration n1)))
;;       ;; 	(tc-eq* (definition (declaration n1)) e2 bindings))
;;       ))

;; (defmethod tc-eq* ((e1 expr) (n2 name-expr) bindings)
;;   (if (name-expr? e1)
;;       (call-next-method)
;;       (when (and (not *replace-cache*)
;; 		 (skolem-const-decl? (declaration n2)))
;; 	(tc-eq* e1 (definition (declaration n2)) bindings))))

;;; Make sure we don't allow bindings and names to be tc-eq*

(defmethod tc-eq* ((n1 binding) (n2 name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 binding) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 modname) bindings)
  (when (and (resolution n1)
	     (resolution n2))
    (tc-eq* (resolution n1) (resolution n2) bindings)))

(defmethod tc-eq* ((n1 modname) (n2 name) bindings)
  (when (and (resolution n1)
	     (resolution n2))
    (tc-eq* (resolution n1) (resolution n2) bindings)))

(defmethod tc-eq* ((n1 name-expr) (n2 type-name) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 type-name) (n2 name-expr) bindings)
  (declare (ignore bindings))
  nil)

(defmethod tc-eq* ((n1 name) (n2 name) bindings)
  (with-slots ((id1 id) (res1 resolutions) (mi1 mod-id) (l1 library)
	       (act1 actuals) (dact1 dactuals) (m1 mappings) (t1 target)) n1
    (with-slots ((id2 id) (res2 resolutions) (mi2 mod-id) (l2 library)
		 (act2 actuals) (dact2 dactuals) (m2 mappings) (t2 target)) n2
      (or (eq n1 n2)
	  (and (or (not *strong-tc-eq-flag*)
		   (and (eq mi1 mi2)
			(eq l1 l2)
			(tc-eq* act1 act2 bindings)
			(tc-eq* dact1 dact2 bindings)
			(tc-eq* m1 m2 bindings)
			(tc-eq* t1 t2 bindings)))
	       (tc-eq* (car res1) (car res2) bindings))
	  (values nil n1 n2 bindings)))))

(defvar *in-tc-eq-resolution* nil)

(defmethod tc-eq* ((res1 resolution) (res2 resolution) bindings)
  (with-slots ((decl1 declaration) (mi1 module-instance) (type1 type)) res1
    (with-slots ((decl2 declaration) (mi2 module-instance) (type2 type)) res2
      (or (let ((bind (cdr (assq decl1 bindings))))
	    (and bind
		 (eq decl2 (if (consp bind) (car bind) bind))))
	  (and (or (eq decl1 decl2)
		   (and *allow-var-decl-comparison*
			(cond ((skolem-const-decl? decl1)
			       (typep decl2 '(or binding var-decl)))
			      ((skolem-const-decl? decl2)
			       (typep decl1 '(or binding var-decl)))
			      (t (and (name-eq (id decl1) (id decl2))
				      (typep decl1 '(or binding var-decl))
				      (typep decl2 '(or binding var-decl)))))
			(tc-eq* type1 type2 bindings)))
	       (and ;;(binding? decl1)
		(if mi1
		    (and mi2
			 (or (null (library mi1))
			     (null (library mi2))
			     (eq (library mi1) (library mi2)))
			 ;; Ignore actuals for the current theory
			 (let ((act1 (unless (eq (module decl1) (current-theory))
				       (actuals mi1)))
			       (act2 (unless (eq (module decl2) (current-theory))
				       (actuals mi2))))
			   (tc-eq* act1 act2 bindings))
			 (tc-eq* (dactuals mi1) (dactuals mi2) bindings)
			 (tc-eq* (mappings mi1) (mappings mi2) bindings))
		    (null mi2))
		(or *in-tc-eq-resolution*
		    (null *strong-tc-eq-flag*)
		    (let ((*in-tc-eq-resolution* t)
			  (*strong-tc-eq-flag* nil))
		      (tc-eq* (type res1) (type res2) bindings)))
		;; (progn (let ((*strong-tc-eq-flag* nil))
		;; 	 (assert (tc-eq* (type res1) (type res2) bindings)))
		;;        t)
		))))))

(defmethod tc-eq* ((n1 modname) (n2 modname) bindings)
  (with-slots ((id1 id) (lib1 library) (act1 actuals)
	       (dacts1 dactuals) (map1 mappings)) n1
    (with-slots ((id2 id) (lib2 library) (act2 actuals)
		 (dacts2 dactuals) (map2 mappings)) n2
      (or (eq n1 n2)
	  (and (eq id1 id2)
	       (or (eq lib1 lib2)
		   (if (resolution n1)
		       (if (resolution n2)
			   (eq (declaration n1) (declaration n2))
			   (eq (declaration n1) (get-theory n2)))
		       (when (resolution n2)
			 (eq (get-theory n1) (declaration n2)))))
	       (tc-eq* act1 act2 bindings)
	       (tc-eq* dacts1 dacts2 bindings)
	       (tc-eq* map1 map2 bindings))))))

(defmethod tc-eq* ((imp1 importing-entity) (imp2 importing-entity) bindings)
  (tc-eq* (theory-name imp1) (theory-name imp2) bindings))

(defmethod tc-eq* ((a1 actual) (a2 actual) bindings)
  (or (eq a1 a2)
      (with-slots ((tv1 type-value) (ex1 expr)) a1
	(with-slots ((tv2 type-value) (ex2 expr)) a2
	  (if tv1
	      (and tv2
		   (tc-eq* tv1 tv2 bindings))
	      (and (not tv2)
		   (tc-eq* ex1 ex2 bindings)))))))

(defmethod tc-eq* ((map1 mapping) (map2 mapping) bindings)
  (with-slots ((lhs1 lhs) (rhs1 rhs)) map1
    (with-slots ((lhs2 lhs) (rhs2 rhs)) map2
      (and (tc-eq* lhs1 lhs2 bindings)
	   (tc-eq* rhs1 rhs2 bindings)))))

(defmethod tc-eq* ((rhs1 mapping-rhs) (rhs2 mapping-rhs) bindings)
  (with-slots ((ex1 expr) (ty1 type-value)) rhs1
    (with-slots ((ex2 expr) (ty2 type-value)) rhs2
      (if ty1
	  (tc-eq* ty1 ty2 bindings)
	  (and (null ty2)
	       (tc-eq* ex1 ex2 bindings))))))


;;; Compatible? for types checks whether two types have a common supertype.
;;; Note that this is used during typechecking, and tends to succeed for
;;; types involving free parameters.  It doesn't check for consistency, so
;;; compatible-type may fail even when this succeeds, unless both types are
;;; fully-instantiated.

(defmethod compatible? ((res1 resolution) (res2 resolution))
  (compatible? (type res1) (type res2)))

(defmethod compatible? ((atype list) (etype list))
  (cond ((null atype) (null etype))
	((null etype) nil)
	(t (and (compatible? (car atype) (car etype))
		(compatible? (cdr atype) (cdr etype))))))

(defmethod compatible? ((atype type-expr) (etype type-expr))
  (compatible?* (find-supertype atype) (find-supertype etype)))

(defmethod compatible? ((atype dep-binding) (etype type-expr))
  (compatible?* (find-supertype atype) (find-supertype etype)))

(defmethod compatible? ((atype type-expr) (etype dep-binding))
  (compatible?* (find-supertype atype) (find-supertype etype)))

(defmethod compatible? ((atype dep-binding) (etype dep-binding))
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

(defmethod compatible?* ((atype type-expr) (etype dep-binding))
  (compatible?* atype (type etype)))

(defmethod compatible? ((aname modname) (ename modname))
  (assert (fully-instantiated? aname))
  (assert (fully-instantiated? ename))
  (and (eq (id aname) (id ename))
       (compatible? (actuals aname) (actuals ename))
       (compatible? (dactuals aname) (dactuals ename))
       (tc-eq (mappings aname) (mappings ename))))

;;; We would like to use tc-eq for type-names, but since this can be
;;; called from tc-unify, the actuals may not be known yet.  Note that
;;; we return the instantiated type, if there is one.

(defmethod compatible?* ((atype type-name) (etype type-name))
  #+pvsdebug (assert (and (resolution atype) (resolution etype)))
  (or (declaration-is-not-in-context etype)
      (declaration-is-not-in-context atype)
      (let* ((mi1 (module-instance atype))
	     (mi2 (module-instance etype))
	     (a1 (actuals mi1))
	     (a2 (actuals mi2))
	     (da1 (dactuals mi1))
	     (da2 (dactuals mi2)))
	(and (eq (id atype) (id etype))
	     (eq (id mi1) (id mi2))
	     ;;(tc-eq (module-instance atype) (module-instance etype))
	     (or (null a1)
		 (null a2)
		 (compatible?* a1 a2))
	     (or (null da1)
		 (null da2)
		 (compatible?* da1 da2))))))

(defmethod compatible?* ((atype type-name) etype)
  (declare (ignore etype))
  (or (formal-not-in-context? (declaration atype))
      (call-next-method)))

(defmethod compatible?* ((atype type-expr) (etype type-name))
  (or (formal-not-in-context? (declaration etype))
      (call-next-method)))

(defmethod compatible?* (atype (etype type-name))
  (declare (ignore atype))
  (or (formal-not-in-context? (declaration etype))
      (call-next-method)))

(defun declaration-is-not-in-context (type-name)
  (unless (or *strong-tc-eq-flag*
	      (type-var? type-name))
    (formal-not-in-context? (declaration type-name))))

(defmethod compatible?* ((atype type-var) (etype dep-binding))
  (compatible?* atype (type etype)))

(defmethod compatible?* ((atype rec-type-variable) (etype recordtype))
  t)

(defmethod compatible?* ((etype recordtype) (atype rec-type-variable))
  t)

(defmethod compatible?* ((atype field-type-variable) (etype type-expr))
  t)

(defmethod compatible?* :around ((etype type-expr) (atype field-type-variable))
  t)

(defmethod compatible?* ((atype tup-type-variable) (etype tupletype))
  t)

(defmethod compatible?* ((etype tupletype) (atype tup-type-variable))
  t)

(defmethod compatible?* ((atype proj-type-variable) (etype type-expr))
  t)

(defmethod compatible?* :around ((etype type-expr) (atype proj-type-variable))
  t)

(defmethod compatible?* ((atype cotup-type-variable) (etype cotupletype))
  t)

(defmethod compatible?* ((etype cotupletype) (atype cotup-type-variable))
  t)

(defmethod compatible?* ((atype out-type-variable) (etype type-expr))
  t)

(defmethod compatible?* :around ((atype type-expr) (etype out-type-variable))
  t)

;; Need to deal with subtypes
(defmethod compatible?* ((atype tup-type-variable) (etype subtype))
  (compatible?* atype (find-supertype etype)))

(defmethod compatible?* ((atype subtype) (etype tup-type-variable))
  (compatible?* (find-supertype atype) etype))

(defmethod compatible?* ((atype proj-type-variable) (etype subtype))
  (compatible?* atype (find-supertype etype)))

(defmethod compatible?* :around ((atype subtype) (etype proj-type-variable))
  (compatible?* (find-supertype atype) etype))

(defmethod compatible?* ((atype cotup-type-variable) (etype subtype))
  (compatible?* atype (find-supertype etype)))

(defmethod compatible?* ((etype subtype) (atype cotup-type-variable))
  (compatible?* (find-supertype atype) etype))


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

(defmethod compatible?* ((atype tupletype) (etype struct-sub-tupletype))
  (when (>= (length (types atype)) (length (types etype)))
    (every #'compatible?* (types atype) (types etype))))

(defmethod compatible?* ((atype struct-sub-tupletype) (etype tupletype))
  (when (<= (length (types atype)) (length (types etype)))
    (every #'compatible?* (types atype) (types etype))))

(defmethod compatible?* ((atype struct-sub-tupletype)
			 (etype struct-sub-tupletype))
  (every #'compatible?* (types atype) (types etype)))
  

(defmethod compatible?* ((atype cotupletype) (etype cotupletype))
  (and (length= (types atype) (types etype))
       (every #'compatible?* (types atype) (types etype))))

(defmethod compatible?* ((atype recordtype) (etype recordtype))
  (and (length= (fields atype) (fields etype))
       (compatible-fields? (fields atype) (fields etype))))

(defmethod compatible?* ((atype recordtype)
			 (etype struct-sub-recordtype))
  (when (>= (length (fields atype)) (length (fields etype)))
    (compatible-fields? (fields etype) (fields atype))))

(defmethod compatible?* ((atype struct-sub-recordtype)
			 (etype recordtype))
  (when (<= (length (fields atype)) (length (fields etype)))
    (compatible-fields? (fields atype) (fields etype))))

(defmethod compatible?* ((atype struct-sub-recordtype)
			 (etype struct-sub-recordtype))
  (if (<= (length (fields atype)) (length (fields etype)))
      (compatible-fields? (fields atype) (fields etype))
      (compatible-fields? (fields etype) (fields atype))))

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
  (or (declaration-is-not-in-context etype)
      (declaration-is-not-in-context atype)
      (let* ((mi1 (module-instance atype))
	     (mi2 (module-instance etype))
	     (a1 (actuals mi1))
	     (a2 (actuals mi2))
	     (da1 (dactuals mi1))
	     (da2 (dactuals mi2)))
	(and (eq (id atype) (id etype))
	     (eq (id mi1) (id mi2))
	     ;;(tc-eq (module-instance atype) (module-instance etype))
	     (or (null a1)
		 (null a2)
		 (strict-compatible?* a1 a2 bindings))
	     (or (null da1)
		 (null da2)
		 (strict-compatible?* da1 da2 bindings))))))

(defmethod strict-compatible?* ((l1 list) (l2 list) bindings)
  (and (length= l1 l2)
       (every #'(lambda (x y) (strict-compatible?* x y bindings)) l1 l2)))

(defmethod strict-compatible?* ((a1 actual) (a2 actual) bindings)
  (if (type-value a1)
      (and (type-value a2)
	   (strict-compatible?* (type-value a1) (type-value a2) bindings)
	   ;;(tc-eq-with-bindings (type-value a1) (type-value a2) bindings)
	   )
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

(defmethod strict-compatible?* ((atype dep-binding) (etype type-expr) bindings)
  (strict-compatible?* (type atype) etype bindings))

(defmethod strict-compatible?* ((atype type-expr) (etype dep-binding) bindings)
  (strict-compatible?* atype (type etype) bindings))


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

(defmethod strict-compatible?* ((atype cotupletype) (etype cotupletype) bindings)
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


;;; disjoint-types? returns t if the two types are known to be disjoint.
;;; Currently only recognizes disjointness of types involving recognizers.
;;; In the long run it will use a disjoint judgement as well.

(defun disjoint-types? (t1 t2)
  (disjoint-types?* t1 t2))

(defmethod disjoint-types?* ((t1 subtype) (t2 subtype))
  (let ((recs1 (collect-recognizer-preds t1)))
    (and recs1
	 (let ((recs2 (collect-recognizer-preds t2)))
	   (and recs2
		(null (intersection recs1 recs2 :test #'same-id)))))))

(defmethod disjoint-types?* (t1 t2)
  (declare (ignore t1 t2))
  nil)

(defun collect-recognizer-preds (type)
  (when (subtype? type)
    (typecase (predicate type)
      (lambda-expr (let ((disjuncts
			  (or+ (list (expression (predicate type)))))
			 (bd (car (bindings (predicate type)))))
		     (when (every #'(lambda (d)
				      (and (recognizer-application? d)
					   (name-expr? (argument d))
					   (eq (declaration (argument d)) bd)))
				  disjuncts)
		       (mapcar #'operator disjuncts))))
      (name-expr (when (recognizer? (predicate type))
		   (list (predicate type)))))))


;;; Compatible-Type for types checks whether two types have a common
;;; supertype.  In simple cases, it returns the least common supertype, but
;;; when that involves dependent types, etc. it tends to return the first
;;; argument.

(defun compatible-type (t1 t2 &optional bindings)
  (when (compatible? t1 t2)
    (let ((ctype (compatible-type* t1 t2 bindings)))
      ;; This isn't true; subtype-of? is strict on function domain types,
      ;; while this returns the largest domain type.
      ;; (assert (or (null ctype)
      ;; 		  (and (subtype-of? t1 ctype)
      ;; 		       (subtype-of? t2 ctype))))
      ctype)))

(defmethod compatible-type* :around ((atype type-expr) (etype type-expr)
				     bindings)
  (if (tc-eq-with-bindings atype etype bindings)
      atype
      (call-next-method)))

(defmethod compatible-type* ((atype dep-binding) etype bindings)
  (let ((stype (compatible-type* (type atype) etype bindings)))
    (if (tc-eq-with-bindings stype (type atype) bindings)
	atype
	(mk-dep-binding (id atype) stype))))

(defmethod compatible-type* (atype (etype dep-binding) bindings)
  (compatible-type* atype (type etype) bindings))

(defmethod compatible-type* ((atype rec-type-variable) (etype recordtype)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* ((atype recordtype) (etype rec-type-variable)
			     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype field-type-variable) (etype type-expr)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* :around ((atype type-expr) (etype field-type-variable)
				     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype tup-type-variable) (etype tupletype)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* ((atype tupletype) (etype tup-type-variable)
			     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype proj-type-variable) (etype type-expr)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* :around ((atype type-expr) (etype proj-type-variable)
				     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype cotup-type-variable) (etype cotupletype)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* ((atype cotupletype) (etype cotup-type-variable)
			     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype out-type-variable) (etype type-expr)
			     bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* :around ((atype type-expr) (etype out-type-variable)
				     bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype struct-sub-tupletype)
			     (etype tuple-or-struct-subtype) bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* ((atype tuple-or-struct-subtype)
			     (etype struct-sub-tupletype) bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype struct-sub-recordtype)
			     (etype record-or-struct-subtype) bindings)
  (declare (ignore bindings))
  atype)

(defmethod compatible-type* ((atype record-or-struct-subtype)
			     (etype struct-sub-recordtype) bindings)
  (declare (ignore bindings))
  etype)

(defmethod compatible-type* ((atype type-name) (etype type-name) bindings)
  (assert (resolution atype))
  (assert (resolution etype))
  (if (or (tc-eq atype (type (resolution atype)))
	  (tc-eq etype (type (resolution etype))))
      (let* ((a1 (actuals atype))
	     (a2 (actuals etype)))
	(cond ((and a1 a2)
	       (if (fully-instantiated? atype)
		   (if (and (fully-instantiated? etype)
			    (every #'(lambda (aa1 aa2)
				       (or (tc-eq aa1 aa2)
					   (and (type-value aa1)
						(type-value aa2)
						(simple-subtype-of? (type-value aa1)
							     (type-value aa2)))))
				   a1 a2))
		       etype
		       atype)
		   etype))
	      ((or a1 a2)
	       (if a1 atype etype))
	      ((declaration-is-not-in-context atype)
	       (if (declaration-is-not-in-context etype)
		   atype
		   etype))
	      (t atype)))
      (let ((cres (compatible-type* (resolution atype) (resolution etype) bindings)))
	(cond ((eq cres (resolution atype))
	       atype)
	      ((eq cres (resolution etype))
	       etype)
	      (t (lcopy atype
		   :actuals (actuals (module-instance cres))
		   :dactuals (dactuals (module-instance cres))
		   :mappings (mappings (module-instance cres))
		   :resolutons (list cres)))))))

(defmethod compatible-type* ((ares resolution) (eres resolution) bindings)
  (let ((adecl (declaration ares))
	(edecl (declaration eres)))
    (cond ((declaration-is-not-in-context edecl)
	   ares)
	  ((declaration-is-not-in-context adecl)
	   eres)
	  (t (let* ((athinst (module-instance ares))
		    (ethinst (module-instance eres))
		    (cthinst (compatible-type* athinst ethinst bindings)))
	       (assert (eq adecl edecl))
	       (if (eq cthinst athinst)
		   ares
		   (let ((ctype (compatible-type* (type ares) (type eres) bindings)))
		     (lcopy ares
		       :declaration adecl
		       :module-instance cthinst
		       :type ctype))))))))

(defmethod compatible-type* ((aname modname) (ename modname) bindings)
  (assert (eq (id aname) (id ename)))
  #+pvsdebug
  (assert (tc-eq (mappings aname) (mappings ename)))
  ;; (when (or (resolution aname) (resolution ename))
  ;;   (break "Deal with resolutions"))
  (let* ((a1 (actuals aname))
	 (a2 (actuals ename))
	 (da1 (dactuals aname))
	 (da2 (dactuals ename))
	 (cact (if (null a1)
		   a2
		   (if (null a2)
		       a1
		       (compatible-type* a1 a2 bindings))))
	 (cdact (if (null da1)
		    da2
		    (if (null da2)
			da1
			(compatible-type* da1 da2 bindings)))))
    (cond ((and (equal cact a1) (equal cdact da1))
	   aname)
	  ((and (equal cact a2) (equal cdact da2))
	   ename)
	  (t (lcopy aname :actuals cact :dactuals cdact)))))

(defmethod compatible-type* ((alist list) (elist list) bindings)
  #+pvsdebug
  (assert (= (length alist) (length elist)))
  (let ((clist (compatible-type*-list alist elist bindings)))
    ;; Make sure unchanged results are eq
    (cond ((equal clist alist)
	   alist)
	  ((equal clist elist)
	   elist)
	  (t clist))))

(defun compatible-type*-list (alist elist bindings &optional clist)
  (if (null alist)
      (nreverse clist)
      (let ((celt (compatible-type* (car alist) (car elist) bindings)))
	(compatible-type*-list (cdr alist) (cdr elist) bindings
			       (cons celt clist)))))

(defmethod compatible-type* ((act1 actual) (act2 actual) bindings)
  ;; Note that if the type-values are null compatible, only returns t
  ;; if the actuals are tc-eq
  (let ((ty1 (type-value act1))
	(ty2 (type-value act2)))
    (assert (iff ty1 ty2))
    (cond ((or ty1 ty2)
	   #+pvsdebug
	   (assert (or ty1
		       (tc-eq act1 act2)
		       ;; FIXME - Not quite right; acts could be expressions
		       ;; that have formals inside, e.g., act1 = f(3), act2 = f(C)
		       ;; where C is a formal parameter.
		       ;; Probably need to define compatible-expr for this
		       (formal-not-in-context? act1)
		       (formal-not-in-context? act2)))
	   (cond ((formal-not-in-context? act2)
		  act1)
		 ((formal-not-in-context? act1)
		  act2)
		 (t (let ((cty (compatible-type* ty1 ty2 bindings)))
		      (assert cty)
		      (cond ((eq cty ty1) act1)
			    ((eq cty ty2) act2)
			    (t (mk-actual cty)))))))
	  (t act1))))

(defmethod compatible-type* ((atype type-name) (etype subtype) bindings)
  (compatible-type* atype (supertype etype) bindings))

(defmethod compatible-type* ((atype type-name) (etype expr-as-type) bindings)
  (compatible-type* atype
		    (or (supertype etype)
			(domain (type (expr etype))))
		    bindings))

(defmethod compatible-type* ((atype type-name) etype bindings)
  (declare (ignore bindings))
  #+pvsdebug
  (assert (or (not (formal-decl? (declaration atype)))
	      (declaration-is-not-in-context atype)))
  etype)

(defmethod compatible-type* (atype (etype type-name) bindings)
  (declare (ignore bindings))
  #+pvsdebug
  (assert (or (not (formal-decl? (declaration etype)))
	      (declaration-is-not-in-context etype)))
  atype)

(defmethod compatible-type* ((atype subtype) (etype type-name) bindings)
  (declare (ignore bindings))
  (if (tc-eq (top-type atype) etype)
      (top-type atype)
      ;;(when (declaration-is-not-in-context etype)
      etype
      ;;)
      ))


;;; These two simply recurse up the supertype.  This will find the least
;;; compatible supertype since the case where they are both subtypes is
;;; another method.

(defmethod compatible-type* ((atype subtype) (etype type-expr) bindings)
  (compatible-type* (supertype atype) etype bindings))

(defmethod compatible-type* ((atype datatype-subtype) (etype adt-type-name)
			     bindings)
  (let* ((dtype (find-declared-adt-supertype atype))
	 (ctype (compatible-type* dtype etype bindings)))
    (if ctype
	(if (tc-eq ctype dtype)
	    atype
	    (call-next-method))
	(call-next-method))))

(defmethod compatible-type* ((atype type-expr) (etype subtype) bindings)
  (compatible-type* atype (supertype etype) bindings))

(defmethod compatible-type* ((atype expr-as-type) (etype subtype) bindings)
  (if (tc-eq (expr atype) (predicate etype))
      etype
      (compatible-type* (supertype atype) (supertype etype) bindings)))

(defmethod compatible-type* ((atype subtype) (etype expr-as-type) bindings)
  (if (tc-eq (predicate atype) (expr etype))
      atype
      (compatible-type* (supertype atype) (supertype etype) bindings)))

(defmethod compatible-type* ((atype expr-as-type) (etype expr-as-type) bindings)
  (if (tc-eq (predicate atype) (predicate etype))
      atype
      (compatible-type* (supertype atype) (supertype etype) bindings)))

;;; This is the tricky one, since we don't want to go higher than necessary.
;;; For example, given posint and nat, we don't want to return int or higher
;;; as the least common supertype.  But this is a trivial case.  In general,
;;; subtypes can include free parameters, etc. on one side or the other.

;;; But in general, they must have matching predicates at some point, or go
;;; up to the top type on both sides.  Even then it's not simple, since if
;;; the top type is a formal not in context, it matches the given subtype.

(defmethod compatible-type* ((atype subtype) (etype subtype) bindings)
  (with-slots ((st1 supertype) (p1 predicate) (top1 top-type)) atype
    (with-slots ((st2 supertype) (p2 predicate) (top2 top-type)) etype
      (let ((tv1? (or (type-var? top1)
		      (and (type-name? top1)
			   (declaration-is-not-in-context top1))))
	    (tv2? (or (type-var? top2)
		      (and (type-name? top2)
			   (declaration-is-not-in-context top2)))))
	(if (or tv1? tv2?)
	    (if tv2? top1 top2)
	    (let ((adepth (subtype-depth atype))
		  (edepth (subtype-depth etype)))
	      ;; Tops must be of the same class
	      ;; First get them to where they could match
	      (cond ((> adepth edepth)
		     (compatible-type* (supertype atype) etype bindings))
		    ((< adepth edepth)
		     (compatible-type* atype (supertype etype) bindings))
		    (t ;; Now we go up the two in parallel, till we find matching predicates
		     ;; same-predicate uses tc-eq - won't work unless atype and etype are
		     ;; fully instantiated.
		     (compatible-type* (supertype atype) (supertype etype) bindings)))))))))

(defmethod subtype-depth ((ty subtype) &optional (depth 0))
  (subtype-depth (supertype ty) (1+ depth)))

(defmethod subtype-depth ((ty expr-as-type) &optional (depth 0))
  (subtype-depth (or (supertype ty)
		     (domain (type (expr ty))))
		 (1+ depth)))

(defmethod subtype-depth ((ty type-expr) &optional (depth 0))
  depth)

(defmethod nth-supertype ((ty subtype) n)
  (if (<= n 0) ty (nth-supertype (supertype ty) (- n 1))))

(defmethod nth-supertype ((ty expr-as-type) n)
  (if (<= n 0)
      ty
      (nth-supertype (or (supertype ty)
			 (domain (type (expr ty))))
		     (- n 1))))

(defun adt-compatible-type (acts1 acts2 formals type postypes compacts
				  bindings)
  (cond ((null acts1)
	 (typecheck* (mk-type-name (id type) (nreverse compacts))
		     nil nil nil))
	((tc-eq-with-bindings (car acts1) (car acts2) bindings)
	 (adt-compatible-type (cdr acts1) (cdr acts2) (cdr formals) type
			      postypes (cons (car acts1) compacts) bindings))
	((member (car formals) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration y)))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (let ((ntype (compatible-type (type-value (car acts1))
				       (type-value (car acts2))
				       bindings)))
	   (adt-compatible-type (cdr acts1) (cdr acts2) (cdr formals) type
				postypes (cons (mk-actual ntype) compacts)
				bindings)))))

(defmethod compatible-type* ((t1 datatype-subtype) (t2 datatype-subtype)
			     bindings)
  (adt-compatible-type
   (actuals (module-instance (find-declared-adt-supertype t1)))
   (actuals (module-instance (find-declared-adt-supertype t2)))
   (formals-sans-usings (adt t1))
   t1
   (positive-types (adt t1))
   nil
   bindings))


;;; An actual funtype is compatible with an expected funtype only if the
;;; actual is a subtype of the expected.  We don't distinguish the
;;; function types, so an array is compatible with a function of the same
;;; signature.

(defmethod compatible-type* ((atype funtype) (etype funtype) bindings)
  (with-slots ((adom domain) (arng range)) atype
    (with-slots ((edom domain) (erng range)) etype
      ;; Should we try to instantiate if adom is not fully-instantiated?
      (let* ((ainst? (fully-instantiated? adom))
	     (einst? (fully-instantiated? edom))
	     (used-edom? nil)
	     (dom (cond ((or (type-var? adom) (type-var? edom))
			 (compatible-type* adom edom bindings))
			((and ainst? einst?)
			 (when (compatible-type* adom edom bindings)
			   adom))
			((not einst?)
			 adom)
			(t (setq used-edom? t) edom)))
	     (nbindings (if (and (dep-binding? adom)
				 (dep-binding? edom))
			    (acons adom edom bindings)
			    bindings))
	     (ran (if used-edom?
		      (compatible-type* erng arng nbindings)
		      (compatible-type* arng erng nbindings))))
	(if used-edom?
	    (lcopy etype 'domain dom 'range ran)
	    (lcopy atype 'domain dom 'range ran))))))

(defmethod compatible-type* ((atype tupletype) (etype tupletype) bindings)
  (compatible-tupletypes (types atype) (types etype) nil bindings))

(defun compatible-tupletypes (atypes etypes types bindings)
  (if (null atypes)
      (mk-tupletype (nreverse types))
      (let ((stype (compatible-type* (car atypes) (car etypes) bindings)))
	(assert (or (null stype)
		    (not (dep-binding? (car atypes)))
		    (dep-binding? stype)))
	(when stype
	  (compatible-tupletypes
	   (if (or (not (dep-binding? stype))
		   (not (dep-binding? (car atypes)))
		   (eq stype (car atypes)))
	       (cdr atypes)
	       (substit (cdr atypes) (acons (car atypes) stype nil)))
	   (cdr etypes)
	   (cons stype types)
	   (if (and (dep-binding? stype)
		    (dep-binding? (car etypes)))
	       (acons (car atypes) (car etypes) bindings)
	       bindings))))))

(defmethod compatible-type* ((atype cotupletype) (etype cotupletype) bindings)
  (compatible-cotupletypes (types atype) (types etype) nil bindings))

(defun compatible-cotupletypes (atypes etypes types bindings)
  (if (null atypes)
      (mk-cotupletype (nreverse types))
      (let* ((stype (compatible-type* (car atypes) (car etypes) bindings)))
	(compatible-cotupletypes (cdr atypes) (cdr etypes)
				 (cons stype types) bindings))))

(defmethod compatible-type* ((atype recordtype) (etype recordtype) bindings)
  (compatible-recordtypes (fields atype) (fields etype) atype nil bindings))

(defun compatible-recordtypes (afields efields atype fields bindings)
  (if (null afields)
      (let* ((nfields (nreverse fields))
	     (rtype (if (equal nfields (fields atype))
			atype
			(make-instance 'recordtype
			  :fields nfields
			  :dependent? (dependent? atype)))))
	#+pvsdebug
	(assert (every #'(lambda (fv)
			   (member fv (freevars atype) :test #'same-declaration))
		       (freevars rtype)))
	rtype)
      (let* ((afld (car afields))
	     (efld (find afld efields :test #'same-id))
	     (stype (compatible-type* (type afld) (type efld) bindings))
	     (nfield (if (eq stype (type afld))
			 afld
			 (mk-field-decl (id afld) stype stype)))
	     (cdrfields (if (or (eq nfield afld)
				(not (dependent? atype)))
			    (cdr afields)
			    (let* ((nres (make-resolution nfield
					   (current-theory-name)
					   stype))
				   (fne (mk-field-name-expr (id afld) nres)))
			      (substit (cdr afields)
				(acons afld fne nil))))))
	(compatible-recordtypes cdrfields
				(remove efld efields :test #'eq)
				atype (cons nfield fields) bindings))))


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
  (if (or (tc-eq atype etype)
	  (and (not (typep atype '(and tupletype)))
	       (subtype-of? atype etype)))
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


;;; (adt-compatible-preds list[int] list[nat] x nil) ==>
;;;   ( every(lambda (i: int): i >= 0, x) )

;;; Note the use of find-supertype, rather than compatible-type
;;; AgExample.mTerm generates uprovable TCCs otherwise.
;;; Could find lub based on predicates on positive types, but this
;;; is more expensive.
(defun adt-compatible-preds (atype etype aexpr incs)
  #+pvsdebug (assert (adt? etype))
  (adt-compatible-pred-actuals (actuals (module-instance atype))
			       (actuals (module-instance etype))
			       (if (inline-recursive-type? (adt atype))
				   (formals-sans-usings (module (adt atype)))
				   (formals-sans-usings (adt atype)))
			       (find-supertype atype)
			       (positive-types (adt atype))
			       aexpr
			       incs))

(defun adt-compatible-pred-actuals (aacts eacts formals atype postypes
				    aexpr incs &optional pospreds)
  (cond ((null aacts)
	 (if (some #'cdr pospreds)
	     (let ((epred (make-compatible-every-pred
			   (reverse pospreds) atype aacts aexpr)))
	       (cons epred incs))
	     incs))
	((null eacts) nil)
	((member (car formals) postypes
		 :test #'(lambda (x y)
			   (let ((ydecl (declaration (or (print-type y) y))))
			     (and (typep ydecl 'formal-type-decl)
				  (same-id x ydecl)))))
	 (multiple-value-bind (nincs npospreds)
	     (adt-compatible-pred-actuals* (car aacts) (car eacts) incs pospreds)
	   (adt-compatible-pred-actuals
	    (cdr aacts) (cdr eacts) (cdr formals) atype postypes aexpr
	    nincs npospreds)))
	((tc-eq (car aacts) (car eacts))
	 (adt-compatible-pred-actuals (cdr aacts) (cdr eacts) (cdr formals)
				      atype postypes aexpr incs pospreds))
	(t (adt-compatible-pred-actuals
	    (cdr aacts) (cdr eacts) (cdr formals) atype postypes aexpr
	    (let ((eqp (actual-equality (car aacts) (car eacts))))
	      (if eqp (cons eqp incs) incs))
	    pospreds))))

(defun adt-compatible-pred-actuals* (aact eact incs pospreds)
  #+pvsdebug (assert (and (type-value aact) (type-value eact)))
  (let* ((atype (type-value aact))
	 (etype (type-value eact))
	 (ctype (compatible-type atype etype))
	 (subtype? (subtype-of? etype ctype)))
    (if subtype?
	(let* ((stype (find-supertype ctype))
	       (preds (nth-value 1 (subtype-preds etype stype))))
	  ;; Note that we use the supertype here; otherwise it could lead to
	  ;; Unprovable TCCs
	  (values incs (cons (cons (mk-actual stype) preds) pospreds)))
	(values (cons (make-actuals-equality aact eact) incs) pospreds))))

;;; pospreds are of the form ((act p1 .. pn) ...)
(defun make-compatible-every-pred (pospreds atype aacts aexpr)
  (let ((every (make-every-name atype aacts))
	(*generate-tccs* 'none))
    (make!-application
	(make!-application* every
	  (mapcar #'make-compatible-every-pred* pospreds))
      (copy aexpr))))

(defun make-every-name (atype acts)
  (declare (ignore acts))
  (let* ((adtth (adt-theory (adt atype)))
	 (decl (find '|every| (theory adtth) :key #'decl-id))
	 (thinst (module-instance atype))
	 (modname (mk-modname (id adtth) (actuals atype) (library thinst)))
	 (res (make-resolution decl modname)))
    (mk-name-expr '|every| nil nil res)))

(defmethod decl-id ((decl declaration))
  (id decl))

(defmethod decl-id ((imp importing))
  nil)

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
    (let* ((*generate-tccs* 'none)
	   (stype (dep-binding-type (compatible-type atype etype)))
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
					  (make!-reduced-application o var))
				apred)))
		      (econj (make!-conjunction*
			      (mapcar #'(lambda (o)
					  (make!-reduced-application o var))
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
		     (cons (make!-reduced-application (predicate etype) aexpr)
			   incs)))

;;; This is the tricky one, since we don't want to go higher than
;;; necessary.  For example, given posint as a subtype of nat, we don't
;;; want to return int as the least common supertype.  So we first do a
;;; direct check whether one is a subtype of the other.  If that fails,
;;; we can recurse on both supertypes at the same time.

(defmethod compatible-preds* ((atype subtype) (etype subtype) aexpr incs)
  (if (or (subtype-of? atype etype)
	  (some #'(lambda (jty)
		    (subtype-of? jty etype))
		(judgement-types aexpr)))
      incs
      (compatible-preds* atype (supertype etype) aexpr
			 (cons (make!-reduced-application (predicate etype)
							  aexpr)
			       incs))))

(defmethod compatible-preds* ((atype subtype) (etype datatype-subtype)
			      aexpr incs)
  (compatible-preds* (supertype atype) etype aexpr incs))

(defmethod compatible-preds* ((atype datatype-subtype) (etype datatype-subtype)
			      aexpr incs)
  (adt-compatible-preds atype etype aexpr incs))


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


;;; 
(defun compatible-funtype-pred (atype adom edom avar evar ;;apred epred
				      aexpr arng erng incs)
  (declare (ignore atype evar))
  (let* ((dpreds (equality-predicates adom edom
				      (when (tc-eq (find-supertype arng)
						   *boolean*)
					aexpr)))
	 (dpred (when dpreds
		  (typecheck* dpreds *boolean* nil nil)))
	 (*bound-variables* (append (when (dep-binding? adom) (list adom))
				    (when (dep-binding? edom) (list edom))
				    *bound-variables*))
	 (appl (make!-reduced-application
		;; Since avar is already declared to be of the given
		;; subtype, restrict is just the identity.
		(if (and (implicit-conversion? aexpr)
			 (name-expr? (operator aexpr))
			 (eq (id (operator aexpr)) '|restrict|)
			 (eq (id (module-instance (operator aexpr)))
			     '|restrict|))
		    (argument aexpr)
		    aexpr)
		avar))
	 (rpreds (compatible-preds*
		  arng
		  (if (and (dep-binding? adom)
			   (dep-binding? edom))
		      (substit erng (acons edom adom nil))
		      erng)
		  appl nil))
	 (ran-preds (remove *true* rpreds :test #'tc-eq))
	 (rpred (when ran-preds
		  (if (and (singleton? ran-preds)
			   (forall-expr? (car ran-preds)))
		      (make!-forall-expr (cons (declaration avar)
					       (bindings (car ran-preds)))
			(expression (car ran-preds)))
		      (make!-forall-expr (list (declaration avar))
			(make!-conjunction* ran-preds)))))
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
  ;; This relates
  ;; {x:[T1, ..., Tn] | p(x)} to [{x: T1| p1(x)}, ..., {x: Tn| pn(x)}]
  ;; The idea is that (compatible-preds* RHS LHS aexpr incs) goes through
  ;; the (type-expr subtype) method first, p(x) is added
  ;; to incs, then the pi(x`i) are removed.
  (compatible-preds-tupletypes (types atype) (types etype) (exprs aexpr) incs))

(defun compatible-preds-tupletypes (atypes etypes aexprs incs)
  (cond ((null atypes)
	 incs)
	((or (dep-binding? (car atypes)) (dep-binding? (car etypes)))
	 ;; Could do more here, but need to substitute
	 incs)
	((tc-eq (car atypes) (car etypes))
	 (compatible-preds-tupletypes (cdr atypes) (cdr etypes) (cdr aexprs) incs))
	((subtype-of? (car atypes) (car etypes))
	 (let ((aincs (compatible-preds* (car etypes) (car atypes) (car aexprs) nil)))
	   (compatible-preds-tupletypes (cdr atypes) (cdr etypes) (cdr aexprs)
					(remove-if #'(lambda (inc)
						       (member inc aincs :test #'tc-eq))
					  incs))))
	(t (compatible-preds-tupletypes (cdr atypes) (cdr etypes) (cdr aexprs) incs))))

(defmethod compatible-preds* ((atype cotupletype) (etype cotupletype)
			      (aexpr injection-application) incs)
  incs)

(defmethod compatible-preds* ((atype tuple-or-struct-subtype)
			      (etype tuple-or-struct-subtype)
			      aexpr incs)
  (compatible-tupletype-preds (types atype) (types etype)
			      (cond ((tuple-expr? aexpr) (exprs aexpr))
				    ((listp aexpr) aexpr)
				    (t (make!-projections aexpr)))
			      incs))

(defun compatible-tupletype-preds (atypes etypes aexprs incs)
  (if (or (null atypes) (null etypes))
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

(defmethod compatible-preds* ((atype cotupletype) (etype cotupletype) aexpr incs)  
  (compatible-cotupletype-preds (types atype) (types etype) aexpr incs))

;;; Given S_i = {x: T_i | p_i(x)}, 1 <= i <= n,
;;; Returns  CASES aexpr OF IN_1(x): p_1(x), ..., IN_n(x): p_n(x) ENDCASES
(defun compatible-cotupletype-preds (atypes etypes aexpr incs
					    &optional (index 1) sels)
  (if (null atypes)
      (cons (make!-unpack-expr aexpr (nreverse sels)) incs)
      (let* ((id (make-new-variable '|x| aexpr))
	     (bd (make-bind-decl id (car atypes)))
	     (var (make-variable-expr bd))
	     (pred (compatible-preds* (car atypes) (car etypes) var nil))
	     (in-expr (make-instance 'injection-expr
			:id (makesym "IN_~d" index)
			:index index)))
	(compatible-cotupletype-preds
	 (cdr atypes) (cdr etypes) aexpr incs (1+ index)
	 (cons (make-instance 'in-selection
		 :constructor in-expr
		 :args (list var)
		 :index index
		 :expression pred)
	       sels)))))

(defmethod compatible-preds* ((atype record-or-struct-subtype)
			      (etype record-or-struct-subtype)
			      (aexpr record-expr) incs)
  incs)

(defmethod compatible-preds* ((atype record-or-struct-subtype)
			      (etype record-or-struct-subtype)
			      aexpr incs)
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

(defun subst-tuptypes (tuptype expr)
  (if (some #'dep-binding? (types tuptype))
      (subst-tuptypes* (types tuptype) expr)
      (types tuptype)))

(defun subst-tuptypes* (types expr &optional (index 1) ntypes)
  (if (null types)
      (nreverse ntypes)
      (if (dep-binding? (car types))
	  (let ((pappl (make-instance 'projappl
			 :id (makesym "PROJ_~d" index)
			 :index index
			 :argument expr
			 :type (type (car types)))))
	    (subst-tuptypes* (substit (cdr types)
			       (acons (car types) pappl nil))
			     expr
			     (1+ index)
			     (cons (type (car types)) ntypes)))
	  (subst-tuptypes* (cdr types) expr (1+ index)
			   (cons (car types) ntypes)))))

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
  (make!-field-application (id field) expr))

(defun subst-var-application (field fields aexpr)
  (when fields
    (let* ((ne aexpr)
	   (appl (make!-field-application (id field) ne)))
      (substit fields (list (cons field appl))))))

;;; Subtype-of? is used to determine if two types are in the subtype
;;; relation to each other.  It returns the distance from t1 to t2 if t1
;;; is a subtype of t2, otherwise it returns NIL.  t1 is a subtype of t2
;;; either directly through type declarations, or indirectly through
;;; judgements.

(defun subtype-of? (t1 t2)
  (when (strict-compatible? t1 t2)
    (let ((*subtypes-matched* nil))
      (subtype-of*? t1 t2))))

(defun strict-subtype-of? (t1 t2)
  (and (not (tc-eq t1 t2))
       (subtype-of? t1 t2)))

(defmethod subtype-of*? :around (t1 t2)
  (or (tc-eq t1 t2)
      (simple-subtype-of? t1 t2)
      (if (and *subtype-of-hash*
	       (not *checking-conversions*))
	  (let ((pair (cons t1 t2)))
	    (multiple-value-bind (st? there?)
		(gethash pair (the hash-table *subtype-of-hash*))
	      (if there?
		  st?
		  (let ((nvalue (or (call-next-method)
				    (known-subtype-of? t1 t2))))
		    (setf (gethash pair (the hash-table *subtype-of-hash*))
			  nvalue)))))
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

(defmethod subtype-of*? ((t1 adt-type-name) (t2 adt-type-name))
  (or (call-next-method)
      (and (eq (adt t1) (adt t2))
	   (adt-subtype-of?
	    (actuals (module-instance t1))
	    (actuals (module-instance t2))
	    (if (inline-recursive-type? (adt t1))
		(formals-sans-usings (module (adt t1)))
		(formals-sans-usings (adt t1)))
	    t1
	    (positive-types (adt t1))))))

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
  (with-slots ((st1 supertype) (pr1 predicate) (conj1 subtype-conjuncts)) t1
    (with-slots ((st2 supertype) (pr2 predicate) (conj2 subtype-conjuncts)) t2
      (when (null conj1)
	(setf conj1 (collect-subtype-conjuncts t1)))
      (when (null conj2)
	(setf conj2 (collect-subtype-conjuncts t2)))
      ;;(assert (and conj1 conj2))
      ;; conji is of form (bd ex1 ex2), where the bd is a bind-decl in the exi.
      ;;(break "subtype-of*? subtype")
      (cond ((and ;;(not *checking-conversions*)
		  conj1 conj2
		  (let ((b1 (car conj1))
			(b2 (car conj2)))
		    (subsetp (cdr conj2) (cdr conj1)
			     :test #'(lambda (c2 c1) (tc-eq-with-bindings c2 c1 (list (cons b2 b1)))))))
	     t)
	    ((or (and (typep pr1 'recognizer-name-expr)
		      (typep pr2 'recognizer-name-expr)
		      (tc-eq-ops pr1 pr2))
		 (same-predicate? t1 t2 nil)
		 (and (compatible? st1 st2)
		      (same-free-parameters? t1 t2)
		      (predicate-subtype-of? t1 t2 (compatible-type st1 st2))))
	     (subtype-of*? st1 st2))
	    ((and (name-expr? pr1)
		  (lambda-expr? pr2)
		  (disjunction? (expression pr2))
		  (member pr1 (disjuncts (expression pr2))
			  :test #'(lambda (x y)
				    (and (application? y)
					 (name-expr? (argument y))
					 (eq (declaration (argument y))
					     (car (bindings pr2)))
					 (tc-eq x (operator y))))))
	     t)
	    (t (call-next-method))))))

(defmethod same-free-parameters? ((t1 type-expr) (t2 type-expr))
  (let ((frees1 (remove-if-not #'(lambda (fp) (occurs-in fp t1))
		  (free-params t1)))
	(frees2 (remove-if-not #'(lambda (fp) (occurs-in fp t2))
		  (free-params t2))))
    (and (length= frees1 frees2)
	 (every #'(lambda (f1) (memq f1 frees2)) frees1))))

(defun predicate-subtype-of? (t1 t2 st)
  (or (simple-subtype-of? t1 t2)
      (and (not (simple-subtype-of? t2 t1))
	   (strict-compatible? t1 t2)
	   (let* ((bid '%)
		  (bd (make!-bind-decl bid st))
		  (bvar (make-variable-expr bd))
		  (preds1 (collect-judgement-preds t1 st bvar))
		  (preds2 (collect-judgement-preds t2 st bvar)))
	     (subsetp preds2 preds1 :test #'tc-eq)))))

(defmethod subtype-of*? ((t1 adt-type-name) (t2 subtype))
  (when (adt t1)
    (when (symbolp (adt t1))
      ;; May happen after restoring from bin files
      (restore-adt-slot t1)))
  (and (singleton? (constructors (adt t1)))
       (tc-eq t1 (supertype t2))
       (recognizer-name-expr? (predicate t2))))

(defmethod subtype-of*? ((t1 subtype) (t2 type-expr))
  (subtype-of*? (supertype t1) t2))

(defmethod subtype-of*? ((t1 funtype) (t2 funtype))
  (when (tc-eq (dep-binding-type (domain t1)) (dep-binding-type (domain t2)))
    (subtype-of*? (range t1) (range t2))))

(defmethod subtype-of*? ((t1 tupletype) (t2 tupletype))
  (when (length= (types t1) (types t2))
    (subtype-of-list (types t1) (types t2))))

(defmethod subtype-of*? ((t1 cotupletype) (t2 cotupletype))
  (when (length= (types t1) (types t2))
    (subtype-of-list (types t1) (types t2))))

(defun subtype-of-list (t1 t2)
  (or (null t1)
      (and (subtype-of*? (car t1) (car t2))
	   (let* ((dep? (and (dep-binding? (car t1))
			     (dep-binding? (car t2))))
		  (*bound-variables*
		   (if dep?
		       (cons (car t2) *bound-variables*)
		       *bound-variables*))
		  (st1cdr (if dep?
			      (substit (cdr t1) (acons (car t1) (car t2) nil))
			      (cdr t1))))
	     (subtype-of-list st1cdr (cdr t2))))))

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
	 (vb (mk-bind-decl vid t2 t2))
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
      (subtype-preds (supertype t1) (compatible-type t2 t1)
		     (cons (predicate t1) incs))
      (multiple-value-bind (ty preds)
	  (subtype-preds t1 (compatible-type t2 t1) nil)
	(when ty
	  (if preds
	      (let* ((vid (make-new-variable '|v| t2))
		     (vb (mk-bind-decl vid t2 t2))
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
	 (bd (mk-bind-decl id t2 t2))
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
  (let ((eq-inc (unless (tc-eq (domain t1) (domain t2))
		  (actual-equality-type (domain t1) (domain t2)))))
    (multiple-value-bind (ty preds)
	(subtype-preds (range t1) (range t2))
      (when (or eq-inc ty)
	(let* ((xid (make-new-variable '|x| (list t1 t2)))
	       (tupty (dep-binding-type (domain t2)))
	       (xb (mk-bind-decl xid tupty tupty))
	       (xvar (mk-name-expr xid nil nil
				   (make-resolution xb
				     (current-theory-name) tupty)))
	       (vid (make-new-variable '|f| (list t1 t2)))
	       (vb (mk-bind-decl vid t2 t2))
	       (var (mk-name-expr vid nil nil
				  (make-resolution vb
				    (current-theory-name) t2)))
	       (rapps (mapcar #'(lambda (pred)
				  (make!-application pred
				    (make!-application var
				      xvar)))
			preds))
	       (conj (make!-conjunction*
		      (if eq-inc
			  (cons eq-inc rapps)
			  rapps)))
	       (npred (make!-lambda-expr (list vb)
			(make!-forall-expr (list xb) conj))))
	  (values t2 (cons npred incs)))))))

(defun extract-domain (ftype)
  (let ((dom (dep-binding-type (domain ftype))))
    (if (and (singleton? dom)
	     (tupletype? (car dom)))
	(types (car dom))
	(list dom))))

(defmethod subtype-preds ((t1 tupletype) (t2 tupletype) &optional incs)
  (when (length= (types t1) (types t2))
    (let* ((vid (make-new-variable '|t| (list t1 t2)))
	   (vb (mk-bind-decl vid t2 t2))
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
      (when preds
	(values type2
		(make!-lambda-expr (list vb)
		  (make!-conjunction* (reverse preds)))))
      (multiple-value-bind (ty cpreds)
	  (subtype-preds (car types1) (car types2))
	(let ((npreds (when ty (subtype-tuple-preds* cpreds var num))))
	  (subtype-tuple-preds (if (typep (car types1) 'dep-binding)
				   (substit (cdr types1)
				     (acons (car types1)
					    (make!-projection-application
					     num var)
					    nil))
				   (cdr types1))
			       (cdr types2)
			       type2 vb var (1+ num)
			       (nconc npreds preds))))))

(defun subtype-tuple-preds* (cpreds var num &optional preds)
  (if (null cpreds)
      preds
      (let ((npred (make!-reduced-application (car cpreds)
		     (make!-projection-application num var))))
	(subtype-tuple-preds* (cdr cpreds) var num (cons npred preds))))) 

(defmethod subtype-preds ((t1 cotupletype) (t2 cotupletype) &optional incs)
  (when (length= (types t1) (types t2))
    (let* ((vid (make-new-variable '|t| (list t1 t2)))
	   (vb (mk-bind-decl vid t2 t2))
	   (var (mk-name-expr vid nil nil
			      (make-resolution vb
				(current-theory-name) t2))))
      (multiple-value-bind (ty pred)
	  (subtype-cotuple-preds (types t1) (types t2) t2 vb var)
	(when ty
	  (values t2 (cons pred incs)))))))

(defun subtype-cotuple-preds (types1 types2 type2 vb var
				     &optional (index 1) sels)
  (if (null types1)
      (values type2
	      (make!-lambda-expr (list vb)
		(make!-unpack-expr var (nreverse sels))))
      (multiple-value-bind (ty cpreds)
	  (subtype-preds (car types1) (car types2))
	(when ty
	  (let ((in-expr (make-instance 'injection-expr
			   :id (makesym "IN_~d" index)
			   :index index))
		(pred (make!-conjunction*
		       (mapcar #'(lambda (o)
				   (make!-reduced-application o var))
			 cpreds))))
	    (subtype-cotuple-preds
	     (cdr types1) (cdr types2) type2 vb var (1+ index)
	     (cons (make-instance 'in-selection
		     :constructor in-expr
		     :args (list var)
		     :index index
		     :expression pred)
	       sels)))))))

(defmethod subtype-preds ((t1 recordtype) (t2 recordtype) &optional incs)
  (when (length= (fields t1) (fields t2))
    (let* ((vid (make-new-variable '|t| (list t1 t2)))
	   (vb (mk-bind-decl vid t2 t2))
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
		(make!-conjunction* (nreverse preds))))
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
;;;  {t: [T1,..,Tn] | p(t)}
;;; Generating [t1: T1, ..., {tn: Tn | p(t1,...,tn)}]
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

;; Type-canon tries to lift predicates to the top level
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
	:supertype te
	:predicate (make-lambda-conjunction te predicates))
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
  (type-canon* (supertype te)
	       (if (everywhere-true? (predicate te))
		   predicates
		   (cons (predicate te)  predicates))))

(defmethod type-canon* ((te dep-binding) predicates)
  (lcopy te :type (type-canon* (type te) predicates)))

(defmethod type-canon* ((te tupletype) predicates)
  (if predicates
      (type-canon-tupletype te predicates)
      te))

;; (defun make-new-dep-bindings (types &optional expr (id '|t|) (num 1) dpbdgs)
;;   (if (null types)
;;       (nreverse dpbdgs)
;;       (if (dep-binding? (car types))
;; 	  (make-new-dep-bindings (cdr types) expr id num (cons (car types) dpbdgs))
;; 	  (let* ((nid (make-new-variable id expr num))
;; 		 (db (mk-dep-binding (dep-binding-type (car types)))))
;; 	(make-new-dep-bindings (cdr types) expr id (
	

;; Should return a subtype, with predicates lifted from the component types
;; [{x:T1|p1(x)}, ..., {x:Tn|pn(x)}] ==>
;;       {x: [T1, ..., Tn] | p1(x`1) ∧ ... ∧ pn(x`n)}
;; Dependent type:
;; [y:{x:T1|p1(x)}, {x:T2(y)|p2(x,y)}] ==>
;;       {x: [y: T1, T2(y)] | p1(x`1) ∧ p2(x`1, x`2)}
;;   Problem if T2 requires arg to satisfy p1 - possible if T2 a function type
  
(defun type-canon-tupletype (te predicates)
  (multiple-value-bind (npreds var)
      (make-preds-with-same-binding te predicates)
    (let* ((parts (partition-projection-predicates
		   var (mapcan #'conjuncts npreds)))
	   (ntypes (add-tupletype-preds (types te) parts))
	   (ntuptype (make-instance 'tupletype :types ntypes))
	   (toppreds (cdr (assq nil parts))))
      (if toppreds
	  (make-instance 'subtype
	    :supertype ntuptype
	    :predicate (make-lambda-expr-or-eta-equivalent (declaration var)
			 (make!-conjunction* toppreds)))
	  ntuptype))))

;; (defun type-canon-tupletype (types predicates)
;;  (let* ((dbdgs (mapcar #'mk-dep-binding 
;;         (parts (partition-projection-predicates
;;                 var (mapcan #'conjuncts npreds)))
;;         (ntypes (add-tupletype-preds (types te) parts))
;;         (ntuptype (make-instance 'tupletype :types ntypes))
;;         (toppreds (cdr (assq nil parts))))
;;    (if toppreds
;;        (make-instance 'subtype
;;          :supertype ntuptype
;;          :predicate (make-lambda-expr-or-eta-equivalent (declaration var)
;;                                                         (make!-conjunction* toppreds)))
;;        ntuptype))))

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

(defmethod type-canon* ((te cotupletype) predicates)
  (declare (ignore predicates))
  te)
	       
(defmethod type-canon* ((te recordtype) predicates)
  (type-canon-recordtype te predicates))

(defun type-canon-recordtype (te predicates)
  (multiple-value-bind (npreds var)
      (make-preds-with-same-binding te predicates)
    (let* ((parts (partition-field-predicates
		   var (mapcan #'conjuncts npreds)))
	   (nfields (add-field-type-preds (fields te) parts))
	   (nrectype (make-instance 'recordtype :fields nfields))
	   (toppreds (cdr (assq nil parts))))
      (if toppreds
	  (make-instance 'subtype
	    :supertype nrectype
	    :predicate (make-lambda-expr-or-eta-equivalent
			(declaration var)
			(make!-conjunction* toppreds)))
	  nrectype))))

(defun partition-field-predicates (var predicates &optional parts)
  (if (null predicates)
      parts
      (let* ((field (which-field-predicate var (car predicates)))
	     (part (assoc field parts :test #'tc-eq)))
	(partition-field-predicates
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
      (let* ((preds (assoc (id (car fields)) parts
			   :key #'(lambda (x) (and x (id x)))))
	     (npreds (when preds
		       (make-new-field-type-preds (car fields) preds)))
	     (ntype (type-canon* (type (car fields))
				 (when npreds (list npreds)))))
	(add-field-type-preds (cdr fields) parts
			      (cons (lcopy (car fields)
				      :type ntype
				      :declared-type ntype)
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

;;; intersection-type returns the largest type contained in the given types,
;;; which must be compatible.  It can return the empty type.  Note that this
;;; never returns a dep-binding.

(defun intersection-type (t1 t2)
  #+pvsdebug
  (assert (compatible? t1 t2))
  (cond ((subtype-of? t1 t2)
	 (dep-binding-type t1))
	((subtype-of? t2 t1)
	 (dep-binding-type t2))
	(t (let* ((preds1 (type-predicates t1 t))
		  (preds2 (type-predicates t2 preds1)))
	     (assert (and preds1 preds2))
	     (make-intersection-subtype t1 preds2)))))

(defun make-intersection-subtype (ty preds)
  (if (null preds)
      ty
      (mk-subtype ty
	(let* ((bd (make-new-bind-decl ty))
	       (var (make-variable-expr bd)))
	  (make!-set-expr (list bd) (make!-application (car preds) var))))))
