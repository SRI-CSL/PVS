
;;; FILE: hashfn.lisp
;;; AUTHOR: David Cyrluk
;;; Date: 7/21/93

;;; This file attempts to build a pvs-sxhash function, that is
;;; faithful to tc-eq.

;;; The heuristic that I used to do this was to folow the structure
;;; of tc-eq*, and replace "and" with "+".

;;; This doesn't quite work if there is an "or" in tc-eq*. Since if there is
;;; an "or", it would be a mistake to factor both parts of the disjunction
;;; into pvs-sxhash. Instead I try to pull out whatever is common to
;;; both disjuncts.

;;; When there is an "eq" (or something similar) I call sxhash.
;;; When there are recursive calls to tc-eq*, I recursively call
;;; pvs-sxhash*.

;;; So far I know of only one place where pvs-sxhash is not faithful to
;;; tc-eq. This has to do with the "bindings" argument to tc-eq*.
;;; The bindings is supposed to keep track of bound variables that are
;;; supposed to be the same, but have different names.
;;; An example of this is alpha conversion:
;;; LAMBDA x: p(x) == LAMBDA y: p(y) (also true for other binding-ops)
;;; tc-eq* handles this by putting (x . y) in the bindings.

;;; pvs-sxhash does not have access to the second argument, so can't
;;; do this. I will try to fix this later by putting (x . 1) in the
;;; bindings. (In general (y . i) for the ith bound variable.)
;;; The functions involved are pvs-sxhash-dep-types, pvs-sxhash-dep-types*,
;;; and pvs-sxhash-range-bindings. Also pvs-sxhash* for names, name-exprs
;;; and maybe some others will have to be changed to look at their bindings
;;; argument.

;;; Sam also suggests adding type declarations saying that the
;;; arguments to plus, etc are integers.

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

(in-package 'pvs)

;(declaim (function pvs-sxhash* (T list) (integer 0 65535)))


;(defun tc-eq (x y &optional bindings)
;  (tc-eq* x y bindings))

(defconstant pvs-sxhash-byte (byte 29 0))

(deftype positive-fixnum () `(integer 0 ,most-positive-fixnum))

(defmacro pvs-sxhash-+ (i j)
  `(ldb pvs-sxhash-byte (+ (the positive-fixnum ,i) (the positive-fixnum ,j))))

(defmacro pvs-sxhash-* (i j)
  `(ldb pvs-sxhash-byte (* (the positive-fixnum ,i) (the positive-fixnum ,j))))

(defmacro pvs-sxhash-1+ (i)
  `(ldb pvs-sxhash-byte (1+ (the positive-fixnum ,i))))

(proclaim '(ftype (function (t list) positive-fixnum) pvs-sxhash*))

(defun pvs-sxhash (x &optional bindings)
  (pvs-sxhash* x bindings))

;;; If two object aren't tc-eq* by more specific methods, then try eq.

;(defmethod tc-eq* (x y bindings)
;  (declare (ignore bindings))
;  (eq x y))

(defmethod pvs-sxhash* :around ((x syntax) (bindings null))
  (with-slots (pvs-sxhash-value) x
    (if (null (freevars x))
	(or pvs-sxhash-value
	    (let ((val (call-next-method)))
	      (when (and (lambda-expr? x) (not (set-expr? x)))
		(break "lambda-expr"))
	      (setf pvs-sxhash-value val)))
	(call-next-method))))

(defmethod pvs-sxhash* (x bindings)
  (declare (ignore bindings))
  (the positive-fixnum (sxhash x)))

(defconstant nil-sxhash (sxhash nil))

(defmethod pvs-sxhash* ((x null) bindings)
  (declare (ignore bindings))
  nil-sxhash)

;(defmethod pvs-sxhash* ((type type-expr) bindings)
;  (declare (ignore bindings))
;  17)

;(defmethod tc-eq* ((l1 cons) (l2 cons) bindings)
;  (declare (cons l1 l2))
;  (let ((list1 (if (and (null (cdr l1))
;			(typep (car l1) 'tuple-expr))
;		   (exprs (car l1))
;		   l1))
;	(list2 (if (and (null (cdr l2))
;			(typep (car l2) 'tuple-expr))
;		   (exprs (car l2))
;		   l2)))
;    (tc-eq-lists list1 list2 bindings)))

(defmethod pvs-sxhash* ((l1 cons) bindings)
  (if (typep (car l1) 'binding)
      (pvs-sxhash-lists-b l1 bindings (pvs-sxhash-1+ (length bindings)) 127)
      (pvs-sxhash-lists-a l1 bindings 127)))

;(defun tc-eq-lists (l1 l2 bindings)
;  (declare (list l1 l2))
;  (if (null l1)
;      (null l2)
;      (and (tc-eq* (car l1) (car l2) bindings)
;	   (tc-eq-lists (cdr l1) (cdr l2)
;			(if (and (typep (car l1) 'binding)
;				 (typep (car l2) 'binding))
;			    (acons (car l1) (car l2) bindings)
;			    bindings)))))

(defun pvs-sxhash-lists-a (l1 bindings hnum)
  (declare (type positive-fixnum hnum))
  (if (null l1)
      hnum
      (if (listp l1)
	  (pvs-sxhash-lists-a
	   (cdr l1)
	   bindings
	   (the positive-fixnum
	     (pvs-sxhash-+ (the positive-fixnum
			     (ldb pvs-sxhash-byte
				  (ash (the positive-fixnum hnum) 1)))
			   (the positive-fixnum
			     (pvs-sxhash* (car l1) bindings)))))
	  (pvs-sxhash-+ (the positive-fixnum
			  (ldb pvs-sxhash-byte
			       (ash (the positive-fixnum hnum) 1)))
			(the positive-fixnum
			  (pvs-sxhash* l1 bindings))))))

(defun pvs-sxhash-lists-b (l1 bindings num hnum)
  (declare (type positive-fixnum num hnum))
  (if (null l1)
      hnum
      (if (listp l1)
	  (pvs-sxhash-lists-b
	   (cdr l1)
	   (acons (car l1) num bindings)
	   (pvs-sxhash-1+ (the positive-fixnum num))
	   (pvs-sxhash-+ (the positive-fixnum
			   (ldb pvs-sxhash-byte
				(ash (the positive-fixnum hnum) 1)))
			 (the positive-fixnum
			   (pvs-sxhash* (car l1) bindings))))
	  (pvs-sxhash-+ (the positive-fixnum
			  (ldb pvs-sxhash-byte
			       (ash (the positive-fixnum hnum) 1)))
			(the positive-fixnum
			  (pvs-sxhash* l1 bindings))))))

;(defmethod pvs-sxhash* ((n1 type-name) bindings)
;  (with-slots (id resolutions) n1
;    (let ((decl1 (when resolutions
;		   (slot-value (car resolutions) 'declaration))))
;      (pvs-sxhash-+
;       (the positive-fixnum (sxhash id))
;       (let ((mi1 (when resolutions
;		    (slot-value (car resolutions) 'module-instance))))
;	 (if mi1
;	     (the positive-fixnum
;	       (pvs-sxhash-+ (the positive-fixnum (sxhash (id mi1)))
;			     (the positive-fixnum (pvs-sxhash* (actuals mi1) bindings))))
;	     (the positive-fixnum (sxhash nil))))))))

;(defun type-binding? (te)
;  (typep te '(or dep-binding field-decl)))

;(defmethod tc-eq* ((t1 dep-binding) (t2 dep-binding) bindings)
;  (or (eq t1 t2)
;      (with-slots ((ty1 type)) t1
;	(with-slots ((ty2 type)) t2
;	  (tc-eq* ty1 ty2 bindings)))))

(defmethod pvs-sxhash* ((t1 dep-binding) bindings)
  (with-slots ((ty1 type)) t1
    (pvs-sxhash* ty1 bindings)))

;;; Type expressions - enumtypes are not handled since they are never
;;; the canonical form.

;(defmethod tc-eq* ((t1 subtype) (t2 subtype) bindings)
;  (or (eq t1 t2)
;      (with-slots ((st1 supertype) (p1 predicate)) t1
;	(with-slots ((st2 supertype) (p2 predicate)) t2
;	  (and (tc-eq* st1 st2 bindings)
;	       (tc-eq* p1 p2 bindings))))))

;(defmethod pvs-sxhash* ((t1 subtype) bindings)
;  (with-slots ((st1 supertype) (p1 predicate)) t1
;    (+ (pvs-sxhash* st1 bindings)
;       (pvs-sxhash* p1 bindings))))

(defmethod pvs-sxhash* ((t1 subtype) bindings)
  ;; Ignore the supertype
  (with-slots (predicate) t1
    (pvs-sxhash-+ 103 (the positive-fixnum (pvs-sxhash* predicate bindings)))))

;(defmethod tc-eq* ((t1 funtype) (t2 funtype) bindings)
;  (or (eq t1 t2)
;      (with-slots ((d1 domain) (r1 range)) t1
;	(with-slots ((d2 domain) (r2 range)) t2
;	  (or (eq t1 t2)
;             (tc-eq* (list d1 r1) (list d2 r2) bindings)))))

(defmethod pvs-sxhash* ((t1 funtype) bindings)
  (with-slots ((d1 domain) (r1 range)) t1
    (pvs-sxhash-1+ (pvs-sxhash* (list d1 r1) bindings))))

;(defmethod pvs-sxhash* ((t1 funtype) bindings)
;  (declare (ignore bindings))
;  2)

;(defmethod tc-eq* ((t1 tupletype) (t2 tupletype) bindings)
;  (or (eq t1 t2)
;      (with-slots ((ty1 types)) t1
;	(with-slots ((ty2 types)) t2
;	  (tc-eq* ty1 ty2 bindings)))))

(defmethod pvs-sxhash* ((t1 tupletype) bindings)
  (with-slots ((ty1 types)) t1
    (pvs-sxhash-lists-b ty1 bindings (pvs-sxhash-1+ (length bindings)) 127)))

(defmethod pvs-sxhash* ((t1 cotupletype) bindings)
  (with-slots ((ty1 types)) t1
    (pvs-sxhash-lists-b ty1 bindings (pvs-sxhash-1+ (length bindings)) 237)))

;(defmethod pvs-sxhash* ((t1 tupletype) bindings)
;  (declare (ignore bindings))
;  3)


;;; dom1 and dom2 are lists, but we need to handle the cases when they
;;; are singletons which are dep-bindings or tuple-types.

;(defun tc-eq-dep-types (dom1 dom2 rng1 rng2 bindings)
;  (let ((odom1 (open-up-domain dom1))
;	(odom2 (open-up-domain dom2)))
;    (declare (list odom1 odom2))
;    (and (length= odom1 odom2)
;	 (multiple-value-bind (eq? bindings)
;	     (tc-eq-dep-types* odom1 odom2 bindings)
;	   (if (and eq? rng1)
;	       (tc-eq* rng1 rng2 (append (tc-eq-range-bindings dom1 dom2)
;					bindings))
;	       eq?)))))

;(defun pvs-sxhash-dep-types (dom1 rng1 bindings)
;  (let ((odom1 (open-up-domain dom1)))
;    (declare (list odom1))
;    (multiple-value-bind (hash bindings)
;	(pvs-sxhash-dep-types* odom1 bindings)
;      (if rng1
;	  (+ hash
;	     (pvs-sxhash* rng1 (append (pvs-sxhash-range-bindings dom1)
;				       bindings)))
;	  hash))))

;(defun open-up-domain (dom)
;  (if (cdr dom)
;      dom
;      (typecase (car dom)
;	(tupletype (types (car dom)))
;	(dep-binding (if (typep (type (car dom)) 'tupletype)
;			 (types (type (car dom)))
;			 (list (type (car dom)))))
;	(t dom))))

;(defun tc-eq-range-bindings (dom1 dom2)
;  (if (and (singleton? dom1)
;	   (typep (car dom1) 'dep-binding))
;      (if (and (singleton? dom2)
;	       (typep (car dom2) 'dep-binding))
;	  (list (cons (car dom1) (car dom2)))
;	  (let ((num 0))
;	    (declare (positive-fixnum num))
;	    (mapcan #'(lambda (ty)
;			(incf (the integer num))
;			(when (typep ty 'dep-binding)
;			  (list (list (car dom1) ty num))))
;		    (if (and (singleton? dom2)
;			     (typep (car dom2) 'tupletype))
;			(types (car dom2))
;			dom2))))
;      (if (and (singleton? dom2)
;	       (typep (car dom2) 'dep-binding))
;	  (let ((num 0))
;	    (mapcan #'(lambda (ty)
;			(incf num)
;			(when (typep ty 'dep-binding)
;			  (list (list ty (car dom2) num))))
;		    (if (and (singleton? dom1)
;			     (typep (car dom1) 'tupletype))
;			(types (car dom1))
;			dom1)))
;	  (mapcan #'(lambda (t1 t2)
;		      (when (and (typep t1 'dep-binding)
;				 (typep t2 'dep-binding))
;			(list (list t1 t2))))
;		  dom1 dom2))))


;(defun pvs-sxhash-range-bindings (dom1)
;  (cond ((and (singleton? dom1)
;	      (typep (car dom1) 'dep-binding))
;	 (list (cons dom1 1)))
;	(t
;	 (let ((num 0))
;	   (mapcan #'(lambda (ty)
;		       (incf num)
;		       (when (typep ty 'dep-binding)
;			 (list ty num)))
;		   (if (and (singleton? dom1)
;			    (typep (car dom1) 'tupletype))
;		       (types (car dom1))
;		       dom1))))))

;(defun tc-eq-dep-types* (types1 types2 bindings)
;  (if (null types1)
;      (values t bindings)
;      (and (tc-eq* (car types1) (car types2) bindings)
;	   (tc-eq-dep-types* (cdr types1) (cdr types2)
;			     (if (and (typep (car types1) 'dep-binding)
;				      (typep (car types2) 'dep-binding))
;				 (acons (car types1) (car types2) bindings)
;				 bindings)))))

;(defun pvs-sxhash-dep-types* (types1 bindings &optional (num 1))
;  (if (null types1)
;      (values 0 bindings)
;      (multiple-value-bind (hash new-bindings)
;	  (pvs-sxhash-dep-types* (cdr types1)
;				 (if (typep (car types1) 'dep-binding)
;				     (acons (car types1) num bindings)
;				     bindings)
;				 (1+ num))
;	(values (+ (pvs-sxhash* (car types1) bindings)
;		   hash)
;		new-bindings))))

;(defmethod tc-eq* ((n1 name-expr) (n2 name-expr) bindings)
;  (or (eq n1 n2)
;      (and (call-next-method)
;	   (or (null *strong-tc-eq-flag*)
;	       (with-slots ((t1 type)) n1
;		 (with-slots ((t2 type)) n2
;		   (tc-eq* t1 t2 bindings)))))))

;(defmethod pvs-sxhash* ((n1 name-expr) bindings)
;  (call-next-method))

(defmethod pvs-sxhash-ops ((op1 adt-name-expr) bindings)
  (declare (ignore bindings))
  (with-slots (id) op1
    (the positive-fixnum (sxhash id))))


;(defmethod tc-eq* ((ne name-expr) (ap application) bindings)
;  (with-slots (id (type-ne type) resolutions) ne
;    (with-slots (operator arguments (type-ap type)) ap
;      (when (typep (declaration (car resolutions)) 'dep-binding)
;	(let ((db (assq (declaration (car resolutions)) bindings)))
;	  (and db
;	       (typep operator 'name-expr)
;	       (singleton? arguments)
;	       (typep (car arguments) 'name-expr)
;	       (eq (declaration (car arguments)) (cadr db))
;	       (if (typep (type (cadr db)) 'tupletype)
;		   (eq (id operator) (makesym "PROJ_~d" (caddr db)))
;		   (eq (id operator) id))
;	       (tc-eq* type-ne type-ap nil)))))))

;(defmethod pvs-sxhash* ((ne name-expr) bindings)
;  (declare (ignore bindings))
;  (with-slots (id (type-ne type) resolutions) ne
;    (if (typep (declaration (car resolutions)) 'dep-binding)
;	(+ (sxhash (declaration (car resolutions)))
;	   (pvs-sxhash* type-ne nil))
;	(call-next-method))))

;(defmethod tc-eq* ((ap application) (ne name-expr) bindings)
;  (with-slots (id (type-ne type) resolutions) ne
;    (with-slots (operator arguments (type-ap type)) ap
;      (when (typep (declaration (car resolutions)) 'dep-binding)
;	(let ((db (car (member (declaration (car resolutions)) bindings
;			       :test #'(lambda (x y)
;					 (and (consp (cdr y))
;					      (eq x (cadr y))))))))
;	  (and db
;	       (typep operator 'name-expr)
;	       (singleton? arguments)
;	       (typep (car arguments) 'name-expr)
;	       (eq (declaration (car arguments)) (car db))
;	       (if (typep (type (car db)) 'tupletype)
;		   (eq (id operator) (makesym "PROJ_~d" (caddr db)))
;		   (eq (id operator) id))
;	       (tc-eq* type-ne type-ap bindings)))))))

;(defmethod pvs-sxhash* ((ap application) bindings)
;  (with-slots (operator arguments (type-ap type)) ap
;    (+ (sxhash (typep operator 'name-expr))
;       (sxhash (and (singleton? arguments)
;		    (typep (car arguments) 'name-expr)))
;       (sxhash (declaration (car arguments)))
;       (sxhash (id operator))
;       (pvs-sxhash* type-ap bindings))))

;(defmethod tc-eq* ((t1 recordtype) (t2 recordtype) bindings)
;  (or (eq t1 t2)
;      (with-slots ((fields1 fields)) t1
;	(with-slots ((fields2 fields)) t2
;	  (tc-eq* fields1 fields2 bindings)))))

(defmethod pvs-sxhash* ((t1 recordtype) bindings)
  (with-slots ((fields1 fields)) t1
    (pvs-sxhash-lists-b fields1 bindings (pvs-sxhash-1+ (length bindings)) 127)))

;(defmethod pvs-sxhash* ((t1 recordtype) bindings)
;  (declare (ignore bindings))
;  4)

;(defun tc-eq-fields-type (flds1 flds2 bindings)
;  (declare (list flds1 flds2))
;  (cond ((null flds1) (null flds2))
;	((null flds2) nil)
;	(t (let* ((fld1 (car flds1))
;		  (fld2 (find@ fld1 flds2
;			       #'(lambda (x y)
;				   (and (same-id x y)
;					(tc-eq* (type x) (type y)
;					       bindings))))))
;	     (and fld2
;		  (tc-eq-fields-type (cdr flds1)
;				     (remove@ fld2 flds2 #'eq)
;				     (acons fld1 fld2 bindings)))))))

(defun pvs-sxhash-fields-type (flds1 bindings)
  (declare (list flds1))
  (cond ((null flds1) (sxhash nil))
	(t (let* ((fld1 (car flds1)))
	     (pvs-sxhash-+
	      (the positive-fixnum (pvs-sxhash-id fld1))
	      (pvs-sxhash-+
	       (the positive-fixnum (pvs-sxhash* (type fld1) nil))
	       (the positive-fixnum
		 (pvs-sxhash-fields-type (cdr flds1)
					 (cons fld1 bindings)))))))))

(defun pvs-sxhash-id (x)
  (pvs-sxhash* (typecase x
		 (symbol x)
		 (string (intern x))
		 (t (id x)))
	       nil))

;(defun tc-eq-fields (flds1 flds2 bindings)
;  (cond ((null flds1) (null flds2))
;	((null flds2) nil)
;	(t (let ((fld2 (find@ (car flds1) flds2
;			      #'(lambda (x y)
;				  (same-id (caar (arguments x))
;					   (caar (arguments y)))))))
;	     (and fld2
;		  (tc-eq* (expression (car flds1)) (expression fld2) bindings)
;		  (tc-eq-fields (cdr flds1) (remove@ fld2 flds2) bindings))))))

(defun pvs-sxhash-fields (flds1 bindings hnum)
  (if (null flds1)
      hnum
      (pvs-sxhash-fields
       (cdr flds1)
       bindings
       (pvs-sxhash-+
	(the positive-fixnum (pvs-sxhash-id (caar (arguments (car flds1)))))
	(pvs-sxhash-+
	 (the positive-fixnum (pvs-sxhash* (expression (car flds1)) bindings))
	 (the positive-fixnum hnum))))))

;(defmethod tc-eq* ((f1 field-decl) (f2 field-decl) bindings)
;  (with-slots ((id1 id) (ty1 type)) f1
;    (with-slots ((id2 id) (ty2 type)) f2
;      (or (eq f1 f2)
;	  (and (eq id1 id2)
;	       (tc-eq* ty1 ty2 bindings))))))

(defmethod pvs-sxhash* ((f1 field-decl) bindings)
  (with-slots ((id1 id) (ty1 type)) f1
    (pvs-sxhash-+ (the positive-fixnum (sxhash id1))
		  (the positive-fixnum (pvs-sxhash* ty1 bindings)))))


;;; Expressions

;(defmethod tc-eq* ((e1 field-name-expr) (e2 field-name-expr) bindings)
;  (or (eq e1 e2)
;      (with-slots ((id1 id) (ty1 type)) e1
;	(with-slots ((id2 id) (ty2 type)) e2
;	  (and (eq id1 id2)
;	       (tc-eq* ty1 ty2 bindings))))))

(defmethod pvs-sxhash* ((e1 field-name-expr) bindings)
  (with-slots ((id1 id) (ty1 type)) e1
    (pvs-sxhash-+ (sxhash id1)
		  (the positive-fixnum (pvs-sxhash* ty1 bindings)))))

;(defmethod tc-eq* ((e1 projection-application) (e2 projection-application)
;		   bindings)
;  (or (eq e1 e2)
;      (with-slots ((id1 id) (arg1 argument)) e1
;	(with-slots ((id2 id) (arg2 argument)) e2
;	  (and (eq id1 id2)
;	       (tc-eq* arg1 arg2 bindings))))))

(defmethod pvs-sxhash* ((e1 projection-application) bindings)
  (with-slots (index argument) e1
    (pvs-sxhash-+ (the positive-fixnum index)
		  (pvs-sxhash* argument bindings))))

(defmethod pvs-sxhash* ((e1 injection-application) bindings)
  (with-slots (index argument) e1
    (ldb pvs-sxhash-byte
	 (ash (pvs-sxhash-+ (the positive-fixnum index)
			    (pvs-sxhash* argument bindings))
	      2))))

;(defmethod tc-eq* ((e1 field-application) (e2 field-application)
;		   bindings)
;  (or (eq e1 e2)
;      (with-slots ((id1 id) (arg1 argument)) e1
;	(with-slots ((id2 id) (arg2 argument)) e2
;	  (and (eq id1 id2)
;	       (tc-eq* arg1 arg2 bindings))))))

(defmethod pvs-sxhash* ((e1 field-application) bindings)
  (with-slots ((id1 id) (arg1 argument)) e1
    (pvs-sxhash-+ (the positive-fixnum (sxhash id1))
		  (pvs-sxhash* arg1 bindings))))

;(defmethod tc-eq* ((e1 number-expr) (e2 number-expr) bindings)
;  (declare (ignore bindings))
;  (with-slots ((n1 number)) e1
;    (with-slots ((n2 number)) e2
;      (= (the integer n1) (the integer n2)))))

(defmethod pvs-sxhash* ((e1 number-expr) bindings)
  (declare (ignore bindings))
  (with-slots ((n1 number)) e1
    (sxhash n1)))

;(defmethod tc-eq* ((e1 record-expr) (e2 record-expr) bindings)
;  (or (eq e1 e2)
;      (with-slots ((ass1 assignments)) e1
;	(with-slots ((ass2 assignments)) e2
;	  (tc-eq-fields ass1 ass2 bindings)))))

(defmethod pvs-sxhash* ((e1 record-expr) bindings)
  (with-slots ((ass1 assignments)) e1
    (pvs-sxhash-fields ass1 bindings 0)))

;(defmethod tc-eq* ((e1 tuple-expr) (e2 tuple-expr) bindings)
;  (or (eq e1 e2)
;      (with-slots ((ex1 exprs)) e1
;	(with-slots ((ex2 exprs)) e2
;	  (tc-eq* ex1 ex2 bindings)))))

(defmethod pvs-sxhash* ((e1 tuple-expr) bindings)
  (with-slots ((ex1 exprs)) e1
    (pvs-sxhash-lists-a ex1 bindings 127)))

;(defmethod tc-eq* ((e1 cases-expr) (e2 cases-expr) bindings)
;  (with-slots ((expr1 expression) (sel1 selections) (else1 else-part)) e1
;    (with-slots ((expr2 expression) (sel2 selections) (else2 else-part)) e2
;      (and (tc-eq* expr1 expr2 bindings)
;	   (tc-eq-selections sel1 sel2 bindings)
;	   (tc-eq* else1 else2 bindings)))))
;
;(defun tc-eq-selections (sel1 sel2 bindings)
;  (cond ((null sel1) (null sel2))
;	((null sel2) nil)
;	(t (let ((s2 (find (constructor (car sel1)) sel2
;			   :test #'tc-eq :key #'constructor)))
;	     (and s2
;		  (tc-eq* (car sel1) s2 bindings)
;		  (tc-eq-selections (cdr sel1) (remove s2 sel2) bindings))))))
;
;(defmethod tc-eq* ((s1 selection) (s2 selection) bindings)
;  (with-slots ((args1 args) (ex1 expression)) s1
;    (with-slots ((args2 args) (ex2 expression)) s2
;      (multiple-value-bind (eq? abindings)
;	  (bindings-eq args1 args2 bindings)
;	(and eq?
;	     (tc-eq* ex1 ex2 abindings))))))

(defmethod pvs-sxhash* ((e1 cases-expr) bindings)
  (with-slots (expression else-part) e1
    (pvs-sxhash-+ (the positive-fixnum (pvs-sxhash* expression bindings))
		  ;; Left out selections on purpose; speeds up sxhash
		  ;; computation and only occasionally leads to collision.
		  (the positive-fixnum (pvs-sxhash* else-part bindings)))))

;(defmethod tc-eq* ((e1 application) (e2 application) bindings)
;  (or (eq e1 e2)
;      (with-slots ((op1 operator) (arg1 arguments)) e1
;	(with-slots ((op2 operator) (arg2 arguments)) e2
;	  (and (tc-eq-ops op1 op2 bindings)
;	       (tc-eq* arg1 arg2 bindings))))))

(defmethod pvs-sxhash* ((e1 negation) bindings)
  (with-slots (argument) e1
    (pvs-sxhash-+ 41 (the positive-fixnum (pvs-sxhash* argument bindings)))))

(defmethod pvs-sxhash* ((e1 conjunction) bindings)
  (with-slots (argument) e1
    (pvs-sxhash* (collect-conjuncts argument) bindings)))

(defmethod pvs-sxhash* ((e1 disjunction) bindings)
  (with-slots (argument) e1
    (pvs-sxhash* (collect-disjuncts argument) bindings)))

(defmethod pvs-sxhash* ((e1 implication) bindings)
  (with-slots (argument) e1
    (pvs-sxhash-+ 53 (the positive-fixnum (pvs-sxhash* argument bindings)))))

(defmethod pvs-sxhash* ((e1 iff) bindings)
  (with-slots (argument) e1
    (pvs-sxhash-+ 59 (the positive-fixnum (pvs-sxhash* argument bindings)))))

(defmethod pvs-sxhash* ((e1 equation) bindings)
  (with-slots (argument) e1
    (pvs-sxhash-+ 61 (the positive-fixnum (pvs-sxhash* argument bindings)))))

(defmethod pvs-sxhash* ((e1 branch) bindings)
  (with-slots (argument) e1
    (pvs-sxhash-+ 71 (the positive-fixnum (pvs-sxhash* argument bindings)))))

(defmethod pvs-sxhash* ((e1 application) bindings)
  (with-slots ((op1 operator) (arg1 argument)) e1
    (pvs-sxhash-+ (the positive-fixnum (pvs-sxhash-ops op1 bindings))
		  (the positive-fixnum (pvs-sxhash* arg1 bindings)))))

;(defmethod tc-eq-ops ((op1 field-name-expr) (op2 field-name-expr) bindings)
;  (with-slots ((id1 id) (ty1 type)) op1
;    (with-slots ((id2 id) (ty2 type)) op2
;      (and (eq id1 id2)
;	   (or (not *strong-tc-eq-flag*)
;	       (tc-eq* ty1 ty2 bindings))))))

(defmethod pvs-sxhash-ops ((op1 field-name-expr) bindings)
  (declare (ignore bindings))
  (with-slots ((id1 id) (ty1 type)) op1
    (the positive-fixnum (sxhash id1))))
       
;(defmethod tc-eq-ops ((op1 field-name-expr) (op2 name-expr) bindings)
;  (declare (ignore bindings))
;  nil)
;
;(defmethod tc-eq-ops ((op1 name-expr) (op2 field-name-expr) bindings)
;  (declare (ignore bindings))
;  nil)
;
;(defmethod tc-eq-ops (op1 op2 bindings)
;  (tc-eq* op1 op2 bindings))

(defmethod pvs-sxhash-ops (op1 bindings)
  (pvs-sxhash* op1 bindings))

;(defmethod tc-eq* ((e1 binding-expr) (e2 binding-expr) ibindings)
;  (or (eq e1 e2)
;      (with-slots ((b1 bindings) (ex1 expression)) e1
;	(with-slots ((b2 bindings) (ex2 expression)) e2
;	  (and (same-binding-op? e1 e2)
;	       (multiple-value-bind (eq? abindings)
;		   (bindings-eq b1 b2 ibindings)
;		 (and eq?
;		      (tc-eq* ex1 ex2 (append abindings ibindings)))))))))

(defmethod pvs-sxhash* ((e1 binding-expr) ibindings)
  (with-slots ((b1 bindings) (ex1 expression)) e1
    (pvs-sxhash-+
     (the positive-fixnum (pvs-sxhash-binding-op e1))
     (multiple-value-bind (hash abindings)
	 (pvs-sxhash-bindings-eq b1 ibindings
				 (pvs-sxhash-1+ (length ibindings)))
       (the positive-fixnum
	 (pvs-sxhash-+
	  (the positive-fixnum hash)
	  (the positive-fixnum (pvs-sxhash* ex1 (append abindings ibindings)))))))))

;(defun bindings-eq (b1 b2 bindings)
;  (declare (list b1 b2))
;  (let ((nbindings (make-compatible-bindings b1 b2 bindings)))
;    (if nbindings
;	(values t nbindings)
;	(bindings-eq* b1 b2 bindings nil))))

;; I am going to ignore make-compatible-bindings for now.
;; (Assuming it will go away some time.)
(defun pvs-sxhash-bindings-eq (b1 bindings num)
  (declare (list b1))
  (pvs-sxhash-bindings-eq* b1 bindings num 0 nil))

;(defun bindings-eq* (b1 b2 bindings result)
;  (cond ((null b1)
;	 (unless b2
;	   (values t result)))
;	((null b2) nil)
;	(t (when (tc-eq* (type (car b1)) (type (car b2))
;			 bindings)
;	     (bindings-eq* (cdr b1) (cdr b2) bindings
;			   (cons (cons (car b1) (car b2))
;				 result))))))

(defun pvs-sxhash-bindings-eq* (b1 bindings bnum hnum nbindings)
  (if (null b1)
      (values hnum nbindings)
      (pvs-sxhash-bindings-eq*
       (cdr b1)
       bindings
       (pvs-sxhash-1+ bnum)
       (pvs-sxhash-+ (the positive-fixnum
		       (pvs-sxhash* (type (car b1))
				    (append nbindings bindings)))
		     (the positive-fixnum hnum))
       (acons (car b1) bnum nbindings))))

;(defun bindings-eq (b1 b2 bindings)
;  (declare (list b1 b2))
;  (labels ((beq (b1 b2 bindings result)
;	     (if (null b1)
;		 (unless b2
;		   (values t result))
;		 (when (tc-eq* (type (car b1)) (type (car b2)) bindings)
;		   (beq (cdr b1) (cdr b2) bindings
;			(cons (cons (car b1) (car b2))
;			      result))))))
;    (beq b1 b2 bindings nil)))

;(defun pvs-sxhash-bindings-eq (b1 bindings)
;  (declare (list b1))
;  (labels ((beq (b1 bindings result num)
;	     (if (null b1)
;		 (values (sxhash nil) result)
;		 (multiple-value-bind (hash new-bindings)
;		     (beq (cdr b1) bindings
;			(cons (cons (car b1) num) result) (1+ (the integer
;								   num)))
;		   (values
;		    (+ (the integer (pvs-sxhash* (type (car b1)) bindings))
;		       (the integer hash))
;		    new-bindings)))))
;    (beq b1 bindings nil 1)))

;(defun bindings-eq* (b1 b2 bindings result)
;  (if (null b1)
;      (values t result)
;      (when (tc-eq* (type (car b1)) (type (car b2)) bindings)
;	(bindings-eq* (cdr b1) (cdr b2) bindings
;		      (cons (cons (car b1) (car b2))
;			    result)))))

;(defmethod same-binding-op? ((op1 lambda-expr) (op2 lambda-expr))
;  t)

(defmethod pvs-sxhash-binding-op ((op1 lambda-expr))
  1)

;(defmethod same-binding-op? ((op1 forall-expr) (op2 forall-expr))
;  t)

(defmethod pvs-sxhash-binding-op ((op1 forall-expr))
  2)

;(defmethod same-binding-op? ((op1 exists-expr) (op2 exists-expr))
;  t)

(defmethod pvs-sxhash-binding-op ((op1 exists-expr))
  3)

;(defmethod same-binding-op? (op1 op2)
;  (declare (ignore op1 op2))
;  nil)

(defmethod pvs-sxhash-binding-op (op1)
  (declare (ignore op1))
  4)

;;NSH: to ignore coercions;;;;;;
;(defmethod tc-eq* ((A coercion) (B expr) bindings)
;  (tc-eq* (args1 A) B bindings))

;(defmethod pvs-sxhash* ((A coercion) bindings)
;  (pvs-sxhash* (args1 A) bindings))

;(defmethod tc-eq* ((A expr) (B coercion) bindings)
;  (tc-eq* A (args1 B) bindings))

;(defmethod tc-eq* ((A name-expr) (B coercion) bindings)
;  (tc-eq* A (args1 B) bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defmethod tc-eq* ((e1 update-expr) (e2 update-expr) bindings)
;  (or (eq e1 e2)
;      (with-slots ((ex1 expression) (ass1 assignments)) e1
;	(with-slots ((ex2 expression) (ass2 assignments)) e2
;	  (and (tc-eq* ex1 ex2 bindings)
;	       (tc-eq* ass1 ass2 bindings))))))

(defmethod pvs-sxhash* ((e1 update-expr) bindings)
  (with-slots ((ex1 expression) (ass1 assignments)) e1
    (pvs-sxhash-+ (the positive-fixnum (pvs-sxhash* ex1 bindings))
		  (the positive-fixnum (pvs-sxhash* ass1 bindings)))))

;(defmethod tc-eq* ((a1 assignment) (a2 assignment) bindings)
;  (or (eq a1 a2)
;      (with-slots ((args1 arguments) (ex1 expression)) a1
;	(with-slots ((args2 arguments) (ex2 expression)) a2
;	  (and (tc-eq* args1 args2 bindings)
;	       (tc-eq* ex1 ex2 bindings))))))

(defmethod pvs-sxhash* ((a1 assignment) bindings)
  (with-slots ((args1 arguments) (ex1 expression)) a1
    (pvs-sxhash-+ (the positive-fixnum (pvs-sxhash* args1 bindings))
		  (the positive-fixnum (pvs-sxhash* ex1 bindings)))))

;(defmethod tc-eq* ((n1 binding) (n2 binding) bindings)
;  (declare (ignore bindings))
;  (eq n1 n2))

(defmethod pvs-sxhash* ((n1 binding) bindings)
  (declare (ignore bindings))
  (sxhash n1))

;;; Make sure we don't allow bindings and names to be tc-eq*

;(defmethod tc-eq* ((n1 binding) (n2 name) bindings)
;  (declare (ignore bindings))
;  nil)
;
;(defmethod tc-eq* ((n1 name) (n2 binding) bindings)
;  (declare (ignore bindings))
;  nil)
;
;(defmethod tc-eq* ((n1 name) (n2 modname) bindings)
;  (declare (ignore bindings))
;  nil)
;
;(defmethod tc-eq* ((n1 modname) (n2 name) bindings)
;  (declare (ignore bindings))
;  nil)

;(defmethod tc-eq* ((n1 name) (n2 name) bindings)
;  (or (eq n1 n2)
;      (let ((decl1 (declaration n1))
;	    (decl2 (declaration n2)))
;	(or (let ((bind (cdr (assq decl1 bindings))))
;	      (and bind
;		   (eq decl2 (if (consp bind)
;				 (car bind)
;				 bind))))
;	    (and (or (eq decl1 decl2)
;		     ;; Handle field-decls separately - it might be better
;		     ;; to ensure that field accessors of tc-eq
;		     ;; recordtypes have eq declarations - needs further
;		     ;; research.
;		     (and (eq (id n1) (id n2));;(id decl1) (id decl2)
;			  (typep decl1 'field-decl)
;			  (typep decl2 'field-decl)
;			  (tc-eq* (type n1) (type n2) bindings)))
;		 (let ((mi1 (module-instance n1))
;		       (mi2 (module-instance n2)))
;		   (if mi1
;		       (and mi2
;			    (eq (id mi1) (id mi2))
;			    (tc-eq* (actuals mi1) (actuals mi2) bindings))
;		       (null mi2))))
;	    (tc-eq-operators (id n1) (id n2) decl1 decl2)))))

(defmethod pvs-sxhash* ((n1 name) bindings)
  (with-slots (resolutions) n1
    (pvs-sxhash* (car resolutions) bindings)))

;(defun tc-eq-operators (id1 id2 decl1 decl2)
;  (let* ((th1 (module decl1))
;	 (th2 (module decl2))
;	 (thid (when th1 (id th1))))
;    (and th1 th2
;	 (eq thid (id (module decl2)))
;	 (eq thid '|booleans|)
;	 (when (or (and (memq id1 '(& AND))
;			(memq id2 '(& AND)))
;		   (and (memq id1 '(=> IMPLIES))
;			(memq id2 '(=> IMPLIES)))
;		   (and (memq id1 '(<=> IFF))
;			(memq id2 '(<=> IFF))))
;	   t))))

;(defun pvs-sxhash-operators (id1 decl1)
;  (let* ((th1 (module decl1))
;	 (thid (when th1 (id th1))))
;    (+ (sxhash thid)
;       (sxhash id1))))

;(defmethod tc-eq* ((n1 modname) (n2 modname) bindings)
;  (or (eq n1 n2)
;      (with-slots ((id1 id) (act1 actuals)) n1
;	(with-slots ((id2 id) (act2 actuals)) n2
;	  (and (eq id1 id2)
;	       (tc-eq* act1 act2 bindings))))))

(defmethod pvs-sxhash* ((n1 modname) bindings)
  (with-slots (id actuals) n1
    (pvs-sxhash-+ (the positive-fixnum (sxhash id))
		  (the positive-fixnum (pvs-sxhash* actuals bindings)))))

;(defmethod tc-eq* ((a1 actual) (a2 actual) bindings)
;  (or (eq a1 a2)
;      (with-slots ((tv1 type-value) (ex1 expr)) a1
;	(with-slots ((tv2 type-value) (ex2 expr)) a2
;	  (if tv1
;	      (and tv2
;		   (tc-eq* tv1 tv2 bindings))
;	      (and (not tv2)
;		   (tc-eq* ex1 ex2 bindings)))))))

(defmethod pvs-sxhash* ((a1 actual) bindings)
  (with-slots ((tv1 type-value) (ex1 expr)) a1
    (if tv1
	(pvs-sxhash* tv1 bindings)
	(pvs-sxhash ex1 bindings))))

;(defmethod tc-eq* ((r1 resolution) (r2 resolution) bindings)
;  (or (eq r1 r2)
;      (with-slots ((d1 declaration) (mi1 module-instance) (t1 type)) r1
;	(with-slots ((d2 declaration) (mi2 module-instance) (t2 type)) r2
;	  (or (and (eq d1 d2)
;		   (tc-eq* mi1 mi2 bindings))
;	      (and (typep d1 'field-decl)
;		   (typep d2 'field-decl)
;		   (same-id d1 d2)
;		   (tc-eq* t1 t2 bindings)))))))

(defmethod pvs-sxhash* ((r1 resolution) bindings)
  (with-slots ((d1 declaration) (mi1 module-instance)) r1
    (or (cdr (assq d1 bindings))
	(let* ((id (id d1))
	       (dnum (sxhash id)))
	  (pvs-sxhash-+ (the positive-fixnum dnum)
			(if (binding? d1)
			    17
			    (the positive-fixnum
			      (pvs-sxhash* mi1 bindings))))))))

;;; for testing:

;(defmethod tc-eq* :around (t1 t2 bindings)
;  (let ((res (call-next-method)))
;    (assert (when (and res (null bindings))
;	      (= (pvs-sxhash t1) (pvs-sxhash t2))))
;    res))
