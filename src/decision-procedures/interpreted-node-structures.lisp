;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intepreted-node-structures.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defun mk-predicate-sym (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result) 'predicate)
    (setf (node-interpreted? result) t)
    result))

(defvar *=* (mk-predicate-sym '=))

(defmacro mk-equality (t1 t2)
  `(mk-term (list *=* ,t1 ,t2)))

(defmacro lhs (equality)
  `(arg 1 ,equality))

(defmacro rhs (equality)
  `(arg 2 ,equality))

(defun equality-p (term)
  (and (application-p term)
       (eq (funsym term) *=*)))

(defvar *true* (mk-constant 'TRUE))
(defvar *false* (mk-constant 'FALSE))

(defmacro true-p (term)
  `(eq ,term *true*))

(defmacro false-p (term)
  `(eq ,term *false*))

(defmacro true-or-false-p (term)
  `(or (eq ,term *false*) (eq ,term *true*)))

(defun mk-arith-operator (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'arith-op)
    (setf (node-interpreted? result) t)
    result))

(defun arith-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'arith-op))

(defvar *plus* (mk-arith-operator 'plus))
(defvar *times* (mk-arith-operator 'times))
(defvar *divide* (mk-arith-operator 'divide))
(defvar *minus* (mk-arith-operator 'minus))
(defvar *difference* (mk-arith-operator 'difference))
(defvar *sup* (mk-arith-operator 'sup))
(defvar *inf* (mk-arith-operator 'inf))
(defvar *floor* (mk-arith-operator 'floor))
(defvar *ceiling* (mk-arith-operator 'ceiling))

;;interpreted arithmetic function symbols
(defvar *arithfuns*
  (list *plus* *times* *divide* *minus* *difference* *sup* *inf*))

(defun mk-arith-pred (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'arith-pred)
    (setf (node-interpreted? result) t)
    result))

(defvar *lesseqp* (mk-arith-pred 'lesseqp))
(defvar *lessp* (mk-arith-pred 'lessp))
(defvar *greatereqp* (mk-arith-pred 'greatereqp))
(defvar *greaterp* (mk-arith-pred 'greaterp))

;;interpreted arithmetic predicate symbols
(defvar *arithpreds*
  (list *lesseqp* *lessp* *greaterp* *greatereqp*))

;;is the top fun. symb. of term arithmetic.
(defun arith-p (term)
  (declare (type node term))
  (cond
   ((application-p term)
    (arith-op-p (the node (funsym (the application term)))))
   ((constant-p term)
    (or (numberp (constant-id (the constant term)))
	(eq (node-type term)  *integer*)
	(eq (node-type term) *number*)))
   (t nil)))

(defvar *number-type-preds* '(integer-pred rational-pred real-pred))

(defun number-type-p (term)
  (and (application-p term)
       (member (node-initial-type (funsym term)) *number-type-preds*
	       :test #'eq)))

(defvar *not* (mk-predicate-sym 'not))
(defvar *nequal* (mk-predicate-sym 'nequal))

(defvar *and* (mk-predicate-sym 'and))
(defvar *or* (mk-predicate-sym 'or))
(defvar *implies* (mk-predicate-sym 'implies))

(defvar *preds*
  (list *not* *and* *or* *nequal* *implies*
	*lesseqp* *lessp* *greaterp* *greatereqp*))

(defun bool-p (term)
  (declare (type node term))
  (cond
   ((application-p term)
    (or (eq (node-type (the node (funsym (the application term))))
	    'predicate)
	(eq (node-type (the node (funsym (the application term))))
	    'arith-pred)))
   ((constant-p term)
    (or (eq term *true*)
	(eq term *false*)
	(eq (node-initial-type term) *boolean*)))
   (t nil)))

(defun arith-pred-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'arith-pred))

(defun arith-bool-p (term)
  (declare (type node term))
  (and (application-p term)
       (arith-pred-p (the node (funsym (the application term))))))

(defun negation-p (term)
  (declare (type node term))
  (and (application-p term)
       (eq (funsym (the application term)) *not*)))

(defun mk-negation (term)
  (mk-term (list *not* term)))

(defun neq-p (term)
  (and (equality-p term)
       (eq (rhs term) *false*)
       (equality-p (lhs term))))

(defun mk-nequality (lhs rhs)
  (mk-equality (mk-equality lhs rhs) *false*))

(defun nequal-p (term)
  (declare (type node term))
  (and (application-p term)
       (eq (funsym (the application term)) *nequal*)))

(defvar *if* (mk-interpreted-constant 'if))

(defun mk-if-then-else (cond then else)
  (mk-term (list *IF* cond then else)))

(defun if-p (term)
  (and (application-p term)
       (eq (funsym term) *if*)))

(defun if-cond (term)
  (arg 1 term))

(defun if-then (term)
  (arg 2 term))

(defun if-else (term)
  (arg 3 term))

(defun and-p (term)
  (and (application-p term)
       (eq (funsym term) *and*)))

(defun or-p (term)
  (and (application-p term)
       (eq (funsym term) *or*)))

(defmacro conjunction-p (trm)
  `(and-p ,trm))
 
(defmacro disjunction-p (trm)
  `(or-p ,trm))

(defun implication-p (term)
  (and (application-p term)
       (eq (funsym term) *implies*)))

(defun dp-numberp (term)
  (declare (type node term))
  (and (constant-p term)
       (numberp (constant-id (the constant term)))))

(defun dp-integerp (term)
  (declare (type node term))
  (and (constant-p term)
       (integerp (constant-id (the constant term)))))

(defun mk-dp-number (num)
  (mk-constant num))

(defvar *zero* (mk-dp-number 0))
(defvar *one* (mk-dp-number 1))
(defvar *neg-one* (mk-dp-number -1))

(defmacro dp-zerop (term)
  `(eq ,term *zero*))

(defmacro dp-onep (term)
  `(eq ,term *one*))

(defmacro dp-minusp (const)
  `(minusp (constant-id (the constant ,const))))

(defun mk-array-operator (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'array-op)
    (setf (node-interpreted? result) t)
    result))

(defun array-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'array-op))

(defvar *update* (mk-array-operator 'update))

(defvar *tuple* (mk-interpreted-constant 'tuple 'tuple-op))

(defun tuple-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'tuple-op))

(defvar *record* (mk-interpreted-constant 'record 'record-op))

(defun record-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'record-op))

(defvar *project* (mk-interpreted-constant 'project 'project-op))

(defun project-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'project-op))

(defvar *th-app* (mk-interpreted-constant 'th-app 'th-app-op))

(defun th-app-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'th-app-op))

(defun th-app-p (term)
  (and (application-p term)
       (th-app-op-p (funsym term))))

(defvar *lambda* (mk-constant 'lambda))

(defun mk-conjunction (trm1 trm2)
  (declare (type node trm))
  (mk-term (list *and* trm1 trm2)))

(defun mk-conjunction* (trms &optional (acc *true*))
  (if (null trms) acc
      (let ((trm (car trms)))
	(cond ((true-p trm)
	       (mk-conjunction* (cdr trms) acc))
	      ((false-p trm)
	       *false*)
	      (t (mk-conjunction* (cdr trms)
				  (mk-conjunction trm acc)))))))

(defun mk-disjunction (trm1 trm2)
  (mk-term (list *or* trm1 trm2)))

(defun mk-implication (trm1 trm2)
  (mk-term (list *implies* trm1 trm2)))

(defun disequality-p (trm)
  (and (equality-p trm)
       (equality-p (lhs trm))
       (false-p (rhs trm))))

(defun mk-disequality (trm1 trm2)
  (mk-equality (mk-equality trm1 trm2) *false*))

(defun destructure-disequality (trm)
  #+dbg(assert (disequality-p trm))
  (let ((arg (lhs trm)))
    (values (lhs arg) (rhs arg))))

(defmacro uninterp? (term)
  `(not (interp? ,term)))

(defmacro interpsym? (sym)
  `(node-interpreted? ,sym))

(defun interp? (term)
  (or (true-p term)
      (false-p term)
      (dp-numberp term)
      (and (application-p term)
	   (or (interpsym? (funsym term))
	       (and (application-p (funsym term))
		    (interpsym? (funsym (funsym term))))))
      (applyupdate-p term)))


(defun occurs-under-interp (term1 term2)
  (cond
   ((eq term1 term2) t)
   ((constant-p term2) nil)
   ((uninterp? term2) nil)
   (t (some #'(lambda (x) (occurs-under-interp term1 x))
	    (funargs term2)))))

(defun bv-p (term)
  (bvec::is-bv? term))

(defun dp-rational-atom-p (term cong-state)
  (dp-integer-atom-p term cong-state))

(defun dp-real-atom-p (term)
  t)

(defun number-theory-p (term cong-state)
  (or (arith-p term)
      (member (dp-type term cong-state)
	      (list *integer* *number*))))
	    
(defun number-equality-p (eqn cong-state)
  (and
   (equality-p eqn)
   (number-theory-p (lhs eqn) cong-state)
   (number-theory-p (rhs eqn) cong-state)))
