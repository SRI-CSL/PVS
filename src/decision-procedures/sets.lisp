(in-package dp)

;; Signature

(defun mk-set-operator (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result) 'set-op)
    (setf (node-interpreted? result) t)
    result))

(defun set-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'set-op))

(defvar *emptyset* (mk-constant 'emptyset))
(defvar *fullset* (mk-constant 'fullset))

(defvar *union* (mk-set-operator 'union))
(defvar *intersection* (mk-set-operator 'intersection))
(defvar *complement* (mk-set-operator 'complement))
(defvar *ite-set* (mk-set-operator 'ite-set)) ; ite(C, A, B) == CA + ~CA

(defmacro mk-union (lhs rhs)
  `(mk-term (list *union* ,lhs ,rhs)))

(defmacro mk-intersection (lhs rhs)
  `(mk-term (list *intersection* ,lhs ,rhs)))

(defmacro mk-complement (set)
  `(mk-term (list *complement* ,set)))

(defun mk-ite-set (cond-set then-set else-set)
  (cond ((true-p cond-set) then-set)
	((false-p cond-set) else-set)
	(t (mk-term (list *ite-set* cond-set then-set else-set)))))

(defun cond-of-ite-set (trm)
   (arg 0 trm))

(defun then-of-ite-set (trm)
   (arg 1 trm))

(defun else-of-ite-set (trm)
   (arg 2 trm))

(defun emptyset-p (trm)
  (eq trm *emptyset*))

(defun fullset-p (trm)
  (eq trm *fullset*))

(defun union-p (trm)
  (and (application-p trm)
       (eq (funsym trm) *union*)))

(defun intersection-p (trm)
  (and (application-p trm)
       (eq (funsym trm) *intersection*)))

(defun complement-p (trm)
  (and (application-p trm)
       (eq (funsym trm) *complement*)))

(defun ite-set-p (trm)
  (and (application-p trm)
       (eq (funsym trm) *ite-set*)))

(defun set-p (trm)
  (or (eq (node-type trm) 'set-op)
      (emptyset-p trm)
      (fullset-p trm)
      (union-p trm)
      (intersection-p trm)
      (complement-p trm)))

;; Canonizing sets

(defun sigset (trm cs)
  (declare (ignore cs))
  (if (equality-p trm)
      (sig-set-equality trm)
    (sig-set-trm trm)))

(defun sig-set-equality (eqn)
  #+dbg(assert (equality-p eqn))
  (bdd-init)
  (let ((bdd1 (set-to-bdd (lhs eqn)))
	(bdd2 (set-to-bdd (rhs eqn))))
    (if (bdd-equal? bdd1 bdd2) *true*
	(mk-equality (bdd-to-ite-set bdd1)
		     (bdd-to-ite-set bdd2)))))
  
(defun sig-set-trm (trm)
  (bdd-init)
  (let ((bdd (set-to-bdd trm)))
    (bdd-to-ite-set bdd)))

(defun set-to-bdd (trm)
  (cond ((emptyset-p trm)
	 (bdd-0))
	((fullset-p trm)
	 (bdd-1))
	((union-p trm)
	 (bdd-or*
	  (mapcar #'set-to-bdd (funargs trm))))
	((intersection-p trm)
	 (bdd-and*
	  (mapcar #'set-to-bdd (funargs trm))))
	((complement-p trm)
	 (bdd-not (set-to-bdd (arg 1 trm))))
	((ite-set-p trm)
	 (bdd-ite (set-to-bdd (arg 1 trm))
		  (set-to-bdd (arg 2 trm))
		  (set-to-bdd (arg 3 trm))))
	((equality-p trm)
	 (bdd-equiv (set-to-bdd (lhs trm))
		    (set-to-bdd (rhs trm))))
	(t (bdd-var trm))))

(defun bdd-to-ite-set (bdd)
  (cond ((bdd-0? bdd)
	 *emptyset*)
	((bdd-1? bdd)
	 *fullset*)
	((bdd-poslit? bdd)
	 (lookup-trm-of (bdd-varid bdd)))
	((bdd-neglit? bdd)
	 (mk-complement (lookup-trm-of (bdd-varid bdd))))
	((or (bdd-X? bdd) (bdd-void? bdd))
	 (break))
	(t (mk-ite-set (lookup-trm-of (bdd-varid bdd))
		       (bdd-to-ite-set (bdd-then bdd))
		       (bdd-to-ite-set (bdd-else bdd))))))

;; Solving sets

(defun set-solve-neq (neq cs)
  (declare (ignore cs))
  (list neq))

(defun set-solve (eqn cs)
  (declare (ignore cs))
  #+dbg(assert (equality-p eqn))
  (bdd-init)
  (let* ((bdd1 (set-to-bdd (lhs eqn)))
	 (bdd2 (set-to-bdd (rhs eqn)))
	 (solved-form (bdd-solve
		       (bdd-equiv bdd1 bdd2)
		       #'new-bdd-id)))
    (cond ((listp solved-form)
	   (mapcar #'(lambda (assoc)
		       #+dbg(assert (consp assoc))
		       (destructuring-bind (x . sbdd) assoc
			 (mk-equality (lookup-trm-of (bdd-varid x))
				      (bdd-to-ite-set sbdd))))
	     solved-form))
	  ((bdd-0? solved-form)
	   (list *false*))
	  ((bdd-1? solved-form)
	   ())
	  (t (break)))))






















