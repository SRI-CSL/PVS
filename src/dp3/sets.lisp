(in-package dp3)

;; Sets abstraction {x | P(x)}

(defun set-abst-p (trm)
  (declare (type node trm))
  (and (lambda-p trm)
       (= (length (args trm)) 2)
       (dp-variable-p (arg 1 trm))
       (bool-p (arg 2 trm))))

(defun destructure-set (trm)
  (declare (type node trm))
  #+dbg(assert (set-abst-p trm))
  (values (arg 1 trm) (arg 2 trm)))

(defun mk-set-abst (x body)
  (mk-lambda x body))

(defun combine-sets (fn s1 &rest sets)
  (declare (type function fn)
	   (type node s1)
	   (type list sets))
  (multiple-value-bind (x1 p1)
      (destructure-set s1)
    (multiple-value-bind (x2 p2)
	(destructure-set s2)
      (let ((bodies (mapcar #'(lambda (set)
				(multiple-value-bind (x2 p2)
				    (destructure-set s2)
				  (if (eq x1 x2) s2
				      (replace-by s2 (acons x2 x1 nil)))))
		      sets)))
	(mk-set-abst x1 (apply fn s1 sets))))))

;; Recognizing sets

(defun set-p (trm)
  ; (declare (type node trm))
  #+dbg(assert (node-p trm))
  (or (eq (node-initial-type trm) *set*)
      (and (application-p trm)
	   (eq (the symbol (node-initial-type (funsym trm))) 'set-op))
      (set-abst-p trm)))

;; Canonizing sets

(defun sig-set (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  #+dbg(assert (set-p trm))
  (cond ((intersection-p trm)
	 (sig-intersection trm cs))
	((union-p trm)
	 (sig-union trm cs))
	((complement-p trm)
	 (sig-complement trm))
	((ite-p trm)
	 (sig-ite-set trm))
	(t (sig-atomic-set trm))))

(defun sig-subset (trm cs)
  (let ((lhs (lhs trm))
	(rhs (rhs trm)))
    (cond ((or (eq lhs rhs)
	       (emptyset-p lhs)
	       (fullset-p rhs))
	   (list *true*))
	  ((and (fullset-p lhs)
		(emptyset-p rhs))
	   (list *false*))
	  (t trm))))

(defun sig-atomic-set (trm)
  (declare (type node trm))
  (with-bdd-environment (*emptyset* *fullset* 'set-op)
     (bdd-var trm)))

(defun sig-set-diseq (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (multiple-value-bind (lhs rhs)
      (destructure-disequality trm)
    (if (and (set-abst-p lhs) (set-abst-p rhs))
	(list (combine-sets #'mk-xor lhs rhs))
        (with-bdd-environment (*emptyset* *fullset* 'set-op)
	   (list (bdd-xor lhs rhs))))))

(defun sig-set-eq (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (let ((lhs (lhs trm))
	(rhs (rhs trm)))
    (if (and (set-abst-p lhs) (set-abst-p rhs))
	(list (combine-sets #'mk-equality lhs rhs))
      (list trm))))
       
(defun sig-intersection (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (let ((lhs (lhs trm))
	(rhs (rhs trm)))
    (if (and (set-abst-p lhs) (set-abst-p rhs))
	(combine-sets #'mk-conjunction lhs rhs)
        (with-bdd-environment (*emptyset* *fullset* 'set-op)
	   (bdd-and lhs rhs)))))

(defun sig-union (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (let ((lhs (lhs trm))
	(rhs (rhs trm)))
    (if (and (set-abst-p lhs) (set-abst-p rhs))
	(combine-sets #'mk-conjunction lhs rhs)
        (with-bdd-environment (*emptyset* *fullset* 'set-op)
	   (bdd-or lhs rhs)))))

(defun sig-complement (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (with-bdd-environment (*emptyset* *fullset* 'set-op)
     (bdd-not (arg 1 trm))))

(defun sig-ite-set (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (multiple-value-bind (t1 t2 t3)
      (destructure-ite trm)
    (if (and (set-abst-p t1)
	     (set-abst-p t2)
	     (set-abst-p t3))
	(combine-sets #'mk-ite t1 t2 t3)
        (with-bdd-environment (*emptyset* *fullset* 'set-op)
	   (bdd-ite t1 t2 t3)))))

;; Solving sets

(defun set-solve-neq (neq cs)
  (declare (type node neq)
	   (ignore cs))
  #+dbg(assert (disequality-p neq))
  (multiple-value-bind (lhs rhs)
      (destructure-disequality neq)
    (with-bdd-environment (*emptyset* *fullset* 'set-op)
        (bdd-solve (bdd-xor lhs rhs)))))

(defun set-solve (eqn cs)
  (declare (type node eqn)
           (ignore cs))
  #+dbg(assert (equality-p eqn))
  (with-bdd-environment (*emptyset* *fullset* 'set-op)
     (bdd-solve (bdd-iff (lhs trm) (rhs trm)))))

(defun solve-subset (trm cs)
  (declare (ignore cs))
  #+dbg(assert (subset-p trm))
  (with-bdd-environment (*emptyset* *fullset* 'set-op)
     (bdd-solve (bdd-implies (lhs trm) (rhs trm)))))













