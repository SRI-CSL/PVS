(in-package dp)

(defun term-var-type (var cong-state)
  (cond ((strict-var-p var) 'strict)
	((nonneg-var-p var) t)
	(t (term-var-type-uninterp var cong-state))))

(defun term-type (term cong-state)
  (term-var-type (term-var term) cong-state))

(defun term-var-type-uninterp (term cong-state)
  (cond ((strict-p term cong-state) 'strict)
	((nonneg-p term cong-state) 'nonneg)
	(t nil)))

;;prefix for internally created (non-negative AA, positive AB variables )
(defvar *nonneg-var-prefix* "non-neg-")
(defvar *strict-nonneg-var-prefix* "strict-")
(defvar *nonzero-var-prefix* "non-zero-")

;;gentemps for internal variables.

(defvar *max-ineq-var-count* 0)

;(defdpstruct (ineq-constant (:include constant))
;  (index 0 :type fixnum))


(defun mk-ineq-var (id cong-state)
  (setf (dp-gethash id *term-hash*)
	(mk-ineq-var* id cong-state)))

(defun mk-ineq-var* (id cong-state)
  (let* ((poly-s (polyhedral-structure cong-state))
	 (max-vars (polyhedral-structure-max-vars poly-s))
	 (ineq-var-count (polyhedral-structure-ineq-var-count poly-s))
	 (new-ineq-var-count (1+ ineq-var-count)))
    (break)
    (setq *max-ineq-var-count*
	  (max *max-ineq-var-count* new-ineq-var-count))
    (when (> new-ineq-var-count max-vars)
      (extend-polyhedral-structure poly-s))
    (setf (polyhedral-structure-ineq-var-count poly-s) new-ineq-var-count)
    (let ((new-ineq-var (make-ineq-constant :sxhash (dp-sxhash id)
					    :id id
					    :index new-ineq-var-count)))
      (setf (aref (polyhedral-structure-ineq-var-index-array poly-s)
		  new-ineq-var-count)
	    new-ineq-var)
      new-ineq-var)))

(defun make-ineq-var (strict initial-type cong-state)
  (if strict
      (mk-strict-ineq-var initial-type cong-state)
      (mk-nonneg-var initial-type cong-state)))

(defun mk-strict-ineq-var (initial-type &optional cong-state)
  (let* ((id (gentemp *strict-nonneg-var-prefix*))
	 (result (mk-ineq-var id cong-state)))
    (setf (constant-type result) 'strict)
    (setf (constant-initial-type result) initial-type)
    (when cong-state
      (setf (dp-type result cong-state) 'strict))
    result))

(defun mk-nonneg-var (initial-type &optional cong-state)
  (let* ((id (gentemp *nonneg-var-prefix*))
	 (result (mk-ineq-var id cong-state)))
    (setf (constant-type result) 'nonneg)
    (setf (constant-initial-type result) initial-type)
    (when cong-state
      (setf (dp-type result cong-state) 'nonneg))
    result))

(defun mk-nonzero-var (initial-type &optional cong-state)
  (let* ((id (gentemp *nonzero-var-prefix*))
	 (result (mk-constant id)))
    (set (constant-type result) 'nonzero)
    (setf (constant-initial-type result) initial-type)
    (when cong-state
      (setf (dp-type result cong-state) 'nonzero))
    result))

;;checks if var is a nonneg-var.
(defun nonneg-var-p (var)
  (and (constant-p var)
       (eq (constant-type var) 'nonneg)))

;;checks if var is a strict-var.
(defun strict-var-p (var)
  (and (constant-p var)
       (eq (constant-type var) 'strict)))

;;checks if var is a nonneg-var.
(defun nonzero-var-p (var)
  (and (constant-p var)
       (eq (constant-type var) 'nonzero)))

(defun nonneg-p (term cong-state)
  (eq (dp-type term cong-state) 'nonneg))

(defun strict-p (term cong-state)
  (eq (dp-type term cong-state) 'strict))

(defun nonzero-p (term cong-state)
  (eq (dp-type term cong-state) 'nonzero))

(defun arith-solve (eqn cong-state)
  (let ((pure-eqns (purify-eqn eqn cong-state)))
    (loop for pe in pure-eqns
	  nconc (arith-solve-pure pe cong-state))))

(defun arith-solve-pure (pure-eqn cong-state)
  (cond
   ((ineq-p pure-eqn)
    (add-pure-ineq pure-eqn cong-state))
   ((ineq-p (lhs pure-eqn))
    (add-pure-ineq (lhs pure-eqn) cong-state))
   (t (solve-normed-arith-eq pure-eqn cong-state))))

(defun arith-solve-neq (neq cong-state)
  (let ((eqn (arg 1 neq)))
    (if (and (integer-equality-p eqn)
	     (dp-numberp (rhs eqn)))
	(add-neq-constraint neq cong-state)
	(list neq))))

(defun add-pure-ineq (pure-ineq cong-state)
  (add-ineq-constraint pure-ineq cong-state))

(defun purify-eqn (eqn cong-state)
  (let* ((head (lhs eqn))
	 (headsgn (neg-sgn (term-sgn head)))
	 (head-var (term-var head))
	 (head-var-type (term-var-type head-var cong-state))
	 (tail (termsof (rhs eqn))))
    (mk-ineq-var (constant-id head-var) cong-state)
    (loop with new-eqns = (unless ineq-head-var?
			    (mk-equality head-var head-ineq-var))
	    with strict = (eq head-var-type 'strict)
	    for term in tail
	    for term-type = (term-type term cong-state) do
	    (cond
	     ((eq term-type 'strict)
	      (setq strict t)
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     ((and term-type (not strict))
	      (if (= headsgn (term-sgn term))
		  (setq new-eqns (cons (mk-equality (term-var term) *zero*)
				       new-eqns))
		  (return (list eqn))))
	     (term-type
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     (t (return (list eqn))))
	    finally (return (if strict (list *false*)
				(cons (mk-equality head-var *zero*)
				      new-eqns))))))

(defun purify-eqn (eqn cong-state)
  (declare (ignore cong-state))
  (list eqn))

(defun solve-normed-arith-eq (eqn cong-state)
  (let* ((head (lhs eqn))
	 (headsgn (neg-sgn (term-sgn head)))
	 (head-var (term-var head))
	 (head-var-type (term-var-type head-var cong-state))
	 (tail (termsof (rhs eqn))))
    (cond
     (head-var-type
      (loop with new-eqns = nil
	    with strict = (eq head-var-type 'strict)
	    for term in tail
	    for term-type = (term-type term cong-state) do
	    (cond
	     ((eq term-type 'strict)
	      (setq strict t)
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     ((and term-type (not strict))
	      (if (= headsgn (term-sgn term))
		  (setq new-eqns (cons (mk-equality (term-var term) *zero*)
				       new-eqns))
		  (return (list eqn))))
	     (term-type
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     (t (return (list eqn))))
	    finally (return (if strict (list *false*)
				(cons (mk-equality head-var *zero*)
				      new-eqns)))))
     (t (list eqn)))))

(defun old-solve-normed-arith-eq (eqn cong-state)
  (let* ((head (lhs eqn))
	 (headsgn (neg-sgn (term-sgn head)))
	 (head-var (term-var head))
	 (head-var-type (term-var-type head-var cong-state))
	 (tail (termsof (rhs eqn))))
    (cond
     (head-var-type
      (loop with new-eqns = nil
	    with strict = (eq head-var-type 'strict)
	    for term in tail
	    for term-type = (term-type term cong-state) do
	    (cond
	     ((eq term-type 'strict)
	      (setq strict t)
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     ((and term-type (not strict))
	      (if (= headsgn (term-sgn term))
		  (setq new-eqns (cons (mk-equality (term-var term) *zero*)
				       new-eqns))
		  (return (list eqn))))
	     (term-type
	      (unless (= headsgn (term-sgn term))
		(return (list eqn))))
	     (t (return (list eqn))))
	    finally (return (if strict (list *false*)
				(cons (mk-equality head-var *zero*)
				      new-eqns)))))
     (t (list eqn)))))

(defun solve-ineq (ineq cong-state)
  (let* ((norm (normineq ineq cong-state)))
    (cond
     ((true-p norm) (list *true*))
     ((false-p norm) (list *false*))
     ((ineq-p norm)
      (solve-normed-arith-ineq norm cong-state))
     (t (list norm)))))

(defun solve-normed-arith-ineq (ineq cong-state)
  (let* ((coef (ineq-coef ineq))
	 (strict (ineq-strict? ineq))
	 (initial-type (ineq-initial-type ineq))
	 (x nil) ;(break)
	 (ineq-var (make-ineq-var strict initial-type cong-state)))
    (declare (ignore x))
    (add-ineq-var-constraint ineq-var cong-state)
    (solve-normed-equality
     (mk-equality (arg 1 ineq)
		  (sigplus
		   (mk-plus* (arg 2 ineq)
			     (mk-times* (mk-constant coef)
					ineq-var))))
     cong-state)))
