;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:54:52
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defvar *use-fourier-motzkin* nil
  "When true uses the fourier motzkin elimination algorithm for handling
inequalities. Otherwise uses the polyhedral package.")

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

(defun arith-solve (eqn cong-state)
  "Interface to the solver for arithmetic."
  (cond
   ((ineq-p eqn)
    (add-ineq eqn cong-state))
   ((ineq-p (lhs eqn))
    (add-ineq (lhs eqn) cong-state))
   (t (solve-normed-arith-eq eqn cong-state))))

(defun arith-solve-neq (neq cong-state)
  "Interface to the solver for negated arithmetic."
  (let ((eqn (arg 1 neq)))
    (if (and (integer-equality-p eqn cong-state)
	     (or (dp-numberp (rhs eqn))
		 (floor-p (lhs eqn))
		 (floor-p (rhs eqn))))
	(add-neq-constraint neq cong-state)
	(list neq))))

(defun norm-after-solve (eqn cong-state)
  (if (equality-p eqn)
      (normineq eqn cong-state)
      eqn))

(defun add-ineq (ineq cong-state)
  (let ((solved-eqns (add-ineq-constraint ineq cong-state)))
    (loop for e in solved-eqns collect
	  (norm-after-solve e cong-state))))

(defun add-ineq-constraint (ineq cong-state)
  (if *use-fourier-motzkin*
      (f-m-add-ineq-constraint ineq cong-state)
      (poly-add-ineq-constraint ineq cong-state)))

(defun add-neq-constraint (neq cong-state)
  (if *use-fourier-motzkin*
      (f-m-add-neq-constraint neq cong-state)
      (poly-add-neq-constraint neq cong-state)))

(defun solve-normed-arith-eq (eqn cong-state)
  "Takes a normineqed equality. Assume it is of the form:
Sum(ci*vi). It checks whether all of the ci*vi* are >= 0.
If they are then it returns forall i: vi=0. If at least one
of the ci*vi is <= 0 it returns (list eqn). If one of the
ci*vi is strictly > 0 it returns *false*."
  (let* ((head (lhs eqn))
	 (headsgn (neg-sgn (term-sgn head)))
	 (head-var (term-var head))
	 (head-var-type (term-var-type head-var cong-state))
	 (rhs (rhs eqn))
	 (tail (termsof rhs)))
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
     ((occurs-under-interp head rhs)
      (solve-normed-arith-loop-eq eqn cong-state))
     (t (list eqn)))))

(defun solve-normed-arith-loop-eq (eqn cong-state)
  "If eqn is of the form lhs = rhs such that lhs occurs only under
interpreted symbols in the rhs then we generate lhs <= rhs and
lhs >= rhs instead. Otherwise shostak loops."
  (let* ((lhs (lhs eqn))
	 (rhs (rhs eqn))
	 (less-ineq (mk-term `(,*lesseqp* ,lhs ,rhs)))
	 (greater-ineq (mk-term `(,*greatereqp* ,lhs ,rhs)))
	 (less-res (add-ineq less-ineq cong-state))
	 (greater-res (add-ineq greater-ineq cong-state)))
    (remove eqn (append less-res greater-res)
	    :test #'eq)))

