;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arith-solve.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:54:52
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defun arith-solve (eqn cs)
  "Interface to the solver for arithmetic."
  (cond ((ineq-p eqn)
	 (add-ineq eqn cs))
	((ineq-p (lhs eqn))
	 (add-ineq (lhs eqn) cs))
	(t (solve-normed-arith-eq eqn cs))))

(defun arith-solve-neq (neq cs)
  "Interface to the solver for negated arithmetic."
  (let ((eqn (arg 1 neq)))
    (if (and (integer-equality-p eqn cs)
	     (or (dp-numberp (rhs eqn))
		 (floor-p (lhs eqn))
		 (floor-p (rhs eqn))))
	(add-neq-constraint neq cs)
	(list neq))))

(defun add-ineq (ineq cs)
  (loop for eqn in (add-ineq-constraint ineq cs)
	collect (if (equality-p trm)  ; norm after solve
		    (normineq trm cs)
		    trm)))

(defun add-ineq-constraint (ineq cs)
  (poly-add-ineq-constraint ineq cs))

(defun add-neq-constraint (neq cs)
  (poly-add-neq-constraint neq cs))

(defun solve-normed-arith-eq (eqn cs)
  "Takes a normineqed equality. Assume it is of the form:
   Sum(ci*vi). It all of the ci*vi* are >= 0 then vi=0 is
   returned for all i. If at least one of the ci*vi is <= 0
   it returns (list eqn). If one of the ci*vi is strictly > 0
   it returns *false*."
  (let* ((head (lhs eqn))
	 (headsgn (neg-sgn (term-sgn head)))
	 (head-var (term-var head))
	 (head-var-type (term-var-type head-var cs))
	 (rhs (rhs eqn))
	 (tail (termsof rhs)))
    (cond (head-var-type
	   (loop with new-eqns = nil
		 with strict = (eq head-var-type 'strict)
		 for term in tail
		 for term-type = (term-type term cs) do
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
	   (solve-normed-arith-loop-eq eqn cs))
	  (t (list eqn)))))

(defun solve-normed-arith-loop-eq (eqn cs)
  "If eqn is of the form lhs = rhs such that lhs occurs only under
   interpreted symbols in the rhs then we generate lhs <= rhs and
   lhs >= rhs instead. Otherwise shostak loops."
  (declare (type node eqn)
	   (type cong-state cs))
  #+dbg(assert (equality-p eqn))
  (let* ((lhs (lhs eqn))
	 (rhs (rhs eqn))
	 (less-ineq (mk-lesseq lhs rhs))
	 (greater-ineq (mk-greatereq lhs rhs))
	 (less-res (add-ineq less-ineq cs))
	 (greater-res (add-ineq greater-ineq cs)))
    (remove eqn (append less-res greater-res))))

;; Miscellaneous recognizers

(defun term-var-type (var cs)
  (cond ((strict-var-p var) 'strict)
	((nonneg-var-p var) t)
	(t (term-var-type-uninterp var cs))))

(defun nonneg-var-p (var)
  "checks if var is a nonneg-var"
  (and (constant-p var)
       (eq (constant-type var) 'nonneg)))

(defun strict-var-p (var)
  (and (constant-p var)
       (eq (constant-type var) 'strict)))

(defun nonzero-var-p (var)
  "checks if var is a nonzero-var"
  (and (constant-p var)
       (eq (constant-type var) 'nonzero)))

(defun nonneg-p (term cs)
  (eq (dp-type term cs) 'nonneg))

(defun strict-p (term cs)
  (eq (dp-type term cs) 'strict))

(defun nonzero-p (term cs)
  (eq (dp-type term cs) 'nonzero))

(defun term-type (term cs)
  (term-var-type (term-var term) cs))

(defun term-var-type-uninterp (term cs)
  (cond ((strict-p term cs) 'strict)
	((nonneg-p term cs) 'nonneg)
	(t nil)))


