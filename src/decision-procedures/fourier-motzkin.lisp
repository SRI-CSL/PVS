;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1999/05/05
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)


(defdpstruct (fourier-motzkin
	      (:print-function
	       (lambda (fm s k)
		 (declare (ignore k))
		 (format s "<~A inequalities, ~A equalities>"
		   (length (fourier-motzkin-inequalities fm))
		   (length (fourier-motzkin-equalities fm))))))
  (max-vars *max-ineq-vars* :type fixnum)
  (ineq-var-count 0 :type fixnum)
  (ineq-var-to-index-hash (dp-make-eq-hash-table)
			  :type hash-table)
  (ineq-var-index-array (make-ineq-var-index-array *max-ineq-vars*)
			:type array)
  (ineq-vars nil :type list)
  (use (dp-make-eq-hash-table) :type hash-table)
  (input-eqns nil :type list)
  (input-neqs nil :type list)
  (inequalities nil :type list)
  (equalities nil :type list))

(defun copy-fourier-motzkin (new-f-m old-f-m)
  (setf (fourier-motzkin-max-vars new-f-m)
	(fourier-motzkin-max-vars old-f-m))
  (setf (fourier-motzkin-ineq-var-count new-f-m)
	(fourier-motzkin-ineq-var-count old-f-m))
  (copy-hash-table (fourier-motzkin-ineq-var-to-index-hash new-f-m)
		   (fourier-motzkin-ineq-var-to-index-hash old-f-m))
  (setf (fourier-motzkin-ineq-var-index-array new-f-m)
	(copy-array (fourier-motzkin-ineq-var-index-array new-f-m)
		    (fourier-motzkin-ineq-var-index-array old-f-m)))
  (setf (fourier-motzkin-input-eqns new-f-m)
	(fourier-motzkin-input-eqns old-f-m))
  (setf (fourier-motzkin-inequalities new-f-m)
	(fourier-motzkin-inequalities old-f-m))
  (setf (fourier-motzkin-equalities new-f-m)
	(fourier-motzkin-equalities old-f-m))
  ;(break)
  new-f-m)

(defun initial-fourier-motzkin ()
  (make-fourier-motzkin))

(defun clr-fourier-motzkin (f-m)
  (clrhash (fourier-motzkin-ineq-var-to-index-hash f-m))
  (setf (fourier-motzkin-input-eqns f-m) nil)
  (setf (fourier-motzkin-inequalities f-m) nil)
  (setf (fourier-motzkin-equalities f-m) nil)
  ;(break)
  f-m)

(defun f-m-add-ineq-constraint (eqn cong-state)
  "One of the two main interfaces to fourier-motzkin.
Adds an inequality constraint.
Similar to poly-add-ineq-constraint."
  (declare (special *contradiction*))
  (let ((new-eqs (f-m-add-ineq eqn (fourier-motzkin cong-state) cong-state)))
    (if *contradiction*
	new-eqs
	(if (and new-eqs (every #'true-p new-eqs))
	    (cons (mk-equality eqn *true*) new-eqs)
	    (cons (mk-equality eqn *true*) new-eqs)))))

(defun f-m-add-neq-constraint (eqn cong-state)
  "One of the two main interfaces to fourier-motzkin."
  (list eqn))

(defun f-m-add-ineq (ineq f-m cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (multiple-value-bind (new-eqs new-ineqs)
      (f-m (fourier-motzkin-inequalities f-m) (list ineq) nil cong-state)
    (when (or *dp-changed*
	      new-eqs
	      (not (equal new-ineqs (fourier-motzkin-inequalities f-m))))
      (setq *dp-changed* t)
      (setf (fourier-motzkin-inequalities f-m) new-ineqs)
      (setf (fourier-motzkin-equalities f-m)
	    (append (fourier-motzkin-equalities f-m) new-eqs)))
    new-eqs))

(defun f-m (prev-ineqs rest-ineqs new-equalities cong-state)
  (if rest-ineqs
      (f-m* prev-ineqs rest-ineqs new-equalities cong-state)
      (f-m-end prev-ineqs new-equalities cong-state)))

(defun f-m* (prev-ineqs rest-ineqs new-equalities cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (let ((new-eqs nil)
	(new-ineqs nil))
    (loop with ineq = (car rest-ineqs)
	  for chainineq in (chainineqs ineq prev-ineqs)
	  for norm = (normineq (residue ineq chainineq) cong-state)
	  while (not *contradiction*)
	  do (cond
	      ((true-p norm)
	       (setq new-eqs
		     (append (make-eqs-from-chain ineq chainineq)
			     new-eqs)))
	      ((false-p norm)
	       (setq *contradiction* t)
	       (setq *dp-changed* t)
	       (setq new-eqs (list *false*))
	       (setq new-ineqs nil))
	      (t (setq new-ineqs (cons norm new-ineqs)))))
    (f-m (cons (car rest-ineqs) prev-ineqs)
	 (unless *contradiction*
	   (sort-inequalities (append new-ineqs (cdr rest-ineqs))))
	 (if *contradiction*
	     new-eqs
	     (append new-eqs new-equalities))
	 cong-state)))

(defun f-m-end (inequalities new-equalities cong-state)
  (if new-equalities
      (if (false-p (car new-equalities))
	  (values new-equalities inequalities)
	  (f-m-add-equalities new-equalities inequalities cong-state))
      (values nil inequalities)))

(defun chainineqs (ineq prev-ineqs)
  (let ((ineq-var (lhs ineq))
	(ineq-pred (funsym ineq))
	(ineqs prev-ineqs))
    (loop for chain in ineqs
	  for chain-var = (lhs chain)
	  for chain-pred = (funsym chain)
	  when (and
		(eq ineq-var chain-var)
		(opposite-ineq-pred? ineq-pred chain-pred))
	  collect chain)))

(defun residue (ineq1 ineq2)
  (let ((residue-pred
	 (case (funsym ineq2)
	   (*LESSP* *GREATERP*)
	   (*GREATERP* *LESSP*)
	   (t (funsym ineq1)))))
    (mk-term (list residue-pred (rhs ineq2) (rhs ineq1)))))

(defun make-eqs-from-chain (ineq1 ineq2)
  ;;; Can add integer reasoning like that found in
  ;;; /project/pvs/pvs2.1/ground-prover/floor-ceil.lisp
  (if (eq (rhs ineq1) (rhs ineq2))
      (list (mk-equality (lhs ineq1) (rhs ineq1)))
      nil))

(defun f-m-add-equalities (new-equalities inequalities cong-state)
  (let ((simped-equalities nil))
    (loop for eq in new-equalities
	  for simp-eq = (simplify-ineq-expr eq simped-equalities cong-state)
	  unless (true-p simp-eq)
	  do
	  (setq simped-equalities (cons simp-eq simped-equalities)))
    (values simped-equalities
	    (simplify-ineqs inequalities simped-equalities cong-state))))

(defun simplify-ineqs (ineqs equalities cong-state)
  (loop for ineq in ineqs
	for simp-ineq = (simplify-ineq-expr ineq equalities cong-state)
	unless (true-p simp-ineq)
	collect simp-ineq))

(defun simplify-arith-expr (expr equalities cong-state)
  (cond
   ((plus-p expr)
    (sigplus (mk-plus (simplify-arith-args (funargs expr)
					   equalities
					   cong-state))))
   ((times-p expr)
    (sigtimes (mk-times (simplify-arith-args (funargs expr)
					     equalities
					     cong-state))))
   ((constant-p expr) (simplify-arith-var expr equalities cong-state))
   (t expr)))

(defun simplify-arith-args (args equalities cong-state)
  (loop for arg in args
	collect (simplify-arith-expr arg equalities cong-state)))

(defun simplify-arith-var (var equalities cong-state)
  (loop for equality in equalities
	for lhs = (lhs equality)
	when (eq var lhs)
	do (return (simplify-arith-expr (rhs equality) equalities cong-state))
	finally (return var)))

(defun simplify-ineq-expr (ineq equalities cong-state)
  (let ((ineq-pred (funsym ineq))
	(dif (sigdifference
	      (mk-difference (simplify-arith-expr
			      (arg 1 ineq)
			      equalities
			      cong-state)
			     (simplify-arith-expr
			      (arg 2 ineq)
			      equalities
			      cong-state)))))
    (mk-diff-into-ineq ineq-pred dif cong-state nil)))

(defun inequality-lessp (ineq1 ineq2)
  (let ((var1 (arg 1 ineq1))
	(var2 (arg 1 ineq2)))
    (cond
     ((monom-< var1 var2) t)
     ((eq var1 var2)
      (monom-< ineq1 ineq2))
     (t nil))))

(defun sort-inequalities (inequalities)
  (sort inequalities #'inequality-lessp))
