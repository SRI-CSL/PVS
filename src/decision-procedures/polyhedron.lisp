(in-package dp)

(defvar *max-ineq-vars* 30)
(defvar *max-rays* 200)

(defun make-epsilon ()
  (let* ((id (gentemp "epsilon"))
	 (result (make-constant :initial-type 'strict
				:sxhash (dp-sxhash id)
				:id id
				:index 0)))
    (setf (dp-gethash id *term-hash*)
	  result)))

(defvar *epsilon* (make-epsilon))

(defun make-empty-constraint (max-ineq-vars)
  (let ((constraint (matrix_alloc 1 (+ max-ineq-vars 3))))
    (loop for i from 0 below (+ max-ineq-vars 3)
	  do (matrix_set constraint 0 i 0))
    constraint))

(defun make-empty-constraints (nb-constraints max-ineq-vars)
  (let ((constraint (matrix_alloc nb-constraints (+ max-ineq-vars 3))))
    (loop for i from 0 below nb-constraints do
	  (loop for j from 0 below (+ max-ineq-vars 3)
		do (matrix_set constraint i j 0)))
    constraint))

(defun make-epsilon-constraint (epsilon max-ineq-vars)
  (let ((constraint (make-empty-constraint max-ineq-vars)))
    (matrix_set constraint 0 0 1) ;;; For inequality
    (matrix_set constraint 0 1 1) ;;; Coef for epsilon
    constraint))

(defun make-universal-polyhedron (max-ineq-vars max-rays)
  (let ((pol (universe_polyhedron (1+ max-ineq-vars)))
	(epsilon-constraint (make-epsilon-constraint *epsilon*
						     max-ineq-vars)))
    (prog1
	(domainaddconstraints pol epsilon-constraint max-rays)
      (matrix_free epsilon-constraint)
      (domain_free pol))))

(defun make-epsilon-leq-0-polyhedron (max-ineq-vars max-rays)
  (let ((constraint (make-empty-constraint max-ineq-vars)))
    (matrix_set constraint 0 0 1)
    (matrix_set constraint 0 1 -1)
    (prog1
	(constraints2polyhedron constraint max-rays)
      (matrix_free constraint))))

(defun add-ineq-var-constraint (ineq-var cong-state)
  (let* ((max-ineq-vars (polyhedral-structure-max-vars
			 (polyhedral-structure cong-state)))
	 (ineq-var-constraint (make-ineq-var-constraint
			       ineq-var max-ineq-vars)))
    (prog1
	(setf (polyhedral-structure-polyhedron
	       (polyhedral-structure cong-state))
	      (add-constraint ineq-var-constraint cong-state))
      (matrix_free ineq-var-constraint))))

(defvar *universal-polyhedral-domain*
  (make-universal-polyhedron *max-ineq-vars* *max-rays*))
(defvar *epsilon-leq-0-poly*
  (make-epsilon-leq-0-polyhedron *max-ineq-vars* *max-rays*))

(defun universal-polyhedral-domain ()
  *universal-polyhedral-domain*)

(defun add-ineq-var-eqn-constraint (eqn cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (let* ((poly-s (polyhedral-structure cong-state))
	 (max-ineq-vars (polyhedral-structure-max-vars
			 poly-s))
	 (max-rays (polyhedral-structure-max-rays
		    poly-s))
	 (epsilon-leq-0-poly (polyhedral-structure-epsilon-poly
			      poly-s))
	 (ineq-var-eqn-constraint (make-ineq-var-eqn-constraint
				   eqn max-ineq-vars))
	 (old-polyhedron (polyhedral-structure-polyhedron
			  poly-s))
	 (new-polyhedron (domainaddconstraints
			  old-polyhedron ineq-var-eqn-constraint max-rays)))
    ;(break)
    (matrix_free ineq-var-eqn-constraint)
    (cond
     ((= (mypolyhedronincludes epsilon-leq-0-poly new-polyhedron) 1)
      (setq *dp-changed* t)
      (setq *contradiction* t)
      (setf (polyhedral-structure-polyhedron poly-s)
	     new-polyhedron)
      *false*)
     ((= (mypolyhedronincludes new-polyhedron old-polyhedron) 1)
      *true*)
     (t (setf (polyhedral-structure-polyhedron poly-s)
	      new-polyhedron)
	(setq *dp-changed* t)
	eqn))))

(defun poly-add-neq-constraint (neq cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (let* ((eqn (arg 1 neq))
	 (diff (make-ineq-to-difference eqn))
	 (strict (ineq-strict? eqn))
	 (predicate (funsym eqn))
	 (poly-s (polyhedral-structure cong-state)))
    (add-to-index-from-diff-terms diff poly-s)
    (let* ((max-ineq-vars (polyhedral-structure-max-vars
			   poly-s))
	   (ineq-var-to-index-hash
	    (polyhedral-structure-ineq-var-to-index-hash poly-s))
	   (ineq-var-index-array
	    (polyhedral-structure-ineq-var-index-array poly-s))
	   (max-rays (polyhedral-structure-max-rays
		      poly-s))
	   (epsilon-leq-0-poly (polyhedral-structure-epsilon-poly
				poly-s))
	   (neq-constraint (make-diff-neq-constraint diff max-ineq-vars
						     ineq-var-to-index-hash))
	   (old-domain (polyhedral-structure-polyhedron
			poly-s))
	   (neq-polyhedron (mydomainaddconstraints
			     (universal-polyhedral-domain)
			     neq-constraint max-rays))
	   (new-domain (DomainDifference old-domain neq-polyhedron max-rays)))
      (when (= (pol_status) 1) (break))
      ;(break)
      (cond
       ((= (mypolyhedronincludes epsilon-leq-0-poly new-domain) 1)
	;(break)
	(setq *dp-changed* t)
	(setq *contradiction* t)
	(setf (polyhedral-structure-polyhedron poly-s)
	      new-domain)
	(setf (polyhedral-structure-input-neqs poly-s)
	      (cons neq (polyhedral-structure-input-neqs poly-s)))
	(list *false*))
       ((= (mydomainincludes new-domain old-domain) 1)
	(list *true*))
       (t (setf (polyhedral-structure-polyhedron poly-s)
		new-domain)
	  (setf (polyhedral-structure-input-neqs poly-s)
		(cons neq
		      (polyhedral-structure-input-neqs poly-s)))
	  (setq *dp-changed* t)
	  (let* ((old-equalities (polyhedral-structure-equalities poly-s))
		 (new-equalities (polyhedral-structure-to-equalities
				  poly-s cong-state))
		 (brand-new-equalities
		  (set-difference new-equalities old-equalities :test #'eq)))
	    (setf (polyhedral-structure-equalities poly-s)
		  (append brand-new-equalities old-equalities))
	    (cons neq
		  (good-equalities brand-new-equalities))))))))

(defun domain-add-all-constraints (poly constraints max-rays)
  (let ((result poly))
    (loop for c in constraints
	  do (setq result
		   (mydomainaddconstraints
		    result c max-rays)))
    result))

(defun poly-add-ineq-constraint (eqn cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (let ((diff (make-ineq-to-difference eqn))
	(strict (ineq-strict? eqn))
	(predicate (funsym eqn))
	(poly-s (polyhedral-structure cong-state)))
    (let* ((new-var-constraints
	    (add-to-index-from-diff-terms diff poly-s))
	   (max-ineq-vars (polyhedral-structure-max-vars
			   poly-s))
	   (ineq-var-to-index-hash
	    (polyhedral-structure-ineq-var-to-index-hash poly-s))
	   (ineq-var-index-array
	    (polyhedral-structure-ineq-var-index-array poly-s))
	   (max-rays (polyhedral-structure-max-rays
		      poly-s))
	   (epsilon-leq-0-poly (polyhedral-structure-epsilon-poly
				poly-s))
	   (ineq-constraint (make-diff-constraint diff strict max-ineq-vars
						  ineq-var-to-index-hash))
	   (old-polyhedron (polyhedral-structure-polyhedron
			    poly-s))
;;;	   (new-polyhedron (loop for np =
;;;                                 (mydomainaddconstraints
;;;                                  old-polyhedron ineq-constraint max-rays)
;;;                                 for ps = (pol_status)
;;;                                 for max-rays = (+ max-rays *max-rays*)
;;;                                 until (= ps 0)
;;;                                 finally (return np)))
	   (constrained-polyhedron (domain-add-all-constraints
				    old-polyhedron
				    new-var-constraints
				    max-rays))
	   (new-polyhedron (mydomainaddconstraints
			    old-polyhedron
			    ineq-constraint
			    max-rays)))
      (setf (polyhedral-structure-max-rays poly-s)
	    max-rays)
      ;(break)
      ;;(when (eq eqn *beqn*) (break))
      (cond
      ((= (pol_status) 1) (setq new-polyhedron old-polyhedron)
       (setf (polyhedral-structure-aux-input-eqns poly-s)
		(cons eqn (polyhedral-structure-aux-input-eqns poly-s)))
       (setq *dp-changed* t)
       (list (mk-equality eqn *true*)))
       ((= (mydomainincludes epsilon-leq-0-poly new-polyhedron) 1)
	(setq *dp-changed* t)
	(setq *contradiction* t)
	(setf (polyhedral-structure-polyhedron poly-s)
	      new-polyhedron)
	(setf (polyhedral-structure-input-eqns poly-s)
	      (cons eqn (polyhedral-structure-input-eqns poly-s)))
	(list *false*))
       ((= (mydomainincludes new-polyhedron constrained-polyhedron)  1)
	(setf (polyhedral-structure-polyhedron poly-s)
	      constrained-polyhedron)
	(list *true*))
       (t (setf (polyhedral-structure-polyhedron poly-s)
		new-polyhedron)
	  (setf (polyhedral-structure-input-eqns poly-s)
		(cons eqn (polyhedral-structure-input-eqns poly-s)))
	  (setq *dp-changed* t)
	  (let* ((old-equalities (polyhedral-structure-equalities poly-s))
		 (new-equalities (polyhedral-structure-to-equalities
				  poly-s cong-state))
		 (brand-new-equalities
		  (set-difference new-equalities old-equalities :test #'eq)))
	    (setf (polyhedral-structure-equalities poly-s)
		  (append brand-new-equalities old-equalities))
	    (cons (mk-equality eqn *true*)
		  (good-equalities brand-new-equalities))))))))

(defun simplify-ineq-constraint (eqn cong-state)
  (declare (special *dp-changed*))
  (declare (special *contradiction*))
  (let ((diff (make-ineq-to-difference eqn))
	(strict (ineq-strict? eqn))
	(predicate (funsym eqn))
	(poly-s (polyhedral-structure cong-state)))
    (if (all-vars-in-poly diff poly-s)
	(let* ((max-ineq-vars (polyhedral-structure-max-vars
			       poly-s))
	       (ineq-var-to-index-hash
		(polyhedral-structure-ineq-var-to-index-hash poly-s))
	       (ineq-var-index-array
		(polyhedral-structure-ineq-var-index-array poly-s))
	       (max-rays (polyhedral-structure-max-rays
			  poly-s))
	       (epsilon-leq-0-poly (polyhedral-structure-epsilon-poly
				    poly-s))
	       (ineq-constraint (make-diff-constraint diff strict max-ineq-vars
						      ineq-var-to-index-hash
						      (equality-p eqn)))
	       (old-polyhedron (polyhedral-structure-polyhedron
				poly-s))
;;;	   (new-polyhedron (loop for np =
;;;                                 (mydomainaddconstraints
;;;                                  old-polyhedron ineq-constraint max-rays)
;;;                                 for ps = (pol_status)
;;;                                 for max-rays = (+ max-rays *max-rays*)
;;;                                 until (= ps 0)
;;;                                 finally (return np)))
	       (ineq-polyhedron (constraints2polyhedron ineq-constraint
							max-rays))
	       (intersect-polyhedron (domainintersection old-polyhedron
							 ineq-polyhedron
							 max-rays)))
	  ;(break)
	  ;;(when (eq eqn *beqn*) (break))
	  (cond
	   ((= (mydomainincludes epsilon-leq-0-poly intersect-polyhedron)
	       1)
	    (domain_free intersect-polyhedron)
	    (domain_free ineq-polyhedron)
	    (matrix_free ineq-constraint)
	    *false*)
	   ((= (mydomainincludes ineq-polyhedron old-polyhedron) 1)
	    (domain_free intersect-polyhedron)
	    (domain_free ineq-polyhedron)
	    (matrix_free ineq-constraint)
	    *true*)
	   (t 
	    (domain_free intersect-polyhedron)
	    (domain_free ineq-polyhedron)
	    (matrix_free ineq-constraint)
	    eqn)))
	eqn)))

(defun add-constraint (constraint cong-state)
  (domainaddconstraints
   (polyhedral-structure-polyhedron (polyhedral-structure cong-state))
   constraint
   (polyhedral-structure-max-rays (polyhedral-structure cong-state))))

(defun make-ineq-var-constraint (ineq-var max-ineq-vars)
  (let ((strict (eq (constant-type ineq-var) 'strict))
	(index (ineq-constant-index ineq-var))
	(constraint (make-empty-constraint max-ineq-vars)))
    (matrix_set constraint 0 0 1)
    (matrix_set constraint 0 (1+ index) 1)
    (when strict (matrix_set constraint 0 1 -1))
    constraint))

(defun matrix-set-coef-from-term (term constraint max-ineq-vars
				       &optional (mult 1))
  (let ((coef (* (constant-id (term-coef term)) mult))
	(var (term-var term)))
    (if (eq var *one*)
	(matrix_set constraint 0 (+ max-ineq-vars 2)
		    (* (constant-id term) mult))
	(matrix_set constraint 0 (1+ (ineq-constant-index var)) coef))
    constraint))

(defun matrix-set-coef-from-node (term constraint max-ineq-vars
				    ineq-var-to-index-hash
				    &optional (i 0) (mult 1))
  (let ((coef (* (constant-id (term-coef term)) mult))
	(var (term-var term)))
    (if (eq var *one*)
	(matrix_set constraint i (+ max-ineq-vars 2)
		    (* (constant-id term) mult))
	(matrix_set constraint i (1+ (dp-gethash var ineq-var-to-index-hash))
		    coef))
    constraint))

(defun make-ineq-var-eqn-constraint (eqn max-ineq-vars)
  (let ((constraint (make-empty-constraint max-ineq-vars))
	(dif (sigdifference (mk-difference (lhs eqn) (rhs eqn)))))
    (matrix_set constraint 0 0 0)
    (if (plus-p dif)
	(matrix-set-coef-from-plus-dif dif constraint max-ineq-vars)
	(matrix-set-coef-from-term dif constraint max-ineq-vars))
    constraint))

(defun add-ineq-var (var poly-s)
  (let* ((max-vars (polyhedral-structure-max-vars poly-s))
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

(defun all-vars-in-poly (diff poly-s)
  (if (plus-p diff)
      (let ((missing-var nil))
	(labels ((check-var
		  (var poly-s)
		  (unless (or missing-var
			      (dp-numberp var)
			      (dp-gethash
			       (term-var var)
			       (polyhedral-structure-ineq-var-to-index-hash
				poly-s)))
		    (setq missing-var var))))
	  (map-funargs #'check-var diff poly-s))
	(not missing-var))
      (dp-gethash diff (polyhedral-structure-ineq-var-to-index-hash poly-s))))

(defun add-to-index-from-diff-terms (diff poly-s)
  (if (plus-p diff)
      (map-funargs-nconc #'add-ineq-node diff poly-s)
      (add-ineq-node diff poly-s)))

(defun add-ineq-node (node poly-s)
  (let ((var (term-var node)))
    (if (dp-gethash var (polyhedral-structure-ineq-var-to-index-hash poly-s))
	nil
	(let ((var-index
	       (incf (polyhedral-structure-ineq-var-count poly-s))))
	  (setq *max-ineq-var-count*
		(max *max-ineq-var-count* var-index))
	  (when (> var-index (polyhedral-structure-max-vars poly-s))
	    (extend-polyhedral-structure poly-s))
	  (setf (dp-gethash var (polyhedral-structure-ineq-var-to-index-hash
				 poly-s))
		var-index)
	  (setf (aref (polyhedral-structure-ineq-var-index-array poly-s)
		      var-index)
		var)
	  (make-var-constraints var var-index
				(polyhedral-structure-max-vars poly-s))))))

(defun make-var-constraints (var var-index max-ineq-vars)
  (when (square-p var)
    (list (make-non-neg-constraint var-index max-ineq-vars))))

(defun square-p (var)
  (and (times-p var)
       (= (length (funargs var)) 2)
       (eq (arg 1 var) (arg 2 var))))

(defun make-non-neg-constraint (var-index max-ineq-vars)
  (let ((constraint (make-empty-constraint max-ineq-vars)))
    (matrix_set constraint 0 0 1)
    (matrix_set constraint 0 (1+ var-index) 1)))

(defun make-diff-constraint (diff strict max-ineq-vars ineq-var-to-index-hash
				  &optional (equality nil))
  (let ((constraint (make-empty-constraint max-ineq-vars)))
    (if equality
	(matrix_set constraint 0 0 0)
	(matrix_set constraint 0 0 1))
    (when strict (matrix_set constraint 0 1 -1))
    (if (plus-p diff)
	(matrix-set-coef-from-node-plus-dif diff constraint max-ineq-vars
					    ineq-var-to-index-hash)
	(matrix-set-coef-from-node diff constraint max-ineq-vars
				   ineq-var-to-index-hash))
    constraint))

(defun make-diff-neq-constraint (diff max-ineq-vars ineq-var-to-index-hash)
  (let ((diff-ineq-constraint (make-diff-constraint diff nil 
						    max-ineq-vars
						    ineq-var-to-index-hash)))
    (matrix_set diff-ineq-constraint 0 0 0)
    diff-ineq-constraint))

(defun make-ineq-constraint (eqn poly-s)
  (let ((constraint (make-empty-constraint max-ineq-vars))
	(dif (make-ineq-to-difference eqn))
	(max-ineq-vars
	 (polyhedral-structure-max-vars poly-s))
	(ineq-var-to-index-hash
	 (polyhedral-structure-ineq-var-to-index-hash poly-s))
	(strict (ineq-strict? eqn)))
    (if (eq (funsym eqn) *=*)
	(matrix_set constraint 0 0 0)
	(matrix_set constraint 0 0 1))
    (when strict (matrix_set constraint 0 1 -1))
    (if (plus-p dif)
	(matrix-set-coef-from-node-plus-dif dif constraint max-ineq-vars
					    ineq-var-to-index-hash)
	(matrix-set-coef-from-node dif constraint max-ineq-vars
				   ineq-var-to-index-hash))
    constraint))

(defun make-ineq-constraints (eqns poly-s)
  (let ((constraint (make-empty-constraints
		     (length eqns)
		     (polyhedral-structure-max-vars poly-s))))
    (loop for i from 0 below (length eqns)
	  for eqn in eqns
	  do (set-ineq-constraint i eqn constraint poly-s))
    constraint))

(defun set-ineq-constraint (i eqn constraint poly-s)
  (let ((dif (make-ineq-to-difference eqn))
	(max-ineq-vars (polyhedral-structure-max-vars poly-s))
	(ineq-var-to-index-hash
	 (polyhedral-structure-ineq-var-to-index-hash poly-s))
	(strict (ineq-strict? eqn)))
    (if (eq (funsym eqn) *=*)
	(matrix_set constraint i 0 0)
	(matrix_set constraint i 0 1))
    (when strict (matrix_set constraint i 1 -1))
    (if (plus-p dif)
	(matrix-set-coef-from-node-plus-dif dif constraint max-ineq-vars
					    ineq-var-to-index-hash i)
	(matrix-set-coef-from-node dif constraint max-ineq-vars
				   ineq-var-to-index-hash i))
    constraint))
    

(defun make-ineq-to-difference (eqn)
  (if (member (funsym eqn) (list *lessp* *lesseqp*) :test #'eq)
      (sigdifference (mk-difference (rhs eqn) (lhs eqn)))
      (sigdifference (mk-difference (lhs eqn) (rhs eqn)))))

(defun number-term-coef (term)
  (constant-id (term-coef term)))

(defun number-term-denominator (term)
  (denominator (number-term-coef term)))

(defun plus-coefs (expr)
  (map-funargs-list #'number-term-coef expr))

(defun plus-denoms (expr)
  (map-funargs-list #'number-term-denominator expr))

(defun matrix-set-coef-from-plus-dif (dif constraint max-ineq-vars i)
  (let ((lcm (apply #'lcm (plus-denoms dif))))
    (map-funargs #'matrix-set-coef-from-term dif constraint max-ineq-vars
		 lcm)))

(defun matrix-set-coef-from-node-plus-dif (dif constraint max-ineq-vars
					     ineq-var-to-index-hash
					     &optional (i 0))
  (let ((lcm (apply #'lcm (plus-denoms dif))))
    (map-funargs #'matrix-set-coef-from-node dif constraint max-ineq-vars
		 ineq-var-to-index-hash i lcm)))
  
(defun make-diagonal-matrix (max-vars)
  (let ((dimension (+ 2 max-vars)))
    (let ((matrix (Matrix_Alloc dimension dimension)))
      (loop for i from 0 below dimension
	    do (loop for j from 0 below dimension
		     do (Matrix_Set matrix i j 0)))
      (loop for i from 0 below dimension
	    do (Matrix_Set matrix i i 1))
      matrix)))

(defun extend-diagonal-matrix (max-vars)
  (make-diagonal-matrix max-vars))

(defvar *initial-projection-matrix* (make-diagonal-matrix
				     (1+ *max-ineq-vars*)))

(defun initial-projection-matrix ()
  *initial-projection-matrix*)

(defun extend-projection-matrix (max-vars)
  (make-diagonal-matrix (1+ max-vars)))

(defun polyhedral-structure-to-equations (poly-s cong-state)
  (when (domain= (polyhedron-next (polyhedral-structure-polyhedron poly-s))
		   0)
    (constraints2equations
     (polyhedron2constraints (polyhedral-structure-polyhedron poly-s))
     (polyhedral-structure-ineq-var-index-array poly-s)
     cong-state)))

(defun constraints2equations (constraints ineq-var-index-array cong-state)
  (loop for i from 0 below (matrix-NbRows constraints)
	collect
	(constraint2equation constraints i ineq-var-index-array cong-state)))

(defun polyhedral-structure-to-equalities (poly-s cong-state &optional all)
  (if (domain= (polyhedron-next (polyhedral-structure-polyhedron poly-s)) 0)
      (polyhedron-to-equalities (polyhedral-structure-polyhedron poly-s)
				(polyhedral-structure-ineq-var-index-array
				 poly-s)
				cong-state
				all)
      (domain-to-equalities (polyhedral-structure-polyhedron poly-s)
			    (polyhedral-structure-ineq-var-index-array poly-s)
			    cong-state
			    all)))

(defun polyhedron-to-equalities (polyhedron ineq-var-index-array
					    cong-state all)
  (constraints2equalities
   (polyhedron2constraints polyhedron)
   ineq-var-index-array
   cong-state
   all))

(defun domain-to-equalities-list (polyhedron ineq-var-index-array cong-state
					     all)
  (if (domain= (polyhedron-next polyhedron) 0)
      (list (polyhedron-to-equalities polyhedron ineq-var-index-array
				      cong-state all))
      (cons (polyhedron-to-equalities polyhedron ineq-var-index-array
				      cong-state all)
	    (domain-to-equalities-list (polyhedron-next polyhedron)
				       ineq-var-index-array
				       cong-state all))))

(defun domain-to-equalities-intersect
  (current-eqs polyhedron ineq-var-index-array cong-state)
  (if (domain= (polyhedron-next polyhedron) 0)
      (intersection
       current-eqs
       (polyhedron-to-equalities polyhedron ineq-var-index-array
				 cong-state nil))
      (let* ((eqs1 (polyhedron-to-equalities polyhedron ineq-var-index-array
					     cong-state nil))
	     (rest-poly (polyhedron-next polyhedron))
	     (ceqs (intersection current-eqs eqs1)))
	(if (domain= rest-poly 0)
	    ceqs
	    (if ceqs
		(domain-to-equalities-intersect
		 ceqs rest-poly ineq-var-index-array cong-state)
		nil)))))

(defun domain-to-equalities (polyhedron ineq-var-index-array cong-state all)
  (let ((equalities-list
	 (domain-to-equalities-list polyhedron ineq-var-index-array
				    cong-state all)))
    ;(reduce #'intersection equalities-list)
    equalities-list))

(defun domain-to-equalities (polyhedron ineq-var-index-array cong-state all)
  (if all
      (domain-to-equalities-list polyhedron ineq-var-index-array
				 cong-state all)
      (let ((first-eqs
	     (polyhedron-to-equalities polyhedron ineq-var-index-array
				       cong-state nil)))
	(domain-to-equalities-intersect
	 first-eqs (polyhedron-next polyhedron)
	 ineq-var-index-array cong-state))))

(defun constraints2equalities (constraints ineq-var-index-array cong-state all)
  (loop for i from 0 below (matrix-NbRows constraints)
	while (or all (= (matrix_ref constraints i 0) 0))
	collect
	(constraint2equation
	 constraints i ineq-var-index-array cong-state)))

(defun constraint2equation (constraints row ineq-var-index-array cong-state)
  (let ((predicate (if (= (matrix_ref constraints row 0) 1)
		       *greatereqp*
		       *=*))
	(monomials
	 (loop for j from 1 below (1- (matrix-NbColumns constraints))
	       for coef = (matrix_ref constraints row j)
	       unless (= coef 0)
	       collect
	       (mk-times (list (mk-dp-number coef)
			       (aref ineq-var-index-array (1- j))))))
	(constant
	 (mk-dp-number
	  (matrix_ref constraints row (1- (matrix-NbColumns constraints))))))
    (let ((result
	   (sigma (mk-term (cons predicate
				 (list (mk-plus (cons constant
						      monomials))
				       *zero*)))
		  cong-state)))
      ;(when (true-p result) (break))
      result)))

(defun initial-epsilon-poly ()
  *epsilon-leq-0-poly*)

(defvar *initial-ineq-var-to-index-hash*
  (dp-make-eq-hash-table))

(defun initial-ineq-var-to-index-hash ()
  *initial-ineq-var-to-index-hash*)

(defun make-ineq-var-index-array (max-vars)
  (let ((result (make-array (1+ max-vars))))
    (setf (aref result 0) *epsilon*)
    result))

(defvar *initial-ineq-var-index-array*
  (make-ineq-var-index-array *max-ineq-vars*))

(defun initial-ineq-var-index-array ()
  *initial-ineq-var-index-array*)

(defun extend-index-array (index-array max-vars)
  (adjust-array index-array (1+ max-vars)))

(defun clr-polyhedral-structure (poly-s)
  (when (and (not (domain-eq
		   (polyhedral-structure-polyhedron poly-s)
		   (universal-polyhedral-domain)))
	     (not (domain-freed? (polyhedral-structure-polyhedron poly-s))))
    (domain_free (polyhedral-structure-polyhedron poly-s)))
  (when (and (not (domain-eq (polyhedral-structure-epsilon-poly poly-s)
			     (initial-epsilon-poly)))
	     (not (domain-freed? (polyhedral-structure-epsilon-poly poly-s))))
    (domain_free (polyhedral-structure-epsilon-poly poly-s)))
  (when (not (eq (polyhedral-structure-projection-matrix poly-s)
		 (initial-projection-matrix)))
    (matrix_free (polyhedral-structure-projection-matrix poly-s)))
  (when (not (eq (polyhedral-structure-ineq-var-to-index-hash poly-s)
		 (initial-ineq-var-to-index-hash)))
    (clrhash (polyhedral-structure-ineq-var-to-index-hash poly-s))))

(defun initial-polyhedral-structure ()
  (let ((result (make-polyhedral-structure)))
    result))

(defun extend-polyhedral-structure (poly-s)
  (let* ((new-max-vars (extend-max-vars poly-s))
	 (max-rays (polyhedral-structure-max-rays poly-s))
	 (new-projection-matrix (extend-projection-matrix new-max-vars))
	 (new-index-array (extend-index-array
			   (polyhedral-structure-ineq-var-index-array poly-s)
			   new-max-vars))
	 (new-epsilon-poly (make-epsilon-leq-0-polyhedron
			    new-max-vars max-rays))
	 (new-polyhedron (extend-polyhedron
			  (polyhedral-structure-polyhedron poly-s)
			  new-max-vars
			  max-rays)))
    ;(break)
    (setf (polyhedral-structure-max-vars poly-s) new-max-vars
	  (polyhedral-structure-projection-matrix poly-s) new-projection-matrix
	  (polyhedral-structure-ineq-var-index-array poly-s) new-index-array
	  (polyhedral-structure-epsilon-poly poly-s) new-epsilon-poly
	  (polyhedral-structure-polyhedron poly-s) new-polyhedron)
    ;(break)
    poly-s))

(defun extend-max-vars (poly-s)
  (+ (polyhedral-structure-max-vars poly-s)
     *max-ineq-vars*))

(defun extend-polyhedron (polyhedron max-vars max-rays)
  (let* ((old-constraints (polyhedron2constraints polyhedron))
	 (new-constraints (extend-constraints old-constraints
					      max-vars)))
    (prog1
	(constraints2polyhedron new-constraints max-rays)
      (matrix_free old-constraints)
      (matrix_free new-constraints))))

(defun extend-constraints (constraints max-vars)
  (let ((new-constraints (matrix_alloc (matrix-NbRows constraints)
				       (+ max-vars 3))))
    (loop for i from 0 below (matrix-NbRows constraints)
	  do
	  (matrix_set new-constraints i (+ max-vars 2)
		      (matrix_ref constraints i
				  (1- (matrix-NbColumns constraints))))
	  (loop for j from 0 below (1- (matrix-NbColumns constraints))
		do (matrix_set new-constraints i j
			       (matrix_ref constraints i j)))
	  (loop for j from (1- (matrix-NbColumns constraints))
                      below (+ max-vars 2)
		do (matrix_set new-constraints i j 0)))
    new-constraints))
