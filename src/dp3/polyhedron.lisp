;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:56:25
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defvar *max-vars* 30)
(defvar *max-rays* 200)

(defvar *print-polyhedron* nil)

(defun init-poly () T)

;; Polyhedral Structure:
;; Type 'poly' is a wrapper around the content structure 'poly*' to allow
;; for delayed copying. Structure poly* is only copied when one of
;; its fields is being changed. In this case the tag 'inline-update-p'
;; is set to indicate that inline updating is now safe from now on.

(defdpstruct poly
   (inline-update-p nil :type boolean)
   (poly* :type poly*))

(defdpstruct (poly*
	      (:print-function
	       (lambda (ps s k)
		 (declare (ignore k))
		 (if *print-polyhedron*
		     (domain_print (poly*-hedron ps))
		     (format s "<~A slack variables, ~A dimension>"
		       (poly*-var-count ps)
		       (polyhedron-dimension (poly*-hedron ps)))))))
  (var-count 0 :type fixnum)
  (var-to-index (initial-var-to-index) :type hash-table)
  (index-to-var (initial-index-to-var) :type array)
  (hedron (universal-poly) :type integer)
  (input-eqns nil :type list)
  (input-neqs nil :type list)
  (equalities nil :type list))

(defvar *initial-poly* nil)

(defun initial-poly ()
  (or *initial-poly*
      (make-poly :poly* (make-poly*))))

(defun new-poly (poly*)
  (declare (type poly* poly*))
  (make-poly :poly* poly*))

(defun copy-poly* (p*)
  "Shallow copy of a polyhedral structure"
  (declare (type poly* p*))
  (make-poly* :var-count    (poly*-var-count p*)
	      :var-to-index (copy-hash-table (poly*-var-to-index p*))
	      :index-to-var (copy-array (poly*-index-to-var p*))
	      :hydron       (domain_copy (poly*-hydron p*))
	      :input-eqns   (poly*-input-eqns p*)
	      :input-neqs  (poly*-input-neqs p*)
	      :equalities   (poly*-equalities p*)))

;; Destructors for top-level structures of type poly

(defun poly-var-count (p)
  (declare (type poly p))
  (the fixnum (poly*-var-count (the poly* (poly-poly* p)))))

(defun poly-index-of (p var)
  (declare (type poly p)
	   (type node var))
  (gethash var (poly*-var-to-index (the poly* (poly-poly* p)))))

(defun poly-var-of (p index)
  (declare (type poly p)
	   (type integer index))
  (aref (poly*-index-to-var (the poly* (poly-poly* p))) index))

(defun poly-hedron (p)
  (declare (type poly p))
  (the integer (poly*-hedron (the poly* (poly-poly* p)))))

(defun poly-input-eqns (p)
  (declare (type poly p))
  (the list (poly*-input-eqns (the poly* (poly-poly* p)))))

(defun poly-input-neqs (p)
  (declare (type poly p))
  (the list (poly*-input-neqs (the poly* (poly-poly* p)))))

(defun poly-equalities (p)
  (declare (type poly p))
  (the list (poly*-equalities (the poly* (poly-poly* p)))))

;; Updates on top-level structures of type poly. Hereby,
;; entries in poly* are copied lazily when required.

(defun updatable-poly* (p)
  "Create a new poly* copy if needed and adjust p accordingly."
  (declare (type poly p))
  (if (poly-inline-update-p p) (poly-poly* p)
      (setf (poly* p) (copy-poly* (poly-poly* p)))))

(defun setf-poly-var-count (p val)
  (declare (type poly p)
	   (type fixnum val))
  (let ((q* (updatable-poly* p)))
    (setf (poly*-var-count q*) val)))

(defsetf poly-var-count setf-poly-var-count)

(defun setf-poly-index-of (p var index)
  (declare (type poly p)
	   (type node var)
	   (type integer index))
  (let ((q* (updatable-poly* p)))
    (setf (gethash var (poly*-var-to-index q*)) index)))

(defsetf poly-index-of setf-poly-index-of)

(defun setf-poly-var-of (p index var)
  (declare (type poly p)
	   (type integer index)
	   (type node var))
  (let ((q* (updatable-poly* p)))
    (setf (aref (poly*-index-to-var q*) index) var)))

(defsetf poly-var-of setf-poly-var-of)

(defun setf-poly-hedron (p val)
  (declare (type poly p)
	   (type integer val))
  (let ((q* (updatable-poly* p)))
    (setf (poly*-hedron q*) val)))

(defsetf poly-hedron setf-poly-hedron)

(defun setf-input-eqns (p val)
  (declare (type poly p)
	   (type list val))
  (let ((q* (updatable-poly* p)))
    (setf (poly*-input-eqns q*) val)))

(defsetf poly-input-eqns setf-input-eqns)

(defun setf-input-neqs (p val)
  (declare (type poly p)
	   (type list val))
  (let ((q* (updatable-poly* p)))
    (setf (poly*-input-neqs q*) val)))

(defsetf poly-input-neqs setf-input-neqs)

(defun setf-poly-equalities (p val)
  (declare (type poly p)
	   (type list val))
  (let ((q* (updatable-poly* p)))
    (setf (poly*-equalities q*) val)))

(defsetf poly-equalities setf-poly-equalities)

;; Some constant polyhedron

(defvar *universal-polyhedron* nil)
(defvar *epsilon-leq-0-polyhedron* nil)

(defun universal-poly ()
  (or *universal-polyhedron*
      (make-universal-polyhedron)))

(defun epsilon-leq-0-poly ()
  (or *epsilon-leq-0-polyhedron*
      (make-epsilon-leq-0-polyhedron)))


;; Basic constraints and polyhedra
 
(defun make-empty-constraint ()
  (let ((cnstrnt (matrix_alloc 1 (+ *max-vars* 3))))
    (loop for i from 0 below (+ *max-vars* 3)
	  do (setf (matrix-entry cnstrnt 0 i) 0))
    cnstrnt))

(defun make-empty-constraints (n)
  "Construct n empty constraints"
  (declare (type integer n))
  (let ((cnstrnt (matrix_alloc n (+ *max-vars* 3))))
    (loop for i from 0 below n do
	  (loop for j from 0 below (+ *max-vars* 3)
		do (setf (matrix-entry cnstrnt i j) 0)))
    cnstrnt))

(defun make-epsilon-constraint ()
  "Creates the constraint epsilon >= 0."
  (declare (ignore epsilon))
  (let ((cnstrnt (make-empty-constraint)))
    (setf (matrix-entry cnstrnt 0 0) 1) ;;; For inequality
    (setf (matrix-entry cnstrnt 0 1) 1) ;;; Coef for epsilon
    cnstrnt))

(defun make-universal-polyhedron ()
  "Creates a polynomial whose only constraint is epsilon >= 0."
  (let ((universal (universe_polyhedron (1+ *max-vars*)))
	(epsilon (make-epsilon-constraint)))
    (prog1 (domainaddconstraints universal epsilon *max-rays*)
      (matrix-free epsilon-constraint)
      (domain-free universal))))

(defun make-epsilon-leq-0-polyhedron ()
  "Creates a polynomial whose only constraint is epsilon <= 0."
  (let ((cnstrnt (make-empty-constraint)))
    (setf (matrix-entry cnstrnt 0 0) 1)
    (setf (matrix-entry cnstrnt 0 1) -1)
    (prog1 (constraints2polyhedron cnstrnt *max-rays*)
      (matrix-free cnstrnt))))


;; Symbol table

(defvar *initial-var-to-index* nil)
(defvar *initial-index-to-var* nil)

(defun initial-var-to-index ()
  (or *initial-var-to-index*
      (let ((result (make-array (1+ *max-vars*))))
	(setf (aref result 0) *epsilon*)
	result)))

(defun initial-index-to-var ()
  (or *initial-index-to-var*
      (dp-make-eq-hash-table)))


;; API

(defun poly-add-ineq-constraint (eqn cs)
  "One of the two main interfaces to the polyhedral package.
   Takes an inequality and adds it to the polyhedral-domain of cong-state.
   It returns a list of newly inferred equations."
  (declare (type node eqn)
	   (type cong-state cs)
           (special *dp-changed*)
           (special *contradiction*))
  (let ((diff (make-ineq-to-difference eqn))
	(strict (ineq-strict? eqn))
	(poly (polyhedral-structure cs)))
    (let* ((new-var-constraints (add-to-index-from-diff-terms diff poly))
	   (ineq-constraint (make-diff-constraint diff strict poly))
	   (old-poly (poly-hedron poly))
	   (constrained-poly (domain-add-all-constraints
				    old-poly new-var-constraints))
	   (new-poly (mydomainaddconstraints old-poly ineq-constraint *max-rays*)))
      (cond ((out-of-space)
	     (setq new-poly old-poly)
	     (setq *dp-changed* t)
	     (list (mk-equality eqn *true*)))
	    ((= (domain>= (epsilon-leq-0-poly) new-poly) 1)  ; Contradiction
	     (setq *dp-changed* t)
	     (setq *contradiction* t)
	     (setf (poly-hedron poly) new-poly)
	     (setf (poly-input-eqns poly) (cons eqn (poly-input-eqns poly)))
	     (list *false*))
	    ((= (domain>= new-poly constrained-poly)  1)
	        ; eqn is redundant wrt old polyhedron plus term constraints.
	     (setf (poly-hedron poly) constrained-poly)
	     (list *true*))
	    (t (setf (poly-hedron poly) new-poly)
	       (setf (poly-input-eqns poly)
		     (cons eqn (poly-input-eqns poly)))
	       (setq *dp-changed* t)
	       (let* ((old-eqs (poly-equalities poly))
		      (new-eqs (poly-to-terms poly cs))
		      (brand-new-eqs (set-difference new-eqs old-eqs)))
		 (setf (poly-equalities poly)
		       (append brand-new-eqs old-eqs))
		 (cons (mk-equality eqn *true*) (good-equalities brand-new-eqs))))))))

(defun poly-add-neq-constraint (neq cs)
  "One of the two main interfaces to the polyhedral package.
   Takes an integer disequality and adds it to the polyhedral-domain
   of cong-state. DomainDifference only works for integers.
   neq is of the form (= (= lhs rhs) *false*) or (not (= lhs rhs)).
   It returns a list of newly inferred equations."
  (declare (type node neq)
	   (type cong-state cs)
           (special *dp-changed*)
           (special *contradiction*))
  (let* ((eqn (arg 1 neq))
	 (diff (make-ineq-to-difference eqn))
	 (poly (polyhedral-structure cs)))
    (add-to-index-from-diff-terms diff poly)
    (let* ((neq-constraint (make-diff-neq-constraint diff poly))
	   (old-domain (poly-hedron poly))
	   (neq-polyhedron (mydomainaddconstraints
			     (universal-poly) neq-constraint *max-rays*))
	   (new-domain (DomainDifference old-domain neq-polyhedron *max-rays*)))
      (when (out-of-space)
	(error "Polyhedron package ran out of space."))
      (cond ((= (polyhedron>= (epsilon-leq-0-poly) new-domain) 1) ; contradiction
	     (setq *dp-changed* t)
	     (setq *contradiction* t)
	     (setf (poly-hedron poly) new-domain)
	     (setf (poly-input-neqs poly)
		   (cons neq (poly-input-neqs poly)))
	     (list *false*))
	    ((= (domain>= new-domain old-domain) 1)
	     (list *true*))
	    (t (setf (poly-hedron poly) new-domain)
	       (setf (poly-input-neqs poly) (cons neq (poly-input-neqs poly)))
	       (setq *dp-changed* t)
	       (let* ((old-eqs (poly-equalities poly))
		      (new-eqs (poly-to-terms poly cs))
		      (brand-new-eqs (set-difference new-eqs old-eqs)))
		 (setf (poly-equalities poly) (append brand-new-eqs old-eqs))
		 (cons neq (good-equalities brand-new-eqs))))))))

(defun simplify-ineq-constraint (eqn cs)
  "Another api that is like poly-add-ineq-constraint,
   but just checks whether eqn is redundant, or contradictory
   with the polyhdal-domain of cong-state without modifying cong-state."
  (declare (type node eqn)
	   (type cong-state cs))
  (simplify-ineq-constraint* eqn (polyhedral-structure cs)))

(defun simplify-ineq-constraint* (eqn poly)
  (let ((diff (make-ineq-to-difference eqn))
	(strict (ineq-strict? eqn)))
    (if (not (all-vars-in-poly diff poly)) eqn
	(let* ((ineq (make-diff-constraint diff strict poly (equality-p eqn)))
	       (old-poly (poly-hedron poly))
	       (ineq-poly (constraints2polyhedron ineq *max-rays*))
	       (inter-poly (domainintersection old-poly ineq-poly *max-rays*)))
	  (unwind-protect
	      (cond ((poly-contradictory-p inter-poly) *false*)
		    ((poly-subsumed-p ineq-poly old-poly) *true*)
		    (t eqn))
	    (progn
	      (domain-free inter-poly)
	      (domain-free ineq-poly)
	      (matrix-free ineq)))))))

(defun poly-contradictory-p (poly)
  (poly-subsumed-p (epsilon-leq-0-poly) poly))

(defun add-to-index-from-diff-terms (diff poly)
  (if (plus-p diff)
      (map-funargs-nconc #'add-ineq-node diff poly)
      (add-ineq-node diff poly)))

(defun add-ineq-node (node poly)
  (let ((var (term-var node)))
    (if (poly-index-of poly var) nil
	(let ((index (incf (poly-var-count poly))))
	  (when (> index *max-vars*)
	    (error "Polyhedral package: max number ~a of variables exceeded"
		   *max-vars*))
	  (setf (poly-index-of poly var) index)
          (setf (poly-var-of poly index) var)
	  (make-var-constraints var index)))))

(defun make-var-constraints (var index)
  (when (square-p var)
    (list (make-nonneg-constraint index))))

(defun make-nonneg-constraint (index)
  (declare (type integer index))
  (let ((constraint (make-empty-constraint)))
    (setf (matrix-entry constraint 0 0) 1)
    (setf (matrix-entry constraint 0 (1+ index)) 1)
    constraint))

(defun make-diff-constraint (diff strict poly
				  &optional (equality nil))
  (let ((constraint (make-empty-constraint)))
    (if equality
	(setf (matrix-entry constraint 0 0) 0)
	(setf (matrix-entry constraint 0 0) 1))
    (when strict
      (setf (matrix-entry constraint 0 1) -1))
    (if (plus-p diff)
	(matrix-set-coef-from-node-plus-dif diff constraint poly)
	(matrix-set-coef-from-node diff constraint poly))
    constraint))

(defun matrix-set-coef-from-node-plus-dif (dif constraint poly &optional (i 0))
  (let ((lcm (apply #'lcm (plus-denoms dif))))
    (map-funargs #'matrix-set-coef-from-node dif constraint poly i lcm)))

(defun matrix-set-coef-from-node (term constraint poly &optional (i 0) (mult 1))
  "Sets the coefficient for the index of term in constraint i
   to be the coefficient of the term * mult."
  (multiple-value-bind (coef var)
      (destructure-monomial term)
    (let ((coef (* (constant-id coef) mult)))
      (if (one-p var)
	  (setf (matrix-entry constraint i (+ *max-vars* 2))
		(* (constant-id term) mult))
	  (let ((index (1+ (poly-index-of poly var))))
	    (setf (matrix-entry constraint i index) coef))))
    constraint))

(defun make-diff-neq-constraint (diff poly)
  (let ((diff-ineq-constraint (make-diff-constraint diff nil poly)))
    ;; Now make it an equality (will be negated in poly-add-neq-constraint):
    (setf (matrix-entry diff-ineq-constraint 0 0) 0)
    diff-ineq-constraint))

(defun all-vars-in-poly (diff poly)
  (if (plus-p diff)
      (let ((missing-var nil))
	(labels ((check-var (var poly)
		  (unless (or missing-var
			      (dp-numberp var)
			      (poly-index-of poly (term-var var)))
		    (setq missing-var var))))
	  (map-funargs #'check-var diff poly))
	(not missing-var))
      (poly-index-of poly diff)))

(defun make-ineq-to-difference (eqn)
  (declare (type application eqn))
  (if (member (funsym eqn) (list *lessp* *lesseqp*))
      (sigdifference (mk-difference (rhs eqn) (lhs eqn)))
      (sigdifference (mk-difference (lhs eqn) (rhs eqn)))))

;; Miscellaneous

(defun domain-add-all-constraints (poly constraints)
  (let ((result poly))
    (loop for c in constraints
	  do (setq result (mydomainaddconstraints result c *max-rays*)))
    result))

(defun good-equalities (equalities)
  (loop for eqn in equalities
	unless (occurs-under-interp (lhs eqn) (rhs eqn))
	collect eqn))

(defun out-of-space ()
  "Polyhedral package (Chernikova) out of table space"
  (= (pol_status) 1))

(defun convex-p (p)
  (declare (type poly p))
  (domain-null-p (polyhedron-next p)))

(defun predicate-of (constraints row)
  (if (= (matrix-entry constraints row 0) 1)
      *greatereqp*
      *=*))

;; Code to get decision procedure constraints out of a polyhedron.

(defun poly-to-terms (poly cs &optional *all*)
  "Returns the decision procedure equalities that are entailed by the
   polyhedral-domain of poly. These are the equalities that are entailed
   by all the polyhedron that make up that domain. If all is set then returns
   a list of lists of inequalities as well as equalities that are equivalent to
   each polyhedron of the domain (think dnf)."
  (declare (type poly poly)
	   (type cong-state cs)
	   (special *all*))
  (let ((polyhedron (poly-hedron poly))
	(*poly* poly))
    (declare (special *poly*))
    (if (convex-p polyhedron)
	(polyhedron-to-equalities polyhedron)
	(domain-to-equalities polyhedron))))

(defun poly-to-dnf (poly cs)
  (poly-to-terms poly cs 't))

(defun convex-poly-to-conjunct (poly cs)
  "When the polyhedron is convex returns all the dp inequalities
   and equalities equivalent to the polyhedron."
  (declare (type poly poly)
	   (ignore cs))
  (when (convex-p (poly-hedron poly))
    (constraints2equations
     (let ((*poly* poly))
       (declare (special *poly*))
       (polyhedron2constraints (poly-hedron poly))))))

(defun constraints2equations (constraints)
  (loop for row from 0 below (matrix-NbRows constraints)
	collect (constraint2equation constraints row)))

(defun constraint2equation (constraints row)
  (declare (special *poly*)
	   (special *zero*))
  (let ((monomials
	 (loop for j from 1 below (1- (matrix-NbColumns constraints))
	       for coef = (matrix-entry constraints row j)
	       unless (= coef 0)
	       collect (mk-times (list (mk-dp-number coef)
				       (poly-var-of *poly* (1- j))))))
	(constant
	 (mk-dp-number
	  (matrix-entry constraints row (1- (matrix-NbColumns constraints))))))
    (let ((funsym (predicate-of constraints row))
	  (args (list (mk-plus (cons constant monomials)) *zero*)))
      (sigma (mk-term (cons funsym args)) (cs)))))

(defun polyhedron-to-equalities (poly)
  (constraints2equalities
   (polyhedron2constraints poly)))

(defun constraints2equalities (constraints)
  (declare (special *all*))
  (loop for i from 0 below (matrix-NbRows constraints)
	while (or *all* (= (matrix-entry constraints i 0) 0))
	collect (constraint2equation constraints i)))

(defun domain-to-equalities (poly)
  (declare (special *all*))
  (if *all* (domain-to-equalities-list poly)
      (let ((first-eqs (polyhedron-to-equalities poly)))
	(domain-to-equalities-intersect (polyhedron-next poly) first-eqs))))

(defun domain-to-equalities-list (poly)
  (if (convex-p poly)
      (list (polyhedron-to-equalities poly))
      (cons (polyhedron-to-equalities poly)
	    (domain-to-equalities-list (polyhedron-next poly)))))

(defun domain-to-equalities-intersect (poly eqs)
  (let ((eqs1 (let ((*all* nil))
		(declare (special *all*))
		(polyhedron-to-equalities poly))))
    (if (convex-p poly)
	(intersection eqs eqs1)
      (let ((rest (polyhedron-next poly))
	    (ceqs (intersection eqs eqs1)))
	(cond ((convex-p rest) ceqs)
	      (ceqs (domain-to-equalities-intersect rest ceqs)))))))
