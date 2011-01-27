;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Routines for solving for variables appearing linearly in (partially)
;;;    linear atoms, directed by the active monomial ordering, MO< **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         17-Nov-2009,
;;;            last updated on  17-Nov-2009.
;;;

;;;
;;; VAR-ID: Given a variable, return its index w.r.t. *VARS-TABLE*.
;;;

(in-package :rahd)

(defun var-id (v)
  (var-id* v *vars-table* 0))

(defun var-id* (v l i)
  (cond ((endp l) nil)
	((equal (car l) v)  i)
	(t (var-id* v (cdr l) (1+ i)))))

;;;
;;; SMALLEST-MONOMIAL-IN-V: Given a polynomial P in alg rep, return the
;;;  smallest monomial in P whose power-product only contains indeterminate
;;;  V.
;;;
;;; * This requires the active MO to be deg-rev-lex; but, if deg-rev-lex
;;;    is not used, the operation is still sound, just incomplete.
;;;

(defun smallest-monomial-in-v (p v)
  (fmt 2 "~%~% >> Searching polynomial for smallest monomial solely in ~A.~%     p: ~A,"
       v (poly-print p))
  (let ((rev-zrhs-oriented-alg-rep
	 (reverse p))
	(v-id (var-id v))
	(out-m nil)
	(count 0))
    (dolist (m rev-zrhs-oriented-alg-rep)
      (fmt 2 "~%     m_~A: ~A; match? ~A,"
	   count (mprint m) (equal (mvars m) (list v-id)))
      (setq count (1+ count))
      (when (equal (mvars m) (list v-id))
	(return (setq out-m m))))
    (fmt 2 "~%     Found?: ~A.~%~%"
	 (when out-m (mprint out-m)))
    out-m))
		   
;;;
;;; ORIENT-PARTIAL-LIN-ATOM: Given an atom and a variable, attempt to orient
;;;  the atom so that the variable is alone on the LHS.
;;;

(defun orient-partial-lin-atom (a v)
  (let ((op (car a))
	(x (cadr a))
	(y (caddr a)))
    (when (not (member v (all-vars-in-conj (list a))))
      (break "Form ~A does not contain indeterminate ~A." a v))
    
    (let* ((a-zrhs-prover-rep `(- ,x ,y))
	   (a-zrhs-alg-rep (poly-prover-rep-to-alg-rep
			    a-zrhs-prover-rep))
	   (a-target-monomial 
	    (smallest-monomial-in-v a-zrhs-alg-rep v)))

      ;; Were we successful in finding a linear monomial in V?

      (if (and a-target-monomial (= (mdeg a-target-monomial) 1))

	  ;; Let's get a-target-monomial on the LHS and build the corresponding RHS.
	    
	  (let* ((a-target-rhs (poly-mult '((-1)) (poly- a-zrhs-alg-rep (list a-target-monomial))))
		 
		 ;; And let's divide through by 1/coeff(a-target-monomial) on the RHS.
		 
		 (a-final-rhs (poly-mult `((,(/ 1 (mcoeff a-target-monomial))))
					 a-target-rhs))
		 (a-final-lhs (poly-mult `((,(/ 1 (mcoeff a-target-monomial))))
					 (list a-target-monomial)))
		   
		 ;;
		 ;; If this is an inequality, then do we need to swap the polarity
		 ;;  of the operator?
		 ;;

		 (adj-op (correct-op op (mcoeff a-target-monomial)))

		 (a-final-directed-atom
		  `(,adj-op ,(poly-alg-rep-to-prover-rep a-final-lhs)
			    ,(poly-alg-rep-to-prover-rep a-final-rhs))))

	    ;; Now, we still need to make sure that RHS does not contain the target var (LHS).
	      
	    (let ((a-target-var (all-vars-in-conj `((= ,(poly-alg-rep-to-prover-rep a-final-lhs) 0))))
		  (a-rhs-vars (all-vars-in-conj `((= ,(poly-alg-rep-to-prover-rep a-final-rhs) 0)))))
	      (if (not (member (car a-target-var) a-rhs-vars))
		  (progn 
		    (fmt 2 "~% >> Orienting a partially linear atom.  ~%     Atom: ~A~%     Target monomial: ~A.~%" 
			 a (mprint a-target-monomial))
		    (fmt 2 "     Final oriented atom: ~A ~A ~A.~%     Representative RAHD atom: ~A.~%~%"
			 (poly-print a-final-lhs) adj-op (if a-final-rhs (poly-print a-final-rhs) 0) a-final-directed-atom)
	      
		    a-final-directed-atom)
		nil)))
	
	;; If we weren't succesful in finding a target variable occuring linearly, we return NIL.
	
	nil))))


;;;
;;; CORRECT-OP: Given an operator and a coefficient, return the dual
;;;  of the operator iff sign(coefficient) = -1.
;;;

(defun correct-op (op coeff)
  (cond ((> coeff 0) op)
	((< coeff 0) 
	 (case op
	   (< '>)
	   (> '<)
	   (<= '=>)
	   (>= '<=)
	   (= '=)))
	((= coeff 0)
	 (break "OP coefficient must be non-zero."))))

;;;
;;; ALL-LINEAR-ORIENTATIONS: Given an atom A, return a list of all
;;;  orientations of A w.r.t. variables which appear only linearly
;;;  in A.
;;;

(defun all-linear-orientations (a)
  (let ((vs (all-vars-in-conj (list a)))
	(out nil))
    (dolist (v vs)
      (let ((orient-v (orient-partial-lin-atom a v)))
	(when orient-v
	  (setq out (cons orient-v out)))))
    out))

;;;
;;; SATURATE-CASE-WITH-LINEAR-ORIENTATIONS: Given a case, saturate it 
;;;  with all linear orientations of its atoms.
;;;

(defun saturate-case-with-linear-orientations (c)
  (let ((derived-atoms nil))
    (dolist (a c)
      (let ((orientations (all-linear-orientations a)))
	(when orientations
	  (setq derived-atoms
		(append orientations derived-atoms)))))
    (break "saturate-case-with-linear-orientations")
    (union c derived-atoms :test 'equal)))
