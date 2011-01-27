;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Linear demodulation routines with rewriting directed by the governing
;;;    active monomial ordering, MO< **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         23-Oct-2008,
;;;            last updated on  18-Nov-2009.
;;;

(in-package :rahd)

;;;
;;; ORIENT-PARTIAL-LINEQ: Given an equation E containing a linear variable, orient it s.t.
;;;  it is of the form V = P s.t. V is the linear variable in E lowest in the variable 
;;;  in {V, Vars(P)} under the *VARS-TABLE* ordering for the current polynomial ring 
;;;  Q[*VARS-TABLE*] s.t. P does not contain V and RCF |= (V = P) <=> E.
;;;
;;;    For example:  Given the quotient ring Q[z,w,x] (with our DEG-REV-LEX induced
;;;     convention that this means x MO< z, x MO< w, so x is the smallest variable
;;;     in the polynomial ring),
;;;
;;;       5x + 2z - w = 1    would reduce to    x = 1/5 - 2/5 z + 1/5 w.
;;;    
;;;    Likewise, we deal with partially linear equations as well:
;;;
;;;       (= (* 2 (* X X)) (+ (* 3 W) 1))    
;;;
;;;          would reduce to
;;;
;;;       (= W (+ (* 2/3 (* X X)) -1/3)).
;;;
;;; This orientation guarantees we always have terminating directed rewrites.
;;;

(defun orient-partial-lineq (e)
  (let ((op (car e))
	(x (cadr e))
	(y (caddr e)))
    (when (not (eq op '=))
      (break "Form ~A is not an equation." e))
    (when (not (all-vars-in-conj (list e)))
      (break "Form ~A contains no indeterminates." e))
    (let* ((e-zrhs-alg-rep
	    (poly-prover-rep-to-alg-rep 
		      `(- ,x ,y)))
	   (e-zrhs-oriented-alg-rep
	    (reverse e-zrhs-alg-rep))
	   (e-zrhs-smallest-monomial
	    (car e-zrhs-oriented-alg-rep))
	   (e-target-monomial
	    (cond ((= (mdeg e-zrhs-smallest-monomial) 0)
		   
		   ;;
		   ;; So, the equation includes a scalar, and this scalar was given
		   ;; the lowest monomial weight through canonicalization.  But, we want the 
		   ;; smallest monomial that includes an indeterminate, so we'll get that
		   ;; instead.
		   ;;
		   
		   (cadr e-zrhs-oriented-alg-rep))
		  (t e-zrhs-smallest-monomial))))

      ;; Were we successful in finding a target variable appearing linearly?

      (if (= (mdeg e-target-monomial) 1)
	    
	    ;; Let's get e-target-monomial on the LHS and build the corresponding RHS.
	    
	    (let* ((e-target-rhs (poly-mult '((-1)) (poly- e-zrhs-alg-rep (list e-target-monomial))))
		   
		   ;; And let's divide through by 1/coeff(e-target-monomial) on the RHS.
		   
		   (e-final-rhs (poly-mult `((,(/ 1 (mcoeff e-target-monomial))))
					   e-target-rhs))
		   (e-final-lhs (poly-mult `((,(/ 1 (mcoeff e-target-monomial))))
					   (list e-target-monomial)))
		   (e-final-directed-eq 
		    `(= ,(poly-alg-rep-to-prover-rep e-final-lhs)
			,(poly-alg-rep-to-prover-rep e-final-rhs))))

	      ;; Now, we still need to make sure that RHS does not contain the target var (LHS).
	      
	      (let ((e-target-var (all-vars-in-conj `((= ,(poly-alg-rep-to-prover-rep e-final-lhs) 0))))
		    (e-rhs-vars (all-vars-in-conj `((= ,(poly-alg-rep-to-prover-rep e-final-rhs) 0)))))
		(if (not (member (car e-target-var) e-rhs-vars))
		    (progn 
		      (fmt 2 "~% >> Orienting a partially linear equation for inducing dimensional-reduction via demodulation.  ~%     Eq: ~A~%     Target monomial: ~A.~%" 
			   e (mprint e-target-monomial))
		      (fmt 2 "     Final directed demodulator: ~A --> ~A.~%     Representative RAHD eq: ~A.~%~%"
			   (poly-print e-final-lhs) (if e-final-rhs (poly-print e-final-rhs) 0) e-final-directed-eq)
	      
		      e-final-directed-eq)
		  nil)))
	
	;; If we weren't succesful in finding a target variable occuring linearly, we return NIL.
	
	nil))))


;;;
;;; DERIVE-PARTIAL-DEMOD-LINS: Given a case, compute all partially linear demodulators we can directly
;;;  derive from its equations and apply them.
;;;

(defun derive-partial-demod-lins (c)
  (let ((derived-demods nil))
    (dolist (l c)
      (if (eq (car l) '=)
	  (let ((l-oriented (orient-partial-lineq l)))
	    (when l-oriented
	      (setq derived-demods (cons l-oriented derived-demods))))))
    
    ;; Now, the collection of demodulators in DERIVED-DEMODS is guaranteed to
    ;; be terminating, so we can just use SUBST-EQS with (TERM -> T) upon the c.

    (if derived-demods 
	(let ((demod-out (subst-eqs c derived-demods #'(lambda (x) (declare (ignore x)) t))))
	  (fmt 2 "~% >> Applying derived demodulators (listed above) to case.  ~%     Case before demodulation: ~A. ~%     Case after demodulation: ~A.~%"
	       c demod-out)
	  demod-out)
      c)))

;;;
;;; GATHER-PURE-LINEQS: Given a case, return the list of all purely linear equations it contains.
;;;

(defun gather-pure-lineqs (c)
  (remove-if #'(lambda (l)
		 (let ((op (car l)) (x (cadr l)) (y (caddr l)))
		   (or (not (eq op '=))
		       (> (poly-deg (poly-prover-rep-to-alg-rep x)) 1)
		       (> (poly-deg (poly-prover-rep-to-alg-rep y)) 1))))
	     c))
