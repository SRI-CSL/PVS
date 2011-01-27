;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Nonlinear demodulation routines **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         25-Feb-2009,
;;;            last updated on  28-Feb-2009.
;;;
;;;

;;;
;;; DEMOD-NL-UNIT-CLAUSES: Given a top-level RCF *G* formula, see if any
;;;  unit clauses induce a DEMOD-NL instance.  If so, this may induce
;;;  an EIL formula which will then need to be passed through the DIV
;;;  processor.
;;;

(defun demod-nl-unit-clauses (f)
  (let* ((unit-clauses (remove-if #'(lambda (x) (> (length x) 1)) f))
	 (unit-eqs (remove-if #'(lambda (x) (not (equal (caar x) '=))) 
			      unit-clauses))
	 (possible-demods nil))
    (dolist (c unit-eqs)
      (let ((vars-in-c (all-vars-in-conj c)))
	(dolist (v vars-in-c)
	  (let ((cur-attempt (solve-eq-for-var (car c) v)))
	    (when (car cur-attempt)
		(setq possible-demods
		      (cons (cadr cur-attempt) possible-demods)))))))

    ;;
    ;; Right now, we just apply the first one.
    ;;

    (if possible-demods
	(let ((var (cadar possible-demods))
	      (rst (caddar possible-demods)))
	  (fmt 2 "~% -- RAHD Pre-processor: Successful demodulation of ~%~%      VAR: ~A~%~%  into~%~%      P:   ~A~%~%" 
	       var rst)
	  (add-vt-binding var rst)
	  (subst rst var f))
      f)))

    

;;;
;;; SOLVE-EQ-FOR-VAR: Given an equation and a variable, attempt to solve the
;;;  equation for that variable.  Note that this process may involve divisions,
;;;  and thus a tuple is returned of the following form:
;;;
;;;    (SOLVED?  SOLUTION  DIVISORS)
;;;
;;;  s.t. all members of the DIVISORS list must be subsequently proven to be
;;;  non-vanishing under goal hypotheses via spawned RAHD subgoals.
;;;
;;;  Note that the equation should be given in PROVER notation.
;;;

(defun solve-eq-for-var (e var)
  (let ((op (car e))
	(x (cadr e))
	(y (caddr e))
	(vars-in-e (all-vars-in-conj (list e)))
	(var-id (find-var var *vars-table* 0)))
    (when (not (eq op '=))
      (break "Form ~A is not an equation." e))
    (when (or (not vars-in-e)
	      (not (member var vars-in-e)))
      (break "Form ~A does not contain indeterminate ~A." e var))
    (let* ((p-alg-rep (poly-prover-rep-to-alg-rep `(- ,x ,y)))
      
	   ;;
	   ;; First, we gather all monomials in E that contain VAR, followed
	   ;; by a checking if the resulting polynomial is a monomial.
	   ;; If it is, then modulo the non-vanishing condition upon the variables 
	   ;;  [VARS(p-mons-in-var) \ {VAR}], we can solve E for VAR *if*
	   ;;   VAR appears linearly (e.g., it is raised to the first power).
	   ;; 
	   ;; Note that p-deg-of-var below is only known to be a valid degree
	   ;; measure when IS-P-MONOMIAL? holds.  We will always check this.
	   ;;

	   (p-mons-in-var (monomials-in-var p-alg-rep var-id))
	   (is-p-monomial? (= (length p-mons-in-var) 1))
	   (var-pp-in-p (assoc var-id (cdar p-mons-in-var)))
	   (p-deg-of-var (cdr var-pp-in-p))
	   
	   ;; 
	   ;; Second, we isolate this gathering as the LHS of a new equation.
	   ;;
	   
	   (e-gathered-lhs `(= ,(poly-alg-rep-to-prover-rep p-mons-in-var)
			       ,(poly-alg-rep-to-prover-rep (poly- nil (poly- p-alg-rep p-mons-in-var))))))
	   
      (fmt 8 "~% *** To solve ~%~%      EQ_0: ~A ~%~%       which canonicalizes to ~%~%      EQ_1: ~A ~%~%       for ~%~%      VAR: ~A ~%~%       we focus on the subpolynomial ~%~%      P_0: ~A ~%~%       and orient it as the LHS of an equivalent equation ~%~%      EQ_2: ~A.~%"
	   e `(= ,(canonicalize-poly `(- ,x ,y)) 0) var 
	   (poly-alg-rep-to-prover-rep p-mons-in-var)
	   e-gathered-lhs)

      ;;
      ;; If IS-P-MONOMIAL? and P-DEG-OF-VAR=1 both hold, then we have:
      ;;    (* c VAR V_1^{j_1} ... V_n^{j_n}) = P.
      ;; So, if VAR is non-vanishing, then:
      ;;    VAR = P/(* C V_1 ... V_N).
      ;;
      ;; If so, let us construct this equation.
      ;; If not, let's tell the user we are sorry.
      ;;
      ;; * Note that this constructed equation cannot be converted directly
      ;;   into ALG-REP, as this representation doesn't support division.
      ;;   Instead, functions using this as a demodulator must do demodulation
      ;;   initially using PROVER-REP and then call the division elimination
      ;;   translator to generate ALG-REP'able (e.g., ordered ring) formulas.
      ;;

      (if (and is-p-monomial? (= p-deg-of-var 1))
	  (let* ((rhs-divisor-mon (cons 
				   ;; the coefficient, c
				   (caar p-mons-in-var) 
				   ;; the remaining PP's not containing VAR
				   (remove var-pp-in-p (cdar p-mons-in-var))))
		 (rhs-divisor-poly (poly-alg-rep-to-prover-rep (cons rhs-divisor-mon nil)))
		 (final-demodulator `(= ,var (/ ,(caddr e-gathered-lhs) ,rhs-divisor-poly))))

	    ;;
	    ;; Success!
	    ;; Let us now report that we were successful and return the form
	    ;; (T <demodulator> <divisors>)
	    ;;
	    
	    
	    (fmt 7 "~%       Since the LHS of EQ_2 is a monomial linear in ~A,~%       modulo non-vanishing checks, we can divide through by ~%~%      P_1: ~A~%~%       to isolate ~A. ~%~%       We now do this and obtain the following final demodulator:~%~%      D: ~A.~%~%        >> SUCCESS! << ~%           We have solved EQ_0 for ~A. ***~%~%" var rhs-divisor-poly var final-demodulator var)
	    `(t ,final-demodulator ,rhs-divisor-poly ))
	
	;;
	;; Failure.  Let us now tell the user the sad story and return 
	;; a 3-tuple rooted in NIL.
	;;

    (progn 
      (fmt 7 "~%       >> NO PROGRESS << ~%          Unfortunately, we now realize that we don't~%          know how to solve EQ_2 for ~A without radicals. ***~%~%" var)
      '(nil nil nil))))))


;;;
;;; MONOMIALS-IN-VAR: Given a polynomial, gather all of its monomials
;;;  that contain the indeterminate VAR.  
;;;
;;;  Polynomials should be given in POLYALG notation,
;;;  VAR as the sought *VARS-TABLE* indeterminate ID.
;;;

(defun monomials-in-var (p var)
  (let ((out-ms nil))
    (dolist (m p)
      (let ((mcoeff (car m))
	    (mpps (cdr m)))
	(declare (ignore mcoeff))
	(when (assoc var mpps)
	  (setq out-ms (poly+ out-ms (cons m nil))))))
    out-ms))


