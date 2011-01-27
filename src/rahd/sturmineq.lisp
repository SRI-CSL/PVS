;;;
;;; (Semi-)Decision Methods for the Existence of Real Solutions to Systems of Univariate
;;;  Polynomial Equations and Inequalities 
;;;
;;;    using
;;;
;;;  Univariate Sturm Theory 
;;;
;;;    for
;;; 
;;;     RAHD: Real Algebra in High Dimensions
;;;   
;;;   v0.5,
;;;
;;; A feasible decision method for the existential theory of real closed fields.
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: (g.passmore@ed.ac.uk . http://homepages.inf.ed.ac.uk/s0793114/)
;;;
;;; >>> Requires: sturm.lisp, polyeval.lisp, polyconv.lisp
;;;
;;;
;;; This file: began on         30-July-2008,
;;;            last updated on  25-Sept-2008.
;;;

(in-package :rahd)

;;;
;;; OPEN-INTERVAL-UNIV-INEQ (c): Given a conjunction of the form:
;;;
;;;  (< P 0) /\ (> x a) /\ (< x b) ..., 
;;;
;;; with P a univariate polynomial in Q[x], decide whether or not P is satisfiable
;;; over the open interval (a,b).  Our algorithm works as follows:
;;;
;;;  (1) Count how many real roots P has in the *closed* interval [a,b] using
;;;      a Sturm sequence calculation,
;;;  (2) Subtract the number of roots there are on the boundary points a and b,
;;;  (3) If the result is 0, then P does not change sign in (a,b).
;;;  (4) So, we pick a sample point in (a,b), (a+b)/2, and then evaluate P at this
;;;      point.  If P( (a+b)/2 ) satisfies the inequality, then we return this
;;;      as a witness of satisfiability.  If P( (a+b)/2 ) does not satisfy the
;;;      inequality, then we return UNSAT.
;;;
;;;  We assume the RHS of the main inequality is 0.
;;;

(defun open-interval-univ-ineq (c)
  (let ((v (univ-sys c)))
    (cond ((not v) c)
	  (t (let ((open-interval-sys (univ-open-interval c v t)))
	       (cond ((not open-interval-sys) c)
		     (t (let ((lower (caar open-interval-sys))
			      (upper (cdar open-interval-sys))
			      (ineq  (cdr  open-interval-sys)))
			  (if (and lower upper ineq) ; must all be non-null
			      (let ((sign-condition (if (equal (car ineq) '<) -1 1))
				    (poly-prover-rep (cadr ineq)))
				(let ((poly-alg-rep (poly-prover-rep-to-alg-rep poly-prover-rep)))
				  (if (or (equal poly-alg-rep nil) (equal poly-alg-rep 0))
				      `(:UNSAT :SIMPLIFY-INEQUALITY
					       :OFFENDING-CLAIM ,ineq :EQUIVALENT-TO
					       (,(car ineq) 0 0))
				    (let ((num-boundary-roots
					   (+ (if (= (poly-univ-eval poly-alg-rep upper) 0) 1 0)
					      (if (= (poly-univ-eval poly-alg-rep lower) 0) 1 0))))
				      (let ((num-real-roots-in-open-interval
					     (- (poly-univ-interval-real-root-count poly-alg-rep lower upper)
						num-boundary-roots)))
					(fmt 9 "~% >> Sturm sequence inequality trace: ~%    Polynomial: ~A~%    Interval: ]~A, ~A[~%    Number of real roots in interval: ~A~%    Number of real roots on boundary (not included, as interval is open): ~A.~%~%"
					     (poly-print poly-alg-rep) lower upper 
					     (max 0 num-real-roots-in-open-interval) num-boundary-roots)
					(cond ((<= num-real-roots-in-open-interval 0)
					       (let ((sample-pt (/ (+ lower upper) 2)))
						 (let ((sign-sample-pt
							(if (< (poly-univ-eval poly-alg-rep sample-pt) 0)
							    -1 1)))
						   (if (= sign-sample-pt sign-condition) 
						       ;; Now, we must make sure *every* conjunct in this univariate system is satisfied.
						       (if (eval (append '(AND) (subst sample-pt v c)))
							   `(:SAT :ASSIGNMENT (,v ,sample-pt))
							 c)
						     `(:UNSAT :STURM-SEQUENCE (:NO-SIGN-CHANGE-IN-OPEN-INTERVAL 
									       :POLY ,poly-prover-rep
									       :OPEN-INTERVAL (,lower . ,upper)
									       :SAMPLE-POINT ,sample-pt
									       :SIGN-OF-POLY-ON-SAMPLE-POINT ,sign-sample-pt
									       :OFFENDING-CLAIM ,ineq))))))
					      (t c))))))) c)))))))))
					   
			       

;;;
;;; UNIV-OPEN-INTERVAL: Is a conjunction a system that places the variable v within an 
;;; explicit (numerical) open interval?  
;;;
;;; If so, and if find-ineq? is false, return (l . u) s.t. u > l where (> v l), (< v u) are in c.
;;; If so, and if find-ineq? is true, return ((l . u) . PC) s.t. the same constraints hold upon l, u,
;;;  but PC is the (first) inequality constraint on v that is not of the form (> v r) or (< v r) with
;;;  r an explicit rational number.
;;;
;;; Otherwise, return nil.
;;;

(defun univ-open-interval (c v find-ineq?)
  (let ((lower nil)
	(upper nil)
	(inequality nil))
    (if find-ineq? (setf inequality nil) t)
    (dolist (lit c)
      (let ((cur-r (car lit))
	    (cur-x (cadr lit))
	    (cur-y (caddr lit)))
	(cond ((and (equal cur-x v)
		    (numberp cur-y))
	       (case cur-r 
		     (> (setf lower cur-y))
		     (< (setf upper cur-y))
		     (otherwise t)))
	      ((and (equal cur-y v)
		    (numberp cur-x))
	       (case cur-r
		     (> (setf upper cur-x))
		     (< (setf lower cur-x)) ; used to be cur-y
		     (otherwise t)))
	      ((and find-ineq?
		    (not inequality)
		    (or (equal cur-r '>)
			(equal cur-r '<))
		    (or (not (numberp cur-x))
			(not (numberp cur-y))))
	       (setf inequality lit))		    
	      (t t))))
    (if (and upper lower)
	(let ((open-interval (cons lower upper)))
	  (if find-ineq? (cons open-interval inequality)
	    open-interval))
      nil)))

;;;
;;; Is a conjunction a univariate system?  If so, return its single variable.
;;; If not, return nil.
;;;	

(defun univ-sys (c)
  (let ((all-vars nil))
    (dolist (lit c)
      (setq all-vars 
	    (union (gather-vars (cadr lit))
		   (union (gather-vars (caddr lit))
			  all-vars))))
    (if (= (length all-vars) 1) (car all-vars) nil)))

(defun gather-vars (p)
  (cond ((equal p nil) nil)
	((and (symbolp p) 
	      (not (equal p '+))
	      (not (equal p '-))
	      (not (equal p '*))
	      (not (equal p '/)))
	      `(,p))
	((not (consp p)) nil)
	(t (union (gather-vars (car p))
		  (gather-vars (cdr p))))))


	       
	       
