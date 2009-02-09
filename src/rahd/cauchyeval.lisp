;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Eventually: An evaluator for ground literals involving real algebraic numbers, using
;;;    a representation of exact real numbers as explicit Cauchy sequences **
;;; ** Currently: A function to compute rational nth roots of a rational when they exist.
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         23-Sept-2008,
;;;            last updated on  14-Nov-2008.
;;;

(in-package RAHD)

;;;
;;; Given a rational x, is its nth root a rational?  If so, we return it.
;;; If not, we return nil.
;;;
;;; Note: We return only `positive' roots (e.g. if n is even).
;;;
;;; Updated on 14-Nov-2008 using an excellent suggestion of Paul Jackson:
;;;  Since (p/q)^n = (p^n / q^n), and floating point representations of 
;;;  rational numbers require denominators to be powers of 2, we can get
;;;  much better coverage by computing (p^n)^(1/n) and (q^n)^(1/n) separately,
;;;  and then can combine them as ((p/q)^n)^(1/n) = ((p^n)^(1/n) / (q^n)^(1/n)).
;;;  Now, we only use floating point representations of integers, whose denom's
;;;  are trivially powers of 2!
;;;

(defun nth-root-rational? (x n)
  (let ((approx-nth-root (/ (rational (expt (numerator x) (/ 1 n)))
			    (rational (expt (denominator x) (/ 1 n))))))
    (let ((rational-approx-x (expt approx-nth-root n)))
      (if (= rational-approx-x x)
	  approx-nth-root
	nil))))

(defun EXACT-REAL-EXPT (x y)
  (let ((nrr (nth-root-rational? x (/ 1 y))))
    (when (not nrr)
      (break "~Dth root of x is not rational" (/ 1 y)))
    nrr))