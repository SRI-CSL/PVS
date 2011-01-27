;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Real nullstellensatz equational refutation routines for simple (explicit) 
;;;    real nullstellensatz certificates **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         22-Sept-2008,
;;;            last updated on  22-Sept-2008.
;;;

(in-package :rahd)

(defun simp-real-nullstellensatz (c)
  (let ((cert? (some #'simple-real-nullstellensatz-cert? c)))
    (if cert? `(:UNSAT :REAL-NULLSTELLENSATZ :POLY-IN-EQ-IS-NEVER-ZERO :OFFENDING-CLAIM ,cert?) c)))

;;;
;;; SIMPLE-REAL-NULLSTELLENSATZ-CERT?
;;; Given an equational constraint e, is e a simple real nullstellensatz refutation certificate?
;;; Note: It is OK if we are given a non-equational constraint -- This will just cause us
;;;  to quickly return NIL.
;;;
;;; If e is a certificate, we return e, so that it can be placed in the *GS* proof object.
;;;

(defun simple-real-nullstellensatz-cert? (e)
  (assert (= (length e) 3))
  (let ((op (car e))
	(cur-x (cadr e))
	(cur-y (caddr e)))
    (if (not (equal op '=)) nil
      (let ((adj-x (if (not (equal cur-y 0))
		       (poly- (poly-prover-rep-to-alg-rep cur-x) 
			      (poly-prover-rep-to-alg-rep cur-y))
		     (poly-prover-rep-to-alg-rep cur-x))))
	(if (or (and (> (poly-deg-zero-scalar adj-x) 0)
		     (poly-trivial-square adj-x))
		(let ((neg-adj-x (poly-mult '((-1)) adj-x)))
		  (and (> (poly-deg-zero-scalar neg-adj-x) 0)
		       (poly-trivial-square neg-adj-x))))
	    e nil)))))
	
    
