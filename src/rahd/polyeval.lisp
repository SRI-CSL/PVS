;;;
;;; Multivariate Polynomial Evaluation 
;;;   for polynomials taken over Q[\vec{x}] and variable assignments over Q^n.
;;;
;;;   including: evaluation of univariate polynomials with rational variable binding in Q,
;;;              evaluation of multivariate polynomials with rational variable binding in Q^n,
;;;
;;;   for
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
;;; >>> Requires: polyalg.lisp
;;;
;;; Began on 17-June-2008
;;;

(in-package :rahd)

;;;
;;; POLY-UNIV-EVAL: Given a univariate polynomial p, return the result of evaluating
;;; it with value v for its variable.
;;;
;;; Note: We assume this univariate polynomial is in normal form (e.g. each power-product
;;; has only one entry).

(defun poly-univ-eval (p v)
  (poly-univ-eval* p v 0))

(defun poly-univ-eval* (p v a)
  (cond ((endp p) a)
	(t (let ((cur-m (car p)))
	     (let ((cur-coeff (car cur-m))
		   (cur-pow (cdar (cdr cur-m))))
	       (poly-univ-eval* (cdr p) v (+ a (* cur-coeff (expt v (if cur-pow cur-pow 0))))))))))
	       
	       
