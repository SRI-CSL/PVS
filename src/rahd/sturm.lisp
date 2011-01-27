;;;
;;; Univariate Sturm Theory 
;;;
;;;   including: partial derivatives for polynomials in Q[\vec{x}],
;;;              derivations of univariate Sturm chains,
;;;              extraction of sign change sequences from evaluated Sturm chains,
;;;              local determination of number of real roots of a univariate 
;;;                polynomial in a closed real interval with rational endpoints,
;;; TODO:
;;;             *global determination of number of real roots of a univariate 
;;;                polynomial in [-inf, +inf] using Cauchy root bounds.
;;;              
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
;;; >>> Requires: polyalg.lisp, polyeval.lisp, (polyrtbd.lisp).
;;;
;;;
;;; This file: began on         16-July-2008,
;;;            last updated on  16-July-2008.
;;;

(in-package :rahd)

;;;
;;; DP/DV: Given a polynomial p and a variable ID v (w.r.t. *VARS-TABLE*),
;;; return the partial derivative of p w.r.t. v.
;;;

(defun dp/dv (p v)
  (cond ((endp p) nil)
	(t (let ((cur-m (car p)))
	     (let ((cur-c  (car cur-m))
		   (cur-pp (cdr cur-m)))
	       (let ((dcur-pp/dv (dpp/dv cur-pp v nil)))
		 (let ((d-scalar (car dcur-pp/dv))
		       (d-pp (cdr dcur-pp/dv)))
		   (poly-zsimp 
		    (cons `(,(s* cur-c d-scalar) . ,d-pp)
			  (dp/dv (cdr p) v))))))))))
  
;;; 
;;; DPP/DV: Given a power-product pp and a variable ID v (w.r.t. *VARS-TABLE*),
;;; return the partial derivative of pp w.r.t. v.  
;;;
;;; Note that we return a monomial (e.g. `(,coeff . ,power-product)).
;;; Note that the accumulator, accum-pp, should be initially nil.
;;;

(defun dpp/dv (pp v accum-pp)
  (cond ((endp pp) nil)
	(t (let ((cur-v (caar pp))
		 (cur-p (cdar pp)))
	     (cond ((= cur-v v)
		    `(,cur-p . ,(append accum-pp
					(cons `(,v . ,(1- cur-p))
					      (cdr pp)))))
		   ((< cur-v v) `(1 . ,(append accum-pp pp)))
		   (t (dpp/dv (cdr pp) v (append accum-pp (cons (car pp) nil)))))))))

;;;
;;; STURM-CHAIN: Given a polynomial, p, univariate in v, return the Sturm Chain
;;; derived from p w.r.t. v.
;;;

(defun sturm-chain (p v)
  (let ((cur-dp/dv (dp/dv p v)))
    (append `(,p ,cur-dp/dv) (sturm-chain* p cur-dp/dv nil))))

(defun sturm-chain* (p1 p2 sc-accum)
  (let ((cur-rem (cdr (poly-univ-/ p1 p2))))
    (cond ((or (eq cur-rem 0) (eq cur-rem nil)) 
	   (reverse sc-accum))
	  (t (sturm-chain* p2 (poly-negate cur-rem) 
			   (cons (poly-negate cur-rem) sc-accum))))))

;;;
;;; SIGN-CHANGES: Given a sequence of rational numbers, S, calculate the number of
;;; sign changes (ignoring zeroes) that occur are in S.
;;;

(defun sign-changes (S)
  (sign-changes* S (calc-sign (car S)) 0))

(defun sign-changes* (S c a)
  (cond ((endp S) a)
	(t (let ((cur-sign (calc-sign (car S))))
	     (cond ((or (= cur-sign 0)
			(= cur-sign c))
		    (sign-changes* (cdr S) c a))
		   (t (sign-changes* (cdr S) cur-sign 
				     (if (= c 0) a (1+ a)))))))))
	     
(defun calc-sign (r)
  (cond ((< r 0) -1)
	((> r 0) 1)
	(t 0)))

;;;
;;; POLY-UNIV-INTERVAL-REAL-ROOT-COUNT: Given a polynomial p, univariate in v, and two 
;;; rational numbers, a and b, compute the number of (unique) real roots of p there are in [a,b].
;;;

(defun poly-univ-interval-real-root-count (p a b)
  (let ((v (caadar p)))
    (let ((cur-sturm-chain (sturm-chain p v)))
      (+ (if (= (poly-univ-eval p a) 0) 1 0) 
	 (- (sign-changes 
	     (mapcar #'(lambda (poly) (poly-univ-eval poly a)) cur-sturm-chain)) 
	    (sign-changes 
	     (mapcar #'(lambda (poly) (poly-univ-eval poly b)) cur-sturm-chain)))))))


#|

 Some examples:

 (defparameter *p1* '( (2 . ((3 . 2) (2 . 7) (0 . 9)))  (3 . ((2 . 11)))))

 (poly-print *p1*)
"2x^9 z^7 u^2  +  3z^11"

(poly-print (dp/dv *p1* 2))
"14x^9 z^6 u^2  +  33z^10"

(poly-print (dp/dv *p1* 4))
"2x^9 z^7 u^2  +  3z^11"


(defparameter *f*
  '((1 . ((0 . 3))) (-2 . ((0 . 2))) (2 . ((0 . 1))) (8 . nil)))


(mapcar #'poly-print (sturm-chain *f* 0))
("3x^2  +  -4x  +  2" "4/9x  +  76/9" "1161")


(defparameter *p2*
  '( (1 . ((0 . 4))) (-5 . ((0 . 2))) (4)))

(poly-print *p2*)
"x^4  +  -5x^2  +  4"

(mapcar #'poly-print (sturm-chain *p2* 0))
("x^4  +  -5x^2  +  4" "4x^3  +  -10x" "5/2x^2  +  -4" "18/5x" "4")

(poly-univ-interval-real-root-count *p2* -2 2)
4

|#
