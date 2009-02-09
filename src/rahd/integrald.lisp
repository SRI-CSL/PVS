;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Tactic for simple integral domain zero-product branching **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         25-Sept-2008,
;;;            last updated on  25-Sept-2008.
;;;

(in-package RAHD)

;;;
;;; INTEGRAL-DOMAIN-ZERO-PRODUCT-BRANCH: Given a case, if it contains (= (* A B) 0) for
;;; some variables A,B, return a waterfall-disjunction for (:OR (= A 0) (= B 0)).
;;;

(defun integral-domain-zero-product-branch (c)
  (let ((eqs (gather-eqs c))
	(ret-val c))
    (if (not eqs) c
      (dolist (eq eqs)
	(let ((cur-x (canonicalize-poly `(- ,(cadr eq) ,(caddr eq)))))
	  (if (consp cur-x)
	      (let ((cur-x-op (car cur-x))
		    (cur-x-x (cadr cur-x))
		    (cur-x-y (caddr cur-x)))
		;; (format *standard-output* "~% cur-x: " cur-x)
		(let ((v0-id (find-var cur-x-x *vars-table* 0))
		      (v1-id (find-var cur-x-y *vars-table* 0)))
		  (if (and (equal cur-x-op '*)
			   v0-id v1-id)
		      (return (setq ret-val 
				    (cons ':DISJ (append `((:OR (= ,cur-x-x 0)
								(= ,cur-x-y 0))) c)))))))))))
    ret-val))
