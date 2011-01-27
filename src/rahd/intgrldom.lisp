;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
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
;;;            last updated on  18-Sept-2009.
;;;

;;;
;;; INTEGRAL-DOMAIN-ZERO-PRODUCT-BRANCH: Given a case, if it contains (= (* A B) 0) for
;;; some variables A,B, return a waterfall-disjunction for (:OR (= A 0) (= B 0)).
;;;
;;; Note: If the :all-terms flag is set, then we will perform the above operation for
;;;  zero-products of arbitrary terms.
;;;

(in-package :rahd)

(defun integral-domain-zpb (c &key all-terms)
  (let ((eqs (gather-eqs c)))
    (if (not eqs) 
	c
      (let ((found-zp nil))
	(dolist (eqn eqs)
	  (if found-zp (return))

	  ;;
	  ;; Now, here we need to take care to not destroy a ZP through
	  ;; canonicalization.  If the RHS of an equation is *not* 0, then we
	  ;; need to do the normal LHS := LHS-RHS and canonicalize it.  
	  ;; But, *if* the RHS is already 0, then doing so could destroy the
	  ;; ZP, since canonicalization will always fully multiply all polynomials
	  ;; into a sum-of-monomials form.  Thus, to avoid this, we first check
	  ;; if the RHS of the eq is already 0.  If it is, then we do the following:
	  ;;  (i) If LHS is already a product, then we use it as the ZP.
	  ;; (ii) Else, then we canonicalize LHS, check if the result is a product, 
	  ;;       using it as the ZP if so.
	  ;;
	  ;; For when RHS is non-zero, then we do canonicalization from the start,
	  ;;  as the normal (- LHS RHS) form for the new LHS will never be a product
	  ;;  without simplification.
	  ;;

	  (let ((cur-x (if (and (equal (caddr eqn) 0)
				(equal (car (cadr eqn)) '*))
			   (cadr eqn)
			 (canonicalize-poly `(- ,(cadr eqn) ,(caddr eqn))))))
	    (if (consp cur-x)
		(let ((cur-x-op (car cur-x))
		      (cur-x-x (cadr cur-x))
		      (cur-x-y (caddr cur-x)))
		  (fmt 5 "[int-dom-zpb] cur-x-op: ~A~%, cur-x-x: ~A~%, cur-x-y: ~A~%."
		       cur-x-op cur-x-x cur-x-y)
		  (when (endp *vars-table*)
		    (canonicalize-poly cur-x)) ; Rehash the *vars-table* if needed.
		  (let ((v0-id (find-var cur-x-x *vars-table* 0))
			(v1-id (find-var cur-x-y *vars-table* 0)))
		    (when (and (equal cur-x-op '*) 
			       (or all-terms (and v0-id v1-id)))
		      (setq found-zp `((:OR (= ,cur-x-x 0)
					    (= ,cur-x-y 0))))))))))
	(fmt 4 "[int-dom-zpb] after loop: found-zp = ~A~%." found-zp)
	(if found-zp 
	    `(:DISJ ,@(append found-zp c))
	  c)))))


(defun integral-domain-zpb-gen (c)
  (integral-domain-zpb c :all-terms t))
