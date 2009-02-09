;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; High-level canonizer for goals.
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         12-Sept-2008,
;;;            last updated on  12-Sept-2008.
;;;

(in-package RAHD)

;;;
;;; Given a list of atoms, return the result of canonicalizing all terms within
;;; the atoms.
;;;

(defun canonize-terms (c)
  (cond ((not (consp c)) (break "CANONIZE-TERMS called with non-sensical clause."))
	(t (mapcar #'(lambda (l)
		       (case l
			     ((t nil) l)
			     (otherwise 
			      (let ((cur-r (car l))
				    (cur-x (cadr l))
				    (cur-y (caddr l)))
				`(,cur-r ,(canonicalize-poly cur-x)
					 ,(canonicalize-poly cur-y))))))
		   c))))

;;;
;;; ZERO-RHS : Zero the RHS of non-trivial formulas.
;;;
;;; Given an atom of the form:
;;;  (OP p1 p2) where p1 and p2 do not have either of the following shape:
;;;               p1=v, p2=n,  OR
;;;               p1=n, p2=v,  where v is a variable, 
;;;                                  n is a number,
;;;
;;;  we place (OP p1 p2) in the form (OP (- p1 p2) 0).
;;;
;;;  If OP is =, then we place (= p1 p2) as (= p2 p1) if p1=n and p2=v 
;;;    (e.g., (= 0 x) ==> (= x 0)).
;;;
;;; Note: <= and >= should not appear in terms here.  We expect these will have been expanded,
;;;   but will just soundly not transform literals rooted in them if we encounter them.
;;;

(defun zero-rhs (c)
  (cond ((not (consp c)) (break "ZERO-RHS called with non-sensical clause."))
	(t (mapcar #'(lambda (l)
		       (let ((cur-r (car l))
			     (cur-x (cadr l))
			     (cur-y (caddr l)))
			 (cond ((and (numberp cur-x) (not (consp cur-y)))     ; k < x     ==>   x < k
				(case cur-r
				      (< `(> ,cur-y ,cur-x))
				      (> `(< ,cur-y ,cur-x))
				      (= `(= ,cur-y ,cur-x))
				      (otherwise l)))
			       ((and (numberp cur-y)                               ; p(x) > k  ==>  p(x) - k > 0
				     (consp cur-x))
				(cond ((zerop cur-y) l)
				      (t `(,cur-r ,(canonicalize-poly `(- ,cur-x ,cur-y)) 0))))
			       ((and (numberp cur-x)                               ; k > p(x) ==> p(x) - k < 0
				     (consp cur-y))
				(cond ((zerop cur-x) 
				       (case cur-r
					     (< `(> ,cur-y ,cur-x))
					     (> `(< ,cur-y ,cur-x))
					     (= `(= ,cur-y ,cur-x))
					     (otherwise l)))
				      (t (case cur-r                               
					       (< `(> ,(canonicalize-poly `(- ,cur-y ,cur-x)) 0))
					       (> `(< ,(canonicalize-poly `(- ,cur-y ,cur-x)) 0))
					       (= `(= ,(canonicalize-poly `(- ,cur-y ,cur-x)) 0))
					       (otherwise l)))))
			       ((and (consp cur-x)                                
				     (consp cur-y))                               ; p(x) > q(x)  ==>  p(x) - q(x) > 0
				`(,cur-r ,(canonicalize-poly `(- ,cur-x ,cur-y)) 0))
			       ;((and (equal cur-r '=)
			       ;     (not (equal cur-y 0)))
			       ;`(= ,(canonicalize-poly `(- ,cur-x ,cur-y)) 0))   ; x*x = c      ==>  x*x - c = 0
			       (t l))))
		   c))))
				       