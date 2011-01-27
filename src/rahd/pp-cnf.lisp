;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; DIV-ELIM-PROCESSED-Goal -> RCF/CNF pre-processor.  This contains functions for 
;;;  converting goal formulas that have arisen via division-elimination into standard
;;;  RCF/CNF formulas amenable to (BUILD-GS ...).
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         09-Dec-2008,
;;;            last updated on  09-Dec-2008.
;;;

(in-package :rahd)

;;;
;;; CNF-BU-AND:
;;;  We know f is always (implicitly) of the following form:
;;;   (:AND (:OR ...) (:OR ...) ... (:OR ...)) 
;;;  s.t. some of the (:OR ...)'s may contain literals of the form
;;;   (:AND X Y).
;;;  This nesting is only one level deep.
;;;  We use this to our advantage, as we are *almost* already clausal.
;;;

(defun cnf-bu-and* (f)
  (let ((out-f nil))
    (dolist (cur-clause f)
      (let ((cur-front nil)
	    (cur-back (cdr cur-clause))
	    (updated-clause nil))
      (dolist (cur-lit cur-clause)
	(let* ((bu-lit (cnf-bu-and-rule cur-lit cur-front cur-back))
	       (updated-lit (if bu-lit bu-lit cur-lit)))
	  (setq updated-clause (append updated-clause (list updated-lit)))
	  (setq cur-front (append cur-front (list updated-lit)))
	  (setq cur-back (cdr cur-back))))
      (setq out-f (append out-f (list updated-clause)))))
    out-f))
				       

;;;
;;; CNF-RULE-A.
;;;  (:OR c1 ... cn (:AND a b) cn+1 ... ck) --> (:AND (:OR a c1 ... ck) (:OR b c1 ... ck)).
;;;

(defun cnf-bu-and-rule (f front back)
  (if (and (consp f)
	   (equal (car f) ':AND)
	   (= (length f) 3))
      `(:AND ,(append `(:OR ,(cadr f))
		      (append front back))
	     ,(append `(:OR ,(caddr f))
		      (append front back)))
    nil))
		       
