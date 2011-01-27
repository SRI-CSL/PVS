;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; Variable interval splitting routines, with a branch-and-bound invoker.
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         28-Feb-2009,
;;;            last updated on  23-Nov-2009.
;;;

;;;
;;; SPLIT-TM-FOR-CASE: Given a case and optionally a term, return c together
;;;  with a generator for a waterfall split on ((= tm 0) (< tm 0) (> tm)).
;;;
;;; If tm is not given, then the first var appearing in c will be selected.
;;;
;;; We now accept an optional pt (point) argument.  If pt is given, then
;;;  the interval for term tm will be split at pt.
;;;

(defun split-term-for-case (c &optional tm pt)
  (let ((a (if tm tm (car (all-vars-in-conj c))))
	(p (if pt pt 0)))
    `(:DISJ ((= ,a ,p) (< ,a ,p) (> ,a ,p)) ,@c)))

;;;
;;; BAB-VARS: Given a case, perform an interval split on a variable that
;;;  is minimally constrainted w.r.t. the current local IHT.
;;;

;; (defun bab-vars (c)
;;   (let ((vs (all-vars-in-conj c)))
;;     ))