;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1999/09/16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)


(defun sigbool (term cong-state &optional (after-solve nil))
  (cond
   ((equality-p term)
    (let ((lhs (lhs term))
	  (rhs (rhs term)))
      (cond
       ((eq rhs *true*)
	(cond
	 ((eq lhs *true*) *true*)
	 ((eq lhs *false*) *false*)
	 (t lhs)))
       ((eq rhs *false*)
	(cond
	 ((eq lhs *true*) *false*)
	 ((eq lhs *false*) *true*)
	 (t (sigma (mk-term (list *not* lhs)) cong-state after-solve))))
       ((eq lhs *true*) rhs)
       ((eq lhs *false*)
	(sigma (mk-term (list *not* rhs)) cong-state after-solve))
       ((monom-< lhs rhs) term)
       (t (mk-equality rhs lhs)))))
   ((ineq-p term) (sigineq term cong-state))
   ((negation-p term)
    (signegation term cong-state))
   ((and-p term)
    (sigand term cong-state))
   ((or-p term)
    (sigor term cong-state))
   ((nequal-p term)
    (sigma (mk-negation (mk-equality (lhs term) (rhs term)))
	   cong-state after-solve))
   (t term)))

(defun sigand (term cong-state)
  (let ((arg1 (arg 1 term))
	(arg2 (arg 2 term)))
    (cond
     ((false-p arg1) *false*)
     ((false-p arg2) *false*)
     ((true-p arg1) arg2)
     ((true-p arg2) arg1)
     (t term))))

(defun sigor (term cong-state)
  (let ((arg1 (arg 1 term))
	(arg2 (arg 2 term)))
    (cond
     ((true-p arg1) *true*)
     ((true-p arg2) *true*)
     ((false-p arg1) arg2)
     ((false-p arg2) arg1)
     (t term))))

(defun sigif (term cong-state)
  (let ((cond (arg 1 term))
	(then (arg 2 term))
	(else (arg 3 term)))
    (cond
     ((true-p cond) then)
     ((false-p cond) else)
     ((eq then else) then)
     (t term))))

(defun signegation (term cong-state)
  (cond
   ((ineq-p (arg 1 term))
    (sigineq (negineq (arg 1 term)) cong-state))
   ((true-p (arg 1 term)) *false*)
   ((false-p (arg 1 term)) *true*)
   ((negation-p (arg 1 term))
    (arg 1 (arg 1 term)))
   (t term)))

(defun solve-bool (bool cong-state)
  (cond
   ((eq bool *true*) ())
   ((eq bool *false*) (list *false*))
   ((number-type-p bool)
    (solve-type bool cong-state))
   ((negation-p bool) (list (mk-equality (arg 1 bool) *false*)))
   (t (list (mk-equality bool *true*)))))

