;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; boolean.lisp -- 
;; Author          : Harald Ruess
;; Created On      : 1999/09/16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defvar *solve-bool* t)

(defun sig-bool (trm cong-state &optional (after-solve nil))
  (declare (type node trm)
	   (ignore cong-state)
           (ignore after-solve))
  (with-bdd-environment (*false* *true* 'predicate)
     #+dbg(assert (bdd-p trm))
     (cond ((true-or-false-p trm)
	    trm)
	   ((negation-p trm)
	    (bdd-not (arg 1 trm)))
	   ((conjunction-p trm)
	    (bdd-and (lhs trm) (rhs trm)))
	   ((disjunction-p trm)
	    (bdd-or (lhs trm) (rhs trm)))
	   ((implication-p trm)
	    (bdd-implies (lhs trm) (rhs trm)))
	   ((ite-p trm)
	    (multiple-value-bind (cnd thn els)
		(destructure-ite trm)
	      (bdd-ite cnd thn els)))
	   (t (bdd-var trm)))))

(defun sig-iff (trm cs)
  (declare (type node trm)
	   (ignore cs))
  (with-bdd-environment (*false* *true* 'predicate)
     (bdd-iff (lhs trm)
	      (rhs trm))))

(defun sig-xor (trm cs)
  (multiple-value-bind (lhs rhs)
      (destructure-disequality trm)
    (with-bdd-environment (*false* *true* 'predicate)  
       (bdd-xor lhs rhs))))
	
(defun solve-bool (trm cong-state)
  (declare (type node trm)
	   (type cong-state cong-state))
  (cond ((true-p trm) ())
	((false-p trm) (list *false*))
	((number-type-p trm)
	 (solve-type trm cong-state))
	(t (if *solve-bool*
	       (with-bdd-environment (*false* *true* 'predicate)
		  (bdd-solve trm))
	       (list trm)))))








