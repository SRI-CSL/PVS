;; qe.lisp -- 
;; Author          : Harald Ruess
;; Created On      : Mon Jul 28 09:18:53 PDT 1997
;; Last Modified By: Harald Ruess
;; Last Modified On: Mon Jul 28 09:18:53 PDT 1997
;; Update Count    : 0
;; Status          : Unknown, use with caution
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; Prover Interface for quantifier elimination

(addrule 'qe nil ((fnums '*) verbose?)
    (qe-step fnums verbose?)
    "Replaces each sequent formula in FNUMS with an equivalent
     quantifier-free formula if such a form can be constructed;
     otherwise the formula is left unchanged.

     Currently, arithmetic (INTEGER, RATIONAL, REAL) and boolean
     quantifiers are eliminated. Quantified formulas involving nonlinear
     arithmetic terms or bound variables in the scope of uninterpreted
     function or predicate symbols are usually not simplified.
 
     NOTE: This code is highly experimental, and its functionality,
     correctness, speed, and robustness is likely to improve in the near
     future. In particular: proof scripts using this command may break
     in future releases of PVS.")

(defun qe-step (fnums verbose?)
  #'(lambda (ps)
      (let ((*verbose* verbose?))
	(declare (special *verbose*))
	(multiple-value-bind (signal subgoal)
	    (sequent-reduce (current-goal ps) #'qe-sform fnums)
	  (values signal (list subgoal))))))

(defun qe-sform (sform)
  (let* ((fmla (formula sform))
         (new-fmla (qe-fmla fmla))
         (new-sform (if (tc-eq fmla new-fmla) sform
			(lcopy sform 'formula new-fmla))))
    (if (s-form-equal? sform new-sform)
        (values 'X sform)
      (values '? new-sform))))

(defun qe-fmla (fml)
  (let ((*translate-rewrite-rule* T)
	(*translate-quantifiers* T))
    (let* ((trm (top-translate-to-dc fml))
	   (ntrm (dp::qe-trm trm)))
      (if (eq trm ntrm) fml
	  (translate-from-dc ntrm)))))


;; Top-level for external calls

(defun qe (fmla &optional (*current-context* (copy-context *prelude-context*)))
  #+dbg(assert (expr? fmla))
  (let ((*pseudo-normalizing* nil)
	(*generate-tccs* 'NONE)
	(*assert-flag* 'simplify)
	(*process-output* nil)
	(*assert-if-arith-hash*
	 (if *assert-if-arith-hash*
	     *assert-if-arith-hash*
	     (make-hash-table :test #'eq)))
	(*newdc* nil)
	(*new-ground?* nil)
	(*old-ground?* t))
    (nprotecting-cong-state ((*dp-state* *init-dp-state*)
			     (*alists* *init-alists*))
       (let* ((*translate-id-hash* (clrhash *pseudo-normalize-translate-id-hash*))
	      (*translate-id-counter* nil)
	      (typealist typealist))
	 (newcounter *translate-id-counter*)
	 (let ((*verbose* nil))
	   (declare (special *verbose*))
	   (qe-fmla fmla state))))))
