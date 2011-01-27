;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Some short abbreviations for RAHD interface commands and tactics **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         28-Feb-2009,
;;;            last updated on  26-Oct-2009.
;;;

;;;
;;; Some abbreviations for RAHD commands.
;;;

(in-package :rahd)

(defun r () (rahd-reset-state))

(defun b () (build-gs))

(defun p (&optional bound) (pug bound))

(defun tr () (tactic-replay))

(defun epa () (setq *enable-proof-analysis* t))
(defun ppa () (print-proof-analysis))
(defun pa () (print-proof-analysis))

(defun go!pa () (epa) (go!) (ppa))

(defmacro wrv (&rest rst)
  `(with-rahd-verbosity ,@rst))

(defmacro wrs (&rest rst)
  `(with-rahd-verbosity 0 ,@rst))

(defmacro wrd (&rest rst)
  `(with-rahd-debugging ,@rst))

;; ``with outcome only''

(defmacro woo (&rest rst)
  `(with-rahd-verbosity 1/2 ,@rst))

(defparameter *regression-suite* nil)

(defmacro rrs (&rest rst)
  `(rahd-regression-suite ,@rst))

(defun ctr ()
  (setq *tactic-replay* nil))

(defun u ()
  (up))

(defun swap-to-subgoal (k)
  (swap-to-goal k))

(defparameter *rrs*
  *regression-suite*)

(defparameter *rs*
  *regression-suite*)

(defun c ()
  (current-stats))

;;;
;;; Some abbreviations for RAHD tactics.
;;;

(defun ocad () (open-ex-inf-cad))

(defun ofcad () (open-frag-ex-inf-cad))

(defun gcad () (gen-ex-cad))

(defun ss () (stable-simp))

(defun sz () (simp-zrhs))

(defun srn () (simp-real-null))

;;;
;;; Some control macros.
;;;

;;;
;;; EXEC-UNTIL-SAT: Execute a sequence of forms (usually tactics)
;;;  but cease execution if a SAT case has been found.
;;;

(defmacro exec-until-sat (&rest rst)
  (let ((out nil))
    (dolist (r rst)
      (setq out (cons `(when (not *sat-case-found?*) ,r) out)))
    `(progn ,@(reverse out))))
