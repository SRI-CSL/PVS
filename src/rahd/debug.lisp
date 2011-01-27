;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Routines to help debug RAHD proof sessions **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         23-Oct-2008,
;;;            last updated on  23-Oct-2008.
;;;

(in-package :rahd)

;;;
;;; WITH-RAHD-DEBUGGING: Dynamically bind the RAHD verbosity level to 9 
;;;  (proof-debug print level), and dynamically enable *RAHD-DEBUG*.  We do this 
;;;  within an implicit PROGN provided by LET.
;;;

(defmacro with-rahd-debugging (&rest rst)
  (append '(let ((*rahd-verbosity* 9)
		 (*rahd-debug* t)))
    rst))

;;;
;;; WITH-RAHD-DEBUGGING*: Dynamically bind the RAHD verbosity level to 10 
;;;  (sys-debug print level), and dynamically enable *RAHD-DEBUG*.  We do this 
;;;  within an implicit PROGN provided by LET.
;;;

(defmacro with-rahd-debugging* (&rest rst)
  (append '(let ((*rahd-verbosity* 10)
		 (*rahd-debug* t)))
    rst))

;;;
;;; WITH-RAHD-VERBOSITY n: Dynamically bind the RAHD verbosity level to n
;;;  and provide an implicit PROGN via LET.
;;;

(defmacro with-rahd-verbosity (n &rest rst)
  (append `(let ((*rahd-verbosity* ,n)))
	     rst))
