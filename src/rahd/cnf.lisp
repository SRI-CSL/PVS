;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; * Intermediate Division CNF form --> RAHD CNF processor.
;;; * This is not a pre-processor for arbitrary CNF formulas.
;;;
;;; *** Note: We assume NOT's have already been pre-processed away by EXPAND-SPECIAL-SYMs.
;;;
;;; * DIV-ELIM-PROCESSED-Goal -> RCF/CNF pre-processor.  This contains functions for 
;;;    converting goal formulas that have arisen via division-elimination into standard
;;;    RCF/CNF formulas amenable to (BUILD-GS ...).
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         09-Dec-2008,
;;;            last updated on  10-Dec-2008.
;;;

(in-package RAHD)

;;;
;;; F-TO-F-THRU-CNF: Given a formula, partially explicit CNF, convert it to 
;;; fully explicit CNF, then convert it back to implicit CNF.
;;;

(defun f-to-f-thru-cnf (f)
  (ef-to-cnf (make-cnf-explicit f)))

;;;
;;; CHECK-F-TO-F: Check the above conversion on the regression suite.
;;;

(defun check-f-to-f ()
  (mapcar #'(lambda (p) 
	      (let* ((f (car p))
		     (f* (f-to-f-thru-cnf f))
		     (passed? (equal f f*)))
		(format t "~% ------------------------------ ~% F0: ~A ~%~% F1: ~A ~%~% Passed? ~A~%------------------------------ ~%"
			f f* passed?) passed?))
	  *regression-suite*))

;;;
;;; MAKE-CNF-EXPLICIT: Given a RAHD implicit CNF formula, make the logical 
;;; connectives explicit.  
;;;
;;;  Ex: (((= x 1) (> x 1)) ((= y 2))) --> (:AND (:OR (= x 1) (> x 1)) (:OR (= y 2))).
;;;
;;;  Note: We also make the connectives all either unary or binary:
;;;
;;;  Ex: (((= x 1) (> x 1) (< x 1))) --> (:AND (:OR (:OR (= x 1) (> x 1)) (< x 1))).
;;;

(defun make-cnf-explicit (f)
  (cond ((not (consp f)) nil)
	((= (length f) 2)
	 `(:AND ,(make-clause-explicit (car f)) 
		,(make-clause-explicit (cadr f))))
	((= (length f) 1)
	 `(:AND ,(make-clause-explicit (car f))))
	(t `(:AND ,(make-clause-explicit (car f))
		  ,(make-cnf-explicit (cdr f))))))
 
(defun make-clause-explicit (c)
  (cond ((not (consp c)) nil)
	((= (length c) 2)
	 `(:OR ,(car c) ,(cadr c)))
	((= (length c) 1)
	 `(:OR ,(car c)))
	(t `(:OR ,(car c)
		 ,(make-clause-explicit (cdr c))))))

;;;
;;; EF-TO-CNF: Explicit formula to CNF formula conversion.
;;;

(defun ef-to-cnf (f)
  (explicit-to-implicit-cnf 
   (clausify-units
    (collapse-connectives (FIXED-POINT #'bu-and f)))))

;;;
;;; EXPLICIT-TO-IMPLICIT-CNF: Given a formula in explicit clausal form,
;;;  as in (:AND (:OR ...) (:OR ...) ...), place it in implicit clausal
;;;  form: (( ... ) ( ... ) ... ).
;;;

(defun explicit-to-implicit-cnf (f)
  (if (not (equal (car f) ':AND)) 
      (break "BAD CNF conversion: :AND expected.")
    (mapcar #'(lambda (x) 
		(if (equal (car x) ':OR) 
		    (cdr x) 
		  (break "Bad CNF conversion: :OR expected.")))
	    (cdr f))))

;;;
;;; CLAUSIFY-UNITS: Unit clauses will now be parts of the top-level :AND
;;; structure.  Let's clausify them by making them (:OR u).
;;;

(defun clausify-units (f)
  (append '(:AND)
	  (mapcar #'(lambda (x)
		      (cond ((not (consp x))
			     (break "CNF conversion error."))
			    ((not (equal (car x) ':OR))
			     `(:OR ,x))
			    (t x)))
		  (cdr f))))

;;;
;;; COLLAPSE-CONNECTIVES: Collapse connectives until we can do so
;;;  no more.
;;;

(defun collapse-connectives (f)
  (collapse-connectives-ORs
   (collapse-connectives-ANDs f)))

(defun collapse-connectives-ORs (f)
  (append '(:AND)
	  (mapcar #'(lambda (x) (FIXED-POINT #'collapse-connectives* x))
		  (cdr f))))
;;;
;;; COLLAPSE-CONNECTIVES-ANDs: Make it so that we have only one :AND
;;;  and it is the top-level connective.
;;; Note: We expect all of the :ANDs have been bubbled-up by this point.

(defun collapse-connectives-ANDs (f)
  (FIXED-POINT #'collapse-connectives* f))

;;;
;;; COLLAPSE-CONNECTIVES*: Given a formula like (:AND (:AND a b) c),
;;; return (:AND a b c).  This only does top-level processing.
;;;

(defun collapse-connectives* (f)
  (cond ((or (not (consp f))
	     (not (member (car f) '(:AND :OR)))) f)
	(t (let ((f-op (car f)))
	     (reduce #'append 
		     (mapcar #'(lambda (x) (collapse-in-context? f-op x))
			     (cdr f))
		     :initial-value (list f-op))))))

;;;
;;; COLLAPSE-IN-CONTEXT?
;;;  Given a context (e.g., either :AND or :OR), decide whether or not
;;;  the connective in F can be collapsed.  If it can, return (CDR F).
;;;  If it can't, return (list f).
;;;  We do this because the result will be APPENDED to a recursively
;;;  constructed formula that gave the context.
;;;

(defun collapse-in-context? (context-connective f)
  (cond ((not (consp f)) (list f))
	((equal (car f) context-connective)
	 (cdr f))
	(t (list f))))

;;;
;;; CNF-RULE-A: ((A and B) or C) --> ((A or C) and (B or C)).
;;;

(defun cnf-rule-A (f)
  (let ((o (or-p f)))
    (when o
      (let ((a (and-p (car o))))
	(when a
	  `(:AND (:OR ,(car a) ,(cdr o))
		 (:OR ,(cdr a) ,(cdr o))))))))

;;;
;;; CNF-RULE-B: (A or (B and C)) --> ((A or B) and (A or C)).
;;;

(defun cnf-rule-B (f)
  (let ((o (or-p f)))
    (when o
      (let ((a (and-p (cdr o))))
	(when a
	  `(:AND (:OR ,(car o) ,(car a))
		 (:OR ,(car o) ,(cdr a))))))))


;;;
;;; CNF-RULE-C: (or A) --> (and A).
;;;

(defun cnf-rule-C (f)
  (let ((o (or-p f)))
    (when (and o (= (length f) 2))
      `(:AND ,(cadr f)))))



;;;
;;; OR-P: Is f an :OR statement?  If so, return a (A . B) s.t.
;;;  f = (:OR a b).
;;;

(defun or-p (f)
  (when (and (consp f)
	     (equal (car f) ':OR)
	     (<= (length f) 3))
    (cons (cadr f) (caddr f))))

;;;
;;; AND-P: Is f an :AND statement?  If so, return a (A . B) s.t.
;;;  f = (:AND a b).
;;;

(defun and-p (f)
  (when (and (consp f)
	     (equal (car f) ':AND)
	     (= (length f) 3))
    (cons (cadr f) (caddr f))))

;;;
;;; BU-AND: Use CNF-RULE-A,B to bubble :ANDs up to the top.
;;;

(defun bu-and (f)
  (cond ((not (consp f)) f)
	((not (member (car f) '(:OR :AND))) f)
	(t (let ((updated-f f))
	     (setq updated-f 
		   (or (cnf-rule-A f)
		       (cnf-rule-B f)
		       (cnf-rule-C f)
		       f))
	     (if (= (length f) 3)
		 `(,(car updated-f)
		   ,(bu-and (cadr updated-f))
		   ,(bu-and (caddr updated-f)))
	       `(,(car updated-f)
		 ,(bu-and (cadr updated-f))))))))

		       
;;;
;;; FIXED-POINT (fcn tm): Given a function : tm -> tm, and a tm,
;;;  return a fixed point of fcn rooted in tm.  This loops the
;;;  application of fcn upon tm until simplification is stable.
;;;

(defun FIXED-POINT (fcn tm)
  (let ((tm tm)
	(last-tm nil)
	(fcn fcn))
    (loop until (equal tm last-tm) do
	  (setq last-tm tm)
	  (setq tm (funcall fcn tm)))
    tm))

