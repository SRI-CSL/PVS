;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** GB-based Real Nullstellensatz procedure, using interval constraints **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         21-Nov-2009,
;;;            last updated on  22-Nov-2009.
;;;

;;;
;;; ZRHS-ATOM: Given an atom, return its ZRHS equivalent without
;;;  canonicalization.  If the atom is an inequality, orient it
;;;  so that it is either > or >=.
;;;

(in-package :rahd)

(defun zrhs-g-atom (a)
  (let ((op (car a))
	(x (cadr a))
	(y (caddr a)))
    (let ((lhs (if (equal y 0) x `(- ,x ,y))))
      (case op 
	    ((= > >=) `(,op ,lhs 0))
	    (< `(> (* -1 ,lhs) 0))
	    (<= `(>= (* -1 ,lhs) 0))))))

;;;
;;; FRESH-VARS: Return a list of n fresh symbols.  These will be
;;;  used as slack variables.
;;;

(defun fresh-vars (n)
  (let ((out nil))
    (loop for i from 1 to n do
	  (setq out (cons (gensym) out)))
    out))

;;;
;;; SLACK-POLYS: Given a list of n polynomials and n variables,
;;;  return the result of using each v[i] as a slack variable
;;;  (negative summand) for p[i].
;;;

(defun slack-polys (ps vs)
  (let ((out nil) (vs* vs))
    (dolist (p ps)
      (setq out (cons `(- ,(car vs*) ,p) out))
      (setq vs* (cdr vs*)))
    out))

;;;
;;; BUILD-EQ-SYS: Build an equational system from an arbitrary case.
;;;  This is done by introducing slack variables corresponding to
;;;  the inequations of the case in a way similar to the Tiwari
;;;  method.
;;;
;;; We return an mv form of the following species:
;;;  (equational-system lst-of->-vars lst-of->=-vars).
;;;

(defun build-eq-sys (c)
  (let ((=-atoms (mapcar #'zrhs-g-atom (gather-eqs c)))
	(>-atoms (mapcar #'zrhs-g-atom (gather-strict-ineqs c)))
	(>=-atoms (mapcar #'zrhs-g-atom (gather-soft-ineqs c))))
    (let ((=-polys (mapcar #'(lambda (x) (cadr x)) =-atoms))
	  (>-polys (mapcar #'(lambda (x) (cadr x)) >-atoms))
	  (>=-polys (mapcar #'(lambda (x) (cadr x)) >=-atoms)))
      (let ((>-vars (fresh-vars (length >-polys)))
	    (>=-vars (fresh-vars (length >=-polys))))
	(let ((slacked->-polys (slack-polys >-polys >-vars))
	      (slacked->=-polys (slack-polys >=-polys >=-vars)))
	  (values 
	   (union =-polys
		  (union slacked->-polys slacked->=-polys))
	   >-vars >=-vars))))))

;;;
;;; FULL-GB-FOR-EQ-SYS: Given an equational system, compute a 
;;;  full (not necessarily reduced) GB for it.
;;;

(defun full-gb-for-eq-sys (ps)
  (let ((gb (gbasis 
	     (remove-if
	      #'(lambda (x) (or (equal x nil) (equal x 0)))
	      (mapcar 
	       #'poly-prover-rep-to-alg-rep 
	       ps)))))
    (mapcar #'poly-alg-rep-to-prover-rep 
	    gb)))

;;;
;;; MAKE->-ATOMS: Given a list of variables, return a list
;;;  of atoms of the form (> v[i] 0) for each v[i].
;;;

(defun make->-atoms (vs)
  (let ((out nil))
    (dolist (v vs)
      (setq out (cons `(> ,v 0) out)))
    out))

(defun make->=-atoms (vs)
  (let ((out nil))
    (dolist (v vs)
      (setq out (cons `(>= ,v 0) out)))
    out))

(defun make-=-atoms (ps)
  (let ((out nil))
    (dolist (p ps)
      (setq out (cons `(= ,p 0) out)))
    out))

;;;
;;; CHECK-SAT-GB-NULL: Check satisfiability of equational system
;;;  using a full GB and interval constraint propagation upon
;;;  the slack variables.
;;;

(defun check-sat-gb-null (ps >-vars >=-vars)
  (let ((gb (full-gb-for-eq-sys ps)))
    (let ((>-atoms (make->-atoms >-vars))
	  (>=-atoms (make->=-atoms >=-vars))
	  (=-atoms (make-=-atoms gb)))
      (icp-on-case (union >-atoms 
			  (union >=-atoms =-atoms))))))

;;;
;;; FULL-GB-REAL-NULL-ON-CASE: Given a case, perform a full GB 
;;;  real nullstellensatz search on it using interval constraints.
;;;
;;; * This becomes infeasible very quickly; the bounded version
;;;   below is much more sensible for nontrivial problems.
;;;

(defun full-gb-real-null-on-case (c)
  (multiple-value-bind 
   (ps >-vars >=-vars)
   (build-eq-sys c)
   (let ((icp-result (check-sat-gb-null ps >-vars >=-vars)))
     (if (equal (car icp-result) ':UNSAT)
	 `(:UNSAT (:REAL-NULLSTELLENSATZ-WITNESS :INDUCED-EMPTY-INTERVAL
						 ,(cdr icp-result)))
       c))))

;;;
;;; CHECK-SAT-EQ-SYS: Given an equational system with >,>= -vars,
;;;  use ICP to check SAT.
;;;

(defun check-sat-eq-sys (ps >-vars >=-vars &key union-case)
  (let ((>-atoms (make->-atoms >-vars))
	(>=-atoms (make->=-atoms >=-vars))
	(=-atoms (make-=-atoms ps)))
    (icp-on-case (union (union >-atoms 
			       (union >=-atoms =-atoms))
			union-case))))

;;;
;;; BOUNDED-GB-REAL-NULL: Given a prepared (polys >-vars >=-vars)
;;;  system, perform a bounded search for Real Nullstellensatz
;;;  witnesses using incremental interval constraint propagation
;;;  during bounded basis completion.
;;;

#-allegro 
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun bounded-gb-real-null (ps >-vars >=-vars 
				&key gb-bound icp-period iht union-case)
  (let ((gb-bound (or gb-bound 20))
	(icp-period (or icp-period 5))
	(iht (or iht (iht-clone *i-boxes-num-local*)))
	(G (remove-if #'(lambda (x) (or (equal x nil) (equal x 0))) 
		      (mapcar #'poly-prover-rep-to-alg-rep ps)))
	(count 0)
	(unsat-witness-found? nil))
    (declare (ignore iht))  
    (let* ((s-pair-lst (s-pairs G))
	   (done? (endp s-pair-lst)))
      (while (not done?) do
	     (let ((cur-s-poly (car s-pair-lst)))
	       (let ((s-reduct-via-G (cdr (poly-multiv-/ cur-s-poly G))))
		 (setq count (1+ count))
		 (cond ((eq s-reduct-via-G nil)
			(setq s-pair-lst (cdr s-pair-lst)))
		       (t (setq s-pair-lst 
				(new-spolys s-reduct-via-G G (cdr s-pair-lst)))
			  (setq G (cons s-reduct-via-G G))))
		 (when (equal (mod count icp-period) 0)
		   (setq unsat-witness-found? 
			 (equal (car (check-sat-eq-sys 
				      (mapcar #'poly-alg-rep-to-prover-rep G) 
				      >-vars >=-vars :union-case union-case)) 
				':UNSAT)))
		 (setq done? (or (endp s-pair-lst)
				 (> count gb-bound)
				 unsat-witness-found?))))))
    (if (not unsat-witness-found?)
	(setq unsat-witness-found? 
	      (equal (car (check-sat-eq-sys 
			   (mapcar #'poly-alg-rep-to-prover-rep G) 
			   >-vars >=-vars :union-case union-case)) 
		     ':UNSAT)))
    (when unsat-witness-found? '(:UNSAT))))

;;;
;;; BOUNDED-GB-REAL-NULL-ON-CASE:.
;;;

(defun bounded-gb-real-null-on-case (c gb-bound icp-period union-case)
  (cond ((> (length c) 1)
	 (let ((vars-table *vars-table*))
	   (multiple-value-bind 
		 (ps >-vars >=-vars)
	       (build-eq-sys c)
	     (when union-case (fmt 2 "~% union-case ~%"))
	     (let ((icp-result (bounded-gb-real-null ps >-vars >=-vars 
						     :gb-bound gb-bound
						     :icp-period icp-period
						     :union-case (when union-case c))))
	       (setq *vars-table* vars-table)
	       (if (and (consp icp-result) (equal (car icp-result) ':UNSAT))
		   `(:UNSAT (:REAL-NULLSTELLENSATZ-WITNESS :INDUCED-EMPTY-INTERVAL
			     ,(cdr icp-result)))
		   c)))))
	(t c)))
