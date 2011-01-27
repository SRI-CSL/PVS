;;;
;;; A decision method for testing the emptiness of open semialgebraic sets,
;;;  each defined as the intersection of finitely many open sets, each defined
;;;  as the set of satisfying real vectors of a polynomial strict inequality.
;;;
;;; This is done via a connection to the cylindrical algebraic decomposition
;;;  package QEPCAD-B, by quantifying every variable in an open semialgebraic
;;;  definition with the EXISTS-INFINITELY-MANY quantifier ((F x) in QEPCAD-B
;;;  notation).
;;;
;;;    for
;;; 
;;;     RAHD: Real Algebra in High Dimensions
;;;   
;;;   v0.5,
;;;
;;; A feasible decision method for the existential theory of real closed fields.
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: (g.passmore@ed.ac.uk . http://homepages.inf.ed.ac.uk/s0793114/)
;;;
;;; >>> Requires: sturmineq.lisp
;;;
;;;
;;; This file: began on         31-July-2008,
;;;            last updated on  22-Nov-2009.
;;;

(in-package :rahd)

;;;
;;; Note: We have now added a ``generic'' mode for interfacing with QEPCAD.
;;;  This mode is for semialgebraic sets that are not a priori known to be
;;;  open.  In this case, we cannot use the EX-INF-MANY relaxation, and just
;;;  do straight-forward CAD.
;;;

;;;
;;; Given an open conjunction, produce an EX-INF relaxation of the problem
;;; as a string in QEPCAD-B notation.
;;;
;;; If generic is T, then we do generic CAD instead of EX-INF relaxation.
;;;

(defun open-cad (c)
  (cad c nil))

(defun gen-cad (c)
  (cad c t))

#+ccl (defun cad (c generic) (declare (ignore c generic)) nil)

#+allegro (defun cad (c generic)
  (cond ((and (not generic) (not (open-conj c))) c)
	((not (all-vars-in-conj c)) c)
	(t (progn
	     (write-open-cad-file c generic)
	     (let ((error-code
		    (#+allegro excl:run-shell-command
			       #+cmu extensions:run-program
			       "qepcad.bash")))
	       (fmt 10 "~% [CAD] :: Sys-call for QEPCAD.BASH successfull with exit code: ~A. ~%" error-code)
	       (if #+allegro (= error-code 0)
		 #+cmu t ;; Need to learn how to get CMUCL error code here.
		 (with-open-file (cad-output "proofobl.out" :direction :input)
				 (let ((cad-decision (read-line cad-output nil)))
				   (fmt 10 "~% [CAD] :: CAD decision: ~A ;  Generic? ~A. ~%" cad-decision generic)
				   (if (equal cad-decision "\"FALSE\"")
				       (if (not generic)
					   '(:UNSAT (:OPEN-PREDICATE :EX-INF-MANY-RELAXATION :QEPCAD-B-REDUCES-TO-FALSE))
					 '(:UNSAT (:GENERIC-PREDICATE :QEPCAD-B-REDUCES-TO-FALSE)))				   
				     (if (equal cad-decision "\"TRUE\"")
					 (if (not generic)
					     '(:SAT (:OPEN-PREDICATE :EX-INF-MANY-RELAXATION :QEPCAD-B-REDUCES-TO-TRUE))
					   '(:SAT (:GENERIC-PREDICATE :QEPCAD-B-REDUCES-TO-TRUE)))
				       c))))
		 c))))))


;;;
;;; OPEN-FRAG-CAD: Given a conjunction that may contain equations, extract only the
;;; open fragment (strict inequalities) and check to see whether or not that subset
;;; of constraints is unsatisfiable over the reals, via OPEN-CAD.
;;;

(defun open-frag-cad (c)
  (let ((open-frag (gather-soft-ineqs c)))
    (if open-frag 
	(let ((result (open-cad open-frag)))
	  (if (or (equal result open-frag) (equal (car result) ':SAT)) ; Cannot trust :SAT here
	      c
	    result))
    c)))

(defparameter is-open? t)

(defun open-conj (c)
  (progn
    (setf is-open? t)
    (dolist (lit c)
      (let ((cur-r (car lit)))
	(setf is-open? (and is-open? (or (equal cur-r '<) (equal cur-r '>))))))
    is-open?))

;;;
;;; RUN-CAD-ON-ENTIRE-GOAL: Run CAD on an entire top-level *G*.
;;;  Note that this goal, G, must already be processed into RAHD RCF form.
;;;

#+ccl (defun run-cad-on-entire-goal (goal) (declare (ignore goal)) nil)

#+allegro 
(defun run-cad-on-entire-goal (goal)
  (let ((g (tlf-to-bin-ops goal)))
    (let ((vars-in-g nil))
      (dolist (c g)
	(setq vars-in-g (union vars-in-g (all-vars-in-conj c)))
	vars-in-g)
      (let ((cad-cnf "")
	    (count 0))
	(dolist (c g)
	  (setq cad-cnf
		(format nil "~A ~A [~A]"
			cad-cnf
			(if (> count 0) " /\\ " "")
			(disj-to-qcb c nil)))
	  (setq count (1+ count)))

	(let ((cad-input
	       (format nil "[ RAHD top-level Goal ]~%~A~%~A~%~A~%finish.~%"
		       (format nil "(~{~D~#[~:;, ~]~})" vars-in-g)
		       0
		       (format nil "~A[~A]." (ex-quant-prefix vars-in-g) cad-cnf))))

	  (with-open-file (cad-file "proofobl.in.raw" :direction :output :if-exists :supersede)
			  (write-line cad-input cad-file)))

	(let ((start-time (get-internal-real-time)))

	  (let ((error-code
		 (#+allegro excl:run-shell-command
			    #+cmu extensions:run-program
			    "qepcad.bash")))

	    (let* ((end-time (get-internal-real-time))
		   (total-time (float (/ (- end-time start-time) internal-time-units-per-second))))
	    
	      (with-open-file (cad-output "proofobl.out" :direction :input)
			      (let ((cad-decision (read-line cad-output nil)))
				(if (equal cad-decision "\"FALSE\"")
				    (fmt -1 "~% ~A solved by QEPCAD-B from top-level in approx. ~A.~% "
					 *cur-prob* total-time)
				  (fmt -1 "~% ~A _NOT_ solved by QEPCAD-B.  It took approx. ~A for QEPCAD-B to fail on this problem.~%"
				       *cur-prob* total-time)))))))))))


(defun write-open-cad-file (c generic)
  (with-open-file (cad-file "proofobl.in.raw" :direction :output :if-exists :supersede)
		  (dolist (l (open-cad-input c generic))
		    (write-line l cad-file))))

(defun open-cad-input (c generic)
  `("[ RAHD Goal ]"
    ,(open-cad-vars-lst c)
    "0"
    ,(open-cad-conj c generic)
    "finish."))

(defun open-cad-vars-lst (c)
  (let ((all-vars (all-vars-in-conj c)))
    (format nil "(~{~D~#[~:;, ~]~})" all-vars)))

(defun open-cad-conj (c generic)
  (concatenate 
   'string
   (ex-inf-quant-prefix c generic)
   "[" (conj-to-qcb c "") "]."))

(defparameter qp "")

(defun ex-inf-quant-prefix (c generic)
  (let ((all-vars (all-vars-in-conj c)))
    (progn
      (setq qp "")
      (dolist (v all-vars)
	(setq qp (concatenate 
		  'string 
		  qp 
		  (if (not generic) "(F " "(E ")
		  (format nil "~D" v) ") ")))
      qp)))

(defun ex-quant-prefix (vars)
  (let ((qp ""))
    (dolist (v vars)
      (setq qp (concatenate 
		'string 
		qp 
		"(E "
		(format nil "~D" v) ") ")))
    qp))

(defparameter all-vars nil)

(defun all-vars-in-conj (c)
  (setq all-vars nil)
  (dolist (lit c)
    (let ((use-lit 
	   (if (equal (car lit) 'NOT)
	       (cadr lit)
	     lit)))
      (setq all-vars 
	    (union all-vars
		   (union 
		    (gather-vars (cadr use-lit))
		    (gather-vars (caddr use-lit)))))))
    all-vars)
    
(defun conj-to-qcb (c result)
  (cond ((endp c) result)
	(t (let ((cur-lit (car c)))
	     (conj-to-qcb
	      (cdr c)
	      (concatenate 'string
			   result
			   (lit-to-qcb cur-lit)
			   (if (consp (cdr c)) " /\\ " "")))))))

(defun disj-to-qcb (d result)
  (cond ((endp d) result)
	(t (let ((cur-lit (car d)))
	     (disj-to-qcb
	      (cdr d)
	      (concatenate 'string
			   result
			   (lit-to-qcb cur-lit)
			   (if (consp (cdr d)) " \\/ " "")))))))


(defun lit-to-qcb (lit)
  (concatenate 'string
	       "["
	       (let ((cur-r (car lit))
		     (cur-x (cadr lit))
		     (cur-y (caddr lit)))
		 (concatenate 'string
			      (term-to-qcb cur-x) 
			      " "
			      (write-to-string cur-r)
			      " "
			      (term-to-qcb cur-y)))
	       "]"))

(defun term-to-qcb (term)
  (cond ((equal term nil) "")
	((numberp term) 
	 (if (< term 0) 
	     (format nil "(0 - ~d)" (write-to-string (- (rational term))))
	     (write-to-string (rational term))))
	((varp term) (format nil "~D" term))
	((consp term)
	 (let ((cur-f (car term))
	       (cur-x (cadr term))
	       (cur-y (caddr term)))
	   (concatenate 
	    'string
	    "(" (term-to-qcb cur-x)
	    (if (equal cur-f '*) " " 
	      (format nil " ~d " (write-to-string cur-f)))
	    (term-to-qcb cur-y) ")")))))
		   
(defun varp (term)
  (and (symbolp term)
       (not (equal term '=))
       (not (equal term '>))
       (not (equal term '<))
       (not (equal term '<=))
       (not (equal term '>=))))


;;;
;;; Functions for exporting REDLOG input.
;;;

(defun goal-to-redlog (goal)
  (let ((g (tlf-to-bin-ops goal)))
    (let ((vars-in-g nil))
      (dolist (c g)
	(setq vars-in-g (union vars-in-g (all-vars-in-conj c)))
	vars-in-g)
      (let ((redlog-formula "")
	    (count 0))
	(dolist (c g)
	  (setq redlog-formula
		(format nil "~A~% phi~A := ~A;"
			redlog-formula
			count
			(disj-to-redlog c "")))
	  (setq count (1+ count)))
	(format nil "~A~% phi := ex({~{~D~#[~:;, ~]~}}, ~A);" 
		redlog-formula 
		vars-in-g 
		(let ((out ""))
		  (loop for i from 0 to (1- count) do
			(setq out (format nil "~A~Aphi~A"
					out (if (> i 0) " and " "") i)))
		  out))))))


(defun term-to-redlog (term)
  (cond ((equal term nil) "")
	((numberp term) 
	 (if (< term 0) 
	     (format nil "(0 - ~d)" (write-to-string (- (rational term))))
	   (write-to-string (rational term))))
	((varp term) (format nil "~D" term))
	((consp term)
	 (let ((cur-f (car term))
	       (cur-x (cadr term))
	       (cur-y (caddr term)))
	   (concatenate 
	    'string
	    "(" (term-to-redlog cur-x)
	    (format nil " ~d " (write-to-string cur-f))
	    (term-to-redlog cur-y) ")")))))

(defun disj-to-redlog (d result)
  (cond ((endp d) result)
	(t (let ((cur-lit (car d)))
	     (let ((use-lit (if (equal (car cur-lit) 'NOT)
				(cadr cur-lit)
			      cur-lit)))
	     (disj-to-redlog
	      (cdr d)
	      (format nil "~A~A~A~A"
		      result
		      (if (not (equal use-lit cur-lit))
			  "not" "")
		      (lit-to-redlog use-lit)
		      (if (consp (cdr d)) " or " ""))))))))

(defun lit-to-redlog (lit)
  (concatenate 'string
	       "("
	       (let ((cur-r (car lit))
		     (cur-x (cadr lit))
		     (cur-y (caddr lit)))
		 (concatenate 'string
			      (term-to-redlog cur-x) 
			      " "
			      (write-to-string cur-r)
			      " "
			      (term-to-redlog cur-y)))
	       ")"))
