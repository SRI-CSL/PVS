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
;;;   v0.0,
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
;;;            last updated on  21-Oct-2008.
;;;

(in-package RAHD)

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

(defun cad (c generic)
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
  (let ((open-frag (gather-ineqs c)))
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

(defparameter all-vars nil)

(defun all-vars-in-conj (c)
  (progn
    (setq all-vars nil)
    (dolist (lit c)
      (setq all-vars 
	    (union all-vars
		   (union 
		    (gather-vars (cadr lit))
		    (gather-vars (caddr lit))))))
    all-vars))
    
(defun conj-to-qcb (c result)
  (cond ((endp c) result)
	(t (let ((cur-lit (car c)))
	     (conj-to-qcb
	      (cdr c)
	      (concatenate 'string
			   result
			   (lit-to-qcb cur-lit)
			   (if (consp (cdr c)) " /\\ " "")))))))

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
       (not (equal term '<))))
