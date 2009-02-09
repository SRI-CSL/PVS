;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Case-splitting routines, and some simple case manipulation functions **
;;;
;;;
;;;  A simple case-splitting procedure for drilling down into the 
;;;   boolean structure of an RCF proof obligation.
;;;
;;;   Also contains some core prover procedures:
;;;     An arithmetic term simplifier,
;;;     A simple equality inconsistency checker,
;;;     A converter to HOL-Light REAL_SOS syntax,
;;;     A simple numerical demodulator.
;;;
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         22-July-2008,
;;;            last updated on  27-Oct-2008.
;;;

(in-package RAHD)

;;;
;;; WATERFALL-DISJ-TO-CNF: Convert a waterfall disjunction to an actual CNF 
;;;  formula suitable for calling (G ...).
;;;

(defun waterfall-disj-to-cnf (c)
  (let ((adj-c nil))
    (dolist (lit c)
      (let ((adj-lit
	     (if (equal (car lit) ':OR) 
		 (cdr lit)
	       (list lit))))
	(setq adj-c (cons adj-lit adj-c))))
    (reverse adj-c)))

;;; Before drill down, we need to fire away NOT's and soft inequalities (<=, =>).

(defun drill-down (o)
  (drill-down* o nil (length o)))

(defun drill-down* (o a n)
  (cond ((not (consp o)) (if (= n (length a)) (list a) nil))
	(t (let ((cur-clause (car o)))
	     (cond ((not (consp cur-clause))
		    (drill-down* (cdr o) a n))
		   (t (let ((cur-hyp (car cur-clause)))
			(append
			 (drill-down* (cdr o) (cons cur-hyp a) n)
			 (drill-down* (cons (cdr cur-clause) (cdr o)) a n)))))))))

(defun expand-formula (c)
  (cond ((endp c) nil)
	(t (let ((expanded-clause (expand-clause (car c) nil)))
	     (cons expanded-clause (expand-formula (cdr c)))))))

(defun expand-clause (cl cur-clause)
  (cond ((endp cl) cur-clause)
	(t (let ((cur-expanded-lit (expand-special-syms (car cl))))
	     (if (consp (car cur-expanded-lit))
		 (expand-clause (cdr cl) 
				(union cur-expanded-lit cur-clause))
	       (expand-clause (cdr cl)
			      (union (list cur-expanded-lit) cur-clause)))))))
      

; 19-Sept-2008 added TERM-TO-BIN-OPS here.
; 23-Nov-2008 added EXPAND-DIVS here.

(defun expand-special-syms (lit)
  (let ((cur-op (car lit)))
    (cond ((not (equal cur-op 'NOT))
	   (let ((cur-x (expand-divs (term-to-bin-ops (cadr lit))))
		 (cur-y (expand-divs (term-to-bin-ops (caddr lit))))) 
	     (case cur-op
		   (<= `((< ,cur-x ,cur-y) 
			 (= ,cur-x ,cur-y)))
		   (>= `((> ,cur-x ,cur-y)
			 (= ,cur-x ,cur-y)))
		   (otherwise `(,cur-op ,cur-x ,cur-y)))))
	  (t (let ((cur-atom (cadr lit)))
	       (let ((cur-r (car cur-atom))
		     (cur-x (expand-divs (term-to-bin-ops (cadr cur-atom))))
		     (cur-y (expand-divs (term-to-bin-ops (caddr cur-atom)))))
		 (case cur-r
		       (<= `(> ,cur-x ,cur-y))
		       (>= `(< ,cur-x ,cur-y))
		       (=  `((< ,cur-x ,cur-y) 
			     (> ,cur-x ,cur-y)))
		       (<  (expand-special-syms `(>= ,cur-x ,cur-y)))
		       (>  (expand-special-syms `(<= ,cur-x ,cur-y)))
		       (otherwise (break "Bad symbol in goal.")))))))))


(defun expand-divs (x) x)

;;;
;;; We want to eliminate cases from the search space that
;;; we can trivially see to be unsatisfiable.  We check for
;;; the following:
;;;
;;;   x = a  /\  x = b  where a != b,
;;;   x > x. 
;;;   
;;; Note that our check right now is not symmetric (but this is intentional -- 
;;;  SIMP-ZRHS takes this into account, as we want to rewrite from right
;;;  to left.)

(defun simply-incons (c eqs)
  (cond ((endp c) nil)
	(t (let ((cur-conjunct (car c)))
	     (let ((cur-relation (car cur-conjunct))
		   (cur-x (cadr cur-conjunct))
		   (cur-y (caddr cur-conjunct)))
	       (case cur-relation
		     (= (cond ((numberp cur-y)
			       (let ((match-eq (assoc cur-x eqs)))
				 (if (and match-eq
					  (let ((match-eq-v (cadr match-eq)))
					    (and (numberp match-eq-v)
						 (not (= cur-y match-eq-v)))))
				     `(:UNSAT (:UNSAT-WITNESS 
				       (= ,(car match-eq) ,(cadr match-eq))
				       (= ,cur-x ,cur-y)))
				   (simply-incons
				    (cdr c)
				    (cons `(,cur-x ,cur-y) eqs)))))
			      (t (simply-incons (cdr c) eqs))))
		     (> (cond ((equal cur-x cur-y)
			       `(:UNSAT (:UNSAT-WITNESS
				 (> ,cur-x ,cur-y))))
			      (t (simply-incons (cdr c) eqs))))
		     (< (cond ((equal cur-x cur-y)
			       `(:UNSAT (:UNSAT-WITNESS
				 (< ,cur-x ,cur-y))))
			      (t (simply-incons (cdr c) eqs))))
		     (otherwise (simply-incons (cdr c) eqs))))))))


(defun simply-incons* (c) 
  (let ((s (simply-incons c nil)))
    (if (equal s nil) c s)))

;;;
;;; Given a collection of cases, prune out those that SIMPLY-INCOS deems
;;; unsat.
;;;

(defun clear-simply-incons (cs keep cleared)
  (cond ((endp cs) `((:UNKNOWN ,keep) (:UNSAT ,cleared)))
	(t (let ((cur-case (car cs)))
	     (let ((is-incons? (simply-incons cur-case nil)))
	       (if is-incons?
		   (clear-simply-incons (cdr cs) keep (cons `(,cur-case ,is-incons?) cleared))
		 (clear-simply-incons (cdr cs) (cons cur-case keep) cleared)))))))

;;;
;;; Print a case to refute in HOL-Light REAL_SOS format.
;;;

(defun rcf-po-to-hl (po)
  (clsi-unknown-to-hl
   (clear-simply-incons (drill-down po) nil nil)))

(defun clsi-unknown-to-hl (clsi)
  (let ((cases-to-check (cadar clsi)))
    (print-cases cases-to-check)))

(defun print-cases (cs)
  (mapcar #'(lambda (c)
	      (concatenate 'string " 

" (print-case c) "

")) cs))
	 
(defun print-case (c)
  (cond ((endp c) "
   ==> &0 = &1")
	(t (concatenate 
	    'string 
	    (print-literal (car c))
	    (if (consp (cdr c)) "  /\\  " "  ")
	    (print-case (cdr c))))))

(defun print-literal (l)
  (let ((cur-r (car l))
	(cur-x (cadr l))
	(cur-y (caddr l)))
    (concatenate 
     'string 
     (print-obj cur-x) " "
     (write-to-string cur-r) " "
     (print-obj cur-y))))

(defun print-obj (o)
  (cond ((numberp o) (print-num o))
	((symbolp o) (write-to-string o))
	((consp o) (print-poly o))
	(t (write-to-string o))))

(defun print-poly (p)
  (cond ((equal p nil) "")
	((not (consp p)) (print-obj p))
	(t (concatenate 'string "("
			(print-poly (cadr p)) " "
			(print-obj (car p)) 
			(if (equal (car p) '-) " (" " ")
			(print-poly (caddr p))
			(if (equal (car p) '-) "))" ")"))))) 


(defun print-num (n)
  (let ((num (numerator n))
	(den (denominator n)))
    (if (= den 1)
	(concatenate 'string "&" (write-to-string num))
      (concatenate 'string 
		   "(&" (write-to-string num) " / &" (write-to-string den) ")"))))
		   
;;;
;;; Count equalities in a conjunction.
;;;

(defun count-eqs (c)
  (cond ((endp c) 0)
	(t (let ((cur-r (caar c)))
	     (if (equal cur-r '=)
		 (1+ (count-eqs (cdr c)))
	       (count-eqs (cdr c)))))))

(defun filter-by-num-eqs (cs dim)
  (cond ((endp cs) nil)
	(t (let ((cur-c (car cs)))
	     (if (= dim (count-eqs cur-c))
		 (cons cur-c (filter-by-num-eqs (cdr cs) dim))
	       (filter-by-num-eqs (cdr cs) dim))))))

;;;
;;; Given a conjunction, its extracted equalities, and a predicate
;;; that maps {(= LHS RHS) | LHS, RHS are terms} to 2, return the result of
;;; applying (LHS -> RHS) to every literal s.t. (= LHS RHS) satisfies
;;; the predicate passed.
;;;
;;; For example, to only apply equalities to terms that will rewrite
;;; variables with numerals, do:
;;;
;;;  (subst-eqs c eqs #'(lambda (eq) (numberp (caddr eq)))).
;;;

(defun subst-eqs (c eqs pred)
  (cond ((endp eqs) (remove-duplicates c :test 'equal))
	(t (let ((cur-eq (car eqs)))
	     (if (funcall pred cur-eq)
		 (subst-eqs 
		  (subst (caddr cur-eq) (cadr cur-eq) c)
		  (cdr eqs)
		  pred)
	       (subst-eqs c (cdr eqs) pred))))))

;;;
;;; Given a conjunction, return the result of rewriting the conjunction with
;;; each equality the conjunction contains that has a number as its RHS.
;;;

(defun demodulate-numerically (c)
  (subst-eqs c (gather-eqs c) 
	     #'(lambda (eq) (and (symbolp (cadr eq)) (numberp (caddr eq))))))

;;;
;;; Is a literal ground?
;;;

(defun lit-ground? (l)
  (and (term-ground? (cadr l))
       (term-ground? (caddr l))))

(defun term-ground? (term)
  (cond 
   ((equal term nil) t)
   ((numberp term) t)
   ((symbolp term)
    (if (member term '(+ - * / EXACT-REAL-EXPT)) t nil))
   (t (and (term-ground? (car term))
	   (term-ground? (cdr term))))))
	
;;;
;;; Simplify ground literals in a conjunction.
;;; Note: We also simplify ground terms in non-ground literals.
;;;

(defun simplify-ground-lits (c)
  (mapcar #'(lambda (l)
	      (if (lit-ground? l)
		  (eval l)
		(let ((cur-op (car l))
		      (cur-x  (cadr l))
		      (cur-y  (caddr l)))
		  `(,cur-op
		    ,(if (term-ground? cur-x)
			 (eval cur-x)
		       cur-x)
		    ,(if (term-ground? cur-y)
			 (eval cur-y)
		       cur-y)))))		
	  c))

;;;
;;; Remove `t's and `nils' from a conjunction.
;;; If c contains `nil' then we return `(:UNSAT (:UNSAT-WITNESS nil)).
;;;

(defun remove-truth-vals (c result)
  (cond ((endp c) result)
	(t (let ((cur-l (car c)))
	     (cond ((equal cur-l nil) '(:UNSAT (:UNSAT-WITNESS nil)))
		   ((equal cur-l t)
		    (remove-truth-vals (cdr c) result))
		   (t (remove-truth-vals (cdr c)
					 (cons cur-l result))))))))

(defun remove-truth-vals* (c)
  (let ((result (remove-truth-vals c nil)))
    (cond ((and (consp result) (equal (car result) ':UNSAT))
	   result)
	  ((equal result nil)

	   ;;; This means the case has been reduced to an implicit T, 
	   ;;; and thus the installed goal is SAT.

	   '(:SAT :CASE-REDUCED-TO-EMPTY-CONJUNCTION))
	  (t (reverse result)))))

;;;
;;; A little term simplifier.  We do these main things:
;;;
;;;  (1) reduce (- x x) to 0,
;;;  (2) reduce (+ x (- x)) to 0,
;;;  (4) reduce (* x 0) to 0,
;;;  (5) reduce (+ x 0) to x,
;;;  (6) reduce (- x 0) to x,
;;;  (7) reduce (* x 1) to x.
;;;
;;; Note that all terms must be forced into binary function notation via
;;; TERM-TO-BIN-OPS before calls to this term simplifier.
;;;

(defun simplify-term (term last-val)
  (cond ((numberp term) term)
	((symbolp term) term)
	((equal term nil) nil)
	((equal term last-val) term)
	(t (let ((cur-op (car term))
		 (x (cadr term))
		 (y (caddr term)))
	     (case cur-op
		   (* (cond ((or (equal x 0) (equal y 0)) 0)
			    ((equal x 1) y)
			    ((equal y 1) x)
			    ((and (equal x y)
				  (consp x)
				  (equal (car x) '-) 
				  (equal (cadr x) 0)) (simplify-term `(* ,(caddr x) ,(caddr x)) nil)) ; (* (- 0 a) (- 0 a)) --> (* a a).
			    (t (simplify-term `(* ,(simplify-term x nil)
						  ,(simplify-term y nil))
					      term))))
		   (+ (cond ((equal x 0) y)
			    ((equal y 0) x)
			    ((equal x `(- 0 ,y)) 0)
			    ((equal y `(- 0 ,x)) 0)
			    (t (simplify-term `(+ ,(simplify-term x nil)
						  ,(simplify-term y nil))
					      term))))
		   (- (cond ((equal y 0) x)
			    ((equal x y) 0)
			    (t (simplify-term `(- ,(simplify-term x nil)
						  ,(simplify-term y nil))
					      term))))
		   (otherwise
		    (simplify-term `(,cur-op ,(simplify-term (cadr term) nil)
					     ,(simplify-term (caddr term) nil))
				   term)))))))

;;;
;;; Expand term to binary ops (e.g. make (* x y z) (* x (* y z))).
;;;

(defun term-to-bin-ops (tm)
  (cond ((equal tm nil) nil)
	((numberp tm) tm)
	((symbolp tm) tm)
	((= (length tm) 3) `(,(car tm) ,(term-to-bin-ops (cadr tm))
			     ,(term-to-bin-ops (caddr tm))))
	((= (length tm) 2) (break "Invalid term: Cannot make ops binary."))
	(t (case (car tm)
		 (* (op-to-bin-op tm '*))
		 (+ (op-to-bin-op tm '+))
		 (otherwise (break "Invalid term: Cannot make ops binary."))))))

(defun op-to-bin-op (tm op)
  (cond ((equal tm nil) nil)
	((numberp tm) tm)
	((symbolp tm) tm)
	(t `(,op ,(term-to-bin-ops (cadr tm))
		 ,(term-to-bin-ops (cons op (cddr tm)))))))

;;;
;;; Simplify a literal.
;;;

(defun arith-simplify-lit (lit)
  `(,(car lit) 
    ,(simplify-term (term-to-bin-ops (cadr lit)) nil) 
    ,(simplify-term (term-to-bin-ops (caddr lit)) nil)))

;;;
;;; Simplify a conjunction (case) arithmetically.
;;;

(defun arith-simplify-case (c)
  (mapcar #'arith-simplify-lit c))
    



#|
;;;
;;; Problematic terms:
;;;

(defparameter *tm*
  '(+ (* (- 1 (* 1 1 1 1)) (- 1 (* 1 d)) (- (* 1 d) (* 1 1))
          (- (* 1 d) (* 1 1)))
       (* (* 2 1 1) (- (* 1 d) (* 1 1)) (- 1 (* 1 1)) (- 1 d) (- 1 d))
       (* (- (* 1 1 1 1) (* 1 1 d d)) (- 1 (* 1 d)) (- 1 1) (- 1 1))))

(defparameter *tm2*
  '(+ (* (- 1 (* 0 0 b b)) (- 1 (* 0 1)) (- (* 0 1) (* b 0))
          (- (* 0 1) (* b 0)))
       (* (* 2 0 b) (- (* 0 1) (* 0 b)) (- 1 (* 0 b)) (- 0 1) (- 0 1))
       (* (- (* 0 0 b b) (* 0 0 1 1)) (- 1 (* 0 1)) (- 0 b) (- 0 b))))
#|
;;;
;;; Case study problem I
;;;

;time REAL_SOS
; `&0 <= a /\ a <= &1 /\ &0 <= b /\ b <= &1 /\
;  &0 <= c /\ c <= &1 /\ &0 <= d /\ d <= &1
;   ==> (&1 - a pow 2 * b pow 2) * (&1 - c * d) * (a * d - b * c) pow 2 +
;       &2 * a * b * (c * d - a * b) * (&1 - a * b) * (c - d) pow 2 +
;       (a pow 2 * b pow 2 - c pow 2 * d pow 2) *
;       (&1 - c * d) * (a - b) pow 2 >= &0`;;


(defparameter *hyps*
  '( ((> a 0) (= a 0))
     ((> b 0) (= b 0))
     ((> c 0) (= c 0))
     ((> d 0) (= d 0))
     ((< a 1) (= a 1))
     ((< b 1) (= b 1))
     ((< c 1) (= c 1))
     ((< d 1) (= d 1))))

(defparameter *negated-goal*
  '(((> a 0) (= a 0))
    ((> b 0) (= b 0))
    ((> c 0) (= c 0))
    ((> d 0) (= d 0))
    ((< a 1) (= a 1))
    ((< b 1) (= b 1))
    ((< c 1) (= c 1))
    ((< d 1) (= d 1))
    ((< (+ (* (- 1 (* a a b b)) (- 1 (* c d)) (- (* a d) (* b c)) (- (* a d) (* b c)))
	   (* (* 2 a b) (- (* c d) (* a b)) (- 1 (* a b)) (- c d) (- c d))
	   (* (- (* a a b b) (* c c d d)) (- 1 (* c d)) (- a b) (- a b))) 
	0))))
     




;;;
;;; The cases not ruled out by simple equality incons elimination:
;;;

|#
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (= c 0) (= b 0) (= a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (= b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (= b 0) (= a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (= d 0) (> c 0) (= b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (> c 0) (= b 0) (= a 0))
((= d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (= a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (= a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (= a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (= d 0) (= c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (= c 0) (> b 0) (= a 0))
((= d 1) (< c 1) (= b 1) (< a 1) (> d 0) (= c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (> d 0) (= c 0) (> b 0) (= a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (> b 0) (= a 0))
((< d 1) (= c 1) (= b 1) (< a 1) (= d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (= d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (= d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (> c 0) (> b 0) (= a 0))
((= d 1) (= c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (= c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((= d 1) (< c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((= d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (= a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (= d 0) (= c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (= c 0) (= b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (= a 1) (> d 0) (= c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (> d 0) (= c 0) (= b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (= b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (= a 1) (= d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (= d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (= d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (> c 0) (= b 0) (> a 0))
((= d 1) (= c 1) (< b 1) (= a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (= a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (= a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((= d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (= b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (= a 1) (= d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (= d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (= d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (= c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (= b 1) (= a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (= a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (= a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (= b 1) (< a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (= c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (= b 1) (= a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (= a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (= a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (= b 1) (< a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (= d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (= c 1) (= b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (= b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (= b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (= c 1) (< b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (= a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (= c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (= b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (= c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((= d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))
((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0))

;;; 81 above to check.

;;; How many are 0-ary (e.g. ground) problems after substitution?

(length (filter-by-num-eqs (cadar (clear-simply-incons (drill-down *hyps*) nil nil)) 4))
16

;;; How many are univariate problems after substitution?

(length (filter-by-num-eqs (cadar (clear-simply-incons (drill-down *hyps*) nil nil)) 3))
32

;;; How many are bivariate?

(length (filter-by-num-eqs (cadar (clear-simply-incons (drill-down *hyps*) nil nil)) 2))
24

;;; How many are trivariate?

(length (filter-by-num-eqs (cadar (clear-simply-incons (drill-down *hyps*) nil nil)) 1))
8

;;; How many contain no equalities?
;;; Only one of course, our open relaxation!

(filter-by-num-eqs (cadar (clear-simply-incons (drill-down *hyps*) nil nil)) 0)
(((< d 1) (< c 1) (< b 1) (< a 1) (> d 0) (> c 0) (> b 0) (> a 0)))


(defun expand-soft-ineqs (o)
  (cond ((endp o) nil)
	(t (let ((cur-lit (car o)))
	     (let ((cur-x (cadr cur-lit))
		   (cur-y (caddr cur-lit)))
	       (let ((modified-lit
		      (case (car cur-lit)
			    ('<=  `((< ,cur-x ,cur-y)
				    (= ,cur-x ,cur-y)))
			    ('>=  `((> ,cur-x ,cur-y)
				    (= ,cur-x ,cur-y)))
			    (otherwise cur-lit))))
		 (cons modified-lit (expand-soft-ineqs (cdr o)))))))))
				  
  



|#
