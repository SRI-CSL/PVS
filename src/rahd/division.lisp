;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; Goal pre-processing and side-condition generation / checking for goals
;;;  that involve division.
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         22-Nov-2008,
;;;            last updated on  19-Jan-2009.
;;;

(in-package RAHD)

;;;
;;; F-TO-DIV-FREE-CNF: 
;;;

(defun f-to-div-free-cnf (f)
  (ef-to-cnf (f-to-div-free-explicit-fof f)))

;;;
;;; F-TO-DIV-FREE-EXPLICIT-FOF: Given a normal RAHD CNF f that may contain
;;; division, return the result of eliminating division and making all
;;; logical connectives in the resulting formula explicit.
;;;

(defun f-to-div-free-explicit-fof (f)
  (make-cnf-explicit 
   (loop for c in f collect (mapcar #'lit-to-div-free-form c))))

;;;
;;; LIT-TO-DIV-FREE-FORM: Given a literal, return a division free
;;;  form.  This must then be further processed to turn the whole
;;;  formula back into CNF.
;;;
;;;  Note: We're assuming that denominators have been checked to be non-zero.
;;;         ** We're also assuming (NOT's ..) have been removed.
;;;

(defun lit-to-div-free-form (l)
  (let* ((op (car l))
	 (x  (cadr l))
	 (y  (caddr l))
	 (x* (one-div x))
	 (y* (one-div y))
	 (dx (div-p x*))
	 (dy (div-p y*)))
    (if (and (not dx) (not dy)) 

	;;
	;; If our literal does not contain division, we just return it unchanged.
	;;

	l

      ;;
      ;; Our literal contains division.  So, let's get rid of it by cross-multiplying.
      ;;

      (let ((new-LHS) (new-RHS) (new-MULTIPLICAND))

	(cond ((and dx dy)

	       ;;
	       ;; l is of the form `(op (/ p q) (/ s t))
	       ;; We reduce LHS to (* p t), RHS to (* s q).
	       ;;

	       (setq new-LHS `(* ,(car dx) ,(cdr dy)))
	       (setq new-RHS `(* ,(car dy) ,(cdr dx)))
	       (setq new-MULTIPLICAND (if (and (numberp (cdr dx))
					       (numberp (cdr dy)))
					  (* (cdr dx) (cdr dy))
					`(* ,(cdr dx) ,(cdr dy)))))

	      (dx

	       ;;
	       ;; l is of the form `(op (/ p q) s) s.t. s division-free
	       ;;

	       (setq new-LHS (car dx))
	       (setq new-RHS `(* ,y* ,(cdr dx)))
	       (setq new-MULTIPLICAND (cdr dx)))

	      (dy

	       ;;
	       ;; l is of the form `(op s (/ p q)) s.t. s division-free
	       ;;
	       
	       (setq new-LHS `(* ,x* ,(cdr dy)))
	       (setq new-RHS (car dy))
	       (setq new-MULTIPLICAND (cdr dy))))

	;;
	;; Now that division has been removed, we must work out the proper
	;; constraints based upon the sign-conditions of our denominators.
	;; These will result in formulas that were once in CNF having explicit 
	;; :ANDs and :ORs within clauses.  These connectives must be processed away
	;; and the formulas converted back to CNF before (BUILD-GS ...) is called.
	;;
	;; Note: As of 19-Jan-2009, we now notice when NEW-MULTIPLICAND is a rational
	;; constant, and then will skip the disjunctive (<,>) split based upon 
	;; sign(NEW-MULTIPLICAND).  This will vastly reduce the search space for many
	;; naturally encountered division problems.
	;;
	
	(if (numberp new-MULTIPLICAND)
	    (let ((s (calc-sign new-MULTIPLICAND)))
	      (case op
		(= `(= ,new-LHS ,new-RHS))
		(> (case s
		     (-1 `(< ,new-LHS ,new-RHS))
		     (1  `(> ,new-LHS ,new-RHS))
		     (0 (break "Division by zero not allowed in input formulas."))))
		(< (case s
		     (-1 `(> ,new-LHS ,new-RHS))
		     (1  `(< ,new-LHS ,new-RHS))
		     (0 (break "Division by zero not allowed in input formulas."))))
		(<= (case s
		      (-1 `(>= ,new-LHS ,new-RHS))
		      (1  `(<= ,new-LHS ,new-RHS))
		      (0 (break "Division by zero not allowed in input formulas."))))
		(>= (case s
		      (-1 `(<= ,new-LHS ,new-RHS))
		      (1  `(>= ,new-LHS ,new-RHS))
		      (0 (break "Division by zero not allowed in input formulas."))))))
	  (case op
	    (= `(= ,new-LHS ,new-RHS))
	    (> `(:OR (:AND (> ,new-MULTIPLICAND 0)
			   (> ,new-LHS ,new-RHS))
		     (:AND (< ,new-MULTIPLICAND 0)
			   (< ,new-LHS ,new-RHS))))
	    (< `(:OR (:AND (> ,new-MULTIPLICAND 0)
			   (< ,new-LHS ,new-RHS))
		     (:AND (< ,new-MULTIPLICAND 0)
			   (> ,new-LHS ,new-RHS))))
	    (<= `(:OR (:AND (>= ,new-MULTIPLICAND 0)
			    (<= ,new-LHS ,new-RHS))
		      (:AND (<= ,new-MULTIPLICAND 0)
			    (>= ,new-LHS ,new-RHS))))
	    (>= `(:OR (:AND (>= ,new-MULTIPLICAND 0)
			    (>= ,new-LHS ,new-RHS))
		      (:AND (<= ,new-MULTIPLICAND 0)
			    (<= ,new-LHS ,new-RHS))))))))))


;;;
;;; ONE-DIV tm: Given a term, return an equal term that only has
;;;  a single top-level division symbol.
;;;

(defun one-div (tm)
  (purge-divs 
   (bubble-divs-up-bu tm)))

;;;
;;; PURGE-DIVS: Purge divisions from a term tm.
;;; Note: We assume tm is a fixed point of BUBBLE-DIVS-UP.
;;;

(defun purge-divs (tm)
  (FIXED-POINT #'purge-divs* tm))

(defun purge-divs* (tm)
  (cond ((not (consp tm)) tm)
	(t (let ((updated-tm tm))
	     (setq updated-tm
		   (or (rule-D tm)
		       (rule-E tm)
		       (rule-F tm)
		       (rule-A-l tm)
		       (rule-A-r tm)
		       tm))
	     `(,(car updated-tm)
	       ,(purge-divs* (cadr updated-tm))
	       ,(purge-divs* (caddr updated-tm)))))))

;;;
;;; BUBBLE-DIVS-UP: Bubble divisions in a term up to the top.
;;;

(defun bubble-divs-up-bu (tm)
  (FIXED-POINT #'bubble-divs-up-bu* tm))

(defun bubble-divs-up-td (tm)
  (FIXED-POINT #'bubble-divs-up* tm))

(defun bubble-divs-up* (tm)
  (cond ((not (consp tm)) tm)
	(t (let ((op (car tm))
		 (updated-tm tm))
	     (case op
	       (* (setq updated-tm 
			(or (rule-A-l tm)
			    (rule-A-r tm)
			    tm)))
	       ((+ -) (setq updated-tm 
			    (or (rule-B-l tm)
				(rule-B-r tm)
				tm))))
	     `(,(car updated-tm)
	       ,(bubble-divs-up* (cadr updated-tm))
	       ,(bubble-divs-up* (caddr updated-tm)))))))


(defun bubble-divs-up-bu* (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (rec-result 
		  `(,op
		    ,(bubble-divs-up-bu* (cadr tm))
		    ,(bubble-divs-up-bu* (caddr tm))))
		  (rr-op (car rec-result)))
	     (case rr-op
	       (/ rec-result)
	       (* (or (rule-A-l rec-result)
		      (rule-A-r rec-result)
		      rec-result))
	       ((+ -) (or (rule-B-l rec-result)
			  (rule-B-r rec-result)
			  rec-result))
	       (otherwise (break "Bad symbol")))))))

;;;
;;; Paul Jackson's ``bubble-up'' rules (A,B,C).
;;; 

;;;
;;;      b         a*b
;;; a * ---  ~~>   ---
;;;      c          c
;;;

(defun rule-A-l (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p y)))
	     (if (and (equal op '*)
		      d)
		 `(/ (* ,x ,(car d)) ,(cdr d))
	       nil)))))


;;;
;;;  b             a*b
;;; --- * a  ~~>   ---
;;;  c              c
;;;

(defun rule-A-r (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p x)))
	     (if (and (equal op '*)
		      d)
		 `(/ (* ,y ,(car d)) ,(cdr d))
	       nil)))))

(defun rule-B-l (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p y)))
	     (if (and (member op '(+ -))
		      d)
		 `(/ (,op (* ,x ,(cdr d)) ,(car d)) ,(cdr d))
	       nil)))))

(defun rule-B-r (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p x)))
	     (if (and (member op '(+ -))
		      d)
		 `(/ (,op ,(car d) (* ,y ,(cdr d))) ,(cdr d))
	       nil)))))

;;;
;;; Paul Jackson's ``multiplication pollution'' rules D, E, F.
;;;

(defun rule-D (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p x)))
	     (if (and (equal op '/)
		      d)
		 `(/ ,(car d) (* ,(cdr d) ,y))
	       nil)))))

(defun rule-E (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (d  (div-p y)))
	     (if (and (equal op '/)
		      d)
		 `(/ (* ,x ,(cdr d)) ,(car d))
	       nil)))))

(defun rule-F (tm)
  (cond ((not (consp tm)) tm)
	(t (let* ((op (car tm))
		  (x  (cadr tm))
		  (y  (caddr tm))
		  (dx (div-p x))
		  (dy (div-p y)))
	     (if (and (equal op '/)
		      dx dy)
		 `(/ (* ,(car dx) ,(car dy)) (* ,(cdr dx) ,(cdr dy)))
	       nil)))))

;;;
;;; DIV-P: Is tm's top-level function division?  If so,
;;;  return (a . b) s.t. tm = (/ a b).
;;;  Otherwise, return nil.
;;;

(defun div-p (tm)
  (when (and (consp tm)
	     (eql (car tm) '/))
    (cons (cadr tm)
	  (caddr tm))))

;;;
;;; TEST-DIVS: A simple function to test our translation for polynomials
;;;  in less than 5 variables (name them a,b,c,d).
;;;

(defun test-divs (tm val-lst)
  (let ((od-tm (one-div tm))
	(out t))
    (dolist (v val-lst)
      (let ((c (eval `((lambda (a b c d) (= ,tm ,od-tm))
		       ,v ,(1+ v) ,(+ v 2) ,(+ v 3)))))
	(format t "~% success? ~A" c)
	(setq out (and c out))))
    out))

		
	 
		       
;;;
;;; *** OLD: This is kept only for historical purposes and is no longer true.
;;;
;;; Essay on how we handle division in RAHD:
;;;
;;; Let G be an existentially quantified goal given in CNF.  If some literal L
;;; in some clause C in G contains the division symbol, we must reduce G to
;;; some G' s.t. G' does not contain the division symbol and G' ==> G.
;;; 
;;; In doing so, we need to be careful about 
;;;   (i) swapping directions of inequality symbols when we multiply through by
;;;       negative definite polynomials (including negative rational constants),
;;;  (ii) checking side-conditions to guarantee that under the assumption of the 
;;;       other conjuncts in G, the polynomials appearing as denominators in G
;;;       are always non-zero.
;;;
;;;  Let p(\vec{x}) in Q[*vars-table*] be some polynomial appearing as a 
;;;   denominator in L in G, with L1 ... Ln division-free.  
;;;
;;;  WLOG, Let G be equivalent to (exists vec{x} (L1 /\ ... /\ Ln /\ L)),
;;;   where L is (op (/ p(\vec{x}) q(\vec{x})) r(\vec{x})) and op in {>,=}.
;;;  
;;;  [Case I]: If q(\vec{x}) is a rational constant, then we can multiply
;;;            through by q(\vec{x}) and adjust op as needed in the following
;;;            way (writing `q' for q(\vec{x}) in Q):
;;;
;;;                 If (q = 0), then we report an error to the user.
;;;
;;;                 If (op = `=') and q != 0, then 
;;;                  let L' := (= p(\vec{x}) (* 1/q r(\vec{x}))),
;;;                 
;;;                 If (op = `>') and q > 0, then
;;;                  let L' := (> p(\vec{x}) (* 1/q r(\vec{x}))),
;;;
;;;                 If (op = `>') and q < 0, then
;;;                  let L' := (< p(\vec{x}) (* 1/q r(\vec{x}))).
;;;
;;;  [Case II]: If q(\vec{x}) is a non-constant polynomial, then we have more work
;;;             to do.  We must first establish that the universal-duals of L1, ..., Ln
;;;             (w. univ-dual(Li) written as Li') imply that q(\vec{x}) is always non-zero: 
;;;
;;;             (a) (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) != 0))).
;;;
;;;                  By the Intermediate Value Theorem coupled with the fact that
;;;                  q(\vec{x}) is a continuous function from R^n -> R, 
;;;                  condition (a) is equivalent to:
;;;
;;;                    (a') (OR (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) < 0)))
;;;                             (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) > 0))).
;;;
;;;                  Perhaps surprisingly, we must in general explicitly prove one of these two 
;;;                  disjuncts in (a') (e.g. just working with the (a) form is not sufficient) 
;;;                  for the following reason: 
;;;
;;;                   (*) If op in L is an inequality symbol, say `<', if we wish to eliminate
;;;                       the division sign by multiplying through by q(\vec{x}), we must know 
;;;                       to swap `<' to `>' if
;;;
;;;                          (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) < 0)) holds.
;;;
;;;              So, we do the following:
;;;
;;;               (i) Ask RAHD to prove (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) > 0)), in
;;;                   RAHD's existential form:
;;;                   
;;;                     ?RAHD |= (L1 /\ ... /\ Ln /\ q(vec{x}) <= 0) ==> (0=1).
;;;
;;;                   If this fails (either by RAHD not being clever enough or by it being false), we
;;;                   then:
;;;
;;;              (ii) Ask RAHD to prove (forall \vec{x} (L1' /\ ... /\ Ln' ==> (q(\vec{x}) < 0)), in
;;;                   RAHD's existential form:
;;;
;;;                     ?RAHD |= (L1 /\ ... /\ Ln /\ q(vec{x}) >= 0) ==> (0=1).
;;;
;;;                   If this fails, then we report to the user that we were unable to establish 
;;;                   sign conditions upon q(\vec{x}) that would allow us to soundly reduce G to a 
;;;                   purely RCF problem (e.g., division-free).  Thus, RAHD cannot soundly proceed on
;;;                   this goal (unless the user verifies these conditions manually; we should think
;;;                   on ways to ease the use of manually verified division conditions in proof efforts
;;;                   with top-level divisions).
;;;
;;;              If either (i) or (ii) succeeds, then we can treat q(\vec{x}) abstractly as a rational
;;;              constant (either one that is positive or negative, based upon whether (i) or (ii)
;;;              succeeded, respectively) and then revert to [Case I] above.
;;;
;;;  Last updated: Nov 22, 2008.
;;;                     
