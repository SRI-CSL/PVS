;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-
; NONLIN Modifications to arith.lisp to fix the loop
; problem in dealing with nonlinear arithmetic.

; In particular, the ordering ("arithord") associated with
; the loop-residue code must be respected by the
; implicit ordering imposed by the pr-union/pr-find code.

; For example, if the normal form of a polynomial is:
; (EQUAL X (TIMES X B)) then this is the way the equality
; would have to be presented to the union/find algorithm, namely
; pr-find(X) = (TIMES X B).

; But this would result in a loop:
; canon(TIMES X B) = TIMES(canon(TIMES X B), B) = ...

; My solution here is to split polynomial equalities (EQUAL X Y) into
; two inequalities (<= X Y) and (>= X Y).
; These inequalities would only be merged back into (EQUAL X Y)
; if doing so would not result in a loop.

; (defun arithord1(u v)
; ;    (cond ((and (consp u)(eq (car u) 'times)(qnumberp (cadr u)))
; ;	   (setq u (caddr u))))
; ;    (cond ((and (consp v)(eq (car v) 'times)(qnumberp (cadr v)))
; ;	   (setq v (caddr v))))
; ;    (when (and (eq *printerpdivide* 'no) (consp u) (eq (funsym u) 'divide))
; ;      (setq u (cons 'apply-2-number (cons '/_9 (cdr u)))))
; ;    (when (and (eq *printerpdivide* 'no) (consp v) (eq (funsym v) 'divide))
; ;      (setq v (cons 'apply-2-number (cons '/_9 (cdr v)))))
;     (cond
;      ((null u) nil)
;      ((null v) t)
;      ((symbolp u)
;       (cond
;        ((symbolp v)(alphalessp u v))
;        ((qnumberp v) nil)
;        (t t)))
;      ((symbolp v)(qnumberp u))
;      ((qnumberp u)
;       (cond
;        ((qnumberp v)(qlessp u v))
;        (t t)))
;      ((qnumberp v) nil)
;      ((equal (car u)(car v))(arithord1 (cdr u)(cdr v)))
;      (t (arithord1 (car u)(car v)))))

(defun arithsolve(lit)
  (case (funsym lit)				
    (equal (equalsolve lit))
    ((lesseqp greatereqp lessp greaterp) (ineqsolve lit))
    (t `((equal ,lit true)))))


(defun termsof (lit)
  (cons (arg1 lit)
	(if (and (consp (arg2 lit)) (eq (funsym (arg2 lit))
					`plus))
	    (argsof (arg2 lit))
	  (ncons (arg2 lit)))))

(defun equalsolve (lit)
  (let ((norm (normineq lit)))
    (if (and t (consp norm))
	(let* ((all-terms (termsof norm))
	       (head (car all-terms)))
	  (if (and (consp head)
		   (eq (funsym head) 'times))
	      (list `(lesseqp ,(arg1 norm) ,(arg2 norm))
		  `(greatereqp ,(arg1 norm) ,(arg2 norm)))
	    (if (onlyoccurrencep head head all-terms)
		(ncons norm)
	      (list `(lesseqp ,(arg1 norm) ,(arg2 norm))
		    `(greatereqp ,(arg1 norm) ,(arg2 norm))))))
      (ncons norm))))

(defun bad-eqn (eqn)
  (or (eq eqn 'ident)
      (and (consp eqn)
	   (or (subtermoflambda (arg1 eqn) (arg2 eqn))
	       (let* ((all-terms (termsof eqn))
		      (head (car all-terms)))
		 (if (not (and (consp head)
			       (eq (funsym head) 'times)))
		     (not (onlyoccurrencep head head all-terms))
		   nil))))))

(setq *jmr-mult* t)

(defvar *use-can* t)

(defun normineq2(lit)
  (prog(var coef dif fnsymbol)
       (setq fnsymbol (funsym lit))
       (setq dif (sigdifference `(difference ,(arg1 lit) ,(arg2 lit))))
       ;(break)
       (return
	(cond
	 ((qnumberp dif)(normconstdif fnsymbol dif)) ; constant dif
	 ((solvable dif)
	  (list
	   (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
	   var
	   ;;	(canon `(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif)))
	   ;;   fix to get around the "order bug"  18-Nov-85
	   (if *use-can*
	       (arithcan
	    ;`(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif))
	    ;; DAC fix to make sure var is cancelled in resulting expression.
		`(times ,(qnorm (qquotient -1 coef))
			,(sigplus `(plus ,(sigtimes `(times ,(qtimes -1 coef) ,var)) ,dif))))
	     (sigma
	    ;`(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif))
	    ;; DAC fix to make sure var is cancelled in resulting expression.
	      `(times ,(qnorm (qquotient -1 coef))
		      ,(sigplus `(plus ,(sigtimes `(times ,(qtimes -1 coef) ,var)) ,dif)))))
	   ))
	 ((eq *printerpmult* 'normal) lit)
	 ((eq *printerpmult* 'experimental)
	  (let ((*jmr-mult* t))
	    (if (solvable dif)
		(list
		 (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
		 var
		 (arithcan 
		  `(times ,(qnorm (qquotient -1 coef))
			  ,(sigplus `(plus ,(sigtimes `(times ,(qtimes -1 coef) ,var)) ,dif))))
		 )
	      lit)))
	 (t (prerr "Formula falls outside domain of completeness"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(1.15.94): Added WHEN clause in front of push of nrmineq.  This is needed
;;to prevent loops where increasingly large inequalities are introduced.
;;DAC(1/28/94): Added nrmineq-unchanged and pushed more often.

(defvar *tc-dbg* nil)

(defvar *tc-ehdm-test* nil)
;; Set to t in order to cause the proof:
;;  ~owre/ehdm6/rcp/minimial_v_prf_2--nf_v_sched to go through in ehdm.
;; Otherwise the proof is false.

; (defun transclosure(ineq)		;(break)
;   (cond ((member ineq *ineqstack* :test #'equal) nil)
; 	(t (let* ((*ineqstack* (cons ineq *ineqstack*))
; 		  (nrmineq (normineqatom (arithcan ineq)))
; 					;(nrmineq-true `(equal ,nrmineq true))
; 		  (nrmineq-unchanged
; 		   (or (not (consp nrmineq)) (equal nrmineq ineq))))
; 	     (if (eq nrmineq 'false) (retfalse))
; 	     (when nrmineq-unchanged
; 	       (setq nrmineq `(equal ,nrmineq true)))
; 	     ;;; 1/28/94: DAC added nrmineq-unchanged so that nrmineq
; 	     ;;; would be added to the begginning or end of ineqpot
; 	     ;;; correctly. (See notes by DAC and NSH below.)
; 	     (unless nrmineq-unchanged
; 	       (push nrmineq ineqpot))
; 	     ;;; 11/17/92: DAC Put nrmineq into the ineqpot after the
; 	     ;;; following loop that adds the transitive closure consequences.
; 	     ;;; This puts the nrmineq at the beginning of the ineqpot so that
; 	     ;;; process1 will process it first. This is important in the case
; 	     ;;; that the nrmineq is of the form (EQUAL blah TRUE).
; 	     ;;; IN this case we need the findalist to note this so that the
; 	     ;;; next time blah is seen it will be reduced to true and not
; 	     ;;; added to the ineqpot.

; 	     (when *tc-dbg* (break "before loop"))

; 	     (loop for chainineq in (chainineqs ineq) with (norm) do
; 		   (setq norm (normineq (residue ineq chainineq)))

; 		   (when *tc-dbg* (break "loop"))

; 		   (cond
; 		    ((eq norm 'true)
; 		     (add-disjunct-of-equals ineq chainineq))
; 		    ((eq norm 'false)(retfalse))
; 		    ((eq norm 'ident)
; 		     (let ((new-eqn
; 			    (if *tc-ehdm-test*
; 				`(equal ,(arg1 ineq) ,(arg2 ineq))
; 				(normineq `(equal ,(canonsig-arith (arg1 ineq))
; 						  ,(canonsig-arith (arg2 ineq)))))))
; 		       (unless (if *tc-ehdm-test*
; 				   (subtermof (arg1 ineq) (arg2 ineq))
; 				   (bad-eqn new-eqn))
; 			 (push new-eqn ineqpot)
; 			 (return nil))))
; 		    (t (push norm ineqpot))))
; 	     ;;; 11/17/92: DAC see note above about nrmineq.
; 	     ;;; NSH(1/15/94): Loops without WHEN clause below.  DAC's note
; 	     ;;; indicates why it might be needed.  

; 	     (when *tc-dbg* (break "after loop"))
	     
; 	     (when nrmineq-unchanged ;(and (consp nrmineq)(eq (car nrmineq) 'equal))
; 	       (push nrmineq ineqpot)
; 	       )
; 	     ))))

(defun eqnsolve(lit)
  (prog (res)
    (return
     (cond
      ((equal (arg1 lit)(arg2 lit)) (retfalse))
      ((eq (setq res (newcontext (process lit))) 'false) '(true))
      ((eq res 'true) (retfalse))
      (t (let ((norm (normineq lit)))
	   (cond ((eq norm 'ident) (retfalse))
		 ((eq norm true) (retfalse))
		 ((eq norm false) '(true))
		 (t `((nequal ,(arg1 norm) ,(arg2 norm)))))))))))

(defun strict?(ineq)
  (loop for u in (use (arg1 ineq))
        thereis (and (consp u)
		     (consp (car u))
		     (equal (arg1 ineq) (caar u))
		     (equal (arg2 ineq) (cdar u)))))

(defun make-strict (ineq)
  (case (funsym ineq)
    (equal (if (strict? ineq) false ineq))
    (lesseqp (if (strict? ineq)
		 `(lessp ,(arg1 ineq) ,(arg2 ineq))
	       ineq))
    (greatereqp (if (strict? ineq)
		    `(greaterp ,(arg1 ineq) ,(arg2 ineq))
		  ineq))
    (t ineq)))

(defun normineq1 (ineq)
  (let ((norm (normineq2 ineq)))
    (if (consp norm)
	(make-strict norm)
      norm)))

(defun arithcan (term)   ;weakened formof canon, added to fix "order bug"
  (catch 'found
    (cond
     ((symbolp term) term)
     ((integerp term) term)
     ((interp term)
      (let ((newterm
	     (sigma (cons (funsym term)
			  (loop for arg in (argsof term)
			       collect (arithcan arg) )))))
	(and (not (symbolp newterm))
	     (not (integerp newterm))
	     (interp newterm)
	     (progn
	      (loop for u in (use (arg1 newterm))
		   do (and (equal newterm (sig u)) (throw 'found 
							   newterm)) )
	      (loop for arg in (argsof newterm) do (adduse newterm arg)) ))
	newterm ))
     (t term) )))

(defvar *merge-dbg-i* nil)
(defvar *merge-dbg* nil)

(defun pr-merge(t1 t2)
  (prog(use2 vptr newsig t2-is-lambda)
    (setq t1 (pr-find t1)
	  t2 (pr-find t2)
	  ;use2 (copy-list (use t2))    ; nil added by JMR 6/9/85 to fix  (set this value later  MES 6/22/88)
	  )				; bug (append without nil is
					; surely useless)
    (when *merge-dbg-i* (break "before union"))
    (cond
     ((equal t1 t2) (return nil))
     ((and (consp t1) (consp (car t1)))    ;t1, t2 known to be unequal	
      (retfalse) ))
    (cond ((and (consp t1) (is-lambda-expr t1))  ; (SJ 5/13/86) added:
	   (setq t2 (prog1 t1 (setq t1 t2)))     ; swap to make lambda expr the find.
	   (setq t2-is-lambda t))
	  ((and (consp t2) (is-lambda-expr t2))  ; JMR 5/30/90 FIXME ??
	   (setq t2-is-lambda t)))	   
    (if (and t2-is-lambda		;DAC: 5/6/91 added check so to fix Butler's loop:
	     (atom t1)			; f = lambda x. f(x) so that it orients it:
	     (function-in t1 t2))	; lambda x. f(x) = f.
	(pr-union t2 t1)
      (pr-union t1 t2))			;t2 is now pr-find(t1)
    (loop for u in (use t1)
	 when (or (and (numberp t2) (not (zerop t2))) ; 8-28-91 dac: Don't replace part of
		  (not (eq (funsym u) 'quo))) do ; quo with a non-number.
      ; This check is necessary since the pr-find of t1 is 1 in some case. (is this ok?)
	 (setq newsig
	       (cons
		(funsym u)
		(loop for arg in (argsof (sig u)) collect
		     (cond ((equal arg t1) t2) (t arg)) )))
	 (when *merge-dbg* (break "newsig"))
	 (cond
	  ;; (SJ 5/13/86) added:
	  ;; checks if newsig will be a reducible applciation;
	  ;; if so, treat as an interpreted expression.
	  ((and t2-is-lambda
		(consp u)
		(is-skolemized-apply-expr u)
		(eq (apply-operator u) t1))
	   (setq newsig
		 (sigma newsig))
	   (setq s (append (solve `(equal ,(pr-find u) ,(canonsig-merge newsig))) s))
	   )
	  ((uninterp newsig)		; SO 9/28/90 was u - is now newsig

	   (when *merge-dbg* (break "uninterp1"))

	   (putsig newsig u)
	   (setq use2 (copy-list (use t2))) ; MES 6/22/88 (CANONSIG call in interpreted term case below can add to (USE T2))
	   (setq vptr (push u use2))
	   (loop while (cdr vptr) do
		  (cond
		   ((equal newsig (sig (cadr vptr)))
		    (or (equal u (cadr vptr))
			(let* ((pr-u (pr-find u))
			       (new-eqn `(equal ,pr-u
					  ,(pr-find (cadr vptr)))))
			  (when *merge-dbg* (break "uninterp2"))
			  (if (or (not (consp pr-u))
				  (uninterp pr-u)
				  (boolp (funsym pr-u)))
			      (setq s
				    (append
				     (solve new-eqn)
				     s ))
			      (setq s
				    (append
				     (append (solve new-eqn) (list new-eqn))
				     s)))))

					;(checkusealist "merge - before rplacd")
					;(break "count")

		    (rplacd vptr (cddr vptr))

					;(checkusealist "merge - after rplacd")
		    )
		   (t (pop vptr)) ))

	   (putuse use2 t2) )
	  ((boolp (funsym u))
	   (cond
	    ((equal (pr-find u) (pr-find 'true)) ;;NSH(5/92): asserts
						 ;;a=b if a<=b and a>=b.
						 ;;NSA's David Ritch
						 ;;detected this anomaly.
	     (when *merge-dbg* (break "boolp"))
	     (let* ((args (loop for arg in (argsof u) collect
					   (if (equal arg t1) t2 arg)))
		    (solvelist (solve
				(cons (funsym u) args))))
	       (cond ((and (memq (funsym u) '(greatereqp lesseqp))
			   (equal solvelist '(true)))
		      (if (equal
			   (catch 'context
			       (solve
				(cons (if (eq (funsym u) 'greatereqp)
					  'lesseqp
					  'greatereqp)
				      args)))
			     
			   '(true))
			  (setq s (append (solvecan `(equal ,@args)) s))
			  (setq s (append solvelist s))))
		     (t (setq s (append solvelist s))))))
	    ((or (isapplyupdate newsig) (isapplylambda newsig))
					; DAC 13-AUG-90: This case is necessary
					; since apply-bool of updates or lambdas
					; are both interpreted as bools
					; and as apply`s of updates and lambdas.
	     (setq newsig
		   (sigma newsig))
	     (push `(equal ,u ,(canonsig-merge newsig)) s))

	    ((equal (pr-find u) (pr-find 'false))
	     (print "msg from merge - this should not have occurred
		       according to Rob Shostak ") )))
	  ((find1 u))
	  (t				; interpreted term
	   (when *merge-dbg* (break "interp"))
	   ;(setq newsig
		; (sigma newsig))
	   ; 7-24-91: dac changed above sigma no longer needed due to change in canonsig.
	   ; bug manifested itself in needing recursive call for sigupdate.
	   (push `(equal ,u ,(canonsig-merge newsig)) s))) )))

(defun subtermoflambda (subterm term)
  (and (consp term)
       (if (eq (funsym term) 'lambda)
	   (subtermof subterm term)
	 (loop for a in term thereis (subtermoflambda subterm a)))))
