;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-
; ARITH - sigma and solver for linear arith

;; HISTORY 
;; 17-Mar-1994		Natarajan Shankar	
;;    Misc

;;; SO 11/26/90 - now driven off emacs variable *printerpdivide*
;(defvar *uninterp-divide* t)

(defun arithord (u v)
  (let ((up (if (and (consp u) (eq (car u) 'times) (qnumberp (cadr u)))
		(caddr u)
	      u))
	(vp (if (and (consp v) (eq (car v) 'times) (qnumberp (cadr v)))
		(caddr v)
	      v)))
;    (let ((a1 (arithord1 up vp))
;	  (a2 (arithord1 vp up)))
;      (when (and (equal a1 a2) (not (equal up vp))) (break))
;      a1)
    (arithord1 up vp)))


(defun arithord1(u v)
;    (cond ((and (consp u)(eq (car u) 'times)(qnumberp (cadr u)))
;	   (setq u (caddr u))))
;    (cond ((and (consp v)(eq (car v) 'times)(qnumberp (cadr v)))
;	   (setq v (caddr v))))
;    (when (and (eq *printerpdivide* 'no) (consp u) (eq (funsym u) 'divide))
;      (setq u (cons 'apply-2-number (cons '/_9 (cdr u)))))
;    (when (and (eq *printerpdivide* 'no) (consp v) (eq (funsym v) 'divide))
;      (setq v (cons 'apply-2-number (cons '/_9 (cdr v)))))
    (cond
     ((null u) nil)
     ((null v) t)
     ((symbolp u)
      (cond
       ((symbolp v)(alphalessp u v))
       ((qnumberp v) nil)
       (t t)))
     ((symbolp v)(qnumberp u))
     ((qnumberp u)
      (cond
       ((qnumberp v)(qlessp u v))
       (t t)))
     ((qnumberp v) nil)
     ((equal (car u)(car v))(arithord1 (cdr u)(cdr v)))
     (t (arithord1 (car u)(car v)))))


(defun sigtimes(u)
  (catch 'fastexit
    (prog (product coef)
      (setq coef 1)
      (mapc #'buildproduct (cdr u))
      (setq coef (qnorm coef))
      (cond
       (product
	(setq product
	      (cond
	       ((cdr product) (cons 'times (sort product 'arithord)))
	       (t (car product))))
	(cond
	 ((equal coef 1) (return product))
	 (t (return `(times ,coef ,product)))))
       (t (return coef))))))

(defun buildproduct(arg)
  (cond
   ((qnumberp arg)
    (and (qzerop arg) (throw 'fastexit 0))
    (setq coef (qtimes arg coef)))
   ((symbolp arg)
    (if (and (eq *printerpdivide* 'yes) (member (make-inverse-1 arg) product :test #'equal))
	(setq product (remove (make-inverse-1 arg) product :test #'equal :count 1))
      (setq product (nconc product (ncons arg)))))
   ((and (eq *printerpdivide* 'yes) (eq (funsym arg) 'divide))
    (if (member (arg2 arg) product :test #'equal)
	(setq product (remove (arg2 arg) product :test #'equal :count 1))
      (setq product (nconc product (ncons arg)))))
   ((eq (car arg) 'times) (loop for u in (cdr arg) do (buildproduct u)))
   ((eq (car arg) 'plus)
    (throw 'fastexit (distrib u)))
   (t (if (and (eq *printerpdivide* 'yes) (member (make-inverse-1 arg) product :test #'equal))
	  (setq product (remove (make-inverse-1 arg) product :test #'equal :count 1))
	(setq product (nconc product (ncons arg)))))))

(defun distrib(u)
  (sigplus
   (cons 
    'plus
    (loop for arg in (distrib1 (cdr u) (ncons nil)) collect 
	 (sigtimes (cons 'times arg))))))

(defun distrib1(product sum)
     (cond
      ((null product) sum)
      ((and (consp (car product))(eq (caar product) 'plus))
       (setq sum (distrib1 (cdr product) sum))
       (loop for arg in (cdar product) nconc (distrib2 arg sum)))
     (t (distrib2 (car product) (distrib1 (cdr product) sum)))))

(defun distrib2(term product)
  (loop for prodarg in product collect (cons term prodarg)))

(defun sigdivide (u)
  (if (eq *printerpdivide* 'no) u
    (let* ((coef 1)
	   (product nil)
	   (numer1 (arg1 u))
	   (denom1 (arg2 u))
	   (numer 1)
	   (denom 1))
      (if (and (consp numer1) (eq (funsym numer1) 'DIVIDE))
	  (setq numer (arg1 numer1)
		denom (arg2 numer1))
	(setq numer numer1))
      (if (and (consp denom1) (eq (funsym denom1) 'DIVIDE))
	  (setq numer (sigtimes `(times ,numer ,(arg2 denom1)))
		denom (sigtimes `(times ,denom ,(arg1 denom1))))
	(setq denom (sigtimes `(times ,denom ,denom1))))
      (if (or (symbolp denom) (and (consp denom) (eq (funsym denom) 'plus)))
	  (sigtimes `(times ,numer ,(make-inverse-1 denom)))
	(progn
	  (make-product-from-numer numer)
	  (make-product-from-denom denom)
	  (setq coef (qnorm coef))
	  (cond
	   (product
	    (setq product
		  (cond
		   ((cdr product) (cons 'times (sort product 'arithord)))
		   (t (car product))))
	    (cond
	     ((equal coef 1) product)
	     (t `(times ,coef ,product))))
	   (t coef)))))))

(defun make-product-from-numer (numer)
  (cond
   ((qnumberp numer)
    (setq coef (qtimes coef numer)))
   ((symbolp numer)
    (setq product (nconc product (ncons numer))))
   ((eq (funsym numer) 'times)
    (if (qnumberp (arg1 numer))
	(prog ()
	      (setq coef (qtimes coef (arg1 numer)))
	      (make-product-from-numer (arg2 numer)))
      (setq product (nconc product (copy-list (argsof numer))))))
   (t (setq product (nconc product (ncons numer))))))

(defun make-product-from-denom (denom)
  (cond
   ((qnumberp denom)
    (if (qzerop denom)
	(make-product-from-numer (make-infinity))
      (setq coef (qquotient coef denom))))
   ((symbolp denom)
    (if (member denom product)
	(setq product (remove denom product :count 1))
      (setq product (nconc product (ncons (make-inverse-1 denom))))))
   ((eq (funsym denom) 'times)
    (loop for d in (argsof denom) do (make-product-from-denom d)))
   (t (if (member denom product :test #'equal)
	  (setq product (remove denom product :test #'equal :count 1))
	(setq product (nconc product (ncons (make-inverse denom))))))))

(defun make-inverse (denom)
  (if (and (consp denom) (eq (funsym denom) 'divide))
      (arg2 denom)
    (make-inverse-1 denom)))

(defun make-inverse-1 (denom)
  (if (is-infinity denom)
      0
      `(DIVIDE 1 ,denom)))

(defun is-infinity (sym)
  (and (symbolp sym)
       (> (length (string sym)) 10)
       (string= "*-infinity" (subseq (string sym) 0 10))))

(defun make-infinity ()
  (gensym "*-infinity"))

(defun sigplus(u)
  (prog(sum const result)
    (setq const 0 sum (ncons nil))
    (loop for v in (cdr u) do (buildsum v))
    (setq result (collectsum (cdr sum)))
    (setq const (qnorm const))
    (return
     (cond
      (result
       (cond
	((equal const 0)
	 (cond
	  ((cdr result)(cons 'plus result))
	  (t (car result))))
	(t `(plus ,const ,.result))))
      (t const)))))

(defun buildsum(u)
  (cond
   ((qnumberp u)(setq const (qplus u const)))
   ((symbolp u)(addtosum u 1))
   ((and (eq (car u) 'times)(qnumberp (cadr u)))
    (addtosum (caddr u) (cadr u)))
   ((eq (car u) 'plus)(loop for v in (cdr u) do (buildsum v)))
   (t (addtosum u 1))))

(defun addtosum(u coef)
  (do
   ((ptr sum (cdr ptr)))
   ((null (cdr ptr))                   ;couldn't find u. Enter it at end.
    (rplacd ptr (ncons (cons u coef))))
   (cond
    ((equal (caadr ptr) u)             ;found it
     (rplacd (cadr ptr) (qnorm (qplus (cdadr ptr) coef)))
     (return nil))
    ((arithord u (caadr ptr))          ;Splice it in here.
       (rplacd ptr
	       (cons (cons u coef)(cdr ptr)))
       (return nil)))))

(defun collectsum(s)
  (loop for pair in s nconc
      (cond
       ((qzerop (cdr pair)) nil)
       ((qeqp (cdr pair) 1)(ncons (car pair)))
       (t (ncons `(times ,(cdr pair) ,(car pair)))))))

(defun sigdifference(u)
  (sigplus `(plus ,(cadr u) ,(sigminus1 (caddr u)))))

(defun sigminus(u)(sigminus1 (cadr u)))


(defun sigminus1(u)
  (cond
   ((qnumberp u)(qminus u))
   ((symbolp u) `(times -1 ,u))
   ((and (eq (car u) 'times)(qnumberp (cadr u)))
    (sigtimes `(times ,(qminus (cadr u)) ,.(cddr u))))	; absence of sigtimes caused 'bug1'  MES 8/16/88
   ((eq (car u) 'plus)
    (sigplus (cons 'plus (mapcar #'sigminus1 (cdr u)))))	; sigplus added but may not be necessary  MES 8/16/88
   (t (sigtimes `(times -1 ,u)))))
;
;(defun sigminus1(u)
;  (cond
;   ((qnumberp u)(qminus u))
;   ((symbolp u) `(times -1 ,u))
;   ((and (eq (car u) 'times)(qnumberp (cadr u)))
;    `(times ,(qminus (cadr u)) ,.(cddr u)))
;   ((eq (car u) 'plus)
;    (cons 'plus (mapcar #'sigminus1 (cdr u))))
;   (t `(times -1 ,u))))


; solver for positive predicates over linear arithmetic

(defun arithsolve(lit)
  (case (funsym lit)				
    (EQUAL (ncons (normineq lit)))
    ((LESSEQP GREATEREQP LESSP GREATERP) (ineqsolve lit))
    (t `((EQUAL ,lit TRUE)))))

; solver for negative predicates over linear arithmetic

(defun arithnsolve(lit)
  (case (funsym lit)				
    (EQUAL (eqnsolve lit))
    ((LESSEQP GREATEREQP LESSP GREATERP)(ineqsolve (negineq lit)))
    (t `((EQUAL ,lit FALSE)))))

; solver for negation of equality over linear arithmetic
; patched 6/3/85 by JMR to do retfalse if process returns true
(defun eqnsolve(lit)
  (prog (res)
    (return
     (cond
      ((equal (arg1 lit)(arg2 lit)) (retfalse))
      ((eq (setq res (newcontext (process lit))) 'FALSE) '(TRUE))
      ((eq res 'TRUE) (retfalse))
      (t `((NEQUAL ,(arg1 lit) ,(arg2 lit))))))))

; returns negation of inequality

(defun negineq(ineq)
  (list
   (case (funsym ineq)				
       (LESSEQP 'GREATERP)
       (LESSP   'GREATEREQP)
       (GREATERP 'LESSEQP)
       (GREATEREQP 'LESSP))
   (arg1 ineq)
   (arg2 ineq)))

; solves an inequality (other than an equality)


(defun ineqsolve(ineq)
  (prog(norm res)
    (setq norm (normineq ineq))      ; normalize it
    (return
     (cond
      ((eq norm 'TRUE) '(TRUE))
      ((eq norm 'FALSE) (retfalse))
      ((eq norm 'IDENT) '(TRUE))
      ((eq (setq res (newcontext (process1 (ncons (negineq norm))))) 'FALSE)
       '(TRUE)) ;dac 8-28-91: used to be TRUE, but process1 could have returned FALSE
		      ; due to using a recently generated pr-union in pr-merge
                      ; but would have retruned TRUE if it didn't use that pr-union.
                      ; thus this is safer as the contradiction will be found later.
      ((eq res 'TRUE) (retfalse))
      (t (ncons norm))))))


; asserts inequality ineq

(defun addineq(ineq)
  (prog(ineqpot)
    (transclosure ineq)              ; perform transitive closure
    (setq s (append ineqpot s))))    ; add result to s

; 1/4/91: DAC flag for converting inequality bounds into disjunct of equalities.

(defvar *jmr-dist* 3)

; 1/4/91: DAC the lhs of ineq1 and ineq2 fall between the rhs's
;         If the difference between the bounds is < *jmr-dist*
;         then form the disjunct of the equals.


(defun add-disjunct-of-equals (ineq1 ineq2)
  (let (lb ub)
    (when (eq (prtype (arg1 ineq1)) 'integer)
      (case (funsym ineq1)
	    (LESSP (setq ub (if (eq (prtype (arg2 ineq1)) 'integer)
				(sigplus `(plus -1 ,(arg2 ineq1)))
				(arg2 ineq1))))
	    (LESSEQP (setq ub (arg2 ineq1)))
	    (GREATERP (setq lb (if (eq (prtype (arg2 ineq1)) 'integer)
				   (sigplus `(plus 1 ,(arg2 ineq1)))
				   (arg2 ineq1))))
	    (GREATEREQP (setq lb (arg2 ineq1))))
      (case (funsym ineq2)
	    (LESSP (setq ub (if (eq (prtype (arg2 ineq2)) 'integer)
				(sigplus `(plus -1 ,(arg2 ineq2)))
				(arg2 ineq2))))
	    (LESSEQP (setq ub (arg2 ineq2)))
	    (GREATERP (setq lb (if (eq (prtype (arg2 ineq2)) 'integer)
				   (sigplus `(plus 1 ,(arg2 ineq2)))
				   (arg2 ineq2))))
	    (GREATEREQP (setq lb (arg2 ineq2))))
      (let ((dif (sigdifference `(difference ,ub ,lb))))
	;(break)
	(when (and (qlesseqp dif *jmr-dist*) (eq (prtype lb) 'integer))
	  (push (make-equals-from-bounds (arg1 ineq1) lb ub dif) ineqpot))))))

(defun make-equals-from-bounds (var lb ub
				    &optional (dif (sigdifference `(difference ,ub ,lb))))
  (if (qzerop dif)
      `(EQUAL ,var ,lb)
    (cons `OR
	  (loop for i from 0 ; to dif
		until (qgreaterp i dif)
		collect `(EQUAL ,var ,(sigplus `(plus ,i ,lb)))))))

; calculates equalities and inequalities in transitive
; closure produced by ineq, adds them to special global ineqpot
; supplied by ineqsolve

;(defun transclosure(ineq)
;  (push `(EQUAL ,(canonsig ineq) TRUE) ineqpot)
;  (for chainineq in (chainineqs ineq) do-bind (norm) do
;       (setq norm (normineq (residue ineq chainineq)))
;       (cond
;	((eq norm 'TRUE))
;	((eq norm 'FALSE)(retfalse))
;	((eq norm 'IDENT)
;	 (push `(EQUAL ,(arg1 ineq),(arg2 ineq)) ineqpot)
;	 (return nil))
;	(t (transclosure norm)))))

;; TRANSCLOSURE WAS CREATING INEQPOT THAT MIGHT CONTAIN
;; BOTH (EQUAL (LESSP X 0) TRUE) AND (EQUAL (GREATERP X 0) TRUE).
;; THE FOLLOWING CHANGE CAUSES SUCH CONTRADICTIONS TO
;; BE DETECTED AND FIXES 'BUG2' AND 'BUG4'.
;; QUESTION: IS THIS OVERKILL?  ARE MORE INEQUALITIES
;; ASSERTED THAN NECESSARY?  ARE CONTRADICTIONS IN
;; INEQPOT SUPPOSED TO BE DETECTED IN ANOTHER WAY?
;; MES 8/17/88

(defvar *ineqstack* NIL) ;;;NSH(4/90): a loop detector for transclosure

(defun normineqatom (ineq)
  (if (consp ineq)
      (let ((norm (normineq ineq)))
	(if (eq norm `IDENT)
	    TRUE
	  norm))
    ineq))

(defun normalize-new-eqn (eqn)
  (normineq `(EQUAL ,(canonsig (arg1 eqn))
		    ,(canonsig (arg2 eqn)))))

(defun transclosure(ineq)		;(break)
  (cond ((member ineq *ineqstack* :test #'equal) NIL)
	(t (let* ((*ineqstack* (cons ineq *ineqstack*))
		  (nrmineq (normineqatom (arithcan ineq)))
					;(nrmineq-TRUE `(EQUAL ,nrmineq TRUE))
		  (nrmineq-unchanged
		   (or (not (consp nrmineq)) (equal nrmineq ineq))))
	     (if (eq nrmineq 'FALSE) (retfalse))
	     (when nrmineq-unchanged
	       (setq nrmineq `(EQUAL ,nrmineq TRUE)))
	     ;;; 1/28/94: DAC added nrmineq-unchanged so that nrmineq
	     ;;; would be added to the begginning or end of ineqpot
	     ;;; correctly. (See notes by DAC and NSH below.)
	     (unless nrmineq-unchanged
	       (push nrmineq ineqpot))
	     ;;; 11/17/92: DAC Put nrmineq into the ineqpot after the
	     ;;; following loop that adds the transitive closure consequences.
	     ;;; This puts the nrmineq at the beginning of the ineqpot so that
	     ;;; process1 will process it first. This is important in the case
	     ;;; that the nrmineq is of the form (EQUAL blah TRUE).
	     ;;; IN this case we need the findalist to note this so that the
	     ;;; next time blah is seen it will be reduced to true and not
	     ;;; added to the ineqpot.

	     (when *tc-dbg* (break "before loop"))

	     (loop for chainineq in (chainineqs ineq) with (norm) do
		   (setq norm (normineq (residue ineq chainineq)))

		   (when *tc-dbg* (break "loop"))

		   (cond
		    ((eq norm 'TRUE)
		     (add-disjunct-of-equals ineq chainineq))
		    ((eq norm 'FALSE)(retfalse))
		    ((eq norm 'IDENT)
		     (let ((new-eqn
			    (if *tc-ehdm-test*
				`(EQUAL ,(arg1 ineq) ,(arg2 ineq))
				(normineq `(EQUAL ,(canonsig (arg1 ineq))
						  ,(canonsig (arg2 ineq)))))))
		       (unless (if *tc-ehdm-test*
				   (subtermof (arg1 ineq) (arg2 ineq))
				   (bad-eqn new-eqn))
			 (push new-eqn ineqpot)
			 (return nil))))
		    (t (push norm ineqpot))))
	     ;;; 11/17/92: DAC see note above about nrmineq.
	     ;;; NSH(1/15/94): Loops without WHEN clause below.  DAC's note
	     ;;; indicates why it might be needed.  

	     (when *tc-dbg* (break "after loop"))
	     
	     (when nrmineq-unchanged ;(and (consp nrmineq)(eq (car nrmineq) 'EQUAL))
	       (push nrmineq ineqpot)
	       )
	     ))))


; returns list of TRUE ineqs that chain with ineq.  e.g. x < y chains
; with x > z

(defun chainineqs(ineq)
  (append
   (chain-square-ineq ineq)
   (loop for u in (use (arg1 ineq))
	 when (and (oppsensep (funsym ineq)(funsym u))
		   (equal (arg1 ineq)(arg1 u))
		   (eq (pr-find u) 'TRUE))
	 collect u)))

(defun chain-square-ineq (ineq)
  (when (and (or (eq (funsym ineq) 'LESSEQP) (eq (funsym ineq) 'LESSP))
	     (consp (arg1 ineq))
	     (eq (funsym (arg1 ineq)) 'TIMES)
	     (equal (arg1 (arg1 ineq))
		    (arg2 (arg1 ineq)))
	     (= (length (argsof (arg1 ineq))) 2))
    `((GREATEREQP ,(arg1 ineq) 0))))

; returns TRUE if fnsym2 is an inequality operator with sense 
; opposite to that of fnsym1

(defun oppsensep(fnsym1 fnsym2)
  (case fnsym1
    ((LESSEQP LESSP)(or (eq fnsym2 'GREATEREQP)(eq fnsym2 'GREATERP)))
    ((GREATERP GREATEREQP)(or (eq fnsym2 'LESSEQP)(eq fnsym2 'LESSP)))))

; returns the residue obtained by chaining two inequalities with
; same first arg and with inequality operators having opposite sense

(defun residue(ineq1 ineq2)
  (list 
   (case (funsym ineq2)
     (LESSP 'GREATERP)
     (GREATERP 'LESSP)
     (t (funsym ineq1)))
   (arg2 ineq2)
   (arg2 ineq1)))

; normalizes an equality or inequality, returns TRUE, FALSE, IDENT
; (for inequals that eval to TRUE), or something of form
; (fnsym var linearexpr)


(defun normineq(lit)
  (integercut (normineq1 lit)))

; normalization other than Gomery cut

;;; DAC 10-26-90 Added EXP to PRARITHERROR. (now *printerpmult* - SO 5/26/91)
;;; This is experimental mode.
;;; It first treats multiplication interpretted as before
;;; meaning it does not treat products as independant variables.
;;; If it finds that it cannot solve the equation this way
;;; it tries again, but treating products as independent variables.
;;; by letting *kmr-mut* to be t.
;;; If *jmr-mult* is originally t then products are alwys treated independently.

(defvar *jmr-mult* nil) ; if t then treat mult as uninterp as much as plausible.

(defun normineq1(lit)
  (prog(var coef dif fnsymbol)
       (setq fnsymbol (funsym lit))
       (setq dif (sigdifference `(difference ,(arg1 lit) ,(arg2 lit))))
       (return
	(cond
	 ((qnumberp dif)(normconstdif fnsymbol dif)) ; constant dif
	 ((solvable dif (equal fnsymbol 'equal))
	  (list
	   (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
	   var
;;;	(canon `(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif)))
;;;   fix to get around the "order bug"  18-Nov-85
	   (arithcan 
	    ;`(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif))
	    ;; DAC fix to make sure var is cancelled in resulting expression.
	    `(times ,(qnorm (qquotient -1 coef))
		   ,(sigplus `(plus ,(sigtimes `(times ,(qtimes -1 coef) ,var)) ,dif))))
	   ))
	 ((eq *printerpmult* 'normal) lit)
	 ((eq *printerpmult* 'experimental)
	  (let ((*jmr-mult* t))
	    (if (solvable dif (equal fnsymbol 'equal))
		(list
		 (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
		 var
		 (arithcan 
		  `(times ,(qnorm (qquotient -1 coef))
			  ,(sigplus `(plus ,(sigtimes `(times ,(qtimes -1 coef) ,var)) ,dif))))
		 )
	      lit)))
	 (t (prerr "Formula falls outside domain of completeness"))))))

(defun arithcan (term)   ;weakened formof canon, added to fix "order bug"
  (catch 'found
    (cond
     ((symbolp term) (pr-find term))
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
							   (pr-find u))) )
	      (loop for arg in (argsof newterm) do (adduse newterm arg)) ))
	newterm ))
     (T term) )))


; if expression dif can be solved, i.e., has a variable v that occurs
; in exactly one term of l and occurs linearly in that term, sets
; global variable var to v and coef to its coefficient.
;
; else returns nil

(defun solvable(dif &optional (funsymis= nil))
  (cond
   ((listp dif)
    (case (funsym dif)
      (PLUS (solvableplus dif funsymis=))
      (TIMES (solvabletimes dif))
      (t (setq var dif coef 1) T)))
   (t (setq var dif coef 1) T)))

; solvable for case in which dif is a PLUS expression

(defun solvableplus(dif &optional (funsymis= nil))
  (loop for term in (argsof dif)
	thereis (and (not (qnumberp term))
		     (cond
		       ((linearp term)
			(setq var (varof term) coef (coefof term))
			(onlyoccurrencep var term dif funsymis=))))))

; solvable for case in which dif is a TIMES expression

(defun solvabletimes(dif)
  (cond
   ((linearp dif)(setq var (varof dif) coef (coefof dif)) T)))

; returns T if term has no nonlinear use of times

;;; 18-Sept-90 DAC: From arith.lisp to change mult to uninterp as far as solving eqns.

(defun linearp (term)
  (or *jmr-mult*
      (cond
       ((and (listp term)(eq (funsym term) 'TIMES))
	(and (qnumberp (arg1 term))
	     (linearp (arg2 term))))
       (t t))))

; returns T if only occurrence of var in PLUS expr l is in term t

(defun onlyoccurrencep(var term l &optional (funsymis= nil))
  (loop for arg in (argsof l)
	never (and (not (equal arg term))
		   (occursin var arg funsymis=))))

; returns T if var is a variable of term

(defun occursin (var term &optional (funsymis= nil))
  (cond
   ((and (listp term)(eq (funsym term) 'TIMES))
    (cond
     ((qnumberp (arg1 term))(occursin var (arg2 term) funsymis=))
     (t (if *jmr-mult*
	    (or (equal var term)
		(if funsymis=;; dac 8/31/92: kludge!!!
		    ;; only reallycare about x = x * y,
		    ;; not about x < x * y.
		    (member-or-div-member var (argsof term))
		    nil))
	    (member var (argsof term))))))
   ((and (listp term)(eq (funsym term) 'DIVIDE) funsymis=)
    (cond
     ((qnumberp (arg1 term))(interp-subtermof var (arg2 term)))
     (t (or (interp-subtermof var (arg1 term))
	    (interp-subtermof var (arg2 term))))))
   ((equal var term))))

(defun interp-subtermof (var term)
  (cond
   ((null term) nil)
   ((not (consp term)) (equal var term))
   (t (or (equal var term)
	  (and (interp term)
	       (loop for st in (argsof term)
		     thereis (interp-subtermof var st)))))))

(defun member-or-div-member-test (x y)
  (or (equal x y)
      (and (consp y)
	   (eq (funsym y) 'divide)
	   (interp-subtermof var (arg2 y)))))

(defun member-or-div-member (var var-and-div-list)
  (member var var-and-div-list
	  :test #'member-or-div-member-test))


; normalize for a constant difference

(defun normconstdif(fnsymbol dif)
  (cond
   ((qzerop dif)
    (case fnsymbol
      (EQUAL 'TRUE)
      ((LESSEQP GREATEREQP) 'IDENT)
      (t 'FALSE)))
   ((qminusp dif)
    (case fnsymbol
      ((LESSP LESSEQP) 'TRUE)
      (t  'FALSE)))
   (t
    (case fnsymbol
      ((GREATERP GREATEREQP) 'TRUE)
      (t 'FALSE)))))

; returns symmetric function symbol for f

(defun antifnsym(fnsym)
  (case fnsym		
    (LESSEQP 'GREATEREQP)
    (GREATEREQP 'LESSEQP)
    (LESSP 'GREATERP)
    (GREATERP 'LESSP)
    (t fnsym)))

; returns first term of linear form or nil if there isn't one
; returns ax in expression of form c + ax + by + ...

(defun firsttermof(l)
  (cond
   ((qnumberp l) nil)
   ((and (consp l)(eq (funsym l) 'PLUS))
    (cond
     ((qnumberp (arg1 l))(arg2 l))
     (t (arg1 l))))
   (t l)))

; returns var part of a term
; e.g., x in 3x

(defun varof(term)
  (cond
   ((and (consp term)			
	 (equal (funsym term) 'TIMES)
	 (qnumberp (arg1 term)))
    (arg2 term))
   (t term)))

; returns coefficient of a term

(defun coefof(term)
  (cond
   ((and (consp term)			
	 (equal (funsym term) 'TIMES)
	 (qnumberp (arg1 term)))
    (arg1 term))
   (t 1)))

; tests for and performs Gomery cut on inequality with integer first argument

(defun integercut(lit)
  (prog(fract)
    (setq fract
	  (and 
	   (consp lit)
	   (eq (prtype (arg1 lit)) 'INTEGER)
	   (fractpt (arg2 lit))))
    (return
     (cond
      ((null fract) lit)
      ((qzerop fract) (zerocut lit))
      ((qminusp fract) (minuscut lit fract))
      (t (poscut lit fract))))))

; performs cut for zero fractional part

(defun zerocut(ineq)
  (case (funsym ineq)	
    (LESSP
     `(LESSEQP
       ,(arg1 ineq)
       ,(sigplus `(plus -1 ,(arg2 ineq)))))
    (GREATERP
     `(GREATEREQP
       ,(arg1 ineq)
       ,(sigplus `(plus 1 ,(arg2 ineq)))))
    (t ineq)))

; performs cut for negative fractional part

(defun minuscut(ineq fract)
  (case (funsym ineq)	
    (EQUAL 'FALSE)
    ((LESSP LESSEQP)
     `(LESSEQP
       ,(arg1 ineq)
       ,(sigplus `(plus ,(qminus (qplus fract 1)) ,(arg2 ineq)))))
    (t
     `(GREATEREQP ,(arg1 ineq)
		  ,(sigdifference `(difference ,(arg2 ineq) ,fract))))))

; performs cut for positive fractional part

(defun poscut(ineq fract)
  (case (funsym ineq)
    (EQUAL 'FALSE)
    ((LESSP LESSEQP)
     `(LESSEQP
       ,(arg1 ineq)
       ,(sigplus `(plus ,(qminus fract) ,(arg2 ineq)))))
    (t
     `(GREATEREQP
       ,(arg1 ineq)
       ,(sigplus `(plus ,(qdifference 1 fract) ,(arg2 ineq)))))))

; returns the fractional part of expression l if one can be determined
; or nil otherwise

(defun fractpt(l)
  (cond
   ((eq (prtype l) 'integer) 0)
   ((symbolp l) nil)
   ((qnumberp l)(qfractpt l))
   (t
    (case (funsym l)	
      (plus (plusfractpt l))
      (times (timesfractpt l))
      (t nil)))))

; fractpt for a plus expression

(defun plusfractpt(l)
  (loop for arg in (cdr l)
	with sum = 0
	with frac = nil
       finally (return (qfractpt sum))
       do (cond ((setq frac (fractpt arg))(setq sum (qplus frac sum)))
		(t (return nil)))))

; fractpt for a times expression

(defun timesfractpt(l)
  (and
   (loop for arg in (cdr l) always (qzerop (fractpt arg)))
   0))
