;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-
; ARITH - sigma and solver for linear arith

;; HISTORY 
;; 17-Mar-1994		Natarajan Shankar	
;;    Misc

;;; SO 11/26/90 - now driven off emacs variable *printerpdivide*
;(defvar *uninterp-divide* t)

(declaim (notinline canonsig-arith))

(defvar *arithops* '(plus minus difference times divide))

(defun canonsig-arith (term &optional (dont-add-use nil))
  (canonsig term dont-add-use))

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
       ((symbolp v)
	;; Added the upcase of interpreted symbols because otherwise
	;; terms are ordered differently in case-sensitive version
	(let ((ui (if (memq u interpsyms)
		      (intern (string-upcase u))
		      u))
	      (vi (if (memq v interpsyms)
		      (intern (string-upcase v))
		      v)))
	  (alphalessp ui vi)))
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
      (if (and (consp numer1) (eq (funsym numer1) 'divide))
	  (setq numer (arg1 numer1)
		denom (arg2 numer1))
	(setq numer numer1))
      (if (and (consp denom1) (eq (funsym denom1) 'divide))
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
      `(divide 1 ,denom)))

(defun is-infinity (sym)
  (eq sym *infinity*))

(defun make-infinity ()
  *infinity*)

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
    ((nonlinarithord u (caadr ptr))          ;Splice it in here.
       (rplacd ptr
	       (cons (cons u coef)(cdr ptr)))
       (return nil)))))

(defun nonlinarithord (u v)
  (if *jmr-mult*
      (let ((uvars (sortedarithvars u))
	    (vvars (sortedarithvars v)))
	(if (and uvars vvars)
	    (arithordlist uvars vvars)
	    (arithord u v)))
      (arithord u v)))

(defun sortedarithvars (term)
  (sort (remove-duplicates (allarithvarsof term)) #'arithord))

(defun allarithvarsof (term)
  (cond ((and (consp term)
	      (memq (car term) *arithops*))
	 (loop for arg in (cdr term)
	       nconc (allarithvarsof arg)))
	((qnumberp term) nil)
	(t (list term))))

(defun arithordlist (x y)
  (if (consp y)
      (if (consp x)
	  (if (equal x y)
	      (arithordlist (cdr x)(cdr y))
	      (arithord x y))
	  t)
      nil))


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
    (equal (ncons (normineq lit)))
    ((lesseqp greatereqp lessp greaterp) (ineqsolve lit))
    (t `((equal ,lit true)))))

; solver for negative predicates over linear arithmetic

(defun arithnsolve(lit)
  (case (funsym lit)				
    (equal (eqnsolve lit))
    ((lesseqp greatereqp lessp greaterp)(ineqsolve (negineq lit)))
    (t `((equal ,lit false)))))

; solver for negation of equality over linear arithmetic
; patched 6/3/85 by JMR to do retfalse if process returns true
(defun eqnsolve(lit)
  (prog (res)
    (return
     (cond
      ((equal (arg1 lit)(arg2 lit)) (retfalse))
      ((eq (setq res (newcontext (process lit))) 'false) '(true))
      ((eq res 'true) (retfalse))
      (t `((nequal ,(arg1 lit) ,(arg2 lit))))))))

; returns negation of inequality

(defun negineq(ineq)
  (list
   (case (funsym ineq)				
       (lesseqp 'greaterp)
       (lessp   'greatereqp)
       (greaterp 'lesseqp)
       (greatereqp 'lessp))
   (arg1 ineq)
   (arg2 ineq)))

; solves an inequality (other than an equality)


(defun ineqsolve(ineq)
  (prog(norm res)
    (setq norm (normineq ineq))      ; normalize it
    (return
     (cond
      ((eq norm 'true) '(true))
      ((eq norm 'false) (retfalse))
      ((eq norm 'ident) '(true))
      ((eq (setq res (newcontext (process1 (ncons (negineq norm))))) 'false)
       '(true)) ;dac 8-28-91: used to be true, but process1 could have returned false
		      ; due to using a recently generated pr-union in pr-merge
                      ; but would have retruned true if it didn't use that pr-union.
                      ; thus this is safer as the contradiction will be found later.
      ((eq res 'true) (retfalse))
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
	    (lessp (setq ub (if (eq (prtype (arg2 ineq1)) 'integer)
				(sigplus `(plus -1 ,(arg2 ineq1)))
				(arg2 ineq1))))
	    (lesseqp (setq ub (arg2 ineq1)))
	    (greaterp (setq lb (if (eq (prtype (arg2 ineq1)) 'integer)
				   (sigplus `(plus 1 ,(arg2 ineq1)))
				   (arg2 ineq1))))
	    (greatereqp (setq lb (arg2 ineq1))))
      (case (funsym ineq2)
	    (lessp (setq ub (if (eq (prtype (arg2 ineq2)) 'integer)
				(sigplus `(plus -1 ,(arg2 ineq2)))
				(arg2 ineq2))))
	    (lesseqp (setq ub (arg2 ineq2)))
	    (greaterp (setq lb (if (eq (prtype (arg2 ineq2)) 'integer)
				   (sigplus `(plus 1 ,(arg2 ineq2)))
				   (arg2 ineq2))))
	    (greatereqp (setq lb (arg2 ineq2))))
      (let ((dif (sigdifference `(difference ,ub ,lb))))
	;(break)
	(when (and (qlesseqp dif *jmr-dist*) (eq (prtype lb) 'integer))
	  (push (make-equals-from-bounds (arg1 ineq1) lb ub dif) ineqpot))))))

(defun make-equals-from-bounds (var lb ub
				    &optional (dif (sigdifference `(difference ,ub ,lb))))
  (if (qzerop dif)
      `(equal ,var ,lb)
    (cons `or
	  (loop for i from 0 ; to dif
		until (qgreaterp i dif)
		collect `(equal ,var ,(sigplus `(plus ,i ,lb)))))))

; calculates equalities and inequalities in transitive
; closure produced by ineq, adds them to special global ineqpot
; supplied by ineqsolve

;(defun transclosure(ineq)
;  (push `(equal ,(canonsig ineq) true) ineqpot)
;  (for chainineq in (chainineqs ineq) do-bind (norm) do
;       (setq norm (normineq (residue ineq chainineq)))
;       (cond
;	((eq norm 'true))
;	((eq norm 'false)(retfalse))
;	((eq norm 'ident)
;	 (push `(equal ,(arg1 ineq),(arg2 ineq)) ineqpot)
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

(defvar *ineqstack* nil) ;;;NSH(4/90): a loop detector for transclosure

(defun normineqatom (ineq)
  (if (consp ineq)
      (let ((norm (normineq ineq)))
	(if (eq norm `ident)
	    true
	  norm))
    ineq))

(defun normalize-new-eqn (eqn)
  (normineq `(equal ,(canonsig-arith (arg1 eqn))
		    ,(canonsig-arith (arg2 eqn)))))

(defun transclosure(ineq)		;(break)
  (cond ((member ineq *ineqstack* :test #'equal) nil)
	(t (let* ((*ineqstack* (cons ineq *ineqstack*))
		  (nrmineq (normineqatom (arithcan ineq)))
					;(nrmineq-true `(equal ,nrmineq true))
		  (nrmineq-unchanged
		   (or (not (consp nrmineq)) (equal nrmineq ineq))))
	     (if (eq nrmineq 'false) (retfalse))
	     (when nrmineq-unchanged
	       (setq nrmineq `(equal ,nrmineq true)))
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
		    ((eq norm 'true)
		     (add-disjunct-of-equals ineq chainineq))
		    ((eq norm 'false)(retfalse))
		    ((eq norm 'ident)
		     (let ((new-eqn
			    (if *tc-ehdm-test*
				`(equal ,(arg1 ineq) ,(arg2 ineq))
				(normineq `(equal ,(canonsig-arith (arg1 ineq))
						  ,(canonsig-arith (arg2 ineq)))))))
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
	     
	     (when nrmineq-unchanged ;(and (consp nrmineq)(eq (car nrmineq) 'equal))
	       (push nrmineq ineqpot)
	       )
	     ))))


; returns list of true ineqs that chain with ineq.  e.g. x < y chains
; with x > z

(defun chainineqs(ineq)
  (append
   (chain-square-ineq ineq)
   (loop for u in (use (arg1 ineq))
	 when (and (oppsensep (funsym ineq)(funsym u))
		   (equal (arg1 ineq)(arg1 u))
		   (eq (pr-find u) 'true))
	 collect u)))

(defun chain-square-ineq (ineq)
  (when (and (or (eq (funsym ineq) 'lesseqp) (eq (funsym ineq) 'lessp))
	     (consp (arg1 ineq))
	     (eq (funsym (arg1 ineq)) 'times)
	     (equal (arg1 (arg1 ineq))
		    (arg2 (arg1 ineq)))
	     (= (length (argsof (arg1 ineq))) 2))
    `((greatereqp ,(arg1 ineq) 0))))

; returns true if fnsym2 is an inequality operator with sense 
; opposite to that of fnsym1

(defun oppsensep(fnsym1 fnsym2)
  (case fnsym1
    ((lesseqp lessp)(or (eq fnsym2 'greatereqp)(eq fnsym2 'greaterp)))
    ((greaterp greatereqp)(or (eq fnsym2 'lesseqp)(eq fnsym2 'lessp)))))

; returns the residue obtained by chaining two inequalities with
; same first arg and with inequality operators having opposite sense

(defun residue(ineq1 ineq2)
  (list 
   (case (funsym ineq2)
     (lessp 'greaterp)
     (greaterp 'lessp)
     (t (funsym ineq1)))
   (arg2 ineq2)
   (arg2 ineq1)))

; normalizes an equality or inequality, returns true, false, ident
; (for inequals that eval to true), or something of form
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
	 ((solvable dif)
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
     (t term) )))


; if expression dif can be solved, i.e., has a variable v that occurs
; in exactly one term of l and occurs linearly in that term, sets
; global variable var to v and coef to its coefficient.
;
; else returns nil

(defun solvable (dif)
  (cond
   ((listp dif)
    (case (funsym dif)
      (plus (solvableplus dif))
      (times (solvabletimes dif))
      (t (setq var dif coef 1) t)))
   (t (setq var dif coef 1) t)))

; solvable for case in which dif is a plus expression

(defun solvableplus(dif)
  (loop for term in (argsof dif)
	thereis (and (not (qnumberp term))
		     (cond
		       ((linearp term)
			(setq var (varof term) coef (coefof term))
			(onlyoccurrencep var term dif))))))

; solvable for case in which dif is a times expression

(defun solvabletimes(dif)
  (cond
   ((linearp dif)(setq var (varof dif) coef (coefof dif)) t)))

; returns t if term has no nonlinear use of times

;;; 18-Sept-90 DAC: From arith.lisp to change mult to uninterp as far as solving eqns.

(defun linearp (term)
  (or *jmr-mult*
      (cond
       ((and (listp term)(eq (funsym term) 'times))
	(and (qnumberp (arg1 term))
	     (linearp (arg2 term))))
       (t t))))

; returns t if only occurrence of var in plus expr l is in term t

(defun onlyoccurrencep(var term l)
  (loop for arg in (argsof l)
	never (and (not (equal arg term))
		   (occursin var arg))))

; returns t if var is a variable of term

(defun occursin (var term)
  (cond
   ((and (listp term)(eq (funsym term) 'times))
    (cond
     ((qnumberp (arg1 term))(occursin var (arg2 term)))
     (t (if *jmr-mult*
	    (or (equal var term)
		(member-or-div-member var (argsof term)))
	    (member var (argsof term))))))
   ((and (listp term)(eq (funsym term) 'divide))
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
      (equal 'true)
      ((lesseqp greatereqp) 'ident)
      (t 'false)))
   ((qminusp dif)
    (case fnsymbol
      ((lessp lesseqp) 'true)
      (t  'false)))
   (t
    (case fnsymbol
      ((greaterp greatereqp) 'true)
      (t 'false)))))

; returns symmetric function symbol for f

(defun antifnsym(fnsym)
  (case fnsym		
    (lesseqp 'greatereqp)
    (greatereqp 'lesseqp)
    (lessp 'greaterp)
    (greaterp 'lessp)
    (t fnsym)))

; returns first term of linear form or nil if there isn't one
; returns ax in expression of form c + ax + by + ...

(defun firsttermof(l)
  (cond
   ((qnumberp l) nil)
   ((and (consp l)(eq (funsym l) 'plus))
    (cond
     ((qnumberp (arg1 l))(arg2 l))
     (t (arg1 l))))
   (t l)))

; returns var part of a term
; e.g., x in 3x

(defun varof(term)
  (cond
   ((and (consp term)			
	 (equal (funsym term) 'times)
	 (qnumberp (arg1 term)))
    (arg2 term))
   (t term)))

; returns coefficient of a term

(defun coefof(term)
  (cond
   ((and (consp term)			
	 (equal (funsym term) 'times)
	 (qnumberp (arg1 term)))
    (arg1 term))
   (t 1)))

; tests for and performs Gomery cut on inequality with integer first argument

(defun integercut(lit)
  (prog(fract)
    (setq fract
	  (and 
	   (consp lit)
	   (eq (prtype (arg1 lit)) 'integer)
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
    (lessp
     `(lesseqp
       ,(arg1 ineq)
       ,(sigplus `(plus -1 ,(arg2 ineq)))))
    (greaterp
     `(greatereqp
       ,(arg1 ineq)
       ,(sigplus `(plus 1 ,(arg2 ineq)))))
    (t ineq)))

; performs cut for negative fractional part

(defun minuscut(ineq fract)
  (case (funsym ineq)	
    (equal 'false)
    ((lessp lesseqp)
     `(lesseqp
       ,(arg1 ineq)
       ,(sigplus `(plus ,(qminus (qplus fract 1)) ,(arg2 ineq)))))
    (t
     `(greatereqp ,(arg1 ineq)
		  ,(sigdifference `(difference ,(arg2 ineq) ,fract))))))

; performs cut for positive fractional part

(defun poscut(ineq fract)
  (case (funsym ineq)
    (equal 'false)
    ((lessp lesseqp)
     `(lesseqp
       ,(arg1 ineq)
       ,(sigplus `(plus ,(qminus fract) ,(arg2 ineq)))))
    (t
     `(greatereqp
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
