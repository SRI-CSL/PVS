;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-
; ARITH - sigma and solver for linear arith

;; HISTORY 
;; 17-Mar-1994		Natarajan Shankar	
;;    Misc

;;; SO 11/26/90 - now driven off emacs variable *printerpdivide*
;(defvar *uninterp-divide* t)

(declaim (notinline canonsig-arith))

(defvar *use-can* t)

;; Set to t in order to cause the proof:
;;  ~owre/ehdm6/rcp/minimial_v_prf_2--nf_v_sched to go through in ehdm.
;; Otherwise the proof is false.
(defvar *tc-ehdm-test* nil)

(defun canonsig-arith (term &optional (dont-add-use nil))
  (canonsig term dont-add-use))

(defun arithord (u v)  ;;NSH(9.14.01):modified to treat nonlin terms.
    (arithord1 u v))  ;(arithord1 up vp)

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

(defun times-arithord-rep (x)
   (if (qnumberp (cadr x))
      (caddr x)
      (cadr x)))

(defun div-arithord-rep (x)
  (arithord-rep (if (qnumberp (cadr x))(caddr x)(cadr x))))

(defun plus-arithord-rep (x)
  (arithord-rep (if (qnumberp (cadr x)) (caddr x) (cadr x))))

(defun arithord-rep (x) ;;NSH(11.25.02) added sincenormineq could not 
  (if (consp x)      ;;solve a nonlinear identity of the ab = 1/(1+ab).
      (let ((fn (funsym x)))
	(cond ((eq fn 'TIMES) (times-arithord-rep x))
	      ((eq fn 'PLUS)(plus-arithord-rep x))
	      ((eq fn 'DIVIDE) (div-arithord-rep x))
	      (t x)))
      x))

(defun arithord1(u v)
    (cond
     ((null u) nil)
     ((null v) t)
     ((symbolp u)
      (cond
       ((symbolp v)(alphalessp u v))
       ((qnumberp v) nil)
       ((consp v) (if (eq (car v) 'TIMES)  ;;NSH(10-23-01)
		      (let ((v1 (times-arithord-rep v)))
			(and (not (equal u v1))
			     (arithord1 u v1)))
		      (if (eq (car v) 'DIVIDE)
			  (let ((rep (div-arithord-rep v)))
			    (and (not (equal u rep))
				 (arithord1 u rep)))
		      t)))
       (t t)))
     ((symbolp v)
      (or (qnumberp u)
	  (and (consp u)  ;;NSH(10-23-01)
	       (or (and (eq (car u) 'TIMES)
			(let  ((u1 (times-arithord-rep u)))
			  (or (equal u1 v)
			      (arithord1 u1 v))))
		   (and (eq (car u) 'DIVIDE)
			(let ((rep (div-arithord-rep u)))
			    (or (equal rep v)
				(arithord1 rep v))))))))
     ((qnumberp u)
      (cond
       ((qnumberp v)(qlessp u v))
       (t t)))
     ((qnumberp v) nil)
     ((equal (car u) 'TIMES)
      (if (equal (car v) 'TIMES)
	  (arithord1 (cdr u) (cdr v))
	  (arithord1 (times-arithord-rep u) v)))
     ((equal (car u) 'DIVIDE)
      (if (equal (car v) 'DIVIDE)
	  (arithord1 (cdr u)(cdr v))
	  (arithord1 (div-arithord-rep u) v)))
     ((equal (car v) 'TIMES)
      (arithord1 u (times-arithord-rep v)))
     ((equal (car v) 'DIVIDE)
      (arithord1 u (div-arithord-rep v)))
     ((equal (car u)(car v))(arithord1 (cdr u)(cdr v)))
     (t (arithord1 (car u)(car v)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(10.26.01): I'm saving the old arithord for use with make-sum
;;and make-prod in the simplifier (assert.lisp).  In the ground prover,
;;the new arithord deals correctly with nonlinear expressions by ordering
;;them according to the minimal product term.

(defun old-arithord (u v)
  (let ((up (if (and (consp u) (eq (car u) 'TIMES) (qnumberp (cadr u)))
		(caddr u)
	      u))
	(vp (if (and (consp v) (eq (car v) 'TIMES) (qnumberp (cadr v)))
		(caddr v)
	      v)))
;    (let ((a1 (old-arithord1 up vp))
;	  (a2 (old-arithord1 vp up)))
;      (when (and (equal a1 a2) (not (equal up vp))) (break))
;      a1)
    (old-arithord1 up vp)))


(defun old-arithord1(u v)
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
     ((equal (car u)(car v))(old-arithord1 (cdr u)(cdr v)))
     (t (old-arithord1 (car u)(car v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
	       ((cdr product) (cons 'TIMES (sort product 'arithord)))
	       (t (car product))))
	(cond
	 ((equal coef 1) (return product))
	 (t (return `(TIMES ,coef ,product)))))
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
   ((and (eq *printerpdivide* 'yes) (eq (funsym arg) 'DIVIDE))
    (if (member (arg2 arg) product :test #'equal)
	(setq product (remove (arg2 arg) product :test #'equal :count 1))
      (setq product (nconc product (ncons arg)))))
   ((eq (car arg) 'TIMES) (loop for u in (cdr arg) do (buildproduct u)))
   ((eq (car arg) 'PLUS)
    (throw 'fastexit (distrib u)))
   (t (if (and (eq *printerpdivide* 'yes) (member (make-inverse-1 arg) product :test #'equal))
	  (setq product (remove (make-inverse-1 arg) product :test #'equal :count 1))
	(setq product (nconc product (ncons arg)))))))

(defun distrib(u)
  (sigplus
   (cons 
    'PLUS
    (loop for arg in (distrib1 (cdr u) (ncons nil)) collect 
	 (sigtimes (cons 'TIMES arg))))))

(defun distrib1(product sum)
     (cond
      ((null product) sum)
      ((and (consp (car product))(eq (caar product) 'PLUS))
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
	  (setq numer (sigtimes `(TIMES ,numer ,(arg2 denom1)))
		denom (sigtimes `(TIMES ,denom ,(arg1 denom1))))
	(setq denom (sigtimes `(TIMES ,denom ,denom1))))
      (if (or (symbolp denom) (and (consp denom) (eq (funsym denom) 'PLUS)))
	  (sigtimes `(TIMES ,numer ,(make-inverse-1 denom)))
	(progn
	  (make-product-from-numer numer)
	  (make-product-from-denom denom)
	  (setq coef (qnorm coef))
	  (cond
	   (product
	    (setq product
		  (cond
		   ((cdr product) (cons 'TIMES (sort product 'arithord)))
		   (t (car product))))
	    (cond
	     ((equal coef 1) product)
	     (t `(TIMES ,coef ,product))))
	   (t coef)))))))

(defun make-product-from-numer (numer)
  (cond
   ((qnumberp numer)
    (setq coef (qtimes coef numer)))
   ((symbolp numer)
    (setq product (nconc product (ncons numer))))
   ((eq (funsym numer) 'TIMES)
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
   ((eq (funsym denom) 'TIMES)
    (loop for d in (argsof denom) do (make-product-from-denom d)))
   (t (if (member denom product :test #'equal)
	  (setq product (remove denom product :test #'equal :count 1))
	(setq product (nconc product (ncons (make-inverse denom))))))))

(defun make-inverse (denom)
  (if (and (consp denom) (eq (funsym denom) 'DIVIDE))
      (arg2 denom)
    (make-inverse-1 denom)))

(defun make-inverse-1 (denom)
  (if (is-infinity denom)
      0
      `(DIVIDE 1 ,denom)))

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
	  ((cdr result)(cons 'PLUS result))
	  (t (car result))))
	(t `(PLUS ,const ,.result))))
      (t const)))))

(defun buildsum(u)
  (cond
   ((qnumberp u)(setq const (qplus u const)))
   ((symbolp u)(addtosum u 1))
   ((and (eq (car u) 'TIMES)(qnumberp (cadr u)))
    (addtosum (caddr u) (cadr u)))
   ((eq (car u) 'PLUS)(loop for v in (cdr u) do (buildsum v)))
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
       (t (ncons `(TIMES ,(cdr pair) ,(car pair)))))))

(defun sigdifference(u)
  (sigplus `(PLUS ,(cadr u) ,(sigminus1 (caddr u)))))

(defun sigminus(u)(sigminus1 (cadr u)))


(defun sigminus1(u)
  (cond
   ((qnumberp u)(qminus u))
   ((symbolp u) `(TIMES -1 ,u))
   ((and (eq (car u) 'TIMES)(qnumberp (cadr u)))
    (sigtimes `(TIMES ,(qminus (cadr u)) ,.(cddr u))))	; absence of sigtimes caused 'bug1'  MES 8/16/88
   ((eq (car u) 'PLUS)
    (sigplus (cons 'PLUS (mapcar #'sigminus1 (cdr u)))))	; sigplus added but may not be necessary  MES 8/16/88
   (t (sigtimes `(TIMES -1 ,u)))))
;
;(defun sigminus1(u)
;  (cond
;   ((qnumberp u)(qminus u))
;   ((symbolp u) `(TIMES -1 ,u))
;   ((and (eq (car u) 'TIMES)(qnumberp (cadr u)))
;    `(times ,(qminus (cadr u)) ,.(cddr u)))
;   ((eq (car u) 'PLUS)
;    (cons 'plus (mapcar #'sigminus1 (cdr u))))
;   (t `(times -1 ,u))))


; solver for positive predicates over linear arithmetic

(defun arithsolve(lit)
  (case (funsym lit)				
    (equal (equalsolve lit))
    ((lesseqp greatereqp lessp greaterp) (ineqsolve lit))
    (t `((equal ,lit true)))))

(defun reciprocal (x)
  (if (and (consp x)
	   (eq (funsym x) 'DIVIDE)
	   (eql (arg1 x) 1))
      (arg2 x)
      `(DIVIDE 1 ,x)))

(defun cancel-reciprocal (lit divterms)
  (if (consp lit)
      (if (eq (funsym lit) 'PLUS)
	  `(PLUS ,@(cancel-reciprocal-list (argsof lit) divterms))
	  (if (eq (funsym lit) 'TIMES)
	      (cancel-times-reciprocal (argsof lit) divterms)
	      (cancel-times-reciprocal (list lit) divterms)))
      `(TIMES ,lit ,@(loop for x in divterms collect (reciprocal x)))))

(defun cancel-reciprocal-list (lit-list divterms)
  (loop for a in lit-list collect (cancel-reciprocal a divterms)))

(defun cancel-times-reciprocal (factors divterms)
  (let ((result (cancel-times-reciprocal* factors divterms nil)))
    (if (consp result)
	(if (null (cdr result))
	    (car result)
	    `(TIMES ,@result))
	1)))

(defun cancel-times-reciprocal* (factors divterms accum)
  (if (consp factors)
      (if (member (car factors) divterms :test #'equal)
	  (cancel-times-reciprocal* (cdr factors)
				   (remove (car factors) divterms
					   :test #'equal
					   :count 1)
				   accum)
	  (if (and (consp (car factors))
		   (eq (funsym (car factors)) 'TIMES))
	      (cancel-times-reciprocal* (append (argsof (car factors))
						(cdr factors))
					divterms accum)
	  (cancel-times-reciprocal* (cdr factors) divterms
				   (cons (car factors) accum))))
      (nconc (nreverse accum)
	     (loop for x in divterms collect (reciprocal x)))))

(defun common-factors (term)
  (if (consp term)
      (if (eq (car term) 'PLUS)
	  (common-factors-list (cdr term))
	  (if (eq (car term) 'TIMES)
	      (cdr term)))
      (if (eql term 0)
	  nil
	  (list term))))

(defun common-factors-list (terms)
  (if (consp terms)
      (let ((init (common-factors (car terms))))
	(loop for x in init
	      when (loop for tm in terms
			 always (member x (common-factors tm)
					:test #'equal))
	      collect x))
      nil))

(defun divterms* (args accum)
  (if (consp args)
      (let ((a (car args)))
	(if (or (isneqzero? a)
		(and (consp a)
		     (eq (funsym a) 'DIVIDE)))
	    (divterms* (cdr args)  (cons a accum))
	    (divterms* (cdr args)  accum)))
      (nreverse accum)))

(defun list-eq (x y)
  (if (and (consp x)(consp y))
      (if (eq (car x)(car y))
	  (list-eq (cdr x)(cdr y))
	  nil)
      (eq x y)))

(defun drop-nzterm (terms)
  (if (consp terms)
      (if (isneqzero? (car terms))
	  (cdr terms)
	  (cons (car terms)(drop-nzterm (cdr terms))))
      terms))

(defun divterms (args rest-terms)
  (if (consp args)
      (let ((a (car args)))
	(if (and (consp a)(eq (funsym a) 'DIVIDE))
	    (cons a (divterms (cdr args) rest-terms))
	  (if (and (isneqzero? a)
		   (member a (common-factors-list rest-terms)
			   :test #'equal))
	      (cons a (divterms (cdr args) rest-terms))
	      (if (and (consp (cdr args))
		       (equal (cadr args) a))
		  (divterms (cdr args) rest-terms)
		  (divterms* (cdr args) nil)))))
    nil))

;   (let ((dterms (divterms* args factors nil)))
;     (if (list-eq args dterms)
; 	(drop-nzterm dterms)
; 	dterms)))

(defun arithord-list (x terms)
  (if (consp terms)
      (if (qnumberp (car terms))
	  (arithord-list x (cdr terms))
	(and (arithord x (car terms))
	     (arithord-list x (cdr terms))))
    t))

(defun equalsolve (lit)
  (let ((norm (normineq lit)))
    (if (and t (consp norm))
	(let* ((all-terms (termsof norm))
	       (head (car all-terms)))
	  (if (and (consp head)
		   (or (eq (funsym head) 'TIMES)
		       (eq (funsym head) 'DIVIDE)))
	      (let* ((args (if (eq (funsym head) 'TIMES)
			       (argsof head)
			     (ncons head)))
		     (divterms (divterms args (cdr all-terms))) ;(common-factors (arg2 norm))))
		     (newhead (when divterms
				  (cancel-reciprocal head divterms)))
				  ;head
		     (check (and divterms
				 (or (arithord (car divterms) newhead)
				     (arithord-list newhead (cdr all-terms)))))
		     (newarg2 (when check
				   (cancel-reciprocal (arg2 norm) divterms)))
				   ;(arg2 norm)
		     (newlits (when check
				  (solvecan `(equal ,newhead ,newarg2)))))
		(if check
		    newlits
; 		    (if (and (singleton? newlits)
; 			     (eq (car newlits) 'true))
; 			newlits
; 			(cons norm newlits))
		    (ncons norm)
		    ;;(list `(lesseqp ,(arg1 norm) ,(arg2 norm))
			;;  `(greatereqp ,(arg1 norm) ,(arg2 norm)))
		    ))
	    (if (onlyoccurrencep head head all-terms t)
		(ncons norm)
	      (list `(lesseqp ,(arg1 norm) ,(arg2 norm))
		    `(greatereqp ,(arg1 norm) ,(arg2 norm))))))
      (ncons norm))))

(defun termsof (lit)
  (cons (arg1 lit)
	(if (and (consp (arg2 lit)) (eq (funsym (arg2 lit))
					`PLUS))
	    (argsof (arg2 lit))
	  (ncons (arg2 lit)))))

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
      ((eq (setq res (newcontext (process lit))) 'false) *truecons*) 
      ((eq res 'true) (retfalse))
      (t (let ((norm (normineq lit)))
	   (cond ((eq norm 'ident) (retfalse))
		 ((eq norm true) (retfalse))
		 ((eq norm false) *truecons*)
		 (t (let ((eqlits (equalsolve lit)))
		      (if (singleton? eqlits)
			  `((nequal ,(arg1 (car eqlits)) ,(arg2 (car eqlits))))
			`((nequal ,(arg1 norm) ,(arg2 norm)))))))))))))

(defun isneqzero? (x)
  (let ((ux (use x)))
    (loop for u in ux
	  thereis (and (consp u)
		       (consp (car u))
		       (consp (cdr u))
		       (equal (caar u) x)
		       (equal (cdar u) 0)
		       (equal (cadr u) x)))))

(defun quotients (expr accum)
  (if (consp expr)
      (if (or (eq (funsym expr) 'PLUS)(eq (funsym expr) 'TIMES))
	  (quotients-list (cdr expr) accum)
	  (if (eq (funsym expr) 'DIVIDE)
	      (cons (arg2 expr) accum)
	      nil))
      accum))

(defun quotients-list (list accum)
  (if (consp list)
      (quotients-list (cdr list) (quotients (car list) accum))
      accum))
	      

; returns negation of inequality

(defun isineq? (ineq)
  (and (consp ineq)
       (memq (funsym ineq) *arithrels*)))

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
      ((eq norm 'true) *truecons*)
      ((eq norm 'false) (retfalse))
      ((eq norm 'ident) *truecons*)
      ((eq (setq res (newcontext (process1 (ncons (negineq norm))))) 'false)
       *truecons*) ;dac 8-28-91: used to be true, but process1 could have returned false
		      ; due to using a recently generated pr-union in pr-merge
                      ; but would have retruned true if it didn't use that pr-union.
                      ; thus this is safer as the contradiction will be found later.
      ((eq res 'true) (retfalse))
      (t (ncons norm))))))


; asserts inequality ineq

(defun addineq(ineq)
  (unless (eq (pr-find ineq) 'true)
    (prog(ineqpot)
      (transclosure ineq)	; perform transitive closure
      (addineq2pot ineqpot)
      ;;was (setq s (append ineqpot s))
      )))	; add result to s

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
				(sigplus `(PLUS -1 ,(arg2 ineq1)))
				(arg2 ineq1))))
	    (lesseqp (setq ub (arg2 ineq1)))
	    (greaterp (setq lb (if (eq (prtype (arg2 ineq1)) 'integer)
				   (sigplus `(PLUS 1 ,(arg2 ineq1)))
				   (arg2 ineq1))))
	    (greatereqp (setq lb (arg2 ineq1))))
      (case (funsym ineq2)
	    (lessp (setq ub (if (eq (prtype (arg2 ineq2)) 'integer)
				(sigplus `(PLUS -1 ,(arg2 ineq2)))
				(arg2 ineq2))))
	    (lesseqp (setq ub (arg2 ineq2)))
	    (greaterp (setq lb (if (eq (prtype (arg2 ineq2)) 'integer)
				   (sigplus `(PLUS 1 ,(arg2 ineq2)))
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
		collect `(equal ,var ,(sigplus `(PLUS ,i ,lb)))))))

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

(defun normineqcan (ineq)
  (normineq `(,(funsym ineq)
	      ,(arithcan (arg1 ineq))
	      ,(arg2 ineq))))

(defun normalize-new-eqn (eqn)
  (normineq `(equal ,(canonsig-arith (arg1 eqn))
		    ,(canonsig-arith (arg2 eqn)))))

(defun transclosure(ineq)		;(break)
;   (when (not (ordered-ineq? ineq)) (break "transclosure"))
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

	     (loop for chainineq in (chainineqs ineq) do ;;with (norm) do
		   (let* ((chainineq `(,(funsym chainineq)
				      ,(arg1 chainineq)
				      ,(arithcan (arg2 chainineq))))
			  (norm (normineq (residue ineq chainineq))))
		   (cond
		    ((eq norm 'true)
		     (add-disjunct-of-equals ineq chainineq))
		    ((eq norm 'false)(retfalse))
		    ((eq norm 'ident) 
		     (let ((new-eqn
			    (if *tc-ehdm-test*
				`(equal ,(arg1 ineq) ,(arg2 ineq))
				 (normineq `(equal ,(arg1 ineq) ,(arg2 ineq)))
; 					     ,(canonsig-arith (arg1 ineq))
; 						  ,(canonsig-arith (arg2 ineq))
				 )))
		       (unless (or (not (consp new-eqn));;NSH(9/10/02)
				   (if *tc-ehdm-test*
				       (subtermof (arg1 ineq) (arg2 ineq))
				     (bad-eqn new-eqn)))
			 (loop for eqn in (equalsolve new-eqn)
			       do (push eqn ineqpot))
			 (return nil))))
		    (t (push norm ineqpot)))))
	     ;;; 11/17/92: DAC see note above about nrmineq.
	     ;;; NSH(1/15/94): Loops without WHEN clause below.  DAC's note
	     ;;; indicates why it might be needed.  
	     
	     (when nrmineq-unchanged ;(and (consp nrmineq)(eq (car nrmineq) 'equal))
	       (push nrmineq ineqpot)
	       )
	     ))))

(defun bad-eqn (eqn)
  (or (eq eqn 'ident)
      (and (consp eqn)
	   (or (subtermoflambda (arg1 eqn) (arg2 eqn))
	       (let* ((all-terms (termsof eqn))
		      (head (car all-terms)))
		 (if (not (and (consp head)
			       (eq (funsym head) 'TIMES)))
		     (not (onlyoccurrencep head head all-terms t))
		   nil))))))

(defun subtermoflambda (subterm term)
  (and (consp term)
       (if (eq (funsym term) 'lambda)
	   (subtermof subterm term)
	 (loop for a in term thereis (subtermoflambda subterm a)))))


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

;;added this because chain-square-ineq was only catching
;;squares of the form (times x x) and missing (times x x y y).
(defun square-list? (args) ;;NSH(6-13-02) assumes args is canonical
  (if (and (consp args)(consp (cdr args)))
      (and (equal (car args)(cadr args))
	   (square-list? (cddr args)))
      (null args)))

(defun chain-square-ineq (ineq)
  (when (and (or (eq (funsym ineq) 'lesseqp) (eq (funsym ineq) 'lessp))
	     (consp (arg1 ineq))
	     (eq (funsym (arg1 ineq)) 'TIMES)
	     (square-list? (cdr (arg1 ineq))))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(1.15.94): Added WHEN clause in front of push of nrmineq.  This is needed
;;to prevent loops where increasingly large inequalities are introduced.
;;DAC(1/28/94): Added nrmineq-unchanged and pushed more often.
;;NSH(10.15.02): added preprocessing integercut to eliminate integer
;;incompleteness: 3f<n => 3f <= n-1.
(defun normineq(lit)
  (let* ((ilit (integercut lit))  
	 (result (if (consp ilit)  
		     (integercut (normineq1 ilit))
		     ilit)))
    (if (and (eq result 'ident)
	     (not (eq lit ilit)))
	'true
	result)))
  
; normalization other than Gomery cut

;;; DAC 10-26-90 Added EXP to PRARITHERROR. (now *printerpmult* - SO 5/26/91)
;;; This is experimental mode.
;;; It first treats multiplication interpretted as before
;;; meaning it does not treat products as independant variables.
;;; If it finds that it cannot solve the equation this way
;;; it tries again, but treating products as independent variables.
;;; by letting *kmr-mut* to be t.
;;; If *jmr-mult* is originally t then products are alwys treated independently.

(defvar *jmr-mult* t) ; if t then treat mult as uninterp as much as plausible.

(defun normineq1 (ineq)
  (let ((norm (normineq2 ineq)))
    (if (consp norm)
	(make-strict norm)
      norm)))

(defun normineq2(lit)
  (prog(var coef dif fnsymbol)
       (setq fnsymbol (funsym lit))
       (setq dif (sigdifference `(difference ,(arg1 lit) ,(arg2 lit))))
       ;(break)
       (return
	(cond
	 ((qnumberp dif)(normconstdif fnsymbol dif)) ; constant dif
	 ((solvable dif nil)
	  (list
	   (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
	   var
;;;	(canon `(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif)))
;;;   fix to get around the "order bug"  18-Nov-85
	   (if *use-can*
	       (arithcan
	    ;`(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif))
	    ;; DAC fix to make sure var is cancelled in resulting expression.
		`(TIMES ,(qnorm (qquotient -1 coef))
			,(sigplus `(PLUS ,(sigtimes `(TIMES ,(qtimes -1 coef) ,var)) ,dif))))
	     (sigma
	    ;`(plus ,var (times ,(qnorm (qquotient -1 coef)) ,dif))
	    ;; DAC fix to make sure var is cancelled in resulting expression.
	      `(TIMES ,(qnorm (qquotient -1 coef))
		      ,(sigplus `(PLUS ,(sigtimes `(TIMES ,(qtimes -1 coef) ,var)) ,dif)))))
	   ))
	 ((eq *printerpmult* 'normal) lit)
	 ((eq *printerpmult* 'experimental)
	  (let ((*jmr-mult* t))
	    (if (solvable dif (and t (eq fnsymbol 'equal)))
		(list
		 (cond ((qminusp coef)(antifnsym fnsymbol))(t fnsymbol))
		 var
		 (arithcan 
		  `(TIMES ,(qnorm (qquotient -1 coef))
			  ,(sigplus `(PLUS ,(sigtimes `(TIMES ,(qtimes -1 coef) ,var)) ,dif))))
		 )
	      lit)))
	 (t (prerr "Formula falls outside domain of completeness"))))))

(defun check-strict?(ineq)
  (check-strict-rec (equalsolve `(equal ,(arg1 ineq) ,(arg2 ineq)))))

(defun check-strict-rec (eqns)
  (if (consp eqns)
      (if (eq (car eqns) 'false)
	  'false
	(or (and (consp (car eqns))
	         (eq (funsym (car eqns)) 'equal)
		 (strict? (car eqns)))
	    (check-strict-rec (cdr eqns))))
    nil))

(defun make-strict (ineq)
  (case (funsym ineq)
    (equal (if (strict? ineq) false ineq))
    (lesseqp (if (or (strict? ineq)(check-strict? ineq))
		 `(lessp ,(arg1 ineq) ,(arg2 ineq))
	       ineq))
    (greatereqp (if (or (strict? ineq)(check-strict? ineq))
		    `(greaterp ,(arg1 ineq) ,(arg2 ineq))
		  ineq))
    (t ineq)))

;;was
; (defun make-strict (ineq)
;   (case (funsym ineq)
;     (equal (if (strict? ineq) false ineq))
;     (lesseqp (if (strict? ineq)
; 		 `(lessp ,(arg1 ineq) ,(arg2 ineq))
; 	       ineq))
;     (greatereqp (if (strict? ineq)
; 		    `(greaterp ,(arg1 ineq) ,(arg2 ineq))
; 		  ineq))
;     (t ineq)))

(defun strict?(ineq)
  (loop for u in (use (arg1 ineq))
        thereis (and (consp u)
		     (consp (car u))
		     (equal (arg1 ineq) (caar u))
		     (equal (arg2 ineq) (cdar u)))))

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
     (t (pr-find term) )))) ;;NSH(9.30.02) added pr-find


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
      (t (setq var dif coef 1) t)))
   (t (setq var dif coef 1) t)))

; solvable for case in which dif is a plus expression

(defun solvableplus(dif &optional (funsymis= nil))
  (loop for term in (argsof dif)
	thereis (and (not (qnumberp term))
		     (cond
		       ((linearp term)
			(setq var (varof term) coef (coefof term))
			(onlyoccurrencep var term dif funsymis=))))))

; solvable for case in which dif is a times expression

(defun solvabletimes(dif)
  (cond
   ((linearp dif)(setq var (varof dif) coef (coefof dif)) t)))

; returns t if term has no nonlinear use of times

;;; 18-Sept-90 DAC: From arith.lisp to change mult to uninterp as far as solving eqns.

(defun linearp (term)
  (or *jmr-mult*
      (linear? term)))

(defun linear? (term)
  (cond
       ((and (listp term)(eq (funsym term) 'TIMES))
	(and (qnumberp (arg1 term))
	     (linearp (arg2 term))))
       (t t)))

; returns t if only occurrence of var in plus expr l is in term t

(defun onlyoccurrencep(var term l &optional (funsymis= nil))
  (loop for arg in (argsof l)
	never (and (not (equal arg term))
		   (occursin var arg funsymis=))))

; returns t if var is a variable of term

;;; davesc (fix from shankar), for bug # 524
(defun occursin (var term &optional (funsymis= nil))
  (cond
   ((and (listp term)(eq (funsym term) 'TIMES))
    (cond
     ((qnumberp (arg1 term))(occursin var (arg2 term) funsymis=))
     (t (if *jmr-mult*
            (or (equal var term)
                    (member-or-div-member var (argsof term)))
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
	   (eq (funsym y) 'DIVIDE)
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
       ,(sigplus `(PLUS -1 ,(arg2 ineq)))))
    (greaterp
     `(greatereqp
       ,(arg1 ineq)
       ,(sigplus `(PLUS 1 ,(arg2 ineq)))))
    (t ineq)))

; performs cut for negative fractional part

(defun minuscut(ineq fract)
  (case (funsym ineq)	
    (equal 'false)
    ((lessp lesseqp)
     `(lesseqp
       ,(arg1 ineq)
       ,(sigplus `(PLUS ,(qminus (qplus fract 1)) ,(arg2 ineq)))))
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
       ,(sigplus `(PLUS ,(qminus fract) ,(arg2 ineq)))))
    (t
     `(greatereqp
       ,(arg1 ineq)
       ,(sigplus `(PLUS ,(qdifference 1 fract) ,(arg2 ineq)))))))

; returns the fractional part of expression l if one can be determined
; or nil otherwise

(defun fractpt(l)
  (cond
   ((eq (prtype l) 'integer) 0)
   ((symbolp l) nil)
   ((qnumberp l)(qfractpt l))
   (t
    (case (funsym l)	
      (PLUS (plusfractpt l))
      (TIMES (timesfractpt l))
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
