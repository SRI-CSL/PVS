;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arithmetic.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:55:04
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defvar *printerpdivide* t "Is division interpreted or not?")

;; Infinity

(defun make-infinity ()
  "gensyms an infinity symbol to be used for zero denominator."
  (mk-constant (gensym "*-infinity")))

(defun is-infinity (term)
  (and (constant-p term)
       (is-infinity-sym (constant-id term))))

(defun is-infinity-sym (sym)
  "checks if sym is a gensym'd infinity symbol."
  (and (symbolp sym)
       (> (length (string sym)) 10)
       (string= "*-infinity" (subseq (string sym) 0 10))))


;; Canonization

(defun sigarith (term cs)
  (declare (type node term)
	   (type cong-state cs))
  (cond ((dp-numberp term) term)
	((plus-p term) (sigplus term))
	((times-p term) (sigtimes term))
	((difference-p term) (sigdifference term))
	((minus-p term) (sigminus term))
	((divide-p term) (sigdivide term))
	((floor-p term) (sigfloor term cs))
	(t term)))


(defun termsof (expr)  ;;;expr must be arithmetic.
  "returns monomials of a polynomial"
  (if (plus-p expr)
      (funargs expr)
      (list expr)))

(defmacro alphalessp (a b)
  "string compares a, b."
  `(string< (string ,a) (string ,b)))

(defun monom-< (u v)
  "compares monomials using arith-term-< (ignores times and coefficient)"
  (let ((up (if (and (times-p u) (dp-numberp (arg 1 u))) (arg 2 u) u))
	(vp (if (and (times-p v) (dp-numberp (arg 1 v))) (arg 2 v) v)))
    (arith-term-< up vp)))

;;arith-term-< puts an ordering on arithmetic terms
;;(uninterpreted terms, symbols, numbers, or their products)
;;so that the sum of monomials will be ordered from
;;smallest monomials to largest monomial.
;;Also, when solving for a variable, the solver
;;will pick the smallest non-numeric term to solve for.
;;The ordering is numbers < symbols < uninterpreted terms < products.

(defun times-< (u v)
  (application-< u v))

(defun application-< (u v)
  (loop with ua = (arity u)
	with va = (arity v)
	for i from 0
	when (> i ua) return nil
	when (> i va) return t
	when (monom-< (arg i u) (arg i v)) return t
	unless (eq (arg i u) (arg i v)) return nil))

(defun arith-term-< (u v)
  (cond ((dp-numberp u)
	 (and (dp-numberp v)
	      (< (constant-id u) (constant-id v))))
	((dp-numberp v) t)
	((dp-variable-p u)
	 (and (dp-variable-p v)
	      (string< (constant-id u) (constant-id v))))
	((dp-variable-p v) t)
	((constant-p u)
	 (and (constant-p v)
	      (string< (constant-id u)
		       (constant-id v))))
	((constant-p v) t)
	((uninterp? u) (if (uninterp? v)
			   (application-< u v)
			   t))
	((uninterp? v) nil)
	((floor-p u) (if (floor-p v)
			 (application-< u v)
			 t))
	((floor-p v) nil)
	((times-p u) (if (times-p v)
			 (times-< u v)
			 t))
	((times-p v) nil)
	(t (application-< u v))))


(defun sigtimes(u)
  "canonizer for times:  returns a sum of monomials where each monomial
   is sorted according to arith-term-<.  Input assumption is that subterms of
   u have been canonized."
  (let ((product nil)
	(coef 1))
    (labels
	((buildproduct (arg)
	  ;;side-effects product and coeff,
	  ;;throws 0 on 0 or (distrib u) on plus.
	  ;;mainly builds a list of monomials corresponding to the
	  ;;argument of times.
	  ;;optionally interpret division carrying out cancellation.
	  (cond ((dp-numberp arg)
		 (if (zero-p arg)
		     (throw 'fastexit *zero*)
		     (setq coef (* (constant-id arg) coef))))
		((and *printerpdivide* (divide-p arg))
		 (if (member (denom arg) product :test #'eq)
		     (setq product (remove (denom arg) product
					   :test #'eq :count 1))
		     (setq product (nconc product (list arg)))))
		((times-p arg) (map-funargs #'buildproduct arg))
		((plus-p arg) (throw 'fastexit (distrib u)))
		(t (if (and *printerpdivide*
			    (member (make-inverse-1 arg) product :test #'eq))
		       (setq product (remove (make-inverse-1 arg) product
					     :test #'eq :count 1))
		       (setq product (nconc product (list arg))))))))
      (catch 'fastexit
	(map-funargs #'buildproduct u)
	(let ((return-product
	       (and product
		    (if (cdr product)
			(mk-times (sort product 'monom-<))
			(car product)))))
	  (if return-product
	      (if (= coef 1)
		  return-product
		  (mk-times (list (mk-constant coef) return-product)))
	      (mk-constant coef)))))))

(defun distrib (u)
  "used by buildproduct to distribute times over plus."
  (sigplus (mk-plus (loop for arg in (distrib1 (funargs u) (list nil))
			  collect (sigtimes (mk-times arg))))))

(defun distrib1 (product sum)
  (if (null product) sum
      (let ((cur-prodarg (car product))
	    (rest-sum (distrib1 (cdr product) sum)))
	(if (plus-p cur-prodarg)
	    (loop for arg in (funargs cur-prodarg)
		  nconc (distrib2 arg rest-sum))
	    (distrib2 cur-prodarg rest-sum)))))

(defun distrib2 (term product)
  (loop for prodarg in product
	collect (cons term prodarg)))

(defun sigdivide (u)
  "canonizer for division: subterms of input assumed canonized,
   returns sum of monomials with denominator expressed as reciprocal.
   does cancellation (on variables) and distribution."
  (if (not *printerpdivide*) u
      (let ((coef 1)
	    (product nil)
	    (numer1 (numer u))
	    (denom1 (denom u))
	    (numer *one*)
	    (denom *one*))
	(if (divide-p numer1)
	    (setq numer (numer numer1)
		  denom (denom numer1))
	    (setq numer numer1))
	(if (divide-p denom1)
	    (setq numer (sigtimes (mk-times `(,numer ,(denom denom1))))
		  denom (sigtimes (mk-times `(,denom ,(numer denom1)))))
	    (setq denom (sigtimes (mk-times `(,denom ,denom1)))))
	(if (or (and (constant-p denom) (not (dp-numberp denom)))
		(plus-p denom))
	    (sigtimes (mk-times `(,numer ,(make-inverse-1 denom))))
	    (labels
		((make-product-from-numer
		  (numer)
		  (cond
		   ((dp-numberp numer)
		    (setq coef (* coef (constant-id numer))))
		   ((constant-p numer)
		    (setq product (nconc product (list numer))))
		   ((times-p numer)
		    (if (dp-numberp (coef numer))
			(prog ()
			  (setq coef (* coef (constant-id (coef numer))))
			  (make-product-from-numer (arg 2 numer)))
			(setq product
			      (nconc product (copy-list (funargs numer))))))
		   (t (setq product (nconc product (list numer))))))
		 (make-product-from-denom
		  (denom)
		  (cond
		   ((dp-numberp denom)
		    (if (zero-p denom)
			(make-product-from-denom (make-infinity))
			(setq coef (/ coef (constant-id denom)))))
		   ((constant-p denom)
		    (if (member denom product :test #'eq)
			(setq product (remove denom product :count 1
					      :test #'eq))
			(setq product (nconc product
					     (list (make-inverse-1 denom))))))
		   ((times-p denom)
		    (map-funargs #'make-product-from-denom
				 denom))
		   (t (if (member denom product :test #'eq)
			  (setq product (remove denom product :test #'eq
						:count 1))
			  (setq product
				(nconc product
				       (list (make-inverse denom)))))))))
	      (make-product-from-numer numer)
	      (make-product-from-denom denom)
	      (cond
	       (product
		(setq product
		      (cond
		       ((cdr product)
			(sigtimes (mk-times (sort product 'monom-<))))
		       (t (car product))))
		(cond
		 ((equal coef 1) product)
		 (t (sigtimes (mk-times `(,(mk-constant coef) ,product))))))
	       (t (mk-constant coef))))))))

(defun make-inverse (denom)
  "makes reciprocal"
  (if (divide-p denom)
      (denom denom)
      (make-inverse-1 denom)))

(defun make-inverse-1 (denom)
  (if (is-infinity denom)
      *zero*
      (mk-divide *one* denom)))

(defun sigplus (u)
  "Canonizer for plus: returns sum of products with like monomials merged,
   sorted, cancelled, and numbers up front." 
  (let ((sum (list nil))
	(const 0)
	(result nil))
    (labels ((addtosum (u coef)
	       (loop for ptr on sum
		     when (null (cdr ptr)) ; couldn't find u. Enter it at end.
		       return (setf (cdr ptr) (list (cons u coef)))
		     when (eq (caadr ptr) u)  ; found it
		        return (setf (cdr (cadr ptr)) (+ (cdadr ptr) coef))
		     when (monom-< u (caadr ptr))  ; Splice it in here.
		        return (setf (cdr ptr) (cons (cons u coef)
						     (cdr ptr)))))
	     (buildsum (u)
	       (cond ((dp-numberp u)
		      (setq const (+ (constant-id u) const)))
		     ((constant-p u)
		      (addtosum u 1))
		     ((and (times-p u)
			   (dp-numberp (coef u)))
		      (addtosum (arg 2 u) (constant-id (coef u))))
		     ((plus-p u)
		      (map-funargs #'buildsum u))
		     (t (addtosum u 1)))))
      (map-funargs #'buildsum u)
      (setq result (collectsum (cdr sum)))
      (cond (result
	     (cond ((= const 0)
		    (cond
		     ((cdr result) (mk-plus result))
		     (t (car result))))
		   (t (mk-plus `(,(mk-constant const) ,.result)))))
	    (t (mk-constant const))))))

(defun collectsum(s)
  "Builds a list of products given a list of terms . coefs."
  (loop for pair in s nconc
	(cond ((zerop (cdr pair)) nil)
	      ((= (cdr pair) 1) (list (car pair)))
	      (t (list (mk-times `(,(mk-constant (cdr pair)) ,(car pair))))))))

(defun sigdifference (u)
  "Canonizer for difference: subterms are already canonized,
   Converts a - b into a + (times -1 b)"
  (sigplus (mk-plus `(,(arg 1 u) ,(sigminus1 (arg 2 u))))))

(defun sigminus(u)
  (sigminus1 (arg 1 u)))

(defun sigminus1 (u)
  (cond ((dp-numberp u) (mk-constant (- (constant-id u))))
	((constant-p u) (mk-times `(,*neg-one* ,u)))
	((and (times-p u) (dp-numberp (coef u)))
	 (sigtimes (mk-times `(,(mk-constant (- (constant-id (coef u))))
			       ,(arg 2 u)))))
	((plus-p u)
	 (sigplus (mk-plus (map-funargs-list #'sigminus1 u))))
	(t (sigtimes (mk-times `(,*neg-one* ,u))))))

(defun sigfloor (u cs)
  (multiple-value-bind (number-args integer-args)
      (split-args-num-int (arg 1 u) cs)
    (cond ((and number-args integer-args)
	   (sigplus (mk-plus (cons (mk-floor (sigplus (mk-plus number-args)))
				   integer-args))))
	  (number-args
	   (mk-floor (sigplus (mk-plus number-args))))
	  (integer-args
	   (sigplus (mk-plus integer-args)))
	  (t *zero*))))

(defun split-args-num-int (arg cs)
  (let ((args (cond ((plus-p arg)
		     (funargs arg))
		    ((dp-numberp arg)
		     (list (mk-constant (floor (constant-id arg)))))
		    (t (list arg)))))
    (loop with integer-args = nil
	  with number-args = nil
	  with const = 0
	  for a in args
	  for integer? = (dp-integer-atom-p a cs)
	  do (cond
	      ((dp-numberp a) (setq const (+ (constant-id a) const)))
	      (integer?
	       (setq integer-args (cons a integer-args)))
	      (t (setq number-args (cons a number-args))))
	  finally (return
		   (cond
		    ((zerop const)
		     (values number-args integer-args))
		    ((integerp const)
		     (values number-args (cons (mk-constant const)
					       integer-args)))
		    ((minusp const)
		     (values (cons (mk-constant (num-fractpt const))
				   number-args)
			     (cons (mk-constant (ceiling const))
				   integer-args)))
		    (t
		     (values (cons (mk-constant (num-fractpt const))
				   number-args)
			     (cons (mk-constant (floor const))
				   integer-args))))))))

;;inverts a -1. 
(defmacro neg-sgn (sgn)
  `(* -1 ,sgn))

(defun term-sgn (term)
  "Sign of the coefficient of the term.
   Returns -1, 0, or +1 depending on whether coeff is neg, zero, or pos."
  (cond ((dp-numberp term)
	 (cond
	  ((eq term *zero*) 0)
	  ((> (constant-id term) 0) 1)
	  (t -1)))
	((constant-p term) 1)
	((times-p term) (term-sgn (coef term)))
	(t 1)))

(defun negineq(ineq)
  "Inverts the inequality relation."
  (mk-ineq (case (constant-id (funsym ineq))
	     (LESSEQP *GREATERP*)
	     (LESSP   *GREATEREQP*)
	     (GREATERP *LESSEQP*)
	     (GREATEREQP *LESSP*))
	   (arg 1 ineq)
	   (arg 2 ineq)))

(defun ineq-coef (ineq)
  "Returns sign -1 for lessp, lesseqp, 1 for greaterp, greatereqp, 0 for equal."
  (case (constant-id (funsym ineq))
    ((lessp lesseqp) -1)
    ((greaterp greatereqp) 1)
    (t 0)))

(defun normineq (ineq cs &optional (var nil))
  "Canonizer for arithmetic equalities and inequalities.
   Assumes that the arguments of ineq are canonical.
   Solves for either the smallest (arith-term-<) term or for var if non-nil."
  (integercut (normineq-first ineq cs var) cs))

(defun normineq-first (ineq cs var)
  "canonizes inequalities a {<, <=, >, >=, =} b by
   canonizing a - b and picking the head term as the lhs of
   the normalized ineq."
  (let ((ineq-pred (funsym ineq))
	(dif (sigdifference (mk-difference (lhs ineq)
					   (rhs ineq)))))
    (if (plus-p dif)
	(let* ((lcm (mk-constant (apply #'lcm (plus-denoms dif))))
	       (lcm-normed-diff (sigtimes (mk-times (list lcm dif)))))
	  (mk-diff-into-ineq ineq-pred lcm-normed-diff cs var))
	(mk-diff-into-ineq ineq-pred dif cs var))))

(defun mk-diff-into-ineq (ineq-pred dif cs var)
  "Converts normalized difference into normalized inequality."
  (let ((sign-type (sign-type dif cs)))
    (cond (sign-type
	   (mk-sign-type-into-ineq ineq-pred sign-type dif cs var))
	  ((dp-numberp dif)
	   (mk-const-ineq ineq-pred dif))
	  (t (mk-no-sign-type-into-ineq ineq-pred dif cs var)))))

(defun mk-no-sign-type-into-ineq (ineq-pred dif cs var)
  (multiple-value-bind (head-term rest-dif)
      (pick-head-term dif var)
    (mk-norm-ineq ineq-pred head-term rest-dif cs)))

(defun sign-type (term cs)
  (cond ((dp-numberp term)
	 (number-sign-type term))
	((constant-p term)
	 (constant-sign-type term cs))
	((times-p term)
	 (times-sign-type term cs))
	((plus-p term)
	 (plus-sign-type term cs))
	(t (nonarith-sign-type term cs))))

(defun nonarith-sign-type (term cs)
  (dp-type-to-sign-type (dp-type term cs)))

(defun number-sign-type (term)
  (cond ((zero-p term) 'zero)
	((dp-minusp term) 'strict-nonpos)
	(t 'strict-nonneg)))

(defun dp-type-to-sign-type (dp-type)
  (case dp-type
    ((nonneg nonpos zero strict-nonneg strict-nonpos) dp-type)
    (strict 'strict-nonneg)
    (t nil)))

(defun constant-sign-type (term cs)
  (let ((dp-type (dp-type term cs)))
    (dp-type-to-sign-type dp-type)))

(defun times-sign-type (term cs)
  (let* ((coef (term-coef term))
	 (monom (term-var term))
	 (monom-sign (monom-sign-type monom cs)))
    (if (and monom-sign (dp-minusp coef))
	(neg-sign-type monom-sign)
	monom-sign)))

(defun neg-sign-type (sign-type)
  (case sign-type
    (nonneg 'strict-nonpos)
    (strict-nonneg 'nonpos)
    (nonpos 'strict-nonneg)
    (strict-nonpos 'nonneg)
    (zero 'nonzero)
    (nonzero 'zero)))

(defun monom-sign-type (monom cs)
  (if (constant-p monom)
      (constant-sign-type monom cs)
      (dp-type-to-sign-type (dp-type monom cs))))

(defun plus-sign-type (plus cs)
  (let ((dp-type (dp-type plus cs)))
    (if dp-type
	(dp-type-to-sign-type dp-type)
	(let ((result (reduce #'sign-plus
			      (map-funargs-list #'sign-type plus cs))))
	  (if result
	      (let ((*dp-changed* *dp-changed*))
		(setf (dp-type plus cs) result))
	      result)))))

(defun sign-plus (sign1 sign2)
  (and sign1 sign2
       (case sign1
	 (nonneg (nonneg-plus sign2))
	 (strict-nonneg (strict-nonneg-plus sign2))
	 (nonpos (nonpos-plus sign2))
	 (strict-nonpos (strict-nonpos-plus sign2))
	 (zero (zero-plus sign2))
	 (nonzero (nonzero-plus sign2)))))

(defun nonneg-plus (sign)
  (case sign
    (nonneg 'nonneg)
    (strict-nonneg 'strict-nonneg)
    (nonpos nil)
    (strict-nonpos nil)
    (zero 'nonneg)
    (nonzero nil)))

(defun strict-nonneg-plus (sign)
  (case sign
    (nonneg 'strict-nonneg)
    (strict-nonneg 'strict-nonneg)
    (nonpos nil)
    (strict-nonpos nil)
    (zero 'stric-nonneg)
    (nonzero nil)))
	 
(defun nonpos-plus (sign)
  (case sign
    (nonneg nil)
    (strict-nonneg nil)
    (nonpos 'nonpos)
    (strict-nonpos 'nonpos)
    (zero 'nonpos)
    (nonzero nil)))

(defun strict-nonpos-plus (sign)
  (case sign
    (nonneg nil)
    (strict-nonneg nil)
    (nonpos 'strict-nonpos)
    (strict-nonpos 'nonpos)
    (zero 'strict-nonpos)
    (nonzero nil)))

(defun zero-plus (sign)
  sign)

(defun nonzero-plus (sign)
  nil)

(defun mk-sign-type-into-ineq (ineq-pred sign-type dif cs var)
  (cond ((eq sign-type 'zero)
	 (case (constant-id ineq-pred)
	   (= *true*)
	   ((LESSEQP GREATEREQP) *true*)
	   (t *false*)))
	((eq sign-type 'nonpos)
	 (case (constant-id ineq-pred)
	   ((LESSP LESSEQP) *true*)
	   (= (mk-no-sign-type-into-ineq ineq-pred dif cs var))
	   (t  *false*)))
	((eq sign-type 'strict-nonpos)
	 (case (constant-id ineq-pred)
	   ((LESSP LESSEQP) *true*)
	   (LESSEQP (mk-no-sign-type-into-ineq *lessp* dif cs var))
	   (t  *false*)))
	((eq sign-type 'nonneg)
	 (case (constant-id ineq-pred)
	   ((GREATERP GREATEREQP) *true*)
	   (= (mk-no-sign-type-into-ineq ineq-pred dif cs var))
	   (t *false*)))
	(t (case (constant-id ineq-pred)
	     ((GREATERP GREATEREQP)*true*)
	     (GREATEREQP (mk-no-sign-type-into-ineq *greaterp* dif cs var))
	     (t *false*)))))

(defun pick-head-term (diff var)
  "Used by mk-diff-into-ineq to pick head term of a normalized difference.
   Returns two values head-term rest-dif s.t. diff = head-rest + rest-dif."
  (cond ((plus-p diff)
	 (loop with front-rest = nil
	       for rest on (funargs diff)
	       for head-rest = (car rest)
	       do
	       (if (and (not (dp-numberp head-rest))
			(or (null var)
			    (eq var (term-var head-rest))))
		   (return (values head-rest
				   (sigplus
				    (mk-plus (nconc front-rest (cdr rest))))))
		   (setq front-rest (nconc front-rest (list head-rest))))
	       finally (pick-head-term diff nil)))
	(t (values diff *zero*))))

(defun mk-const-ineq (ineq-pred dif)
  "if the normalized difference is a constant, then mk-const-ineq
   constructs the appropriate truth value corresponding to the original ineq."
  (cond ((zero-p dif)
	 (case (constant-id ineq-pred)
	   (EQUAL *true*)
	   ((LESSEQP GREATEREQP) *true*)
	   (t *false*)))
	((dp-minusp dif)
	 (case (constant-id ineq-pred)
	   ((LESSP LESSEQP) *true*)
	   (t  *false*)))
	(t (case (constant-id ineq-pred)
	     ((GREATERP GREATEREQP) *true*)
	     (t *false*)))))

(defun mk-norm-ineq (ineq-pred head tail cs)
  "if the normalized difference is not a number, then moves the head term
   to the left of the normalized inequality."
  (let* ((integered-ineq (mk-ineq ineq-pred head
				  (sigtimes (mk-times (list *neg-one* tail)))))
	 (integer-normed-ineq (integercut integered-ineq cs)))
    (let ((coef (term-coef (lhs integer-normed-ineq)))
	  (var (term-var (lhs integer-normed-ineq)))
	  (ineq-pred (funsym integer-normed-ineq)))
      (mk-ineq (if (dp-minusp coef)
		   (opposite-ineq-pred ineq-pred)
		   ineq-pred)
	       var
	       (sigtimes (mk-times
			  (cons (mk-constant (/ 1 (constant-id coef)))
				(list (rhs integer-normed-ineq)))))))))

(defun opposite-ineq-pred (ineq-pred)
  "Flips inequality relation"
  (case (constant-id ineq-pred)
    (LESSEQP *GREATEREQP*)
    (GREATEREQP *LESSEQP*)
    (LESSP *GREATERP*)
    (GREATERP *LESSP*)
    (t ineq-pred)))

(defun opposite-ineq-pred? (ineq-pred1 ineq-pred2)
  "checks if ineq-pred1 and ineq-pred2 are of opposing comparisons.
   so if the lhs of two inequalities with opposing comparisons match, then
   these inequalities can be chained."
  (case (constant-id ineq-pred1)
    ((LESSEQP LESSP)
     (or (eq ineq-pred2 *GREATEREQP*)
	 (eq ineq-pred2 *GREATERP*)))
    ((GREATERP GREATEREQP)
     (or (eq ineq-pred2 *LESSEQP*)
	 (eq ineq-pred2 *LESSP*)))))

(defun destructure-monomial (trm)
  "Destructures a monomial into the coefficient
   and the non-coefficient part."
  (declare (type node trm))
  (cond ((dp-numberp trm)
	 (values trm *one*))
	((and (times-p term) (dp-numberp (coef term)))
	 (values (coef trm) (arg 2 trm)))
	(t *one* trm)))

(defun term-coef (term)
  "returns the coeff of a monomial"
  (cond ((and (times-p term)
	      (dp-numberp (coef term)))
	 (coef term))
	((dp-numberp term) term)
	(t *one*)))

(defun term-var (term)
  "returns the non-coeff part of a monomial."
  (cond ((dp-numberp term) *one*)
	((and (times-p term) (dp-numberp (coef term)))
	 (arg 2 term))
	(t term)))
