(in-package dp)


;(defstruct (dp-number (:include constant (id nil :type number))))

;;is division intepreted or not?
(defvar *printerpdivide* t)

;(deftype divide-array ()
;  '(and (array node (3))
;	(satisfies (lambda (x) (eq (arg 0 x) *divides*)))))

;(defstruct (divide :include application (arguments nil :type divide-array)))

;;recognizer for division
(defun divide-p (term)
  (and (application-p term)
       (eq (funsym term) *divide*)))

;;recognizer for multiplication
(defun times-p (expr)
  (and (application-p expr)
       (eq (funsym expr) *times*)))

;;recognizer for addition
(defun plus-p (expr)
  (and (application-p expr)
       (eq (funsym expr) *plus*)))

;;recognizer for difference
(defun difference-p (expr)
  (and (application-p expr)
       (eq (funsym expr) *difference*)))

(defun minus-p (expr)
  (and (application-p expr)
       (eq (funsym expr) *minus*)))

;;constructors
(defun mk-plus (args)
  (mk-term (cons *plus* args)))

(defun mk-plus* (&rest args)
  (mk-term (cons *plus* args)))

(defun mk-times (args)
  #+dp-dbg (assert (loop for a in args always (node-p a)))
  (mk-term (cons *times* args)))

(defmacro coef (times)
  `(arg 1 ,times))

(defun mk-times* (&rest args)
  (mk-term (cons *times* args)))

(defun mk-difference (arg1 arg2)
  (mk-term `(,*difference* ,arg1 ,arg2)))

(defun mk-divide (arg1 arg2)
  (mk-term `(,*divide* ,arg1 ,arg2)))

(defmacro numer (divide)
  `(arg 1 ,divide))

(defmacro denom (divide)
  `(arg 2 ,divide))

(defun mk-minus (arg)
  (mk-term `(,*minus* ,arg)))

;;returns monomials of a polynomial
(defun termsof (expr)  ;;;expr must be arithmetic.
  (if (plus-p expr)
      (funargs expr)  ;;;clumsy
      (list expr)))

;;is fun an inequality symbol.
(defun ineqfun-p (fun)
  (eq (node-type fun) 'arith-pred))

;;builds inequality
(defun mk-ineq (ineq-pred arg1 arg2)
  (mk-term `(,ineq-pred ,arg1 ,arg2)))

;;recognizer for inequalities
(defun ineq-p (ineq)
  (and (application-p ineq)
       (ineqfun-p (funsym ineq))))

;;string compares a, b.
(defmacro alphalessp (a b)
  `(string< (string ,a) (string ,b)))

;;compares monomials using arith-term-< (ignores times and coefficient)
(defun monom-< (u v)
  (let ((up (if (and (times-p u) (dp-numberp (arg 1 u)))
		(arg 2 u)
		u))
	(vp (if (and (times-p v) (dp-numberp (arg 1 v)))
		(arg 2 v)
		v)))
    (arith-term-< up vp)))

(defun old-arithord1(u v)
  (cond
   ((null u) nil)
   ((null v) t)
   ((symbolp u)
    (cond
     ((symbolp v)(alphalessp u v))
     ((numberp v) nil)
     (t t)))
   ((symbolp v)(numberp u))
   ((numberp u)
    (cond
     ((numberp v)(< u v))
     (t t)))
   ((numberp v) nil)
   ((equal (car u)(car v))(old-arithord1 (cdr u)(cdr v)))
   (t (old-arithord1 (car u)(car v)))))

;;arith-term-< puts an ordering on arithmetic terms
;;(uninterpreted terms, symbols, numbers, or their products)
;;so that the sum of monomials will be ordered from
;;smallest monomials to largest monomial.
;;Also, when solving for a variable, the solver
;;will pick the smallest non-numeric term to solve for.
;;The ordering is numbers < symbols < uninterpreted terms < products.

(defun arith-term-< (u v)
  (cond
   ((dp-numberp u) (if (dp-numberp v)
		       (< (constant-id u)
			  (constant-id v))
		       t))
   ((dp-numberp v) nil)
   ((constant-p u) (if (constant-p v)
		       (string< (constant-id u)
				(constant-id v))
		       t))
   ((constant-p v) nil)
   ((times-p u) (if (times-p v)
		    (times-< u v)
		    nil))
   ((times-p v) t)
   (t (application-< u v))))

(defun times-< (u v)
  (application-< u v))

(defun application-< (u v)
  (loop with ua = (arity u)
	with va = (arity v)
	for i from 0
	when (> i ua) return nil
	when (> i va) return t
	when (arith-term-< (arg i u) (arg i v)) return t
	unless (eq (arg i u) (arg i v)) return nil))

(defun arith-term-< (u v)
  (cond
   ((dp-numberp u) (if (dp-numberp v)
		       (< (constant-id u)
			  (constant-id v))
		       nil))
   ((dp-numberp v) t)
   ((constant-p u) (if (constant-p v)
		       (string< (constant-id u)
				(constant-id v))
		       nil))
   ((constant-p v) t)
   ((times-p u) (if (times-p v)
		    (times-< u v)
		    t))
   ((times-p v) nil)
   (t (application-< u v))))

;;like old-arithord1 but treats nonneg/strict-nonneg-vars
;;as biggest in ordering
;;should be fixed to respect abstract syntax.
(defun arithord1(u v)
  (cond
   ((null u) nil)
   ((null v) t)
   ((symbolp u)
    (cond
     ((symbolp v)
      (cond
       ((or (nonneg-var? u) (strict-nonneg-var? u))
	(cond
	 ((or (nonneg-var? v) (strict-nonneg-var? v)) (alphalessp u v))
	 (t nil)))
       ((or (nonneg-var? v) (strict-nonneg-var? v)) t)
       (t (alphalessp u v))))
     ((numberp v) nil)
     ((nonneg-var? u) nil)
     ((strict-nonneg-var? u) nil)
     (t t)))
   ((symbolp v)
    (or (numberp u)
	(nonneg-var? v)
	(strict-nonneg-var? v)))
   ((numberp u)
    (cond
     ((numberp v)(< u v))
     (t t)))
   ((numberp v) nil)
   ((equal (car u)(car v))(arithord1 (cdr u)(cdr v)))
   (t (arithord1 (car u)(car v)))))

;;canonizer for times:  returns a sum of monomials where each monomial
;;is sorted according to arith-term-<.  Input assumption is that subterms of
;;u have been canonized.
;;Needs to be rewritten to look nice.
(defun sigtimes(u)
  (let ((product nil)
	(coef 1))
    (labels
	((buildproduct
	  (arg)
	  ;;side-effects product and coeff,
	  ;;throws 0 on 0 or (distrib u) on plus.
	  ;;mainly builds a list of monomials corresponding to the
	  ;;argument of times.
	  ;;optionally interpret division carrying out cancellation.
	  (cond
	   ((dp-numberp arg)
	    (if (dp-zerop arg)
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
			(mk-times (sort product 'arith-term-<))
			(car product)))))
	  ;(break)
	  (if return-product
	      (if (= coef 1)
		  return-product
		  (mk-times (list (mk-constant coef) return-product)))
	      (mk-constant coef)))))))

;;used by buildproduct to distribute times over plus.
(defun distrib (u)
  (sigplus
   (mk-plus
    (loop for arg in (distrib1 (funargs u) (list nil))
	  collect (sigtimes (mk-times arg))))))

(defun distrib1 (product sum)
  (if (null product)
      sum
      (let ((cur-prodarg (car product))
	    (rest-sum (distrib1 (cdr product) sum)))
	(if (plus-p cur-prodarg)
	    (loop for arg in (funargs cur-prodarg)
		  nconc (distrib2 arg rest-sum))
	    (distrib2 cur-prodarg rest-sum)))))

(defun distrib2 (term product)
  (loop for prodarg in product collect (cons term prodarg)))

;;canonizer for division: subterms of input assumed canonized,
;;returns sum of monomials with denominator expressed as reciprocal.
;;does cancellation (on variables) and distribution.
(defun sigdivide (u)
  (if (not *printerpdivide*)
      u
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
		    (if (dp-zerop denom)
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
		       ((cdr product) (mk-times (sort product 'arith-term-<)))
		       (t (car product))))
		(cond
		 ((equal coef 1) product)
		 (t (mk-times `(,(mk-constant coef) ,product)))))
	       (t (mk-constant coef))))))))

;;makes reciprocal.
(defun make-inverse (denom)
  (if (divide-p denom)
      (denom denom)
      (make-inverse-1 denom)))

(defun make-inverse-1 (denom)
  (if (is-infinity denom)
      *zero*
      (mk-divide *one* denom)))

;;gensyms an infinity symbol to be used for zero denominator.
(defun make-infinity ()
  (mk-constant (gensym "*-infinity")))

(defun is-infinity (term)
  (and (constant-p term)
       (is-infinity-sym (constant-id term))))

;;checks if sym is a gensym'd infinity symbol.
(defun is-infinity-sym (sym)
  (and (symbolp sym)
       (> (length (string sym)) 10)
       (string= "*-infinity" (subseq (string sym) 0 10))))

;;canonizer for plus: returns sum of products with like monomials merged,
;;sorted, cancelled, and numbers up front.  
(defun sigplus (u)
  (let ((sum (list nil))
	(const 0)
	(result nil))
    (labels
	((addtosum
	  (u coef)
	  ;;;(format t "~%Entering addtosum: ~A, ~A" u coef)
	  (loop for ptr on sum
		when ;;;couldn't find u. Enter it at end.
		(null (cdr ptr)) return (setf (cdr ptr)
					      (list (cons u coef)))
		when ;;;found it
		(eq (caadr ptr) u) return (setf (cdr (cadr ptr))
						(+ (cdadr ptr) coef))
		when ;;;Splice it in here.
		(arith-term-< u (caadr ptr)) return (setf (cdr ptr)
							  (cons (cons u coef)
								(cdr ptr)))))
	 (buildsum
	  (u)
	  ;;;(format t "~%Entering buildsum: ~A" u)
	  (cond
	   ((dp-numberp u) (setq const (+ (constant-id u) const)))
	   ((constant-p u) (addtosum u 1))
	   ((and (times-p u) (dp-numberp (coef u)))
	    (addtosum (arg 2 u) (constant-id (coef u))))
	   ((plus-p u) (map-funargs #'buildsum u))
	   (t (addtosum u 1)))))
      (map-funargs #'buildsum u)
      (setq result (collectsum (cdr sum)))
      (cond
       (result
	(cond
	 ((= const 0)
	  (cond
	   ((cdr result) (mk-plus result))
	   (t (car result))))
	 (t (mk-plus `(,(mk-constant const) ,.result)))))
       (t (mk-constant const))))))

;;builds a list of products given a list of terms . coefs.
(defun collectsum(s)
  (loop for pair in s nconc
	(cond
	 ((zerop (cdr pair)) nil)
	 ((= (cdr pair) 1) (list (car pair)))
	 (t (list (mk-times `(,(mk-constant (cdr pair)) ,(car pair))))))))

;;canonizer for difference: subterms are already canonized,
;;converts a - b into a + (times -1 b)
(defun sigdifference (u)
  (sigplus (mk-plus `(,(arg 1 u) ,(sigminus1 (arg 2 u))))))

(defun sigminus(u)
  (sigminus1 (arg 1 u)))

(defun sigminus1 (u)
  (cond
   ((dp-numberp u) (mk-constant (- (constant-id u))))
   ((constant-p u) (mk-times `(,*neg-one* ,u)))
   ((and (times-p u) (dp-numberp (coef u)))
    (sigtimes (mk-times `(,(mk-constant (- (constant-id (coef u))))
			  ,(arg 2 u)))))
   ((plus-p u)
    (sigplus (mk-plus (map-funargs-list #'sigminus1 u))))
   (t (sigtimes (mk-times `(,*neg-one* ,u))))))

;;inverts a -1. 
(defmacro neg-sgn (sgn)
  `(* -1 ,sgn))

;;returns -1, 0, or +1 depending on whether coeff is neg, zero, or pos.
(defun term-sgn (term)
  (cond
   ((dp-numberp term)
    (cond
     ((eq term *zero*) 0)
     ((> (constant-id term) 0) 1)
     (t -1)))
   ((constant-p term) 1)
   ((times-p term) (term-sgn (coef term)))
   (t 1)))

;;inverts the inequality relation.
(defun negineq(ineq)
  (mk-ineq
   (case (constant-id (funsym ineq))
     (LESSEQP *GREATERP*)
     (LESSP   *GREATEREQP*)
     (GREATERP *LESSEQP*)
     (GREATEREQP *LESSP*))
   (arg 1 ineq)
   (arg 2 ineq)))

;;returns sign -1 for lessp, lesseqp, 1 for greaterp, greatereqp, 0 for equal.
(defun ineq-coef (ineq)
  (case (constant-id (funsym ineq))
    ((lessp lesseqp) -1)
    ((greaterp greatereqp) 1)
    (t 0)))

;;recognizes strict inequalities.
(defun ineq-strict? (ineq)
  (member (funsym ineq) (list *lessp* *greaterp*) :test #'eq))

(defun normineq (ineq cong-state)
  (integercut (normineq-rational ineq cong-state)))

;;canonizes inequalities a {<, <=, >, >=, =} b by
;;canonizing a - b and picking the head term as the lhs of
;;the normalized ineq.
(defun normineq-rational (ineq cong-state)
  (let ((ineq-pred (funsym ineq))
	(dif (sigdifference (mk-difference (arg 1 ineq)
					   (arg 2 ineq)))))
    (mk-diff-into-ineq ineq-pred dif cong-state)))

;;converts normalized difference into normalized inequality.
(defun mk-diff-into-ineq (ineq-pred dif cong-state)
  (let ((sign-type (sign-type dif cong-state)))
    (cond
     (sign-type (mk-sign-type-into-ineq ineq-pred sign-type dif cong-state))
     ((dp-numberp dif)
      (mk-const-ineq ineq-pred dif))
     (t (mk-no-sign-type-into-ineq ineq-pred dif cong-state)))))

(defun mk-no-sign-type-into-ineq (ineq-pred dif cong-state)
  (multiple-value-bind (head-term rest-dif)
	    (pick-head-term dif)
	  (mk-norm-ineq ineq-pred head-term rest-dif)))

(defun sign-type (term cong-state)
  (cond
   ((dp-numberp term)
    (number-sign-type term))
   ((constant-p term)
    (constant-sign-type term cong-state))
   ((times-p term)
    (times-sign-type term cong-state))
   ((plus-p term)
    (plus-sign-type term cong-state))
   (t (nonarith-sign-type term cong-state))))

(defun nonarith-sign-type (term cong-state)
  (dp-type-to-sign-type (dp-type term cong-state)))

(defun number-sign-type (term)
  (cond
   ((dp-zerop term) 'zero)
   ((dp-minusp term) 'strict-nonpos)
   (t 'strict-nonneg)))

(defun dp-type-to-sign-type (dp-type)
  (case dp-type
      ((nonneg nonpos zero strict-nonneg strict-nonpos) dp-type)
      (strict 'strict-nonneg)
      (t nil)))

(defun constant-sign-type (term cong-state)
  (let ((dp-type (dp-type term cong-state)))
    (dp-type-to-sign-type dp-type)))

(defun times-sign-type (term cong-state)
  (let* ((coef (term-coef term))
	 (monom (term-var term))
	 (monom-sign (monom-sign-type monom cong-state)))
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

(defun monom-sign-type (monom cong-state)
  (if (constant-p monom)
      (constant-sign-type monom cong-state)
      (dp-type-to-sign-type (dp-type monom cong-state))))

(defun plus-sign-type (plus cong-state)
  (let ((dp-type (dp-type plus cong-state)))
    (if dp-type
	(dp-type-to-sign-type dp-type)
	(let ((result (reduce #'sign-plus
			      (map-funargs-list #'sign-type plus cong-state))))
	  (if result
	      (let ((*dp-changed* *dp-changed*))
		(setf (dp-type plus cong-state) result))
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

(defun mk-sign-type-into-ineq (ineq-pred sign-type dif cong-state)
  (cond
   ((eq sign-type 'zero)
    (case (constant-id ineq-pred)
      (= *true*)
      ((LESSEQP GREATEREQP) *true*)
      (t *false*)))
   ((eq sign-type 'nonpos)
    (case (constant-id ineq-pred)
      ((LESSP LESSEQP) *true*)
      (= (mk-no-sign-type-into-ineq ineq-pred dif cong-state))
      (t  *false*)))
   ((eq sign-type 'strict-nonpos)
    (case (constant-id ineq-pred)
      ((LESSP LESSEQP) *true*)
      (LESSEQP (mk-no-sign-type-into-ineq *lessp* dif cong-state))
      (t  *false*)))
   ((eq sign-type 'nonneg)
    (case (constant-id ineq-pred)
      ((GREATERP GREATEREQP) *true*)
      (= (mk-no-sign-type-into-ineq ineq-pred dif cong-state))
      (t *false*)))
   (t
    (case (constant-id ineq-pred)
      ((GREATERP GREATEREQP)*true*)
      (GREATEREQP (mk-no-sign-type-into-ineq *greaterp* dif cong-state))
      (t *false*)))))

;;used by mk-diff-into-ineq to pick head term of a normalized difference.
;;returns two values head-term rest-dif s.t. diff = head-rest + rest-dif.
(defun pick-head-term (diff)
  (cond
   ((plus-p diff)
    (loop with front-rest = nil
	  for rest on (funargs diff)
	  for head-rest = (car rest)
	  do
	  (if (not (dp-numberp head-rest))
	      (return (values head-rest
			      (sigplus
			       (mk-plus (nconc front-rest (cdr rest))))))
	      (setq front-rest (nconc front-rest (list head-rest))))))
   (t (values diff *zero*))))

;;if the normalized difference is a constant, then mk-const-ineq
;;constructs the appropriate truth value corresponding to the original ineq 
(defun mk-const-ineq (ineq-pred dif)
  (cond
   ((dp-zerop dif)
    (case (constant-id ineq-pred)
      (EQUAL *true*)
      ((LESSEQP GREATEREQP) *true*)
      (t *false*)))
   ((dp-minusp dif)
    (case (constant-id ineq-pred)
      ((LESSP LESSEQP) *true*)
      (t  *false*)))
   (t
    (case (constant-id ineq-pred)
      ((GREATERP GREATEREQP) *true*)
      (t *false*)))))

;;if the normalized difference is not a number, then moves the head term
;; to the left of the normalized inequality.
(defun mk-norm-ineq (ineq-pred head tail)
  (let ((coef (term-coef head))
	(var (term-var head)))
    (mk-ineq (if (dp-minusp coef)
		 (opposite-ineq-pred ineq-pred)
		 ineq-pred)
	     var
	     (sigtimes (mk-times (cons (mk-constant (/ -1 (constant-id coef)))
				       (list tail)))))))

;;flips inequality relation.
(defun opposite-ineq-pred (ineq-pred)
  (case (constant-id ineq-pred)
    (LESSEQP *GREATEREQP*)
    (GREATEREQP *LESSEQP*)
    (LESSP *GREATERP*)
    (GREATERP *LESSP*)
    (t ineq-pred)))

;;checks if ineq-pred1 and ineq-pred2 are of opposing comparisons.
;;so if the lhs of two inequalities with opposing comparisons match, then
;;these inequalities can be chained.
(defun opposite-ineq-pred? (ineq-pred1 ineq-pred2)
  (case (constant-id ineq-pred1)
    ((LESSEQP LESSP)
     (or (eq ineq-pred2 *GREATEREQP*) (eq ineq-pred2 *GREATERP*)))
    ((GREATERP GREATEREQP)
     (or (eq ineq-pred2 *LESSEQP*) (eq ineq-pred2 *LESSP*)))))

;;returns the coeff of a monomial
(defun term-coef (term)
  (cond
   ((and (times-p term)
	 (dp-numberp (coef term)))
    (coef term))
   ((dp-numberp term) term)
   (t *one*)))

;;returns the non-coeff part of a monomial.
(defun term-var (term)
  (cond
   ((dp-numberp term) *one*)
   ((and (times-p term) (dp-numberp (coef term)))
    (arg 2 term))
   (t term)))

(defun sigarith (term cong-state)
  (cond
   ((dp-numberp term) term)
   ((plus-p term) (sigplus term))
   ((times-p term) (sigtimes term))
   ((difference-p term) (sigdifference term))
   ((minus-p term) (sigminus term))
   ((divide-p term) (sigdivide term))
   (t term)))
