2;; /* Copyright (c) SRI International 2011. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; vts.lisp --
;; Author          : Tim King
;; Created On      : Thu Jul 08, 2011
;; Last Modified By: Tim King
;; Last Modified On: 
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :vts)

(defpackage :virtual-term-substitution
  (:nicknames :vts)
  (:export 
	   constraint
	   zero-constraint
	   positive-constraint
	   non-negative-constraint
	   non-zero-constraint
	   constraint-set
	   vts-node
	   poly-lists-to-constraint-set
	   is-consistent
	   lit-value
	   is-sat?
	   is-unsat?
	   is-unknown?
	   summarize-node
	   leaves-blocked-nodes
	   node-get-zeroes-positives-non-negatives-polys
	   split-non-zeroes
	   reset-ids
	   *default-resource-limit*)
  (:use :polynomial-representation-core :cl-user :common-lisp))
(in-package :virtual-term-substitution)

;;
;; This algorithm implements the ideas in a few papers.
;; These are referenced in the comments by the first author name.
;;
;; 1. Quantifier Elimination for Real Algebra - the Quadratic Case and Beyond
;;    V. Weispfenning
;; 2. A Lazy SMT-Solver for a Non-Linear Subset of Real Algebra
;;    E. Ábrahám, U. Loup, F. Corzilius, T. Strum
;;

;; This is a rough draft implementation of the virtual term substiution
;; quantifier elimination method.

;; Polynomials are in sum of power product form
;;    \Sum coeff * pp
;; where
;;    pp = \Prod x^i

;; 4 types of constraints
;; f = 0, f > 0, f >=, f != 0
;; f is a polynomial

;; Surd Extended Polynomials
;; --------------------
;;   (d^{-1})(a + b\sqrt(c))
;; where a,b,c,d is a polynomial
;; 
;; General Extended Polynomials
;;   f
;;   f +/- epsilon
;;   +/- infty
;; where
;;   f : (d^-1)(a + b\sqrt(c))
;;
;; A substitution is a pair (x e) where e is a general extended polynomial 

;; Nodes
;; --------------------
;; Each node has a set of constraints, and a substitution trail.
;;  (C,S)
;;
;; Each node has children.
;; A node is "equivalent" to the disjunction of its children.
;; 
;; Reasons nodes can be expanded:
;; 1. Project a variable
;; 2. Virtually substitute a constraint
;; 3. Simplify the constraints
;; 4. Case split
;; 5. Add entailed constraint
;;
;; Leafs are unproven goal nodes or contradictions.

;; Operations on Constraint Sets
;; ----------------------
;; 1. Determine the degrees of variables in a polynomial
;; 2. Determine if a variable can be used for projection
;; 3. Determine the test cases
;; 4. Determine if a substitution applies

;; DNF normal form for case splitting?

(defvar *verbosity* -10 "Controls how verbose the vts package is.")
(defmacro vformat (level destination control-string &rest args)
  `(if (<= ,level *verbosity*)
       (format ,destination ,control-string ,@args)
       nil
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;      Polynomial Utilites     ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; toPolyRepCnst
;; ----------
;; Makes a constant polynomial for the non-zero constant x.
;; For uses with the :polynomial-representation-core package.
(defun toPolyRepCnst (x)
  (assert (not (= x 0)))
  (list (list x)))

;; Define constant polynomial that appear in various parts of the algorithm.
(defvar *poly-zero* nil "The polynomial representation of 0.")
(defvar *poly-one* (toPolyRepCnst 1) "The polynomial representation of 1.")
(defvar *poly-negative-one* (toPolyRepCnst -1) "The polynomial representation of -1.")
(defvar *poly-two* (toPolyRepCnst 2) "The polynomial representation of 2.")
(defvar *poly-four* (toPolyRepCnst 4) "The polynomial representation of 4.")

(defun polyIsZero? (p)
  "Returns true if p is the polynomial representing zero."
  (null p))

(defun monoCoeff (mono)
  (car mono))

(defun monoPP (mono)
  (cdr mono))

(defun monoToPoly (mono)
  (list mono))

(defun is-constant-mono (mono)
  "Returns true if the monomial is a constant."
  (null (cdr mono)))

(defun constant-monomial (mono)
  "If a monomial is a constant-monomial, returns the coefficient for the monomial."
  (assert (is-constant-mono mono))
  (car mono))

(defun constant-poly (p)
  "If the polynomial is a constant, returns the constant.
   Otherwise, returns nil.
   If (polyIsZero? p), 0 is returned (not nil)."
  (if (polyIsZero? p)
      0
      (let ((head (car p)) (rest (cdr p)))
	(if (and (null rest) (is-constant-mono head))
	    (constant-monomial head)
	    nil))))

(defun poly-is-constant? (p)
  (not (null (constant-poly p))))

(defun leading-coeff (p)
  (assert (not (polyIsZero? p)))
  (monoCoeff (car p)))


(defun leading-coeff-is-one? (p)
  (assert (not (polyIsZero? p)))
  (= 1 (leading-coeff p)))

(defun leading-coeff-is-minus-one? (p)
  (assert (not (polyIsZero? p)))
  (= -1 (leading-coeff p)))

(defun leading-coeff-is-plus-minus-one? (p)
  (assert (not (polyIsZero? p)))
  ;;(vformat 1 t "leading-coeff: ~a ~%" (leading-coeff p))
  (or (leading-coeff-is-one? p)
      (leading-coeff-is-minus-one? p)))

(defun constantPart (p)
  "Returns the non-zero constant monomial in a polynomial.
   If no non-zero constant monomial is in the polynomial,
   the zero polynomial is returned."
  (if (null p) nil ;; 0 implictly
      (let ((mono (car p)) (rest (cdr p)))
	(if (is-constant-mono mono)
	    (list mono) ;; must return a polynomial
	    (constantPart rest)))))


(defun removeConstantPart (p)
  "Returns (values p' c) where p = p' + c,
   c is a constant (not in polyrep form!),
   and p' has no constant monomials."
  (let* ((c (constantPart p))
	 (p-sub-c (prep:polyrepAddPoly p (prep:polyrepNegativePoly  c)))
	 (simp-c (if (polyIsZero? c) 0 (leading-coeff c))))
    (values p-sub-c simp-c)))

(defun divide-by-leading-coefficient (p use-abs)
  "Divides p by the coefficient of the leading monomial.
   If use-abs is true, divide by the absolute value of leading coefficient."
  (if (polyIsZero? p) p
      (let* ((coeff (leading-coeff p))
	     (d (if use-abs (abs coeff) coeff)))
	(prep:polyrepDividePolyCst p d))))

(defun mono-is-linear-subterm? (mono)
  "Returns true if mono is a constant or a variable to pow 1 times a constant."
  (let ((pp (monoPP mono)))
    (if (null pp)
	t
	(and (null (cdr pp)) (= 1 (cdr (car pp)))))))

(defun poly-is-sum? (f)
  "Returns true if f is the sum of 2 monomials."
  (if (polyIsZero? f) nil
      (if (polyIsZero? (cdr f)) nil
	  t)))

(defun var-pow-in-pp (ppl x)
  (assoc x ppl))

(defun var-pow-in-mono (mono x)
  (var-pow-in-pp (monoPP mono) x))

(defun var-pow-to-pp (v_p)
  (list v_p))

;;The formal degree of x is the highest power of x appearing in a polynomial
(defun formal-degree-pp (pp x)
  (if (null pp)
      0
      (let ((head (car pp)) (rest  (cdr pp)))
	(if (eq x (car head))
	    (cdr head)
	    (formal-degree-pp rest x)))))

(defun formal-degree-poly (p x &optional (curr 0))
  (if (null p)
      curr
      (formal-degree-poly (cdr p) x (max curr (formal-degree-pp (cdr (car p)) x)))))

(defun power-parity (f x)
  (mod (formal-degree-poly f x) 2))

(defun pp-to-poly (pp)
  (list (cons 1 pp)))

;; Represents f as a univariate polynomial for x
;; Returns an associative list of pairs ((x . i) . (p_i)) 
;;   f = \Sum (x ^ i) * p_i
;; p_i are polynomials not containing x
;; p_i's are in decreasing order w.r.t. the power of x^i.
;;
;; From a list perspective f is a list of uv-products for var x.
;; A univariate product for the variable x has the structure:
;; (x . i) . p_i
(defun uv-product-get-var-pow (uv-prod)
  (car uv-prod))

(defun uv-product-get-var (uv-prod)
  (car (uv-product-get-var-pow uv-prod)))

(defun uv-product-get-pow (uv-prod)
  (cdr (uv-product-get-var-pow uv-prod)))

(defun uv-product-get-coefficient (uv-prod)
  (cdr uv-prod))

(defun univariate-coeff-list (uvp)
  (mapcar 'uv-product-get-coefficient uvp))



(defun univariate-poly-to-polyrep (uv)
  "Converts a univariate polynomial to a polyrep polynomial."
  (reduce 'prep:polyrepAddPoly (mapcar 'univariate-product-to-polyrep uv)))

(defun univariate-product-to-polyrep (uv-prod)
  (prep:polyrepMultiplyPoly
   (var-pow-to-polyrep (uv-product-get-var-pow uv-prod))
   (uv-product-get-coefficient uv-prod)))

(defun var-pow-to-polyrep (var-pow)
  (if (= 0 (cdr var-pow))
      *poly-one*
      (list (cons 1 (list var-pow))))) ;; (1 . ((x . i)))

(defun var-to-polyrep (var)
  (var-pow-to-polyrep (cons var 1)))

;; Represents f as a univariate polynomial for x

(defun univariate-rep (f x)
  (let ((sum (univariate-sum f x)))
    (combine-univariates sum x nil nil)))

(defun map-powers-over-accum (powers accum)
  (mapcar (lambda (p) (cdr (assoc p accum))) (sort powers #'>)))

;; Takes the list of polynomial 
(defun combine-univariates (sum x powers accum)
  (if (null sum)
      (map-powers-over-accum powers accum)
      (let* ((x_pow_p (car sum)) (rest (cdr sum))
	     (x_pow (car x_pow_p)) (p (cdr x_pow_p))
	     (var (car x_pow)) (pow (cdr x_pow))
	     (ares (assoc pow accum)))
	(assert (eq var x))
	(if (null ares)
	    ;; This power has not been seen before
	    (combine-univariates rest x (cons pow powers) (acons pow x_pow_p accum))
	    ;; This power has been seen before
	    ;; ares = (pow . ((x . pow) . q))
	    (let* ((q (cdr (cdr ares)))
		   (p_plus_q (prep:polyrepAddPoly p q))
		   (new-accum (acons pow (cons x_pow p_plus_q) accum)))
	      (combine-univariates rest x powers new-accum))))))

(defun univariate-sum (f x &optional accum)
  (if (null f) accum
      (let ((mono (car f)) (rest (cdr f)))
	(univariate-sum rest x (cons (univariate-monomial mono x) accum)))))

(defun univariate-monomial (mono x)
  (let ((x-pow (var-pow-in-mono mono x))
	(mono-as-poly (monoToPoly mono)))
    (if (null x-pow)
	(acons x 0 mono-as-poly)
	(cons x-pow (prep:polyrepDividePolyPP mono-as-poly (var-pow-to-pp x-pow))))))

(defmethod uv-coeff (uvp (i integer))
  "Given a univariate-polynomial for variable x,
   return the polynomial associated with x^i."
  (if (null uvp) *poly-zero*
      (let ((head (car uvp)) (rest (cdr uvp)))
	(if (= i (cdr (car head))) (cdr head) (uv-coeff rest i))))) 


(defun univariate-derivative (uv)
  (remove nil (mapcar 'uv-prod-derivative uv)))
(defun uv-prod-derivative (uv-prod)
  (let ((x (uv-product-get-var uv-prod))
	(pow (uv-product-get-pow uv-prod))
	(coeff (uv-product-get-coefficient uv-prod)))
    (case pow
      (0 nil)
      ;;(1 (coeff)
      (otherwise (cons (cons x (- pow 1)) (prep:polyrepMultiplyCstPoly pow coeff))))))
	
(defun formal-derivative (f var)
  (let* ((uv-f (univariate-rep f var))
	 (uv-f-prime (univariate-derivative uv-f))
	 (res (univariate-poly-to-polyrep uv-f-prime)))
    (vformat 4 t "(formal-derivative ~a ~a) -> ~a -> ~a -> ~a~%" f var uv-f uv-f-prime res)
    res))


(defun determinant (a b c)
  (prep:polyrepAddPoly
   (prep:polyrepMultiplyPoly b b)
   (prep:polyrepMultiplyCstPoly -4 (prep:polyrepMultiplyPoly a c))))

;; lit-value
;; ----------
;; A three valued logical constant.
;; This is useful for when a function can take on three different results.
(deftype lit-value ()
  '(member
    :lit-unknown
    :lit-sat
    :lit-unsat))

(defun is-sat? (lv)
  (eq lv :lit-sat))

(defun is-unsat? (lv)
  (eq lv :lit-unsat))

(defun is-unknown? (lv)
  (eq lv :lit-unknown))

;; constraint
;; ----------
;; Each constraint has an associated polynomial.
;; The polynomial is represented using the :polynomial-representation-core
;; package.
;; 
;; There are 4 types of constraints.
;; Each type of constraint has a different type that inherits from constraint.
;; If f is a polynomial,
;; f = 0  : zero-constraint
;; f > 0  : positive-constraint
;; f >= 0 : non-negative-constraint
;; f != 0 : non-zero-constraint

(defstruct constraint
  poly             ;; Defined in polyrep-totdeglex.lisp
)

(defstruct (zero-constraint
	     (:include constraint)
	     (:print-function print-zero-constraint))
  "f = 0")

(defstruct (positive-constraint
	     (:include constraint)
	     (:print-function print-positive-constraint))
  "f > 0")

(defstruct (non-negative-constraint
	     (:include constraint)
	     (:print-function print-non-negative-constraint))
  "f >= 0")

(defstruct (non-zero-constraint
	     (:include constraint)
	     (:print-function print-non-zero-constraint))
  "f != 0")

(defun print-zero-constraint (zc stream depth)
  (declare (ignore depth))
  (format stream "(= 0 ~a)" (prep:polyrepPrint (constraint-poly zc))))

(defun print-positive-constraint (pc stream depth)
  (declare (ignore depth))
  (format stream "(< 0 ~a)" (prep:polyrepPrint (constraint-poly pc))))

(defun print-non-negative-constraint (nnc stream depth)
  (declare (ignore depth))
  (format stream "(<= 0 ~a)" (prep:polyrepPrint (constraint-poly nnc))))

(defun print-non-zero-constraint (nzc stream depth)
  (declare (ignore depth))
  (format stream "(not (= 0 ~a))" (prep:polyrepPrint (constraint-poly nzc))))

(defvar *false* (make-zero-constraint :poly *poly-one*)
  "Defines a trivially false constraint.")

;; This is the constructor that is supposed to be used for constraints!
;; This guarentees that the leading coefficient is:
;;  - 1 for zero and non-zero constraints 
;;  - +/- for positive and non-negative constraints.
(defun p-is-zero-c (p)
  (make-zero-constraint :poly (divide-by-leading-coefficient p nil)))
(defun p-is-non-zero-c (p)
  (make-non-zero-constraint :poly (divide-by-leading-coefficient p nil)))
(defun p-is-positive-c (p)
  (make-positive-constraint :poly (divide-by-leading-coefficient p t)))
(defun p-is-non-negative-c (p)
  (make-non-negative-constraint :poly (divide-by-leading-coefficient p t)))



(defmethod negate-constraint ((c zero-constraint))
  (p-is-non-zero-c (constraint-poly c)))
(defmethod negate-constraint ((c positive-constraint))
  (p-is-non-negative-c (polyrepNegativePoly (constraint-poly c))))
(defmethod negate-constraint ((c non-negative-constraint))
  (p-is-positive-c (polyrepNegativePoly (constraint-poly c))))
(defmethod negate-constraint ((c non-zero-constraint))
  (p-is-zero-c (constraint-poly c)))


(defmethod constant-constraint-lit-value ((psi constraint))
  "If the polynomial for the constraint is constant, the constraint is evaluated
   and if the constraint holds :lit-sat is returned (otherwise :lit-unsat).
   If the polynomial for the constraint is not constant, :lit-unknown is returned."
  (let ((constant (constant-poly (constraint-poly psi))))
    (if (null constant) :lit-unknown
	(let ((is-sat 
	       (typecase psi
		 (zero-constraint (= 0 constant))
		 (non-zero-constraint (not (= 0 constant)))
		 (positive-constraint (< 0 constant))
		 (non-negative-constraint (<= 0 constant)))))
	  (if is-sat :lit-sat :lit-unsat)))))

(defmethod constant-constraint-is-unsat ((psi constraint))
  (eq :lit-unsat (constant-constraint-lit-value psi)))

(defmethod constant-constraint-is-sat ((psi constraint))
  (eq :lit-sat (constant-constraint-lit-value psi)))

(defun p-greater-than-zero-c (f strict)
  (if strict (p-is-positive-c f) (p-is-non-negative-c f)))

(defun zeroes-list (polys)
  "[p_i] -> [p_i = 0]"
  (mapcar 'p-is-zero-c polys))
(defun positives-list (polys)
  "[p_i] -> [p_i > 0]"
  (mapcar 'p-is-positive-c polys))
(defun non-negatives-list (polys)
  "[p_i] -> [p_i >= 0]"
  (mapcar 'p-is-non-negative-c polys ))
(defun non-zeroes-list (polys)
  "[p_i] -> [p_i != 0]"
  (mapcar 'p-is-non-zero-c polys))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;        Surd Polynomial       ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (surd-polynomial
	     (:print-function print-surd)
	     (:conc-name surd-))
  "(a + b\sqrt(c))/d where a,b,c,d are polynomials.
  The default value for a,b,c are 0. The default value for d is 1."
  (a *poly-zero*)
  (b *poly-zero*)
  (c *poly-zero*)
  (d *poly-one*)
)

(defvar *surd-polynomial-zero* (make-surd-polynomial) "Defines a surd polynomial 0.")
(defvar *surd-polynomial-one* (make-surd-polynomial :a *poly-one*) "Defines a surd polynomial 1.")

(defun print-surd (s stream depth)
  (declare (ignore depth))
  (format stream "(/ (~a + ~a*sqrt(~a)) (~a))"
	  (prep:polyrepPrint (surd-a s))
	  (prep:polyrepPrint (surd-b s))
	  (prep:polyrepPrint (surd-c s))
	  (prep:polyrepPrint (surd-d s))))

(defun polynomial-to-surd (p)
  "Convert a regular polynomial to a surd-polynomial."
  (make-surd-polynomial :a p))

(defmethod surd-rad-is-zero? ((sp surd-polynomial))
  (or (polyIsZero? (surd-b sp))
      (polyIsZero? (surd-c sp))))

(defun simp-surd-polynomial (a b c d)
  "A surd polynomial is a simplified surd-polynomial iff (b = 0 iff c = 0).
   Simplified surd polynomial are not unique due to divisibilty."
  (assert (not (polyIsZero? d)))
  (let ((zero-rad (or (polyIsZero? b) (polyIsZero? c))))
    (make-surd-polynomial :a a
			  :b (if zero-rad *poly-zero* b)
			  :c (if zero-rad *poly-zero* c)
			  :d d)))

(defmethod compatible-surd
    ((x surd-polynomial) (y surd-polynomial))
  "Two surd polynomials x and y are closed under addition and multiplication
   if the radical is the same or if either radical is zero.
   Returns t if the two can be combined."
  (let ((xc (surd-c x))
	(yc (surd-c y)))
    (or (prep:polyrepEqual? xc yc) (surd-rad-is-zero? x) (surd-rad-is-zero? y))))

(defmethod general-surd
    ((x surd-polynomial) (y surd-polynomial))
  "Returns the most general surd of two compatible surd-polynomials x and y."
  (assert (compatible-surd x y))
  (cond ((surd-rad-is-zero? x) (surd-c y))
	((surd-rad-is-zero? y) (surd-c x))
	(t (surd-c x))))


(defmethod add-surds
    ((x surd-polynomial) (y surd-polynomial))
  (assert (compatible-surd x y))
  "If c = c', then
  [(a + b sqrt(c))/d] + [(a' + b' sqrt(c')/d'] =
  [(ad' + da')+ (bd' + db') sqrt(c)]/(dd')"
  (let ((xa (surd-a x))
	(xb (surd-b x))
	(xd (surd-d x))
	(ya (surd-a y))
	(yb (surd-b y))
	(yd (surd-d y))
	(newc (general-surd x y)))
    (if (prep:polyrepEqual? xd yd)
	(let ((xa-ya (prep:polyrepAddPoly xa ya))
	      (xb-yb (prep:polyrepAddPoly xb yb)))
	  (simp-surd-polynomial xa-ya xb-yb newc xd))
	(let ((xdyd (prep:polyrepMultiplyPoly xd yd))
	      (xayd (prep:polyrepMultiplyPoly xa yd))
	      (xdya (prep:polyrepMultiplyPoly xd ya))
	      (xbyd (prep:polyrepMultiplyPoly xb yb))
	      (xdyb (prep:polyrepMultiplyPoly xd yb)))
	  (let ((xayd-xdya (prep:polyrepAddPoly xayd xdya))
		(xdyb-xbyd (prep:polyrepAddPoly xdyb xbyd)))
	    (simp-surd-polynomial xayd-xdya xdyb-xbyd newc xdyd))))))

(defmethod mult-surds
    ((x surd-polynomial) (y surd-polynomial))
  (assert (compatible-surd x y))
  "If c = c', then
  (a + b sqrt(c))/d * (a' + b' sqrt(c')/d' =
  ((aa' + bb'c)+ (ab' + ab') sqrt(c))/(dd')"
  (let ((xa (surd-a x))
	(xb (surd-b x))
	(xd (surd-d x))
	(ya (surd-a y))
	(yb (surd-b y))
	(yd (surd-d y))
	(newc (general-surd x y)))
    (let ((xdyd (prep:polyrepMultiplyPoly xd yd))
	  (xaya (prep:polyrepMultiplyPoly xa ya))
	  (xbybc (prep:polyrepMultiplyPoly (prep:polyrepMultiplyPoly xb yb) newc))
	  (xayb (prep:polyrepMultiplyPoly xa yb))
	  (xbya (prep:polyrepMultiplyPoly xb ya)))
      (simp-surd-polynomial
       (prep:polyrepAddPoly xaya xbybc)
       (prep:polyrepAddPoly xayb xbya)
       newc
       xdyd))))

(defmethod exp-surd ((sp surd-polynomial) (i integer))
  "Raises a surd-polynomial sp to the i'th power via repeated multiplication."
  ;; Not as efficient as repeated squaring,
  ;; but large powers are not really going to kill this algorithm anyways.
  (assert (>= i 0))
  (if (= i 0)
      *surd-polynomial-one*
      (mult-surds sp (exp-surd sp (- i 1)))))

(defmethod surd-into-univariate-product* (uv-prod (sp surd-polynomial) k)
  "((x^i) * p_i)[sp/x] -> (sp)^i * p_i -> (sp)^i * p_i * (d/d)^{k-i}
   where d is the denominator of sp."
  ;;The final multiplication is to prevent the denominators from blowing up during addition.
  ;;Better solutions are available once a polynomial gcd has been implemented. 
  (let* ((p-surd (polynomial-to-surd (uv-product-get-coefficient uv-prod)))
	 (pow (uv-product-get-pow uv-prod))
	 (sp-to-pow (exp-surd sp pow))
	 (d (surd-d sp))
	 (d-over-d (make-surd-polynomial :a d :d d))
	 (diff-pow (- k pow))
	 (d-over-d-to-diff (exp-surd d-over-d diff-pow))
	 (p0 (mult-surds p-surd sp-to-pow))
	 (p1 (mult-surds p0 d-over-d-to-diff)))
    p1))

(defmethod substitute-surd-into-poly (f (sp surd-polynomial) x)
  "Substitutes a variable x for a surd-polynomial sp into the polynomial f."
  (let* ((uv-f (univariate-rep f x))
	 (k (formal-degree-poly f x))
	 (x-free (mapcar (lambda (x) (surd-into-univariate-product* x sp k)) uv-f)))
    (reduce 'add-surds x-free)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;     Extended Polynomials     ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General Extended Polynomials
;;   f
;;   f +/- epsilon
;;   +/- infty
;; where
;;   f : (d^-1)(a + b\sqrt(c))
(defstruct extended-polynomial
  "An extended polynomial e is for representing possible substitutions
  for a variable x, [e/x], which can lie outside the real algebra."
)

(defstruct (plain-extended-polynomial
	     (:include extended-polynomial)
	     (:print-function print-plain-extended-polynomial))
  "An extended polynomial of the form f where f is a surd polynomial."
  (poly *poly-zero* :type surd-polynomial)
)

(defstruct (epsilon-extended-polynomial
	     (:include extended-polynomial)
	     (:print-function print-epsilon-extended-polynomial))
  "An extended polynomial of the form f +/- epsilon where f is a surd polynomial
  and epsilson is an implicit strictly greater than zero variable."
  (poly *poly-zero* :type surd-polynomial)
  (sgn t :type boolean)
)

(defstruct (infty-extended-polynomial
	     (:include extended-polynomial)
	     (:print-function print-infty-extended-polynomial))
  (sgn t :type boolean)
)
(defvar *infty* (make-infty-extended-polynomial :sgn t) "+infty")
(defvar *neg-infty* (make-infty-extended-polynomial :sgn nil) "-infty")

(defmethod infty-is-pos ((ep infty-extended-polynomial))
  (infty-extended-polynomial-sgn ep))
(defmethod infty-is-neg ((ep infty-extended-polynomial))
  (not (infty-is-pos ep)))


(defun print-plain-extended-polynomial (pep stream depth)
  (declare (ignore depth))
  (format stream "~a" (plain-extended-polynomial-poly pep)))

(defun print-epsilon-extended-polynomial (eep stream depth)
  (declare (ignore depth))
  (if (not (epsilon-extended-polynomial-sgn eep) ) (format stream "-"))
  (format stream "*ep* + ~a" (epsilon-extended-polynomial-poly eep)))

(defun print-infty-extended-polynomial (iep stream depth)
  (declare (ignore depth))
  (if (infty-extended-polynomial-sgn iep)
      (format stream "+infty")
      (format stream "-infty")))

;; Constraint-sets are representations of a conjunction of
;; constraints. These are currently represented as lists.
(defstruct (constraint-set 
	     (:print-function print-constraint-set)
	     (:conc-name cs-))
  "Constraint-sets are representations of a conjunction of constraints.
   These are currently represented as lists.
   The empty list of constraints is considered equivalent to true."
  (constraints nil) ;; list of constraints
)

(defun print-constraint-set (phi stream depth)
  (declare (ignore depth))
  (let ((psis (cs-constraints phi)))
    (if (null psis)
	(format stream "true")
	(format stream "(and ~{~a~^, ~})" psis))))

(defun cs-list (&rest args)
  (make-constraint-set :constraints args))

(defvar *cs-true* (cs-list) "A trivially true constraint set.")
(defvar *cs-false* (cs-list *false*) "A trivially false constraint set.")

(defmethod constraint-set-is-true ((cs constraint-set))
  (null (cs-constraints cs)))


(defmethod and-constraint-sets ((a constraint-set) (b constraint-set))
  (let ((cas (cs-constraints a))
	(cbs (cs-constraints b)))
    (make-constraint-set :constraints (append cas cbs))))

(defun poly-lists-to-constraint-set
    (zeros non-zeroes positives non-negatives)
  (let ((zs  (zeroes-list zeros))
	(nzs (non-zeroes-list non-zeroes))
	(ps  (positives-list positives))
	(nns (non-negatives-list non-negatives)))
    (make-constraint-set :constraints (append zs nzs ps nns))))

(defstruct (dnf
	     (:print-function print-dnf))
  "A version of Disjunctive Normal Form.
   A dnf is handled as a list of constraint-sets.
   DNFs are used as a means of handling case splits introduced by virtual substitutions."
  (conjunctions nil) ;; conjunctions is a list of constraint-sets
)

(defmethod constraint-set-well-formed? ((psi constraint-set))
  (every (lambda (x) (typep x 'constraint)) (cs-constraints psi)))

(defmethod dnf-well-formed? ((phi dnf))
  (every 'constraint-set-well-formed? (dnf-conjunctions phi)))

(defun print-dnf (phi stream depth)
  (declare (ignore depth))
  (let ((psis (dnf-conjunctions phi)))
    (if (null psis)
	(format stream "false")
	(format stream "(or ~{~a~^, ~})" psis))))


(defmethod dnf-from-cs ((cs constraint-set))
  (make-dnf :conjunctions (list cs)))

(defvar *dnf-true* (dnf-from-cs *cs-true*) "Defines a trivially true dnf.")
(defvar *dnf-false* (make-dnf) "Defines a false dnf.")

(defmethod dnf-from-constraint ((psi constraint))
  (dnf-from-cs (cs-list psi)))


(defun or-dnf-list (dnfs)
  "Constructs a dnf equivalent to the disjunction of a list of dnfs."
  (make-dnf :conjunctions (reduce 'append (mapcar 'dnf-conjunctions dnfs))))

(defmethod or-dnfs ((phi dnf) (psi dnf))
  "Constructs a dnf equivalent to (or psi phi)."
  (or-dnf-list (list phi psi)))

(defmethod and-dnfs ((phi dnf) (psi dnf))
  "Constructs a dnf equivalent to (and psi phi)."
  (let* ((phi-cs (dnf-conjunctions phi))
	 (dist-phi-cs (mapcar (lambda (x) (and-constraint-set-dnf x psi)) phi-cs))
	 (res  (or-dnf-list dist-phi-cs)))
    ;; dist-phi-cs is a list of dnfs
    (assert (dnf-well-formed? res))
    res))

(defun and-dnf-list (phis)
  "Constructs a dnf equivalent to anding a list of dnfs together."
  (reduce 'and-dnfs phis))

(defmethod and-constraint-set-dnf ((phi constraint-set) (psi dnf))
  "Constructs a dnf equivalent to the and of a constraint set and a dnf."
  (let* ((psi-cs (dnf-conjunctions psi))
	 (dist-psi-cs (mapcar (lambda (x) (and-constraint-sets phi x)) psi-cs))
	 (res (make-dnf :conjunctions  dist-psi-cs)))
    (assert (dnf-well-formed? res))
    res))


(defmethod negate-constraint-set ((phi constraint-set))
  "Constructs a dnf equivalent to the negation of a constraint-set."
  (let* ((psis (cs-constraints phi))
	 (negated-psis (mapcar 'negate-constraint psis))
	 (cnsis (mapcar 'cs-list negated-psis)))
    (make-dnf :conjunctions cnsis)))
     
(defmethod negate-dnf ((phi dnf))
  (let* ((psis (dnf-conjunctions phi))
	 (negated-css (mapcar 'negate-constraint-set psis)))
    (and-dnf-list negated-css)))

;; compute the degrees of the variables in a constraint set
;; [Constraints] -> [(x i)]
;; Constraint -> [(x i)]
;; polynomial -> [(x i)]
;; [monomial] -> [(x i)]
;; monomial -> [(x i)]
;; pp -> [(x i)]
;; [(x i)] -> Map (Var [degree])

(defmethod var-powers-constraint-set* ((phi constraint-set))
  (let ((psis (cs-constraints phi)))
    (var-powers-constraint-list* psis)))
(defun var-powers-constraint-list* (psis)
  (reduce 'union (mapcar 'var-powers-constraint* psis)))
(defmethod var-powers-constraint*
    ((psi constraint))
  (var-powers-polynomial* (constraint-poly psi) nil))
(defun var-powers-polynomial* (monos accum)
  (if (null monos) accum
      (let ((mono (car monos))
	    (rest (cdr monos)))
	(var-powers-polynomial* rest (union accum (var-powers-monomial* mono))))))

(defun var-powers-monomial* (mono)
  (let ((coeff (car mono))
	(pps (cdr mono)))
    (assert (not (= 0 coeff)))
    pps))

(defun var-powers-set-variables (var-powers &optional accum)
  "The set of variables appearing in a list [(x.i)]."
  (if (null var-powers) accum
      (let ((var (car (car var-powers)))
	    (rest (cdr var-powers)))
	(var-powers-set-variables rest (adjoin var accum)))))

(defun var-to-power-map (var var-powers &optional accum)
  (if (null var-powers) accum
      (let ((x (car (car var-powers)))
	    (pow (cdr (car var-powers)))
	    (rest (cdr var-powers)))
	(var-to-power-map var rest (if (eq x var) (adjoin pow accum) accum)))))

(defun vars-to-power-map (vars var-powers &optional accum)
  (if (null vars) accum
      (let ((var (car vars))
	    (rest (cdr vars)))
	(vars-to-power-map rest var-powers
			   (acons var (var-to-power-map var var-powers) accum)))))

(defmethod constraint-set-to-powers-map ((cs constraint-set))
  (let* ((vps (var-powers-constraint-set* cs))
	 (vars (var-powers-set-variables vps)))
    (vars-to-power-map vars vps)))


(defun variables-in-pp (pp accum)
  (if (null pp) accum
      (let* ((head (car pp)) (rest (cdr pp))
	     (var (car head)))
	(variables-in-pp rest (adjoin var accum)))))
	
(defun variables-in-mono (m)
  (variables-in-pp (cdr m) nil))

(defun variables-in-poly (p)
  (if (null p) nil
      (let ((head (car p)) (rest (cdr p)))
	(union (variables-in-mono head) (variables-in-poly rest)))))

(defun make-quad-surd (a b c sign)
  (make-surd-polynomial
   :a (prep:polyrepNegativePoly b)
   :b (if sign *poly-one* *poly-negative-one*) 
   :c (determinant a b c)
   :d (prep:polyrepMultiplyCstPoly 2 a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;         Constraints          ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (zero-constraint
	     (:include constraint)
	     (:print-function print-zero-constraint))
  "f = 0")

(defstruct (positive-constraint
	     (:include constraint)
	     (:print-function print-positive-constraint))
  "f > 0")

(defstruct (non-negative-constraint
	     (:include constraint)
	     (:print-function print-non-negative-constraint))
  "f >= 0")

(defstruct (non-zero-constraint
	     (:include constraint)
	     (:print-function print-non-zero-constraint))
  "f != 0")


(defmethod zero-constraint? ((c constraint))
  (typep c 'zero-constraint))

(defmethod non-zero-constraint? ((c constraint))
  (typep c 'non-zero-constraint))

(defmethod non-negative-constraint? ((c constraint))
  (typep c 'non-negative-constraint))

(defmethod positive-constraint? ((c constraint))
  (typep c 'positive-constraint))

(defmethod variable-is-constant? ((psi zero-constraint))
  "Returns true iff the constraint is equivalent to [b*x + c = 0]
   where b and c are constants."
  (let ((f (constraint-poly psi)))
    (if (null f) nil
	(let ((lmono (car f)) (g (cdr f)))
	  (and (poly-is-constant? g)
	       (mono-is-linear-subterm? lmono)
	       (not (poly-is-constant? f)) )))))

;; candidate-virtual-bounds
;; ------------------------
;; Given a variable and a constraint set generate a list of candidate substituions
;; and side conditions.
;; ConstraintSet -> Var -> Map (Extended-Polynomial, Constraint-Set)


(defmethod candidate-virtual-bounds ((cs constraint-set) x)
  (let* ((psis (cs-constraints cs))
	 (negative-inf-list (acons *neg-infty* *cs-true* nil))
	 (cand (candidate-virtual-bounds* psis x negative-inf-list)))
    (vformat 4 t "~% candidate-virtual-bounds: ~% ~a ~%" cand)
    cand))

(defun candidate-virtual-bounds* (psis x accum)
  (if (null psis)
      accum
      (let ((psi (car psis)) (rest (cdr psis)))
	(let ((naccum (candidate-virtual-bounds-constraint psi x accum)))
	  (candidate-virtual-bounds* rest x naccum)))))

(defun cnd-add-ep (epsilson sp)
  (if epsilson
      (make-epsilon-extended-polynomial :poly sp :sgn t)
      (make-plain-extended-polynomial :poly sp)))
      
(defmethod add-vb-to-accum ((ep extended-polynomial) (cs constraint-set) accum cnd)
  (if cnd
      (let ((res (assoc ep accum)))
	(if (null res)
	    (acons ep cs accum)
	    (acons ep (and-constraint-sets cs (cdr res)) accum)))
      accum))

(defun cvbc (a b c epsilon accum)
  "For e_0, e_1, e_2 see Weis pg. 7.
   Assumes constraint is ax^2 + bx + c ? 0 where ? is either =, <=, <, !=.
   epsilon is t iff ? is either < or !=."
  (assert (not (and (polyIsZero? a) (polyIsZero? b))))
  ;; This check is not strictly nessecary,
  ;; but is to defend against unnessecary virtual bounds.
  (let ((e_0 (cnd-add-ep epsilon (make-surd-polynomial :a (polyrepNegativePoly c) :d b)))
	(e_1 (cnd-add-ep epsilon (make-quad-surd a b c t)))
	(e_2 (cnd-add-ep epsilon (make-quad-surd a b c nil)))
	(det-geq-0 (p-is-non-negative-c (determinant a b c)))
	(a-is-0 (p-is-zero-c a))
	(degree-is-1 (polyIsZero? a))
	(a-not-0 (p-is-non-zero-c a))
	(b-not-0 (p-is-non-zero-c b)))
    (let* ((a0 (add-vb-to-accum e_0 (cs-list a-is-0 b-not-0) accum t))
	   (a1 (add-vb-to-accum e_1 (cs-list a-not-0 det-geq-0) a0 (not degree-is-1)))
	   (a2 (add-vb-to-accum e_2 (cs-list a-not-0 det-geq-0) a1 (not degree-is-1))))
      a2)))

(defmethod candidate-virtual-bounds-constraint ((psi constraint) x accum)
  (assert (<= (formal-degree-poly (constraint-poly psi) x) 2))
  (let ((upoly (univariate-rep (constraint-poly psi) x)))
    (let ((a (uv-coeff upoly 2))
	  (b (uv-coeff upoly 1))
	  (c (uv-coeff upoly 0)))
      
      (vformat 4 t "(constraint-poly psi) ~a~%" (constraint-poly psi))
      (vformat 4 t "a: ~a~%" a)
      (vformat 4 t "b: ~a~%" b)
      (vformat 4 t "c: ~a~%" c)
      (if (and (polyIsZero? a) (polyIsZero? b))
	  ;; The constraint does not contain x. Do not introduce candidate constraints
	  accum
	  (typecase psi
	    (zero-constraint (cvbc a b c nil accum))
	    (non-negative-constraint (cvbc
				      (polyrepNegativePoly a)
				      (polyrepNegativePoly b)
				      (polyrepNegativePoly c)
				      nil
				      accum))	
	    (positive-constraint (cvbc
				  (polyrepNegativePoly a)
				  (polyrepNegativePoly b)
				  (polyrepNegativePoly c)
				  t
				  accum))
	    (non-zero-constraint (cvbc a b c t accum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;        Poly Bound Maps       ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct (delta-constant
	     (:conc-name dc-))
  ;; delta-constant is infinite if dominant is nil
  ;; If delta-constant is inifinite it is either +/- inifity
  ;; delta-constant is +infty if dominant is nil and delta is +1
  ;; delta-constant is -infty if dominant is nil and delta is -1
  ;;
  ;; Otherwise, delta-constant is finite.
  dominant
  (delta 0 :type integer) ;;should be either -1,0,1
  explanation)

(defvar *dc-neg-infty*
  (make-delta-constant :dominant nil :delta -1 :explanation nil))
(defvar *dc-infty*
  (make-delta-constant :dominant nil :delta 1 :explanation nil))

(defmethod infinite? ((a delta-constant))
  (null (dc-dominant a)))
(defmethod infty? ((a delta-constant))
  (and (infinite? a) (= (dc-delta a) 1)))
(defmethod neg-infty? ((a delta-constant))
  (and (infinite? a) (= (dc-delta a) -1)))


(defmethod cmp-delta-constant ((a delta-constant) (b delta-constant))
  "Both a and b cannot be infinite."
  (assert (not (and (infinite? a) (infinite? b))))
  (cond ((infinite? a) (if (infty? a) 1 -1))
	((infinite? b) (if (infty? b) -1 1))
	(t (let ((a-dom (dc-dominant a)) (b-dom (dc-dominant b))
		 (a-delta (dc-delta a)) (b-delta (dc-delta b)))
	     (cond ((< a-dom b-dom) -1)
		   ((> a-dom b-dom) 1)
		   ((< a-delta b-delta) -1)
		   ((> a-delta b-delta) 1)
		   (t 0))))))

(defmethod dc-less? ((a delta-constant) (b delta-constant))
  (< (cmp-delta-constant a b) 0))
(defmethod dc-leq? ((a delta-constant) (b delta-constant))
  (<= (cmp-delta-constant a b) 0))
(defmethod dc-eq? ((a delta-constant) (b delta-constant))
  (= (cmp-delta-constant a b) 0))
(defmethod dc-greater? ((a delta-constant) (b delta-constant))
  (> (cmp-delta-constant a b) 0))
(defmethod dc-geq? ((a delta-constant) (b delta-constant))
  (>= (cmp-delta-constant a b) 0))

(defstruct (bound-information
	     (:conc-name bi-))
  poly
  (upperbound *dc-infty* :type delta-constant)
  ;; p <= u, delta-constant

  (lowerbound *dc-neg-infty* :type delta-constant)  
  ;; p >= l, delta-constant
  
  (excluded-values nil)
  ;; [p != c1, p!= c2, ...] ;; list of delta-constant

  ;; Invariant:
  ;; l <= c_i <= u in delta constant ordering.
)

(defmethod clone-bound-information (parent &key ub lb ex)
  (let ((new-ub (if (null ub) (bi-upperbound parent) ub))
	(new-lb (if (null lb) (bi-lowerbound parent) lb))
	(new-ex (if (null ex) (bi-excluded-values parent) ex)))
    (make-bound-information :poly (bi-poly parent)
			    :upperbound new-ub
			    :lowerbound new-lb
			    :excluded-values new-ex)))

;; If a new upperbound comes in, what are the possible outcomes
;; p <= u'
;; ---------
;; 1. A new bound-information must be made
;; 2. u <= u', so p <= u' can be dropped.
;; 3. u' < u, so p <= u can be dropped.
;; 4. l > u', so l <= p <= u' is a contradiction
;; 5. l = u', so l <= p <= u' and p = u'.
;; 7. p <= u' is not strict and u' is in excluded-values,
;;    then p < u' (i.e. can be added strict)
;;    also p != u' and p <= u' can be dropped
;; 8. l < u' < u, u' is not in excluded-values,
;;    then u' < c_i for c_i in excluded-values can be dropped.
;;
;; Lower bounds are isomorphic.
;;
;; If a new equality comes in, p = c,
;; 1. c is in excluded-values, contradiction.
;; 2. l, u, and c_i constraints can be dropped.
;; 
;;
;; Outcomes are
;; 1. Contradiction [Constraints],
;; 2. Subsumption Constraint [Constraints]
;;    where the list can be dropped and the constraint asserted
;; 3. Tighten Bounds-Information [Constraint]
;;    Tightened bounds information and a list of constraints to drop.
;; Union of these:
;; (values restype Bounds-Information [Constraint] [Constraint])
;;
;; restype is either lit-sat if the constraint is a duplicate,
;;  lit-unsat if a condradiction has been found,
;;  lit-unknown otherwise.
;; If restype is lit-sat,
;;   Everything else is nil.
;; If restype is lit-unsat,
;;   the first list of constraints is the condradiction.
;;   Everything else is undefined!
;; If restype is lit-unknown,
;;   the first list is the constraints to drop.
;;   The second list is constraints to assert.

(defmethod assert-ub ((bi bound-information) (u-prime delta-constant))
  ;; Ensure that, p <= u-prime
  (assert (not (infinite? u-prime)))
  (assert (or (= 0 (dc-delta u-prime)) (= -1 (dc-delta u-prime))))
  (let ((u (bi-upperbound bi))
	(l (bi-lowerbound bi))
	(excluded (bi-excluded-values bi))
	(up-exp (dc-explanation u-prime)))
    (cond ((dc-leq? u u-prime)
	   ;; u <= u', so p <= u' can be dropped.
	   (values :lit-sat nil nil nil))
	  ;; assume u' <= u
	  ((dc-less? u-prime l)
	   (values :lit-unsat nil (list (dc-explanation l) up-exp) nil))
	  ;; assume l <= u'
	  ((dc-eq? l u-prime)
	   (values :lit-unknown bi nil
		   (list (p-is-zero-c (constraint-poly up-exp)))))
	  ((member u-prime excluded :test #'dc-eq?)
	   ;; If p != u' and p <= u'  then p < u'
	   (values :lit-unknown bi 
		   (list (dc-explanation (find u-prime excluded :test #'dc-eq?)))
		   ;; An odd artifact is that lp-exp cannot be removed as well
		   ;; This requires 2 rounds of simplification
		   (list (p-is-positive-c (constraint-poly up-exp)))))
	  (t (let* ((gt-than-u-prime (lambda (x) (dc-greater? x u-prime)))
		    (new-ex (remove-if gt-than-u-prime excluded))
		    (new-bi (clone-bound-information bi :ub u-prime :ex new-ex))
		    (drop-ex (remove-if-not gt-than-u-prime excluded))
		    (to-drop (mapcar 'dc-explanation (cons u drop-ex))))
	       (values :lit-unknown
		       new-bi
		       (remove nil to-drop) ;;the explanation of u may be nil
		       nil))))))

(defmethod assert-lb ((bi bound-information) (l-prime delta-constant))
  ;; Ensure that, p >= l-prime
  (assert (not (infinite? l-prime)))
  (assert (or (= 0 (dc-delta l-prime)) (= 1 (dc-delta l-prime))))
  (let ((u (bi-upperbound bi))
	(l (bi-lowerbound bi))
	(excluded (bi-excluded-values bi))
	(lp-exp (dc-explanation l-prime)))
    (vformat 6 t "assert-lb~%")
    (vformat 6 t "l-prime: ~a ~%" l-prime)
    (vformat 6 t "l: ~a ~%" l)
    (vformat 6 t "u: ~a ~%" u)
    (vformat 6 t "bi: ~a ~%" bi)
    (cond ((dc-geq? l l-prime)
	   ;; l > l', so p >= l' can be dropped.
	   (values :lit-sat nil nil nil))
	  ;; assume l' <= l
	  ((dc-greater? l-prime u)
	   (values :lit-unsat nil (list (dc-explanation u) lp-exp) nil))
	  ;; assume l' <= u
	  ((dc-eq? u l-prime)
	   (values :lit-unknown bi nil
		   (list (p-is-zero-c (constraint-poly lp-exp)))))
	  ((member l-prime excluded :test #'dc-eq?)
	   ;; If p != l' and p >= l'  then p > l'
	   (values :lit-unknown bi
		   (list (dc-explanation (find l-prime excluded :test #'dc-eq?)))
		   ;; An odd artifact is that lp-exp cannot be removed as well
		   ;; This requires 2 rounds of simplification
		   (list (p-is-positive-c (constraint-poly lp-exp)))))
	  (t (let* ((lt-than-l-prime (lambda (x) (dc-less? x l-prime)))
		    (new-ex (remove-if lt-than-l-prime excluded))
		    (new-bi (clone-bound-information bi :lb l-prime :ex new-ex))
		    (drop-ex (remove-if-not lt-than-l-prime excluded))
		    (to-drop (mapcar 'dc-explanation (cons l drop-ex))))
	       (vformat 6 t "drop-ex: ~a ~%" drop-ex)
	       (vformat 6 t "to-drop: ~a ~%" to-drop)
	       (values :lit-unknown
		       new-bi
		       (remove nil to-drop) ;;the explanation of l may be nil
		       nil))))))

(defmethod assert-eq ((bi bound-information) (c delta-constant))
  (assert (not (infinite? c)))
  (assert (= 0 (dc-delta c)))
  (let ((u (bi-upperbound bi))
	(l (bi-lowerbound bi))
	(excluded (bi-excluded-values bi))
	(c-exp (dc-explanation c)))
    (cond
      ((and (dc-eq? c u)
	    (dc-eq? c l)
	    (equal (dc-explanation u) (dc-explanation l)))
       (values :lit-sat nil nil nil))
      ((dc-less? c l) ;; c < l is a conflict
       (vformat 3 t "c < l: ~a < ~a !%" c l)
       (values :lit-unsat nil (list (dc-explanation l) c-exp) nil))
      ((dc-greater? c u) ;; c > u
       (vformat 3 t "c > u: ~a < ~a !%" c u)
       (values :lit-unsat nil (list (dc-explanation u) c-exp) nil))
      ((member c excluded :test #'dc-eq?)
       (vformat 3 t "c in excluded: ~a \in ~a ~%" c excluded)
       (values :lit-unsat nil
	       (list (dc-explanation (find c excluded :test #'dc-eq?)) c-exp) nil))
      (t
       (vformat 3 t "t: ~%")
       (let ((to-drop (mapcar 'dc-explanation (cons u (cons l excluded))))
	     (new-bi (make-bound-information :lowerbound c
					     :upperbound c
					     :poly (bi-poly bi))))
	 (values :lit-unknown
		 new-bi
		 (remove nil to-drop) ;;the explanation of u and l may be nil
		 nil))))))

(defmethod assert-diseq ((bi bound-information) (c delta-constant))
  (assert (not (infinite? c)))
  (assert (= 0 (dc-delta c)))
  (let ((u (bi-upperbound bi))
	(l (bi-lowerbound bi))
	(excluded (bi-excluded-values bi))
	(c-exp (dc-explanation c)))
    (cond ((member c excluded :test #'dc-eq?)
	   (values :lit-sat nil nil nil))
	  ((and (dc-eq? c u) (dc-eq? c l))
	   (values :lit-unsat nil
		   (list (dc-explanation l) (dc-explanation u) c-exp) nil))
	  ((dc-eq? c l) ;; p != c and p >= l, then p > c
	   (values :lit-unknown bi nil
		   (list (p-is-positive-c (constraint-poly (dc-explanation l))))))
	  ((dc-eq? c u) ;; p != c and p <= u, then p < c
	   (values :lit-unknown bi nil
		   (list (p-is-positive-c (constraint-poly (dc-explanation u))))))

	  (t
	   (values :lit-unknown
		   (clone-bound-information bi :ex (cons c excluded))
		   nil
		   nil)))))

(defmethod push-onto-bound-information ((zc zero-constraint) bis)
  (assert (not (poly-is-constant? (constraint-poly zc))))
  (assert (leading-coeff-is-one? (constraint-poly zc)))
  (multiple-value-bind (p c) (removeConstantPart (constraint-poly zc))
    (let* ((dc (make-delta-constant :dominant (- c) :delta 0 :explanation zc))
	   (bi (assoc p bis :test #'prep:polyrepEqual?))
	   (bi* (if (null bi) (make-bound-information :poly p) (cdr bi))))
      (assert-eq bi* dc))))

(defmethod push-onto-bound-information ((nzc non-zero-constraint) bis)
  (assert (not (poly-is-constant? (constraint-poly nzc))))
  (assert (leading-coeff-is-one? (constraint-poly nzc)))
  (multiple-value-bind (p c) (removeConstantPart (constraint-poly nzc))
    (let* ((dc (make-delta-constant :dominant (- c) :delta 0 :explanation nzc))
	   (bi (assoc p bis :test #'prep:polyrepEqual?))
	   (bi* (if (null bi) (make-bound-information :poly p) (cdr bi))))
      (assert-diseq bi* dc))))

(defun push-greater-onto-bound-information* (poly exp bis strict)
  (assert (not (poly-is-constant? poly)))
  (assert (leading-coeff-is-plus-minus-one? poly))
  (let* ((is-ub (leading-coeff-is-minus-one? poly))
	 (norm-p (if is-ub (prep:polyrepNegativePoly poly) poly)))
    (multiple-value-bind (p c) (removeConstantPart norm-p)
      (let* ((dc (make-delta-constant :dominant (- c)
				      :delta (if strict (if is-ub -1 1) 0)
				      :explanation exp))
	     (bi (assoc p bis :test #'prep:polyrepEqual?))
	     (bi* (if (null bi) (make-bound-information :poly p) (cdr bi))))
	(if is-ub
	    (assert-ub bi* dc)
	    (assert-lb bi* dc))))))
  

(defmethod push-onto-bound-information ((pc positive-constraint) bis)
  (push-greater-onto-bound-information* (constraint-poly pc) pc bis t))

(defmethod push-onto-bound-information ((nnc non-negative-constraint) bis)
  (push-greater-onto-bound-information* (constraint-poly nnc) nnc bis nil))

;; (values any-effect is-consistent psis)
(defun process-constraint-list* (cs bis any-effect to-keep to-add)
  (if (null cs)
      (if any-effect
	  (values t t (append to-keep to-add))
	  (values nil t nil))
      (let ((c (car cs)) (rest (cdr cs)))
	(multiple-value-bind (restype new-bi l0 l1)
	    (push-onto-bound-information c bis)
	  (vformat 6 t "push-onto-bound-information c ~a ~%" c)
	  (vformat 6 t "bis: ~a ~%" bis)
	  (vformat 6 t "restype: ~a l0: ~a l1: ~a ~%" restype l0 l1)
	  (case restype
	    (:lit-sat (process-constraint-list* rest bis t to-keep to-add))
	    (:lit-unsat (values t nil l0))
	    (:lit-unknown
	     (process-constraint-list*
	      rest
	      (acons (bi-poly new-bi) new-bi bis)
	      (or any-effect (not (null l1)) (not (null l0)))
	      (cons c (set-difference to-keep l0))
	      (append l1 to-add))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;           VTS Nodes          ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *vts-id-dst* 0
  "A global variable for giving out unique id's to new vts nodes")

(defun get-unique-id ()
  (let ((curr *vts-id-dst*))
    (setf *vts-id-dst* (+ 1 *vts-id-dst*))
    curr))

(defun reset-ids ()
  (setf *vts-id-dst* 0))

(deftype node-status ()
  '(member
    :status-leaf
    :status-projection
    :status-mixed-projection
    :status-substitution
    :status-simplification
    :status-unsat
    :status-sat
    :status-blocked))

(defstruct (vts-node
	     (:conc-name vts-))
  "Each node has a set of constraints, and a substitution trail."
  id
  ;; A unique node id. This should be used for debugging purposes only!

  (witness *cs-true* :type constraint-set)
  ;; A node is considered to be logically equivalent to the virtual substitutions
  ;; applied to the witness of the node.

  (substitutions nil)
  ;; AList Variables -> Extended Polynomials
  ;; An associative list mapping variables to extended polynomials
  ;; Each association indicates the virtual substitution test case
  ;; for projecting a variable.
  ;; Each association must be unique.

  (status :status-leaf :type node-status)
  ;; The node's status

  (explanation nil)
  ;; Documents the reason for the equivalence of the children.
  ;; Currently used for debugging purposes only

  (children nil)
  ;; If a node has children, 
  ;; then the node is logically equivalent to the disjunction of its children
)

;; (defun print-vts-node (n stream depth)
;;   (let ((id (vts-id n))
;; 	(wit (vts-witness n))
;; 	(subs (vts-substitutions n))
;; 	(status (vts-status n))
;; 	(exp (vts-explanation n))
;; 	(children (vts-children n)))
;;     (format stream "~va(Node: ~a~%" depth "   " id)
;;     (format stream "~viStatus: ~a~%" depth status)
;;     (format stream "~viWitness: ~a~%" depth wit)
;;     (format stream "~viSubstitutions: ~a~%" depth subs)
;;     (format stream "~viexplanation: ~a~%" depth exp)
;;     (format stream "~vichildren:~%" depth)
;;     (format stream "~vi~a~%" depth children)
;;     (format stream "~vi)" depth)))

(defmethod vts-is-status? ((n vts-node) s)
  (eq (vts-status n) s))

;; vts-leaf?
;; ------------
;; If n is a leaf, returns t.
;; If n is not a leaf, return nil

(defmethod vts-leaf? ((n vts-node))
  (vts-is-status? n :status-leaf))

(defmethod vts-blocked? ((n vts-node))
  (vts-is-status? n :status-blocked))

(defmethod vts-projection? ((n vts-node))
  (vts-is-status? n :status-projection))

(defmethod vts-mixed-projection? ((n vts-node))
  (vts-is-status? n :status-mixed-projection))

(defmethod vts-substitution? ((n vts-node))
  (vts-is-status? n :status-substitution))

(defmethod vts-simplification? ((n vts-node))
  (vts-is-status? n :status-simplification))

(defmethod vts-sat? ((n vts-node))
  (vts-is-status? n :status-sat))

(defmethod vts-unsat? ((n vts-node))
  (vts-is-status? n :status-unsat))


(defmethod clone-node ((n vts-node) &key witness substitutions)
  (let ((cwit (if (null witness) (vts-witness n) witness))
	(csub (if (null substitutions) (vts-substitutions n) substitutions))
	(cid (get-unique-id)))
    (make-vts-node :witness cwit :substitutions csub :id cid)))


(defmethod add-child ((n vts-node) (c vts-node))
  (setf (vts-children n) (cons c (vts-children n))))

(defmethod set-node-status ((n vts-node) s)
  (setf (vts-status n) s))

(defmethod set-node-explanation ((n vts-node) e)
  (assert (null (vts-explanation n)))
  (setf (vts-explanation n) e))


;; simplify-node
;; -------------
;; Applies simplifications to a leaf vts-node.
;; 
;; Returns nil if no simplifications succeeded and returns n otherwise.
;; The side-effect of this function if it succeeds is to add children to n
;; and change the status of n to status-simplification.
(defmethod simplify-node ((n vts-node))
  (assert (vts-leaf? n))
  (cond ((simp-remove-trivially-sat-constraints n) t)
	((simp-factor-pp-gcd n) t)
	((simp-pp-constraints n) t)
	((simp-by-bounds-info n) t)
	(t nil)))

(defmethod simp-remove-trivially-sat-constraints ((n vts-node))
  (assert (vts-leaf? n))
  (let* ((psis (cs-constraints (vts-witness n)))
	 (sats (remove-if-not 'constant-constraint-is-sat psis)))
    (if (null sats) nil
	(progn (set-node-status n :status-simplification)
	       (add-child n (clone-node n :witness (make-constraint-set :constraints (set-difference psis sats))))
	       t))))

(defun find-pp-gcd-in-poly (f)
  "If f has at least 2 monomials, returns the gcd of the monomials.
   Otherwise, returns nil."
  (if (poly-is-sum? f)
      (reduce 'polyrepGCD-PP (mapcar 'monoPP f))
      nil))

(defun find-pp-gcd-in-list (psis)
  "Returns a pair of a power product and a constraint in a list of constraints.
   The power product factors the constraint."
  (if (null psis) (values nil nil)
      (let* ((psi (car psis)) (rest (cdr psis))
	     (pp (find-pp-gcd-in-poly (constraint-poly psi))))
	(if (null pp) (find-pp-gcd-in-list rest)
	    (values pp psi)))))
  
       
(defmethod constraint-factors-pp ((psi constraint) pp)
  "Given f ? 0 and pp s.t. pp divides f, returns (values pp (divide f pp))."
  (let ((f (constraint-poly psi))
	(pp-poly (pp-to-poly pp)))
      (assert (poly-is-sum? f))
      (let ((f-div-pp (prep:polyrepDividePolyPP f pp)))
	(values f-div-pp pp-poly))))

(defmethod factor-constraint-by-pp ((psi zero-constraint) pp)
  "Suppose psi: f = 0. f : p * q. then p = 0 or q = 0.
   pp is a power product that divides f. 
   f must have at least 2 monomials."
  (negate-dnf (factor-constraint-by-pp (negate-constraint psi) pp)))

(defmethod factor-constraint-by-pp ((psi non-zero-constraint) pp)
  "Suppose psi: f != 0. f : p * q. then p != 0 and q != 0.
   pp is a power product that divides f. 
   f must have at least 2 monomials."
  (multiple-value-bind
   (p q) (constraint-factors-pp psi pp)
   (dnf-from-cs (cs-list (p-is-non-zero-c p)
			 (p-is-non-zero-c q)))))

(defmethod factor-constraint-by-pp ((psi positive-constraint) pp)
  "Suppose psi: f > 0. f : p * q. then (p > 0 and q >0) or (p < 0 and q <0)
   pp is a power product that divides f. 
   f must have at least 2 monomials."
  (multiple-value-bind
	(p q) (constraint-factors-pp psi pp)
    (signs-are-the-same p q t)))

(defmethod factor-constraint-by-pp ((psi non-negative-constraint) pp)
  "Suppose psi: f >= 0. f : p * q.
   Then (p >= 0 and q >= 0) or (p <= 0 and q <= 0)
   pp is a power product that divides f. 
   f must have at least 2 monomials."
  (multiple-value-bind
	(p q) (constraint-factors-pp psi pp)
    (signs-are-the-same p q nil)))

(defmethod simp-factor-pp-gcd ((n vts-node))
  "Looks for a constraint with a polynomial f s.t.
   f is a sum of at least 2 monomials and f can be factorized.
   Or, f : p*q and f : s + t where s and t are non zero.
   The constraint is replaced by equivalent sign constraints on p and q."
  (assert (vts-leaf? n))
  (let ((psis (cs-constraints (vts-witness n))))
    (multiple-value-bind (pp psi) (find-pp-gcd-in-list psis)
      (if (null pp)
	  nil
	  (let ((cases (factor-constraint-by-pp psi pp)))
	    (case-split-node n psi cases :status-simplification)
	    (vformat 2 t "simp-factor-pp-gcd")
	    t)))))

(defun poly-is-pp? (p)
  (vformat 5 t "p: ~a,(polyIsZero? p):~a (poly-is-sum? p):~a (prep:polyrepTotalDegreePoly p):~a"
	  p
	  (polyIsZero? p)
	  (poly-is-sum? p)
	  (prep:polyrepTotalDegreePoly p))
  (vformat 5 t "poly-is-pp? ~a ~%"
	  (and (not (polyIsZero? p))
	       (not (poly-is-sum? p))
	       (<= 2 (prep:polyrepTotalDegreePoly p))))
  (and (not (polyIsZero? p))
       (not (poly-is-sum? p))
       (<= 2 (prep:polyrepTotalDegreePoly p))))

(defun find-pp-constraint-in-list (psis)
  "Returns a constraint whose polynomial is a power product of total degree at least 2."
  (find-if (lambda (x) (poly-is-pp? (constraint-poly x))) psis))

(defmethod pp-sign-conditions ((zc zero-constraint))
  "\prod (x_i . j) = 0 iff (or [x_1 = 0] [x_2 = 0] ...)"
  (assert (poly-is-pp? (constraint-poly zc)))
  (let ((pp (monoPP (car (constraint-poly zc)))))
    (or-dnf-list
     (mapcar (lambda (x) (dnf-from-constraint (p-is-zero-c (var-to-polyrep x))))
	     (variables-in-pp pp nil)))))
    
(defmethod pp-sign-conditions ((nzc non-zero-constraint))
  "\prod (x_i . p_i) != 0 iff !(\prod (x_i . j) = 0)"
  (assert (poly-is-pp? (constraint-poly nzc)))
  (negate-dnf (pp-sign-conditions (negate-constraint nzc))))

(defun sign-agreement (vars coeff)
  "Logically equivalent to [coeff * \prod v_i > 0].
   coeff is +/0."
  (assert (or (= -1 coeff) (= 1 coeff)))
  (cond
    ((null vars) (dnf-from-constraint (p-is-positive-c (toPolyRepCnst coeff))))
    ((null (cdr vars))
	   (let ((c-times-v (prep:polyrepMultiplyCstPoly coeff (var-to-polyrep (car vars)))))
	     (dnf-from-constraint (p-is-positive-c c-times-v))))
    (t
     (let* ((v (car vars)) (rest (cdr vars))
	    (v-is-pos (dnf-from-constraint (p-is-positive-c (var-to-polyrep v))))
	    (v-is-neg (dnf-from-constraint (p-is-positive-c (prep:polyrepNegativePoly (var-to-polyrep v)))))
	    (rest-pos (sign-agreement rest coeff))
	    (rest-neg (sign-agreement rest (* -1 coeff))))
       ;; v_1 * [coeff * \prod_{i=2}^n v_i] > 0 iff
       ;; v_1 > 0 and [coeff * \prod_{i=2}^n v_i > 0] or
       ;; v_1 < 0 and [coeff * \prod_{i=2}^n v_i < 0]
       (or-dnfs (and-dnfs v-is-pos rest-pos) 
		(and-dnfs v-is-neg rest-neg))))))

(defun pp-parity-decomposition (pp evens odds)
  (if (null pp)
      (values evens odds)
      (let* ((head (car pp)) (rest (cdr pp))
	     (var (car head)) (pow (cdr head))
	     (is-even (= 0 (mod pow 2))))
	(pp-parity-decomposition rest
				 (if is-even (cons var evens) evens)
				 (if is-even odds (cons var odds))))))

(defmethod pp-sign-conditions ((pc positive-constraint))
  (assert (poly-is-pp? (constraint-poly pc)))
  (let* ((poly (constraint-poly pc))
	 (pp (monoPP (car poly)))
	 (coeff (leading-coeff poly)))
    (multiple-value-bind (evens odds)
	(pp-parity-decomposition pp nil nil)
      (vformat 3 t "p: ~a evens: ~a odds ~a ~%" poly evens odds)
      (let* ((evens-as-polys (mapcar 'var-to-polyrep evens))
	     (evens-prod (reduce 'prep:polyrepMultiplyPoly evens-as-polys :initial-value *poly-one*))
	     (evens-prod-non-zero (p-is-non-zero-c evens-prod))
	     (even-conds (if (poly-is-pp? evens-prod)
			     (pp-sign-conditions evens-prod-non-zero)
			     (dnf-from-constraint evens-prod-non-zero))))
	(and-dnfs even-conds
		  (sign-agreement odds coeff))))))
  
(defmethod pp-sign-conditions ((nnc non-negative-constraint))
  "c* \prod (x_i . p_i) >= 0 iff !(-1*c*\prod (x_i . j) < 0)"
  (assert (poly-is-pp? (constraint-poly nnc)))
  (negate-dnf (pp-sign-conditions (negate-constraint nnc))))
  


(defmethod simp-pp-constraints ((n vts-node))
  "Looks for a constraint that is the relationship between
   a power product of total degree at least 2.
   Replace this with equivalent sign constraints on the variables."
  (assert (vts-leaf? n))
  (let* ((psis (cs-constraints (vts-witness n)))
	 (psi (find-pp-constraint-in-list psis)))
    (if (null psi)
	nil
	(let ((cases (pp-sign-conditions psi)))
	  (case-split-node n psi cases :status-simplification)
	  (vformat 2 t "simp-pp-constraints")
	  t))))

(defmethod simp-by-bounds-info ((n vts-node))
  "Looks for constraints related by constants,
    i.e. p + c ? 0 and p ? d
   and simplifies the constraints using this information."
  (assert (vts-leaf? n))
  (multiple-value-bind (any-effect consistent new-psis)
      (process-constraint-list* (cs-constraints (vts-witness n)) nil nil nil nil)
    (vformat 6 t "process-constraint-list* ~a ~a ~a ~%" any-effect consistent new-psis)
    (let ((new-psis-cs (make-constraint-set :constraints new-psis)))
      (cond ((not any-effect) nil)
	    ((null consistent) 
	     (progn (set-node-status n :status-unsat)
		    (set-node-explanation n new-psis-cs)
		    t))
	    (t (progn (set-node-status n :status-simplification)
		      (add-child n (clone-node n :witness new-psis-cs))
		      t))))))

;; is-trivially-unsat
;; -------------
;; If a node's witness contains a constant polynomial that is unsatisfiable,
;; the node is trivially unsat.
;; 
;; Returns nil if the node is not trivially unsat.
;; Returns t if the node is trivially unsat.
;; The node's status is set to :status-unsat.
(defmethod is-trivially-unsat ((n vts-node))
  (assert (vts-leaf? n))
  (let ((psis (cs-constraints (vts-witness n))))
    (let ((uc (find-if 'constant-constraint-is-unsat psis)))
      (if (null uc) nil	  
	  (progn (set-node-status n :status-unsat)
		 (set-node-explanation n uc)
		 t)))))

;; is-trivially-sat
;; -------------
;; If a node's witness is empty, then it is trivially true.
;; 
;; Returns nil if the node is not trivially sat.
;; Returns t if the node is trivially sat.
;; The node's status is set to :status-sat.
(defmethod is-trivially-sat ((n vts-node))
  (assert (vts-leaf? n))
  (if (constraint-set-is-true (vts-witness n))
      (progn (set-node-status n :status-sat)
	     t)
      nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;          Case Split          ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod case-split-node*
    ((n vts-node) (nopsi constraint-set) cases)
  (if (not (null cases))
      (let* ((head (car cases)) (rest (cdr cases))
	     (ncs (and-constraint-sets nopsi head))
	     (child (clone-node n :witness ncs)))
	(progn (add-child n child)
	       (case-split-node* n nopsi rest)))))


(defmethod case-split-node
    ((n vts-node) (psi constraint) (phi dnf) status)
  "Replaces a constraint in the witness of n, psi,
   with an dnf, phi, that is equivalent (under the current assumptions)
   If n has the witness (and psi rho),
   then child_i of n has the witness (and phi_i rho).
   Sets the status of n to status.
   Sets the explanation of n to (psi . phi)."
  (vformat 2 t "case-split-node")
  (vformat 2 t "psi: ~a~%" psi)
  (vformat 2 t "phi: ~a~%" phi)
  (assert (dnf-well-formed? phi))
  (let* ((nopsi-list (remove psi (cs-constraints (vts-witness n))))
	 (nopsi-cs (make-constraint-set :constraints nopsi-list))
	 (case-list (dnf-conjunctions phi)))
    (progn (set-node-status n status)
	   (set-node-explanation n (cons psi phi))
	   (case-split-node* n nopsi-cs case-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;     Virtual Substitution     ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun coefficients-are-zeroes (f var)
  "For f = \sum a_i var^i, returns a DNF equivalent to (and (a_i = 0) ...))"
  (let* ((uvf (univariate-rep f var))
	 (a_is (univariate-coeff-list uvf))
	 (a_is-are-zeroes (zeroes-list a_is)))
    (dnf-from-cs (make-constraint-set :constraints a_is-are-zeroes))))

(defmethod substitution-cases
    ((psi zero-constraint) (e infty-extended-polynomial) var)
  "(f = 0)[-infty/x] : (and (a_i = 0) ...) for f = \sum a_i var^i"
  (coefficients-are-zeroes (constraint-poly psi) var))

(defmethod substitution-cases
    ((psi non-zero-constraint) (e infty-extended-polynomial) var)
  "(f != 0)[-infty/x] : (not (f = 0)[-infty/x])"
  (negate-dnf (substitution-cases (negate-constraint psi) e var)))

(defmethod substitution-cases
    ((psi non-negative-constraint) (e infty-extended-polynomial) var)
  "(f >= 0)[-infty/x] : (f > 0)[-infty/x] or (f = 0)[-infty/x]"
  (let* ((f (constraint-poly psi))
	 (f-pos (p-is-positive-c f))
	 (f-pos-cases (substitution-cases f-pos e var))
	 (f-zero (p-is-zero-c f))
	 (f-zero-cases (substitution-cases f-zero e var)))
    (or-dnfs f-pos-cases f-zero-cases)))
	 
(defmethod substitution-cases
    ((psi positive-constraint) (e infty-extended-polynomial) var)
  "(-f < 0)[-infty/x] : mew(f)"
  (let* ((f (constraint-poly psi))
	 (neg-f (prep:polyrepNegativePoly f))
	 (uv-neg-f (univariate-rep neg-f var)))
    (mew uv-neg-f)))

(defun mew (f)
  "f is in univariate representation w.r.t. x and f is not empty."
  (if (null f)
      *dnf-false* ;; if f is null, it is the 0 polynomial. mew(0) = false.
      (let* ((highest-prod (car f))
	     (rest (cdr f))
	     (n (cdr (car highest-prod)))
	     (a_n (cdr highest-prod))
	     (neg-lead? (if (evenp n) (prep:polyrepNegativePoly a_n) a_n)) ;; -1*(-1)^n * a_n
	     (neg-lead?-is-positive? (dnf-from-constraint (p-is-positive-c neg-lead?)))
	     (a_n-is-zero (dnf-from-constraint (p-is-zero-c a_n))))
	(assert (dnf-well-formed? neg-lead?-is-positive?))
	(assert (dnf-well-formed? a_n-is-zero))
	(if (= n 0)
	    neg-lead?-is-positive?
	    (let* ((m-rest (mew rest))
		   (smaller-n-is-positive? (and-dnfs a_n-is-zero m-rest))
		   (either (or-dnfs neg-lead?-is-positive? smaller-n-is-positive?)))
	      (assert (dnf-well-formed? m-rest))
	      (assert (dnf-well-formed? smaller-n-is-positive?))
	      (assert (dnf-well-formed? either))
	      either)))))

(defmethod surd-rel-zero-poly ((sp surd-polynomial))
  "Returns a polynomial equal to [a^2 - (b^2 * c)]."
  ;;(assert (not (polyIsZero? c)))
  (let* ((a (surd-a sp))
	 (b (surd-b sp))
	 (c (surd-c sp))
	 (a-square (prep:polyrepExpPolyCst a 2))
	 (b-square (prep:polyrepExpPolyCst b 2))
	 (b-sq-c (prep:polyrepMultiplyPoly b-square c))
	 (neg-b-sq-c (prep:polyrepNegativePoly b-sq-c)))
    (prep:polyrepAddPoly a-square neg-b-sq-c)))

(defun signs-differ (x y strict)
  "Returns a dnf: (x > 0 and y >0) or (x < 0 and y < 0).
   Logically equivalent to x*y < 0.
   If strict, use >. Else use, >=."
  (let ((x-is-pos (p-greater-than-zero-c x strict))
	(x-is-neg (p-greater-than-zero-c (prep:polyrepNegativePoly x) strict))
	(y-is-pos (p-greater-than-zero-c y strict))
	(y-is-neg (p-greater-than-zero-c (prep:polyrepNegativePoly y) strict)))
    (or-dnfs
     (dnf-from-cs (cs-list x-is-pos y-is-neg))
     (dnf-from-cs (cs-list y-is-pos x-is-neg)))))

(defun signs-are-the-same (x y strict)
  "Returns a dnf: (p > 0 and q >0) or (p < 0 and q <0).
   Logically equivalent to p*q > 0.
   If strict, use >. Else use, >=."
  (let ((x-is-pos (p-greater-than-zero-c x strict))
	(x-is-neg (p-greater-than-zero-c (prep:polyrepNegativePoly x) strict))
	(y-is-pos (p-greater-than-zero-c y strict))
	(y-is-neg (p-greater-than-zero-c (prep:polyrepNegativePoly y) strict)))
    (or-dnfs
     (dnf-from-cs (cs-list x-is-pos y-is-pos))
     (dnf-from-cs (cs-list y-is-neg x-is-neg)))))

(defmethod substitution-cases
    ((psi zero-constraint) (e plain-extended-polynomial) var)
  (let* ((sp (plain-extended-polynomial-poly e))
	 (f (constraint-poly psi))
	 (sub-x-in-f (substitute-surd-into-poly f sp var)))
    (if (surd-rad-is-zero? sub-x-in-f)
	;; The test case assumptions guarenteee d != 0.
	;; sub-x-in-f: a / d
	;; then f[e//x] = 0 iff a = 0
	(let* ((a (surd-a sub-x-in-f))
	       (a-is-zero (p-is-zero-c a)))
	  (dnf-from-constraint a-is-zero))
	;; sub-x-in-f: a + b * sqrt(c) / d
	;; then f[e//x] = 0 iff
	;;      a*b <= 0 and a^2 - b^2 c = 0
	(let* ((srzp (surd-rel-zero-poly sub-x-in-f))
	       (srzp-is-zero (p-is-zero-c srzp))
	       (sign-cases (signs-differ (surd-a sub-x-in-f) (surd-b sub-x-in-f) nil)))
	  (and-dnfs (dnf-from-constraint srzp-is-zero) sign-cases)))))


(defmethod substitution-cases
    ((psi non-negative-constraint) (e plain-extended-polynomial) var)
  (let* ((sp (plain-extended-polynomial-poly e))
	 (f (constraint-poly psi))
	 (neg-f (prep:polyrepNegativePoly f))
	 (sub-x-in-f (substitute-surd-into-poly neg-f sp var))
	 (d (surd-d sp))
	 (d-delta (prep:polyrepExpPolyCst d (power-parity neg-f var)))
	 (ad-sign-cases (signs-differ (surd-a sub-x-in-f) d-delta nil)))
    (if (surd-rad-is-zero? sub-x-in-f)
	;; The test case assumptions guarenteee d != 0.
	;; sub-x-in-f: a / d
	;; then f[e//x] <= 0 iff [a * d^delta <= 0]
	ad-sign-cases
	;; sub-x-in-f: a + b * sqrt(c) / d
	;; then f[e//x] <= 0 iff
	;;    [a * d^delta <= 0 and 0 <= a^2 - b^2 c] or
	;;    [b * d^delta <= 0 and a^2 - b^2 c <= 0]
	(let* ((srzp (surd-rel-zero-poly sub-x-in-f))
	       (srzp-is-non-negative (p-is-non-negative-c srzp))
	       (srzp-is-non-positive (p-is-non-negative-c (prep:polyrepNegativePoly srzp)))
	       (bd-sign-cases (signs-differ (surd-b sub-x-in-f) d-delta nil)))
	  (or-dnfs
	   (and-dnfs ad-sign-cases (dnf-from-constraint srzp-is-non-negative))
	   (and-dnfs bd-sign-cases (dnf-from-constraint srzp-is-non-positive)))))))


(defmethod substitution-cases
    ((psi positive-constraint) (e plain-extended-polynomial) var)
  (let* ((sp (plain-extended-polynomial-poly e))
	 (f (constraint-poly psi))
	 (neg-f (prep:polyrepNegativePoly f))
	 (sub-x-in-f (substitute-surd-into-poly neg-f sp var))
	 (d (surd-d sp))
	 (d-delta (prep:polyrepExpPolyCst d (power-parity neg-f var)))
	 (ad-sign-cases (signs-differ (surd-a sub-x-in-f) d-delta t)))
    (if (surd-rad-is-zero? sub-x-in-f)
	;; The test case assumptions guarenteee d != 0.
	;; sub-x-in-f: a / d
	;; then f[e//x] < 0 iff [a * d^delta < 0]
	ad-sign-cases
	;; sub-x-in-f: a + b * sqrt(c) / d
	;; then f[e//x] < 0 iff
	;;    [a * d^delta < 0 and 0 < a^2 - b^2 c] or
	;;    [b * d^delta <= 0 and (a * d^delta < 0 or a^2 - b^2 c < 0)]
	(let* ((srzp (surd-rel-zero-poly sub-x-in-f))
	       (srzp-is-pos (p-is-positive-c srzp))
	       (srzp-is-neg (p-is-positive-c (prep:polyrepNegativePoly srzp)))
	       (bd-sign-cases (signs-differ (surd-b sub-x-in-f) d-delta nil)))
	  (or-dnfs
	   (and-dnfs ad-sign-cases (dnf-from-constraint srzp-is-pos))
	   (and-dnfs bd-sign-cases (or-dnfs ad-sign-cases
					    (dnf-from-constraint srzp-is-neg))))))))


(defmethod substitution-cases
    ((psi non-zero-constraint) (e plain-extended-polynomial) var)
  (let ((f (constraint-poly psi)))
    (negate-dnf (substitution-cases (p-is-zero-c f) e var))))

(defmethod substitution-cases
    ((psi zero-constraint) (e epsilon-extended-polynomial) var)
  (coefficients-are-zeroes (constraint-poly psi) var))

(defmethod nu (f var (e plain-extended-polynomial))
  (let ((n (formal-degree-poly f var))
	(f-is-neg (p-is-positive-c (prep:polyrepNegativePoly f))))
    (if (= n 0)
	(dnf-from-constraint f-is-neg)
	(let* ((f-is-zero (p-is-zero-c f))
	       (f-is-zero-vt (substitution-cases f-is-zero e var))
	       (f-is-neg-vt (substitution-cases f-is-neg e var))
	       (f-prime (formal-derivative f var)))
	  (or-dnfs f-is-neg-vt
		   (and-dnfs f-is-zero-vt
			     (nu f-prime var e)))))))
	

(defmethod substitution-cases
    ((psi positive-constraint) (e epsilon-extended-polynomial) var)
  (assert (epsilon-extended-polynomial-sgn e))
  ;; Only sp + epsilon is currently supported
  (let* ((f (constraint-poly psi))
	 (sp (epsilon-extended-polynomial-poly e))
	 (neg-f (prep:polyrepNegativePoly f))
	 (no-ep-e (make-plain-extended-polynomial :poly sp))
	 (nu-res (nu neg-f var no-ep-e)))
    (vformat 4 t "(substitution-cases ~a ~a ~a)" psi e var)
    (vformat 4 t "-> ~a ~%" nu-res)
    nu-res))

(defmethod substitution-cases
    ((psi non-negative-constraint) (e epsilon-extended-polynomial) var)
  (let* ((f (constraint-poly psi))
	 (f-is-pos (p-is-positive-c f))
	 (f-is-zero (p-is-zero-c f)))
    (or-dnfs (substitution-cases f-is-pos e var)
	     (substitution-cases f-is-zero e var))))

(defmethod substitution-cases
    ((psi non-zero-constraint) (e epsilon-extended-polynomial) var)
  "(f != 0)[e + epsilon/x] : (not (f = 0)[e + epsilon/x])"
  (negate-dnf (substitution-cases (negate-constraint psi) e var)))
	 

(defmethod substitution-cases
    ((psi constraint) (e extended-polynomial) var)
  (vformat 2 t "substitution-cases [~a/~a]~%" e var)
  (vformat 2 t "into ~a~%" psi)
  (error "Not implemented yet"))

(defmethod substitution-applies
    ((psi constraint) subs)
  (let* ((p (constraint-poly psi))
	 (vars (variables-in-poly p)))
    (assoc-if (lambda (x) (not (null (find x vars)))) (reverse subs))))

(defmethod find-substitution (subs psis)
  (if (null psis) nil
      (let* ((head (car psis)) (rest (cdr psis))
	     (res (substitution-applies head subs)))
	(if (null res)
	    (find-substitution subs rest)
	    (cons res head)))))

(defmethod apply-substitutions ((n vts-node))
  (assert (vts-leaf? n))
  (let ((psis (cs-constraints (vts-witness n)))
	(subs (vts-substitutions n)))
  (let ((sub-psi (find-substitution subs psis)))
    (if (null sub-psi) nil
	(let* ((sub (car sub-psi)) (psi (cdr sub-psi))
	       (var (car sub)) (e (cdr sub))
	       (cases (substitution-cases psi e var)))
	  (progn (case-split-node n psi cases :status-substitution) t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;      Variable Projection     ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun var-powers-max (vp-map)
  (let ((vp-max-fn (lambda (vp-list)
		     (let ((var (car vp-list)) (powers (cdr vp-list)))
		       (cons var (reduce #'max powers))))))
    (mapcar vp-max-fn vp-map)))

(defun filter-candidates (vp-max)
  (let* ((in-range (lambda (x) (<= (cdr x) 2)))
	 (sat-mm (remove-if-not in-range vp-max)))
    (mapcar 'first sat-mm)))

(defun select-candidate (vars)
  (assert (not (null vars)))
  (car vars))

(defmethod add-substitutions* ((n vts-node) x subs)
  (if (not (null subs))
      (let* ((head (car subs))
	     (rest (cdr subs))
	     (e (car head))
	     (psi (cdr head))
	     (cwit (and-constraint-sets psi (vts-witness n)))
	     (csub (acons x e (vts-substitutions n)))
	     (child (clone-node n :witness cwit :substitutions csub)))
	(progn (add-child n child)
	       (add-substitutions* n x rest)))))

(defmethod add-substitutions ((n vts-node) x subs)
  (progn (vformat 4 t "add-substitutions ~a~%" n)
	 (set-node-status n :status-projection)
	 (set-node-explanation n subs)
	 (add-substitutions* n x subs)
	 (vformat 4 t "~a~%" n)))

(defun is-degree? (f x d)
  (= d (formal-degree-poly f x)))

(defmethod vars-of-degree-in-constraint ((c constraint) degree)
  (let* ((f (constraint-poly c))
	 (vp (var-powers-constraint* c))
	 (vars (var-powers-set-variables vp)))
    (remove-if-not (lambda (x) (is-degree? f x degree)) vars)))

(defmethod select-best-degree-one-zero-constraint (zeroes)
  "Returns (x . zc) where x is a variable of degree 1 in zc and zc is in zeroes.
   zeroes is a list of zero-constraints."
  (let ((best-in-each-constraint (mapcar 'select-best-degree-one-in-constraint zeroes)))
    (reduce 'select-better-degree-one best-in-each-constraint :initial-value nil)))

(defmethod select-best-degree-one-in-constraint ((zc zero-constraint))
  (let* ((degree-one-vars (vars-of-degree-in-constraint zc 1))
	 (dov-with-zc (mapcar (lambda (x) (cons x zc) ) degree-one-vars)))
    (reduce 'select-better-degree-one dov-with-zc :initial-value nil)))
	 
(defun coeff-degree (v-zc)
  (assert (not (null v-zc)))
  (let* ((v (car v-zc))
	 (zc (cdr v-zc))
	 (f (constraint-poly zc))
	 (uv-f (univariate-rep f v))
	 (coeff (uv-coeff uv-f 1)))
    (prep:polyrepTotalDegreePoly coeff)))

(defun select-better-degree-one (v-zc-a v-zc-b)
  (if (null v-zc-a) v-zc-b
      (if (null  v-zc-b) v-zc-a
	  (let ((d-a (coeff-degree v-zc-a))
		(d-b (coeff-degree v-zc-b)))
	    (cond ((< d-a d-b)  v-zc-a)
		  ((> d-a d-b)  v-zc-b)
		  (t v-zc-a) ;; TODO further refine this to take into account the complexity of c
		  )))))


(defmethod mixed-projection-case-split ((n vts-node))
  ;; Performs a mixed projection and case-split for a variable appearing
  ;; in a zero-constraint with degree one.
  ;;   (exists x (and [b * x + c = 0] phi)
  ;; is logically equivalent to
  ;;   (or (and (b = 0) (c = 0) (exists x phi)) (and (b != 0) phi[-c*b^{-1} /x]))
  ;;
  ;; A variable equal to a constant is a special case of this.
  (assert (vts-leaf? n))
  (let* ((phi (vts-witness n))
	 (psis (cs-constraints phi))
	 (zeroes (remove-if-not 'zero-constraint? psis))
	 (var-power-one-zc (select-best-degree-one-zero-constraint zeroes)))
    (if (null var-power-one-zc)
	nil
	(let* ((x (car var-power-one-zc))
	       (zc (cdr var-power-one-zc))
	       (f (constraint-poly zc))
	       (upoly (univariate-rep f x))
	       (b (uv-coeff upoly 1))
	       (c (uv-coeff upoly 0))
	       (neg-c-over-b (make-plain-extended-polynomial :poly (make-surd-polynomial :a (prep:polyrepNegativePoly c) :d b)))
	       (b-is-zero-c (p-is-zero-c b))
	       (b-is-not-zero-c (p-is-non-zero-c b))
	       (c-is-zero-c (p-is-zero-c c))
	       ;;(sub (candidate-virtual-bounds-constraint zc x))
	       (psis-minus-zc (remove zc psis))
	       (branch1-consts (append (list b-is-zero-c c-is-zero-c) psis-minus-zc))
	       (branch2-consts (cons b-is-not-zero-c psis-minus-zc))
	       (branch1-cs (make-constraint-set :constraints branch1-consts))
	       (branch2-cs (make-constraint-set :constraints branch2-consts))
	       (branch2-subs (acons x neg-c-over-b  (vts-substitutions n)))
	       (child-b-is-zero (clone-node n :witness branch1-cs))
	       (child-b-is-not-zero (clone-node n
						:witness branch2-cs
						:substitutions branch2-subs)))
	  (progn (set-node-status n :status-mixed-projection)
		 (add-child n child-b-is-zero)
		 (add-child n child-b-is-not-zero)
		 (vformat 2 t "~% mixed-projection-case-split x:~a, f:~a~%" x f)
		 t)))))
	       


(defmethod project-variable ((n vts-node))
  (assert (vts-leaf? n))
  (let* ((phi (vts-witness n))
	 (vp-map (constraint-set-to-powers-map phi))
	 (vp-max (var-powers-max vp-map))
	 (candidates (filter-candidates vp-max)))
    (if (null candidates)
	nil
	(let* ((x (select-candidate candidates))
	       (subs (candidate-virtual-bounds phi x)))
	  (progn (add-substitutions n x subs) t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;          Consistency         ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; visit-node
;; ----------
;; Visits a leaf node in the search for a satisfiable witness.
;; Returns a literal-status.
;; In all cases, the node's status changes.
;; If lit-unsat is returned, the witness is known to be unsatisfiable.
;; If lit-sat is returned, the witness is known to be satisfiable.
;; If lit-unknown is returned, and progress may be possible on the node's children.
;; If progress cannot be made, the node's status is blocked and will have no children.

(defmethod visit-node ((n vts-node) (resource-limit integer))
  (cond ((is-trivially-sat n) :lit-sat)
	((is-trivially-unsat n) :lit-unsat)
	((simplify-node n) :lit-unknown)
	((apply-substitutions n) :lit-unknown)
	((mixed-projection-case-split n) :lit-unknown)
	((and (< 0 resource-limit) (project-variable n)) :lit-unknown)
	(t (progn
	     ;; We cannot make progress on the current node.
	     ;; Mark this node as blocked!
	     (assert (null (vts-children n)))
	     (set-node-status n :status-blocked)
	     :lit-unknown))))

(defvar *vts-interactive* nil
  "If set the vts algorithm waits for user input before preceeding.")

(defvar *sat-iter* 0)

;; DFS search
(defun find-sat-node (nodes blocked resource-limit)
  (cond
    ((null nodes) blocked)
    (t (let ((head (car nodes)) (rest (cdr nodes)))
	 (progn (setf *sat-iter* (+ 1 *sat-iter*))
		(vformat (if (= (mod *sat-iter* 10000) 0) 0 6)
			 t "find-sat-node ~a~%" head))
	 (let ((res (visit-node head resource-limit)))
	   (vformat 6 t "find-sat-node post: ~a~%" head)
	   (if *vts-interactive* (read nil)) ;; wait for user input
	   (case res
	     (:lit-sat (cons head blocked))
	     (:lit-unsat (find-sat-node rest blocked resource-limit))
	     (:lit-unknown
	      (let ((next-nodes (append (vts-children head) rest))
		    (next-blocked (if (vts-blocked? head) (cons head blocked) blocked))
		    (next-rl (if (vts-projection? head) (- resource-limit 1) resource-limit)))
		(find-sat-node next-nodes next-blocked next-rl)))))))))

(defmethod is-consistent ((cs constraint-set) (resource-limit integer))
  (let* ((root (make-vts-node :witness cs :id (get-unique-id)))
	 (candidate-sat-nodes (find-sat-node (list root) nil resource-limit))
	 (status (if (null candidate-sat-nodes)
		     :lit-unsat
		     (if (null (find-if 'vts-sat? candidate-sat-nodes))
			 :lit-unknown
			 :lit-sat))))
    (vformat 5 t "resulting tree~% ~a" root)
    (values status root candidate-sat-nodes)))

(defvar *default-resource-limit* 100)


;; Summarizes a node by the number of types of each step
(defmethod summarize-node ((n vts-node))
  (let ((nodes (list-nodes n)))
    (acons :status-leaf (count-if 'vts-leaf? nodes)
    (acons :status-projection (count-if 'vts-projection? nodes)
    (acons :status-mixed-projection (count-if 'vts-mixed-projection? nodes)
    (acons :status-substitution (count-if 'vts-substitution? nodes)
    (acons :status-simplification (count-if 'vts-simplification? nodes)
    (acons :status-unsat (count-if 'vts-unsat? nodes)
    (acons :status-sat (count-if 'vts-sat?  nodes)
    (acons :status-blocked (count-if 'vts-blocked? nodes)
	   nil))))))))))


(defmethod list-nodes ((n vts-node))
  (cons n (reduce 'append (mapcar 'list-nodes (vts-children n)))))

(defmethod vts-leaf-or-blocked? ((n vts-node))
  (or (vts-leaf? n) (vts-blocked? n)))

(defmethod leaves-blocked-nodes ((n vts-node))
  "Returns all of the blocked sub-nodes of a tree."
  (remove-if-not 'vts-leaf-or-blocked? (list-nodes n)))

(defmethod split-non-zero-constraint ((psi non-zero-constraint))
  (let* ((f (constraint-poly psi))
	 (f-is-pos (p-is-positive-c f))
	 (f-is-neg (p-is-positive-c (prep:polyrepNegativePoly f))))
    (or-dnfs (dnf-from-constraint f-is-pos)
	     (dnf-from-constraint f-is-neg))))

(defmethod split-non-zeroes ((n vts-node))
  "If the witness of n contains any constraints f !=0,
   this is split into two subnodes using the split f<0 or f>0."
  (let* ((psis (cs-constraints (vts-witness n)))
	 (nnc (find-if 'non-zero-constraint? psis)))
    (if (not (null nnc))
	(let ((cases (split-non-zero-constraint nnc)))
	  (case-split-node n nnc cases :status-simplification)
	  (mapcar 'split-non-zeroes (vts-children n))))))

(defmethod node-get-zeroes-positives-non-negatives-constraints ((n vts-node))
  (let* ((psis (cs-constraints (vts-witness n)))
	 (zeroes (remove-if-not 'zero-constraint? psis))
	 (positives (remove-if-not 'positive-constraint? psis))
	 (non-negatives (remove-if-not 'non-negative-constraint? psis)))
    (values zeroes positives non-negatives)))


(defmethod node-get-zeroes-positives-non-negatives-polys ((n vts-node))
  (multiple-value-bind (zs ps nns)
      (node-get-zeroes-positives-non-negatives-constraints n)
    (values (mapcar 'constraint-poly zs)
	    (mapcar 'constraint-poly ps) 
	    (mapcar 'constraint-poly nns))))