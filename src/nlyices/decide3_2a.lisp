;; /* Copyright (c) SRI International 2003, 2004. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; decide3.2.lisp --
;; Author          : Ashish Tiwari
;; Created On      : ?? 
;; Last Modified By: Ashish Tiwari
;; Last Modified On: Wed Apr 26, 2006
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;; CSL 2005 paper implementation
;;
;; Version 3_2a (2009): Bruno Dutertre
;; -----------------------------------
;; Updates for integration with Yices 2
;; new heuristics
;; performance improvements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :gb)		;; This module is named GB: GB based dp for reals

(defpackage :gb
  (:nicknames :GB)
  (:export :sos :sos-cheap
	   :set-debug-level :set-newU :set-newV :get-newU :get-newV
	   :set-degree-ratio :set-length-ratio :reset-bounds
	   :set-iteration-bound :poly-in-POL-list? :show-basis :show-witness)
  (:use :polynomial-representation-core :cl-user :common-lisp))

(in-package :gb)	;; The core procedure




;;----------------------------------------------
;; Positive/non-negative variables
;;
;; *newU* = list of positive variables
;; *newV* = list of non-negative variables
;;----------------------------------------------

(defvar *newU* nil)	;; new vars U > 0
(defvar *newV* nil)	;; new vars V >= 0

(defun set-newU (u) (setf *newU* u))
(defun set-newV (u) (setf *newV* u))
(defun get-newU () *newU*)
(defun get-newV () *newV*)


;;-------------------
;; Debugging level
;;------------------

(defvar *dlevel* nil)		;; Default nil: nothing gets printed

;; Change the level (external)
(defun set-debug-level (n)
  (setf *dlevel* n))

;;
;; Print at level <= debug level
;; 
;; BD: This used to be if level > debug level. Changed this to get a more
;; natural behavior: higher debug level gives more information.
;; 
(defmacro print-debug (level &rest others)
  `(if (and *dlevel* (<= ,level *dlevel*))
       (funcall #'format ,@others)))



;;------------------------------------------------------------------------------
;; Parameters to reduce the chance of blowup during the GB computation
;; - degreebound
;; - lengthbound
;; when a new S-polynomial is created, it's dropped if it is too
;; large (i.e., it has total degree > *degreebound* or length > lengthbound).
;;
;; *degreebound* and *lengthbound* are set based on the total degree
;; and max length of the input polynomials:
;; degreebound = degreeratio * max degree of input polynomials
;; lengthbound = lengthratio * max length of input polynomials
;;-----------------------------------------------------------------------------

(defvar *degreeratio* 2)	;; generated-poly-degree/input-poly-degree
(defvar *lengthratio* 5)	;; generated-poly-length/input-poly-length

(defvar *degreebound* 1)	;; degree of polynomial
(defvar *lengthbound* 1)	;; number of monomials in any polynomial

;; external functions to set parameters
(defun set-degree-ratio (n) (setf *degreeratio* n))
(defun set-length-ratio (n) (setf *lengthratio* n))

;; reset bounds to their default
(defun reset-bounds ()
  (setf *degreebound* 1)
  (setf *lengthbound* 1))


;; check whether p's degree and length are below the bounds
(defun bound-check? (p)
  (declare (special *degreebound* *lengthbound*))
  (and (<= (length p) *lengthbound*)
       (<= (prep:polyrepTotalDegreePoly p) *degreebound*)))



;;--------------------------------------------------------------
;; Bound on the number of phase-2 iterations
;; - each iteration adds a new variable + equation to a basis
;;--------------------------------------------------------------

(defvar *iteration-bound* 13)

(defun set-iteration-bound (n) (setf *iteration-bound* n))


;;--------------------------------------------------------------
;; Resource limit for grobner basis construction
;;--------------------------------------------------------------

(defvar *gb-resource-limit* 30)

(defun set-gb-resource-limit (n) (setf *gb-resource-limit* n))

;;--------------------------------------------------------------
;; Result of a grobner basis computation.
;; These computations can fail by running out of resources.
;;
;; Let g be an example:
;;   (gbcomp-success g) is either nil or t
;; Basis is determined by (gbcmp-success g)
;;   if (gpcomp-success g) then (gpcomp-basis g) is a grobner basis
;;   if (null (gpcomp-success g)) then (gpcomp-basis g) is nil
;;--------------------------------------------------------------

(defstruct gbcomp success basis)
(defun make-success (b) (make-gbcomp :success t :basis b))
(defun make-failure () (make-gbcomp :success nil :basis nil))

;;-----------------------------------------------------------------------------
;; Polynomial and witness and side condition c
;; 
;; Each element of the Grobner basis is stored as a structure (POL p wit c)
;; - p = the polynomial (i.e., p == 0) is a constraint
;; - wit = list of polynomials (q_1 ... q_t) from which (p == 0) was derived
;;   (i.e., p == 0 is implied by q_1 == 0 and ... and q_t == 0
;; - c can be '= or '>= or '> or nil 
;; - c is also used to avoid creating redundant S-polynomials during
;;   Grobner basis computation
;;-----------------------------------------------------------------------------

(defstruct POL pol wit c)

;; constructor
(defun make-pol (p wit c) (make-POL :pol p :wit wit :c c))

;; Convert p to a structure (POL p nil symbol) where symbol
;; indicates where p comes from 
;; E --> equalities '=
;; R --> strict positive '>
;; S --> nonnegative '>=
(defun make-pole (p) (make-pol p nil '=))
(defun make-polr (p) (make-pol p nil '> ))
(defun make-pols (p) (make-pol p nil '>=))

;; Union of witnesses
;; q is a POL structure
;; p is either a polynomial or a POL structure
(defun combine-wit (p q)
  (union (if (POL-p p) (POL-wit p) p) (POL-wit q)))

(defun get-witness (E)
  (print-debug 8 t "Get-witness function called with WITNESS~%~A~%" E)
  E)



;; Check whether p is in list of POL objects E 
;; this is a helper function to query unsat witnesses
(defun poly-in-POL-list? (p E)
  (cond ((null E) nil)
	((eq p (POL-pol (car E))) t)
	(t (poly-in-POL-list? p (cdr E)))))
;;  (member p E :test #'(lambda (x y) (eq (POL-pol x) y))))



;;--------------------------------------------------------------------
;; Pretty printing
;; - E must be a list
;; - :key must be a function that maps elements of E to polynomials
;; return a list of strings: each string is the pretty printing of
;; a polynomial in E (obtained by applying key)
;;--------------------------------------------------------------------

(defun pp (E &key (key #'(lambda(x) x)))
  (if (null E) nil
    (nreverse (pp* (car E) (cdr E) key nil))))

(defun pp* (first rest key l)
  (let ((pretty (prep:polyrepPrint (funcall key first))))
    (if (null rest) (cons pretty l)
      (pp* (car rest) (cdr rest) key (cons (format nil "~a, " pretty) l)))))


;; More readable: print one poly per line
(defun show-basis (E)
  (format t "~%basis:~%")
  (dolist (p E) (format t "   ~a~%" (prep:polyrepPrint (POL-pol p))))
  (format t "~%positive vars: ~a" *newU*)
  (format t "~%nonneg. vars:  ~a~%~%" *newV*))


;; Witness is a list of input polynomials
(defun show-witness (E)
  (format t "~%witness:~%")
  (dolist (p E) (format t "   ~a ~a 0~%" (prep:polyrepPrint (POL-pol p)) (POL-c p)))
  (format t "~%~%"))


;;------------------------------------------------
;; Tests and auxiliary operations on polynomials
;;------------------------------------------------

;; Make the main coefficient of p equal to 1
(defun normalize-poly (p)
  (if (null p) p           ;; p is zero
      (let ((c0 (caar p))) ;; c0 = main coefficient of p
	(cond ((eq c0 1) p)
	      ((eq c0 -1) (prep:polyrepNegativePoly p))
	      (t (prep:polyrepDividePolyCst p c0))))))


;; Check whether p is positive or negative
;; - return t if all monomials of p are >= 0 or all monomials of p are <= 0
;; - return nil otherwise
(defun pn-poly? (p)
  (cond ((every #'(lambda(x) (> (car x) 0)) p) (p-poly?* p))
	((every #'(lambda(x) (< (car x) 0)) p) (p-poly?* p))
	(t nil)))

;; check whether all mu's in p are p >= 0
(defun p-poly?* (p)
  (cond ((null p) t)
	((p-mono? (cdar p)) (p-poly?* (cdr p)))
	(t nil)))

;; check whether mu >= 0
;; i.e., all variables of mu are in *newU* or *newV* or have even degree
(defun p-mono? (mu)
  (cond ((null mu) t)
        ((var-in-list? (caar mu) *newU*) (p-mono? (cdr mu)))
        ((var-in-list? (caar mu) *newV*) (p-mono? (cdr mu)))
	((evenp (cdar mu)) (p-mono? (cdr mu)))
        (t nil)))

(defun var-in-list? (var vlist)
  (member var vlist :test #'prep:var-equal?))



;; Check whether p is non-zero and constant
;; - p is a sorted list of monomials
;; - the ordering ensures that the constant monomial if any is the 
;;   last monomial of p.
;; So it's enough to check whether p has at least one monomial and 
;; the first monomial is of the form (k . nil) for k /= 0
(defun nz-poly? (p) (and p (null (cdar p)) (/= (caar p) 0)))
 

;; Check whether p is a monomial
;; - i.e., list of size 1
(defun mono? (p) (and p (null (cdr p))))


;; Check whether mu is a multiple of nu
;; - both mu and nu must be association lists of the form
;;    ((x_1.d_1) ... (x_k.d_k))
;;  (which represents the monomial x_1^d_1 ... x_k^d_k).
(defun multiple? (mu nu)
  (cond ((null nu) t)
	((null mu) nil)
	((prep:var-equal? (caar nu) (caar mu))
	 (if (<= (cdar nu) (cdar mu))
	     (multiple? (cdr mu) (cdr nu)) 
	     nil))
	(t (multiple? (cdr mu) nu))))


;; Check whether mu and nu are equal
(defun equal-terms? (mu nu)
  (cond ((eq mu nu) t)
	((null mu) nil)
	((null nu) nil)
	(t (and (prep:var-equal? (caar nu) (caar mu))
		(= (cdar nu) (cdar mu))
		(equal-terms? (cdr mu) (cdr nu))))))






;;---------------------------------------------------------------------------
;; Simplification rules
;; 
;; Rule 1:
;;     x_1^d_1 * ... * x_k^d_k == 0   ==>  x_{i_1} * ... * x_{i_t} == 0
;;   where x_{i_1} ... x_{i_t} are the variables in x_1 ... x_k that do
;;   not belong to *newU* (i.e., we remove the positive variables)
;;
;; Rule 2:
;;     (a_1 t_1 + ... + a_n t_n == 0)  ==>  t_1 == 0, ..., t_n == 0
;;   if a_1 ... a_n are all positive or all negative
;;   and t_1 >= 0 ... t_n >= 0 (and n >= 2)
;;
;; Rule 3:
;;   remove trivial equations 0 == 0
;;---------------------------------------------------------------------------


;; Given p = (POL q witness c), check whether q is a monomial
;; i.e.. check whether rule 1 applies
(defun monomial? (p) (mono? (POL-pol p)))

;; check whether rule 2 is applicable:
;; - i.e., all monomials of p are >= 0 or <= 0
(defun pn-polynomial? (p) (pn-poly? (POL-pol p)))

;; check whether p is zero
(defun zero-polynomial? (p) (null (POL-pol p)))


;; Force the main coeff of p to 1
(defun force-monic (p)
;;  (setf (POL-pol p) (normalize-poly (POL-pol p)))
;;  p
;; for testing: no side effects
  (make-pol (normalize-poly (POL-pol p)) (POL-wit p) (POL-c p))
)



;;
;; Simplification rule 1
;; ---------------------
;; If x_1, ..., x_n are variables then x_1^d_1 * ... * x_n^d_n = 0 
;; is equivalent to x_1 * ... * x_n = 0
;;
;; if any x_i is known to be positive, then it can be removed
;; from the product.
;;
;; mu = the product (x_1^d_1 * ... * x_n ^ d_n)
;; represented as an association list ((x_1 . d_1) ... (x_n . d_n))
;; returned value = a new monomial mu' = (x_1 ^ 1 * ... * x_n ^ 1)
;; 
(defun simplify-zero-prod (mu) 
  (cond ((null mu) mu)
	((var-in-list? (caar mu) *newU*) (simplify-zero-prod (cdr mu)))
	(t (cons (cons (caar mu) 1) (simplify-zero-prod (cdr mu))))))


;; Build the polynomial 1 * (simplify-zero-prod mu)
;; where m = a * mu (a must be non zero)
(defun simplify-zero-mono (m)
  (list (cons 1 (simplify-zero-prod (cdr m)))))


;; Given p = (POL q witness c), where q is a monomial,
;; construct (POL (simplify q) witness nil)
(defun simplify-monomial (p)
  (make-pol (simplify-zero-mono (car (POL-pol p))) (POL-wit p) nil))


;;
;; Simplification rule 2
;  ---------------------
;; Given p = a_1 t_1 + ... + a_n t_n,
;; if t_1 >= 0, ..., t_n >= 0
;; and all coefficients a_1, ..., a_n have the same sign,
;; then  (p = 0) is equivalent to (t_1=0 and  ...  and t_n=0)
;;
;; This function constructs the list (t'_1 .... t'_n) from p
;; where t'_i = simplified monomial equivalent to t_i
;; each t'_i has the same witness as p
;;
(defun simplify-pn-polynomial (p) 
  (let ((witness (POL-wit p)))
    (loop for i in (POL-pol p) collect (make-pol (simplify-zero-mono i) witness nil))))



;;
;; Simplification of a list of monomials
;; -------------------------------------
;: Given a list l of monomials (POL ...), 
;; remove element m1 from l if m1 is a multiple of 
;; another element m2 of l.
;; (i.e., m2 = 0 implies m1 = 0 so m1 = 0 is redundant)
;;

;; extension of multiple? to POL structures
;; - m1 and m2 must be monomials
(defun multiple-monomial? (m1 m2)
  (multiple? (cdar (POL-pol m1)) (cdar (POL-pol m2))))

;; check whether m is a multiple of an element of l
;; - m must be a monomial
;; - l must be a list of monomial
(defun subsumed-monomial? (m l)
  (some #'(lambda (x) (multiple-monomial? m x)) l))

;; remove all multiples of m from list l1 
;; l2 accumulates the result
(defun remove-multiples (m l1 l2) 
  (if (null l1) l2
    (remove-multiples m (cdr l1) (if (multiple-monomial? (car l1) m) l2 (cons (car l1) l2)))))

;; add monomial m to the list of monomials l
;; - remove multiples of m from l
;; - add m to l unless it's a multiple of an m1 in l
(defun insert-monomial (m l) 
  (if (subsumed-monomial? m l) l
    (cons m (remove-multiples m l nil))))

;; Simplify list l
(defun reduce-monomial-list (l) 
  (reduce-monomial-list* l nil))

(defun reduce-monomial-list* (l1 l2)
  (if (null l1) l2
    (reduce-monomial-list* (cdr l1) (insert-monomial (car l1) l2))))




;;
;; Support for reduction
;; ---------------------
;; apply the rewrite rule s0 --> -cisi to polynomial poly
;; - s0 must be a term ((x_0.d_0) .... (x_k.d_k))
;; - cisi must be a polynomial smaller than s0 in the term ordering
;;
;; return 0 if poly is unchanged by the substitution 
;; (i.e., s0 does not divide any monomial of poly)
;; return the substitution result otherwise
;;
;; NOTES:
;; - there's no ambiguity since the zero polynomial is represented
;;   by nil (empty list of monomials), not by 0.
;; - the rewrite is applied once (in parallel) to all monomials of poly
;;

;; BD: made the function tail-recursive
(defun rewrite-poly (s0 cisi poly)
  (let ((aux (rewrite-poly* s0 cisi poly nil nil nil)))
    aux))


;; r = unchanged monomials of poly in reverse order
;; sum = partial sum (for the monomials of poly that changed)
;; change = t if one monomial has changed
(defun rewrite-poly* (s0 cisi poly sum r changed)
  (if (null poly)
      (if changed (prep:polyrepAddPoly sum (nreverse r)) 0)
    (let ((poly1 (rewrite-mono s0 cisi (car poly))))
      (if (eq poly1 0) 
	  (rewrite-poly* s0 cisi (cdr poly) sum (cons (car poly) r) changed)
	(rewrite-poly* s0 cisi (cdr poly) (prep:polyrepAddPoly sum poly1) r t)))))


;; older version
(defun rewrite-poly-old (s0 cisi poly)
  (if (null poly) 0
      (let ((poly1 (rewrite-mono s0 cisi (car poly)))
	    (poly2 (rewrite-poly-old s0 cisi (cdr poly))))
        (cond ((and (eq poly1 0) (eq poly2 0))
	       0)
	      ((eq poly1 0)
	       (cons (car poly) poly2))	;; monomial ordering allows this
	      ((eq poly2 0)
	       (prep:polyrepAddPoly poly1 (cdr poly)))
	      (t
	       (prep:polyrepAddPoly poly1 poly2))))))


;; apply the rewrite rule s0 --> -cisi to monomial mono
;; mono is (d0 . t0) where t0 = x_1^d_1 ... x_k^dk 
;;                    and d0 is a constant
;; return 0 if s0 does not divide t0
;; return -d0 * u0 * cisi if t0 = u0 * s0
(defun rewrite-mono (s0 cisi mono)
  (let* ((d0 (car mono))
	 (t0 (cdr mono))
	 (t0bys0 (prep:polyrepDividePP t0 s0)))
    ;; if s0 divides t0 then t0bys0 is u0 such that t0 = u0 * s0
    ;; otherwise t0bys0 is 0
    (if (eq t0bys0 0)
	0
	(prep:polyrepMultiplyMonoPoly (cons (- d0) t0bys0) cisi))))



;;
;; Recursively apply the rewrite rule s0 --> -cisi to poly
;; return 0 if the rule does not apply
;;
;; The rewrite rule is applied until nothing changes
;;
(defun fully-rewrite-poly (s0 cisi poly)
  (let ((newp (rewrite-poly s0 cisi poly)))
    (if (eq newp 0) 0 (fully-rewrite-poly* s0 cisi newp))))

(defun fully-rewrite-poly* (s0 cisi p)
  (let ((newp (rewrite-poly s0 cisi p)))
    (if (eq newp 0) p (fully-rewrite-poly* s0 cisi newp))))




;; Reduction of p by q
;; -------------------
;; - both p and q are (POL xx yy zz)
;; - q is used as a rewrite rule
;; - q's lead coefficient must be 1
;; 
;; - return p if there's no change
;; - return p' = (POL newp wit zz) otherwise
;;   where newp = reduction of (POL-pol p) by (POL-pol q),
;;                scaled to a monic polynomial
;;         wit = updated to record that p' was derived from p and q
;;         zz = copied from p
;;
;; reduce-poly applies one parallel rewrite to all monomials of p
;; reduce-poly-full applies the rewrite rule recursively until
;; nothing changes
;;
(defun reduce-poly-by-poly (p q)
  (let* ((c0s0 (car (POL-pol q)))   ;; main monomial of q, c0 must be 1
	 (cisi (cdr (POL-pol q)))   ;; rest of q
	 (s0 (cdr c0s0)))           ;; main term s0 of q
    (assert (= (car c0s0) 1)) 
    (let ((newp (rewrite-poly s0 cisi (POL-pol p))))
      (if (eq newp 0) p
	(make-pol (normalize-poly newp) (combine-wit p q) (POL-c p))))))

(defun fully-reduce-poly-by-poly (p q)
  (let* ((c0s0 (car (POL-pol q)))   ;; main monomial of q, c0 must be 1
	 (cisi (cdr (POL-pol q)))   ;; rest of q
	 (s0 (cdr c0s0)))           ;; main term s0 of q
    (assert (= (car c0s0) 1)) 
    (let ((newp (fully-rewrite-poly s0 cisi (POL-pol p))))
      (if (eq newp 0) p
	(make-pol (normalize-poly newp) (combine-wit p q) (POL-c p))))))



;;
;; Reduction of p modulo a set of equations
;; ----------------------------------------
;; - p is a decorated polynomial (POL xx yy zz)
;; - E is a list of polynomials (representing equations)
;;   the main coefficient of each polynomial in E must be 1
;; - each element of E is used as a rewrite rule
;; 
;; return p' = p reduced by E
;; - the witness field of p' records all the rewrite rules
;;   used to produce p'
;;
;; NOTE: if p can't be reduced then the function returns p itself
;; so we can test whether p was reduced using (eq p p').
;; 
;; BD: changed the code so that cheaper rewrite rules have priority:
;; - this assumes that E is sorted so that the cheapest rules occur first
;; - a reasonable heuristic is to sort by length
;;
(defun fully-reduce-poly (E p)
  (fully-reduce-poly* E p E))

;; E = full list of equations
;; A = subset of E (as a sublist) = rules that remain to try
(defun fully-reduce-poly* (A p E)
  (if (null A) p
    (let ((p2 (reduce-poly-by-poly p (car A))))
      (if (eq p2 p)
	  (fully-reduce-poly* (cdr A) p E)
	(fully-reduce-poly* E p2 E)))))






;;
;; Reduction of a basis by a polynomial
;; ------------------------------------
;; G: a list of (POL ...) 
;; q: a (POL ...)
;; fully reduce all elements of G modulo q
;;
;; - q must be monic (leading coefficient = 1)
;;
;; return a pair (E G') such that
;; - G' = list of reduced elements of G whose leading monomial did not change,
;; - E  = list of reduced elements of G whose leading monomial did change
;;
(defun fully-reduce-basis (G q)
  (let* ((c0s0 (car (POL-pol q)))   ;; main monomial of q, c0 must be 1
	 (cisi (cdr (POL-pol q)))   ;; rest of q
	 (s0 (cdr c0s0)))           ;; main term s0 of q
    (assert (= (car c0s0) 1)) 
    (fully-reduce-basis* s0 cisi G nil nil (POL-wit q))))

;; check wether the lead terms of p and q are equal
(defun same-lead-term? (p q)
  (equal-terms? (cdar p) (cdar q)))

;; apply rewrite s0 --> -cisi to all elements of G
;; accumulate the result into the pair E G1
(defun fully-reduce-basis* (s0 cisi G E G1 wit)
  (if (null G) (values E G1)
    (let* ((p (car G))
	   (newp (fully-rewrite-poly s0 cisi (POL-pol p))))
      (cond 
       ((eq newp 0)
	;; no change to p: keep it in G1
	(fully-reduce-basis* s0 cisi (cdr G) E (cons p G1) wit))
       ((null newp)
	;; p is reduced to 0: skip it
	(fully-reduce-basis* s0 cisi (cdr G) E G1 wit))
       ((and (same-lead-term? newp (POL-pol p))
	     (<= (length newp) *lengthbound*) ;; --> try to avoid blowing up??
	     (not (mono? newp))
	     (not (pn-poly? newp)))
	;; p is reduced to newp but the leading term is unchanged
	;; and newp is not reducible by rule 1 or rule 2
        ;; add newp to G1, with POL-c copied from p
	(fully-reduce-basis* s0 cisi (cdr G) E (cons (make-pol newp (combine-wit wit p) (POL-c p)) G1) wit))
       (t 
	;; p is reduced to newp with a smaller leading term
	;; or newp can be reduced by rule 1 or rule 2
	;; add newp to E
	(fully-reduce-basis* s0 cisi (cdr G) (insert-poly (make-pol newp (combine-wit wit p) nil) E) G1 wit))))))


;; apply simplification rules to q if possible
;; then add the result to E
(defun insert-poly (q E)
  (cond ((monomial? q) (cons (simplify-monomial q) E))
	((pn-polynomial? q) (nconc (simplify-pn-polynomial q) E))
	((bound-check? (POL-pol q)) (cons (force-monic q) E))
	(t  E)))


;;
;; Superposition: Critical pair computation
;; ----------------------------------------
;; Compute the s-polynomial of p and q and filter out large/high degree polynomials
;;
;; - return nil if (p, q) is not a critical pair
;; - return nil is s-polynomial is too large (too many monomials) or has high degree.
;;   this filtering relies on the global variables *degreebound* and *lengthbound*
;; - return the s-polynomial otherwise with witness = (union of p and q's witnesses)
;;
(defun superposition (p q)
  (let* ((cp1 (superposition* p q))
	 (cp (if (bound-check? cp1) cp1 nil)))
    (if (null cp) nil 
        (make-POL :pol cp :wit (combine-wit p q) :c nil))))


;;
;; Critical pair:
;; - compute the S-polynomial for p1 and q1
;;   p1 is (POL p ...)
;;   q1 is (POL q ...)
;; - p is c0.s0 + ... (where c0.s0 is the leading monomial)
;; - q is d0.t0 + ... (where d0.t0 is the leading monomial)
;; p and q form a critical pair if the lcm of s0 and t0 is not s0.t0
;; then we have lcm(s0, t0) = s0.v0 = t0.w0
;;
;; The S-polynomial is (d0 * v0 * p - c0 * w0 * q) whose leading 
;; monomial is smaller than lcm(s0, t0) in the term ordering.
;;
;; Side effect: add w0 to q1's POL-c component
;;
;; Returned value: 
;; - nil if p1 q1 is not a critical pair or if it's redundant
;; - the S-polynomial for p1 and q1 otherwise
;;
(defun superposition* (p1 q1)
  (let* ((p (POL-pol p1))
	 (q (POL-pol q1))
	 (c0s0 (car p))
	 (d0t0 (car q))
	 (c0 (car c0s0))
	 (d0 (car d0t0))
	 (d0p c0)
	 (c0p (- 0 d0)))
    (multiple-value-bind (overlap? s0p t0p)
	(overlap-pp-pp (cdr c0s0) (cdr d0t0) (POL-c p1) (POL-c q1))
	(if overlap?
	    (let ((ans (prep:polyrepAddPoly 
			(prep:polyrepMultiplyMonoPoly (cons c0p s0p) (cdr p)) 
			(prep:polyrepMultiplyMonoPoly (cons d0p t0p) (cdr q)))))
	      (setf (POL-c q1) (cons t0p (POL-c q1)))
	      (normalize-poly ans))   ;;   BD: don't know whether that helps?
	    nil))))

;;
;; Check whether mu and nu overlap and compute mu0 nu0 such that
;;  (lcm mu nu) = mu0 * mu = nu0 * nu
;; 
;; - mu = leading term of a polynomial p
;; - nu = leading term of a polynomial q
;; - nolismu = list of mu0's used in previous critical pairs for p
;; - nolusnu = list of nu0's used in previous critical pairs for q
;; 
;; The new critical pair is redundant if there's already an
;; S-polynomial whose main term divides (lcm mu nu). This is
;; filtered out by checking whether a multiple of mu0 is in nolistmu
;; or a multiple of nu0 is in nolistnu.
;;
(defun overlap-pp-pp (mu nu nolistmu nolistnu)
  (let ((order (prep:get-order)))
    (multiple-value-bind (st mu0 nu0) (overlap-pp-pp* mu nu order nil nil nil)
      (if (or (member mu0 nolistmu :test #'multiple?)
	      (member nu0 nolistnu :test #'multiple?))
	  (progn (print-debug 9 t "OPTIMIZATION-1 works~%") nil)
	  (values st mu0 nu0)))))


;;
;; Check whether mu and nu overlap
;; - mu = term (x_1^d_1 ... x_n^d_n)
;; - nu = term (x_1^e_1 ... x_n^e_n)
;; both are represented as association lists, 
;; e.g., mu is ((x_i . d_i) ... (x_n . d_n))
;; - order = list of variables in order!
;;
;; return (t mu1 nu1) if mu and nu overlap, 
;; with mu1 and nu1 such that lcm(mu, nu) = mu1 * mu = nu1 * nu
;;
;; return (nil ? ?) if mu and nu do not overlap
;;
;; optimization: return nil if the common variables of mu and nu are 
;; all parameters (i.e., variables in *newU* or *newV*)
;;
(defun overlap-pp-pp* (mu nu order yes mu1 nu1)
  (when (and (null yes) (member (car order) (append *newU* *newV*) :test #'prep:var-equal?))
      (print-debug 9 t "OPTIMIZATION-2 works~%")
      (return-from overlap-pp-pp* nil))
  (if (null order) 
      (values yes (nreverse mu1) (nreverse nu1))
      (let* ((x1 (car order))
	     (x1c1 (assoc x1 mu :test #'prep:var-equal?))
	     (x1c2 (assoc x1 nu :test #'prep:var-equal?))
	     (c1 (cdr x1c1))
	     (c2 (cdr x1c2)))
	(cond ((and (null c1) (null c2))
	       (overlap-pp-pp* mu nu (cdr order) yes mu1 nu1))
	      ((null c1)
	       (overlap-pp-pp* mu nu (cdr order) yes (cons x1c2 mu1) nu1))
	      ((null c2)
	       (overlap-pp-pp* mu nu (cdr order) yes mu1 (cons x1c1 nu1)))
	      ((= c1 c2)
	       (overlap-pp-pp* mu nu (cdr order) t mu1 nu1))
	      ((> c1 c2)
	       (overlap-pp-pp* mu nu (cdr order) t mu1
			(cons (cons (car x1c2) (- c1 c2)) nu1)))
	      ((< c1 c2)
	       (overlap-pp-pp* mu nu (cdr order) t 
			(cons (cons (car x1c2) (- c2 c1)) mu1) nu1))))))


;; Compute the list of S-polynomials (p q) for q in qlist
(defun all-pairs-spolys (p qlist)
  (all-pairs-spolys* p qlist nil))

(defun all-pairs-spolys* (p qlist slist)
  (if (null qlist) slist
    (let ((sp (superposition p (car qlist))))
      (all-pairs-spolys* p (cdr qlist) (if sp (cons sp slist) slist)))))




;;
;; Grobner-basis construction
;; --------------------------
;;
;; Apply the simplification rules and compute a fully-reduced 
;; Grobner basis of (union E G)
;; - E = list of polynomials as (POL ..) structures
;; - G = list of polynomials as (POL ..) structures
;;
;; Requirements on G:
;; - all polynomials of G must be monic (lead coefficient = 1)
;; - if p is in G, then p is fully reduced modulo (G - {p})
;; - Grobner-basis property: if p1 and p2 are in G then (S-poly p1 p2) 
;;   is reducible to 0 by G.
;;
;; The returned value is a gbcomp structure.
;; If the computation does not run out of resources, gbcomp is successful
;; and the basis G' is a list of polynomials G' that has the same
;; properties as G. 
;; If the computation runs out of resources, gbcomp is not successful.
;;
;; NOTES: 
;; - In general, G' is not a Grobner basis of the ideal generated by 
;; (union E G), because the simplification rules may make the 
;; ideal bigger. 
;; - Because of the bounds on degrees/size used in computing S-polynomials,
;; and other heuristics to avoid blowing up, the Grobner-basis property may
;; not hold either.
;;
;; However, the function ensures soundness: the set of real solutions
;; to the constraints (G' == 0, U>0, V>=0) is included in the set of
;; real solutions to (E == 0, G == 0, U>0, V>= 0).
;;
;; If the function detects an inconsistency, then it returns G' = (1).
;;
(defun grobner (E G)
  (print-debug 15 t "Order for constructing GB is ~a~%" (prep:get-order))
  (print-debug 15 t "Constructing GB for:~%~a~%~%" (pp (append E G) :key #'POL-pol))
  (grobner-core (grobner-preprocess E) nil G *gb-resource-limit*))



;; Preprocessing of a list of polynomials E
;; - apply the simplification rules
;; - make sure every polynomial is monic
(defun grobner-preprocess (E)
  (grobner-preprocess* E nil nil))

;; E = list of polynomials to process
;; M = monomials
;; F = non-monomials
(defun grobner-preprocess* (E F M)
  (if (null E) (nconc M F)
    (let ((p (car E)))
      (cond ((zero-polynomial? p)
	     (grobner-preprocess* (cdr E) F M))
	    ((monomial? p) 
	     (grobner-preprocess* (cdr E) F (cons (simplify-monomial p) M)))
	    ((pn-polynomial? p) 
	     (grobner-preprocess* (cdr E) F (nconc M (simplify-pn-polynomial p))))
	    (t
	     (grobner-preprocess* (cdr E) (cons (force-monic p) F) M))))))




;;
;; Sort polynomials by increasing length
;;
(defun sort-by-length (E) 
  (sort E #'shorter?))

(defun shorter? (p q) (< (length (POL-pol p)) (length (POL-pol q))))



;; Simplify p modulo a basis G
;; - first reduce p modulo G to p'
;; - then apply the simplification rules to p'
;; - p should be monic
;;
;; The result is a list of monic polynomials:
;; - the empty list if p' is zero
;; - a list of monomials (m_1 ... m_k) such that
;;   1) p' == 0 is equivalent to m_1 == 0 ... m_k == 0, 
;;   2) no m_j is divisible by m_i for i/=j
;; - the singleton list (p') if no simplification rule applies
;;
(defun grobner-simplify (p G)
  (if (and *dlevel* (> *dlevel* 6)(> (length (POL-pol p)) 20))
      (format t "grobner-simplify: degree=~a length=~a~%~a~%"
	      (prep:polyrepTotalDegreePoly (POL-pol p)) (length (POL-pol p))
	      (prep:polyrepPrint (POL-pol p))))
  (let ((p1 (fully-reduce-poly G p)))
    (cond ((zero-polynomial? p1) nil)
	  ((monomial? p1) (list (simplify-monomial p1)))
	  ((pn-polynomial? p1) (reduce-monomial-list (simplify-pn-polynomial p1)))
	  ((bound-check? (POL-pol p1)) (list p1))
	  (t nil)))) ;; drop p1 if it's too big (to avoid blowing up in Grobner basis computation)??


;; Add polynomial p to a basis G (p must be monic)
;; - reduce G by p to E, G1
;; - compute the new S-polynomials (spoly p q) for q in G1
;; return three lists:
;;   E = polynomials of G whose lead term was reduced by p 
;;   CP = new S polynomials
;;   G2 = new basis = { p } union G1
;;
(defun grobner-add-poly (G p)
  (multiple-value-bind 
   (E G1) (fully-reduce-basis G p)
   (let ((CP (all-pairs-spolys p G1)))
     (values E CP (sort-by-length (cons p G1))))))

;;
;; Grobner basis procedure
;; Input:
;; - E = list of polynomials
;; - G = basis under construction 
;; - M = list of polynomials
;;
;; - r = resource bound
;;
;; Invariants:
;; - all polynomials of E are monic, and non-reducible by the simplification rules
;; - all polynomials of G are monic, fully reduced in G
;; - if p1 and p2 are in G, then (S-poly p1 p2) is in E or it's known to be
;;   reducible/redundant
;; - all polynomials of M are monic and fully reduced modulo G
;; - if p is in M, then p can't be reduced by any polynomial in M - { p }
;;
;; - the resource bound is always positive r > 0
;;
;; Result:
;; - if computation is successful,
;;   the result is the basis G' = a basis of (union E M G)
;;   wrapped in a successful gbcomp
;; - otherwise, a gbcomp indicating failure is returned
(defun grobner-core (E M G r)
  (print-debug 3 t "grobner core E: ~a polys, M: ~a polys, G: ~a polys resources: ~a~%" (length E) (length M) (length G) r)
  (if (null M) 
      (if (null E)
	  (make-success G) ;;found a grobner basis
	  (let ((newE (sort-by-length E)))
	    (grobner-core (cdr newE) (grobner-simplify (car newE) G) G r)))
      ;; add the first element of M to the basis
      (let ((p (car M)))
	(print-debug 4 t "adding poly: ~a~%" (prep:polyrepPrint (POL-pol p)))
	(if (nz-poly? (POL-pol p)) ;; p is 1
	    (make-success (list p)) ;; the basis is reduced to p
	    (multiple-value-bind 
		  (E1 CP G1) (grobner-add-poly G p) ;; grobner-add-poly consumes 1 resource
	      (if (> r 0)
		  (grobner-core (nconc E E1 CP) (cdr M) G1 (- r 1))
		  (make-failure)))))))





;; ---------------------------------------------------------------------------
;; CSL paper procedure
;; Input: list of pols: E=0, R>0, S>=0 and Context Eold
;; - Eold is a Grobner basis (or nil) = list of (POL ...)
;; - E, R, S are lists of polynomials
;;
;; Output: (status, witness or newGB)
;; - status is t if no conflict found,
;;   newGB is the Grobner basis built from E, R, S, and Eold
;;   (can be used as Eold in a future call to sos)
;; - status is nil if a conflict is found
;;   witness is the unsatisfiability witness, represented as (POL p expl ..)
;; ---------------------------------------------------------------------------

(defun sos (E R &optional (S nil) (Eold nil))
  (declare (special *newU* *newV*))
  (init-params E R S)
  (print-debug 5 t "Input equations E:~%~a~%" (pp E))
  (print-debug 5 t "Input inequalities R: ~%~a~%" (pp R))
  (print-debug 5 t "Input inequalities S: ~%~a~%" (pp S))
  (let ((origU *newU*)
	(origV *newV*)
	(origX (prep:get-variables)) 
	(origC (prep:get-parameters))
	(E (mapcar #'make-pole E)) 
	(R (mapcar #'make-polr R))
	(S (mapcar #'make-pols S)))
    (let ((newE (add-extension-vars E R S))
	  (tempU *newU*) 
	  (tempV *newV*)
	  (tempX (prep:get-variables))
	  (tempC (prep:get-parameters)))
      (multiple-value-bind (st EE) (sos-full-tst newE Eold)
	;; if the gb computation fails (st = t) and ( EE = nil)
	(if st 
	    (progn (setf *newU* tempU) 
		   (setf *newV* tempV) 
		   (prep:set-variables tempX) 
		   (prep:set-parameters tempC)
;;;		   (print-debug 2 t "SATISFIABLE~%~a~%" (pp EE :key #'POL-pol))
		   (cond ((and *dlevel* (> *dlevel* 0))
			  (format  t "SAT~%")
			  (show-basis EE)))
		   (values t EE))
	    (progn (setf *newU* origU) 
		   (setf *newV* origV) 
		   (prep:set-variables origX) 
		   (prep:set-parameters origC)
;;		   (print-debug 2 t "UNSATISFIABLE WITNESS IS ~%~a~%" (pp (get-witness EE) :key #'POL-pol))
		   (cond ((and *dlevel* (> *dlevel* 0))
			  (format t "UNSAT~%")
			  (show-witness EE)))
		   (values nil EE)))))))


;;
;; Cheap variant: no phase 2
;;
(defun sos-cheap (E R &optional (S nil) (Eold nil))
  (declare (special *newU* *newV*))
  (init-params E R S)
  (print-debug 5 t "Input equations E:~%~a~%" (pp E))
  (print-debug 5 t "Input inequalities R: ~%~a~%" (pp R))
  (print-debug 5 t "Input inequalities S: ~%~a~%" (pp S))
  (let ((origU *newU*)
	(origV *newV*)
	(origX (prep:get-variables)) 
	(origC (prep:get-parameters))
	(E (mapcar #'make-pole E)) 
	(R (mapcar #'make-polr R))
	(S (mapcar #'make-pols S)))
    (let ((newE (add-extension-vars E R S))
	  (tempU *newU*) 
	  (tempV *newV*)
	  (tempX (prep:get-variables))
	  (tempC (prep:get-parameters)))
      (multiple-value-bind (st EE) (sos-tst newE Eold)
	;; if the gb computation fails (st = t) and ( EE = nil)
	(if st 
	    (progn (setf *newU* tempU) 
		   (setf *newV* tempV) 
		   (prep:set-variables tempX) 
		   (prep:set-parameters tempC)
;;;		   (print-debug 2 t "SATISFIABLE~%~a~%" (pp EE :key #'POL-pol))
		   (cond ((and *dlevel* (> *dlevel* 0))
			  (format  t "SAT~%")
			  (show-basis EE)))
		   (values t EE))
	    (progn (setf *newU* origU) 
		   (setf *newV* origV) 
		   (prep:set-variables origX) 
		   (prep:set-parameters origC)
;;		   (print-debug 2 t "UNSATISFIABLE WITNESS IS ~%~a~%" (pp (get-witness EE) :key #'POL-pol))
		   (cond ((and *dlevel* (> *dlevel* 0))
			  (format t "UNSAT~%")
			  (show-witness EE)))
		   (values nil EE)))))))


;; check whether G is unsat (reduced to a non-zero constant polynomial p)
(defun unsat-grobner (G)
  (and G (nz-poly? (POL-pol (car G)))))

;; simple version for testing the new grobner code
;; - compute the grobner G1 basis of (union E G)
;; - return the pair (t G1) if G1 does not reduce to (1)
;; - return the pair (values t nil) if the grobner basis computation fails
;; - return the pair (f witness) otherwise
(defun sos-tst (E G) 
  (print-debug 0 t "SOS-TEST~%")
  (print-debug 0 t "*degreebound*: ~a, *lengthbound*: ~a~%" *degreebound* *lengthbound*)
  (print-debug 5 t "polynomials in E:~%~a~%" (pp E :key #'POL-pol))
  (print-debug 5 t "polynomials in G:~%~a~%~%" (pp G :key #'POL-pol))
  (let* ((comp (grobner E G))
	 (G1 (gbcomp-basis comp)))
    (if (and (gbcomp-success comp) (unsat-grobner G1))
	(values nil (POL-wit (car G1)))
	(values t G1)))) ;; if this is a failure G1 = nil


;; Set the limits *degreebound* and *lengthbound*
;; - based on the input polynomials in E, R, S
(defun maxdegree (E)
  (if (null E) 0 (apply #'max (mapcar #'prep:polyrepTotalDegreePoly E))))

(defun maxlength (E)
  (if (null E) 0 (apply #'max (mapcar #'length E))))

(defun init-params (E R S)
  (declare (special *degreebound* *degreeratio* *lengthbound* *lengthratio*))
  (let ((d (max (maxdegree E) (maxdegree R) (maxdegree S)))
	(n (max (maxlength E) (maxlength R) (maxlength S))))
;;    (setf *degreebound* (max (* *degreeratio* d) *degreebound*))
;;    (setf *lengthbound* (max (* *lengthratio* n) *lengthbound*))))
    (setf *degreebound* (max (* *degreeratio* d) 3))
    (setf *lengthbound* (max (* *lengthratio* n) 5))
    (print-debug 2 t "max degree: ~a~%" d)
    (print-debug 2 t "max length: ~a~%" n)
    (print-debug 2 t "degree bound: ~a, length bound: ~a ~%" *degreebound* *lengthbound*)))


;;
;; Add extension/slack variables
;; E, R, S are sorted s.t. x > 0 occurs first. This way we don't incur additional
;; overhead when we introduce new var for x > 0 cases (no reordering reqd too)
;; - return the concatenation of E, R, S as a list of POL ...
;;
(defun add-extension-vars (E R S)
  (let* ((newR (sort R #'lesser?))
	 (newS (sort S #'lesser?))
	 (newE (sort E #'lesser?))
	 (E2E (mapcar #'(lambda(x) (make-pol (POL-pol x) (list x) nil)) newE))
	 (R2E (mapcar #'(lambda(x) (addU x '>)) newR))
	 (S2E (mapcar #'(lambda(x) (addU x '>=)) newS)))
    (merge 'list (merge 'list E2E R2E #'lesser?) S2E #'lesser?)))

(defun lesser? (x y)
  (case (prep:polyrepComparePoly (POL-pol x) (POL-pol y))
	(prep:less t) 
	(t nil)))

(defun get-new-var (op)
  (let* ((newu (intern (gensym))))
    (if (eq op '>) 
	(setf *newU* (cons newu *newU*))
	(setf *newV* (cons newu *newV*)))
    (prep:set-parameters (append (prep:get-parameters) (list newu)))
    (print-debug 10 t "new order is ~a~%" (prep:get-order))
    newu))

;; P, ret: POL; copy witness from P to ret.
(defun addU (P op)
  (let ((newu (get-new-var op))
	(pol (POL-pol P)) 
	(wit (if (eq (POL-wit P) -1) nil (list P))))
    (make-pol (prep:polyrepAddPoly pol (list (list -1 (cons newu 1)))) wit nil)))






;;----------------------------------------------------------------------------
;; Second phase of the algorithm
;; - search for an unsatisfiability witness after pivoting
;; - each pivoting step introduces a new variable and equation
;;
;; Before doing that: 
;; 1) remove useless polynomials from the Grobner basis. 
;;    Any polynomial whose leading term is an original variable can be removed.
;; 2) we homogenize the polynomials so that constant terms can be eliminated
;;----------------------------------------------------------------------------

;;
;; Generic filter: remove all polynomials of E that satisfy pred
;; - the input to pred must be a list of monomials (not a POL structure)
;; - preserve the ordering
;; 
(defun filter (E pred) (filter* E nil pred))

(defun filter* (E F pred)
  (cond ((null E) (nreverse F))
	((funcall pred (POL-pol (car E))) (filter* (cdr E) F pred))
	(t (filter* (cdr E) (cons (car E) F) pred))))


;;
;; Check whether p's leading polynomial is an original variable
;; 
(defun lead-term-is-orig? (p)
  (is-orig-pp? (cdar p)))

(defun is-orig-pp? (mu)
  (and (null (cdr mu)) (eq (cdar mu) 1) (is-orig-var? (caar mu))))

(defun is-orig-var? (v)
  (not (var-in-list? v (prep:get-parameters))))


;;
;; Check whether p is binomial
;; or of the form (a u + rest) with a>0 and rest all negative
;;                               or a<0 and rest all positive
;;
(defun binomial? (p)
  (or (null (cddr p))
      (and (null (cddar p)) 
	   (eq (cdadar p) 1) 
	   (or (and (> (caar p) 0) (every #'(lambda(x) (< (car x) 0)) (cdr p)))
      	       (and (< (caar p) 0) (every #'(lambda(x) (> (car x) 0)) (cdr p)))))))


;;
;; Filter for phase2
;;
(defun useless? (p) (or (lead-term-is-orig? p) (binomial? p)))


;;
;; Homogenize all polynomials in E
;; - introduce a new positive variable u for this
;;
(defun homogenize (E)
  (let ((newu (get-new-var '>)))
    (loop for i in E collect (homogenizePoly i newu))))

;;
;; BD: Fixed a bug here.
;; The monomial ordering is not necessarily preserved when we make
;; the terms homogeneous. So we need to sort the monomials.
;;
;; Example: if x and y are parameters with x < y
;; then y^2 is before x in the monomial ordering (cf. polyrepComparePP)
;; but if we multiply by a fresh parameter u to homogenize them,
;; y^2 should be after x u in the monomial ordering.
;;
;; NOTE: if p's leading term contains original variables then it will
;; remain leading term after multiplying by u^k (for any k).
;; - but this is not true if p contains only parameter variables
;; Consequence: to keep the result monic, we must normalize after homogenization
;;
(defun homogenizePoly (p u)
  (let ((N (prep:polyrepTotalDegreePoly (POL-pol p))))
    (make-POL
     :pol (normalize-poly (reorder (mapcar #'(lambda (x) (homogenize-mono x u N)) (POL-pol p))))
     :wit (POL-wit p) :c (POL-c p))))

;; multiply mu by u^k to reach degree n
(defun homogenize-mono (mu u n)
  (let ((d (prep:polyrepTotalDegreePoly (list mu))))
    (if (> n d) (append mu (list (cons u (- n d)))) mu)))

;; comparison for reordering
(defun mono-greater? (x y) 
  (case (prep:polyrepComparePP (cdr x) (cdr y))
    (prep:greater t)
    (t nil)))

(defun reorder (l) (sort l #'mono-greater?))



;;
;; Prepare G for phase 2:
;; - eliminate useless polynomials then homogenize
;; 
(defun prepare-phase2-polys (E)
  (let* ((E1 (filter E #'useless?))
	 (E2 (homogenize E1)))
    (print-debug 4 t "~%PREP FOR PHASE 2~%")
    (print-debug 4 t "Input basis:~%~a~%~%" (pp E :key #'POL-pol))
    (print-debug 4 t "After filtering:~%~a~%~%" (pp E1 :key #'POL-pol))
    (print-debug 4 t "After homogenization:~%~a~%~%" (pp E2 :key #'POL-pol))
    E2))






;;----------------------------------------------------------------------------
;; Pivoting
;; - create a new parameter u
;; - add the equation p - u = 0 to G
;; - compute the Grobner basis of the result
;; - p must be a polynomial in the prep representation (list of monomials)
;;----------------------------------------------------------------------------

;; keep a list of all variables created for pivoting
(defvar *auxvars* nil)

;; Note this may fail to compute a grobner basis
;; This returns a gbcomp
(defun pivot (G p)
  (let ((newp (make-aux-pol p)))
    (print-debug 1 t "PIVOT: new poly: ~a~%~%" (prep:polyrepPrint (POL-pol newp)))
    (print-debug 1 t "PIVOT: aux vars: ~a~%" *auxvars*)
    (grobner (list newp) G)))


;; Create a new parameter u (of same sign as p)
;; and return the POL (p - u)
;; for this new POL: witness = nil, c = nil
(defun make-aux-pol (p)
  (let ((pos (pos-poly? p))
	(newvar (intern (gensym))))
    (cond ((eq pos 1) (setf *newU* (cons newvar *newU*)))    ;; u is positive
	  ((eq pos 0) (setf *newV* (cons newvar *newV*))))   ;; u is non-negative
    (prep:set-parameters (append (prep:get-parameters) (list newvar)))
    (setf *auxvars* (cons newvar *auxvars*))
    (make-pol (prep:polyrepAddPoly p (list (list -1 (cons newvar 1)))) nil nil)))


;; Check whether p is positive or non-negative
;; returned value:
;; 1 means strictly positive (all monomials non-negative, one monomial positive)
;; 0 means non-negative (all monomials non-negative)
;; nil means don't know
(defun pos-poly? (p) (pos-poly* p 0))

(defun pos-poly* (p sgn)
  (cond ((null p) sgn)
	((< (caar p) 0) nil) ;; negative coefficient
	(t (let ((sgn-mono (pos-mono? (cdar p))))
	     (cond ((eq sgn-mono 1) (pos-poly* (cdr p) 1))
		   ((eq sgn-mono 0) (pos-poly* (cdr p) sgn))
		   (t nil))))))

;; check whether mu is a positive monomial
;; 1 means yes
;; 0 means non-negative
;; nil means can't tell
(defun pos-mono? (mu) (pos-mono* mu 1))
  
(defun pos-mono* (mu sgn)	      
  (cond ((null mu) sgn)
	((var-in-list? (caar mu) *newU*) (pos-mono* (cdr mu) sgn))
	((var-in-list? (caar mu) *newV*) (pos-mono* (cdr mu) 0))
	((evenp (cdar mu)) (pos-mono* (cdr mu) 0))
	(t nil)))







;;----------------------------------------------------------------
;; Heuristic 1:
;; - search for an "almost positive" polynomial p in the basis
;; - replace the leading term of p by a new variable
;;----------------------------------------------------------------

;;
;; Filter for heuristic 1: eliminate polynomials
;; whose leading term contains an original variable
;;

;; check whether x is unconstrained: not in *newU* and no in *newV*
(defun is-unconstrained-var? (x)
  (not (or (var-in-list? x *newU*) (var-in-list? x *newV*))))

;; check whether mu contains an unconstrained variable
(defun not-positive-pp? (mu)
  (some #'(lambda(x) (is-unconstrained-var? (car x))) mu))

;; check whether mu is an auxiliary variable
(defun aux-pp? (mu)
  (member (caar mu) *auxvars*))

;; check whether the leading term of p is non-linear
(defun non-linear-lead-term? (p) 
  (or (cddar p) (> (cdadar p) 1)))

;; filter for heuristic 1
(defun useless-for-heuristic1? (p)
  (or (non-linear-lead-term? p)
      (aux-pp? (cdar p))
      (not-positive-pp? (cdar p))
      (binomial? p)))


;; check whether p contains atmost N positive or negative monomials
(defun almost-pn-poly? (p N) (almost-pn-poly* p N 0 0))

;; pos = number of positive monomials in p
;; neg = number of negative monomials in p
;; - return t if (pos <= N or neg <= N)
(defun almost-pn-poly* (p N pos neg)
  (cond ((and (> pos N) (> neg N)) nil)
	((null p) t)
	((> (caar p) 0) (almost-pn-poly* (cdr p) N (1+ pos) neg))
	(t (almost-pn-poly* (cdr p) N pos (1+ neg)))))



;; search for an almost positive polynomial in G, i.e., a polynomial p
;; that satisfies (almost-pn-poly? p N).
;; - k = round index. It's used to avoid always picking the same p
;; - return nil if none is found
;; - return (POL ..) otherwise
(defun find-almost-pn-poly (G N k)
  (let ((G1 (filter G #'useless-for-heuristic1?)))
    (print-debug 4 t "~%---~%Heuristic 1: all polys:~%~a~%~%" (pp G :key #'POL-pol))
    (print-debug 4 t "Heuristic 1: after filtering: ~a~%~%" (pp G1 :key #'POL-pol))
    (find-if #'(lambda(x) (almost-pn-poly? (POL-pol x) N)) (cycle G1 k))))



;; if all else fails, pick a large polynomial
(defun find-large-linear-poly (G)
  (let ((G1 (filter G #'useless-for-heuristic1?)))
    (max-of-list G1 #'> :key '(lambda(x)(length (POL-pol x))))))


;; --> nil if plist is empty
;; --> element x in plist where (key x) is maximal
(defun max-of-list (plist fn &key (key #'(lambda(x) x)))
  (if plist (max-of-list* (cdr plist) fn (car plist) key)
    nil))

(defun max-of-list* (plist fn ans key)
  (if (null plist) ans 
      (if (funcall fn (funcall key (car plist)) (funcall key ans))
	  (max-of-list* (cdr plist) fn (car plist) key)
	  (max-of-list* (cdr plist) fn ans key))))





;; new trick: if p is selected, pivot on the positive 
;; prefix of p rather than just the leading term 
;; - p must be a list of monomials
;; DON'T USE: THIS MAKES THINGS WORSE!
(defun pos-prefix (p) (pos-prefix* p nil))

(defun pos-prefix* (p r)
  (cond ((null p) (nreverse r))
	((< (caar p) 0) (nreverse r))
	(t (pos-prefix* (cdr p) (cons (car p) r)))))

;; leading term of p
(defun make-pivoter (p) (list (cons 1 (cdar p))))






;;--------------------------------------------
;; Heuristic 2: search for a polynomial
;; of the form (A x^2 + B x + C)
;; then add the equation u = (A x + B/2)
;;--------------------------------------------

;; contruct two polynomials A and B such that p = A u + B t + C where 
;;    A u = the monomials of p divisible by u
;;    B t = the monomials of p divisible by t and not by u
;;    C  = the monomials of p not divisible by u or t
;; - p must be a list of monomials
;; - u and t must be two terms
;; - return the triple (A B C)
(defun decompose-poly (p u v)
  (decompose-poly* p u v nil nil nil))

; a, b, c --> store the monomials in reverse order
(defun decompose-poly* (p u v a b c)
  (if (null p)
      (values (nreverse a) (nreverse b) (nreverse c))
    (let* ((mono (car p))
	   (k (car mono))  ;; coefficient
	   (s (cdr mono))) ;; term
      (let ((s_by_u (prep:polyrepDividePP s u)))
	(if (not (eq s_by_u 0))
	    ;; s is divisible by u and s_by_u is (s/u)
	    (decompose-poly* (cdr p) u v (cons (cons k s_by_u) a) b c) 
	  (let ((s_by_v (prep:polyrepDividePP s v)))
	    (if (not (eq s_by_v 0))
		;; s is divisible by v and s_by_v is (s/v)
		(decompose-poly* (cdr p) u v a (cons (cons k s_by_v) b) c)
	      ;; s not divisible by u or v
	      (decompose-poly* (cdr p) u v a b (cons mono c)))))))))



;;
;; Find the largest power-product that divides mu and all monomials of p
;; 
(defun gcd-pp-poly (mu p)
  (cond ((null mu) mu) ;; mu = 1
	((null p) mu)  ;; p = 0
	(t (gcd-pp-poly (prep:polyrepGCD-PP mu (cdar p)) (cdr p)))))



;;
;; Check whether p is free of mu: no monomial of p has a common
;; divisor with mu
;;
(defun free-of-pp? (p mu)
  (every #'(lambda (x) (null (prep:polyrepGCD-PP mu (cdr x)))) p))



;;
;; Extract the largest common factor d of all monomials in a and b
;; then build the polynomial (a0 mu + b0/2) where 
;;   a0 = a/d and b0 = b/d
;; - mu is a power-product
;;
(defun make-h2-poly (mu a b)
  (if (null a) b ;; a = 0
    (let* 
	((d0 (cdar a)) ;; first term of a
	 (d1 (gcd-pp-poly d0 (cdr a)))
	 (d  (gcd-pp-poly d1 b)))
      (if d ;; d != 1
	  (make-h2-poly* mu (prep:polyrepDividePolyPP a d) (prep:polyrepDividePolyPP b d))
	(make-h2-poly* mu a b)))))

(defun make-h2-poly* (mu a0 b0)
  (prep:polyrepAddPoly (prep:polyrepMultiplyPPPoly mu a0) (prep:polyrepDividePolyCst b0 2)))



;;
;; check whether mu is quadratic
;;
(defun quadratic-pp? (mu)
  (every #'(lambda (x) (evenp (cdr x))) mu))

;;
;; root of mu
;;
(defun root-pp (mu)
  (assert (quadratic-pp? mu))
  (mapcar #'(lambda (x) (cons (car x) (/ (cdr x) 2))) mu))





;;
;; Largest quadratic divisor of mu
;;
(defun gqd-pp (mu) (gqd-pp* mu nil))

(defun gqd-pp* (mu d)
  (cond ((null mu) (nreverse d))
	((< (cdar mu) 2) (gqd-pp* (cdr mu) d))
	((evenp (cdar mu)) (gqd-pp* (cdr mu) (cons (car mu) d)))
	(t (gqd-pp* (cdr mu) (cons (cons (caar mu) (even-floor (cdar mu))) d)))))

;; largest even integer <= d
(defun even-floor (d) (* 2 (floor d 2)))



;;
;; Collect all the products (x^d) from mu where d is even 
;; and x is an original variable
;; - return a list of all these products
;;
(defun simple-even-powers (mu)
  (simple-even-powers* mu nil))

(defun simple-even-powers* (mu l)
  (cond ((null mu) l)
	((simple-even-pow? (car mu))
	 (simple-even-powers* (cdr mu) (cons (list (car mu)) l)))
	(t (simple-even-powers* (cdr mu) l))))

(defun simple-even-pow? (xd)
  (and (evenp (cdr xd)) (is-orig-var? (car xd))))




;;
;; Attempt to apply heuristic 2 on p
;; - contruct a polynomial (a u + b/2) if a u^2 + b u occurs in p
;;   and a u^2 occurs in p's leading monomial
;;   and u is (x^d) for an original variable x
;; - return nil (zero polynomial) if that fails
;;
(defun quad-attempt (p)
  (quad-attempt* p (simple-even-powers (cdar p))))

(defun quad-attempt* (p l)
  (if (null l) nil
    (let* ((u2 (car l))
	   (u (root-pp u2)))
      (multiple-value-bind 
       (a b c) (decompose-poly p u2 u)
       (if (and b (free-of-pp? a u) (free-of-pp? b u) (free-of-pp? c u))
	   ;; the decomposition worked: both a and b are non-null
	   ;; and neither a nor b contains a divisor of u
	   (progn 
	     (print-debug 0 t "~%Quad success~%")
	     (print-debug 0 t "    u = ~a~%" (prep:polyrepPrint (list (cons 1 u))))
	     (print-debug 0 t "    P = ~a~%" (prep:polyrepPrint p))
	     (print-debug 0 t "    A = ~a~%" (prep:polyrepPrint a))
	     (print-debug 0 t "    B = ~a~%" (prep:polyrepPrint b))
	     (print-debug 0 t "    C = ~a~%" (prep:polyrepPrint b))
	     (make-h2-poly u a b))
	 ;; failed decomposition: try next candidate
	 (quad-attempt* p (cdr l)))))))


;;
;; Filter for heuristic 2
;; 

;; if the lead term of p contains no simple even power, skip p
;; if p is too big, skip it too
(defun useless-for-heuristic2? (p)
  (or (not (bound-check? p))
      (not (some #'simple-even-pow? (cdar p)))))


;;
;; Apply heuristic 2 to G
;; - k = round index (not used for now)
(defun find-quad-instance (G)
  (let ((G1 (filter G #'useless-for-heuristic2?)))
    (print-debug 1 t "~%---~%Quad heuristic: all polys:~%~a~%~%" (pp G :key #'POL-pol))
    (print-debug 1 t "~%Quad heuristic: after filtering: ~a~%~%" (pp G1 :key #'POL-pol))
    (some #'(lambda (x) (quad-attempt (POL-pol x))) G1)))




;;----------------------------------------------------------------
;; Phase2 test:
;; - k = iteration counter, we give up when k >= *iteration-bound*
;; return a witness (as a list of POL ...) if unsat is detected
;; return nil otherwise
;;----------------------------------------------------------------

;; rotate list l by n
(defun cycle (l n)
  (let ((N (length l)))
    (loop for i from 0 upto (- N 1) collect (nth (mod (+ n i) N) l))))


;; try a heuristic to pick a polynomial
;; - choose = heuristic to pick an element of G
;; - k = round (used for tracing)
(defun try-heuristic (G name k choose)
  (let ((p (funcall choose G)))
    (trace-heuristic name k p)
    p))


;; print trace/debugging information
;; name = heuristic name
;; k = round
;; p = what the heuritic found
(defun trace-heuristic (name k p)
  (if (and *dlevel* (>= *dlevel* 0))
      (if p
	  (format t "Round ~a: ~a found: ~a~%" k name (prep:polyrepPrint (POL-pol p)))
	  (format t "Round ~a: ~a failed~%" k name))))

;; Apply heuritics in order
(defun pick-poly (G k)
  (or (try-heuristic G "H1" k #'(lambda (E) (find-almost-pn-poly E 1 k)))
      (try-heuristic G "H2" k #'(lambda (E) (find-almost-pn-poly E 2 k)))
      (try-heuristic G "Large" k #'find-large-linear-poly)))




;; trace + apply quad heuristic
(defun trace-quad (k p)
  (if (and *dlevel* (>= *dlevel* 0))
      (if p
	  (format t "Round ~a: Quad found: ~a~%" k (prep:polyrepPrint p))
          (format t "Round ~a: Quad failed~%" k))))   

(defun try-quad (G k)
  (let ((p (find-quad-instance G)))
    (trace-quad k p)
    p))



;; phase 2: G = basis
;; This can fail by either running out of resources during pivoting or
;; grobner basis computation.  In the event of a failure, (nil) is returned
(defun sos-tst2 (G)
  (setf *auxvars* nil)
  (sos-tst2* G 0))

(defun sos-tst2* (G k)
  (print-debug 0 t "~%SOS-TST2: k=~a~%" k)
  (if (and *dlevel* (> *dlevel* 0)) (show-basis G))
  (if (>= k *iteration-bound*) nil
    ;; q := new poly for pivoting or nil
    (let* ((p (pick-poly G k))
	   (q (if p (make-pivoter (POL-pol p)) (try-quad G k))))
      (if q 
	  ;; The heuristics worked
	  ;; add new equation u = q then compute the new basis
	  (let* ((comp (pivot G q)) (G1 (gbcomp-basis comp)))
	    (if (gbcomp-success comp)
		(if (unsat-grobner G1) (POL-wit (car G1)) (sos-tst2* G1 (1+ k))))
	    nil)
	  nil))))






;;-----------------------------------------------------------------------------
;; Full unsatisfiability test for (union E G)
;; - compute the grobner basis G1 for the union
;; - if the initial grobner basis computation fails, return the pair (t nil)
;; - if G1 is unsat (i.e., reduced to { 1 }), return the pair (t witness)
;;   where witness is the witness filed of the polynomial '1' in G1
;; - otherwise, try phase 2: remove the useless elements from G1 and attempt
;;   to build an unsatisfiability witness by pivoting
;; - if phase 2 fails, return the pair (t G1)
;;-----------------------------------------------------------------------------

(defun sos-full-tst (E G) 
  (print-debug 0 t "SOS-FULL-TEST~%")
  (print-debug 0 t "*degreebound*: ~a, *lengthbound*: ~a~%" *degreebound* *lengthbound*)
  (print-debug 5 t "polynomials in E:~%~a~%" (pp E :key #'POL-pol))
  (print-debug 5 t "polynomials in G:~%~a~%~%" (pp G :key #'POL-pol))  
  (let ((comp (grobner E G)))
    (if (gbcomp-success comp)
	(let ((G1 (gbcomp-basis comp)))
	  (if (unsat-grobner G1)
	      (values nil (POL-wit (car G1)))
	      ;; phase2: search for an unsatisfiability witness
	      (let ((wit (sos-tst2 (prepare-phase2-polys G1))))
		(if wit 
		    (values nil wit)     ;; witness found
		    (values t G1)))))
	(values t nil)))) ;;grobner-basis computation failed





;;; OLD CODE

;; old-definition
;; (defun almost-positive? (P &optional (N 1) (n 0) (m 0))
;;   (let ((p (POL-pol P)))
;;     (loop for i in p do (if (> (car i) 0) (setf n (+ n 1))) (if (> n N) (return nil)))
;;     (if (<= n N) p
;;       (progn
;;         (loop for i in p do (if (< (car i) 0) (setf m (+ m 1))) (if (> m N) (return nil)))
;; 	(if (<= m N) p nil)))))



;; 
;; Goal of sos-test2: search for a polynomial p in gb such that
;; - p is of the form c0 t0 + ... with the leading term t0 >= 0 (or t0 > 0)
;; - then recursively invoke sos-att with the added constraint (t0 >= 0) or (t0 > 0)
;; - this will introduce a new variable u with t0 - u as a new equation
;;   then t0 can be eliminated from the equations
;;
;; On the first round (i.e., K = 0), the polynomials are homogenized so that
;; constant terms can be eliminated too. For example,  p + c0 = 0 is turned into 
;; p + c0.u^d where d = degree of p.
;;
;; (defun sos-test2 (gb K Eret)
;;   (let ((rgb (loop for i in gb if (not (useless? (POL-pol i))) collect i)))
;;     (print-debug 5 t "Second phase: testing unsatisfiability of:~%~a~%" (pp rgb :key #'POL-pol))
;;     (if (<= (length rgb) 1) (values t gb)
;;         (let* ((rgb (sort rgb #'lesser?))
;; 	       (rgb (if (eq K 0) (homogenize rgb) rgb)))
;;     	  (multiple-value-bind (st wit) (sos-test2* rgb K Eret)
;; 	  	(if st (values t gb) (values nil wit)))))))


;; Heuristic: p is not useful 
;; - if its leading term t0 is an original variable
;; - if p is of the form (a0 t0 + a1 t1)
;; - if p's leading term contains an original variable
; (defun useless? (p)
;  (or (is-orig-pp? (cdar p)) (binomial? p)	;; COMPLETE
;      (not-positive-pp? (cdar p))))		;; INCOMPLETE



;; rename leading PP of "p" to a new var to make it smaller
;; and recurse
;; (defun sos-test2* (gb K Eret)
;;   (format t "SOS-TEST2 with K = ~a~%" K)
;;   (if (or (> K 13) (<= (length gb) 1)) (return-from sos-test2* (values t Eret)))
;;   (let* ((gb1 (cycle gb K))
;; 	 (p (some #'almost-positive? gb1))
;; 	 (p (if p p (some #'(lambda(x) (almost-positive? x 2)) gb1)))
;; 	 (p (if p p (maxOfList gb #'> :key #'(lambda(x)(length (POL-pol x))) :ret #'POL-pol)))
;; 	 (pol (make-pol (list (cons 1 (cdar p))) -1 nil))) ;; NOTE: Ignoring U,V distinction
;;     (print-debug 20 t "Main Poly is:~%~a~%" (pp (list p)))
;;     (sos-att nil (list pol) nil gb (+ K 1) Eret)))	;; NOTE: Ignoring U,V distinction/witness


;; ---------------------------------------------------------------------------
  
;; ----------------------------------------------------------------------------
;; maxOfList: return maximum element of list, use fn to compare.
;; ----------------------------------------------------------------------------
;; (defun maxOfList (plist fn &key (key #'(lambda(x) x)) (ret #'(lambda(x) x)))
;;   (assert (car plist))
;;   (maxOfList* (cdr plist) fn (car plist) key ret))

;; (defun maxOfList* (plist fn ans key ret)
;;   (if (null plist) (funcall ret ans)
;;       (if (funcall fn (funcall key (car plist)) (funcall key ans))
;; 	  (maxOfList* (cdr plist) fn (car plist) key ret)
;; 	  (maxOfList* (cdr plist) fn ans key ret))))
;; ----------------------------------------------------------------------------

