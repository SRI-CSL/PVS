;; /* Copyright (c) SRI International 2003, 2004. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; polyrep.lisp --
;; Author          : Ashish Tiwari
;; Created On      : Wed Feb 06, 2002
;; Last Modified By: Ashish Tiwari
;; Last Modified On: Wed Apr 26, 2006
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide :polynomial-representation-core)
(defpackage :polynomial-representation-core
  (:nicknames :prep)
  (:export :polyrepAddPoly :polyrepNegativePoly :polyrepMultiplyPoly
   :polyrepMultiplyMonoPoly :polyrepMultiplyCstPoly :polyrepMultiplyPPPoly
   :polyrepDividePolyPP :polyrepDividePolyCst :polyrepExpPolyCst 
   :polyrepMultiplyPP :polyrepDividePP :polyrepComparePP :polyrepGCD-PP
   :polyrepConstMultiple? :polyrepEqual? :polyrepComparePoly
   :polyrepTotalDegreePoly :greater :less :equal ;; Symbols returned by polyrepCompare
   :contains? :applySubstitution :getRelevantPols :allVarsIn :getExactlyRelevantPols
   :getAllRelevantPols :derivativeNewPol :polyrepGetVariables
   :set-variables :set-parameters :get-variables :get-parameters
   :set-var-equal? :var-equal? :get-order :polyrepConstant?
   :contains? :parameter? :interval? :polyrepPrint) ;; hsal-compose.lisp uses this.
  (:use :cl-user :common-lisp))		

(in-package :polynomial-representation-core)

;; Summary: Polynomials are represented as sums of monomials, i.e,
;; list of monomials. Each monomial is represented as a list whose
;; car is constant coefficient, and whose cdr is a power-product.
;; A power-product is a list of associations (x . exp), where x is
;; in *order*.*parameters* and exp is a positive integer. If package
;; SAL is present, then *order* comes from there, else from user.
;; See var-equal?

;; ASSUMES: sal::*order* and sal::*parameters* OR
;;	user::*order* and user::*parameters*. See (get-order)
;; Similarly, pvs:id.
;; *order*: list of whatever, I don't care!!
;; PROVIDES: simple functions to algebraically manipulate polynomials

(defvar *order* nil)		;; LOCAL
(defvar *variables* nil)	;; GLOBALLY accessible thro constructors
(defvar *parameters* nil)	;; GLOBALLY accessible thro constructors
(defvar *var-equal?* #'eq)	;; GLOBALLY accessible thro constructors

;; ------------------------------------------------------------------
;; Addition and Substraction
;; ------------------------------------------------------------------
;; add polynomials p and q given in the new representation
;; BD: changed this to make it tail recursive
(defun polyrepAddPoly (p q)
  (polyrepAddPoly* p q nil))

;; r accumulates the result as a list of monomials in reverse order
(defun polyrepAddPoly* (p q r)
  (cond 
   ((null p) (nreconc r q))
   ((null q) (nreconc r p))
   (t (let* ((c0s0 (car p))
	     (d0t0 (car q)))
	(case (polyrepComparePP (cdr c0s0) (cdr d0t0))
	 ('less    (polyrepAddPoly* p (cdr q) (cons d0t0 r)))
	 ('greater (polyrepAddPoly* (cdr p) q (cons c0s0 r)))
	 ('equal  
	  (let ((c1 (+ (car c0s0) (car d0t0))))
	    (polyrepAddPoly* (cdr p) (cdr q) 
			     (if (= c1 0) r (cons (cons c1 (cdr c0s0)) r))))))))))

;; non-tail recursive version
(defun polyrepAddPoly-old (p q)
  (cond ((null p) q)
	((null q) p)
	(t
  (let* ((c0s0 (car p))
	 (d0t0 (car q))
	 (leq (polyrepComparePP (cdr c0s0) (cdr d0t0))))
    (cond ((eq leq 'less) 
	   (cons d0t0 (polyrepAddPoly p (cdr q))))
	  ((eq leq 'greater)
	   (cons c0s0 (polyrepAddPoly (cdr p) q)))
	  ((eq leq 'equal)
	   (let ((c1 (+ (car c0s0) (car d0t0))))
	     (if (= c1 0)
		 (polyrepAddPoly (cdr p) (cdr q))
	         (cons (cons c1 (cdr c0s0)) 
		       (polyrepAddPoly (cdr p) (cdr q)))))))))))

;; multiply polynomial p by -1: p is given in the new representation
(defun polyrepNegativePoly (p)
  (loop for cisi in p collect (cons (- (car cisi)) (cdr cisi))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Multiplication
;; ------------------------------------------------------------------
;; multiply polynomials p and q given in the new representation
(defun polyrepMultiplyPoly (p q)
  (cond ((or (null p) (null q)) 
	 '())
	((null (cdr p))
	 (polyrepMultiplyMonoPoly (car p) q))
	((null (cdr q))
	 (polyrepMultiplyMonoPoly (car q) p))
	(t
	 (let* ((p1q (polyrepMultiplyMonoPoly (car p) q))
		(prq (polyrepMultiplyPoly (cdr p) q)))
	   (polyrepAddPoly p1q prq)))))

;; multiply power-products p and q given in the new representation
(defun polyrepMultiplyPP (s0 t0 &optional (order (get-order)) (fn #'var-equal?) &key (strict? nil))
  (if (and (null s0) (null t0)) (return-from polyrepMultiplyPP nil))
  (if (null s0) (return-from polyrepMultiplyPP
      (if (and strict? (some #'(lambda(x) (< (cdr x) 0)) t0)) 0 t0)))
  (if (null t0) (return-from polyrepMultiplyPP
      (if (and strict? (some #'(lambda(x) (< (cdr x) 0)) s0)) 0 s0)))
  (let ((x1 (caar s0))
	(y1 (caar t0)))
    (case (polyrepCompareVar x1 y1 :order order :test fn)
 	  (equal
	   (let ((expo (+ (cdar s0) (cdar t0))))
	     (cond ((eq expo 0) 
	 	    (polyrepMultiplyPP (cdr s0) (cdr t0) order fn :strict? strict?))
		   ((null strict?) 
		    (cons (cons x1 expo) (polyrepMultiplyPP (cdr s0) (cdr t0) order fn :strict? strict?)))
		   (t 
		    (if (< expo 0) 0
		        (let ((ans (polyrepMultiplyPP (cdr s0) (cdr t0) order fn :strict? t)))
			  (if (eq ans 0) 0 (cons (cons x1 expo) ans))))))))
	  (greater 
	   (if (and strict? (< (cdar s0) 0)) 0 
	       (let ((ans (polyrepMultiplyPP (cdr s0) t0 order fn :strict? strict?)))
		 (if (eq ans 0) 0 (cons (car s0) ans)))))
	  (t 
	   (if (and strict? (< (cdar t0) 0)) 0
	       (let ((ans (polyrepMultiplyPP s0 (cdr t0) order fn :strict? strict?)))
		 (if (eq ans 0) 0 (cons (car t0) ans))))))))

;; multiply monomial m with polynomial p
(defun polyrepMultiplyMonoPoly (d0t0 p)
  (loop for c0s0 in p collect 
	(cons (* (car d0t0) (car c0s0)) 
	      (polyrepMultiplyPP (cdr d0t0) (cdr c0s0)))))

(defun polyrepMultiplyPPPoly (t0 p)
  (loop for c0s0 in p collect 
	(cons (car c0s0) (polyrepMultiplyPP t0 (cdr c0s0)))))

;; multiply poly p with constant c
(defun polyrepMultiplyCstPoly (c p)
  (if (eq c 1) p (if (eq c 0) nil
  (loop for c0s0 in p collect (cons (* c (car c0s0)) (cdr c0s0))))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Division
;; ------------------------------------------------------------------
;; divide a polynomial by a power-product
(defun polyrepDividePolyPP (poly mu)
  (polyrepMultiplyPPPoly (polyrepExpPPCst mu -1) poly))

(defun polyrepDivideMonoPP (c0s0 mu)
  (cons (car c0s0) (polyrepDividePP (cdr c0s0) mu)))

;; divide the power-product mu by power-product nu.
;; Return 0 if cannot divide!
(defun polyrepDividePP (mu nu)
  (polyrepMultiplyPP mu (polyrepExpPPCst nu -1) (get-order) #'var-equal? :strict? t))

;; divide polynomial by a constant
(defun polyrepDividePolyCst (p c)
  (cond ((eq c 0) (format t "Cannot divide by zero.~%") nil)
	((eq c 1) p)
	(t (loop for c0s0 in p collect (cons (/ (car c0s0) c) (cdr c0s0))))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Exponentiation
;; ------------------------------------------------------------------
(defun polyrepExpPolyCst (p c)
  (cond ((>= c 1) (polyrepExpPolyCst* p c))
	((eq c 0) '((1)))
	((null (cdr p)) 	;; Power-product
	 (list (polyrepExpMonoCst (car p) c)))
	(t (format t "Can't divide by polynomials~%") (break))))

(defun polyrepExpMonoCst (c0s0 c)
  (cons (expt (car c0s0) c) (polyrepExpPPCst (cdr c0s0) c)))

(defun polyrepExpPPCst (s0 c)
  (loop for i in s0 collect (cons (car i) (* c (cdr i)))))

(defun polyrepExpPolyCst* (p c)
  (if (eq c 1) p
      (polyrepMultiplyPoly p (polyrepExpPolyCst* p (- c 1)))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Return the total degree of the polynomial.
;; ------------------------------------------------------------------
(defun polyrepTotalDegreePoly (p)
  (apply #'max (cons 0 (mapcar #'(lambda (x) (apply #'+ (mapcar #'cdr (cdr x)))) p))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Comparision: LEXICOGRAPHIC PATH ORDERING. Not total-deg-lex!
;; ------------------------------------------------------------------
(defun polyrepComparePoly (p q)
  (cond ((and (null p) (null q)) 'equal)
	((null p) 'less)
	((null q) 'greater)
	(t (case (polyrepComparePP (cdar p) (cdar q))
		 (equal (polyrepComparePoly (cdr p) (cdr q)))
		 (less 'less)
		 (greater 'greater)))))
  
(defun polyrepComparePP (mu1 mu2)
  (polyrepComparePP* mu1 mu2 (get-order) #'var-equal?))

;;---------------------------------------------------
;; BD: attempt to use a different monomial ordering
;;---------------------------------------------------

;; degree-pair mu = (d1 d2) where 
;; d1 = total degree of mu in variables
;; d2 = total degree of mu in parameters
(defun polyrepDegreePair (mu)
  (polyrepDegreePair* mu 0 0))

(defun polyrepDegreePair* (mu d1 d2)
  (cond ((null mu) (values d1 d2))
	((parameter? (caar mu)) (polyrepDegreePair* (cdr mu) d1 (+ d2 (cdar mu))))
	(t (polyrepDegreePair* (cdr mu) (+ d1 (cdar mu)) d2))))

;; comparison using lexicographic ordering
(defun polyrepLexComparePP* (mu1 mu2 order fn)
  (cond ((and (null mu1) (null mu2)) 'equal)
	((null mu1) 'less)
	((null mu2) 'greater)
	(t (case (polyrepCompareVar (caar mu1) (caar mu2) :order order :test fn)
		 ('equal (cond ((= (cdar mu1) (cdar mu2))
				(polyrepLexComparePP* (cdr mu1) (cdr mu2) order fn))
			       ((> (cdar mu1) (cdar mu2)) 'greater)
			       (t 'less)))
		 ('greater 'greater)
		 ('less 'less)))))


(defun polyrepComparePP* (mu1 mu2 order fn)
  (polyrepComparePP-old* mu1 mu2 order fn))

;; new ordering
(defun polyrepComparePP-new* (mu1 mu2 order fn)
  (multiple-value-bind 
   (v1 p1) (polyrepDegreePair mu1)
   (multiple-value-bind 
    (v2 p2) (polyrepDegreePair mu2)
    (cond ((> v1 v2) 'greater)
 	  ((< v1 v2) 'less)
 	  ((> p1 p2) 'greater)
 	  ((< p1 p2) 'less)
 	  (t (polyrepLexComparePP*  mu1 mu2 order fn))))))

		       

;; old ordering: lexicographic on the variables
;; total deglex on the parameters
(defun polyrepComparePP-old* (mu1 mu2 order fn)
  (cond ((and (null mu1) (null mu2))
	 'equal)
	((null mu1)
	 (if (> (cdar mu2) 0) 'less 'greater))
	((null mu2)
	 (if (> (cdar mu1) 0) 'greater 'less))
	(t 
 	   (if (and (parameter? (caar mu1)) (parameter? (caar mu2)))
	       (let ((n1 (apply #'+ (mapcar #'cdr mu1)))
	       	     (n2 (apply #'+ (mapcar #'cdr mu2))))
		 (if (> n1 n2) (return-from polyrepComparePP-old* 'greater)) 
		 (if (< n1 n2) (return-from polyrepComparePP-old* 'less)))) 
	   (case (polyrepCompareVar (caar mu1) (caar mu2) :order order :test fn)
	         (equal (cond ((eq (cdar mu1) (cdar mu2))
			       (polyrepComparePP* (cdr mu1) (cdr mu2) order fn))
			      ((> (cdar mu1) (cdar mu2)) 'greater)
			      (t 'less)))
		 (greater (if (> (cdar mu1) 0) 'greater 'less))
		 (t (if (> (cdar mu2) 0) 'less 'greater))))))

(defun polyrepCompareVar (v1 v2 &key (order (get-order)) (test #'eq))
  (if (funcall test v1 v2) 'equal
      (loop for i in order if (funcall test v1 i) return 'greater
			   if (funcall test v2 i) return 'less)))



;;------------------------------------
;; BD: GCD of two terms mu1 and mu2
;;------------------------------------

(defun polyrepGCD-PP (mu nu)
  (gcd-pp* mu nu nil (get-order) #'var-equal?))

; accumulate the result in g (in reverse order)
(defun gcd-pp* (mu nu g order fn)
  (cond ((null mu) (nreverse g))
	((null nu) (nreverse g))
	(t (case (polyrepCompareVar (caar mu) (caar nu) :order order :test fn)
		 ('equal
		  (let ((d (min (cdar mu) (cdar nu)))) ;; min degree
		    (gcd-pp* (cdr mu) (cdr nu) (cons (cons (caar mu) d) g) order fn)))
		 ('greater (gcd-pp* (cdr mu) nu g order fn))
		 (t (gcd-pp* mu (cdr nu) g order fn))))))





;; Note this is not comparison in the above sense. It is check for identity
;; and not a comparison in the lexicographic ordering.
(defun polyrepEqual? (p1 q1)
  (if (eq p1 q1) (return-from polyrepEqual? t))
  (cond ((and (null p1) (null q1))
	 t)
	((null p1)
	 nil)
	((null q1)
	 nil)
	((not (eql (caar p1) (caar q1)))
	 nil)
	(t (let* ((mu1 (cdar p1))
		  (nu1 (cdar q1))
		  (cmp (if (eq mu1 nu1) 'equal (polyrepComparePP mu1 nu1))))
	     (if (eq cmp 'equal)
		 (polyrepEqual? (cdr p1) (cdr q1))
		 nil)))))

(defun polyrepConstMultiple? (p1 q1)
  (cond ((and (null p1) (null q1)) 1)
	((null q1) nil)
	((null p1) 0)
	(t
  (let* ((c1 (caar p1))
	 (d1 (caar q1))
	 (p11 (polyrepDividePolyCst p1 c1))
	 (q11 (polyrepDividePolyCst q1 d1))
	 (ans (if (> c1 0) 
		  (if (> d1 0) 1 -1)
		  (if (> d1 0) -1 1))))
    (if (polyrepEqual? p11 q11) ans nil)))))

(defun polyrepConstant? (p)
  (and (null (cdr p)) (null (cdar p))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; contains?: (p:polynomial-in-new-rep var) --> boolean
;; return-value: t if p contains var. 
;; Var: in same representation as in the polynomial representation.
;; ------------------------------------------------------------------
(defun contains? (p var &key (orr? t))
  (if (listp var) 
      (if orr?
          (if (null var) nil 
	      (or (containsPoly? p (car var)) (contains? p (cdr var))))
          (if (null var) t 
	      (and (containsPoly? p (car var)) 
		   (contains? p (cdr var) :orr? orr?))))
      (containsPoly? p var)))

(defun containsPoly? (p var)
  (if (null p) nil
      (or (containsMono? (car p) var) (containsPoly? (cdr p) var))))

(defun containsMono? (p var)
  (containsPP? (cdr p) var))

(defun containsPP? (p var)
  (if (null p) nil
      (if (var-equal? var (caar p)) t (containsPP? (cdr p) var))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; applySubtitution: (p:poly-in-new-rep list of (x.f(x)))
;; Return Value: p(f(x))
;; x: In same representation as in p's representation.
;; ------------------------------------------------------------------
(defun applySubstitution (p map)
  (if (null map) p
      (applySubstitution (applySubstitutionPoly p (car map)) (cdr map))))

(defun applySubstitutionPoly (p xfx)
  (if (null p) nil
      (let* ((m1 (applySubstitutionMono (car p) xfx))
	     (r1 (applySubstitutionPoly (cdr p) xfx)))
	(polyrepAddPoly m1 r1))))

(defun applySubstitutionMono (p xfx)
  (let* ((c1 (car p))
	 (r1 (applySubstitutionPP (cdr p) xfx)))
    (polyrepMultiplyCstPoly c1 r1)))

(defun applySubstitutionPP (p xfx)
  (cond ((null p)
	 (list (list 1)))
	((null (cdr p))
	 (applySubstitutionUni (car p) xfx))
	(t 
	 (polyrepMultiplyPoly (applySubstitutionUni (car p) xfx)
			 (applySubstitutionPP  (cdr p) xfx)))))

(defun applySubstitutionUni (xToC xfx)
  (let* ((x1 (car xToC))
	 (c1 (cdr xToC))
	 (y1 (car xfx))
	 (fy (cdr xfx)))
    (if (var-equal? x1 y1)
	(polyrepExpPolyCst fy c1)
	(list (cons 1 (list xToC))))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; get-all-variables-in: p:poly-in-new-rep --> list of pvs:name-exprs
;; ------------------------------------------------------------------
(defun allVarsIn (p &optional (vars '()) (pars '()))
  (allVarsInPoly p vars pars))

(defun allVarsInPoly (p &optional (vars '()) (pars '()))
  (if (null p) (values vars pars)
      (multiple-value-bind (nvars npars) (allVarsInPP (cdar p) vars pars)
        (allVarsInPoly (cdr p) nvars npars))))

(defun allVarsInPP (p &optional (vars '()) (pars '()))
  (if (null p) (values vars pars)
      ; (if (member (caar p) res) )
      (if (parameter? (caar p))
	  (if (member (caar p) pars)
	      (allVarsInPP (cdr p) vars pars)
	      (allVarsInPP (cdr p) vars (cons (caar p) pars)))
	  (if (member (caar p) vars)
	      (allVarsInPP (cdr p) vars pars)
	      (allVarsInPP (cdr p) (cons (caar p) vars) pars)))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; getRelevantPols: pol-list vars --> pol-list
;; Return Value: Subset of input pols that contains some var in vars.
;; ------------------------------------------------------------------
(defun getRelevantPols (plist vlist &key (indices? nil) (key nil))
  (loop for i in plist 
	 as j upfrom 0 if (contains? (if key (funcall key i) i) vlist) 
	collect (if indices? (nth j indices?) i)))

(defun getAllRelevantPols (plist vlist)
  (loop for i in plist for j = (allVarsIn i) 
	if (subsetp j vlist) collect (if (subsetp vlist j) (cons i t) (cons i nil))))

(defun getExactlyRelevantPols (plist vlist)
  (loop for i in plist for j = (allVarsIn i) 
	if (and (subsetp vlist j) (subsetp j vlist)) collect i)) 
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; Dynamic Aspects of this Package!
;; ------------------------------------------------------------------
(defun var-equal? (x y)		;; pvs:tc-eq
  (funcall *var-equal?* x y))

(defun set-var-equal? (f)
  (setf *var-equal?* f))
;  (let ((pvs (find-package "pvs"))) 
    ;(eq (if pvs (funcall (find-symbol "id" pvs) x) x)
 	;(if pvs (funcall (find-symbol "id" pvs) y) y)))

(defun get-order ()
  *order*)

(defun set-variables (vars)
  (setf *variables* vars)
  (setf *order* (append *variables* *parameters*)))
  ;(let* ((sal1 (find-package "sal"))
	 ;(sal2 (if sal1 sal1 (find-package "user"))))
    ;(append (eval (find-symbol "*order*" sal2))
	    ;(eval (find-symbol "*parameters*" sal2))))

(defun get-variables ()
  *variables*)
  
(defun set-parameters (params)
  (setf *parameters* params)
  (setf *order* (append *variables* *parameters*)))

(defun get-parameters ()
  *parameters*)
  ;(let* ((sal1 (find-package "sal"))
	 ;(sal2 (if sal1 sal1 (find-package "user"))))
    ;(eval (find-symbol "*order*" sal2)))

(defun parameter? (x)
  (member x *parameters* :test #'var-equal?))

(defun interval? (p)
  (and (null (cddr p)) (null (cddar p)) (cadar p) (eq (cdadar p) 1) 
       (or (null (cdr p)) (null (cdadr p)))))

(defun difference? (p)
  (if (and (null (cddr p)) (null (cddar p)) (cadar p) (eq (cdadar p) 1) 
       (cdr p) (null (cddr (nth 1 p))) (eq (cdr (nth 1 (nth 1 p))) 1))
      (car (nth 1 (nth 1 p)))))
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;; flow: var to poly-in-new-rep mapping. pol: polynomial in new-rep
;; Return: derivative of the polynomial in new-rep.
;; ------------------------------------------------------------------
(defun derivativeNewPol (pol flow)
  ;; (break "derivativeNew-application")
  ;; (debug-msg "Into derivativeNewPol on polynomial ~A.~%" pol)
  (if (null pol)
      '()
      (let* ((term1 (car pol))
	     (rest1 (cdr pol))
	     (c1 (car term1))
	     (mu1 (cdr term1))
	     (mu1dot (derivativeNewPP mu1 flow)))
	(if (null mu1dot)
	    (derivativeNewPol rest1 flow)
	    (let ((term1dot (polyrepMultiplyCstPoly c1 mu1dot)))
	      (polyrepAddPoly term1dot (derivativeNewPol rest1 flow)))))))

(defun derivativeNewPP (pp flow)
  (cond ((null pp)
	 nil)
	((null (cdr pp))
	 (let* ((var (caar pp))
		(exp (cdar pp))
		(vardot (derivativeNewVar var flow)))
	   (cond ((null vardot) nil)
		 ((eq exp 1) vardot)
		 (t
		  (polyrepMultiplyMonoPoly (cons exp 
						 (list (cons var (- exp 1)))) 
				    vardot)))))
	(t
	 (let* ((var (caar pp))
		(exp (cdar pp))
		(tmp (list (cons var (- exp 1))))
		(vardot (derivativeNewVar var flow))
		(restdot (derivativeNewPP (cdr pp) flow))
	   	(thisdot (cond ((null vardot) nil)
				((eq exp 1) vardot)
				(t (polyrepMultiplyMonoPoly (cons exp tmp) 
							    vardot))))
		(thisdotrest (polyrepMultiplyMonoPoly (cons 1 (cdr pp)) 
						      thisdot))
		(thisrestdot (polyrepMultiplyMonoPoly (cons 1 (list (car pp))) 
						      restdot)))
	   (polyrepAddPoly thisdotrest thisrestdot)))))

(defun derivativeNewVar (var flow)
  ;;(debug-msg "Into derivativeNewVar on variable ~A.~%" var)
  (let* ((xxdot (assoc var flow :test #'var-equal?))	;; CHECK CHECK: eq OK?
	 (xdot (cdr xxdot)))
    (if (null xdot) nil xdot)))
;; ============================================================================

;; ============================================================================
;; Return all variables and parameters in this polynomial
;; ============================================================================
(defun polyrepGetVariables (p)
  (polyrepGetVariables* p (get-variables) nil nil))

(defun polyrepGetVariables* (p variables vars params)
  (if (null p) (cons vars params)
      (let* ((pp1 (car p))
	     (res (polyrepGetVariablesPP (cdr pp1) variables vars params)))
	(polyrepGetVariables* (cdr p) variables (car res) (cdr res)))))

(defun polyrepGetVariablesPP (mu variables vars params)
  (if (null mu) (cons vars params)
      (let ((v1 (caar mu)))
	(if (or (member v1 vars :test #'var-equal?)
		(member v1 params :test #'var-equal?))
	    (polyrepGetVariablesPP (cdr mu) variables vars params)
	    (if (member v1 variables :test #'var-equal?)
		(polyrepGetVariablesPP (cdr mu) variables (cons v1 vars) params)
		(polyrepGetVariablesPP (cdr mu) variables vars (cons v1 params)))))))
;; ============================================================================

;; ============================================================================
;; Print routine
;; ============================================================================
(defun polyrepPrint (pol)
  (if (null pol) (format nil "0")
      (apply #'concatenate 'string (polyrepPrintMono (car pol) t)
        (loop for i in (cdr pol) collect (polyrepPrintMono i nil)))))

(defun polyrepPrintMono (cmu head?)
  ;; (if (null (cdr cmu)) (return-from polyrepPrintMono (format nil "~a" (car cmu))))
  (if (null (cdr cmu)) (return-from polyrepPrintMono (polyrepPrintCoef (car cmu) head? nil)))
  (let ((coeff (polyrepPrintCoef (car cmu) head? t))
    	(pp1 (polyrepPrintPP1 (cadr cmu))))
    (apply #'concatenate 'string coeff pp1 (loop for i in (cddr cmu) nconc (list "*" (polyrepPrintPP1 i))))))

(defun polyrepPrintPP1 (vardotexp)
  (concatenate 'string (format nil "~a" (car vardotexp))
    (if (not (eq (cdr vardotexp) 1)) (format nil "^~a" (cdr vardotexp)) "")))

(defun polyrepPrintCoef (num head? tail?)
  (cond ((and head? (eq num 1) tail?) 
	 "")
	((and head? (eq num 1) (null tail?)) 
	 "1")
	((and head? tail?) 
	 (format nil "~a*" num))
	((and head? (null tail?)) 
	 (format nil "~a" num))
	((null tail?)
	 (format nil " ~a ~a" (if (> num 0) '+ '-) (abs num)))
	((eq (abs num) 1)
	 (format nil " ~a " (if (> num 0) '+ '-)))
	(t 
	 (format nil " ~a ~a*" (if (> num 0) '+ '-) (abs num)))))
;; ============================================================================
