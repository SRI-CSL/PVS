;;;
;;; Multivariate Commutative Polynomial Algebra and Ideal Membership Routines 
;;;   with polynomials taken over Q[\vec{x}] and algebraic varieties taken over the complexes.
;;;
;;;   including: basic polynomial arithmetic,
;;;              degree-lexicographic and degree-reverse-lexicographic term orderings,
;;;              univariate polynomial division and GCD,
;;;              multivariate polynomial division,
;;;              multivariate power-product LCM,
;;;              reduction of arbitrary Q[x] ideals to principal ideals
;;;               (allowing us to treat Q[x] properly as a P.I.D.),
;;;              univariate ideal membership decisions utilizing principal ideal reduction,
;;;              construction of S-polynomials,
;;;              Groebner basis derivation using S-polynomials,
;;;              multivariate ideal membership decisions utilizing Groebner bases,
;;;              minimization of Groebner bases to minimal dimension with monic leads,
;;;              reduction of arbitrary Groebner bases to canonical reduced Groebner bases,
;;;              ideal triviality checking,
;;;              some simple positivity predicates,
;;;              pretty-printing for polynomials.
;;;
;;;      Note: all core computations are parameterized by the active term ordering, which
;;;            is itself parameterized by the active variable ordering.  
;;;            * See SET-ACTIVE-MONOMIAL-ORDERING and *VARS-TABLE* for more.
;;;
;;;      To do:
;;;              a priori 0-reduction of S-polynomials for pairwise relative prime polynomials
;;;               (a.k.a. Buchberger's Criterion I from ``A Criterion for Detecting Unnecessary 
;;;                 Reductions in Polynomial Ideal Theory,'' in Recent Trends in Multidimensional 
;;;                 System Theory, edited by N.K. Bose. Chapter 6, pp.184-232. D. Reidel 
;;;                 Publishing Company, 1986.),
;;;           
;;;   for
;;;
;;;     RAHD: Real Algebra in High Dimensions
;;;   
;;;   v0.0,
;;;
;;; A feasible decision method for the existential theory of real closed fields.
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: (g.passmore@ed.ac.uk . http://homepages.inf.ed.ac.uk/s0793114/)
;;;
;;; This file: began on         16-June-2008,
;;;            last updated on  25-Sept-2008.
;;;

(in-package RAHD)

;;;
;;; Format of monomials: '(c . ((v_{n-1} . p_{n-1}) (v_{n-2} . p_{n-2}) ... (v_0 . p_0)))
;;;  where c is a rational constant,
;;;   each v_i is a natural number corresponding to the variable's position in 
;;;    *var-table*, and
;;;   each p_i is a natural number corresponding to the power of v_i.
;;;
;;;   *** In a monomial, the left-most variable is the monomial's smallest variable in terms
;;;   of the variable ordering defined by *VARS-TABLE*.  This *VARS-TABLE* ordering is s.t.
;;;   a variable v_1 is less than another variable v_2 if v_1's index in *VARS-TABLE* is greater 
;;;   than v_2's.  So, the smaller a variable's numerical identifier, the higher it is in
;;;   the variable ordering.  This is for efficiency (see below).
;;;
;;;
;;; Format of polynomials: '(m_0 m_1 ... m_{n-1}) where each m_i is a monomial s.t.
;;;   m_0 >> m_1 >> ... >> m_{n-1} under the active monomial ordering (MO<) which is set 
;;;   initially to DEG-REV-LEX.
;;;
;;; Note the difference in storage order:
;;;  Monomial as above:    v_{n-1} <<< v_{n-2} <<< ... <<< v_0,
;;;  Polynomial as above:  m_0 >> m_1 >> ... >> m_{n-1},
;;; 
;;;    where <<< is the variable ordering defined by *VARS-TABLE*
;;;        s.t. *VARS-TABLE* = '(x_0 x_1 ... x_{k-1})  ==>  x_0 >>> x_1 >>> ... >>> x_{k-1},
;;;    and << is the current monomial ordering, which is DEG-REV-LEX by default, but can be
;;;        changed by invoking (SET-ACTIVE-MONOMIAL-ORDERING 'DEG-LEX), for instance.
;;;
;;; Why do we store monomials with their variable/power pairs in reverse order w.r.t. the
;;; variable ordering?  Because this is convenient for computing the degree-reverse-lexicographic 
;;; ordering on monomials, as it avoids the need for calling REVERSE upon the monomials being 
;;; compared.  DEG-REV-LEX is especially convenient for computing Groebner bases.
;;;

(defparameter *vars-table*
  '(x y z u v w a b c d e f g h i j k l m n o p q r s t))

;;; MCOEF: Given a monomial, return its leading coefficient.

(defun mcoeff (m)
  (car m))

;;;
;;; MPP: Given a monomial, return its power-product of variables
;;; (e.g. the monomial with the leading coefficient removed).
;;;

(defun mpp (m)
  (cdr m))

;;;
;;; MMULT-PP (pp1 pp2): Given two power-products of variables,
;;; multiply them and return the product in canonical form.
;;;
;;; Note: This canonical form is as given by the reverse of *VARS-TABLE* 
;;; and places variable-powers in order from smallest to largest
;;; in terms of the variable ordering (amenable to DEG-REV-LEX).
;;;
;;; Note: We assume both pp1 and pp2 are already in 
;;; canonical form!
;;;

(defun mmult-pp (pp1 pp2)
  (cond ((eq pp1 nil) pp2)
	((eq pp2 nil) pp1)
	(t (let ((cur-pp1-vp (car pp1))
		 (cur-pp2-vp (car pp2)))
	     (let ((var-id1 (car cur-pp1-vp))
		   (var-id2 (car cur-pp2-vp))
		   (var-pow1 (cdr cur-pp1-vp))
		   (var-pow2 (cdr cur-pp2-vp)))
	       (cond ((= var-id1 var-id2) 
		      (cons (cons var-id1 (+ var-pow1 var-pow2))
			    (mmult-pp (cdr pp1) (cdr pp2))))
		     ((> var-id1 var-id2)
		      (cons cur-pp1-vp 
			    (mmult-pp (cdr pp1) pp2)))
		     ((< var-id1 var-id2)
		      (cons cur-pp2-vp
			    (mmult-pp pp1 (cdr pp2))))))))))

;;; S*: Multiplication for scalars (takes into account NIL=0).

(defun s* (x y)
  (cond ((or (eq x nil) (eq y nil)) 0)
	(t (* x y))))

;;; MCONS: Given two monomials, m1 and m2, return their product.

(defun mcons (m1 m2)
  (cond ((eq m1 nil) m2)
	((eq m2 nil) m1)
	(t (cons (* (mcoeff m1) (mcoeff m2))
		 (mmult-pp (mpp m1) (mpp m2))))))

;;; MNEGATE: Negate a monomial.

(defun mnegate (m)
  (cons (- (mcoeff m)) (cdr m)))

;;; MDEG: Compute the degree of a monomial.

(defun mdeg (m)
  (mdegpp (mpp m)))

(defun mdegpp (pp)
  (cond ((eq pp nil) 0)
	(t (+ (cdar pp)
	      (mdegpp (cdr pp))))))

;;;
;;; DEG-DIFF: Given two monomials, return the exponents for which
;;; the monomials first differ (variables are assumed to be already in
;;; canonical order from smallest to largest as in DEG-REV-LEX).  This
;;; can be overrided manually -- see below.
;;;
;;;                     m1     m2
;;;                     ||     ||
;;; Example: DEG-DIFF(2x3z^2, 5xyz) under x > y > z  
;;;
;;;            ==> (2 . 1)
;;;
;;;                 indicating that in the first position in which
;;;                 m1 and m2 differ under the current variable ordering from
;;;                 the right (e.g. in reverse as in deg-rev-lex), 
;;;                 pow(m1(i)) = 2 and pow(m2(i)) = 1.
;;;
;;; If an alternate ordering should be used, such as DEG-LEX, then 
;;; pass 'DEG-LEX as o.  Other orderings can be supported easily if needed.
;;;

(defun deg-diff (m1 m2 o)
  (let ((pp1 (if (eq o 'DEG-LEX) (reverse (mpp m1)) (mpp m1)))
	(pp2 (if (eq o 'DEG-LEX) (reverse (mpp m2)) (mpp m2))))
    (cond ((eq pp1 pp2) nil)
	  (t (pp-deg-diff pp1 pp2 o)))))

(defun pp-deg-diff (pp1 pp2 o) ; o should be 'DEG-REV-LEX or 'DEG-LEX
  (cond ((and (eq pp1 nil)
	      (eq pp2 nil)) nil)
	((and (eq pp1 nil)
	      (consp pp2))
	 (cons 0 (cdar pp2)))
	((and (consp pp1)
	      (eq pp2 nil))
	 (cons (cdar pp1) 0))
	(t (let ((var-id1 (caar pp1))
		 (var-id2 (caar pp2))
		 (var-pow1 (cdar pp1))
		 (var-pow2 (cdar pp2))
		 (o> (if (eq o 'DEG-REV-LEX) #'> #'<))
		 (o< (if (eq o 'DEG-REV-LEX) #'< #'>)))
	     (cond ((funcall o> var-id1 var-id2)
		    (cons var-pow1 0))
		   ((funcall o< var-id1 var-id2)
		    (cons 0 var-pow2))
		   ((not (= var-pow1 var-pow2))
		    (cons var-pow1 var-pow2))
		   (t (pp-deg-diff (cdr pp1) (cdr pp2) o)))))))

;;;
;;; DEG-REV-LEX<: Degree reverse lexicographic order.  This will be our
;;; default term ordering since it has such a nice leading term division
;;; property (see e.g. Ex. 1.4.9 of `An Introduction to Groebner Bases' by
;;; Adams and Loustaunau).
;;;
;;; Note that in canonical form, monomials are stored in reverse order:
;;;  (c . ((v_{n-1} . p_{n-1}) ... (v_0 . p_0))) s.t. 
;;;   v_0 > v_1 > ... > v_{n-1}.  
;;; So, we need not REVERSE the monomials to compute this ordering below.
;;;
;;; Hmm, I wonder if this motivates having monomials always carry around
;;; their degree explicitly, instead of it taking a linear pass through
;;; each to calculate it?
;;;

(defun deg-rev-lex< (m1 m2)
  (let ((deg1 (mdeg m1)) (deg2 (mdeg m2)) 
	(pp1 (mpp m1)) (pp2 (mpp m2)))
    (cond ((eq pp1 pp2) nil)
	  (t (or (< deg1 deg2)
		 (and (= deg1 deg2)
		      (let ((rev-diff (pp-deg-diff pp1 pp2 'DEG-REV-LEX)))
			(and (consp rev-diff) 
			     (> (car rev-diff) (cdr rev-diff))))))))))

;;;
;;; DEG-LEX: Degree lexicographic ordering.  We piggy-back off of the above machinery
;;; for DEG-REV-LEX.  Note the implicit power-product reversals we must do.
;;;

(defun deg-lex< (m1 m2)
  (let ((deg1 (mdeg m1)) (deg2 (mdeg m2)) 
	(pp1 (reverse (mpp m1))) (pp2 (reverse (mpp m2))))
    (cond ((eq pp1 pp2) nil)
	  (t (or (< deg1 deg2)
		 (and (= deg1 deg2)
		      (let ((rev-diff (pp-deg-diff pp1 pp2 'DEG-LEX)))
			(and (consp rev-diff) 
			     (< (car rev-diff) (cdr rev-diff))))))))))

;;; *** Set the default term ordering here. ***

(defparameter active-mo< nil)

(setq active-mo< #'deg-rev-lex<)

(defun set-active-term-ordering (quoted-name)
  (cond ((eq quoted-name 'DEG-REV-LEX)
	 (setq active-mo< #'deg-rev-lex<))
	((eq quoted-name 'DEG-LEX)
	 (setq active-mo< #'deg-lex<))
	(t (break "ERROR: Choices are: 'DEG-REV-LEX or 'DEG-LEX"))))

(defun mo< (m1 m2)
  (funcall active-mo< m1 m2))

(defun mo> (m1 m2)
  (mo< m2 m1))

(defun mo= (m1 m2)
  (and (not (mo< m1 m2))
       (not (mo< m2 m1))))

;;;
;;; MPRINT: Pretty print a monomial using the mapping in *vars-table*.
;;; 
;;; An example with standard *vars-table*: 
;;;
;;; (mprint '(5 . ((0 . 1) (1 . 2))))
;;; "5x y^2"
;;;

(defun mprint (m)
  (cond ((eq m nil) (break "(ERROR: EMPTY MONOMIAL?)"))
	(t (concatenate 'string 
			(if (not (= (mcoeff m) 1)) (write-to-string (mcoeff m)) 
			  (if (eq (mpp m) nil) "1" ""))
			(pp-print (reverse (mpp m))))))) ; For why we reverse, see above about DEG-REV-LEX.

(defun pp-print (pp)
  (cond ((eq pp nil) "")
	(t (let ((cur-vp (car pp)))
	     (let ((cur-vp-id (car cur-vp))
		   (cur-vp-pow (cdr cur-vp)))
	       (if (> cur-vp-pow 0)
		   (concatenate 'string
				(string (nth cur-vp-id *vars-table*))
				(if (> cur-vp-pow 1) 
				    (concatenate 'string "^" 
						 (write-to-string cur-vp-pow))
				  "")
				(if (cdr pp) " " "")
			    (pp-print (cdr pp))) 
	       (pp-print (cdr pp))))))))

;;;
;;; POLY-PRINT: Pretty print a polynomial using the mapping in *vars-table*.
;;;
;;; An example with standard *vars-table* and *poly-ex0* as below:
;;;
;;; (poly-print *poly-ex0*)
;;; "5x y^2  +  3x^2 d"
;;;

(defun poly-print (p)
  (cond ((eq p nil) "")
	(t (concatenate 'string
			(mprint (car p))
			(if (cdr p) "  +  " "")
			(poly-print (cdr p))))))
		 
;;;
;;; POLY+: Add two polynomials together, adjusting the coefficients of shared
;;; monomials accordingly.  Note that we assume the polynomials are already
;;; in canonical form (in descending order w.r.t. mo<).  We return their sum
;;; in canonical form.
;;;

(defun poly+ (p1 p2)
  (cond ((eq p1 nil) p2)
	((eq p2 nil) p1)
	(t (let ((cur-p1 (car p1)) (cur-p2 (car p2)))
	     (cond ((mo= cur-p1 cur-p2)
		    (if (= (- (mcoeff cur-p1)) (mcoeff cur-p2))
			(poly+ (cdr p1) (cdr p2))
		      (cons (cons (+ (mcoeff cur-p1) (mcoeff cur-p2)) (cdr cur-p1))
			    (poly+ (cdr p1) (cdr p2)))))
		   ((mo> cur-p1 cur-p2)
		    (cons cur-p1 (poly+ (cdr p1) p2)))
		   ((mo< cur-p1 cur-p2)
		    (cons cur-p2 (poly+ p1 (cdr p2)))))))))
			  
(defun poly- (p1 p2)
  (poly+ p1 (poly-negate p2)))

(defun poly-negate (p)
  (mapcar #'mnegate p))

;;;
;;; PP-ZSIMP: Given a power-product pp, return the result of simplifying p
;;; by removing every (v . 0) (e.g., 1).
;;;

(defun pp-zsimp (pp)
  (cond ((endp pp) nil)
	((= (cdar pp) 0) 
	 (pp-zsimp (cdr pp)))
	(t (cons (car pp)
		 (pp-zsimp (cdr pp))))))

;;;
;;; POLY-ZSIMP: Given a polynomial p, return the result of simplifying p by 
;;; clearing monomials with zero coefficients.
;;;

(defun poly-zsimp (p)
  (cond ((endp p) nil)
	(t (let ((cur-pp (cdar p))
		 (cur-c (caar p)))
	     (cond ((= cur-c 0) 
		    (poly-zsimp (cdr p)))
		   (t (cons `(,cur-c . ,(pp-zsimp cur-pp)) 
			    (poly-zsimp (cdr p)))))))))

;;;
;;; MPOLY*: Given a monomial m and a polynomial p, return m*p.
;;;

(defun mpoly* (m p)
  (cond ((eq p nil) nil)
	(t (poly+ `(,(mcons m (car p)))
		  (mpoly* m (cdr p))))))

;;; POLY-LP: Given a polynomial in canonical form, return its leading power-product.

(defun poly-lp (p)
  (cdar p))

;;; POLY-LC: Given a polynomial in canonical form, return its leading coefficient.

(defun poly-lc (p)
  (mcoeff (car p)))

;;; POLY-LT: Given a polynomial in canonical form, return its leading term.

(defun poly-lt (p)
  (car p))

;;; POLY-DEG: Given a polynomial in canonical form, return its degree.

(defun poly-deg (p)
  (mdeg (poly-lt p)))

;;;
;;; POLY-UNIV-/: Univariate polynomial division.
;;; Given two polynomials f, g in Q[x], we return a pair (q . r) s.t.
;;; f = gq + r.
;;;

(defun poly-univ-/ (f g)
  (cond ((or (eq g 0) (eq g nil)) (break "ERROR: POLYNOMIAL DIVISION BY ZERO."))
	((not (poly-univ-compatp f g))
	      (break "ERROR: POLY-UNIV-/ REQS. UNIV. POLYNOMIALS IN SAME INDETERMINATE."))
	((< (poly-deg f) (poly-deg g)) (cons 0 f))
	(t (poly-univ-/* (poly-zsimp g) nil (poly-zsimp f)))))

(defun poly-univ-/* (g q r)
  (cond ((or (eq r 0) (eq r nil) 
	     (< (poly-deg r) (poly-deg g))) (cons q r))
	(t (let ((ratio-of-lts (m/ (poly-lt r) (poly-lt g))))
	     (poly-univ-/* g 
			   (poly+ q `(,ratio-of-lts))
			   (poly- r (mpoly* ratio-of-lts g)))))))

;;; 
;;; POLY-UNIVP: Is p a univariate polynomial?
;;;

(defun poly-univp (p)
  (let ((first-var (caadar p)))
    (every #'(lambda (q) (let ((q1 (cdr q)))
			   (or (eq q1 nil)
			       (and (= (caar q1) first-var)
				    (<= (length q1) 1)))))
	   p)))


;;; 
;;; POLY-UNIV-COMPATP: Are f and g compatible univariate polynomials?
;;;  (e.g. both univariate polynomials in same indeterminate)
;;;

(defun poly-univ-compatp (f g)
  (and (poly-univp f) (poly-univp g)
       (or (not (and (consp (cdar f)) (consp (cdar g))))
		(= (caadar f) (caadar g)))))

;;;
;;; M/: Simple monomial division.
;;; Note that this requires that m2 does not share a variable v with m1 
;;; s.t. degree of v in m2 is greater than the degree of v in m1.
;;;
;;; Note: To aid a divisibility testing trick, we do not break when we
;;; see the "ERROR: UNCLEAN ..." string.  See MDIVIDESP for why.
;;;

(defun m/ (m1 m2)
  (cond ((eq m1 m2) `(1 . nil))
	((not (mvarsubsetp (mvars m2) (mvars m1)))
	 `(nil . ("ERROR: UNCLEAN MONOMIAL DIVISION.")))
	(t (let ((coeff-ratio (/ (mcoeff m1)
				 (mcoeff m2))))
	     (cons coeff-ratio
		   (m/-pp (mpp m1) (mpp m2)))))))

(defun m/-pp (pp1 pp2)
  (cond ((eq pp1 nil) pp2)
	((eq pp2 nil) pp1)
	(t (let ((vp1 (car pp1)) (vp2 (car pp2)))
	     (cond ((= (car vp1) (car vp2))
		    (cond ((= (cdr vp1) (cdr vp2))
			   (m/-pp (cdr pp1) (cdr pp2)))
			  ((> (cdr vp1) (cdr vp2))
			   (cons (cons (car vp1)
				       (- (cdr vp1) (cdr vp2)))
				 (m/-pp (cdr pp1) (cdr pp2))))
			  (t '("ERROR: UNCLEAN MONOMIAL DIVISION."))))
		   ((< (car vp1) (car vp2))
		    '("ERROR: UNCLEAN MONOMIAL DIVISION."))
		   ((> (car vp1) (car vp2))
		    (cons vp1 (m/-pp (cdr pp1) pp2))))))))

;;;
;;; MVARS: Given a monomial, return a list of all its variable symbols.
;;;

(defun mvars (m)
  (cond ((numberp (car m)) (mvars (cdr m)))
	((endp m) nil)
	(t (cons (caar m)
		 (mvars (cdr m))))))

;;;
;;; MVARSUBSETP: Given two lists of variables in canonical order, v1 and v2, is 
;;; v1 a subset of v2?
;;;

(defun mvarsubsetp (m1 m2)
  (cond ((endp m1) t)
	((endp m2) nil)
	((= (car m1) (car m2)) 
	 (mvarsubsetp (cdr m1) (cdr m2)))
	((> (car m1) (car m2)) nil)
	(t (mvarsubsetp m1 (cdr m2)))))
			
;;;
;;; MDIVIDESP: Given monomials m1 and m2, does m1 divide m2?
;;;

(defun mdividesp (m1 m2)
  (every #'(lambda (f) (or (consp f) (numberp f))) (m/ m2 m1)))

;;;
;;; POLY-UNIV-GCD: Given two univariate polynomials in same indeterminate,
;;; return their monic GCD.
;;;

(defun poly-univ-gcd (f g)
  (cond ((not (poly-univ-compatp f g))
	 (break "ERROR: UNIVARIATE GCD MUST TAKE PLACE BETWEEN POLYNOMIALS IN SAME INDETERMINATE."))
	((and (eq f nil)
	      (eq g nil)) (break "ERROR: GCD WITH TWO ZERO ARGS IS UNDEFINED."))
	(t (let ((raw-gcd (poly-univ-gcd* f g)))
	     (let ((normalizing-coeff (/ 1 (poly-lc raw-gcd))))
	       (mpoly* `(,normalizing-coeff . nil) raw-gcd)))))) ; Make GCD monic.

(defun poly-univ-gcd* (f g)
  (cond ((eq g nil) f)
	(t (let ((rem-fg (cdr (poly-univ-/ f g))))
	     (poly-univ-gcd* g rem-fg)))))

;;;
;;; IDEAL-UNIV-MAKE-PRINCIPAL: Given an ideal I over Q[x], reduce I to an equivalent
;;; principle ideal.  (Ideals are given as a list of their generators.)
;;;

(defun ideal-univ-make-principal (I)
  (cond ((= (length I) 1) I)
	(t (let ((p1 (car I)) (p2 (cadr I)))
	     (let ((gcd-p1-p2 (poly-univ-gcd p1 p2)))
	       (ideal-univ-make-principal
		(cons gcd-p1-p2 (cddr I))))))))

;;;
;;; IDEAL-UNIV-MEMBERP: Given a polynomial p and an ideal I, all univariate over same
;;; polynomial ring Q[x], is p a member of I?
;;;

(defun ideal-univ-memberp (p I)
  (let ((principal-generator (car (ideal-univ-make-principal I))))
    (eq (cdr (poly-univ-/ p principal-generator)) nil)))

;;;
;;; IDEAL-MEMBERP: Given a polynomial p and an ideal I, assumed multivariate over same
;;; polynomial ring Q[*vars-table*], is p a member of I?

(defun ideal-memberp (p I)
  (let ((G (gbasis I)))
    (eq (cdr (poly-multiv-/ p G)) nil)))

;;;
;;; IDEAL-REDUCED-GBASIS-MEMBERP: Given a polynomial p and an ideal I, assumed multi-
;;; variate over same polynomial ring Q[*vars-table*], is p a member of I?
;;;
;;; This is just as IDEAL-MEMBERP except we compute the unique Reduced Groebner Basis
;;; w.r.t. the current term ordering.  Of course, the predicates are extentionally
;;; equivalent, but doing a membership test with a reduced basis is often more
;;; desirable.
;;;

(defun ideal-reduced-gbasis-memberp (p I)
 (let ((G (reduce-gbasis (gbasis I))))
    (eq (cdr (poly-multiv-/ p G)) nil)))

;;;
;;; POLY-MULTIV-/: Given a polynomial f and a sequence of polynomials P = p_1, ..., p_n, 
;;; all in Q[*vars-table*], return a sequence of quotients u_1, ..., u_n together 
;;; with a remainder r in Q[*vars-table*] s.t. 
;;;
;;;        f = u_1*p_1 + ... + u_n*p_n + r.
;;;

(defun poly-multiv-/ (f P)
  (poly-multiv-/* f P nil nil))

(defun poly-multiv-/* (f P Q r)
  (cond ((eq f nil) (cons Q r))
	(t (let ((divisor-idx (poly-find-divisor f P 0)))
	     (cond ((not divisor-idx)
		    (let ((lt-f-as-poly `(,(poly-lt f))))
		      (poly-multiv-/* 
		       (poly- f lt-f-as-poly)
		       P Q 
		       (poly+ r lt-f-as-poly))))
		   (t (let ((adj-ui (cdr (assoc divisor-idx Q)))
			    (div-pi (nth divisor-idx P)))
			(let ((lt-f/lt-pi (m/ (poly-lt f) (poly-lt div-pi))))
			  (let ((new-ui (poly+ adj-ui `(,lt-f/lt-pi)))
				(new-f (poly- f (mpoly* lt-f/lt-pi div-pi))))
			    (poly-multiv-/* new-f P 
					    (aput Q divisor-idx new-ui)
					    r))))))))))

;;;
;;; APUT: A simple `put' operation for association lists.
;;; Given an association list (alst), a key (k), and a value (v), we
;;; replace the first occurence of (k . v') in alst with (k . v), adding
;;; (k . v) to the end of alst of no such (k . v') exists.
;;;

(defun aput (alst k v)
  (cond ((endp alst) (cons (cons k v) nil))
	(t (let ((cur-alst (car alst)))
	     (let ((cur-key (car cur-alst)))
	       (cond ((eq cur-key k) 
		      (cons (cons k v) (cdr alst)))
		     (t (cons cur-alst
			      (aput (cdr alst) k v)))))))))

;;;
;;; POLY-MAKE-MONIC: Given a polynomial p, return p/lc(p).
;;;

(defun poly-make-monic (p)
  (let ((c (poly-lc p)))
    (fmt 10 ">> POLY-MAKE-MONIC: p = ~A; c = ~A." (poly-print p) c)
    (if (not (or (eql c nil) (= c 0)))
	(mpoly* `(,(/ 1 c) . nil) p)
      p)))

;;;
;;; Given a polynomial f and a sequence of polynomials P = p_1, ..., p_n,
;;; return the least i s.t. 
;;;
;;;   lp(p_i) divides lp(f).
;;;

(defun poly-find-divisor (f P i)
  (cond ((endp P) nil)
	(t (let ((lt-f (poly-lt f))
		 (lt-p (poly-lt (car P))))
	     (cond ((mdividesp lt-p lt-f) i)
		   (t (poly-find-divisor f (cdr P) (1+ i))))))))

;;; 
;;; Given two power-products in canonical form, pp1 and pp2, compute their 
;;; least common multiple (returning a canonical power-product).
;;; 

(defun pp-lcm (pp1 pp2)
  (cond ((eq pp1 nil) pp2)
	((eq pp2 nil) pp1)
	(t (let ((cur-v1 (caar pp1)) (cur-v2 (caar pp2))
		 (cur-p1 (cdar pp1)) (cur-p2 (cdar pp2)))
	     (cond ((> cur-v1 cur-v2) 
		    (cons (car pp1) (pp-lcm (cdr pp1) pp2)))
		   ((< cur-v1 cur-v2)
		    (cons (car pp2) (pp-lcm pp1 (cdr pp2))))
		   (t (cons (cons cur-v1 (max cur-p1 cur-p2))
			    (pp-lcm (cdr pp1) (cdr pp2)))))))))

;;;
;;; S-POLY: Given two polynomials, f and g in Q[*vars-table*], return
;;; the S-polynomial of f and g.
;;;

(defun s-poly (f g)
  (let ((L `(1 . ,(pp-lcm (poly-lp f) (poly-lp g)))))
    (let ((f-ratio (m/ L (poly-lt f)))
	  (g-ratio (m/ L (poly-lt g))))
      (poly- (mpoly* f-ratio f)
	     (mpoly* g-ratio g)))))

;;;
;;; S-PAIRS: Given a sequence of polynomials, return a sequence of all nontrivial
;;; S-polynomials obtained from them.  This omits S-Poly(f_i, f_i) and 
;;; S-Poly(f_i, f_j) when S-Poly(f_j, f_i) is already present.
;;;

(defun s-pairs (F)
  (s-pairs* F 0 1 (1- (length F))))

(defun s-pairs* (F i j ub)
  (cond ((= i ub) nil)
	((= i j) (s-pairs* F i (1+ j) ub))
	(t (cons (cons (cons i j) (s-poly (nth i F) (nth j F)))
		 (s-pairs* F
			   (if (= j ub) (1+ i) i)
			   (if (= j ub) (+ i 2) (1+ j)) ub)))))

;;;
;;; GBASIS: Given an (possibly multivariate) ideal I over Q[*vars-table*], 
;;; compute an initial Groebner basis for I using MO< (ACTIVE-MO<) as the 
;;; term ordering.  
;;;
;;; Note: It may be advisable to *always* compute an initial Groebner basis
;;; utilizing DEG-REV-LEX, due to its efficiency, and then to use a change
;;; of basis algorithm if ACTIVE-MO< is not #'DEG-REV-LEX.  I need to
;;; think more about this.
;;;

(defun gbasis (I)
  (let ((I* (remove-duplicates I)))
    (gbasis* I* (s-pairs I*))))

(defun gbasis* (G s-polys)
  (cond ((endp s-polys) G)
	(t (let ((cur-s-poly (cdar s-polys)))
	     (let ((s-reduct-via-G (cdr (poly-multiv-/ cur-s-poly G))))
	       (cond ((eq s-reduct-via-G nil)
		      (gbasis* G (cdr s-polys)))
		     (t (gbasis* (cons s-reduct-via-G G)
				 (append (mapcar 
					  #'(lambda (f) 
					      (cons 'gb (s-poly f s-reduct-via-G))) G)
					 s-polys)))))))))

;;;
;;; MINIMIZE-GBASIS: Given a Groebner basis G for an ideal I, computed w.r.t. the
;;; active term ordering MO<, compute *a* minimal Groebner basis of I.
;;;

(defun minimize-gbasis (G)
  (mapcar #'poly-make-monic (minimize-gbasis* G nil)))

(defun minimize-gbasis* (G G*)
  (cond ((endp G) G*)
	(t (let ((g_i (car G)))
	     (let ((g_i-lt (poly-lt g_i)))
	       (cond ((some #'(lambda (g) (mdividesp (poly-lt g) g_i-lt)) 
			    (append (cdr G) G*))
		      (minimize-gbasis* (cdr G) G*))
		     (t (minimize-gbasis* (cdr G) (cons g_i G*)))))))))

;;;
;;; REDUCE-GBASIS: Given a Groebner basis G for an ideal I, computed w.r.t. the 
;;; active term ordering MO<, compute the unique reduced Groebner basis of I
;;; from G w.r.t. MO<.
;;;

(defun reduce-gbasis (G)
  (reduce-gbasis* (minimize-gbasis G) nil))

(defun reduce-gbasis* (G H)
  (cond ((endp G) H)
	(t (let ((g_i (car G)))
	     (let ((g_i-G-reduct (cdr (poly-multiv-/ g_i (append H (cdr G))))))
	       (reduce-gbasis* (cdr G) 
			       (if g_i-G-reduct (cons g_i-G-reduct H) H)))))))

;;;
;;; Some trivial positivity detection:
;;;
;;; MSQUAREP: Is a monomial a square?
;;;  We detect this by seeing if every exponent in the monomial is even and
;;;  the leading coefficient is non-negative.
;;;

(defun msquarep (m)
  (and (>= (mcoeff m) 0)
       (every #'(lambda (pp) (evenp (cdr pp))) (mpp m))))

;;; POLY-TRIVIAL-SQUARE (p)
;;; Is a polynomial a trivial square (e.g. are each of its monomials squares)?

(defun poly-trivial-square (p) 
  (every #'msquarep p))

;;;
;;; POLY-MULT (p q): Given polynomials p, q in canonical form, return
;;;  their product, p*q, in canonical form.
;;;

(defun poly-mult (p q)
  (let ((running-prod nil))
    (dolist (m1 p)
      (dolist (m2 q)
	(setq running-prod
	      (poly+ running-prod
		     `(,(mcons m1 m2))))))
    running-prod))
    
;;;
;;; POLY-DEG-ZERO-SCALAR (p): Given a polynomial, compute the rational value of its
;;; degree zero scalar term.
;;;

(defun poly-deg-zero-scalar (p)
  (let ((scalar 0)) 
    (dolist (m p) (if (= (mdeg m) 0) 
		      (setq scalar (+ scalar (mcoeff m))) t)) 
    scalar))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Some examples:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#| 
(defparameter *f1* `((1 . ((0 . 3))) (-3 . ((0 . 1))) (2 . nil)))
(defparameter *f2* `((1 . ((0 . 2))) (-1 . nil)))
(poly-univ-gcd *f1* *f2*) ; yields ((1 (0 . 1)) (-1)) = "x + -1" as it should!

(defparameter *m1* '(2 . ((3 . 5) (0 . 1))))
(defparameter *m2* '(3 . ((4 . 1) (0 . 2))))
(defparameter *m4* '(1 . ((2 . 1) (1 . 1) (0 . 2))))
(defparameter *m5* '(1 . ((1 . 3) (0 . 1))))
(defparameter *p1* `(,*m1* ,*m2*))

(defparameter *poly-ex0* 
  '((5 . ((0 . 1) (1 . 2)))
    (3 . ((0 . 2) (9 . 1)))))

;;; Another example:

(defparameter *p2*
  (poly+ (poly+ '((2 . ((2 . 1) (1 . 1) (0 . 2)))) '((3 . ((1 . 3) (0 . 1)))))
	 '((-2 . ((0 . 3))))))

(poly-print *p2*)
; yields: "3x y^3  +  2x^2 y z  +  -2x^3" as it should!

(defparameter *f*
  '((1 . ((0 . 3))) (-2 . ((0 . 2))) (2 . ((0 . 1))) (8 . nil)))

(defparameter *g*
  '((2 . ((0 . 2))) (3 . ((0 . 1))) (1 . nil)))


(ideal-univ-memberp (mpoly* '(5 . nil) *f2*) (list *f1* *f2*))

;;; yields: t as it should!

(poly-print (car (ideal-univ-make-principal (list (mpoly* '(5 . nil) *f2*) *f1* *f2*))))

;;; yields: "x  +  -1" as it should!


;;;
;;; An LCM example for power-products (remember, power-products are internally reversed
;;; w.r.t. the active variable ordering.  They are put in the correct order when monomials
;;; are printed, but not when power-products are printed (though perhaps counter-intuitive,
;;; internally a power-product is not a monomial unless it is extended with a leading 
;;; coefficient of 1)).  This is for efficiency of default DEG-REV-LEX ordering.
;;;

(pp-print (pp-lcm (mpp *m1*) (mpp *m2*)))
;;; yields: "v u^5 x^2", where
(pp-print (mpp *m1*))
;;; yields: "u^5 x", and
(pp-print (mpp *m2*))
;;; yields: "v x^2".


;;;
;;; A multivariate division example:
;;;

(defparameter *f1* '(  (2 . ((0 . 1))) (1 . ((1 . 1))) (1 . nil) ))
(defparameter *f*  '(  (1 . ((1 . 1) (0 . 2))) (4 . ((1 . 1) (0 . 1))) (-3 . ((1 . 2)))))
(set-active-term-ordering 'DEG-LEX)

(poly-multiv-/ *f* `(,*f1*))
; yields: (((0 (1/2 (1 . 1) (0 . 1)) (-1/4 (1 . 2)) (7/4 (1 . 1)))) (1/4 (1 . 3))
;              (-9/2 (1 . 2)) (-7/4 (1 . 1)))

(poly-print (cdr (assoc 0 (car (poly-multiv-/ *f* `(,*f1*))))))
; yields: "1/2x y  +  -1/4y^2  +  7/4y" as it should!



;;;
;;; A second multivariate division example:
;;;

(defparameter *f1* '((1 . ((1 . 1) (0 . 1))) (-1 . ((0 . 1)))))
(defparameter *f2* '((1 . ((0 . 2))) (-1 . ((1 . 1)))))
(defparameter *f*  '((1 . ((1 . 1) (0 . 2)))))

(poly-print (cdr (assoc 0 (car (poly-multiv-/ *f* `(,*f1* ,*f2*))))))
; yields: "x"

(poly-print (cdr (assoc 1 (car (poly-multiv-/ *f* `(,*f1* ,*f2*))))))
; yields: "1"

(poly-print (cdr (poly-multiv-/ *f* `(,*f1* ,*f2*))))
; yields: "y"

; So, f = x*f1 + f2 + y, as it should!


;;; Some Groebner examples:

(ideal-memberp *f* `(,*f1* ,*f2* ,*f*))
; yields: t
(ideal-memberp (poly+ *f* '((1 . nil))) `(,*f1* ,*f2* ,*f*))
; yields: nil
(ideal-memberp '((1 . nil)) '( ((3 . nil)) ))
; yields: t


(defparameter *f1* '(  (2 . ((0 . 1))) (1 . ((1 . 1))) (1 . nil) ))

(mapcar #'poly-print (reduce-gbasis (gbasis `(,*f1* ,*f2*))))
; yields: ("y^2  +  -2y  +  1" "x  +  1/2y  +  1/2") as it should!

; where 
(mapcar #'poly-print `(,*f1* ,*f2*))
; yields: ("2x  +  y  +  1" "x^2  +  -1y")




(mapcar #'poly-print (reduce-gbasis (gbasis `(,*f1* ,*f*))))
; yields: ("y^3  +  -18y^2  +  -7y" "x  +  1/2y  +  1/2") as it should!

; where
(mapcar #'poly-print `(,*f1* ,*f*))
; yields: ("2x  +  y  +  1" "x^2 y  +  4x y  +  -3y^2")
|#