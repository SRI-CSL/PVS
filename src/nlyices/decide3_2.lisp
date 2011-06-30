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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gb)		;; This module is named GB: GB based dp for reals
(cl:defpackage "gb"
  (:nicknames "GB")
  (:export "saturate" "sos" "set-debug-level" "set-newU" "set-newV" "get-newU" "get-newV" 
	   "set-degree-ratio" "set-length-ratio")
  (:use "polynomial-representation-core" "cl" "user" "clos"))

(in-package "gb")	;; The core procedure


;; Polynomial and witness and side condition c
;; 
;; each element of the Grobner basis is stored as a structure (POL p wit c)
;; - p = the polynomial (i.e., p == 0) is a constraint
;; - wit = list of polynomials (q_1 ... q_t) from which (p == 0) was derived
;;   (i.e., p == 0 is implied by q_1 == 0 and ... and q_t == 0
;; - c can be '= or '>= or '> or ??
(defstruct POL pol wit c)


(defvar *newU* nil)	;; new vars U > 0
(defvar *newV* nil)	;; new vars V >= 0

;; (defvar *dlevel* nil)		;; Default t. Set and Get using accessors
(defvar *dlevel* 10)

;; ---------------------------------------------------------------------------
;; Introducing new PARAMETERS to bound the GB computation
;; ---------------------------------------------------------------------------
;; External
(defvar *degreeratio* 2)	;; generated-poly-degree/input-poly-degree
(defvar *lengthratio* 5)	;; generated-poly-length/input-poly-length
;; Internal
(defvar *degreebound* 1)	;; degree of polynomial
(defvar *lengthbound* 1)	;; number of monomials in any polynomial
;; external functions to set parameters
(defun set-degree-ratio (n) (setf *degreeratio* n))
(defun set-length-ratio (n) (setf *lengthratio* n))
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Print function
;; ---------------------------------------------------------------------------
(defun pp (E &key (key #'(lambda(x) x)))
  (loop for i in E collect (format nil "~a,  " (prep:polyrepPrint (funcall key i)))))

(defmacro print-debug (level &rest others)
  `(if (and *dlevel* (> ,level *dlevel*))
       (funcall #'format ,@others)))

(defun set-debug-level (n)
  (setf *dlevel* n))

(defmacro test-and-return (funname R st)
  `(if (eq ,st 'inconsistent) (return-from ,funname (values nil ,R))
       (multiple-value-bind (newR wit) (consistencyCheck ,R)
         (if (eq newR 'inconsistent) (return-from ,funname (values nil wit)))
         newR)))

(defun set-newU (u) (setf *newU* u))
(defun set-newV (u) (setf *newV* u))
(defun get-newU () *newU*)
(defun get-newV () *newV*)
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; Consistency Checks. Input could be inconsistent (chainPartialAllWith)
;; ----------------------------------------------------------------------------

;; Check whether an element of plist can't be zero (using simple means)
;; and remove all zero polynomials.
;; - return the non-zero elements of plist if no conflict is found
;; - return ('inconsistent witness) if there's a p in plist that can't be zero
(defun consistencyCheck (plist)
  (loop for i in plist 
	for j = (formula-status i)
	if (eq j 'satisfiable) collect i
	if (eq j 'inconsistent) return (values j (POL-wit i))))

;; Status of the equalition  p == 0
;; Return value:
;; 'valid        --> p is 0
;; 'inconsistent --> p can't be zero (as determined by sos-test or 
;;                   because p is a non-zero constant)
;; 'satisfiable  --> all other cases
(defun formula-status (p)
  (let ((poly (POL-pol p)))
  (cond ((null poly) 'valid)
	((prep:polyrepConstant? poly)
	 (cond ((eq (caar poly) 0) 'valid)
	       (t 'inconsistent)))
	((null (sos-test (list p))) 'inconsistent)
	(t 'satisfiable))))
;; ----------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
(defun saturate (E R &optional (S nil) (ERS nil) (Eold nil))
  (cond ((not (or E R S ERS)) (values t Eold))
	((and (or E R S) (null ERS)) (sos E R S Eold))
	((or E R S)
	 (multiple-value-bind (st Enew) (sos E R S Eold)
	   (if st (saturate nil nil nil ERS Enew) (values st Enew))))
	(t
      	 (let ((p (car ERS)) (op (cadr ERS)))
	   (multiple-value-bind (st Enew)
	     (case op (= (sos (list p) nil nil Eold))
		      (> (sos nil (list p) nil Eold))
		      (>= (sos nil nil (list p) Eold))
		      (t (format t "Unidentified operator~A~%" op) (break)))
	     (if st (saturate nil nil nil (cddr ERS) Enew) (values st Enew)))))))
;; ---------------------------------------------------------------------------


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
  (print-debug 5 t "Input equations E:~%~a~% " (pp E))
  (print-debug 5 t "Input inequalities R: ~%~a~% " (pp R))
  (print-debug 5 t "Input inequalities S: ~%~a~% " (pp S))
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
      (multiple-value-bind (st EE) (sos-att newE nil nil Eold 0 nil)
	(if st 
	    (progn (setf *newU* tempU) 
		   (setf *newV* tempV) 
		   (prep:set-variables tempX) 
		   (prep:set-parameters tempC)
		   (print-debug 15 t "SATISFIABLE~%~a~%" (pp EE :key #'POL-pol))
		   (values t EE))
	    (progn (setf *newU* origU) 
		   (setf *newV* origV) 
		   (prep:set-variables origX) 
		   (prep:set-parameters origC)
		   (print-debug 17 t "UNSATISFIABLE WITNESS IS ~%~a~%" (pp (get-witness EE) :key #'POL-pol))
		   (values st EE)))))))

;; E, R, S, Eold: 'POL list; K: int;
(defun sos-att (E R S Eold K Eret)
  (format t "SOS-ATT with K = ~a~%" K)
  (format t "Order: ~%~a~%" (prep:get-order))
  (print-debug 5 t "polynomials in E:~%~a~%" (pp E :key #'POL-pol))
  (print-debug 5 t "polynomials in R:~%~a~%" (pp R :key #'POL-pol))
  (print-debug 5 t "polynomials in S:~%~a~%" (pp S :key #'POL-pol))
  (print-debug 5 t "polynomials in Eold:~%~a~%~%" (pp Eold :key #'POL-pol))  
  (if (> K 13) (return-from sos-att (values t Eret)))
  (multiple-value-bind (st gb1 gb2) 
	(grobner (if (eq K 0) E (add-extension-vars E R S)) Eold)
    (if (null st) (return-from sos-att (values st gb1)))
    (if st (print-debug 5 t "printing new GB:~%~a~%~a~%" 
		(pp gb1 :key #'POL-pol) (pp gb2 :key #'POL-pol)))
    (multiple-value-bind (st ngb) (sos-test gb1)
      (if (null st) (values st ngb)
	  (let ((gb12 (append gb1 gb2)))
	    (sos-test2 gb12 K (if (eq K 0) gb12 Eret)))))))


;; Convert p to a (POL p nil symbol) where symbol
;; indicates where p comes from 
;; E --> equalities '=
;; R --> strict positive '>
;; S --> nonnegative '>=
(defun make-pole (p) (make-pol p nil '=))
(defun make-polr (p) (make-pol p nil '> ))
(defun make-pols (p) (make-pol p nil '>=))
(defun make-pol (p wit c)
  (make-POL :pol p :wit wit :c c))		;; Constraint is OP for R,S

;; Initialize/Set the parameters, if necessary
;; - set the limits *degreebound* and *lengthbound*
(defun maxdegree (E)
  (if (null E) 0 (apply #'max (mapcar #'prep:polyrepTotalDegreePoly E))))

(defun maxlength (E)
  (if (null E) 0 (apply #'max (mapcar #'length E))))

(defun init-params (E R S)
  (declare (special *degreebound* *degreeratio* *lengthbound* *lengthratio*))
  (let ((d (max (maxdegree E) (maxdegree R) (maxdegree S)))
	(n (max (maxlength E) (maxlength R) (maxlength S))))
    (setf *degreebound* (max (* *degreeratio* d) *degreebound*))
    (setf *lengthbound* (max (* *lengthratio* n) *lengthbound*))))

(defun bound-check? (p)
  (declare (special *degreebound* *lengthbound*))
  (and (<= (prep:polyrepTotalDegreePoly p) *degreebound*)
       (<= (length p) *lengthbound*)))
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Introducing extension/slack variables.
;; E,R,S are sorted s.t. x > 0 occurs first. This way we don't incur additional
;; overhead when we introduce new var for x > 0 cases (no reordering reqd too)
;; return-value, E, R, S: 'POL list
;; ---------------------------------------------------------------------------
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
	(prep:less t) (t nil)))

(defun get-new-var (op)
  (let* ((newu (intern (gensym))))
    (if (eq op '>) 
	(setf *newU* (cons newu *newU*))
	(setf *newV* (cons newu *newV*)))
    (prep:set-parameters (append (prep:get-parameters) (list newu)))
    (print-debug 4 t "new order is ~a~%" (prep:get-order))
    newu))

;; P, ret: POL; copy witness from P to ret.
(defun addU (P op)
  (let ((newu (get-new-var op))
	(pol (POL-pol P)) 
	(wit (if (eq (POL-wit P) -1) nil (list P))))
    (make-pol (prep:polyrepAddPoly pol (list (list -1 (cons newu 1)))) wit nil)))
;; ---------------------------------------------------------------------------



;; ---------------------------------------------------------------------------
;; Detection Phase
;; ---------------------------------------------------------------------------

;; Check whether an element of gb is non-zero (using simple techniques)
;; return (nil witness) if there's a non-zero polynomial in gb
;; return (t gb) otherwise
(defun sos-test (gb)
  (let ((p (some #'pn1? gb)))
    (if p (print-debug 6 t "Poly ~a found to be inconsistent." 
		(prep:polyrepPrint (POL-pol p))))
    (if p (values nil (POL-wit p)) 
	(values t gb))))

;; return value of 0 should be treated differently.
;; when pn-poly returns 0, then we only know that each monomial of p = 0
;; BUT currently, we will just say that p is inconsistent!!!
(defun pn1? (p)
  (if (eq (pn-poly? (POL-pol p)) 1) p))


;; Check whether p is positive or negative
;; - return 1 if all monomials of p are >= 0 and at least one is strictly positive
;;            or all monomials of p are <= 0 and at least one is striclty negative
;; - return 0 if all monomials of p are >= 0 or all monomials of p are <= 0
;; - return nil otherwise
(defun pn-poly? (p)
  (cond ((every #'(lambda(x) (> (car x) 0)) p)
         (p-poly?* p))
	((every #'(lambda(x) (< (car x) 0)) p)
         (p-poly?* p))
	(t nil)))

;; ans = 1 if all mu's in p are >= 0 and one mu > 0
;; ans = 0 if all mu's in p are p >= 0
;; ans = nil otherwise
(defun p-poly?* (p &optional (ans 0))
  (if (null p) (return-from p-poly?* ans))
  (let ((sgn (p-mono? (cdar p))))
    (cond ((eq sgn -1) nil)
	  ((eq sgn 1) (p-poly?* (cdr p) 1))
	  (t (p-poly?* (cdr p) ans)))))

;; ans = 1 if mu > 0  (i.e., all variables of mu are in *newU*)
;; ans = 0 if mu >= 0 
;; ans = -1 otherwise
(defun p-mono? (mu &optional (ans 1))
  (cond ((null mu) ans)
        ((var-in-list? (caar mu) *newU*) (p-mono? (cdr mu) ans))
        ((var-in-list? (caar mu) *newV*) (p-mono? (cdr mu) 0))
	((evenp (cdar mu)) (p-mono? (cdr mu) 0))
        (t -1)))
;; ---------------------------------------------------------------------------



;; ---------------------------------------------------------------------------
;; The second phase: nontrivial detection algorithm.
;; Algo: First we remove all equations whose leading PP is an ORIG variable.
;; ---------------------------------------------------------------------------

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
(defun sos-test2 (gb K Eret)
  (let ((rgb (loop for i in gb if (not (useless? (POL-pol i))) collect i)))
    (print-debug 5 t "Second phase: testing unsatisfiability of:~%~a~%" (pp rgb :key #'POL-pol))
    (if (<= (length rgb) 1) (values t gb)
        (let* ((rgb (sort rgb #'lesser?))
	       (rgb (if (eq K 0) (homogenize rgb) rgb)))
    	  (multiple-value-bind (st wit) (sos-test2* rgb K Eret)
	  	(if st (values t gb) (values nil wit)))))))


;; Heuristic: p is not useful 
;; - if its leading term t0 is an original variable
;; - if p is of the form (a0 t0 + a1 t1)
;; - if p's leading term contains an original variable
(defun useless? (p)
  (or (is-orig-pp? (cdar p)) (binomial? p)	;; COMPLETE
      (not-positive-pp? (cdar p))))		;; INCOMPLET

;; check also for order of first two monomials?
(defun binomial? (p)
  (or (null (cddr p)) 	;; binomial
      (and (null (cddar p)) (eq (cdadar p) 1) 
	   (or (and (> (caar p) 0) (every #'(lambda(x) (< (car x) 0)) (cdr p)))
      	       (and (< (caar p) 0) (every #'(lambda(x) (> (car x) 0)) (cdr p)))))))


;; check whether term mu is equal to a single original variable
;; mu is (x_1^d_1 ... x_n^d_n) represented as ((x_1.d_1) ... (x_n.d_n))
;; return t if mu ((y.1)) where y is an original variable
(defun is-orig-pp? (mu)
  (and (null (cdr mu)) (eq (cdar mu) 1) (is-orig-var? (caar mu))))

(defun is-orig-var? (v)
  (not (var-in-list? v (append *newV* *newU*))))

(defun var-in-list? (var vlist)
  (member var vlist :test #'prep:var-equal?))


;; check whether mu contains an unconstrained (i.e., original variable)
(defun not-positive-pp? (mu)
  (some #'(lambda(x) (is-orig-var? (car x))) mu))



;; homogenize all polynomials in gb
;; - introduce a new positive variable u for this
(defun homogenize (gb)
  (let ((newu (get-new-var '>)))
    (loop for i in gb collect (homogenizePoly i newu))))

;; BD: Bug here! the monomial ordering is not preserved.
;; example if x and y are parameters with x < y
;; then y^2 is before x in the monomial ordering (cf. polyrepComparePP)
;; but if we multiply by a fresh parameter u to homgenize them,
;; y^2 should be after x u in the monomial ordering.
(defun homogenizePoly (p u)
  (let ((N (prep:polyrepTotalDegreePoly (POL-pol p))))
    (make-POL
     :pol (reorder (loop for i in (POL-pol p) collect
		      (let ((n (prep:polyrepTotalDegreePoly (list i))))
			(if (> N n) 
			    (append i (list (cons u (- N n)))) 
			  i))))
     :wit (POL-wit p) :c (POL-c p))))


;; Attempt to fix the bug
(defun mono-greater? (x y) 
  (case (prep:polyrepComparePP (cdr x) (cdr y))
    (prep:greater t)
    (t nil)))

(defun reorder (l) (sort l #'mono-greater?))


;; rename leading PP of "p" to a new var to make it smaller
;; and recurse
(defun sos-test2* (gb K Eret)
  (format t "SOS-TEST2 with K = ~a~%" K)
  (if (or (> K 13) (<= (length gb) 1)) (return-from sos-test2* (values t Eret)))
  (let* ((gb1 (cycle gb K))
	 (p (some #'almost-positive? gb1))
	 (p (if p p (some #'(lambda(x) (almost-positive? x 2)) gb1)))
	 (p (if p p (maxOfList gb #'> :key #'(lambda(x)(length (POL-pol x))) :ret #'POL-pol)))
	 (pol (make-pol (list (cons 1 (cdar p))) -1 nil))) ;; NOTE: Ignoring U,V distinction
    (print-debug 20 t "Main Poly is:~%~a~%" (pp (list p)))
    (sos-att nil (list pol) nil gb (+ K 1) Eret)))	;; NOTE: Ignoring U,V distinction/witness

;; rotate list l by n
(defun cycle (l n)
  (let ((N (length l)))
    (loop for i from 0 upto (- N 1) collect (nth (mod (+ n i) N) l))))

;; atmost N positive (or negative) monomials in p
;; INPUT: POL, OUTPUT: polyrep poly
(defun almost-positive? (P &optional (N 1) (n 0) (m 0))
  (let ((p (POL-pol P)))
  ;; if the leading term is nonlinear, return nil
  (if (or (cddar p) (> (cdadar p) 1)) (return-from almost-positive? nil))
  (loop for i in p do (if (> (car i) 0) (setf n (+ n 1))) (if (> n N) (return nil)))
  (if (<= n N) p
      (progn
        (loop for i in p do (if (< (car i) 0) (setf m (+ m 1))) (if (> m N) (return nil)))
	(if (<= m N) p nil)))))


;; ---------------------------------------------------------------------------
  
;; ---------------------------------------------------------------------------
;; Grobner Basis Computation
;; ---------------------------------------------------------------------------

;; Compute a Grobner basis of (union E Eold)
;; - E = list of polynomials as (POL ..) structures
;; - Eold = list of polynomials as (POL ..)
;; Eold must be a Grobner basis
;; 
;; If an inconsistency is detected (i.e., a polynomial added to the basis
;; can't be zero) then the returned value is (nil witness).
;;
;; Otherwise, the returned value is (t new old) such that
;; - new + old is the Grobner basis of (union E Eold)
;; - old = elements of Eold that remain in the basis
;; - new = polynomials in the basis that were not in Eold
;;
(defun grobner (E &optional (Eold nil))
  (print-debug 15 t "Order for constructing GB is ~a~%" (prep:get-order))
  (print-debug 15 t "Constructing GB for:~%~a~%~%" (pp (append E Eold) :key #'POL-pol))
  (multiple-value-bind (st Enew Eold) (gb-full E Eold)
    (if st (print-debug 5 t "Eqns sat. GB = ~%~a~%" (pp (append Enew Eold) :key #'POL-pol)))
    (if st 
	(values t (remove-if #'null Enew :key #'POL-pol) Eold)
	(values nil Enew))))
;; ----------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; gb1-normalize( unproc_list, gb) --> GB(gb+unproc_list)
;; Algo: (i) pick first p from unproc_list; normalize it by gb 
;;      (ii) collapse gb by p: (newgb, oldgb) <-- gb2-collapse( p, gb )
;;     (iii) superpose oldgb with p: unproc_new <-- gb3-superpose( p, gbold)
;;      (iv) gb1-full( unproc_new+newgb, gbold+p )
;; ----------------------------------------------------------------------------

(defun gb-full (unproc_list &optional (gbold nil) (gbnew nil))
  (cond ((null unproc_list) (values t gbnew gbold))
	((and (null gbold) (null gbnew))
	 (gb-full (cdr unproc_list) nil (list (car unproc_list))))
	(t
	 (let ((plist (list (car unproc_list))))
	 (multiple-value-bind (st newE oldE)	;; collapse new eqn
		(dynamicSimplifyRbyErec (append gbold gbnew) plist)
	   (print-debug 2 t "Simplified new equation ~a by GB ~a~%" 
			 (pp plist :key #'POL-pol) (pp (append gbold gbnew) :key #'POL-pol))
	   (print-debug 2 t "newE is ~a~%" (pp newE :key #'POL-pol))
	   (print-debug 2 t "oldE is ~a~%~%" (pp oldE :key #'POL-pol))
	   ;; 1) simplify first polynomial p of unproc_list using old + new
	   ;;    st is 'satisfiable
	   ;;    if p is reduced then newE = (reduced form of p), oldE is nil
	   ;;    if p isn't reduced then newE = nil, oldE = (p)
	   (let* ((newE  (test-and-return gb-full newE st))
		  (plist (nconc newE oldE)))
	     (if (car plist)
		 ;; if p is not empty
	         (multiple-value-bind (st newGBOld oldGBOld) (dynamicSimplifyRbyErec plist gbold)
		   ;; 2) simplify gbold using the new equation p=0
		   ;;    newGBOld = elements of gbold that are reduced by p=0
		   ;;    oldGBOld = elements of gbold that are unchanged
		   (show-debug "old" plist gbold newGBOld oldGBOld)
		   (let ((newGBOld (test-and-return gb-full newGBOld st)))
	             (multiple-value-bind (st newGBNew oldGBNew) (dynamicSimplifyRbyErec plist gbnew)
			;; 3) simplify gbnew using the equation p=0
			;;    newGBNew = elements of gbnew that are reduced by p=0
			;;    oldGBNew = elements of gbnew that are unchanged
			(show-debug "new" plist gbnew newGBNew oldGBNew)
		   	(let ((newGBNew (test-and-return gb-full newGBNew st))
			      (newCPs (allPairCPs (car plist) (append oldGBOld oldGBNew))))
			  (print-debug 4 t "No of new CPS = ~a~%" (length newCPs))
			  (print-debug 4 t "~a~%" (pp newCPs :key #'POL-pol))
			  (gb-full (nconc newCPs newGBOld newGBNew (cdr unproc_list)) 
					    oldGBOld (nconc plist oldGBNew))))))

	       ;; if p is reduced to 0
	       (gb-full (cdr unproc_list) gbold gbnew))))))))

;; Compute a list of S-polynomials (p q) for q in qlist
;; - use optimizations to filter out some (redundant) S-polynomials 
(defun allPairCPs (p qlist)
  (delete nil (mapcar #'(lambda(x) (superposition p x)) qlist)))

(defun show-debug (which plist a b c)
	   	       (print-debug 2 t "-------~%")
	   	       (print-debug 2 t "Simplified ~a GB by new equation~a~%" which
			 (pp plist :key #'POL-pol))
	   	       (print-debug 2 t "Old GB:~a~%" (pp a :key #'POL-pol)) 
	   	       (print-debug 2 t "Changed:~a~%" (pp b :key #'POL-pol))
	   	       (print-debug 2 t "UnChanged:~a~%" (pp c :key #'POL-pol))
	   	       (print-debug 2 t "-------~%"))
;; ---------------------------------------------------------------------------


;; ---------------------------------------------------------------------------
;; Stuff from decide2.0.lisp; that is required here.
;; Normalize R using the equations E as rewrite rules
;; - R and E are lists of (POL ...)
;; Return a triple (status, newR, oldR).
;; - status is always 'satisfiable
;; - newR = list of polynomials = reduced elements of R
;; - oldR = list of polynomials = elements of R that were not changed
;; ----------------------------------------------------------------------------
(defun dynamicSimplifyRbyErec (E R)
  (dynamicSimplifyRbyErec* E R nil nil))

(defun dynamicSimplifyRbyErec* (E R new old)
  (if (null R) (values 'satisfiable new old)
      (multiple-value-bind (changed? newp) (fully-reduce E (car R))
        (cond
         ((monomial? newp)
	  (multiple-value-bind (changed2? newp2) (simplify-monomial newp)
	     (if (or changed? changed2?) 
		 (dynamicSimplifyRbyErec* E (cdr R) (cons newp2 new) old)
	         (dynamicSimplifyRbyErec* E (cdr R) new (cons (car R) old)))))
	 ((and (cdr (POL-pol newp)) (eq (pn-poly? (POL-pol newp)) 0))
	  ;; p >= 0 or p <= 0 holds and p has at least two monomials
	  (dynamicSimplifyRbyErec* E (cdr R) (append (collectProducts newp) new) old))
	 (changed?
	  (dynamicSimplifyRbyErec* E (cdr R) (cons newp new) old))
	 (t
	  (dynamicSimplifyRbyErec* E (cdr R) new (cons (car R) old)))))))


;;
;; Simplification rule 1
;; ---------------------
;; if x_1, ..., x_n are variables then x_1^d_1 * ... * x_n^d_n = 0 
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
  (assert (not (zerop (car m))))
  (list (cons 1 (simplify-zero-prod (cdr m)))))


;; Given p = (POL q witness c), check whether q is a monomial
(defun monomial? (p) 
  (eq (length (POL-pol p)) 1))

;; Given p = (POL q witness c), where q is a monomial,
;; construct (POL (simplify q) witness c)
;; return (changed p') where
;; p' = (POL (simplify q) ...)
;; changed = t if p' is different from p
;; chnaged = nil if p' equals p
(defun simplify-monomial (p)
  (assert (monomial? p))
  (let ((q (simplify-zero-mono (car (POL-pol p)))))
    (if (prep:polyrepEqual? (POL-pol p) q)
	(values nil p)
        (values t (make-pol q (POL-wit p) nil)))))


;;
;; Simplification rule 2
;  ---------------------
;; Given p = a_1 t_1 + ... + a_n t_n,
;; if t_1 >= 0, ..., t_n >= 0
;; and all coefficients a_1, ..., a_n have the same sign,
;; then  (p = 0) is equivalent to (t_1=0 and  ...  and t_n=0)
;; 
;; This function constructs the list (t_1 .... t_n) from p
;; 
(defun collectProducts (p) 
  (let ((witness (POL-wit p)))
    (let ((B (loop for i in (POL-pol p) collect (make-pol (simplify-zero-mono i) witness nil))))
      (format t "Simplifying ~a~%" (POL-pol p))
      (format t "Result: ~%~a~%" (pp B :key #'POL-pol))
      B)))



;; normalize p by E
;; - p is a decorated polynomial (POL xx yy zz)
;; - E is a list of decorated polynomials (representing equations)
;; return a pair (changed p')
;; - p' is the normalization of p
;; - changed is t if p was reduced (i.e., p' != p)
;; - changed is nil if p was unchanged
(defun fully-reduce (E p)
  (fully-reduce* E p nil 0 nil (length E)))

(defun fully-reduce* (E p Eold n changed? N)
  (cond ((eq n N) (values changed? p))
	((null E) (fully-reduce* (nreverse Eold) p nil n changed? N))
	(t
	 (multiple-value-bind (newp oldp) (dynamicSimplify (car E) (list p))
	   (declare (ignore oldp))
	   (if newp 
	       (fully-reduce* E (car newp) Eold 0 t N)
	       (fully-reduce* (cdr E) p (cons (car E) Eold) (+ n 1) changed? N))))))

;; Simplify each poly in plist by p = 0
;; return a pair (new old)
;; - new and old are lists of polynomials
;; - old contains the polynomials of plist that did not get simplified
;; - new contains the simplified polynomials of plist
(defun dynamicSimplify (p plist)
  (cond ((null plist)
	 (values nil nil))
	((null p)
	 (values nil plist))
	(t (dynamicSimplify* p plist))))

;; Simplify each poly in plist by p = 0
;; return a pair (new old)
;; - old = polynomials of plist that didn't get simplified by (p == 0)
;; - new = simplified polynomials from plists
(defun dynamicSimplify* (p plist)
  (let* ((c0s0 (car (POL-pol p)))
	 (c0 (* (car c0s0) -1))		;; -1 for moving cisi to RHS!!
	 (cisi (prep:polyrepDividePolyCst (cdr (POL-pol p)) c0))
	 (s0 (cdr c0s0)))
    (dynamicSimplifyList* s0 cisi plist (POL-wit p))))	;; pass the WITNESS


;; Replace s0 by cisi in each poly in plist: 
;; - s0 is (x_1^d_1 ... x_k^d_k) 
;;   cisi is a polynomial smaller than s0 in the term ordering
;; - plist is a list of decorated polynomials (cf. POL structure)
;; - wit is the witness for the equality s0 = cisi
;; return (new old) such that
;;   old = polynomials from plist not changed by the substitution
;;   new = result of applying hte substitution to the other polynomials of plist
(defun dynamicSimplifyList* (s0 cisi plist wit &optional (new nil) (old nil))
  (if (null plist) (values new old)
      (let* ((p (car plist))
	     (newp (simplify-poly* s0 cisi (POL-pol p))))
        (if (not (eq newp 0)) (print-debug 1 t "Simplified ~A to~% ~A~%" (POL-pol p) newp))
	(if (eq newp 0)
	    (dynamicSimplifyList* s0 cisi (cdr plist) wit new (cons p old))
	    (let* ((newp1 (normalizePoly newp))
		   (newp2 (combine-wit wit p))
		   (newp3 (make-POL :pol newp1 :wit newp2)))
	      (dynamicSimplifyList* s0 cisi (cdr plist) wit (cons newp3 new) old))))))


;; appply substitution s0 --> cisi to polynomial poly
;; return 0 if poly is unchanged by the substitution 
;; (i.e., s0 does not divide any monomial of poly)
;; return the substitution result otherwise
;;
;; NOTE: there's no confusion since the zero polynomial is represented
;; by nil (empty list of monomials), not by 0.
(defun simplify-poly* (s0 cisi poly)
  (if (null poly) 0
      (let ((poly1 (simplify-mono* s0 cisi (car poly)))
	    (poly2 (simplify-poly* s0 cisi (cdr poly))))
        (cond ((and (eq poly1 0) (eq poly2 0))
	       0)
	      ((eq poly1 0)
	       (cons (car poly) poly2))	;; ordering guarantees this
	      ((eq poly2 0)
	       (prep:polyrepAddPoly poly1 (cdr poly)))
	      (t 
	       (prep:polyrepAddPoly poly1 poly2))))))


;; apply subsitution s0 --> cisi to monomial mono
;; mono is (d0 . t0) where t0 = x_1^d_1 ... x_k^dk 
;;                     and d0 is a constant
;; return 0 if s0 does not divide t0
;; return c * u0 * cisi if t0 = u0 * s0
(defun simplify-mono* (s0 cisi mono)
  (let* ((d0 (car mono))
	 (t0 (cdr mono))
	 (newt0 (simplify-pp* s0 cisi t0)))
    (if (eq newt0 0)
	0
	(prep:polyrepMultiplyCstPoly d0 newt0))))


;; apply substitution s0 --> cisi to term t0
;; t0 is a product (x_1^d_1 ... x_k^dk)
;; return 0 if s0 does not divide t0
;; return u0 * cisi if t0 = u0 * s0
(defun simplify-pp* (s0 cisi t0)
  (let ((t0bys0 (prep:polyrepDividePP t0 s0)))
    (if (eq t0bys0 0)
	0
	(prep:polyrepMultiplyMonoPoly (cons 1 t0bys0) cisi))))


;; make the main coefficient of p equal to 1
(defun normalizePoly (p)
  (if (null p) p
      (let ((c0 (abs (caar p))))
	(if (eq c0 1) p (prep:polyrepDividePolyCst p c0)))))

;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Witness functions
;; ---------------------------------------------------------------------------
(defun combine-wit (p q)
  (union (if (POL-p p) (POL-wit p) p) (POL-wit q)))

(defun get-witness (E)
  (print-debug 2 t "Get-witness function called with WITNESS~%~A~%" E)
  E)
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Superposition: Critical pair computation
;; ---------------------------------------------------------------------------

;; Compute the s-polynomial of p and q and filter out large/high degree polynomials
;;
;; - return nil if (p, q) is not a critical pair
;; - return nil is s-polynomial is too large (too many monomials) or has high degree
;;   this filtering relies on the global variables *degreebound* and *lengthbound*
;; - return the s-polynomial otherwise with witness = (union of p and q's witnesses)
;; 
(defun superposition (p q)
  (let* ((cp1 (superposition* p q))
	 (cp (if (bound-check? cp1) cp1 nil)))
    (if (null cp) nil 
        (make-POL :pol cp :wit (combine-wit p q) :c nil))))

;; Critical pair:
;; - compute the S-polynomial for p1 and q1
;;   p1 is (POL p ...)
;;   q1 is (POL q ...)
;; - p is c0.s0 + ... (where c0.s0 is the leading monomial)
;; - q is d0.t0 + ... (where d0.t0 is the leading monomial)
;; p and q form a critical pair if the lcm of s0 and t0 is not s0.t0
;; then we have lcm(s0, t0) = s0.v0 = t0.w0
;; and the S-polynomial is (d0 * v0 * p - c0 * w0 * q) whose leading 
;; monomial is smaller than lcm(s0, t0) in the term ordering.
;;
;; Side effect: add w0 to q1's POL-c component
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
	      (normalizePoly ans))
	    nil))))

;; 
(defun overlap-pp-pp (mu nu nolistmu nolistnu)
  (let ((order (prep:get-order)))
    (multiple-value-bind (st mu0 nu0) (overlap-pp-pp* mu nu order nil nil nil)
      (if (or (member mu0 nolistmu :test #'multiple?)
	      (member nu0 nolistnu :test #'multiple?))
	  (progn (print-debug 5 t "OPTIMIZATION-1 works~%") nil)
	  (values st mu0 nu0)))))

;;
;; check whether mu and nu overlap
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
      (print-debug 5 t "OPTIMIZATION-2 works~%")
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

;; Check whether mu is a multiple of nu
;; - return t if mu = nu * nu1
;; - return nil otherwise
(defun multiple? (mu nu)
  (cond ((null nu) t)
	((null mu) nil)
	((prep:var-equal? (caar nu) (caar mu))
	 (if (<= (cdar nu) (cdar mu))
	     (multiple? (cdr mu) (cdr nu)) 
	     nil))
	(t (multiple? (cdr mu) nu))))

;; ---------------------------------------------------------------------------

;; ----------------------------------------------------------------------------
;; maxOfList: return maximum element of list, use fn to compare.
;; ----------------------------------------------------------------------------
(defun maxOfList (plist fn &key (key #'(lambda(x) x)) (ret #'(lambda(x) x)))
  (assert (car plist))
  (maxOfList* (cdr plist) fn (car plist) key ret))

(defun maxOfList* (plist fn ans key ret)
  (if (null plist) (funcall ret ans)
      (if (funcall fn (funcall key (car plist)) (funcall key ans))
	  (maxOfList* (cdr plist) fn (car plist) key ret)
	  (maxOfList* (cdr plist) fn ans key ret))))
;; ----------------------------------------------------------------------------

