;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;;
;;; Conversion of multivariate polynomials between two representations:
;;;  (a) The (mo<)-ordered monomial based representation used for polynomial algebra
;;;      as developed and utilised in polyalg.lisp, sturm.lisp, etc., and
;;;  (b) The free extended quoted Lisp notation as used in the top-level prover,
;;;      simplifier, and case-splitter,
;;;
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: (g.passmore@ed.ac.uk . http://homepages.inf.ed.ac.uk/s0793114/)
;;;
;;; >> Requires: polyalg.lisp
;;;
;;; This file: began on         30-July-2008,
;;;            last updated on  22-Oct-2008.
;;;

(in-package :rahd)

;;;
;;; CANONICALIZE-POLY (p): Convert a polynomial in PROVER representation to
;;; an equivalent polynomial in PROVER representation that has been 
;;; canonicalized by the term order MO<.  This is done by passing it
;;; through the PROVER ==> POLYALG converter and then returning back.
;;;
;;; Updated (22-Oct-2008) to utilize *CANON-POLY-CACHE* when possible.
;;;

(defun canonicalize-poly (p)
  (if *canon-poly-use-cache*

      ;; User wants to use the canon-poly-cachhe, so let's do it:

      (multiple-value-bind (canon-poly-val canon-poly-hash-exists?)
	  (gethash p *canon-poly-cache*)
	
	;; Is this poly's canonicalized form already in the cache?
	
	(if canon-poly-hash-exists?
	    (progn 
	      (if *rahd-debug* (format t "~%[CANON-POLY-CACHE]:: ~A --> ~A" p canon-poly-val))
	      (when (and *rahd-debug* 
			 (not (equal (poly-alg-rep-to-prover-rep
				      (poly-prover-rep-to-alg-rep p)) canon-poly-val)))
		(format t "~%~%>>>>>>>>>>>>> [CANON-POLY-CACHE]:: VAR-ORDER-PERMUTATION:: ~A --> ~A" p canon-poly-val))	    
	      canon-poly-val)
	  
	  ;; If not, let's compute it and place it in the cache.
	  
	  (let ((canon-poly (poly-alg-rep-to-prover-rep
			     (poly-prover-rep-to-alg-rep p))))
	    (setf (gethash p *canon-poly-cache*) canon-poly)
	    canon-poly)))

    ;; Otherwise, the user doesn't want to use the canon-poly-cache:

    (poly-alg-rep-to-prover-rep
     (poly-prover-rep-to-alg-rep p))))
   
;;;
;;; CANONICALIZE-POLY-WITH-ALG-REP (p): Given a polynomial in PROVER representation,
;;; return a pair: (p1 . p2) where p1 is the polynomial canonicalized in PROVER rep.,
;;; and p2 is the polynomial in ALG rep.
;;;

(defun canonicalize-poly-with-alg-rep (p)
  (let ((poly-alg-rep (poly-prover-rep-to-alg-rep p)))
    (let ((poly-canonicalized (poly-alg-rep-to-prover-rep poly-alg-rep)))
      (cons poly-canonicalized poly-alg-rep))))

;;;
;;; POLYALG representation ==> PROVER representation
;;;

(defun poly-alg-rep-to-prover-rep (p)
  (let ((result (poly-alg-rep-to-prover-rep* p)))
    (if result result 0)))

(defun poly-alg-rep-to-prover-rep* (p)
  (cond ((endp p) nil)
	(t (let ((cur-mon (car p)))
	     (if (consp (cdr p))
		 `(+ ,(mon-alg-rep-to-prover-rep cur-mon)
		     ,(poly-alg-rep-to-prover-rep* (cdr p)))
	       (mon-alg-rep-to-prover-rep cur-mon))))))

(defun mon-alg-rep-to-prover-rep (m)
  (let ((cur-coeff (car m))
	(cur-pp (cdr m)))
    (case cur-coeff
	  (0 0)
	  (1 (if cur-pp (pp-alg-rep-to-prover-rep cur-pp)
	       1))
	  (otherwise 
	   (if cur-pp `(* ,cur-coeff ,(pp-alg-rep-to-prover-rep cur-pp))
	     cur-coeff)))))
      
(defun pp-alg-rep-to-prover-rep (pp)
  (cond ((endp pp) nil)
	(t (let ((cur-vp (car pp)))
	     (let ((cur-v (car cur-vp))
		   (cur-p (cdr cur-vp))) 
	     (if (consp (cdr pp))
		 `(* ,(vp-alg-rep-to-prover-rep cur-v cur-p)
		     ,(pp-alg-rep-to-prover-rep (cdr pp)))
	       (vp-alg-rep-to-prover-rep cur-v cur-p)))))))

(defun vp-alg-rep-to-prover-rep (v p)
  (cond ((= p 0) 1)
	(t (let ((v-sym (nth v *vars-table*)))
	     (if (> p 1)
		 `(* ,v-sym
		     ,(vp-alg-rep-to-prover-rep v (1- p)))
	       v-sym)))))

;;;
;;; PROVER representation ==> POLYALG representation
;;;
;;; Note: We will extend *VARS-TABLE* if a variable is encountered in the PROVER notation
;;;  that is not currently within our polynomial ring Q[*vars-table*].
;;;
;;; Note: Polynomials must be converted into binary notation (TERM-TO-BIN-OPS) before
;;;  being converted to POLYALG notation.

(defun poly-prover-rep-to-alg-rep* (p)
  (cond ((equal p nil) `((0)))
	((numberp p) `((,p)))
	((symbolp p) 
	 (let ((var-id (find-var p *vars-table* 0)))
	   (if (not var-id)
	       (let ((var-id* (extend-vars-table p)))
		 `((1 . ((,var-id* . 1)))))
	     `((1 . ((,var-id . 1)))))))
	((consp p)
	 (let ((fcn (car p))
	       (x   (cadr p))
	       (y   (caddr p)))
	   (case fcn
		 (* (poly-mult (poly-prover-rep-to-alg-rep* x)
			       (poly-prover-rep-to-alg-rep* y)))
		 (+ (poly+     (poly-prover-rep-to-alg-rep* x)
			       (poly-prover-rep-to-alg-rep* y)))
		 (- (poly-     (poly-prover-rep-to-alg-rep* x)
			       (poly-prover-rep-to-alg-rep* y))))))))

(defun poly-prover-rep-to-alg-rep (p)
  (poly-zsimp (poly-prover-rep-to-alg-rep* p)))

(defun find-var (v v-table pos)
  (cond ((endp v-table) nil)
	((equal (car v-table) v) pos)
	(t (find-var v (cdr v-table) (1+ pos)))))

(defun extend-vars-table (v)
  (setq *vars-table* (append *vars-table* `(,v)))
  (1- (length *vars-table*)))
  
