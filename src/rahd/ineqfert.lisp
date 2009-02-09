;;;
;;; Simple inequality fertilization for multivariate polynomial constraints
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
;;; This file: began on         30-June-2008,
;;;            last updated on  22-Sept-2008.
;;;

(in-package RAHD)

;;;
;;; FERTILIZE-TRIVIAL-SQUARES (c): Given a conjunction, return the result of conjoining
;;; a constraint (>= p 0) for each polynomial in c that is a trivial square (e.g. every
;;; variable in every power-product has even degree, every monomial coefficient is non-neg).  
;;; We also keep track of context here, and *if* we are inside a cojunct that is proclaiming 
;;; (< p 0), we return :UNSAT (:UNSAT-WITNESS (< p 0) (>= p 0)) if p is a trivial square.
;;;
;;; Note: All polynomials are canonicalized in the process.  This is expensive (e.g. O(nm)).  
;;; Perhaps I will try and cache such a thing in the future.  In the mean time, I will just 
;;; remain aware of this fact during waterfall construction.
;;;
;;; Adjusted to recognize negated trivial squares:
;;;   e.g., we now recognize that (> (* (-1) P) 0), where P is SOS, is unsat.
;;;

(defun fertilize-trivial-squares* (c result)
  (cond ((endp c) result)
	(t (let ((cur-lit (car c)))
	     (if (consp cur-lit)
		 (let ((cur-rel (car cur-lit))
		       (cur-x   (cadr cur-lit))
		       (cur-y   (caddr cur-lit)))
		   (let ((cur-x-poly-reps (canonicalize-poly-with-alg-rep cur-x))
			 (cur-y-poly-reps (canonicalize-poly-with-alg-rep cur-y)))
		   (let ((x-triv-sq? (and (poly-trivial-square (cdr cur-x-poly-reps)) 
					  (not (numberp cur-x))))
			 (y-triv-sq? (and (poly-trivial-square (cdr cur-y-poly-reps))
					  (not (numberp cur-y))))
			 (x-canonical (car cur-x-poly-reps))
			 (y-canonical (car cur-y-poly-reps)))
		     (cond ((and (equal cur-rel '<)
				 (equal y-canonical 0)
				 x-triv-sq?)
			    `(:UNSAT (:UNSAT-WITNESS ,cur-lit (>= ,cur-x 0) :TRIVIAL-SQUARE)))
			   ((and (equal cur-rel '>)
				 (equal x-canonical 0)
				 y-triv-sq?)
			    `(:UNSAT (:UNSAT-WITNESS ,cur-lit (>= ,cur-y 0) :TRIVIAL-SQUARE)))
			   ((and (equal cur-rel '>)
				 (equal y-canonical 0)
				 (poly-trivial-square (poly-mult (cdr cur-x-poly-reps) '((-1)))))
			    `(:UNSAT (:UNSAT-WITNESS ,cur-lit (<= ,cur-x 0) :TRIVIAL-SQUARE-NEGATED)))
			   (t (if (or x-triv-sq? y-triv-sq?) 
				  (let ((new-fertilizers
					 (if (and x-triv-sq? y-triv-sq?)
					     `((>= ,cur-x 0) (>= ,cur-y 0))
					   (if x-triv-sq? `((>= ,cur-x 0))
					     `((>= ,cur-y 0))))))
				    (fertilize-trivial-squares* 
				     (cdr c) 
				     (append (append `((,cur-rel ,x-canonical ,y-canonical)) new-fertilizers) result)))
				(fertilize-trivial-squares* 
				 (cdr c)
				 (cons cur-lit result)))))))) ;; Better not to canonicalize poly if we don't have to.
	       cur-lit)))))

;;;
;;; Adjusted on 18-Sept-2008 to no longer return the fertilized conjunct if a
;;; refutation was not found.
;;;

(defun fertilize-trivial-squares (c)
  (let ((result (fertilize-trivial-squares* c nil)))
    (if (and (consp result)
	     (not (equal (car result) ':UNSAT)))
	;(reverse result)
	c 
      result)))