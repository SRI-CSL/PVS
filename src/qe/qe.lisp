;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qelim.lisp -- 
;; Author          : Harald Ruess
;; Created On      : Mon Jul 28 09:18:53 PDT 1997
;; Last Modified By: Harald Ruess
;; Last Modified On: Mon Jul 28 09:18:53 PDT 1997
;; Update Count    : 0
;; Status          : Unknown, use with caution
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;; Prover Interface

(addrule 'qe nil ((fnums '*))
    (qelim-step fnums)
    "Quantifier Elimination. Replaces sequent formulas
     specified by 'fnums' with equivalent formulas, where
     arithmetic (both *reals* and *integers*) and boolean quantifiers
     have been eliminated whenever possible; i.e. the formula should,
     usually, only contain linear arithmetic terms and bound variables
     should, usually, not occur in the scope of uninterpreted functions.

     NOTE: This code is highly experimental, and its functionality,
     speed, and robustness is likely to improve in the near future.
     Thus, proof scripts using this command may break in future releases
     of PVS.")
     

(defun qelim-step (fnums)
  #'(lambda (ps)
      (multiple-value-bind (signal subgoal)
	  (sequent-reduce (current-goal ps) #'qelim-sform fnums)
	(values signal (list subgoal)))))

(defun qelim-sform (sform)
  (reset-translate-to-dc)
  (reset-translate-from-dc)
  (let* ((fmla (formula sform))
         (new-fmla (qelim fmla))
         (new-sform (if (tc-eq fmla new-fmla) sform
                      (lcopy sform 'formula new-fmla))))
    (if (s-form-equal? sform new-sform)
        (values 'X sform)
      (values '? new-sform))))

(defun qelim (fml &optional (state (dp::null-single-cong-state)))
  (let ((*state* state))
    (declare (special *state*))
    (let ((new-fml (catch 'unable
		     (qelim* fml))))
      (or new-fml fml))))

(defmethod qelim* ((fml expr))
  fml)

(defmethod qelim* ((fml exists-expr))
  (multiple-value-bind (bndngs body)
      (destructure-existential (lift-predicates-in-quantifier fml
							      (list *integer*)))
    (let ((*bound-variables* (append bndngs *bound-variables*)))
      (declare (special *bound-variables*))
      (qelim1 bndngs (qelim* body)))))

(defmethod qelim* ((fml forall-expr))
  (multiple-value-bind (bndngs body)
      (destructure-universal (lift-predicates-in-quantifier fml
							    (list *integer*)))
    (let ((*bound-variables* (append bndngs *bound-variables*)))
      (declare (special *bound-variables*))
      (make!-negation (qelim1 bndngs
			      (make!-negation (qelim* body)))))))

(defmethod qelim* ((fml conjunction))
  (make!-conjunction (qelim* (args1 fml))
		     (qelim* (args2 fml))))

(defmethod qelim* ((fml disjunction))
  (make!-disjunction (qelim* (args1 fml))
		     (qelim* (args2 fml))))

(defmethod qelim* ((fml implication))
  (make!-implication (qelim* (args1 fml))
		     (qelim* (args2 fml))))

(defmethod qelim* ((fml branch))
  (make!-if-expr (qelim* (condition fml))
		 (qelim* (then-part fml))
		 (qelim* (else-part fml))))

(defmethod qelim* ((fml iff-or-boolean-equation))
  (make!-iff (qelim* (args1 fml))
	     (qelim* (args2 fml))))
		
(defmethod qelim* ((fml negation))
  (make!-negation (qelim* (args1 fml))))

(defmethod qelim1 ((bndngs list) fml)
  (if (null bndngs) fml
      (qelim1 (car bndngs)
	      (qelim1 (cdr bndngs) fml))))

(defmethod qelim1 :around ((bndng bind-decl) fml)
    (if (occurs-in bndng fml)
	(call-next-method)
      fml))

(defmethod qelim1 ((bndng bind-decl) fml)
  (error-format-if "~%Eliminating ~a" bndng)
  (cond ((tc-eq (type bndng) *boolean*)
	 (qelim1-boolean bndng fml))
	(;(tc-eq (find-supertype (type bndng)) *number*) ; needed?
	 (or (tc-eq (type bndng) *real*) (tc-eq (type bndng) *integer*))
	 (let* ((dnf (dnf fml)))
	   (error-format-if "~%Disjunctions: ~a" (length dnf)) 
	   (if (in-scope-of-floor? bndng dnf)
               (let* ((new-bndng (my-make-new-bind-decl bndng))
		      (*bound-variables* (cons new-bndng *bound-variables*)))
		 (declare (special *bound-variables))
		 (error-format-if "~%Eliminating ~a in floor" bndng)
		 (let ((elim-dnf (eliminate-shielded new-bndng bndng dnf)))
		   (if (or (in-scope-of-floor? bndng elim-dnf)
			   (in-scope-of-floor? new-bndng elim-dnf))
		       (unable-to-eliminate bndng fml) 
		       (qelim-disjuncts new-bndng elim-dnf))))    
		 (qelim-disjuncts bndng dnf))))
	 (t
	  (unable-to-eliminate bndng fml))))

(defun unable-to-eliminate (bndng fml)
  (error-format-if "~%Unable to eliminate (EXISTS (~a): ~a)" bndng fml)
  (throw 'unable nil))

(defun qelim-disjuncts (bndng disjunct &optional acc)
  (when (complementary-pair? disjunct)
    (return-from qelim-disjuncts *true*))
  (if (null disjunct)
      (disjuncts-to-fml acc)
      (let* ((conjunct (car disjunct))
	     (new-fml (catch 'unsatisfiable
			(qelim-conjuncts bndng conjunct)))
	     (new-acc (if new-fml
			  (cons new-fml acc)
			 acc)))
	  (qelim-disjuncts bndng
			   (cdr disjunct)
			   new-acc))))

(defun qelim-conjuncts (bndng conjuncts)
  (declare (special *state*))
  (error-format-if "~%Eliminate ~a" bndng)
  (when (complementary-pair? conjuncts)
    (return-from qelim-conjuncts *false*))
  (protecting-dp-state ((*state* *state*))
     (multiple-value-bind (lower-ineqs upper-ineqs solved-eqs disequalities noccurs others)
	 (partition-conjuncts bndng conjuncts)
       (assert (every #'(lambda (fml) (not (occurs-in bndng fml))) noccurs))
      
       (cond ((some #'(lambda (diseq)
			(inconsistent-disequality diseq *state*))
		    disequalities)
	      *false*)
	     ((consp solved-eqs)
	      (let ((solved-expr (choose-solved-form bndng solved-eqs)))
		(error-format-if "~%Solved Form: ~a" solved-expr)
		(assert (not (occurs-in bndng solved-expr)))
		(let ((subst (acons bndng solved-expr nil)))
		  (conjuncts-to-fml
		   (append (instantiate-fmlas lower-ineqs subst *state*)
			   (instantiate-fmlas upper-ineqs subst *state*)
			   (instantiate-fmlas solved-eqs subst *state*)
			   (instantiate-fmlas disequalities subst *state*)
			   (instantiate-fmlas noccurs subst *state*)
			   (instantiate-fmlas others subst *state*)
			   (if (and (tc-eq (type bndng) *integer*)
				    (not (integer? solved-expr)))
			       (list (make!-equation solved-expr
						     (make!-floor solved-expr)))
			       nil))))))
	     ((not (null others))
	      (error-format-if "~%Bound variable ~a in ~{~a~^, ~}" bndng others)
	      (unable-to-eliminate bndng (conjuncts-to-fml conjuncts)))
	     ((and (null lower-ineqs)
		   (null upper-ineqs))
	      (conjuncts-to-fml noccurs))
	     ((or (null lower-ineqs)
		  (null upper-ineqs))
	      (conjuncts-to-fml noccurs))
	     (t (error-format-if "~%FME: ~{~a~^, ~}, ~{~a~^, ~}" lower-ineqs upper-ineqs)
	        (let ((new-ineqs (FME lower-ineqs upper-ineqs bndng)))
		  (cond ((some #'(lambda (fml) (tc-eq fml *false*)) new-ineqs)
			 *false*)
			((not (occurs-in bndng disequalities))
			 (conjuncts-to-fml
			  (append new-ineqs noccurs disequalities)))
			(t
			 (unable-to-eliminate bndng (conjuncts-to-fml conjuncts))))))))))

(defun display-partition (lower-ineqs upper-ineqs solved-eqs disequalities noccurs others)
  (error-format-if "~% --> Lower ineqs: ~a, Upper ineqs: ~a, Eqs: ~a, Diseqs: ~a, Noccurs: ~a, Others: ~a" (length lower-ineqs)
	     (length upper-ineqs)
	     (length solved-eqs)
	     (length disequalities)
	     (length noccurs)
	     (length others)))

(defun FME (lower-ineqs upper-ineqs bndng &optional acc)
  "Fourier-Motzkin Elimination."
  (if (null lower-ineqs) (nreverse acc)
      (FME (cdr lower-ineqs) upper-ineqs bndng
	   (union (FME1 (car lower-ineqs) upper-ineqs bndng) acc :test #'tc-eq))))

(defun FME1 (lower-ineq upper-ineqs bndng &optional acc)
  (declare (special *state*))
  (if (null upper-ineqs) (nreverse acc)
      (let* ((upper-ineq (car upper-ineqs))
	     (new-ineq (solve-for (eliminate-variable bndng
						      lower-ineq
						      upper-ineq)
				  bndng
				  *state*))
	     (newacc (cond ((tc-eq new-ineq *true*)
			    acc)
			   ((tc-eq new-ineq *false*)
			    (throw 'unsatisfiable nil))
			   (t
			    (adjoin new-ineq acc :test #'tc-eq)))))
	(FME1 lower-ineq (cdr upper-ineqs) bndng newacc))))

(defun eliminate-variable (bndng lower-ineq upper-ineq)
  (let* ((lower (solved-form lower-ineq bndng))
         (upper (solved-form upper-ineq bndng)))
    (cond ((and (strict-ineq? lower-ineq)
                (strict-ineq? upper-ineq))
           (if (tc-eq (type bndng) *integer*)
               (make!-lt (make!-succ (make!-floor lower))
                         upper)
               (make!-lt lower upper)))
          ((and (nonstrict-ineq? lower-ineq)
                (nonstrict-ineq? upper-ineq))
           (make!-le lower upper))
          (t
	   (throw 'unable nil)))))

(defun solved-form (expr bndng)
  (let ((lhs (args1 expr))
	(rhs (args2 expr)))
     (cond ((solved? lhs rhs bndng) rhs)
	   ((solved? rhs lhs bndng) lhs)
	   (t (throw 'unable nil)))))

(defun solved? (x expr bndng)
  (and (var-bound-by? x bndng)
       (not (occurs-in bndng expr))))

(defun var-bound-by? (x bndng)
  (and (name-expr? x)
       (tc-eq (declaration x) bndng)))
  
(defun instantiate-fmlas (fmls subst state)
  (mapcar #'(lambda (fml)
	      (let ((new-fml (substitute-and-simplify fml subst state)))
		(if (tc-eq new-fml *false*)
		    (throw 'unsatisfiable nil)
		  new-fml)))
          fmls))

(defun choose-solved-form (bndng equalities) 
  (if (null equalities)
      (throw 'unable nil)
      (let ((lhs (args1 (car equalities)))
	    (rhs (args2 (car equalities))))
	(cond ((not (occurs-in bndng rhs))
	       rhs)
	      ((not (occurs-in bndng lhs))
	       lhs)
	      (t
	       (choose-solved-form bndng (cdr equalities)))))))
	      
(defun partition-conjuncts (bndng conjuncts)
  (declare (special *state*))
  (let ((lower-ineqs nil)
	(upper-ineqs nil)
	(solved-eqs nil)
	(disequalities nil)
	(noccurs nil)
	(others nil))
    (loop for fml in conjuncts
       do (cond ((not (occurs-in bndng fml))
		 (push fml noccurs))
		((or (disequality? fml)
		     (and (negation? fml)
			  (equation? (args1 fml))))
		 (multiple-value-bind (lhs rhs)
		     (destructure-disequality fml)
		   (when (tc-eq (dp-canon lhs *state*)
				(dp-canon rhs *state*))
		     (throw 'unsatisfiable nil))
		   (push fml disequalities)))
		((negation? fml)
		 (push fml others))
		(t
		 (let ((new-fml (solve-for fml bndng *state*)))
		   (cond ((tc-eq new-fml *true*)
			  nil)  ; skip
			 ((tc-eq new-fml *false*)
			  (throw 'unsatisfiable nil))
			 ((equation? new-fml)
			  (push new-fml solved-eqs))
			 ((and (less? new-fml)        ; x < e
			       (var-bound-by? (args1 new-fml) bndng)
			       (not (occurs-in bndng (args2 new-fml))))
			  (push new-fml upper-ineqs))
			 ((and (greater? new-fml)        ; x > e
			       (var-bound-by? (args1 new-fml) bndng)
			       (not (occurs-in bndng (args2 new-fml))))
			  (push new-fml lower-ineqs))
			 ((and (lesseq? new-fml)        ; x <= e
			       (integer? (args1 new-fml))
			       (integer? (args2 new-fml))
			       (var-bound-by? (args1 new-fml) bndng)
			       (not (occurs-in bndng (args2 new-fml))))
			  (push (make!-lt (args1 new-fml)
					  (make!-succ (args2 new-fml)))
				upper-ineqs))
			 ((and (greatereq? new-fml)        ; x >= e
			       (integer? (args1 new-fml))
			       (integer? (args2 new-fml))
			       (var-bound-by? (args1 new-fml) bndng)
			       (not (occurs-in bndng (args2 new-fml))))
			  (push (make!-gt (args1 new-fml)
					  (make!-pred (args2 new-fml)))
				lower-ineqs))
			 (t
			  (push new-fml others)))))))
    (values lower-ineqs upper-ineqs solved-eqs
	    disequalities noccurs others)))

(defun inconsistent-disequality (diseq *state*)
  (multiple-value-bind (lhs rhs)
      (destructure-disequality diseq)
    (tc-eq (dp-canon lhs *state*)
	   (dp-canon rhs *state*))))

; Quantifier Elimination for Booleans

(defun qelim1-boolean (bndng fml)
  (declare (special *state*))
  (let ((lhs (substitute-and-simplify fml (acons bndng *true* nil) *state*))
	(rhs (substitute-and-simplify fml (acons bndng *false* nil) *state*)))
    (cond ((tc-eq lhs *false*) rhs)
	  ((tc-eq rhs *false*) lhs)
	  ((or (tc-eq lhs *true*) (tc-eq rhs *true*)) *true*)
	  (t (make!-disjunction lhs rhs)))))

;; is a certain variable in the scope of a floor

(defmethod in-scope-of-floor? (bndng (exprs list) &optional inside)
  (some #'(lambda (expr)
	    (in-scope-of-floor? bndng expr inside))
	exprs))

(defmethod in-scope-of-floor? (bndng (expr name-expr) &optional inside)
  (and inside (tc-eq bndng (declaration expr))))

(defmethod in-scope-of-floor? (bndng (expr number-expr) &optional inside)
  (declare (ignore bndng) (ignore inside))
  nil)

(defmethod in-scope-of-floor? (bndng (expr application) &optional inside)
  (if (tc-eq (operator expr) (floor-operator))
      (in-scope-of-floor? bndng (arguments expr) 'T)
    (in-scope-of-floor? bndng (arguments expr) inside)))

(defmethod in-scope-of-floor? (bndng (expr binding-expr) &optional inside)
  (in-scope-of-floor? bndng (expression expr) inside))
	    
;; Eliminate Shielded occurrences of a binding in the scope of floors

(defun eliminate-shielded (new-bndng bndng dnf &optional (acc *false-dnf*))
  (if (null dnf) (nreverse acc)
      (eliminate-shielded new-bndng bndng
			  (cdr dnf)
			  (dnf-disjunction
			    (eliminate-shielded1 new-bndng bndng (car dnf))
			    acc))))
				  
(defun eliminate-shielded1 (new-bndng bndng conjuncts)
  (let ((delta (delta bndng conjuncts)))
    (assert (not (null delta))) ; ???
    (error-format-if ", delta: ~a" delta)
    (cond ((= delta 1)
	   (list conjuncts))
	  ((integerp delta)
	   (let* ((multiple (make!-times (make!-number-expr delta)
					 (make-variable-expr new-bndng))))
	     (loop for k from (1- delta) downto 0
		   collect (let ((new-expr (make!-plus multiple
						       (make!-number-expr k))))  
			     (mapcar #'(lambda (atom)
					 (substitute-and-simplify atom
								  (acons bndng new-expr nil)
								  (dp::null-single-cong-state)))
			       conjuncts)))))
	  (t
	   (list conjuncts)))))

(defun my-make-new-bind-decl (bndng)
  (let ((type (type bndng))
	(newid (intern (format nil "~a~a" (id bndng) (gensym)))))
    (mk-bind-decl newid type type)))
                       

;; Least common multiple of the of accumulated coefficients of occurences of
;; bound variable x within the scope of a floor; e.g. 
;;        (delta x (1/2 * [1/5 + [2/3 * x + 1/4]])) = 6
;; Returns nil if there is a non-linear multiplication or no occurrence of x.

(defun delta (bndng exprs)
  (let ((*bndng* bndng))
    (declare (special *bndng*))
    (let ((coefficients (catch 'nonlinear
			  (coefficients* exprs))))
      (and coefficients
	   (apply #'lcm coefficients)))))

(defmethod coefficients* ((conjuncts null))
  '())

(defmethod coefficients* ((conjuncts cons))
  (union (coefficients* (car conjuncts))
	 (coefficients* (cdr conjuncts))
	 :test #'eql))
      
(defmethod coefficients* ((expr name-expr))
  (declare (special *bndng*))
  (if (tc-eq (declaration expr) *bndng*) (list 1) '()))

(defmethod coefficients* ((expr number-expr))
    '())

(defmethod coefficients* ((expr application))
  (let ((op (operator expr)))
    (if (tc-eq op (times-operator))
	(multiple-value-bind (coefficient arg)
	    (destructure-linear-multiplication expr)
	  (declare (ignore arg))
	  (cons (denominator coefficient)
		(coefficients* (arguments expr))))
      (coefficients* (arguments expr)))))

(defun destructure-linear-multiplication (expr)
  (let ((r1 (rational-coefficient
	      (pseudo-normalize (args1 expr))))
	(r2 (rational-coefficient
	      (pseudo-normalize (args2 expr)))))
    (cond ((and r1 r2) (values (* r1 r2) nil))
	  (r1 (values r1 r2))
	  (r2 (values r2 r1))
	  (t (throw 'nonlinear nil)))))

(defun rational-coefficient (expr)
  (or (and (number-expr? expr)
	   (number expr))
      (and (application? expr)
	   (tc-eq (operator expr) (divides-operator))
	   (expr-to-rational expr))
      (and (application? expr)
	   (tc-eq (operator expr) (unary-minus-operator))
	   (application? (argument expr))
	   (tc-eq (operator (argument expr)) (divides-operator))
	   (let ((rat (expr-to-rational (argument expr))))
	     (when rat (- rat))))))

(defun expr-to-rational (expr)
  (when (and (number-expr? (args1 expr))
	     (number-expr? (args2 expr)))
    (/ (number (args1 expr))
       (number (args2 expr)))))

(defun complementary-pair? (conjuncts)
  (if (null conjuncts) nil
    (let ((focus (car conjuncts)))
      (or (some #'(lambda (expr)
		    (complementary? focus expr))
		(cdr conjuncts))
	  (complementary-pair? (cdr conjuncts))))))

(defun complementary? (expr1 expr2)
  (or (and (negation? expr1)
	   (tc-eq (argument expr1) expr2))
      (and (negation? expr2)
	   (tc-eq (argument expr2) expr1))))


