;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qe.lisp -- 
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

(addrule 'qe nil ((fnums '*) verbose?)
    (qe-step fnums nil verbose?)
    "Replaces each sequent formula in FNUMS with an equivalent
     quantifier-free formula if such a form can be constructed;
     otherwise the formula is left unchanged.

     Currently, arithmetic (INTEGER, RATIONAL, REAL) and boolean
     quantifiers are eliminated. Quantified formulas involving nonlinear
     arithmetic terms or bound variables in the scope of uninterpreted
     function or predicate symbols are usually not simplified.
 
     NOTE: This code is highly experimental, and its functionality,
     correctness, speed, and robustness is likely to improve in the near
     future. In particular: proof scripts using this command may break
     in future releases of PVS.")

(defun qe-step (fnums complete? verbose?)
  #'(lambda (ps)
      (let ((*verbose* verbose?)
	    (*complete* complete?))
	(declare (special *verbose*)
		 (special *complete*))
	(multiple-value-bind (signal subgoal)
	    (sequent-reduce (current-goal ps) #'qe-sform fnums)
	  (values signal (list subgoal))))))

(defun qe-sform (sform)
  (reset-translate-to-dc)
  (reset-translate-from-dc)
  (let* ((fmla (formula sform))
	 (state (or *dp-state*
		    *init-dp-state*
		    (dp::null-single-cong-state)))
         (new-fmla (qe fmla state))
         (new-sform (if (tc-eq fmla new-fmla) sform
			(lcopy sform 'formula new-fmla))))
    (if (s-form-equal? sform new-sform)
        (values 'X sform)
      (values '? new-sform))))

;; Messages and aborts

(defmacro qe-msg (str &rest args)
  `(locally (declare (special *verbose*))
       (when *verbose*
	 (format t ,str ,@args))))

(defmacro qe-abort (&optional fmt &rest args)
  `(progn (error-format-if ,fmt ,@args)
	  (throw 'unable nil)))

(defun unsat () :unsat)

(defun unsat? (x) (eq x :unsat))

(defmacro throw-unsat (&optional fmt &rest args)
  `(progn (qe-msg ,fmt ,@args)
	  (throw 'unsatisfiable (unsat))))

;; Quantifier elimination

(defun qe (fml state)
  (let ((*state* state))
    (declare (special *state*))
    (let ((new-fml (catch 'unable
		     (qe* fml))))
      (or new-fml fml))))

(defmethod qe* ((fml expr))
  fml)

(defmethod qe* ((fml exists-expr))
  (let ((fml1 (lift-quantifier fml)))
    (multiple-value-bind (bndngs body)
	(destructure-existential fml1)
      (let ((*bound-variables* (append bndngs *bound-variables*)))
	(declare (special *bound-variables*))
	(qe1 bndngs (qe* body))))))

(defmethod qe* ((fml forall-expr))
  (let ((fml1 (lift-quantifier fml)))
    (multiple-value-bind (bndngs body)
	(destructure-universal fml1)
      (let ((*bound-variables* (append bndngs *bound-variables*)))
	(declare (special *bound-variables*))
	(simplified-negation (qe1 bndngs
				 (simplified-negation (qe* body))))))))

(defun lift-quantifier (fml)
  (let ((stop-types (list *integer* *rational* *real*)))
    (lift-predicates-in-quantifier fml stop-types)))

(defmethod qe* ((fml conjunction))
  (lcopy-conjunction fml (qe* (args1 fml))
		         (qe* (args2 fml))))

(defmethod qe* ((fml disjunction))
  (lcopy-disjunction fml (qe* (args1 fml))
		         (qe* (args2 fml))))

(defmethod qe* ((fml implication))
  (lcopy-implication fml (qe* (args1 fml))
		         (qe* (args2 fml))))

(defmethod qe* ((fml branch))
  (lcopy-branch fml (qe* (condition fml))
		    (qe* (then-part fml))
		    (qe* (else-part fml))))

(defmethod qe* ((fml iff-or-boolean-equation))
  (lcopy-iff fml (qe* (args1 fml))
	         (qe* (args2 fml))))
		
(defmethod qe* ((fml negation))
  (lcopy-negation fml (qe* (args1 fml))))

(defmethod qe1 ((bndngs list) fml)
  (if (null bndngs) fml
      (qe1 (car bndngs)
	      (qe1 (cdr bndngs) fml))))

(defmethod qe1 :around ((bndng bind-decl) fml)
   (if (eliminated? fml bndng) fml
      (call-next-method)))

(defmethod qe1 ((bndng bind-decl) fml)
  (qe-msg "~2%Eliminating ~a:" (id bndng))
  (cond ((tc-eq (type bndng) *boolean*)
	 (qe1-finite-extension bndng (list *false* *true*)))
	((arithmetic? bndng)
	 (dnf-to-fml (qe1-arithmetic bndng fml)))
	(t (qe-abort
	      "~%Elimination of ~a of type ~a not supported"
	      (id bndng) (type bndng)))))

(defun arithmetic? (bndng)
  (let ((type (type bndng)))
    (or (tc-eq type *real*) (tc-eq type *integer*)(tc-eq type *rational*))))

(defun qe1-arithmetic (bndng fml)
  (declare (special *complete*))
  (let* ((dnf (dnf fml *complete*)))
    (qe-msg "~%DNF(~a): ~a" (length dnf) dnf)
    (if (in-scope-of? bndng dnf (floor-operator))
	(multiple-value-bind (new-bndng new-dnf)
	    (eliminate-shielded bndng dnf)
	  (let ((*bound-variables* (adjoin new-bndng *bound-variables*)))
	    (declare (special *bound-variables*))
	    (qe-disjuncts new-bndng new-dnf)))  
      (qe-disjuncts bndng dnf))))

(defun qe-disjuncts (bndng dnf &optional (acc *false-dnf*) (counter 0))
  (if (eq dnf *false-dnf*) acc
    (let ((new-dnf (qe-conjuncts bndng (car dnf) counter)))
      (cond ((eq new-dnf *true-dnf*)
	     *true-dnf*)
	    ((eq new-dnf *false-dnf*)
	     (qe-disjuncts bndng (cdr dnf) acc (1+ counter)))
            (t (let ((newacc (dnf-disjunction new-dnf acc)))
		 (qe-disjuncts bndng (cdr dnf) newacc (1+ counter))))))))

(defun qe-conjuncts (bndng conjuncts counter)
  (declare (special *state*))
  (qe-msg "~%Conjunct ~a:" counter)
  (dp::protecting-dp-state ((*state* *state*))
     (let ((dnf (catch 'unsatisfiable
		  (qe-conjuncts* bndng conjuncts))))
       (if (unsat? dnf) *false-dnf* dnf))))
		       
(defun qe-conjuncts* (bndng conjuncts)
  (declare (special *state*))
  (multiple-value-bind (solved-expr
			lowers uppers diseqns noccurs others)
      (partition-conjuncts bndng conjuncts)
    (assert (eliminated? noccurs bndng))
    (cond (solved-expr
	   (qe-msg "~%Success: ~a = ~a" (id bndng) solved-expr)
	   (dnf-conjuncts (qe-solved-expr bndng solved-expr conjuncts)))
	  ((eliminated? (dp-find bndng *state*) bndng)
	   (let ((solved-expr (dp-find bndng *state*)))
	     (qe-msg "~%Success: ~a = ~a" (id bndng) solved-expr)
	     (dnf-conjuncts (qe-solved-expr bndng solved-expr conjuncts))))
	  ((inconsistent-disequation? diseqns)
	   (qe-msg "~%Success: ~a. Inconsistent disequalities: ~a"
		   (id bndng) diseqns)
	   *false-dnf*)
	  ((not (null others))
	   (qe-abort "~%Fail: ~a occurs in ~a"  (id bndng) (car others)))
	  ((or (null lowers)
	       (null uppers))
	   (qe-msg "~%Success: ~a. Unbounded: ~a"
		   (id bndng) (append lowers uppers))
	   (dnf-conjuncts noccurs))
	  (t (let ((new-ineqs (FME lowers uppers bndng)))
	       (qe-msg "~%FME: ~a ~% ---> ~a"
		       (append lowers uppers) new-ineqs)
	       (cond ((eliminated? (dp-find bndng *state*) bndng)
		      (let ((solved-expr (dp-find bndng *state*)))
			(qe-msg "~%Success: ~a = ~a" (id bndng) solved-expr)
			(dnf-conjuncts
			 (qe-solved-expr bndng solved-expr conjuncts))))
		     ((and (eliminated? new-ineqs bndng)
			   (eliminated? diseqns bndng))
		      (qe-msg "~%Success: ~a" (id bndng))
		      (dnf-conjuncts (append new-ineqs noccurs diseqns)))
		     (t (qe-msg "~%Retry: ~a. Splitting " (id bndng))
		        (let* ((conjuncts (append lowers uppers diseqns))
			       (disjuncts (dnf* conjuncts 'T)))
			  (qe-msg "~%DNF(a): ~a" (length disjuncts) disjuncts)
			  (let ((new-dnf (qe-disjuncts bndng disjuncts)))
			    (cond ((eliminated? new-dnf bndng)
				   (qe-msg "~%Success: ~a" (id bndng))
				   (dnf-conjunction new-dnf
						    (dnf-conjuncts noccurs)))
				  (t (qe-abort "~%Abort: ~a. Incompleteness..."
					       bndng))))))))))))

(defun qe-solved-expr (bndng expr conjuncts)
  (declare (special *state*))
  #+dbg(assert (eliminated? expr bndng))
  (let ((new-conjuncts (substitute-and-simplify conjuncts bndng expr)))
    (if (and (tc-eq (type bndng) *integer*)
	     (not (integer? expr)))
	(let ((integer? (dp-canon (make!-equation expr
						  (make!-floor expr))
				  *state*)))
	  (cond ((tc-eq integer? *true*)
		 (qe-msg "~%Known to be an integer: ~a" expr)
		 new-conjuncts)
		((tc-eq integer? *false*)
		 (throw-unsat "~%Not an integer: ~a" expr))
		(t (qe-msg "~%Adjoining: ~a" integer?)
		   (adjoin integer? new-conjuncts :test #'tc-eq))))
      new-conjuncts)))

;; Fourier Motzkin Elimination for a set of lower and upper inequalities.
;; Newly generated inequations are added to the current *state*, redundant
;; inequations are disregarded, and the exception 'unsatisfiable is raised
;; if an inconsistency has been deteceted. The resulting set of inequalities
;; may still contain the variable bndng if bndng is not an integer and if
;; there is a combination of strict and nonstrict constraints.

(defun FME (lowers uppers bndng)
  (let ((*bndng* bndng))
    (declare (special *bndng*))
    (FME* lowers uppers)))
  
(defun FME* (lowers uppers &optional acc)
  (if (null lowers) (nreverse acc)
      (FME* (cdr lowers) uppers
	   (union (FME1 (car lowers) uppers) acc :test #'tc-eq))))

(defun FME1 (lower uppers &optional acc)
  (declare (special *state*))
  (if (null uppers) (nreverse acc)
      (let ((new-ineq (eliminate-variable lower
					  (car uppers))))
	(if (null new-ineq)
	    (FME1 lower (cdr uppers) (cons lower (cons (car uppers) acc)))
	  (let* ((new-ineq (dp-canon new-ineq *state*))
		 (result (dp::invoke-process (top-translate-to-dc new-ineq) *state*))
		 (newacc (cond ((or (tc-eq new-ineq *true*)
				    (eq result dp::*true*))
				acc)
			       ((or (tc-eq new-ineq *false*)
				    (eq result dp::*false*))
				(throw-unsat "~%Inconsistency: ~a and ~a"
					     lower (car uppers)))
			       (t (adjoin new-ineq acc
					  :test #'tc-eq)))))
	    (FME1 lower (cdr uppers) newacc))))))

(defun eliminate-variable (lower-ineq upper-ineq)
  (declare (special *bndng*))
  (let* ((lower (solved-form lower-ineq *bndng*))
         (upper (solved-form upper-ineq *bndng*)))
    (cond ((and (strict-ineq? lower-ineq)
                (strict-ineq? upper-ineq))
           (if (tc-eq (type *bndng*) *integer*)
               (make!-lt (make!-succ (make!-floor lower))
                         upper)
               (make!-lt lower upper)))
          ((and (nonstrict-ineq? lower-ineq)
                (nonstrict-ineq? upper-ineq))
           (make!-le lower upper))
	  (t (if (tc-eq (type *bndng*) *integer*)
		 (cond ((and (strict-ineq? lower-ineq)
			     (nonstrict-ineq? upper-ineq))
			(make!-le (make!-succ lower) upper))
		       ((and (nonstrict-ineq? lower-ineq)
			     (strict-ineq? upper-ineq))
			(make!-lt (make!-succ lower) upper))
		       (t (error "unreachable")))
	       nil))))) ; could not eliminate
     
(defun solved-form (expr bndng)
  (assert (solved-form? expr bndng))
  (args2 expr))

(defmethod eliminated? ((expr expr) bndng)
  (not (occurs-in bndng expr)))

(defmethod eliminated? ((exprs null) bndng)
  (declare (ignore bndng))
  'T)

(defmethod eliminated? ((exprs cons) bndng)
  (and (eliminated? (car exprs) bndng)
       (eliminated? (cdr exprs) bndng)))
	      
(defun partition-conjuncts (bndng conjuncts)
  (declare (special *state*))
  (let ((solved-expr nil)
	(lowers nil) (uppers nil) (diseqns nil) (noccurs nil) (others nil))
    (loop for fml in conjuncts
       do (let ((res (top-translate-to-dc fml))) ; (dp::invoke-process (top-translate-to-dc fml) *state*)))
	    (cond ((eq res dp::*false*)
		   (throw-unsat "~%Inconsistency: ~a" fml))
		  ((eq res dp::*true*)
		   nil) ; skip
		  (t (cond ((not (occurs-in bndng fml))
			    (push fml noccurs))
			   ((or (disequation? fml)
				(and (negation? fml) (equation? (args1 fml))))
			    (if (inconsistent-disequation? fml)
				(throw-unsat "~%Inconsistency: ~a" fml)
			      (push fml diseqns)))
			   ((negation? fml)
			    (push fml others))
			   (t (let ((new-fml (solve-for fml bndng)))
				(cond ((tc-eq new-fml *true*)
				       nil)  ; skip
				      ((tc-eq new-fml *false*)
				       (throw-unsat "~%Inconsistency: ~a" fml))
				      ((equation? new-fml)
				       (if (solved-form? new-fml bndng)
					   (return-from partition-conjuncts
					     (args2 new-fml))
					 (push new-fml others)))
				      ((solved-form? new-fml bndng)
				       (let ((op (operator new-fml)))
					 (cond ((or (tc-eq op (less-operator))
						    (tc-eq op (lesseq-operator)))
						(push new-fml uppers))
					       ((or (tc-eq op (greater-operator))
						    (tc-eq op (greatereq-operator)))
						(push new-fml lowers))
					       (t (push new-fml others)))))
				      (t (push new-fml others))))))))))
       (values solved-expr
	       lowers uppers diseqns noccurs others)))

(defmethod solved-form? ((expr application) bndng)
  (and (= (length (arguments expr)) 2)
       (name-expr? (args1 expr))
       (tc-eq (declaration (args1 expr)) bndng)
       (not (occurs-in bndng (args2 expr)))))
 
(defmethod solved-form? ((expr expr) bndng)
  (declare (ignore bndng))
  nil)

;; Elimination of quantifiers over types for which
;; a finite extension has been computed.

(defun qe1-finite-extension (bndng exprs)
   (qe-msg "~%Elimination of ~a by forming its extension" (length exprs))
   (qe1-finite-extension* bndng exprs))

(defun qe1-finite-extension* (bndng exprs &optional (acc *false*))
  (if (null exprs) acc
      (qe1-finite-extension* bndng (cdr exprs)
				(simplified-disjunction (car exprs) acc))))

;; Tries to eliminate occurrences of binding bndng from the scope of
;; some floors in the disjunctive normal form dnf. If successful,
;; it returns a new binding and a modified dnf; this new dnf is
;; guaranteed not to contain the original binding anymore.
;; Otherwise, an 'unable exception is raised.

(defun eliminate-shielded (bndng dnf)
  (qe-msg "~%Eliminate ~a from floor(s)" (id bndng))
  (let*  ((*bndng*  bndng)
	  (*new-bndng* (fresh-bind-decl *bndng*))
	  (*bound-variables* (cons *new-bndng* *bound-variables*)))
    (declare (special *bound-variables)
	     (special *bndng*)
	     (special *new-bndng*))
    (let ((elim-dnf (eliminate-shielded* dnf)))
      (cond ((or (not (eliminated? elim-dnf *bndng*))
		 (in-scope-of? *new-bndng* elim-dnf (floor-operator)))
	     (qe-abort "~%Failed to eliminate ~a from floor" *bndng*))
	    (t
	     (qe-msg "~%  --> ~a" elim-dnf)
	     (values *new-bndng* elim-dnf))))))

(defun fresh-bind-decl (bndng)
  (let ((type (type bndng))
	(newid (intern (format nil "~a~a" (id bndng) (gensym)))))
    (mk-bind-decl newid type type)))
   
(defun eliminate-shielded* (dnf &optional (acc *false-dnf*))
  (if (null dnf)
      (nreverse acc)
    (let ((new-dnf (eliminate-shielded1 (car dnf))))
      (if (eq new-dnf *true-dnf*) *true-dnf*
        (eliminate-shielded* (cdr dnf)
			     (dnf-disjunction new-dnf acc))))))
				  
(defun eliminate-shielded1 (conjuncts)
  (declare (special *bndng*) (special *new-bndng*))
  (let ((delta (delta *bndng* conjuncts)))
    (cond ((= delta 1)
	   (dnf-conjuncts conjuncts))
	  ((and (integerp delta) (> delta 1))
	   (let* ((*multiple* (make!-times (make!-number-expr delta)
					   (make-variable-expr *new-bndng*))))
	     (declare (special *multiple*))
	     (eliminated-dnf delta conjuncts)))
	  (t (dnf-conjuncts conjuncts)))))

(defun eliminated-dnf (k conjuncts &optional (acc *false-dnf*))
  (declare (special *multiple*))
  (if (= k 0) acc
      (let* ((new-expr (make!-plus *multiple*
				    (make!-number-expr (1- k))))
	     (new-conjuncts (catch 'unsatisfiable
			      (eliminated-conjuncts new-expr conjuncts)))      
	     (newacc (if (unsat? new-conjuncts) acc
			 (dnf-disjunction (dnf-conjuncts new-conjuncts)
					  acc))))
	(eliminated-dnf (1- k) conjuncts newacc))))

(defun eliminated-conjuncts (new-expr conjuncts &optional acc)
  (declare (special *bndng*))
  (if (null conjuncts) (nreverse acc)
      (let* ((new-conjunct (substitute-and-simplify
			      (car conjuncts) *bndng* new-expr))
	     (newacc (if (tc-eq new-conjunct *true*) acc
			 (adjoin new-conjunct acc :test #'tc-eq))))
	(eliminated-conjuncts new-expr (cdr conjuncts) newacc))))           

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

;; Solving, relative to the current state *state*,
;; an expression for the variable specified by the binding bndng.
;; Throws exception 'unsatisfiable with value nil if an inconsistency
;; is detected, and exception 'unable if some other error has been
;; detected.

(defun solve-for (expr bndng)
  (declare (special *state*))
  #+dbg(assert (not (negation? expr)))
  #+dbg(assert (typep bndng 'bind-decl))
  (let* ((trm (top-translate-to-dc expr))
	 (canonized-trm (dp::canon trm *state*))
	 (x (top-translate-to-dc (make-variable-expr bndng))))
    (cond ((dp::false-p canonized-trm)
	   (throw-unsat "~%Inconsistency: ~a" expr))
	  ((dp::true-p canonized-trm)
	   *true*)
	  (t (if (and (dp::occurs-p x trm)
		      (not (dp::occurs-in-scope-of-uninterp-p x trm)))
		 (let ((solved-trm (dp::normineq canonized-trm *state* x)))
		   (unless (dp::well-formed-node-p solved-trm)
		     (qe-abort "Term ~a may contain nonlinearities"
				  (translate-from-dc canonized-trm)))
		   (translate-from-dc solved-trm))
		 expr)))))

;; Substitute expr1 for bndng in an expression or
;; a list of expressions; the resulting expressions are
;; normalized relative to some given context *state*.
;; If one of the expressions reduces to *false* then
;; the exception 'unsatisfiable is raised.

(defun substitute-and-simplify (expr bndng expr1)
  (assert (or (expr? expr) (and (listp expr) (every #'expr? expr))))
  (let ((*subst* (acons bndng expr1 nil)))
    (declare (special *subst*))
    (substitute-and-simplify* expr)))
	  
(defmethod substitute-and-simplify* ((expr expr))
  (declare (special *subst*)
	   (special *state*))
  (let ((new-expr (dp-canon (substit expr *subst*) *state*)))
    (if (tc-eq new-expr *false*)
	(throw-unsat "~%Inconsistency: (~a)[~a := ~a]"
		     expr (id (caar *subst*)) (cdar *subst*))
      new-expr)))

(defmethod substitute-and-simplify* ((exprs null))
  nil)

(defmethod substitute-and-simplify* ((exprs cons))
  (let ((expr1 (substitute-and-simplify* (car exprs))))
    (if (tc-eq expr1 *true*)
	(substitute-and-simplify* (cdr exprs))
      (cons expr1 (substitute-and-simplify* (cdr exprs))))))

;; Check if a disequation or a list of disequalities
;; (interpreted as a conjunction) is inconsistent
;; relative to some given context *state*.

(defmethod inconsistent-disequation? ((expr disequation))
  (declare (special *state*))
  (tc-eq (dp-canon (args1 expr) *state*)
	 (dp-canon (args2 expr) *state*)))

(defmethod inconsistent-disequation? ((expr negation))
  (declare (special *state*))
  (and (equation? (argument expr))
       (tc-eq (dp-canon (args1 (argument expr)) *state*)
	      (dp-canon (args2 (argument expr)) *state*))))

(defmethod inconsistent-disequation? ((exprs null))
  nil)

(defmethod inconsistent-disequation? ((exprs cons))
  #+dbg(assert (every #'expr? exprs))
  (or (inconsistent-disequation? (car exprs))
      (inconsistent-disequation? (cdr exprs))))







