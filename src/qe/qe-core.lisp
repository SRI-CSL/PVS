(in-package :dp)

(defun qe (x trm *state* *integer-pred*)
  (declare (special *state*)
	   (special *integer-pred*))
  (catch 'unable
    (let ((kind (node-type x)))
      (if (or (eq kind *integer*)
	      (eq kind *number*))
	  (qe1-arithmetic x trm)
	  (qe1 x trm)))))
	
(defun qe1-arithmetic (x fml)
  (let* ((dnf (dnf fml nil)))
     (if (or (in-scope-of? x dnf *floor*)
	     (in-scope-of? x dnf *integer-pred*))
	 (multiple-value-bind (x1 dnf1)
	     (eliminate-shielded x dnf)
	   (qe-disjuncts x1 dnf1))  
	 (qe-disjuncts x dnf))))

(defun qe1 (x trm)
  (qe-disjuncts x (dnf trm nil)))  

(defun qe-disjuncts (x dnf &optional (acc *false-dnf*))
  (if (eq dnf *false-dnf*) acc
    (let ((new-dnf (qe-conjuncts x (car dnf))))
      (cond ((eq new-dnf *true-dnf*)
	     *true-dnf*)
	    ((eq new-dnf *false-dnf*)
	     (qe-disjuncts x (cdr dnf) acc))
            (t (let ((newacc (dnf-disjunction new-dnf acc)))
		 (qe-disjuncts x (cdr dnf) newacc)))))))

(defun qe-conjuncts (x conjuncts)
  (declare (special *state*))
  (nprotecting-cong-state (*state* *state*)
     (catch 'fastexit
       (qe-conjuncts* x conjuncts *state*))))
		       
(defun qe-conjuncts* (x conjuncts *state*)
  (declare (special *state*))
  (multiple-value-bind (solved-expr
			lowers uppers diseqns noccurs others)
      (partition-conjuncts x conjuncts)
    (assert (eliminated? noccurs x))
    (cond (solved-expr
	   (dnf-conjuncts (qe-solved-expr x solved-expr conjuncts)))
	  ((eliminated? (signature x *state*) x)
	   (let ((solved-expr (signature x *state*)))
	     (dnf-conjuncts (qe-solved-expr x solved-expr conjuncts))))
	  ((some #'(lambda (diseq)
		     (inconsistent-disequality-p diseq *state*))
		 diseqns)
	   *false-dnf*)
	  ((not (null others))
	   (throw 'unable :unable))
	  ((or (null lowers)
	       (null uppers))
	   (dnf-conjuncts noccurs))
	  (t (let ((new-ineqs (FME lowers uppers x)))
	       (cond ((eliminated? (signature x *state*) x)
		      (let ((solved-expr (signature x *state*)))
			(dnf-conjuncts
			 (qe-solved-expr x solved-expr conjuncts))))
		     ((and (eliminated? new-ineqs x)
			   (eliminated? diseqns x))
		      (dnf-conjuncts (append new-ineqs noccurs diseqns)))
		     (t (let* ((conjuncts (append lowers uppers diseqns))
			       (disjuncts (dnf* conjuncts 'T)))
			  (let ((new-dnf (qe-disjuncts x disjuncts)))
			    (cond ((eliminated? new-dnf x)
				   (dnf-conjunction new-dnf
						    (dnf-conjuncts noccurs)))
				  (t (throw 'unable :unable))))))))))))

(defun qe-solved-expr (x expr conjuncts)
  (declare (special *state*))
  #+dbg(assert (eliminated? expr x))
  (let ((new-conjuncts (conjuncts-subst conjuncts x expr *state*)))
    (cond ((false-p new-conjuncts)
	   (throw 'fastexit *false-dnf*))
          ((and (eq (node-type x) *integer*)
		(not (eq (node-type expr) *integer*)))
	   (let ((integer? (mk-integer-pred expr *state*)))
	     (cond ((true-p integer?)
		    new-conjuncts)
		   ((false-p integer?)
		    (throw 'fastexit *false-dnf*))
		   (t (adjoin integer? new-conjuncts)))))
	  (t new-conjuncts))))

(defun mk-integer-pred (expr state)
  (let ((ceqn (canon (mk-equality expr (mk-floor expr))
		     *state*)))
    ceqn))
  ;  (if (or (true-p ceqn) (false-p ceqn)) ceqn
  ;     (mk-term (list *integer-pred* expr)))))

;; Fourier Motzkin Elimination for a set of lower and upper inequalities.
;; Newly generated inequations are added to the current *state*, redundant
;; inequations are disregarded, and the exception 'unsatisfiable is raised
;; if an inconsistency has been deteceted. The resulting set of inequalities
;; may still contain the variable bndng if bndng is not an integer and if
;; there is a combination of strict and nonstrict constraints.

(defun FME (lowers uppers *x*)
  (declare (special *x*))
  (FME* lowers uppers))
  
(defun FME* (lowers uppers &optional acc)
  (if (null lowers) (nreverse acc)
      (FME* (cdr lowers) uppers
	   (union (FME1 (car lowers) uppers) acc))))

(defun FME1 (lower uppers &optional acc)
  (declare (special *state*))
  (if (null uppers) (nreverse acc)
      (let ((new-ineq (eliminate-variable lower
					  (car uppers))))
	(if (null new-ineq)
	    (FME1 lower (cdr uppers) (cons lower (cons (car uppers) acc)))
	  (let* ((result (invoke-process new-ineq *state*))
		 (newacc (cond ((true-p result)
				acc)
			       ((false-p result)
				(throw 'fastexit *false-dnf*))
			       (t (adjoin new-ineq acc)))))
	    (FME1 lower (cdr uppers) newacc))))))

(defun eliminate-variable (lower-ineq upper-ineq)
  (declare (special *x*))
  (let* ((lower (solved-form lower-ineq *x*))
         (upper (solved-form upper-ineq *x*)))
    (cond ((and (strict-ineq-p lower-ineq)
                (strict-ineq-p upper-ineq))
           (if (eq (node-type *x*) *integer*)
               (mk-less (mk-plus (list (mk-floor lower) *one*))
                         upper)
               (mk-less lower upper)))
          ((and (nonstrict-ineq-p lower-ineq)
                (nonstrict-ineq-p upper-ineq))
           (mk-lesseq lower upper))
	  (t (if (eq (node-type *x*) *integer*)
		 (cond ((and (strict-ineq-p lower-ineq)
			     (nonstrict-ineq-p upper-ineq))
			(mk-lesseq (mk-plus (list lower *one*)) upper))
		       ((and (nonstrict-ineq-p lower-ineq)
			     (strict-ineq-p upper-ineq))
			(mk-less (mk-plus (list lower *one*)) upper))
		       (t (error "unreachable")))
	       nil))))) ; could not eliminate
     
(defun solved-form (expr x)
  (assert (solved-form? expr x))
  (rhs expr))

(defun eliminated? (expr x)
  (cond ((node-p expr)
	 (not (occurs-p x expr)))
	((listp expr)
	 (every #'(lambda (trm)
		    (eliminated? trm x))
		expr))
	(t (break))))

(defun partition-conjuncts (x conjuncts)
  (declare (special *state*))
  (let ((solved-expr nil)
	(lowers nil) (uppers nil) (diseqns nil) (noccurs nil) (others nil))
    (loop for fml in conjuncts
       do (let ((res (canon fml *state*)))
	    (cond ((false-p res)
		   (throw 'fastexit *false-dnf*))
		  ((true-p res)
		   nil) ; skip
		  (t (cond ((not (occurs-p x fml))
			    (push fml noccurs))
			   ((disequality-p fml)
			    (if (inconsistent-disequality-p fml *state*)
				(throw 'fastexit *false-dnf*)
			      (push fml diseqns)))
			   ((negation-p fml)
			    (push fml others))
			   (t (let ((sfml (solve-for fml x *state*)))
				(cond ((true-p sfml)
				       nil)  ; skip
				      ((false-p sfml)
				       (throw 'fastexit *false-dnf*))
				      ((equality-p sfml)
				       (if (solved-form? sfml x)
					   (return-from partition-conjuncts
					     (rhs sfml))
					 (push sfml others)))
				      ((solved-form? sfml x)
				       (cond ((or (less-p sfml)
						  (lesseq-p sfml))
					      (push sfml uppers))
					     ((or (greater-p sfml)
						  (greatereq-p sfml))
					      (push sfml lowers))
					     (t (push sfml others))))
				      (t (push sfml others))))))))))
       (values solved-expr
	       lowers uppers diseqns noccurs others)))

(defun solved-form? (trm x)
  (and (application-p trm)
       (= (length (funargs trm) 2))
       (leaf-p (lhs trm))
       (not (occurs-p x (rhs trm)))))

;; Elimination of quantifiers over types for which
;; a finite extension has been computed.

(defun qe1-finite-extension (x exprs)
   (qe1-finite-extension* x exprs))

(defun qe1-finite-extension* (x exprs &optional (acc *false*))
  (if (null exprs) acc
      (qe1-finite-extension* x (cdr exprs)
			     (simplified-disjunction (car exprs) acc)))) ;???

;; Tries to eliminate occurrences of binding bndng from the scope of
;; some floors in the disjunctive normal form dnf. If successful,
;; it returns a new binding and a modified dnf; this new dnf is
;; guaranteed not to contain the original binding anymore.
;; Otherwise, an 'unable exception is raised.

(defun eliminate-shielded (*x* dnf) 
  (declare (special *x*))
  (let ((*new-x* (fresh-bind-decl *x*)))
    (declare (special *new-x*))
    (let ((elim-dnf (eliminate-shielded* dnf)))
      (cond ((or (not (eliminated? elim-dnf *x*))
		 (in-scope-of? *new-x* elim-dnf *floor*)
		 (in-scope-of? *new-x* elim-dnf *integer-pred*))
	     (throw 'unable :unable))
	    (t (values *new-x* elim-dnf))))))

(defun fresh-bind-decl (x)
  (let* ((type (node-type x))
	 (newid (intern (format nil "~a~a" (leaf-id x) (gensym))))
	 (y (mk-variable newid type)))
    (setf (node-type y) (node-type x))
    (when pvs::*use-translate-from-dc-hash*  ; set stage for back translation
      (pvs::set-inverse-translation y 
	  (pvs::get-inverse-translation x)))
    y))
   
(defun eliminate-shielded* (dnf &optional (acc *false-dnf*))
  (if (null dnf) (nreverse acc)
    (let ((new-dnf (eliminate-shielded1 (car dnf))))
      (if (eq new-dnf *true-dnf*) *true-dnf*
        (eliminate-shielded* (cdr dnf)
			     (dnf-disjunction new-dnf acc))))))
				  
(defun eliminate-shielded1 (conjuncts)
  (declare (special *x*) (special *new-x*) (special *state*))
  (let ((delta (delta *x* conjuncts *state*)))
    (cond ((= delta 1)
	   (dnf-conjuncts conjuncts))
	  ((and (integerp delta) (> delta 1))
	   (let ((c (mk-times (list (mk-constant delta) *new-x*))))
	     (eliminated-dnf c delta conjuncts)))
	  (t (dnf-conjuncts conjuncts)))))

(defun eliminated-dnf (c k conjuncts &optional (acc *false-dnf*))
  (declare (special *multiple*))
  (if (= k 0) acc
      (let* ((new-expr (mk-plus (list c (mk-constant (1- k)))))
	     (new-conjuncts (eliminated-conjuncts new-expr conjuncts))      
	     (newacc (if (eq new-conjuncts *false*) acc
			 (dnf-disjunction (dnf-conjuncts new-conjuncts)
					  acc))))
	(eliminated-dnf c (1- k) conjuncts newacc))))

(defun eliminated-conjuncts (trm conjuncts &optional acc)
  (declare (special *x*) (special *state*))
  (if (null conjuncts) (nreverse acc)
      (let ((new-trm (substitute-and-simplify
			      (car conjuncts) *x* trm *state*)))
	(if (false-p new-trm) *false*
	    (let ((newacc (if (true-p new-trm) acc
			      (adjoin new-trm acc))))
	      (eliminated-conjuncts trm (cdr conjuncts) newacc))))))        

;; Least common multiple of the of accumulated coefficients of occurences of
;; bound variable x within the scope of a floor; e.g. 
;;        (delta x (1/2 * [1/5 + [2/3 * x + 1/4]])) = 6
;; Returns nil if there is a non-linear multiplication or no occurrence of x.

(defun delta (x exprs state)
  (let ((coefficients (catch 'nonlinear
			(coefficients* x exprs state))))
    (if coefficients (apply #'lcm coefficients) 1)))

(defun coefficients* (x trms state &optional acc)
  (if (null trms) acc
      (let* ((coeff (coefficients1 x (car trms) state))
	     (newacc (if coeff (union coeff acc) acc)))
	(coefficients* x (cdr trms) state newacc))))

(defun coefficients1 (x trm state)
  (cond ((times-p trm)
	 (cond ((rational-p (lhs trm))
		(multiple-value-bind (numer denom)
		    (destructure-rational (lhs trm))
		  (declare (ignore numer))
		  (cond ((eq (rhs trm) x)
			 (list (constant-id denom)))
			((or (floor-p (rhs trm))
			     (integer-pred-p (rhs trm)))
			 (let ((coeffs (coefficients1 x (arg 1 (rhs trm)) state)))
			   (and coeffs (cons (constant-id denom) coeffs))))
			(t nil))))
	       ((rational-p (rhs trm))
		(multiple-value-bind (numer denom)
		    (destructure-rational (rhs trm))
		  (declare (ignore numer))
		  (cond ((eq (lhs trm) x)
			 (list (constant-id denom)))
			((or (floor-p (lhs trm))
			     (integer-pred-p (lhs trm)))
			 (let ((coeffs (coefficients1 x (arg 1 (lhs trm)) state)))
			   (and coeffs (cons (constant-id denom) coeffs))))
			(t nil)))) 
	       (t nil)))
	((application-p trm)
	 (coefficients* x (funargs trm) state))
	(t nil)))

(defun integer-pred-p (trm)
  (declare (special *integer-pred*))
  (and (application-p trm)
       (eq (funsym trm) *integer-pred*)))

(defun rational-p (trm)
  (or (pos-rational-p trm)
      (neg-rational-p trm)))

(defun pos-rational-p (trm)
  (and (divide-p trm)
       (dp-numberp (lhs trm))
       (dp-numberp (rhs trm))))

(defun neg-rational-p (trm)
  (and (minus-p trm)
       (pos-rational-p (arg 1 trm))))

(defun destructure-rational (trm)
  (if (neg-rational-p trm)
      (multiple-value-bind (a b)
	  (destructure-rational (arg 1 trm))
	(values (mk-minus a) b))
    (values (numer trm) (denom trm))))
	
;; Solving, relative to the current state *state*,
;; an expression for the variable specified by the binding bndng.
;; Throws exception 'unsatisfiable with value nil if an inconsistency
;; is detected, and exception 'unable if some other error has been
;; detected.

(defun solve-for (trm x state)
  #+dbg(assert (not (negation-p trm)))
  #+dbg(assert (leaf-p x))
  #+dbg(assert (cong-state-p state))
  (let ((ctrm (canon trm state 'no-mod)))
    (cond ((false-p ctrm)
	   (throw 'fastexit *false-dnf*))
	  ((true-p ctrm)
	   *true*)
	  (t (cond ((or (not (occurs-p x ctrm))
		        (occurs-in-scope-of-uninterp-p x ctrm))
		    ctrm)
		   ((or (eq (node-type x) *integer*)
			(eq (node-type x) *number*))
		    (let ((strm (normineq ctrm state x)))
		      (if (well-formed-node-p strm) strm
			  (throw 'unable :unable))))
		   (t (let ((seqn (find-if #'(lambda (eqn)
					       (solved-form? x eqn))
				    (solve ctrm state))))       
			  (or seqn ctrm))))))))

;; Substitute trm1 for x in an expression or
;; a list of expressions; the resulting expressions are
;; normalized relative to some given context state.
;; If one of the expressions reduces to *false* then
;; it returns *false*.

(defun conjuncts-subst (trms x trm1 state &optional acc)
  (assert (cong-state-p state))
  (if (null trms) (nreverse acc)
      (let ((strm (substitute-and-simplify (car trms) x trm1
					   state)))
	(if (false-p strm) *false*
	    (let ((newacc (if (true-p strm) acc
			      (cons strm acc))))
	      (conjuncts-subst (cdr trms) x trm1 state newacc))))))

(defun substitute-and-simplify (trm x trm1 state)
  (assert (cong-state-p state))
  (canon (replace-by trm (acons x trm1 nil)) state 'no-mod))
   
;; Check if a disequality or a list of disequalities
;; (interpreted as a conjunction) is inconsistent
;; relative to some given context *state*.

(defun inconsistent-disequality-p (trm state)
  #+dbg(assert (cong-state-p state))
  #+dbg(assert (disequality-p))
  (multiple-value-bind (lhs rhs)
      (destructure-disequality trm)
    (eq (canon lhs state 'no-mod)
	(canon rhs state 'no-mod))))
 
