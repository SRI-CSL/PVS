(in-package :dp)

;; Miscellaneous

(defun mk-integer-pred (trm state)
  (if (integer-p trm) *true*
      (canon (mk-equality trm (mk-floor trm))
	     state)))

(defun integer-pred-p (trm)
  (declare (special *integer-pred*))
  (and (application-p trm)
       (eq (funsym trm) *integer-pred*)))

(defun number-p (trm)
 (let ((kind (node-type trm)))
   (or (eq kind *integer*)
       (eq kind *number*))))

(defun integer-p (trm)
  (eq (node-initial-type trm) *integer*))
	
(defun in-scope-of-p (x trm op &optional in-scope)
  (assert (node-p trm))
  (cond ((leaf-p trm)
	 (and in-scope (eq x trm)))
	((application-p trm)
         (let ((new-in-scope (if (eq (funsym trm) op) 'T
				 in-scope)))
	   (some #'(lambda (arg)
		     (in-scope-of-p x arg op new-in-scope))
		 (funargs trm))))
	(t nil)))

(defun in-scope-of-floor-p (x trm)
  (declare (special *integer-pred*))
  (or (in-scope-of-p x trm *floor*)
      (in-scope-of-p x trm *integer-pred*)))

;; Top-level

(defun qe (x bdd state *integer-pred*)
  "Tries to eliminate x in trm. Returns
   a BDD if successful; otherwise :unable"
  (declare (special *integer-pred*))
  (let ((bdd1 (catch 'unable
		(qe1 x bdd state))))
    (if (or (null bdd1)
	    (bdd-some #'(lambda (trm)
			  (occurs-p x trm))
		      bdd1))
	:unable
	bdd1)))

(defun qe1 (x bdd state)
  (qe-dnf x (bdd-to-dnf bdd state x) state))

(defun qe-dnf (x dnf state)
   (bdd-or* (mapcar #'(lambda (trms)
			(qe-conjunct x trms state))
	      dnf)))

(defun qe-conjunct (x trms state)
  (if (and (integer-p x)
	   (some #'(lambda (trm)
		     (in-scope-of-floor-p x trm))
		 trms))
      (multiple-value-bind (x1 dnf)
	  (eliminate-shielded x trms state)
	(qe-dnf x1 dnf state))
      (qe-conjunct* x trms state)))

(defun qe-conjunct* (x trms state)
  (multiple-value-bind (eqlts diseqlts lowers uppers noccurs others)
      (partition-trms x trms)
    (multiple-value-bind (strm eqlts)
	(solution x eqlts)
      (if strm
	  (let* ((occurs (append eqlts diseqlts lowers uppers others))
		 (elims (subst-and-simplify occurs x strm state))
		 (integer? (if (integer-p x)
			       (bdd-var (mk-integer-pred strm state))
			       (bdd-1))))
	    (bdd-and (conjunct-to-bdd (append elims noccurs))
		     integer?))
	  (cond ((some #'(lambda (diseqlt)
			   (inconsistent-disequality-p diseqlt state))
		       diseqlts)
		 (bdd-0))
		((consp others)
		 (throw 'unable nil))
		((and (null eqlts)  ; unbounded
		      (or (null lowers)	(null uppers)))
		 (conjunct-to-bdd (append noccurs others)))
		(t (let* ((new-ineqlts (FME lowers uppers x state))
			  (new-trms (append new-ineqlts noccurs)))
		     (if (every #'(lambda (ineqlt)
				    (not (occurs-p x ineqlt)))
				new-ineqlts)
			 (conjunct-to-bdd new-trms)
			 (let ((dnf (bool-to-dnf (mk-conjunction* new-trms)
						 state x 'T 'T)))
			   (qe-dnf x dnf state))))))))))

(defun subst-and-simplify (trms x strm state)
  (nprotecting-cong-state (state state)
     (let ((subst (acons x strm nil)))
       (subst-and-simplify* trms subst state))))

(defun subst-and-simplify* (trms subst state &optional acc)
  (if (null trms) (nreverse acc)
      (let ((trm0 (canon (replace-by (car trms) subst) state)))
	(cond ((true-p trm0)
	       (subst-and-simplify* (cdr trms) subst state acc))
	      ((false-p trm0)
	       (list *false*))
	      (t (let ((res (invoke-process trm0 state)))
		   (cond ((true-p res)
			  (subst-and-simplify* (cdr trms) subst state acc))
			 ((false-p res)
			  (list *false*))
			 (t (subst-and-simplify* (cdr trms) subst state
						 (cons trm0 acc))))))))))

(defun partition-trms (x trms)
  (let ((eqlts) (diseqlts) (lowers)
	(uppers) (noccurs) (others))
    (loop for trm in trms
	  do (cond ((not (occurs-p x trm))
		    (push trm noccurs))
	           ((equality-p trm)
		    (push trm eqlts))
		   ((disequality-p trm)
		    (push trm diseqlts))
		   ((or (less-p trm) (lesseq-p trm))
		    (let ((lhs (lhs trm))
			  (rhs (rhs trm)))
		      (cond ((and (eq x lhs) (not (occurs-p x rhs)))
			     (push trm uppers))
			    ((and (eq x rhs) (not (occurs-p x lhs)))
			     (push trm lowers))
			    (t (push trm others)))))
		   ((or (greater-p trm) (greatereq-p trm))
		    (let ((lhs (lhs trm))
			  (rhs (rhs trm)))
		      (cond ((and (eq x lhs) (not (occurs-p x rhs)))
			     (push trm lowers))
			    ((and (eq x rhs) (not (occurs-p x lhs)))
			     (push trm uppers))
			    (t (push trm others)))))
		   (t (push trm others))))
    (values eqlts diseqlts lowers uppers noccurs others)))

(defun solution (x eqlts &optional processed)
  (if (null eqlts)
      (values nil (nreverse processed))
      (let ((lhs (lhs (car eqlts)))
	    (rhs (rhs (car eqlts))))
	(cond ((and (eq x lhs) (not (occurs-p x rhs)))
	       (values rhs (append (cdr eqlts) processed)))
	      ((and (eq x rhs) (not (occurs-p x lhs)))
	       (values lhs (append (cdr eqlts) processed)))
	      (t (solution x (cdr eqlts) (cons (car eqlts) processed)))))))

;; Tries to eliminate occurrences of binding bndng from the scope of
;; some floors in the conjuncts trms. If successful,
;; it returns a new binding and a modified dnf; this new dnf is
;; guaranteed not to contain the original binding anymore.
;; Otherwise, an 'unable exception is raised.

(defun eliminate-shielded (x trms state) 
  (let ((new-x (fresh-bind-decl x)))
    (let ((dnf (eliminate-shielded* x new-x trms state)))
      (cond ((not (dnf-eliminated-p x new-x dnf))
	     (throw 'unable nil))
	    (t (values new-x dnf))))))

(defun dnf-eliminated-p (x y dnf)
  (every #'(lambda (trms)
	     (every #'(lambda (trm)
			(and (not (occurs-p x trm))
			     (not (in-scope-of-floor-p y trm))))
		    trms))
	 dnf))

(defun fresh-bind-decl (x)
  (let* ((type (node-type x))
	 (newid (intern (format nil "~a~a" (leaf-id x) (gensym))))
	 (y (mk-variable newid type)))
    (setf (node-type y) (node-type x))
    (when pvs::*use-translate-from-dc-hash*  ; set stage for back translation
      (pvs::set-inverse-translation y 
	  (pvs::get-inverse-translation x)))
    y))
				  
(defun eliminate-shielded* (x y trms state)
  (let ((delta (delta x trms)))
    (cond ((not (and (integerp delta) (>= delta 0)))
	   (throw 'unable nil))
	  ((= delta 1)
	   (list trms))
	  (t (let* ((c (mk-times (list (mk-constant delta) y)))
		    (disjunct (bdd-or* (eliminate-conjuncts x c delta trms))))
	       (bdd-to-dnf disjunct state y))))))

(defun eliminate-conjuncts (x c k trms &optional acc)
  (if (= k 0)
      (nreverse acc)
      (let* ((e (mk-plus (list c (mk-constant (1- k)))))
	     (ntrms (mapcar #'(lambda (trm)
				   (replace-by trm (acons x e nil)))
			 trms))
	     (newacc (cons (conjunct-to-bdd ntrms) acc)))
	(eliminate-conjuncts x c (1- k) trms newacc))))   

;; Least common multiple of the of accumulated coefficients of occurences of
;; bound variable x within the scope of a floor; e.g. 
;;        (delta x (7/17 * x  + (1/2 * [1/5 + [2/3 * x + 1/4]])) = 6
;; Throws 'unable exception if there is a non-linear multiplication
;; or no occurrence of x.

(defun delta (x trms)
  (let ((coeffs (coefficients* x trms)))
    (if coeffs (apply #'lcm (mapcar #'denominator coeffs)) 1)))

(defun coefficients* (x trms)
  (mapcan #'(lambda (trm) (coefficient1 x trm)) trms))

(defun coefficient1 (x trm)
  (if (negation-p trm)
      (coefficient1 x (arg 1 trm))
      (and (in-scope-of-floor-p x trm)
	   (or (equality-p trm)
	       (disequality-p trm)
	       (and (application-p trm)
		    (arith-pred-p (funsym trm))))
	   (mapcan #'(lambda (arg)
		       (coefficients-plus* x arg))
	     (funargs trm)))))

(defun coefficients-plus* (x trm)
    (cond ((times-p trm)
	   (coefficients-times x (lhs trm) (rhs trm)))
	  ((plus-p trm)
	   (coefficients-plus x (lhs trm) (rhs trm)))
	  ((floor-p trm)
	   (coefficients-plus* x (arg 1 trm)))
	  ((eq x trm)
	   (list 1))
	  (t nil)))

(defun coefficients-plus (x lhs rhs)
  (union (coefficients-plus* x lhs)
	 (coefficients-plus* x rhs)))

(defun coefficients-times (x lhs rhs)
  (cond ((eq x lhs)
	 (let ((val (rational-val rhs)))
	   (if val (list val)
	       (throw 'unable nil))))
	((eq x rhs)
	 (let ((val (rational-val lhs)))
	   (if val (list val)
	       (throw 'unable nil))))
	((floor-p lhs)
	 (let ((val (rational-val rhs))
	       (coeffs (coefficients-plus* x (arg 1 lhs))))
	   (if val (and coeffs (cons val coeffs))
	       (throw 'unable nil))))
	((floor-p rhs)
	 (let ((val (rational-val lhs))
	       (coeffs (coefficients-plus* x (arg 1 rhs))))
	   (if val (and coeffs (cons val coeffs))
	       (throw 'unable nil))))
	(t nil)))

(defun rational-val (trm)
  (or (pos-rational-val trm)
      (neg-rational-val trm)))

(defun pos-rational-val (trm)
  (or (and (divide-p trm)
	   (dp-numberp (lhs trm))
	   (dp-numberp (rhs trm))
	   (/ (numer trm) (denom trm)))
      (and (constant-p trm)
	   (let ((val (constant-id trm)))
	     (and (rationalp val)
		  val)))))

(defun neg-rational-val (trm)
  (and (minus-p trm)
       (let ((val (rational-val (arg 1 trm))))
	 (and val (- val)))))

;; Fourier Motzkin Elimination for a set of lower and upper inequalities.
;; Newly generated inequations are added to the current *state*, redundant
;; inequations are disregarded, and the exception 'unsatisfiable is raised
;; if an inconsistency has been deteceted. The resulting set of inequalities
;; may still contain the variable bndng if bndng is not an integer and if
;; there is a combination of strict and nonstrict constraints.

(defun FME (lowers uppers *x* *state*)
  (declare (special *x*)
	   (special *state*))
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

(defun eliminate-variable (lower upper)
  (declare (special *x*))
  (let* ((lb (lower-bound lower *x*))
         (ub (upper-bound upper *x*)))
    (cond ((and (strict-ineq-p lower)
                (strict-ineq-p upper))
           (if (integer-p *x*)
               (mk-less (mk-plus (list (mk-floor lb) *one*))
                         ub)
               (mk-less lb ub)))
          ((and (nonstrict-ineq-p lower)
                (nonstrict-ineq-p upper))
           (mk-lesseq lb ub))
	  (t (if (integer-p *x*)
		 (cond ((and (strict-ineq-p lower)
			     (nonstrict-ineq-p upper))
			(mk-lesseq (mk-plus (list lb *one*)) ub))
		       ((and (nonstrict-ineq-p lower)
			     (strict-ineq-p upper))
			(mk-less (mk-plus (list lb *one*)) ub))
		       (t (error "unreachable")))
	       nil))))) ; could not eliminate
     
(defun lower-bound (ineq x)
  (or (and (or (less-p ineq) (lesseq-p ineq))
	   (eq x (rhs ineq))
	   (not (occurs-p x (lhs ineq)))
	   (lhs ineq))
      (and (or (greater-p ineq) (greatereq-p ineq))
	   (eq x (lhs ineq))
	   (not (occurs-p x (rhs ineq)))
	   (rhs ineq))))

(defun upper-bound (ineq x)
  (or (and (or (greater-p ineq) (greatereq-p ineq))
	   (eq x (rhs ineq))
	   (not (occurs-p x (lhs ineq)))
	   (lhs ineq))
      (and (or (less-p ineq) (lesseq-p ineq))
	   (eq x (lhs ineq))
	   (not (occurs-p x (rhs ineq)))
	   (rhs ineq))))
