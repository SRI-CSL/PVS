(in-package :pvs)

;; Operations on disjunctive normal forms

(defvar *false-dnf* '())
(defvar *true-dnf* '(()))

(defun dnf-atom (expr)
  (list (list expr)))

(defun dnf-insert (cs1 dnf)
  (if (null dnf) (list cs1)
     (let ((cs2 (car dnf)))
       (cond ((subsetp cs1 cs2 :test #'tc-eq)
	      (cons cs1 (cdr dnf)))
	     ((subsetp cs2 cs1 :test #'tc-eq)
	      dnf)
	     (t (cons cs2 (dnf-insert cs1 (cdr dnf))))))))

(defun dnf-disjunction (dnf1 dnf2)
  (cond ((or (eq dnf1 *true-dnf*) (eq dnf2 *true-dnf*))
	 *true-dnf*)
	((eq dnf1 *false-dnf*)
	 dnf2)
	((eq dnf2 *false-dnf*)
	 dnf1)
	(t (dnf-insert (car dnf1)
		       (dnf-disjunction (cdr dnf1) dnf2)))))

(defun dnf-conjunction (dnf1 dnf2)
  (cond ((not (eq dnf1 *false-dnf*))
	 (dnf-disjunction (dnf-conjunction1 (car dnf1) dnf2)
			  (dnf-conjunction (cdr dnf1) dnf2)))
	((not (eq dnf2 *false-dnf*))
	 (dnf-disjunction (dnf-conjunction1 (car dnf2) dnf1)
			  (dnf-conjunction (cdr dnf2) dnf1)))
	(t *false-dnf*)))

(defun dnf-conjunction1 (c1 dnf2 &optional acc)
  (if (null dnf2)
      (nreverse acc)
      (let* ((newc (catch 'unsatisfiable
		     (union-conjunctions c1 (car dnf2))))
	     (newacc (if (eq newc :unsat) acc
			 (adjoin-conjunction newc acc))))
	(dnf-conjunction1 c1 (cdr dnf2) newacc))))

(defun dnf-negation (dnf)
  (if (null dnf) *true-dnf*
      (let ((neg-conjuncts (mapcar #'(lambda (fml)
				       (list (simplified-negation fml)))
			     (car dnf))))
	(dnf-conjunction neg-conjuncts
	                 (dnf-negation (cdr dnf))))))

(defun union-conjunctions (cs1 cs2)
  (union cs1 cs2 :test #'(lambda (c1 c2)
			   (if (complementary? c1 c2)
			       (throw 'unsatisfiable :unsat)
			     (tc-eq c1 c2)))))

(defun adjoin-conjunction (cs dnf)
  (if (complementary-pair? cs) dnf
      (adjoin cs dnf :test #'conjuncts=)))
  
(defun conjuncts= (cs1 cs2)
  (and (subsetp cs1 cs2 :test #'tc-eq)
       (subsetp cs2 cs1 :test #'tc-eq)))

(defun dnf-conjuncts (cs)
  "Compute a dnf with one reduced set of conjunctions"
   (list (reduced-conjuncts cs)))

(defun reduced-conjuncts (cs &optional acc)
  (if (null cs) acc
     (let ((c (car cs)))
       (cond ((tc-eq c *false*)
	      (throw 'unsatisfiable :unsat))
	     ((tc-eq c *true*)
	      (reduced-conjuncts (cdr cs) acc))
	     ((some #'(lambda (a)
			(complementary? c a))
		    acc)
	      (qe-msg "~%Inconsistency: ~a" cs)
	      (throw 'unsatisfiable :unsat))
	     (t
	      (reduced-conjuncts (cdr cs)
				 (adjoin c acc :test #'tc-eq)))))))
		    

;; Computing the disjunctive normal form (DNF)

(defun dnf* (fmls &optional (split-atoms? T) (acc *true-dnf*))
  #+dbg(assert (listp fmls) (every #'expr? fmls))
  (if (null fmls) acc
      (let* ((dnf (dnf (car fmls) split-atoms?))
	     (newacc (dnf-conjunction dnf acc)))
	(dnf* (cdr fmls) split-atoms? newacc))))

(defun dnf (fml &optional (split-atoms? T))
  #+dbg(assert (expr? fml))
  (let ((*split-atoms* split-atoms?))
    (declare (special *split-atoms*))
    (dnf+ fml)))

(defmethod dnf+ ((fml expr))
  (dnf-atom fml))

(defmethod dnf+ ((fml disjunction))
  (dnf-disjunction (dnf+ (args1 fml))
	           (dnf+ (args2 fml))))

(defmethod dnf+ ((fml conjunction))
  (dnf-conjunction (dnf+ (args1 fml))
		   (dnf+ (args2 fml))))

(defmethod dnf+ ((fml implication))
  (dnf-disjunction (dnf- (args1 fml))
	           (dnf+ (args2 fml))))

(defmethod dnf+ ((fml branch))
  (dnf-disjunction (dnf-conjunction (dnf+ (condition fml))
				    (dnf+ (then-part fml)))
		   (dnf-conjunction (dnf- (condition fml))
				    (dnf+ (else-part fml)))))

(defmethod dnf+ ((fml iff-or-boolean-equation))
  (dnf-conjunction (dnf-disjunction (dnf- (args1 fml))
				    (dnf+ (args2 fml)))
		   (dnf-disjunction (dnf- (args2 fml))
				    (dnf+ (args1 fml)))))

(defmethod dnf+ ((fml negation))
  (declare (special *split-atoms*))
  (let ((arg (args1 fml)))
    (if (and *split-atoms*
	     (equation? arg)
	     (and (real? (args1 arg))
		  (real? (args2 arg))))
	(dnf-disjunction (dnf-atom (make!-lt* (argument arg)))
			 (dnf-atom (make!-gt* (argument arg))))
	(dnf- arg))))

(defmethod dnf- ((fml disjunction))
  (dnf-conjunction (dnf- (args1 fml))
		   (dnf- (args2 fml))))

(defmethod dnf- ((fml conjunction))
  (dnf-disjunction (dnf- (args1 fml))
		   (dnf- (args2 fml))))

(defmethod dnf- ((fml implication))
  (dnf-conjunction (dnf+ (args1 fml))
		   (dnf- (args2 fml))))


(defmethod dnf- ((fml branch))
  (dnf-conjunction (dnf-disjunction (dnf- (condition fml))
				    (dnf- (then-part fml)))
		   (dnf-disjunction (dnf+ (condition fml))
				    (dnf- (else-part fml)))))
  
(defmethod dnf- ((fml iff-or-boolean-equation))
  (dnf-disjunction (dnf-conjunction (dnf+ (args1 fml))
				    (dnf- (args2 fml)))
		   (dnf-conjunction (dnf+ (args2 fml))
				    (dnf- (args1 fml)))))
 
(defmethod dnf- ((fml negation))
  (dnf+ (args1 fml)))

(defmethod dnf+ ((expr disequation))
  (declare (special *split-atoms*))
  (let ((arg (argument expr)))
    (if (and *split-atoms*
	     (and (real? (args1 expr))
		  (real? (args2 expr))))
	(dnf-disjunction (dnf-atom (make!-lt* arg))
			 (dnf-atom (make!-gt* arg)))
     (dnf-atom (make!-disequation* arg)))))

(defmethod dnf+ ((expr application))
  (declare (special *split-atoms*))
  (if *split-atoms*
      (let ((op (operator expr))
	    (arg (argument expr)))
	(cond ((tc-eq op (lesseq-operator))
	       (dnf-disjunction (dnf-atom (make!-lt* arg))
				(dnf-atom (make!-equation* arg))))
	      ((tc-eq op (greatereq-operator))             
	       (dnf-disjunction (dnf-atom (make!-gt* arg))
				(dnf-atom (make!-equation* arg))))
	      (t (dnf-atom expr))))
    (call-next-method)))

(defmethod dnf- ((expr expr))
  (cond ((tc-eq expr *true*)
	 *false-dnf*)
	((tc-eq expr *false*)
	 *true-dnf*)
	(t (dnf-atom (make!-negation expr)))))

(defmethod dnf- ((expr equation))
  (declare (special *split-atoms*))
  (let ((arg (argument expr)))
    (if (and *split-atoms*
	     (and (real? (args1 expr))
		  (real? (args2 expr))))
	(dnf-disjunction (dnf-atom (make!-lt* arg))
			 (dnf-atom (make!-gt* arg)))
     (dnf-atom (make!-disequation* arg)))))

(defmethod dnf- ((expr disequation))
  (list (list (make!-equation* (argument expr)))))

(defmethod dnf- ((expr application))
  (declare (special *split-atoms*))
  (let ((op (operator expr))
	(arg (argument expr)))
    (cond ((tc-eq op (lesseq-operator))
	   (dnf-atom (make!-gt* arg)))
	  ((tc-eq op (greatereq-operator))
	   (dnf-atom (make!-lt* arg)))
	  (t (cond ((tc-eq op (less-operator))
		    (if *split-atoms*
			(dnf-disjunction (dnf-atom (make!-gt* arg))
					 (dnf-atom (make!-equation* arg)))
		      (dnf-atom (make!-ge* arg))))
		   ((tc-eq op (greater-operator))
		    (if *split-atoms*
			(dnf-disjunction (dnf-atom (make!-lt* arg))
					 (dnf-atom (make!-equation* arg)))
		      (dnf-atom (make!-le* arg))))
		   (t (call-next-method)))))))

;; Translating a DNF to a Formula

(defun dnf-to-fml (dnf)
  (disjuncts-to-fml (mapcar #'conjuncts-to-fml dnf)))

(defun disjuncts-to-fml (disjuncts &optional (acc *false*))
  (if (null disjuncts) acc
      (disjuncts-to-fml (cdr disjuncts)
			(cond ((tc-eq (car disjuncts) *false*) acc)
			      ((tc-eq (car disjuncts) *true*) *true*)
			      ((tc-eq acc *false*) (car disjuncts))
			      ((tc-eq acc *true*) *true*)
			      (t (simplified-disjunction (car disjuncts) acc))))))

(defun conjuncts-to-fml (conjuncts &optional (acc *true*))
  (if (null conjuncts) acc
      (conjuncts-to-fml (cdr conjuncts)
			(cond ((tc-eq (car conjuncts) *true*) acc)
			      ((tc-eq (car conjuncts) *false*) *false*)
			      ((tc-eq acc *true*) (car conjuncts))
			      ((tc-eq acc *false*) *false*)
			      (t (simplified-conjunction (car conjuncts) acc))))))

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










