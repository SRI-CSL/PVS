;;; Computing the disjunctive normal form (DNF)

(in-package :pvs)

(defvar *false-dnf* '())
(defvar *true-dnf* '(()))

(defun dnf (fml)
  (dnf+ fml))

(defmethod dnf+ ((fml expr))
  (list (list fml)))

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
  (let ((arg (args1 fml)))
    (if (and (equation? arg)
	     (subtype-of? (type (argument arg)) (number-cross-number)))
	(list (list (make!-lt* (argument arg)))
	      (list (make!-gt* (argument arg))))
	(dnf- arg))))
      
(defmethod dnf- ((fml expr))
  (list (list (make!-negation fml))))

(defmethod dnf- ((fml disjunction))
  (dnf-conjunction (dnf- (args1 fml))
		   (dnf- (args2 fml))))

(defmethod dnf- ((fml conjunction))
  (dnf-disjunction (dnf- (args1 fml))
		   (dnf- (args1 fml))))

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

(defun dnf-disjunction (dnf1 dnf2)
  (union dnf1 dnf2 :test #'conjuncts=))

(defun conjuncts= (cs1 cs2)
  (not (set-difference cs1 cs2 :test #'tc-eq)))

(defun dnf-conjunction (dnf1 dnf2 &optional acc)
  (if (null dnf1) (nreverse acc)
      (dnf-conjunction (cdr dnf1) dnf2
		       (union (dnf-conjunction1 (car dnf1) dnf2)
			      acc
			      :test #'conjuncts=))))

(defun dnf-conjunction1 (c1 dnf2 &optional acc)
  (if (null dnf2)
      (nreverse acc)
      (let ((newc (union c1 (car dnf2) :test #'tc-eq)))
	(dnf-conjunction1 c1 (cdr dnf2) (adjoin newc acc :test #'conjuncts=)))))

(defmethod dnf+ ((expr disequation))
  (let ((arg (argument expr)))
    (if (subtype-of? (find-supertype (type arg)) (number-cross-number))
	(list (list (make!-lt* arg))
	      (list (make!-gt* arg)))
     (list (list (make!-disequation* arg))))))

(defmethod dnf+ ((expr application))
  (let ((op (operator expr))
	(arg (argument expr)))
    (cond ((tc-eq op (lesseq-operator))
	   (list (list (make!-lt* arg))
		 (list (make!-equation* arg))))
	  ((tc-eq op (greatereq-operator))              
	   (list (list (make!-gt* arg))
		 (list (make!-equation* arg))))
	  (t (call-next-method)))))

(defmethod dnf- ((expr expr))
  (cond ((tc-eq expr *true*)
	 *false-dnf*)
	((tc-eq expr *false*)
	 *true-dnf*)
	(t
	 (list (list
	  (make!-negation expr))))))

(defmethod dnf- ((expr equation))
  (let ((arg (argument expr)))
    (if (subtype-of? (find-supertype (type arg)) (number-cross-number))
	(list (list (make!-lt* arg))
	      (list (make!-gt* arg)))
     (list (list (make!-disequation* arg))))))

(defmethod dnf- ((expr disequation))
  (list (list (make!-equation* (argument expr)))))

(defmethod dnf- ((expr application))
  (let ((op (operator expr))
	(arg (argument expr)))
    (cond ((tc-eq op (less-operator))         
	   (list (list (make!-gt* arg))
		 (list (make!-equation* arg))))
	  ((tc-eq op (greater-operator))              
	   (list (list (make!-lt* arg))
		 (list (make!-equation* arg))))
	  ((tc-eq op (lesseq-operator))
	   (list (list (make!-gt* arg))))
	  ((tc-eq op (greatereq-operator))
	   (list (list (make!-lt* arg))))
	  (t
	   (call-next-method)))))

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
			      (t (make!-disjunction (car disjuncts) acc))))))

(defun conjuncts-to-fml (conjuncts &optional (acc *true*))
  (if (null conjuncts) acc
      (conjuncts-to-fml (cdr conjuncts)
			(cond ((tc-eq (car conjuncts) *true*) acc)
			      ((tc-eq (car conjuncts) *false*) *false*)
			      ((tc-eq acc *true*) (car conjuncts))
			      ((tc-eq acc *false*) *false*)
			      (t (make!-conjunction (car conjuncts) acc))))))
