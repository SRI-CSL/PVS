(in-package :dp)

;; A substitution actually is a pair consisting of an ordinary substitution plus a score

(defstruct (score-subst
	    (:print-function
		  (lambda (p s k)
		    (declare (ignore k))
		    (format s "<Score: ~a, ~{~a~^,~}>"
		      (score-subst-score p)
		      (score-subst-subst p)))))
  subst
  score)

(defun empty-score-subst ()
  (make-score-subst :subst nil :score 0))

(defun subst-of (s) (score-subst-subst s))
(defun score-of (s) (score-subst-score s))

(defun score-subst= (score-subst1 score-subst2)
  (and (subst= (subst-of s1) (subst-of s2))
       (equal (score-of s1) (score-of s2))))

;; Top-Level

(defun gensubsts (vars trms)
  "Generate a list of substitutions (for all variables in vars)
   together with scores"
  (let ((*lvars* vars))
    (declare (special *lvars*))
    (multiple-value-bind (ha hn ca cn)
	(flatten () () () () () trms)
      (let ((sequents (split1 ha hn ca cn)))
	(filter-substs
	 (gensubsts-sequents sequents))))))

(defun filter-substs (score-substs)
  (remove-if #'(lambda (score-subst)
		       (incomplete? (subst-of score-subst)))
	  score-substs))
	  
(defun incomplete? (subst)
   (some #'(lambda (x)
	     (or (not (member x (dom subst)))
		 (some #'(lambda (trm)
			   (and trm (occurs-p x trm)))
		       (mapcar #'cdr subst))))
	 *lvars*))

;; Collect all possible substitutions for a given set of sequents

(defun sequent? (seq)
  (and (listp seq)
       (listp (first seq))
       (listp (second seq))
       (every #'node-p (first seq))
       (every #'node-p (second seq))))

(defun gensubsts-sequents (sequents &optional (score-substs
					       (list (empty-score-subst))))
  #+dbg(assert (every #'sequent? sequents))
  (if (null sequents) score-substs
      (gensubsts-sequents (cdr sequents)
			  (destructuring-bind (hyps concs) (car sequents)
			    (let ((*state* (init-dp-state
					    (mapcar #'(lambda (trm)
							(equality-p trm))
					      hyps))))
			      (declare (special *state*))
			      (gensubsts-sequent hyps concs score-substs))))))

(defun gensubsts-sequent (hyps concs score-substs &optional acc)
  #+dbg(assert (every #'node-p hyps))
  #+dbg(assert (every #'node-p concs))
  #+dbg(assert (every #'score-subst-p score-substs))
  (if (null score-substs) (nreverse acc)
      (let* ((subst (subst-of (car score-substs)))
	     (score (score-of (car score-substs)))
	     (ext-score-substs (mapcar #'(lambda (subst)
					   (make-score-subst :score (1+ score)
							     :subst subst))
				 (gensubsts-sequent1 hyps concs subst)))
	     (new-score-substs (mapcar #'(lambda (subst)
					  (make-score-subst :score 1
							    :subst subst))
				 (gensubsts-sequent1 hyps concs nil)))
	     (newacc (add-score-substs new-score-substs
			  (add-score-substs ext-score-substs acc))))
	(gensubsts-sequent hyps concs (cdr score-substs) newacc))))

(defun add-score-substs (score-substs1 score-substs2)
  (if (null score-substs1) score-substs2
      (add-score-substs (cdr score-substs1)
			(add-score-subst (car score-substs1) score-substs2))))

(defun add-score-subst (score-subst score-substs)
  (assert (score-subst-p score-subst))
  (assert (every #'score-subst-p score-substs))
  (if (some #'(lambda (score-subst1)
		(notbetter? score-subst score-subst1))
	  score-substs)
      score-substs
    (cons score-subst score-substs)))
	
(defun notbetter? (score-subst1 score-subst2)
  (assert (score-subst-p score-subst1))
  (assert (score-subst-p score-subst2))
  (and (subst= (subst-of score-subst1) (subst-of score-subst2))
       (<= (score-of score-subst1) (score-of score-subst2))))

(defun gensubsts-sequent1 (hyps concs subst)
  "Given a substitution list all substitutions generated from
   unifying complementary literals (involving variables)"
    (let ((*insts* nil))
      (loop for hyp in hyps
	    do (loop for conc in concs
		     do (when (or (var-occurs-p hyp)
				  (var-occurs-p conc)) 
			  (let ((new-subst (E-unify hyp conc *state* subst))) 
			      (setf *insts* (add-to-subst new-subst *insts*))))))
      *insts*))

(defun var-occurs-p (trm)
  (some #'(lambda (x)
	    (occurs-p x trm))
	*lvars*))

(defun add-to-subst (subst substs)
  (if (or (fail? subst) (some #'(lambda (subst1)
				  (subst= subst subst1))
			      substs))
	  substs
      (cons subst substs)))

;; Splitting

(defun split1 (ha hn ca cn)
  #+dbg(assert (every #'node-p (append ha hn ca cn)))
  (split1* ha hn ca cn))

(defun split1* (ha hn ca cn)
  (cond ((and (null hn) (null cn))
	 (list (list (remove-duplicates ha)
		     (remove-duplicates ca))))
	((consp cn)
	 (split1+ ha hn ca (car cn) (cdr cn)))
	((consp hn)
	 (split1- ha (car hn) (cdr hn) ca cn))))

(defun split1+ (ha hn ca e cn)
  (cond ((conjunction-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1)
	     (flatten ha hn () ca cn (list (lhs e)))
	   (multiple-value-bind (ha2 hn2 ca2 cn2)
	       (flatten ha hn () ca cn (list (rhs e)))
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	(t
	 (split1* ha hn (cons e ca) cn))))

(defun split1- (ha e hn ca cn)
  (cond ((disjunction-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten ha hn (list (args1 e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2) 
	       (flatten ha hn (list (args2 e)) ca cn ())
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	((implication-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten ha hn (list (rhs e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2)
	       (flatten ha hn () ca cn (list (lhs e)))
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	(t
	 (split1* (cons e ha) hn ca cn))))

(defun init-dp-state (&optional eqns)
  (let ((state (null-single-cong-state)))
    (loop for eqn in eqns
	  do (when (equality-p eqn)
	       (assert-eqn! (lhs eqn) (rhs eqn) state)))
    state))








