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

(defun subst-of (score-subst) (score-subst-subst subst))
(defun score-of (score-subst) (score-subst-score subst))

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
      (remove-if #'(lambda (score-subst)
		     (incomplete-subst? (subst-of score-subst)))
	(gensubsts-sequents (split1 ha hn ca cn))))))

(defun incomplete-subst? (subst)
   (some #'(lambda (x)
	     (not (member x (dom subst))))
	 *lvars*))

;; Collect all possible substitutions for a given set of sequents

(defun gensubsts-sequents (sequents &optional (score-substs
					       (list (empty-score-subst))))
  (if (null sequents) score-substs
      (gensubsts-sequents (cdr sequents)
			  (destructuring-bind (hyps concs) (car sequents)
			    (let ((*state* (init-dp-state (mapcar #'(lambda (trm)
								      (equality-p trm))
							    hyps))))
			      (gensubsts-sequent hyps concs score-substs))))))

(defun gensubsts-sequent (hyps concs score-substs &optional acc)
  (if (null score-substs) (nreverse acc)
      (let* ((subst (subst-of (car score-substs)))
	     (score (score-of (car score-substs)))
	     (ext-score-substs (mapcar #'(lambda (subst)
					   (make-score-subst :score (1+ score)
							     :subst subst))
				 (gensubsts-sequent1 hyps concs subst)))
	     (new-score-subst (mapcar #'(lambda (subst)
					  (make-score-subst :score 1
							    :subst subst))
				(gensubsts-sequent1 hyps concs nil)))
	     (newacc (add-score-subst new-score-subst
			  (mapcar #'(lambda (score-subst)
				      (add-score-subst score-subst acc))
			    (ext-score-substs)))))
	(gensubsts-sequent hyps concs (cdr score-substs) newacc))))

(defun add-score-subst (score-subst score-substs)
  (if (null score-substs) score-subst
      (cond ((better? score-subst (car score-substs))
	     (cons score-subst (cdr score-substs)))
	    ((better? (car score-substs) score-subst)
	     score-substs)
	    (t
	     (cons (car score-substs)
		   (add-score-subst score-subst (cdr score-substs)))))))

(defun better? (score-subst1 score-score2)
  (and (subst= (subst-of score-subst1) (subst-of score-subst2))
       (> (score-of score-subst1) (score-of score-subst2))))

(defun gensubsts-sequent1 (hyps concs subst)
  "Given a substitution list all substitutions generated from
   unifying complementary literals (involving variables)"
    (let ((*insts* insts))
      (loop for hyp in hyps
	    do (loop for conc in concs
		     do (when (or (occurs-p *lvars* hyp)
				  (occurs-p *lvars* conc)) 
			  (let ((new-subst (E-unify hyp conc *state* subst)))
			    (unless (or (fail? new-subst)
					(some #'(lambda (subst1)
						  (subst= new-subst subst1)))) 
			      (setf *insts* (cons new-subst *inst*)))))))
      *insts*))

;; Splitting

(defun split1 (ha hn ca cn)
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
	     (append (split1 ha1 hn1 ca1 cn1)
		     (split1 ha2 hn2 ca2 cn2)))))
	(t
	 (split1 ha hn (cons e ca) cn))))

(defun split1- (ha e hn ca cn)
  (cond ((disjunction-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten ha hn (list (args1 e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2) 
	       (flatten ha hn (list (args2 e)) ca cn ())
	     (append (split1 ha1 hn1 ca1 cn1)
		     (split1 ha2 hn2 ca2 cn2)))))
	((implication-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten ha hn (list (rhs e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2)
	       (flatten ha hn () ca cn (list (lhs e)))
	     (append (split1 ha1 hn1 ca1 cn1)
		     (split1 ha2 hn2 ca2 cn2)))))
	(t
	 (split1 (cons ha e) hn ca cn))))

(defun init-dp-state (&optional eqns (state (null-single-cong-state))) ; (copy-cong-state* *start-state*)))
    (loop for eqn in eqns
	  do (when (equality-p eqn)
	       (assert-eqn! (lhs eqn) (rhs eqn) state))))








