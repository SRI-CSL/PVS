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

(defvar *empty-score-subst*
  (make-score-subst :subst nil :score 0))

(defun substitution-of (s) (score-subst-subst s))
(defun score-of (s) (score-subst-score s))

(defun score-subst= (s1 s2)
  (and (subst= (substitution-of s1) (substitution-of s2))
       (equal (score-of s1) (score-of s2))))

;; Top-Level

(defun gensubsts (vars trms &optional verbose?)
  "Generate a list of substitutions (for all variables in vars)
   together with scores"
  #+dbg(assert (every #'dp-variable-p vars))
  #+dbg(assert (every #'node-p trms))
  (let ((*lvars* vars)
	(*verbose* verbose?))
    (declare (special *lvars*)
	     (special *verbose*))
    (multiple-value-bind (ha hn ca cn)
	(flatten-seq () () () () () trms)
      (let ((sequents (split1 ha hn ca cn)))
	(display-sequents sequents)
        #+dbg(assert (every #'sequent? sequents))
	(filter-substs
	 (gensubsts-sequents sequents))))))

(defun display-sequents (sequents)
  (declare (special *verbose*))
  (when *verbose*
    (format t "~%Splits: ~a " (length sequents))))

(defun filter-substs (score-substs &optional acc)
  (if (null score-substs) (nreverse acc)
    (let* ((score-subst (car score-substs))
	   (subst (subst-of score-subst))
	   (newacc (if (null subst) acc
		       (cons (variable-free-part-of score-subst) acc))))
      (filter-substs (cdr score-substs) newacc))))

(defun variable-free-part-of (score-subst)
  (make-score-subst :score (score-of score-subst)
		    :subst (remove-if #'(lambda (pair)
					  (var-occurs-p (cdr pair)))
			     (substitution-of score-subst))))
	  
;(defun incomplete? (subst)
;  (declare (special *lvars*))
;  (some #'(lambda (x)
;	    (or ; (not (member x (dom subst)))   ; now also deal with partial substs
;		(some #'(lambda (trm)
;			  (and trm (occurs-p x trm)))
;		      (mapcar #'cdr subst))))
;	*lvars*))

;; Collect all possible substitutions for a given set of sequents

(defun sequent? (seq)
  (and (listp seq)
       (listp (first seq))
       (listp (second seq))
       (every #'node-p (first seq))
       (every #'node-p (second seq))))

(defun gensubsts-sequents (sequents &optional (score-substs
					       (list *empty-score-subst*)))
  #+dbg(assert (every #'sequent? sequents))
  (if (null sequents) score-substs
      (let ((new-score-substs
	     (destructuring-bind (hyps concs) (car sequents)
	       (let ((*state* (init-dp-state nil)))
			    ;   (collect-equalities hyps))))
		 (declare (special *state*))
		 (score-substs-union score-substs
				     (gensubsts-sequent hyps concs
							(extended-score-substs score-substs)))))))
	(gensubsts-sequents (cdr sequents) new-score-substs))))

(defun collect-equalities (fmlas &optional acc)
  (if (null fmlas) (nreverse acc)
    (let* ((fmla (car fmlas))
	   (newacc (if (equality-p fmla)
		       (cons fmla acc)
		     acc)))
      (collect-equalities (cdr fmlas) newacc))))
			 
(defun extended-score-substs (score-substs)
  (if (some #'(lambda (score-subst)
		(null (substitution-of score-subst)))
	    score-substs)
      score-substs
    (cons *empty-score-subst* score-substs)))

(defun gensubsts-sequent (hyps concs score-substs)
  #+dbg(assert (every #'node-p hyps))
  #+dbg(assert (every #'node-p concs))
  #+dbg(assert (every #'score-subst-p score-substs))
  (if (null score-substs) nil
      (let* ((subst (substitution-of (car score-substs)))
	     (score (score-of (car score-substs)))
	     (new-score-substs
	      (mapcar #'(lambda (subst)
			  (make-score-subst :score (1+ score)
					    :subst subst))
		(gensubsts-sequent1 hyps concs subst))))
	(score-substs-union new-score-substs
			    (gensubsts-sequent hyps
					       concs
					       (cdr score-substs))))))

(defun score-substs-union (ss1 ss2)
  (add-score-substs (append ss1 ss2) nil))

(defun add-score-substs (ss1 ss2)
  (if (null ss1) ss2
      (add-score-substs (cdr ss1)
			(add-score-subst (car ss1) ss2))))

(defun add-score-subst (score-subst score-substs)
  (if (null score-substs)
      (list score-subst)
    (let ((score-subst1 (car score-substs)))
      (if (subst= (substitution-of score-subst1) (substitution-of score-subst))
	  (if (< (score-of score-subst1) (score-of score-subst))
	      (cons score-subst (cdr score-substs))
	    score-substs)
       (cons score-subst1 (add-score-subst score-subst (cdr score-substs)))))))

(defun gensubsts-sequent1 (hyps concs subst)
  "Given a substitution list all substitutions generated from
   unifying complementary literals (involving variables)"
  (declare (special *state*))
  #+dbg(assert (every #'node-p hyps))
  #+dbg(assert (every #'node-p concs))
  (let ((*instantiations* nil))
    (loop for hyp in hyps
	  do (loop for conc in concs
		   do (when (or (var-occurs-p hyp)
				(var-occurs-p conc)) 
			(let ((new-subst (E-unify hyp conc *state* subst))) 
			  (setf *instantiations*
				(add-to-subst new-subst *instantiations*))))))
    *instantiations*))

(defun var-occurs-p (trm)
  (declare (special *lvars*))
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
  #+dbg(assert (every #'node-p
		      (append ha hn ca cn)))
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
  (assert (node-p e))
  (cond ((conjunction-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1)
	     (flatten-seq ha hn () ca cn (list (lhs e)))
	   (multiple-value-bind (ha2 hn2 ca2 cn2)
	       (flatten-seq ha hn () ca cn (list (rhs e)))
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	(t
	 (split1* ha hn (cons e ca) cn))))

(defun split1- (ha e hn ca cn)
  (cond ((disjunction-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten-seq ha hn (list (lhs e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2) 
	       (flatten-seq ha hn (list (rhs e)) ca cn ())
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	((implication-p e)
	 (multiple-value-bind (ha1 hn1 ca1 cn1) 
	     (flatten-seq ha hn (list (rhs e)) ca cn ())
	   (multiple-value-bind (ha2 hn2 ca2 cn2)
	       (flatten-seq ha hn () ca cn (list (lhs e)))
	     (append (split1* ha1 hn1 ca1 cn1)
		     (split1* ha2 hn2 ca2 cn2)))))
	(t
	 (split1* (cons e ha) hn ca cn))))

;; Propositional flattening and categorization into
;; atomic and non-atomic literals hypotheses (ha hs) and conclusions (ca cs)

(defun flatten-seq (ha hs hn ca cs cn)
  #+dbg(assert (every #'node-p (append ha hs hn ca cs cn)))
  (flatten-seq* ha hs hn ca cs cn))

(defun flatten-seq* (ha hs hn ca cs cn)
  (cond ((consp cn)
	 (flatten-seq+ ha hs hn ca cs cn))
	((consp hn)
	 (flatten-seq- ha hs hn ca cs cn))
	(t
	 (values ha hs ca cs))))

(defun flatten-seq+ (ha hs hn ca cs cn)
  #+dbg(assert (consp cn))
  (let* ((c (car cn))
	 (rst (cdr cn)))
    (cond ((negation-p c)
	   (flatten-seq* ha hs (cons (arg 1 c) hn)
			 ca cs rst))
	  ((disjunction-p c)
	   (flatten-seq* ha hs hn
			 ca cs (cons (lhs c)
				     (cons (rhs c) rst))))
	  ((implication-p c)
	   (flatten-seq* ha hs (cons (lhs c) hn)
			 ca cs (cons (rhs c) rst)))
	  ((conjunction-p c)
	   (flatten-seq* ha hs hn
			 ca (cons c cs) rst))
	  (t
	   (flatten-seq* ha hs hn
			 (cons c ca) cs rst)))))

(defun flatten-seq- (ha hs hn ca cs cn)
  #+dbg(assert (consp hn))
  (let* ((h (car hn))
	 (rst (cdr hn)))
    (cond ((negation-p h)
	   (flatten-seq* ha hs rst
			 ca cs (cons (arg 1 h) cn)))
	  ((conjunction-p h)
	   (flatten-seq* ha hs (cons (lhs h)
				     (cons (rhs h) rst))
			 ca cs cn))
	  ((or (disjunction-p h)
	       (implication-p h))
	   (flatten-seq* ha (cons h hs) rst
			 ca cs cn))
	  (t
	   (flatten-seq* (cons h ha) hs rst
			 ca cs cn)))))

(defun init-dp-state (&optional eqns)
  (let ((state (null-single-cong-state)))
    (loop for eqn in eqns
	  do (when (equality-p eqn)
	       (assert-eqn! (lhs eqn) (rhs eqn) state)))
    state))

