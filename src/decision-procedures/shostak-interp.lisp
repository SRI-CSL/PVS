(in-package dp)


(defun dp-theory (term)
  (cond
   ((equality-p term)
    (or (dp-theory (lhs term))
	(dp-theory (rhs term))))
   ((arith-p term) 'arith)
   ((array-p term) 'array)
   ((bv-p term) 'bv)
   (t nil)))

(defun array-p (term) nil)
(defun bv-p (term) nil)

(defun sigma (term cong-state)
  ;;; assumes immediate args are already in sigma-normal form.
  ;(break)
  (cond
   ((number-type-p term) (sigtype term cong-state))
   ((arith-p term) (sigarith term cong-state))
   ((update-p term) (sigupdate term cong-state))
   ((applyupdate-p term) (sigapplyupdate term cong-state))
   ((equality-p term) (sigequal term cong-state))
   ((bool-p term) (sigbool term cong-state))
   (t term)))

(defvar *process-types* nil)

(defun sigtype (term cong-state)
  (if *process-types*
      term
      *true*))

(defun sigbool (term cong-state)
  (cond
   ((equality-p term)
    (let ((lhs (lhs term))
	  (rhs (rhs term)))
      (cond
       ((eq rhs *true*)
	(cond
	 ((eq lhs *true*) *true*)
	 ((eq lhs *false*) *false*)
	 (t lhs)))
       ((eq rhs *false*)
	(cond
	 ((eq lhs *true*) *false*)
	 ((eq lhs *false*) *true*)
	 (t (sigma (mk-term (list *not* lhs)) cong-state))))
       ((eq lhs *true*) rhs)
       ((eq lhs *false*)
	(sigma (mk-term (list *not* rhs)) cong-state))
       ((arith-term-< lhs rhs) term)
       (t (mk-equality rhs lhs)))))
   ((ineq-p term) (sigineq term cong-state))
   ((negation-p term)
    (signegation term cong-state))
   ((nequal-p term)
    (sigma (mk-negation (mk-equality (lhs term) (rhs term))) cong-state))
   (t term)))

(defun signegation (term cong-state)
  (cond
   ((ineq-p (arg 1 term))
    (sigineq (negineq (arg 1 term)) cong-state))
   ((true-p (arg 1 term)) *false*)
   ((false-p (arg 1 term)) *true*)
   (t term)))

(defun sigequal (term cong-state)
  (let ((lhs (lhs term))
	(rhs (rhs term)))
    (cond
     ((eq lhs rhs) *true*)
     ((or (bool-p lhs)
	  (bool-p rhs))
      (sigbool term cong-state))
     ((eq (dp-theory term) 'arith)
      (normineq term cong-state))
     (t term)
     ((arith-term-< lhs rhs) term)
     (t (mk-equality rhs lhs)))))

(defun sigineq (term cong-state)
  (normineq term cong-state))

(defun solve (eqn cong-state)
  (let ((solved (solve* eqn cong-state)))
    (loop for seqn in solved
	  do (adduse-of-term seqn cong-state))
    solved))
  
(defun canon-after-solve (eqn cong-state)
  (adduse-of-term eqn cong-state)
  eqn)

(defun solve* (bool-term cong-state)
  (cond
   ((equality-p bool-term)
    (solve-equality bool-term cong-state))
   ((ineq-p bool-term)
    (arith-solve bool-term cong-state))
   ((negation-p bool-term)
    (solve-neq (mk-equality (arg 1 bool-term) *false*) cong-state))
   ((bool-p bool-term)
    (solve-bool bool-term))
   (t ;;(error "~%No solver for ~A." bool-term)
      (solve-bool bool-term))))

(defun solve-neq (neq cong-state)
  (unless (member neq (nequals cong-state))
    (solve-new-neq neq cong-state)))

(defun solve-new-neq (neq cong-state)
  (let ((theory (dp-theory (arg 1 neq))))
    (case theory
      (arith (arith-solve-neq neq cong-state))
      (array (array-solve-neq neq cong-state))
      (bv (bv-solve-neq neq cong-state))
      (t (list neq)))))

(defun solve-bool (bool)
  (cond
   ((eq bool *true*) ())
   ((eq bool *false*) (list *false*))
   ((negation-p bool) (list (mk-equality (arg 1 bool) *false*)))
   (t (list (mk-equality bool *true*)))))

(defun solve-equality (eqn cong-state)
  (let ((normed-eqn eqn))
    (solve-normed-equality normed-eqn cong-state)))

(defun solve-normed-equality (eqn cong-state)
  (cond
   ((true-p eqn) ())
   ((false-p eqn) (list *false*))
   (t (solve-nontriv-normed-equality eqn cong-state))))

(defun solve-nontriv-normed-equality (eqn cong-state)
  (let* ((theory (dp-theory eqn)))
    (case theory
      (arith (arith-solve eqn cong-state))
      (array (array-solve eqn cong-state))
      (bv (bv-solve eqn cong-state))
      (t (list eqn)))))

(defun add-if-pure-to-theory (eqn cong-state)
  (let ((theory (pure-theory? eqn)))
    (case theory
      (number (add-ineq-var-eqn-constraint eqn cong-state))
      (t eqn))))

(defun add-neq (neq cong-state)
  (declare (special *dp-changed*))
  (let ((nequals (nequals cong-state)))
    (unless (member neq nequals :test #'eq)
      (setq *dp-changed* t)
      (setf (nequals cong-state)
	    (cons neq (nequals cong-state))))))

(defun pure-theory? (eqn) ;(break)
  (simple-pure-theory? eqn))


(defun simple-pure-theory? (eqn)
  (and
   nil
   (cond
    ((ineq-var-p (lhs eqn)) 'number)
    (t nil))))

(defun number-theory-p (term)
  (or (arith-p term)
      (memq (node-initial-type term)
	    (list *integer* *number*))))
	    

(defun more-pure-theory? (eqn)
  (cond
   ((arith-bool-p eqn) 'number)
   ((and
     (equality-p eqn)
     (number-theory-p (lhs eqn))
     (number-theory-p (rhs eqn)))
    'number)
   (t nil)))
