(in-package dp)

(defvar *distinct-lists* nil)

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

(defun bv-p (term)
  nil
  ;(bvec::is-bv? term)
  )

(defun sigbvec (term cong-state)
  (declare (ignore cong-state))
  term
  ;(bvec::fixed-sigma term)
  )

(defun sigma (term cong-state)
  ;;; assumes immediate args are already in sigma-normal form.
  ;(break)
  (cond
   ((number-type-p term) (sigtype term cong-state))
   ((arith-p term) (sigarith term cong-state))
   ((update-p term) (sigupdate term cong-state))
   ((applyupdate-p term) (sigapplyupdate term cong-state))
   ((project-p term) (sigproject term cong-state))
   ((equality-p term) (sigequal term cong-state))
   ((if-p term) (sigif term cong-state))
   ((bool-p term) (sigbool term cong-state))
   ((bv-p term) (sigbvec term cong-state))
   (t term)))

(defvar *process-types* t)

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
   ((and-p term)
    (sigand term cong-state))
   ((or-p term)
    (sigor term cong-state))
   ((nequal-p term)
    (sigma (mk-negation (mk-equality (lhs term) (rhs term))) cong-state))
   (t term)))

(defun sigand (term cong-state)
  (let ((arg1 (arg 1 term))
	(arg2 (arg 2 term)))
    (cond
     ((false-p arg1) *false*)
     ((false-p arg2) *false*)
     ((true-p arg1) arg2)
     ((true-p arg2) arg1)
     (t term))))

(defun sigor (term cong-state)
  (let ((arg1 (arg 1 term))
	(arg2 (arg 2 term)))
    (cond
     ((true-p arg1) *true*)
     ((true-p arg2) *true*)
     ((false-p arg1) arg2)
     ((false-p arg2) arg1)
     (t term))))

(defun sigif (term cong-state)
  (let ((cond (arg 1 term))
	(then (arg 2 term))
	(else (arg 3 term)))
    (cond
     ((true-p cond) then)
     ((false-p cond) else)
     ((eq then else) then)
     (t term))))

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
     ((some #'(lambda (distinct-list)
		(and (member lhs distinct-list :test #'eq)
		     (member rhs distinct-list :test #'eq)))
	    *distinct-lists*)
      *false*)
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

(defun bv-solve (eq cong-state)
  (list eq)
  ;(bvec::fixed-bv-solve eq)
  )

(defun bv-solve-neq (neq cong-state)
  (list neq)
  ;(bvec::fixed-bv-solve neq)
  )

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
	    (cons neq (nequals cong-state)))
      (when (equality-p neq)
	(process-neq-users neq cong-state)))))

(defun process-neq-users (neq cong-state)
  (let* ((eqn (arg 1 neq))
	 (t1 (lhs (lhs neq)))
	 (t2 (rhs (lhs neq))))
    (when (number-equality-p eqn)
      (let ((t1-gt-t2 (simplify-ineq-constraint
		       (mk-term `(,*greatereqp* ,t1 ,t2)) cong-state))
	    (t1-lt-t2 (simplify-ineq-constraint
		       (mk-term `(,*lesseqp* ,t1 ,t2)) cong-state)))
	(cond
	 ((and (true-p t1-gt-t2)
	       (true-p t1-lt-t2)) (setq *contradiction* t) *false*)
	 ((true-p t1-gt-t2)
	  (process* (sigma (mk-term `(,*greaterp* ,t1 ,t2)) cong-state)
		    cong-state))
	 ((true-p t1-lt-t2)
	  (process* (sigma (mk-term `(,*lessp* ,t1 ,t2)) cong-state)
		    cong-state)))))))

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
      (member (node-initial-type term)
	      (list *integer* *number*))))
	    

(defun number-equality-p (eqn)
  (and
   (equality-p eqn)
   (number-theory-p (lhs eqn))
   (number-theory-p (rhs eqn))))

(defun more-pure-theory? (eqn)
  (cond
   ((arith-bool-p eqn) 'number)
   ((number-equality-p eqn)
    'number)
   (t nil)))
