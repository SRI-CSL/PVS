(in-package dp)

(defvar *distinct-lists* nil)

(defun dp-theory (term)
  (cond
   ((equality-p term)
    (or (dp-theory (lhs term))
	(dp-theory (rhs term))))
   ((arith-p term) 'arith)
   ((record-p term) 'record)
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

(defun recursive-sigma (term cong-state)
  (cond
   ((application-p term)
    (if (interp? term)
	(sigma (mk-term
		   (cons (funsym term)
			 (map-funargs-list
			  #'recursive-sigma term cong-state)))
	       cong-state)
	term))
   (t term)))
	
(defvar *process-types* t)


(defun dp-rational-atom-p (term cong-state)
  (dp-integer-atom-p term cong-state))

(defun dp-real-atom-p (term)
  t)


(defun sigtype (term cong-state)
  (if *process-types*
      (case (node-initial-type (dp::funsym term))
	(integer-pred
	 (if (dp-integer-atom-p (dp::arg 1 term) cong-state)
	     *true*
	     term))
	(rational-pred
	 (if (dp-rational-atom-p (dp::arg 1 term) cong-state)
	     *true*
	     term))
	(real-pred
	 (if (dp-real-atom-p (dp::arg 1 term))
	     *true*
	     term)))
      *true*))

(defun solve-type (term cong-state)
  (when *process-types*
    (case (node-initial-type (dp::funsym term))
      (integer-pred
       (setf (dp-type (arg 1 term) cong-state)
	     *integer*))))
  (list (mk-equality term *true*)))

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
     ((and (node-constructor? lhs)
	   (node-constructor? rhs))
      *false*)
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
    (solve-bool bool-term cong-state))
   (t ;;(error "~%No solver for ~A." bool-term)
      (solve-bool bool-term cong-state))))

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

(defun solve-bool (bool cong-state)
  (cond
   ((eq bool *true*) ())
   ((eq bool *false*) (list *false*))
   ((number-type-p bool)
    (solve-type bool cong-state))
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
      (record (record-solve eqn cong-state))
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
    (when (number-equality-p eqn cong-state)
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

(defun number-theory-p (term cong-state)
  (or (arith-p term)
      (member (dp-type term cong-state)
	      (list *integer* *number*))))
	    

(defun number-equality-p (eqn cong-state)
  (and
   (equality-p eqn)
   (number-theory-p (lhs eqn) cong-state)
   (number-theory-p (rhs eqn) cong-state)))

(defun more-pure-theory? (eqn cong-state)
  (cond
   ((arith-bool-p eqn) 'number)
   ((number-equality-p eqn cong-state)
    'number)
   (t nil)))
