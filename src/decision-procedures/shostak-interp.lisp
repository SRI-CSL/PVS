;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:56:41
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defun dp-theory (term)
  "As you add new theories add them to this function.
Used by solve to determine which solver to call."
  (cond
   ((equality-p term)
    (or (dp-theory (lhs term))
	(dp-theory (rhs term))))
   ((arith-p term) 'arith)
   ((record-p term) 'record)
   ((tuple-p term) 'tuple)
   ((update-p term) 'array)
   ((adt-p term) 'adt)
   ((bv-p term) 'bv)
   ((set-p term) 'set)
   (t nil)))

(defun sigma (term cong-state &optional (after-solve nil))
  "Main interface to interpreted canonizers.
Add new theory canonizers here.
Assumes immediate args are already in sigma-normal form."
  (cond
   ((number-type-p term) (sigtype term cong-state))
   ((arith-p term) (sigarith term cong-state))
   ((update-p term) (sigupdate term cong-state))
   ((applyupdate-p term) (sigapplyupdate term cong-state))
   ((project-p term) (sigproject term cong-state))
   ((constructor-p term) (sig-constructor term cong-state))
   ((accessor-p term) (sig-accessor term cong-state))
   ((equality-p term) (sigequal term cong-state after-solve))
   ((if-p term) (sigif term cong-state))
   ((bool-p term) (sigbool term cong-state after-solve))
   ((bv-p term) (sigbvec term cong-state))
   ((set-p term) (sigset term cong-state))
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
	
(defun sigtype (term cong-state)
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
	 term))))

(defun solve-type (term cong-state)
  (case (node-initial-type (dp::funsym term))
    (integer-pred
     (setf (dp-type (arg 1 term) cong-state)
	   *integer*)))
  (list (mk-equality term *true*)))

(defun sigequal (term cong-state &optional (after-solve nil))
  (let ((lhs (lhs term))
	(rhs (rhs term)))
    (cond
     ((eq lhs rhs) *true*)
     ((or (bool-p lhs)
	  (bool-p rhs))
      (sigbool term cong-state after-solve))
     ((eq (dp-theory term) 'arith)
      (normineq term cong-state))
     ((eq (dp-theory term) 'adt)
      (sig-adt term cong-state))
     ((and (node-constructor? lhs)
	   (node-constructor? rhs))
      *false*)
     (after-solve term)
     ((monom-< lhs rhs) term)
     (t (mk-equality rhs lhs)))))

(defun sigineq (term cong-state)
  (normineq term cong-state))

(defun sigbvec (term cong-state)
  (declare (ignore cong-state))
  (bvec::fixed-sigma term))

(defun add-interp-use-of-term (term cong-state)
  "Some theory's are such that there are interpreted uses of
terms in addition to the simple syntactic uses.
This function should be modified as those theories are added."
  (let ((theory (dp-theory term)))
    (if (eq theory 'array)
	(add-use-of-update term cong-state))))

(defun solve (eqn cong-state)
  "Main interface for the solvers.
Eqn is assumed to be canonical, but not
necessarily in the cong-state use universe.
Solve first calls the individual solvers and then
adds those results to the use universe."
  (let ((solved (solve* eqn cong-state)))
    (loop for seqn in solved
	  collect (sigma-after-solve seqn cong-state))))

(defun sigma-after-solve (seqn cong-state)
  "Called on solved equations.
This function respects the orientation that the
solvers return and adds the terms gernerated by
the solvers to the use universe."
  (let ((sigma-eqn (after-solve-sigma seqn cong-state)))
    (adduse-of-term sigma-eqn cong-state)
    sigma-eqn))

(defun after-solve-sigma (seqn cong-state)
  "Like sigma, but won't reorient equations."
  (cond
   ((and (equality-p seqn)
	 (not (equality-p (lhs seqn)))
	 (not (negation-p (lhs seqn)))
	 (true-p (rhs seqn)))
    seqn)
   ((equality-p seqn)
    (let ((new-lhs (dp-find (lhs seqn) cong-state))
	  (new-rhs (dp-find (rhs seqn) cong-state)))	
      (sigma (mk-equality new-lhs new-rhs) cong-state t)))
   (t (sigma seqn cong-state t))))

(defun solve* (bool-term cong-state)
  "This calls the individual theory solvers.
This looks at the top level predicate to decide
which theory to call. All equalities are passed to solve-equality.
Modify this function as new theories are added."
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
  "Solver for disequalities.
If a new theory has a disequality solver, add it here."
  (let ((theory (dp-theory (arg 1 neq))))
    (case theory
      (arith (arith-solve-neq neq cong-state))
      (array (array-solve-neq neq cong-state))
      (bv (bv-solve-neq neq cong-state))
      (set (set-solve-neq neq cong-state))
      (t (list neq)))))

(defun solve-equality (eqn cong-state)
  (let ((normed-eqn eqn))
    (solve-normed-equality normed-eqn cong-state)))

(defun solve-normed-equality (eqn cong-state)
  (cond
   ((true-p eqn) ())
   ((false-p eqn) (list *false*))
   (t (solve-nontriv-normed-equality eqn cong-state))))

(defun solve-nontriv-normed-equality (eqn cong-state)
  "This function is the main work-horse for solving equalities.
It looks at the theories of the lhs or rhs of the equality to
decide which theory solver to call.
Modify this function as you add new theories."
  (let* ((theory (dp-theory eqn)))
    (case theory
      (arith (arith-solve eqn cong-state))
      (record (record-solve eqn cong-state))
      (tuple (tuple-solve eqn cong-state))
      (array (array-solve eqn cong-state))
      (adt (adt-solve eqn cong-state))
      (bv (bv-solve eqn cong-state))
      (set (set-solve eqn cong-state))
      (t (list eqn)))))

(defun bv-solve (eq cong-state)
  (list eq)
  ;(bvec::fixed-bv-solve eq)
  )

(defun bv-solve-neq (neq cong-state)
  (list neq)
  ;(bvec::fixed-bv-solve neq)
  )

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
