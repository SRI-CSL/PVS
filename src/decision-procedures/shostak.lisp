;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:56:47
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(defun invoke-process (eqn cong-state)
  "Main api interface to the dps.
This is the only function that should be called to add constraints
to the decision procedures.
eqn is an atomic formula, either an equality or a boolean term.
Invoke-process processes eqn in cong-state. If eqn is entailed by
cong-state invoke-process returns *true*, and cong-state is unchanged.
If eqn is contradictory to cong-state, invoke-process returns *false*.
Otherwise cong-state is modified to include the consequences of eqn,
and invoke-process returns nil.
eqn needn't be canonized, but should be part of *term-hash*,
i.e. created using the apis (mk-constant, mk-term, (mk-dp-variable ?))"
  (if (need-neg-processing eqn cong-state)
      (let ((neg-res (invoke-process-neg eqn cong-state)))
	(or neg-res (invoke-process* eqn cong-state)))
      (invoke-process* eqn cong-state)))

(defun need-neg-processing (eqn cong-state)
  "This is a heuristic for deciding whether to try to refute eqn
in the hopes of getting a contradiction showing that eqn is true.
This is necessary because the combination of dps does not necessarily
reduce all true assertions to true."
  (let ((norm-eqn (recursive-sigma eqn cong-state)))
    (or (integer-inequality-p norm-eqn cong-state)
	(and (ineq-p norm-eqn)
	     (dp-numberp (rhs norm-eqn)))
	(negation-p norm-eqn))))

(defun invoke-process* (eqn cong-state)
  "The main workhorse for invoke-process."
  (let* ((*dp-changed* nil)
	 (*contradiction* nil)
	 (canon-eqn (canon eqn cong-state 'no-mod))
	 (result (process* canon-eqn (unless (false-p canon-eqn)
				       (add-assertion eqn cong-state)))))
    (declare (special *dp-changed*))
    (declare (special *contradiction*))
    (cond
     ((or *contradiction* (eq result *false*))
      *false*)
     (*dp-changed* (invoke-process-changed canon-eqn cong-state))
     (t (setf (cong-state-used-assertions result)
	      (cdr (cong-state-used-assertions result)))
	*true*))))


(defun invoke-process-changed (canon-eqn cong-state)
  nil)

(defun invoke-process-neg (eqn cong-state)
  (let ((neg-result (negate-and-check-eqn eqn cong-state)))
    (cond
     ((true-p neg-result)
      *false*)
     ((false-p neg-result)
      *true*)
     (t nil))))

(defun negate-and-check-eqn (eqn cong-state)
  (nprotecting-cong-state
   (new-cong-state cong-state)
   (invoke-process* (mk-negation eqn) new-cong-state)))

(defun make-nequality (lhs rhs cong-state)
  (let ((new-eq (sigma (mk-equality lhs rhs) cong-state)))
    (cond
     ((true-p new-eq) *false*)
     ((false-p new-eq) *true*)
     (t (mk-equality new-eq *false*)))))


(defvar *use-t-c* t)

(defun check-and-canonize-neqs (cong-state)
  (let ((false-eq?
	 (loop for neq in (nequals cong-state)
	       ;for find-neq = (find-neq neq cong-state)
	       for canon-neq = (canon-neq neq cong-state)
	       unless (true-p canon-neq) collect canon-neq into new-neqs
	       when (and (not *use-t-c*)
			 (not (eq canon-neq neq))
			 (eq neq find-neq))
	       do (break "This is not a problem. ~%Just send email to cyrluk and do a :cont 0")
	       thereis (and (false-p canon-neq) (not (false-p neq)))
	       finally (setf (nequals cong-state) new-neqs))))
    (cond
     (false-eq?
      (setq *contradiction* t)
      (add-neq *false* cong-state)
      *false*)
     (t nil))))


(defun process (eqns cong-state)
  (if eqns
      (let* ((eqn (car eqns))
	     (new-cong-state (invoke-process eqn cong-state)))
	(process (cdr eqns) new-cong-state))
      cong-state))

(defun process* (eqn cong-state)
  (declare (special *contradiction*))
  (let* ((new-cong-state cong-state)
	 ;;(x (break))
	 (solve-eqns (solve eqn cong-state))
	 (after-merge-cong-state
	  (loop for eq in solve-eqns
		for eq-result = (process-solved-eqn eq new-cong-state)
		when (false-p eq-result)
		do (progn (setq *contradiction* t)
			  (return *false*))
		unless (true-p eq-result)
		do (setq new-cong-state eq-result)
		finally (return new-cong-state))))
    after-merge-cong-state))

(defun process-solved-eqn (eqn cong-state) ;(break)
  (cond
   ((false-p eqn) *false*)
   ((true-p eqn) cong-state)
   ((neq-p eqn) (add-neq eqn cong-state)
    cong-state)
   ((negation-p eqn)
    (cond
     ((equality-p (arg 1 eqn))
      (add-neq (mk-equality (arg 1 eqn) *false*) cong-state)
      (dp-merge (arg 1 eqn) *false* cong-state))
     (t (dp-merge (arg 1 eqn) *false* cong-state))))
   ((false-p eqn) *false*)
   ((true-p eqn) cong-state)
   ((equality-p eqn)
    (dp-merge (lhs eqn) (rhs eqn) cong-state)
    (forward-chain eqn cong-state))
   (t (dp-merge eqn *true* cong-state)
      (forward-chain eqn cong-state))))

(defvar *dbg* nil)

(defun dp-merge (t1 t2 cong-state)
  ;(declare (special *contradiction*))
  #+dp-dbg(assert (and (eq t1 (dp-find t1 cong-state))
		       (eq t2 (dp-find t2 cong-state))
		       (not (false-p t1))
		       (not (true-p t1))))
  (when (false-p t1) (break))
  (unless (eq t1 t2)
    (when (and nil (application-p t1)
	       (member (funsym t1) (list *greaterp* *lessp*)))
      (break))
    (dp-union t1 t2 cong-state)
    (type-union t1 t2 cong-state)
    (loop for u in (use t1 cong-state) do (when *dbg* (break))
	  (cond
	   ((uninterp? u)
	    (replace-term-in-sig t1 t2 u cong-state)
	    (loop for v in (use t2 cong-state)
		  when (eq (sig v cong-state) (sig u cong-state))
		  do (when *dbg* (break "uninterp"))
		     (process* (sigma
				(mk-equality
				 (sigma (dp-find u cong-state) cong-state)
				 (sigma (dp-find v cong-state) cong-state))
				cong-state)
			       cong-state))
	    (add-use t2 u cong-state))
	   (t (let* ((u-find (dp-find u cong-state))
		     (u_sig (replace-term-in-term t1 t2 u))
		     (u_sigma (canonsig-merge (sigma u_sig cong-state) cong-state)))
		(when *dbg* (break "interp"))
		(cond
		 ((true-p u-find) ;(when (false-p u_sigma) (break))
		  (process* (canonsig
			     (sigma
			      (mk-equality u_sigma u-find) cong-state)
			     cong-state)
			    cong-state))
		 ((and (equality-p u) (false-p u-find)) ;found a disequality.
		  (process* (canonsig
			     (sigma
			      (mk-equality u_sigma u-find) cong-state)
			     cong-state)
			    cong-state))
		 ((eq u-find u)
		  (dp-merge u u_sigma cong-state))
		 (t (and nil
			(process* (canon (mk-equality u-find u_sigma) cong-state t)
			      cong-state)))))))))
  cong-state)

(defun replace-term-in-term (t1 t2 term)
  (labels
      ((replarg
	(arg)
	(if (eq arg t1) t2 arg)))
    (let ((args (map-args-list #'replarg (the application term))))
      (let ((result (mk-term args)))
	(setf (node-external-info result)
	      (node-external-info t1))
	result))))

(defun replace-term-in-sig (t1 t2 u cong-state)
  (setf (sig u cong-state)
	(replace-term-in-term t1 t2 (sig u cong-state))))

(defun find-eq (eqn cong-state)
  (let ((lhs (lhs eqn))
	(rhs (rhs eqn)))
    (sigma (mk-equality (dp-find lhs cong-state)
			(dp-find rhs cong-state))
	   cong-state)))

(defun find-top (term cong-state)
  (cond
   ((equality-p term) (find-eq term cong-state))
   (t (dp-find term cong-state))))

(defvar *use-canon-hash* nil) ;;; If t hashes the results of canon.
;;; Canon is a very expensive operation because it recursively traverses the
;;; whole term.
;;; Once a term is canonized you should not need to recursively traverse the
;;; term again. So hashing canons should be faster.
;;; But, at least when used in pvs, there are very few terms that are canon'd
;;; many times, since terms are lazily introduced to the dps,
;;; and thus the speedup is minimal.

(defun canon (term cong-state &optional (no-mod nil))
  (if *use-canon-hash*
      (let ((canon-hash (canon-hash term cong-state)))
	(cond
	 (canon-hash canon-hash)
	 (t (let ((result (canon* term cong-state no-mod)))
	      (setf (canon-hash term cong-state) canon-hash)
	      result))))
      (canon* term cong-state no-mod)))

(defun canon* (term cong-state &optional (no-mod nil))
  (canonsig (signature term cong-state no-mod) cong-state no-mod))

(defun canonsig (w cong-state &optional (no-mod nil))
  (cond
   ((interpsym? w) w)
   ((application-p w)
    (let ((ww (if (and (interp? w)
		       (not (project-p w)))
		  (replace-args-with-canonsig w cong-state no-mod)
		  w)))
      (if (application-p ww)
	  (loop for u in (use (arg 1 ww) cong-state)
		when (eq ww (sig u cong-state))
		return (dp-find u cong-state)
		finally (unless no-mod
			  (map-args #'add-use ww ww cong-state))
		(return ww))
	  (dp-find ww cong-state))))
   (t (dp-find w cong-state))))

(defun signature (term cong-state &optional (no-mod nil))
  (declare (type node term)
	   (type cong-state cong-state))
  (if (application-p term)
      (let ((result
	     (sigma
	      (mk-term (map-args-list
			#'canon* (the application term)
			cong-state no-mod))
	      cong-state)))
	(setf (node-external-info result)
	      (node-external-info term))
	result)
      term))

(defun replace-args-with-canonsig (w cong-state &optional (no-mod nil))
  (let ((result
	 (mk-term
	     (cons (funsym w)
		   (map-funargs-list #'canonsig w cong-state no-mod)))))
    (setf (node-external-info result)
	  (node-external-info w))
    (sigma result cong-state)))

(defun adduse-of-term (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (when (and (application-p term) (not (seen term cong-state)))
    (map-args #'adduse-of-term (the application term) cong-state)
    (adduse-of-term-top term cong-state)))

(defun adduse-of-term-top (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (cond
   ((application-p term)
    (map-args #'add-use (the application term)
	      (the application term) (the cong-state cong-state)))
   (t nil))
  (add-interp-use-of-term term cong-state)
  (setf (seen term cong-state) t))
