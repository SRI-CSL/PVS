;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:56:47
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defun invoke-process (trm cs)
  "Main api interface to the dps. This is the only function that should be
   called to add constraints to the decision procedures. trm is an atomic
   formula, either an equality or a boolean term. Invoke-process processes trm
   in cong-state. If trm is entailed by cong-state invoke-process returns *true*,
   and cong-state is unchanged. If trm is contradictory to cong-state,
   invoke-process returns *false*. Otherwise cong-state is modified to include
   the consequences of trm, and invoke-process returns nil.
   trm needn't be canonized, but should be part of *term-hash*, i.e. created
   using the apis (mk-constant, mk-term, (mk-dp-variable ?))"
  (declare (type node trm)
	   (type cong-state cs))
  (or (and (need-neg-processing trm cs) 
	   (invoke-process-neg trm cs))
      (invoke-process* trm cs)))

(defun need-neg-processing (trm cs)
  "This is a heuristic for deciding whether to try to refute trm
   in the hopes of getting a contradiction showing that trm is true.
   This is necessary because the combination of dps does not necessarily
   reduce all true assertions to true."
  (declare (type node trm)
	   (type cong-state cs))
  (let ((norm-trm (recursive-sigma trm cs)))
    (or (integer-inequality-p norm-trm cs)
	(and (ineq-p norm-trm)
	     (dp-numberp (rhs norm-trm)))
	(negation-p norm-trm))))

(defun invoke-process* (eqn cs)
  "The main workhorse for invoke-process."
  (let* ((*dp-changed* nil)
	 (*contradiction* nil)
	 (ceqn (the node (canon eqn cs 'no-mod)))
	 (result (process* ceqn (unless (false-p ceqn)
				  (add-assertion eqn cs)))))
    (declare (special *dp-changed*))
    (declare (special *contradiction*))
    (cond ((or *contradiction* (false-p result))
	   *false*)
	  (*dp-changed* nil)
	  (t (setf (cong-state-used-assertions result)
		   (cdr (cong-state-used-assertions result)))
	     *true*))))

(defun invoke-process-neg (eqn cs)
  (let ((neg-result (negate-and-check-eqn eqn cs)))
    (cond ((true-p neg-result) *false*)
	  ((false-p neg-result) *true*)
	  (t nil))))

(defun negate-and-check-eqn (trm cs)
  (nprotecting-cong-state (new-cs cs)
     (invoke-process* (mk-negation trm) new-cs)))

(defun make-nequality (lhs rhs cs)
  (let ((new-eq (sigma (mk-equality lhs rhs) cs)))
    (cond ((true-p new-eq) *false*)
	  ((false-p new-eq) *true*)
	  (t (mk-equality new-eq *false*)))))

(defun check-and-canonize-neqs (cs)
  (let ((false-eq?
	 (loop for neq in (nequals cs)
	       for cneq = (cneq neq cs)
	       unless (true-p cneq) collect cneq into new-neqs
	       thereis (and (false-p cneq) (not (false-p neq)))
	       finally (setf (nequals cs) new-neqs))))
    (when false-eq?
      (setq *contradiction* t)
      (add-neq *false* cs)
      *false*)))

(defun process (trms cs)
  (declare (type list trms)
	   (type cong-state cs))
  (if (null trms) cs
      (process (cdr trms) (invoke-process (car trms) cs))))

(defun process* (eqn cs)
  (declare (type node eqn)
	   (type cong-state cs)
           (special *contradiction*))
  (let ((new-cs cs))
    (loop for eq in (solve eqn cs)             ; after merge cong-state
	  for eq-result = (process-solved-eqn eq new-cs)
	  when (false-p eq-result)
	  do (progn (setq *contradiction* t)
		    (return *false*))
	  unless (true-p eq-result)
	  do (setq new-cs eq-result)
	  finally (return new-cs))))

(defun process-solved-eqn (eqn cs)
  (cond ((false-p eqn) *false*)
	((true-p eqn) cs)
	((neq-p eqn)
	 (add-neq eqn cs)
	 cs)
	((negation-p eqn)
	 (cond ((equality-p (arg 1 eqn))
		(add-neq (mk-equality (arg 1 eqn) *false*) cs)
		(dp-merge (arg 1 eqn) *false* cs))
	       (t (dp-merge (arg 1 eqn) *false* cs))))
	((equality-p eqn)
	 (dp-merge (lhs eqn) (rhs eqn) cs)
	 (when *forward-chain*
	   (forward-chain eqn cs)))
	(t (dp-merge eqn *true* cs)
	   (when *forward-chain*
	     (forward-chain eqn cs)))))

(defun dp-merge (t1 t2 cs)
  (declare (type node t1)
	   (type node t2)
	   (type cong-state cs))
  #+dbg(assert (and (representative-p t1 cs)
		    (representative-p t2 cs)
		    (not (false-p t1))
		    (not (true-p t1))))
  (unless (eq t1 t2)
    #+dbg(assert (not (or (greater-p t1) (less-p t1))))
    (dp-union t1 t2 cs)
    (type-union t1 t2 cs)
    (loop for u in (the node (use t1 cs)) do
	  (cond ((uninterp? u)
		 (replace-term-in-sig t1 t2 u cs)
		 (loop for v in (the node (use t2 cs))
		       when (eq (sig v cs) (sig u cs))
		       do (process* (sigma (mk-equality
					    (sigma (dp-find u cs) cs)
					    (sigma (dp-find v cs) cs))
					   cs) cs))
		 (add-use t2 u cs))
		(t (let* ((u-find (dp-find u cs))
			  (u-sig (replace-term-in-term t1 t2 u))
			  (u-sigma (canonsig (sigma u-sig cs) cs)))
		     (cond ((true-p u-find)
			    (process* (canonsig (sigma (mk-equality u-sigma u-find)
						       cs) cs) cs))
		      ((and (equality-p u)
			    (false-p u-find)) ;found a disequality.
		       (process* (canonsig (sigma (mk-equality u-sigma u-find)
						  cs) cs) cs))
		      ((eq u-find u)
		       (dp-merge u u-sigma cs))
		      (t (and nil (process* (canon (mk-equality u-find u-sigma) cs t)
					    cs)))))))))
  cs)

(defun replace-term-in-term (t1 t2 term)
  (declare (type node t1)
	   (type node t2)
	   (type node term))
  (labels ((replarg (arg)
	      (if (eq arg t1) t2 arg)))
    (let ((args (map-args-list #'replarg
			       (the application term))))
      (let ((result (mk-term args)))
	(setf (node-external-info result) (node-external-info t1))
	result))))

(defun replace-term-in-sig (t1 t2 u cs)
  (declare (type node t1)
	   (type node t2)
	   (type node u)
	   (type cong-state cs))
  (setf (sig u cs) (the node (replace-term-in-term t1 t2
						   (the node (sig u cs))))))

(defun find-eq (eqn cs)
  (declare (type node eqn)
	   (type cong-state cs))
  #+dbg(assert (equality-p eqn))
  (sigma (mk-equality (dp-find (lhs eqn) cs)
		      (dp-find (rhs eqn) cs))
	 cs))

(defun find-top (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (if (equality-p trm)
      (find-eq trm cs)
      (dp-find trm cs)))

(defvar *use-canon-hash* nil)
;;; If t hashes the results of canon.
;;; Canon is a very expensive operation because it recursively traverses the
;;; whole term.
;;; Once a term is canonized you should not need to recursively traverse the
;;; term again. So hashing canons should be faster.
;;; But, at least when used in pvs, there are very few terms that are canon'd
;;; many times, since terms are lazily introduced to the dps,
;;; and thus the speedup is minimal.

(defun canon (trm cs &optional (no-mod nil))
  (declare (type node trm)
	   (type cong-state cs)
	   (type symbol no-mod))
  (if *use-canon-hash*
      (or (canon-hash trm cs)
	  (setf (canon-hash trm cs)
		(canon* trm cs no-mod)))
      (canon* trm cs no-mod))))

(defun canon* (trm cs &optional (no-mod nil))
  (canonsig (signature trm cs no-mod) cs no-mod))

(defun canonsig (trm cs &optional (no-mod nil))
  (declare (type node trm)
	   (type cong-state cs)
	   (type symbol no-mod))
  (cond ((interpsym? trm) trm)
	((application-p trm)
	 (let ((new-trm (if (and (interp? trm)
				 (not (project-p trm)))
			    (replace-args-with-canonsig trm cs no-mod)
			    trm)))
	   (if (application-p new-trm)
	       (loop for u in (the node (use (arg 1 new-trm) cs))
		     when (eq new-trm (sig u cs))
		     return (dp-find u cs)
		     finally (unless no-mod
			       (map-args #'add-use new-trm new-trm cs))
		     (return new-trm))
	       (dp-find new-trm cs))))
	(t (dp-find trm cs))))

(defun replace-args-with-canonsig (trm cs &optional (no-mod nil))
  (declare (type node trm)
	   (type cong-state cs))
  (let* ((new-args (map-funargs-list #'canonsig trm cs no-mod))
	 (result (mk-term (cons (funsym trm) new-args))))
    (setf (node-external-info result) (node-external-info trm))
    (sigma result cs)))

(defun signature (trm cs &optional (no-mod nil))
  (declare (type node trm)
	   (type cong-state cs))
  (if (application-p trm)
      (let* ((new-args (mapcar #'(lambda (arg)
				   (canon* arg cs no-mod))
			 (args trm)))
	     (new-trm (sigma (mk-term new-args) cs)))
	(setf (node-external-info new-trm) (node-external-info trm))
	new-trm)
      trm))

(defun adduse-of-term (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (when (and (application-p trm)
	     (not (seen trm cs)))
    (mapcar #'(lambda (arg)
		(adduse-of-term arg cs))
      (args trm))
    (adduse-of-term-top trm cs)))

(defun adduse-of-term-top (trm cs)
  (declare (type node trm)
	   (type cong-state cs))
  (when (application-p trm)
    (mapcar #'(lambda (arg)
		(add-use arg trm cs))
      (args trm)))
  (add-interp-use-of-term trm cs)
  (setf (seen trm cs) t))




