(in-package dp)

(defmacro uninterp? (term)
  `(not (interp? ,term)))

(defmacro interpsym? (sym)
  `(node-type ,sym))

(defun interp? (term)
  (or (and (application-p term)
	   (interpsym? (funsym term)))
      (applyupdate-p term)))

(defvar *current-cs* nil)

(defun test-process (eqns equality &optional (cs (null-single-cong-state)))
  (return-all-cong-states *made-cong-states*)
  (let ((new-cs (process eqns cs)))
    (setq *current-cs* new-cs)
    (canon equality new-cs)))

;  (let ((lhs-canon (canon (lhs equality) cs))
;	(rhs-canon (canon (rhs equality) cs)))
;    (if (eq lhs-canon rhs-canon)
;	*true*
;	(mk-equality lhs-canon rhs-canon cs)))

(defvar *bass* nil)

(defun invoke-process (eqn cong-state)
  (when (and nil
	     (eq eqn pvs::*beqn*)
	     (equal (cong-state-used-assertions cong-state)
		    (cdr pvs::*bused*)))
    (break))
  ;(declare (special *dp-changed*))
  (when (or (and nil (and (application-p eqn)
			  (eq (funsym eqn) *greaterp*)))
	    (and nil (eq eqn *bass*))) (break))
  (let* ((*dp-changed* nil)
	 (*contradiction* nil) ;(x (break))
	 (canon-eqn (canon eqn cong-state 'no-mod))
	 ;(x (when (false-p canon-eqn) (break)))
	 (result (process* canon-eqn (add-assertion eqn cong-state))))
    (declare (special *dp-changed*))
    (declare (special *contradiction*))
    ;(break "~%dp-changed=~A" *dp-changed*)
    (cond
     ((or *contradiction* (eq result *false*))
      (when (false-p canon-eqn)
	(setf (cong-state-used-assertions cong-state)
	      (cdr (cong-state-used-assertions cong-state))))
      *false*)
     (*dp-changed* (check-and-canonize-neqs cong-state))
     (t (setf (cong-state-used-assertions result)
	      (cdr (cong-state-used-assertions result)))
	*true*))))

(defun canon-neq (neq cong-state)
  (let* ((eq (lhs neq))
	 (new-lhs (canon (lhs eq) cong-state 'nomod))
	 (new-rhs (canon (rhs eq) cong-state 'nomod)))
    (if (eq new-lhs new-rhs)
	*false*
	(mk-nequality new-lhs new-rhs))))

(defun check-and-canonize-neqs (cong-state)
  (let ((false-eq?
	 (loop for neq in (nequals cong-state)
	       for canon-neq = (canon-neq neq cong-state)
	       collect canon-neq into new-neqs
	       thereis (eq canon-neq *false*)
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
   (t (let ((new-eqn (add-if-pure-to-theory eqn cong-state)))
	(cond
	 ((false-p new-eqn) *false*)
	 ((true-p new-eqn) cong-state)
	 (t (dp-merge (lhs new-eqn) (rhs new-eqn) cong-state)))))))

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
    (loop for u in (use t1 cong-state) do
	  ;;(break)
	  (cond
	   ((uninterp? u)
	    (replace-term-in-sig t1 t2 u cong-state)
	    (loop for v in (use t2 cong-state)
		  when (eq (sig v cong-state) (sig u cong-state))
		  do ;;(break "uninterp")
		     (process* (sigma
				(mk-equality
				 (dp-find u cong-state)
				 (dp-find v cong-state))
				cong-state)
			       cong-state))
	    (add-use t2 u cong-state))
	   (t (let* ((u-find (dp-find u cong-state))
		     (u_sig (replace-term-in-term t1 t2 u))
		     (u_sigma (canonsig (sigma u_sig cong-state) cong-state)))
		;;(break "interp")
		(cond
		 ((true-p u-find) ;(when (false-p u_sigma) (break))
		  (process* (canon (mk-equality u_sigma u-find) cong-state)
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
    (let ((args (map-args-array #'replarg (the application term))))
      (mk-term-array args))))

(defun replace-term-in-sig (t1 t2 u cong-state)
  (setf (sig u cong-state)
	(replace-term-in-term t1 t2 (sig u cong-state))))

(defun canon (term cong-state &optional (no-mod nil))
  (canon* term cong-state no-mod))

(defun canon* (term cong-state &optional (no-mod nil))
  (canonsig (signature term cong-state no-mod) cong-state no-mod))

(defun canonsig (w cong-state &optional (no-mod nil))
  (cond
   ((equality-p w) w)
   ((application-p w)
    (let ((ww (if (interp? w)
		 (or (sigma (replace-args-with-canonsig w cong-state no-mod)
			cong-state)
		  (replace-args-with-canonsig w cong-state no-mod))
		 w)))
      (if (application-p ww)
	  (loop for u in (use (arg 0 ww) cong-state)
		when (or		;(break)
		      (eq ww (sig u cong-state)))
		return (dp-find u cong-state)
		finally (unless no-mod
			  (map-args #'add-use ww ww cong-state))
		(return ww))
	  (dp-find ww cong-state))))
   ((interpsym? w) w)
   (t (dp-find w cong-state))))

(defun signature (term cong-state &optional (no-mod nil))
  (declare (type node term)
	   (type cong-state cong-state))
  (if (application-p term)
      (sigma
       (mk-term-array (map-args-array
		       #'canon* (the application term) cong-state no-mod))
       cong-state)
       term))

(defun replace-args-with-canonsig (w cong-state &optional (no-mod nil))
  (mk-term-array (map-args-array #'canonsig w cong-state no-mod)))

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
   ((equality-p term) term)
   ((application-p term)
    (map-args #'add-use (the application term)
	      (the application term) (the cong-state cong-state)))
   (t nil))
  (setf (seen term cong-state) t))
