(in-package dp)

(defmacro uninterp? (term)
  `(not (interp? ,term)))

(defmacro interpsym? (sym)
  `(node-interpreted? ,sym))

(defun interp? (term)
  (or (true-p term)
      (false-p term)
      (dp-numberp term)
      (and (application-p term)
	   (interpsym? (funsym term)))
      (applyupdate-p term)))

(declaim (notinline canonsig-merge canonsig-canon))

(defun canonsig-merge (w cong-state &optional (no-mod nil))
  (canonsig w cong-state no-mod))

(defun canonsig-canon (w cong-state &optional (no-mod nil))
  ;(format t "~%Canonizing: ~S" (node-to-list w))
  (canonsig w cong-state no-mod))

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
  (if (need-neg-processing eqn cong-state)
      (let ((neg-res (invoke-process-neg eqn cong-state)))
	(or neg-res (invoke-process* eqn cong-state)))
      (invoke-process* eqn cong-state)))

(defun need-neg-processing (eqn cong-state)
  (let ((norm-eqn (recursive-sigma eqn cong-state)))
    (or (integer-inequality-p norm-eqn cong-state)
	(and (ineq-p norm-eqn)
	     (dp-numberp (rhs norm-eqn)))
	(negation-p norm-eqn))))

(defun invoke-process* (eqn cong-state)
  (when (and nil
	     (eq eqn pvs::*beqn*)
	     (equal (cong-state-used-assertions cong-state)
		    (cdr pvs::*bused*)))
    (break))
  ;(declare (special *dp-changed*))
  (when (or (and nil (and (application-p eqn)
			  (eq (funsym eqn) *greatereqp*)))
	    (and nil (eq eqn *bass*))) (break))
  (let* ((*dp-changed* nil)
	 (*contradiction* nil) ;(x (break))
	 (canon-eqn (canon eqn cong-state 'no-mod))
	 ;(x (when (false-p canon-eqn) (break)))
	 ;(x (break))
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
     (*dp-changed* (invoke-process-changed canon-eqn cong-state))
     (t (setf (cong-state-used-assertions result)
	      (cdr (cong-state-used-assertions result)))
	*true*))))


(defun invoke-process-changed (canon-eqn cong-state)
  (let ((check-neqs (check-and-canonize-neqs cong-state)))
    (cond
     ((false-p check-neqs)
      (setf (cong-state-used-assertions cong-state)
	    (cdr (cong-state-used-assertions cong-state)))
      *false*)
     (t nil))))

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
   (invoke-process* (signegation (mk-negation eqn) new-cong-state)
		    new-cong-state)))

(defun make-nequality (lhs rhs cong-state)
  (let ((new-eq (sigma (mk-equality lhs rhs) cong-state)))
    (cond
     ((true-p new-eq) *false*)
     ((false-p new-eq) *true*)
     (t (mk-equality new-eq *false*)))))

(defun canon-neq (neq cong-state)
  (cond
   ((false-p neq) neq)
   (t
    (let* ((eq (lhs neq))
	   (lhs (if (equality-p eq) (lhs eq) eq))
	   (rhs (if (equality-p eq) (rhs eq) *true*))
	   (new-lhs (canon lhs cong-state 'nomod))
	   (new-rhs (canon rhs cong-state 'nomod)))
      (if (eq new-lhs new-rhs)
	  *false*
	  (make-nequality new-lhs new-rhs cong-state))))))

(defun find-neq (neq cong-state)
  (if (false-p neq) neq
      (let* ((eq (lhs neq))
	     (new-lhs (dp-find (lhs eq) cong-state))
	     (new-rhs (dp-find (rhs eq) cong-state)))
	(if (eq new-lhs new-rhs)
	    *false*
	    (make-nequality new-lhs new-rhs cong-state)))))

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
      cong-state)
     (t (dp-merge (arg 1 eqn) *false* cong-state))))
   ((false-p eqn) *false*)
   ((true-p eqn) cong-state)
   ((equality-p eqn)
    (dp-merge (lhs eqn) (rhs eqn) cong-state)
    (forward-chain eqn cong-state))
   (t (dp-merge eqn *true* cong-state)
      (forward-chain eqn cong-state))))

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
	  (when (equality-p u) (break))
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
		     (u_sigma (canonsig-merge (sigma u_sig cong-state) cong-state)))
		;;(break "interp")
		(cond
		 ((true-p u-find) ;(when (false-p u_sigma) (break))
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

(defun canon (term cong-state &optional (no-mod nil))
  (let ((canon-hash (canon-hash term cong-state)))
    (cond
     (canon-hash
      (let (;(t-f (find-top canon-hash cong-state))
	    (t-c (canon* term cong-state no-mod))
	    )
	(cond
	 (t t-c)
	 ((eq t-f t-c) t-f)
	 (*use-t-c* t-c)
	 (t (break "Please contact Cyrluk and do a :cont 0 to continue")
	    t-c))))
     (t (let ((result (canon* term cong-state no-mod)))
	  (setf (canon-hash term cong-state) canon-hash)
	  result)))))

(defun canon* (term cong-state &optional (no-mod nil))
  (canonsig-canon (signature term cong-state no-mod) cong-state no-mod))

(defun mk-term-cs (list)
  (mk-term list))

(defun need-equality-use (eqn cong-state)
  (let ((uses (use eqn cong-state)))
    (loop for u in uses
	  thereis (not (or (equality-p u)
			   (negation-p u))))))

(defun canonsig (w cong-state &optional (no-mod nil))
  (declare (notinline mk-term-cs))
  (cond
   ((interpsym? w) w)
   ;((and (equality-p w) (not (need-equality-use w cong-state))) w)
   ((application-p w)
    ;(when (and (equality-p w) (need-equality-use w cong-state)) (break))
    (let ((ww (if (and (interp? w)
		       (not (project-p w)))
		 (or (sigma
		      (mk-term-cs
			  (cons (funsym w)
				(map-funargs-list #'canonsig
						  w cong-state no-mod)))
		      cong-state)
		     t
		     (full-replace-args-with-canonsig w cong-state no-mod)
		     t
		     (replace-args-with-canonsig w cong-state no-mod)
		     t
		     (replace-args-with-canonsig w cong-state no-mod))
		 w)))
      (if (application-p ww)
	  (loop for u in (use (arg 1 ww) cong-state)
		when (or		;(break)
		      (eq ww (sig u cong-state)))
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
    result))

(defun full-replace-args-with-canonsig (w cong-state &optional (no-mod ni l))
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
   ;((and (equality-p term) (not (need-equality-use term cong-state))) term)
   ;((equality-p term) term)
   ((application-p term)
    (map-args #'add-use (the application term)
	      (the application term) (the cong-state cong-state)))
   (t nil))
  (setf (seen term cong-state) t))
