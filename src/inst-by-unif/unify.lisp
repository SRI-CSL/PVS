(in-package :dp)

(defmacro protecting-dp-state (((new-dp-state old-dp-state)) &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-dp-state (dp::push-new-cong-state ,old-dp-state))
	   (,resultsym nil))
       (declare (special ,new-dp-state))
       (unwind-protect
	   (progn
	     (setq ,resultsym
		   (multiple-value-list (progn ,@body)))
	     (values-list ,resultsym))
	 (dp::npop-cong-state ,new-dp-state)))))

(defvar *fail* :fail)

(defun fail? (subst)
  (eq subst *fail*))

; Initialization

(defvar *E-unify-table* nil)
(defvar *cache-hits* 0)
(defvar *cache-misses* 0)

(defun E-unify-init ()
  (setf *cache-hits* 0)
  (setf *cache-misses* 0)
  (if *E-unify-table*
      (clrhash *E-unify-table*)
    (setf *E-unify-table* (make-hash-table :test 'E-unify-args=))))

(defun E-unify-args= (args1 args2)
  (and (eq (first args1) (first args2))
       (eq (second args1) (second args2))
       (eq (third args1) (third args2))
       (eq (fourth args1) (fourth args2))))

;; Top-Level E-Unification
;;    Usage: e.g. (progn (E-unify-init)
;;                       ... (E-unify ...) ...)

(defun E-unify (t1 t2 cs subst)
  (when (eq t1 t2) subst)
  (let ((args (list t1 t2 cs subst)))
    (let ((result (gethash args *E-unify-table*)))
      (if result
	  (progn
	    (setf *cache-hits* (1+ *cache-hits*))
	    result)
	(let ((new-subst (E-unify* t1 t2 cs subst)))
	  (setf *cache-misses* (1+ *cache-misses*))
	  (setf (gethash args *E-unify-table*) new-subst))))))

(defun E-unify* (t1 t2 cs subst)
  "Given a substituion and a congruence closure state,
   tries to unify terms t1 and t2. Returns *fail* if if
   does not succeed to do so; otherwise it returns an
   extended substitution that equalizes t1 and t2."
  #+dbg(assert (node-p t1))
  #+dbg(assert (node-p t2))
  #+dbg(assert (not (null cs)))
  #+dbg(assert (listp subst))
  (let* ((s1 (app t1 subst))
	 (s2 (app t2 subst))
	 (xs (vars-of s1))
         (ys (vars-of s2)))
    (when (eq s1 s2) subst)
    (protecting-dp-state ((*cs* cs))
       (let ((res (catch 'fail                       ; unification closure
		    (progn (unif-closure s1 s2)
			   (consistent? s1 *cs*)
			   *cs*))))
	 (if (symbolp res) *fail*                    ; resulting substitution
             (let ((new-subst (make-subst (union xs ys)
					  *cs*)))
	       (composition subst new-subst)))))))

;; Unification Closure

(defun unif-closure (s1 s2)
  (declare (special *cs*))
  (when (or (eq s1 s2)
	    (eq (canon s1 *cs*) (canon s2 *cs*)))
    (return-from unif-closure))
  (let ((r1 (dp-schema (dp-find s1 *cs*) *cs*))
	(r2 (dp-schema (dp-find s2 *cs*) *cs*)))
    (when (eq r1 r2)
      (return-from unif-closure))
    (when (and (dp-variable-p r1) (dp-variable-p r2))
      (dp-union r1 r2 *cs*)
      (return-from unif-closure))
    (when (and (dp-variable-p r1) (constant-p r2)) ; experiment
      (dp-union r1 r2 *cs*)
      (return-from unif-closure))
    (let ((xs (union (vars-of r1)
		     (vars-of r2))))
      (when (null xs)
	(throw 'fail :clash))
      (loop for x in xs
	    do (let ((res (solving r1 r2 x *cs*)))
		 (when (true-p res)
		   (return-from unif-closure))
		 (when (false-p res)
		   (throw 'fail :inconsistency))
		 (when (solved? res x)
		   (dp-union (lhs res) (rhs res) *cs*)
		   (return-from unif-closure))))
      (when (and (application-p r1) 
		 (application-p r2))
	(multiple-value-bind (op1 args1)
	    (destructure-application r1)
	  (multiple-value-bind (op2 args2)
	      (destructure-application r2)
	    (if (eq op1 op2)
		(progn
		  (dp-union s1 s2 *cs*)
		  (loop for a1 in args1
			as a2 in args2
			do (unif-closure a1 a2))
		  (return-from unif-closure))
		(throw 'fail :clash)))))
      (throw 'fail :notequal))))
	
(defun destructure-application (trm &optional args)
  (if (application-p trm)
      (destructure-application (funsym trm)
			       (append (funargs trm) args))
    (values trm args)))

;; Finding a solution from the unification closure

(defun consistent? (s cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (unwind-protect
	(progn
	  (init-visited)
	  (init-acyclic)
	  (consistent* s))
      (progn
	(init-visited)
	(init-acyclic)))))

(defun consistent* (s)
  (declare (special *cs*)
	   (special *acyclic*)
	   (special *visited*))
  (when (constant-p s)
    (return-from consistent*))
  (let ((s (dp-schema (dp-find (canon s *cs*) *cs*) *cs*)))
    (when (constant-p s)
      (return-from consistent*))
    (when (gethash s *acyclic*)
       (return-from consistent*))
    (when (gethash s *visited*)
       (throw 'fail :cycle))
    (when (application-p s)
      (setf (gethash s *visited*) T)
      (loop for a in (funargs s)
	    do (consistent* a))
      (setf (gethash s *visited*) NIL))
    (setf (gethash s *acyclic*) T)))

(defvar *visited* nil)
(defvar *acyclic* nil)

(defun init-visited ()
  (if *visited*
      (clrhash *visited*)
      (setf *visited* (dp-make-eq-hash-table))))

(defun init-acyclic ()
  (if *acyclic*
      (clrhash *acyclic*)
      (setf *acyclic* (dp-make-eq-hash-table))))

;; Generating an mgu

(defun make-subst (xs cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (make-subst* xs nil)))

(defun make-subst* (xs subst)
  (declare (special *cs*))
  (if (null xs)
      (nreverse subst)
    (make-subst* (cdr xs)
		 (let* ((x (car xs))
			(e (instance x *cs*)))
		   (if (eq x (canon e *cs*)) subst
		       (acons x e subst))))))

(defun instance (e cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (instance* e)))

(defun instance* (e)
  (declare (special *cs*))
  #+dbg(assert (consistent? e *cs*))
  (let ((s (dp-schema (dp-find (canon e *cs*) *cs*) *cs*)))
    (cond ((dp-variable-p s) s)
	  ((application-p s)
	   (mk-term (cons (funsym s)
			  (mapcar #'instance*
			      (funargs s)))))
	  (t s))))

(defun instantiated? (e cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (null (vars-of (instance* e)))))

; (defun subst= (subst1 subst2 &optional state)
;  (or (eq subst1 subst2)
;      (and (subst<= subst1 subst2 state)
;	   (subst<= subst2 subst1 state))))

(defun subst= (sigma tau)
 (or (eq sigma tau)
     (let ((xs (support sigma))
	   (ys (support tau)))
       (and (subsetp xs ys)
	    (subsetp ys xs)
            (not (fail? (renamings-of sigma tau)))))))

(defun renamings-of (sigma tau &optional eta)
  (if (null sigma)
      (nreverse eta)
    (destructuring-bind (x . t1)
	(car sigma)
      (let ((t2 (cdr (lookup x tau))))
	(assert (node-p t2))
	(let ((new-eta (renaming-of t1 t2 eta)))
	  (if (fail? new-eta) *fail*
	      (renamings-of (cdr sigma) tau new-eta)))))))

(defun renaming-of (t1 t2 &optional eta)
  (cond ((and (dp-variable-p t1) (dp-variable-p t2))
	 (let ((res1 (lookup t1 eta))
	       (res2 (lookup t2 eta)))
	   (cond ((and res1 res2)
		  (if (eq (cdr res1) (cdr res2)) eta *fail*))
		 (res1
		  (if (eq (cdr res1) t2) eta (acons t2 (cdr res1) eta)))
		 (res1
		  (if (eq (cdr res2) t1) eta (acons t1 (cdr res2) eta)))
		 (t
		  (if (eq t1 t2) eta (acons t1 t2 eta))))))
	((and (constant-p t1) (constant-p t2))
	 (if (eq t1 t2) eta *fail*))
	((and (application-p t1) (application-p t2))
	 (if (eq (funsym t1) (funsym t2))
	     (renaming-of* (funargs t1) (funargs t2) eta)
	   *fail*))
	(t *fail*)))

(defmethod renaming-of* (l1 l2 eta)
  (cond ((and (null l1) (null l2))
	 eta)
	((and (consp l1) (consp l2))
	 (renaming-of* (cdr l1) (cdr l2)
		       (renaming-of (car l1) (car l2))))
	(t *fail*)))
	

(defun subst<= (subst1 subst2 &optional state)             ; needs work
  (every #'(lambda (pair)
	     (let ((res (lookup (car pair) subst2)))
	       (and res (if state
			    (eq (canon (cdr res) state)
				(canon (cdr pair) state))
			    (eq (cdr res) (cdr pair))))))
	 subst1))

(defun support (sigma)
  (mapcar #'car sigma))

(defun app (trm sigma)
  (replace-by trm sigma))

(defun composition (sigma tau)
  (multiple-value-bind (support-of-sigma subst1)
      (composition1 sigma tau)
    (let ((subst2 (composition2 tau support-of-sigma)))
      (append subst1 subst2))))
			    
(defun composition1 (sigma tau &optional support subst)
  (if (null sigma)
      (values (nreverse support) (nreverse subst))
    (destructuring-bind (x . trm)
	(car sigma)
      (composition1 (cdr sigma) tau (cons x support)
		    (let ((new-trm (app trm tau)))
		      (if (eq x new-trm) subst
			  (acons x new-trm subst)))))))

(defun composition2 (tau support-of-sigma &optional subst)
  (if (null tau)
      (nreverse subst)
    (let* ((pair (car tau))
	   (new-subst (if (member (car pair) support-of-sigma) subst
			  (cons pair subst))))
      (composition2 (cdr tau) support-of-sigma new-subst))))
	
      
;; E-solving

(defun solving (t1 t2 x cs)
  "Solving an equality t1 = t2 for x; returns either *true*,
   *false*, or an equality which may not necessarily be solved for x;
   the congruence state may be changed."
  #+dbg(assert (node-p t1))
  #+dbg(assert (node-p t2))
  #+dbg(assert (dp-variable-p x))
  #+dbg(assert (not (null cs)))
  (let ((canonized (canon (mk-equality t1 t2) cs)))
    (if (or (true-p canonized) (false-p canonized)) canonized
        (let ((solved (normineq canonized cs x)))
      (if (well-formed-node-p solved) solved
	  canonized)))))

(defun solved? (eqn x)
  "Checks if the equation eqn is of the form x = e s.t.
   x does not occur in e."
  #+dbg(assert (node-p eqn))
  #+dbg(assert (dp-variable-p x))
  (and (equality-p eqn)
       (eq (lhs eqn) x)
       (not (member x (vars-of (rhs eqn))))))
