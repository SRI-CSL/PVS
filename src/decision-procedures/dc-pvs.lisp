(in-package pvs)

(unless (find-package 'dp) (make-package 'dp))

(unless (member "src/prover/dc-prototypes" *pvs-directories*)
  (setq *pvs-directories*
	(append *pvs-directories*
		(list "src/prover/dc-prototypes"))))

(unless (member "src/prover/dc-prototypes/polylib" *pvs-directories*)
  (setq *pvs-directories*
	(append *pvs-directories*
		(list "src/prover/dc-prototypes/polylib"))))

(defvar *dp-state* nil)

(defvar *new-ground?* nil)
(defvar *old-ground?* t)

(defvar *init-alists* (make-instance 'dpinfo
			'dpinfo-sigalist nil
			'dpinfo-findalist nil
			'dpinfo-usealist *init-usealist*))

(declaim (special *dp-changed* *alists* *dp-state*
		  *top-alists* *top-dp-state*))

(defvar *print-expanded-dpinfo* t)

(defmethod print-object ((alists dpinfo) stream)
  (if (or (not *print-expanded-dpinfo*) *debugging-print-object*)
      (call-next-method)
      (format stream "<#dpinfo:~a>" (dpinfo-findalist alists))))

(defun new-ground ()
  (setq *newdc* t
	*new-ground?* t
	*old-ground?* nil))

(defun old-ground ()
  (setq *newdc* nil
	*new-ground?* nil
	*old-ground?* t))

(defun both-ground ()
  (setq *newdc* nil
	*new-ground?* t
	*old-ground?* t))

(defun dp::restore ()
  (in-package pvs)
  (restore))

(defmacro nprotecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (unwind-protect
	   (setq ,resultsym
		 (multiple-value-list (progn ,@body)))
	 (restore-old-cs ,new-cong-state))
       (values-list ,resultsym))))

(defmacro protecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defun alists-changed (old-alists new-alists)
  (not (and (eq (dpinfo-usealist old-alists)
		(dpinfo-usealist new-alists))
	    (eq (dpinfo-findalist old-alists)
		(dpinfo-findalist new-alists))
	    (eq (dpinfo-sigalist old-alists)
		(dpinfo-sigalist new-alists)))))

(defvar *break-on-ground-diff* t)

(defun dp-changed (old-dpstate new-dpstate old-alists new-alists)
  (let ((new-changed (and *new-ground?*
			  (dp::dp-changed old-dpstate new-dpstate)))
	(old-changed (and *old-ground?*
			  (alists-changed old-alists new-alists))))
    (assert (or (not *break-on-ground-diff*)
		(not (and *new-ground?* *old-ground?*))
		(eq new-changed old-changed))
	    (*break-on-ground-diff*))
    (if *new-ground?*
	new-changed
	old-changed)))

(defun top-translate-to-dc (expr)
  (let ((*newdc* t))
    (top-translate-to-prove expr)))

(defun top-translate-to-old-prove (expr)
  (let ((*newdc* nil))
    (top-translate-to-prove expr)))

(defun translate-from-dc (expr)
  (cond
   ((eq expr dp::*true*) *true*)
   ((eq expr dp::*false*) *false*)
   (t expr)))

(defun translate-from-prove (expr)
  (cond
   ((eq expr 'true) 'true)
   ((eq expr 'false) 'false)
   (t expr)))

(defvar *dp-print-incompatible-warning* t)

(defmacro call-process (expr dp-state alists)
  (let ((g-expr (gentemp))
	(g-dp-state (gentemp))
	(g-alists (gentemp)))
    `(let* ((g-expr ,expr)
	    (g-dp-state ,dp-state)
	    (g-alists ,alists)
	    (typealist (append *local-typealist* typealist))
	    (sigalist (dpinfo-sigalist g-alists))
	    (findalist (dpinfo-findalist g-alists))
	    (usealist (dpinfo-usealist g-alists))
	    (new-expr (when *new-ground?* (top-translate-to-dc g-expr)))
	    ;; put in (typep expr 'syntax) check
	    ;; in case call-process is called from process-assert
	    ;; which already has translated exprs
	    (old-expr (if (typep g-expr 'syntax)
			  (top-translate-to-old-prove g-expr)
			  g-expr))
	    (new-result nil)
	    (old-result nil))
       (when *new-ground?*
	 (setq new-result (translate-from-dc
			   (dp::invoke-process new-expr g-dp-state))))
       (when *old-ground?*
	 (setq old-result (translate-from-prove-list
			   (invoke-process old-expr)))
	 (setf (dpinfo-sigalist g-alists) sigalist
	       (dpinfo-findalist g-alists) findalist
	       (dpinfo-usealist g-alists) usealist))
       (let ((not-incompatible
	      (or (not (and *new-ground?* *old-ground?*))
		  (and (compatible-dp-results new-result old-result)))))
	 (when (and *dp-print-incompatible-warning*
		    (not not-incompatible))
	   (format t "~%***IncompatibleWarning*** expr: ~A,~%new-result: ~A, ~%old-result:~A"
	     new-expr new-result old-result))
	 (assert (or (not *break-on-ground-diff*)
		     not-incompatible)
		 (*break-on-ground-diff*)))
       (setq *break-on-ground-diff* t)
       (if *new-ground?*
	   new-result
	   old-result))))

(defun init-cong-state ()
  (when *new-ground?* (dp::null-single-cong-state)))

(defun new-cs (old-cs)
  (when *new-ground?*
    (dp::push-new-cong-state old-cs)))

(defun restore-old-cs (new-cs)
  (when *new-ground?*
    (dp::npop-cong-state new-cs)))

(defun init-dp (&optional strong)
  (when *new-ground?*
    (dp::init-dp-0 strong)))

(defun compatible-dp-results (new-result old-result)
  (or (tc-eq new-result old-result)
      (and (eq new-result dp::*true*) (eq old-result TRUE))
      (and (eq new-result dp::*false*) (eq old-result FALSE))
      (and (or (listp old-result)
	       (typep old-result 'syntax))
	   (null new-result))))

(defun compatible-dp-results (new-result old-result)
  (or (tc-eq new-result old-result)
      (and (true-p new-result) (true-p old-result))
      (and (true-p new-result) (null old-result))
      (and (false-p new-result) (false-p old-result))
      (and 
	   (or (listp old-result)
	       (and (not (or (true-p old-result)
			     (false-p old-result)))
		    (typep old-result 'syntax)))
	   (null new-result))))

(defvar *init-dp-state*
  (when nil
    (init-cong-state)))

(defun init-dc (&optional strong)
  (init-dp strong)
  (dp::return-all-cong-states dp::*made-cong-states*)
  (setq *init-dp-state* (init-cong-state))
  (reset-translate-from-dc)
  (reset-translate-to-dc))

