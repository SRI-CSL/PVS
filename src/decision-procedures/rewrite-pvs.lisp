(in-package pvs)

(defmethod translate-to-dc ((rewrite rewrite))
  (let ((*translate-rewrite-rule* t))
    (let* ((dc-lhs (translate-to-dc (lhs rewrite)))
	   (dc-rhs (translate-to-dc (rhs rewrite)))
	   (dc-condition (translate-to-dc (hyp rewrite)))
	   (res (res rewrite))
	   (name (make-instance 'name
		   'id (id (declaration res))
		   'actuals (actuals (module-instance res))))
	   (name-string (unparse name :string t)))
      (dp::make-rewrite-rule :name name-string
			     :lhs dc-lhs
			     :rhs dc-rhs
			     :condition dc-condition))))

(defun get-pvs-rewrite-rules ()
  (let ((rewrites! nil)
	(rewrites nil))
    (maphash #'(lambda (key val)
		 (loop for rewrite in val
		       do (cond
			   ((member (res rewrite)
				    *auto-rewrites-names*)
			    (setq rewrites
				  (cons (top-translate-to-prove rewrite)
					rewrites)))
			   ((member (res rewrite)
				    *auto-rewrites!-names*)
			    (setq rewrites!
				  (cons (top-translate-to-prove rewrite)
					rewrites!)))
			   (t nil))))
	     *auto-rewrites*)
    (dp::make-rewrite-rules
     :rules! rewrites!
     :rules rewrites)))

(defun install-ground-rewrite-step ()
  #'(lambda (ps)
      (let ((rewrites! nil)
	    (rewrites nil))
	(maphash
	 #'(lambda (key val)
	     (loop for rewrite in val
		   do (cond
		       ((member (res rewrite)
				*auto-rewrites-names*)
			(let ((ground-rewrite
			       (top-translate-to-prove rewrite)))
			  (setf (dp::rr-always? ground-rewrite) nil)
			  (setq rewrites
				(cons ground-rewrite
				      rewrites))))
		       ((member (res rewrite)
				*auto-rewrites!-names*)
			(let ((ground-rewrite
			       (top-translate-to-prove rewrite)))
			  (setf (dp::rr-always? ground-rewrite) t)
			  (setq rewrites!
				(cons ground-rewrite
				      rewrites!))))
		       (t nil))))
		   *auto-rewrites*)
	     (protecting-cong-state
	      ((*dp-state* (dp-state ps)))
	      (dp::add-rewrite-rules
	       rewrites rewrites! *dp-state*)
	      (values '? (list (cons (current-goal ps)
				     (list 'dp-state
					   *dp-state*))))))))

(addrule 'install-ground-rewrites nil nil (install-ground-rewrite-step)
	 "Install's the current set of auto-rewrites into the ground prover.")

(addrule 'ground-rewrite () ((fnums *) (replace? t))
	 (ground-rewrite-step fnums replace?)
	 "Uses the ground decision procedures to rewrite the terms in
the sequent. If replace? is set it replaces the sform with its
rewritten sform, otherwise it just adds the rewritten sform.
Should be proceeded by an install-ground-rewrites.")

(defun ground-rewrite-step (sformnums replace?)
  #'(lambda (ps)
      (ground-rewrite-sformnums sformnums replace? ps)))

(defun ground-rewrite-sformnums (sformnums replace? ps)
  (let* ((goalsequent (current-goal ps))
	 (*dp-state* (dp-state ps)))
    (protecting-cong-state
     ((*dp-state* *dp-state*))
     (ground-rewrite-sequent goalsequent sformnums replace?))))

(defun ground-rewrite-sequent (sequent sformnums replace?)
  (let* ((simplifiable-sformnums
	  (find-all-sformnums (s-forms sequent) sformnums
			      #'(lambda (fmla)
				  (and nil
				  (if (not-expr? fmla)
				      (connective-occurs? (args1 fmla))
				      (connective-occurs? fmla))))))
	 (other-sformnums
	  (find-remaining-sformnums (s-forms sequent) sformnums
				    simplifiable-sformnums)))
    (multiple-value-bind (signal subgoal)
	(sequent-reduce sequent
			#'(lambda (sform)
			    (ground-rewrite-sform sform replace?))
			other-sformnums)
      (cond ((eq signal '!) (values '! nil nil))
	    ((eq signal 'X) (values 'X nil nil))
	    (t (values
		'?
		(list (cons subgoal
			    (list 'dp-state
				  *dp-state*)))))))))

(defun ground-rewrite-sform (sform replace?)
  (multiple-value-bind (signal new-sform)
      (ground-rewrite-sform* sform replace?)
    (cond ((eq signal '!) (values signal new-sform))
	  ((eq signal '?)
	   (values '? new-sform))
	  (t (values signal new-sform)))))

(defun ground-rewrite-sform* (sform replace?)
  (let ((fmla (formula sform)))
    (multiple-value-bind (sig newfmla)
	(ground-rewrite-fmla fmla)
      (cond ((eq sig '!)
	     (values sig sform))
	    ((eq sig 'X)
	     (values 'X sform))
	    ((eq sig '?)
	     (if replace?
		 (values '? (copy sform
			      'formula
			      newfmla))
		 (values '? (list sform
				  (copy sform
				    'formula
				    newfmla)))))
	    (t (break))))))

(defun ground-rewrite-fmla (fmla)
  (let ((ground-fmla (top-translate-to-prove fmla)))
    (multiple-value-bind (new-fmla change?)
	(dp::normalize-term-top ground-fmla *dp-state*)
      (if change?
	  (values '? (translate-from-dc new-fmla))
	  (values 'X fmla)))))

(defun my-translate-cases-to-if* (expr selections else-part &optional chained?)
  (cond ((and (null (cdr selections))
	      (null else-part))
	 (subst-accessors-in-selection expr (car selections)))
	((null selections)
	 else-part)
	(t (let* ((sel (car selections))
		  (thinst (module-instance (find-supertype (type expr))))
		  (cons-or-rec
		   (if (or t (args sel))
		       (subst-mod-params (recognizer (constructor sel))
					 thinst)
		       (subst-mod-params (constructor sel)
					 thinst)))
		  (cond (if (or t (args sel))
			    (make-application cons-or-rec expr)
			    (make-equation expr cons-or-rec)))
		  (then ;(subst-mod-params
			 (subst-accessors-in-selection expr sel)
			 ;thinst)
		    )
		  (else (translate-cases-to-if* expr (cdr selections)
						else-part t)))
	     (if chained?
		 (make-chained-if-expr cond then else)
		 (make-if-expr cond then else))))))
