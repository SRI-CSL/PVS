;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrays.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/12/01 19:08:16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package pvs)


(defun collect-forward-chain-infos (name context)
  (let* ((nm (pc-parse name 'name))
	 (resolutions
	  (let ((*generate-tccs* 'ALL))
	    (resolve nm 'formula nil context)))
	 (infos (loop for res in resolutions
		      for info = (check-forward-lemma res)
		      when info
		      collect info)))
    infos))

(defun ground-forward-chain-step (names)
  #'(lambda (ps)
      (let ((new-ground-fcs (names-to-ground-fc names)))
	(if new-ground-fcs
	    (protecting-cong-state
	     ((*dp-state* (dp-state ps)))
	     (setf (dp::forward-chains-orig-rules
		    (dp::forward-chains *dp-state*))
		   (append new-ground-fcs
			   (dp::forward-chains-orig-rules
			    (dp::forward-chains *dp-state*))))
	     (setf (dp::forward-chains-partial-rules
		    (dp::forward-chains *dp-state*))
		   (append new-ground-fcs
			   (dp::forward-chains-partial-rules
			    (dp::forward-chains *dp-state*))))
	     (values '? (list (cons (copy (current-goal ps))
				    (list 'dp-state
					  *dp-state*)))))
	    (values 'X nil nil)))))

(defun names-to-ground-fc (names)
  (loop for name in names
	    nconc
	    (let ((name (if (consp name) name (list name))))
	      (loop for nm in name
		    nconc
		    (let ((fc-infos
			   (collect-forward-chain-infos
			    nm
			    *current-context*)))
		      (cond
		       ((null fc-infos)
			(format-if "~%No resolution for ~a" nm)
			nil)
		       (t (loop for info in fc-infos
				collect (fcinfo-to-ground-fc nm info)))))))))

(defun split-fc-ants (ants)
  (loop for ant in ants
	if (not-expr? ant)
	collect (args1 ant) into neg-ants
	else
	collect ant into pos-ants
	finally (return (values pos-ants neg-ants))))

(defun make-ground-fc (name pos-ants neg-ants conc)
  (let ((*translate-rewrite-rule* t))
    (let ((ground-pos (mapcar #'top-translate-to-dc pos-ants))
	  (ground-neg (mapcar #'top-translate-to-dc neg-ants))
	  (ground-conc (top-translate-to-dc conc)))
      (dp::make-forward-chain :pos-antecedents ground-pos
			      :neg-antecedents ground-neg
			      :consequent ground-conc
			      :name name))))

(defun fcinfo-to-ground-fc (name info)
  (let ((ants (cdr info))
	(conc (car info)))
    (multiple-value-bind (pos-ants neg-ants)
	(split-fc-ants ants)
      (make-ground-fc name pos-ants neg-ants conc))))

(defun install-ground-fcs (ground-fcs ps)
  (let ((ants (cdr info))
	(conc (car info)))
    (multiple-value-bind (pos-ants neg-ants)
	(split-fc-ants ants)
      (let ((ground-fc (make-ground-fc name pos-ants neg-ants conc)))
	(protecting-cong-state
	 ((*dp-state* (dp-state ps)))
	 (setf (dp::forward-chains-orig-rules
		(dp::forward-chains *dp-state*))
	       (cons ground-fc (dp::forward-chains-orig-rules
				(dp::forward-chains *dp-state*))))
	 (setf (dp::forward-chains-partial-rules
		(dp::forward-chains *dp-state*))
	       (cons ground-fc (dp::forward-chains-partial-rules
				(dp::forward-chains *dp-state*))))
	 (values '? (list (cons (current-goal ps)
				(list 'dp-state
				      *dp-state*)))))))))

(addrule 'ground-forward-chain nil (&rest names)
	 (ground-forward-chain-step names)
	 "Tells the ground decision procedure to automatically
forward-chain with names."
	 "Ground forward chaining on ~a")

(defstep ground-forward-chain-theory (name &optional exclude
					   tccs?)
  (let ((name (pc-parse name 'modname))
	(current? (eq (id name)(id *current-theory*)))
	(theory-name (resolve-theory-name name))
	(theory (get-theory theory-name))
	(exclude (if (listp exclude) exclude (list exclude)))
	(exclude (mapcar #'(lambda (x) (pc-parse x 'name)) exclude))
	(okay?  (and theory
		     (or (and current?
			      (null (actuals theory-name)))
			 (null (formals-sans-usings theory))
			 (actuals theory-name)))))
    (if theory
	(if okay?
	    (let ((all-decls (when theory
			       (append (assuming theory)
				       (theory theory))))
		  (exclude-current
		   (when current?
		     (memq (declaration *top-proofstate*)
			   all-decls)))
		  (names
		   (loop for decl in all-decls
			 when
			 (and (formula-decl? decl)
			      (or tccs?
				  (not (tcc? decl)))
			      (or (null current?)
				  (not (memq decl
					     exclude-current)))
			      (not (member (id decl)
					   exclude
					   :test #'same-id)))
			 collect
			 (let ((name (format nil "~a.~a"
				       theory-name
				       (id decl))))
			   name)))
;		  (rule `(ground-forward-chain ,@names))
;		  (stop-rule '(skip))
		  )
	      (ground-forward-chain :names names))
	    (if current?
		(skip-msg "Current theory cannot be given actuals.")
		(skip-msg "Need theory actuals to install rewrites.")))
	(skip-msg "No such theory in current context.")))
	 "Installs an entire theory as ground forward chanins.
Declarations named in EXCLUDE are not introduced.
By default, TCCs in the theory are excluded but they can be included
when the TCC? flag is T."
	 "Forward-chaining relative to the theory: ~a")

(defstep ap-assert ()
  (assert)
  "test" "test")
