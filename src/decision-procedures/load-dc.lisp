(in-package pvs)

(eval-when (load compile eval)
  (defvar *dp* (or (find-package 'dp) (make-package 'dp))))

(defun dp::show (x) (show x))

(defvar *dp-directory* #-linux86 "/project/pvs/src/prover/dc-prototypes/"
                       #+linux86 "/home/pvs/src/prover/dc-prototypes/")

#+allegro (defun string-append (&rest args)
	    (apply #'clos::string-append args))

(defun load-dc ()
  (load (string-append *dp-directory* "load"))
  (load (string-append *dp-directory* "test"))
  (dp::load-shostak)
  ;(load (string-append *dp-directory* "dc-pvs"))
  (load (string-append *dp-directory* "translate-to-dc"))
  ;(load (string-append *dp-directory* "pvs-dc-interfaces"))
  )

(defun compile-dc (&optional (mode nil))
  (cond
   ((eq mode 'speed)
    (dp::proclaim-speed)
    (setq dp::*develop* nil))
   ((eq mode 'dev)
    (dp::proclaim-dev)
    (setq dp::*develop* t)))
  (dp::compile-shostak)
  ;(compile-file (string-append *dp-directory* "dc-pvs"))
  ;(load (string-append *dp-directory* "dc-pvs"))
  (compile-file (string-append *dp-directory* "translate-to-dc"))
  (load (string-append *dp-directory* "translate-to-dc"))
  ;(compile-file (string-append *dp-directory* "pvs-dc-interfaces"))
  ;(load (string-append *dp-directory* "pvs-dc-interfaces"))
  )


(defun my-prove-file-at (name line rerun?
			   &optional origin buffer prelude-offset
			   background? display? return-to-pvs?)
  (let ((*to-emacs* background?))
    (if *in-checker*
	(pvs-message "Must exit the current proof first")
	(multiple-value-bind (fdecl place)
	    (formula-decl-to-prove name line origin)
	  (if (and rerun?
		   fdecl
		   (null (justification fdecl)))
	      (pvs-message "Formula ~a has no proof to rerun." (id fdecl))
	      (if fdecl
		  (let ((*current-theory* (module fdecl))
			(*current-system* (if (member origin '("tccs" "ppe"))
					      'pvs
					      (intern (string-upcase
						       origin))))
			(*start-proof-display* display?)
			(ojust (extract-justification-sexp
				(justification fdecl)))
			(*justifications-changed?* nil))
		    (read-strategies-files)
		    (let ((proof (cond (background?
					(pvs-prove-decl fdecl t))
				       (t (auto-save-proof-setup fdecl)
					  (prove (id fdecl)
						 :strategy
						 (when rerun? '(rerun)))))))
		      (when (typep proof 'proofstate)
			(setq *last-proof* proof)))
		    ;; Save the proof if it is different.
		    (unless (or (equal origin "prelude")
				(from-prelude? fdecl))
		      (when (or *justifications-changed?*
				(not (equal ojust
					    (extract-justification-sexp
					     (justification fdecl)))))
			(save-all-proofs *current-theory*))
		      ;; If the proof status has changed, update the context.
		      (update-context-proof-status fdecl))
		    (remove-auto-save-proof-file)
		    (let ((*to-emacs* t))
		      (when return-to-pvs?
			(pvs-locate buffer fdecl
				    (if prelude-offset
					(vector (- (line-begin place) prelude-offset)
						(col-begin place)
						(- (line-end place) prelude-offset)
						(col-end place))
					place)))))))))))
