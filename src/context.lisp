;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; context.lisp -- Context structures and accessors
;; Author          : Sam Owre
;; Created On      : Fri Oct 29 19:09:32 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun May 28 19:14:47 1995
;; Update Count    : 69
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

;;; Called from both Emacs and PVS
(export '(save-context context-usingchain))

;;; Called from Emacs only
(export '(change-context get-pvs-file-dependencies pvs-rename-file
	  show-context-path context-files-and-theories show-proof-file
	  show-proofs-pvs-file show-orphaned-proofs pvs-select-proof
	  pvs-view-proof pvs-delete-proof))

;;; Called from PVS only
(export '(handle-deleted-theories restore-from-context update-context
	  te-formula-info te-id fe-status fe-id get-theory-dependencies
	  restore-context valid-proofs-file get-context-theory-names
	  get-context-theory-entry context-file-of pvs-file-of
	  context-entry-of save-proofs copy-theory-proofs-to-orphan-file
	  read-strategies-files))

(proclaim '(inline pvs-context-version pvs-context-libraries
	    pvs-context-entries))


;;; *pvs-context-writable* indicates whether we have write permission in
;;; the current context.  It is set by change-context.

(defvar *pvs-context-writable* nil)


;;; *pvs-context-changed* controls the saving of the PVS context.  It is
;;; initially nil, and is set to t when a change occurs that affects the
;;; context, and reset to nil after the context is written again.  The changes
;;; that affect the context are:
;;;   - A new file is parsed
;;;   - A file has been modified
;;;   - A file/theory is deleted from the context
;;;   - The status of a proof has changed

(defvar *pvs-context-changed* nil
  "Set to T when the context has changed, nil after it's been saved.")

;;; The pvs-context is a list whose first element is the version, second
;;; element is a list of prelude libraries, and the rest are context entries.
;;; This is currently written directly to file, although it could be made
;;; faster by keeping a symbol table and writing arrays to file.  The version
;;; number would then provide enough information to translate to current
;;; contexts.

(defun pvs-context-version ()
  (car *pvs-context*))

(defun pvs-context-libraries ()
  (cadr *pvs-context*))

(defun pvs-context-default-decision-procedure ()
  (or (when (listp (caddr *pvs-context*))
	(getf (caddr *pvs-context*) :default-decision-procedure))
      'shostak))

(defun pvs-context-entries ()
  (if (listp (caddr *pvs-context*))
      (cdddr *pvs-context*)
      (cddr *pvs-context*)))

(defstruct (context-entry (:conc-name ce-)
			  (:print-function
			   (lambda (ce stream depth)
			     (declare (ignore depth))
			     (format stream "<CE ~a>"
			       (ce-file ce)))))
  file
  write-date
  proofs-date
  object-date ;; is now a list of object-dates corresponding to the theories
  dependencies
  theories
  extension)

(defstruct (theory-entry (:conc-name te-)
			 (:print-function
			   (lambda (te stream depth)
			     (declare (ignore depth))
			     (format stream "<TE ~a>"
			       (te-id te)))))
  id
  status
  dependencies ;; An alist ((lib-ref theory-id ...) ... (nil theory-id ...))
  formula-info)


;;; The formula-entry keeps track of the proof status and the
;;; references for the given formula id.  The status is one of the
;;; values untried, unfinished, unchecked, proved-incomplete, or
;;; proved-complete.  Note that it only reflects the default proof,
;;; and is now used only for providing the proof summary if the
;;; theories have not been parsed.

(defstruct (formula-entry (:conc-name fe-)
			  (:print-function
			   (lambda (fe stream depth)
			     (declare (ignore depth))
			     (format stream "<FE ~a>"
			       (fe-id fe)))))
  id
  status
  proof-refers-to
  decision-procedure-used
  proof-time)

(defstruct (declaration-entry (:conc-name de-)
			      (:print-function
			       (lambda (de stream depth)
				 (declare (ignore depth))
				 (format stream "<DE ~a.~a>"
				   (de-theory-id de) (de-id de)))))
  id
  class
  type
  theory-id
  (library nil))

(defun ce-eq (ce1 ce2)
  (and (equal (ce-file ce1) (ce-file ce2))
       (equal (ce-write-date ce1) (ce-write-date ce2))
       (equal (ce-proofs-date ce1) (ce-proofs-date ce2))
       (equal (ce-object-date ce1) (ce-object-date ce2))
       (length= (ce-dependencies ce1) (ce-dependencies ce2))
       (null (set-difference (ce-dependencies ce1) (ce-dependencies ce2)
			     :test #'equal))
       (length= (ce-theories ce1) (ce-theories ce2))
       (every #'te-eq (ce-theories ce1) (ce-theories ce2))
       (equal (ce-extension ce1) (ce-extension ce2))))

(defun te-eq (te1 te2)
  (and (eq (te-id te1) (te-id te2))
       (equal (te-status te1) (te-status te2))
       (length= (te-dependencies te1) (te-dependencies te2))
       (null (set-difference (te-dependencies te1) (te-dependencies te2)
			     :test #'equal))
       ;; This one is tricky, since after parsing there are no TCCs, while
       ;; after typechecking they show up.  However, since the only way to
       ;; get a difference here that matters is to change the source file,
       ;; which fe-eq will catch, we simply check that every one from on
       ;; list that can be found on the other is fe-eq.
       (every #'(lambda (fe2)
		  (let ((fe1 (find (fe-id fe2) (te-formula-info te1)
				   :key #'fe-id)))
		    (or (null fe1)
			(fe-eq fe1 fe2))))
	      (te-formula-info te2))))

(defun fe-eq (fe1 fe2)
  (and (eq (fe-id fe1) (fe-id fe2))
       (equal (fe-status fe1) (fe-status fe2))
       (length= (fe-proof-refers-to fe1) (fe-proof-refers-to fe2))
       (every #'de-eq (fe-proof-refers-to fe1) (fe-proof-refers-to fe2))))

(defun de-eq (de1 de2)
  (and (eq (de-id de1) (de-id de2))
       (eq (de-class de1) (de-class de2))
       (equal (de-type de1) (de-type de2))
       (eq (de-theory-id de1) (de-theory-id de2))))

#-gcl
(defmethod id ((entry theory-entry))
  (te-id entry))

#-gcl
(defmethod id ((entry formula-entry))
  (fe-id entry))

#+gcl
(defmethod id (entry)
  (typecase entry
    (theory-entry (te-id entry))
    (formula-entry (fe-id entry))
    (t (error "Id not applicable here"))))

(defvar *valid-entries* nil)

(defvar *strat-file-dates* (list 0 0 0)
  "Used to keep track of the file-dates for the pvs, home, and context
pvs-strategies files.")

(defun cc (directory)
  (change-context directory))


;;; Change-context checks that the specified directory exists, prompting
;;; for a different directory otherwise.  When a directory is given, the
;;; current context is saved (if writable), *last-proof* is cleared, the
;;; working-directory is set, and the context is restored.

(defun change-context (directory &optional init?)
  (let ((dir (get-valid-context-directory directory nil)))
    (unless init? (save-context))
    (setq *pvs-context-writable* (write-permission? dir))
    (setf (caddr *strat-file-dates*) 0)
    (set-working-directory dir)
    (setq *pvs-context-path* (shortpath (working-directory)))
    (setq *pvs-current-context-path* *pvs-context-path*)
    (setq *default-pathname-defaults* *pvs-context-path*)
    (clear-theories t)
    (restore-context)
    (pvs-message "Context changed to ~a"
      (shortname *pvs-context-path*))
    (when *pvs-context-writable*
      (copy-auto-saved-proofs-to-orphan-file))
    (shortname *pvs-context-path*)))

(defun reset-context ()
  (setq *last-proof* nil)
  (clrhash *pvs-files*)
  (clrhash *pvs-modules*)
  (setq *current-context* nil))

(defun get-valid-context-directory (directory prompt)
  (multiple-value-bind (dir reason)
      (directory-p (or directory
		       (pvs-prompt
			'directory
			(or prompt "Change context to (directory): "))))
    (cond ((not (pathnamep dir))
	   (get-valid-context-directory nil (format nil "~a  cc to: "
					      (protect-format-string reason))))
;; 	  ((and (write-permission? dir)
;; 		(not (file-exists-p (context-pathname dir)))
;; 		(not (pvs-y-or-n-p "Context not found - create new one? ")))
;; 	   (get-valid-context-directory nil nil))
	  (t dir))))

(defun context-pathname (&optional (dir *pvs-context-path*))
  (make-pathname :name *context-name* :defaults dir))

(defvar *dont-write-object-files* nil)

;;; Save Context - called from Emacs and parse-file (pvs.lisp), saves any
;;; changes since the last time the context was saved.

(defun save-context ()
  (cond ((not (file-exists-p *pvs-context-path*))
	 (pvs-message "Directory ~a seems to have disappeared!"
	   (namestring *pvs-context-path*)))
	(t (unless *loading-prelude*
	     (unless (or *dont-write-object-files*
			 (not *pvs-context-writable*))
	       (write-object-files))
	     (write-context)))))

(defun sc ()
  (save-context))


(defun write-context ()
  (when *pvs-context-changed*
    (if *pvs-context-writable*
	(let ((context (make-pvs-context)))
	  (multiple-value-bind (value condition)
	      (ignore-file-errors
	       (store-object-to-file context (context-pathname)))
	    (declare (ignore value))
	    (cond (condition
		   (pvs-message "~a" condition))
		  (t (setq *pvs-context* context)
		     (setq *pvs-context-changed* nil)
		     (pvs-log "Context file ~a written"
			      (namestring (context-pathname)))))))
	(pvs-log "Context file ~a not written, do not have write permission"
		 (namestring (context-pathname))))))

(defvar *testing-restore* nil)

(defun write-object-files (&optional force?)
  (update-stored-mod-depend)
  (when (and (> (hash-table-count *pvs-modules*))
	     (ensure-bin-subdirectory))
    (if *testing-restore*
	(maphash #'(lambda (id theory)
		     (declare (ignore id))
		     (dolist (imp (all-usings theory))
		       (when (theory-interpretation? (car imp))
			 (write-object-file (car imp))))
		     (write-object-file theory force?))
		 *pvs-modules*)
	(multiple-value-bind (value condition)
	    (ignore-file-errors
	     (maphash #'(lambda (id theory)
			  (declare (ignore id))
			  (dolist (imp (all-usings theory))
			    (when (theory-interpretation? (car imp))
			      (write-object-file (car imp))))
			  (write-object-file theory force?))
		      *pvs-modules*))
	  (declare (ignore value))
	  (when condition
	    (pvs-message "~a" condition))))))

(defun write-object-file (theory &optional force?)
  ;; If there is no filename, the theory must be a theory-interpretation
  ;; in which case we use the generating file.
  (let* ((gtheory (if (filename theory)
		      theory
		      (module (generated-by-decl theory))))
	 (file (filename gtheory))
	 (binpath (make-binpath (binpath-id theory)))
	 (bindate (file-write-date binpath))
	 (specpath (make-specpath file))
	 (specdate (file-write-date specpath))
	 (ce (get-context-file-entry file))
	 (te (get-context-theory-entry gtheory ce))
	 (te-date (when ce (assq (id theory) (ce-object-date ce)))))
    (when (and te specdate)
      (unless (or (not (typechecked? theory))
		  (and (not force?)
		       bindate
		       (or (theory-interpretation? theory)
			   (eql bindate (cdr te-date)))
		       (< specdate bindate)))
	(pvs-log "Saving bin file for theory ~a" (binpath-id theory))
	(save-theory theory)
	(setf (ce-object-date ce)
	      (acons (te-id te) (file-write-date binpath)
		     (delete te-date (ce-object-date ce))))
	(setq *pvs-context-changed* t)))))

(defmethod binpath-id ((th theory-interpretation))
  (makesym "~a.~a" (binpath-id (module (generated-by-decl th))) (id th)))

(defmethod binpath-id (th)
  (id th))

(defmethod binpath-name ((inm modname) (th theory-interpretation))
  (copy inm 'mod-id (id (module (generated-by-decl th)))))

(defmethod binpath-name ((inm modname) th)
  inm)


(defun ensure-bin-subdirectory ()
  (let ((subdir (make-pathname
		 :defaults *pvs-context-path*
		 :name "pvsbin")))
    (if (file-exists-p subdir)
	(if (directory-p subdir)
	    t
	    (pvs-message "~a is a regular file,~%  and can't be used as a~
                          subdirectory for .bin files unless it is moved."
	      subdir))
	(multiple-value-bind (result error)
	    (ignore-errors (excl:make-directory subdir))
	  (declare (ignore result))
	  (cond (error
		 (pvs-message "Error creating ~a: ~a" subdir error))
		(t (pvs-message "Created directory ~a" subdir)
		   t))))))
			  

(defun context-is-current ()
  (and (file-exists-p (context-pathname))
       (let ((current? t))
	 (maphash #'(lambda (fname info)
		      (declare (ignore info))
		      (let ((file (make-specpath fname))
			    (entry (get-context-file-entry fname)))
			(unless (and entry
				     (file-exists-p file)
				     (= (file-write-date file)
					(ce-write-date entry)))
			  (setq current? nil))))
		  *pvs-files*)
	 current?)))
;  (labels ((current? (entries)
;	     (or (null entries)
;		 (let ((file (make-specpath (ce-file (car entries)))))
;		   
;		   (and (probe-file file)
;			(= (file-write-date file)
;			   (ce-write-date (car entries)))
;			(current? (cdr entries)))))))
;    (current? (pvs-context-entries)))

(defun update-pvs-context ()
  (setq *pvs-context* (make-pvs-context))
  t)

(defun make-pvs-context ()
  (let ((context nil)
	(*valid-entries* (make-hash-table :test #'eq)))
    (maphash #'(lambda (name info)
		 (declare (ignore info))
		 (push (create-context-entry name) context))
	     *pvs-files*)
    (mapc #'(lambda (entry)
	      (unless (or (not (file-exists-p (make-specpath (ce-file entry))))
			  (member (ce-file entry) context
				  :key #'ce-file
				  :test #'string=))
		(push entry context)))
	  (pvs-context-entries))
    (cons *pvs-version*
	  (cons (pvs-context-libraries)
		(cons (list :default-decision-procedure
			    *default-decision-procedure*)
		      context)))))


;;; update-context is called by parse-file after parsing or
;;; typechecking a file.  It compares the context-entry of the file
;;; with the write-date of the file.  If the context entry is missing,
;;; or the write-date is different, then the *pvs-context* is updated,
;;; and the *pvs-context-changed* variable is set.  Note that this
;;; doesn't actually write out the .pvscontext file.

(defun update-context (filename)
  (let ((oce (get-context-file-entry filename))
	(nce (create-context-entry filename)))
    (unless (and (not *pvs-context-changed*)
		 oce
		 (equal (ce-write-date oce)
			(file-write-date (make-specpath filename)))
		 (ce-eq oce nce))
      (when (and oce (ce-object-date oce))
	(setf (ce-object-date nce) (ce-object-date oce)))
      (setq *pvs-context*
	    (cons *pvs-version*
		  (cons (pvs-context-libraries)
			(cons (list :default-decision-procedure
				    *default-decision-procedure*)
			      (cons nce (delete oce (pvs-context-entries)))))))
      (setq *pvs-context-changed* t))))

(defun delete-file-from-context (filename)
  (let ((ce (get-context-file-entry filename)))
    (when ce
      (setq *pvs-context* (remove ce (pvs-context-entries)))
      (setq *pvs-context-changed* t))))

(defun update-context-proof-status (fdecl)
  (let ((fe (get-context-formula-entry fdecl)))
    (if fe
	(let ((status (proof-status-symbol fdecl)))
	  (unless (eq (fe-status fe) status)
	    (setf (fe-status fe) status)
	    (setq *pvs-context-changed* t))
	  (assert (memq (fe-status fe)
			'(proved-complete proved-incomplete unchecked
					  unfinished untried nil))))
	(setq *pvs-context-changed* t))))

(defun create-context-entry (filename)
  (let* ((theories (gethash filename *pvs-files*))
	 (file-entry (get-context-file-entry filename))
	 (prf-file (make-prf-pathname filename))
	 (proofs-write-date (file-write-date prf-file))
	 (fdeps (file-dependencies filename)))
    (assert (cdr theories))
    (make-context-entry
     :file filename
     :write-date (car theories)
     :object-date (when file-entry (ce-object-date file-entry))
     :extension nil
     :proofs-date proofs-write-date
     :dependencies fdeps
     :theories (let ((tes (mapcar #'(lambda (th)
				      (create-theory-entry th file-entry))
			    (cdr theories))))
		 (assert tes)
		 tes))))

(defun create-theory-entry (theory file-entry)
  (let* ((valid? (when file-entry (valid-context-entry file-entry)))
	 (tentry (when file-entry
		   (get-context-theory-entry theory file-entry)))
	 (finfo (mapcar #'(lambda (d)
			    (create-formula-entry
			     d (when tentry (te-formula-info tentry)) valid?))
			(remove-if-not #'(lambda (d)
					   (provable-formula? d))
			  (append (assuming theory) (theory theory)))))
	 (status (if (and valid? tentry)
		     (te-status tentry)
		     (status theory))))
    (make-theory-entry
     :id (id theory)
     :status (if (generated-by theory)
		 (cons 'generated status)
		 status)
     :dependencies (get-binfile-dependencies theory)
     :formula-info (append finfo
			   (unless (memq 'typechecked (status theory))
			     (hidden-formula-entries tentry finfo valid?))))))

(defun get-binfile-dependencies (theory)
  ;; Create a list of the form
  ;; ((lib1 . (thid1 ... )) ... (nil . (thid ...)))
  (if (memq 'typechecked (status theory))
      (multiple-value-bind (i-theories i-names)
	  (all-importings theory)
	(multiple-value-bind (impg-theories impg-names)
	    (add-generated-adt-theories i-theories i-names)
	  (when (recursive-type? theory)
	    (let ((adt-ths (adt-generated-theories theory)))
	      (setf impg-theories
		    (append impg-theories adt-ths))
	      (setf impg-names
		    (append impg-names
			    (mapcar #'(lambda (ath) (mk-modname (id ath)))
			      adt-ths)))))
	  (multiple-value-bind (imp-theories imp-names)
	      (add-generated-interpreted-theories
	       impg-theories impg-names theory)
	    (let ((inames
		   (loop for ith in imp-theories
			 as inm in imp-names
			 unless (from-prelude? ith)
			 collect
			 (cons (binpath-name inm ith)
			       (when (and (library-datatype-or-theory? ith)
					  (not (eq (gethash (id ith)
							    *pvs-modules*)
						   ith)))
				 (lib-ref ith)))))
		  (libalist nil)
		  (thlist nil)
		  (*current-context* (saved-context theory)))
	      ;; 	    (assert (or (not (memq 'typechecked (status theory)))
	      ;; 			*current-context*))
	      (loop for (inm . libref) in inames
		    do (if libref
			   (let ((libentry (assoc libref libalist :test #'string=)))
			     (if libentry
				 (nconc libentry (list (id inm)))
				 (setq libalist
				       (nconc libalist (list (list libref (id inm)))))))
			   (push (makesym "~a" inm) thlist)))
	      (nconc libalist (list (cons nil (nreverse thlist))))))))
      (let ((te (get-context-theory-entry (id theory))))
	(when te
	  (te-dependencies te)))))

(defun add-generated-interpreted-theories (i-theories i-names theory
						      &optional
						      imp-theories imp-names)
  (cond ((null i-theories)
	 (dolist (use (all-usings theory))
	   (when (and (theory-interpretation? (car use))
		      (not (memq (car use) imp-theories)))
	     (push (car use) imp-theories)
	     (push (cadr use) imp-names)))
	 (values (nreverse imp-theories) (nreverse imp-names)))
	(t (multiple-value-bind (int-theories int-names)
	       (add-generated-interpreted-theories*
		(car i-theories) (car i-names) imp-theories)
	     (add-generated-interpreted-theories
	      (cdr i-theories)
	      (cdr i-names)
	      theory
	      (nconc int-theories imp-theories)
	      (nconc int-names imp-names))))))

(defun add-generated-interpreted-theories* (theory name imp-theories)
  (let ((int-theories nil)
	(int-names nil))
    (dolist (use (all-usings theory))
      (when (and (theory-interpretation? (car use))
		 (not (memq (car use) imp-theories)))
	(push (car use) int-theories)
	(push (cadr use) int-names)))
    (values (cons theory int-theories) (cons name int-names))))

(defun add-generated-adt-theories (i-theories i-names
					      &optional imp-theories imp-names)
  (if (null i-theories)
      (values (nreverse imp-theories) (nreverse imp-names))
      (add-generated-adt-theories
       (cdr i-theories)
       (cdr i-names)
       (if (recursive-type? (car i-theories))
	   (nconc (nreverse (nconc (adt-generated-theories (car i-theories))
				   (list (car i-theories))))
		  imp-theories)
	   (cons (car i-theories) imp-theories))
       (if (recursive-type? (car i-theories))
	   (let* ((aths (adt-generated-theories (car i-theories)))
		  (anames (mapcar #'(lambda (ath)
				      (copy (car i-names) 'id (id ath)))
			    aths)))
	     (nconc (nreverse (nconc anames (list (car i-names)))) imp-names))
	   (cons (car i-names) imp-names)))))
  

(defun create-formula-entry (decl fentries valid?)
  (let ((fentry (find-if #'(lambda (fe)
			     (and (typep fe 'formula-entry)
				  (same-id fe decl)))
		  fentries)))
    (make-formula-entry
     :id (id decl)
     :status (cond ((typechecked? decl)
		    (proof-status-symbol decl))
		   (fentry
		    (if valid?
			(fe-status fentry)
			(case (fe-status fentry)
			  ((proved-complete proved-incomplete proved
					    unchecked)
			   'unchecked)
			  (unfinished 'unfinished)
			  (t 'untried))))
		   (t 'untried))
     :decision-procedure-used (cond ((typechecked? decl)
				     (decision-procedure-used decl))
				    (fentry
				     (fe-decision-procedure-used fentry)))
     :proof-time (cond ((typechecked? decl)
			(let ((dpr (default-proof decl)))
			  (when dpr
			    (list (run-time dpr)
				  (real-time dpr)
				  (interactive? dpr)))))
		       (fentry (fe-proof-time fentry)))
     :proof-refers-to (if (proof-refers-to decl)
			  (mapcar #'create-declaration-entry
			    (remove-if-not
				#'(lambda (d)
				    (typep d '(and declaration
						   (not skolem-const-decl))))
							  
			      (proof-refers-to decl)))
			  (if fentry
			      (fe-proof-refers-to fentry))))))

(defun hidden-formula-entries (tentry finfo valid?)
  ;; The consp cases are for older style contexts - eventually they will
  ;; go away.
  (when tentry
    (flet ((feid (fi) (if (consp fi)
			  (car fi)
			  (fe-id fi))))
      (let ((fentries (remove-if #'(lambda (fi)
				     (let ((id (feid fi)))
				       (find-if #'(lambda (ffi)
						    (eq id (feid ffi)))
						finfo)))
			(te-formula-info tentry))))
	(unless valid?
	  (mapc #'(lambda (fe)
		    (if (consp fe)
			(if (eq (cadr fe) t) (setf (cadr fe) 'unchecked))
			(if (eq (fe-status fe) 'proved)
			    (setf (fe-status fe) 'unchecked))))
		fentries))
	fentries))))
  

(defun create-declaration-entry (decl)
  (make-declaration-entry
   :id (id decl)
   :class (type-of decl)
   :type (when (and (typed-declaration? decl)
		    (not (typep decl 'formal-type-decl)))
	   (or (declared-type-string decl)
	       (setf (declared-type-string decl)
		     (unparse (declared-type decl) :string t))))
   :theory-id (when (module decl) (id (module decl)))))


;;; Returns the list of filenames on which the input file depends.  It
;;; does this by looking at the theory dependencies and extracting the
;;; files from this.  The only wrinkle is with datatypes - a given file
;;; may both create a datatype and use it in a later theory - in this case
;;; the input filename does not depend on the generated one.  We also
;;; ignore any dependencies on theories from the prelude.

(defun file-dependencies (filename)
  (let ((deps (file-dependencies* filename)))
    deps))

(defun file-dependencies* (filename)
  (let ((theories (cdr (gethash filename *pvs-files*))))
    (if (and theories
	     (every #'(lambda (th) (memq 'typechecked (status th))) theories))
	(let ((depfiles nil))
	  (dolist (dth (all-importings theories))
	    (unless (memq dth theories)
	      (when (filename dth)
		(let ((depname (filename dth)))
		  (unless (or (from-prelude? dth)
			      (and (equal filename depname)
				   (not (library-datatype-or-theory? dth)))
			      (some #'(lambda (th)
					(and (datatype? th)
					     (eq (id th)
						 (generated-by dth))))
				    theories))
		    (when (and (library-datatype-or-theory? dth)
			       (not (file-equal
				     (directory-namestring (pvs-file-path dth))
				     (namestring *pvs-context-path*))))
		      (let ((lib-ref (get-dependent-file-lib-ref
				      (lib-ref dth))))
			(setq depname (format nil "~a~a"
					lib-ref (filename dth)))))
		    (pushnew depname depfiles
			     :test #'(lambda (x y)
				       (or (equal x filename)
					   (equal x y)))))))))
	  (delete-if #'null (nreverse depfiles)))
	(let ((entry (get-context-file-entry filename)))
	  (when entry
	    (mapcar #'string (delete-if #'null (ce-dependencies entry))))))))

;;; Given a lib-ref from a library theory (that is relative to the
;;; *pvs-current-context-path*), return a lib-ref relative to the
;;; *pvs-context-path*, which is were we are currently saving the
;;; .pvscontext file.
(defun get-dependent-file-lib-ref (lib-ref)
  (if (or (eq *pvs-current-context-path* *pvs-context-path*)
	  (not (member (char lib-ref 0) '(#\/ #\. #\~))))
      lib-ref
      (let ((lib-path (merge-pathnames lib-ref *pvs-current-context-path*)))
	(assert (file-exists-p lib-path))
	(relative-path lib-path *pvs-context-path*))))

(defun circular-file-dependencies (filename)
  (let ((deps (assoc filename *circular-file-dependencies* :test #'equal)))
    (if deps
	(cdr deps)
	(let ((cdeps (circular-file-dependencies*
		      (cdr (gethash filename *pvs-files*)))))
	  (push (cons filename (car cdeps)) *circular-file-dependencies*)
	  (car cdeps)))))

(defun circular-file-dependencies* (theories &optional deps circs)
  (if (null theories)
      circs
      (let* ((th (car theories))
	     (fname (dep-filename th)))
	(unless (assoc fname *circular-file-dependencies* :test #'equal)
	  (if (and (cdr deps)
		   (equal fname (dep-filename (car (last deps))))
		   (some #'(lambda (dep)
			     (not (equal fname (dep-filename dep))))
			 deps))
	      ;; Found a circularity
	      (circular-file-dependencies* (cdr theories) deps
					   (cons (reverse (cons th deps))
						 circs))
	      (append (circular-file-dependencies*
		       (delete-if #'(lambda (ith)
				      (or (null ith) (from-prelude? ith)))
			 (if (generated-by th)
			     (list (get-theory (generated-by th)))
			     (delete-if #'library-theory?
			       (mapcar #'(lambda (th) (get-theory th))
				 (get-immediate-usings th)))))
		       (cons th deps))
		      (circular-file-dependencies* (cdr theories)
						   deps circs)))))))

(defmethod dep-filename ((th module))
  (if (generated-by th)
      (filename (get-theory (generated-by th)))
      (filename th)))

(defmethod dep-filename ((adt recursive-type))
  (filename adt))


(defun context-usingchain (theoryref)
  (let ((uchain nil)
	(seen nil))
    (labels ((usingchain (theoryids)
	       (when theoryids
		 (let ((tid (car theoryids)))
		   (unless (or (memq tid uchain)
			       (memq tid seen))
		     (let ((te (get-context-theory-entry tid)))
		       (if (or (null te)
			       (memq 'generated (te-status te)))
			   (push tid seen)
			   (push tid uchain)))
		     (usingchain (cdr (assq nil (get-theory-dependencies tid)))))
		   (usingchain (cdr theoryids))))))
      (usingchain (list (ref-to-id theoryref))))
    (mapcar #'string uchain)))

;;; Used by dump-pvs-files

(defun get-pvs-file-dependencies (filename)
  (cons filename (file-dependencies filename)))

(defun get-theory-dependencies (theoryid)
  (let ((te (get-context-theory-entry theoryid)))
    (when te
      (te-dependencies te))))

;;; Restore context
;;; Reads in the .pvscontext file to the *pvs-context* variable.  Most of
;;; the rest of this function provides for reading previous versions of the
;;; .pvscontext

(defun restore-context ()
  (let ((ctx-file (if (eq *pvs-context-path* *pvs-current-context-path*)
		      *context-name*
		      (merge-pathnames *context-name* *pvs-context-path*))))
    (if (file-exists-p ctx-file)
	(multiple-value-bind (context error)
	    (ignore-errors (if (with-open-file (in ctx-file)
				 (and (char= (read-char in) #\()
				      (char= (read-char in) #\")))
			       (with-open-file (in ctx-file) (read in))
			       (fetch-object-from-file ctx-file)))
	  (cond (error
		 (pvs-message "PVS context unreadable - resetting")
		 (pvs-log "  ~a" error)
		 (setq *pvs-context* (list *pvs-version*))
		 (write-context))
		((duplicate-theory-entries?)
		 (pvs-message "PVS context has duplicate entries - resetting")
		 (setq *pvs-context* (list *pvs-version*))
		 (write-context))
		((same-major-version-number (car context) *pvs-version*)
		 (setq *pvs-context* context)
		 (cond ((listp (cadr context))
			(load-prelude-libraries (cadr context))
			(setq *default-decision-procedure*
			      (or (when (listp (caddr context))
				    (getf (caddr context)
					  :default-decision-procedure))
				  'shostak))
			(dolist (ce (cdddr context))
			  (unless (listp (ce-object-date ce))
			    (setf (ce-object-date ce) nil))))
		       ((typep (cadr context) 'context-entry)
			(setq *pvs-context*
			      (cons (car *pvs-context*)
				    (cons nil
					  (cons nil (cdr *pvs-context*))))))
		       (t (pvs-message "PVS context was not written correctly ~
                                      - resetting")
			  (pvs-log "  ~a" error)
			  (setq *pvs-context* (list *pvs-version*)))))
		((make-new-context-from-old context)
		 (load-prelude-libraries (cadr *pvs-context*)))
		(t (if (pvs-y-or-n-p "Context is unrecognizable - rename it? ")
		       (unless (ignore-errors
				 (rename-file *context-name* ".pvscontext-old"))
			 (pvs-message "Cannot rename .pvscontext")
			 (change-context nil))
		       (change-context nil)))))
	(setq *pvs-context* (list *pvs-version*))))
  nil)


(defun duplicate-theory-entries? ()
  (duplicates? (mapcar #'car (cdr (collect-theories))) :test #'string=))

(defvar *theories-restored* nil)
(defvar *files-seen* nil)

;;; Called from parse-file - already checked that binfiles are valid.
(defun restore-theories (filename)
  (let* ((*theories-restored* nil)
	 (*adt-type-name-pending* nil)
	 (*fetched-theory-interpretations* nil))
    (dolist (te (ce-theories (get-context-file-entry filename)))
      (loop for (libref . theories) in (te-dependencies te)
	    do (restore-theories* libref theories))
      (restore-theory (id te)))
    (assert (null *adt-type-name-pending*))
    (let ((files nil))
      (dolist (th *theories-restored*)
	(let ((file (filename th)))
	  (unless (or (member file files :test #'string=)
		      (not (some #'null (gethash file *pvs-files*))))
	    (pushnew file files))))
      (dolist (file files)
	(restore-theories file)))
    (get-theories (make-specpath filename))))

(defun restore-theories* (libref theories)
  (if libref
      (restore-imported-library-files libref theories)
      (dolist (th theories)
	(restore-theory th))))

(defun restore-theory (thname)
  (unless (gethash thname *pvs-modules*)
    (let* ((start-time (get-internal-real-time))
	   (theory (get-theory-from-binfile thname))
	   (load-time (get-internal-real-time))
	   (tes (when (filename theory)
		  (ce-theories (get-context-file-entry (filename theory)))))
	   (theories (mapcar #'(lambda (te) (gethash (id te) *pvs-modules*))
		       tes)))
      (assert theory)
      (push theory *theories-restored*)
      (unless (null (filename theory))
	(setf (gethash (filename theory) *pvs-files*)
	      (cons (file-write-date (make-specpath (filename theory)))
		    theories))
	(dolist (th theories)
	  (when (memq th *theories-restored*)
	    (update-restored-theories th)
	    ;;(assert (or (generated-by th) (typechecked? th)))
	    )))
;;       (pvs-message
;; 	  "Restored theory from ~a.bin in ~,2,-3fs (load part took ~,2,-3fs)"
;; 	thname (realtime-since start-time)
;; 	(floor (- load-time start-time) millisecond-factor))
      )))


;;; Invoked after a bin file has been restored.
(defmethod update-restored-theories ((theory module))
  (setf (formals-sans-usings theory)
	(remove-if #'(lambda (d) (typep d 'importing))
	  (formals theory)))
  ;;(generate-xref theory)
  ;;(reset-restored-types theory)
  (unless (valid-proofs-file (filename theory))
    (restore-from-context (filename theory) theory))
  (assert (saved-context theory)))

(defmethod update-restored-theories ((adt datatype))
  (setf (formals-sans-usings adt)
	(remove-if #'(lambda (d) (typep d 'importing))
	  (formals adt)))
  (let* ((adt-theories (adt-generated-theories adt))
	 (adt-file (filename (car adt-theories))))
    (assert adt-file)
    (setf (gethash adt-file *pvs-files*)
	  (cons (file-write-date (make-specpath adt-file)) adt-theories))
    (mapc #'update-restored-theories adt-theories)))

(defun make-new-context-from-old (context)
  ;; First copy the .pvscontext file
  (copy-file *context-name* ".pvscontext-old")
  ;; Now filter the context through the context upgrades.
  (let ((nctx (funcall (pvs-context-upgrade-function (car context))
		       (cdr context))))
    (when nctx
      (setq *pvs-context* nctx)
      (write-context)
      t)))

;;; There is no difference between 1.0 Beta and 1.1 Beta context structures.
;;; The difference between 1.1 Beta and 2.0 Beta is that the latter includes a
;;; list of prelude libraries and an extra slot in the context-entry for
;;; extensions. 

(defun pvs-context-upgrade-function (version)
  (cond ((string= version "1.0 Beta")
	 #'upgrade-from-1.0-beta)
	((string= version "1.1 Beta")
	 #'upgrade-from-1.1-beta)
	(t #'(lambda (context) (declare (ignore context)) nil))))

(defun upgrade-from-1.1-beta (context)
  (upgrade-from-1.0-beta context))

(defun upgrade-from-1.0-beta (context)
  (cons *pvs-version* (cons nil (cdr context))))



(defun same-major-version-number (v1 v2)
  (and (stringp v1) (stringp v2)
       (let ((i1 (parse-integer v1 :junk-allowed t))
	     (i2 (parse-integer v2 :junk-allowed t)))
	 (and (integerp i1) (integerp i2) (= i1 i2)))))

(defun major-version (&optional (vers *pvs-version*))
  (parse-integer vers :junk-allowed t))

(defun pvs-rename-file (file new-name)
  (ignore-file-errors (rename-file file new-name)))


;;; Show Context Path

(defun show-context-path ()
  (pvs-message (shortname *pvs-context-path*)))


;;; For a given filename, valid-context-entry returns two values: the
;;; first is a boolean indicating whether the entry is valid, and the
;;; second is the entry itself.

;;; The entry is valid if:

;;;  1. The ce-write-date of the entry matches the file-write-date of
;;;     the corresponding pvs-file
;;;  2. Every dependent file has a valid context entry 

#-gcl
(defmethod valid-context-entry (filename)
  (let ((entry (get-context-file-entry filename)))
    (and entry
	 (values (valid-context-entry entry) entry))))

#-gcl
(defmethod valid-context-entry ((entry context-entry))
  (if *valid-entries*
      (clrhash *valid-entries*)
      (setq *valid-entries* (make-hash-table :test #'equal)))
  (valid-context-entry* entry))

#+gcl
(defmethod valid-context-entry (filename)
  (if (typep filename 'context-entry)
      (let ((entry filename))
	(if *valid-entries*
	    (clrhash *valid-entries*)
	    (setq *valid-entries* (make-hash-table :test #'equal)))
	(valid-context-entry* entry))
      (let ((entry (get-context-file-entry filename)))
	(and entry
	     (values (valid-context-entry entry) entry)))))

(defun valid-context-entry* (entry)
  (multiple-value-bind (valid? there?)
      (gethash (ce-file entry) *valid-entries*)
    (if there?
	valid?
	(let ((file (make-specpath (ce-file entry))))
	  ;; First set it to nil to stop recursing
	  (setf (gethash (ce-file entry) *valid-entries*) nil)
	  ;; Then set it to the real value
	  (setf (gethash (ce-file entry) *valid-entries*)
		(valid-context-entry-file entry file))))))

(defun valid-context-entry-file (entry file)
  (and file
       (equal (file-write-date file)
	      (ce-write-date entry))
       (every #'(lambda (d)
		  (let* ((fentry (get-context-file-entry d))
			 (dfile (when fentry
				  (make-specpath (ce-file fentry)))))
		    (if fentry
			(and (ce-write-date fentry)
			     (equal (file-write-date dfile)
				    (ce-write-date fentry)))
			(let ((dir (pathname-directory d)))
			  (not (or (null dir)
				   (file-equal (make-pathname :directory dir)
					       *pvs-context-path*)))))))
	      (ce-dependencies entry))))

(defmethod get-context-file-entry ((filename string))
  (car (member filename (pvs-context-entries)
	       :test #'(lambda (x y)
			 (string= x (ce-file y))))))

(defmethod get-context-file-entry ((theory module))
  (get-context-file-entry (filename theory)))

(defmethod get-context-file-entry ((decl declaration))
  (get-context-file-entry (theory decl)))

(defun context-files-and-theories (&optional context)
  (when (or (null context)
	    (file-exists-p context))
    (if (or (null context)
	    (file-equal context *pvs-context-path*))
	(sort (mapcan #'(lambda (e)
			  (let ((file (format nil "~a.~a"
					(ce-file e)
					(or (ce-extension e) "pvs"))))
			    (mapcar #'(lambda (te) (list (string (id te))
							 file))
				    (ce-theories e))))
		      (pvs-context-entries))
	      #'string-lessp :key #'car)
	(let ((cfile (make-pathname :defaults context :name ".pvscontext")))
	  (when (file-exists-p cfile)
	    (multiple-value-bind (context error)
		(ignore-errors
		  (if (with-open-file (in cfile)
			(and (char= (read-char in) #\()
			     (char= (read-char in) #\")))
		      (with-open-file (in cfile) (read in))
		      (fetch-object-from-file cfile)))
	      (cond (error
		     (pvs-message "Error reading context file ~a"
		       (namestring cfile))
		     (pvs-log (format nil "  ~a" error))
		     nil)
		    (t (sort (mapcan
			      #'(lambda (e)
				  (let ((file (format nil "~a.~a"
						(ce-file e)
						(or (ce-extension e)
						    "pvs"))))
				    (mapcar #'(lambda (te)
						(list (string (id te))
						      file))
					    (ce-theories e))))
			      (cddr context))
			     #'string-lessp :key #'car)))))))))

(defun get-context-theory-names (filename)
  (let ((file-entry (get-context-file-entry filename)))
    (when file-entry
      (mapcar #'(lambda (e) (te-id e))
	      (ce-theories file-entry)))))

(defun get-context-theory-entry (theoryname &optional file-entry)
  (if file-entry
      (car (member (ref-to-id theoryname) (ce-theories file-entry)
		   :test #'(lambda (id e) (eq id (te-id e)))))
      (get-context-theory-entry* theoryname (pvs-context-entries))))

(defun get-context-theory-entry* (theoryname file-entries)
  (when file-entries
    (or (get-context-theory-entry theoryname (car file-entries))
	(get-context-theory-entry* theoryname (cdr file-entries)))))

(defun get-context-formula-entry (fdecl &optional again?)
  (unless (from-prelude? fdecl)
    (let ((te (get-context-theory-entry (module fdecl))))
      (cond (te
	     (find-if #'(lambda (fe)
			  (eq (id fdecl) (fe-id fe)))
	       (te-formula-info te)))
	    (again?
	     (error "something's wrong with the context"))
	    (t (update-pvs-context)
	       (get-context-formula-entry fdecl t))))))

(defun context-file-of (theoryref)
  (context-file-of* (ref-to-id theoryref) (pvs-context-entries)))

(defun pvs-file-of (theoryref)
  (multiple-value-bind (file ext)
      (context-file-of theoryref)
    (shortname (make-specpath file (or ext "pvs")))))

(defun context-file-of* (theoryid entries)
  (when entries
    (if (member theoryid (ce-theories (car entries))
		:test #'(lambda (x y) (eq x (te-id y))))
	(values (ce-file (car entries)) (ce-extension (car entries)))
	(context-file-of* theoryid (cdr entries)))))

(defun context-entry-of (theoryref)
  (labels ((entry (id entries)
	     (when entries
	       (if (member id (ce-theories (car entries))
			   :test #'(lambda (x y) (eq x (te-id y))))
		   (car entries)
		   (entry id (cdr entries))))))
    (entry (ref-to-id theoryref) (pvs-context-entries))))

(defun context-entry-of-file (file)
  (find-if #'(lambda (ce)
	       (string= (ce-file ce) file))
    (pvs-context-entries)))


;;; Called from typecheck-theories in pvs.lisp and from
;;; update-restored-theories
;;; First restores the proofs, then 

(defun restore-from-context (filename theory &optional proofs)
  (restore-proofs filename theory proofs))

(defun get-declaration-entry-decl (de)
  (get-referenced-declaration*
   (de-id de)
   (de-class de)
   (de-type de)
   (de-theory-id de)
   (de-library de)))

(defun get-referenced-declaration (declref)
  (apply #'get-referenced-declaration* declref))

(defun get-referenced-declaration* (id class &optional type theory-id library)
  (if (eq class 'module)
      (let ((th (get-theory id)))
	(assert th)
	th)
      (if (and (memq id '(+ - * /))
	       (eq theory-id 'reals))
	  ;; Needed for backward compatibility after introduction of
	  ;; number_field
	  (let* ((theory (get-theory (mk-modname 'number_fields)))
		 (decls (remove-if-not
			    #'(lambda (d)
				(and (typep d 'declaration)
				     (eq (id d) id)
				     (eq (type-of d) class)))
			  (all-decls theory))))
	    (assert decls)
	    (if (singleton? decls)
		(car decls)
		;; must be minus, check for type = "[real -> real]"
		(if (string= type "[real -> real]")
		    (cadr decls)
		    (car decls))))
	  (let ((theory
		 (or (and library
			  (let ((prehash
				 (cadr (gethash library
						*prelude-libraries*))))
			    (and prehash (gethash theory-id prehash))))
		     (get-theory (mk-modname theory-id nil
					     (when library
					       (get-lib-id library))))
		     (and (null library)
			  ;; Old proofs may not have library
			  ;; set, so we look for a unique one
			  (let ((theories (get-imported-theories
					   theory-id)))
			    (if (cdr theories)
				(pvs-message
				    "Loading Proof Error: theory reference ambiguous - ~a"
				  (id (car theories)))
				(car theories)))))))
	    (when theory
	      (let ((decls (remove-if-not
			       #'(lambda (d)
				   (and (typep d 'declaration)
					(eq (id d) id)
					(eq (type-of d) class)))
			     (all-decls theory))))
		;;(assert decls)
		(cond ((singleton? decls)
		       (car decls))
		      ((and (cdr decls) type)
		       (let ((ndecls (remove-if-not
					 #'(lambda (d)
					     (string= (unparse (declared-type d)
							:string t)
						      type))
				       decls)))
			 (when (singleton? ndecls)
			   (car ndecls)))))))))))

(defun invalidate-context-formula-proof-info (filename file nth)
  (unless (or (null (get-context-file-entry filename))
	      (and (ce-write-date (get-context-file-entry filename))
		   (= (file-write-date file)
		      (ce-write-date (get-context-file-entry filename)))))
    (dolist (ce (pvs-context-entries))
      (when (and (member filename (ce-dependencies ce) :test #'string=)
		 (not (gethash (ce-file ce) *pvs-files*)))
	(setf (ce-write-date ce) 0)
	(setf (ce-object-date ce) 0)
	(setq *pvs-context-changed* t))
      (dolist (te (ce-theories ce))
	(when (and (member (string (id nth)) (te-dependencies te))
		   (not (get-theory (te-id te))))
	  (invalidate-theory-proofs te))))))

(defun invalidate-theory-proofs (te)
  (dolist (fe (te-formula-info te))
    (when (memq (fe-status fe)
		'(proved-complete proved-incomplete))
      (setf (fe-status fe) 'unchecked)
      (setq *pvs-context-changed* t))))
  

(defun invalidate-proofs (theory)
  (when theory
    (let ((te (get-context-theory-entry (id theory))))
      (when te
	(invalidate-theory-proofs te)))
    (mapc #'(lambda (d)
	      (when (typep d 'formula-decl)
		(mapc #'(lambda (prinfo)
			  (when (memq (status prinfo)
				      '(proved proved-complete
					       proved-incomplete))
			    (setf (status prinfo) 'unchecked)))
		      (proofs d))))
	  (append (assuming theory) (theory theory)))))

(defmethod valid-proofs-file ((entry context-entry))
  (and (valid-context-entry entry)
       (valid-proofs-file (ce-file entry))))

(defmethod valid-proofs-file (filename)
  (multiple-value-bind (valid? entry)
      (valid-context-entry filename)
    (and valid?
	 (let ((prf-file (make-prf-pathname filename)))
	   (if (probe-file prf-file)
	       (eql (file-write-date prf-file)
		    (ce-proofs-date entry))
	       (null (ce-proofs-date entry)))))))

;;; Proof handling functions - originally provided by Shankar.

(defun save-all-proofs (&optional theory)
  (unless (or *loading-prelude*
	      (and theory (from-prelude? theory)))
    (if theory
	(save-proofs (make-prf-pathname (filename theory))
		     (cdr (gethash (filename theory) *pvs-files*)))
	(maphash #'(lambda (file mods)
		     (when (some #'has-proof? (cdr mods))
		       (save-proofs (make-prf-pathname file) (cdr mods))))
		 *pvs-files*))))

(defun has-proof? (mod)
  (some #'(lambda (d) (and (formula-decl? d) (justification d)))
	(append (assuming mod)
		(when (module? mod) (theory mod)))))

(defvar *save-proofs-pretty* t)

(defvar *validate-saved-proofs* t)

(defvar *number-of-proof-backups* 1)

(defun toggle-proof-prettyprinting ()
  (setq *save-proofs-pretty* (not *save-proofs-pretty*)))

(defun save-proofs (filestring theories)
  (if *pvs-context-writable*
      (let* ((oldproofs (read-pvs-file-proofs filestring))
	     (curproofs (collect-theories-proofs theories)))
	#+pvsdebug
	(current-proofs-contain-old-proofs curproofs oldproofs theories)
	(unless (equal oldproofs curproofs)
	  (when (and (file-exists-p filestring)
		     (> *number-of-proof-backups* 0))
	    (backup-proof-file filestring))
	  (multiple-value-bind (value condition)
	      (ignore-file-errors
	       (with-open-file (out filestring :direction :output
				    :if-exists :supersede)
		 (mapc #'(lambda (prf)
			   (write prf :length nil :level nil :escape t
				  :pretty *save-proofs-pretty*
				  :stream out)
			   (when *save-proofs-pretty* (terpri out)))
		       curproofs)
		 (terpri out)))
	    (declare (ignore value))
	    (cond ((or condition
		       (setq condition
			     (and *validate-saved-proofs*
				  (invalid-proof-file filestring curproofs))))
		   (if (pvs-yes-or-no-p
			"Error writing out proof file:~%  ~a~%Try again?"
			condition)
		       (save-proofs filestring theories)))
		  (t (dolist (th theories)
		       (let ((te (get-context-theory-entry (id th))))
			 (when (and te (memq 'invalid-proofs (te-status te)))
			   (setf (te-status te)
				 (delete 'invalid-proofs (te-status te))))))
		     (pvs-log "Wrote proof file ~a"
			      (file-namestring filestring)))))))
      (pvs-message
	  "Do not have write permission for saving proof files")))

(defun backup-proof-file (file)
  (let ((filestring (namestring file)))
    (if (= *number-of-proof-backups* 1)
	(let ((nfile (concatenate 'string filestring "~")))
	  (rename-file filestring (concatenate 'string
				    (namestring filestring) "~"))
	  (pvs-log "Renamed ~a to ~a"
		   (file-namestring filestring)
		   (file-namestring nfile)))
	(let* ((files (directory (concatenate 'string filestring ".~*~")))
	       (numbers (mapcar #'(lambda (fname)
				    (parse-integer (pathname-type fname)
						   :start 1 :junk-allowed t))
			  files))
	       (min (when numbers (apply #'min numbers)))
	       (max (if numbers (apply #'max numbers) 0)))
	  (when (= (length files) *number-of-proof-backups*)
	    (delete-file (format nil "~a.~~~d~~" filestring min)))
	  (let ((nfile (format nil "~a.~~~d~~" filestring (1+ max))))
	    (rename-file filestring nfile)
	    (pvs-log "Renamed ~a to ~a"
		     (file-namestring filestring)
		     (file-namestring nfile)))))))

(defun invalid-proof-file (filestring &optional outproofs)
  (with-open-file (in filestring :direction :input)
    (multiple-value-bind (inproofs condition)
	(invalid-proof-file* in (unless outproofs t))
      (or condition
	  (and outproofs
	       (not (equal inproofs outproofs))
	       (format nil
		   "Proof file ~a was not written correctly (not caught by lisp)"
		 filestring))))))

(defun invalid-proof-file* (input &optional proofs)
  (multiple-value-bind (prfs condition)
      (ignore-errors (read input nil nil))
    (cond (condition
	   (values nil condition))
	  ((null prfs)
	   (or (eq proofs t)
	       (nreverse proofs)))
	  (t (invalid-proof-file* input (when (listp proofs)
					  (cons prfs proofs)))))))

(defun current-proofs-contain-old-proofs (curproofs oldproofs theories)
  (dolist (theory theories)
    (current-proofs-contain-old-proofs*
     (cdr (assq (id theory) curproofs))
     (cdr (assq (id theory) oldproofs))
     theory)))

(defun current-proofs-contain-old-proofs* (curproofs oldproofs theory)
  (dolist (fmla (provable-formulas theory))
    (current-proofs-contain-old-proofs**
     (cdr (assq (id fmla) curproofs))
     (cdr (assq (id fmla) oldproofs))
     fmla)))

(defun current-proofs-contain-old-proofs** (curproof oldproof fmla)
  (declare (ignore fmla))
  (assert (or curproof (not oldproof))))


(defun collect-theories-proofs (theories)
  (let ((curproofs (collect-theories-proofs* theories nil)))
    curproofs))

(defun collect-theories-proofs* (theories proofs)
  (if (null theories)
      (nreverse proofs)
      (let* ((theory (get-theory (car theories))))
	(collect-theories-proofs*
	 (cdr theories)
	 (if theory
	     (cons (collect-theory-proofs theory) proofs)
	     proofs)))))

(defun collect-theory-proofs (theory)
  (cons (id theory)
	(collect-theory-proofs* (append (assuming theory) (theory theory))
				nil)))

(defun collect-theory-proofs* (decls proofs)
  (if (null decls)
      (nreverse proofs)
      (let* ((decl (car decls))
	     (prfs (collect-decl-proofs decl)))
	(collect-theory-proofs*
	 (cdr decls)
	 (if prfs
	     (cons prfs proofs)
	     proofs)))))

(defmethod collect-decl-proofs ((decl formula-decl))
  (let ((prfs (proofs decl)))
    (when prfs
      (cons (id decl)
	    (cons (position (default-proof decl) prfs)
		  (mapcar #'sexp prfs))))))

(defmethod collect-decl-proofs (obj)
  (declare (ignore obj))
  nil)

(defun merge-proofs (oldproofs proofs)
  (if (null oldproofs)
      (nreverse proofs)
      (merge-proofs (cdr oldproofs)
		    (if (assq (caar oldproofs) proofs)
			proofs
			(nconc proofs (list (car oldproofs)))))))

(defun restore-proofs (filename theory &optional proofs)
  (if proofs
      (let ((tproofs (cdr (assq (id theory) proofs))))
	(restore-theory-proofs theory tproofs))
      (let ((prfpath (make-prf-pathname filename)))
	(when (file-exists-p prfpath)
	  (multiple-value-bind (ignore error)
	      (ignore-lisp-errors
		(with-open-file (input prfpath :direction :input)
		  (restore-theory-proofs-from-file input prfpath theory)))
	    (declare (ignore ignore))
	    (when error
	      (pvs-message "Error reading proof file ~a"
		(namestring prfpath))
	      (pvs-log (format nil "  ~a" error))))))))

(defun restore-theory-proofs-from-file (input filestring theory)
  (let ((theory-proofs (read input nil nil)))
    (when theory-proofs
      (let* ((theoryid (car theory-proofs))
	     (proofs (cdr theory-proofs)))
	(unless (every #'consp proofs)
	  (pvs-message "Proofs file ~a is corrupted, will try to keep going."
	    filestring)
	  (setq proofs (remove-if-not #'consp proofs)))
	(if (eq theoryid (id theory))
	    (restore-theory-proofs theory proofs)
	    (restore-theory-proofs-from-file input filestring theory))))))

(defun restore-theory-proofs (theory proofs)
  (let ((te (get-context-theory-entry (id theory)))
	(restored (mapcar #'(lambda (decl)
			      (restore-theory-proofs* decl proofs))
		    (append (assuming theory)
			    (theory theory)))))
    (when (and te (memq 'invalid-proofs (te-status te)))
      (invalidate-proofs theory))
    (copy-proofs-to-orphan-file
     (id theory) (set-difference proofs restored :test #'equal))))

(defun restore-theory-proofs* (decl proofs)
  (when (formula-decl? decl)
    (let ((prf-entry (find-associated-proof-entry decl proofs)))
      (cond ((integerp (cadr prf-entry))
	     ;; Already in multiple-proof form, but may need to be lower-cased
	     (let ((wrong-case? (memq (nth 9 (car (cddr prf-entry)))
				      '(NIL T))))
	       (setf (proofs decl)
		     (mapcar #'(lambda (p)
				 (apply #'mk-proof-info
				   (if wrong-case?
				       (cons (car p)
					     (convert-proof-form-to-lowercase
					      (cdr p)))
				       p)))
		       (cddr prf-entry))))
	     (setf (default-proof decl)
		   (nth (cadr prf-entry) (proofs decl))))
	    (prf-entry
	     ;; Need to convert from old form
	     (let ((proof (if (consp (car (cdr prf-entry)))
			      (cddr prf-entry)
			      (cdr prf-entry))))
	       (unless (some #'(lambda (prinfo)
				 (equal (script prinfo) proof))
			   (proofs decl))
		 (let ((prinfo (make-proof-info (convert-proof-form-to-lowercase
						 proof)
						(next-proof-id decl)))
		       (fe (get-context-formula-entry decl)))
		   (when fe
		     (dolist (dref (fe-proof-refers-to fe))
		       (pushnew (get-declaration-entry-decl dref)
				(refers-to prinfo)))
		     (setf (status prinfo) (fe-status fe)))
		   (push prinfo (proofs decl))
		   (setf (default-proof decl) prinfo))))))
      prf-entry)))

(defun convert-proof-form-to-lowercase (proof-form)
  (cond ((symbolp proof-form)
	 (intern (string-downcase proof-form) (symbol-package proof-form)))
	((consp proof-form)
	 (cons (convert-proof-form-to-lowercase (car proof-form))
	       (convert-proof-form-to-lowercase (cdr proof-form))))
	(t proof-form)))

(defmethod find-associated-proof-entry ((decl tcc-decl) proofs)
  (or (assq (id decl) proofs)
      (let ((old-id (cdr (assq (id decl) *old-tcc-names*))))
	(when old-id
	  (assq old-id proofs)))))

(defmethod find-associated-proof-entry ((decl declaration) proofs)
  (assq (id decl) proofs))

(defun copy-theory-proofs-to-orphan-file (theoryref)
  (when *pvs-context-writable*
    (let ((filename (context-file-of theoryref))
	  (tid (ref-to-id theoryref))
	  (tproofs (read-theory-proofs theoryref))
	  (oproofs (read-orphaned-proofs)))
      (ignore-file-errors
       (with-open-file (orph "orphaned-proofs.prf"
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
	 (mapc #'(lambda (prf)
		   (let ((oprfs (remove-if-not
				    #'(lambda (prf)
					(and (equal filename (car prf))
					     (eq tid (cadr prf))))
				  oproofs)))
		     (unless (member prf oprfs
				     :test #'(lambda (x y) (equal x (cdr y))))
		       (write (cons filename (cons (car tproofs) prf))
			      :length nil :level nil :escape t :pretty nil
			      :stream orph))))
	       (cdr tproofs)))))))

(defun copy-proofs-to-orphan-file (theoryid proofs)
  (when (and *pvs-context-writable* proofs)
    (let ((oproofs (read-orphaned-proofs))
	  (filename (context-file-of theoryid))
	  (count 0))
      (ignore-file-errors
       (with-open-file (orph "orphaned-proofs.prf"
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
	 (mapc #'(lambda (prf)
		   (let ((oprfs (remove-if-not
				    #'(lambda (prf)
					(and (equal filename (car prf))
					     (eq theoryid (cadr prf))))
				  oproofs)))
		     (unless (member prf oprfs
				     :test #'(lambda (x y) (equal x (cddr y))))
		       (incf count)
		       (write (cons filename (cons theoryid prf))
			      :length nil :level nil :escape t :pretty nil
			      :stream orph))))
	       proofs)))
      (unless (zerop count)
	(pvs-message
	    "Added ~d proof~:p from theory ~a to orphaned-proofs.prf"
	  count theoryid)))))

(defun read-theory-proofs (theoryref)
  (let* ((filename (context-file-of theoryref))
	 (tid (ref-to-id theoryref))
	 (prffile (when filename (make-prf-pathname filename))))
    (when (and prffile (file-exists-p prffile))
      (multiple-value-bind (proofs error)
	  (with-open-file (in prffile :direction :input)
	    (labels ((findprfs ()
			       (let ((prfs (read in nil nil)))
				 (when prfs
				   (if (eq (car prfs) tid)
				       prfs
				       (findprfs))))))
	      (findprfs)))
	(cond (error
	       (pvs-message "Error reading proof file ~a"
		 (namestring prffile))
	       (pvs-log (format nil "  ~a" error))
	       nil)
	      (t proofs))))))

(defun read-pvs-file-proofs (filename &optional (dir *pvs-context-path*))
  (let ((prf-file (make-prf-pathname filename dir)))
    (when (file-exists-p prf-file)
      (multiple-value-bind (proofs error)
	  (ignore-lisp-errors
	    (with-open-file (input prf-file :direction :input)
	      (labels ((reader (proofs)
			       (let ((nproof (read input nil 'eof)))
				 (if (eq nproof 'eof)
				     (nreverse proofs)
				     (let ((cproof
					    (convert-proof-case-if-needed
					     nproof)))
				       (reader (if (member cproof proofs
							   :test #'equal)
						   proofs
						   (cons cproof proofs))))))))
		(reader nil))))
	(cond (error
	       (pvs-message "Error reading proof file ~a"
		 (namestring prf-file))
	       (pvs-log (format nil "  ~a" error))
	       nil)
	      (t proofs))))))

(defun convert-proof-case-if-needed (proofs)
  (if (integerp (cadr (car (cdr proofs))))
      proofs
      (cons (car proofs)
	    (mapcar #'(lambda (pform)
			(cons (car pform)
			      (convert-proof-form-to-lowercase
			       (cdr pform))))
	      (cdr proofs)))))

(defun read-orphaned-proofs (&optional theoryref)
  (when (file-exists-p "orphaned-proofs.prf")
    (ignore-file-errors
     (with-open-file (orph "orphaned-proofs.prf"
			   :direction :input)
       (let ((proofs nil)
	     (tid (when theoryref (ref-to-id theoryref))))
	 (labels ((readprf ()
			   (let ((prf (read orph nil 'eof)))
			     (unless (eq prf 'eof)
			       (when (or (null tid)
					 (eq tid (cadr prf)))
				 (pushnew prf proofs :test #'equal))
			       (readprf)))))
	   (readprf))
	 proofs)))))

(defvar *displayed-proofs* nil
  "The proofs currently being displayed in the Proofs buffer")

(defun show-proof-file (context filename)
  (let ((proofs (read-pvs-file-proofs filename context)))
    (cond (proofs
	   (setq *displayed-proofs*
		 (mapcan #'(lambda (thprfs)
			     (mapcar #'(lambda (prf)
					 (cons filename
					       (cons (car thprfs) prf)))
				     (cdr thprfs)))
			 proofs))
	   (pvs-buffer "Proofs"
	     (format nil "~{~a~^~%~}"
	       (mapcar #'(lambda (prf)
			   (format nil "~20a~20a~20a"
			     (car prf) (cadr prf) (caddr prf)))
		       (cons (list "File" "Theory" "Formula")
			     (cons (list "----" "------" "-------")
				   *displayed-proofs*))))
	     'popto t)
	   t)
	  (t (pvs-message "No proof file for ~a in ~a"
	       filename context)))))


(defun show-orphaned-proofs ()
  (let ((proofs (read-orphaned-proofs)))
    (cond (proofs
	   (setq *displayed-proofs* proofs)
	   (pvs-buffer "Proofs"
	     (format nil "~{~a~^~%~}"
	       (mapcar #'(lambda (prf)
			   (format nil "~20a~20a~20a"
			     (car prf) (cadr prf) (caddr prf)))
		       (cons (list "File" "Theory" "Formula")
			     (cons (list "----" "------" "-------")
				   proofs))))
	     'popto t)
	   t)
	  (t (pvs-message "No orphaned proofs available in this context")))))

(defun pvs-select-proof (num)
  (let ((proof (nth num *displayed-proofs*)))
    (if proof
	(pvs-buffer "Proof"
	  (with-output-to-string (out)
	    (format out ";;; Proof for formula ~a.~a~%"
	      (cadr proof) (caddr proof))
	    (let ((just (if (integerp (cadddr proof))
			    (fifth (nth (cadddr proof) (cddddr proof)))
			    (cdddr proof))))
	      (write (editable-justification just)
		     :stream out :pretty t :escape t :level nil :length nil
		     :pprint-dispatch *proof-script-pprint-dispatch*)))
	  t)
	(pvs-message "Cannot find proof for this entry"))))

(defun pvs-view-proof (num)
  (let ((proof (nth num *displayed-proofs*)))
    (if proof
	(pvs-buffer "View Proof"
	  (with-output-to-string (out)
	    (write (editable-justification (cdddr proof))
		   :stream out :pretty t :escape t :level nil :length nil
		   :pprint-dispatch *proof-script-pprint-dispatch*))
	  t)
	(pvs-message "Cannot find proof for this entry"))))

(defun pvs-delete-proof (num)
  (if *pvs-context-writable*
      (let ((proof (nth num *displayed-proofs*)))
	(cond (proof
	       (setq *displayed-proofs* (remove proof *displayed-proofs*))
	       (ignore-file-errors
		(with-open-file (orph "orphaned-proofs.prf"
				      :direction :output
				      :if-exists :supersede
				      :if-does-not-exist :create)
		  (dolist (prf *displayed-proofs*)
		    (write prf :length nil :escape t :pretty nil
			   :stream orph))))
	       (pvs-buffer "Proofs"
		 (when *displayed-proofs*
		   (format nil "~{~a~^~%~}"
		     (mapcar #'(lambda (prf)
				 (format nil "~20a~20a~20a"
				   (car prf) (cadr prf) (caddr prf)))
			     (cons (list "File" "Theory" "Formula")
				   (cons (list "----" "------" "-------")
					 *displayed-proofs*)))))
		 'popto t)
	       (pvs-message (if *displayed-proofs* "" "No more orphaned proofs"))
	       t)
	      (t (pvs-message "Cannot find proof for this entry"))))
      (pvs-message "Do not have write permission in this context")))

(defun read-strategies-files ()
  (let ((pvs-strat-file (merge-pathnames
			 (directory-p (environment-variable "PVSPATH"))
			 "pvs-strategies"))
	(home-strat-file (merge-pathnames "~/" "pvs-strategies"))
	(ctx-strat-file (merge-pathnames *pvs-context-path*
					 "pvs-strategies")))
    (load-strategies-file pvs-strat-file *strat-file-dates*)
    (load-strategies-file home-strat-file (cdr *strat-file-dates*))
    (load-library-strategy-files)
    (load-strategies-file ctx-strat-file (cddr *strat-file-dates*))))

(defvar *library-strategy-file-dates* nil)

(defun load-library-strategy-files ()
  (dolist (lib (current-library-pathnames))
    (load-library-strategy-file lib)))

(defun load-library-strategy-file (lib)
  (let* ((sys:*load-search-list* *pvs-library-path*)
	 (*default-pathname-defaults* lib)
	 (file (merge-pathnames lib "pvs-strategies")))
    (when (file-exists-p file)
      (let ((fwd (file-write-date file))
	    (entry (assoc lib *library-strategy-file-dates* :test #'equal)))
	(unless (and (cdr entry)
		     (= fwd (cdr entry)))
	  (if entry
	      (setf (cdr entry) fwd)
	      (push (cons lib fwd) *library-strategy-file-dates*))
	  (unless (ignore-errors (load file :verbose nil))
	    (with-open-file (str file :direction :input)
	      (if *testing-restore*
		  (load str :verbose t)
		  (unless (ignore-errors (load str :verbose nil))
		    (pvs-message "Error in loading ~a" file))))))))))

(defun load-strategies-file (file dates)
  (when (file-exists-p file)
    (let ((fwd (file-write-date file)))
      (unless (= fwd (car dates))
	(setf (car dates) fwd)
	#+(version>= 6)
	(unwind-protect
	    (progn (excl:set-case-mode :case-insensitive-lower)
		   (multiple-value-bind (v err)
		       (ignore-errors (load file))
		     (declare (ignore v))
		     (when err
		       (pvs-message "Error in loading ~a:~%  ~a" file err))))
	  (excl:set-case-mode :case-sensitive-lower)
	  (add-lowercase-prover-ids))
	#-(version>= 6)
	(with-open-file (str file :direction :input)
	  (multiple-value-bind (v err)
	      (ignore-errors (load str))
	    (declare (ignore v))
	    (when err
	      (pvs-message "Error in loading ~a:~%  ~a" file err))))))))

(defun add-lowercase-prover-ids ()
  (add-lowercase-prover-hash-ids *rulebase*)
  (add-lowercase-prover-hash-ids *rules*)
  (add-lowercase-prover-hash-ids *steps*)
  (add-lower-case-prover-keywords))

(defun add-lowercase-prover-hash-ids (hash)
  (let ((new-entries nil))
    (maphash #'(lambda (id entry)
		 (when (some #'upper-case-p (string id))
		   (let* ((lid (intern (string-downcase (string id))))
			  (lentry (gethash lid hash)))
		     (unless (eq entry lentry)
		       (push (cons lid entry) new-entries)))))
	     hash)
    (dolist (ent new-entries)
      (setf (gethash (car ent) hash) (cdr ent))
      (let* ((old (assoc (car ent) *prover-keywords*))
	     (formals (formals (cdr ent)))
	     (has-rest? (when (memq '&rest formals) t)))
	(if old
	    (setf (cdr old) (cons has-rest? (make-prover-keywords formals)))
	    (push (cons (car ent)
			(cons has-rest? (make-prover-keywords formals)))
		  *prover-keywords*))))))

(defun add-lower-case-prover-keywords ()
  (dolist (rule *prover-keywords*)
    (let ((id (car rule))
	  (entry (cdr rule)))
      (when (some #'upper-case-p (string id))
	(let ((lid (intern (string-downcase (string id)))))
	  (unless (assoc lid *prover-keywords*)
	    (push (cons lid entry) *prover-keywords*)))))))

(defun make-prf-pathname (fname &optional (dir *pvs-context-path*))
  (assert (or (stringp fname) (pathnamep fname)))
  (let* ((pname (pathname fname))
	 (name (pathname-name pname))
	 (fdir (pathname-directory pname)))
    (make-pathname :directory (or fdir (pathname-directory dir))
		   :name name
		   :type "prf")))

;(defun cleanup-current-context ()
  

;;; Handle-deleted-theories is called by parse-file to handle the situation
;;; where some theories have been deleted from a file that has just been
;;; parsed.

(defun handle-deleted-theories (file newtheories)
  (mapc #'(lambda (ot)
	    (unless (or (member ot newtheories :test #'same-id)
			(null (get-theory (id ot)))
			(not (equal file (filename (get-theory (id ot))))))
	      (delete-theory (id ot))))
	(let ((entry (get-context-file-entry file)))
	  (when entry
	    (ce-theories entry))))
  (unless t ;;(probe-file (make-prf-pathname file))
    (mapc #'(lambda (nt)
	      (let ((oproofs (read-orphaned-proofs nt)))
		(when oproofs
		  (reload-theory-proofs nt oproofs))))
	  newtheories)))

(defun reload-theory-proofs (theory proofs)
  (mapc #'(lambda (decl)
	    (when (typep decl 'formula-decl)
	      (let ((prf-entry (car (member (id decl) proofs
					    :test #'(lambda (id prf)
						      (eq id (caddr prf)))))))
		(when (and prf-entry
			   (null (justification decl)))
		  (setf (justification decl)
			(cdddr prf-entry))))))
	(append (assuming theory) (theory theory))))


(defun collect-theories ()
  (cons (shortname *pvs-context-path*)
	(sort (apply #'append
		     (mapcar #'collect-theories*
			     (pvs-context-entries)))
	      #'string-lessp :key #'car)))

(defun collect-theories* (fe)
  (let ((file (string (ce-file fe))))
    (when (file-exists-p (make-specpath file))
      (mapcar #'(lambda (te)
		  (list (string (te-id te))
			file))
	      (ce-theories fe)))))

(defun find-all-usedbys (theoryref)
  (let ((tid (ref-to-id theoryref))
	(usedbys nil))
    (mapc #'(lambda (fe)
	      (mapc #'(lambda (te)
			(let ((deps (cdr (assq nil (te-dependencies te)))))
			  (when (memq tid deps)
			    (pushnew (id te) usedbys))))
		    (ce-theories fe)))
	  (pvs-context-entries))
    usedbys))

(defun verify-usedbys ()
  (maphash #'(lambda (tid theory)
	       (let ((usedbys (find-all-usedbys tid)))
		 (maphash #'(lambda (id th)
			      (declare (ignore id))
			      (when (assq theory (all-usings th))
				(memq (id th) usedbys)))
			  *pvs-modules*)))
	   *pvs-modules*))

(defun install-pvs-proof-file (filename)
  (let ((prf-file (make-prf-pathname filename))
	(theories (get-theories filename)))
    (cond ((not (file-exists-p prf-file))
	   (pvs-message "~a.prf not found" filename))
	  ((null theories)
	   (if (file-exists-p (make-specpath filename))
	       (pvs-message
		   "Proof will be loaded when the file is next parsed.")
	       (pvs-message "~a.pvs not found." filename)))
	  (t (mapc #'(lambda (th)
		       (restore-proofs prf-file th)
		       (clear-proof-status th)
		       )
		   theories)))))

(defmethod clear-proof-status (theory)
  (let ((te (get-context-theory-entry theory)))
    (when te
      (dolist (fe (te-formula-info te))
	(when fe
	  (setf (fe-status fe) 'untried)
	  (setf (fe-proof-refers-to fe) nil))))))

(defvar *auto-save-proof-file* nil)

(defun auto-save-proof-setup (decl)
  (when *in-checker*
    (setq *auto-save-proof-file*
	  (merge-pathnames (format nil "#~a.prf#" (filename (module decl)))
			   *pvs-context-path*))))

(defun auto-save-proof ()
  (assert *in-checker*)
  (when (and *pvs-context-writable*
	     *auto-save-proof-file*)
    (let* ((decl (declaration *top-proofstate*))
	   (theory (module decl))
	   (curproof (extract-justification-sexp
		      (collect-justification *top-proofstate*)))
	   (thproof (cons (id theory) (cons (id decl) curproof))))
      (multiple-value-bind (value condition)
	  (ignore-file-errors
	   (with-open-file (out *auto-save-proof-file*
				:direction :output
				:if-exists :supersede)
	     (write thproof :length nil :level nil :escape t
		    :pretty nil :stream out)))
	(declare (ignore value))
	(when condition
	  (pvs-message "~a" condition))))))

(defun remove-auto-save-proof-file ()
  (when *auto-save-proof-file*
    (ignore-file-errors
     (delete-file *auto-save-proof-file*))
    (setq *auto-save-proof-file* nil)))

(defun copy-auto-saved-proofs-to-orphan-file ()
  (let ((auto-saved-files (directory (make-pathname
				      :defaults *pvs-context-path*
				      :name :wild
				      :type "prf#")))
	(proofs nil))
    (when auto-saved-files
      (ignore-file-errors
       (with-open-file (orph "orphaned-proofs.prf"
			     :direction :output
			     :if-exists :append
			     :if-does-not-exist :create)
	 (dolist (auto-saved-file auto-saved-files)
	   (with-open-file (asave auto-saved-file :direction :input)
	     (let ((proof (read asave)))
	       (write (cons (subseq (pathname-name auto-saved-file) 1) proof)
		      :length nil :level nil :escape t :pretty nil
		      :stream orph)
	       (push proof proofs)))
	   (delete-file auto-saved-file))))
      (pvs-buffer "PVS Info"
	(format nil
	    "The following proofs have been copied from auto-saved files to~%~
             the orphaned proof file: ~{~%  ~a~}~%~
             Use M-x show-orphaned-proofs to examine or install the proofs"
	  (mapcar #'(lambda (x)
		      (format nil "~a.~a" (car x) (cadr x)))
		  proofs))
	t t))))

(defun check-pvs-context ()
  (dolist (ce (pvs-context-entries))
    (check-pvs-context-entry ce))
  (maphash #'check-pvs-context-files *pvs-files*))

(defun check-pvs-context-entry (ce)
  (let ((file (make-specpath (ce-file ce))))
    (and (file-exists-p file)
	 (= (file-write-date file) (ce-write-date ce))
	 ;; proofs-date
	 ;; object-date
	 ;; dependencies
	 ;; theories
	 ;; extension
	 )))

(defun restore-proofs-from-split-file (file)
  (let ((prfpath (make-prf-pathname file)))
    (if (file-exists-p prfpath)
	(with-open-file (input prfpath :direction :input)
	  (restore-proofs-from-split-file* input prfpath))
	(format t "~%Proof file ~a does not exist" prfpath))))

(defun restore-proofs-from-split-file* (input prfpath)
  (let ((theory-proofs (read input nil nil)))
    (when theory-proofs
      (let* ((theoryid (car theory-proofs))
	     (proofs (cdr theory-proofs))
	     (theory (get-theory theoryid)))
	(unless (every #'consp proofs)
	  (error "Proofs file ~a is corrupted" prfpath))
	(cond (theory
	       (restore-theory-proofs theory proofs)
	       (format t "~%Theory ~a proofs restored" theoryid))
	      (t (format t "~%Theory ~a not found, ignoring" theoryid)))
	(restore-proofs-from-split-file* input prfpath)))))


;;; There are 3 forms of justification used in PVS.
;;;   justification - this is what is generated during proof
;;;   justification-sexp - what is saved to file:
;;     ("" (INDUCT "i")
;;      (("1" (INST + "1" "1") (("1" (ASSERT) nil nil)) nil)
;;       ("2" (SKOSIMP*)
;; 	  (("2" (CASE-REPLACE "n5!1=0")
;; 	    (("1" (INST 1 "n3!1-3" "2")
;; 	      (("1" (ASSERT) nil nil) ("2" (ASSERT) nil nil)) nil)
;; 	     ("2" (INST + "n3!1+2" "n5!1-1")
;; 	      (("1" (ASSERT) nil nil) ("2" (ASSERT) nil nil)) nil))
;; 	    nil))
;; 	  nil))
;;      nil)
;;;   justification-view - what is shown to users
;;     ("" (INDUCT "i")
;;      (("1" (INST + "1" "1") (ASSERT))
;;       ("2" (SKOSIMP*) (CASE-REPLACE "n5!1=0")
;;        (("1" (INST 1 "n3!1-3" "2") (("1" (ASSERT)) ("2" (ASSERT))))
;; 	   ("2" (INST + "n3!1+2" "n5!1-1") (("1" (ASSERT)) ("2" (ASSERT))))))))

;;(defun editable-justification-to-sexp (just)

(defun maximal-theory-hierarchy-elements ()
  (let ((max nil) (nonmax nil))
    (dolist (ce (pvs-context-entries))
      (dolist (te (ce-theories ce))
	(loop for (libref . theories) in (te-dependencies te)
	      do (when (null libref)
		   (dolist (th theories)
		     (unless (memq th nonmax)
		       (when (memq th max)
			 (setq max (remove th max)))
		       (push th nonmax)))))
	(unless (memq (te-id te) nonmax)
	  (push (te-id te) max))))
    (values max nonmax)))
