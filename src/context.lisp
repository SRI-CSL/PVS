;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; context.lisp -- Context structures and accessors
;; Author          : Sam Owre
;; Created On      : Fri Oct 29 19:09:32 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun May 28 19:14:47 1995
;; Update Count    : 69
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

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
;;; initially NIL, and is set to T when a change occurs that affects the
;;; context, and reset to NIL after the context is written again.  The changes
;;; that affect the context are:
;;;   - A new file is parsed
;;;   - A file has been modified
;;;   - A file/theory is deleted from the context
;;;   - The status of a proof has changed

(defvar *pvs-context-changed* nil
  "Set to T when the context has changed, NIL after it's been saved.")

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

(defun pvs-context-entries ()
  (cddr *pvs-context*))

(defstruct (context-entry (:conc-name ce-))
  file
  write-date
  proofs-date
  object-date
  dependencies
  theories
  extension) ;; Added in 2.0 Beta

(defstruct (theory-entry (:conc-name te-))
  id
  status
  dependencies
  formula-info)


;;; The formula-entry keeps track of the proof status and the references
;;; for the given formula id.  The status is one of the values untried,
;;; unfinished, unchecked, proved-incomplete, or proved-complete.

(defstruct (formula-entry (:conc-name fe-))
  id
  status
  proof-refers-to)

(defstruct (declaration-entry (:conc-name de-))
  id
  class
  type
  theory-id)

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

(defun change-context (directory)
  (let ((dir (get-valid-context-directory directory nil)))
    (setq *pvs-context-writable* (write-permission? dir))
    (when *pvs-initialized*
      (save-context))
    (clrhash *prelude-libraries*)
    (setq *prelude-libraries-uselist* nil)
    (clrhash *imported-libraries*)
    (setf (caddr *strat-file-dates*) 0)
    (set-working-directory dir)
    (setq *pvs-context-path* (working-directory))
    (clear-theories)
    (restore-context)
    (unless (probe-file (context-pathname dir))
      (write-context))
    (pvs-message "Context changed to ~a"
      (shortname (working-directory)))
    (when *pvs-context-writable*
      (copy-auto-saved-proofs-to-orphan-file))
    (shortname (working-directory))))

(defun reset-context ()
  (setq *last-proof* nil)
  (clrhash *pvs-files*)
  (clrhash *pvs-modules*)
  (setq *current-theory* nil)
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
	  ((and (write-permission? dir)
		(not (probe-file (context-pathname dir)))
		(not (pvs-y-or-n-p "Context not found - create new one? ")))
	   (get-valid-context-directory nil nil))
	  (t dir))))

(defun context-pathname (&optional (dir (working-directory)))
  (make-pathname :name *context-name* :defaults dir))

(defvar *dont-write-object-files* nil)

;;; Save Context - called from Emacs and parse-file (pvs.lisp), saves any
;;; changes since the last time the context was saved.

(defun save-context ()
  (cond ((not (probe-file (working-directory)))
	 (pvs-message "Directory ~a seems to have disappeared!"
	   (namestring (working-directory))))
	(t (unless (or *loading-prelude*
		       *loading-library*)
	     (unless (or *dont-write-object-files*
			 (not *pvs-context-writable*))
	       (write-object-files))
	     (write-context)))))

(defun sc ()
  (save-context))


(defun write-context ()
  (when (or *pvs-context-changed*
	    (not (probe-file (context-pathname))))
    (if *pvs-context-writable*
	(let ((context (make-pvs-context)))
	  (multiple-value-bind (value condition)
	      (ignore-file-errors
	       (store-object-to-file context *context-name*))
	    (declare (ignore value))
	    (cond (condition
		   (pvs-message "~a" condition))
		  (t (setq *pvs-context* context)
		     (setq *pvs-context-changed* nil)
		     (pvs-log "Context written")))))
	(pvs-message "Context not written, do not have write permission"))))

(defvar *testing-restore* nil)

(defun write-object-files (&optional force?)
  (update-stored-mod-depend)
  (if *testing-restore*
      (maphash #'(lambda (file theories)
		    (declare (ignore theories))
		    (write-object-file file force?))
		*pvs-files*)
      (multiple-value-bind (value condition)
	  (ignore-file-errors
	   (maphash #'(lambda (file theories)
			(declare (ignore theories))
			(write-object-file file force?))
		    *pvs-files*))
	(declare (ignore value))
	(when condition
	  (pvs-message "~a" condition)))))

(defun write-object-file (file &optional force?)
  (let* ((binpath (make-binpath file))
	 (bindate (file-write-date binpath))
	 (specpath (make-specpath file))
	 (specdate (file-write-date specpath))
	 (fe (get-context-file-entry file)))
    ;;(assert (and fe specdate) () "Error in writing binfile")
    (when (and fe specdate)
      (unless (or (not (typechecked-file? file))
		  (every #'generated-by (cdr (gethash file *pvs-files*)))
		  (and (not force?)
		       bindate
		       (equal bindate (ce-object-date fe))
		       (< specdate bindate)))
	(pvs-log "Saving theories for ~a" file)
	(save-theories file)
	(setf (ce-object-date fe) (file-write-date binpath))
	(setq *pvs-context-changed* t)))))
			  

(defun context-is-current ()
  (and (probe-file (context-pathname))
       (let ((current? t))
	 (maphash #'(lambda (fname info)
		      (declare (ignore info))
		      (let ((file (make-specpath fname))
			    (entry (get-context-file-entry fname)))
			(unless (and entry
				     (probe-file file)
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
	      (unless (or (not (probe-file (make-specpath (ce-file entry))))
			  (member (ce-file entry) context
				  :key #'ce-file
				  :test #'string=))
		(push entry context)))
	  (pvs-context-entries))
    (cons *pvs-version*
	  (cons (pvs-context-libraries)
		(sort-context context)))))

(defun prelude-libraries-pathnames ()
  (let ((paths nil))
    (maphash #'(lambda (p ht)
		 (declare (ignore ht))
		 (let ((str (cdr (assoc p *library-alist* :test #'equal))))
		   (push str paths)))
	     *prelude-libraries*)
    paths))

(defun visible-libraries-pathnames ()
  (let ((paths nil))
    (maphash #'(lambda (p ht)
		 (declare (ignore ht))
		 (push p paths))
	     *visible-libraries*)
    (mapcar #'namestring paths)))


;;; update-context is called by parse-file after parsing a file.  It compares
;;; the context-entry of the file with the write-date of the file.  If the
;;; context entry is missing, or the write-date is different, then the
;;; *pvs-context* is updated, and the *pvs-context-changed* variable is set.
;;; Note that this doesn't actually write out the .pvscontext file.

(defun update-context (filename)
  (let ((oce (get-context-file-entry filename))
	(nce (create-context-entry filename)))
    (unless (and (not *pvs-context-changed*)
		 oce
		 (equal (ce-write-date oce)
			(file-write-date (make-specpath filename)))
		 (ce-eq oce nce))
      (when oce
	(setf (ce-object-date nce) (ce-object-date oce)))
      (setq *pvs-context*
	    (cons *pvs-version*
		  (cons (pvs-context-libraries)
			(cons nce (remove oce (pvs-context-entries))))))
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
    (make-context-entry
     :file filename
     :write-date (car theories)
     :object-date (when file-entry (ce-object-date file-entry))
     :extension nil
     :proofs-date proofs-write-date
     :dependencies fdeps
     :theories (mapcar #'(lambda (th) (create-theory-entry th file-entry))
		       (cdr theories)))))

(defun sort-context (context &optional result)
  (if (null context)
      (nreverse result)
      (let ((entry (find-if
		    #'(lambda (e1)
			(notany #'(lambda (e2)
				    (member (ce-file e2)
					    (ce-dependencies e1)))
				context))
			    context)))
	(assert entry)
	(sort-context (remove entry context) (cons entry result)))))

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
     :dependencies (mapcar #'id (remove theory (dependencies theory)))
     :formula-info (append finfo
			   (unless (typechecked? theory)
			     (hidden-formula-entries tentry finfo valid?))))))

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
			  ((proved-complete proved-incomplete proved unchecked)
			   'unchecked)
			  (unfinished 'unfinished)
			  (t 'untried))))
		   (t 'untried))
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
  (let ((theories (cdr (gethash filename *pvs-files*))))
    (if theories
	(let ((depfiles nil))
	  (dolist (theory theories)
	    (when (memq 'typechecked (status theory))
	      (dolist (dth (dependencies theory))
		(when (filename dth)
		  (let ((depname (pathname-name (filename dth))))
		    (unless (or (from-prelude? dth)
				(equal filename depname)
				(some #'(lambda (th)
					  (and (datatype? th)
					       (eq (id th)
						   (generated-by dth))))
				      theories))
		      (when (and (typep dth '(or library-theory
						 library-datatype))
				 (not (equal (library dth)
					     (working-directory))))
			(setq depname
			      (namestring
			       (merge-pathnames
				(pvs-truename (library dth)) (filename dth)))))
		      (pushnew depname depfiles
			       :test #'(lambda (x y)
					 (or (equal x filename)
					     (equal x y))))))))))
	  (delete-if #'null (nreverse depfiles)))
	(let ((entry (get-context-file-entry filename)))
	  (when entry
	    (delete-if #'null (mapcar #'string (ce-dependencies entry))))))))

(defun dependencies (theory)
  (let ((*current-theory* theory)
	(*modules-visited* nil))
    (dependencies* theory)
    (remove theory *modules-visited*)))

(defun dependencies* (theory)
  (unless (or (null theory)
	      (from-prelude? theory)
	      (memq theory *modules-visited*))
    (push theory *modules-visited*)
    (dolist (th (cons (generated-by theory)
		      (remove-if #'(lambda (th)
				     (let ((ce (context-entry-of th)))
				       (and ce
					    (memq (id th)
						  (ce-dependencies ce)))))
			(get-immediate-usings theory))))
      (dependencies* (get-theory th)))))

(defun context-usingchain (theoryref)
  (let ((uchain nil))
    (labels ((usingchain (theoryids)
	       (when theoryids
		 (let ((tid (car theoryids)))
		   (unless (memq tid uchain)
		     (let ((te (get-context-theory-entry tid)))
		       (unless (or (null te)
				   (memq 'generated (te-status te)))
			 (push tid uchain)))
		     (usingchain (get-theory-dependencies tid)))
		   (usingchain (cdr theoryids))))))
      (usingchain (list (ref-to-id theoryref))))
    (mapcar #'string uchain)))

(defun get-pvs-file-dependencies (filename)
  (cons filename (file-dependencies filename)))

(defun get-theory-dependencies (theoryid)
  (let ((te (get-context-theory-entry theoryid)))
    (when te
      (te-dependencies te))))

;;; Restore context

(defun restore-context ()
  ;;(reset-context)
  ;;(clear-theories)
  (if (probe-file *context-name*)
      (multiple-value-bind (context error)
	  (ignore-errors (if (with-open-file (in *context-name*)
			       (and (char= (read-char in) #\()
				    (char= (read-char in) #\")))
			     (with-open-file (in *context-name*) (read in))
			     (fetch-object-from-file *context-name*)))
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
		      (load-prelude-libraries (cadr context)))
		     ((typep (cadr context) 'context-entry)
		      (setq *pvs-context*
			    (cons (car *pvs-context*)
				  (cons nil (cdr *pvs-context*)))))
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
      (setq *pvs-context* (list *pvs-version*)))
  nil)


(defun duplicate-theory-entries? ()
  (duplicates? (mapcar #'car (cdr (collect-theories))) :test #'string=))

(defvar *files-seen* nil)

(defun restore-theories (filename)
  (let ((*files-seen* nil))
    (restore-theories* filename)
    (get-theories (make-specpath filename))))

(defun restore-theories* (filename)
  (let ((deps (file-dependencies filename)))
    (dolist (dep deps)
      (if (find #\/ dep)
	  (let* ((pos (position #\/ dep :from-end t))
		 (lib (subseq dep 0 (1+ pos)))
		 (file (subseq dep (1+ pos))))
	    (multiple-value-bind (imports errcond)
		(with-no-type-errors 
		 (load-imported-library-file lib file))
	      (when errcond
		(restore-filename-error file errcond))))
	  (unless (member dep *files-seen* :test #'string=)
	    (unless (gethash dep *pvs-files*)
	      (restore-theories* dep))
	    (push dep *files-seen*))))
    (if (valid-binfile? filename)
	(restore-theories-from-file filename)
	(typecheck-file filename))))

(defun restore-theories-from-file (filename)
  (pvs-message "Restoring theories from ~a.bin" filename)
  (let ((start-time (get-internal-real-time))
	(*bin-theories-set* nil))
    (multiple-value-bind (theories condition)
	(ignore-errors (get-theories-from-file filename))
      (cond (condition
	     (restore-filename-error filename condition))
	    (t (setf (gethash filename *pvs-files*)
		     (cons (file-write-date (make-specpath filename))
			   theories))
;	       (mapc #'(lambda (th)
;			 (setf (gethash (id th) *pvs-modules*) th))
;		     theories)
	       (dolist (theory theories)
		 (update-restored-theories theory))
	       (pvs-message "Restored file ~a (~{~a~^, ~}) in ~,2f seconds"
			filename
			(mapcar #'id theories)
			(/ (- (get-internal-real-time) start-time)
			   internal-time-units-per-second 1.0)))))))

(defun restore-filename-error (filename condition)
  (remhash filename *pvs-files*)
  (let ((ce (get-context-file-entry filename)))
    (when ce
      (setf (ce-object-date ce) nil)))
  (dolist (thid *bin-theories-set*)
    (remhash thid *pvs-modules*))
  (pvs-message "Couldn't restore ~a: ~a"
    filename condition)
  (multiple-value-bind (val condition)
      (ignore-errors (delete-file (make-binpath filename)))
    (if condition
	(pvs-message "Couldn't delete ~a: ~a"
	  (make-binpath filename) condition)
	(pvs-message "Deleted file ~a" (make-binpath filename)))))

(defmethod update-restored-theories ((theory module))
  (setf (formals-sans-usings theory)
	(remove-if #'(lambda (d) (typep d 'importing))
	  (formals theory)))
  ;;(generate-xref theory)
  ;;(reset-restored-types theory)
  (restore-from-context (filename theory) theory)
  (setf (saved-context theory) (context theory))
  (assert (saved-context theory)))

(defmethod update-restored-theories ((adt datatype))
  (setf (formals-sans-usings adt)
	(remove-if #'(lambda (d) (typep d 'using))
	  (formals adt)))
  (mapc #'update-restored-theories (adt-generated-theories adt)))

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




;(defun restore-theories (entries &optional untypechecked)
;  (when entries
;    (restore-theories (cdr entries)
;		      (if (restore-theory (car entries) untypechecked)
;			  untypechecked
;			  (cons (ce-file (car entries))
;				untypechecked)))))
;
;(defun restore-theory (entry untypechecked)
;  (if (typep entry 'context-entry)
;      (let* ((file (ce-file entry))
;	     (sdate (ce-write-date entry))
;	     (deps (ce-dependencies entry))
;	     (theories (ce-theories entry))
;	     (src (probe-file (make-specpath file))))
;	(cond ((null src)
;	       (pvs-message "File for ~a is not in the current context" file)
;	       nil)
;	      ((not (= (file-write-date src) sdate))
;	       (pvs-message "~a has been modified since last context save" file)
;	       ;;(put-module file)
;	       nil)
;	      ((some #'(lambda (d) (member d untypechecked)) deps)
;	       (pvs-message "~a depends on ~a: needs to be retypechecked"
;			    file (find-if #'(lambda (d) (member d untypechecked))
;					  deps))
;	       ;;(put-module file)
;	       )
;	      (t (pvs-message "Restoring ~a" file)
;		 (typecheck-file file nil t)
;		 ;;(restore-proofs file entry)
;		 t)))
;      (pvs-message "Unrecognizable context entry")))

;(defun restore-proofs (id entry)
;  (let ((mod (get-module id)))
;    (format t "~%Need to restore proof information")))

;;; Show Context Path

(defun show-context-path ()
  (pvs-message (shortname (working-directory))))


;;; Update-from-context is called by typecheck (module)

;(defun update-from-context (mod)
;  (when (every #'(lambda (id)
;		   (let ((ent (find-if #'(lambda (e) (eq id (second e)))
;				       (pvs-context-entries))))
;		     (and ent
;			  (= (third ent) (file-write-date (make-specpath id))))))
;	       (cons (id mod) (dependencies mod)))
;    (let ((entry (find-if #'(lambda (e) (eq (id mod) (second e)))
;			  (pvs-context-entries))))
;      
;      (mapcar #'(lambda (pd pair)
;		  (setf (status pd) (car pair)))
;	      (remove-if-not #'(lambda (d) (typep d 'proof-decl))
;			     (append (assuming mod) (theory mod)))
;	      (sixth entry)))))

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
		  (let ((fentry (get-context-file-entry d)))
		    (if fentry
			(valid-context-entry* fentry)
			(let ((dir (pathname-directory d)))
			  (not (or (null dir)
				   (file-equal (make-pathname :directory dir)
					       (working-directory))))))))
	      (ce-dependencies entry))))

(defmethod valid-proofs-file (entry)
  (and (valid-context-entry entry)
       (let ((prf-file (make-prf-pathname (ce-file entry))))
	 (and (probe-file prf-file)
	      (eql (file-write-date prf-file)
		   (ce-proofs-date entry))))))

(defmethod get-context-file-entry ((filename string))
  (car (member filename (pvs-context-entries)
	       :test #'(lambda (x y)
			 (string= x (ce-file y))))))

(defmethod get-context-file-entry ((theory module))
  (get-context-file-entry (filename theory)))

(defmethod get-context-file-entry ((decl declaration))
  (get-context-file-entry (theory decl)))

(defmethod get-context-file-entry (obj)
  (error "Illegal call to get-context-file-entry"))

;(defun get-context-file-names (&optional context)
;  (when (or (null context)
;	    (probe-file context))
;    (if (or (null context)
;	    (equal (truename context) (truename (working-directory))))
;	(sort (mapcar #'(lambda (e) (format nil "~a.~a"
;				      (ce-file e) (ce-extension e)))
;		      (pvs-context-entries))
;	      #'string-lessp)
;	(let ((cfile (make-pathname :defaults context :name ".pvscontext")))
;	  (when (probe-file cfile)
;	    (let ((context (with-open-file (in cfile) (read in))))
;	      (sort (mapcar #'(lambda (e) (format nil "~a.~a"
;				      (ce-file e) (ce-extension e)))
;			    (cdr context))
;		    #'string-lessp)))))))

;(defun get-context-all-theory-names (&optional context)
;  (when (or (null context)
;	    (probe-file context))
;    (if (or (null context)
;	    (equal (truename context) (truename (working-directory))))
;	(sort (mapcan #'(lambda (e)
;			  (mapcar #'(lambda (te) (string (id te)))
;				  (ce-theories e)))
;		      (pvs-context-entries))
;	      #'string-lessp)
;	(let ((cfile (make-pathname :defaults context :name ".pvscontext")))
;	  (when (probe-file cfile)
;	    (let ((context (with-open-file (in cfile) (read in))))
;	      (sort (mapcan #'(lambda (e)
;				(mapcar #'(lambda (te) (string (id te)))
;					(ce-theories e)))
;			    (cdr context))
;		    #'string-lessp)))))))

(defun context-files-and-theories (&optional context)
  (when (or (null context)
	    (probe-file context))
    (if (or (null context)
	    (equal (truename context) (truename (working-directory))))
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
	  (when (probe-file cfile)
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
		   :test #'(lambda (id e)
			     (eq id (te-id e)))))
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
    (shortname (make-specpath file ext))))

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


;;; Called from typecheck-theories in pvs.lisp

(defun restore-from-context (filename theory)
  (when (probe-file (make-prf-pathname filename))
    (restore-proofs (make-prf-pathname filename) theory))
  (multiple-value-bind (valid? entry)
      (valid-context-entry filename)
    (when entry
      (let* ((thentry (get-context-theory-entry theory entry))
	     (theory (when thentry
		       (get-theory (te-id thentry)))))
	(when theory
	  (let* ((finfo (te-formula-info thentry))
		 (prf-file (make-prf-pathname filename))
		 (valid-prfs (and (probe-file prf-file)
				  (eql (file-write-date prf-file)
				       (ce-proofs-date entry)))))
	    (mapc #'(lambda (d)
		      (restore-formula-info d finfo
					    (and valid? valid-prfs)))
		  (remove-if-not #'formula-decl?
		    (append (assuming theory) (theory theory))))))))))


;;; For now, allow for formula entries to be lists - eventually we will
;;; remove the consp cases below.

(defun restore-formula-info (decl finfolist valid?)
  (let* ((finfo (find-if #'(lambda (fi)
			     (if (consp fi)
				 (eq (id decl) (car fi))
				 (same-id decl fi)))
			 finfolist))
	 (stat (when finfo
		 (if (consp finfo)
		     (cadr finfo)
		     (fe-status finfo)))))
    (setf (proof-status decl)
	  (if (and stat
		   (justification decl)
		   (not valid?))
	      'unchecked
	      (if (eq stat t) 'proved stat)))
    (when finfo
      (unless (consp finfo)
	(setf (proof-refers-to decl)
	      (mapcar #'get-declaration-entry-decl
		      (fe-proof-refers-to finfo)))))))

(defun get-declaration-entry-decl (de)
  (let ((theory (get-theory (de-theory-id de))))
    (when theory
      (let ((decls (remove-if-not
		       #'(lambda (d)
			   (and (typep d 'declaration)
				(eq (id d) (de-id de))
				(eq (type-of d) (de-class de))))
		       (all-decls theory))))
	(cond ((singleton? decls)
	       (car decls))
	      ((and (cdr decls)
		    (de-type de))
	       (let ((ndecls (remove-if-not
				 #'(lambda (d)
				     (string= (unparse (declared-type d)
						:string t)
					      (de-type de)))
			       decls)))
		 (when (singleton? ndecls)
		   (car ndecls)))))))))


;;; Proof handling functions - originally provided by Shankar.

(defun save-all-proofs (&optional theory)
  (unless *loading-prelude*
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
	  (when (and (probe-file filestring)
		     (> *number-of-proof-backups* 0))
	    (backup-proof-file filestring))
	  (multiple-value-bind (value condition)
	      (ignore-file-errors
	       (with-open-file (out filestring :direction :output
				    :if-exists :supersede)
		 (mapc #'(lambda (prf)
			   (write prf :length nil :level nil :escape t
				  :pretty *save-proofs-pretty*
				  :stream out))
		       curproofs)
		 (format out "~%")))
	    (declare (ignore value))
	    (cond ((or condition
		       (setq condition
			     (and *validate-saved-proofs*
				  (invalid-proof-file filestring curproofs))))
		   (if (pvs-yes-or-no-p
			"Error writing out proof file:~%  ~a~%Try again?"
			condition)
		       (save-proofs filestring theories)))
		  (t (pvs-log "Wrote proof file ~a"
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
      (ignore-errors (read input NIL NIL))
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
  (assert (or curproof
	      (not oldproof))))


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
	     (prf (when (and (formula-decl? decl)
			     (justification decl))
		    (cons (id decl)
			  (extract-justification-sexp
			   (justification decl))))))
	(collect-theory-proofs*
	 (cdr decls)
	 (if prf
	     (cons prf proofs)
	     proofs)))))

(defun merge-proofs (oldproofs proofs)
  (if (null oldproofs)
      (nreverse proofs)
      (merge-proofs (cdr oldproofs)
		    (if (assq (caar oldproofs) proofs)
			proofs
			(nconc proofs (list (car oldproofs)))))))

(defun restore-proofs (filestring theory)
  (unless *loading-prelude*
    (multiple-value-bind (ignore error)
	(ignore-errors
	  (with-open-file (input filestring :direction :input)
	    (restore-theory-proofs input filestring theory)))
      (declare (ignore ignore))
      (when error
	(pvs-message "Error reading proof file ~a"
	  (namestring filestring))
	(pvs-log (format nil "  ~a" error))))))

(defun restore-theory-proofs (input filestring theory)
  (let ((theory-proofs (read input NIL NIL)))
    (when theory-proofs
      (let* ((theoryid (car theory-proofs))
	     (proofs (cdr theory-proofs)))
	(unless (every #'consp proofs)
	  (pvs-message "Proofs file ~a is corrupted, will try to keep going."
	    filestring)
	  (setq proofs (remove-if-not #'consp proofs)))
	(if (eq theoryid (id theory))
	    (let ((restored (mapcar #'(lambda (decl)
					(restore-theory-proofs* decl proofs))
				    (append (assuming theory) (theory theory)))))
	      (copy-proofs-to-orphan-file
	       theoryid (set-difference proofs restored :test #'equal)))
	    (restore-theory-proofs input filestring theory))))))

(defun restore-theory-proofs* (decl proofs)
  (when (formula-decl? decl)
    (let ((prf-entry (assoc (id decl) proofs :test #'eql)))
      (when prf-entry
	(let ((script (if (integerp (cadr prf-entry))
			  (fifth (caddr prf-entry))
			  (cdr prf-entry))))
	  (if (justification decl)
	      (unless (equal (extract-justification-sexp
			      (justification decl))
			     script)
		(setf (justification2 decl) (justification decl))
		(setf (justification decl) script))
	      (setf (justification decl) script))))
      prf-entry)))


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
			      :length nil :level nil :escape t :stream orph))))
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
			      :length nil :level nil :escape t :stream orph))))
	       proofs)))
      (unless (zerop count)
	(pvs-message "Found ~d orphaned proof~:p from theory ~a"
	  count theoryid)))))

(defun read-theory-proofs (theoryref)
  (let* ((filename (context-file-of theoryref))
	 (tid (ref-to-id theoryref))
	 (prffile (when filename (make-prf-pathname filename))))
    (when (and prffile (probe-file prffile))
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

(defun read-pvs-file-proofs (filename &optional (dir (working-directory)))
  (let ((prf-file (make-prf-pathname filename dir)))
    (when (probe-file prf-file)
      (multiple-value-bind (proofs error)
	  (ignore-errors
	    (with-open-file (input prf-file :direction :input)
	      (labels ((reader (proofs)
			       (let ((nproof (read input nil 'eof)))
				 (if (eq nproof 'eof)
				     (nreverse proofs)
				     (reader (if (member nproof proofs
							 :test #'equal)
						 proofs
						 (cons nproof proofs)))))))
		(reader nil))))
	(cond (error
	       (pvs-message "Error reading proof file ~a"
		 (namestring prf-file))
	       (pvs-log (format nil "  ~a" error))
	       nil)
	      (t proofs))))))

(defun read-orphaned-proofs (&optional theoryref)
  (when (probe-file "orphaned-proofs.prf")
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
	    (write (editable-justification (cdddr proof))
		   :stream out :pretty t :escape t :level nil :length nil
		   :pprint-dispatch *proof-script-pprint-dispatch*))
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
		    (write prf :length nil :escape t :stream orph))))
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
	(ctx-strat-file (merge-pathnames (working-directory)
					 "pvs-strategies")))
    (load-strategies-file pvs-strat-file *strat-file-dates*)
    (load-strategies-file home-strat-file (cdr *strat-file-dates*))
    (load-strategies-file ctx-strat-file (cddr *strat-file-dates*))))

(defun load-strategies-file (file dates)
  (when (probe-file file)
    (let ((fwd (file-write-date file)))
      (unless (= fwd (car dates))
	(setf (car dates) fwd)
	(with-open-file (str file :direction :input)
	  (unless (ignore-errors (load str :verbose nil))
	    (pvs-message "Error in loading ~a" file)))))))

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
  (cons (shortname (working-directory))
	(sort (apply #'append
		     (mapcar #'collect-theories*
			     (pvs-context-entries)))
	      #'string-lessp :key #'car)))

(defun collect-theories* (fe)
  (let ((file (string (ce-file fe))))
    (when (probe-file (make-specpath file))
      (mapcar #'(lambda (te)
		  (list (string (te-id te))
			file))
	      (ce-theories fe)))))

(defun find-all-usedbys (tid)
  (let ((usedbys nil))
    (mapc #'(lambda (fe)
	      (mapc #'(lambda (te)
			(let ((deps (te-dependencies te)))
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
    (cond ((not (probe-file prf-file))
	   (pvs-message "~a.prf not found" filename))
	  ((null theories)
	   (if (probe-file (make-specpath filename))
	       (pvs-message
		   "Proof will be loaded when the file is next parsed.")
	       (pvs-message "~a.pvs not found." filename)))
	  (t (mapc #'(lambda (th)
		       (restore-proofs prf-file th)
		       (clear-proof-status th))
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
  (setq *auto-save-proof-file*
	(format nil "#~a.prf#" (filename (module decl)))))

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
				      :defaults (working-directory)
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
		      :length nil :level nil :escape t :stream orph)
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
    (and (probe-file file)
	 (= (file-write-date file) (ce-write-date ce))
	 ;; proofs-date
	 ;; object-date
	 ;; dependencies
	 ;; theories
	 ;; extension
	 )))
