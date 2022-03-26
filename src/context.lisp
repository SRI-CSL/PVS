;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; context.lisp -- Context structures and accessors
;; Author          : Sam Owre
;; Created On      : Fri Oct 29 19:09:32 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun May 28 19:14:47 1995
;; Update Count    : 69
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;;; Called from outside PVS
(export '(change-workspace context-files-and-theories context-usingchain
	  current-context-path get-pvs-file-dependencies pvs-delete-proof
	  pvs-rename-file pvs-select-proof pvs-view-proof save-context
	  show-context-path show-orphaned-proofs show-proof-file
	  show-proofs-pvs-file))

;;; Called from PVS only
(export '(handle-deleted-theories restore-from-context update-context
	  te-formula-info te-id fe-status fe-id get-theory-dependencies
	  restore-context valid-proofs-file get-context-theory-names
	  get-context-theory-entry context-file-of pvs-file-of
	  context-entry-of save-proofs save-all-proofs copy-theory-proofs-to-orphan-file
	  read-strategies-files))

(proclaim '(inline pvs-context-version pvs-context-libraries
	    pvs-context-entries))


;;; (current-pvs-context-changed) controls the saving of the PVS context.  It is
;;; initially nil, and is set to t when a change occurs that affects the
;;; context, and reset to nil after the context is written again.  The changes
;;; that affect the context are:
;;;   - A new file is parsed
;;;   - A file has been modified
;;;   - A file/theory is deleted from the context
;;;   - The status of a proof has changed
;;; The pvs-context is a list whose first element is the version, second
;;; element is a list of prelude libraries, and the rest are context entries.
;;; This is currently written directly to file, although it could be made
;;; faster by keeping a symbol table and writing arrays to file.  The version
;;; number would then provide enough information to translate to current
;;; contexts.

(defun pvs-context-version ()
  (car (current-pvs-context)))

(defun pvs-context-libraries ()
  (cadr (current-pvs-context)))

(defun pvs-context-default-decision-procedure ()
  (or (when (listp (caddr (current-pvs-context)))
	(getf (caddr (current-pvs-context)) :default-decision-procedure))
      'shostak))

(defun pvs-context-yices-executable ()
  (when (listp (caddr (current-pvs-context)))
    (getf (caddr (current-pvs-context)) :yices-executable)))

(defun pvs-context-yices2-executable ()
  (when (listp (caddr (current-pvs-context)))
    (getf (caddr (current-pvs-context)) :yices2-executable)))

(defun pvs-context-entries (&optional (ctx (current-pvs-context)))
  (if (listp (caddr ctx))
      (cdddr ctx)
      (cddr ctx)))

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
  extension
  (md5sum 0))

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

(defun context-entries-not-updated ()
  (mapcan #'(lambda (ce)
	      (let ((nce (create-context-entry (ce-file ce))))
		(unless (ce-eq ce nce)
		  (list ce))))
    (pvs-context-entries)))

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

(defvar *workspace-stack* nil)

(defun cw (directory)
  (change-workspace directory))


;;; Change-workspace checks that the specified directory exists, prompting
;;; for a different directory otherwise.  When a directory is given, the
;;; current context is saved (if writable), *last-proof* is cleared, the
;;; working-directory is set, and the context is restored.

(defun change-workspace (directory &optional init?)
  "PVS must always have a workspace, usually the initial one is the
directory it was started in.  The *current-workspace* has an associated
workspace-session, Which is where the parsed/typechecked theories are found.
Note that changing workspaces does not modify the current one; if you return
again during the same PVS session, it will be exactly as you left it."
  (let ((dir (get-library-path directory)))
    (unless dir
      (pvs-error "Change Workspace"
	(format nil "Workspace directory %s not found")))
    (if (and (not init?) (file-equal dir *default-pathname-defaults*))
	(pvs-message "Change Workspace: already in ~a" directory)
	(let* ((have-ws (get-workspace-session dir))
	       (next-ws (or have-ws
			    (let ((ws (make-instance 'workspace-session :path dir)))
			      (push ws *all-workspace-sessions*)
			      ws))))
	  (unless init? (save-context)) ;; Saves .pvscontext
	  (set-working-directory dir)
	  (setq *default-pathname-defaults* dir)
	  (push *workspace-session* *workspace-stack*)
	  (setq *workspace-session* next-ws)
	  (restore-context)          ;; load it
	  (when (write-permission?)
	    (copy-auto-saved-proofs-to-orphan-file))
	  (pvs-message "Context changed to ~a"
	    (current-context-path))))
    (assert (pvs-context *workspace-session*))
    (current-context-path)))

(defun change-context (directory &optional init?)
  "Old - deprecated"
  (change-workspace directory init?))

(defun reset-workspace ()
  "Called after loading prelude-libraries, to ensure everything is
retypechecked."
  ;; First reset local context
  ;;(setq *last-proof* nil)
  (clrhash (current-pvs-files))
  (clrhash (current-pvs-theories))
  (setq *current-context* nil)
  (reset-typecheck-caches))

(defun context-pathname (&optional (dir *default-pathname-defaults*))
  (make-pathname :name *context-name* :defaults dir))

(defvar *dont-write-object-files* nil)

;;; Save Context - called from Emacs and parse-file (pvs.lisp), saves any
;;; changes since the last time the context was saved.

(defun save-context ()
  #+pvsdebug
  (assert (file-equal *default-pathname-defaults* (working-directory))
	  () "Mismatch between *default-pathname-defaults* = ~a~%~
              and                      (working-directory) = ~a"
	  *default-pathname-defaults* (working-directory))
  #+pvsdebug
  (assert (file-equal *default-pathname-defaults* (current-context-path))
	  () "Mismatch between *default-pathname-defaults* = ~a~%~
              and                   (current-context-path) = ~a"
	  *default-pathname-defaults* (current-context-path))
  (if (not (file-exists-p (working-directory)))
      (pvs-message "Directory ~a seems to have disappeared!"
	(namestring *default-pathname-defaults*))
      (unless *loading-prelude*
	(unless (or *dont-write-object-files*
		    (not (write-permission?)))
	  (write-object-files))
	;; (maphash #'(lambda (file theories)
	;; 	     (assert (or (some #'(lambda (th)
	;; 				   (not (typechecked? th)))
	;; 			       (cdr theories))
	;; 			 (check-binfiles file))
	;; 		     () "check-binfiles failed after write-object-files"))
	;; 	 (current-pvs-files))
	(write-context)
	;; (maphash #'(lambda (file theories)
	;; 	     (assert (or (some #'(lambda (th)
	;; 				   (not (typechecked? th)))
	;; 			       (cdr theories))
	;; 			 (check-binfiles file))))
	;; 	 (current-pvs-files))
	t)))

(defun sc ()
  (save-context))


(defun write-context (&optional (ws *workspace-session*))
  (unless ws (setq ws (initialize-workspaces)))
  (assert (pvs-context ws))
  (with-workspace ws
    (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
    (when (or (pvs-context-changed ws)
	      (and (pvs-context-entries)
		   (not (file-exists-p (path ws)))))
      (if (write-permission? (path ws))
	  (let ((context (make-pvs-context)))
	    (assert (every #'(lambda (ce)
			       (file-exists-p (make-specpath (ce-file ce))))
			   (cdddr context)))
	    (multiple-value-bind (value condition)
		(progn			;ignore-file-errors
		  (store-object-to-file context (context-pathname)))
	      (declare (ignore value))
	      (cond (condition
		     (pvs-message "~a" condition))
		    (t (setf (pvs-context *workspace-session*) context)
		       (setf (pvs-context-changed *workspace-session*) nil)
		       (pvs-log "Context file ~a written~%"
				(namestring (context-pathname)))))))
	  (pvs-log "Context file ~a not written, do not have write permission"
		   (namestring (context-pathname)))))))

(defvar *testing-restore* nil)

(defun write-object-files (&optional force?)
  (when (and (> (hash-table-count (current-pvs-theories)) 0)
	     (ensure-bin-subdirectory))
    (if t ; *testing-restore*
	(maphash #'(lambda (file theories)
		     (declare (ignore file))
		     (dolist (theory (cdr theories))
		       (write-object-file theory force?)
		       (let ((bp (make-binpath theory))
			     (ce (get-context-file-entry theory)))
			 (assert (or (not (typechecked? theory))
				     (and ce (file-exists-p bp)
					  (ce-object-date ce)
					  (assq (id theory) (ce-object-date ce))
					  (= (file-write-time bp)
					     (cdr (assq (id theory) (ce-object-date ce))))))))))
		 (current-pvs-files))
	(multiple-value-bind (value condition)
	    (ignore-file-errors
	     (maphash #'(lambda (id theory)
			  (declare (ignore id))
			  (write-object-file theory force?))
		      (current-pvs-theories)))
	  (declare (ignore value))
	  (when condition
	    (pvs-message "~a" condition))))))

(defun write-object-file (theory &optional force?)
  (when (typechecked? theory)
    (let* ((gtheory (get-theory-with-filename theory))
	   (file (filename gtheory))
	   (binpath (make-binpath (binpath-id theory)))
	   (bindate (file-write-time binpath))
	   (specpath (make-specpath file))
	   (specdate (file-write-time specpath))
	   (ce (get-context-file-entry file)))
      (when (and ce (not (listp (ce-object-date ce))))
	(setf (ce-object-date ce) nil))
      (let* ((te (get-context-theory-entry gtheory ce))
	     (te-date (when ce (assq (id theory) (ce-object-date ce)))))
	(when (and te
		   specdate
		   (or force?
		       (null bindate)
		       (null (cdr te-date))
		       (>= specdate bindate)
		       (not (eql bindate (cdr te-date)))))
	  (pvs-log "Saving bin file for theory ~a" (binpath-id theory))
	  (save-theory theory)
	  (setf (ce-object-date ce)
		(acons (te-id te) (file-write-time binpath)
		       (delete te-date (ce-object-date ce))))
	  (setf (pvs-context-changed *workspace-session*) t))))))

(defmethod get-theory-with-filename ((m datatype-or-module))
  (assert (filename m))
  m)

(defmethod binpath-id (th)
  (id th))

(defmethod binpath-name ((inm modname) th)
  (declare (ignore th))
  inm)


(defun ensure-bin-subdirectory ()
  (let ((subdir (make-pathname
		 :defaults *default-pathname-defaults*
		 :name *pvsbin-string*)))
    (if (file-exists-p subdir)
	(if (directory-p subdir)
	    t
	    (pvs-message "~a is a regular file,~%  and can't be used as a~
                          subdirectory for .bin files unless it is moved."
	      subdir))
	(multiple-value-bind (result err)
	    (ignore-lisp-errors #+allegro (excl:make-directory subdir)
			   #+cmu (unix:unix-mkdir (namestring subdir) #o777)
			   #+sbcl
			   (sb-unix:unix-mkdir (namestring subdir) #o777))
	  (cond (result (pvs-message "Created directory ~a" subdir)
			t)
		(t (pvs-message "Error creating ~a: ~a" subdir err)
		   nil))))))
			  

(defun context-is-current ()
  (and (file-exists-p (context-pathname))
       (let ((current? t))
	 (maphash #'(lambda (fname info)
		      (declare (ignore info))
		      (let ((file (make-specpath fname))
			    (entry (get-context-file-entry fname)))
			(unless (and entry
				     (file-exists-p file)
				     (= (file-write-time file)
					(ce-write-date entry)))
			  (setq current? nil))))
		  (current-pvs-files))
	 current?)))
;  (labels ((current? (entries)
;	     (or (null entries)
;		 (let ((file (make-specpath (ce-file (car entries)))))
;		   
;		   (and (probe-file file)
;			(= (file-write-time file)
;			   (ce-write-date (car entries)))
;			(current? (cdr entries)))))))
;    (current? (pvs-context-entries)))

(defun update-pvs-context ()
  (setf (pvs-context *workspace-session*) (make-pvs-context))
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
  t)

(defun make-pvs-context (&optional (ws *workspace-session*))
  "Returns a list representing the .pvscontext file.
Has form (version (prelude-libnames) ce1 ce2 ...)
context-entry ce is a struct with slots:
  file: filename without extension,
  write-date, proofs-date, object-date,
  dependencies: list of pvs filenames,
  theories: list of theory-entry structs,
  extension: e.g., \"pvs\"
theory-entry is a struct, with slots:
  id, status,
  dependencies: list of binfile-dependencies,
  formula-info: list of formula-entry structs
formula-entry is a struct with slots:
  id, status, decision-procedure-used, proof-time,
  proof-refers-to: list of declaration-entry structs
declaration-entry has slots
  id, class, type, theory-id"
  (with-workspace ws
    (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
    (let ((*valid-entries* (make-hash-table :test #'eq))
	  (context nil))
      ;; Collect from (current-pvs-files
      (maphash #'(lambda (name info)
		   (declare (ignore info))
		   (let ((ce (create-context-entry name)))
		     (when ce
		       (push ce context))))
	       (pvs-files ws))
      (assert (not (duplicates? context :key #'ce-file)) () "after pvs-files")
      ;; Collect from current pvs-context remaining ce's that still have an
      ;; associated existing file.
      (mapc #'(lambda (entry)
		(when (and (not (member (ce-file entry) context
					:key #'ce-file
					:test #'string=))
			   (file-exists-p (make-specpath (ce-file entry))))
		  (push entry context)))
	    (pvs-context-entries))
      (assert (not (duplicates? context :key #'ce-file)))
      (cons *pvs-version*
	    (cons (pvs-context-libraries)
		  (cons (context-parameters) context))))))

(defun context-parameters ()
  (nconc (when *default-decision-procedure*
	   (list :default-decision-procedure *default-decision-procedure*))
	 (when (and *yices-executable*
		    (not (member *yices-executable* '("yices" "yices1") :test #'string=)))
	   (list :yices-executable *yices-executable*))
	 (when (and *yices2-executable*
		    (not (member *yices2-executable* '("yices" "yices2") :test #'string=)))
	   (list :yices2-executable *yices2-executable*))))


;;; update-context writes the .pvscontext file.  It is called by parse-file
;;; after parsing or typechecking a file.  It compares the context-entry of
;;; the file with the write-date of the file.  If the context entry is
;;; missing, or the write-date is different, then the (current-pvs-context) is
;;; updated, and the (current-pvs-context-changed) variable is set.  Note that this
;;; doesn't actually write out the .pvscontext file.

(defun update-context (filename)
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
  (let ((oce (get-context-file-entry filename))
	(nce (create-context-entry filename)))
    (unless (and (not (current-pvs-context-changed))
		 oce
		 (equal (ce-write-date oce)
			(file-write-time (make-specpath filename)))
		 (ce-eq oce nce))
      (when (and oce (ce-object-date oce))
	;; (format t "~%update-context: copy ~a to nce ~a"
	;;   (ce-object-date oce) nce)
	(setf (ce-object-date nce) (ce-object-date oce)))
      (let ((nctx-entries (cons nce (remove oce (pvs-context-entries)))))
	(assert (not (duplicates? nctx-entries :key #'ce-file)))
	(setf (pvs-context *workspace-session*)
	      (cons *pvs-version*
		    (cons (pvs-context-libraries)
			  (cons (list :default-decision-procedure
				      *default-decision-procedure*)
				nctx-entries))))
	(assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
	(setf (pvs-context-changed *workspace-session*) t)))))

(defun delete-file-from-workspace (filename)
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
  (let ((ce (get-context-file-entry filename)))
    (when ce
      (remove-context-entry-deps ce)
      (setf (pvs-context-changed *workspace-session*) t))))

(defun remove-context-entry-deps (filename)
  "removes filename from (pvs-context-entries) and recursively does the
same for any element of (pvs-context-entries) for which ce is in
its dependencies."
  (let ((ce (typecase filename
	      (context-entry filename)
	      (string (get-context-file-entry filename))
	      (t (error "bad filename ~a: ~a" filename (type-of filename))))))
    (unless (context-entry-p ce)
      (break "Check this"))
    (setf (cdddr (current-pvs-context)) (remove ce (pvs-context-entries)))
    (dolist (ce2 (pvs-context-entries))
      (when (some #'(lambda (dep) (string= dep (ce-file ce))) (ce-dependencies ce2))
	(remove-context-entry-deps ce2)))))

(defun update-context-proof-status (fdecl)
  (unless (or (from-prelude? fdecl)
	      (from-library? fdecl))
    (let ((fe (get-context-formula-entry fdecl)))
      (if fe
	  (let ((status (proof-status-symbol fdecl)))
	    (unless (eq (fe-status fe) status)
	      (setf (fe-status fe) status)
	      (setf (pvs-context-changed *workspace-session*) t))
	    (assert (memq (fe-status fe)
			  '(proved-complete proved-incomplete unchecked
					    unfinished untried nil))))
	  (setf (pvs-context-changed *workspace-session*) t)))))

(defun create-context-entry (pathname)
  (let ((specpath (make-specpath pathname)))
    (when (file-exists-p specpath)
      (let* ((filename (pathname-name specpath))
	     (file-entry (get-context-file-entry filename))
	     (theories (or (gethash filename (current-pvs-files))
			   (and file-entry (ce-theories file-entry))))
	     (prf-file (make-prf-pathname filename))
	     (proofs-write-date (file-write-time prf-file))
	     (fdeps (file-dependencies filename))
	     (objdate (when file-entry (ce-object-date file-entry)))
	     (md5sum (md5-file (make-specpath filename))))
	(assert (cdr theories))
	(assert (plusp md5sum))
	;;(format t "~%create-context-entry: ~a objdate = ~a" filename objdate)
	(make-context-entry
	 :file filename
	 :write-date (car theories)
	 :object-date objdate
	 :extension nil
	 :proofs-date proofs-write-date
	 :dependencies fdeps
	 :theories (let ((theories (gethash filename (current-pvs-files))))
		     (if theories
			 (mapcar #'(lambda (th)
				     (create-theory-entry th file-entry))
			   (cdr theories))
			 (ce-theories file-entry)))
	 :md5sum md5sum)))))

#+allegro
(defun md5-file (file)
  (excl:md5-file file))

#+(or cmu sbcl)
(defun md5-file (file)
  (let ((digest (#+cmu md5:md5sum-file #+sbcl sb-md5:md5sum-file file))
	(sum 0))
    (loop for x across digest
	  do (setq sum (+ (* sum 256) x)))
    sum))
  

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
      (let ((*current-context* (context theory)))
	(multiple-value-bind (i-theories i-names)
	    (all-importings theory)
	  (multiple-value-bind (imp-theories imp-names)
	      (add-generated-adt-theories i-theories i-names)
	    (when (recursive-type? theory)
	      (let ((adt-ths (adt-generated-theories theory)))
		(setf imp-theories
		      (append imp-theories adt-ths))
		(setf imp-names
		      (append imp-names
			      (mapcar #'(lambda (ath) (mk-modname (id ath)))
				adt-ths)))))
	    (let ((inames
		   (loop for ith in imp-theories
			 as inm in imp-names
			 unless (from-prelude? ith)
			 collect
			 (cons (binpath-name inm ith)
			       (when (and (lib-datatype-or-theory? ith)
					  (not (from-prelude? ith))
					  (not (eq (gethash (id ith)
							    (current-pvs-theories))
						   ith)))
				 (context-path ith)))))
		  (libalist nil)
		  (thlist nil)
		  (*current-context* (saved-context theory)))
	      ;; (assert (or (not (memq 'typechecked (status theory)))
	      ;;             *current-context*))
	      (loop for (inm . lib-path) in inames
		    do (if lib-path
			   (let ((libentry (assoc lib-path libalist :test #'equalp))
				 (nm (makesym "~a" (lcopy inm 'library nil))))
			     (if libentry
				 (nconc libentry (list nm))
				 (setq libalist
				       (nconc libalist
					      (list (list lib-path nm))))))
			   (push (makesym "~a" (lcopy inm 'library nil))
				 thlist)))
	      (nconc libalist (list (cons nil (nreverse thlist))))))))
      (let ((te (get-context-theory-entry (id theory))))
	(when te
	  (te-dependencies te)))))


(defmethod lib-datatype-or-theory? ((mod datatype-or-module))
  (assert (context-path mod))
  (and (not (equal (context-path mod) *default-pathname-defaults*))
       (not (from-prelude? mod))))

(defmethod lib-datatype-or-theory? ((mod inline-recursive-type))
  (assert (adt-theory mod))
  (lib-datatype-or-theory? (adt-theory mod)))

(defmethod lib-datatype-or-theory? (obj)
  (declare (ignore obj))
  nil)


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
  (let ((theories (cdr (gethash filename (current-pvs-files)))))
    (if (and theories
	     (every #'(lambda (th) (memq 'typechecked (status th))) theories))
	(let ((depfiles nil))
	  (dolist (dth (all-importings theories))
	    (unless (memq dth theories)
	      (when (filename dth)
		(let ((depname (filename dth)))
		  (unless (or (from-prelude? dth)
			      (and (equal filename depname)
				   (not (lib-datatype-or-theory? dth)))
			      (some #'(lambda (th)
					(and (datatype? th)
					     (eq (id th)
						 (generated-by dth))))
				    theories))
		    (when (lib-datatype-or-theory? dth)
		      (let* ((*current-context* nil) ; Don't want library-decls
			     (lib-path (context-path dth))
			     (lib-id (get-library-id lib-path)))
			(assert (file-exists-p lib-path))
			(setq depname (format nil "~a/~a"
					(or lib-id lib-path) (filename dth)))))
		    (pushnew depname depfiles
			     :test #'(lambda (x y)
				       (or (equal x filename)
					   (equal x y)))))))))
	  (delete-if #'null (nreverse depfiles)))
	(let ((entry (get-context-file-entry filename)))
	  (when entry
	    (mapcar #'string (delete-if #'null (ce-dependencies entry))))))))

(defun circular-file-dependencies (filename)
  (let ((deps (assoc filename *circular-file-dependencies* :test #'equal)))
    (if deps
	(cdr deps)
	(let ((cdeps (circular-file-dependencies*
		      (cdr (gethash filename (current-pvs-files))))))
	  (push (cons filename (car cdeps)) *circular-file-dependencies*)
	  (car cdeps)))))

(defun circular-file-dependencies* (theories &optional deps circs)
  (if (null theories)
      circs
      (let* ((theory (car theories))
	     (fname (dep-filename theory)))
	(unless (assoc fname *circular-file-dependencies* :test #'equal)
	  (if (and (cdr deps)
		   (equal fname (dep-filename (car (last deps))))
		   (some #'(lambda (dep)
			     (not (equal fname (dep-filename dep))))
			 deps))
	      ;; Found a circularity
	      (circular-file-dependencies* (cdr theories) deps
					   (cons (reverse (cons theory deps))
						 circs))
	      (append (circular-file-dependencies*
		       (delete-if #'(lambda (ith)
				      (or (null ith) (from-prelude? ith)))
			 (let ((*current-context* (saved-context theory)))
			   (if (generated-by theory)
			       (list (get-theory (generated-by theory)))
			       (delete-if #'lib-datatype-or-theory?
				 (mapcar #'(lambda (th) (get-theory th))
				   (get-immediate-usings theory))))))
		       (cons theory deps))
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

(defvar *file-dependencies*)

(defun get-pvs-file-dependencies (fileref)
  (with-pvs-file (filename) fileref
    (if (gethash filename (current-pvs-files))
	;; Things have been parsed, we can use that information
	(let ((*file-dependencies* nil))
	  (get-pvs-file-dependencies* filename)
	  *file-dependencies*)
	;; Not even parsed - must go by the .pvscontext information
	(cons filename (file-dependencies filename)))))

(defun get-pvs-file-dependencies* (filename)
  (unless (member filename *file-dependencies* :test #'string=)
    (push filename *file-dependencies*)
    (let ((theories (cdr (gethash filename (current-pvs-files)))))
      (dolist (theory theories)
	(if (rectype-theory? theory)
	    (get-pvs-file-dependencies*
	     (filename (get-theory (generated-by theory))))
	    (dolist (importing (get-immediate-usings theory))
	      (let ((itheory (unless (library importing)
			       (gethash (id importing) (current-pvs-theories)))))
		(if itheory
		    (if (rectype-theory? itheory)
			(get-pvs-file-dependencies*
			 (filename (get-theory (generated-by itheory))))
			(get-pvs-file-dependencies* (filename itheory)))
		    (unless (or (library importing)
				(gethash (id importing) *prelude*))
		      (pvs-message "~a not available" importing))))))))))

(defun get-theory-dependencies (theoryid)
  (let ((te (get-context-theory-entry theoryid)))
    (when te
      (te-dependencies te))))

(defun consistent-workspace-paths ()
  "Checks whether *default-pathname-defaults*, (working-directory), and (current-context-path)
are all the same."
  (and (file-equal *default-pathname-defaults* (working-directory))
       (file-equal *default-pathname-defaults* (current-context-path))))

;;; Restore context
;;; Reads in the .pvscontext file to the (current-pvs-context) variable.  Most of
;;; the rest of this function provides for reading previous versions of the
;;; .pvscontext

(defun restore-context (&optional (ws *workspace-session*))
  #+pvsdebug (consistent-workspace-paths)
  (assert ws)
  (let ((ctx-file (merge-pathnames *context-name*)))
    (if (file-exists-p ctx-file)
	(handler-case
	    (unless (pvs-context ws)
	      (let ((context (read-context-file ctx-file)))
		(setf (pvs-context ws) context)))
	  (file-error (err)
	    (pvs-message "PVS context problem - resetting")
	    (pvs-log "  ~a" err)
	    (setf (pvs-context ws) (list *pvs-version*))
	    (write-context)))
	(setf (pvs-context ws) (list *pvs-version*))))
  nil)

(defun read-context-file (ctx-file)
  (let ((*default-pathname-defaults* ;; for calls to make-specpath
	 (asdf/pathname:pathname-directory-pathname ctx-file))
	(context (if (with-open-file (in ctx-file)
		       (and (char= (read-char in) #\()
			    (char= (read-char in) #\")))
		     (with-open-file (in ctx-file) (read in))
		     (fetch-object-from-file ctx-file))))
    (assert (uiop:directory-exists-p *default-pathname-defaults*))
    (setf (cdddr context)
	  (remove-duplicates (cdddr context) :key #'ce-file :test #'equal :from-end t))
    (setf (cdddr context)
	  (delete-if-not #'(lambda (ce)
			     (file-exists-p (make-specpath (ce-file ce))))
	    (cdddr context)))
    (dolist (ce (cdddr context))
      (let ((ndeps (remove-if-not #'(lambda (dep)
				      (file-exists-p (make-specpath dep)))
		     (ce-dependencies ce))))
	(unless (equal ndeps (ce-dependencies ce))
	  (pvs-message "PVS context has bad deps: ~a"
	    (remove-if #'(lambda (dep) (file-exists-p (make-specpath dep)))
	      (ce-dependencies ce)))
	  (setf (ce-dependencies ce) nil)
	  (setf (ce-object-date ce) nil)
	  (setf (ce-theories ce) nil))))
    (cond ((duplicate-theory-entries?)
	   (pvs-message "PVS context has duplicate entries - resetting")
	   (list *pvs-version*))
	  (t ;;(same-major-version-number (car context) *pvs-version*)
	   ;; Hopefully we are backward compatible between versions
	   ;; 3 and 4.
	   (assert (every #'(lambda (ce)
			      (file-exists-p (make-specpath (ce-file ce))))
			  (pvs-context-entries context)))
	   (assert (not (duplicates? (pvs-context-entries context) :key #'ce-file)))
	   (setf (cadr context)
		 (delete "PVSio/"
			 (delete "Manip/"
				 (delete "Field/" (cadr context) :test #'string=)
				 :test #'string=)
			 :test #'string=))
	   (cond ((and (listp (cadr context))
		       (listp (caddr context))
		       (every #'context-entry-p (cdddr context)))
		  (load-prelude-libraries (cadr context))
		  (setq *default-decision-procedure*
			(or (when (listp (caddr context))
			      (getf (caddr context) :default-decision-procedure))
			    'shostak))
		  (dolist (ce (cdddr context))
		    (unless (listp (ce-object-date ce))
		      (setf (ce-object-date ce) nil)))
		  (assert (not (duplicates? (pvs-context-entries context) :key #'ce-file)))
		  context)
		 ((every #'context-entry-p (cdr context))
		  (cons (car context)
			(cons nil (cons nil (cdr context))))
		  (assert (not (duplicates? (pvs-context-entries context) :key #'ce-file)))
		  context)
		 (t (pvs-message "PVS context is not quite right ~
                                      - resetting")
		    (list *pvs-version*)))))))

(defun duplicate-theory-entries? ()
  (and *workspace-session*
       (duplicates? (mapcar #'car (cdr (collect-theories))) :test #'string=)))

(defvar *theories-restored* nil)
(defvar *files-seen* nil)

;;; Called from parse-file
;;; parse-file ->
;;;   restore-theories ->
;;;     restore-theories* ->
;;;       restore-theory
;;;     restore-theory ->
;;;       get-theory-from-binfile
;;;       update-restored-theories ->
;;;         restore-from-context ->
;;;           restore-proofs ->
;;;             restore-theory-proofs
;;;             restore-theory-proofs-from-file ->
;;;               restore-theory-proofs ->
;;;                 restore-theory-proofs*
(defun restore-theories (filename)
  (let* ((*theories-restored* nil)
	 (*adt-type-name-pending* nil))
    (dolist (te (ce-theories (get-context-file-entry filename)))
      (loop for (lib-ref . theories) in (te-dependencies te)
	    do (restore-theories* lib-ref theories))
      (restore-theory (id te)))
    #+pvsdebug (assert (null *adt-type-name-pending*))
    (let ((files nil))
      (dolist (th *theories-restored*)
	(let ((file (filename th)))
	  (unless (or (member file files :test #'string=)
		      (not (some #'null (gethash file (current-pvs-files)))))
	    (push file files))))
      (dolist (file files)
	(restore-theories file)))
    (when (some #'null (gethash filename (current-pvs-files)))
      (remhash filename (current-pvs-files)))
    (get-theories (make-specpath filename))))

(defun restore-theories* (libref theories)
  (if libref
      (restore-imported-library-files libref theories)
      (dolist (th theories)
	(restore-theory th))))

(defun restore-theory (thname)
  (unless (gethash thname (current-pvs-theories))
    (let ((theory (get-theory-from-binfile thname)))
      (when theory
	(let* ((tes (when (filename theory)
		      (ce-theories (get-context-file-entry
				    (filename theory)))))
	       (theories (mapcar #'(lambda (te)
				     (gethash (id te) (current-pvs-theories)))
			   tes)))
	  (assert (filename theory))
	  (push theory *theories-restored*)
	  (when (filename theory)
	    (setf (gethash (filename theory) (current-pvs-files))
		  (cons (file-write-time (make-specpath (filename theory)))
			theories)))
	  (dolist (th theories)
	    (when (memq th *theories-restored*)
	      (update-restored-theories th)
	      ;;(assert (or (generated-by th) (typechecked? th)))
	      )))))))

;;; Invoked after a bin file has been restored.
(defmethod update-restored-theories ((theory module))
  (setf (formals-sans-usings theory)
	(remove-if #'importing-param? (formals theory)))
  ;;(generate-xref theory)
  ;;(reset-restored-types theory)
  (unless (valid-proofs-file (filename theory))
    (let ((*current-context* (saved-context theory)))
      (assert *current-context*)
      (restore-from-context (filename theory) theory)))
  (assert (saved-context theory)))

(defmethod update-restored-theories ((adt recursive-type))
  (setf (formals-sans-usings adt)
	(remove-if #'importing-param? (formals adt)))
  (let* ((adt-theories (adt-generated-theories adt))
	 (adt-file (filename (car adt-theories))))
    (assert adt-file)
    (setf (gethash adt-file (current-pvs-files))
	  (cons (file-write-time (make-specpath adt-file)) adt-theories))
    (mapc #'update-restored-theories adt-theories)))

(defun make-new-context-from-old (context)
  ;; First copy the .pvscontext file
  (copy-file *context-name* ".pvscontext-old")
  ;; Now filter the context through the context upgrades.
  (let ((nctx (funcall (pvs-context-upgrade-function (car context))
		       (cdr context))))
    (when nctx
      (setf (pvs-context *workspace-session*) nctx)
      (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
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
  (pvs-message (current-context-path)))

;;; workspace access functions

(defun current-workspace ()
  *workspace-session*)

(defun current-context-path ()
  (if *workspace-session*
      (shortname (path *workspace-session*))
      (shortname *default-pathname-defaults*)))


;;; For a given filename, valid-context-entry returns two values: the
;;; first is a boolean indicating whether the entry is valid, and the
;;; second is the entry itself.

;;; The entry is valid if:

;;;  1. The ce-write-date of the entry matches the file-write-time of
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
       (equal (file-write-time file)
	      (ce-write-date entry))
       (every #'(lambda (d)
		  (let* ((fentry (get-context-file-entry d))
			 (dfile (when fentry
				  (make-specpath (ce-file fentry)))))
		    (if fentry
			(and (ce-write-date fentry)
			     (equal (file-write-time dfile)
				    (ce-write-date fentry)))
			(let ((dir (pathname-directory d)))
			  (not (or (null dir)
				   (file-equal (make-pathname :directory dir)
					       *default-pathname-defaults*)))))))
	      (ce-dependencies entry))))

(defmethod get-context-file-entry ((th datatype-or-module))
  (let ((ws (get-workspace-session (context-path th))))
    (car (member (filename th) (pvs-context-entries (pvs-context ws))
		 :test #'(lambda (x y)
			   (string= x (ce-file y)))))))
  
(defmethod get-context-file-entry ((filename pathname))
  (get-context-file-entry (pathname-name filename)))

(defmethod get-context-file-entry ((filename string))
  (car (member filename (pvs-context-entries)
	       :test #'(lambda (x y)
			 (string= x (ce-file y))))))

(defmethod get-context-file-entry ((decl declaration))
  (get-context-file-entry (theory decl)))

(defun context-files-and-theories (&optional context)
  (when (or (null context)
	    (file-exists-p context))
    (if (or (null context)
	    (file-equal context *default-pathname-defaults*))
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
  (let ((thid (ref-to-id theoryref)))
    (if (member thid *prelude-theories* :key #'id)
	(if (member thid (core-prelude-theories) :key #'id)
	    "prelude" "pvsio_prelude")
	(context-file-of*  thid (pvs-context-entries)))))

(defun pvs-file-of (theoryref)
  (multiple-value-bind (file ext)
      (context-file-of theoryref)
    (if (or (equal file "prelude")
	    (equal file "pvsio_prelude"))
	(format t "~a/~a.pvs" *pvs-path* file)
	(truename (make-specpath file (or ext "pvs"))))))

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


;;; Called from typecheck-theories, typecheck-top-level-adt, and
;;; update-restored-theories (module)

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

(defun get-referenced-declaration* (id class &optional type theory-id lib-ref)
  (if (eq class 'module)
      (let ((th (get-theory id)))
	;;(assert th)
	th)
      (if (and (memq id '(+ - * /))
	       (eq theory-id 'reals))
	  ;; Needed for backward compatibility after introduction of
	  ;; number_field
	  (let* ((theory (get-theory (mk-modname '|number_fields|)))
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
	  (multiple-value-bind (lib-path lib-id)
	      (if (consp lib-ref)
		  (values (cdr lib-ref) (car lib-ref))
		  (get-library-reference lib-ref))
	    (let ((theory
		   (or (and lib-path
			    (member lib-path (current-prelude-libraries)
				    :test #'file-equal)
			    (let ((ws (get-workspace-session lib-path)))
			      (gethash theory-id (pvs-theories ws))))
		       (get-theory* theory-id lib-id)
		       (and (null lib-id)
			    ;; Old proofs may not have library
			    ;; set, so we look for a unique one
			    (let ((theories (get-imported-theories theory-id)))
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
			     (car ndecls))))))))))))

(defun invalidate-context-formula-proof-info (filename file nth)
  (unless (or (null (get-context-file-entry filename))
	      (and (ce-write-date (get-context-file-entry filename))
		   (= (file-write-time file)
		      (ce-write-date (get-context-file-entry filename)))))
    (dolist (ce (pvs-context-entries))
      (when (and (member filename (ce-dependencies ce) :test #'string=)
		 (not (gethash (ce-file ce) (current-pvs-files))))
	(setf (ce-write-date ce) 0)
	(setf (ce-object-date ce) 0)
	(setf (pvs-context-changed *workspace-session*) t))
      (dolist (te (ce-theories ce))
	(when (and (member (string (id nth)) (te-dependencies te))
		   (not (get-theory (te-id te))))
	  (invalidate-theory-proofs te))))))

(defun invalidate-theory-proofs (te)
  (dolist (fe (te-formula-info te))
    (when (memq (fe-status fe)
		'(proved-complete proved-incomplete))
      (setf (fe-status fe) 'unchecked)
      (setf (pvs-context-changed *workspace-session*) t))))
  

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
	       (eql (file-write-time prf-file)
		    (ce-proofs-date entry))
	       (null (ce-proofs-date entry)))))))

;;; Proof handling functions - originally provided by Shankar.

;;; Should check that
;;; (save-all-proofs theory)
;;; (restore-proofs pvsfile theory)
;;; Gives the same proofs

(defun save-all-proofs (&optional theory)
  (unless (or *loading-prelude*
	      (and theory (from-prelude? theory)))
    (if theory
	(save-proofs (make-prf-pathname (filename theory))
		     (cdr (gethash (filename theory) (current-pvs-files))))
	(maphash #'(lambda (file mods)
		     (when (some #'has-proof? (cdr mods))
		       (save-proofs (make-prf-pathname file) (cdr mods))))
		 (current-pvs-files)))))

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
  (if (write-permission?)
      (let* ((oldproofs (read-pvs-file-proofs filestring))
	     (curproofs (collect-theories-proofs theories)))
	#+pvsdebug
	(current-proofs-contain-old-proofs curproofs oldproofs theories)
	(unless (equal oldproofs curproofs)
	  (when (and (file-exists-p filestring)
		     (> *number-of-proof-backups* 0))
	    (backup-proof-file filestring))
	  (when (save-proofs-to-file filestring curproofs)
	    (dolist (th theories)
	      (let ((te (get-context-theory-entry (id th))))
		(when (and te (memq 'invalid-proofs (te-status te)))
		  (setf (te-status te)
			(delete 'invalid-proofs (te-status te))))))
	    (pvs-log "Wrote proof file ~a"
		     (file-namestring filestring)))))
      (pvs-message
	  "Do not have write permission for saving proof files")))

(defun save-proofs-to-file (filestring theory-proofs)
  (let ((prf-file (make-prf-pathname filestring)))
    (multiple-value-bind (value condition)
	(ignore-file-errors
	 (with-open-file (out prf-file :direction :output
			      :if-exists :supersede)
	   (mapc #'(lambda (prf)
		     (write prf :length nil :level nil :escape t
			    :pretty *save-proofs-pretty*
			    :stream out)
		     (when *save-proofs-pretty* (terpri out)))
		 theory-proofs)
	   (terpri out)))
      (declare (ignore value))
      (unless condition
	(setq condition
	      (and *validate-saved-proofs*
		   (invalid-proof-file prf-file theory-proofs))))
      (or (not condition)
	  (and (pvs-yes-or-no-p
		"Error writing out proof file:~%  ~a~%Try again?"
		condition)
	       (save-proofs-to-file prf-file theory-proofs)
	       (pvs-message "Proof not saved"))))))


(defun backup-proof-file (file)
  (let ()
    (if (= *number-of-proof-backups* 1)
	(let ((bfile (make-pathname :type "prf~" :defaults file)))
	  (rename-file file bfile)
	  (pvs-log "Renamed ~a to ~a" file bfile))
	(let* ((filestring (namestring file))
	       (files (directory (concatenate 'string filestring ".~*~")))
	       (numbers (remove-if #'null
			  (mapcar #'(lambda (fname)
				      (parse-integer (pathname-type fname)
						     :start 1 :junk-allowed t))
			    files)))
	       (min (when numbers (apply #'min numbers)))
	       (max (if numbers (apply #'max numbers) 0)))
	  (when (= (length files) *number-of-proof-backups*)
	    (let ((ofile (format nil "~a.~~~d~~" filestring min)))
	      (when (file-exists-p ofile)
		(ignore-file-errors
		 (delete-file ofile)))))
	  (let ((nfile (format nil "~a.~~~d~~" filestring (1+ max))))
	    (rename-file file nfile)
	    (pvs-log "Renamed ~a to ~a" file nfile))))))

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
  "Given a list of theories (usually associated with a pvs file), generates
   a list of theory-proofs-sexp of the form
   ((thid (declid index prfinfo prfinfo ...) ...) ...)
   The index is the 0-based index to the default proof for the decl.
   Each prfinfo is of the form
   (prfid description create-date script refers-to decision-procedure-used [tcc-origin])
   The tcc-origin is only for TCCs, and has the form
   (root kind expr type)"
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

;;; Top level, called from restore-from-context and install-pvs-proof-file

(defun restore-proofs (filename theory &optional proofs)
  (let* ((*current-context* (context theory))
	 (*generate-tccs* 'none)
	 (aproofs (or proofs (read-pvs-file-proofs filename)))
	 (tproofs (cdr (assq (id theory) aproofs))))
    (when tproofs
      (restore-theory-proofs theory tproofs))))

;;; Proofs currently are of the form
;;;  (declid index prfinfo ...)
;;; where each proofinfo is of the form
;;;  (prfid description create-date script refers-to decision-procedure-used [origin])
;;; The origin is only there for TCCs, and is of the form
;;;  (root kind expr type)

;;; Proofs were stored in files in various forms over the years
;;; Before multiple proofs, a proof for a declid was of the form
;;;  (declid script)
;;;  (declid (...) script)  ;; I don't remember what the second field had in it
;;; After multiple proofs only the prfinfo is different
;;;  (prfid description create-date run-date script status refers-to
;;;	    real-time run-time interactive? decision-procedure-used)
;;; but rerunning could change the run-date, status, real-time, run-time, and interactive?
;;; slots, causing proof files to be saved unnecessarily

;;; We still try to accomodate old .prf files, as seen below

(defun restore-theory-proofs (theory proofs)
  (assert (every #'(lambda (prf)
		     (and (listp prf) (integerp (cadr prf))))
		 proofs))
  (let* ((have-tcc-origins?
	  (some #'(lambda (prf) (= (length (car (cddr prf))) 7)) proofs))
	 (te (get-context-theory-entry (id theory)))
	 (rem-proofs (restore-decls-proofs
		      ;; Note that formal parameters cannot have formulas,
		      ;; even TCCs are put in the assuming or theory parts
		      (append (assuming theory) (theory theory))
		      proofs
		      have-tcc-origins?)))
    (when (and te (memq 'invalid-proofs (te-status te)))
      (invalidate-proofs theory))
    (copy-proofs-to-orphan-file (id theory) rem-proofs)))

(defun restore-decls-proofs (decls proofs have-tcc-origins?)
  (cond ((null decls)
	 proofs)
	((and have-tcc-origins? (tcc? (car decls)))
	 ;; Note that TCCs generated from the formal parameters will all be
	 ;; put in either the assuming part, if it exists, or the theory
	 ;; part It's the only exception to TCCs appearing before the
	 ;; declaration that generated them
	 (multiple-value-bind (tccs rem-decls tcc-proofs rem-proofs)
	     (decl-tccs-and-proofs decls proofs)
	   ;; (mapcar #'(lambda (tcc) (sexp (origin tcc))) tccs)
	   ;; (mapcar #'(lambda (prf) (nth 6 (caddr prf))) tcc-proofs)
	   (restore-tcc-proofs tccs tcc-proofs)
	   (restore-decls-proofs rem-decls rem-proofs have-tcc-origins?)))
	((formula-decl? (car decls))
	 (let ((rem-proofs (restore-formula-proofs (car decls) proofs)))
	   (restore-decls-proofs (cdr decls) rem-proofs have-tcc-origins?)))
	(t (restore-decls-proofs (cdr decls) proofs have-tcc-origins?))))

(defun restore-formula-proofs (decl proofs)
  ;; decl is a formula-decl, but either not a TCC, or the proofs don't have
  ;; origins
  (let ((prf-entry (assq (id decl) proofs)))
    (cond (prf-entry
	   (let ((dproofs (make-proof-infos-from-sexp decl prf-entry)))
	     (setf (proofs decl) dproofs)
	     (setf (default-proof decl)
		   (nth (cadr prf-entry) (proofs decl))))
	   (let ((fe (get-context-formula-entry decl)))
	     (setf (status (default-proof decl))
		   (if fe
		       (fe-status fe)
		       'unchecked)))
	   (remove prf-entry proofs))
	  (t ;; Decl has no associated proof in file
	   proofs))))

(defmethod make-proof-infos-from-sexp ((decl tcc-decl) prf-entry)
  (mapcar #'(lambda (prf)
	      (assert (or (= (length prf) 6)
			  (= (length prf) 7)))
	      (let ((prinfo (apply #'mk-tcc-proof-info prf)))
		(setf (origin prinfo) (origin decl))
		prinfo))
    (cddr prf-entry)))
  
(defmethod make-proof-infos-from-sexp ((decl formula-decl) prf-entry)
  (mapcar #'(lambda (prf)
	      (assert (= (length prf) 6))
	      (apply #'mk-proof-info prf))
    (cddr prf-entry)))

(defun decl-tccs-and-proofs (decls proofs)
  (assert (tcc? (car decls)))
  (multiple-value-bind (tcc-decls rem-decls)
      (decl-tccs (car decls) (cdr decls))
    (multiple-value-bind (tcc-proofs rem-proofs)
	(collect-tcc-proofs (car decls) proofs)
      (values tcc-decls rem-decls tcc-proofs rem-proofs))))

(defun decl-tccs (tcc-decl decls &optional tccs rem-decls)
  "Collect all TCCs following the given one that are for the same root
declaration"
  (if (null decls)
      (values (cons tcc-decl (nreverse tccs)) (nreverse rem-decls))
      (if (and (tcc-decl? (car decls))
	       (eq (root (origin tcc-decl))
		   (root (origin (car decls)))))
	  (decl-tccs tcc-decl (cdr decls) (cons (car decls) tccs) rem-decls)
	  (decl-tccs tcc-decl (cdr decls) tccs (cons (car decls) rem-decls)))))

(defun collect-tcc-proofs (tcc proofs &optional tcc-proofs rem-proofs)
  ;; Each elt of proofs has form (declid index prfinfo ...)
  ;; All should have the same origin info - redundant, but trying to keep
  ;; things backward compatible
  (if (null proofs)
      (values (sort tcc-proofs #'< :key #'(lambda (prf) (numeric-suffix (car prf))))
	      (nreverse rem-proofs))
      (let* ((prfinfo (caddr (car proofs)))
	     (prf-origin (nth 6 prfinfo)))
	(if (eq (root (origin tcc)) (car prf-origin))
	    (collect-tcc-proofs tcc (cdr proofs)
				(cons (car proofs) tcc-proofs) rem-proofs)
	    (collect-tcc-proofs tcc (cdr proofs)
				tcc-proofs (cons (car proofs) rem-proofs))))))

(defun numeric-suffix (obj)
  (let* ((str (string obj))
	 (pos (position-if-not #'digit-char-p str :from-end t)))
    (assert (and pos (not (= (1+ pos) (length str)))))
    (parse-integer str :start (1+ pos))))

(defun restore-tcc-proofs (tccs proofs)
  "Restores proofs to TCCs, by trying to associate them based on origin.
tccs is a list of tcc-decls, and proofs are proofs with the same origin root.
Note that the lists might not be the same length."
  (restore-tcc-proofs* tccs proofs))

;;; The TCCs all come from the same declaration/importing
;;; The proofs are all the ones with the same root (i.e., from the same declaration)

(defun restore-tcc-proofs* (tccs proofs &optional rem-tccs)
  (when proofs
    (if (null tccs)
	;; Deal with remaining TCCs
	(if (null rem-tccs)
	    proofs ;; these will be orphaned
	    (when proofs
	      ;; Whatever is left, we match the first by kind
	      (setf rem-tccs (nreverse rem-tccs))
	      #+pvs-tcc-test (when rem-tccs (break "match by kind only"))
	      (dolist (tcc rem-tccs)
		(let ((mproof (find-if #'(lambda (prf)
					   (let* ((prfinfo (car (cddr prf)))
						  (prf-orig (nth 6 prfinfo)))
					     (eq (kind (origin tcc))
						 (cadr prf-orig))))
				proofs)))
		  (when mproof
		    #+pvs-tcc-test (unless (eq (car mproof) (id tcc)) (break "maybe wrong TCC"))
		    (restore-proof-to-tcc tcc mproof)
		    (setf proofs (remove mproof proofs)))))
	      (let ((unassigned-tccs (remove-if #'(lambda (tcc) (proofs tcc))
				       rem-tccs)))
		(when (and proofs unassigned-tccs)
		  #+pvsdebug
		  (break "Why are there unassigned-tccs with proofs left?")
		  (mapc #'restore-proof-to-tcc unassigned-tccs proofs)))
	      proofs))
	(let* ((mproofs (tcc-expr-matches (car tccs) proofs))
	       (mproof (when mproofs
			 (if (cdr mproofs)
			     (or (find-if #'(lambda (prf)
					      (let* ((prfinfo (car (cddr prf)))
						     (prf-orig (nth 6 prfinfo)))
						(string= (type (origin (car tccs)))
							 (cadddr prf-orig))))
				   mproofs)
				 (find-if #'(lambda (prf)
					      (let* ((pr-id (car prf)))
						(string= (id (car tccs)) pr-id)))
				   mproofs)
				 (car mproofs))
			     (car mproofs)))))
	  (cond (mproof
		 #+pvs-tcc-test
		 (unless (eq (car mproof) (id (car tccs))) (break "maybe wrong TCC 2"))
		 (restore-proof-to-tcc (car tccs) mproof)
		 (restore-tcc-proofs* (cdr tccs) (remove mproof proofs) rem-tccs))
		(t #+pvs-tcc-test
		 (break "mproof not found")
		   ;; (mapcar #'(lambda (tcc) (sexp (origin tcc))) tccs)
		   ;; (mapcar #'(lambda (prf) (nth 6 (caddr prf))) proofs)
		   (restore-tcc-proofs* (cdr tccs) proofs (cons (car tccs) rem-tccs))))))))

(defun tcc-expr-matches (tcc proofs)
  (let ((tcc-orig (origin tcc)))
    (remove-if #'(lambda (prf)
		   ;; prf ~ (declid index prfinfo prfinfo ...)
		   (let* ((prfinfo (car (cddr prf)))
			  (prf-orig (nth 6 prfinfo)))
		     ;; proof-orig ~ (root kind expr type)
		     ;; Note that we're assuming each of the multiple-proofs
		     ;; shares the same origin, so we only need to look at
		     ;; the first one
		     (assert (eq (root tcc-orig) (car prf-orig)))
		     (or (not (string-equal (kind tcc-orig) (cadr prf-orig)))
			 (not (string= (expr tcc-orig) (caddr prf-orig))))))
      proofs)))

(defun restore-proof-to-tcc (tcc mproof)
  (let ((tcc-proofs (mapcar #'(lambda (mprf)
				(let ((pinfo (apply #'mk-tcc-proof-info mprf)))
				  (setf (origin pinfo) (origin tcc))
				  pinfo))
		      (cddr mproof))))
    (setf (proofs tcc) tcc-proofs)
    (setf (default-proof tcc) (nth (cadr mproof) tcc-proofs))))

(defun get-smaller-proof-info (pr)
  ;; Older proofs had long lists
  (if (> (length pr) 7)
      (list (nth 0 pr) (nth 1 pr) (nth 2 pr) (nth 4 pr) (nth 6 pr) (nth 10 pr))
      pr))

(defun convert-proof-form-to-lowercase (proof-form)
  (cond ((symbolp proof-form)
	 (intern (string-downcase proof-form) (symbol-package proof-form)))
	((consp proof-form)
	 (cons (convert-proof-form-to-lowercase (car proof-form))
	       (convert-proof-form-to-lowercase (cdr proof-form))))
	(t proof-form)))

(defvar *pvs-class-names* nil)

(defun pvs-class-names ()
  (unless *pvs-class-names*
    (all-subclasses (find-class 'syntax)))
  *pvs-class-names*)

(defun all-subclasses (class)
  (let ((cname (class-name class)))
    (unless (memq cname *pvs-class-names*)
      (push cname *pvs-class-names*)
      (dolist (subclass (class-direct-subclasses class))
	(all-subclasses subclass)))))

(defun convert-refersto-to-lowercase (refers-to)
  ;; refers-to is a list of lists of the form
  ;; ((decl-id class type theory-id) ...)
  (mapcar #'convert-refersto-to-lowercase* refers-to))

(defun convert-refersto-to-lowercase* (ref)
  (unless (memq ref '(nil NIL))
    (let ((id (car ref))		; could check it exists
	  (class (if (find-class (cadr ref) nil)
		     (cadr ref)
		     (or (find-if #'(lambda (x) (string-equal x (cadr ref)))
			   (pvs-class-names))
			 (break "CLASS not found, proof file probably corrupt"))))
	  (type (unless (eq (caddr ref) 'NIL) (caddr ref)))		; a string
	  (theory-id (unless (eq (cadddr ref) 'NIL) (cadddr ref))))
      (list id class type theory-id))))

(defun copy-theory-proofs-to-orphan-file (theoryref)
  (when (if *loading-prelude*
	    (write-permission? (format nil "~a/lib" *pvs-path*))
	    (write-permission?))
    (let* ((tid (ref-to-id theoryref))
	   (filename (if *loading-prelude*
			 (if (member tid (core-prelude-theories) :key #'id)
			     "prelude" "pvsio_prelude")
			 (context-file-of theoryref)))
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
  (when (and proofs
	     (if *loading-prelude*
		 (write-permission? (format nil "~a/lib" *pvs-path*))
		 (write-permission?)))
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

(defvar *proof-file-debug* nil)

(defun read-pvs-file-proofs (filename &optional (dir *default-pathname-defaults*))
  (let ((prf-file (make-prf-pathname filename dir)))
    (when (file-exists-p prf-file)
      (if *proof-file-debug*
	  (with-open-file (input prf-file :direction :input)
	    (let ((proofs (read-proof-file-stream input)))
	      (make-current-proofs-sexps proofs)))
	  (handler-case 
	      (with-open-file (input prf-file :direction :input)
		(let ((proofs (read-proof-file-stream input)))
		  (make-current-proofs-sexps proofs)))
	    (error (condition)
	      (pvs-message "Error reading proof file ~a:~%  ~a"
		(namestring prf-file) condition)
	      nil))))))

(defun read-proof-file-stream (input &optional proofs)
  (let ((nproof (read-proof input 'eof)))
    (if (eq nproof 'eof)
	(nreverse proofs)
	(let ((cproof (convert-proof-if-needed nproof)))
	  (read-proof-file-stream
	   input
	   (if (member cproof proofs :test #'equal)
	       proofs
	       (cons cproof proofs)))))))

(defun make-current-proofs-sexps (proofs)
  ;; proofs of form ((thid (declid index prf prf ...) ...) ...)
  ;; Look at the first nonempty proof, see if it has the right form
  (let* ((tprf (find-if #'cdr proofs))
	 (fprf (cadr tprf)))
    (if (and (integerp (cadr fprf))
	     (or (= (length (caddr fprf)) 6)
		 (= (length (caddr fprf)) 7)))
	;; First one is OK, assume the rest are also
	proofs
	(make-current-proofs-sexps* proofs))))

(defun make-current-proofs-sexps* (proofs &optional fproofs)
  (if (null proofs)
      (nreverse fproofs)
      (let* ((proof (car proofs))
	     (fproof (cons (car proof)
			   (mapcar #'make-current-proof-sexps
			     (cdr proof)))))
	(make-current-proofs-sexps* (cdr proofs) (cons fproof fproofs)))))

(defun make-current-proof-sexps (decl-proof)
  (if (integerp (cadr decl-proof))
      (cons (car decl-proof)
	    (cons (cadr decl-proof)
		  (mapcar #'make-current-prfinfo-sexp (cddr decl-proof))))
      (cons (car decl-proof)
	    (cons 0 (make-current-prfinfo-sexp (cdr decl-proof))))))

(defun make-current-prfinfo-sexp (prf)
  (assert (= (length prf) 11))
  (list (first prf) (second prf) (third prf) (fifth prf) (seventh prf) (nth 10 prf)))

#+allegro
(defun read-proof (stream eof-value)
  (read stream nil eof-value))

;;; This allows proof files to be read when they were produced by
;;; case-sensitive lisp (i.e., Allegro).  It does this by selectively
;;; using different readtables, according to the depth of the parentheses.

;;; A proof file (extension .prf) has the following form:
;;; proof-file := {'(' theoryid  {'(' declid proof+ ')'} + ')'} +
;;; proof := '(' proofid      % symbol
;;;              description  % string
;;;              create-date  % integer
;;;              run-date     % integer
;;;              script       % sexpr
;;;              status       % symbol
;;;              refers-to    % list of declaration references
;;;              real-time    % integer
;;;              run-time     % integer
;;;              interactive? % symbol
;;;              decision-procedure-used % symbol
;;;          ')'

;;; The theoryid, declid, proofid, and refers-to should be read in a case
;;; sensitive way, but the status, interactive? and decision-procedure-used
;;; should be read in a case-insensitive mode.  The script is a bit tricky,
;;; but for now we will read it case-insensitively.

;;; read-proof simply reads the next proof in the stream,
;;; The way we do this is to use read-char to read the first \#(, then keep
;;; track of what we should be reading, and read using the proper readtable

#+(or cmu sbcl)
(defun read-proof (stream eof-value)
  (read-case-sensitive stream eof-value))
  
  ;; ;; For now, we ignore possible comments - since the proof file should be
  ;; ;; generated by PVS, this is reasonably safe.  So we just look for the
  ;; ;; first #\(, and return eof-value if we run off the end.
  ;; (let ((tag (gensym)))
  ;;   (block tag
  ;;     (handler-bind ((end-of-file #'(lambda (c)
  ;; 				      (declare (ignore c))
  ;; 				      (return-from tag eof-value))))
  ;; 	(read-to-left-paren stream)
  ;; 	(let ((theoryid (read-case-sensitive stream))
  ;; 	      (decls-proofs (read-proof-decls stream)))
  ;; 	  ;;(read-to-right-paren stream)
  ;; 	  (cons theoryid decls-proofs))))))

#+(or cmu sbcl)
(defun read-proof-decls (stream &optional proofs)
  (let ((paren (read-to-paren stream)))
    (if (char= paren #\()
	(let ((declid (read-case-sensitive stream))
	      (default (read stream)))
	  (if (integerp default)
	      (let ((decl-proofs (read-proofs-of-decls stream)))
		;; no need to (read-to-right-paren stream)
		(read-proof-decls stream
				  (cons (cons declid (cons default decl-proofs))
					proofs)))
	      ;; Old-style proof
	      (let ((proof (read-delimited-list #\) stream)))
		(read-proof-decls stream
				  (cons (cons declid (cons default proof))
					proofs)))))
	(nreverse proofs))))

#+(or cmu sbcl)
(defun read-proofs-of-decls (stream &optional proofs)
  (let ((char (read-to-paren stream)))
    (if (char= char #\()
	;; Old proof had 11 entries; now 6 or 7
	;; Old:                         New:
	;; ------------------------     ------------------------
	;; id                           id
	;; description                  description
	;; create-date                  create-date
	;; run-date
	;; script                       script
	;; status
	;; refers-to                    refers-to
	;; real-time
	;; run-time
	;; interactive?
	;; decision-procedure-used      decision-procedure-used
	;;                              origin   (only for TCCs)
	(let* ((proofid (read-case-sensitive stream))
	       (description (read stream nil))
	       (create-date (read stream nil))
	       (script (read stream nil))
	       ;; After this, it depends on if script is integer
	       ;; do the actual reading below
	       (status nil)
	       (refers-to nil)
	       (real-time nil)
	       (run-time nil)
	       (interactive? nil)
	       (dp nil)
	       (origin nil))
	  (cond ((and script (integerp script))
		 ;; Old
		 (setq script (read stream nil))
		 (setq status (read stream nil))
		 (setq refers-to
		       (if *proof-file-debug*
			   (read-proofs-refers-to stream)
			   (ignore-errors (read-proofs-refers-to stream))))
		 (setq real-time (read stream nil))
		 (setq run-time (read stream nil))
		 (setq interactive? (read stream nil))
		 (setq dp (read stream nil)))
		(t
		 (setq refers-to
		       (if *proof-file-debug*
			   (read-proofs-refers-to stream)
			   (ignore-errors (read-proofs-refers-to stream))))
		 (setq dp (read stream nil))
		 ;; Check for origin
		 (when (let ((ch (read-char stream)))
			 (unread-char ch stream)
			 (not (char= ch #\))))
		   (setq origin (read-case-sensitive stream)))))
	  (read-to-right-paren stream)
	  (read-proofs-of-decls
	   stream
	   (cons (list proofid description create-date script
		       refers-to dp origin)
		 proofs)))
	(nreverse proofs))))

#+(or cmu sbcl)
(defun read-proofs-refers-to (stream)
  ;; list of form (id class type theory-id library-id)
  ;; The class should be up-cased, and any |nil|s should be upcased
  (let ((refers-to (read-case-sensitive stream)))
    (if (symbolp refers-to)
	;; Could be nil, or status from old proof-info format
	(intern (string-upcase refers-to) :pvs)
	(mapcar #'(lambda (ref)
		    (unless (eq ref '|nil|)
		      (list (first ref)
			    (intern (string-upcase (second ref)) :pvs)
			    (if (eq (third ref) '|nil|) nil (third ref))
			    (fourth ref)
			    (if (eq (fifth ref) '|nil|) nil (fifth ref)))))
	  refers-to))))
      
#+(or cmu sbcl)
(defun read-to-paren-or-quote (stream)
  (let ((ch (read-char stream)))
    (if (member ch '(#\( #\) #\") :test #'char=)
	ch
	(read-to-paren stream))))

#+(or cmu sbcl)
(defun read-to-paren (stream)
  (let ((ch (read-char stream)))
    (if (member ch '(#\( #\)) :test #'char=)
	ch
	(read-to-paren stream))))

#+(or cmu sbcl)
(defun read-to-left-paren (stream)
  (let ((ch (read-char stream)))
    (unless (char= ch #\()
      (read-to-left-paren stream))))

#+(or cmu sbcl)
(defun read-to-right-paren (stream)
  (let ((ch (read-char stream)))
    (unless (char= ch #\))
      (read-to-left-paren stream))))

#+(or cmu sbcl)
(defvar *case-sensitive-readtable* nil)

#+(or cmu sbcl)
(defun read-case-sensitive (stream eof-value)
  (unless *case-sensitive-readtable*
    (setq *case-sensitive-readtable* (copy-readtable nil))
    (setf (readtable-case *case-sensitive-readtable*) :preserve))
  (let ((*readtable* *case-sensitive-readtable*))
    (read stream nil eof-value)))

(defun upcase-symbols (val)
  (cond ((consp val)
	 (let ((lcar (upcase-symbols (car val)))
	       (lcdr (upcase-symbols (cdr val))))
	   (cons lcar lcdr)))
	((keywordp val) val)
	((symbolp val)
	 (read-from-string (string val)))
	(t val)))

(defun upcase-t-and-nil (val)
  (cond ((consp val)
	 (let ((lcar (upcase-t-and-nil (car val)))
	       (lcdr (upcase-t-and-nil (cdr val))))
	   (cons lcar lcdr)))
	((eq val '|t|) t)
	((eq val '|nil|) nil)
	(t val)))
	 

;;; proofs is the proofs for a theory
;;; Note that only very old proofs need this
(defun convert-proof-if-needed (theory-proofs)
  ;; This may be a problem when proof is generated by SBCL and read in
  ;; Allegro, or vice-versa
  (cons (car theory-proofs)
	(mapcar #'convert-proof-if-needed* (cdr theory-proofs))))

;;; Proof for a formula
(defun convert-proof-if-needed* (formula-proof)
  (if (integerp (cadr formula-proof))
      (cons (car formula-proof)
	    (cons (cadr formula-proof)
		  (mapcar #'convert-proof-if-needed** (cddr formula-proof))))
      (let* ((script (if (consp (cadr formula-proof))
		     (cddr formula-proof)
		     (cdr formula-proof)))
	     (prinfo (make-proof-info
		      #+case-sensitive
		      (convert-proof-form-to-lowercase script)
		      #-case-sensitive
		      script
		      (makesym "~a-1" (car formula-proof)))))
	(cons (car formula-proof)
	      (cons 0 (list (sexp prinfo)))))))

(defun convert-proof-if-needed** (mprf)
  (if (> (length mprf) 7)
      ;; Older proof
      (multiple-value-bind (id description create-date run-date script
			       status refers-to real-time run-time
			       interactive? decision-procedure-used)
	  (values-list mprf)
	(declare (ignore run-date status real-time run-time interactive?))
	(convert-proof-form id description create-date script refers-to decision-procedure-used))
      (multiple-value-bind (id description create-date script refers-to
			       decision-procedure-used origin)
	  (values-list mprf)
	(convert-proof-form id description create-date script refers-to decision-procedure-used origin))))

(defun convert-proof-form (id description create-date script refers-to decision-procedure-used
			   &optional origin)
  (assert (or (stringp description) (memq description '(|nil| NIL))))
  (assert (or (listp script) (memq script '(|nil| NIL))))
  (assert (or (listp refers-to) (memq refers-to '(|nil| NIL))))
  (assert (symbolp decision-procedure-used))
  #+case-sensitive
  ;; Allegro in case-sensitive mode
  ;; May need to convert a proof done in case-insensitive mode
  (let* ((check (check-if-case-change-needed script))
	 (desc (unless (eq description 'NIL) description))
	 (scr (if check
		  (convert-proof-form-to-lowercase script)
		  script))
	 ;; refers-to should be fixed for Allegro/SBCL
	 (ref (if check
		  (unless (eq refers-to 'NIL)
		    (convert-refersto-to-lowercase refers-to))
		  refers-to))
	 (dec (unless (eq decision-procedure-used 'NIL) decision-procedure-used)))
    (if origin
	(list id desc create-date scr ref dec origin)
	(list id desc create-date scr ref dec)))
  #-case-sensitive ;; Others
  (let ((ncreate-date (upcase-t-and-nil create-date))
	(nscript (upcase-symbols script))
	(nrefers-to (upcase-t-and-nil refers-to))
	(ndp-used (upcase-symbols decision-procedure-used))
	(norigin (when (upcase-t-and-nil origin)
		   (list (list (car origin) (upcase-symbols (cadr origin))
			       (caddr origin) (cadddr origin))))))
    `(,id ,description ,ncreate-date ,nscript ,nrefers-to ,ndp-used
	  ,@norigin)))

#+case-sensitive
(defun check-if-case-change-needed (script)
  (let ((sym (find-first-symbol script)))
    (every #'upper-or-not-alpha-p (string sym))))

(defun upper-or-not-alpha-p (char)
  (or (not (alpha-char-p char))
      (upper-case-p char)))

(defun find-first-symbol (obj)
  (typecase obj
    (null nil)
    (string nil)
    (symbol obj)
    (cons (or (find-first-symbol (car obj))
	      (find-first-symbol (cdr obj))))))

#-case-sensitive
(defun check-if-case-change-needed (script)
  nil)
       

(defun transfer-orphaned-proofs (from-theory to-theory)
  (let* ((th (get-typechecked-theory to-theory))
	 (*current-context* (saved-context th))
	 (oprfs (read-orphaned-proofs from-theory)))
    (when (and th oprfs)
      (dolist (fdecl (provable-formulas (all-decls th)))
	(let ((prf (find (id fdecl) oprfs :key #'third)))
	  (if prf
	      (let ((prfs (mapcar #'(lambda (pr)
				      (let ((p (get-smaller-proof-info pr)))
					(apply #'mk-proof-info p)))
			    (cddr (cddr prf)))))
		(setf (proofs fdecl) prfs)
		(setf (default-proof fdecl) (nth (fourth prf) prfs)))
	      (format t "~%Couldn't find proof for ~a" (id fdecl))))))))

(defun read-orphaned-proofs (&optional theoryref)
  (let ((orph-file (if *loading-prelude*
		       (format nil "~a/lib/orphaned-proofs.prf" *pvs-path*)
		       "orphaned-proofs.prf")))
    (when (file-exists-p orph-file)
      (ignore-file-errors
       (with-open-file (orph orph-file :direction :input)
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
	   proofs))))))

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
			    (let ((prf (nth (cadddr proof) (cddddr proof))))
			      (if (> (length prf) 9)
				  (fifth prf)
				  (fourth prf)))
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
	    (write (get-editable-justification (cddr proof))
		   :stream out :pretty t :escape t :level nil :length nil
		   :pprint-dispatch *proof-script-pprint-dispatch*))
	  t)
	(pvs-message "Cannot find proof for this entry"))))

(defun pvs-delete-proof (num)
  (if (write-permission?)
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
			 (directory-p *pvs-path*)
			 "pvs-strategies"))
	(home-strat-file (merge-pathnames "~/" "pvs-strategies"))
	(ctx-strat-file (merge-pathnames *default-pathname-defaults*
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
  (let* (#+allegro
	 (sys:*load-search-list* *pvs-library-path*)
	 (file (make-pathname :directory (namestring lib) :name "pvs-strategies")))
    (when (file-exists-p file)
      (let ((fwd (file-write-time file))
	    (entry (assoc lib *library-strategy-file-dates* :test #'equal)))
	(unless (and (cdr entry)
		     (= fwd (cdr entry)))
	  (if entry
	      (setf (cdr entry) fwd)
	      (push (cons lib fwd) *library-strategy-file-dates*))
	  (unless (ignore-lisp-errors (load file :verbose nil))
	    (with-open-file (str file :direction :input)
	      (if *testing-restore*
		  (load str :verbose t)
		  (unless (ignore-errors (load str :verbose nil))
		    (pvs-message "Error in loading ~a" file))))))))))

(defun load-strategies-file (file dates)
  (let ((*loading-files* :strategies))
    (when (file-exists-p file)
      (let ((fwd (file-write-time file)))
	(unless (= fwd (car dates))
	  (setf (car dates) fwd)
	  #+(and allegro (version>= 6) (not pvs6) (not pvs7))
	  (unwind-protect
	       (progn (excl:set-case-mode :case-insensitive-lower)
		      (multiple-value-bind (v err)
			  (ignore-errors (load file))
			(declare (ignore v))
			(when err
			  (pvs-message "Error in loading ~a:~%  ~a" file err))))
	    (excl:set-case-mode :case-sensitive-lower)
	    (add-lowercase-prover-ids))
	  #+(and allegro (version>= 6) (or pvs6 pvs7))
	  (unwind-protect
	       (multiple-value-bind (v err)
		   (ignore-errors (load file))
		 (declare (ignore v))
		 (when err
		   (pvs-message "Error in loading ~a:~%  ~a" file err)))
	    (add-lowercase-prover-ids))
	  #-(and allegro (version>= 6))
	  (with-open-file (str file :direction :input)
	    (multiple-value-bind (v err)
		(ignore-lisp-errors (load str))
	      (declare (ignore v))
	      (when err
		(pvs-message "Error in loading ~a:~%  ~a" file err)))))))))


(defun add-lowercase-prover-ids ()
  (add-lowercase-prover-hash-ids *rulebase*)
  (add-lowercase-prover-hash-ids *rules*)
  (add-lowercase-prover-hash-ids *steps*)
  (add-lower-case-prover-keywords))

(defun add-lowercase-prover-hash-ids (hash)
  (let ((new-entries nil))
    (maphash #'(lambda (id entry)
		 (when (some #'upper-case-p (string id))
		   (let* ((lid (intern (string-downcase (string id)) :pvs))
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
	(let ((lid (intern (string-downcase (string id)) :pvs)))
	  (unless (assoc lid *prover-keywords*)
	    (push (cons lid entry) *prover-keywords*)))))))

(defun make-prf-pathname (fname &optional (dir *default-pathname-defaults*))
  (assert (or (stringp fname) (pathnamep fname)))
  (let* ((pname (pathname fname))
	 (name (if (let ((type (pathname-type pname)))
		     (and type (not (string= type "pvs")) (not (string= type "prf"))))
		   (format nil "~s" pname)
		 (pathname-name pname)))
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


(defun collect-all-theories ()
  "Collects all parsed theories"
  (let ((theories nil))
    (labels ((collect-theory (thid th)
	       (declare (ignore thid))
	       (when (module? th)
		 (pushnew th theories)))
	     (collect-thrys (thrys)
	       (maphash #'collect-theory thrys)))
      (maphash #'collect-theory (current-pvs-theories))
      (maphash #'collect-theory *prelude*)
      (dolist (ws *all-workspace-sessions*)
	(collect-thrys (pvs-theories ws))))
    (sort theories #'string-lessp :key #'id)))

(defun collect-prelude-theories ()
  (mapcar #'(lambda (th)
	      (list (string (id th))
		    (full-pvs-file-path th)
		    (coerce (place th) 'list)))
    *prelude-theories*))

;;; Called from emacs for theory completion
(defun collect-theories (&optional no-prelude?)
  "Returns the alist of known theories and their context-paths, including
the prelude and any parsed or typchecked theory from any current
workspace-session.  The current context is first, followed by the rest of
the workspaces in *all-workspace-sessions*, followed by the prelude.  Within
each context, the theories are in alphabetic order."
  (let ((theories nil))
    (do-all-theories
	#'(lambda (th)
	    (push (list (string (id th))
			(format nil "~a/~a.pvs"
			  (context-path th) (filename th))
			(place-list (place th)))
		  theories))
      no-prelude?)
    (nreverse theories)))

(defun collect-theory-file-pairs ()
  "Creates list of (theory-name pvs-file) pairs of strings."
  (let ((pairs (mapcan #'(lambda (th)
			   (unless (generated-by th)
			     (list
			      (list (if (and (lib-datatype-or-theory? th)
					     (not (from-prelude? th)))
					(format nil "~a@~a"
					  (get-library-id (context-path th)) (id th))
					(string (id th)))
				    (cond ((filename th))
					  ((memq th (pvsio-prelude-theories))
					   (format nil "~a/lib/pvsio_prelude" *pvs-path*))
					  ((from-prelude? th)
					   (format nil "~a/lib/prelude" *pvs-path*))
					  (t (error "filename not set in theory ~a" th)))))))
		   (collect-all-theories))))
    ;; Now loop through the .pvscontext entries, from earlier parses.
    (dolist (ce (cdddr (current-pvs-context)))
      (dolist (te (ce-theories ce))
	(unless (assoc (string (te-id te)) pairs :test #'string=)
	  (push (list (string (te-id te)) (ce-file ce)) pairs))
	;; In principle, should be able to follow dependencies, but not enough info
	;; currently - see below.
	;; (let ((fdeps (ce-dependencies ce))) ; lib/file strings
	;;   (dolist (dep (te-dependencies te))
	;;     (dolist (dep-th (cdr dep))
	;;       ;; Need to associate "lib/file" of fdep to ("lib" "thid") of dep
	;;       ;; Not enough info here, unless lib is empty.
	;;       (pushnew (format nil "~a@~a" (car dep) dep-th) thnames :test #'string=))))
	))
    (sort pairs #'string-lessp :key #'car)))

(defun collect-theory-names ()
  "Creates list of theory-name strings, of the form \"libref@thid\", or just
\"thid\" in the current-context."
  
  (let ((thnames (mapcar #'(lambda (th)
			     (if (and (lib-datatype-or-theory? th)
				      (not (from-prelude? th)))
				 (format nil "~a@~a"
				   (get-library-id (context-path th)) (id th))
				 (string (id th))))
		   (collect-all-theories))))
    ;; Now loop through the .pvscontext entries, from earlier parses.
    (dolist (ce (cdddr (current-pvs-context)))
      (dolist (te (ce-theories ce))
	(pushnew (string (te-id te)) thnames :test #'string=)
	(dolist (dep (te-dependencies te))
	  (dolist (dep-th (cdr dep))
	    (pushnew (format nil "~a@~a" (car dep) dep-th) thnames :test #'string=)))))
    (sort thnames #'string-lessp)))

(defun collect-theories* (fe)
  (let ((file (string (ce-file fe))))
    (when (file-exists-p (make-specpath file))
      (mapcar #'(lambda (te)
		  (list (string (te-id te))
			file))
	(ce-theories fe)))))

(defun collect-element-ids (thid)
  (let ((th (get-theory thid)))
    (when th
      (let ((eltids nil))
	(dolist (elt (all-decls th))
	  (when (declaration? elt)
	    (pushnew (id elt) eltids))
	  (pushnew (unique-id elt) eltids))
	(sort eltids #'string<)))))

(defun find-all-usedbys (theoryref)
  "Returns theory ids for theories that depend on this theoryref"
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
			  (current-pvs-theories))))
	   (current-pvs-theories)))

;;; Called from Emacs
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
		       (let ((*current-context* (saved-context th)))
			 (restore-proofs prf-file th)
			 (clear-proof-status th)))
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
			   *default-pathname-defaults*))))

(defun auto-save-proof ()
  (assert *in-checker*)
  (when (and (write-permission?)
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
				      :defaults *default-pathname-defaults*
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

;; (defun check-pvs-context ()
;;   (dolist (ce (pvs-context-entries))
;;     (check-pvs-context-entry ce))
;;   (maphash #'check-pvs-context-files (current-pvs-files)))

;; (defun check-pvs-context-entry (ce)
;;   (let ((file (make-specpath (ce-file ce))))
;;     (and (file-exists-p file)
;; 	 (= (file-write-time file) (ce-write-date ce))
;; 	 ;; proofs-date
;; 	 ;; object-date
;; 	 ;; dependencies
;; 	 ;; theories
;; 	 ;; extension
;; 	 )))

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

(defun cleanup-proofs-pvs-file (file)
  (let* ((all-proofs (read-pvs-file-proofs file))
	 (aproofs (proofs-with-associated-decls file all-proofs))
	 (dproofs (collect-default-proofs aproofs)))
    (if (equalp all-proofs dproofs)
	(pvs-message "Proof file is already cleaned up")
	(let* ((prf-file (make-prf-pathname file))
	       (prf-fstr (namestring prf-file))
	       (prf-bak (concatenate 'string prf-fstr ".bak")))
	  (pvs-message "Moving ~a to ~a" prf-file prf-bak)
	  (rename-file prf-file prf-bak)
	  (pvs-message "Writing cleaned up proof file ~a" prf-file) 
	  (multiple-value-bind (value condition)
	      (ignore-file-errors
	       (with-open-file (out prf-file :direction :output
				    :if-exists :supersede)
		 (mapc #'(lambda (prf)
			   (write prf :length nil :level nil :escape t
				  :pretty *save-proofs-pretty*
				  :stream out)
			   (when *save-proofs-pretty* (terpri out)))
		       dproofs)
		 (terpri out)))
	    (declare (ignore value))
	    (if (or condition
		    (setq condition
			  (and *validate-saved-proofs*
			       (invalid-proof-file prf-file dproofs))))
		(pvs-message "Error writing out proof file:~%  ~a"
		  condition)
		(pvs-message "Proof file ~a written" prf-file)))))))

(defun collect-default-proofs (proofs)
  (mapcar #'collect-theory-default-proofs proofs))

(defun collect-theory-default-proofs (proofs)
  (cons (car proofs) ;; theory id
	(mapcar #'collect-formula-default-proofs (cdr proofs))))

(defun collect-formula-default-proofs (proofs)
  (let ((index (cadr proofs)))
    (list (car proofs) ;; formula id
	  0            ;; new index
	  (list (nth index (cddr proofs))))))

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

;;; Find the maximal theory elements according to the .pvscontext
;;; Example:
;;; (with-pvs-context "../prelude/"
;;;    (restore-context) (maximal-theory-hierarchy-elements))

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

(defvar *binfiles-checked*)

(defun check-binfiles (filename)
  (let ((*binfiles-checked* nil)
	(ce (get-context-file-entry filename)))
    (and ce
	 (every #'(lambda (te)
		    (and (loop for (lib-ref . theory-ids) in (te-dependencies te)
			    always (check-binfiles* lib-ref theory-ids))
			 (check-binfile (id te) ce)))
		(ce-theories ce)))))

(defun check-binfiles* (libref theory-ids)
  (if libref
      (let ((libstr (if (pathnamep libref) (namestring libref) libref)))
	(when (and (char= (char libstr 0) #\/) (not (file-exists-p libref)))
	  (let* ((lstr (if (char= (char libstr (1- (length libstr))) #\/)
			   (subseq libstr 0 (1- (length libstr)))
			   libstr))
		 (lpos (position #\/ lstr :from-end t)))
	    (when lpos
	      (setq libstr (subseq lstr (1+ lpos))))))
	(with-workspace libstr
	  (check-binfiles* nil theory-ids)))
      (every #'(lambda (thid)
		 (let ((ce (context-entry-of thid)))
		   (when ce
		     (check-binfile thid ce))))
	     theory-ids)))

;; (defvar *binfile-contexts*)
;; (defvar *ces-checked*)

;; (defun check-binfiles (filename)
;;   "Checks that the binfiles for filename and all its dependencies are valid,
;; e.g., the binfile date matches the ce-object-date of the current-pvs-context.
;; The dependencies come from ce-dependencies."
;;   (let ((*binfiles-checked* nil)
;; 	(*binfile-contexts* nil)
;; 	(*ces-checked* nil))
;;     (check-binfiles* filename)))

;; (defun check-binfiles* (filename)
;;   (let* ((lib-ref (simple-directory-namestring filename))
;; 	 (lib-path (when lib-ref (get-library-path lib-ref))))
;;     (unless (or (null lib-ref) lib-path)
;;       (break "bad path for ~a?" filename))
;;     (when (or (null lib-ref) lib-path) ; check-binfiles fails
;;       (if lib-path
;; 	  (with-workspace lib-path
;; 	    (check-binfiles** (file-namestring filename)))
;; 	  (check-binfiles** filename)))))

;; (defun simple-directory-namestring (filename)
;;   ;; if filename starts with a single ".", it is lost by directory-namestring
;;   ;; so we use a simpler form.
;;   (let ((pos (position #\/ filename :from-end t)))
;;     (when pos
;;       (subseq filename 0 (1+ pos)))))

;; (defun check-binfiles** (filename)
;;   (let ((ce (get-context-file-entry filename)))
;;     ;; (unless ce (break "No ce for ~a?" filename))
;;     (when ce
;;       (if (and (boundp '*ces-checked*)
;; 	       (assq ce *ces-checked*))
;; 	  (cdr (assq ce *ces-checked*))
;; 	  (let ((check (cond ((and (every #'check-binfiles* (ce-dependencies ce))
;; 				   (check-binfile filename ce)))
;; 			     (t ;;(when (assq 'sigma_l_adt (ce-object-date ce))
;; 				  ;;(break "Failed check-binfile: resetting ce" filename))
;; 				(if (listp (ce-object-date ce))
;; 				    (dolist (objdate (ce-object-date ce))
;; 				      (setf (cdr objdate) 0))
;; 				    (setf (ce-object-date ce) nil))
;; 				nil))))
;; 	    (setq *ces-checked* (acons ce check *ces-checked*))
;; 	    check)))))

(defun check-binfile (theory-id ce)
  "Checks that the file write date is after the spec write date, and matches
the ce-object-date for each theory in the file.  Note that each theory has a
binfile, not the filename."
  (when (listp (ce-object-date ce))
    (let* ((filename (ce-file ce))
	   (spec-file (make-specpath filename))
	   (file-info (get-file-info spec-file))
	   (checked (member file-info *binfiles-checked* :test #'equal)))
      (or checked
	  (let* ((spec-date (file-write-time spec-file))
		 (expected-spec-date (ce-write-date ce))
		 (expected-bin-date (cdr (assq theory-id (ce-object-date ce))))
		 (spec-ok? (and spec-date
				expected-spec-date
				(= spec-date expected-spec-date)))
		 (ok? (when spec-ok?
			(let ((bin-date (file-write-time (make-binpath theory-id))))
			  (and bin-date
			       expected-bin-date
			       (= bin-date expected-bin-date)
			       (<= spec-date bin-date))))))
	    (when ok? (push file-info *binfiles-checked*))
	    ok?)))))

(defun remove-binfiles ()
  (dolist (file (directory "pvsbin/"))
    (when (equal (pathname-type file) "bin")
      (delete-file file))))

(defmethod proved? ((fe formula-entry))
  (or (and (member (fe-status fe)
		   '(proved proved-complete proved-incomplete)
		   :test #'string-equal)
	   t)
      ;; TODO: formula-decl also has this; should fix for formula-entries
      ;; (when (mapped-formula-decl? fe)
      ;; 	(proved? (from-formula fe)))
      ))

(defmethod unproved? ((fdecl formula-entry))
  (not (proved? fdecl)))

(defun collect-current-default-proofs ()
  (maphash #'collect-default-proofs-theories (current-pvs-files)))

(defun collect-default-proofs-theories (fname theories)
  (with-open-file (out (format nil "~a.dprf" fname) :direction :output
		       :if-exists :supersede)
    (dolist (theory (cdr theories))
      (let ((proofs (collect-default-proofs-theory theory)))
	(write proofs :length nil :level nil :escape t
	       :pretty t
	       :stream out)
	(terpri out)))))

(defun collect-default-proofs-theory (theory)
  (cons (id theory)
	(collect-default-proofs-theory*
	 (append (assuming theory) (theory theory))
	 nil)))

(defun collect-default-proofs-theory* (decls proofs)
  (if (null decls)
      (nreverse proofs)
      (collect-default-proofs-theory*
       (cdr decls)
       (if (and (formula-decl? (car decls))
		(default-proof (car decls)))
	   (list (id (car decls)) 0 (sexp (default-proof (car decls))) proofs)
	   proofs))))

;; (defun check-proof-file-is-current (&optional file)
;;   (if file
;;       (check-proof-file-is-current* file (gethash file (current-pvs-files)))
;;       (maphash #'check-proof-file-is-current* (current-pvs-files))))

;; (defun check-proof-file-is-current* (file date&theories)
;;   (let ((prfpath (make-prf-pathname file))
;; 	(cur-proofs (collect-theories-proofs (cdr date&theories))))
;;     (if (file-exists-p prfpath)
;; 	(let ((file-proofs (read-pvs-file-proofs file)))
;; 	  (check-proof-file-is-current** file-proofs cur-proofs))
;; 	(unless (every #'(lambda (prf) (null (cdr prf))) cur-proofs)
;; 	  (error "~%Proof file ~a does not exist" prfpath)))))

;; (defun check-proof-file-is-current** (file-proofs cur-proofs)
;;     (if file-proofs
;; 	(let* ((file-theory-proofs (car file-proofs))
;; 	       (theoryid (car file-theory-proofs))
;; 	       (cur-theory-proofs (assq theoryid cur-proofs)))
;; 	  ;;(unless cur-theory-proofs
;; 	    ;;(error "No internal-proofs for ~a" theoryid))
;; 	  (when cur-theory-proofs
;; 	    (unless (proofs-equal file-theory-proofs cur-theory-proofs)
;; 	      (error "File and internal proof mismatch for ~a" theoryid)))
;; 	  (check-proof-file-is-current**
;; 	   (cdr file-proofs) (remove cur-theory-proofs cur-proofs)))
;; 	(if cur-proofs
;; 	    (error "No file proof for internal proofs: ~{~a~^, ~}"
;; 		   (mapcar #'car cur-proofs)))))

;; (defun proofs-equal (file-theory-proofs cur-theory-proofs)
;;   (every #'proofs-equal*
;; 	 (remove-if (complement #'(lambda (pf)
;; 				    (assq (car pf) cur-theory-proofs)))
;; 	   (cdr file-theory-proofs))
;; 	 (cdr cur-theory-proofs)))

;; (defun proofs-equal* (file-formula-proof cur-formula-proof)
;;   (and (eq (car file-formula-proof) (car cur-formula-proof)) ; formula id
;;        (= (cadr file-formula-proof) (cadr cur-formula-proof)) ; index
;;        (every #'proofs-equal**
;; 	      (cddr file-formula-proof) (cddr cur-formula-proof))))

;; (defun proofs-equal** (file-formula-proof cur-formula-proof)
;;   (and (eq (car file-formula-proof) (car cur-formula-proof)) ; proof id
;;        (equal (cadr file-formula-proof) (cadr cur-formula-proof)) ; description
;;        ;; create-date - ignore
;;        ;; run-date - ignore
;;        (equal (fifth file-formula-proof) (fifth cur-formula-proof)) ; script 
;;        ;; status - ignore
;;        ;; refers-to - ignore
;;        ;; real-time 
;;        ;; run-time
;;        ;; interactive?
;;        ;; decision-procedure-used
;;        ))
       
;; #+(and allegro (version>= 7))
;; (defun wild-pathname-p (pathname &optional field-key)
;;   nil)


;; Two functions are available for renaming: rename-module and
;; rename-declaration.

;; rename-declaration replaces one decl id for another throughout a context.
;; The context files are collected based on the .pvscontext, and they are
;; parsed and/or typechecked.  Then the possible declarations corresponding
;; to that id are found.  Give a warning if more than one declaration found,
;; that is not local (e.g., var-decl, bind-decl, or decl-parameter).  Set up
;; a substitution decl -> id.  This should be saved in a history.  The idea
;; is that if this is a change to a PVS library, then importing the library
;; would have the information needed for the typechecker to fix errors due
;; to renaming.

;; Having found the substitution, we need to replace this throughout the
;; .pvs and .prf files, as well as the .pvscontext, and possibly the
;; pvs-strategy and pvs-attachments files.  Bin files will simply be
;; ignored - they will be invalid for changed files anyway.

;; PVS files: if typechecked, can use this to unambiguously substitute,
;; except within comments, which will be treated the same as .prf files
;; (ProofLite uses comments to add proofs to the PVS spec).

;; Proof files: A safe, but expensive possibility is to rerun the proofs,
;; with hooks to do the substitution when strings are parsed or typechecked.
;; Otherwise, the proof file should be read in as a proof-info instance, and
;; the script, refers-to, and TCC origin get substituted.

;; .pvscontext: has context-entry instances, the theories slot is a list of
;; theory-entry instances, which have a formula-info slot, consisting of a
;; list of formula-entry instances.  This has an id that may need
;; substitution, and a proof-refers-to slot which is a list of
;; declaration-entry instances.  Each of these has an id, and a type, that
;; may need substituting.

;; if everything typechecked
;; then if rerunning proofs
;;      then bottom up, substitute the PVS file, typecheck it,
;;           and set up hooks for doing the substitution as the proof is rerun.
;;      else do the substitutions on the proof-info structures, and write them out.
;; elsif everything parsed

;; (defun rename-declaration (filename place newname)
;;   (parse-file filename nil t)
;;   (let ((id (get-name-at filename place)))
;;     (when id
;;       (if (string= id newname)
;; 	  (pvs-error "New name is the same as the old")

;;; Context includes the following environment variables and values at the
;;; end of the previous session:
;;;  PVSPATH - the path under which this context was last run
;;;  PVSMINUSQ - ignores ~/.pvs.lisp; pvs shell script sets this from '-q' arg
;;;  PVSINEMACS - whether to sync with Emacs
;;;  PVSNONINTERACTIVE - won't attempt to prompt user; just take default
;;;                      value when there are choices to be made.
;;;  PVSTIMEOUT -
;;;  PVSPORT - if set, uses this port for creating an xml-rpc server
;;;  PVS_LIBRARY_PATH
;;;  PVSLISP
;;;

(defun git-init ()
  (uiop:run-program (format nil "git init")
    :input "//dev//null"
    :output '(:string :stripped t)))

(defun git-add (file)
  (uiop:run-program (format nil "git add ~a" file)
    :input "//dev//null"
    :output '(:string :stripped t)))

(defun under-git-control? (&optional dir)
  "Checks if the current directory is under git control.

This is determined by git ls-files.  What this does is find an ancestor directory containing
a .git repo, and list the files of the current context that have been added to that repo.
If there is no such ancestor, this results in an error; this means it's safe create a repo.
If there is no error, but the ls-files is empty, then "
  (or (directory-p (format nil "~@[~a/~].git" dir))
      (let ((lsf (handler-case
		     (uiop:run-program (format nil "git ~@[-C ~a~] ls-files" dir)
		       :input "//dev//null"
		       :output '(:string :stripped t))
		   (uiop:subprocess-error (cnd)
		     (declare (ignore cnd)) nil))))
	(cond ((not (zerop (length lsf)))
	       ;; Already have files from this dir in a repo
	       t)
	      ((null lsf)
	       ;; Offer to create a new git repo
	       (when (pvs-yes-or-no-p
		      "Would you like to have PVS create a new Git repo (.git subdirectory)~%~
                         and automatically track files for this context?  It should not interfere~%~
                         with any existing repos.")
		 (git-init)
		 t))
	      ((and (zerop (length lsf))
		    ;; Ask whether to use ancestor
		    (pvs-yes-or-no-p
		     "Would you like to use the repo at ~a for pvs-related files in this directory?"
		     (uiop:run-program "git rev-parse --show-toplevel"
		       :input "//dev//null"
		       :output '(:string :stripped t))))
	       t)
	      ((and (zerop (length lsf))
		    ;; create .pvscontext, create new branch, save to git
		    (pvs-yes-or-no-p
		     "Would you like to create a new repo in the current directory?")
		    (git-init)
		    t))))))
	     
;; (defun pvs-context-info ()
;;   `((:pvs-files ,(maphash #'(lambda (name info)
;; 			      (declare (ignore info))
;; 			      (let ((ce (create-context-entry name)))
;; 				(when ce
;; 				  (push ce context))))
;; 			  (current-pvs-files)))
;;   (append (pvs-meta-info)
;; 	  (
  
(defun pvs-file-info (ce)
  `((:pvs-file . ,(ce-file ce))
    (:write-date . ,(ce-write-date ce))
    (:proofs-date . ,(ce-proofs-date ce))
    (:object-date . ,(ce-object-date ce))
    (:dependencies . ,(ce-dependencies ce))
    (:theories . ,(mapcar #'te-alist (ce-theories ce)))
    (:extension . ,(ce-extension ce))
    (:md5sum . ,(ce-md5sum ce))))

(defun te-alist (te)
  `((:id . ,(te-id te))
    (:status . ,(te-status te))
    (:dependencies . ,(te-dependencies te))
    (:formula-info . ,(te-formula-info te))))
