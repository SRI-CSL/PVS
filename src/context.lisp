;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; context.lisp -- PVS context, proof file, and binfile handling
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2026, SRI International. All Rights Reserved.
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the 3-Clause BSD License.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; 3-Clause BSD License for more details.
;; --------------------------------------------------------------------

(in-package :pvs)

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

(defun initial-context ()
  (make-pvscontext
   :pvs-version *pvs-version*
   :prelude-libs nil
   :parameters nil ;; list with, e.g., :default-decision-procedure
   :entries nil))

(defun current-pvs-context ()
  (assert *workspace-session*)
  (assert (memq *workspace-session* *all-workspace-sessions*))
  (pvscontext *workspace-session*))

(defun pvs-context-version (&optional (ctx (current-pvs-context)))
  (pvscontext-pvs-version ctx))

(defun pvs-context-libraries (&optional (ctx (current-pvs-context)))
  (pvscontext-prelude-libs ctx))

(defun pvs-context-entries (&optional (ctx (current-pvs-context)))
  (pvscontext-entries ctx))

;; The rest are on the pvscontext parameters 
(defun pvs-context-default-dp (&optional (ctx (current-pvs-context)))
  (or (getf (pvscontext-parameters ctx) :default-decision-procedure)
      'shostak))

(defun pvs-context-yices-executable (&optional (ctx (current-pvs-context)))
  (getf (pvscontext-parameters ctx) :yices-executable))

(defun pvs-context-yices2-executable (&optional (ctx (current-pvs-context)))
  (getf (pvscontext-parameters ctx) :yices2-executable))

(defmethod pvs-context ((ws workspace-session))
  (let* ((ctx-file (merge-pathnames ".pvscontext" (path ws)))
	 (fdate (when (uiop:file-exists-p ctx-file)
		  (file-write-date ctx-file))))
    (when (and fdate
	       (pvs-context-date ws)
	       (< (pvs-context-date ws) fdate))
      ;; Context was changed after being read
      (with-workspace ws
	(setf (pvscontext ws) (read-context-file ctx-file)))
      (setf (pvs-context-date ws) fdate)))
  (pvscontext ws))

(defsetf current-pvs-context () (pvsctx)
  `(setf (pvscontext *workspace-session*) ,pvsctx))

(defun current-pvs-context-changed ()
  (pvs-context-changed *workspace-session*))

(defsetf current-pvs-context-changed () (cc)
  `(setf (pvs-context-changed *workspace-session*) ,cc))

(defun current-context ()
  *current-context*)

(defmethod context-path ((ctx context))
  (context-path (theory ctx)))

(defmethod context-path ((decl declaration))
  (context-path (module decl)))

;; (defun context-entries-not-updated ()
;;   (mapcan #'(lambda (ce)
;; 	      (let ((nce (create-context-entry (ce-file ce))))
;; 		(unless (ce-eq ce nce)
;; 		  (list ce))))
;;     (pvs-context-entries)))

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
  (and (string= (te-id te1) (te-id te2))
       (equal (te-status te1) (te-status te2))
       (length= (te-dependencies te1) (te-dependencies te2))
       (null (set-difference (te-dependencies te1) (te-dependencies te2)
			     :key #'str :test #'string=))
       ;; This one is tricky, since after parsing there are no TCCs, while
       ;; after typechecking they show up.  However, since the only way to
       ;; get a difference here that matters is to change the source file,
       ;; which fe-eq will catch, we simply check that every one from on
       ;; list that can be found on the other is fe-eq.
       (every #'(lambda (fe2)
		  (let ((fe1 (find (fe-id fe2) (te-formula-info te1)
				   :key #'fe-id :test #'string=)))
		    (or (null fe1)
			(fe-eq fe1 fe2))))
	      (te-formula-info te2))))

(defun fe-eq (fe1 fe2)
  (and (string= (fe-id fe1) (fe-id fe2))
       (string-equal (fe-status fe1) (fe-status fe2))
       (length= (fe-proof-refers-to fe1) (fe-proof-refers-to fe2))
       (every #'de-eq (fe-proof-refers-to fe1) (fe-proof-refers-to fe2))))

(defun de-eq (de1 de2)
  (and (string= (de-id de1) (de-id de2))
       (string= (de-class de1) (de-class de2))
       (string= (de-type de1) (de-type de2))
       (string= (de-theory-id de1) (de-theory-id de2))))

(defmethod id ((entry theory-entry))
  (if (symbolp (te-id entry)) (te-id entry) (intern (te-id entry) :pvs)))

(defmethod id ((entry formula-entry))
  (if (symbolp (fe-id entry)) (fe-id entry) (intern (fe-id entry) :pvs)))

(defvar *valid-entries* nil)

(defvar *strat-file-dates* (list 0 0 0)
  "Used to keep track of the file-dates for the pvs, home, and context
pvs-strategies files.")

(defun context-pathname (&optional (dir *default-pathname-defaults*))
  (make-pathname :name *context-name* :defaults dir))

(defvar *dont-write-object-files* nil)

;;; Save Context - called from Emacs and parse-file (pvs.lisp), saves any
;;; changes since the last time the context was saved.

(defun save-context (&optional empty quiet?)
  "Saves the ~/.pvscontext and pvsbin/ files"
  #+pvsdebug
  (assert (uiop:file-equal *default-pathname-defaults* (working-directory))
	  () "Mismatch between *default-pathname-defaults* = ~a~%~
              and                      (working-directory) = ~a"
	  *default-pathname-defaults* (working-directory))
  #+pvsdebug
  (assert (uiop:file-equal *default-pathname-defaults* (current-context-path))
	  () "Mismatch between *default-pathname-defaults* = ~a~%~
              and                   (current-context-path) = ~a"
	  *default-pathname-defaults* (current-context-path))
  (if (not (uiop:directory-exists-p (working-directory)))
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
	(write-context *workspace-session* empty quiet?)
	;; (maphash #'(lambda (file theories)
	;; 	     (assert (or (some #'(lambda (th)
	;; 				   (not (typechecked? th)))
	;; 			       (cdr theories))
	;; 			 (check-binfiles file))))
	;; 	 (current-pvs-files))
	t)))

(defun sc ()
  (save-context))


(defun write-context (&optional (wksp *workspace-session*) empty? quiet?)
  (let ((ws (or wksp *workspace-session*)))
    (assert (pvs-context ws))
    (cond ((uiop:directory-exists-p (path ws))
	   (with-workspace ws
	     (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)))
	     (when (or (pvs-context-changed ws)
		       (and (pvs-context-entries)
			    (not (file-exists-p (path ws)))))
	       (if (write-permission? (path ws))
		   (let* ((context (if empty? (initial-context) (pvscontext ws)))
			  (context-sexp `(,(pvs-context-version context)
					   ,(pvs-context-libraries context)
					   ,(pvscontext-parameters context)
					   ,@(pvscontext-entries context))))
		     (let ((ce (find-if-not #'(lambda (ce)
						(file-exists-p
						 (make-specpath (ce-file ce))))
				 (pvscontext-entries context))))
		       (assert (null ce)))
		     (handler-case
			 (store-object-to-file context-sexp (context-pathname))
		       (error (condition) (pvs-message "~a" condition)))
		     (setf (pvscontext ws) context)
		     (setf (pvs-context-changed ws) nil)
		     (unless quiet?
		       (pvs-message "Context file ~a written~%"
			 (namestring (context-pathname)))))
		   (pvs-log "Context file ~a not written, do not have write permission"
			    (namestring (context-pathname)))))))
	  (t (setq *all-workspace-sessions* (remove ws *all-workspace-sessions*))
	     (pvs-message "Directory ~a has disappeared" (path ws))))))

(defvar *testing-restore* nil)

(defun write-object-files (&optional force?)
  (when (and (> (hash-table-count (current-pvs-theories)) 0)
	     (ensure-bin-subdirectory))
    (if *testing-restore*
	(maphash #'(lambda (file theories)
		     (declare (ignore file))
		     (dolist (theory (cdr theories))
		       (write-object-file theory force?)
		       (let ((bp (make-binpath theory))
			     (ce (get-context-file-entry theory)))
			 (when (typechecked? theory)
			   (assert ce () "write-object-files - ~a missing ce?" (id theory))
			   (assert (file-exists-p bp) ()
				   "write-object-files - ~a doesn't exist" (id theory))
			   (assert (ce-object-date ce) ()
				   "write-object-files - ~a object date missing" (id theory))
			   (assert (assoc (id theory) (ce-object-date ce) :test #'string=) ()
				   "write-object-files - ~a theory not in ce" (id theory))
			   (assert (= (file-write-time bp)
				      (cdr (assoc (id theory) (ce-object-date ce)
						  :test #'string=)))
				   () "write-object-files - date mismatch in ~a" bp)))))
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
	  (setf (current-pvs-context-changed) t))))))

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
  (setf (pvscontext *workspace-session*) (make-pvs-context))
  (assert (memq *workspace-session* *all-workspace-sessions*))
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)) ()
	  "update-pvs-context dups")
  t)
    

(defun make-pvs-context (&optional (ws *workspace-session*))
  "Returns a pvscontext instance representing the .pvscontext file.
pvscontext is a struct with slots:
  :pvs-version,
  :prelude-libs,
  :properties,
  :entries
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
    (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)) ()
	    "make-pvs-context: entry dups")
    (let ((pvs-ctx (current-pvs-context)))
      ;; Collect from (current-pvs-files
      (let ((ce (find-if-not #'(lambda (ce)
				 (file-exists-p
				  (make-specpath (ce-file ce))))
		  (pvscontext-entries pvs-ctx))))
	(assert (null ce) () (format t "make-pvs-context")))
      (maphash #'(lambda (name info)
		   (declare (ignore info))
		   (let ((ce (create-context-entry name)))
		     (when ce
		       (pushnew ce (pvscontext-entries pvs-ctx)
				:key #'ce-file :test #'string=))))
	       (current-pvs-files))
      (assert (not (duplicates? (pvs-context-entries pvs-ctx) :key #'ce-file)) ()
	      "make-pvs-context: after pvs-files")
      pvs-ctx)))

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

(defun update-context (fname)
  "Updates the pvs-context of the given fname, diffs is a list of theory
differences that were changed rather than replaced; i.e., they are partially
typechecked. If t, then all theories are new (or forced typechecking), nil
means only lexical differences were found, anything else is at the
theory-element level."
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)) ()
	  "update-context - dups")
  (assert (file-exists-p (make-specpath fname)))
  (let* ((filename (pvs-filename fname))
	 (oce (get-context-file-entry filename))
	 (nce (create-context-entry filename)))
    (assert (string= (ce-file nce) filename))
    (unless (and (not (current-pvs-context-changed))
		 oce
		 (equal (ce-write-date oce)
			(file-write-time (make-specpath filename)))
		 (ce-eq oce nce))
      ;; Make changes as needed
      (when (and oce (ce-object-date oce) (check-binfiles fname))
	;; (format t "~%update-context: copy ~a to nce ~a"
	;;   (ce-object-date oce) nce)
	(setf (ce-object-date nce) (ce-object-date oce)))
      (let* ((nctx-entries (cons nce (remove oce (pvs-context-entries))))
	     (rctx-entries (remove-missing-file-references nctx-entries)))
	(assert (not (duplicates? rctx-entries :key #'ce-file)) ()
		"update-context: duplicate context entries")
	(setf (pvscontext-entries (current-pvs-context)) rctx-entries)
	;; (write-context nil nil t)
	(setf (current-pvs-context-changed) t)))))

(defun remove-missing-file-references (ctx-entries &optional exist-entries)
  (if (null ctx-entries)
      (nreverse exist-entries)
      (let ((ce (car ctx-entries)))
	(cond ((file-exists-p (make-specpath (ce-file ce)))
	       (remove-missing-file-references (cdr ctx-entries) (cons ce exist-entries)))
	      (t (pvs-message "Removing ~a from .pvscontext for missing file"
		   (ce-file ce))
		 (remove-context-entry-deps (ce-file ce))
		 (remove-missing-file-references (cdr ctx-entries) exist-entries))))))
  

(defun delete-file-from-workspace (filename)
  (assert (not (duplicates? (pvs-context-entries) :key #'ce-file)) ()
	  "delete-file-from-workspace: dups")
  (let ((prf-file (make-prf-pathname filename)))
    (when (uiop:file-exists-p prf-file)
      (copy-proofs-to-orphan-file filename (read-pvs-file-proofs filename))
      (delete-file prf-file)))
  (let ((ce (get-context-file-entry filename)))
    (when ce
      (remove-context-entry-deps ce)
      (setf (current-pvs-context-changed) t))))

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
    (setf (pvscontext-entries (current-pvs-context)) (remove ce (pvs-context-entries)))
    (dolist (ce2 (pvs-context-entries))
      (when (some #'(lambda (dep) (string= dep (ce-file ce))) (ce-dependencies ce2))
	(remove-context-entry-deps ce2)))))

(defun update-context-proof-status (fdecl)
  (unless (or (from-prelude? fdecl)
	      (from-library? fdecl))
    (let ((fe (get-context-formula-entry fdecl)))
      (if fe
	  (let ((status (proof-status-symbol fdecl)))
	    (unless (string-equal (fe-status fe) status)
	      (setf (fe-status fe) (format nil "~(~a~)" status))
	      (setf (current-pvs-context-changed) t))
	    (assert (member (fe-status fe)
			    '("proved-complete" "proved-incomplete" "unchecked"
			      "unfinished" "untried" nil)
			    :test #'string-equal)))
	  (setf (current-pvs-context-changed) t)))))

(defun create-context-entry (pathname)
  (let ((specpath (make-specpath pathname)))
    (when (uiop:file-exists-p specpath)
      (let* ((filename (pvs-filename specpath))
	     (file-entry (get-context-file-entry filename))
	     (valid? (or *loading-prelude*
			 (and file-entry (valid-context-entry file-entry))))
	     (date&theories (gethash filename (current-pvs-files)))
	     (cur-theories (cdr date&theories))
	     (ce-theories (and file-entry (ce-theories file-entry)))
	     (te-entries
	      (if cur-theories
		  (mapcar #'(lambda (th)
			      (let ((te (when file-entry
					  (get-context-theory-entry th file-entry))))
				;; Note that th is the just parsed theory
				;;           te is the previous theory-entry, or nil
				(create-theory-entry th te valid?)))
		    cur-theories)
		  ce-theories))
	     (prf-file (make-prf-pathname filename))
	     (proofs-write-date (when (uiop:file-exists-p prf-file)
				  (file-write-date prf-file)))
	     (fdeps (file-dependencies filename))
	     (objdate (when (and file-entry (check-binfiles filename))
			(ce-object-date file-entry)))
	     (md5sum (md5-file (make-specpath filename))))
	(assert cur-theories)
	(assert (plusp md5sum))
	;;(format t "~%create-context-entry: ~a objdate = ~a" filename objdate)
	(make-context-entry
	 :file filename
	 :write-date (car date&theories)
	 :object-date objdate
	 :extension nil
	 :proofs-date proofs-write-date
	 :dependencies fdeps
	 :theories te-entries
	 :md5sum md5sum)))))

(defun md5-file (file)
  (let ((digest (sb-md5:md5sum-file file))
	(sum 0))
    (loop for x across digest
	  do (setq sum (+ (* sum 256) x)))
    sum))
  

(defun create-theory-entry (theory th-entry valid?)
  ;; Note that theory is the just parsed theory
  ;;           th-entry is the previous theory-entry, or nil
  ;; Need to create formula-info entries. Problem is that new theory
  ;; isn't typechecked, and hence TCCs for the part of the theory that
  ;; follows the first difference have not been generated, so they will
  ;; not have their status updated
  (let* ((finfo (mapcar #'(lambda (d)
			    (create-formula-entry
			     d (when th-entry (te-formula-info th-entry)) valid?))
			(remove-if-not #'(lambda (d)
					   (provable-formula? d))
			  (append (assuming theory) (theory theory)))))
	 (all-finfo (append finfo
			    (unless (memq 'typechecked (status theory))
			      (hidden-formula-entries th-entry finfo valid?))))
	 (status (if (and valid? th-entry)
		     (te-status th-entry)
		     (status theory))))
    (assert (every #'formula-entry-p all-finfo))
    (make-theory-entry
     :id (string (id theory))
     :status (if (generated-by theory)
		 (cons 'generated status)
		 status)
     :dependencies (get-binfile-dependencies theory)
     :formula-info all-finfo)))

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
				 (nm (format nil "~a" (lcopy inm 'library nil))))
			     (if libentry
				 (nconc libentry (list nm))
				 (setq libalist
				       (nconc libalist
					      (list (list lib-path nm))))))
			   (push (format nil "~a" (lcopy inm 'library nil))
				 thlist)))
	      (let ((result (nconc libalist
				   (when thlist (list (cons nil (nreverse thlist)))))))
		(assert (every #'(lambda (elt)
				   (and (listp elt)
					(and (typep (car elt) '(or null pathname))
					     (every #'stringp (cdr elt)))))
			       result))
		result)))))
      (let ((te (get-context-theory-entry (id theory))))
	(when te
	  (te-dependencies te)))))


(defmethod lib-datatype-or-theory? ((mod datatype-or-module))
  (assert (context-path mod))
  (and (not (from-prelude? mod))
       (let ((cur-path (if (current-context)
			   (context-path (current-theory))
			   (current-context-path))))
	 (not (file-equal (context-path mod) cur-path)))))

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
				      (let* ((mi (copy (car i-names)
						   :id (id ath)
						   :resolutions nil))
					     (res (mk-resolution ath mi nil)))
					(setf (resolutions mi) (list res))
					mi))
			    aths)))
	     (nconc (nreverse (nconc anames (list (car i-names)))) imp-names))
	   (cons (car i-names) imp-names)))))
  

(defun create-formula-entry (decl fentries valid?)
  (let* ((fentry (find-if #'(lambda (fe)
			      (and (typep fe 'formula-entry)
				   (string= (fe-id fe) (id decl))))
		   fentries))
	 (status (cond ((typechecked? decl)
			(format nil "~(~a~)" (proof-status-symbol decl)))
		       (fentry
			(if valid?
			    (fe-status fentry)
			    (cond ((member (fe-status fentry)
					   '("proved-complete" "proved-incomplete" "proved"
					     "unchecked")
					   :test #'string-equal)
				   "unchecked")
				  ((string-equal (fe-status fentry) "unfinished")
				   "unfinished")
				  (t "untried"))))
		       (t "untried"))))
    (make-formula-entry
     :id (string (id decl))
     :status status
     :decision-procedure-used (cond ((typechecked? decl)
				     (format nil "~(~a~)" (decision-procedure-used decl)))
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
						    (string= id (feid ffi)))
						finfo)))
			(te-formula-info tentry))))
	(unless valid?
	  (mapc #'(lambda (fe)
		    (if (consp fe)
			(if (eq (cadr fe) t) (setf (cadr fe) "unchecked"))
			(when (string-equal (fe-status fe) "proved")
			  (break "hidden-formula-entries")
			  (setf (fe-status fe) "unchecked"))))
		fentries))
	fentries))))
  

(defun create-declaration-entry (decl)
  ;; (unless (or (from-prelude? decl)
  ;; 	      (get-library-id decl))
  ;;   (break "create-declaration-entry"))
  (make-declaration-entry
   :id (string (id decl))
   :class (format nil "~(~a~)" (type-of decl))
   :type (when (and (typed-declaration? decl)
		    (not (typep decl 'formal-type-decl)))
	   (or (declared-type-string decl)
	       (setf (declared-type-string decl)
		     (unparse (declared-type decl) :string t))))
   :theory-id (when (module decl) (string (id (module decl))))
   :library (let ((libid (get-library-id decl)))
	      (when libid (string libid)))))


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
  (let* ((fname (pvs-filename filename))
	 (theories (cdr (gethash fname (current-pvs-files)))))
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
  (let* ((fname (pvs-filename filename))
	 (deps (assoc fname *circular-file-dependencies* :test #'equal)))
    (if deps
	(cdr deps)
	(let ((cdeps (circular-file-dependencies*
		      (cdr (gethash fname (current-pvs-files))))))
	  (push (cons fname (car cdeps)) *circular-file-dependencies*)
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
		   (unless (or (member tid uchain :test #'string=)
			       (member tid seen :test #'string=))
		     (let ((te (get-context-theory-entry tid)))
		       (if (or (null te)
			       (member "generated" (te-status te) :test #'string=))
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
    (if (gethash (pvs-filename filename) (current-pvs-files))
	;; Things have been parsed, we can use that information
	(let ((*file-dependencies* nil))
	  (get-pvs-file-dependencies* filename)
	  *file-dependencies*)
	;; Not even parsed - must go by the .pvscontext information
	(cons filename (file-dependencies filename)))))

(defun get-pvs-file-dependencies* (fname)
  (let ((filename (pvs-filename fname)))
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
			(pvs-message "~a not available" importing)))))))))))

(defun get-theory-dependencies (theoryid)
  (let ((te (get-context-theory-entry theoryid)))
    (when te
      (te-dependencies te))))

;;; Restore context
;;; Reads in the .pvscontext file to the (current-pvs-context) variable.  Most of
;;; the rest of this function provides for reading previous versions of the
;;; .pvscontext

(defun restore-context (&optional (ws *workspace-session*))
  #+pvsdebug (consistent-workspace-paths)
  (assert ws)
  (let* ((*workspace-session* ws)
	 (ctx-file (merge-pathnames *context-name* (path ws))))
    (if (uiop:file-exists-p ctx-file)
	(let ((ctx-file-date (file-write-date ctx-file)))
	  (handler-case
	      (unless (and (pvscontext ws)
			   (= ctx-file-date (pvs-context-date ws)))
		(let ((context (read-context-file ctx-file)))
		  (setf (pvs-context-date ws) ctx-file-date)
		  (setf (pvscontext ws) context)
		  ;; (setf (pvscontext-entries context)
		  ;; 	(remove-duplicates (pvscontext-entries context)
		  ;; 	  :key #'ce-file :test #'equal :from-end t))
		  ;; (setf (pvscontext-entries context)
		  ;; 	(delete-if-not #'(lambda (ce)
		  ;; 			   (file-exists-p (make-specpath (ce-file ce))))
		  ;; 	  (pvscontext-entries context)))
		  ))
	    (file-error (err)
	      (pvs-message "PVS context problem - resetting")
	      (pvs-log "  ~a" err)
	      (setf (pvscontext ws) (initial-context))
	      (write-context))))
	(setf (pvscontext ws) (initial-context))))
  nil)

(defun read-context-file (ctx-file)
  (let* ((*default-pathname-defaults* ;; for calls to make-specpath
	  (asdf/pathname:pathname-directory-pathname ctx-file))
	 (context-sexp (if (with-open-file (in ctx-file)
			     (and (char= (read-char in) #\()
				  (char= (read-char in) #\")))
			   (with-open-file (in ctx-file) (read in))
			   (if *testing-restore*
			       (fetch-object-from-file ctx-file)
			       (ignore-errors (fetch-object-from-file ctx-file)))))
	 (context (if context-sexp
		      (make-pvscontext
		       :pvs-version (car context-sexp)
		       :prelude-libs (cadr context-sexp)
		       :parameters (caddr context-sexp)
		       :entries (cdddr context-sexp))
		      (initial-context))))
    (assert (uiop:directory-exists-p *default-pathname-defaults*))
    (setf (pvscontext-entries context)
	  (remove-duplicates (pvscontext-entries context)
	    :key #'ce-file :test #'equal :from-end t))
    (setf (pvscontext-entries context)
	  (delete-if-not #'(lambda (ce)
			     (file-exists-p (make-specpath (ce-file ce))))
	    (pvscontext-entries context)))
    (dolist (ce (pvscontext-entries context))
      (let ((ndeps (remove-if-not #'(lambda (dep)
				      (let ((spos (position #\/ dep :from-end t)))
					(if spos
					    (let ((dir (subseq dep 0 spos))
						  (file (subseq dep (1+ spos))))
					      (with-workspace dir
						(file-exists-p (make-specpath file))))
					    (file-exists-p (make-specpath dep)))))
		     (ce-dependencies ce))))
	(unless (equal ndeps (ce-dependencies ce))
	  (pvs-message "PVS context has bad deps: ~a"
	    (remove-if #'(lambda (dep) (file-exists-p (make-specpath dep)))
	      (ce-dependencies ce)))
	  (setf (ce-dependencies ce) nil)
	  (setf (ce-object-date ce) nil)
	  (setf (ce-theories ce) nil))))
    ;;(same-major-version-number (car context) *pvs-version*)
    ;; Hopefully we are backward compatible between versions
    ;; 3 and 4.
    (assert (every #'(lambda (ce)
		       (file-exists-p (make-specpath (ce-file ce))))
		   (pvscontext-entries context)))
    (assert (not (duplicates? (pvs-context-entries context) :key #'ce-file)) ()
	    "read-context-file: dups")
    (load-prelude-libraries (pvscontext-prelude-libs context))
    (setq *default-decision-procedure*
	  (or (getf (pvscontext-parameters context) :default-decision-procedure)
	      "shostak"))
    (dolist (ce (pvscontext-entries context))
      (unless (listp (ce-object-date ce))
	(setf (ce-object-date ce) nil)))
    (assert (not (duplicates? (pvs-context-entries context) :key #'ce-file)) ()
	    "read-context-file: dups 2")
    (assert (every #'context-entry-p (pvs-context-entries context)))
    context))

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
;;;         restore-proofs ->
;;;           restore-proofs ->
;;;             restore-theory-proofs
(defun restore-theories (fname)
  "Restores theories from pvsbin/ files"
  (let* ((*theories-restored* nil)
	 (*adt-type-name-pending* nil)
	 (filename (pvs-filename fname)))
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
  (let ((thid (if (symbolp thname) thname (intern thname :pvs))))
    (unless (gethash thid (current-pvs-theories))
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
	      (setf (gethash (pvs-filename (filename theory)) (current-pvs-files))
		    (cons (file-write-time (make-specpath (filename theory)))
			  theories)))
	    (dolist (th theories)
	      (when (memq th *theories-restored*)
		(update-restored-theories th)
		;;(assert (or (generated-by th) (typechecked? th)))
		))))))))

;;; Invoked after a bin file has been restored.
(defmethod update-restored-theories ((theory module))
  (setf (formals-sans-usings theory)
	(remove-if #'importing-param? (formals theory)))
  ;;(generate-xref theory)
  ;;(reset-restored-types theory)
  ;; (unless (valid-proofs-file (filename theory))
  ;;   ;; If proofs file is valid, wait for typecheck-theories to call this,
  ;;   ;; so TCCs can get their proofs
  ;;   (let ((*current-context* (saved-context theory)))
  ;;     (assert *current-context*)
  ;;     (restore-proofs (filename theory) (list theory))))
  (assert (saved-context theory)))

(defmethod update-restored-theories ((adt recursive-type))
  (setf (formals-sans-usings adt)
	(remove-if #'importing-param? (formals adt)))
  (let* ((adt-theories (adt-generated-theories adt))
	 (adt-file (filename (car adt-theories))))
    (assert adt-file)
    (setf (gethash (pvs-filename adt-file) (current-pvs-files))
	  (cons (file-write-time (make-specpath adt-file)) adt-theories))
    (mapc #'update-restored-theories adt-theories)))


(defun pvs-rename-file (file new-name)
  (break "Need to move proofs to orphaned-proofs file")
  (ignore-file-errors (rename-file file new-name)))



;;; For a given filename, valid-context-entry returns two values: the
;;; first is a boolean indicating whether the entry is valid, and the
;;; second is the entry itself.

;;; The entry is valid if:

;;;  1. The ce-write-date of the entry matches the file-write-time of
;;;     the corresponding pvs-file
;;;  2. Every dependent file has a valid context entry 

(defmethod valid-context-entry (filename)
  (let ((entry (get-context-file-entry filename)))
    (and entry
	 (values (valid-context-entry entry) entry))))

(defmethod valid-context-entry ((entry context-entry))
  (if *valid-entries*
      (clrhash *valid-entries*)
      (setq *valid-entries* (make-hash-table :test #'equal)))
  (valid-context-entry* entry))

(defun valid-context-entry* (entry)
  (multiple-value-bind (valid? there?)
      (gethash (ce-file entry) *valid-entries*)
    (if there?
	valid?
	(let ((file (make-specpath (ce-file entry))))
	  (assert (file-exists-p file))
	  ;; First set it to nil to stop recursing
	  (setf (gethash (ce-file entry) *valid-entries*) nil)
	  ;; Then set it to the real value
	  (setf (gethash (ce-file entry) *valid-entries*)
		(valid-context-entry-file entry file))))))

(defun valid-context-entry-file (entry file)
  "Checks that the given context-entry and associated file have matching
dates, then does the same for the dependencies, returning t if everything is
valid."
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
  (let ((fname (pathname-name (protect-pvs-filename filename))))
    (car (member fname (pvs-context-entries)
		 :test #'(lambda (x y) (string= x (ce-file y)))))))

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
			    (mapcar #'(lambda (te) (list (string (id te)) file))
			      (ce-theories e))))
		(pvs-context-entries))
	      #'string-lessp :key #'car)
	(let ((cfile (make-pathname :defaults context :name ".pvscontext")))
	  (when (file-exists-p cfile)
	    (multiple-value-bind (context error)
		(if (with-open-file (in cfile)
		      (and (char= (read-char in) #\()
			   (char= (read-char in) #\")))
		    (with-open-file (in cfile) (read in))
		    (if *ignore-binfile-errors*
			(ignore-errors
			  (fetch-object-from-file cfile))
			(fetch-object-from-file cfile)))
	      (cond (error
		     (pvs-message "Error reading context file ~a"
		       (namestring cfile))
		     (pvs-log (format nil "  ~a" error))
		     nil)
		    (t (sort (mapcan
			      #'(lambda (e)
				  (let ((file (sformat "~a.~a" (ce-file e)
						       (or (ce-extension e) "pvs"))))
				    (mapcar #'(lambda (te) (list (string (id te)) file))
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
      (find theoryname (ce-theories file-entry)
	    :test #'(lambda (tn e) (string= (ref-to-id tn) (te-id e))))
      (get-context-theory-entry* theoryname (pvs-context-entries))))

(defun get-context-theory-entry* (theoryname file-entries)
  (when file-entries
    (or (and (uiop:file-exists-p (make-specpath (ce-file (car file-entries))))
	     (get-context-theory-entry theoryname (car file-entries)))
	(get-context-theory-entry* theoryname (cdr file-entries)))))

(defun get-context-formula-entry (fdecl)
  (unless (from-prelude? fdecl)
    (let ((te (get-context-theory-entry (module fdecl))))
      (unless te
	(update-pvs-context)
	(setq te (get-context-theory-entry (module fdecl))))
      (when te
	(find-if #'(lambda (fe) (string= (id fdecl) (fe-id fe)))
	  (te-formula-info te))))))

(defun context-file-of (theoryref)
  (let ((thid (ref-to-id theoryref)))
    (if (member thid *prelude-theories* :key #'id)
	(if (member thid (core-prelude-theories) :key #'id)
	    "prelude" "pvsio_prelude")
	(context-file-of* thid (pvs-context-entries)))))

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
		:test #'(lambda (x y) (string= x (te-id y))))
	(values (ce-file (car entries)) (ce-extension (car entries)))
	(context-file-of* theoryid (cdr entries)))))

(defun context-entry-of (theoryref)
  (labels ((entry (id entries)
	     (when entries
	       (if (member id (ce-theories (car entries))
			   :test #'(lambda (x y) (string= x (te-id y))))
		   (car entries)
		   (entry id (cdr entries))))))
    (entry (ref-to-id theoryref) (pvs-context-entries))))

(defun context-entry-of-file (file)
  (find-if #'(lambda (ce)
	       (string= (ce-file ce) file))
    (pvs-context-entries)))


(defun get-declaration-entry-decl (de)
  (get-referenced-declaration*
   (intern (de-id de) :pvs)
   (intern (de-class de) :pvs)
   (de-type de)
   (intern (de-theory-id de) :pvs)
   (intern (de-library de) :pvs)))

(defun get-referenced-declaration (declref)
  (apply #'get-referenced-declaration* declref))

(defun get-referenced-declaration* (id class &optional type theory-id lib-ref)
  (unless (find-class class nil)
    (setq class (read-from-string (string class))))
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
				  (let* ((imps (current-sorted-importings))
					 (th (min-theory-wrt-imps theories imps)))
				    th)
				  (car theories)))))))
	      ;;(assert theory)
	      (when (and theory
			 (not (string-equal class 'skolem-const-decl)))
		(let ((decls (remove-if-not
				 #'(lambda (d)
				     (and (typep d 'declaration)
					  (eq (id d) id)
					  (eq (type-of d) class)))
			       (all-decls theory))))
		  (cond ((singleton? decls)
			 (car decls))
			((and (cdr decls) type)
			 (let ((ndecls (or (remove-if-not
					       #'(lambda (d) (string= (str (type d)) type))
					     decls)
					   (remove-if-not
					       #'(lambda (d)
						   (string= (str (declared-type d)) type))
					     decls))))
			   (cond ((singleton? ndecls)
				  (car ndecls))
				 ((null (cddr ndecls))
				  (cadr ndecls))
				 (t ;; (break "get-referenced-declaration* - multiple decls")
				  (car (last ndecls))))))
			(t (if (memq class '(subtype-judgement name-judgement
					     application-judgement expr-judgement
					     rec-application-judgement))
			       ;; These are copies of judgements created by judgements.lisp
			       ;; for instances of generic judgements - it should have kept
			       ;; the generic jugment instead of the instance, but we'll
			       ;; try and recover it anyway.
			       (let ((jdecls (remove-if-not #'judgement?
					       (get-declarations id))))
				 (if (singleton? jdecls)
				     (car jdecls)
				     (unless (null id)
				       (break "get-referenced-declaration* - no judgement"))))))))))))))
				
				

(defun min-theory-wrt-imps (theories imps &optional min)
  (cond ((null theories) (declaration (theory-name (car min))))
	((member (car theories) min :key #'(lambda (imp) (declaration (theory-name imp))))
	 (min-theory-wrt-imps (cdr theories) imps min))
	(t (let ((new-min (member (car theories) imps
				  :key #'(lambda (imp) (declaration (theory-name imp))))))
	     (assert (or (null min)
			 (member (declaration (theory-name (car min))) new-min
				 :key #'(lambda (imp) (declaration (theory-name imp))))))
	     (min-theory-wrt-imps (cdr theories) imps new-min)))))


(defun invalidate-theory-proofs (te)
  (dolist (fe (te-formula-info te))
    (when (member (fe-status fe) '("proved-complete" "proved-incomplete")
		  :test #'string-equal)
      (setf (fe-status fe) "unchecked")
      (setf (current-pvs-context-changed) t))))
  

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
  "A filename has a valid .prf file if it has a valid context-entry, the
.prf file exists, and the file write time matches the context-entry.
Note that this doesn't check if the .pvs file is the matches as well."
  (multiple-value-bind (valid? entry)
      (valid-context-entry filename)
    (and valid?
	 (let ((prf-file (make-prf-pathname filename)))
	   (if (uiop:file-exists-p prf-file)
	       (eql (file-write-time prf-file)
		    (ce-proofs-date entry))
	       (null (ce-proofs-date entry)))))))

;;; Proof handling functions - originally provided by Shankar.

;;; Should check that
;;; (save-all-proofs theory)
;;; (restore-proofs pvsfile theory)
;;; Gives the same proofs

(defun save-all-proofs (&optional theory force?)
  "The main interface to saving proofs - force? is generally for debugging,
otherwise saves only if something changed."
  (assert (or theory *current-context*))
  (unless (or *loading-prelude*
	      (and theory (from-prelude? theory)))
    (if theory
	(save-pvs-file-proofs (filename theory) force?)
	(maphash #'(lambda (file mods)
		     ;; (car mods) is the timestamp
		     (when (some #'has-proof? (cdr mods))
		       (save-proofs (make-prf-pathname file) (cdr mods) force?)))
		 (current-pvs-files)))))

(defun save-pvs-file-proofs (fileref &optional force?)
  (with-pvs-file (file) fileref
    (save-proofs (make-prf-pathname file)
		 (cdr (gethash file (current-pvs-files)))
		 force?)))

(defun has-proof? (mod)
  (some #'(lambda (d) (and (formula-decl? d) (justification d)))
	(all-decls mod)))

(defvar *save-proofs-pretty* t)

(defvar *validate-saved-proofs* t)

(defvar *number-of-proof-backups* 1)

(defun toggle-proof-prettyprinting ()
  (setq *save-proofs-pretty* (not *save-proofs-pretty*)))

(defun save-proofs (prf-file theories &optional force?)
  "Save proofs for the given theories in the given prf-file."
  (if (write-permission?)
      (let* ((file-proofs (read-pvs-file-proofs prf-file))
	     (theory-proofs (collect-theories-proofs theories)))
	#+pvsdebug
	(current-proofs-contain-old-proofs theory-proofs file-proofs theories)
	(when (or force? (not (equal file-proofs theory-proofs)))
	  (when (save-proofs-to-file prf-file theory-proofs)
	    (dolist (th theories)
	      (let ((te (get-context-theory-entry (id th))))
		(when (and te (memq 'invalid-proofs (te-status te)))
		  (setf (te-status te)
			(delete 'invalid-proofs (te-status te))))))
	    (orphan-detached-proofs prf-file file-proofs theory-proofs)
	    (pvs-log "Wrote proof file ~a" (file-namestring prf-file))
	    t)))
      (pvs-message "Do not have write permission for saving proof files")))

(defun orphan-detached-proofs (filename file-proofs theory-proofs)
  "Copies any proofs in file-proofs that are not in theory-proofs to the
orphan-proofs.prf file"
  (let ((orphs nil))
    (dolist (fprfs file-proofs)
      (let ((tprfs (assq (car fprfs) theory-proofs)))
	(if (null tprfs)
	    ;; Copy all fprfs to orphaned-proofs.prf file
	    (setq orphs (nconc orphs
			       (mapcar #'(lambda (fprf)
					   ;; Add filename and theoryid
					   `(,filename ,(car fprfs) ,@fprf))
				 (cdr fprfs))))
	    (dolist (fprf (cdr fprfs)) ;; formula proofs
	      (unless (assq (car fprf) (cdr tprfs))
		(setq orphs (nconc orphs `(,filename ,(car fprfs) ,@fprf))))))))
    (when orphs
      (add-to-orphaned-proofs-file orphs))))

(defun add-to-orphaned-proofs-file (new-orphs)
  (let* ((orphs (read-orphaned-proofs))
	 (norphs (remove-if #'(lambda (norph) (member norph orphs :test #'equal))
		   new-orphs)))
    (pvs-message "Found ~d orphan proofs; ~d already orphaned"
      (length orphs) (- (length orphs) (length norphs)))
    (when norphs
      (write-to-orphan-file (append norphs orphs)))))

(defun save-proofs-to-file (prf-file theory-proofs)
  "This replaces the contents of prf-file with theory-proofs.  It also
checks for elements of file-proofs that are not in theory-proofs; these are
sent to orphaned-proofs.prf. Should only be called from save-proofs, which
is the real API."
  (let ((backup-file (when (and (file-exists-p prf-file)
				(> *number-of-proof-backups* 0))
		       (backup-proof-file prf-file))))
    (handler-case
	(with-open-file (out prf-file :direction :output :if-exists :supersede)
	  (dolist (prf theory-proofs)
	    (write prf :stream out :length nil :level nil :readably t
		   :pretty *save-proofs-pretty*)
	    (when *save-proofs-pretty* (terpri out)))
	  (terpri out)
	  t)
      (file-error (condition)
	(when *validate-saved-proofs*
	  (invalid-proof-file prf-file theory-proofs))
	(pvs-error "Error writing out proof file ~a, ~@[backup ~a~]:~%  ~a~%"
	  prf-file backup-file condition)))))
    

(defmethod default-proof-script ((decl formula-decl))
  (script (default-proof decl)))

;; For proofs from .prf or orphaned-proofs files
(defmethod default-proof-script ((prf list))
  (let ((dprf (cond ((integerp (cadr prf)) ;; From prf file
		     (nth (cadr prf) (cddr prf)))
		    ((integerp (fourth prf)) ;; from orphaned-proof file
		     (nth (fourth prf) (cddddr prf))))))
    (fourth dprf)))
	    

(defun save-proofs-to-json (fileref)
  (with-pvs-file (filename) fileref
    (save-pvs-file-proofs filename)
    (let* ((prf-file (make-prf-pathname filename))
	   (json-path (format nil "~a/pvsjson" (working-directory)))
	   (json-prf-file (format nil "~a/~a.prf.json" json-path filename))
	   (theories-proofs (read-pvs-file-proofs filename))
	   (file-proofs `(("proof-file" . ,json-prf-file)
			  ("file-date" . ,(file-write-time prf-file))
			  ("theory-proofs" . ,theories-proofs))))
      (uiop:ensure-all-directories-exist (list json-path))
      (handler-case
	  (with-open-file (out json-prf-file :direction :output
			       :if-exists :supersede)
	    (json:encode-json file-proofs out))
	(file-error (err)
	  (pvs-message "~a" err)
	  (pvs-log "  ~a" err))))))

(defun proof-file-alist (theories-proofs)
  (mapcar #'proof-theory-alist theories-proofs))

(defun proof-theory-alist (theory-proofs)
  `(("theory" . ,(string (car theory-proofs)))
    ("decl-proofs" . ,(mapcar #'proof-decls-alist (cdr theory-proofs)))))

(defun proof-decls-alist (decl-proofs)
  `(("decl" . ,(string (car decl-proofs)))
    ("current-proof" . ,(cadr decl-proofs))
    ("proofs" . ,(mapcar #'proof-element-alist (cddr decl-proofs)))))

(defun proof-element-alist (prf)
  `(("proof-id" . ,(string (car prf)))
    ("description" . ,(cadr prf))
    ("create-date" . ,(caddr prf))
    ("script" . ,(sformat "~s" (cadddr prf)))
    ("refers-to" . ,(mapcar #'proof-decl-ref-alist (car (cddddr prf))))))

(defun proof-decl-ref-alist (decl-ref)
  `(("decl-id" . ,(string (car decl-ref)))
    ("class" . ,(string (cadr decl-ref)))
    ("type" . ,(caddr decl-ref))
    ("theory-id" . ,(string (cadddr decl-ref)))))


(defun backup-proof-file (file)
  (let ()
    (if (= *number-of-proof-backups* 1)
	(let ((bfile (make-pathname :type "prf~" :defaults file)))
	  (rename-file file bfile)
	  (pvs-log "Renamed ~a to ~a" file bfile)
	  bfile)
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
	    (pvs-log "Renamed ~a to ~a" file nfile)
	    nfile)))))

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
  (handler-case
      (let ((prfs (read input nil nil)))
	(if (null prfs)
	    (or (eq proofs t)
		(nreverse proofs))
	    (invalid-proof-file* input (when (listp proofs) (cons prfs proofs)))))
    (error (condition) (values nil condition))))


(defun collect-theories-proofs (theories)
  "Given a list of theories (usually associated with a pvs file), generates
a list of theory-proofs-sexp of the form
   ((thid (declid index prfinfo prfinfo ...) ...) ...)
The index is the 0-based index to the default proof for the decl.
Each prfinfo is of the form
   (prfid description create-date script refers-to decision-procedure-used [tcc-origin])
The tcc-origin is only for TCCs, and has the form
   (root kind expr type)"
  (let ((theory-proofs (collect-theories-proofs* theories nil)))
    theory-proofs))

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

;;; Called from typecheck-theories, typecheck-top-level-adt, and
;;; update-restored-theories (module)

;;; Proofs are of the form
;;;  (declid index prfinfo ...)
;;; where each proofinfo is of the form
;;;  (prfid description create-date script refers-to decision-procedure-used [origin])
;;; The origin is only there for TCCs, and is of the form
;;;  (root kind expr type place)

;;; Orphaned proofs need to include the file and theory:
;;;  (filename theoryid declid index prfinfo ...)

;;; Proofs were stored in files in various forms over the years
;;; Before multiple proofs, a proof for a declid was of the form
;;;  (declid script)
;;;  (declid (...) script)  ;; I don't remember what the second field had in it
;;; After multiple proofs only the prfinfo is different
;;;  (prfid description create-date run-date script status refers-to
;;;	    real-time run-time interactive? decision-procedure-used)
;;; but rerunning could change the run-date, status, real-time, run-time, and interactive?
;;; slots, causing proof files to be saved unnecessarily

;;; We still try to accomodate old .prf files, as seen below;
;;; note that older proof files will be overwritten after restoration.

(defvar *valid-proofs-file*)

;;; Libraries in <pvs-path>/lib/, for example finite_sets and bitvectors, are
;;; trusted, as the core prelude is. When the current workspace is not the
;;; library, proved? is true for each formula of the library that has a
;;; proof, and PVS does not change the proof files of the library. The
;;; stored proof status does not change, thus .bin and .pvscontext files
;;; keep the real status. In the workspace of the library itself, PVS
;;; behaves as usual.

(defvar *pvs-lib-truename* nil)

(defvar *pvs-lib-paths* (make-hash-table :test #'equal)
  "Caches pvs-lib-path?, from the namestring of a path to t or nil.")

(defun pvs-lib-path? (path)
  "True when path is <pvs-path>/lib/ or a directory in it."
  (let ((key (namestring path)))
    (multiple-value-bind (value found?)
	(gethash key *pvs-lib-paths*)
      (if found?
	  value
	  (setf (gethash key *pvs-lib-paths*)
		(let ((lib-path (or *pvs-lib-truename*
				    (setq *pvs-lib-truename*
					  (uiop:truename*
					   (merge-pathnames "lib/" *pvs-path*)))))
		      (truepath (uiop:truename* path)))
		  (and lib-path truepath
		       (uiop:subpathp truepath lib-path)
		       t)))))))

(defun trusted-library-decl? (decl)
  "True when decl has a proof, decl is in a library in <pvs-path>/lib/, and
the current workspace is not that library."
  (and *workspace-session*
       (proofs decl)
       (let ((path (and (module decl) (context-path (module decl)))))
	 (and path
	      (pvs-lib-path? path)
	      (not (file-equal path (current-context-path)))))))

(defun trusted-library-workspace? ()
  "True when the current workspace is a library in <pvs-path>/lib/ and
with-workspace entered it from another workspace, see *loading-library*."
  (and *loading-library*
       (pvs-lib-path? (current-context-path))))

(defun restore-proofs (filename &key read-only theories file-proofs)
  "Restore proofs from the corresponding .prf file - looks in
orphaned-proofs.prf for any formula that has no proofs in the prf file.  An
invariant to try for is that, within a workspace, all saved proofs are in the
union of the .prf (including orphaned-proofs.prf) files, and these are all
pairwise disjoint. Assigns proofs to formula declarations from the .prf file,
looking in orphaned proofs if not there. Unassigned proofs are added to
orphaned-proofs, and the .prf and orphaned-proofs.prf are backed up and replaced
if either has changed, unless :read-only is t.

Note that the proof status is set from the .pvscontext, since it also keeps the
date of the .prf file for validation."
  (let* ((ftheories (or theories (get-theories filename)))
	 (*valid-proofs-file* (valid-proofs-file filename))
	 (read-only (or read-only (trusted-library-workspace?)))
	 (fproofs (or file-proofs (read-pvs-file-proofs filename))))
    (when fproofs
      (let* ((orphs (get-orphaned-proofs))
	     (new-orphs (restore-proofs* filename ftheories fproofs orphs)))
	(unless (or read-only
		    (null new-orphs))
	  (pvs-message "Moving ~d proof(s) from ~a to orphaned-proofs.prf file"
	    (length new-orphs) filename)
	  (write-to-orphan-file (append new-orphs orphs)))
	(let ((restored-proofs (collect-theories-proofs ftheories)))
	  (unless (or read-only
		      (tree-equal fproofs restored-proofs :test #'equalp))
	    (if new-orphs
		(pvs-message "Overwriting proof file to remove orphaned proofs")
		(pvs-message "Overwriting proof file for new TCC info"))
	    (write-pvs-file-proofs filename restored-proofs)))))))

(defun restore-proofs* (filename theories proofs orphs &optional new-orphs)
  (if (null theories)
      new-orphs
      (let* ((theory (car theories))
	     (*current-context* (context theory))
	     (tproofs (assq (id theory) proofs)))
	(if tproofs
	    (let ((nproofs (remove tproofs proofs))
		  (norphs (restore-theory-proofs filename theory tproofs orphs)))
	      (restore-proofs* filename (cdr theories) nproofs orphs
			       (nconc new-orphs norphs)))
	    (restore-proofs* filename (cdr theories) proofs orphs new-orphs)))))

(defun restore-theory-proofs (filename theory proofs orphs)
  "Restores proofs to all formula-decls in theory, looking in orphs for any that
are missing, and sets the proof status from the theory-entry of .pvscontext for
proof status."
  (assert (eq (id theory) (car proofs)))
  (assert (every #'(lambda (prf) (and (listp prf) (integerp (cadr prf)))) (cdr proofs)))
  (let ((rem-proofs (restore-decls-proofs (all-decls theory) (cdr proofs) orphs)))
    (mapcar #'(lambda (rprf) `(,filename ,(id theory) ,@rprf)) rem-proofs)))

(defun restore-decls-proofs (decls proofs orphs)
  (if (null decls)
      proofs
      (let ((rem-proofs (restore-decl-proofs (car decls) proofs orphs)))
	(restore-decls-proofs (cdr decls) rem-proofs orphs))))

(defmethod restore-decl-proofs ((decl formula-decl) proofs orphs)
  "Sets proofs and default-proof in decl, and gets the status from .pvscontext.
If an associated proof is found in proofs, returns proofs with proof removed.
Otherwise looks for a corresponding proof in orphs, and returns proofs unchanged."
  (let ((prf-entry (assq (id decl) proofs))
	(fe (when *valid-proofs-file*
	      (get-context-formula-entry decl)))) ;; Used to set the status
    (cond (prf-entry
	   (let ((dproofs (make-proof-infos-from-sexp decl prf-entry)))
	     (setf (proofs decl) dproofs))
	   (setf (default-proof decl)
		 (nth (cadr prf-entry) (proofs decl)))
	   (if *loading-prelude*
	       (setf (status (default-proof decl)) 'proved)
	       (when fe
		 (setf (status (default-proof decl))
		       (fe-status-to-proof-status (fe-status fe)))))
	   (remove prf-entry proofs))
	  ((let ((oprf (find-best-matching-orphan decl orphs)))
	     (when oprf
	       (let ((dproofs (make-proof-infos-from-sexp decl oprf)))
		 (setf (proofs decl) dproofs)
		 (setf (default-proof decl) (nth (cadr oprf) (proofs decl)))
		 proofs))))
	  (t ;; Decl has no associated proof in file
	   ;;(pvs-warning "Declaration ~a.~a has no proof" (id (module decl) (id decl) ))
	   proofs))))

(defmethod restore-decl-proofs ((tcc tcc-decl) proofs orphs)
  "Restores proofs to TCCs, by trying to associate them based on origin.
tccs is a list of tcc-decls, and proofs are proofs with the same origin root.
Note that the lists might not be the same length."
  (let* ((tcc-origin (origin tcc))
	 (prf-entry1 (find-if #'(lambda (pentry)
				  (let* ((prf (nth (cadr pentry) (cddr pentry)))
					 (prf-origin (seventh prf)))
				    (if prf-origin
					(and (string= (root tcc-origin) (car prf-origin))
					     (string-equal (kind tcc-origin) (cadr prf-origin))
					     (string= (expr tcc-origin) (caddr prf-origin)))
					(string= (car pentry) (id tcc)))))
		       proofs))
	 (prf-entry2 (or prf-entry1
			 (find-if #'(lambda (pentry)
				      (let* ((prf (nth (cadr pentry) (cddr pentry)))
					     (prf-origin (seventh prf)))
					(when prf-origin
					  (and (string= (root tcc-origin) (car prf-origin))
					       (string-equal (kind tcc-origin)
							     (cadr prf-origin))))))
			   proofs)))
	 (prf-entry (or prf-entry2
			(find-if #'(lambda (pentry)
				     (string= (car pentry) (id tcc)))
			  proofs)))
	 (fe (when *valid-proofs-file* (get-context-formula-entry tcc))))
    (cond (prf-entry
	   (let ((tproofs (make-proof-infos-from-sexp tcc prf-entry)))
	     (setf (proofs tcc) tproofs))
	   (setf (default-proof tcc)
		 (nth (cadr prf-entry) (proofs tcc)))
	   (if *loading-prelude*
	       (setf (status (default-proof tcc)) 'proved)
	       (when (and fe
			  (eq (car prf-entry) (id tcc)))
		 (setf (status (default-proof tcc))
		       (fe-status-to-proof-status (fe-status fe)))))
	   (remove prf-entry proofs))
	  ((let ((orph (find-best-matching-orphan tcc orphs)))
	     (when orph
	       (let ((dproofs (make-proof-infos-from-sexp tcc orph)))
		 (setf (proofs tcc) dproofs))
	       (setf (default-proof tcc)
		     (nth (cadddr orph) (proofs tcc)))
	       proofs)))
	  (t ;; TCC has no associated proof available
	   (pvs-warning "TCC ~a.~a has no proof" (id (module tcc)) (id tcc))
	   proofs))))

(defmethod restore-decl-proofs (decl proofs orphs)
  proofs)

(defmethod find-best-matching-orphan ((decl formula-decl) orphs)
  (let* ((file (filename (module decl)))
	 (thid (id (module decl)))
	 (fmid (id decl))
	 ;; Orphan files came in different forms in the past; we use string-equalp
	 ;; rather than test that the form is correct
	 (orph (or (find-if #'(lambda (orph) (and (string-equalp (car orph) file)
						  (string-equalp (cadr orph) thid)
						  (string-equalp (caddr orph) fmid)))
		     orphs)
		   (find-if #'(lambda (orph) (and (string-equalp (cadr orph) thid)
						  (string-equalp (caddr orph) fmid)))
		     orphs)
		   (find-if #'(lambda (orph) (string-equalp (caddr orph) fmid))
		     orphs))))
    (when orph (cddr orph))))

(defmethod find-best-matching-orphan ((decl tcc-decl) orphs)
  "Tries to find the 'best' match from orphaned proofs. Each orph is a list of
the form (file thid fmid index proofs), where each proof is a list of the form
(proofid description create-date script refers-to dp origin), where the origin
is there only for TCCs, and has the form (root kind trigger type place); older
proofs may not have the origin. The tcc-decl has an origin slot with a
tcc-origin instance with the same data.
First we collect all orphs that have origins with the same kind and expr."
  (let* ((dorig (origin decl))
	 (orphs1 (remove-if-not #'(lambda (orph)
				    (let* ((index (fourth orph))
					   (prf (nth index (cddddr orph)))
					   (orig (seventh prf)))
				      (and orig
					   (string-equal (kind dorig) (second orig))
					   (string= (expr dorig) (third orig)))))
		   orphs))
	 (orphs2 (cond ((null orphs1)
			(remove-duplicates 
			    (remove-if-not #'(lambda (orph) (eq (third orph) (id decl)))
			      orphs)
			  :test #'(lambda (x y) (tree-equal x y :test #'equalp))))
		       ((cdr orphs1)
			(or (remove-if-not
				#'(lambda (orph)
				    (let* ((index (fourth orph))
					   (prf (nth index (cddddr orph)))
					   (orig (seventh prf)))
				      (string-equal (type dorig) (fourth orig))))
			      orphs1)
			    orphs1))
		       (t orphs1)))
	 (orphs3 (if (cdr orphs2)
		     (or (remove-if-not
			     #'(lambda (orph)
				 (let* ((index (fourth orph))
					(prf (nth index (cddddr orph)))
					(orig (seventh prf)))
				   (string-equal (root dorig) (first orig))))
			   orphs2)
			 orphs2)
		     orphs2)))
    (car orphs3)))

(defun has-no-real-proof (fdecl)
  (let ((prinfo (default-proof fdecl)))
    (or (null (script prinfo))
	(equal (script prinfo) '("" (postpone) nil nil))
	(and (tcc-decl? fdecl)
	     (let ((tcc-strat (tcc-strategy fdecl)))
	       (or (equal (script prinfo) tcc-strat)
		   (equal (script prinfo) (append tcc-strat '(nil nil)))))))))
	 

(defun fe-status-to-proof-status (fe-status)
  (if (stringp fe-status)
      (if (member fe-status '("proved-complete" "proved-incomplete" "proved") :test #'string=)
	  'proved
	  (intern (string-upcase fe-status) :pvs))
      fe-status))

(defmethod make-proof-infos-from-sexp ((decl tcc-decl) prf-entry)
  ;; (decl index proofs) - from proof file
  ;; (file theory decl index proofs) - from orphaned-proofs file
  (mapcar #'(lambda (prf)
	      (assert (or (= (length prf) 6)
			  (= (length prf) 7)))
	      (let ((prinfo (apply #'mk-tcc-proof-info prf)))
		(setf (origin prinfo) (origin decl))
		prinfo))
    ;; Distinguish by index location
    (if (integerp (cadr prf-entry))
	(cddr prf-entry)
	(cddddr prf-entry))))
  
(defmethod make-proof-infos-from-sexp ((decl formula-decl) prf-entry)
  ;; (decl index proofs)
  (with-current-decl decl
    (mapcar #'(lambda (prf)
		(assert (or (= (length prf) 6)
			    (= (length prf) 7)))
		(apply #'mk-proof-info prf))
      (if (integerp (cadr prf-entry))
	(cddr prf-entry)
	(cddddr prf-entry)))))

;; (defun convert-proof-form-to-lowercase (proof-form)
;;   (cond ((and proof-form (symbolp proof-form))
;; 	 (intern (string-downcase proof-form) (symbol-package proof-form)))
;; 	((consp proof-form)
;; 	 (cons (convert-proof-form-to-lowercase (car proof-form))
;; 	       (convert-proof-form-to-lowercase (cdr proof-form))))
;; 	(t proof-form)))

(defvar *pvs-class-names* nil)

(defun pvs-class-names ()
  (unless *pvs-class-names*
    (all-subclasses (find-class 'syntax)))
  *pvs-class-names*)

(defun all-subclasses (class)
  (let ((cname (class-name class)))
    (unless (memq cname *pvs-class-names*)
      (push cname *pvs-class-names*)
      (let ((subclasses #+sbcl (sb-mop:class-direct-subclasses class)
			#-sbcl (mop:class-direct-subclasses class)))
	(dolist (subclass subclasses)
	  (all-subclasses subclass))))))

;; (defun convert-refersto-to-lowercase (refers-to)
;;   ;; refers-to is a list of lists of the form
;;   ;; ((decl-id class type theory-id) ...)
;;   (mapcar #'convert-refersto-to-lowercase* refers-to))

(defun filter-nil-form (obj)
  ;; Removes forms of nil resulting from different Common Lisps writing of nil
  ;; string-equal allows strings or symbols, and is case-insensitive
  (unless (and (symbolp obj)
	       (string-equal obj "nil"))
    obj))

;; (defun convert-refersto-to-lowercase* (ref)
;;   (when (filter-nil-form ref) ;; Don't allow nil in any form for ref
;;     ;; This is because orphan files could be written by different Common Lisps
;;     (let ((id (car ref))		; could check it exists
;; 	  (class (if (find-class (cadr ref) nil)
;; 		     (cadr ref)
;; 		     (or (find-if #'(lambda (x) (string-equal x (cadr ref)))
;; 			   (pvs-class-names))
;; 			 (break "CLASS not found, proof file probably corrupt"))))
;; 	  (type (filter-nil-form (caddr ref))) ; a string
;; 	  (theory-id (filter-nil-form (cadddr ref))))
;;       (list id class type theory-id))))


(defun copy-proofs-to-orphan-file (filename proofs)
  "Typechecking tries to assign proofs in each .prf file to the
corresponding formula declarations, but sometimes proofs are left over (decl
renaming, etc.) which are then added to the orphaned-proofs.prf file"
  ;; Now proofs is a list of the form ((thid ...) (thid ...) ...)
  (when (and proofs
	     (or *loading-prelude*
		 (write-permission?)))
    (let ((oproofs (get-orphaned-proofs))
	  (count 0))
      ;; Now we copy new proofs into oproofs entries
      (handler-case 
	  (dolist (th-prf proofs)
	    (let ((th-id (car th-prf)))
	      (dolist (decl-proofs (cdr th-prf))
		(let ((dproofs (cddr decl-proofs))) ;; Formulas have multiple proofs
		  (dolist (dprf dproofs)
		    (let ((odprf `(,filename ,th-id ,@dprf)))
		      (unless (member odprf oproofs
				      :test #'(lambda (odpr opr)
						(tree-equal odpr opr :test #'tree-equal)))
			(incf count)
			(push odprf oproofs))))))))
	;; If there's an error, it's probably an old orphaned-proof.prf file
	;; Copy orphaned-proof.prf to a bak file, and set oproofs to just the proofs
	(error ()
	  (pvs-message "Error in file ~a:~%  probably old, copied to .bak just in case"
	    (truename "orphaned-proofs.prf"))
	  (uiop:copy-file "orphaned-proofs.prf" "orphaned-proofs.bak")
	  (setq oproofs nil)
	  (setq count 0)
	  (dolist (th-prf proofs)
	    (let ((th-id (car th-prf)))
	      (dolist (decl-proofs (cdr th-prf))
		(let ((decl-id (car decl-proofs))
		      (dproofs (cddr decl-proofs)))
		  (dolist (dprf dproofs)
		    (incf count)
		    (let ((oprf (cons filename (cons th-id (cons decl-id (fourth dprf))))))
		      (push oprf oproofs)))))))))
      (if *proof-file-debug*
	  (write-to-orphan-file oproofs)
	  (handler-case
	      (write-to-orphan-file oproofs)
	    (file-error (err) (pvs-error "~a" err))))
      (unless (zerop count)
	(pvs-message
	    "Added ~d proof~:p from file ~a.prf to orphaned-proofs.prf"
	  count filename)))))

;; (defun same-orphaned-proofs (filename th-id decl-id dprf oprf)
;;   ;; orphaned proofs include the filename, th-id, and decl-id, whereas proofs
;;   ;; in the .prf files implicitely have the filename (e.g., come from filename.prf),
;;   ;; and inside have a theory-id consed onto the list of decl proofs
;;   (and (string= filename (car oprf))
;;        (eq th-id (cadr oprf))
;;        (eq decl-id (caddr oprf))
;;        (let* ((dscript (fourth dprf))
;; 	      (oscript (cdddr oprf)))
;; 	 (equalp dscript oscript))))

(defun write-to-orphan-file (oproofs)
  (with-open-file (orph "orphaned-proofs.prf"
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (dolist (oprf oproofs)
      (write oprf :stream orph
	     :length nil :level nil :escape t :pretty nil))))

;; (defun file-proofs-equal (proofs1 proofs2)
;;   ;; lists of theory-proofs
;;   (every #'theory-proofs-equal proofs1 proofs2))

;; (defun theory-proofs-equal (proofs1 proofs2)
;;   ;; theory id followed by decl-proofs
;;   (and (eq (car proofs1) (car proofs2))
;;        (every #'decl-proofs-equal (cdr proofs1) (cdr proofs2))))
  
;; (defun decl-proofs-equal (proof1 proof2)
;;   (or (and (eq (car proof1) (car proof2)) ; formula id
;; 	   (eql (cadr proof1) (cadr proof2)) ; index
;; 	   (every #'proofs-equal (cddr proof1) (cddr proof2)))
;;       (break "decl-proofs-equal")))

;; (defun proofs-equal (proof1 proof2)
;;   (or (and (eq (car proof1) (car proof2))	       ; proof id
;; 	   (equalp (cadr proof1) (cadr proof2))	       ; description
;; 	   (eql (caddr proof1) (caddr proof2))	       ;; create-date
;; 	   (scripts-equal (fourth proof1) (fourth proof2)) ; script 
;; 	   (refers-to-equal (fifth proof1) (fifth proof2)) ;; refers-to
;; 	   (equalp (sixth proof1) (sixth proof2)) ;; decision-procedure-used
;; 	   (tcc-origin-equal (seventh proof1) (seventh proof2)) ;; TCC origin
;; 	   )
;;       (break "proofs-equal")))

;; (defun scripts-equal (scr1 scr2)
;;   (or (tree-equal scr1 scr2 :test #'equalp)
;;       (break "scripts-equal")))

;; (defun refers-to-equal (refs1 refs2)
;;   (or (tree-equal refs1 refs2 :test #'equalp)
;;       (break "refers-to-equal")))

;; (defun tcc-origin-equal (org1 org2)
;;   (or (tree-equal org1 org2 :test #'equalp)
;;       (break "tcc-origin-equal")))

;; (defun orph-proofs-equal (proofs oproof)
;;   ;; Orphaned proofs just keep the formula id and proofscript
;;   (and (eq (car proofs) (car oproof)) ; formula id
;;        (some #'(lambda (prf) (orph-proofs-equal* (cddr prf) oproof)) proofs)))

;; (defun orph-proofs-equal* (proofs oproof)
;;   (some #'(lambda (prf) (equalp (fourth prf) (cdr oproof))) (cddr proofs)))


(defun read-theory-proofs (filename thid)
  (let ((fproofs (read-pvs-file-proofs filename)))
    (assoc thid fproofs :test #'string=)))

(defvar *proof-file-debug* nil)

(defun read-pvs-file-proofs (filename &optional (dir *default-pathname-defaults*))
  (let ((prf-file (make-prf-pathname filename dir)))
    (when (uiop:file-exists-p prf-file)
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
	   (if (member cproof proofs :test #'equalp)
	       proofs
	       (cons cproof proofs)))))))

(defun write-pvs-file-proofs (filename theory-proofs
			      &optional (dir *default-pathname-defaults*))
  (let ((prf-file (make-prf-pathname filename dir)))
    (if *proof-file-debug*
	(with-open-file (out prf-file :direction :output
			     :if-exists :supersede :if-does-not-exist :create)
	  (dolist (prf theory-proofs)
	    (write prf :stream out :length nil :level nil :readably t
		   :pretty *save-proofs-pretty*)
	    (when *save-proofs-pretty* (terpri out))))
	(handler-case 
	    (with-open-file (out prf-file :direction :output
				    :if-exists :supersede :if-does-not-exist :create)
	      (dolist (prf theory-proofs)
		(write prf :stream out :length nil :level nil :readably t
		       :pretty *save-proofs-pretty*)
		(when *save-proofs-pretty* (terpri out))))
	  (error (condition)
	    (pvs-message "Error writing proof file ~a:~%  ~a"
	      (namestring prf-file) condition))))))
  

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


;;; This allows proof files to be read when they were produced by
;;; case-sensitive lisp (i.e., Allegro).  It does this by selectively
;;; using different readtables, according to the depth of the parentheses.

;;; A proof file (extension .prf) has the following form:
;;; proof-file := {'(' theoryid  {'(' declid index proof + ')'} + ')'} +
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

;;; orphaned-proofs.prf is similar:
;;; orphaned-proofs := { '(' filename theoryid declid index proof ')' }+

;;; The reason for this is that individual declarations may have their
;;; proofs orphaned, so it makes no sense to try and keep them by filename
;;; and theoryid

;;; The theoryid, declid, proofid, and refers-to should be read in a case
;;; sensitive way, but the status, interactive? and decision-procedure-used
;;; should be read in a case-insensitive mode.  The script is a bit tricky,
;;; but for now we will read it case-insensitively.

;;; read-proof simply reads the next proof in the stream,
;;; The way we do this is to use read-char to read the first \#(, then keep
;;; track of what we should be reading, and read using the proper readtable

#+sbcl
(defun read-proof (stream &optional eof-value)
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

#+sbcl
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

#+sbcl
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
	       ;;(status nil)
	       (refers-to nil)
	       ;;(real-time nil)
	       ;;(run-time nil)
	       ;;(interactive? nil)
	       (dp nil)
	       (origin nil))
	  (cond ((and script (integerp script))
		 ;; Old
		 (setq script (read stream nil))
		 (read stream nil) ;; status 
		 (setq refers-to
		       (if *proof-file-debug*
			   (read-proofs-refers-to stream)
			   (ignore-errors (read-proofs-refers-to stream))))
		 (read stream nil) ;; real-time
		 (read stream nil) ;; run-time 
		 (read stream nil) ;; interactive?
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

#+sbcl
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
      
#+sbcl
(defun read-to-paren-or-quote (stream)
  (let ((ch (read-char stream)))
    (if (member ch '(#\( #\) #\") :test #'char=)
	ch
	(read-to-paren stream))))

#+sbcl
(defun read-to-paren (stream)
  (let ((ch (read-char stream)))
    (if (member ch '(#\( #\)) :test #'char=)
	ch
	(read-to-paren stream))))

#+sbcl
(defun read-to-left-paren (stream)
  (let ((ch (read-char stream)))
    (unless (char= ch #\()
      (read-to-left-paren stream))))

#+sbcl
(defun read-to-right-paren (stream)
  (let ((ch (read-char stream)))
    (unless (char= ch #\))
      (read-to-left-paren stream))))

#+sbcl
(defvar *case-sensitive-readtable* nil)

#+sbcl
(defun read-case-sensitive (stream &optional eof-value)
  (unless *case-sensitive-readtable*
    (setq *case-sensitive-readtable* (copy-readtable nil))
    (setf (readtable-case *case-sensitive-readtable*) :preserve)
    (setf (sb-ext:readtable-normalization *case-sensitive-readtable*) nil))
  (let ((*readtable* *case-sensitive-readtable*))
    (read stream nil eof-value)))

(defun upcase-symbols (val)
  (cond ((consp val)
	 (let ((lcar (upcase-symbols (car val)))
	       (lcdr (upcase-symbols (cdr val))))
	   (cons lcar lcdr)))
	((keywordp val)
	 (intern (string-upcase val) :keyword))
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
		      (upcase-symbols script)
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

(defun convert-proof-form (id description create-date script refers-to
			   decision-procedure-used &optional origin)
  (assert (or (stringp description) (null (filter-nil-form description))))
  (assert (listp (filter-nil-form script)))
  (assert (or (listp refers-to) (null (filter-nil-form refers-to))))
  (assert (typep decision-procedure-used '(or symbol string)))
  (let ((ncreate-date (upcase-t-and-nil create-date))
	(nscript (upcase-symbols script))
	(nrefers-to (upcase-t-and-nil refers-to))
	(ndp-used (upcase-symbols (if (stringp decision-procedure-used)
				      (intern decision-procedure-used :pvs)
				      decision-procedure-used)))
	(norigin (when (upcase-t-and-nil origin)
		   (list (list (car origin) (upcase-symbols (cadr origin))
			       (caddr origin) (cadddr origin) (fifth origin))))))
    `(,id ,description ,ncreate-date ,nscript ,nrefers-to ,ndp-used
	  ,@norigin)))

(defun find-first-symbol (obj)
  (typecase obj
    (null nil)
    (string nil)
    (symbol obj)
    (cons (or (find-first-symbol (car obj))
	      (find-first-symbol (cdr obj))))))

(defun check-if-case-change-needed (script)
  (declare (ignore script))
  nil)

(defun get-orphaned-proof-file (&optional (dir (current-context-path)))
  (let ((file (cond ((uiop:directory-exists-p dir)
		     (merge-pathnames "orphaned-proofs.prf"
				      (ensure-trailing-slash dir)))
		    ((and (uiop:file-exists-p dir)
			  (string= (pathname-name dir) "orphaned-proofs")
			  (string= (pathname-type dir) "prf"))
		     dir)
		    (t (error "Invalid argument to read-orphaned-proofs: ~s"
			      dir)))))
    (when (uiop:file-exists-p file)
      file)))

(defun get-orphaned-proofs (&optional (dir (current-context-path)))
  (let ((file (get-orphaned-proof-file dir)))
    (when file
      (with-open-file (orph-strm file :direction :input)
	(let ((oproofs nil)
	      (old-form-prfs nil)
	      (oprf (read orph-strm nil :eof)))
	  (loop while (not (eq oprf :eof))
		do (progn (if (valid-orphaned-proof oprf)
			      (push oprf oproofs)
			      (push oprf old-form-prfs))
			  (setq oprf (read orph-strm nil :eof))))
	  (when old-form-prfs
	    (with-open-file (olds "orphaned-proofs.old" :direction :output
				  :if-exists :append :if-does-not-exist :create)
	      (dolist (old-prf old-form-prfs)
		(format olds "~a~%" old-prf)))
	    (pvs-message "Badly formed orphaned proofs: ~d appended to orphaned-proofs.old"
	      (length old-form-prfs)))
	  oproofs)))))

(defun valid-orphaned-proof (oprf)
  ;; oprf has form
  ;;  (filename theory-id formula-id index &rest proofs),
  ;; Where each proof has form
  ;;  (proof-id description creation-date script refers-to dp &optional tcc-origin)
  ;; and the optional tcc-origin has form
  ;;  (root kind trigger type place)
  (and (listp oprf)
       (> (length oprf) 4)
       (stringp (car oprf)) ; filename
       (symbolp (cadr oprf))
       (symbolp (caddr oprf))
       (integerp (cadddr oprf))
       (every #'valid-proof-info-list (cddddr oprf))))

(defun valid-proof-info-list (pri)
  (and (<= 6 (length pri) 7)
       (symbolp (car pri))
       (integerp (caddr pri))
       ;; Assume this is enough to validate
       ))
    

(defun read-orphaned-proofs (&optional (dir (current-context-path)))
  (let ((file (get-orphaned-proof-file dir)))
    (when file
      (if *proof-file-debug*
	  (with-open-file (orph-strm file :direction :input)
	    (multiple-value-bind (oproofs total dropped)
		(read-orphaned-file-entries orph-strm)
	      (unless (zerop dropped)
		(pvs-message "Orphan file ~a returned ~d proofs, found ~d bad ones"
		  file total dropped))
	      oproofs))
	  (handler-case
	      (with-open-file (orph-strm file :direction :input)
		(multiple-value-bind (oproofs total dropped)
		    (read-orphaned-file-entries orph-strm)
		  (unless (zerop dropped)
		    (pvs-message "Orphan file ~a returned ~d proofs, found ~d bad ones"
		      file total dropped))
		  oproofs))
	    (file-error (err)
	      (pvs-message "~a" err)))))))

(defun read-orphaned-file-entries (orph-strm &optional proofs (total 0) (dropped 0))
  (ignore-errors
    (let ((prf (read orph-strm nil 'eof)))
      (cond ((eq prf 'eof)
	     (values (nreverse proofs) total dropped))
	    (t (let* ((nprf (get-orphaned-proof-entry prf))
		      (nproofs (cond (nprf
				      (pushnew nprf proofs :test #'equalp))
				     (t (incf dropped)
					(pvs-log "Ignored bad orphaned-proofs.prf entry")
					proofs))))
		 (read-orphaned-file-entries orph-strm nproofs (1+ total) dropped)))))))

(defun get-orphaned-proof-entry (prf)
  ;; orphaned-proofs := { '(' filename theoryid declid index proof ')' }+
  (when (true-listp prf)
    (multiple-value-bind (filename theoryid declid index)
	(values-list prf)
      (when (and (or (symbolp declid) (stringp declid))
		 (> (length prf) 4))
	(let ((proofs (cddddr prf))
	      (changed? nil))
	  (or (> (length prf) 4)
	      (break "get-orphaned-proof-entry: length"))
	  (setq filename (filter-nil-form filename))
	  (setq theoryid (filter-nil-form theoryid))
	  (unless (or (null filename) (stringp filename))
	    (if (and (symbolp filename)
		     (symbolp theoryid)
		     (equal declid ""))
		(let ((proof (list (gentemp "prfid-") "" nil (cddr prf) nil nil)))
		  ;; proofid description create-date script refers-to decision-procedure-used
		  (setf declid theoryid
			theoryid filename
			filename nil
			index 0
			proofs (list proof)
			changed? t))
		(break "get-orphaned-proof-entry: filename = ~a" filename)))
	  (unless (integerp index)
	    (when (or (equal index "")
		      (and (consp index)
			   (string-equal (car index) :new-ground?)))
	      (let* ((script (if (equal index "") (cdddr prf) (cddddr prf)))
		     (proof (list (gentemp "prfidx-") "" nil script nil nil)))
		(assert (and (listp script) (or (null script) (equal (car script) ""))))
		(setf index 0
		      proofs (list proof)
		      changed? t))))
	  ;; Give up if we can't find or create an index
	  (when (integerp index)
	    (or (symbolp theoryid)
		(break "get-orphaned-proof-entry: theoryid"))
	    (or (symbolp declid) 
		(break "get-orphaned-proof-entry: declid = ~s" declid))
	    (let ((nproofs nil))
	      (dolist (prf proofs)
		(let ((nprf (get-proofs-entry prf)))
		  (if nprf
		      (pushnew nprf nproofs :test #'equalp)
		      (setf changed? t))))
	      (setf nproofs (nreverse nproofs))
	      (if (and (equal nproofs proofs)
		       (not changed?))
		  prf
		  `(,filename ,theoryid ,declid ,index ,@nproofs)))))))))

(defun get-proofs-entry (proof)
  (let* ((nproof (mapcar #'filter-nil-form proof))
	 (changed? (not (equal nproof proof))))
    (multiple-value-bind (proofid description create-date run-date script status refers-to
				  real-time run-time interactive? decision-procedure-used
				  tcc-origin)
	(values-list nproof)
      (when (or (integerp run-date) ;; Actual run-date
		(null run-date)	    ;; none provided
		(and (listp run-date) (equal (car run-date) ""))) ;; script from old entry
	(cond ((<= 10 (length proof) 12)
	       ;; proof := '(' proofid      % symbol
	       ;;              description  % string
	       ;;              create-date  % integer
	       ;;              run-date     % integer
	       ;;              script       % sexpr
	       ;;              status       % symbol
	       ;;              refers-to    % list of declaration references
	       ;;              real-time    % integer
	       ;;              run-time     % integer
	       ;;              interactive? % symbol
	       ;;              decision-procedure-used % symbol
	       ;;              tcc-origin   % (root kind expr type place)
	       ;;          ')'
	       )
	      ((<= 6 (length proof) 7)
	       ;; proofid description create-date script refers-to decision-procedure-used
	       (assert (or (null run-date)
			   (and (listp run-date) (equal (car run-date) "")))) ;; script
	       (assert (or (listp script))) ;; refers-to
	       (when (= (length proof) 7)
		 (setf tcc-origin refers-to))
	       (setf refers-to script)
	       (setf script run-date)
	       (setf decision-procedure-used status)
	       (setf status nil)
	       (setf run-date nil)
	       (setf changed? t))
	      (t (break "(length proof) = ~d" (length proof))))
	(or (symbolp proofid) (stringp proofid)
	    (break "proofid = ~s" proofid))
	(unless (or (null description) (stringp description))
	  (break "description = ~a" description))
	(or (null create-date) (integerp create-date)
	    (break "create-date = ~a" create-date))
	(or (null run-date) (integerp run-date)
	    ;; (break "run-date")
	    )
	(or (symbolp status)
	    (break "status"))
	(unless (listp refers-to)
	  (break "refers-to = ~a" refers-to))
	(unless (or (null real-time) (integerp real-time))
	  (break "real-time = ~a" real-time))
	(unless (or (null run-time) (integerp run-time))
	  (break "run-time = ~a" run-time))
	(or (symbolp interactive?)
	    (break "interactive?"))
	(or (symbolp decision-procedure-used)
	    (break "decision-procedure-used"))
	(when (and (listp script) (or (null script) (equalp (car script) "")))
	  (if (and (<= 10 (length proof) 12)
		   (not changed?))
	      proof
	      (list proofid description create-date run-date script status refers-to
		    real-time run-time interactive? decision-procedure-used tcc-origin)))))))


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
  "Called from Emacs"
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
  "Called from Emacs"
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
  "Called from Emacs"
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
  (dolist (lib-path (current-library-pathnames))
    (load-library-strategy-file lib-path)))

(defun load-library-strategy-file (lib)
  (let* (#+allegro (sys:*load-search-list* *pvs-library-path*)
	 #+sbcl (sb-unix::*on-dangerous-wait* nil)
	 (file (make-pathname :directory (namestring lib) :name "pvs-strategies")))
    (when (file-exists-p file)
      (let ((fwd (file-write-time file))
	    (entry (assoc lib *library-strategy-file-dates* :test #'equal)))
	(unless (and (cdr entry)
		     (= fwd (cdr entry)))
	  (if entry
	      (setf (cdr entry) fwd)
	      (push (cons lib fwd) *library-strategy-file-dates*))
	  (unless (load file :verbose nil)
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
	  (unwind-protect
	       (multiple-value-bind (v err)
		   (ignore-errors (load file))
		 (declare (ignore v))
		 (when err
		   (pvs-message "Error in loading ~a:~%  ~a" file err)))
	    (add-lowercase-prover-ids)))))))


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
		     (and type (not (member type '("pvs" "prf") :test #'string=))))
		   (format nil "~a" pname)
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
	      (let ((oproofs (read-orphaned-proofs)))
		(when oproofs
		  (reload-theory-orphaned-proofs nt oproofs))))
	  newtheories)))

(defun reload-theory-orphaned-proofs (theory oproofs)
  (mapc #'(lambda (decl)
	    (when (typep decl 'formula-decl)
	      (let ((prf-entry (car (member (id decl) oproofs
					    :test #'(lambda (id prf)
						      ;; (file theory decl . proof-script)
						      (string= id (caddr prf)))))))
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
    (dolist (ce (pvs-context-entries))
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
    (dolist (ce (pvs-context-entries))
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
  "Called from Emacs"
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
			  (when (member tid deps :test #'string=)
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
	   (pvs-message "~a not found" prf-file))
	  ((null theories)
	   (if (file-exists-p (make-specpath filename))
	       (pvs-message
		   "Proof will be loaded when the file is next parsed.")
	       (pvs-message "~a.pvs not found." filename)))
	  (t (restore-proofs filename :theories theories)))))

(defmethod clear-proof-status (theory)
  (let ((te (get-context-theory-entry theory)))
    (when te
      (dolist (fe (te-formula-info te))
	(when fe
	  (setf (fe-status fe) "untried")
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

(defun move-auto-saved-proofs-to-orphan-file ()
  (let ((auto-saved-files (directory (make-pathname
				      :defaults *default-pathname-defaults*
				      :name :wild
				      :type "prf#")))
	(proofs nil))
    (when auto-saved-files
      (dolist (auto-saved-file auto-saved-files)
	(with-open-file (asave auto-saved-file :direction :input)
	  (let ((proof (read asave)))
	    ;; proof has form
	    ;; ((thy-id (fmla-id index proofs) (fmla-id ...) ...) (thy-id ...) ...)
	    ;; proofs is a list of elements of the form
	    ;; (proof-id description create-date script refers-to)
	    ;; orhpaned-proof-entry has form
	    ;; (filename thy-id fmla-id script)
	    (push proof proofs)))
	(delete-file auto-saved-file)
	(let ((prf-file (string-trim "#" auto-saved-file)))
	  (when (file-exists-p prf-file)
	    (let ((proofs (read-pvs-file-proofs prf-file)))
	      (copy-proofs-to-orphan-file (pathname-name prf-file) proofs)))))
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

;;; ============================================================================
;;; Purge non-default proofs for a single formula (if default is proved)
;;; ============================================================================

(defun purge-formula-proofs (theoryref formularef &optional (save? t))
  "Remove non-default proofs from a formula if its default proof status is 'proved'.
   THEORYREF: theory name (string)
   FORMULAREF: formula name (string)
   SAVE?: if T, save the proof file after purging (default T)
   Returns T if proofs were purged, NIL otherwise.
   Uses find-formula from prooflite.lisp"
  (let ((formula (find-formula theoryref formularef)))
    (cond
      ((null formula)
       (pvs-message "Formula ~a not found in theory ~a" formularef theoryref)
       nil)
      ((not (eq (proof-status formula) 'proved))
       (pvs-message "Formula ~a is not proved (status: ~a)"
                    formularef (proof-status formula))
       nil)
      ((<= (length (proofs formula)) 1)
       (pvs-message "Formula ~a has only ~a proof(s), nothing to purge"
                    formularef (length (proofs formula)))
       nil)
      (t
       (purge-formula-proofs* formula save?)))))

(defun purge-formula-proofs* (formula &optional save?)
  "Actually purge the non-default proofs from FORMULA.
   SAVE?: if T, save the proof file after purging"
  (let* ((default (default-proof formula))
         (num-before (length (proofs formula))))
    ;; Keep only the default proof
    (setf (proofs formula) (list default))
    ;; Update the proof file if requested
    (when save?
      (let ((file (filename (module formula))))
        (save-pvs-file-proofs file t)))  ;; force save
    (pvs-message "Purged ~a non-default proof(s) from ~a (kept: ~a)"
                 (1- num-before) (id formula) (id default))
    t))

(defun purge-proved-formulas-file (filename &optional (dir *default-pathname-defaults*))
  "Purge non-default proofs from all proved formulas in a file.
   FILENAME: pvs/prf file name
   Returns the number of formulas purged."
  (let ((prf-file (make-prf-pathname filename dir)))
    (unless (uiop:file-exists-p prf-file)
      (pvs-message "Proof file ~a does not exist" prf-file)
      (return-from purge-proved-formulas-file nil))
    (with-pvs-file
	(file) prf-file
	(let ((theories (cdr (gethash file (current-pvs-files))))
	      (count 0))
	  (dolist (th theories)
	    (let ((theory (get-theory th)))
	      (when theory
		(dolist (decl (append (assuming theory)
				      (when (module? theory) (theory theory))))
		  (when (and (formula-decl? decl)
                             (eq (proof-status decl) 'proved)
                             (> (length (proofs decl)) 1))
                    (purge-formula-proofs* decl nil)  ;; don't save yet
                    (incf count))))))
	  (when (> count 0)
            (save-pvs-file-proofs file t))  ;; save once at the end
	  (if (zerop count)
              (pvs-message "No formulas needed purging in ~a" file)
              (pvs-message "Purged proofs from ~a formula(s) in ~a" count file))
	  count))))

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
	(unless (member (te-id te) nonmax :test #'string=)
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
		 (expected-bin-date (cdr (assoc theory-id (ce-object-date ce)
						:test #'string=)))
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
  (or (and (member (fe-status fe) '("proved" "proved-complete" "proved-incomplete")
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

(defun all-proof-statuses (theory)
  (with-context theory
    (let ((fe-infos (te-formula-info (get-context-theory-entry (id theory))))
	  (fmlas (provable-formulas theory)))
      (assert (= (length fe-infos) (length fmlas)))
      (mapcar #'(lambda (fe fm)
		  (assert (string= (fe-id fe) (id fm)))
		  (format t "~%~a: ~s - ~s"
		    (id fm) (fe-status fe)
		    (if (default-proof fm) (status (default-proof fm)) "No proofs")))
	fe-infos fmlas)
      (terpri)
      nil)))
    

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
;;;  PVSEMACS - whether to sync with Emacs
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
If there is no error, but the ls-files is empty"
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

(defun pvs-context-to-json (&optional (ws (current-workspace)))
  (pvs-context-to-json* ws))

(defun pvs-context-to-json-file (&optional (file ".pvscontext.json")
				   (ws (current-workspace)))
  (pvs-context-to-json* ws file))
  
(defun pvs-context-to-json* (workspace &optional file)
  "First saves the .pvscontext file if needed, then generates the JSON
representation. If file is nil, the JSON string is returned, else if file is
't', .pvscontext.json is used, else the file provided will be written to."
  (with-workspace ws
    (save-context)
    (let* ((ctx-alist (ctx-alist ws)))
      (if file
	  (with-open-file (ctx-fd json-file
				  :direction :output :if-exists :supersede
				  :if-does-not-exist :create)
	    (json:encode-json ctx-alist ctx-fd))
	  (json:encode-json-to-string ctx-alist)))))

(defun ctx-alist (&optional (ws (current-workspace)))
  (with-workspace ws
    (let ((ctx (pvs-context ws)))
      `(("version" . ,(pvs-context-version ctx))
	("prelude-libraries" . ,(pvscontext-prelude-libs ctx))
	("pvs-files" . ,(mapcar #'ce-alist (pvscontext-entries ctx)))))))

(defun ce-alist (ce)
  (let* ((fname (ce-file ce))
	 (file (make-specpath fname))
	 (sha1 (get-file-git-sha1 file)))
    `(("pvs-file" . ,fname)
      ("write-date" . ,(when (ce-write-date ce) (iso8601-date (ce-write-date ce))))
      ("proofs-date" . ,(when (ce-proofs-date ce) (iso8601-date (ce-proofs-date ce))))
      ("object-date" . ,(mapcar #'(lambda (obd)
				    (cons (car obd) (iso8601-date (cdr obd))))
			  (ce-object-date ce)))
      ("dependencies" . ,(ce-dependencies ce))
      ("theories" . ,(mapcar #'te-alist (ce-theories ce)))
      ("SHA1" . ,sha1))))

(defun te-alist (te)
  `(("theory-id" . ,(te-id te))
    ("status" . ,(te-status te))
    ("dependencies" . ,(mapcar #'te-dependency (te-dependencies te)))
    ("formula-info" . ,(mapcar #'te-formula (te-formula-info te)))))

(defun te-dependency (dep)
  (let ((lib-path (car dep))
	(dep-theories (cdr dep)))
    `(("path" . ,(when lib-path (namestring lib-path)))
      ("dep-theories" . ,dep-theories))))

(defun te-formula (fe)
  `(("formula-id" . ,(fe-id fe))
    ("status" . ,(string (fe-status fe)))
    ("proof-refers-to" . ,(mapcar #'decl-entry (fe-proof-refers-to fe)))))

(defun decl-entry (de)
  `(("decl-id" . ,(de-id de))
    ("class" . ,(de-class de))
    ("type" . ,(de-type de))
    ("theory-id" . ,(de-theory-id de))
    ("library" . ,(de-library de))))

(defun consistent-fe-entries-and-proofs (thname)
  (let ((all-good t))
    (every #'(lambda (fe fdecl)
	       (if (string= (fe-id fe) (id fdecl))
		   (let ((prstat (proof-status fdecl))
			 (festat (fe-status fe)))
		     (unless (cond ((null prstat) (string-equal festat "untried"))
				   ((string-equal prstat "proved")
				    (member festat
					    '("proved-complete" "proved-incomplete")
					    :test #'string-equal))
				   (t (string-equal prstat festat)))
		       (setq all-good nil)
		       (error "proof/fe status mismatch: ~a vs ~a"
			      (or prstat "untried") festat)))
		   (progn (setq all-good nil)
			  (error "proof/fe formula mismatch: ~a vs ~a"
				 (id fdecl) (fe-id fe)))))
	   (te-formula-info (get-context-theory-entry thname))
	   (provable-formulas (get-theory thname)))
    all-good))
