;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspaces.lisp -- Support for workspace-sesstions.
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

;;; Note: workspace-sessions all have an absolute pathname, and
;;; *all-workspace-sessions* is a list of instances that have been visited,
;;; either through change-workspace or with-workspace.

;;; workspace-session class is in classes-decl.lisp

;;; with-workspace is in macros.lisp - temporarily changes to the specified ws
;;; *workspace-session* is the current ws - don't bind this, use with-workspace.

(defvar *workspace-stack* nil)

(defun initialize-workspaces (&optional (path *default-pathname-defaults*))
  "Creates the initial *workspace-session*, and adds it to an empty
  *all-workspace-sessions*"
  ;; in case a prelude-workspace is useful, but it would only have the path and
  ;; pvs-theories (same as *prelude-theories*)
  ;; (init-prelude-workspace)
  (break "initialize-workspaces")
  (let ((ws (get-workspace-session path)))
    (setq *all-workspace-sessions* (list ws))
    (setq *workspace-session* ws)
    (set-pvs-paths-defaults)
    (restore-context ws)
    (assert (pvscontext ws))))

(defun init-prelude-workspace ()
  (assert (not *loading-prelude*))
  (let* ((lib-path (merge-pathnames "lib/" *pvs-path*))
	 (ws (make-instance 'workspace-session :path lib-path)))
    (push ws *all-workspace-sessions*)
    ;; Note that the prelude workspace has no pvs-context and no pvs-files
    (setf (pvs-theories ws) *prelude*)
    ws))

(defun find-workspace (lib-path)
  "Given a path, finds the workspace, if it exists. Does not create it."
  (find lib-path *all-workspace-sessions* :key #'path :test #'file-equal))

(defun pvs-paths-consistent ()
  ;; Each workspace has a path; *workspace-session* is set as the (current-workspace)
  ;; (working-directory) and *default-pathname-defaults* are kept the same
  ;; The working directory is where external programs are run;
  ;; *default-pathname-defaults* is for pathname manipulation
  (let ((ws-path (path (current-workspace))))
    (and (eq (working-directory) ws-path)
	 (eq *default-pathname-defaults* ws-path))))

(defun set-pvs-paths-defaults (&optional path)
  "Ensures PVS path consistency by setting (working-directory) and
*default-pathname-defaults* to the path of (current-workspace)"
  (let ((ws-path (pathname (or path (path (current-workspace))))))
    (set-working-directory ws-path)
    (setq *default-pathname-defaults* ws-path)))

(defun get-workspace-session (libref)
  "get-workspace-session gets the workspace corresponding to libref,
creating it if necessary. Gets the absolute pathname associated with libref,
and uses that as the key to find the ws in *all-workspace-sessions*,
creating a new one if needed.  Error if an existing directory could not be
found for libref."
  (cond ((null libref)
	 *workspace-session*)
	((workspace-session? libref)
	 libref)
	(t (let ((lib-path (get-library-path libref)))
	     (if lib-path
		 (or (unless *loading-prelude* ; find-workspace may not be set up
		       (let ((ws (find-workspace lib-path)))
			 (when ws
			   (assert (pvs-context ws))
			   ws)))
		     (let ((ws (make-instance 'workspace-session
				 :path lib-path)))
		       (push ws *all-workspace-sessions*)
		       (if *loading-prelude*
			   (setf (pvscontext ws) (initial-context))
			   (restore-context ws))
		       (assert (pvscontext ws))
		       ws))
		 (pvs-error "Library reference error"
		   (format nil "Path for ~a not found" libref)))))))

;;; Show Context Path

(defun show-context-path ()
  (pvs-message (current-context-path)))

;;; workspace access functions

(defun current-workspace ()
  (assert (memq *workspace-session* *all-workspace-sessions*))
  *workspace-session*)

(defun current-context-path ()
  (assert (memq *workspace-session* *all-workspace-sessions*))
  (let ((cpath (path (current-workspace))))
    (unless (uiop:directory-exists-p cpath)
      (pvs-error "Directory missing" (sformat "Directory ~a no longer exists" cpath)))
    cpath))

;;; Change-workspace checks that the specified directory exists, prompting
;;; for a different directory otherwise.  When a directory is given, the
;;; current context is saved (if writable), *last-proof* is cleared, the
;;; working-directory is set, and the context is restored.

(defun change-workspace (libref &optional init? quiet?)
  "PVS must always have a workspace, usually the initial one is the
directory it was started in.  The *current-workspace* has an associated
workspace-session, Which is where the parsed/typechecked theories are found.
Note that changing workspaces does not modify the current one; if you return
again during the same PVS session, it will be exactly as you left it."
  (let* ((ws (get-workspace-session libref))
	 (wspath (path ws)))
    (assert (memq ws *all-workspace-sessions*))
    (assert (or init? (memq *workspace-session* *all-workspace-sessions*)))
    (unless (pvscontext ws)
      (if *loading-prelude*
	  (setf (pvscontext ws) (initial-context))
	  (restore-context ws)))
    ;; (assert (pvscontext ws))
    (cond ((and (not init?)
		(eq (current-workspace) ws))
	   (unless quiet?
	     (pvs-message "Change Workspace: already in ~a" libref)))
	  (t 
	   (unless init?
	     (save-context)) ;; Saves .pvscontext and binfiles of current ws
	   (push *workspace-session* *workspace-stack*)
	   (setq *workspace-session* ws)
	   (set-pvs-paths-defaults)
	   (when (and (not init?)
		      (write-permission?))
	     (move-auto-saved-proofs-to-orphan-file))
	   (unless quiet?
	     (pvs-message "Context changed to ~a" wspath))))
    (namestring wspath)))

(defun cw (directory)
  (change-workspace directory))

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

(defun consistent-workspace-paths ()
  "Checks whether *default-pathname-defaults*, (working-directory), and
(current-context-path) are all the same."
  (and (file-equal *default-pathname-defaults* (working-directory))
       (file-equal *default-pathname-defaults* (current-context-path))))

(defun clear-theories (&key workspace ;; nil => *workspace-session*
			 empty-pvs-context delete-binfiles dont-load-prelude-libraries)
  (clear-workspace :workspace workspace
		   :empty-pvs-context empty-pvs-context
		   :delete-binfiles delete-binfiles
		   :dont-load-prelude-libraries dont-load-prelude-libraries))

(defun clear-all-workspaces (&key empty-pvs-context delete-binfiles)
  (clear-workspace :workspace t
		   :empty-pvs-context empty-pvs-context
		   :delete-binfiles delete-binfiles))

(defun clear-workspace (&key workspace empty-pvs-context delete-binfiles
			  dont-load-prelude-libraries)
  "Clears the given workspace, or the current workspace if nil, and all
workspaces if t or 'all.  Roughly speaking, it's in the state of a workspace
at the start of a PVS session.

Clearing a workspace tries to save the .pvscontext file, initializes the
workspace-session instance, removes binfiles if delete-binfiles is not nil,
loads .pvscontext, and any prelude library extensions in the .pvscontext
 (see load-prelude-libraries), unless dont-load-prelude-libraries is not
nil."
  (clear-background-context)
  (let ((*dont-write-object-files* t))
    ;; Don't see errors if trying to clear workspaces - only binfiles and
    ;; proof statuses are affected, both of which will be recreated
    (ignore-errors (save-context empty-pvs-context)))
  (reset-typecheck-caches)
  (let* ((*circular-file-dependencies* nil)
	 (workspaces (cond ((member workspace '("all" "t") :test #'string-equal)
			    *all-workspace-sessions*)
			   ((typep workspace '(or string pathname))
			    (list (get-workspace-session workspace)))
			   ((null workspace)
			    (list *workspace-session*))))
	 ;; (ws-closure (if (eq workspaces *all-workspace-sessions*)
	 ;; 		 workspaces
	 ;; 		 ;;(close-workspace-dependencies workspaces)
	 ;; 		 workspaces))
	 )
    (assert (memq *workspace-session* *all-workspace-sessions*))
    (dolist (ws workspaces)
      (cond ((uiop:directory-exists-p (path ws))
	     (clrhash (pvs-files ws))
	     (clrhash (pvs-theories ws))
	     (clrhash (all-subst-mod-params-caches ws))
	     (setf (last-kept-decls ws) nil)
	     (if empty-pvs-context
		 (setf (pvscontext ws) (initial-context))
		 (when (and (not dont-load-prelude-libraries)
			    (listp (pvscontext-prelude-libs (current-pvs-context)))
			    (every #'stringp (pvscontext-prelude-libs (current-pvs-context))))
		   ;; May need to make sure a later ws isn't a prelude-library
		   (load-prelude-libraries (prelude-libs ws))))
	     (when delete-binfiles
	       (let ((bindir (format nil "~a~a/" (path ws) *pvsbin-string*)))
		 (dolist (bf (uiop:directory-files bindir "*.bin"))
		   (delete-file bf)))))
	    (t (setq *all-workspace-sessions* (remove ws *all-workspace-sessions*))
	       (pvs-message "Directory ~a has disappeared" (path ws)))))
    t))

(defvar *workspace-deps*)

(defun workspace-depends-on (workspace)
  (let ((*workspace-deps* nil))
    (workspace-depends-on* workspace)
    *workspace-deps*))

(defun workspace-depends-on* (workspace)
  (maphash #'(lambda (id th)
	       (declare (ignore id))
	       (unless (from-prelude? th)
		 (dolist (imp (all-usings th))
		   (unless (from-prelude? (car imp))
		     (let* ((cp (context-path (car imp)))
			    (ws (get-workspace-session cp)))
		       (assert ws)
		       (unless (or (eq ws workspace)
				   (memq ws *workspace-deps*))
			 (push ws *workspace-deps*)
			 (workspace-depends-on* ws)))))))
	   (pvs-theories workspace)))

(defun workspace-dependencies-alist ()
  (let ((deps-alist nil))
    (dolist (ws *all-workspace-sessions*)
      (let ((ws-deps (workspace-dependencies ws)))
	(push ws-deps deps-alist)))
    deps-alist))

(defun workspace-dependencies (workspace)
  (let ((deps nil))
    (maphash #'(lambda (id th)
		 (declare (ignore id))
		 (unless (from-prelude? th)
		   (dolist (imp-th (immediate-importings th))
		     (unless (from-prelude? imp-th)
		       (let* ((cp (context-path imp-th))
			      (ws (get-workspace-session cp)))
			 (assert ws)
			 (unless (eq ws workspace)
			   (pushnew ws deps)))))))
	     (pvs-theories workspace))
    (cons workspace deps)))

;; workspaces is the list of workspaces to be cleared; need to also clear
;; any workspace that has a theory referencing it.
;; (defun workspace-upward-closure (workspaces)
;;   ;; ws-alist is s.t. the cdr are all the workspaces directly referenced
;;   ;; by any theory in the first workspace
;;   (let ((ws-alist (workspace-dependencies-alist)))
;;     (workspace-upward-closure* workspaces ws-alist nil)))

;; (defun workspace-upward-closure* (workspaces ws-alist closure)
;;   (let ((imm-workspaces (remove-if-not #'(lambda (ws-entry)
;; 					   (some #'(lambda (ws) (memq ws (cdr ws-entry)))
;; 						 workspaces))
;; 			  ws-alist)))
;;     (break)))

(defun initialize-workspace-session (ws)
  (with-workspace ws
    (clrhash (current-pvs-files))
    (clrhash (current-pvs-theories))))
