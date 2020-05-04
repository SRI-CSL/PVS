;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tclib.lisp -- Functions for dealing with the prelude and library specs
;; Author          : Sam Owre
;; Created On      : Wed Dec  1 18:07:09 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 17:08:27 1998
;; Update Count    : 49
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

(export '(all-decls))

;;; Load-prelude initializes the *prelude*, *prelude-context*, and
;;; *prelude-theories* variables

(defparameter *prelude-filename* "prelude.pvs")
(defparameter *pvsio-filename* "pvsio_prelude.pvs")

(defun load-prelude ()
  (assert *pvs-path*)
  (let* ((pvs-lib (format nil "~a/lib" *pvs-path*))
	 (pvs-ctx (format nil "~a/.pvscontext" pvs-lib))
	 (*loading-prelude* t))
    (when (file-exists-p pvs-ctx)
      (delete-file pvs-ctx))
    (with-workspace pvs-lib
      (load-core-prelude)
      (load-pvsio-prelude))
    (initialize-workspaces)))

(defun load-core-prelude ()
  (let ((theories (parse :file *prelude-filename*)))
    (when (and *prelude-theories*
	       (or ;;(not (length= *prelude-theories* theories))
		(let ((*current-context* *prelude-context*))
		  (some #'compare *prelude-theories* theories))))
      (format t "~%Prelude has changed - invalidating current binfiles")
      (incf *binfile-version*))
    (setq sbrt::*disable-caching* t)
    (setq *prelude-theories* nil
	  *boolean* nil
	  *number* nil
	  *real* nil
	  *rational* nil
	  *integer* nil
	  *naturalnumber* nil
	  *posint* nil
	  *ordinal* nil
	  *even_int* nil
	  *odd_int* nil
	  *even_nat* nil
	  *even_posnat* nil
	  *odd_posnat* nil
	  *even_negint* nil
	  *odd_negint* nil
	  *negrat* nil
	  *posrat* nil
	  *negint* nil)
    (makunbound '*manip-supported-types*)
    (when *pvs-initialized*
      (clear-theories t))
    (let ((*all-workspace-sessions* nil)
	  (*generate-tccs* 'all))
      (reset-typecheck-caches)
      (dolist (fn *load-prelude-hook*)
	(funcall fn))
      (setq *prelude-context* nil)
      (setq *prelude-core-context* nil)
      (reset-equality-decl)
      (reset-if-declaration)
      (reset-boolean-aliases)
      (dolist (th theories)
	(let ((*current-context* (context th)))
	  (format t "~%Typechecking ~a" (id th))
	  (setf (status th) '(parsed))
	  (setf (gethash (id th) *prelude*) th)
	  (typecheck th)
	  (multiple-value-bind (tot prv unprv sub simp)
	      (numbers-of-tccs th)
	    (if (zerop tot)
		(format t "~%~a typechecked: No TCCs generated" (id th))
		(format t "~%~a typechecked: ~d TCC~:p, ~
                               ~d proved, ~d subsumed, ~d unproved, ~d trivial~
                               ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		  (id th) tot prv sub unprv simp
		  (length (warnings th)) (length (info th)))))
	  ;; No need to have saved-context set for prelude contexts
	  (assert (typep th '(or datatype module)))
	  (setq *prelude-theories*
		(nconc *prelude-theories* (cons th nil)))
	  (setq *prelude-core-context*
		(update-prelude-context th))
	  (setq *prelude-context* *prelude-core-context*)
	  (when (eq (id th) '|booleans|)
	    (let ((*current-context* (saved-context th)))
	      (setq *true*
		    (typecheck* (mk-name-expr 'TRUE) *boolean* nil nil))
	      (setq *false*
		    (typecheck* (mk-name-expr 'FALSE) *boolean* nil
				nil))))))
      (format t "~%Done typechecking the core prelude")
      (restore-prelude-proofs)
      ;;(initialize-prelude-attachments)
      (register-manip-type *number_field* 'pvs-type-real))))

(defun load-pvsio-prelude ()
  (assert *prelude-core-context*)
  (setq sbrt::*disable-caching* t)
  (when *pvs-initialized*
    (clear-theories t))
  (let ((*generate-tccs* 'all)
	(theories (parse :file *pvsio-filename*)))
    (reset-typecheck-caches)
    (dolist (th theories)
      (let ((*current-context* (context th)))
	(format t "~%Typechecking ~a" (id th))
	(setf (status th) '(parsed))
	(setf (gethash (id th) *prelude*) th)
	(typecheck th)
	(multiple-value-bind (tot prv unprv sub simp)
	    (numbers-of-tccs th)
	  (if (zerop tot)
	      (format t "~%~a typechecked: No TCCs generated" (id th))
	      (format t "~%~a typechecked: ~d TCC~:p, ~
                               ~d proved, ~d subsumed, ~d unproved, ~d trivial~
                               ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		(id th) tot prv sub unprv simp
		(length (warnings th)) (length (info th)))))
	;; No need to have saved-context set for prelude contexts
	(assert (typep th '(or datatype module)))
	(setq *prelude-theories*
	      (nconc *prelude-theories* (cons th nil)))
	(setq *prelude-context* (update-prelude-context th))))
    (format t "~%Done typechecking the prelude")
    (restore-pvsio-proofs)
    (initialize-prelude-attachments)))

(defun update-prelude-context (th)
  (let* ((thy (if (typep th 'datatype)
		  (adt-theory th)
		  th))
	 (ctx (saved-context thy))
	 (ndecls-hash (copy (declarations-hash ctx)))
	 (nusing-hash (copy (using-hash ctx))))
    (typecase th
      (datatype
       (setf (get-importings (adt-theory th) nusing-hash)
	     (list (make-theoryname (adt-theory th))))
       (when (adt-map-theory th)
	 (setf (get-importings (adt-map-theory th) nusing-hash)
	       (list (make-theoryname (adt-map-theory th))))
	 (dolist (decl (append (assuming (adt-map-theory th))
			       (theory (adt-map-theory th))))
	   (when (and (declaration? decl) (visible? decl))
	     (put-decl decl ndecls-hash))))
       (when (adt-reduce-theory th)
	 (setf (get-importings (adt-reduce-theory th) nusing-hash)
	       (list (make-theoryname (adt-reduce-theory th))))
	 (dolist (decl (append (assuming (adt-reduce-theory th))
			       (theory (adt-reduce-theory th))))
	   (when (and (declaration? decl) (visible? decl))
	     (put-decl decl ndecls-hash)))))
      (t (setf (get-importings th nusing-hash)
	       (list (make-theoryname th)))))
    (let ((changes nil))
      (maphash #'(lambda (id decls)
		   (let ((rdecls
			  (remove-if #'(lambda (d)
					 (typep d '(or formal-decl
						    var-decl)))
			    decls)))
		     (unless (equal rdecls decls)
		       (push (cons id rdecls) changes))))
	       (lhash-table ndecls-hash))
      (dolist (cdecls changes)
	(setf (gethash (car cdecls) (lhash-table ndecls-hash))
	      (cdr cdecls))))
    (let ((pusing-hash (copy nusing-hash)))
      (setf (get-importings (theory ctx) pusing-hash)
	    (list (theory-name ctx)))
      (copy ctx
	'declarations-hash ndecls-hash
	'using-hash pusing-hash))))

(defun restore-prelude-proofs ()
  (let ((prfile (merge-pathnames (format nil "~a/lib/" *pvs-path*)
				 "prelude.prf")))
    (assert (file-exists-p prfile))
    (format t "~%Restoring the prelude proofs from ~a" prfile)
    (let ((proofs (read-pvs-file-proofs prfile)))
      (dolist (theory (core-prelude-theories))
	(format t "restoring for ~a" (id theory))
	(restore-proofs prfile theory proofs)
	(mapc #'(lambda (decl)
		  (if (justification decl)
		      (setf (proof-status decl) 'proved)
		      ;;(break "No proof for ~a?" (id decl))
		      ))
	      (provable-formulas theory))))))

(defun restore-pvsio-proofs ()
  (let ((prfile (merge-pathnames (format nil "~a/lib/" *pvs-path*)
				 "pvsio_prelude.prf")))
    (assert (file-exists-p prfile))
    (format t "~%Restoring the prelude proofs from ~a" prfile)
    (let ((proofs (read-pvs-file-proofs prfile)))
      (dolist (theory (pvsio-prelude-theories))
	(restore-proofs prfile theory proofs)
	(mapc #'(lambda (decl)
		  (if (justification decl)
		      (setf (proof-status decl) 'proved)
		      ;;(break "No proof for ~a?" (id decl))
		      ))
	      (provable-formulas theory))))))

;;; This is invoked after adding some theories to the prelude Takes a file
;;; name (e.g., "~/widget/foo", and installs all proofs from
;;; ~/widget/foo.prf that have now have a corresponding theory in the
;;; prelude.  This is really only for the PVS maintainers.

(defun merge-proofs-into-updated-prelude (file)
  (let ((prfpath (make-prf-pathname file)))
    (if (file-exists-p prfpath)
	(with-open-file (input prfpath :direction :input)
	  (restore-prelude-proofs-from-file input prfpath))
	(format t "~%Proof file ~a does not exist" prfpath))))

(defun restore-prelude-proofs-from-file (input prfpath)
  (let ((theory-proofs (read input nil nil)))
    (when theory-proofs
      (let* ((theoryid (car theory-proofs))
	     (proofs (cdr theory-proofs))
	     (theory (gethash theoryid *prelude*)))
	(unless (every #'consp proofs)
	  (error "Proofs file ~a is corrupted" prfpath))
	(cond (theory
	       (restore-theory-proofs theory proofs)
	       (format t "~%Theory ~a proofs restored" theoryid))
	      (t (format t "~%Theory ~a not in prelude, ignoring" theoryid)))
	(restore-prelude-proofs-from-file input prfpath)))))

(defun save-prelude-core-proofs ()
  (let ((prfile (namestring (merge-pathnames (format nil "~a/lib/" *pvs-path*)
					     "prelude.prf")))
	(theories (ldiff *prelude-theories*
			 (cdr (memq (theory *prelude-core-context*)
				    *prelude-theories*)))))
    (save-proofs prfile theories)))

(defun core-prelude-theories ()
  (ldiff *prelude-theories* (pvsio-prelude-theories)))

(defun pvsio-prelude-theories ()
  (cdr (memq (theory *prelude-core-context*) *prelude-theories*)))

(defun save-pvsio-prelude-proofs ()
  (let ((prfile (namestring (merge-pathnames (format nil "~a/lib/" *pvs-path*)
					     "pvsio_prelude.prf")))
	(theories (pvsio-prelude-theories)))
    (save-proofs prfile theories)))

(defun prove-prelude (&optional retry? use-default-dp?)
  (let ((theories *prelude-theories*)
	(*loading-prelude* t)
	(*proving-tcc* t))
    (prove-theories "prelude" theories retry? use-default-dp?)
    (prelude-summary)))

(defun prove-pvsio-prelude (&optional retry? use-default-dp?)
  (let ((theories (pvsio-prelude-theories))
	(*loading-prelude* t)
	(*proving-tcc* t))
    (prove-theories "pvsio-prelude" theories retry? use-default-dp?)
    (pvsio-prelude-summary)))

(defun pvsio-prelude-summary ()
  (let ((theories (pvsio-prelude-theories)))
    (pvs-buffer "PVS Status"
      (with-output-to-string (*standard-output*)
	(proof-summaries theories "pvsio_prelude"))
      t)))

(defun prelude-summary (&optional unproved?)
  (let ((theories *prelude-theories*))
    (pvs-buffer "PVS Status"
      (with-output-to-string (*standard-output*)
	(proof-summaries theories "prelude" unproved?))
      t)))

(defun prelude-proofchain ()
  (let ((theories *prelude-theories*))
    (mapc #'(lambda (th)
	      (mapc #'(lambda (d)
			(when (and (formula-decl? d)
				   (not (memq (spelling d) '(assumption
							     axiom))))
			  (pc-analyze d)))
		    (theory th)))
	  theories)))

(defun prelude-tccs ()
  (dolist (th *prelude-theories*)
    (let ((tccs (collect-tccs th)))
      (when tccs
	(format t "~3%%%% TCCs for ~a~%" (id th))
	(show-tccs (id th))))))

(defun prelude-duplicated-names ()
  (map-lhash #'(lambda (id decls)
		 (when (prelude-duplicates? decls)
		   (format t "~%~a" id)))
	     (declarations-hash *prelude-context*)))

(defun prelude-duplicates? (decls)
  (when (cdr decls)
    (let ((rdecls (remove-if #'judgement? decls)))
      (when (cdr rdecls)
	t))))


;;; Library support

;;; There are two forms of library support; extending the prelude, and
;;; importing from a different context.  There are various hash tables
;;; associated with these libraries, most of which assocaite a pathname
;;; with another hashtable, that associates identifiers with theories.
;;; The following variables are used by the library mechanism:

;;; load-prelude-libraries is called from restore-context
(defun load-prelude-libraries (lib-refs)
  (dolist (lib-ref lib-refs)
    (load-prelude-library lib-ref nil t)))

(defvar *loading-libraries* nil)

;;; This is called from the Emacs load-prelude-library command as well.
;;; Loads the .pvscontext, associated PVS files, pvs-lib.lisp file
;;; (and any libloaded file), and pvs-lib.el.  Note that pvs-strategies is
;;; not loaded here, but when the prover is invoked.
(defun load-prelude-library (lib-ref &optional force? quiet?)
  "Given a lib-ref, determines the lib-path, and loads the library.  Note
that a library consists of PVS files, lisp files, and/or emacs files.
There's a function for each of these that says what files were loaded in
each case.  This information is kept in the workspace-session of this
lib-path, along with modification dates."
  (let ((lib-path (get-library-path lib-ref)))
    (unless lib-path
      (pvs-error "Load Prelude Error" (format nil "Directory for ~a not found" lib-ref)))
    (assert (absolute-pathname-p lib-path))
    (when (member lib-path *loading-libraries* :test #'file-equal)
      (pvs-error "Load Prelude Error"
	(format nil "Detected circular calls to load-prelude-library,~%~
                     check pvs-lib.lisp files in ~{~a~^, ~}" *loading-libraries*)))
    (let* ((*loading-libraries* (cons lib-path *loading-libraries*))
	   (pvs-files-loaded (load-prelude-library-workspace lib-path force?))
	   (lisp-files-loaded (load-pvs-lib-lisp-file lib-path force?))
	   (emacs-files-loaded (load-pvs-lib-emacs-file lib-path force?)))
      (when pvs-files-loaded
	(pushnew lib-path (current-prelude-libraries)
		 :test #'uiop:pathname-equal))
      (if (or pvs-files-loaded
	      lisp-files-loaded
	      emacs-files-loaded)
	  (unless (member lib-path (cadr (current-pvs-context)) :test #'file-equal)
	    (if (cdr (current-pvs-context))
		(push lib-ref (cadr (current-pvs-context)))
		(nconc (current-pvs-context) (list (list lib-ref)))))
	  (when (and (not quiet?)
		     (not force?)
		     (prelude-library-loaded? lib-path))
	    (pvs-message "Prelude library ~a is already loaded." lib-ref))))))

(defvar *libloads* nil)

(defun load-pvs-lib-lisp-file (lib-path force?)
  ;; Set up *default-pathname-defaults* and sys:*load-search-list*
  ;; so that simple loads from the pvs-lib.lisp file work.
  ;; TODO: Should check if already loaded.
  (declare (ignore force?))
  (with-workspace lib-path
    (let* (#+allegro (sys:*load-search-list* *pvs-library-path*)
	   (lfile "pvs-lib.lisp")
	   (*suppress-printing* t))
      (if (file-exists-p lfile)
	  (let ((bfile (format nil "pvs-lib.~a" *pvs-binary-type*)))
	    (when (or (not (file-exists-p bfile))
		      (compiled-file-older-than-source? lfile bfile))
	      (handler-case
		  (progn (compile-file lfile)
			 (chmod "ug+w" (namestring bfile)))
		(error (cnd)
		  (pvs-message "Compilation error - ~a" cnd)
		  (pvs-message "Loading lib file ~a interpreted"
		    (shortname lfile))
		  (setq bfile nil))))
	    (pvs-message "Loading ~a..." (or bfile lfile))
	    (let ((*libloads* nil))
	      (handler-case
		  (progn (load (or bfile lfile))
			 (pvs-message "~a loaded" (or bfile lfile))
			 (cons lfile *libloads*))
		(error (cnd)
		  (pvs-message "Error loading ~a:~%  ~a"
		    (or bfile lfile) cnd)
		  (when bfile
		    (pvs-message "Trying source ~a:" lfile)
		    (handler-case
			(progn (load lfile)
			       (pvs-message "~a loaded" lfile)
			       (cons lfile *libloads*))
		      (error (cnd)
			(pvs-message "Error loading ~a:~%  ~a"
			  lfile cnd))))))
	      *libloads*))
	  ;; (pvs-message "~a not found in ~a"
	  ;;   lfile *default-pathname-defaults*)
	  ))))

(defun list-pvs-libraries ()
  (dolist (path *pvs-library-path*)
    (dolist (lib (directory path))
      (when (directory-p lib)
	(format t "~%~a/ - ~a" (file-namestring lib) lib)))))

;;; id to abspath
(defvar *pvs-library-alist*)

(defun pvs-library-alist ()
  (if (and (boundp '*pvs-library-alist*) *pvs-library-alist*)
      *pvs-library-alist*
      (setq *pvs-library-alist* (get-pvs-library-alist))))

(defun get-pvs-library-alist ()
  (let ((alist nil))
    (dolist (path *pvs-library-path*)
      (assert (directory-p path))
      (dolist (sdir (uiop:subdirectories path))
	(let* ((subdir (truename sdir))
	       (dname (or (pathname-name sdir)
			  (car (last (pathname-directory sdir))))))
	  (when (valid-pvs-id* dname)
	    (push (cons (intern dname :pvs) subdir) alist)))))
    ;; earlier paths in *pvs-library-path* shadow later ones.
    (nreverse alist)))

;;; This is provided for library support, expected to be used in
;;; pvs-lib.lisp files (or files loaded by them).  If the filestr
;;; begins with a ".", then it is relative to the
;;; *default-pathname-defaults*.
(defun libload (filestr)
  (declare (special libloads))
  (let* ((libloads (if (boundp 'libloads) libloads nil))
	 (lib (directory-namestring filestr))
	 (filename (file-namestring filestr))
	 (libpath (unless (or (string= lib "") (string= lib "./"))
		    (get-library-reference lib)))
	 (*default-pathname-defaults* (or (and libpath (pathname libpath))
					  *default-pathname-defaults*))
	 (file (namestring
		(merge-pathnames filename *default-pathname-defaults*))))
    (cond ((and (boundp 'libloads) (member file libloads :test #'string=))
	   (pvs-error "Error in libload"
	     (format nil "Detected circular calls to libload,~%check files in ~{~a~^, ~}"
	       libloads)))
	  (t (pvs-message "Loading file ~a" file)
	     (unwind-protect
		 (progn #+(and allegro (not pvs6) (not pvs7))
			(excl:set-case-mode :case-insensitive-lower)
			(multiple-value-bind (ignore error)
			    (ignore-errors (load file))
			  (declare (ignore ignore))
			  (cond (error
				 (pvs-message "Error loading ~a:~%  ~a"
				   filestr error))
				(t (pvs-message "~a loaded" filestr)
				   (push file *libloads*)))))
	       #+(and allegro (not pvs6) (not pvs7))
	       (excl:set-case-mode :case-sensitive-lower)
	       (add-lowercase-prover-ids))))))


(defun load-pvs-lib-emacs-file (lib-path force?)
  (declare (ignore force?))
  (when *pvs-emacs-interface*
    (let ((emacs-file (format nil "~a/pvs-lib.el" lib-path)))
      (when (file-exists-p emacs-file)
	;; Note that this is a noop if Emacs is not there
	(pvs-message "Loading prelude library Emacs file ~a" emacs-file)
	(let ((result (pvs-emacs-eval
		       (format nil "(load-pvs-lib-file ~s)" lib-path))))
	  (cond ((and (stringp result)
		      (string= result "t"))
		 (pvs-message "~a loaded" emacs-file)
		 (list emacs-file))
		(t (pvs-message "Error in loading ~a" emacs-file))))))))


(defun load-prelude-library-workspace (lib-path force?)
  "Within the workspace of lib-path, load the theories known in .pvscontext,
which is loaded into (current-pvs-context) by restore-context.  (cddr (current-pvs-context))
corresponds to the list of pvs-files which have at least been parsed at some
point."
  (assert (pathnamep lib-path))
  (unless (absolute-pathname-p lib-path)
    (setq lib-path (merge-pathnames lib-path)))
  (assert (not (file-equal lib-path *default-pathname-defaults*)))
  (unless (and (not force?) (prelude-library-loaded? lib-path))
    (let ((loaded-files nil)
	  (prelude-ctx (prelude-context *workspace-session*)))
      (with-workspace lib-path
	(pvs-message "Loading prelude library context from ~a..." lib-path)
	(cond ((cddr (current-pvs-context))
	       (dolist (ce (pvs-context-entries))
		 (unless (or force? (typechecked? (ce-file ce)))
		   (typecheck-file (ce-file ce) force? nil nil t)
		   (pushnew (ce-file ce) loaded-files :test #'string=)))
	       ;; First check that all current theories are typechecked
	       ;; see Bug # 894 (2005-01-27_ChrisGeorge)
	       (maphash
		#'(lambda (id th)
		    (unless (typechecked? th)
		      (type-error id
			"Error in loading prelude file ~a~a"
			lib-path (filename th))))
		(current-pvs-theories))
	       (dolist (ce (pvs-context-entries))
		 (dolist (te (ce-theories ce))
		   (let* ((id (te-id te))
			  (th (get-theory id)))
		     ;; extend (prelude-context *workspace-session*)
		     (setq prelude-ctx
			   (update-prelude-library-context th prelude-ctx))
		     (when (filename th)
		       (pushnew (filename th) loaded-files
				:test #'string=)))))
	       (if loaded-files
		   (pvs-message
		       "Loaded prelude library context from ~a~
                            ~%  and reset the context"
		     lib-path)
		   (pvs-message "Error loading prelude library context ~a~
                                     ~%  no pvs files loaded"
		     lib-path)))
	      (t (pvs-message "~a.pvscontext is empty~%  no PVS files loaded"
		   lib-path))))
      ;; Back to previous *workspace-session*
      ;; loaded-files and prelude-ctx have been updated
      (when (and loaded-files
		 (not *typechecking-module*)
		 (not *tc-add-decl*))
	(reset-workspace)
	(setf (pvs-context-changed *workspace-session*) t))
      (mapcar #'(lambda (file)
		  (namestring (make-pathname :name file :type "pvs"
					     :defaults lib-path)))
	loaded-files)
      (setf (prelude-context *workspace-session*) prelude-ctx)
      (when (member lib-path (prelude-libs *workspace-session*)
		    :test #'file-equal)
	(break "already there?"))
      (push lib-path (prelude-libs *workspace-session*))
      (assert (prelude-library-loaded? lib-path))
      loaded-files)))

(defun update-prelude-library-context (th pctx)
  (let* ((prel-ctx (or pctx (copy-context (saved-context th)))))
    (unless pctx
      ;; Make sure there is a dummy theory in the prelude context -
      ;; otherwise a theory that has formal parameters might be taken for
      ;; the context, and the formals could be visible outside.  In
      ;; particular, if the chosen theory imports judgement instances of its
      ;; parameters, those would be viewed as instantiated and appear in the
      ;; judgements-graph.
      (let ((nth (make-instance 'module
		   :id (gentemp "%dtheory")
		   :context-path (context-path th)
		   :exporting (make-instance 'exporting :kind 'default))))
	(setf (theory prel-ctx) nth)
	(setf (declaration prel-ctx) nil)
	(setf (theory-name prel-ctx) (make-theoryname nth))))
    (let* ((*current-context* prel-ctx)
	   (judgements (judgements prel-ctx))
	   (number-judgements-alist (number-judgements-alist judgements))
	   (name-judgements-alist (name-judgements-alist judgements))
	   (appl-judgements-alist (application-judgements-alist judgements)))
      (setf (number-judgements-alist judgements) nil)
      (setf (name-judgements-alist judgements) nil)
      (setf (application-judgements-alist judgements) nil)
      (merge-number-judgements number-judgements-alist
			       (number-judgements-alist judgements)
			       th (make-theoryname th))
      (merge-name-judgements name-judgements-alist
			     (name-judgements-alist judgements)
			     th (make-theoryname th))
      (merge-application-judgements appl-judgements-alist
				    (application-judgements-alist judgements)
				    th (make-theoryname th))
      (let* ((*current-context* prel-ctx)
	     (thname (make-theoryname th)))
	(update-current-context th thname)
	(dolist (decl (append (assuming th) (theory th)))
	  (when (and (declaration? decl) (visible? decl))
	    (put-decl decl)))
	(setf (get-importings th) (list thname))))
    prel-ctx))

(defun prelude-library-loaded? (lib-path)
  (member lib-path (current-prelude-libraries) :test #'file-equal))

;;; Called from restore-theories*; the dep-lib-ref comes from the .pvscontext
;;; So is always relative to the *default-pathname-defaults*.
(defun restore-imported-library-files (lib-id theory-ids)
  (let ((lib-path (get-library-path lib-id)))
    (unless lib-path (break "no lib-path?"))
    (unless (file-equal lib-path *default-pathname-defaults*)
      (with-workspace lib-path
	(let* ((theories (mapcan
	  		     #'(lambda (thid)
	  			 (unless (or (gethash thid (current-pvs-theories))
	  				     ;; From a theory declaration
	  				     (find #\. (string thid)))
	  			   (let ((th (with-no-type-errors
	  					 (get-typechecked-theory thid))))
	  			     (when th (list th)))))
	  		   theory-ids)))
	  (when (and theories
	  	     (every #'typechecked? theories))
	    theories))))))

;;; Load-imported-library loads imported libraries - called from
;;; get-parsed-theory when a library name is given in an IMPORTING clause.
(defun load-imported-library (lib-id theoryname)
  (assert (symbolp lib-id))
  (let ((lib-path (get-library-reference lib-id))
	(theory-name
	 (typecase theoryname
	   (modname theoryname)
	   (symbol (mk-modname theoryname))
	   (string (mk-modname (intern theoryname :pvs)))
	   (t (error
	       "Bad theoryname argument to load-imported-library")))))
    (assert (pathnamep lib-path))
    (load-library-theory lib-id lib-path theory-name)))

(defun load-library-theory (lib-id lib-path theory-name)
  (assert (and lib-id (symbolp lib-id)))
  (assert (pathnamep lib-path))
  (unless (absolute-pathname-p lib-path)
    (setq lib-path (merge-pathnames lib-path)))
  (assert (not (file-equal lib-path *default-pathname-defaults*)))
  (with-workspace lib-path
    (let* ((filename (or (context-file-of theory-name)
			 (if (file-exists-p (make-specpath theory-name))
			     (string (id theory-name))
			     (look-for-theory-in-directory-files
			      theory-name)))))
      (if filename
	  (multiple-value-bind (theories changed-theories)
	      (typecheck-file filename nil nil nil t)
	    (let ((theory (find theory-name theories :test #'same-id)))
	      (if theory
		  (let ((*current-context* (context theory)))
		    (dolist (cth changed-theories)
		      (untypecheck-usedbys cth))
		    (values theory theories))
		  (error "Theory ~a could  not be found in ~
                          the PVS context of library ~a: path ~a"
			 theory-name lib-id lib-path))))
	  (type-error theory-name
	    "Theory ~a ~_not found in the PVS context of library ~a: path ~a"
	    theory-name lib-id lib-path)))))

(defun parsed-library-file? (th)
  (assert (lib-datatype-or-theory? th))
  (if (assq th *theories-seen*)
      (cdr (assq th *theories-seen*))
      (with-workspace (context-path th)
	(let ((plf? (if (generated-by th)
			(let ((gth (get-theory (generated-by th))))
			  (and gth (parsed? gth)))
			(file-exists-p (pvs-file-path th)))))
	  (setq *theories-seen* (acons th plf? *theories-seen*))
	  plf?))))

(defun remove-prelude-library (lib)
  (let* ((lib-path (get-library-reference lib))
	 (entry (find lib-path (current-prelude-libraries) :test #'file-equal)))
    (if entry
	(setf (current-prelude-libraries) (delete entry (current-prelude-libraries)))
	(pvs-message "Library ~a is not loaded" lib))))

(defun list-prelude-libraries ()
  (if (current-prelude-libraries)
      (pvs-buffer "PVS Prelude Libraries"
	(format nil "Loaded prelude libraries and their associated files~%~
                     ~{~%~{~a~%  Full path: ~a~
                     ~@[~%  PVS files:~{~%    ~/pvs:fmt-prlib/~}~]~
                     ~@[~%  Lisp files:~{~%    ~a~}~]~
                     ~}~^~%~}"
	  (mapcar #'(lambda (lib-path)
		      (let ((ws (get-workspace-session lib-path)))
			(list (pvs-files ws) (lisp-files ws))))
	    (current-prelude-libraries)))
	t t)
      (pvs-message "No prelude-libraries found in the current context")))

(defun fmt-prlib (stream file&date &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~a" (file-namestring (car file&date))))

(defun prelude-libraries ()
  (let ((libs nil))
    (dolist (path (current-prelude-libraries))
      (push (list (get-library-id path) path) libs))
    libs))

(defun library-files ()
  (let ((files nil))
    (dolist (ws *all-workspace-sessions*)
      (maphash #'(lambda (file date&theories)
		   (pushnew (format nil "~a~a"
			      (pvs-file-path (cadr date&theories))
			      file)
			    files
			    :test #'equal))
	       (pvs-files ws)))
    (sort files #'string<)))

(defun library-theories ()
  (collect-theories t))

(defun current-libraries ()
  (let ((libs nil))
    (dolist (ws *all-workspace-sessions*)
      (break "fix this")
      (push (path ws) libs))
    (sort libs #'string<)))

(defun current-library-pathnames ()
  (let ((libs nil))
    (dolist (ws *all-workspace-sessions*)
      (push (path ws) libs))
    ;; (maphash #'(lambda (lib-id lib-contents)
    ;; 		 (declare (ignore lib-contents))
    ;; 		 (pushnew lib-id libs :test #'string=))
    ;; 	     (current-prelude-libraries))
    (mapcar #'get-library-reference (nreverse libs))))

(defun library-file? (pvsfile)
  (let* ((fdir (make-pathname :directory (pathname-directory pvsfile)))
	 (lib-path (get-library-reference fdir))
	 (file (pathname-name pvsfile))
	 (ext (pathname-type pvsfile))
	 (ws (when lib-path
	       (get-workspace-session lib-path))))
    (when (and ws file
	       (or (null ext) (string= ext "pvs")))
      (and (gethash file (pvs-files ws))
	   t))))


;;; There are thus three kinds of library references:
;;;   1. A libref, which is the string provided by the user.
;;;      Two different librefs may point to the same library.
;;;      If the libref is a simple name, e.g., "foo", then the reference
;;;      may be ambiguous; there are three possibilities:
;;;      a. There is a visible library declaration in the current context
;;;         of the form foo: LIBRARY = ...
;;;      b. There is a foo subdirectory of the current directory
;;;      c. There is a <PVS_LIBRARY_PATH>/foo directory.
;;;      Note that if the string ends in "/", then a. is not possible.
;;;      If the name starts with ".", "..", or "/" then it is a
;;;      relative or absolute pathname, and there is no ambiguity.

;;;      We return a canonical libkey; in case a. we use the canonical
;;;      lib-ref given in the library declaration, b. uses "./foo/", and
;;;      c. uses "foo/".

;;;   2. A libkey, which is a string that is the canonical libref
;;;      This is used to look up the library in hash tables, and to
;;;      store information in the .pvscontext and .bin files.
;;;      a. If the libref is to <PVS_LIBRARY_PATH>/foo, then this is "foo/"
;;;      b. This is a relative pathname if the directory is  a subdirectory
;;;         of the directory two levels above the current directory.
;;;         In this case the string always starts with "./" or "../"
;;;      c. It is a users home directory, in which case it starts with
;;;         "~/" or "~username/", depending on whether it is the current
;;;         users home directory.
;;;      d. Otherwise, it is an absolute pathname, starting with "/".

;;;   3. A libpath, which is an absolute pathname.

(defvar *pvs-lib-path* (directory-p (format nil "~a/lib/" *pvs-path*)))

(defun get-library-reference (libref)
  "Given a libref, attempts to return three values: an absolute lib-path, a
lib-id, and the origin.  Returns nil if a lib-path cannot be determined.  A
lib-path may be returned with a null lib-id.  The lib-ref is a symbol,
string, or pathname; typically the library id from an importing name, or the
string from the change-context command.

It first checks whether the libref is an existing directory.  If it is, then
it may be absolute or relative to the current workspace.  In addition, it
could be a valid library id, though it may have a '/' at the end when
associated with the PVS_LIBRARY_PATH (older .pvscontext files do this).  If
the libref has '/'s other than at the end, the last subdir is checked for
valid PVS id.

Note that if libref is an existing directory, it may shadow library-decls or
the PVS_LIBRARY_PATH.

absolute: try to find a corresponding library id:
          if *current-context* is set
              if there is a corresponding library-decl, use its id
                 origin is the library-decl
          elsif match in PVS_LIBRARY_PATH, and valid PVS id, use it
                 origin is :pvs-library-path
          else no lib-id or origin
relative: expand to absolute pathname for lib-path.  Origin is :relative.
          if a valid PVS id, use it else no lib-id
not a dir: if a valid id
              if *current-context* is set
                 if corresponding library-decl, use it's lib-path
                    origin is library-decl
              else if in PVS_LIBRARY_PATH, use lib-path
                      origin :pvs-library-path"
  (assert (typep libref '(or pathname symbol string)))
  (let* ((libstr (typecase libref
		   (symbol (string libref))
		   (pathname (namestring libref))
		   (t libref)))
	 (ws (find libref *all-workspace-sessions* :key #'path :test #'equal))
	 (dirp (if ws libref (directory-p libstr)))    ; directory exists
	 (lib-path (when dirp (merge-pathnames dirp))) ; get the absolute path
	 (nlibstr (if (char= (char libstr (1- (length libstr))) #\/)
		      (subseq libstr 0 (1- (length libstr)))
		      libstr))			  ; Remove trailing '/'
	 (pos (position #\/ nlibstr :from-end t)) ; position of last '/', if any
	 (idstr (if pos (subseq nlibstr (1+ pos)) nlibstr)) ; possible id is after the last '/'
	 (pid (when (valid-pvs-id* idstr) (intern idstr :pvs)))) ; is it a valid pvs id?
    (cond (dirp
	   (cond ((equal lib-path *pvs-lib-path*)
		  (values lib-path nil :prelude))
		 ((equal lib-path (path *workspace-session*))
		  (values lib-path nil :current-workspace))
		 ((rassoc pid (current-subdir-alist))
		  (values lib-path pid :subdirectory))
		 ((rassoc dirp (pvs-library-alist) :test #'equal)
		  (let ((lib-elt (rassoc dirp (pvs-library-alist) :test #'equal)))
		    (values lib-path (car lib-elt) :pvs-library-path)))
		 ((visible-lib-decl-id lib-path)
		  (let ((libid (visible-lib-decl-id lib-path)))
		    (values lib-path libid :Lib-decl)))
		 (t lib-path)))
	  (pos
	   ;;(pvs-message "directory ~a not found" libref)
	   nil)
	  (pid (let ((lib-path (visible-lib-decl-pathname pid)))
		 (if lib-path
		     (values lib-path pid :lib-decl)
		     (let ((lib-elt (assq pid (pvs-library-alist))))
		       (when lib-elt
			 (values (cdr lib-elt) pid :pvs-library-path)))))))))

(defmethod get-library-path ((mod datatype-or-module))
  (context-path mod))

(defmethod get-library-path ((res resolution))
  (get-library-path (module (declaration res))))

(defmethod get-library-path ((mn modname))
  (or (and (resolution mn)
	   (get-library-path (declaration mn)))
      (let ((th (get-theory mn)))
	(and th (get-library-path th)))))

(defmethod get-library-path (libref)
  (assert (typep libref '(or pathname symbol string)))
  (let* ((pstr (typecase libref
		 (symbol (string libref))
		 (pathname (namestring libref))
		 (t libref)))
	 (dirp (when (directory-p pstr) (truename pstr)))
	 (lib-path (when dirp (merge-pathnames dirp))))
    ;; dirp works for both absolute and relative pathnames Note that a
    ;; local subdirectory shadows a PVS_LIBRARY_PATH subdirectory of the
    ;; same name.
    (or lib-path
	(let ((nstr (when (stringp pstr)
		      (if (char= (char pstr (1- (length pstr))) #\/)
			  (subseq pstr 0 (1- (length pstr)))
			  pstr))))
	  (when (and (stringp nstr)
		     (valid-pvs-id* nstr))
	    (let ((lib-id (intern nstr :pvs)))
	      (or (visible-lib-decl-pathname lib-id)
		  (cdr (assq lib-id (pvs-library-alist))))))))))

(defmethod get-library-id ((mod datatype-or-module))
  (get-library-id (context-path mod)))

(defmethod get-library-id (libref)
  (nth-value 1 (get-library-reference libref)))

(defmethod get-library-id ((res resolution))
  (get-library-id (module (declaration res))))

(defmethod get-library-id ((mn modname))
  (or (library mn)
      (and (resolution mn)
	   (or (library (module-instance mn))
	       (get-library-id (declaration mn))))))

(defun visible-lib-decl-id (lib-path)
  (when (current-context)
    (let ((lib-decls (remove-if-not #'(lambda (ld)
					(file-equal (lib-ref ld) lib-path))
		       (all-visible-lib-decls))))
      (when lib-decls
	(if (cdr lib-decls)
	    (let* ((locdecl (car lib-decls))
		   (loc (locality locdecl)))
	      (dolist (ldecl (cdr lib-decls))
		(let ((lloc (locality ldecl)))
		  (when (< lloc loc)
		    (setq locdecl ldecl
			  loc lloc))))
	      (id locdecl))
	    (id (car lib-decls)))))))

(defun visible-lib-decl-pathname (lib-id)
  (when (current-context)
    (let ((lib-decls (remove-if-not #'lib-decl?
		    (get-declarations lib-id))))
      (when lib-decls
	(let* ((locdecl (car lib-decls))
	       (loc (locality locdecl)))
	  (dolist (ldecl (cdr lib-decls))
	    (let ((lloc (locality ldecl)))
	      (when (< lloc loc)
		(setq locdecl ldecl
		      loc lloc))))
	  (merge-pathnames (lib-ref locdecl)))))))

(defun all-visible-lib-decls ()
  ;; Skip the prelude
  (let ((dht (lhash-table (current-declarations-hash)))
	(lib-decls nil))
    (maphash #'(lambda (id decls)
		 (declare (ignore id))
		 (dolist (d decls)
		   (when (lib-decl? d)
		     (push d lib-decls))))
	     dht)
    lib-decls))

(defun find-lib-path-subdir (abspath
			     &optional
			       (lib-paths (cons *default-pathname-defaults*
						*pvs-library-path*)))
  "Looks through the current directory and the *pvs-library-path* for
abspath as a subdirectory.  If it is found, and is a valid library id, it is returned."
  (when lib-paths
    (let* ((subdir (subdirectoryp abspath (car lib-paths)))
	   (substr (when (and subdir (not (string= subdir "")))
		     (if (char= (char subdir 0) #\/)
			 (subseq subdir 1)
			 subdir))))
      (if (and substr
	       (valid-pvs-id* substr))
	  (intern substr :pvs)
	  ;; Recurse, may have both D and D/A in lib-paths, so D/A/B gives
	  ;; A/B in the first case, not valid id, but is in the second one.
	  (find-lib-path-subdir abspath (cdr lib-paths))))))
	    

(defun get-lib-decls (libid)
  (assert *current-context*)
  (assert (symbolp libid))
  (let ((lib-decls (remove-if (complement #'lib-decl?)
		     (get-declarations libid))))
    (remove-duplicates (sort (copy-list lib-decls) #'< :key #'locality)
      :test #'equal :key #'lib-ref :from-end t)))

(defun all-decls (theory)
  (or (all-declarations theory)
      (let ((decls (append (formals theory)
			   (mapcan #'(lambda (d)
				       (when (typep d '(or formal-subtype-decl
							formal-struct-subtype-decl))
					 (remove-if #'tcc? (generated d))))
			     (formals theory))
			   (when (recursive-type? theory)
			     (importings theory))
			   (theory-formal-decls theory)
			   (assuming theory)
			   (theory theory))))
	(when (memq 'typechecked (status theory))
	  (setf (all-declarations theory) decls))
	decls)))

(defun all-decls-but-theory-formals (theory)
  (append (formals theory)
	  (mapcan #'(lambda (d)
		      (when (typep d 'formal-subtype-decl)
			(remove-if #'tcc? (generated d))))
	    (formals theory))
	  (when (recursive-type? theory)
	    (importings theory))
	  (assuming theory)
	  (theory theory)))

(defun get-most-local-reference (list &optional (min 1)
				      most-local most-local-num)
  (if (null list)
      (id most-local)
      (let ((lnum (locality (car list))))
	(if (<= lnum min)
	    (id (car list))
	    (if (or (null most-local)
		    (< lnum most-local-num))
		(get-most-local-reference
		 (cdr list) min (car list) lnum)
		(get-most-local-reference
		 (cdr list) min most-local most-local-num))))))

(defun do-all-lib-theories (fn)
  (dolist (ws *all-workspace-sessions*)
    (maphash #'(lambda (id th)
		 (declare (ignore id))
		 (when (module? th)
		   (funcall fn th)))
	     (pvs-theories ws))))


;;; pvs-file-path is called from file-dependencies and
;;; parsed-library-file?  Given a theory, it constructs the pathname to
;;; the file containing the theory.  If it's in the current context, it is
;;; simply the file name.  Otherwise it is the full path, which should not
;;; be saved in the lib-ref of a lib-decl, nor as a depname in
;;; file-dependencies.

(defmethod pvs-file-path ((th datatype-or-module))
  (if (lib-datatype-or-theory? th)
      (format nil "~a/~a.pvs" (context-path th) (filename th))
      (format nil "~a.pvs" (filename th))))
