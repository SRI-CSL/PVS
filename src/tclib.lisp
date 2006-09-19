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

;;; Load-prelude initializes the *prelude*, *prelude-context*, and
;;; *prelude-theories* variables

(defparameter *prelude-filename* "prelude.pvs")

(defun load-prelude ()
  (assert *pvs-path*)
  (setq sbrt::*disable-caching* t)
  (setq *pvs-context* nil)
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
	*odd_int* nil)
  (when *pvs-initialized*
    (clear-theories t))
  (let ((cdir (or *pvs-context-path* (working-directory)))
	(*pvs-context-path* nil)
	(*pvs-modules* (make-hash-table :test #'eq :size 20 :rehash-size 10))
	(*pvs-files* (make-hash-table :test #'equal))
	(*loaded-libraries* (make-hash-table :test #'equal))
	(*prelude-libraries* (make-hash-table :test #'equal))
	(*imported-libraries* (make-hash-table :test #'equal))
	(*pvs-context* nil)
	(*loading-prelude* t)
	(*generate-tccs* 'all)
	(mods (parse :file (format nil "~a/lib/~a"
			     *pvs-path* *prelude-filename*))))
    (reset-typecheck-caches)
    (dolist (fn *load-prelude-hook*)
      (funcall fn))
    (setq *prelude-context* nil)
    (reset-equality-decl)
    (reset-if-declaration)
    (reset-boolean-aliases)
    (unwind-protect
	(progn
	  (set-working-directory (format nil "~a/lib/" *pvs-path*))
	  (setq *pvs-context-path* (working-directory))
	  (dolist (m mods)
	    (let ((*current-theory* m))
	      (format t "~%Typechecking ~a" (id m))
	      (setf (status m) '(parsed))
	      (setf (gethash (id m) *prelude*) m)
	      (typecheck m)
	      (let* ((tot (car (tcc-info m)))
		     (prv (cadr (tcc-info m)))
		     (mat (caddr (tcc-info m)))
		     (obl (- tot prv mat)))
		(if (zerop tot)
		    (format t "~%~a typechecked: No TCCs generated" (id m))
		    (format t "~%~a typechecked: ~d TCC~:p, ~
                               ~d proved, ~d subsumed, ~d unproved~
                               ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		      (id m) tot prv mat obl
		      (length (warnings m)) (length (info m)))))
	      ;; No need to have saved-context set for prelude contexts
	      (assert (typep m '(or datatype module)))
	      (setq *prelude-theories*
		    (nconc *prelude-theories* (cons m nil)))
	      (let* ((th (if (typep m 'datatype)
			     (adt-theory m)
			     m))
		     (ctx (saved-context th))
		     (ndecls-hash (copy (declarations-hash ctx)))
		     (nusing-hash (copy (using-hash ctx))))
		(typecase m
		  (datatype
		   (setf (get-importings (adt-theory m) nusing-hash)
			 (list (mk-modname (id (adt-theory m)))))
		   (when (adt-map-theory m)
		     (setf (get-importings (adt-map-theory m) nusing-hash)
			   (list (mk-modname (id (adt-map-theory m)))))
		     (dolist (decl (append (assuming (adt-map-theory m))
					   (theory (adt-map-theory m))))
		       (when (and (declaration? decl) (visible? decl))
			 (put-decl decl ndecls-hash))))
		   (when (adt-reduce-theory m)
		     (setf (get-importings (adt-reduce-theory m) nusing-hash)
			   (list (mk-modname (id (adt-reduce-theory m)))))
		     (dolist (decl (append (assuming (adt-reduce-theory m))
					   (theory (adt-reduce-theory m))))
		       (when (and (declaration? decl) (visible? decl))
			 (put-decl decl ndecls-hash)))))
		  (t (setf (get-importings m nusing-hash)
			   (list (mk-modname (id m))))))
		(maphash #'(lambda (id decls)
			     (setf (gethash id (lhash-table ndecls-hash))
				   (remove-if #'(lambda (d)
						  (typep d '(or formal-decl
								var-decl)))
				     decls)))
			 (lhash-table ndecls-hash))
		(let ((pusing-hash (copy nusing-hash)))
		  (setf (get-importings (theory ctx) pusing-hash)
			(list (theory-name ctx)))
		  (setq *prelude-context*
			(copy ctx
			  'declarations-hash ndecls-hash
			  'using-hash pusing-hash))))
	      (when (eq (id m) '|booleans|)
		(let ((*current-context* (saved-context m)))
		  (setq *true*
			(typecheck* (mk-name-expr 'TRUE) *boolean* nil nil))
		  (setq *false*
			(typecheck* (mk-name-expr 'FALSE) *boolean* nil
				    nil))))))
	  (format t "~%Done typechecking the prelude")
	  (restore-prelude-proofs))
      (set-working-directory cdir))))

(defun restore-prelude-proofs ()
  (let ((prfile (merge-pathnames (format nil "~a/lib/" *pvs-path*)
				 "prelude.prf")))
    (assert (file-exists-p prfile))
    (format t "~%Restoring the prelude proofs from ~a" prfile)
    (let ((proofs (read-pvs-file-proofs prfile)))
      (maphash #'(lambda (id theory)
		   (declare (ignore id))
		   (restore-proofs prfile theory proofs)
		   (mapc #'(lambda (decl)
			     (if (justification decl)
				 (setf (proof-status decl) 'proved)
				 ;;(break "No proof for ~a?" (id decl))
				 ))
			 (provable-formulas theory)))
	       *prelude*))))

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

(defun save-prelude-proofs ()
  (let ((prfile (namestring (merge-pathnames (format nil "~a/lib/" *pvs-path*)
					     "prelude.prf"))))
    (save-proofs prfile *prelude-theories*)))

(defun prove-prelude (&optional retry? use-default-dp?)
  (let ((theories *prelude-theories*)
	(*loading-prelude* t)
	(*proving-tcc* t))
    (prove-theories "prelude" theories retry? use-default-dp?)
    (prelude-summary)))

(defun prelude-summary ()
  (let ((theories *prelude-theories*))
    (pvs-buffer "PVS Status"
      (with-output-to-string (*standard-output*)
	(proof-summaries theories "prelude"))
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
;;;   *prelude-libraries* - library hashtable of preloaded libraries and
;;;                         prelude libraries that have been introduced
;;;                         in the current session.
;;;   *visible-libraries* - a subset of *prelude-libraries*; the
;;;                         libraries that have been made visible through
;;;                         the load-prelude-library command.
;;;   *visible-libraries-uselist* - used to quickly construct contexts
;;;   *imported-libraries* - library hashtable of libraries that have been
;;;                          explicitly imported.

;;; load-prelude-libraries is called from restore-context
(defun load-prelude-libraries (lib-refs)
  (dolist (lib-ref lib-refs)
    (multiple-value-bind (lib-path err-msg)
	(libref-to-pathname lib-ref)
      (if lib-path
	  (load-prelude-library lib-path nil t)
	  (pvs-error err-msg
	    (format nil
		"Library reference ~a not found~
                 ~%  check the PVS_LIBRARY_PATH environment variable and restart PVS,~
                 ~%  or run M-x remove-prelude-library and remove this library."
	      lib-ref))))))

(defvar *loading-libraries* nil)

;;; This is called from the Emacs load-prelude-library command as well.
;;; Loads the .pvscontext, associated PVS files, pvs-lib.lisp file
;;; (and any libloaded file), and pvs-lib.el.  Note that pvs-strategies is
;;; not loaded here, but when the prover is invoked.
(defun load-prelude-library (lib &optional force? quiet?)
  (multiple-value-bind (lib-ref lib-path err-msg)
      (get-prelude-library-refs lib)
    (cond (err-msg
	   (pvs-message err-msg))
	  ((and (not force?)
		(prelude-library-loaded? lib-ref))
	   (unless quiet?
	     (pvs-message "Prelude library ~a is already loaded." lib)))
	  ((member lib-ref *loading-libraries* :test #'string=)
	   (pvs-error "Load Prelude Error"
	     "Detected circular calls to load-prelude-library,~%~
              check pvs-lib.lisp files in ~{~a~^, ~}"
	     *loading-libraries*))
	  (t (let ((*loading-libraries* (cons lib-ref *loading-libraries*))
		   (pvs-files-loaded
		    (load-prelude-library-context lib-ref lib-path))
		   (lisp-files-loaded (load-pvs-lib-lisp-file lib-path))
		   (emacs-files-loaded (load-pvs-lib-emacs-file lib-path)))
	       (when (or pvs-files-loaded
			 lisp-files-loaded
			 emacs-files-loaded)
		 (unless (member lib-ref (cadr *pvs-context*) :test #'string=)
		   (if (cdr *pvs-context*)
		       (push lib-ref (cadr *pvs-context*))
		       (nconc *pvs-context* (list (list lib-ref)))))
		 (update-prelude-libraries-files
		  lib-ref lib-path
		  pvs-files-loaded lisp-files-loaded emacs-files-loaded)
		 t))))))

(defun update-prelude-libraries-files (lib-ref lib-path pvs-files-loaded
					       lisp-files-loaded
					       emacs-files-loaded)
  (flet ((fdates (files)
		 (mapcar #'(lambda (file)
			     (cons file (file-write-date file)))
		   files)))
    (let ((oldentry (assoc lib-ref *prelude-libraries-files* :test #'string=))
	  (newentry (list lib-path
			  (fdates pvs-files-loaded)
			  (fdates lisp-files-loaded)
			  (fdates emacs-files-loaded))))
      (if oldentry
	  (setf (cdr oldentry) newentry)
	  (setf *prelude-libraries-files*
		(acons lib-ref newentry *prelude-libraries-files*))))))

(defun get-prelude-library-refs (lib)
  (let ((libstr (if (char= (char lib (- (length lib) 1)) #\/)
		    lib
		    (concatenate 'string lib "/"))))
    (if (file-exists-p libstr)
	(values (pathname-to-libref libstr) libstr)
	(let ((lib-path (pvs-library-path-ref libstr)))
	  (if lib-path
	      (values libstr lib-path)
	      (values nil nil
		      (format nil "Prelude library ~a not found" lib)))))))

(defvar *libloads* nil)

(defun load-pvs-lib-lisp-file (lib-path)
  ;; Set up *default-pathname-defaults* and sys:*load-search-list*
  ;; so that simple loads from the pvs-lib.lisp file work.
  (let* ((*default-pathname-defaults* (merge-pathnames lib-path))
	 #+allegro (sys:*load-search-list* *pvs-library-path*)
	 (lfile (format nil "~apvs-lib.lisp" lib-path))
	 (*suppress-printing* t))
    (when (file-exists-p lfile)
      (let ((bfile (format nil "~apvs-lib.~a" lib-path *pvs-binary-type*)))
	(when (or (not (file-exists-p bfile))
		  (compiled-file-older-than-source? lfile bfile))
	  (multiple-value-bind (ignore error)
	      (ignore-errors (compile-file lfile))
	    (declare (ignore ignore))
	    (cond (error
		   (pvs-message "Compilation error - ~a" error)
		   (pvs-message "Loading lib file ~a interpreted"
		     (shortname lfile))
		   (setq bfile nil))
		  (t
		   (chmod "ug+w" (namestring bfile))))))
	(pvs-message "Loading ~a..." (or bfile lfile))
	(let ((*libloads* nil))
	  (multiple-value-bind (ignore error)
	      (ignore-errors (load (or bfile lfile)))
	    (declare (ignore ignore))
	    (cond (error
		   (pvs-message "Error loading ~a:~%  ~a"
		     (or bfile lfile) error)
		   (when bfile
		     (pvs-message "Trying source ~a:" lfile)
		     (multiple-value-bind (ignore lerror)
			 (ignore-errors (load lfile))
		       (declare (ignore ignore))
		       (cond (lerror
			      (pvs-message "Error loading ~a:~%  ~a"
				lfile error))
			     (t (pvs-message "~a loaded" lfile)
				(cons lfile *libloads*))))))
		  (t (pvs-message "~a loaded" (or bfile lfile))
		     (cons lfile *libloads*)))))))))

(defun list-pvs-libraries ()
  (dolist (path *pvs-library-path*)
    (dolist (lib (directory path))
      (when (directory-p lib)
	(format t "~%~a/ - ~a" (file-namestring lib) lib)))))


;;; This is provided for library support, expected to be used in
;;; pvs-lib.lisp files (or files loaded by them).  If the filestr
;;; begins with a ".", then it is relative to the
;;; *default-pathname-defaults*.
(defun libload (filestr)
  (declare (special libloads))
  (let* ((libloads (if (boundp 'libloads) libloads nil))
	 (lib (directory-namestring filestr))
	 (filename (file-namestring filestr))
	 (libpath (unless (string= lib "./") (libref-to-pathname lib)))
	 (*default-pathname-defaults* (or libpath
					  *default-pathname-defaults*))
	 (file (namestring
		(merge-pathnames filename *default-pathname-defaults*))))
    (cond ((and (boundp 'libloads) (member file libloads :test #'string=))
	   (pvs-error "Error in libload"
	     "Detected circular calls to libload,~%check files in ~{~a~^, ~}"
	     libloads))
	  (t (pvs-message "Loading file ~a" file)
	     (unwind-protect
		 (progn #+allegro (excl:set-case-mode :case-insensitive-lower)
			(multiple-value-bind (ignore error)
			    (ignore-errors (load filename))
			  (declare (ignore ignore))
			  (cond (error
				 (pvs-message "Error loading ~a:~%  ~a"
				   filestr error))
				(t (pvs-message "~a loaded" filestr)
				   (push file *libloads*)))))
	       #+allegro (excl:set-case-mode :case-sensitive-lower)
	       (add-lowercase-prover-ids))))))


(defun load-pvs-lib-emacs-file (lib-path)
  (when *pvs-emacs-interface*
    (let ((emacs-file (concatenate 'string lib-path "pvs-lib.el")))
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


(defun load-prelude-library-context (lib-ref lib-path)
  (let ((loaded-files nil))
    (with-pvs-context lib-ref
      (let ((*current-theory* *current-theory*)
	    (*pvs-modules* (make-hash-table :test #'eq))
	    (*pvs-files* (make-hash-table :test #'equal))
	    (*pvs-context-writable* (write-permission? *pvs-context-path*))
	    (*pvs-context-changed* nil))
	(pvs-message "Loading prelude library context from ~a..." lib-path)
	(restore-context)
	(cond ((cddr *pvs-context*)
	       (unwind-protect
		   (progn
		     (dolist (ce (pvs-context-entries))
		       (typecheck-file (ce-file ce) nil nil nil t))
		     (save-context)
		     ;; We have to do the generated-by theories first, or
		     ;; the typechecked? test fails (Bug # 894)
		     (maphash
		      #'(lambda (id th)
			  (when (generated-by th)
			    (cond ((typechecked? th)
				   (change-to-library-class th lib-ref)
				   (update-prelude-library-context th)
				   (when (filename th)
				     (pushnew (filename th) loaded-files
					      :test #'string=)))
				  (t (type-error id
				       "Error in loading prelude file ~a~a"
				       lib-path (filename th))))))
		      *pvs-modules*)
		     (maphash
		      #'(lambda (id th)
			  (unless (generated-by th)
			    (cond ((typechecked? th)
				   (change-to-library-class th lib-ref)
				   (update-prelude-library-context th)
				   (when (filename th)
				     (pushnew (filename th) loaded-files
					      :test #'string=)))
				  (t (type-error id
				       "Error in loading prelude file ~a~a"
				       lib-path (filename th))))))
		      *pvs-modules*)
		     (setf (gethash lib-ref *prelude-libraries*)
			   (list *pvs-files* *pvs-modules*)))
		 (maphash #'(lambda (thid theory)
			      (unless (library-datatype-or-theory? theory)
				(remhash thid *pvs-modules*)
				(remhash (filename theory) *pvs-files*)
				(setf loaded-files
				      (remove (filename theory)
					      loaded-files
					      :test #'string=))))
			  *pvs-modules*))
	       (if loaded-files
		   (pvs-message
		       "Loaded prelude library context from ~a~
                            ~%  and reset the context"
		     lib-path)
		   (pvs-message "Error loading prelude library context ~a~
                                     ~%  no pvs files loaded"
		     lib-path)))
	      (t (pvs-message lib-path
		   "~a.pvscontext is empty~%  no PVS files loaded"
		   lib-path)))))
    (add-to-prelude-libraries lib-ref)
    (when (and loaded-files
	       (not *typechecking-module*)
	       (not *tc-add-decl*))
      (reset-context)
      (setq *pvs-context-changed* t))
    (mapcar #'(lambda (file)
		(namestring (make-pathname :name file :type "pvs"
					   :defaults lib-path)))
      loaded-files)))

(defun add-to-prelude-libraries (lib-ref)
  (setf (gethash lib-ref *loaded-libraries*)
	(gethash lib-ref *prelude-libraries*))
  (setq *prelude-libraries-uselist*
	(let ((uselist nil))
	  (maphash #'(lambda (lref files&theories)
		       (declare (ignore lref))
		       (maphash #'(lambda (id theory)
				    (push (list theory (mk-modname id))
					  uselist))
				(cadr files&theories)))
		   *prelude-libraries*)
	  uselist)))

(defun prelude-libraries-uselist ()
  *prelude-libraries-uselist*)

(defun update-prelude-library-context (th)
  (unless *prelude-library-context*
    (setq *prelude-library-context* (copy-context (saved-context th))))
  (unless (eq (theory *prelude-library-context*) th)
    (setf (theory *prelude-library-context*) th)
    (setf (theory-name *prelude-library-context*) (mk-modname (id th)))
    (let* ((*current-theory* th)
	   (*current-context* *prelude-library-context*)
	   (judgements (judgements *prelude-library-context*))
	   (number-judgements-alist (number-judgements-alist judgements))
	   (name-judgements-alist (name-judgements-alist judgements))
	   (appl-judgements-alist (application-judgements-alist judgements)))
      (setf (number-judgements-alist judgements) nil)
      (setf (name-judgements-alist judgements) nil)
      (setf (application-judgements-alist judgements) nil)
      (merge-number-judgements number-judgements-alist
			       (number-judgements-alist judgements)
			       th (mk-modname (id th)))
      (merge-name-judgements name-judgements-alist
			     (name-judgements-alist judgements)
			     th (mk-modname (id th)))
      (merge-application-judgements appl-judgements-alist
				    (application-judgements-alist judgements)
				    th (mk-modname (id th)))))
  (let* ((*current-theory* th)
	 (*current-context* *prelude-library-context*)
	 (thname (mk-modname (id th))))
    (update-current-context th thname)
    (dolist (decl (append (assuming th) (theory th)))
      (when (and (declaration? decl) (visible? decl))
	(put-decl decl)))
    (setf (get-importings th) (list thname))))



(defun prelude-library-loaded? (lib)
  (assoc lib *prelude-libraries-files* :test #'string=))


;;; Called from restore-theories*; the dep-lib-ref comes from the .pvscontext
;;; So is always relative to the *pvs-context-path*.
(defun restore-imported-library-files (dep-lib-ref theory-ids)
  (let ((lib-ref (get-relative-library-reference
		  (get-library-reference dep-lib-ref)))
	(orig-context-path *pvs-context-path*))
    (multiple-value-bind (lib-path err-msg)
	(libref-to-pathname lib-ref)
      (when err-msg
	(type-error dep-lib-ref err-msg))
      (unless (or (gethash lib-ref *prelude-libraries*)
		  (file-equal lib-path *pvs-context-path*))
	(with-pvs-context lib-ref
	  (restore-context)
	  (multiple-value-bind (*pvs-files* *pvs-modules*)
	      (get-imported-files-and-theories lib-ref)
	    (unless (every #'(lambda (thid)
			       (gethash thid *pvs-modules*))
			   theory-ids)
	      (relativize-imported-libraries
	       lib-ref orig-context-path
	       (let* ((*prelude-libraries*
		       (make-hash-table :test #'equal))
		      (theories (mapcan
				    #'(lambda (thid)
					(unless (or (gethash thid *pvs-modules*)
						    (find #\. (string thid)))
					  (list (get-typechecked-theory
						 thid))))
				  theory-ids)))
		 (when (and theories
			    (every #'typechecked? theories))
		   (save-context)
		   (dolist (th theories)
		     (unless (library-datatype-or-theory? th)
		       (change-to-library-class th lib-ref)
		       (setf (all-imported-theories th) 'unbound)))
		   theories))))))))))

;;; Load-imported-library loads imported libraries - called from
;;; get-parsed-theory when a library name is given in an IMPORTING clause.
(defun load-imported-library (lib-id theoryname)
  (assert (symbolp lib-id))
  (multiple-value-bind (lib-ref err-msg)
      (get-library-reference lib-id) 
    (if err-msg
	(type-error theoryname err-msg)
	(let ((theory-name
	       (typecase theoryname
		 (modname theoryname)
		 (symbol (mk-modname theoryname))
		 (string (mk-modname (intern theoryname)))
		 (t (error
		     "Bad theoryname argument to load-imported-library")))))
	  (load-library-theory lib-ref theory-name)))))

(defun load-library-theory (lib-ref theory-name)
  (if (or (gethash lib-ref *prelude-libraries*)
	  (file-equal (libref-to-pathname lib-ref) *pvs-context-path*))
      (get-typechecked-theory (copy theory-name 'library nil))
      (let ((value nil)
	    (changed-theories nil)
	    (orig-context-path *pvs-context-path*))
	(with-pvs-context lib-ref
	  (let ((*current-theory* *current-theory*)
		(*pvs-context-writable*
		 (write-permission? *pvs-context-path*))
		(*pvs-context-changed* nil))
	    (restore-context)
	    (multiple-value-bind (*pvs-files* *pvs-modules*)
		;; This initializes *imported-libraries* for lib-ref
		(get-imported-files-and-theories lib-ref)
	      ;;; Reset *imported-libraries* here
	      (relativize-imported-libraries
	       lib-ref orig-context-path
	       (let* ((*prelude-libraries* (make-hash-table :test #'equal))
		      (filename (context-file-of theory-name)))
		 (unless filename
		   (if (file-exists-p (make-specpath theory-name))
		       (setq filename (string (id theory-name)))
		       (setq filename (look-for-theory-in-directory-files
				       theory-name))))
		 (if filename
		     (unwind-protect
			 (multiple-value-bind (theories changed)
			     (typecheck-file filename nil nil nil t)
			   (setq changed-theories changed)
			   (let ((theory (find theory-name theories
					       :test #'same-id)))
			     (cond (theory
				    (let ((*current-context* (context theory))
					  (*current-theory* theory))
				      (save-context))
				    (let ((untcs nil))
				      (maphash
				       #'(lambda (id th)
					   (declare (ignore id))
					   (unless (typechecked? th)
					     (push th untcs)))
				       *pvs-modules*)
				      (maphash
				       #'(lambda (id th)
					   (declare (ignore id))
					   (unless (or (memq th untcs)
						       (library-datatype-or-theory? th))
					     (change-to-library-class
					      th lib-ref)))
				       *pvs-modules*))
				    (setq value theory))
				   (t (setq value
					    (format nil
						"Theory ~a could  not be found in ~
                                          the PVS context of library ~a"
					      theory-name lib-ref))))))
		       (maphash #'(lambda (thid theory)
				    (unless (library-datatype-or-theory?
					     theory)
				      (remhash thid *pvs-modules*)
				      (remhash (filename theory)
					       *pvs-files*)))
				*pvs-modules*))
		     (setq value
			   (format nil
			       "Theory ~a ~_not found in the PVS context of ~
                                library ~a"
			     theory-name lib-ref))))))))
	(dolist (cth changed-theories)
	  (untypecheck-usedbys cth))
	(if (stringp value)
	    (type-error theory-name value)
	    (progn (assert (library-datatype-or-theory? value))
		   (pvs-file-path value)
		   value)))))


(defun parsed-library-file? (th)
  (let ((lib (lib-ref th)))
    (and lib
	 (let* ((impfiles (car (gethash lib *imported-libraries*)))
		(prefiles (car (gethash lib *prelude-libraries*)))
		(*pvs-files* (cond ((and (hash-table-p impfiles)
					 (gethash (filename th) impfiles))
				    impfiles)
				   ((hash-table-p prefiles)
				    prefiles)
				   (t *pvs-files*))))
	   (if (generated-by th)
	       (let ((gth (get-theory* (generated-by th) lib)))
		 (and gth
		      (parsed? gth)))
	       (and (filename th)
		    (let ((path (probe-file (pvs-file-path th))))
		      (parsed?* path))))))))

(defun remove-prelude-library (lib)
  (multiple-value-bind (lib-ref lib-path err-msg)
      (get-prelude-library-refs lib)
    (declare (ignore lib-path))
    (if err-msg
	(pvs-message err-msg)
	(let ((libhash (gethash lib-ref *prelude-libraries*)))
	  (cond (libhash
		 (remhash lib-ref *prelude-libraries*)
		 (setq *prelude-libraries-files*
		       (remove (assoc lib-ref *prelude-libraries-files*
				      :test #'string=)
			       *prelude-libraries-files*))
		 (maphash #'(lambda (id theory)
			      (declare (ignore id))
			      (setf *prelude-libraries-uselist*
				    (delete (assq theory
						  *prelude-libraries-uselist*)
					    *prelude-libraries-uselist*)))
			  (cadr libhash))
		 (setq *pvs-context-changed* t)
		 (reset-context)
		 (setf (cadr *pvs-context*)
		       (remove lib-ref (cadr *pvs-context*) :test #'string=))
		 (pvs-message "Library ~a has been removed, and the context reset"
		   lib-ref))
		(t (pvs-message "Library ~a is not loaded" lib-ref)))))))

(defun list-prelude-libraries ()
  (if *prelude-libraries-files*
      (pvs-buffer "PVS Prelude Libraries"
	(format nil "Loaded prelude libraries and their associated files~%~
                     ~{~%~{~a~%  Full path: ~a~
                     ~@[~%  PVS files:~{~%    ~/pvs:fmt-prlib/~}~]~
                     ~@[~%  Lisp files:~{~%    ~/pvs:fmt-prlib/~}~]~
                     ~@[~%  Emacs files:~{~%    ~/pvs:fmt-prlib/~}~]~
                     ~}~^~%~}"
	  *prelude-libraries-files*)
	t t)
      (pvs-message "No prelude-libraries found in the current context")))

(defun fmt-prlib (stream file&date &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~a" (file-namestring (car file&date))))

(defun prelude-libraries ()
  (let ((libs nil))
    (maphash #'(lambda (lib theories)
		 (declare (ignore theories))
		 (push (list lib (libref-to-pathname lib)) libs))
	     *prelude-libraries*)
    libs))

(defun library-files ()
  (let ((files nil))
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (maphash #'(lambda (file date&theories)
			      (pushnew (format nil "~a~a"
					 (pvs-file-path (cadr date&theories))
					 file)
				       files
				       :test #'equal))
			  (car files&theories)))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (maphash #'(lambda (file date&theories)
			      (pushnew (format nil "~a~a"
					 (pvs-file-path (cadr date&theories))
					 file)
				       files
				       :test #'equal))
			  (car files&theories)))
	     *prelude-libraries*)
    (sort files #'string<)))

(defun library-theories ()
  (let ((theories nil))
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (maphash #'(lambda (file date&theories)
			      (dolist (th (cdr date&theories))
				(push (list (format nil "~a~a"
					      (pvs-file-path th) (id th))
					    (format nil "~a~a"
					      (pvs-file-path th) file)
					    (place-list (place th)))
				      theories)))
			  (car files&theories)))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (maphash #'(lambda (file date&theories)
			      (dolist (th (cdr date&theories))
				(push (list (format nil "~a~a"
					      (pvs-file-path th) (id th))
					    (format nil "~a~a"
					      (pvs-file-path th) file)
					    (place-list (place th)))
				      theories)))
			  (car files&theories)))
	     *prelude-libraries*)
    (sort theories #'string< :key #'car)))

(defun current-libraries ()
  (let ((libs nil))
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore files&theories))
		 (pushnew (namestring lib) libs :test #'equal))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore files&theories))
		 (pushnew (namestring lib) libs :test #'equal))
	     *prelude-libraries*)
    (sort libs #'string<)))

(defun current-library-pathnames ()
  (let ((libs nil))
    (maphash #'(lambda (lib-ref files&theories)
		 (declare (ignore files&theories))
		 (pushnew lib-ref libs :test #'string=))
	     *imported-libraries*)
    (maphash #'(lambda (lib-ref files&theories)
		 (declare (ignore files&theories))
		 (pushnew lib-ref libs :test #'string=))
	     *prelude-libraries*)
    (mapcar #'libref-to-pathname (nreverse libs))))

(defun library-file? (pvsfile)
  (let* ((lib-ref (pathname-to-libref (pathname-directory pvsfile)))
	 (file (pathname-name pvsfile))
	 (ext (pathname-type pvsfile))
	 (hash (when lib-ref
		 (car (or (gethash lib-ref *imported-libraries*)
			  (gethash lib-ref *prelude-libraries*))))))
    (when (and hash
	       file
	       (or (null ext) (string= ext "pvs"))
      (and (gethash file hash)
	   t)))))

(defun get-imported-files-and-theories (lib-ref)
  (get-library-files-and-theories lib-ref *imported-libraries*))

(defun get-library-files-and-theories (lib-ref libhash)
  (let ((files&theories (gethash lib-ref libhash)))
    (if files&theories
	(values-list files&theories)
	(let ((fileshash (make-hash-table :test #'equal))
	      (theorieshash (make-hash-table
			     :test #'eq :size 20 :rehash-size 10)))
	  (setf (gethash lib-ref libhash) (list fileshash theorieshash))
	  (values fileshash theorieshash)))))


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

;;; Get-library-pathname takes a libref (e.g., the fset in
;;; fset@finite_sets[int] or the string provided in a lib-decl) and a flag
;;; indicating whether to use the libref as a lib-decl reference.  It
;;; returns three values: the libref, libname, and libpath.  It first
;;; checks for a library declaration of the same name, and returns the
;;; associated path if it is found, otherwise it checks for the name as
;;; part of the current directory, then the default library.  If a library
;;; is not found, it returns a third value indicating the problem with the
;;; library.

(defmethod get-library-reference ((libstr string))
  ;; libstr is the string in a lib-decl, and must be a valid directory
  ;; Once validated, a library reference is returned, relativized if possible.
  (let ((dirstr (if (char= (char libstr (1- (length libstr))) #\/)
		    libstr
		    (concatenate 'string libstr "/"))))
    (cond ((member (char dirstr 0) '(#\/ #\~) :test #'char=)
	   (if (file-exists-p dirstr)
	       (pathname-to-libref dirstr)
	       (values nil (format nil "Directory ~a does not exist" libstr))))
	  ((char= (char dirstr 0) #\.)
	   (if (file-exists-p (merge-pathnames dirstr *pvs-context-path*))
	       (relative-path (merge-pathnames dirstr *pvs-context-path*)
			      *pvs-context-path*)
	       (values nil (format nil "Directory ~a does not exist" libstr))))
	  (t (let* ((csubdir (format nil "./~a" dirstr))
		    (csubref (when (file-exists-p
				    (merge-pathnames csubdir
						     *pvs-context-path*))
			       csubdir))
		    (lsubref (when (some #'(lambda (lpath)
					     (file-exists-p
					      (concatenate 'string
						lpath "/" dirstr)))
					 *pvs-library-path*)
			       libstr)))
	       (or csubref
		   lsubref
		   (values nil
			   (format nil
			       "Directory ~a does not exist" libstr))))))))


(defmethod get-library-reference ((libid symbol))
  ;; libid is the library part of a name, e.g., the foo in foo@bar.x
  ;; Must be a reference to a lib-decl, a subdirectory of the current context,
  ;; or a library in *pvs-path*/lib/
  (assert *current-context*)
  (or (cdr (assq libid (library-alist *current-context*)))
      (let* ((lib-decls (get-lib-decls libid))
	     (librefs (mapcar #'get-library-reference lib-decls))
	     (csubdir (format nil "./~a/" libid))
	     (csubref (when (and (not (member csubdir librefs :test #'string=))
				 (file-exists-p
				  (merge-pathnames csubdir
						   *pvs-context-path*)))
			csubdir))
	     (lsubref (when (some #'(lambda (lpath)
				      (file-exists-p (concatenate 'string
						       lpath "/"
						       (string libid))))
				  *pvs-library-path*)
			(concatenate 'string (string libid) "/")))
	     (refs (nconc librefs
			  (when csubref (list csubref))
			  (when lsubref (list lsubref)))))
	(when refs
	  (push (cons libid (car refs)) (library-alist *current-context*)))
	(or (car refs)
	    (values nil (format nil "Library id ~a cannot be resolved to a library declaration, a subdirectory of the current context, a subdirectory of PVS_LIBRARY_PATH, nor a subdirectory of ~a/lib/"
			  libid *pvs-path*))))))

(defmethod get-library-reference ((ldecl lib-decl))
  (if (not (library-datatype-or-theory? (module ldecl)))
      (lib-ref ldecl)
      (let* ((ldecl-lib-ref (lib-ref ldecl)))
	(cond ((member (char ldecl-lib-ref 0) '(#\/ #\~))
	       ldecl-lib-ref)
	      ((char= (char ldecl-lib-ref 0) #\.)
	       (let ((cpath (merge-pathnames (lib-ref (module ldecl))
						 *pvs-context-path*)))
		 (relative-path (merge-pathnames ldecl-lib-ref cpath)
				*pvs-context-path*)))
	      (t ldecl-lib-ref)))))

;;; Given a lib-ref from a separate library (that is relative to the
;;; *pvs-context-path*), returns it (old function - should remove).
(defun get-relative-library-reference (lib-ref)
  lib-ref)

(defun get-lib-decls (libid)
  (assert *current-context*)
  (assert (symbolp libid))
  (let ((lib-decls (remove-if (complement #'lib-decl?)
		     (get-declarations libid))))
    (remove-duplicates (sort (copy-list lib-decls) #'< :key #'locality)
      :test #'equal :key #'lib-ref :from-end t)))

;;; Called by restore-theories* to clean up saved dependencies, which
;;; used to save absolute pathnames.
(defun get-library-file-reference (dep-file-ref)
  (let ((dir (libref-directory dep-file-ref))
	(file (pathname-name dep-file-ref)))
    (concatenate 'string (get-library-reference dir) file)))
  

(defun all-decls (theory)
  (or (all-declarations theory)
      (let ((decls (append (formals theory)
			   (mapcan #'(lambda (d)
				       (when (typep d 'formal-subtype-decl)
					 (remove-if #'tcc? (generated d))))
			     (formals theory))
			   (when (recursive-type? theory)
			     (importings theory))
			   (assuming theory)
			   (theory theory))))
	(when (memq 'typechecked (status theory))
	  (setf (all-declarations theory) decls))
	decls)))


;;; lib-ref in this case is always relative to the pvs-context-path
(defun libref-to-pathname (lib-ref)
  (assert (stringp lib-ref))
  (assert (char= (char lib-ref (- (length lib-ref) 1)) #\/))
  (let ((lib-path (cond ((member (char lib-ref 0) '(#\~ #\/) :test #'char=)
			 ;; It's already a full pathname
			 lib-ref)
			((char= (char lib-ref 0) #\.)
			 (namestring
			  (merge-pathnames lib-ref
					   *pvs-context-path*)))
			(t 
			 ;; Otherwise it's a PVS library ref
			 ;; (e.g., finite_sets) in the PVS_LIBRARY_PATH
			 (pvs-library-path-ref lib-ref)))))
    (if (file-exists-p lib-path)
	lib-path
	(values nil (format nil "Library ~a does not exist" lib-ref)))))

(defun pvs-library-path-ref (lib-ref &optional (libs *pvs-library-path*))
  (let ((lib-path (cdr (assoc lib-ref *pvs-library-ref-paths*
			      :test #'string=))))
    (or lib-path
	(when libs
	  (let ((lib-path (concatenate 'string (car libs) lib-ref)))
	    (cond ((file-exists-p lib-path)
		   (push (cons lib-ref lib-path) *pvs-library-ref-paths*)
		   lib-path)
		  (t (pvs-library-path-ref lib-ref (cdr libs)))))))))

(defun pathname-to-libref (lib-path)
  (if (and (file-exists-p lib-path)
	   (directory-p lib-path))
      (let* ((lib-string (if (stringp lib-path)
			     lib-path
			     (namestring lib-path)))
	     (lib-string/ (if (char= (char lib-string
					   (- (length lib-string) 1))
				     #\/)
			      lib-string
			      (concatenate 'string lib-string "/")))
	     (dir-list (pathname-directory lib-string/)))
	(if (and (> (length dir-list) 2)
		 ;;(string= (car (last (butlast dir-list))) "lib")
		 ;;(file-equal (make-pathname :directory (butlast (butlast dir-list)))
			;;     *pvs-path*)
		 (let ((dpath (make-pathname :directory (butlast dir-list))))
		   (some #'(lambda (libpath) (file-equal dpath libpath))
			 *pvs-library-path*)))
	    ;; We match a PVS library
	    (concatenate 'string (car (last dir-list)) "/")
	    ;; Otherwise, try for a relative library
	    (relative-path lib-string/)))
      (values nil (format nil "~a does not exist" lib-path))))

;;; This function is goes from a lib-ref to a library id, suitable for
;;; building a theory name.
(defun libref-to-libid (lib-ref)
  (assert *current-context*)
  (car (rassoc lib-ref (library-alist *current-context*)
	       :test #'string=)))

(defun libref-to-lib-decl-id (lib-ref)
  (let ((lib-decls nil))
    (do-all-declarations #'(lambda (d)
			     (when (and (lib-decl? d)
					(string= lib-ref (lib-ref d)))
			       (push d lib-decls))))
    (when lib-decls
      (get-most-local-reference lib-decls))))

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
		 

(defun get-all-lib-decls ()
  (let ((lib-decls nil))
    (do-all-declarations #'(lambda (decl)
			     (when (lib-decl? decl)
			       (push decl lib-decls))))
    lib-decls))

;;; pvs-file-path is called from file-dependencies and
;;; parsed-library-file?  Given a theory, it constructs the pathname to
;;; the file containing the theory.  If it's in the current context, it is
;;; simply the file name.  Otherwise it is the full path, which should not
;;; be saved in the lib-ref of a lib-decl, nor as a depname in
;;; file-dependencies.

(defmethod pvs-file-path ((th datatype-or-module))
  (format nil "~a.pvs" (filename th)))

;;; The library-datatype-or-theory always has lib-ref set relative to the
;;; "real" current context, not the current context we may have
;;; temporarily gone into to typecheck a library.
;;; pvs-file-path gets the path for the current context.
(defmethod pvs-file-path ((th library-datatype-or-theory))
  (let* ((lib-ref (lib-ref th))
	 (lib-path (if (char= (char lib-ref 0) #\.)
		       (merge-pathnames lib-ref *pvs-context-path*)
		       (libref-to-pathname lib-ref))))
    (assert (file-exists-p lib-path))
    (format nil "~a~a.pvs" lib-path (filename th))))

(defmethod dep-lib-ref ((th datatype-or-module))
  (dep-lib-ref (lib-ref th)))

(defmethod dep-lib-ref ((lib-ref string))
  (let ((rel-lib-path (libref-to-pathname lib-ref)))
    (assert (file-exists-p rel-lib-path))
    rel-lib-path))

(defun libref-directory (pvs-file-string)
  (subseq pvs-file-string 0 (1+ (position #\/ pvs-file-string :from-end t))))

(defmethod change-to-library-class (th lib-ref)
  (assert (not (from-prelude? th)))
  (typecase th
    (library-datatype-or-theory th)
    (rectype-theory
     (change-class th 'library-rectype-theory))
    (module
     (change-class th 'library-theory))
    (datatype-with-subtypes
     (change-class th 'library-datatype-with-subtypes))
    (datatype
     (change-class th 'library-datatype))
    (codatatype-with-subtypes
     (change-class th 'library-codatatype-with-subtypes))
    (codatatype
     (change-class th 'library-codatatype)))
  (setf (lib-ref th) lib-ref)
  th)

(defmethod change-to-library-class ((obj library-datatype-or-theory) lib-ref)
  (declare (ignore lib-ref))
  obj)

(defmethod change-from-library-class (th)
  (typecase th
    (library-rectype-theory
     (change-class th 'rectype-theory))
    (library-theory
     (change-class th 'module))
    (library-datatype-with-subtypes
     (change-class th 'datatype-with-subtypes))
    (library-datatype
     (change-class th 'datatype))
    (library-codatatype-with-subtypes
     (change-class th 'codatatype-with-subtypes))
    (library-codatatype
     (change-class th 'codatatype))
    (t th))
  th)

(defun relativize-imported-library (from-context-path to-context-path)
  (let ((ref-alist nil))
    (maphash #'(lambda (from-ref files&theories)
		 (when (char= (char from-ref 0) #\.)
		   (let* ((from-lib-dir (merge-pathnames
					 from-ref from-context-path))
			  (to-ref (relative-path from-lib-dir
						     to-context-path)))
		     (unless (string= to-ref from-ref) ;; a sibling dir
		       (assert (or (null from-lib-dir)
				   (file-exists-p from-lib-dir)))
		       (assert (file-exists-p
				(merge-pathnames
				 to-ref to-context-path)))
		       (push (cons to-ref from-ref) ref-alist)
		       (maphash #'(lambda (id theory)
				    (declare (ignore id))
				    (assert (string= from-ref (lib-ref theory)))
				    (setf (lib-ref theory) to-ref))
				(cadr files&theories))))))
	     *imported-libraries*)
    ;; We mapped the theories, now change the *imported-libraries* keys
    (loop for (to-ref . from-ref) in ref-alist
	  do (let ((val (gethash from-ref *imported-libraries*)))
	       (assert val)
	       (remhash from-ref *imported-libraries*)
	       (setf (gethash to-ref *imported-libraries*) val)))
    ref-alist))

;;; This undoes the changes made by relativize-imported-library - keep in
;;; mind that there will be new libraries here, which need to be relativized.
(defun revert-relativized-imported-library (from-context-path to-context-path
							      ref-alist)
  (maphash #'(lambda (from-ref files&theories)
	       (let ((to-ref (cdr (assoc from-ref ref-alist :test #'string=))))
		 (if to-ref
		     (unless (string= to-ref from-ref)
		       (maphash #'(lambda (id theory)
				    (declare (ignore id))
				    (assert (string= from-ref
						     (lib-ref theory)))
				    (setf (lib-ref theory) to-ref))
				(cadr files&theories)))
		     ;; Wasn't already there - may need to relativize
		     (when (char= (char from-ref 0) #\.)
		       (let* ((from-lib-dir (merge-pathnames from-ref
							     from-context-path))
			      (to-ref (relative-path from-lib-dir
						     to-context-path)))
			 (unless (string= to-ref from-ref) ;; a sibling dir
			   (assert (or (null from-lib-dir)
				       (file-exists-p from-lib-dir)))
			   (assert (file-exists-p
				    (merge-pathnames
				     to-ref to-context-path)))
			   (push (cons from-ref to-ref) ref-alist)
			   (maphash #'(lambda (id theory)
					(declare (ignore id))
					(assert (string= from-ref (lib-ref theory)))
					(setf (lib-ref theory) to-ref))
				    (cadr files&theories))))))))
	   *imported-libraries*)
  ;; We mapped the theories, now change the *imported-libraries* keys
  (loop for (from-ref . to-ref) in ref-alist
	do (let ((val (gethash from-ref *imported-libraries*)))
	     (assert val)
	     (remhash from-ref *imported-libraries*)
	     (setf (gethash to-ref *imported-libraries*) val)))
  ref-alist)
