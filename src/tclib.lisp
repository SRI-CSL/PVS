;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tclib.lisp -- Functions for dealing with the prelude and library specs
;; Author          : Sam Owre
;; Created On      : Wed Dec  1 18:07:09 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 17:08:27 1998
;; Update Count    : 49
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

;;; Load-prelude initializes the *prelude*, *prelude-context*, and
;;; *prelude-theories* variables

(defparameter *prelude-filename* "prelude.pvs")

(defun load-prelude ()
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
		   (setf (gethash (adt-theory m) nusing-hash)
			 (list (mk-modname (id (adt-theory m)))))
		   (when (adt-map-theory m)
		     (setf (gethash (adt-map-theory m) nusing-hash)
			   (list (mk-modname (id (adt-map-theory m)))))
		     (dolist (decl (append (assuming (adt-map-theory m))
					   (theory (adt-map-theory m))))
		       (when (and (declaration? decl) (visible? decl))
			 (put-decl decl ndecls-hash))))
		   (when (adt-reduce-theory m)
		     (setf (gethash (adt-reduce-theory m) nusing-hash)
			   (list (mk-modname (id (adt-reduce-theory m)))))
		     (dolist (decl (append (assuming (adt-reduce-theory m))
					   (theory (adt-reduce-theory m))))
		       (when (and (declaration? decl) (visible? decl))
			 (put-decl decl ndecls-hash)))))
		  (t (setf (gethash m nusing-hash)
			   (list (mk-modname (id m))))))
		(maphash #'(lambda (id decls)
			     (setf (gethash id ndecls-hash)
				   (remove-if #'(lambda (d)
						  (typep d '(or formal-decl
								var-decl)))
				     decls)))
			 ndecls-hash)
		(let ((pusing-hash (copy nusing-hash)))
		  (setf (gethash (theory ctx) pusing-hash)
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
	  (format t "~%Done typechecking the prelude; restoring the proofs")
	  (restore-prelude-proofs))
      (set-working-directory cdir))))

(defun restore-prelude-proofs ()
  (let ((prfile (merge-pathnames (format nil "~a/lib/" *pvs-path*)
				 "prelude.prf")))
    (when (file-exists-p prfile)
      (let ((proofs (read-pvs-file-proofs prfile)))
	(maphash #'(lambda (id theory)
		     (declare (ignore id))
		     (restore-proofs prfile theory proofs)
		     (mapc #'(lambda (decl)
			       (when (justification decl)
				 (setf (proof-status decl) 'proved)))
			   (provable-formulas theory)))
		 *prelude*)))))

(defun save-prelude-proofs ()
  (let ((prfile (namestring (merge-pathnames (format nil "~a/lib/" *pvs-path*)
					     "prelude.prf"))))
    (save-proofs prfile *prelude-theories*)))

(defun prove-prelude (&optional retry?)
  (let ((theories *prelude-theories*)
	(*loading-prelude* t)
	(*proving-tcc* t))
    (prove-theories "prelude" theories retry?)
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
  (mapc #'load-prelude-library lib-refs))

;;; This is called from the Emacs load-prelude-library command as well.
(defun load-prelude-library (lib-path)
  (multiple-value-bind (lib-ref err-msg)
      (pathname-to-libref lib-path)
    (cond (err-msg
	   (pvs-message err-msg))
	  ((prelude-library-loaded? lib-ref)
	   (pvs-message "Library ~a is already loaded." lib-path))
	  (t (load-prelude-library* lib-ref lib-path)
	     (unless (member lib-ref (cadr *pvs-context*) :test #'string=)
	       (push lib-ref (cadr *pvs-context*))
	       (setq *pvs-context-changed* t))
	     (add-to-prelude-libraries lib-ref)))))

(defun load-prelude-library* (lib-ref lib-path)
  (if (gethash lib-ref *loaded-libraries*)
      (setf (gethash lib-ref *prelude-libraries*)
	    (gethash lib-ref *loaded-libraries*))
      (with-pvs-context lib-ref
	(let ((*current-theory* *current-theory*)
	      (*pvs-modules* (make-hash-table :test #'eq))
	      (*pvs-files* (make-hash-table :test #'equal))
	      (*pvs-context-writable* (write-permission? *pvs-context-path*))
	      (*pvs-context-changed* nil))
	  (restore-context)
	  (cond ((cddr *pvs-context*)
		 (dolist (ce (pvs-context-entries))
		   (typecheck-file (ce-file ce)))
		 (save-context)
		 (maphash #'(lambda (id th)
			      (declare (ignore id))
			      (if (module? th)
				  (change-class th 'library-theory)
				  (change-class th 'library-datatype))
			      (setf (lib-ref th) lib-ref)
			      (update-prelude-library-context th))
			  *pvs-modules*)
		 (setf (gethash lib-ref *prelude-libraries*)
		       (list *pvs-files* *pvs-modules*)))
		(t (type-error lib-path
		     "~a.pvscontext is empty - library not loaded"
		     lib-path)))))))

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
	   (name-judgements-hash (name-judgements-hash judgements))
	   (appl-judgements-hash (application-judgements-hash judgements)))
      (setf (name-judgements-hash judgements) (make-hash-table :test 'eq))
      (setf (application-judgements-hash judgements)
	    (make-hash-table :test 'eq))
      (merge-name-judgements name-judgements-hash
			     (name-judgements-hash judgements)
			     th (mk-modname (id th)))
      (merge-application-judgements appl-judgements-hash
				    (application-judgements-hash judgements)
				    th (mk-modname (id th)))))
  (let* ((*current-theory* th)
	 (*current-context* *prelude-library-context*)
	 (thname (mk-modname (id th))))
    (update-current-context th thname)
    (dolist (decl (append (assuming th) (theory th)))
      (when (and (declaration? decl) (visible? decl))
	(put-decl decl (current-declarations-hash))))
    (setf (gethash th (current-using-hash)) (list thname))))



(defun prelude-library-loaded? (lib)
  (gethash lib *prelude-libraries*))


;;; Called from restore-theories*; the dep-file-ref comes from the
;;; .pvscontext So is always relative to the *pvs-context-path*, but not
;;; necessarily the *pvs-current-context-path*.  lib-ref is relative to
;;; the *pvs-current-context-path*.
(defun restore-imported-library-file (dep-file-ref)
  (let* ((dep-lib-ref (libref-directory dep-file-ref))
	 (lib-ref (get-relative-library-reference dep-lib-ref)))
    (multiple-value-bind (lib-path err-msg)
	(libref-to-pathname dep-lib-ref)
      (if err-msg
	  (type-error dep-file-ref err-msg)
	  (unless (or (gethash lib-ref *prelude-libraries*)
		      (file-equal lib-path *pvs-context-path*))
	    (with-pvs-context dep-lib-ref
	      (let ((*current-theory* *current-theory*)
		    (*pvs-context-writable* (write-permission? lib-path))
		    (*pvs-context-changed* nil)
		    (filename (pathname-name dep-file-ref)))
		(restore-context)
		(multiple-value-bind (*pvs-files* *pvs-modules*)
		    (get-imported-files-and-theories lib-ref)
		  (let* ((*prelude-libraries* (make-hash-table :test #'equal))
			 (theories (typecheck-file filename nil nil nil t)))
		    (cond ((and theories
				(every #'typechecked? theories))
			   (save-context)
			   (maphash
			    #'(lambda (id th)
				(declare (ignore id))
				(unless (library-datatype-or-theory? th)
				  (if (typep th 'module)
				      (change-class th 'library-theory)
				      (change-class th 'library-datatype)))
				(unless (and (lib-ref th)
					     (file-equal (lib-ref th)
							 *pvs-current-context-path*))
				  (setf (lib-ref th) lib-ref)))
			    *pvs-modules*))
			  (t
			   (remhash filename *pvs-files*)
			   (dolist (th theories)
			     (remhash (id th) *pvs-modules*)))))))))))))

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
  (let ((rel-lib-ref (get-relative-library-reference lib-ref)))
    (if (or (gethash rel-lib-ref *prelude-libraries*)
	    (file-equal rel-lib-ref *pvs-context-path*))
	(get-theory (copy theory-name 'library nil))
	(let ((value nil))
	  (with-pvs-context lib-ref
	    (let ((*current-theory* *current-theory*)
		  (*pvs-context-writable* (write-permission? *pvs-context-path*))
		  (*pvs-context-changed* nil))
	      (restore-context)
	      (multiple-value-bind (*pvs-files* *pvs-modules*)
		  (get-imported-files-and-theories rel-lib-ref)
		(let* ((*prelude-libraries* (make-hash-table :test #'equal))
		       (filename (context-file-of theory-name)))
		  (unless filename
		    (if (file-exists-p (make-specpath theory-name))
			(setq filename (string (id theory-name)))
			(setq filename (look-for-theory-in-directory-files
					theory-name))))
		  (if filename
		      (let* ((theories (typecheck-file filename))
			     (theory (find theory-name theories :test #'same-id)))
			(cond (theory
			       (let ((*current-context* (context theory))
				     (*current-theory* theory))
				 (save-context))
			       (maphash
				#'(lambda (id th)
				    (declare (ignore id))
				    (unless (library-datatype-or-theory? th)
				      (if (typep th 'module)
					  (change-class th 'library-theory)
					  (change-class th 'library-datatype)))
				    (setf (lib-ref th) rel-lib-ref))
				*pvs-modules*)
			       (setq value theory))
			      (t (setq value
				       (format nil
					   "Theory ~a could  not be found in ~
                                          the PVS context of library ~a"
					 theory-name lib-ref)))))
		      (setq value
			    (format nil
				"Theory ~a not found in the PVS context of ~
                               library ~a"
			      theory-name lib-ref)))))))
	  (if (stringp value)
	      (type-error theory-name value)
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
				   (t (make-hash-table :test #'equal)))))
	   (and (filename th)
		(let ((path (probe-file (pvs-file-path th))))
		  (parsed?* path)))))))

(defun remove-prelude-library (lib-path)
  (multiple-value-bind (lib-ref err-msg)
      (pathname-to-libref lib-path)
    (if err-msg
	(pvs-message err-msg)
	(let ((libhash (gethash lib-ref *prelude-libraries*)))
	  (cond (libhash
		 (remhash lib-ref *prelude-libraries*)
		 (maphash #'(lambda (id theory)
			      (declare (ignore id))
			      (setf *prelude-libraries-uselist*
				    (delete (assq theory
						  *prelude-libraries-uselist*)
					    *prelude-libraries-uselist*)))
			  (cadr libhash))
		 (setq *pvs-context-changed* t)
		 (reset-context)
		 (pvs-message "Library ~a has been removed, and the context reset"
		   lib-path))
		(t (pvs-message "Library ~a is not loaded" lib-path)))))))

(defun prelude-libraries ()
  (let ((libs nil))
    (maphash #'(lambda (lib theories)
		 (declare (ignore theories))
		 (push (shortname lib) libs))
	     *prelude-libraries*)
    libs))

(defun get-prelude-libraries ()
  (assert *current-context*)
  (nconc (get-prelude-libraries* (all-decls (module *current-context*))
				 (declaration *current-context*))
	 (get-using-prelude-libraries (using-hash *current-context*))))

(defun get-prelude-libraries* (decls &optional end-decl libs)
  (if (or (null decls)
	  (eq (car decls) end-decl))
      (nreverse libs)
      (get-prelude-libraries*
       (cdr decls) end-decl
       (if (and (typep (car decls) 'lib-decl)
		(not (member (library (car decls)) libs :test #'string=)))
	   (cons (library (car decls)) libs)
	   libs))))

(defun get-using-prelude-libraries (using &optional libs)
  (if (null using)
      (nreverse libs)
      (get-using-prelude-libraries
       (cdr using)
       (get-theory-prelude-libraries* (caar using) libs))))

(defun get-theory-prelude-libraries* (theory libs)
  (if (or (from-prelude? theory)
	  (typep theory '(or library-theory library-datatype)))
      libs
      (get-prelude-libraries* (all-decls theory) nil (nreverse libs))))

(defun get-visible-libraries ()
  (assert *current-context*)
  (nconc (get-visible-libraries* (all-decls (theory *current-context*))
				 (declaration *current-context*))
	 (get-using-visible-libraries (using-hash *current-context*))))

(defun get-visible-libraries* (decls &optional end-decl libs)
  (if (or (null decls)
	  (eq (car decls) end-decl))
      (nreverse libs)
      (get-visible-libraries*
       (cdr decls) end-decl
       (if (and (typep (car decls) 'lib-decl)
		(not (member (library (car decls)) libs :test #'string=)))
	   (cons (library (car decls)) libs)
	   libs))))

(defun get-using-visible-libraries (using &optional libs)
  (if (null using)
      (nreverse libs)
      (get-using-visible-libraries
       (cdr using)
       (get-theory-visible-libraries* (caar using) libs))))

(defun get-theory-visible-libraries* (theory libs)
  (if (or (from-prelude? theory)
	  (typep theory 'library-theory))
      libs
      (get-visible-libraries* (all-decls theory) nil (nreverse libs))))

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
    (if (file-exists-p dirstr)
	(pathname-to-libref dirstr)
	(values nil "Directory ~a does not exist" libstr))))


(defmethod get-library-reference ((libid symbol))
  ;; libid is the library part of a name, e.g., the foo in foo@bar.x
  ;; Must be a reference to a lib-decl, a subdirectory of the current context,
  ;; or a library in *pvs-path*/lib/
  (let* ((lib-decls (get-lib-decls libid))
	 (librefs (mapcar #'get-library-reference lib-decls))
	 (csubdir (format nil "./~a/" libid))
	 (csubref (when (and (not (member csubdir librefs :test #'string=))
			     (file-exists-p
			      (merge-pathnames csubdir *pvs-context-path*)))
		    csubdir))
	 (lsubref (when (some #'(lambda (lpath)
				  (file-exists-p (concatenate 'string
						   lpath "/" (string libid))))
			      *pvs-library-path*)
		    (concatenate 'string (string libid) "/")))
	 (refs (nconc librefs
		      (when csubref (list csubref))
		      (when lsubref (list lsubref)))))
;;     (when (cdr refs)
;;       (pvs-warning "Library id ~a is ambiguous, ~
;;                     the first one of the following is used:~
;;                     ~{~^  ~%~{~a  ~a~}~}" libid refs))
    (or (car refs)
	(values nil (format nil "Library id ~a cannot be resolved to a library declaration, a subdirectory of the current context, a subdirectory of PVS_LIBRARY_PATH, nor a subdirectory of ~a/lib/"
		      libid *pvs-path*)))))

(defmethod get-library-reference ((ldecl lib-decl))
  (if (library-datatype-or-theory? (module ldecl))
      (let* ((ldecl-lib-ref (lib-ref ldecl))
	     (theory-lib-ref (lib-ref (module ldecl)))
	     (theory-lib-path (libref-to-pathname theory-lib-ref)))
	(assert theory-lib-path)
	(if (string= ldecl-lib-ref theory-lib-ref)
	    theory-lib-ref
	    ;; Need to create lib-ref from current-context to ldecl-lib-ref
	    ;; Do this by temporarily changing the working directory to
	    ;; theory-lib-path, and getting the real path, then relativizing
	    ;; it to the current context
	    (let ((cdir (working-directory))
		  (*default-pathname-defaults* *default-pathname-defaults*))
	      (unwind-protect
		  (progn (set-working-directory theory-lib-path)
			 (setq *default-pathname-defaults*
			       (working-directory))
			 (relative-path (libref-to-pathname ldecl-lib-ref)))
		(set-working-directory cdir)))))
      (lib-ref ldecl)))

(defun get-relative-library-reference (lib-ref)
  (if (or (eq *pvs-current-context-path* *pvs-context-path*)
	  (not (member (char lib-ref 0) '(#\/ #\. #\~))))
      lib-ref
      (relative-path (libref-to-pathname lib-ref)
		     *pvs-current-context-path*)))

(defun get-lib-decls (libid)
  (assert *current-context*)
  (assert (symbolp libid))
  (let ((lib-decls (remove-if (complement #'lib-decl?)
		     (gethash libid (current-declarations-hash)))))
    (remove-duplicates (sort (copy-list lib-decls) #'< :key #'locality)
      :test #'equal :key #'lib-ref :from-end t)))

;;; Called by restore-theories* to clean up saved dependencies, which
;;; used to save absolute pathnames.
(defun get-library-file-reference (dep-file-ref)
  (let ((dir (libref-directory dep-file-ref))
	(file (pathname-name dep-file-ref)))
    (concatenate 'string (get-library-reference dir) file)))
  

;; (defun get-lib-decl-pathname (lib-decl)
;;   (libref-to-pathname (lib-ref lib-decl)))

;; (defun get-context-library-pathname (libstr)
;;   (unless (member (char libstr 0) '(#\. #\/ #\~) :test #'char=)
;;     (multiple-value-bind (libpath condition)
;; 	(ignore-errors
;; 	  (namestring (merge-pathnames libstr *pvs-context-path*)))
;;       (if condition
;; 	  (values nil nil condition)
;; 	  (let ((lib-key (get-file-info libpath)))
;; 	    (when lib-key
;; 	      (let ((entry (assoc lib-key *library-alist* :test #'equal)))
;; 		(if entry
;; 		    (values (cdr entry) libpath)
;; 		    (let ((reldir (concatenate 'string "./" libstr)))
;; 		       (push (cons lib-key reldir) *library-alist*)
;; 		       (values reldir libpath))))))))))

;; (defun get-pvslib-pathname (libdir)
;;   (unless (member (char libdir 0) '(#\. #\/ #\~) :test #'char=)
;;     (multiple-value-bind (libpath condition)
;; 	(ignore-errors
;; 	  (namestring (merge-pathnames
;; 		       libdir (format nil "~a/lib/" *pvs-path*))))
;; 	(if condition
;; 	    (values nil nil (format nil "~a" condition))
;; 	    (let ((lib-key (get-file-info libpath)))
;; 	      (if lib-key
;; 		  (let ((entry (assoc lib-key *library-alist* :test #'equal)))
;; 		    (cond (entry
;; 			   (values (cdr entry) libpath))
;; 			  (t
;; 			   (push (cons lib-key libdir) *library-alist*)
;; 			   (values libdir libpath))))))))))

;; (defun get-rel-or-abs-pathname (libdir)
;;   (when (member (char libdir 0) '(#\. #\/ #\~) :test #'char=)
;;     (multiple-value-bind (libpath condition)
;; 	(ignore-errors
;; 	  (namestring (merge-pathnames libdir *pvs-context-path*)))
;; 	(if condition
;; 	    (values nil nil (format nil "~a" condition))
;; 	    (let ((lib-key (get-file-info libpath)))
;; 	      (if lib-key
;; 		  (let ((entry (assoc lib-key *library-alist* :test #'equal)))
;; 		    (if entry
;; 			(values (cdr entry) libpath)
;; 			(let ((rdir (if *pvs-relative-context-path*
;; 					(relative-path
;; 					 libdir *pvs-relative-context-path*)
;; 					libdir)))
;; 			  (push (cons lib-key rdir) *library-alist*)
;; 			  (values rdir libpath))))))))))

(defun all-decls (theory)
  (or (all-declarations theory)
      (let ((decls (append (formals theory)
			   (mapcan #'(lambda (d)
				       (when (typep d 'formal-subtype-decl)
					 (remove-if #'tcc? (generated d))))
			     (formals theory))
			   (assuming theory)
			   (theory theory))))
	(when (memq 'typechecked (status theory))
	  (setf (all-declarations theory) decls))
	decls)))


;;; lib-ref in this case is always relative to the pvs-context-path
;;; NOT necessarily the pvs-current-context-path.
(defun libref-to-pathname (lib-ref)
  (assert (stringp lib-ref))
  (assert (char= (char lib-ref (- (length lib-ref) 1)) #\/))
  (let ((lib-path (if (member (char lib-ref 0) '(#\~ #\. #\/) :test #'char=)
		      ;; It's already a real pathname
		      lib-ref
		      ;; Otherwise it's a PVS library ref (e.g., finite_sets)
		      ;; Prepend "*pvs-path*/lib/"
		      ;;(format nil "~a/lib/~a" *pvs-path* lib-ref)
		      (pvs-library-path-ref lib-ref))))
    (assert (file-exists-p lib-path))
    (if (file-exists-p lib-path)
	lib-path
	(values nil (format nil "Library ~a does not exist" lib-path)))))

(defun pvs-library-path-ref (lib-ref &optional (libs *pvs-library-path*))
  (when libs
    (let ((lib-path (concatenate 'string (car libs) "/" lib-ref)))
      (if (directory-p lib-path)
	  lib-path
	  (pvs-library-path-ref lib-ref (cdr libs))))))

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
  ;; First see if there is a corresponding lib-decl
  (or (libref-to-lib-decl-id lib-ref)
      ;; If not, might be a local subdirectory ./foo/
      (and (char= (char lib-ref 0) #\.)
	   (char= (char lib-ref 1) #\/)
	   (= (count #\/ lib-ref) 2)
	   (file-exists-p lib-ref)
	   (intern (subseq lib-ref 2 (1- (length lib-ref)))))
      ;; Finally, look for PVSLIB, e.g., foo/
      (and (not (member (char lib-ref 0) '(#\/ #\~ #\.) :test #'char=))
	   (= (count #\/ lib-ref) 1)
	   (pvs-library-path-ref lib-ref)
	   (intern (subseq lib-ref 0 (1- (length lib-ref)))))))

(defun libref-to-lib-decl-id (lib-ref)
  (let ((lib-decls nil))
    (maphash #'(lambda (id decls)
		 (declare (ignore id))
		 (setq lib-decls
		       (nconc (mapcan
				  #'(lambda (d)
				      (when (and (lib-decl? d)
						 (string= lib-ref (lib-ref d)))
					(list d)))
				decls)
			      lib-decls)))
	     (current-declarations-hash))
    (when lib-decls
      (get-most-local-reference lib-decls))))

(defun get-most-local-reference (list &optional (min 1)
				      most-local most-local-num)
  (if (null list)
      (id most-local)
      (let ((lnum (locality (car list))))
	(if (<= lnum min)
	    (car list)
	    (if (or (null most-local)
		    (< lnum most-local-num))
		(get-most-local-reference
		 (cdr list) min (car list) lnum)
		(get-most-local-reference
		 (cdr list) min most-local most-local-num))))))
		 

(defun get-all-lib-decls ()
  (let ((lib-decls nil))
    (maphash #'(lambda (id decls)
		 (declare (ignore id))
		 (setq lib-decls
		       (append (remove-if (complement #'lib-decl?) decls)
			       lib-decls)))
	     (current-declarations-hash))
    lib-decls))

(defmethod pvs-file-path ((th datatype-or-module))
  (format nil "~a.pvs" (filename th)))

;;; The library-datatype-or-theory always has lib-ref set relative to the
;;; "real" current context, not the current context we may have
;;; temporarily gone into to typecheck a library.
;;; pvs-file-path gets the path for the current context.
(defmethod pvs-file-path ((th library-datatype-or-theory))
  (let* ((lib-ref (lib-ref th))
	 (rel-lib-path
	  (if (eq *pvs-context-path* *pvs-current-context-path*)
	      (libref-to-pathname lib-ref)
	      (let ((cdir (working-directory))
		    (*default-pathname-defaults* *default-pathname-defaults*))
		(unwind-protect
		    (progn (set-working-directory *pvs-current-context-path*)
			   (setq *default-pathname-defaults*
				 (working-directory))
			   (relative-path (libref-to-pathname lib-ref)
					  *pvs-context-path*))
		  (set-working-directory cdir))))))
    (assert (file-exists-p rel-lib-path))
    (format nil "~a~a.pvs" rel-lib-path (filename th))))

(defmethod dep-lib-ref ((th datatype-or-module))
  (dep-lib-ref (lib-ref th)))

(defmethod dep-lib-ref ((lib-ref string))
  (let ((rel-lib-path
	 (if (eq *pvs-context-path* *pvs-current-context-path*)
	     (libref-to-pathname lib-ref)
	     (let ((cdir (working-directory))
		   (*default-pathname-defaults* *default-pathname-defaults*))
	       (unwind-protect
		   (progn (set-working-directory *pvs-current-context-path*)
			  (setq *default-pathname-defaults*
				(working-directory))
			  (relative-path (libref-to-pathname lib-ref)
					 *pvs-context-path*))
		 (set-working-directory cdir))))))
    (assert (file-exists-p rel-lib-path))
    rel-lib-path))

(defun libref-directory (pvs-file-string)
  (subseq pvs-file-string 0 (1+ (position #\/ pvs-file-string :from-end t))))

;;; Converts a dep-file to one relative to the current context path
;; (defun dep-file-to-pathname (pvs-file-string)
;;   (let ((dirstr (libref-directory pvs-file-string)))
;;     (multiple-value-bind (lib-dir err-msg)
;; 	(dep-lib-ref dirstr)
;;       (if err-msg
;; 	  (values nil err-msg)
;; 	  (let ((file (format nil "~a~a.pvs"
;; 			lib-dir (pathname-name pvs-file-string))))
;; 	    (if (file-exists-p file)
;; 		file
;; 		(values nil (format nil "~a not found" file))))))))

;;; Needed when getting file dependencies
;; (defun indirect-libref-to-direct-libref (lib-ref)
;;   (if (or (eq *pvs-context-path* *pvs-current-context-path*) ; no problem
;; 	  (not (char= (char lib-ref 0) #\.))) ; Not a relative lib-ref
;;       lib-ref
;;       (let* ((cdir (working-directory))
;; 	     (*default-pathname-defaults* *default-pathname-defaults*)
;; 	     (dlibref
;; 	      (unwind-protect
;; 		  (progn (set-working-directory *pvs-context-path*)
;; 			 (setq *default-pathname-defaults* (working-directory))
;; 			 (relative-path (libref-to-pathname lib-ref)
;; 					*pvs-current-context-path*))
;; 		(set-working-directory cdir))))
;; 	(assert (let ((*default-pathname-defaults* *default-pathname-defaults*))
;; 		  (unwind-protect
;; 		      (progn (set-working-directory *pvs-current-context-path*)
;; 			     (setq *default-pathname-defaults* (working-directory))
;; 			     (file-exists-p dlibref))
;; 		    (set-working-directory *pvs-context-path*))))
;; 	dlibref)))

;; This one is needed when ???
;; (defun direct-libref-to-indirect-libref (lib-ref)
;;   (if (eq *pvs-context-path* *pvs-current-context-path*)
;;       lib-ref
;;       (let* ((cdir (working-directory))
;; 	     (*default-pathname-defaults* *default-pathname-defaults*)
;; 	     (ilibref
;; 	      (unwind-protect
;; 		  (progn (set-working-directory *pvs-current-context-path*)
;; 			 (setq *default-pathname-defaults*
;; 			       (working-directory))
;; 			 (relative-path (libref-to-pathname lib-ref)))
;; 		(set-working-directory cdir))))
;; 	ilibref)))

;;; Some documentation:
;;; Every library theory or datatype has a lib-ref slot, which is always set
;;; relative to the *pvs-current-context-path*.  Lib-decls also have a
;;; lib-ref slot, but it is always relative to the context of its
;;; declaration.
;;;
;;;  indirect-libref-to-direct-libref (lib-ref)
;;;    Converts a lib-ref of the *pvs-context-path* to one of the
;;;    *pvs-current-context-path*
;;;  get-relative-library-reference (lib-ref)
;;;    Returns the lib-ref relative to the *pvs-current-context-path*
;;;  pvs-file-path  (th)
;;;    creates a file path to the given library theory th.
;;;    This will be accessible to the *pvs-context-path*, and the file
;;;    is checked with file-exists-p.
;;;  dep-lib-ref (lib-ref)
;;;    Converts a lib-ref of the *pvs-current-context-path* to one of the
;;;    *pvs-context-path*.  The directory should satisfy file-exists-p.
;;;  pathname-to-libref (lib-path)
;;;    Given a lib-path - an existing directory - creates a lib-ref
;;;    If the lib-path is a subdirectory of the <PVS>/lib directory, then
;;;    "subd/" is returned.  Otherwise it tries to find a way to access
;;;    the subdirectory from two levels above the *pvs-context-path*.
;;;  libref-to-pathname (lib-ref)
;;;    Returns the directory associated with a lib-ref - inverse to
;;;    pathname-to-libref.
;;;  get-library-reference (arg)
;;;    Converts a library reference as provided by the user to a lib-ref.

