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
	*ordinal* nil)
  (let ((cdir (working-directory))
	(*pvs-context-path* nil)
	(*pvs-modules* (make-hash-table :test #'eq :size 20 :rehash-size 10))
	(*pvs-files* (make-hash-table :test #'equal))
	(*loaded-libraries* (make-hash-table :test #'equal))
	(*prelude-libraries* (make-hash-table :test #'equal))
	(*imported-libraries* (make-hash-table :test #'equal))
	(*pvs-context* nil)
	(*loading-prelude* t)
	(*generate-tccs* 'all!)
	(mods (parse :file (format nil "~a/lib/~a"
			     *pvs-path* *prelude-filename*))))
    (clear-theories t)
    (setq *prelude-context* nil)
    (clrnumhash)
    (clear-ignored-type-constraints)
    (reset-all-operators)
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
		(setq *prelude-context*
		      (copy ctx
			'declarations-hash ndecls-hash
			'using-hash nusing-hash)))
	      (when (eq (id m) '|booleans|)
		(let ((*current-context* (saved-context m)))
		  (setq *true*
			(typecheck* (mk-name-expr 'true) *boolean* nil nil))
		  (setq *false*
			(typecheck* (mk-name-expr 'false) *boolean* nil
				    nil))))))
	  (format t "~%Done typechecking the prelude; restoring the proofs")
	  (restore-prelude-proofs))
      (set-working-directory cdir))))

(defun restore-prelude-proofs ()
  (let ((prfile (merge-pathnames (format nil "~a/lib/" *pvs-path*)
				 "prelude.prf")))
    (when (probe-file prfile)
      (maphash #'(lambda (id theory)
		   (declare (ignore id))
		   (restore-proofs prfile theory)
		   (mapc #'(lambda (decl)
			     (when (justification decl)
			       (setf (proof-status decl) 'proved)))
			 (provable-formulas theory)))
	       *prelude*))))

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
	(proof-summaries theories nil "prelude"))
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

(defun load-prelude-libraries (libnames)
  (mapc #'load-prelude-library libnames))

(defun load-prelude-library (libname)
  (multiple-value-bind (lib msg)
      (library-of libname)
    (cond ((null lib)
	   (pvs-message msg libname))
	  ((prelude-library-loaded? lib)
	   (pvs-message "Library ~a is already loaded." libname))
	  (t (load-prelude-library* lib)
	     (unless (member libname (cadr *pvs-context*) :test #'string=)
	       (push libname (cadr *pvs-context*))
	       (setq *pvs-context-changed* t))
	     (add-to-prelude-libraries lib libname)))))

(defun add-to-prelude-libraries (lib libname)
  (setf (gethash lib *loaded-libraries*)
	(gethash lib *prelude-libraries*))
  (setq *prelude-libraries-uselist*
	(let ((uselist nil))
	  (maphash #'(lambda (path files&theories)
		       (declare (ignore path))
		       (maphash #'(lambda (id theory)
				    (push (list theory (mk-modname id))
					  uselist))
				(cadr files&theories)))
		   *prelude-libraries*)
	  uselist)))

(defun prelude-libraries-uselist ()
  *prelude-libraries-uselist*)


;;; Loads the library into the *prelude-libraries*

(defun load-prelude-library* (lib)
  (if (gethash lib *loaded-libraries*)
      (setf (gethash lib *prelude-libraries*)
	    (gethash lib *loaded-libraries*))
      (with-pvs-context lib
	(let ((*loading-library* lib)
	      (*pvs-modules* (make-hash-table :test #'eq))
	      (*pvs-files* (make-hash-table :test #'equal))
	      (*pvs-context-writable* (write-permission? lib))
	      (*pvs-context-changed* nil))
	  (restore-context)
	  (cond ((cddr *pvs-context*)
		 (dolist (ce (pvs-context-entries))
		   (typecheck-file (ce-file ce)))
		 (save-context)
		 (maphash #'(lambda (id th)
			      (declare (ignore id))
			      (if (typep th 'module)
				  (change-class th 'library-theory)
				  (change-class th 'library-datatype))
			      (setf (library th) lib))
			  *pvs-modules*)
		 (setf (gethash lib *prelude-libraries*)
		       (list *pvs-files* *pvs-modules*)))
		(t (pvs-message "~a.pvscontext is empty - library not loaded"
		     (namestring lib))))))))

;;; Given a string, determine whether it is a simple string or a directory
;;; name, and add the pvs lib directory to the latter.

(defun library-of (rawlibname)
  (let* ((libstr (string rawlibname))
	 (libname (if (char= (char libstr (1- (length libstr))) #\/)
		      libstr
		      (concatenate 'string libstr "/"))))
    (or (let ((libr (rassoc libname *library-alist* :test #'string=)))
	  (when libr
	    (cond ((probe-file (car libr))
		   (car libr))
		  (t (setf *library-alist* (delete libr *library-alist*))
		     nil))))
	(multiple-value-bind (lib condition)
	    (ignore-errors
	      (if (valid-pvs-id* rawlibname)
		  (or (directory-p (make-pathnames
				    :directory libname
				    :defaults (format nil "~a/lib/"
						*pvs-path*)))
		      (make-pathname :directory libname
				     :defaults (working-directory)))
		  (make-pathname :directory libname
				     :defaults (working-directory))))
	  (cond (condition
		 (values nil (format nil "~a" condition)))
		((not (probe-file lib))
		 (values nil "Library ~a does not exist"))
		((not (probe-file (context-pathname lib)))
		 (values nil "Library ~a does not have a PVS context"))
		(t (let ((lib (pvs-truename lib)))
		     (pushnew (cons lib libname) *library-alist*
			      :test #'equal)
		     lib)))))))
	


;;; Get-library-pathname returns the pathname associated with a library name.
;;; It first checks for a library declaration of the same name, and returns
;;; the associated path if it is found, otherwise it checks for the name as
;;; part of the default library.

(defun get-library-pathname (libname theory)
  (let ((libn (if (pathnamep libname)
		  (namestring libname)
		  libname)))
    (multiple-value-bind (lib msg)
	(library-of libname)
      (or lib
	  (find-library-pathname libname theory)
	  (values nil msg)))))

(defun find-library-pathname (libname theory)
  (let ((lib-decl (find-if #'(lambda (d)
			       (and (typep d 'lib-decl)
				    (eq (id d) libname)))
		    (all-decls theory))))
    (if lib-decl
	(let ((path (if (typep theory 'library-theory)
			(merge-pathnames (library lib-decl) (library theory))
			(library lib-decl))))
	  (if (probe-file path)
	      (pvs-truename path)
	      (progn (setf (module lib-decl) *current-theory*)
		     (type-error lib-decl "Library directory ~s does not exist"
				 path))))
	(find-library-pathname* libname (get-immediate-nonlibrary-usings
					 theory)))))

(defmethod get-immediate-nonlibrary-usings ((theory module))
  (mapappend #'(lambda (imp)
		 (unless (library imp)
		   (let ((th (get-theory imp)))
		     (if (typep th 'datatype)
			 (datatype-instances imp)
			 (list imp)))))
	     (mapcar #'theory-name
	       (remove-if-not #'mod-or-using?
		 (all-decls theory)))))

(defmethod get-immediate-nonlibrary-usings ((adt datatype))
  (remove-if #'library
    (append (mapcar #'theory-name
	      (remove-if-not #'mod-or-using?
		(append (formals adt) (assuming adt))))
	    (when (importings adt)
	      (mapcar #'theory-name (importings adt))))))

(defun all-decls (theory)
  (append (formals theory)
	  (mapcan #'(lambda (d)
		      (when (typep d 'formal-subtype-decl)
			(list (car (generated d)))))
		  (formals theory))
	  (assuming theory)
	  (theory theory)))

(defun find-library-pathname* (libname usings)
  (when usings
    (or (let ((theory (get-theory (car usings))))
	  (and theory
	       (find-library-pathname libname theory)))
	(find-library-pathname* libname (cdr usings)))))


(defun library-exists? (lib)
  (and (probe-file lib)
       (probe-file (context-pathname lib))))

(defun prelude-library-loaded? (lib)
  (and (gethash lib *prelude-libraries*)
       (library-current? lib)))

;;; Checks that the library context file is current, and that all of the
;;; pvs-files of that context have not changed.

(defun library-current? (lib)
  (and (library-context-current? lib)
       (library-files-current? lib)))

(defun library-context-current? (lib)
  t)

(defun library-files-current? (lib)
  t)


(defmethod load-imported-library-file (lib filename)
  (unless (or (gethash lib *prelude-libraries*)
	      (equal (pvs-truename lib) (working-directory)))
    (with-pvs-context lib
      (let ((*pvs-context-writable* (write-permission? lib))
	    (*pvs-context-changed* nil))
	(let ((*loading-library* lib))
	  (restore-context))
	(multiple-value-bind (*pvs-files* *pvs-modules*)
	    (get-imported-files-and-theories lib)
	  (let* ((*prelude-libraries* (make-hash-table :test #'equal))
		 (theories (typecheck-file filename)))
	    (cond ((and theories
			(every #'typechecked? theories))
		   (save-context)
		   (maphash #'(lambda (id th)
				(declare (ignore id))
				(if (typep th 'module)
				    (change-class th 'library-theory)
				    (change-class th 'library-datatype))
				(setf (library th) lib))
			    *pvs-modules*))
		  (t
		   (remhash filename *pvs-files*)
		   (dolist (th theories)
		     (remhash (id th) *pvs-modules*))))))))))

(defmethod load-imported-library-file ((libname string) filename)
  (multiple-value-bind (lib msg)
      (library-of libname)
    (if lib
	(load-imported-library-file lib filename)
	(type-error filename msg libname))))

;;; Load-imported-library loads imported libraries - called from
;;; get-parsed-theory when a library name is given in an IMPORTING clause.

(defmethod load-imported-library ((libname string) theoryname)
  (multiple-value-bind (lib msg)
      (library-of libname)
    (if lib
	(let ((thname
	       (typecase theoryname
		 (modname theoryname)
		 (symbol (mk-modname theoryname))
		 (string (mk-modname (intern theoryname)))
		 (t (error
		     "Bad theoryname argument to load-imported-library")))))
	  (load-library-theory lib thname))
	(type-error theoryname msg libname))))

#-gcl
(defmethod load-imported-library ((lib pathname) theoryname)
  (load-library-theory
   lib
   (typecase theoryname
     (modname theoryname)
     (symbol (mk-modname theoryname))
     (string (mk-modname (intern theoryname)))
     (t (error "Bad theoryname argument to load-imported-library")))))

#+gcl
(defmethod load-imported-library (lib theoryname)
  (if (pathnamep lib)
      (load-library-theory lib theoryname)
      (error "Invalid argument")))

(defun load-library-theory (lib theoryname)
  (if (or (gethash lib *prelude-libraries*)
	  (equal (pvs-truename lib) (working-directory)))
      (get-theory (copy theoryname 'library nil))
      (with-pvs-context lib
	(let ((*pvs-context-writable* (write-permission? lib))
	      (*pvs-context-changed* nil))
	  (let ((*loading-library* lib))
	    (restore-context))
	  (multiple-value-bind (*pvs-files* *pvs-modules*)
	      (get-imported-files-and-theories lib)
	    (let* ((*prelude-libraries* (make-hash-table :test #'equal))
		   (filename (context-file-of theoryname)))
	      (unless (or filename
			  (not (probe-file (make-specpath theoryname))))
		(setq filename (string (id theoryname))))
	      (if filename
		  (let* ((theories (typecheck-file filename))
			 (theory (find theoryname theories :test #'same-id)))
		    (cond (theory
			   (save-context)
			   (maphash #'(lambda (id th)
					(declare (ignore id))
					(if (typep th 'module)
					    (change-class th 'library-theory)
					    (change-class th 'library-datatype))
					(setf (library th) lib))
				    *pvs-modules*)
			   theory)
			  (t (type-error lib
			       "Theory ~a could  not be found in the PVS context of library ~a"
			       theoryname lib))))
		  (type-error lib
		    "Theory ~a not found in the PVS context of library ~a"
		    theoryname lib))))))))


(defun add-to-visible-libraries (lib libname)
  (push (cons lib libname) *library-alist*)
  (setf (gethash lib *visible-libraries*)
	(gethash lib *prelude-libraries*))
  (setq *visible-libraries-uselist*
	(let ((uselist nil))
	  (maphash #'(lambda (path lib)
		       (declare (ignore path))
		       (maphash #'(lambda (id theory)
				    (push (list theory (mk-modname id))
					  uselist))
				lib))
		   *visible-libraries*)
	  uselist)))

(defun visible-libraries-pathnames ()
  (let ((paths nil))
    (maphash #'(lambda (p ht)
		 (declare (ignore ht))
		 (let ((str (cdr (assoc p *library-alist* :test #'equal))))
		   (push str paths)))
	     *visible-libraries*)
    paths))

(defun visible-libraries-uselist ()
  *visible-libraries-uselist*)

(defun remove-prelude-library (libname)
  (multiple-value-bind (lib msg)
      (library-of libname)
    (if lib
	(let ((libhash (gethash lib *prelude-libraries*)))
	  (cond (libhash
		 (remhash lib *prelude-libraries*)
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
			       libname))
		(t (pvs-message "Library ~a is not loaded" libname))))
	(pvs-message msg libname))))

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
		 (maphash #'(lambda (file date&theories)
			      (pushnew (format nil "~a~a" lib file) files
				       :test #'equal))
			  (car files&theories)))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (maphash #'(lambda (file date&theories)
			      (pushnew (format nil "~a~a" lib file) files
				       :test #'equal))
			  (car files&theories)))
	     *prelude-libraries*)
    (sort files #'string<)))

(defun library-theories ()
  (let ((theories nil))
    (maphash #'(lambda (lib files&theories)
		 (maphash #'(lambda (file date&theories)
			      (dolist (th (cdr date&theories))
				(push (list (format nil "~a~a" lib (id th))
					    (format nil "~a~a" lib file)
					    (place-list (place th)))
				      theories)))
			  (car files&theories)))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (maphash #'(lambda (file date&theories)
			      (dolist (th (cdr date&theories))
				(push (list (format nil "~a~a" lib (id th))
					    (format nil "~a~a" lib file)
					    (place-list (place th)))
				      theories)))
			  (car files&theories)))
	     *prelude-libraries*)
    (sort theories #'string< :key #'car)))

(defun current-libraries ()
  (let ((libs nil))
    (maphash #'(lambda (lib files&theories)
		 (pushnew (namestring lib) libs :test #'equal))
	     *imported-libraries*)
    (maphash #'(lambda (lib files&theories)
		 (pushnew (namestring lib) libs :test #'equal))
	     *prelude-libraries*)
    (sort libs #'string<)))

(defun library-file? (pvsfile)
  (let ((lib (library-of (namestring (make-pathname
				      :directory (pathname-directory pvsfile)))))
	(file (pathname-name pvsfile))
	(ext (pathname-type pvsfile)))
    (when (and lib file
	       (or (null ext) (string= ext "pvs")))
      (let ((hash (car (or (gethash lib *imported-libraries*)
			   (gethash lib *prelude-libraries*)))))
	(when hash
	  (and (gethash file hash)
	       t))))))
