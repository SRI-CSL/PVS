;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils.lisp -- 
;; Author          : Sam Owre and N. Shankar
;; Created On      : Thu Dec  2 13:31:00 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Jun 30 17:22:59 1999
;; Update Count    : 92
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

;;(proclaim '(inline resolution))

(export '(argument* copy-all copy-context current-declarations-hash
	  current-theory current-theory-name current-using-hash file-older
	  find-supertype get-theory lf make-new-context mapappend operator*
	  put-decl pvs-current-directory resolution show))

(declaim (notinline current-theory))

(defun current-theory ()
  (when *current-context*
    (theory *current-context*)))

(defsetf current-theory () (theory)
  (assert *current-context*)
  `(setf (theory *current-context*) ,theory))

(defun current-theory-name ()
  (assert *current-context*)
  (theory-name *current-context*))

(defsetf current-theory-name () (name)
  `(setf (theory-name *current-context*) ,name))

(defun current-declaration ()
  (assert *current-context*)
  (declaration *current-context*))

(defsetf current-declaration () (decl)
  `(setf (declaration *current-context*) ,decl))

(defun current-declarations-hash ()
  (assert *current-context*)
  (declarations-hash *current-context*))

(defsetf current-declarations-hash () (decl-hash)
  `(setf (declarations-hash *current-context*) ,decl-hash))

(defun current-using-hash ()
  (assert *current-context*)
  (using-hash *current-context*))

(defsetf current-using-hash () (using-hash)
  `(setf (using-hash *current-context*) ,using-hash))

(defun current-library-alist ()
  (assert *current-context*)
  (library-alist *current-context*))

(defsetf current-library-alist () (lib-alist)
  `(setf (library-alist *current-context*) ,lib-alist))

(defun current-known-subtypes ()
  (assert *current-context*)
  (known-subtypes *current-context*))

(defsetf current-known-subtypes () (known-subtypes)
  `(setf (known-subtypes *current-context*) ,known-subtypes))

(defun current-judgements ()
  (assert *current-context*)
  (judgements *current-context*))

(defsetf current-judgements () (judgements)
  `(setf (judgements *current-context*) ,judgements))

(defun current-conversions ()
  (assert *current-context*)
  (conversions *current-context*))

(defsetf current-conversions () (conversions)
  `(setf (conversions *current-context*) ,conversions))

(defmethod initialize-instance :around ((obj syntax) &rest initargs)
  (let ((place (getf initargs 'place)))
    (when place
      (remf initargs 'place))
    (call-next-method)
    (when place
      (setf (place obj) place))))

#-(or allegro cmu sbcl)
(defun file-exists-p (file)
  (probe-file file))

(defun mapappend (fun list)
  (mapcan #'copy-list (mapcar fun list)))

#+lucid
(defmethod copy ((ht hash-table) &rest args)
  (let ((new-ht (make-hash-table :test (hash-table-test ht))))
    (maphash #'(lambda (id data)
		 (setf (gethash id new-ht) data))
	     ht)
    new-ht))

#-(or allegro lucid)
(defmethod copy (obj &rest args)
  (if (typep obj 'hash-table)
      (let ((new-ht (make-hash-table :test (hash-table-test obj))))
	(maphash #'(lambda (id data)
		     (setf (gethash id new-ht) data))
		 obj)
	new-ht)
      (error "copy called for unknown type: ~a" (type-of obj))))

(defmethod copy ((ht hash-table) &rest args)
  (let* ((test (hash-table-test ht))
	 (size (hash-table-count ht))
	 (weak? #+allegro (excl:hash-table-weak-keys ht)
		#+cmu     (lisp::hash-table-weak-p ht)
		#+sbcl    (sb-ext:hash-table-weakness ht))
	 (new-ht (if (memq test '(eq eql equal equalp))
		     (make-hash-table
		      :test test :size size
		      #+allegro :weak-keys #+cmu :weak-p #+sbcl :weakness weak?)
		     (make-pvs-hash-table :strong-eq? (eq test 'strong-tc-eq)
					  :size size
					  :weak-keys? weak?))))
    (maphash #'(lambda (id data)
		 (setf (gethash id (the hash-table new-ht)) data))
	     (the hash-table ht))
    new-ht))

;;; Used to copy a hash table that is not expected to grow
(defun copy-static-hash (ht)
  (let* ((test (hash-table-test ht))
	 (size (floor (hash-table-count ht) .95))
	 (new-ht (if (memq test '(eq eql equal equalp))
		     (make-hash-table :test test :size size
				      :rehash-threshold 1.0)
		     (make-pvs-hash-table :strong-eq? (eq test 'strong-tc-eq)
					  :size size
					  :rehash-threshold 1.0))))
    (maphash #'(lambda (id data)
		 (setf (gethash id (the hash-table new-ht)) data))
	     (the hash-table ht))
    new-ht))

(defmethod copy ((lht linked-hash-table) &rest args)
  (declare (ignore args))
  (make-linked-hash-table
   :table (if (hash-table-p (lhash-table lht))
	      (copy (lhash-table lht))
	      (lhash-table lht))
   :next (lhash-next lht)))

(defmethod copy :around ((ex application) &rest args)
  (declare (ignore args))
  (let ((nex (call-next-method)))
    (if (or (not (type ex))
	    (not (type nex))
	    (and (eq (operator ex) (operator nex))
		 (eq (argument ex) (argument nex))))
	nex
	(change-application-class-if-necessary ex nex))))

(defmethod copy :around ((ex equation) &rest args)
  (declare (ignore args))
  (let ((nex (call-next-method)))
    (if (or (not (type ex))
	    (not (type nex))
	    (not (type (args1 nex)))
	    (iff-or-boolean-equation? nex)
	    (not (tc-eq (find-supertype (type (args1 nex))) *boolean*)))
	nex
	(change-class nex 'infix-boolean-equation))))

;; Function composition

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
    #'identity))

(defmacro lf (file &optional force)
  `(make-file ,file ,force))

(defun make-file (file &optional force)
  (let* ((source (make-file-name file))
	 (bin (make-pathname :type cl-user::*pvs-binary-type*
			     :defaults source)))
    (unless (file-exists-p source)
      (error "~%File ~a does not exist~%" source))
    (when (or (eq force t)
	      (and (compiled-file-older-than-source? source bin)
		   (not (eq force :source))))
      (compile-file source :output-file bin)
      (chmod "g+w" bin))
    (if (eq force :source)
	#+lucid
	(load source
	      :if-source-only :load-source 
	      :if-source-newer :load-source)
	#-lucid
	(load source)
	(load bin))))

(defun make-file-name (file)
  (let* ((dir (pathname-directory file))
	 (name (pathname-name file))
	 (type (or (pathname-type file) "lisp")))
    (cond (dir
	   (let ((path (make-pathname :directory dir :name name :type type)))
	     (if (file-exists-p path)
		 path
		 (error "File ~a cannot be found" path))))
	  (t (dolist (dir *pvs-directories*)
	       (let* ((defaults (or (probe-file (format nil "~a/~a/"
						  *pvs-path* dir))
				    (directory-p dir)))
		      (path (make-pathname :name name :type type
					   :defaults defaults)))
		 (when (file-exists-p path)
		   (return-from make-file-name path))))
	     (error "File ~a.~a cannot be found in *pvs-directories*"
		    name type)))))

(defun compiled-file-older-than-source? (sourcefile binfile)
  (or (not (file-exists-p binfile))
      (file-older binfile sourcefile)))

(defun load-parser-source ()
  (lf "pvs-lexer" :source)
  (lf "pvs-parser" :source)
  (lf "pvs-sorts" :source))

(defun load-parser ()
  (lf (format nil "~a/ess/term/terms/rel/opers" *pvs-path*))
  (lf (format nil "~a/ess/term/terms/rel/sorts" *pvs-path*))
  (lf "pvs-lexer")
  (lf "pvs-parser")
  (lf "pvs-sorts")
  (lf "pvs-parse-fixes")
  (lf "ergo-runtime-fixes"))

;; Same as describe, but returns the object, rather than nil
(defun show (obj)
  (describe obj)
  obj)

(defun special-variable-p (obj)
  (and (symbolp obj)
       #+lucid (system:proclaimed-special-p obj)
       #+kcl (system:specialp obj)
       #+(and allegro (version>= 7)) (or (excl::variable-special-p obj nil)
					 (constantp obj))
       #+(and allegro (version>= 6) (not (version>= 7)))
       (excl::variable-special-p obj nil)
       #+(and allegro (not (version>= 6))) (clos::variable-special-p obj nil)
       #+cmu (eq (extensions:info variable kind obj) :special)
       #+sbcl (sb-walker:var-globally-special-p obj)
       #+harlequin-common-lisp (system:declared-special-p obj)
       #-(or lucid kcl allegro harlequin-common-lisp cmu sbcl)
       (error "Need to handle special variables for this version of lisp")
       t))


#+allegro
(defun run-program (command &key arguments)
  (let* ((shell (pathname-name (sys:getenv "SHELL")))
	 (string (format nil "~:[PATH=~;setenv PATH ~]~a;~a ~{~a ~}"
		   (member shell '("csh" "tcsh") :test #'string=)
		   (sys:getenv "PATH") command arguments)))
    (excl:shell string)))


#+lucid
(defun ls ()
  (run-program "ls" :arguments (list "-CF" (namestring (working-directory))))
  nil)

#+harlequin-common-lisp
(defun working-directory ()
  (system::get-working-directory))

#+harlequin-common-lisp
(defun ls ()
  (foreign:call-system (format nil "ls -CF ~a"
			 (namestring (working-directory))))
  nil)

#+(or gcl cmu)
(defun working-directory ()
  (pathname (nth-value 1 (unix:unix-current-directory))))

#+(or gcl cmu)
(defun set-working-directory (dir)
  (unix:unix-chdir (namestring dir)))

#+sbcl
(defun working-directory ()
  (make-pathname :directory (sb-posix:getcwd)))

#+sbcl
(defun set-working-directory (dir)
  (sb-posix:chdir dir))

#+allegro
(defun working-directory ()
  (excl:current-directory))

#+allegro
(defun set-working-directory (dir)
  (excl:chdir (pathname dir)))

#+harlequin-common-lisp
(defun set-working-directory (dir)
  (system::change-directory dir))

#+allegro
(defun environment-variable (string)
  (sys:getenv string))

#+cmu
(defun environment-variable (string)
  (tools:getenv string))

#+sbcl
(defun environment-variable (string)
  (sb-posix:getenv string))

#+harlequin-common-lisp
(defun environment-variable (string)
  ;; This didn't work before
  (getenv string))

#+gcl
(defun environment-variable (string)
  (si:getenv string))

#+lucid
(defun chmod (prot file)
  (run-program "chmod"
	       :arguments (list prot (namestring file))
	       :output "/dev/null"
	       :if-output-exists nil))

#+allegro
(defun chmod (prot file)
  (excl:run-shell-command (format nil "chmod ~a ~a" prot (namestring file))
			  :output "/dev/null"
			  :if-output-exists :append
			  :error-output "/dev/null"
			  :if-error-output-exists :append))

#+harlequin-common-lisp
(defun chmod (prot file)
  (foreign:call-system-showing-output
   (format nil "chmod ~a ~a" prot (namestring file))
   :prefix nil
   :show-cmd nil
   :output-stream (open "/dev/null" :direction :output
			  :if-exists :append)))
#+cmu
(defun chmod (prot file)
  (extensions:run-program
   "chmod"
   (list prot (namestring file))
   :output nil :error nil :wait nil))

#+sbcl
(defun chmod (prot file)
  (sb-ext:run-program
   "chmod"
   (list prot (namestring file))
   :output nil :error nil :wait nil))

#+gcl
(defun chmod (prot file)
  (system (format nil "chmod ~a ~a" prot (namestring file))))

(defun pvs-current-directory ()
  (if (file-exists-p *pvs-context-path*)
      (shortname *pvs-context-path*)
      "/dev/null"))

(defun get-formula (theory id)
  (or (find-if #'(lambda (decl)
		   (and (typep decl 'formula-decl)
			(eq (id decl) id)))
	(assuming theory))
      (find-if #'(lambda (decl)
		   (and (typep decl 'formula-decl)
			(eq (id decl) id)))
	(theory theory))))

(defmethod get-theory ((name modname))
  (with-slots (library id) name
    (get-theory* id library)))

(defmethod get-theory ((name name))
  (with-slots (mod-id library id) name
    (unless mod-id
      (get-theory* id library))))

(defmethod get-theory ((str string))
  (get-theory (pc-parse str 'modname)))

(defmethod get-theory ((id symbol))
  (get-theory* id nil))

(defmethod get-theory ((mod module))
  mod)

(defmethod get-theory ((dt recursive-type))
  dt)

#+(or gcl cmu sbcl)
(defmethod get-theory (pathname)
  (when (pathnamep pathname)
    (get-theory (pathname-name pathname))))

#-(or gcl cmu sbcl)
(defmethod get-theory ((path pathname))
  (get-theory (pathname-name path)))

(defun get-theory* (id library)
  ;;(assert (symbolp library))
  (let ((have-cc *current-context*)
	(*current-context* (or *current-context*
			       *prelude-library-context*
			       *prelude-context*)))
    (if library
	(let* ((lib-ref (get-library-reference library))
	       ;;(lib-ref (when lref (get-relative-library-reference lref)))
	       )
	  (and lib-ref
	       (let* ((imphash (cadr (gethash lib-ref *imported-libraries*)))
		      (prehash (cadr (gethash lib-ref *prelude-libraries*))))
		 (if (file-equal lib-ref *pvs-context-path*)
		     (gethash id *pvs-modules*)
		     (or (and imphash (gethash id imphash))
			 (and prehash (gethash id prehash))
			 ;;(gethash id *prelude*)
			 )))))
	(or (gethash id *pvs-modules*)
	    (gethash id *prelude*)
	    (let ((cth (when *current-context* (theory *current-context*))))
	      (when (and cth (eq (id cth) id))
		cth))
	    (car (assoc id (prelude-libraries-uselist)
			:test #'eq :key #'id))
	    (unless have-cc
	      ;; We only allow this from top-level calls, in effect,
	      ;; when there is no context.
	      (let ((theories (get-imported-theories id)))
		(if (cdr theories)
		    (progn (pvs-message
			       "Ambiguous theories: should include library - ~a"
			     (id (car theories)))
			   theories)
		    (car theories))))
	    ))))

(defun get-imported-theories (id)
  (let ((theories nil))
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (let ((th (gethash id (cadr files&theories))))
		   (when th
		     (push th theories))))
	     *imported-libraries*)
    theories))


(defmethod get-lib-id ((th library-datatype-or-theory))
  (get-lib-id (lib-ref th)))

(defmethod get-lib-id ((str string))
  (car (rassoc str (current-library-alist) :test #'equal)))

(defmethod get-lib-id ((th datatype-or-module))
  nil)

;;; Useful methods - can almost be used as accessors.

(defmethod condition ((expr if-expr))
  (car (arguments expr)))

(defmethod then-part ((expr if-expr))
  (cadr (arguments expr)))

(defmethod else-part ((expr if-expr))
  (caddr (arguments expr)))

(defmethod condition ((expr branch))
  (car (arguments expr)))

(defmethod then-part ((expr branch))
  (cadr (arguments expr)))

(defmethod else-part ((expr branch))
  (caddr (arguments expr)))

(defmethod theory ((adt recursive-type))
  nil)

(defmethod resolution ((name name))
  (with-slots ((res resolutions)) name
    (unless (or (cdr res)
		(not (resolution? (car res))))
      (car res))))

(defmethod declaration ((act actual))
  (if (type-value act)
      (when (typep (type-value act) 'type-name)
	(declaration (resolution (type-value act))))
      (when (typep (expr act) 'name-expr)
	(declaration (expr act)))))

(defmethod declaration ((name name))
  (let ((res (resolutions name)))
    (when (and res (null (cdr res)))
      (declaration (car res)))))

(defmethod declaration ((obj declaration))
  obj)

(defmethod module-instance ((name name))
  (let ((res (resolutions name)))
    (when (and res (null (cdr res)))
      (module-instance (car res)))))

(defmethod importing-param? ((x importing)) t)
(defmethod importing-param? ((x theory-abbreviation-decl)) t)
(defmethod importing-param? (x) (declare (ignore x)) nil)

(defun listify (x) (if (listp x) x (list x)))

(defun duplicates? (list &key (test #'eql) (key #'identity))
  (when list
    (or (car (member (funcall key (car list)) (cdr list) :test test :key key))
	(duplicates? (cdr list) :test test :key key))))

(defun has-id? (x)
  (or (symbolp x)(stringp x)(numberp x)
      (and (syntax? x)(slot-exists-p x 'id))))

;; Returns a symbol or integer
(defun get-id (x)
  (typecase x
    (symbol x)
    (string (intern x))
    (number x)
    (t (id x))))

(defun last-id (x)
  (let* ((str (string x))
	 (dotpos (position #\. str :from-end t)))
    (if dotpos
	(values (intern (subseq str (1+ dotpos))) t)
	(values x nil))))

(defun same-last-id (x y)
  (or (same-id x y)
      (let ((idx (get-id x))
	    (idy (get-id y)))
	(if (integerp idx)
	    (and (integerp idy)
		 (= idx idy))
	    (and (not (integerp idy))
		 (eq (last-id idx) (last-id idy)))))))

(defun same-id (x y)
  (let ((idx (get-id x))
	(idy (get-id y)))
    (eq idx idy)))

;; True if x is a suffix of y
(defun id-suffix (x y)
  (or (same-id x y)
      (let ((idx (get-id x))
	    (idy (get-id y)))
	(when (and (symbolp idx) (symbolp idy))
	  (let ((strx (string (get-id x)))
		(stry (string (get-id y))))
	    ;; Note we can't simply call suffix? here, as (suffix? "a" "th.aa") is true
	    ;; But we can assume that x is a valid id, so prepending "." will work
	    (suffix? (concatenate 'string "." strx) stry))))))

(defun id-suffixes (id)
  (id-suffixes* (string (get-id id))))

(defun id-suffixes* (string &optional ids)
  (let ((dotpos (position #\. string)))
    (if dotpos
	(id-suffixes* (subseq string (1+ dotpos))
		      (cons (intern string :pvs) ids))
	(cons (intern string :pvs) ids))))
	

(defun prefix? (x y) ;both strings
  (let ((lx (length x))
	(ly (length y)))
    (and (<= lx ly)
	 (string= x (subseq y 0 lx)))))

(defun suffix? (x y) ;both strings
  (let ((lx (length x))
	(ly (length y)))
    (and (<= lx ly)
	 (string= x (subseq y (- ly lx) ly)))))
    

;(defmethod make-specpath ((mod module-form))
;  (make-specpath (id mod)))

(defmethod make-specpath ((name symbol) &optional (ext "pvs"))
  (make-pathname :defaults *pvs-context-path* :name (string name) :type ext))

(defmethod make-specpath ((name string) &optional (ext "pvs"))
  (make-pathname :defaults *pvs-context-path* :name name :type ext))

(defmethod make-specpath ((name name) &optional (ext "pvs"))
  (make-specpath (id name) ext))

(defmethod make-binpath ((name symbol))
  (make-binpath (string name)))

(defmethod make-binpath ((name string))
  (make-pathname :defaults *pvs-context-path*
		 :directory (append (pathname-directory *pvs-context-path*)
				    (list #+case-sensitive "pvsbin"
					  #-case-sensitive "PVSBIN"))
		 :name name
		 :type "bin"))

(defun shortname (directory)
  (let* ((home (namestring (truename (user-homedir-pathname))))
	 (cdir (namestring (truename directory))))
    (cond ((and (< (length home) (length cdir))
		(string= cdir home :end1 (length home)))
	   (concatenate 'string "~/" (subseq cdir (length home))))
	  ((and (< 8 (length cdir))
		(string= cdir "/tmp_mnt" :end1 8))
	   (let ((ndir (subseq cdir 8)))
	     (if (file-exists-p ndir)
		 ndir
		 cdir)))
	  (t cdir))))

(defvar *shortpath-directories*
  (make-hash-table :test #'equal))

(defun shortpath (directory)
  (or (gethash directory *shortpath-directories*)
      (let* ((realdir (namestring (truename directory)))
	     (dirlist (pathname-directory
		       (directory-p
			#+allegro (excl:pathname-resolve-symbolic-links realdir)
			#+cmu (unix:unix-resolve-links realdir)
			#-(or allegro cmu) realdir)))
	     (file-info (get-file-info directory))
	     (result (if (eq (car dirlist) :absolute)
			 (shortpath* (reverse (cdr dirlist)) file-info)
			 directory)))
	(setf (gethash directory *shortpath-directories*) result))))

(defun shortpath* (revdirlist file-info &optional dirlist)
  (let ((path (make-pathname :directory (cons :absolute dirlist))))
    (if (equal (get-file-info path) file-info)
	path
	(shortpath* (cdr revdirlist) file-info
		    (cons (car revdirlist) dirlist)))))

(defun relative-path (path &optional (relpath *pvs-context-path*) (depth 2))
  (let ((dirlist (pathname-directory (shortpath path)))
	(reldirlist (pathname-directory (shortpath relpath))))
    (when (and (eq (car dirlist) :absolute)
	       (eq (car reldirlist) :absolute))
      (or (relative-path* (cdr dirlist) (cdr reldirlist) depth)
	  (namestring path)))))

(defun relative-path* (dirlist reldirlist depth)
  (if (and dirlist
	   reldirlist
	   (string= (car dirlist) (car reldirlist)))
      (relative-path* (cdr dirlist) (cdr reldirlist) depth)
      (when (or (null depth)
		(and (integerp depth)
		     (>= depth (length reldirlist))))
	(let ((reldir
	       (namestring
		(make-pathname :directory
			       (cons :relative
				     (nconc (make-list (length reldirlist)
						       :initial-element
						       #+allegro :back
						       #+(or cmu sbcl) :up)
					    dirlist))))))
	  (if (and dirlist (null reldirlist))
	      (concatenate 'string "./" reldir)
	      reldir)))))

;;; Checks if the dir is in fact a directory; returns the expanded
;;; pathname ending with a slash.

#-(or allegro cmu sbcl)
(defun directory-p (dir)
  (let* ((dirstr (namestring dir))
	 (dirslash (merge-pathnames
		    (if (char= (char dirstr (1- (length dirstr))) #\/)
			dirstr
			(concatenate 'string dirstr "/"))
		    *pvs-context-path*))
	 (dirnoslash (merge-pathnames
		      (if (char= (char dirstr (1- (length dirstr))) #\/)
			  (subseq dirstr 0 (1- (length dirstr)))
			  dirstr)
		      *pvs-context-path*)))
    (cond ((not (probe-file dirnoslash))
	   (values nil (format nil "Directory ~a does not exist." dir)))
	  ((not (probe-file dirslash))
	   (values nil (format nil "~a is not a directory." dir)))
	  (t dirslash))))

(defvar *subdirectory-hash* (make-hash-table :test #'equal))

(defun subdirectoryp (dir1 dir2)
  (multiple-value-bind (subdir there?)
      (gethash (list dir1 dir2) *subdirectory-hash*)
    (if there?
	subdir
	(setf (gethash (list dir1 dir2) *subdirectory-hash*)
	      (when (and (directory-p dir1)
			 (directory-p dir2))
		(let* ((dirinfos (mapcar #'(lambda (d)
					     (cons d (get-file-info d)))
				   (directory-path dir1)))
		       (tdir1 (caar (last dirinfos)))
		       (tdir2+ (namestring (truename dir2)))
		       (tdir2 (if (char= (char tdir2+ (1- (length tdir2+)))
					 #\/)
				  (subseq tdir2+ 0 (1- (length tdir2+)))
				  tdir2+))
		       (dirinfo2 (get-file-info tdir2))
		       (comdir (car (find-if #'(lambda (d)
						 (equal (cdr d) dirinfo2))
				      dirinfos))))
		  (when comdir
		    (subseq tdir1 (length comdir)))))))))

(defun directory-path (dir)
  (let ((truedir (if (directory-p dir)
		     (namestring (truename dir))
		     dir)))
    (if (char= (char truedir (1- (length truedir))) #\/)
	(directory-path* (subseq truedir 0 (1- (length truedir))))
	(directory-path* truedir))))

(defun directory-path* (dir &optional path)
  (let ((pos (position #\/ dir :from-end t)))
    (if pos
	(directory-path* (subseq dir 0 pos)
			 (cons dir path))
	(if (string= dir "")
	    path
	    (cons dir path)))))


(defun splice (new-elt after-elt list)
  (let ((tail (and after-elt (memq after-elt list))))
    (if tail
	(append (ldiff list tail) (cons new-elt tail))
	(cons new-elt tail))))
      

;;(defmethod generated-by ((u importing)) nil)

(defun add-decl-test (x y)
  (and (eq (kind-of x) (kind-of y))
       (case (kind-of x)
	 (judgement (add-decl-test* x y))
	 (conversion (and (eq (class-of x) (class-of y))
			  (tc-eq (type x) (type y))))
	 (auto-rewrite (and (if (auto-rewrite-minus-decl? x)
				(auto-rewrite-minus-decl? y)
				(not (auto-rewrite-minus-decl? y)))
			    (tc-eq (rewrite-names x)
				   (rewrite-names y))))
	 (expr (tc-eq (type x) (type y)))
	 (t t))))

(defmethod add-decl-test* ((x subtype-judgement) (y subtype-judgement))
  (and (tc-eq (type x) (type y))
       (tc-eq (subtype x) (subtype y))))

(defmethod add-decl-test* ((x number-judgement) (y number-judgement))
  (and (= (number (number-expr x)) (number (number-expr y)))
       (tc-eq (type x) (type y))))

(defmethod add-decl-test* ((x name-judgement) (y name-judgement))
  (and (eq (declaration (name x)) (declaration (name y)))
       (tc-eq (type x) (type y))))

(defmethod add-decl-test* ((x application-judgement) (y application-judgement))
  (and (eq (declaration (name x)) (declaration (name y)))
       (tc-eq (judgement-type x) (judgement-type y))))

(defmethod add-decl-test* (x y)
  (declare (ignore x y))
  nil)


;;; Simple file copy function.  See SunCL Advanced User's Guide for
;;; future expansions (e.g. error handling, etc.)

#+lucid
(defun copy-file (from to)
  (run-program "cp" :arguments (list (namestring from) (namestring to))
	       :if-output-exists :supersede))

#+allegro
(defun copy-file (from to)
  (excl:run-shell-command
   (format nil "cp ~a ~a" (namestring from) (namestring to))))

#+harlequin-common-lisp
(defun copy-file (from to)
  (foreign:call-system
   (format nil "cp ~a ~a" (namestring from) (namestring to))))

#+gcl
(defun copy-file (from to)
  (system
   (format nil "cp ~a ~a" (namestring from) (namestring to))))

#+gcl
(defun write-permission? (&optional (dir *pvs-context-path*))
  (let ((path (make-pathname :defaults dir :name "PVS" :type "tmp"))
	(*break-enable* nil)
	(error t))
    (with-output-to-string (*debug-io*)
      (with-output-to-string (*error-output*)
	(multiple-value-bind (err str)
	    (si:error-set `(open ,path :direction :output
			    :if-exists :append
			    :if-does-not-exist :create))
	  (unless err (close str) (delete-file path) (setq error nil)))))
    (not error)))

#+(or lucid harlequin-common-lisp)
(defun write-permission? (&optional (dir *pvs-context-path*))
  (let* ((path (make-pathname :defaults dir :name "PVS" :type "tmp"))
	 (str (ignore-errors (open path :direction :output
				   :if-exists :append
				   :if-does-not-exist :create))))
    (when str (close str) (delete-file path) t)))


;(defmethod module-name ((n name)) (mod-id n))
;(defmethod actual-params ((n name)) (actuals n))

;(defmethod type ((expr expr))
;  (when (singleton? (types expr))
;    (car (types expr))))


; (defmethod conjunction? ((expr application))
;   (with-slots ((op operator)) expr
;     (boolean-op? op '(and &))))

; (defmethod conjunction? (expr)
;   (declare (ignore expr))
;   nil)

; (defmethod disjunction? ((expr application))
;   (with-slots ((op operator)) expr
;     (boolean-op? op '(or))))

; (defmethod disjunction? (expr)
;   (declare (ignore expr))
;   nil)

; (defmethod implication? ((expr application))
;   (with-slots ((op operator)) expr
;     (boolean-op? op '(implies =>))))

; (defmethod implication? (expr)
;   (declare (ignore expr))
;   nil)

; (defmethod negation? ((expr application))
;   (with-slots ((op operator)) expr
;     (boolean-op? op '(not))))

; (defmethod negation? (expr)
;   (declare (ignore expr))
;   nil)

; (defmethod iff? ((expr application))
;   (with-slots ((op operator)) expr
;     (or (boolean-op? op '(iff <=>))
; 	(boolean-equality-op? op))))

; (defmethod iff? (expr)
;   (declare (ignore expr))
;   nil)

(defmethod boolean-when-expr? ((expr application))
  (with-slots ((op operator)) expr
    (boolean-op? op '(WHEN))))

(defmethod boolean-when-expr? (expr)
  (declare (ignore expr))
  nil)

(defmethod boolean-op? ((op name-expr) id-list)
  (with-slots (id resolutions) op
    (and (memq id id-list)
	 (eq (id (module-instance (car resolutions))) '|booleans|))))

(defmethod boolean-op? (op id-list)
  (declare (ignore op id-list))
  nil)

(defmethod boolean-equality-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '=)
	 (let ((mi (module-instance (car resolutions))))
	   (and (eq (id mi) '|equalities|)
		(tc-eq (find-supertype (type-value (car (actuals mi))))
		       *boolean*))))))

(defmethod boolean-equality-op? (op)
  (declare (ignore op))
  nil)

(defmethod relation-type? ((te funtype))
  (with-slots (domain range) te
    (and (tc-eq (find-supertype range) *boolean*)
	 (relation-type-domain? domain))))

(defmethod relation-type? ((te subtype))
  (with-slots (supertype) te
    (relation-type? supertype)))

(defmethod relation-type? (te)
  (declare (ignore te))
  nil)

(defmethod relation-type-domain? ((te tupletype))
  (with-slots (types) te
    (and (= (length types) 2)
	 (tc-eq (car types) (cadr types)))))

(defmethod relation-type-domain? (te)
  (declare (ignore te))
  nil)


(defmethod boolean-binop-type? ((te funtype))
  (with-slots (domain range) te
    (and (tc-eq (find-supertype range) *boolean*)
	 (boolean-binop-domain-type? domain))))

(defmethod boolean-binop-type? (te)
  (declare (ignore te))
  nil)

(defmethod boolean-binop-domain-type? ((te tupletype))
  (with-slots (types) te
    (every #'(lambda (ty) (tc-eq (find-supertype ty) *boolean*))
	   types)))

(defmethod boolean-binop-domain-type? (te)
  (declare (ignore te))
  nil)


(defmethod context ((theory module))
  (if (saved-context theory)
      (copy-context (saved-context theory))
      (let ((last-decl (car (last (or (theory theory)
				   (assuming theory)
				   (formals theory))))))
	(if last-decl
	    (decl-context last-decl t)
	    (make-new-context theory)))))

(defmethod context ((using importing))
  (decl-context using))

(defmethod context ((decl declaration))
  (decl-context decl))

(defun decl-context (decl &optional include?)
  (let* ((*generate-tccs* 'none)
	 (theory (module decl))
	 (libalist (when *current-context*
		     (library-alist *current-context*))) ;; Before we change
	 (all-decls (reverse (all-decls theory)))
	 (pdecls (or (memq decl all-decls) (cons decl all-decls)))
	 (prev-decls (if include?
			 pdecls
			 (cdr pdecls)))
	 (prev-imp (find-if #'mod-or-using? prev-decls))
	 (rem-decls (if (and prev-imp (saved-context prev-imp))
			(ldiff prev-decls (memq prev-imp prev-decls))
			prev-decls))
	 (*current-theory* theory)
	 (*current-context*
	  (cond ((and prev-imp (saved-context prev-imp))
		 (copy-context (saved-context prev-imp)
			       theory (reverse rem-decls)
			       (or (car rem-decls) decl)))
		((from-prelude? decl)
		 (let ((prevp
			(cadr (memq theory
				    (reverse
				     *prelude-theories*)))))
		   (copy-context (saved-context
				  (if (datatype? prevp)
				      (or (adt-reduce-theory prevp)
					  (adt-map-theory prevp)
					  (adt-theory prevp))
				      prevp))
				 theory
				 (reverse rem-decls)
				 (or (car rem-decls) decl))))
		(t (make-new-context theory)))))
    ;;; Need to clear this hash or the known-subtypes table won't get
    ;;; updated properly - see add-to-known-subtypes.
    (clrhash *subtype-of-hash*)
    (dolist (d (reverse rem-decls))
      (typecase d
	(lib-decl
	 (check-for-importing-conflicts d)
	 (put-decl d))
	((or mod-decl theory-abbreviation-decl formal-theory-decl)
	 (put-decl d)
	 (let* ((thname (theory-name d))
		(th (get-theory thname)))
	   (add-usings-to-context* th thname))
	 (setf (saved-context d) (copy-context *current-context*)))
	(importing
	 (let* ((thname (theory-name d))
		(th (get-theory* (id thname)
				 (or (library thname)
				     (and (library-datatype-or-theory? theory)
					  (car (rassoc (lib-ref theory) libalist
						       :test #'string=)))))))
	   (assert th)
	   (add-usings-to-context* th thname))
	 (setf (saved-context d) (copy-context *current-context*)))
	;;(subtype-judgement (add-to-known-subtypes (subtype d) (type d)))
	(judgement (add-judgement-decl d t))
	(conversionminus-decl (disable-conversion d))
	(conversion-decl (push d (conversions *current-context*)))
	(auto-rewrite-minus-decl (push d (disabled-auto-rewrites
					 *current-context*)))
	(auto-rewrite-decl (add-auto-rewrite-to-context  d))
	(type-def-decl (unless (enumtype? (type-expr d))
			 (put-decl d)))
	(declaration (put-decl d))
	(datatype nil)))
    (when (from-prelude? decl)
      (let* ((prevp (cadr (memq theory
				(reverse *prelude-theories*))))
	     (pths (if (datatype? prevp)
		       (delete-if #'null
			 (list (adt-theory prevp)
			       (adt-map-theory prevp)
			       (adt-reduce-theory prevp)))
		       (list prevp))))
	(dolist (pth pths)
	  (setf (get-importings pth)
		(list (mk-modname (id pth)))))))
    (setf (declaration *current-context*) decl)
    (update-context-importing-for-mapped-tcc decl)
    *current-context*))

(defmethod update-context-importing-for-mapped-tcc ((decl mapped-axiom-tcc))
  (assert (theory-instance decl))
  (let* ((thname (theory-instance decl))
	 (th (get-theory thname))
	 (thdecls (all-decls th))
	 (prev-decls (ldiff thdecls (memq decl thdecls)))
	 (*insert-add-decl* nil)
	 (imp-context (when (and (generated-by decl)
				 (typep (generated-by decl)
					'(or importing theory-abbreviation-decl
					     mod-decl formal-theory-decl)))
			(saved-context (generated-by decl)))))
    ;;; Want something like add-usings-to-context*, but only for those
    ;;; importings that precede the given declaration.
    (when imp-context
      (dolist (entry (library-alist imp-context))
	(pushnew entry (current-library-alist) :key #'car))
      (add-to-using thname th)
      (add-preceding-importings prev-decls th thname))))

(defmethod update-context-importing-for-mapped-tcc ((decl assuming-tcc))
  (let* ((thname (theory-instance decl))
	 (th (module (generating-assumption decl)))
	 (thdecls (all-decls th))
	 (prev-decls (ldiff thdecls (memq decl thdecls)))
	 (*insert-add-decl* nil)
	 (imp-context (when (and (generated-by decl)
				 (typep decl
					'(or importing theory-abbreviation-decl
					     mod-decl formal-theory-decl)))
			(saved-context (generated-by decl)))))
    (when imp-context
      (dolist (entry (library-alist imp-context))
	(pushnew entry (current-library-alist) :key #'car))
      (add-to-using thname th)
      (add-preceding-importings prev-decls th thname))))

(defun add-preceding-importings (prev-decls theory thinst)
  (dolist (d prev-decls)
    (typecase d
      ((or importing mod-decl theory-abbreviation-decl formal-theory-decl)
       (let* ((lthname (if (and (library-datatype-or-theory? theory)
				(null (library (theory-name d))))
			   (copy (theory-name d)
			     'library (libref-to-libid (lib-ref theory)))
			   (theory-name d)))
	      (thname (subst-mod-params lthname thinst theory))
	      (th (get-theory lthname)))
	 (add-usings-to-context* th thname))))))

(defmethod update-context-importing-for-mapped-tcc (decl)
  (declare (ignore decl))
  nil)

(defmethod saved-context ((adt recursive-type))
  (when (adt-theory adt)
    (saved-context (adt-theory adt))))
	      

(defmethod add-imported-assumings ((decl assuming-tcc))
  (add-usings-to-context (list (theory-instance decl))))

(defmethod add-imported-assumings (decl)
  (declare (ignore decl))
  nil)

(defmethod add-immediate-importings-to-context (decl)
  (declare (ignore decl))
  nil)

(defmethod add-immediate-importings-to-context ((decl tcc-decl))
  (let ((modinst (car (importing-instance decl)))
	(gdecl (generated-by decl)))
    (when (and modinst (typep gdecl 'importing))
      (add-usings-to-context (theory-name gdecl)))))

(defun remove-disallowed-decls-from-context (decl prevdecls)
  (if (and (tcc? decl)
	   (typep (generated-by decl) 'formal-decl))
      (let* ((adecls (memq (generated-by decl) prevdecls))
	     (fdecls (ldiff prevdecls adecls))
	     (pdecls (remove-if #'(lambda (d) (typep d 'formal-decl))
		       adecls))
	     (badass (find-if #'(lambda (d)
				 (and (typep d 'formula-decl)
				      (eq (spelling d) 'ASSUMPTION)
				      (some #'(lambda (dd)
						(and (typep dd 'formal-decl)
						     (not (memq dd fdecls))))
					    (refers-to d))))
		       pdecls)))
	(append fdecls
		(if badass
		    (ldiff pdecls (memq badass pdecls))
		    pdecls)))
      prevdecls))

(defmethod add-conversions-to-context (decl)
  (declare (ignore decl))
  nil)

(defmethod add-conversions-to-context ((decl conversion-decl))
  (push decl (conversions *current-context*)))


(defun make-new-context (theory)
  (let ((pctx (or *prelude-library-context*
		  *prelude-context*)))
    (if pctx
	(let ((*current-context*
	       (make-instance 'context
		 :theory theory
		 :theory-name (mk-modname (id theory))
		 :using-hash (if *loading-prelude*
				 (copy (using-hash pctx))
				 (copy-lhash-table (using-hash pctx)))
		 :declarations-hash (if *loading-prelude*
					(copy (declarations-hash pctx))
					(copy-lhash-table
					 (declarations-hash pctx)))
		 :known-subtypes (known-subtypes pctx)
		 :conversions (conversions pctx)
		 :disabled-conversions (copy-list (disabled-conversions pctx))
		 :auto-rewrites (copy-list (auto-rewrites pctx))
		 :disabled-auto-rewrites (copy-list
					  (disabled-auto-rewrites pctx)))))
	  
	  (setf (judgements *current-context*)
		(if *loading-prelude*
		    (set-prelude-context-judgements (judgements pctx))
		    (copy-judgements (judgements pctx))))
	  *current-context*)
	(make-instance 'context
	  :theory theory
	  :theory-name (mk-modname (id theory))
	  :using-hash (make-lhash-table :test 'eq)
	  :declarations-hash (make-lhash-table :test 'eq)))))

(defun copy-using-hash (ht)
  (let* ((size (floor (hash-table-size ht) 1.5384616))
	 (new-ht (make-hash-table :test 'eq :size size)))
    (maphash #'(lambda (th thinsts)
		 (setf (gethash th (the hash-table new-ht))
		       (copy-list thinsts)))
	     (the hash-table ht))
    new-ht))

(defun copy-context (context &optional theory decls current-decl)
  (let ((*current-theory* (or theory (theory context)))
	(*current-context*
	 (make-instance 'context
	   :theory (or theory (theory context))
	   :theory-name (if theory
			    (mk-modname (id theory))
			    (theory-name context))
	   :declaration (or (car (last decls))
			    current-decl
			    (declaration context))
	   :declarations-hash (copy (declarations-hash context))
	   :using-hash (copy (using-hash context))
	   :library-alist (library-alist context)
	   :conversions (conversions context)
	   :disabled-conversions (copy-list (disabled-conversions context))
	   :known-subtypes (known-subtypes context)
	   :auto-rewrites (copy-list (auto-rewrites context))
	   :disabled-auto-rewrites (copy-list (disabled-auto-rewrites context)))))
    (setf (judgements *current-context*)
	  (if (from-prelude? *current-theory*)
	      (set-prelude-context-judgements (judgements context))
	      (copy-judgements (judgements context))))
    *current-context*))

(defun copy-prover-context (&optional (context *current-context*))
  (assert *in-checker*)
  (assert (declaration context))
  (copy context))

(defmethod context (ignore)
  (declare (ignore ignore))
  (copy-context (or *prelude-library-context* *prelude-context*)))

(defun add-usings-to-context (modinsts)
  (when modinsts
    (add-usings-to-context* (get-theory (car modinsts)) (car modinsts))
    (add-usings-to-context (cdr modinsts))))

(defmethod add-usings-to-context* ((theory module) inst)
  (add-to-using inst theory)
  (add-exporting-with-theories theory inst))

(defmethod add-usings-to-context* ((adt recursive-type) inst)
  (let ((acts (actuals inst)))
    (add-usings-to-context
     (mapcar #'(lambda (gen)
		 (when gen
		   (let ((frms (formals-sans-usings gen)))
		     (cond ((length= acts frms)
			    (mk-modname (id gen) (actuals inst)))
			   (t (mk-modname (id gen) nil))))))
	     (delete-if #'null
			(list (adt-theory adt)
			      (adt-map-theory adt)
			      (adt-reduce-theory adt)))))))

(defmethod module ((using importing))
  (let ((utheory nil))
    (maphash #'(lambda (id theory)
		 (declare (ignore id))
		 (unless utheory
		   (when (or (memq using (formals theory))
			     (memq using (assuming theory))
			     (memq using (theory theory)))
		     (setq utheory theory))))
	     *pvs-modules*)
    utheory))

(defmethod module ((ctx context))
  (theory ctx))


;;; lambda-depth - returns the number of lambdas in the
;;; right-hand-side of the given definition.  E.g.
;;;   foo(a:int):[int -> [int -> int]] = (lam x: (lam y: a))
;;; would return 3.  Returns nil if the declaration does not have a
;;; definition.

(defmethod lambda-depth ((decl const-decl))
  (when (definition decl)
    (+ (lambda-depth (definition decl))
       (length (formals decl)))))

(defmethod lambda-depth ((decl def-decl))
  (when (definition decl)
    (+ (lambda-depth (definition decl))
       (length (formals decl)))))

(defmethod lambda-depth ((expr lambda-expr))
  (1+ (lambda-depth (expression expr))))

(defmethod lambda-depth ((expr expr))
  0)

(defmethod lambda-depth (obj)
  (declare (ignore obj))
  nil)


;;; create-formula - creates a formula for a given const-decl or def-decl.

(defmethod create-formulas ((sym symbol) &optional (context *current-context*))
  (let* ((*current-context* context)
	 (*generate-tccs* 'none)
	 (name (parse :string (symbol-name sym) :nt 'expr))
	 (formula-resolutions (resolve name 'formula nil))
	 (formula-fmlas (loop for res in formula-resolutions
			      nconc (create-formulas res)))
	 (constant-resolutions
	  (loop for res in (resolve name 'expr nil)
		when (definition (declaration res))
		collect res))
	 (definition-fmlas
	     (loop for res in constant-resolutions
		   nconc (create-formulas res)))
	 (all-fmlas (nconc formula-fmlas definition-fmlas)))
    (or all-fmlas
	(type-error nil "No resolution for ~a" name))))

(defvar *no-expected* nil
  "Controls whether universal-closure typechecks with an expected type.")

(defmethod create-formulas ((res resolution) &optional (ctx *current-context*))
  (let ((*current-context* ctx)
	(*substit-dont-simplify* t)
	(hashentry (gethash res *create-formulas-cache*))
	(decl (declaration res)))
    (if hashentry hashentry
	(let ((formulas (create-formulas* res decl)))
	  (setf (gethash res *create-formulas-cache*) formulas)
	  formulas))))

(defun create-formulas* (res decl)	  
  (cond ((formula-decl? decl)
	 (unless (closed-definition decl)
	   (let* ((*in-checker* nil)
		  (*current-context* (context decl)))
	     (setf (closed-definition decl)
		   (universal-closure (definition decl)))))
	 (let ((*no-expected* t))
	   (list
	    (subst-mod-params (closed-definition decl)
			      (module-instance res)
			      (module decl)))))
	((typep decl '(or const-decl def-decl))
	 (copy-list (subst-mod-params (def-axiom decl)
				      (module-instance res)
				      (module decl))))
	(t nil)))

;(defun create-formula (decl modinst num)
;  (create-definition-formula
;   (subst-mod-params (def-axiom decl) modinst) num))

;(defmethod create-formula ((decl declaration) &optional (num 0))
;  (when (and (typep decl '(or const-decl def-decl))
;	     (definition decl))
;    (unless (typep num `(integer 0 ,(lambda-depth decl)))
;      (error "create-formula called with number out of range"))
;    (let* ((*generating-tcc* t)		; TCCs have already been generated
;	   ;;mk-application (NSH:8/91)
;
;	   (name  (mk-name-expr (id decl)))
;	   (def (mk-lambda-exprs (formals decl) (definition decl)))
;;	   (lhs (create-formula-lhs name def num))
;;	   (rhs (create-formula-rhs def num))
;	   (appl (typecheck (mk-application '= name def) :expected *boolean*)))
;      (create-definition-formula
;       (typecheck appl :expected *boolean*) num))))
;;      (universal-closure appl))))


;;; The following two functions create the lhs and rhs of the formula
;;; resulting from a definition, recursively eliminating the top-level
;;; lambdas, e.g.
;;;   c:[int -> [int -> int]] = (lambda i: (lambda j: i + j))
;;; generates the formula
;;;   c: formula = c(i)(j) = i + j

;;(NSH:9-14)create-definition-formula replaces create-formula-lhs and
;;create-formula-rhs since it creates the typechecked, universally
;;closed form of the entire definition formula.  
(defun create-definition-formula (defn num)
  (if (zerop num)
      defn
      (let* ((defn (create-definition-formula defn (1- num)))
	     (forall? (forall? defn))
	     (forall-vars (if forall? (bindings defn) nil))
	     (equality (if forall? (expression defn) defn))
	     (rhs (if (typep (args2 equality) 'implicit-conversion)
		      (args1 (args2 equality))
		      (args2 equality)))
	     (rhs-bindings (if (lambda? rhs) (bindings rhs) nil))
	     (varlist (mapcar #'mk-name-expr rhs-bindings))
	     (newbindings (append forall-vars rhs-bindings))
	     (*bound-variables* newbindings)
	     (new-lhs (typecheck (mk-application* (args1 equality) varlist)
			:expected (find-supertype (type (expression rhs)))))
	     (new-rhs (expression rhs))
	     (new-appl (make!-equation new-lhs new-rhs))
	     (def-form (close-freevars new-appl *current-context*
				       newbindings nil nil)))
	(assert (equation? (expression def-form)))
	def-form)))

;(defmethod expression ((ex implicit-coercion))
;  (expression (args1 ex)))

;(defun create-formula-lhs (op body num)
;  (if (zerop num)
;      op
;      (create-formula-lhs (mk-application* op
;			    (mapcar #'mk-name-expr (bindings body)))
;			  (expression body)
;			  (1- num))))
;
;(defun create-formula-rhs (body num)
;  (if (zerop num)
;      body
;      (create-formula-rhs (expression body) (1- num))))


;;; Called from tcdecls after typechecking a const-decl that has a
;;; definition or a def-decl.  Sets the def-axiom slot to the resulting
;;; formula, which is used in create-formula.

(defun make-def-axiom (decl)
  (let* ((*generate-tccs* 'none)
	 (def (make!-lambda-exprs (formals decl) (definition decl)))
	 (res (mk-resolution decl (current-theory-name) (type decl)))
	 (name (mk-name-expr (id decl) nil nil res))
	 (appl (make!-equation name def))
	 (depth (lambda-depth decl)))
    (assert (eq (declaration name) decl))
    (loop for i from 0 to depth
	  do (push (create-definition-formula appl i)
		   (def-axiom decl)))))

(defmethod def-axiom ((map mapping))
  (when (name-expr? (expr (rhs map)))
    (full-name (def-axiom (declaration (expr (rhs map)))) 1 t)))

(defmethod def-axiom (obj)
  (declare (ignore obj))
  nil)

(defun mk-lambda-exprs (varslist expr)
  (if (null varslist)
      expr
      (let ((lexpr (mk-lambda-expr (car varslist)
		     (mk-lambda-exprs (cdr varslist) expr))))
	(setf (place lexpr) (place expr))
	lexpr)))

(defun make!-lambda-exprs (varslist expr)
  (if (null varslist)
      expr
      (let ((lexpr (make!-lambda-expr (car varslist)
		     (make!-lambda-exprs (cdr varslist) expr))))
	(setf (place lexpr) (place expr))
	lexpr)))

(defun typed-lambda-vars (expr end-expr &optional vars)
  (if (eq expr end-expr)
      vars
      (typed-lambda-vars (expression expr)
			 end-expr
			 (nconc vars
				(mapcar #'(lambda (bind)
					    (if (declared-type bind)
						bind
						(copy bind
						  'declared-type (type bind))))
					(bindings expr))))))


(defmethod actuals ((n null))
  nil)

(defun universal-closure (form)
  (let ((freevars-form (sort-freevars
			(set-difference (freevars form)
					*keep-unbound*
					:test #'(lambda (x y)
						  (eq (declaration x) y))))))
    (close-freevars form *current-context* freevars-form nil nil)))

(defun existential-closure (form)
  (let ((freevars-form (sort-freevars
			(set-difference (freevars form)
					*keep-unbound*
					:test #'(lambda (x y)
						  (eq (declaration x) y))))))
    (close-freevars form *current-context* freevars-form t nil)))

(defun close-freevars (form context freevars-form &optional exist? fresh-quant?)
  (let ((*current-context* context))
    (cond ((null freevars-form) form)
	  ((and (not fresh-quant?)
		(or (and (forall-expr? form)
			 (not exist?))
		    (and (exists-expr? form)
			 exist?)))
	   (multiple-value-bind (newbindings new?)
	       (var-to-binding freevars-form form)
	     ;; CRW 7/27/94: fixed to do the substit before changing the
	     ;; bindings (when it was using the other order, substit
	     ;; was alpha-renaming the bindings to avoid capture)
	     (let* ((nform
		     (if new?
			 (freevar-substit form freevars-form newbindings)
			 form))
		    ;;(sbindings (minimal-sort-bindings
			;;	(reverse newbindings) (bindings nform)))
		    (ibindings (insert-bindings (reverse newbindings) (bindings nform))))
	       ;;(unless (equal sbindings ibindings)
		 ;;(break "Unequal"))
	       (when (and (not (eq (car (last newbindings))
				   (declaration (car (last freevars-form)))))
			  (tc-eq (type (car (last newbindings)))
				 (type (car (bindings nform)))))
		 (setf (chain? (car (last newbindings))) t))
	       (let ((cform (copy nform
			      'bindings ibindings
			      'commas? nil)))
		 #+pvsdebug
		 (assert (every #'(lambda (x) (member x freevars-form
						      :test #'same-declaration))
				(freevars cform)))
		 cform))))
	  (t
	   (multiple-value-bind (newbindings new?)
	       (var-to-binding freevars-form form)
	     (let* ((qform (make-instance (if exist?
					      'exists-expr
					      'forall-expr)
			     :bindings newbindings
			     :expression (if new?
					     (freevar-substit form
							      freevars-form
							      newbindings)
					     form)))
		    (tform (typecheck* qform (unless *no-expected* *boolean*)
				       'expr nil)))
	       #+pvsdebug
	       (assert (every #'(lambda (x) (member x freevars-form
						    :test #'same-declaration))
			      (freevars tform)))
	       tform))))))

;;; Put newbindings in bindings after all dependencies
(defun insert-bindings (newbindings bindings)
  (if (null newbindings)
      bindings
      (insert-bindings (cdr newbindings)
		       (if (member (car newbindings) bindings
				   :test #'same-declaration)
			   bindings
			   (insert-binding (car newbindings) bindings)))))

(defun insert-binding (nbinding bindings)
  (let* ((frees (remove-if (complement #'(lambda (fv)
					  (member fv bindings
						  :test #'same-declaration)))
		  (freevars nbinding)))
	 (ibindings (insert-binding* nbinding bindings frees nil)))
    ibindings))

(defun insert-binding* (nbinding bindings frees preds)
  (if (null frees)
      (nconc (nreverse preds) (cons nbinding bindings))
      (insert-binding* nbinding
		       (cdr bindings)
		       (remove (car bindings) frees :test #'same-declaration)
		       (cons (car bindings) preds))))
		       
  

;;; Naively want to append newbindings in front of bindings, but the
;;; newbindings may depend on bindings.  So we sort as little as possible,
;;; but satisfy the dependencies.
(defun minimal-sort-bindings (newbindings bindings)
  (let ((fvars (freevars newbindings)))
    (if fvars
	(let ((fvar (smallest-freevar fvars)))
	  ;;(assert (memq (declaration fvar) bindings))
	  (minimal-sort-bindings (cons (declaration fvar) newbindings)
				 (remove (declaration fvar) bindings)))
	(let ((result (append (reverse newbindings) bindings)))
	  ;;(assert (null (freevars result)))
	  result))))

(defun freevar-substit (form freevars-form newbindings)
  (let ((*bound-variables* (append newbindings *bound-variables*))
	(*substit-dont-simplify* t))
    (substit form (pairlis (mapcar #'declaration freevars-form) newbindings))))

;(defun sort-freevars (freevars)
;  (sort (sort (copy-list freevars) #'alphalessp
;	      :key #'(lambda (x) (string (id x))))
;	#'(lambda (x y)
;	    (member (declaration x) (freevars y)
;		    :test #'(lambda (u v)
;			      (eq u (declaration v)))))))

(defun sort-freevars (freevars)
  (sort-freevars* (sort (copy-list freevars) #'alphalessp
			:key #'(lambda (x) (string (id x))))))

(defun sort-freevars* (freevars &optional result)
  (if (null freevars)
      (nreverse result)
      (let ((next (smallest-freevar freevars)))
	(sort-freevars* (delete next freevars)
		       (cons next result)))))

(defun smallest-freevar (freevars &optional (pos 0))
  (let* ((nextfv (nth pos freevars))
	 (next-freevars (freevars nextfv)))
    (if (some #'(lambda (fv)
		  (and (not (eq fv nextfv))
		       (member (declaration fv) next-freevars
			       :test #'(lambda (u v)
					 (eq u (declaration v))))))
	      freevars)
	(smallest-freevar freevars (1+ pos))
	nextfv)))
	      

(defun sort-freevars2 (freevars expr)
  (let ((alist (mapcar #'list freevars))
	(occ 0))
    (mapobject #'(lambda (ex)
		   (when (typep ex 'name-expr)
		     (incf occ)
		     (let ((ass (assoc ex alist :test #'tc-eq)))
		       (when (and ass (null (cdr ass)))
			 (setf (cdr ass) occ)))))
	       expr)
    (sort (sort freevars #'<
		:key #'(lambda (x) (cdr (assoc x alist :test #'tc-eq))))
	  #'(lambda (x y)
	      (member (declaration x) (freevars y)
		      :test #'(lambda (u v)
				(eq u (declaration v))))))))

(defun var-to-binding (varlist expr)
  (let ((newvars? nil))
    (labels ((vtb (vars result new?)
		  (if (null vars)
		      (let ((bindings (nreverse result)))
			(mapl #'(lambda (b)
				  (when (and (cdr b)
					     (tc-eq (type (car b))
						    (type (cadr b))))
				    (setf (chain? (car b)) t)))
			      bindings)
			(setq newvars? new?)
			bindings)
		      (multiple-value-bind (newbind bnew?)
			  (var-to-binding* (car vars) expr result)
			(vtb (cdr vars) (cons newbind result)
			     (or new? bnew?))))))
      (let ((nbindings (vtb varlist nil nil)))
	(values (if newvars?
		    (substit-bindings nbindings varlist)
		    nbindings)
		newvars?)))))

(defun var-to-binding* (var expr vlist)
  (cond ((needs-naming-apart? var expr)
	 (let* ((new-id (make-new-variable (op-to-id var) (cons expr vlist) 1))
		(bind-decl (mk-bind-decl new-id
			     (get-declared-type var) (type var))))
	   (values bind-decl t)))
	((and (bind-decl? (declaration var))
	      (tc-eq (type var) (type (declaration var))))
	 (declaration var))
	(t (let ((bind-decl (mk-bind-decl (id var)
			      (get-declared-type var) (type var))))
	     (values bind-decl t)))))

;;; Checks if there is a variable with the same id in form, but a different
;;; declaration.
(defun needs-naming-apart? (var form)
  (let ((foundone nil))
    (mapobject #'(lambda (x)
		   (or foundone
		       (or (and (type-expr? x)
				(not (member var (freevars x)
				       :test
				       #'(lambda (u v)
					   (and (eq (id u) (id v))
						(not (eq (declaration u)
							 (declaration v))))))))
			   (and (name-expr? x)
				(not (binding? x))
				(eq (id x) (id var))
				(declaration x)
				(not (same-declaration x var))
				(setq foundone t)))))
	       form)
    foundone))

(defun substit-bindings (nbindings varlist &optional newbs)
  (if (null nbindings)
      (nreverse newbs)
      (substit-bindings (if (eq (declaration (car varlist)) (car nbindings))
			    (cdr nbindings)
			    (substit (cdr nbindings)
			      (acons (declaration (car varlist))
				     (car nbindings)
				     nil)))
			(cdr varlist)
			(cons (car nbindings) newbs))))

(defun get-declared-type (var)
  (or (and (slot-exists-p var 'declared-type)
	   (declared-type var))
      ;; Doesn't work if there are actual parameters involved.
;      (and (slot-exists-p var 'resolutions)
;	   (resolutions var)
;	   (slot-exists-p (declaration (car (resolutions var))) 'declared-type)
;	   (declaration (car (resolutions var)))
;	   (let* ((decl (declaration (car (resolutions var))))
;		  (dtype (declared-type decl)))
;	     (unless (and (slot-exists-p decl 'formals) (formals decl))
;	       dtype)))
      (type var)))

(defmethod bindings ((expr expr))
  nil)

;;;;;;;;;; ADT methods ;;;;;;;;;;;

;;; Given a type-name, determine whether it is an adt

(defmethod adt? (te)
  (declare (ignore te))
  nil)

(defmethod adt? ((te type-name))
  #+lucid (restore-adt te)
  (when (adt te)
    (change-class te 'adt-type-name
      'adt (adt te)
      'single-constructor? (singleton? (constructors (adt te))))
    (adt te)))

(defmethod adt? ((te adt-type-name))
  #+lucid (restore-adt te)
  (adt te))

(defmethod adt? ((te datatype-subtype))
  (adt? (declared-type te)))

(defun restore-adt-slot (te)
  (let ((adt (get-adt-slot-value te)))
    (when adt
      (if (adt-type-name? te)
	  (setf (adt te) adt)
	  (change-class te 'adt-type-name
	    'adt adt
	    'single-constructor? (singleton? (constructors adt)))))))

(defun get-adt-slot-value (te)
  ;; te must be a type-name
  (or (let ((dt (get-theory (id te))))
	(and (recursive-type? dt) dt))
      (find-if #'(lambda (d)
		   (and (typep d 'recursive-type)
			(eq (id d) (id te))))
	(all-decls (module (declaration te))))
      (let ((enumtype (find-if #'(lambda (d)
				   (and (typep d 'type-eq-decl)
					(eq (id d) (id te))
					(typep (type-expr d) 'recursive-type)))
			(all-decls (module (declaration te))))))
	(when enumtype
	  (type-expr enumtype)))))

(defun enum-adt? (te)
  (and (adt? te)(enumtype? (adt te))))


;;; Given a function name, determine whether it is a constructor,
;;; recognizer, or accessor

(defmethod constructor? ((fn constructor-name-expr))
  t)

(defmethod constructor? ((fn name-expr))
  (when (constructor? (resolution fn))
    (change-class fn 'constructor-name-expr)
    t))

(defmethod constructor? ((res resolution))
  (typep (declaration res) 'adt-constructor-decl))

(defmethod constructor? ((expr expr))
  nil)

(defmethod recognizer? ((fn recognizer-name-expr))
  t)

(defmethod recognizer? ((fn name-expr))
  (when (recognizer? (resolution fn))
    (change-class fn 'recognizer-name-expr)
    t))

(defmethod recognizer? ((res resolution))
  (typep (declaration res) 'adt-recognizer-decl))

(defmethod recognizer? ((fn coercion))
  (recognizer? (args1 fn)))

(defmethod recognizer? ((obj expr))
  nil)

(defmethod recognizer? ((te type-expr))
  (and (subtype? te)
       (recognizer? (predicate te))))

(defmethod recognizer? ((te dep-binding))
  (recognizer? (type te)))

(defmethod accessor? ((fn accessor-name-expr))
  t)

(defmethod accessor? ((fn injection-expr))
  nil)

(defmethod accessor? ((fn name-expr))
  (when (accessor? (resolution fn))
    (change-class fn 'accessor-name-expr)
    t))

(defmethod accessor? ((res resolution))
  (typep (declaration res) 'adt-accessor-decl))

(defmethod accessor? ((expr expr))
  nil)


;;; Given a constructor name, return the appropriately instantiated adt
;;; type name.

(defmethod adt ((te type-name))
  nil)

(defmethod adt ((fn constructor-name-expr))
  (or (adt-type fn)
      (let* ((adt (find-declared-adt-supertype
		   (if (typep (type fn) 'funtype)
		       (range (type fn))
		       (type fn)))))
	(setf (adt-type fn) adt))))

(defmethod adt ((fn recognizer-name-expr))
  (or (adt-type fn)
      (let ((adt (find-declared-adt-supertype (domain (type fn)))))
	(assert (adt-type-name? (find-supertype adt)))
	(setf (adt-type fn) adt))))

(defmethod adt ((fn accessor-name-expr))
  (or (adt-type fn)
      (let* ((dtype (domain (type fn)))
	     (adt (find-declared-adt-supertype
		   (if (typep dtype 'subtype)
		       (supertype dtype)
		       dtype))))
	;; Usually, adt is (a subtype of) an adt-type-name instance
	;; But if there are mappings, the type could be anything
	(when (adt-type-name? (find-supertype adt))
	  (setf (adt-type fn) adt)))))

(defmethod adt ((fn name-expr))
  nil)

(defmethod adt ((fn subtype))
  (adt (supertype fn)))

(defmethod adt ((x dep-binding))
  (adt (type x)))

(defmethod adt-subtype ((fn name-expr))
  (when (constructor? fn)
    (let ((adt-sub (if (typep (type (declaration fn)) 'funtype)
		       (range (type (declaration fn)))
		       (type (declaration fn)))))
      (subst-mod-params adt-sub (module-instance fn)
			(module (declaration fn))))))

(defmethod recognizer ((fn name-expr))
  nil)

(defmethod recognizer ((fn constructor-name-expr))
  (or (recognizer-name fn)
      (let* ((con (car (member fn (constructors (adt (adt fn)))
			       :test #'same-last-id)))
	     (rd (rec-decl con))
	     (res (make-resolution rd (module-instance fn))))
	(setf (recognizer-name fn)
	      (mk-name-expr (id rd) nil nil res)))))

(defmethod recognizer ((fn injection-expr))
  (let ((cotuptype (find-supertype (range (type fn)))))
    (make-instance 'injection?-expr
      :id (makesym "IN?_~d" (index fn))
      :index (index fn)
      :type (mk-funtype cotuptype *boolean*))))

(defmethod accessors ((fn name-expr))
  (when (constructor? fn)
    (let* ((con (car (member fn (constructors (adt (adt fn)))
			     :test #'same-id))))
       (mapcar #'(lambda (acc)
		  (let ((res (make-resolution acc (module-instance fn))))
		    (mk-name-expr (id acc) nil nil res)))
	      (acc-decls con)))))

(defmethod accessors ((fn constructor-name-expr))
  (if (eq (accessor-names fn) 'unbound)
      (let* ((con (car (member fn (constructors (adt (adt fn)))
			       :test #'same-id))))
	(setf (accessor-names fn)
	      (mapcar #'(lambda (acc)
			  (let ((res (make-resolution acc
				       (module-instance fn))))
			    (mk-name-expr (id acc) nil nil res)))
		(acc-decls con))))
      (accessor-names fn)))

(defmethod accessors ((fn injection-expr))
  (let* ((cotuptype (find-supertype (range (type fn))))
	 (inrec (make-instance 'injection?-expr
		  :id (makesym "IN?_~d" (index fn))
		  :index (index fn)
		  :type (mk-funtype cotuptype *boolean*)))
	 (insubtype (make!-expr-as-type inrec))
	 (intype (nth (1- (index fn)) (types cotuptype))))
    (list (make-instance 'extraction-expr
	    :id (makesym "OUT_~d" (index fn))
	    :index (index fn)
	    :type (mk-funtype insubtype intype)))))

(defmethod constructor ((fn recognizer-name-expr))
  (or (constructor-name fn)
      (setf (constructor-name fn) (call-next-method))))

(defmethod constructor ((fn injection?-expr))
  (or (constructor-name fn)
      (setf (constructor-name fn)
	    (let* ((cotupletype (find-supertype (domain (type fn))))
		   (intype (nth (1- (index fn)) (types cotupletype))))
	      (make-instance 'injection-expr
		:index (index fn)
		:id (makesym "IN_~d" (index fn))
		:type (mk-funtype intype cotupletype))))))

(defmethod adt ((decl adt-accessor-decl))
  (let* ((dtype (domain (type decl)))
	 (adt (find-declared-adt-supertype
	       (if (typep dtype 'subtype)
		   (supertype dtype)
		   dtype))))
    ;; Usually, adt is (a subtype of) an adt-type-name instance
    ;; But if there are mappings, the type could be anything
    (when (adt-type-name? (find-supertype adt))
      (adt adt))))

(defmethod adt-constructor-decl ((decl adt-accessor-decl))
  (let* ((adt (adt decl))
	 (constr (find-if #'(lambda (c) (memq decl (arguments c))) (constructors adt))))
    (assert (adt-constructor-decl? constr))
    constr))

(defmethod constructor ((fn accessor-name-expr))
  ;; An accessor-name-expr has a declaration of class adt-accessor-decl
  ;; Normally, the adt associated with this is (a subtype of) an instance of
  ;; adt-type-name, but mappings can change this.
  (when (adt fn)
    (let* ((constrs (remove-if-not #'(lambda (c) (part-of-constructor fn c))
		      (constructors (adt (adt fn)))))
	   (decl (declaration fn))
	   (cons (remove-if-not #'(lambda (c) (memq decl (acc-decls c)))
		   constrs)))
      (mapcar #'(lambda (con)
		  (let* ((cd (con-decl con))
			 (res (make-resolution cd (module-instance fn))))
		    (mk-name-expr (id cd) nil nil res)))
	cons))))

(defmethod constructor ((fn name-expr))
  (let* ((constrs (remove-if-not #'(lambda (c) (part-of-constructor fn c))
		     (constructors (adt (adt fn)))))
	 (decl (declaration fn))
	 (con (find-if #'(lambda (c)
			   (or (eq decl (con-decl c))
			       (eq decl (rec-decl c))
			       (memq decl (acc-decls c))))
		       constrs)))
    (when con
       (let* ((cd (con-decl con))
	      (res (make-resolution cd (module-instance fn))))
	 (mk-name-expr (id cd) nil nil res)))))

(defun part-of-constructor (fn con)
  (cond ((recognizer? fn)
	 (eq (id fn) (recognizer con)))
	((accessor? fn)
	 (some #'(lambda (a) (same-id fn a))
	       (arguments con)))))

(defmethod constructors ((tn type-name))
  (when (adt? tn)
    (mapcar #'(lambda (cd)
		(mk-name-expr (id cd) nil nil
			      (make-resolution cd (module-instance tn))))
	    (mapcar #'con-decl (constructors (adt tn))))))

(defmethod constructors ((te cotupletype))
  (let ((index 0))
    (mapcar #'(lambda (rec)
		(incf index)
		(make-instance 'injection-expr
		  :id (makesym "IN_~d" index)
		  :index index
		  :type (mk-funtype te (make!-expr-as-type rec))))
      (recognizers te))))

(defmethod constructors ((te subtype))
  (constructors (supertype te)))

(defmethod constructors ((te datatype-subtype))
  (constructors (declared-type te)))

(defmethod recognizers ((tn type-name))
  (when (adt? tn)
    (if (and (recognizer-names tn)
	     (fully-instantiated? (recognizer-names tn)))
	(recognizer-names tn)
	(setf (recognizer-names tn)
	      (subst-mod-params (mapcar #'recognizer (constructors tn))
				(module-instance tn)
				(module (declaration tn)))))))

(defmethod recognizers ((te cotupletype))
  (let ((index 0))
    (mapcar #'(lambda (ty)
		(declare (ignore ty))
		(incf index)
		(make-instance 'injection?-expr
		  :id (makesym "IN?_~d" index)
		  :index index
		  :type (mk-funtype te *boolean*)))
      (types te))))
		

(defmethod recognizers ((te subtype))
  (recognizers (supertype te)))

(defun rec-accessors (rtype)
  (mapcar #'mk-name (fields rtype)))

(defmethod non-recursive-construction? ((ex application))
  (let ((op (operator ex)))
    (and (constructor? op)
	 (let* ((adt (adt (adt op)))
		(type (adt-type-name adt)))
	   (non-recursive-constructor (find-if #'(lambda (c)
						   (same-id op c))
					(constructors adt))
				      type)))))

(defmethod non-recursive-construction? ((ex expr))
  nil)

(defun tup-accessors (tuptype)
  (let ((projnum 0))
    (mapcar #'(lambda (type)
		(let* ((fn (mk-name-expr (makesym "PROJ_~d" (incf projnum))))
		       (ty (find-supertype type))
		       (fty (mk-funtype (list tuptype) ty)))
		  (setf (resolutions fn)
			(list (make-resolution (mk-bind-decl (id fn) ty)
				nil fty)))
		  (setf (type fn) fty)
		  fn))
	    (types tuptype))))

(defun mk-tup-accessor (tuptype projnum)
  (let* ((dtype (nth (1- projnum) (types tuptype)))
	 (type (if (dep-binding? dtype) (type dtype) dtype))
	 (fn (mk-name-expr (makesym "PROJ_~d" projnum)))
	 (ty (find-supertype type))
	 (fty (mk-funtype (list tuptype) ty)))
    (setf (resolutions fn)
	  (list (make-resolution (mk-bind-decl (id fn) ty)
		  nil fty)))
    (setf (type fn) fty)
    fn))
			 
	      

(defmethod domain ((te subtype))
  (domain (supertype te)))

(defmethod domain ((te dep-binding))
  (domain (type te)))

(defmethod range ((te subtype))
  (range (supertype te)))

(defmethod range ((te dep-binding))
  (range (type te)))

#-lucid
(defun assq (obj alist)
  (assoc obj alist :test #'eq))

#-lucid
(defun sbrt::assq (obj alist)
  (assoc obj alist :test #'eq))


;;; Full-name

(defvar *full-name-depth* nil)

(defvar *exclude-prelude-names* nil)

(defun full-name (obj &optional depth prelude?)
  (if (and depth (zerop depth))
      obj
      (let ((*full-name-depth* depth)
	    (*exclude-prelude-names* (or prelude? *exclude-prelude-names*)))
	(gensubst obj #'full-name! #'full-name?))))

(defmethod full-name? (obj)
  (declare (ignore obj))
  nil)

(defmethod full-name? ((x name))
  (and (resolution x)
       (not (variable? x))
       (module-instance (resolution x))
       (or (not (current-theory))
	   (not (eq (module (declaration (resolution x)))
		    (current-theory)))
	   (not (eq (id x) (id (resolution x))))
	   (actuals (module-instance (resolution x)))
	   (integerp (id x))
	   (mappings (module-instance (resolution x))))
       (or (not *exclude-prelude-names*)
	   (not (from-prelude? (declaration x)))
	   (integerp (id x))
	   (mappings (module-instance (resolution x))))))

(defmethod full-name? ((x adt-name-expr))
  (and (adt x)
       (resolution (adt x))
       (module-instance (resolution (adt x)))
       (or (not (current-theory))
	   (not (eq (module (declaration (resolution (adt x))))
		    (current-theory)))
	   (not (eq (id x) (id (resolution (adt x)))))
	   (actuals (module-instance (resolution (adt x))))
	   (integerp (id x))
	   (mappings (module-instance (resolution (adt x)))))
       (or (not *exclude-prelude-names*)
	   (not (from-prelude? (declaration x)))
	   (integerp (id x))
	   (mappings (module-instance (resolution (adt x)))))))

(defmethod full-name? ((x type-expr))
  (if (print-type x)
      (full-name? (print-type x))
      (call-next-method)))

(defmethod full-name? ((x type-name))
  (and (resolution x)
       (module-instance (resolution x))
       (let ((cth (current-theory)))
	 (or (not cth)
	     (not (eq (id (module-instance (resolution x))) (id cth)))
	     (not (type-name? (type (resolution x))))
	     (not (eq (id (type (resolution x))) (id x)))
	     (actuals (module-instance (resolution x)))))))

(defmethod full-name! ((x name))
  (copy x
    'id (id (resolution x))
    'mod-id (when (or (not (current-theory))
		      (integerp (id x))
		      (not (eq (id (module-instance (resolution x)))
			       (id (current-theory)))))
	      (id (module-instance (resolution x))))
    'library (or (library x)
		 (library (module-instance (resolution x)))
		 (when (and (declaration x)
			    (library-datatype-or-theory?
			     (module (declaration x))))
		   (libref-to-libid (lib-ref (module (declaration x))))))
    'actuals (full-name (actuals (module-instance (resolution x)))
			(when *full-name-depth*
			  (1- *full-name-depth*)))
    'mappings (mappings (module-instance (resolution x)))))

(defmethod full-name! ((x adt-name-expr))
  (copy x
    'id (id (resolution x))
    'mod-id (when (or (not (current-theory))
		      (integerp (id x))
		      (not (eq (id (module-instance (resolution (adt x))))
			       (id (current-theory)))))
	      (id (module-instance (resolution (adt x)))))
    'library (or (library x)
		 (library (module-instance (resolution (adt x))))
		 (when (and (declaration x)
			    (library-datatype-or-theory?
			     (module (declaration (adt x)))))
		   (libref-to-libid (lib-ref (module (declaration x))))))
    'actuals (full-name (actuals (module-instance (resolution (adt x))))
			(when *full-name-depth*
			  (1- *full-name-depth*)))
    'mappings (mappings (module-instance (resolution (adt x))))))

(defmethod full-name! ((te type-expr))
  (assert (print-type te))
  (lcopy te 'print-type (full-name! (print-type te))))

(defmethod full-name! ((x type-name))
  (let* ((mi (module-instance (resolution x)))
	 (modid (id mi)))
    (copy x
      'mod-id (when (and (or (not *exclude-prelude-names*)
			     (not (gethash modid *prelude*)))
			 (or (not (current-theory))
			     (not (eq modid (id (current-theory))))))
		modid)
      'library (or (library x)
		   (library (module-instance (resolution x)))
		   (when (and (declaration x)
			      (library-datatype-or-theory?
			       (module (declaration x))))
		     (libref-to-libid (lib-ref (module (declaration x))))))
      'actuals (full-name (actuals mi)
			  (when *full-name-depth*
			    (1- *full-name-depth*)))
      'mappings (mappings (module-instance (resolution x))))))

(defmethod module ((map mapping))
  (module (declaration (lhs map))))

(defmethod full-name? ((x injection-expr))
  (null (actuals x)))

(defmethod full-name! ((x injection-expr))
  (lcopy x 'actuals (list (mk-actual (range (type x))))))

(defmethod full-name? ((x extraction-expr))
  (null (actuals x)))

(defmethod full-name! ((x extraction-expr))
  (lcopy x 'actuals (list (mk-actual (domain (type x))))))

(defmethod full-name? ((x injection?-expr))
  (null (actuals x)))

(defmethod full-name! ((x injection?-expr))
  (lcopy x 'actuals (list (mk-actual (domain (type x))))))

(defmethod full-name? ((x injection-application))
  (null (actuals x)))

(defmethod full-name! ((x injection-application))
  (lcopy x 'actuals (list (mk-actual (find-supertype (type x))))))

(defmethod full-name? ((x extraction-application))
  (null (actuals x)))

(defmethod full-name! ((x extraction-application))
  (lcopy x 'actuals (list (mk-actual (type (argument x))))))

(defmethod full-name? ((x injection?-application))
  (null (actuals x)))

(defmethod full-name! ((x injection?-application))
  (lcopy x 'actuals (list (mk-actual (type (argument x))))))

;;; Raise-actuals

(defvar *raise-actuals-of-actuals* nil)

(defvar *raise-actuals-theory-ids* nil)

(defun raise-actuals (obj &optional (actuals-also? t) theory-ids?)
  (let ((*raise-actuals-of-actuals* actuals-also?)
	(*raise-actuals-theory-ids* theory-ids?)
	(*pseudo-normalizing* t)
	(*visible-only* t))
    (gensubst obj #'raise-actuals! #'raise-actuals?)))

(defmethod raise-actuals? (obj)
  (declare (ignore obj))
  nil)

(defmethod raise-actuals? ((x name))
  (unless (or (and (eq (id x) '=)
		   (module-instance x)
		   (eq (id (module-instance x)) '|equalities|))
	      (and (eq (id x) '/=)
		   (module-instance x)
		   (eq (id (module-instance x)) '|notequal|)))
    (raise-actuals-name? x)))

(defmethod raise-actuals? ((a actual))
  (not *raise-actuals-of-actuals*))

(defun raise-actuals-name? (x)
  (and (resolution x)
       (module-instance (resolution x))
       (or (and (null (actuals x))
		(actuals (module-instance (resolution x))))
	   (and *raise-actuals-theory-ids*
		(null (mod-id x))
		(declaration x)
		(module? (module (declaration x)))
		(not (from-prelude? (module (declaration x))))
		(not (eq (id (module-instance x)) (id (current-theory))))))))

(defmethod raise-actuals? ((x type-expr))
  (or (call-next-method)
      (and (print-type x)
	   (if (typep (print-type x) 'name)
	       (raise-actuals-name? (print-type x))
	       (raise-actuals? (print-type x))))))

(defmethod raise-actuals! ((x type-expr))
  (lcopy (call-next-method) 'print-type (raise-actuals (print-type x))))

(defmethod raise-actuals! (x) x)

(defmethod raise-actuals! ((x name))
  (copy x
    'actuals (if (actuals x)
		 (if *raise-actuals-of-actuals*
		     (raise-actuals (actuals x) *raise-actuals-theory-ids*
				    *raise-actuals-theory-ids*)
		     (actuals x))
		 (if *raise-actuals-of-actuals*
		     (raise-actuals (actuals (module-instance (resolution x)))
				    *raise-actuals-theory-ids*
				    *raise-actuals-theory-ids*)
		     (actuals (module-instance (resolution x)))))
    'mod-id (or (mod-id x)
		(and *raise-actuals-theory-ids*
		     (id (module-instance (resolution x)))))))

#+(or lucid allegro)
(defmethod ppr (obj)
  (format t "~&")
  (write obj :pretty t :level nil :length nil)
  nil)

#+gcl
(defmethod ppr (obj)
  (if (typep obj 'hashtable)
      (maphash #'(lambda (x y) (format t "~%~a - ~a" x y))
		   obj)
      (if (typep obj 'ht)
	  (pvs-maphash #'(lambda (x y) (format t "~%~a - ~a" x y))
		       obj)
	  (write obj :pretty t :level nil :length nil)))
  nil)

(defmethod ppr ((obj hash-table))
  (maphash #'(lambda (x y) (format t "~%~@<~w~:_ ==> ~w~:>" x y))
	   obj))

(defmethod ref-to-id ((ref symbol))
  ref)

(defmethod ref-to-id ((ref string))
  (intern ref))

(defmethod ref-to-id ((ref integer))
  (makesym "~r" ref))

(defmethod ref-to-id ((ref syntax))
  (if (slot-exists-p ref 'id)
      (ref-to-id (id ref))
      (error "No id slot for <# ~a - ~a #>" (class-name (class-of ref)) ref)))

(defmethod ref-to-id ((ref subtype-judgement))
  (or (id ref)
      '|subtype|))

(defmethod ref-to-id ((ref number-judgement))
  (or (id ref)
      (intern (substitute #\_ #\space
			  (substitute #\_ #\-
				      (format nil "~r"
					(number (number-expr ref))))))))

(defmethod ref-to-id ((ref name-judgement))
  (or (id ref)
      (id (name ref))))

(defmethod ref-to-id ((ref application-judgement))
  (or (id ref)
      (id (name ref))))

(defmethod ref-to-id ((ref expr-judgement))
  (or (id ref)
      '|expr_judgement|))


;;; CASES v OF                      IF c1?(v) THEN e1
;;;  c1: e1,                        ELSIF c2?(v)
;;;  c2(v1, v2): e2      ----->        THEN e2[a1(v)/v1, a2(v)/v2]
;;;  ELSE e3                        ELSE e3

(defun translate-cases-to-if (cases-expr)
  (translate-cases-to-if* (expression cases-expr)
			  (selections cases-expr)
			  (else-part cases-expr)))

(defun translate-cases-to-if* (expr selections else-part &optional chained?)
  (cond ((and (null (cdr selections))
	      (null else-part))
	 (subst-accessors-in-selection expr (car selections)))
	((null selections)
	 else-part)
	(t (let* ((sel (car selections))
		  (stype (find-supertype (type expr)))
		  (thinst (unless (cotupletype? stype)
			    (module-instance stype)))
		  (rec (if thinst
			   (subst-mod-params (recognizer (constructor sel))
					     thinst
					     (module (declaration stype)))
			   (recognizer (constructor sel))))
		  (cond (make!-application rec expr))
		  (then ;(subst-mod-params
			 (subst-accessors-in-selection expr sel)
			 ;thinst)
		    )
		  (else (translate-cases-to-if* expr (cdr selections)
						else-part t)))
	     (if chained?
		 (make!-chained-if-expr cond then else)
		 (make!-if-expr cond then else))))))

(defun subst-accessors-in-selection (expr sel)
  (let* ((stype (find-declared-adt-supertype (type expr)))
	 (thinst (unless (cotupletype? stype)
		   (module-instance stype)))
	 (accs (if thinst
		   (subst-mod-params (accessors (constructor sel)) thinst
				     (module (declaration stype)))
		   (accessors (constructor sel))))
	 (vars (args sel))
	 (selexpr (expression sel)))
    (substit selexpr
      (pairlis vars
	       (mapcar #'(lambda (acc)
			   (if thinst
			       (make!-application acc expr)
			       (make-instance 'extraction-application
				 :id (id acc)
				 :index (index acc)
				 :argument expr
				 :type (range (type acc)))))
		       accs)))))

;;; Translate update applications to if expressions, e.g.
;;;   (F WITH [(x) := 0])(y)  ==>  IF x = y THEN 0 ELSE F(y) ENDIF
;;; It also performs the following transformation:
;;;   (IF A THEN B ELSE C ENDIF)(x)  ==>  IF A THEN B(x) ELSE C(x) ENDIF

(defmethod translate-update-to-if ((expr application))
  (let ((op (operator* expr))
	(args (arguments* expr))
	(*generate-tccs* 'none))
    (or (translate-update-to-if* op args)
	expr)))

(defmethod translate-update-to-if* ((op name-expr) args)
  (when (eq (id (module-instance (resolution op))) '|if_def|)
    ;; Note that (car args) are the cond, then, and else parts of the IF
    (let* ((if-args (car args))
	   (cond (car if-args))
	   (then (translate-update-to-if
		  (make-applications (cadr if-args) (cdr args))))
	   (else (translate-update-to-if
		  (make-applications (caddr if-args) (cdr args)))))
      (if (tc-eq then else)
	  then
	  (make-if-expr cond then else)))))

(defun make-applications (expr args)
  (if (null args)
      expr
      (make-applications (make-application* expr (car args)) (cdr args))))

(defmethod translate-update-to-if* ((op update-expr) applargs)
  (let ((if-op (translate-update-to-if! op)))
    (make!-applications if-op applargs)))

(defun translate-applied-update-to-if (op applargs args exprs recargs recexprs)
  (cond ((and args (null (car args)))
	 (translate-update-to-if (make-applications (car exprs) applargs)))
	((null applargs)
	 (assert (and (null recargs) (null recexprs)))
	 (translate-applied-update-leaf op args exprs))
	((null args)
	 (translate-applied-update-to-if
	  (translate-update-to-if (make-application* op (car applargs)))
	  (cdr applargs) (reverse recargs) (reverse recexprs) nil nil))
	(t (let ((condition (make-applied-update-equation
			     (car applargs) (caar args)))
		 (then-part (translate-applied-update-to-if
			     op applargs
			     (when (cdar args) (cdr args))
			     (when (cdar args) (cdr exprs))
			     (cons (cdar args) recargs)
			     (cons (car exprs) recexprs)))
		 (else-part (translate-applied-update-to-if
			     op applargs (cdr args) (cdr exprs)
			     recargs recexprs)))
	     (if (tc-eq then-part else-part)
		 then-part
		 (make-if-expr condition then-part else-part))))))

(defun translate-applied-update-leaf (op args exprs)
  (if args
      (make!-update-expr op
			 (nreverse
			  (mapcar #'(lambda (arg expr)
				      (mk-assignment nil arg expr))
			    args exprs)))
      op))

(defun make-applied-update-equation (applarg arg)
  (make-equality (make-arg-tuple-expr arg)
		 (make-arg-tuple-expr applarg)))

(defmethod translate-update-to-if* ((op expr) args)
  (declare (ignore args))
  nil)

(defun translate-update-to-if-ass (assignments expr args &optional chain?)
  (if (null assignments)
      (translate-update-to-if (make-applications expr args))
      (let* ((ass (car assignments))
	     (ass-args (arguments ass))
	     (ass-expr (expression ass)))
	(multiple-value-bind (cond remass remargs)
	    (make-update-condition ass-args args)
	  (let ((then (if remass
			  (make-update-expr (make-applications expr args)
					    (list (mk-assignment nil
						    remass ass-expr)))
			  (make-applications ass-expr remargs)))
		(else (translate-update-to-if-ass
		       (cdr assignments) expr args t)))
	    (if (tc-eq then else)
		then
		(if chain?
		    (make-chained-if-expr cond then else)
		    (make-if-expr cond then else))))))))

(defun make-update-condition (ass-args args &optional equalities)
  (cond ((null ass-args)
	 (values (make-conjunction (nreverse equalities)) nil args))
	((null args)
	 (values (make-conjunction (nreverse equalities)) ass-args nil))
	(t (let ((nequality (make-equality (make-arg-tuple-expr (car ass-args))
					   (make-arg-tuple-expr (car args)))))
	     (make-update-condition (cdr ass-args) (cdr args)
				    (cons nequality equalities))))))

(defun max-update-arg-length (assigns &optional max)
  (cond ((null assigns)
	 max)
	((null max)
	 (max-update-arg-length (cdr assigns)
				(length (arguments (car assigns)))))
	(t (let ((nlen (length (arguments (car assigns)))))
	     (max-update-arg-length (cdr assigns) (max max nlen))))))
	 

(defmethod translate-update-to-if ((expr expr))
  expr)


;;; translate-update-to-if! is like translate-update-to-if, but works even
;;; if the update is not applied.

(defvar *translate-update-conditions*)

(defmethod translate-update-to-if! ((expr update-expr))
  (with-slots (type expression assignments) expr
    (let ((*generate-tccs* 'none)
	  (*translate-update-conditions* nil))
      (translate-update-to-if!*
       type expression
       (mapcar #'arguments assignments)
       (mapcar #'expression assignments)))))

(defmethod translate-update-to-if!* ((type subtype) ex args exprs)
  (translate-update-to-if!* (supertype type) ex args exprs))

(defmethod translate-update-to-if!* (type ex args exprs)
  (declare (ignore type args))
  (or (car (last exprs)) ex))

;;; This one recurses down assignments
(defmethod translate-update-to-if!* ((type funtype) ex args-list exprs)
  (if (null args-list)
      ex
      (let* ((*translate-update-conditions* *translate-update-conditions*)
	     (nex (translate-update-to-if-funtype
		   type ex (car args-list) (car exprs))))
	(translate-update-to-if!* type nex (cdr args-list) (cdr exprs)))))

;;; This one recurses down the arguments of a single assignment
(defun translate-update-to-if-funtype (type ex args expr)
  (if args
      (let* ((bid (make-new-variable '|x| (cons ex (cons expr args))))
	     (bd (make-bind-decl bid (domain type)))
	     (bvar (make-variable-expr bd))
	     (car-arg (if (cdar args)
			  (make!-tuple-expr* (car args))
			  (caar args)))
	     (eqn (make!-equation bvar car-arg)))
	(push car-arg *translate-update-conditions*)
	(let ((nex (make-update-if-application ex bvar)))
	  (make!-lambda-expr (list bd)
	    (make-update-function-if-expr eqn type nex (cdr args) expr))))
      (translate-update-to-if! expr)))

(defmethod make-update-if-application ((ex if-expr) arg)
  (let ((then (make-update-if-application (then-part ex) arg))
	(else (make-update-if-application (else-part ex) arg)))
    (if (tc-eq then else)
	then
	(make!-if-expr (condition ex) then else))))

(defmethod make-update-if-application (ex arg)
  (make!-application ex arg))
    
  
(defun make-update-function-if-expr (eqn type ex args expr)
  (let ((then (if args
		  (translate-update-to-if!* (range type) ex
					    (list args) (list expr))
		  expr)))
    (if (tc-eq then ex)
	ex
	(let* ((needs-reducing? (member (car *translate-update-conditions*)
					(cdr *translate-update-conditions*)
					:test #'tc-eq))
	       (nthen (if needs-reducing?
			  (reduce-update-if-expr then eqn t)
			  then))
	       (nex (if needs-reducing?
			(reduce-update-if-expr ex eqn nil)
			ex)))
	  (make!-if-expr eqn nthen nex)))))

(defun reduce-update-if-expr (ex eqn true?)
  (gensubst ex
    #'(lambda (x) (if true? (then-part x) (else-part x)))
    #'(lambda (x) (and (branch? x) (tc-eq (condition x) eqn)))))

(defmethod translate-update-to-if!* ((type recordtype) ex args exprs)
  (make-record-expr
   (mapcar #'(lambda (fld)
	       (multiple-value-bind (fargs fexprs)
		   (matching-update-args-and-exprs fld args exprs)
		 (mk-assignment 'uni
		   (list (list (make-instance 'field-assignment-arg
				 :id (id fld))))
		   (if fargs
		       (if (some #'cdr args)
			   (translate-update-to-if!*
			    (type fld)
			    (make-update-field-application fld ex)
			    (mapcar #'cdr fargs)
			    fexprs)
			   (car (last fexprs)))
		       (make-update-field-application fld ex)))))
     (fields type))
   type))

(defmethod make-update-field-application (fld (ex if-expr))
  (let ((then (make-update-field-application fld (then-part ex)))
	(else (make-update-field-application fld (else-part ex))))
    (if (tc-eq then else)
	then
	(make-if-expr
	 (condition ex)
	 then
	 else))))

(defmethod make-update-field-application (fld ex)
  (make-field-application fld ex))

(defun matching-update-args-and-exprs (fld args exprs &optional fargs fexprs)
  (cond ((null args)
	 (values (nreverse fargs) (nreverse fexprs)))
	((if (typep fld 'field-decl)
	     (same-id fld (caaar args))
	     (= fld (number (caaar args))))
	 (matching-update-args-and-exprs
	  fld (cdr args) (cdr exprs)
	  (cons (car args) fargs) (cons (car exprs) fexprs)))
	(t (matching-update-args-and-exprs
	    fld (cdr args) (cdr exprs) fargs fexprs))))

(defmethod translate-update-to-if!* ((type tupletype) ex args exprs)
  (let ((cnt 0))
    (make-tuple-expr
     (mapcar #'(lambda (ty)
		 (incf cnt)
		 (multiple-value-bind (fargs fexprs)
		     (matching-update-args-and-exprs cnt args exprs)
		   (if fargs
		       (if (some #'cdr fargs)
			   (translate-update-to-if!*
			    ty
			    (make-projection-application cnt ex)
			    (mapcar #'cdr fargs)
			    fexprs)
			   (car (last fexprs)))
		       (make-projection-application cnt ex))))
       (types type)))))

(defun remove-nth (num list &optional acclist)
  (cond ((null list)
	 (nreverse acclist))
	((= num 0)
	 (nconc (nreverse acclist) (cdr list)))
	(t (remove-nth (1- num) (cdr list) (cons (car list) acclist)))))

(defmethod translate-update-to-if! (expr)
  (translate-update-to-if expr))


(defmethod arguments ((decl def-decl))
  (mapcar #'(lambda (b)
	      (let ((nexpr (mk-name-expr (id b))))
		(setf (type nexpr) (type b))
		(setf (resolutions nexpr)
		      (list (make-resolution
				b (mk-modname (id (module decl))))))
		nexpr))
	  (or (apply #'append (formals decl))
	      (bindings (definition decl)))))


(defmethod find-supertype ((te subtype))
  (with-slots (supertype top-type) te
    (or top-type
	(setf top-type (find-supertype supertype)))))

(defmethod find-supertype ((te expr-as-type))
  (if (supertype te)
      (call-next-method)
      (domain (type (expr te)))))

(defmethod find-supertype ((te dep-binding))
  (with-slots (type) te
    (find-supertype type)))

(defmethod find-supertype ((te type-expr))
  te)

(defmethod find-supertype ((te type-name))
  #+lucid (restore-adt te)
  (let ((adt (adt te))
	(dth (module (declaration te)))
	(modinst (module-instance te)))
    #+pvsdebug (assert (or (null adt)
			   (inline-recursive-type? adt)
			   (rectype-theory? dth)))
    (if (and adt
	     (not (inline-recursive-type? adt))
	     (actuals modinst)
	     (rectype-theory? dth)
	     (positive-types dth)
	     (not (every #'null (positive-types dth))))
	(let* ((nmodinst (adt-modinst (module-instance te) dth)))
	  (if (tc-eq nmodinst modinst)
	      te
	      (let* ((res (mk-resolution (declaration te) nmodinst nil))
		     (nte (copy te
			    'resolutions (list res)
			    'actuals (actuals nmodinst))))
		(setf (type res) nte)
		nte)))
	te)))

(defmethod find-supertype ((te type-var))
  te)


;; Given a variable name R and a dependent type like
;; [# size: nat, contents: [below(size)->T]#],
;; returns [# size: nat, contents: [below(R`size) -> T]#]
(defmethod remove-dependent-type (ex (te recordtype))
  (assert *current-context*)
  (assert (compatible? (type ex) te))
  (if (dependent? te)
      (let ((nfields (remove-dependent-fields ex (fields te))))
	(mk-recordtype nfields nil))
      te))

(defun remove-dependent-fields (ex fields &optional nfields)
  (if (null fields)
      (sort-fields nfields nil)
      (remove-dependent-fields
       ex 
       (let ((fappl (make!-field-application (car fields) ex)))
	 (substit (cdr fields) (acons (car fields) fappl nil)))
       (cons (car fields) nfields))))

(defmethod remove-dependent-type (ex (te tupletype))
  (assert *current-context*)
  (assert (compatible? (type ex) te))
  (let ((ntypes (remove-dependent-types ex (types te))))
    (lcopy te 'types ntypes)))

(defun remove-dependent-types (ex types &optional (index 1) ntypes)
  (if (null types)
      (nreverse ntypes)
      (remove-dependent-types
       ex
       (if (dep-binding? (car types))
	   (let ((nex (make!-projection-application index ex)))
	     (substit (cdr types) (acons (car types) nex nil)))
	   (cdr types))
       (1+ index)
       (cons (car types) ntypes))))

;; Note that this is different - ex is of the domain type, NOT the
;; function types
(defmethod remove-dependent-type (ex (te funtype))
  (assert *current-context*)
  (assert (compatible? (type ex) (domain te)))
  (if (dep-binding? (domain te))
      (copy te
	'range (substit (range te) (acons (domain te) ex nil)))
      te))


;;; copy-all makes copies all the way down the object.  Because it uses
;;; gensubst, this function may only be used when the object has been
;;; typechecked, and *current-context* must be set.

(defun copy-all (obj &optional not-parsing)
  (let ((*copy-print-type* t)
	;;(*gensubst-cache* nil)
	(*parsing-or-unparsing* (not not-parsing)))
    (gensubst obj #'copy-all! #'copy-all?)))

(defmethod copy-all? ((ex name))
  t)

(defmethod copy-all? ((ex number-expr))
  t)

;;; otherwise the name method kicks in
(defmethod copy-all? ((ex bind-decl)) nil)

(defmethod copy-all? (obj)
  (declare (ignore obj))
  nil)

(defmethod copy-all! ((ex name))
  (copy ex 'actuals (mapcar #'copy-all (actuals ex))))

(defmethod copy-all! ((ex bind-decl))
  (let ((nex (copy ex)))
    (when (resolutions ex)
      (setf (resolutions nex)
	    (list (copy (car (resolutions ex)) 'declaration nex))))
    nex))

(defmethod copy-all! ((ex binding))
  (copy ex
    'declared-type (copy-all (declared-type ex))))

(defmethod copy-all! ((ex number-expr))
  (copy ex))


(defun op-to-id (ref)
  (let ((id (ref-to-id ref)))
    (or (cdr (assoc id *pvs-operators*))
	id)))

(defmethod dactuals ((ex number-expr))
  nil)

(defmethod formal-params ((imp importing))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(9.2.93) pseudo-normalize uses assert-if to simplify the given
;;expression, but does not really normalize.  Equivalent expressions can
;;differ on the order of terms in a product, the order of addends, and the
;;order of the if-the-else conditionals.  This is for use in the typechecker
;;when setting actuals so that they can be compared with other possibly
;;equivalent actuals.  

(defvar *pseudo-normalize-hash* nil
  "Do not make this a hash-table, or it will be pointed to from static
space")

(defvar *pseudo-normalize-translate-id-hash* nil)

(defvar *pseudo-normalize-translate-to-prove-hash* nil)

(defvar *pseudo-normalizing* nil)

(defvar *pseudo-normalize-subtype-hash* nil)

(defun reset-pseudo-normalize-caches ()
  (if *pseudo-normalize-hash*
      (clrhash *pseudo-normalize-hash*)
      (setq *pseudo-normalize-hash*
	    (make-pvs-hash-table)))
  (if *pseudo-normalize-translate-to-prove-hash*
      (clrhash *pseudo-normalize-translate-to-prove-hash*)
      (setq *pseudo-normalize-translate-to-prove-hash*
	    (make-pvs-hash-table)))
  (if *pseudo-normalize-translate-id-hash*
      (clrhash *pseudo-normalize-translate-id-hash*)
      (setq *pseudo-normalize-translate-id-hash*
	    (make-pvs-hash-table)))
  (if *pseudo-normalize-subtype-hash*
      (clrhash *pseudo-normalize-subtype-hash*)
      (setq *pseudo-normalize-subtype-hash*
	    (make-pvs-hash-table))))

(defun remove-pseudo-normalize-freevar-entries ()
  (maphash #'(lambda (x y)
	       (declare (ignore y))
	       (when (freevars (car x))
		 (remhash x *pseudo-normalize-hash*)))
	   *pseudo-normalize-hash*))

(defun remove-pseudo-normalize-cache ()
  (setq *pseudo-normalize-hash* nil)
  (setq *pseudo-normalize-translate-id-hash* nil))

(defun pseudo-normalize (expr &optional include-typepreds?)
  (if (or *pseudo-normalizing*		; Don't allow recursion
	  (number-expr? expr)
	  (and (name? expr)
	       (null (actuals expr))
	       (null (mappings expr)))
	  (typep (declaration *current-context*)
		 '(or adt-constructor-decl adt-recognizer-decl
		      adt-accessor-decl adt-def-decl))
	  (not (fully-instantiated? expr)))
      expr
      (let* ((key (cons expr include-typepreds?))
	     (nexpr (gethash key *pseudo-normalize-hash*)))
	(if nexpr
	    (if (tc-eq nexpr expr)
		expr
		nexpr)
	    (let* ((*pseudo-normalizing* (if include-typepreds?
					     'include-typepreds?
					     t))
		   (*subtype-hash* (when include-typepreds?
				     (clrhash *pseudo-normalize-subtype-hash*)))
		   ;;(*beta-cache* (make-hash-table :test #'eq))
		   (*generate-tccs* 'none)
		   (*assert-typepreds* nil)
		   (*sequent-typealist* nil)
		   ;;(typealist primtypealist);;NSH(2.16.94)
		   (*local-typealist* nil)
		   (*assert-flag* 'simplify)
		   (*process-output* nil)
		   (*assert-if-arith-hash*
		    (or (and (boundp '*assert-if-arith-hash*) ;;NSH(11.30.95) 
			     *assert-if-arith-hash*) ;;not real shadowing
			(make-hash-table :test #'eq)))
		   (*current-decision-procedure* 'shostak))
	      (nprotecting-cong-state
	       ((*dp-state* (dpi-empty-state)))
	       (let ((result (if *translate-id-counter*
				 (assert-if-simplify expr)
				 (let* ((*translate-to-prove-hash*
					 (clrhash
					  *pseudo-normalize-translate-to-prove-hash*))
					(*translate-id-hash*
					 (clrhash
					  *pseudo-normalize-translate-id-hash*))
					(*translate-id-counter* nil)
					(typealist typealist))
				   (newcounter *translate-id-counter*)
				   (assert-if-simplify expr)))))
		 (setf (gethash key *pseudo-normalize-hash*) result)
		 ;; (unless (tc-eq result (partial-normalize expr))
		 ;;   (break "Different pseudo-normalize results"))
		 result)))))))

;; (defun partial-normalize (expr)
;;   (partial-normalize* expr))

;; (defmethod partial-normalize* ((ex lambda-expr))
;;   (lcopy ex 'expression (partial-normalize* (expression ex))))

;; (defmethod partial-normalize* ((ex application))
;;   (cond ((or (is-addition? ex)
;; 	     (is-subtraction? ex))
;; 	 (partial-normalize-addition (arguments ex)))
;; 	((is-plus? (operator ex))
;; 	 (partial-normalize-sums (arguments ex)))
;; 	((is-times? (operator ex))
;; 	 (partial-normalize-times (arguments ex)))
;;   (lcopy 


#-(or gcl cmu sbcl)
(defun direct-superclasses (class)
  (slot-value class 'clos::direct-superclasses))

#+gcl
(defun direct-superclasses (class)
  (slot-value class 'pcl:class-direct-superclasses))

#+cmu
(defun direct-superclasses (class)
  (class-direct-superclasses class))

#+sbcl
(defun direct-superclasses (class)
  (sb-mop:class-direct-superclasses class))

(defun types-of (obj)
  (let ((types nil))
    (labels ((tof (type)
	      (unless (or (memq type types)
			  (memq type '(t standard-object)))
		(push type types)
		(let ((class (find-class type nil)))
		  (when class
		    (mapc #'(lambda (s) (tof (class-name s)))
			  (class-direct-superclasses class)))))))
      (tof (type-of obj)))
    (nreverse types)))



(defun fully-typed? (obj)
  (let ((untyped? nil)
	(expr nil))
    (mapobject #'(lambda (ex)
		   (unless untyped?
		     (multiple-value-bind (unt ex)
			 (untyped* ex)
		       (when unt
			 (setq untyped? t
			       expr ex)))))
	       obj)
    (values (not untyped?) expr)))

(defmethod untyped* (obj)
  (declare (ignore obj))
  nil)

(defmethod untyped* ((expr expr))
  (values (not (type expr))
	  expr))

(defmethod untyped* ((expr bind-decl))
  (values (not (type expr)) expr))

(defmethod untyped* ((expr extraction-expr))
  (values (not (type expr))
	  expr))

(defmethod untyped* ((expr name-expr))
  (values (not (and (type expr) (resolution expr)))
	  expr))

(defmethod untyped* ((expr field-assignment-arg))
  nil)

(defmethod untyped* ((te type-name))
  (values (not (resolution te))
	  te))

(defmethod k-combinator? ((n adt-name-expr))
  nil)

(defmethod k-combinator? ((n projection-expr))
  nil)

(defmethod k-combinator? ((n name-expr))
  (k-combinator? (declaration n)))

(defmethod k-combinator? ((c const-decl))
  (and (definition c)
       (k-combinator? (args2 (car (last (def-axiom c)))))))

(defmethod k-combinator? ((e lambda-expr))
  (and (singleton? (bindings e))
       (let ((ee (expression e)))
	 (and (typep ee 'lambda-expr)
	      (typep (expression ee) 'name-expr)
	      (eq (declaration (expression ee)) (car (bindings e)))))))

(defmethod k-combinator? ((e expr))
  nil)

(defun same-declaration (x y)
  (eq (declaration x) (declaration y)))

;;; from subst-mod-params,
;;; returns either type-value or expr slot of actuals

(defmethod actual-value ((act actual))
  (with-slots (type-value expr) act
    (or type-value expr)))

(defmethod actual-value (ex)
  ex)

(defmethod module ((db dep-binding)) nil)

;;;

(defmethod domtype ((type funtype))
  (domtype* (domain type)))

(defmethod domtype* ((type dep-binding))
  (type type))

(defmethod domtype* (type)
  type)

(defun domain-types (type)
  (domain-types* (domain type)))

(defmethod domain-types* ((type dep-binding))
  (domain-types* (type type)))

(defmethod domain-types* ((type tupletype))
  (types type))

(defmethod domain-types* ((type type-expr))
  (list type))

;;;

(defmethod domain* ((te funtype) &optional domains)
  (domain* (range te) (cons (domain te) domains)))

(defmethod domain* ((te subtype) &optional domains)
  (domain* (supertype te) domains))

(defmethod domain* ((te dep-binding) &optional domains)
  (domain* (type te) domains))

(defmethod domain* ((te type-expr) &optional domains)
  (nreverse domains))


(defmethod range* ((te funtype))
  (range* (range te)))

(defmethod range* ((te subtype))
  (let ((stype (find-supertype te)))
    (if (typep stype 'funtype)
	(range* stype)
	te)))

(defmethod range* ((te dep-binding))
  (range* (type te)))

(defmethod range* ((te type-expr))
  te)

;;; operator* returns the eventual operator of an application, or the
;;;   expression itself if it is not an application.
;;;   e.g., if e is f(1,2)(x)(a,b,c), then
;;;   (operator* e)  ==>  f
;;; arg* returns a list of the arguments of an application, or nil if it
;;;   is not an application.  This will be a list of expressions.
;;;   (arg* e) ==> ((1,2) x (a,b,c))
;;; arguments returns a list of the arguments to an application.
;;;   (arguments e) ==> (a b c)
;;; arguments* returns a list of lists of arguments of an application.
;;;   (arguments* e) ==> ((1 2) (x) (a b c))
;;; argument-list returns a list of expressions.  For tuple-exprs, it pulls
;;;   out the exprs, lists are simply returned, and for anything else it
;;;   simply returns the singleton list of the expression.
;;;   (argument-list e) ==> (f(1,2)(x)(a,b,c))

#-(or cmu sbcl)
(defmethod operator* ((expr application))
  (with-slots (operator) expr
    (operator* operator)))

#-(or cmu sbcl)
(defmethod operator* ((expr expr))
  expr)

#+(or cmu sbcl)
(defun operator* (expr)
  (if (application? expr)
      (operator* (operator expr))
      expr))
  

(defmethod argument* ((expr application) &optional args)
  (with-slots (operator argument) expr
    (argument* operator (cons argument args))))

(defmethod argument* ((expr expr) &optional args)
  args)

(defmethod arguments ((expr projection-application))
  (with-slots (argument) expr
    (argument-list argument)))

(defmethod arguments ((expr injection-application))
  (with-slots (argument) expr
    (argument-list argument)))

(defmethod arguments ((expr injection?-application))
  (with-slots (argument) expr
    (argument-list argument)))

(defmethod arguments ((expr extraction-application))
  (with-slots (argument) expr
    (argument-list argument)))

(defmethod arguments ((expr application))
  (with-slots (argument) expr
    (argument-list (argument expr))))

(defmethod argument-list ((expr tuple-expr))
  (with-slots (exprs) expr
    exprs))

(defmethod argument-list ((expr expr))
  (list expr))

(defmethod argument-list ((list list))
  (assert (every #'expr? list))
  list)

(defmethod arguments* ((expr application) &optional accum)
  (with-slots ((op operator)) expr
    (arguments* op (cons (arguments expr) accum))))

(defmethod arguments* ((expr expr) &optional accum)
  accum)

(defmethod ptypes ((expr expr))
  (if (type expr) (list (type expr)) (types expr))
  ;;(or (types expr) (and (type expr) (list (type expr))))
  )

(defun split-on (pred list)
  (split-on* pred list nil nil))

(defun split-on* (pred list match rest)
  (if (null list)
      (values (nreverse match) (nreverse rest))
      (if (funcall pred (car list))
	  (split-on* pred (cdr list) (cons (car list) match) rest)
	  (split-on* pred (cdr list) match (cons (car list) rest)))))

(defmethod assuming-instances ((decl declaration))
  (let* ((theory (module decl))
	 (decls (all-decls theory))
	 (not-visible (cdr (memq decl decls))))
    (remove-if #'(lambda (ai)
		   (intersection (collect-references ai) not-visible))
      (assuming-instances theory))))

#+gcl
(defun hash-table-test (ht)
  #'equal)

(defmethod id ((map mapping))
  (id (lhs map)))

(defmethod id ((expr coercion))
  (id (argument expr)))

(defmethod id ((ex implicit-conversion))
  (id (argument ex)))

(defmethod constructor ((expr coercion))
  (constructor (argument expr)))

(defun make-negated-conjunction (e1 e2)
  (make!-negation (make!-conjunction e1 e2)))

#+allegro
(defvar *pvs-gc-count* 0)
#+allegro
(defvar *prevent-gc-recursion* nil)
#+allegro-v4.2
(defun pvs-gc-after-hook (global-p to-new to-old eff)
  (declare (ignore eff to-new))
  (unless *prevent-gc-recursion*
    (cond (global-p
	   (setq *pvs-gc-count* 0))
	  (t (setq *pvs-gc-count* (+ *pvs-gc-count* to-old))
	     (if (> *pvs-gc-count* excl:*tenured-bytes-limit*)
		 (excl:without-interrupts
		  (setq *prevent-gc-recursion* t)
		  (format t ";;; GC:")
		  (excl:gc t)
		  (format t ";;; Finished GC~%")
		  (setq *pvs-gc-count* 0)
		  (setq *prevent-gc-recursion* nil)))))))
#+(and allegro (not allegro-v4.2))
(defun pvs-gc-after-hook (global-p to-new to-old eff to-be-allocated)
  (declare (ignore eff to-new to-be-allocated))
  (unless *prevent-gc-recursion*
    (cond (global-p
	   (setq *pvs-gc-count* 0))
	  (t (setq *pvs-gc-count* (+ *pvs-gc-count* to-old))
	     (if (> *pvs-gc-count* excl:*tenured-bytes-limit*)
		 (excl:without-interrupts
		  (setq *prevent-gc-recursion* t)
		  (unless *disable-gc-printout*
		    (format t ";;; GC:"))
		  (excl:gc t)
		  (unless *disable-gc-printout*
		    (format t ";;; Finished GC~%"))
		  (setq *pvs-gc-count* 0)
		  (setq *prevent-gc-recursion* nil)))))))

#+allegro
(eval-when (load)
  (when (compiled-function-p #'pvs-gc-after-hook)
    (setf excl:*gc-after-hook* #'pvs-gc-after-hook)))

#+cmu
(eval-when (load)
  (setf extensions:*gc-verbose* nil))

(defun reset-print-equal-cache ()
  (if *term-print-strings*
      (clrhash *term-print-strings*)
      (setq *term-print-strings* (make-hash-table :test #'eq))))

(defun print-equal (x y)
  (string= (print-string x) (print-string y)))

(defun print-string (x)
  (or (gethash x *term-print-strings*)
      (let ((*sb-print-depth* nil)
	    (*sb-print-length* nil))
	(setf (gethash x *term-print-strings*)
	      (format nil "~a" x)))))

;; (defun remove-coercions (obj)
;;   (gensubst obj
;;     #'(lambda (ex) (argument ex))
;;     #'(lambda (ex) (typep ex 'coercion))))

(defun remove-coercions (ex)
  (remove-coercions* ex))

(defmethod remove-coercions* ((name modname))
  (lcopy name 'actuals (remove-coercions* (actuals name))))

(defmethod remove-coercions* ((list list))
  (let ((nlist (mapcar #'remove-coercions* list)))
    (if (equal list nlist)
	list
	nlist)))

(defmethod remove-coercions* ((act actual))
  (if (type-value act)
      act
      (lcopy act
	'expr (remove-coercions* (expr act)))))

(defmethod remove-coercions* ((ex expr))
  ex)

(defmethod remove-coercions* ((ex coercion))
  (argument ex))

(defun file-equal (file1 file2)
  (let ((finfo1 (get-file-info file1)))
    (and finfo1
	 (equal finfo1 (get-file-info file2)))))

(defmethod resolution ((te datatype-subtype))
  (resolution (declared-type te)))

(defmethod module-instance ((te datatype-subtype))
  (module-instance (declared-type te)))

(defmethod declaration ((te datatype-subtype))
  (declaration (declared-type te)))

(defmethod actuals ((te datatype-subtype))
  (actuals (declared-type te)))

(defmethod adt ((te datatype-subtype))
  (adt (declared-type te)))

(defmethod id ((te datatype-subtype))
  (id (declared-type te)))

(defun expr-size (expr)
  (let ((depth 0))
    (mapobject #'(lambda (ex) (declare (ignore ex)) (incf depth) nil) expr)
    depth))

(defun date-string (time)
  (multiple-value-bind (sec min hour date month year day-of-week dst time-zone)
      (decode-universal-time time)
    (declare (ignore dst time-zone))
    (format nil "~a ~a ~d ~2,'0d:~2,'0d:~2,'0d ~d"
      (nth day-of-week '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
      (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
		 "Aug" "Sep" "Oct" "Nov" "Dec"))
      date hour min sec year)))

(defconstant millisecond-factor
  (/ 1000 internal-time-units-per-second))

#+allegro
(defun get-run-time ()
  (multiple-value-bind (rtuser rtsys gcuser gcsys)
      (excl::get-internal-run-times)
    (declare (ignore rtsys gcsys))
    (- rtuser gcuser)))

#-allegro
(defun get-run-time ()
  (get-internal-run-time))

(defun runtime-since (time)
  (floor (* (- (get-run-time) time) millisecond-factor)))

(defun realtime-since (time)
  (floor (* (- (get-internal-real-time) time) millisecond-factor)))
  

(defmethod change-application-class-if-necessary (expr new-expr)
  (declare (ignore expr))
  new-expr)

(defmethod change-application-class-if-necessary ((expr application) new-expr)
  (with-slots (operator argument) new-expr
    (when (and (infix-op? operator)
	       (= (arg-length argument) 2))
      (change-class new-expr 'infix-application)
      (incf (parens new-expr)))
    (change-application-class-if-needed new-expr)
    new-expr))

(defmethod change-application-class-if-necessary ((expr infix-application)
						  new-expr)
  (with-slots (operator argument) new-expr
    (when (or (not (infix-op? operator))
	      (not (= (arg-length argument) 2)))
      (change-class new-expr 'application)
      (setf (parens new-expr) 0))
    (change-application-class-if-needed new-expr)
    new-expr))

(defmethod change-application-class-if-necessary ((expr unary-application)
						  new-expr)
  (with-slots (operator argument) new-expr
    (when (or (not (unary-op? operator))
	      (not (= (arg-length argument) 1)))
      (change-class new-expr 'application)
      (setf (parens new-expr) 0))
    (change-application-class-if-needed new-expr)
    new-expr))

(defmethod unary-op? (expr)
  (declare (ignore expr))
  nil)

(defmethod unary-op? ((expr name-expr))
  (with-slots (id) expr
    (memq id *unary-operators*)))

(defmethod unary-op? ((id symbol))
  (memq id *unary-operators*))

(defmethod unary-op? ((id string))
  (memq (intern id) *unary-operators*))

(defmethod infix-op? ((id symbol))
  (memq id *infix-operators*))

(defmethod infix-op? ((id string))
  (memq (intern id) *infix-operators*))

;(defstruct (pvs-tables (:conc-name nil))
;  ;;(judgement-types-cache
;  ;; (make-pvs-hash-table))
;  (subst-mod-params-cache
;   (make-pvs-hash-table))
;  )
;
;(defun reset-pvs-tables (table)
;  ;;(clrhash (judgement-types-cache table))
;  (clrhash (subst-mod-params-cache table))
;  )
;
;(defmacro lookup-table (obj table &key (test #'eq))
;  `(lookup-table* ,obj ,table *pvs-global-tables* ,test))
;
;(defun lookup-table* (obj table tables test)
;  (when tables
;    (multiple-value-bind (value there?)
;	(lookup-table** obj (funcall table (car tables)) test)
;      (if there?
;	  (values value there?)
;	  (lookup-table* obj table (cdr tables) test)))))
;
;(defun lookup-table** (obj table test)
;  (if (hash-table-p table)
;      (gethash obj table)
;      (let ((pair (assoc obj table :test test)))
;	(values (cdr pair) (when pair t)))))
;
;(defsetf lookup-table (obj table &key (test #'eq)) (new)
;  `(setf-lookup-table* ,obj ,new ,table ,test))
;
;(defun setf-lookup-table* (obj new table test)
;  (let ((tbl (funcall table (car *pvs-global-tables*))))
;    (if (hash-table-p tbl)
;	(setf (gethash obj tbl) new)
;	(let ((pair (assoc obj tbl :test test)))
;	  (if pair
;	      (setf (cdr pair) new)
;	      (push (cons obj new)
;		    (funcall table (car *pvs-global-tables*))))))))

;; (defvar *dependent-type-substitutions*
;;   (make-pvs-hash-table))

;; (defmethod dep-substit ((list list) alist)
;;   (let ((elt (cons list alist)))
;;     (or (gethash elt *dependent-type-substitutions*)
;; 	(let ((nlist (substit list alist)))
;; 	  (mapc #'(lambda (e ne) (cache-dep-substitutions e ne alist))
;; 		list nlist)
;; 	  (setf (gethash elt *dependent-type-substitutions*) nlist)))))

;; (defun cache-dep-substitutions (old new alist)
;;   (setf (gethash (cons old alist) *dependent-type-substitutions*) new))

;; (defmethod dep-substit (obj alist)
;;   (let ((elt (cons obj alist)))
;;     (or (gethash elt *dependent-type-substitutions*)
;; 	(setf (gethash elt *dependent-type-substitutions*)
;; 	      (substit obj alist)))))

(defmethod lift-predicates-in-quantifier ((ex forall-expr) &optional exclude)
  (multiple-value-bind (nbindings preds)
      (collect-bindings-predicates (bindings ex) exclude)
    (if preds
	(copy ex
	  'bindings nbindings
	  'expression (make!-implication
		       (make!-conjunction* preds)
		       (substit (expression ex)
			 (pairlis (bindings ex) nbindings))))
	ex)))

(defmethod lift-predicates-in-quantifier ((ex exists-expr) &optional exclude)
  (multiple-value-bind (nbindings preds)
      (collect-bindings-predicates (bindings ex) exclude)
    (if preds
	(copy ex
	  'bindings nbindings
	  'expression (make!-conjunction
		       (make!-conjunction* preds)
		       (substit (expression ex)
			 (pairlis (bindings ex) nbindings))))
	ex)))

(defmethod lift-predicates-in-quantifier ((ex expr) &optional exclude)
  (declare (ignore exclude))
  ex)

(defun collect-bindings-predicates (bindings exclude &optional nbindings preds)
  (if (null bindings)
      (values (nreverse nbindings)
	      (apply #'nconc (nreverse preds)))
      (multiple-value-bind (nbinding npreds)
	  (collect-binding-predicates (car bindings)
				      (if (listp exclude)
					  exclude
					  (list exclude)))
	(collect-bindings-predicates
	 (if (eq nbinding (car bindings))
	     (cdr bindings)
	     (substit (cdr bindings) (acons (car bindings) nbinding nil)))
	 exclude
	 (cons nbinding nbindings)
	 (cons npreds preds)))))

(defun collect-binding-predicates (binding exclude)
  (let* ((etype (find-if #'(lambda (ety)
			     (compatible? (type binding) ety))
		  exclude))
	 (type (if etype
		   (compatible-type etype (type binding))
		   (find-supertype (type binding)))))
    (if (eq type (type binding))
	(values binding nil)
	(let* ((nbd (make!-bind-decl (id binding) type))
	       (nvar (make-variable-expr nbd))
	       (preds (collect-predicates (type binding) type nvar)))
	  (values nbd preds)))))

(defmethod collect-predicates (subtype supertype expr)
  (compatible-preds supertype subtype expr))

(defun always-true (x)
  (declare (ignore x))
  t)

(defun always-false (x)
  (declare (ignore x))
  nil)

(defmethod constant? ((expr binding))
  nil)

(defmethod constant? ((expr name-expr))
  (not (variable? expr)))

(defmethod constant? ((expr projection-expr))
  t)

(defmethod constant? ((expr injection-expr))
  t)

(defmethod constant? ((expr field-assignment-arg))
  t)

(defmethod constant? ((expr t))
  nil)

(defmethod variable? ((expr binding))
  t)

(defmethod variable? ((expr name-expr))
  (with-slots (resolutions) expr
    (assert (singleton? resolutions))
    (typep (declaration (car resolutions)) '(or var-decl binding))))

(defmethod variable? ((expr field-assignment-arg))
  nil)

(defmethod variable? ((expr t))
  nil)


;; Destructuring of curried applications and detupling
;; e.g. f(a)(b, c)(d) ==> f (a b c d)

(defun destructure-application (e &optional acc)
  (if (application? e)
      (destructure-application (operator e)
			       (append (arguments e) acc))
    (values e acc)))

;; Destructering of universal and existential-strength quantifications
;; in a list of bindings and the body

(defmethod destructure-existential ((fml exists-expr) &optional bndngs negative?)
  (destructure-existential (expression fml)
			   (append (bindings fml) bndngs)
			   negative?))

(defmethod destructure-existential ((fml negation) &optional bndngs negative?)
  (destructure-universal (args1 fml) bndngs (not negative?)))

(defmethod destructure-existential ((fml expr) &optional bndngs negative?)
  (values (nreverse bndngs)
	  (if negative? (make!-negation fml) fml)))

(defmethod destructure-universal ((fml forall-expr) &optional bndngs negative?)
  (destructure-universal (expression fml)
			 (append (bindings fml) bndngs)
			 negative?))

(defmethod destructure-universal ((fml negation) &optional bndngs negative?)
  (destructure-universal (args1 fml) bndngs (not negative?)))

(defmethod destructure-universal ((fml expr) &optional bndngs negative?)
  (values (nreverse bndngs)
	  (if negative? (make!-negation fml) fml)))

;; Tests if a quantified formula is of universal or
;; of existential strengths

(defmethod essentially-universal? ((fml forall-expr))
  fml)

(defmethod essentially-universal? ((fml negation))
  (essentially-existential? (args1 fml)))

(defmethod essentially-universal? ((fml expr))
  nil)

(defmethod essentially-existential? ((fml exists-expr))
  fml)

(defmethod essentially-existential? ((fml negation))
  (essentially-universal? (args1 fml)))

(defmethod essentially-existential? ((fml expr))
  nil)

;; Lazy copying for some built-in predicates

(defun lcopy-negation (orig arg)
  (if (eq (argument orig) arg) orig
    (make!-negation arg)))

(defun lcopy-conjunction (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1)
	   (eq (args2 orig) arg2))
      orig
    (make!-conjunction arg1 arg2)))

(defun lcopy-disjunction (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1)
	   (eq (args2 orig) arg2))
      orig
    (make!-disjunction arg1 arg2)))

(defun lcopy-implication (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-implication arg1 arg2)))

(defun lcopy-branch (orig cond-expr then-expr else-expr)
  (if (and (eq (condition orig) cond-expr)
	   (eq (then-part orig) then-expr)
	   (eq (else-part orig) else-expr))
      orig
    (make!-if-expr cond-expr then-expr else-expr)))

(defun lcopy-iff (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-iff arg1 arg2)))

(defun lcopy-equation (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-equation arg1 arg2)))

(defun lcopy-disequation (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-disequation arg1 arg2)))

;; checks if argument expression is known to be an integer

(defun integer? (expr)
  (or (and (type expr)
	   (subtype-of? (type expr) *integer*))
      (and (number-expr? expr)
	   (integerp (number expr)))
      (some #'(lambda (type)
		(subtype-of? type *integer*))
	    (judgement-types+ expr))))

(defun real? (expr)
  (or (and (type expr)
	   (subtype-of? (type expr) *real*))
      (and (number-expr? expr)
	   (rationalp (number expr)))
      (some #'(lambda (type)
		(subtype-of? type *real*))
	    (judgement-types+ expr))))

(defmethod formals ((decl field-decl)) nil)

(defmethod formals ((map mapping)) nil)

;;; sexp converts a given object to a list;
;;; create-date run-date status real-time run-time interactive?

(defmethod sexp ((prinfo proof-info))
  (with-slots (id description create-date ;;run-date
		  script ;;status
		  refers-to ;;real-time run-time interactive?
		  decision-procedure-used)
      prinfo
    (list id description create-date ;;run-date
	  script ;;status
	  (sexp refers-to) ;;real-time run-time interactive?
	  decision-procedure-used)))

(defmethod sexp ((list list))
  (mapcar #'sexp list))

(defmethod sexp ((dref decl-reference))
  (with-slots (id class type theory-id library) dref
    (list id class type theory-id library)))

(defmethod sexp ((decl declaration))
  (list (id decl)
	(type-of decl)
	(when (and (typed-declaration? decl)
		   (not (typep decl 'formal-type-decl)))
	  (or (declared-type-string decl)
	      (setf (declared-type-string decl)
		    (unparse (or (declared-type decl)
				 (type decl)) :string t))))
	(when (module decl) (id (module decl)))
	(when (typep (module decl) 'library-theory)
	  (lib-ref (module decl)))))

(defmethod sexp ((theory module))
  (list (id theory)
	'module))

(defmethod update-instance-for-redefined-class :before
  ((fdecl formula-decl) added deleted plist &rest initargs)
  (declare (ignore added deleted))
  (when (and plist
	     (getf plist 'justification))
    (let ((prinfo (make-proof-info (getf plist 'justification)
				   (next-proof-id fdecl))))
      (setf (refers-to prinfo) (getf plist 'proof-refers-to))
      (when (getf plist 'proof-time)
	(setf (run-time prinfo) (car (getf plist 'proof-time)))
	(setf (real-time prinfo) (cadr (getf plist 'proof-time)))
	(setf (interactive? prinfo) (caddr (getf plist 'proof-time))))
      (setf (status prinfo) (getf plist 'proof-status))
      (setf (proofs fdecl) (list prinfo))
      (setf (default-proof fdecl) prinfo)
      (setf (decision-procedure-used prinfo)
	    (if (getf plist 'new-ground?) 'cyrluk 'shostak)))))

(defmethod justification ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (script (default-proof decl))))

(defmethod (setf justification) (just (decl formula-decl))
  (ensure-default-proof decl)
  (setf (script (default-proof decl))
	(extract-justification-sexp just)))

(defmethod proof-status ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (status (default-proof decl))))

(defmethod (setf proof-status) (stat (decl formula-decl))
  (ensure-default-proof decl)
  (setf (status (default-proof decl)) stat))

(defmethod decision-procedure-used ((decl formula-decl))
  (cond ((proofs decl)
	 (ensure-default-proof decl)
	 (or (decision-procedure-used (default-proof decl))
	     'shostak))
	(t 'shostak)))

(defmethod (setf decision-procedure-used) (dp (decl formula-decl))
  (ensure-default-proof decl)
  (setf (decision-procedure-used (default-proof decl)) dp))

(defmethod proof-refers-to ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (refers-to (default-proof decl))))

(defmethod (setf proof-refers-to) (refs (decl formula-decl))
  (ensure-default-proof decl)
  (setf (refers-to (default-proof decl)) refs))

(defmethod real-time ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (real-time (default-proof decl))))

(defmethod (setf real-time) (time (decl formula-decl))
  (ensure-default-proof decl)
  (setf (real-time (default-proof decl)) time))

(defmethod run-time ((decl formula-decl))
  (when (proofs decl)
    (ensure-default-proof decl)
    (run-time (default-proof decl))))

(defmethod (setf run-time) (time (decl formula-decl))
  (ensure-default-proof decl)
  (setf (run-time (default-proof decl)) time))

(defun ensure-default-proof (fdecl &optional script id description)
  (unless (default-proof fdecl)
    (if (proofs fdecl)
	(setf (default-proof fdecl) (car (proofs fdecl)))
	(make-default-proof fdecl script id description))))

(defun make-default-proof (fdecl script &optional id description)
  (let* ((pid (or id (next-proof-id fdecl)))
	 (prinfo (make-proof-info (or script (tcc-strategy fdecl))
				  pid description)))
    (setf (decision-procedure-used prinfo) *default-decision-procedure*)
    (push prinfo (proofs fdecl))
    (setf (default-proof fdecl) prinfo)))

(defun next-proof-id (fdecl &optional (num 1))
  (let ((id (makesym "~a-~d" (id fdecl) num)))
    (if (and (slot-boundp fdecl 'proofs)
	     (member id (proofs fdecl)
		     :test #'(lambda (x y) (eq x (id y)))))
	(next-proof-id fdecl (1+ num))
	id)))

(defmethod id ((ex number-expr)) (number ex))
(defmethod mod-id ((ex number-expr)) nil)
(defmethod actuals ((ex number-expr)) nil)
(defmethod mappings ((ex number-expr)) nil)
(defmethod library ((ex number-expr)) nil)

(defun name-to-modname (name)
  (mk-modname (or (mod-id name) (id name))
    (actuals name)
    (library name)
    (mappings name)))

(defun equality? (obj)
  (equation? obj))

(defmethod negate! ((formula negation))
  (argument formula))

(defmethod negate! (formula)
  (make!-negation formula))

(defmethod from-macro ((ex expr))
  (assert (current-theory))
  (cdr (assq ex (macro-expressions (current-theory)))))

(defmethod macro-expressions ((dt recursive-type))
  nil)
  
(defmethod (setf from-macro) (ex1 (ex2 expr))
  (assert (current-theory))
  (push (cons ex2 ex1) (macro-expressions (current-theory))))

(defun collect-theory-instances (theory)
  (let ((th (get-theory theory))
	(theory-instances nil))
    (mapobject
     #'(lambda (x)
	 (when (and (name? x)
		    (resolution x)
		    (not (eq (id (module-instance x)) (id th))))
	   (cond ((freevars (module-instance x))
		  (format t
		      "~%The generic for ~a is collected since it contains free variables"
		    (module-instance x))
		  (pushnew (copy (module-instance x) 'actuals nil)
			   theory-instances
			   :test #'tc-eq))
		 (t (pushnew (module-instance x) theory-instances
			     :test #'tc-eq)))))
     th)
    theory-instances))

(defun expose-binding-types (expr)
  (let ((*visible-only* t))
    (gensubst expr #'expose-binding-types! #'expose-binding-types?)))

(defmethod expose-binding-types? (ex)
  (declare (ignore ex))
  nil)

(defmethod expose-binding-types? ((ex type-application))
  t)

(defmethod expose-binding-types? ((ex untyped-bind-decl))
  t)

(defmethod expose-binding-types! ((ex type-application))
  ex)

(defmethod expose-binding-types! ((ex untyped-bind-decl))
  (let ((dtype (or (and (type ex) (print-type (type ex)))
		   (declared-type ex)
		   (type ex))))
    (if dtype
	(change-class (copy ex 'declared-type dtype) 'bind-decl)
	ex)))

(defmethod update-instance-for-different-class :after
  ((prev syntax) (cur syntax) &rest initargs)
  (setf (pvs-sxhash-value cur) nil))

(defmethod update-instance-for-different-class :after
  ((prev expr) (cur expr) &rest initargs)
  (setf (free-variables cur) 'unbound)
  (setf (free-parameters cur) 'unbound))


;; Like remove, but only copies as much as necessary
(defun remove* (elt list &optional (olist list) elts pelts)
  (cond ((null list)
	 (nconc (nreverse pelts) olist))
	((eq elt (car list))
	 (remove* elt (cdr list) (cdr list) nil (nconc elts pelts)))
	(t (remove* elt (cdr list) olist (cons (car list) elts) pelts))))


;; (defmethod initialize-instance :around ((res resolution) &rest initargs)
;;   (prog1 (call-next-method)
;;     (when (and (module-instance res)
;; 	       (string= (unparse (module-instance res) :string t) "bv[2 * n]")
;; 	       (type res)
;; 	       (string= (unparse (type res) :string t) "bvec[k]"))
;;       (break "Strange"))
;;     (unless (or (null (module-instance res))
;; 		(null (declaration res))
;; 		(library (module-instance res))
;; 		(not (library-datatype-or-theory? (module (declaration res)))))
;;       (break "Bad init"))))

;; (defmethod (setf module-instance) :around (v (res resolution))
;;   (unless (or (null v)
;; 	      (null (declaration res))
;; 	      (library v)
;; 	      (not (library-datatype-or-theory? (module (declaration res)))))
;;     (break "Bad setf"))
;;   (call-next-method))


;;; An improved trace, in that it expands generic functions to their
;;; method forms, so that the individual methods can be determined
;;; from the trace.  Otherwise only the function name is produced,
;;; which is not easy to debug.
;;;
;;; Use this exactly like trace, e.g., (trace* substit substit*) or (trace*  

(defmacro trace* (&rest trace-forms)
  `(trace ,@(expand-trace-form-methods trace-forms)))

(defun expand-trace-form-methods (trace-forms &optional nforms)
  (if (null trace-forms)
      (nreverse nforms)
      (let ((nform (expand-trace-form-method (car trace-forms))))
	(expand-trace-form-methods
	 (cdr trace-forms)
	 (if (consp nform)
	     (append nform nforms)
	     (cons nform nforms))))))

(defun expand-trace-form-method (trace-form)
  (if (consp trace-form)
      (if (or (eq (car trace-form) 'method)
	      (consp (car trace-form))
	      (not (typep (symbol-function (car trace-form))
			  'generic-function)))
	  trace-form
	  (mapcar #'(lambda (method-form)
		      (cons method-form (cdr trace-form)))
	    (collect-method-forms (car trace-form))))
      (if (typep (symbol-function trace-form) 'generic-function)
	  (mapcar #'list (collect-method-forms trace-form))
	  trace-form)))

(defun trace-methods (funsym)
  (dolist (frm (collect-method-forms funsym))
    (eval `(trace (,frm)))))

(defun collect-method-forms (funsym)
  (mapcar #'(lambda (m)
	      (let ((q (method-qualifiers m))
		    (a (mapcar #'class-name (method-specializers m))))
		`(method ,funsym ,@q ,a)))
    (generic-function-methods (symbol-function funsym))))

;;; equals is like equalp, but is case-sensitive
(defun equals (x y)
  (or (eq x y)
      (typecase x
	(number (and (numberp y) (= x y)))
	(character (and (characterp y) (char= x y)))
	(string (if (stringp y)
		    (#+allegro excl::simple-string= #-allegro string= x y)
		    (and (arrayp y)
			 (equals-arrays x y))))
	(cons (and (consp y)
		   (equals (car x) (car y))
		   (equals (cdr x) (cdr y))))
	;; Note that this also works for strings, as well as comparing a string
	;; to an array of chars
	(array (and (arrayp y)
		    (equals-arrays x y)))
	(hash-table (and (hash-table-p y)
			 (equals-hashes x y)))
	(structure-object
	 (and (eq (type-of x) (type-of y))
	      (let ((slots (class-slots (class-of x))))
		(every #'(lambda (slot)
			   (let ((name (slot-definition-name slot)))
			     (equals (slot-value x name) (slot-value y name))))
		       slots)))))))
		     
(defun equals-arrays (x y)
  (and (equal (array-dimensions x) (array-dimensions y))
       (let ((size (array-total-size x)))
	 (or (zerop size)
	     (equals-arrays* x y (1- size))))))

(defun equals-arrays* (x y i)
  (declare (type (array *) x) (type (array *) y) (type fixnum i))
  (and (equals (row-major-aref x i) (row-major-aref y i))
       (or (zerop i)
	   (equals-arrays* x y (1- i)))))

(defun equals-hashes (x y)
  (and (eq (hash-table-test x) (hash-table-test y))
       (= (hash-table-count x) (hash-table-count y))
       (let ((equals? t))
	 (maphash #'(lambda (key value1)
		      (multiple-value-bind (value2 there?)
			  (gethash key y)
			(unless (and there?
				     (equals value1 value2))
			  (setf equals? nil))))
		  x)
	 equals?)))

(defun equals-structs (x y)
  (let ((slots (class-slots (class-of x))))
    (every #'(lambda (slot)
	       (let ((name (slot-value slot
				       '#+allegro excl::name
				       #+cmu pcl::name
				       #+sbcl sb-pcl::name)))
		 (equals (slot-value x name) (slot-value y name))))
	   slots)))

(defun posnat? (x) (and (integerp x) (plusp x)))

(defun parse-unparse (ex &optional (nt 'expr))
  (pc-parse (unparse ex :string t) nt))

(defun make-new-symbol (string &optional num)
  (let ((str (if num (format nil "~a-~d" string num) string)))
    (if (find-symbol str)
	(make-new-symbol string (if num (1+ num) 1))
	(intern str))))

(defmethod dep-binding-type ((te dep-binding))
  (type te))

(defmethod dep-binding-type ((te type-expr))
  te)

;; This is the function for making hash tables
(defun make-pvs-hash-table (&rest other-keys &key strong-eq? weak-keys?
				  &allow-other-keys)
  #+allegro
  (apply #'make-hash-table
    :hash-function 'pvs-sxhash
    :test (if strong-eq? 'strong-tc-eq 'tc-eq)
    :weak-keys weak-keys?
    :allow-other-keys t
    other-keys)
  #+cmu
  (apply #'make-hash-table
    :test (if strong-eq? 'strong-tc-eq-test 'tc-eq-test)
    :weak-p weak-keys?
    :allow-other-keys t
    other-keys)
  #+sbcl
  (apply #'make-hash-table
    :test (if strong-eq? 'strong-tc-eq 'tc-eq)
    :weakness (when weak-keys? :key-and-value)
    :allow-other-keys t
    other-keys)
  #-(or allegro cmu sbcl)
  (error "Need a hash-table for tc-eq for this lisp"))

#+cmu
(extensions:define-hash-table-test 'tc-eq-test #'tc-eq #'pvs-sxhash)
#+cmu
(extensions:define-hash-table-test 'strong-tc-eq-test
				   #'strong-tc-eq #'pvs-sxhash)
#+(and sbcl (not sunos))
(sb-ext:define-hash-table-test tc-eq pvs-sxhash)
#+(and sbcl (not sunos))
(sb-ext:define-hash-table-test strong-tc-eq pvs-sxhash)

#+(and sbcl sunos)
(sb-int:define-hash-table-test 'tc-eq #'tc-eq #'pvs-sxhash)
#+(and sbcl sunos)
(sb-int:define-hash-table-test 'strong-tc-eq #'strong-tc-eq #'pvs-sxhash)
