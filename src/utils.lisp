;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utils.lisp -- 
;; Author          : Sam Owre and N. Shankar
;; Created On      : Thu Dec  2 13:31:00 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 14:25:48 1998
;; Update Count    : 91
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;;(proclaim '(inline resolution))

#+allegro
(defun file-exists-p (file)
  (excl::filesys-inode (namestring file)))

#-allegro
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

#+allegro
(defmethod copy ((ht hash-table) &rest args)
  (let* ((test (hash-table-test ht))
	 (size (floor (hash-table-size ht) 1.5384616))
	 (new-ht (if (memq test '(eq eql equal equalp))
		     (make-hash-table :test test :size size)
		     (make-hash-table :test test :size size
				      :hash-function 'pvs-sxhash))))
    (maphash #'(lambda (id data)
		 (setf (gethash id (the hash-table new-ht)) data))
	     (the hash-table ht))
    new-ht))

(defmethod copy :around ((ex application) &rest args)
  (declare (ignore args))
  (let ((nex (call-next-method)))
    (if (or (not (type ex))
	    (eq (operator ex) (operator nex)))
	nex
	(change-application-class-if-necessary ex nex))))

;;; The following allows slot-exists-p to be called on anything.
;#+gcl
;(defmethod pcl::find-slot-definition (obj slot)
;  nil)

(defmacro lf (file &optional force)
  `(make-file ,file ,force))

(defun make-file (file &optional force)
  (let* ((source (make-file-name file))
	 (bin (make-pathname :type user::*pvs-binary-type*
			     :defaults source)))
    (unless (probe-file source)
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
  (dolist (dir *pvs-directories*)
    (let* ((defaults (or (probe-file (format nil "~a/~a/" *pvs-path* dir))
			 (directory-p dir)))
	   (path (make-pathname :name file :type "lisp" :defaults defaults)))
      (when (probe-file path)
	(return-from make-file-name path))))
  (error "File ~a.lisp cannot be found" file))

(defun compiled-file-older-than-source? (sourcefile binfile)
  (or (not (probe-file binfile))
      (file-older binfile sourcefile)))

(defun load-parser-source ()
  (lf "pvs-lexer" :source)
  (lf "pvs-parser" :source)
  (lf "pvs-unparser" :source)
  (lf "pvs-sorts" :source))

(defun load-parser ()
  (lf "pvs-lexer")
  (lf "pvs-parser")
  (lf "pvs-unparser")
  (lf "pvs-sorts"))

(defun show (obj)
  (describe obj)
  obj)

(defun special-variable-p (obj)
  #+lucid (system:proclaimed-special-p obj)
  #+kcl (system:specialp obj)
  #+allegro (clos::variable-special-p obj nil)
  #+harlequin-common-lisp (system:declared-special-p obj)
  #-(or lucid kcl allegro harlequin-common-lisp)
  (error "Need to handle special variables for this version of lisp"))


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
  *default-pathname-defaults*)

#+(or gcl cmu)
(defun set-working-directory (dir)
  (setq *default-pathname-defaults* (pathname dir)))

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
	       :arguments (list prot file)
	       :output "/dev/null"
	       :if-output-exists nil))

#+allegro
(defun chmod (prot file)
  (excl:run-shell-command (format nil "chmod ~a ~a" prot file)
			  :output "/dev/null"
			  :if-output-exists :append
			  :error-output "/dev/null"
			  :if-error-output-exists :append))

#+harlequin-common-lisp
(defun chmod (prot file)
  (foreign:call-system-showing-output
   (format nil "chmod ~a ~a" prot file)
   :prefix nil
   :show-cmd nil
   :output-stream (open "/dev/null" :direction :output
			  :if-exists :append)))

#+gcl
(defun chmod (prot file)
  (system (format nil "chmod ~a ~a" prot file)))

(defun pvs-current-directory ()
  (if (probe-file (working-directory))
      (shortname (working-directory))
      "/dev/null"))


;;; pvs-truename does what truename should do in the first place; it
;;; returns a cononical pathname.

(let ((canonical-pathnames (make-hash-table :test #'equal))
      (pathnames (make-hash-table :test #'equal)))
  #-gcl
  (defmethod pvs-truename ((file pathname))
    (let ((path (strip-pathname (truename file))))
      (or (gethash path canonical-pathnames)
	  (let* ((attrs (pvs-stable-file-attributes path))
		 (pname (gethash attrs pathnames)))
	    (if (and pname
		     (probe-file pname))
		(setf (gethash path canonical-pathnames) pname)
		(progn (setf (gethash attrs pathnames) path)
		       (setf (gethash path canonical-pathnames) path)))))))
  #+gcl
  (defmethod pvs-truename (file)
    (when (pathnamep file)
      (let ((path (strip-pathname (truename file))))
	(or (gethash path canonical-pathnames)
	    (let* ((attrs (pvs-stable-file-attributes path))
		   (pname (gethash attrs pathnames)))
	      (if (and pname
		       (probe-file pname))
		  (setf (gethash path canonical-pathnames) pname)
		  (progn (setf (gethash attrs pathnames) path)
			 (setf (gethash path canonical-pathnames) path))))))))
  (defmethod pvs-truename ((file string))
    (pvs-truename (pathname file))))

(defun strip-pathname (pathname)
  (let ((dirs (pathname-directory pathname)))
    (if (and (consp dirs)
	     (eq (car dirs) :root))
	(strip-pathname* pathname (cddr dirs))
	pathname)))

(defun strip-pathname* (pathname dirs)
  (let ((npath (make-pathname :directory (cons :root dirs)
			      :defaults pathname)))
    (if (and (probe-file npath)
	     (equal (truename npath) (truename pathname)))
	npath
	pathname)))

(defun get-formula (theory id)
  (or (find-if #'(lambda (decl)
		   (and (typep decl 'formula-decl)
			(eq (id decl) id)))
	(assuming theory))
      (find-if #'(lambda (decl)
		   (and (typep decl 'formula-decl)
			(eq (id decl) id)))
	(theory theory))))

(defun get-decl (theory id)
  (remove-if-not #'(lambda (d)
		     (and (declaration? d)
			  (eq (id d) id)))
      (all-decls theory)))

(defmethod get-theory ((name modname))
  (with-slots (library id) name
    (get-theory* id library)))

(defmethod get-theory ((str string))
  (get-theory* (intern str) nil))

(defmethod get-theory ((id symbol))
  (get-theory* id nil))

(defmethod get-theory ((mod module))
  mod)

(defmethod get-theory ((dt datatype))
  dt)

#+(or gcl cmu)
(defmethod get-theory (pathname)
  (when (pathnamep pathname)
    (get-theory (pathname-name pathname))))

#-(or gcl cmu)
(defmethod get-theory ((path pathname))
  (get-theory (pathname-name path)))

(defun get-theory* (id library)
  (let ((*current-context* (or *current-context* *prelude-context*)))
    (if library
	(let* ((libpath (get-library-pathname library))
	       (imphash (cadr (gethash libpath *imported-libraries*)))
	       (prehash (cadr (gethash libpath *prelude-libraries*))))
	  (if (and libpath
		   (equal (pvs-truename libpath)
			  (pvs-truename *pvs-context-path*)))
	      (gethash id *pvs-modules*)
	      (or (and imphash (gethash id imphash))
		  (and prehash (gethash id prehash))
		  (gethash id *prelude*))))
	(or (gethash id *prelude*)
	    ;;(gethash id *pvs-modules*)
	    (car (assoc id (prelude-libraries-uselist)
			:test #'(lambda (x y) (eq x (id y)))))
	    (let ((theories (get-imported-theories id)))
	      (cond ((cdr theories)
		     (pvs-message "Ambiguous theories - ~a"
		       (id (car theories))))
		    ((car theories))
		    (t (gethash id *pvs-modules*))))))))

(defun get-imported-theories (id)
  (let ((theories nil))
    (maphash #'(lambda (lib files&theories)
		 (declare (ignore lib))
		 (let ((th (gethash id (cadr files&theories))))
		   (when th
		     (push th theories))))
	     *imported-libraries*)
    theories))

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

(defmethod theory ((adt datatype))
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

(defun listify (x) (if (listp x) x (list x)))

(defun duplicates? (list &key (test #'eql) (key #'identity))
  (when list
    (or (car (member (funcall key (car list)) (cdr list) :test test :key key))
	(duplicates? (cdr list) :test test :key key))))

(defun has-id? (x)
  (or (symbolp x)(stringp x)(numberp x)
      (and (syntax? x)(slot-exists-p x 'id))))

(defun same-id (x y)
  (let ((idx (typecase x
	       (symbol x)
	       (string (intern x))
	       (number x)
	       (t (id x))))
	(idy (typecase y
	       (symbol y)
	       (string (intern y))
	       (number y)
	       (t (id y)))))
    (eq idx idy)))


;(defmethod make-specpath ((mod module-form))
;  (make-specpath (id mod)))

(defmethod make-specpath ((name symbol) &optional (ext "pvs"))
  (make-pathname :defaults *pvs-context-path* :name name :type ext))

(defmethod make-specpath ((name string) &optional (ext "pvs"))
  (make-pathname :defaults *pvs-context-path* :name name :type ext))

(defmethod make-specpath ((name name) &optional (ext "pvs"))
  (make-specpath (id name) ext))

(defmethod make-binpath ((name symbol))
  (make-pathname :defaults (working-directory)
		 :name name
		 :type "bin"))

(defmethod make-binpath ((name string))
  (make-pathname :defaults (working-directory)
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
	     (if (probe-file ndir)
		 ndir
		 cdir)))
	  (t cdir))))

;;; Checks if the dir is in fact a directory; returns the expanded
;;; pathname ending with a slash.

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

(defun splice (new-elt after-elt list)
  (let ((tail (and after-elt (memq after-elt list))))
    (if tail
	(append (ldiff list tail) (cons new-elt tail))
	(cons new-elt tail))))
      

(defmethod generated-by ((u importing)) nil)

(defun add-decl-test (x y)
  (and (eq (kind-of x) (kind-of y))
       (or (not (eq (kind-of x) 'expr))
	   (tc-eq (type x) (type y)))))


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
(defun write-permission? (&optional (dir (working-directory)))
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

#+(or lucid allegro harlequin-common-lisp)
(defun write-permission? (&optional (dir (working-directory)))
  (let* ((path (make-pathname :defaults dir :name "PVS" :type "tmp"))
	 (str (ignore-errors (open path :direction :output
				   :if-exists :append
				   :if-does-not-exist :create))))
    (when str (close str) (delete-file path) t)))

#+(or lucid allegro harlequin-common-lisp)
(defun file-write-permission? (file &optional (dir (working-directory)))
  (let* ((path (merge-pathnames file dir))
	 (str (ignore-errors (open path :direction :output
				   :if-exists :append
				   :if-does-not-exist :create))))
    (when str (close str) t)))

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
    (boolean-op? op '(when))))

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
		(tc-eq (type-value (car (actuals mi))) *boolean*))))))

(defmethod boolean-equality-op? (op)
  (declare (ignore op))
  nil)

(defmethod relation-type? ((te funtype))
  (with-slots (domain range) te
    (and (tc-eq range *boolean*)
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



(defmethod context ((theory module))
  (if (saved-context theory)
      (copy-context (saved-context theory))
      (decl-context (car (last (or (theory theory)
				   (assuming theory)
				   (formals theory))))
		    t)))

(defmethod context ((using importing))
  (decl-context using))

(defmethod context ((decl declaration))
  (decl-context decl))

(defun decl-context (decl &optional include?)
  (let* ((*generate-tccs* 'none)
	 (theory (module decl))
	 (all-decls (reverse (all-decls theory)))
	 (prev-decls (if include?
			 (memq decl all-decls)
			 (cdr (memq decl all-decls))))
	 (prev-imp (find-if #'importing? prev-decls))
	 (rem-decls (if prev-imp
			(ldiff prev-decls (memq prev-imp prev-decls))
			prev-decls)))
    (if (or (not prev-imp) (saved-context prev-imp))
	(copy-context (if prev-imp
			  (saved-context prev-imp)
			  *prelude-context*)
		      (module decl)
		      (reverse rem-decls)
		      (or (car rem-decls) decl))
	(let ((*current-context* (make-new-context (module decl))))
	  (setf (declaration *current-context*)
		(or (car prev-decls) decl))
	  (dolist (d (reverse prev-decls))
	    (typecase d
	      (mod-decl
	       (put-decl d (current-declarations-hash))
	       (let* ((thname (theory-name d))
		      (th (get-theory thname)))
		 (add-exporting-with-theories th thname)
		 (add-to-using thname))
	       (setf (saved-context d) (copy-context *current-context*)))
	      (importing
	       (let* ((thname (theory-name d))
		      (th (get-theory thname)))
		 (add-usings-to-context* th thname))
	       (setf (saved-context d) (copy-context *current-context*)))
	      (declaration (put-decl d (current-declarations-hash)))
	      (datatype nil)))
	  *current-context*))))
	      

(defmethod add-imported-assumings ((decl assuming-tcc))
  (add-usings-to-context (list (theory-instance decl))))

(defmethod add-imported-assumings (decl)
  (declare (ignore decl))
  nil)

(defmethod add-formal-importings-to-context (decl)
  (declare (ignore decl))
  nil)

(defmethod add-formal-importings-to-context ((decl tcc-decl))
  (let ((modinst (car (importing-instance decl)))
	(fml (cadr (importing-instance decl))))
    (when modinst
      (add-formal-importings-to-context*
       (formals (get-theory modinst)) modinst fml))))

(defun add-formal-importings-to-context* (formals modinst fml)
  (unless (eq fml (car formals))
    (when (typep (car formals) 'importing)
      (dolist (m (modules (car formals)))
	(add-to-using (subst-mod-params m modinst))))
    (add-formal-importings-to-context* (cdr formals) modinst fml)))

(defmethod add-immediate-importings-to-context (decl)
  (declare (ignore decl))
  nil)

(defmethod add-immediate-importings-to-context ((decl tcc-decl))
  (let ((modinst (car (importing-instance decl)))
	(gdecl (generated-by decl)))
    (when (and modinst (typep gdecl 'importing))
      (let ((rem (memq modinst (modules gdecl))))
	(assert rem)
	(add-usings-to-context (ldiff (modules gdecl) rem))))))

(defun remove-disallowed-decls-from-context (decl prevdecls)
  (if (and (tcc? decl)
	   (typep (generated-by decl) 'formal-decl))
      (let* ((adecls (memq (generated-by decl) prevdecls))
	     (fdecls (ldiff prevdecls adecls))
	     (pdecls (remove-if #'(lambda (d) (typep d 'formal-decl))
		       adecls))
	     (badass (find-if #'(lambda (d)
				 (and (typep d 'formula-decl)
				      (eq (spelling d) 'assumption)
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


(defun add-prelude-info-to-context (prelude-name)
  (let ((theory (car prelude-name)))
    (when (saved-context theory)
      (dolist (c (conversions (saved-context theory)))
	(pushnew (copy-tree c) (conversions *current-context*) :test #'equal))
      (setf (judgements *current-context*)
	    (copy-judgements (judgements (saved-context theory))))
      (setf (known-subtypes *current-context*)
	    (copy-known-subtypes (known-subtypes (saved-context theory)))))))

(defun make-new-context (theory)
  (let ((pctx (or *prelude-library-context*
		  *prelude-context*)))
    (if pctx
	(let ((*current-context*
	       (make-instance 'context
		 'theory theory
		 'theory-name (mk-modname (id theory))
		 'using-hash (copy-using-hash (using-hash pctx))
		 'declarations-hash (copy (declarations-hash pctx))
		 'known-subtypes (copy-tree (known-subtypes pctx))
		 'conversions (copy-list (conversions pctx)))))
	  (setf (judgements *current-context*)
		(copy-judgements (judgements pctx)))
	  *current-context*)
	(make-instance 'context
	  'theory theory
	  'theory-name (mk-modname (id theory))
	  'using-hash (make-hash-table)
	  'declarations-hash (make-hash-table)))))

(defun copy-using-hash (ht)
  (let* ((size (floor (hash-table-size ht) 1.5384616))
	 (new-ht (make-hash-table :test 'eq :size size)))
    (maphash #'(lambda (th thinsts)
		 (setf (gethash th (the hash-table new-ht))
		       (copy-list thinsts)))
	     (the hash-table ht))
    new-ht))

(defun copy-context (context &optional theory decls current-decl)
  (let ((*current-context*
	 (make-instance 'context
	   'theory (or theory (theory context))
	   'theory-name (if theory
			    (mk-modname (id theory))
			    (theory-name context))
	   'declaration (or (car (last decls))
			    current-decl
			    (declaration context))
	   'declarations-hash (let ((dhash (copy (declarations-hash context))))
				(dolist (decl decls)
				  (put-decl decl dhash))
				dhash)
	   'using-hash (copy (using-hash context))
	   'conversions (copy-list (conversions context))
	   'known-subtypes (copy-tree (known-subtypes context)))))
    (setf (judgements *current-context*)
	  (copy-judgements (judgements context)))
    *current-context*))

(defun copy-prover-context (&optional (context *current-context*))
  (assert *in-checker*)
  (assert (declaration context))
  (copy context))

(defmethod context (ignore)
  (declare (ignore ignore))
  (copy-context *prelude-context*))

(defun add-usings-to-context (modinsts)
  (when modinsts
    (add-usings-to-context* (get-theory (car modinsts)) (car modinsts))
    (add-usings-to-context (cdr modinsts))))

(defmethod add-usings-to-context* ((theory module) inst)
  (add-to-using inst)
  (add-exporting-with-theories theory inst))

(defmethod add-usings-to-context* ((adt datatype) inst)
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
	(hashentry (gethash res *create-formulas-cache*))
	(decl (declaration res)))
    (if hashentry hashentry
	(let ((formulas (create-formulas* res decl)))
	  (setf (gethash res *create-formulas-cache*) formulas)
	  formulas))))

(defun create-formulas* (res decl)	  
  (cond ((formula-decl? decl)
	 (unless (closed-definition decl)
	   (let ((*current-context* (context decl)))
	     (setf (closed-definition decl)
		   (universal-closure (definition decl)))))
	 (let ((*no-expected* t))
	   (list
	    (subst-mod-params (closed-definition decl)
			      (module-instance res)))))
	((typep decl '(or const-decl def-decl))
	 (copy-list (subst-mod-params (def-axiom decl)
				      (module-instance res))))
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
	     (varlist (mapcar #'(lambda (x)
				  (let ((name (mk-name-expr x)))
				    (setf (kind name) 'VARIABLE)
				    name))
				rhs-bindings))
	     (newbindings (append forall-vars rhs-bindings))
	     (*bound-variables* newbindings)
	     (new-lhs (typecheck (mk-application* (args1 equality) varlist)
			:expected (find-supertype (type (expression rhs)))))
	     (new-rhs (expression rhs))
	     (new-appl (make!-equation new-lhs new-rhs)))
	(close-freevars new-appl *current-context* newbindings nil nil))))

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
	 (name (mk-name-expr (id decl) nil nil res 'constant))
	 (appl (make!-equation name def))
	 (depth (lambda-depth decl)))
    (assert (eq (declaration name) decl))
    (loop for i from 0 to depth
	  do (push (create-definition-formula appl i)
		   (def-axiom decl)))))

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


(defmethod actuals ((n null)) NIL)

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
	       (var-to-binding freevars-form)
	     ;; CRW 7/27/94: fixed to do the substit before changing the
	     ;; bindings (when it was using the other order, substit
	     ;; was alpha-renaming the bindings to avoid capture)
	     (let ((nform
		    (if new?
			(freevar-substit form freevars-form newbindings)
			form)))
	       (copy nform
		 'bindings (append newbindings (bindings nform))))))
	  (t
	   (multiple-value-bind (newbindings new?)
	       (var-to-binding freevars-form)
	     (let* ((qform (make-instance (if exist?
					      'exists-expr
					      'forall-expr)
			     'bindings newbindings
			     'expression (if new?
					     (freevar-substit form
							      freevars-form
							      newbindings)
					     form)
			     'parens 1))
		    (tform (typecheck* qform (unless *no-expected* *boolean*)
				       'expr nil)))
	       ;;(assert (null (freevars tform)))
	       tform))))))

(defun freevar-substit (form freevars-form newbindings)
  (let ((*bound-variables* (append newbindings *bound-variables*)))
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

(defun var-to-binding (varlist)
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
			  (var-to-binding* (car vars))
			(vtb (cdr vars) (cons newbind result)
			     (or new? bnew?))))))
      (let ((nbindings (vtb varlist nil nil)))
	(values (if newvars?
		    (substit-bindings nbindings varlist)
		    nbindings)
		newvars?)))))

(defun substit-bindings (nbindings varlist &optional newbs)
  (if (null nbindings)
      (nreverse newbs)
      (substit-bindings (substit (cdr nbindings)
			  (acons (declaration (car varlist))
				 (car nbindings)
				 nil))
			(cdr varlist)
			(cons (car nbindings) newbs))))

(defun var-to-binding* (var)
  ;;(change-class (copy var) 'bind-decl)
  (if (and ;; nil
	   (bind-decl? (declaration var))
	   (tc-eq (type var) (type (declaration var))))
      (declaration var)
      (let ((bind-decl (mk-bind-decl (id var)
			 (get-declared-type var) (type var))))
	(values bind-decl t))))

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
  NIL)

;;;;;;;;;; ADT methods ;;;;;;;;;;;

;;; Given a type-name, determine whether it is an adt

(defmethod adt? (te)
  (declare (ignore te))
  nil)

(defmethod adt? ((te type-name))
  #+lucid (restore-adt te)
  (when (adt te)
    (change-class te 'adt-type-name)
    (adt te)))

(defmethod adt? ((te adt-type-name))
  #+lucid (restore-adt te)
  (adt te))

(defmethod adt? ((te datatype-subtype))
  (adt? (declared-type te)))

(defun restore-adt-slot (te)
  (setf (adt te) (get-adt-slot-value te)))

(defun get-adt-slot-value (te)
  (or (get-theory (id te))
      (find-if #'(lambda (d)
		   (and (typep d 'datatype)
			(eq (id d) (id te))))
	(all-decls (module (declaration te))))
      (let ((enumtype (find-if #'(lambda (d)
				   (and (typep d 'type-eq-decl)
					(eq (id d) (id te))
					(typep (type-expr d) 'datatype)))
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

(defmethod adt :around ((te type-name))
  (with-slots (adt) te
    (if (and adt (symbolp adt))
	(restore-adt-slot te)
	adt)))

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
	(setf (adt-type fn) adt))))

(defmethod adt ((fn accessor-name-expr))
  (or (adt-type fn)
      (let* ((dtype (domain (type fn)))
	     (adt (find-declared-adt-supertype
		   (if (typep dtype 'subtype)
		       (supertype dtype)
		       dtype))))
	(setf (adt-type fn) adt))))

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
      (subst-mod-params adt-sub (module-instance fn)))))

(defmethod recognizer ((fn name-expr))
  nil)

(defmethod recognizer ((fn constructor-name-expr))
  ;;(when (recognizer-name fn) (break))
  (or (recognizer-name fn)
      (let* ((con (car (member fn (constructors (adt (adt fn)))
			       :test #'same-id)))
	     (rd (rec-decl con))
	     (res (make-resolution rd (module-instance fn))))
	(setf (recognizer-name fn)
	      (mk-name-expr (id rd) nil nil res 'constant)))))

(defmethod accessors ((fn name-expr))
  (when (constructor? fn)
    (let* ((con (car (member fn (constructors (adt (adt fn)))
			     :test #'same-id))))
       (mapcar #'(lambda (acc)
		  (let ((res (make-resolution acc (module-instance fn))))
		    (mk-name-expr (id acc) nil nil res 'constant)))
	      (acc-decls con)))))

(defmethod accessors ((fn constructor-name-expr))
  (if (eq (accessor-names fn) 'unbound)
      (let* ((con (car (member fn (constructors (adt (adt fn)))
			       :test #'same-id))))
	(setf (accessor-names fn)
	      (mapcar #'(lambda (acc)
			  (let ((res (make-resolution acc
				       (module-instance fn))))
			    (mk-name-expr (id acc) nil nil res 'constant)))
		(acc-decls con))))
      (accessor-names fn)))

(defmethod constructor ((fn recognizer-name-expr))
  (or (constructor-name fn)
      (setf (constructor-name fn) (call-next-method))))

(defmethod constructor ((fn accessor-name-expr))
  (let* ((constrs (remove-if-not #'(lambda (c) (part-of-constructor fn c))
		     (constructors (adt (adt fn)))))
	 (decl (declaration fn))
	 (cons (remove-if-not #'(lambda (c) (memq decl (acc-decls c)))
		 constrs)))
    (mapcar #'(lambda (con)
		(let* ((cd (con-decl con))
		       (res (make-resolution cd (module-instance fn))))
		  (mk-name-expr (id cd) nil nil res 'constant)))
      cons)))

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
	 (mk-name-expr (id cd) nil nil res 'constant)))))

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
			      (make-resolution cd (module-instance tn))
			      'constant))
	    (mapcar #'con-decl (constructors (adt tn))))))

(defmethod constructors ((te subtype))
  (constructors (supertype te)))

(defmethod recognizers ((tn type-name))
  (when (adt? tn)
    (if (and (recognizer-names tn)
	     (fully-instantiated? (recognizer-names tn)))
	(recognizer-names tn)
	(setf (recognizer-names tn)
	      (subst-mod-params (mapcar #'recognizer (constructors tn))
				(module-instance tn))))))

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
		  (setf (kind fn) 'constant)
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
    (setf (kind fty) 'constant)
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
       (module-instance (resolution x))
       (or (null (current-theory))
	   (not (eq (id (module-instance (resolution x)))
		    (id (current-theory))))
	   (actuals (module-instance (resolution x))))
       (or (not *exclude-prelude-names*)
	   (not (and (from-prelude? (declaration x))
		     (or (null (actuals (module-instance (resolution x))))
			 (eq (id (module-instance (resolution x)))
			     '|equalities|)))))))

(defmethod full-name? ((x type-expr))
  (if (print-type x)
      (full-name? (print-type x))
      (call-next-method)))

(defmethod full-name? ((x type-name))
  (and (resolution x)
       (module-instance (resolution x))
       (or (null (current-theory))
	   (not (eq (id (module-instance (resolution x)))
		    (id (current-theory))))
	   (actuals (module-instance (resolution x))))))

(defmethod full-name! ((x name))
  (copy x
    'mod-id (when (or (null (current-theory))
		      (not (eq (id (module-instance (resolution x)))
			       (id (current-theory)))))
	      (id (module-instance (resolution x))))
    'actuals (full-name (actuals (module-instance (resolution x)))
			(when *full-name-depth*
			  (1- *full-name-depth*)))))

(defmethod full-name! ((te type-expr))
  (assert (print-type te))
  (lcopy te 'print-type (full-name! (print-type te))))

(defmethod full-name! ((x type-name))
  (let* ((mi (module-instance (resolution x)))
	 (modid (id mi)))
    (copy x
      'mod-id (when (and (or (not *exclude-prelude-names*)
			     (not (gethash modid *prelude*)))
			 (or (null (current-theory))
			     (not (eq modid (id (current-theory))))))
		modid)
      'actuals (full-name (actuals mi)
			  (when *full-name-depth*
			    (1- *full-name-depth*))))))



;;; Raise-actuals

(defvar *raise-actuals-of-actuals* nil)

(defun raise-actuals (obj &optional (actuals-also? t))
  (let ((*raise-actuals-of-actuals* actuals-also?)
	(*visible-only* t))
    (gensubst obj #'raise-actuals! #'raise-actuals?)))

(defmethod raise-actuals? (obj)
  (declare (ignore obj))
  nil)

(defmethod raise-actuals? ((x name))
  (unless (and (eq (id x) '=)
	       (module-instance x)
	       (eq (id (module-instance x)) '|equalities|))
    (raise-actuals-name? x)))

(defmethod raise-actuals? ((a actual))
  (not *raise-actuals-of-actuals*))

(defun raise-actuals-name? (x)
  (and (resolution x)
       (null (actuals x))
       (module-instance (resolution x))
       (actuals (module-instance (resolution x)))))

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
  (copy x 'actuals (actuals (module-instance (resolution x)))))

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
  (maphash #'(lambda (x y) (format t "~%~a - ~a" x y))
	   obj))

(defmethod ref-to-id ((ref symbol))
  ref)

(defmethod ref-to-id ((ref string))
  (intern ref))

(defmethod ref-to-id ((ref syntax))
  (if (slot-exists-p ref 'id)
      (id ref)
      (error "No id slot for <# ~a - ~a #>" (class-name (class-of ref)) ref)))

(defmethod ref-to-id ((ref subtype-judgement))
  (or (id ref)
      'subtype))

(defmethod ref-to-id ((ref number-judgement))
  (or (id ref)
      (makesym "~r" (number (number ref)))))

(defmethod ref-to-id ((ref name-judgement))
  (or (id ref)
      (id (name ref))))

(defmethod ref-to-id ((ref application-judgement))
  (or (id ref)
      (id (name ref))))


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
		  (thinst (module-instance (find-supertype (type expr))))
		  (cons-or-rec
		   (if (args sel)
		       (subst-mod-params (recognizer (constructor sel))
					 thinst)
		       (subst-mod-params (constructor sel)
					 thinst)))
		  (cond (if (args sel)
			    (make-application cons-or-rec expr)
			    (make-equation expr cons-or-rec)))
		  (then ;(subst-mod-params
			 (subst-accessors-in-selection expr sel)
			 ;thinst)
		    )
		  (else (translate-cases-to-if* expr (cdr selections)
						else-part t)))
	     (if chained?
		 (make-chained-if-expr cond then else)
		 (make-if-expr cond then else))))))

(defun subst-accessors-in-selection (expr sel)
  (let* ((thinst (module-instance (find-supertype (type expr))))
	 (accs (subst-mod-params (accessors (constructor sel)) thinst))
	 (vars (args sel))
	(selexpr (expression sel)))
    (substit selexpr
      (pairlis vars
	       (mapcar #'(lambda (acc) (make-application acc expr))
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
      (make-if-expr cond then else))))

(defun make-applications (expr args)
  (if (null args)
      expr
      (make-applications (make-application* expr (car args)) (cdr args))))

(defmethod translate-update-to-if* ((op update-expr) applargs)
  (translate-applied-update-to-if
   (expression op)
   applargs
   (nreverse (mapcar #'arguments (assignments op)))
   (nreverse (mapcar #'expression (assignments op)))
   nil nil))

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
	     (make-if-expr condition then-part else-part)))))

(defun translate-applied-update-leaf (op args exprs)
  (if args
      (make!-update-expr op
			 (mapcar #'(lambda (arg expr)
				     (mk-assignment nil arg expr))
			   args exprs))
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
	    (if chain?
		(make-chained-if-expr cond then else)
		(make-if-expr cond then else)))))))

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

(defmethod translate-update-to-if! ((expr update-expr))
  (with-slots (type expression assignments) expr
    (let ((*generate-tccs* 'none))
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
      (let ((nex (translate-update-to-if-funtype
		  type ex (car args-list) (car exprs))))
	(translate-update-to-if!* type nex (cdr args-list) (cdr exprs)))))

;;; This one recurses down the arguments of a single assignment
(defun translate-update-to-if-funtype (type ex args expr)
  (if args
      (let* ((bid (make-new-variable '|x| (cons ex (cons expr args))))
	     (bd (make-bind-decl bid (domain type)))
	     (bvar (make-variable-expr bd))
	     (car-arg (if (cdar args)
			  (make-tuple-expr (car args))
			  (caar args)))
	     (eqn (make-equation bvar car-arg))
	     (nex (make-update-if-application ex bvar)))
	(make-lambda-expr (list bd)
	  (make-update-function-if-expr eqn type nex (cdr args) expr)))
      (translate-update-to-if! expr)))

(defmethod make-update-if-application ((ex if-expr) arg)
  (let ((then (make-update-if-application (then-part ex) arg))
	(else (make-update-if-application (else-part ex) arg)))
    (if (tc-eq then else)
	then
	(make-if-expr (condition ex) then else))))

(defmethod make-update-if-application (ex arg)
  (beta-reduce (make-application ex arg)))
    
  
(defun make-update-function-if-expr (eqn type ex args expr)
  (make-if-expr
   eqn
   (if args
       (translate-update-to-if!* (range type) ex (list args) (list expr))
       expr)
   ex))

(defmethod translate-update-to-if!* ((type recordtype) ex args exprs)
  (make-record-expr
   (mapcar #'(lambda (fld)
	       (multiple-value-bind (fargs fexprs)
		   (matching-update-args-and-exprs fld args exprs)
		 (mk-assignment 'uni
		   (list (list (make-instance 'field-assignment-arg
				 'id (id fld))))
		   (if fargs
		       (translate-update-to-if!*
			(type fld)
			(make-update-field-application fld ex)
			(mapcar #'cdr fargs)
			fexprs)
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
		       (translate-update-to-if!*
			ty
			(make-projection-application cnt ex)
			(mapcar #'cdr fargs)
			fexprs)
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
		(setf (kind nexpr) 'variable)
		(setf (resolutions nexpr)
		      (list (make-resolution
				b (mk-modname (id (module decl))))))
		nexpr))
	  (or (apply #'append (formals decl))
	      (bindings (definition decl)))))


(defmethod find-supertype ((te subtype))
  (find-supertype (supertype te)))

(defmethod find-supertype ((te dep-binding))
  (find-supertype (type te)))

(defmethod find-supertype ((te type-expr))
  te)

(defmethod find-supertype ((te type-name))
  #+lucid (restore-adt te)
  (if (and (adt te)
	   (positive-types (adt te))
	   (not (every #'null (positive-types (adt te)))))
      (let* ((nmodinst (adt-modinst (module-instance te))))
	(if (tc-eq nmodinst (module-instance te))
	    te
	    (let* ((res (mk-resolution (declaration te) nmodinst nil))
		   (nte (copy te
			  'resolutions (list res)
			  'actuals (actuals nmodinst))))
	      (setf (type res) nte)
	      nte)))
      te))

;;; copy-all makes copies all the way down the object.  Because it uses
;;; gensubst, this function may only be used when the object has been
;;; typechecked, and *current-context* must be set.

(defun copy-all (obj)
  (let ((*copy-print-type* t)
	;;(*gensubst-cache* nil)
	(*parsing-or-unparsing* t))
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

(defvar *pseudo-normalizing* nil)

(defvar *pseudo-normalize-subtype-hash*
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))

(defun reset-pseudo-normalize-caches ()
  (if *pseudo-normalize-hash*
      (clrhash *pseudo-normalize-hash*)
      (setq *pseudo-normalize-hash*
	    (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq)))
  (if *pseudo-normalize-translate-id-hash*
      (clrhash *pseudo-normalize-translate-id-hash*)
      (setq *pseudo-normalize-translate-id-hash*
	    (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))))

(defun remove-pseudo-normalize-cache ()
  (setq *pseudo-normalize-hash* nil)
  (setq *pseudo-normalize-translate-id-hash* nil))

(defun pseudo-normalize (expr)
  (if (or *pseudo-normalizing*		; Don't allow recursion
	  (not (fully-instantiated? expr)))
      expr
      (let* ((fvars (freevars expr))
	     (nexpr (unless fvars
		      (gethash expr *pseudo-normalize-hash*))))
	(when (eq *assert-flag* 'simplify) 'break)
	(if nexpr
	    (if (tc-eq nexpr expr)
		expr
		nexpr)
	    (let* ((*pseudo-normalizing* t)
		   (*generate-tccs* 'none)
		   ;;(typealist primtypealist);;NSH(2.16.94)
		   (*assert-flag* 'simplify)
		   (*process-output* nil)
		   (*assert-if-arith-hash*
		    (if *assert-if-arith-hash*;;NSH(11.30.95) 
			*assert-if-arith-hash*;;not real shadowing
			(make-hash-table :test #'eq))))
	      (nprotecting-cong-state
	       ((*dp-state* *init-dp-state*)
		(*alists* *init-alists*))
	       (let ((result (if *translate-id-counter*
				 (assert-if-simplify expr)
				 (let* ((*translate-id-hash*
					 (clrhash
					  *pseudo-normalize-translate-id-hash*))
					(*translate-id-counter* nil)
					(typealist typealist))
				   (newcounter *translate-id-counter*)
				   (assert-if-simplify expr)))))
		 (when (and nil (not (tc-eq result expr)))
		   (break "pseudo-norm changed expr"))
		 (unless fvars
		   (setf (gethash expr *pseudo-normalize-hash*)
			 result))
		 result)))))))


(defmethod get-coercions ((res resolution))
  (let ((conversions (get-conversions (type res))))
    (mapcar #'(lambda (c)
		(lcopy c 'type (subst-mod-params (type c)
						 (module-instance res))))
	    conversions)))

(defmethod get-conversions ((name name-expr))
  (assert (resolution name))
  (get-conversions (resolution name)))


(defun find-conversions-for (atype etype)
  (find-conversions* (conversions *current-context*)
		     (mk-funtype atype etype)))

(defun find-conversions* (conversions ftype &optional result)
  (if (null conversions)
      result
      (append (get-conversions ftype) result)))

;;; Given a type, find the set of compatible conversion names.  These are
;;; in the order of preference.  Note that the conversion name has the
;;; conversion-decl as its declaration.

(defmethod get-conversions ((type type-expr))
  (compatible-conversions (conversions *current-context*) type))

(defmethod get-conversions ((type dep-binding))
  (get-conversions (type type)))

(defun compatible-conversions (conversions type &optional result)
  (if (null conversions)
      result
      (let ((cos (compatible-conversion (car conversions) type)))
	(compatible-conversions (cdr conversions) type
				(if cos
				    (cons cos result)
				    result)))))

(defun compatible-conversion (conversion type)
  (let* ((theory (module conversion))
	 (ctype (find-supertype (type conversion)))
	 (fmls (formals-sans-usings theory)))
    (if (and fmls
	     (not (eq theory (current-theory)))
	     (not (fully-instantiated? ctype)))
	(let ((bindings (tc-match type ctype (mapcar #'list fmls))))
	  (when (and bindings (every #'cdr bindings))
	    (let* ((acts (mapcar #'(lambda (a)
				     (mk-res-actual (cdr a) theory))
				 bindings))
		   (nmi (mk-modname (id theory) acts))
		   (*generate-tccs* 'none))
	      (and (with-no-type-errors
		    (let ((*current-context*
			   (if (free-params type)
			       (or (saved-context
				    (module (car (free-params type))))
				   *current-context*)
			       *current-context*)))
		      (subtypes-satisfied? acts fmls)))
		   (check-conversion
		    (subst-mod-params (name conversion) nmi))))))
	(when (compatible? ctype type)
	  (name conversion)))))

(defun subtypes-satisfied? (actuals formals &optional alist)
  (or (notany #'(lambda (fm) (typep fm 'formal-subtype-decl)) formals)
      (multiple-value-bind (nfml nalist)
	  (subst-actuals-in-next-formal (car actuals) (car formals) alist)
	(and (or (not (typep nfml 'formal-subtype-decl))
		 (subtype-of? (type-canon (type-value (car actuals)))
			      (type-canon (type-value nfml))))
	     (subtypes-satisfied? (cdr actuals) (cdr formals) nalist)))))

(defun check-conversion (name)
  (let ((type (find-supertype (type name))))
    (unless (strict-compatible? (domain type) (range type))
      (setf (types name) (list (type name)))
      name)))


;;; Given a type, find the set of compatible k-conversion names (properly
;;; instantiated).  This differs from get-conversions in that we are only using
;;; conversions that are k-combinators, and matching against the range rather
;;; than the domain of the conversion type.

(defmethod get-k-conversions ((type type-expr))
  (compatible-k-conversions (conversions *current-context*) type))

(defun compatible-k-conversions (conversions type &optional result)
  (if (null conversions)
      (nreverse result)
      (compatible-k-conversions
       (cdr conversions)
       type
       (let ((cos (when (k-combinator? (car conversions))
		    (compatible-k-conversion (car conversions) type))))
	 (if cos
	     (cons cos result)
	     result)))))

(defun compatible-k-conversion (conversion type)
  (let* ((ctype (range (find-supertype (type conversion))))
	 (fmls (formals-sans-usings (module conversion))))
    (if (and fmls
	     (not (fully-instantiated? ctype)))
	(let ((bindings (tc-match type ctype (mapcar #'list fmls))))
	  (when (and bindings (every #'cdr bindings))
	    (let ((nmi (mk-modname (id (module conversion))
			 (mapcar #'(lambda (a)
				     (mk-res-actual (cdr a) (module conversion)))
				 bindings))))
	      (subst-mod-params (name conversion) nmi))))
	(when (compatible? ctype type)
	  (name conversion)))))

#-gcl
(defun direct-superclasses (class)
  (slot-value class 'clos::direct-superclasses))

#+gcl
(defun direct-superclasses (class)
  (slot-value class 'pcl:class-direct-superclasses))

(defun types-of (obj)
  (let ((types nil))
    (labels ((tof (type)
	      (unless (or (memq type types)
			  (memq type '(t standard-object)))
		(push type types)
		(let ((class (find-class type nil)))
		  (when class
		    (mapc #'(lambda (s) (tof (class-name s)))
			  (direct-superclasses class)))))))
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

(defmethod untyped* ((act actual))
  (multiple-value-bind (typed? ex)
      (fully-typed? (or (type-value act)
			(expr act)))
    (values (not typed?) ex)))

(defmethod untyped* ((expr expr))
  (values (not (type expr))
	  expr))

(defmethod untyped* ((expr bind-decl))
  (values (not (type expr)) expr))

(defmethod untyped* ((expr name-expr))
  (values (not (and (type expr) (resolution expr)))
	  expr))

(defmethod untyped* ((expr field-assignment-arg))
  nil)

(defmethod untyped* ((te type-name))
  (values (not (resolution te))
	  te))

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

(defmethod operator* ((expr application))
  (operator* (operator expr)))

(defmethod operator* ((expr expr))
  expr)

(defmethod argument* ((expr application) &optional args)
  (argument* (operator expr) (cons (argument expr) args)))

(defmethod argument* ((expr expr) &optional args)
  args)

(defmethod arguments ((expr projection-application))
  (argument-list (argument expr)))

(defmethod arguments ((expr application))
  (argument-list (argument expr)))

(defmethod argument-list ((expr tuple-expr))
  (exprs expr))

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
#+(or allegro-v4.3 allegro-v5.0)
(defun pvs-gc-after-hook (global-p to-new to-old eff to-be-allocated)
  (declare (ignore eff to-new to-be-allocated))
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

#+allegro
(eval-when (load)
  (when (compiled-function-p #'pvs-gc-after-hook)
    (setf excl:*gc-after-hook* #'pvs-gc-after-hook)))

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

(defun remove-coercions (obj)
  (gensubst obj
    #'(lambda (ex) (argument ex))
    #'(lambda (ex) (typep ex 'coercion))))

(defun file-equal (file1 file2)
  (or (equal file1 file2)
      (multiple-value-bind (inode1 idev1)
	  (pvs-file-inode file1)
	(and inode1
	     (multiple-value-bind (inode2 idev2)
		 (pvs-file-inode file2)
	       (and (eql inode1 inode2)
		    (eql idev1 idev2)))))))


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
      (nth month '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul"
		 "Aug" "Sep" "Oct" "Nov" "Dec"))
      date hour min sec year)))

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
;  ;; (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
;  (subst-mod-params-cache
;   (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
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

(defvar *dependent-type-substitutions*
  (make-hash-table :test 'tc-eq :hash-function 'pvs-sxhash))

(defmethod dep-substit ((list list) alist)
  (let ((elt (cons list alist)))
    (or (gethash elt *dependent-type-substitutions*)
	(let ((nlist (substit list alist)))
	  (mapc #'(lambda (e ne) (cache-dep-substitutions e ne alist))
		list nlist)
	  (setf (gethash elt *dependent-type-substitutions*) nlist)))))

(defun cache-dep-substitutions (old new alist)
  (setf (gethash (cons old alist) *dependent-type-substitutions*) new))

(defmethod dep-substit (obj alist)
  (let ((elt (cons obj alist)))
    (or (gethash elt *dependent-type-substitutions*)
	(setf (gethash elt *dependent-type-substitutions*)
	      (substit obj alist)))))

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
