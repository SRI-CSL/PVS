;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs.lisp -- Main file for PVS handling PVS commands.
;; Author          : Sam Owre
;; Created On      : Wed Dec  1 15:00:38 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jan 28 16:58:59 1999
;; Update Count    : 93
;; Status          : Alpha test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;; This file provides the basic commands of PVS.  It provides the
;;; functions invoked by pvs-cmds.el, as well as the functions used in
;;; batch mode.

;;; Keeps track of the latest edit-proof information - has the form
;;; (DECL PLACE BUFFER PRELUDE-OFFSET)
;;;   - see edit-proof-at, install-proof, and prove-proof-at

(defvar *edit-proof-info* nil)

(defvar *justifications-changed?* nil)

;;; Invoked from Emacs

(defun pvs-init (&optional dont-load-patches)
  (setq excl:*enclose-printer-errors* nil)
  (setq *print-pretty* t)
  (setf (symbol-function 'ilisp::ilisp-restore) #'pvs-ilisp-restore)
  #+allegro (setq top-level::*print-length* nil
		  top-level::*print-level* nil)
  (unless dont-load-patches
    (load-pvs-patches))
  (pvs-init-globals))

(defun pvs-init-globals ()
  (setq *pvs-modules* (make-hash-table :test #'eq :size 20 :rehash-size 10))
  (setq *pvs-files* (make-hash-table :test #'equal))
  (setq *loaded-libraries* (make-hash-table :test #'equal))
  (setq *imported-libraries* (make-hash-table :test #'equal))
  (setq *prelude-libraries* (make-hash-table :test #'equal))
  (reset-typecheck-caches)
  (setq *current-theory* nil)
  (setq *last-proof* nil)
  (clrnumhash)
  (setq *pvs-context-writable* (write-permission?))
  (restore-context)
  (setq *pvs-initialized* t)
  (when *to-emacs*
    (pvs-emacs-eval "(setq *pvs-initialized* t)")))

(defun reset-typecheck-caches ()
  (dolist (fn *untypecheck-hook*)
    (funcall fn))
  (reset-subst-mod-params-cache)
  (reset-pseudo-normalize-caches)
  ;;(reset-fully-instantiated-cache)
  (reset-beta-cache) ;;; PDL added Nov23 1994
  (reset-type-canon-cache)
  (reset-store-object-caches)
  (reset-print-equal-cache)
  (setq *store-object-hash* (make-hash-table :test #'eq))
  (setq *current-theory* nil)
  (setq *last-proof* nil)
  (setq *subtype-names* nil)
  (setq *named-exprs* nil)
  (setq *edit-proof-info* nil)
  (setq *add-declaration-info* nil)
  (setq *mod-declaration-info* nil))

(defun remove-typecheck-caches ()
  (remove-subst-mod-params-cache)
  (remove-pseudo-normalize-cache)
  (remove-beta-cache)
  (remove-store-object-caches)
  (setq *store-object-hash* nil))

(defun clear-theories (&optional all?)
  (reset-typecheck-caches)
  (clrhash *pvs-modules*)
  (clrhash *pvs-files*)
  (when all?
    (clrhash *loaded-libraries*)
    (clrhash *prelude-libraries*)
    (clrhash *imported-libraries*)
    (setq *prelude-libraries-uselist* nil)))

(defvar *pvs-patches-loaded* nil)

(defvar *pvs-binary-type*
    #+(and allegro sparc) "fasl"	; Sun4
    #+(and allegro rios) "rfasl"	; PowerPC/RS6000
    #+(and allegro hpux) "hfasl"	; HP 9000
    #+(and allegro x86) "lfasl"         ; Intel x86
    #+(and lucid lcl4.1 sparc) "sbin"	; Sun4 new Lucid
    #+(and lucid (not lcl4.1) sparc) "obin" ; Sun4 old Lucid
    #+(and lucid rios) "rbin"		; PowerPC/RS6000
    #+(and lucid mips) "mbin"		; DEC
    ;;; These are experimental
    #+gcl "o"
    #+cmu "sparcf"
    #+harlequin-common-lisp "wfasl"
    )

(defun load-pvs-patches ()
  (dolist (pfile (collect-pvs-patch-files))
    (let* ((bfile (make-pathname :defaults pfile :type *pvs-binary-type*))
	   (compile? (and #+runtime nil
			  (probe-file pfile)
			  (or (not (probe-file bfile))
			      (compiled-file-older-than-source? pfile bfile)))))
      (multiple-value-bind (ignore error)
	  (ignore-file-errors
	   (cond (compile?
		  (pvs-message "Compiling patch file ~a"
		    (pathname-name pfile))
		  (multiple-value-bind (ignore condition)
		      (ignore-file-errors
		       (values (compile-file pfile)))
		    (declare (ignore ignore))
		    (cond (condition
			   (pvs-message "Compilation error - ~a" condition)
			   (pvs-message "Loading patch file ~a interpreted"
			     (shortname pfile))
			   (load pfile))
			  (t
			   (chmod "ug+w" (namestring bfile))
			   (pvs-message "Compilation complete: loading ~a"
			     (shortname bfile))
			   (load bfile)))))
		 (t (pvs-message "Loading compiled patch file ~a"
		      (shortname bfile))
		    (load bfile))))
	(declare (ignore ignore))
	(if error
	    (pvs-message "Error in loading ~a:~%  ~a"
	      (shortname pfile) error)
	    (pushnew (pathname-name pfile) *pvs-patches-loaded*
		     :test #'string=))))))

(defun collect-pvs-patch-files ()
  (case (ignore-errors (parse-integer
			(environment-variable "PVSPATCHLEVEL")))
    (0 nil)
    (1 (pvs-patch-files-for nil))
    (2 (append (pvs-patch-files-for nil)
	       (pvs-patch-files-for "test")))
    (3 (append (pvs-patch-files-for nil)
	       (pvs-patch-files-for "test")
	       (pvs-patch-files-for "exp")))))

(defun pvs-patch-files-for (ext)
  (let* ((defaults (pathname (format nil "~a/"
			      (environment-variable "PVSPATH"))))
	 (pfile (make-pathname
		 :defaults defaults
		 :name (format nil "patch~d~@[-~a~]~@[~a~]"
			 (major-version) ext (pvs-image-suffix))
		 :type "lisp"))
	 (bfile (make-pathname :defaults pfile :type *pvs-binary-type*)))
    (when (or (probe-file pfile)
	      (probe-file bfile))
      (list pfile))))

(defun get-pvs-version-information ()
  (list *pvs-version*
	(get-patch-version)
	(when (fboundp 'get-patch-test-version)
	  (get-patch-test-version))
	(when (fboundp 'get-patch-exp-version)
	  (get-patch-exp-version))
	(lisp-implementation-type)
	(lisp-implementation-version)))

(defun pvs-image-suffix ()
  (let* ((lisp (environment-variable "PVSLISP"))
	 (last-char (char lisp (1- (length lisp))))
	 (next-to-last-char (char lisp (- (length lisp) 2))))
    (when (and (alpha-char-p last-char)
	       (digit-char-p next-to-last-char))
      last-char)))

(defun pvs-patch< (p1 p2)
  (let ((name1 (pathname-name p1))
	(name2 (pathname-name p2)))
    (or (string= name1 "patch")
	(and (not (string= name2 "patch"))
	     (let* ((suff1 (subseq name1 5))
		    (suff2 (subseq name2 5))
		    (i1 (parse-integer suff1))
		    (i2 (parse-integer suff2)))
	       (cond ((= i1 i2)
		      (pvs-message "Ambiguous patch order - loading ~a before ~a"
			(if (string< suff1 suff2) name1 name2)
			(if (string< suff1 suff2) name2 name1))
		      (string< suff1 suff2))
		     (t (< i1 i2))))))))

(defun valid-pvs-patch-name (p)
  (let ((name (pathname-name p)))
    (or (string= name "patch")
	(and (string= name "patch" :end1 5)
	     (every #'digit-char-p (subseq name 5))))))


;;; Parsing

(defmethod parse-file ((filename string) &optional forced? no-message?)
  (let* ((file (make-specpath filename))
	 (*current-file* filename)
	 (theories (get-theories file)))
    (cond ((not (probe-file file))
	   (unless no-message?
	     (pvs-message "~a is not in the current context" filename)))
	  ((and (not forced?)
		(gethash filename *pvs-files*)
		(parsed-file? file))
	   (unless no-message?
	     (pvs-message "~a is already parsed" filename))
	   theories)
	  (*in-checker*
	   (pvs-message "Must exit the prover first"))
	  ((and (null theories)
		(not forced?)
		(valid-binfile? filename)
		(restore-theories filename))
	   (let ((theories (get-theories filename)))
	     (dolist (th theories)
	       (remove-associated-buffers (id th)))
	     (values theories t)))
	  ((adt-generated-file? filename)
	   (let* ((fe (get-context-file-entry filename))
		  (deps (if fe
			    (ce-dependencies fe)
			    (list (adt-generated-file? filename)))))
	     (when deps
	       (dolist (dep deps)
		 (typecheck-file dep forced? no-message?)))))
	  (t (let ((fe (get-context-file-entry filename)))
	       (when fe (setf (ce-object-date fe) nil)))
	     (let ((theories (parse-file* filename file theories forced?)))
	       (when (eq forced? 'all)
		 (parse-importchain theories))
	       (dolist (th theories)
		 (remove-associated-buffers (id th)))
	       theories)))))

(defun adt-generated-file? (filename)
  (and (> (length filename) 4)
       (string= (subseq filename (- (length filename) 4)) "_adt")
       (let ((fstring (with-open-file (str (make-specpath filename))
			(read-line str)))
	     (alen (length *adt-generated-string*)))
	 (and (> (length fstring) alen)
	      (string= (subseq fstring 0 alen) *adt-generated-string*)
	      (subseq fstring alen)))))

(defvar *parsed-theories* nil)

(defun parse-importchain (theories)
  (let ((*parsed-theories* nil))
    (parse-importchain* theories)))

(defun parse-importchain* (theories)
  (when theories
    (dolist (thname (get-immediate-usings (car theories)))
      (unless (library thname)
	(let ((th (get-theory thname)))
	  (cond ((null th)
		 (let* ((filename (context-file-of thname))
			(nth (if (and filename
				      (probe-file (make-specpath filename)))
				 (parse-file filename t nil)
				 (parse-file thname t nil))))
		   (setq *parsed-theories* (append nth *parsed-theories*))
		   (parse-importchain* nth)))
		((not (memq th *parsed-theories*))
		 (let ((nth (parse-file (filename th) t nil)))
		   (setq *parsed-theories* (append nth *parsed-theories*))
		   (parse-importchain* nth)))))))))
		     
  

(defun valid-binfile? (filename)
  (let ((fe (get-context-file-entry filename)))
    (and fe
	 (ce-object-date fe)
	 (let ((specdate (file-write-date (make-specpath filename)))
	       (bindate (file-write-date (make-binpath filename))))
	   (and bindate
		specdate
		(= bindate (ce-object-date fe))
		(<= specdate bindate))))))

(defun parse-file* (filename file theories forced?)
  ;;(save-context)
  (pvs-message "Parsing ~a" filename)
  (multiple-value-bind (new-theories time)
      (let ((*no-obligations-allowed* t))
	(parse :file file))
    (check-for-theory-clashes new-theories filename)
    ;;(check-import-circularities new-theories filename)
    (update-parsed-file filename file theories new-theories forced?)
    (pvs-message "~a parsed in ~d seconds" filename time)
    #+pvsdebug (assert (every #'(lambda (nth) (get-theory (id nth)))
			      new-theories))
    (mapcar #'(lambda (nth) (get-theory (id nth))) new-theories)))

(defun check-for-theory-clashes (new-theories filename)
  (check-for-duplicate-theories new-theories)
  (check-for-prelude-theory-clashes new-theories)
  (check-for-context-theory-clashes new-theories filename))

(defun check-for-duplicate-theories (new-theories)
  (when (cdr new-theories)
    (let ((dup (car (member (car new-theories) (cdr new-theories)
			    :test #'same-id))))
      (when dup
	(parse-error dup
	  "Theory ~a was declared earlier in this file" (id dup)))
      (check-for-duplicate-theories (cdr new-theories)))))

(defun check-for-prelude-theory-clashes (new-theories)
  (when new-theories
    (let* ((theory (car new-theories))
	   (clash (gethash (id theory) *prelude*)))
      (when clash
	(parse-error theory
	  "~a is in use as a prelude ~a name and may not be redefined"
	  (if (datatype? clash) "datatype" "theory") (id clash)))
      (check-for-prelude-theory-clashes (cdr new-theories)))))

(defun check-for-context-theory-clashes (new-theories filename)
  (unless *generating-adt*
    (let ((clashes (collect-theory-clashes new-theories filename)))
      (when clashes
	;; clashes is an assoc list with (new-theory . oldfilename) entries
	(unless (pvs-yes-or-no-p
		 "~d theor~@P clash~:[~;es~] with those in other files - continue? "
		 (length clashes) (length clashes) (eql (length clashes) 1))
	  (parse-error (caar clashes)
	    "Theory ~a has been declared previously in file ~a.pvs"
	    (id (caar clashes)) (cdar clashes)))
	;; At some point, should spit out pvs-info here.
	(dolist (clfname (remove-duplicates (mapcar #'cdr clashes)
			   :test #'string=))
	  (dolist (clth (get-theories clfname))
	    (delete-theory clth))
	  (remhash clfname *pvs-files*)
	  (delete-file-from-context clfname))
	(reset-typecheck-caches)))))

(defun check-import-circularities (theories file)
  (let ((alltheories (all-importings theories)))
    (check-import-circularities* alltheories file)))

(defun check-import-circularities* (theories file)
  (or (null (cdr theories))
      (let ((theory
             (find-if #'(lambda (th)
                          (not (member (id th) theories
                                       :test #'import-circularity-test)))
	       theories)))
        (if theory
            (check-import-circularities* (remove theory theories) file)
            (let* ((imports (get-immediate-usings (car theories)))
                   (imp (find-if #'(lambda (i)
                                     (member i theories :test #'same-id))
			  imports)))
              (parse-error imp "Circular IMPORTs are not allowed."))))))

(defun import-circularity-test (x y)
  (member x (get-immediate-usings y)
	  :test #'(lambda (u v) (eq x (id v)))))


;;; All-importings (list) walks down the immediate-usings of the list of
;;; theories/datatypes, returning the transitive closure of the
;;; immediate-usings.  This is straight-forward, except in the case where
;;; a theory imports the generated theories of a datatype by importing the
;;; datatype.  Thus we want to substitute the generated theories in place
;;; of the datatype, but only if we are not getting the importings for the
;;; datatype itself.  Thus (all-importings "list") ==> NIL,
;;; (all-importings "list_adt") ==> (#<Theory list>), and (all-importings
;;; "foo") contains list, list_adt, list_adt_map, list_adt_reduce, and
;;; list if foo imports (an instance of) list.

(defmethod all-importings ((theory datatype-or-module))
  (delete theory (all-importings (list theory))))

(defmethod all-importings ((list list))
  (let ((*theories-visited* nil))
    (declare (special *theories-visited*))
    (mapc #'all-importings* list)
    (remove-if-not #'datatype-or-module?
      *theories-visited*)))

(defun all-importings* (theory)
  (declare (special *theories-visited*))
  (unless (or (null theory)
	      (memq theory *theories-visited*))
    (push theory *theories-visited*)
    (dolist (ith (get-immediate-usings theory))
      (all-importings* (get-theory ith)))))


(defun update-parsed-file (filename file theories new-theories forced?)
  (handle-deleted-theories filename new-theories)
  (when forced?
    (reset-typecheck-caches))
  (let ((mth (update-parsed-theories filename file theories new-theories
				     forced?)))
    (setf (gethash filename *pvs-files*)
	  (cons (file-write-date file) mth)))
  (update-context filename))

(defun update-parsed-theories (filename file oldtheories new-theories forced?
					&optional result)
  (if (null new-theories)
      (nreverse result)
      (let* ((nth (car new-theories))
	     (oth (find nth oldtheories :test #'same-id))
	     (kept? nil))
	(unless (or (null (get-context-file-entry filename))
		    (= (file-write-date file)
		       (ce-write-date (get-context-file-entry filename))))
	  (dolist (ce (pvs-context-entries))
	    (when (and (member filename (ce-dependencies ce) :test #'string=)
		       (not (gethash (ce-file ce) *pvs-files*)))
	      (setf (ce-write-date ce) 0)
	      (setf (ce-object-date ce) 0)
	      (setq *pvs-context-changed* t))
	    (dolist (te (ce-theories ce))
	      (when (and (memq (id nth) (te-dependencies te))
			 (not (get-theory (te-id te))))
		(dolist (fe (te-formula-info te))
		  (when (memq (fe-status fe)
			      '(proved-complete proved-incomplete))
		    (setf (fe-status fe) 'unchecked)
		    (setq *pvs-context-changed* t)))))))
	(setf (filename nth) filename)
	(setf (path nth) (make-specpath filename))
	(if (and oth (memq 'typechecked (status oth)))
	    (let ((diffs (or forced? (compare oth nth))))
	      (cond ((null diffs)
		     (setq kept? t)
		     (copy-lex oth nth))
		    (t (reset-typecheck-caches)
		       (unless (typep oth 'datatype)
			 (dolist (ty (nonempty-types oth))
			   (setf (nonempty? ty) nil)))
		       (reset-proof-statuses oth)
		       (when (typep oth 'datatype)
			 (let ((gen (make-specpath (id (adt-theory oth)))))
			   (when
			       #+allegro (excl::filesys-inode (namestring gen))
			       #-allegro (probe-file gen)
			     (ignore-errors
			       (chmod "a+w" (namestring gen)))
			     (ignore-file-errors
			      (delete-file (namestring gen))))))
		       (untypecheck-usedbys oth)
		       (setf (gethash (id nth) *pvs-modules*) nth))))
	    ;; Don't need to do anything here, since oth was never typechecked.
	    (setf (gethash (id nth) *pvs-modules*) nth))
	(update-parsed-theories filename file
				(remove oth oldtheories) (cdr new-theories)
				forced?
				(cons (if kept? oth nth) result)))))

(defun untypecheck-usedbys (theory)
  (dolist (tid (find-all-usedbys theory))
    (let ((th (get-theory tid)))
      (reset-proof-statuses th)
      (untypecheck-theory th))))

(defun reset-proof-statuses (theory)
  (let ((fdecls (remove-if-not #'proved? (provable-formulas theory))))
    (when fdecls
      (setq *pvs-context-changed* t)
      (dolist (fdecl fdecls)
	(setf (proof-status fdecl) 'unchecked)
	(update-context-proof-status fdecl)))))


;;; collect-theory-clashes takes a list of theories and their associated
;;; filename and checks to see whether any of the theories is associated
;;; with a different filename.  It returns an association list of those
;;; new theories that clash and the file containing the old theory.  It
;;; uses the pvs context to determine the clashes.

(defun collect-theory-clashes (new-theories filename &optional result)
  (if (null new-theories)
      (nreverse result)
      (let ((cfile (context-file-of (id (car new-theories)))))
	(collect-theory-clashes
	 (cdr new-theories)
	 filename
	 (if (or (null cfile)
		 (string= cfile filename))
	     result
	     (cons (cons (car new-theories) cfile) result))))))


(defun remove-associated-buffers (theoryid)
  (pvs-buffer (format nil "~a.tccs" theoryid) nil)
  (pvs-buffer (format nil "~a.ppe" theoryid) nil))


#-(or akcl cmu)
(defmethod parse-file ((filename pathname) &optional forced? no-message?)
  (parse-file (pathname-name filename) forced? no-message?))

#+(or akcl cmu)
(defmethod parse-file (filename &optional forced? no-message?)
  (assert (pathnamep pathname))
  (parse-file (pathname-name filename) forced? no-message?))

(defmethod parse-file ((filename symbol) &optional forced? no-message?)
  (parse-file (string filename) forced? no-message?))

(defmethod parse-file ((filename name) &optional forced? no-message?)
  (parse-file (string (id filename)) forced? no-message?))

(defmethod parse-file ((theory module) &optional forced? no-message?)
  (parse-file (id theory) forced? no-message?))

;(defun ps (theoryname &optional forced?)
;  (parse-file theoryname forced? no-message?))


;;; Typechecking files.

(defun typecheck-file (filename &optional forced? prove-tccs? importchain?
				nomsg?)
  (multiple-value-bind (theories restored?)
      (parse-file filename forced? t)
    (let ((*current-file* filename)
	  (*typechecking-module* nil)
	  (*tc-theories* *tc-theories*)
	  ;;(start-time (get-internal-real-time))
	  )
      (when theories
	(cond ((and (not forced?)
		    theories
		    (every #'(lambda (th)
			       (let ((*current-theory* th))
				 (typechecked? th)))
			   theories))
	       (unless (or nomsg? restored?)
		 (pvs-message
		     "~a ~:[is already typechecked~;is typechecked~]~a"
		   filename
		   restored?
		   (if (and prove-tccs? (not *in-checker*))
		       " - attempting proofs of TCCs" ""))))
	      (*in-checker*
	       (pvs-message "Must exit the prover first"))
	      (t (pvs-message "Typechecking ~a" filename)
		 (typecheck-theories filename theories)
		 ;;(assert (every #'typechecked? theories))
		 (update-context filename)))
	(when prove-tccs?
	  (if *in-checker*
	      (pvs-message
		  "Must exit the prover before running typecheck-prove")
	      (if importchain?
		  (prove-unproved-tccs
		   (delete-duplicates (mapcan #'collect-theory-usings theories)
				      :test #'eq)
		   t)
		  (prove-unproved-tccs theories))))
	theories))))

(defun typecheck-theories (filename theories)
  (dolist (theory (sort-theories theories))
    (let ((start-time (get-universal-time)))
      (mapc #'(lambda (u) (get-typechecked-theory u))
	    (get-immediate-usings theory))
      (unless (typechecked? theory)
	(typecheck theory)
	#+pvsdebug (assert (typechecked? theory))
	(restore-from-context filename theory)
;;	(when (and *prove-tccs* (module? theory))
;;	  (prove-unproved-tccs (list theory))
;;	  (setf (tccs-tried? theory) t))
	(setf (filename theory) filename)
	(setf (path theory) (make-specpath filename))
	(when (module? theory)
	  (setf (tcc-info theory)
		(list (car (tcc-info theory))
		      (length (remove-if-not #'(lambda (d)
						 (and (formula-decl? d)
						      (eq (kind d) 'TCC)
						      (proved? d)))
				(append (assuming theory) (theory theory))))
		      (caddr (tcc-info theory)))))
	(let* ((tot (car (tcc-info theory)))
	       (prv (cadr (tcc-info theory)))
	       (mat (caddr (tcc-info theory)))
	       (obl (- tot prv mat))
	       (time (- (get-universal-time) start-time)))
	  (if (zerop tot)
	      (pvs-message "~a typechecked in ~ds: No TCCs generated~
                            ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		(id theory) time (length (warnings theory))
		(length (info theory)))
	      (pvs-message
		  "~a typechecked in ~ds: ~d TCC~:p, ~
                   ~d proved, ~d subsumed, ~d unproved~
                   ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		(id theory) time tot prv mat obl
		(length (warnings theory)) (length (info theory)))))))
    (setq *current-theory* (car (last theories))))
  theories)

(defun prove-unproved-tccs (theories &optional importchain?)
  (read-strategies-files)
  (mapc #'prove-unproved-tccs* theories)
  (let ((tot (reduce #'+ (mapcar #'(lambda (th)
				     (or (car (tcc-info th)) 0))
				 theories)))
	(proved (reduce #'+ (mapcar #'(lambda (th)
					(or (cadr (tcc-info th)) 0))
				    theories)))
	(subsumed (reduce #'+ (mapcar #'(lambda (th)
					  (or (caddr (tcc-info th)) 0))
				      theories)))
	;;(trivial (reduce #'+ (mapcar #'(lambda (th)
	;;				 (or (cadddr (tcc-info th)) 0))
	;;			     theories)))
	)
    (if (zerop tot)
	(pvs-message "File ~a typechecked: No TCCs to prove~a"
	  (filename (car theories)) (if importchain? "on importchain" ""))
	(pvs-message
	    "File ~a typechecked:~a ~d TCCs, ~d proved, ~d subsumed, ~d unproved~
             ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
	  (filename (car theories))
	  (if importchain? " importchain has" "")
	  tot proved subsumed
	  (- tot proved subsumed)
	  (reduce #'+ (mapcar #'(lambda (th) (length (warnings th))) theories))
	  (reduce #'+ (mapcar #'(lambda (th) (length (info th))) theories))))))

(defun prove-unproved-tccs* (theory)
  (if (tccs-tried? theory)
      (progn
	(pvs-message "TCCs attempted earlier on ~a" (id theory))
	(update-tcc-info theory (collect-tccs theory)))
      (let ((tccs (collect-tccs theory))
	    (*justifications-changed?* nil))
	(unless (every #'proved? tccs)
	  (let ((*current-theory* theory))
	    (mapc #'(lambda (d)
		      (when (tcc? d)
			(let ((*current-context* (context d)))
			  (prove-tcc d))
			(when (proved? d)
			  (incf (cadr (tcc-info theory))))))
		  (append (assuming theory) (theory theory)))))
	(when *justifications-changed?*
	  (save-all-proofs *current-theory*))
	(setf (tccs-tried? theory) t)
	(update-tcc-info theory tccs))))

(defun collect-tccs (theory)
  (remove-if-not #'tcc?
    (append (formals theory)
	    (assuming theory)
	    (theory theory))))

(defun update-tcc-info (theory tccs)
  (setf (car (tcc-info theory)) (+ (length tccs) (caddr (tcc-info theory)))
	(cadr (tcc-info theory)) (length (remove-if-not #'proved? tccs))))

(defun proved? (fdecl)
  (memq (proof-status fdecl)
	'(proved proved-complete proved-incomplete)))

(defun unproved? (formula-decl)
  (not (memq (proof-status formula-decl)
	     '(proved proved-complete proved-incomplete))))

(defmethod tccs-tried? ((adt datatype))
  t)

(defun prove-tcc (decl)
  (unless (and (proved? decl)
	       (justification decl))
    (unless (justification decl)
      (setf (justification decl) (tcc-strategy decl))
      (setq *justifications-changed?* t))
    (let* ((start-time (get-universal-time))
	   (*proving-tcc* 'TCC)
	   (proof (rerun-prove decl))
	   (proof-time (- (get-universal-time) start-time)))
      (pvs-message
	  "~:[Unable to prove~;Proved~] ~:[~;TCC ~]~a in ~d seconds"
	(eq (status-flag proof) '!) (tcc? decl) (id decl) proof-time)
      ;; Must return non-NIL if proved, NIL otherwise.
      (if (eq (status-flag proof) '!)
	  (setf (proof-status decl) 'proved)
	  (setf (proof-status decl) 'unfinished))
      (eq (status-flag proof) '!))))

(defun tcc-prove (decl context)
  (let ((*suppress-printing* T)
	(*printproofstate* nil)
	(*proving-tcc* 'TCC));;'TCC to save the proof.
    (prove-decl decl
		:strategy `(then ,(tcc-strategy decl) (fail))
		:context context)))

(defmethod tcc-strategy ((decl tcc-decl))
  (list "" (list (class-name (class-of decl)))))

(defmethod tcc-strategy (decl)
  (declare (ignore decl))
  '(tcc))

(defun sort-theories (theories)
  (let ((usings (mapcar #'(lambda (th)
			    (cons th (mapcar #'id (get-immediate-usings th))))
			theories)))
    (sort-theories* usings)))

(defun sort-theories* (usings &optional sorted)
  (if (null usings)
      sorted
      (let ((th (find-if #'(lambda (th)
			     (not (some #'(lambda (u)
					    (memq (id (car th)) (cdr u)))
					usings)))
		  usings
		  :from-end t)))
	(if th
	    (sort-theories* (remove th usings) (cons (car th) sorted))
	    (let ((theories (mapcar #'(lambda (th) (id (car th))) usings)))
	      (type-error (caar usings)
		"Circularity found in importings of theor~@P:~%  ~{~a~^, ~}"
		(length theories) theories))))))

(defmethod tcc-info ((d datatype))
  '(0 0 0))

(defun tc (modname &optional forced?)
  (typecheck-file modname forced?))

(defun tcp (modname &optional forced?)
  (typecheck-file modname forced? t))


(defun grind-untried-of-pvs-file (filename)
  (dolist (theory (get-theories filename))
    (grind-untried-formulas theory nil))
  (status-proof-pvs-file filename))

(defun grind-untried-of-theory (theoryname)
  (grind-untried-formulas (get-theory theoryname) t))

(defun grind-untried-formulas (theory &optional (show? t))
  (dolist (fmla (provable-formulas theory))
    (unless (justification fmla)
      (pvs-message "Grinding ~a" (id fmla))
      (setf (justification fmla) '("" (grind) nil))
      (rerun-prove fmla)))
  (when show?
    (status-proof-theory theory)))


;;; Prettyprinting

(defun prettyprint-region (filename pos1 &optional (pos2 pos1))
  (parse-file filename nil t)
  (let ((start-reg (car pos1))
	(end-reg (car pos2))
	(*no-comments* nil))
    (dolist (theory (reverse (get-theories filename)))
      (let ((start-theory (line-begin (place theory)))
	    (end-theory (line-end (place theory))))
	(cond ((<= end-theory start-reg) nil) ;theory above the region
	      ((>= start-theory end-reg) nil) ;theory below the region
	      ((and (<= start-reg start-theory)
		    (<= end-theory end-reg)) ;theory contained in region
	       (prettyprint-theory (id theory) filename))
	      (t (prettyprint-decls theory pos1 pos2)))))))

(defun prettyprint-decls (theory pos1 pos2)
  (let ((*no-comments* nil))
    (mapc #'(lambda (d) (prettyprint-decl d theory))
	  (nreverse
	   (chained-decls-list
	    (remove-if #'(lambda (d)
			   (or (> (car pos1) (line-end (place d)))
			       (> (line-begin (place d)) (car pos2))))
	      (remove-if #'generated-by (theory theory))))))
    (mapc #'(lambda (d) (prettyprint-decl d theory))
	  (nreverse
	   (chained-decls-list
	    (remove-if #'(lambda (d)
			   (or (> (car pos1) (line-end (place d)))
			       (> (line-begin (place d)) (car pos2))))
	      (remove-if #'generated-by (assuming theory))))))))

(defun chained-decls-list (decls &optional ldecls decls-list)
  (if (null decls)
      (if ldecls
	  (nreverse (cons (nreverse ldecls) decls-list))
	  (nreverse decls-list))
      (if (or (null ldecls)
	      (and (chain? (car ldecls))
		   (compatible-chain? (car ldecls) (car decls))))
	  (chained-decls-list (cdr decls)
			      (cons (car decls) ldecls)
			      decls-list)
	  (chained-decls-list (cdr decls)
			      (list (car decls))
			      (cons (nreverse ldecls) decls-list)))))

(defun prettyprint-decl (d theory)
  (let* ((place (place (if (consp d) (car d) d)))
	 (indent (- *default-char-width* (col-begin place)))
	 (dstr (unpindent d indent :string t :comment? t))
         (dfinal (string-trim '(#\Space #\Tab #\Newline) dstr)))
    (pvs-modify-buffer (shortname (working-directory))
                       (filename theory)
                       place dfinal)))

(defun prettyprint-theory (theoryname filename)
  (let ((file (or filename
		  (pvs-file-of theoryname))))
    (when file
      (parse-file file nil t)))
  (let* ((theory (get-parsed?-theory theoryname))
	 (*no-comments* nil))
    (when theory
      (let ((string (unparse theory
		      :string t
		      :char-width *default-char-width*)))
	(pvs-modify-buffer (shortname (working-directory))
			   (filename theory)
			   (place theory)
			   (string-right-trim '(#\space #\tab #\newline)
					      string))))))

(defun prettyprint-pvs-file (filename)
  (let ((theories (parse-file filename nil t))
	(*no-comments* nil))
    (pvs-buffer (makesym "~a.pvs" filename)
      (format nil "~{~a~}"
	(mapcar #'(lambda (th)
		    (unparse th
		      :string t
		      :char-width *default-char-width*))
		theories))
      t)))

; (defmethod prettyprint (theory)
;   (let ((*no-comments* nil))
;     (prettyprint (get-parsed-theory theory))))

;(defun pp (theory)
;  (prettyprint theory))

;;; View Theory

(defun prettyprint-expanded (theoryref)
  (let ((*no-comments* nil)
	(*unparse-expanded* t))
    (pvs-buffer (format nil "~a.ppe" theoryref)
      (let* ((theory (get-typechecked-theory theoryref))
	     (thstring (unparse theory
			 :string t
			 :char-width *default-char-width*)))
	(unless (datatype? theory)
	  (setf (ppe-form theory) (parse :string thstring)))
	thstring)
      t t)))

(defun ppe (theory)
  (prettyprint-expanded theory))

(defun show-tccs (theoryref &optional unproved-only?)
  (let* ((theory (get-typechecked-theory theoryref))
	 (*no-comments* nil))
    (when theory
      (let* ((*comment-on-proof-status* t)
	     (tccs (remove-if-not #'(lambda (d)
				      (and (tcc? d)
					   (or (not unproved-only?)
					       (unproved? d))))
		     (append (assuming theory) (theory theory))))
	     (str (let ((*no-comments* t)
			(*unparse-expanded* t))
		    (string-trim '(#\Space #\Tab #\Newline)
				 (unparse tccs :string t))))
	     (buffer (when theory (format nil "~a.tccs" (id theory)))))
	(cond (tccs
	       (let ((*valid-id-check* nil))
		 (setf (tcc-form theory)
		       (parse :string str :nt 'theory-part)))
	       (pvs-buffer buffer str t t))
	      (t (pvs-message "Theory ~a has no TCCs" theoryref)))))))

(defun tcc? (decl)
  (and (formula-decl? decl)
       (eq (kind decl) 'tcc)))


(defun parsed-file? (filename)
  (let ((file (if (pathnamep filename)
		  filename
		  (make-specpath filename))))
    (eql (parsed-date file)
	 (file-write-date file))))

(defmethod parsed? ((mod datatype-or-module))
  (parsed?* mod))

(defmethod parsed? ((modref modname))
  (parsed?* (get-theory modref)))

(defmethod parsed?* ((mod datatype-or-module))
  (or (from-prelude? mod)
      (and (filename mod)
	   (eql (car (gethash (filename mod) *pvs-files*))
		(file-write-date (path mod))))))

(defmethod parsed?* ((mod library-theory))
  (let* ((impfiles (car (gethash (library mod) *imported-libraries*)))
	 (prefiles (car (gethash (library mod) *prelude-libraries*)))
	 (*pvs-files* (if (and (hash-table-p impfiles)
			       (gethash (filename mod) impfiles))
			  impfiles
			  prefiles)))
    (and (filename mod)
	 (parsed?* (make-pathname :defaults (library mod)
				  :name (filename mod)
				  :type "pvs")))))

(defmethod parsed?* ((mod library-datatype))
  (let ((*pvs-files* (nth-value 0 (get-imported-files-and-theories
				   (library mod)))))
    (and (filename mod)
	 (parsed?* (make-pathname :defaults (library mod)
				 :name (filename mod)
				 :type "pvs")))))

(defun get-imported-files-and-theories (lib)
  (get-library-files-and-theories lib *imported-libraries*))

(defun get-library-files-and-theories (lib libhash)
  (let ((files&theories (gethash lib libhash)))
    (if files&theories
	(values-list files&theories)
	(let ((fileshash (make-hash-table :test #'equal))
	      (theorieshash (make-hash-table
			     :test #'eq :size 20 :rehash-size 10)))
	  (setf (gethash lib libhash) (list fileshash theorieshash))
	  (values fileshash theorieshash)))))

#-gcl
(defmethod parsed?* ((path pathname))
  (eql (parsed-date path)
       (file-write-date path)))

#+gcl
(defmethod parsed?* (path)
  (assert (pathnamep path))
  (eql (parsed-date path)
       (file-write-date path)))

(defmethod parsed?* ((x null))
  nil)

(defun typechecked-file? (filename)
  (and (parsed-file? filename)
       (every #'(lambda (m)
		  (member 'typechecked (status m)))
	      (get-theories filename))))


;;; Must be a method, since the slot exists for declarations.

(defmethod typechecked? ((theory datatype-or-module))
  (or *in-checker*
      (let ((importings (all-importings (list theory))))
	(every #'(lambda (th)
		   (and (parsed? th)
			(memq 'typechecked (status th))))
	       importings))))

(defmethod typechecked? ((theory module))
  (let ((importings (all-importings (list theory))))
    (every #'(lambda (th)
	       (and (parsed? th)
		    (memq 'typechecked (status th))))
	   importings)))

(defmethod typechecked? (theoryref)
  (break "typechecked? being called with ~a" (type-of theoryref))
  (let ((theory (get-theory theoryref)))
    (and theory
	 (typechecked? theory))))

(defmethod typechecked? ((tname modname))
  (let ((theory (get-theory tname)))
    (and theory
	 (typechecked? theory))))

(defmethod typechecked? ((theory library-theory))
  t)

(defmethod typechecked? ((theory library-datatype))
  t)

(defun get-theories (filename)
  (let ((fn (if (pathnamep filename)
		(pathname-name filename)
		filename)))
    (or (cdr (gethash fn *pvs-files*))
	(and (equal fn "prelude")
	     *prelude-theories*))))

(defun tca (theory &optional forced?)
  (typecheckall theory forced?))

(defun typecheckall (theory &optional forced?)
  (typecheck-file theory forced? t))


;;; Proving - entrypoints:
;;;  Emacs               Lisp
;;;  -----               ----
;;;  typecheck-prove   - typecheck-file
;;;  prove             - prove-file-at
;;;  redo-proof        - prove-file-at
;;;  prove-theory      - prove-theory
;;;  prove-pvs-file    - prove-pvs-file
;;;  prove-importchain - prove-usingchain
;;;  prove-proofchain  - prove-proofchain
;;;  install-proof     - install-proof


;;; prove-file-at is called from Emacs for a prove command, in which the
;;; cursor should be on a formula in one of the allowable buffers, which
;;; include the PVS file, a tccs or ppe buffer, the prelude file itself,
;;; or a view-prelude-theory buffer.

(defun prove-file-at (name line rerun?
			   &optional origin buffer prelude-offset
			   background? display?)
  (let ((*to-emacs* background?))
    (if *in-checker*
	(pvs-message "Must exit the current proof first")
	(multiple-value-bind (fdecl place)
	    (formula-decl-to-prove name line origin)
	  (if (and rerun?
		   fdecl
		   (null (justification fdecl)))
	      (pvs-message "Formula ~a has no proof to rerun." (id fdecl))
	      (if fdecl
		  (let ((*current-theory* (module fdecl))
			(*current-system* (if (member origin '("tccs" "ppe"))
					      'pvs
					      (intern (string-upcase
						       origin))))
			(*start-proof-display* display?)
			(ojust (extract-justification-sexp
				(justification fdecl)))
			(*justifications-changed?* nil))
		    (read-strategies-files)
		    (let ((proof (cond (background?
					(pvs-prove-decl fdecl t))
				       (t (auto-save-proof-setup fdecl)
					  (prove fdecl
						 :strategy
						 (when rerun? '(rerun)))))))
		      (when (typep proof 'proofstate)
			(setq *last-proof* proof)))
		    (unless (or background?
				(null (proof-time fdecl)))
		      (setf (caddr (proof-time fdecl)) t))
		    ;; Save the proof if it is different.
		    (unless (or (equal origin "prelude")
				(from-prelude? fdecl))
		      (when (or *justifications-changed?*
				(not (equal ojust
					    (extract-justification-sexp
					     (justification fdecl)))))
			(save-all-proofs *current-theory*))
		      ;; If the proof status has changed, update the context.
		      (update-context-proof-status fdecl))
		    (remove-auto-save-proof-file)
		    (let ((*to-emacs* t))
		      (pvs-locate buffer fdecl
				  (if prelude-offset
				      (vector (- (line-begin place) prelude-offset)
					      (col-begin place)
					      (- (line-end place) prelude-offset)
					      (col-end place))
				      place))))))))))

(defun formula-decl-to-prove (name line origin)
  (if (and (member origin '("ppe" "tccs") :test #'string=)
	   (not (get-theory name)))
      (pvs-message "~a is not typechecked" name)
      (case (intern (string-upcase origin))
	(ppe (let* ((theories (ppe-form (get-theory name)))
		    (decl (get-decl-at line 'formula-decl theories)))
	       (values (find-if #'(lambda (d)
				    (and (formula-decl? d)
					 (eq (id d) (id decl))))
			 (all-decls (get-theory name)))
		       (place decl))))
	(tccs (let* ((theory (get-theory name))
		     (decls (tcc-form theory))
		     (decl (find-if #'(lambda (d)
					(>= (line-end (place d)) line))
				    decls)))
		(values (find-if #'(lambda (d) (and (eq (module d) theory)
						    (formula-decl? d)
						    (eq (id d) (id decl))))
			  (all-decls theory))
			(place decl))))
	(prelude (let* ((theory (get-theory name))
			(theories (if (and theory (generated-by theory))
				      (list theory)
				      (remove-if #'generated-by
					*prelude-theories*)))
			(decl (get-decl-at line 'formula-decl theories)))
		   (values decl (place decl))))
	(t (let* ((theories (typecheck-file name nil nil nil t))
		  (decl (get-decl-at line 'formula-decl theories)))
	     (values decl (when decl (place decl))))))))


;;; This function is invoked from Emacs by pvs-prove-formula.  It provides
;;; an answer as to whether the proof should be rerun.  It takes a file, a
;;; line number, an optional origin, and an optional flag indicating
;;; whther it should try to rerun.  It returns T if the proof should be
;;; rerun, NO if the proof should not be rerun, and NIL if the formula
;;; declaration could not be found.

(defun rerun-proof-at? (name line &optional origin rerun?)
  (let ((fdecl (formula-decl-to-prove name line origin)))
    (cond ((and fdecl rerun?)
	   (if (justification fdecl)
	       rerun?
	       (pvs-message "Formula has no associated proof")))
	  (fdecl
	   (if (justification fdecl)
	       (and (or (unproved? fdecl)
			(pvs-y-or-n-p-with-timeout
			 "Formula has already been proved: try again? "))
		    (if (pvs-y-or-n-p-with-timeout "Rerun Existing proof? ")
			t 'NO))
	       'NO))
	  (t (pvs-message "Not at a formula declaration")))))

(defun prove-formula (modname formname rerun?)
  (declare (ignore modname))
  (let* ((fid (intern formname))
	 (fdecl (find-if #'(lambda (d) (and (formula-decl? d)
					    (eq (id d) fid)))
		  (all-decls (current-theory))))
	 (strat (when rerun? '(rerun))))
    (cond (fdecl
	   (setq *current-theory* (slot-value fdecl 'module))
	   (prove formname :strategy strat))
	  (t (pvs-message "Formula ~a not found" formname)))))

(defun rerun-proof-of? (modname formname)
  (setq *current-theory* (get-theory modname))
  (let* ((fid (intern formname))
	 (fdecl (find-if #'(lambda (d) (and (formula-decl? d)
					    (eq (id d) fid)))
		  (all-decls (current-theory)))))
    (and fdecl
	 (justification fdecl)
	 (pvs-y-or-n-p "Rerun Existing proof? "))))
    

;;; Non-interactive Proving

(defun prove-theory (theoryname &optional retry? filename)
  (when filename
    (typecheck-file filename))
  (let ((theory (get-typechecked-theory theoryname)))
    (when theory
      (prove-theories theoryname (list theory) retry?)
      (status-proof-theory theoryname))))


(defun prove-pvs-file (file retry?)
  (let ((theories (typecheck-file file)))
    (prove-theories file theories retry?)
    (status-proof-pvs-file file)))

(defun prove-pvs-theories (theory-names retry?)
  (when theory-names
    (let ((theories (mapcar #'get-typechecked-theory theory-names)))
      (prove-theories "" theories retry?)
      (status-proof-theories theories))))

(defun prove-usingchain (theoryname retry? &optional exclude)
  (let ((theory (get-typechecked-theory theoryname)))
    (when theory
      (prove-theories theoryname
		      (remove-if #'(lambda (th)
				     (typep th '(or library-datatype
						    library-theory)))
			(collect-theory-usings theoryname exclude))
		      retry?))
    (status-proof-importchain theoryname)))


(defun prove-proofchain (filename line origin retry?)
  (multiple-value-bind (fdecl place)
      (formula-decl-to-prove filename line origin)
    (declare (ignore place))
    (cond ((null fdecl)
	   (pvs-message "Unable to find formula declaration"))
	  ((null (justification fdecl))
	   (pvs-message "~a has no proof" (id fdecl)))
	  (t (prove-proofchain-decl fdecl retry?)))))

(defun prove-proofchain-decl (fdecl retry?)
  (let ((decls-tried nil) (total 0) (proved 0) (time 0) (unfin 0))
    (read-strategies-files)
    (labels ((ppd (decl)
	       (unless (memq decl decls-tried)
		 (let ((*justifications-changed?* nil))
		   (push decl decls-tried)
		   (incf total)
		   (incf time (nth-value 1 (pvs-prove-decl decl retry?)))
		   (if (unproved? decl) (incf unfin) (incf proved))
		   (when *justifications-changed?*
		     (save-all-proofs (module decl))))
		 (when (justification decl)
		   (ppds (provable-formulas (get-proofchain decl))))))
	     (ppds (decls)
	       (when decls
		 (ppd (car decls))
		 (ppds (cdr decls)))))
      (ppd fdecl))
    (pvs-buffer "PVS Status"
      (with-output-to-string (*standard-output*)
	(mapc #'(lambda (decl)
		  (format t "~%    ~55,1,0,'.a~10a"
		    (id decl) (proof-status-string decl)))
	      decls-tried)
	(format t "~%    Totals: ~d formulas, ~d attempted, ~d succeeded."
	  total (+ proved unfin) proved))
      t)))

(defun pvs-prove-decl (decl retry?)
  (setq *current-theory* (module decl))
  (cond ((and (or (justification decl)
		  (eq (kind decl) 'tcc))
	      (or retry?
		  (unproved? decl)))
	 (let ((start-time (get-universal-time))
	       (*rerunning-proof* (format nil "Proving ~a.~a"
				    (id *current-theory*) (id decl))))
	   (setf (proof-status decl) 'unproved)
	   (cond ((justification decl)
		  (pvs-message "Rerunning proof of ~a" (id decl))
		  (let ((pstat (rerun-prove decl))
			(time (- (get-universal-time) start-time)))
		    (pvs-message "~a ~aproved in ~d seconds"
		      (id decl) (if (unproved? decl) "un" "")
		      (- (get-universal-time) start-time))
		    (when (eq (proof-status decl) 'unproved)
		      (setf (proof-status decl) 'unfinished))
		    (update-context-proof-status decl)
		    (values pstat time)))
		 (t (pvs-message "Proving ~a..." (id decl))
		    (values (prove-tcc decl)
			    (- (get-universal-time) start-time))))))
	((or (justification decl)
	     (not (unproved? decl)))
	 (pvs-message "~a is already proved" (id decl))
	 (values nil 0))
	(t (pvs-message "~a has no proof" (id decl))
	   (values nil 0))))


(defun get-proofchain (fdecl)
  (union (refers-to fdecl) (proof-refers-to fdecl)))

(defun prove-proofchain-decls (decls fdecl retry?)
  (let ((total 1) (proved (if (unproved? fdecl) 0 1)) (time 0))
    (mapc #'(lambda (d)
	      (incf total)
	      (cond ((and (justification d)
			  (or retry?
			      (unproved? d)))
		     (let ((start-time (get-universal-time))
			   (*rerunning-proof* (format nil "Proving ~a"
						      (id d))))
		       (setq *current-theory* (slot-value d 'module))
		       (setf (proof-status d) 'unfinished)
		       (rerun-prove d)
		       (unless (unproved? d) (incf proved))
		       (incf time (- (get-universal-time) start-time))
		       (pvs-message "~a ~aproved in ~d seconds"
				      (id d) (if (unproved? d) "un" "")
				      (- (get-universal-time) start-time))))
		    ((justification d)
		     (incf proved)
		     (pvs-message "~a is already proved" (id d)))
		    (t (pvs-message "~a has no proof" (id d)))))
	   decls)))


(defun prove-theories (name theories retry?)
  (let ((total 0) (proved 0) (time 0))
    (read-strategies-files)
    (dolist (theory theories)
      (let ((*justifications-changed?* nil))
	(dolist (d (provable-formulas theory))
	  (incf total)
	  (incf time (nth-value 1 (pvs-prove-decl d retry?)))
	  (unless (unproved? d) (incf proved)))
	(when *justifications-changed?*
	  (save-all-proofs *current-theory*))))
    (pvs-message "~a: ~d proofs attempted, ~d proved in ~d seconds"
		   name total proved time)))



;;; Editing proofs

;(defun pprint-proof-script (s list)
;  (pprint-logical-block (s list :prefix "(" :suffix ")")
;    (pprint-exit-if-list-exhausted)
;    (loop (write (pprint-pop) :stream s)
;	  (pprint-exit-if-list-exhausted)
;	  (write-char #\space s)
;	  (write (pprint-pop) :stream s)
;	  (pprint-newline :linear s))))

(defun edit-proof-at (filename line origin buffer prelude-offset full-label)
  (multiple-value-bind (fdecl place)
      (formula-decl-to-prove filename line origin)
    (when fdecl
      (setq *edit-proof-info* (list fdecl place buffer prelude-offset)))
    (cond ((and fdecl (justification fdecl))
	   ;;(setq *current-theory* (module fdecl))
	   (pvs-buffer "Proof"
	     (with-output-to-string (out)
	       ;;(format out ";;; Proof ~a for formula ~a.~a~%"
		 ;;(id (default-proof fdecl)) (id (module fdecl)) (id fdecl))
	       (write (editable-justification (justification fdecl)
					      nil nil (when full-label ""))
		      :stream out :pretty t :escape t
		      :level nil :length nil
		      :pprint-dispatch *proof-script-pprint-dispatch*))
	     'popto))
	  (fdecl
	   (pvs-buffer "Proof" " " 'popto)
	   (pvs-message "Formula ~a has no proof to edit"
	     (id fdecl)))
	  (t (pvs-message "Unable to find formula declaration")))))


(defun install-proof (tmpfilename name line origin buffer prelude-offset)
  ;; If the origin is supplied, simply install the proof.  Otherwise the
  ;; proof is being installed from the Proof buffer, and the declaration
  ;; is gotten from *edit-proof-info*, in this case ask before installing.
  (when (or origin
	    (if *edit-proof-info*
		(prog1 (pvs-y-or-n-p "Install proof on formula ~a? "
				     (id (car *edit-proof-info*)))
		  (pvs-message ""))
		(pvs-message "No proof is being edited.")))
    (let ((sexpr (ignore-errors (with-open-file (in tmpfilename) (read in)))))
      (unless (listp sexpr)
	(justification-error sexpr sexpr "Proof must be a list"))
      (multiple-value-bind (msg subexpr)
	  (check-edited-justification sexpr)
	(if subexpr
	    (justification-error subexpr sexpr msg)
	    (let ((just (revert-justification sexpr)))
	      (multiple-value-bind (fdecl place)
		  (if origin
		      (formula-decl-to-prove name line origin)
		      (car *edit-proof-info*))
		(when (and origin fdecl)
		  (setq *edit-proof-info*
			(list fdecl place buffer prelude-offset)))
		(cond ((null fdecl)
		       (pvs-message "Proof is not associated with a formula"))
		      ((equal (extract-justification-sexp (justification fdecl))
			      just)
		       (pvs-message "Proof was not changed")
		       t)
		      (t (setf (justification2 fdecl) (justification fdecl))
			 (setf (justification fdecl) just)
			 (setf (proof-status fdecl) 'unfinished)
			 (unless (from-prelude? (module fdecl))
			   (save-all-proofs (module fdecl)))
			 (pvs-message "Proof installed on ~a" (id fdecl))
			 t)))))))))


;;; Called from install-proof by Emacs while in an edit proof buffer.  The
;;; *edit-proof-info* variable must have previously been set by
;;; edit-proof-at.

(defun prove-proof-at (line step? display?)
  (declare (ignore line))
  (let* ((fdecl (car *edit-proof-info*))
	 (*current-theory* (module fdecl)))
    (read-strategies-files)
    (auto-save-proof-setup fdecl)
    (let ((*start-proof-display* display?))    
      (setq *last-proof*
	    (if step?
		(prove (id fdecl))
		(prove (id fdecl) :strategy '(rerun)))))
    ;; Save the proof.
    (unless (from-prelude? fdecl)
      (save-all-proofs *current-theory*)
      ;; If the proof status has changed, update the context.
      (update-context-proof-status fdecl))
    (when (typep fdecl 'tcc-decl)
      (update-tcc-info (module fdecl) (collect-tccs (module fdecl))))
    (remove-auto-save-proof-file)
    (when (proof-time fdecl)
      (setf (caddr (proof-time fdecl)) t))
    (let* ((*to-emacs* t)
	   (place (second *edit-proof-info*))
	   (buffer (third *edit-proof-info*))
	   (prelude-offset (fourth *edit-proof-info*)))
      (pvs-locate buffer fdecl
		  (if prelude-offset
		      (vector (- (line-begin place) prelude-offset)
			      (col-begin place)
			      (- (line-end place) prelude-offset)
			      (col-end place))
		      place)))))
		   

(defun remove-proof-at (name line origin)
  (let ((fdecl (formula-decl-to-prove name line origin)))
    (cond ((and fdecl (justification fdecl))
	   (let ((just (justification fdecl)))
	     (setf (justification fdecl) nil)
	     (setf (justification2 fdecl) just)
	     (when (tcc? fdecl)
	       (setf (tccs-tried? (module fdecl)) nil))
	     (setf (proof-status fdecl) nil)
	     (update-context-proof-status fdecl)
	     (save-all-proofs *current-theory*)
	     (pvs-message
		 "Proof removed from ~a, use M-x revert-proof to get it back"
	       (id fdecl))))
	  (fdecl
	   (pvs-message "Formula ~a has no proof to remove" (id fdecl)))
	  (t (pvs-message "Unable to find formula declaration")))))

(defun revert-proof-at (name line origin)
  (let ((fdecl (formula-decl-to-prove name line origin)))
    (cond ((and fdecl (justification2 fdecl))
	   (let ((just (justification fdecl)))
	     (setf (justification fdecl) (justification2 fdecl))
	     (setf (justification2 fdecl) just)
	     (setf (proof-status fdecl)
		   (if (postpone-occurs-in-justification?
			(justification fdecl))
		       'unfinished
		       'unchecked))
	     (update-context-proof-status fdecl)
	     (save-all-proofs *current-theory*)
	     (pvs-message "Proof reverted")))
	  ((and fdecl (justification fdecl))
	   (pvs-message "Formula ~a has no proof to revert to"
	     (id fdecl)))
	  (fdecl
	   (pvs-message "Formula ~a has no proof"
	     (id fdecl)))
	  (t (pvs-message "Unable to find formula declaration")))))  

(defun postpone-occurs-in-justification? (justification)
  (postpone-in? (editable-justification justification)))

(defun postpone-in? (list)
  (and (consp list)
       (or (member '(postpone) list :test #'equal)
	   (postpone-in? (car list))
	   (postpone-in? (cdr list)))))

(defun justification-error (subexpr sexpr msg)
  (let ((pos (or (ignore-errors (matching-position subexpr sexpr "" 0))
		 0))
	(*from-buffer* "Proof"))
    (pvs-error "Proof Syntax Error" (or msg "Proof syntax error")
	       "Proof" (pos-to-place pos sexpr))
    nil))

(defun pos-to-place (pos sexpr)
  (declare (ignore pos sexpr))
  (let ((row 0) (col 0))
;    (dotimes (i pos)
;      (cond ((char= (aref string i) #\newline)
;	     (incf row)
;	     (setq col 0))
;	    (t (incf col))))
    (list row col)))


(defun matching-position (subexpr expr string pos)
  (if (eq subexpr expr)
      pos
      (and (consp expr)
	   (let ((spos (1+ pos)))
	     (dolist (e expr)
	       (let ((mpos (matching-position subexpr e string spos)))
		 (when mpos (return mpos))
		 (multiple-value-bind (subex npos)
		     (read-from-string (subseq string spos))
		   (assert (equal subex e))
		   (setq spos (+ spos npos
				 (position-if-not
				  #'(lambda (ch)
				      (member ch '(#\space #\tab #\newline)))
				  (subseq string (+ npos spos))))))))))))
      

;;;---------------------------------------------
;;; Theory Commands

;;; Find Theory is done completely in Emacs

;;; New Theory

(defun new-theory (modname)
  ;;(save-some-modules)
  (let ((id (if (stringp modname) (intern modname) modname)))
    (if (gethash id *pvs-modules*)
	(progn ;(pvs-message "Theory already exists")
	       nil)
	(namestring (make-pathname :name modname :type "pvs"
				   :defaults (working-directory))))))


;;; Delete Theory

(defun delete-pvs-file (filename &optional delete-file?)
  (let ((theories (get-context-theory-names filename)))
    (when delete-file?
      (mapc #'copy-theory-proofs-to-orphan-file theories))
    (dolist (tid theories)
      (let ((theory (get-theory tid)))
	(when theory
	  (when (typechecked? theory)
	    (untypecheck-theory theory))
	  (remhash tid *pvs-modules*))))
    (remhash filename *pvs-files*)
    (delete-file-from-context filename))
  (when delete-file?
    (delete-file (make-specpath filename)))
  (if delete-file?
      (pvs-message "~a has been deleted" filename)
      (pvs-message "~a has been removed from the context" filename)))

(defun delete-theory (theoryref)
  (let ((theory (gethash (ref-to-id theoryref) *pvs-modules*)))
    (when theory
      (copy-theory-proofs-to-orphan-file theoryref)
      (when (typechecked? theory)
	(untypecheck-theory theory))
      (remhash (id theory) *pvs-modules*))))


;;; List Theories

(defun lt (&optional context)
  (list-theories context))

(defun list-theories (&optional context)
  (if (or (null context)
	  (equal (truename context) (truename (working-directory))))
      (let ((theories nil))
	(maphash #'(lambda (id mod)
		     (declare (ignore mod))
		     (push (string id) theories))
		 *pvs-modules*)
	(sort theories #'string<))
      (let ((path (make-pathname :defaults context
				 :name "context" :type "cxt")))
	(if (probe-file path)
	    (let ((ctx (with-open-file (in path) (read in))))
	      (if (and (consp ctx)
		       (equal (car ctx) *pvs-version*))
		  (mapcar #'(lambda (m) (string (car m))) (cdr ctx))
		  (if (pvs-y-or-n-p
		       "Context from an earlier version - list all theory files? ")
		      (mapcar #'pathname-name
			      (directory (make-pathname :defaults path
							:name :wild
							:type "pvs"))))))
	    (if (pvs-y-or-n-p "No context - list all theory files? ")
		(mapcar #'pathname-name (directory path)))))))


;;;---------------------------------------------
;;; Environment Commands

;;; Help

;;; Suspend Pvs - done in Emacs

;;; Exit Pvs

#+lucid
(unless (fboundp 'bye)
  (setf (symbol-function 'bye) (symbol-function 'quit)))

#+excl
(defun bye ()
  (excl:exit))

#+harlequin-common-lisp
(defun bye ()
  (system::bye))

(defun quit (&optional status)
  (declare (ignore status))
  (when (y-or-n-p "Do you really want to kill the PVS process? ")
    (bye)))

(defun exit-pvs ()
  (save-context)
  (bye))

;;; Pvs Version

(defun pvs-version ()
  (pvs-message "PVS Version ~a" *pvs-version*))

;;; Source

(defun batch (infile &optional outfile)
  (let ((*in-pvs-batch* t))
    (if (probe-file infile)
	(if outfile
	    (with-open-file (*standard-output* outfile :direction :output
					       :if-exists :append
					       :if-does-not-exist :create)
	      (with-open-file (in infile)
		(batch* in)))
	    (with-open-file (in infile)
	      (batch* in)))
	(pvs-message "Batch error: file ~a does not exist" infile))))

(defun batch* (in)
  (let ((cmdline (read-line in nil 'eof)))
    (unless (eq cmdline 'eof)
      (execute-cmd (parse-cmd cmdline))
      (batch* in))))

(defun execute-cmd (cmd)
  (format t "~&> ~(~a~) ~{~a ~}~%" (car cmd) (cdr cmd))
  (apply (car cmd) (cdr cmd)))

(defun parse-cmd (cmdline &optional (index 0) result)
  (multiple-value-bind (obj ind)
      (read-from-string cmdline nil 'eof? :start index)
    (if (eq obj 'eof?)
	(nreverse result)
	(parse-cmd cmdline ind (cons obj result)))))


;;; help-prover

(defun help-prover (&optional name)
  (let ((rule (if (stringp name) (intern (string-upcase name)) '*)))
    (pvs-buffer "Prover Help"
      (with-output-to-string (*standard-output*)
	(funcall (help-rule-fun rule) nil))
      'popto t)))

;;; Misc functions

;;; get-parsed-theory gets the parsed theory, but will not save the context
;;; (last argument to parse-file)

(defun get-parsed-theory (theoryref)
  (let ((mod (get-theory theoryref)))
    (cond ((and mod (gethash (id mod) *prelude*))
	   mod)
	  ((and mod (parsed? mod))
	   mod)
	  ((library theoryref)
	   (multiple-value-bind (lib msg)
	       (get-library-pathname (library theoryref))
	     (if lib
		 (load-imported-library lib theoryref)
		 (type-error theoryref
		   (or msg "Library ~a could not be found")
		   (library theoryref)))))
	  ((and mod (filename mod))
	   (parse-file (filename mod) nil t)
	   (get-theory theoryref))
	  (t (let ((filename (context-file-of theoryref)))
	       (if (and filename (probe-file (make-specpath filename)))
		   (parse-file filename nil nil)
		   (parse-file theoryref nil nil))
	       (let ((pmod (get-theory theoryref)))
		 (or pmod
		     (type-error theoryref
		       (format nil "Can't find file for theory ~a"
			 theoryref)))))))))


(defun get-parsed?-theory (theoryref)
  (let ((theory (get-theory theoryref)))
    (cond ((null theory)
	   (pvs-message "~a is unknown in this context." theoryref))
	  ((gethash (id theory) *prelude*)
	   theory)
	  ((parsed? theory)
	   theory)
	  (t (pvs-message "~a has not been parsed." theoryref)))))

(defun get-typechecked-theory (theoryref)
  (or (and (or *in-checker*
	       *generating-adt*)
	   (get-theory theoryref))
      (let* ((theory (get-parsed-theory theoryref)))
	(when theory
	  (unless (or *in-checker* (typechecked? theory))
	    (let ((*generating-adt* nil))
	      (typecheck-file (filename theory)))))
	theory)))

(defun parsed-date (filename)
  (car (gethash (pathname-name filename) *pvs-files*)))

(defun reset-parsed-date (filename)
  (let ((path (make-specpath filename)))
    (setf (car (gethash filename *pvs-files*))
	  (file-write-date path))))

(defun find-theory-at (file line)
  (let ((theories (get-theories file)))
    (when theories
      (find-theory-at* theories line))))

(defun find-theory-at* (theories line)
  (when theories
    (if (and (<= (starting-row (place (car theories))) line)
	     (<= line (ending-row (place (car theories)))))
	(car theories)
	(find-theory-at* (cdr theories) line))))

(defun get-decl-at (line class theories)
  (when theories
    (let* ((theory (car theories))
	   (decl (find-if #'(lambda (d)
			      (and (typep d class)
				   (place d)
				   (>= (line-end (place d)) line)))
			  (append (assuming theory)
				  (theory theory)))))
      (if decl
	  (values decl theory)
	  (get-decl-at line class (cdr theories))))))

;(defun get-decl-at (line class mod &optional visible-only?)
;  (find-if #'(lambda (d)
;	       (and (if (listp class)
;			(some@ #'(lambda (c) (typep d c)) class)
;			(typep d class))
;		    (or (not (generated-by d)) (not visible-only?))
;		    (>= (car (location d)) line)))
;	   (theory mod)))

(defun get-decls (ref)
  (let ((decls nil))
    (maphash #'(lambda (mid mod)
		 (declare (ignore mid))
		 (when (module? mod)
		   (setq decls
			 (append (remove-if-not
				     #'(lambda (d)
					 (and (declaration? d)
					      (eq (id d) (ref-to-id ref))))
				   (all-decls mod))
				 decls))))
	     *pvs-modules*)
    (maphash #'(lambda (mid mod)
		 (declare (ignore mid))
		 (when (module? mod)
		   (setq decls
			 (append (remove-if-not
				     #'(lambda (d)
					 (and (declaration? d)
					      (eq (id d) (ref-to-id ref))))
				   (all-decls mod))
				 decls))))
	     *prelude*)
    (delete-duplicates decls :test #'eq)))


;;; Returns a list of theories in the transitive closure of the usings
;;; of the specified theoryname.  The theory must be typechecked.

(defun collect-theory-usings (theoryname &optional exclude)
  (let ((theory (get-theory theoryname)))
    (if theory
	(if (typechecked? theory)
	    (let* ((excl-theories (mapcar #'get-theory exclude))
		   (*modules-visited* excl-theories))
	      (collect-theory-usings* theory)
	      (nreverse (remove-if #'(lambda (x) (memq x excl-theories))
			  *modules-visited*)))
	    (pvs-message "Theory ~a has not been typechecked" theoryname))
	(if (get-context-theory-entry theoryname)
	    (pvs-message "Theory ~a has not been parsed" theoryname)
	    (pvs-message "Theory ~a is not in the current context"
	      theoryname)))))

(defun collect-theory-usings* (theory)
  (unless (memq theory *modules-visited*)
    (let ((*current-theory* theory))
      (push theory *modules-visited*)
      (dolist (use (get-immediate-usings theory))
	(let ((th (get-theory use)))
	  (when th
	    (collect-theory-usings* th)))))))


;;; Returns the filenames in the transitive closure of the usings of the
;;; specified filename.  The filenames are strings without the directory
;;; or extension.

(defun collect-file-usings (filename)
  (let ((theories (get-theories filename)))
    (if theories
	(let ((*modules-visited* nil))
	  (mapc #'collect-theory-usings* theories)
	  (remove-duplicates
	   (mapcar #'filename (nreverse *modules-visited*))
	   :from-end t :test #'equal))
	(pvs-message "File ~a.pvs is not known in this context."
	  filename))))

(defun file-and-place (theoryname)
  (let ((theory (get-theory theoryname)))
    (when theory 
      (cons (shortname (make-specpath (filename theory)))
	    (when (parsed? theory) (place-list (place theory)))))))

(defun id-place (name)
  (let* ((row (starting-row (place name)))
	 (scol (starting-col (place name)))
	 (ecol (+ scol (length (string (id name))))))
    (vector row scol row ecol)))

(defun get-name-at (filename pos)
  (if (parsed-file? filename)
      (let ((theories (get-theories filename))
	    (name nil))
	(mapobject
	 #'(lambda (ex)
	     (or name
		 (and (syntax? ex)
		      (place ex)
		      (not (within-place pos (place ex))))
		 (when (and (name? ex) (place ex))
		   (setq name ex)
		   t)))
	 theories)
	(if (or (actuals name)
		(typep name 'binding))
	    (get-name-at* pos name)
	    name))
      (pvs-message "~a has not been parsed" filename)))

(defun get-name-at* (pos name)
  (cond ((actuals name)
	 (let ((place (place name)))
	   (if (within-place pos place)
	       name
	       (let ((nname nil))
		 (mapobject
		  #'(lambda (ex)
		      (or nname
			  (and (syntax? ex)
			       (place ex)
			       (not (within-place pos (place ex))))
			  (when (and (name? ex) (place ex))
			    (setq nname ex)
			    t)))
		  (actuals name))
		 (if nname
		     (get-name-at* pos nname)
		     name)))))
	((and (typep name 'binding)
	      (declared-type name)
	      (place (declared-type name))
	      (not (chain? name))
	      (within-place pos (place (declared-type name))))
	 (let ((nname nil))
	   (mapobject
	    #'(lambda (ex)
		(or nname
		    (and (syntax? ex)
			 (place ex)
			 (not (within-place pos (place ex))))
		    (when (and (name? ex) (place ex))
		      (setq nname ex)
		      t)))
	    (declared-type name))
	   (if nname
	       (get-name-at* pos nname)
	       name)))
	(t name)))

(defun within-place (pos place)
  (and (<= (starting-row place) (car pos) (ending-row place))
       (if (= (starting-row place) (ending-row place))
	   (<= (starting-col place) (cadr pos) (ending-col place))
	   t)))

(defun show-last-proof (&optional terse?)
  (if *last-proof*
      (pvs-buffer "Proof Display"
	(with-output-to-string (*standard-output*)
	  (let ((*prover-indent* *prover-indent*)
		(*report-mode* terse?)
		(*top-proofstate* *last-proof*)
		(ps (non-strat-subgoal-proofstate *last-proof*)))
	    (report-proof* ps)
	    (when (and (typep *last-proof* 'top-proofstate)
		       (eq (status-flag *last-proof*) '!))
	      (format t "~%Q.E.D."))))
	t t)
      (pvs-message "No proof has been run yet")))

(defun show-expanded-sequent (&optional all?)
  (if (and *in-checker* *ps*)
      (pvs-buffer "Expanded Sequent"
	(with-output-to-string (*standard-output*)
;	  (unless all?
;	    (format t "%%% Prelude names are not expanded~%")
;	    (format t "%%%    use C-u M-x show-expanded-sequent to see them~2%"))
	  (write (expanded-sequent all?)))
	t)
      (pvs-message "Not in prover")))

(defun expanded-sequent (&optional all?)
  (let ((*current-theory* (unless all? *current-theory*))
	(*parsing-or-unparsing* t))
    (copy *ps*
      'current-goal
      (copy (current-goal *ps*)
	's-forms (mapcar #'(lambda (sf)
			     (copy sf
			       'formula (full-name (formula sf)
						   nil (not all?))))
		   (s-forms (current-goal *ps*)))))))

(defun show-skolem-constants ()
  (if *in-checker*
      (let ((skoconsts (collect-skolem-constants)))
	(if skoconsts
	    (pvs-buffer "Proof Display"
	      (with-output-to-string (*standard-output*)
		(format t "~%Skolem-constant: type")
		(format t "~%---------------------")
		(dolist (sc skoconsts)
		  (format t "~%~a: ~a" (id sc) (type sc))))
	      t)
	    (pvs-message "No Skolem Constants on this branch of the proof")))
      (pvs-message "Not in the prover")))

(defun collect-skolem-constants ()
  (let ((skoconsts nil))
    (maphash #'(lambda (id decls)
		 (declare (ignore id))
		 (dolist (d decls)
		   (when (typep d 'skolem-const-decl)
		     (pushnew d skoconsts))))
	     (current-declarations-hash))
    (sort skoconsts #'string-lessp :key #'id)))

(defun get-patch-version ()
  (when (boundp '*patch-revision*)
    (let ((str (symbol-value '*patch-revision*)))
      (when (search "$Revision: " str)
	(subseq str 11 (- (length str) 2))))))

(defun get-patch-test-version ()
  (when (boundp '*patch-test-revision*)
    (let ((str (symbol-value '*patch-test-revision*)))
      (when (search "$Revision: " str)
	(subseq str 11 (- (length str) 2))))))

(defun get-patch-exp-version ()
  (when (boundp '*patch-exp-revision*)
    (let ((str (symbol-value '*patch-exp-revision*)))
      (when (search "$Revision: " str)
	(subseq str 11 (- (length str) 2))))))

(defun collect-strategy-names ()
  (let ((names nil))
    (maphash #'(lambda (n s)
		 (push (string-downcase (string n)) names))
	     *rulebase*)
    (maphash #'(lambda (n s)
		 (unless (defhelper-entry? s)
		   (push (string-downcase (string n)) names)))
	     *rules*)
    (maphash #'(lambda (n s)
		 (unless (or (defhelper-entry? s)
			     (defstep-entry? s))
		   (push (string-downcase (string n)) names)))
	     *steps*)
    (sort names #'string<)))

(defvar *typecheck-formula-decl* nil)

(defun typecheck-formula-decl (formula-decl &optional theory-name context)
  (unless (and *typecheck-formula-decl*
	       (equal formula-decl (car *typecheck-formula-decl*))
	       (equal theory-name (cadr *typecheck-formula-decl*))
	       (or (null theory-name)
		   (let ((th (get-theory theory-name)))
		     (and th
			  (or (from-prelude? th)
			      (= (parsed-date (filename th))
				 (caddr *typecheck-formula-decl*)))))))
    (let* ((*current-theory* (if theory-name
				 (get-typechecked-theory theory-name)
				 (car (last *prelude-theories*))))
	   (*current-context* (or context (copy (context *current-theory*))))
	   (*generate-tccs* 'none)
	   (*from-buffer* "Formula Decl"))
      (pvs-buffer *from-buffer* formula-decl nil t)
      (let ((fdecl (car (pc-parse formula-decl 'theory-elt))))
	(unless (typep fdecl 'formula-decl)
	  (type-error fdecl "Not a formula declaration"))
	(typecheck* fdecl nil nil nil)
	(setq *typecheck-formula-decl*
	      (list formula-decl
		    theory-name
		    (when (and theory-name
			       (not (from-prelude? *current-theory*)))
		      (parsed-date (filename *current-theory*)))
		    fdecl)))))
  (pvs-message "Formula typechecked")
  (fourth *typecheck-formula-decl*))

(defvar *prove-formula-proof* nil)

(defun prove-formula-decl (formula-decl &optional theory-name context)
  (let* ((*current-theory* (if theory-name
			       (get-typechecked-theory theory-name)
			       (car (last *prelude-theories*))))
	 (*current-context* (copy (context *current-theory*)))
	 (fdecl (typecheck-formula-decl formula-decl theory-name context)))
    (let ((*in-checker* t)
	  (*generate-tccs* 'all)
	  (*tccforms* nil))
      (typecheck (definition fdecl) :expected *boolean*)
      (when *tccforms*
	(setq fdecl
	      (typecheck-formula-decl
	       (unparse
		   (copy fdecl
		     'definition (mk-conjunction
				  (nconc (mapcar #'(lambda (tcc)
						     (tccinfo-formula tcc))
					   *tccforms*)
					 (list (definition fdecl)))))
		 :string t)
	       theory-name context))))
    (setq *to-emacs* nil)
    (setq *prove-formula-proof* nil)
    (unwind-protect
	(progn (prove-decl fdecl :context *current-context*)
	       (setq *prove-formula-proof*
		     (extract-justification-sexp (justification fdecl))))
      (pvs-emacs-eval "(pvs-ready)"))))

(defun get-prove-formula-proof ()
  *prove-formula-proof*)

(defun show-strategy (strat-name)
  (let* ((strat-id (intern (string-upcase strat-name)))
	 (strategy (or (gethash strat-id *rulebase*)
		       (gethash strat-id *steps*)
		       (gethash strat-id *rules*))))
    (if strategy
	(pvs-buffer "Strategy Display"
	  (with-output-to-string (out)
	    (show-strategy* strategy out))
	  t)
	(pvs-message "No such strategy: ~a" strat-id))))
    
(defmethod show-strategy* ((strat rule-entry) out)
  (format out "~(~a~) is a primitive rule:" (name strat))
  (format out "~2%Arguments: ~(~a~)" (append (required-args strat)
					     '(&optional)
					     (optional-args strat)))
  (format out "~2%Definition: A compiled lisp function")
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defmethod show-strategy* ((strat defrule-entry) out)
  (format out "~(~a~) is a ~:[strategy~;defined rule~]:"
    (name strat) (gethash (name strat) *rules*))
  (format out "~2%Arguments: ~(~a~)" (formals strat))
  (format out "~2%Definition: ~%")
  (write (defn strat) :stream out :pretty t :level nil :length nil)
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defmethod show-strategy* ((strat defstep-entry) out)
  (format out "~(~a~) is a ~:[strategy~;defined rule~]:"
    (name strat) (gethash (name strat) *rules*))
  (format out "~2%Arguments: ~(~a~)" (formals strat))
  (format out "~2%Definition: ~%")
  (write (defn strat) :stream out :pretty t :level nil :length nil)
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defun dump-sequents-to-file (ps)
  (let* ((decl (declaration ps))
	 (theory *current-theory*)
	 (file (format nil "~a-~a.sequents" (id theory) (id decl))))
    (if (eq (status-flag *top-proofstate*) '!)
	(when (probe-file file)
	  (delete-file file))
	(with-open-file (out file
			     :direction :output
			     :if-file-exists :supersede)
	  (format out "~{~%~a~}" (collect-all-remaining-subgoals ps))))))
