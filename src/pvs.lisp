;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs.lisp -- Main file for PVS handling PVS commands.
;;             Note that the API for the PVS image depends on the following
;;             environment variables:
;;             PVSPATH, PVS_LIBRARY_PATH, PVSPATCHLEVEL, PVSMINUSQ
;; Author          : Sam Owre
;; Created On      : Wed Dec  1 15:00:38 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri May 21 04:08:38 2004
;; Update Count    : 96
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

(export '(exit-pvs parse-file typecheck-file show-tccs clear-theories
	  formula-decl-to-prove prove-formula proved? get-proof-script
	  get-proof-status get-tccs prove-tccs write-pvs-version-file
	  get-pvs-version-information))

;;; This file provides the basic commands of PVS.  It provides the
;;; functions invoked by pvs-cmds.el, as well as the functions used in
;;; batch mode.

;;; Keeps track of the latest edit-proof information - has the form
;;; (DECL PLACE BUFFER PRELUDE-OFFSET)
;;;   - see edit-proof-at, install-proof, and prove-proof-at

(defvar *edit-proof-info* nil)

(defvar *justifications-changed?* nil)

(defvar *started-with-minus-q* nil)

(defvar *parsing-files*)

(defvar *theories-seen* nil)

(defstruct pvs-meta-info
  version
  environment
  patch-files
  strategy-files
  lisp-files
  libfiles)


;;; A robodoc header
;****f* interface/pvs-init
;*
;*  NAME
;*    pvs-init -- initialize the pvs session
;*      Used to be invoked from Emacs, it is now called form the
;*      lisp initialization code - see make-pvs.lisp and pvs.system (startup-pvs)
;*      It uses a number of environment variables - these are mostly
;*      set in the pvs shell script:
;*        PVSPATH - must be set to the PVS directory (where it was installed)
;*        PVSINEMACS - set if the pvs lisp image was invoked from emacs
;*        PVS_LIBRARY_PATH - 
;*        PVSMINUSQ - set if the shell script was given a -q
;*        PVSNONINTERACTIVE - set if -batch was given
;*        PVSTIMEOUT - set to the -timeout value
;*        PVSDEFAULTDP - set to -decision-procedures arg (shostak or ics)
;*        PVSFORCEDP - set to -force-decision-procedures arg
;*        PVSPATCHLEVEL - the patches level (0 - 3)
;*        PVSVERBOSE - the verbosity (0 - 3)
;*              
;*  SYNOPSIS
;*    pvs-init &optional dont-load-patches dont-load-user-lisp
;*  FUNCTION
;*    Initializes the pvs session, setting up various global variables,
;*    loading user lisp and patch files, and initializing the decision
;*    procedures.
;*  INPUTS
;*    dont-load-patches - indicates that the patches should not be loaded
;*    dont-load-user-lisp - indicates that the ~/.pvs.lisp file should not be
;*                          loaded
;*  RESULT
;*    N/A
;*  SEE ALSO
;*    pvs-emacs/pvs
;*
;******

(defun pvs-init (&optional dont-load-patches dont-load-user-lisp path)
  (start-load-watching)
  #+allegro (setq excl:*enclose-printer-errors* nil)
  ;; The uiop:*temporary-directory* can be set wrong on the Mac
  ;; Possible we want to use TMPDIR value regardless
  (let ((tmpdir (environment-variable "TMPDIR")))
    (when tmpdir
      (unless (and (directory-p tmpdir)
		   (write-permission? tmpdir))
	(pvs-message "environment variable TMPDIR is not a writable directory:~%  ~a"
	  tmpdir)
	(pvs-message "Please fix this and try again")
	(cl-user:bye 1))
      (unless (char= (char tmpdir (1- (length tmpdir))) #\/)
	(setq tmpdir (format nil "~a/" tmpdir)))
      (pvs-message "Setting tmp dir to value of environment variable TMPDIR:~%  ~a~%"
	tmpdir)
      (setq uiop:*temporary-directory* tmpdir)))
  (setq *print-pretty* t)
  ;;(setf (symbol-function 'ilisp::ilisp-restore) #'pvs-ilisp-restore)
  #+allegro (setq top-level::*print-length* nil
		  top-level::*print-level* nil)
  (setq *pvs-path*
	(or (let ((evpath (environment-variable "PVSPATH")))
	      (when evpath
		(if (uiop:directory-exists-p evpath)
		    (truename evpath)
		    (pvs-warning "The environment variable PVSPATH is set to ~a, ~
                                  which does not exist" evpath))))
	    (let* ((dirp (truename (car (uiop:raw-command-line-arguments))))
		   (dirs (pathname-directory dirp)))
	      ;; We were started as, e.g., bin/ix86_64-Linux/runtime/pvs-allegro
	      ;; assume the parent of bin is a valid PVS installed directory.
	      (when dirs
		(namestring (make-pathname :directory (butlast dirs 3)))))
	    (if (and path (uiop:directory-exists-p path))
		(truename path)
		(when path
		  (pvs-error "Init error"
		    (format nil "~a does not exist: cannot find a pvs-path" path))))))
   #+(or cmu sbcl)
  (let ((exepath (directory-namestring (car (uiop:raw-command-line-arguments)))))
    (pushnew exepath *pvs-directories*)
    (#+cmu ext:load-foreign #+sbcl sb-alien:load-shared-object
		      (format nil "~a/mu.~a" exepath
			      #+darwin "dylib"
			      #-darwin "so"))
    (#+cmu ext:load-foreign #+sbcl sb-alien:load-shared-object
		      (format nil "~a/ws1s.~a" exepath
			      #+darwin "dylib"
			      #-darwin "so"))
    ;; Have no idea what is going on here, but if you leave this out,
    ;; bdd-cmu gives a compile error.
    #+cmu (fmakunbound 'bdd_cofactor_neg_)
    #+cmu (lf "bdd-cmu") #+sbcl (lf "bdd-sbcl")
    #+cmu (lf "mu-cmu") #+sbcl (lf "mu-sbcl")
    (BDD_bdd_init)
    #+allegro
    (nlyices-init)
    #+cmu (lf "dfa-foreign-cmu") #+sbcl (lf "dfa-foreign-sbcl"))
  (setq *started-with-minus-q*
	(or dont-load-user-lisp
	    (let ((mq (environment-variable "PVSMINUSQ")))
	      (and mq (not (equalp mq ""))))))
  (setq *pvs-emacs-interface*
	(let ((ei (environment-variable "PVSINEMACS")))
	  (and ei (not (equal ei "")))))
  (setq *noninteractive*
	(let ((ni (environment-variable "PVSNONINTERACTIVE")))
	  (and ni (not (equal ni "")))))
  (setq *noninteractive-timeout*
	(let ((to (environment-variable "PVSTIMEOUT")))
	  (and to (ignore-errors (parse-integer to)))))
  (setq *pvs-verbose*
	(let ((str (environment-variable "PVSVERBOSE")))
	  (when str
	    (or (ignore-errors (parse-integer str)) 0))))
  (setq *force-dp*
	(let ((fd (environment-variable "PVSFORCEDP")))
	  (and fd (intern fd :pvs))))
  (let ((dp (environment-variable "PVSDEFAULTDP")))
    (when dp (set-decision-procedure (intern dp :pvs))))
  (setq *pvs-library-path* (get-pvs-library-path))
  (unless dont-load-patches
    (load-pvs-patches))
  (pvs-init-globals)
  (do-theories #'(lambda (th)
		   (setf (context-path th)
			 (shortpath (format nil "~a/lib" *pvs-path*))))
    *prelude*)
  (initialize-decision-procedures)
  ;;(initialize-prelude-attachments)
  ;;(register-manip-type *number_field* 'pvs-type-real)
  (unless *default-pathname-defaults*
    ;; Need to make sure this is set to something
    (setq *default-pathname-defaults* (shortpath (working-directory))))
  ;; Load files specified on the command line
  (let ((evalload (environment-variable "PVSEVALLOAD")))
    (when evalload
      (multiple-value-bind (ignore error)
	  (ignore-errors (eval (read-from-string evalload)))
	(declare (ignore ignore))
	(when error
	  (pvs-message "Error executing ~a:~% ~a" evalload error)))))
  ;; Fix ASDF absolute pathnames - see src/asdf-patch.lisp for details
  (let ((uname #+allegro (sys:user-name)
	       #+sbcl (tools:user-name)))
    (unless (string= uname "owre")
      (lf "asdf-patch")))
  ;; If port is set, start the XML-RPC server
  (let ((port (environment-variable "PVSPORT")))
    (when port
      (pvs-xml-rpc:pvs-server :port (parse-integer port))))
  #+allegro
  ;; Add a newline to the beginning of *prompt*.  The prompt actually has
  ;; ~&, which is supposed to add a newline unless it "knows" it's already
  ;; at the beginning of the line, but sometimes this fails.  Just replacing
  ;; this with the unconditional newline ~%.
  (when (string= (subseq top-level:*prompt* 0 2) "~&")
    (setf (char top-level:*prompt* 1) #\%)))

(defparameter *pvs-env-variables*
  '("PVSDEFAULTDP"
    "PVSEVALLOAD"
    "PVSFORCEDP"
    "PVSINEMACS"
    "PVS_LIBRARY_PATH"
    "PVSLISP"
    "PVSMINUSQ"
    "PVSNONINTERACTIVE"
    "PVSPATCHLEVEL"
    "PVSPATH"
    "PVSPORT"
    "PVSTIMEOUT"
    "PVSVERBOSE"
    #+(or cmu sbcl) "LD_LIBRARY_PATH"))

(defun collect-pvs-env-variables ()
  (mapcar #'(lambda (env) (cons env (environment-variable env)))
    *pvs-env-variables*))

(defun pvs-init-globals ()
  (initialize-workspaces)
  (reset-typecheck-caches)
  (clrnumhash)
  ;; Prover hash tables
  (setq *translate-to-prove-hash* (make-pvs-hash-table))
  (setq *translate-id-hash* (make-pvs-hash-table))
  (setq *create-formulas-cache* (make-pvs-hash-table))
  (setq *subst-fields-hash* (make-pvs-hash-table))
  (setq *pvs-initialized* t)
  (when *pvs-emacs-interface*
    (pvs-emacs-eval "(setq pvs-initialized t)")))

(defun reset-typecheck-caches ()
  (dolist (fn *untypecheck-hook*)
    (funcall fn))
  (when *subst-type-hash* (clrhash *subst-type-hash*))
  (clrhash *subtype-of-hash*)
  (reset-subst-mod-params-cache)
  (reset-pseudo-normalize-caches)
  ;;(clrhash *assert-if-arith-hash*)
  ;;(reset-fully-instantiated-cache)
  (reset-beta-cache) ;;; PDL added Nov23 1994
  (reset-type-canon-cache)
  (reset-store-object-caches)
  (reset-print-equal-cache)
  (setq *store-object-hash* (make-hash-table :test #'eq))
  (setq *last-proof* nil)
  (setq *subtype-names* nil)
  (setq *named-exprs* nil)
  (setq *edit-proof-info* nil)
  (setq *add-declaration-info* nil)
  (setq *mod-declaration-info* nil))

(defun remove-typecheck-caches ()
  ;;(remove-subst-mod-params-cache)
  (remove-pseudo-normalize-cache)
  (reset-beta-cache)
  (remove-store-object-caches)
  (setq *exprs-generating-actual-tccs* nil)
  (setq *store-object-hash* nil))

(defun clear-theories (&optional workspace
		       &key delete-binfiles dont-load-prelude-libraries)
  (clear-workspace workspace
		   :delete-binfiles delete-binfiles
		   :dont-load-prelude-libraries dont-load-prelude-libraries))

(defun clear-workspace (&optional workspace
			  &key delete-binfiles dont-load-prelude-libraries)
  "Clears the given workspace, or the current workspace if nil, and all
workspaces if t or 'all.  Roughly speaking, it's in the state of a workspace
at the start of a PVS session.

Clearing a workspace saves the .pvscontext file if needed, initializes the
workspace-session instance, removes binfiles if delete-binfiles is not nil,
loads .pvscontext, and any prelude library extensions in the .pvscontext
(see load-prelude-libraries), unless dont-load-prelude-libraries is not
nil."
  (reset-typecheck-caches)
  (setq *circular-file-dependencies* nil)
  (let ((workspaces (cond ((memq workspace '(all :all t))
			   *all-workspace-sessions*)
			  ((typep workspace '(or string pathname))
			   (list (get-workspace-session workspace)))
			  ((null workspace)
			   (list *workspace-session*)))))
    (dolist (ws workspaces)
      (clrhash (pvs-files ws))
      (clrhash (pvs-theories ws))
      (clrhash (all-subst-mod-params-caches ws))
      (when (and (not dont-load-prelude-libraries)
		 (listp (pvs-context-libraries))
		 (every #'stringp (pvs-context-libraries)))
	;; May need to make sure a later ws isn't a prelude-library
	(load-prelude-libraries (prelude-libs ws)))
      (when delete-binfiles
	(let ((bindir (format nil "~a/~a" (path ws) *pvsbin-string*)))
	  (dolist (bf (uiop:directory-files bindir "*.bin"))
	    (delete-file bf)))))
    t))

(defun intialize-workspace-session (ws)
  (with-workspace ws
    (clrhash (current-pvs-files))
    (clrhash (current-pvs-theories))))
    
  

(defun get-pvs-library-path ()
  (setq *pvs-library-path* nil)
  (let ((libs nil)
	(pathenv (environment-variable "PVS_LIBRARY_PATH"))
	(pvs-path-lib (namestring (merge-pathnames #p"lib/" *pvs-path*))))
    (when pathenv
      (let ((dirs (split-path pathenv)))
	(dolist (dir dirs)
	  (when (string= dir "")
	    (setq dir pvs-path-lib))
	  (if (uiop:directory-exists-p dir)
	      (pushnew (namestring (truename dir)) libs :test #'file-equal)
	      (pvs-message "Directory ~a in PVS_LIBRARY_PATH does not exist"
		dir)))))
    (let ((pvs-path-lib-entry (find pvs-path-lib libs :test #'file-equal)))
      (setq *pvs-library-path*
	    (if pvs-path-lib-entry
		(nreverse libs)
		(nreverse (cons pvs-path-lib libs)))))))


(defun library-path-to-id (abspath)
  (car (rassoc abspath (pvs-library-alist) :test #'file-equal)))

(defun lirary-id-to-path (lib-id)
  (cdr (assoc lib-id (pvs-library-alist))))

;;; Called by Emacs function pvs-add-library-path
(defun add-library-path (dir)
  (when (string= dir "")
    (setq dir (merge-pathnames #p"lib/" *pvs-path*)))
  (if (file-exists-p dir)
      (let ((tdir (truename dir)))
	(if (member tdir *pvs-library-path* :test #'file-equal)
	    (pvs-warning "~a is already in your pvs library path")
	    (push tdir *pvs-library-path*)))
      (pvs-warning "Directory ~a does not exist" dir)))

;; Splits the PVS_LIBRARY_PATH, which is a string of paths separated by colons
;; Note that empty colons return empty strings, i.e.,
;; (split-path ":bar::foo:") ==> ("" "bar" "" "foo" "")
(defun split-path (pathstr &optional (spos 0) paths)
  (let ((pos (position #\: pathstr :start spos)))
    (if pos
	(split-path pathstr (1+ pos)
		    (cons (subseq pathstr spos pos) paths))
	(if (= spos (length pathstr))
	    (nreverse (cons "" paths))
	    (if (= spos 0)
		(list pathstr)
		(nreverse (cons (subseq pathstr spos) paths)))))))

(defvar *pvs-patches-loaded* nil)

(defun load-pvs-patches ()
  (let ((*loading-files* :patches))
    (dolist (pfile (append (collect-pvs-patch-files)
			   (user-pvs-lisp-file)))
      (let* ((bfile (make-pathname :defaults pfile :type *pvs-binary-type*))
	     (compile? (and (file-exists-p pfile)
			    (or (not (file-exists-p bfile))
				(compiled-file-older-than-source?
				 pfile bfile))))
	     (compilation-error? nil)
	     (bfile-loaded? nil))
	(when (and (not compile?)
		   (file-exists-p bfile))
	  ;; Everything looks up-to-date, try loading
	  (multiple-value-bind (ignore error)
	      (ignore-errors (load bfile))
	    (declare (ignore ignore))
	    (cond (error
		   ;; Likely due to a different lisp version
		   (when (file-exists-p pfile)
		     (setq compile? t))
		   (pvs-message "Error in loading ~a:~%  ~a"
		     (shortname bfile) error))
		  (t (pushnew pfile *pvs-patches-loaded*
			      :test #'equalp)
		     (setq bfile-loaded? t)))))
	(when compile?
	  ;; Needs compilation - haven't tried loading yet, or the load failed.
	  (pvs-message "Attempting to compile patch file ~a"
	    (pathname-name pfile))
	  (multiple-value-bind (ignore condition)
	      (ignore-file-errors
	       (values (compile-file pfile)))
	    (declare (ignore ignore))
	    (cond (condition
		   ;; Could be many reasons - permissions, source errors
		   (pvs-message "Compilation error - ~a" condition)
		   (setq compilation-error? t)
		   ;; Note that this may fail as well
		   (ignore-errors (delete-file bfile)))
		  (t
		   ;; Change so that next person in the same group may compile
		   (chmod "ug+w" (namestring bfile))
		   (pvs-message "Compilation complete, generated file ~a"
		     (shortname bfile))))))
	(when (and (not bfile-loaded?)
		   compile?
		   (not compilation-error?)
		   (file-exists-p bfile))
	  ;; Here when we have not loaded the fasl, needed to compile, and
	  ;; didn't get a compilation error
	  (pvs-message "Attempting to load compiled patch file ~a"
	    (shortname bfile))
	  (multiple-value-bind (ignore error)
	      (ignore-file-errors (load bfile))
	    (declare (ignore ignore))
	    (cond (error
		   (pvs-message "Error in loading ~a:~%  ~a"
		     (shortname pfile) error))
		  (t (setq bfile-loaded? t)
		     (pushnew pfile *pvs-patches-loaded*
			      :test #'equalp)))))
	(unless (or bfile-loaded?
		    (not (file-exists-p pfile)))
	  ;; Haven't loaded the fasl, so we try to load the source
	  (pvs-message "Loading file ~a interpreted" (shortname pfile))
	  (multiple-value-bind (ignore error)
	      (ignore-file-errors (load pfile))
	    (declare (ignore ignore))
	    (if error
		(pvs-message "Error in loading ~a:~%  ~a"
		  (shortname pfile) error)
		(pushnew pfile *pvs-patches-loaded*
			 :test #'equalp))))))))

(defun collect-pvs-patch-files ()
  (let ((pl (ignore-errors (parse-integer
			    (environment-variable "PVSPATCHLEVEL")))))
    (unless (and (integerp pl) (zerop pl))
      (let ((pfiles nil))
	(dolist (dir (cons *pvs-path*
			   (append (reverse *pvs-library-path*)
				   (list (user-homedir-pathname)))))
	  (setq pfiles (append pfiles (find-pvs-patch-files dir))))
	pfiles))))

(defun find-pvs-patch-files (dir)
  (let ((pdir (format nil "~apvs-patches/" dir)))
    (when (directory-p pdir)
      ;; First get the files
      (let ((pfiles (remove-if (complement #'valid-patch-filename)
		      (directory (format nil "~a/*.lisp" pdir)))))
	(sort pfiles #'string< :key #'pathname-name)))))

(defun pvs-build-date ()
  (multiple-value-bind (sec min hour date mon year)
      (decode-universal-time *pvs-build-time*)
    (declare (ignore sec min hour))
    (format nil "~4,'0d~2,'0d~2,'0d" year mon date)))

(defun valid-patch-filename (pathname)
  (let ((fname (pathname-name pathname)))
    (and (>= (length fname) 14) ; patch-YYYYMMDD#, where # is any seq of chars
       (let ((date (subseq fname 6 14)))
	 (and (every #'digit-char-p date)
	      (string> date (pvs-build-date)))))))

(defun user-pvs-lisp-file ()
  (unless *started-with-minus-q*
    (let* ((homedir (truename (user-homedir-pathname)))
	   (home-lisp-file (make-pathname :defaults homedir
					  :name ".pvs" :type "lisp"))
	   (home-fasl-file (make-pathname :defaults homedir
					  :name ".pvs" :type *pvs-binary-type*)))
      (when (or (file-exists-p home-lisp-file)
		(file-exists-p home-fasl-file))
	(list home-lisp-file)))))

(defun pvs-patch-files-for (ext)
  (let* ((defaults (pathname (format nil "~a/"
			      (environment-variable "PVSPATH"))))
	 (pfile (make-pathname
		 :defaults defaults
		 :name (format nil "patch~d~@[-~a~]~@[~a~]"
			 (major-version) ext (pvs-image-suffix))
		 :type "lisp"))
	 (bfile (make-pathname :defaults pfile :type *pvs-binary-type*)))
    (when (or (file-exists-p pfile)
	      (file-exists-p bfile))
      (list pfile))))

(defun get-pvs-version-information ()
  (list (get-pvs-version)
	(get-patch-version)
	(when (fboundp 'get-patch-test-version)
	  (get-patch-test-version))
	(when (fboundp 'get-patch-exp-version)
	  (get-patch-exp-version))
	(lisp-implementation-type)
	(lisp-implementation-version)))

(defun get-pvs-version ()
  "Returns the major.minor.revision form (e.g., 7.0.1078)"
  (if (file-exists-p (format nil "~a/pvs-version.lisp" *pvs-path*))
      (with-open-file (vers (format nil "~a/pvs-version.lisp" *pvs-path*))
	(read vers))
      (t *pvs-version*)))

(defun write-pvs-version-file ()
  (when (file-exists-p (format nil "~a/.git" *pvs-path*))
    (let ((verstr (format nil "~a.~d" *pvs-version* (pvs-git-count-since))))
      (with-open-file (vers (format nil "~a/pvs-version.lisp" *pvs-path*)
			    :direction :output :if-exists :overwrite)
	(format vers ";; Generated by the PVS make process (see pvs.system)")
	(format vers "~%;; third number is the count of commits since tag ~a~%" *pvs-version*)
	(format vers "~s" verstr)))))

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

(defmethod parse-file ((fileref string) &optional forced? no-message? typecheck?)
  "Invokes the parser on the given file.  If it has been parsed before, it
is in (current-pvs-files) and its write-date should match that in the
pvs-context.  forced? t says to ignore this, and parse anyway.  no-message?
t means don't give normal progress messages, and typecheck? says whether to
use binfiles."
  (with-pvs-file (filename) fileref
    (let* ((*current-file* filename)
	   (file (make-specpath filename))
	   (theories (get-theories file)))
      (cond ((not (file-exists-p file))
	     (unless no-message?
	       (pvs-message "PVS file ~a is not in the current context" filename)))
	    ((and (not forced?)
		  (gethash filename (current-pvs-files))
		  (parsed-file? file))
	     (unless no-message?
	       (pvs-message "~a is already parsed" filename))
	     theories)
	    ((and *in-checker*
		  (not *tc-add-decl*))
	     (if (pvs-yes-or-no-p "A proof is running; quit it now?")
		 (throw 'quit nil)
		 (pvs-error "Parse error" "Must exit the prover first")))
	    ((and *in-evaluator*
		  (not *tc-add-decl*))
	     (pvs-error "Parse error" "Must exit the evaluator first"))
	    ((and typecheck?
		  (null theories)
		  (not forced?)
		  (check-binfiles filename)
		  ;;(valid-binfile? filename)
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
		   (if typecheck?
		       (typecheck-file dep forced? nil nil no-message?)
		       (parse-file dep forced? no-message? typecheck?))))))
	    (t (let ((fe (get-context-file-entry filename)))
		 (when fe
		   ;;(format t "~%parse-file: ~a setting ce-object-date to nil" fe)
		   (setf (ce-object-date fe) nil)))
	       (multiple-value-bind (theories changed-theories)
		   (parse-file* filename file theories forced?)
		 (when (eq forced? 'all)
		   (parse-importchain theories typecheck?))
		 (dolist (th theories)
		   (remove-associated-buffers (id th)))
		 (values theories nil changed-theories)))))))

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

(defun parse-importchain (theories typecheck?)
  (let ((*parsed-theories* nil))
    (parse-importchain* theories typecheck?)))

(defun parse-importchain* (theories typecheck?)
  (when theories
    (dolist (thname (get-immediate-usings (car theories)))
      (unless (library thname)
	(let ((th (get-theory thname)))
	  (cond ((null th)
		 (let* ((filename (context-file-of thname))
			(nth (if (and filename
				      (file-exists-p (make-specpath filename)))
				 (parse-file filename t nil typecheck?)
				 (parse-file thname t nil typecheck?))))
		   (setq *parsed-theories* (append nth *parsed-theories*))
		   (parse-importchain* nth typecheck?)))
		((not (memq th *parsed-theories*))
		 (let ((nth (parse-file (filename th) t nil typecheck?)))
		   (setq *parsed-theories* (append nth *parsed-theories*))
		   (parse-importchain* nth typecheck?)))))))))


;;; A filename satisfies valid-binfile? if it has a context entry, the
;;; write date matches, and for each theory of the file the bin file
;;; date matches the corresponding date in ce-object-date.
(defun valid-binfile? (filename)
  (let* ((ce (get-context-file-entry filename))
	 (bin-dates (when ce (ce-object-date ce))))
    (and ce
	 (let ((spec-date (file-write-date (make-specpath filename)))
	       (expected-spec-date (ce-write-date ce))
	       (th-entries (ce-theories ce)))
	   (and spec-date
		expected-spec-date
		(= spec-date expected-spec-date)
		(every #'(lambda (te)
			   (let ((bin-date (file-write-date
					    (make-binpath (te-id te))))
				 (expected-bin-date
				  (cdr (assq (te-id te) bin-dates))))
			     (and bin-date
				  expected-bin-date
				  (= bin-date expected-bin-date)
				  (<= spec-date bin-date))))
		       th-entries))))))

(defun parse-file* (filename file theories forced?)
  ;;(save-context)
  (pvs-message "Parsing ~a" filename)
  (when (boundp '*parsing-files*)
    (pushnew filename *parsing-files* :test #'string=))
  (multiple-value-bind (new-theories time)
      (let ((*no-obligations-allowed* t))
	(parse :file file))
    (check-for-theory-clashes new-theories filename)
    ;;(check-import-circularities new-theories)
    (let ((changed
	   (update-parsed-file filename file theories new-theories forced?)))
      (pvs-message "~a parsed in ~,2,-3f seconds" filename time)
      #+pvsdebug (assert (every #'(lambda (nth) (get-theory (id nth)))
				new-theories))
      (values (mapcar #'(lambda (nth) (get-theory (id nth))) new-theories)
	      changed))))

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
	  "~a is a prelude ~a and may not be redefined"
	  (id clash) (if (datatype? clash) "datatype" "theory")))
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
	  (remhash clfname (current-pvs-files))
	  (delete-file-from-workspace clfname))
	(reset-typecheck-caches)))))

(defun check-import-circularities (theories)
  (let ((*modules-visited* nil))
    (check-import-circularities* theories nil)))

(defun check-import-circularities* (theories chain)
  (unless (null theories)
    (cond ((memq (car theories) chain)
	   (let ((dchain (cons (car theories)
			       (ldiff chain (cdr (memq (car theories)
						       chain)))))
		 (*current-context*
		  (make-instance 'context
		    'theory (car theories))))
	     (type-error (car theories)
	       "Circularity found in importings of theor~@P:~
                ~%  ~{~a~^ -> ~}"
	       (length dchain)
	       (mapcar #'id (nreverse dchain)))))
	  (t (unless (or (lib-datatype-or-theory? (car theories))
			 (memq (car theories) *modules-visited*))
	       (push (car theories) *modules-visited*)
	       (let* ((imp-names (get-immediate-using-ids (car theories)))
		      (imp-theories (mapcan #'(lambda (id)
						(let ((th (get-theory id)))
						  (when th (list th))))
				      imp-names)))
		 (check-import-circularities* imp-theories
					      (cons (car theories) chain))))
	     (check-import-circularities* (cdr theories) chain)))))


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
  (assert (or (not *saving-theory*) (not (eq (all-imported-theories theory) 'unbound))))
  (if (eq (all-imported-theories theory) 'unbound)
      (let* ((*current-context* (or (and (not (lib-datatype-or-theory?
					       theory))
					 (saved-context theory))
				    *current-context*)))
	(multiple-value-bind (imp-theories imp-names)
	    (all-importings* theory)
;; 	  (assert (every #'(lambda (th) (listp (all-imported-theories th)))
;; 			 imp-theories))
	  #+pvsdebug
	  (maplist #'(lambda (theories)
		       (assert (not (some #'(lambda (th)
					      (let ((imps
						     (all-imported-theories
						      (car theories))))
						(when (listp imps)
						  (memq th imps))))
					  (cdr theories)))))
		   imp-theories)
	  #+pvsdebug (assert (= (length imp-theories) (length imp-names)))
	  (when (memq 'typechecked (status theory))
	    #+pvsdebug (assert (or (null (get-immediate-usings theory))
				   imp-theories))
	    (setf (all-imported-theories theory) imp-theories)
	    (setf (all-imported-names theory) imp-names))
	  (values imp-theories imp-names)))
      (values (all-imported-theories theory)
	      (all-imported-names theory))))

(defmethod all-importings ((theories list))
  (if (null theories)
      nil
      (multiple-value-bind (imp-theories imp-names)
	  (let ((*current-context* (or *current-context*
				       (saved-context (car theories)))))
	    (assert *current-context*)
	    (all-importings (car theories)))
	(all-importings-list (cdr theories)
			     (reverse imp-theories) (reverse imp-names)))))

(defun all-importings-list (theories imp-theories imp-names)
  (if (null theories)
      (values (reverse imp-theories) (reverse imp-names))
      (multiple-value-bind (i-theories i-names)
	  (let ((*current-context* (or *current-context*
				       (saved-context (car theories)))))
	    (assert *current-context*)
	    (all-importings (car theories)))
	#+pvsdebug (assert (= (length i-theories) (length i-names)))
	(multiple-value-bind (u-theories u-names)
	    (importings-union i-theories i-names imp-theories imp-names)
	  #+pvsdebug (assert (= (length u-theories) (length u-names)))
	  (all-importings-list (cdr theories) u-theories u-names)))))

(defun importings-union (iths1 inms1 iths2 inms2)
  (if (null iths1)
      (values iths2 inms2)
      (if (memq (car iths1) iths2)
	  (importings-union (cdr iths1) (cdr inms1) iths2 inms2)
	  (importings-union (cdr iths1) (cdr inms1)
			    (cons (car iths1) iths2)
			    (cons (car inms1) inms2)))))

(defun all-importings* (theory &optional imimps)
  (let* ((imp-theories nil)
	 (imp-names nil)
	 (imps (or imimps (get-immediate-usings theory)))
	 ;; Note that adt-generated theories are not importings
	 )
    #+pvsdebug (assert (or (null (get-immediate-usings theory)) imps))
    (dolist (ith imps)
      (assert (modname? ith))
      (let* ((gtheory (get-theory ith))
	     (lib (or (library ith)
		      (and (null gtheory)
			   (lib-datatype-or-theory? theory)
			   (get-library-id (context-path theory)))))
	     (itheory (or gtheory
			  (and lib (get-theory* (id ith) lib))))
	     (iname (lcopy ith 'library lib 'actuals nil 'mappings nil)))
	(when (and itheory
		   (generated-by itheory))
	  (setq iname (lcopy iname 'id (generated-by itheory)))
	  (setq itheory (get-theory* (generated-by itheory) lib)))
	(when itheory
	  (multiple-value-bind (i-theories i-names)
	      (all-importings itheory)
	    #+pvsdebug (assert (= (length i-theories) (length i-names)) ()
			       "Not equal 1")
	    #+pvsdebug (assert (= (length imp-theories) (length imp-names)) ()
		    "Not equal 2")
	    #+pvsdebug (assert (or (null (get-immediate-usings itheory))
				   i-theories))
	    (let ((pos (position itheory imp-theories :test #'eq)))
	      (when pos
		(let* ((iths (copy-list imp-theories))
		       (inms (copy-list imp-names))
		       (nm (nth pos inms)))
		  (setf imp-theories (delete itheory iths :test #'eq))
		  (setf imp-names (delete nm inms :test #'eq))))
	      #+pvsdebug (assert (= (length imp-theories) (length imp-names)) ()
				 "Not equal 3"))
	    (push itheory imp-theories)
	    (push iname imp-names)
	    (loop for th in (reverse i-theories)
		  as nm in (reverse i-names)
		  do (progn
		       #+pvsdebug (assert (or (not (lib-datatype-or-theory?
						    th))
					      (library nm)
					      lib))
		       (when (and (lib-datatype-or-theory? th)
				  (not (library nm)))
			 (setq nm
			       (lcopy nm
				 :library (get-library-id nm)
				 :actuals nil
				 :mappings nil)))
		       (let ((pos (position th imp-theories :test #'eq)))
			 (when pos
			   (let ((nnm (nth pos imp-names)))
			     (assert (same-id nnm th))
			     (setf imp-theories
				   (delete th imp-theories :test #'eq))
			     (setf imp-names
				   (delete nnm imp-names :test #'eq))))
			 (push th imp-theories)
			 (push nm imp-names))))
	    #+pvsdebug (assert (= (length imp-theories) (length imp-names)) ()
		    "Not equal 4")))))
    #+pvsdebug (assert (every #'datatype-or-module? imp-theories))
    #+pvsdebug (assert (every #'modname? imp-names))
    #+pvsdebug (assert (every #'same-id imp-theories imp-names))
    (values imp-theories imp-names)))

(defmethod all-importings-update-theories ((th module) theories lib)
  (if (generated-by th)
      (all-importings-update-theories (get-theory* (generated-by th) lib)
				      theories lib)
      (cons th (delete th theories :test #'eq))))

(defmethod all-importings-update-theories ((th recursive-type) theories lib)
  (declare (ignore lib))
  (dolist (ath (adt-generated-theories th))
    (setf theories (cons ath (delete ath theories :test #'eq))))
  (cons th (delete th theories :test #'eq)))

;; (defmethod all-importings-update-names ((th module) names lib)
;;   (if (generated-by th)
;;       (all-importings-update-names (get-theory* (generated-by th) lib)
;; 				   names lib)
;;       (cons th (delete th names :test #'impname-eq))))

;; (defmethod all-importings-update-names ((th recursive-type) names lib)
;;   (declare (ignore lib))
;;   (dolist (ath (adt-generated-theories th))
;;     (setf names (cons ath (delete ath names :test #'eq))))
;;   (cons th (delete th names :test #'eq)))

;; (defun impname-eq (imp1 imp2)
;;   (and (eq (id imp1) (id imp2))
;;        (eq (library imp1) (library imp2))))

;;; Like all-importings, but returns only the immediate importings
(defun immediate-importings (theory &optional lib)
  (declare (ignore lib))
  (assert (not *saving-theory*))
  (let* ((*current-context* (or (and (not (lib-datatype-or-theory?
					   theory))
				     (saved-context theory))
				*current-context*))
	 (imp-theories nil)
	 (imp-names nil)
	 (imps (get-immediate-usings theory))
	 ;; Note that adt-generated theories are not importings
	 )
    (dolist (ith imps)
      (let* ((ith-nolib (get-theory (id ith)))
	     (lib (unless ith-nolib
		    (or (library ith)
			(and (lib-datatype-or-theory? theory)
			     (get-library-id (context-path theory))))))
	     (itheory (or ith-nolib
			  (and lib
			       (get-theory* (id ith) lib))))
	     (iname (lcopy ith 'library lib 'actuals nil 'mappings nil)))
	(when (and itheory
		   (generated-by itheory))
	  (setq iname (lcopy iname 'id (generated-by itheory)))
	  (setq itheory (get-theory* (generated-by itheory) lib)))
	(when itheory
	  (push itheory imp-theories)
	  (push iname imp-names))))
      (values imp-theories imp-names)))

(defun update-parsed-file (filename file theories new-theories forced?)
  (handle-deleted-theories filename new-theories)
  (when forced?
    (reset-typecheck-caches))
  (multiple-value-bind (mth changed)
      (update-parsed-theories filename file theories new-theories forced?)
    (setf (gethash filename (current-pvs-files))
	  (cons (file-write-date file) mth))
    (update-context filename)
    changed))

(defun update-parsed-theories (filename file oldtheories new-theories forced?
					&optional result changed)
  (if (null new-theories)
      (values (nreverse result) (nreverse changed))
      (let* ((nth (car new-theories))
	     (oth (find nth oldtheories :test #'same-id))
	     (kept? nil)
	     (changed? nil))
	(invalidate-context-formula-proof-info filename file nth)
	(setf (filename nth) filename)
	(if (and oth (memq 'typechecked (status oth)))
	    (let* ((*current-context* (saved-context oth))
		   (diff (or forced? (compare oth nth))))
	      ;;; diff is nil, t, or (odecl . ndecl)
	      (cond ((null diff)
		     (setq kept? t)
		     (copy-lex oth nth))
		    ((and (consp diff)
			  (memq (car diff) (all-decls oth)))
		     ;; Copies lexical info from new to old, up to diff.
		     ;; This is info that can't change the meaning, like
		     ;; place info, and keywords FORMULA vs LEMMA
		     (copy-lex-upto diff oth nth)
		     (let ((replaced (append (generated (car diff))
					     (memq (car diff)
						   (all-decls-but-theory-formals oth)))))
		       ;; assuming-instances
		       (when (assuming-instances oth)
		       	 (let ((pdecls (memq (car diff)
					     (reverse (all-declarations oth)))))
		       	   (assert pdecls)
		       	   (if (cdr pdecls)
		       	       (let ((vis-instances (assuming-instances (cadr pdecls))))
		       		 (setf (assuming-instances oth) vis-instances))
		       	       (setf (assuming-instances oth) nil))))
		       (setf (all-declarations oth) nil)
		       (setf (saved-context oth) nil)
		       (setf (tccs-tried? oth) nil)
		       ;; tcc-comments
		       (dolist (d replaced)
			 (let ((dcmts (assq d (tcc-comments oth))))
			   (when dcmts
			     (setf (tcc-comments oth)
				   (delete dcmts (tcc-comments oth))))))
		       ;; info
		       (dolist (dinfo (info oth))
			 (when (memq (car dinfo) replaced)
			   (setf (info oth) (delete dinfo (info oth)))))
		       ;; warnings
		       (dolist (dwarn (warnings oth))
			 (when (memq (car dwarn) replaced)
			   (setf (warnings oth) (delete dwarn (warnings oth)))))
		       ;; conversion-messages
		       (dolist (dcnv (conversion-messages oth))
			 (when (memq (car dcnv) replaced)
			   (setf (conversion-messages oth)
				 (delete dcnv (conversion-messages oth)))))
		       ;; all-imported-theories
		       (setf (all-imported-theories oth) 'unbound)
		       ;; all-imported-names
		       (setf (all-imported-names oth) 'unbound)
		       ;; nonempty-types
		       (when (nonempty-types oth)
			 (setf (nonempty-types oth)
			       (remove-if #'(lambda (ty-decl)
					      (when (memq (cdr ty-decl) replaced)
						(setf (nonempty? (car ty-decl)) nil)
						t))
				 (nonempty-types oth))))
		       ;; all-usings
		       ;; immediate-usings
		       (setf (immediate-usings oth) 'unbound)
		       ;; instances-used
		       (when (instances-used oth) (break "instances-used"))
		       ;; Now apply the diffs - modifies the old theory,
		       ;; keeping everything above the
		       (cond ((memq (car diff) (formals oth))
			      (setf (formals oth)
				    (append (ldiff (formals oth)
						   (memq (car diff) (formals oth)))
					    (memq (cdr diff) (formals nth))))
			      ;; Careful here - some formals cause changes to assuming/theory
			      (dolist (d (generated (car diff)))
				(setf (formals oth) (delete d (formals oth))))
			      (setf (theory-formal-decls oth) (theory-formal-decls nth))
			      (setf (assuming oth) (assuming nth))
			      (setf (theory oth) (theory nth)))
			     ((memq (car diff) (theory-formal-decls oth))
			      (setf (theory-formal-decls oth)
				    (append (ldiff (theory-formal-decls oth)
						   (memq (car diff) (theory-formal-decls oth)))
					    (memq (cdr diff) (theory-formal-decls nth))))
			      ;; Careful here - some formals cause changes to assuming/theory
			      (dolist (d (generated (car diff)))
				(setf (theory-formal-decls oth)
				      (delete d (theory-formal-decls oth))))
			      (setf (assuming oth) (assuming nth))
			      (setf (theory oth) (theory nth)))
			     ((memq (car diff) (assuming oth))
			      (setf (assuming oth)
				    (append (ldiff (assuming oth)
						   (memq (car diff) (assuming oth)))
					    (memq (cdr diff) (assuming nth))))
			      (dolist (d (generated (car diff)))
				(setf (assuming oth) (delete d (assuming oth))))
			      (setf (theory oth) (theory nth)))
			     (t
			      (setf (theory oth)
				    (append (ldiff (theory oth)
						   (memq (car diff) (theory oth)))
					    (memq (cdr diff) (theory nth))))
			      (dolist (d (generated (car diff)))
				(setf (theory oth) (delete d (theory oth)))))))
		     ;; formals-sans-usings
		     (setf (formals-sans-usings oth)
			   (remove-if #'importing-param? (formals oth)))
		     (reset-typecheck-caches)
		     (when (recursive-type? oth)
		       (break "recursive-type"))
		     (untypecheck-usedbys oth)
		     (setf (status oth) '(parsed))
		     (setq changed? t)
		     (setq kept? t))
		    (t (reset-typecheck-caches)
		       (when (module? oth)
			 (dolist (ty-decl (nonempty-types oth))
 			   (setf (nonempty? (car ty-decl)) nil)))
		       (when (typep oth 'recursive-type)
			 (let ((gen (make-specpath (id (adt-theory oth)))))
			   (when
			       #+allegro (file-exists-p gen)
			       #-allegro (probe-file gen)
			     (ignore-errors
			       (chmod "a+w" (namestring gen)))
			     (ignore-file-errors
			      (delete-file (namestring gen))))))
		       (setf (gethash (id nth) (current-pvs-theories)) nth)
		       (untypecheck-usedbys oth)
		       (setq changed? t)))))
	;; Don't need to do anything here, since oth was never typechecked.
	(unless kept?
	  (setf (gethash (id nth) (current-pvs-theories)) nth))
	(assert (gethash (id nth) (current-pvs-theories)))
	(update-parsed-theories filename file
				(remove oth oldtheories) (cdr new-theories)
				forced?
				(cons (if kept? oth nth) result)
				(if changed? (cons oth changed) changed)))))


(defun untypecheck-usedbys (theory)
  ;;   (format t "~%Untypechecking ~{~%  ~a~}"
  ;;     (cons (id theory) (find-all-usedbys theory)))
  (dolist (tid (find-all-usedbys theory))
    (let ((th (get-theory tid)))
      (when th
	(reset-proof-statuses th)
	(untypecheck-theory th)
	(tcdebug "~%~a untypechecked" (id th)))))
  (dolist (pair *tc-theories*)
    (reset-proof-statuses (car pair))
    (untypecheck-theory (car pair)))
  (reset-proof-statuses theory)
  ;;(untypecheck-theory theory)
  (tcdebug "~%~a untypechecked" (id theory)))

(defun reset-proof-statuses (theory)
  (when theory
    (let ((te (get-context-theory-entry (id theory))))
      (when te
	(pushnew 'invalid-proofs (te-status te))))
    (invalidate-proofs theory)))


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
  (pvs-buffer (format nil "~a.lisp" theoryid) nil)
  (pvs-buffer (format nil "~a.ppe" theoryid) nil))


#-(or akcl cmu sbcl)
(defmethod parse-file ((filename pathname) &optional forced? no-message? typecheck?)
  (parse-file (pathname-name filename) forced? no-message? typecheck?))

#+(or akcl cmu sbcl)
(defmethod parse-file (filename &optional forced? no-message? typecheck?)
  (assert (pathnamep pathname))
  (parse-file (pathname-name filename) forced? no-message? typecheck?))

(defmethod parse-file ((filename symbol) &optional forced? no-message? typecheck?)
  (parse-file (string filename) forced? no-message? typecheck?))

(defmethod parse-file ((filename name) &optional forced? no-message? typecheck?)
  (parse-file (string (id filename)) forced? no-message? typecheck?))

(defmethod parse-file ((theory module) &optional forced? no-message? typecheck?)
  (parse-file (id theory) forced? no-message? typecheck?))

;(defun ps (theoryname &optional forced?)
;  (parse-file theoryname forced? no-message?))


;;; Typechecking files.

(defun typecheck-file (fileref &optional forced? prove-tccs? importchain?
				  nomsg? outside-call?)
  (with-pvs-file (filename) fileref
    (cond ((string= filename "prelude")
	   (ldiff *prelude-theories* (member 'stdlang *prelude-theories* :key #'id)))
	  ((string= filename "pvsio_prelude")
	   (member 'stdlang *prelude-theories* :key #'id))
	  (t
	   (let ((*parsing-files* (when (boundp '*parsing-files*) *parsing-files*)))
	     (multiple-value-bind (theories restored? changed-theories)
		 (parse-file filename forced? nomsg? t)
	       (let ((*current-file* filename)
		     (*typechecking-module* nil))
		 (unless theories
		   (let ((err (format nil "~a not found" filename)))
		     (pvs-error "Typecheck error" err)))
		 (cond ((and (not forced?)
			     theories
			     (every #'(lambda (th)
					(let ((*current-context* (saved-context th)))
					  (typechecked? th)))
				    theories))
			(unless (or nomsg? restored?)
			  (pvs-message
			      "~a ~:[is already typechecked~;is typechecked~]~a"
			    filename
			    restored?
			    (if (and prove-tccs? (not *in-checker*))
				" - attempting proofs of TCCs" ""))))
		       ((and *in-checker*
			     (not *tc-add-decl*))
			(pvs-message "Must exit the prover first"))
		       ((and *in-evaluator*
			     (not *tc-add-decl*))
			(pvs-message "Must exit the evaluator first"))
		       (t (pvs-message "Typechecking ~a" filename)
			  (when forced?
			    (delete-generated-adt-files theories))
			  (typecheck-theories filename theories)
			  #+pvsdebug (assert (every #'typechecked? theories))
			  ;; .pvscontext
			  (update-context filename)
			  ;;(update-info-file filename theories)
			  ))
		 (when prove-tccs?
		   (if *in-checker*
		       (pvs-message
			   "Must exit the prover before running typecheck-prove")
		       (prove-tccs* theories importchain?)))
		 (if outside-call?
		     ;; Emacs expects t or nil - error will not get here
		     (and changed-theories t)
		     (values theories changed-theories)))))))))

(defun prove-tccs (fileref &optional importchain?)
  (with-pvs-file (filename) fileref
    (let ((theories (typecheck-file filename)))
      (prove-tccs* theories importchain?)
      )))

(defun prove-tccs* (theories &optional importchain?)
  (unless (listp theories)
    (setq theories (list theories)))
  (assert (every #'datatype-or-module? theories))
  (if importchain?
      (prove-unproved-tccs
       (delete-duplicates
	(mapcan #'(lambda (th)
		    (let ((*current-context* (saved-context th)))
		      (collect-theory-usings th)))
	  theories)
	:test #'eq)
       t)
      (prove-unproved-tccs theories)))

;; This is intended to create a JSON file with information for external use.
;; Mostly a list of theories, which have declarations and place information.
;; (defun update-info-file (filename theories)
;;   ;; (let* ((info-file (make-infopath filename)))
;;   ;;   (break))
;;   )
    

(defvar *etb-typechecked-theories*)

(defun etb-typecheck-theory (theoryref)
  (let ((*etb-typechecked-theories* nil))
    (etb-typecheck-theory* theoryref)))

(defun etb-typecheck-theory* (theoryref)
  (let ((th (get-typechecked-theory theoryref nil t)))
    (if th
	(unless (memq th *etb-typechecked-theories*)
	  (push th *etb-typechecked-theories*)
	  (let ((impths (get-immediate-usings th)))
	    (dolist (ith impths)
	      (etb-typecheck-theory* ith))
	    (prove-unproved-tccs* th t)
	    (let ((tccs (remove-if-not #'tcc-decl? (all-decls th))))
	      (format t "~%~a typechecked, ~d/~d proved TCCs, depends on ~{~a ~}"
		theoryref
		(count-if #'proved? tccs)
		(length tccs)
		impths))))
	(pvs-error "Typecheck error" (format nil "Theory ~a not found" theoryref)))))
      

(defun delete-generated-adt-files (theories)
  (dolist (th theories)
    (when (datatype? th)
      (let* ((adt-file (concatenate 'string (string (id th)) "_adt"))
	     (adt-path (make-specpath adt-file)))
	(when (file-exists-p adt-path)
	  (let ((sname (shortname adt-path))
		(error (nth-value 1 (ignore-errors (delete-file adt-path)))))
	    (if error
		(pvs-message "Error in removing file ~a:~% ~" sname error)
		(pvs-message "Deleted file ~a" sname))))))))

(defun typecheck-theories (pathname theories)
  (let* ((specpath (make-specpath pathname))
	 (filename (pathname-name specpath))
	 (all-proofs (read-pvs-file-proofs filename))
	 (sorted-theories (sort-theories theories)))
    ;;(check-import-circularities sorted-theories)
    (dolist (theory sorted-theories)
      (unless (typechecked? theory)
	(let ((start-time (get-internal-real-time))
	      (*current-context* (make-new-context theory))
	      (*old-tcc-names* nil))
	  (typecheck theory)
	  ;; (unwind-protect (typecheck theory)
	  ;;   (reset-subst-mod-params-cache))
	  (assert (saved-context theory))
	  (assert (typechecked? theory) nil
		  "Theory ~a not typechecked?" (id theory))
	  (restore-from-context filename theory all-proofs)
	  (set-default-proofs theory)
	  ;;	(when (and *prove-tccs* (module? theory))
	  ;;	  (prove-unproved-tccs (list theory))
	  ;;	  (setf (tccs-tried? theory) t))
	  (setf (filename theory) filename)
	  (multiple-value-bind (tot prv unprv sub simp)
	      (numbers-of-tccs theory)
	    (let ((time (realtime-since start-time)))
	      (if (zerop tot)
		  (pvs-message "~a typechecked in ~,2,-3fs: No TCCs generated~
                                ~[~:;; ~:*~d conversion~:p~]~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		    (id theory) time
		    (length (conversion-messages theory))
		    (length (warnings theory))
		    (length (info theory)))
		  (pvs-message
		      "~a typechecked in ~,2,-3fs: ~d TCC~:p, ~
                       ~d proved, ~d subsumed, ~d unproved~[~:;, ~:*~d trivial~]~
                       ~[~:;; ~:*~d conversion~:p~]~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
		    (id theory) time tot prv sub unprv simp
		    (length (conversion-messages theory))
		    (length (warnings theory)) (length (info theory))))))))))
  (let* ((filename (pathname-name pathname))
	 (ctheory (car (last theories)))
	 (*current-context* (saved-context ctheory)))
    (let ((dep (assoc filename *circular-file-dependencies* :test #'equal)))
      (when dep
	(setq *circular-file-dependencies*
	      (delete dep *circular-file-dependencies*))))
    (let ((deplist (mapcar #'(lambda (d)
			       (list (id d) (filename d)))
		     (circular-file-dependencies filename))))
      (when deplist
	(pvs-warning
	    "Circularity detected in file dependencies:~%~
           ~{  ~{~a from ~a.pvs~}~^, which imports~%~}~%~
           bin files will not be generated for any of these pvs files."
	  deplist))))
  theories)

(defun set-default-proofs (theory)
  (dolist (d (all-decls theory))
    (when (and (tcc? d)
	       (null (proofs d)))
      (set-default-proof d))))

(defun set-default-proof (tcc-decl)
  (setf (proofs tcc-decl)
	(list (mk-tcc-proof-info
	       (makesym "~a-1" (id tcc-decl))
	       nil (get-universal-time)
	       (list "" (list (default-tcc-proof tcc-decl)) nil nil)
	       nil nil (origin tcc-decl))))
  (setf (default-proof tcc-decl) (car (proofs tcc-decl))))

(defmethod default-tcc-proof (tcc-decl)
  (class-name (class-of tcc-decl)))
      

(defun prove-unproved-tccs (theories &optional importchain?)
  (read-strategies-files)
  (mapc #'prove-unproved-tccs* theories)
  (let* ((totals 0) (proved 0) (unproved 0) (subsumed 0) (simplified 0))
    (dolist (theory theories)
      (multiple-value-bind (tot prv unprv sub simp)
	  (numbers-of-tccs theory)
	(incf totals tot)
	(incf proved prv)
	(incf unproved unprv)
	(incf subsumed sub)
	(incf simplified simp)))
    (if (zerop totals)
	(pvs-message "File ~a typechecked: No TCCs to prove~a"
	  (filename (car theories)) (if importchain? "on importchain" ""))
	(pvs-message
	    "File ~a typechecked:~a ~d TCCs, ~d proved, ~d subsumed, ~d unproved~[~:;, ~:*~d trivial~]~
             ~[~:;; ~:*~d conversion~:p~]~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
	  (filename (car theories))
	  (if importchain? " importchain has" "")
	  totals proved subsumed unproved simplified
	  (reduce #'+ (mapcar #'(lambda (th) (length (conversion-messages th))) theories))
	  (reduce #'+ (mapcar #'(lambda (th) (length (warnings th))) theories))
	  (reduce #'+ (mapcar #'(lambda (th) (length (info th))) theories))))
    `((totals . ,totals) (proved . ,proved) (unproved . ,unproved)
      (subsumed . ,subsumed) (simplified . ,simplified))))

(defun prove-unproved-tccs* (theory &optional quiet?)
  (if (tccs-tried? theory)
      (progn
	(unless quiet?
	  (pvs-message "TCCs attempted earlier on ~a" (id theory))))
      (let ((tccs (collect-tccs theory))
	    (*justifications-changed?* nil))
	(unless (every #'proved? tccs)
	  (mapc #'(lambda (d)
		    (when (tcc? d)
		      (let ((*current-context* (context d)))
			(prove-tcc d))))
		(append (assuming theory) (theory theory))))
	(when *justifications-changed?*
	  (save-all-proofs theory))
	(setf (tccs-tried? theory) t))))

(defun collect-tccs (theory)
  (remove-if-not #'tcc?
    (append (formals theory)
	    (assuming theory)
	    (theory theory))))

(defun get-tccs (thref)
  (let* ((thstr (typecase thref
		 (cons ;; should be assoc list with cars
		  ;; :fileName, :fileExtension, :theoryName, :contextFolder
		  (unless (assq :fileName thref)
		    (error "bad thref: ~a" thref))
		  (format nil "~a/~a~a#~a"
		    (cdr (assq :contextFolder thref))
		    (cdr (assq :fileName thref))
		    (cdr (assq :fileExtension thref))
		    (cdr (assq :theoryName thref))))
		 (pathname (namestring thref))
		 (string thref)))
	 (theory (get-typechecked-theory thstr))
	 (tccs (collect-tccs theory)))
    (mapcar #'(lambda (tcc)
		  `((id . ,(id tcc))
		    (theory . ,(id (module tcc)))
		    (comment . ,(newline-comment tcc))
		    (from-decl . ,(id (generated-by tcc)))
		    (definition . ,(str (definition tcc)))
		    (proved . ,(proved? tcc))))
	tccs)))

(defmethod proved? ((fdecl formula-decl))
  (or (and (member (proof-status fdecl)
		   '(proved proved-complete proved-incomplete)
		   :test #'string-equal)
	   t)
      (when (mapped-formula-decl? fdecl)
	(proved? (from-formula fdecl)))))

(defmethod unproved? ((fdecl formula-decl))
  (not (proved? fdecl)))

(defmethod tccs-tried? ((adt recursive-type))
  t)

(defun prove-tcc (decl)
  (unless (and (default-proof decl)
	       (proved? decl))
    (unless (and (default-proof decl)
		 (not (or (null (script (default-proof decl)))
			  (equal (script (default-proof decl))
				 '("" (postpone) nil nil)))))
      (make-default-proof decl (tcc-strategy decl))
      (setq *justifications-changed?* t))
    (let* ((*proving-tcc* 'TCC)
	   (proof (rerun-prove decl)))
      (pvs-message
	  "~:[Unable to prove~;Proved~] ~:[~;TCC ~]~a in ~,2,-3f seconds"
	(eq (status-flag proof) '!) (tcc? decl) (id decl)
	(real-proof-time decl))
      ;; Must return non-NIL if proved, NIL otherwise.
      (if (eq (status-flag proof) '!)
	  (setf (proof-status decl) 'proved)
	  (setf (proof-status decl) 'unfinished))
      (eq (status-flag proof) '!))))

(defun tcc-prove (decl context)
  (let ((*suppress-printing* t)
	(*printproofstate* nil)
	(*proving-tcc* 'TCC));;'TCC to save the proof.
    (prove-decl decl
		:strategy `(then ,(tcc-strategy decl) (fail))
		:context context)))

(defmethod tcc-strategy ((decl tcc-decl))
  (list "" (list (class-name (class-of decl)))))

(defmethod tcc-strategy (decl)
  (declare (ignore decl))
  '("" (postpone) nil nil))

(defun sort-theories (theories)
  (let ((usings (mapcar #'(lambda (th)
			    (cons th (get-immediate-using-ids th)))
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

(defun get-immediate-using-ids (theory)
  (mapcan #'(lambda (d)
	      (get-immediate-using-ids* (theory-name d) theory))
    (remove-if-not #'mod-or-using? (all-decls theory))))

(defun get-immediate-using-ids* (tn theory &optional ids)
  (if (null tn)
      (nreverse ids)
      (let ((id (unless (library tn)
		   (if (eq (id tn) (id theory))
		       (if ids
			   (type-error tn
			     "Target ~a may not reference itself" (id theory))
			   (type-error tn
			     "Theory ~a may not import itself" (id theory)))
		       (id tn)))))
	(get-immediate-using-ids*
	 (target tn)
	 theory
	 (if id (cons id ids) ids)))))


(defun tc (modname &optional forced?)
  (typecheck-file modname forced?))

(defun tcp (modname &optional forced?)
  (typecheck-file modname forced? t))


;;; prove-untried commands

(defun prove-untried-importchain (theoryname
				  &optional (strategy '(grind)) tccs? exclude)
  (prove*-formulas-importchain theoryname strategy tccs? exclude 'untried))

(defun prove-untried-pvs-file (filename &optional (strategy '(grind)) tccs?)
  (prove*-formulas-pvs-file filename strategy tccs? 'untried))

(defun prove-untried-theory (theoryname &optional (strategy '(grind))
					tccs? filename)
  (prove*-formulas-theory theoryname strategy tccs? filename 'untried))

;;; prove-formulas commands

(defun prove-formulas-importchain (theoryname
				   &optional (strategy '(grind))
				   also-proved? exclude)
  (prove*-formulas-importchain theoryname strategy
			       also-proved? exclude 'formulas))

(defun prove-formulas-pvs-file (filename
				&optional (strategy '(grind)) also-proved?)
  (prove*-formulas-pvs-file filename strategy also-proved? 'formulas))

(defun prove-formulas-theory (theoryname &optional (strategy '(grind))
					 also-proved? filename)
  (prove*-formulas-theory theoryname strategy also-proved? filename 'formulas))

;;; prove-tccs commands

(defun prove-tccs-importchain (theoryname
			       &optional (strategy '(grind))
			       also-proved? exclude)
  (prove*-formulas-importchain theoryname strategy also-proved? exclude 'tccs))

(defun prove-tccs-pvs-file (filename
			    &optional (strategy '(grind)) also-proved?)
  (prove*-formulas-pvs-file filename strategy also-proved? 'tccs))

(defun prove-tccs-theory (theoryname
			  &optional (strategy '(grind)) also-proved? filename)
  (prove*-formulas-theory theoryname strategy also-proved? filename 'tccs))

;;; The generic prove*-formulas- functions

(defun prove*-formulas-importchain (theoryname
				    &optional (strategy '(grind))
				    flag exclude kind)
  (let ((just `("" ,strategy)))
    (multiple-value-bind (msg subjust)
	(check-edited-justification just)
      (when subjust
	(justification-error subjust just msg)))
    (let ((root-theory (get-typechecked-theory theoryname)))
      (if root-theory
	  (let ((*current-context* (context root-theory))
		(imports (remove-if #'(lambda (th)
				       (or (from-prelude? th)
					   (lib-datatype-or-theory? th)))
			   (collect-theory-usings theoryname exclude)))
		(total-tried 0)
		(total-proved 0))
	    (dolist (th imports)
	      (multiple-value-bind (tried proved)
		  (prove-formulas th just (get-formula-pred kind flag))
		(incf total-tried tried)
		(incf total-proved proved)))
	    (if (zerop total-tried)
		(pvs-message "No formulas attempted")
		(progn
		  (status-proof-importchain theoryname)
		  (pvs-message "~d formulas attempted, ~d proved"
		    total-tried total-proved))))
	  (pvs-message "Can't find theory ~a in the current context"
	    theoryname)))))

(defun prove*-formulas-pvs-file (filename
				 &optional (strategy '(grind)) flag kind)
  (let ((just `("" ,strategy)))
    (multiple-value-bind (msg subjust)
	(check-edited-justification just)
      (when subjust
	(justification-error subjust just msg)))
    (let ((total-tried 0)
	  (total-proved 0))
      (dolist (theory (typecheck-file filename))
	(multiple-value-bind (tried proved)
	    (prove-formulas theory just (get-formula-pred kind flag))
	  (incf total-tried tried)
	  (incf total-proved proved)))
      (if (zerop total-tried)
	  (pvs-message "No formulas attempted")
	  (progn
	    (status-proof-pvs-file filename)
	    (pvs-message "~d formulas attempted, ~d proved"
	      total-tried total-proved))))))

(defun prove*-formulas-theory (theoryname &optional (strategy '(grind))
					  flag filename kind)
  (when filename
    (typecheck-file filename))
  (let ((just `("" ,strategy)))
    (multiple-value-bind (msg subjust)
	(check-edited-justification just)
      (when subjust
	(justification-error subjust just msg)))
    (let ((theory (get-typechecked-theory theoryname)))
      (when theory
	(multiple-value-bind (total-tried total-proved)
	    (prove-formulas theory just (get-formula-pred kind flag))
	  (if (zerop total-tried)
	      (pvs-message "No formulas attempted")
	      (progn
		(status-proof-theory theory)
		(pvs-message "~d formulas attempted, ~d proved"
		  total-tried total-proved))))))))

(defun prove-formulas (theory just &optional formula-pred)
  (read-strategies-files)
  (let ((save-proofs nil)
	(tried-proofs 0)
	(proved-proofs 0))
    (dolist (fmla (provable-formulas (all-decls theory)))
      (when (funcall formula-pred fmla)
	(let ((orig-just (extract-justification-sexp (justification fmla)))
	      (orig-status (proof-status fmla))
	      (orig-dp (decision-procedure-used fmla))
	      (orig-proof-refers-to (proof-refers-to fmla))
	      (orig-real-time (real-time fmla))
	      (orig-run-time (run-time fmla)))
	  (pvs-message "Proving formula ~a" (id fmla))
	  (incf tried-proofs)
	  (setf (justification fmla) just)
	  (setf (proof-status fmla) nil)
	  (rerun-prove fmla)
	  (cond ((proved? fmla)
		 (pvs-message "~a proved - changing strategy to ~a"
		   (id fmla) just)
		 (setq save-proofs t)
		 (incf proved-proofs)
		 (copy-proofs-to-orphan-file
		  (id theory) (list (cons (id fmla) orig-just))))
		(orig-just
		 (pvs-message "~a unproved - keeping original strategy"
		   (id fmla))
		 (setf (justification fmla) orig-just
		       (proof-status fmla) orig-status
		       (decision-procedure-used fmla) orig-dp
		       (proof-refers-to fmla) orig-proof-refers-to
		       (real-time fmla) orig-real-time
		       (run-time fmla) orig-run-time))
		(t (pvs-message
		       "~a unproved - no current strategy so adding new one"
		     (id fmla))
		   (setq save-proofs t))))))
    (cond (save-proofs
	   (save-all-proofs theory))
	  (tried-proofs
	   (pvs-message "Every attempted formula of theory ~a has an existing proof and failed to prove with the given strategy"
	     (id theory)))
	  (t (pvs-message "No formulas attempted from theory ~a"
	       (id theory))))
    (values tried-proofs proved-proofs)))

(defun get-formula-pred (kind flag)
  (case kind
    (untried (if flag #'formula-without-proof? #'nontcc-without-proof?))
    (formulas (if flag #'nontcc? #'unproved-nontcc?))
    (tccs (if flag #'tcc? #'unproved-tcc?))
    (t (error "get-formula-pred: unknown kind ~a" kind))))

(defun formula-without-proof? (fdecl)
  (null (justification fdecl)))

(defun nontcc-without-proof? (fdecl)
  (and (not (tcc? fdecl))
       (null (justification fdecl))))

(defun nontcc? (fdecl)
  (not (tcc? fdecl)))

(defun unproved-nontcc? (fdecl)
  (and (not (tcc? fdecl))
       (not (proved? fdecl))))

(defun unproved-tcc? (fdecl)
  (and (tcc? fdecl)
       (not (proved? fdecl))))

(defun set-pvs-file-tcc-proofs (pvs-file strategy)
  "Simple minded resetting of all TCCs to the given strategy
Note that even proved ones get overwritten"
  (let ((just `("" ,strategy)))
    (multiple-value-bind (msg subjust)
	(check-edited-justification just)
      (when subjust
	(justification-error subjust just msg)))
    (dolist (theory (typecheck-file pvs-file nil nil nil t))
      (dolist (tcc (collect-tccs theory))
	(setf (justification tcc) just)))))

(defun sizeof-proof (fdecl)
  (numberof-steps (editable-justification (justification fdecl))))

(defun write-theory-sizes (theory out)
  (let ((total 0))
    (format out "Proof Sizes for theory ~a~%" (id theory))
    (format out "~:{ ~a: ~d~%~}"
      (mapcar #'(lambda (fd)
		  (let ((psize (sizeof-proof fd)))
		    (incf total psize)
		    (list (id fd) psize)))
	(provable-formulas (all-decls theory))))
    (format out "Total proof size for theory ~a: ~d~2%"
      (id theory) total)
    total))

(defun sizeof-proof-at (filename declname line &optional (origin "pvs"))
  (let ((fdecl (formula-decl-to-prove filename declname line origin)))
    (if fdecl
	(pvs-message "Proof of ~a has ~d steps" (id fdecl) (sizeof-proof fdecl))
	(pvs-message "Unable to find formula declaration"))))

(defun sizeof-proofs-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (if theory
	(pvs-buffer "Proof Sizes"
	  (with-output-to-string (out)
	    (write-theory-sizes theory out))
	  t t)
	(pvs-message "~a has not been typechecked" theoryname))))

(defun sizeof-proofs-pvs-file (filename)
  (let ((theories (cdr (gethash filename (current-pvs-files)))))
    (if theories
	(pvs-buffer "Proof Sizes"
	  (with-output-to-string (out)
	    (let ((total 0))
	      (dolist (th theories)
		(incf total (write-theory-sizes th out)))
	      (when (cdr theories)
		(format out "Total proof size for PVS File ~a: ~d~2%"
		  filename total))))
	  t t)
	(pvs-message "~a has not been typechecked" filename))))

(defun sizeof-proofs-importchain (theoryname)
  (let ((theory (get-theory theoryname)))
    (if theory
	(let ((imports (remove-if #'(lambda (th)
				      (or (from-prelude? th)
					  (lib-datatype-or-theory? th)))
			 (collect-theory-usings theoryname)))
	      (total 0))
	  (pvs-buffer "Proof Sizes"
	    (with-output-to-string (out)
	      (format out "Proof sizes for importchain of ~a~2%" theoryname)
	      (dolist (th imports)
		(incf total (write-theory-sizes th out)))
	      (format out "Total proof size for import chain of ~a: ~d~2%"
		theoryname total))
	    t t))
	(pvs-message "~a has not been typechecked" theoryname))))

(defun sizeof-proofs-proofchain-at (filename declname line
					     &optional (origin "pvs"))
  (if (or (gethash filename (current-pvs-files))
	  (and (member origin '("ppe" "tccs") :test #'string=)
	       (get-theory filename)))
      (let ((fdecl (formula-decl-to-prove filename declname line origin)))
	(if fdecl
	    (pvs-buffer "Proof Sizes"
	      (with-output-to-string (out)
		(format out "Proof Sizes for proofchain of ~a" (id fdecl))
		(let ((total 0))
		  (format out "~:{ ~a: ~d~%~}"
		    (mapcar #'(lambda (fd)
				(let ((psize (sizeof-proof fd)))
				  (incf total psize)
				  (list (id fd) psize)))
		      (provable-formulas (get-proofchain fdecl))))
		  (format out "Total proof size for proofchain of ~a: ~d~2%"
		    (id fdecl) total)))
	      t t)
	    (pvs-message "Unable to find formula declaration")))
      (pvs-message "~a.pvs has not been typechecked" filename)))

;;; Prettyprinting

(defun prettyprint-region (fileref pos1 &optional (pos2 pos1))
  (with-pvs-file (filename) fileref
    (parse-file filename nil t)
    (let ((start-reg (car pos1))
	  (end-reg (car pos2))
	  (*no-comments* nil)
	  (*ppmacros* t))
      (dolist (theory (reverse (get-theories filename)))
	(let ((start-theory (line-begin (place theory)))
	      (end-theory (line-end (place theory))))
	  (cond ((<= end-theory start-reg) nil) ;theory above the region
		((>= start-theory end-reg) nil) ;theory below the region
		((and (<= start-reg start-theory)
		      (<= end-theory end-reg)) ;theory contained in region
		 (prettyprint-theory (id theory) filename))
		(t (prettyprint-decls theory pos1 pos2))))))))

(defun prettyprint-decls (theory pos1 pos2)
  (let ((*no-comments* nil)
	(*show-conversions* nil)
	(*ppmacros* t)
	(*current-context* (saved-context theory)))
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
  (let* ((*show-conversions* nil)
	 (place (place d))
	 (indent (col-begin place))
	 (dstr (unpindent d indent :string t))
         (dfinal (string-trim '(#\Space #\Tab #\Newline) dstr))
	 (*ppmacros* t))
    (pvs-modify-buffer (shortname *default-pathname-defaults*)
                       (filename theory)
                       place dfinal)))

(defun prettyprint-theory (theoryname filename)
  (let ((file (or filename
		  (pvs-file-of theoryname))))
    (when file
      (parse-file file nil t)))
  (let* ((theory (get-parsed?-theory theoryname))
	 (*current-context* (when theory (saved-context theory)))
	 (*no-comments* nil)
	 (*show-conversions* nil)
	 (*ppmacros* t))
    (when theory
      (let ((string (unparse theory
		      :string t
		      :char-width *default-char-width*)))
	(pvs-modify-buffer (uiop:pathname-directory-pathname filename)
			   (filename theory)
			   (place theory)
			   (string-right-trim '(#\space #\tab #\newline)
					      string))))))

(defun prettyprint-pvs-file (fileref)
  (with-pvs-file (filename) fileref
    (let ((theories (parse-file filename nil t))
	  (*no-comments* nil)
	  (*show-conversions* nil)
	  (*ppmacros* t))
      (pvs-buffer (makesym "~a.pvs" filename)
	(format nil "~{~a~^~2%~}"
	  (mapcar #'(lambda (th)
		      (let ((*current-context* (saved-context th)))
			(unparse th
			  :string t
			  :char-width *default-char-width*)))
	    theories))
	t))))

(defun prettyprint-theory-instance (theoryname-string
				    &optional context-theoryname)
  (declare (ignore theoryname-string context-theoryname))
  (pvs-message
      "prettyprint-theory-instance no longer used - use prettyprint-expanded instead"))

(defun make-tccinfo-tcc-name (tccinfo context-theory ctr)
  (makesym "IMP_~a_~@[~a_~]TCC~d"
	   (id context-theory)
	   (when (eq (tccinfo-kind tccinfo) 'mapped-axiom)
	     (id (tccinfo-type tccinfo)))
	   ctr))

; (defmethod prettyprint (theory)
;   (let ((*no-comments* nil))
;     (prettyprint (get-parsed-theory theory))))

;(defun pp (theory)
;  (prettyprint theory))

;;; View Theory

(defun prettyprint-expanded (theoryref)
  (with-pvs-file (fname thname) theoryref
    (let ((*no-comments* nil)
	  (*unparse-expanded* t)
	  (*xt-periods-allowed* t))
      (pvs-buffer (format nil "~a.ppe" (or thname fname))
	(let* ((theory (get-typechecked-theory (or thname fname)))
	       (thstring (unparse theory
			   :string t
			   :char-width *default-char-width*)))
	  (unless (recursive-type? theory)
	    (setf (ppe-form theory) (parse :string thstring)))
	  thstring)
      t t))))

(defun ppe (theory)
  (prettyprint-expanded theory))


(defun show-tccs (theoryref &optional arg)
  (with-pvs-file (fname thname) theoryref
    (let* ((theory (get-typechecked-theory (or thname fname)))
	   (unproved-only? (and arg (not (minusp arg))))
	   (include-trivial? (and arg (minusp arg)))
	   (*xt-periods-allowed* t)
	   (*no-comments* nil))
      (when theory
	(let* ((*comment-on-proof-status* t)
	       (*no-comments* t)
	       (*unparse-expanded* t)
	       (*pp-new-projection-forms* t)
	       (unparsed-a-tcc? nil)
	       (str (string-trim
		     '(#\Space #\Tab #\Newline)
		     (with-output-to-string (out)
		       (dolist (decl (all-decls theory))
			 (dolist (cmt (cdr (assq decl (tcc-comments theory))))
			   (when (or include-trivial?
				     (not (memq (fourth cmt) '(trivial in-context))))
			     (write (apply #'print-tcc-comment decl cmt)
				    :stream out :escape nil)
			     (terpri out) (terpri out)))
			 (when (and (tcc? decl)
				    (or (not unproved-only?)
					(unproved? decl)))
			   (unparse decl :stream out)
			   (terpri out) (terpri out)
			   (setq unparsed-a-tcc? t))))))
	       ;; json form:
	       ;; pvsfile := [ theory+ ]
	       ;; theory := {id: Id, decls: [decl]}
	       ;; decl := importing | typed-decl | formula-decl
	       ;; importing := 
	       (buffer (format nil "~a.tccs" (id theory))))
	  (cond ((not (string= str ""))
		 (let ((*valid-id-check* nil))
		   (setf (tcc-form theory)
			 (if unparsed-a-tcc?
			     (parse :string str :nt 'theory-part)
			     str)))
		 (pvs-buffer buffer str t t))
		(t (pvs-message "Theory ~a has no TCCs" theoryref))))))))

(defun show-declaration-tccs (bufname origin line &optional unproved-only?)
  (let* ((decl (get-decl-at-origin bufname origin line))
	 (theory (module decl))
	 (tccs (reverse (remove-if (complement #'tcc?) (generated decl))))
	 ;; Even if tccs is null, there might be tcc comments, so we go on.
	 (*comment-on-proof-status* t)
	 (*no-comments* t)
	 (*unparse-expanded* t)
	 (*pp-new-projection-forms* t)
	 (unparsed-a-tcc? nil)
	 (str (string-trim
	       '(#\Space #\Tab #\Newline)
	       (with-output-to-string (out)
		 (dolist (tcc tccs)
		   (dolist (cmt (cdr (assq tcc (tcc-comments theory))))
		     (write cmt :stream out :escape nil)
		     (terpri out) (terpri out))
		   (when (or (not unproved-only?)
			     (unproved? tcc))
		     (unparse tcc :stream out)
		     (terpri out) (terpri out)
		     (setq unparsed-a-tcc? t)))
		 (dolist (cmt (cdr (assq decl (tcc-comments theory))))
		   (write cmt :stream out :escape nil)
		   (terpri out) (terpri out)))))
	 (theory-decl (format nil "~a.~a" (id theory) (decl-to-declname decl)))
	 (buffer (format nil "~a.~a.tccs" (id theory) (decl-to-declname decl))))
    (cond ((not (string= str ""))
	   (let ((*valid-id-check* nil))
	     (setf (tcc-form decl)
		   (if unparsed-a-tcc?
		       (parse :string str :nt 'theory-part)
		       str)))
	   (pvs-buffer buffer str t t)
	   theory-decl)
	  (t (pvs-message "Declaration ~a.~a has no TCCs"
	       (id theory) (decl-to-declname decl))))))

;;; Given a declaration, returns a declname, used to create the
;;; show-declaration-tccs buffer.  For a declaration with an id, this is
;;; the string of the id itself, if it is the only declaration of the
;;; theory with that id.  If there is more than one, then it is suffixed
;;; with "-D", where D is a number.  For importings, the name is simply
;;; IMPORTING-D, etc.
(defmethod decl-to-declname ((decl declaration))
  (let* ((theory (module decl))
	 (decls (remove-if (complement #'(lambda (d)
					   (and (declaration? d)
						(eq (id d) (id decl)))))
		  (all-decls theory)))
	 (pos (when (cdr decls)
		(position decl decls))))
    (assert decls)
    (assert (or (null (cdr decls)) pos))
    (if pos
	(format nil "~a-~d" (id decl) (1+ pos))
	(string (id decl)))))

(defmethod decl-to-declname ((imp importing))
  (let* ((theory (module imp))
	 (imps (remove-if (complement #'importing?) (all-decls theory)))
	 (pos (position imp imps)))
    (assert pos)
    (format nil "IMPORTING-~d" (1+ pos))))

(defun declname-to-decl (declname theory)
  (let ((decl-and-pos
	 #+allegro (excl:split-regexp "-" declname)
	 #-allegro (make::split-string declname :item #\-)))
    (if (string= (car decl-and-pos) "IMPORTING")
	(let ((pos (parse-integer (cdr decl-and-pos)))
	      (imps (remove-if (complement #'importing?) (all-decls theory))))
	  (nth (1- pos) imps))
	(let* ((declid (intern (car decl-and-pos) :pvs))
	       (pos (if (cadr decl-and-pos)
			(1- (parse-integer (cadr decl-and-pos)))
			0))
	       (decls (remove-if (complement
				  #'(lambda (d)
				      (and (declaration? d)
					   (eq (id d) declid))))
			(all-decls theory))))
	  (nth pos decls)))))

(defun tcc? (decl)
  (and (formula-decl? decl)
       (eq (kind decl) 'tcc)))


(defun parsed-file? (filename)
  (let* ((file (if (pathnamep filename)
		   filename
		   (make-specpath filename)))
	 (pdate (parsed-date file)))
    (and pdate
	 (eql pdate (file-write-date file)))))

(defmethod parsed? ((mod datatype-or-module))
  (let ((*theories-seen* *theories-seen*))
    (parsed?* mod)))

(defmethod parsed? ((modref modname))
  (parsed? (get-theory modref)))

(defmethod parsed?* ((mod datatype-or-module))
  (or (memq mod *theories-seen*)
      (let ((prsd? (cond ((from-prelude? mod))
			 ((lib-datatype-or-theory? mod)
			  (with-workspace (context-path mod)
			    (parsed?* mod)))
			 ((generated-by mod)
			  (let ((gth (get-theory (generated-by mod))))
			    ;;(unless gth (break "parsed?*: ~a" (generated-by mod)))
			    (and gth
				 (or (parsed?* gth)
				     ;;(break "(parsed?* ~a) failed" gth)
				     ))))
			 (t (and (filename mod)
				 (eql (car (gethash (filename mod) (current-pvs-files)))
				      (file-write-date (make-specpath (filename mod)))))))))
	(push mod *theories-seen*)
	prsd?)))


#-gcl
(defmethod parsed?* ((path pathname))
  (let ((pdate (parsed-date path))
	(fdate (file-write-date path)))
    (and pdate fdate
	 (eql (parsed-date path)
	      (file-write-date path)))))

#+gcl
(defmethod parsed?* (path)
  (assert (pathnamep path))
  (let ((pdate (parsed-date path))
	(fdate (file-write-date path)))
    (and pdate fdate
	 (eql (parsed-date path)
	      (file-write-date path)))))

(defmethod parsed?* ((x null))
  nil)

(defun typechecked-file? (filename)
  (and (parsed-file? filename)
       (every #'(lambda (m)
		  (member 'typechecked (status m)))
	      (get-theories filename))))


;;; Must be a method, since the slot exists for declarations.

(defmethod typechecked? ((theory module))
  (let ((*theories-seen* nil))
    (typechecked*? theory)))

(defun typechecked*? (theory)
  (if (assq theory *theories-seen*)
      (cdr (assq theory *theories-seen*))
      (let ((tyckd? (or (and (parsed? theory)
			     (memq 'typechecked (status theory))
			     (saved-context theory)
			     (let* ((*current-context* (saved-context theory))
				    (importings (all-importings theory)))
			       (every #'(lambda (th)
					  (or (and (parsed? th)
						   (memq 'typechecked (status th)))
					      ;;(break "importing ~a" th)
					      ))
				      importings)))
			;; (when (memq 'typechecked (status theory))
			;;   (break "typechecked*? failed: ~a" theory) nil)
			)))
	(setq *theories-seen* (acons theory tyckd? *theories-seen*))
	tyckd?)))

(defmethod typechecked? ((theory datatype-or-module))
  (or *in-checker*
      (and (memq 'typechecked (status theory))
	   (saved-context theory)
	   (let* ((*current-context* (saved-context theory))
		  (importings (all-importings theory)))
	     (every #'(lambda (th)
			(and (parsed? th)
			     (memq 'typechecked (status th))))
		    importings)))))

(defmethod typechecked? ((theoryref string))
  (let ((theory (get-theory (pc-parse theoryref 'modname))))
    (and theory
	 (typechecked? theory))))

(defmethod typechecked? ((imp importing))
  (saved-context imp))

(defmethod typechecked? (theoryref)
  (let ((theory (get-theory theoryref)))
    (and theory
	 (typechecked? theory))))

(defmethod typechecked? ((tname modname))
  (let ((theory (get-theory tname)))
    (and theory
	 (typechecked? theory))))

(defun get-theories (filename &optional libref)
  (if libref
      (let ((lib-path (get-library-path libref)))
	(and lib-path
	     (let* ((ws (get-workspace-session lib-path)))
	       (and ws
		    (cdr (gethash filename (pvs-files ws)))))))
      (let ((fn (if (pathnamep filename)
		    (pathname-name filename)
		    filename)))
	(or (cdr (gethash fn (current-pvs-files)))
	    (and (equal fn "prelude")
		 *prelude-theories*)))))

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

(defun prove-file-at (name declname line rerun?
			   &optional origin buffer prelude-offset
			   background? display? unproved?)
  ;; Check for old style input - there was no declname then
  (unless (integerp line)
    (setq unproved? display?
	  display? background?
	  background? prelude-offset
	  prelude-offset buffer
	  buffer origin
	  origin rerun?
	  rerun? line
	  line declname
	  declname nil))
  (let ((*to-emacs* background?))
    (if (or *in-checker* *in-evaluator*)
	(pvs-message "Must exit the prover/evaluator first")
	(multiple-value-bind (fdecl place)
	    (formula-decl-to-prove name declname line origin unproved?)
	  (if (and rerun?
		   fdecl
		   (null (justification fdecl)))
	      (pvs-message "Formula ~a has no proof to rerun." (id fdecl))
	      (if fdecl
		  (let ((*current-context* (context fdecl))
			(*current-system* (if (member origin '("tccs" "ppe"))
					      'pvs
					      (intern origin :pvs)))
			(*start-proof-display* display?)
			(ojust (extract-justification-sexp
				(justification fdecl)))
			(decision-procedure (decision-procedure-used fdecl))
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
				(null (default-proof fdecl)))
		      (setf (interactive? (default-proof fdecl)) t))
		    ;; Save the proof if it is different.
		    (unless (or (equal origin "prelude")
				(from-prelude? fdecl))
		      (when (or *justifications-changed?*
				(not (equal ojust
					    (extract-justification-sexp
					     (justification fdecl))))
				(not (eq (decision-procedure-used fdecl)
					 decision-procedure)))
			(save-all-proofs (current-theory)))
		      ;; If the proof status has changed, update the context.
		      (update-context-proof-status fdecl))
		    (remove-auto-save-proof-file)
		    (let ((*to-emacs* t))
		      (pvs-locate buffer fdecl
				  (if (and prelude-offset
					   (not (zerop prelude-offset)))
				      (vector (- (line-begin place) prelude-offset)
					      (col-begin place)
					      (- (line-end place) prelude-offset)
					      (col-end place))
				      place)))))))))
  ;; This prints nothing - better than "nil"
  ;; Actually causes problems - will look into other solutions
  ;; (unless *noninteractive*
  ;;   (values))
  nil)

(deftype unproved-formula-decl () '(and formula-decl (satisfies unproved?)))

(defun formula-decl-to-prove (fileref declname line origin &optional unproved?)
  (with-pvs-file (name) fileref
    (if (and (member origin '("ppe" "tccs") :test #'string=)
	     (not (get-theory name)))
	(pvs-message "~a is not typechecked" name)
	(case (intern #+allegro (string-downcase origin)
		      #-allegro (string-upcase origin)
		      :pvs)
	  (ppe (let* ((theories (ppe-form (get-theory name)))
		      (typespec (formula-typespec unproved?))
		      (decl (get-decl-at line typespec theories)))
		 (when decl
		   (values (find-if #'(lambda (d)
					(and (formula-decl? d)
					     (eq (id d) (id decl))))
			     (all-decls (get-theory name)))
			   (place decl)))))
	  (tccs (let* ((theory (get-theory name))
		       (decls
			(if declname
			    (tcc-form (declname-to-decl declname theory))
			    (tcc-form theory)))
		       (decl (find-if #'(lambda (d)
					  (and (>= (line-end (place d)) line)
					       (or (null unproved?)
						   (unproved? d))))
			       decls)))
		  (when decl
		    (values (find-if #'(lambda (d) (and (eq (module d) theory)
							(formula-decl? d)
							(eq (id d) (id decl))))
			      (all-decls theory))
			    (place decl)))))
	  ((prelude pvsio_prelude)
	   (let* ((theory (get-theory name))
		  (theories (if (and theory (generated-by theory))
				(list theory)
				(remove-if #'generated-by
				  (if (string= name "pvsio_prelude")
				      (member '|stdlang| *prelude-theories*
					      :key #'id)
				      *prelude-theories*))))
		  (typespec (formula-typespec unproved?))
		  (decl-at (get-decl-at line typespec theories))
		  (decl (if (judgement? decl-at)
			    (let ((jtcc (find-if #'(lambda (d)
						     (eq (id d)
							 (id decl-at)))
					  (generated decl-at))))
			      (unless (place jtcc)
				(setf (place jtcc) (place decl-at)))
			      jtcc)
			    decl-at)))
	     (values decl (place decl))))
	  (proof-status
	   (let* ((theory (get-theory name))
		  (fdecl (find-if #'(lambda (d)
				      (and (formula-decl? d)
					   (string= (id d) declname)))
			   (all-decls theory))))
	     (values fdecl (vector line 0 line 0))))
	  (t (let* ((theories (typecheck-file name nil nil nil t))
		    (typespec (formula-typespec unproved?))
		    (decl-at (get-decl-at line typespec theories))
		    (decl (if (judgement? decl-at)
			      (let ((jtcc (find-if #'judgement-tcc?
					    (generated decl-at))))
				(unless (place jtcc)
				  (setf (place jtcc) (place decl-at)))
				jtcc)
			      decl-at)))
	       (values decl (when decl (place decl)))))))))

(defun formula-typespec (unproved?)
  (if unproved?
      '(or unproved-formula-decl
	   (and judgement
		(satisfies some-unproved-generated-judgement-tcc?)))
      '(or formula-decl
	   (and judgement
		(satisfies some-generated-judgement-tcc?)))))

(defun some-generated-judgement-tcc? (jd)
  (and (not (generated-by jd))
       (some #'judgement-tcc? (generated jd))))

(defun some-unproved-generated-judgement-tcc? (jd)
  (and (not (generated-by jd))
       (some #'(lambda (d)
		 (and (judgement-tcc? d)
		      (unproved? d)))
	     (generated jd))))
  


;;; This function is invoked from Emacs by pvs-prove-formula.  It provides
;;; an answer as to whether the proof should be rerun.  It takes a file, a
;;; line number, an optional origin, and an optional flag indicating
;;; whther it should try to rerun.  It returns T if the proof should be
;;; rerun, NO if the proof should not be rerun, and NIL if the formula
;;; declaration could not be found.

(defun rerun-proof-at? (name declname line &optional origin rerun? unproved?)
  (let ((fdecl (formula-decl-to-prove name declname line origin unproved?)))
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
			t 'no))
	       'no))
	  (unproved? (pvs-message "No more unproved formulas below"))
	  (t (pvs-message
		 "Not at a formula declaration (line ~d)~@[ - ~a buffer may be invalid~]"
	       line
	       (car (member (intern #+allegro (string-downcase origin)
				    #-allegro (string-upcase origin)
				    :pvs)
			    '(tccs ppe))))))))

(defun prove-formula (formref &optional formname rerun?)
  "Starts a proof with formula given by formref.  If formname is provided,
it is the name of the formula, and formref should be a theoryname.  If
formname is nil, then formref should resolve to a unique name."
  (when *in-checker*
    (pvs-error "Prove-formula error" "Must exit the prover first"))
  (let ((fdecl (get-formula-decl formref formname)))
    (with-workspace (context-path (module fdecl))
      (let* ((strat (when rerun? '(rerun)))
	     (*please-interrupt* t))
	(read-strategies-files)
	(setq *last-proof* (prove fdecl :strategy strat))))))

(defun get-proof-status (formref &optional formname)
  (let ((fdecl (get-formula-decl formref formname)))
    (status (default-proof fdecl))))

(defun get-formula-decl (formref &optional formname)
  (with-pvs-file (name thname fname) formref
    (assert (or formname fname thname name) ()
	    "get-formula-decl missing formula name?")
    (if formname
	(unless thname
	  (setf thname (or name fname)))
	(if fname
	    (setf formname fname
		  thname (or thname name))
	    (setf formname thname
		  thname name)))
    (let ((fdecls (get-matching-prove-formulas name thname formname)))
      (cond ((cdr fdecls)
	     (let ((locdecls (remove-if-not
				 #'(lambda (fd)
				     (uiop:pathname-equal
				      (context-path (module fd))
				      (current-context-path)))
			       fdecls)))
	       (cond ((cdr locdecls)
		      ;; Only report errors on current context ambiguities
		      (pvs-error "formula ambiguous error"
			(format nil "formula ambiguous for name ~a, ~
                          it appears in the following theories:~%~{~a~^~%~}"
			  (or formname name)
			  (mapcar #'(lambda (fd) (id (module fd))) locdecls))))
		     ((null locdecls)
		      ;; Ambiguous - give error for now
		      (pvs-error "formula ambiguous error"
			(format nil "formula ambiguous for name ~a, ~
                          it appears in the following theories:~%~{~a~^~%~}"
			  (or formname name)
			  (mapcar #'(lambda (fd) (id (module fd))) fdecls))))
		     (t (car locdecls)))))
	    ((null fdecls)
	     (pvs-error "formula not found error"
	       (format nil
		   "Formula name ~a not found in any (typechecked) theories"
		 (or formname name))))
	    (t (car fdecls))))))

(defun get-matching-prove-formulas (name thname formname)
  (let ((formulas nil))
    (when (and formname
	       name
	       (file-exists-p (make-specpath name)))
      ;; A PVS file, but if no ext, could still be a theory name
      (let ((theories (with-no-type-errors (typecheck-file name))))
	(if thname
	    (let ((theory (find thname theories :test #'string= :key #'id)))
	      (dolist (d (all-decls theory))
		(when (and (formula-decl? d)
			   (string= (id d) formname))
		  (push d formulas))))
	    (dolist (theory theories)
	      (dolist (d (all-decls theory))
		(when (and (formula-decl? d)
			   (string= (id d) formname))
		  (push d formulas)))))))
    ;; name could be a theory - but we only do this if no formulas found
    (when (and formname
	       (or thname name)
	       (null formulas))
      (let ((theory (with-no-type-errors (get-typechecked-theory (or thname name)))))
	(when theory
	  (dolist (d (all-decls theory))
	    (when (and (formula-decl? d)
		       (string= (id d) formname))
	      (push d formulas))))))
    (unless formname
      ;; name must refer to a formula name
      ;; Simply look through every available theory
      (do-all-theories
	  #'(lambda (theory)
	      (when (typechecked? theory)
		(dolist (d (all-decls theory))
		  (when (and (formula-decl? d)
			     (string= (id d) name))
		    (push d formulas)))))))
    formulas))

(defun rerun-proof-of? (modname formname)
  (let* ((*current-context* (context (get-theory modname)))
	 (fid (intern formname :pvs))
	 (fdecl (find-if #'(lambda (d) (and (formula-decl? d)
					    (eq (id d) fid)))
		  (all-decls (current-theory)))))
    (and fdecl
	 (justification fdecl)
	 (pvs-y-or-n-p "Rerun Existing proof? "))))

(defun prove-next-unproved-formula (name declname line rerun?
					 &optional origin buffer prelude-offset
					 background? display?)
  (prove-file-at name declname line rerun? origin buffer prelude-offset
		 background? display? t))

(defun get-proof-script (formref &optional formname)
  "Gets the proof script for formula given by formref.  If formname is
provided, it is the name of the formula, and formref should be a theoryname.
If formname is nil, then formref should resolve to a unique formula name."
  (with-pvs-file (name thname fname) formref
    (unless (or formname fname thname name)
      (error "get-proof-script missing formula name?"))
    ;; Check for no dir, or it's already the current context-path
    (if formname
	(unless thname
	  (setf thname (or name fname)))
	(if fname
	    (setf formname fname
		  thname (or thname name))
	    (setf formname thname
		  thname name)))
    (let ((fdecls (get-matching-prove-formulas name thname formname)))
      (cond ((cdr fdecls)
	     (let ((locdecls (remove-if-not
				 #'(lambda (fd)
				     (uiop:pathname-equal
				      (context-path (module fd))
				      (current-context-path)))
			       fdecls)))
	       (cond ((cdr locdecls)
		      ;; Only report errors on current context ambiguities
		      (pvs-error "get-proof-script error"
			(format nil "get-proof-script ambiguous for name ~a, ~
                          it appears in the following theories:~%~{~a~^~%~}"
			  (or formname name)
			  (mapcar #'(lambda (fd) (id (module fd))) locdecls))))
		     ((null locdecls)
		      ;; Ambiguous - give error for now
		      (pvs-error "get-proof-script error"
			(format nil "get-proof-script ambiguous for name ~a, ~
                          it appears in the following theories:~%~{~a~^~%~}"
			  (or formname name)
			  (mapcar #'(lambda (fd) (id (module fd))) fdecls))))
		     (t (get-proof-script-output-string (car locdecls))))))
	    ((null fdecls)
	     (pvs-error "get-proof-script error"
	       (format nil
		   "get-proof-script formula name ~a not found in any (typechecked) theories"
		 (or formname name))))
	    (t (get-proof-script-output-string (car fdecls)))))))

;;; Non-interactive Proving

(defun prove-theory (theoryname &optional retry? filename use-default-dp? save-proofs?)
  (when filename
    (typecheck-file filename))
  (let ((theory (get-typechecked-theory theoryname)))
    (when theory
      (prove-theories theoryname (list theory) retry? use-default-dp? save-proofs?)
      (status-proof-theory theoryname))))


(defun prove-pvs-file (fileref retry? &optional use-default-dp? save-proofs?)
  (with-pvs-file (fname) fileref
    (let ((theories (typecheck-file fname))
	  (*use-default-dp?* use-default-dp?))
      (prove-theories fname theories retry? use-default-dp? save-proofs?)
      (status-proof-pvs-file fname))))

(defun prove-pvs-theories (theory-names retry? &optional use-default-dp? save-proofs?)
  (when theory-names
    (let ((theories (mapcar #'get-typechecked-theory theory-names))
	  (*use-default-dp?* use-default-dp?))
      (prove-theories "" theories retry? use-default-dp? save-proofs?)
      (status-proof-theories theories))))

(defun prove-usingchain (theoryname retry? &optional exclude use-default-dp?)
  (prove-importchain theoryname retry? exclude use-default-dp?))

(defun prove-importchain (theoryname retry? &optional exclude use-default-dp? save-proofs?)
  (let ((theory (get-typechecked-theory theoryname))
	(*use-default-dp?* use-default-dp?))
    (when theory
      (let ((*current-context* (context theory)))
	(prove-theories theoryname
			(remove-if #'(lambda (th)
				       (or (from-prelude? th)
					   (lib-datatype-or-theory? th)))
			  (collect-theory-usings theoryname exclude))
			retry?
			use-default-dp?
			save-proofs?)))
    (status-proof-importchain theoryname)))


(defun prove-proofchain (filename declname line origin retry?
				  &optional use-default-dp?)
  (let ((fdecl (formula-decl-to-prove filename declname line origin)))
    (cond ((null fdecl)
	   (pvs-message "Unable to find formula declaration"))
	  ((null (justification fdecl))
	   (pvs-message "~a has no proof" (id fdecl)))
	  (t (let ((*use-default-dp?* use-default-dp?))
	       (prove-proofchain-decl fdecl retry?))))))

(defun prove-proofchain-decl (fdecl retry? &optional save-proofs?)
  (let ((decls-tried nil) (total 0) (proved 0) (unfin 0)
	(realtime 0) (runtime 0))
    (read-strategies-files)
    (labels ((ppd (decl)
	       (unless (memq decl decls-tried)
		 (let ((*justifications-changed?* nil))
		   (push decl decls-tried)
		   (incf total)
		   (pvs-prove-decl decl retry?)
		   (incf realtime (or (real-proof-time decl) 0))
		   (incf runtime (or (run-proof-time decl) 0))
		   (if (unproved? decl) (incf unfin) (incf proved))
		   (when (and save-proofs? *justifications-changed?*)
		     (save-all-proofs (module decl))))
		 (when (justification decl)
		   (ppds (provable-formulas (get-proofchain decl))))))
	     (ppds (decls)
	       (when decls
		 (ppd (car decls))
		 (ppds (cdr decls)))))
      (ppd fdecl))
    (pvs-buffer "PVS Status"
      (with-output-to-string (out)
	(format out "~2%Proof summary for proofchain for ~a.~a"
	  (id (module fdecl)) (id fdecl))
	(let* ((maxtime (reduce #'max decls-tried
				:key #'(lambda (d)
					 (or (run-proof-time d) 0))
				:initial-value 0))
	       (statuslength 20)
	       (dplength (+ (apply #'max
			      (mapcar #'(lambda (x) (length (string x)))
				*decision-procedures*))
			    2))
	       (timelength (length (format nil "~,2f" maxtime)))
	       (idlength (- 79 4 statuslength dplength timelength 4)))
	  (dolist (decl decls-tried)
	    (format out "~%    ~v,1,0,'.a~19a [~a](~a s)"
	      idlength
	      (id decl)
	      (proof-status-string decl)
	      (if (justification decl)
		  (decision-procedure-used decl)
		  "Untried")
	      (if (run-proof-time decl)
		  (format nil "~v,2f" timelength (run-proof-time decl))
		  (format nil "~v<n/a~>" timelength))))
	  (format out "~%    Totals: ~d formulas, ~d attempted, ~d succeeded ~
                       (~,2,-3f s)"
	    total (+ proved unfin) proved runtime)))
      t)))

(defun pvs-prove-decl (decl retry?)
  (let ((*current-context* (context (module decl))))
    (cond ((and (or (justification decl)
		    (eq (kind decl) 'tcc))
		(or retry?
		    (unproved? decl)))
	   (let ((*rerunning-proof* (format nil "Proving ~a.~a"
				      (id (current-theory)) (id decl)))
		 (*rerunning-proof-message-time* (get-internal-real-time))
		 (orig-status (proof-status decl)))
	     (setf (proof-status decl) 'unproved)
	     (prog1
		 (cond ((justification decl)
			(pvs-message "Rerunning proof of ~a.~a"
			  (id (module decl)) (id decl))
			(let ((pstat (rerun-prove decl)))
			  (pvs-message
			      "~a.~a ~aproved in ~,2,-3f real, ~,2,-3f cpu seconds"
			    (id (module decl)) (id decl) (if (unproved? decl) "un" "")
			    (real-proof-time decl) (run-proof-time decl))
			  (when (eq (proof-status decl) 'unproved)
			    (setf (proof-status decl) 'unfinished))
			  (update-context-proof-status decl)
			  pstat))
		       (t (pvs-message "Proving ~a..." (id decl))
			  (prove-tcc decl)))
	       (unless (eq (proof-status decl) orig-status)
		 (setq *justifications-changed?* t)))))
	  ((or (justification decl)
	       (not (unproved? decl)))
	   (pvs-message "~a is already proved" (id decl))
	   nil)
	  (t (pvs-message "~a has no proof" (id decl))
	     nil))))

(defvar *proofchain-formulas*)
(defvar *proofchain-decls*)

(defun get-proofchain (fdecl)
  (let ((*proofchain-formulas* nil)
	(*proofchain-decls* nil))
    (get-proofchain* fdecl)
    (cons fdecl
	  (pc-sort (remove fdecl *proofchain-formulas*)
		   (module fdecl)))))

(defmethod get-proofchain* ((decls list))
  (get-proofchain* (car decls))
  (get-proofchain* (cdr decls)))

(defmethod get-proofchain* ((decls null))
  nil)

(defmethod get-proofchain* ((decl formula-decl))
  (unless (memq decl *proofchain-formulas*)
    (push decl *proofchain-formulas*)
    (unless (from-prelude? decl)
      (get-proofchain* (refers-to decl))
      (unless (or (axiom? decl) (assumption? decl))
	(get-proofchain* (proof-refers-to decl)))
      (get-proofchain* (generated decl)))))

(defmethod get-proofchain* ((decl declaration))
  (unless (or (from-prelude? decl)
	      (memq decl *proofchain-decls*))
    (push decl *proofchain-decls*)
    (get-proofchain* (generated decl))))

(defmethod get-proofchain* (decl)
  (declare (ignore decl))
  nil)

(defun prove-theories (name theories retry? &optional use-default-dp? save-proofs?)
  (let ((total 0) (proved 0) (realtime 0) (runtime 0)
	(*use-default-dp?* use-default-dp?))
    (read-strategies-files)
    (dolist (theory theories)
      ;; Added by C.Munoz
      (pvs-message "Proving ~a" (id theory))
      (let ((*justifications-changed?* nil))
	(dolist (d (provable-formulas theory))
	  (incf total)
	  (pvs-prove-decl d retry?)
	  (when (real-proof-time d)
	    (incf realtime (real-proof-time d)))
	  (when (run-proof-time d)
	    (incf runtime (run-proof-time d)))
	  (unless (unproved? d) (incf proved)))
	(when (and save-proofs? (null retry?) *justifications-changed?*)
	  (save-all-proofs (current-theory)))))
    (pvs-message
	"~a: ~d proofs attempted, ~d proved in ~,2,-3f real, ~,2,-3f cpu seconds"
      name total proved realtime runtime)))



;;; Editing proofs

;(defun pprint-proof-script (s list)
;  (pprint-logical-block (s list :prefix "(" :suffix ")")
;    (pprint-exit-if-list-exhausted)
;    (loop (write (pprint-pop) :stream s)
;	  (pprint-exit-if-list-exhausted)
;	  (write-char #\space s)
;	  (write (pprint-pop) :stream s)
;	  (pprint-newline :linear s))))

(defun edit-proof-at (fileref declname line origin buffer
		      prelude-offset full-label)
  (with-pvs-file (filename) fileref
    (multiple-value-bind (fdecl place)
	(formula-decl-to-prove filename declname line origin)
      (when fdecl
	(setq *edit-proof-info* (list fdecl place buffer prelude-offset)))
      (cond ((and fdecl (justification fdecl))
	     (let ((*current-context* (context fdecl)))
	       (pvs-buffer "Proof"
		 (get-proof-script-output-string fdecl full-label)
		 'popto)))
	    (fdecl
	     (pvs-buffer "Proof" " " 'popto)
	     (pvs-message "Formula ~a has no proof to edit"
	       (id fdecl)))
	    (t (pvs-message "Unable to find formula declaration"))))))

(defun get-proof-script-output-string (fdecl &optional full-label)
  (with-output-to-string (out)
    (format out ";;; Proof ~a for formula ~a.~a~%"
      (id (default-proof fdecl)) (id (module fdecl)) (id fdecl))
    ;; (format out
    ;; 	";;; developed with ~a decision procedures~%"
    ;;   (decision-procedure-used fdecl))
    (write (editable-justification (justification fdecl)
				   nil nil (when full-label ""))
	   :stream out :pretty t :escape t
	   :level nil :length nil
	   :pprint-dispatch *proof-script-pprint-dispatch*)))


(defun install-proof (tmpfilename name declname line origin buffer prelude-offset)
  ;; If the origin is supplied, simply install the proof.  Otherwise the
  ;; proof is being installed from the Proof buffer, and the declaration
  ;; is gotten from *edit-proof-info*, in this case ask before installing.
  (when (or origin
	    (if *edit-proof-info*
		(prog1 (pvs-y-or-n-p "Install proof on formula ~a? "
				     (id (car *edit-proof-info*)))
		  (pvs-message ""))
		(pvs-message "No proof is being edited.")))
    (let ((sexpr (ignore-errors (with-open-file (in tmpfilename)
				  (read-preserving-comments-as-strings in)))))
      (unless (listp sexpr)
	(justification-error sexpr sexpr "Proof must be a list"))
      (multiple-value-bind (msg subexpr)
	  (check-edited-justification (remove-leading-comments sexpr))
	(if subexpr
	    (justification-error subexpr sexpr msg)
	    (let ((just (revert-justification
			 (complete-checkpointed-proof sexpr))))
	      (multiple-value-bind (fdecl place)
		  (if origin
		      (formula-decl-to-prove name declname line origin)
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
		      ((some #'(lambda (prinfo)
				 (equal (script prinfo) just))
			     (proofs fdecl))
		       (let ((prinfo (find-if #'(lambda (prinfo)
						  (equal (script prinfo) just))
				       (proofs fdecl))))
			 (setf (default-proof fdecl) prinfo)
			 (unless (from-prelude? (module fdecl))
			   (save-all-proofs (module fdecl)))
			 (pvs-message "Proof already found on ~a as ~a"
			   (id fdecl) (id prinfo))
			 t))
		      (t (let ((prinfo
				(if (tcc-decl? fdecl)
				    (make-tcc-proof-info
				     just
				     (next-proof-id fdecl)
				     (when (default-proof
					       (car *edit-proof-info*))
				       (description
					(default-proof
					    (car *edit-proof-info*))))
				     (origin fdecl))
				    (make-proof-info
				     just
				     (next-proof-id fdecl)
				     (when (default-proof
					       (car *edit-proof-info*))
				       (description
					(default-proof
					    (car *edit-proof-info*))))))))
			   (push prinfo (proofs fdecl))
			   (setf (default-proof fdecl) prinfo)
			   (unless (from-prelude? (module fdecl))
			     (save-all-proofs (module fdecl)))
			   (pvs-message "Proof installed on ~a as ~a"
			     (id fdecl) (id prinfo)))
			 t)))))))))

(defun read-preserving-comments-as-strings (stream)
  (let ((*readtable* (copy-readtable)))
    (set-macro-character #\; #'pvs-lisp-comment-reader t)
    (read stream t nil t)))

(defun pvs-lisp-comment-reader (stream char)
  (pvs-lisp-read-comment stream (list char)))

(defun pvs-lisp-read-comment (stream chars)
  (let ((ch (read-char stream nil 'eof t)))
    (if (char= ch #\Newline)
	(cons (coerce (nreverse chars) 'string)
	      (read-preserving-comments-as-strings stream))
	(pvs-lisp-read-comment stream (cons ch chars)))))

(defun remove-leading-comments (sexpr)
  (if (and (stringp (car sexpr))
	   (plusp (length (car sexpr)))
	   (char= (char (car sexpr) 0) #\;))
      (remove-leading-comments (cdr sexpr))
      sexpr))


;;; Called from install-proof by Emacs while in an edit proof buffer.  The
;;; *edit-proof-info* variable must have previously been set by
;;; edit-proof-at.

(defun prove-proof-at (line step? display?)
  (declare (ignore line))
  (let* ((fdecl (car *edit-proof-info*))
	 (*current-context* (context fdecl)))
    (read-strategies-files)
    (auto-save-proof-setup fdecl)
    (let ((*start-proof-display* display?))    
      (setq *last-proof*
	    (if step?
		(prove fdecl)
		(prove fdecl :strategy '(rerun)))))
    ;; Save the proof.
    (unless (from-prelude? fdecl)
      (save-all-proofs (current-theory))
      ;; If the proof status has changed, update the context.
      (update-context-proof-status fdecl))
    (remove-auto-save-proof-file)
    (when (default-proof fdecl)
      (setf (interactive? (default-proof fdecl)) t))
    (let* ((*to-emacs* t)
	   (place (second *edit-proof-info*))
	   (buffer (third *edit-proof-info*))
	   (prelude-offset (fourth *edit-proof-info*)))
      (pvs-locate buffer fdecl
		  (if (and prelude-offset
			   (> prelude-offset 0))
		      (vector (- (line-begin place) prelude-offset)
			      (col-begin place)
			      (- (line-end place) prelude-offset)
			      (col-end place))
		      place)))))
		   

(defun remove-proof-at (name declname line origin)
  (let ((fdecl (formula-decl-to-prove name declname line origin)))
    (cond ((and fdecl (default-proof fdecl))
	   (let ((prf (default-proof fdecl)))
	     (setf (proofs fdecl) (delete prf (proofs fdecl)))
	     (setf (default-proof fdecl) (car (proofs fdecl)))
	     (when (tcc? fdecl)
	       (setf (tccs-tried? (module fdecl)) nil))
	     (update-context-proof-status fdecl)
	     (save-all-proofs (current-theory))
	     (pvs-message "Proof ~a removed from ~a"
	       (id prf) (id fdecl))))
	  (fdecl
	   (pvs-message "Formula ~a has no proof to remove" (id fdecl)))
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
	(*from-buffer* "Proof")
	(err (format nil "~a - ~s" (or msg "Proof syntax error") subexpr)))
    (pvs-error "Proof error" err
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
    (vector row col)))


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
  (let ((id (if (stringp modname) (intern modname :pvs) modname)))
    (if (gethash id (current-pvs-theories))
	(progn ;(pvs-message "Theory already exists")
	       nil)
	(namestring (make-pathname :name modname :type "pvs"
				   :defaults *default-pathname-defaults*)))))


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
	  (remhash tid (current-pvs-theories)))))
    (remhash filename (current-pvs-files))
    (delete-file-from-workspace filename))
  (when delete-file?
    (delete-file (make-specpath filename)))
  (if delete-file?
      (pvs-message "~a has been deleted" filename)
      (pvs-message "~a has been removed from the workspace (the file is still there)" filename)))

(defun delete-theory (theoryref)
  (let ((theory (gethash (ref-to-id theoryref) (current-pvs-theories))))
    (when theory
      (copy-theory-proofs-to-orphan-file theoryref)
      (untypecheck-usedbys theory)
      (remhash (id theory) (current-pvs-theories))
      (setf (gethash (filename theory) (current-pvs-files))
	    (remove theory (gethash (filename theory) (current-pvs-files)))))))


;;; List Theories

(defun lt (&optional context)
  (list-theories context))

(defun list-theories (&optional context)
  (if (or (null context)
	  (file-equal context *default-pathname-defaults*))
      (let ((theories nil))
	(maphash #'(lambda (id mod)
		     (declare (ignore mod))
		     (push (string id) theories))
		 (current-pvs-theories))
	(sort theories #'string<))
      (let ((path (make-pathname :defaults context
				 :name "context" :type "cxt")))
	(if (file-exists-p path)
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

(defun quit (&optional (status 0))
  (when (y-or-n-p "Do you really want to kill the PVS process? ")
    (cl-user:bye status)))

(defun exit-pvs (&optional dont-ask)
  (multiple-value-bind (ignore condition)
      (ignore-errors (save-context))
    (declare (ignore ignore))
    (if (and condition (not dont-ask))
	(progn
	  (if (pvs-yes-or-no-p "Problem saving context - ~a~%Exit anyway? "
			       condition)
	      (cl-user:bye)
	      (error "Exit aborted")))
	(cl-user:bye))))

;;; PVS Version

(defun pvs-version ()
  (pvs-message "PVS Version ~a" *pvs-version*))


;;; help-prover

(defun help-prover (&optional name)
  (let ((rule (if (stringp name)
		  (intern #+allegro (string-downcase name)
			  #-allegro (string-upcase name)
			  :pvs)
		  '*))
	(*disable-gc-printout* t))
    (pvs-buffer "Prover Help"
      (with-output-to-string (*standard-output*)
	(funcall (help-rule-fun rule) nil))
      'temp t)))

;;; Misc functions

;;; get-parsed-theory gets the parsed theory, but will not save the context
;;; (last argument to parse-file)

(defun get-parsed-theory (theoryref &optional quiet? typecheck?)
  "Get a parsed theory corresponding to the theoryref.  First checks if the
theory is already parsed, and returns it if so.  Then tries to find a unique
file containing a theory matching theoryref, parses it, and returns the
contained theory.  If quiet? is nil then errors are signaled, otherwise
errors are quietly ignored, and nil is returned in that case."
  (let ((mod (get-theory theoryref)))
    (when (and mod
	       (filename mod)
	       (gethash (filename mod) (current-pvs-files))
	       (not (file-exists-p (make-specpath (filename mod)))))
      (pvs-message "File ~a.pvs has disappeared!" (filename mod))
      (remhash (filename mod) (current-pvs-files))
      (remhash (id mod) (current-pvs-theories))
      (delete-file-from-workspace (filename mod))
      (setq mod nil))
    (cond ((and mod (gethash (id mod) *prelude*))
	   mod)
	  ((and mod
		(parsed? mod)
		(or (not (lib-datatype-or-theory? mod))
		    (and (name? theoryref)
			 (library theoryref))))
	   mod)
	  ((and (name? theoryref)
		(library theoryref))
	   (let ((lth (get-parsed-library-theory theoryref)))
	     (unless lth (break "no lth?"))
	     lth))
	  ((and mod (filename mod))
	   (parse-file (filename mod) nil t typecheck?)
	   (get-theory theoryref))
	  (t (let ((filename (context-file-of theoryref)))
	       (if (and filename (file-exists-p (make-specpath filename)))
		   (parse-file filename nil quiet? typecheck?)
		   (if (file-exists-p (make-specpath theoryref))
		       (parse-file theoryref nil quiet? typecheck?)
		       (let ((file
			      (look-for-theory-in-directory-files theoryref)))
			 (if file
			     (parse-file file nil quiet? typecheck?)
			     (parse-file theoryref nil quiet? typecheck?)))))
	       (let ((pmod (get-theory theoryref)))
		 (or pmod
		     (unless quiet?
		       (type-error theoryref
			 "Can't find file for theory ~a" theoryref)))))))))

(defun look-for-theory-in-directory-files (theoryref)
  (let* ((thname (ref-to-id theoryref))
	 (grep-form (format nil "grep -l -w ~a *.pvs" thname))
	 (grep-files
	  (uiop:run-program grep-form
	    :output '(:string :stripped t)
	    :ignore-error-status t))
	 (pvs-files (uiop:split-string grep-files :separator (list #\newline)))
	 (files-with-clashes nil)
	 (files-with-theoryref nil))
    (dolist (file pvs-files)
      (let ((fname (pathname-name file)))
	(unless (parsed-file? fname)
	  ;; More refined search - strips comments first
	  (let* ((sed-grep-form (format nil "sed 's/%.*//g' ~a | grep -q -w ~a" file thname))
		 (found (zerop (nth-value 2
				 (uiop:run-program sed-grep-form :ignore-error-status t)))))
	    (when found
	      (let ((theories (with-no-parse-errors (parse :file file))))
		(when (member thname theories :key #'id)
		  ;; Make sure we're not introducing a name clash
		  ;; E.g., file1 has theories th1 and th2
		  ;;       file2 has theories th2 and th3
		  ;; and we're looking for th3 from file1.
		  (if (some #'(lambda (th)
				(let ((cth (gethash (id th) (current-pvs-theories))))
				  (and cth
				       (filename cth)
				       (not (string= fname (filename cth))))))
			    theories)
		      (push fname files-with-clashes)
		      (push fname files-with-theoryref)))))))))
    (cond ((null files-with-theoryref)
	   (when files-with-clashes
	     (type-error theoryref
	       "Theory ~a appears in other files:~%  ~{~a~^, ~}~
              ~%but other theories in those files clash with current theories."
	       theoryref files-with-clashes)))
	  ((cdr files-with-theoryref)
	   (type-error theoryref
	     "Theory ~a appears in more than one file:~%  ~{~a~^, ~}~
              ~%pick one and typecheck it."
	     theoryref files-with-theoryref))
	  (t (car files-with-theoryref)))))

(defun get-parsed-library-theory (theoryname)
  (load-imported-library (library theoryname) theoryname))

(defun get-parsed?-theory (theoryref)
  (let ((theory (get-theory theoryref)))
    (cond ((null theory)
	   (pvs-message "~a is unknown in this context." theoryref))
	  ((gethash (id theory) *prelude*)
	   theory)
	  ((parsed? theory)
	   theory)
	  (t (pvs-message "~a has not been parsed." theoryref)))))

(defmethod get-typechecked-theory ((th datatype-or-module) &optional theories quiet?)
  (declare (ignore theories quiet?))
  th)

(defmethod get-typechecked-theory ((theoryref string) &optional theories quiet?)
  "Theoryref may be a URI of the form [pvsfile '#'] thname"
  (with-pvs-file (fname thname) theoryref
    (if thname
	(let ((theories (typecheck-file fname)))
	  (car (member thname theories :key #'id :test #'string=)))
	(get-typechecked-theory (pc-parse theoryref 'modname) theories quiet?))))

(defmethod get-typechecked-theory ((theoryref symbol) &optional theories quiet?)
  (get-typechecked-theory (mk-modname theoryref) theories quiet?))

(defmethod get-typechecked-theory ((thname modname) &optional theories quiet?)
  ;;(when (eq (id thname) 'Sigma_l_adt) (break "Sigma_l_adt"))
  (if (library thname)
      (let ((lib-path (get-library-path (library thname))))
	(with-workspace lib-path
	  (get-typechecked-theory (copy thname 'library nil) theories quiet?)))
      (if (and (resolution thname)
	       (theory-reference? (declaration thname)))
	  (get-typechecked-theory (theory-name (declaration thname)))
	  (or (and (or *in-checker*
		       *generating-adt*
		       (and *current-context*
			    (eq *current-context* *working-current-context*)))
		   (get-theory thname))
	      (let ((theory (get-parsed-theory thname t t)))
		(when theory
		  (unless (or *in-checker*
			      (typechecked? theory)
			      (memq theory theories))
		    (let ((*generating-adt* nil)
			  (*insert-add-decl* t))
		      (typecheck-file (filename theory))))
		  #+pvsdebug (assert (typechecked? theory))
		  (unless (or (from-prelude? theory)
			      (check-binfiles (filename theory)))
		    (setf (pvs-context-changed *workspace-session*) t)))
		theory)))))

(defun parsed-date (filename)
  (car (gethash (pathname-name filename) (current-pvs-files))))

(defun reset-parsed-date (filename)
  (let ((path (make-specpath filename)))
    (when (gethash filename (current-pvs-files))
      (setf (car (gethash filename (current-pvs-files)))
	    (file-write-time path)))
    nil))

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
	   (*current-context* (saved-context theory))
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
	     (current-pvs-theories))
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
    (do-all-lib-theories
	#'(lambda (th)
	    (setq decls
		  (append (remove-if-not
			      #'(lambda (d)
				  (and (declaration? d)
				       (eq (ref-to-id ref)
					   (id d))))
			    (all-decls th))
			  decls))))
    (delete-duplicates decls :test #'eq)))

(defun get-typechecked-theories ()
  (let ((theories nil))
    (maphash #'(lambda (thid th)
		 (declare (ignore thid))
		 (push th theories))
	     (current-pvs-theories))
    theories))

(defun get-all-current-tccs ()
  (let ((tccs nil))
    (dolist (th (get-typechecked-theories))
      (dolist (decl (all-decls th))
	(when (tcc? decl)
	  (push decl tccs))))
    tccs))

(defun tcc-conclusion (tcc)
  (tcc-conclusion* (definition tcc) nil))

(defmethod tcc-conclusion* ((ex forall-expr) last-impl)
  (tcc-conclusion* (expression ex) last-impl))

(defmethod tcc-conclusion* ((ex implication) last-impl)
  (declare (ignore last-impl))
  (tcc-conclusion* (args2 ex) (args2 ex)))

(defmethod tcc-conclusion* ((ex expr) last-impl)
  last-impl)

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
    (let ((*current-context* (context theory)))
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

(defmethod id-place (name)
  (let* ((row (starting-row (place name)))
	 (scol (starting-col (place name)))
	 (ecol (+ scol (length (string (id name))))))
    (vector row scol row ecol)))

(defmethod id-place ((ex fieldappl))
  (let* ((row (ending-row (place ex)))
	 (ecol (ending-col (place ex)))
	 (scol (- ecol (length (string (id ex))))))
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

(defun precedes-place (pos place)
  (assert (vectorp place))
  (and (<= (car pos) (starting-row place))
       (if (= (car pos) (starting-row place))
	   (<= (cadr pos) (starting-col place))
	   t)))

(defun follows-place (pos place)
  (assert (vectorp place))
  (and (>= (car pos) (ending-row place))
       (if (= (car pos) (ending-row place))
	   (>= (cadr pos) (ending-col place))
	   t)))

(defun within-place (pos place)
  (assert (vectorp place))
  (and (<= (starting-row place) (car pos) (ending-row place))
       (or (not (= (starting-row place) (car pos)))
	   (<= (starting-col place) (cadr pos)))
       (or (not (= (ending-row place) (car pos)))
	   (< (cadr pos) (ending-col place)))))

(defun show-last-proof (&optional terse?)
  (if *last-proof*
      (pvs-buffer "Proof Display"
	(with-output-to-string (*standard-output*)
	  (let ((*disable-gc-printout* t)
		(*prover-indent* *prover-indent*)
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
      (let ((*disable-gc-printout* t))
	(pvs-buffer "Expanded Sequent"
	  (with-output-to-string (*standard-output*)
	    (unless all?
	      (format t ";;; Expanding up to differences (e.g., actuals or theory); ")
	      (format t "C-u M-x show-expanded-sequent fully expands~%"))
	    (let ((exp-seq (if all?
			       (expanded-sequent all?)
			       (create-distinct-names-sequent)))) 
	      (write exp-seq)))
	  t))
      (pvs-message "Not in prover")))

(defun create-distinct-names-sequent ()
  (copy *ps*
    'current-goal
    (copy (current-goal *ps*)
      's-forms (create-distinct-names
		(s-forms (current-goal *ps*))))))

(defun expanded-sequent (&optional all?)
  (let ((*parsing-or-unparsing* t))
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
      (let ((skoconsts (collect-skolem-constants))
	    (*disable-gc-printout* t))
	(if skoconsts
	    (pvs-buffer "Proof Display"
	      (with-output-to-string (*standard-output*)
		(format t "~%Skolem-constant: type [= defn]")
		(format t "~%------------------------------")
		(dolist (sc skoconsts)
		  (let* ((decl (format nil "~a: ~a" (id sc) (type sc)))
			 (def (when (definition sc)
				(unpindent (definition sc) 5 :string t))))
		    (format t "~%~a~@[~%   = ~a~]" decl def))))
	      t)
	    (pvs-message "No Skolem Constants on this branch of the proof")))
      (pvs-message "Not in the prover")))

(defun collect-skolem-constants ()
  ;; No need to look in prelude
  (let ((dht (lhash-table (current-declarations-hash)))
	(skoconsts nil))
    (maphash #'(lambda (id decls)
		 (declare (ignore id))
		 (dolist (d decls)
		   (when (typep d 'skolem-const-decl)
		     (pushnew d skoconsts))))
	     dht)
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

(defun collect-strategy-names (&optional all?)
  (with-open-file (*standard-output* "/dev/null" :direction :output
				     :if-exists :overwrite)
    (with-open-file (*error-output* "/dev/null" :direction :output
				    :if-exists :overwrite)
      (read-strategies-files)))
  (let ((names nil))
    (maphash #'(lambda (n s)
		 (push (string-downcase (string n)) names))
	     *rulebase*)
    (maphash #'(lambda (n s)
		 (unless (and (not all?)
			      (defhelper-entry? s))
		   (push (string-downcase (string n)) names)))
	     *rules*)
    (maphash #'(lambda (n s)
		 (unless (or (and (not all?)
				  (defhelper-entry? s))
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
    (let* ((ctheory (if theory-name
			(get-typechecked-theory theory-name)
			(car (last *prelude-theories*))))
	   (*current-context* (or context (copy (context ctheory))))
	   (*generate-tccs* 'none)
	   (*from-buffer* "Formula Decl"))
      (pvs-buffer *from-buffer* formula-decl nil t)
      (let* ((pdecl (pc-parse formula-decl 'theory-elt))
	     (fdecl (if (listp pdecl) (car pdecl) pdecl)))
	(unless (typep fdecl 'formula-decl)
	  (type-error fdecl "Not a formula declaration"))
	(typecheck-decl fdecl)
	(let ((*generate-xref-declaration* fdecl)
	      (*xref-names-seen* nil)
	      (*xref-types-seen* nil))
	  (generate-xref fdecl))
	(setq *typecheck-formula-decl*
	      (list formula-decl
		    theory-name
		    (when (and theory-name
			       (not (from-prelude? (current-theory))))
		      (parsed-date (filename (current-theory))))
		    fdecl)))))
  (pvs-message "Formula typechecked")
  (fourth *typecheck-formula-decl*))

(defvar *prove-formula-proof* nil)

(defvar *prove-formula-proved?* nil)

(defun prove-formula-decl (formula-decl &optional theory-name strategy)
  (let* ((*to-emacs* t)
	 (ctheory (if theory-name
		      (get-typechecked-theory theory-name)
		      (car (last *prelude-theories*))))
	 (*current-context* (copy (context ctheory)))
	 (fdecl (typecheck-formula-decl formula-decl theory-name))
	 (*collecting-tccs* t)
	 (*tccforms* nil))
    (typecheck (definition fdecl) :expected *boolean* :tccs 'all)
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
	     theory-name)))
    (setq *prove-formula-proof* nil)
    (when strategy
      (multiple-value-bind (strat err)
	  (if (stringp strategy)
	      (ignore-errors (values (read-from-string strategy)))
	      strategy)
	(let ((just (unless err
		      (or (and (listp strat)
			       (eq (car strat) "")
			       (revert-justification strat))
			  (revert-justification (list "" strat))
			  strat))))
	  (unless just
	    (type-error strategy "Bad form for strategy~%  ~s" strategy))
	  (setf (justification fdecl) just))))
    (setq *to-emacs* nil)
    (read-strategies-files)
    (unwind-protect
	(let ((proof (prove-decl fdecl :strategy (when strategy
						   '(then (rerun) (quit))))))
	  (setq *prove-formula-proved?*
		(eq (status-flag proof) '!))
	  (setq *prove-formula-proof*
		(editable-justification
		 (extract-justification-sexp (justification proof)))))
      (pvs-emacs-eval "(pvs-ready)"))))

(defun get-prove-formula-proof ()
  *prove-formula-proof*)

  

(defun show-strategy (strat-name)
  (let* ((strat-id (intern #+allegro (string-downcase strat-name)
			   #-allegro (string-upcase strat-name)
			   :pvs))
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
  (format out "~(~a~) is a primitive rule" (name strat))
  #+allegro
  (let ((source (cdr (assq :strategy (excl:source-file (name strat) t)))))
    (when source
      (format out " (defined in ~a)" source)))
  (format out ":~2%Arguments: ~(~a~)"
    (if (optional-args strat)
	(append (required-args strat)
		(cons '&optional (optional-args strat)))
	(required-args strat)))
  (format out "~2%Definition: A compiled lisp function")
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defmethod show-strategy* ((strat defrule-entry) out)
  (format out "~(~a~) is a ~:[strategy~;defined rule~]"
    (name strat) (gethash (name strat) *rules*))
  #+allegro
  (let ((source (cdr (assq :strategy (excl:source-file (name strat) t)))))
    (when source
      (format out " (defined in ~a)" source)))
  (format out ":~2%Arguments: ~(~a~)" (formals strat))
  (format out "~2%Definition: ~%")
  (write (defn strat) :stream out :pretty t :level nil :length nil)
  (format out ":~2%Original Arguments: ~(~a~)" (source-formals strat))
  (format out "~2%Original Definition: ~%")
  (write (source-defn strat) :stream out :pretty t :level nil :length nil)
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defmethod show-strategy* ((strat defstep-entry) out)
  (format out "~(~a~) is a ~:[strategy~;defined rule~]"
    (name strat) (gethash (name strat) *rules*))
  #+allegro
  (let ((source (cdr (assq :strategy (excl:source-file (name strat) t)))))
    (when source
      (format out " (defined in ~a)" source)))
  (format out ":~2%Arguments: ~(~a~)" (formals strat))
  (format out "~2%Definition: ~%")
  (write (defn strat) :stream out :pretty t :level nil :length nil)
  (format out ":~2%Original Arguments: ~(~a~)" (source-formals strat))
  (format out "~2%Original Definition: ~%")
  (write (source-defn strat) :stream out :pretty t :level nil :length nil)
  (format out "~2%Format string: ~s" (format-string strat))
  (format out "~2%Documentation: ~%~a" (docstring strat))
  #+lucid
  (let ((file (ignore-errors (get-source-file (name strat) 'strategy))))
    (when file
      (format out "~2%Defined in file: ~s" (namestring file)))))

(defun prelude-stats ()
  (let ((consts 0) (defs 0) (recs 0) (inds 0) (coinds 0) (utdecls 0)
	(tdecls 0) (tcc-decls 0) (ass-decls 0) (subjdgs 0) (jdgs 0) (convs 0)
	(fdecls 0) (axioms 0) (postulates 0))
    (do-all-declarations
     #'(lambda (d)
	 (typecase d
	   (def-decl (incf recs))
	   (inductive-decl (incf inds))
	   (coinductive-decl (incf coinds))
	   (const-decl (if (definition d)
			   (incf defs)
			   (incf consts)))
	   (type-def-decl (incf tdecls))
	   (type-decl (incf utdecls))
	   (tcc-decl (incf tcc-decls))
	   (assuming-decl (incf ass-decls))
	   (formula-decl (case (spelling d)
			   (AXIOM (incf axioms))
			   (POSTULATE (incf postulates))
			   (t (incf fdecls))))
	   (subtype-judgement (incf subjdgs))
	   (judgement (incf jdgs))
	   (conversionminus-decl nil)
	   (conversion-decl (incf convs))))
     (declarations-hash *prelude-context*))
    (format t "~%~10d uninterpreted type declarations~
               ~%~10d defined type declarations~
               ~%~10d uninterpreted constants~
               ~%~10d defined constants~
               ~%~10d recursive definitions~
               ~%~10d inductive definitions~
               ~%~10d coinductive definitions~
               ~%~10d subtype judgements~
               ~%~10d constant judgements~
               ~%~10d conversions~
               ~%~10d TCCs~
               ~%~10d axioms~
               ~%~10d postulates~
               ~%~10d formulas"
      utdecls tdecls consts defs recs inds coinds subjdgs jdgs convs
      tcc-decls axioms postulates fdecls)))

(defun collect-format-strings ()
  (with-open-file (*standard-output* "/dev/null" :direction :output
				     :if-exists :overwrite)
    (read-strategies-files))
  (let ((format-strings nil))
    (maphash #'(lambda (n s)
		 (declare (ignore n))
		 (unless (string= (format-string s) "")
		   (push (format-string s) format-strings)))
	     *rulebase*)
    (maphash #'(lambda (n s)
		 (declare (ignore n))
		 (unless (string= (format-string s) "")
		   (push (format-string s) format-strings)))
	     *rules*)
    (maphash #'(lambda (n s)
		 (declare (ignore n))
		 (unless (string= (format-string s) "")
		   (push (format-string s) format-strings)))
	     *steps*)
    format-strings))

(defun do-all-strategies (fn)
  (maphash #'(lambda (id entry)
	       (declare (ignore id))
	       (funcall fn entry))
	   *rulebase*)
  (maphash #'(lambda (id entry)
	       (declare (ignore id))
	       (funcall fn entry))
	   *rules*)
  (maphash #'(lambda (id entry)
	       (declare (ignore id))
	       (funcall fn entry))
	   *steps*))

(defmethod formals ((rule rule-entry))
  (append (required-args rule)
	  (when (optional-args rule)
	    (if (eq (car (optional-args rule)) '&rest)
		(optional-args rule)
		(cons '&optional (optional-args rule))))))

;;; Allows help to be used outside the prover.
(defmacro help (&optional name)
  `(progn (funcall (funcall #'help-rule-fun ',name) nil) nil))

(defun used-prelude-theory-names (theory &optional prelude-theory-names)
  (let ((th (get-theory theory)))
    (unless th
      (error "Theory ~a not found - may need to typecheck first" theory))
    (dolist (decl (all-decls th))
      (when (typep decl '(or type-decl const-decl))
	(dolist (d (refers-to decl))
	  (when (and (typep d '(or type-decl const-decl))
		     (from-prelude? d))
	    (unless (or (eq (module d) th)
			(member (module d) prelude-theory-names :test #'same-id))
	      (setq prelude-theory-names
		    (used-prelude-theory-names (module d)
					       (cons (mk-modname (id (module d)))
						     prelude-theory-names))))))))
    prelude-theory-names))

(defun collect-prelude-decls-if (pred)
  (let ((decls nil))
    (dolist (th *prelude-theories*)
      (dolist (decl (all-decls th))
	(when (funcall pred decl)
	  (push decl decls))))
    (nreverse decls)))

(defun proof-refers-to? (decl id)
  (when (and (formula-decl? decl)
	     (default-proof decl))
    (and (member id (refers-to (default-proof decl))
		 :test #'same-id)
	 t)))

(defun get-theory-api (theory)
  "Simply lists all the declarations of a given theory, along with their
types."
  (let* ((th (get-theory theory))
	 (decls (remove-if-not #'(lambda (te)
				   (and (declaration? te) (visible? te)
					(not (formula-decl? te))))
		  (all-decls th))))
    (dolist (d decls)
      (format t "~%~a: ~a" (id d)
	      (typecase d
		(type-decl "TYPE")
		(typed-declaration (type d))
		(t (break "more needed")))))))
