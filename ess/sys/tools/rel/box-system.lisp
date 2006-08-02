;;; -*- Mode: Lisp; Package: TOOLS -*-
;;;
;;; The system-dependent stuff that is used by the box facility.
;;;
;;; Author: Frank Pfenning.  Last Modified Wed Oct 11 01:15:05 1989
;;;
;;; Sccs Id @(#)box-system.lisp	1.18 10/11/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


(in-package :tools) (use-package #+sbcl :common-lisp :ergolisp)
;; For some reason the next line does NOT work in compiled code in Lucid.
;; It is therefore repeated in the init-load file.
#+lucid (import '(system::cd) :tools)
#+allegro (import '(cl-user::cd) :tools)

(export '(user-name))
(export '(cd wdir while-connected-to))	; No longer necessary, but useful.
(export '(getenv windowsystem)) ; Environment queries.
(export '(*time-function* ;; touch-time adjusted-time
	  )) ; For timing

(export '(*load-messages*))
(export '(*compiler-messages*))

(defun user-name ()
  "Name of current user."
  (let ((homedir (pathname-directory (truename (user-homedir-pathname)))))
    (elt homedir (1- (length homedir)))))

(defun getenv (varname)
  "Gets an environment variable.  
Returns a string if defined or nil if undefined."
  #+sbcl (sb-ext:posix-getenv varname)
  #+cmu
  (cdr (assoc (intern varname "KEYWORD") extensions::*environment-list*))
  #+(and lucid lcl3.0) (lucid-common-lisp:environment-variable varname)
  #+(and lucid (not lcl3.0))  (system:environment-variable varname)
  #+allegro (system:getenv varname)
  #+gcl (si:getenv varname)
  #-(or allegro lucid cmu sbcl gcl) nil
  )

#-(or cmu sbcl allegro lucid)
(warn "The function GETENV which is used to determine if you are running
a version of the X Window system is undefined for this Common Lisp
implementation.  This means that a number of features that require the
X-Windows have not been ported to this version of Common Lisp.")

(defun windowsystem ()
  "Nil for none, :x10 or :x11 if either x10 or x11.  
Looks at the DISPLAY environment variable."
  (let ((displayname (getenv "DISPLAY")))
    (cond ((null displayname) nil)
	  ((search "." (subseq displayname (search ":" displayname))) :x11)
	  (t :x10))))

#+lucid
(defun wdir ()
  "Returns the current working directory as a string."
  (system::working-directory))

#+allegro
(defun cd (&optional (pathname (user-homedir-pathname)))
  (excl:chdir pathname))
#+cmu
(defun cd (&optional (pathname (user-homedir-pathname)))
  (unix::unix-chdir pathname))
#+sbcl
(defun cd (&optional (pathname (user-homedir-pathname)))
  (assert (pathnamep pathname))
  (setq *default-pathname-defaults* pathname))
#+allegro
(defun wdir ()
  (excl:current-directory))
#+cmu
(defun wdir ()
  (nth-value 1 (UNIX:UNIX-CURRENT-DIRECTORY)))
#+sbcl
(defun wdir ()
  *default-pathname-defaults*)

#-(or lucid allegro cmu sbcl)
(warn "Functions CD and WDIR not defined for this implementation of Lisp.
These functions are not essential.")

;;; This should now no longer be used.

(defmacro while-connected-to (where &rest body)
  "Connects to a directory, executes the body, and connects back to
the original working directory.  A correctable error is signalled, if the
first cd fails."
  (let ((pwd-var (gensym)))
    `(let ((,pwd-var (wdir)))
       (cd ,where)
       (unwind-protect
	   (progn ,@body)
	 (cd ,pwd-var)))))

#+allegro
(defun has-directory-component (pathname)
  "Returns T, is the given pathname has a directory component, NIL otherwise."
  (not (string= (directory-namestring pathname) "./")))

#-allegro
(defun has-directory-component (pathname)
  "Returns T, is the given pathname has a directory component, NIL otherwise."
  (not (string= (directory-namestring pathname) "")))

#-(or lucid allegro cmu sbcl ibcl kcl)
(warn "The function HAS-DIRECTORY-COMPONENT assumes that the namestring
of a file with no directory component is \"\".  This is not specified
in the standard and may be different in you implementation of Common
Lisp.  To change this, look in the file sys/tools/rel/box-system.lisp.")

(defun directory-only (pathname)
  "Takes a pathname which is supposed to be only a directory and returns
a string with that directory.  Signals a correctable error if the
pathname has a name compononent."
  (when (pathname-name pathname)
    (cerror "Make filename part of directory path."
	    "Directory pathname ~S has a name component."
	    pathname)
    (setq pathname (directorify (namestring pathname))))
  ;;; CMU Common Lisp interprets the standard to mean that
  ;;; probe-file should return NIL on directories, Sun Common Lisp
  ;;; and Allegro return the true directory name instead.
  (let ((truename (probe-file pathname)))
    (cond ((not truename)
	   (cerror "Give new directory"
		   "Directory ~S does not exist."
		   pathname)
	   (format *query-io* "~&New directory (without quotes): ")
	   (directorify (read-line *query-io*)))
	  (t (directory-namestring truename)))))

(defun directorify (pathname)
  "Takes a pathname which is supposed to be only a directory and tries
to coerce it into one.  This often means appending a /."
  (ctypecase pathname
      (pathname (directory-only pathname))
      (string (if (equal (char pathname (1- (length pathname)))
			 #\/)
		  (directory-only pathname)
		  (directory-only (format nil "~A/" pathname))))
      (symbol (directorify (string pathname)))
      (list (directorify (make-pathname :directory pathname)))))

;;; lisp-load is used by the lisp-loader box.
;;; This definition used to be in box-defs.lisp.

(defvar *load-messages* nil)

(defun lisp-load (&key file (lisp-loader t)
		       (messages *load-messages*)
		       (load-source nil)
		       (readtable nil)
		       &allow-other-keys)
  (if lisp-loader
      (if load-source
	  (boxwarn "Loading of file ~S explicitly prohibited." file)
	  (if readtable
	      (let ((*readtable* readtable))
		(load file :verbose messages :print nil))
	      (load file :verbose messages :print nil))
	  ))
  nil)

;;; lisp-load is used by the lisp-loader box.
;;; This definition used to be in box-defs.lisp.
;;; The masking business does not port.  It is the reponsibility of the
;;;  operating system/version control environment to maintain that.

(defvar *compiler-messages* nil)

(defun lisp-compile (&key source-file compiled-file ; should always be given
			  (lisp-compiler t) ; to prevent compiling
			  (messages *compiler-messages*)
			  (boot nil)
			  (readtable nil)
			  &allow-other-keys)
  ":BOOT may be specified as a local option in a box declaration.
Other keys are allowed, but will be ignored."
  (when lisp-compiler
    (when boot
      (load source-file))
    (if readtable
	(let ((*readtable* readtable))
	  (compile-file source-file :output-file compiled-file
	     #+(or lucid allegro) :messages #+(or lucid allegro) messages
	     #+(or cmu sbcl) :progress #+(or cmu sbcl) messages
	     ))
	(compile-file source-file :output-file compiled-file
	     #+(or lucid allegro) :messages #+(or lucid allegro) messages
	     #+(or cmu sbcl) :progress #+(or cmu sbcl) messages
	     ))
    )
  ;;  #+kcl (rename-file (merge-pathnames ".o" source-file) compiled-file)
  nil)

;;; Bug workaround for Lucid 2.1.
;;; This redefines the previous function, but is OK.
#+(and lucid (not lcl3.0))
(defun lisp-compile (&key source-file compiled-file ; should always be given
			  (lisp-compiler t) ; to prevent compiling
			  (messages *compiler-messages*)
			  (boot nil)
			  (readtable nil)
			  &allow-other-keys)
  ":BOOT may be specified as a local option in a box declaration.
Other keys are allowed, but will be ignored."
  (when lisp-compiler
    (when boot
      (load source-file))
    (let ((*print-case* :upcase))	; Case bug in Lucid 2.1
      (if readtable
	  (let ((*readtable* readtable))
	    (compile-file source-file :output-file compiled-file
			  :messages messages))
	  (compile-file source-file :output-file compiled-file
			:messages messages))))
  nil)


(defparameter *c-compiler-options* '("-O"))

;;;How do I detect error statuses in allegro?
#+(and regression allegro)
(progn
  (assert (not (= (excl:run-shell-command "cc -c /dev/error.c") 0)))
  (assert (= (excl:run-shell-command "touch /tmp/foo") 0)))

(defun c-make (&rest stuff &key source-file object-file
		     #|header-file|# (ccom t)
		     (compiler-options *c-compiler-options*)
		     &allow-other-keys)
  (when (and source-file object-file ccom)
    (let* ((arguments `("-c" ,source-file #+sun "-DSUN"
			"-o" ,object-file
			,@*c-compiler-options*))
	   (status
	    #+lucid
	    (multiple-value-bind (junk1 junk2 result junk4)
		(run-program "cc" :arguments arguments)
	      result)
	    #+allegro
	    (excl:run-shell-command
	     (format nil "cc ~{ ~a~}" arguments))))
      (if (= status 0) nil (values t "A C compiler error happened.")))))

#+(and regression allegro)
(progn
  (format t "Exercising allegro's foreign function interface.~%")
  (defun writebar (m)
    (with-open-file (bar "/tmp/bar.c" :direction :output :if-exists :supersede)
      (format bar "int bar (n) int n; { return (n+~a); }~%" m))
    (excl:run-shell-command "cc -c /tmp/bar.c")
    (ff:remove-entry-point (ff:convert-to-lang "bar" :language :c))
    ;; Observation: If load bar.o twice in a row, it fails the second time.
    (load "bar.o")
    ;; The shape of defforeign-list was learned from $clx/excldep.lisp.
    (ff:defforeign-list `((bar
			   :entry-point
			   ,(ff:convert-to-lang "bar" :language :c)
			   :return-type :fixnum
			   :arg-checking t
			   :arguments (fixnum)))))
  (writebar 1)
  (assert (equal (bar 3) 4))
  (writebar 2)
  (assert (equal (bar 4) 6)))

#+regression
(progn
  (format t "Testing our foreign function loader.~%")
  (defbox ff-test "Foreign function test box."
    (:maintainers tsf) ;; Wish I could say :maintainers everyone so
		       ;; everyone could run this test case automatically.
    (:path "/tmp/")
    (:files
     ("bar.o" :entry-points ("bar" "bar2"))))
  (defun writebar (m)
    (with-open-file (bar "/tmp/bar.c" :direction :output :if-exists :supersede)
      (format bar "int bar (n) int n; { return (n+~a); }~%" m)
      (format bar "int strlen (); ~%")
      (format bar "int bar2 (n, s) int n; char *s; ~%~
                   { return (n+~a+strlen(s)); }~%" m))
    (boxgen "ff-test")
    #-allegro
    (format t "This test case doesn't define the entry points of the
foreign function, so you'll surely lose.~%")
    #+allegro
    (ff:defforeign-list
     `((baz :entry-point ,(ff:convert-to-lang "bar" :language :c)
	    :return-type :fixnum
	    :arg-checking t
	    :arguments (fixnum))
       (bar2 :entry-point ,(ff:convert-to-lang "bar2" :language :c)
	     :return-type :fixnum
	     :arg-checking nil
	     :arguments (fixnum string)))))
  (writebar 1)
  (assert (equal (baz 3) 4))
  (assert (equal (bar2 3 "ab") 6))
  (writebar 2)
  (assert (equal (baz 3) 5))
  (assert (equal (bar2 3 "ab") 7)))

(defparameter *default-libraries* '("-lc")
  "The default libraries to use when loading .o files.")

(defun c-load (&rest stuff &key object-file (libraries *default-libraries*)
		     (cload t)
		     ;; The following should be a list of strings,
		     ;; which will be the C names of the functions we
		     ;; intend to use.  This seems to be necessary for
		     ;; Allegro's foreign function interface if we
		     ;; want to be able to regenerate and reload the
		     ;; file without user intervention.
		     (entry-points '())
		     &allow-other-keys)
  #+lucid (declare (ignore entry-points))
  (when (and object-file cload)
    #+lucid (load-foreign-files `(,object-file) libraries)
    #+allegro
    (progn
      (mapc #'(lambda (entry-point)
		(ff:remove-entry-point (ff:convert-to-lang
					entry-point :language :c)))
	    entry-points)
      (load object-file)
      (when (and libraries (not (equal libraries '("-lc"))))
	(warn "Don't know how to load libraries ~s in allegro." libraries))))
  nil)

;;; Timing functions.  These are here because file-server
;;; and machine may be out of synch, leading to problems with
;;; regeneration analysis.
;;; Commented out for now, since unportable and slow.

#|
(defvar *touch-file* "/usrea2/ergo/ess/sys/tools/touch")

(defvar *timediff* 0)

#+(and lucid (not :sparc))
(defun touch-time ()
  (system::run-unix-program "touch" :arguments (list *touch-file*))
  (file-write-date *touch-file*))

#+allegro
(defun touch-time ()
  (error "Function TOUCH-TIME not available in Allegro.
Use #'get-universal-time"))

#-(or (and lucid (not sparc)) allegro)
(eval-when (compile load eval)
  (warn "Function TOUCH-TIME not defined for this Lisp implementation."))

(defun timediff ()
  (let ((timediff (- (touch-time) (get-universal-time))))
    (setq *timediff* timediff)
    (cond ((> timediff 0)
	   (format t "You are ~D seconds behind the file server."
		   timediff))
	  ((< timediff 0)
	   (format t "You are ~D seconds ahead of the file server."
		   timediff))
	  (t (format t "Your local time seems correct.")))))

(defun adjusted-time ()
  (+ (get-universal-time) *timediff*))
|#

(defvar *time-function* #'get-universal-time
  "Should be #'touch-time, #'get-universal-time, or #'adjusted-time.
Currently only #'get-universal-time is supported.")

(defun current-time ()
  "Returns the current time in universal format."
  (funcall *time-function*))

;;; filename extensions.
;;; all source files are .lisp, so we need only one set.
;;; Recommend not changing the source extension. -fp

(defconstant *lisp-source-extension* "lisp")
(defconstant *lisp-compiled-extension*
  #+(and allegro sparc) "fasl"		; Sun4
  #+(and allegro rios) "rfasl"		; PowerPC/RS6000
  #+(and allegro hpux) "hfasl"		; HP 9000
  #+(and allegro macosx powerpc) "mfasl" ; Mac OS X powerpc
  #+(and allegro macosx x86) "nfasl"	; Mac OS X intel
  #+(and allegro x86) "lfasl"		; Intel x86
  #+(and lucid lcl4.1 sparc) "sbin"	; Sun4 new Lucid
  #+(and lucid (not lcl4.1) sparc) "obin" ; Sun4 old Lucid
  #+(and lucid rios) "rbin"		; PowerPC/RS6000
  #+(and lucid mips) "mbin"		; DEC
    ;;; These are experimental
  #+gcl "o"
  #+(and cmu linux) "x86f"
  #+(and cmu darwin) "ppcf"
  #+(and cmu solaris) "sparcf"
  #+(and sbcl x86-64 linux) "x8664s"
  #+(and sbcl (not x86-64) linux) "x86s"
  #+(and sbcl darwin) "ppcs"
  #+(and sbcl solaris) "sparcs"
  #+(and clisp pc386) "clfasl"
  #+harlequin-common-lisp "wfasl"
  #+clisp "fas"
  )

#-(or lucid allegro cmu sbcl ibcl kcl harlequin-common-lisp)
(warn "You may need to redefine the constant tools:*lisp-compiled-extension*
for this implementation of Lisp in the file sys/tools/rel/box-system.lisp.
Right now it is assumed to be \"bin\".")

(defconstant *lisp-source-suffix-string*
  (concatenate 'string "." *lisp-source-extension*))

(defconstant *lisp-compiled-suffix-string*
  (concatenate 'string "." *lisp-compiled-extension*))

(defconstant *lisp-source-extension-pathname*
  (make-pathname :type *lisp-source-extension*))

(defconstant *lisp-compiled-extension-pathname*
  (make-pathname :type *lisp-compiled-extension*))
