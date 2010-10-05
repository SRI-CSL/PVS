;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(in-package :common-lisp)

#+(and allegro-version>= (version>= 8 2))
(eval-when (:execute :compile-toplevel :load-toplevel)
  ;; Allegro 8.2 does not allow 'the' readtable to be modified
  (defvar *pvs-readtable* (copy-readtable nil))
  (setq *readtable* *pvs-readtable*)
  (setf (third (assoc '*readtable* excl:*cl-default-special-bindings*))
	'*pvs-readtable*))

(#-(or cmu sbcl excl) progn
 #+cmu ext:without-package-locks
 #+sbcl sb-ext:without-package-locks
 #+excl excl:without-package-locks
 (defmacro defconstant-if-unbound (name value &optional doc)
   `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
		       ,@(when doc (list doc))))
 (export 'defconstant-if-unbound))

(in-package :cl-user)

(export '(*pvs-path* *pvs-binary-type* *pvs-platform* bye))

(defparameter *pvs-path*
  (or #+allegro (sys:getenv "PVSPATH")
      #+gcl (si:getenv "PVSPATH")
      #+cmu (cdr (assoc :PVSPATH extensions::*environment-list*))
      ;; Assume this is loaded while cd'd to the PVS directory 
      (namestring (truename *default-pathname-defaults*))))

(eval-when (:execute :load-toplevel)
  (defvar *pvs-platform*
    (let ((cmd (format nil "~a/bin/pvs-platform" *pvs-path*)))
      #+allegro (car (excl.osi:command-output cmd))
      #+cmu (string-trim
	     '(#\Newline)
	     (with-output-to-string (str)
	       (extensions:run-program
		cmd nil :output str :pty nil :error nil)))
      #+sbcl (string-trim
	     '(#\Newline)
	     (with-output-to-string (str)
	       (sb-ext:run-program
		cmd nil :output str :pty nil :error nil))))))

;;; The *pvs-binary-type* is used to be able to build PVS under several
;;; lisps/platforms without rerunning configure each time

(defvar *pvs-binary-type*
  #+(and allegro sparc) "fasl"		; Sun4
  #+(and allegro rios) "rfasl"		; PowerPC/RS6000
  #+(and allegro hpux) "hfasl"		; HP 9000
  #+(and allegro linux x86) "lfasl"		; Intel x86
  #+(and allegro linux x86-64) "l64fasl" ; Intel x86_64
  #+(and allegro macosx powerpc) "mfasl" ; Mac OS X powerpc
  #+(and allegro macosx x86) "nfasl"	; Mac OS X intel
  #+(and allegro macosx x86-64) "n64fasl"	; Mac OS X intel x86_64
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
  #+(and sbcl sparc) "sparcs"
  #+(and clisp pc386) "clfasl"
  #+harlequin-common-lisp "wfasl"
  )

#+allegro
(eval-when (eval load)
  (setq *ignore-package-name-case* t))

#+allegro
(eval-when (eval load)
  (setq excl:*fasl-default-type* *pvs-binary-type*)
  (setq system:*load-search-list*
	(list #p"" (make-pathname :type *pvs-binary-type*)
	      #p(:type "cl") #p(:type "lisp"))))

#+cmu
(eval-when (eval load)
  (setq extensions:*load-object-types*
	(remove "fasl" extensions:*load-object-types* :test #'string=))
  (pushnew *pvs-binary-type* extensions:*load-object-types* :test #'string=)
  ;;(pushnew *pvs-binary-type* lisp:*load-lp-object-types* :test #'string=)
  )

#+sbcl
(eval-when (:execute :load-toplevel)
  (setq sb-c::*fasl-file-type* *pvs-binary-type*))

#+allegro
(setq *cltl1-in-package-compatibility-p* t)

#+lucid
(unless (fboundp 'bye)
  (defun bye (&optional (exit-status 0))
    (quit exit-status)))

#+allegro
(defun bye (&optional (exit-status 0))
  (excl:exit exit-status :no-unwind t :quiet t))

#+harlequin-common-lisp
(defun bye (&optional (exit-status 0))
  (system::bye exit-status))

#+cmu
(defun bye (&optional (exit-status 0))
  (unix:unix-exit exit-status))

#+sbcl
(defun bye (&optional (exit-status 0))
  (quit :unix-status exit-status))

(defun pvs-version-and-quit ()
  (format t "PVS Version ~a" (eval (find-symbol (string :*pvs-version*) :pvs)))
  (bye))
