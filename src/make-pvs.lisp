;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs.lisp -- Used to make the PVS system
;; Author          : Sam Owre
;; Created On      : Tue Dec 29 17:02:40 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 29 21:08:34 1998
;; Update Count    : 5
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

; Note - the call to (excl::translate-shlib-filenames t) was the result
; of large conversations with Franz Inc under SPR 23653.  As a non-exported
; symbol from excl, it's technically not supported.

(defvar *runtime* nil)

(defun build-pvs (builddir runtime?)
  (let ((tmpdir "/tmp/pvs-allegro-build/")
	(pvspath (or (sys:getenv "PVSPATH") ".")))
    (setq *pvs-path* nil)
    (excl:generate-application
     (format nil "~a-~a"
       (or (sys:getenv "SYSTEM") "pvs")
       (or (sys:getenv "LISP") "allegro"))
     tmpdir
     (list :list2 (format nil "~a/src/make-allegro-pvs.lisp" pvspath))
     :additional-arguments nil
     :additional-forms nil
     :additional-plus-arguments nil
     :allow-existing-directory (not runtime?)
     :autoload-warning t
     #+(version>= 6) :build-debug #+(version>= 6) t
     #-(or macosx x86-64) :c-heap-start #-(or macosx x86-64) "2752512K"	;; (/ #xa8000000 1024)
     #+(version>= 6) :case-mode #+(version>= 6) :case-sensitive-lower
     #-(version>= 6) :debug-on-error #-(version>= 6) t
     :discard-arglists nil
     :discard-compiler nil
     :discard-local-name-info nil
     :discard-source-file-info runtime?
     :discard-xref-info runtime?
     :dst t
     #-(version>= 6) :exit-after-image-build #-(version>= 6) t
     :generate-fonts nil
     :image-only (not runtime?)
     :include-clim nil
     :include-compiler t
     :include-composer nil
     :include-debugger (not runtime?)
     :include-devel-env (not runtime?)
     :include-tpl t
     :include-xcw nil
     :internal-debug nil
     #-x86-64 :lisp-heap-size #-x86-64 20000000
     #-(or macosx x86-64) :lisp-heap-start #-(or macosx x86-64) #x20000000
     :load-local-names-info nil
     :load-source-file-info (not runtime?)
     :load-xref-info (not runtime?)
     :newspace 4000000
     :oldspace 512000
     :opt-debug 1
     :opt-safety 1
     :opt-space 1
     :opt-speed 3
     :post-load-form (unless runtime?
		       '(excl::translate-shlib-filenames t))
     :pre-dump-form nil
     :pre-load-form nil
     :preserve-documentation-strings t
     :presto nil
     :presto-flush-to-code-file nil
     :read-init-files nil
     :record-source-file-info (not runtime?)
     :record-xref-info (not runtime?)
     :restart-app-function nil
     :restart-init-function 'startup-pvs
     :runtime (when runtime? :dynamic)
     :runtime-bundle runtime?
     :server-name nil
     :temporary-directory "/tmp/"
     :us-government nil
     :verbose t)
    (dolist (file (directory (format nil "~a*" tmpdir)))
      (let ((dest (format nil "~a/~a" builddir (file-namestring file))))
	(ignore-errors (delete-file dest))
	(sys:copy-file file dest :overwrite t)))
    (ignore-errors (excl:shell (format nil "rm -f ~a/*" tmpdir)))
    ))

(defun copy-devel-license (targetdir)
  (sys:copy-file (format nil "~adevel.lic" (translate-logical-pathname "sys:"))
		 (format nil "~adevel.lic" targetdir)
		 :overwrite t))
  

(let* ((exitcode 1)
       (platform (or (sys:getenv "PLATFORM") "ix86-Linux"))
       (runtimedir (format nil "~a/bin/~a/runtime/"
		     (or (sys:getenv "TARGETPATH") ".") platform))
       (develdir (format nil "~a/bin/~a/devel/"
		  (or (sys:getenv "TARGETPATH") ".") platform)))
 (progn ;;unwind-protect
     (multiple-value-bind (v err)
	 (progn ;;ignore-errors
	   (if *runtime*
	       (build-pvs runtimedir t)
	       (progn
		 (build-pvs develdir t)
		 (build-pvs develdir nil)
		 #+(version>= 6) (copy-devel-license develdir))))
       (if err
	   (let ((*print-readably* nil))
	     (format t "~a" err))
	   (setq exitcode 0)))
   (excl:exit exitcode)))
