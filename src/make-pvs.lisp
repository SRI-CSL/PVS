;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs.lisp -- Used to make the PVS system
;; Author          : Sam Owre
;; Created On      : Tue Dec 29 17:02:40 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 29 21:08:34 1998
;; Update Count    : 5
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Note - the call to (excl::translate-shlib-filenames t) was the result
; of large conversations with Franz Inc under SPR 23653.  As a non-exported
; symbol from excl, it's technically not supported.

(defvar *runtime* nil)

(defun build-pvs (builddir runtime?)
  (excl:generate-application
	     "pvs-allegro6.0"
	     builddir	     
	     (list :list2 "./src/make-allegro-pvs.lisp")
	     :additional-arguments nil
	     :additional-forms nil
	     :additional-plus-arguments nil
	     :allow-existing-directory (not runtime?)
	     :autoload-warning t
	     :case-mode :case-sensitive-lower
	     :debug-on-error t
	     :discard-arglists nil
	     :discard-compiler nil
	     :discard-local-name-info nil
	     :discard-source-file-info runtime?
	     :discard-xref-info runtime?
	     :dst t
	     :exit-after-image-build t
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
	     :lisp-heap-size 20000000
	     :lisp-heap-start nil
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
	     :restart-init-function nil
	     :runtime (when runtime? :dynamic)
	     :runtime-bundle runtime?
	     :server-name nil
	     :temporary-directory "/usr/tmp/"
	     :us-government nil
	     :verbose t ))

(defun copy-devel-license (targetdir)
  (sys:copy-file (format nil "~adevel.lic" (translate-logical-pathname "sys:"))
		 (format nil "~adevel.lic" targetdir)))
  

(let* ((exitcode 1)
       (platform (or (sys:getenv "PLATFORM") "ix86-redhat5"))
       (runtimedir (format nil "./bin/~a/runtime/" platform))
       (fulldir (format nil "./bin/~a/full/" platform)))
 (unwind-protect
     (multiple-value-bind (v err)
	 (ignore-errors
	   (if *runtime*
	       (build-pvs runtimedir t)
	       (progn
		 (build-pvs fulldir t)
		 (build-pvs fulldir nil)
		 (copy-devel-license fulldir))))
       (if err
	   (let ((*print-readably* nil))
	     (format t "~a" err))
	   (setq exitcode 0)))
   (excl:exit exitcode)))












