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

(defvar *runtime* nil)

(let ((exitcode 1))
  (unwind-protect
      (multiple-value-bind (v err)
	  (ignore-errors
	    (excl:generate-application
	     "pvs-allegro5.0"
	     (format nil "./bin/~a/~a/"
	       (or (sys:getenv "PLATFORM") "ix86-redhat5")
	       (if *runtime* "runtime" "full"))
	     (list "./src/make-allegro-pvs.lisp")
	     :additional-arguments nil
	     :additional-forms nil
	     :additional-plus-arguments nil
	     :autoload-warning t
	     ;; :bundle-file nil
	     ;; :c-heap-size nil
	     ;; :c-heap-start nil
	     :case-mode :case-insensitive-upper
	     ;; :copy-shared-libraries nil
	     :debug-on-error t
	     ;; :destination-directory nil
	     :discard-arglists nil
	     :discard-compiler nil
	     :discard-local-name-info nil
	     :discard-source-file-info nil
	     :discard-xref-info nil
	     ;; :dribble-file nil
	     :dst t
	     :exit-after-image-build t
	     :generate-fonts nil
	     :include-clim nil
	     ;; :include-common-graphics nil ;Windows
	     :include-compiler t
	     :include-composer nil
	     :include-debugger t
	     :include-devel-env t
	     ;; :include-ide nil ;Windows
	     :include-tpl t
	     :include-xcw nil
	     :internal-debug nil
	     :lisp-heap-size 20000000
	     :lisp-heap-start nil
	     ;; :lisp-files nil
	     :load-local-names-info nil
	     :load-source-file-info t
	     :load-xref-info t
	     :newspace 4000000
	     :oldspace 512000
	     :opt-debug 1
	     :opt-safety 1
	     :opt-space 1
	     :opt-speed 3
	     ;; :pll-file nil
	     ;; :pll-from-sys nil
	     :post-load-form nil
	     :pre-dump-form nil
	     :pre-load-form nil
	     :preserve-documentation-strings t
	     :presto nil
	     :presto-flush-to-code-file nil
	     ;; :presto-lib nil
	     ;; :print-startup-message t
	     :read-init-files nil
	     :record-source-file-info t
	     :record-xref-info t
	     :restart-app-function nil
	     :restart-init-function nil
	     :runtime nil
	     :server-name nil
	     ;; :show-window nil ;Windows
	     ;; :splash-from-file nil ;Windows
	     :temporary-directory "/usr/tmp/"
	     :us-government nil
	     ;;    :user-shared-libraries t
	     :verbose t
	     ;; :wait nil ;Windows
	     ))
	(if err
	    (let ((*print-readably* nil))
	      (format t "~a" err))
	    (setq exitcode 0)))
    (excl:exit exitcode)))
