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

;; This is loaded right after packages.lisp in pvs.asd, providing globals
;; and functions needed for loading PVS and making an image

;; Note that the pvs-parser should be built separately

;; (asdf:oos :program-op :pvs)
;; loads packages.lisp
;; loads pvs-config.lisp
;; loads "ess" module
;; loads other modules; builds libraries for file_utils, mu, and ws1s
;; calls make-pvs-program
;;  #+allegro excl:generate-application
;;     make-allegro-lisp.lisp
;;       asdf:load-system :pvs
;;  #+sbcl sb-ext::save-lisp-and-die

(in-package :common-lisp)

(defmacro defconstant-if-unbound (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
(export 'defconstant-if-unbound)

(in-package :cl-user)

#+allegro
(defun startup-pvs ()
  (tpl:setq-default *package* (find-package :pvs))
  (rplacd (assoc 'tpl::*saved-package*
		 tpl:*default-lisp-listener-bindings*)
	  'common-lisp:*package*)
  ;; Can't directly reference pvs::pvs-init here
  (apply (find-symbol (string :pvs-init) :pvs) nil))

#+cmu
(defun startup-pvs ()
  (in-package :pvs)
  ;; Can't directly reference pvs::pvs-init here
  (apply (find-symbol (string :pvs-init) :pvs) nil)
  (lisp::%top-level))

#+sbcl
(defun startup-pvs ()
  (in-package :pvs)
  ;; Turn off compiler warnings
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    ;; Normally set to :warn, but is issued after the prompt, which is not what ILISP wants.
    ;; See SBCL/src/code/unix.lisp for details about this warning.
    (setq sb-unix::*on-dangerous-wait* nil)
    ;; Can't directly call (pvs::pvs-init)
    (apply (find-symbol (string :pvs-init) :pvs) nil)
    (sb-impl::toplevel-init)))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (or (find :case-sensitive common-lisp:*features*)
	      (find :case-insensitive common-lisp:*features*))
    (if (or (eq excl:*current-case-mode* :case-sensitive-lower)
	    (eq excl:*current-case-mode* :case-sensitive-upper))
	(push :case-sensitive common-lisp:*features*)
      (push :case-insensitive common-lisp:*features*))))

(in-package :pvs)

(defvar *pvs-path*
  (or #+allegro (sys:getenv "PVSPATH")
      #+gcl (si:getenv "PVSPATH")
      #+cmu (cdr (assoc :PVSPATH extensions::*environment-list*))
      ;; Assume this is loaded while cd'd to the PVS directory 
      (namestring (truename *default-pathname-defaults*))))

;;; The *pvs-fasl-type* is the default type for fasls, typically "fasl"

(defvar *pvs-fasl-type*
  #+allegro excl:*fasl-default-type*
  #+sbcl sb-fasl:*fasl-file-type*)

#+allegro
(eval-when (:load-toplevel :execute)
  (setq *ignore-package-name-case* t))

#+allegro
(setq *cltl1-in-package-compatibility-p* t)

#+allegro
(defun startup-pvs ()
  (tpl:setq-default *package* (find-package :pvs))
  (rplacd (assoc 'tpl::*saved-package*
		 tpl:*default-lisp-listener-bindings*)
	  'common-lisp:*package*)
  ;; Can't directly reference pvs::pvs-init here
  (apply (find-symbol (string :pvs-init) :pvs) nil))

#+cmu
(defun startup-pvs ()
  (in-package :pvs)
  ;; Can't directly reference pvs::pvs-init here
  (apply (find-symbol (string :pvs-init) :pvs) nil)
  (lisp::%top-level))

#+sbcl
(defun startup-pvs ()
  (in-package :pvs)
  ;; Turn off compiler warnings
  (handler-bind ((sb-ext:compiler-note #'muffle-warning))
    ;; Can't directly call (pvs::pvs-init)
    (apply (find-symbol (string :pvs-init) :pvs) nil)
    (sb-impl::toplevel-init)))

#+lucid
(unless (fboundp 'bye)
  (defun bye (&optional (exit-status 0))
    (quit exit-status)))

#+allegro
(defun bye (&optional (exit-status 0))
  (pvs-ws:stop-pvs-server)
  (excl:exit exit-status :no-unwind t :quiet t))

#+harlequin-common-lisp
(defun bye (&optional (exit-status 0))
  (system::bye exit-status))

#+cmu
(defun bye (&optional (exit-status 0))
  (unix:unix-exit exit-status))

#+sbcl
(defun bye (&optional (exit-status 0))
  (pvs-ws:stop-pvs-server)
  (sb-ext:quit :unix-status exit-status))

(defun pvs-version-and-quit ()
  (format t "PVS Version ~a" (eval (find-symbol (string :*pvs-version*) :pvs)))
  (bye))

;;; Definitions to support :perform operations

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (declaim (optimize (compilation-speed 0) (space 1) (safety 2) (speed 3) (cl:debug 1)))
  #+allegro
  (declaim (optimize (compilation-speed 0) (space 1) (safety 1) (speed 3) (cl:debug 1)))
  ;; Note that these do very little SBCL simply does not want to shut up
  ;; #+sbcl (declaim (sb-ext:muffle-conditions cl:warning))
  ;; #+sbcl (declaim (sb-ext:muffle-conditions cl:style-warning))
  ;; #+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  )

(defun file-time (file)
  #+allegro (excl.osi:stat-mtime (excl.osi:stat file))
  #+sbcl (sb-posix:stat-mtime (sb-posix:stat file)))

(defun file-time-lt (file1 file2)
  (< (file-time file1) (file-time file2)))

(defun compile-file-and-load (file)
  (let* ((src (format nil "~a.lisp" file))
	 (fasl (asdf/output-translations:apply-output-translations
		(make-pathname :defaults src :type *pvs-fasl-type*))))
    (when (file-time-lt fasl src)
      (compile-file src))
    (load file)))

(defun parser-needs-rebuilding? ()
  (uiop:with-current-directory ("src/")
    (or (not (uiop:file-exists-p "pvs-lexer.lisp"))
	(file-time-lt "pvs-lexer.lisp" "pvs-gr.txt") ;; Grammar file unchanged
	(file-time-lt "pvs-lexer.lisp" "ergo-gen-fixes.lisp")
	(file-time-lt "pvs-lexer.lisp" "pvs-lang-def.lisp"))))

(defun make-pvs-parser ()
  (uiop:with-current-directory ("src/")
    ;; pvs-parser.lisp, pvs-lexer.lisp, and pvs-sorts.lisp are generated together
    ;; Using pvs-lexer.lisp as a proxy for these
    (when (parser-needs-rebuilding?)
      (compile-file-and-load "ergo-gen-fixes")
      (compile-file-and-load "pvs-lang-def")
      (sb:sb-make :language "pvs" :directory "." :unparser? nil))))

(defun pvs-platform ()
  (uiop:run-program
      (format nil "~a/bin/pvs-platform"
	(or (and (boundp '*pvs-path*) *pvs-path*) "."))
    :output '(:string :stripped t)
    :ignore-error-status t))

(defun make-in-platform (dir lib &optional exe)
  "Runs make in the directory corresponding to the pvs-platform, creating
targets and copying them to the corresponding bin directory."
  (let* ((make-dir (format nil "~a/~a" dir (pvs-platform)))
	 (make-cmd (format nil "make -C ~a" make-dir))
	 (foreign-ext #-(or macosx os-macosx) "so" #+(or macosx os-macosx) "dylib")
	 (lib-file (format nil "~a/~a.~a" make-dir lib foreign-ext))
	 ;; Note the trailing slash is needed for ensure-directories-exist
	 ;;(bin-dir (format nil "~abin/~a/runtime/" *pvs-path* (pvs-platform)))
	 ;;(bin-libfile (format nil "~a~a.~a" bin-dir lib foreign-ext))
	 )
    ;;(ensure-directories-exist bin-dir)
    (multiple-value-bind (out-str err-str err-code)
	(uiop:run-program make-cmd
	  :output '(:string :stripped t)
	  :error-output '(:string :stripped t)
	  :ignore-error-status t)
      (unless (zerop err-code)
	(format t "~%***** make-in-platform ~a ~a ~a" dir lib err-code)
	(error "Failure in ~a:~%output:~%~a~%error output:~%~a"
	       make-cmd out-str err-str))
      ;; (when (probe-file bin-libfile)
      ;; 	(delete-file bin-libfile))
      ;; (uiop:copy-file lib-file bin-libfile)
      ;; (when exe
      ;; 	(let ((exe-file (format nil "~a/~a" bin-dir exe)))
      ;; 	  (when (probe-file exe-file)
      ;; 	    (delete-file exe-file))
      ;; 	  (uiop:copy-file (format nil "~a/~a" make-dir exe) exe-file)))
      #+allegro (format t "~% make-in-platform loading ~a" lib-file)
      #+allegro (cffi:load-foreign-library lib-file)
      )))

(defun finally-do ()
  (format t "~%In finally-do")
  (setq *pvs-log-stream* nil)
  (funcall (intern (string :clear-pvs-hooks) :pvs))
  #-(or cmu sbcl)
  (funcall (intern (string :BDD_bdd_init) :pvs))
  ;;(when *pvs-has-libyices*
  ;;(pvs::nlyices-init))
  (when t ;;*load-pvs-prelude*
    (let ((*package* (find-package :pvs)))
      (funcall (intern (string :load-prelude) :pvs))
      ;;(load (format nil "~a/src/PVSio/prelude-attachments" *pvs-path*))
      ;;(pvs::initialize-prelude-attachments)
      ;;(pvs::register-manip-type pvs::*number_field* 'pvs::pvs-type-real)
      ))
  ;; #+allegro
  ;; (let* ((optfile (format nil "~a/src/closopt.lisp"
  ;; 		    ;;(symbol-value (intern (string :*pvs-path*) :pvs))
  ;; 		    (asdf:system-relative-pathname :pvs ".")))
  ;; 	 (fasl-file (funcall (intern (string :make-fasl-file-name) :pvs) optfile)))
  ;;   (unless (uiop:file-exists-p fasl-file)
  ;;     (compile-file optfile :output-file fasl-file))
  ;;   (load fasl-file))
  (funcall (intern (string :remove-typecheck-caches) :pvs))
  (assert (every #'fboundp (symbol-value (intern (string :*untypecheck-hook*) :pvs))))
  ;;(asdf:clear-configuration)
  (setq *pvs-build-time* (get-universal-time))
  (setq *pvs-git-describe* (funcall (intern (string :pvs-git-description) :pvs)))
  (funcall (intern (string :write-pvs-version-file) :pvs))
  )

#+allegro
(defun make-pvs-program ()
  (make-pvs-program* nil)
  ;;(make-pvs-program* t)
  )

(defvar *runtime* nil)

#+allegro
(defun make-pvs-program* (runtime?)
  (format t "~%Making Allegro ~a" (if runtime? "runtime" "devel"))
  (let* ((tmp-dir "/tmp/pvs-allegro-build/")
	 (platform (pvs-platform))
	 (platform-dir (format nil "./bin/~a/" platform))
	 (build-dir (format nil "~a~a/"
		     platform-dir (if runtime? "runtime" "devel")))
	 (foreign-ext #-(or macosx os-macosx) "so" #+(or macosx os-macosx) "dylib")
	 (bin-dir (format nil "bin/~a/~a/" platform (if runtime? "runtime" "devel")))
	 (allegro-home (or (sys:getenv "ALLEGRO_HOME") "~/acl")))
    ;; (setq *pvs-path* nil)
    (ensure-directories-exist build-dir)
    ;; (ensure-directories-exist platform-dir)
    (if runtime?
	(excl:delete-directory-and-files tmp-dir :if-does-not-exist :ignore)
	(ensure-directories-exist tmp-dir))
    (format t "~%Calling generate-application for ~a" tmp-dir)
    (excl:generate-application
     "pvs-allegro"
     tmp-dir
     (list "./src/make-allegro-pvs.lisp" :list2)
     ;; :additional-arguments nil
     ;; :additional-forms nil
     ;; :additional-plus-arguments nil
     :allow-existing-directory (not runtime?)
     ;; :application-files nil
     ;; :application-administration nil
     ;; :application-type :exe
     ;; :autoload-warning t ;; Default nil  ; Not much difference
     ;; :build-debug t ;;:interactive ; No difference
     ;; :build-executable mlisp
     ;;#-(or macosx x86-64) :aclmalloc-heap-start #-(or macosx x86-64) "2752512K"	;; (/ #xa8000000 1024)
     ;; :build-input nil ;; "idout" :verbose t
     :case-mode :case-sensitive-lower
     ;; :copy-shared-libraries t ; new ; Makes no difference by itself
     ;; :discard-arglists nil
     ;; :discard-compiler nil
     :discard-local-name-info nil
     :discard-source-file-info runtime?
     :discard-xref-info runtime?
     ;; :dst t ;; daylight savings time
     ;; :generate-fonts nil
     :image-only (not runtime?) ;; Commenting causes Error: nil is an illegal :runtime value.
     :include-clim nil
     :include-compiler t
     :include-composer nil
     :include-debugger (not runtime?)
     :include-devel-env (not runtime?)
     :include-locales nil
     :include-tpl t
     :include-xcw nil
     :internal-debug nil
     #-x86-64 :lisp-heap-size #-x86-64 300000000
     ;;#-(or macosx x86-64) :lisp-heap-start #-(or macosx x86-64) #x20000000
     #+(or macosx x86-64) :aclmalloc-heap-start #+(or macosx x86-64) #xa0000000000
     ;; :load-local-names-info *load-local-names-info*
     :load-source-file-info (not runtime?)
     :load-xref-info (not runtime?)
;;     :newspace 4000000
;;     :oldspace 512000
     :opt-debug 1
     :opt-safety 1
     :opt-space 1
     :opt-speed 3
     :post-load-form (unless runtime? '(excl::translate-shlib-filenames t))
     ;; :pre-dump-form nil
     ;; :pre-load-form nil
     :preserve-documentation-strings t
     :read-init-files nil
     :record-source-file-info (not runtime?)
     :record-xref-info (not runtime?)
     ;; :restart-app-function nil
     :restart-init-function 'startup-pvs
     :runtime (when runtime? :dynamic) ; 
     :runtime-bundle runtime?
     ;; :server-name nil
     :temporary-directory "/tmp/"
     :us-government nil
     :verbose t)
    (format t "~%After generate-application, copying files to ~a~%" build-dir)
    (dolist (file (directory (format nil "~a*" tmp-dir)))
      (let ((dest (format nil "~a/~a" build-dir (file-namestring file))))
	(format t "~%Copying ~a to ~a" file dest)
	;;(ignore-errors (delete-file dest))
	(sys:copy-file file dest :overwrite t)))
    ;; Copy shared-object files
    (dolist (sdir-file '(("utils" . "file_utils") ("BDD" . "mu") ("WS1S" . "ws1s")))
      (let* ((file (format nil "~a.~a" (cdr sdir-file) foreign-ext))
	     (libsrc (format nil "src/~a/~a/~a" (car sdir-file) platform file))
	     (libdest (format nil "bin/~a/~a/~a"
			platform (if runtime? "runtime" "devel") file)))
	(format t "~%Copying ~a to ~a" libsrc libdest)
	(sys:copy-file libsrc libdest :overwrite t)))
    ;; Now copy files.bu and libacli10196s.{so,dylib} from ALLEGRO_HOME
    (let ((fsrc (format nil "~a/files.bu" allegro-home))
	  (fdest (format nil "~a/files.bu" bin-dir)))
      (format t "~%Copying ~a to ~a" fsrc fdest)
      (sys:copy-file fsrc fdest :overwrite t))
    (let ((src (format nil "~a/libacli10196s.~a" allegro-home foreign-ext))
	  (dest (format nil "~a/libacli10196s.~a" bin-dir foreign-ext)))
      (sys:copy-file src dest :overwrite t))
    ;; Copy mlisp to pvs-allegro
    (let ((src (format nil "~a/mlisp" allegro-home))
	  (dest (format nil "~a/pvs-allegro" bin-dir)))
      (sys:copy-file src dest :overwrite t))
    (unless runtime?
      (copy-devel-license build-dir))
    ))

#+allegro
(defun copy-devel-license (targetdir)
  (sys:copy-file (format nil "~adevel.lic" (translate-logical-pathname "sys:"))
		 (format nil "~adevel.lic" targetdir)
		 :overwrite t))

#+sbcl
(defun make-pvs-program ()
  (let* ((tmp-dir "/tmp/pvs-sbcl-build/")
	 (platform (pvs-platform))
	 (platform-dir (format nil "./bin/~a/" platform))
	 (lext #-(or macosx os-macosx) "so" #+(or macosx os-macosx) "dylib")
	 (build-dir (format nil "~aruntime/" platform-dir))
	 (pvs-prog (format nil "~apvs-sbclisp" build-dir)))
    (format t "~%Creating SBCL core image in ~a" pvs-prog)
    (ensure-directories-exist pvs-prog)
    (let* ((lib (format nil "file_utils.~a" lext))
	   (lib-src (format nil "~asrc/utils/~a/~a" *pvs-path* platform lib))
	   (lib-dst (format nil "~a/~a" build-dir lib)))
      (alexandria:copy-file lib-src lib-dst))
    (let* ((lib (format nil "mu.~a" lext))
	   (lib-src (format nil "~asrc/BDD/~a/~a" *pvs-path* platform lib))
	   (lib-dst (format nil "~a/~a" build-dir lib)))
      (alexandria:copy-file lib-src lib-dst))
    (let* ((lib (format nil "ws1s.~a" lext))
	   (lib-src (format nil "~asrc/WS1S/~a/~a" *pvs-path* platform lib))
	   (lib-dst (format nil "~a/~a" build-dir lib)))
      (alexandria:copy-file lib-src lib-dst))
    ;;(clrhash asdf/source-registry:*source-registry*)
    (dolist (shobj sb-sys:*shared-objects*)
      (when (member (sb-alien::shared-object-namestring shobj)
		    '("libcrypto.so" "libssl.so")
		    :test #'(lambda (str1 str2) (uiop:string-prefix-p str2 str1)))
	(sb-alien:unload-shared-object (sb-alien::shared-object-pathname shobj))
	;;(setf (sb-alien::shared-object-dont-save shobj) t)
	))
    (sb-ext:save-lisp-and-die
     pvs-prog
     :toplevel (function startup-pvs)
     :executable t
     :save-runtime-options t)))
