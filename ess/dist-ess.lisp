;;; -*- Mode: Lisp; Package: user -*-
;;;
;;; Construction of core images with the ESS.
;;; NOTE: for "Ergo internal" versions with more experimental
;;;	  tools and applications, use "internal-ess.lisp"
;;;	  instead.
;;;
;;; Last Modified Fri Oct 13 01:03:46 1989
;;;
;;; Instructions:  see /homes/EHDM/systems/ess/README
;;;

(in-package :user)

#+(and allegro compiler)
(setq comp:*cltl1-compile-file-toplevel-compatibility-p* t)
#+allegro
(setq *record-source-file-info* nil
      *record-xref-info* nil
      excl:*cltl1-in-package-compatibility-p* t
      excl:*enable-package-locked-errors* nil)

;;; Defines:
;;; #+(and lucid lcl3.0) (export '(load-production-compiler))
;;; (export '(build-ess use-production-compiler use-development-compiler))

;;; First load init-load, which includes box-defs.lisp
(load "/homes/EHDM/systems/ess/init-load.lisp")

;;; Why are these constants put into the tools package??

(in-package "TOOLS")

(export '(*ess-version* *ess-version-date*))
(export '(*ess-feature-alist*))
(export '(*ess-name* *ess-temp-name*))
(export '(nice-time-string sys-parameters-string))
(export '(build-ess-lisp generate-ess-lisp recompile-ess-lisp load-ess-lisp))
(export '(save-ess-lisp))

(defparameter *ess-version* "0.15")
(defparameter *ess-version-date* "[12 Feb 90]")

(defun nice-time-string ()
  (multiple-value-bind (sec min hour date month year doy dst zone)
      (get-decoded-time)
    (declare (ignore doy dst zone))
    (format nil "~S/~S/~S ~d:~2,'0d:~2,'0d" month date year hour min sec)))

(defun sys-parameters-string ()
  (format nil "~A [~A]~%  on a ~A [~A] running ~A [~A]."
	  (lisp-implementation-type) (lisp-implementation-version)
	  (machine-type) (machine-version)
	  (software-type) (software-version)))

(defparameter *ess-feature-alist*
  '(("ergolisp" (basics term-support ergolisp-with-term-support) (ess-no-load)
	      "Ergo Lisp")
    ("sb"  (syntax-facility)   (sb-examples)  "Syntax Facility")
    ("ab"  (analysis-facility) (ab-examples)  "Analysis Facility")
    ("elp" (elp)	       (elp-examples ebg-system ebg-examples)
     					      "eLP")
    ("clx" (clx)	       (clx-source clx-examples)
					      "Common Lisp/X11 interface")
    ("display" (display)       (display-doc)  "DISPLAY X-window interface")
    ("mellowcard" (mellowcard) ()	      "X-window eLP documentation"))
  "An alist of (FEATURE-NAME CRATES TAR-CRATES ID).  CRATES is the list of
crates that have to be loaded for the feature to be present in the core
image, TAR-CRATES is the list of crates that have to be put on tape so that
the tape includes documentation and examples.
  Each feature should correspond to a tar-file in the directory that is
accessible via anonymous ftp.")

(defun string-or-name-equal (ssym1 ssym2)
  (let ((str1 (if (symbolp ssym1) (symbol-name ssym1) ssym1))
	(str2 (if (symbolp ssym2) (symbol-name ssym2) ssym2)))
    (string-equal ssym1 ssym2)))

(defun filter-features (features)
  (remove-if-not #'(lambda (feat) (member (car feat) features
					  :test #'string-or-name-equal))
		 *ess-feature-alist*))

(defun build-ess-lisp (filename features)
  ;; Make sure "ergolisp" is always there.
  (generate-ess-lisp features)
  (save-ess-lisp filename features))

(defun generate-ess-lisp (features &optional (gen-fun 'crategen-fun))
  (let ((feature-alist (filter-features features)))
    (let ((ess-crates (apply #'append (mapcar #'second feature-alist))))
      (let ((*system-administration-mode* t))
	(funcall gen-fun ess-crates)))))

(defun recompile-ess-lisp (features)
  (regenerate-with "lisp-compiler")
  (generate-ess-lisp features))

(defun load-ess-lisp (features)
  (generate-ess-lisp features 'crateload-fun))

(defparameter *backup-image-p* nil)

(defun save-ess-lisp (ess-filename features)
  (let ((feature-alist (filter-features features)))
    (let ((ess-string (format nil "~{~A~:^, ~}."
			      (mapcar #'fourth feature-alist))))
      (format t "Now disksaving ... ")
      (let ((in-reborn-image-p nil)
	    (builder (user-name))
	    (build-time (nice-time-string))
	    (build-parms (sys-parameters-string)))
	;; To determine whether we are in the reborn Lisp image.
	;;  Needed by Allegro because it doesn't restart at the top-level loop.
	(when *backup-image-p*
	  (format t "Renaming ... ")
	  (mover ess-filename))
	(ergo-disksave 
	 ess-filename
	 :restart-function
	 #'(lambda ()
	     (format t "~%~72:@<Welcome to the Ergo Support System~>~%")
	     (format t "~% Version ~A ~A" *ess-version* *ess-version-date*)
	     (format t "~% with ~A" ess-string)
	     (format t "~% in ~A" build-parms)
	     (format t "~% generated ~A by ~A~%~%"
		     build-time builder)
	     (setq in-reborn-image-p t)
	     (case (windowsystem)
	       (:x11
		(unless (member :x11 *features*)
		  (warn "This lisp is running under X11 but it was not saved under X11.")
		  (push :x11 *features*)))
	       (:x10
		(when (member :x11 *features*)
		  (warn "This lisp is running under X10 but is was saved under X11.")
		  (setq *features* (delete :x11 *features*)))))))
	(unless in-reborn-image-p
	  (format t "Done.~%"))))))

;;; Switching back to user-visible stuff.

(in-package "USER")

#+(and lucid lcl3.0)
(defun load-production-compiler ()
 (warn "Please modify the the function LOAD-PRODUCTION-COMPILER
in file build-ess-lisp.lisp to suit your environment, if necessary.")
 )

#+(and lucid lcl3.0)
(defun use-production-compiler ()
  (format t "~&;;; Switching compiler to production mode.~%")
  (proclaim '(optimize (compilation-speed 0)))
  (warn "Please make sure the production compiler is available and loaded by
checking in the listed compiler options if compilation-speed is 0.")
  (lcl:report-compiler-options))

#+(and lucid lcl3.0)
(defun use-development-compiler ()
  (format t "~&;;; Switching compiler to development mode.~%")
  (proclaim '(optimize (compilation-speed 3))))

#+allegro
(defun use-production-compiler ()
  (format t "~&;;; Switching compiler to production mode.~%")
  (setq *record-source-file-info* nil)
  (setq *record-xref-info* nil)
  (proclaim '(optimize (speed 3) (safety 1) (space 0) (debug 0))))

#+allegro
(defun use-development-compiler ()
  (format t "~&;;; Switching compiler to development mode.~%")
  (proclaim '(optimize (speed 1) (safety 1))))

#+cmu
(defun use-production-compiler ()
  (format t "~&;;; Switching compiler to production mode.~%")
  (proclaim '(optimize (speed 3) (safety 1) (space 2) (compilation-speed 0))))

#+cmu
(defun use-development-compiler ()
  (format t "~&;;; Switching compiler to development mode.~%")
  (warn "Development mode for CMU Common Lisp not yet defined."))

#-(or allegro lucid cmu)
(warn "Production compilation declarations undefined for this Common Lisp
implementation.  You may add them by defining USE-PRODUCTION-COMPILER
in the file dist-ess.lisp.")

#-(or allegro lucid cmu)
(warn "Development compilation declarations undefined for this Common Lisp
implementation.  You may add them by defining USE-DEVELOPMENT-COMPILER
in the file dist-ess.lisp.")

(defmacro build-ess (filename &rest features)
  `(tools:build-ess-lisp ,filename ',features))

(defmacro generate-ess (&rest features)
  `(tools:generate-ess-lisp ',features))

(defmacro save-ess (filename &rest features)
  `(tools:save-ess-lisp ,filename ',features))
