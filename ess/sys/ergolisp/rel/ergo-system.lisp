;;; -*- Mode: Lisp; Package: ERGO-SYSTEM -*-
;;;
;;; Ergo modification that may be system specific.
;;; This should be changed to another package which is imported by
;;; all others.
;;;
;;; Author: Frank Pfenning.  Last Modified Tue Sep 26 16:54:54 1989
;;;
;;; Sccs Id @(#)ergo-system.lisp	1.21 9/26/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


#-gcl
(defpackage :ergo-system)
(in-package :ergo-system) (use-package :ergolisp)

#-(or allegro lucid cmu)
(eexport '(memq))
#+harlequin-common-lisp
(eexport '(assq))
(eexport '(ergo-disksave def-disksave-hook mover))
(eexport '(ergo-ignore-if-unused))

#-(or allegro lucid cmu)
(defun memq (item list) (member item list :test #'eq))

#+harlequin-common-lisp
(defun assq (item list) (assoc item list :test #'eq))
;;; If there is something that you want to happen when the lisp image
;;; restarts, wrap it in a def-disksave-hook and put it on the top
;;; level.  Beware that if you load a file containing a
;;; def-disksave-hook several times, the hook will happen more than
;;; once next time we start things up.

#+allegro
(defmacro def-disksave-hook (&body body)
  `(pushnew #'(lambda () . ,body) excl:*restart-actions* :test #'equal))

#-allegro
(defvar *disksave-hooks* nil)
#-allegro
(defmacro def-disksave-hook (&body body)
  `(pushnew #'(lambda () . ,body) *disksave-hooks* :test #'equal))

#-(or lucid allegro cmu ibcl kcl harlequin-common-lisp)
(warn "You may need to redefine DEF-DISKSAVE-HOOK for this implementation
of Lisp in file sys/ergolisp/rel/ergo-system.lisp.")
#+(or ibcl kcl)
(warn "Restart functions appear not to be definable for this implementation of
Common Lisp.  Disksave hooks will therefore not be executed.")

#+lucid
(defun ergo-disksave (filename &key (restart-function #'(lambda () nil))
			       &allow-other-keys)
  #+(and lcl3.0 sparc)
  (warn "Ignoring disksave hooks since a :restart-function leads to a
segmentation violation in this implementation of Common Lisp.")
  (funcall #+lcl3.0 #'lcl:disksave #-lcl3.0 #'system:disksave
	   filename
	   #-(and lcl3.0 sparc) :restart-function
	   #-(and lcl3.0 sparc) #'(lambda () (dolist (i *disksave-hooks*)
						     (funcall i))
				    (funcall restart-function))
	   :full-gc t))

#+allegro
(defun ergo-disksave (filename &key (restart-function #'(lambda () nil))
			       &allow-other-keys)
  (format t "Initiating global garbage collection ... ")
  (excl:gc t)
  (format t "Done.~%")
  (excl:dumplisp :name filename
		 :restart-function restart-function
		 :read-init-file t))

#+cmu
(defun ergo-disksave (filename &key (restart-function #'(lambda () nil))
			       &allow-other-keys)
  (extensions:save-lisp filename
			:init-function
			#'(lambda () (dolist (i *disksave-hooks*)
				       (funcall i))
			    (funcall restart-function))
			;; :print-herald nil
			))

#+harlequin-common-lisp
(defun ergo-disksave (filename &key (restart-function #'(lambda () nil))
			       &allow-other-keys)
  (system:save-image filename
			:init-function
			#'(lambda () (dolist (i *disksave-hooks*)
				       (funcall i))
			    (funcall restart-function))
			;; :print-herald nil
			))

#+(or ibcl kcl)
(defun ergo-disksave (filename &allow-other-keys)
  (warn "Ibuki Common Lisp apparently allows no restart function.
Ignoring DISKSAVE-HOOKS.")
  (lisp:save filename))

#-(or lucid allegro cmu ibcl kcl harlequin-common-lisp)
(error "Please define ERGO-DISKSAVE for this implementation of Lisp
in the file sys/ergolisp/rel/ergo-system.lisp.")

(defun mover (file-name)
  (let ((next-file-name (concatenate 'string file-name "!")))
    (when (probe-file file-name)
      (rename-file file-name next-file-name))))



(eval-when (compile eval load)
  (defmacro ergo-ignore-if-unused (&rest vars)
    #+excl				; for allegro
    nil ;;`(declare (excl:ignore-if-unused ,@vars))
    #+lucid
    `(declare (ignore ,@vars))		; lucid inconsistency
    #+cmu
    nil
    #-(or excl lucid cmu)
    `(declare))
  )

