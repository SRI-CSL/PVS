;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-parser.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Dec 29 02:55:05 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 29 03:28:07 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(eval-when (eval load)
  ;; This sets *pvs-path* and sets *pvs-binary-type*
  (load "pvs-config.lisp"))

(load (format nil "~a/ess/dist-ess.lisp" *pvs-path*))
(generate-ess ergolisp sb)

#+allegro
(compile-file-if-needed (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
#-allegro
(compile-file (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
(load (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
(let ((sbmake (intern (string :sb-make) :sb)))
  (funcall sbmake
	   :language "pvs"
	   :working-dir (format nil "~a/src/" (or *pvs-path* "."))
	   :unparser? nil))
(bye)
