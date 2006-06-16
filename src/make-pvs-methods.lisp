;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-methods.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec 31 19:16:33 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Dec 31 21:13:07 1998
;; Update Count    : 11
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(eval-when (eval load)
  ;; This sets *pvs-path* and sets *pvs-binary-type*
  (load "pvs-config.lisp"))

(defpackage pvs (:use #+lucid :lucid-common-lisp :lisp
		      #-(or gcl cmu) :clos #+(or gcl cmu) :pcl))

(in-package :pvs)
(import '(cl-user:*pvs-path*))
(let (#+allegro (excl:*enable-package-locked-errors* nil))
  (load "src/defcl.lisp")
  (load "src/store-object.lisp")
  (load "src/classes-expr.lisp")
  (load "src/classes-decl.lisp")
  (load "src/prover/estructures.lisp"))

(write-deferred-methods-to-file t)

(cl-user:bye)
