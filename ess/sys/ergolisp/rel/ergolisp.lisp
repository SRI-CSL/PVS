;;; -*- Mode: Lisp; Package: ERGOLISP -*-
;;;
;;; General package for combining our general purpose Lisp extensions.
;;;
;;; Author: Conal Elliott.  Last Modified Sat Sep 23 12:38:43 1989
;;;
;;; Sccs Id @(#)ergolisp.lisp	1.6 9/23/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

#-gcl
(defpackage :ergolisp #+sbcl (:use :common-lisp))
(in-package :ergolisp)

(export '(eexport))

(defun eexport (symbol-or-symbols
		&optional (package (package-name *package*)))
  "Ergo-export.  First, check that the given symbol or symbols are already
exported from the ergolisp package.  This should have happened in
ergolisp-exports.lisp.  Then also export them from the current package."
  (flet ((already-exported (sym)
	   (when (package-name (symbol-package sym))
	     (string= (package-name (symbol-package sym))
		      (string :ergolisp)))))
    (let ((symbols (if (listp symbol-or-symbols)
		       symbol-or-symbols
		       (list symbol-or-symbols))))
      (if (every #'already-exported symbols)
	  (export symbols package)
	  (progn
	   (warn "Some of the eexported symbols are not yet exported from ERGOLISP, 
so I won't export them here.  Add to ergolisp-exports.lisp.
  The unexported symbols are ~s."
		 (remove-if #'already-exported symbols))
	   (export (remove-if-not #'already-exported symbols) package))))))
