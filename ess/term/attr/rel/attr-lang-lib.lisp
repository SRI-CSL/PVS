;;; -*- Mode: Lisp; Package: LANGUAGE -*-
;;; Sccs Id 9/21/89 @(#)attr-lang-lib.lisp	1.3
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; Interface to Lang Package
;;; Author: fp
;;;
;;; Interface to Language, Sort, Operator Packages.

#-gcl
(defpackage "LANGUAGE")
(in-package "LANGUAGE")  (use-package :ergolisp)

(export '(op-sort check-op))

(export '(subsort-p))
(export '(language-sortlist sublanguage-p))

(defun op-p (op)
  "Returns true iff op is an operator structure or names an operator."
  (cond ((oper:operp op) t)
	((symbolp op) (sort:opsig-table-lookup (oper:mk-sim-op op)))
	(t nil)))
  
(defun check-op (op)
  "Checks that the argument is or names a known argument."
  (assert (op-p op) (op) "~S is not a known operator." op))

(defun language-sortlist (lang)
  "Returns the list of sorts of LANG, which is coerced into a language."
  (let* ((lang-struct (lang:coerce-find-lang lang))
	 (sort-table-name (lang:lang-sort-table-name lang-struct))
	 (sublangs (lang:lang-sub-languages lang-struct)))
    (append
     (mapcar #'(lambda (entry)
		 (let ((sort-ttype (sort:ds-sort-ttype (car entry))))
		   (if (oper:is-sim-op sort-ttype)
		       (oper:ds-sim-op sort-ttype)
		       sort-ttype)))
	     (sort:sort-table-contents
	      (symbol-value sort-table-name)))
     (mapcan #'language-sortlist sublangs))))

(defun coerce-opstruct (op)
  "Returns the operator structure."
  (cond ((oper:operp op) op)
	((symbolp op) (oper:mk-sim-op op))))

(defun op-sort (op)
  "Returns the sort of the given operator"
  (let ((opsig (sort:opsig-table-lookup (coerce-opstruct op))))
    (sort:ds-sort-ttype (sort:opsig-output opsig))))

;;; This will do for now

(defun subsort-p (subsort sort)
  (if (eq sort :global) t
      (eq subsort sort)))

(defun sublang-struct-p (sublang-struct lang-struct)
  (if (eq lang-struct sublang-struct)
      t
      (some #'(lambda (slang)
		(sublang-struct-p sublang-struct
				  (lang:coerce-find-lang slang)))
	    (lang:lang-sub-languages lang-struct))))

(defun sublanguage-p (sublang lang)
  "Checks if SUBLANG is (eventually) a sublanguage of LANG."
  (if (eq lang :global)
      t
      (if (eq sublang :global)
	  nil
	  (sublang-struct-p (lang:coerce-find-lang sublang)
			    (lang:coerce-find-lang lang)))))
