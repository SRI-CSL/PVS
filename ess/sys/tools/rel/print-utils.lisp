;;; -*- Package: print-utils -*-

;;; Various useful printing-related utilities

;;; Author: Conal

#-gcl
(defpackage "PRINT-UTILS")
(in-package "PRINT-UTILS") (use-package :ergolisp)

(export '(lisp::print-struct lisp::writing-readably)
	:lisp)
(export '(print-struct writing-readably))


(defmacro print-struct (obj stream struct &rest slots)
  "Macro to print a structure that can be read in again"
  ;; Ought to deal with depth too
  (let ((format-string (format nil "#S( ~~S ~{ ~~S ~~S~*~})" slots)))
    `(format ,stream ,format-string ',struct
	    ,@(mapcan #'(lambda (slot)
			  (list `',slot `(,(slot-func-name struct slot) ,obj)))
		      slots))))



(defun slot-func-name (struct slot)
  "Build the name of a slot-accessor function for a given structure and slot.
Put the resulting symbol in the same package as the structure."
  (intern (concatenate 'string (symbol-name struct) "-" (symbol-name slot))
	  (symbol-package struct)))




(defmacro writing-readably (&rest forms)
  "Macro to wrap around some forms, causing their writing to suitable for
reading back in."
  `(let ((*print-escape* t)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*print-array* t)
	 #+(and lucid (not lcl3.0)) (system:*print-structure* t)
	 #+(and lucid lcl3.0) (lcl:*print-structure* t)
	 )
     ,@forms))

