;;; -*- Mode: Lisp; Package: ERGO-TYPES -*-
;;;
;;; Misc generally useful types.  Part of ERGOLISP
;;;
;;; Last Modified Sat Sep 23 12:38:54 1989
;;;
;;; Sccs Id @(#)ergo-types.lisp	1.3 9/23/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(defpackage "ERGO-TYPES")
(in-package :ERGO-TYPES) (use-package :ergolisp)

(eexport '(Nat Optional Ref
	       Boolean boolean-equal List-of
	       Sequence-of Function-of
	       Alist-of Hash-table-of))

(deftype Nat ()
  "The type of natural nubmers"
  `(and integer (satisfies non-negative-p)))

(defun non-negative-p (i)
  (>= i 0))

(deftype Optional (type)
  "A type composed of TYPE or NIL.  Often used to signal failure (e.g. assoc)."
  `(or ,type Null))

(deftype Ref (type)
  "Means the same as the argument type, but serves as documentation that this
function argument, constr field, or whatever, will be destructively modified."
  type)

(deftype Boolean (&rest doc-args)
  "Boolean type, i.e. NIL or T.  Arguments may be given for documentation or
other purposes."
  (declare (ignore doc-args))
  '(member nil t))

(proclaim '(inline boolean-equal))
(defun boolean-equal (b1 b2)
  "Test to booleans for equality.  Useful for automatically generated equality
functions.  (see constr.{lisp,txt})"
  (eq b1 b2))

(deftype List-of (type)
  "List of elements of a given type.  Element type given for documentation
only, not checked."
  (declare (ignore type))
  `List)

(deftype Sequence-of (type)
  "Sequence of elements of a given type.  Element type given for documentation
only, not checked."
  (declare (ignore type))
  `Sequence)



(deftype Function-of (arg-types &rest result-types)
  "Function from given types to given types.  Types given for documentation
only, not checked."
  (declare (ignore arg-types result-types))
  `Function)

(deftype Alist-of (domain range)
  "Association list, mapping from DOMAIN to RANGE.  Types given for
documentation only, not checked."
  (declare (ignore domain range))
  `list)

(deftype Hash-table-of (domain range test)
  "Hash-table type mapping from DOMAIN to RANGE using given TEST (e.g. #'EQ).
These types are given for documentation only, and are not checked."
  (declare (ignore domain range test))
  `Hash-table)
