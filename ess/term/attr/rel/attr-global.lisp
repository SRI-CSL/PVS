;;; -*- Package: NEWATTR -*-
;;; Sccs Id 9/21/89 @(#)attr-global.lisp	1.5
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; The new attribute ADT.
;;; Author: fp
;;;
;;; This part is "language generic" and "sort generic"
;;; It is implemented using the "language specific" version, by inventing
;;; a global language :global.  All other functions just refer to the
;;; "language-specific" versions of the ADT.

(in-package "NEWATTR")  (use-package :ergolisp)

(export '(defgcon defgsyn defgattr))
(export '(defgcon-deltafun defgsyn-compfun defgattr-attrfun))
(export '(delta-gcon get-gsyn get-gsyn-argn get-gattr))

;;; The definition functions.
;;; Simple refering to the language-specific macros.

(defmacro defgcon (gcon-name &optional (doc-string ""))
  "Predefines a global context."
  `(defcon ,gcon-name :global ,doc-string))

(defmacro defgsyn (gsyn-name gcon-name &optional (doc-string ""))
  "Predefines a global syntext depending on a global context."
  `(defsyn ,gsyn-name :global ,gcon-name ,doc-string))

(defmacro defgattr (gattr-name gsyn-name &optional (doc-string ""))
  "Predefines an attribute by declaring the syntext it depends on."
  `(defattr ,gattr-name :global ,gsyn-name ,doc-string))

;;;
;;; The definition function merely refer to the corresponding
;;; functions for the language-specific versions.
;;;

(defmacro defgcon-deltafun (gcon-name term-vars gcon-vars &rest body)
  "Defines the delta function for a predefined global context.
The second argument (a list of two variables) binds the number and the term,
the third argument binds the values of the incoming context,
the rest is the body which may start with declaration and should
return multiple values."
  `(defcon-deltafun ,gcon-name ,term-vars ,gcon-vars ,@body))


(defmacro defgsyn-compfun (gsyn-name term-vars gcon-vars &rest body)
  "Defines the computation function for a predefined global syntext.
The second argument (a singleton list) is bound to the term,
the third argument (a list) will be bound to the values of the context,
the rest is the body of the function, which may start with declarations."
  `(defsyn-compfun ,gsyn-name ,term-vars ,gcon-vars ,@body))

(defmacro defgattr-attrfun (gattr-name term-vars gcon-vars gsyn-vars
				       &rest body)
  "Defines an attribute function of a predefined global attribute.
Second argument is the singleton bound to the term,
third argument is a list and will bind the context values,
fourth argument is a list and will bind the syntext values,
rest is the body, which may start with declarations and should return
a single value."
  `(defattr-attrfun ,gattr-name ,term-vars ,gcon-vars ,gsyn-vars
     ,@body))

;;; 
;;; The delta and computation function which merely refer to the
;;; corresponding functions for families.

(defmacro delta-gcon (gcon-name n term &rest context-values)
  "Returns the values for the context after descent
to the Nth argument of TERM."
  `(delta-con ,gcon-name ,n ,term ,@context-values))

(defmacro get-gsyn-argn (gsyn-name n term &rest context-values)
  "Returns the values of the given syntext for the Nth argument of TERM."
  `(get-syn-argn ,gsyn-name ,n ,term ,@context-values))

(defmacro get-gsyn (gsyn-name term &rest context-values)
  "Returns the values of the given syntext for the term."
  `(get-syn ,gsyn-name ,term ,@context-values))

(defmacro get-gattr (gattr-name term &rest context-values)
  "Returns the values for the given attribute."
  `(get-attr ,gattr-name ,term ,@context-values))
