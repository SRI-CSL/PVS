;;; -*- Package: NEWATTR -*-
;;; Sccs Id 9/21/89 @(#)attr-lang.lisp	1.6
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
;;; This part is "language specific" but "sort generic"
;;; It is implemented using the "sort specific" version, by inventing
;;; a global sort :global.  All other functions just refer to the
;;; "sort-specific" versions of the ADT.

(in-package "NEWATTR")   (use-package :ergolisp)

(export '(defcon defsyn defattr))
(export '(defcon-deltafun defsyn-compfun defattr-attrfun))
(export '(delta-con get-syn get-syn-argn get-attr))

;;; The definition functions.
;;; This cannot merely refer to the corresponding functions for the
;;; context and syntext families, because the deltafun-family
;;; is a singleton.

(defmacro defcon (con-name lang-name &optional (doc-string ""))
  "Predefines a context."
  (let ((deltafun (deltafun-fam-name con-name :global)))
    `(eval-when (compile load eval)
       (setf (gethash ',con-name *context-table*)
	     (make-context-family
	      :name ',con-name
	      :lang-name ',lang-name
	      :deltafun-family ',(list (cons ':global deltafun))
	      :doc-string ,doc-string
	      )))))

(defmacro defsyn (syn-name lang-name con-name &optional (doc-string ""))
  "Predefines a syntext depending on a context."
  (let ((compfun (compfun-fam-name syn-name :global)))
    `(eval-when (compile load eval)
       (let* ((con
	       (sometable-lookup ',con-name *context-table*
				 ,(format nil "The context ~S on which the syntext ~~S
depends has not yet been declared" con-name)))
	      (clang-name (context-family-lang-name con)))
	 (assert (language:sublanguage-p clang-name ',lang-name)
		 () "~S is not a sublanguage of ~S, the language
for which the context ~S is defined." ',lang-name clang-name ',con-name)
	 (setf (gethash ',syn-name *syntext-table*)
	       (make-syntext-family
		:name ',syn-name
		:confam-name ',con-name
		:lang-name ',lang-name
		:compfun-family ',(list (cons ':global compfun))
		:doc-string ,doc-string
		))))))

(defmacro defattr (attr-name lang-name syn-name &optional (doc-string ""))
  "Predefines an attribute by declaring the syntext it depends on."
  (let ((attrfun (attrfun-fam-name attr-name :global)))
    `(eval-when (compile load eval)
       (let* ((syn
	       (sometable-lookup ',syn-name *syntext-table*
				 ,(format nil "The syntext ~S on which the attribute ~~S
depends has not yet been declared" syn-name)))
	      (slang-name (syntext-family-lang-name syn)))
	 (assert (language:sublanguage-p slang-name ',lang-name)
		 () "~S is not a sublanguage of ~S, the language
for which the syntext ~S is defined." ',lang-name slang-name ',syn-name)
	 (setf (gethash ',attr-name *attribute-table*)
	       (make-attribute-family
		:name ',attr-name
		:synfam-name ',syn-name
		:lang-name ',lang-name
		:attrfun-family ',(list (cons ':global attrfun))
		:doc-string ,doc-string
		))))))

;;;
;;; The definition function merely refer to the corresponding
;;; functions for the families.
;;;

(defmacro defcon-deltafun (con-name term-vars con-vars &rest body)
  "Defines the delta function for a predefined context.
The second argument (a list of two variables) binds the number and the term,
the third argument binds the values of the incoming context,
the rest is the body which may start with declaration and should
return multiple values."
  `(defconfam-deltafun ,con-name :global ,term-vars ,con-vars ,@body))


(defmacro defsyn-compfun (syn-name term-vars con-vars &rest body)
  "Defines the computation function for a predefined syntext.
The second argument (a singleton list) is bound to the term,
the third argument (a list) will be bound to the values of the context,
the rest is the body of the function, which may start with declarations."
  `(defsynfam-compfun ,syn-name :global ,term-vars ,con-vars ,@body))

(defmacro defattr-attrfun (attr-name term-vars con-vars syn-vars
				     &rest body)
  "Defines an attribute function of a predefined attribute.
Second argument is the singleton bound to the term,
third argument is a list and will bind the context values,
fourth argument is a list and will bind the syntext values,
rest is the body, which may start with declarations and should return
a single value."
  `(defattrfam-attrfun ,attr-name :global ,term-vars ,con-vars ,syn-vars
     ,@body))

;;; 
;;; The delta and computation function which merely refer to the
;;; corresponding functions for families.

(defmacro delta-con (con-name n term &rest context-values)
  "Returns the values for the context after descent
to the Nth argument of TERM."
  `(delta-confam ,con-name :global ,n ,term ,@context-values))

(defmacro get-syn-argn (syn-name n term &rest context-values)
  "Returns the values of the given syntext for the Nth argument of TERM."
  `(get-synfam-argn ,syn-name :global :global ,n ,term ,@context-values))

(defmacro get-syn (syn-name term &rest context-values)
  "Returns the values of the given syntext for the term."
  `(get-synfam ,syn-name :global ,term ,@context-values))

(defmacro get-attr (attr-name term &rest context-values)
  "Returns the values for the given attribute."
  `(get-attrfam ,attr-name :global ,term ,@context-values))
