;;; -*- Package: NEWATTR -*-
;;; Sccs Id 9/21/89 @(#)attr-sort.lisp	2.7
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; The new attribute ADT.
;;; The sort-specific part.
;;; Author: fp

#-gcl
(defpackage :newattr #+sbcl (:use :common-lisp :ergolisp))
(in-package :newattr) #-sbcl(use-package :ergolisp)

(export '(defconfam defsynfam defattrfam))
(export '(defconfam-deltafun defsynfam-compfun defattrfam-attrfun))
(export '(delta-confam get-synfam get-synfam-argn get-attrfam))

;;; The representation of contexts, syntexts, and attributes.

(defstruct context-family
  "A family of contexts in the attribute ADT."
  name					; Its name.
  lang-name				; The language it is defined for.
  deltafun-family			; The delta functions.
  doc-string				; Optional documentation.
  )

(defstruct syntext-family
  "A family of syntexts in the attribute ADT."
  name					; Its name.
  confam-name				; The context family it depends on.
  lang-name
  compfun-family			; The computation function.
  doc-string				; Optional documentation.
  )

(defstruct attribute-family
  "An Attribute in the attribute ADT."
  name					; Its name.
  synfam-name				; The syntext family it depends on.
  lang-name
  attrfun-family			; The computation function.
  doc-string				; Optional documentation.
  )

;;; Misc support function.  Some of them should eventually be moved.

(defmacro term-nth (n term)
  "Like NTH, except for terms.  There is also term-argn, which reversed
the arguments."
  `(term:term-argn ,term ,n))

;;; The global tables and lookup function.

(defvar *context-table* (make-hash-table :test #'eq)
  "Hash table of all known contexts.  Key is the name.")

(defvar *syntext-table* (make-hash-table :test #'eq)
  "Hash table of all known syntexts.  Key is the name.")

(defvar *attribute-table* (make-hash-table :test #'eq)
  "Hash table of all known attributes.  Key is the name.")

(defun sometable-lookup (name table error-string)
  "Looks up NAME in TABLE, reporting a missing entry with ERROR-STRING"
  (let (result)
    (assert (not (null (setq result (gethash name table))))
	    (name) error-string name)
    result))

(defun sometable-get (name table &optional (default nil))
  (gethash name table default))

(defun context-name-p (name)
  (sometable-get name *context-table*))

(defun syntext-name-p (name)
  (sometable-get name *syntext-table*))

(defun attribute-name-p (name)
  (sometable-get name *attribute-table*))

;;; Naming conventions.

(defun deltafun-fam-name (confam-name sort)
  "Given the name of a context family and a sort,
generates a name for its delta function."
  (intern (concatenate 'string (string confam-name) "-"
		        (string sort) "-DELTAFUN")))

(defun compfun-fam-name (synfam-name sort)
  "Given the name of a syntext family and a sort,
generates a name for its comp function."
  (intern (concatenate 'string (string synfam-name) "-"
		       (string sort) "-COMPFUN")))

(defun attrfun-fam-name (attrfam-name sort)
  "Given the name of an attribute family and a sort,
generates a name for its attr function."
  (intern (concatenate 'string (string attrfam-name) "-"
		       (string sort) "-ATTRFUN")))

;;; The definition functions.
;;; There are two for each type of object: one declares the dependencies,
;;; the other defines the functions computing the values.

(defmacro defconfam (confam-name lang-name &optional (doc-string ""))
  "Predefines a context family."
  (let* ((sortlist (language:language-sortlist lang-name))
	 (deltafun-alist
	  (mapcar #'(lambda (sort)
		      (cons sort (deltafun-fam-name confam-name sort)))
		  sortlist)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',confam-name *context-table*)
	     (make-context-family
	      :name ',confam-name
	      :lang-name ',lang-name
	      :deltafun-family ',deltafun-alist
	      :doc-string ,doc-string
	      )))))

(defmacro defconfam-deltafun (confam-name sort term-vars con-vars &rest body)
  "Defines the delta function for one sort of a predefined context family.
The third argument (a list of two variables) binds the number and the term,
the fourth argument binds the values of the incoming context,
the rest is the body which may start with declaration and should
return multiple values."
  (let ((deltafun (deltafun-fam-name confam-name sort))
	(vars (append term-vars con-vars)))
    `(progn
       (sometable-lookup ',confam-name *context-table*
			 "You are trying to declare the delta function for context family ~S,
which has not yet been defined.")
       (defun ,deltafun ,vars ,@body)
       ',deltafun)))

(defmacro defsynfam (synfam-name lang-name confam-name
				 &optional (doc-string ""))
  "Predefines a syntext family depending on a context family."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((con-fam
	     (sometable-lookup ',confam-name *context-table*
			       ,(format nil "The context family ~S on which the syntext family ~~S
depends has not yet been declared" confam-name)))
	    (clang-name (context-family-lang-name con-fam))
	    (sortlist (language:language-sortlist ',lang-name))
	    (compfun-alist
	     (mapcar #'(lambda (sort)
			 (cons sort (compfun-fam-name ',synfam-name sort)))
		     sortlist)))
       (assert (language:sublanguage-p ',lang-name clang-name)
	       () "~S is not a sublanguage of ~S, the language
for which the context ~S is defined." ',lang-name clang-name ',confam-name)
       (setf (gethash ',synfam-name *syntext-table*)
	     (make-syntext-family
	      :name ',synfam-name
	      :confam-name ',confam-name
	      :lang-name ',lang-name
	      :compfun-family compfun-alist
	      :doc-string ,doc-string
	      )))))

(defmacro defsynfam-compfun (synfam-name sort term-vars con-vars &rest body)
  "Defines the computation function for a sort for a predefined syntext family.
The third argument (a singleton list) is bound to the term,
the fourth argument (a list) will be bound to the values of the context,
the rest is the body of the function, which may start with declarations."
  (let ((compfun (compfun-fam-name synfam-name sort))
	(vars (append term-vars con-vars)))
    `(let* ((syn-fam (sometable-lookup ',synfam-name *syntext-table*
				       "You are trying to declare the computation function for syntext family ~S,
which has not yet been defined."))
	    (con-fam (sometable-lookup (syntext-family-confam-name syn-fam)
				       *context-table*
				       (format nil "The context ~S on which the syntext ~~S
depends has not yet been declared" (syntext-family-confam-name syn-fam)))))
       (declare (ignore con-fam))
       (defun ,compfun ,vars ,@body)
       ',compfun)))

(defmacro defattrfam (attrfam-name lang-name synfam-name
				   &optional (doc-string ""))
  "Predefines an attribute family depending on a syntext family."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let* ((syn-fam
	     (sometable-lookup ',synfam-name *syntext-table*
			       ,(format nil "The syntext family ~S on which the attribute family ~~S
depends has not yet been declared" attrfam-name)))
	    (slang-name (syntext-family-lang-name syn-fam))
	    (sortlist (language:language-sortlist ',lang-name))
	    (attrfun-alist
	     (mapcar #'(lambda (sort)
			 (cons sort (attrfun-fam-name ',attrfam-name sort)))
		     sortlist)))
       (assert (language:sublanguage-p ',lang-name slang-name)
	       () "~S is not a sublanguage of ~S, the language
for which the syntext ~S is defined." ',lang-name slang-name ',synfam-name)
       (setf (gethash ',attrfam-name *attribute-table*)
	     (make-attribute-family
	      :name ',attrfam-name
	      :synfam-name ',synfam-name
	      :lang-name ',lang-name
	      :attrfun-family attrfun-alist
	      :doc-string ,doc-string
	      )))))

(defmacro defattrfam-attrfun (attrfam-name sort term-vars con-vars syn-vars
					&rest body)
  "Defines an attribute function of a predefined attribute.
Third argument is the singleton bound to the term,
fourth argument is a list and will bind the context values,
fifth argument is a list and will bind the syntext values,
rest is the body, which may start with declarations and should return
a single value."
  (let ((attrfun (attrfun-fam-name attrfam-name sort))
	(vars (append term-vars con-vars syn-vars)))
    `(let* ((attr-fam (sometable-lookup ',attrfam-name *attribute-table*
					 "You are trying to declare the attribute function for attribute ~S,
which has not yet been defined."))
	    (syn-fam (sometable-lookup (attribute-family-synfam-name attr-fam)
				       *syntext-table*
				       (format nil "The syntext family ~S on which the attribute family ~~S
depends has not yet been declared" (attribute-family-synfam-name attr-fam)))))
       (declare (ignore syn-fam))
       (defun ,attrfun ,vars ,@body)
       ',attrfun)))

;;; The retrieval functions.

(defmacro sortassoc (sort-place alist)
  (let ((resultvar (gensym)))
    `(let ((,resultvar (assoc ,sort-place ,alist :test #'language:subsort-p)))
       (assert (not (null ,resultvar)) (,sort-place)
	       "Sort ~S is not in the alist ~S" ,sort-place ,alist)
       ,resultvar)))


;;; Context families.

(defun confam-coerce (confam)
  (ctypecase confam
    (context-family confam)
    (symbol (sometable-lookup confam *context-table*
			      "~S is not a known context family."))))

(defun confam-deltafun (confam sort)
  "Retrieves the delta function for the given context family and sort."
  (cdr (sortassoc
	sort
	(context-family-deltafun-family
	 (confam-coerce confam)))))

(defun confam-deltafuns (confam &optional (sorts t))
  (let ((confam (confam-coerce confam)))
    (if (or (eq sorts t) (eq sorts :global))
	(mapcar #'cdr (context-family-deltafun-family confam))
	(ctypecase sorts
	  (symbol (list (confam-deltafun confam sorts)))
	  (list (mapcar #'confam-deltafun confam sorts))))))

;;; Syntext families.

(defun synfam-coerce (synfam)
  (ctypecase synfam
    (syntext-family synfam)
    (symbol (sometable-lookup synfam *syntext-table*
			      "~S is not a known syntext family."))))  

(defun synfam-confam (synfam)
  "Retrieves the context for the given syntext (name)."
  (syntext-family-confam-name (synfam-coerce synfam)))

(defun synfam-compfun (synfam sort)
  "Retrieves the comp function for the given syntext (name)."
  (cdr (sortassoc
	sort
	(syntext-family-compfun-family (synfam-coerce synfam)))))

(defun synfam-compfuns (synfam &optional (sorts t))
  (let ((synfam (synfam-coerce synfam)))
    (if (or (eq sorts t) (eq sorts :global))
	(mapcar #'cdr (syntext-family-compfun-family synfam))
	(ctypecase sorts
	  (symbol (list (synfam-compfun synfam sorts)))
	  (list (mapcar #'synfam-compfun synfam sorts))))))

;;; Attribute families.

(defun attrfam-coerce (attrfam)
  (ctypecase attrfam
    (attribute-family attrfam)
    (symbol (sometable-lookup attrfam *attribute-table*
			      "~S is not a known attribute family."))))

(defun attrfam-synfam (attrfam)
  "Retrieves the syntext for the given attribute (name)."
  (attribute-family-synfam-name (attrfam-coerce attrfam)))

(defun attrfam-attrfun (attrfam sort)
  "Retrieves the comp function for the given attribute (name)."
  (cdr (sortassoc sort
		  (attribute-family-attrfun-family
		   (attrfam-coerce attrfam)))))

(defun attrfam-attrfuns (attrfam &optional (sorts t))
  (let ((attrfam (attrfam-coerce attrfam)))
    (if (or (eq sorts t) (eq sorts :global))
	(mapcar #'cdr (attribute-family-attrfun-family attrfam))
	(ctypecase sorts
	  (symbol (list (attrfam-attrfun attrfam sorts)))
	  (list (mapcar #'attrfam-attrfun attrfam sorts))))))

;;; These first two are universal.  They are not affected right
;;; now by caching.

(defmacro delta-confam (confam-name sort n term &rest context-values)
  "Returns the values for the context family after descent
to the Nth argument of TERM."
  `(,(confam-deltafun confam-name sort) ,n ,term ,@context-values))

(defmacro get-attrfam (attrfam-name sort term &rest context-values)
  "Returns the values for the given attribute family."
  (let ((termsym (gensym))
	(contextsym-list (mapcar #'(lambda (cv) (declare (ignore cv))
				     (gensym))
				 context-values)))
    `(let ((,termsym ,term)
	   ,@(mapcar #'list contextsym-list context-values))
       (multiple-value-call ',(attrfam-attrfun attrfam-name sort)
	 ,termsym
	 ,@contextsym-list
	 (get-synfam ,(attrfam-synfam attrfam-name) ,sort ,termsym ,@contextsym-list)))))

(defmacro get-synfam-argn (synfam-name sort child-sort
				       n term &rest context-values)
  "Returns the values of the given syntext for the Nth argument of TERM."
  (let ((nsym (gensym)) (termsym (gensym)))
    `(let ((,nsym ,n) (,termsym ,term))
       (get-synfam-cvsform ,synfam-name ,child-sort
	(term-nth ,nsym ,termsym)
	(delta-confam ,(synfam-confam synfam-name) ,sort ,nsym ,termsym
		      ,@context-values)))))

;;; The computation macros.  Their expansion is somewhat tricky, because
;;; it should depend on some declarations.
;;; Will need to find a graceful way of setting the compile-time options.

(defvar *attr-cache* t
  "If T, cache a mapping from context information to syntext information.")

(defmacro get-synfam (synfam-name sort term &rest context-values)
  "Returns the values of the given syntext family for the term."
  (if *attr-cache*
      `(get-synfam%cache ,synfam-name ,sort ,term ,@context-values)
      `(get-synfam%compute ,synfam-name ,sort ,term ,@context-values)))

(defmacro get-synfam-cvsform (synfam-name sort term context-values-form)
    (if *attr-cache*
      `(get-synfam-cvsform%cache ,synfam-name ,sort ,term ,context-values-form)
      `(get-synfam-cvsform%compute ,synfam-name ,sort ,term ,context-values-form)))

;;; For now, we expand very simply - No caching in this version of
;;; the macros.

(defmacro get-synfam%compute (synfam-name sort term &rest context-values)
  "Returns the values of the given syntext for the term.
The remaining arguments form the context.
This version of the function does no lookup or caching."
  `(,(synfam-compfun synfam-name sort) ,term ,@context-values))

(defmacro get-synfam-cvsform%compute (synfam-name sort term context-values-form)
  "Return the values of the given syntext for the term.  The last
argument is a form which returns the context as multiple values.
This version of the function does no lookup or caching."
  `(multiple-value-call ',(synfam-compfun synfam-name sort) ,term
			,context-values-form))


;;; The next set of macros could be used for caching and lookup
;;; of a mapping from context to syntexts (given the term).

;;; The attribute slot of a term contains an alist
;;; ((syn-name . ((con-valuelist . syn-valuelist) ...)) ...)
;;; This could be optimized significantly, if a syntext
;;; had a fixed position within a vector.
;;; Also, con-valuelist could be ordered, or even be a tree
;;; (eg for occurrences).
;;; This method requires a lot of consing, because the valuelist has to
;;; be made.  Special purpose functions (if there are 0 or 1 values) may
;;; be useful later.
;;; Also, one could change the structure of the recursion by passing
;;; lists, instead of multiple values.  This would save consing at least
;;; when there is no change in the syntext.

;;; First general caching functions.

(defun find-syn (syn-name term)
  "Return the place of the given syntext in the attribute list for term,
or NIL if it doesn't exist yet."
  (assoc syn-name (term:term-attr term) :test #'eq))

(defun set-syn (syn-name term)
  "Make a place for the given syntext in the attribute list of term,
and return that place."
  (let ((syn-place (cons syn-name nil)))
    (push syn-place (term:term-attr term))
    syn-place))

(defun find-syn-values (syn-place context-values)
  "Given the place of a syntext in the attribute list, return the values
of the syntext, or NIL if they were not yet cached."
  (cdr (assoc context-values (cdr syn-place) :test #'equal)))

(defun set-syn-values (syn-place syn-name term context-values syntext-values)
  "Given the place of the syntext in the attribute list, store the
syntext values given the context values, and return them as a list."
  (declare (ignore syn-name term))
  (push (cons context-values syntext-values) (cdr syn-place))
  syntext-values)

;;; Should distinguish special case of empty syntext or context.

(defmacro get-synfam-cache%cache (synfam-name sort termsym contextsym)
  "Given a syntext name, a term and a list with the values of the context,
it will lookup, possibly compute, and cache the values of the syntext.
Term and context should be bound variables, since they may be evaluated
several times."
  `(let* ((syn-place (or (find-syn ',synfam-name ,termsym)
			 (set-syn ',synfam-name ,termsym)))
	  (syn-values (or (find-syn-values syn-place ,contextsym)
			  (set-syn-values
			   syn-place ',synfam-name
			   ,termsym ,contextsym
			   (multiple-value-list
			    ;; Below we could have #',(...), but it
			    ;; helps debugging if its only ',(...).
			    (apply ',(synfam-compfun synfam-name sort)
				   ,termsym
				   ,contextsym))))))
     (values-list syn-values)))

(defmacro get-synfam%cache (synfam-name sort term &rest context-values)
  "Returns the values of the given syntext for the term.
This does caching."
  (let ((termsym (gensym)) (contextsym (gensym)))
    `(let* ((,termsym ,term)
	    (,contextsym (list ,@context-values)))
       (get-synfam-cache%cache ,synfam-name ,sort ,termsym ,contextsym))))

(defmacro get-synfam-cvsform%cache (synfam-name sort term context-values-form)
  "Returns the values of the given syntext, where context-values-form
returns (as multiple values) the values of the context.
This version of the macro does caching."
  (let ((termsym (gensym)) (contextsym (gensym)))
    `(let* ((,termsym ,term)
	    (,contextsym (multiple-value-list ,context-values-form)))
       (get-synfam-cache%cache ,synfam-name ,sort ,termsym ,contextsym))))

