;;; -*- Package: NEWATTR -*-
;;; Sccs Id 9/21/89 @(#)attr-occ.lisp	1.3
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; The context of a term-occurrence, which is a pair of the "root"
;;;  together with an "occurrence" or "path" or "tree address".
;;;
;;; Author: fp

(in-package "NEWATTR")   (use-package :ergolisp)

(export '(revocc tocc))			; Global context and attribute.

(export '(get-syn-at get-attr-at))	; Macros to access syntext and
					; attribute at a subterm, eg after
					; pointing.

;;; The current ADT of occurrence lists the occurrences in their
;;; natural order, with no pointer to the end of the list.  So the context
;;; only calculates the reverse of an occurrence (in the form of a list).
;;; The attribute is the occurrence (but not the term-occurrence right now).

(defgcon revocc
  "The global context of a reverse term occurrence.")

(defgcon-deltafun revocc (n term) (root revocc)
  (declare (ignore term))
  (vls root (cons n revocc)))

(defemptygsyn tocc-syn revocc)

(defgattr tocc tocc-syn
  "The global attribute of an occurrence in the context of a reverse
term occurrence.")

(defgattr-attrfun tocc (term) (root revocc) ()
  (declare (ignore term root))
  (vls (occ:list-to-occ (reverse revocc))))

;;; 
;;; Next section implements the walk through a term, given an
;;; occurrence, and a root.  This is like get-syn-argn,
;;; except that it takes an occurrence.
;;;
;;; Right now I am not defining this for sort-specific syntexts.
;;;


(defmacro get-syn-at (syn-name occ term &rest context-values)
  "Returns the value of the given syntext for the subterm of TERM
at OCC."
  (let ((local-label (gensym))
	(con-name (synfam-confam syn-name)))
    `(labels ((,local-label
	       (occ term &rest context-values)
	       (if (occ:null-occ occ)
		   (get-synfam-cvsform ,syn-name :global
				       term (values-list context-values))
		   (multiple-value-call
		       #',local-label (occ:occ-rest occ)
		       (term:term-argn term (occ:occ-top occ))
		       (apply ',(confam-deltafun con-name :global)
			      (occ:occ-top occ) term context-values)))))
       (,local-label ,occ ,term ,@context-values))))

(defmacro get-attr-at (attr-name occ term &rest context-values)
  "Returns the value of the given syntext for the subterm of TERM
at OCC."
  (let* ((local-label (gensym))
	 (syn-name (attrfam-synfam attr-name))
	 (con-name (synfam-confam syn-name)))
    `(labels ((,local-label
	       (occ term &rest context-values)
	       (if (occ:null-occ occ)
		   (multiple-value-call
		       ',(attrfam-attrfun attr-name :global)
		     term
		     (values-list context-values)
		     (get-synfam-cvsform ,syn-name :global
					 term (values-list context-values)))
		   (multiple-value-call
		       #',local-label (occ:occ-rest occ)
		       (term:term-argn term (occ:occ-top occ))
		       (apply ',(confam-deltafun con-name :global)
			      (occ:occ-top occ) term context-values)))))
       (,local-label ,occ ,term ,@context-values))))
