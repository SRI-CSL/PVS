;;; -*- Package: NEWATTR -*-
;;; Sccs Id 9/21/89 @(#)attr-gsort.lisp	1.4
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; The global attribute of the term's sort.  This is inefficient and could
;;; be replace with a (deflangsort <name> <lang>) macro, but right now
;;; they couldn't easily be incoporated into one context.  
;;;
;;; Author: fp

(in-package "NEWATTR")  (use-package :ergolisp)

(export '(sort))			; Global context and attribute.

(export '(numval idsym strval))		; To retrieve value of a number.
					; symbol of an identifier.
					; Lisp string of a string.

(defgattr sort empty-gsyn
  "The global attribute of the sort of a term.")

(defgattr-attrfun sort (term) () ()
  "The way this is defined it requires lookup in a global hash-table."
  (language:op-sort (term:term-op term)))

(defattrfam numval sb-runtime::lexical-terminals empty-gsyn
  "The attribute with the value of a number.")

(defattrfam-attrfun numval sb-runtime::number (term) () ()
  (sb-runtime::ds-number term))

(defattrfam idsym sb-runtime::lexical-terminals empty-gsyn
  "The attribute with the symbol of an identifier.")

(defattrfam-attrfun idsym sb-runtime::id (term) () ()
  (sb-runtime::ds-id term))

(defattrfam strval sb-runtime::lexical-terminals empty-gsyn
  "The attribute with the value of a string.")

(defattrfam-attrfun strval sb-runtime::string (term) () ()
  (sb-runtime::ds-string term))




