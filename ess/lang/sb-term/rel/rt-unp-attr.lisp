;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-unp-attr.lisp	1.12 9/22/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Tue Oct  6 15:32:22 1987

(in-package 'sb-runtime)  (use-package :ergolisp)


;;; The following is a hack to avoid the problems inherent in the circularity
;;; between the two packages. 

(cond ((find-package "NEWATTR"))
      (t
       (make-package "NEWATTR")))


(export '(theuterm theaw
	  memo-uterm memo-aw
	  *disable-caching* *disable-uterm-caching* *disable-aw-caching*))




(defvar *disable-caching* nil
  "Setting non-nil disables the caching of unparsing results via the attribute
   ADT.")

(defvar *disable-nested-caching* t
  "Setting nil enables the caching of nested uterms via the attribute
   ADT.  Set non-nil BY DEFAULT.") 




(defun memo-uterm (term unp-function &key (top-level? nil))
  (if (or *disable-caching*
	  (and *disable-nested-caching*
	       (null top-level?)))
      (funcall unp-function term)
      (newattr::get-gsyn theuterm
			 term
			 (list unp-function
			       *unparse-style*
			       *no-escapes*
			       *sb-print-depth*
			       *sb-print-length*
			       *formatting-off*))))


(defun memo-aw (uterm width indent-unit-width fontwidth fontheight)
  (if *disable-caching*
      (let* ((aw (make-aw :uterm uterm
			  :indent-unit-width indent-unit-width)))
	(format-aw uterm aw width))
      (newattr::get-gsyn theaw
			 (uterm-term uterm)
			 (list uterm
			       width
			       indent-unit-width
			       fontwidth
			       fontheight))))





(newattr::defgcon uterm-args)
(newattr::defgsyn theuterm uterm-args)

(newattr::defgsyn-compfun theuterm (term)
			  (uterm-args)
  "The Uterm (unparsed-term) of a term given the unparsing function and 
   style contexts"
  (multiple-value-bind
      (unp-function style insert-escapes depth length no-form)
      (values-list uterm-args)
    (declare (ignore style insert-escapes depth length no-form))
	     ; globally bound. 
    (funcall unp-function term)))   


(newattr::defgcon aw-args)
(newattr::defgsyn theaw aw-args)

(newattr::defgsyn-compfun theaw (term)
			  (aw-args)
  "The AW (abstract window) assoiciated with a TERM (and UTERM) 
   given width and unit-indent-width contexts."
  (declare (ignore term))
  (multiple-value-bind
      (uterm width indent-unit-width fontwidth fontheight)
      (values-list aw-args)
    (declare (ignore fontwidth fontheight)) ; globally bound
    (let* ((aw (make-aw :uterm uterm
			:indent-unit-width indent-unit-width)))
      (format-aw uterm aw width))))

