;;; -*- Mode: Lisp; Package: DLAMBDA-LIB -*-
;;;
;;; Destructuring support library.
;;;
;;; Author: Conal Elliott.  Last Modified Sat Sep 23 12:39:17 1989
;;;
;;; Sccs Id @(#)dlambda-lib.lisp	1.6 9/23/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

#-gcl
(defpackage :dlambda-lib #+sbcl (:use :common-lisp :ergolisp))
(in-package :dlambda-lib) #-sbcl (use-package :ergolisp)

(eexport '(re-cons re-acons re-mapcar))


(declare-constructor cons (first rest))


(declare-constructor 1+ (1-) positivep)

(defun positivep (n)
  "Is the integer N positive?"
  (declare (integer n))
  (> n 0))


(declare-constructor intern (symbol-name symbol-package) symbolp)


;; A list is its own reversal
(declare-constructor reverse (reverse) listp)


(declare-constructor acons (caar cdar rest))

(defun aconsp (x) (and (consp x) (consp (car x))))


;;; Seems like an appropriate place for this.
(defun re-mapcar (fun listarg)
  "Like mapcar, but tries to reuse cons cells.  For now, only takes one list.
Type is ``'a -> 'b # 'a* -> 'b*''."
  (dcase listarg
    (() ())
    ((cons first rest)
     (re-cons listarg first rest
	      (funcall fun first)
	      (re-mapcar fun rest)))))
