;;; -*- Mode: Lisp; Package: ERGOLISP -*-
;;;
;;; All symbols that will be eexported should have corresponding exports here.
;;;
;;; Author: Conal Elliott.  Last Modified Sat Sep 23 12:38:28 1989
;;;
;;; Sccs Id @(#)ergolisp-exports.lisp	1.9 9/26/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


(in-package :ergolisp)

;;; ERGO-TYPES

(export '(Nat Optional Ref
	      Boolean boolean-equal List-of
	      Sequence-of Function-of
	      Alist-of Hash-table-of))

;;;; ERGO-SYSTEM

#+(and lucid lcl3.0)
(import '(lcl:memq) :ergolisp)
#+(and lucid (not lcl3.0))
(import '(system:memq) :ergolisp)
#+cmu
(import '(extensions:memq) :ergolisp)
#+allegro-v4.2
(import '(excl:memq) :ergolisp)

(export '(memq))
#+harlequin-common-lisp
(export '(assq))
(export '(ergo-disksave def-disksave-hook mover))

;;;; TYPE-CHECK

;;; Programming support
(export '(declare-ftype proclaim-ftype))

;;; Runtime environment support.  (Should they be eexported?)
(export '(undeclare-ftype
	  type-check type-check-functions type-uncheck
	  *type-check-exceptions*))

;;;; DLAMBDA

(export '(dlambda fdlambda ddefun dlet dlet* dcase))

(export '(declare-constructor defreconstr all-constructors))

;;;; DLAMBDA-LIB

(export '(re-cons re-acons re-mapcar))

;;;; TDEFUN

(export '(tdefun tdefvar defstype))


;;;; CONSTR

(export '(defconstr defmulticonstr))


;;;; DEFSCONSTR

(export '(defsconstr))

;;;; BOX

(export '(\#>))
(export '(push2 pop2))

;;;; IGNORE

(export '(ergo-ignore-if-unused))
