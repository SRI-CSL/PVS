;;; -*- Mode: Lisp; Package: DEFSCONSTR -*-
;;;
;;; Compile a constructor spec into a lisp file.
;;;
;;; Author: Conal Elliott.  Last Modified Mon Sep 25 09:52:13 1989
;;;
;;; Sccs Id @(#)defsconstr.lisp	1.6 9/25/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


(defpackage "DEFSCONSTR")
(in-package :DEFSCONSTR) (use-package :ergolisp)

(eexport '(defsconstr))

(defmacro defsconstr (string)
  "Declare a collection of constr Items in concrete syntax."
  (constrg:constr-parse :nt 'constr-term-rep::Items :string string))
