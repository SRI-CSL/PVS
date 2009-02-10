;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-methods.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec 31 19:16:33 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Dec 31 21:13:07 1998
;; Update Count    : 11
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :cl-user)

(eval-when (eval load)
  ;; This sets *pvs-path* and sets *pvs-binary-type*
  (load "pvs-config.lisp"))

(defpackage pvs (:use #+lucid :lucid-common-lisp #-sbcl :lisp #+sbcl :cl
		      #-(or gcl cmu sbcl) :clos #+(or gcl cmu) :pcl
		      #+sbcl :sb-pcl)
	 #+sbcl (:shadowing-import-from :sb-int memq)
	 #+sbcl (:export memq))

(in-package :pvs)
(import '(cl-user:*pvs-path*))
(let (#+allegro (excl:*enable-package-locked-errors* nil))
  (load "src/defcl.lisp")
  (load "src/store-object.lisp")
  (load "src/classes-expr.lisp")
  (load "src/classes-decl.lisp")
  (load "src/prover/estructures.lisp"))

(write-deferred-methods-to-file t)

(cl-user:bye)
