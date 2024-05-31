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

(eval-when (:execute :load-toplevel)
  ;; This sets *pvs-path* and *pvs-fasl-type*
  (load "pvs-config.lisp"))

(in-package :pvs)

(load "src/defcl.lisp")
(load "src/store-object.lisp")
(load "src/classes-expr.lisp")
(load "src/classes-decl.lisp")
(load "src/prover/estructures.lisp")

(write-deferred-methods-to-file t)

(uiop:quit 0)
