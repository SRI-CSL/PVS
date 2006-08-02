;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-parser.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Dec 29 02:55:05 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 29 03:28:07 1998
;; Update Count    : 1
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

(load (format nil "~a/ess/dist-ess.lisp" *pvs-path*))
(generate-ess ergolisp sb)

#+allegro
(compile-file-if-needed (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
#-allegro
(compile-file (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
(load (format nil "~a/src/ergo-gen-fixes" *pvs-path*))
(let ((sbmake (intern (string :sb-make) :sb)))
  (funcall sbmake
	   :language "pvs"
	   :working-dir (format nil "~a/src/" (or *pvs-path* "."))
	   :unparser? nil))
(bye)
