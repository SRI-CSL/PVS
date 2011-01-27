;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-allegro-pvs.lisp -- Invoked by build-pvs within make-pvs.lisp
;;                          used for Allegro build only
;; Author          : Sam Owre
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
(defvar *pvs-path* (or (sys:getenv "PVSPATH") "."))
(eval-when (:load-toplevel :execute)
  (require 'tpl-debug)
  #+(or runtime-standard runtime-dynamic)
  (pushnew :runtime *features*)
  #-(or runtime-standard runtime-dynamic)
  (require 'prof)
  (require 'ffcompat)
  (setq excl:*global-gc-behavior* :auto)
  (load (format nil "~a/pvs.system" *pvs-path*))
  (require 'uri)
  #+(version>= 6)
  (require 'pxml)
  (require 'sock-s))
(mk:operate-on-system :pvs :compile)
(when (sys:getenv "PVSMAKELOADAFTER")
  (load (sys:getenv "PVSMAKELOADAFTER")))
