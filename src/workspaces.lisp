;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; workspaces.lisp -- Support for workspace-sesstions.
;; Author          : Sam Owre
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

(in-package :pvs)

;;; Note: workspace-sessions all have an absolute pathname, and
;;; *all-workspace-sessions* is a list of instances that have been visited,
;;; either through change-workspace or with-workspace.

;;; workspace-session class is in classes-decl.lisp

;;; with-workspace is in macros.lisp - temporarily changes to the specified ws
;;; *workspace-session* is the current ws - don't bind this, use with-workspace.

(defun initialize-workspaces ()
  "Creates the initial *workspace-session*, and adds it to an empty
  *all-workspace-sessions*"
  (setq *all-workspace-sessions* nil)
  (let ((ws (get-workspace-session (working-directory))))
    (assert (pvs-context ws))
    (setq *workspace-session* ws)))

(defmethod get-workspace-ref ((wsref pathname))
  (get-workspace-ref (namestring wsref)))

(defmethod get-workspace-ref ((wsref string))
  )

(defun find-workspace (lib-path)
  "Given a path, finds the workspace, if it exists. Does not create it."
  (find lib-path *all-workspace-sessions* :key #'path :test #'file-equal))

(defmethod get-workspace-session (libref)
  "get-workspace-session gets the absolute pathname associated with libref,
and uses that as the key to find the ws in *all-workspace-sessions*,
creating a new one if needed.  Error if an existing directory could not be
found for libref."
  (let ((lib-path (get-library-path libref)))
    (if lib-path
	(or (unless *loading-prelude* ; find-workspace causes problems when building SBCL
	      (let ((ws (find-workspace lib-path)))
		(when ws
		  (assert (pvs-context ws))
		  ws)))
	    (let ((ws (make-instance 'workspace-session
			:path lib-path)))
	      (push ws *all-workspace-sessions*)
	      (restore-context ws)
	      (assert (pvs-context ws))
	      ws))
	(pvs-error "Library reference error"
	  (format nil "Path for ~a not found" libref)))))
