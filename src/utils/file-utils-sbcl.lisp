;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2008, SRI International.  All Rights Reserved.

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
;; Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA
;; 02110-1301, USA.
;; --------------------------------------------------------------------

(in-package :pvs)
(export '(file-exists-p directory-p read-permission? write-permission?
	  file-write-time get-file-info))

(defun file-exists-p (file)
   (uiop:file-exists-p (sb-ext:parse-native-namestring file)))

(defun directory-p (dir)
  (uiop:directory-exists-p dir))

(defun read-permission? (file)
  (handler-case (zerop (sb-posix:access file sb-posix:r-ok))
    (sb-posix:syscall-error () nil)))

(defun write-permission? (&optional (file *default-pathname-defaults*))
  (let ((pfile (uiop:file-exists-p file)))
    (and pfile
	 (handler-case (zerop (sb-posix:access pfile sb-posix:w-ok))
	   (sb-posix:syscall-error () nil)))))

(defconstant u1970 (encode-universal-time 0 0 0 1 1 1970 0))

(defun file-write-time (file)
  (let ((pfile (uiop:file-exists-p file)))
    (and pfile
	 (handler-case (+ u1970 (sb-posix:stat-mtime (sb-posix:stat pfile)))
	   (sb-posix:syscall-error () nil)))))

(defun get-file-info (file)
  (let ((pfile (uiop:file-exists-p file)))
    (and pfile
	 (handler-case
	     (let ((stat (sb-posix:stat pfile)))
	       (list (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))
	   (sb-posix:syscall-error () nil)))))
