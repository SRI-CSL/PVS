
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

(in-package :file-utils)

(define-foreign-library fileutils
    (t (:default "fileutils")))

(defcfun ("file_exists_p" fileutils___file_exists_p) :int
  (filename :string))

(defcfun ("directory_p" fileutils___directory_p) :int
  (filename :string))

(defcfun ("read_permission_p" fileutils___read_permission_p) :int
  (filename :string))

(defcfun ("write_permission_p" fileutils___write_permission_p) :int
  (filename :string))

(defcfun ("file_write_time" fileutils___file_write_time) :long
  (filename :string))

(defcfun ("getfileinfo" fileutils___getfileinfo) :int
  (filename :string)
  (i :pointer))

(defmacro expanded-tilde-namestring (filename)
  `(ignore-errors (uiop:native-namestring (pathname ,filename))))

(defun file-exists-p (filename)
  (let ((exp-file (expanded-tilde-namestring filename)))
    (and exp-file
	 (zerop (fileutils___file_exists_p exp-file)))))

(defun directory-p (filename)
  (when filename
    (let ((filestring (expanded-tilde-namestring filename)))
      (unless (zerop (fileutils___directory_p filestring))
	(pathname (if (char= (char filestring (1- (length filestring))) #\/)
		      filestring
		      (concatenate 'string filestring "/")))))))

(defun read-permission? (filename)
  (zerop (fileutils___read_permission_p
	  (expanded-tilde-namestring filename))))

(defun write-permission? (&optional (filename *default-pathname-defaults*))
  (zerop (fileutils___write_permission_p
	  (expanded-tilde-namestring filename))))

(defconstant u1970 (encode-universal-time 0 0 0 1 1 1970 0))

(defun file-write-time (filename)
  (let ((date (fileutils___file_write_time
	       (expanded-tilde-namestring filename))))
    (unless (zerop date)
      (+ date u1970))))

(defun get-file-info (filename)
  (let ((pfile (expanded-tilde-namestring filename)))
    (and pfile
	 (zerop (fileutils___file_exists_p pfile))
	 #+allegro
	 (let ((stat (excl.osi:stat pfile)))
	   (list (excl.osi:stat-dev stat) (excl.osi:stat-ino stat)))
	 #+sbcl
	 (handler-case 
	     (let ((stat (sb-posix:stat pfile)))
	       (list (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))
	   (error (err)
	     (get-file-info pfile))))))
