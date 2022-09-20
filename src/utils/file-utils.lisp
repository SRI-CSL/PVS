
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

#-sbcl
(defcfun ("getfileinfo" fileutils___getfileinfo) :int
  (filename :string)
  (i :pointer int))

(defmacro expanded-tilde-namestring (filename)
  `(ignore-errors (uiop:native-namestring ,filename)))

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

;;; #(dev inode mtime isdir mode)
#-sbcl
(let ((fstat-array
       (make-array 2 :initial-element 0 :element-type '(unsigned-byte 32))))
  (defun get-file-info (filename)
    (when (zerop (fileutils___getfileinfo
		  (expanded-tilde-namestring filename) fstat-array))
      (list (aref fstat-array 0)
	    (aref fstat-array 1)))))

#+sbcl
(defun get-file-info (file)
  (let ((pfile (uiop:probe-file* file :truename t)))
    (and pfile
	 (handler-case
	     (let ((stat (sb-posix:stat pfile)))
	       (list (sb-posix:stat-dev stat) (sb-posix:stat-ino stat)))
	   (sb-posix:syscall-error () nil)))))
