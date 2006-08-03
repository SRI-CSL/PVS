;;; Functions which replace slower functions in Lucid, AKCL, etc.

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

(defun assoc@ (obj list &optional (test #'eql))
  (labels ((assoc* (obj list test)
	     (when list
	       (if (funcall test obj (caar list))
		   (car list)
		   (assoc* obj (cdr list) test)))))
    (assoc* obj list test)))

(defun some@ (fn list)
  (unless (null list)
    (or (funcall fn (car list))
	(some@ fn (cdr list)))))

(defun every@ (fn list)
  (or (null list)
      (and (funcall fn (car list))
	   (every@ fn (cdr list)))))

(defun everym@ (fn list &rest more-lists)
  (labels ((everym* (fn list more-lists)
	     (or (null list)
		 (and (apply fn (car list) (mapcar #'car more-lists))
		      (everym* fn (cdr list) (mapcar #'cdr more-lists))))))
    (everym* fn list more-lists)))

(defun find@ (obj list &optional (test #'eql))
  (labels ((find* (obj list test)
	     (when list
	       (if (funcall test obj (car list))
		   (car list)
		   (find* obj (cdr list) test)))))
    (find* obj list test)))

(defun find-if@ (fn list)
  (when list
    (if (funcall fn (car list))
	(car list)
	(find-if@ fn (cdr list)))))

(defun find-if-not@ (fn list)
  (when list
    (if (funcall fn (car list))
	(find-if@ fn (cdr list))
	(car list))))

(defun position@ (obj list &optional (test #'eql) (pos 0))
  (declare (fixnum pos))
  (labels ((position* (obj list test pos)
	     (declare (fixnum pos))
	     (when list
	       (if (funcall test obj (car list))
		   pos
		   (position* obj (cdr list) test (1+ pos))))))
    (position* obj list test pos)))

(defun position-if@ (fn list &optional (pos 0))
  (declare (fixnum pos))
  (labels ((position-if* (fn list pos)
	     (declare (fixnum pos))
	     (when list
	       (if (funcall fn (car list))
		   pos
		   (position-if* fn (cdr list) (1+ pos))))))
    (position-if* fn list pos)))

(defun position-if-not@ (fn list &optional (pos 0))
  (declare (fixnum pos))
  (labels ((position-if-not* (fn list pos)
	     (declare (fixnum pos))
	     (when list
	       (if (funcall fn (car list))
		   (position-if-not* fn (cdr list) (1+ pos))
		   pos))))
    (position-if-not* fn list pos)))

(defun remove@ (obj list &optional (test #'eql))
  (labels ((remove* (obj list test cont)
	     (if (null list)
		 (nreverse cont)
		 (if (funcall test obj (car list))
		     (remove* obj (cdr list) test cont)
		     (remove* obj (cdr list) test (cons (car list) cont))))))
    (remove* obj list test nil)))

(defun remove-if@ (fn list)
  (labels ((remove-if* (fn list cont)
	     (if (null list)
		 (nreverse cont)
		 (if (funcall fn (car list))
		     (remove-if* fn (cdr list) cont)
		     (remove-if* fn (cdr list) (cons (car list) cont))))))
    (remove-if* fn list nil)))

(defun remove-if-not@ (fn list)
  (labels ((remove-if-not* (fn list cont)
	     (if (null list)
		 (nreverse cont)
	         (if (funcall fn (car list))
		     (remove-if-not* fn (cdr list) (cons (car list) cont))
		     (remove-if-not* fn (cdr list) cont)))))
	  (remove-if-not* fn list nil)))
