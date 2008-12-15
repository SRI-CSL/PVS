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

; Symbol table, implemented as associations (i . expr), where i is an integer index, and expr
; a PVS expression

(defvar *index* 0)

(defun symtab-boundvars (symtab)
  (car symtab))

(defun symtab-freevars (symtab)
  (cdr symtab))

(defun symtab-make (bound free)
  (cons bound free))

(defun symtab-init ()
  (setf *index* 0))

(defun symtab-empty ()
  (cons nil nil))

(defun symtab-fresh-index ()
  (setf *index* (1+ *index*))
  *index*)

(defun symtab-add-free (expr symtab)
  (assert (consp symtab))
  (setf *index* (1+ *index*))
  (let ((new-symtab (symtab-make (symtab-boundvars symtab)
				 (cons (cons *index* expr) (symtab-freevars symtab)))))
    (values *index* new-symtab)))

(defun symtab-add-bound (expr symtab)
  (assert (consp symtab))
  (setf *index* (1+ *index*))
  (let ((new-symtab (symtab-make (cons (cons *index* expr) (symtab-boundvars symtab))
			         (symtab-freevars symtab))))
    (values *index* new-symtab)))

(defun symtab-add-bounds (exprs symtab &optional indices)
  (assert (consp symtab))
  (cond ((null exprs)
	 (values indices symtab))
	(t
	 (multiple-value-bind (new-index new-symtab)
	     (symtab-add-bound (car exprs) symtab)
	   (symtab-add-bounds (cdr exprs) new-symtab (cons new-index indices))))))

(defun symtab-index (expr symtab)
  (assert (consp symtab))
  (car (or (rassoc expr (symtab-boundvars symtab) :test #'tc-eq)
	   (rassoc expr (symtab-freevars symtab) :test #'tc-eq))))

(defun symtab-value (idx symtab)
  (assert (consp symtab))
  (cdr (or (assoc idx (symtab-boundvars symtab) :test #'=)
	   (assoc idx (symtab-freevars symtab) :test #'=))))

(defun symtab-strip (symtab)
  (assert (eq (symtab-boundvars symtab) nil))
  (let* ((free (symtab-freevars symtab))
	 (size (length free))
	 (offsets (make-array size :element-type 'fixnum))
	 (fvars   (make-array size :element-type 'string :initial-element ""))
	 (types   (make-string size))
	 (i       0))
    (mapc  #'(lambda (bndng)
	       (let ((idx (car bndng))
		     (expr (cdr bndng)))
		 (setf (elt offsets i) idx)
		 (setf (elt fvars i) (format nil "~a" expr))
		 (setf (elt types i)
		       (let ((level (level (type expr))))
			 (cond ((eql level 0) #\0)
			       ((eql level 1) #\1)
			       ((eql level 2) #\2)
			       (t (break)))))
		 (setf i (1+ i))))
      free)
    (values free
	    size
	    offsets
	    fvars
	    types)))

(defun symtab-length (symtab)
  (length (symtab-freevars symtab)))


; An expression is shielding if it contains bound variables

(defun symtab-shielding? (expr symtab)
  (some #'(lambda (bndng)
	    (occurs-in (cdr bndng) expr))
	(symtab-boundvars symtab)))
