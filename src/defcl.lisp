;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; defcl.lisp -- This provides the a macro that expands into defclass plus
;;               the methods 
;; Author          : Sam Owre
;; Created On      : Tue May 31 01:36:04 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jul  1 14:02:27 1994
;; Update Count    : 14
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

;; To make this standalone
(eval-when (:execute :compile-toplevel :load-toplevel)
  (unless (find-package :pvs)
    (defpackage :pvs
      (:use :cl-user :common-lisp)
      (:export :copy :defcl :lcopy :memq :write-deferred-methods-to-file)
      )))

(in-package :pvs)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (unless (fboundp 'memq)
    (defun memq (elt list)
      (member elt list :test #'eq))))

(export '(defcl copy write-deferred-methods))

#+(or cmu sbcl)
(defmethod slot-exists-p-using-class (c o ss)
  (declare (ignore c o ss))
  nil)

#+gcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro ignore-errors (&body forms)
    `(progn ,@forms)))

;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   (unless (fboundp 'memq)
;;     (defun memq (elt list)
;;       (member elt list :test #'eq))))

(defvar *slot-info* nil
  "An association list mapping classes to superclasses, immediate
unignored slots, saved-slots, and unsaved-slots.")


(defmacro defcl (name classes &rest args)
  "Used to define classes in PVS, with name, and superclasses classes.
Automatically creates accessor forms for slots.
Also used to generate pvs-methods.lisp (after all classes have been loaded).
pvs-methods.lisp generates copy, store-object*, update-fetched, and restore-object*
methods for each class.
In addition to the usual, slots may have the following attributes:
  :parse - not currently used.  Was intended for untypecheck.
  :ignore - this slot will not be copied
  :store-as - a function to call to get the object to store for this slot.
  :fetch-as - the value to set the slot to in update-fetched
  :restore-as - should be nil to have restore-object* method ignore this slot.
"
  (setf args (mapcar #'(lambda (a) (if (consp a) a (list a))) args))
  `(progn ,@(mapcar #'(lambda (a)
			#+(or allegro sbcl)
				  `(declaim (ftype (function
						    (t)
						    ,(cadr (member :type a)))
						   ,(car a)))
			#-(or allegro sbcl)
				  `(proclaim '(function ,(car a) (t)
					       ,(cadr (member :type a)))))
		    (remove-if-not #'(lambda (a) (member :type a))
		      args))
    (defclass ,name ,classes
      ,(mapcar #'(lambda (a)
		   (setq a (remove-keyword
			    :parse
			    (remove-keyword
			     :ignore
			     (remove-keyword
			      :store-as
			      (remove-keyword
			       :restore-as
			       (remove-keyword
				:fetch-as a))))))
		   (append a (list :accessor (car a)
				   :initarg (intern (string (car a))
						    'keyword)
				   :initarg (car a))
			   (unless (memq :initform a)
			     (list :initform nil))))
	 args))
    (when (fboundp 'declare-make-instance)
      (declare-make-instance ,name))
    (declaim (inline ,(intern (format nil "~a?" name) :pvs)))
    (defun ,(intern (format nil "~a?" name) :pvs) (obj)
      (typep obj ',name))
    (eval-when (:execute :compile-toplevel :load-toplevel)
      (setq *slot-info*
	    (cons (cons ',name
			'(,classes ,args))
		   (delete (assoc ',name *slot-info*)
			   *slot-info*))))
;;     (defmethod untc*
;; 	,@(when classes (list :around))
;; 	((obj ,name))
;; 	,@(when classes (list '(call-next-method)))
;; 	,@(mapcar #'(lambda (a)
;; 		      (let ((slot (car a)))
;; 			(if (cadr (memq :parse a))
;; 			    `(untc* (slot-value obj ',slot))
;; 			    `(setf (slot-value obj ',slot)
;; 			      ,(cadr (memq :initform a))))))
;; 		  args))
    ',name))


;;; lcopy is a lazy copy that only makes a copy if there is a difference

(defun lcopy (obj &rest initargs)
  (if (loop for (key val) on initargs by #'cddr
	    always (let ((slot (intern (string key) :pvs)))
		     (eq (slot-value obj slot) val)))
      obj
      (apply #'copy obj initargs)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun remove-keyword (key list)
    (let ((tail (member key list)))
      (if tail
	  (append (ldiff list tail) (cddr tail))
	  list))))

(defun write-deferred-methods-to-file (&optional force?)
  (let ((mfile (format nil "~a/src/pvs-methods.lisp" (or *pvs-path* ".")))
	(cefile (format nil "~a/src/classes-expr.lisp" (or *pvs-path* ".")))
	(cdfile (format nil "~a/src/classes-decl.lisp" (or *pvs-path* ".")))
	(csfile (format nil "~a/src/prover/estructures.lisp" (or *pvs-path* "."))))
    (unless (and (not force?)
		 (probe-file mfile)
		 (file-older cefile mfile)
		 (file-older cdfile mfile)
		 (file-older csfile mfile))
      (with-open-file (out mfile :direction :output :if-exists :supersede)
	(let ((*print-case* :downcase))
	  (write '(in-package :pvs) :stream out)
	  (dolist (si *slot-info*)
	    (write-deferred-methods (car si) out)))))))

(defgeneric copy (obj &rest initargs))
(defgeneric store-object* (obj))
(defgeneric update-fetched (obj))
(defgeneric restore-object* (obj))

(defun write-deferred-methods (name out)
  (let* ((slots (get-all-slots-of (list name)))
	 (unignored-slots (mapcar #'car (unignored-slots% slots))) ; slot names only
	 (fetched-slots (fetched-slots% slots)) ; Those with fetch-as
	 (saved-slots (saved-slots% slots)) ; Those without :fetch-as
	 (stored-slots (stored-slots% slots)) ; Those with :store-as
	 (restored-slots (restored-slots% saved-slots)) ; Without :restore-as,
					; print-type moved to end
	 (copy-slots (mapcar #'(lambda (x) (if (eq x 'var) '(cvar var) x))
		       unignored-slots)) ; var is special in SBCL
	 (stobj-slots (let ((slts (mapcar #'car
				    (remove-duplicates
					(append saved-slots stored-slots)))))
			(mapcar #'(lambda (x) (if (eq x 'var) '(cvar var) x)) slts)))
	 (updf-slots (append stobj-slots
			     (mapcan #'(lambda (slt)
					 (unless (memq (car slt) stobj-slots)
					   (if (eq (car slt) 'var)
					       '((cvar var)) (list (car slt)))))
			       fetched-slots)))
	 (restr-slots (mapcar #'(lambda (sl) (if (eq (car sl) 'var) '(cvar var) (car sl)))
			restored-slots))
	 (copy-mthd (copy-method name slots copy-slots))
	 (strobj-mthd (store-object-method name stobj-slots stored-slots))
	 (updf-mthd (update-fetched-method name updf-slots fetched-slots))
	 (restr-mthd (restore-object-method name restr-slots restored-slots))
	 ;;(strobj-pushes (mapcar #'cadadr (cdddr (cadddr (fourth strobj-mthd)))))
	 ;;(updf-setfs (mapcar #'cadr (cdddr (fourth updf-mthd))))
	 )
    ;; store-object and update-fetched must correspond:
    (when unignored-slots
      (format out "~2%")
      (write copy-mthd :stream out :level nil :length nil :pretty t)
      (format out "~2%")
      (write strobj-mthd :stream out :level nil :length nil :pretty t)
      (format out "~2%")
      (write updf-mthd :stream out :level nil :length nil :pretty t)
      (format out "~2%")
      (write restr-mthd :stream out :level nil :length nil :pretty t))))

(defun copy-method (name slots copy-slots)
  `(defmethod copy ((obj ,name) &rest initargs)
     (with-slots ,copy-slots obj
       (make-instance ',name
	 ,@(mapcan #'(lambda (sl)
		       (let ((ksl (intern (string (car sl)) :keyword)))
			 `(,ksl
			   (let* ((get1 (getf initargs ',(car sl)
					      '%nogetf))
				  (getfv (if (eq get1 '%nogetf)
					     (getf initargs ,ksl
						   '%nogetf)
					     get1)))
			     (if (eq getfv '%nogetf)
				 ,(if (ignored-slot% sl)
				      (getf (cdr sl) :initform)
				      (if (eq (car sl) 'var) 'cvar (car sl)))
				 getfv)))))
	     slots)))))

(defun store-object-method (name stobj-slots stored-slots)
  ;; Note that this must match fetch-object
  `(defmethod store-object* ((obj ,name))
     (with-slots ,stobj-slots obj
       (reserve-space ,(1+ (length stobj-slots))
	 (push-word (store-obj ',name))
	 ,@(mapcar #'(lambda (a)
		       (let* ((sslot (assoc a stored-slots))
			      (store-as (when sslot (memq :store-as (cdr sslot)))))
			 (if store-as
			     `(push-word (store-obj ,(when (cadr store-as)
						       (list (cadr store-as) 'obj))))
			     `(push-word (store-obj ,(if (equal a '(cvar var)) 'cvar a)))))
		       ;;`(push-word (store-obj ,(if (eq (car a) 'var) 'cvar (car a))))
		       )
	     stobj-slots)))))

(defun update-fetched-method (name updf-slots fetched-slots)
  ""
  (let ((i 0))
    `(defmethod update-fetched ((obj ,name))
       (with-slots ,updf-slots obj
	 ,@(mapcar #'(lambda (a)
		       (let* ((fslot (assoc a fetched-slots))
			      (fetch-as (when fslot (getf (cdr fslot) :fetch-as)))
			      (slot (if (equal a '(cvar var)) 'cvar a)))
			 (if fslot
			     `(setf ,slot ,fetch-as)
			     `(setf ,slot (fetch-obj (stored-word ,(incf i)))))))
	     updf-slots)))))

(defun restore-object-method (name restr-slots restored-slots)
  `(defmethod restore-object* ((obj ,name))
     (with-slots ,restr-slots obj
       (let ((*restore-object-parent* obj))
	 ,@(mapcar #'(lambda (a)
		       `(when ,(if (eq (car a) 'var) 'cvar (car a))
			  (let ((*restore-object-parent-slot*
				 ',(car a)))
			    (restore-object* ,(if (eq (car a) 'var) 'cvar (car a))))))
	     restored-slots)
	 obj))))

(defun get-all-slots-of (classes &optional slots)
  (if (null classes)
      (remove-duplicates slots :key #'car)
      (let ((slot-info (assoc (car classes) *slot-info*)))
	(get-all-slots-of (append (cadr slot-info) (cdr classes))
			  (append (caddr slot-info) slots)))))


(defun unignored-slots% (args)
  (remove-if #'ignored-slot% args))

(defun ignored-slot% (arg)
  (cadr (memq :ignore arg)))

(defun saved-slots% (args)
  (remove-if #'(lambda (a) (memq :fetch-as a)) args))

(defun stored-slots% (args)
  (remove-if (complement #'(lambda (a) (memq :store-as a))) args))

(defun fetched-slots% (args)
  (remove-if (complement #'(lambda (a) (memq :fetch-as a))) args))

(defun restored-slots% (args)
  (let ((rslots (remove-if #'(lambda (a) (memq :restore-as a)) args)))
    (if (assoc 'print-type rslots)
	(append (remove (assoc 'print-type rslots) rslots)
		(list (assoc 'print-type rslots)))
	rslots)))


;;; Grabbed off the net, from jmorrill@bbn.com (Jeff Morrill)
;;; Not used, but may come in handy.

;(defmethod shallow-copy ((object standard-object))
;  (let ((copy (make-instance (class-of object))))
;    (dolist (slotd (class-slots (class-of object)))
;       (let ((name (slot-definition-name slotd)))
;         (setf (slot-value copy name) (slot-value object name))))
;    copy))

;(defmethod eequal (obj1 obj2)
;  (equal obj1 obj2))

(defun file-older (file1 file2)
  (let ((time1 (file-write-date file1))
	(time2 (file-write-date file2)))
    (or (null time1)
	(null time2)
	(<= time1 time2))))

;; Same as describe, but returns the object, rather than nil
(defun show (obj)
  (format t "class: ~(~a~)" (class-name (class-of obj)))
  (describe obj)
  obj)
