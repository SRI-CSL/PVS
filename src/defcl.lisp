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

(in-package :pvs)

(export '(defcl copy write-deferred-methods))

#+(or cmu sbcl)
(defmethod slot-exists-p-using-class (c o s)
  (declare (ignore c o s))
  nil)

#+gcl
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defmacro ignore-errors (&body forms)
    `(progn ,@forms)))


(defvar *slot-info* nil
  "An association list mapping classes to superclasses, immediate
unignored slots, saved-slots, and unsaved-slots.")


(defmacro defcl (name classes &rest args)
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
    (declare-make-instance ,name)
    (proclaim '(inline ,(intern (format nil "~a?" name))))
    (defun ,(intern (format nil "~a?" name)) (obj)
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


; (defmacro defcl* (name classes &rest args)
;   (let ((cl (macroexpand `(defcl ,name ,classes ,@args))))
;     (eval (second cl))
;     (eval (sixth cl))   ;; updates *slot-info*
;     (append cl
; 	    (generate-defcl-methods (list name))
; 	    (generate-update-fetched-methods (list name)))))

; (defvar *classes-done* nil)
; (defvar *methods-collected* nil)

; (defun generate-defcl-methods (names)
;   (let ((*classes-done* nil)
; 	(*methods-collected* nil))
;     (generate-defcl-methods* names)
;     *methods-collected*))

; (defun generate-defcl-methods* (names)
;   (when names
;     (let* ((name (car names))
; 	   (class (find-class name)))
;       (unless (memq name *classes-done*)
; 	(push name *classes-done*)
; 	(setq *methods-collected*
; 	      (nconc *methods-collected*
; 		     (list (generate-copy-method name)
; 			   (generate-store-object*-method name)
; 			   ;;(generate-update-fetched-method name)
; 			   )))
; 	(generate-defcl-methods* (mapcar #'class-name
; 				   (class-direct-subclasses class)))))
;     (generate-defcl-methods* (cdr names))))

; (defun generate-update-fetched-methods (names)
;   (let ((*classes-done* nil)
; 	(*methods-collected* nil))
;     (generate-update-fetched-methods* names)
;     (nreverse *methods-collected*)))

; (defun generate-update-fetched-methods* (names)
;   (when names
;     (let* ((name (car names))
; 	   (class (find-class name)))
;       (unless (memq name *classes-done*)
; 	(push name *classes-done*)
; 	(push (generate-update-fetched-method name) *methods-collected*)
; 	(generate-update-fetched-methods* (mapcar #'class-name
; 					    (class-direct-subclasses class)))))
;     (generate-update-fetched-methods* (cdr names))))


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
  (let ((mfile (format nil "~a/src/pvs-methods.lisp" *pvs-path*))
	(cefile (format nil "~a/src/classes-expr.lisp" *pvs-path*))
	(cdfile (format nil "~a/src/classes-decl.lisp" *pvs-path*))
	(csfile (format nil "~a/src/prover/estructures.lisp" *pvs-path*)))
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

(defun write-deferred-methods (name out)
  (let* ((slots (get-all-slots-of (list name)))
	 (unignored-slots (mapcar #'car (unignored-slots% slots)))
	 (saved-slots (saved-slots% slots))
	 (stored-slots (stored-slots% slots))
	 (fetched-slots (fetched-slots% slots))
	 (restored-slots (restored-slots% saved-slots)))
    (format out "~2%")
    (write `(defmethod copy ((obj ,name) &rest initargs)
	      (with-slots ,unignored-slots obj
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
					       (car sl))
					  getfv)))))
			    slots))))
	   :stream out :level nil :length nil :pretty t)
    (format out "~2%")
    (write `(defmethod store-object* ((obj ,name))
	      (reserve-space ,(+ (length saved-slots) (length stored-slots) 1)
		(with-slots ,(mapcar #'car (append saved-slots stored-slots))
		    obj
		  (push-word (store-obj ',name))
		  ,@(mapcar #'(lambda (a)
				`(push-word (store-obj ,(car a))))
		      saved-slots)
		  ,@(mapcar #'(lambda (a)
				`(push-word
				  (store-obj
				   (,(getf (cdr a) :store-as) obj))))
		      stored-slots))))
	   :stream out :level nil :length nil :pretty t)
    (format out "~2%")
    (write `(defmethod update-fetched ((obj ,name))
	      (with-slots (,@(mapcar #'car saved-slots)
			     ,@(mapcar #'car stored-slots)
			     ,@(mapcar #'car fetched-slots)) obj
		,@(let ((arg-setters nil))
		    (dotimes (i (length saved-slots))
		      (let ((a (nth i saved-slots)))
			(push `(setf ,(car a)
				     (fetch-obj (stored-word ,(1+ i))))
			      arg-setters)))
		    (dotimes (i (length stored-slots))
		      (let ((a (nth i stored-slots)))
			(push `(setf ,(car a)
				     (fetch-obj (stored-word
						 ,(+ (length saved-slots)
						     i 1))))
			      arg-setters)))
		    (dolist (a fetched-slots)
		      (push `(setf ,(car a)
				   ,(getf (cdr a) :fetch-as))
			    arg-setters))
		    (nreverse arg-setters))))
	   :stream out :level nil :length nil :pretty t)
    (format out "~2%")
    (write `(defmethod restore-object* ((obj ,name))
	      (let ((*restore-object-parent* obj))
		(with-slots ,(mapcar #'car restored-slots) obj
		  ,@(mapcar #'(lambda (a)
				`(when ,(car a)
				   (let ((*restore-object-parent-slot*
					  ',(car a)))
				     (restore-object* ,(car a)))))
		      restored-slots)
		  obj)))
	   :stream out :level nil :length nil :pretty t)
;;     (format out "~2%")
;;     (write `(defmethod count-instances* ((obj ,name))
;; 	      (with-slots (,@(mapcar #'car slots)) obj
;; 		(let ((entry (assq ',name *instance-count*)))
;; 		  (if entry
;; 		      (incf (cdr entry))
;; 		      (push (cons ',name 1) *instance-count*)))
;; 		(dolist (ss ',(mapcar #'car slots))
;; 		  (count-instances* (slot-value obj ss)))))
;; 	   :stream out :level nil :length nil :pretty t)
;;     (format out "~2%")
;;     (write `(defmethod collect-common-objects* ((obj ,name))
;; 	      (with-slots (,@(mapcar #'car slots)) obj
;; 		(dolist (ss ',(mapcar #'car slots))
;; 		  (collect-common-objects* (slot-value obj ss)))))
;; 	   :stream out :level nil :length nil :pretty t)
    ))

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

(eval-when (:execute :compile-toplevel :load-toplevel)
  (unless (fboundp 'memq)
    (defun memq (elt list)
      (member elt list :test #'eq))))

(defun file-older (file1 file2)
  (let ((time1 (file-write-date file1))
	(time2 (file-write-date file2)))
    (or (null time1)
	(null time2)
	(<= time1 time2))))
