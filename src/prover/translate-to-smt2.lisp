;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smt2.lisp -- 
;; Author          : Karthik Nukala and N. Shankar
;; Created On      : June 2023
;; Last Modified By: N. Shankar
;; Last Modified On: 
;; Update Count    : 
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

;;NSH(12/5/10): Adapting translate-to-prove-yices to yices2 output. 

(defvar *smt2bindings* nil)
(defvar *smt2embeddedf* nil)
(defvar *smt2defns* nil)
;;(defvar *smt2datatype-warning* nil)
(defvar *smt2name-hash* (make-pvs-hash-table))
(defvar *translate-to-yices2-hash* (make-pvs-hash-table))
(defvar *smt2-executable* nil)
(defvar *smt2-flags* "--mode=one-shot")
(defvar *smt2-id-counter*)  ;;needs to be initialized in eproofcheck
(defvar *smt2-conditions* nil)
(defvar *smt2-subtype-constraints* nil)

(newcounter *smt2-id-counter*)

(defun clear-smt2 ()
  (setq *smt2defns* nil)
  (clrhash *smt2name-hash*)
  (clrhash *translate-to-smt2-hash*)
  (newcounter *smt2-id-counter*))

(defun smt2-name (expr &optional id) ;;expr must have id field,
                                      ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *y2name-hash*)))
	(or entry
	    (let ((name (smt2-id-name (or id (id expr)))))
	      (setf (gethash expr *y2name-hash*) name)
	      name)))))

(defun smt2-id-name (id)
  (intern
   (concatenate 'string
     (string (if (integerp id)
		 (format nil "~r"
		   id)
		 id))
     "_"
     (princ-to-string
      (funcall
       *smt2-id-counter*))) :pvs))

;;Next, we translate PVS types to Yices types so that any enumerated
;;types or datatypes introduce names.

(defun smt2-type-name (expr)
  (smt2-name expr))

(defmethod translate-to-smt2* ((ty type-name) bindings)
  (declare (ignore bindings))
  (let ((smt2name-hash (gethash ty *smt2name-hash*)))
    (or smt2name-hash 
	(cond ((enum-adt? ty)
	       ;; (let ((constructors (constructors ty))
	       ;; 	     (yname (yices2-name ty)))	;;bindings can be ignored
		 (error "yices2 tranlation does not support scalars")
		 ;;(translate-to-yices2-scalar yname constructors))
	       )
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "Bool"))
	      ((tc-eq ty *number*) "Real")
		;;else uninterpreted type
	      (t (let ((smt2name (smt2-name ty)))
		  (push (format nil "(declare-sort ~a)" smt2name) *smt2defns*)
		  smt2name))))))

(defmethod translate-to-smt2* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ;;((tc-eq ty *naturalnumber*) 'nat) ;;there is no nat
	  ((tc-eq ty *integer*) "Int")
	  ((tc-eq ty *real*) "Real")
	  (t (translate-to-smt2* supertype bindings)))))



;; (defmethod translate-to-smt2* ((ty funtype) bindings)
;;   (with-slots (domain range) ty
;; 	      (let ((sdom (if (dep-binding? domain)  ;;NSH(4-16-11)
;; 			      (find-supertype (type domain))
;; 			    (find-supertype domain))))
;; 	      (format nil "(Array ~{ ~a~} ~a)"
;; 		      (translate-to-yices2-list (types sdom) nil
;; 						(translate-to-yices2* range bindings)
;; 						bindings)))))
