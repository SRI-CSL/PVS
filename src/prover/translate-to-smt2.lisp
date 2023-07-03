;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smt2.lisp -- 
;; Author          : K. Nukala and N. Shankar
;; Created On      : June 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 07/03/2023
;; Update Count    : 2
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
(defvar *translate-to-smt2-hash* (make-pvs-hash-table))
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

(defun smt2-type-name (expr)
  (smt2-name expr))

(defmethod translate-to-smt* :around ((obj type-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      result)))))

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

(defmethod translate-to-smt* ((ty tupletype) bindings)
  (with-slots (types) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* types bindings))))

(defmethod translate-to-smt2* ((ty recordtype) bindings)
  (with-slots (fields) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* fields bindings))))

(defmethod translate-to-smt2* ((ty field-decl) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty dep-binding) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty funtype) bindings)
  (with-slots (domain range) ty
    (format nil "(Array ~a ~a)"
	    (translate-to-smt2* domain bindings)
	    (translate-to-smt2* range bindings))))


(defmethod translate-to-smt2* ((list list) bindings)
  (cond ((consp list)
	 (cons (translate-to-smt2* (car list) bindings)
	       (translate-to-smt2* (cdr list) bindings)))
	(t nil)))


(defmethod translate-to-smt2* :around ((obj expr) bindings)
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let* ((result (call-next-method))
		   (type-constraints (type-constraints obj :none))
		   (rtype-constraints (loop for fmla in type-constraints
					 nconc (and+ fmla))))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      (loop for tc in rtype-constraints
		 do (let* ((ytc (translate-to-smt2* tc bindings))
			   (yclause (if *smt2-conditions*
					(format nil "(assert (implies (and ~{ ~a~}) ~a))"
					  *smt2-conditions*
					  ytc)
					(format nil "(assert ~a)" ytc))))
		      (push yclause *smt2-subtype-constraints*)))
	      result)))))

(defun smt2-recognizer (name bindings)
  (when (recognizer? name)
    (translate-to-smt2* (type name) bindings)
    (format nil "~a?" (translate-to-smt2* (constructor name) bindings) )))

(defmethod translate-to-smt2* ((expr name-expr) bindings)
  (let ((bpos (assoc expr bindings
		     :test #'same-declaration)))
    (if bpos (if (consp (cdr bpos))
		 (format nil "(Prod ~{ ~a~})"  (cdr bpos))
		 (cdr bpos))
	(let* ((smt2name-hashentry (gethash expr *smt2name-hash*)))
	  (or smt2name-hashentry
	      (smt2-interpretation expr)
	      ;(eta-expanded-smt2-interpretation expr)
	      (smt2-recognizer expr bindings)
	      (let* ((smt2type (translate-to-smt2* (type expr)
						bindings))
		     (smt2name-hashentry (gethash expr *smt2name-hash*)))
		(or smt2name-hashentry
		    (let* ((smt2name (smt2-name expr))
			   (defn (format nil "(declare-const ~a ~a)"
					 smt2name
					 smt2type)))
		      (push defn
			    *smt2defns*)
		      (format-if "~%Adding definition: ~a" defn)
		      smt2name))))))))


(defmethod translate-to-smt2* ((expr constructor-name-expr) bindings)
  (call-next-method (lift-adt expr) bindings))

(defmethod translate-to-smt2* ((expr rational-expr) bindings)
  (declare (ignore bindings))
  (number expr))

(defmethod translate-to-smt2* ((ex string-expr) bindings)
  (declare (ignore bindings))
  (string->int (string-value ex)))

(defmethod translate-to-smt2* ((expr record-expr) bindings)
  (with-slots (assignments) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (sort-assignments (assignments expr)) bindings))))

(defmethod translate-to-smt2* ((expr tuple-expr) bindings)
  (with-slots (exprs) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (exprs expr) bindings))))

(defmethod translate-to-smt2* ((expr branch) bindings)
  (let ((smt2condition (translate-to-smt2* (condition expr) bindings)))
  (format nil "(ite ~a ~a ~a)"
    smt2condition
    (let ((*smt2-conditions* (push smt2condition *smt2-conditions*)))
      (translate-to-smt2* (then-part expr) bindings))
    (let ((*smt2-conditions* (push `(not ,smt2condition) *smt2-conditions*)))
      (translate-to-smt2* (else-part expr) bindings)))))

(defmethod translate-to-smt2* ((expr projection-expr) bindings)
  (break "Can't translate standalone projections"))

(defmethod translate-to-smt2* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (if (variable? argument)
	(let ((bnd (assoc argument bindings
			  :test #'same-declaration)))
	  (if (and bnd (consp (cdr bnd)))
	      (nth (1- index) (cdr bnd))
	      (format nil "((_ project ~a) ~a)"
		(1+ (index expr))
		(translate-to-yices2* argument bindings))))
	(format nil "((_ project ~a) ~a)"
		(1+ (index expr))
		(translate-to-yices2* argument bindings)))))
