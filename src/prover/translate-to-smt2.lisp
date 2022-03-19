;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-yices2.lisp -- 
;; Author          : Nabil Kherraf and Natarajan Shankar
;; Created On      : Mar 18, 2022
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

(defvar *smt2defns* nil)
(defvar *smt2name-hash* (make-pvs-hash-table))
(defvar *translate-to-smt2-hash* (make-pvs-hash-table))
(defvar *smt2-id-counter*)  ;;needs to be initialized in eproofcheck
(defvar *smt2-conditions* nil)
(defvar *smt2-subtype-constraints* nil)
(defvar *smt2-tuple-length-bound* 36)
(newcounter *smt2-id-counter*)

;;PVS is missing bvudiv, bvurem, bvshl bvlshr
;;SMT2 is missing bv-sub, bv-xor
(defparameter *smt2-interpreted-names*
  '((=  (|equalities| . =))
    (/=  (|notequal| . distinct))
    (TRUE (|booleans| . true))
    (FALSE (|booleans| . false))
    (IMPLIES  (|booleans| . =>))
    (=>  (|booleans| . =>))
    (⇒ (|booleans| . =>))
    (IFF (|booleans| . =))
    (XOR (|xor_def| . xor))
    (⇔ (|booleans| . =))
    (AND (|booleans| . and) (|bv_bitwise| . bvand))
    (∧ (|booleans| . and))
    (& (|booleans| . and))
    (OR  (|booleans| . or) (|bv_bitwise| . bvor))
    (∨  (|booleans| . or))
    (NOT  (|booleans| . not)(|bv_bitwise| . bvnot))
    (¬ (|booleans| . not))
    (+  (|number_fields| . +)(|bv_arith_nat| . bvadd))
;    (- (|number_fields| . -)(|bv_arithmetic_defs| . bvsub));not in smt-lib2
    (*   (|number_fields| . *))
    (/  (|number_fields| . /))
;    (rem (|modulo_arithmetic| . mod))
;    (ndiv (|modulo_arithmetic| . div))
    (< (|reals| . <)(|bv_arith_nat| . bv-lt))
    (<=  (|reals| . <=)(|bv_arith_nat| . bv-le))
    (> (|reals| . >)(|bv_arith_nat| . bv-gt))
    (>=  (|reals| . >=)(|bv_arith_nat| . bv-ge))
    (O (|bv_concat_def| . bv-concat))
    (& (|booleans| . and)(|bv_bitwise| . bv-and))
;    (XOR  (|bv_bitwise| . bv-xor)) ;missing in smt2
    (^ (|bv_caret| .  extract))
    (sign_extend   (|bv_extend| . bv-sign-extend))
    ;; (|bv_slt| (|bv_arithmetic_defs| . bv-slt))
    ;; (|bv_sle| (|bv_arithmetic_defs| . bv-sle))
    ;; (|bv_sgt| (|bv_arithmetic_defs| . bv-sgt))
    ;; (|bv_sge| (|bv_arithmetic_defs| . bv-sge))
    (|bv_splus| (|bv_arithmetic_defs| . bvadd))
    (|bv_stimes| (|bv_arithmetic_defs| . bvmul))
    ))


(defun clear-smt2 ()
  (setq *smt2defns* nil)
  (clrhash *smt2name-hash*)
  (clrhash *translate-to-smt2-hash*)
  (newcounter *smt2-id-counter*))

(defun smt2-name (expr &optional id) ;;expr must have id field,
                                      ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *smt2name-hash*)))
	(or entry
	    (let ((name (smt2-id-name (or id (id expr)))))
	      (setf (gethash expr *smt2name-hash*) name)
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

(defmethod translate-to-smt2* ((ty type-name) bindings)
  (declare (ignore bindings))
  (let ((smt2name-hash (gethash ty *smt2name-hash*)))
    (or smt2name-hash 
	(cond ((enum-adt? ty)
	       ;; (let ((constructors (constructors ty))
	       ;; 	     (yname (yices2-name ty)))	;;bindings can be ignored
		 (error "smt2 translation does not support scalars")
		 ;;(translate-to-yices2-scalar yname constructors))
	       )
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "Bool"))
	      ((tc-eq ty *number*) "Real")
		;;else uninterpreted type
	      (t (let ((s2name (smt2-name ty)))
		  (push (format nil "(define-sort ~a)" s2name) *smt2defns*)
		  yname))))))

(defmethod translate-to-smt2* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ;;((tc-eq ty *naturalnumber*) 'nat) ;;there is no nat
	  ((tc-eq ty *integer*) "Int")
	  ((tc-eq ty *real*) "Real")
	  (t (translate-to-smt* supertype bindings)))))

(defmethod translate-to-smt2* ((ty tupletype) bindings)
  (with-slots (types) ty
    (let ((length (length types)))
      (if (<= length *smt2-tuple-length-bound*)
	  (format nil "(tuple_~a ~{~a ~})" length (translate-to-smt2-list types))
	(error "smt2 translation does not support tuples with length greater than ~a" *smt2-tuple-length-bound*)))))
