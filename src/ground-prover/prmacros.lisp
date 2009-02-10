;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-

;------------------------------------------------------------------------------
; PRMACROS - Macros for the prover subsystem
;------------------------------------------------------------------------------

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


(defmacro newcontext (form)
  `(let ((sigalist sigalist) (findalist findalist) (usealist usealist))
     (catch 'context 
       ,form )))

(defmacro retfalse () `(throw 'context 'false))

; term accessor macros

(defmacro funsym (term) `(car ,term))

(defmacro arg1 (term) `(cadr ,term))

(defmacro arg2 (term) `(caddr ,term))

(defmacro arg3 (term) `(cadddr ,term))

(defmacro argsof (term) `(cdr ,term))

(defmacro lside (term) `(cadr ,term))

(defmacro rside (term) `(caddr ,term))

; getq:
; (getq arg alist) returns the item associated with arg in alist, or nil.

(defmacro getq (arg alist) `(cdr (assq ,arg ,alist)))

(defun prerr (&rest args)
  (apply #'error args))

(defconstant-if-unbound *truecons* '(true))

(defconstant-if-unbound *eqarithrels* '(greatereqp lesseqp))

(defconstant *ifops* nil ;;'(if if*)
  )

(defconstant-if-unbound *boolconstants* '(false true))

(defconstant-if-unbound *arithrels* '(lessp lesseqp greaterp greatereqp))

(defconstant-if-unbound *arithops* '(PLUS TIMES DIFFERENCE MINUS))

(defconstant-if-unbound *boolops* '(and or implies not ;;if
				    iff))

(defmacro singleton? (obj)
  `(and (consp ,obj) (null (cdr ,obj))))


(defun add2pot (atoms)
  (let ((atoms (nreverse atoms)))
    (loop for atom in atoms
	  when (not (and (bound-inequality? atom)
			 (subsumed-ineq-list? atom s)))
	  do (push atom s))))

(defmacro addneq2pot (atoms)
  `(add2pot ,atoms))

(defmacro addineq2pot (atoms)
  `(add2pot ,atoms))

(defmacro addprm2pot (atoms)
  `(add2pot ,atoms))

(defmacro arithlist (x)
  `(let ((xx ,x))
     (if (and (consp xx)
	   (qnumberp (car xx)))
      (cdr xx)
      xx)))
