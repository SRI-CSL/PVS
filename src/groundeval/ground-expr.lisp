
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

; dave_sc, 3/8/99
;
; "ground-ness" test for typechecked expressions.
;
; ground-expr?  --  no quantification.
;               --  no higher-order equalities.
;               --  nothing uninterpreted (inc "choose" & "epsilon")
;               --  not a closure
;               --  no free variables
;

(in-package :pvs)

(defun ground-expr? (expr)
  (assert (type expr))
  (and (not (freevars expr))          ; no free variables
       (not (funtype? (type expr)))   ; not a closure
       
       (ground-expr?* expr)))

(defun ground-type? (type)
  (ground-type?* type))

;;;was   (not (funtype? type)))


(defmethod ground-type?* ((type funtype))
  (let ((ab (array-bound type)))
    (and ab (ground-expr? ab))))

(defmethod ground-type?* ((type tupletype))
  (ground-type?* (types type)))

(defmethod ground-type?* ((type cotupletype))
  (ground-type?* (types type)))

(defmethod ground-type?* ((type recordtype))
  (ground-type?* (fields type)))

(defmethod ground-type?* ((type subtype))
  (ground-type?* (find-supertype type)))

(defmethod ground-type?* ((type type-name))
  nil) ;;this must be an undefined type.

(defmethod ground-type?* ((type type-expr))
  t)  

; No quantification

(defmethod ground-expr?* ((expr quant-expr))
  (declare (ignore expr))
  nil)

; No higher-order equality

(defmethod ground-expr?* ((expr equation))
  (and (not (ground-type? (find-supertype (args1 expr))))
       (call-next-method)))

; No higher-order disequality

(defmethod ground-expr?* ((expr disequation))
  (and (not (funtype? (find-supertype (args1 expr))))
       (call-next-method)))

; nothing uninterpreted

(defmethod ground-expr?* ((expr name-expr))
  (and (or (pvs2cl-primitive? expr)
	   (definition (declaration (resolution expr))))
       (ground-expr?* (actuals (module-instance (resolution expr))))))

; ADT derived operators a special case - just check actuals.

(defmethod ground-expr?* ((expr adt-name-expr))
  (ground-expr?* (actuals (module-instance (resolution expr)))))

; actuals may be types - ignore types in actuals

(defmethod ground-expr?* ((expr actual))
  (or (type-value expr)
      (ground-expr?* (expr expr))))

; methods to recurse on structure as appropriate

(defmethod ground-expr?* ((expr application))
  (and (ground-expr?* (operator expr))
       (ground-expr?* (argument expr))))

(defmethod ground-expr?* ((expr tuple-expr))
  (ground-expr?* (exprs expr)))

(defmethod ground-expr?* ((expr field-application))
  (ground-expr?* (argument expr)))

(defmethod ground-expr?* ((expr projection-application))
  (ground-expr?* (argument expr)))

(defmethod ground-expr?* ((expr injection-application))
  (ground-expr?* (argument expr)))

(defmethod ground-expr?* ((expr record-expr))
  (ground-expr?* (assignments expr)))

(defmethod ground-expr?* ((expr cases-expr))
  (and (ground-expr?* (expression expr))
       (ground-expr?* (selections expr))
       (ground-expr?* (else-part expr))))

(defmethod ground-expr?* ((expr selection))
  (ground-expr?* (expression expr)))

(defmethod ground-expr?* ((expr update-expr))
  (and (ground-expr?* (expression expr))
       (ground-expr?* (assignments expr))))

(defmethod ground-expr?* ((expr assignment))
  (and (ground-expr?* (expression expr))
       (ground-expr?* (arguments expr))))


(defmethod ground-expr?* ((expr list))
  (every #'ground-expr?* expr))

; lambda-exprs may introduce higher-order complications
; should check this out further....

(defmethod ground-expr?* ((expr binding-expr))
  (declare (ignore expr))
  t)

(defmethod ground-expr?* ((expr field-assignment-arg))
  (declare (ignore expr))
  t)

; fundamental things are ground

(defmethod ground-expr?* ((expr number-expr))
  (declare (ignore expr))
  t)
  
; temporary method to see what we're not catching.

(defmethod ground-expr?* (expr)
  (format t "~%DEBUG: fell through ground-expr on ~a" expr)
  (break))
