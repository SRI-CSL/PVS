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

(defun destructure-bindings (bndngs &key exclude)
  (let ((*exclude* exclude))
    (declare (special *exclude*))
    (destructure-bindings* bndngs)))

(defun destructure-bindings* (bndngs &optional xs types preds)
  (declare (special *exclude*))
  (if (null bndngs)
      (values (nreverse xs) (nreverse types) (nreverse preds))
    (multiple-value-bind (x type newpreds)
	(destructure-binding (car bndngs) :exclude *exclude*)
      (destructure-bindings* (cdr bndngs)
			    (cons x xs)
			    (cons type types)
			    (append newpreds preds)))))

(defun destructure-binding (bndng &key exclude)
  (assert (typep bndng 'bind-decl))
  (let ((x (make-variable-expr bndng)))
    (multiple-value-bind (type preds)
	(destructure-type x :exclude exclude)
      (values x type preds))))
	
(defun destructure-type (expr &key exclude (all? 'T))
  (let* ((jtypes (judgement-types+ expr))
	 (*subtypes-seen* nil)
	 (*expr* expr)
	 (*all* all?)
	 (*stop-types* (remove-if-not #'(lambda (ty)
					  (compatible? ty (type expr)))
			 (if (listp exclude) exclude (list exclude))))
	 (*stop-subtypes* (remove-if-not #'(lambda (ty)
					     (some #'(lambda (jty)
						       (subtype-of? jty ty))
						   jtypes))
			    *stop-types*)))
    (declare (special *expr* *subtypes-seen* *all* *stop-types*
		      *stop-subtypes*))
    (destructure-type* (or jtypes (type *expr*)) nil)))

(defmethod destructure-type* ((types cons) preds)
  (destructure-type-list types nil preds))

(defun destructure-type-list (types type preds)
  (if (null types)
      (values type preds)
      (multiple-value-bind (car-type car-preds)
	  (destructure-type* (car types) nil)
	(destructure-type-list
	 (cdr types)
	 (if type
	     (compatible-type car-type type)
	     car-type)
	 (nconc preds car-preds)))))

(defmethod destructure-type* ((texpr subtype) preds)
  (declare (special *stop-types* *stop-subtypes* *all* *expr*))
  (let ((stop-type (find-if #'(lambda (sty) (subtype-of? sty texpr))
		     *stop-types*)))
    (if stop-type
	(if (memq stop-type *stop-subtypes*)
	    (values stop-type preds)
	    (values texpr preds))
	(unless (or (member texpr *subtypes-seen* :test #'tc-eq)
		    (and (not *all*) (ignored-type-constraint texpr)))
	  (push texpr *subtypes-seen*)
	  (let ((supertype (supertype texpr))
		(pred (make!-reduced-application (predicate texpr) *expr*)))
	    (destructure-type* supertype (cons pred preds)))))))

(defmethod destructure-type* ((texpr dep-binding) preds)
   (destructure-type* (type texpr) preds))

(defmethod destructure-type* (texpr preds)
  (values texpr (nreverse preds)))

;; Type definitions

(defun natural-number-expr? (expr)
  (and (typep expr 'number-expr)
       (integerp (number expr))
       (>= (number expr) 0)))

(defun natural-expr? (expr)
  (assert (typep expr 'expr))
  (or (subtype-of? (type expr) *naturalnumber*)
      (natural-number-expr? expr)
      (and (application? expr)
	   (or (tc-eq (operator expr) (plus1))
	       (tc-eq (operator expr) (minus1))))
      (some #'(lambda (type) (subtype-of? type *naturalnumber*)) (judgement-types+ expr))))

;; (incomplete) check if the expression is a finite set of 
;; natural numbers

(defmethod finite-set-of-nat? ((ex expr))
  (some #'finite-set-of-nat? (judgement-types+ ex)))

(defmethod finite-set-of-nat? ((te subtype))
  (or (and (is-finite-of-nat? (predicate te))
	   (set-of-nat? (supertype te)))
      (finite-set-of-nat? (supertype te))))

(defmethod finite-set-of-nat? (te)
  (declare (ignore te))
  nil)

(defmethod is-finite-of-nat? ((ex name-expr))
  (and (eq (declaration ex) (is-finite-decl))
       (subtype-of? (type-value (car (actuals (module-instance ex))))
		    *naturalnumber*)))

(defmethod is-finite-of-nat? (ex)
  (declare (ignore ex))
  nil)

(defmethod set-of-nat? ((te funtype))
  (and (subtype-of? (domain te) *naturalnumber*)
       (tc-eq (range te) *boolean*)))

(defmethod set-of-nat? ((te subtype))
  (set-of-nat? (supertype te)))

(defmethod set-of-nat? (te)
  (declare (ignore te))
  nil)

(let ((is-finite-decl nil))
  (defun is-finite-decl ()
    (or is-finite-decl
	(setq is-finite-decl
	      (find-if #'(lambda (d)
			   (eq (id (module d)) '|finite_sets_def|))
		(gethash '|is_finite| (current-declarations-hash))))))
  
  (defun reset-is-finite-decl ()
    (setq is-finite-decl nil))
)

;; Check if some name-expr is a certain symbol of some theory 

(defun is? (op sym mod)
   (and (typep op 'name-expr)
	(eq (id op) sym)
	(eq (id (module-instance (car (resolutions op)))) mod)))
