;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gensubst.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Oct 20 01:35:37 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Apr 15 16:01:21 1998
;; Update Count    : 24
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

(defvar *gensubst-cache* (make-hash-table :test #'eq))

(defvar *dont-expand-adt-subtypes* nil)

(defvar *gensubst-reset-types* nil)

(defvar *gensubst-subst-types* nil)

(defun gensubst (obj substfn testfn)
  "gensubst takes a pvs abstract term obj, and a subst and test fn.  It
basically walks down the obj ast, replacing those subterms that satisfy
testfn by invoking substfn.  Various global variables control the detailed
behavior:
   *dont-expand-adt-subtypes*
   *gensubst-subst-types*
   *gensubst-reset-types*
   *parsing-or-unparsing*
   *visible-only*
   "
  (unwind-protect (gensubst* obj substfn testfn)
    (clrhash *gensubst-cache*)))

(defmethod gensubst* :around (obj substfn testfn)
  (or (and *gensubst-cache*
	   (gethash obj *gensubst-cache*))
      (let ((nobj (if (funcall testfn obj)
		      (funcall substfn obj)
		      (call-next-method))))
	(if (and *gensubst-cache*
		 (not *dont-expand-adt-subtypes*))
	    (setf (gethash obj *gensubst-cache*) nobj)
	    nobj))))

(defmethod gensubst* ((obj string) substfn testfn)
  (declare (ignore substfn testfn))
  obj)

(defmethod gensubst* ((obj module) substfn testfn)
  (let* ((formals (gensubst* (formals obj) substfn testfn))
	 (formals-sans-usings (if (eq formals (formals obj))
				  (formals-sans-usings obj)
				  (remove-if #'(lambda (ff)
						 (typep ff 'importing))
				    formals))))

	
    (lcopy obj
      'formals formals
      'formals-sans-usings formals-sans-usings
      'assuming (gensubst* (assuming obj) substfn testfn)
      'exporting (gensubst* (exporting obj) substfn testfn)
      'theory (gensubst* (theory obj) substfn testfn))))

(defmethod gensubst* ((obj recursive-type) substfn testfn)
  (let* ((formals (gensubst* (formals obj) substfn testfn))
	 (formals-sans-usings (if (eq formals (formals obj))
				  (formals-sans-usings obj)
				  (remove-if #'(lambda (ff)
						 (typep ff 'importing))
				    formals))))
    (lcopy obj
      'formals formals
      'formals-sans-usings formals-sans-usings
      'importings (gensubst* (importings obj) substfn testfn)
      'constructors (gensubst* (constructors obj) substfn testfn))))

(defmethod gensubst* ((obj simple-constructor) substfn testfn)
  (lcopy obj
    'arguments (gensubst* (arguments obj) substfn testfn)))

(defmethod gensubst* ((obj adtdecl) substfn testfn)
  (lcopy obj 'bind-decl (gensubst* (bind-decl obj) substfn testfn)))

(defmethod gensubst* ((decl formula-decl) substfn testfn)
  (lcopy (call-next-method)
    'definition (gensubst* (definition decl) substfn testfn)))

;(defmethod gensubst* :around ((obj syntax) substfn testfn)
;  (let ((nobj (call-next-method)))
;    (if (or (eq nobj obj)
;	    (null *copy-abstract-syntax*))
;	nobj
;	(progn (setf (abstract-syntax nobj) (abstract-syntax obj))
;	       nobj))))

(defmethod gensubst* :around ((te type-expr) substfn testfn)
  (let ((nte (call-next-method)))
    (if (or (eq te nte)
	    (null (print-type te))
	    ;; It's possible for the print-type to have been set if te is a
	    ;; datatype-subtype.  This comes from
	    ;;   gensubst* (subtype) -> gensubst* (type-name)
	    ;;                     -> adt-expand-positive-subtypes!
	    (not (eq (print-type nte) (print-type te))))
	nte
	(let* ((*dont-expand-adt-subtypes* t)
	       (npt (gensubst* (print-type te) substfn testfn)))
	  ;; Note that npt could come from the *gensubst-cache*
	  ;;#+pvsdebug
	  (assert (typep (print-type te)
			 '(or null type-name expr-as-type
			   type-application)))
	  (if (typep npt '(or null type-name
			   expr-as-type type-application))
	      (copy nte :print-type npt)
	      (copy nte :print-type (print-type npt)))))))

(defmethod gensubst* ((te print-type-name) substfn testfn)
  (let ((mi (gensubst* (module-instance te) substfn testfn)))
    (if (eq mi (module-instance te))
	te
	(copy te
	  :actuals (when (actuals te) (actuals mi))
	  :dactuals (when (dactuals te) (dactuals mi))
	  :resolutions (list (mk-resolution (declaration te) mi nil))))))

(defmethod gensubst* ((te print-type-application) substfn testfn)
  (lcopy te
    :type (gensubst* (type te) substfn testfn)
    :parameters (gensubst* (parameters te) substfn testfn)))

(defmethod gensubst* ((te print-expr-as-type) substfn testfn)
  (lcopy te :expr (gensubst* (expr te) substfn testfn)))

(defmethod gensubst* ((list list) substfn testfn)
  (let ((nlist (gensubst-list list substfn testfn nil)))
    (if (equal nlist list) list nlist)))

(defun gensubst-list (list substfn testfn nlist)
  (if (null list)
      (nreverse nlist)
      (let* ((ncar (gensubst* (car list) substfn testfn))
	     (ncdr (if (and (not *parsing-or-unparsing*)
			    (not (eq ncar (car list)))
			    (typep (car list) 'binding)
			    (declaration (car list)))
		       (substit (cdr list) (acons (car list) ncar nil))
		       (cdr list))))
	(if (listp ncdr)
	    (gensubst-list ncdr substfn testfn (cons ncar nlist))
	    (cons ncar (gensubst* ncdr substfn testfn))))))

(defmethod gensubst* ((obj exporting) substfn testfn)
  (lcopy obj
    'names (gensubst* (slot-value obj 'names) substfn testfn)
    'but-names (gensubst* (but-names obj) substfn testfn)
    'modules (gensubst* (modules obj) substfn testfn)))

(defmethod gensubst* ((using importing) substfn testfn)
  (lcopy using 'theory-name (gensubst* (theory-name using) substfn testfn)))

(defmethod gensubst* ((decl declaration) substfn testfn)
  (declare (ignore substfn testfn))
  decl)

(defmethod gensubst* ((decl typed-declaration) substfn testfn)
  (lcopy decl
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type decl) substfn testfn))
    'type (gensubst* (type decl) substfn testfn)))

(defmethod gensubst* ((decl type-def-decl) substfn testfn)
  (lcopy decl
    'type-value (gensubst* (type-value decl) substfn testfn)
    'type-expr  (gensubst* (type-expr decl) substfn testfn)
    'contains   (gensubst* (contains decl) substfn testfn)))

(defmethod gensubst* ((decl formal-subtype-decl) substfn testfn)
  (lcopy decl
    'type-value (gensubst* (type-value decl) substfn testfn)
    'type-expr  (gensubst* (type-expr decl) substfn testfn)))

(defmethod gensubst* ((decl formal-const-decl) substfn testfn)
  (lcopy decl
    'declared-type (gensubst* (declared-type decl) substfn testfn)
    'type (gensubst* (type decl) substfn testfn)))

(defmethod gensubst* ((decl mod-decl) substfn testfn)
  (lcopy decl 'modname (gensubst* (modname decl) substfn testfn)))

(defmethod gensubst* ((decl theory-abbreviation-decl) substfn testfn)
  (lcopy decl 'theory-name (gensubst* (theory-name decl) substfn testfn)))

(defmethod gensubst* ((decl const-decl) substfn testfn)
  (lcopy (call-next-method)
    'definition (gensubst* (definition decl) substfn testfn)))

(defmethod gensubst* ((decl def-decl) substfn testfn)
  (lcopy (call-next-method)
    'measure (gensubst* (measure decl) substfn testfn)
    'ordering (gensubst* (ordering decl) substfn testfn)))

(defmethod gensubst* ((te dep-binding) substfn testfn)
  (if (and (not *parsing-or-unparsing*)
	   (type te))
      (let ((ntype (if *visible-only*
		     (type te)
		     (gensubst* (type te) substfn testfn))))
	(if (eq ntype (type te))
	    te
	    (let ((dtype (let ((*dont-expand-adt-subtypes* t))
			   (gensubst* (declared-type te) substfn testfn))))
	      (lcopy te :type ntype :declared-type dtype))))
      (let ((dtype (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type te) substfn testfn))))
	(lcopy te :declared-type dtype))))

(defmethod gensubst* ((te type-name) substfn testfn)
  (cond ((and (resolution te)
	      (type (resolution te))
	      (tc-eq (type (resolution te)) te))
	 (let ((nmi (gensubst* (module-instance te) substfn testfn)))
	   (if (eq nmi (module-instance te))
	       te
	       (let* ((nres (mk-resolution (declaration te) nmi nil))
		      (nte (copy te
			     :actuals (actuals nmi)
			     :dactuals (dactuals nmi)
			     :mappings (mappings nmi)
			     :resolutions (list nres)))
		      (ente (if (and (not *dont-expand-adt-subtypes*)
				     (resolution te)
				     (adt-expand-positive-subtypes? nte))
				(adt-expand-positive-subtypes! nte)
				nte)))
		 (setf (type nres) ente)
		 ente))))
	(t (let ((nte (call-next-method)))
	     (if (and (not *dont-expand-adt-subtypes*)
		      (resolution te)
		      (adt-expand-positive-subtypes? nte))
		 (adt-expand-positive-subtypes! nte)
		 nte)))))

(defmethod gensubst* ((te type-application) substfn testfn)
  (let ((ntype (gensubst* (type te) substfn testfn))
	(nargs (gensubst* (parameters te) substfn testfn)))
    (lcopy te 'type ntype 'parameters nargs)))

(defmethod gensubst* ((te expr-as-type) substfn testfn)
  (let ((nexpr (gensubst* (expr te) substfn testfn)))
    (lcopy te
      'expr (if (or (null (type (expr te))) ; not yet typechecked
		    *parsing-or-unparsing*
		    *visible-only*
		    (eq nexpr (expr te)))
		nexpr
		(pseudo-normalize nexpr)))))

(defmethod gensubst* ((te subtype) substfn testfn)
  (let ((stype (gensubst* (supertype te) substfn testfn))
	(pred (gensubst* (predicate te) substfn testfn)))
    (lcopy te
      'supertype stype
      'predicate (if (or *parsing-or-unparsing*
			 *visible-only*
			 (eq pred (predicate te)))
		     pred
		     (pseudo-normalize pred)))))

(defmethod gensubst* ((te datatype-subtype) substfn testfn)
  (let ((stype (gensubst* (supertype te) substfn testfn))
	(dtype (let ((*dont-expand-adt-subtypes* t))
		 (gensubst* (declared-type te) substfn testfn)))
	(pred (gensubst* (predicate te) substfn testfn)))
    (lcopy te
      'supertype stype
      'declared-type dtype
      'predicate (if (or *parsing-or-unparsing*
			 *visible-only*
			 (eq pred (predicate te)))
		     pred
		     (pseudo-normalize pred)))))

(defmethod gensubst* ((te setsubtype) substfn testfn)
  ;; setsubtype adds formula and formals slots, from which the predicate is created
  (if (predicate te)
      (let ((nte (call-next-method)))
	(multiple-value-bind (formals alist)
	    (apply-to-bindings #'(lambda (bd) (gensubst* bd substfn testfn))
			       (formals te))
	  (let ((formula (substit (gensubst* (formula te) substfn testfn) alist)))
	    (lcopy nte :formals formals :formula formula))))
      (let ((nform (gensubst* (formula te) substfn testfn)))
	(lcopy te 'formula nform))))

(defmethod gensubst* ((te funtype) substfn testfn)
  (if *parsing-or-unparsing*
      (let* ((types (list (domain te) (range te)))
	     (ntypes (gensubst* types substfn testfn)))
	(if (eq types ntypes)
	    te
	    (copy te 'domain (car ntypes) 'range (cadr ntypes))))
      (let* ((dom (gensubst* (domain te) substfn testfn))
	     (ran (gensubst* (range te) substfn testfn)))
	(assert (eq (dep-binding? (domain te)) (dep-binding? dom)))
	(lcopy te
	  :domain dom
	  :range (if (dep-binding? dom)
		     (substit ran (acons (domain te) dom nil))
		     ran)))))

(defmethod gensubst* ((te tupletype) substfn testfn)
  (if *parsing-or-unparsing*
      (let ((types (gensubst* (types te) substfn testfn)))
	(lcopy te 'types types))
      (let ((types (apply-to-bindings #'(lambda (ty) (gensubst* ty substfn testfn))
				      (types te))))
	(if (equal types (types te))
	    te
	    (lcopy te 'types types)))))

(defmethod gensubst* ((te cotupletype) substfn testfn)
  (let ((types (gensubst* (types te) substfn testfn)))
    (lcopy te 'types types)))

(defmethod gensubst* ((te recordtype) substfn testfn)
  (let ((fields (apply-to-bindings #'(lambda (fld) (gensubst* fld substfn testfn))
				   (fields te))))
    (if (equal fields (fields te))
	te
	(copy te
	  'fields (if *parsing-or-unparsing*
		      fields
		      (sort-fields fields
				   (dependent-fields? fields)))))))


(defmethod gensubst* ((fd field-decl) substfn testfn)
  (let ((ntype (if *visible-only*
		   (type fd)
		   (gensubst* (type fd) substfn testfn))))
    (if (and ntype
	     (eq ntype (type fd))
	     (not *visible-only*))
	fd
	(let ((dtype (let ((*dont-expand-adt-subtypes* t))
		       (gensubst* (declared-type fd) substfn testfn))))
	  (lcopy fd 'type ntype 'declared-type dtype)))))

(defmethod gensubst* ((te struct-sub-recordtype) substfn testfn)
  (let ((fields (gensubst* (fields te) substfn testfn)))
    (if (eq fields (fields te))
	te
	(copy te
	  'fields (if *parsing-or-unparsing*
		      fields
		      (sort-fields fields
				   (dependent-fields? fields)))))))

(defmethod gensubst* ((te struct-sub-tupletype) substfn testfn)
  (let ((types (gensubst* (types te) substfn testfn)))
    (lcopy te 'types types)))

(defmethod gensubst* ((te type-extension) substfn testfn)
  (let ((type (gensubst* (type te) substfn testfn))
	(ext (gensubst* (extension te) substfn testfn)))
    (lcopy te 'type type 'extension ext)))

;;; Expressions

(defmethod gensubst* :around ((ex expr) substfn testfn)
  (let ((nex (call-next-method)))
    (cond (*gensubst-reset-types*
	   (unless (eq ex nex)
	     (setf (type ex) nil
		   (types ex) nil)
	     (when (name-expr? ex)
	       (setf (resolutions ex) nil))))
	  (*gensubst-subst-types*
	   (when (and (eq ex nex) (type ex)
		      (not (name-expr? ex))) ;; type already done for name-exprs
	     (let ((ntype (gensubst* (type ex) substfn testfn)))
	       (unless (eq ntype (type ex))
		 (setq nex (copy ex :type ntype))))))
	  (t (unless (or (eq ex nex) (type ex))
	       (setf (types nex) (gensubst* (types ex) substfn testfn)))))
    nex))

(defmethod gensubst* ((ex name-expr) substfn testfn)
  (declare (ignore substfn testfn))
  (let ((nex (call-next-method)))
    (if (or *parsing-or-unparsing*
	    *visible-only*
	    (and (not *gensubst-reset-types*)
		 (eq ex nex)))
	nex
	(let ((ntype (type (resolution nex))))
	  (if (eq ntype (type ex))
	      nex
	      (lcopy nex
		'type ntype))))))

(defmethod gensubst* ((ex adt-name-expr) substfn testfn)
  (let ((nex (call-next-method)))
    (if (or *visible-only* (eq ex nex))
	nex
	(copy nex 'adt-type (gensubst* (adt-type ex) substfn testfn)))))

(defmethod gensubst* ((ex number-expr) substfn testfn)
  (declare (ignore substfn testfn))
  ex)

(defmethod gensubst* ((ex rational-expr) substfn testfn)
  (declare (ignore substfn testfn))
  ex)

(defmethod gensubst* ((ex record-expr) substfn testfn)
  (let ((ass (gensubst* (assignments ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'assignments ass 'type ntype)))

(defmethod gensubst* ((ex tuple-expr) substfn testfn)
  (let ((exprs (gensubst* (exprs ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex
      :exprs (if (equal exprs (exprs ex)) (exprs ex) exprs)
      :type ntype)))

;(defmethod gensubst* ((ex coercion) substfn testfn)
;  (let ((nexpr (gensubst* (expression ex) substfn testfn))
;	(ntype (gensubst* (type ex) substfn testfn)))
;    (lcopy ex 'expression nexpr 'type ntype)))

;(defmethod gensubst* ((ex intype) substfn testfn)
;  (let ((nexpr (gensubst* (expression ex) substfn testfn))
;	(ntype (gensubst* (type-value ex) substfn testfn)))
;    (lcopy ex 'expression nexpr 'type-value ntype)))

(defmethod gensubst* ((ex cases-expr) substfn testfn)
  (let ((nexpr (gensubst* (expression ex) substfn testfn)))
    (if (or (eq nexpr (expression ex))
	    *parsing-or-unparsing*
	    (compatible? (type nexpr) (type (expression ex))))
	(let ((sels (gensubst* (selections ex) substfn testfn))
	      (else (gensubst* (else-part ex) substfn testfn))
	      (ntype (if (or *parsing-or-unparsing*
			     *visible-only*)
			 (type ex)
			 (gensubst* (type ex) substfn testfn))))
	  (lcopy ex
	    'expression nexpr
	    'selections sels
	    'else-part else
	    'type ntype))
	(gensubst* (translate-cases-to-if ex) substfn testfn))))

(defmethod gensubst* ((ex fieldex) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex projection-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex injection-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex injection?-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex extraction-expr) substfn testfn)
  (let ((ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'type ntype)))

(defmethod gensubst* ((ex projection-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex injection-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex injection?-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex extraction-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex field-application) substfn testfn)
  (let ((narg (gensubst* (argument ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'argument narg 'type ntype)))

(defmethod gensubst* ((ex application) substfn testfn)
  (let* ((nop (gensubst* (operator ex) substfn testfn))
	 (narg (gensubst* (argument ex) substfn testfn))
	 (ntype (cond ((or *parsing-or-unparsing*
			   *visible-only*
			   (and (not *gensubst-subst-types*)
				(eq nop (operator ex))
				(eq narg (argument ex))))
		       (type ex))
		      ((dep-binding? (domain (find-supertype (type nop))))
		       (substit (range (find-supertype (type nop)))
			 (acons (domain (find-supertype (type nop))) narg
				nil)))
		      (t (range (find-supertype (type nop)))))))
    (simplify-or-copy-app
     ex
     (if (or (eq narg (argument ex))
	     (not (eq nop (operator ex))))
	 nop
	 (copy nop))
     narg
     ntype)))

(defmethod gensubst* :around ((ex table-expr) substfn testfn)
  (let ((nex (call-next-method)))
    (if (eq ex nex)
	ex
	(lcopy nex
	  'row-headings (gensubst* (row-headings nex) substfn testfn)
	  'col-headings (gensubst* (col-headings nex) substfn testfn)
	  'table-entries (gensubst* (table-entries nex) substfn testfn)))))

(defmethod gensubst* ((ex binding-expr) substfn testfn)
  (multiple-value-bind (nbindings alist)
      (apply-to-bindings #'(lambda (bd) (gensubst* bd substfn testfn))
			 (bindings ex))
    (let* ((gexpr (gensubst* (expression ex) substfn testfn))
	   (nexpr (if (or *parsing-or-unparsing*
			  (null alist) ; no changes to bindings
			  (not (declaration (car (bindings ex)))))
		      gexpr
		      (substit gexpr alist)))
	   (ntype (if (or *visible-only*
			  *parsing-or-unparsing*)
		      (type ex)
		      (substit (gensubst* (type ex) substfn testfn) alist))))
      (if (and (eq (expression ex) nexpr) (null alist))
	  (lcopy ex 'type ntype)
	  (let ((nex (copy ex :bindings nbindings :expression nexpr :type ntype)))
	    ;; (assert (or *visible-only*
	    ;; 		*parsing-or-unparsing*
	    ;; 		(set-equal (freevars nex) (freevars ex) :test #'same-declaration)))
	    nex)))))

(defmethod gensubst* ((ex bind-decl) substfn testfn)
  (let ((ntype (if *visible-only*
		   (type ex)
		   (gensubst* (type ex) substfn testfn)))
	(ndeclared-type (let ((*dont-expand-adt-subtypes* t))
			  (gensubst* (declared-type ex) substfn testfn))))
    (lcopy ex
      'type ntype
      'declared-type ndeclared-type)))

(defmethod gensubst* ((ex update-expr) substfn testfn)
  (let ((nexpr (gensubst* (expression ex) substfn testfn))
	(nass (gensubst* (assignments ex) substfn testfn))
	(ntype (if (or *parsing-or-unparsing*
		       *visible-only*)
		   (type ex)
		   (gensubst* (type ex) substfn testfn))))
    (lcopy ex 'expression nexpr 'assignments nass 'type ntype)))

(defmethod gensubst* ((ex assignment) substfn testfn)
  (let ((nargs (gensubst* (arguments ex) substfn testfn))
	(nexpr (gensubst* (expression ex) substfn testfn)))
    (lcopy ex 'arguments nargs 'expression nexpr)))

(defmethod gensubst* ((ex selection) substfn testfn)
  (let ((nconstr (gensubst* (constructor ex) substfn testfn))
	(nargs (gensubst* (args ex) substfn testfn))
	(nexpr (gensubst* (expression ex) substfn testfn)))
    (if (and (eq nconstr (constructor ex))
	     (eq nexpr (expression ex))
	     (eq nargs (args ex)))
	ex
	(lcopy ex
	  'constructor nconstr
	  'args nargs
	  'expression (if (and (not *parsing-or-unparsing*)
			       (args ex) (declaration (car (args ex))))
			  (substit nexpr (pairlis (args ex) nargs))
			  nexpr)))))

(defmethod gensubst* ((name name) substfn testfn)
  (if (or *parsing-or-unparsing*
	  *visible-only*
	  (null (resolutions name)))
      (lcopy name
	'actuals (gensubst* (actuals name) substfn testfn)
	'dactuals (gensubst* (dactuals name) substfn testfn)
	'mappings (gensubst* (mappings name) substfn testfn))
      (let ((nres (gensubst* (resolutions name) substfn testfn)))
	(if (eq nres (resolutions name))
	    name
	    (if (name-expr? name)
		(let ((ntype (when (type name)
			       (if (singleton? nres)
				   (type (car nres))
				   (break "shouldn't happen")))))
		  (copy name
		    'resolutions nres
		    'type ntype
		    'actuals (gensubst* (actuals name) substfn testfn)
		    'dactuals (gensubst* (dactuals name) substfn testfn)
		    'mappings (gensubst* (mappings name) substfn testfn)))
		(copy name
		  'resolutions nres
		  'actuals (gensubst* (actuals name) substfn testfn)
		  'dactuals (gensubst* (dactuals name) substfn testfn)
		  'mappings (gensubst* (mappings name) substfn testfn)))))))

(defmethod gensubst* ((map mapping) substfn testfn)
  (lcopy map
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type map) substfn testfn))
    'type (gensubst* (type map) substfn testfn)
    'lhs (gensubst* (lhs map) substfn testfn)
    'rhs (gensubst* (rhs map) substfn testfn)))

(defmethod gensubst* ((map mapping-with-formals) substfn testfn)
  (lcopy map
    'declared-type (let ((*dont-expand-adt-subtypes* t))
		     (gensubst* (declared-type map) substfn testfn))
    'type (gensubst* (type map) substfn testfn)
    'lhs (gensubst* (lhs map) substfn testfn)
    'rhs (gensubst* (rhs map) substfn testfn)
    'formals (apply-to-bindings #'(lambda (bd) (gensubst* bd substfn testfn))
				(formals map))))

(defmethod gensubst* ((res resolution) substfn testfn)
  (with-slots (declaration module-instance type) res
    (let* ((mi (theory-instance-with-lib res))
	   (nmi (gensubst* mi substfn testfn)))
      (cond ((eq nmi mi)
	     (if (and *gensubst-subst-types*
		      (funcall testfn declaration))
		 (let ((ndecl (gensubst* declaration substfn testfn)))
		   (if (eq ndecl declaration)
		       res
		       (make-resolution ndecl nmi)))
		 res))
	    ((and (type-expr? type)
		  (type-name? (print-type type))
		  (eq (declaration (resolution (print-type type))) declaration))
	     (let* ((ntype (gensubst* (copy type :print-type nil) substfn testfn))
		    (nres (mk-resolution declaration nmi ntype)))
	       (unless (print-type ntype)
		 (let ((nprint-type (lcopy (print-type type)
				      :actuals (actuals nmi)
				      :dactuals (dactuals nmi)
				      :mappings (mappings nmi)
				      :resolutions (list (copy nres :type nil)))))
		   (setf (print-type ntype) nprint-type)))
	       nres))
	    (*gensubst-subst-types*
	     (let ((ntype (gensubst* type substfn testfn)))
	       (mk-resolution declaration nmi ntype)))
	    (t (make-resolution declaration nmi))))))
	       

(defmethod gensubst* ((act actual) substfn testfn)
  (if (and (not *parsing-or-unparsing*)
	   ;;(not *visible-only*)
	   (type-value act))
      (let ((ntype (gensubst* (type-value act) substfn testfn)))
	(if (and (eq ntype (type-value act))
		 (not *visible-only*))
	    act
	    (let ((nact (mk-actual ntype)))
	      (when (typep (expr act) 'expr-as-type)
		(setf (expr nact) (gensubst* (expr act) substfn testfn)))
	      nact)))
      (let* ((gexpr (gensubst* (expr act) substfn testfn))
	     (nexpr (if (or *parsing-or-unparsing*
			    *visible-only*
			    (eq gexpr (expr act))
			    (not (typep gexpr 'expr))
			    (and (name-expr? (expr act))
				 (module? (declaration (expr act)))))
			gexpr
			(pseudo-normalize gexpr))))
	;; type-value shouldn't matter - but setting it to nil causes
	;; problems for bugs/1999-01-29_EllenMunthe-Kass,
	;; and leaving it alone means full-name doesn't work correctly
	(if (eq nexpr (expr act))
	    act
	    (lcopy act
	      'expr nexpr
	      'type-value (gensubst* (type-value act) substfn testfn))))))

(defmethod gensubst* ((name modname) substfn testfn)
  (let* ((gacts (gensubst* (actuals name) substfn testfn))
	 (nacts (if (and gacts
			 (not (eq gacts (actuals name)))
			 (memq (id name) '(|equalities| |notequal|)))
		    (list (mk-actual (find-supertype
				      (type-value (car gacts)))))
		    gacts))
	 (ndacts (gensubst* (dactuals name) substfn testfn))
	 (nmaps (gensubst* (mappings name) substfn testfn))
	 (nthname (lcopy name
		    'actuals nacts
		    'dactuals ndacts
		    'mappings nmaps)))
    (if (or (eq nthname name)
	    (null (resolutions name)))
	nthname
	(let ((res (mk-resolution (declaration (resolution name)) nthname nil)))
	  (setf (resolutions nthname) (list res))
	  nthname))))
	  

(defmethod gensubst* ((obj symbol) substfn testfn)
  (declare (ignore substfn testfn))
  obj)

(defmethod gensubst* ((obj s-formula) substfn testfn)
  (let ((nfmla (gensubst* (formula obj) substfn testfn)))
    (lcopy obj
      'formula nfmla)))


;;; Mapobject - maps a function fn down the abstract syntax.
;;;             Returns the object
;;;             If the function fn returns non-NIL, will not go down
;;;                subcomponents.
;;;  Notes: 1. If the type-value of an actual is set, will not go down
;;;            the expr.
;;;         2. Caches the result for each call to mapobject in *mapobject-cache*
;;;         3. *parsing-or-unparsing* controls whether to go down types, etc.

(defvar *mapobject-cache* (make-hash-table :test #'eq :size 37))

(defvar *mapobject-infix* nil)

(defun mapobject (fn obj)
  (unwind-protect
      (mapobject* fn obj)
    (clrhash *mapobject-cache*))
  obj)

(defmethod mapobject* :around (fn obj)
  (unless (gethash obj *mapobject-cache*)
    (setf (gethash obj *mapobject-cache*) t)
    (unless (funcall fn obj)
      (call-next-method))))

(defmethod mapobject* (fn (obj null))
  (declare (ignore fn))
  nil)

(defmethod mapobject* (fn (obj cons))
  (mapobject* fn (car obj))
  (mapobject* fn (cdr obj)))

(defmethod mapobject* (fn (obj datatype-or-module))
  (mapobject* fn (formals obj))
  (mapobject* fn (assuming obj)))

(defmethod mapobject* (fn (obj module))
  (call-next-method)
  (mapobject* fn (exporting obj))
  (mapobject* fn (theory obj)))

(defmethod mapobject* (fn (obj exporting))
  (mapobject* fn (names obj))
  (mapobject* fn (but-names obj))
  (mapobject* fn (modules obj)))

(defmethod mapobject* (fn (obj importing))
  (mapobject* fn (theory-name obj)))

;;; Declarations

(defmethod mapobject* (fn (obj declaration))
  (mapobject* fn (formals obj)))

(defmethod mapobject* (fn (obj typed-declaration))
  (mapobject* fn (declared-type obj))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type obj)))
  (call-next-method))

(defmethod mapobject* (fn (obj theory-reference))
  (mapobject* fn (theory-name obj))
  (call-next-method))

(defmethod mapobject* (fn (obj type-decl))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type-value obj)))
  (call-next-method))

(defmethod mapobject* (fn (obj type-def-decl))
  (mapobject* fn (type-expr obj))
  (mapobject* fn (contains obj))
  (call-next-method))

(defmethod mapobject* (fn (obj const-decl))
  (mapobject* fn (definition obj))
  (call-next-method))

(defmethod mapobject* (fn (obj def-decl))
  (mapobject* fn (definition obj))
  (mapobject* fn (measure obj))
  (call-next-method))

(defmethod mapobject* (fn (obj formula-decl))
  (mapobject* fn (definition obj))
  ;;(mapobject* fn (justification obj))
  (call-next-method))

(defmethod mapobject* :around (fn (obj tcc-decl))
  (declare (ignore fn))
  (unless *parsing-or-unparsing*
    (call-next-method)))

(defmethod mapobject* (fn (obj subtype-judgement))
  (mapobject* fn (subtype obj))
  (call-next-method))

(defmethod mapobject* (fn (obj name-judgement))
  (mapobject* fn (name obj))
  (call-next-method))

(defmethod mapobject* (fn (obj application-judgement))
  (mapobject* fn (name obj))
  (mapobject* fn (formals obj))
  (call-next-method))

;;; Type Expressions

(defmethod mapobject* :around (fn (te type-expr))
  (unless (and *parsing-or-unparsing*
	       (print-type te))
    (call-next-method))
  (mapobject* fn (print-type te)))

(defmethod mapobject* (fn (te subtype))
  (unless *parsing-or-unparsing*
    (mapobject* fn (supertype te)))
  (mapobject* fn (predicate te)))

(defmethod mapobject* (fn (te type-application))
  (call-next-method)
  (mapobject* fn (type te))
  (mapobject* fn (parameters te)))

(defmethod mapobject* (fn (te setsubtype))
  (call-next-method)
  ;;(mapobject* fn (formals te))
  (mapobject* fn (formula te)))

(defmethod mapobject* (fn (te expr-as-type))
  (call-next-method)
  (mapobject* fn (expr te)))

(defmethod mapobject* (fn (te funtype))
  (mapobject* fn (domain te))
  (mapobject* fn (range te)))

(defmethod mapobject* (fn (te tupletype))
  (mapobject* fn (types te)))

(defmethod mapobject* (fn (te cotupletype))
  (mapobject* fn (types te)))

(defmethod mapobject* (fn (te recordtype))
  (mapobject* fn (fields te)))

(defmethod mapobject* (fn (fd field-decl))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type fd)))
  (mapobject* fn (declared-type fd)))

(defmethod mapobject* (fn (te struct-sub-recordtype))
  (mapobject* fn (fields te)))

(defmethod mapobject* (fn (te struct-sub-tupletype))
  (mapobject* fn (types te)))

;;; Expressions

(defmethod mapobject* :around (fn (ex expr))
  (call-next-method)
  (unless *parsing-or-unparsing*
    (mapobject* fn (type ex))
    (unless (type ex)
      (mapobject* fn (types ex)))))

;(defmethod mapobject* (fn (ex name-expr))
;  (declare (ignore fn))
;  (call-next-method)			; For names
;  )

(defmethod mapobject* (fn (ex number-expr))
  (declare (ignore fn))
  nil)

(defmethod mapobject* (fn (ex record-expr))
  (mapobject* fn (assignments ex)))

(defmethod mapobject* (fn (ex tuple-expr))
  (mapobject* fn (exprs ex)))

;(defmethod mapobject* (fn (ex intype))
;  (mapobject* fn (expression ex))
;  (mapobject* fn (type-value ex))
;  (mapobject* fn (declared-type ex)))

;(defmethod mapobject* (fn (ex coercion))
;  (mapobject* fn (expression ex))
;  ;;(mapobject* fn (type-value ex))
;  (mapobject* fn (declared-type ex)))

(defmethod mapobject* (fn (ex cases-expr))
  (mapobject* fn (expression ex))
  (mapobject* fn (selections ex))
  (mapobject* fn (else-part ex)))

(defmethod mapobject* (fn (ex projection-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex injection-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex injection?-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex extraction-expr))
  (declare (ignore fn obj))
  nil)

(defmethod mapobject* (fn (ex projection-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex injection-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex injection?-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex extraction-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex field-application))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex application))
  (mapobject* fn (operator ex))
  (mapobject* fn (argument ex)))

(defmethod mapobject* (fn (ex infix-application))
  (cond (*mapobject-infix*
	 (mapobject* fn (args1 ex))
	 (mapobject* fn (operator ex))
	 (mapobject* fn (args2 ex)))
	(t (call-next-method))))

(defmethod mapobject* (fn (ex binding-expr))
  (mapobject* fn (bindings ex))
  (mapobject* fn (expression ex)))

(defmethod mapobject* (fn (ex update-expr))
  (mapobject* fn (expression ex))
  (mapobject* fn (assignments ex)))

(defmethod mapobject* (fn (ex assignment))
  (mapobject* fn (arguments ex))
  (mapobject* fn (expression ex)))

(defmethod mapobject* (fn (ex selection))
  (mapobject* fn (constructor ex))
  (mapobject* fn (args ex))
  (mapobject* fn (expression ex)))

(defmethod mapobject* (fn (ex lambda-conversion))
  (if *parsing-or-unparsing*
      (mapobject* fn (expression ex))
      (call-next-method)))

(defmethod mapobject* (fn (ex argument-conversion))
  (if *parsing-or-unparsing*
      (mapobject* fn (operator ex))
      (call-next-method)))

;;; Misc

(defmethod mapobject* (fn (obj simple-decl))
  (mapobject* fn (declared-type obj))
  (unless *parsing-or-unparsing*
    (mapobject* fn (type obj)))
  (when (next-method-p) (call-next-method)))

;(defmethod mapobject* (fn (te dep-binding))
;  (declare (ignore fn))
;  (call-next-method))

(defmethod mapobject* (fn (obj bind-decl))
  (with-slots (actuals dactuals) obj
    (call-next-method)			; binding
    (when actuals (mapobject* fn actuals))
    (when dactuals (mapobject* fn dactuals))
    ;;(mapobject* fn (resolutions obj))
    ))

(defmethod mapobject* (fn (obj name))
  (with-slots (actuals dactuals mappings) obj
    (when (next-method-p) (call-next-method)) ; Handles expr part of name-expr
    (when actuals (mapobject* fn actuals))
    (when dactuals (mapobject* fn dactuals))
    (when mappings (mapobject* fn mappings))))

(defmethod mapobject* (fn (act actual))
  (if (and (not *parsing-or-unparsing*)
	   (type-value act))
      (mapobject* fn (type-value act))
      (mapobject* fn (expr act))))

(defmethod mapobject* (fn (map mapping))
  (mapobject* fn (lhs map))
  (mapobject* fn (rhs map)))

(defmethod mapobject* (fn obj)
  (declare (ignore fn obj))
  nil)


;;; Copy-untyped copies untyped expressions.

(defmethod copy-untyped* ((ex type-expr))
  (pc-parse (unparse ex :string t) 'type-expr))

(defun copy-untyped (expr)
  (copy-untyped* expr))

(defmethod copy-untyped* ((list list))
  (when list
    (cons (copy-untyped* (car list))
	  (copy-untyped* (cdr list)))))

(defmethod copy-untyped* ((ex name-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex field-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex adt-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex null-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex recognizer-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex accessor-name-expr))
  (change-class (call-next-method) 'name-expr))

(defmethod copy-untyped* ((ex name))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'resolutions nil)))

(defmethod copy-untyped* ((ex bind-decl))
  (with-slots (declared-type) ex
    (copy ex
      'type nil
      'declared-type (copy-untyped* declared-type)
      'resolutions nil)))

(defmethod copy-untyped* ((ex rational-expr))
  (copy ex
    'type nil))

(defmethod copy-untyped* ((ex tuple-expr))
  (with-slots (exprs) ex
    (copy ex
      'type nil
      'exprs (copy-untyped* exprs))))

(defmethod copy-untyped* ((ex record-expr))
  (with-slots (assignments) ex
    (copy ex
      'type nil
      'assignments (copy-untyped* assignments))))

(defmethod copy-untyped* ((ex cases-expr))
  (with-slots (expression selections else-part) ex
    (copy ex
      'type nil
      'expression (copy-untyped* expression)
      'selections (copy-untyped* selections)
      'else-part (copy-untyped* else-part))))

(defmethod copy-untyped* ((ex selection))
  (with-slots (constructor args expression) ex
    (copy ex
      'constructor (copy-untyped* constructor)
      'args (copy-untyped* args)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((ex projection-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'type nil)))

(defmethod copy-untyped* ((ex injection-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'type nil)))

(defmethod copy-untyped* ((ex injection?-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'type nil)))

(defmethod copy-untyped* ((ex extraction-expr))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'type nil)))

(defmethod copy-untyped* ((ex projection-application))
  (with-slots (actuals argument) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex injection-application))
  (with-slots (actuals argument) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex injection?-application))
  (with-slots (actuals argument) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex extraction-application))
  (with-slots (actuals argument) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex field-application))
  (with-slots (id argument) ex
    (make-instance 'application
      :place (place ex)
      :parens (parens ex)
      :operator (make-instance 'name-expr :id id)
      :argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex fieldappl))
  (with-slots (id actuals argument) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex fieldex))
  (with-slots (id actuals dactuals) ex
    (copy ex
      'type nil
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals))))

(defmethod copy-untyped* ((ex application))
  (with-slots (operator argument) ex
    (copy ex
      'type nil
      'operator (copy-untyped* operator)
      'argument (copy-untyped* argument))))

(defmethod copy-untyped* ((ex binding-expr))
  (with-slots (bindings expression) ex
    (copy ex
      'type nil
      'bindings (copy-untyped* bindings)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((ex update-expr))
  (with-slots (expression assignments) ex
    (copy ex
      'type nil
      'expression (copy-untyped* expression)
      'assignments (copy-untyped* assignments))))

(defmethod copy-untyped* ((ex assignment))
  (with-slots (arguments expression) ex
    (copy ex
      'arguments (copy-untyped* arguments)
      'expression (copy-untyped* expression))))

(defmethod copy-untyped* ((act actual))
  (with-slots (expr type-value) act
    (let* ((nex (copy-untyped* expr))
	   (cex (typecase expr
		  (print-type-name (change-class nex 'type-name))
		  (print-type-application (change-class nex 'type-application))
		  (print-expr-as-type (change-class nex 'expr-as-type))
		  (t nex))))
      (copy act
	'expr cex
	'type-value (unless expr (copy-untyped* type-value))))))

(defmethod copy-untyped* ((ex table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (make-instance 'table-expr
      :row-expr (copy-untyped* row-expr)
      :col-expr (copy-untyped* col-expr)
      :row-headings (copy-untyped* row-headings)
      :col-headings (copy-untyped* col-headings)
      :table-entries (copy-untyped* table-entries))))

(defmethod copy-untyped* ((ex let-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (make-instance 'table-expr
      :row-expr (copy-untyped* row-expr)
      :col-expr (copy-untyped* col-expr)
      :row-headings (copy-untyped* row-headings)
      :col-headings (copy-untyped* col-headings)
      :table-entries (copy-untyped* table-entries))))

(defmethod copy-untyped* ((ex argument-conversion))
  (with-slots (operator) ex
    (copy-untyped* operator)))

(defmethod copy-untyped* ((ex lambda-conversion))
  (with-slots (expression) ex
    (copy-untyped* expression)))

(defmethod copy-untyped* ((ex implicit-conversion))
  (with-slots (argument) ex
    (copy-untyped* argument)))

(defmethod copy-untyped* :around ((ex type-expr))
  (if (print-type ex)
      (copy-untyped* (print-type ex))
      (call-next-method)))

(defmethod copy-untyped* ((te print-type-name))
  (change-class (copy te) 'type-name
    :actuals (copy-untyped* (actuals te))
    :dactuals (copy-untyped* (dactuals te))
    :resolutions nil))

(defmethod copy-untyped* ((te print-type-application))
  (change-class (copy te
		  :type (copy-untyped* (type te))
		  :parameters (copy-untyped* (parameters te)))
      'type-application))

(defmethod copy-untyped* ((te print-expr-as-type))
  (change-class (copy te :expr (copy-untyped* (expr te)))
      'expr-as-type))

(defmethod copy-untyped* ((ex type-application))
  (with-slots (type parameters) ex
    (copy ex
      'type (copy-untyped* type)
      'parameters (copy-untyped* parameters)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex subtype))
  (with-slots (supertype predicate) ex
    (copy ex
      'supertype (copy-untyped* supertype)
      'predicate (copy-untyped* predicate)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex setsubtype))
  (with-slots (supertype predicate formals formula) ex
    (copy ex
      'supertype nil
      'predicate nil
      'formals (copy-untyped* formals)
      'formula (copy-untyped* formula)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex expr-as-type))
  (with-slots (supertype predicate expr) ex
    (copy ex
      'supertype (copy-untyped* supertype)
      'predicate (copy-untyped* predicate)
      'expr (copy-untyped* expr)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex funtype))
  (with-slots (domain range) ex
    (copy ex
      'domain (copy-untyped* domain)
      'range (copy-untyped* range)
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex tupletype))
  (with-slots (types) ex
    (copy ex
      'types (copy-untyped* types)
      'generated? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex cotupletype))
  (with-slots (types) ex
    (copy ex
      'types (copy-untyped* types)
      'generated? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex recordtype))
  (with-slots (fields) ex
    (copy ex
      'fields (copy-untyped* fields)
      'dependent? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex type-name))
  (with-slots (actuals dactuals) ex
    (copy ex
      'actuals (copy-untyped* actuals)
      'dactuals (copy-untyped* dactuals)
      'resolutions nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex field-decl))
  (with-slots (declared-type) ex
    (copy ex
      'declared-type (copy-untyped* declared-type)
      'type nil
      'resolutions nil)))

(defmethod copy-untyped* ((ex struct-sub-recordtype))
  (with-slots (fields) ex
    (copy ex
      'fields (copy-untyped* fields)
      'dependent? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))

(defmethod copy-untyped* ((ex struct-sub-tupletype))
  (with-slots (types) ex
    (copy ex
      'fields (copy-untyped* types)
      'dependent? nil
      'print-type nil
      'from-conversion nil
      'nonempty? nil)))
