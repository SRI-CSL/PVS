;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Feb 26 21:41:39 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:07:41 1998
;; Update Count    : 15
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

;;; The entry point is compare, which compares two theories and returns two
;;; values: an indication of whether they compare and an association list
;;; which indicates the differences found.  The possible association list
;;; entries are:
;;;    (oexporting nexporting difference)
;;;    (ousing nusing difference)
;;;    (oconstructor nconstructor difference)
;;;    (odecl ndecl difference)
;;; where
;;;   difference - added, deleted, reordered, changed, signature, body

;;; compare compares two theories, generally calling compare-decl-lists on the
;;; formals, etc.  These set the variable *differences* according to the
;;; differences found.

(in-package :pvs)

(defvar *differences*)
(defvar *needs-retypechecking* nil)
(defvar *compare-selections* nil)

(defun compare (old new)
  (assert (and (typep old 'datatype-or-module)
	       (typep new 'datatype-or-module)))
  (compare-top old new))

(defmethod compare-top :around ((old datatype-or-module) (new datatype-or-module))
  (or (compare-decl-lists (formals old) (formals new))
      (compare-decl-lists (assuming old) (assuming new))
      (call-next-method)))

(defmethod compare-top ((old module) (new module))
  (assert (and (exporting old) (exporting new)))
  (or (compare-decl-lists (theory old) (theory new))
      (compare-decl (exporting old) (exporting new))))

(defmethod compare-top ((old datatype) (new codatatype))
  (cons old new))

(defmethod compare-top ((old codatatype) (new datatype))
  (cons old new))

(defmethod compare-top ((old recursive-type) (new recursive-type))
  (or (if (and (importings old) (importings new))
	  (when (compare-decl-lists (importings old) (importings new))
	    (cons old new))
	  (when (or (importings old) (importings new))
	    (cons old new)))
      (unless (compare-constructors (constructors old) (constructors new))
	(cons old new))))

(defmethod compare-top ((old module) (new recursive-type))
  old)

(defmethod compare-top ((old recursive-type) (new module))
  old)

;;; Declarations

;;; compare-decl-lists is called on the top-level declarations lists.  and
;;; simply returns the first old declaration that is no longer valid -
;;; because the new one is substantially different, the old one has been
;;; deleted, or the new one has been inserted before the old one.  Note that
;;; nil means we ran off the end - there may be new decls after that still
;;; need typechecking.

(defun compare-decl-lists (olist nlist)
  ;; First remove generated declarations from consideration
  (let ((rolist (remove-if #'(lambda (d)
			       (and (typep d 'declaration)
				    (generated-by d)))
		  olist)))
    (compare-decls rolist nlist)))

;;; Return nil if they compare (still may have whitespace issues).
;;; Otherwise returns (odecl . ndecl) pair, which corresponds to the firs
;;; mismatch between old decls and new decls.  Either may be nil, indicating
;;; addition or deletion at the end.
(defun compare-decls (olist nlist)
  (if (null olist)
      (unless (null nlist) (cons nil (car nlist)))
      (if (null nlist)
	  (cons (car olist) nil)
	  (if (compare-decl (car olist) (car nlist))
	      (compare-decls (cdr olist) (cdr nlist))
	      (cons (car olist) (car nlist))))))

;;; Top level declarations

(defmethod compare-decl ((old inline-datatype) (new inline-codatatype))
  nil)

(defmethod compare-decl ((old inline-codatatype) (new inline-datatype))
  nil)

(defmethod compare-decl ((old inline-recursive-type) (new inline-recursive-type))
  (when (eq (id old) (id new))
    (and (compare-importings (importings old) (importings new))
	 (compare-constructors (constructors old) (constructors new)))))

(defun compare-importings (oimps nimps)
  (or (and (null oimps) (null nimps))
      (and oimps nimps
	   (compare* (importings old) (importings new)))))

(defun compare-constructors (ocstrs ncstrs)
  (and (= (length ocstrs) (length ncstrs))
       (every #'compare* ocstrs ncstrs)))

(defmethod compare-decl :around ((old enumtype) (new enumtype))
  (compare-decl-lists (constructors old) (constructors new)))

(defmethod compare-decl ((old exporting) (new exporting))
  (let* ((same? (and (eq (kind old) (kind new))
		     (if (and (listp (names old)) (listp (names new)))
			 (compare* (names old) (names new))
			 (eq (names old) (names new)))
		     (compare* (but-names old) (but-names new))
		     (compare* (modules old) (modules new)))))
    (unless same?
      (break "Exporting differences"))))

(defmethod compare-decl ((old importing) (new importing))
  (compare* (theory-name old) (theory-name new)))

;;; Compare* on declarations returns NIL if the ids or classes don't
;;; match, otherwise returns a list of the form (odecl ndecl level)
;;; where level is BODY or SIGNATURE.  LEXICAL differences will be
;;; handled during update, since it simply copies the new stuff over.

(defmethod compare-decl :around ((old declaration) (new declaration))
  (when (and (eq (id old) (id new))
	     (eq (type-of old) (type-of new)))
    (and (compare* (formals old) (formals new))
	 (call-next-method))))

(defmethod compare-decl ((old typed-declaration) (new typed-declaration))
  (compare* (declared-type old) (declared-type new)))

;(defmethod compare-decl ((old formal-decl) (new formal-decl))
;  (call-next-method))

(defmethod compare-decl ((old mod-decl) (new mod-decl))
  (and (call-next-method)
       (compare* (modname old) (modname new))))

(defmethod compare-decl ((old theory-abbreviation-decl)
			 (new theory-abbreviation-decl))
  (and (call-next-method)
       (compare* (theory-name old) (theory-name new))))

;(defmethod compare-decl ((old type-decl) (new type-decl))
;  (call-next-method))

(defmethod compare-decl ((old type-def-decl) (new type-def-decl))
  (and ;;(call-next-method)
       (compare* (contains old) (contains new))
       (compare* (type-expr old) (type-expr new))))

;(defmethod compare-decl ((old var-decl) (new var-decl))
;  (call-next-method))

(defmethod compare-decl ((old const-decl) (new const-decl))
  (and (call-next-method)
       (compare* (definition old) (definition new))))

(defmethod compare-decl ((old def-decl) (new def-decl))
  (and (call-next-method)
       (compare* (definition old) (definition new))
       (compare* (declared-measure old) (declared-measure new))
       (compare* (ordering old) (ordering new))))

(defmethod compare-decl ((old formula-decl) (new formula-decl))
  (and ;;(call-next-method)
       (or (eq (spelling old) (spelling new))
	   (and (memq (spelling old) '(AXIOM POSTULATE))
		(memq (spelling new) '(AXIOM POSTULATE))))
       (compare* (kind old) (kind new))
       (compare* (definition old) (definition new))))

(defmethod compare-decl ((old subtype-judgement) (new subtype-judgement))
  (and ;;(call-next-method)
       (compare* (declared-subtype old) (declared-subtype new))))

(defmethod compare-decl ((old number-judgement) (new number-judgement))
  (and (call-next-method)
       (compare* (number-expr old) (number-expr new))))

(defmethod compare-decl ((old name-judgement) (new name-judgement))
  (and (call-next-method)
       (compare* (name old) (name new))))

(defmethod compare-decl ((old application-judgement) (new application-judgement))
  (and (call-next-method)
       (compare* (name old) (name new))
       (compare* (formals old) (formals new))))

(defmethod compare-decl ((old conversion-decl) (new conversion-decl))
  (and ;;(call-next-method)
       (typecase old
	 (conversionminus-decl (conversionminus-decl? new))
	 (conversion-decl (not (conversionminus-decl? new))))
       (compare* (expr old) (expr new))))

(defmethod compare-decl ((old auto-rewrite-decl) (new auto-rewrite-decl))
  (and (call-next-method)
       (typecase old
	 (auto-rewrite-minus-decl (auto-rewrite-minus-decl? new))
	 (auto-rewrite-decl (not (or (auto-rewrite-minus-decl? new)))))
       (compare* (rewrite-names old) (rewrite-names new))))

(defmethod compare-decl ((old rewrite-name) (new rewrite-name))
  (and (call-next-method)
       (eq (class-of old) (class-of new))))

(defmethod compare-decl (old new)
  (equal old new))


;;; Below the declaration level, compare* simply returns t or nil
;;; according to whether the entities match

(defmethod compare* ((old simple-constructor) (new simple-constructor))
  (and (same-id old new)
       (= (length (arguments old)) (length (arguments new)))
       (every #'compare* (arguments old) (arguments new))
       (compare* (recognizer old) (recognizer new))))

(defmethod compare* ((old adtdecl) (new adtdecl))
  (and (compare* (id old) (id new))
       (compare* (declared-type old) (declared-type new))))

;; (defmethod compare* ((old field-decl) (new field-decl))
;;   (call-next-method))


;;; Type expressions

;(defmethod compare* ((old type-expr) (new type-expr))
;  nil)

;;; This isn't handled by the name method, since type-expr comes first in
;;; the class hierarchy.

(defmethod compare* ((old type-name) (new type-name))
  (and (compare* (mod-id old) (mod-id new))
       (compare* (library old) (library new))
       (and (or (and (null (actuals old)) (null (actuals new)))
		(and (actuals old) (actuals new)))
	    (compare* (actuals old) (actuals new)))
       (and (or (and (null (mappings old)) (null (mappings new)))
		(and (mappings old) (mappings new)))
	    (compare* (mappings old) (mappings new)))
       (compare* (id old) (id new))))

(defmethod compare* ((old type-application) (new type-application))
  (and (compare* (type old) (type new))
       (compare* (parameters old) (parameters new))))

(defmethod compare* ((old subtype) (new subtype))
  (and (eq (class-of old) (class-of new))
       (compare* (predicate old) (predicate new))))

(defmethod compare* ((old setsubtype) (new setsubtype))
  (and (compare* (supertype old) (supertype new))
       (compare* (formals old) (formals new))
       (compare* (formula old) (formula new))))

(defmethod compare* ((old nsetsubtype) (new nsetsubtype))
  (and ;;(compare* (contains old) (contains new))
       (compare* (formals old) (formals new))
       (compare* (formula old) (formula new))))

(defmethod compare* ((old expr-as-type) (new expr-as-type))
  (and (call-next-method)
       (compare* (expr old) (expr new))))

(defmethod compare* ((old funtype) (new funtype))
  (and (compare* (domain old) (domain new))
       (compare* (range old) (range new))))

(defmethod compare* ((old tupletype) (new tupletype))
  (compare* (types old) (types new)))

(defmethod compare* ((old cotupletype) (new cotupletype))
  (compare* (types old) (types new)))

(defmethod compare* ((old recordtype) (new recordtype))
  (compare* (fields old) (fields new)))


;;; Expressions

(defmethod compare* :around ((old expr) (new expr))
  (if (from-macro old)
      (compare* (from-macro old) new)
      (call-next-method)))

(defmethod compare* ((old number-expr) (new number-expr))
  (compare* (number old) (number new)))

(defmethod compare* ((old tuple-expr) (new tuple-expr))
  (compare* (exprs old) (exprs new)))

(defmethod compare* ((old record-expr) (new record-expr))
  (compare* (assignments old) (assignments new)))

(defmethod compare* ((old cases-expr) (new cases-expr))
  (and (compare* (expression old) (expression new))
       (compare* (selections old) (selections new))
       (compare* (else-part old) (else-part new))))

(defmethod compare* ((old selection) (new selection))
  (and (compare* (constructor old) (constructor new))
       (let ((*compare-selections* t))
	 (compare* (args old) (args new)))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old projection-expr) (new projection-expr))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))))

(defmethod compare* ((old injection-expr) (new injection-expr))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))))

(defmethod compare* ((old injection?-expr) (new injection?-expr))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))))

(defmethod compare* ((old extraction-expr) (new extraction-expr))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))))


(defmethod compare* ((old projection-application) (new projection-application))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old injection-application) (new injection-application))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old injection?-application) (new injection?-application))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old extraction-application) (new extraction-application))
  (and (compare* (id old) (id new))
       (compare* (actuals old) (actuals new))
       (compare* (index old) (index new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old field-application) (new field-application))
  (and (compare* (id old) (id new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old field-application) (new application))
  (and (typep (operator new) 'name-expr)
       (not (mod-id (operator new)))
       (not (library (operator new)))
       (not (actuals (operator new)))
       (not (mappings (operator new)))
       (not (target (operator new)))
       (eq (id old) (id (operator new)))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old application) (new application))
  (and (compare* (operator old) (operator new))
       (compare* (arguments old) (arguments new))))

(defmethod compare* ((old when-expr) (new application))
  (and (typep (argument new) 'arg-tuple-expr)
       (= (length (exprs (argument new))) 2)
       (compare* (reverse (exprs (argument old))) (exprs (argument new)))))

(defmethod compare* ((old coercion) (new coercion))
  (compare* (argument old) (argument new)))

(defmethod compare* ((old table-expr) (new table-expr))
  (and (compare* (row-expr old) (row-expr new))
       (compare* (col-expr old) (col-expr new))
       (compare* (row-headings old) (row-headings new))
       (compare* (col-headings old) (col-headings new))
       (compare* (table-entries old) (table-entries new))))

(defmethod compare* ((old implicit-conversion) (new expr))
  (compare* (args1 old) new))

(defmethod compare* ((old argument-conversion) (new expr))
  (compare* (operator old) new))

(defmethod compare* ((old lambda-conversion) (new expr))
  (compare* (expression old) new))

(defmethod compare* ((old binding-expr) (new binding-expr))
  (and (same-binding-op? old new)
       (compare* (bindings old) (bindings new))
       (compare* (expression old) (expression new))))

;(defmethod compare* ((old quant-expr) (new quant-expr))
;  (call-next-method))

(defmethod compare* ((old update-expr) (new update-expr))
  (and (compare* (expression old) (expression new))
       (compare* (assignments old) (assignments new))))

(defmethod compare* ((old assignment) (new assignment))
  (and (compare* (arguments old) (arguments new))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old maplet) (new maplet))
  (and (compare* (arguments old) (arguments new))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old assignment) (new maplet))
  nil)

(defmethod compare* ((old maplet) (new assignment))
  nil)


;;; Names and simple-decls

(defmethod compare* ((old simple-decl) (new simple-decl))
  (and (compare* (id old) (id new))
       (or (compare* (declared-type old) (declared-type new))
	   (and *compare-selections*
		(null (declared-type new))))))

(defmethod compare* ((old bind-decl) (new bind-decl))
  (and (compare* (id old) (id new))
       (or (null (declared-type new))
	   (compare* (declared-type old) (declared-type new)))))

(defmethod compare* ((old untyped-bind-decl) (new untyped-bind-decl))
  (compare* (id old) (id new)))

;(defmethod compare* ((old modname) (new modname))
;  (and (call-next-method)
;       (compare (renamings old) (renamings new))))

(defmethod compare* ((old name) (new name))
  (and (compare* (mod-id old) (mod-id new))
       (compare* (library old) (library new))
       (and (or (and (null (actuals old)) (null (actuals new)))
		(and (actuals old) (actuals new)))
	    (compare* (actuals old) (actuals new)))
       (and (or (and (null (mappings old)) (null (mappings new)))
		(and (mappings old) (mappings new)))
	    (compare* (mappings old) (mappings new)))
       (and (or (and (null (target old)) (null (target new)))
		(and (target old) (target new)))
	    (compare* (target old) (target new)))
       (compare* (id old) (id new))))

(defmethod compare* ((old expr) (new expr))
  nil)

(defmethod compare* ((old type-expr) (new type-expr))
  nil)

(defmethod compare* ((old actual) (new actual))
  (and (or (and (zerop (parens (expr old)))
		(zerop (parens (expr new))))
	   (and (not (zerop (parens (expr old))))
		(not (zerop (parens (expr new))))))
       (compare* (expr old) (expr new))))

(defmethod compare* ((old mapping) (new mapping))
  (and (eq (class-of old) (class-of new))
       (compare* (lhs old) (lhs new))
       (compare* (rhs old) (rhs new))))

(defmethod compare* ((old mapping-rhs) (new mapping-rhs))
  (and (or (and (zerop (parens (expr old)))
		(zerop (parens (expr new))))
	   (and (not (zerop (parens (expr old))))
		(not (zerop (parens (expr new))))))
       (compare* (expr old) (expr new))))

(defmethod compare* ((old list) (new list))
  (and (length= old new)
       (every #'(lambda (oe ne) (compare* oe ne))
	      old new)))

#-gcl
(defmethod compare* ((old term::default-term) (new term::default-term))
  (equal (term-to-sexp old) (term-to-sexp new)))

(defmethod compare* (old new)
  (equal old new))

(defmethod compare* ((old syntax) (new syntax))
  (eq (class-of old) (class-of new)))

(defun add-decl-diffs (old new)
  (when *decl-diffs*
    (push (list old new
		(or (find 'signature *decl-diffs*)
		    (find 'body *decl-diffs*)))
	  *differences*)))
