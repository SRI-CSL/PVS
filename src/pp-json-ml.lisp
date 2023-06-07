;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-json-ml.lisp -- The PVS prettyprinter
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 23:19:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 18 20:56:23 2012
;; Update Count    : 10
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

;;PVS pretty-printer for machine learning

(in-package :pvs)

(defvar *pp-json-ml-type-hash* nil "maintains the cached extraction for types")
(defvar *pp-type-json-ml-hash* nil "not really used, but useful for testing")
(defvar *pp-json-ml-term-hash* nil "maintains the cached extraction for terms")
(defvar *pp-json-ml-bindings*)

(defstruct (pp-json-ml-type-entry (:conc-name jsonte-))
  typehash texpression)

;;We can encode Booleans as B and numbers as N, and then subtypes of a type T as T/,
;;tuple types as Uk....., where k is the arity, and record types as Rk where k is the arity.
;;These types can then be encoded by a vector consisting of the supertype structure and the
;;first weights of the first k terms.   Let's initially use vectors of size 5, with possible expansion to 20. 
;;This way all subtypes at the same level will be identical.
;;For terms, we generate a sequence of tokens of operators and their arities.
;;For variables, we generate the type/id pair.  

;;;Declarations

(defun pp-json-ml-decl (decl)
  (let ((*current-context* (decl-context decl))
	(*pp-json-ml-type-hash* (or *pp-json-ml-type-hash* (make-pvs-hash-table)))
	(*pp-type-json-ml-hash* (or *pp-type-json-ml-hash* (make-hash-table :test 'string=))))
    (pp-json-ml-decl* decl)))

(defgeneric pp-json-ml-decl* (x))

(defgeneric pp-json-ml* (x))

;; (defmethod pp-json-ml-decl* :around ((decl syntax))
;;   (let ((jdecl-alist (call-next-method)))
;;     (if (or (null (place decl))
;; 	    (assoc "place" jdecl-alist :test #'string=))
;; 	jdecl-alist
;; 	(nconc jdecl-alist `(("place" . ,(place decl)))))))

(defvar *pp-json-decl* nil)

(defmethod pp-json-ml-decl* ((decls list))
  (when decls
    (cons (let ((*pp-json-decl* (car decls)))
	    (pp-json-ml-decl* (car decls)))
	  (pp-json-ml-decl* (cdr decls)))))

(defmethod pp-json-ml-decl* ((imp importing))
  `(("tag" . "importing")
    ("theory-name" . (pp-json-ml* (theory-name imp)))))

(defmethod pp-json-ml-decl* ((decl type-eq-decl))
  ;;name, kind, parameters, definition
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
  `(("tag" . "type-eq-decl")
    ("id" . ,(string (id decl)))
    ("theory" . ,(string (id (module decl))))
    ,@(when lib `(("library" . ,lib)))
    ,@(when path `(("path" . ,path)))
    ("type" ,(pp-json-ml* (type-value decl))))))

(defmethod pp-json-ml-decl* ((decl enumtype))
  (with-slots (id constructors) decl
    (let* ((th (module decl))
	   (lib (pp-json-library decl))
	   (path (unless lib (pp-json-path th))))
      `(("tag" . "enumtype")
	("id" . ,(string id))
	("theory" . ,(string (id (module decl))))
	,@(when lib `(("library" . ,lib)))
	,@(when path `(("path" . ,path)))
	("elements" . ,(mapcar #'id constructors))))))

;; (defun pp-json-ml-decl-id (decl)
;;   (let* ((path (context-path (module decl)))
;; 	 (lib (if (from-prelude? decl)
;; 		  "prelude"
;; 		  (car (rassoc path (pvs-library-alist) :test #'equal)))))
;;     (if lib
;; 	(format nil "~a@~a.~a" lib (id (module decl)) (id decl))
;; 	(format nil "~a#~a.~a" path (id (module decl)) (id decl)))))

(defmethod pp-json-ml-decl* ((decl type-decl))
  ;;name, kind, parameters, definition
  (unless (and (generated-by decl)
	       (adt-type-name? (type-value decl)))
    (let* ((th (module decl))
	   (lib (pp-json-library decl))
	   (path (unless lib (pp-json-path th))))
      `(("tag" . "type-decl")
	("id" . ,(string (id decl)))
	("theory" . ,(string (id th)))
	,@(when lib `(("library" . ,lib)))
	,@(when path `(("path" . ,path)))))))

(defmethod pp-json-ml-decl* ((decl type-def-decl))
  ;;name, kind, parameters, definition
  (if (eq (class-of decl) (find-class 'type-def-decl))
      (let ((type-alist (call-next-method)))
	(nconc type-alist
	       `(("type-definition" ,(pp-json-ml* (type-expr decl))))))
      (call-next-method)))

(defmethod pp-json-ml-decl* ((decl type-from-decl))
  (let ((type-alist (call-next-method)))
    (nconc type-alist
	   `(("supertype" ,(pp-json-ml* (supertype decl)))
	     ("predicate" . (pp-json-ml* (predicate decl)))))))

(defmethod pp-json-ml-decl* ((decl struct-subtype-decl))
  (let ((type-alist (call-next-method)))
    (nconc type-alist
	   `(("supertype" ,(pp-json-ml* (supertype decl)))
	     ("projection" . (pp-json-ml* (projection decl)))))))

(defmethod pp-json-ml-decl* ((decl formal-type-decl))
  ;;name, kind, parameters, definition
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "formal-type-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path))))))

(defmethod pp-json-ml-decl* ((decl formal-subtype-decl))
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "formal-subtype-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path)))
      ("supertype" ,(pp-json-ml* (supertype decl)))
      ("predicate" . (pp-json-ml* (predicate decl))))))

(defmethod pp-json-ml-decl* ((decl formal-struct-subtype-decl))
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "formal-struct-subtype-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path)))
      ("supertype" ,(pp-json-ml* (supertype decl)))
      ("projection" . (pp-json-ml* (predicate decl))))))

(defmethod pp-json-ml-decl* ((decl formal-const-decl))
  ;;name, kind, parameters, definition
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "formal-const-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path)))
      ("type" ,(pp-json-ml* (type decl))))))

(defmethod pp-json-ml-decl* ((decl formal-theory-decl))
  ;; theory-name
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "formal-theory-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path)))
      ("theory-name" . ,(pp-json-ml* (theory-name decl))))))

(defmethod pp-json-ml-decl* ((decl theory-abbreviation-decl))
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "theory-abbreviation-decl")
      ("id" . ,(string (id decl)))
      ("theory" . ,(string (id th)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path)))
      ("theory-name" . ,(pp-json-ml* (theory-name decl))))))

(defmethod pp-json-ml-decl* ((decl const-decl))
  ;;name, kind, parameters, definition
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th)))
	 (cdecl `(("tag" . "const-decl")
		  ("id" . ,(string (id decl)))
		  ("theory" . ,(string (id th)))
		  ,@(when lib `(("library" . ,lib)))
		  ,@(when path `(("path" . ,path)))
		  ("type" ,(pp-json-ml* (type decl)))
		  ("parameters" . ,(map 'vector #'(lambda (params)
						    (map 'vector #'pp-json-ml* params))
					(formals decl)))
		  ,@(when (decl-formals decl)
		      `(("decl-formals" . ,(map 'vector #'pp-json-ml* (decl-formals decl)))))
		  ,@(when (definition decl)
		      `(("const-def" . ,(pp-json-ml* (definition decl))))))))
    cdecl))

(defmethod pp-json-ml-decl* ((decl def-decl))
  ;;name, kind, parameters, definition
  (let* ((th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th)))
	 (cdecl `(("tag" . "recursive-func-decl")
		  ("id" . ,(string (id decl)))
		  ("theory" . ,(string (id th)))
		  ,@(when lib `(("library" . ,lib)))
		  ,@(when path `(("path" . ,path)))
		  ("type" ,(pp-json-ml* (type decl)))
		  ("parameters" . ,(map 'vector #'(lambda (params)
						    (map 'vector #'pp-json-ml* params))
					(formals decl)))
		  ,@(when (decl-formals decl)
		      `(("decl-formals" . ,(map 'vector #'pp-json-ml* (decl-formals decl)))))
		  ("recursive-def" . ,(pp-json-ml* (definition decl)))
		  ("measure" . ,(pp-json-ml* (measure decl)))
		  ,@(when (ordering decl)
		      `(("ordering" . ,(pp-json-ml* (ordering decl))))))))
    cdecl))

(defmethod pp-json-ml-decl* ((decl var-decl))
  (with-slots (id declared-type type) decl
    `(("tag" . "var-decl")
      ("id" . ,(string (id decl)))
      ;;("declared-type" ,(pp-json-ml* declared-type))
      ("type" ,(pp-json-ml* type)))))

(defmethod pp-json-ml-decl* ((decl formula-decl))
  (with-slots (id spelling closed-definition default-proof) decl ;just the default proof for now
    (let* ((prf (pp-json-ml* default-proof))
	   (fdalist `(("tag" . "formula-decl")
		      ("id" . ,(string id))
		      ("label" . ,spelling)
		      ,@(when (decl-formals decl)
			  `(("decl-formals" . ,(map 'vector #'pp-json-ml*
						    (decl-formals decl)))))
		      ("definition" . ,(pp-json-ml* closed-definition))
		      ("proof" . ,prf))))
      fdalist)))

(defmethod pp-json-ml-decl* ((decl subtype-judgement))
  (with-slots (id declared-type type declared-subtype subtype) decl
    (when (freevars subtype)
      (setq *pp-json-ml-bindings* (append *pp-json-ml-bindings*
					  (mapcar #'declaration (freevars subtype)))))
    `(("tag" . "subtype-judgement")
      ,@(when id `(("id" . ,(string id))))
      ;;("declared-type" ,(pp-json-ml* declared-type))
      ("type" ,(pp-json-ml* type))
      ;;("declared-subtype" ,(pp-json-ml* declared-subtype))
      ("subtype" ,(pp-json-ml* subtype)))))

(defmethod pp-json-ml-decl* ((decl name-judgement))
  (with-slots (id declared-type type name) decl
    `(("tag" . "name-judgement")
      ("id" . ,(string id))
      ;; ("declared-type" ,(list (pp-json-ml* declared-type)))
      ("type" ,(pp-json-ml* type))
      ("name" . ,(pp-json-ml* name)))))

(defmethod pp-json-ml-decl* ((decl application-judgement))
  (with-slots (id declared-type type name formals judgement-type) decl
    `(("tag" . "application-judgement")
      ("id" . ,(string id))
      ;; ("declared-type" ,(list (pp-json-ml* declared-type)))
      ("type" ,(pp-json-ml* type))
      ("name" . ,(pp-json-ml* name))
      ,@(when formals `(("formals" . ,(pp-json-ml* formals))))
      ("judgement-type" ,(list (pp-json-ml* judgement-type))))))

(defmethod pp-json-ml-decl* ((decl expr-judgement))
  (with-slots (id declared-type type expr formals judgement-type) decl
    `(("tag" . "expr-judgement")
      ("id" . ,(string id))
      ;; ("declared-type" ,(list (pp-json-ml* declared-type)))
      ("type" ,(pp-json-ml* type))
      ("expr" . ,(pp-json-ml* expr))
      ,@(when formals `(("formals" . ,(pp-json-ml* formals))))
      ,@(when judgement-type
	  `(("judgement-type" ,(list (pp-json-ml* judgement-type))))))))

(defmethod pp-json-ml-decl* ((decl conversion-decl))
  (with-slots (id expr) decl
    `(("tag" . "conversion-decl")
      ("id" . ,(string id))
      ("expr" . ,(pp-json-ml* expr)))))

(defmethod pp-json-ml-decl* ((decl auto-rewrite-decl))
  (with-slots (id rewrite-names) decl
    `(("tag" . "auto-rewrite-decl")
      ,@(when id `(("id" . ,(string id))))
      ("rewrite-names" . ,(mapcar #'pp-json-ml* rewrite-names)))))

(defmethod pp-json-ml* ((rname rewrite-name))
  (let* ((decl (declaration rname))
	 (th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "rewrite-name")
      ("id" . ,(string (id decl)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path))) ; Only when library not available
      ("theory" . ,(string (id th)))
      ("actuals" . ,(pp-json-ml* (actuals (module-instance rname)))))))

(defmethod pp-json-ml* ((pinfo proof-info))
  `(("tag" . "proof-info")
    ("script" . ,(format nil "~s" (script pinfo)))
    ("status" . ,(string (status pinfo)))
    ;;(:refers-to . ,(refers-to pinfo))
    ))

(defmethod print-type-hash ()
  (let ((entries nil))
    (when (hash-table-p *pp-json-ml-type-hash*)
      (maphash #'(lambda (key val)
		   (declare (ignore key))
		   (push `(,(cdr (jsonte-typehash val)) . ,(jsonte-texpression val))
			 entries))
	       *pp-json-ml-type-hash*)
      `(("tag" . "typelist")
	("entries" . ,entries)))))

;;; Expressions

(defun pp-json-ml (expr)
  (let* ((*pp-json-ml-type-hash* (or *pp-json-ml-type-hash* (make-pvs-hash-table)))
	 (*pp-type-json-ml-hash* (or *pp-type-json-ml-hash* (make-hash-table :test 'string=)))
	 (result (pp-json-ml* expr)))
    (values result *pp-json-ml-type-hash*)))

;; (defmethod pp-json-ml* :around ((expr syntax))
;;   (let ((jexpr-alist (call-next-method)))
;;     (if (or (null (place expr))
;; 	    (assoc "place" jexpr-alist :test #'string=))
;; 	jexpr-alist
;; 	(nconc jexpr-alist `(("place" . ,(place expr)))))))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defcl int-expr (rational-expr)))

(defmethod pp-json-ml* ((expr int-expr))
  (with-slots (number) expr
    `(("tag" . "integer")
      ("integer-value" . ,number))))

(defmethod pp-json-ml* ((expr number-expr))
  (with-slots (number) expr
    `(("tag" . "integer")
      ("integer-value" . ,number))))

(defmethod pp-json-ml* ((expr string-expr))
  `(("tag" . "string")
    ("string-value" . ,(string-value expr))))

(defun pp-json-library (decl)
  (let* ((th (module decl))
	 (cpath (when th (context-path th)))
	 (lib (when (and cpath
			 (not (file-equal cpath (current-context-path))))
		(if (from-prelude? decl)
		    "pvs:prelude"
		    (library-path-to-id cpath)))))
    (when lib (string lib))))

(defmethod pp-json-ml* ((expr name-expr))
  (if (null (declaration expr))
      (call-next-method)
      (let* ((decl (declaration expr))
	     (tag (typecase decl
		    (formal-const-decl "formal-constant")
		    (const-decl "constant")
		    (t "variable"))))
	(if (string= tag "variable")
	    `(("tag" . ,tag)
	      ("id" . ,(string (id decl)))
	      ("type" ,(pp-json-ml* (type expr))))
	    (let* ((thinst (module-instance expr))
		   (th (or (module decl)
			   (and (resolution thinst)
				(declaration (resolution thinst)))))
		   (thid (when th (id th)))
		   (lib (when th (pp-json-library th)))
		   (path (when (and th (null lib)) (pp-json-path th))))
	      `(("tag" . ,tag)
		("id" . ,(string (id decl)))
		,@(when lib `(("library" . ,lib)))
		,@(when path `(("path" . ,path))) ; Only when library not available
		,@(when th `(("theory" . ,(string thid))))
		,@(when (actuals thinst) `(("actuals" . ,(pp-json-ml* (actuals thinst)))))
		,@(when (dactuals thinst) `(("dactuals" . ,(pp-json-ml* (dactuals thinst)))))
		;; ,@(when (mappings thinst) `(("mappings" . ,(pp-json-ml* (mappings thinst)))))
		("type" ,(pp-json-ml* (type expr)))))))))

(defmethod pp-json-ml* ((act actual))
  (with-slots (expr type-value) act
    (if type-value
	`(("tag" . "type-actual")
	  ("type" ,(pp-json-ml* type-value)))
	`(("tag" . "const-actual")
	  ("expr" . ,(pp-json-ml* expr))))))

(defmethod pp-json-ml* :around ((expr binding-expr))
  (setq *pp-json-ml-bindings* (append *pp-json-ml-bindings* (bindings expr)))
  (call-next-method))

(defmethod pp-json-ml* ((expr lambda-expr))
  (with-slots (bindings expression) expr
    `(("tag" . "lambda")
      ("bindings" . ,(pp-json-ml* bindings))
      ("expression" . ,(pp-json-ml* expression)))))

(defmethod pp-json-ml* ((expr lambda-expr-with-type))
  "Example: 'lambda (x:int) -> nat: f(x)'"
  (with-slots (bindings expression return-type) expr
    `(("tag" . "lambda")
      ("bindings" . ,(pp-json-ml* bindings))
      ;; declared-ret-type
      ("return-type" ,(pp-json-ml* return-type))
      ("expression" . ,(pp-json-ml* expression)))))

(defmethod pp-json-ml* ((expr forall-expr))
  (with-slots (bindings expression) expr
    `(("tag" . "forall")
      ("bindings" . ,(pp-json-ml* bindings))
      ("expression" . ,(pp-json-ml* expression)))))

(defmethod pp-json-ml* ((expr exists-expr))
  (with-slots (bindings expression) expr
    `(("tag" . "exists")
      ("bindings" . ,(pp-json-ml* bindings))
      ("expression" . ,(pp-json-ml* expression)))))

(defmethod pp-json-ml* ((expr binding-expr))
  (with-slots (op bindings expression) expr
    `(("tag" . "bind")
      ("operator" ,op)
      ("bindings" . ,(pp-json-ml* bindings))
      ("expression" . ,(pp-json-ml* expression)))))

(defmethod pp-json-ml* ((expr application))
  (with-slots (operator argument) expr
    `(("tag" . "apply")
      ("operator" . ,(pp-json-ml* operator))
      ("argument" . ,(pp-json-ml* argument)))))

(defmethod pp-json-ml* ((expr list))
  ;; mapcar won't work for cons pairs
  (if (true-listp expr)
      (map 'vector #'pp-json-ml* expr)
      (pp-json-ml-cons expr)))

(defun pp-json-ml-cons (expr)
  (when expr
    (cons (pp-json-ml* (car expr))
	  (pp-json-ml-cons (cdr expr)))))

(defmethod pp-json-ml* ((expr cases-expr))
  (with-slots (expression selections else-part) expr
    `(("tag" . "cases")
      ("expr" . ,(pp-json-ml* expression))
      ("selections" . ,(pp-json-ml-selections selections))
      ("else-part" . ,(pp-json-ml* else-part)))))

(defun pp-json-ml-selections (selections)
  (loop for sel in selections
	collect `(("tag" . "selection")
		  ("pattern" . ("apply" ,(pp-json-ml* (constructor sel)) ,(pp-json-ml* (args sel))))
		  ("expr" . ,(pp-json-ml* (expression sel))))))

(defmethod pp-json-ml* ((expr if-expr))
  (cond ((branch? expr)
	 (if (last-cond-expr? expr)
	     (pp-json-ml* (then-part expr))
	     `(("tag" . "if")
	       ("test" . ,(pp-json-ml* (condition expr)))
	       ("then" . ,(pp-json-ml* (then-part expr)))
	       ("else" . ,(pp-json-ml* (else-part expr))))))
	(t (call-next-method))))

(defmethod pp-json-ml* ((ex let-expr))
  (with-slots (operator) ex
    (let ((let-bindings (bindings operator))
	  (arguments (arguments ex))
	  (expr (expression operator)))
      `(("tag" . "let")
	("bindings" . ,(mapcar #'pp-json-ml-let-binding let-bindings arguments))
	("body" . ,(pp-json-ml* expr))))))

(defun pp-json-ml-let-binding (binding arg)
  `(("tag" . "let-binding")
    ("id" . ,(string (id binding)))
    ;; ("declared-type" ,(list (pp-json-ml* (declared-type binding))))
    ("type" ,(pp-json-ml* (type binding)))
    ("expr" . ,(pp-json-ml* arg))))

(defmethod pp-json-ml* ((expr record-expr))
  (with-slots (assignments) expr
    `(("tag" . "record")
      ("assignments" ,(mapcar #'pp-json-ml* (sort-assignments assignments))))))

(defmethod pp-json-ml* ((assn assignment))
  (let ((alist `(("tag" . "assignment")
		 ("arguments" . ,(pp-json-ml* (arguments assn)))
		 ("expr" . ,(pp-json-ml* (expression assn))))))
    alist))

(defmethod pp-json-ml* ((expr tuple-expr))
  (with-slots (exprs) expr
    `(("tag" . "tuple")
      ("exprs" . ,(map 'vector #'pp-json-ml* exprs)))))

(defmethod pp-json-ml* ((expr projection-application))
  (with-slots (index argument) expr
    `(("tag" . "project")
      ("argument" . ,(pp-json-ml* argument))
      ("index" . ,index))))

(defmethod pp-json-ml* ((expr field-application))
  (with-slots (id argument) expr
    `(("tag" . "getfield")
      ("field" . ,(string id))
      ("argument" . ,(pp-json-ml* argument)))))

(defmethod pp-json-ml* ((expr update-expr))
  (with-slots (type expression assignments) expr
    `(("tag" . "update")
      ("expression" . ,(pp-json-ml* expression))
      ("assignments" . ,(pp-json-ml* assignments)))))

(defmethod pp-json-ml* ((expr field-assignment-arg))
  `(("field" . ,(string (id expr)))))

(defmethod pp-json-ml* ((expr proj-assign))
  `(("projection" . ,(number expr))))

(defmethod pp-json-ml* ((expr accessor-assignment-arg))
  `(("accessor" . ,(string (id expr)))))


;;;Types

(defmethod pp-json-ml*-binding (bdecl)
  `(("tag" . "bind-decl")
    ("variable-name" . ,(string (id bdecl)))
    ("type" ,(pp-json-ml* (type bdecl)))))

(defun mk-pp-json-ml-type-entry (typehash texpression)
  (assert (stringp (cdr typehash)))
  (make-pp-json-ml-type-entry
    :typehash typehash
    :texpression texpression))

(defvar *pp-ml-dont-hash* nil)

(defmethod pp-json-ml* :around ((texpr type-expr))
  (let ((entry (gethash texpr *pp-json-ml-type-hash*)))
    (or (and entry (jsonte-typehash entry))
 	(let ((texpression (call-next-method)))
	  (if *pp-ml-dont-hash*
	      texpression
	      (set-json-ml-hashes texpr texpression))))))

(defmethod pp-json-ml* :around ((ex binding))
  ;;(setq *pp-json-ml-bindings* (nconc *pp-json-ml-bindings* (list ex)))
  (call-next-method))


(defmethod pp-json-ml* :around ((texpr dep-binding))
  (call-next-method))

(defun set-json-ml-hashes (texpr texpression)
  (let* ((print-type (get-print-type texpr))
	 (fexpr (full-name texpr nil nil *pp-json-ml-bindings*))
	 (print-str (str fexpr))
	 (hash (sha1:sha1-hex print-str))
	 (typehash `("typehash" . ,hash)))
    (when (some #'(lambda (fv) (typep fv '(or dep-binding field-decl)))
		(freevars texpr))
      (break "set-json-ml-hashes bad type: ~a" texpr))
    ;; (let ((pexpr (gethash hash *pp-type-json-ml-hash*)))
    ;;   (when pexpr (break "already in *pp-type-json-ml-hash*: ~a" pexpr)))
    (unless (or (null (print-type texpr))
		(eq print-type texpr))
      (let* ((*pp-ml-dont-hash* t)
	     ;;(ptype-ml (pp-json-ml* print-type))
	     (ptype-ml (pp-json-ml* (full-name (print-type texpr)))))
	(nconc texpression `(("print-type" ,ptype-ml)))))
    ;;(json:encode-json texpression)
    (setf (gethash texpr *pp-json-ml-type-hash*)
	  (mk-pp-json-ml-type-entry typehash texpression))
    (setf (gethash hash *pp-type-json-ml-hash*) texpr)
    (let ((mth-count (hash-table-count *pp-json-ml-type-hash*))
	  (tmh-count (hash-table-count *pp-type-json-ml-hash*)))
      (cond ((< mth-count tmh-count) (break "check"))
	    ((> mth-count tmh-count)
	     (maphash #'(lambda (ty struct)
			  (declare (ignore ty))
			  (let ((hash (cdr (jsonte-typehash struct))))
			    (unless (gethash hash *pp-type-json-ml-hash*)
			      (break "Why different?"))))
		      *pp-json-ml-type-hash*))))
    typehash))

(defmethod pp-json-ml* ((texpr arraytype))
  (with-slots (domain range) texpr
    `(("tag" . "arraytype")
      ("domain" ,(pp-json-ml* domain))
      ("range" ,(pp-json-ml* range)))))

(defmethod pp-json-ml* ((texpr funtype))
  (with-slots (domain range) texpr
    (if (dep-binding? domain)
	(let* ((dom (pp-json-ml* domain))
	       (ran (let ((*pp-ml-dont-hash* t))
		      (pp-json-ml* range)))
	       (ftype-ml `(("tag" . "dependent-functiontype")
			   ("domain" . ,dom)
			   ("range" ,ran))))
	  ftype-ml)
	`(("tag" . "functiontype")
	  ("domain" ,(pp-json-ml* domain))
	  ("range" ,(pp-json-ml* range))))))

(defmethod pp-json-ml* ((dep dep-binding))
  (with-slots (id type) dep
    `(("tag" . "dep-binding")
      ("id" . ,(string id))
      ("type" ,(pp-json-ml* type)))))

(defmethod pp-json-ml* ((texpr recordtype))
  (with-slots (fields) texpr
    (let* ((dfields (dependent-fields? fields))
	   (sfields (sort-fields fields dfields))
	   (ml-fields (pp-json-ml-fields sfields)))
      (if dfields
	  `(("tag" . "dependent-recordtype")
	    ("fields" ,ml-fields))
	  `(("tag" . "recordtype")
	    ("fields" ,ml-fields))))))

(defun pp-json-ml-fields (fields &optional ml-fields dfields)
  (if (null fields)
      (nreverse ml-fields)
      (let ((ml-field
	     (let ((*pp-ml-dont-hash*
		    (or *pp-ml-dont-hash*
			(some #'(lambda (fv) (memq (declaration fv) dfields))
			      (freevars (type (car fields)))))))
	       (pp-json-ml* (car fields)))))
	(pp-json-ml-fields (cdr fields)
			   (cons ml-field ml-fields)
			   (cons (car fields) dfields)))))

(defmethod pp-json-ml* ((fld field-decl))
  (with-slots (id type) fld
    `(("tag" . "field")
      ("id" . ,(string id))
      ("type" ,(pp-json-ml* type)))))

(defmethod pp-json-ml* ((texpr tupletype))
  (with-slots (types) texpr
    (if (some #'dep-binding? types)
	(let ((ntypes (pp-json-ml-dep-tupletypes types)))
	  `(("tag" . "dependent-tupletype")
	    ("types" . ,(coerce ntypes 'vector))))
	`(("tag" . "tupletype")
	  ("types" . ,(map 'vector #'(lambda (ty) (list (pp-json-ml* ty))) types))))))

(defun pp-json-ml-dep-tupletypes (types &optional ml-types dbindings)
  (if (null types)
      (nreverse ml-types)
      (let* ((nohash (or *pp-ml-dont-hash*
			  (some #'(lambda (fv) (memq (declaration fv) dbindings))
				(freevars (car types)))))
	     (ml-ntype (let ((*pp-ml-dont-hash* nohash))
			 (pp-json-ml* (car types))))
	     (ml-type (if nohash ml-ntype (list ml-ntype))))
	;;(break "pp-json-ml-dep-tupletypes")
	(pp-json-ml-dep-tupletypes (cdr types)
				   (cons ml-type ml-types)
				   (if (dep-binding? (car types))
				       (cons (car types) dbindings)
				       dbindings)))))

(defmethod pp-json-ml* ((texpr subtype))
  (with-slots (supertype predicate) texpr
    (let ((stype `(("tag" . "subtype")
		   ("supertype" ,(pp-json-ml* supertype))
		   ("predicate" . ,(pp-json-ml* predicate)))))
      stype)))

(defmethod pp-json-ml* ((texpr type-name))
  (let* ((decl (declaration texpr))
	 (th (module decl))
	 (lib (pp-json-library decl))
	 (path (unless lib (pp-json-path th))))
    `(("tag" . "typename")
      ("id" . ,(string (id decl)))
      ,@(when lib `(("library" . ,lib)))
      ,@(when path `(("path" . ,path))) ; Only when library not available
      ("theory" . ,(string (id th)))
      ("actuals" . ,(pp-json-ml* (actuals (module-instance texpr)))))))

(defmethod pp-json-ml* ((texpr type-application))
  (with-slots (type parameters) texpr
    `(("tag" . "type-application")
      ("type" ,(pp-json-ml* type))
      ("parameters" . ,(pp-json-ml* parameters)))))


;;; Proof sessions

(defun pp-json-proof-session (session)
  ;; Session has the form
  ;; (("id" . id) ("proofstate" . ps) ("status" . status)
  ;;  ("commentary" . (list of strings and proofstates))
  (pp-json-proof-session* (cdr (assoc "commentary" session :test #'string=))))

(defun pp-json-proof-session* (session &optional commentary ps-jsons)
  (cond ((null session)
	 (when commentary
	   (let ((last-commentary (assoc "commentary" (car ps-jsons) :test #'string=)))
	     (if last-commentary
		 (nconc last-commentary (nreverse commentary))
		 (nconc (car ps-jsons) `(("commentary" . ,(nreverse commentary)))))))
	 (reverse ps-jsons))
	((proofstate? (car session))
	 (let ((ps-json (pp-json-proofstate (car session) (reverse commentary))))
	   (pp-json-proof-session* (cdr session) nil (cons ps-json ps-jsons))))
	(t (assert (stringp (car session)))
	   (pp-json-proof-session* (cdr session) (cons (car session) commentary) ps-jsons))))

;;; Proofstates

(defun pp-json-proofstate (ps commentary)
  (unless (or (top-proofstate? ps)
	      (parent-proofstate ps))
    (break "no way to determine new?"))
  (let* ((label (label ps))
	 (current-goal (current-goal ps))
	 (current-rule (current-rule ps))
	 (current-input (current-input ps))
	 (par-sforms (unless (top-proofstate? ps)
		       (s-forms (current-goal (parent-proofstate ps)))))
	 (pp-json-ml-rule (pp-json-ml-input current-rule))
	 (pp-json-ml-inp (pp-json-ml-input current-input))
	 (pp-json-ml-ps `(("tag" . "proofstate")
			  ("label" . ,label)
			  ("depth" . ,(proofstate-depth ps))
			  ("current-goal" . ,(pp-json-ml-sequent current-goal par-sforms))
			  ,@(when current-rule `(("current-rule" . ,pp-json-ml-rule)))
			  ,@(when current-input `(("current-input" . ,pp-json-ml-inp)))
			  ,@(when commentary `(("commentary" . ,(nreverse commentary)))))))
    ;; (with-output-to-string (*standard-output*)
    ;; 	(json:encode-json pp-json-ml-ps))
    pp-json-ml-ps))

(defun pp-json-ml-sequent (seq par-sforms)
  (with-slots (p-sforms n-sforms hidden-s-forms) seq
    `(("tag" . "sequent")
      ;;,@(when n-sforms `(("antecedents" . ,(pp-json-ml-sforms n-sforms par-sforms))))
      ("antecedents" . ,(pp-json-ml-sforms n-sforms par-sforms))
      ("consequents" . ,(pp-json-ml-sforms p-sforms par-sforms))
      ;;,@(when hidden-s-forms `(("hidden" . ,(pp-json-ml-sforms hidden-s-forms nil))))
      ("hidden" . ,(pp-json-ml-sforms hidden-s-forms nil))
      )))

(defun pp-json-ml-sforms (sforms par-sforms)
  (map 'vector #'(lambda (sf) (pp-json-ml-sform sf par-sforms)) sforms))

(defmethod pp-json-ml-sform (sform par-sforms)
  (with-slots (formula label) sform
    (let ((new? (not (memq sform par-sforms)))
	  (*pp-json-ml-bindings* nil))
      `(("tag" . "s-formula")
	,@(when label `(("label" . ,label)))
	("new?" . ,new?)
	("formula" . ,(pp-json-ml* formula))
	;; ("asserted?" . ,asserted?) ;; always nil currently, and shouldn't be a slot
	))))

(defmethod pp-json-ml* ((expr t))
  (format nil "~a" expr))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;declarations

;; (defmethod pp-json-ml* ((decl type-decl))
;;   (with-slots (id type-value) decl
;;     `(type-decl :id ,id
;; 		:type-value ,(pp-json-ml* type-value))))

;; (defmethod pp-json-ml* ((decl type-eq-decl))
;;   (with-slots (id type-expr) decl
;;     `(type-eq-decl :id ,id
;; 		   :type-expr ,(pp-json-ml* type-expr))))

;; (defmethod pp-json-ml* ((decl const-decl))
;;   (with-slots (id type definition) decl
;;       `(const-decl :id ,id
;; 		   :type ,(pp-json-ml* type)
;; 		   :definition ,(pp-json-ml* definition))))

;; (defmethod pp-json-ml* ((decl def-decl))
;;   (with-slots (id type definition measure) decl
;;       `(def-decl :id ,id
;; 	 :type ,(pp-json-ml* type)
;; 	 :definition ,(pp-json-ml* definition)
;; 	 :measure ,(pp-json-ml* measure))))

(defun pp-json-path (th)
  (let ((cpath (context-path th)))
    (unless (file-equal cpath (current-context-path))
      (namestring cpath))))

(defmethod pp-json-ml* ((module module))
  (with-slots (id formals assuming theory) module
    (let* ((lib (pp-json-library module))
	   (path (unless lib (pp-json-path module))))
      `(("tag" . "theory")
	("id" . ,(string id))
	,@(when lib `(("library" . ,lib)))
	;; Only when library not available or in current workspace
	,@(when path `(("path" . ,path))) 
	,@(when formals `(("formals" . ,(pp-json-ml-decl* formals))))
	,@(when assuming `(("assuming" . ,(remove-if #'null (pp-json-ml-decl* assuming)))))
	("declarations" . ,(remove-if #'null (pp-json-ml-decl* theory)))))))

(defmethod pp-json-ml* ((adt datatype))
  (with-slots (id formals assuming constructors) adt
    (let ((alist `(("tag" . "datatype")
		   ("id" . ,(string id))
		   ,@(when formals
		       `(("formals" . ,(pp-json-ml-decl* formals))))
		   ,@(when assuming
		       `(("assuming" . ,(pp-json-ml-decl* assuming))))
		   ("constructors" . ,(pp-json-ml-decl* constructors)))))
      ;; (break "pp-json-ml* datatype")
      ;; (json:encode-json alist)
      alist)))

;;; Theory elements

(defmethod pp-json-ml-decl* :around ((elt inline-recursive-type))
  (with-current-decl elt
    (let ((*pp-json-ml-bindings* nil))
      (call-next-method))))

(defmethod pp-json-ml-decl* :around ((elt exporting))
  (with-current-decl elt
    (let ((*pp-json-ml-bindings* nil))
      (call-next-method))))

(defmethod pp-json-ml-decl* :around ((elt importing-entity))
  (with-current-decl elt
    (let ((*pp-json-ml-bindings* nil))
      (call-next-method))))

(defmethod pp-json-ml-decl* :around ((decl declaration))
  (with-current-decl decl
    (let ((*pp-json-ml-bindings* (apply #'append (formals decl))))
      (call-next-method))))

;;; Declarations

(defmethod pp-json-ml-decl* ((decl inline-datatype))
  (with-slots (id constructors) decl
    `(("tag" . "datatype")
      ("id" . ,(string id))
      ("constructors" ,(pp-json-ml-decl* constructors)))))

(defmethod pp-json-ml-decl* ((c simple-constructor))
  (with-slots (id arguments recognizer) c
    (let ((alist `(("tag" . "constructor")
		   ("id" . ,(string id))
		   ,@(when arguments
		       `(("accessors" . ,(pp-json-ml-decl* arguments))))
		   ("recognizer" . ,recognizer))))
      alist)))

(defmethod pp-json-ml-decl* ((decl adtdecl))
  (with-slots (id declared-type type) decl
    `(("tag" . "accessor")
      ("id" . ,(string id))
      ;; ("declared-type" ,(pp-json-ml* declared-type))
      ("type" ,(pp-json-ml* type)))))
  
(defun pp-json-ml-theory (theory)
  (pp-json-ml* theory))

(defmethod pp-justification ((justification justification))
  (pp-justification* (extract-justification-sexp justification) nil))

(defmethod pp-justification (justification)
  (pp-justification* justification nil))

(defun pp-justification* (justification label)
  (cond ((null justification)
	 nil)
	(t (when (comment justification)
	     (commentary "~%~a" (comment justification)))
	   (cond ((equal (label justification) label)
		  (commentary "~%")
		  (commentary "~V@T" (+ 3 (length (string label)))))
		 (t (commentary "~%~a : " (label justification))))
	   (commentary "~a" (format-rule (rule justification)))
	   (loop for entry in (reverse (subgoals justification))
		 do (pp-justification* entry (car justification))))))

(defun pp-json-ml-prelude ()
  (with-workspace (format nil "~a/lib/" *pvs-path*)
    (uiop:ensure-all-directories-exist (list "json/"))
    (dolist (th *prelude-theories*)
      (theory-to-json-file th)
      (when (recursive-type? th)
	(theory-to-json-file (adt-theory th))
	(when (adt-map-theory th)
	  (theory-to-json-file (adt-map-theory th)))
	(when (adt-reduce-theory th)
	  (theory-to-json-file (adt-reduce-theory th)))))))

(defun pp-json-ml-all-libraries ()
  (dolist (lpair (pvs-library-alist))
    (pp-json-ml-library (cdr lpair))))

(defun pp-json-ml-library (lib &optional (theory-names '("top")))
  (with-workspace lib
    (unless nil ;(uiop:directory-exists-p "json")
      (uiop:ensure-all-directories-exist (list "json/"))
      (dolist (thname theory-names)
	(typecheck-file thname))
      (maphash #'(lambda (thid th)
		   (declare (ignore thid))
		   (theory-to-json-file th)
		   (let ((formulas (provable-formulas th)))
		     (dolist (formula formulas)
		       (collect-json-proof formula))))
	       (current-pvs-theories)))))

(defvar *testing-json* nil)

(defun theory-to-json-file (th)
  (with-workspace th
    (with-context th
      (let* ((*pp-json-ml-type-hash* (make-pvs-hash-table))
	     (*pp-type-json-ml-hash* (make-hash-table :test 'string=))
	     (mlth (pp-json-ml-theory th))
	     (thash (print-type-hash))
	     (mlmod `(("tag" . "module-with-hash")
		      ("module" . ,mlth)
		      ("type-hash" . ,thash)))
	     (json-path (format nil "json/~a.json" (id th))))
	(uiop:ensure-all-directories-exist (list "json/"))
	(with-open-file (jdmp json-path
			      :direction :output :if-exists :supersede
			      :if-does-not-exist :create)
	  (json:encode-json mlmod jdmp))
	(format t "~%Wrote ~a" (truename json-path))
	(when *testing-json*
	  (let ((json:*identifier-name-to-key* 'string-downcase))
	    (json:decode-json-from-source (pathname json-path))))))))

(defvar *prover-input-list*)

(defun collect-prelude-json-proofs ()
  (dolist (th *prelude-theories*)
    (let ((formulas (provable-formulas th)))
      (dolist (formula formulas)
	(collect-json-proof formula)))))

;; (defun prover-list-input ()
;;   (if *prover-input-list*
;;       (let ((input (pop *prover-input-list*)))
;; 	(push input *dump-proof-data-to-file*)
;;       '(quit))))

;; (defun pp-json-output-hook (ps)
;;   (when *dump-proof-data-to-file*
;;     (push ps *pp-json-ml-proof-data*)))

;; (defun pp-json-done-hook ()
;;   (when *dump-proof-data-to-file*
;;     (assert *pp-json-ml-type-hash*)
;;     (let* ((data (reverse *pp-json-ml-proof-data*))
;; 	   (date (car data))
;; 	   (prf-data (mapcar #'(lambda (d)
;; 				 (assert (listp d))
;; 				 ;; proofstates already in pp-json-ml form, need :inputs also
;; 				 (if (symbolp (car d))
;; 				     (pp-json-ml d)
;; 				     d))
;; 		       (cdr data)))
;; 	   (prdata `(("tag" . "proof-session")
;; 		     ("time" . ,date)
;; 		     ("status" . ,(if (eq (status-flag *top-proofstate*) '!)
;; 				      "proved" "quit"))
;; 		     ("proof" . ,prf-data)
;; 		     ("type-hash" . ,(print-type-hash)))))
;;       (ensure-directories-exist *dump-proof-data-to-file*)
;;       (with-open-file (prfdump *dump-proof-data-to-file*
;; 			       :direction :output :if-exists :supersede
;; 			       :if-does-not-exist :create)
;; 	(setq *last-prdata* prdata)
;; 	(json:encode-json-alist prdata prfdump)))))

(defun collect-json-proof (formula)
  (let* ((fdecl (get-formula-decl formula))
	 (*current-context* (context fdecl))
	 (thdir (format nil "~ajson/~a-proofs"
		  (context-path (module fdecl)) (id (module fdecl))))
	 (dfile (format nil "~a/~a.json" thdir (id fdecl)))
	 ;;(*dump-proof-data-to-file* dfile)
	 (*pp-json-ml-type-hash* (make-pvs-hash-table))
	 (*pp-type-json-ml-hash* (make-hash-table :test 'string=))
	 (*log-proofs* t)
	 (*noninteractive* t)
	 (*rewrite-msg-off* t) ;; *suppress-printing* isn't enough to shut off these messages
	 (*multiple-proof-default-behavior* :noquestions)
	 (def-prf (default-proof fdecl))
	 (input-list (when def-prf
		       (flatten-proof-script
			(editable-justification (script def-prf))))))
    (format t "~%Generating ~a" dfile)
    (uiop:ensure-all-directories-exist (list thdir))
    ;; (with-prover-input-hook 'prover-list-input
    ;;   (with-prover-output-hook 'pp-json-output-hook
    ;; 	(with-prover-done-hook 'pp-json-done-hook
    ;; 	  (prove-formula fdecl))))
    (let* ((result (prover-init
		    fdecl
		    :input-hook (let ((pr-input input-list))
				  (lambda ()
				    (if pr-input
					(let ((inp (pop pr-input)))
					  (commentary "input: ~a" inp)
					  inp)
					'(quit))))))
	   (prf-session (pp-json-proof-session result)))
      (with-open-file (str dfile :direction :output :if-exists :supersede)
	(json:encode-json prf-session str)))
    (format t "~%Generated size ~d"
      #+allegro (excl.osi:stat-size (excl.osi:stat dfile))
      #+sbcl (sb-posix:stat-size (sb-posix:stat dfile)))
    (when *testing-json*
      (let ((json:*identifier-name-to-key* 'string-downcase))
	(json:decode-json-from-source dfile)))))

(defun pp-json-ml-input (inp)
  (let* ((inp-args (when (listp inp) (pp-json-ml-input-args inp)))
	 (pp-json-ml-inp `(("tag" . "input")
			   ("rule" . ,(string-downcase (if (listp inp) (car inp) inp)))
			   ("arguments" . ,inp-args))))
    ;;(json:encode-json pp-json-ml-inp)
    pp-json-ml-inp))

(defun pp-json-ml-input-args (input)
  (let ((rule (car input)))
    ;; rule may be needed in some cases
    (pp-json-ml-input-args* (cdr input) rule nil)))

(defun pp-json-ml-input-args* (args rule pp-json-ml-args)
  (if (null args)
      (nreverse pp-json-ml-args)
      (let* ((ml-arg (pp-json-ml (car args)))
	     (pp-json-ml-arg (typecase ml-arg
			 ((or list string symbol cl:number vector) ml-arg)
			 (t (break "pp-json-ml-input-args*: ~s" ml-arg)))))
	(pp-json-ml-input-args* (cdr args) rule (cons pp-json-ml-arg pp-json-ml-args)))))

;;; cmdpred interface

(defvar *cmdpred-server-available* nil)

(defun use-cmdpred-server ()
  (push 'cmdpred-hook *proofstate-hooks*))

(defun cmdpred-server-available ()
  (let ((stat (uiop:run-program
		  "curl -o /dev/null --silent -Iw '%{http_code}' http://localhost:7001"
		:output '(:string :stripped t)
		:ignore-error-status t)))
    (string= stat "200")))

(defvar *cmdpred-history* nil)
(defvar *cmdpred-ctr* 0)

(defun cmdpred-hook (ps)
  (unless *cmdpred-server-available*
    (setq *cmdpred-server-available* (cmdpred-server-available)))
  (when (and *cmdpred-server-available*
	     (not *in-apply*))
    (if (top-proofstate? ps)
	(setq *cmdpred-history* nil)
	(when (parent-proofstate ps)
	  (setq *cmdpred-history*
		(nconc *cmdpred-history*
		       (list (current-input (parent-proofstate ps)))))))
    (let* ((*pp-json-ml-type-hash* (make-pvs-hash-table))
	   (*pp-type-json-ml-hash* (make-hash-table :test 'string=))
	   (json-ps (pp-json-ml* ps))
	   (json-state `(("state" . ,json-ps)
			 ("cmd_history" ,@(mapcar #'(lambda (c) (string (car c)))
					    (last *cmdpred-history* 3)))))
	   (js-file (format nil "~aproofstate~d.json"
		      (namestring (truename (current-context-path)))
		      (incf *cmdpred-ctr*))))
      (with-open-file (psjs js-file :direction :output
			    :if-exists :supersede :if-does-not-exist :create)
	(json:encode-json-alist json-state psjs))
      (let* ((curl-cmd (format nil "curl --header \"Content-Type: application/json\" --request POST --data @~a http://localhost:7001/query" js-file))
	     (cmds (uiop:run-program curl-cmd :output '(:string :stripped t))))
	;;(delete-file js-file)
	(unless (string= cmds "<!doctype html>" :end1 (length "<!doctype html>"))
	  (format t "~%Predicted commands: ~a" cmds))))))

;; Essentially replicates CoProver/demos/cmdpred/demo_query.sh
(defun demo-query ()
  (let ((file "/home/owre/CoProver/demos/cmdpred/demo_state.json"))
    (uiop:run-program (format nil "curl --header \"Content-Type: application/json\" --request POST --data @~a http://localhost:7001/query" file)
      :output '(:string :stripped t))))

(defvar *lemmaref-server-available* nil)

(defun use-lemmaref-server ()
  (push 'lemmaref-hook *proofstate-hooks*))

(defun lemmaref-server-available ()
  (let ((stat (uiop:run-program
		  "curl -o /dev/null --silent -Iw '%{http_code}' http://localhost:7111"
		:output '(:string :stripped t)
		:ignore-error-status t)))
    (string= stat "200")))

(defvar *lemmaref-history* nil)
(defvar *lemmaref-ctr* 0)

(defun lemmaref-hook (ps)
  (unless *lemmaref-server-available*
    (setq *lemmaref-server-available* (lemmaref-server-available)))
  (when (and *lemmaref-server-available*
	     (not *in-apply*))
    (if (top-proofstate? ps)
	(setq *lemmaref-history* nil)
	(when (parent-proofstate ps)
	  (setq *lemmaref-history*
		(nconc *lemmaref-history*
		       (list (current-input (parent-proofstate ps)))))))
    (let* ((*pp-json-ml-type-hash* (make-pvs-hash-table))
	   (*pp-type-json-ml-hash* (make-hash-table :test 'string=))
	   (json-ps (pp-json-ml* ps))
	   (json-state `(("state" . ,json-ps)
			 ("cmd_history" ,@(mapcar #'(lambda (c) (string (car c)))
					    (last *lemmaref-history* 3)))))
	   (js-file (format nil "~aproofstate~d.json"
		      (namestring (truename (current-context-path)))
		      (incf *lemmaref-ctr*))))
      (with-open-file (psjs js-file :direction :output
			    :if-exists :supersede :if-does-not-exist :create)
	(json:encode-json-alist json-state psjs))
      (let* ((curl-cmd (format nil "curl --header \"Content-Type: application/json\" --request POST --data @~a http://localhost:7111/query" js-file))
	     (lemma-str (uiop:run-program curl-cmd :output '(:string :stripped t))))
	;;(delete-file js-file)
	(unless (string= lemma-str "<!doctype html>" :end1 (length "<!doctype html>"))
	  (let* ((lemmas (json:decode-json-from-string lemma-str))
		 (suggestions (remove-if-not #'(lambda (lem)
						 (let ((decls (get-declarations (intern lem :pvs))))
						   (and decls
							(not (tcc? (car decls))))))
				lemmas)))
	  (format t "~%Suggested lemmas:~% ~a" suggestions)))))))
