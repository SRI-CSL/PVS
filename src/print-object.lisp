;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-object.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:42:15 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 22 15:56:53 1999
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

(export '(*debugging-print-object* kind-of))

(defvar *debugging-print-object* nil
  "If true, will print using the default print-object.  Useful when
print object produces an error, and won't allow inspection of the object.")
(defvar *print-full-name* nil)

(defmethod print-object ((mod module) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((det (if (filename mod) "" ":detached ")))
	(if (lib-datatype-or-theory? mod)
	    (format stream "#<Theory ~a~a/~a>" det (context-path mod) (id mod))
	    (format stream "#<Theory ~a~a>" det (id mod))))))

(defmethod print-object ((dt datatype) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Datatype ~a>" (id dt))))

(defmethod print-object ((dt codatatype) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<CoDatatype ~a>" (id dt))))

(defmethod print-object ((decl declaration) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<~a ~a~a.~a>"
	(type-of decl)
	(if (module decl) (if (filename (module decl)) "" ":detached ") ":nopath ")
	(when (module decl) (id (module decl)))
	(id decl))))

(defmethod print-object ((decl decl-formal) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<~a ~a.~a :: ~a>"
	(type-of decl) (when (module decl) (id (module decl))) (id decl)
	(if (associated-decl decl)
	    (format nil "~a.~a"
	      (when (module (associated-decl decl))
		(id (module (associated-decl decl))))
	      (id (associated-decl decl)))
	    "no associated-decl"))))

(defmethod print-object ((decl conversion-decl) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<~a: ~a>"
	(type-of decl) (expr decl))))

(defmethod print-object ((imp importing) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~@<#<IMPORTING ~a>~:>" (theory-name imp))))

(defmethod print-object ((c adt-constructor) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse c :stream stream))))

(defmethod print-object ((sel selection) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse sel :stream stream))))

(defmethod print-object ((expr expr) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse expr :stream stream))))

(defmethod print-object ((te type-expr) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t)
	    (*unparse-expanded* t))
	(unparse te :stream stream))))


(defmethod print-object ((fd field-decl) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~a:~a" (id fd) (declared-type fd))))

(defmethod print-object ((ass assignment) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse ass :stream stream))))

(defmethod print-object ((ctx context) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*print-escape* nil))
	(format stream "~@<#<context ~w~@[~w.pvs~]~@[~_#~w~]~@[~_.~w~]>~:>"
	  (if (theory ctx)
	      (context-path (theory ctx))
	      *default-pathname-defaults*)
	  (when (theory ctx)
	    (filename (theory ctx)))
	  (when (theory ctx)
	    (id (theory ctx)))
	  (when (declaration ctx)
	    (if (importing? (declaration ctx))
		(declaration ctx)
		(id (declaration ctx))))))))

;(defmethod print-object ((obj name-expr) stream)
;  (if *debugging-print-object*
;      (call-next-method)
;      (format stream "#<Name-Expr ~a>" (name obj))))

;(defmethod print-object ((obj number-expr) stream)
;  (if *debugging-print-object*
;      (call-next-method)
;      (format stream "#<Number ~a>" (number obj))))

(defmethod print-object ((obj bind-decl) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse obj :stream stream))))

(defmethod print-object ((obj subtype-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Subtype-judgement ~a SUBTYPE_OF ~a>"
	(declared-subtype obj) (declared-type obj))))

(defmethod print-object ((obj number-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Judgement ~a~@[: ~a~]>" (number-expr obj) (type obj))))

(defmethod print-object ((obj name-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Judgement ~a~@[: ~a~]>"
	(or (id obj) (name obj)) (unless (id obj) (type obj)))))

(defmethod print-object ((obj application-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<appl-jdgt ~a:~_ ~a: ~a>"
	(or (id obj) "@") (name obj) (judgement-type obj))))

(defmethod print-object ((obj rec-application-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<rec-appl-jdgt ~a:~_ ~a: ~a>"
	(or (id obj) "@") (name obj) (judgement-type obj))))

(defmethod print-object ((obj dep-binding) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse obj :stream stream))))

(defmethod print-object ((obj actual) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse obj :stream stream))))

(defmethod print-object ((mapping mapping) stream)
  (if *debugging-print-object*
      (call-next-method)
      (unparse mapping :stream stream)))

(defmethod print-object ((rhs mapping-rhs) stream)
  (if *debugging-print-object*
      (call-next-method)
      (unparse (expr rhs) :stream stream)))

(defmethod print-object ((obj expname) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse obj :stream stream))))

(defmethod print-object ((name name) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse name :stream stream))))

(defun resolution-string (res)
  (format nil "~@<~@[~a@~]~a~@[~I~<[~;~@{~W~^, ~:_~}~;]~:>~]~
               ~@[~I~<[~;~@{~W~^, ~:_~}~;]~:>~]~
               ~@[~I~<{{~;~@{~W~^, ~:_~}~;}}~:>~]~@[.~a~]~:>"
    (and (module-instance res) (library (module-instance res)))
    (and (module-instance res) (id (module-instance res)))
    (and (module-instance res) (actuals (module-instance res)))
    (and (module-instance res) (dactuals (module-instance res)))
    (and (module-instance res) (mappings (module-instance res)))
    (when (declaration res) (id (declaration res)))
    (when (declaration res)
      (if (eq (kind-of (declaration res)) 'expr)
	  (or (type res) (type (declaration res)))
	  (kind-of (declaration res))))))

(defmethod print-object ((res resolution) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream
	  "#<Resolution ~@<~a~:_:~a~:>>"
	(resolution-string res)
	(when (declaration res)
	  (if (eq (kind-of (declaration res)) 'expr)
	      (or (type res) (type (declaration res)))
	      (kind-of (declaration res)))))))

(defmethod print-object ((res mapping-resolution) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream
	  "~@<~2I#<Mapping-Resolution~:@_ ~@<~2I~a ~:_{{ ~a }}~:>~:>>"
	(and (module-instance res) (lcopy (module-instance res) 'mappings nil))
	(declaration res))))

(defmethod print-object ((alists dpinfo) stream)
  (if (or (not *print-expanded-dpinfo*) *debugging-print-object*)
      (call-next-method)
      (let ((*print-level* 3)
	    (*print-length* 3))
	(format stream "#<DPINFO: ~w>" (dpinfo-findalist alists)))))

(defmethod print-object ((strat strategy) stream)
  (if *debugging-print-object*
      (call-next-method)
      (if (strategy? (topstep strat))
	  (print-object (topstep strat) stream)
	  (format stream "#<STRATEGY: ~s>" (topstep strat)))))

(defmethod print-object ((rule rule-instance) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<RULE-INSTANCE: ~s>" (rule-input rule))))

(defmethod print-object ((rule rule-entry) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<RULE-ENTRY: ~s>" (name rule))))

;(defmethod print-object ((list cons) stream)
;  (if *debugging-print-object*
;      (call-next-method)
;      (format stream "~&~@:<~{~d~^ ~:@_~}~:>" list)))

(defmethod print-object ((prinfo proof-info) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#PROOF-INFO~@[ ~a:~] ~a>"
	(id prinfo) (if (description prinfo)
			(description prinfo)))))

(defmethod print-object ((tcc-orig tcc-origin) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#TCC-ORIGIN ~a TCC from ~a>"
	(kind tcc-orig) (root tcc-orig))))

(defmethod print-object ((pt store-print-type) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#store-print-type ~a>"
	(print-type pt))))

(defmethod print-object ((cr conversion-result) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#conversion-result ~a>" (expr cr))))

(defmethod print-object ((jdgmts judgements) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#judgements: judgement-types-hash: ~d,~
                      ~%              judgement-declarations: ~d,~
                      ~%              number-judgements-alist: ~d,~
                      ~%              name-judgements-alist: ~d,~
                      ~%              application-judgements-alist: ~d>"
	(hash-table-count (judgement-types-hash jdgmts))
	(length (judgement-declarations jdgmts))
	(length (number-judgements-alist jdgmts))
	(length (name-judgements-alist jdgmts))
	(length (application-judgements-alist jdgmts)))))

(defmethod print-object ((jdgs application-judgements) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let* ((jdg (or (car (generic-judgements jdgs))
		      (caar (judgements-graph jdgs))))
	     (ndecl (declaration (name jdg))))
	(format stream "#<application-judgements ~a.~a>"
	  (when (module ndecl) (id (module ndecl))) (id ndecl)))))

(defmethod print-object ((ws workspace-session) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<workspace ~a>" (path ws))))
  

(defmethod pp* ((pt store-print-type))
  (format t "<#store-print-type ~a>"
    (print-type pt)))

#+allegro
(defmethod describe-object :around (obj stream)
  (call-next-method)
  (when (ignore-errors (excl:source-file obj))
    (format stream "  Its source file is ~a" (excl:source-file obj))))

(defmethod kind-of ((decl module)) 'module)
(defmethod kind-of ((decl recursive-type)) 'module)
(defmethod kind-of ((decl type-decl)) 'type)
(defmethod kind-of ((decl formal-type-decl)) 'type)
(defmethod kind-of ((decl formal-theory-decl)) 'module)
(defmethod kind-of ((decl typed-declaration)) 'expr)
(defmethod kind-of ((decl bind-decl)) 'expr)
(defmethod kind-of ((decl field-decl)) 'expr)
(defmethod kind-of ((decl dep-binding)) 'expr)
(defmethod kind-of ((decl inline-recursive-type)) 'datatype)
(defmethod kind-of ((decl enumtype)) 'enum)
(defmethod kind-of ((decl lib-decl)) 'library)
(defmethod kind-of ((decl mod-decl)) 'module)
(defmethod kind-of ((decl theory-abbreviation-decl)) 'module)
(defmethod kind-of ((decl formula-decl)) 'formula)
(defmethod kind-of ((decl judgement)) 'judgement)
(defmethod kind-of ((decl conversion-decl)) 'conversion)
(defmethod kind-of ((decl auto-rewrite-decl)) 'auto-rewrite)
(defmethod kind-of ((decl mapped-type-decl)) 'type)
