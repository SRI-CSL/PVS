;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; print-object.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:42:15 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 01:33:15 1998
;; Update Count    : 9
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

(defvar *debugging-print-object* nil
  "If true, will print using the default print-object.  Useful when
print object produces an error, and won't allow inspection of the object.")
(defvar *print-full-name* nil)

(defmethod print-object ((mod module) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Theory ~a>" (id mod))))

(defmethod print-object ((mod library-theory) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Library-theory ~a>" (id mod))))

(defmethod print-object ((dt datatype) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Datatype ~a>" (id dt))))

(defmethod print-object ((mod library-datatype) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Library-datatype ~a>" (id mod))))

(defmethod print-object ((decl declaration) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<~a ~a>" (type-of decl) (id decl))))

(defmethod print-object ((imp using) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse imp :stream stream))))

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

(defmethod print-object ((expr string-expr) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(format stream "<String-expr ~a>" (unparse expr :string t)))))

(defmethod print-object ((te type-expr) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*no-comments* t))
	(unparse-expanded te :stream stream))))


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
      (format stream "#<Context ~a.~a>"
	      (id (module ctx))
	      (when (declaration ctx)
		(if (using? (declaration ctx))
		    (declaration ctx)
		    (id (declaration ctx)))))))

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

(defmethod print-object ((obj judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Judgement ~a~@[: ~a~]>"
	(or (id obj) (name obj)) (unless (id obj) (type obj)))))

(defmethod print-object ((obj number-judgement) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Judgement ~a~@[: ~a~]>" (number obj) (type obj))))

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

(defmethod print-object ((res resolution) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Resolution ~a~@[[~{~a~^, ~}]~].~a:~a>"
	      (and (module-instance res) (id (module-instance res)))
	      (and (module-instance res) (actuals (module-instance res)))
	      (id (declaration res))
	      (if (eq (kind-of (declaration res)) 'expr)
		  (or (type res) (type (declaration res)))
		  (kind-of (declaration res))))))

(defmethod print-object ((res judgement-resolution) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "#<Judgement-res ~a~@[[~{~a~^, ~}]~].~a:~a>"
	      (and (module-instance res) (id (module-instance res)))
	      (and (module-instance res) (actuals (module-instance res)))
	      (id (declaration res))
	      (judgement-type res))))

#-allegro-v4.3
(defmethod print-object ((ht ht) stream)
  (format stream "<pvs-hash-table with ~d element~:p>"
    (ht-num-elements ht)))

#+allegro
(defmethod describe-object :around (obj stream)
  (call-next-method)
  (when (excl:source-file obj)
    (format stream "  Its source file is ~a" (excl:source-file obj))))

(defmethod kind-of ((decl type-decl)) 'type)
(defmethod kind-of ((decl formal-type-decl)) 'type)
(defmethod kind-of ((decl typed-declaration)) 'expr)
(defmethod kind-of ((decl bind-decl)) 'expr)
(defmethod kind-of ((decl field-decl)) 'expr)
(defmethod kind-of ((decl dep-binding)) 'expr)
(defmethod kind-of ((decl inline-datatype)) 'datatype)
(defmethod kind-of ((decl lib-decl)) 'library)
(defmethod kind-of ((decl mod-decl)) 'module)
(defmethod kind-of ((decl formula-decl)) 'formula)
(defmethod kind-of ((decl judgement)) 'judgement)
(defmethod kind-of ((decl conversion-decl)) 'conversion)

(defmethod print-object ((prinfo proof-info) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "<#PROOF-INFO~@[ ~a:~] ~a>"
	(id prinfo) (if (run-date prinfo)
			(date-string (run-date prinfo))))))

(defmethod print-object ((ht ht) stream)
  (format stream "<pvs-hash-table with ~d element~:p>"
    (ht-num-elements ht)))
