;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; occurs-in.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Jan 16 22:08:45 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Oct 29 22:50:42 1998
;; Update Count    : 9
;; Status          : Beta
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(defmethod occurs-in (x y)
  (when (eql x y) y))

(defmethod occurs-in (obj (l list))
  (when l
    (or (occurs-in obj (car l))
	(occurs-in obj (cdr l)))))

(defmethod occurs-in (obj (te type-name))
  (if (tc-eq obj te)
      te
      (call-next-method)))

(defmethod occurs-in (obj (te adtdecl))
  (occurs-in obj (type te)))

(defmethod occurs-in (obj (te dep-binding))
  (occurs-in obj (type te)))

(defmethod occurs-in ((obj declaration) (te type-name))
  (or (when (eq obj (declaration te))
	te)
      (call-next-method)))

(defmethod occurs-in (obj (te expr-as-type))
  (if (tc-eq obj te)
      te
      (occurs-in obj (expr te))))

(defmethod occurs-in ((obj subtype) (te subtype))
  (or (when (tc-eq obj te)
	te)
      (occurs-in obj (print-type te))
      (occurs-in obj (supertype te))
      (occurs-in obj (predicate te))))

(defmethod occurs-in (obj (te subtype))
  (or (occurs-in obj (print-type te))
      (occurs-in obj (supertype te))
      (occurs-in obj (predicate te))))

(defmethod occurs-in ((obj funtype) (te funtype))
  (or (when (tc-eq obj te)
	te)
      (occurs-in obj (domain te))
      (occurs-in obj (range te))))

(defmethod occurs-in (obj (te funtype))
  (or (occurs-in obj (domain te))
      (occurs-in obj (range te))))

(defmethod occurs-in ((obj tupletype) (te tupletype))
  (or (when (tc-eq obj te)
	te)
      (occurs-in obj (types te))))

(defmethod occurs-in (obj (te tupletype))
  (occurs-in obj (types te)))

(defmethod occurs-in ((obj cotupletype) (te cotupletype))
  (or (when (tc-eq obj te)
	te)
      (occurs-in obj (types te))))

(defmethod occurs-in (obj (te cotupletype))
  (occurs-in obj (types te)))

(defmethod occurs-in ((obj recordtype) (te recordtype))
  (or (when (tc-eq obj te)
	te)
      (occurs-in obj (fields te))))

(defmethod occurs-in (obj (te recordtype))
  (occurs-in obj (fields te)))


;;; Recurse on the range, otherwise we will never terminate.

(defmethod occurs-in ((obj name) (fd field-decl))
  (or (when (same-declaration obj fd)
	fd)
      (occurs-in obj (type fd))))

(defmethod occurs-in (obj (fd field-decl))
  (occurs-in obj (type fd)))


;;; Expressions

(defmethod occurs-in ((obj name) (ex name-expr))
  (or (when (same-declaration obj ex)
	ex)
      (call-next-method)))

(defmethod occurs-in ((decl field-decl) (ex name-expr))
  (when (eq decl (declaration ex))
    ex))

(defmethod occurs-in ((decl declaration) (ex name-expr))
  (when (eq decl (declaration ex))
    ex))

(defmethod occurs-in ((obj number-expr) (ex number-expr))
  (when (= (number obj) (number ex)) ex))

(defmethod occurs-in (obj (ex number-expr))
  (declare (ignore obj))
  nil)

(defmethod occurs-in ((obj record-expr) (ex record-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (assignments ex))))

(defmethod occurs-in (obj (ex record-expr))
  (occurs-in obj (assignments ex)))

(defmethod occurs-in ((obj tuple-expr) (ex tuple-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (exprs ex))))

(defmethod occurs-in (obj (ex tuple-expr))
  (occurs-in obj (exprs ex)))

;(defmethod occurs-in (obj (ex coercion))
;  (or (when (tc-eq obj ex) ex)
;      (occurs-in obj (expression ex))
;      (occurs-in obj (type ex))))

;(defmethod occurs-in (obj (ex intype))
;  (or (when (tc-eq obj ex) ex)
;      (occurs-in obj (expression ex))
;      (occurs-in obj (type-value ex))))

(defmethod occurs-in ((obj projection-expr) (ex projection-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex projection-expr))
  (occurs-in obj (actuals ex)))

(defmethod occurs-in ((obj injection-expr) (ex injection-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex injection-expr))
  (occurs-in obj (actuals ex)))

(defmethod occurs-in ((obj injection?-expr) (ex injection?-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex injection?-expr))
  (occurs-in obj (actuals ex)))

(defmethod occurs-in ((obj extraction-expr) (ex extraction-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex extraction-expr))
  (occurs-in obj (actuals ex)))

(defmethod occurs-in ((obj projection-application) (ex projection-application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex projection-application))
  (or (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in ((obj injection-application) (ex injection-application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex injection-application))
  (or (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in ((obj injection?-application) (ex injection?-application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex injection?-application))
  (or (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in ((obj extraction-application) (ex extraction-application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in (obj (ex extraction-application))
  (or (occurs-in obj (argument ex))
      (occurs-in obj (actuals ex))))

(defmethod occurs-in ((obj field-application) (ex field-application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (argument ex))))

(defmethod occurs-in (obj (ex field-application))
  (occurs-in obj (argument ex)))

(defmethod occurs-in ((obj application) (ex application))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (operator ex))
      (occurs-in obj (argument ex))))

(defmethod occurs-in (obj (ex application))
  (or (occurs-in obj (operator ex))
      (occurs-in obj (argument ex))))

(defmethod occurs-in ((obj binding-expr) (ex binding-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (bindings ex))
      (occurs-in obj (expression ex))))

(defmethod occurs-in (obj (ex binding-expr))
  (or (occurs-in obj (bindings ex))
      (occurs-in obj (expression ex))))

(defmethod occurs-in ((obj cases-expr) (ex cases-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (expression ex))
      (occurs-in obj (selections ex))
      (occurs-in obj (else-part ex))))

(defmethod occurs-in (obj (ex cases-expr))
  (or (occurs-in obj (expression ex))
      (occurs-in obj (selections ex))
      (occurs-in obj (else-part ex))))

(defmethod occurs-in (obj (ex selection))
  (or (occurs-in obj (constructor ex))
      (occurs-in obj (args ex))
      (occurs-in obj (expression ex))))

(defmethod occurs-in ((obj update-expr) (ex update-expr))
  (or (when (tc-eq obj ex) ex)
      (occurs-in obj (expression ex))
      (occurs-in obj (assignments ex))))

(defmethod occurs-in (obj (ex update-expr))
  (or (occurs-in obj (expression ex))
      (occurs-in obj (assignments ex))))

(defmethod occurs-in ((obj assignment) (ass assignment))
  (or (when (tc-eq obj ass) ass)
      (occurs-in obj (arguments ass))
      (occurs-in obj (expression ass))))

(defmethod occurs-in (obj (ass assignment))
  (or (occurs-in obj (arguments ass))
      (occurs-in obj (expression ass))))

(defmethod occurs-in ((decl bind-decl) (bd bind-decl))
  (or (when (eq decl bd) bd)
      (occurs-in decl (type bd))))

(defmethod occurs-in (obj (bd bind-decl))
  (occurs-in obj (type bd)))

(defmethod occurs-in ((obj name) (nm name))
  (or (when (tc-eq obj nm) nm)
      (occurs-in obj (actuals nm))
      (call-next-method)))

(defmethod occurs-in (obj (nm name))
  (occurs-in obj (actuals nm)))

(defmethod occurs-in ((obj actual) (act actual))
  (or (when (tc-eq obj act) act)
      (if (type-value act)
	  (occurs-in obj (type-value act))
	  (occurs-in obj (expr act)))))

(defmethod occurs-in (obj (act actual))
  (if (type-value act)
      (occurs-in obj (type-value act))
      (occurs-in obj (expr act))))

(defmethod occurs-in ((obj formal-theory-decl) (nm name))
  (occurs-in (generated-theory obj) nm))

(defmethod occurs-in ((obj theory-interpretation) (nm name))
  (and (resolution nm)
       (eq (module (declaration nm)) obj)))

;;; id-occurs-in checks whether the id occurs in the term.  Note that we
;;; use string= rather than eq, since string= ignores package names.
;;; Thus (eq pvs::TRUE sal::TRUE) is nil,
;;; but  (string= pvs::TRUE sal::TRUE) is t.
;;; This is kind of a hack, may want to fix it later.

(defmethod id-occurs-in (id y)
  (and (symbolp y)
       (string= id y)))

(defmethod id-occurs-in (id (adt recursive-type))
  (or (id-occurs-in id (formals adt))
      (id-occurs-in id (assuming adt))
      (id-occurs-in id (constructors adt))))

(defmethod id-occurs-in (id (c simple-constructor))
  (or (eq id (id c))
      (some@ #'(lambda (a) (id-occurs-in id a))
	    (arguments c))))

(defmethod id-occurs-in (id (a typed-declaration))
  (or (eq id (id a))
      (id-occurs-in id (declared-type a))))


(defmethod id-occurs-in (id (l list))
  (unless (null l)
    (or (id-occurs-in id (car l))
	(id-occurs-in id (cdr l)))))

;(defmethod id-occurs-in (id (te type-name))
;  (or (tc-eq id te)
;      (call-next-method)))

(defmethod id-occurs-in (id (te type-application))
  (id-occurs-in id (parameters te)))

(defmethod id-occurs-in (id (te dep-binding))
  (or (eq id (id te))
      (id-occurs-in id (declared-type te))))

(defmethod id-occurs-in (id (te expr-as-type))
  (id-occurs-in id (expr te)))

(defmethod id-occurs-in (id (te subtype))
  (or (id-occurs-in id (supertype te))
      (id-occurs-in id (predicate te))
      (and (slot-exists-p te 'formula)
	   (id-occurs-in id (formula te)))))

(defmethod id-occurs-in (id (te funtype))
  (or (id-occurs-in id (domain te))
      (id-occurs-in id (range te))))

(defmethod id-occurs-in (id (te tupletype))
  (id-occurs-in id (types te)))

(defmethod id-occurs-in (id (te cotupletype))
  (id-occurs-in id (types te)))

(defmethod id-occurs-in (id (te recordtype))
  (id-occurs-in id (fields te)))

(defmethod id-occurs-in (id (fd field-decl))
  (or (eq id (id fd))
      (id-occurs-in id (declared-type fd))))


;;; Expressions

;(defmethod id-occurs-in (id (ex name-expr))
;  (or (tc-eq id ex)
;      (call-next-method)))

(defmethod id-occurs-in (id (ex number-expr))
  (declare (ignore id))
  nil)

(defmethod id-occurs-in (id (ex record-expr))
  (id-occurs-in id (assignments ex)))

(defmethod id-occurs-in (id (ex tuple-expr))
  (id-occurs-in id (exprs ex)))

;(defmethod id-occurs-in (id (ex coercion))
;  (or (id-occurs-in id (expression ex))
;      (id-occurs-in id (type ex))))

;(defmethod id-occurs-in (id (ex intype))
;  (or (id-occurs-in id (expression ex))
;      (id-occurs-in id (type-value ex))))

(defmethod id-occurs-in (id (ex projection-expr))
  (string= id (id ex)))

(defmethod id-occurs-in (id (ex injection-expr))
  (string= id (id ex)))

(defmethod id-occurs-in (id (ex injection?-expr))
  (string= id (id ex)))

(defmethod id-occurs-in (id (ex extraction-expr))
  (string= id (id ex)))

(defmethod id-occurs-in (id (ex projection-application))
  (or (string= id (id ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex injection-application))
  (or (string= id (id ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex injection?-application))
  (or (string= id (id ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex extraction-application))
  (or (string= id (id ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex field-application))
  (or (string= id (id ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex application))
  (or (id-occurs-in id (operator ex))
      (id-occurs-in id (argument ex))))

(defmethod id-occurs-in (id (ex binding-expr))
  (or (id-occurs-in id (bindings ex))
      (id-occurs-in id (expression ex))))

(defmethod id-occurs-in (id (ex cases-expr))
  (or (id-occurs-in id (expression ex))
      (id-occurs-in id (selections ex))
      (id-occurs-in id (else-part ex))))

(defmethod id-occurs-in (id (ex selection))
  (or (id-occurs-in id (constructor ex))
      (id-occurs-in id (args ex))
      (id-occurs-in id (expression ex))))

(defmethod id-occurs-in (id (ex update-expr))
  (or (id-occurs-in id (expression ex))
      (id-occurs-in id (assignments ex))))

(defmethod id-occurs-in (id (ass assignment))
  (or (id-occurs-in id (arguments ass))
      (id-occurs-in id (expression ass))))

(defmethod id-occurs-in (id (nm name))
  (or (if (symbolp id)
	  (and (symbolp (id nm))
	       (string= id (id nm)))
	  (and (integerp id)
	       (integerp (id nm))
	       (= id (id nm))))
      (id-occurs-in id (actuals nm))))

(defmethod id-occurs-in (id (act actual))
  (if (type-value act)
      (id-occurs-in id (type-value act))
      (id-occurs-in id (expr act))))

(defmethod id-occurs-in (id (bd bind-decl))
  (or (string= id (id bd))
      (id-occurs-in id (declared-type bd))))

(defun occurs-in-eq (x y)
  (let ((found nil))
    (mapobject #'(lambda (ex)
		   (or found
		       (when (eq ex x)
			 (setq found t))))
	       y)
    found))
