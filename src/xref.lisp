;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xref.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:44:28 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 19:15:36 1998
;; Update Count    : 13
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

(defvar *generate-xref-declaration* nil)

(defvar *xref-names-seen* nil)
(defvar *xref-types-seen* nil)

;;; collects the references of an expression

(defmethod collect-references (ex)
  (let ((*generate-xref-declaration* nil)
	(*xref-names-seen* nil)
	(*xref-types-seen* nil))
    (generate-xref ex)
    *generate-xref-declaration*))

(defmethod collect-references ((ex declaration))
  ex)

#-gcl
(defmethod generate-xref (obj)
  (error "~a not handled" obj))

#+gcl
(defmethod generate-xref (obj)
  (if (typep obj 'tccinfo)
      (generate-xref (tccinfo-formula ti))
      (error "~a not handled" obj)))

#-(or akcl cmu)
(defmethod generate-xref ((h hash-table))
  (maphash #'(lambda (id obj)
	       (declare (ignore id))
	       (generate-xref obj))
	   h))

(defmethod generate-xref ((l list))
  (when l
    (generate-xref (car l))
    (generate-xref (cdr l))))

(defmethod generate-xref ((m module))
  (unless (gethash m *prelude*)
    (mapc #'(lambda (u) (pushnew m (used-by (car u)) :test #'eq))
	  (all-usings m))
    (dolist (d (all-decls m))
      (let ((*generate-xref-declaration* d)
	    (*xref-names-seen* nil)
	    (*xref-types-seen* nil))
	(generate-xref d))))
  m)

(defmethod generate-xref ((use using))
  (generate-xref (modules use)))

(defmethod generate-xref ((d formal-type-decl))
  (call-next-method))

(defmethod generate-xref ((d formal-const-decl))
  (generate-xref (declared-type d)))

(defmethod generate-xref ((d inline-datatype))
  nil)

(defmethod generate-xref ((d lib-decl))
  nil)

(defmethod generate-xref ((d mod-decl))
  nil)

(defmethod generate-xref ((d type-decl))
  (generate-xref (type-value d)))

(defmethod generate-xref ((te enumtype))
  nil)

(defmethod generate-xref :around ((te type-expr))
  (unless (memq te *xref-types-seen*)
    (push te *xref-types-seen*)
    (call-next-method)
    (generate-xref (print-type te))))

(defmethod generate-xref ((te type-application))
  (generate-xref (type te))
  (generate-xref (parameters te)))

(defmethod generate-xref ((te expr-as-type))
  (generate-xref (expr te)))

(defmethod generate-xref ((te subtype))
  (generate-xref (supertype te))
  (generate-xref (predicate te)))

(defmethod generate-xref ((te funtype))
  (generate-xref (domain te))
  (generate-xref (range te)))

(defmethod generate-xref ((te tupletype))
  (generate-xref (types te)))

(defmethod generate-xref ((te recordtype))
  (generate-xref (fields te)))

(defmethod generate-xref ((d field-decl))
  (generate-xref (declared-type d)))

(defmethod generate-xref ((te dep-binding))
  (generate-xref (declared-type te)))

(defmethod generate-xref ((d declaration))
  (error "What are we doing here?"))

(defmethod generate-xref ((d var-decl))
  (generate-xref (declared-type d)))

(defmethod generate-xref ((d const-decl))
  (generate-xref (declared-type d))
  (when (and (slot-boundp d 'definition)
	     (definition d))
    (generate-xref (definition d))))

(defmethod generate-xref ((d def-decl))
  (generate-xref (declared-type d))
  (generate-xref (definition d))
  (generate-xref (measure d)))

(defmethod generate-xref ((d formula-decl))
  (generate-xref (definition d)))

(defmethod generate-xref ((d subtype-judgement))
  (generate-xref (declared-type d))
  (generate-xref (declared-subtype d)))

(defmethod generate-xref ((d number-judgement))
  (generate-xref (number d))
  (generate-xref (declared-type d)))

(defmethod generate-xref ((d name-judgement))
  (generate-xref (name d))
  (generate-xref (declared-type d)))

(defmethod generate-xref ((d application-judgement))
  (generate-xref (name d))
  (generate-xref (declared-type d))
  (generate-xref (formals d)))

(defmethod generate-xref ((d conversion-decl))
  (generate-xref (name d)))

(defmethod generate-xref ((d typed-conversion-decl))
  (call-next-method)
  (generate-xref (declared-type d)))


;;; Expressions

;(defmethod generate-xref ((e expr))
;  (break)
;  (assert (type e)))

(defmethod generate-xref ((e number-expr))
  (assert (type e)))

(defmethod generate-xref ((e record-expr))
  (assert (type e))
  (generate-xref (assignments e)))

(defmethod generate-xref ((e tuple-expr))
  (assert (type e))
  (generate-xref (exprs e)))

(defmethod generate-xref ((e cases-expr))
  (assert (type e))
  (generate-xref (expression e))
  (generate-xref (selections e))
  (generate-xref (else-part e)))

(defmethod generate-xref ((s selection))
  (generate-xref (constructor s))
  (generate-xref (args s))
  (generate-xref (expression s)))

;(defmethod generate-xref ((e coercion))
;  (assert (type e))
;  (generate-xref (expression e)))

;(defmethod generate-xref ((e intype))
;  (assert (type e))
;  (generate-xref (expression e))
;  (generate-xref (type-value e)))

;(defmethod generate-xref ((e if-expr))
;  (assert (type e))
;  (generate-xref (condition e))
;  (generate-xref (then-part e))
;  (generate-xref (else-part e)))

(defmethod generate-xref ((e projection-application))
  (assert (type e))
  (generate-xref (argument e)))

(defmethod generate-xref ((e field-application))
  (assert (type e))
  (generate-xref (argument e)))

(defmethod generate-xref ((e application))
  (assert (type e))
  (generate-xref (operator e))
  (generate-xref (argument e)))

(defmethod generate-xref ((e binding-expr))
  (assert (type e))
  (mapc #'generate-xref (bindings e))
  (generate-xref (expression e)))

(defmethod generate-xref ((e update-expr))
  (assert (type e))
  (generate-xref (expression e))
  (mapc #'generate-xref (assignments e))) 

(defmethod generate-xref ((a assignment))
  (generate-xref (expression a))
  (mapc #'(lambda (args)
	    (mapc #'(lambda (aa)
		      (generate-xref aa))
		  args))
	(arguments a)))

(defmethod generate-xref ((ex field-assignment-arg))
  nil)

(defmethod generate-xref ((b bind-decl))
  (generate-xref (type b)))

(defmethod generate-xref ((p projection-expr))
  (break "shouldn't happen")
  nil)

(defmethod generate-xref ((n field-name-expr))
  (assert (type n))
  nil)

(defmethod generate-xref ((n name-expr))
  (assert (type n))
  (unless (memq n *xref-names-seen*)
    (push n *xref-names-seen*)
    (generate-xref (type n)))
  (call-next-method))

(defmethod generate-xref ((a actual))
  (if (type-value a)
      (generate-xref (type-value a))
      (generate-xref (expr a))))

(defmethod generate-xref ((n modname))
  (generate-xref (actuals n)))

(defmethod generate-xref ((n name))
  (with-slots (resolutions) n
    (assert (and resolutions (null (cdr resolutions))))
    (generate-xref (car resolutions))))

(defmethod generate-xref ((r resolution))
  (with-slots ((decl declaration) (mi module-instance)) r
    (generate-xref mi)
    (unless (or (eq decl *generate-xref-declaration*)
		(binding? decl)
		;;(from-prelude? decl)
		)
      (if (listp *generate-xref-declaration*)
	  (pushnew decl *generate-xref-declaration*)
	  (add-xref decl *generate-xref-declaration*)))))

#-gcl
(defmethod generate-xref ((ti tccinfo))
  (generate-xref (tccinfo-formula ti)))

(defun add-xref (to-decl by-decl)
  (unless (field-decl? by-decl)
    (pushnew to-decl (refers-to by-decl) :test #'eq)))
;; PDL nov 24, 1994.   deleted since referred-by not used anywhere
;;                     and this may cause a space leak
;;    (pushnew by-decl (referred-by to-decl) :test #'eq)))

(defmethod from-prelude? ((th module))
  (gethash (id th) *prelude*))

(defmethod from-prelude? ((th datatype))
  (gethash (id th) *prelude*))

(defmethod from-prelude? ((decl declaration))
  (and (module decl)
       (gethash (id (module decl)) *prelude*)))

(defmethod from-prelude? ((decl binding))
  nil)

(defmethod from-prelude? ((use using))
  nil)
