;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy-lex.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Feb 27 01:34:00 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:10:01 1998
;; Update Count    : 10
;; Status          : Unknown, Use with caution!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

(defvar *copy-lex-exact* nil)

(defun copy-lex (otheory ntheory)
  (copy-lex* otheory ntheory))

(defmethod copy-lex* :around ((old syntax) (new syntax))
  (call-next-method)
  (setf (newline-comment old) (newline-comment new))
  ;;(setf (abstract-syntax old) (abstract-syntax new))
  (setf (place old) (place new)))

(defmethod copy-lex* :around ((old datatype-or-module) (new datatype-or-module))
  (call-next-method)
  (copy-lex-decls (formals old) (formals new))
  (copy-lex-decls (assuming old) (assuming new)))

(defmethod copy-lex* ((old module) (new module))
  (copy-lex* (exporting old) (exporting new))
  (copy-lex-decls (theory old) (theory new)))

(defmethod copy-lex* ((old datatype) (new datatype))
  (copy-lex* (importings old) (importings new))
  (copy-lex* (constructors old) (constructors new)))

;;; inline-datatype not needed

;;; enumtype not needed

(defmethod copy-lex* ((old simple-constructor) (new simple-constructor))
  ;;(copy-lex* (recognizer old) (recognizer new))
  (copy-lex* (arguments old) (arguments new)))

(defmethod copy-lex* ((old exporting) (new exporting))
  (copy-lex* (names old) (names new))
  (copy-lex* (but-names old) (but-names new))
  (copy-lex* (modules old) (modules new)))

(defmethod copy-lex* ((old expname) (new expname))
  )

(defmethod copy-lex* ((old importing) (new importing))
  (copy-lex* (theory-name old) (theory-name new))
  (setf (semi old) (semi new))
  (setf (chain? old) (chain? new)))


(defun copy-lex-decls (old-list new-list)
  (when old-list
    (cond ((and (not *copy-lex-exact*)
		(or (typep (car old-list) 'field-decl)
		    (generated-by (car old-list))))
	   (copy-lex-decls (cdr old-list) new-list))
	  (t (copy-lex* (car old-list) (car new-list))
	     (copy-lex-decls (cdr old-list) (cdr new-list))))))

(defmethod copy-lex* :around ((old declaration) (new declaration))
  (call-next-method)
  (copy-lex* (formals old) (formals new))
  (setf (chain? old) (chain? new))
  (setf (semi old) (semi new)))

(defmethod copy-lex* :around ((old typed-declaration) (new typed-declaration))
  (call-next-method)
  (copy-lex* (declared-type old) (declared-type new)))

(defmethod copy-lex* ((old declaration) (new declaration))
  )

(defmethod copy-lex* ((old typed-declaration) (new typed-declaration))
  )

(defmethod copy-lex* ((old formal-type-decl) (new formal-type-decl))
  (call-next-method)
  (when (type old)
    (setf (place (type old)) (place new)))
  (when (type-value old)
    (setf (place (type-value old)) (place new))))

(defmethod copy-lex* ((old mod-decl) (new mod-decl))
  (copy-lex* (modname old) (modname new)))

(defmethod copy-lex* ((old theory-abbreviation-decl)
		      (new theory-abbreviation-decl))
  (copy-lex* (theory-name old) (theory-name new)))

(defmethod copy-lex* ((old type-def-decl) (new type-def-decl))
  (copy-lex* (type-expr old) (type-expr new))
  (copy-lex* (contains old) (contains new)))

(defmethod copy-lex* ((old const-decl) (new const-decl))
  (copy-lex* (definition old) (definition new)))

(defmethod copy-lex* ((old def-decl) (new def-decl))
  (copy-lex* (declared-measure old) (declared-measure new)))

(defmethod copy-lex* ((old formula-decl) (new formula-decl))
  (setf (spelling old) (spelling new))
  (copy-lex* (definition old) (definition new)))

(defmethod copy-lex* ((old subtype-judgement) (new subtype-judgement))
  (call-next-method)
  (copy-lex* (declared-subtype old) (declared-subtype new)))

(defmethod copy-lex* ((old number-judgement) (new number-judgement))
  (call-next-method)
  (copy-lex* (number-expr old) (number-expr new)))

(defmethod copy-lex* ((old name-judgement) (new name-judgement))
  (call-next-method)
  (copy-lex* (name old) (name new)))

(defmethod copy-lex* ((old application-judgement) (new application-judgement))
  (call-next-method)
  (copy-lex* (name old) (name new))
  (copy-lex* (formals old) (formals new)))

(defmethod copy-lex* ((old conversion-decl) (new conversion-decl))
  (unless (eq (class-of old) (class-of new))
    (change-class old (class-of new)))
  (copy-lex* (name old) (name new)))

(defmethod copy-lex* ((old typed-conversion-decl) (new typed-conversion-decl))
  (call-next-method)
  (copy-lex* (declared-type old) (declared-type new)))


;;; Type expressions

(defmethod copy-lex* :around ((old type-expr) (new type-expr))
  (when (print-type old)
    (copy-lex* (print-type old) new))
  (call-next-method)
  (setf (parens old) (parens new)))

(defmethod copy-lex* ((old type-expr) (new type-expr))
  )

(defmethod copy-lex* ((old type-name) (new type-name))
  (when (and (null (actuals old))
	     (actuals new))
    (setf (actuals old) (actuals (module-instance (resolution old)))))
  (copy-lex* (actuals old) (actuals new)))

(defmethod copy-lex* ((old type-application) (new type-application))
  (copy-lex* (type old) (type new))
  (copy-lex* (parameters old) (parameters new)))

(defmethod copy-lex* ((old subtype) (new subtype))
  (when (supertype new)
    (copy-lex* (supertype old) (supertype new)))
  (copy-lex* (predicate old) (predicate new)))

(defmethod copy-lex* ((old setsubtype) (new setsubtype))
  (cond ((formals old)
	 (copy-lex* (formals old) (formals new))
	 (copy-lex* (formula old) (formula new))
	 (copy-lex* (supertype old) (supertype new)))
	(t (copy-lex* (predicate old) (predicate new)))))

(defmethod copy-lex* ((old nsetsubtype) (new nsetsubtype))
  (cond ((formals old)
	 (copy-lex* (formals old) (formals new))
	 (copy-lex* (formula old) (formula new)))
	(t (copy-lex* (predicate old) (predicate new)))))

(defmethod copy-lex* ((old expr-as-type) (new expr-as-type))
  (copy-lex* (expr old) (expr new)))

(defmethod copy-lex* ((old funtype) (new funtype))
  (unless (eq (class-of old) (class-of new))
    (change-class old (class-of new)))
  (copy-lex* (domain old) (domain new))
  (copy-lex* (range old) (range new)))

(defmethod copy-lex* ((old tupletype) (new tupletype))
  (copy-lex* (types old) (types new)))

(defmethod copy-lex* ((old cotupletype) (new cotupletype))
  (copy-lex* (types old) (types new)))

(defmethod copy-lex* ((old recordtype) (new recordtype))
  (copy-lex* (fields old) (fields new)))


;;; Expressions

(defmethod copy-lex* :around ((old expr) (new expr))
  (call-next-method)
  (setf (parens old) (parens new)))

(defmethod copy-lex* ((old number-expr) (new number-expr))
  )

(defmethod copy-lex* ((old tuple-expr) (new tuple-expr))
  (copy-lex* (exprs old) (exprs new)))

(defmethod copy-lex* ((old record-expr) (new record-expr))
  (copy-lex* (assignments old) (assignments new)))

(defmethod copy-lex* ((old cases-expr) (new cases-expr))
  (copy-lex* (expression old) (expression new))
  (copy-lex* (selections old) (selections new))
  (copy-lex* (else-part old) (else-part new)))

(defmethod copy-lex* ((old selection) (new selection))
  (copy-lex* (constructor old) (constructor new))
  (copy-lex* (args old) (args new))
  (copy-lex* (expression old) (expression new)))

(defmethod copy-lex* ((old projection-application) (new projection-application))
  (copy-lex* (argument old) (argument new)))

(defmethod copy-lex* ((old injection-application) (new injection-application))
  (copy-lex* (argument old) (argument new)))

(defmethod copy-lex* ((old field-application) (new field-application))
  (copy-lex* (argument old) (argument new)))

(defmethod copy-lex* ((old field-application) (new application))
  (copy-lex* (argument old) (argument new)))

(defmethod copy-lex* ((old application) (new application))
  (copy-lex* (operator old) (operator new))
  (copy-lex* (argument old) (argument new)))

(defmethod copy-lex* ((old when-expr) (new application))
  (copy-lex* (operator old) (operator new))
  (copy-lex* (argument old) (reverse (exprs (argument new)))))

(defmethod copy-lex* ((old implicit-conversion) (new expr))
  (copy-lex* (args1 old) new))

(defmethod copy-lex* ((old argument-conversion) (new expr))
  (copy-lex* (operator old) new))

(defmethod copy-lex* ((old lambda-conversion) (new expr))
  (copy-lex* (expression old) new))

(defmethod copy-lex* ((old table-expr) (new table-expr))
  (copy-lex* (row-expr old) (row-expr new))
  (copy-lex* (col-expr old) (col-expr new))
  (copy-lex* (row-headings old) (row-headings new))
  (copy-lex* (col-headings old) (col-headings new))
  (copy-lex* (table-entries old) (table-entries new)))

(defmethod copy-lex* ((old binding-expr) (new binding-expr))
  (unless (eq (class-of old) (class-of new))
    (change-class old (class-of new)))
  (copy-lex* (bindings old) (bindings new))
  (copy-lex* (expression old) (expression new)))

(defmethod copy-lex* ((old update-expr) (new update-expr))
  (copy-lex* (assignments old) (assignments new))
  (copy-lex* (expression old) (expression new)))

(defmethod copy-lex* ((old assignment) (new assignment))
  (copy-lex* (arguments old) (arguments new))
  (copy-lex* (expression old) (expression new)))

(defmethod copy-lex* :around ((old simple-decl) (new simple-decl))
  (when (next-method-p) (call-next-method))
  (when (declared-type old)
    (if (declared-type new)
	(copy-lex* (declared-type old) (declared-type new))
	(setf (declared-type old) nil))))

(defmethod copy-lex* ((old field-decl) (new field-decl))
  (setf (chain? old) (chain? new)))

(defmethod copy-lex* ((old bind-decl) (new bind-decl))
  (setf (chain? old) (chain? new)))

;;(defmethod copy-lex* ((old modname) (new modname))
;;  (copy-lex* (mappings old) (mappings new)))

(defmethod copy-lex* ((old name) (new name))
  (when (and (null (actuals old))
	     (actuals new))
    (setf (actuals old)
	  (mapcar #'copy-all
		  (actuals (module-instance (resolution old))))))
  (when (actuals new)
    (copy-lex* (actuals old) (actuals new))))

(defmethod copy-lex* ((old actual) (new actual))
  (copy-lex (expr old) (expr new))
  (when (and (type-value old) (type-value new))
    (copy-lex (type-value old) (type-value new))))

(defmethod copy-lex* ((old list) (new list))
  (when old
    (copy-lex* (car old) (car new))
    (copy-lex* (cdr old) (cdr new))))

(defmethod copy-lex* (old new)
  (declare (ignore old new))
  nil)
