;;pvs2ir translates PVS expressions, definitions, and theories to an intermediate
;;representation (IR).  The IR consists of variables, tuples, function applications,
;;lambda-expressions, if-expressions, lets, and updates.

(in-package :pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defmethod judgement-types+ ((expr lambda-expr-with-type))
;;   (let ((jtypes (judgement-types expr)))
;;     (if (consp jtypes)
;; 	(if (some #'(lambda (jty) (subtype-of? jty (type expr)))
;; 		  jtypes)
;; 	    (if (some #'(lambda (jty) (subtype-of? (range jty) (return-type expr)))
;; 		      jtypes)
;; 		jtypes
;; 		(let ((ftype (mk-funtype (domain (type expr)) (return-type expr))))
;; 		  (assert (null (freevars ftype)))
;; 		  (cons ftype jtypes)))
;; 	    (if (subtype-of? (return-type expr) (range (type expr)))
;; 		(let ((ftype (mk-funtype (domain (type expr)) (return-type expr))))
;; 		  (assert (null (freevars ftype)))
;; 		  (cons ftype jtypes))
;; 		(if (subtype-of? (range (type expr)) (return-type expr))
;; 		    (cons (type expr) jtypes)
;; 		    (let ((ftype (mk-funtype (domain (type expr)) (return-type expr))))
;; 		      (cons ftype (cons (type expr) jtypes))))))
;; 	(list (type expr)))))

;; (defun mk-lambda-expr (vars expr &optional ret-type)
;;   (if ret-type
;;       (make-instance 'lambda-expr-with-type
;; 	:declared-ret-type (or (print-type rettype) ret-type)
;; 	:return-type ret-type
;; 	:bindings (mk-bindings vars)
;; 	:expression expr)
;;       (make-instance 'lambda-expr
;; 	:bindings (mk-bindings vars)
;; 	:expression expr)))

;; (defun make-lambda-expr (vars expr &optional ret-type)
;;   (let ((nexpr (mk-lambda-expr vars expr ret-type)))
;;     (assert *current-context*)
;;     (cond ((and (type expr)
;; 		(every #'type (bindings nexpr)))
;; 	   (typecheck nexpr
;; 		      :expected (mk-funtype (mapcar #'type
;; 						    (bindings nexpr))
;; 					    (or ret-type (type expr)))))
;; 	  (t (error "Types not available in make-lambda-expr")))))

;; (defun make!-lambda-expr (bindings expr &optional ret-type)
;;   (assert (every #'type bindings))
;;   (assert (type expr))
;;   (if ret-type
;;       (make-instance 'lambda-expr-with-type
;; 	:declared-ret-type (or (print-type ret-type) ret-type)
;; 	:return-type ret-type
;; 	:bindings bindings
;; 	:expression expr
;; 	:type (make-formals-funtype (list bindings) (or ret-type (type expr))))
;;       (make-instance 'lambda-expr
;; 	:bindings bindings
;; 	:expression expr
;; 	:type (make-formals-funtype (list bindings) (type expr)))))

;; (defmethod copy-slots ((ex1 lambda-expr-with-type) (ex2 lambda-expr-with-type))
;;   (call-next-method)
;;   (setf (declared-ret-type ex1) (declared-ret-type ex2)
;; 	(return-type ex1) (return-type ex2)))

;; (defun make-def-axiom (decl)
;;   (with-current-decl decl
;;     (let* ((*generate-tccs* 'none)
;; 	   (defexpr (expression* (definition decl)))
;; 	   (def (make!-lambda-exprs (formals decl) (definition decl) (type decl)))
;; 	   (res (mk-resolution decl (current-theory-name) (type decl)))
;; 	   (name (mk-name-expr (id decl) nil nil res))
;; 	   (appl (make!-equation name def))
;; 	   (depth (lambda-depth decl)))
;;       (assert (eq (declaration name) decl))
;;       (loop for i from 0 to depth
;; 	    do (push (create-definition-formula appl i)
;; 		     (def-axiom decl))))))

;; (defun make!-lambda-exprs (varslist expr &optional type)
;;   (if (null varslist)
;;       (if type
;; 	  (make!-lambda-exprs-rem expr type)
;; 	  expr)
;;       (let ((lexpr (if type
;; 		       (let ((ftype (range (find-supertype type))))
;; 			 (assert (null (freevars ftype)))
;; 			 (make!-lambda-expr (car varslist)
;; 			   (make!-lambda-exprs (cdr varslist) expr ftype)
;; 			   ftype))
;; 		       (make!-lambda-expr (car varslist)
;; 			 (make!-lambda-exprs (cdr varslist) expr)))))
;; 	(setf (place lexpr) (place expr))
;; 	lexpr)))

;; (defmethod make!-lambda-exprs-rem ((expr lambda-expr) type)
;;   (let ((ftype (range (find-supertype type))))
;;     (assert (null (freevars ftype)))
;;     (make!-lambda-expr (bindings expr)
;;       (make!-lambda-exprs-rem (expression expr) ftype)
;;       ftype)))

;; (defmethod make!-lambda-exprs-rem ((expr expr) type)
;;   expr)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcl ir-expr ()
  (ir-freevars :initform 'unbound :fetch-as 'unbound))

(defcl ir-type ())

(defcl ir-integer (ir-expr)
  ir-intval)

(defcl ir-float (ir-expr)
  ir-float-mantissa
  ir-float-exponent)

(defcl ir-double (ir-expr)
  ir-mantissa
  ir-exponent)

(defcl ir-bool (ir-expr)
  ir-boolval)

(defcl ir-variable (ir-expr)
  ir-name
  ir-vtype)

(defcl ir-function (ir-expr)
  ir-fname
  declaration);; the declaration is optional; it can be used to access other eval-info

(defcl ir-apply (ir-expr)
  ir-func
  ir-params ;both type and const actuals
  ir-args
  ir-atype);return type of the application

(defcl ir-let (ir-expr)
  ir-vartype ;an ir-variable
  ir-bind-expr
  ir-body)

(defcl ir-lett (ir-let)
  ir-bind-type);adds the type of the bound expression for casting purposes

(defcl ir-record (ir-expr)
  ir-fields
  ir-recordtype)

(defcl ir-field (ir-expr)
  ir-fieldname
  ir-value)

(defcl ir-lambda (ir-expr)
  ir-vartypes ; list of ir-types
  ir-lastvars; list of marked freevars in the lambda-expr
  ir-rangetype
  ir-body)

(defcl ir-ift (ir-expr)
  ir-condition
  ir-then
  ir-else)

;; (defcl ir-switch (ir-expr)
;;   ir-cvar
;;   ir-selections
;;   ir-switch-else)

(defcl ir-selection (ir-expr)
  ir-svalue ir-sbranch)

(defcl ir-nil (ir-expr))

(defcl ir-lookup (ir-expr);;not used
  ir-array
  ir-index)

(defcl ir-update (ir-expr)
  ir-target
  ir-lhs
  ir-rhs)

(defcl ir-const-formal (ir-variable))

(defcl ir-constructor-update (ir-update))  ;;this is for the internal update operation to preserve refcounts

(defcl ir-new (ir-expr)
  ir-size
  ir-etype)

(defcl ir-get (ir-expr)
  ir-record
  ir-field)

(defcl ir-const-actual (ir-expr)
  ir-actual-expr)

(defcl ir-type-actual (ir-type)
  ir-actual-type)

(defcl ir-actuals ()
  ir-actuals-list);;list of ir-expr or ir-type actuals

(defcl ir-typename (ir-type)
  ir-type-id
  ir-type-defn)

(defcl ir-formal-typename (ir-typename));;ir-type without ir-type-defn

(defcl ir-recordtype (ir-type)
  ir-field-types)

(defcl ir-tupletype (ir-recordtype))

(defcl ir-adt-recordtype (ir-recordtype)
  ir-field-types
  ir-constructors)

(defcl ir-adt-constructor-recordtype (ir-recordtype)
  ir-field-types
  ir-adt-name)

(defcl ir-fieldtype (ir-type)
  ir-id ir-ftype)

(defcl ir-funtype (ir-type)
  ir-domain
  ir-range)

(defcl ir-subrange (ir-type)
  ir-low ir-high)

;;NSH(1/27/16): These classes are not being used. 
;;An ADT is a name (id) and a list of constructors.  Each constructor has a name
;;and a list of accessors, and each accessor has a name and type.  
(defcl ir-adt-constructor ()
  ir-adt-constructor-id
  ir-adt-constructor-index
  ir-adt-accessors)

(defcl ir-adt-accessor ()
  ir-adt-accessor-id
  ir-adt-accessor-type)

(defcl ir-adt (ir-type)
  ir-adt-name
  ir-adt-constructors)

;;The ir-array is a class
(defcl ir-arraytype (ir-type)
  size
  elemtype);later on, we could add an offset


;;other types are char, bool, int32_t, int64_t, uint32_t, uint64_t, mpi, and mpz
;;we'll add floats later on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *free-ir-formals* nil)
(defun free-ir-formals (ir-type)
  (let ((*free-ir-formals* nil))
    (free-ir-formals* ir-type)
    *free-ir-formals*))

(defmethod free-ir-formals* ((ir-type ir-formal-typename))
  (with-slots (ir-type-id) ir-type
	      (pushnew ir-type *free-ir-formals* :test #'equalp)))

(defmethod free-ir-formals* ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
	      (free-ir-formals* ir-type-defn)))


(defmethod free-ir-formals* ((ir-type ir-recordtype))
  (with-slots (ir-field-types) ir-type
	      (free-ir-formals* ir-field-types)))

(defmethod free-ir-formals* ((ir-type ir-fieldtype))
  (with-slots (ir-ftype) ir-type
	      (free-ir-formals* ir-ftype)))

(defmethod free-ir-formals* ((ir-type ir-funtype))
  (with-slots (ir-domain ir-range) ir-type
	      (free-ir-formals* ir-domain)
	      (free-ir-formals* ir-range)))

(defmethod free-ir-formals* ((ir-type ir-arraytype))
  (with-slots (elemtype) ir-type
	      (free-ir-formals* elemtype)))

(defmethod free-ir-formals* ((ir-type ir-adt))
  (with-slots (ir-adt-constructors) ir-type
	      (free-ir-formals ir-adt-constructors)))

(defmethod free-ir-formals* ((ir-type ir-adt-constructor))
  (with-slots (ir-adt-accessors) ir-type
	      (free-ir-formals* ir-adt-accessors)))

(defmethod free-ir-formals* ((ir-type ir-adt-accessor))
  (with-slots (ir-adt-accessor-type) ir-type
	      (free-ir-formals* ir-adt-accessor-type)))

(defmethod free-ir-formals* ((ir-type list))
  (cond ((consp ir-type)
	 (free-ir-formals* (car ir-type))
	 (free-ir-formals* (cdr ir-type)))
	(t nil)))

(defmethod free-ir-formals* ((ir-type t))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;From Sam:
;;; Generates a unique id for a given typechecked decl
;;; Currently assumes the decl id is unique except for const-decls
;;; If the const-decl is unique already, simply returns it.
;;; Otherwise creates, e.g., the symbol 'c_2' for the second decl
;;; with id 'c' in the list of all declarations for the theory
;;; the decl occurs in.
;;;It's modified here to remove non-ascii symbols and to replace ? with p
;;;so as to avoid duplicates.
(defun simple-id (id)
  (intern (substitute #\p #\? (string (op-to-id id)))))

(defmacro make-c-name (module-id decl-id)
  `(intern (format nil "~a_~a" ,module-id ,decl-id)))

(defun pvs2ir-unique-decl-id (decl)
  (let ((module-id (simple-id (id (module decl))))
	(decl-id (simple-id (id decl))))
  (if (const-decl? decl)
      (let ((same-id-decls (remove-if
			       (complement #'(lambda (d)
					       (and (const-decl? d)
						    (eq (simple-id (id d)) decl-id))))
			     (all-decls (module decl)))))
	(assert (memq decl same-id-decls))
	(if (cdr same-id-decls)
	    (let ((idx (1+ (position decl same-id-decls))))
	      (make-c-name module-id (format nil "~a_~d"  decl-id idx)))
	  (make-c-name module-id decl-id)))
    (make-c-name  module-id decl-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *pvs2ir-primitives*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'TRUE nil '|booleans|)
	(mk-name 'FALSE nil '|booleans|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name '=> nil '|booleans|)
	(mk-name '<=> nil '|booleans|)
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
	(mk-name 'OR nil '|booleans|)
 	(mk-name 'NOT nil '|booleans|)
	(mk-name 'WHEN nil '|booleans|)
	(mk-name 'IFF nil '|booleans|)
	(mk-name '+ nil '|number_fields|)
	(mk-name '- nil '|number_fields|)
	(mk-name '* nil '|number_fields|)
	(mk-name '/ nil '|number_fields|)
	(mk-name '|number_field_pred| nil '|number_fields|)
	(mk-name '< nil '|reals|)
	(mk-name '<= nil '|reals|)
	(mk-name '> nil '|reals|)
	(mk-name '>= nil '|reals|)
	(mk-name '|real_pred| nil '|reals|)
	(mk-name '|integer_pred| nil '|integers|)
	(mk-name '|integer?| nil '|integers|)
	(mk-name '|rational_pred| nil '|rationals|)
	(mk-name '|floor| nil '|floor_ceil|)
	(mk-name '|ceiling| nil '|floor_ceil|)
	(mk-name '|nrem| nil '|modulo_arithmetic|)
	(mk-name '|ndiv| nil '|modulo_arithmetic|)
	(mk-name '|sqrt| nil '|sqrt|)
	(mk-name '|even?| nil '|integers|)
	(mk-name '|odd?| nil '|integers|)
	(mk-name '|restrict| nil '|restrict|)
	))
;;The operations below
;; (mk-name '|cons| nil '|list_adt|)
;; 	(mk-name '|car| nil '|list_adt|)
;; 	(mk-name '|cdr| nil '|list_adt|)
;; 	(mk-name '|cons?| nil '|list_adt|)
;; 	(mk-name '|null| nil '|list_adt|)
;;         (mk-name '|null?| nil '|list_adt|)
;;         (mk-name '|length| nil '|list_props|)
;; 	(mk-name '|member| nil '|list_props|)
;; 	(mk-name '|nth| nil '|list_props|)
;; 	(mk-name '|append| nil '|list_props|)
;; 	(mk-name '|reverse| nil '|list_props|)

(defmethod pvs2ir-primitive? ((expr name-expr))
  (member expr *pvs2ir-primitives*
	  :test #'same-primitive?))

(defmethod pvs2ir-primitive? ((expr t))
  nil)


(defparameter *ir-primitives*
  '(= /= TRUE FALSE IMPLIES => <=> NOT AND & OR NOT WHEN IFF + - * /
   number_field_pred < <= > >= real_pred integer_pred integer?
   rational_pred floor ceiling ndiv nrem sqrt even? odd? cons car cdr cons?
   null null? restrict length member nth append reverse))

(defparameter *ir-arith-primitives*
  '(+ - * / number_field_pred = < <= > >= real_pred integer_pred integer?
   rational_pred floor ceiling ndiv nrem sqrt even? odd? AND OR IMPLIES WHEN))

(defvar *var-counter* nil)
(defmacro new-irvar () ;;index this by the theory and declaration so that labels are stable
  `(intern (format nil "ivar_~a"  (funcall *var-counter*))))

(defmacro new-indvar () ;;index this by the theory and declaration so that labels are stable
  `(intern (format nil "index_~a"  (funcall *var-counter*))))

(defvar *ir-type-def-hash* (make-hash-table :test #'eq))

(defmacro new-irvars (length)
  `(loop for index from 1 to ,length collect (new-irvar)))

(defun new-irvartype (ir-type)
  (let ((ir-var (new-irvar)))
    (mk-ir-variable ir-var ir-type)))

(defun new-irvartypes (ir-types)
  (if (consp ir-types)
      (loop for irt in ir-types collect (new-irvartype irt))
    (list (new-irvartype ir-types))))

(defun mk-eval-type-info (name)
  (make-instance 'eval-type-info
		 :ir-type-name name))


(defcl constructor-eval-info (eval-info)
  ctype)

(defcl accessor-eval-info (eval-info)
  update-cdefn)

(defcl formal-const-eval-info (eval-info)
  )

(defcl ir-defn ()
  ir-function-name
  ir-args
  ir-return-type
  ir-defn)

(defcl ir-constructor-defn (ir-defn)
  ir-constructor-type)

(defcl ir-accessor-defn (ir-defn)
  ir-update-defn) ;this slot is itself an ir-defn

(defcl ir-formal-const-defn (ir-defn))

(defcl ir-last (ir-expr)
  ir-var)

(defun mk-ir-variable (ir-name ir-type)
  ;(when (not ir-type) (break "mk-ir-variable"))
  (make-instance 'ir-variable
		 :ir-name ir-name
		 :ir-vtype ir-type))

(defun mk-ir-function (fname &optional decl)
  (make-instance 'ir-function
		 :ir-fname fname
		 :declaration decl))

(defun mk-ir-const-formal (ir-name ir-type)
  ;(when (not ir-type) (break "mk-ir-variable"))
  (make-instance 'ir-const-formal
		 :ir-name ir-name
		 :ir-vtype ir-type))

(defun mk-ir-integer (intval)
  (make-instance 'ir-integer
		 :ir-intval intval))

(defun mk-ir-bool (bval)
  (make-instance 'ir-bool
		 :ir-boolval bval))

(defun mk-ir-nil ()
  (make-instance 'ir-nil))

(defun mk-ir-new (size etype)
  (make-instance 'ir-new
		 :ir-size size
		 :ir-etype etype))

(defun mk-ir-apply (function args &optional params atype)
  (make-instance 'ir-apply
		 :ir-func function
		 :ir-params params
		 :ir-args args
		 :ir-atype atype))

(defun mk-ir-let (vartype expr  body)
  (make-instance 'ir-let
		 :ir-vartype vartype
		 :ir-bind-expr expr
		 :ir-body body))

(defun mk-ir-lett (vartype bind-type expr  body)
  (make-instance 'ir-lett
		 :ir-vartype vartype
		 :ir-bind-type bind-type
		 :ir-bind-expr expr
		 :ir-body body))

(defun mk-ir-record (fields type)
  (make-instance 'ir-record
		 :ir-fields fields
		 :ir-recordtype type))

(defun mk-ir-field (fieldname value)
  (make-instance 'ir-field
		 :ir-fieldname fieldname
		 :ir-value value))

(defun mk-ir-update (target lhs rhs)
  (make-instance 'ir-update
		 :ir-target target
		 :ir-lhs lhs
		 :ir-rhs rhs))

(defun mk-ir-constructor-update (target lhs rhs)
  (make-instance 'ir-constructor-update
		 :ir-target target
		 :ir-lhs lhs
		 :ir-rhs rhs))

(defun mk-ir-lambda (vartypes rangetype body);;ir-lastvars is nil
  (make-instance 'ir-lambda
		 :ir-vartypes vartypes
		 :ir-rangetype rangetype
		 :ir-body  body))

(defun mk-ir-lambda-with-lastvars (vartypes lastvars rangetype body)
  (make-instance 'ir-lambda
		 :ir-vartypes vartypes
		 :ir-lastvars lastvars
		 :ir-rangetype rangetype
		 :ir-body  body))

(defun mk-ir-ift (condition then else)
  (make-instance 'ir-ift
		 :ir-condition condition
		 :ir-then then
		 :ir-else else))

(defun mk-ir-selection (svalue sbranch)
  (make-instance 'if-selection :ir-svalue svalue :ir-sbranch sbranch))

;; (defun mk-ir-switch (cvar selections else)
;;   (make-instance 'ir-switch
;; 		 :ir-cvar cvar
;; 		 :ir-selections selections
;; 		 :ir-switch-else else))

(defun mk-ir-lookup (array index)
  (make-instance 'ir-lookup
		 :ir-array array
		 :ir-index index))

(defun mk-ir-get (record field)
  (make-instance 'ir-get
		 :ir-record record
		 :ir-field field))

(defun mk-ir-const-actual (expr)
  (make-instance 'ir-const-actual
		 :ir-actual-expr expr))

(defun mk-ir-type-actual (type)
  (make-instance 'ir-type-actual
		 :ir-actual-type type))

(defun mk-ir-typename (id type)
  (make-instance 'ir-typename
		 :ir-type-id id
		 :ir-type-defn type))

(defun mk-ir-formal-typename (id)
  (make-instance 'ir-formal-typename
		 :ir-type-id id))



(defun mk-ir-recordtype (field-types)
  (make-instance 'ir-recordtype
		 :ir-field-types field-types))

(defun mk-ir-tupletype (field-types)
  (make-instance 'ir-tupletype
		 :ir-field-types field-types))

(defun mk-ir-adt-recordtype (field-types constructors)
  (make-instance 'ir-adt-recordtype
		 :ir-field-types field-types
		 :ir-constructors constructors))

(defun mk-ir-adt-constructor-recordtype (field-types adt-name)
  (make-instance 'ir-adt-constructor-recordtype
		 :ir-field-types field-types
		 :ir-adt-name adt-name))



(defun mk-ir-fieldtype (id type)
  (make-instance 'ir-fieldtype
		 :ir-id id
		 :ir-ftype type))

(defun mk-ir-funtype (domain range)
  (make-instance 'ir-funtype
		 :ir-domain domain
		 :ir-range range))

(defun mk-ir-subrange (low high)
  (make-instance 'ir-subrange
		 :ir-low low
		 :ir-high high))

(defun mk-ir-adt-constructor (id index accessors)
  (make-instance 'ir-adt-constructor
		 :ir-adt-constructor-id id
		 :ir-adt-constructor-index index
		 :ir-adt-accessors accessors))

(defun mk-ir-adt-accessor (id type)
  (make-instance 'ir-adt-accessor
		 :ir-adt-accessor-id id
		 :ir-adt-accessor-type type))

(defun mk-ir-adt (id constructors)
  (make-instance 'ir-adt
		 :ir-adt-name id
		 :ir-adt-constructors constructors))

(defun mk-ir-defn (id args returntype body)
  (make-instance 'ir-defn
		 :ir-function-name id
		 :ir-args args
		 :ir-return-type returntype
		 :ir-defn body))

(defun mk-ir-accessor-defn (id args returntype body update-defn)
  (make-instance 'ir-accessor-defn
		 :ir-function-name id
		 :ir-args args
		 :ir-return-type returntype
		 :ir-defn body
		 :ir-update-defn update-defn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-ir ((ir-expr ir-integer))
  (with-slots (ir-intval) ir-expr
	      ir-intval))

(defmethod print-ir ((ir-expr ir-bool))
  (with-slots (ir-boolval) ir-expr
	      (if ir-boolval 'true 'false)))

(defmethod print-ir ((ir-expr ir-variable))
  `(,(ir-name ir-expr) ,(print-ir (ir-vtype ir-expr))))

(defmethod print-ir ((ir-expr ir-apply))
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr
	      `(,(print-ir ir-func) ,@(print-ir ir-params) ,@(print-ir ir-args)  ,(print-ir ir-atype))))

(defmethod print-ir ((ir-expr ir-let))
  (with-slots (ir-vartype ir-bind-expr ir-bind-expr-type ir-body) ir-expr
	      (with-slots (ir-name ir-vtype) ir-vartype
			  `(let ,ir-name ,(print-ir ir-vtype)
				,(print-ir ir-bind-expr)
				,(print-ir ir-body)))))

(defmethod print-ir ((ir-expr ir-lett))
  (with-slots (ir-vartype ir-bind-type ir-bind-expr ir-bind-expr-type ir-body) ir-expr
	      (with-slots (ir-name ir-vtype) ir-vartype
			  `(lett ,ir-name ,(print-ir ir-vtype)
				 ,(print-ir ir-bind-type)
				,(print-ir ir-bind-expr)
				,(print-ir ir-body)))))

(defmethod print-ir ((ir-expr ir-record ))
  (with-slots (ir-fields) ir-expr
	      `(record ,(print-ir ir-fields))))

(defmethod print-ir ((ir-expr ir-field))
  (with-slots (ir-fieldname ir-value) ir-expr
	      `(= ,ir-fieldname ,(print-ir ir-value))))

(defmethod print-ir ((ir-expr ir-lambda))
  (with-slots (ir-vartypes ir-rangetype ir-body) ir-expr
  `(lambda (,@(print-ir ir-vartypes)) '-> ,(print-ir ir-rangetype) ,(print-ir ir-body))))

(defmethod print-ir ((ir-expr ir-ift))
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      `(if ,(print-ir ir-condition) ,(print-ir ir-then) ,(print-ir ir-else))))

(defmethod print-ir ((ir-expr ir-nil))
  nil)

(defmethod print-ir ((ir-expr ir-lookup))
  (with-slots (ir-array ir-index) ir-expr
	      `(lookup ,(print-ir ir-array) ,(print-ir ir-index))))

(defmethod print-ir ((ir-expr ir-update))
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      `(update ,(print-ir ir-target) ,(print-ir ir-lhs) ,(print-ir ir-rhs))))

(defmethod print-ir ((ir-expr ir-constructor-update))
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      `(update-constructor ,(print-ir ir-target) ,(print-ir ir-lhs) ,(print-ir ir-rhs))))

(defmethod print-ir ((ir-expr ir-new))
  (with-slots (ir-size ir-etype) ir-expr
	      `(new ,ir-size ,(print-ir ir-etype))))

(defmethod print-ir ((ir-expr ir-get))
  (with-slots (ir-record ir-field) ir-expr
	      `(get ,(print-ir ir-record) ,ir-field)))

(defmethod print-ir ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      ir-type-id))

(defmethod print-ir ((ir-type ir-recordtype))
  (with-slots (ir-field-types) ir-type
	      `(recordtype ,(print-ir ir-field-types))))

(defmethod print-ir ((ir-type ir-adt-recordtype))
  (with-slots (ir-field-types ir-constructors) ir-type
	      `(adt-recordtype ,(print-ir ir-field-types) :constructors ir-constructors)))

(defmethod print-ir ((ir-type ir-adt-constructor-recordtype))
  (with-slots (ir-field-types ir-adt-name) ir-type
	      `(constructor-recordtype ,(print-ir ir-field-types) :adt ir-adt-name)))

(defmethod print-ir ((ir-type ir-fieldtype))
  (with-slots (ir-id ir-ftype) ir-type 
	      `(=> ,ir-id ,(print-ir ir-ftype))))

(defmethod print-ir ((ir-type ir-funtype))
  (with-slots (ir-domain ir-range) ir-type
	      `(-> ,(print-ir ir-domain) ,(print-ir ir-range))))

(defmethod print-ir ((ir-type ir-subrange))
  (with-slots (ir-low ir-high) ir-type
	      `(subrange ,ir-low ,ir-high)))

(defmethod print-ir ((ir-expr list))
  (cond ((consp ir-expr)
	 (cons (print-ir (car ir-expr))
	       (print-ir (cdr ir-expr))))
	(t nil)))

(defmethod print-ir ((ir-expr t))
  ir-expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pvs-integer computes the integer value of a number or unary-negated number
(defun pvs-integer? (expr)
  (or (and (number-expr? expr)(number expr))
      (and (is-unary-minus? expr)(number-expr? (argument expr)) (- (number (argument expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pvs2ir (expr &optional ir-theory-tbindings context)
    (let* ((*current-context*
	    (if context context *current-context*))
	   (*current-theory* (theory *current-context*))
	   (*generate-tccs* 'none))
      (pvs2ir* expr ir-theory-tbindings)));;NSH(8/13/17): added ir-theory-tbindings


(defmethod pvs2ir* ((expr number-expr) bindings)
  (declare (ignore bindings))
  (with-slots (number) expr
	      (mk-ir-integer number)))

(defmethod pvs2ir* ((expr actual) bindings)
  (with-slots (expr type-value) expr
	      (if type-value ;then it's a type actual
		  (mk-ir-type-actual (pvs2ir-type type-value))
		(mk-ir-const-actual (pvs2ir* expr bindings)))))

(defun pvs2ir-formal-types (formals ir-actuals bindings)
  (cond ((consp formals)
	 (cond ((formal-const-decl? (car formals))(break "const?")
		(cons (pvs2ir-type (type (car formals)) bindings)
		      (pvs2ir-formal-types (cdr formals)(cdr ir-actuals) bindings)))
	       (t (break "type?")(cons *type-actual-ir-name*
			(pvs2ir-formal-types (cdr formals)(cdr ir-actuals)
					     (cons (cons (car formals)
							 (if (ir-const-formal? (car ir-actuals))
							     (car ir-actuals)
							   (ir-actual-type (car ir-actuals))))
						   bindings))))))
	(t nil)))

(defmethod pvs2ir* ((expr formal-type-decl) bindings)
  (let ((bnd (assoc expr bindings)))
    (if bnd (cdr bnd) (pvs2ir-decl* expr))))

(defmethod pvs2ir* ((expr formal-const-decl) bindings)
  (let ((bnd (assoc expr bindings)))
    (if bnd (cdr bnd)
      (pvs2ir-decls* expr))))

(defmethod pvs2ir* ((expr name-expr) bindings)
    (let* ((decl (declaration expr))
	   (bnd (assoc  decl bindings :key #'declaration)))
      (assert (not (and bnd (const-decl? decl))))
      (if bnd
	  (cdr bnd)
	(if (const-decl? decl)
	    (let* ((actuals (or (actuals (module-instance expr))
				(free-parameters expr)))
		   (ir-actuals (when actuals
				 (pvs2ir* actuals ;;needs to be fixed when handling actuals
					bindings)))
		   (ir-vars (new-irvars (length ir-actuals)))
		   (formals (formals-sans-usings (module decl)))
		   (ir-formal-types (pvs2ir-formal-types formals ir-actuals nil))
		   (ir-actuals-types (loop for actual in actuals
					   as ir-actual in ir-actuals
					   collect (unless (type-value actual)
						     (if (and (ir-const-actual? ir-actual)
							      (ir-integer? (ir-actual-expr ir-actual)))
							 (mk-ir-subrange (ir-intval (ir-actual-expr ir-actual))
									 (ir-intval (ir-actual-expr ir-actual)))
						       (pvs2ir-type (type (expr actual)) bindings)))))
		   (ir-vartypes (loop for ir-var in ir-vars
				      as ir-ft in ir-formal-types
				      collect (mk-ir-variable ir-var ir-ft)))
		   (ir-function (pvs2ir-constant expr))
		   (ir-atype (pvs2ir-type (type expr) bindings)));(break "pvs2ir*(name-expr)")
	      (if ir-actuals
		  (mk-ir-lett* ir-vartypes ir-actuals-types ir-actuals  
			      (mk-ir-apply ir-function nil ir-vartypes ir-atype));;nil args/return atype
		ir-function))
	  (break "Should not happen")))))

(defmethod pvs2ir* ((expr lambda-expr) bindings)
  (with-slots ((expr-bindings bindings) expression) expr 
	      (let* ((binding-vars (new-irvars (length expr-bindings)))
		     (ir-binds (loop for irvar in binding-vars
				  as bind in expr-bindings
				  collect (mk-ir-variable irvar (pvs2ir-type (type bind) bindings))))
		     (ir-var-bindings (pairlis expr-bindings ir-binds))
		     (ir-rangetype (pvs2ir-expr-type  expression)) ;;needs bindings?
		     (eta-form? (and (application? expression)
				     (not (name-expr? (operator expression)))
				     (eql (length expr-bindings)
					  (length (arguments expression)))
				     (loop for bvar in expr-bindings
					   as arg in (arguments expression)
					   always (and (variable? arg)
						       (same-declaration arg bvar)
						       (not (member arg (freevars (operator expression)) :test #'same-declaration)))))) 
		     (ir-expr (if eta-form?
				  (pvs2ir* (operator expression) bindings)
				(pvs2ir* expression (append ir-var-bindings bindings)))));(break "lambda")
		(if eta-form? ir-expr 
		  (mk-ir-lambda ir-binds ir-rangetype ir-expr)))))

(defmethod pvs2ir* ((expr lambda-expr-with-type) bindings)
  (with-slots ((expr-bindings bindings) expression) expr 
	      (let* ((binding-vars (new-irvars (length expr-bindings)))
		     (ir-binds (loop for irvar in binding-vars
				  as bind in expr-bindings
				  collect (mk-ir-variable irvar (pvs2ir-type (type bind) bindings))))
		     (ir-var-bindings (pairlis expr-bindings ir-binds))
		     (ir-rangetype (pvs2ir-type (return-type expr) bindings))
		     (ir-expr (pvs2ir* expression (append ir-var-bindings bindings))));(break "lambda-with")
		(mk-ir-lambda ir-binds ir-rangetype ir-expr))))

;;intersects the subranges (if any) in ir-types; returns nil, otherwise. 
(defun best-ir-subrange-list (ir-types)
  (cond ((consp ir-types)
	 (if (ir-subrange? (car ir-types))
	     (let ((cdr-subrange (best-ir-subrange-list (cdr ir-types))))
	       (if cdr-subrange
		   (intersect-subrange (car ir-types) cdr-subrange)
		 (car ir-types)))
	   (best-ir-subrange-list (cdr ir-types))))
	(t nil)))

;;intersects the subranges in type and judgement-types; returns type if there are no subranges
(defun best-ir-subrange (type judgement-types)
  (let* ((ir-type (pvs2ir-type type))
	 (ir-jtypes (loop for jtype in judgement-types collect (pvs2ir-type jtype))))
    (best-ir-subrange* ir-type ir-jtypes)))

(defun best-ir-subrange* (ir-type ir-jtypes)
  (if (ir-funtype? ir-type);;(NSH: 9/7/17): added best computation for function types
      (let* ((ir-dom (ir-domain ir-type))
	     (domain-equiv-jtypes-range (loop for ir-jtype in ir-jtypes
				       when (and (ir-funtype? ir-jtype)
						 (ir2c-tequal ir-dom
							      (ir-domain ir-jtype)))
				       collect (ir-range ir-jtype)))
	     (best-range (best-ir-subrange* (ir-range ir-type) domain-equiv-jtypes-range)))
	(mk-ir-funtype ir-dom best-range))
    (let ((ir-jsubrange (best-ir-subrange-list ir-jtypes)))
      (if ir-jsubrange
	  (if (ir-subrange? ir-type)
	      (intersect-subrange ir-type ir-jsubrange)
	    ir-jsubrange)
	ir-type))))

(defun best-ir-subrange-pair (ty1 ty2)
  (cond ((and (ir-subrange? ty1)(ir-subrange? ty2))
	 (format t "best-ir-subrange-pair(~a, ~a): ~a"
		 (print-ir ty1)(print-ir ty2)
		 (print-ir (intersect-subrange ty1 ty2)))
	 (intersect-subrange ty1 ty2))
	(t ty1)))
			       

(defmethod pvs2ir* ((expr application) bindings);(break "app")
  (with-slots (operator argument) expr
	      (let ((val (pvs-integer? expr))
		    (expr-type (type expr)))
		(or (and val 
			 (mk-ir-integer val))
		    (let ((ir-expr-type
			   (if (and (pvs2ir-primitive? operator)
				    (memq (id operator) '(+ - * /)));ensures that these are arithops
			       (best-ir-subrange expr-type (judgement-types expr))
			     (pvs2ir-type expr-type bindings))))
		      (pvs2ir-application operator (arguments expr) ir-expr-type bindings))))))

(defun mk-ir-let* (vartypes exprs body);;no need to add expr-types here
  (cond ((consp vartypes)
	 (mk-ir-let (car vartypes)(car exprs)
		    (mk-ir-let* (cdr vartypes)(cdr exprs) body)))
	(t body)))

(defun mk-ir-lett* (vartypes expr-types exprs body);;no need to add expr-types here
  (cond ((consp vartypes)
	 (if (car expr-types)
	     (mk-ir-lett (car vartypes)(car expr-types)(car exprs)
			 (mk-ir-lett* (cdr vartypes)(cdr expr-types)(cdr exprs) body))
	   (mk-ir-let (car vartypes)(car exprs)
		      (mk-ir-lett* (cdr vartypes)(cdr expr-types)(cdr exprs) body))))
	(t body)))

(defun ir-tuple-args (ir-typ)
  (if (ir-tupletype? ir-typ)
      (loop for fld in (ir-field-types ir-typ) collect (ir-ftype fld))
    (list ir-typ)))

(defun make-ir-lett (vartype expr-type expr body)
  (let ((ir-vtype (ir-vtype vartype)))
    (if (ir2c-tequal ir-vtype expr-type)
      (mk-ir-let vartype expr body)
    (if (and (ir-funtype? expr-type)
	     (ir-funtype? (ir-vtype vartype)))
	;;The range types might not match, e.g., expr returns mpz_t but range(vartype) is uint8_t
	;;we use eta-expansion on expr to get
	;;(let ev expr (let vt (let earg nuv (lambda nuv->range (ev earg)))))
	(let* ((expr-var (new-irvartype expr-type))
	       (expr-range-type (ir-range expr-type))
	       (apply-var (new-irvartype expr-range-type))
	       (expr-arg-vars (new-irvartypes (ir-tuple-args (ir-domain expr-type))))
	       (nu-vars (new-irvartypes (ir-tuple-args (ir-domain ir-vtype))))
	       (range-type (ir-range ir-vtype))
	       (return-var (new-irvartype range-type)))
	  (mk-ir-let expr-var expr 
		     (mk-ir-let vartype (mk-ir-lambda nu-vars range-type;;coerce to range type
						      (make-ir-lett apply-var expr-range-type
								    (mk-ir-let* expr-arg-vars nu-vars
										(mk-ir-apply expr-var  expr-arg-vars nil
										 expr-range-type))
								    (make-ir-lett return-var range-type
										  apply-var
										  return-var)))
				body)))
      (if (or (ir-variable? expr)(ir-last? expr))
	  (mk-ir-lett vartype expr-type expr body)
	(let* ((bind-var (new-irvar))
	       (bind-vartype (mk-ir-variable bind-var expr-type)))
	  (mk-ir-let bind-vartype expr
		     (mk-ir-lett vartype expr-type (mk-ir-last bind-vartype) body))))))))
  

(defun make-ir-lett* (vartypes expr-types exprs body);;more sophisticated version of mk-ir-lett*
  (cond ((consp vartypes)
	 (make-ir-lett (car vartypes) (car expr-types) (car exprs)
		       (make-ir-lett* (cdr vartypes)(cdr expr-types)(cdr exprs) body)))
	(t body)))

(defun adt-decl? (decl)
  (or (adt-constructor-decl? decl)
      (adt-accessor-decl? decl)
      (adt-recognizer-decl? decl)))

(defun pvs2ir-decl (decl)
  (let* ((*current-context* (decl-context decl))
	 (*current-theory* (theory *current-context*))
	 (*generate-tccs* 'none))
    ;(unless (decl-formals decl))
    (pvs2ir-decl* decl)))

(defun copy-without-print-type (type)
  (copy type 'print-type nil))

(defmethod pvs2ir-decl* ((decl type-eq-decl))
  (let ((ir-type-value (ir-type-value decl)))
    (if ir-type-value
	(ir-type-name ir-type-value)
      (unless (formals decl)
	(let ((ir-type (pvs2ir-type (copy-without-print-type (type-value decl))))) ;;loops without copy
	  (if (or (ir-typename? ir-type)
		  (ir-subrange? ir-type))
	      ir-type ;;ir-type might not be a typename
	    (let ((ir-type-name (mk-ir-typename (pvs2ir-unique-decl-id decl) ir-type)))
	      (push ir-type-name *ir-type-info-table*)
	      (setf (ir-type-value decl)
		    (mk-eval-type-info ir-type-name))
	      (ir-type-name (ir-type-value decl)))))))))

(defmethod pvs2ir-decl* ((decl type-decl))
  (and (or (ir-type-value decl)
	   (let ((type-value (type-value decl)))
	     (if (and type-value
		      (adt type-value))
		 (pvs2ir-adt-decl decl)
	       (let ((ir-type-name (mk-ir-typename (pvs2ir-unique-decl-id decl) nil)))
		 (push ir-type-name *ir-type-info-table*)
		 (setf (ir-type-value decl)
		       (mk-eval-type-info ir-type-name))))))  ;;Should check that the ir-type-value slot is set
       (ir-type-name (ir-type-value decl))))

(defmethod pvs2ir-decl* ((decl formal-type-decl))
  (let ((ir-type-name (mk-ir-formal-typename (pvs2ir-unique-decl-id decl))))
    (push ir-type-name *ir-type-info-table*)
    (setf (ir-type-value decl)
	  (mk-eval-type-info ir-type-name))
    ir-type-name))

(defmethod pvs2ir-decl* ((decl formal-const-decl))
  (let* ((einfo (eval-info decl))
	 (einfo (or einfo
		    (let ((new-einfo (make-instance 'formal-const-eval-info)))
		      (setf (eval-info decl) new-einfo)
		      new-einfo))))
    (setf (ir einfo) (make-instance 'ir-formal-const-defn))
    (setf (ir-function-name (ir einfo)) (mk-ir-function (id decl) decl))
    (setf (ir-defn (ir einfo)) (mk-ir-const-formal (id decl) (pvs2ir-type (type decl))))
    (id decl)))
	    


(defmethod pvs2ir-decl* ((decl const-decl))
  (let* ((einfo (eval-info decl))
	 (einfo (or einfo
		    (let ((new-einfo (make-instance 'eval-info)))
		      (setf (eval-info decl) new-einfo)
		      new-einfo))))
    (let* ((ir-einfo (ir einfo))
	   (ir-function-name (when ir-einfo (ir-function-name ir-einfo))))
      (or ir-function-name
	  (let* ((defns (def-axiom decl))
		 (defn (if defns (args2 (car (last defns)))
			 ;special treatment for rem operation 
			 (when (and (eq (id decl) 'rem)
				    (eq (id (module decl)) '|modulo_arithmetic|))
			   (let ((remnrem0-decls (get-decls "rem_nrem0")))
			     (loop for dd in remnrem0-decls
				   when (eq (id (module dd)) '|modulo_arithmetic|)
				   return (args2 (definition dd))))))))
	    (unless ir-einfo ;first create eval-info then fill the function name
		  (setf (ir einfo)
			(make-instance 'ir-defn)))
	    (setf (ir-function-name (ir einfo))
		  (mk-ir-function (pvs2ir-unique-decl-id decl)
				  decl))
		;;create the ir for the definition
					;(newcounter *var-counter*)
;	    (break "pvs2ir-decl* (const-decl)")
	    (let* ((context (decl-context decl))
		   (formals (formals decl))
		   (type (find-supertype (type decl)))
		   (ir-defn (when defn (pvs2ir defn *ir-theory-tbindings* context)))
		   (ir-defn-with-tformals
		    (when defn
		      (if *ir-theory-formals*
			  (cond ((ir-lambda? ir-defn)
				 (setf (ir-vartypes ir-defn)
				       (nconc *ir-theory-formals* (ir-vartypes ir-defn)))
				 ir-defn)
				(t (mk-ir-lambda *ir-theory-formals* (pvs2ir-type (type decl))
						 ir-defn)))
			ir-defn)))
		     (args (if (and (null defn) formals)
			       (let* ((topformals (car formals))
				      (binding-vars (new-irvars (length topformals)))
				      (ir-formals (loop for irvar in binding-vars
							as var in topformals
							collect (mk-ir-variable irvar (pvs2ir-type (type var) nil)))))
				 ir-formals)
			     (when (ir-lambda? ir-defn-with-tformals)
			       (ir-vartypes ir-defn-with-tformals))))
		     (returntype (if (and (null defn) formals)
				     (ir-range (pvs2ir-type (type decl)))
				   (or (and (ir-lambda? ir-defn-with-tformals)
					    (ir-rangetype ir-defn-with-tformals))
				       (pvs2ir-type (type decl)))))
		     (body (if (ir-lambda? ir-defn-with-tformals)
			       (ir-body ir-defn-with-tformals)
			     ir-defn-with-tformals)))
	      ;(break "pvs2ir-decl2")
		(format t "~%ir-theory-formals = ~{~a, ~}" *ir-theory-formals*)
		(format t "~% pvs2ir-decl*, ir-defn = ~a" (print-ir ir-defn-with-tformals))
		(setf (ir-args (ir einfo)) args
		      (ir-return-type (ir einfo)) returntype
		      (ir-defn (ir einfo)) body)
		(ir-function-name (ir einfo))))))))

(defun pvs2ir-constant (expr)
  (let ((decl (declaration expr)))
    (cond ((pvs2ir-primitive? expr) ;;borrowed from pvseval-update.lisp
	   (cond ((eq (id expr) 'TRUE) (mk-ir-bool t))
		 ((eq (id expr) 'FALSE) (mk-ir-bool nil))
		 (t (mk-ir-function (id expr) decl))));for primitives, types are derived from args
	  (t 
	   (if (adt-decl? decl)
	       (let ((adt (adt expr)))
		 (pvs2ir-adt adt)
		 (ir-function-name (ir (eval-info decl))))
	     (progn (pvs2ir-decl decl)
		    (ir-function-name (ir (eval-info decl)))))))))

(defun pvs2ir-adt (adt)
  (let* ((adt-decl (declaration adt)));(break "adt")
    (unless (and (ir-type-value adt-decl)(ir-type-name (ir-type-value adt-decl)))
	(pvs2ir-adt-decl adt-decl))))

(defun pvs2ir-adt-decl (adt-decl);;only called with ir-type-value is empty
  (let* ((adt-type (type-value adt-decl))
	 (adt-type-id (pvs2ir-unique-decl-id adt-decl))
	 (adt-adt-id (format nil "~a_adt" adt-type-id))
	 (adt (adt adt-type))
	 (constructors (constructors adt))
	 (index-id (intern (format nil "~a_index" adt-adt-id)))
	 (index-type (mk-ir-subrange 0 (1- (length constructors))))
	 (adt-enum-or-record-type
	  (if (enumtype? adt)
	      index-type
	    (mk-ir-adt-recordtype (list (mk-ir-fieldtype index-id
							 index-type))
				  (loop for con in constructors
					collect (cons (pvs2ir-unique-decl-id (con-decl con))
						      (loop for acc in (acc-decls con)
							    collect (pvs2ir-unique-decl-id acc)))))))
	 (adt-type-name (mk-ir-typename adt-adt-id adt-enum-or-record-type)))
    (push adt-type-name *ir-type-info-table*)
    (setf (ir-type-value adt-decl)
	  (mk-eval-type-info adt-type-name))
    (loop for constructor in constructors
	  as index from 0
	  do (pvs2ir-adt-constructor constructor index index-id index-type
				     adt-enum-or-record-type adt-type-name))
					;(break "before accessor")
    (unless (enumtype? adt)
      (loop for constructor in constructors
	    as index from 0
	    do (pvs2ir-adt-accessors (acc-decls constructor) constructor constructors
				     index index-id 
				     adt-type-name)))
    adt-type-name
    ))

(defun pvs2ir-adt-constructor
    (constructor index index-id index-type adt-enum-or-record-type adt-type-name)
  (if (ir-subrange? adt-enum-or-record-type)
      (pvs2ir-adt-enum-constructor constructor index index-id
				   index-type adt-enum-or-record-type adt-type-name)
    (pvs2ir-adt-record-constructor constructor index index-id
				   index-type adt-enum-or-record-type adt-type-name)))

(defun pvs2ir-adt-enum-constructor
    (constructor index index-id index-type adt-enum-type adt-type-name)
  (let* ((cdecl (con-decl constructor))
	 ;(args (arguments constructor))
	 (cid (pvs2ir-unique-decl-id cdecl))
	 (einfo (get-eval-info cdecl)))
    (unless (ir einfo)
      (setf (ir einfo) (make-instance 'ir-constructor-defn))
      (setf (ir-constructor-type (ir einfo)) adt-type-name)
      (setf (ir-function-name (ir einfo))
	    (mk-ir-function cid cdecl)
	    (ir-defn (ir einfo))
	    (mk-ir-integer index))
      (pvs2ir-adt-enum-recognizer (rec-decl constructor) index index-id index-type
				   adt-type-name))))

	 
(defun pvs2ir-adt-record-constructor (constructor index index-id index-type adt-recordtype adt-type-name)
  (let* ((cdecl (con-decl constructor))
	 (args (arguments constructor))
	 (cid (pvs2ir-unique-decl-id cdecl))
	 (einfo (get-eval-info cdecl))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    (unless (ir einfo);(break "constructor")
      (setf (ir einfo) (make-instance 'ir-constructor-defn))
      (let* ((indexvar (mk-ir-variable (new-irvar) index-type))
	     (cargs (loop for arg in args
			  collect (mk-ir-variable (new-irvar);;range is the same for shared accessors
						  (pvs2ir-type (type arg))))) ;(declared-type arg)
	     (cbody-fields (cons (mk-ir-field index-id indexvar)
				   (loop for arg in args
					 as carg in cargs
					 collect (mk-ir-field (id arg) carg))))
	     (accessor-types (loop for arg in args
					      as carg in cargs 
					      collect (mk-ir-fieldtype (id arg)
								       (ir-vtype carg))))
	     (cbody-field-types (append (ir-field-types adt-recordtype);;add the index field
					accessor-types))
	     (cbody-recordtype (if args
				   (mk-ir-adt-constructor-recordtype cbody-field-types adt-type-name)
				 adt-recordtype));retain adt type for nullary constructors
	     (ctypename (if args (mk-ir-typename cid cbody-recordtype) adt-type-name))
	     (cvar (mk-ir-variable (new-irvar) ctypename))
	     (cbody-record (mk-ir-record cbody-fields cbody-recordtype))
	     (cbody (mk-ir-let indexvar (mk-ir-integer index)
			       (mk-ir-let cvar cbody-record cvar))));(break "c2")
	(setf (ir-constructor-type (ir einfo)) ctypename);(break "-constructor")
	(setf (ir-function-name (ir einfo))
	      (mk-ir-function cid cdecl)
	      (ir-args (ir einfo)) cargs
	      (ir-return-type (ir einfo)) adt-type-name
	      (ir-defn (ir einfo)) cbody)
	(pvs2ir-adt-record-recognizer (rec-decl constructor) index index-id index-type
			        adt-type-name)
	))))

(defun pvs2ir-adt-accessors (acc-decls constructor constructors
				       index index-id 
				       adt-type-name)
  (cond ((consp acc-decls)
	 (and (pvs2ir-adt-accessor* (car acc-decls) constructor constructors
				   index index-id adt-type-name)
	      (pvs2ir-adt-accessors (cdr acc-decls) constructor constructors
				   index index-id adt-type-name)))
	(t nil)))

(defun get-eval-info (declaration)
  (or (eval-info declaration)
      (let ((new-einfo (if (adt-constructor-decl? declaration)
			   (make-instance 'constructor-eval-info)
			 (if (adt-accessor-decl? declaration)
			     (make-instance 'accessor-eval-info)
			   (make-instance 'eval-info)))))
	(setf (eval-info declaration) new-einfo)
	new-einfo)))

(defun pvs2ir-adt-enum-recognizer (rdecl index index-id index-type 
				    adt-type-name)
  (let* ((einfo (get-eval-info rdecl))
	 (ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    (or ir-einfo
	(let* ((rid (pvs2ir-unique-decl-id rdecl))
	       (rarg  (mk-ir-variable (new-irvar) adt-type-name))
	       (index-var (mk-ir-variable (new-irvar) index-type))
	       (check-expr (mk-ir-apply (mk-ir-function '=) (list rarg index-var)))
	       (rbody (mk-ir-let index-var (mk-ir-integer index)
				 check-expr))
	       (recognizer-name (mk-ir-function (intern (format nil "r_~a" rid)) rdecl)))
	  (setf (ir einfo)(mk-ir-defn recognizer-name
				      (list rarg) 'bool rbody))
	  (ir einfo)))))



(defun pvs2ir-adt-record-recognizer (rdecl index index-id index-type 
				    adt-type-name)
  (let* ((einfo (get-eval-info rdecl))
	 (ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    (or ir-einfo
	(let* ((rid (pvs2ir-unique-decl-id rdecl))
	       (rargs (list (mk-ir-variable (new-irvar) adt-type-name)))
	       (index-expr (mk-ir-get (car rargs) index-id))
	       (index-expr-var (mk-ir-variable (new-irvar) index-type))
	       (index-var (mk-ir-variable (new-irvar) index-type))
	       (check-expr (mk-ir-apply (mk-ir-function '=) (list index-expr-var index-var)))
	       (rbody (mk-ir-let index-var (mk-ir-integer index)
				 (mk-ir-let index-expr-var index-expr
					    check-expr)))
	       (recognizer-name (mk-ir-function (intern (format nil "r_~a" rid)) rdecl))
	       (rdefn (mk-ir-lambda rargs 'bool rbody)));(break "recognizer")
	  (setf (ir einfo)(mk-ir-defn recognizer-name rargs 'bool rbody))
	  (ir einfo)))))



(defmethod pvs2ir-adt-accessor* ((adecl adt-accessor-decl)
				 constructor constructors index index-id adt-type-name)
   (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 (ir-einfo (ir einfo))
	 (cinfo (get-eval-info (con-decl constructor)))
	 (ir-cinfo (ir cinfo))
	 (ctype (ir-constructor-type ir-cinfo))
	 (*var-counter* nil));(break "accessor*")
    (newcounter *var-counter*)
    (or (ir einfo)
	(let* ((aid (pvs2ir-unique-decl-id adecl))
	       (aargvar (mk-ir-variable (new-irvar) adt-type-name))
	       (accessor-ir-type (pvs2ir-type (range (type adecl))))
	       (cast-var (mk-ir-variable (new-irvar) ctype))
	       (project-expr (mk-ir-get cast-var adecl-id))
	       (abody (mk-ir-let cast-var aargvar
				 project-expr))
;	       (adefn (mk-ir-lambda (list aargvar) accessor-ir-type abody))
	       (new-value-var (mk-ir-variable (new-irvar) accessor-ir-type))
	       (accessor-name (mk-ir-function (format nil "~a_~a" (ir-type-id adt-type-name) adecl-id) adecl))
	       (update-name (mk-ir-function (format nil "update_~a_~a" (ir-type-id adt-type-name) adecl-id) adecl))
	       (ubody (mk-ir-let cast-var aargvar (mk-ir-constructor-update cast-var adecl-id new-value-var))))
	  (setf (ir einfo)
		(mk-ir-accessor-defn accessor-name (list aargvar) accessor-ir-type abody
				     (mk-ir-defn update-name (list aargvar new-value-var) ctype ubody)))
	  (format t "~%Adding definition for singular accessor: ~a" adecl-id)
	  (ir einfo)))))
	  
	  ;; (setf (ir einfo)(make-instance 'ir-accessor-defn)
	  ;; 	(ir-update-defn (ir einfo)) (make-instance 'ir-defn))

	  ;; (setf (ir-function-name (ir einfo))
	  ;; 	(mk-ir-function (format nil "~a_~a" (ir-type-id adt-type-name) adecl-id) adecl)
	  ;; 	(ir-defn (ir einfo))
	  ;; 	adefn
	  ;; 	(ir-function-name (ir-update-defn (ir einfo)))
	  ;; 	(mk-ir-function (format nil "update_~a_~a" (ir-type-id adt-type-name) adecl-id)
	  ;; 			adecl)
	  ;; 	(ir-defn (ir-update-defn (ir einfo)))
	  ;; 	(mk-ir-lambda (list aargvar new-value-var) ctype ubody)
	  ;; 	)))))

(defmethod pvs2ir-adt-accessor* ((adecl shared-adt-accessor-decl)
				 constructor constructors index index-id adt-type-name)
  (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 (ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    (or ir-einfo
	(let* ((acc-constructor-ids (constructors adecl))
	       (acc-constructor-index-decls (loop for cnstr in constructors
						  as idx from 0 
						  when (memq (id cnstr) acc-constructor-ids)
						  collect (cons idx (con-decl cnstr))))
	       (aid (pvs2ir-unique-decl-id adecl))
	       (aargvar (mk-ir-variable (new-irvar) adt-type-name))
	       (accessor-ir-type (pvs2ir-type (range (type adecl))))
	       (abody (pvs2ir-accessor-body adecl-id aargvar acc-constructor-index-decls index-id))
	       (new-value-var (mk-ir-variable (new-irvar) accessor-ir-type))
	       (accessor-name (mk-ir-function (format nil "~a_~a" (ir-type-id adt-type-name) adecl-id) adecl))
	       (update-name (mk-ir-function
			     (format nil "update_~a_~a" (ir-type-id adt-type-name) adecl-id)
			     adecl))
	       (ubody (pvs2ir-accessor-update-body adecl-id aargvar new-value-var
						   acc-constructor-index-decls index-id)))
	  (setf (ir einfo)
		(mk-ir-accessor-defn accessor-name (list aargvar) accessor-ir-type abody
				     (mk-ir-defn update-name (list aargvar new-value-var) adt-type-name ubody)))
	  (format t "~%Adding definition for shared accessor: ~a" adecl-id)
	  (ir einfo)))))


	  ;; (setf (ir-function-name (ir einfo))
	  ;; 	(mk-ir-function (format nil "~a_~a" (ir-type-id adt-type-name) adecl-id) adecl)
	  ;; 	(ir-defn (ir einfo))
	  ;; 	(mk-ir-lambda (list aargvar) accessor-ir-type abody)
	  ;; 	(ir-function-name (ir-update-defn (ir einfo)))
	  ;; 	(mk-ir-function
	  ;; 	 (format nil "update_~a_~a" (ir-type-id adt-type-name) adecl-id)
	  ;; 	 adecl)
	  ;; 	(ir-defn (ir-update-defn (ir einfo))) ;no unique constructor so return type below is adt-type-name
	  ;; 	(mk-ir-lambda (list aargvar new-value-var) adt-type-name ubody))))))
	       
	 ;; (ctypes (map #'id acc-constructors))
	 ;; (cinfo (get-eval-info (declaration constructor)))
	 ;; (ir-cinfo (ir cinfo))
	 ;; (ctype (ir-function-name ir-cinfo)))))))

;;pvs2ir-accessor-body builds the body of a multi-constructor accessor for the given accessor
(defun pvs2ir-accessor-body (adecl-id aargvar acc-constructor-index-decls index-id)
  ;(break "pvs2ir-accessor-body")
    (cond ((consp  acc-constructor-index-decls)
	   (let* ((cindex (caar acc-constructor-index-decls))
		  (cdecl (cdar acc-constructor-index-decls))
		  (cbranch (let ((cast-var (mk-ir-variable (new-irvar)
							   (ir-constructor-type (ir (get-eval-info cdecl))))))
			     (mk-ir-let cast-var  aargvar (mk-ir-get  cast-var adecl-id)))));(break "acc-body")
	     (if (consp (cdr acc-constructor-index-decls))
		 (let ((condvar (mk-ir-variable (new-irvar) 'bool))
		       (indvar (mk-ir-variable (new-irvar) 'uint32))
		       (intvar (mk-ir-variable (new-irvar) 'uint32)))
		   (mk-ir-let condvar
			      (mk-ir-let indvar
					 (mk-ir-get aargvar index-id)
					 (mk-ir-let intvar (mk-ir-integer cindex)
						    (mk-ir-apply (mk-ir-function '=) (list indvar intvar))))
			      (mk-ir-ift condvar
					 cbranch
					 (pvs2ir-accessor-body adecl-id aargvar (cdr acc-constructor-index-decls) index-id))))
	       cbranch)))
	  (t (format t "Shouldn't reach here."))))

;;pvs2ir-accessor-update-body builds the body of a multi-constructor update for the given accessor
(defun pvs2ir-accessor-update-body (adecl-id aargvar new-value-var
					     acc-constructor-index-decls index-id)
    (cond ((consp  acc-constructor-index-decls)
	   (let* ((cindex (caar acc-constructor-index-decls))
		  (cdecl (cdar acc-constructor-index-decls))
		  (cbranch (let ((cast-var (mk-ir-variable (new-irvar)
							   (ir-constructor-type (ir (get-eval-info cdecl))))))
			     (mk-ir-let cast-var  aargvar
					(mk-ir-constructor-update cast-var adecl-id new-value-var)))));(break "acc-body")
	     (if (consp (cdr acc-constructor-index-decls))
		 (let ((condvar (mk-ir-variable (new-irvar) 'bool))
		       (indvar (mk-ir-variable (new-irvar) 'uint32))
		       (intvar (mk-ir-variable (new-irvar) 'uint32)))
		   (mk-ir-let condvar
			      (mk-ir-let indvar
					 (mk-ir-get aargvar index-id)
					 (mk-ir-let intvar (mk-ir-integer cindex)
						    (mk-ir-apply (mk-ir-function '=) (list indvar intvar))))
			      (mk-ir-ift condvar
					 cbranch
					 (pvs2ir-accessor-update-body adecl-id aargvar new-value-var (cdr acc-constructor-index-decls) index-id))))
	       cbranch)))
	  (t (format t "Shouldn't reach here."))))

(defmethod pvs2ir-constructor-recordtype ((constructor ir-adt-constructor))
  (let ((index-fieldtype (mk-ir-fieldtype (intern "_index")
					  (mk-ir-subrange (ir-adt-constructor-index constructor)
							  (ir-adt-constructor-index constructor))))
	(rest-fieldtypes (loop for accessor in (ir-adt-accessors constructor)
			       collect (mk-ir-fieldtype (ir-adt-accessor-id accessor)
							 (ir-adt-accessor-type accessor)))))
    (mk-ir-recordtype (cons index-fieldtype rest-fieldtypes))))

(defmethod ir-formal-id ((ir-formal ir-formal-typename))
  (with-slots (ir-type-id) ir-formal
	ir-type-id))

(defmethod ir-formal-id ((ir-formal ir-const-formal))
  (with-slots (ir-name) ir-formal
	ir-name))

(defparameter *type-actual-ir-name* (mk-ir-typename 'type_actual nil))

(defmethod ir-formal-type ((ir-formal ir-formal-typename))
  *type-actual-ir-name*)

(defmethod ir-formal-type ((ir-formal ir-const-formal))
  (with-slots (ir-vtype) ir-formal
	ir-vtype))

(defun ir-apply-op-type (pvsop argtypes atype)
  (if (pvs2ir-primitive? pvsop)
      (if (or (eq (car argtypes) 'mpq)
	      (eq (cadr argtypes) 'mpq))
	  (or atype 'mpq)
	(let ((new-subrange-type
	       (case (id pvsop)
		 (+ (plus-subrange  (car argtypes)
				    (cadr argtypes)))
		 (- (if (and (< (length argtypes) 2)
			     (ir-subrange? (car argtypes)))
			(negate-subrange (car argtypes))
		      (minus-subrange (car argtypes)
				      (cadr argtypes))))
		 (* (times-subrange (car argtypes)
				    (cadr argtypes)))
		 (t))))
	  (format t "~%new-subrange-type : ~a" (print-ir new-subrange-type))
	  (if new-subrange-type
	      (if atype
		  (intersect-subrange new-subrange-type atype)
		new-subrange-type)
	    atype)))
    atype))

(defmethod get-ir-domain-types ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
    (get-ir-domain-types ir-type-defn)))

(defmethod get-ir-domain-types ((ir-type ir-funtype))
  (with-slots (ir-domain) ir-type
    (if (ir-recordtype? ir-domain)
	(loop for irft in (ir-field-types ir-domain)
	      collect (ir-ftype irft))
      (list ir-domain))))

(defun pvs2ir-application (op args ir-expr-type bindings)
  ;(format t "pvs2ir-application")
  (let* ((arg-names (new-irvars (length args)))
					;(dummy (format t "arg-types"))
	 (args-ir (pvs2ir* args bindings))
	 (arg-types 
	  (loop for arg in args
		as ir-arg in args-ir
		collect (let ((num (pvs-integer? arg)))
			  (if num
			      (mk-ir-subrange num num)
			    (best-ir-subrange-pair (pvs2ir-expr-type arg)
						   (ir-arith-type ir-arg))))));;NSH(3-20-16):
	 (op-arg-types arg-types) ;;NSH(3-22-18)
	 ;; (if (pvs2ir-primitive? op)
	 ;; 		   arg-types
	 ;; 		   (loop for type in (types (domain (find-supertype (type op))))
	 ;; 			 collect (pvs2ir-type type)))
	 (ir-expr-type (ir-apply-op-type op op-arg-types ir-expr-type))
	 ;(op-range-type (pvs2ir-type (range (find-supertype (type op)))))
	 (apply-return-var (new-irvartype ir-expr-type))
	 ;(dummy (format t "arg-vartypes"))
	 ;; (arg-vartypes 
	 ;; 	       (loop for ir-var in arg-names
	 ;; 		     as ir-typ in op-arg-types
	 ;; 		     collect (mk-ir-variable ir-var ir-typ)))
	 );(break "pvs2ir-application")
    ;(format t "~%op: ~a: ~{ ~a~}" (when (constant? op)(id op))(print-ir op-arg-types))
    ;; (when (and (constant? op)(eq (id op) 'transpose_step)) (break "pvs2ir-application"))
    (if (constant? op)
	(if (pvs2ir-primitive? op)
	    (let ((arg-vartypes (loop for ir-var in arg-names
				      as ir-typ in op-arg-types
				      collect (mk-ir-variable ir-var ir-typ))))
	      (mk-ir-let* arg-vartypes
			  args-ir
			  (mk-ir-apply (pvs2ir-constant op) arg-vartypes nil ir-expr-type)))
	  (let* ((actuals (actuals (module-instance op)));;handling theory actuals
		 (formals (formals-sans-usings (module (declaration op))))
		 (ir-formals (pvs2ir* formals bindings))
		 (ir-actuals (pvs2ir* actuals bindings))
		 (actvars
		  (loop for fml in ir-formals
			collect (mk-ir-variable (new-irvar)(ir-formal-type fml))))
		 (actual-types
		  (loop for actual in ir-actuals
			as formal in formals
			collect (if (formal-const-decl? formal)
				    (pvs2ir-type (type actual))
				  *type-actual-ir-name*)))
		 (op-domain (types (domain (type op))))
		 (op-domain-vars (loop for ty in op-domain
				       collect (mk-ir-variable (new-irvar) (pvs2ir-type ty))))
		 (op-ir-function (pvs2ir-constant op))
		 (op-ir (ir (eval-info (declaration op))))
		 (op-ir-defn (ir-defn op-ir))
		 (op-ir-args (ir-args op-ir))
		 );(when (and op-ir-defn args-ir (null op-ir-args)) (break "pvs2ir-app"))
	    (if formals
		(make-ir-lett* actvars actual-types ir-actuals
			       (make-ir-lett* op-domain-vars ; was arg-vartypes
					      arg-types
					      args-ir
					      (mk-ir-let apply-return-var ;op-range-type
							 (mk-ir-apply
							  op-ir-function op-domain-vars
							  actvars)
							  ;op-range-type
							 apply-return-var)))
	      (if (and op-ir-defn  args-ir (null op-ir-args))
		  (let ((op-var (mk-ir-variable (new-irvar)(pvs2ir-type (type op)))))
		    ;(break "pvs2ir-application")
		    (make-ir-lett* op-domain-vars ;was arg-vartypes
				   arg-types
				   args-ir
				   (mk-ir-let op-var
					      op-ir-function
					      (mk-ir-let apply-return-var ;op-range-type
							 (mk-ir-apply op-var op-domain-vars nil) ;op-range-type
							 apply-return-var))))
		(make-ir-lett* op-domain-vars ;was arg-vartypes
			       arg-types
			       args-ir
			       (mk-ir-let apply-return-var ;op-range-type
					  (mk-ir-apply op-ir-function op-domain-vars nil) ;op-range-type
					  apply-return-var))))))
      (let* ((op-ir-type (pvs2ir-type (type op)))
	     (op-var (new-irvartype op-ir-type))
	     (op-ir (pvs2ir* op bindings))
	     (arg-vartypes (if (or (ir-array? op-ir-type)
				   (ir-lambda? op-ir))
			       (loop for ir-var in arg-names
				     as ir-typ in op-arg-types
				     collect (mk-ir-variable ir-var ir-typ))
			       (loop for irft in (get-ir-domain-types op-ir-type)
				     collect (mk-ir-variable (new-irvar) irft)))))
	(if (ir-array? op-ir-type)
	    (mk-ir-let op-var op-ir
		       (mk-ir-let (car arg-vartypes)(car args-ir)
				  (mk-ir-let apply-return-var ;op-range-type
						(mk-ir-lookup op-var (car arg-vartypes))
					     apply-return-var)))
	   (if (ir-lambda? op-ir);;op-var is ignored, IR is beta-reduced
	       (with-slots (ir-vartypes ir-body) op-ir
			   (mk-ir-let* arg-vartypes args-ir
				       (if (eql (length ir-vartypes)(length arg-vartypes))
					   (mk-ir-let* ir-vartypes arg-vartypes
						       ir-body)
					 (if (eql (length arg-vartypes) 1)
					     (let ((ir-projected-args
						    (loop for ir-vartype in ir-vartypes
							  as i from 1
							  collect (mk-ir-get (car arg-vartypes)
									     (intern (format nil "project_~a" i))))))

					       (mk-ir-let* ir-vartypes ir-projected-args ir-body))
					   (let* ((ir-fields (loop for ir-vartype in arg-vartypes
								   as i from 1 
								   collect
								   (mk-ir-field (intern (format nil "project_~a" i))
										ir-vartype)))
						  (ir-recordtype (ir-vtype (car ir-vartypes))))
					     (mk-ir-let (car ir-vartypes) (mk-ir-record ir-fields ir-recordtype)
							ir-body))))))
	     (mk-ir-let op-var op-ir 
		     (make-ir-lett* arg-vartypes
				  arg-types
				  args-ir
				  (mk-ir-let apply-return-var ;op-range-type
						(mk-ir-apply op-var arg-vartypes nil) ;op-range-type
						apply-return-var)))))))))
	  
				 ;; (if (eql (length arg-vartypes) 1)
				 ;;     (mk-ir-apply op-var arg-vartypes)
				 ;;   (let* ((ir-fields (loop for ir-vartype in arg-vartypes
				 ;; 			   as i from 1 
				 ;; 			   collect
				 ;; 			   (mk-ir-field (intern (format nil "project_~a" i))
				 ;; 					ir-vartype)))
				 ;; 	  (ir-recordtype (mk-ir-tupletype
				 ;; 			  (loop for ir-vartype in arg-vartypes
				 ;; 				as i from 1
				 ;; 				collect (mk-ir-fieldtype (intern (format nil "project_~a" i))
				 ;; 							 (ir-vtype ir-vartype)))))
				 ;; 	  (ir-recordvar (new-irvartype ir-recordtype)))
				 ;;     (mk-ir-let ir-recordvar (mk-ir-record ir-fields ir-recordtype)
				 ;; 		(mk-ir-apply op-var (list ir-recordvar)))))))))))))

(defmethod pvs2ir* ((expr list) bindings)
  (cond ((consp expr)
	 (cons (pvs2ir* (car expr) bindings)
	       (pvs2ir* (cdr expr) bindings)))
	(t nil)))

(defmethod pvs2ir* ((cexpr cases-expr) bindings)
  (with-slots (expression selections else-part) cexpr
	      (pvs2ir-cases expression selections else-part bindings)))

(defun pvs2ir-cases (expression selections else-part bindings)
  (cond ((consp selections)
	 (let* ((sel (car selections))
		(selexpr (expression sel))
		(stype (find-declared-adt-supertype (type selexpr)))
		(dec-stype (declaration stype))
		(modinst (module-instance stype))
		(accs (subst-mod-params (accessors (constructor sel))
					modinst
					(module dec-stype)
					dec-stype))
		(accs-exprs (loop for acc in accs collect
				  (make!-application acc expression)))
		(arg-ir-vars (new-irvars (length (args sel))))
		(args-ir-vtypes (loop for ir-var in arg-ir-vars
				      as var in (args sel)
				      collect (mk-ir-variable ir-var (pvs2ir-type (type var)))))
		(ir-accs (pvs2ir* accs-exprs bindings))
		(branch (mk-ir-let* args-ir-vtypes
				    ir-accs
				    (pvs2ir* selexpr
					     (nconc (pairlis (args sel) args-ir-vtypes) bindings)))))
	   (if (and (null (cdr selections))(null else-part))
	       branch
	     (let ((condvar (mk-ir-variable (new-irvar) 'bool)))
	       (mk-ir-let condvar
			  (pvs2ir* (make!-application (recognizer (constructor sel))
						    expression)
				 bindings)
			(mk-ir-ift  condvar
			    branch
			    (pvs2ir-cases expression (cdr selections) else-part bindings)))))))
	(t (pvs2ir* else-part bindings))))
		
;;  (pvs2ir* (translate-cases-to-if cexpr) bindings))

;;partial 
  ;; (with-slots (expr selections else-part) cexpr
  ;; 	      (let* ((adt (find-supertype (type expr)))
  ;; 		     (ir-selections (pvs2ir-selections selections adt))
  ;; 		     (ir-switch-else (pvs2ir* else-part bindings)))))
		
	      

    

(defmethod pvs2ir* ((expr if-expr) bindings)
  (cond ((branch? expr)
	 (let ((ifvar (mk-ir-variable (new-irvar) 'boolean))
	       (cond-ir (pvs2ir* (condition expr) bindings))
	       (then-ir (pvs2ir* (then-part expr) bindings))
	       (else-ir (pvs2ir* (else-part expr) bindings)))
	   (mk-ir-let  ifvar cond-ir
		       (mk-ir-ift ifvar then-ir else-ir))))
	(t (call-next-method))))

(defmethod pvs2ir* ((expr let-expr) bindings)
  (let* ((let-bindings (bindings (operator expr)))
	 (args (arguments expr))
	 (args (if (= (length let-bindings)(length args)) args
		 (loop for bnd in let-bindings
		       as i from 1
		       collect (make-projection-application i (car args)))))
	(expression (expression (operator expr))))
    (pvs2ir-let-expr let-bindings args expression bindings)))

(defun pvs2ir-let-expr (let-bindings args expression bindings)
  ;;deal with simple let-expressions first, and pattern matching lets later.
  (cond ((consp let-bindings)
	 (let* ((ir-var (new-irvar))
		(ir-type (pvs2ir-type (type (car let-bindings))))
		(ir-arg-type (pvs2ir-type (type (car args))))
		(ir-vartype (mk-ir-variable ir-var ir-type))
		(ir-bind-expr (pvs2ir* (car args) bindings))
		(ir-body (pvs2ir-let-expr (cdr let-bindings) (cdr args)
				       expression
				       (cons (cons (car let-bindings)
						   ir-vartype)
					     bindings))));(break "pvs2ir(let-expr)")
	   (if (or (ir-subrange? ir-type)
		   (ir2c-tequal* ir-type ir-arg-type))
	       (mk-ir-let ir-vartype ir-bind-expr ir-body)
	     (let* ((arg-var (new-irvar))
		    (arg-vartype (mk-ir-variable arg-var ir-arg-type)))
	     (mk-ir-let  arg-vartype
			 ir-bind-expr
			 (mk-ir-lett ir-vartype ir-type arg-vartype ir-body))))))
	(t (pvs2ir* expression bindings))))

(defmethod pvs2ir* ((expr tuple-expr) bindings)
  (let* ((expressions (exprs expr))
	 (ir-assignments (pvs2ir*  expressions bindings))
	 (ir-field-vars (new-irvars (length expressions)))
	 (ir-field-types (pvs2ir-type (mapcar #'type expressions)))
	 (ir-field-var-types (mk-vartype-list  ir-field-vars
					       ir-field-types))
	 (ir-fields (loop for i from 1 to (length expressions)
			  as ir-var in ir-field-var-types
			  collect
			  (mk-ir-field (intern (format nil "project_~a" i))
				       ir-var)))
	 (ir-recordtype (mk-ir-tupletype (loop for type in ir-field-types
						as field in ir-fields
						collect (mk-ir-fieldtype (ir-fieldname field) type)))))
  (mk-ir-let* ir-field-var-types ir-assignments
	      (mk-ir-record ir-fields ir-recordtype))))


(defmethod pvs2ir* ((expr record-expr) bindings)
  (pvs2ir-fields (sort-assignments (assignments expr)) bindings))

(defun mk-vartype-list (vars types)
  (cond ((consp vars)(cons (mk-ir-variable (car vars)(car types))
			   (mk-vartype-list (cdr vars)(cdr types))))
	(t nil)))

(defmethod pvs2ir-expr-type ((expr number-expr))
  (mk-ir-subrange (number expr)(number expr)))

(defmethod pvs2ir-expr-type ((expr record-expr))
  (with-slots (assignments) expr
	      (mk-ir-recordtype (pvs2ir-expr-type (sort-assignments assignments)))))

(defmethod pvs2ir-expr-type ((expr tuple-expr))
  (with-slots (exprs) expr
	      (mk-ir-tupletype (loop for ir-type in (pvs2ir-expr-type exprs)
				      as i from 1
				      collect (mk-ir-fieldtype (intern (format nil "project_~a" i))
							       ir-type)))))

(defmethod pvs2ir-expr-type ((expr  list))
  (cond ((consp expr) (cons (pvs2ir-expr-type (car expr))
			    (pvs2ir-expr-type (cdr expr))))
	(t nil)))

(defmethod pvs2ir-expr-type ((expr uni-assignment))
  (with-slots (arguments expression) expr
	      (mk-ir-fieldtype (id (caar arguments))
				(pvs2ir-expr-type expression))))

(defmethod pvs2ir-expr-type ((expr t))
  (let ((val (pvs-integer? expr)))
    (if val (mk-ir-subrange val val)
      (if (and (application? expr)
	       (pvs2ir-primitive? (operator expr))
	       (memq (id (operator expr)) '(+ - * /)))
	  (best-ir-subrange (type expr) (judgement-types expr))
	(if (funtype? (type expr))
	    (best-ir-subrange (type expr)(judgement-types+ expr))
	  (pvs2ir-type (type expr)))))))

(defun pvs2ir-fields (assignments bindings)
  (let* ((expressions (mapcar #'expression assignments))
	 (ir-assignments (pvs2ir*  expressions bindings))
	 (ir-field-vars (new-irvars (length assignments)))
	 (ir-field-types (loop for expr in expressions
			       collect (pvs2ir-expr-type expr)))
	 (ir-field-vartypes (mk-vartype-list ir-field-vars ir-field-types))
	 (ir-fields (loop for assignment in assignments
			  as ir-field-vartype in ir-field-vartypes
			  collect
			  (mk-ir-field (id (caar (arguments assignment)))
				       ir-field-vartype)))
	 (ir-recordtype (mk-ir-recordtype (loop for field in ir-fields
						as type in ir-field-types
						collect (mk-ir-fieldtype (ir-fieldname field) type)))))
    ;(break "pvs2ir-fields")
  (mk-ir-let* ir-field-vartypes ir-assignments
	      (mk-ir-record ir-fields ir-recordtype))))

(defmethod pvs2ir* ((expr fieldappl) bindings)
  (with-slots (id argument) expr
	      (let ((ir-argument (pvs2ir* argument bindings))
		    (argvar (mk-ir-variable (new-irvar)(pvs2ir-type (type argument)))))
		(mk-ir-let argvar
			   ir-argument
			   (mk-ir-get argvar id)))))

(defmethod pvs2ir* ((expr projappl) bindings)
  (with-slots (index argument) expr
	      (let ((ir-argument (pvs2ir* argument bindings))
		    (argvar (mk-ir-variable (new-irvar)(pvs2ir-type (type argument)))))
		(mk-ir-let argvar
			   ir-argument
			   (mk-ir-get argvar (intern (format nil "project_~a" index)))))))

(defmethod pvs2ir* ((expr update-expr) bindings)
  (with-slots (type expression assignments) expr
	      (let ((ir-expression (pvs2ir* expression bindings)))
		(pvs2ir-update assignments ir-expression
			       (pvs2ir-type (type expression))
			       bindings))))

(defmethod pvs2ir* ((expr quant-expr) bindings)
  (format t "~%PVS2C Error generating code for ~% ~a ~%Quantifiers are not handled" expr)
  (mk-ir-function 'u_undef_quant_expr));;using a dummy name

;;gets the type of component of nested arrays/records being updated.
;;This is used to get the right-hand side type of an update expression.
(defmethod get-component-ir-type ((ir-expr-type ir-funtype) lhs-args)
  (with-slots (ir-domain ir-range) ir-expr-type
	      (cond ((consp lhs-args)
		     (get-component-ir-type ir-range (cdr lhs-args)))
		    (t ir-expr-type))))

(defmethod get-component-ir-type ((ir-expr-type ir-recordtype) lhs-args)
  (with-slots (ir-field-types) ir-expr-type
	      (cond ((consp lhs-args)
		     (let* ((field-label (caar lhs-args))
			    (ir-field-type (if (accessor-assign? field-label)
				       (pvs2ir-type (range (type field-label)))
				     (let ((field-id (id field-label)))
				       (ir-ftype (find field-id ir-field-types :key #'ir-id))))))
		       (get-component-ir-type ir-field-type (cdr lhs-args))))
		    (t ir-expr-type))))

(defmethod get-component-ir-type ((ir-expr-type ir-typename) lhs-args)
  (with-slots (ir-type-id ir-type-defn) ir-expr-type
	      (get-component-ir-type ir-type-defn lhs-args)))

(defmethod get-component-ir-type ((ir-expr-type t) lhs-args)
  ;;lhs-args should be empty
  ir-expr-type)

;;evaluates the right-hand sides; creates bindings; and then constructs the
;;updates over the assignments
(defun pvs2ir-update (assignments ir-expression expression-type bindings)
  (let ((maplet? (loop for assgn in assignments thereis (maplet? assgn))))
    (when maplet? (error 'pvs2c-error :format-control "Maplets not supported")))
  (let ((rhs-list (mapcar #'expression assignments))
	(lhs-list (mapcar #'arguments assignments)))
    (let* ((rhs-irvar-list (loop for i from 1 to (length rhs-list)
				 collect (new-irvar)))
	   (ir-rhs-types (loop for lhs in lhs-list
			       collect (get-component-ir-type expression-type lhs)))
	   (ir-rhs-vartypes (mk-vartype-list rhs-irvar-list ir-rhs-types))
	   (ir-rhs-list (loop for rhs in rhs-list
			      collect
			      (pvs2ir* rhs bindings)))
	   (ir-exprvar (mk-ir-variable (new-irvar) expression-type))) ;binds the ir-expression value
      (let ((ir-update-expr (pvs2ir-assignments lhs-list ;build updates
						ir-rhs-vartypes
						ir-exprvar
						expression-type
						bindings)))
	(mk-ir-let* ir-rhs-vartypes ir-rhs-list
		    (mk-ir-let ir-exprvar 
			       ir-expression
			       ir-update-expr))))))

;;iterates through the assignments constructing updates.  
(defun pvs2ir-assignments (lhs-list rhs-irvar-list
			       ir-exprvar ir-expr-type bindings)
  (cond ((consp lhs-list)
	 (let ((ir-assignment1
		(pvs2ir-assignment1 ir-expr-type
				    (car lhs-list)
				    (car rhs-irvar-list)
				    ir-exprvar bindings))
	       (ir-var1 (new-irvar)))
	   (let ((ir-vartype1 (mk-ir-variable ir-var1 ir-expr-type)))
	     (mk-ir-let  ir-vartype1 ir-assignment1
			 (pvs2ir-assignments (cdr lhs-list)(cdr rhs-irvar-list)
					     ir-vartype1 ir-expr-type bindings)))))
	(t ir-exprvar)))


;;The translation of A WITH [(i)(j):=v] becomes
;;let l1 = i
;;    e11 = A[i]
;;    e1 = A WITH [i := nil]    ;;to preserve reference count of A[i]
;;    en = e11 WITH [j := v]
;; in e1 WITH [i := en]
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-funtype) lhs rhs-irvar ir-exprvar bindings)
  (cond ((consp lhs)
	 (let* ((ir-exprvar11 (new-irvar))
		(ir-expr-type11 (ir-range ir-expr-type))
		(ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		(ir-rest-assignment (if (consp (cdr lhs))
					(pvs2ir-assignment1 ir-expr-type11
							    (cdr lhs)
							    rhs-irvar ir-expr-vartype11
							    bindings)
				      rhs-irvar))
		(lhs1 (caar lhs)))
	   (let ((lhs1-irvar (new-irvar))
		 (lhs1-ir (pvs2ir* lhs1 bindings)) ;;the index expression
		 (lhs1-ir-type (ir-domain ir-expr-type))
		 (ir-exprvar1 (new-irvar))
		 (ir-new-rhsvar (new-irvar)))
	     (let ((ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
		   (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		   (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
	       (mk-ir-let  ir-lhs1-vartype lhs1-ir
			   (mk-ir-let ir-expr-vartype11
				      (mk-ir-apply ir-exprvar (list ir-lhs1-vartype));;was lookup
				      (mk-ir-let ir-expr-vartype1
						 (if (ir-reference-type? ir-expr-type11)
						     (let* ((ir-nullvar (new-irvar))
							    (ir-nullvartype (mk-ir-variable ir-nullvar ir-expr-type11)))
						     (mk-ir-let ir-nullvartype
								(mk-ir-nil)
								(mk-ir-update ir-exprvar ir-lhs1-vartype ir-nullvartype)))
						   ir-exprvar)
						 (mk-ir-let ir-new-rhsvartype
							    ir-rest-assignment
							    (mk-ir-update ir-expr-vartype1 ir-lhs1-vartype ir-new-rhsvartype)))))))))
	(t ir-exprvar)))

(defmethod pvs2ir-assignment1 ((ir-expr-type ir-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (cond ((consp lhs)
	   (let* ((lhs1 (caar lhs));;lhs1 is a field-name-expr
		  (ir-field-decl (find (id lhs1) (ir-field-types ir-expr-type) :key #'ir-id))
		  (ir-expr-type11 (ir-ftype ir-field-decl)) ;;(pvs2ir-type (range (type lhs1)))
		  (ir-exprvar11 (new-irvar))
		  (ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		  (ir-rest-assignment (if (consp (cdr lhs))
					  (pvs2ir-assignment1 ir-expr-type11 (cdr lhs)
							      rhs-irvar ir-expr-vartype11
							      bindings)
					rhs-irvar)))
	     (let* ((ir-exprvar1 (new-irvar))
		    (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		    (ir-new-rhsvar (new-irvar))
		    (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
	       (mk-ir-let ir-expr-vartype11
			  (mk-ir-get ir-exprvar (id lhs1));;directly get the field
			  (mk-ir-let ir-expr-vartype1
				     (if (ir-reference-type? ir-expr-type11)
					 (let* ((ir-nullvar (new-irvar))
						(ir-nullvartype (mk-ir-variable ir-nullvar ir-expr-type11)))
					   (mk-ir-let ir-nullvartype
						      (mk-ir-nil)
						      (mk-ir-update ir-exprvar (id lhs1) ir-nullvartype)))
				       ir-exprvar)
				     (mk-ir-let ir-new-rhsvartype
						ir-rest-assignment
						(mk-ir-update ir-expr-vartype1 (id lhs1) ir-new-rhsvartype)))))))
	  (t ir-exprvar)))


;;had to add method for ir-adt-recordtype since the type here is the adt and not the constructor,
;;whereas in the record case, the field-assign does not have a type s othat ir-expr-type11 has to
;;be computed from the ir-type of the field.  
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-adt-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (cond ((consp lhs)
	   (let* ((lhs1 (caar lhs));;lhs1 is a field-name-expr
		  ;(ir-field-decl (find (id lhs1) (ir-field-types ir-expr-type) :key #'ir-id))
		  (ir-expr-type11 (pvs2ir-type (range (type lhs1)))) ;;(ir-ftype ir-field-decl)
		  (ir-exprvar11 (new-irvar))
		  (ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		  (ir-rest-assignment (if (consp (cdr lhs))
					  (pvs2ir-assignment1 ir-expr-type11 (cdr lhs)
							      rhs-irvar ir-expr-vartype11
							      bindings)
					rhs-irvar)))
	     (let* ((ir-exprvar1 (new-irvar))
		    (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		    (ir-new-rhsvar (new-irvar))
		    (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
	       (mk-ir-let ir-expr-vartype11
			  (mk-ir-get ir-exprvar (id lhs1));;directly get the field
			  (mk-ir-let ir-expr-vartype1
				     (if (ir-reference-type? ir-expr-type11)
					 (let* ((ir-nullvar (new-irvar))
						(ir-nullvartype (mk-ir-variable ir-nullvar ir-expr-type11)))
					   (mk-ir-let ir-nullvartype
						      (mk-ir-nil)
						      (mk-ir-update ir-exprvar (id lhs1) ir-nullvartype)))
				       ir-exprvar)
				     (mk-ir-let ir-new-rhsvartype
						ir-rest-assignment
						(mk-ir-update ir-expr-vartype1 (id lhs1) ir-new-rhsvartype)))))))
	  (t ir-exprvar)))

;(defmethod pvs2ir-assignment1 ((ir-expr-type ir-adt-recordtype) lhs rhs-irvar ir-exprvar bindings)

(defmethod pvs2ir-assignment1 ((ir-expr-type ir-typename) lhs rhs-irvar ir-exprvar bindings)
  (pvs2ir-assignment1 (ir-type-defn ir-expr-type) lhs rhs-irvar ir-exprvar bindings))
					 
;;We are dealing with the simplified situation of types that are non-dependent
;;tuples, records, functions/arrays, and datatypes.  
(defun pvs2ir-type (type &optional binding)
  (pvs2ir-type* type binding))

;;For the time being, translate types in a fully expanded form, and return to named types later. 
;; (defmethod pvs2ir-type* :around ((type type-expr))
;;   (if (type-name? (print-type type))
;;       (let* ((type-decl (declaration (print-type type)))
;; 	     (eval-type-info (ir-type-value type-decl)))
;; 	(if eval-type-info
;; 	    (ir-type-name eval-type-info)
;; 	  (let* ((ir-type-name (intern (format nil "~a_t" (gentemp (format nil "~a" (id type-decl))))))
;; 		 (eval-type-info (make-instance 'eval-type-info
;; 						:ir-type-name ir-type-name)))
;; 	    (setf (ir-type-value type-decl) ;;has to be done in this order
;; 		  eval-type-info) ;;so that the name is already there
;; 	    (let ((ir-type-definition (pvs2ir-type (type-value type-decl))))
;; 	      (setf (gethash ir-type-name *ir-type-def-hash*) ir-type-definition)
;; 	      (setf (ir-type-definition (ir-type-value type-decl)) ir-type-definition))
;; 	    ir-type-name)))
;;     (call-next-method)))

(defmethod pvs2ir-type* :around ((type type-expr) tbindings)
  (let ((type-name (if (type-name? type) type
		     (print-type type))))
    (if (and type-name (not (subtype? type)))
	(let ((type-decl (declaration type-name)));(break "around")
	     (cond ((formal-type-decl? type-decl)
		    (let ((tbind (assoc type-decl tbindings))) ;(break "around")
		      (if tbind (cdr tbind) (call-next-method))))
		   ((ir-type-value type-decl)
		    (ir-type-name (ir-type-value type-decl)))
		   (t (call-next-method))))
      (call-next-method))))

	    ;; (let* ((ir-type (call-next-method))
	    ;; 	   (ir-type-id (pvs2ir-unique-decl-id type-decl))
	    ;; 	   (ir-typename (mk-ir-typename ir-type-id ir-type)))
	    ;;   (setf (ir-type-value type-decl)
	    ;; 	    (mk-eval-type-info  ir-typename))
	    ;;   ir-typename)))


(defmethod pvs2ir-type* ((type funtype) tbinding)
  (with-slots (domain range) type
	      (mk-ir-funtype (pvs2ir-type* domain tbinding)
			     (pvs2ir-type* range tbinding))))

(defmethod pvs2ir-type* ((type recordtype) tbinding)
  (let ((fields (sort-fields (fields type))))
    (mk-ir-recordtype (pvs2ir-type* fields tbinding))))

(defmethod pvs2ir-type* ((type field-decl) tbinding)
  (mk-ir-fieldtype (id type)(pvs2ir-type* (type type) tbinding)))

(defmethod pvs2ir-type* ((type dep-binding) tbinding)
  (pvs2ir-type* (type type) tbinding))

(defmethod pvs2ir-type* ((type tupletype) tbinding)
  (let* ((types (types type))
	 (tuple-fields
	  (loop for typ in types as i from 1
		collect (mk-ir-fieldtype (intern (format nil "project_~a" i))
			       (pvs2ir-type* typ tbinding)))))
    (mk-ir-tupletype tuple-fields)))

(defmethod pvs2ir-type* ((type type-name) tbindings)
  (if (tc-eq type *boolean*)
      'bool
    ;;remove?     (if (tc-eq type *numfield* ))
    (if (formal-type-decl? (declaration type))
	(mk-ir-formal-typename (pvs2ir-unique-decl-id (declaration type)))
	(let ((tbind (assoc (declaration type) tbindings)))
	  (if tbind (cdr tbind) (pvs2ir-decl (declaration type)))))))
    ;;(pvs2ir-decl (declaration type)))));;returns the type name

(defmethod pvs2ir-type* ((type list) tbinding)
  (cond ((consp type)
	 (cons (pvs2ir-type* (car type) tbinding)
	       (pvs2ir-type* (cdr type) tbinding)))
	(t nil)))

(defmethod pvs2ir-type* ((type subtype) tbinding)
  (cond ((tc-eq type *naturalnumber*) (mk-ir-subrange 0 '*))
	((tc-eq type *integer*) (mk-ir-subrange  '* '*))
	((tc-eq type *posint*)(mk-ir-subrange 1 '*))
	((tc-eq type *negint*)(mk-ir-subrange '* -1))
	((subtype-of? type *integer*) ;;was *number*
	                              ;;was *integer* but misses some subranges
	 (let ((sub (pvs2ir-subrange-index type)))
	   (if sub 
	       (mk-ir-subrange (car sub)(cadr sub))
	     (mk-ir-subrange '* '*))))
	((subtype-of? type *number*)
	 'mpq)
	;((subtype-of? type *number*) (break "pvs2ir-type*(subtype)"))
	(t (pvs2ir-type* (supertype type) tbinding))))

(defun intersect-subrange (sub1 sub2)
  (with-slots ((low1 ir-low) (high1 ir-high)) sub1
	      (with-slots ((low2 ir-low) (high2 ir-high)) sub2
			  (let ((new-low (if (eq low1 '*) low2
					   (if (eq low2 '*) low1
					     (if (< low1 low2) low2 low1))))
				(new-high (if (eq high1 '*) high2
					    (if (eq high2 '*) high1
					      (if (< high1 high2) high1 high2)))))
			    (mk-ir-subrange new-low new-high)))))

(defun pvseval-integer (expr)
  (handler-case
      (let ((expr-value (eval (pvs2cl expr))))
	(and (integerp expr-value) expr-value))
    (pvseval-error (condition) nil)))
	  

;returns a pair of the lower and upper bound
(defun pvs2ir-subrange-index (type)
  (let ((below (simple-below? type)))
    (if below
	(let ((val (pvseval-integer below))) ;if (number-expr? upto)
	  (if val (list 0 (1- val)) ;(number upto)
	    (let ((hihi (pvs2ir-subrange-index (type below))))
	      (if hihi (list 0 (cadr hihi))
		(list 0 '*)))))
	(let ((upto (simple-upto? type)))
	  (or (and upto (let ((val (pvseval-integer upto))) ;if (number-expr? upto)
			  (if val (list 0 val) ;(number upto)
			  (let ((hihi (pvs2ir-subrange-index (type upto))))
			    (if hihi (list 0 (cadr hihi))
			      (list 0 '*))))))
	      (let ((x (simple-subrange? type)))
		(if x
		  (let ((lo (or (pvseval-integer (car x))
			      (let* ((type-ir (pvs2ir-type (type (car x))))
				     (judgement-type-irs (loop for type in (judgement-types (car x))
							       collect (pvs2ir-type type)))
				     (lo-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
							when (and (ir-subrange? ir-type)(numberp (ir-low ir-type)))
							collect (ir-low ir-type))))
				(if lo-subrange (apply #'min lo-subrange) '*))))
			(hi (or (pvseval-integer (cdr x))
				(let* ((type-ir (pvs2ir-type (type (car x))))
				       (judgement-type-irs (loop for type in (judgement-types (car x))
								 collect (pvs2ir-type type)))
				       (hi-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
							  when (and (ir-subrange? ir-type)(numberp (ir-high ir-type)))
							  collect (ir-high ir-type))))
				  (if hi-subrange (apply #'max hi-subrange) '*)))))
		    (list lo hi))
		  (if (subtype? type)
			(pvs2ir-subrange-index (supertype type))
		    (if (subtype-of? type *integer*)
		      (list '* '*)
		      nil)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pvs2ir-freevars* :around ((ir-expr ir-expr))
  (with-slots (ir-freevars) ir-expr
	      (if (eq ir-freevars 'unbound)
		  (let ((ir-freevars (call-next-method)))
		    (setf (ir-freevars ir-expr) ir-freevars)
		    ir-freevars)
		ir-freevars)))

(defmethod pvs2ir-freevars* ((ir-expr ir-variable))
  (list ir-expr))

(defmethod pvs2ir-freevars* ((ir-expr ir-last))
  (with-slots (ir-var) ir-expr
	      (list ir-var)))

(defmethod pvs2ir-freevars* ((ir-expr ir-apply))
  (with-slots (ir-func ir-params ir-args) ir-expr
	      (union (pvs2ir-freevars* ir-func)
		     (union (pvs2ir-freevars* ir-params)
			    (pvs2ir-freevars* ir-args) :test #'eq)
		     :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-let))
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
	      (union (pvs2ir-freevars* ir-bind-expr)
		     (remove ir-vartype (pvs2ir-freevars* ir-body) :test #'eq)
		     :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-record))
  (with-slots (ir-fields) ir-expr
	      (pvs2ir-freevars* ir-fields)))

(defmethod pvs2ir-freevars* ((ir-expr ir-field))
  (with-slots (ir-fieldname ir-value) ir-expr
	      (pvs2ir-freevars* ir-value)))

(defmethod pvs2ir-freevars* ((ir-expr ir-lambda))
  (with-slots (ir-vartypes ir-body) ir-expr
	      (set-difference (pvs2ir-freevars* ir-body)
			      ir-vartypes
			      :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-ift))
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (union (pvs2ir-freevars* ir-condition)
		     (union (pvs2ir-freevars* ir-then)
			    (pvs2ir-freevars* ir-else)
			    :test #'eq)
		     :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-nil))
  nil)

(defmethod pvs2ir-freevars* ((ir-expr ir-lookup))
  (with-slots (ir-array ir-index) ir-expr
	      (union (pvs2ir-freevars* ir-array)
		     (pvs2ir-freevars* ir-index)
		     :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-update))
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      (union (pvs2ir-freevars* ir-target)
		     (union (pvs2ir-freevars* ir-lhs)
			    (pvs2ir-freevars* ir-rhs)
			    :test #'eq)
		     :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-new))
  nil)

(defmethod pvs2ir-freevars* ((ir-expr ir-get))
  (with-slots (ir-record) ir-expr
	      (pvs2ir-freevars* ir-record)))

(defmethod pvs2ir-freevars* ((ir-expr list))
  (cond ((consp ir-expr)
	 (union (pvs2ir-freevars* (car ir-expr))
		(pvs2ir-freevars* (cdr ir-expr))
		:test #'tc-eq))
	(t nil)))

(defmethod pvs2ir-freevars* ((ir-expr t))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk-ir-last (ir-var)
  (make-instance 'ir-last
		 :ir-var ir-var))

(defmethod print-ir ((ir-expr ir-last))
  (with-slots (ir-var) ir-expr
  `(last ,(print-ir ir-var))))

(defcl ir-release (ir-expr) ;;these are the variables that should be freed on the then/else branches
  pre-ir-vars
  post-ir-vars
  ir-body)

(defun mk-ir-release (pre-ir-vars post-ir-vars ir-body)
  (make-instance 'ir-release
		 :pre-ir-vars pre-ir-vars
		 :post-ir-vars post-ir-vars
		 :ir-body ir-body))

(defmethod print-ir ((ir-expr ir-function))
  (with-slots (ir-fname) ir-expr 
	      ir-fname))

(defmethod print-ir ((ir-expr ir-release))
  (with-slots (pre-ir-vars post-ir-vars ir-body) ir-expr
	      `(release ,(print-ir pre-ir-vars)
			,(print-ir post-ir-vars)
			,(print-ir ir-body))))

(defun get-assoc (var bindings)
  (let ((bnd (assoc var bindings)))
    (if bnd (cdr bnd) var)))

(defun apply-bindings (bindings var-list)
  (cond ((consp var-list)
	 (cons (get-assoc (car var-list) bindings)
	       (apply-bindings bindings (cdr var-list))))
	(t nil)))

(defun preprocess-ir (ir-expr)
  (preprocess-ir* ir-expr nil nil))

(defmethod preprocess-ir* ((ir-expr ir-variable) livevars bindings)
  (let ((ir-var (get-assoc ir-expr bindings)))
    (if (memq ir-var livevars)
	ir-var ;;then variable is live and this is not the last occurrence
      (mk-ir-last ir-var))))

(defun bound-plus (bound1 bound2)
  (if (or (eq '* bound1)(eq '* bound2))
      '*
    (+ bound1 bound2)))

(defun plus-subrange (subrange1 subrange2)
  (with-slots (ir-low ir-high) subrange1
	      (with-slots ((low2 ir-low)(high2 ir-high)) subrange2
			  (mk-ir-subrange (bound-plus ir-low low2)
					  (bound-plus ir-high high2)))))

(defun negate-subrange (subrange)
  (with-slots (ir-low ir-high) subrange
	      (mk-ir-subrange (if (eq ir-high '*) '* (- ir-high))
			      (if (eq ir-low '*) '* (- ir-low)))))

(defun minus-subrange (subrange1 subrange2)
  (plus-subrange subrange1 (negate-subrange subrange2)))

(defun bound-times (bound1 bound2)
  (cond ((eq '* bound1)
	 (if (eq '* bound2)
	     bound1
	   (if (eq '-* bound2) bound2
	     (cond ((zerop bound2) 0)
		   ((< bound2 0) '-*)
		   (t '*)))))
	((eq '-* bound1) (cond ((eq bound2 '*) '-*)
			       ((eq bound2 '-*) '*)
			       ((zerop bound2) 0)
			       ((< bound2 0) '*)
			       (t '-*)))
	((zerop bound1) 0)
	((< bound1 0) (cond ((eq bound2 '*) '-*)
			    ((eq bound2 '-*) '*)
			    ((zerop bound2) 0)
			    (t (* bound1 bound2))))
	(t (cond ((eq bound2 '*) '*)
		 ((eq bound2 '-*) '-*)
		 (t (* bound1 bound2))))))

(defun make-low-bound (x);turns a lower bound * into an explicit -*.
  (if (eq x '*) '-* x))

(defun bound-min (x y)
    (cond ((or (eq x '-*)(eq y '-*))
	   '-*)
	  ((eq x '*) y)
	  ((eq y '*) x)
	  (t (min x y))))

(defun bound-max (x y)
    (cond ((or (eq x '*)(eq y '*))
	   '*)
	  ((eq x '-*) y)
	  ((eq y '-*) x)
	  (t (max x y))))

(defun bound-norm (x)
  (if (eq x '-*) '* x))

(defun times-subrange (subrange1 subrange2)
  (with-slots ((low1 ir-low) (high1 ir-high)) subrange1
	      (with-slots ((low2 ir-low)(high2 ir-high)) subrange2
			  (let ((low1 (make-low-bound low1))
				(low2 (make-low-bound low2)))
			    (let ((lowhigh (bound-times low1 high2))
				  (lowlow (bound-times low1 low2))
				  (highlow (bound-times high1 low2))
				  (highhigh (bound-times high1 high2)))
			      (let ((low (bound-min lowhigh (bound-min lowlow (bound-min highlow highhigh))))
				    (high (bound-max lowhigh (bound-max lowlow (bound-max highlow highhigh)))))
				(mk-ir-subrange (bound-norm low) (bound-norm high))))))))

(defmethod ir-arith-type ((ir-expr ir-integer))
  (with-slots (ir-intval) ir-expr
	      (mk-ir-subrange ir-intval ir-intval)))

(defmethod ir-arith-type ((ir-expr ir-variable))
  (with-slots (ir-vtype) ir-expr
	      ir-vtype))

(defmethod ir-arith-type ((ir-expr ir-last))
  (with-slots (ir-var) ir-expr
	      (ir-arith-type ir-var)))

(defmethod ir-arith-type ((ir-expr ir-let))
  (with-slots (ir-body) ir-expr
	      (ir-arith-type ir-body)))

(defmethod ir-arith-type ((ir-expr ir-apply))
  (with-slots (ir-atype) ir-expr
	      ir-atype))

(defmethod ir-arith-type ((ir-expr t))
  (mk-ir-subrange '* '*))


(defun ir-apply-arith-type (op args atype)
  (if (and (ir-function? op)
	   (ir-primitive-arith-op? (ir-fname op)))
      (let ((new-subrange-type
	     (case (ir-fname op)
	       (+ (plus-subrange (ir-arith-type (car args))
				 (ir-arith-type (cadr args))))
	       (- (if (< (length args) 2)
		      (negate-subrange (ir-arith-type (car args)))
		    (minus-subrange (ir-arith-type (car args))
				    (ir-arith-type (cadr args)))))
	       (* (times-subrange (ir-arith-type (car args))
				  (ir-arith-type (cadr args))))
	       (t))))
	(if new-subrange-type
	    (if atype
		(intersect-subrange new-subrange-type atype)
	      new-subrange-type)
	  atype))
    atype))    



(defmethod preprocess-ir* ((ir-expr ir-apply) livevars bindings);;the ir-args are always distinct, but we are
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr               ;;not exploiting this here.
	      (let* ((ir-params-with-bindings (apply-bindings bindings (pvs2ir-freevars* ir-params)))
		     (ir-args-with-bindings (apply-bindings bindings (pvs2ir-freevars* ir-args)))
		     (new-ir-func
		      (preprocess-ir* ir-func (union ir-params-with-bindings
					       (union ir-args-with-bindings 
						      livevars :test #'eq) :test #'eq)
					       
				      bindings))
		     (new-ir-params (preprocess-ir* ir-params livevars bindings))
		     (new-ir-args (preprocess-ir* ir-args (union ir-params-with-bindings
								 livevars :test #'eq)
						  bindings))
		     ;; (new-ir-atype (ir-apply-arith-type new-ir-func new-ir-args ir-atype))
		     )
		(mk-ir-apply new-ir-func new-ir-args new-ir-params ir-atype))))


		     ;; (new-ir-atype (if (and (ir-primitive-arith-op? new-ir-func)
		     ;; 			    (memq new-ir-func '(+ * -)))
		     ;; 		       (ir-apply-arith-type new-ir-func ir-atype
							    

;Irrelevant let-bindings are discarded
(defmethod preprocess-ir* ((ir-expr ir-let) livevars bindings)
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
	      (let ((body-freevars (pvs2ir-freevars* ir-body)))
		;(loop for iv in body-freevars do (format t "~% free: ~a" (ir-name iv)))
		;(when (not (memq ir-vartype body-freevars)) (break "preprocess"))
		(if (memq ir-vartype body-freevars);;note: without apply-bindings
		    (let ((new-ir-bind-expr
			   (preprocess-ir* ir-bind-expr (union (apply-bindings bindings
									       body-freevars)
								       livevars :test #'eq)
							bindings)))
		      ;(when (eq (ir-name ir-vartype) 'ivar_13)(break "preprocess ir-let: ~a" (ir-name ir-vartype)))
		    (if (and (or (ir-variable? new-ir-bind-expr) ;;bind var to var
				 (ir-last? new-ir-bind-expr))
			     (ir2c-tequal (ir-vtype ir-vartype)(ir-vtype (get-ir-last-var new-ir-bind-expr)))
			     )
			    ;binds var to var with same type
			(preprocess-ir* ir-body livevars
					(acons ir-vartype
					       (get-assoc (get-ir-last-var new-ir-bind-expr) bindings)
					       bindings))
		      (let* ((new-ir-vartype ir-vartype)
			     ;; (if (ir-integer? new-ir-bind-expr)
			     ;; 	  (mk-ir-variable (ir-name ir-vartype)
			     ;; 			  (mk-ir-subrange (ir-intval ir-bind-expr)(ir-intval ir-bind-expr)))
			     ;; 	ir-vartype)
			     (new-ir-body (preprocess-ir* ir-body livevars bindings)))
			;;(acons ir-vartype new-ir-vartype bindings)
			;(when (ir-lett? ir-expr) (break "ir-lett"))
			(if (ir-lett? ir-expr)
			    (make-ir-lett new-ir-vartype (ir-bind-type ir-expr)
					new-ir-bind-expr
					new-ir-body)
			  (mk-ir-let  new-ir-vartype new-ir-bind-expr new-ir-body)))))
		  ;(progn (format t "~%not free: ~a" (ir-name ir-vartype))
		  (preprocess-ir* ir-body livevars bindings)))))

(defmethod preprocess-ir* ((ir-expr ir-record) livevars bindings)
  (with-slots (ir-fields ir-recordtype) ir-expr
	      (mk-ir-record (preprocess-ir* ir-fields livevars bindings)
			    ir-recordtype)))

(defmethod preprocess-ir* ((ir-expr ir-field) livevars bindings)
  (with-slots (ir-fieldname ir-value) ir-expr
	      (mk-ir-field ir-fieldname (preprocess-ir* ir-value livevars bindings))))

(defun ir-numeric-type? (ir-type)
  (or (eq ir-type 'mpq)
      (ir-subrange? ir-type)))

(defun ir-mpq-type? (ir-type)
  (eq ir-type 'mpq))

(defmethod preprocess-ir* ((ir-expr ir-lambda) livevars bindings)
  (with-slots (ir-vartypes ir-rangetype ir-body) ir-expr
	      (let* ((ir-mpq-vartypes (loop for ir-var in ir-vartypes
					    when (ir-mpq-type? (ir-vtype ir-var))
					    collect ir-var))
		      (expr-freevars (pvs2ir-freevars* ir-expr))
		       (last-expr-freevars (set-difference expr-freevars livevars :test #'eq))
		       ;;last-expr-freevars preserve refcount when closure is created
		       ;;other expr-freevars have their refcounts incremented by one.
		       (other-livevars (append ir-mpq-vartypes (union last-expr-freevars livevars :test #'eq)))
		       (body-freevars (pvs2ir-freevars* ir-body))
					;(irrelevant-args (set-difference ir-vartypes body-freevars :test #'eq))
		       (preprocessed-body (preprocess-ir* ir-body other-livevars bindings))
		       ;; (preprocessed-wrapped-body
		       ;; 	(if irrelevant-args
		       ;; 	    (mk-ir-release (extract-reference-vars irrelevant-args)
		       ;; 			   nil
		       ;; 			   preprocessed-body)
		       ;; 	  preprocessed-body))
		       (preprocessed-ir
			(mk-ir-lambda-with-lastvars
			 ir-vartypes
			 last-expr-freevars
			 ir-rangetype
			 preprocessed-body)))
		  ;; (format t "~%Preprocessing Lambda")
		  ;; (format t "~%Freevars = ~s" (print-ir expr-freevars))
		  ;; (format t "~%Irrelevant Args = ~s" (print-ir irrelevant-args))
		  ;; (format t "~%Non-live freevars = ~s" (print-ir last-expr-freevars))
		  preprocessed-ir)))

		  ;; (if last-expr-freevars
		  ;;     (mk-ir-release nil (extract-reference-vars last-expr-freevars)
		  ;; 		       preprocessed-ir)
		  ;;   preprocessed-ir)

(defun extract-reference-vars (ir-varlist)
  (loop for ir-var in ir-varlist
	when (ir-reference-type? (ir-vtype ir-var))
	collect ir-var))


(defmethod preprocess-ir* ((ir-expr ir-ift) livevars bindings)
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (let* ((then-freevars (apply-bindings bindings (pvs2ir-freevars* ir-then)))
		     (then-marked (set-difference then-freevars livevars :test #'eq))		     
		     (else-freevars (apply-bindings bindings (pvs2ir-freevars* ir-else)))
		     (else-marked (set-difference else-freevars livevars :test #'eq))
		     (then-release (extract-reference-vars
				    (set-difference else-marked then-marked :test #'eq)))
		     (else-release (extract-reference-vars
				    (set-difference then-marked else-marked :test #'eq))))
		(mk-ir-ift (if (or (memq ir-condition livevars)
				   (memq ir-condition then-freevars)
				   (memq ir-condition else-freevars))
			       ir-condition
			     (mk-ir-last ir-condition))
			   (if then-release (mk-ir-release then-release nil (preprocess-ir* ir-then livevars bindings))
			     (preprocess-ir* ir-then livevars bindings))
			   (if else-release (mk-ir-release else-release nil (preprocess-ir* ir-else livevars bindings))
			     (preprocess-ir* ir-else livevars bindings))))))

(defmethod preprocess-ir* ((ir-expr ir-nil) livevars bindings)
  (declare (ignore livevars bindings))
  ir-expr)

(defmethod preprocess-ir* ((ir-expr ir-bool) livevars bindings)
  (declare (ignore livevars bindings))
  ir-expr) 

(defmethod preprocess-ir* ((ir-expr ir-lookup) livevars bindings)
    (with-slots (ir-array ir-index) ir-expr
		(mk-ir-lookup (preprocess-ir* ir-array livevars bindings)
			      (preprocess-ir* ir-index livevars bindings))))

(defmethod preprocess-ir* ((ir-expr ir-update) livevars bindings)
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      (let ((new-ir-target (preprocess-ir* ir-target livevars bindings))
		    (new-ir-lhs (preprocess-ir* ir-lhs livevars bindings))
		    (new-ir-rhs (preprocess-ir* ir-rhs livevars bindings)))
		(unless (ir-last? new-ir-target)
		  (format t "~%Updating an unmarked variable in ~s" (print-ir ir-expr)))
	      (mk-ir-update new-ir-target new-ir-lhs new-ir-rhs))))


(defmethod preprocess-ir* ((ir-expr ir-constructor-update) livevars bindings)
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      (mk-ir-constructor-update (preprocess-ir* ir-target livevars bindings)
			    (preprocess-ir* ir-lhs livevars bindings)
			    (preprocess-ir* ir-rhs livevars bindings))))

(defmethod preprocess-ir* ((ir-expr ir-new) livevars bindings)
  (declare (ignore livevars bindings))
  ir-expr)

(defmethod preprocess-ir* ((ir-expr ir-get) livevars bindings)
    (with-slots (ir-record ir-field) ir-expr
		(mk-ir-get (preprocess-ir* ir-record livevars bindings) ir-field)))

(defmethod preprocess-ir* ((ir-expr list) livevars bindings)
  (cond ((consp ir-expr)
	 (cons (preprocess-ir* (car ir-expr)
			       (union (apply-bindings bindings
						      (pvs2ir-freevars* (cdr ir-expr)))
				      livevars
				      :test #'eq)
			       bindings)
	       (preprocess-ir* (cdr ir-expr)
			       livevars bindings)))
	(t nil)))

(defmethod preprocess-ir* ((ir-expr t) livevars bindings)
  (declare (ignore livevars bindings))
  ir-expr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -------- C constants (implementation dependent) -------- from pvs2c-types.lisp
(defvar *min8-C-int* (- (expt 2 7)))
(defvar *max8-C-int* (- (expt 2 7) 1))
(defvar *max8-C-uli* (- (expt 2 8) 1))
(defvar *min16-C-int* (- (expt 2 15)))
(defvar *max16-C-int* (- (expt 2 15) 1))
(defvar *max16-C-uli* (- (expt 2 16) 1))
(defvar *min32-C-int* (- (expt 2 31)))
(defvar *max32-C-int* (- (expt 2 31) 1))
(defvar *max32-C-uli* (- (expt 2 32) 1))
(defvar *min64-C-int* (- (expt 2 63)))
(defvar *max64-C-int* (- (expt 2 63) 1))
(defvar *max64-C-uli* (- (expt 2 64) 1))
(defvar *min128-C-int* (- (expt 2 127)))
(defvar *max128-C-int* (- (expt 2 127) 1))
(defvar *max128-C-uli* (- (expt 2 128) 1))



;;ir2c/ir2c* works by generating a list of statements for each ir expression.
;;One assumption through this is that whenever a PVS operation is being executed,
;;the data structures always contain non-NULL references, but this might not be
;;the case when an intermediate IR step is taken.  Either way, whenever an IR array/record
;;is released, any nested references are non-NULL and there is no need to check
;;for non-NULLity.

(defun mk-result-var (ir-type)
  (mk-ir-variable 'result ir-type))

(defun ir2c-theory-formals (ir-theory-formals theory-formals)
  (loop for ir-formal in ir-theory-formals
	as tformal in theory-formals
	collect (let ((*pvs2c-current-decl* tformal))
		  (format nil "~a_t ~a"
			  (if (formal-const-decl? tformal)
			      (add-c-type-definition (ir2c-type (ir-vtype ir-formal)))
			    'type_actual)
			  (ir-formal-id ir-formal)))))

(defun ir2c (ir-expr return-type);;this is called on a whole definition with a result
  ;(when (null return-type) (break "ir2c"))
    (ir2c* ir-expr 'result (ir2c-type return-type)))

(defcl if-instr ()
  if-cond
  then-instr
  else-instr)

(defun mk-if-instr (if-cond then-instr else-instr)
  (make-instance 'if-instr
		 :if-cond if-cond
		 :then-instr then-instr
		 :else-instr else-instr))

(defcl for-instr ()
  for-index
  for-body)

(defun mk-for-instr (for-index for-body)
  (make-instance 'for-instr
		 :for-index for-index
		 :for-body for-body))

(defun make-c-application-assignment (lhs lhs-type fname args rhs-type)
  (let ((rhs (format nil "~a(~a)" fname args)))
    (case lhs-type
      ((uint8 uint16 uint32 uint64 __uint128) 
       (case rhs-type
	 (mpz (list (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs)))
	 (mpq (list (let ((tmp (gentemp "tmp")))
		    (format nil "mpz_t ~a" tmp)
		    (format nil "mpz_init(~a)" tmp)
		    (format nil "mpz_set_q(~a, ~a)" tmp rhs)
		    (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs)
		    (format nil "mpz_clear(~a)" tmp))))
	 (t (list (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs)))))
    ((int8 int16 int32 int64 __int128)
     (case rhs-type
       (mpz (list (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs)))
       (mpq (list (let ((tmp (gentemp "tmp")))
		    (format nil "mpz_t ~a" tmp)
		    (format nil "mpz_init(~a)" tmp)
		    (format nil "mpz_set_q(~a, ~a)" tmp rhs)
		    (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs)
		    (format nil "mpz_clear(~a)" tmp))))
       (t (list (format nil "~a = (~a_t)~a" lhs lhs-type rhs)))))
    (mpz (case rhs-type
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (list (format nil "mpz_mk_set_ui(~a, ~a)" lhs rhs)))
	   ((int8 int16 int32 int64); __int128)
	    (list (format nil "mpz_mk_set_si(~a, ~a)" lhs rhs)))
	   (mpq (let ((tmp (gentemp "tmp")))
		  (list (format nil "mpz_mk_set_q(~a, ~a)" lhs rhs)
			)))
	   (t (list ;; (format nil "mpz_ptr_t ~a" tmp)
		 (format nil "~a = (~a_t)~a" lhs "mpz_ptr" rhs)
		      ;;(format nil "~a = (mpz_t) safe_malloc(sizeof(mpz_t))" lhs)
		      ;; (format nil "mpz_init(~a)" lhs)
		      ;; (format nil "mpz_set(~a, ~a)" lhs tmp)
		      ;; (format nil "mpz_clear(~a)" tmp)
		      ))))
    (mpq (case rhs-type
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (list (format nil "~a = (mpq_t) safe_malloc(sizeof(mpq_t))" lhs)
		  (format nil "mpq_init(~a)" lhs)
		  (format nil "mpq_set_ui(~a, ~a, 1)" lhs rhs)))
	   ((int8 int16 int32 int64); __int128)
	    (list (format nil "mpq_mk_set_si(~a, ~a, 1)" lhs rhs)))
	   (mpz  (list (format nil "mpq_mk_set_z(~a, ~a)" lhs rhs)
			))
	   (t (list (format nil "~a = (~a_t)~a" lhs "mpq_ptr" rhs)))))
    (t (list (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))))

(defun mk-c-assignment (lhs lhs-type rhs rhs-type);;mallocs lhs for gmp
  (case lhs-type
    (mpz (case rhs-type
	   (mpz (format nil "mpz_mk_set(~a, ~a)" lhs rhs))
	   (mpq (format nil "mpz_mk_set_q(~a, ~a)" lhs rhs))
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (format nil "mpz_mk_set_ui(~a, ~a)" lhs rhs))
	   ((int8 int16 int32 int64); __int128)
	    (format nil "mpz_mk_set_si(~a, ~a)" lhs rhs))))
    (mpq (case rhs-type
	   (mpz (format nil "mpq_mk_set_z(~a, ~a)" lhs rhs))
	   (mpq (format nil "mpq_mk_set(~a, ~a)" lhs rhs))
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (format nil "mpq_mk_set_ui(~a, ~a, 1)" lhs rhs))
	   ((int8 int16 int32 int64); __int128)
	    (format nil "mpq_mk_set_si(~a, ~a, 1)" lhs rhs))))
    ((uint8 uint16 uint32 uint64); __uint128) 
     (case rhs-type
       (mpz (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs))
       (mpq (format nil "~a = (~a_t)mpq_get_ui(~a)" lhs lhs-type rhs))
       (t (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))
    ((int8 int16 int32 int64); __int128)
     (case rhs-type
       (mpz (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs))
       (mpq (format nil "~a = (~a_t)mpq_get_si(~a)" lhs lhs-type rhs))       
       (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))
    (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))

(defun make-c-assignment (lhs lhs-type rhs rhs-type)
  (case lhs-type
    (mpz (case rhs-type
	   (mpz (format nil "mpz_set(~a, ~a)" lhs rhs))
	   (mpq (format nil "mpz_set_q(~a, ~a)" lhs rhs))
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (format nil "mpz_set_ui(~a, ~a)" lhs rhs))
	   ((int8 int16 int32 int64); __int128)
	    (format nil "mpz_set_si(~a, ~a)" lhs rhs))))
    (mpq (case rhs-type
	   (mpz (format nil "mpq_set_z(~a, ~a)" lhs rhs))
	   (mpq (format nil "mpq_set(~a, ~a)" lhs rhs))
	   ((uint8 uint16 uint32 uint64); __uint128)
	    (format nil "mpq_set_ui(~a, ~a, 1)" lhs rhs))
	   ((int8 int16 int32 int64); __int128)
	    (format nil "mpq_set_si(~a, ~a, 1)" lhs rhs))))
    ((uint8 uint16 uint32 uint64); __uint128) 
     (case rhs-type
       (mpz (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs))
       (mpq (format nil "~a = (~a_t)mpq_get_ui(~a)" lhs lhs-type rhs))
       (t (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))
    ((int8 int16 int32 int64); __int128)
     (case rhs-type
       (mpz (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs))
       (mpq (format nil "~a = (~a_t)mpq_get_si(~a)" lhs lhs-type rhs))       
       (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))
    (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))


	    

;;deprecated pvs2ir now generates ir-function symbols as below
;; (defmethod ir2c* ((ir-expr symbol) return-var return-type)
;;   (let ((rhs (if (eq ir-expr 'TRUE) (format nil "~a" 'true)
;; 	       (if (eq ir-expr 'FALSE) (format nil "~a" 'false)
;; 		 (format nil "~a()" ir-expr))))
;; 	(c-return-type (add-c-type-definition (ir2c-type return-type))))
;;     (list (make-c-assignment return-var c-return-type (format nil "~a = (~a_t)~a" return-var c-return-type rhs)))))

;;Due to eta-expansion with let-expressions, the ir-function case can only arise for constants.
(defmethod ir2c* ((ir-expr ir-function) return-var return-type)
  (with-slots (ir-fname declaration) ir-expr ;;emulating the symbol method
	      (let ((rhs (format nil "~a()" ir-fname))
		    (c-return-type (add-c-type-definition (ir2c-type return-type)))
		    (rhs-type (add-c-type-definition (ir2c-type (type declaration)))))
		(list (mk-c-assignment return-var c-return-type rhs rhs-type)))))


(defmethod ir2c* ((ir-expr ir-bool) return-var return-type)
  (with-slots (ir-boolval) ir-expr
    (list (format nil "~a = (bool_t) ~a" return-var (if ir-boolval "true" "false")))))

(defmethod ir2c* ((ir-expr ir-integer) return-var return-type);;we need to handle number representations. 
  (with-slots (ir-intval) ir-expr
	      (let* ((ir-value-type (mk-ir-subrange ir-intval ir-intval))
		     (c-value-type (ir2c-type ir-value-type))
		     (c-return-type (print-ir return-type))
		     (c-assignment (mk-c-assignment return-var c-return-type
						      ir-intval c-value-type)))
		(case c-return-type
		  ((mpz mpq)
		   (list (format nil "~a = safe_malloc(sizeof(~a_t))"
				 return-var c-return-type)
			 (format nil "~a_init(~a)" c-return-type return-var)
			 c-assignment))
		  (t (list c-assignment))))))
		;;     (suffix (if (>= ir-intval 0) "_ui" "_si")))
		;; (list (case c-return-type ;;this isn't correct for mpq (NSH 5-15-16)
		;; 	(mpz (format nil "mpz_set~a(~a, ~a)" suffix return-var ir-intval))
		;; 	(mpq (format nil "mpq_set~a(~a, ~a, 1)" suffix return-var ir-intval))
		;; 	(t (format nil "~a = (~a_t)~a" return-var c-return-type ir-intval)))))))


(defmethod ir2c* ((ir-expr ir-last) return-var return-type)
  (with-slots (ir-var) ir-expr
	      (with-slots (ir-name ir-vtype) ir-var
			  (let* ((c-return-type (add-c-type-definition (ir2c-type return-type)))
				 (c-rhs-type (add-c-type-definition (ir2c-type ir-vtype)))
				 (c-assignment (mk-c-assignment return-var c-return-type ir-name c-rhs-type)))
			    ;(format t "~% last: ~a, return-var: ~a, c-return-type:~a, c-rhs-type: ~a" (print-ir ir-var)(print-ir return-var) c-return-type c-rhs-type)
			    (case c-return-type
			      ((mpq mpz)
			       (let ((return-init-instrs nil); already initialized
				     ;; (list (format nil "~a = safe_malloc(sizeof(~a_t))"
				     ;; 		    return-var c-return-type)
				     ;; 	    (format nil "~a_init(~a)" c-return-type return-var))
				     )
				 (case c-rhs-type
				   ((mpq mpz)
				    (append return-init-instrs
					    (if (member ir-var *mpvar-parameters*)
						(list c-assignment)
					      (cons c-assignment
						    (list (format nil "~a_clear(~a)"
								  c-rhs-type ir-name))))))
				   (t (append return-init-instrs
					    (list c-assignment))))))
			      (t (case c-rhs-type
				   ((mpq mpz);(break "ir-last")
				    (if (member ir-var *mpvar-parameters*)
					(list c-assignment)
				      (cons c-assignment
					    (list (format nil "~a_clear(~a)"
							  c-rhs-type ir-name)))))
				   (t (list c-assignment)))))))))

(defmethod ir2c* ((ir-expr ir-variable) return-var return-type)
  (with-slots (ir-name ir-vtype) ir-expr
	      (let* ((c-return-type (add-c-type-definition (ir2c-type return-type)))
		     (c-rhs-type (add-c-type-definition (ir2c-type ir-vtype)))
		     (c-assignment (mk-c-assignment return-var c-return-type ir-name c-rhs-type)))
		(case c-return-type
		  ((mpq mpz)
		   (list (format nil "~a = safe_malloc(sizeof(~a_t))"
				 return-var c-return-type)
			 (format nil "~a_init(~a)" c-return-type return-var)
			 c-assignment))
		  (t (if (ir-reference-type? ir-vtype)
			 (list c-assignment ;;assign then increment refcount
			       (format nil "~a->count++" ir-name));;ir-name contains non-NULL
		       (list c-assignment)))))))



(defmethod ir2c* ((ir-expr ir-type-actual) return-var return-type)
  (with-slots (ir-actual-type) ir-expr
	      (let ((c-return-type (add-c-type-definition (ir2c-type return-type)))
		    (c-type (add-c-type-definition (ir2c-type ir-actual-type))));(break "ir2c*(actual)")
		(list (mk-c-assignment return-var c-return-type
					 (format nil "actual_~a" c-type)
					 c-type)))))

(defmethod ir2c* ((ir-expr ir-const-actual) return-var return-type)
  (with-slots (ir-actual-expr) ir-expr
	      (ir2c* ir-actual-expr return-var return-type)))

(defun gmp-type? (x) (or (eq x 'mpz)(eq x 'mpq)))

(defmethod ir2c* ((ir-expr ir-record) return-var return-type)
  (with-slots (ir-fields ir-recordtype) ir-expr
	      (let ((ctype (add-c-type-definition (ir2c-type ir-recordtype))))
		 (cons (format nil "~a = new_~a();" return-var ctype)
		       (loop for fld in ir-fields
			     as fldtype in (ir-field-types ir-recordtype)
			     append
			     (with-slots (ir-fieldname ir-value) fld
					 (let* ((rhs-var (get-ir-last-var ir-value))
						(c-rhs-type (add-c-type-definition (ir2c-type (ir-vtype rhs-var)))))
					   (cons (make-c-assignment (format nil "~a->~a"  return-var ir-fieldname)
								  (add-c-type-definition (ir2c-type (ir-ftype fldtype)))
								  (ir-name rhs-var)
								  c-rhs-type)
						 (cond ((and (ir-reference-type? (ir-vtype rhs-var))
							     (not (ir-last? ir-value)))
							(list (format nil "~a->count++" (ir-name rhs-var))))
						       ((and (ir-last? ir-value)
							     (gmp-type? c-rhs-type)
							     (not (member rhs-var *mpvar-parameters*)))
							(list (format nil "~a_clear(~a)" c-rhs-type
								      (ir-name rhs-var))
							      ))
						       (t nil))))))))))
						      

(defun ir-record-field-type (rectype field)
  (with-slots (ir-field-types) rectype
	      (ir-record-field-type-rec ir-field-types field)))

(defun ir-record-field-type-rec (field-types field)
  (cond ((consp field-types)
	 (with-slots (ir-id ir-ftype) (car field-types)
		     (or (and (eq ir-id field)
			      ir-ftype)
			 (ir-record-field-type-rec (cdr field-types) field))))
	(t nil)))

(defmethod get-ir-type-value ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      (get-ir-type-value ir-type-defn)))

(defmethod get-ir-type-value ((ir-type t))
;  (break "get-ir-type-value")
  ir-type)


(defun get-field-type (ir-recordtype field)
  (with-slots (ir-field-types) ir-recordtype
	      (loop for ftype in ir-field-types
		    when (eq (ir-id ftype) field)
		    return (ir-ftype ftype))))

(defmethod ir2c* ((ir-expr ir-get) return-var return-type)
  (with-slots (ir-record ir-field) ir-expr
	      (let* ((ir-record-var (get-ir-last-var ir-record))
		     (c-return-type (add-c-type-definition (ir2c-type return-type)))
		     (c-field-type (add-c-type-definition
					(ir2c-type
					 (get-field-type
					  (get-ir-type-value
					   (ir-vtype ir-record-var))
					  ir-field))))
		     (assign-instr
		      (make-c-assignment return-var c-return-type
				       (format nil "~a->~a" (ir-name ir-record-var) ir-field)
				       c-field-type))
		     (assign-instrs
		      (case c-return-type
			((mpq mpz)
			 (list (format nil "~a = safe_malloc(sizeof(~a_t))" return-var c-return-type)
			       (format nil "~a_init(~a)" c-return-type return-var)
			       assign-instr))
			(t (list assign-instr))))
		     (refcount-get-instr	;if lookup is a reference, first bump its count
		      (when (ir-reference-type? (ir-record-field-type (get-ir-type-value (ir-vtype ir-record-var)) ir-field))
			(list (format nil "~a->count++"  return-var))))
		     (release-record-instr
		      (when (ir-last? ir-record) ;if last, then release the array
			(list (release-last-var ir-record-var)))))
		(append assign-instrs (append refcount-get-instr release-record-instr)))))

  
(defvar *max-PVS-array-size* 32768);;Chosen arbitrarily to be 2^15, but tunable by the user

(defmethod ir-index? ((ir-typ ir-subrange))
  (with-slots (ir-low ir-high) ir-typ
	      (and (eql (ir-low ir-typ) 0)
		   (integerp ir-high)
		   (>= ir-high ir-low)
		   (< ir-high *max-PVS-array-size*)
		   (1+ ir-high))))

(defmethod ir-index? ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
	      (ir-index? ir-type-defn)))

(defmethod ir-index? ((ir-type t))
  nil)

;;only used when ir-index? is nil - returns the index type
(defun ir-hashable-index? (ctype)
  (case ctype
    ((uint64 __uint128 int64 __int128) 'uint64);64/128 get coerced to uint64.
    ((int8 int16 int32 uint8 uint16 uint32) 'uint32)
    (t nil)))
	 ;;using ir2c-type to return u/intx or mpz

(defmethod ir-array? ((ir-typ ir-funtype))
  (with-slots (ir-domain ir-range) ir-typ
	      (ir-index? ir-domain)));;add one to high to get array size

(defmethod ir-array? ((ir-typ ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-typ
	      (ir-array? ir-type-defn)))

(defmethod ir-array? ((ir-typ t))
  nil)

(defun get-ir-last-var (ir-last-or-var) ;;extracts the variable from (last x) or x. 
  (if (ir-last? ir-last-or-var)
      (ir-var ir-last-or-var)
    ir-last-or-var))

(defun ir-lvariable? (ir-last-or-var)
  (or (ir-variable? ir-last-or-var)(ir-last? ir-last-or-var)))

;;The reference counting on the last occurrence is done by the client: ir2c*(ir-apply)
;;An application is always assigned to a variable of the same type - casting is purely
;;between IR variables
(defun ir2c-function-apply (return-var return-type ir-function ir-args)
    (let* ((ir-arg-vars (loop for ir-var in ir-args
			      collect (get-ir-last-var ir-var)))
	   (ir-arg-var-names (loop for ir-var in ir-arg-vars
				   collect (ir-name ir-var)))
	   (ir-function-name (when (ir-function? ir-function)(ir-fname ir-function))))
      (if (ir-primitive-op? ir-function-name)
	  (let ((instrs (ir2c-primitive-apply return-var return-type ir-function-name ir-arg-vars ir-arg-var-names)))
					;(format t "~%~a" instrs)
	    instrs
	    ;; (if (mpnumber-type? return-type)
	    ;; 	(cons (format nil "~a = safe_malloc(sizeof(~a_t))" return-var  return-type)
	    ;; 	      (cons (format nil "~a_init(~a)" return-type return-var) instrs))
	    ;;   instrs)
	    )
	(let* ((ir-funvar (get-ir-last-var ir-function))
	       (c-return-type (add-c-type-definition (ir2c-type return-type))))
	  (if (ir-variable? ir-funvar)
	      (let* ((arg-string (format nil "~{~a~^, ~}" ir-arg-var-names))
		     (ir-fun-name (if (eql (length ir-arg-vars) 1)
				      (format nil "~a->ftbl->fptr" (ir-name ir-funvar))
				    (format nil "~a->ftbl->mptr" (ir-name ir-funvar))))
		     (apply-instr
		      (format nil "~a = (~a_t)~a(~a, ~a)"  return-var
			      (mppointer-type c-return-type)
			      ir-fun-name (ir-name ir-funvar) arg-string)
		      ;; (case c-return-type
		      ;; 	    ((mpz mpq)
		      ;; 	     (format nil "~a(~a, ~a, ~a)" ir-fun-name (ir-name ir-funvar) return-var arg-string))
		      ;; 	    (t (format nil "~a = (~a_t)~a(~a, ~a)"  return-var  c-return-type
		      ;; 		       ir-fun-name (ir-name ir-funvar) arg-string)))
		      )
		     (closure-release-instrs (when (ir-last? ir-function)
					       (list (format nil "~a->ftbl->rptr(~a)"
							     (ir-name ir-funvar)
							     (ir-name ir-funvar))))))
		(cons apply-instr closure-release-instrs))
	    (let* ((fdecl (declaration ir-function))
		   (function-ir (ir (eval-info fdecl)))
		   (ir-return-type (ir-return-type function-ir))
		   (function-has-closure-value? (and (null (ir-args function-ir))(ir-funtype? ir-return-type)))
		   (ir-funarg-types (or (and (cdefn (eval-info fdecl))
					     (op-arg-types (cdefn (eval-info fdecl))))
					(loop for ir-formal in (ir-args function-ir)
					      collect (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))))
		   (c-fun-range-type (or (and (cdefn (eval-info fdecl))
					      (op-return-type (cdefn (eval-info fdecl))))
					 (add-c-type-definition (ir2c-type ir-return-type))))
		   
		   (arg-pairs (loop for argtype in ir-funarg-types
				    as arg in ir-arg-var-names
				    collect (case argtype
					      ((mpz mpq)(format nil "~a" arg))
					      (t (format nil "(~a_t)~a" argtype arg)))))
		   (arg-string (format nil "~{~a~^, ~}" arg-pairs)))
	      ;(when (ir-function? ir-function-name) (break "ir-function"))
	       (make-c-application-assignment return-var c-return-type
					      ir-function-name arg-string
					      c-fun-range-type)))))))
				       
	       ;; (format nil "~a = (~a_t)~a(~a)"  return-var (mppointer-type c-return-type)
	       ;; 		    ir-function-name arg-string)
		    ;; (case c-return-type
		    ;;   ((mpz mpq) (format nil "~a(~a, ~a)"  ir-function-name  return-var arg-string))
		    ;;   (t (format nil "~a = (~a_t)~a(~a)"  return-var c-return-type ir-function-name arg-string)))


(defun gmp-suffix (c-type)
  (case c-type
    ((int8 int16 int32 int64 __int128) "_si")
    ((uint8 uint16 uint32 uint64 __uint128) "_ui")
    (t "")))

(defun ir-primitive-op? (ir-function-name)
  (memq ir-function-name *ir-primitives*))

(defun ir-primitive-arith-op? (ir-function-name)
  (memq ir-function-name *ir-arith-primitives*))

(defun tweak-equal (ir-function-name)
  (case ir-function-name
    (= '==)
    (t ir-function-name)))

(defun ir2c-equality (return-var c-return-type ir-arg-names c-arg-types)
  (case (car c-arg-types)
    ((int8 int16 int32 int64 __int128 uint8 uint16 uint32 uint64 __uint128 mpz mpq)
     (ir2c-arith-relations '== return-var ir-arg-names c-arg-types))
    (t (list (format nil "~a = (~a_t) equal_~a(~a, ~a)"
		     return-var c-return-type (car c-arg-types) (car ir-arg-names)
		     (cadr ir-arg-names))))))

(defun numtype-abbrev (ctype)
  (case ctype
    ((int8 int16 int32 int64 int128) 'i64)
    ((uint8 uint16 uint32 uint64 uint128) 'u64)
    (mpz 'z)
    (mpq 'q)
    (t "")))

(defun ir2c-primitive-apply (return-var return-type ir-function-name ir-args ir-arg-names)
  (cond ((ir-primitive-op? ir-function-name);
	 (let ((c-arg-types (loop for ir-var in ir-args
				collect (add-c-type-definition (ir2c-type (ir-vtype ir-var)))))
	       (c-return-type (add-c-type-definition (ir2c-type return-type)))
					;(arity (length ir-args))
	       )
	   ;(format t "~%c-arg-types: ~a" c-arg-types)
	   (case ir-function-name
	     (+ (ir2c-addition return-var c-return-type ir-arg-names c-arg-types))
	     (- (ir2c-subtraction return-var c-return-type ir-arg-names c-arg-types))
	     (* (ir2c-multiplication  return-var c-return-type ir-arg-names c-arg-types))
	     (/ (ir2c-division return-var c-return-type ir-arg-names c-arg-types))
	     (= (ir2c-equality return-var c-return-type ir-arg-names c-arg-types))
	     (/= (ir2c-arith-relations '!= return-var ir-arg-names c-arg-types))
	     ((ndiv nrem) (ir2c-divrem ir-function-name return-var c-return-type ir-arg-names c-arg-types))
	     ((< <= > >=) (ir2c-arith-relations ir-function-name ;(tweak-equal ir-function-name)
						  return-var
						  ir-arg-names c-arg-types))
	     (floor (list (format nil "~a = (~a_t)pvsfloor_~a_~a(~a)"
				  return-var (mppointer-type return-type)
				  (numtype-abbrev (car c-arg-types))
				  (numtype-abbrev return-type)
				  (car ir-arg-names))))
	     (ceiling (list (format nil "~a = (~a_t)pvsceiling_~a_~a(~a)"
				  return-var (mppointer-type return-type)
				  (numtype-abbrev (car c-arg-types))
				  (numtype-abbrev return-type)
				  (car ir-arg-names))))	     
	     (NOT (list (format nil "~a = !~a" return-var (car ir-arg-names) )))
	     (OR
	      (list (format nil "~a = ~a || ~a" return-var (car ir-arg-names) 
		      (cadr ir-arg-names))))
	     (AND
	      (list (format nil "~a = ~a && ~a" return-var (car ir-arg-names) 
			    (cadr ir-arg-names))))
	     (IMPLIES (list (format nil "~a = (!~a) ||  ~a" return-var (car ir-arg-names)
				    (cadr ir-arg-names))))
	     (WHEN (list (format nil "~a = ~a || ! ~a" return-var (car ir-arg-names)
				 (cadr ir-arg-names))))
	     (IFF (list (format nil "~a = (~a || ! ~a) && ((!~a) ||  ~a)" return-var
				(car ir-arg-names)  (cadr ir-arg-names)
				(car ir-arg-names)  (cadr ir-arg-names)))))))
	(t (break "not defined"))))

(defun ir2c-division (return-var c-return-type ir-args c-arg-types)
  (let ((arg1 (car ir-args))
	(arg2 (cadr ir-args))
	(arg1-c-type (car c-arg-types))
	(arg2-c-type (cadr c-arg-types)))
    (ir2c-division-step return-var c-return-type arg1
			arg1-c-type arg2
			arg2-c-type)))

(defun gmp-ui-or-si (c-type)
  (case c-type
    ((uint8 uint16 uint32 uint64 __uint128) "ui")
    ((int8 int16 int32 int64 __int128) "si")
    (t "")))

(defun uint-or-int (c-type)
  (case c-type
    ((uint8 uint16 uint32 uint64 __uint128) "uint")
    ((int8 int16 int32 int64 __int128) "int")
    (t "")))

(defun ir2c-division-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (mpq (case arg1-c-type
	   (mpq
	    (case arg2-c-type
	      (mpq (list (format nil "mpq_div(~a, ~a, ~a)" return-var arg1 arg2)))
	      (mpz (let ((arg2-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg2-mpq-var)
			   (format nil "mpq_init(~a)" arg2-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 arg2-mpq-var)
			   (format nil "mpq_clear(~a)" arg2-mpq-var)
			   )))
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (list (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
			     (gmp-ui-or-si arg2-c-type)
			     return-var (uint-or-int arg2-c-type) arg2)
		     (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 return-var)))
	      (t (break "Not implemented yet"))))
	   (mpz
	    (case arg2-c-type
	      (mpq (let ((arg1-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg1-mpq-var)
			   (format nil "mpq_init(~a)" arg1-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var arg2)
			   (format nil "mpq_clear(~a)" arg1-mpq-var)
			   )))
	      (mpz (let ((arg1-mpq-var (gentemp "tmp"))
			 (arg2-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg1-mpq-var)
			   (format nil "mpq_init(~a)" arg1-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
			   (format nil "mpq_t ~a" arg2-mpq-var)
			   (format nil "mpq_init(~a)" arg2-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var arg2-mpq-var)
			   (format nil "mpq_clear(~a)" arg1-mpq-var)
			   (format nil "mpq_clear(~a)" arg2-mpq-var)
			   )))
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (let ((arg1-mpq-var (gentemp "tmp")))
		 (list (format nil "mpq_t ~a" arg1-mpq-var)
		       (format nil "mpq_init(~a)" arg1-mpq-var)
		       (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
		       (format nil "mpq_mk_set_~a(~a, (~a_t)~a, 1)"
			       (gmp-ui-or-si arg1-c-type)
			       return-var (uint-or-int arg1-c-type) arg2)
		       (format nil "mpq_div(~a, ~a, ~a)" return-var
			       arg1-mpq-var return-var)
		       (format nil "mpq_clear(~a)" arg1-mpq-var)
		       )))
	      (t (break "Not implemented yet."))))
	   ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	     (case arg2-c-type
	       (mpq (let ((tmp (gentemp "tmp")))
		      (list (format nil "mpq_t ~a" tmp)
			    (format nil "mpq_init(~a)" tmp)
			    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
				  (gmp-ui-or-si arg1-c-type)
				  tmp (uint-or-int arg1-c-type) arg1)
			  (format nil "mpq_mk_div(~a, ~a, ~a)" return-var tmp arg2))))
	       (mpz (let ((arg1-mpq-var (gentemp "tmp"))
			  (arg2-mpq-var (gentemp "tmp")))
		      (list (format nil "mpq_t ~a" arg1-mpq-var)
			    (format nil "mpq_init(~a)" arg1-mpq-var)
			    (format nil "mpq_t ~a" arg2-mpq-var)
			    (format nil "mpq_init(~a)" arg2-mpq-var)
			    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)" (gmp-ui-or-si arg1-c-type)
				    arg1-mpq-var (uint-or-int arg1-c-type) arg1)
			    (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			    (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var
				    arg2-mpq-var)
			    (format nil "mpq_clear(~a)" arg1-mpq-var)
			    (format nil "mpq_clear(~a)" arg2-mpq-var)
			    )))
	       ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
		(let ((arg1-mpq-var (gentemp "tmp"))
		      (arg2-mpq-var (gentemp "tmp")))
		  (list (format nil "mpq_t ~a" arg1-mpq-var)
			(format nil "mpq_init(~a)" arg1-mpq-var)
			(format nil "mpq_t ~a" arg2-mpq-var)
			(format nil "mpq_init(~a)" arg2-mpq-var)
			(format nil "mpq_set_~a(~a, (~a64_t)~a, 1)" (gmp-ui-or-si arg1-c-type)
				arg1-mpq-var (uint-or-int arg1-c-type) arg1)
			(format nil "mpq_set_~a(~a, ~a, 1)" (gmp-ui-or-si arg2-c-type)
				arg2-mpq-var arg2)
			(format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var
				arg2-mpq-var)
			(format nil "mpq_clear(~a)" arg1-mpq-var)
			(format nil "mpq_clear(~a)" arg2-mpq-var)
			)))
	       (t (break "Not implemented yet."))))
	   (t (break "Not implemented yet."))))
    ((__int128 __uint128) (break "Not implemented yet."))
    (mpz (let* ((mpq-return-var (gentemp "tmp"))
		(mpq-instrs (ir2c-division-step mpq-return-var "mpq" arg1 arg1-c-type arg1 arg2-c-type)))
	   (cons (format nil "mpq_t ~a" mpq-return-var)
		 (nconc mpq-instrs
			(list (format nil "mpz_mk_set_q(~a, ~a)" return-var mpq-return-var)
			      (format nil "mpq_clear(~a)" mpq-return-var)
			      )))))
    ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
     (let* ((mpz-return-var (gentemp "tmp"))
	    (mpz-instrs (ir2c-division-step mpz-return-var "mpz" arg1 arg1-c-type arg1 arg2-c-type)))
       (cons (format nil "mpz_t ~a" mpz-return-var)
	     (nconc mpz-instrs
		    (list (format nil "mpz_mk_set_q(~a, ~a)" return-var mpz-return-var)
			  (format nil "mpq_clear(~a)" mpz-return-var)
			  )))))
    (t (break "Not implemented."))))

	    
(defun ir2c-sqrt (return-var arg);both have to be of type mpq
  (let ((tmp1 (gentemp "tmp"))
	(tmp2 (gentemp "tmp")))
    (list  (format nil  "mpf_t ~a, ~a" tmp1 tmp2)
	   (format nil "mpf_init(~a)" tmp1)
	   (format nil "mpf_init(~a)" tmp2)
	   (format nil "mpf_set_q(~a, ~a)" tmp1 arg)
	   (format nil "mpf_sqrt(~a, ~a)" tmp2 tmp1)
	   (format nil "mpz_set_f(~a, ~a)" return-var tmp2)
	   (format nil "mpf_clear(~a)" tmp1)
	   (format nil "mpf_clear(~a)" tmp2))))

(defun ir2c-divrem (op return-var c-return-type ir-arg-names c-arg-types)
  (let ((arg1 (car ir-arg-names))
	(arg2 (cadr ir-arg-names))
	(arg1-c-type (car c-arg-types))
	(arg2-c-type (cadr c-arg-types))
	(opname (if (eq op 'nrem) 'rem 'div)))
    (let ((arg1-type (case arg1-c-type
		       ((uint8 uint16 uint32) 'uint32)
		       ((int8 int16 int32) 'int32)
		       (t arg1-c-type)))
	  (arg2-type (case arg2-c-type
		       ((uint8 uint16 uint32) 'uint32)
		       (t arg2-c-type))))
      (case c-return-type
	(mpz
	 (case arg1-type
	   (mpz (case arg2-type
		  (mpz (list (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				     (if (eq opname 'rem) 'r 'q) return-var arg1 arg2)))
		  (t (list (format nil "mpz_mk_fdiv_~a_ui(~a, ~a, ~a)"
				   (if (eq opname 'rem) 'r 'q) return-var arg1 arg2)))))
	   ((uint32 uint64); __uint128)
	    (case arg2-type
	      (mpz (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_ui(~a, ~a)" tmp arg1)
			   (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				   (if (eq opname 'rem) 'r 'q) return-var tmp arg2)
			   (format nil "mpz_clear(~a)" tmp)
			   )))
		(t (list (format nil "mpz_mk_set_ui(~a, ~a_~a_~a(~a, ~a))"
				 return-var opname arg1-type arg2-type arg1 arg2)))))
	   ((int32 int64); __int128)
	    (case arg2-type 
	      (mpz (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_si(~a, ~a)" tmp arg1)
			   (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				   (if (eq opname 'rem) 'r 'q) return-var tmp arg2)
			   (format nil "mpz_clear(~a)" tmp)
			   )))
	      (t (list (format nil "mpz_mk_set_si(~a, ~a_~a_~a(~a, ~a))"
			       return-var opname arg1-type arg2-type arg1 arg2)))))))
	(t (list (format nil "~a = (~a_t)~a_~a_~a(~a, ~a)"
			 return-var c-return-type opname arg1-type arg2-type arg1 arg2)))))))

(defun ir2c-arith-relations (ir-function-name return-var
			      ir-arg-names c-arg-types)
  (let ((arg1 (car ir-arg-names))
	(arg2 (cadr ir-arg-names))
	(arg1-c-type (car c-arg-types))
	(arg2-c-type (cadr c-arg-types)))
    (let ((instrs 
    (ir2c-arith-relations-step ir-function-name return-var
			       arg1 arg1-c-type arg2 arg2-c-type)))
      ;(format t "ir2c-arith-relations-step: ~%~a" instrs)
      instrs)))

(defun ir2c-arith-relations-step
  (ir-function-name return-var
		    arg1 arg1-c-type arg2 arg2-c-type)
    (case arg1-c-type
      ((int8 int16 int32 int64); __int128)
       (case arg2-c-type
	 ((int8 int16 int32 int64); __int128)
	  (list (format nil "~a = (~a ~a ~a)"
			return-var arg1 ir-function-name arg2)))
	 ((uint8 uint16 uint32 uint64 __uint128)
	  (list (mk-if-instr (format nil "(~a < 0)" arg1)
			     (list (format nil "~a = ~a"
					   return-var
					   (case ir-function-name
					     ((< <=) "true")
					     (t "false"))))
			     (list (format nil "~a = ((~a_t)~a ~a ~a)"
					   return-var arg2-c-type
					   arg1 ir-function-name arg2)))))
	 (mpz (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpz_cmp_si(~a, ~a)" tmp arg2 arg1)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      (arith-relation-inverse ir-function-name)))))
	 (mpq (break "mpq comparison not implemented")))) 
      ((uint8 uint16 uint32 uint64); __uint128)
       (case arg2-c-type
	 ((uint8 uint16 uint32 uint64); __uint128)
	  (list (format nil "~a = (~a ~a ~a)"
			return-var arg1 ir-function-name arg2)))
	 ((int8 int16 int32 int64); __int128)
	  (list (mk-if-instr (format nil "(~a < 0)" arg2)
			     (list (format nil "~a = ~a"
					   return-var
					   (case ir-function-name
					     ((< <=) "false")
					     (t "true"))))
			     (list (format nil "~a = (~a ~a (~a_t)~a)"
					   return-var 
					   arg1 ir-function-name arg1-c-type arg2)))))
	 (mpz (let ((tmp (gentemp "tmp")))
		      (list (format nil "int64_t ~a = mpz_cmp_ui(~a, ~a)" tmp arg2 arg1)
			    (format nil "~a = (~a ~a 0)" return-var tmp
				    ir-function-name))))
	 (mpq (break "mpq comparison not implemented"))))
      (mpz (case arg2-c-type
	     ((uint8 uint16 uint32 uint64 int8 int16 int32 int64);__uint128 __int128)
	      (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpz_cmp_~a(~a, ~a)" tmp
			      (gmp-ui-or-si arg2-c-type)
			   arg1 arg2)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      ir-function-name))))
	     (mpz (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpz_cmp(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name))))
	     (mpq (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp_z(~a, ~a)" tmp
				  arg2 arg1)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  (arith-relation-inverse ir-function-name)))))))
      (mpq (case arg2-c-type
	     ((uint8 uint16 uint32 uint64 int8 int16 int32 int64);__uint128 __int128)
	      (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpq_cmp_~a(~a, ~a, 1)" tmp
			      (gmp-ui-or-si arg2-c-type)
			   arg1 arg2)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      ir-function-name))))
	     (mpz (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp_z(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name))))
	     (mpq (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name)
			  )))))))


(defun arith-relation-inverse (ir-relation)
  (case ir-relation
    (< '>=)
    (<= '>)
    (> '<=)
    (>= '<)
    (!= '!=)))

(defun intsize (u)
  (case u
    ((int8 uint8) 8)
    ((int16 uint16) 16)
    ((int32 uint32) 32)
    (t 64)))

(defun uintlub (&rest args)
  (let ((max (apply #'max (loop for u in args collect (intsize U)))))
    (format nil "uint~a" (max (intsize u1)(intsize u2)))))
  
  
(defun ir2c-addition (return-var c-return-type ir-args c-arg-types)
  (let ((arg1 (car ir-args))
	(arg2 (cadr ir-args))
	(arg1-c-type (car c-arg-types))
	(arg2-c-type (cadr c-arg-types)))
    (ir2c-addition-step return-var c-return-type arg1
			arg1-c-type arg2
			arg2-c-type)))

;;mpz_add_ui/si operation can be done in place.
(defun ir2c-addition-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (mpz (case arg1-c-type
	   ((uint8 uint16 uint32 uint64);  __uint128
	    (case arg2-c-type
	      ((uint8 uint16 uint32 uint64)
	       (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg1)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
	      ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
	      ((int8 int16 int32 int64)
	       (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg1)))
	      (mpz (list (format nil "mpz_mk_add_ui(~a, ~a, (uint64_t)~a)" return-var arg2 arg1)))
	      (mpq (list (format nil "mpz_mk_set_q(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_ui(~a, ~a, ~a)" return-var return-var arg1)))))
	   ((int8 int16 int32 int64)
	    (case arg2-c-type
	      ((uint8 uint16 uint32 uint64)
	       (list (format nil "mpz_mk_set_si(~a, (uint64_t)~a)" return-var arg1)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
	      ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
	      ((int8 int16 int32 int64)
	       (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg1)
		     (format nil "mpz_add_si(~a, ~a (int64_t)~a)" return-var return-var arg2)))
	      (mpz (list (format nil "mpz_mk_add_si(~a, ~a, (int64_t)~a)" return-var arg2 arg1)))
	      (mpq (list (format nil "mpz_mk_set_q(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_si(~a, ~a, ~a)" return-var return-var arg1)))))
	   (mpz (case arg2-c-type
		  ((uint8 uint16 uint32 uint64)
		   (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg2)
			 (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var arg1 arg2)))
		  ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		  ((int8 int16 int32 int64)
		   (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var arg2 arg1)))
		  (mpz (list (format nil "mpz_mk_add(~a, ~a, ~a)" return-var arg2 arg1)))
		  (mpq (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpz_t ~a" tmp)
			       (format nil "mpz_init(~a)" tmp)
			       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
			       (format nil "mpz_mk_add(~a, ~a, ~a)" return-var arg1 tmp)
			       (format nil "mpz_clear(~a)" tmp)
			       )))))
	   (t (break "Not implemented."))))
    ((uint8 uint16 uint32 uint64); __uint128)
     (case arg1-c-type
       ((uint8 uint16 uint32 uint64); __uint128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64); __uint128)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type
			 arg1 arg2)));args don't need to be cast
	  ((int8 int16 int32 int64);  __int128)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)((~a_t)~a - (~a_t)(-~a)) ;} else {~a =  (~a_t)(~a + ~a);}"
	   arg2   ;;since return type is unsigned, subtraction is always non-negative.
	   return-var c-return-type lub lub arg1 arg2
	   return-var c-return-type arg1  arg2))))
	  (mpz (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_ui(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	 (mpq (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_ui(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       ((int8 int16 int32 int64 __int128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64 __uint128)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg1
					     return-var c-return-type arg2 lub arg1
					     return-var c-return-type arg1  arg2))))
	  ((int8 int16 int32 int64);  __int128)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type arg1 arg2)))
	  (mpz (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	  (mpq (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       (mpz (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
       (mpq (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
       (t (break "Not implemented."))))
    ((int8 int16 int32 int64) ; __int128)
     (case arg1-c-type
       ((uint8 uint16 uint32 uint64);  __uint128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64);  __uint128)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type arg1 arg2)))
	  ((int8 int16 int32 int64);  __int128)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg2
					     return-var c-return-type arg1 lub arg2
					     return-var c-return-type arg1  arg2))))
	  (mpz (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	  (mpq (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       ((int8 int16 int32 int64); __int128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64); __uint128);
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg1
					     return-var c-return-type arg2 lub arg1
					     return-var c-return-type arg1  arg2))))
	  ((int8 int16 int32 int64); __int128)
	   (list (format nil "~a = (~a_t)((int64_t)~a + (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (mpz (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
	  (mpq (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))))
       (mpz (case arg2-c-type
	      (mpz (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_add(~a, ~a, ~a)" tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_si(~a)"
				   return-var c-return-type tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   )))
	      (mpq (let ((tmp1 (gentemp "tmp"))
			 (tmp2 (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp1)
			   (format nil "mpz_init(~a)" tmp1)
			   (format nil "mpz_set_q(~a)" arg2)
			   (format nil "mpz_t ~a" tmp2)
			   (format nil "mpz_init(~a)" tmp2)
			   (format nil "mpz_add(~a, ~a, ~a)" tmp2 arg1 tmp2)
			   (format nil "~a = (~a_t)mpz_get_si(~a)"
				   return-var c-return-type tmp2)
			   (format nil "mpz_clear(~a)" tmp1)
			   (format nil "mpz_clear(~a)" tmp2)
			   )))
	      ((__int128 __uint128)(break "128-bit GMP arithmetic not available."))
	      (t (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_add_~a(~a, ~a, ~a)"
				   (gmp-ui-or-si arg2-c-type)
				   tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)"
				   return-var c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   )))))))))


	 
		  
  ;; (case arg1-c-type
    
  ;;   (uint32 (case arg2-c-type
  ;; 		  (uint32 (case c-return-type
  ;; 			      (uint32
  ;; 			       (list (format nil "~a = ~a + ~a" return-var arg1 arg2)))
  ;; 			      (int32 (format nil "~a = (int32_t) (~a + ~a)" return-var arg1 arg2))
  ;; 			      (mpz (list (format nil "mpz_set_ui(~a, ~a)"
  ;; 						   return-var arg1)
  ;; 					   (format nil "mpz_add_ui(~a, ~a, ~a)" return-var return-var arg2)))
  ;; 			      (mpq (break "deal with this later"))))
  ;; 		  (int32 (case c-return-type
  ;; 			   (uint32 
  ;; 			    (list (format nil "if (~a < 0){~a =  (~a - (~a_t)(-~a));} else {~a =  (~a + (~a_t)~a);}"
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type arg2)))
  ;; 			   (int32 (list (format nil "if (~a < 0){if ((uint32_t)(-~a) <= ~a){~a = (int32_t)(~a - (uint32_t)(-~a));} else {~a = (int32_t)~a + ~a;};} else {~a = (int32_t)(~a + (uint32_t)~a);}"
  ;; 					     arg2 arg2 arg1 return-var arg1 arg2
  ;; 					     return-var arg1 arg2
  ;; 					     return-var arg1 arg2)))
  ;; 			   (mpz (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "if (~a < 0){mpz_sub_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_add_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 		  (mpz (let ((tmp (gentemp "tmp")))
  ;; 			 (case c-return-type
  ;; 			   ((uint32 int32)
  ;; 			    (ir2c-addition-step  return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type))
  ;; 			   (mpz (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "mpz_add(~a, ~a, ~a)" return-var return-var arg2))))))))
  ;;     (int32 (case arg2-c-type
  ;; 	       (int32 (case c-return-type
  ;; 			(uint32 (list (format nil "~a = (~a_t) (~a + ~a)"
  ;; 					      return-var c-return-type arg1 arg2)))
  ;; 			(int32 (list (format nil "~a = ~a + ~a"
  ;; 					     return-var arg1 arg2)))
  ;; 			(mpz (list (format nil "mpz_set_si(~a, ~a)" return-var arg1)
  ;; 				   (format nil "if (~a < 0){mpz_sub_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_add_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 	       (t (ir2c-addition-step return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type ))))
  ;;     (mpz (case c-return-type
  ;; 	     ((uint32 int32)
  ;; 	      (case arg2-c-type
  ;; 		(uint32 (ir2c-addition-step return-var c-return-type arg2 arg2-c-type
  ;; 					      arg1 arg1-c-type))
  ;; 		(int32 (let ((tmp (gentemp "tmp")))
  ;; 			 (list (format nil "mpz_t ~a" tmp)
  ;; 			       (format nil "mpz_init(~a)" tmp)
  ;; 			       (format nil "if (~a < 0){mpz_sub_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_add_ui(~a, ~a, (uint32_t)(~a));}"
  ;; 				       arg2
  ;; 				       tmp arg1 arg2
  ;; 				       tmp arg1 arg2)
  ;; 			       (format nil "~a = mpz_get~a(~a)"
  ;; 				       return-var
  ;; 				       (gmp-suffix c-return-type)
  ;; 				       tmp)
  ;; 			       (format nil "mpz_clear(~a)" tmp))))
  ;; 		(mpz (let ((tmp (gentemp "tmp")))
  ;; 		       (list (format nil "mpz_t ~a" tmp)
  ;; 			     (format nil "mpz_init(~a)" tmp)
  ;; 			     (format nil "mpz_add(~a, ~a, ~a)"
  ;; 				     tmp arg1 arg2)
  ;; 			     (format nil "~a = mpz_get~a(~a, ~a)"
  ;; 				     return-var
  ;; 				     (gmp-suffix c-return-type)
  ;; 				     tmp)
  ;; 			     (format nil "mpz_clear(~a)" tmp))))))
  ;; 	     (mpz (list (format nil "mpz_set~a(~a, ~a)"
  ;; 				(gmp-suffix arg2-c-type)
  ;; 				return-var arg2)
  ;; 			(format nil "mpz_add(~a, ~a, ~a)"
  ;; 				return-var return-var arg1)))))))

(defun ir2c-subtraction (return-var c-return-type ir-args c-arg-types)
  (let ((arg1 (car ir-args))
	(arg1-c-type (car c-arg-types)))
    (if (null (cdr ir-args))
	(ir2c-minus-step return-var c-return-type arg1 arg1-c-type)
      (let ((arg2 (cadr ir-args))
	    (arg2-c-type (cadr c-arg-types)))
	(ir2c-subtraction-step return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)))))

(defun ir2c-minus-step (return-var c-return-type arg1 arg1-c-type)
  (case c-return-type
    ((mpz mpq)
     (case arg1-c-type
       ((mpz mpq)
	(list (format nil "~a_mk_set~a(~a, ~a)" c-return-type (if (eq c-return-type arg1-c-type) ""
							     (if (eq arg1-c-type 'mpz) "_z" "_q"))
		      return-var arg1)
	      (format nil "~a_neg(~a, ~a)" c-return-type return-var return-var)))
       (t (list (format nil "~a_mk_set_~a(~a, ~a)" c-return-type (gmp-ui-or-si arg1-c-type)
			return-var arg1)
		(format nil "~a_neg(~a, ~a)" c-return-type return-var return-var)))))
    (t (case arg1-c-type
	 ((mpz mpq)
	  (list (format nil "~a = (~a_t) ~a_get_si(~a)"  return-var c-return-type arg1-c-type arg1)))
	 (t 
	  (list (format nil "~a = (~a_t)-~a" return-var c-return-type arg1)))))))

(defun ir2c-subtraction-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
    (case  c-return-type
      (mpz (case arg1-c-type
	     ((uint8 uint16 uint32 uint64); __uint128)
	      (case arg2-c-type
		((uint8 uint16 uint32 uint64)
		 (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg1)
		       (format nil "mpz_sub_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
		((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		((int8 int16 int32 int64)
		 (list (format nil "mpz_mk_set_si(~a, -~a)" return-var arg2)
		       (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var  arg1)))
		(mpz (list (format nil "mpz_ui_sub(~a, (uint64_t)~a,  ~a)" return-var arg1 arg2)))
		(mpq (let ((tmp1 (gentemp "tmp"))
			   (tmp2 (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp1)
			     (format nil "mpq_t ~a" tmp2)
			     (format nil "mpq_init(~a)" tmp1)
			     (format nil "mpq_init(~a)" tmp2)
			     (format nil "mpq_set_ui(~a, ~a, 1)" tmp1 arg1)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
			     (format nil "mpq_clear(~a)" tmp1)
			     (format nil "mpq_clear(~a)" tmp2)
			     )))))
	     ((int8 int16 int32 int64)
	      (case arg2-c-type
		((uint8 uint16 uint32 uint64)
		 (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg2)
		       (format nil "mpz_si_sub(~a, (int64_t)~a, ~a)" return-var arg1 return-var)))
		((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		((int8 int16 int32 int64)
		 (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
		       (format nil "mpz_si_sub_si(~a, (int64_t)~a, ~a)" return-var arg1 return-var)))
		(mpz (list (format nil "mpz_mk_set_si(~a, (int64_t)~a)" return-var arg1)
			   (format nil "mpz_sub(~a, ~a, ~a)" return-var return-var arg2)))
		(mpq (let ((tmp1 (gentemp "tmp"))
			   (tmp2 (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp1)
			     (format nil "mpq_t ~a" tmp2)
			     (format nil "mpq_set_si(~a, ~a, 1)" tmp1 arg1)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
			     (format nil "mpq_clear(~a)" tmp1)
			     (format nil "mpq_clear(~a)" tmp2)
			     )))))
	      (mpz (case arg2-c-type
		     ((uint8 uint16 uint32 uint64)
		      (list (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var arg1 arg2)))
		     ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		     ((int8 int16 int32 int64)
		      (list (format nil "mpz_mk_sub_si(~a, ~a (int64_t)~a)" return-var arg1 arg2)))
		     (mpz (list (format nil "mpz_mk_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     (mpq (let ((tmp1 (gentemp "tmp"))
				(tmp2 (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp1)
			     (format nil "mpq_t ~a" tmp2)
			     (format nil "mpq_set_z(~a, ~a)" tmp1 arg1)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
			     (format nil "mpq_clear(~a)" tmp1)
			     (format nil "mpq_clear(~a)" tmp2)
			     )))))
	      (mpq (case arg2-c-type
		     ((uint8 uint16 uint32 uint64)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpz_t ~a" tmp)
			      (format nil "mpz_init(~a)" tmp)
			      (format nil "mpz_set_q(~a, ~a)" tmp arg1)
			      (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var tmp arg2)
			      (format nil "mpz_clear(~a)" tmp))))
		     ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		     ((int8 int16 int32 int64)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpz_t ~a" tmp)
			      (format nil "mpz_init(~a)" tmp)
			      (format nil "mpz_set_q(~a, ~a)" tmp arg1)
			      (format nil "mpz_mk_sub_si(~a, ~a (int64_t)~a)" return-var tmp arg2)
			      (format nil "mpz_clear(~a)" tmp))))
		     (mpz (let ((tmp (gentemp "tmp")))
			    (list (format nil "mpz_t ~a" tmp)
				  (format nil "mpz_init(~a)" tmp)
				  (format nil "mpz_set_q(~a, ~a)" tmp arg1)
				  (format nil "mpz_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
				  (format nil "mpz_clear(~a)" tmp))))
		     (mpq (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp arg1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp)
			     (format nil "mpq_clear(~a)" tmp)
			     )))))
	      (t (break "Not implemented"))))
      (mpq (case arg1-c-type
	     ((uint8 uint16 uint32 uint64); __uint128)
	      (case arg2-c-type
		((uint8 uint16 uint32 uint64)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg1)
			 (format nil "mpz_sub_ui(~a, ~a, (uint64_t)~a)" tmp tmp arg2)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		((int8 int16 int32 int64)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a, 1)" tmp arg1)
			 (format nil "mpz_sub_si(~a, ~a, (int64_t)~a)" tmp tmp arg2)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		(mpz (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_ui_sub(~a, (uint64_t)~a,  ~a)" tmp arg1 arg2)
			     (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			     (format nil "mpz_clear(~a)" tmp))))
		(mpq (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_set_ui(~a, ~a, 1)" tmp arg1)
			     (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
			     (format nil "mpq_clear(~a)" tmp)
			     )))
		(t (break "Not implemented"))))
	     ((int8 int16 int32 int64)
	      (case arg2-c-type
		((uint8 uint16 uint32 uint64)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg2)
			 (format nil "mpz_si_sub(~a, (int64_t)~a, ~a)" tmp arg1 tmp)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		((int8 int16 int32 int64)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_si(~a, ~a)" tmp arg2)
			 (format nil "mpz_si_sub_si(~a, (int64_t)~a, ~a)" tmp arg1 tmp)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		(mpz (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_si_sub(~a, (int64_t)~a,  ~a)" tmp arg1 arg2)
			     (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			     (format nil "mpz_clear(~a)" tmp))))
		(mpq (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_set_si(~a, ~a, 1)" tmp arg1)
			     (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
			     (format nil "mpq_clear(~a)" tmp))))
		))
	      (mpz (case arg2-c-type
		     ((uint8 uint16 uint32 uint64)
		      (list (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var arg1 arg2)))
		     ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		     ((int8 int16 int32 int64)
		      (list  (format nil "mpz_mk_sub_si(~a, ~a, (int64_t)~a)" return-var arg1 arg2)))
		     (mpz (list (format nil "mpz_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     (mpq (let ((tmp1 (gentemp "tmp"))
				(tmp2 (gentemp "tmp")))
			    (list (format nil "mpq_t ~a" tmp1)
				  (format nil "mp1_t ~a" tmp2)
				  (format nil "mpq_set_z(~a, ~a)" tmp1 arg1)
				  (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
				  (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
				  (format nil "mpq_clear(~a)" tmp1)
				  (format nil "mpq_clear(~a)" tmp2)
				  )))))
	      (mpq (case arg2-c-type
		     ((uint8 uint16 uint32 uint64)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpq_t ~a" tmp)
			      (format nil "mpq_init(~a)" tmp)
			      (format nil "mpq_set_ui(~a, ~a, 1)" tmp arg2)
			      (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
			      (format nil "mpq_clear(~a)" tmp))))
		     ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		     ((int8 int16 int32 int64)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpq_t ~a" tmp)
			      (format nil "mpq_init(~a)" tmp)
			      (format nil "mpq_set_si(~a, ~a, 1)" tmp arg2)
			      (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
			      (format nil "mpq_clear(~a)" tmp))))
		     (mpz (let ((tmp (gentemp "tmp")))
			    (list (format nil "mpq_t ~a" tmp)
				  (format nil "mpq_init(~a)" tmp)
				  (format nil "mpq_set_z(~a, ~a)" tmp arg2)
				  (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
				  (format nil "mpq_clear(~a)" tmp))))
		     (mpq (list (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     ))
	      (t (break "Not implemented"))))
      ((uint8 uint16 uint32 uint64); __uint128)
       (case arg1-c-type
	 ((uint8 uint16 uint32 uint64); __uint128)
	  (case arg2-c-type
	    ((uint8 uint16 uint32 uint64); __uint128)
	     (list (format nil "~a = (~a_t)(~a - ~a)" return-var c-return-type arg1 arg2)))
	    ((int8 int16 int32 int64); __int128)
	     (list (format nil "if (~a < 0){~a =  (~a_t)((uint64_t)~a + (~a_t)-~a);} else {~a =  (~a_t)((uint64_t)~a - (uint64_t)~a);}"
			   arg2
			   return-var c-return-type arg1 c-return-type arg2
			   return-var c-return-type arg1  arg2)))
	    (mpz (let ((tmp (gentemp "tmp")))
		   (list (format nil "~a_t ~a" arg1-c-type tmp)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" tmp arg1-c-type arg2)
			 (format nil "~a = (~a_t) ((uint64_t)~a - ~a)" return-var c-return-type
				 arg1 tmp))))
	    (mpq (let ((tmp1 (gentemp "tmp"))
		       (tmp2 (gentemp "tmp")))
		   (list (format nil "mpz_t ~a"  tmp1)
			 (format nil "mpz_init(~a)" tmp1)
			 (format nil "mpz_set_q(~a, ~a)" tmp1 arg2)
			 (format nil "~a_t ~a" arg1-c-type tmp2)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" tmp2 arg1-c-type tmp1)
			 (format nil "~a = (~a_t)((uint64_t)~a - ~a)" return-var c-return-type
				 arg1 tmp2)
			 (format nil "mpz_clear(~a)" tmp1)
			 )))))
	 ((int8 int16 int32 int64); __int128)
	  (case arg2-c-type ;can only reach here if arg1 is non-negative
	    ((uint8 uint16 uint32 uint64); __uint128)
	     (list (format nil "~a = (~a_t)((int64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	    ((int8 int16 int32 int64); __int128)
	     (list (format nil "~a = (~a_t)((int64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	    (mpz (let ((tmp (gentemp "tmp")))
		   (list (format nil "~a_t ~a" arg1-c-type tmp)
			 (format nil "~a = mpz_get_si(~a)" tmp arg2)
			 (format nil "~a = (~a_t)((int64_t) ~a - ~a)" return-var c-return-type arg1 tmp))))
	    (mpq (let ((tmp1 (gentemp "tmp"))
		       (tmp2 (gentemp "tmp")))
		   (list (format nil "mpz_t ~a"  tmp1)
			 (format nil "mpz_init(~a)" tmp1)
		   (list (format nil "mpz_t ~a"  tmp2)
			 (format nil "mpz_init(~a)" tmp2)
			 (format nil "mpz_set_si(~a, ~a)" tmp1 arg1)
			 (format nil "mpz_set_q(~a, ~a)" tmp2 arg2)
			 (format nil "mpz_sub(~a, ~a, ~a)" tmp1 tmp1 tmp2)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" return-var c-return-type tmp1)
			 (format nil "mpz_clear(~a)" tmp1)
			 (format nil "mpz_clear(~a)" tmp2)
			 ))))))
	 (mpz (break "Not implemented"))
	 (mpq (break "Not available"))))
    ((int8 int16 int32 int64); __int128)
     (case arg1-c-type
       ((uint8 uint16 uint32 uint64); __uint128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64); __uint128)
	   (list (format nil "~a = (~a_t)((uint64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	  ((int8 int16 int32 int64); __int128)
	   (list (format nil "~a = (~a_t)(((uint64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (mpz (break "Not implemented"))
	  (mpq (break "Not available"))))
       ((int8 int16 int32 int64 __int128)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64); __uint128)
	   (list (format nil "~a = (~a_t)((int64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	  ((int8 int16 int32 int64); __int128)
	   (list (format nil "~a = (~a_t)((int64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (mpz (break "Not implemented"))
	  (mpq (break "Not available"))))
       (mpz (break "Not implemented"))
       (mpq (break "Not available"))))))

    
  ;; (case arg1-c-type
  ;;   (uint32 (case arg2-c-type
  ;; 		  (uint32 (case c-return-type
  ;; 			      (uint32
  ;; 			       (list (format nil "~a = ~a - ~a" return-var arg1 arg2)))
  ;; 			      (int32 ;;checked that this (below) works in C
  ;; 			       (format nil "~a = (int32_t) (~a - ~a)" return-var arg1 arg2))
  ;; 			      (mpz (list (format nil "mpz_set_ui(~a, ~a)"
  ;; 						   return-var arg1)
  ;; 					   (format nil "mpz_sub_ui(~a, ~a, ~a)" return-var return-var arg2)))
  ;; 			      (mpq (break "deal with this later"))))
  ;; 		  (int32 (case c-return-type
  ;; 			   (uint32 
  ;; 			    (list (format nil "if (~a < 0){~a =  (~a + (~a_t)(-~a));} else {~a =  (~a - (~a_t)~a);}"
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type arg2)))
  ;; 			   (int32 (list (format nil "if (~a < 0){if ((uint32_t)(-~a) <= ~a){~a = (int32_t)(~a + (uint32_t)(-~a));} else {~a = (int32_t)~a - ~a;};} else {~a = (int32_t)(~a - (uint32_t)~a);}"
  ;; 					     arg2 arg2 arg1 return-var arg1 arg2
  ;; 					     return-var arg1 arg2
  ;; 					     return-var arg1 arg2)))
  ;; 			   (mpz (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "if (~a < 0){mpz_add_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_sub_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 		  (mpz (let ((tmp (gentemp "tmp")))
  ;; 			 (case c-return-type
  ;; 			   ((uint32 int32)
  ;; 			    (ir2c-subtraction-step  return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type))
  ;; 			   (mpz (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "mpz_sub(~a, ~a, ~a)" return-var return-var arg2))))))))
  ;;     (int32 (case arg2-c-type
  ;; 	       (int32 (case c-return-type
  ;; 			(uint32 (list (format nil "~a = (~a_t) (~a - ~a)"
  ;; 					      return-var c-return-type arg1 arg2)))
  ;; 			(int32 (list (format nil "~a = ~a - ~a"
  ;; 					     return-var arg1 arg2)))
  ;; 			(mpz (list (format nil "mpz_set_si(~a, ~a)" return-var arg1)
  ;; 				   (format nil "if (~a < 0){mpz_add_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_sub_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 	       (t (ir2c-subtraction-step return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type ))))
  ;;     (mpz (case c-return-type
  ;; 	     ((uint32 int32)
  ;; 	      (case arg2-c-type
  ;; 		(uint32 (ir2c-subtraction-step return-var c-return-type arg2 arg2-c-type
  ;; 					      arg1 arg1-c-type))
  ;; 		(int32 (let ((tmp (gentemp "tmp")))
  ;; 			 (list (format nil "mpz_t ~a" tmp)
  ;; 			       (format nil "mpz_init(~a)" tmp)
  ;; 			       (format nil "if (~a < 0){mpz_add_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_sub_ui(~a, ~a, (uint32_t)(~a));}"
  ;; 				       arg2
  ;; 				       tmp arg1 arg2
  ;; 				       tmp arg1 arg2)
  ;; 			       (format nil "~a = mpz_get~a(~a)"
  ;; 				       return-var
  ;; 				       (gmp-suffix c-return-type)
  ;; 				       tmp)
  ;; 			       (format nil "mpz_clear(~a)" tmp))))
  ;; 		(mpz (let ((tmp (gentemp "tmp")))
  ;; 		       (list (format nil "mpz_t ~a" tmp)
  ;; 			     (format nil "mpz_init(~a)" tmp)
  ;; 			     (format nil "mpz_sub(~a, ~a, ~a)"
  ;; 				     tmp arg1 arg2)
  ;; 			     (format nil "mpz_get~a(~a, ~a)"
  ;; 				     (gmp-suffix c-return-type)
  ;; 				     return-var tmp)
  ;; 			     (format nil "mpz_clear(~a)" tmp))))))
  ;; 	     (mpz (list (format nil "mpz_set~a(~a, ~a)"
  ;; 				(gmp-suffix arg2-c-type)
  ;; 				return-var arg2)
  ;; 			(format nil "mpz_sub(~a, ~a, ~a)"
;; 				return-var return-var arg1))))))

(defun ir2c-multiplication (return-var c-return-type ir-args c-arg-types)
  (let ((arg1 (car ir-args))
	(arg2 (cadr ir-args))
	(arg1-c-type (car c-arg-types))
	(arg2-c-type (cadr c-arg-types)))
    (ir2c-multiplication-step return-var c-return-type arg1
			arg1-c-type arg2
			arg2-c-type)))

(defun mpq-ui-or-si (ctype)
  (or (and (eq ctype 'mpq) 'q)
      (gmp-ui-or-si ctype)))

(defun mpq-set (ctype qvar var)
  (if (eq ctype 'mpz)
      (format nil "mpq_set_z(~a, ~a)" qvar var)
    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
	    (gmp-ui-or-si ctype) qvar
	    (uint-or-int ctype)
	     var)))

(defun ir2c-multiplication-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (mpz (case arg1-c-type
	   ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	    (case arg2-c-type
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_~a(~a, (~a64_t)~a)" (gmp-ui-or-si arg1-c-type)
			       tmp (uint-or-int arg1-c-type) arg1)
		       (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)" (gmp-ui-or-si arg2-c-type)
			       return-var tmp (uint-or-int arg2-c-type) arg2)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	      ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
	      (mpz (list (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)"
				 (gmp-ui-or-si arg1-c-type)
				 return-var arg2 (uint-or-int arg1-c-type) arg1)))
	      (mpq (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (format nil "mpz_set_q(~a, ~a)" 
				   tmp arg2)
			   (format nil "mpz_mk_mul_~a(~a, ~a, ~a)"
				   (gmp-ui-or-si arg1-c-type)
				   return-var tmp arg1)
			   (format nil "mpq_clear(~a)" tmp)
			   )))))
	   (mpz (case arg2-c-type
		  ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
		   (list (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)" (gmp-ui-or-si arg2-c-type)
				 return-var arg1 (uint-or-int arg2-c-type) arg2)))
		  ((__uint128 __int128) (break "128-bit GMP arithmetic not available." ))
		  (mpz (list (format nil "mpz_mk_mul(~a, ~a, ~a)" return-var arg2 arg1)))
		  (mpq (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpz_t ~a" tmp)
			       (format nil "mpz_init(~a)" tmp)
			       (format nil "mpz_set_q(~a, ~a)" 
				       tmp arg2)
			       (format nil "mpz_mk_mul(~a, ~a, ~a)" return-var arg1 tmp)
			       (format nil "mpz_clear(~a)" tmp)
			       
			       )))))
	   (t (break "Not implemented"))))
    ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
     (case arg1-c-type
       ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	(case arg2-c-type
	  ((uint8 uint16 uint32 uint64 int8 int16 int32 int64); __uint128 _int128)
	   (list (format nil "~a = (~a_t)((~a64_t)~a * (~a64_t)~a)" return-var c-return-type (uint-or-int arg1-c-type) arg1 (uint-or-int arg2-c-type) arg2)))
	  (mpz (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg1-c-type)
				   tmp  arg2 arg1)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   
			   )))
	  (mpq (let ((tmp1 (gentemp "tmp"))
		     (tmp2 (gentemp "tmp"))
		     (tmp3 (gentemp "tmp")))
		 (list (format nil "mpq_t ~a" tmp1)
		       (format nil "mpq_init(~a)" tmp1)
		       (format nil "mpq_t ~a" tmp2)
		       (format nil "mpq_init(~a)" tmp2)
		       (format nil "mpq_set_~a(~a, ~a, 1)" (gmp-ui-or-si arg1-c-type)
			       tmp1 arg1)
		       (format nil "mpq_mul(~a, ~a, ~a)" 
			       tmp2 tmp1 arg2)
		       (format nil "mpz_t ~a" tmp3)
		       (format nil "mpz_init(~a)" tmp3)
		       (format nil "mpz_set_q(~a, ~a)" tmp3 tmp2)
		       (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
			       c-return-type (gmp-ui-or-si c-return-type) tmp3)
		       (format nil "mpq_clear(~a)" tmp1)
		       (format nil "mpq_clear(~a)" tmp2)
		       (format nil "mpz_clear(~a)" tmp3)
		       )))))
       ((__uint128 __int128)(break "128-bit GMP arithmetic not available"))
	;; (case arg2-c-type
	;;   ((uint8 uint16 uint32 uint64 __uint128 int8 int16 int32 int64 _int128)
	;;    (list (format nil "~a = (~a_t)(~a * ~a)" return-var c-return-type arg1 arg2)))
	;;   ((mpz mpq) (break "128-bit GMP arithmetic not available")))
       (mpz (case arg2-c-type
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg2-c-type)
			       tmp  arg1 arg2)
		       (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
			       c-return-type (gmp-ui-or-si c-return-type) tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	      (mpz (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_mul(~a, ~a, ~a)" 
				   tmp  arg2 arg1)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   
			   )))
	      (mpq (let ((tmp1 (gentemp "tmp"))
			 (tmp2 (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp1)
			   (format nil "mpz_init(~a)" tmp1)
			   (format nil "mpz_t ~a" tmp2)
			   (format nil "mpz_init(~a)" tmp2)
			   (format nil "mpz_set_q(~a, ~a)" 
				   tmp1 arg2)
			   (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg1-c-type)
				   tmp2 tmp1 arg1)
			   (format nil "~a = (~a_t)mpq_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp2)
			   (format nil "mpq_clear(~a)" tmp1)
			   (format nil "mpq_clear(~a)" tmp2)
			   )))))
       (mpq (case arg2-c-type
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (let ((tmp1 (gentemp "tmp"))
		     (tmp2 (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp1)
		       (format nil "mpz_init(~a)" tmp1)
		       (format nil "mpz_t ~a" tmp2)
		       (format nil "mpz_init(~a)" tmp2)
		       (format nil "mpz_set_q(~a, ~a)" 
			       tmp1 arg1)
		       (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg2-c-type)
			       tmp2 tmp1 arg2)
		       (format nil "~a = (~a_t)mpq_get_~a(~a)" return-var
			       c-return-type (gmp-ui-or-si c-return-type) tmp2)
		       (format nil "mpq_clear(~a)" tmp1)
		       (format nil "mpq_clear(~a)" tmp2)
		       )))
	      (mpz (let ((tmp1 (gentemp "tmp"))
			 (tmp2 (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp1)
		       (format nil "mpz_init(~a)" tmp1)
		       (format nil "mpz_t ~a" tmp2)
		       (format nil "mpz_init(~a)" tmp2)
		       (format nil "mpz_set_q(~a, ~a)" 
			       tmp1 arg1)
		       (format nil "mpz_mul(~a, ~a, ~a)"
			       tmp2 tmp1 arg2)
		       (format nil "~a = (~a_t)mpq_get_~a(~a)" return-var
			       c-return-type (gmp-ui-or-si c-return-type) tmp2)
		       (format nil "mpq_clear(~a)" tmp1)
		       (format nil "mpq_clear(~a)" tmp2)
		       )))
	      (mpq (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (format nil "mpq_mul(~a, ~a, ~a)" 
				   tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpq_clear(~a)" tmp)
			   )))
	      (t (break "Not implemented."))))
       (t (break "Not implemented."))))
    ((__uint128 __int128)(break "128-bit GMP arithmetic not available"))
     ;; (case arg1-c-type
     ;;   ((uint8 uint16 uint32 uint64 __uint128 int8 int16 int32 int64 __int128)
     ;; 	(case arg2-c-type
     ;; 	  ((uint8 uint16 uint32 uint64 __uint128 int8 int16 int32 int64 _int128)
     ;; 	   (list (format nil "~a = (~a_t)(~a * ~a)" return-var c-return-type arg1 arg2)))
     ;; 	  ((mpz mpq) (break "128-bit GMP arithmetic not available"))))
     ;;   ((mpz mpq) (break "128-bit GMP arithmetic not available"))
     ;;   (t (break "Not implemented.")))
    (mpq (case arg1-c-type
	   (mpq (case arg2-c-type
		  (mpq (list (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var arg1 arg2)))
		  (mpz (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_set_z(~a, ~a)" tmp arg2)
			       (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var arg1 tmp)
			       (format nil "mpq_clear(~a)" tmp))))
		  ((uint8 uint16 uint32 uint64 int8 int16 int32 int64 mpq)
		   (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (mpq-set arg2-c-type tmp arg2)
			   (format nil "mpq_mul(~a, ~a, ~a)" return-var arg1 tmp)
			   (format nil "mpq_clear(~a)" tmp)
			   )))
		  ((uint128 int128)
		   (break "128-bit GMP arithmetic not available." ))))
	   (mpz (case arg2-c-type
		  (mpq (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_set_z(~a, ~a)" tmp arg1)
			       (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var tmp arg2)
			       (format nil "mpq_clear(~a)" tmp))))
		  (mpz (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_mul(~a, ~a, ~a)" tmp arg1 arg2)
			       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp))))
		  ((uint8 uint16 uint32 uint64 int8 int16 int32 int64 mpq)
		   (let ((tmp1 (gentemp "tmp"))
			 (tmp2 (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp1)
			   (format nil "mpq_init(~a)" tmp1)
			   (mpq-set arg2-c-type tmp1 arg2)
			   (format nil "mpz_mul(~a, ~a, ~a)" tmp2 arg1 tmp1)
			   (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp2)
			   (format nil "mpq_clear(~a)" tmp1)
			   (format nil "mpq_clear(~a)" tmp2)
			   )))
		  ((uint128 int128)
		   (break "128-bit GMP arithmetic not available." ))))
	   ((uint8 uint16 uint32 uint64 int8 int16 int32 int64 mpq)
	    (case arg2-c-type
	      (mpq (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (mpq-set arg1-c-type tmp arg1)
			   (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var tmp arg2)
			   (format nil "mpq_clear(~a)" tmp)
			   )))
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64 mpq)
	       (let ((tmp1 (gentemp "tmp"))
		     (tmp2 (gentemp "tmp")))
		 (list (format nil "mpq_t ~a, ~a" tmp1 tmp2)
		       (format nil "mpq_init(~a)" tmp1)
		       (format nil "mpq_init(~a)" tmp2)
		       (mpq-set arg1-c-type tmp1 arg1)
		       (mpq-set arg2-c-type tmp2 arg2)
		       (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var tmp1 tmp2)
		       (format nil "mpq_clear(~a)" tmp1)
		       (format nil "mpq_clear(~a)" tmp2)
		       )))
		  (t (break "Not implemented"))))
	   (t (break "Not implemented"))))))

							   
			   


(defun mpnumber-type? (c-type)
  (or (eq c-type 'mpz)(eq c-type 'mpq)))

(defmethod ir2c* ((ir-expr ir-lookup) return-var return-type)
  (with-slots (ir-array ir-index) ir-expr
	      (let* ((ir-array-var (get-ir-last-var ir-array))
		     (ir-index-var (get-ir-last-var ir-index))
		     (c-return-type (add-c-type-definition (ir2c-type return-type)))
		     (elem-type (ir-range (get-ir-type-value (ir-vtype ir-array-var))))
		     (c-elem-type  (add-c-type-definition (ir2c-type return-type)))
		     (rhs (format nil "~a->elems[~a]" (ir-name ir-array-var)(ir-name ir-index-var)))
		     (assign-instrs (case c-return-type
				      ((mpq mpz)
				       (list (format nil "~a = safe_malloc(sizeof(~a_t))"
						     return-var c-return-type)
					     (format nil "~a_init(~a)" c-return-type return-var)
					     (mk-c-assignment return-var c-return-type
								rhs c-elem-type)))
				      (t (list (format nil "~a = (~a_t)~a"  return-var
						       c-return-type rhs)))))
		     (refcount-lookup-instr	;if lookup is a reference, first bump its count
		      (when (ir-reference-type? elem-type)
			(list (format nil "~a->count++"  return-var))))
		     (release-array-instr
		      (when (ir-last? ir-array) ;if last, then release the array
			(list (release-last-var ir-array-var)))))
		(append assign-instrs (append refcount-lookup-instr release-array-instr)))))

(defun release-last-var (ir-array-var)
  (let ((ir2c-type (ir2c-type (ir-vtype ir-array-var))))
    (format nil "release_~a(~a~{, ~a~})"
	    (add-c-type-definition ir2c-type)
	    (ir-name ir-array-var)
	    (loop for ir-formal in (free-ir-formals ir2c-type)
		  collect (ir-type-id ir-formal)))))

(defmethod ir2c* ((ir-expr ir-apply) return-var return-type)
;  (break "ir2c*(ir-apply)")
  (with-slots (ir-func ir-params ir-args) ir-expr
	      (let* ((ir-func-var (get-ir-last-var ir-func))
		     (hi-array (when (ir-variable? ir-func-var)
				 (ir-array? (ir-vtype ir-func-var))))
		     (ir-index-var (when hi-array (get-ir-last-var (car ir-args)))))
	      (if hi-array;;if operator is an array
		  (let* ((assign-instr (format nil "~a = (~a_t)~a->elems[~a]"
					       return-var
					       (add-c-type-definition (ir2c-type return-type))
					       (ir-name ir-func-var)
					       (ir-name ir-index-var)))
			 (refcount-lookup-instr	;if lookup is a reference, first bump its count
			  (when (ir-reference-type? (ir-range (get-ir-type-value (ir-vtype ir-func-var))))
			    (list (format nil "~a->count++"  return-var))))
			 (release-array-instr
			  (when (ir-last? ir-func) ;if last, then release the array
			    (list (release-last-var ir-func-var)))))
		    ;(break "ir-apply")
		    (cons assign-instr (append refcount-lookup-instr release-array-instr)))
		;;otherwise, it's a function call
		(let* ((ir-params-args (append ir-params ir-args))
		       (invoke-instrs (ir2c-function-apply return-var return-type ir-func ir-params-args
							   ))
		     ;; (rhs-string (format nil "~a(~{~a~^, ~})" ir-func-var ir-arg-vars))
		     ;; (invoke-instr (format nil "~a = ~a" return-var rhs-string))
		     (release-instrs (loop for ir-var in ir-params-args ;;bump the counts of non-last references by one
					   when (and (not (ir-last? ir-var))(ir-reference-type? (ir-vtype (get-ir-last-var ir-var))))
					   collect (format nil "~a->count++" (ir-name ir-var))))
		     (mp-release-instrs (loop for ir-var in ir-params-args
					      when (and (ir-last? ir-var)
							(not (member (get-ir-last-var ir-var) *mpvar-parameters*))
							(mpnumber-type? (ir2c-type (ir-vtype (get-ir-last-var ir-var)))))
					      collect (format nil "~a_clear(~a)"
							      (ir2c-type (ir-vtype (get-ir-last-var ir-var)))
							      (ir-name (get-ir-last-var ir-var))))))
		  ;(format t "~%*mpvar-parameters* = ~{~a, ~}" (print-ir *mpvar-parameters*))
		;(break "ir-apply")
		  (nconc release-instrs (nconc invoke-instrs mp-release-instrs
					       )))))))

(defun mppointer-type (type)
  (let ((mpnumber-type (mpnumber-type? type)))
    (or (and mpnumber-type (intern (format nil "~a_ptr" type)))
	type)))

(defmethod ir2c* ((ir-expr ir-let) return-var return-type)
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
    (with-slots (ir-name ir-vtype) ir-vartype
;;      (when (ir-lvariable? ir-body)(break "variable body"))
      (cond ((and (ir-lvariable? ir-body)
		  (equal return-type (ir2c-type ir-vtype)))
	     (ir2c* ir-bind-expr return-var return-type))
	    (t (let* ((ir2c-vtype (ir2c-type ir-vtype))
		      (var-ctype (add-c-type-definition ir2c-vtype))
		      (decl-var-ctype (mppointer-type var-ctype))
		      (decl-instr (format nil "~a_t ~a" decl-var-ctype ir-name))
		      (init-instrs nil)
		       ;; (when (mpnumber-type? var-ctype)
		       ;;   (list (format nil "~a = (~a_t)safe_malloc(sizeof(~a_t))"
		       ;; 		       ir-name decl-var-ctype var-ctype)
		       ;; 	       (format nil "~a_init(~a)" var-ctype ir-name)
		       ;; 	       ))
		      ;;This is done when (last var) is encountered
		      ;; (clear-instr (when (mpnumber-type? var-ctype)
		      ;; 	       (format nil "~a_clear(~a)" var-ctype ir-name)))
		      (bind-instrs (ir2c* ir-bind-expr ir-name 
					  ir2c-vtype)) ;(ir2c-type (ir-vtype ir-vartype))
		      (body-instrs (ir2c* ir-body return-var return-type)) ;(break "ir-let")
		      ;; (body-with-mp-clear-instrs
		      ;;  (if (mpnumber-type? var-ctype)
		      ;;      (append body-instrs
		      ;; 	      (list (format nil "~a_clear(~a)" var-ctype ir-name)
		      ;; 		    (format nil "safe_free(~a)" ir-name)))
		      ;;    body-instrs))
		      )
		 (if init-instrs
		     (cons decl-instr (append init-instrs
			     		      (append bind-instrs body-instrs)))
		   (cons decl-instr (append bind-instrs body-instrs)))))))))

(defmethod ir2c* ((ir-expr ir-lett) return-var return-type);;assume ir-bind-expr is an ir-variable
  (with-slots (ir-vartype ir-bind-type ir-bind-expr ir-body) ir-expr
	      (with-slots (ir-name ir-vtype) ir-vartype
			  (let* ((ir2c-vtype (ir2c-type ir-vtype))
				 (ir2c-btype (ir2c-type ir-bind-type))
				 (var-ctype (add-c-type-definition ir2c-vtype))
				 (decl-var-ctype (mppointer-type var-ctype))
				 (var-btype (add-c-type-definition ir2c-btype))
				 (decl-instr (format nil "~a_t ~a" decl-var-ctype ir-name));;need mpz_init/release for mpz_t
				 (init-instrs nil
					      ;; (when (and (mpnumber-type? var-ctype);init mpvar when bound to a var
					      ;; 		 (ir-lvariable? ir-bind-expr))
				 	      ;; 	(list (format nil "~a = (~a_t)safe_malloc(sizeof(~a_t))"
				 	      ;; 		      ir-name decl-var-ctype var-ctype)
				 	      ;; 	      (format nil "~a_init(~a)" var-ctype ir-name)))
					      )
				 (bind-instrs (copy-type ir2c-vtype ir2c-btype ir-name
							 (ir-name (get-ir-last-var ir-bind-expr))))
				 (all-bind-instrs (cons decl-instr (append init-instrs bind-instrs))
						  ;; (if init-instrs
						  ;;     (cons decl-instr (append init-instrs bind-instrs))
						  ;;   (cons decl-instr bind-instrs))
						  )
				 (body-instrs (ir2c* ir-body return-var return-type)));(break "ir-let")
			    (if (and (ir-last? ir-bind-expr)
				     (not (member (get-ir-last-var ir-bind-expr) *mpvar-parameters*))
				     (mpnumber-type? var-btype))
				(let ((clear-instrs
				       (list
					(format nil "~a_clear(~a)" var-btype
						(ir-name (ir-var ir-bind-expr)))
					)))
				  (append all-bind-instrs (append clear-instrs body-instrs)))
			      (append all-bind-instrs body-instrs))))))
				

(defmethod ir2c* ((ir-expr ir-nil) return-var return-type)
  (list (format nil "~a = NULL" return-var)))

(defmethod ir2c-type ((ir-typ ir-subrange))
    (with-slots (ir-low ir-high) ir-typ 
		(if (and (integerp ir-low) (>= ir-low 0))
		    (if (integerp ir-high)
			(cond ((<= ir-high *max8-C-uli*) 'uint8)
			      ((<= ir-high *max16-C-uli*) 'uint16)
			      ((<= ir-high *max32-C-uli*) 'uint32)
			      ((<= ir-high *max64-C-uli*) 'uint64)
			      ((<= ir-high *max128-C-uli*) '__uint128)
			      (t 'mpz))
		      'mpz)
		  (if (and (integerp ir-low)(integerp ir-high))
		      (cond ((and (>= ir-low *min8-C-int*)(<= ir-high *max8-C-int*)) 'int8)
			    ((and (>= ir-low *min16-C-int*)(<= ir-high *max16-C-int*)) 'int16)
			    ((and (>= ir-low *min32-C-int*)(<= ir-high *max32-C-int*)) 'int32)
			    ((and (>= ir-low *min64-C-int*)(<= ir-high *max64-C-int*)) 'int64)
			    ((and (>= ir-low *min128-C-int*)(<= ir-high *max128-C-int*))
			     '__int128)
			    (t 'mpz))
		    'mpz))))

(defun ir-integer-type? (ir-typ)
  (and (ir-subrange? ir-typ)
       (with-slots (ir-low ir-high) ir-typ
		   (or (and (>= ir-low *min-C-uli*)
			    (<= ir-high *max-C-uli*))
		       (and (>= ir-low *min-C-int*)
			    (<= ir-high *max-C-int*))))))

(defun mk-ir-arraytype (size elemtype)
  (make-instance 'ir-arraytype
		 :size size
		 :elemtype elemtype))

;;Not used anywhere
;; (defcl ir-hashtype (ir-type)
;;   hdomain)

(defmethod print-ir ((ir-type ir-arraytype))
  (with-slots (size elemtype) ir-type
	      (format nil "~a[~d]" (print-ir elemtype) size)))

(defmethod ir2c-type ((ir-typ ir-funtype))
  (with-slots (ir-domain ir-range) ir-typ
	      (let ((size (ir-index? ir-domain)))
		(if size ;;check that size is below max-PVS-array-size
		    (mk-ir-arraytype size (ir2c-type ir-range))
		  (mk-ir-funtype  (ir2c-type ir-domain)
				  (ir2c-type ir-range))))))

(defmethod ir2c-type ((ir-typ ir-recordtype))
  (with-slots (ir-field-types) ir-typ
	      (mk-ir-recordtype
	       (ir2c-type-fields ir-field-types))))

(defmethod ir2c-type ((ir-typ ir-tupletype))
  (with-slots (ir-field-types) ir-typ
	      (mk-ir-tupletype
	       (ir2c-type-fields ir-field-types))))

(defmethod ir2c-type ((ir-typ ir-adt-recordtype))
  (with-slots (ir-field-types ir-constructors) ir-typ
	      (mk-ir-adt-recordtype
	       (ir2c-type-fields ir-field-types)
	       ir-constructors)))

(defmethod ir2c-type ((ir-typ ir-adt-constructor-recordtype))
  (with-slots (ir-field-types ir-adt-name) ir-typ
	      (mk-ir-adt-constructor-recordtype
	       (ir2c-type-fields ir-field-types)
	       ir-adt-name)))

(defun ir2c-type-fields (ir-field-types)
  (cond ((consp ir-field-types)
	 (with-slots (ir-id ir-ftype) (car ir-field-types)
		     (cons (mk-ir-fieldtype ir-id
					    (ir2c-type ir-ftype))
			   (ir2c-type-fields (cdr ir-field-types)))))
	(t nil)))

(defmethod ir2c-type ((ir-type symbol))
  (case ir-type
    (boolean 'bool)
    (t ir-type)))

(defmethod ir2c-type ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      (if (ir-subrange? ir-type-defn)
		  (ir2c-type ir-type-defn)
		(mk-ir-typename ir-type-id (ir2c-type ir-type-defn)))))

(defmethod ir2c-type ((ir-type ir-formal-typename))
  ir-type)

  ;; (let ((bind (rassoc ir-type *ir2c-type-binding*)))
  ;;   (or (and bind (cdr bind))
  ;; 	ir-type)))

  ;; (with-slots (ir-type-id ir-type-defn) ir-type
  ;; 	      (let ((tdefn (ir2c-type ir-type-defn)))
  ;; 		(if (symbolp tdefn) ;;i.e., it's a primitive
  ;; 		    tdefn
  ;; 		  (mk-ir-typename ir-type-id tdefn)))))

(defmethod ir2c-type ((ir-type t))
  ir-type)


(define-condition pvs2c-error (simple-error) (error-string))

	      ;; (let ((array? (ir-array? ir-domain))
	      ;; 	    ) ;; ignoring non-arrays for now
	      ;; 	(if (ir-integer-type? ir-range)
	      ;; 	    (format nil "ref_int_t")
	      ;; 	  (format nil "ref_ref_t")))))


(defvar *mpvar-parameters* nil)

;;A lambda-expression turns into the initialization for an array.  
(defmethod ir2c* ((ir-expr ir-lambda) return-var return-type)
  (with-slots (ir-vartypes ir-rangetype ir-body) ir-expr
	    (let ((array? (and (eql (length ir-vartypes) 1)
				(ir-index? (ir-vtype (car ir-vartypes))))))     
	      (if array?
		  (let* ((ir-arraytype  (get-arraytype return-type)) ;(mk-ir-arraytype array? (ir2c-type ir-rangetype)))
			 (elemtype (with-slots (elemtype) ir-arraytype elemtype))
			 (c-arraytype (add-c-type-definition ir-arraytype))
			 (index (ir-name (car ir-vartypes)))
			 (return-location (format nil "~a->elems[~a]" return-var index))
			 (return-location-type (add-c-type-definition (ir2c-type elemtype)))
			(c-body (ir2c* ir-body return-location return-location-type)))
		    (with-slots (size elemtype) ir-arraytype
				(list (format nil "~a = new_~a()" return-var c-arraytype)
				      ;(format nil "~a->count = 1");;this should be done within new.
				      (mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
					      index index size index)
						    c-body))))
		(let* ((fvars (pvs2ir-freevars* ir-expr))
		       (lastvars (ir-lastvars ir-expr))
		       (closure-name (add-closure-definition ir-expr))
		       (closure-var (gentemp "cl"))
		       (new-instrs (list (format nil "~a_t ~a" closure-name closure-var)
					 (format nil "~a = new_~a()" closure-var closure-name)))
		       (fvar-instrs (loop for fvar in fvars
					  as i from 1
					  collect (let ((fv-type (add-c-type-definition (ir2c-type (ir-vtype fvar)))))
						    (make-c-assignment (format nil "~a->fvar_~a"  closure-var i)
								       fv-type
								       (ir-name fvar)
								       fv-type))))
		       (return-instrs (list (format nil "~a = (~a_t)~a"
						    return-var
						    (add-c-type-definition return-type) closure-var)))
		       ;;refcount isn't needed
		       ;; (refcount-instrs (loop for fvar in fvars
		       ;; 			      when (and (ir-reference-type? (ir-vtype fvar))
		       ;; 					(not (memq fvar lastvars)))
		       ;; 			      collect (format nil "~a->count++" (ir-name fvar))))
		       )
		  (nconc new-instrs (nconc fvar-instrs return-instrs)))))))
							  
		;; (break "closure alert"))))); (error 'pvs2c-error :format-control "closures not yet implemented")))))

(defmethod get-arraytype ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      (get-arraytype ir-type-defn)))

(defmethod get-arraytype ((ir-type ir-arraytype))
  ir-type)

(defmethod ir2c* ((ir-expr ir-ift) return-var return-type)
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (let ((c-then-instrs (ir2c* ir-then return-var return-type))
		    (c-else-instrs (ir2c* ir-else return-var return-type)))
		(list (mk-if-instr 
		       (ir-name (get-ir-last-var ir-condition))
		       c-then-instrs
		       c-else-instrs)))))

(defmethod ir2c* ((ir-expr ir-release) return-var return-type)
  (with-slots (pre-ir-vars post-ir-vars ir-body) ir-expr
	      (let ((c-body (ir2c* ir-body return-var return-type))
		    (pre-release-instrs (loop for vartype in pre-ir-vars
					  collect (release-last-var vartype)))
		    (post-release-instrs (loop for vartype in post-ir-vars
					  collect (release-last-var vartype))))
		(append pre-release-instrs c-body post-release-instrs))))

(defmethod ir2c* ((ir-expr ir-new) return-var return-type)
  (with-slots (ir-etype) ir-expr
	      (ir2c-new ir-etype return-var)))

(defmethod ir2c-new ((ir-type ir-arraytype) return-var)
  (with-slots (size elemtype) ir-type
	      (let ((ctype (add-c-type-definition (ir2c-type ir-type))))
		(format nil "~a = new_~a()" return-var ctype))))

(defmethod ir2c-new ((ir-type ir-recordtype) return-var)
  (let ((ctype (add-c-type-definition (ir2c-type ir-type))))
    (format nil "~a = new_~a()" return-var ctype)))

(defmethod ir2c* ((ir-expr ir-update) return-var return-type)
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
	      (let* ((target-var (get-ir-last-var ir-target))
		     (target-var-name (ir-name target-var))
		     (rhs-var (get-ir-last-var ir-rhs))
		     (rhs-var-name (ir-name rhs-var))
		     (ir-ctype (ir2c-type (ir-vtype target-var)))
		     (ctype (add-c-type-definition ir-ctype))
		     (creturn-type (add-c-type-definition (ir2c-type return-type)))
		     (target-last (ir-last? ir-target))
		     (rhs-last (and (not (ir-constructor-update? ir-expr))
				    (ir-last? ir-rhs)
				    (ir-reference-type? (ir-vtype rhs-var))))
		     (rhs-last-instr (if rhs-last
					(list (format nil "if (~a != NULL) ~a->count--"
						      rhs-var-name rhs-var-name))
				       nil))) ;(break "ir2c*(ir-update)")
		;(when (ir-constructor-update? ir-expr)(break "ir2c*(ir-constructor-update)"))
		(if (ir-lvariable? ir-lhs)  ;;this doesn't work with names
		                            ;;(ir-arraytype? (get-ir-type-value ir-ctype))
		    (let* ((lhs-var (get-ir-last-var ir-lhs))
			   (lhs-var-name (ir-name lhs-var)))
		      (if target-last
			  (cons (format nil "~a = (~a_t)update_~a(~a, ~a, ~a)"
					return-var creturn-type ctype target-var-name lhs-var-name rhs-var-name)
				rhs-last-instr)
			(cons (format nil "{~a = (~a_t)copy_~a(~a); update_~a(~a, ~a, ~a)}"
				      return-var creturn-type ctype target-var-name ctype
				       return-var  lhs-var-name rhs-var-name)
			      rhs-last-instr)))
		  ;;else we're assuming it's a record
		  (if target-last
		      (cons (format nil "~a = (~a_t)update_~a_~a(~a, ~a)"
					return-var creturn-type ctype ir-lhs target-var-name rhs-var-name)
			    rhs-last-instr)
		    (cons (format nil "{~a = (~a_t)copy_~a(~a); ~a = (~a_t)update_~a_~a(~a, ~a);}"
				  return-var creturn-type ctype target-var-name
				  return-var creturn-type ctype ir-lhs target-var-name rhs-var-name)
			  rhs-last-instr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;process a type into its C definition relative to *c-type-info-table*

(defcl c-defn-info ()
  op-name
  op-arg-types
  op-return-type
  op-header
  op-defn)

(defun mk-c-defn-info (op-name op-header op-defn &optional op-arg-types op-return-type)
  (make-instance 'c-defn-info
		 :op-name op-name
		 :op-arg-types op-arg-types
		 :op-return-type op-return-type
		 :op-header op-header
		 :op-defn op-defn))


(defcl simple-c-type-info ()
  ir-texpr tname tdefn act-defn);act-defn is the type for the actual for this type.

(defcl closure-c-type-info  (simple-c-type-info)
  tdecl
  ftable-type-defn
  release-info
  hash-entry-type-defn
  hash-type-defn
  copy-info
  lookup-info
  dupdate-info
  update-info)


(defcl c-type-info (simple-c-type-info)
  new-info
  release-info
  copy-info
  equal-info
  update-info)


(defcl c-closure-info ()
  ir-lambda-expr
  tname
  tdecl
  tdefn
  fdefn
  mdefn
  hdefn
  new-info
  release-info
  copy-info)

(defvar *c-type-info-table* nil) ;;a list of c-type-info
(defvar *ir-type-info-table* nil) ;;a list of ir-typenames/doesn't seem to be used
(defvar *copy-operations* nil);;list of type to type copy operations

(defmethod type-info-texpr ((type-info c-type-info))
  (with-slots (ir-texpr) type-info ir-texpr))

(defmethod type-info-texpr ((type-info simple-c-type-info))
  (with-slots (ir-texpr) type-info ir-texpr))

(defmethod type-info-texpr ((type-info t))
  nil)

(defmethod closure-info-lambda-expr ((closure-info c-closure-info))
  (with-slots (ir-lambda-expr) closure-info ir-lambda-expr))

(defmethod closure-info-lambda-expr ((closure-info t))
  nil)

(defun get-c-type-info (ir-texpr)
  (find ir-texpr *c-type-info-table* :key #'type-info-texpr :test #'ir2c-tequal))

(defun get-c-type-info-by-name (tname)
  (find tname *c-type-info-table* :key #'tname))

(defun get-c-closure-info (ir-lambda-expr)
  (find ir-lambda-expr *c-type-info-table* :key #'closure-info-lambda-expr :test #'equalp))

(defun mk-simple-c-type-info (ir-texpr tname tdefn act-defn)
  (make-instance 'simple-c-type-info
		   :ir-texpr ir-texpr
		   :tname tname
		   :tdefn tdefn
		   :act-defn act-defn))

(defun mk-closure-c-type-info (ir-texpr tname tdecl tdefn ftable-type-defn release-info copy-info
					hash-entry-type-defn hash-type-defn
					lookup-info dupdate-info update-info)
  (make-instance 'closure-c-type-info
		   :ir-texpr ir-texpr
		   :tname tname
		   :tdefn tdefn
		   :tdecl tdecl
		   :ftable-type-defn ftable-type-defn
		   :release-info release-info
		   :copy-info copy-info
		   :hash-entry-type-defn hash-entry-type-defn
		   :hash-type-defn hash-type-defn
		   :lookup-info lookup-info
		   :dupdate-info dupdate-info
		   :update-info update-info))

(defun mk-c-type-info (ir-texpr tname tdefn act-defn new-info release-info copy-info equal-info update-info)
  (make-instance 'c-type-info
		   :ir-texpr ir-texpr
		   :tname tname
		   :tdefn tdefn
		   :act-defn act-defn
		   :new-info new-info
		   :release-info release-info
		   :copy-info copy-info
		   :equal-info equal-info
		   :update-info update-info))


(defun mk-c-closure-info (ir-lambda-expr tname tdecl tdefn fdefn mdefn hdefn new-info release-info copy-info)
  (make-instance 'c-closure-info
		 :ir-lambda-expr ir-lambda-expr
		 :tname tname
		 :tdecl tdecl
		 :tdefn tdefn
		 :fdefn fdefn
		 :mdefn mdefn
		 :hdefn hdefn
		 :new-info new-info
		 :release-info release-info
		 :copy-info copy-info))

(defun add-closure-definition (ir-lambda-expr)
  (let* ((closure-info (get-c-closure-info ir-lambda-expr))
	 (closure-fdefn (when closure-info (fdefn closure-info))))
    (or (and closure-fdefn (op-name closure-fdefn))
	(let* ((ir-freevars (pvs2ir-freevars* ir-lambda-expr))
	       (ir-fvar-types (loop for fvar in ir-freevars collect (ir-vtype fvar)))
	       (ir-fvar-ctypes (loop for fvtype in ir-fvar-types collect
				     (add-c-type-definition (ir2c-type fvtype))))
	       (ir-boundvars (ir-vartypes ir-lambda-expr))
	       (ir-rangetype (ir-rangetype ir-lambda-expr))
	       (ir-domaintype (if (eql (length ir-boundvars) 1)
				   (ir-vtype (car ir-boundvars))
				 (let ((ir-field-type (mapcar #'ir-vtype ir-boundvars)))
				   (mk-ir-tupletype (loop for type in ir-field-type
							   as i from 1
							   collect (mk-ir-fieldtype (intern (format nil "project_~a" i)) type))))))
							
	       (c-rangetype (add-c-type-definition (ir2c-type ir-rangetype)))
	       (c-domaintype (add-c-type-definition (ir2c-type ir-domaintype)))
	       (c-funtype (add-c-type-definition (ir2c-type (mk-ir-funtype ir-domaintype ir-rangetype))))
	       (ir-body (ir-body ir-lambda-expr))
	       (closure-name-root (format nil "~a_closure_~a" *theory-id* (length *c-type-info-table*)))
	       (closure-type-name (format nil "~a_t" closure-name-root))
	       (closure-struct-name (format nil "~a_s" closure-name-root))
	       (bvar-ctypes
		(loop for bv in ir-boundvars collect (format nil "~a_t" (add-c-type-definition (ir2c-type (ir-vtype bv))))))
	       (bvar-cvars (if (eql (length ir-boundvars) 1) (list "bvar")
			     (loop for bv in ir-boundvars as i from 1 collect (format nil "bvar_~a" i))))
	       (bvar-cvar-decls (loop for bv-ctype in bvar-ctypes
				      as bv in bvar-cvars
				      collect (format nil "~a ~a" bv-ctype bv)))
	       (fvar-cvars (loop for fv in ir-fvar-ctypes as i from 1 collect (format nil "fvar_~a" i)))
	       (fvar-cvar-decls (loop for fv-ctype in ir-fvar-ctypes
				      as fv in fvar-cvars
				      collect (format nil "~a_t ~a" fv-ctype fv)))
	       (ftbl-type-name (format nil "~a_ftbl" c-funtype))
	       (closure-type-decl (format nil "~%struct ~a;~%~8Ttypedef struct ~a * ~a;"
					  closure-struct-name closure-struct-name closure-type-name))
	       (closure-type-defn ;the fptr takes the closure itself and a pointer to the lambda-args
		(format nil "struct ~a {uint32_t count;~%~8T ~a_t ftbl;~%~8T ~a_htbl_t htbl;~{~%~8T~a;~}};"
			closure-struct-name 
			ftbl-type-name c-funtype
			fvar-cvar-decls
			))
	       (closure-fptr-name (format nil "f_~a" closure-name-root))
	       (closure-mptr-name (format nil "m_~a" closure-name-root))
	       (closure-hptr-name (format nil "h_~a" closure-name-root))
	       (closure-cptr-name (format nil "copy_~a" closure-name-root))
	       (closure-cptr-header (format nil "struct ~a * ~a(struct ~a * closure)"
					    closure-struct-name closure-cptr-name closure-struct-name))
	       (closure-fptr-header
		(format nil "~a_t ~a(struct ~a * closure, ~a_t bvar);"
			(mppointer-type c-rangetype)
			closure-fptr-name closure-struct-name c-domaintype))
		;(when (null c-rangetype) (break "closure"))
		;; (case c-rangetype
		;;   ((mpq mpz)
		;;    (format nil "void ~a(struct ~a * closure, ~a_t result, ~a_t bvar);"
		;; 	   closure-fptr-name closure-struct-name c-rangetype  c-domaintype))
		;;   (t (format nil "~a_t ~a(struct ~a * closure, ~a_t bvar);"
		;; 	     c-rangetype closure-fptr-name closure-struct-name c-domaintype)))
	       (closure-mptr-header
		(format nil "~a_t ~a(struct ~a * closure, ~{~a~^, ~});"
			(mppointer-type c-rangetype)
			closure-mptr-name closure-struct-name
			bvar-cvar-decls))
		;; (case c-rangetype
		;; 		      ((mpq mpz)
		;; 		       (format nil "void ~a(struct ~a * closure, ~a_t result, ~{~a~^, ~});"
		;; 			       closure-mptr-name closure-struct-name c-rangetype
		;; 			       bvar-cvar-decls))
		;; 		      (t (format nil "~a_t ~a(struct ~a * closure, ~{~a~^, ~});"
		;; 				 c-rangetype closure-mptr-name closure-struct-name
		;; 				 bvar-cvar-decls)))
	       (bvar_projections
		(when (> (length ir-boundvars) 1)
		  (nconc (loop for ir-bvar in ir-boundvars
			       as i from 1
			       nconc (let ((atype (add-c-type-definition (ir2c-type (ir-vtype ir-bvar)))))
				       (cons (format nil "~a_t bvar_~a" atype i)
					     (cons (make-c-assignment
						    (format nil "bvar_~a" i)
						    atype
						    (format nil "bvar->project_~a" i)
						    atype)
						   (when (ir-reference-type? (ir-vtype ir-bvar))
						     (list (format nil "bvar->project_~a->count++" i)))))))
			 (list (format nil "release_~a(bvar~{, ~a~})" c-domaintype
				       (loop for ir-formal in (free-ir-formals ir-domaintype)
					     collect (ir-type-id ir-formal)))))))
	       (release-bvar-projections
		(when (> (length ir-boundvars) 1)
		  (loop for ir-bvar in ir-boundvars
			as i from 1
			when (or (ir-reference-type? (ir-vtype ir-bvar))
				 (gmp-type? (ir2c-type (ir-vtype ir-bvar))))
			collect (let ((atype (add-c-type-definition (ir2c-type (ir-vtype ir-bvar)))))
				  (format nil "release_~a(bvar_~a)" atype i)))))
	       (bvar-fvar-args (format nil "~{~a~^, ~}~{, ~a~}"
				       bvar-cvars
				       (loop for i from 1 to (length ir-freevars)
					     collect (format nil "closure->fvar_~a" i))))
	       (hashable-domain (ir-hashable-index? (ir2c-type ir-domaintype)))
	       (hashable-range (ir-hashable-index? (ir2c-type ir-rangetype)))
	       (closure-fptr-defn 
		(if (and hashable-domain hashable-range)
		    (format nil "~a_t ~a(struct ~a * closure, ~a_t bvar){~
~%if (closure->htbl != NULL){~
~%~8T~a_htbl_t htbl = closure->htbl;~
~%~8Tuint32_t hash = ~a_hash(bvar);~
~%~8Tuint32_t hashindex = lookup_~a(htbl, bvar, hash);~
~%~8Tif (htbl->data[hashindex].key !=0 && htbl->data[hashindex].keyhash != 0)~
~%~12Treturn htbl->data[hashindex].value;
~%~8Treturn ~a(~a);}
~%return ~a(~a);}"
			    c-rangetype closure-fptr-name closure-struct-name c-domaintype
			    c-funtype
			    hashable-domain
			    c-funtype
			    closure-hptr-name bvar-fvar-args
			    closure-hptr-name bvar-fvar-args)
		  
		(format nil "~a_t ~a(struct ~a * closure, ~a_t bvar){~{~%~8T~a;~}~%~8T~a_t result = ~a(~a); ~{~%~8T~a;~}~%~8Treturn result;}"
			(mppointer-type c-rangetype)
			closure-fptr-name closure-struct-name c-domaintype
			bvar_projections
			(mppointer-type c-rangetype)
			closure-hptr-name
			bvar-fvar-args
			release-bvar-projections
			)
		;; (case c-rangetype
		;;   ((mpq mpz)
		;;    ;(format t "bvar-fvar-args: ~a" bvar-fvar-args)
		;;    (format nil "void ~a(struct ~a * closure, ~a_t result, ~a_t bvar){~{~%~8T~a;~}~%~8T~a(result, ~a);}"
		;; 	closure-fptr-name closure-struct-name c-rangetype c-domaintype
		;; 	bvar_projections
		;; 	closure-hptr-name
		;; 	bvar-fvar-args
		;; 	))
		;;    (t (format nil "~a_t ~a(struct ~a * closure, ~a_t bvar){~{~%~8T~a;~}~%~8Treturn ~a(~a);}"
		;; 	      c-rangetype closure-fptr-name closure-struct-name c-domaintype
		;; 	      bvar_projections
		;; 	      closure-hptr-name
		;; 	      bvar-fvar-args
		;; 	      )))
		))
	       (closure-fptr-definition (mk-c-defn-info closure-fptr-name closure-fptr-header
							closure-fptr-defn))
	       (closure-mptr-defn
		(format nil "~a_t ~a(struct ~a * closure, ~{~a~^, ~}){~%~8Treturn ~a(~a);}"
			(mppointer-type c-rangetype)
			closure-mptr-name closure-struct-name bvar-cvar-decls
			closure-hptr-name
			bvar-fvar-args
			)
		;; (case c-rangetype
		;;   ((mpq mpz)
		;;    ;(format t "bvar-fvar-args: ~a" bvar-fvar-args)
		;;    (format nil "void ~a(struct ~a * closure, ~a_t result, ~{~a~^, ~}){~%~8T~a(result, ~a);}"
		;; 	   closure-mptr-name closure-struct-name c-rangetype
		;; 	   bvar-cvar-decls
		;; 	closure-hptr-name
		;; 	bvar-fvar-args
		;; 	))
		;;    (t (format nil "~a_t ~a(struct ~a * closure, ~{~a~^, ~}){~%~8Treturn ~a(~a);}"
		;; 	      c-rangetype closure-mptr-name closure-struct-name bvar-cvar-decls
		;; 	      closure-hptr-name
		;; 	      bvar-fvar-args
		;; 	      )))
		)
	       (closure-mptr-definition (mk-c-defn-info closure-mptr-name closure-mptr-header
							closure-mptr-defn))
	       (fptr-cast (format nil "~a_t (*)(~a_t, ~a_t)" (mppointer-type c-rangetype)
				  c-funtype c-domaintype))
	       ;; (case c-rangetype
	       ;; 			 ((mpq mpz)
	       ;; 			  (format nil "void (*)(~a_t, ~a_t, ~a_t)" c-funtype c-rangetype c-domaintype))
	       ;; 			 (t (format nil "~a_t (*)(~a_t, ~a_t)" c-rangetype c-funtype c-domaintype)))
	       (mptr-cast (format nil "~a_t (*)(~a_t, ~{~a~^, ~})" (mppointer-type c-rangetype)
				  c-funtype bvar-ctypes)
			  ;; (case c-rangetype
			    ;; ((mpq mpz)
			    ;;  (format nil "void (*)(~a_t, ~a_t, ~{~a~^, ~})" c-funtype c-rangetype bvar-ctypes))
			    ;; (t (format nil "~a_t (*)(~a_t, ~{~a~^, ~})" c-rangetype c-funtype bvar-ctypes)))
			  )
	       (rptr-cast (format nil "void (*)(~a_t)" c-funtype))
	       (cptr-cast (format nil "~a_t (*)(~a_t)" c-funtype c-funtype))
	       (copy-info (make-closure-copy-info closure-name-root c-funtype ir-freevars))
	       (new-info (make-closure-new-info closure-name-root ftbl-type-name fptr-cast mptr-cast rptr-cast cptr-cast ir-freevars));can reuse record-new-info
	       (release-info (make-closure-release-info c-funtype closure-name-root ir-fvar-types ir-fvar-ctypes))
	       (closure-defn-info
		(mk-c-closure-info ir-lambda-expr closure-name-root closure-type-decl closure-type-defn closure-fptr-definition closure-mptr-definition nil new-info release-info copy-info)))
	  (push closure-defn-info
		*c-type-info-table*)
	  (let ((closure-function-definition
		 (make-c-closure-defn-info ir-lambda-expr closure-hptr-name))
		)
	    (setf (hdefn closure-defn-info) closure-function-definition)
	    closure-name-root)))))

(defun make-closure-copy-info (type-name-root c-funtype ir-freevars)
  (let* ((new-name (intern (format nil "copy_~a" type-name-root)))
	 (new-header (format nil "extern ~a_t copy_~a(~a_t x);" type-name-root type-name-root type-name-root))
	 (new-defn (format nil "~a_t copy_~a(~a_t x){~%~8T~a_t y = new_~a();
~8Ty->ftbl = x->ftbl;
~{~%~8T~a;~}
~8Tif (x->htbl != NULL){
~12T~a_htbl_t new_htbl = (~a_htbl_t) safe_malloc(sizeof(~a_htbl_t));
~12Tnew_htbl->size = x->htbl->size;
~12Tnew_htbl->num_entries = x->htbl->num_entries;
~12T~a_hashentry_t * new_data = (~a_hashentry_t *) safe_malloc(new_htbl->size * sizeof(~a_hashentry_t));
~12Tmemcpy(new_data, new_htbl->data, (new_htbl->size * sizeof(~a_hashentry_t)));
~8T}
~8Treturn y;
}"
			   type-name-root type-name-root type-name-root type-name-root type-name-root
			   (loop for ifv in ir-freevars as i from 1
				 collect
				 (let* ((fv-type (ir-vtype ifv))
				       (fv-ctype (add-c-type-definition (ir2c-type fv-type))))
				   (if (ir-reference-type? fv-type)
				       (format nil "y->fvar_~a = x->fvar_~a; x->fvar_~a->count++"
					       i i i)
				     (make-c-assignment (format nil "y->fvar_~a" i) fv-ctype
							(format nil "x->fvar_~a" i) fv-ctype))))
			   c-funtype c-funtype c-funtype
			   c-funtype c-funtype c-funtype
			   c-funtype
			   )))
	 (mk-c-defn-info new-name new-header new-defn (list type-name-root) type-name-root )))

(defun make-closure-new-info (type-name-root ftbl-type-name fptr-cast mptr-cast rptr-cast cptr-cast ir-freevars)
    (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	   (new-header (format nil "extern ~a_t new_~a(void);" type-name-root type-name-root))
	   (gmp-freevars-init (loop for ifv in ir-freevars
				    as i from 1
				    when (mpnumber-type? (ir2c-type (ir-vtype ifv)))
				    collect (format nil "~a_init(tmp->fvar_~d)" (ir2c-type (ir-vtype ifv)) i)))
	   (new-defn   (format nil "~a_t new_~a(void){~%~8Tstatic struct ~a_s ftbl = {.fptr = (~a)&f_~a, .mptr = (~a)&m_~a, .rptr =  (~a)&release_~a, .cptr = (~a)&copy_~a};~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s));~%~8Ttmp->count = 1;~%~8Ttmp->ftbl = &ftbl;~%~8Ttmp->htbl = NULL;~{~%~8T~a;~}~%~8Treturn tmp;}"
			       type-name-root type-name-root
			       ftbl-type-name fptr-cast type-name-root mptr-cast type-name-root rptr-cast type-name-root cptr-cast type-name-root
			       type-name-root type-name-root type-name-root gmp-freevars-init)))
      (mk-c-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-closure-release-info (funtype-root closure-name-root ir-fvar-types ir-fvar-ctypes)
  (let* ((release-name (intern (format nil "release_~a" closure-name-root)))
	 (release-header (format nil "extern void release_~a(~a_t closure);" closure-name-root funtype-root))
	 (release-defn (let ((release-fields (loop for ft in ir-fvar-types
						   as cft in ir-fvar-ctypes
						   as i from 1 
						   when (ir-reference-type? ft)
						   collect (format nil "release_~a(x->fvar_~a)" cft i))))
						   
			 (format nil "void release_~a(~a_t closure){~%~8T~a_t x = (~a_t)closure;~%~8Tx->count--;~%~8Tif (x->count <= 0){~{~%~8T~8T~a;~}~%~8T//printf(\"\\nFreeing\\n\");~%~8Tsafe_free(x);}}"
				 closure-name-root funtype-root closure-name-root closure-name-root
				 release-fields))))
    (mk-c-defn-info release-name release-header release-defn (list funtype-root) 'void)))

										  
(defmethod push-type-info-to-decl (c-type-info (decl const-decl))
  (when (consp c-type-info) (break "push-type-info-to-decl"))
  (push c-type-info *c-type-info-table*)
  (push c-type-info (c-type-info-table (eval-info decl))))

(defmethod push-type-info-to-decl (c-type-info (decl formal-const-decl))
  (when (consp c-type-info) (break "push-type-info-to-decl"))  
  (push c-type-info *c-type-info-table*)
  (push c-type-info (c-type-info-table (eval-info decl))))

(defmethod push-type-info-to-decl (c-type-info (decl type-decl))
  (when (consp c-type-info) (break "push-type-info-to-decl"))  
  (push c-type-info *c-type-info-table*)
  (push c-type-info (c-type-info-table (ir-type-value decl))))

(defmethod push-type-info-to-decl (c-type-info (decl t))
  (when (consp c-type-info) (break "push-type-info-to-decl"))  
  (format t "\nEmpty decl")
  (push c-type-info *c-type-info-table*))

(defmethod get-c-type-info-table ((decl  const-decl))
  (c-type-info-table (eval-info decl)))

(defmethod get-c-type-info-table ((decl  formal-const-decl))
  (c-type-info-table (eval-info decl)))

(defmethod get-c-type-info-table ((decl type-decl))
  (c-type-info-table (ir-type-value decl)))

(defmethod add-c-type-definition ((ir2c-type t) &optional tname)
  (cond (tname
	 (let ((c-type-info (get-c-type-info-by-name tname)))
	   ;(when (null ir2c-type) (break "add-c-type-definition"))
	   (cond (c-type-info tname)
		 (t (let ((c-type-info (mk-simple-c-type-info ir2c-type tname (format nil "typedef ~a_t ~a_t;" ir2c-type tname) nil)));;replace nil with act-defn
		      (push-type-info-to-decl c-type-info
						*pvs2c-current-decl*)
		      tname)))))
	(t ir2c-type)))

(defmethod add-c-type-definition ((ir2c-type ir-type-actual) &optional tname) 
  (with-slots (ir-actual-type) ir2c-type
	      (add-c-type-definition ir-actual-type tname)))

(defmethod add-c-type-definition ((ir2c-type ir-typename) &optional tname) ;;tname is ignored
  ;;we assume that a named type already has generated code
  (with-slots (ir-type-id ir-type-defn) ir2c-type
    (let* ((c-type-info (get-c-type-info ir2c-type))
	   (c-type-name (when c-type-info (tname c-type-info))))
      (or c-type-name ir-type-id))))

(defmethod add-c-type-definition ((ir2c-type ir-funtype) &optional tname)
  (with-slots (ir-domain ir-range) ir2c-type
	      (let* ((c-type-info (get-c-type-info ir2c-type))
		     (c-type-name (when c-type-info (tname c-type-info))))
		(or c-type-name
		    (let* ((c-domain-root (add-c-type-definition (ir2c-type ir-domain)));remove ir2c-type
			   (c-range-root (add-c-type-definition (ir2c-type ir-range)))
			   (type-name-root (or tname (format nil "~a_funtype_~a"
							     *theory-id*
							     (length *c-type-info-table*))))
			   (type-name (intern (format nil "~a_t" type-name-root)))
			   (struct-name (intern (format nil "~a_s" type-name-root)))
			   (theory-params (free-ir-formals ir2c-type)))
		      ;(break "add-c-type-definition ir-funtype")
		      (if (ir-index? ir-domain);;NSH(8/15/2016): This should be unreachable since
			  ;;the ir2c-type would be ir-arraytype in this case
			  (let* ((size (ir-index? ir-domain))
				 (elemtype ir-range)
				 (theory-formals (loop for ir-formal in theory-params
						       collect (car (rassoc (ir-type-id ir-formal)
									    *ir-theory-tbindings* :key #'ir-name))))
				 (theory-c-params (ir2c-theory-formals theory-params theory-formals))
				 (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
				 (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
										  theory-params
										  collect (ir-type-id ir-formal))))
				 (type-defn (format nil "struct ~a { uint32_t count;~% ~a_t elems[~a]; };~%typedef struct ~a * ~a;"
						    struct-name c-range-root size struct-name type-name))
				 (new-info (make-array-new-info type-name-root size elemtype))
				 (release-info (make-array-release-info type-name-root size elemtype c-range-root c-param-arg-string c-param-decl-string))
				 (copy-info (make-array-copy-info type-name-root size elemtype c-range-root c-param-decl-string))
				 (equal-info (make-array-equal-info type-name-root size elemtype c-range-root c-param-decl-string))
				 (update-info (list (make-array-update-info type-name-root elemtype c-param-arg-string c-param-decl-string)))
				 (actual-info (make-array-actual-info type-name-root elemtype c-param-arg-string c-param-decl-string))
				 );(break "add-c-type-definition(ir-funtype)")(break "Shouldn't reach here")
			    (push-type-info-to-decl
			     (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info copy-info equal-info update-info) ; deleted actual-info
			     *pvs2c-current-decl*)
			    type-name-root)
			(let* ((hash-entry-type-defn
				(format nil "struct ~a_hashentry_s {uint32_t keyhash; ~a_t key; ~a_t value;}; ~%typedef struct ~a_hashentry_s ~a_hashentry_t;" type-name-root c-domain-root c-range-root type-name-root type-name-root))
				(hash-type-defn
				 (format nil "struct ~a_htbl_s {uint32_t size; uint32_t num_entries; ~a_hashentry_t * data;}; ~%typedef struct ~a_htbl_s * ~a_htbl_t;" type-name-root type-name-root type-name-root type-name-root))
				(arg-types (if (ir-tupletype? ir-domain)
					       (loop for fld in (ir-field-types ir-domain)
						     collect (add-c-type-definition (ir-ftype fld)))
					     (list c-domain-root)))
				(ftable-name (intern (format nil "~a_ftbl" type-name-root)))
				(ftable-type-defn
				 (format nil "struct ~a_s {~a_t (* fptr)(struct ~a *, ~a_t);~%~8T~a_t (* mptr)(struct ~a *, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
					 ftable-name (mppointer-type c-range-root)
					 struct-name c-domain-root  (mppointer-type c-range-root)
					 struct-name arg-types  struct-name struct-name struct-name ftable-name ftable-name)
				 ;; (case c-range-root
				 ;;  ((mpq mpz)
				 ;;   (format nil "struct ~a_s {void (* fptr)(struct ~a *, ~a_t, ~a_t);~%~8Tvoid (* mptr)(struct ~a *, ~a_t, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
				 ;; 		  ftable-name  struct-name c-range-root c-domain-root struct-name c-range-root arg-types struct-name struct-name struct-name ftable-name ftable-name))
				 ;;  (t (format nil "struct ~a_s {~a_t (* fptr)(struct ~a *, ~a_t);~%~8T~a_t (* mptr)(struct ~a *, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
				 ;; 	     ftable-name c-range-root struct-name c-domain-root  c-range-root struct-name arg-types  struct-name struct-name struct-name ftable-name ftable-name)))
				 )
				(type-decl (format nil "struct ~a;~%~8Ttypedef struct ~a * ~a;" struct-name
						   struct-name type-name))
				(type-defn (format nil "struct ~a {uint32_t count;~%~8T~a_ftbl_t ftbl;~%~8T~a_htbl_t htbl;};~%typedef struct ~a * ~a;" struct-name  type-name-root type-name-root struct-name type-name))
				(hashtype (when (ir-hashable-index? ir-range)(ir-hashable-index? ir-domain)))
				;;need range to be a fixed-width int
				(release-info (make-function-release-info type-name-root))
				(copy-info (make-function-copy-info type-name-root c-domain-root))
				(lookup-info (when hashtype (make-function-lookup-info type-name-root c-domain-root hashtype)))
				(dupdate-info (when hashtype (make-function-dupdate-info type-name-root c-domain-root hashtype c-range-root)))
				(update-info (when hashtype (make-function-update-info type-name-root c-domain-root c-range-root))))
			  (push-type-info-to-decl
			   (mk-closure-c-type-info ir2c-type type-name-root type-decl type-defn ftable-type-defn
						   release-info copy-info  hash-entry-type-defn hash-type-defn
						   lookup-info dupdate-info update-info)
			   *pvs2c-current-decl*)))
		      type-name-root)))))

(defun make-function-copy-info (type-name-root c-domain-root)
  (let* ((name (format nil "copy_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t x)" type-name-root name type-name-root c-domain-root))
	 (body   (format nil "{return x->ftbl->cptr(x);}"))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "extern ~a;" header) defn (list type-name-root) type-name-root)))

(defun make-function-lookup-info (type-name-root c-domain-root htype )
  (let* ((name (format nil "lookup_~a" type-name-root))
	 (header (format nil "uint32_t ~a(~a_htbl_t htbl, ~a_t i, uint32_t ihash)" name type-name-root c-domain-root))
	 (body   (format nil "{
~8Tuint32_t mask = htbl->size - 1;
~8Tuint32_t hashindex = ihash & mask;~
~8T~a_hashentry_t * data = htbl->data;
~8Twhile ((data[hashindex].key != 0 || data[hashindex].keyhash != 0) &&
~16T (data[hashindex].keyhash != ihash || data[hashindex].key != i)){
~16Thashindex++;
~16Thashindex = hashindex & mask;
~16T}
~8Treturn hashindex;
~8T}"
			 type-name-root))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "~a;" header) defn
		    (list (format nil "~a_htbl" type-name-root) c-domain-root 'uint32))))

(defun make-function-update-info (type-name-root c-domain-root c-range-root)
  (let* ((name (format nil "update_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t a, ~a_t i, ~a_t v)" type-name-root name type-name-root c-domain-root c-range-root))
	 (body (format nil "{
~8Tif (a->count == 1){
~16Treturn dupdate_~a(a, i, v);
~12T} else {
~16T~a_t x = copy_~a(a);
~16Treturn dupdate_~a(x, i, v);
~12T}}" type-name-root type-name-root type-name-root type-name-root))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name (format nil "extern ~a;" header) defn (list type-name-root c-domain-root c-range-root) type-name-root)))

(defun make-function-dupdate-info (type-name-root c-domain-root htype c-range-root)
  (let* ((name (format nil "dupdate_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t a, ~a_t i, ~a_t v)" type-name-root name type-name-root c-domain-root c-range-root))
	 (body (function-destructive-update-body type-name-root c-domain-root htype))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "~a;" header) defn (list type-name-root c-domain-root c-range-root) type-name-root)))

(defun function-destructive-update-body (type-name-root c-domain-root htype)
  (format nil "{~%~8T~a_htbl_t htbl = a->htbl;~
~%~8Tif (htbl == NULL){//construct new htbl~
~%~16Thtbl = (~a_htbl_t)safe_malloc(sizeof(struct ~a_htbl_s));~
~%~16Thtbl->size = HTBL_DEFAULT_SIZE; htbl->num_entries = 0;~
~%~16Thtbl->data = (~a_hashentry_t *)safe_malloc(HTBL_DEFAULT_SIZE * sizeof(struct ~a_hashentry_s));~
~%~16Tfor (uint32_t j = 0; j < HTBL_DEFAULT_SIZE; j++){htbl->data[j].key = 0; htbl->data[j].keyhash = 0;~
~%~16T}~
~%~8T}~
~%~8Ta->htbl = htbl;~
~%~8Tuint32_t size = htbl->size;~
~%~8Tuint32_t num_entries = htbl->num_entries;~
~%~8T~a_hashentry_t * data = htbl->data;~
~%~8Tif (num_entries/3 >  size/5){//resize data~
~%~16Tuint32_t new_size = 2*size; uint32_t new_mask = new_size - 1;~
~%~16Tif (size >= HTBL_MAX_SIZE) out_of_memory();~
~%~16T~a_hashentry_t * new_data = (~a_hashentry_t *)safe_malloc(new_size * sizeof(struct ~a_hashentry_s));~
~%~16Tfor (uint32_t j = 0; j < size; j++){//transfer entries~
~%~24T~a_t key = data[j].key;~
~%~24Tuint32_t keyhash = data[j].keyhash;~
~%~24Tif (key != 0 || keyhash != 0){~
~%~32Tuint32_t new_loc = keyhash ^ new_mask;~
~%~32Twhile (new_data[new_loc].key == 0 && new_data[new_loc].keyhash == 0){~
~%~40Tnew_loc++;~
~%~40Tnew_loc = new_loc ^ new_mask;}~
~%~32Tnew_data[new_loc].key = key;~
~%~32Tnew_data[new_loc].keyhash = keyhash;~
~%~32Tnew_data[new_loc].value = data[j].value;~
~%~32T}}~
~%~16Thtbl->size = new_size;~
~%~16Thtbl->num_entries = num_entries;~
~%~16Thtbl->data = new_data;~
~%~16Tsafe_free(data);}~
~%~8Tuint32_t ihash = ~a_hash((~a_t)i);~
~%~8Tuint32_t hashindex = lookup_~a(htbl, i, ihash);~
~%~8T~a_hashentry_t hentry = htbl->data[hashindex];~
~%~8Tuint32_t hkeyhash = hentry.keyhash;~
~%~8Tif ((hentry.key == 0) && (hkeyhash == 0))~
~%~16T{htbl->data[hashindex].key = i; htbl->data[hashindex].keyhash = ihash; htbl->data[hashindex].value = v;}~
~%~12Telse htbl->data[hashindex].value = v;~
~%~8Treturn a;
~%}"
	  type-name-root
	  type-name-root type-name-root
	  type-name-root type-name-root
	  type-name-root
	  type-name-root type-name-root type-name-root 
	  c-domain-root
	  htype htype
	  type-name-root
	  type-name-root))

(defun make-function-release-info (type-name-root)
  (let* ((name (format nil "release_~a" type-name-root))
	  (header (format nil "void ~a(~a_t x)" name type-name-root))
	 (body (format nil "{~%~8Tif (x->htbl != NULL){
~%~12Tif (x->htbl->data != NULL) safe_free(x->htbl->data);
~%~12Tsafe_free(x->htbl);}
~%~8Tx->ftbl->rptr(x);}"))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name (format nil "extern ~a;" header) defn (list type-name-root) 'void)))



(defmethod add-c-type-definition ((ir2c-type ir-arraytype) &optional tname)
;  (break "add-c-type-def arraytype")
  (with-slots (size elemtype) ir2c-type
    (let* ((c-type-info (get-c-type-info ir2c-type)))
      (or (and c-type-info (tname c-type-info))
	  (let* ((c-range-root (add-c-type-definition elemtype))
		 (type-name-root (or tname (format nil "~a_array_~a"
						   *theory-id*
						   (length *c-type-info-table*))))
		 (type-name (intern (format nil "~a_t" type-name-root)))
		 (struct-name (intern (format nil "~a_s" type-name-root)))
		 (theory-params (free-ir-formals ir2c-type)) ;;only ir-formal-typenames
		 (theory-formals (loop for ir-formal in theory-params
				       collect (car (rassoc (ir-type-id ir-formal)
							    *ir-theory-tbindings* :key #'ir-name))))
		 (theory-c-params (ir2c-theory-formals theory-params theory-formals))
		 (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
		 (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								  theory-params
								  collect (ir-type-id ir-formal))))
		 (type-defn (format nil "struct ~a { uint32_t count;~% ~a_t elems[]; };~%typedef struct ~a * ~a;"  ;~%  uint16_t size~%  uint16_t max;
				    struct-name c-range-root ;size
				    struct-name type-name))) ;(break "add-c-type")
	    (make-array-c-type-info ir2c-type type-name-root type-defn size
				    elemtype c-range-root theory-params c-param-arg-string c-param-decl-string)
	    )))))

(defun make-array-c-type-info (ir2c-type type-name-root type-defn size elemtype c-range-root theory-params c-param-arg-string c-param-decl-string)
  (let* ((new-info (make-array-new-info type-name-root size elemtype))
	 (release-info (make-array-release-info type-name-root size elemtype
						c-range-root c-param-arg-string c-param-decl-string))
	 (copy-info (make-array-copy-info type-name-root size elemtype c-range-root c-param-decl-string))
	 (equal-info (make-array-equal-info type-name-root size elemtype c-range-root c-param-decl-string))
	 (update-info (list (make-array-update-info type-name-root elemtype c-param-arg-string c-param-decl-string)))
	 (actual-info (make-array-actual-info type-name-root theory-params c-param-arg-string c-param-decl-string))
	 )
    (push-type-info-to-decl
     (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info copy-info equal-info update-info)
     *pvs2c-current-decl*)
    type-name-root))

(defcl ir-actual-info ()
  ir-actual-type-defn
  ir-actual-fun-defn)

(defun mk-ir-actual-info (type-defn fun-defn)
  (make-instance 'ir-actual-info
		 :ir-actual-type-defn type-defn
		 :ir-actual-fun-defn fun-defn))

(defun make-array-actual-info (type-name-root theory-params c-param-arg-string c-param-decl-string)
  (let* ((type-param-ids (loop for formal in theory-params
			      when (ir-formal-typename? formal)
			      collect (ir-type-id formal)))
	 (actual-type-def (format nil "struct actual_~a_s {~%~8Tuint32_t count;~%~8Tbool_t *eq_ptr (~a_t x, ~a_t y~a);~
~8T(void *) * release_ptr(~a_t~a);~
~%~{~8Ttype_actual_t ~a;~}}~
~%~8Ttypedef struct actual_~a_s * actual_~a_t ;"
				  type-name-root type-name-root type-name-root
				  c-param-decl-string type-name-root c-param-decl-string
				  type-param-ids
				  type-name-root type-name-root))
	 (actual-def (format nil "actual_~a_t actual_~a(~{type_actual_t ~a~^,~}){~
~%~8Tactual_~a_t new = (actual_~a_t)safe_malloc(sizeof(struct actual_~a_s));~
~%~8Tnew->eq_ptr = (*eq_~a);
~%~8Tnew->release_ptr = (*release_~a);
~{~%~8T~a~}
};"
			     type-name-root type-name-root type-param-ids
			     type-name-root
			     type-name-root type-name-root type-name-root type-name-root
			     (loop for id in type-param-ids collect
				   (format nil "new->~a = ~a;" id id)))))
  (mk-ir-actual-info actual-type-def actual-def)))

(defun make-array-new-info (type-name-root size elemtype)
  (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	 (new-header (make-new-header new-name type-name-root))
	 (new-defn (make-array-new-defn new-name type-name-root size elemtype)))
    (mk-c-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-new-header (new-name type-name-root) ; c-param-decl-string
  (format nil "extern ~a_t ~a(void);" type-name-root new-name))


(defun make-array-new-defn (new-name type-name-root size ir-range)
  (let ((c-range-root  (add-c-type-definition ir-range)))
    (if (ir-reference-type? ir-range)
	(format nil "~a_t ~a(){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s) + (~d * sizeof(~a_t)));~%~8Ttmp->count = 1;~%~8Treturn tmp;}"  ;~%~8Tsize = ~d;~%~8Tmax = ~d;
		type-name-root new-name type-name-root type-name-root type-name-root size c-range-root)
      (format nil "~a_t ~a(){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s) + (~d * sizeof(~a_t)));~%~8Ttmp->count = 1;~%~8T return tmp;}"  ;~%~8Tmax = ~d;
	      type-name-root new-name type-name-root type-name-root type-name-root size c-range-root
	      ))))

(defun make-array-release-info (type-name-root size elemtype c-range-root c-param-arg-string c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (make-array-release-header release-name type-name-root c-param-decl-string))
	 (release-defn (make-array-release-defn release-name type-name-root size elemtype c-range-root  c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-array-copy-info (type-name-root size elemtype c-range-root c-param-decl-string)
  (let* ((copy-name (intern (format nil "copy_~a" type-name-root)))
	 (copy-header (make-array-copy-header copy-name type-name-root elemtype c-param-decl-string))
	 (copy-defn (make-array-copy-defn copy-name type-name-root size elemtype c-range-root c-param-decl-string)))
    (mk-c-defn-info copy-name copy-header copy-defn (list type-name-root) type-name-root)))

(defun make-array-update-info (type-name-root elemtype c-param-arg-string c-param-decl-string)
  (let* ((update-name (intern (format nil "update_~a" type-name-root)))
	 (update-header (make-array-update-header update-name type-name-root elemtype c-param-decl-string))
	 (update-defn (make-array-update-defn update-name type-name-root elemtype c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info update-name update-header update-defn (list type-name-root elemtype) type-name-root)))
					     
(defmethod ir-reference-type? ((ir-type ir-arraytype))
  t)

(defmethod ir-reference-type? ((ir-type ir-funtype));an array or closure
  t)

(defmethod ir-reference-type? ((ir-type ir-recordtype))
  t)

(defmethod ir-reference-type? ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      (ir-reference-type? ir-type-defn)))

(defmethod ir-reference-type? ((ir-type t))
  nil)

(defun make-array-release-header (release-name type-name-root c-param-decl-string)
  (format nil "extern void ~a(~a_t x~a);" release-name type-name-root c-param-decl-string))
;;The release operation reduces the reference count of a reference by one, and frees the object
;;(releasing any connected objects) if the count falls to 0.  
(defun make-array-release-defn (release-name type-name-root size ir-range c-range-root
					      c-param-arg-string c-param-decl-string)
  (if (ir-reference-type? ir-range)
      (format nil "void ~a(~a_t x~a){~%~8Tx->count--;~%~8Tif (x->count <= 0){~%~16Tfor (int i = 0; i < ~a; i++){release_~a(x->elems[i]~{, ~a~});};~%~8T//printf(\"\\nFreeing\\n\");~%~8Tsafe_free(x);}~%}"
	      release-name type-name-root c-param-decl-string size
	      c-range-root 
	      (loop for ir-formal in (free-ir-formals ir-range)
		    collect (ir-type-id ir-formal)))
    ;if there are nested references, these need to be released before freeing x,
    ;;otherwise, just free x.
    (if (ir-formal-typename? ir-range)
	(format nil "void ~a(~a_t x~a){~%~8Tx->count--;~%~8Tif (x->count <= 0){~%~16Tif (~a->tag == 'p'){~
  ~%~24T for (int i = 0; i < ~a; i++){~a->release_ptr(x->elems[i], ~a)};~%~16T};~%~16Tsafe_free(x);}~%}"
		release-name type-name-root
		c-param-decl-string
		(ir-type-id ir-range)
		size
		(ir-type-id ir-range)
		(ir-type-id ir-range))
      (format nil "void ~a(~a_t x){~%~8Tx->count--;~%~8T if (x->count <= 0){safe_free(x);}~%}"
	      release-name type-name-root))))

(defun make-array-copy-header (copy-name type-name-root elemtype c-param-decl-string)
  (format nil "extern ~a_t ~a(~a_t x~a);" type-name-root copy-name type-name-root c-param-decl-string))

(defun make-array-copy-defn (copy-name type-name-root size elemtype c-range-root c-param-decl-string)
  (let ((copy-instr (if (ir-reference-type? elemtype)
			(format nil "for (uint32_t i = 0; i < ~a; i++){tmp->elems[i] = x->elems[i];~%~
                                     ~16Tx->elems[i]->count++;}"
				size)
		      (if (ir-formal-typename? elemtype)
			  (format nil "for (uint32_t i = 0; i < ~a; i++){~a;~%~16Tif (~a->tag == 'p') x->elems[i]->count++;}"
			      size (make-c-assignment "tmp->elems[i]" c-range-root
						      "x->elems[i]" c-range-root)
			      (ir-type-id elemtype))
			(format nil "for (uint32_t i = 0; i < ~a; i++){~a;}"
				size (make-c-assignment "tmp->elems[i]" c-range-root
							"x->elems[i]" c-range-root))))))
  (format nil "~a_t ~a(~a_t x~a){~%~8T~a_t tmp = new_~a();~%~8T~
               tmp->count = 1;~
	       ~%~8T~a;~%~8T return tmp;}"   
	  type-name-root copy-name 
	  type-name-root c-param-decl-string type-name-root type-name-root copy-instr)))

(defun make-array-equal-info (type-name-root size elemtype c-range-root c-param-decl-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (make-array-equal-header equal-name type-name-root c-param-decl-string))
	 (equal-defn (make-array-equal-defn equal-name type-name-root size elemtype c-range-root c-param-decl-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-array-equal-header (equal-name type-name-root c-param-decl-string)
  (format nil "extern bool_t ~a(~a_t x, ~a_t y~a);" 
	  equal-name type-name-root type-name-root c-param-decl-string))

(defun make-array-equal-defn (equal-name type-name-root size elemtype c-range-root c-param-decl-string)
  (let ((equal-instr (if (ir-reference-type? elemtype)
			(format nil "while (i < ~a && tmp){tmp = !equal_~a(x->elems[i], y->elems[i]);}"
				size c-range-root)
		       (if (ir-formal-typename? elemtype)
			(format nil "while (i < ~a && tmp){tmp = !~a->eq_ptr(x->elems[i], y->elems[i], ~a);}"
				size c-range-root c-range-root)   
		      (format nil "while (i < ~a && tmp){tmp = (x->elems[i] != y->elems[i]); i++;}"
			      size)))))
    (if (ir-formal-typename? elemtype)
	(format nil "bool_t ~a(~a_t x, ~a_t y~a){~%~8Tbool_t tmp = true;~%~8Tuint32_t i = 0;~
	       ~%~8T~a;~%~8Treturn tmp;}"   
		equal-name type-name-root type-name-root
		c-param-decl-string
		equal-instr)
      (format nil "bool_t ~a(~a_t x, ~a_t y){~%~8Tbool_t tmp = true;~%~8Tuint32_t i = 0;~
	       ~%~8T~a;~%~8Treturn tmp;}"   
	      equal-name type-name-root type-name-root equal-instr))))

(defun make-array-update-header (update-name type-name-root ir-range c-param-decl-string)
  (let* ((range-type-info (get-c-type-info ir-range))
	 (range-type-name (if range-type-info (tname range-type-info) ir-range)))
    (format nil "extern ~a_t ~a(~a_t x, uint32_t i, ~a_t v~a);" type-name-root update-name  type-name-root range-type-name c-param-decl-string)))

(defun make-array-update-defn (update-name type-name-root ir-range c-param-arg-string c-param-decl-string)
  (let* ((range-type-info (get-c-type-info ir-range))
	 (range-type-name (if range-type-info (tname range-type-info)  ir-range)))
    (if (ir-reference-type? ir-range)
	(format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T ~a_t y;~%~8T if (x->count == 1){y = x;}~%~16T else {y = copy_~a(x~a);};~%~8T~
                     ~a_t * yelems = y->elems;~%~8T~
                     if (yelems[i] != NULL){release_~a(yelems[i]~{, ~a~});};~%~8T yelems[i] = v; if (v != NULL){v->count++;}~%~8T return y;}"
		type-name-root update-name type-name-root range-type-name
		c-param-decl-string
		type-name-root type-name-root c-param-arg-string
		range-type-name range-type-name
		(loop for ir-formal in (free-ir-formals ir-range)
		      collect (ir-type-id ir-formal)))
      (if (ir-formal-typename? ir-range)
	(format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T ~a_t y;~%~8T if (x->count == 1){y = x;}~%~16T else {y = copy_~a(x~a);};~%~8T~
                     ~a_t * yelems = y->elems;~%~8T~
                     if (~a->tag == 'p' && yelems[i] != NULL){~a->release_ptr(yelems[i]~{, ~a~});};~%~8T yelems[i] = v; if (~a->tag == 'p' && v != NULL){v->count++;}~%~8T return y;}"
		type-name-root update-name type-name-root range-type-name
		c-param-decl-string
		type-name-root type-name-root c-param-arg-string
		range-type-name range-type-name range-type-name
		(loop for ir-formal in (free-ir-formals ir-range)
		      collect (ir-type-id ir-formal)) range-type-name)  
    (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v){~%~8T~a_t y; ~%~8T if (x->count == 1){y = x;}~%~10T else {y = copy_~a(x);}~%~8T~
                    ~a;~%~8T~
                    return y;}"
	    type-name-root update-name type-name-root range-type-name type-name-root type-name-root
	    (make-c-assignment "y->elems[i]" range-type-name "v" range-type-name))))))


(defmethod add-c-type-definition ((ir2c-type ir-recordtype) &optional tname)
  (with-slots (ir-field-types) ir2c-type
	      (let* ((c-type-info (get-c-type-info ir2c-type))
		     (c-type-name (when c-type-info (tname c-type-info))))
		(or (and (or (null tname)(eq c-type-name tname)) c-type-name)
		    (let* ((c-field-types (loop for ft in ir-field-types
					       collect (add-c-type-definition (ir-ftype ft))))
			   (type-name-root (or tname (format nil "~a_record_~a"
							     *theory-id*
							     (length *c-type-info-table*))))
			   (c-field-decls (loop for cft in c-field-types
						as ft in ir-field-types
						collect (format nil "~a_t ~a" cft (ir-id ft))))
			   (type-defn (format nil "struct ~a_s {~%~8Tuint32_t count; ~{~%~8T~a;~}};~%typedef struct ~a_s * ~a_t;"
					      type-name-root c-field-decls type-name-root type-name-root)));(break "add-c-type-definition")
		      (let* ((new-info (make-record-new-info type-name-root))
			     (release-info (make-record-release-info type-name-root ir-field-types c-field-types))
			     (copy-info (make-record-copy-info type-name-root ir-field-types c-field-types))
			     (equal-info (make-record-equal-info type-name-root ir-field-types c-field-types))
			     (update-info (loop for cft in c-field-types
						as ft in ir-field-types
						collect (make-record-field-update-info type-name-root ft cft))))
			(push-type-info-to-decl
			 (mk-c-type-info ir2c-type type-name-root type-defn nil new-info release-info copy-info equal-info update-info)
			 *pvs2c-current-decl*)
			    type-name-root))))))

(defmethod add-c-type-definition ((ir2c-type ir-adt-recordtype) &optional tname)
 ; (break "add-c-type-defn adt")
  (with-slots (ir-field-types ir-constructors) ir2c-type
	      (let* ((c-type-info (get-c-type-info ir2c-type))
		     (c-type-name (when c-type-info (tname c-type-info))))
		(or c-type-name
		    (let* ((c-field-types (loop for ft in ir-field-types
					       collect (add-c-type-definition (ir-ftype ft))))
			   (type-name-root (or tname (format nil "record_~a" (length *c-type-info-table*))))
			   (c-field-decls (loop for cft in c-field-types
						as ft in ir-field-types
						collect (format nil "~a_t ~a" cft (ir-id ft))))
			   (type-defn (format nil "struct ~a_s {~%~8T uint32_t count; ~{~%~8T~a;~}};~%typedef struct ~a_s * ~a_t;"
					      type-name-root c-field-decls type-name-root type-name-root)));(break "add-c-type-definition")
		      (let* ((new-info (make-record-new-info type-name-root))
			     (release-info (make-adt-record-release-info type-name-root ir-field-types c-field-types ir-constructors))
			     (copy-info (make-record-copy-info type-name-root ir-field-types c-field-types))
			     (equal-info (make-adt-record-equal-info type-name-root ir-field-types c-field-types ir-constructors))
			     (update-info (loop for cft in c-field-types
						as ft in ir-field-types
						collect (make-record-field-update-info type-name-root ft cft))))
			    (push-type-info-to-decl (mk-c-type-info ir2c-type type-name-root type-defn nil new-info release-info copy-info equal-info update-info)
						    *pvs2c-current-decl*)
			    type-name-root))))))



(defun make-record-new-info (type-name-root)
    (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	   (new-header (format nil "extern ~a_t new_~a(void);" type-name-root type-name-root))
	   (new-defn   (format nil "~a_t new_~a(void){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s));~%~8Ttmp->count = 1;~%~8Treturn tmp;}"
			       type-name-root type-name-root type-name-root type-name-root type-name-root)))
      (mk-c-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-adt-record-release-info (type-name-root ir-field-types c-field-types constructors)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (format nil "extern void release_~a(~a_t x);" type-name-root type-name-root))
	 (release-defn (let ((release-fields (loop for constructor in constructors
						   as index from 0
						   when (cdr constructor)
						   collect (format nil "case ~a:  release_~a((~a_t) x); break"
								   index
								   (car constructor)
								   (car constructor)))))
						   
			 (format nil "void release_~a(~a_t x){~%switch (x->~a_index) {~{~%~a;~}~%}}"
				 type-name-root type-name-root type-name-root release-fields))))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-record-release-info (type-name-root ir-field-types c-field-types)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (format nil "extern void release_~a(~a_t x);" type-name-root type-name-root))
	 (release-defn (let ((release-fields (loop for ft in ir-field-types
						   as cft in c-field-types
						   when (ir-reference-type? (ir-ftype ft))
						   collect (format nil "release_~a(x->~a)" cft (ir-id ft)))))
						   
			 (format nil "void release_~a(~a_t x){~%~8Tx->count--;~%~8Tif (x->count <= 0){~{~%~8T~8T~a;~}~%~8T//printf(\"\\nFreeing\\n\");~%~8Tsafe_free(x);}}"
				 type-name-root type-name-root release-fields))))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-record-copy-info (type-name-root ir-field-types c-field-types)
  (let* ((copy-name (intern (format nil "copy_~a" type-name-root)))
	 (copy-header (format nil "extern ~a_t ~a(~a_t x);" type-name-root copy-name type-name-root))
	 (copy-defn (make-record-copy-defn type-name-root ir-field-types c-field-types)))
    (mk-c-defn-info copy-name copy-header copy-defn (list type-name-root) type-name-root)))

(defun make-record-copy-defn (type-name-root ir-field-types c-field-types)
  (let ((copy-field-instrs
	 (loop for ft in ir-field-types
	       as cft in c-field-types
	       collect (if (ir-reference-type? (ir-ftype ft))
			   (format nil "~%~8Ty->~a = x->~a;~%~8Tif (y->~a != NULL){y->~a->count++;}"
					;type-name-root
				   (ir-id ft)
					;type-name-root
				   (ir-id ft)
					;type-name-root
				   (ir-id ft)
					;type-name-root
				   (ir-id ft))
			 (let ((lhs (format nil "y->~a" (ir-id ft)))
			       (rhs (format nil "x->~a" (ir-id ft)))) 
			   (format nil "~%~8T~a"
				   (make-c-assignment lhs cft rhs cft)))))))
    (format nil "~a_t copy_~a(~a_t x){~%~8T~a_t y = new_~a();~{~a;~}~%~8Ty->count = 1;~%~8Treturn y;}"
	    type-name-root type-name-root type-name-root type-name-root type-name-root copy-field-instrs)))

(defun make-adt-record-equal-info (type-name-root ir-field-types c-field-types constructors)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (format nil "extern bool_t ~a(~a_t x, ~a_t y);" equal-name
			      type-name-root type-name-root))
	 (equal-defn (make-adt-record-equal-defn type-name-root ir-field-types
						 c-field-types constructors)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-adt-record-equal-defn (type-name-root ir-field-types c-field-types constructors)
  (let ((equal-constructor-instrs (loop for constructor in constructors
					as index from 0
					when (cdr constructor)
					collect
					(format nil "case ~a: tmp = tmp && equal_~a((~a_t) x, (~a_t) y); break"
						index (car constructor)(car constructor)
						(car constructor)))))
    (format nil "bool_t equal_~a(~a_t x, ~a_t y){~%~8Tbool_t tmp = x->~a_index == y->~a_index;~%~8Tswitch  (x->~a_index) {~{~%~16T~a;~}~%~8T}~%~8Treturn tmp;~%}" type-name-root type-name-root type-name-root
	    type-name-root type-name-root type-name-root equal-constructor-instrs)))

(defun make-record-equal-info (type-name-root ir-field-types c-field-types)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (format nil "extern bool_t ~a(~a_t x, ~a_t y);" equal-name
			      type-name-root type-name-root))
	 (equal-defn (make-record-equal-defn type-name-root ir-field-types c-field-types)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-record-equal-defn (type-name-root ir-field-types c-field-types)
  (let ((equal-field-instrs
	 (loop for ft in ir-field-types
	       as cft in c-field-types
	       collect (if (ir-reference-type? (ir-ftype ft))
			   (format nil "~%~8Ttmp = tmp && equal_~a(x->~a, y->~a)"
				   cft
				   (ir-id ft)
				   (ir-id ft))
			 (format nil "~%~8Ttmp = tmp && x->~a == y->~a"
				    (ir-id ft)  (ir-id ft))))))
    (format nil "bool_t equal_~a(~a_t x, ~a_t y){~%~8Tbool_t tmp = true;~{~a;~}~%~8Treturn tmp;}"
	     type-name-root type-name-root type-name-root  equal-field-instrs)))


(defun make-record-field-update-info (type-name-root ir-field-type c-field-type)
  (let* ((update-name (intern (format nil "update_~a_~a" type-name-root (ir-id ir-field-type))))
	 (update-header (format nil "extern ~a_t ~a(~a_t x, ~a_t v);" type-name-root update-name type-name-root c-field-type))
	 (update-defn (make-record-field-update-defn update-name type-name-root ir-field-type c-field-type)))
    (mk-c-defn-info update-name update-header update-defn (list type-name-root c-field-type) type-name-root)))

(defun make-record-field-update-defn (update-name type-name-root ir-field-type c-field-type)
  (let ((fname (ir-id ir-field-type))
	(ftype (ir-ftype ir-field-type)))
    (if (ir-reference-type? ftype)
	(format nil "~a_t ~a(~a_t x, ~a_t v){~%~8T~a_t y;~%~8Tif (x->count == 1){y = x; if (x->~a != NULL){release_~a(x->~a);};}~%~8T~
                     else {y = copy_~a(x); y->~a->count--;};~%~8T~
                     ~a;~%~8Tif (v != NULL){v->count++;};~%~8Treturn y;}"
		type-name-root update-name type-name-root c-field-type type-name-root fname c-field-type
		fname type-name-root fname (make-c-assignment (format nil "y->~a"fname)
							      c-field-type
							       "v" c-field-type))
      (format nil "~a_t ~a(~a_t x, ~a_t v){~%~8T~a_t y;~%~8Tif (x->count == 1){y = x;}~%~8T~
                     else {y = copy_~a(x);};~%~
                     ~8T~a;~%~8Treturn y;}"
		type-name-root update-name type-name-root c-field-type type-name-root 
		type-name-root (make-c-assignment (format nil "y->~a"fname)
							      c-field-type
							       "v" c-field-type)))))

;;equality method for ir-types
(defun ir2c-tequal (texpr1 texpr2)
  ;(format t "~%ir2c-tequal")
  ;(format t "~%texpr1: ~a" (print-ir texpr1))
   ; (format t "~%texpr2: ~a" (print-ir texpr2))
  (ir2c-tequal* texpr1 texpr2))

(defmethod ir2c-tequal* ((texpr1 ir-recordtype)(texpr2 ir-recordtype))
  (with-slots ((ir-ftypes1 ir-field-types)) texpr1
	      (with-slots ((ir-ftypes2 ir-field-types)) texpr2
			  (ir2c-tequal* ir-ftypes1 ir-ftypes2))))

(defmethod ir2c-tequal* ((texpr1 list)(texpr2 list))
  (cond ((consp texpr1)
	 (and (consp texpr2)
	      (ir2c-tequal* (car texpr1)(car texpr2))
	      (ir2c-tequal* (cdr texpr1)(cdr texpr2))))
	(t (not (consp texpr2)))))

(defmethod ir2c-tequal* ((texpr1 ir-fieldtype)(texpr2 ir-fieldtype))
  (with-slots ((ir-id1 ir-id)(ir-ftype1 ir-ftype)) texpr1
	      (with-slots ((ir-id2 ir-id)(ir-ftype2 ir-ftype)) texpr2
			  (and (eq ir-id1 ir-id2)
			       (ir2c-tequal* ir-ftype1 ir-ftype2)))))

(defmethod ir2c-tequal* ((texpr1 ir-funtype)(texpr2 ir-funtype))
  (with-slots ((ir-dom1 ir-domain)(ir-ran1 ir-range)) texpr1
	      (with-slots ((ir-dom2 ir-domain)(ir-ran2 ir-range)) texpr2
			  (and (ir2c-tequal* ir-dom1 ir-dom2)
			       (ir2c-tequal* ir-ran1 ir-ran2)))))

(defmethod ir2c-tequal* ((texpr1 ir-arraytype)(texpr2 ir-arraytype))
  (with-slots ((size1 size)(elemtype1 elemtype)) texpr1
	      (with-slots ((size2 size)(elemtype2 elemtype)) texpr2
			  (and (eql size1 size2)
			       (ir2c-tequal* elemtype1 elemtype2)))))

(defmethod ir2c-tequal* ((texpr1 ir-subrange )(texpr2 ir-subrange))
  (equal (ir2c-type texpr1)(ir2c-type texpr2)))

(defmethod ir2c-tequal* ((texpr1 ir-typename)(texpr2 ir-typename))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)) texpr1
	      (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)) texpr2
			  ;(and (eq id1 id2))
			  (ir2c-tequal* tdef1 tdef2))))

(defmethod ir2c-tequal* ((texpr1 ir-typename)(texpr2 t))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)) texpr1
	      (ir2c-tequal* tdef1 texpr2)))

(defmethod ir2c-tequal* ((texpr1 t)(texpr2 ir-typename))
  (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)) texpr2
	      (ir2c-tequal* texpr1 tdef2)))



(defmethod ir2c-tequal* ((texpr1 t)(texpr2 t))
  (eq texpr1 texpr2));;Since the base case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-type (texpr1 texpr2 lhs rhs)
  (let ((*var-counter* nil))
    (newcounter *var-counter*)
    (cons (format nil "//copying to ~a from ~a"
		  (add-c-type-definition texpr1)
		  (add-c-type-definition texpr2))
	  (copy-type* texpr1 texpr2 lhs rhs))))

(defmethod copy-type* ((ltype ir-recordtype)(rtype ir-recordtype) lhs rhs)
  (with-slots ((ift1 ir-field-types)) ltype
	      (with-slots ((ift2 ir-field-types)) rtype
			  (loop for ft1 in ift1
				as ft2 in ift2
				append (copy-type* ft1 ft2 lhs rhs)))))

(defmethod copy-type* ((texpr1 ir-fieldtype)(texpr2 ir-fieldtype) lhs rhs)
  (with-slots ((ft1 ir-ftype)(id1 ir-id)) texpr1
	      (with-slots ((ft2 ir-ftype)(id2 ir-id)) texpr2
			  (let ((newlhs (format nil "~a->~a" lhs id1))
				(newrhs (format nil "~a->~a" rhs id2)))
			  (copy-type* ft1 ft2 newlhs newrhs)))))

(defmethod copy-type* ((texpr1 ir-arraytype)(texpr2 ir-arraytype) lhs rhs)
  (with-slots ((size1 size)(et1 elemtype)) texpr1
	      (with-slots ((size2 size)(et2 elemtype)) texpr2
			  (let ((index (new-indvar)))
			    (list (mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
							index index size1 index)
					(copy-type* et1 et2
						    (format nil "~a->elems[~a]" lhs index)
						    (format nil "~a->elems[~a]" rhs index))))))))

(defmethod copy-type* ((texpr1 ir-typename)(texpr2 t) lhs rhs)
  (with-slots ((id1 ir-type-id)(typedef1 ir-type-defn)) texpr1
	      (cons (format nil "~a = new_~a()" lhs id1)
		    (copy-type* typedef1 texpr2 lhs rhs))))

(defmethod copy-type* ((texpr1 t)(texpr2 ir-typename) lhs rhs)
  (with-slots ((id2 ir-type-id)(typedef2 ir-type-defn)) texpr2
	      (copy-type* texpr1 typedef2 lhs rhs)))

(defmethod copy-type* ((texpr1 t)(texpr2 t) lhs rhs)
  (list (mk-c-assignment lhs texpr1 rhs texpr2)))



			  
	       
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *pvs2c-current-decl* nil)
(defun pvs2c-decl (decl)
  (let ((saved-c-type-info-table *c-type-info-table*)
	(*pvs2c-current-decl* decl);;used to save auxilliary type defns in c-type-info-table
	(*current-context* (decl-context decl)))
    (pvs2c-decl* decl)))

    ;;NSH(8/6/2016): Closures are handled now. 
    ;; (handler-case
    ;;  (pvs2c-decl* decl)
    ;;  (pvs2c-error (condition) (format t "~%closures not handled")
    ;; 		  (setq *c-type-info-table* saved-c-type-info-table)))))
  
(defmethod pvs2c-decl* ((decl type-eq-decl))
  (let ((typename (pvs2ir-decl decl)))
    ;(break "type-eq-decl")
    (when  (and (ir-typename? typename)
		(ir-type-value decl));some type definitions translate to no-ops
      (add-c-type-definition (ir2c-type (ir-type-defn typename))(ir-type-id typename)))))

(defmethod pvs2c-decl* ((decl type-decl)) ;;has to be an adt-type-decl
  (let ((typename (pvs2ir-decl* decl)))
    (if (adt (type-value decl))
	(add-c-type-definition (ir2c-type (ir-type-defn typename))(ir-type-id typename))
      (let* ((ir-type-id (ir-type-id typename))
	     (c-type-info (mk-simple-c-type-info nil ir-type-id
						 (format nil "//uninterpreted type~%typedef void * ~a_t" ir-type-id) nil)))
	(push-type-info-to-decl c-type-info decl)
	ir-type-id))))

(defmethod pvs2c-decl* ((decl formal-type-decl))
   (pvs2ir-decl decl))

(defmethod pvs2c-decl* ((decl formal-const-decl))
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl))))
    (ir2c-decl* ir decl)))
  

;;conversion for a definition
(defmethod pvs2c-decl* ((decl const-decl))
  ;; (unless (and (eq (id (module decl)) 'ordinals);this can be removed since we have closures
  ;; 	       (eq (id decl) 'size))
  (let ((*var-counter* nil))
    (newcounter *var-counter*)
    (pvs2ir-decl decl)
    (let ((ir (ir (eval-info decl))));(break "pvs2c-decl*/const-decl")
	  (ir2c-decl* ir decl))))

(defun c-args-string (ir-vars)
  (let* ((c-arg-types (loop for arg in ir-vars
			    collect (add-c-type-definition (ir2c-type (ir-vtype arg)))))
	 (c-args (loop for arg in ir-vars
		       as c-arg-type in c-arg-types
		       collect (format nil "~a_t ~a"
				       c-arg-type
				       (ir-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void"))))
    c-args-string))

(defun make-c-defn-info (ir pvs-return-type)
  (with-slots
   (ir-function-name ir-return-type ir-args ir-defn) ir
;   (when (eq (id decl) 'coef)(break "coef"))
   ;(format t "~%ir-defn~% =~a" (print-ir ir-defn))
   (if (null ir-defn)
       (let* ((ir-function-name (ir-fname ir-function-name))
	      (ir-result-type ir-return-type) ;(pvs2ir-type (type decl))
	      (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	      (dummy (when (null c-result-type) (break "make-c-defn-info")))
	      ;might need to adjust c-header when result type is gmp
	      (c-header (format nil "extern ~a_t ~a(~a)" (mppointer-type c-result-type) ir-function-name
				(c-args-string ir-args))))
	 (format t "~%No definition for ~a" ir-function-name)
	 (mk-c-defn-info ir-function-name (format nil "~a;" c-header) nil nil
			c-result-type))
     (let* ((ir-function-name (ir-fname ir-function-name))
	    (pre-ir (if ir-args
			(mk-ir-lambda ir-args ir-return-type ir-defn)
		      ir-defn))
	     (post-ir (preprocess-ir pre-ir))
	     (ir-args (when (ir-lambda? post-ir)
			(ir-vartypes post-ir)))
;	     (ir-decl-type (add-c-type-definition (ir2c-type (pvs2ir-type (declared-type decl)))))
	     (ir-result-type (if (ir-lambda? post-ir)
				 (ir-rangetype post-ir) ;(pvs2ir-type (range (find-supertype (type decl))))
			       (pvs2ir-type pvs-return-type)))
	     (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	     (c-arg-types (loop for arg in ir-args
				collect (add-c-type-definition (ir2c-type (ir-vtype arg)))))
	     (*mpvar-parameters*  (loop for arg in ir-args
					as c-arg-type in c-arg-types
					when (mpnumber-type? c-arg-type)
					collect arg))
	     (c-args (loop for arg in ir-args
			   as c-arg-type in c-arg-types
			   collect (format nil "~a_t ~a"
					   c-arg-type
					   (ir-name arg))))
	     (c-args-string (if (consp c-args)
				(format nil "~{~a~^, ~}" c-args)
			      (format nil "void")))
	     (c-header (format nil "extern ~a_t ~a(~a)" (mppointer-type c-result-type) ir-function-name c-args-string))
	     ;; (case c-result-type
	     ;; 		 ((mpz mpq)
	     ;; 		  (if (consp c-args)
	     ;; 		      (format nil "extern void ~a(~a_t ~a, ~a)"
	     ;; 			      ir-function-name c-result-type 'result c-args-string)
	     ;; 		    (format nil "extern void ~a(~a_t ~a)"
	     ;; 			      ir-function-name c-result-type 'result)))
	     ;; 		 (t (format nil "extern ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	     (ir-body (if (ir-lambda? post-ir)
			  (ir-body post-ir)
			post-ir))
	     (c-body (print2c (ir2c ir-body ir-result-type)))
	     (c-result-decl (if (mpnumber-type? c-result-type)
				(let ((mptype (mppointer-type c-result-type)))
				  (format nil "~a_t result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
					  mptype; c-result-type c-result-type
					  ))
			      (format nil "~a_t result;" c-result-type)))
	     (c-defn  (format nil "~a{~%~8T~a~%~a~%~8Treturn result;~%}"
				 c-header 
				 c-result-decl
				 c-body))
	     ;; (case c-result-type
	     ;; 	       ((mpq mpz)
	     ;; 		(format nil "~a{~%~a~%}"
	     ;; 		     c-header 
	     ;; 		     c-body))
	     ;; 	       (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	     ;; 		     c-header 
	     ;; 		     (mppointer-type c-result-type)
	     ;; 		     c-body)))
	     )
       (format t "~%Function ~a"  ir-function-name)
       (format t "~%MPvars ~{~a, ~}" (print-ir *mpvar-parameters*))
	(format t "~%Before  preprocessing = ~%~a" (print-ir pre-ir))	   
	(format t "~%After preprocessing = ~%~a" (print-ir post-ir))
	(format t "~%Generates C definition = ~%~a" c-defn)
	(mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn c-arg-types
			c-result-type)))))

(defun make-c-closure-defn-info (ir-lambda-expr ir-function-name)
  (let* ((ir-args (ir-vartypes ir-lambda-expr))
	 (ir-result-type (ir-rangetype ir-lambda-expr)) ;(pvs2ir-type (range (find-supertype (type decl))))
	 (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	 (fvars (pvs2ir-freevars* ir-lambda-expr))
	 (c-arg-types (loop for arg in (append ir-args fvars)
		       collect (add-c-type-definition (ir2c-type (ir-vtype arg)))))
	 (c-args (loop for arg in (append ir-args fvars)
		       collect (format nil "~a_t ~a"
				       (add-c-type-definition (ir2c-type (ir-vtype arg)))
				       (ir-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void")))
	 (c-header (format nil "extern ~a_t ~a(~a)" (mppointer-type c-result-type)
			   ir-function-name c-args-string))
	 ;; (case c-result-type
	 ;; 	     ((mpz mpq)
	 ;; 	      (if (consp c-args)
	 ;; 		  (format nil "extern void ~a(~a_t ~a, ~a)"
	 ;; 		      ir-function-name c-result-type 'result c-args-string)
	 ;; 		  (format nil "extern void ~a(~a_t ~a)"
	 ;; 		      ir-function-name c-result-type 'result)))
	 ;; 	     (t (format nil "extern ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	 (c-defn-arg-types c-arg-types)
	 ;; (case c-result-type
	 ;; 		     ((mpz mpq)(cons c-result-type c-arg-types))
	 ;; 		     (t c-arg-types))
	 (c-defn-result-type (mppointer-type c-result-type))
	     ;; (case c-result-type
	     ;; 		       ((mpz mpq) 'void)
	     ;; 		       (t c-result-type))
	 (ir-body (ir-body ir-lambda-expr))
	 (*mpvar-parameters*  (loop for arg in ir-args
				    as c-arg-type in c-arg-types 
				    when (mpnumber-type? c-arg-type)
				    collect arg))
	 (c-body (print2c (ir2c ir-body ir-result-type)))
	 (c-result-decl (case c-result-type
			  ((mpq mpz)
			   (format nil "~a_t result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
				   (mppointer-type c-result-type); c-result-type c-result-type
				   ))
			  (t (format nil "~a_t result;" c-result-type))))
	 (c-defn  (format nil "~a{~%~8T~a~%~a~%~8Treturn result;~%}"
			  c-header 
			  c-result-decl
			  c-body))
	 ;; (case c-result-type
	 ;; 	   ((mpq mpz)
	 ;; 	    (format nil "~a{~%~a~%}" c-header c-body))
	 ;; 	   (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	 ;; 		      c-header
	 ;; 		      c-result-type
	 ;; 		      c-body)))
	 )
    (format t "~%Closure After preprocessing = ~%~a" (print-ir ir-lambda-expr))
    (format t "~%Generates C definition = ~%~a" c-defn)
    (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn
		    c-defn-arg-types c-defn-result-type)))


(defmethod ir2c-decl* ((ir ir-accessor-defn) decl)
  (let ((cdefn (make-c-defn-info ir (type decl)))
	(udefn (make-c-defn-info (ir-update-defn ir) nil)))
    (setf (cdefn (eval-info decl)) cdefn
	  (update-cdefn (eval-info decl)) udefn)
    (op-name cdefn)))

(defmethod ir2c-decl* ((ir ir-defn) decl)
  (let ((cdefn (make-c-defn-info ir (type decl))));(break "ir2c-decl*")
    (setf (cdefn (eval-info decl)) cdefn)
    (or (and cdefn (op-name cdefn))
	(ir-uname (ir-function-name ir)))))

(defun ir-uname (fname)
  (format nil "undef_~a" fname))

;; (defmethod ir2c-decl* ((ir ir-formal-const-defn) decl)
;;   (with-slots (ir-defn) ir
;; 	      (
;; 	      (add-c-type-declaration (ir-vtype ir-defn) decl)))

(defmethod ir2c-decl* ((ir ir-constructor-defn) decl)
  (with-slots (ir-constructor-type) ir
	      (with-slots (ir-type-id ir-type-defn) ir-constructor-type
			  (add-c-type-definition (ir2c-type ir-type-defn) ir-type-id))
	      (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printing out the C code in a block-structured manner
;;the translated code is a list of instructions, where each
;;instruction is either a regular C instruction or an if-instr

(defvar *c-scope-string* "~8T")
(defun print2c (c-instrs)
  (cond ((consp c-instrs)
	 (cond ((if-instr? (car c-instrs))
		(with-slots (if-cond then-instr else-instr) (car c-instrs)
			    (let* ((then-part
				    (let ((*c-scope-string*
					   (format nil "~a~a" "~~8T" *c-scope-string*)))
				      (print2c then-instr)))
				   (else-part
				    (let ((*c-scope-string*
					   (format nil "~a~a" "~~8T" *c-scope-string*)))
				      (print2c else-instr)))
				
				   (if-string (format nil "~aif (~a){~a~%~a} else {~%~a~%~a}"
						      *c-scope-string*
						      if-cond
						      *c-scope-string*
						      then-part
						      *c-scope-string*
						      else-part)))
			      (format nil "~a;~%~a" (format nil if-string)
				      (print2c (cdr c-instrs))))))
	       ((for-instr? (car c-instrs))
		(let* ((for-body (for-body (car c-instrs)))
		       (for-index (for-index (car c-instrs)))
		       (c-for-body (let ((*c-scope-string*
					  (format nil "~a~a" "~~8T" *c-scope-string*)))
				     (print2c for-body))))
		  (format nil (format nil "~afor (~a){~%~a~a};~%~a"
				      *c-scope-string* for-index
				      c-for-body *c-scope-string*
				      (print2c (cdr c-instrs))))))
	       (t (format nil (format nil "~a~a;~%~a" *c-scope-string* (car c-instrs)
				      (print2c (cdr c-instrs)))))))
	(t (format nil ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;print the header file from the information in *c-type-info-table*
;;If the type has a PVS definition, the type-info is saved with this declaration.
;;Otherwise, the type information is generated for the type expression in the global.

(defun print-header-file (theory)
  (let* ((theory-id (id theory))
	 (file-string (format nil "~a_c.h" theory-id))
	 (preceding-theories (pvs2c-preceding-theories theory))
	 (preceding-theories-without-formals
	  (loop for thy in preceding-theories
		;when (null (formals (get-theory thy)))
		collect thy)))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
		    (format output "//Code generated using pvs2ir")
		    (format output "~%#ifndef _~a_h ~%#define _~a_h" theory-id theory-id)
		    (format output "~%~%#include <stdio.h>")
		    (format output "~%~%#include <stdlib.h>")
		    (format output "~%~%#include <inttypes.h>")
		    (format output "~%~%#include <stdbool.h>")
		    (format output "~%~%#include <string.h>")		    		    
		    (format output "~%~%#include <gmp.h>")
		    (format output "~%~%#include \"pvslib.h\"")
		    (loop for thy in  preceding-theories-without-formals
			  when (not (eq thy theory-id))
			  do (format output "~%~%#include \"~a_c.h\"" thy))
		    (format output "~%~%//cc -Wall -o ~a" theory-id )
		    (format output " pvslib.c ")
		    (loop for thy in preceding-theories-without-formals
			  do (format output " ~a_c.c" thy))
		    (format output " -lgmp ")
		    ;; (loop for formal in (formals theory)
		    ;;  	  when (formal-type-decl? decl)
		    ;;  	  do (format output "~%~%typedef void * ~a_t;" (ir-type-id (ir-type-name (ir-type-value decl)))))
		    (print-type-info-headers-to-file output *c-type-info-table*)
		    (loop for decl in (theory theory)
		     	  when (and (const-decl? decl)(eval-info decl)(cdefn (eval-info decl)))
			  do (print-header-decl decl output))
		    (format output "~%#endif")
		    (format t "~%Wrote ~a" file-string)
		    (id theory))))

(defun print-header-decl (decl output)
  (let ((einfo (eval-info decl)))
    (when (accessor-eval-info? einfo)
      (format output "~%~%~a" (op-header (update-cdefn (eval-info decl)))))
    (format output "~%~%~a" (op-header (cdefn (eval-info decl))))))

(defun print-type-info-headers-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-headers-to-file output (cdr type-info-stack))
	 (print-type-defn-headers (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defn-headers ((type-info simple-c-type-info) output)
  (format output "~%~%~a~%~%"
	  (tdefn type-info)))

(defmethod print-type-defn-headers ((type-info closure-c-type-info) output)
  (with-slots (tdecl tdefn ftable-type-defn release-info hash-entry-type-defn hash-type-defn
		     copy-info lookup-info dupdate-info update-info)
	      type-info
	      (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
		      tdecl ftable-type-defn hash-entry-type-defn hash-type-defn tdefn)
	      (format output "~a~%~%" (op-header release-info))
	      (format output "~a~%~%" (op-header copy-info));copy is required, but functions below are optional
	      (when lookup-info (format output "~a~%~%" (op-header lookup-info)))
	      (when dupdate-info (format output "~a~%~%" (op-header dupdate-info)))
	      (when update-info (format output "~a~%~%" (op-header update-info)))
	      ))

(defmethod print-type-defn-headers ((type-info c-type-info) output)
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdefn type-info)
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  (op-header (equal-info type-info)))
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-header t-info))))

(defmethod print-type-defn-headers ((type-info c-closure-info) output)
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdecl type-info)
	  (tdefn type-info)
	  (op-header (fdefn type-info))
	  (op-header (mdefn type-info))
	  (op-header (hdefn type-info))	  
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  ))

(defun print-type-info-defns-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-defns-to-file output (cdr type-info-stack))
	 (print-type-defns (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defns ((type-info simple-c-type-info) output)
  nil); do nothing

(defmethod print-type-defns ((type-info closure-c-type-info) output)
  (with-slots (release-info copy-info lookup-info dupdate-info update-info) type-info
	      (format output "~%~%~a" (op-defn release-info))
	      (format output "~%~%~a" (op-defn copy-info))
	      (when lookup-info (format output "~%~%~a" (op-defn lookup-info)))
	      (when dupdate-info (format output "~%~%~a" (op-defn dupdate-info)))
	      (when update-info (format output "~%~%~a" (op-defn update-info)))))


(defmethod print-type-defns ((type-info c-type-info) output)
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (op-defn (copy-info type-info))
	  (op-defn (equal-info type-info)))
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-defn t-info))))

(defmethod print-type-defns ((type-info c-closure-info) output);(break "closure-info")
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a"
	  (op-defn (fdefn type-info))
	  (op-defn (mdefn type-info))	  
	  (op-defn (hdefn type-info))
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (op-defn (copy-info type-info))
))

(defun print-body-file (theory)
  (let* ((file-string (format nil "~a_c.c" (id theory))))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
		    (format output "//Code generated using pvs2ir2c")
		    (format output "~%#include \"~a_c.h\"" (id theory))
		    (print-type-info-defns-to-file output *c-type-info-table*)
		    (loop for decl in (theory  theory)
			  do (let ((einfo (and (const-decl? decl)(eval-info decl))))
			       (when (and einfo (cdefn einfo))
				 (let ((op-defn (op-defn (cdefn einfo))))
				   (when op-defn 
				     (when (accessor-eval-info? einfo)
				       (format output "~%~%~a" (op-defn (update-cdefn (eval-info decl)))))
				 (format output "~%~%~a" (op-defn (cdefn (eval-info decl)))))))))
		    (format t "~%Wrote ~a" file-string)
		    (id theory))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adt-operation? (decl)
  (typep decl
	 '(or adt-constructor-decl adt-recognizer-decl
	      adt-accessor-decl adt-def-decl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pvs2c-theory computes definitions for the entire theory while
;;stacking type definitions in *c-type-info-table*

(defun adt-type-decl? (decl)
  (and (type-decl? decl)
       (adt-type-name? (type-value decl))))

(defvar *theory-id* nil)
(defvar *theory-formals* nil)


(defparameter *primitive-prelude-theories*
  '(defined_types relations orders if_def naturalnumbers reals
 rationals integers equalities
 notequal numbers booleans number_fields))

(defvar *pvs2c-preceding-theories* nil)
(defvar *ir-theory-formals* nil)
(defvar *ir-theory-tbindings* nil)

;; (defun used-prelude-theory-names (theory &optional prelude-theory-names)
;;   (let ((th (get-theory theory)))
;;     (unless th
;;       (error "Theory ~a not found - may need to typecheck first" theory))
;;     (dolist (decl (all-decls th))
;;       (when (typep decl '(or type-decl const-decl))
;; 	(dolist (d (refers-to decl))
;; 	  (when (and (typep d '(or type-decl const-decl))
;; 		     (from-prelude? d))
;; 	    (unless (member (module d) prelude-theory-names :test #'same-id)
;; 	      (setq prelude-theory-names
;; 		    (used-prelude-theory-names (module d)
;; 					       (cons (id (module d)) ;(mk-modname (id (module d)))
;; 						     prelude-theory-names))))))))
;;     prelude-theory-names))

(defun pvs2c-preceding-theories (theory)
  (let ((*pvs2c-preceding-theories* nil))
    (pvs2c-preceding-theories* theory)
    *pvs2c-preceding-theories*))

(defun pvs2c-preceding-theories* (theory)
  (let ((theory-defn (get-theory theory)))
    (loop for thy in (used-prelude-theory-names theory)
	  when (not (memq (id thy) *primitive-prelude-theories*))
	  do (pushnew thy *pvs2c-preceding-theories*))
    (unless (eq (all-imported-theories theory-defn) 'unbound)
      (loop for thy in (all-imported-theories theory-defn)
	    do (pvs2c-preceding-theories* thy)))
    (pushnew (id theory-defn) *pvs2c-preceding-theories*)))

(defun pvs2c-theory (theory)
  (pvs2c-theories (pvs2c-preceding-theories theory)))

(defun pvs2c-theories (theories)
  (cond ((consp theories)
	 (pvs2c-theories (cdr theories));;process theories in reverse order
	 (pvs2c-theory* (car theories)))
	(t)))

(defun ir-theory-formal-type (formal)
    (if (formal-type-decl? formal)
	'type_actual
      (pvs2ir-type (type formal))))

(defun pvs2c-theory* (theory)
  (let* ((theory (or (get-theory theory) *current-theory*)))
    (let ((*theory-id* (simple-id (id theory)))
	  (*theory-formals* (formals-sans-usings theory))
	  (*ir-type-info-table* nil)
	  (*c-type-info-table* nil)
	  (*closure-info-table* nil)) ;;not currently used
	(let* ((formal-ids (loop for decl in *theory-formals* do (pvs2ir-decl decl)))
	       (*ir-theory-formals* (loop for formal in *theory-formals*
					  collect (if (formal-const-decl? formal)
							(ir-defn (ir (eval-info formal)))
						      (mk-ir-const-formal (ir-type-id (ir-type-name (ir-type-value formal))) 
									  *type-actual-ir-name*))))
	       (*ir-theory-tbindings* (pairlis *theory-formals* *ir-theory-formals*))
	       (*c-theory-formals*
		(ir2c-theory-formals *ir-theory-formals* *theory-formals*))
	       )
	  (if (eq *theory-id* '|modulo_arithmetic|)
	      (loop for decl in (theory theory)
		    when (eq (id decl) '|rem|)
		    do (pvs2c-decl decl))
	    (loop for decl in (theory theory)
		  when (or (const-decl? decl) ;(or (adt-operation? decl)(def-axiom decl))
			   (type-eq-decl? decl)(type-decl? decl)(adt-type-decl? decl))
		  do (pvs2c-decl decl)))
      (print-header-file theory)
      (print-body-file theory)))))
