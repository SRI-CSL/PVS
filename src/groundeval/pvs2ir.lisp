;;pvs2ir translates PVS expressions, definitions, and theories to an intermediate
;;representation (IR).  The IR consists of variables, tuples, function applications,
;;lambda-expressions, if-expressions, lets, and updates.

(in-package :pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;defvars
(defvar *free-ir-formals* nil)
(defvar *var-counter* nil)
;; (defvar *ir-type-def-hash* (make-hash-table :test #'eq))
(defvar *max-PVS-array-size* (expt 2 32)) 
(defvar *mpvar-parameters* nil)
(defvar *c-type-info-table* nil) ;;a list of c-type-info
(defvar *ir-type-info-table* nil) ;;a list of ir-typenames/doesn't seem to be used
(defvar *copy-operations* nil);;list of type to type copy operations
(defvar *c-primitive-type-attachments-hash* (make-hash-table :test #'eq))
(defvar *c-primitive-attachments-hash* (make-hash-table :test #'eq))
(defvar *c-scope-string* "~8T")
(defvar *theory-id* nil)
(defvar *theory-formals* nil)
(defvar *theory-type-formals* nil)
(defvar *context-path* nil)
(defvar *ir-theory-formals* nil)
(defvar *ir-theory-tbindings* nil)
(defvar *preceding-prelude-theories* nil)
(defvar *preceding-mono-theories* nil)
(defvar *suppress-output* nil)

(define-condition pvs2c-error (simple-error) (error-string))

;; Just a convenience macro for invoking pvs2c-error
(defmacro pvs2c-err (ctl &rest args)
  `(error 'pvs2c-error :format-control ,ctl :format-arguments (list ,@args)))

(defmethod decl-eval-info-instance ((decl formal-const-decl))
  'formal-const-eval-info)

(defmethod decl-eval-info-instance ((decl adt-constructor-decl))
  'constructor-eval-info)

(defmethod decl-eval-info-instance ((decl adt-accessor-decl))
  'accessor-eval-info)

(defmethod decl-eval-info-instance ((decl t))
  'eval-info)

(defun make-c-eval-info (decl)
  (let ((decl-instance (decl-eval-info-instance decl)))
    (cond ((eval-info decl)
	   (unless (internal (eval-info decl))
	     (setf (internal (eval-info decl))
		   (make-instance 'eval-defn-info
				  'unary (make-instance 'eval-defn)
				  'multiary (make-instance 'eval-defn)
				  'destructive (make-instance 'eval-defn))))
	   (unless (external (eval-info decl))
	     (setf (external (eval-info decl))
		   (make-instance 'eval-defn-info
				  'unary (make-instance 'eval-defn)
				  'multiary (make-instance 'eval-defn)
				  'destructive (make-instance 'eval-defn))))
	   (eval-info decl))
	  (t (setf (eval-info decl)
		   (make-instance decl-instance
				  'internal (make-instance 'eval-defn-info
							   'unary (make-instance 'eval-defn)
							   'multiary (make-instance 'eval-defn)
							   'destructive (make-instance 'eval-defn))
				  'external (make-instance 'eval-defn-info
							   'unary (make-instance 'eval-defn)
							   'multiary (make-instance 'eval-defn)
							   'destructive (make-instance 'eval-defn))))
	     (eval-info decl)))))


;;other types are char, bool, int32_t, int64_t, uint32_t, uint64_t, mpi, and mpz
;;we'll add floats later on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun free-ir-formals (ir-type)
  (let ((*free-ir-formals* nil))
    (free-ir-formals* ir-type)
    *free-ir-formals*))

(defmethod free-ir-formals* ((ir-type ir-formal-typename))
  (with-slots (ir-type-id) ir-type
	      (pushnew ir-type *free-ir-formals* :test #'equalp)))

(defmethod free-ir-formals* ((ir-type ir-typename))
  (with-slots (ir-type-defn ir-params) ir-type
	      (free-ir-formals* ir-type-defn)))


(defmethod free-ir-formals* ((ir-type ir-recordtype))
  (with-slots (ir-field-types) ir-type
	      (free-ir-formals* ir-field-types)))

(defmethod free-ir-formals* ((ir-type ir-fieldtype))
  (with-slots (ir-vtype) ir-type
    (free-ir-formals* ir-vtype)))

(defmethod free-ir-formals* ((ir-type ir-funtype))
  (with-slots (ir-domain ir-range) ir-type
	      (free-ir-formals* ir-domain)
	      (free-ir-formals* ir-range)))

(defmethod free-ir-formals* ((ir-type ir-arraytype))
  (with-slots (ir-range) ir-type
	      (free-ir-formals* ir-range)))

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
  `(intern (format nil "~a__~a" ,module-id ,decl-id)))

(defun pvs2ir-unique-decl-id (decl)
  (let ((module-id *theory-id*)		;(simple-id (id (module decl))))
	(decl-id (simple-id (id decl))))
    (if (const-decl? decl)
	(let ((same-id-decls (remove-if
				 (complement #'(lambda (d)
						 (and (const-decl? d)
						      (eq (simple-id (id d)) decl-id))))
			       (all-decls (module decl)))))
					;(assert (memq decl same-id-decls))
	  (if (cdr same-id-decls)
	      (let ((idx (1+ (position decl same-id-decls))))
		(make-c-name module-id (format nil "~a__~d"  decl-id idx))) ;;two underscores to avoid name confusion
	      (let ((dummy (when (eq decl-id '|ordstruct_adt|) (break "pvs2ir-unique-decl-id"))))
		(declare (ignore dummy))
		(make-c-name module-id decl-id))))
	(make-c-name  module-id decl-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *pvs2ir-primitives*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'TRUE nil '|booleans|)
	(mk-name 'FALSE nil '|booleans|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name '=> nil '|booleans|)
	(mk-name '⇒ nil '|booleans|)
	(mk-name '<=> nil '|booleans|)
	(mk-name '⇔ nil '|booleans|)
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
	(mk-name '∧ nil '|booleans|)	
	(mk-name 'OR nil '|booleans|)
	(mk-name '∨ nil '|booleans|)	
 	(mk-name 'NOT nil '|booleans|)
	(mk-name '¬ nil '|booleans|)
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
	(mk-name '|sqrt| nil '|sqrt| nil '|reals|)
	(mk-name '|even?| nil '|integers|)
	(mk-name '|odd?| nil '|integers|)
	(mk-name '|restrict| nil '|restrict|)
	(mk-name '|char?| nil '|character_adt|)
	(mk-name '|code| nil '|character_adt|)
	(mk-name '|char| nil '|character_adt|)
	(mk-name '|ord| nil '|character_adt|)
	(mk-name '|u8xor| nil '|integer_bv_ops|)
	(mk-name '|u16xor| nil '|integer_bv_ops|)
	(mk-name '|u32xor| nil '|integer_bv_ops|)
	(mk-name '|u64xor| nil '|integer_bv_ops|)
	(mk-name '|u8and| nil '|integer_bv_ops|)
	(mk-name '|u16and| nil '|integer_bv_ops|)
	(mk-name '|u32and| nil '|integer_bv_ops|)
	(mk-name '|u64and| nil '|integer_bv_ops|)
	(mk-name '|u8or| nil '|integer_bv_ops|)
	(mk-name '|u16or| nil '|integer_bv_ops|)
	(mk-name '|u32or| nil '|integer_bv_ops|)
	(mk-name '|u64or| nil '|integer_bv_ops|)
	(mk-name '|u8not| nil '|integer_bv_ops|)
	(mk-name '|u16not| nil '|integer_bv_ops|)
	(mk-name '|u32not| nil '|integer_bv_ops|)
	(mk-name '|u64not| nil '|integer_bv_ops|)
	(mk-name '|cons| nil '|list_adt|)
	(mk-name '|car| nil '|list_adt|)
	(mk-name '|cdr| nil '|list_adt|)
	(mk-name '|cons?| nil '|list_adt|)
	(mk-name '|null| nil '|list_adt|)
        (mk-name '|null?| nil '|list_adt|)
        (mk-name '|length| nil '|list_props|)
	(mk-name '|member| nil '|list_props|)
	(mk-name '|nth| nil '|list_props|)
	(mk-name '|append| nil '|list_props|)
	(mk-name '|reverse| nil '|list_props|)
	(mk-name '|nth| nil '|sequences|)
	(mk-name '|first| nil '|sequences|)
	(mk-name '|suffix| nil '|sequences|)
	(mk-name '|insert| nil '|sequences|)
	))

(defmethod pvs2ir-primitive? ((expr name-expr))
  (unless (every #'resolutions *pvs2ir-primitives*)
    (dolist (pr *pvs2ir-primitives*) (typecheck pr))
    (pushnew 'untypecheck-pvs2ir-primitives *untypecheck-hook*)
    (assert (every #'resolutions *pvs2ir-primitives*)))
  ;; Note that resolutions are not necessarily unique; number_fields.- is ambiguous
  (member expr *pvs2ir-primitives*
	  :test #'(lambda (ex pr) (member (declaration ex) (resolutions pr) :key #'declaration))))

(defmethod pvs2ir-primitive? ((expr t))
  nil)

(defun untypecheck-pvs2ir-primitives ()
  "Needed in case the any primitive is modified, e.g., by reloading prelude"
  (dolist (pr *pvs2ir-primitives*) (untypecheck pr)))


;;Not used
;; (defparameter *ir-primitives*
;;   '(= /= TRUE FALSE IMPLIES => ⇒ <=> ⇔ AND & ∧ OR ∨ NOT ¬ WHEN IFF + - * /
;;     |number_field_pred| < <= > >= |real_pred| |integer_pred| |integer?|
;;     |rational_pred| |floor| |ceiling| |nrem| |ndiv| |sqrt| |even?| |odd?|
;;     |cons| |car| |cdr| |cons?| |null| |null?| |restrict| |length|
;;     |member| |nth| |append| |reverse|
;;     |u8xor| |u16xor| |u32xor| |u64xor| |u8and| |u16and| |u32and| |u64and|
;;     |u8or| |u16or| |u32or| |u64or| |u8not| |u16not| |u32not| |u64not|))

;; (defparameter *ir-arith-primitives*
;;   '(+ - * / |number_field_pred| = < <= > >= |real_pred| |integer_pred| |integer?|
;;     |rational_pred| |floor| |ceiling| |ndiv| |nrem| |sqrt| |even?| |odd?|
;;     AND OR IMPLIES WHEN))

(defmacro new-irvar () ;;index this by the theory and declaration so that labels are stable
  `(intern (format nil "ivar_~a"  (funcall *var-counter*))))

(defmacro new-indvar () ;;index this by the theory and declaration so that labels are stable
  `(intern (format nil "index_~a"  (funcall *var-counter*))))

(defmacro new-irvars (length)
  `(loop for index from 1 to ,length collect (new-irvar)))

(defun new-irvartype (ir-type)
  (let ((ir-var (new-irvar)))
;    (format t "new-irvartype: ~a" ir-var)
    (mk-ir-variable ir-var ir-type)))

(defun new-irvartypes (ir-types)
  (if (consp ir-types)
      (loop for irt in ir-types collect (new-irvartype irt))
    (list (new-irvartype ir-types))))

(defun mk-eval-type-info (name)
  (make-instance 'eval-type-info
		 :ir-type-name name))



(defun mk-ir-variable (ir-name ir-type &optional ir-pvsid)
;;  (when (eq ir-type '|mpq|) (break "mk-ir-variable"))
  (make-instance 'ir-variable
		 :ir-name ir-name
		 :ir-vtype ir-type
		 :ir-pvsid ir-pvsid))

(defun mk-ir-function (fname &optional decl)
  (make-instance 'ir-function
		 :ir-fname fname
		 :declaration decl))

(defun mk-ir-primitive-function (fname &optional decl)
  (make-instance 'ir-primitive-function
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

(defun mk-ir-string (stringval)
  (make-instance 'ir-string :ir-stringval stringval))

(defun mk-ir-char (charval)
  (make-instance 'ir-char :ir-charval charval))

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
  (unless (or (ir-variable? function)(ir-last? function)(ir-function? function))
    (break "Expecting variable or function symbol here: ~a" (print-ir function)))
  (make-instance 'ir-apply
		 :ir-func function
		 :ir-params params
		 :ir-args args
		 :ir-atype atype))

(defun mk-ir-let (vartype expr body)
  (when (syntax? expr) (break "mk-ir-let"))
  (make-instance 'ir-let
		 :ir-vartype vartype
		 :ir-bind-expr expr
		 :ir-body body))

(defun mk-ir-lett (vartype bind-type expr body)
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

(defun mk-ir-array-literal (exprs)
  (make-instance 'ir-array-literal
		 :ir-array-literal-exprs exprs))
		 

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

(defun mk-ir-forall (vartype body);;ir-lastvars is nil
  (make-instance 'ir-forall
		 :ir-vartype vartype
		 :ir-body  body))

(defun mk-ir-exists (vartype body);;ir-lastvars is nil
  (make-instance 'ir-exists
		 :ir-vartype vartype
		 :ir-body  body))

(defun mk-ir-lambda-with-lastvars (vartypes lastvars rangetype body)
  (make-instance 'ir-lambda
		 :ir-vartypes vartypes
		 :ir-lastvars lastvars
		 :ir-rangetype rangetype
		 :ir-body  body))

;;Wrote this to move independent subexpressions out of the scope of a lambda. 
;;This can be revisited when there are motivating examples.  
;; (defun make-ir-lambda-with-livevars
;;     (vartypes livevars rangetype body dependent-boundvars dependent-lets liftable-lets)
;;   (cond ((ir-let? body)
;; 	 (with-slots (ir-vartype ir-bind-expr ir-body) body
;; 	   (let ((bind-freevars (pvs2ir-freevars* ir-bind-expr)))
;; 	     (cond ((null (intersection dependent-boundvars bind-freevars :test #'eq))
;; 		    (make-ir-lambda-with-livevars vartypes livevars rangetype
;; 						  ir-body dependent-boundvars dependent-lets
;; 						  (cons body liftable-lets)))
;; 		   (t (make-ir-lambda-with-livevars vartypes livevars rangetype
;; 						    ir-body (append (pvs2ir-freevars* ir-bind-expr)
;; 								    dependent-boundvars)
;; 						    (cons body dependent-lets) liftable-lets))))))
;; 	(t (rebuild-ir-lambda vartypes livevars rangetype body dependent-lets liftable-lets))))

;; (defun rebuild-ir-lambda (vartypes livevars rangetype body dependent-lets liftable-lets)
;;   (cond ((consp dependent-lets)
;; 	 (rebuild-ir-lambda vartypes rangetype (lcopy  (car dependent-lets) 'ir-body body)
;; 			    (cdr dependent-lets) liftable-lets))
;; 	(t
;; 	 (rebuild-lifted-lets (mk-ir-lambda-with-lastvars vartypes
;; 							    (set-difference (pvs2ir-freevars* body) livevars :test #'eq)
;; 							    rangetype body)
;; 			      liftable-lets))))

;; (defun rebuild-lifted-lets (body liftable-lets)
;;   (cond ((consp liftable-lets)
;; 	 (rebuild-lifted-lets (lcopy (car liftable-lets) 'ir-body body)
;; 			      (cdr liftable-lets)))
;; 	(t body)))
						    
		    

(defun mk-ir-ift (condition then else)
  (make-instance 'ir-ift
		 :ir-condition condition
		 :ir-then then
		 :ir-else else))

(defun mk-ir-offset (expr offset)
  ;(break "mk-ir-offset")
  (make-instance 'ir-offset
		 :expr expr
		 :offset offset))

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

(defun mk-ir-exit (message code)
  (make-instance 'ir-exit
		 :ir-message message
		 :ir-code code))

(defun mk-ir-type-actual (type)
  (make-instance 'ir-type-actual
		 :ir-actual-type type))

(defun mk-ir-typename (id type formals actuals decl)
  (make-instance 'ir-typename
		 :ir-type-id id
		 :ir-type-defn type
		 :ir-formals formals
		 :ir-actuals actuals
		 :type-declaration decl))

(defun mk-ir-formal-typename (id)
  (make-instance 'ir-formal-typename
		 :ir-type-id id))



(defun mk-ir-recordtype (field-types &optional label)
  (make-instance 'ir-recordtype
		 :ir-label label
		 :ir-field-types field-types))

(defun mk-ir-tupletype (field-types &optional label)
;  (when (and field-types (null (ir-id (car field-types))))(break "mk-ir-tupletype"))
  (make-instance 'ir-tupletype
		 :ir-label label
		 :ir-field-types field-types))

(defun mk-ir-stringtype ()
  (make-instance 'ir-stringtype))

(defun mk-ir-chartype ()
  (make-instance 'ir-chartype))

(defun mk-ir-adt-recordtype (field-types constructors)
;  (when (and (consp field-types) (null (ir-id (car field-types))))(break "~a" (print-ir field-types)))
  (make-instance 'ir-adt-recordtype
		 :ir-field-types field-types ;; ir-fieldtype with id and type
		 :ir-constructors constructors))

(defun mk-ir-adt-constructor-recordtype (constructor-id field-types adt-name)
  (make-instance 'ir-adt-constructor-recordtype
		 :constructor-id constructor-id
		 :ir-field-types field-types
		 :ir-adt-name adt-name))



(defun mk-ir-fieldtype (id type);doubles as a variable
 ; (when (null id) (break "mk-ir-fieldtype"))
  (make-instance 'ir-fieldtype
		 :ir-id id ; ir-id is the field name
		 :ir-name (gentemp (string id))  ;;use ir-name for unique name for the declaration
		 :ir-vtype type))

(defun mk-ir-funtype (domain range)
  (make-instance 'ir-funtype
		 :ir-domain domain
		 :ir-range range))

(defun mk-ir-subrange (low high &optional ir-low-expr ir-high-expr)
  ;(break "mk-ir-subrange")
  (make-instance 'ir-subrange
		 :ir-low low
		 :ir-high high
		 :ir-low-expr ir-low-expr
		 :ir-high-expr ir-high-expr))

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
    (if ir-boolval '|true| '|false|)))

(defmethod print-ir ((ir-expr ir-string))
  (with-slots (ir-stringval) ir-expr
    ir-stringval))

(defmethod print-ir ((ir-expr ir-char))
  (with-slots (ir-charval) ir-expr
    ir-charval))

(defmethod print-ir ((ir-expr ir-array-literal))
  (with-slots (ir-array-literal-exprs) ir-expr
    `(array-literal ,(print-ir ir-array-literal-exprs))))

(defmethod print-ir ((ir-expr ir-variable))
  `(,(ir-name ir-expr) ,(print-ir (ir-vtype ir-expr)) ,(ir-pvsid ir-expr)))

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
  (with-slots (ir-fields ir-recordtype) ir-expr
	      `(record [ ,(print-ir ir-recordtype) ] ,(print-ir ir-fields))))

(defmethod print-ir ((ir-expr ir-field))
  (with-slots (ir-fieldname ir-value) ir-expr
	      `(= ,ir-fieldname ,(print-ir ir-value))))

(defmethod print-ir ((ir-expr ir-lambda))
  (with-slots (ir-vartypes ir-lastvars ir-rangetype ir-body) ir-expr
    `(lambda (,@(print-ir ir-vartypes)) '-> ,(print-ir ir-rangetype) ,(print-ir ir-body)
       ,(print-ir ir-lastvars))))

(defmethod print-ir ((ir-expr ir-forall))
  (with-slots (ir-vartype ir-body) ir-expr
    `(forall ,(print-ir ir-vartype) ,(print-ir ir-body))))

(defmethod print-ir ((ir-expr ir-exists))
  (with-slots (ir-vartype ir-body) ir-expr
    `(exists ,(print-ir ir-vartype) ,(print-ir ir-body))))


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

(defmethod print-ir ((ir-expr ir-exit))
  '(|exit|))

(defmethod print-ir :around ((ir-type ir-type))
  (with-slots (renamings) ir-type
    (let ((print-renames (loop for (x . y) in renamings collect (cons (print-ir x)(print-ir y)))))
      (if print-renames
	  `(rename ,print-renames in ,(call-next-method))
	(call-next-method)))))
	  
(defmethod print-ir ((ir-expr ir-const-actual))
  (with-slots (ir-actual-expr) ir-expr
	      `(const-actual ,(print-ir ir-actual-expr))))

(defmethod print-ir ((ir-type ir-type-actual))
  (with-slots (ir-actual-type) ir-type
	      `(type-actual ,(print-ir ir-actual-type))))

(defmethod print-ir ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn type-declaration) ir-type
    ;(when (and type-declaration (eq (id (module type-declaration)) 'ordstruct_adt))(break "print-ir(ordstruct_adt"))
    (format nil "~a!~a"
	    (if type-declaration (id (module type-declaration)) "")
	    ir-type-id)))


(defmethod print-ir ((ir-type ir-formal-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
    ir-type-id))



(defmethod print-ir ((ir-type ir-recordtype))
  (with-slots (ir-field-types) ir-type
    `(recordtype ,(print-ir ir-field-types))))

(defmethod print-ir ((ir-type ir-stringtype))
  '|string|)

(defmethod print-ir ((ir-type ir-chartype))
  '|char|)


(defmethod print-ir ((ir-type ir-adt-recordtype))
  (with-slots (ir-field-types ir-constructors) ir-type
	      `(adt-recordtype ,(print-ir ir-field-types) :constructors ,ir-constructors)))

(defmethod print-ir ((ir-type ir-adt-constructor-recordtype))
  (with-slots (ir-field-types ir-adt-name) ir-type
	      `(constructor-recordtype ,(print-ir ir-field-types) :adt ir-adt-name)))

(defmethod print-ir ((ir-type ir-fieldtype))
  (with-slots (ir-id ir-name ir-vtype) ir-type 
	      `(=> (,ir-id ,ir-name) ,(print-ir ir-vtype))))

(defmethod print-ir ((ir-type ir-funtype))
  (with-slots (ir-domain ir-range) ir-type
	      `(-> ,(print-ir ir-domain) ,(print-ir ir-range))))

(defmethod print-ir ((ir-type ir-subrange))
  (with-slots (ir-low ir-high ir-low-expr ir-high-expr) ir-type
	      `(subrange ,ir-low ,ir-high ,(print-ir ir-low-expr) ,(print-ir ir-high-expr))))

(defmethod print-ir ((ir-expr list))
  (cond ((consp ir-expr)
	 (cons (print-ir (car ir-expr))
	       (print-ir (cdr ir-expr))))
	(t nil)))

(defmethod print-ir ((ir-expr t))
  ir-expr)

(defmethod print-ir ((ir-texpr simple-c-type-info))
  (print-ir ir-texpr))

(defmethod ir-field-types ((ir-expr ir-variable))
  (ir-field-types (ir-vtype ir-expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pvs-integer computes the integer value of a number or unary-negated number
(defun pvs-integer? (expr)
  (or (and (number-expr? expr)(number expr))
      (and (is-unary-minus? expr)(number-expr? (argument expr)) (- (number (argument expr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pvs2ir (expr &optional ir-theory-tbindings context expected)
    (let* ((*current-context*
	    (if context context *current-context*))
	   (*generate-tccs* 'none))
      (pvs2ir* expr ir-theory-tbindings expected)));;NSH(8/13/17): added ir-theory-tbindings


(defmethod pvs2ir* ((expr int-expr) bindings expected)
  (declare (ignore bindings expected))
  (with-slots (number) expr
    (mk-ir-integer number)))

(defmethod pvs2ir* ((expr number-expr) bindings expected)
  (declare (ignore bindings expected))
  (with-slots (number) expr
    (mk-ir-integer number)))


(defmethod pvs2ir* ((expr string-expr) bindings expected)
  (declare (ignore bindings expected))
  (mk-ir-string (string-value expr)))

(defmethod pvs2ir* ((expr array-expr) bindings expected)
  ;(break "pvs2ir*(array-expr)")
  (with-slots (exprs type) expr
    (let* ((type (or expected (type expr)))
	   ;;(stype (find-supertype type))
	   )
      (if (arraytype? type)
	  (let* ((elem-type (range (find-supertype type)))
		 (ir-elem-type (pvs2ir-type elem-type bindings))
		 (let-vars (loop for x in exprs collect (new-irvartype ir-elem-type)))
		 (ir-exprs (loop for x in exprs collect (pvs2ir* x bindings elem-type)))) ;(break "array-expr")
	    (mk-ir-let* let-vars ir-exprs  (mk-ir-array-literal let-vars)))
	(let ((lexpr (change-class (copy expr) 'lambda-expr)))
					;(NSH:10/20/23) Why doesn't call-next-method work?
	  (pvs2ir* lexpr bindings expected))))))
  

(defmethod pvs2ir* ((expr actual) bindings expected)
  (with-slots (expr type-value) expr
    (if type-value			 ;then it's a type actual
	(if (null (freevars type-value)) ;;NSH(1/1/21): lifting actuals to the top of decl. 
	    (let ((bnd (assoc type-value *pvs2c-defn-actuals* :test #'tc-eq)))
	      (if bnd (cdr bnd)
		(let ((new-binding (mk-ir-variable (new-irvar) *type-actual-ir-name*)))
		  (push (cons type-value new-binding) *pvs2c-defn-actuals*)
		  new-binding)))
	  (mk-ir-type-actual (pvs2ir-type type-value bindings)))
      (mk-ir-const-actual (pvs2ir* expr bindings expected)))))

(defun pvs2ir-formal-types (formals ir-actuals bindings)
  (cond ((consp formals)
	 (cond ((formal-const-decl? (car formals));(break "const?")
		(cons (pvs2ir-type (type (car formals)) bindings)
		      (pvs2ir-formal-types (cdr formals)(cdr ir-actuals) bindings)))
	       (t ;(break "type?")
		(cons *type-actual-ir-name*
			(pvs2ir-formal-types (cdr formals)(cdr ir-actuals)
					     (cons (cons (car formals)
							 (if (ir-const-formal? (car ir-actuals))
							     (car ir-actuals)
							   *type-actual-ir-name*))
						   bindings))))))
	(t nil)))

(defmethod pvs2ir* ((expr formal-type-decl) bindings expected)
  (declare (ignore expected))
  (let ((bnd (assoc expr bindings)))
    (if bnd (if (ir-const-formal? (cdr bnd))
		(mk-ir-formal-typename (ir-name (cdr bnd)))
	      (cdr bnd))
      (pvs2ir-decl* expr))))

(defmethod pvs2ir* ((expr formal-const-decl) bindings expected)
  (declare (ignore expected))
  (let ((bnd (assoc expr bindings)))
    (cond (bnd (cdr bnd))
	  (t (pvs2ir-decl* expr)
	     (ir-defn (ir (eval-info expr)))))))

(defmethod pvs2ir* ((expr name-expr) bindings expected)
  (let* ((decl (declaration expr))
	 (bnd (assoc  decl bindings :key #'declaration)))
    (assert (not (and bnd (const-decl? decl))))
    (if bnd
	(let ((ir-expected-type (and expected (pvs2ir-type expected)))
	      (bnd-irvar (cdr bnd)))
	  (if (or (null ir-expected-type) (ir2c-tequal ir-expected-type (ir-vtype (cdr bnd))))
	      bnd-irvar
	    (let ((expected-irvar (mk-ir-variable (new-irvar) ir-expected-type)))
	      (mk-ir-let expected-irvar
		       bnd-irvar
		       expected-irvar))))
      (if (const-decl? decl)
	  (let* ((formals (formals-sans-usings (module decl)))
		 (actuals (actuals (module-instance expr)))
		 ;(formals));;actuals can be the parameters of the current theory
		 (ref-actuals (loop for act in actuals;;collect const actuals and ref actuals
				    when (or (null (type-value act))
					     (eq (check-actual-type (type-value act) bindings) 'ref))
				collect act))
		 (ir-actuals (if actuals
				 (pvs2ir* ref-actuals ;;needs to be fixed when handling actuals
					  bindings nil)
				 (loop for formal in formals
				       collect (or (cdr (assoc formal bindings))
						   (pvs2ir* formal nil nil)))))
		 (ir-vars (new-irvars (length ir-actuals)))
		 (ref-formals (if actuals;if external call, exclude the formals matching nonref type actuals
				  (loop for fml in formals
					as act in actuals ;;collect const actuals and ref actuals
					when (or (null (type-value act))
						 (eq (check-actual-type (type-value act) bindings) 'ref))
					collect fml)
				formals))
		 (ir-formal-types (pvs2ir-formal-types ref-formals ir-actuals nil))
		 (ir-actuals-types (loop for actual in ref-actuals
					 as ir-actual in ir-actuals
					 when (or (formal-const-decl? actual) (and (actual? actual)(expr actual)))
					 collect (if (and (ir-const-actual? ir-actual)
							  (ir-integer? (ir-actual-expr ir-actual)))
						     (mk-ir-subrange (ir-intval (ir-actual-expr ir-actual))
								     (ir-intval (ir-actual-expr ir-actual)))
						   (pvs2ir-type (or (type-value actual)
								    (type (expr actual)))
								;; (if (formal-const-decl? actual)
								;;     (type actual)
								;;    (type-value actual))
					;NSH(3/21/22) was (type (expr..
								bindings))))
		 (ir-vartypes (loop for ir-var in ir-vars
				    as ir-ft in ir-formal-types
				    as formal in ref-formals
				    collect (mk-ir-variable ir-var ir-ft (id formal))))	;;NSH(1/17/20): added id
		 (ir-function (pvs2ir-constant expr bindings))
		 (defn (decl-defn decl))
		 (ir-atype (pvs2ir-type (type expr) bindings))
		 (ir-atype-defn (if (ir-typename? ir-atype)
				    (ir-type-defn ir-atype)
				  ir-atype))
		 (eta-expr (if (lambda-expr? defn)
			       (with-slots (ir-domain ir-range) ir-atype-defn
				 (let* ((expr-arg-vars (new-irvartypes (ir-tuple-args ir-domain))))
				   (mk-ir-lambda expr-arg-vars ir-range
						 (mk-ir-apply ir-function expr-arg-vars ir-vartypes 
							       ir-range))))
			     (if ir-actuals (mk-ir-apply ir-function nil ir-vartypes ir-atype)
			       ir-function))));(break "pvs2ir* ")
						 
	    (if ir-actuals ;;NSH(5/28/19) check if  eta-expansion is needed here.
		(mk-ir-lett* ir-vartypes ir-actuals-types ir-actuals
			     eta-expr)
	      eta-expr))
	(if (formal-const-decl? decl)
	    (pvs2ir* decl bindings expected)
	  (break "Should not happen"))))))

(defun pvs2ir-lambda-bindings (boundvars bindings)
  (cond ((consp boundvars)
	 (let* ((bnd1 (car boundvars))
	       (ir-var1 (mk-ir-variable (new-irvar)(pvs2ir-type (type bnd1) bindings)(id bnd1))));;NSH(1/17/20)
	   (cons ir-var1 (pvs2ir-lambda-bindings (cdr boundvars)(acons bnd1 ir-var1 bindings)))))
	(t nil)))

(defmethod get-range ((ir-type ir-funtype))
  (with-slots (ir-range) ir-type ir-range))

(defmethod get-range ((ir-type ir-arraytype))
  (with-slots (ir-range) ir-type ir-range))


(defmethod get-range ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type (get-range ir-type-defn)))

(defun dependent-function-range (funtype argvars)
  (with-slots (domain range) funtype
    (if (typep domain 'dep-binding)
	(if (> (length argvars) 1)
	    (substit range (acons domain
				  (make-tuple-expr (loop for bd in argvars collect (make-variable-expr bd)))
				  nil))
	  (substit range (acons domain (make-variable-expr (car argvars)) nil)))
      range)))

(defmethod pvs2ir* ((expr lambda-expr) bindings expected)
  (with-slots ((expr-bindings bindings) expression) expr 
    (let* ((ir-binds (pvs2ir-lambda-bindings expr-bindings bindings))
	   (ir-var-bindings (pairlis expr-bindings ir-binds))
	   (in-bindings (append ir-var-bindings bindings))
	   (expected-supertype (and expected (find-supertype expected)))
	   (expected-range (and expected (dependent-function-range expected-supertype expr-bindings)))
	   (ir-rangetype (or (and expected (pvs2ir-type expected-range in-bindings))
			     (pvs2ir-expr-type  expression in-bindings)))
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
			(pvs2ir* (operator expression) bindings expected)
		      (pvs2ir* expression in-bindings  expected-range)))) ;(break "lambda")
      (let ((lambda-ir (if eta-form? ir-expr 
			 (mk-ir-lambda ir-binds ir-rangetype ir-expr)))) ;(break "pvs2ir*(lambda)")
					;(format t "~%PVS: ~s becomes ~s" expr (print-ir lambda-ir))
	lambda-ir))))

(defmethod pvs2ir* ((expr lambda-expr-with-type) bindings expected)
  (with-slots ((expr-bindings bindings) expression) expr 
    (let* (;;(ir-expected (pvs2ir-type expected bindings))
	   (ir-binds (pvs2ir-lambda-bindings expr-bindings bindings))
	   (ir-var-bindings (pairlis expr-bindings ir-binds))
	   (new-bindings (append ir-var-bindings bindings))
	   (ir-rangetype (pvs2ir-type (return-type expr) new-bindings))
	   (ir-expr (pvs2ir* expression new-bindings
			     (and expected (dependent-function-range
					    (find-supertype expected)
					    expr-bindings)))))
      ;(break "lambda-with")
      (let ((lambda-ir (mk-ir-lambda ir-binds ir-rangetype ir-expr)))
					;(format t "~%PVS: ~s becomes ~s" expr (print-ir lambda-ir))
	lambda-ir))))


(defmethod bitvector-type? ((ty funtype))
  (with-slots (domain range) ty
    (and (tc-eq (find-supertype range) *boolean*)
	 (or (simple-below? domain)
	     (simple-upto? domain)))))

(defmethod bitvector-type? ((ty subtype))
  (with-slots (supertype) ty
    (bitvector-type? supertype)))

(defmethod bitvector-type? ((ty t))
  nil)

;;intersects the subranges (if any) in ir-types; returns nil, otherwise. 
(defun best-ir-subrange-list (ir-types)
  (cond ((consp ir-types)
	 (let ((ir-typ1 (is-ir-subrange? (car ir-types))))
	   (if ir-typ1
	       (let ((cdr-subrange (best-ir-subrange-list (cdr ir-types))))
		 (if cdr-subrange
		     (intersect-subrange ir-typ1 cdr-subrange)
		   ir-typ1))
	     (best-ir-subrange-list (cdr ir-types)))))
	(t nil)))

;;intersects the subranges in type and judgement-types; returns type if there are no subranges
(defun best-ir-subrange (type judgement-types &optional bindings)
  (let* ((ir-type (pvs2ir-type type bindings))
	 (ir-jtypes (loop for jtype in judgement-types collect (pvs2ir-type jtype bindings))))
    (best-ir-subrange* ir-type ir-jtypes bindings)))

(defun best-ir-subrange* (ir-type ir-jtypes bindings)
  (if (ir-funtype? ir-type);;(NSH: 9/7/17): added best computation for function types
      (let* ((ir-dom (ir-domain ir-type))
	     (domain-equiv-jtypes-range (loop for ir-jtype in ir-jtypes
				       when (and (ir-funtype? ir-jtype)
						 (ir2c-tequal ir-dom
							      (ir-domain ir-jtype)))
				       collect (ir-range ir-jtype)))
	     (best-range (best-ir-subrange* (ir-range ir-type) domain-equiv-jtypes-range bindings)))
	(mk-ir-funtype ir-dom best-range))
    (let ((ir-jsubrange (best-ir-subrange-list ir-jtypes)))
      (if ir-jsubrange
	  (let ((ir-subrange-type (is-ir-subrange? ir-type)))
	    (if ir-subrange-type
		(intersect-subrange ir-subrange-type ir-jsubrange)
	      ir-jsubrange))
	ir-type))))

(defun best-ir-subrange-pair (ty1 ty2)
  (cond ((and (ir-subrange? ty1)(ir-subrange? ty2))
	 ;; (format t "best-ir-subrange-pair(~a, ~a): ~a"
	 ;; 	 (print-ir ty1)(print-ir ty2)
	 ;; 	 (print-ir (intersect-subrange ty1 ty2)))
	 (intersect-subrange ty1 ty2))
	(t ty1)))
			       

(defmethod pvs2ir* ((expr application) bindings expected);(break "app")
  (with-slots (operator argument) expr
	      (let ((val (pvs-integer? expr))
		    (expr-type (type expr)))
		(or (and val 
			 (mk-ir-integer val))
		    (let ((ir-expr-type
			   (if (and (pvs2ir-primitive? operator)
				    (memq (id operator) '(+ - * /)));ensures that these are arithops
			       (best-ir-subrange expr-type (judgement-types expr) bindings)
			     (pvs2ir-type expr-type bindings))))
		      (pvs2ir-application operator (arguments expr) ir-expr-type bindings
					  (and expected (find-supertype expected))))))))

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
      (loop for fld in (ir-field-types ir-typ) collect (ir-vtype fld))
    (list ir-typ)))

(defun make-ir-let (vartype expr body)
  (with-slots (ir-name ir-vtype) vartype
     (let ((body-freevars (pvs2ir-freevars* body)))
       (if (memq vartype body-freevars);;(10-26-22)discard let-binding when vartype is not free in body
	  (if (and (ir-function? expr)
		   (ir-funtype? ir-vtype))
	      (with-slots (ir-domain ir-range) ir-vtype
		(let* ((expr-arg-vars (new-irvartypes (ir-tuple-args ir-domain)))
		       (eta-expr (mk-ir-lambda expr-arg-vars ir-range
					       (mk-ir-apply expr expr-arg-vars nil ir-range))))
		  (mk-ir-let vartype eta-expr body)))
	    (mk-ir-let vartype expr body))
	body))))

(defun make-ir-let* (vartypes exprs body);;no need to add expr-types here
  (cond ((consp vartypes)
	 (make-ir-let (car vartypes)(car exprs)
		    (make-ir-let* (cdr vartypes)(cdr exprs) body)))
	(t body)))

(defun make-ir-lett (vartype expr-type expr body)
  (let ((ir-vtype (ir-vtype vartype)))  
    (if  (ir2c-tcompatible ir-vtype expr-type);;(ir-arraytype? expr-type)(ir-arraytype? ir-vtype))
      (make-ir-let vartype expr body)
    (if (and (ir-funtype? expr-type)
	     (ir-funtype? (ir-vtype vartype)))
	;;The range types might not match, e.g., expr returns mpz_t but range(vartype) is uint8_t
	;;we use eta-expansion on expr to get
	;;(let ev expr (let vt (let earg nuv (lambda nuv->range (ev earg)))))
	(let* ((expr-range-type (ir-range expr-type))
	       (range-type (ir-range ir-vtype))
	       (expr-var (new-irvartype expr-type))
	       (apply-var (new-irvartype expr-range-type))
	       (expr-arg-vars (new-irvartypes (ir-tuple-args (ir-domain expr-type))))
	       (nu-vars (new-irvartypes (ir-tuple-args (ir-domain ir-vtype))))
	       
	       (return-var (new-irvartype range-type)))
;;	  (break "make-ir-lett with expected type ~a ~%and actual type ~a" (print-ir ir-vtype) (print-ir expr-type))
	  (make-ir-let expr-var expr 
		       (mk-ir-let vartype (mk-ir-lambda nu-vars range-type ;;coerce to range type
							(make-ir-lett apply-var expr-range-type
								      (mk-ir-let* expr-arg-vars nu-vars
										  (mk-ir-apply expr-var  expr-arg-vars nil
											       expr-range-type))
								      (make-ir-lett return-var expr-range-type
										    apply-var
										    return-var)))
				  body)))
      (if (or (ir-variable? expr)(ir-last? expr))
	  (mk-ir-lett vartype expr-type expr body)
	(let* ((bind-var (new-irvar))
	       (bind-vartype (mk-ir-variable bind-var expr-type)))
	  (mk-ir-let bind-vartype expr;;ensures that mk-ir-lett always binds var to var'
		     (mk-ir-lett vartype expr-type (mk-ir-last bind-vartype) body))))))))
  

(defun make-ir-lett* (vartypes expr-types exprs body);;more sophisticated version of mk-ir-lett*
  (cond ((consp vartypes)
	 (make-ir-lett (car vartypes) (car expr-types) (car exprs)
		       (make-ir-lett* (cdr vartypes)(cdr expr-types)(cdr exprs) body)))
	(t body)))

(defun adt-expr? (ex)
  ;; Note that if ex is a plain name-expr, it will be converted to
  ;; a constructor-name-expr, etc.
  (or (constructor? ex)
      (accessor? ex)
      (recognizer? ex)))

(defun clear-decl (decl)
  (clear-decl* decl))

(defmethod clear-decl* ((decl type-eq-decl))
  (setf (ir-type-value decl) nil) decl)

(defmethod clear-decl* ((decl type-decl))
  (setf (ir-type-value decl) nil) decl)

(defmethod clear-decl* ((decl formal-type-decl))
  (setf (ir-type-value decl) nil) decl)

(defmethod clear-decl* ((decl const-decl))
  (and (eval-info decl)(setf (ir (eval-info decl)) nil
			     (cdefn (eval-info decl)) nil
			     (c-type-info-table (eval-info decl)) nil))
  decl)

(defmethod clear-decl* ((decl test-formula))
  (and (eval-info decl)(setf (ir (eval-info decl)) nil
			     (cdefn (eval-info decl)) nil
			     (c-type-info-table (eval-info decl)) nil))
  decl)

;adt decls are clear with (eval-info decl) is set; clearing them at pvs2c-decl
;causes the properly set values of this slot to be wiped out. 
(defmethod clear-decl* ((decl adt-recognizer-decl))
  decl)


(defmethod clear-decl* ((decl adt-constructor-decl))
  decl)


(defmethod clear-decl* ((decl adt-accessor-decl))
  decl)





(defmethod clear-decl* ((decl t))
  decl)




(defun pvs2ir-decl (decl)
  (let* ((*current-context* (decl-context decl))
	 (*generate-tccs* 'none))
    ;(unless (decl-formals decl))
    (pvs2ir-decl* decl)))

(defun copy-without-print-type (type)
  (copy type 'print-type nil))

(defmethod pvs2ir-decl* ((decl type-eq-decl))
  (let ((ir-type-value (ir-type-value decl)))
    (if ir-type-value ;it must be an ir-typename
	(ir-type-id (ir-type-name ir-type-value))
      (let* ((theory-formals (formals-sans-usings (module decl)))
	     (decl-formals (loop for fmls in (formals decl)
				 append fmls))
	     (ir-theory-formals (pvs2ir* theory-formals nil nil))
	     (ir-decl-formals (pvs2ir-lambda-bindings decl-formals nil))
	     (all-formals (append theory-formals decl-formals))
	     (ir-formals (append ir-theory-formals ir-decl-formals))
	     (bindings (pairlis all-formals ir-formals))
	     (ir-type (pvs2ir-type (copy-without-print-type (type-value decl)) bindings))) ;;loops without copy
	  ;; (if (or (ir-typename? ir-type)
	  ;; 	  (ir-subrange? ir-type))
	  ;;     ir-type ;;ir-type might not be a typename
	  (cond ((ir-typename? ir-type);;return the existing name-don't nest ir-typenames
		 (setf (ir-type-value decl) (mk-eval-type-info ir-type))
		 (ir-type-id ir-type));otherwise, create a new definition
		(t (let ((ir-type-name (mk-ir-typename (pvs2ir-unique-decl-id decl) ir-type
						       ir-formals nil decl)));nil actuals 
		     (push ir-type-name *ir-type-info-table*)
		     (setf (ir-type-value decl)
			   (mk-eval-type-info ir-type-name))
		     (ir-type-id ir-type-name))))))))

(defmethod pvs2ir-decl* ((decl type-decl))
  (and (or (ir-type-value decl)
	   (let ((type-value (type-value decl)))
	     (if (and type-value
		      (adt type-value))
		 (pvs2ir-adt-decl decl)
	       (let* ((theory-formals (formals-sans-usings (module decl)))
		      (decl-formals (loop for fmls in (formals decl)
					  append fmls))
		      (ir-theory-formals (pvs2ir* theory-formals nil nil))
		      (ir-decl-formals (pvs2ir-lambda-bindings decl-formals nil))
		      ;; (all-formals (append theory-formals decl-formals))
		      (ir-formals (append ir-theory-formals ir-decl-formals))
		      (ir-type-name (mk-ir-typename (pvs2ir-unique-decl-id decl) nil ir-formals nil  decl)))
		 (push ir-type-name *ir-type-info-table*)
		 (setf (ir-type-value decl)
		       (mk-eval-type-info ir-type-name))))))  ;;Should check that the ir-type-value slot is set
       (ir-type-name (ir-type-value decl))))

(defmethod pvs2ir-decl* ((decl formal-type-decl))
  (let ((ir-type-value (ir-type-value decl)))
    (or (and ir-type-value (ir-type-name ir-type-value))
	(let* ((ir-type-name-id (pvs2ir-unique-decl-id decl))
	       (ir-type-name (mk-ir-formal-typename ir-type-name-id)))
	  (push ir-type-name *ir-type-info-table*)
	  (setf (ir-const-formal ir-type-name)
		(mk-ir-const-formal ir-type-name-id
				    *type-actual-ir-name*))
	  (setf (ir-type-value decl)
		(mk-eval-type-info ir-type-name))
	  ir-type-name))))

(defmethod pvs2ir-decl* ((decl formal-const-decl))
  (let* ((einfo (eval-info decl))
	 (einfo (or einfo
		    (let ((new-einfo (make-c-eval-info decl)))
		      (setf (eval-info decl) new-einfo)
		      new-einfo))))
    (setf (ir einfo) (make-instance 'ir-formal-const-defn))
    (setf (ir-function-name (ir einfo)) (mk-ir-function (pvs2ir-unique-decl-id decl) decl))
    (setf (ir-defn (ir einfo)) (mk-ir-const-formal (pvs2ir-unique-decl-id decl)
						   (pvs2ir-type (type decl))))
    (id decl)))

(defun pvs2ir-formals (irvars pvars bindings);irvars and pvars (pvsvars) are same length
  (cond ((and (consp irvars)(consp pvars))
	 (let ((ir-variable (mk-ir-variable (car irvars) (pvs2ir-type (type (car pvars)) bindings) (id (car pvars)))))
	  (cons ir-variable
		(pvs2ir-formals (cdr irvars)(cdr pvars) (acons (declaration (car pvars)) ir-variable bindings)))))
	(t nil)))

(defun pvs2ir-lifted-actuals (ir-defn lifted-actuals)
  (cond ((consp lifted-actuals)
	 (mk-ir-let (cdar lifted-actuals)(mk-ir-type-actual (pvs2ir-type (caar lifted-actuals) nil))
		    (pvs2ir-lifted-actuals ir-defn (cdr lifted-actuals))))
	(t ir-defn)))

(defun decl-defn (const-decl)
  (let (;;(defns (def-axiom const-decl))
	(definition (definition const-decl)))
    (if definition
	  (make!-lambda-exprs (formals const-decl) definition)
	(when (and (eq (id const-decl) '|rem|)
		   (eq (id (module const-decl)) '|modulo_arithmetic|))
	  (let ((remnrem0-decls (get-decls "rem_nrem0")))
	    (loop for dd in remnrem0-decls
		  when (eq (id (module dd)) '|modulo_arithmetic|)
		  return (args2 (definition dd))))))))

(defmethod pvs2ir-decl* ((decl const-decl))
  (let* ((einfod (eval-info decl))
	 (einfo (or einfod (make-c-eval-info decl))))
    (let* ((ir-einfo (ir einfo))
	   (ir-function-name (when ir-einfo (ir-function-name ir-einfo))))
      (or ir-function-name
	  (let* ((defn (decl-defn decl)))
	    (unless ir-einfo ;first create eval-info then fill the function name
	      (setf (ir einfo)
		    (make-instance 'ir-defn)))
	    (setf (ir-function-name (ir einfo))
		  (mk-ir-function (pvs2ir-unique-decl-id decl)
				  decl))
	    ;;create the ir for the definition
	    ;;(newcounter *var-counter*)
	    ;;(break "pvs2ir-decl* (const-decl)")
	    (let* ((context (decl-context decl))
		   (type (find-supertype (type decl)))
		   (ir-defn (if defn
				(pvs2ir defn *ir-theory-tbindings* context type)
				(pvs2c-err "Missing definition for ~a" (id decl))))
		   (ir-defn-with-tformals
		    (if *ir-theory-formals*
			(cond ((ir-lambda? ir-defn)
			       (setf (ir-vartypes ir-defn)
				     (append *ir-theory-formals* (ir-vartypes ir-defn)))
					;(break "defn-with-theory-formals")
			       ir-defn)
			      (t (mk-ir-lambda *ir-theory-formals* (pvs2ir-type (type decl))
					       ir-defn)))
			ir-defn))
		   (args (when (ir-lambda? ir-defn-with-tformals)
			   (ir-vartypes ir-defn-with-tformals)))
		   (returntype (or (and (ir-lambda? ir-defn-with-tformals)
					(ir-rangetype ir-defn-with-tformals))
				   (pvs2ir-type (type decl))))
		   (body (if (ir-lambda? ir-defn-with-tformals)
			     (ir-body ir-defn-with-tformals)
			     ir-defn-with-tformals))
		   (lifted-actuals-body (pvs2ir-lifted-actuals body *pvs2c-defn-actuals*)))
	      ;;(break "pvs2ir-decl2")
	      ;;(format t "~%ir-theory-formals = ~{~a, ~}" *ir-theory-formals*)
	      ;;(format t "~% pvs2ir-decl*, ir-defn = ~a" (print-ir ir-defn-with-tformals))
	      (setf (ir-args (ir einfo)) args
		    (ir-return-type (ir einfo)) returntype
		    (ir-defn (ir einfo)) lifted-actuals-body)
	      (ir-function-name (ir einfo))))))))

(defmethod pvs2ir-decl* ((decl test-formula))
  (let* ((einfod (eval-info decl))
	 (einfo (or einfod (make-c-eval-info decl)))
	 (ir-einfo (ir einfo))
	 (ir-function-name (when ir-einfo (ir-function-name ir-einfo))))
    (or ir-function-name
	(let ((defn (closed-definition decl)))
	  (unless ir-einfo ;first create eval-info then fill the function name
	    (setf (ir einfo) (make-instance 'ir-defn)))
	  (setf (ir-function-name (ir einfo))
		(mk-ir-function (pvs2ir-unique-decl-id decl) decl))
	  (let* ((context (decl-context decl))
		 (type *boolean*)
		 (ir-defn (pvs2ir defn *ir-theory-tbindings* context type))
		 (ir-defn-with-tformals
		  (if *ir-theory-formals*
		      (cond ((ir-lambda? ir-defn)
			     (setf (ir-vartypes ir-defn)
				   (append *ir-theory-formals* (ir-vartypes ir-defn)))
					;(break "defn-with-theory-formals")
			     ir-defn)
			    (t (mk-ir-lambda *ir-theory-formals* (pvs2ir-type (type decl))
					     ir-defn)))
		      ir-defn))
		 (args (when (ir-lambda? ir-defn-with-tformals)
			 (ir-vartypes ir-defn-with-tformals)))
		 (returntype (pvs2ir-type *boolean*))
		 (body (if (ir-lambda? ir-defn-with-tformals)
			   (ir-body ir-defn-with-tformals)
			   ir-defn-with-tformals))
		 (lifted-actuals-body (pvs2ir-lifted-actuals body *pvs2c-defn-actuals*)))
	    ;;(format t "~%ir-theory-formals = ~{~a, ~}" *ir-theory-formals*)
	    ;;(format t "~% pvs2ir-decl*, ir-defn = ~a" (print-ir ir-defn-with-tformals))
	    (setf (ir-args (ir einfo)) args
		  (ir-return-type (ir einfo)) returntype
		  (ir-defn (ir einfo)) lifted-actuals-body)
	    (ir-function-name (ir einfo)))))))
    

;;NSH(4-22-21): Modified this to return the ir since the theory-instance needs this.
(defun pvs2ir-constant (expr bindings)
  (let* ((decl (declaration expr)))
    (cond ((pvs2ir-primitive? expr) ;;borrowed from pvseval-update.lisp
	   (cond ((eq (id expr) 'TRUE) (mk-ir-bool t))
		 ((eq (id expr) 'FALSE) (mk-ir-bool nil))
		 (t (mk-ir-primitive-function (id expr) decl))));for primitives, types are derived from args
	  (t (let ((ir-defn (pvs2ir-constant-ir expr bindings decl)))
	       ;; (format t "~%pvs2ir-constant: ~a" (ir-fname (ir-function-name ir-defn)))
	       (ir-function-name ir-defn))))))

(defun pvs2ir-constant-ir (expr bindings decl)
  (if (adt-expr? expr)
      (let ((adt (adt expr)));(break "pvs2ir-constant-ir(adt)")
	(pvs2ir-adt adt)
	(ir (eval-info decl)))
    (let* ((theory (module decl))
	   (thinst (module-instance expr))
	   (actuals (actuals thinst))
	   (type-actuals (loop for act in actuals
			       when (type-value act)
			       collect (check-actual-type (type-value act) bindings)))
	   (nonref-actuals (loop for actlabel in type-actuals
				 when (not (eq actlabel 'ref))
				 collect actlabel))
	   (new-theory-id (format nil "~a_~{~a~^_~}" (simple-id (id theory))
				  type-actuals))
	   (intern-theory-id (intern new-theory-id :pvs))
	   ) ;(when nonref-actuals (break "nonref-actuals"))
      (cond ((and nonref-actuals (not (eq theory *current-pvs2c-theory*)))  ;; was (eq intern-theory-id *theory-id*)))
	     (let* ((*theory-id* intern-theory-id)
		    (monoclones (ht-instance-clone theory))
		    (dummy (when (null monoclones)(format t "~% No monoclones")))
		    (thclone (and monoclones (gethash  intern-theory-id monoclones)))
		    (theory-instance (or thclone
					 (let ((new-instance (subst-mod-params theory thinst)))
					   (setf (id new-instance) *theory-id*)
					   new-instance)))
		    (dummy2 (when thclone (format t "~%Found thclone")))
		    (instdecl (find  decl (theory theory-instance) :key #'generated-by))
		    ) ;(break "nonref-actuals")			;place information matches
	       (declare (ignore dummy dummy2))
	       (cond (thclone
		      (format t "~%Pushing ~a" intern-theory-id)
		      (pushnew theory-instance *preceding-mono-theories*)
		      (ir (eval-info instdecl)))
		     (t (unless monoclones (setf (ht-instance-clone theory)(make-hash-table :test #'eq)))
			(setf (gethash intern-theory-id (ht-instance-clone theory)) theory-instance)
			(pvs2c-theory-body-step theory-instance t nil)  ;;NSH(3/20/22): Need to clear inherited eval-info
			(format t "~%Pushing ~a" intern-theory-id)
			(pushnew theory-instance *preceding-mono-theories*)
			;; (if (memq theory *preceding-prelude-theories*)
			;; 	 (push theory-instance *preceding-prelude-theories*)
			;;   (push theory-instance *preceding-theories*))
			(ir (eval-info instdecl))))))
	    (t (ir (eval-info decl)))))))

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
					collect (cons (pvs2ir-unique-decl-id (con-decl con)) ;; constructor has no type
						      (loop for acc in (acc-decls con)
							    collect (mk-ir-fieldtype (pvs2ir-unique-decl-id acc)
										     (range (type acc)))))
				  ))))
	 (adt-type-name (mk-ir-typename adt-adt-id adt-enum-or-record-type *ir-theory-formals* nil adt-decl)));;need to add params/nil for now
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
  (declare (ignore adt-enum-type))
  (let* ((cdecl (con-decl constructor))
	 ;(args (arguments constructor))
	 (cid (pvs2ir-unique-decl-id cdecl))
	 (einfo (get-eval-info cdecl)))
;    (unless (ir einfo))
      (setf (ir einfo) (make-instance 'ir-constructor-defn))
      (setf (ir-constructor-type (ir einfo)) adt-type-name)
      (setf (ir-function-name (ir einfo))
	    (mk-ir-function cid cdecl)
	    (ir-defn (ir einfo))
	    (mk-ir-integer index))
      (pvs2ir-adt-enum-recognizer (rec-decl constructor) index index-id index-type
				   adt-type-name)))


(defun mk-ir-variables-from-args (args bindings accum)
  (cond ((consp args)
	 (let ((new-var (mk-ir-variable (new-irvar)(pvs2ir-type (type (car args)) bindings))))
	   (mk-ir-variables-from-args (cdr args)
				      (acons (car args) new-var bindings)
				      (cons new-var accum))))
	(t (nreverse accum))))
	 
(defun pvs2ir-adt-record-constructor (constructor index index-id index-type adt-recordtype adt-type-name)
  (let* ((cdecl (con-decl constructor))
	 (args (arguments constructor))
	 (cid (pvs2ir-unique-decl-id cdecl))
	 (einfo (get-eval-info cdecl))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    ;(unless (ir einfo);(break "constructor")
    (setf (ir einfo) (make-instance 'ir-constructor-defn)
	  (cdefn einfo) nil (c-type-info-table einfo) nil)
      (let* ((indexvar (mk-ir-variable (new-irvar) index-type))
	     (cargs (mk-ir-variables-from-args args nil nil))
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
				   (mk-ir-adt-constructor-recordtype cid cbody-field-types adt-type-name)
				 adt-recordtype));retain adt type for nullary constructors
	     (ctypename (if args (mk-ir-typename cid  cbody-recordtype nil nil cdecl) adt-type-name));;need to add params
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
	)))

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
  (let ((einfo (eval-info declaration)))
;    (when (and einfo (adt-accessor-decl? declaration)) (break "get-eval-info"))
    (cond (einfo (let ((new-einfo
			(if (adt-constructor-decl? declaration)
			    (change-class einfo 'constructor-eval-info)
			  (if (adt-accessor-decl? declaration)
			      (change-class einfo 'accessor-eval-info)
			    einfo))))
		   (setf (eval-info declaration) new-einfo)
		   new-einfo))
	  (t (let ((new-einfo (make-c-eval-info declaration)))
	       (setf (eval-info declaration) new-einfo)
	       new-einfo)))))

(defun pvs2ir-adt-enum-recognizer (rdecl index index-id index-type 
					 adt-type-name)
  (declare (ignore index-id))
  (let* ((einfo (get-eval-info rdecl))
	 (ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    (or ir-einfo
	(let* ((rid (pvs2ir-unique-decl-id rdecl))
	       (rarg  (mk-ir-variable (new-irvar) adt-type-name))
	       (index-var (mk-ir-variable (new-irvar) index-type))
	       (check-expr (mk-ir-apply (mk-ir-primitive-function '=) (list rarg index-var)))
	       (rbody (mk-ir-let index-var (mk-ir-integer index)
				 check-expr))
	       (recognizer-name (mk-ir-function (intern (format nil "r_~a" rid)) rdecl)))
	  (setf (ir einfo)(mk-ir-defn recognizer-name
				      (list rarg) '|bool| rbody)
		(cdefn einfo) nil
		(c-type-info-table einfo) nil)
	  (ir einfo)))))



(defun pvs2ir-adt-record-recognizer (rdecl index index-id index-type 
				    adt-type-name)
  (let* ((einfo (get-eval-info rdecl))
	 ;(ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
;    (or ir-einfo)
    (let* ((rid (pvs2ir-unique-decl-id rdecl))
	   (rargs (list (mk-ir-variable (new-irvar) adt-type-name)))
	   (index-expr (mk-ir-get (car rargs) index-id))
	   (index-expr-var (mk-ir-variable (new-irvar) index-type))
	   (index-var (mk-ir-variable (new-irvar) index-type))
	   (check-expr (mk-ir-apply (mk-ir-primitive-function '=) (list index-expr-var index-var)))
	   (rbody (mk-ir-let index-var (mk-ir-integer index)
			     (mk-ir-let index-expr-var index-expr
					check-expr)))
	   (recognizer-name (mk-ir-function (intern (format nil "r_~a" rid)) rdecl))
					;(rdefn (mk-ir-lambda rargs 'bool rbody))
	   )				;(break "recognizer")
      
      (setf (ir einfo)(mk-ir-defn recognizer-name rargs '|bool| rbody)
	    (cdefn einfo) nil
	    (c-type-info-table einfo) nil)
      (ir einfo))))



(defmethod pvs2ir-adt-accessor* ((adecl adt-accessor-decl)
				 constructor constructors index index-id adt-type-name)
  (declare (ignore index index-id constructors))
   (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 ;(ir-einfo (ir einfo))
	 (cinfo (get-eval-info (con-decl constructor)))
	 (ir-cinfo (ir cinfo))
	 (ctype (ir-constructor-type ir-cinfo))
	 (*var-counter* nil));(break "accessor*")
    (newcounter *var-counter*)
;    (or (ir einfo))
	(let* (;(aid (pvs2ir-unique-decl-id adecl))
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
				     (mk-ir-defn update-name (list aargvar new-value-var) ctype ubody))
		(cdefn einfo) nil (c-type-info-table einfo) nil (update-cdefn einfo) nil)
	  ;(format t "~%Adding definition for singular accessor: ~a" adecl-id)
	  (ir einfo))))
	  
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
  (declare (ignore index constructor))
  (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 ;(ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    ;(or ir-einfo)
    (let* ((acc-constructor-ids (constructors adecl))
	   (acc-constructor-index-decls (loop for cnstr in constructors
					      as idx from 0 
					      when (memq (id cnstr) acc-constructor-ids)
					      collect (cons idx (con-decl cnstr))))
					;(aid (pvs2ir-unique-decl-id adecl))
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
					;(format t "~%Adding definition for shared accessor: ~a" adecl-id)
      (ir einfo))))


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
		 (let ((condvar (mk-ir-variable (new-irvar) '|bool|))
		       (indvar (mk-ir-variable (new-irvar) '|uint32|))
		       (intvar (mk-ir-variable (new-irvar) '|uint32|)))
		   (mk-ir-let condvar
			      (mk-ir-let indvar
					 (mk-ir-get aargvar index-id)
					 (mk-ir-let intvar (mk-ir-integer cindex)
						    (mk-ir-apply (mk-ir-primitive-function '=) (list indvar intvar))))
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
		 (let ((condvar (mk-ir-variable (new-irvar) '|bool|))
		       (indvar (mk-ir-variable (new-irvar) '|uint32|))
		       (intvar (mk-ir-variable (new-irvar) '|uint32|)))
		   (mk-ir-let condvar
			      (mk-ir-let indvar
					 (mk-ir-get aargvar index-id)
					 (mk-ir-let intvar (mk-ir-integer cindex)
						    (mk-ir-apply (mk-ir-primitive-function '=) (list indvar intvar))))
			      (mk-ir-ift condvar
					 cbranch
					 (pvs2ir-accessor-update-body adecl-id aargvar new-value-var (cdr acc-constructor-index-decls) index-id))))
	       cbranch)))
	  (t (format t "Shouldn't reach here."))))

(defmethod pvs2ir-constructor-recordtype ((constructor ir-adt-constructor))
  (let* ((index (intern "_index"))
	 (index-fieldtype (mk-ir-fieldtype index
					   (mk-ir-subrange (ir-adt-constructor-index constructor)
							   (ir-adt-constructor-index constructor))))
	 (rest-fieldtypes (loop for accessor in (ir-adt-accessors constructor)
				collect (mk-ir-fieldtype (ir-adt-accessor-id accessor)
							 (ir-adt-accessor-type accessor)))))
    (mk-ir-recordtype (cons index-fieldtype rest-fieldtypes) (gensym (string (id constructor))))));NSH(4-3-22): label added

(defmethod ir-formal-id ((ir-formal ir-formal-typename))
  (with-slots (ir-type-id) ir-formal
	ir-type-id))

(defmethod ir-formal-id ((ir-formal ir-const-formal))
  (with-slots (ir-name) ir-formal
	ir-name))

(defparameter *type-actual-ir-name* (mk-ir-typename '|type_actual| nil nil nil nil))

(defmethod ir-formal-type ((ir-formal ir-formal-typename))
  *type-actual-ir-name*)

(defmethod ir-formal-type ((ir-formal ir-const-formal))
  (with-slots (ir-vtype) ir-formal
	ir-vtype))

(defun ir-apply-op-type (pvsop argtypes atype)
  (if (pvs2ir-primitive? pvsop)
      (let ((argtyp1 (ir-type-def (car argtypes)))
	    (argtyp2 (ir-type-def (cadr argtypes))))
	(if (or (eq argtyp1 '|mpq|)
		(eq argtyp2 '|mpq|))
	    (or atype '|mpq|)
	  (let ((new-subrange-type
		 (case (id pvsop)
		   (+ (plus-subrange  argtyp1
				      argtyp2))
		   (- (if (and (< (length argtypes) 2)
			       (ir-subrange? argtyp1))
			  (negate-subrange argtyp1)
			(minus-subrange argtyp1
					argtyp2)))
		   (* (times-subrange argtyp1
				      argtyp2))
		   (t))))
					;(format t "~%new-subrange-type : ~a" (print-ir new-subrange-type))
	    (if new-subrange-type
		(if atype
		    (intersect-subrange new-subrange-type atype)
		  new-subrange-type)
	      atype))))
    atype))

(defmethod get-ir-domain-types ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
    (get-ir-domain-types ir-type-defn)))

(defmethod get-ir-domain-types ((ir-type ir-funtype))
  (with-slots (ir-domain) ir-type
    (if (ir-tupletype? ir-domain)
	(loop for irft in (ir-field-types ir-domain)
	      collect (ir-vtype irft))
      (list ir-domain))))

(defmethod get-ir-domain-types ((ir-type ir-arraytype))
  (with-slots (size) ir-type
    (list (mk-ir-subrange 0 (1- size)))))

(defmethod mk-variables-from-ir-domain-types ((ir-type ir-typename) bindings)
  (with-slots (ir-type-defn) ir-type
    (mk-variables-from-ir-domain-types ir-type-defn bindings)))

(defmethod mk-variables-from-ir-domain-types ((ir-type ir-funtype) bindings)
  (with-slots (ir-domain) ir-type
    (let* ((dtype (if (ir-fieldtype? ir-domain) (ir-vtype ir-domain) ir-domain))
	   (dtypes (if (ir-tupletype? dtype) (ir-field-types dtype) (list dtype))))
      (mk-variables-from-ir-types dtypes bindings))))

(defmethod mk-variables-from-ir-domain-types ((ir-type ir-arraytype) bindings)
  (declare (ignore bindings))
  (with-slots (size) ir-type
    (list (mk-ir-variable (new-irvar) (mk-ir-subrange 0 (1- size))))))

(defun mk-variables-from-ir-types (ir-types bindings)      
  (cond ((consp ir-types)
	 (let* ((typ1 (car ir-types))
		(vtyp1 (if (ir-fieldtype? typ1)(ir-vtype typ1) typ1))
		(rtyp1 (rename-type vtyp1 bindings))
		(ir-var1 (mk-ir-variable (new-irvar) rtyp1))
		(new-bindings (if (ir-fieldtype? typ1) (acons typ1 ir-var1 bindings) bindings)))
	   (cons ir-var1 (mk-variables-from-ir-types (cdr ir-types) new-bindings))))
	(t nil)))

(defun mk-variables-from-types (types bindings)
  (cond ((consp types)
	 (let* ((typ1 (car types))
		(ir-var1 (mk-ir-variable (new-irvar)(pvs2ir-type typ1 bindings)))
		(new-bindings (if (binding? typ1) (acons typ1 ir-var1 bindings) bindings)))
	   (cons ir-var1 (mk-variables-from-types (cdr types) new-bindings))))
	(t nil)))

(defun pvs2ir-primitive-application (op arg-names op-arg-types args-ir ir-expr-type bindings)
  (let ((ir-op (pvs2ir-constant op bindings)))
    (with-slots (ir-fname) ir-op
      (let ((arg-var-type1 (mk-ir-variable (car arg-names)(car op-arg-types))))
	(case ir-fname
	  ((AND & ∧)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (cadr args-ir)
				 (mk-ir-bool nil))))
	  ((OR ∨)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (mk-ir-bool t)
				 (cadr args-ir))))
	  ((IMPLIES => ⇒)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (cadr args-ir)
				 (mk-ir-bool t))))
	  ((restrict) (car args-ir))
	(t (let ((arg-vartypes (loop for ir-var in arg-names
				     as ir-typ in op-arg-types
				     collect (mk-ir-variable ir-var ir-typ))))
	     (mk-ir-let* arg-vartypes
			 args-ir
			 (mk-ir-apply (pvs2ir-constant op bindings) arg-vartypes nil ir-expr-type)))))))))

(defun pvs2ir-application (op args ir-expr-type bindings expected)
  (declare (ignore expected))
  (let* ((arg-names (new-irvars (length args)))
	 (args-ir (pvs2ir* args bindings nil))
	 (arg-types 
	  (loop for arg in args
		as ir-arg in args-ir
		collect (let ((num (pvs-integer? arg)))
			  (if num
			      (mk-ir-subrange num num)
			      (best-ir-subrange-pair (pvs2ir-expr-type arg bindings)
						     (ir-arith-type ir-arg)))))) ;;NSH(3-20-16):
	 (op-arg-types arg-types)
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
	 )				;(break "pvs2ir-application")
					;(format t "~%op: ~a: ~{ ~a~}" (when (constant? op)(id op))(print-ir op-arg-types))
    (if (constant? op)
	(if (pvs2ir-primitive? op)
	    (pvs2ir-primitive-application op arg-names op-arg-types args-ir ir-expr-type bindings)
	    (let* ((opdecl (declaration op))
		   (theory (module opdecl)))
	      (when (memq (id theory) *primitive-prelude-theories*) ;;NSH(6-16-21)
		(pvs2c-err "Non-executable theory for constant: ~a.~a" (id theory) (id op)))
	      (let* ((formals (formals-sans-usings (module opdecl)))
		     (actuals (actuals (module-instance op))) ;;handling theory actuals
		     (ref-actuals (loop for act in actuals ;;collect const actuals and ref actuals
					when (or (null (type-value act))
						 (eq (check-actual-type (type-value act) bindings) 'ref))
					collect act))
		     (ref-formals (if actuals ;if there are actuals, then collect only the type formals 
				      (loop for fml in formals ;where the matching actual has a ref type
					;other non-ref type actuals are monomorphized
					    as act in actuals ;;collect const actuals and ref actuals
					    when (or (null (type-value act))
						     (eq (check-actual-type (type-value act) bindings) 'ref))
					    collect fml)
				      formals)) ;otherwise, return all the formals. 
		     (ir-formals (pvs2ir* ref-formals bindings nil))
		     (ir-actuals (pvs2ir* ref-actuals bindings nil))
		     (actvars (loop for fml in ir-formals
				    as formal in ref-formals
				    collect (mk-ir-variable (new-irvar)(ir-formal-type fml) (id formal))))
		     ;; (if ref-actuals 
		     ;;     (loop for fml in ir-formals
		     ;; 	  as formal in ref-formals
		     ;; 	  collect (mk-ir-variable (new-irvar)(ir-formal-type fml) (id formal)))
		     ;; (loop for fml in formals collect (get-assoc fml bindings)))
		     (actual-types
		      (loop for actual in ref-actuals
			    as formal in ref-formals
			    collect (if (formal-const-decl? formal)
					(pvs2ir-expr-type (expr actual) bindings) ; was (pvs2ir-type (type (expr..)))
					*type-actual-ir-name*)))
		     (op-domain-vars (mk-variables-from-types (types (domain (find-supertype (type op)))) bindings))
		     ;; (let ((ir-args (ir-args (ir (eval-info opdecl)))))
		     ;; 		    (if ir-args
		     ;; 			(loop for ir-var in ir-args
		     ;; 			      when (not (ir-const-formal? ir-var))
		     ;; 			      collect (mk-ir-variable (new-irvar)(ir-vtype ir-var)))
		     ;; 		      (mk-variables-from-types (types (domain (find-supertype (type op)))) bindings)))
					;		       (op-domain-vars (mk-variables-from-ir-types op-domain));(mk-variables-from-types op-domain bindings)
		     (op-ir (pvs2ir-constant-ir op bindings opdecl))
		     (op-ir-function (ir-function-name op-ir))
		     (op-ir-defn (ir-defn op-ir))
		     (op-ir-args (ir-args op-ir))
		     (op-arg-application-ir
		      (make-ir-lett* op-domain-vars ; was arg-vartypes
				     arg-types
				     args-ir
				     (mk-ir-let apply-return-var ;op-range-type
						(mk-ir-apply
						 op-ir-function op-domain-vars
						 actvars)
					;op-range-type
						apply-return-var)))
		     )			;(break "pvs2ir-application")
		(if formals
		    (if ir-actuals ;;then generating code outside theory
			(make-ir-lett* actvars actual-types ir-actuals
				       op-arg-application-ir)
			(make-ir-let* actvars ir-formals op-arg-application-ir)) ;;generating code within theory
		  
		    (if (and op-ir-defn  args-ir (null op-ir-args))
			(let ((op-var (mk-ir-variable (new-irvar)(pvs2ir-type (type op) bindings))))
					;(break "pvs2ir-application")
			  (make-ir-lett* op-domain-vars ;was arg-vartypes
					 arg-types
					 args-ir
					 (make-ir-let op-var
						      op-ir-function
						      (mk-ir-let apply-return-var ;op-range-type
								 (mk-ir-apply op-var op-domain-vars nil) ;op-range-type
								 apply-return-var))))
			(make-ir-lett* op-domain-vars ;was arg-vartypes
				       arg-types
				       args-ir
				       (mk-ir-let apply-return-var ;op-range-type
						  (mk-ir-apply op-ir-function op-domain-vars nil) ;op-range-type
						  apply-return-var))))))))
    (let* ((op-ir-type (pvs2ir-type (type op) bindings))
	   (op-var (new-irvartype op-ir-type))
	   (op-ir (pvs2ir* op bindings nil)) ; expected is nil 
	   (arg-vartypes (mk-variables-from-ir-domain-types op-ir-type nil))) ; (break "pvs2ir-application-2")
      ;;	  (when (not (eql (length arg-vartypes)(length arg-types))) (break "pvs2-ir-application: arg-vartypes: ~s, ~% argtypes: ~s" arg-vartypes arg-types))
      (if (ir-array? op-ir-type)
	  (mk-ir-let op-var op-ir
		     (mk-ir-let (car arg-vartypes)(car args-ir)
				(mk-ir-let apply-return-var ;op-range-type
					   (mk-ir-lookup op-var (car arg-vartypes))
					   apply-return-var)))
	  (if (ir-lambda? op-ir) ;;op-var is ignored, IR is beta-reduced
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
	      (make-ir-let op-var op-ir 
			   (make-ir-lett* arg-vartypes
					  arg-types
					  args-ir
					  (mk-ir-let apply-return-var ;op-range-type
						     (mk-ir-apply op-var arg-vartypes nil) ;op-range-type
						     apply-return-var))))))))
	  
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

(defmethod pvs2ir* ((expr list) bindings expected)
  (cond ((consp expr)
	 (cons (pvs2ir* (car expr) bindings (car expected))
	       (pvs2ir* (cdr expr) bindings (cdr expected))))
	(t nil)))

(defmethod pvs2ir* ((cexpr cases-expr) bindings expected)
  (with-slots (expression selections else-part) cexpr
    (let ((expression-var (make-variable-expr (make-bind-decl (gentemp "cx") (type expression))))
	  (expression-ir-var (mk-ir-variable (new-irvar)(pvs2ir-type (type expression) bindings)))
	  (expression-ir (pvs2ir* expression bindings nil)))
      (mk-ir-let expression-ir-var expression-ir
	      (pvs2ir-cases expression-var expression-ir-var selections else-part bindings expected)))))

(defun pvs2ir-cases (expression-var expression-ir-var selections else-part bindings expected)
  (cond ((consp selections)
	 (let* ((sel (car selections))
		(selexpr (expression sel))
		(stype (find-declared-adt-supertype (type expression-var)))
		(dec-stype (declaration stype))
		(modinst (module-instance stype))
		(accs (subst-mod-params (accessors (constructor sel))
					modinst
					(module dec-stype)
					dec-stype))
		(accs-exprs (loop for acc in accs collect
				  (make!-application acc expression-var)))
		(arg-ir-vars (new-irvars (length (args sel))))
		(args-ir-vtypes (loop for ir-var in arg-ir-vars
				      as var in (args sel)
				      collect (mk-ir-variable ir-var (pvs2ir-type (type var) bindings))))
		(ir-accs (pvs2ir* accs-exprs (acons expression-var expression-ir-var bindings) nil))
		(branch (mk-ir-let* args-ir-vtypes
				    ir-accs
				    (pvs2ir* selexpr
					     (nconc (pairlis (args sel) args-ir-vtypes) bindings)
					     expected))))
	   (if (and (null (cdr selections))(null else-part))
	       branch
	     (let ((condvar (mk-ir-variable (new-irvar) '|bool|)))
	       (mk-ir-let condvar
			  (pvs2ir* (make!-application (recognizer (constructor sel))
						    expression-var)
				 (acons expression-var expression-ir-var bindings) nil)
			(mk-ir-ift  condvar
			    branch
			    (pvs2ir-cases expression-var expression-ir-var
					  (cdr selections) else-part bindings expected)))))))
	(t (pvs2ir* else-part bindings expected))))
		
;;  (pvs2ir* (translate-cases-to-if cexpr) bindings))

;;partial 
  ;; (with-slots (expr selections else-part) cexpr
  ;; 	      (let* ((adt (find-supertype (type expr)))
  ;; 		     (ir-selections (pvs2ir-selections selections adt))
  ;; 		     (ir-switch-else (pvs2ir* else-part bindings)))))
		
	      
(defmethod pvs2ir* ((expr if-expr) bindings expected)
  (cond ((branch? expr)
	 (if (last-cond-expr? expr)
	     (pvs2ir* (then-part expr) bindings expected)
	   (let ((ifvar (mk-ir-variable (new-irvar) '|bool|))
		 (cond-ir (pvs2ir* (condition expr) bindings nil))
		 (then-ir (pvs2ir* (then-part expr) bindings expected))
		 (else-ir (pvs2ir* (else-part expr) bindings expected)))
	     (mk-ir-let  ifvar cond-ir
			 (mk-ir-ift ifvar then-ir else-ir)))))
	(t (call-next-method))))

(defmethod pvs2ir* ((expr let-expr) bindings expected)
  (let* ((let-bindings (bindings (operator expr)))
	 (args (arguments expr))
	 (args (if (= (length let-bindings)(length args)) args
		 (loop for bnd in let-bindings
		       as i from 1
		       collect (make-projection-application i (car args)))))
	(expression (expression (operator expr))))
    (pvs2ir-let-expr let-bindings args expression bindings expected)))

(defun pvs2ir-let-expr (let-bindings args expression bindings expected)
  ;;deal with simple let-expressions first, and pattern matching lets later.
  (cond ((consp let-bindings)
	 (let* ((var1 (car let-bindings))
		(ir-var (new-irvar))
		(type1 (type var1))
		(ir-type (pvs2ir-type type1 bindings))
		(ir-vartype (mk-ir-variable ir-var ir-type (id var1)));;NSH(1/17/20)
		(ir-bind-expr (pvs2ir* (car args) bindings type1))
		(ir-body (pvs2ir-let-expr (cdr let-bindings) (cdr args)
				       expression
				       (acons var1
					      ir-vartype
					      bindings)
				       expected)));(break "pvs2ir(let-expr)")
	   (mk-ir-let ir-vartype ir-bind-expr ir-body)
	   ;; (if (or (ir-subrange? ir-type)
	   ;; 	   (ir2c-tequal ir-type ir-arg-type))
	   ;;     (mk-ir-let ir-vartype ir-bind-expr ir-body)
	   ;;   (let* ((arg-var (new-irvar))
	   ;; 	    (arg-vartype (mk-ir-variable arg-var ir-arg-type)))
	   ;;   (make-ir-let  arg-vartype
	   ;; 		 ir-bind-expr
	   ;; 		 (mk-ir-lett ir-vartype ir-arg-type arg-vartype ir-body))))
	   ));;ir-type==>ir-var-type
	(t (pvs2ir* expression bindings expected))))

(defun ir-type-name-with-actuals (declaration ir-actuals)
  (lcopy (ir-type-name (ir-type-value declaration))
	 'ir-actuals ir-actuals))

;;Need to handle bindings in the expected.  
(defmethod pvs2ir* ((expr tuple-expr) bindings expected)
  (let* ((expressions (exprs expr))
	 (ir-assignments (pvs2ir*  expressions bindings (and expected (types (find-supertype expected)))))
	 (ir-field-vars (new-irvars (length expressions)))
	 (ir-field-types (pvs2ir-type (mapcar #'type expressions) bindings))
	 (ir-field-var-types (mk-vartype-list  ir-field-vars
					       ir-field-types))
	 (ir-fields (loop for i from 1 to (length expressions)
			  as ir-var in ir-field-var-types
			  collect
			  (mk-ir-field (intern (format nil "project_~a" i))
				       ir-var)))
	 ;;removing print-type as expected since it doesn't work with actuals
	 ;; (print-type-expected (print-type expected))
	 ;; (ir-actuals (loop for act in (actuals print-type-expected)
	 ;; 		   collect (if (type-value act)
	 ;; 			       (mk-ir-type-actual (pvs2ir-type (type-value act) bindings))
	 ;; 			     (mk-ir-const-actual (pvs2ir* act bindings nil)))))
	 ;; (ir-expected (and (type-name? print-type-expected)
	 ;; 		   (ir-type-name-with-actuals  (declaration print-type-expected)
	 ;; 					       ir-actuals)))
	 (ir-recordtype ;(or ir-expected)
	  (mk-ir-tupletype (loop for type in ir-field-types
				 as field in ir-fields
				 collect (mk-ir-fieldtype (ir-fieldname field) type)))))
  (mk-ir-let* ir-field-var-types ir-assignments
	      (mk-ir-record ir-fields ir-recordtype))))


(defmethod pvs2ir* ((expr record-expr) bindings expected);(break "pvs2ir*(record-expr)")
  (let* ((expected-supertype (and expected (find-supertype expected)))
	 (sorted-fields (and expected
			     (sort-fields (fields expected-supertype)
					  (dependent-fields? (fields expected-supertype))))))
    (pvs2ir-fields (sort-record-assignments (assignments expr) sorted-fields) bindings
		   sorted-fields)))


(defun mk-vartype-list (vars types)
  (cond ((consp vars)(cons (mk-ir-variable (car vars)(car types))
			   (mk-vartype-list (cdr vars)(cdr types))))
	(t nil)))

(defmethod pvs2ir-expr-type ((expr int-expr) &optional bindings)
  (declare (ignore bindings))
  (mk-ir-subrange (number expr)(number expr)))

(defmethod pvs2ir-expr-type ((expr number-expr) &optional bindings)
  (declare (ignore bindings))
  (mk-ir-subrange (number expr)(number expr)))

(defmethod pvs2ir-expr-type ((expr name-expr) &optional bindings)
  (let ((entry (assoc (declaration expr) bindings)))
    (if (and entry (ir-variable? (cdr entry)))
	(ir-vtype (cdr entry))
      (call-next-method))))

(defun sort-record-assignments (assignments sorted-fields)
  (if sorted-fields
      (loop for fld in sorted-fields
	    collect (find (id fld) assignments :key #'(lambda (x)(caar (arguments x)))
			  :test #'same-id))
    (sort-assignments assignments)))

(defmethod pvs2ir-expr-type ((expr record-expr) &optional bindings)
  (with-slots (assignments) expr
	      (mk-ir-recordtype (pvs2ir-expr-type (sort-assignments assignments) bindings))));no label/not dependent

(defmethod pvs2ir-expr-type ((expr tuple-expr) &optional bindings)
  (with-slots (exprs) expr
	      (mk-ir-tupletype (loop for ir-type in (pvs2ir-expr-type exprs bindings)
				      as i from 1
				      collect (mk-ir-fieldtype (intern (format nil "project_~a" i))
							       ir-type)))))

(defmethod pvs2ir-expr-type ((expr  list) &optional bindings)
  (cond ((consp expr) (cons (pvs2ir-expr-type (car expr) bindings)
			    (pvs2ir-expr-type (cdr expr) bindings)))
	(t nil)))

(defmethod pvs2ir-expr-type ((expr uni-assignment) &optional bindings)
  (with-slots (arguments expression) expr
	      (mk-ir-fieldtype (id (caar arguments))
				(pvs2ir-expr-type expression bindings))))

(defmethod pvs2ir-expr-type ((expr t) &optional bindings)
  (let ((val (pvs-integer? expr)))
    (if val (mk-ir-subrange val val)
      (if (and (application? expr)
	       (pvs2ir-primitive? (operator expr))
	       (memq (id (operator expr)) '(+ - * /)))
	  (best-ir-subrange (type expr) (judgement-types expr) bindings)
	(if (funtype? (type expr))
	    (best-ir-subrange (type expr)(judgement-types+ expr) bindings)
	  (pvs2ir-type (type expr) bindings))))))

;Recurses through assignments a1 := e1, .., an := en
(defun pvs2ir-field-assignments (assignments bindings expected 
					     accum-ir-fieldvars
					     accum-ir-assignments
					     accum-ir-expr-types
					     accum-ir-fields
					     accum-ir-fieldtypes)
  (cond ((consp assignments)
	 (let* ((assgn1 (car assignments))
		(expr1 (expression assgn1))
		(ir-expr-type (pvs2ir-expr-type expr1 bindings))
		(expec1 (car expected))
		(ir-assgn1 (pvs2ir* expr1 bindings (and expected (type expec1))))
		(ir-fieldvar (new-irvar))
		(ir-field-type (if expected (pvs2ir-type  (type expec1) bindings)
				 ir-expr-type))
		(ir-field-vartype (mk-ir-variable ir-fieldvar ir-field-type))
		(ir-field (mk-ir-field (id (caar (arguments assgn1))) ir-field-vartype))
		(ir-record-fieldtype (mk-ir-fieldtype (ir-fieldname ir-field) ir-field-type))
		(new-bindings (if expected (acons expec1 ir-field-vartype bindings)
				bindings)))
	   
	   (pvs2ir-field-assignments (cdr assignments) new-bindings (cdr expected) 
				     (cons ir-field-vartype accum-ir-fieldvars)
				     (cons ir-assgn1 accum-ir-assignments)
				     (cons ir-expr-type accum-ir-expr-types)
				     (cons ir-field accum-ir-fields)
				     (cons ir-record-fieldtype accum-ir-fieldtypes))))
	(t ;; (let* ((print-expected-record-type (and expected-record-type (print-type expected-record-type)))
	   ;; 	  (ir-actuals (loop for act in (actuals print-expected-record-type)
	   ;; 			    collect (if (type-value act)
	   ;; 					(mk-ir-type-actual (pvs2ir-type (type-value act) bindings))
	   ;; 				      (mk-ir-const-actual (pvs2ir* act bindings nil)))))
	   ;; 	  (ir-expected (and (type-name? print-expected-record-type)
	   ;; 			    (ir-type-name-with-actuals (declaration print-expected-record-type)
	   ;; 						       ir-actuals)))))
	 (mk-ir-lett* (nreverse accum-ir-fieldvars)(nreverse accum-ir-expr-types)
		      (nreverse accum-ir-assignments)
		      (mk-ir-record (nreverse accum-ir-fields)
				    ;(or ir-expected)
				    (mk-ir-recordtype (nreverse accum-ir-fieldtypes)))))));no label
  

(defun pvs2ir-fields (assignments bindings expected)
  (pvs2ir-field-assignments assignments bindings expected  nil nil nil nil nil))
  ;; (let* ((expressions (mapcar #'expression assignments))
  ;; 	 (expected-field-types (mapcar #'type expected))
  ;; 	 (ir-assignments (pvs2ir*  expressions bindings expected-field-types));ignoring dependencies
  ;; 	 (ir-field-vars (new-irvars (length assignments)))
  ;; 	 (ir-field-types (if expected
  ;; 			     (loop for eft in expected-field-types
  ;; 				   collect (pvs2ir-type eft bindings))
  ;; 			   (loop for expr in expressions
  ;; 				   collect (pvs2ir-expr-type expr bindings))))
  ;; 	 (ir-field-vartypes (mk-vartype-list ir-field-vars ir-field-types))
  ;; 	 (ir-fields (loop for assignment in assignments
  ;; 			  as ir-field-vartype in ir-field-vartypes
  ;; 			  collect
  ;; 			  (mk-ir-field (id (caar (arguments assignment)))
  ;; 				       ir-field-vartype)))
  ;; 	 (ir-recordtype (mk-ir-recordtype (loop for field in ir-fields
  ;; 						as type in ir-field-types
  ;; 						collect (mk-ir-fieldtype (ir-fieldname field) type)))))
  ;;   (break "pvs2ir-fields")
  ;; (mk-ir-let* ir-field-vartypes ir-assignments
  ;; 	      (mk-ir-record ir-fields ir-recordtype)))


(defmethod pvs2ir* ((expr field-application) bindings expected)
  (declare (ignore expected))
  (with-slots (id argument) expr
	      (let ((ir-argument (pvs2ir* argument bindings nil));don't know the exact record type of argument
		    (argvar (mk-ir-variable (new-irvar)(pvs2ir-type (type argument) bindings))))
		(mk-ir-let argvar
			   ir-argument
			   (mk-ir-get argvar id)))))

(defmethod pvs2ir* ((expr projection-application) bindings expected)
  (declare (ignore expected))  
  (with-slots (index argument) expr
	      (let ((ir-argument (pvs2ir* argument bindings nil))
		    (argvar (mk-ir-variable (new-irvar)(pvs2ir-type (type argument) bindings))))
		(mk-ir-let argvar
			   ir-argument
			   (mk-ir-get argvar (intern (format nil "project_~a" index)))))))

(defmethod pvs2ir* ((expr update-expr) bindings expected)
  (with-slots (type expression assignments) expr
	      (let ((ir-expression (pvs2ir* expression bindings expected)))
		(pvs2ir-update assignments ir-expression
			       (pvs2ir-type (or expected (type expression)) bindings)
			       bindings))))

(defmethod pvs2ir* ((expr forall-expr) bindings expected)
  (with-slots ((expr-bindings bindings) expression) expr
    (let* ((ir-bindings (pvs2ir-lambda-bindings expr-bindings bindings))
	   (ir-var-bindings (pairlis expr-bindings ir-bindings))
	   (in-bindings (append ir-var-bindings bindings))
	   (ir-expr (pvs2ir* expression in-bindings expected)))
      (pvs2ir-forall-expr ir-bindings ir-expr))))

(defmethod pvs2ir* ((expr exists-expr) bindings expected)
  (with-slots ((expr-bindings bindings) expression) expr
    (let* ((ir-bindings (pvs2ir-lambda-bindings expr-bindings bindings))
	   (ir-var-bindings (pairlis expr-bindings ir-bindings))
	   (in-bindings (append ir-var-bindings bindings))
	   (ir-expr (pvs2ir* expression in-bindings expected)))
      (pvs2ir-exists-expr ir-bindings ir-expr))))

(defun check-quantifier-subrange (ir-low ir-high)
  (and (integerp ir-low)
       (integerp ir-high)
       (and (>= ir-low 0)(< ir-high (expt 2 32)))))

(defun pvs2ir-forall-expr (bindings expr)
  (cond (bindings
	 (let* ((bnd1 (car bindings))
		(rest (cdr bindings))
		(bnd1type (ir-vtype bnd1)))
	   (cond ((and (ir-subrange? bnd1type)
		       (check-quantifier-subrange (ir-low bnd1type)(ir-high bnd1type)))
		  (mk-ir-forall bnd1 (pvs2ir-forall-expr rest expr)))
		 (t (pvs2c-err "Quantifiers are not handled")
		    ;;(mk-ir-function 'u_undef_quant_expr)
		    ))))
	(t expr)))

(defun pvs2ir-exists-expr (bindings expr)
  (cond (bindings
	 (let* ((bnd1 (car bindings))
		(rest (cdr bindings))
		(bnd1type (ir-vtype bnd1)))
	   (cond ((and (ir-subrange? bnd1type)
		       (check-quantifier-subrange (ir-low bnd1type)(ir-high bnd1type)))
		  (mk-ir-exists bnd1 (pvs2ir-exists-expr rest expr)))
		 (t (pvs2c-err "Quantifiers are not handled")
		    ;; (mk-ir-function 'u_undef_quant_expr)
		    ))))
	(t expr)))

(defmethod pvs2ir* ((expr quant-expr) bindings expected)
  (declare (ignore bindings expected))
  (pvs2c-err "Quantifiers not allowed:~%  ~a" expr)
  (mk-ir-function 'u_undef_quant_expr));;using a dummy name

;;gets the type of component of nested arrays/records being updated.
;;This is used to get the right-hand side type of an update expression.
(defmethod get-component-ir-type ((ir-expr-type ir-funtype) lhs-args)
  (with-slots (ir-domain ir-range) ir-expr-type
	      (cond ((consp lhs-args)
		     (get-component-ir-type ir-range (cdr lhs-args)))
		    (t ir-expr-type))))


;;same as ir-funtype above
(defmethod get-component-ir-type ((ir-expr-type ir-arraytype) lhs-args)
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
				       (ir-vtype (find field-id ir-field-types :key #'ir-id))))))
		       (get-component-ir-type ir-field-type (cdr lhs-args))))
		    (t ir-expr-type))))

(defmethod get-component-ir-type ((ir-expr-type ir-typename) lhs-args)
  (cond ((consp lhs-args)
	 (with-slots (ir-type-id ir-type-defn) ir-expr-type
	   (get-component-ir-type ir-type-defn lhs-args)))
	(t ir-expr-type)))

(defmethod get-component-ir-type ((ir-expr-type t) lhs-args)
  (declare (ignore lhs-args))
  ;;lhs-args should be empty
  ir-expr-type)

;;evaluates the right-hand sides; creates bindings; and then constructs the
;;updates over the assignments
(defun pvs2ir-update (assignments ir-expression expression-type bindings)
  (let ((maplet (find-if #'maplet? assignments)))
    (when maplet
      (pvs2c-err "Maplets not supported: ~a" maplet)))
  (let ((rhs-list (mapcar #'expression assignments))
	(lhs-list (mapcar #'arguments assignments)))
    (let* ((rhs-irvar-list (loop for i from 1 to (length rhs-list)
				 collect (new-irvar)))
	   (ir-rhs-types (loop for lhs in lhs-list
			       collect (get-component-ir-type expression-type lhs)))
	   (ir-rhs-vartypes (mk-vartype-list rhs-irvar-list ir-rhs-types))
	   (ir-rhs-list (loop for rhs in rhs-list
			      collect
			      (pvs2ir* rhs bindings nil)))
	   (ir-exprvar (mk-ir-variable (new-irvar) expression-type))) ;binds the ir-expression value
					;(break "pvs2ir-update")
      (let ((ir-update-expr (pvs2ir-assignments lhs-list ;build updates
						ir-rhs-vartypes
						ir-exprvar
						expression-type
						bindings)))
	(make-ir-let* ir-rhs-vartypes ir-rhs-list
		      (make-ir-let ir-exprvar 
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
		(lhs1 (if (singleton? (car lhs))
			  (caar lhs)
			(make!-tuple-expr* (car lhs)))))
	   (let ((lhs1-irvar (new-irvar))
		 (lhs1-ir (pvs2ir* lhs1 bindings nil)) ;;the index expression
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

(defmethod pvs2ir-assignment1 ((ir-expr-type ir-arraytype) lhs rhs-irvar ir-exprvar bindings)
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
		 (lhs1-ir (pvs2ir* lhs1 bindings nil)) ;;the index expression
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
		  (ir-expr-type11 (ir-vtype ir-field-decl)) ;;(pvs2ir-type (range (type lhs1)))
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
;;whereas in the record case, the field-assign does not have a type so that ir-expr-type11 has to
;;be computed from the ir-type of the field.  
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-adt-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (cond ((consp lhs)
	   (let* ((lhs1 (caar lhs));;lhs1 is a field-name-expr
		  ;(ir-field-decl (find (id lhs1) (ir-field-types ir-expr-type) :key #'ir-id))
		  (ir-expr-type11 (pvs2ir-type (range (type lhs1)) bindings)) ;;(ir-ftype ir-field-decl)
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

;;Actual type parameters can only be int/uint bool, _8 _16 _32 _64, nat, int, rat, array/function, record, tuple, adt.
;;For the reference types (array/function, record, tuple, adt), we generate polymorphic code.
;;For the numeric types, we create monomorphic clones and generate code for these as these instances
;; are encountered.  We need a predicate that checks that a given actual type is one of the above, and
;;returns a canonical representation of the type and a tag that is used to generate the theory instance name.  

(defmethod check-actual-type ((type arraytype) bindings)
  (declare (ignore bindings))
  (values 'ref type))

(defmethod check-actual-type ((type funtype) bindings)
  (declare (ignore bindings))  
  (values 'ref type))

(defmethod check-actual-type ((type tupletype) bindings)
  (declare (ignore bindings))  
  (values 'ref type))

(defmethod check-actual-type ((type recordtype) bindings)
  (declare (ignore bindings))  
  (values 'ref type))

(defmethod check-actual-type ((type subtype) bindings)
  (if (subtype-of? type *number*)
      (with-slots (ir-low ir-high)
	  (pvs2ir-subrange-index type bindings)
	(cond ((and (integerp ir-low)(>= ir-low 0)(integerp ir-high))
	       (cond ((< ir-high (expt 2 8))
		      (with-context :prelude (values '|uint8| (tc-type "integertypes.uint8"))))
		     ((< ir-high (expt 2 16))
		      (with-context :prelude (values '|uint16| (tc-type "integertypes.uint16"))))
		     ((< ir-high (expt 2 32))
		      (with-context :prelude (values '|uint32| (tc-type "integertypes.uint32"))))
		     ((< ir-high (expt 2 64))
		      (with-context :prelude (values (tc-type "integertypes.uint64") '|uint64|)))
		     (t (values  '|mpz| *naturalnumber*))))
	      ((and (integerp ir-low)(integerp ir-high))
	       (cond ((and (>= ir-low (expt 2 7))(< ir-high (expt 2 7)))
		      (with-context :prelude (values  '|int8| (tc-type "integertypes.int8"))))
		     ((and (>= ir-low (expt 2 15))(< ir-high (expt 2 15)))
		      (with-context :prelude (values  '|int16| (tc-type "integertypes.int16"))))
		     ((and (>= ir-low (expt 2 31))(< ir-high (expt 2 31)))
		      (with-context :prelude (values '|int32| (tc-type "integertypes.int32"))))
		     ((and (>= ir-low (expt 2 63))(< ir-high (expt 2 63)))
		      (with-context :prelude (values '|int64| (tc-type "integertypes.int64"))))
		     (t *integer*)))
	      ((subtype-of? type *naturalnumber*) (values  '|mpz| *naturalnumber*))
	      ((subtype-of? type *integer*) (values '|mpz| *integer*))
	      ((subtype-of? type *rational*) (values  '|mpq| *rational*))
	      ((subtype-of? type *real*) (values  '|mpq| *real*))
	      (t (values  '|mpq| *number*))))
      (check-actual-type (supertype type) bindings)))


(defmethod check-actual-type ((type type-name) bindings)
  (declare (ignore bindings))
  (cond ((tc-eq type *boolean*)
	 (values type '|bool|))
	((tc-eq type *number*)
	 (values *number* '|mpq|))
	((tc-eq type *character*)
	 (values type '|char|))
	((adt-type-name? type)
	 'ref)
	((formal-type-decl? (declaration type))
	 'ref)
	((type-eq-decl? (declaration type))
	 (check-actual-type (copy-without-print-type (type-value (declaration type))) nil)) ;zero out the bindings
	(t (break "check-actual-type(type-name)"))))
    
(defmethod check-actual-type ((type t) bindings)
  (declare (ignore bindings))  
  ;(break "check-actual-type")
  (values type 'ref))
      
	

					 
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

;;The main idea is that local named types are used by name, and foreign named types are
;;used by definition.  This means that the actuals are always inlined within the definition.
(defmethod pvs2ir-type* :around ((type type-expr) tbinding)
  (declare (ignore tbinding))
  (let ((type-name (if (type-name? type) type
		     (print-type type))))
;    (when (and (type-name? type-name)(eq (id type-name) 'byv))(break "pvs2ir-type*(around)"))
    (cond ;((tc-eq type *string-type*) (mk-ir-stringtype))
	  ((subtype-of? type *character*) (mk-ir-chartype))
	  ((type-name? type-name)
	   (let* ((type-decl (declaration type-name))
		  (decl-theory (module type-decl))) ;(break "around")
	      (if (and (not (eq (id decl-theory) *theory-id*))
	      	       (formals-sans-usings (module type-decl)));if the theory has formals, use expanded defn
		  (call-next-method)
	     (if (ir-type-value type-decl)
		 (ir-type-name (ir-type-value type-decl))
		 (call-next-method)))))
	   (t (call-next-method)))))

	    ;; (let* ((ir-type (call-next-method))
	    ;; 	   (ir-type-id (pvs2ir-unique-decl-id type-decl))
	    ;; 	   (ir-typename (mk-ir-typename ir-type-id ir-type)))
	    ;;   (setf (ir-type-value type-decl)
	    ;; 	    (mk-eval-type-info  ir-typename))
;;   ir-typename)))

(defun check-arraytype-ir-subrange (ir-low ir-high);;must be a subrange, returns ir-high
     (and (integerp ir-high)
	  (integerp ir-low)
	  (<= ir-low 0)
	  (<= 0 ir-high)
	  (< ir-high *max-PVS-array-size*)
	  ir-high))

(defmethod pvs2ir-type* ((type arraytype) tbinding)
  (with-slots (domain range) type
    (let* ((ir-dom (pvs2ir-type* domain tbinding))
	   (new-tbinding (if (binding? domain)
			     (acons domain (mk-ir-variable (new-irvar)
							   ir-dom)
				    tbinding)
			   tbinding))
	   (ptype (print-type type))
	   (ir-actuals (when (type-name? ptype)
			 (loop for act in (actuals ptype)
			       collect (if (type-value act)
					   (mk-ir-type-actual (pvs2ir-type (type-value act) tbinding))
					 (mk-ir-const-actual (pvs2ir* act tbinding nil))))))
	   (defined-ir-type-value (when (type-name? ptype)
			     (ir-type-name-with-actuals (declaration ptype) ir-actuals)))
	   (defined-ir-domtype (when (and defined-ir-type-value
					  (ir-arraytype? (ir-type-defn defined-ir-type-value)))
				 (ir-domain (ir-type-defn defined-ir-type-value))))
	   (ir-type (let ((ir-dom-subrange (is-ir-subrange? ir-dom)))
		      (if (ir-subrange? ir-dom-subrange)
			  (with-slots (ir-low ir-high ir-high-expr) ir-dom-subrange
			    (let ((check-range (check-arraytype-ir-subrange ir-low ir-high)))
			      (if check-range
				  (mk-ir-arraytype check-range (or ir-high-expr check-range)
						   ir-dom (pvs2ir-type* range new-tbinding))
				(if defined-ir-domtype
				    (with-slots ((ir-defined-low ir-low) (ir-defined-high ir-high)) defined-ir-domtype
				      (let ((check-defined-range (check-arraytype-ir-subrange ir-defined-low ir-defined-high)))
					(if check-defined-range 
					    (mk-ir-arraytype check-defined-range (or ir-high-expr check-defined-range)
							     ir-dom (pvs2ir-type* range new-tbinding))
					  (mk-ir-funtype ir-dom
							 (pvs2ir-type* range new-tbinding)))))
				  (mk-ir-funtype ir-dom
						 (pvs2ir-type* range new-tbinding))))))
		      (mk-ir-funtype ir-dom
				     (pvs2ir-type* range new-tbinding))))))
      (when (ir-funtype? ir-type)(break "pvs2ir-type*(arraytype)")
	(format t "~%Unable to translate ~a as an array; using function type instead" type))
      ir-type)))

(defmethod pvs2ir-type* ((type funtype) tbinding)
  (with-slots (domain range) type
    (let* ((ir-dom (pvs2ir-type* domain tbinding))
	   (new-irvar (new-irvar))
	   (new-tbinding (if (binding? domain)
			    (acons domain (mk-ir-variable new-irvar
							  ir-dom)
				   tbinding)
			   tbinding))
	   (new-ir-dom (if (binding? domain) (mk-ir-fieldtype new-irvar ir-dom) ir-dom)))
      (mk-ir-funtype new-ir-dom
		     (pvs2ir-type* range new-tbinding)))))

(defun pvs2ir-recordfields (types tbinding &optional record-label)
  (cond ((consp types)
	 (let* (;;(field-id (id (car types)))
		(car-irtype (pvs2ir-type* (car types) tbinding))
		;; (varname (intern (format nil "~a_~a" record-label field-id)))
		;; (type-variable (mk-ir-variable varname car-irtype))

		(new-tbinding (if (binding? (car types))
				  (acons (car types) car-irtype ;type-variable
					 tbinding)
				tbinding)))
	 (cons car-irtype
	       (pvs2ir-recordfields (cdr types) new-tbinding record-label))))
	(t nil)))

(defmethod pvs2ir-type* ((type list) tbinding)
  (cond ((consp type)
	 (let ((car-irtype (pvs2ir-type* (car type) tbinding)))
	   (cons car-irtype
		 (pvs2ir-type* (cdr type) tbinding))))
	(t nil)))

(defmethod pvs2ir-type* ((type recordtype) tbinding)
  (with-slots (fields) type
    (let* ((label (gensym "record"))
	  (sfields (sort-fields fields (dependent-fields? fields))))
	(mk-ir-recordtype (pvs2ir-recordfields sfields tbinding label) label))))

(defmethod pvs2ir-type* ((type field-decl) tbinding)
  (mk-ir-fieldtype (id type)(pvs2ir-type* (type type) tbinding)))

(defmethod pvs2ir-type* ((type dep-binding) tbinding)
  (pvs2ir-type* (type type) tbinding))

(defun pvs2ir-tuplefields (types tbinding &optional  tuple-label (index 1))
  (cond ((consp types)
	 (let* ((field-id (intern (format nil "project_~a" index)))
		(car-irtype (pvs2ir-type* (car types) tbinding))
		(car-ir-fieldtype (mk-ir-fieldtype field-id car-irtype))
		;; (varname (intern (format nil "~a_~a" tuple-label field-id)))
		;; (type-variable (mk-ir-variable varname car-irtype))
		(new-tbinding (if (binding? (car types))
				  (acons (car types) car-ir-fieldtype ;type-variable
					 tbinding)
				tbinding)))
	   (cons car-ir-fieldtype
		 (pvs2ir-tuplefields (cdr types) new-tbinding tuple-label (1+ index)))))
	(t nil)))

(defmethod pvs2ir-type* ((type tupletype) tbinding)
  (let* ((types (types type))
	 (label (gensym "tuple"))
	 (tuple-fields (pvs2ir-tuplefields types tbinding label)))
    (mk-ir-tupletype tuple-fields label)))

(defmethod pvs2ir-type* ((type type-name) tbindings)
  ;(break "pvs2ir-type* (type-name)")
  (if (tc-eq type *boolean*)
      '|bool|
    ;;remove?     (if (tc-eq type *numfield* ))
    (if (formal-type-decl? (declaration type))
	(mk-ir-formal-typename (pvs2ir-unique-decl-id (declaration type)))
      (let ((tbind (assoc (declaration type) tbindings))
	    (decl (declaration type)))
	(unless tbind (pvs2ir-decl decl))
	(if tbind (cdr tbind) (ir-type-name (ir-type-value decl)))))))
;;(pvs2ir-decl (declaration type)))));;returns the type name

(defmethod pvs2ir-type* ((type subtype) tbinding);(break "subtype")
  (let ((result
	 (cond ((tc-eq type *naturalnumber*) (mk-ir-subrange 0 '*))
	       ((tc-eq type *integer*) (mk-ir-subrange  '* '*))
	       ((tc-eq type *posint*)(mk-ir-subrange 1 '*))
	       ((tc-eq type *negint*)(mk-ir-subrange '* -1))
	       ((subtype-of? type *integer*) ;;was *number*
	        ;;was *integer* but misses some subranges
		(let ((sub (pvs2ir-subrange-index type tbinding)))
		  (intersect-subrange sub 
				      (ir-type-def (pvs2ir-type* (supertype type) tbinding)))
		  ))
	       ((subtype-of? type *number*) ;;treating number as real/mpq-need to fix this
		'|mpq|)
	       ((subtype-of? type *character*) '|char|)
					;((subtype-of? type *number*) (break "pvs2ir-type*(subtype)"))
	       (t (pvs2ir-type* (supertype type) tbinding)))))
;    (when (eq result '|mpq|)(break "pvs2ir-type* (subtype)"))
    ;(format t "~%PVS type: ~a => IR type: ~a" type (print-ir result))
    result))


(defun intersect-subrange (sub1 sub2)
  (intersect-subrange* sub1 sub2))

(defmethod intersect-subrange* ((sub1 ir-subrange) (sub2 ir-subrange));;prefers ir-low/high-expr from sub1
  (with-slots ((low1 ir-low) (high1 ir-high) ir-low-expr ir-high-expr) sub1
	      (with-slots ((low2 ir-low) (high2 ir-high)) sub2
			  (let ((new-low (if (eq low1 '*) low2
					   (if (eq low2 '*) low1
					     (if (< low1 low2) low2 low1))))
				(new-high (if (eq high1 '*) high2
					    (if (eq high2 '*) high1
					      (if (< high1 high2) high1 high2)))))
			    (mk-ir-subrange new-low new-high ir-low-expr ir-high-expr)))))

(defmethod intersect-subrange* ((sub1 ir-subrange) (sub2 ir-typename))
  (with-slots (ir-type-defn) sub2
    (intersect-subrange* sub1 ir-type-defn)))

(defmethod intersect-subrange* ((sub1 ir-typename) (sub2 ir-subrange))
  (with-slots (ir-type-defn) sub1
    (intersect-subrange*  ir-type-defn sub2)))

(defmethod intersect-subrange* ((sub1 ir-subrange) (sub2 t))
  sub1)

(defmethod intersect-subrange* ((sub1 t) (sub2 ir-subrange))
  sub2)

(defmethod intersect-subrange* ((sub1 t) (sub2 t))
  (mk-ir-subrange '* '*))


(defun pvseval-integer (expr)
  (handler-case
      (let ((expr-value (eval (pvs2cl expr))))
	(and (integerp expr-value) expr-value))
    (pvseval-error nil)))

(defun mk-ir-subtraction (ir-expr1 ir-expr2)
  (mk-ir-apply (mk-ir-primitive-function '-)
	       (list ir-expr1 ir-expr2)))

(defun is-ir-subrange? (ir-type);;variant of ir-subrange? predicate but handles ir-typenames
  (cond ((ir-subrange? ir-type) ir-type)
	((ir-typename? ir-type)
	 (is-ir-subrange? (ir-type-defn ir-type)))
	(t nil)))


(defun extract-type-lower-bound (bound-expr bindings)
  (or (pvseval-integer bound-expr)
      (let* ((type-ir (pvs2ir-type (type bound-expr) bindings))
	     (judgement-type-irs (loop for type in (judgement-types bound-expr)
				       collect (pvs2ir-type type bindings)))
	     (lo-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
				when (let ((ir-subrange (is-ir-subrange? ir-type)))
				       (and (ir-subrange? ir-subrange)(numberp (ir-low ir-subrange))))
				collect (ir-low (is-ir-subrange? ir-type)))))
	(if lo-subrange (apply #'min lo-subrange) '*))))

(defun extract-type-upper-bound (bound-expr bindings)
  (or (pvseval-integer bound-expr)
      (let* ((type-ir (pvs2ir-type (type bound-expr) bindings))
	     (judgement-type-irs (loop for type in (judgement-types bound-expr)
				       collect (pvs2ir-type type bindings)))
	     (hi-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
				when (let ((ir-subrange (is-ir-subrange? ir-type)))
				       (and (ir-subrange? ir-subrange)(numberp (ir-high ir-subrange))))
				collect (ir-high (is-ir-subrange? ir-type)))))
	(if hi-subrange (apply #'max hi-subrange) '*))))

;returns a pair of the lower and upper bound
(defun pvs2ir-subrange-index (type bindings)
  (let ((below (simple-below? type)));;was failing on below(0) since this was simplified as {x:nat | FALSE}
    (if below                       ;;need to check if empty subranges have the same issue
	(let ((val (pvseval-integer below))) ;if (number-expr? upto)
	  (if val (mk-ir-subrange 0 (1- val)) ;(number upto)
	    (let* ((hihi (is-ir-subrange? (pvs2ir-type (type below) bindings)))
		   (high-hihi (if (ir-subrange? hihi) (ir-high hihi) '*)))
	      (if (loop for fvar in (freevars below);;bindings can be missing for dependent function types
			always (assoc (declaration fvar) bindings :key #'declaration))
		  (let (;;(high-expr-hihi (when (ir-subrange? hihi) (ir-high-expr hihi)))
			(ir-offset (mk-ir-offset (pvs2ir* below bindings nil) -1)))
		    ;;ignoring the nested ir-high-expr for now, but need to take min of the
		    ;;ir-high-exprs. 
		      (mk-ir-subrange 0 high-hihi
				      nil ir-offset))
		    (mk-ir-subrange 0 high-hihi)))))
	(let ((upto (simple-upto? type)))
	  (or (and upto (let ((val (pvseval-integer upto))) ;if (number-expr? upto)
			  (if val (mk-ir-subrange 0 val) ;(number upto)
			    (let* ((hihi (is-ir-subrange? (pvs2ir-type (type upto) bindings)))
				   (high-hihi (if (ir-subrange? hihi)(ir-high hihi) '*)))
			      (if (loop for fvar in (freevars upto);;bindings can be missing for dependent function types
					always (assoc (declaration fvar) bindings :key #'declaration))
				  (let ((ir-offset (mk-ir-offset (pvs2ir* upto bindings nil) 0)))
				    (mk-ir-subrange 0 high-hihi
						    nil ir-offset))
				(mk-ir-subrange 0 high-hihi))))))
	      (let ((x (simple-subrange? type)))
		(if x
		  (let ((lo (or (pvseval-integer (car x))
			      (let* ((type-ir (pvs2ir-type (type (car x)) bindings))
				     (judgement-type-irs (loop for type in (judgement-types (car x))
							       collect (pvs2ir-type type bindings)))
				     (lo-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
							when (let ((ir-subrange (is-ir-subrange? ir-type)))
							       (and (ir-subrange? ir-subrange)(numberp (ir-low ir-subrange))))
							collect (ir-low (is-ir-subrange? ir-type)))))
				(if lo-subrange (apply #'min lo-subrange) '*))))
			(hi (or (pvseval-integer (cdr x))
				(let* ((type-ir (pvs2ir-type (type (cdr x)) bindings))
				       (judgement-type-irs (loop for type in (judgement-types (cdr x))
								 collect (pvs2ir-type type bindings)))
				       (hi-subrange (loop for ir-type in (cons type-ir judgement-type-irs)
							  when (let ((ir-subrange (is-ir-subrange? ir-type)))
								 (and (ir-subrange? ir-subrange)(numberp (ir-high ir-subrange))))
							  collect (ir-high (is-ir-subrange? ir-type)))))
				  (if hi-subrange (apply #'max hi-subrange) '*)))))
		    (mk-ir-subrange lo hi))
		  (if (subtype? type)
		      (let ((super (is-ir-subrange? (pvs2ir-type (supertype type) bindings))))
			(or (and (ir-subrange? super) super)
			    (mk-ir-subrange '* '*)))
		      (mk-ir-subrange '* '*)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pvs2ir-freevars* :around ((ir-expr ir-expr))
  (with-slots (ir-freevars) ir-expr
    ;(when (not (eq ir-freevars 'unbound)) (break "pvs2ir-freevars*(around)"))
    (if (eq ir-freevars 'unbound)    
	(let ((new-ir-freevars (call-next-method)))
	  (setf (ir-freevars ir-expr) new-ir-freevars)
	  new-ir-freevars)
    ir-freevars)))

(defmethod pvs2ir-freevars* ((ir-expr ir-variable))
  (with-slots (ir-vtype) ir-expr  ;;NSH(4-6-24): don't need to collect freevars from types
    (list ir-expr)));; freevars in bound occurrences matter since it could be used for bound computation. 
  ;;NSH(4-6-24):  (union (list ir-expr) (pvs2ir-freevars* ir-vtype) :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-formal-typename))
  (let ((ir-const-formal (loop for entry in *ir-theory-tbindings*
			       when (eq (pvs2ir-unique-decl-id (car entry))
					(ir-type-id ir-expr))
			       return (cdr entry))))
    (if (null ir-const-formal) nil ;(break "missing ir-formal-typename")
      (list ir-const-formal))))

(defmethod pvs2ir-freevars* ((ir-expr ir-last))
  (with-slots (ir-var) ir-expr
    ;(when (ir-fieldtype? ir-var)(break "pvs2ir-freevars*(ir-last)"))
	      (pvs2ir-freevars* ir-var)))

(defmethod pvs2ir-freevars* ((ir-expr ir-apply))
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr
	      (union (pvs2ir-freevars* ir-func)
		     (union (pvs2ir-freevars* ir-params)
			    (union (pvs2ir-freevars* ir-args)
				   (pvs2ir-freevars* (rename-type ir-atype nil))
				   :test #'eq)
			    :test #'eq)
		     :test #'eq)))


(defmethod pvs2ir-freevars* ((ir-expr ir-let))
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
    (union (pvs2ir-freevars* ir-bind-expr)
	   (union (pvs2ir-freevars* (ir-vtype ir-vartype))
		  (remove (ir-name ir-vartype) (pvs2ir-freevars* ir-body) :key #'ir-name)
		  :test #'eq)
	   :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-lett))
  (with-slots (ir-vartype ir-bind-type ir-bind-expr ir-body) ir-expr
    (union (pvs2ir-freevars* ir-bind-expr)
	   (union (pvs2ir-freevars* ir-bind-type)
		  (union (pvs2ir-freevars*  (ir-vtype ir-vartype))
			 (remove (ir-name ir-vartype) (pvs2ir-freevars* ir-body) :key #'ir-name)
			 :test #'eq)
		  :test #'eq)
	   :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-record))
  (with-slots (ir-fields ir-recordtype) ir-expr
    (union (pvs2ir-freevars* ir-fields)
	   (pvs2ir-freevars* ir-recordtype)
	   :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-expr ir-field))
  (with-slots (ir-fieldname ir-value) ir-expr
	      (pvs2ir-freevars* ir-value)))

(defmethod pvs2ir-freevars* ((ir-expr ir-lambda))
  (with-slots (ir-vartypes ir-body ir-rangetype) ir-expr
    (set-difference (union (pvs2ir-freevars* ir-rangetype)
			   (union (pvs2ir-freevars* ir-vartypes)
				  (union (pvs2ir-freevars* (mapcar #'ir-vtype ir-vartypes));;NSH(4-6-24)
					 (pvs2ir-freevars* ir-body)
					 :test #'eq)
				  :test #'eq)
			   :test #'eq)
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

(defmethod pvs2ir-freevars* ((ir-expr ir-array-literal))
  (with-slots (ir-array-literal-exprs) ir-expr
    (pvs2ir-freevars* ir-array-literal-exprs)))

(defmethod pvs2ir-freevars* ((ir-expr ir-forall))
  (with-slots (ir-vartype ir-body) ir-expr
    (let ((bind-freevars (pvs2ir-freevars* (ir-vtype ir-vartype)))
	  (body-freevars (pvs2ir-freevars* ir-body)))
      (remove (ir-name ir-vartype) (union bind-freevars body-freevars :test #'eq) :key #'ir-name))))

(defmethod pvs2ir-freevars* ((ir-expr ir-exists))
  (with-slots (ir-vartype ir-body) ir-expr
    (let ((bind-freevars (pvs2ir-freevars* (ir-vtype ir-vartype)))
	  (body-freevars (pvs2ir-freevars* ir-body)))
      (remove (ir-name ir-vartype) (union bind-freevars body-freevars :test #'eq) :key #'ir-name))))

(defmethod pvs2ir-freevars* ((ir-expr list))
  (cond ((consp ir-expr)
	 (let ((car-fvars (pvs2ir-freevars* (car ir-expr)))
	       (cdr-fvars (pvs2ir-freevars* (cdr ir-expr))))
			  
	   (if (ir-fieldtype? (car ir-expr))
	       (union car-fvars (remove (ir-name (car ir-expr)) cdr-fvars :key #'ir-name)
		      :test #'eq)
	     (union car-fvars cdr-fvars :test #'eq))))
	(t nil)))


(defmethod pvs2ir-freevars* ((ir-expr ir-release))
  (with-slots (ir-body) ir-expr
    (pvs2ir-freevars* ir-body)))

(defmethod pvs2ir-freevars* ((ir-expr ir-typename))
  nil)

(defmethod pvs2ir-freevars* ((ir-expr t))
  nil)

(defmethod pvs2ir-freevars* :around ((ir-type ir-type))
  (let ((new-ir-type (rename-type ir-type nil)))
    (call-next-method new-ir-type)))

(defmethod pvs2ir-freevars* ((ir-type ir-recordtype))
  (with-slots (ir-field-types) ir-type
    (pvs2ir-freevars* ir-field-types)))

(defmethod pvs2ir-freevars* ((ir-type ir-fieldtype))
  (with-slots (ir-vtype) ir-type
    (pvs2ir-freevars* ir-vtype)))

(defmethod pvs2ir-freevars* ((ir-type ir-funtype))
  (with-slots (ir-domain ir-range) ir-type
    (let ((dom-fvars (pvs2ir-freevars* ir-domain))
	  (range-fvars (pvs2ir-freevars* ir-range)))
      (if (ir-fieldtype? ir-domain)
	  (union dom-fvars (remove (ir-name ir-domain) range-fvars :key #'ir-name)
		 :test #'eq)
	(union dom-fvars range-fvars :test #'eq)))))

(defmethod pvs2ir-freevars* ((ir-type ir-arraytype))
  (with-slots (size high ir-domain ir-range) ir-type
    (let ((dom-fvars (pvs2ir-freevars* ir-domain))
	  (range-fvars (pvs2ir-freevars* ir-range)))
      (if (ir-fieldtype? ir-domain)
	  (union dom-fvars (remove (ir-name ir-domain) range-fvars :key #'ir-name)
		 :test #'eq)
	(union dom-fvars range-fvars :test #'eq)))))


(defmethod pvs2ir-freevars* ((ir-type ir-subrange))
  (with-slots (ir-low ir-high ir-low-expr ir-high-expr)
      ir-type
    (union (pvs2ir-freevars* ir-low-expr)
	   (pvs2ir-freevars* ir-high-expr)
	   :test #'eq)))

(defmethod pvs2ir-freevars* ((ir-type ir-offset))
  (with-slots (expr) ir-type
    (pvs2ir-freevars* expr)))

      
     

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mk-ir-last (ir-var)
  (make-instance 'ir-last
		 :ir-var ir-var))

(defmethod print-ir ((ir-expr ir-last))
  (with-slots (ir-var) ir-expr
  `(last ,(print-ir ir-var))))

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

(defmethod print-ir ((ir-expr ir-offset))
  (with-slots (expr offset) ir-expr
    `(offset ,(print-ir expr) ,offset)))


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
  (let* ((ir-var (get-assoc ir-expr bindings)))
    (if (memq ir-var livevars)
	ir-var
	   ;;then variable is live and this is not the last occurrence
      (mk-ir-last ir-var))))

;;Since ir-fieldtype is also an ir-variable 
(defmethod preprocess-ir* ((ir-expr ir-fieldtype) livevars bindings)
  (declare (ignore livevars))
  (let ((entry (assoc ir-expr bindings)))
    ;;(break "preproc(ir-fieldtype)")
    (if entry (cdr entry) ir-expr)))

;;since rename-type introduces last variables, these have to be preprocessed as well
(defmethod preprocess-ir* ((ir-expr ir-last) livevars bindings)
  (with-slots (ir-var) ir-expr
    (let ((iv (get-assoc ir-var bindings)))
      (if (memq iv livevars) iv (mk-ir-last iv)))))

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

;; Not used
;; (defun ir-apply-arith-type (op args atype)
;;   (if (and (ir-function? op)
;; 	   (ir-primitive-arith-op? (ir-fname op)))
;;       (let ((new-subrange-type
;; 	     (case (ir-fname op)
;; 	       (+ (plus-subrange (ir-arith-type (car args))
;; 				 (ir-arith-type (cadr args))))
;; 	       (- (if (< (length args) 2)
;; 		      (negate-subrange (ir-arith-type (car args)))
;; 		    (minus-subrange (ir-arith-type (car args))
;; 				    (ir-arith-type (cadr args)))))
;; 	       (* (times-subrange (ir-arith-type (car args))
;; 				  (ir-arith-type (cadr args))))
;; 	       (t))))
;; 	(if new-subrange-type
;; 	    (if atype
;; 		(intersect-subrange new-subrange-type atype)
;; 	      new-subrange-type)
;; 	  atype))
;;     atype))    



(defmethod preprocess-ir* ((ir-expr ir-apply) livevars bindings);;the ir-args are always distinct, but we are
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr               ;;not exploiting this here.
	      (let* ((ir-params-with-bindings (apply-bindings bindings (pvs2ir-freevars* ir-params)))
		     (ir-args-with-bindings (apply-bindings bindings (pvs2ir-freevars* ir-args)))
		     (new-ir-atype (preprocess-ir-in-type ir-atype bindings))
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
		(mk-ir-apply new-ir-func new-ir-args new-ir-params new-ir-atype))))


		     ;; (new-ir-atype (if (and (ir-primitive-arith-op? new-ir-func)
		     ;; 			    (memq new-ir-func '(+ * -)))
		     ;; 		       (ir-apply-arith-type new-ir-func ir-atype
							    

(defun rename-variable (ir-vartype bindings)
  (if bindings
      (with-slots (ir-vtype) ir-vartype
	(setf (ir-vtype ir-vartype) (rename-type ir-vtype bindings)
	      (ir-freevars ir-vartype) 'unbound)
	ir-vartype)
    ir-vartype))

(defmethod rename-type ((ir-type ir-recordtype) bindings)
  (with-slots (ir-field-types renamings) ir-type
    (lcopy ir-type
	   'ir-field-types (rename-type ir-field-types (append renamings bindings))
	   'renamings nil)))

(defmethod rename-type ((ir-type list) bindings)
  (cond ((consp ir-type)
	 (cons (rename-type (car ir-type) bindings)
	       (rename-type (cdr ir-type) bindings)))
	(t nil)))

(defmethod rename-type ((ir-type ir-fieldtype) bindings)
  (with-slots (ir-vtype renamings) ir-type
    (lcopy ir-type
	   'ir-vtype (rename-type ir-vtype (append renamings bindings))
	   'renamings nil)))

(defmethod rename-type ((ir-type ir-funtype) bindings)
  (with-slots (ir-domain ir-range renamings) ir-type
    (let ((new-bindings (append renamings bindings)))
      (lcopy ir-type 'ir-domain (rename-type ir-domain new-bindings)
	     'ir-range (rename-type ir-range new-bindings)
	     'renamings nil))))

(defun preprocess-in-type (ir-expr bindings)
  (let ((*preprocessing-in-type* t))
    (preprocess-ir* ir-expr nil bindings)))

(defmethod rename-type ((ir-type ir-arraytype) bindings)
  (with-slots (size high ir-domain ir-range renamings) ir-type
    (let ((new-bindings (append renamings bindings)))
      (lcopy ir-type 'high (preprocess-in-type high new-bindings)
	     'ir-domain (rename-type ir-domain new-bindings)
	     'ir-range (rename-type ir-range new-bindings)
	     'renamings nil))))

(defmethod rename-type ((ir-type ir-subrange) bindings)
  (with-slots (ir-low-expr ir-high-expr renamings) ir-type
    (let ((new-bindings (append renamings bindings)))
      (lcopy ir-type 'ir-low-expr (preprocess-in-type ir-low-expr new-bindings)
	     'ir-high-expr (preprocess-in-type ir-high-expr new-bindings)
	     'renamings nil))))

(defmethod rename-type ((ir-type t) bindings)
  (declare (ignore bindings))
  ir-type)

(defun ir2c-tcompatible-but (texpr1 texpr2)
  (let ((gtexpr1 (get-ir-type-value texpr1))
	(gtexpr2 (get-ir-type-value texpr2)))
    (if (ir-adt-constructor-recordtype? gtexpr1)
	(and (ir-adt-constructor-recordtype? gtexpr2)
	     (ir2c-tcompatible texpr1 texpr2))
      (ir2c-tcompatible texpr1 texpr2))))
	      

;Irrelevant let-bindings are discarded
(defmethod preprocess-ir* ((ir-expr ir-let) livevars bindings)
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
    (let ((body-freevars (pvs2ir-freevars* ir-body)))
      ;;(format t "preprocess-ir* (irlet): expr = ~a)" (print-ir ir-expr))
					;(loop for iv in body-freevars do (format t "~% free: ~a" (ir-name iv)))
					;(when (not (memq ir-vartype body-freevars)) (break "preprocess"))
      (if (memq ir-vartype body-freevars) ;;note: without apply-bindings
	  (let ((new-ir-vartype (rename-variable ir-vartype bindings));does an in-place substitution
		(new-ir-bind-expr
		 (preprocess-ir* ir-bind-expr (union (apply-bindings bindings
								     body-freevars)
						     livevars :test #'eq)
				 bindings)));(break "preprocess ir-let: ~a" (ir-name ir-vartype))
					;(when (eq (ir-name ir-vartype) 'ivar_13)(break "preprocess ir-let: ~a" (ir-name ir-vartype)))
	    (if (eq ir-vartype ir-body)
		new-ir-bind-expr;;for let x = a in b, if x ~ b, then simplify to a
	      (if (and (or (and (ir-variable? new-ir-bind-expr) ;;bind var to var
				(not (mpnumber-type? (ir-vtype new-ir-bind-expr))))
			   (ir-last? new-ir-bind-expr))
		       (ir2c-tcompatible-but (ir-vtype ir-vartype)(ir-vtype (get-ir-last-var new-ir-bind-expr)))
		       )
					;binds var to var' with same type, then replace var with var'
		  (preprocess-ir* ir-body livevars
				  (acons ir-vartype ; should be eq to new-ir-vartype
					 (get-assoc (get-ir-last-var new-ir-bind-expr) bindings)
					 bindings))
		(let* ((new-ir-body (preprocess-ir* ir-body livevars bindings))) ;(break "preprocess*(ir-let)")
		  ;;(acons ir-vartype new-ir-vartype bindings)
					;(when (ir-lett? ir-expr) (break "ir-lett"))
		  ;;(format t "old-ir-body: ~a" (print-ir ir-body))
		  ;;(format t "preprocess-ir*: output = (let ~a ~a ~a)" (print-ir new-ir-vartype)(print-ir new-ir-bind-expr)(print-ir new-ir-body))
		  ;;		(format t "~%old-ir~%~s" (print-ir ir-expr))
		  (if (ir-lett? ir-expr)
		      (let ((new-ir-expr (make-ir-lett new-ir-vartype (rename-type (ir-bind-type ir-expr) bindings)
						       new-ir-bind-expr
						       new-ir-body)));(break "preprocess-ir*(ir-lett)")
			;(format t "~%new-ir~%~s" (print-ir new-ir-expr))
			new-ir-expr)
		    (mk-ir-let  new-ir-vartype new-ir-bind-expr new-ir-body))))))
					;(progn (format t "~%not free: ~a" (ir-name ir-vartype))
	(preprocess-ir* ir-body livevars bindings)))))

(defmethod preprocess-ir* ((ir-expr ir-record) livevars bindings)
  (with-slots (ir-fields ir-recordtype) ir-expr
	      (mk-ir-record (preprocess-ir* ir-fields livevars bindings)
			    (preprocess-ir-in-type ir-recordtype bindings))))

(defmethod preprocess-ir* ((ir-expr ir-field) livevars bindings)
  (with-slots (ir-fieldname ir-value) ir-expr
	      (mk-ir-field ir-fieldname (preprocess-ir* ir-value livevars bindings))))

(defun ir-numeric-type? (ir-type)
  (or (eq ir-type '|mpq|)
      (ir-subrange? ir-type)))

(defun ir-mpq-type? (ir-type)
  (eq ir-type '|mpq|))

(defvar *preprocessing-in-type* nil)

(defun preprocess-ir-in-type (ir-type bindings)
  ;(when (consp bindings) (break "preprocess-ir-in-type"))
  (if (and (ir-type? ir-type)(pvs2ir-freevars* ir-type))
      (with-slots (renamings) ir-type 
	(lcopy ir-type 
	       'renamings  (append bindings renamings)))
    ir-type))

(defmethod preprocess-ir* ((ir-expr ir-offset) livevars bindings)
  (with-slots (expr) ir-expr
    (lcopy ir-expr
	   'ir-freevars 'unbound
	   'expr (preprocess-ir* expr livevars bindings))))

(defmethod preprocess-ir* ((ir-expr ir-array-literal) livevars bindings)
  (with-slots (ir-array-literal-exprs) ir-expr
    (mk-ir-array-literal (preprocess-ir* ir-array-literal-exprs livevars bindings))))

(defmethod preprocess-ir* ((ir-expr ir-forall) livevars bindings)
  (with-slots (ir-vartype ir-body) ir-expr
    (let ((new-ir-vartype (rename-variable ir-vartype bindings))
	  (body-freevars (pvs2ir-freevars* ir-body)))
      (if (memq ir-vartype body-freevars)
	  (lcopy ir-expr 'ir-vartype new-ir-vartype
		 'ir-body (preprocess-ir* ir-body livevars bindings)
		 'ir-freevars 'unbound)
	(preprocess-ir* ir-body livevars bindings)))))

(defmethod preprocess-ir* ((ir-expr ir-lambda) livevars bindings)
  (with-slots (ir-vartypes ir-rangetype ir-body) ir-expr
    (let* ((new-ir-vartypes (loop for ir-var in ir-vartypes
				  collect (rename-variable ir-var bindings)))
	   (ir-mpq-vartypes (loop for ir-var in new-ir-vartypes
				  when (ir-mpq-type? (ir-vtype ir-var))
				  collect ir-var))
	   (expr-freevars (apply-bindings bindings (pvs2ir-freevars* ir-expr)));;no need to substitute into types of freevars-only names matter
	   (last-expr-freevars (set-difference expr-freevars livevars :test #'eq))
		       ;;last-expr-freevars preserve refcount when closure is created
		       ;;other expr-freevars have their refcounts incremented by one.
	   (other-livevars (append ir-mpq-vartypes (union last-expr-freevars livevars :test #'eq)))
		       ;;(body-freevars (pvs2ir-freevars* ir-body))
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
	     new-ir-vartypes
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
	when (or (ir-reference-type? (ir-vtype ir-var))
		 (mpnumber-type? (ir-vtype ir-var)))
	collect ir-var))


(defmethod preprocess-ir* ((ir-expr ir-ift) livevars bindings);(break "preprocess-ir* (ift)")
  (with-slots (ir-condition ir-then ir-else) ir-expr
    (if (last-cond-expr? ir-expr)
	(preprocess-ir* ir-else livevars bindings)
      (let* ((pre-then (preprocess-ir* ir-then livevars bindings))
	     (pre-else (preprocess-ir* ir-else livevars bindings))
	     (then-freevars (apply-bindings bindings (pvs2ir-freevars* pre-then)))
	     (then-marked (set-difference then-freevars livevars :test #'eq))		     
	     (else-freevars (apply-bindings bindings (pvs2ir-freevars* pre-else)))
	     (else-marked (set-difference else-freevars livevars :test #'eq))
	     (then-release		;(extract-reference-vars
	      (set-difference else-marked then-marked :test #'eq))
	     (else-release		;(extract-reference-vars
	      (set-difference then-marked else-marked :test #'eq))
	     )
	(mk-ir-ift ir-condition ;;always a variable
		   (if then-release (mk-ir-release then-release nil pre-then)
		     pre-then)
		   (if else-release (mk-ir-release else-release nil pre-else)
		     pre-else))))))

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
		(unless (or *to-emacs*
			    (ir-last? new-ir-target))
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
;;    (when (not (ir-variable? (get-ir-last-var ir-record))) (break "ir-get"))
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

;; ;;preprocess-ir* on types
;; (defmethod preprocess-ir* ((ir-type ir-arraytype) livevars bindings)
;;   (declare (ignore livevars))(assert *preprocessing-in-type*)
;;   (with-slots (size high ir-domain ir-range) ir-type
;;     (let* ((new-ir-domain (preprocess-ir* ir-domain nil bindings))
;; 	   (new-bindings (if (ir-fieldtype? ir-domain)
;; 			     (acons ir-domain new-ir-domain bindings)
;; 			   bindings)))
;;       (mk-ir-arraytype size high new-ir-domain
;; 		    (preprocess-ir* ir-range nil new-bindings)))))

;; (defmethod preprocess-ir* ((ir-type ir-recordtype) livevars bindings)
;;   (declare (ignore livevars))(assert *preprocessing-in-type*)
;;   (with-slots (ir-field-types) ir-type
;;     (let* ((new-field-types (preprocess-ir* ir-field-types nil bindings)))
;;       (mk-ir-recordtype new-field-types))))

;; (defmethod preprocess-ir* ((ir-type ir-adt-recordtype) livevars bindings)
;;   (declare (ignore livevars))(assert *preprocessing-in-type*)
;;   (with-slots (ir-field-types constructors) ir-type
;;     (let* ((new-field-types (preprocess-ir* ir-field-types nil bindings)))
;;       (mk-ir-adt-recordtype new-field-types constructors))))

;; (defmethod preprocess-ir* ((ir-type ir-subrange) livevars bindings)
;;   (declare (ignore livevars))  (assert *preprocessing-in-type*)
;;   (with-slots (ir-low ir-high ir-low-expr ir-high-expr) ir-type
;;     (mk-ir-subrange ir-low ir-high
;;      (preprocess-ir* ir-low-expr nil bindings)
;;      (preprocess-ir* ir-high-expr nil bindings))))

  

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
  (mk-ir-variable '|result| ir-type))

(defun ir2c-theory-formals (ir-theory-formals theory-formals)
  (let ((*ir-theory-formals* nil)
	(*theory-formals* nil))
    (ir2c-theory-formals-rec ir-theory-formals theory-formals nil nil nil)))

(defun ir2c-theory-formals-rec (ir-theory-formals theory-formals pre-ir-theory-formals pre-theory-formals accum)
  (cond ((null theory-formals)
	 (nreverse accum))
	(t (let* ((tformal (car theory-formals))
		  (ir-formal (car ir-theory-formals))
		  (new-pre-ir-theory-formals (append pre-ir-theory-formals (list ir-formal)))
		  (new-pre-theory-formals (append pre-theory-formals (list tformal)))
		  (*pvs2c-current-decl* tformal)
		  (result (format nil "~a_t ~a"
				  (if (formal-const-decl? tformal)
				      (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))
				    '|type_actual|)
				  (ir-formal-id ir-formal))))
	     (let ((*ir-theory-formals* new-pre-ir-theory-formals)
		   (*theory-formals* new-pre-theory-formals)
		   )
	       (ir2c-theory-formals-rec (cdr ir-theory-formals)(cdr theory-formals)
					new-pre-ir-theory-formals
					new-pre-theory-formals
					(cons result accum)))))))

(defun ir2c-theory-formal-types (ir-theory-formals theory-formals)
    (let ((*ir-theory-formals* nil)
	  (*theory-formals* nil))
      (ir2c-theory-formals-types-rec ir-theory-formals theory-formals nil nil nil)))


(defun ir2c-theory-formals-types-rec (ir-theory-formals theory-formals pre-ir-theory-formals pre-theory-formals accum)
  (cond ((null theory-formals)
	 (nreverse accum))
	(t (let* ((tformal (car theory-formals))
		  (ir-formal (car ir-theory-formals))
		  (new-pre-ir-theory-formals (append pre-ir-theory-formals (list (car ir-theory-formals))))
		  (new-pre-theory-formals (append pre-theory-formals (list (car theory-formals))))
		  (*pvs2c-current-decl* tformal)
		  (result (if (formal-const-decl? tformal)
			      (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))
			    '|type_actual|))
		  (*ir-theory-formals* new-pre-ir-theory-formals)
		  (*theory-formals* new-pre-theory-formals)
		  )
	     (ir2c-theory-formals-types-rec
	      (cdr ir-theory-formals)(cdr theory-formals)
	      new-pre-ir-theory-formals
	      new-pre-theory-formals
	      (cons result accum))))))

(defun ir2c-theory-formals-variadic (cvarname ir-theory-formals theory-formals)
    (let ((*ir-theory-formals* nil)
	  (*theory-formals* nil))
      (ir2c-theory-formals-variadic-rec cvarname ir-theory-formals theory-formals nil nil nil)))

(defun ir2c-theory-formals-variadic-rec (cvarname ir-theory-formals theory-formals pre-ir-theory-formals pre-theory-formals accum)
  (cond ((null theory-formals)
	 (nreverse accum))
	(t (let* ((new-pre-ir-theory-formals (append pre-ir-theory-formals (list (car ir-theory-formals))))
		  (new-pre-theory-formals (append pre-theory-formals (list (car theory-formals))))
		  (tformal (car theory-formals))
		  (ir-formal (car ir-theory-formals))
		  (*pvs2c-current-decl* tformal)
		  (result (let* ((ir-type
				  (if (formal-const-decl? tformal)
				      (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))
				    '|type_actual|))
				 (ir-var (ir-formal-id ir-formal))) ;;should be make-c-assignment (mppointer-type)
			    (format nil "~a_t ~a = ~a->~a" ;va_arg(args, ~a_t)
				    ir-type ir-var cvarname ir-var)))
		  (*ir-theory-formals* new-pre-ir-theory-formals)
		  (*theory-formals* new-pre-theory-formals)
		  )
	     (ir2c-theory-formals-variadic-rec
	      cvarname (cdr ir-theory-formals)(cdr theory-formals)
	      new-pre-ir-theory-formals
	      new-pre-theory-formals
	      (cons result accum))))))


(defun ir2c (ir-expr return-type);;this is called on a whole definition with a result
  ;(when (null return-type) (break "ir2c"))
  (ir2c* ir-expr '|result| (ir2c-type return-type)))

(defun mk-if-instr (if-cond then-instr else-instr)
  (make-instance 'if-instr
		 :if-cond if-cond
		 :then-instr then-instr
		 :else-instr else-instr))

(defun mk-for-instr (for-index for-body)
  (make-instance 'for-instr
		 :for-index for-index
		 :for-body for-body))

(defun make-c-application-assignment (lhs lhs-type fname args rhs-type)
  (let ((rhs (format nil "~a(~a)" fname args)))
    (case lhs-type
      ((|uint8| |uint16| |uint32| |uint64| |__uint128|)
       (case rhs-type
	 (|mpz| (list (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs)))
	 (|mpq| (list (let ((tmp (gentemp "tmp")))
		    (format nil "mpz_t ~a" tmp)
		    (format nil "mpz_init(~a)" tmp)
		    (format nil "mpz_set_q(~a, ~a)" tmp rhs)
		    (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs)
		    (format nil "mpz_clear(~a)" tmp))))
	 (t (list (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs)))))
    ((|int8| |int16| |int32| |int64| |__int128|)
     (case rhs-type
       (|mpz| (list (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs)))
       (|mpq| (list (let ((tmp (gentemp "tmp")))
		    (format nil "mpz_t ~a" tmp)
		    (format nil "mpz_init(~a)" tmp)
		    (format nil "mpz_set_q(~a, ~a)" tmp rhs)
		    (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs)
		    (format nil "mpz_clear(~a)" tmp))))
       (t (list (format nil "~a = (~a_t)~a" lhs lhs-type rhs)))))
    (|mpz| (case rhs-type
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (list (format nil "mpz_mk_set_ui(~a, ~a)" lhs rhs)))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (list (format nil "mpz_mk_set_si(~a, ~a)" lhs rhs)))
	   (|mpq| (list (format nil "mpz_mk_set_q(~a, ~a)" lhs rhs)))
	   (t (list ;; (format nil "mpz_ptr_t ~a" tmp)
		 (format nil "~a = (~a_t)~a" lhs "mpz_ptr" rhs)
		      ;;(format nil "~a = (mpz_t) safe_malloc(sizeof(mpz_t))" lhs)
		      ;; (format nil "mpz_init(~a)" lhs)
		      ;; (format nil "mpz_set(~a, ~a)" lhs tmp)
		      ;; (format nil "mpz_clear(~a)" tmp)
		      ))))
    (|mpq| (case rhs-type
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (list (format nil "~a = (mpq_t) safe_malloc(sizeof(mpq_t))" lhs)
		  (format nil "mpq_init(~a)" lhs)
		  (format nil "mpq_set_ui(~a, ~a, 1)" lhs rhs)))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (list (format nil "mpq_mk_set_si(~a, ~a)" lhs rhs)))
	   (|mpz|  (list (format nil "mpq_mk_set_z(~a, ~a)" lhs rhs)
			))
	   (t (list (format nil "~a = (~a_t)~a" lhs "mpq_ptr" rhs)))))
    (t (list (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))))

(defun mk-c-assignment (lhs lhs-type rhs rhs-type);;mallocs lhs for gmp
  (case lhs-type
    (|mpz| (case rhs-type
	   (|mpz| (format nil "mpz_mk_set(~a, ~a)" lhs rhs))
	   (|mpq| (format nil "mpz_mk_set_q(~a, ~a)" lhs rhs))
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (format nil "mpz_mk_set_ui(~a, ~a)" lhs rhs))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (format nil "mpz_mk_set_si(~a, ~a)" lhs rhs))))
    (|mpq| (case rhs-type
	   (|mpz| (format nil "mpq_mk_set_z(~a, ~a)" lhs rhs))
	   (|mpq| (format nil "mpq_mk_set(~a, ~a)" lhs rhs))
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (format nil "mpq_mk_set_ui(~a, ~a)" lhs rhs))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (format nil "mpq_mk_set_si(~a, ~a)" lhs rhs))))
    ((|uint8| |uint16| |uint32| |uint64|); |__uint128|) 
     (case rhs-type
       (|mpz| (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs))
       (|mpq| (format nil "~a = (~a_t)mpq_get_ui(~a)" lhs lhs-type rhs))
       (t (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))
    ((|int8| |int16| |int32| |int64|); |__int128|)
     (case rhs-type
       (|mpz| (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs))
       (|mpq| (format nil "~a = (~a_t)mpq_get_si(~a)" lhs lhs-type rhs))       
       (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))
    (t (format nil "~a = (~a_t)~a" lhs lhs-type rhs))))

(defun mk-c-assignment-with-count (lhs ir-lhs-type rhs rhs-type)
  (if (and (ir-reference-type? ir-lhs-type)
	   (not (ir-formal-typename? ir-lhs-type)))
      (list (mk-c-assignment lhs (add-c-type-definition ir-lhs-type) rhs rhs-type)
	    (format nil "if (~a != NULL) ~a->count++" lhs lhs))
    (list (mk-c-assignment lhs (add-c-type-definition ir-lhs-type) rhs rhs-type))))

(defun make-c-decl-assignment-with-count (lhs ir-lhs-type rhs rhs-type) ;lhs must be a variable
  (let ((lhs-type (add-c-type-definition ir-lhs-type)))  
    (if (and (ir-reference-type? ir-lhs-type)
	     (not (ir-formal-typename? ir-lhs-type)))
	(list (format nil "~a_t ~a" lhs-type (make-c-assignment lhs lhs-type rhs rhs-type))
	      (format nil "if (~a != NULL) ~a->count++" lhs lhs))
      (cons (format nil "~a_t ~a" lhs-type lhs)
	    (ir2c-initialize lhs lhs-type rhs rhs-type)))))

(defun make-c-assignment-with-count (lhs ir-lhs-type rhs rhs-type)
  (if (and (ir-reference-type? ir-lhs-type)
	   (not (ir-formal-typename? ir-lhs-type)))
      (list (make-c-assignment lhs (add-c-type-definition ir-lhs-type) rhs rhs-type)
	    (format nil "if (~a != NULL) ~a->count++" lhs lhs))
    (list (make-c-assignment lhs (add-c-type-definition ir-lhs-type) rhs rhs-type))))


(defun make-c-assignment (lhs lhs-type rhs rhs-type)
  (case lhs-type
    (|mpz| (case rhs-type
	   (|mpz| (format nil "mpz_set(~a, ~a)" lhs rhs))
	   (|mpq| (format nil "mpz_set_q(~a, ~a)" lhs rhs))
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (format nil "mpz_set_ui(~a, ~a)" lhs rhs))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (format nil "mpz_set_si(~a, ~a)" lhs rhs))))
    (|mpq| (case rhs-type
	   (|mpz| (format nil "mpq_set_z(~a, ~a)" lhs rhs))
	   (|mpq| (format nil "mpq_set(~a, ~a)" lhs rhs))
	   ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	    (format nil "mpq_set_ui(~a, ~a, 1)" lhs rhs))
	   ((|int8| |int16| |int32| |int64|); |__int128|)
	    (format nil "mpq_set_si(~a, ~a, 1)" lhs rhs))))
    ((|uint8| |uint16| |uint32| |uint64|); |__uint128|) 
     (case rhs-type
       (|mpz| (format nil "~a = (~a_t)mpz_get_ui(~a)" lhs lhs-type rhs))
       (|mpq| (format nil "~a = (~a_t)mpq_get_ui(~a)" lhs lhs-type rhs))
       (t (format nil "~a = (~a_t)~a" lhs (mppointer-type lhs-type) rhs))))
    ((|int8| |int16| |int32| |int64|); |__int128|)
     (case rhs-type
       (|mpz| (format nil "~a = (~a_t)mpz_get_si(~a)" lhs lhs-type rhs))
       (|mpq| (format nil "~a = (~a_t)mpq_get_si(~a)" lhs lhs-type rhs))       
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
	      (let* ((rhs (format nil "~a()" ir-fname))
		     (ir2c-return-type (ir2c-type return-type))
		     (rhs-type (or (and declaration (add-c-type-definition (ir2c-type (pvs2ir-type (type declaration)))))
				   (add-c-type-definition ir2c-return-type))))
		(mk-c-assignment-with-count return-var ir2c-return-type rhs rhs-type))))


(defmethod ir2c* ((ir-expr ir-bool) return-var return-type)
  (declare (ignore return-type))
  (with-slots (ir-boolval) ir-expr
    (list (format nil "~a = (bool_t) ~a" return-var (if ir-boolval "true" "false")))))

(defmethod ir2c* ((ir-expr ir-exit) return-var return-type)
  (declare (ignore return-var return-type))
  (with-slots (ir-message ir-code) ir-expr 
    (list (format nil "pvs2cerror(~s, ~a)" ir-message ir-code))))

(defmethod ir2c* ((ir-expr ir-string) return-var return-type)
  (with-slots (ir-stringval) ir-expr
    (let ((stringvar (gentemp "string"))
	  (lenvar (gentemp "len"))
	  (avar (gentemp "characters"))
	  (length (length ir-stringval))
	  (c-return-type (add-c-type-definition (ir2c-type return-type))))
      (list (format nil "uint32_t ~a = ~a" lenvar length)
	    (format nil "uint32_t ~a[~a] = {~{~a~^, ~}}" avar length
		    (loop for i from 0 to (1- length)
			  collect (char-code (char ir-stringval i))))
	    (format nil "stringliteral_t ~a = mk_string(~a, ~a)" stringvar length avar
		    )
	    (format nil "~a = (~a_t)strings__make_string(~a, ~a)"
		     return-var c-return-type lenvar stringvar)
	    ))))

(defmethod ir2c* ((ir-expr ir-array-literal) return-var return-type)
  ;(break "ir2c*(ir-array-literal)")
  (with-slots (ir-array-literal-exprs) ir-expr
    (let* ((c-return-type (add-c-type-definition (ir2c-type return-type)))
	   (rhs-exprs (loop for x in ir-array-literal-exprs collect (ir-name (get-ir-last-var x))))
	   (size (length rhs-exprs))
	   (rhs (format nil "new_~a(~a)" c-return-type  size))
	   (exprs-instrs (loop for x in rhs-exprs as i from 0 collect (format nil "~a->elems[~a]= ~a" return-var i x))))
      (cons (mk-c-assignment return-var c-return-type  rhs c-return-type)
	    exprs-instrs))))


(defmethod ir2c* ((ir-expr ir-integer) return-var return-type);;we need to handle number representations. 
  (with-slots (ir-intval) ir-expr
	      (let* ((ir-value-type (mk-ir-subrange ir-intval ir-intval))
		     (c-value-type (ir2c-type ir-value-type))
		     (c-return-type (print-ir return-type))
		     (c-assignment (mk-c-assignment return-var c-return-type
						      ir-intval c-value-type)))
		(case c-return-type
		  ((|mpz| |mpq|)
		   (list (format nil "~a = safe_malloc(sizeof(~a_t))"
				 return-var c-return-type)
			 (format nil "~a_init(~a)" c-return-type return-var)
			 c-assignment))
		  (t (list c-assignment))))))
		;;     (suffix (if (>= ir-intval 0) "_ui" "_si")))
		;; (list (case c-return-type ;;this isn't correct for mpq (NSH 5-15-16)
		;; 	(|mpz| (format nil "mpz_set~a(~a, ~a)" suffix return-var ir-intval))
		;; 	(|mpq| (format nil "mpq_set~a(~a, ~a, 1)" suffix return-var ir-intval))
		;; 	(t (format nil "~a = (~a_t)~a" return-var c-return-type ir-intval)))))))


(defmethod ir2c* ((ir-expr ir-last) return-var return-type)
  (with-slots (ir-var) ir-expr
    (with-slots (ir-name ir-vtype) ir-var
      (let* ((ir2c-return-type (ir2c-type return-type))
	     (c-return-type (add-c-type-definition ir2c-return-type))
	     (ir2c-rhs-type  (ir2c-type ir-vtype))
	     (c-rhs-type (add-c-type-definition ir2c-rhs-type))
	     (c-assignments (copy-type ir2c-return-type ir2c-rhs-type return-var ir-name)))
					;(mk-c-assignment return-var c-return-type ir-name c-rhs-type)
					;(format t "~% last: ~a, return-var: ~a, c-return-type:~a, c-rhs-type: ~a" (print-ir ir-var)(print-ir return-var) c-return-type c-rhs-type)
	(case c-return-type
	  ((|mpq| |mpz|)
	   (let ((return-init-instrs nil) ; already initialized
		 ;; (list (format nil "~a = safe_malloc(sizeof(~a_t))"
		 ;; 		    return-var c-return-type)
		 ;; 	    (format nil "~a_init(~a)" c-return-type return-var))
		 )
	     (case c-rhs-type
	       ((|mpq| |mpz|)
		(append return-init-instrs
			(if nil		;(member ir-var *mpvar-parameters*)
			    c-assignments
			    (append c-assignments
				    (list (format nil "~a_clear(~a)"
					    c-rhs-type ir-name))))))
	       (t (append return-init-instrs
			  c-assignments)))))
	  (t (case c-rhs-type
	       ((|mpq| |mpz|)		;(break "ir-last")
		(if nil			;(member ir-var *mpvar-parameters*)
		    c-assignments
		    (append c-assignments
			    (list (format nil "~a_clear(~a)"
				    c-rhs-type ir-name)))))
	       (t			;(break "ir-last: rhs: ~a" ir-name)
		(if (ir-reference-type? ir2c-rhs-type)
		    (append c-assignments (list (release-last-var ir-var)))
		    c-assignments)))))))))

(defmethod ir2c* ((ir-expr ir-formal-typename) return-var return-type)
  (let ((c-return-type (add-c-type-definition return-type)))
    (list (mk-c-assignment return-var c-return-type (ir-type-id ir-expr) c-return-type))))

(defmethod ir2c* ((ir-expr ir-variable) return-var return-type)
  (with-slots (ir-name ir-vtype) ir-expr
    (let* ((ir2c-return-type (ir2c-type return-type))
	   (ir2c-vtype  (ir2c-type ir-vtype))
	   (c-return-type (add-c-type-definition ir2c-return-type))
	   ;;(c-rhs-type (add-c-type-definition ir2c-vtype))
	   (c-assignments (copy-type ir2c-return-type ir2c-vtype return-var ir-name)))
      (declare (ignore c-return-type))
      c-assignments)))

      ;; (case c-return-type
      ;; 	((|mpq| |mpz|)
      ;; 	 (cons (format nil "~a = safe_malloc(sizeof(~a_t))"
      ;; 		       return-var c-return-type)
      ;; 	       (cons (format nil "~a_init(~a)" c-return-type return-var)
      ;; 		     c-assignments)))
      ;; 	(t c-assignments)))));;copy increments reference count

(defmethod ir2c* ((ir-expr ir-type-actual) return-var return-type)
  (with-slots (ir-actual-type) ir-expr
    (let ((ir2c-return-type (ir2c-type return-type))
	  (ir2c-type (ir2c-type ir-actual-type))) ;(break "ir2c*(actual)")
      (if (ir-actualparameter-type? ir2c-type)
	  (let ((c-return-type (add-c-type-definition ir2c-return-type))
		(c-type (add-c-type-definition ir2c-type)))
	    (mk-c-assignment-with-count
	     return-var c-return-type
	     (format nil "actual_~a(~{~a~^,~})" c-type
		     (loop for ir-formal in *ir-theory-formals* collect (ir-name ir-formal)))
	     c-type))
	  (progn (break "actual")
		 (pvs2c-err "actual type must be a reference" ir-actual-type))))))

(defmethod ir2c* ((ir-expr ir-const-actual) return-var return-type)
  (with-slots (ir-actual-expr) ir-expr
	      (ir2c* ir-actual-expr return-var return-type)))

(defun gmp-type? (x) (or (eq x '|mpz|)(eq x '|mpq|)))

(defmethod ir-type-def ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
    (ir-type-def ir-type-defn)))

(defmethod ir-type-def ((ir-type t))
  ir-type)

(defmethod ir2c* ((ir-expr ir-record) return-var return-type)
  (with-slots (ir-fields ir-recordtype) ir-expr ;(break "ir2c*(ir-record)")
    (let ((ctype (add-c-type-definition (ir2c-type ir-recordtype)))
	  ;; (ir-field-types (if (ir-recordtype? )))
	  (c-return-type (add-c-type-definition (ir2c-type return-type)))
	  (tmpvar (gentemp "tmp")))  ;;tmp is transfered to result with refcount 1.
      (cons (format nil "~a_t ~a = new_~a();" ctype tmpvar ctype)
	    (cons (format nil "~a = (~a_t)~a" return-var c-return-type tmpvar)
		  (loop for fld in ir-fields
			as fldtype in (ir-field-types (ir-type-def ir-recordtype)) ;;NSH(8-27-22) was return-type
			append
			(with-slots (ir-fieldname ir-value) fld
			  (let* ((rhs-var (get-ir-last-var ir-value))
				 (c-rhs-type (add-c-type-definition (ir2c-type (ir-vtype rhs-var))))
				 (c-ftype (add-c-type-definition (ir2c-type (ir-vtype fldtype))))
				 (c-field-expr (format nil "~a->~a"  tmpvar ir-fieldname))
				 (c-field-assignment (make-c-assignment 
						      c-field-expr c-ftype
						      (ir-name rhs-var)
						      c-rhs-type))
				 (c-field-assign-instrs
				  (if (mpnumber-type? c-ftype)
				      (list (format nil "~a_init(~a)" c-ftype c-field-expr)
					    c-field-assignment)
				    (list c-field-assignment))))
					;(break "ir2c(ir-record)")
			    (append c-field-assign-instrs
				    (cond ((and (ir-reference-type? (ir-vtype rhs-var))
						(not (ir-last? ir-value)))
					   (list (format nil "~a->count++" (ir-name rhs-var))))
					  ((and (ir-last? ir-value)
						(gmp-type? c-rhs-type)
					;(not (member rhs-var *mpvar-parameters*))
						)
					   (list (format nil "~a_clear(~a)" c-rhs-type
							 (ir-name rhs-var))
						 ))
					  (t nil)))))))))))
						      

(defun ir-record-field-type (rectype field)
  (if (ir-stringtype? rectype)
      (if (eq field 'length) (mk-ir-subrange 0 '*) rectype)
    (with-slots (ir-field-types) rectype
      (ir-record-field-type-rec ir-field-types field))))

(defun ir-record-field-type-rec (field-types field)
  (cond ((consp field-types)
	 (with-slots (ir-id ir-vtype) (car field-types)
		     (or (and (eq ir-id field)
			      ir-vtype)
			 (ir-record-field-type-rec (cdr field-types) field))))
	(t nil)))

(defmethod get-ir-type-value ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
	      (get-ir-type-value ir-type-defn)))

(defmethod get-ir-type-value ((ir-type t))
;  (break "get-ir-type-value")
  ir-type)

(defun get-field-type (ir-recordtype field)
  (if (ir-stringtype? ir-recordtype) ;return input itself for the seq field
      (if (eq field 'length) (mk-ir-subrange 0 '*) ir-recordtype)
  (with-slots (ir-field-types) ir-recordtype
	      (loop for ftype in ir-field-types
		    when (eq (ir-id ftype) field)
		    return (ir-vtype ftype)))))

(defmethod ir2c* ((ir-expr ir-get) return-var return-type)
  (with-slots (ir-record ir-field) ir-expr
	      (let* ((ir-record-var (get-ir-last-var ir-record))
		     (c-return-type (add-c-type-definition (ir2c-type return-type)))
		     (record-var-type (get-ir-type-value
				       (ir-vtype ir-record-var)))
		     (field-type (get-field-type record-var-type ir-field))
		     (c-field-type (add-c-type-definition
					(ir2c-type field-type)))
		     (c-rhs (if (ir-stringtype? record-var-type)
				(if (eq ir-field 'length)
				    (format nil "strlen(~a)" (ir-name ir-record-var))
				    (format nil "~a" (ir-name ir-record-var)))
				(format nil "~a->~a" (ir-name ir-record-var) ir-field)))
		     (assign-instr
		      (make-c-assignment return-var c-return-type
				       c-rhs
				       c-field-type))
		     (assign-instrs
		      (case c-return-type
			((|mpq| |mpz|)
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

(defmethod ir-subrange-index?  ((ir-typ ir-subrange))
  (with-slots (ir-low ir-high ir-low-expr ir-high-expr) ir-typ
    (or ir-high-expr
	(and (integerp ir-low)
	     (integerp ir-high)
	     ;;(>= ir-high ir-low) ;;removing this since we have [0,-1] as an empty type.
	     (< ir-high *max-PVS-array-size*)
	     (1+ ir-high)))))

(defmethod ir-index? ((ir-typ ir-subrange))
  (with-slots (ir-low ir-high ir-high-expr) ir-typ
    (or ir-high-expr
	(and (eql (ir-low ir-typ) 0)
	     (integerp ir-high)
	     ;;(>= ir-high ir-low) ;;removing this since we have [0,-1] as an empty type.
	     (< ir-high *max-PVS-array-size*)
	     (1+ ir-high)))))

(defmethod ir-index? ((ir-type ir-typename))
  (with-slots (ir-type-defn) ir-type
	      (ir-index? ir-type-defn)))

(defmethod ir-index? ((ir-type t))
  nil)

;;only used when ir-index? is nil - returns the index type
(defun ir-hashable-index? (ctype)
  (case ctype
    ((|uint64| |__uint128| |int64| |__int128|) '|uint64|);64/128 get coerced to uint64.
    ((|int8| |int16| |int32| |uint8| |uint16| |uint32|) '|uint32|)
    ((|mpz|) '|mpz|)
    (t nil)))
	 ;;using ir2c-type to return u/intx or mpz

(defmethod ir-array? ((ir-typ ir-arraytype))
  (with-slots (size) ir-typ
	      size));;add one to high to get array size

(defmethod ir-array? ((ir-typ ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-typ
    (ir-array? ir-type-defn)))

(defmethod ir-array? ((ir-typ ir-funtype)) ;;can be removed for default case
  nil)
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
	   (ir-function-name (when (ir-function? ir-function)(ir-fname ir-function))));;(break "ir2c-function-apply")
      (if (ir-primitive-function? ir-function)
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
		      (mk-c-assignment return-var c-return-type
				       (format nil "~a(~a, ~a)" ir-fun-name (ir-name ir-funvar) arg-string)
				       (add-c-type-definition (ir2c-type (get-range (ir-vtype ir-funvar))))))
		      ;; (case c-return-type
		      ;; 	    ((|mpz| |mpq|)
		      ;; 	     (format nil "~a(~a, ~a, ~a)" ir-fun-name (ir-name ir-funvar) return-var arg-string))
		      ;; 	    (t (format nil "~a = (~a_t)~a(~a, ~a)"  return-var  c-return-type
		      ;; 		       ir-fun-name (ir-name ir-funvar) arg-string)))
		     (closure-release-instrs (when (ir-last? ir-function)
					       (list (format nil "~a->ftbl->rptr(~a)"
							     (ir-name ir-funvar)
							     (ir-name ir-funvar))))))
		(cons apply-instr closure-release-instrs))
	    (let* ((fdecl (declaration ir-function))
		   (function-ir (ir (eval-info fdecl)))
		   (ir-return-type (ir-return-type function-ir))
		   (function-has-closure-value? (and (null (ir-args function-ir))
						     (or (ir-funtype? ir-return-type)
							 (and (ir-typename? ir-return-type)
							      (ir-funtype? (ir-type-defn ir-return-type)))))))
	      (if function-has-closure-value?
		  (let* ((tmp (gentemp "tmp"))
			 (funvar (mk-ir-variable tmp ir-return-type))
			 (new-ir (mk-ir-let funvar ir-function
					    (mk-ir-apply (mk-ir-last funvar) ir-args))))
		    (ir2c* new-ir return-var return-type))
		(let* ((ir-funarg-types (or (and (cdefn (eval-info fdecl))
						 (op-arg-types (cdefn (eval-info fdecl))))
					    (loop for ir-formal in (ir-args function-ir)
						  collect (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))))
		       (c-fun-range-type (or (and (cdefn (eval-info fdecl))
						  (op-return-type (cdefn (eval-info fdecl))))
					     (add-c-type-definition (ir2c-type ir-return-type))))
		   
		       (arg-pairs (loop for argtype in ir-funarg-types
					as arg in ir-arg-var-names
					collect (case argtype
						  ((|mpz| |mpq|)(format nil "~a" arg))
						  (t (format nil "(~a_t)~a" argtype arg)))))
		       (arg-string (format nil "~{~a~^, ~}" arg-pairs)))
;	      (when (ir-function? ir-function-name) (break "ir-function"))
	       (make-c-application-assignment return-var c-return-type
					      ir-function-name arg-string
					      c-fun-range-type)))))))))
				       
	       ;; (format nil "~a = (~a_t)~a(~a)"  return-var (mppointer-type c-return-type)
	       ;; 		    ir-function-name arg-string)
		    ;; (case c-return-type
		    ;;   ((|mpz| |mpq|) (format nil "~a(~a, ~a)"  ir-function-name  return-var arg-string))
		    ;;   (t (format nil "~a = (~a_t)~a(~a)"  return-var c-return-type ir-function-name arg-string)))


(defun gmp-suffix (c-type)
  (case c-type
    ((|int8| |int16| |int32| |int64| |__int128|) "_si")
    ((|uint8| |uint16| |uint32| |uint64| |__uint128|) "_ui")
    (t "")))

;; (defun ir-primitive-op? (ir-function-name)
;;   (memq ir-function-name *ir-primitives*))

;; (defun ir-primitive-arith-op? (ir-function-name)
;;   (memq ir-function-name *ir-arith-primitives*))

(defun tweak-equal (ir-function-name)
  (case ir-function-name
    (= '==)
    (t ir-function-name)))

(defun ir2c-disequality (return-var c-return-type ir-arg-names c-arg-types arg-types)
  (case (car c-arg-types)
    ((|int8| |int16| |int32| |int64| |__int128| |uint8| |uint16| |uint32| |uint64| |__uint128| |mpz| |mpq|)
     (ir2c-arith-relations '!= return-var ir-arg-names c-arg-types))
    (t (if (ir-formal-typename? (car arg-types))
	   (list (format nil "~a = (~a_t) !~a->equal_ptr(~a, ~a, ~a)"
		   return-var c-return-type (car c-arg-types) (car ir-arg-names)
		   (cadr ir-arg-names)
		   (car c-arg-types)))
	   (let* ((theory-params *ir-theory-formals*)
		  (c-param-arg-string (format nil "~{, ~a~}"
					(loop for ir-formal in theory-params
					      collect (ir-name ir-formal)))))
	     (if (eq c-return-type 'bool)
		 (list (format nil "~a = (~a != ~a~a)"
			 return-var (car ir-arg-names) (cadr ir-arg-names) c-param-arg-string))
		 (list (format nil "~a = (~a_t) !equal_~a(~a, ~a~a)"
			 return-var c-return-type (car c-arg-types) (car ir-arg-names)
			 (cadr ir-arg-names) c-param-arg-string))))))))


(defun ir2c-equality (return-var c-return-type ir-arg-names c-arg-types arg-types)
  (case (car c-arg-types)
    ((|int8| |int16| |int32| |int64| |__int128| |uint8| |uint16| |uint32| |uint64| |__uint128| |mpz| |mpq|)
     (ir2c-arith-relations '== return-var ir-arg-names c-arg-types))
    (|bool| (list (format nil "~a = (~a == ~a)" return-var (car ir-arg-names)(cadr ir-arg-names))));;NSH(5-15-24)
    (t (if (ir-formal-typename? (car arg-types))
	   (list (format nil "~a = (~a_t) ~a->equal_ptr(~a, ~a, ~a)"
			 return-var c-return-type (car c-arg-types) (car ir-arg-names)
			 (cadr ir-arg-names)
			 (car c-arg-types)))
	 (let* ((theory-params *ir-theory-formals*)
		(c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								 theory-params
								 collect (ir-name ir-formal)))))
	 (list (format nil "~a = (~a_t) equal_~a(~a, ~a~a)"
		       return-var c-return-type (car c-arg-types) (car ir-arg-names)
		       (cadr ir-arg-names) c-param-arg-string)))))))

(defun numtype-abbrev (ctype)
  (case ctype
    ((|int8| |int16| |int32| |int64| |int128|) '|i64|)
    ((|uint8| |uint16| |uint32| |uint64| |uint128|) '|u64|)
    (|mpz| '|z|)
    (|mpq| '|q|)
    (t "")))

(defun ir2c-primitive-apply (return-var return-type ir-function-name ir-args ir-arg-names)
;  (cond ((ir-primitive-function-name? ir-function-name);
  (let* ((arg-types (loop for ir-var in ir-args
			  collect (ir2c-type (ir-vtype ir-var))))
	 (c-arg-types (loop for ir-type in arg-types
			   collect (add-c-type-definition ir-type)))
	(c-return-type (add-c-type-definition (ir2c-type return-type)))
					;(arity (length ir-args))
	)
					;(format t "~%c-arg-types: ~a" c-arg-types)
    (case ir-function-name
      (+ (ir2c-addition return-var c-return-type ir-arg-names c-arg-types))
      (- (ir2c-subtraction return-var c-return-type ir-arg-names c-arg-types))
      (* (ir2c-multiplication  return-var c-return-type ir-arg-names c-arg-types))
      (/ (ir2c-division return-var c-return-type ir-arg-names c-arg-types))
      (= (ir2c-equality return-var c-return-type ir-arg-names c-arg-types arg-types))
      (/= (ir2c-disequality return-var c-return-type ir-arg-names c-arg-types arg-types))
      ((|ndiv| |nrem|) (ir2c-divrem ir-function-name return-var c-return-type ir-arg-names c-arg-types))
      ((< <= > >=) (ir2c-arith-relations ir-function-name ;(tweak-equal ir-function-name)
					 return-var
					 ir-arg-names c-arg-types))
      (|floor| (list (format nil "~a = (~a_t)pvsfloor_~a_~a(~a)"
			   return-var (mppointer-type return-type)
			   (numtype-abbrev (car c-arg-types))
			   (numtype-abbrev return-type)
			   (car ir-arg-names))))
      (|ceiling| (list (format nil "~a = (~a_t)pvsceiling_~a_~a(~a)"
			     return-var (mppointer-type return-type)
			     (numtype-abbrev (car c-arg-types))
			     (numtype-abbrev return-type)
			     (car ir-arg-names))))
      (|char?| (list (format nil "~a = (~a_t)(~a < 0x110000)"
			   return-var return-type (car ir-arg-names))))
      (|ord| (list (format nil "~a = (~a_t) 0"
			   return-var return-type)))
      ((NOT ¬) (list (format nil "~a = !~a" return-var (car ir-arg-names) )))
      ((OR ∨)
       (list (format nil "~a = ~a || ~a" return-var (car ir-arg-names) 
		     (cadr ir-arg-names))))
      ((AND & ∧)
       (list (format nil "~a = ~a && ~a" return-var (car ir-arg-names) 
		     (cadr ir-arg-names))))
      ((IMPLIES => ⇒) (list (format nil "~a = (!~a) ||  ~a" return-var (car ir-arg-names)
			     (cadr ir-arg-names))))
      (WHEN (list (format nil "~a = ~a || ! ~a" return-var (car ir-arg-names)
			  (cadr ir-arg-names))))
      ((IFF <=> ⇔) (list (format nil "~a = (~a || ! ~a) && ((!~a) ||  ~a)" return-var
			 (car ir-arg-names)  (cadr ir-arg-names)
			 (car ir-arg-names)  (cadr ir-arg-names))))
      ((|code| |char|) (list (format nil "~a = (uint32_t) ~a" return-var (car ir-arg-names))))
      (t (list (format nil "~a = ~a(~{~a~^, ~})" return-var ir-function-name ir-arg-names)))
      )))
	;(t (break "not defined"))))

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
    ((|uint8| |uint16| |uint32| |uint64| |__uint128|) "ui")
    ((|int8| |int16| |int32| |int64| |__int128|) "si")
    (t "")))

(defun uint-or-int (c-type)
  (case c-type
    ((|uint8| |uint16| |uint32| |uint64| |__uint128|) "uint")
    ((|int8| |int16| |int32| |int64| |__int128|) "int")
    (t "")))

(defun fixnum-type (c-type)
  (case c-type
    ((|uint8| |uint16| |uint32| |uint64| |__uint128|) t)
    ((|int8| |int16| |int32| |int64| |__int128|) t)
    (t nil)))
  

(defun ir2c-division-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (|mpq| (case arg1-c-type
	   (|mpq|
	    (case arg2-c-type
	      (|mpq| (list (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 arg2)))
	      (|mpz| (let ((arg2-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg2-mpq-var)
			   (format nil "mpq_init(~a)" arg2-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 arg2-mpq-var)
			   (format nil "mpq_clear(~a)" arg2-mpq-var)
			   )))
	      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	       (list (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
			     (gmp-ui-or-si arg2-c-type)
			     return-var (uint-or-int arg2-c-type) arg2)
		     (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 return-var)))
	      (t (break "Not implemented yet"))))
	   (|mpz|
	    (case arg2-c-type
	      (|mpq| (let ((arg1-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg1-mpq-var)
			   (format nil "mpq_init(~a)" arg1-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var arg2)
			   (format nil "mpq_clear(~a)" arg1-mpq-var)
			   )))
	      (|mpz| (let ((arg1-mpq-var (gentemp "tmp"))
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
	      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	       (let ((arg1-mpq-var (gentemp "tmp")))
		 (list (format nil "mpq_t ~a" arg1-mpq-var)
		       (format nil "mpq_init(~a)" arg1-mpq-var)
		       (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
		       (format nil "mpq_mk_set_~a(~a, (~a_t)~a, 1)"
			       (gmp-ui-or-si arg1-c-type)
			       return-var (uint-or-int arg1-c-type) arg2)
		       (format nil "mpq_mk_div(~a, ~a, ~a)" return-var
			       arg1-mpq-var return-var)
		       (format nil "mpq_clear(~a)" arg1-mpq-var)
		       )))
	      (t (break "Not implemented yet."))))
	   ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	     (case arg2-c-type
	       (|mpq| (let ((tmp (gentemp "tmp")))
		      (list (format nil "mpq_t ~a" tmp)
			    (format nil "mpq_init(~a)" tmp)
			    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
				  (gmp-ui-or-si arg1-c-type)
				  tmp (uint-or-int arg1-c-type) arg1)
			  (format nil "mpq_mk_div(~a, ~a, ~a)" return-var tmp arg2))))
	       (|mpz| (let ((arg1-mpq-var (gentemp "tmp"))
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
	       ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
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
    ((|__int128| |__uint128|) (break "Not implemented yet."))
    (|mpz| (let* ((mpq-return-var (gentemp "tmp"))
		(mpq-instrs (ir2c-division-step mpq-return-var "mpq" arg1 arg1-c-type arg1 arg2-c-type)))
	   (cons (format nil "mpq_t ~a" mpq-return-var)
		 (nconc mpq-instrs
			(list (format nil "mpz_mk_set_q(~a, ~a)" return-var mpq-return-var)
			      (format nil "mpq_clear(~a)" mpq-return-var)
			      )))))
    ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
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
	(opname (if (eq op '|nrem|) '|rem| '|div|)))
    (let ((arg1-type (case arg1-c-type
		       ((|uint8| |uint16| |uint32|) '|uint32|)
		       ((|int8| |int16| |int32|) '|int32|)
		       (t arg1-c-type)))
	  (arg2-type (case arg2-c-type
		       ((|uint8| |uint16| |uint32|) '|uint32|)
		       (t arg2-c-type))))
      (case c-return-type
	(|mpz|
	 (case arg1-type
	   (|mpz| (case arg2-type
		  (|mpz| (list (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				     (if (eq opname '|rem|) '|r| '|q|) return-var arg1 arg2)))
		  (t (list (format nil "mpz_mk_fdiv_~a_ui(~a, ~a, ~a)"
				   (if (eq opname '|rem|) '|r| '|q|) return-var arg1 arg2)))))
	   ((|uint32| |uint64|); |__uint128|)
	    (case arg2-type
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_ui(~a, ~a)" tmp arg1)
			   (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				   (if (eq opname '|rem|) '|r| '|q|) return-var tmp arg2)
			   (format nil "mpz_clear(~a)" tmp)
			   )))
		(t (list (format nil "mpz_mk_set_ui(~a, ~a_~a_~a(~a, ~a))"
				 return-var opname arg1-type arg2-type arg1 arg2)))))
	   ((|int32| |int64|); |__int128|)
	    (case arg2-type 
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_si(~a, ~a)" tmp arg1)
			   (format nil "mpz_mk_fdiv_~a(~a, ~a, ~a)"
				   (if (eq opname '|rem|) '|r| '|q|) return-var tmp arg2)
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
		    arg1 arg1-c-type arg2 arg2-c-type);(break "ir2c-arith-relations-step")
    (case arg1-c-type
      ((|int8| |int16| |int32| |int64|); |__int128|)
       (case arg2-c-type
	 ((|int8| |int16| |int32| |int64|); |__int128|)
	  (list (format nil "~a = (~a ~a ~a)"
			return-var arg1 ir-function-name arg2)))
	 ((|uint8| |uint16| |uint32| |uint64| |__uint128|)
	  (list (mk-if-instr (format nil "(~a < 0)" arg1)
			     (list (format nil "~a = ~a"
					   return-var
					   (case ir-function-name
					     ((< <=) "true")
					     (t "false"))))
			     (list (format nil "~a = ((~a_t)~a ~a ~a)"
					   return-var arg2-c-type
					   arg1 ir-function-name arg2)))))
	 (|mpz| (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpz_cmp_si(~a, ~a, 1)" tmp arg2 arg1)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      (arith-relation-inverse ir-function-name)))))
	 (|mpq| (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpq_cmp_si(~a, ~a, 1)" tmp arg2 arg1)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      (arith-relation-inverse ir-function-name)))))))
      ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
       (case arg2-c-type
	 ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	  (list (format nil "~a = (~a ~a ~a)"
			return-var arg1 ir-function-name arg2)))
	 ((|int8| |int16| |int32| |int64|); |__int128|)
	  (list (mk-if-instr (format nil "(~a < 0)" arg2)
			     (list (format nil "~a = ~a"
					   return-var
					   (case ir-function-name
					     ((< <=) "false")
					     (t "true"))))
			     (list (format nil "~a = (~a ~a (~a_t)~a)"
					   return-var 
					   arg1 ir-function-name arg1-c-type arg2)))))
	 (|mpz| (let ((tmp (gentemp "tmp")))
		      (list (format nil "int64_t ~a = mpz_cmp_ui(~a, ~a)" tmp arg2 arg1)
			    (format nil "~a = (~a ~a 0)" return-var tmp
				    ir-function-name))))
	 (|mpq| (let ((tmp (gentemp "tmp")))
		      (list (format nil "int64_t ~a = mpq_cmp_ui(~a, ~a, 1)" tmp arg2 arg1)
			    (format nil "~a = (~a ~a 0)" return-var tmp
				    ir-function-name))))))
      (|mpz| (case arg2-c-type
	     ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|);|__uint128| |__int128|)
	      (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpz_cmp_~a(~a, ~a)" tmp
			      (gmp-ui-or-si arg2-c-type)
			   arg1 arg2)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      ir-function-name))))
	     (|mpz| (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpz_cmp(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name))))
	     (|mpq| (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp_z(~a, ~a)" tmp
				  arg2 arg1)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  (arith-relation-inverse ir-function-name)))))))
      (|mpq| (case arg2-c-type
	     ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|);|__uint128| |__int128|)
	      (let ((tmp (gentemp "tmp")))
		(list (format nil "int64_t ~a = mpq_cmp_~a(~a, ~a, 1)" tmp
			      (gmp-ui-or-si arg2-c-type)
			   arg1 arg2)
		      (format nil "~a = (~a ~a 0)" return-var tmp
			      ir-function-name))))
	     (|mpz| (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp_z(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name))))
	     (|mpq| (let ((tmp (gentemp "tmp")))
		    (list (format nil "int64_t ~a = mpq_cmp(~a, ~a)" tmp
				  arg1 arg2)
			  (format nil "~a = (~a ~a 0)" return-var tmp
				  ir-function-name)
			  )))))))


(defun arith-relation-inverse (ir-relation)
  (case ir-relation
    (== '==)
    (< '>=)
    (<= '>)
    (> '<=)
    (>= '<)
    (!= '!=)))

(defun intsize (u)
  (case u
    ((|int8| |uint8|) 8)
    ((|int16| |uint16|) 16)
    ((|int32| |uint32|) 32)
    (t 64)))

(defun uintlub (&rest args)
  (let ((max (apply #'max (loop for u in args collect (intsize u)))))
    (format nil "uint~a" max)))
  
  
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
    (|mpz| (case arg1-c-type
	   ((|uint8| |uint16| |uint32| |uint64|);  |__uint128|
	    (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64|)
	       (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg1)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
	      ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
	      ((|int8| |int16| |int32| |int64|)
	       (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg1)))
	      (|mpz| (list (format nil "mpz_mk_add_ui(~a, ~a, (uint64_t)~a)" return-var arg2 arg1)))
	      (|mpq| (list (format nil "mpz_mk_set_q(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_ui(~a, ~a, ~a)" return-var return-var arg1)))))
	   ((|int8| |int16| |int32| |int64|)
	    (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64|)
	       (list (format nil "mpz_mk_set_si(~a, (uint64_t)~a)" return-var arg1)
		     (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
	      ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
	      ((|int8| |int16| |int32| |int64|)
	       (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg1)
		     (format nil "mpz_add_si(~a, ~a (int64_t)~a)" return-var return-var arg2)))
	      (|mpz| (list (format nil "mpz_mk_add_si(~a, ~a, (int64_t)~a)" return-var arg2 arg1)))
	      (|mpq| (list (format nil "mpz_mk_set_q(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_si(~a, ~a, ~a)" return-var return-var arg1)))))
	   (|mpz| (case arg2-c-type
		  ((|uint8| |uint16| |uint32| |uint64|)
		   (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg2)
			 (format nil "mpz_add(~a, ~a, ~a)" return-var return-var arg1)))
		  ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		  ((|int8| |int16| |int32| |int64|)
		   (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
			 (format nil "mpz_add(~a, ~a, ~a)" return-var return-var arg1)))
		  (|mpz| (list (format nil "mpz_mk_add(~a, ~a, ~a)" return-var arg2 arg1)))
		  (|mpq| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpz_t ~a" tmp)
			       (format nil "mpz_init(~a)" tmp)
			       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
			       (format nil "mpz_mk_add(~a, ~a, ~a)" return-var arg1 tmp)
			       (format nil "mpz_clear(~a)" tmp)
			       )))))
	   (|mpq| (case arg2-c-type
		  ((|uint8| |uint16| |uint32| |uint64|)
		   (list (format nil "mpz_mk_set_q(~a, ~a)" return-var arg1)
			 (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
		  ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		  ((|int8| |int16| |int32| |int64|)
		   (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
			 (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var arg2 arg1)))
		  (|mpz| (list (format nil "mpz_mk_add(~a, ~a, ~a)" return-var arg2 arg1)))
		  (|mpq| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_mk_add(~a, ~a, ~a)" tmp arg1 arg2)
			       (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp)
			       (format nil "mpq_clear(~a)" tmp))
			       ))))
	   (t (break "Not implemented."))))
    ;;;working on this 
    (|mpq| (case arg1-c-type
	   ((|uint8| |uint16| |uint32| |uint64|);  |__uint128|
	    (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg1)
		       (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" tmp tmp arg2)
		       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
	      ((|int8| |int16| |int32| |int64|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg1)
		       (format nil "mpz_add_si(~a, ~a, (int64_t)~a)" tmp tmp arg2)
		       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set(~a, ~a)" tmp arg2)
			   (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" tmp tmp arg1)
			   (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			   (format nil "mpz_clear(~a)" tmp))))
	      (|mpq| (list (format nil "mpq_mk_set_ui(~a, ~a)" return-var arg1)
			 (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg2)))))
	   ((|int8| |int16| |int32| |int64|)
	    (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_si(~a, (int64_t)~a)" tmp arg1)
		       (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" tmp tmp arg2)
		       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
	      ((|int8| |int16| |int32| |int64|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_si(~a, (int64_t)~a)" tmp arg1)
		       (format nil "mpz_add_si(~a, ~a, (int64_t)~a)" tmp tmp arg2)
		       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set(~a, ~a)" tmp arg2)
			   (format nil "mpz_add_si(~a, ~a, (int64_t)~a)" tmp tmp arg1)
			   (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			   (format nil "mpz_clear(~a)" tmp))))
	      (|mpq| (list (format nil "mpq_mk_set_si(~a, ~a)" return-var arg1)
			 (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg2)))))
	   (|mpz| (case arg2-c-type
		  ((|uint8| |uint16| |uint32| |uint64|)
		   (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg2)
			   (format nil "mpz_add(~a, ~a, ~a)" tmp tmp arg1)
			   (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			   (format nil "mpz_clear(~a)" tmp))))
		  ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		  ((|int8| |int16| |int32| |int64|)
		   (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_set_si(~a, (int64_t)~a)" tmp arg2)
			   (format nil "mpz_add(~a, ~a, ~a)" tmp tmp arg1)
			   (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			   (format nil "mpz_clear(~a)" tmp))))
		  (|mpz| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpz_t ~a" tmp)
			       (format nil "mpz_init(~a)" tmp)
			       (format nil "mpz_set(~a, (int64_t)~a)" tmp arg1)
			       (format nil "mpz_add(~a, ~a, ~a)" tmp tmp arg2)
			       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			       (format nil "mpz_clear(~a)" tmp))))
		  (|mpq| (list (format nil "mpq_mk_set_z(~a ~a)" return-var arg1)
			     (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg2)))
		  ))
	   (|mpq| (case arg2-c-type
		  ((|uint8| |uint16| |uint32| |uint64|)
		   (list (format nil "mpq_mk_set_ui(~a, (uint64_t)~a)" return-var arg2)
			 (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg1)))
		  ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		  ((|int8| |int16| |int32| |int64|)
		   (list (format nil "mpq_mk_set_si(~a, (int64_t)~a)" return-var arg2)
			 (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg1)))
		  (|mpz| (list (format nil "mpq_mk_set_z(~a, (int64_t)~a)" return-var arg2)
			 (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg1)))
		  (|mpq| (list (format nil "mpq_mk_set(~a, ~a)" return-var arg1)
			     (format nil "mpq_add(~a, ~a, ~a)" return-var return-var arg2)))
			       ))
	   (t (break "Not implemented."))))
    ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
     (case arg1-c-type
       ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type
			 arg1 arg2)));args don't need to be cast
	  ((|int8| |int16| |int32| |int64|); |__int128|)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)((~a_t)~a - (~a_t)(-~a)) ;} else {~a =  (~a_t)(~a + ~a);}"
	   arg2   ;;since return type is unsigned, subtraction is always non-negative.
	   return-var c-return-type lub lub arg1 arg2
	   return-var c-return-type arg1  arg2))))
	  (|mpz| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_ui(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	 (|mpq| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_ui(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       ((|int8| |int16| |int32| |int64| |__int128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64| |__uint128|)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg1
					     return-var c-return-type arg2 lub arg1
					     return-var c-return-type arg1  arg2))))
	  ((|int8| |int16| |int32| |int64|);  |__int128|)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type arg1 arg2)))
	  (|mpz| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	  (|mpq| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       (|mpz| (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
       (|mpq| (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
       (t (break "Not implemented."))))
    ((|int8| |int16| |int32| |int64|) ; |__int128|)
     (case arg1-c-type
       ((|uint8| |uint16| |uint32| |uint64|);  |__uint128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64|);  |__uint128|)
	   (list (format nil "~a = (~a_t)(~a + ~a)" return-var c-return-type arg1 arg2)))
	  ((|int8| |int16| |int32| |int64|);  |__int128|)
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg2
					     return-var c-return-type arg1 lub arg2
					     return-var c-return-type arg1  arg2))))
	  (|mpz| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp arg2 arg1)
		       (format nil "~a = (~a_t)mpz_get_ui(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	  (|mpq| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_q(~a, ~a)" tmp arg2)
		       (format nil "mpz_add_si(~a, ~a, ~a)" tmp tmp arg1)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))))
       ((|int8| |int16| |int32| |int64|); |__int128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64|); |__uint128|);
	   (let ((lub (uintlub c-return-type arg1-c-type arg2-c-type)))
	     (list (format nil "if (~a < 0){~a =  (~a_t)(~a - (~a_t)(-~a));} else {~a =  (~a_t)(~a + ~a);}"
					     arg1
					     return-var c-return-type arg2 lub arg1
					     return-var c-return-type arg1  arg2))))
	  ((|int8| |int16| |int32| |int64|); |__int128|)
	   (list (format nil "~a = (~a_t)((int64_t)~a + (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (|mpz| (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))
	  (|mpq| (ir2c-addition-step return-var c-return-type arg2 arg2-c-type arg1 arg1-c-type))))
       (|mpz| (case arg2-c-type
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_add(~a, ~a, ~a)" tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_si(~a)"
				   return-var c-return-type tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   )))
	      (|mpq| (let ((tmp1 (gentemp "tmp"))
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
	      ((|__int128| |__uint128|)(break "128-bit GMP arithmetic not available."))
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
    
  ;;   (|uint32| (case arg2-c-type
  ;; 		  (|uint32| (case c-return-type
  ;; 			      (|uint32|
  ;; 			       (list (format nil "~a = ~a + ~a" return-var arg1 arg2)))
  ;; 			      (|int32| (format nil "~a = (int32_t) (~a + ~a)" return-var arg1 arg2))
  ;; 			      (|mpz| (list (format nil "mpz_set_ui(~a, ~a)"
  ;; 						   return-var arg1)
  ;; 					   (format nil "mpz_add_ui(~a, ~a, ~a)" return-var return-var arg2)))
  ;; 			      (|mpq| (break "deal with this later"))))
  ;; 		  (|int32| (case c-return-type
  ;; 			   (|uint32| 
  ;; 			    (list (format nil "if (~a < 0){~a =  (~a - (~a_t)(-~a));} else {~a =  (~a + (~a_t)~a);}"
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type arg2)))
  ;; 			   (|int32| (list (format nil "if (~a < 0){if ((uint32_t)(-~a) <= ~a){~a = (int32_t)(~a - (uint32_t)(-~a));} else {~a = (int32_t)~a + ~a;};} else {~a = (int32_t)(~a + (uint32_t)~a);}"
  ;; 					     arg2 arg2 arg1 return-var arg1 arg2
  ;; 					     return-var arg1 arg2
  ;; 					     return-var arg1 arg2)))
  ;; 			   (|mpz| (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "if (~a < 0){mpz_sub_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_add_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 		  (|mpz| (let ((tmp (gentemp "tmp")))
  ;; 			 (case c-return-type
  ;; 			   ((|uint32| |int32|)
  ;; 			    (ir2c-addition-step  return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type))
  ;; 			   (|mpz| (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "mpz_add(~a, ~a, ~a)" return-var return-var arg2))))))))
  ;;     (|int32| (case arg2-c-type
  ;; 	       (|int32| (case c-return-type
  ;; 			(|uint32| (list (format nil "~a = (~a_t) (~a + ~a)"
  ;; 					      return-var c-return-type arg1 arg2)))
  ;; 			(|int32| (list (format nil "~a = ~a + ~a"
  ;; 					     return-var arg1 arg2)))
  ;; 			(|mpz| (list (format nil "mpz_set_si(~a, ~a)" return-var arg1)
  ;; 				   (format nil "if (~a < 0){mpz_sub_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_add_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 	       (t (ir2c-addition-step return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type ))))
  ;;     (|mpz| (case c-return-type
  ;; 	     ((|uint32| |int32|)
  ;; 	      (case arg2-c-type
  ;; 		(|uint32| (ir2c-addition-step return-var c-return-type arg2 arg2-c-type
  ;; 					      arg1 arg1-c-type))
  ;; 		(|int32| (let ((tmp (gentemp "tmp")))
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
  ;; 		(|mpz| (let ((tmp (gentemp "tmp")))
  ;; 		       (list (format nil "mpz_t ~a" tmp)
  ;; 			     (format nil "mpz_init(~a)" tmp)
  ;; 			     (format nil "mpz_add(~a, ~a, ~a)"
  ;; 				     tmp arg1 arg2)
  ;; 			     (format nil "~a = mpz_get~a(~a, ~a)"
  ;; 				     return-var
  ;; 				     (gmp-suffix c-return-type)
  ;; 				     tmp)
  ;; 			     (format nil "mpz_clear(~a)" tmp))))))
  ;; 	     (|mpz| (list (format nil "mpz_set~a(~a, ~a)"
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
    ((|mpz| |mpq|)
     (case arg1-c-type
       ((|mpz| |mpq|)
	(list (format nil "~a_mk_set~a(~a, ~a)" c-return-type (if (eq c-return-type arg1-c-type) ""
							     (if (eq arg1-c-type '|mpz|) "_z" "_q"))
		      return-var arg1)
	      (format nil "~a_neg(~a, ~a)" c-return-type return-var return-var)))
       (t (list (format nil "~a_mk_set_~a(~a, ~a)" c-return-type (gmp-ui-or-si arg1-c-type)
			return-var arg1)
		(format nil "~a_neg(~a, ~a)" c-return-type return-var return-var)))))
    (t (case arg1-c-type
	 ((|mpz| |mpq|)
	  (list (format nil "~a = (~a_t) ~a_get_si(~a)"  return-var c-return-type arg1-c-type arg1)))
	 (t 
	  (list (format nil "~a = (~a_t)-~a" return-var c-return-type arg1)))))))

(defun ir2c-subtraction-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
    (case  c-return-type
      (|mpz| (case arg1-c-type
	     ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	      (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64|)
		 (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg1)
		       (format nil "mpz_sub_ui(~a, ~a, (uint64_t)~a)" return-var return-var arg2)))
		((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		((|int8| |int16| |int32| |int64|)
		 (list (format nil "mpz_mk_set_si(~a, -~a)" return-var arg2)
		       (format nil "mpz_add_ui(~a, ~a, (uint64_t)~a)" return-var return-var  arg1)))
		(|mpz| (list (format nil "mpz_ui_sub(~a, (uint64_t)~a,  ~a)" return-var arg1 arg2)))
		(|mpq| (let ((tmp1 (gentemp "tmp"))
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
	     ((|int8| |int16| |int32| |int64|)
	      (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64|)
		 (list (format nil "mpz_mk_set_ui(~a, (uint64_t)~a)" return-var arg2)
		       (format nil "mpz_si_sub(~a, (int64_t)~a, ~a)" return-var arg1 return-var)))
		((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		((|int8| |int16| |int32| |int64|)
		 (list (format nil "mpz_mk_set_si(~a, ~a)" return-var arg2)
		       (format nil "mpz_si_sub_si(~a, (int64_t)~a, ~a)" return-var arg1 return-var)))
		(|mpz| (list (format nil "mpz_mk_set_si(~a, (int64_t)~a)" return-var arg1)
			   (format nil "mpz_sub(~a, ~a, ~a)" return-var return-var arg2)))
		(|mpq| (let ((tmp1 (gentemp "tmp"))
			   (tmp2 (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp1)
			     (format nil "mpq_t ~a" tmp2)
			     (format nil "mpq_set_si(~a, ~a, 1)" tmp1 arg1)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
			     (format nil "mpq_clear(~a)" tmp1)
			     (format nil "mpq_clear(~a)" tmp2)
			     )))))
	      (|mpz| (case arg2-c-type
		     ((|uint8| |uint16| |uint32| |uint64|)
		      (list (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var arg1 arg2)))
		     ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		     ((|int8| |int16| |int32| |int64|)
		      (list (format nil "mpz_mk_sub_si(~a, ~a (int64_t)~a)" return-var arg1 arg2)))
		     (|mpz| (list (format nil "mpz_mk_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     (|mpq| (let ((tmp1 (gentemp "tmp"))
				(tmp2 (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp1)
			     (format nil "mpq_t ~a" tmp2)
			     (format nil "mpq_set_z(~a, ~a)" tmp1 arg1)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
			     (format nil "mpq_clear(~a)" tmp1)
			     (format nil "mpq_clear(~a)" tmp2)
			     )))))
	      (|mpq| (case arg2-c-type
		     ((|uint8| |uint16| |uint32| |uint64|)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpz_t ~a" tmp)
			      (format nil "mpz_init(~a)" tmp)
			      (format nil "mpz_set_q(~a, ~a)" tmp arg1)
			      (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var tmp arg2)
			      (format nil "mpz_clear(~a)" tmp))))
		     ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		     ((|int8| |int16| |int32| |int64|)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpz_t ~a" tmp)
			      (format nil "mpz_init(~a)" tmp)
			      (format nil "mpz_set_q(~a, ~a)" tmp arg1)
			      (format nil "mpz_mk_sub_si(~a, ~a (int64_t)~a)" return-var tmp arg2)
			      (format nil "mpz_clear(~a)" tmp))))
		     (|mpz| (let ((tmp (gentemp "tmp")))
			    (list (format nil "mpz_t ~a" tmp)
				  (format nil "mpz_init(~a)" tmp)
				  (format nil "mpz_set_q(~a, ~a)" tmp arg1)
				  (format nil "mpz_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
				  (format nil "mpz_clear(~a)" tmp))))
		     (|mpq| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_sub(~a, ~a, ~a)" tmp arg1 arg2)
			     (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp)
			     (format nil "mpq_clear(~a)" tmp)
			     )))))
	      (t (break "Not implemented"))))
      (|mpq| (case arg1-c-type
	     ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	      (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg1)
			 (format nil "mpz_sub_ui(~a, ~a, (uint64_t)~a)" tmp tmp arg2)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		((|int8| |int16| |int32| |int64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a, 1)" tmp arg1)
			 (format nil "mpz_sub_si(~a, ~a, (int64_t)~a)" tmp tmp arg2)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		(|mpz| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_ui_sub(~a, (uint64_t)~a,  ~a)" tmp arg1 arg2)
			     (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			     (format nil "mpz_clear(~a)" tmp))))
		(|mpq| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_set_ui(~a, ~a, 1)" tmp arg1)
			     (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
			     (format nil "mpq_clear(~a)" tmp)
			     )))
		(t (break "Not implemented"))))
	     ((|int8| |int16| |int32| |int64|)
	      (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_ui(~a, (uint64_t)~a)" tmp arg2)
			 (format nil "mpz_si_sub(~a, (int64_t)~a, ~a)" tmp arg1 tmp)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		((|int8| |int16| |int32| |int64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_si(~a, ~a)" tmp arg2)
			 (format nil "mpz_si_sub_si(~a, (int64_t)~a, ~a)" tmp arg1 tmp)
			 (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			 (format nil "mpz_clear(~a)" tmp))))
		(|mpz| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_si_sub(~a, (int64_t)~a,  ~a)" tmp arg1 arg2)
			     (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp)
			     (format nil "mpz_clear(~a)" tmp))))
		(|mpq| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpq_t ~a" tmp)
			     (format nil "mpq_init(~a)" tmp)
			     (format nil "mpq_set_si(~a, ~a, 1)" tmp arg1)
			     (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var tmp arg2)
			     (format nil "mpq_clear(~a)" tmp))))
		))
	      (|mpz| (case arg2-c-type
		     ((|uint8| |uint16| |uint32| |uint64|)
		      (list (format nil "mpz_mk_sub_ui(~a, ~a, (uint64_t)~a)" return-var arg1 arg2)))
		     ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		     ((|int8| |int16| |int32| |int64|)
		      (list  (format nil "mpz_mk_sub_si(~a, ~a, (int64_t)~a)" return-var arg1 arg2)))
		     (|mpz| (list (format nil "mpz_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     (|mpq| (let ((tmp1 (gentemp "tmp"))
				(tmp2 (gentemp "tmp")))
			    (list (format nil "mpq_t ~a" tmp1)
				  (format nil "mp1_t ~a" tmp2)
				  (format nil "mpq_set_z(~a, ~a)" tmp1 arg1)
				  (format nil "mpq_sub(~a, ~a, ~a)" tmp2 tmp1 arg2)
				  (format nil "mpz_mk_set_q(~a, ~a)" return-var tmp2)
				  (format nil "mpq_clear(~a)" tmp1)
				  (format nil "mpq_clear(~a)" tmp2)
				  )))))
	      (|mpq| (case arg2-c-type
		     ((|uint8| |uint16| |uint32| |uint64|)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpq_t ~a" tmp)
			      (format nil "mpq_init(~a)" tmp)
			      (format nil "mpq_set_ui(~a, ~a, 1)" tmp arg2)
			      (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
			      (format nil "mpq_clear(~a)" tmp))))
		     ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		     ((|int8| |int16| |int32| |int64|)
		      (let ((tmp (gentemp "tmp")))
			(list (format nil "mpq_t ~a" tmp)
			      (format nil "mpq_init(~a)" tmp)
			      (format nil "mpq_set_si(~a, ~a, 1)" tmp arg2)
			      (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
			      (format nil "mpq_clear(~a)" tmp))))
		     (|mpz| (let ((tmp (gentemp "tmp")))
			    (list (format nil "mpq_t ~a" tmp)
				  (format nil "mpq_init(~a)" tmp)
				  (format nil "mpq_set_z(~a, ~a)" tmp arg2)
				  (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 tmp)
				  (format nil "mpq_clear(~a)" tmp))))
		     (|mpq| (list (format nil "mpq_mk_sub(~a, ~a, ~a)" return-var arg1 arg2)))
		     ))
	      (t (break "Not implemented"))))
      ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
       (case arg1-c-type
	 ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	  (case arg2-c-type
	    ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	     (list (format nil "~a = (~a_t)(~a - ~a)" return-var c-return-type arg1 arg2)))
	    ((|int8| |int16| |int32| |int64|); |__int128|)
	     (list (format nil "if (~a < 0){~a =  (~a_t)((uint64_t)~a + (~a_t)-~a);} else {~a =  (~a_t)((uint64_t)~a - (uint64_t)~a);}"
			   arg2
			   return-var c-return-type arg1 c-return-type arg2
			   return-var c-return-type arg1  arg2)))
	    (|mpz| (let ((tmp (gentemp "tmp")))
		   (list (format nil "~a_t ~a" arg1-c-type tmp)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" tmp arg1-c-type arg2)
			 (format nil "~a = (~a_t) ((uint64_t)~a - ~a)" return-var c-return-type
				 arg1 tmp))))
	    (|mpq| (let ((tmp1 (gentemp "tmp"))
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
	 ((|int8| |int16| |int32| |int64|); |__int128|)
	  (case arg2-c-type ;can only reach here if arg1 is non-negative
	    ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	     (list (format nil "~a = (~a_t)((int64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	    ((|int8| |int16| |int32| |int64|); |__int128|)
	     (list (format nil "~a = (~a_t)((int64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	    (|mpz| (let ((tmp (gentemp "tmp")))
		   (list (format nil "~a_t ~a" arg1-c-type tmp)
			 (format nil "~a = mpz_get_si(~a)" tmp arg2)
			 (format nil "~a = (~a_t)((int64_t) ~a - ~a)" return-var c-return-type arg1 tmp))))
	    (|mpq| (let ((tmp1 (gentemp "tmp"))
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
	 (|mpz| (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_sub_ui(~a, ~a, ~a)" tmp arg1 arg2)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" return-var c-return-type tmp)
			 (format nil "mpz_clear(~a)" tmp)))
		 )
		((|int8| |int16| |int32| |int64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "if (~a < 0){mpz_add_ui(~a, ~a, ~a);} else {mpz_sub_ui(~a, ~a, ~a);}"
				 arg2 tmp arg1 arg2 tmp arg1 arg2)
			 (format nil "~a = (~a_t) mpz_get_ui(~a)" return-var c-return-type tmp)
			 (format nil "mpz_clear(~a)" tmp)))
		 )
		(|mpz| (let ((tmp (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_sub(~a, ~a, ~a)" tmp arg1 arg2)
			     (format nil "~a = (~a_t) mpz_get_ui(~a)" return-var c-return-type tmp)
			     (format nil "mpz_clear(~a)" tmp))))
		(|mpq|  (let ((tmp (gentemp "tmp"))
			    (tmp2 (gentemp "tmp")))
		       (list (format nil "mpz_t ~a" tmp)
			     (format nil "mpz_init(~a)" tmp)
			     (format nil "mpz_t ~a" tmp2)
			     (format nil "mpz_init(~a)" tmp2)
			     (format nil "mpz_set_q(~a, ~a)" tmp2 arg2)
			     (format nil "mpz_sub(~a, ~a, ~a)" tmp arg1 tmp2)
			     (format nil "~a = (~a_t) mpz_get_ui(~a)" return-var c-return-type tmp)
			     (format nil "mpz_clear(~a)" tmp)
			     (format nil "mpz_clear(~a)" tmp2)))
		      )))
	 (|mpq| (break "Not available"))))
    ((|int8| |int16| |int32| |int64|); |__int128|)
     (case arg1-c-type
       ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	   (list (format nil "~a = (~a_t)((uint64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	  ((|int8| |int16| |int32| |int64|); |__int128|)
	   (list (format nil "~a = (~a_t)((uint64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (|mpz| )
	  (|mpq| (break "Not available"))))
       ((|int8| |int16| |int32| |int64| |__int128|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64|); |__uint128|)
	   (list (format nil "~a = (~a_t)((int64_t)~a - (uint64_t)~a)" return-var c-return-type arg1 arg2)))
	  ((|int8| |int16| |int32| |int64|); |__int128|)
	   (list (format nil "~a = (~a_t)((int64_t)~a - (int64_t)~a)" return-var c-return-type arg1 arg2)))
	  (|mpz| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_set_si(~a, ~a)" tmp arg1)
		       (format nil "mpz_sub(~a, ~a, ~a)" tmp tmp arg2)
		       (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	  (|mpq| (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpq_t ~a" tmp)
		       (format nil "mpq_init(~a)" tmp)
		       (format nil "mpq_set_si(~a, ~a)" tmp arg1)
		       (format nil "mpq_sub(~a, ~a, ~a)" tmp tmp arg2)
		       (format nil "~a = (~a_t)mpq_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpq_clear(~a)" tmp))))))
       (|mpz| (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64|) ; |__uint128|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_sub_ui(~a, ~a, ~a)" tmp arg1 arg2)
		       (format nil "~a = (~a_t) mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      ((|int8| |int16| |int32| |int64|) ; |__int128|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_mk_si(~a, ~a, ~a)" tmp arg1 arg2)
		       (format nil "~a = (~a_t) mpz_get_si(~a)" return-var c-return-type tmp)
		       (format nil "mpz_clear(~a)" tmp))))
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_sub(~a, ~a, ~a)" tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
			   (format nil "mpz_clear(~a)" tmp))))
	      (|mpq| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_sub_q(~a, ~a, ~a)" tmp arg1 arg2)
			   (format nil "~a = (~a_t)mpz_get_si(~a)" return-var c-return-type tmp)
			   (format nil "mpq_clear(~a)" tmp))))))
       (|mpq| (break "Not available"))))))

    
  ;; (case arg1-c-type
  ;;   (|uint32| (case arg2-c-type
  ;; 		  (|uint32| (case c-return-type
  ;; 			      (|uint32|
  ;; 			       (list (format nil "~a = ~a - ~a" return-var arg1 arg2)))
  ;; 			      (|int32| ;;checked that this (below) works in C
  ;; 			       (format nil "~a = (int32_t) (~a - ~a)" return-var arg1 arg2))
  ;; 			      (|mpz| (list (format nil "mpz_set_ui(~a, ~a)"
  ;; 						   return-var arg1)
  ;; 					   (format nil "mpz_sub_ui(~a, ~a, ~a)" return-var return-var arg2)))
  ;; 			      (|mpq| (break "deal with this later"))))
  ;; 		  (|int32| (case c-return-type
  ;; 			   (|uint32| 
  ;; 			    (list (format nil "if (~a < 0){~a =  (~a + (~a_t)(-~a));} else {~a =  (~a - (~a_t)~a);}"
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type
  ;; 					     arg2
  ;; 					     return-var arg1 c-return-type arg2)))
  ;; 			   (|int32| (list (format nil "if (~a < 0){if ((uint32_t)(-~a) <= ~a){~a = (int32_t)(~a + (uint32_t)(-~a));} else {~a = (int32_t)~a - ~a;};} else {~a = (int32_t)(~a - (uint32_t)~a);}"
  ;; 					     arg2 arg2 arg1 return-var arg1 arg2
  ;; 					     return-var arg1 arg2
  ;; 					     return-var arg1 arg2)))
  ;; 			   (|mpz| (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "if (~a < 0){mpz_add_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_sub_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 		  (|mpz| (let ((tmp (gentemp "tmp")))
  ;; 			 (case c-return-type
  ;; 			   ((|uint32| |int32|)
  ;; 			    (ir2c-subtraction-step  return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type))
  ;; 			   (|mpz| (list (format nil "mpz_set_ui(~a, ~a)" return-var arg1)
  ;; 				      (format nil "mpz_sub(~a, ~a, ~a)" return-var return-var arg2))))))))
  ;;     (|int32| (case arg2-c-type
  ;; 	       (|int32| (case c-return-type
  ;; 			(|uint32| (list (format nil "~a = (~a_t) (~a - ~a)"
  ;; 					      return-var c-return-type arg1 arg2)))
  ;; 			(|int32| (list (format nil "~a = ~a - ~a"
  ;; 					     return-var arg1 arg2)))
  ;; 			(|mpz| (list (format nil "mpz_set_si(~a, ~a)" return-var arg1)
  ;; 				   (format nil "if (~a < 0){mpz_add_ui(~a, ~a, (uint32_t)(-~a));} else {mpz_sub_ui(~a, ~a (uint32_t)(~a));}"
  ;; 					      arg2
  ;; 					      return-var return-var arg2
  ;; 					      return-var return-var arg2)))))
  ;; 	       (t (ir2c-subtraction-step return-var c-return-type arg2 arg2-c-type
  ;; 				      arg1 arg1-c-type ))))
  ;;     (|mpz| (case c-return-type
  ;; 	     ((|uint32| |int32|)
  ;; 	      (case arg2-c-type
  ;; 		(|uint32| (ir2c-subtraction-step return-var c-return-type arg2 arg2-c-type
  ;; 					      arg1 arg1-c-type))
  ;; 		(|int32| (let ((tmp (gentemp "tmp")))
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
  ;; 		(|mpz| (let ((tmp (gentemp "tmp")))
  ;; 		       (list (format nil "mpz_t ~a" tmp)
  ;; 			     (format nil "mpz_init(~a)" tmp)
  ;; 			     (format nil "mpz_sub(~a, ~a, ~a)"
  ;; 				     tmp arg1 arg2)
  ;; 			     (format nil "mpz_get~a(~a, ~a)"
  ;; 				     (gmp-suffix c-return-type)
  ;; 				     return-var tmp)
  ;; 			     (format nil "mpz_clear(~a)" tmp))))))
  ;; 	     (|mpz| (list (format nil "mpz_set~a(~a, ~a)"
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
  (or (and (eq ctype '|mpq|) '|q|)
      (gmp-ui-or-si ctype)))

(defun mpq-set (ctype qvar var)
  (if (eq ctype '|mpz|)
      (format nil "mpq_set_z(~a, ~a)" qvar var)
    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
	    (gmp-ui-or-si ctype) qvar
	    (uint-or-int ctype)
	     var)))

(defun ir2c-multiplication-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (|mpz| (case arg1-c-type
	     ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	      (case arg2-c-type
		((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
		 (let ((tmp (gentemp "tmp")))
		   (list (format nil "mpz_t ~a" tmp)
			 (format nil "mpz_init(~a)" tmp)
			 (format nil "mpz_set_~a(~a, (~a64_t)~a)" (gmp-ui-or-si arg1-c-type)
				 tmp (uint-or-int arg1-c-type) arg1)
			 (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)" (gmp-ui-or-si arg2-c-type)
				 return-var tmp (uint-or-int arg2-c-type) arg2)
			 (format nil "mpz_clear(~a)" tmp)
			 )))
		((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		(|mpz| (list (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)"
			       (gmp-ui-or-si arg1-c-type)
			       return-var arg2 (uint-or-int arg1-c-type) arg1)))
		(|mpq| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpz_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpz_set_q(~a, ~a)" 
				 tmp arg2)
			       (format nil "mpz_mk_mul_~a(~a, ~a, ~a)"
				 (gmp-ui-or-si arg1-c-type)
				 return-var tmp arg1)
			       (format nil "mpq_clear(~a)" tmp)
			       )))))
	     (|mpz| (case arg2-c-type
		      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
		       (list (format nil "mpz_mk_mul_~a(~a, ~a, (~a64_t)~a)" (gmp-ui-or-si arg2-c-type)
				     return-var arg1 (uint-or-int arg2-c-type) arg2)))
		      ((|__uint128| |__int128|) (break "128-bit GMP arithmetic not available." ))
		      (|mpz| (list (format nil "mpz_mk_mul(~a, ~a, ~a)" return-var arg2 arg1)))
		      (|mpq| (let ((tmp (gentemp "tmp")))
			       (list (format nil "mpz_t ~a" tmp)
				     (format nil "mpz_init(~a)" tmp)
				     (format nil "mpz_set_q(~a, ~a)" 
				       tmp arg2)
				     (format nil "mpz_mk_mul(~a, ~a, ~a)" return-var arg1 tmp)
				     (format nil "mpz_clear(~a)" tmp)
			       
				     )))))
	     (t (break "Not implemented"))))
    ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
     (case arg1-c-type
       ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	(case arg2-c-type
	  ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|); |__uint128| |_int128|)
	   (list (format nil "~a = (~a_t)((~a64_t)~a * (~a64_t)~a)" return-var c-return-type (uint-or-int arg1-c-type) arg1 (uint-or-int arg2-c-type) arg2)))
	  (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg1-c-type)
				   tmp  arg2 arg1)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   
			   )))
	  (|mpq| (let ((tmp1 (gentemp "tmp"))
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
       ((|__uint128| |__int128|)(break "128-bit GMP arithmetic not available"))
	;; (case arg2-c-type
	;;   ((|uint8| |uint16| |uint32| |uint64| |__uint128| |int8| |int16| |int32| |int64| |_int128|)
	;;    (list (format nil "~a = (~a_t)(~a * ~a)" return-var c-return-type arg1 arg2)))
	;;   ((|mpz| |mpq|) (break "128-bit GMP arithmetic not available")))
       (|mpz| (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	       (let ((tmp (gentemp "tmp")))
		 (list (format nil "mpz_t ~a" tmp)
		       (format nil "mpz_init(~a)" tmp)
		       (format nil "mpz_mul_~a(~a, ~a, ~a)" (gmp-ui-or-si arg2-c-type)
			       tmp  arg1 arg2)
		       (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
			       c-return-type (gmp-ui-or-si c-return-type) tmp)
		       (format nil "mpz_clear(~a)" tmp)
		       )))
	      (|mpz| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpz_t ~a" tmp)
			   (format nil "mpz_init(~a)" tmp)
			   (format nil "mpz_mul(~a, ~a, ~a)" 
				   tmp  arg2 arg1)
			   (format nil "~a = (~a_t)mpz_get_~a(~a)" return-var
				   c-return-type (gmp-ui-or-si c-return-type) tmp)
			   (format nil "mpz_clear(~a)" tmp)
			   
			   )))
	      (|mpq| (let ((tmp1 (gentemp "tmp"))
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
       (|mpq| (case arg2-c-type
	      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
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
	      (|mpz| (let ((tmp1 (gentemp "tmp"))
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
	      (|mpq| (let ((tmp (gentemp "tmp")))
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
    ((|__uint128| |__int128|)(break "128-bit GMP arithmetic not available"))
     ;; (case arg1-c-type
     ;;   ((|uint8| |uint16| |uint32| |uint64| |__uint128| |int8| |int16| |int32| |int64| |__int128|)
     ;; 	(case arg2-c-type
     ;; 	  ((|uint8| |uint16| |uint32| |uint64| |__uint128| |int8| |int16| |int32| |int64| |_int128|)
     ;; 	   (list (format nil "~a = (~a_t)(~a * ~a)" return-var c-return-type arg1 arg2)))
     ;; 	  ((|mpz| |mpq|) (break "128-bit GMP arithmetic not available"))))
     ;;   ((|mpz| |mpq|) (break "128-bit GMP arithmetic not available"))
     ;;   (t (break "Not implemented.")))
    (|mpq| (case arg1-c-type
	   (|mpq| (case arg2-c-type
		  (|mpq| (list (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var arg1 arg2)))
		  (|mpz| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_set_z(~a, ~a)" tmp arg2)
			       (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var arg1 tmp)
			       (format nil "mpq_clear(~a)" tmp))))
		  ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
		   (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (mpq-set arg2-c-type tmp arg2)
			   (format nil "mpq_mul(~a, ~a, ~a)" return-var arg1 tmp)
			   (format nil "mpq_clear(~a)" tmp)
			   )))
		  ((|uint128| |int128|)
		   (break "128-bit GMP arithmetic not available." ))))
	   (|mpz| (case arg2-c-type
		  (|mpq| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_set_z(~a, ~a)" tmp arg1)
			       (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var tmp arg2)
			       (format nil "mpq_clear(~a)" tmp))))
		  (|mpz| (let ((tmp (gentemp "tmp")))
			 (list (format nil "mpq_t ~a" tmp)
			       (format nil "mpq_init(~a)" tmp)
			       (format nil "mpq_mul(~a, ~a, ~a)" tmp arg1 arg2)
			       (format nil "mpq_mk_set_z(~a, ~a)" return-var tmp))))
		  ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
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
		  ((|uint128| |int128|)
		   (break "128-bit GMP arithmetic not available." ))))
	   ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
	    (case arg2-c-type
	      (|mpq| (let ((tmp (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" tmp)
			   (format nil "mpq_init(~a)" tmp)
			   (mpq-set arg1-c-type tmp arg1)
			   (format nil "mpq_mk_mul(~a, ~a, ~a)" return-var tmp arg2)
			   (format nil "mpq_clear(~a)" tmp)
			   )))
	      ((|uint8| |uint16| |uint32| |uint64| |int8| |int16| |int32| |int64|)
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
  (or (eq c-type '|mpz|)(eq c-type '|mpq|)))

(defmethod ir2c* ((ir-expr ir-lookup) return-var return-type)
  (with-slots (ir-array ir-index) ir-expr
	      (let* ((ir-array-var (get-ir-last-var ir-array))
		     (ir-index-var (get-ir-last-var ir-index))
		     (c-return-type (add-c-type-definition (ir2c-type return-type)))
		     (elem-type (ir-range (get-ir-type-value (ir-vtype ir-array-var))))
		     (c-elem-type  (add-c-type-definition (ir2c-type return-type)))
		     (rhs (format nil "~a->elems[~a]" (ir-name ir-array-var)(ir-name ir-index-var)))
		     (assign-instrs (case c-return-type 
				      ((|mpq| |mpz|)  ;;could've used mk-c-assignment here
				       (list (format nil "~a = safe_malloc(sizeof(~a_t))"
						     return-var c-return-type)
					     (format nil "~a_init(~a)" c-return-type return-var)
					     (make-c-assignment return-var c-return-type
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
  (let* ((ir2c-type (ir2c-type (ir-vtype ir-array-var)))
	 (ctype (add-c-type-definition ir2c-type))
	 (theory-params *ir-theory-formals*)
	 (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								  theory-params
								  collect (ir-formal-id ir-formal)))))
    (make-release-call ir2c-type ctype (ir-name ir-array-var)  c-param-arg-string)))

(defmethod ir2c* ((ir-expr ir-apply) return-var return-type)
  (with-slots (ir-func ir-params ir-args) ir-expr
	      (let* ((ir-func-var (get-ir-last-var ir-func))
		     (hi-array (when (ir-variable? ir-func-var)
				 (ir-array? (ir-vtype ir-func-var))))
		     (ir-index-var (when hi-array (get-ir-last-var (car ir-args)))))
	      (if hi-array;;if operator is an array
		  (let* ((assign-instr (mk-c-assignment 
					       return-var
					       (add-c-type-definition (ir2c-type return-type))
					       (format nil "~a->elems[~a]"
						       (ir-name ir-func-var)(ir-name ir-index-var))
					       (add-c-type-definition (get-range (ir-vtype ir-func-var)))))
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
		     ;; (mp-release-instrs (loop for ir-var in ir-params-args
		     ;; 			      when (and (ir-last? ir-var)
		     ;; 					;(not (member (get-ir-last-var ir-var) *mpvar-parameters*))
		     ;; 					(mpnumber-type? (ir2c-type (ir-vtype (get-ir-last-var ir-var)))))
		     ;; 			      collect (format nil "~a_clear(~a)"
		     ;; 					      (ir2c-type (ir-vtype (get-ir-last-var ir-var)))
		     ;; 					      (ir-name (get-ir-last-var ir-var)))))
		     )
		  ;(format t "~%*mpvar-parameters* = ~{~a, ~}" (print-ir *mpvar-parameters*))
		;(break "ir-apply")
		  (nconc release-instrs invoke-instrs) ;nconc invoke-instrs mp-release-instrs
					       )))))

(defun mppointer-type (type)
  (let ((mpnumber-type (mpnumber-type? type)))
    (or (and mpnumber-type (intern (format nil "~a_ptr" type)))
	type)))

(defmethod ir2c* ((ir-expr ir-let) return-var return-type)
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr
    (with-slots (ir-name ir-vtype ir-pvsid) ir-vartype
;;      (when (ir-lvariable? ir-body)(break "variable body"))
      (cond ((and (ir-lvariable? ir-body)
		  (equal return-type (ir2c-type ir-vtype)))
	     (ir2c* ir-bind-expr return-var return-type))
	    (t (let* ((ir2c-vtype (ir2c-type ir-vtype))
		      (var-ctype (add-c-type-definition ir2c-vtype))
		      (decl-var-ctype (mppointer-type var-ctype))
		      (decl-instr (if ir-pvsid
				      (format nil "/* ~a */ ~a_t ~a" ir-pvsid decl-var-ctype ir-name)
				    (format nil "~a_t ~a" decl-var-ctype ir-name)))
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
		      (finalize-instrs (when (ir-type-actual? ir-bind-expr)
					 (list (format nil "safe_free(~a)" ir-name))))
		      )
		 (if init-instrs
		     (cons decl-instr (append init-instrs
			     		      (append bind-instrs body-instrs)))
		   (cons decl-instr (append bind-instrs body-instrs finalize-instrs)))))))))

(defmethod ir2c* ((ir-expr ir-lett) return-var return-type);;assume ir-bind-expr is an ir-variable
  (with-slots (ir-vartype ir-bind-type ir-bind-expr ir-body) ir-expr
	      (with-slots (ir-name ir-vtype ir-pvsid) ir-vartype
			  (let* ((ir2c-vtype (ir2c-type ir-vtype))
				 (ir2c-btype (ir2c-type ir-bind-type))
				 (var-ctype (add-c-type-definition ir2c-vtype))
				 (decl-var-ctype (mppointer-type var-ctype))
				 (var-btype (add-c-type-definition ir2c-btype))
				 (decl-instr (if ir-pvsid
						 (format nil "/* ~a */ ~a_t ~a" ir-pvsid decl-var-ctype ir-name)
						 (format nil "~a_t ~a" decl-var-ctype ir-name)));;need mpz_init/release for mpz_t
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
				 (body-instrs (ir2c* ir-body return-var return-type))
				 (finalize-instrs (when (ir-type-actual? ir-bind-expr)
					 (list (format nil "safe_free(~a)" ir-name)))));(break "ir-let")
			    (if (ir-last? ir-bind-expr)
				     ;(not (member (get-ir-last-var ir-bind-expr) *mpvar-parameters*))
				(let ((clear-instrs
				       (if (mpnumber-type? var-btype)
					   (list
					    (format nil "~a_clear(~a)" var-btype
						    (ir-name (ir-var ir-bind-expr)))
					    )
					 (if (ir-reference-type? ir2c-btype)
					     (list (release-last-var (ir-var ir-bind-expr)))
					   nil))))
				  (append all-bind-instrs clear-instrs body-instrs finalize-instrs))
			      (append all-bind-instrs body-instrs finalize-instrs))))))
				

(defmethod ir2c* ((ir-expr ir-nil) return-var return-type)
  (declare (ignore return-type))
  (list (format nil "~a = NULL" return-var)))

(defmethod ir2c-type ((ir-typ ir-subrange))
    (with-slots (ir-low ir-high) ir-typ 
		(if (and (integerp ir-low) (>= ir-low 0))
		    (if (integerp ir-high)
			(cond ((<= ir-high *max8-C-uli*) '|uint8|)
			      ((<= ir-high *max16-C-uli*) '|uint16|)
			      ((<= ir-high *max32-C-uli*) '|uint32|)
			      ((<= ir-high *max64-C-uli*) '|uint64|)
			      ;((<= ir-high *max128-C-uli*) '|__uint128|)
			      (t '|mpz|))
		      '|mpz|)
		  (if (and (integerp ir-low)(integerp ir-high))
		      (cond ((and (>= ir-low *min8-C-int*)(<= ir-high *max8-C-int*)) '|int8|)
			    ((and (>= ir-low *min16-C-int*)(<= ir-high *max16-C-int*)) '|int16|)
			    ((and (>= ir-low *min32-C-int*)(<= ir-high *max32-C-int*)) '|int32|)
			    ((and (>= ir-low *min64-C-int*)(<= ir-high *max64-C-int*)) '|int64|)
			    ;((and (>= ir-low *min128-C-int*)(<= ir-high *max128-C-int*))
			    ; '|__int128|)
			    (t '|mpz|))
		    '|mpz|))))

;; (defun ir-integer-type? (ir-typ)
;;   (and (ir-subrange? ir-typ)
;;        (with-slots (ir-low ir-high) ir-typ
;; 		   (or (and (>= ir-low *min-C-uli*)
;; 			    (<= ir-high *max-C-uli*))
;; 		       (and (>= ir-low *min-C-int*)
;; 			    (<= ir-high *max-C-int*))))))

(defun mk-ir-arraytype (size high domain range)
  (make-instance 'ir-arraytype
		 :size size
		 :high high
		 :ir-domain domain
		 :ir-range range))

;;Not used anywhere
;; (defcl ir-hashtype (ir-type)
;;   hdomain)


(defmethod print-ir ((ir-type ir-arraytype))
  (with-slots (size high ir-range) ir-type
	      (format nil "~a[~d/~a]" (print-ir ir-range) size (print-ir high))))

(defmethod ir2c-type ((ir-typ ir-funtype))
  (with-slots (ir-domain ir-range) ir-typ
    (mk-ir-funtype  (ir2c-type ir-domain)
		    (ir2c-type ir-range))))

(defmethod ir2c-type ((ir-typ ir-fieldtype))
  (with-slots (ir-vtype) ir-typ
    (ir2c-type ir-vtype)))

(defmethod ir2c-type ((ir-typ ir-arraytype))
  (with-slots (size high ir-domain ir-range) ir-typ
    (mk-ir-arraytype size high (ir2c-type ir-domain)(ir2c-type ir-range))))
;;arraytypes are only created for arraytypes in PVS.    
	      ;; (let ((size (ir-index? ir-domain)))
	      ;; 	(if size ;;check that size is below max-PVS-array-size
	      ;; 	    (mk-ir-arraytype size (ir2c-type ir-range))
	      ;; 	  (mk-ir-funtype  (ir2c-type ir-domain)
	      ;; 			  (ir2c-type ir-range))))))

(defmethod ir2c-type ((ir-typ ir-recordtype))
  (with-slots (ir-field-types) ir-typ
	      (mk-ir-recordtype
	       (ir2c-type-fields ir-field-types)
	       (ir-label ir-typ))))

(defmethod ir2c-type ((ir-typ ir-tupletype))
  (with-slots (ir-field-types) ir-typ
	      (mk-ir-tupletype
	       (ir2c-type-fields ir-field-types)
	       (ir-label ir-typ))))

(defmethod ir2c-type ((ir-typ ir-adt-recordtype))
  (with-slots (ir-field-types ir-constructors) ir-typ
	      (mk-ir-adt-recordtype
	       (ir2c-type-fields ir-field-types)
	       ir-constructors)))

(defmethod ir2c-type ((ir-typ ir-adt-constructor-recordtype))
  (with-slots (constructor-id ir-field-types ir-adt-name) ir-typ
    (mk-ir-adt-constructor-recordtype
     constructor-id
     (ir2c-type-fields ir-field-types)
     ir-adt-name)))

(defun ir2c-type-fields (ir-field-types)
  (cond ((consp ir-field-types)
	 (with-slots (ir-id ir-vtype) (car ir-field-types)
	   (setf (ir-vtype (car ir-field-types))
		       (ir2c-type ir-vtype))
	   (cons (car ir-field-types)
		 (ir2c-type-fields (cdr ir-field-types)))))
	(t nil)))

(defmethod ir2c-type ((ir-type symbol))
  (case ir-type
    (boolean 'bool)
    (t ir-type)))

(defmethod ir2c-type ((ir-type ir-typename))
  ;(break "ir2c-type")
  (with-slots (ir-type-id ir-type-defn type-declaration ir-actuals) ir-type
    (if (ir-subrange? ir-type-defn)
	(ir2c-type ir-type-defn)
      (let ((new-ir-type (copy ir-type)))
	(setf (ir-type-defn new-ir-type) (ir2c-type ir-type-defn)
	      (type-declaration new-ir-type) type-declaration)
	new-ir-type))))

(defmethod ir2c-type ((ir-type ir-const-formal)) ;(break "ir2c-type")
  (with-slots (ir-name ir-vtype) ir-type
    (mk-ir-typename ir-name  ir-vtype nil nil nil)))

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

	      ;; (let ((array? (ir-array? ir-domain))
	      ;; 	    ) ;; ignoring non-arrays for now
	      ;; 	(if (ir-integer-type? ir-range)
	      ;; 	    (format nil "ref_int_t")
	      ;; 	  (format nil "ref_ref_t")))))


(defun array-size-expr (ir-arraytype)
  (with-slots (size high) ir-arraytype
    (or (and (ir-offset? high) high)
	size)))

(defmethod ir2c* ((ir-expr ir-offset) returnvar returntype)
  (with-slots (expr offset) ir-expr
    (let* ((size-offset (1+ offset))
	   (offset-instr
	    (format nil "~a += ~a" returnvar size-offset)))
    (append (ir2c* expr returnvar returntype)
	    (list offset-instr)))))

(defmethod ir2c* ((ir-expr ir-forall) return-var return-type)
  (with-slots (ir-vartype ir-body) ir-expr
    (let* ((index (ir-name  ir-vartype))
	   (ir-vtype (ir-vtype ir-vartype))
	   (index-c-type (add-c-type-definition (ir2c-type ir-vtype)))
	   (lowvar (gentemp "low"))
	   (highvar (gentemp "highvar"))
	   (low-instrs (if (ir-low-expr ir-vtype)
			   (ir2c* (ir-low-expr ir-vtype) lowvar ir-vtype)
			 (list (format nil "~a = (~a_t) ~a" lowvar index-c-type (ir-low ir-vtype)))))
	   (high-instrs (if (ir-high-expr ir-vtype)
			    (ir2c* (ir-high-expr ir-vtype) highvar ir-vtype)
			  			 (list (format nil "~a = (~a_t) ~a" lowvar index-c-type (ir-high ir-vtype)))))
	   (prelude-instrs (list (format nil "~a_t ~a" index-c-type lowvar)
				 (format nil "~a_t ~a" index-c-type highvar)
				 (format nil "~a = true" return-var)))
	   (c-body (ir2c* ir-body return-var return-type))
	   (return-instr (format nil "if (!~a) break" return-var))
	   (loop-body (append c-body (list return-instr)))
	   (for-instr (mk-for-instr (format nil "uint32_t ~a = ~a; ~a < ~a; ~a++"
						index lowvar index highvar index)
					loop-body)))
      (append prelude-instrs low-instrs high-instrs (list for-instr)))))

(defmethod ir2c* ((ir-expr ir-exists) return-var return-type)
  (with-slots (ir-vartype ir-body) ir-expr
    (let* ((index (ir-name  ir-vartype))
	   (ir-vtype (ir-vtype ir-vartype))
	   (index-c-type (add-c-type-definition (ir2c-type ir-vtype)))
	   (lowvar (gentemp "low"))
	   (highvar (gentemp "highvar"))
	   (low-instrs (if (ir-low-expr ir-vtype)
			   (ir2c* (ir-low-expr ir-vtype) lowvar ir-vtype)
			 (list (format nil "~a = (~a_t) ~a" lowvar index-c-type (ir-low ir-vtype)))))
	   (high-instrs (if (ir-high-expr ir-vtype)
			    (ir2c* (ir-high-expr ir-vtype) highvar ir-vtype)
			  			 (list (format nil "~a = (~a_t) ~a" lowvar index-c-type (ir-high ir-vtype)))))
	   (prelude-instrs (list (format nil "~a_t ~a" index-c-type lowvar)
				 (format nil "~a_t ~a" index-c-type highvar)
				 (format nil "~a = false" return-var)))
	   (c-body (ir2c* ir-body return-var return-type))
	   (return-instr (format nil "if (~a) break" return-var))
	   (loop-body (append c-body (list return-instr)))
	   (for-instr (mk-for-instr (format nil "uint32_t ~a = ~a; ~a < ~a; ~a++"
						index lowvar index highvar index)
					loop-body)))
      (append prelude-instrs low-instrs high-instrs (list for-instr)))))


;;A lambda-expression turns into the initialization for an array.  
(defmethod ir2c* ((ir-expr ir-lambda) return-var return-type)
  (with-slots (ir-vartypes ir-lastvars ir-rangetype ir-body) ir-expr
    ;(break "ir2c*(ir-lambda)")
    (let* ((ir-arraytype  (get-arraytype return-type)) 
	   (array? (and (ir-arraytype? ir-arraytype)
			(array-size-expr ir-arraytype))))
      ;; (and (eql (length ir-vartypes) 1)
      ;; 		       (ir-index? (ir-vtype (car ir-vartypes))))
      (if array?;ignore this and retain lambda exprs
	  (let* ((elemtype (with-slots (ir-range) ir-arraytype ir-range))
		 (ir2c-arraytype (ir2c-type ir-arraytype))
		 (c-arraytype (add-c-type-definition ir2c-arraytype))
		 (index (ir-name (car ir-vartypes)))
		 (index-c-type (add-c-type-definition (ir2c-type (ir-vtype (car ir-vartypes)))))
		 (for-index (gentemp "index"))
		 (return-location (format nil "~a->elems[~a]" return-var index)) ;(NSH:4/3/24):replaced for-index w/index
		 (return-location-type (ir2c-type elemtype))
		 (c-body (ir2c* ir-body return-location return-location-type))
		 (mp-prelude (when (mpnumber-type? index-c-type)
			       (list (format nil "mpz_t ~a" index)
				     (format nil "mpz_init(~a)" index))))
		 (c-body-with-index (append mp-prelude ;;index is initialized within the for loop
					    (cons (make-c-assignment index index-c-type for-index '|uint32|)
						  c-body)))
		 ;; (mp-final (when (mpnumber-type? index-c-type)
		 ;; 	     (list (format nil "mpz_clear(~a)" index))))
		 (release-instrs (release-instrs ir-lastvars)) ;;NSH(1/21/20)
		 (size (if (integerp array?) (1+ array?) (gentemp "size")))
		 (size-instrs (if (integerp array?) nil
				  (with-slots (expr offset) array?
				    (let* ((size-offset (1+ offset))
					   (offset-instr
					    (format nil "~a += ~a" size size-offset)))
				      (append (list (format nil "uint32_t ~a" size));;link to *max-PVS-array-size*
					      (ir2c* expr size '|uint32|)
					      (list offset-instr))))))
		 (for-instr (mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
						for-index for-index size for-index)
					c-body-with-index)))
	    ;(break "ir2c*(ir-lambda)")
	    (append mp-prelude
		    size-instrs
		    (cons (format nil "~a = new_~a(~a)" return-var c-arraytype size)
					;(format nil "~a->count = 1");;this should be done within new.
			  (if mp-prelude
			      (list for-instr)
			    (list (format nil "~a_t ~a" index-c-type index)
				    for-instr)))
;		    mp-final
		    release-instrs))
	  (let* ((fvars (pvs2ir-freevars* ir-expr))
		 ;;(lastvars (ir-lastvars ir-expr))
		 (c-return-type (add-c-type-definition return-type))
		 (closure-name (add-closure-definition ir-expr c-return-type))
		 (closure-var (gentemp "cl"))
		 (new-instrs (list (format nil "~a_t ~a" closure-name closure-var)
				   (format nil "~a = new_~a()" closure-var closure-name)))
		 (fvar-instrs (loop for fvar in fvars
				 as i from 1
				 nconc (let ((ir-fv-type (ir2c-type (freevar-type fvar))))
					 (make-c-assignment-with-count
					  (format nil "~a->fvar_~a"  closure-var i)
					  ir-fv-type
					  (freevar-name fvar)
					  (add-c-type-definition ir-fv-type)))))
		 (release-instrs (release-instrs ir-lastvars))
		 (return-instrs (list (format nil "~a = (~a_t)~a"
					return-var
					c-return-type closure-var)))
		 ;;refcount isn't needed
		 ;; (refcount-instrs (loop for fvar in fvars
		 ;; 			      when (and (ir-reference-type? (ir-vtype fvar))
		 ;; 					(not (memq fvar lastvars)))
		 ;; 			      collect (format nil "~a->count++" (ir-name fvar))))
		 )			;(break "ir2c*(ir-lambda)")
	    (nconc new-instrs (nconc fvar-instrs (nconc release-instrs return-instrs))))))))
							  
		;; (break "closure alert"))))); (pvs2c-err "closures not yet implemented")

(defmethod get-arraytype ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
    (get-arraytype ir-type-defn)))

(defmethod get-arraytype ((ir-type ir-arraytype))
  ir-type)

(defmethod get-arraytype ((ir-type t)) nil)

(defmethod ir2c* ((ir-expr ir-ift) return-var return-type);(break "ir2c*(ift)")
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (let ((c-then-instrs (ir2c* ir-then return-var return-type))
		    (c-else-instrs (ir2c* ir-else return-var return-type)))
		(list (mk-if-instr 
		       (ir-name (get-ir-last-var ir-condition))
		       c-then-instrs
		       c-else-instrs)))))

(defun release-instrs (ir-varlist)
  (cond ((consp ir-varlist)
	 (let* ((ir-vtype (ir2c-type (ir-vtype (car ir-varlist))))
		(ctype (add-c-type-definition ir-vtype))
		(release-cdr (release-instrs (cdr ir-varlist))))
	   (declare (ignore ctype))
	   (if (ir-reference-type? ir-vtype)
	       (cons (release-last-var (car ir-varlist)) release-cdr)
	     release-cdr)))
	(t nil)))

(defmethod ir2c* ((ir-expr ir-release) return-var return-type)
  (with-slots (pre-ir-vars post-ir-vars ir-body) ir-expr
	      (let ((c-body (ir2c* ir-body return-var return-type))
		    (pre-release-instrs (release-instrs pre-ir-vars)
					)
		    (post-release-instrs (release-instrs post-ir-vars)))
		(append pre-release-instrs c-body post-release-instrs))))

(defmethod ir2c* ((ir-expr ir-new) return-var return-type)
  (declare (ignore return-type))
  (with-slots (ir-etype) ir-expr
	      (ir2c-new ir-etype return-var)))

(defmethod ir2c-new ((ir-type ir-arraytype) return-var)
  (let ((ctype (add-c-type-definition (ir2c-type ir-type))))
    (format nil "~a = new_~a()" return-var ctype)))

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
		     (theory-params *ir-theory-formals*)
		     (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								      theory-params
								      collect (ir-formal-id ir-formal))))		     
		     (rhs-last (and (not (ir-constructor-update? ir-expr))
				    (ir-last? ir-rhs)
				    (ir-reference-type? (ir-vtype rhs-var))))
		     (rhs-last-instr (if rhs-last
					(list (format nil "if (~a != NULL) ~a->count--"
						      rhs-var-name rhs-var-name))
				       nil))) ;(break "ir2c*(ir-update)")
		;;(when (ir-constructor-update? ir-expr)(break "ir2c*(ir-constructor-update)"))
		(if (ir-lvariable? ir-lhs)  ;;this doesn't work with names
		                            ;;(ir-arraytype? (get-ir-type-value ir-ctype))
		    (let* ((lhs-var (get-ir-last-var ir-lhs))
			   (lhs-var-name (ir-name lhs-var)))
		      (if target-last
			  (cons (format nil "~a = (~a_t)update_~a(~a, ~a, ~a~a)"
					return-var creturn-type ctype target-var-name lhs-var-name rhs-var-name c-param-arg-string)
				rhs-last-instr)
			(cons (format nil "{~a = (~a_t)copy_~a(~a); update_~a(~a, ~a, ~a)}"
				      return-var creturn-type ctype target-var-name ctype
				       return-var  lhs-var-name rhs-var-name)
			      rhs-last-instr)))
		  ;;else we're assuming it's a record
		  (if target-last
		      (cons (format nil "~a = (~a_t)update_~a_~a(~a, ~a~a)"
					return-var creturn-type ctype ir-lhs target-var-name rhs-var-name c-param-arg-string)
			    rhs-last-instr)
		    (cons (format nil "{~a = (~a_t)copy_~a(~a); ~a = (~a_t)update_~a_~a(~a, ~a);}"
				  return-var creturn-type ctype target-var-name
				  return-var creturn-type ctype ir-lhs return-var rhs-var-name)
			  rhs-last-instr))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;process a type into its C definition relative to *c-type-info-table*


(defun mk-c-defn-info (op-name op-header op-defn &optional op-arg-types op-return-type)
  (make-instance 'c-defn-info
    :op-name op-name
    :op-arg-types op-arg-types
    :op-return-type op-return-type
    :op-header (format nil "extern ~a" op-header)
    :op-defn op-defn))

(defun mk-c-noextern-defn-info (op-name op-header op-defn &optional op-arg-types op-return-type)
  (make-instance 'c-defn-info
    :op-name op-name
    :op-arg-types op-arg-types
    :op-return-type op-return-type
    :op-header op-header		;(format nil "extern ~a" op-header)
    :op-defn op-defn))




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
  (loop for entry in *c-type-info-table*
	when (and (or (c-type-info? entry)
		      (closure-c-type-info? entry))
		  (ir2c-tequal (ir-texpr entry) ir-texpr))
	return entry))
;;  (find ir-texpr *c-type-info-table* :key #'type-info-texpr :test #'ir2c-tequal))

(defun get-c-type-info-by-name (tname)
  (find tname *c-type-info-table* :key #'tname))

(defun get-c-closure-info (ir-lambda-expr)
  (find ir-lambda-expr *c-type-info-table* :key #'closure-info-lambda-expr :test #'equalp))

(defun mk-simple-c-type-info (ir-texpr tname tdefn act-defn comment-string)
  (when (ir-type? ir-texpr)
    (setf (unique-name ir-texpr) tname))
  (make-instance 'simple-c-type-info
		   :ir-texpr ir-texpr
		   :tname tname
		   :tdefn tdefn
		   :act-defn act-defn
		   :comment-string comment-string))

(defun mk-closure-c-type-info (ir-texpr tname tdecl tdefn ftable-type-defn release-info copy-info
					hash-entry-type-defn hash-type-defn
					lookup-info dupdate-info update-info equal-info json-info
					comment-string)
    (when (ir-type? ir-texpr)(setf (unique-name ir-texpr) tname))
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
		   :update-info update-info
		   :equal-info equal-info
		   :json-info json-info
		   :comment-string comment-string))

(defun mk-c-type-info (ir-texpr tname tdefn act-defn new-info release-info release-ptr-info copy-info equal-info equal-ptr-info json-info json-ptr-info update-info comment-string &optional upgrade-info)
  (when (ir-type? ir-texpr) (setf (unique-name ir-texpr) tname))
  (make-instance 'c-type-info
		   :ir-texpr ir-texpr
		   :tname tname
		   :tdefn tdefn
		   :act-defn act-defn
		   :new-info new-info
		   :release-info release-info
		   :release-ptr-info release-ptr-info
		   :copy-info copy-info
		   :equal-info equal-info
		   :equal-ptr-info equal-ptr-info
		   :json-info json-info
		   :json-ptr-info json-ptr-info		   
		   :update-info update-info
		   :upgrade-info upgrade-info
		   :comment-string comment-string))


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

(defmethod freevar-type ((var ir-variable))
  (with-slots (ir-vtype) var
    ir-vtype))

(defmethod freevar-type ((var ir-formal-typename))
  *type-actual-ir-name*)

(defmethod freevar-name ((var ir-variable))
  (with-slots (ir-name) var
    ir-name))

(defmethod freevar-name ((var ir-formal-typename))
  (with-slots (ir-type-id) var
    ir-type-id))

(defun add-closure-definition (ir-lambda-expr &optional c-return-type)
  (let* ((closure-info (get-c-closure-info ir-lambda-expr))
	 (closure-fdefn (when closure-info (fdefn closure-info))))
    (or (and closure-fdefn (op-name closure-fdefn))
	(let* ((ir-freevars (pvs2ir-freevars* ir-lambda-expr))
	       (ir-fvar-types (loop for fvar in ir-freevars collect (freevar-type fvar)))
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
	       (ir2c-rangetype (ir2c-type ir-rangetype))
	       (c-rangetype (add-c-type-definition ir2c-rangetype))
	       (c-domaintype (add-c-type-definition (ir2c-type ir-domaintype)))
	       (c-funtype (or c-return-type (add-c-type-definition (ir2c-type (mk-ir-funtype ir-domaintype ir-rangetype)))))
	       ;(ir-body (ir-body ir-lambda-expr))
	       (closure-name-root (format nil "~a_closure_~a" *theory-id* (length *c-type-info-table*)))
	       (closure-type-name (format nil "~a_t" closure-name-root))
	       (closure-struct-name (format nil "~a_s" closure-name-root))
	       (bvar-ctypes
		(loop for bv in ir-boundvars collect (format nil "~a_t" (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype bv)))))))
	       (bvar-cvars (if (eql (length ir-boundvars) 1) (list "bvar")
			     (loop for bv in ir-boundvars as i from 1 collect (format nil "bvar_~a" i))))
	       (bvar-cvar-decls (loop for bv-ctype in bvar-ctypes
				      as bv in bvar-cvars
				      collect (format nil "~a ~a" bv-ctype bv)))
	       (fvar-cvars (loop for fv in ir-fvar-ctypes as i from 1 collect (format nil "fvar_~a" i)))
	       (fvar-cvar-decls (loop for fv-ctype in ir-fvar-ctypes
				      as fv in fvar-cvars
				      collect (format nil "~a_t ~a" fv-ctype fv)))
	       (theory-params *ir-theory-formals*)
	       (theory-formals *theory-formals*)
	       (theory-c-params (ir2c-theory-formals theory-params theory-formals))
	       (theory-c-params-variadic (ir2c-theory-formals-variadic "closure" theory-params theory-formals))
	       (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
	       (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								theory-params
								collect (ir-name ir-formal))))
	       (c-param-cvars-string (substitute #\; #\, c-param-decl-string))
	       (ftbl-type-name (format nil "~a_ftbl" c-funtype))
	       (closure-type-decl (format nil "~%struct ~a;~%~8Ttypedef struct ~a * ~a;"
					  closure-struct-name closure-struct-name closure-type-name))
	       (closure-type-defn ;the fptr takes the closure itself and a pointer to the lambda-args
		(format nil "struct ~a {uint32_t count;~%~8T ~a_t ftbl;~%~8T ~a_htbl_t htbl~{;~%~8T~a~^~}~a;};"
			closure-struct-name 
			ftbl-type-name c-funtype
			fvar-cvar-decls
			c-param-cvars-string
			))
	       (closure-fptr-name (format nil "f_~a" closure-name-root))
	       (closure-mptr-name (format nil "m_~a" closure-name-root))
	       (closure-hptr-name (format nil "h_~a" closure-name-root))
	       ;;(closure-cptr-name (format nil "copy_~a" closure-name-root))
	       ;; (closure-cptr-header (format nil "struct ~a * ~a(struct ~a * closure)"
	       ;; 				    closure-struct-name closure-cptr-name closure-struct-name))
	       (closure-fptr-header
		(format nil "~a_t ~a(struct ~a * closure, ~a_t bvar);"
			(mppointer-type c-rangetype)
			closure-fptr-name closure-struct-name (mppointer-type c-domaintype)))
		;(when (null c-rangetype) (break "closure"))
		;; (case c-rangetype
		;;   ((|mpq| |mpz|)
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
		;; 		      ((|mpq| |mpz|)
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
			       nconc (let* ((atype (add-c-type-definition (ir2c-type (ir-vtype ir-bvar))))
					    (lhs (format nil "bvar_~a" i))
					    (rhs (format nil "bvar->project_~a" i))
					    (init-instrs (cons (format nil "~a_t ~a"  atype lhs)
							       (ir2c-initialize lhs atype rhs atype)))
					    (count-instrs (when (ir-reference-type? (ir-vtype ir-bvar))
							    (list (format nil "~a->count++" rhs))))) ;; i
				       (nconc init-instrs count-instrs)))
			 (list (format nil "release_~a(bvar~a)" c-domaintype
				       c-param-arg-string)))))
	       ;;(8/7/21) Might need all theory formals and not just the free ones. 
	       (release-bvar-projections
		(when (> (length ir-boundvars) 1)
		  (loop for ir-bvar in ir-boundvars
			as i from 1
			when (or (ir-reference-type? (ir-vtype ir-bvar))
				 (gmp-type? (ir2c-type (ir-vtype ir-bvar))))
			collect (let ((atype (add-c-type-definition (ir2c-type (ir-vtype ir-bvar)))))
				  (format nil "release_~a(bvar_~a)" atype i)))))
	       (bvar-fvar-args (format nil "~{~a~^, ~}~{, ~a~}" ;~{, ~a~}
				       bvar-cvars
				       (loop for i from 1 to (length ir-freevars)
					     collect (format nil "closure->fvar_~a" i))
				       ;;(loop for param in theory-params collect (format nil "closure->~a" (ir-formal-id param)))
				       ))
	       (hashable-domain (ir-hashable-index? (ir2c-type ir-domaintype)))
	       ;;(hashable-range (ir-hashable-index? (ir2c-type ir-rangetype)))
	       (closure-fptr-defn 
		(if hashable-domain  ;;was (and hashable-domain hashable-range)
		    (format nil "~a_t ~a(struct ~a * closure, ~a_t bvar){~
~%if (closure->htbl != NULL){~
~%~8T~a_htbl_t htbl = closure->htbl;~
~%~8Tuint32_t hash = ~a_hash(bvar);~
~%~8Tuint32_t hashindex = lookup_~a(htbl, bvar, hash);~
~a~
~%~8Tif (!keyzero || entry.keyhash != 0){~
~%~12T~a_t result;~
~{~%~12T~a;~}~
~%~12Treturn result;}~
~%~8T
~%~8Treturn ~a(~a);};
~%return ~a(~a);}"
			    (mppointer-type c-rangetype) closure-fptr-name closure-struct-name (mppointer-type c-domaintype)
			    c-funtype
			    hashable-domain
			    c-funtype
			    (format nil "~%~8T~a_hashentry_t entry = htbl->data[hashindex];~%~8Tbool_t keyzero;~{~%~8T ~a;~}"
				    c-funtype;
				    (ir2c-arith-relations '== "keyzero"  (list "entry.key" 0)
							  (list c-domaintype '|uint8|)))
			    (mppointer-type c-rangetype)
			    (mk-c-assignment-with-count "result" ir2c-rangetype
							"entry.value" c-rangetype)
			    closure-hptr-name bvar-fvar-args
			    closure-hptr-name bvar-fvar-args)
		(format nil "~a_t ~a(struct ~a * closure, ~a_t bvar){~{~%~8T~a;~}~%~8T~a_t result = ~a(~a); ~{~%~8T~a;~}~%~8Treturn result;}"
			(mppointer-type c-rangetype)
			closure-fptr-name closure-struct-name (mppointer-type c-domaintype)
			bvar_projections
			(mppointer-type c-rangetype)
			closure-hptr-name
			bvar-fvar-args
			release-bvar-projections
			)
		;; (case c-rangetype
		;;   ((|mpq| |mpz|)
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
	       (closure-fptr-definition (mk-c-noextern-defn-info closure-fptr-name closure-fptr-header
							closure-fptr-defn))
	       (closure-mptr-defn
		(format nil "~a_t ~a(struct ~a * closure, ~{~a~^, ~}){~%~8Treturn ~a(~a);}"
			(mppointer-type c-rangetype)
			closure-mptr-name closure-struct-name bvar-cvar-decls
			closure-hptr-name
			bvar-fvar-args
			)
		;; (case c-rangetype
		;;   ((|mpq| |mpz|)
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
	       (closure-mptr-definition (mk-c-noextern-defn-info closure-mptr-name closure-mptr-header
							closure-mptr-defn))
	       (fptr-cast (format nil "~a_t (*)(~a_t, ~a_t)" (mppointer-type c-rangetype)
				  c-funtype (mppointer-type c-domaintype)))
	       ;; (case c-rangetype
	       ;; 			 ((|mpq| |mpz|)
	       ;; 			  (format nil "void (*)(~a_t, ~a_t, ~a_t)" c-funtype c-rangetype c-domaintype))
	       ;; 			 (t (format nil "~a_t (*)(~a_t, ~a_t)" c-rangetype c-funtype c-domaintype)))
	       (mptr-cast (format nil "~a_t (*)(~a_t, ~{~a~^, ~})" (mppointer-type c-rangetype)
				  c-funtype bvar-ctypes)
			  ;; (case c-rangetype
			    ;; ((|mpq| |mpz|)
			    ;;  (format nil "void (*)(~a_t, ~a_t, ~{~a~^, ~})" c-funtype c-rangetype bvar-ctypes))
			    ;; (t (format nil "~a_t (*)(~a_t, ~{~a~^, ~})" c-rangetype c-funtype bvar-ctypes)))
			  )
	       (rptr-cast (format nil "void (*)(~a_t)" c-funtype))
	       (cptr-cast (format nil "~a_t (*)(~a_t)" c-funtype c-funtype))
	       (copy-info (make-closure-copy-info closure-name-root c-funtype ir-rangetype ir-freevars))
	       (new-info (make-closure-new-info closure-name-root ftbl-type-name fptr-cast mptr-cast rptr-cast cptr-cast ir-freevars));can reuse record-new-info
	       (release-info (make-closure-release-info c-funtype closure-name-root ir-fvar-types ir-fvar-ctypes
							c-param-arg-string c-param-decl-string))
	       (closure-defn-info
		(mk-c-closure-info ir-lambda-expr closure-name-root closure-type-decl closure-type-defn closure-fptr-definition closure-mptr-definition nil new-info release-info copy-info)))
	  (declare (ignore theory-c-params-variadic))
	  (push closure-defn-info
		*c-type-info-table*)
	  (let ((closure-function-definition
		 (make-c-closure-defn-info ir-lambda-expr closure-hptr-name c-param-decl-string))
		)
	     ;(break "add-closure-definition")
	    ;(when (null closure-function-definition) (break "add-closure-definition"))
	    (setf (hdefn closure-defn-info) closure-function-definition)
	    closure-name-root)))))


(defun make-closure-copy-info (type-name-root c-funtype ir-rangetype ir-freevars)
  (let* ((new-name (intern (format nil "copy_~a" type-name-root)))
	 (new-header (format nil "~a_t copy_~a(~a_t x);" type-name-root type-name-root type-name-root))
	 (new-defn (format nil "~a_t copy_~a(~a_t x){~%~8T~a_t y = new_~a();
~8Ty->ftbl = x->ftbl;
~{~%~8T~a;~}
~8Tif (x->htbl != NULL){
~12T~a_htbl_t new_htbl = (~a_htbl_t) safe_malloc(sizeof(struct ~a_htbl_s));
~12Tnew_htbl->size = x->htbl->size;
~12Tnew_htbl->num_entries = x->htbl->num_entries;
~12T~a_hashentry_t * new_data = (~a_hashentry_t *) safe_malloc(new_htbl->size * sizeof(struct ~a_hashentry_s));
~12Tmemcpy(new_data, x->htbl->data, (new_htbl->size * sizeof(struct ~a_hashentry_s)));
~12Tnew_htbl->data = new_data;~a;
~12Ty->htbl = new_htbl;
~8T} else
~12T{y->htbl = NULL;};
~8Treturn y;
}"
			   type-name-root type-name-root type-name-root type-name-root type-name-root
			   (loop for ifv in ir-freevars as i from 1
				 collect
				 (let* ((fv-type (freevar-type ifv))
				       (fv-ctype (add-c-type-definition (ir2c-type fv-type))))
				   (if (ir-reference-type? fv-type)
				       (format nil "y->fvar_~a = x->fvar_~a; x->fvar_~a->count++"
					       i i i)
				     (make-c-assignment (format nil "y->fvar_~a" i) fv-ctype
							(format nil "x->fvar_~a" i) fv-ctype))))
			   c-funtype c-funtype c-funtype
			   c-funtype c-funtype c-funtype
			   c-funtype
			   (if (ir-reference-type? ir-rangetype)
			       (format nil "~%~12Tfor (uint32_t j = 0; j < new_htbl->size; j++){~
~%~24Tif ((new_htbl->data[j].key != 0) || new_htbl->data[j].keyhash != 0) new_htbl->data[j].value->count++;}")
			     (format nil ""))			   
			   )))
	 (mk-c-noextern-defn-info new-name new-header new-defn (list type-name-root) type-name-root )))

(defun make-closure-new-info (type-name-root ftbl-type-name fptr-cast mptr-cast rptr-cast cptr-cast ir-freevars)
    (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	   (new-header (format nil "~a_t new_~a(void);" type-name-root type-name-root))
	   (gmp-freevars-init (loop for ifv in ir-freevars
				    as i from 1
				    when (mpnumber-type? (ir2c-type (freevar-type ifv)))
				    collect (format nil "~a_init(tmp->fvar_~d)" (ir2c-type (freevar-type ifv)) i)))
	   (new-defn   (format nil "~a_t new_~a(void){~%~8Tstatic struct ~a_s ftbl = {.fptr = (~a)&f_~a, .mptr = (~a)&m_~a, .rptr =  (~a)&release_~a, .cptr = (~a)&copy_~a};~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s));~%~8Ttmp->count = 1;~%~8Ttmp->ftbl = &ftbl;~%~8Ttmp->htbl = NULL;~{~%~8T~a;~}~%~8Treturn tmp;}"
			       type-name-root type-name-root
			       ftbl-type-name fptr-cast type-name-root mptr-cast type-name-root rptr-cast type-name-root cptr-cast type-name-root
			       type-name-root type-name-root type-name-root gmp-freevars-init)))
      (mk-c-noextern-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-closure-release-info (funtype-root closure-name-root ir-fvar-types ir-fvar-ctypes
					       c-param-arg-string
					       c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" closure-name-root)))
	 (release-header (format nil "void release_~a(~a_t closure~a);" closure-name-root funtype-root
				 c-param-decl-string))
	 (release-defn (let ((release-fields (loop for ft in ir-fvar-types
						   as cft in ir-fvar-ctypes
						   as i from 1 
						   when (and (ir-reference-type? ft)
							     (not (ir-formal-typename? ft)))
						   collect (format nil "release_~a(x->fvar_~a~a)" cft i
								   c-param-arg-string))))
			 ;;(8/7/21)This assumes that all reference types are locally (re-)defined.
			 ;;Need to revisit this if types are parametric.  
			 (format nil "void release_~a(~a_t closure~a){~%~8T~a_t x = (~a_t)closure;~%~8Tx->count--;~%~8Tif (x->count <= 0){~{~%~8T~8T~a;~}~%~8T//printf(\"\\nFreeing\\n\");~%~8Tsafe_free(x);}}"
				 closure-name-root funtype-root c-param-decl-string closure-name-root closure-name-root
				 release-fields))))
    (mk-c-noextern-defn-info release-name release-header release-defn (list funtype-root) 'void)))

(defmacro push-new-type-info (c-type-info info-table)
  `(let ((c-type-info ,c-type-info))
     (unless (get-c-type-info (ir-texpr c-type-info))
       (push c-type-info ,info-table))))

										  
(defmethod push-type-info-to-decl (c-type-info (decl const-decl))
					;  (when (consp c-type-info) (break "push-type-info-to-decl"))
  (assert (or (not (ir-type? (ir-texpr c-type-info)))
	      (not (null (unique-name (ir-texpr c-type-info))))))
;;  (assert (null (get-c-type-info (ir-texpr c-type-info))))
  (push-new-type-info  c-type-info *c-type-info-table*)
  (push-new-type-info c-type-info (c-type-info-table (eval-info decl))))

(defmethod push-type-info-to-decl (c-type-info (decl formal-const-decl))
					;  (when (consp c-type-info) (break "push-type-info-to-decl"))
    (assert (or (not (ir-type? (ir-texpr c-type-info)))
		(not (null (unique-name (ir-texpr c-type-info))))))
;;    (assert (null (get-c-type-info (ir-texpr c-type-info))))
  (push-new-type-info c-type-info *c-type-info-table*)
  (push-new-type-info c-type-info (c-type-info-table (eval-info decl))))

(defmethod push-type-info-to-decl (c-type-info (decl type-decl))
					;  (when (consp c-type-info) (break "push-type-info-to-decl"))
  (assert (or (null (ir-texpr c-type-info))
	      (not (ir-type? (ir-texpr c-type-info)))
	      (not (null (unique-name (ir-texpr c-type-info))))))
;;    (assert (null (get-c-type-info (ir-texpr c-type-info))))
  (push-new-type-info c-type-info *c-type-info-table*)
  (when (null (ir-type-value decl))
    (let ((ir-type-name (mk-ir-typename (pvs2ir-unique-decl-id decl) nil nil nil decl)));;added params
      (push ir-type-name *ir-type-info-table*)
      (setf (ir-type-value decl)
	    (mk-eval-type-info ir-type-name))))
  (push-new-type-info c-type-info (c-type-info-table (ir-type-value decl))))

(defmethod push-type-info-to-decl (c-type-info (decl test-formula))
  (assert (or (not (ir-type? (ir-texpr c-type-info)))
	      (not (null (unique-name (ir-texpr c-type-info))))))
  (push-new-type-info c-type-info *c-type-info-table*)
  (push-new-type-info c-type-info (c-type-info-table (eval-info decl))))

(defmethod push-type-info-to-decl (c-type-info (decl t))
  ;;  (when (consp c-type-info) (break "push-type-info-to-decl"))
  (unless *to-emacs*
    (format t "\nEmpty decl"))
  (with-slots (ir-expr tname) c-type-info
    (format t "~%Adding ~a: ~a to c-type-info-table" tname (print-ir ir-expr)))
  (push-new-type-info  c-type-info *c-type-info-table*))

(defmethod get-c-type-info-table ((decl  const-decl))
  (c-type-info-table (eval-info decl)))

(defmethod get-c-type-info-table ((decl  formal-const-decl))
  (c-type-info-table (eval-info decl)))

(defmethod get-c-type-info-table ((decl type-decl))
  (c-type-info-table (ir-type-value decl)))

(defmethod add-c-type-definition ((ir2c-type ir-chartype) &optional tname)
  (declare (ignore tname))
  '|uint32|);was char, but chartype now includes unicode (NSH: 5/31/21)

;deprecated
(defmethod add-c-type-definition ((ir2c-type ir-stringtype) &optional tname)
  (declare (ignore tname))
  'string)

  

(defmethod add-c-type-definition ((ir2c-type t) &optional tname)
  (cond (tname
	 (let ((c-type-info (get-c-type-info-by-name tname)))
	   ;(when (null ir2c-type) (break "add-c-type-definition"))
	   (cond (c-type-info tname)
		 (t (let ((c-type-info (mk-simple-c-type-info ir2c-type tname (format nil "typedef ~a_t ~a_t;" ir2c-type tname) nil (format nil "~a" (id *pvs2c-current-decl*)))));;replace nil with act-defn
		      (push-type-info-to-decl c-type-info
						*pvs2c-current-decl*)
		      tname)))))
	(t ir2c-type)))

(defmethod add-c-type-definition ((ir2c-type ir-type-actual) &optional tname) 
  (with-slots (ir-actual-type) ir2c-type
    (add-c-type-definition ir-actual-type tname)))


;;NSH(4-9-22): Typenames can have parameters which makes it challenging to import generated C code
;;from other modules.  Add-c-type-definition either retrieves an existing definition or creates a new
;;one for the given type along with the copy, equal, update, dupdate, release definitions, and returns
;;the type name.  If a type corresponds to a parametric name imported from another theory, then the
;;parameters need to be evaluated in order to execute the type operations.  However, the type definitions
;;are context-free: they are standalone definitions that are not within an evaluation context.
;;There is also a problem with ignoring the imported type and using the definition to create a new
;;type local to the theory where the type information is lost.  This happens for example with
;;bytevector[m + n], where we have already checked that m + n is within the array index bound.
;;If we expand the definition of bytevector, this type information is lost and the expanded
;;definition is translated to a function type. 
(defmethod add-c-type-definition ((ir2c-type ir-typename) &optional tname) ;;tname is ignored
  ;(format t "~%add-c-type-def ~a" (print-ir ir2c-type))
  ;;(break "add-c-td ir-typename")
  (declare (ignore tname))
  (with-slots (ir-type-id ir-type-defn type-declaration) ir2c-type
    (if (ir-typename? ir-type-defn)
	(add-c-type-definition ir-type-defn ir-type-id)
      (if (and type-declaration
     	       (type-eq-decl? type-declaration))
     	  (if (or (eq (id (module type-declaration)) *theory-id*)
		  (formals-sans-usings (module type-declaration))
		  (decl-formals type-declaration))
	      (let ((c-type-info (get-c-type-info-by-name ir-type-id)))
    		(cond (c-type-info ir-type-id)
		      (t ;(when (eq (id (module type-declaration)) *theory-id*) (break "add-c-type ir-typename"))
		       (if (eq (id (module type-declaration)) *theory-id*)
			   (add-c-type-definition  ir-type-defn ir-type-id)
			   (add-c-type-definition  ir-type-defn ))))); ir-type-id
	    ir-type-id)
	ir-type-id))))


    ;; (let ((declared-type-id (and type-declaration (ir-type-value type-declaration)
    ;; (cond ((and type-declaration
    ;; 		(type-eq-decl? type-declaration)
    ;; 		(eq (id (module type-declaration)) *theory-id*))
    ;; 	   (let ((c-type-info (get-c-type-info-by-name ir-type-id)))
    ;; 	     (cond (c-type-info ir-type-id)
    ;; 		   (t (let* ((typedef-name (add-c-type-definition (ir2c-type ir-type-defn) tname))
    ;; 			     (c-type-info (mk-simple-c-type-info ir2c-type ir-type-id
    ;; 								 (format nil "typedef ~a_t ~a_t;" typedef-name ir-type-id) nil)))
    ;; 			(push-type-info-to-decl c-type-info
    ;; 						*pvs2c-current-decl*))))))
    ;; 	  (t ir-type-id))))


      ;; (let* ((c-type-info (get-c-type-info ir2c-type))
      ;; 	     (c-type-name (when c-type-info (tname c-type-info))))
      ;; 	(or c-type-name ir-type-id)))))

(defmethod add-c-type-definition ((ir2c-type ir-formal-typename) &optional tname) ;;tname is ignored
  (declare (ignore tname))
  ;;we assume that a named type already has generated code
  (with-slots (ir-type-id) ir2c-type
    (let* ((c-type-info (get-c-type-info ir2c-type))
	   (c-type-name (when c-type-info (tname c-type-info))))
      (or c-type-name ir-type-id))))

(defmethod add-c-type-definition ((ir2c-type ir-funtype) &optional tname)
  (with-slots (ir-domain ir-range) ir2c-type
	      (let* ((c-type-info (get-c-type-info ir2c-type))
		     (c-type-name (when c-type-info (tname c-type-info))))
		(or c-type-name
		    (let* ((c-domain-root (add-c-type-definition (ir2c-type ir-domain))) ;remove ir2c-type
			   (elemtype (ir2c-type ir-range))
			   (c-range-root (add-c-type-definition elemtype))
			   (type-name-root (or tname (format nil "~a_funtype_~a"
							     *theory-id*
							     (length *c-type-info-table*))))
			   (type-name (intern (format nil "~a_t" type-name-root)))
			   (struct-name (intern (format nil "~a_s" type-name-root)))
			   (theory-params *ir-theory-formals*)
			   (theory-formals *theory-formals*
					   ;; (loop for ir-formal in theory-params
					   ;;       collect (car (rassoc (ir-type-id ir-formal)
					   ;; 			    *ir-theory-tbindings* :key #'ir-name)))
					   )
			   (theory-c-params (ir2c-theory-formals theory-params theory-formals))
			   (theory-c-types (ir2c-theory-formal-types theory-params theory-formals))
			   ;;(theory-c-params-variadic (ir2c-theory-formals-variadic "closure" theory-params theory-formals))
			   (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
			   (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
									    theory-params
									    collect (ir-name ir-formal))))
			   ;;(c-param-type-string (format nil "~{, ~a_t~}" theory-c-types))
			   ) ;(free-ir-formals ir2c-type))
		      (declare (ignore theory-c-types))
		      ;(break "add-c-type-definition ir-funtype")
		      (if nil ;;(ir-index? ir-domain);;NSH(9/15/2022): remove this option
			  (let* ((size (ir-index? ir-domain))
				 (type-defn (format nil "struct ~a { uint32_t count;~% ~a_t elems[~a]; };~%typedef struct ~a * ~a;"
						    struct-name (mppointer-type c-range-root) size struct-name type-name))
				 (new-info (make-array-new-info type-name-root size elemtype))
				 (release-info (make-array-release-info type-name-root size elemtype c-range-root c-param-arg-string c-param-decl-string))
				 (release-ptr-info (make-array-release-ptr-info type-name-root size elemtype c-range-root c-param-arg-string theory-c-params-variadic c-param-decl-string))
				 (copy-info (make-array-copy-info type-name-root size elemtype c-range-root c-param-decl-string))
				 (equal-info (make-array-equal-info type-name-root size elemtype  c-range-root c-param-decl-string c-param-arg-string))
				 (equal-ptr-info (make-array-equal-ptr-info type-name-root size elemtype  c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string))
				 (json-info (make-array-json-info type-name-root size elemtype  c-range-root c-param-decl-string c-param-arg-string))
				 (json-ptr-info (make-array-json-ptr-info type-name-root size elemtype  c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string))
				 (update-info (list (make-array-update-info type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)))
				 (actual-info (make-actual-info type-name-root theory-params c-param-decl-string))
				 (decl-info (format nil "~a" (id *pvs2c-current-decl*)))
				 );(break "add-c-type-definition(ir-funtype)");(break "Shouldn't reach here")
			    (push-type-info-to-decl
			     (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info release-ptr-info copy-info equal-info equal-ptr-info json-info json-ptr-info update-info decl-info) ; deleted actual-info
			     *pvs2c-current-decl*)
			    type-name-root)
			(let* ((hash-entry-type-defn
				(format nil "struct ~a_hashentry_s {uint32_t keyhash; ~a_t key; ~a_t value;}; ~%typedef struct ~a_hashentry_s ~a_hashentry_t;" type-name-root (mppointer-type c-domain-root) c-range-root type-name-root type-name-root))
				(hash-type-defn
				 (format nil "struct ~a_htbl_s {uint32_t size; uint32_t num_entries; ~a_hashentry_t * data;}; ~%typedef struct ~a_htbl_s * ~a_htbl_t;" type-name-root type-name-root type-name-root type-name-root))
				(arg-types (if (ir-tupletype? ir-domain)
					       (loop for fld in (ir-field-types ir-domain)
						     collect (mppointer-type (add-c-type-definition (ir-vtype fld))))
					     (list (mppointer-type c-domain-root))))
				(ftable-name (intern (format nil "~a_ftbl" type-name-root)))
				(ftable-type-defn
				 (format nil "struct ~a_s {~a_t (* fptr)(struct ~a *, ~a_t);~%~8T~a_t (* mptr)(struct ~a *, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
					 ftable-name (mppointer-type c-range-root)
					 struct-name (mppointer-type c-domain-root)
					 (mppointer-type c-range-root)
					 struct-name arg-types  
					 struct-name 
					 struct-name struct-name ftable-name ftable-name)
				 ;; (case c-range-root
				 ;;  ((|mpq| |mpz|)
				 ;;   (format nil "struct ~a_s {void (* fptr)(struct ~a *, ~a_t, ~a_t);~%~8Tvoid (* mptr)(struct ~a *, ~a_t, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
				 ;; 		  ftable-name  struct-name c-range-root c-domain-root struct-name c-range-root arg-types struct-name struct-name struct-name ftable-name ftable-name))
				 ;;  (t (format nil "struct ~a_s {~a_t (* fptr)(struct ~a *, ~a_t);~%~8T~a_t (* mptr)(struct ~a *, ~{~a_t~^, ~});~%~8Tvoid (* rptr)(struct ~a *);~%~8Tstruct ~a * (* cptr)(struct ~a *);};~%typedef struct ~a_s * ~a_t;"
				 ;; 	     ftable-name c-range-root struct-name c-domain-root  c-range-root struct-name arg-types  struct-name struct-name struct-name ftable-name ftable-name)))
				 )
				(type-decl (format nil "struct ~a;~%~8Ttypedef struct ~a * ~a;" struct-name
						   struct-name type-name))
				(type-defn (format nil "struct ~a {uint32_t count;~%~8T~a_ftbl_t ftbl;~%~8T~a_htbl_t htbl;};~%typedef struct ~a * ~a;" struct-name  type-name-root type-name-root struct-name type-name))
				(hashtype (ir-hashable-index? ir-domain))
				;;need range to be a fixed-width int
				(release-info (make-function-release-info type-name-root c-param-decl-string))
				(copy-info (make-function-copy-info type-name-root)) ;; c-domain-root
				(lookup-info (when hashtype (make-function-lookup-info type-name-root c-domain-root hashtype)))
				(dupdate-info (when hashtype (make-function-dupdate-info type-name-root c-domain-root hashtype c-range-root elemtype c-param-arg-string c-param-decl-string)))
				(update-info (when hashtype (make-function-update-info type-name-root c-domain-root c-range-root c-param-arg-string c-param-decl-string)))
				(equal-info (make-function-equal-info type-name-root c-param-decl-string))
				(json-info (make-function-json-info type-name-root c-param-decl-string)))
			  ;;equal is defined to return false on function types.
			  ;(break "add-c- ir-funtype")
			  ;(when (get-c-type-info ir2c-type) (break "add-c- ir-funtype"))
			  (push-type-info-to-decl
			   (mk-closure-c-type-info ir2c-type type-name-root type-decl type-defn ftable-type-defn
						   release-info copy-info  hash-entry-type-defn hash-type-defn
						   lookup-info dupdate-info update-info equal-info json-info
						   (format nil "~a" (id *pvs2c-current-decl*)))
			   *pvs2c-current-decl*)))
		      type-name-root)))))

;;equality and closures will return false with a warning
(defun make-function-equal-info (type-name-root c-param-decl-string)
  (let* ((new-name (intern (format nil "equal_~a" type-name-root)))
	 (new-header (format nil "bool_t equal_~a(~a_t x, ~a_t y~a);"
			      type-name-root type-name-root type-name-root c-param-decl-string))
	 (new-defn (format nil "bool_t equal_~a(~a_t x, ~a_t y~a){~%~8Treturn false;}"
			   type-name-root type-name-root type-name-root c-param-decl-string)))
    (mk-c-defn-info new-name new-header new-defn (list type-name-root type-name-root) type-name-root )))

(defun make-function-json-info (type-name-root c-param-decl-string)
  (let* ((name (format nil "json_~a" type-name-root))
	 (header (format nil "char* ~a(~a_t x~a)" name type-name-root c-param-decl-string))
	 (type-name-root-string (string type-name-root))
	 (defn (format nil "char* ~a(~a_t x~a){char * result = safe_malloc(~a); sprintf(result, \"%s\", \"\\\"~a\\\"\"); return result;}"
		       name type-name-root c-param-decl-string
		       (+ (length type-name-root-string) 10) type-name-root-string)))
    (mk-c-defn-info name  (format nil "~a;" header) defn (list type-name-root) type-name-root)))

(defun make-function-copy-info (type-name-root) ;; c-domain-root
  (let* ((name (format nil "copy_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t x)" type-name-root name type-name-root ;;(mppointer-type c-domain-root)
			 ))
	 (body   (format nil "{return x->ftbl->cptr(x);}"))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "~a;" header) defn (list type-name-root) type-name-root)))

(defun make-function-lookup-info (type-name-root c-domain-root htype )
  (declare (ignore htype))
  (let* ((name (format nil "lookup_~a" type-name-root))
	 (header (format nil "uint32_t ~a(~a_htbl_t htbl, ~a_t i, uint32_t ihash)" name type-name-root (mppointer-type c-domain-root)))
	 (keyzero-instrs (ir2c-arith-relations '== "keyzero"  (list "data.key" 0)
					       (list c-domain-root '|uint8|)))
	 (keymatch-instrs (ir2c-arith-relations '== "keymatch"  (list "data.key" "i")
						(list c-domain-root c-domain-root)))
	 (body   (format nil "{
~8Tuint32_t mask = htbl->size - 1;
~8Tuint32_t hashindex = ihash & mask;
~8T~a_hashentry_t data = htbl->data[hashindex];
~8Tbool_t keyzero;
~{~%~8T~a;~}
~8Tbool_t keymatch;~
~{~%~8T~a;~}
~8Twhile ((!keyzero || data.keyhash != 0) &&
~16T (data.keyhash != ihash || !keymatch)){
~16Thashindex++;
~16Thashindex = hashindex & mask;
~16Tdata = htbl->data[hashindex];
~{~%~16T~a;~}
~{~%~16T~a;~}
~16T}
~8Treturn hashindex;
~8T}"
			 type-name-root
			 keyzero-instrs
			 keymatch-instrs
			 keyzero-instrs
			 keymatch-instrs
			 ))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "~a;" header) defn
		    (list (format nil "~a_htbl" type-name-root) c-domain-root '|uint32|))))

(defun make-function-update-info (type-name-root c-domain-root c-range-root c-param-arg-string c-param-decl-string)
  (let* ((name (format nil "update_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t a, ~a_t i, ~a_t v~a)" type-name-root name type-name-root (mppointer-type c-domain-root)(mppointer-type  c-range-root) c-param-decl-string))
	 (body (format nil "{
~8Tif (a->count == 1){
~16Treturn dupdate_~a(a, i, v~a);
~12T} else {
~16T~a_t x = copy_~a(a);
~16Ta->count--;
~16Treturn dupdate_~a(x, i, v~a);
~12T}}" type-name-root c-param-arg-string type-name-root type-name-root type-name-root c-param-arg-string))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name (format nil "~a;" header) defn (list type-name-root c-domain-root c-range-root) type-name-root)))

(defun make-function-dupdate-info (type-name-root c-domain-root htype c-range-root elemtype c-param-arg-string c-param-decl-string)
  (let* ((name (format nil "dupdate_~a" type-name-root))
	 (header (format nil "~a_t ~a(~a_t a, ~a_t i, ~a_t v~a)" type-name-root name type-name-root (mppointer-type c-domain-root) (mppointer-type c-range-root) c-param-decl-string))
	 (body (function-destructive-update-body type-name-root c-domain-root htype c-range-root elemtype c-param-arg-string))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name  (format nil "~a;" header) defn (list type-name-root c-domain-root c-range-root) type-name-root)))

(defun ir2c-initialize (lhs lctype rhs rctype)
  (let ((assignment (make-c-assignment lhs lctype rhs rctype)))
    (if (mpnumber-type? lctype)
	(list (format nil "~a_init(~a)" lctype lhs)
	      assignment)
      (list assignment))))


(defun function-destructive-update-body (type-name-root c-domain-root htype c-range-root elemtype c-param-arg-string)
  (format nil "{~%~8T~a_htbl_t htbl = a->htbl;~
~%~8Tif (htbl == NULL){//construct new htbl~
~%~16Thtbl = (~a_htbl_t)safe_malloc(sizeof(struct ~a_htbl_s));~
~%~16Thtbl->size = HTBL_DEFAULT_SIZE; htbl->num_entries = 0;~
~%~16Thtbl->data = (~a_hashentry_t *)safe_malloc(HTBL_DEFAULT_SIZE * sizeof(struct ~a_hashentry_s));~
~%~16Tfor (uint32_t j = 0; j < HTBL_DEFAULT_SIZE; j++){~{~a;~} htbl->data[j].keyhash = 0;~
~%~16T}~
~%~16Ta->htbl = htbl;~
~%~8T}~
~%~8Tuint32_t size = htbl->size;~
~%~8Tuint32_t num_entries = htbl->num_entries;~
~%~8T~a_hashentry_t * data = htbl->data;~
~%~8Tif (num_entries/3 >  size/5){//resize data~
~%~16Tuint32_t new_size = 2*size; uint32_t new_mask = new_size - 1;~
~%~16Tif (size >= HTBL_MAX_SIZE) out_of_memory();~
~%~16T~a_hashentry_t * new_data = (~a_hashentry_t *)safe_malloc(new_size * sizeof(struct ~a_hashentry_s));~
~%~16Tfor (uint32_t j = 0; j < new_size; j++){~
~%~24Tnew_data[j].key = 0;~
~%~24Tnew_data[j].keyhash = 0;}~
~%~16Tfor (uint32_t j = 0; j < size; j++){//transfer entries~
~%~24Tuint32_t keyhash = data[j].keyhash;~
~%~24Tbool_t keyzero;~
~%~24T~{~a;~}~
~%~24Tif (!keyzero || keyhash != 0){~
~%~32Tuint32_t new_loc = keyhash ^ new_mask;~
~{~%~32T~a;~}~
~%~32Twhile (keyzero && new_data[new_loc].keyhash == 0){~
~%~40Tnew_loc++;~
~%~40Tnew_loc = new_loc ^ new_mask;
~{~%~40T~a;~}~
~%~32T}~
~%~32T~a;~
~%~32Tnew_data[new_loc].keyhash = keyhash;~
~%~32T~a;~
~%~32T}}~
~%~16Thtbl->size = new_size;~
~%~16Thtbl->num_entries = num_entries;~
~%~16Thtbl->data = new_data;~
~%~16Tsafe_free(data);}~
~%~8Tuint32_t ihash = ~a_hash(i);~
~%~8Tuint32_t hashindex = lookup_~a(htbl, i, ihash);~
~%~8T~a_hashentry_t hentry = htbl->data[hashindex];~
~%~8Tuint32_t hkeyhash = hentry.keyhash;~
~%~8Tbool_t hentrykeyzero;~
~%~8T~{~a;~}
~%~8Tif (hentrykeyzero && (hkeyhash == 0))~
~%~16T{~a; htbl->data[hashindex].keyhash = ihash; ~{~a;~} htbl->num_entries++;}~
~%~12Telse {~{~a;~}};~
~%~8Treturn a;
~%}"
	  type-name-root
	  type-name-root type-name-root
	  type-name-root type-name-root
	  (ir2c-initialize "htbl->data[j].key" c-domain-root 0 '|uint8|)
	  type-name-root
	  type-name-root type-name-root type-name-root 
	  (ir2c-arith-relations '== "keyzero" (list "data[j].key"  0)
				(list c-domain-root '|uint8|))
	  (ir2c-arith-relations '== "keyzero" (list "new_data[new_loc].key"  0)
				(list c-domain-root '|uint8|))
	  (ir2c-arith-relations '== "keyzero" (list "new_data[new_loc].key"  0)
				(list c-domain-root '|uint8|))
	  (make-c-assignment "new_data[new_loc].key" c-domain-root "data[j].key" c-domain-root)
	  (make-c-assignment "new_data[new_loc].value" c-range-root  "data[j].value" c-range-root)
	  htype 
	  type-name-root
	  type-name-root
	  (ir2c-arith-relations '== "hentrykeyzero" (list "hentry.key"  0)
				(list c-domain-root '|uint8|))
	  (make-c-assignment "htbl->data[hashindex].key" c-domain-root "i" c-domain-root)
	  (make-c-assignment-with-count "htbl->data[hashindex].value" c-range-root "v" c-range-root)
	  (if (ir-reference-type? elemtype)
	      (cons (format nil "~a_t tempvalue" (mppointer-type c-range-root))
		    (cons (mk-c-assignment "tempvalue" c-range-root "htbl->data[hashindex].value" c-range-root)
		          (append (make-c-assignment-with-count "htbl->data[hashindex].value" c-range-root "v" c-range-root)
				  (list (format nil "if (v != NULL) v->count++")
					(if (ir-formal-typename? elemtype)
					    (format nil "if (tempvalue != NULL)~a->release_ptr(tempvalue, ~a)" c-range-root  c-range-root)
					  (format nil "if (tempvalue != NULL)release_~a(tempvalue~a)" c-range-root c-param-arg-string))))))
	    (make-c-assignment-with-count "htbl->data[hashindex].value" c-range-root "v" c-range-root))
	  ))

(defun make-function-release-info (type-name-root c-param-decl-string)
  (let* ((name (format nil "release_~a" type-name-root))
	 (header (format nil "void ~a(~a_t x~a)" name type-name-root c-param-decl-string))
	 (body (format nil "{~%~8Tx->count--;~%~12Tif (x->count <= 0)
~%~16Tx->ftbl->rptr(x);}"))
	 (defn (format nil "~a~a" header body)))
    (mk-c-defn-info name (format nil "~a;" header) defn (list type-name-root) 'void)))
;;Let the rptr do all the work including disposing the ftbl and htbl, so no type parameters needed
;; ~%~20Tif (x->htbl->data != NULL)
;; safe_free(x->htbl->data);
;; ~%~20Tsafe_free(x->htbl);}



(defmethod add-c-type-definition ((ir2c-type ir-arraytype) &optional tname)
  ;(break "add-c-type-def arraytype")
  (with-slots (size ir-range) ir2c-type
    (let* ((c-type-info (get-c-type-info ir2c-type)))
      (or (and c-type-info (tname c-type-info))
	  (let* ((c-range-root (add-c-type-definition (ir2c-type ir-range)))
		 (type-name-root (or tname (format nil "~a_array_~a"
						   *theory-id*
						   (length *c-type-info-table*))))
		 (type-name (intern (format nil "~a_t" type-name-root)))
		 (struct-name (intern (format nil "~a_s" type-name-root)))
		 (theory-params *ir-theory-formals*) ;(free-ir-formals ir2c-type)) ;;only ir-formal-typenames
		 (theory-formals *theory-formals*
				 ;; (loop for ir-formal in theory-params
				 ;;       collect (car (rassoc (ir-type-id ir-formal)
				 ;; 			    *ir-theory-tbindings* :key #'ir-name)))
				 )
		 (theory-c-params (ir2c-theory-formals theory-params theory-formals))
		 (theory-c-params-variadic (ir2c-theory-formals-variadic "actual" theory-params theory-formals))
		 (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
		 (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								  theory-params
								  collect (ir-name ir-formal))))
		 (type-defn (format nil "struct ~a { uint32_t count;~%~8Tuint32_t size;~%~8T uint32_t max;~%~8T ~a_t elems[]; };~%typedef struct ~a * ~a;"  
				    struct-name (mppointer-type c-range-root) ;size
				    struct-name type-name))) ;(break "add-c-type")
	    (make-array-c-type-info ir2c-type type-name-root type-defn size
				    ir-range c-range-root theory-params c-param-arg-string theory-c-params-variadic c-param-decl-string)
	    )))))

(defun make-array-c-type-info (ir2c-type type-name-root type-defn size elemtype c-range-root theory-params c-param-arg-string theory-c-params-variadic c-param-decl-string)
  (let* ((new-info (make-array-new-info type-name-root size elemtype))
	 (release-info (make-array-release-info type-name-root size elemtype
						c-range-root c-param-arg-string c-param-decl-string))
	 (release-ptr-info (make-array-release-ptr-info type-name-root size elemtype
						c-range-root c-param-arg-string theory-c-params-variadic c-param-decl-string))
	 (copy-info (make-array-copy-info type-name-root size elemtype c-range-root c-param-decl-string))
	 (equal-info (make-array-equal-info type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string))
	 (equal-ptr-info (make-array-equal-ptr-info type-name-root size elemtype c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string))
	 (json-info (make-array-json-info type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string))
	 (json-ptr-info (make-array-json-ptr-info type-name-root size elemtype c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string))
	 (update-info (list (make-array-update-info type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)))
	 (upgrade-info (list (make-array-upgrade-info type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)))
	 (actual-info (make-actual-info type-name-root theory-params c-param-decl-string))
	 (decl-info (format nil "~a" (id *pvs2c-current-decl*)))
	 )
    (push-type-info-to-decl
     (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info release-ptr-info copy-info equal-info equal-ptr-info json-info json-ptr-info update-info decl-info upgrade-info)
     *pvs2c-current-decl*)
    type-name-root))

(defun mk-ir-actual-info (type-defn fun-header fun-defn)
  (make-instance 'ir-actual-info
		 :ir-actual-type-defn type-defn
		 :ir-actual-fun-header fun-header
		 :ir-actual-fun-defn fun-defn))

(defun make-actual-info (type-name-root theory-params c-param-decl-string)
  (let* (;; (type-param-ids (loop for formal in theory-params
	 ;; 		      when (ir-formal-typename? formal)
 	 ;; 		      collect (ir-type-id formal)))
	 ;;NSH(1/3/21): removed ~%~8Tuint32_t count;
	 (actual-type-def   (format nil "typedef struct actual_~a_s {equal_ptr_t equal_ptr; release_ptr_t release_ptr; json_ptr_t json_ptr~a;} * actual_~a_t;"
			      type-name-root (substitute #\; #\, c-param-decl-string)
			      type-name-root)
	   )
	 ;; (actual-def (format nil "static struct type_actual_s actual_~a = {(*equal_~a_ptr), (*release_~a_ptr)};"
	 ;; 		     type-name-root type-name-root type-name-root))
	 (actual-def-header (format nil "actual_~a_t actual_~a(~a);" type-name-root type-name-root 
				    (if theory-params (subseq c-param-decl-string 2) "void"))) ;remove leading comma+space
 	 (actual-def (format nil "actual_~a_t actual_~a(~a){~
 ~%~8Tactual_~a_t new = (actual_~a_t)safe_malloc(sizeof(struct actual_~a_s));~
 ~%~8Tnew->equal_ptr = (equal_ptr_t)(*equal_~a_ptr);
 ~%~8Tnew->release_ptr = (release_ptr_t)(*release_~a_ptr);
 ~%~8Tnew->json_ptr = (json_ptr_t)(*json_~a_ptr);
 ~%~{~%~8T~a~}
 ~%~8Treturn new;
 };"
 		       type-name-root type-name-root ;removed ~{type_actual_t ~a~^,~} type-param-ids
		       (if (> (length c-param-decl-string) 2)
			   (subseq c-param-decl-string 2) ;remove leading comma+space
			   "")
 		       type-name-root type-name-root type-name-root
		       type-name-root type-name-root type-name-root ;;for equal/release/json_ptr
 		       (loop for param in theory-params collect
 			     (format nil "new->~a = ~a;" (ir-formal-id param) (ir-formal-id param))))))
    (mk-ir-actual-info actual-type-def actual-def-header actual-def)))

(defun make-array-new-info (type-name-root size elemtype)
  (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	 (new-header (make-new-header new-name type-name-root))
	 (new-defn (make-array-new-defn new-name type-name-root size elemtype)))
    (mk-c-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-new-header (new-name type-name-root) ; c-param-decl-string
  (format nil "~a_t ~a(uint32_t size);" type-name-root new-name))

;;array has a size (number of elements) and max (capacity).  Initially, both are set to the actual size,
;;but as the array grows, the max is double till it reaches the limit of 2^32.
(defun make-array-new-defn (new-name type-name-root size ir-range)
  (declare (ignore size))
  (let ((c-range-root  (add-c-type-definition (ir2c-type ir-range))))
    (if (ir-reference-type? ir-range)
	(format nil "~a_t ~a(uint32_t size){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s) + (size * sizeof(~a_t)));~%~8Ttmp->count = 1;~%~8Ttmp->size = size;~%~8Ttmp->max = size;~%~8Treturn tmp;}"
					;~%~8Tsize = ~d;~%~8Tmax = ~d;
		type-name-root new-name type-name-root type-name-root type-name-root ;size
		(mppointer-type c-range-root))
      (format nil "~a_t ~a(uint32_t size){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s) + (size * sizeof(~a_t)));~%~8Ttmp->count = 1;~%~8Ttmp->size = size;;~%~8Ttmp->max = size;~%~8T return tmp;}"  
	      type-name-root new-name type-name-root type-name-root type-name-root  (mppointer-type c-range-root)
	      ))))

(defun make-array-release-info (type-name-root size elemtype c-range-root c-param-arg-string c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (make-array-release-header release-name type-name-root c-param-decl-string))
	 (release-defn (make-array-release-defn release-name type-name-root size elemtype c-range-root c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-array-release-ptr-info (type-name-root size elemtype c-range-root c-param-arg-string theory-c-params-variadic c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)));;same name as release, _ptr added below
	 (release-ptr-header (make-array-release-ptr-header release-name type-name-root c-param-decl-string))
	 (release-ptr-defn (make-array-release-ptr-defn release-name type-name-root size elemtype c-range-root c-param-arg-string theory-c-params-variadic c-param-decl-string)))
    (mk-c-defn-info release-name release-ptr-header release-ptr-defn (list type-name-root) 'void)))

(defun make-array-copy-info (type-name-root size elemtype c-range-root c-param-decl-string)
  (let* ((copy-name (intern (format nil "copy_~a" type-name-root)))
	 (copy-header (make-array-copy-header copy-name type-name-root elemtype c-param-decl-string))
	 (copy-defn (make-array-copy-defn copy-name type-name-root size elemtype c-range-root c-param-decl-string)))
    (mk-c-defn-info copy-name copy-header copy-defn (list type-name-root) type-name-root)))

(defun make-array-update-info (type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)
  (let* ((update-name (intern (format nil "update_~a" type-name-root)))
	 (update-header (make-array-update-header update-name type-name-root elemtype c-range-root c-param-decl-string))
	 (update-defn (make-array-update-defn update-name type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info update-name update-header update-defn (list type-name-root elemtype) type-name-root)))

(defun make-array-upgrade-info (type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)
  (let* ((upgrade-name (intern (format nil "upgrade_~a" type-name-root)))
	 (upgrade-header (make-array-upgrade-header upgrade-name type-name-root elemtype c-range-root c-param-decl-string))
	 (upgrade-defn (make-array-upgrade-defn upgrade-name type-name-root elemtype c-range-root c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info upgrade-name upgrade-header upgrade-defn (list type-name-root elemtype) type-name-root)))

(defmethod ir-reference-type? ((ir-type ir-arraytype))
  t)

(defmethod ir-reference-type? ((ir-type ir-funtype));an array or closure
  t)

(defmethod ir-reference-type? ((ir-type ir-recordtype))
  t)

(defmethod ir-reference-type? ((ir-type ir-formal-typename))
  ;;NSH(12-21-20): all formals are (void *) and hence refs but they are not freed
  t)

(defmethod ir-reference-type? ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn) ir-type
    (or (eq (ir-type-id ir-type) 'file__file)
	(ir-reference-type? ir-type-defn))))

(defmethod ir-reference-type? ((ir-type t));;fallthrough case
  nil)


(defmethod ir-actualparameter-type? (ir-type);used for checking valid type actuals
  ;;parameters are automatically reference counted so we can't handle  (memq ir-type '(|mpz| |uint64|))
  (ir-reference-type? ir-type))

(defun make-release-call (ir-type c-ir-type c-expr c-param-arg-string);(break "mkrc")
;;  (when (ir-formal-typename? ir-type) (break "mkrc"))
  (if (ir-formal-typename? ir-type)
      (format nil "~a->release_ptr(~a, ~a)" c-ir-type c-expr c-ir-type)
    (if (ir-typename? ir-type)
	(if (ir-formals ir-type)
	    (if (ir-actuals ir-type)
		(break "Not implemented.")
	      (format nil "release_~a(~a~a)"  c-ir-type c-expr c-param-arg-string))
	  (format nil "release_~a(~a)"  c-ir-type c-expr))
      (format nil "release_~a(~a~a)"  c-ir-type c-expr c-param-arg-string))))

(defun make-array-release-header (release-name type-name-root c-param-decl-string)
  (format nil "void ~a(~a_t x~a);" release-name type-name-root c-param-decl-string))
;;The release operation reduces the reference count of a reference by one, and frees the object
;;(releasing any connected objects) if the count falls to 0.  
(defun make-array-release-defn (release-name type-name-root size ir-range c-range-root
					     c-param-arg-string c-param-decl-string)
  (declare (ignore size))
  (if (ir-reference-type? ir-range)
      (format nil "void ~a(~a_t x~a){~%~8Tx->count--;~%~8Tif (x->count <= 0){~%~16Tfor (int i = 0; i < x->size; i++){~a;};~%~8T//printf(\"\\nFreeing\\n\");~%~8Tsafe_free(x);}~%}"
	      release-name type-name-root c-param-decl-string 
	      (make-release-call ir-range c-range-root "x->elems[i]"  c-param-arg-string))
					;if there are nested references, these need to be released before freeing x,
    ;;otherwise, just free x.
    (format nil "void ~a(~a_t x~a){~%~8Tx->count--;~%~8T if (x->count <= 0){safe_free(x);}~%}"
	    release-name type-name-root c-param-decl-string)))

(defun make-array-release-ptr-header (release-name type-name-root c-param-decl-string)
  (declare (ignore c-param-decl-string))
  (format nil "void ~a_ptr(pointer_t x, type_actual_t ~a);" release-name  type-name-root))
;;The release operation reduces the reference count of a reference by one, and frees the object
;;(releasing any connected objects) if the count falls to 0.  

(defun make-array-release-ptr-defn (release-name type-name-root size ir-range c-range-root
						 c-param-arg-string theory-c-params-variadic
						 c-param-decl-string)
  (declare (ignore c-param-decl-string c-range-root ir-range size))
;(when (null theory-c-params-variadic) (break "null theory-c-params"))
  (format nil "void ~a_ptr(pointer_t x, type_actual_t T){~a~%~8T~a((~a_t)x~a);~%}" release-name ;type-name-root
	(if theory-c-params-variadic ;don't introduce actual if params is empty
	    (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}"
		    type-name-root type-name-root theory-c-params-variadic)
	  "")
	release-name type-name-root c-param-arg-string))

(defun make-array-copy-header (copy-name type-name-root elemtype c-param-decl-string)
  (declare (ignore elemtype c-param-decl-string))
  (format nil "~a_t ~a(~a_t x);" type-name-root copy-name type-name-root))

(defun make-array-copy-defn (copy-name type-name-root size elemtype c-range-root c-param-decl-string)
  (declare (ignore size c-param-decl-string))
  (let ((copy-instr (if (ir-reference-type? elemtype)
			(format nil "for (uint32_t i = 0; i < x->size; i++){tmp->elems[i] = x->elems[i];~%~
                                     ~16Tx->elems[i]->count++;}"
					;size
			  )
			(if (ir-formal-typename? elemtype)
			    (format nil "for (uint32_t i = 0; i < x->size; i++){~a;~%~16Tif (~a->tag == 'p') x->elems[i]->count++;}"
					;size
			      (make-c-assignment "tmp->elems[i]" c-range-root
						 "x->elems[i]" c-range-root)
			      (ir-type-id elemtype))
			  (if (mpnumber-type? elemtype)
			      (format nil "for (uint32_t i = 0; i < x->size; i++){~16T~a;~%~16T~a;~%~16T~a;}"
					;size
				      (format nil "tmp->elems[i] = (~a_ptr_t)safe_malloc(sizeof(~a_t))"
					      c-range-root c-range-root)
				      (format nil "~a_init(tmp->elems[i])" c-range-root)
				      (make-c-assignment "tmp->elems[i]" c-range-root
							 "x->elems[i]" c-range-root))
				(format nil "for (uint32_t i = 0; i < x->size; i++){~a;}"
					;size
					(make-c-assignment "tmp->elems[i]" c-range-root
						 "x->elems[i]" c-range-root)))))))
    (format nil "~a_t ~a(~a_t x){~%~8T~a_t tmp = new_~a(x->size);~%~8T~
               tmp->count = 1;~
	       ~%~8T~a;~%~8T return tmp;}"   
      type-name-root copy-name 
      type-name-root type-name-root type-name-root copy-instr)))

(defun make-array-equal-info (type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (make-array-equal-header equal-name type-name-root elemtype c-param-decl-string))
	 (equal-defn (make-array-equal-defn equal-name type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-array-json-info (type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)
  (let* ((json-name (intern (format nil "json_~a" type-name-root)))
	 (json-header (make-array-json-header json-name type-name-root elemtype c-param-decl-string))
	 (json-defn (make-array-json-defn json-name type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)))
    (mk-c-defn-info json-name json-header json-defn (list type-name-root) "char *")))

(defun make-array-json-header (json-name type-name-root elemtype c-param-decl-string)
  (declare (ignore elemtype))
  (format nil "char * ~a(~a_t x~a);" 
	  json-name type-name-root  c-param-decl-string)
  )

(defun make-array-json-defn (json-name type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)
  (declare (ignore size))
  (let* ((json-expr (if (ir-formal-typename? elemtype)
			(format nil "~a->json_ptr(x->elems[i], ~a)"
				c-range-root c-range-root)
		      (if (ir-reference-type? elemtype)
			  (format nil "json_~a(x->elems[i]~a)"
					;size
				  c-range-root c-param-arg-string)
			(format nil "json_~a(x->elems[i])"
					;size
				c-range-root))))
	 (json-instr (format nil "for (uint32_t i = 0; i < x->size; i++){tmp[i] = ~a;}"
					;size
			      json-expr)))
    (format nil "char * ~a(~a_t x~a){~%~8Tchar ** tmp = (char **)safe_malloc(sizeof(void *) * x->size);~%~8T~a;~%~8Tchar * result = json_list_with_sep(tmp, x->size, '[', ',', ']');~%~8Tfor (uint32_t i = 0; i < x->size; i++) free(tmp[i]);~%~8Tfree(tmp);~%~8Treturn result;}"   
	    json-name type-name-root 
	    c-param-decl-string
	    json-instr)
    ))

(defun make-array-json-ptr-info (type-name-root size elemtype c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string)
  (let* ((json-name (intern (format nil "json_~a" type-name-root)))
	 (json-header (make-array-json-ptr-header json-name type-name-root elemtype c-param-decl-string))
	 (json-defn (make-array-json-ptr-defn json-name type-name-root size elemtype c-range-root theory-c-params-variadic c-param-arg-string)))
    (mk-c-defn-info json-name json-header json-defn (list type-name-root type-name-root) 'bool)))

(defun make-array-json-ptr-header (json-name type-name-root elemtype c-param-decl-string)
  (declare (ignore type-name-root elemtype c-param-decl-string))
  (format nil "char * ~a_ptr(pointer_t x, type_actual_t T);" 
	  json-name)
    )

(defun make-array-json-ptr-defn (json-name type-name-root size elemtype c-range-root theory-c-params-variadic c-param-arg-string)
  (declare (ignore size elemtype c-range-root))
  (format nil "char * ~a_ptr(pointer_t x, type_actual_t T){~a~%~8Treturn ~a((~a_t)x~a);~%}" json-name
	(if theory-c-params-variadic ;don't introduce actual if params is empty
	    (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}" type-name-root type-name-root theory-c-params-variadic)
	  "")
	json-name type-name-root  c-param-arg-string)
)

(defun make-array-equal-header (equal-name type-name-root elemtype c-param-decl-string)
  (declare (ignore elemtype))
;;  (if (or (ir-formal-typename? elemtype)(ir-reference-type? elemtype))
  (format nil "bool_t ~a(~a_t x, ~a_t y~a);" 
	  equal-name type-name-root type-name-root c-param-decl-string)
    ;; (format nil "bool_t ~a(~a_t x, ~a_t y);" 
    ;; 	      equal-name type-name-root type-name-root)
  )

(defun make-array-equal-defn (equal-name type-name-root size elemtype c-range-root c-param-decl-string c-param-arg-string)
  (declare (ignore size))
  (let ((equal-instr (if (ir-formal-typename? elemtype)
			     (format nil "while (i < x->size && tmp){tmp = ~a->equal_ptr(x->elems[i], y->elems[i], ~a); i++;}"
					;size
				     c-range-root c-range-root)
		       (if (ir-reference-type? elemtype)
			   (format nil "while (i < x->size && tmp){tmp = equal_~a(x->elems[i], y->elems[i]~a); i++;}"
					;size
				   c-range-root c-param-arg-string)
			 (if (eq elemtype '|mpz|)
			     (format nil "tmp = (mpz_cmp(x->elems[i], y->elems[i]) == 0)")
			   (if (eq elemtype '|mpq|)
			       (format nil "tmp = (mpq_cmp(x->elems[i], y->elems[i]) == 0)")
			     (format nil "while (i < x->size && tmp){tmp = (x->elems[i] == y->elems[i]); i++;}"
					;size
				     )))))))
;;    (if (or (ir-formal-typename? elemtype)(ir-reference-type? elemtype))
    (format nil "bool_t ~a(~a_t x, ~a_t y~a){~%~8Tbool_t tmp = true;~%~8Tuint32_t i = 0;~
	       ~%~8T~a;~%~8Treturn tmp;}"   
	    equal-name type-name-root type-name-root
	    c-param-decl-string
	    equal-instr)
	;; (format nil "bool_t ~a(~a_t x, ~a_t y){~%~8Tbool_t tmp = true;~%~8Tuint32_t i = 0;~
	;;        ~%~8T~a;~%~8Treturn tmp;}"   
	;; 	equal-name type-name-root type-name-root equal-instr)
	))


(defun make-array-equal-ptr-info (type-name-root size elemtype c-range-root theory-c-params-variadic c-param-decl-string c-param-arg-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (make-array-equal-ptr-header equal-name type-name-root elemtype c-param-decl-string))
	 (equal-defn (make-array-equal-ptr-defn equal-name type-name-root size elemtype c-range-root theory-c-params-variadic c-param-arg-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-array-equal-ptr-header (equal-name type-name-root elemtype c-param-decl-string)
  (declare (ignore type-name-root elemtype c-param-decl-string))
;;  (if (or (ir-formal-typename? elemtype)(ir-reference-type? elemtype))  
  (format nil "bool_t ~a_ptr(pointer_t x, pointer_t y, type_actual_t T);" 
	  equal-name)
    ;; (format nil bool_t ~a_ptr(pointer_t x, pointer_t y);" 
    ;; 	      equal-name)
    )

(defun make-array-equal-ptr-defn (equal-name type-name-root size elemtype c-range-root theory-c-params-variadic c-param-arg-string)
  (declare (ignore size elemtype c-range-root))
   ;;  (if (or (ir-formal-typename? elemtype)(ir-reference-type? elemtype)))
(format nil "bool_t ~a_ptr(pointer_t x, pointer_t y, type_actual_t T){~a~%~8Treturn ~a((~a_t)x, (~a_t)y~a);~%}" equal-name
	(if theory-c-params-variadic ;don't introduce actual if params is empty
	    (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}" type-name-root type-name-root theory-c-params-variadic)
	  "")
	equal-name type-name-root type-name-root c-param-arg-string)
    ;; (format nil  "bool_t ~a_ptr(pointer_t x, pointer_t y){return ~a((~a_t)x, (~a_t)y);~%}" equal-name
    ;; 	      equal-name type-name-root type-name-root)
)

(defun make-array-update-header (update-name type-name-root ir-range c-range-root c-param-decl-string)
  (declare (ignore ir-range))
  (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a);" type-name-root update-name  type-name-root c-range-root c-param-decl-string))

(defun make-array-update-defn (update-name type-name-root ir-range c-range-root c-param-arg-string c-param-decl-string)
  (if (ir-reference-type? ir-range) ;;NSH(2/6/20):update is only invoked on last-marked array variable
      (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T ~a_t y;~%~8T if (x->count == 1){y = x;}~%~16T else {y = copy_~a(x);~%~22Tx->count--;};~%~8T~
                     ~a_t * yelems = y->elems;~%~8Tif (v != NULL){v->count++;}~%~8T~
                     if (yelems[i] != NULL){~a;};~%~8T yelems[i] = v;~%~8T return y;}"
	      type-name-root update-name type-name-root c-range-root c-param-decl-string

	      type-name-root type-name-root 
	      c-range-root
	      (make-release-call ir-range c-range-root "yelems[i]" c-param-arg-string)
	      )
      (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T~a_t y; ~%~8T if (x->count == 1){y = x;}~%~10T else {y = copy_~a(x );~%~16Tx->count--;};~%~8T~
                    ~a;~%~8T~
                    return y;}"
	      type-name-root update-name type-name-root c-range-root c-param-decl-string type-name-root type-name-root
	      (let ((lhs-c-expr "y->elems[i]"))
		(make-c-assignment lhs-c-expr c-range-root "v" c-range-root)))))

(defun make-array-upgrade-header (upgrade-name type-name-root ir-range c-range-root c-param-decl-string)
  (declare (ignore ir-range))
  (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a);" type-name-root upgrade-name  type-name-root c-range-root c-param-decl-string))

(defun make-array-upgrade-defn (upgrade-name type-name-root ir-range c-range-root c-param-arg-string c-param-decl-string)
  (if (ir-reference-type? ir-range) ;;NSH(2/6/20):upgrade is only invoked on last-marked array variable
      (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T ~a_t y;~%~8T if (x->count == 1 && i < x->max){y = x;}~%~16T else if (i > x->max){uint32_t newmax = x->max <= UINT32_MAX/2 ? 2*x->max: UINT32_MAX;~%~16Ty = safe_realloc(x, sizeof(struct ~a_s) + (newmax * sizeof(~a_t)));~%~16Ty->count = 1;~%~16Ty->size = i+1;~%~16Ty->max = newmax;~%~16Trelease_~a(x~a);} else {y = copy_~a(x);~%~22Tx->count--;};~%~8T~
                     ~a_t * yelems = y->elems;~%~8Tif (v != NULL){v->count++;}~%~8T~
                     if (yelems[i] != NULL){~a;};~%~8T yelems[i] = v;~%~8T return y;}"
	      type-name-root upgrade-name type-name-root c-range-root c-param-decl-string
	      type-name-root
	      type-name-root c-range-root type-name-root c-param-arg-string;i > x->max case
	      type-name-root ; copy
	      c-range-root
	      (make-release-call ir-range c-range-root "yelems[i]" c-param-arg-string)
	      )
      (format nil "~a_t ~a(~a_t x, uint32_t i, ~a_t v~a){~%~8T~a_t y; ~%~8T if (x->count == 1 && i < x->max){y = x;}~%~10T else if (i > x->max){uint32_t newmax = x->max <= UINT32_MAX/2 ? 2*x->max: UINT32_MAX;~%~16Ty = safe_realloc(x, sizeof(struct ~a_s) + (newmax * sizeof(~a_t)));~%~16Ty->count = 1;~%~16Ty->size = i+1;~%~16Ty->max = newmax;~%~16Trelease_~a(x~a);}~%~10T else {y = copy_~a(x );~%~16Tx->count--;};~%~8T~
                    ~a;~%~8T~
                    return y;}"
	      type-name-root upgrade-name type-name-root c-range-root c-param-decl-string type-name-root
	      type-name-root c-range-root type-name-root ;i > x->max case
	      c-param-arg-string;release argument
	      type-name-root
	      (make-c-assignment "y->elems[i]" c-range-root "v" c-range-root))))

(defmethod add-c-type-definition ((ir2c-type ir-recordtype) &optional tname)
					;(break "add-c-type-definition(ir-recordtype)")
  (with-slots (ir-field-types ir-label) ir2c-type
    (let* ((c-type-info (get-c-type-info ir2c-type))
	   (c-type-name (when c-type-info (tname c-type-info))))
      (or (and (or (null tname)(eq c-type-name tname)) c-type-name)
	  (let* ((c-field-types (loop for ft in ir-field-types
				      collect (add-c-type-definition (ir-vtype ft))))
		 ;; (c-field-type-params (loop for ft in ir-field-types
		 ;; 			      collect (pvs2ir-type-parameters ft)))
		 (type-name-root (or tname (format nil "~a_record_~a"
					     *theory-id*
					     (length *c-type-info-table*))))
		 (c-field-decls (loop for cft in c-field-types
				      as ft in ir-field-types
				      collect (format nil "~a_t ~a" cft (ir-id ft))))
		 (type-defn (format nil "struct ~a_s {~%~8Tuint32_t count; ~{~%~8T~a;~}};~%typedef struct ~a_s * ~a_t;"
			      type-name-root c-field-decls type-name-root type-name-root))) ;(break "add-c-type-definition")
	    (let* ((theory-params *ir-theory-formals*)
		   (theory-formals *theory-formals*)
		   (theory-c-params (ir2c-theory-formals theory-params theory-formals))
		   (theory-c-params-variadic (ir2c-theory-formals-variadic "actual" theory-params theory-formals))
		   (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
		   (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
								    theory-params
								    collect (ir-name ir-formal))))
		   (new-info (make-record-new-info type-name-root))
		   (release-info (make-record-release-info type-name-root ir-field-types c-field-types
							   c-param-arg-string 
							   c-param-decl-string))
		   (release-ptr-info (make-record-release-ptr-info type-name-root 
								   c-param-arg-string theory-c-params-variadic
								   c-param-decl-string))
		   (copy-info (make-record-copy-info type-name-root ir-field-types c-field-types))
		   (equal-info (make-record-equal-info type-name-root ir-field-types c-field-types
						       c-param-arg-string
						       c-param-decl-string))
		   (equal-ptr-info (make-record-equal-ptr-info type-name-root 
							       theory-c-params-variadic 
							       c-param-arg-string))
		   (json-info (make-record-json-info type-name-root ir-field-types c-field-types
						     c-param-arg-string
						     c-param-decl-string))
		   (json-ptr-info (make-record-json-ptr-info type-name-root 
							     theory-c-params-variadic 
							     c-param-arg-string))
		   (update-info (loop for cft in c-field-types
				      as ft in ir-field-types
				      collect (make-record-field-update-info type-name-root ft cft
									     c-param-arg-string
									     c-param-decl-string)))
		   (actual-info (make-actual-info type-name-root theory-params c-param-decl-string))
		   (decl-info (format nil "~a" (id *pvs2c-current-decl*))))
					;(when (get-c-type-info ir2c-type) (break "add-c- ir-recordtype"))
	      (push-type-info-to-decl
	       (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info release-ptr-info copy-info equal-info equal-ptr-info json-info json-ptr-info update-info decl-info)
	       *pvs2c-current-decl*)
	      type-name-root))))))

(defmethod add-c-type-definition ((ir2c-type ir-adt-recordtype) &optional tname)
;  (break "add-c-type-defn adt")
  (with-slots (ir-field-types ir-constructors) ir2c-type
	      (let* ((c-type-info (get-c-type-info ir2c-type))
		     (c-type-name (when c-type-info (tname c-type-info))))
		(or c-type-name
		    (let* ((c-field-types (loop for ft in ir-field-types
					       collect (add-c-type-definition (ir-vtype ft))))
			   (type-name-root (or tname (format nil "record_~a" (length *c-type-info-table*))))
			   (c-field-decls (loop for cft in c-field-types
						as ft in ir-field-types
						collect (format nil "~a_t ~a" cft (ir-id ft))))
			   (type-defn (format nil "struct ~a_s {~%~8T uint32_t count; ~{~%~8T~a;~}};~%typedef struct ~a_s * ~a_t;"
					      type-name-root c-field-decls type-name-root type-name-root)));(break "add-c-type-definition")
		      (let* ((theory-params *ir-theory-formals*)
			     (theory-formals *theory-formals*)
			     (theory-c-params (ir2c-theory-formals theory-params theory-formals))
			     (theory-c-params-variadic (ir2c-theory-formals-variadic "actual" theory-params theory-formals))
			     (c-param-decl-string (format nil "~{, ~a~}" theory-c-params))
			     (c-param-arg-string (format nil "~{, ~a~}" (loop for ir-formal in
									      theory-params
									      collect (ir-name ir-formal))))
			     (new-info (make-record-new-info type-name-root))
			     (release-info (make-adt-record-release-info type-name-root
									 ir-field-types c-field-types
									 ir-constructors
									 c-param-arg-string 
									 c-param-decl-string))
			     (release-ptr-info (make-record-release-ptr-info type-name-root c-param-arg-string theory-c-params-variadic c-param-decl-string))
			     (copy-info (make-record-copy-info type-name-root ir-field-types c-field-types))
			     (equal-info (make-adt-record-equal-info type-name-root ir-field-types c-field-types ir-constructors
								     c-param-arg-string c-param-decl-string))
			     (equal-ptr-info (make-record-equal-ptr-info type-name-root theory-c-params-variadic c-param-arg-string));reusing the record-equal-ptr defn
			     (json-info (make-adt-record-json-info type-name-root ir-field-types c-field-types ir-constructors
								     c-param-arg-string c-param-decl-string))
			     (json-ptr-info (make-record-json-ptr-info type-name-root theory-c-params-variadic c-param-arg-string));reusing the record-equal-ptr defn
			     (update-info (loop for cft in c-field-types
						as ft in ir-field-types
						collect (make-record-field-update-info type-name-root ft cft
										       c-param-arg-string
										       c-param-decl-string)))
			     (actual-info (make-actual-info type-name-root theory-params c-param-decl-string))
			     (decl-info (format nil "~a" (id *pvs2c-current-decl*))))
			(push-type-info-to-decl (mk-c-type-info ir2c-type type-name-root type-defn actual-info new-info release-info release-ptr-info copy-info equal-info equal-ptr-info json-info json-ptr-info update-info decl-info)
						    *pvs2c-current-decl*)
			    type-name-root))))))



(defun make-record-new-info (type-name-root)
    (let* ((new-name (intern (format nil "new_~a" type-name-root)))
	   (new-header (format nil "~a_t new_~a(void);" type-name-root type-name-root))
	   (new-defn   (format nil "~a_t new_~a(void){~%~8T~a_t tmp = (~a_t) safe_malloc(sizeof(struct ~a_s));~%~8Ttmp->count = 1;~%~8Treturn tmp;}"
			       type-name-root type-name-root type-name-root type-name-root type-name-root)))
      (mk-c-defn-info new-name new-header new-defn nil type-name-root)))

(defun make-adt-record-release-info (type-name-root ir-field-types c-field-types constructors
						    c-param-arg-string  c-param-decl-string)
  (declare (ignore ir-field-types c-field-types))
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (format nil "void release_~a(~a_t x~a);" type-name-root type-name-root c-param-decl-string))
	 (release-defn (let ((release-fields (loop for constructor in constructors
						   as index from 0
						   when (cdr constructor)
						   collect (format nil "case ~a:  release_~a((~a_t) x~a); break"
								   index
								   (car constructor)
								   (car constructor)
								   c-param-arg-string))))
						   
			 (format nil "void release_~a(~a_t x~a){~%switch (x->~a_index) {~{~%~a;~}~%}}"
				 type-name-root type-name-root c-param-decl-string type-name-root release-fields))))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-record-release-ptr-info (type-name-root c-param-arg-string theory-c-params-variadic c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)));;same name as release, _ptr added below
	 (release-ptr-header (make-record-release-ptr-header release-name type-name-root c-param-decl-string))
	 (release-ptr-defn (make-record-release-ptr-defn release-name type-name-root  c-param-arg-string theory-c-params-variadic)))
    (mk-c-defn-info release-name release-ptr-header release-ptr-defn (list type-name-root) 'void)))

(defun make-record-release-ptr-header (release-name type-name-root c-param-decl-string)
  (declare (ignore c-param-decl-string))
  (format nil "void ~a_ptr(pointer_t x, type_actual_t ~a);" release-name type-name-root))

(defun make-record-release-ptr-defn (release-name type-name-root 
						 c-param-arg-string theory-c-params-variadic)
  (format nil "void ~a_ptr(pointer_t x, type_actual_t T){~a~%~8T~a((~a_t)x~a);~%}" release-name
	(if theory-c-params-variadic ;don't introduce actual if params is empty
	    (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}"
		    type-name-root type-name-root theory-c-params-variadic)
	  "")
	release-name type-name-root c-param-arg-string))

(defun make-record-release-info (type-name-root ir-field-types c-field-types c-param-arg-string c-param-decl-string)
  (let* ((release-name (intern (format nil "release_~a" type-name-root)))
	 (release-header (format nil "void release_~a(~a_t x~a);" type-name-root type-name-root c-param-decl-string))
	 (release-defn (let ((release-fields (loop for ft in ir-field-types
						   as cft in c-field-types
						   when (ir-reference-type? (ir-vtype ft))
						   collect (make-release-call (ir-vtype ft) cft
									      (format nil "x->~a" (ir-id ft))
									      c-param-arg-string))))
						   
			 (format nil "void release_~a(~a_t x~a){~%~8Tx->count--;~%~8Tif (x->count <= 0){~{~%~8T~8T~a;~}~%~8T//printf(\"\\nFreeing\\n\");~a~%~8Tsafe_free(x);}}"
				 type-name-root type-name-root c-param-decl-string release-fields
				 (if (file-type-name? type-name-root)
				     (format nil "~%~8T~8Tmunmap(x->contents, x->capacity);~%~8T~8Tclose(x->fd);")
				   (format nil ""))))))
    (mk-c-defn-info release-name release-header release-defn (list type-name-root) 'void)))

(defun make-record-copy-info (type-name-root ir-field-types c-field-types)
  (let* ((copy-name (intern (format nil "copy_~a" type-name-root)))
	 (copy-header (format nil "~a_t ~a(~a_t x);" type-name-root copy-name type-name-root))
	 (copy-defn (make-record-copy-defn type-name-root ir-field-types c-field-types)))
    (mk-c-defn-info copy-name copy-header copy-defn (list type-name-root) type-name-root)))

(defun file-type-name?(type-name)
  (eq type-name 'file__file))

(defun make-record-copy-defn (type-name-root ir-field-types c-field-types)
  (if (file-type-name? type-name-root)
      (format nil "~a_t copy_~a(~a_t x){pvs2cerror(~s, ~a);}" type-name-root type-name-root type-name-root "Copying of files not allowed." "PVS2C_EXIT_ERROR")
    (let ((copy-field-instrs
	   (loop for ft in ir-field-types
		 as cft in c-field-types
		 collect (if (ir-reference-type? (ir-vtype ft))
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
	      type-name-root type-name-root type-name-root type-name-root type-name-root copy-field-instrs))))

(defun make-adt-record-equal-info (type-name-root ir-field-types c-field-types constructors
						  c-param-arg-string  c-param-decl-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (format nil "bool_t ~a(~a_t x, ~a_t y~a);" equal-name
			      type-name-root type-name-root c-param-decl-string))
	 (equal-defn (make-adt-record-equal-defn type-name-root ir-field-types
						 c-field-types constructors c-param-arg-string  c-param-decl-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-adt-record-json-info (type-name-root ir-field-types c-field-types constructors
						  c-param-arg-string  c-param-decl-string)
  (let* ((json-name (intern (format nil "json_~a" type-name-root)))
	 (json-header (format nil "char * ~a(~a_t x~a);" json-name
			      type-name-root c-param-decl-string))
	 (json-defn (make-adt-record-json-defn type-name-root ir-field-types
						 c-field-types constructors c-param-arg-string  c-param-decl-string)))
    (mk-c-defn-info json-name json-header json-defn (list type-name-root type-name-root) 'bool)))

(defun make-adt-record-equal-defn (type-name-root ir-field-types c-field-types constructors
						  c-param-arg-string  c-param-decl-string)
  (declare (ignore ir-field-types c-field-types))
  (let ((equal-constructor-instrs (loop for constructor in constructors
					as index from 0
					when (cdr constructor)
					collect
					(format nil "case ~a: tmp = tmp && equal_~a((~a_t) x, (~a_t) y~a); break"
						index (car constructor)(car constructor)
						(car constructor)
						c-param-arg-string))))
    (format nil "bool_t equal_~a(~a_t x, ~a_t y~a){~%~8Tbool_t tmp = x->~a_index == y->~a_index;~%~8Tswitch  (x->~a_index) {~{~%~16T~a;~}~%~8T}~%~8Treturn tmp;~%}" type-name-root type-name-root type-name-root c-param-decl-string
	    type-name-root type-name-root type-name-root equal-constructor-instrs)))

(defun make-adt-record-json-defn (type-name-root ir-field-types c-field-types constructors
						  c-param-arg-string  c-param-decl-string)
  (declare (ignore ir-field-types c-field-types))
  (let ((json-constructor-instrs (loop for constructor in constructors
					as index from 0
					when (cdr constructor)
					collect
					(format nil "case ~a: tmp = safe_strcat(tmp, json_~a((~a_t) x~a)); break"
						index (car constructor)(car constructor)
						c-param-arg-string))))
    (format nil "char * json_~a(~a_t x~a){~%~8Tchar * tmp = safe_malloc(24); sprintf(tmp, \"{ \\\"constructor\\\" = %u,\", x->~a_index);~%~8Tswitch  (x->~a_index) {~{~%~16T~a;~}~%~8T};~%~8Ttmp = safe_strcat(tmp, \" }\");~%~8Treturn tmp;~%}" type-name-root type-name-root c-param-decl-string
	    type-name-root type-name-root json-constructor-instrs)))

(defun make-record-equal-ptr-info (type-name-root theory-c-params-variadic c-param-arg-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (make-record-equal-ptr-header equal-name type-name-root))
	 (equal-defn (make-record-equal-ptr-defn equal-name type-name-root theory-c-params-variadic c-param-arg-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-record-json-ptr-info (type-name-root theory-c-params-variadic c-param-arg-string)
  (let* ((json-name (intern (format nil "json_~a" type-name-root)))
	 (json-header (make-record-json-ptr-header json-name type-name-root))
	 (json-defn (make-record-json-ptr-defn json-name type-name-root theory-c-params-variadic c-param-arg-string)))
    (mk-c-defn-info json-name json-header json-defn (list type-name-root type-name-root) "char *")))

(defun make-record-equal-ptr-header (equal-name type-name-root)
  (format nil "bool_t ~a_ptr(pointer_t x, pointer_t y, actual_~a_t T);" 
	  equal-name type-name-root))

(defun make-record-json-ptr-header (json-name type-name-root)
  (format nil "char * ~a_ptr(pointer_t x,  actual_~a_t T);" 
	  json-name type-name-root))

(defun make-record-equal-ptr-defn (equal-name type-name-root  theory-c-params-variadic c-param-arg-string)
  (format nil "bool_t ~a_ptr(pointer_t x, pointer_t y, actual_~a_t T){~a~%~8Treturn ~a((~a_t)x, (~a_t)y~a);~%}" equal-name type-name-root
	  (if theory-c-params-variadic ;don't introduce actual if params is empty
	      (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}" type-name-root type-name-root theory-c-params-variadic)
	    "")
	  equal-name type-name-root type-name-root c-param-arg-string))

(defun make-record-json-ptr-defn (json-name type-name-root  theory-c-params-variadic c-param-arg-string)
  (format nil "char * ~a_ptr(pointer_t x, actual_~a_t T){~a~%~8Treturn ~a((~a_t)x~a);~%}" json-name type-name-root
	  (if theory-c-params-variadic ;don't introduce actual if params is empty
	      (format nil "~%~8Tactual_~a_t actual = (actual_~a_t)T;~{~%~8T~a;~}" type-name-root type-name-root theory-c-params-variadic)
	    "")
	  json-name type-name-root c-param-arg-string))

(defun make-record-json-info (type-name-root ir-field-types c-field-types c-param-arg-string c-param-decl-string)
    (let* ((json-name (intern (format nil "json_~a" type-name-root)))
	   (json-header (format nil "char * ~a(~a_t x~a);" json-name
				type-name-root  c-param-decl-string))
	   (json-defn (make-record-json-defn type-name-root ir-field-types c-field-types c-param-arg-string
					     c-param-decl-string)))
      (mk-c-defn-info json-name json-header json-defn (list type-name-root type-name-root) "char *")))

(defun make-record-equal-info (type-name-root ir-field-types c-field-types c-param-arg-string c-param-decl-string)
  (let* ((equal-name (intern (format nil "equal_~a" type-name-root)))
	 (equal-header (format nil "bool_t ~a(~a_t x, ~a_t y~a);" equal-name
			      type-name-root type-name-root c-param-decl-string))
	 (equal-defn (make-record-equal-defn type-name-root ir-field-types c-field-types c-param-arg-string
					     c-param-decl-string)))
    (mk-c-defn-info equal-name equal-header equal-defn (list type-name-root type-name-root) 'bool)))

(defun make-record-equal-defn (type-name-root ir-field-types c-field-types c-param-arg-string c-param-decl-string)
  (let ((equal-field-instrs
	 (loop for ft in ir-field-types
	       as cft in c-field-types
	       collect (if (ir-formal-typename? (ir-vtype ft))
			   (format nil "~%~8Ttmp = tmp && ~a->equal_ptr(x->~a, y->~a, ~a)"
				   cft
				   (ir-id ft)
				   (ir-id ft)
				   cft)
			 (if (ir-reference-type? (ir-vtype ft))
			     (let ((ft-vtype (ir-vtype ft)))
			       (if (and (ir-typename? ft-vtype)
					(ir-formals ft-vtype)
					(ir-actuals ft-vtype))
				   (format nil "pvs2cerror(~s, ~a)" "Parametric datatypes not handled" 0)
				 (format nil "~%~8Ttmp = tmp && equal_~a(x->~a, y->~a~a)"
					 cft
					 (ir-id ft)
					 (ir-id ft)
					 c-param-arg-string)))
			   (if (eq (ir-vtype ft) '|mpz|)
			       (format nil "tmp = tmp && (mpz_cmp(x->~a, y->~a) == 0)" (ir-id ft) (ir-id ft))
			     (if (eq (ir-vtype ft) '|mpq|)
				 (format nil "tmp = tmp && (mpq_cmp(x->~a, y->~a) == 0)" (ir-id ft) (ir-id ft))
			       (format nil "~%~8Ttmp = tmp && x->~a == y->~a"
				       (ir-id ft)  (ir-id ft)))))))))
    (format nil "bool_t equal_~a(~a_t x, ~a_t y~a){~%~8Tbool_t tmp = true;~{~a;~}~%~8Treturn tmp;}"
	    type-name-root type-name-root type-name-root  c-param-decl-string equal-field-instrs)))

(defun make-record-json-defn (type-name-root ir-field-types c-field-types c-param-arg-string c-param-decl-string)
  (let ((len (length ir-field-types))
	(json-field-instrs
	 (loop for ft in ir-field-types
	       as cft in c-field-types
	       as index from 0
	       collect (let* ((stringft (string (ir-id ft)))
			      (lengthft (+ (length stringft) 12))
			      (fldvar (format nil "fld~a" index))
			      (stmt (format nil "~%~8Tchar * ~a = safe_malloc(~a);~%~8T sprintf(~a, \"\\\"~a\\\" : \")" fldvar lengthft fldvar stringft)))
			 (if (ir-formal-typename? (ir-vtype ft))
			     (format nil "~a;~%~8Ttmp[~a] = safe_strcat(~a, ~a->json_ptr(x->~a, ~a))"
				     stmt
				     index
				     fldvar
				   cft
				   (ir-id ft)
				   cft)
			   (if (ir-reference-type? (ir-vtype ft))
			       (format nil "~a;~%~8Ttmp[~a] = safe_strcat(~a, json_~a(x->~a~a))"
				       stmt
				       index
				       fldvar
				       cft
				       (ir-id ft)
				       (if (and (ir-typename? (ir-vtype ft))
						(ir-formals (ir-vtype ft))
						(ir-actuals (ir-vtype ft)))
					   "";;Need to deal with actuals
					 c-param-arg-string))
			     (format nil "~a;~%~8Ttmp[~a] = safe_strcat(~a, json_~a(x->~a))"
				     stmt index fldvar cft (ir-id ft))))))))
    (format nil "char * json_~a(~a_t x~a){~%~8Tchar * tmp[~a]; ~{~a;~}~%~8T char * result = json_list_with_sep(tmp, ~a,  \'{\', \',\', \'}\');~%~8T for (uint32_t i = 0; i < ~a; i++) free(tmp[i]);~%~8Treturn result;}"
	     type-name-root type-name-root c-param-decl-string len  json-field-instrs len len)))


(defun make-record-field-update-info (type-name-root ir-field-type c-field-type
						     c-param-arg-string c-param-decl-string)
  (let* ((update-name (intern (format nil "update_~a_~a" type-name-root (ir-id ir-field-type))))
	 (ftype (ir-vtype ir-field-type))
	 (update-header (if (ir-reference-type? ftype)
			    (format nil "~a_t ~a(~a_t x, ~a_t v~a);" type-name-root update-name type-name-root (mppointer-type c-field-type) c-param-decl-string)
			  (format nil "~a_t ~a(~a_t x, ~a_t v);" type-name-root update-name type-name-root (mppointer-type c-field-type))))
	 (update-defn (make-record-field-update-defn update-name type-name-root ir-field-type c-field-type
						     c-param-arg-string c-param-decl-string)))
    (mk-c-defn-info update-name update-header update-defn (list type-name-root c-field-type) type-name-root)))

;;Not doing anything special for file__file since copy will crash the program if the refcount > 1. 
(defun make-record-field-update-defn (update-name type-name-root ir-field-type c-field-type
						  c-param-arg-string c-param-decl-string)
  (let ((fname (ir-id ir-field-type))
	(ftype (ir-vtype ir-field-type))
	(mpcft (mppointer-type c-field-type)))
    (if (ir-reference-type? ftype)
	(format nil "~a_t ~a(~a_t x, ~a_t v~a){~%~8T~a_t y;~%~8Tif (v != NULL){v->count++;};~
                     ~%~8Tif (x->count == 1){y = x; if (x->~a != NULL){~a;};}~%~8T~
                     else {y = copy_~a(x); x->count--; y->~a->count--;};~%~8T~
                     ~a;~%~8Treturn y;}"
		type-name-root update-name type-name-root mpcft
		c-param-decl-string
		type-name-root 
		fname ;c-field-type
		(make-release-call ftype c-field-type (format nil "x->~a" fname) c-param-arg-string)
		;fname
		;c-param-arg-string
		type-name-root fname (make-c-assignment (format nil "y->~a"fname)
							      c-field-type
							       "v" c-field-type))
      (format nil "~a_t ~a(~a_t x, ~a_t v){~%~8T~a_t y;~%~8Tif (x->count == 1){y = x;}~%~8T~
                     else {y = copy_~a(x); x->count--;};~%~
                     ~8T~a;~%~8Treturn y;}"
		type-name-root update-name type-name-root mpcft type-name-root 
		type-name-root
		(let ((lhs-c-expr (format nil "y->~a" fname)))
		  (make-c-assignment lhs-c-expr
				     c-field-type
				     "v" c-field-type))))))

;;equality method for ir-types; treats adt and constructor types as tequal since these have been typechecked
(defun ir2c-tequal (texpr1 texpr2)
  ;(format t "~%ir2c-tequal")
  ;(format t "~%texpr1: ~a" (print-ir texpr1))
   ; (format t "~%texpr2: ~a" (print-ir texpr2))
  (ir2c-tequal* texpr1 texpr2))

(defmethod ir2c-tequal* ((texpr1 ir-recordtype)(texpr2 ir-recordtype))
  (with-slots ((ir-ftypes1 ir-field-types)) texpr1
	      (with-slots ((ir-ftypes2 ir-field-types)) texpr2
			  (ir2c-tequal* ir-ftypes1 ir-ftypes2))))

(defmethod ir2c-tequal* ((texpr1 ir-adt-constructor-recordtype)(texpr2 ir-adt-constructor-recordtype))
  (with-slots ((constructor-id1 constructor-id)(ir-ftypes1 ir-field-types)) texpr1
    (with-slots ((constructor-id2 constructor-id)(ir-ftypes2 ir-field-types)) texpr2
      (and (equal constructor-id1 constructor-id2)
	   (ir2c-tequal* ir-ftypes1 ir-ftypes2)))))

(defmethod ir2c-tequal* ((texpr1 list)(texpr2 list))
  (cond ((consp texpr1)
	 (and (consp texpr2)
	      (ir2c-tequal* (car texpr1)(car texpr2))
	      (ir2c-tequal* (cdr texpr1)(cdr texpr2))))
	(t (not (consp texpr2)))))

(defmethod ir2c-tequal* ((texpr1 ir-fieldtype)(texpr2 ir-fieldtype))
  (with-slots ((ir-id1 ir-id)(ir-ftype1 ir-vtype)) texpr1
	      (with-slots ((ir-id2 ir-id)(ir-ftype2 ir-vtype)) texpr2
			  (and (eq ir-id1 ir-id2)
			       (ir2c-tequal* ir-ftype1 ir-ftype2)))))

(defmethod ir2c-tequal* ((texpr1 ir-funtype)(texpr2 ir-funtype))
  (with-slots ((ir-dom1 ir-domain)(ir-ran1 ir-range)) texpr1
	      (with-slots ((ir-dom2 ir-domain)(ir-ran2 ir-range)) texpr2
			  (and (ir2c-tequal* ir-dom1 ir-dom2)
			       (ir2c-tequal* ir-ran1 ir-ran2)))))

(defmethod ir2c-tequal* ((texpr1 ir-arraytype)(texpr2 ir-arraytype))
  (with-slots ((size1 size)(elemtype1 ir-range)) texpr1
	      (with-slots ((size2 size)(elemtype2 ir-range)) texpr2
	;		  (and (eql size1 size2);NSF:4/19: size is irrelevant
		(ir2c-tequal* elemtype1 elemtype2))))

(defmethod ir2c-tequal* ((texpr1 ir-subrange )(texpr2 ir-subrange))
  (equal (ir2c-type texpr1)(ir2c-type texpr2)))

(defmethod ir2c-tequal* ((texpr1 ir-typename)(texpr2 ir-typename))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)(act1 ir-actuals)) texpr1
	      (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)(act2 ir-actuals)) texpr2
		(and (eq id1 id2)
		     (ir2c-tequal* tdef1 tdef2)
		     (ir2c-tequal* act1 act2)))));;actuals fall through so they need to be eq.

(defmethod ir2c-tequal* ((texpr1 ir-formal-typename)(texpr2 ir-formal-typename))
  (with-slots ((id1 ir-type-id)) texpr1
    (with-slots ((id2 ir-type-id)) texpr2
      (eq id1 id2))))



(defmethod ir2c-tequal* ((texpr1 ir-typename)(texpr2 t))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)) texpr1
	      (ir2c-tequal* tdef1 texpr2)))

(defmethod ir2c-tequal* ((texpr1 t)(texpr2 ir-typename))
  (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)) texpr2
	      (ir2c-tequal* texpr1 tdef2)))

(defmethod ir2c-tequal* ((texpr1 ir-chartype)(texpr2 ir-chartype))
  t)

(defmethod ir2c-tequal* ((texpr1 ir-stringtype)(texpr2 ir-stringtype))
  t)

(defmethod ir2c-tequal* ((texpr1 t)(texpr2 t))
  (eq texpr1 texpr2));;Since the base case

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(2/11/20): type are tcompatible when a value of one can be directly assigned to the other. 
;;equality method for ir-types; treats adt and constructor types as tequal since these have been typechecked
(defun ir2c-tcompatible (texpr1 texpr2)
  ;(format t "~%ir2c-tcompatible")
  ;(format t "~%texpr1: ~a" (print-ir texpr1))
   ; (format t "~%texpr2: ~a" (print-ir texpr2))
  (ir2c-tcompatible* texpr1 texpr2))

(defmethod ir2c-tcompatible* ((texpr1 ir-recordtype)(texpr2 ir-recordtype))
  (if (or (ir-adt-constructor-recordtype? texpr1)
	  (ir-adt-constructor-recordtype? texpr2))
      (call-next-method)
    (with-slots ((ir-ftypes1 ir-field-types)) texpr1
      (with-slots ((ir-ftypes2 ir-field-types)) texpr2
	(ir2c-tcompatible* ir-ftypes1 ir-ftypes2)))))

;;NSH(2/7/20): 
(defmethod ir2c-tcompatible* ((texpr1 ir-adt-constructor-recordtype)(texpr2 t))
  (with-slots ((ir-tname ir-adt-name)) texpr1
    (ir2c-tcompatible* (ir2c-type ir-tname) texpr2)))

(defmethod ir2c-tcompatible* ((texpr1 t)(texpr2 ir-adt-constructor-recordtype))
  (with-slots ((ir-tname ir-adt-name)) texpr2 
    (ir2c-tcompatible* texpr1 (ir2c-type ir-tname))))

(defmethod ir2c-tcompatible* ((texpr1 list)(texpr2 list))
  (cond ((consp texpr1)
	 (and (consp texpr2)
	      (ir2c-tcompatible* (car texpr1)(car texpr2))
	      (ir2c-tcompatible* (cdr texpr1)(cdr texpr2))))
	(t (not (consp texpr2)))))

(defmethod ir2c-tcompatible* ((texpr1 ir-fieldtype)(texpr2 ir-fieldtype))
  (with-slots ((ir-id1 ir-id)(ir-ftype1 ir-vtype)) texpr1
	      (with-slots ((ir-id2 ir-id)(ir-ftype2 ir-vtype)) texpr2
			  (and (eq ir-id1 ir-id2)
			       (ir2c-tcompatible* ir-ftype1 ir-ftype2)))))

(defmethod ir2c-tcompatible* ((texpr1 ir-funtype)(texpr2 ir-funtype))
  (with-slots ((ir-dom1 ir-domain)(ir-ran1 ir-range)) texpr1
	      (with-slots ((ir-dom2 ir-domain)(ir-ran2 ir-range)) texpr2
			  (and (ir2c-tcompatible* ir-dom1 ir-dom2)
			       (ir2c-tcompatible* ir-ran1 ir-ran2)))))

(defmethod ir2c-tcompatible* ((texpr1 ir-arraytype)(texpr2 ir-arraytype))
  (with-slots ((size1 size)(elemtype1 ir-range)) texpr1
	      (with-slots ((size2 size)(elemtype2 ir-range)) texpr2
	;		  (and (eql size1 size2);NSF:4/19: size is irrelevant
		(ir2c-tcompatible* elemtype1 elemtype2))))

(defmethod ir2c-tcompatible* ((texpr1 ir-subrange )(texpr2 ir-subrange))
  (eq (ir2c-type texpr1)(ir2c-type texpr2)))
  ;; (and (fixnum-type (ir2c-type texpr1))
  ;;      (fixnum-type (ir2c-type texpr2)))

(defmethod ir2c-tcompatible* ((texpr1 ir-typename)(texpr2 ir-typename))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)) texpr1
	      (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)) texpr2
			  ;(and (eq id1 id2))
			  (ir2c-tcompatible* tdef1 tdef2))))

(defmethod ir2c-tcompatible* ((texpr1 ir-typename)(texpr2 t))
  (with-slots ((id1 ir-type-id)(tdef1 ir-type-defn)) texpr1
	      (ir2c-tcompatible* tdef1 texpr2)))

(defmethod ir2c-tcompatible* ((texpr1 t)(texpr2 ir-typename))
  (with-slots ((id2 ir-type-id)(tdef2 ir-type-defn)) texpr2
	      (ir2c-tcompatible* texpr1 tdef2)))

(defmethod ir2c-tcompatible* ((texpr1 ir-formal-typename)(texpr2 t));;ir2c-tequal is asymmetric 
  (ir-actualparameter-type? texpr2))

(defmethod ir2c-tcompatible* ((texpr1 ir-chartype)(texpr2 ir-chartype))
  t)

(defmethod ir2c-tcompatible* ((texpr1 ir-stringtype)(texpr2 ir-stringtype))
  t)


(defmethod ir2c-tcompatible* ((texpr1 t)(texpr2 t))
  (eq texpr1 texpr2));;Since the base case


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;expects ir2c-types for texpr1 and texpr2; made it copy lazily only when types are mismatched. 
(defun copy-type (texpr1 texpr2 lhs rhs)
  ;; (let ((*var-counter* nil))
  ;;   (newcounter *var-counter*)
  (cons (format nil "//copying to ~a from ~a"
	  (add-c-type-definition texpr1)
	  (add-c-type-definition texpr2))
	(copy-type* texpr1 texpr2 lhs rhs)))

(defmethod copy-type* :around ((ltype t)(rtype t) lhs rhs)
  (if (ir2c-tcompatible ltype rtype)
      (mk-c-assignment-with-count lhs ltype rhs rtype)
    (call-next-method)))

(defun copy-field-types-instrs (ift1 ift2 lhs rhs)
  (cond ((consp ift1)
	 (let* ((ft1 (car ift1))
		(id-ft1 (ir-id ft1))
		(rft1  (ir-vtype ft1))
		(ft2 (find id-ft1 ift2 :test #'eq :key #'ir-id))
		;;(id-ft2 (ir-id ft2))
		(rft2 (ir-vtype ft2))
		(lhs-field (format nil "~a->~a" lhs id-ft1))
		(rhs-field (format nil "~a->~a" rhs id-ft1))
		(cft1 (add-c-type-definition rft1))
		(cft2 (add-c-type-definition rft2))
		(bind-instrs (make-c-decl-assignment-with-count
				    (ir-name ft1)
				    cft1
				    lhs-field
				    cft1))
		(instrs1 (if (mpnumber-type? cft1)
			     (cons (format nil "~a_init(~a)" cft1 lhs-field)
				     (cons
				      (make-c-assignment (format nil "~a" lhs-field)
							 cft1 rhs-field
							 cft2)
				      bind-instrs
				      ;; (make-c-decl-assignment-with-count
				      ;; (intern (format nil "~a_~a" irl1 id-ft1))
				      ;; (ir-ftype ft1)
				      ;; lhs-field
				      ;; cft1)
				      ))
			   (append (copy-type* rft1 rft2
					       lhs-field
					       rhs-field)
				   bind-instrs)
				   ;; (make-c-decl-assignment-with-count
				   ;;  (intern (format nil "~a_~a" irl1 id-ft1))
				   ;;  (ir-ftype ft1)
				   ;;  lhs-field
				   ;;  cft1)
				   )))
	   (append instrs1 (copy-field-types-instrs (cdr ift1) ift2
						    lhs rhs))))
	(t nil)))
	 
(defmethod copy-type* ((ltype ir-recordtype)(rtype ir-recordtype) lhs rhs)
  (with-slots ((ift1 ir-field-types)(irl1 ir-label)) ltype
    (with-slots ((ift2 ir-field-types)(irl2 ir-label)) rtype
      (let ((new-instr (format nil "~a = new_~a()" lhs (add-c-type-definition ltype)))
	    (field-instrs (copy-field-types-instrs ift1 ift2 lhs rhs))
	    (finalize-instrs nil) ;; (loop for ft1 in ift1
				  ;;  append (decrement-or-clear-instrs (intern (format nil "~a_~a" irl1 (ir-id ft1)))
				  ;; 				     (ir-ftype ft1)))
	    )
	;(break "copy-type* (record,record)")
	(list (cons new-instr (append field-instrs finalize-instrs))) ;;make this a block
	))))
	

(defun decrement-or-clear-instrs (var type)
  (if (ir-reference-type? type)
      (list (format nil "~a->count--" var))
    (if (mpnumber-type? type)
	(list (format nil "~a_clear(~a)" type var))
      nil)))

(defmethod copy-type* ((texpr1 ir-fieldtype)(texpr2 ir-fieldtype) lhs rhs)
  (with-slots ((ft1 ir-vtype)(id1 ir-id)) texpr1
	      (with-slots ((ft2 ir-vtype)(id2 ir-id)) texpr2
			  (let ((newlhs (format nil "~a->~a" lhs id1))
				(newrhs (format nil "~a->~a" rhs id2)))
			    (copy-type* ft1 ft2 newlhs newrhs)))))

(defmethod copy-type* ((texpr1 ir-arraytype)(texpr2 ir-funtype) lhs rhs)
  (with-slots ((size1 size)(high1 high)(et1 ir-range)) texpr1
    (with-slots ((dom2 ir-domain)(et2 ir-range)) texpr2 ;(break "ct: array-fun")
      (let* ((index (new-indvar))
	     (tmp (gentemp "tmp"))
	     (size-instrs (cons (format nil "uint32_t ~a" tmp)
				(if (ir-offset? high1)
				    (with-slots (expr offset) high1 ;;(break "copy-type*(array)(fun)")
				      (append (ir2c*  expr tmp '|uint32|)
					      (list (format nil "~a += ~a" tmp (1+ offset)))))
				  (if high1
				      (ir2c*  (mk-ir-integer (1+ high1)) tmp '|uint32|)
				    (list (make-c-assignment tmp '|uint32| (1+ size1) '|uint32|))))))
	     (et1-ctype (add-c-type-definition (ir2c-type et1)))
	     (mpz-dom2? (mpnumber-type? dom2))
	     (mpindex (when mpz-dom2? (new-indvar)))
	     (mp-prelude (when mpz-dom2?
			   (list (format nil "mpz_t ~a" mpindex)
				 (format nil "mpz_init(~a)" mpindex))))
	     (lhs-mp-init-instrs (when (mpnumber-type? et1)
				   (list (format nil "~a->elems[~a] = (~a_ptr_t)safe_malloc(sizeof(~a_t))" lhs index et1-ctype et1-ctype) ;; et1-ctype
					 (format nil "~a_init(~a->elems[~a])" et1-ctype lhs index))))
	     (for-body-instrs  (if mpz-dom2?
				   (cons
				    (format nil "mpz_set_ui(~a, ~a)" mpindex index)
				    (append lhs-mp-init-instrs
					    (copy-type* et1 et2
							 (format nil "~a->elems[~a]" lhs index)
							 (format nil "~a->ftbl->fptr(~a, ~a)" rhs rhs mpindex))))
				 (append lhs-mp-init-instrs
					 (copy-type* et1 et2
						     (format nil "~a->elems[~a]" lhs index)
						     (format nil "~a->ftbl->fptr(~a, ~a)" rhs rhs index)))))
	     (for-instr 
	      (mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
				    index index tmp index)
			    for-body-instrs))) ;(break "ct2")
	(append size-instrs
		(cons (format nil "~a = new_~a(~a)" lhs
			      (add-c-type-definition (ir2c-type texpr1))
			      tmp)
		      (append mp-prelude
			      (cons for-instr
				    (when mpz-dom2? (list (format nil "mpz_clear(~a)" mpindex)))))))))))

(defmethod copy-type* ((texpr1 ir-arraytype)(texpr2 ir-arraytype) lhs rhs)
  ;;NSH(9/9/19): need to generalize from uint32 below. 
  (with-slots ((size1 size)(high1 high)(et1 ir-range)) texpr1
	      (with-slots ((size2 size)(high2 high)(et2 ir-range)) texpr2
		(let* ((index (new-indvar))
		       (tmp (gentemp "tmp"))
		       (size-instrs (cons (format nil "uint32_t ~a" tmp)
					  (if (ir-offset? high1)
					      (with-slots (expr offset) high1 ;(break "copy-type*(array)(array)")
						(append (ir2c*  expr tmp '|uint32|)
							(list (format nil "~a += ~a" tmp (1+ offset)))))
					    (if high1
						(ir2c*  (1+ high1) tmp '|uint32|)
					      (list (make-c-assignment tmp '|uint32| size1 '|uint32|))))))
		       (et1-ctype (add-c-type-definition (ir2c-type et1)))
		       (for-body-instrs (if (mpnumber-type? et1-ctype)
					    (list
					     (format nil "~a->elems[~a] = (~a_ptr_t)safe_malloc(sizeof(~a_t))" lhs index et1-ctype et1-ctype) ;; et1-ctype
					     (format nil "~a_init(~a->elems[~a])" et1-ctype lhs index)
					     (make-c-assignment (format nil "~a->elems[~a]" lhs index)
								et1-ctype
								(format nil "~a->elems[~a]" rhs index)
								(add-c-type-definition (ir2c-type et2))))
					  (copy-type* et1 et2
						      (format nil "~a->elems[~a]" lhs index)
						      (format nil "~a->elems[~a]" rhs index))))
		       (for-instr 
			(mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
					      index index tmp index)
				      for-body-instrs))
		       )
		  (append size-instrs
			  (list (format nil "~a = new_~a(~a)" lhs
					(add-c-type-definition (ir2c-type texpr1))
					tmp)
				for-instr))))))


		    ;; 			(format nil "~a = new_~a(~a)" lhs (add-c-type-definition (ir2c-type texpr1))
		    ;; 				tmp)
		    ;; 			(mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
		    ;; 			      index index tmp index)
		    ;; 				      (copy-type* et1 et2
								  
		    ;; 					      (format nil "&(~a->elems[~a])" lhs index)
		    ;; 					    (format nil "~a->elems[~a]" lhs index))
		    ;; 						  (format nil "~a->elems[~a]" rhs index)))))))
		    ;; (list (format nil "~a = new_~a(~a)" lhs (add-c-type-definition (ir2c-type texpr1)) size1)
		    ;; 	  (mk-for-instr (format nil "uint32_t ~a = 0; ~a < ~a; ~a++"
		    ;; 				index index size1 index)
		    ;; 			(copy-type* et1 et2
		    ;; 				    (if (mpnumber-type? (add-c-type-definition (ir2c-type et1)))
		    ;; 					      (format nil "&(~a->elems[~a])" lhs index)
		    ;; 					    (format nil "~a->elems[~a]" lhs index))
		    ;; 				    (format nil "~a->elems[~a]" rhs index)))))))))
		    

(defmethod copy-type* ((texpr1 ir-typename)(texpr2 t) lhs rhs)
  (with-slots ((id1 ir-type-id)(typedef1 ir-type-defn)) texpr1
    (copy-type* typedef1 texpr2 lhs rhs)))

;; (cons (format nil "~a = new_~a()" lhs id1)
;; 		    (copy-type* typedef1 texpr2 lhs rhs))

(defmethod copy-type* ((texpr1 t)(texpr2 ir-typename) lhs rhs)
  (with-slots ((id2 ir-type-id)(typedef2 ir-type-defn)) texpr2
	      (copy-type* texpr1 typedef2 lhs rhs)))

(defmethod copy-type* ((texpr1 t)(texpr2 t) lhs rhs)
  (mk-c-assignment-with-count lhs texpr1 rhs (add-c-type-definition texpr2)))



(defmethod ir2c-decl* ((ir ir-accessor-defn) decl)
  (let ((cdefn (make-c-defn-info ir (type decl)))
	(udefn (make-c-defn-info (ir-update-defn ir) nil)))
    (setf (cdefn (eval-info decl)) cdefn
	  (update-cdefn (eval-info decl)) udefn)
    (op-name cdefn)))

(defmethod ir2c-decl* ((ir ir-defn) decl)
  (let* ((dtype (if (test-formula? decl) *boolean* (type decl)))
	 (cdefn (make-c-defn-info ir dtype)));(break "ir2c-decl*")
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
  (declare (ignore decl))
  (with-slots (ir-constructor-type) ir
    (with-slots (ir-type-id ir-type-defn) ir-constructor-type
      (add-c-type-definition (ir2c-type ir-type-defn) ir-type-id))
    (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printing out the C code in a block-structured manner
;;the translated code is a list of instructions, where each
;;instruction is either a regular C instruction or an if-instr

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
	       ((consp (car c-instrs));;NSH(4/5/22): Added block structure
		;(break "nested block")
		(format nil
			(format nil "~a{~%~a~a};~%~a"
				*c-scope-string*
				(let ((*c-scope-string* (format nil "~8T~a" *c-scope-string*)))
				  (print2c (car c-instrs)))
				*c-scope-string*
				(print2c (cdr c-instrs)))))
	       (t (format nil (format nil "~a~a;~%~a" *c-scope-string* (car c-instrs)
				      (print2c (cdr c-instrs)))))))
	(t (format nil ""))))

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

(defun ir-theory-formal-type (formal)
    (if (formal-type-decl? formal)
	'|type_actual|
      (pvs2ir-type (type formal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This can be removed once this code is included in the typechecker files
(defun implicit-prelude-importings (theoryref)
  "Given a theory reference, returns the list of prelude theories that are
  implicitly imported through the type and constant declarations."
  (let ((theory (get-theory theoryref))
	(prelude-refs nil))
    (dolist (decl (all-decls theory))
      (when (typep decl '(or type-decl const-decl recursive-type theory-reference))
	(dolist (ref-decl (refers-to decl))
	  (let ((ref-th (module ref-decl)))
	    (when (and (not (eq ref-th theory))
		       (from-prelude? ref-th))
	      (pushnew (module ref-decl) prelude-refs))))))
    prelude-refs))

(defmethod refers-to ((decl inline-datatype))
  nil)

(defmethod print-object ((obj ir-defn) stream)
  (with-slots (ir-function-name ir-args ir-return-type ir-defn) obj
    (if *debugging-print-object*
	(call-next-method)
	(format stream "~@<#<ir-defn ~2I~_~0:Iir-function-name: ~a~:@_ir-args: ~a~:@_ir-return-type: ~a~:@_ir-defn: ~a>~:>"
	  ir-function-name ir-args ir-return-type ir-defn))))

(defmethod print-object ((obj ir-function) stream)
  (with-slots (ir-freevars ir-fname declaration) obj
    (if *debugging-print-object*
	(call-next-method)
	(format stream "~@<#<ir-function ~2I~_~0:Iir-freevars: ~a~:@_ir-fname: ~s~:@_declaration: ~a>~:>"
	  (if (eq ir-freevars 'unbound) ir-freevars (length ir-freevars))
	  ir-fname declaration))))

(defmethod print-object ((obj ir-variable) stream)
  (with-slots (ir-freevars ir-name ir-vtype ir-pvsid) obj
    (if *debugging-print-object*
	(call-next-method)
	;; (format stream "~@<#<ir-variable ~2I~_~0:Iir-freevars: ~a~:@_ir-name: ~s~:@_ir-vtype: ~s~:@_ir-pvsid: ~s~:@_>~:>"
	;;   (if (and (singleton? ir-freevars)
	;; 	   (eq (car ir-freevars) obj))
	;;       "(self_pointer)"
	;;       (if (eq ir-freevars 'unbound) ir-freevars (length ir-freevars)))
	;;   ir-name ir-vtype ir-pvsid)
	(format stream "~@<#<ir-variable: ~s>~:>" ir-name)
	)))

(defmethod print-object ((obj ir-typename) stream)
  (with-slots (renamings unique-name ir-type-id type-declaration ir-actuals ir-formals ir-type-defn) obj
    (if *debugging-print-object*
	(call-next-method)
	(format stream "~@<#<ir-typename ~2I~_~0:Irenamings: ~a~:@_unique-name: ~s~:@_ir-type-id: ~a~:@_type-declaration: ~a~:@_ir-actuals: ~a~:@_ir-formals: ~a~:@_ir-type-defn: ~a>~:>"
	  renamings unique-name ir-type-id type-declaration ir-actuals ir-formals
	  ir-type-defn))))

(defmethod print-object ((obj ir-last) stream)
  (with-slots (ir-freevars ir-var) obj
    (if *debugging-print-object*
	(call-next-method)
	(format stream "~@<#<ir-last ~2I~_~0:Iir-freevars ~a~:@_ir-var: ~s>~:>"
	  (if (eq ir-freevars 'unbound) ir-freevars (length ir-freevars)) ir-var))))

(defmethod print-object ((obj ir-let) stream)
  (with-slots (ir-freevars ir-vartype ir-bind-expr ir-body) obj
    (if *debugging-print-object*
	(call-next-method)
	(format stream "~@<#<ir-let ~2I~_~0:Iir-freevars ~a~:@_ir-vartype: ~s~:@_ir-bind-expr: ~s~:@_ir-body: ~s>~:>"
	  (if (eq ir-freevars 'unbound) ir-freevars (length ir-freevars))
	  ir-vartype (type-of ir-bind-expr) ir-body))))
