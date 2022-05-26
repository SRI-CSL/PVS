(in-package :pvs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The classes defined below capture the abstract syntax of the IR with parent classes ir-expr
;;and ir-type.  
;;

(defcl ir-expr ()
  (ir-freevars :initform 'unbound :fetch-as 'unbound))

(defcl ir-type ()
  renamings unique-name);;Needed to lazily apply the renaming bindings during preprocessing

(defcl ir-integer (ir-expr)
  ir-intval)
;;ir-fload and ir-double are not used
(defcl ir-float (ir-expr)
  ir-float-mantissa
  ir-float-exponent)

(defcl ir-double (ir-expr)
  ir-mantissa
  ir-exponent)

(defcl ir-bool (ir-expr)
  ir-boolval)

(defcl ir-string (ir-expr)
  ir-stringval)

(defcl ir-char (ir-expr)
  ir-charval)
  

(defcl ir-variable (ir-expr)
  ir-name
  ir-vtype
  ir-pvsid) ;optional pvsid for the variable

(defcl ir-function (ir-expr)
  ir-fname
  declaration);; the declaration is optional; it can be used to access other eval-info

(defcl ir-primitive-function (ir-function)
  )

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

(defcl ir-offset (ir-expr)
  expr offset)

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

(defcl ir-exit (ir-expr)
  ir-message
  ir-code)
  

(defcl ir-type-actual (ir-type)
  ir-actual-type)

(defcl ir-actuals ()
  ir-actuals-list);;list of ir-expr or ir-type actuals

(defcl ir-root-typename (ir-type) ;;ir-formal-typename is now a subclass of ir-root-typename
                                  ;;and not a parent of ir-typename
  ir-type-id
  type-declaration)

(defcl ir-formal-typename (ir-root-typename))


(defcl ir-typename (ir-root-typename);;ir-formal-typename with ir-type-defn
  ir-actuals
  ir-formals
  ir-type-defn);;ir-type-defn is the unsubstituted definition of the typename


(defcl ir-recordtype (ir-type)
  ir-label ;needed for type dependencies
  ir-field-types)

(defcl ir-filetype (ir-recordtype)) ;; keep track of records that are actually files. 

(defcl ir-tupletype (ir-recordtype))

(defcl ir-stringtype (ir-type))

(defcl ir-chartype (ir-type))

(defcl ir-adt-recordtype (ir-recordtype)
  ir-constructors)

(defcl ir-adt-constructor-recordtype (ir-recordtype)
  ir-adt-name)

(defcl ir-fieldtype (ir-type)
  ir-id ir-ftype)

(defcl ir-funtype (ir-type)  ;;NSH(4/3/22): Need to handle dependencies in range.
  ir-domain
  ir-range)

(defcl ir-subrange (ir-type)
  ir-low ir-high ir-low-expr ir-high-expr)

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
  high
  ir-domain
  ir-range
  );later on, we could add an offset

(defcl constructor-eval-info (eval-info)
  ctype)

(defcl accessor-eval-info (eval-info)
  update-cdefn)

(defcl formal-const-eval-info (eval-info)
  )
