;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes-expr.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:41:18 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 14:51:55 1998
;; Update Count    : 28
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;; Provide a class on which to hang syntactic information

(defcl syntax ()
  newline-comment
  place
  (pvs-sxhash-value :fetch-as nil :ignore t))

;;; Expressions

(defcl expr (syntax)
  (parens :initform 0 :parse t)
  type
  (free-variables :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound))

(defcl name (syntax)
  (mod-id :parse t)
  (library :parse t)
  (actuals :parse t)
  (id :parse t)
  resolutions)

(defcl name-expr (name expr)
  (kind :documentation "Variable, constant, etc."))

(defcl field-name-expr (name-expr))

(defcl adt-name-expr (name-expr)
  adt-type)

(defcl constructor-name-expr (adt-name-expr)
  recognizer-name
  (accessor-names :initform 'unbound))

(defcl recognizer-name-expr (adt-name-expr)
  constructor-name
  (unit? :initform 'unbound))

(defcl accessor-name-expr (adt-name-expr))

(defcl field-application (expr)
  id
  argument)

(defcl fieldappl (field-application))

(defcl projection-expr (name-expr)
  (index :parse t))

(defcl projection-application (expr)
  id
  index
  argument)

(defcl projappl (projection-application))

(defcl number-expr (expr)
  (number :parse t))

;(defcl function-expr (expr)
;  assignments)

(defcl tuple-expr (expr)
  (exprs :parse t))


;;; Arg-tuple-expr instances are generated for application arguments of
;;; length > 1.

(defcl arg-tuple-expr (tuple-expr))

(defcl record-expr (expr)
  (assignments :parse t))

(defcl cases-expr (expr)
  (expression :parse t)
  (selections :parse t)
  (else-part :parse t))

(defcl selection (syntax)
  (constructor :parse t)
  (args :parse t)
  (expression :parse t))


(defcl application (expr)
  (operator :parse t)
  (argument :parse t))

(defcl binding-application (application))

(defcl list-expr (application)
  ;;"Used for lists built with (: :)"
  )

(defcl null-expr (constructor-name-expr)
  ;;"Used for the null list"
  )

(defcl string-expr (application)
  string-value)

(defcl bracket-expr (application)
  ;;"Used for expressions of the form [| |]"
  )
  
(defcl if-expr (application))

(defcl chained-if-expr (if-expr))

(defcl infix-application (application))

(defcl unary-application (application))

(defcl when-expr (infix-application))

(defcl let-expr (application))

(defcl chained-let-expr (let-expr))

;;(defcl parlet-expr (application))

(defcl where-expr (application))

(defcl chained-where-expr (where-expr))

(defcl coercion (application))


;;; A cond-expr is an if-expr.  The first-cond-expr is used to control the
;;; generation of TCCs. The last-cond-expr handles cond-exprs without
;;; ELSEs, e.g., to distinguish COND e1 -> a1, e2 -> a2 ENDCOND from COND
;;; e1 -> a1, e2 -> a2, ELSE -> a2 ENDCOND which translate to the same
;;; if-expressions.

(defcl first-cond-expr (if-expr))

(defcl single-cond-expr (if-expr))

(defcl cond-expr (if-expr))

(defcl last-cond-expr (if-expr))

(defcl else-condition (unary-application))


(defcl table-expr (expr)
  row-expr
  col-expr
  row-headings
  col-headings
  table-entries)

(defcl cases-table-expr (cases-expr table-expr))

(defcl cond-table-expr (first-cond-expr table-expr))

(defcl single-cond-table-expr (single-cond-expr table-expr))

(defcl let-table-expr (let-expr table-expr))


;;; argument-conversions go hand-in-hand with lambda-conversions, e.g.
;;;   k: [int -> [state -> int]] = (LAMBDA i: (LAMBDA s: i))
;;;   CONVERSION k
;;;   f: [[state -> int] -> bool]
;;;   ss: [state -> int]
;;; then f(ss + 1) becomes (LAMBDA s: f(ss(s) + 1)), where the outer LAMBDA
;;; is a lambda-conversion and ss(s) is an argument-conversion

(defcl argument-conversion (application))

(defcl implicit-conversion (application))

;(defcl intype (application)
;  expression
;  declared-type
;  type-value)

(defcl binding-expr (expr)
  (bindings :parse t)
  (expression :parse t)
  (result-type :parse t)
  (commas? :parse t)
  type-value
  (chain? :parse t))

(defcl lambda-expr (binding-expr))

;;(defcl lam-expr (lambda-expr))

(defcl lambda-conversion (lambda-expr))

(defcl set-expr (lambda-expr))

(defcl quant-expr (binding-expr))

(defcl forall-expr (quant-expr))

;;(defcl all-expr (forall-expr))

(defcl exists-expr (quant-expr))

;;(defcl some-expr (exists-expr))

;;(defcl exists!-expr (application))

;;(defcl choose-expr (application))

(defcl update-expr (expr)
  (expression :parse t)
  (assignments :parse t))

(defcl assignment (syntax)
  (arguments :parse t)
  (expression :parse t))

(defcl maplet (assignment))

(defcl uni-assignment (assignment))

(defcl uni-maplet (maplet uni-assignment))

(defcl field-assignment-arg (field-name-expr))

(defcl field-assign (field-assignment-arg))

(defcl proj-assign (number-expr))

;;; Misc

;(defcl bindings ()
;  declarations)

(defcl simple-decl (syntax)
  (id :parse t)
  (declared-type :parse t)
  type)

(defcl binding (simple-decl))

(defcl dep-binding (binding name)
  (parens :initform 0 :parse t))

(defcl field-decl (binding name)
  ;;(recordtype :documentation "A pointer to the record typeof this decl")
  (chain? :parse t))


;;; bind-decl is for local bindings, used in binding-exprs and in the
;;; formals for const-decls, etc.  pred-bind-decls are a special form of
;;; bind-decl; for example (x|p(x)) which is translated by the parser
;;; into (x:{x|p(x)}).  arg-bind-decl is used by the parser to handle
;;; the difference between
;;;   foo(x,(y:int)): nat = ...
;;;   foo(x,y:int): nat = ...
;;; The x is an arg-bind-decl, as is the y in the second case.

(defcl bind-decl (binding name-expr)
  (chain? :parse t))

(defcl arg-bind-decl (bind-decl))

(defcl pred-bind-decl (bind-decl))

(defcl untyped-bind-decl (bind-decl))


(defcl modname (name)
  (mappings :parse t))

(defcl full-modname (modname))

(defcl datatype-modname (modname))

;;; Used to keep modnames for which TCCs have not yet been generated.  Used to
;;; delay the generation of TCCs until the TCC context has been built up,
;;; while allowing module instances to be generated, which all satisfy the
;;; invariant that they are fully typechecked.

(defcl modname-no-tccs (modname)) 

(defcl actual (syntax)
  (expr :parse t)
  type-value)

;;; The resolution class holds the possibilities generated during name
;;; resolution.  The inclusions are the predicates which will become
;;; TCCs if that particular resolution is chosen.

(defcl resolution ()
  declaration
  module-instance
  type)

;(defcl judgement-resolution (resolution)
;  judgement-type
;  comparison-type
;  judgement)

(defcl conversion-resolution (resolution)
  conversion)

(defcl lambda-conversion-resolution (resolution)
  conversion)

(defcl context ()
  module
  declaration
  mod-name
  (local-decls :type hash-table :initform (make-hash-table :test #'eq))
  (local-proof-decls :type hash-table :initform (make-hash-table :test #'eq))
  using
  (judgements :initform (make-instance 'judgements))
  (known-subtypes :initform nil)
  (conversions :initform nil))

(defcl judgements ()
  (judgement-types-hash
   :initform (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
  (number-judgements-hash :initform (make-hash-table :test 'eql))
  (name-judgements-hash :initform (make-hash-table :test 'eq))
  (application-judgements-hash :initform (make-hash-table :test 'eq)))

(defcl name-judgements ()
  (minimal-judgements :initform nil)
  (generic-judgements :initform nil))

;;; Application judgements are treated specially; we cannot just keep the
;;; minimal elements, as it is driven by the actual arguments.  The
;;; judgements-list is a tree of judgement declarations, starting with the
;;; minimal types.
;;; The argtype-hash is only a hash-table as long as no judgement is a
;;; dependent type.

(defcl application-judgements ()
  (argtype-hash :initform nil)
  (generic-judgements :initform nil)
  (judgements-graph :initform nil))

