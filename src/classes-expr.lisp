;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes-expr.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:41:18 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 29 19:35:03 1999
;; Update Count    : 31
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


(export '(assignments boolean-equation conjunction disequation disjunction
	  equation iff iff-or-boolean-equation implication index negation
	  propositional-application))

;; SBCL changed things so this no longer works - pvs.system
;; simply unlocks the :common-lisp package
#+cmu
(ext:without-package-locks
 (defgeneric type (x))
 (defgeneric (setf type) (x y))
 (defgeneric number (x))
 (defgeneric (setf number) (x y))
 (defgeneric declaration (x))
 (defgeneric (setf declaration) (x y)))

  ;; This is actually defined in utils, but convenient to add here
#+cmu
(ext:without-package-locks
 (defgeneric condition (x)))

;;; Provide a class on which to hang syntactic information

(defcl syntax ()
  ;;newline-comment
  (place :restore-as nil)
  (pvs-sxhash-value :fetch-as nil :ignore t))

;;; Expressions
(defcl expr (syntax)
  (parens :initform 0 :parse t :restore-as nil)
  type
  units
  (free-variables :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound)
;;   (tcc-status :documentation "An alist of expected types and tcc-status -
;;      'none means no TCCs could be generated,
;;      'nocond means that a TCC was generated without conditions")
  ;;from-macro
  )

;; A name of the form 'lib@th[x]{{a:=b}}:->th.id'
(defcl name (syntax)
  (mod-id :parse t :restore-as nil)
  (library :parse t :restore-as nil)
  (actuals :parse t)
  (id :parse t :restore-as nil)
  (mappings :parse t)
  (target :parse t)
  resolutions)

(defcl formula-name (name)
  (mod-id :parse t :restore-as nil))

(defcl name-expr (name expr)
  ;;(kind :documentation "Variable, constant, etc.")
  )

(defcl theory-name-expr (name-expr modname)
  ;; For names with resolutions to a theory
  )

(defcl typed-name-expr (name-expr)
  ;; For (pc-parse "foo: bool" 'unique-name)
  ;; the declared-type is bool
  declared-type)

(defcl field-name-expr (name-expr))

;; For stand-alone field projections, e.g., `a[R]
(defcl fieldex (expr)
  id
  actuals)

(defcl adt-name-expr (name-expr)
  adt-type)

(defcl constructor-name-expr (adt-name-expr)
  (recognizer-name :fetch-as nil :ignore t)
  (accessor-names :initform 'unbound :ignore t))

(defcl recognizer-name-expr (adt-name-expr)
  (constructor-name :fetch-as nil :ignore t)
  (unit? :initform 'unbound :fetch-as 'unbound :ignore t))

(defcl accessor-name-expr (adt-name-expr))

(defcl field-application (expr)
  (id :restore-as nil)
  actuals
  argument)

(defcl fieldappl (field-application))

;; When a fieldex is used as a conversion
(defcl field-conversion (fieldappl))

(defcl projection-expr (name-expr)
  (index :type fixnum :parse t :restore-as nil))

;; For stand-alone projections, e.g., `2[T]
(defcl projex (projection-expr))

(defcl projection-application (expr)
  (id :restore-as nil)
  actuals
  (index :type fixnum :restore-as nil)
  argument)

(defcl projappl (projection-application))

;; When a projection-expr is used as a conversion
(defcl projection-conversion (projappl))

(defcl injection-expr (constructor-name-expr)
  (index :type fixnum :parse t :restore-as nil))

(defcl injection?-expr (recognizer-name-expr)
  (index :type fixnum :parse t :restore-as nil))

(defcl extraction-expr (accessor-name-expr)
  (index :type fixnum :parse t :restore-as nil))

(defcl injection-application (expr)
  (id :restore-as nil)
  actuals
  (index :type fixnum :restore-as nil)
  argument)

;; When an injection-expr is used as a conversion
(defcl injection-conversion (injection-application))

(defcl injection?-application (expr)
  (id :restore-as nil)
  actuals
  (index :type fixnum :restore-as nil)
  argument)

(defcl extraction-application (expr)
  (id :restore-as nil)
  actuals
  (index :type fixnum :restore-as nil)
  argument)

;; When an extraction-expr is used as a conversion
(defcl extraction-conversion (extraction-application))

(defcl rational-expr (expr)
  (number :type rational))
  
(defcl number-expr (rational-expr)
  (number :type integer :parse t :restore-as nil))

;; This is for integers of the form xxx.000, where the fractional part is
;; all zeros.  We keep it as a number expr, but store the number of zeros so
;; the printer can restore it.
(defcl decimal-integer (number-expr)
  (fractional-length :type fixnum :restore-as nil))

;(defcl function-expr (expr)
;  assignments)

(defcl tuple-expr (expr)
  (exprs :parse t))


;;; Arg-tuple-expr instances are generated for application arguments of
;;; length > 1.

(defcl arg-tuple-expr (tuple-expr))

(defcl projected-arg-tuple-expr (arg-tuple-expr))

(defcl record-expr (expr)
  (assignments :parse t))

(defcl cases-expr (expr)
  (expression :parse t)
  (selections :type list :parse t)
  (else-part :parse t))

(defcl selection (syntax)
  (constructor :parse t)
  (args :type list :parse t)
  (expression :parse t))

(defcl unpack-expr (cases-expr))

(defcl in-selection (selection)
  (index :type fixnum :restore-as nil))

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
  (string-value :restore-as nil))

(defcl bracket-expr (application)
  ;;"Used for expressions of the form [| |]"
  )

(defcl paren-vbar-expr (application)
  ;;"Used for expressions of the form (| |)"
  )

(defcl brace-vbar-expr (application)
  ;;"Used for expressions of the form {| |}"
  )

(defcl infix-application (application))

(defcl unary-application (application))

(defcl propositional-application (application))

(defcl negation (propositional-application))
(defcl unary-negation (negation unary-application))

(defcl conjunction (propositional-application))
(defcl infix-conjunction (conjunction infix-application))

(defcl disjunction (propositional-application))
(defcl infix-disjunction (disjunction infix-application))

(defcl implication (propositional-application))
(defcl infix-implication (implication infix-application))
(defcl when-expr (implication))
(defcl infix-when-expr (infix-implication))

(defcl iff-or-boolean-equation (application))

(defcl iff (iff-or-boolean-equation propositional-application))
(defcl infix-iff (iff infix-application))

(defcl equation (application))
(defcl infix-equation (equation infix-application))

(defcl boolean-equation (iff-or-boolean-equation equation))
(defcl infix-boolean-equation (boolean-equation infix-equation))

(defcl disequation (application))
(defcl infix-disequation (disequation infix-application))


;; This is just a special class for prettyprinting 3.1416, which internally is
;; (/ 31416 10000) 
(defcl decimal (application))

  
(defcl if-expr (application))

(defcl chained-if-expr (if-expr))

;;; A branch is an application of the form IF(a,b,c)
;;; where the IF is the one defined in the prelude.
(defcl branch (application))

;;; This is an application of the form IF a THEN b ELSE c ENDIF
;;; where the IF is the one defined in the prelude.
;;; Used strictly for unparsing, other methods should only be defined over
;;; if-conditionals.
(defcl mixfix-branch (if-expr branch))

;;; For an if-then-elsif-then-else form of an IF from the prelude.
(defcl chained-branch (mixfix-branch chained-if-expr))

(defcl let-expr (application))

(defcl chained-let-expr (let-expr))

;;(defcl parlet-expr (application))

(defcl where-expr (let-expr))

(defcl chained-where-expr (where-expr))

(defcl coercion (application))


;;; A cond-expr is an if-expr.  The first-cond-expr is used to control the
;;; generation of TCCs. The last-cond-expr handles cond-exprs without
;;; ELSEs, e.g., to distinguish COND e1 -> a1, e2 -> a2 ENDCOND from COND
;;; e1 -> a1, e2 -> a2, ELSE -> a2 ENDCOND which translate to the same
;;; if-expressions.

(defcl first-cond-expr (mixfix-branch))

(defcl single-cond-expr (mixfix-branch))

(defcl cond-expr (mixfix-branch))

(defcl last-cond-expr (mixfix-branch))

(defcl else-condition (unary-negation))


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
  (commas? :parse t :restore-as nil)
  (chain? :parse t :restore-as nil))

(defcl lambda-expr (binding-expr))

;;(defcl lam-expr (lambda-expr))

(defcl lambda-conversion (lambda-expr))

(defcl set-expr (lambda-expr))

(defcl set-list-expr (set-expr)
  exprs)

(defcl let-lambda-expr (lambda-expr))

;; After typechecking, a fieldex is converted to a fieldex-lambda-expr, to
;; minimize the effect of adding the fieldex class.
(defcl fieldex-lambda-expr (lambda-expr)
  actuals)

(defcl funtype-conversion (lambda-expr)
  domain-conversion
  range-conversion)

(defcl rectype-conversion (lambda-expr)
  conversions)

(defcl tuptype-conversion (lambda-expr)
  conversions)

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

(defcl quoted-assign (syntax))

(defcl id-assign (name-expr quoted-assign))

(defcl field-assign (field-assignment-arg id-assign))

(defcl proj-assign (number-expr quoted-assign))

(defcl accessor-assignment-arg (accessor-name-expr))

(defcl accessor-assign (accessor-assignment-arg id-assign))

;;; Misc

;(defcl bindings ()
;  declarations)

(defcl simple-decl (syntax)
  (id :parse t :restore-as nil)
  (declared-type :parse t)
  type
  units)

(defcl binding (simple-decl))

(defcl dep-binding (binding name)
  (parens :initform 0 :parse t :restore-as nil))

(defcl field-decl (binding name)
  ;;(recordtype :documentation "A pointer to the record typeof this decl")
  (chain? :parse t :restore-as nil))


;;; bind-decl is for local bindings, used in binding-exprs and in the
;;; formals for const-decls, etc.  pred-bind-decls are a special form of
;;; bind-decl; for example (x|p(x)) which is translated by the parser
;;; into (x:{x|p(x)}).  arg-bind-decl is used by the parser to handle
;;; the difference between
;;;   foo(x,(y:int)): nat = ...
;;;   foo(x,y:int): nat = ...
;;; The x is an arg-bind-decl, as is the y in the second case.

(defcl bind-decl (binding name-expr)
  (chain? :parse t :restore-as nil))

(defcl arg-bind-decl (bind-decl))

(defcl pred-bind-decl (bind-decl))

(defcl untyped-bind-decl (bind-decl))


(defcl modname (name))

(defcl full-modname (modname))

(defcl datatype-modname (modname))

(defcl interpreted-modname (modname)
  interp-theory)

;;; Used to keep modnames for which TCCs have not yet been generated.  Used to
;;; delay the generation of TCCs until the TCC context has been built up,
;;; while allowing module instances to be generated, which all satisfy the
;;; invariant that they are fully typechecked.

(defcl modname-no-tccs (modname)) 

(defcl actual (syntax)
  (expr :parse t)
  type-value)

(defcl mapping (syntax)
  (lhs :parse t)
  (rhs :parse t)
  (kind :parse t :restore-as nil)
  (declared-type :parse t)
  type)

(defcl mapping-subst (mapping))
(defcl mapping-rename (mapping)
  mapped-decl)

;;; Mapping-with-formals is used for mappings of the form
;;;  f(a,b:int)(x:bool) = g(a,x,b)
;;; which is treated the same as
;;;  f = lambda (a,b:int)(x:bool): g(a,x,b)
(defcl mapping-with-formals (mapping)
  (formals :parse t))

(defcl mapping-subst-with-formals (mapping-with-formals mapping-subst))
(defcl mapping-rename-with-formals (mapping-with-formals mapping-rename))

(defcl mapping-rhs (actual))

(defcl mapping-rhs-rename (mapping-rhs))

;;; The resolution class holds the possibilities generated during name
;;; resolution.  The inclusions are the predicates which will become
;;; TCCs if that particular resolution is chosen.

(defcl resolution ()
  (declaration :restore-as nil)
  module-instance
  type)

;(defcl judgement-resolution (resolution)
;  judgement-type
;  comparison-type
;  judgement)

(defcl recursive-defn-conversion (lambda-expr)
  from-expr)

(defcl conversion-resolution (resolution)
  conversion)

(defcl lambda-conversion-resolution (resolution)
  (k-conv-type :documentation "The common conversion type, e.g., state"))

;; This allows us to keep track of where the conversion expr came from.
(defcl conversion-result ()
  conversion
  expr)

(defcl recursive-function-resolution (resolution)
  conversion)

;; Needed for lhs references, e.g.,
;;   IMPORTING th{{x := ...}} AS foo
;;    ... foo.x ...
;; In this case, the declaration is the mapping, the module instance is foo
;; and the type is the type of the expression, or the type value, depending on
;; x.  If x is a theory declaration, resolutions are not needed, since in that
;; case foo.x is not a type-name or name-expr.
(defcl mapping-resolution (resolution))

(defcl context ()
  (theory :restore-as nil)
  (theory-name :restore-as nil)
  (declaration :restore-as nil)
  library-alist
  (declarations-hash :restore-as nil)
  (using-hash :restore-as nil)
  (judgements :initform (make-instance 'judgements) :restore-as nil)
  (known-subtypes :initform nil :restore-as nil)
  (conversions :initform nil :restore-as nil)
  (disabled-conversions :initform nil :restore-as nil)
  (auto-rewrites :initform nil)
  (disabled-auto-rewrites :initform nil))

(defcl judgements ()
  (judgement-types-hash
   :initform (make-pvs-hash-table #-cmu :weak-keys? #-cmu t)
   :fetch-as nil)
  judgement-declarations ;;keeps track of all the judgement declarations
  number-judgements-alist
  name-judgements-alist
  application-judgements-alist
  expr-judgements-alist)

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
  ;;(argtype-hash :initform nil)
  (generic-judgements :initform nil)
  (judgements-graph :initform nil))
