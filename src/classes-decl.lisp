;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes-decl.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:40:37 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jul  1 18:50:34 1999
;; Update Count    : 56
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; This file defines most of the classes used in PVS.  Below is a
;;; summary, which leaves out many of the slots and some of the classes
;;; which are mostly useful for unparsing.  This list is mostly useful
;;; for defining methods.

;;; Module level
;;;   modules		- modules
;;;   datatype-or-module- id, formals, formals-sans-usings, assuming,
;;;                       filename, status, generated-by
;;;   datatype		- importings, constructors, typechecked?,
;;;                       adt-type-name,
;;;                       adt-theory, adt-map-theory, adt-reduce-theory,
;;;                       generated-file-date, positive-types, semi
;;;   adt-constructor	- recognizer, ordnum
;;;   simple-constructor- id, arguments, con-decl, rec-decl, acc-decls
;;;   module		- declarations, judgements, coercions,
;;;                       

;;; Declarations: all have an id, and all but the last two have formals
;;;   formal-type-decl	- type
;;;   formal-const-decl	- type
;;;   adtdecl		- type
;;;   mod-decl		- modname
;;;   type-decl		- type-value
;;;   type-eq-decl	- type-expr
;;;   type-from-decl	- type-expr
;;;   var-decl		- type
;;;   const-decl	- type, definition
;;;   def-decl		- type, definition, measure
;;;   formula-decl	- spelling, definition, justification
;;;   field-decl	- type

;;; Type Expressions: all have a print-type
;;;   enumtype		- elements
;;;   type-name		- (see name)
;;;   subtype		- supertype, predicate
;;;   funtype		- domain, range
;;;   tupletype		- types
;;;   recordtype	- fields
;;;   expr-as-type	- expr

;;; Expressions: all but the last 2 have a type
;;;   name-expr		- (see name)
;;;   number-expr	- number
;;;   record-expr	- assignments
;;;   coercion		- expression, declared-type
;;;   tuple-expr	- exprs
;;;   intype		- expression, declared-type, type-value
;;;   cases-expr	- expression, selections, else-part
;;;   application	- operator, arguments
;;;   binding-expr	- bindings, type-value, expression
;;;   update-expr	- expression, assignments
;;;   assignment	- arguments, expression
;;;   selection		- pattern, expression

;;; Misc.
;;;   dep-binding	- (see name and var-decl)
;;;   modname		- renamings, (see name)
;;;   rename		- source, target
;;;   name		- mod-id, actuals, id, resolutions
;;;   actual		- expr, type-value
;;;   resolution	- declaration, module-instance, type

;;; The classes for formals, assumings, theory, and fields
;;; come in three parts.  One is a dummy, used to provide a way to create
;;; multiple declarations from, for example, "i,j,k: int".  The problem
;;; is that the parser would like to break these apart for typechecking,
;;; but the unparser wants them put back together.  See parse.lisp and
;;; unparse.lisp for details.

(in-package 'pvs)

;;; So we can unparse a list of modules into a single file.

(defcl modules ()
  modules)

;;; The slots common to both datatypes and modules
;;; id, formals, assuming are set by the parser

(defcl datatype-or-module (syntax)
  (id :type symbol :parse t)
  (formals :type list
	   :documentation "a list of formal-decls"
	   :parse t)
  (formals-sans-usings :type list
		       :documentation "a list of formal decls, without usings"
		       :fetch-as nil)
  (assuming :type list
	    :documentation "a list of declarations and usings"
	    :parse t)
  (filename :type (or null string)
	    :documentation "The filename sans directory or extension")
  (path :type (or null pathname))
  (status :type list
	  :documentation "A list containing the completed actions")
  (generated-by :documentation "a module id")
  (info :documentation
	"A list of information strings produced by calls to pvs-info")
  (warnings :documentation
	    "A list of warning strings produced by calls to pvs-info"))


;;; Datatypes and related classes

(defcl recursive-type (datatype-or-module)
  (importings :parse t)
  (constructors :documentation "a list of constructors"
		:parse t)
  positive-types
  generated-file-date
  adt-type-name
  adt-theory
  adt-map-theory
  adt-reduce-theory
  (semi :parse t))

(defcl recursive-type-with-subtypes (recursive-type)
  subtypes)

(defcl inline-recursive-type (recursive-type)
  generated
  typechecked?)

;;; Datatypes

(defcl datatype (recursive-type))

(defcl inline-datatype (inline-recursive-type datatype))

(defcl datatype-with-subtypes (recursive-type-with-subtypes datatype))

(defcl library-datatype (datatype)
  library
  library-path)

(defcl inline-datatype-with-subtypes (inline-datatype datatype-with-subtypes))

;;; Codatatypes

(defcl codatatype (recursive-type))

(defcl inline-codatatype (inline-recursive-type codatatype))

(defcl codatatype-with-subtypes (codatatype recursive-type-with-subtypes))

(defcl library-codatatype (codatatype)
  library
  library-path)

(defcl inline-codatatype-with-subtypes (inline-codatatype codatatype-with-subtypes))

(defcl adt-constructor (syntax)
  (recognizer :type symbol :parse t)
  (ordnum :type fixnum))

(defcl constructor-with-subtype (simple-constructor)
  subtype)


;;; This should be merged with the above, since there are no
;;; record-constructors anymore

(defcl simple-constructor (adt-constructor)
  (id :type symbol :parse t)
  (arguments :documentation "a list of adtdecls" :parse t)
  con-decl
  rec-decl
  acc-decls)


;;; Modules

(defcl module (datatype-or-module)
  (theory :type list :parse t) ; The declarations of the theory-part
  (exporting :type list :parse t)  ; A list of exportings
  nonempty-types  ; Keep track of types marked nonempty during typechecking
  all-usings ; The transitive closure of the usings of the theory
  (immediate-usings :initform 'unbound) ; immediate usings of the theory
  instances-used
  assuming-instances
  used-by
  (saved-context :fetch-as nil)
  tccs-tried?
  modified-proof?
  (tcc-info :type list :initform (list 0 0 0 0))
  (ppe-form :fetch-as nil)
  (tcc-form :fetch-as nil)
  typecheck-time)

(defcl library-theory (module)
  library
  library-path)

(defcl exporting (syntax)
  (names :documentation "a list of names and absexpnames" :parse t)
  (but-names :documentation "a list of names and absexpnames" :parse t)
  (kind :documentation "One of NIL, ALL, CLOSURE, or DEFAULT" :parse t)
  (modules :documentation "a list of modnames" :parse t)
  (closure :documentation "The closure of the list of modules"))

(defcl expname (syntax)
  (id :parse t)
  (kind :parse t)
  type)

(defcl importing (syntax)
  (theory-name :parse t)
  (semi :parse t)
  (chain? :parse t)
  refers-to
  generated
  (saved-context :fetch-as nil))

;;; DECLARATION Classes.  Many of these have both a declared-type and a
;;; type slot.  The declared-type is set by the parser and used by the
;;; unparser.  The type is set by the typechecker to the canonical value
;;; of the declared-type.

(defcl declaration (syntax)
  (id :type symbol :parse t)
  (formals :type list :parse t)
  module
  (refers-to :type list)
  (referred-by :type list :fetch-as nil)
  (chain? :type symbol :parse t)
  (typechecked? :type symbol)
  (visible? :type symbol)
  (generated :documentation "a list of declarations")
  (generated-by :documentation "a declaration instance")
  (semi :parse t)
  typecheck-time)

;;; declared-type-string keeps the string of the declared type for
;;; creating the pvs context - see create-declaration-entry

(defcl typed-declaration (declaration)
  (declared-type :parse t)
  (declared-type-string :fetch-as nil)
  type)


;;;  t: TYPE  --> type-decl
;;;  t: TYPE+ --> nonempty-type-decl
;;;  t: TYPE = x  --> type-eq-decl
;;;  t: TYPE+ = x --> nonempty-type-eq-decl
;;;  t: TYPE FROM x  --> type-from-decl
;;;  t: TYPE+ FROM x --> nonempty-type-from-decl

(defcl type-decl (declaration)
  type-value)

(defcl nonempty-type-decl (type-decl)
  keyword)

;;; A mixin
(defcl type-def-decl (type-decl)
  (type-expr :parse t)
  (contains :parse t))

;;; A mixin
(defcl nonempty-type-def-decl (type-def-decl nonempty-type-decl))

(defcl type-eq-decl (type-def-decl))

(defcl nonempty-type-eq-decl (type-eq-decl nonempty-type-def-decl))

(defcl type-from-decl (type-def-decl))

(defcl nonempty-type-from-decl (type-from-decl nonempty-type-def-decl))


;;; Formal theory parameter declarations

(defcl formal-decl (declaration)
  dependent?)

(defcl formal-type-decl (formal-decl type-decl typed-declaration))

(defcl formal-nonempty-type-decl (formal-type-decl nonempty-type-decl))

(defcl formal-subtype-decl (formal-type-decl type-from-decl))

(defcl formal-nonempty-subtype-decl (formal-subtype-decl nonempty-type-decl))

(defcl formal-const-decl (formal-decl typed-declaration)
  possibly-empty-type?)

(defcl adtdecl (typed-declaration)
  (bind-decl :documentation "Keeps a corresponding bind-decl"))

(defcl lib-decl (declaration)
  (lib-string :parse t)
  (library :parse t))

(defcl lib-eq-decl (lib-decl))

(defcl mod-decl (declaration)
  (modname :parse t)
  (saved-context :fetch-as nil))

(defcl var-decl (typed-declaration))

;; if from-object-decl? = t then the declaration is an object-decl.
;; ada-generated-params records the parameters generated for an ada 
;; subprogram spec, so that same parameters are used for the subprogram 
;; body.

(defcl const-decl (typed-declaration)
  (definition :parse t)
  def-axiom
  (eval-info :fetch-as nil))

(defcl adt-constructor-decl (const-decl)
  ordnum)

(defcl adt-recognizer-decl (const-decl)
  ordnum)

(defcl adt-accessor-decl (const-decl))

(defcl def-decl (const-decl)
  (declared-measure :parse t)
  (ordering :parse t)
  measure
  measure-depth)

(defcl fixpoint-decl (const-decl))

(defcl inductive-decl (fixpoint-decl)) 

(defcl adt-def-decl (def-decl))

(defcl corecursive-decl (const-decl))

(defcl coinductive-decl (fixpoint-decl))


;;; Formula-decl slots have the following meaning:
;;;   spelling:   One of FORMULA, AXIOM, LEMMA, etc.
;;;   definition: The body of the formula declaration
;;;   closed-definition: The closure of the body of the definition
;;;   kind:       The kind of formula-decl (e.g., TCC, EXISTENCE)
;;;   justification: The default justification
;;;   justifications: The justifications for this delaration - the default
;;;                   justification is one of these
;;;   proof-status: The status of the default justification - one of
;;;                   proved, unproved, unfinished, or unchecked
;;;   proof-refers-to: The delarations that the default justification refers to
;;;   proof-time: The times associated with the proof; a list of the form
;;;               (runtime, realtime, interactive?)

(defcl formula-decl (declaration)
  (spelling :documentation "One of formula, axiom, lemma, etc." :parse t)
  (definition :parse t)
  ;; The universal closure of the definition, used in create-formulas
  (closed-definition :fetch-as nil)
  kind
  (default-proof :fetch-as nil)
  (proofs :fetch-as nil))

(defcl tcc-decl (formula-decl)
  (tcc-disjuncts
   :documentation "The disjuncts of the definition used for TCCs")
  importing-instance)

(defcl subtype-tcc (tcc-decl))

(defcl termination-tcc (tcc-decl))

(defcl existence-tcc (tcc-decl))

(defcl assuming-tcc (tcc-decl)
  theory-instance
  generating-assumption)

(defcl mapped-axiom-tcc (tcc-decl)
  theory-instance
  generating-axiom)

(defcl cases-tcc (tcc-decl))

(defcl well-founded-tcc (tcc-decl))

(defcl same-name-tcc (tcc-decl))

(defcl cond-disjoint-tcc (tcc-decl))

(defcl cond-coverage-tcc (tcc-decl))

(defcl monotonicity-tcc (tcc-decl))

;;; judgement class is a mixin
(defcl judgement (typed-declaration))

(defcl subtype-judgement (judgement)
  (declared-subtype :parse t)
  subtype)

(defcl number-judgement (judgement)
  (number-expr :parse t))

(defcl name-judgement (judgement)
  (name :parse t))

(defcl application-judgement (judgement)
  (name :parse t)
  (formals :parse t)
  judgement-type)

(defcl conversion-decl (declaration)
  k-combinator?
  (name :parse t))

(defcl typed-conversion-decl (conversion-decl)
  (declared-type :parse t))

;;; Same as conversion-decl, but prints differently
(defcl conversionplus-decl (conversion-decl))

(defcl typed-conversionplus-decl (typed-conversion-decl))

;;; Not only prints differently, but removes the conversion(s)
(defcl conversionminus-decl (conversion-decl))

(defcl typed-conversionminus-decl (typed-conversion-decl))


;;; Type Expressions

(defcl type-expr (syntax)
  (parens :initform 0 :parse t)
  print-type
  from-conversion
  (free-variables :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound)
  nonempty?)

;;(defcl type-variable (type-expr))

(defcl enumtype (inline-datatype))

(defcl type-name (type-expr name)
  adt
  uninterpreted?)

(defcl uninterpreted-type-name (type-name)) 

(defcl adt-type-name (type-name)
  adt
  (recognizer-names :ignore t :fetch-as nil)
  struct-name)

(defcl type-application (type-expr)
  (type :parse t)
  (parameters :parse t))


;;; Subtypes are of the form {x [: type] | expr},
;;; but also come in with bind-decls of the form (x [: type] | pred)
;;; The formals and formula are for this latter form, as the predicate is
;;; constructed from them.  Thus for (x:int | p(x)) the formals are (x: int)
;;; the formula is p(x), and the predicate is (LAMBDA (x: int): p(x))


;;; This is a mixin class

(defcl subtype (type-expr)
  (supertype :parse t)
  predicate)

(defcl datatype-subtype (subtype)
  declared-type)

;;; The optional supertype is given

(defcl setsubtype (subtype)
  (formals :parse t)
  (formula :parse t))

;;; For setsubtypes in which the supertype is not provided.

(defcl nsetsubtype (setsubtype))

;;; Parenthesized expressions used as types.  Simple-expr-as-type is used
;;; for print-types, and is what is parsed.  Expr-as-type is a combination
;;; (it may not be needed).

(defcl simple-expr-as-type (type-expr)
  (expr :parse t))

(defcl expr-as-type (subtype simple-expr-as-type))

;(defcl quotienttype (type-expr)
;  basetype
;  relation)

(defcl funtype (type-expr)
  (domain :parse t)
  (range :parse t))

(defcl functiontype (funtype))

(defcl arraytype (funtype))

(defcl tupletype (type-expr)
  (types :parse t)
  generated?)


;;; The domain-tupletype is a tupletype created for a function type, to
;;; distinguish between [a,b,c -> d] and [[a,b,c] -> d].

(defcl domain-tupletype (tupletype))


;;; This is used for converting to/from functions types of the form
;;;  [x:a, y:b -> c(x,y)]   and   [t:[a,b] -> c(PROJ_1(t),PROJ_2(t))]
;;; The var-bindings in this case are ((x . PROJ_1(t)) (y . PROJ_2(t)))

(defcl dep-domain-tupletype (domain-tupletype)
  (var-bindings :parse t))

(defcl recordtype (type-expr)
  (fields :parse t)
  ;;generated?
  dependent?)


(defcl eval-defn-info ()
  unary  ;;defn needed for returning functional values.
  multiary ;;fully applied form
  destructive) ;;destructive version of multiary form

(defcl eval-defn ()
  name
  definition
  output-vars) ;;These are the input vars that structure share with output.

(defcl eval-info ()
  internal  ;both are eval-defn-info 
  external)

(defcl destructive-eval-defn (eval-defn)
  side-effects) ;;alist of updated variables/live vars when updated.


;;; A proof-info object contains the information pertaining to a given
;;; proof.  A formula-decl has a list of these, and if there is a default
;;; proof, it is one of these.  The id is an optional identifier
;;; associated with the proof.  The description is an optional string
;;; describing the proof.  The script is the proof script or justification
;;; that is run to get the proof.  The status is one of PROVED, UNTRIED,
;;; UNFINISHED, or UNCHECKED.  The refers-to is a list of declarations
;;; that are referred to during the proof.  Real-time and run-time are
;;; times in internal time units reflecting the clock time and CPU time
;;; for the given proof.  Interactive? is T or NIL indicating whether the
;;; last attempt of this proof script was interactive or not.  The 

(defcl proof-info ()
  id
  description
  create-date
  run-date
  script
  status
  refers-to
  real-time
  run-time
  interactive?
  decision-procedure-used)

(defcl decl-reference ()
  id
  class
  type
  theory-id
  library)
