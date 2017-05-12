;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes-decl.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  2 13:40:37 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Jul  1 18:50:34 1999
;; Update Count    : 56
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

;;; This file defines most of the classes used in PVS.  Below is a
;;; summary, which leaves out many of the slots and some of the classes
;;; which are mostly useful for unparsing.  This list is mostly useful
;;; for defining methods.

;;; This uses the defcl macro, which is similar to defclass, but it
;;; always creates an :accessor and :initarg, and if an :initarg is
;;; not given, it defaults to nil rather than being unbound.  Besides
;;; generating a defclass, this also generates a predicate (the class
;;; name followed by '?') which may be used as a shorthand in place of
;;; typep.  It also updates the *slot-info* global variable, which is
;;; used by write-deferred-methods-to-file to create the
;;; pvs-methods.lisp file containing the copy, store-object*,
;;; update-fetched, and restore-object* methods.  The latter three are
;;; used solely for saving and restoring bin files.  There are also
;;; slot keywords that control the generated methods treatment of
;;; slots.  The :parse slot is currently not used; it is intended for
;;; a automatically generated untypecheck methods, but currently these
;;; have been developed manually.  The :ignore flag indicates that the
;;; slot is not to be copied in the copy method.  The :store-as
;;; indicates the function to be called when storing a given slot.  If
;;; not provided, store-object* is recursively called on the slot.  If
;;; nil, then it is stored as nil.  Otherwise the specified function
;;; is invoked when the bin file is created.  This is needed in order
;;; to not save session-dependent information, and to break some kinds
;;; of circularities.  The :fetch-as and :restore-as flags are
;;; similar, see save-theories.lisp for details.  Note that
;;; :restore-as nil simply means that there is no need to restore this
;;; slot, update-fetched did the job.

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

(in-package :pvs)

(export '(actual actuals adt-or-theory adtdecl application argument arguments 
	  assignment assuming bind-decl binding-expr bindings branch chain?
	  const-decl const-decl? constructors context conversion-decl
	  conversionplus-decl? conversions datatype-or-module declaration
	  declarations-hash declared-type definition domain enumtype enumtype
	  expr expression exprs field-application field-decl fields filename 
	  formal-const-decl formal-type-decl formals formals-sans-usings
	  formula-decl funtype generated-by id importing infix-application
	  info inline-datatype judgement? justification lambda-expr let-expr
	  modname module name name-expr nonempty-types nonempty? number
	  number-expr operator parens predicate print-type
	  projection-application quant-expr range recognizer record-expr
	  recordtype resolutions saved-context simple-constructor status
	  subtype supertype theory theory-name tuple-expr tupletype type
	  type-decl type-def-decl type-expr type-name type-name? type-value
	  typed-declaration types update-expr using-hash var-decl visible?))
#+(or cmu sbcl)
(#-sbcl ext:without-package-locks #+sbcl sb-ext:without-package-locks
 (defgeneric class (x))
 (defgeneric (setf class) (x y))
 (defgeneric keyword (x))
 (defgeneric (setf keyword) (x y)))

;;; So we can unparse a list of modules into a single file.

(defcl modules ()
  modules)

;;; The slots common to both datatypes and modules
;;; id, formals, assuming are set by the parser

(defcl datatype-or-module (syntax)
  (id :type symbol :parse t :restore-as nil)
  (formals :type list
	   :documentation "a list of formal-decls"
	   :parse t)
  (formals-sans-usings :type list
		       :documentation "a list of formal decls, without usings"
		       :fetch-as nil)
  (theory-formal-decls :type list
		       :documentation "Decls generated by a theory-formal-decl")
  (assuming :type list
	    :documentation "a list of declarations and usings"
	    :parse t)
  (filename :type (or null string)
	    :documentation "The filename sans directory or extension"
	    :restore-as nil)
  (status :type list
	  :documentation "A list containing the completed actions"
	  :restore-as nil)
  (generated-by :documentation "a module id" :restore-as nil)
  ;;generated-theories
  (tcc-comments :documentation
		"An alist of TCCs and comments that follow."
		:restore-as nil)
  (info :documentation
	"A list of information strings produced by calls to pvs-info"
	:restore-as nil)
  (warnings :documentation
	    "A list of warning strings produced by calls to pvs-info"
	    :restore-as nil)
  (conversion-messages :documentation
		       "A list of conversion message strings produced by calls to pvs-conversion-msg"
		       :restore-as nil)
  (all-declarations :fetch-as nil)
  (all-imported-theories :fetch-as 'unbound :initform 'unbound)
  (all-imported-names :fetch-as 'unbound :initform 'unbound)
  ;; places is restored, but after the theory part - see save-theories.lisp
  ;;(places :restore-as nil)
  )


;;; Datatypes and related classes

(defcl recursive-type (datatype-or-module)
  (importings :parse t)
  (constructors :documentation "a list of constructors"
		:parse t)
  (adt-type-name :restore-as nil)
  (positive-types :restore-as nil)
  (adt-theory :restore-as nil)
  (adt-map-theory :restore-as nil)
  (adt-reduce-theory :restore-as nil)
  (generated-file-date :restore-as nil)
  (semi :parse t :restore-as nil))

(defcl recursive-type-with-subtypes (recursive-type)
  subtypes)

(defcl inline-recursive-type (recursive-type)
  (decl-formals :parse t)
  module
  (generated :restore-as nil)
  (typechecked? :restore-as nil))

;;; Datatypes

(defcl datatype (recursive-type))

(defcl inline-datatype (inline-recursive-type datatype))

(defcl datatype-with-subtypes (recursive-type-with-subtypes datatype))

;;; Library classes

(defcl library-datatype-or-theory (datatype-or-module)
  (lib-ref :documentation
	   "The canonical form of the library path - if relative, then it is
            relative to the current context"
	   :restore-as nil))

(defcl library-recursive-type (library-datatype-or-theory))

(defcl library-datatype (datatype library-recursive-type))

(defcl inline-datatype-with-subtypes (inline-datatype datatype-with-subtypes))

(defcl library-datatype-with-subtypes (library-datatype datatype-with-subtypes))

;;; Codatatypes

(defcl codatatype (recursive-type))

(defcl inline-codatatype (inline-recursive-type codatatype))

(defcl codatatype-with-subtypes (codatatype recursive-type-with-subtypes))

(defcl library-codatatype (codatatype library-recursive-type))

(defcl inline-codatatype-with-subtypes (inline-codatatype codatatype-with-subtypes))

(defcl library-codatatype-with-subtypes (library-codatatype codatatype-with-subtypes))

(defcl adt-constructor (syntax)
  (recognizer :type symbol :parse t)
  (ordnum :type (or null fixnum)
	  :restore-as nil)
  generated-by)

(defcl constructor-with-subtype (simple-constructor)
  subtype)


;;; This should be merged with the above, since there are no
;;; record-constructors anymore

(defcl simple-constructor (adt-constructor)
  (id :type symbol :parse t :restore-as nil)
  (arguments :documentation "a list of adtdecls" :parse t)
  con-decl
  rec-decl
  acc-decls)


;;; Modules

(defcl module (datatype-or-module)
  (theory :type list :parse t) ; The declarations of the theory-part
  (exporting :parse t)  ; A list of exportings
  nonempty-types  ; Keep track of types marked nonempty during typechecking
  all-usings ; The transitive closure of the usings of the theory
  (immediate-usings :initform 'unbound) ; immediate usings of the theory
  instances-used
  assuming-instances
  used-by
  saved-context
  dependent-known-subtypes ; Those that reference the theory parameters
  (macro-expressions :restore-as nil)
  (macro-subtype-tcc-args-alist :restore-as nil)
  (tccs-tried? :restore-as nil)
  (modified-proof? :restore-as nil)
  (tcc-info :type list :initform (list 0 0 0 0) :restore-as nil)
  (ppe-form :fetch-as nil)
  (tcc-form :fetch-as nil)
  (typecheck-time :restore-as nil))

(defcl rectype-theory (module)
  ;; The module instances generated from a non-inline recursive type
  positive-types ; speeds up tc-eq, and allows restore-object* to work
                 ; properly with store-print-types
  single-constructor?)


(defcl library-theory (module library-datatype-or-theory))

(defcl library-rectype-theory (library-theory rectype-theory))

(defcl exporting (syntax)
  (names :documentation "a list of names and absexpnames" :parse t)
  (but-names :documentation "a list of names and absexpnames" :parse t)
  (kind :documentation "One of NIL, ALL, CLOSURE, or DEFAULT" :parse t)
  (modules :documentation "a list of modnames" :parse t)
  (closure :documentation "The closure of the list of modules"))

(defcl expname (syntax)
  (id :parse t :restore-as nil)
  (kind :parse t :restore-as nil)
  type)

;; A wrapper for entities that import (explicitly or implicitly)
;; Currently importing, formal-theory-decl, mod-decl, theory-abbreviation-decl
;; The saved-context is the context including the imported theories.
(defcl importing-entity (syntax)
  saved-context)

(defcl importing (importing-entity)
  (theory-name :parse t)
  (semi :parse t :restore-as nil)
  (chain? :parse t :restore-as nil)
  (refers-to :restore-as nil)
  (generated :restore-as nil)
  (generated-by :restore-as nil)
  (tcc-form :fetch-as nil :ignore t))

;;; DECLARATION Classes.  Many of these have both a declared-type and a
;;; type slot.  The declared-type is set by the parser and used by the
;;; unparser.  The type is set by the typechecker to the canonical value
;;; of the declared-type.

(
 #-sbcl progn
 #+sbcl sb-ext:without-package-locks
(defcl declaration (syntax)
  (newline-comment :restore-as nil)
  (id :type (or symbol number) :parse t :restore-as nil)
  (decl-formals :type list :parse t)
  (formals :type list :parse t)
  (module :restore-as nil)
  (refers-to :type list :restore-as nil)
  ;;(referred-by :type list :fetch-as nil)
  (chain? :type symbol :parse t :restore-as nil)
  (typechecked? :type symbol :restore-as nil)
  (visible? :type symbol :restore-as nil)
  (generated :documentation "a list of declarations" :restore-as nil)
  (generated-by :documentation "a declaration instance" :restore-as nil)
  (semi :parse t :restore-as nil)
  (tcc-form :fetch-as nil :ignore t)
  (typecheck-time :restore-as nil))
)

;; A mixin
(defcl mapped-decl ())

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
  (type-value :store-as ignore-self-reference-type-values)
  ir-type-value)

(#-sbcl progn #+sbcl sb-ext:without-package-locks
(defcl nonempty-type-decl (type-decl)
  (keyword :restore-as nil))
)

;;; A mixin
(defcl type-def-decl (type-decl)
  (type-expr :parse t)
  (contains :parse t))

;;; A mixin
(defcl nonempty-type-def-decl (type-def-decl nonempty-type-decl))

(defcl type-eq-decl (type-def-decl))

(defcl mapped-type-decl (type-eq-decl mapped-decl))

(defcl nonempty-type-eq-decl (type-eq-decl nonempty-type-def-decl))

(defcl type-from-decl (type-def-decl)
  predicate
  supertype)

(defcl nonempty-type-from-decl (type-from-decl nonempty-type-def-decl))

(defcl struct-subtype-decl (type-def-decl)
  projection
  supertype)

(defcl nonempty-struct-subtype-decl (struct-subtype-decl nonempty-type-def-decl))


;;; At the moment, units-appls (and hence units-exprs) are not type-exprs
;;; This may change in the future, but creates parser headaches
;;; Thus units-appls may only appear in units-decls
(defcl units-decl (type-decl)
  declared-units
  offset)

(defcl units-expr (syntax)
  (parens :initform 0 :parse t :restore-as nil)
  scale
  dimensionality)

(defcl units-appl (units-expr)
  operator
  arguments)

(defcl units-name (type-name units-expr))


;;; Formal theory parameter declarations

(defcl formal-decl (declaration)
  (dependent? :restore-as nil))

(defcl decl-formal (formal-decl))

(defcl formal-type-decl (formal-decl type-decl typed-declaration)
  )

(defcl decl-formal-type (formal-type-decl decl-formal)
  associated-decl ; set if this is for a declaration parameter
  )

(defcl formal-nonempty-type-decl (formal-type-decl nonempty-type-decl))

(defcl decl-formal-nonempty-type (decl-formal-type nonempty-type-decl))

(defcl formal-subtype-decl (formal-type-decl type-from-decl))

(defcl decl-formal-subtype (decl-formal-type formal-subtype-decl))

(defcl formal-nonempty-subtype-decl (formal-subtype-decl nonempty-type-decl))

(defcl decl-formal-nonempty-subtype (decl-formal-subtype formal-nonempty-subtype-decl))

(defcl formal-struct-subtype-decl (formal-type-decl struct-subtype-decl))

(defcl decl-formal-struct-subtype (decl-formal-type formal-struct-subtype-decl))

(defcl formal-nonempty-struct-subtype-decl (formal-struct-subtype-decl nonempty-type-decl))

(defcl decl-formal-nonempty-struct-subtype (decl-formal-struct-subtype
					    formal-nonempty-struct-subtype-decl))

(defcl formal-type-appl-decl (formal-type-decl)
  (parameters :parse t))

(defcl decl-formal-type-appl (decl-formal-type formal-type-appl-decl))

(defcl formal-nonempty-type-appl-decl (formal-type-appl-decl
				       nonempty-type-decl))

(defcl decl-formal-nonempty-type-appl (decl-formal-type-appl
				       formal-nonempty-type-appl-decl))

(defcl formal-const-decl (formal-decl typed-declaration)
  (possibly-empty-type? :restore-as nil))

(defcl formal-theory-decl (formal-decl importing-entity)
  (theory-name :parse t)
  theory-mappings
  other-mappings)

(defcl adtdecl (typed-declaration)
  (bind-decl :documentation "Keeps a corresponding bind-decl")
  (accessor-decl :documentation "The corresponding accessor-decl - may be shared"))

(defcl lib-decl (declaration)
  (lib-string :parse t :restore-as nil)
  (lib-ref :restore-as nil))

(defcl lib-eq-decl (lib-decl))

(defcl theory-reference (declaration importing-entity))

(defcl mod-decl (theory-reference)
  (modname :parse t)
  theory-mappings
  other-mappings)

(defcl mapped-mod-decl (mod-decl mapped-decl))

(defcl theory-abbreviation-decl (theory-reference)
  (theory-name :parse t))

(defcl var-decl (typed-declaration))

;; if from-object-decl? = t then the declaration is an object-decl.
;; ada-generated-params records the parameters generated for an ada 
;; subprogram spec, so that same parameters are used for the subprogram 
;; body.

(defcl const-decl (typed-declaration)
  (definition :parse t)
  def-axiom
  (positive-types :initform :none)
  (eval-info :fetch-as nil))

(defcl macro-decl (const-decl))

;;; mapped-decls are created for mappings, e.g.,
;;; importing th {{c := 1}} creates a mapped-decl that is added to the context
;;; Always has a definition
(defcl mapped-const-decl (macro-decl))

(defcl adt-constructor-decl (const-decl)
  (ordnum :restore-as nil))

(defcl adt-recognizer-decl (const-decl)
  (ordnum :restore-as nil))

(defcl adt-accessor-decl (const-decl))

(defcl shared-adt-accessor-decl (adt-accessor-decl)
  constructors)

(defcl def-decl (const-decl)
  (declared-measure :parse t)
  (ordering :parse t)
  measure
  (measure-depth :restore-as nil)
  recursive-signature)

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
;;;   default-proof The default proof-info (it is a member of proofs)
;;;   proofs      The list of proof-info instances

(defcl formula-decl (declaration)
  (spelling :documentation "One of formula, axiom, lemma, etc."
	    :parse t
	    :restore-as nil)
  (definition :parse t)
  ;; The universal closure of the definition, used in create-formulas
  (closed-definition :fetch-as nil)
  (kind :restore-as nil)
  (default-proof :restore-as nil)
  (proofs :restore-as nil))

(defcl assuming-decl (formula-decl)
  original-definition)

(defcl tcc-decl (formula-decl)
  (origin :type list
	  :documentation "list of root-id, kind, expr, expected, and place,
saved by TCC generation, and with proof files.  Later, whenever the proof file is
restored, the TCCs are checked")
  (tcc-disjuncts
   :documentation "The disjuncts of the definition used for TCCs"
   :restore-as nil)
  importing-instance)

(defcl mapped-formula-decl (formula-decl)
  from-formula)

(defcl mapped-assuming-decl (assuming-decl mapped-formula-decl))

(defcl mapped-tcc-decl (tcc-decl mapped-formula-decl))

(defcl subtype-tcc (tcc-decl))

(defcl termination-tcc (tcc-decl))

(defcl judgement-tcc (subtype-tcc))

(defcl recursive-judgement-tcc (judgement-tcc))

(defcl recursive-judgement-axiom (formula-decl))

(defcl existence-tcc (tcc-decl))

(defcl assuming-tcc (tcc-decl)
  theory-instance
  (generating-assumption :restore-as nil))

(defcl mapped-axiom-tcc (tcc-decl)
  theory-instance
  (generating-axiom :restore-as nil))

(defcl mapped-eq-def-tcc (tcc-decl))

(defcl cases-tcc (tcc-decl))

(defcl well-founded-tcc (tcc-decl))

(defcl same-name-tcc (tcc-decl))

(defcl cond-disjoint-tcc (tcc-decl))

(defcl cond-coverage-tcc (tcc-decl))

(defcl monotonicity-tcc (tcc-decl))

;;; judgement class is a mixin
(defcl judgement (typed-declaration)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameter-theories :ignore t :initform 'unbound :fetch-as 'unbound))

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

(defcl expr-judgement (judgement)
  (expr :parse t)
  ;; closed-form is a list, or a forall-expr, whise expression is a list
  ;; the list consists of an expr and a type, representing the has_type relation,
  ;; But the bindings cover all veriables of the expr and type
  closed-form 
  judgement-type)
  

;; The rewrite-formula is generated by typecheck*
;; (rec-application-judgement) It is what would have been generated as the
;; judgement TCC formula if "RECURSIVE" was omitted.  Saved here because it
;; is difficult to get at later, and is used for judgement-rewriting.
(defcl rec-application-judgement (application-judgement)
  rewrite-formula)

(defcl conversion-decl (declaration)
  (k-combinator? :restore-as nil)
  (expr :parse t)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameter-theories :ignore t :initform 'unbound :fetch-as 'unbound))

;;; Same as conversion-decl, but prints differently
(defcl conversionplus-decl (conversion-decl))

;;; Not only prints differently, but removes the conversion(s)
(defcl conversionminus-decl (conversion-decl))

(defcl auto-rewrite-decl (declaration)
  (rewrite-names :parse t))

;;; Same as auto-rewrite-decl, but prints differently
(defcl auto-rewrite-plus-decl (auto-rewrite-decl))

;;; Not only prints differently, but removes the auto-rewrite(s)
(defcl auto-rewrite-minus-decl (auto-rewrite-decl))

;;; Rewrites

(defcl rewrite-elt (syntax))

(defcl rewrite-name (rewrite-elt name)) ; A mixin

(defcl lazy-rewrite (rewrite-elt)) ; A mixin

(defcl eager-rewrite (rewrite-elt)) ; A mixin

(defcl macro-rewrite (rewrite-elt)) ; A mixin

(defcl constant-rewrite-name (rewrite-name) ; A mixin
  declared-type
  type)

(defcl formula-rewrite-name (rewrite-name) ; A mixin
  (spelling :restore-as nil))

(defcl lazy-rewrite-name (rewrite-name lazy-rewrite))

(defcl eager-rewrite-name (rewrite-name eager-rewrite))

(defcl macro-rewrite-name (rewrite-name macro-rewrite))

(defcl lazy-constant-rewrite-name (constant-rewrite-name lazy-rewrite-name))

(defcl lazy-formula-rewrite-name (formula-rewrite-name lazy-rewrite-name))

(defcl eager-constant-rewrite-name (constant-rewrite-name eager-rewrite-name))

(defcl eager-formula-rewrite-name (formula-rewrite-name eager-rewrite-name))

(defcl macro-constant-rewrite-name (constant-rewrite-name macro-rewrite-name))

(defcl macro-formula-rewrite-name (formula-rewrite-name macro-rewrite-name))

(defcl fnum-rewrite (rewrite-elt)
  fnum)

(defcl lazy-fnum-rewrite (fnum-rewrite lazy-rewrite))

(defcl eager-fnum-rewrite (fnum-rewrite eager-rewrite))

(defcl macro-fnum-rewrite (fnum-rewrite macro-rewrite))


;;; Type Expressions

(defcl type-expr (syntax)
  (parens :initform 0 :parse t :restore-as nil)
  print-type
  ghost?
  from-conversion
  (free-variables :ignore t :initform 'unbound :fetch-as 'unbound)
  (free-parameters :ignore t :initform 'unbound :fetch-as 'unbound)
  (nonempty? :restore-as nil))

;;(defcl type-variable (type-expr))

(defcl enumtype (inline-datatype))

(defcl type-name (type-expr name)
  ;;adt
  )

(defcl uninterpreted-type-name (type-name)) 

(defcl adt-type-name (type-name)
  adt
  single-constructor? ; Speeds things up slightly, and allows restore-object*
		      ; to work for store-print-types
  (recognizer-names :ignore t :fetch-as nil)
  struct-name)

(defcl type-application (type-expr)
  (type :parse t)
  (parameters :parse t))


(defcl type-var (type-name)) ;; The mixin

;; projection-expr proj_i (or `i) is of type [tupT -> T_i]
;; tupT is a tup-type-variable
;; T_i is a proj-type-variable
;; tupT points to T_i, used in tc-unify

(defcl tup-type-variable (type-var)
  proj-type-var)

(defcl proj-type-variable (type-var)
  (index :type fixnum :parse t :restore-as nil))

;; fieldex `a is of type [recT -> T_a]
;; recT is a rec-type-variable
;; T_a is a field-type-variable
;; recT points to T_a, used in tc-unify

(defcl rec-type-variable (type-var)
  field-type-var)

(defcl field-type-variable (type-var)
  (field-id :restore-as nil))

;; extraction-expr out_i is of type [coT -> T_i]
;; coT is a cotup-type-variable - maps to (in?_i)
;; T_i is a out-type-variable
;; coT points to T_i, used in tc-unify

(defcl cotup-type-variable (type-var)
  out-type-var)

(defcl out-type-variable (type-var)
  (index :type fixnum :parse t :restore-as nil))

;; This doesn't work - no way to figure out the cotuple type
;; instead generate type-error, saying to include actuals.
    ;; injection-expr in_i is of type [T_i -> coT]
    ;; T_i is an in-type-variable
    ;; coT is a cotup-in-variable - maps to (in?_i)
    ;; T_i points to coT, used in tc-unify

    ;; (defcl in-type-variable (type-var)
    ;;   cotup-var)

    ;; (defcl cotup-in-variable (type-var)
    ;;   (index :type fixnum :parse t :restore-as nil))


;;; Subtypes are of the form {x [: type] | expr},
;;; but also come in with bind-decls of the form (x [: type] | pred)
;;; The formals and formula are for this latter form, as the predicate is
;;; constructed from them.  Thus for (x:int | p(x)) the formals are (x: int)
;;; the formula is p(x), and the predicate is (LAMBDA (x: int): p(x))


;;; This is a mixin class

(defcl subtype (type-expr)
  (supertype :parse t)
  (top-type :fetch-as nil :ignore t)
  predicate
  (subtype-conjuncts :ignore t))

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

(defcl struct-subtype (type-expr)
  generated-by)

(defcl functiontype (funtype))

(defcl arraytype (funtype))

(defcl tuple-or-struct-subtype (type-expr)
  ;; A mixin for making common methods
  (types :type list :parse t)
  (generated? :restore-as nil))

(defcl tupletype (tuple-or-struct-subtype))

(defcl struct-sub-tupletype (struct-subtype tuple-or-struct-subtype)
  type)

(defcl cotupletype (type-expr)
  (types :type list :parse t)
  (generated? :restore-as nil))


;;; The domain-tupletype is a tupletype created for a function type, to
;;; distinguish between [a,b,c -> d] and [[a,b,c] -> d].

(defcl domain-tupletype (tupletype))


;;; This is used for converting to/from functions types of the form
;;;  [x:a, y:b -> c(x,y)]   and   [t:[a,b] -> c(PROJ_1(t),PROJ_2(t))]
;;; The var-bindings in this case are ((x . PROJ_1(t)) (y . PROJ_2(t)))

(defcl dep-domain-tupletype (domain-tupletype)
  (var-bindings :parse t))

(defcl record-or-struct-subtype (type-expr)
  ;; A mixin for making common methods
  (fields :type list :parse t)
  (dependent? :restore-as nil))

(defcl recordtype (record-or-struct-subtype))

(defcl struct-sub-recordtype (struct-subtype record-or-struct-subtype)
  type)

(defcl type-extension (type-expr)
  (type :parse t)
  (extension :parse t))

(defcl binding-type (type-expr)
  (bindings :parse t)
  (type :parse t))

(defcl quant-type (binding-type))

(defcl forall-type (quant-type))

(defcl exists-type (quant-type))

;;; This is used solely by save-theories.  The idea is to store the
;;; print-type of a type whenever it appears, and to recreate the
;;; canonical form when restoring.  This ensures that eq structures
;;; are created whenever possible.
(defcl store-print-type ()
  print-type
  (type :fetch-as nil))


(defcl eval-defn-info ()
  unary  ;;defn needed for returning functional values.
  multiary ;;fully applied form
  destructive) ;;destructive version of multiary form

(defcl eval-defn ()
  name
  definition
  output-vars) ;;These are the input vars that structure share with output.

(defcl eval-info ()
  ir
  cdefn
  c-type-info-table
  internal  ;both are eval-defn-info 
  external)

(defcl eval-type-info ()
  ir-type-name
  c-type-info
  c-type-info-table)

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
  run-date ;; Not saved
  script
  status ;; Not saved - in the .pvscontext
  refers-to
  real-time ;; Not saved
  run-time ;; Not saved
  interactive? ;; Not saved
  decision-procedure-used)

;; subtype expr expected
;; termination-subtype expr expected
;; recursive name arguments ex
;; well-founded decl mtype
;; existence type expr
;; assuming modinst expr
;; mapped-eq-def lhs rhs mapthinst
;; mapped-axiom modinst
;; cases constructors expr adt
;; actuals act mact
;; cond-disjoint expr conditions values
;; cond-coverage expr conditions
;;

(defcl tcc-origin ()
  root
  kind
  expr
  type)

(defcl tcc-proof-info (proof-info)
  origin)

(
 #-sbcl progn
 #+sbcl sb-ext:without-package-locks
(defcl decl-reference ()
  id
  class
  type
  theory-id
  library)
)
