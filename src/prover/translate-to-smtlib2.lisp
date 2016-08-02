;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smtlib2.lisp -- 
;; Author          : T. King
;; Created On      : Jul. 18, 2012
;; Last Modified By: T. King
;; Last Modified On: 
;; Update Count    : 
;; Status          : Testing?
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

;;TK(07/18/12): This is a page 1 rewrite of translate-to-yices2.lisp and translate-to-yices.lisp to support single query smtlib2.


;; (lisp (load "~/ws/pvs/src/prover/translate-to-smtlib2.lisp"))
;; (load "~/ws/pvs/src/prover/translate-to-smtlib2.lisp")
;; :bt
;; :dn <X>
;; :eval :context t
;; <VARIABLE>

(defpackage :smt-translator
  (:use :pvs :common-lisp)
  (:export :smt
	   :smt-with-rewrites 
	   :smt-with-rewrites$
	   :smt-simp
	   :smt-simp$
	   :smt-grind
	   :smt-grind$ ))
(in-package :smt-translator)

(import 'smt :pvs)
(import 'smt-with-rewrites :pvs)
(import 'smt-grind :pvs)

(import 'pvs::newcounter)
(import 'pvs::make-pvs-hash-table)
(import 'pvs::select-seq)
(import 'pvs::current-goal)
(import 'pvs::s-forms)
(import 'pvs::negate)
(import 'pvs::defstep)
(import 'pvs::then)
(import 'pvs::simplify-with-rewrites)
(import 'pvs::install-rewrites$)
(import 'pvs::repeat*)
(import 'pvs::bash$)
(import 'pvs::inst?)
(import 'pvs::skosimp*)

(import 'pvs::addrule)
(import 'pvs::formula)
(import 'pvs::make-projection-application)
(import 'pvs::name-expr?)
(import 'pvs::enum-adt?)
(import 'pvs::constructor?)
(import 'pvs::module-instance)
(import 'pvs::type-constraints)
(import 'pvs::application?)
(import 'pvs::dep-binding?)
(import 'pvs::simple-below?)
(import 'pvs::number-expr?)
(import 'pvs::pvs-tmp-file)
(import 'pvs::label)
(import 'pvs::working-directory)
(import 'pvs::file-contents)
(import 'pvs::format-if)
(import 'pvs::same-declaration)
(import 'pvs::variable?)
(import 'pvs::tupletype?)
(import 'pvs::recognizer?)
(import 'pvs::tuple-expr?)
(import 'pvs::unary-application?)
(import 'pvs::args1)
(import 'pvs::args2)
(import 'pvs::exists-expr?)
(import 'pvs::forall-expr?)
(import 'pvs::lambda-expr?)
(import 'pvs::datatype?)
(import 'pvs::and+)
(import 'pvs::memq)

(import 'pvs::=)
(import 'pvs::/=)
(import 'pvs::TRUE)
(import 'pvs::FALSE)
(import 'pvs::IMPLIES)
(import 'pvs::=>)
(import 'pvs::IFF)
(import 'pvs::AND)
(import 'pvs::OR)
(import 'pvs::NOT)
(import 'pvs::+)
(import 'pvs::-)
(import 'pvs::*)
(import 'pvs::/)
(import 'pvs::O)
(import 'pvs::<)
(import 'pvs::<=)
(import 'pvs::>)
(import 'pvs::>=)
(import 'pvs::&)
(import 'pvs::XOR)
(import 'pvs::^)
(import 'pvs::sign_extend)


;(import '(= /= TRUE FALSE IMPLIES => IFF AND OR NOT + - * / O < <= > >= & XOR ^ sign_extend))
;(import '(|bv_slt| |bv_sle| |bv_sgt| |bv_sge| |bv_splus| |bv_stimes|) :pvs)


;; From the smt-lib2 documentation:
;; Commands 
;; These keywords have a predefined meaning in Version 2.0.
;; :all-statistics, :authors, :axioms, :chainable, :definition,
;; :diagnostic-output-channel, :error-behavior :expand-definitions, :extensions,
;; :funs, :funs-description, :interactive-mode, :language, :left-assoc, :name, :named,
;; :notes, :print-success, :produce-assignments, :produce-models, :produce-proofs,
;; :produce-unsat-cores, :random-seed, :reason-unknown, :regular-output-channel,
;; :right-assoc, :sorts, :sorts-description, :status, :theories, :values, :verbosity,
;; :version.
;; Reserved Words
;; General: !, , as, DECIMAL, exists, forall, let, NUMERAL, par, STRING.
;; Command names: assert, check-sat, declare-sort, declare-fun, define-sort,
;; define-fun, exit, get-assertions, get-assignment, get-info, get-option,
;;get-proof, get-unsat-core, get-value, pop, push, set-logic, set-info, set-option.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                        Solvers                          ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deftype solvers () '(member z3 cvc4 mathsat5 opensmt stp))

(defun generate-smt-solver-call-string (solver fname)
  "Generates a valid call string for an smt solver.
  Eventually this will also add option flags to each of the solvers."
  (cond ((typep solver :cvc4) (format t "/Users/shankar/Downloads/CVC4/builds/bin/cvc4 ~a" fname))
	((typep solver :z3) (format "z3 ~a" fname))))

;; (lisp (load "~/ws/pvs/src/prover/translate-to-yices2.lisp"))
;; :bt
;; :dn <X>
;; :eval :context t
;; <VARIABLE>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                          Logic                          ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass smt-features ()
  ((quantified :accessor quantified? :initform nil)
   (uninterpreted-functions :accessor uninterpreted-functions? :initform nil)
   (arrays-ex :accessor arrays-ex? :initform nil)
   (inductive-datatypes :accessor inductive-datatypes? :initform nil)
   (bitvectors :accessor bitvectors? :initform nil)
   (arithmetic :accessor arithmetic? :initform nil)
   (real-arithmetic :accessor real-arithmetic? :initform nil)
   (integer-arithmetic :accessor integer-arithmetic? :initform nil)
   (nonlinear-arithmetic :accessor nonlinear-arithmetic? :initform nil)
   (strictly-real-linear-terms-appear :accessor strictly-real-linear-terms-appear? :initform nil)
   (frozen :accessor frozen? :initform nil) )
  (:documentation "This corresponds to the features that were used in the generation in the current smt query.
Knowing these features in advance is important for deciding which solvers can be used."))

(defmethod linear-arithmetic? ((f smt-features))
  (null (linear-arithmetic? f)))

(defun make-empty-smt-features ()
  (make-instance 'smt-features))

(defmethod reset-to-empty-features ((l smt-features))
  (setf l (make-empty-smt-features)))

(deftype official-smt-logics ()
  '(member
    AUFLIA
    AUFLIRA
    AUFNIRA
    LRA
    QF_ABV
    QF_AUFBV
    QF_AUFLIA
    QF_AX
    QF_BV
    QF_IDL
    QF_LIA
    QF_LRA
    QF_NIA
    QF_NRA
    QF_RDL
    QF_UF
    QF_UFIDL
    QF_UFBV
    QF_UFLIA
    QF_UFLRA
    QF_UFNRA
    UFLRA
    UFNIA))

(defparameter *official-smt-logics-to-solvers*
  '((AUFLIA (cvc4 z3))
    (AUFLIRA (cvc4 z3))
    (AUFNIRA (z3))
    (LRA (cvc4 z3))
    (QF_ABV (cvc4 stp z3))
    (QF_AUFBV (cvc4 z3))
    (QF_AUFLIA (cvc4 z3))
    (QF_AX (cvc4 mathsat5 z3))
    (QF_BV (cvc4 stp opensmt z3))
    (QF_IDL (cvc4 mathsat5 opensmt z3))
    (QF_LIA (cvc4 mathsat5 z3))
    (QF_LRA (cvc4 mathsat5 z3))
    (QF_NIA (z3))
    (QF_NRA (z3))
    (QF_RDL (cvc4 mathsat5 opensmt z3))
    (QF_UF (cvc4 opensmt mathsat5 z3))
    (QF_UFBV (cvc4 stp mathsat5 z3))
    (QF_UFIDL (cvc4 mathsat5 opensmt z3))
    (QF_UFLIA (cvc4 mathsat5 z3))
    (QF_UFLRA (cvc4 opensmt mathsat5 z3))
    (QF_UFNRA (z3))
    (UFLRA (cvc4 z3))
    (UFNIA (cvc4 z3))))

(defparameter *supports-all* '(cvc4 z3))


(defun official-smt-logic-to-string (l)
  (cond ((typep l :AUFLIA) "AUFLIA")
	((typep l :AUFLIRA) "AUFLIRA")
	((typep l :AUFNIRA) "AUFNIRA")
	(t nil)))

(defmethod features-to-official-smt-logic ((l smt-features))
  (let ((q (quantified? l))
	(u (uninterpreted-functions? l))
	(ae (arrays-ex? l))
	(idt (inductive-datatypes? l))
	(bv (bitvectors? l))
	(ra (real-arithmetic? l))
	(ia (integer-arithmetic? l))
	(lin (linear-arithmetic? l)))
    (cond ((and q u ae ia lin (not (or idt bv ra))) :AUFLIA)
	  ((and q u ae ra ia lin (not (or idt bv))) :AUFLIRA)
	  ((and q u ae ra ia (not (or idt bv lin))) :AUFNIRA)
	  ((and q ra lin (not (or u ae idt bv ia))) :LRA)
	  ((and ae bv (not (or q u idt ra ia lin))) :QF_ABV)
	  ((and ae u bv (not (or q idt ra ia lin))) :QF_AUFBV)
	  ((and ae u ia lin (not (or q idt bv ra))) :QF_AUFLIA)
	  ((and ae (not (or q u idt bv ra ia lin))) :QF_AX)
	  ((and bv (not (or q u ae idt ra ia lin))) :QF_BV)
	  ((and ia lin (not (or q u ae idt bv ra))) :QF_LIA)
	  ((and ra lin (not (or q u ae idt bv ia))) :QF_LRA)
	  ((and ia (not (or q u ae idt bv ra lin))) :QF_NIA)
	  ((and ra (not (or q u ae idt bv ia lin))) :QF_NRA)
	  ((and u (not (or q ae idt bv ra ia lin))) :QF_UF)
	  ((and u bv (not (or q ae idt ra ia lin))) :QF_UFBV)
	  ((and u ia lin (not (or q ae idt bv ra))) :QF_UFLIA)
	  ((and u ra lin (not (or q ae idt bv ia))) :QF_UFLRA)
	  ((and u ra (not (or q ae idt bv ia lin))) :QF_UFNRA)
	  ((and q ra lin (not (or u ae idt bv ia))) :UFLRA)
	  ((and q u ia (not (or ae idt bv ra lin))) :UFNIA)
	  (t nil))))

(defmethod official-smt-logic? ((f smt-features))
  (if (features-to-official-smt-logic f) t nil))

(defmethod set-logic-string ((f smt-features) solver)
  (if (official-smt-logic? f)
      (format nil "(set-logic ~a)~%(set-info :smt-lib-version 2.0)"
	      (official-smt-logic-to-string (features-to-official-smt-logic f)))
      (if (typep solver :cvc4)
	  (format nil "(set-logic ALL) ;using cvc4-specific logic string for the non-official logic ~a" f)
	  (format nil ";the logic in use is non-standard ~a" f))))

(defmethod select-solver-by-features ((f smt-features) &optional preferred-solver)
  (let* ((logic (features-to-official-smt-logic f))
	 (off-supports (cdr (assoc logic *official-smt-logics-to-solvers*)))
	 (supports (or off-supports *supports-all*)))
    (if (and preferred-solver (memq preferred-solver supports))
	preferred-solver
	(let* ((result (car supports))
	       (warning (format t "selecting the solver ~a instead of ~a as to handle the features ~a" result preferred-solver f)))
	  (push-warning warning)
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                 GLOBAL VARIABLES                        ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *smt-features* (make-empty-smt-features)
  "This represents the current logic. This may or may not be an official logic.")

(defvar *smt-name-hash* (make-pvs-hash-table)
  "Makes sure every variable has a unique name.")

(defvar *translate-to-smt-hash* (make-pvs-hash-table)
  "Cache for expression translation.")

(defvar *expr-to-smt-type* (make-pvs-hash-table)
  "This maps every pvs expression to the corresponding smt type. The type always lives in *smt-type-declarations*")

;; When the file is produced, it is always in the order:
;; *smt-prefix*
;; *smt-type-declarations*
;; *smt-uninterpreted-constants*
;; *smt-assertions*
;; (check-sat)

(defvar *smt-prefix* nil
  "The prefix includes hints to solvers (set-logic ...) hints to readers, etc.")

(defvar *pvs-type-to-smt-type* (make-pvs-hash-table)
  "This maps type in pvs to the corresponding smt-type.")

(defvar *smt-type-declarations* nil
"A list of all of the types in current use.  Depending on the type a subset of these will be printed out.
This is printed in reverse order.")

(defvar *smt-uninterpreted-constants* nil
  "A list of uninterpreted constant symbols. This can include datatypes, functions, and constants.
This is printed in reverse order.")
(defvar *smt-assertions* nil
  "A list of assertions to an smt query.")

(defvar *smt-warnings* nil)
  
(defvar *smt-id-counter*)  ;;needs to be initialized in eproofcheck
(newcounter *smt-id-counter*)

(defparameter *smt-interpreted-names*
  '((=  (|equalities| . =))
    (/=  (|notequal| . distinct))
    (TRUE (|booleans| . true))
    (FALSE (|booleans| . false))
    (IMPLIES  (|booleans| . =>))
    (=>  (|booleans| . =>))
    (IFF (|booleans| . =))
    (AND (|booleans| . and) (|bv_bitwise| . bvand))
    (OR  (|booleans| . or) (|bv_bitwise| . bvor))
    (NOT  (|booleans| . not) (|bv_bitwise| . bvnot))
    (+  (|number_fields| . +) (|bv_arith_nat| . bvadd))
    (- (|number_fields| . -) (|bv_arithmetic_defs| . bvsub))
    (*   (|number_fields| . *) )
    (/  (|number_fields| . /))
    (rem (|modulo_arithmetic| . mod))
    (ndiv (|modulo_arithmetic| . div))
    (< (|reals| . <)(|bv_arith_nat| . bvult))
    (<=  (|reals| . <=)(|bv_arith_nat| . bvule))
    (> (|reals| . >)(|bv_arith_nat| . bvugt))
    (>=  (|reals| . >=)(|bv_arith_nat| . bvuge))
    (O (|bv_concat_def| . concat))
    (& (|booleans| . and)(|bv_bitwise| . bvand))
    (XOR  (|bv_bitwise| . bvxor))
    (^ (|bv_caret| .  extract)) ;; TODO : parameterize this correctly
    (sign_extend   (|bv_extend| . sign_extend)) ;; TODO : parameterize this correctly
    (|bv_slt| (|bv_arithmetic_defs| . bvslt))
    (|bv_sle| (|bv_arithmetic_defs| . bvsle))
    (|bv_sgt| (|bv_arithmetic_defs| . bvsgt))
    (|bv_sge| (|bv_arithmetic_defs| . bvsge))
    (|bv_splus| (|bv_arithmetic_defs| . bvadd))
    (|bv_stimes| (|bv_arithmetic_defs| . bvmul)) ;; TODO : check semantics
    ;;
    ))
(defparameter *non-linear-arithmetic-operators*
  '(* / mod div))

(defun is-non-linear-operator? (intrepeted-name)
  (memq intrepeted-name *non-linear-arithmetic-operators*))


;; SMT2 commands that were handled by the yices 2 input:
;;    (bvneg (_ BitVec m) (_ BitVec m))
;;    (bvmul (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvudiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvurem (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvshl (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvlshr (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvnand (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvnor (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvxnor (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvcomp (_ BitVec m) (_ BitVec m) (_ BitVec 1))
;;    (bvsub (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvsdiv (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvsrem (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvsmod (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    (bvashr (_ BitVec m) (_ BitVec m) (_ BitVec m))
;;    ((_ repeat i) (_ BitVec m) (_ BitVec i*m))
;;    ((_ zero_extend i) (_ BitVec m) (_ BitVec m+i))
;;    ((_ rotate_left i) (_ BitVec m) (_ BitVec m))
;;    ((_ rotate_right i) (_ BitVec m) (_ BitVec m))

(defun clear-smt ()
  (reset-to-empty-features *smt-features*)
  (clrhash *smt-name-hash*)
  (clrhash *translate-to-smt-hash*)
  (clrhash *expr-to-smt-type*)
  (clrhash *pvs-type-to-smt-type*)
  (setf *smt-prefix* nil)
  (setf *smt-type-declarations* nil)
  (setf *smt-uninterpreted-constants* nil)
  (setf *smt-assertions* nil)
  (setf *smt-warnings* nil)
  (newcounter *smt-id-counter*))

(defun smt-name (expr &optional id) ;;expr must have id field,
                                    ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *smt-name-hash*)))
	(or entry
	    (let ((name (smt-id-name (or id (id expr)))))
	      (setf (gethash expr *smt-name-hash*) name)
	      name)))))

(defun unique-smt-id () (funcall *smt-id-counter*))

(defun smt-id-name (id)
  (intern
   (concatenate 'string "|"
     (string (if (integerp id)
		 (format nil "~r"
		   id)
		 id))
     "_"
     (princ-to-string
      (unique-smt-id)) "|")))

(defun make-name (prefix)
  (assert (null (find #\| prefix)))
  (format nil "|~a.~a|" prefix (unique-smt-id)))

(defun push-warning (str)
  (push str *smt-warnings*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                          TYPES                          ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass smt-type ()
  ((name :accessor smt-type-name :initarg :name))
  (:documentation
   "An SMT-type is an internal representation the corresponding type
    that can be represented in SMT-LIBv2.
    This is the root class for the other SMT-type classes.

    The name for the type is interpreted differently for each subclass.
    A direct instance of this should !never! be constructed."
))
 ;; Uninterpreted types:
 ;; yices: (define-type [name])
 ;; smtlib2: (declare-sort [name] 0)
 ;; (smt-type [name])

;; All of the types:
;; smt-type
;; - smt-boolean-type
;; - smt-uninterpreted-type
;; - smt-number-type
;; - smt-real-type
;; - smt-int-type
;; - smt-bitvector-type
;; - smt-predicate-subtype
;; - smt-function-type
;; - smt-array-type
;; - smt-tuple-type
;; - smt-record-type
;; - smt-scalar-type
;; - smt-inductive-datatype

(defclass smt-boolean-type (smt-type)
  ()
  (:documentation "The built-in type corresponding to boolean terms."))

(defclass smt-uninterpreted-type (smt-type)
  ()
  (:documentation "The type corresponding to an uninterpreted sort."))

(defclass smt-number-type (smt-type)
  ()
  (:documentation "A super type to reals and integers that will be coerced to one or the other depending on the problem's form."))

(defclass smt-real-type (smt-type)
  ()
  (:documentation "The built-in type corresponding to the reals."))

(defclass smt-integer-type (smt-type)
  ()
  (:documentation "The built-in type corresponding to the integers."))

(defclass smt-bitvector-type (smt-type)
  ((width :accessor smt-bitvector-width :initarg :width :documentation "A natural number"))
  (:documentation "Example (BitVect 8)"))

(defclass smt-function-type (smt-type)
  ((domain-type-list :accessor smt-function-domain :initarg :domain-type-list :documentation "A non-empty list of types")
   (range-type :accessor smt-function-range :initarg :range)))

(defclass smt-array-type (smt-type)
  ((domain-type :accessor smt-array-domain :initarg :domain)
   (range-type :accessor smt-array-range :initarg :range)))

(defclass smt-predicate-subtype (smt-type)
  ((predicate :accessor smt-predicate-subtype-predicate :initarg :predicate)
   (parent-type :accessor smt-predicate-subtype-parent-type :initarg :parent-type
		:documentation "This is the type being aliased by this type.")))

(defclass smt-tuple-type (smt-type)
  ((element-types :accessor smt-tuple-elements :initarg :element-types)))

(defclass smt-record-type (smt-type)
  ((field-list :accessor smt-record-field-list :initarg :field-list
	       :documentation "This is a non-empty associative list from names to types")))

(defclass smt-scalar-type (smt-type)
  ((identifiers :accessor smt-scalar-identifiers :initarg :identifiers
		:documentation "This is a list of scalar names.")))

(defclass idt-constructor ()
  ((constructor-name :accessor constructor-name)
   (accessor-type-list :accessor accessor-type-list))) ; associative list from names to smt-types

(defclass smt-inductive-datatype (smt-type)
  ((constructors :accessor smt-idt-constructors :initarg :constructors))) ; list of idt-constructors

(defun make-boolean-type ()
  (make-instance 'smt-boolean-type :name "Bool"))

(defun make-uninterpreted-type (&optional name)
  (make-instance 'smt-uninterpreted-type :name (if name name (smt-id-name "uninterpreted"))))

(defun make-number-type ()
  (make-instance 'smt-number-type :name "Number"))

(defun make-real-type ()
  (make-instance 'smt-real-type :name "Real"))

(defun make-integer-type ()
  (make-instance 'smt-integer-type :name "Int"))

(defun make-bitvector-type (width)
  (assert (< 0 width))
  (let ((bvname (format t "(_ BitVect ~a)" width)))
    (make-instance 'smt-bitvector-type :name bvname :width width)))

(defun make-function-type (domainlist range)
  (let ((name (smt-id-name "function")))
    (make-instance 'smt-function-type :name name :domain-type-list domainlist :range range)))

(defun make-array-type (domain range)
  (let ((name (smt-id-name "array")))
    (make-instance 'smt-array-type :name name :domain domain :range range)))

(defun make-predicate-subtype (parent predicate)
  (assert (can-be-predicate-subtype? parent))
  (let ((name (smt-id-name "pst")))
    (make-instance 'smt-predicate-subtype :name name :parent-type parent :predicate predicate)))

(defun make-tuple-type (element-types)
  (assert (list-of-types? element-types))
  (let ((name (smt-id-name "tuple")))
    (make-instance 'smt-tuple-type :name name :element-types element-types)))

(defun make-record-type (field-list)
  (let ((name (smt-id-name "record")))
    (make-instance 'smt-record-type :name name :field-list field-list)))

(defun make-scalar-type (identifiers)
  (let ((name (smt-id-name "scalar")))
    (make-instance 'smt-scalar-type :name name :identifiers identifiers)))

(defun list-of-types? (elements)
  (if (null elements)
      t
      (and (smt-type? (car elements)) (list-of-types? (cdr elements)))))

;; smt-type
;; - smt-boolean-type
;; - smt-uninterpreted-type
;; - smt-real-type
;; - smt-int-type
;; - smt-bitvector-type
;; - smt-predicate-subtype
;; - smt-function-type
;; - smt-array-type
;; - smt-tuple-type
;; - smt-record-type
;; - smt-scalar-type
;; - smt-inductive-datatype
(defmethod smt-type? ((smtt smt-type)) (typep smtt 'smt-type))
(defmethod smt-boolean-type? ((smtt smt-type)) (typep smtt 'smt-boolean-type))
(defmethod smt-uninterpreted-type? ((smtt smt-type)) (typep smtt 'smt-uninterpreted-type))
(defmethod smt-number-type? ((smtt smt-type)) (typep smtt 'smt-number-type))
(defmethod smt-real-type? ((smtt smt-type)) (typep smtt 'smt-real-type))
(defmethod smt-integer-type? ((smtt smt-type)) (typep smtt 'smt-int-type))
(defmethod smt-bitvector-type? ((smtt smt-type)) (typep smtt 'smt-bitvector-type))
(defmethod smt-predicate-subtype? ((smtt smt-type)) (typep smtt 'smt-predicate-subtype))
(defmethod smt-function-type? ((smtt smt-type)) (typep smtt 'smt-function-type))
(defmethod smt-array-type? ((smtt smt-type)) (typep smtt 'smt-array-type))
(defmethod smt-tuple-type? ((smtt smt-type)) (typep smtt 'smt-tuple-type))
(defmethod smt-record-type? ((smtt smt-type)) (typep smtt 'smt-record-type))
(defmethod smt-scalar-type? ((smtt smt-type)) (typep smtt 'smt-scalar-type))
(defmethod smt-inductive-datatype? ((smtt smt-type)) (typep smtt 'smt-inductive-datatype))

(defmethod record-type-list ((rt smt-record-type))
  (loop for assoc in (smt-record-field-list rt) collect (cdr assoc)))


(defmethod smt-type-finite? ((smtt smt-boolean-type)) t)
(defmethod smt-type-finite? ((smtt smt-bitvector-type)) t)
(defmethod smt-type-finite? ((smtt smt-predicate-subtype)) (parent-type smtt))
(defmethod smt-type-finite? ((smtt smt-scalar-type)) t)
(defmethod smt-type-finite? ((smtt smt-type))
  (let ((dps (type-direct-predecessors smtt)))
    (every #'smt-type-finite? dps)))


;; A direct predecessor type is any type used in the construction of the current type
;; s(... t ...) then t is a direct predecessor of s
(defmethod type-direct-predecessors ((ft smt-function-type))
  (cons (smt-function-range ft) (smt-function-domain ft)))
(defmethod type-direct-predecessors ((at smt-array-type))
  (list (smt-function-range at) (smt-function-domain at)))
(defmethod type-direct-predecessors ((tt smt-tuple-type))
  (smt-tuple-elements tt))
(defmethod type-direct-predecessors ((rt smt-record-type))
  (record-type-list rt))
(defmethod types-in-idt-constructor ((c idt-constructor))
  (loop for assoc in (accessor-type-list c) collect (cdr assoc)))
(defmethod type-direct-predecessors ((idt smt-inductive-datatype))
  (apply 'append (loop for c in (smt-idt-constructors idt) collect (types-in-idt-constructor c))))
(defmethod type-direct-predecessors ((idt smt-type))
  nil)

(defmethod contained-types ((ty smt-type))
  (cons ty (loop for dp in (type-direct-predessors ty)
		 append (contained-types dp))))

;;   (let* ((dps (type-direct-predecessors ty)) ; list of types
;; 	 (contained-in-dps (mapcar 'contained-types dps)) ; list of list of types
;; 	 (contained-flat (apply 'append contained-in-dps)) ; list of types
;; 	 (result (cons ty contained-flat))) ; list of types including t
;;     result))


(defmethod essentially-functional ((ft smt-function-type)) t)
(defmethod essentially-functional ((at smt-array-type)) t)
(defmethod essentially-functional ((ty smt-type)) nil)

(defmethod contains-essentially-functional-type? ((ty smt-type))
  (some #'essentially-functional (contained-types ty)))

(defmethod contains-predicate-subtype? ((ty smt-type))
  (some #'smt-predicate-subtype? (contained-types ty)))

;; TK : Sam
;; (defmethod can-be-function-type? (domainlist (ran smt-type))
;;   (assert (list-of-types? domainlist))
;;   (let ((temp-tuple (make-tuple-type domainlist)))
;;     (and (not (contains-essentially-functional-type? temp-tuple))
;; 	 (not (contains-predicate-subtype? temp-tuple))
;; 	 (not (contains-essentially-functional-type? ran))
;; 	 (not (contains-predicate-subtype? ran)))))

(defmethod non-nested-function-type? (domainlist (ran smt-type))
  (assert (list-of-types? domainlist))
  (let ((temp-tuple (make-tuple-type domainlist)))
    (and (not (contains-essentially-functional-type? temp-tuple))
	 (not (contains-essentially-functional-type? ran)))))

(defmethod smt-function-can-be-array? ((ft smt-function-type))
  "Well yes if the domain list is a singleton, and is not essentially a function type."
  (let ((domain (smt-function-domain ft))
	(range (smt-function-range ft)))
    (and (= 1 (length domain))
	 (not (contains-essentially-functional-type? (car domain)))
	 (not (contains-essentially-functional-type? range))
	 (not (contains-predicate-subtype? (car domain)))
	 (not (contains-predicate-subtype? range)))))

(defmethod smt-function-to-array ((ft smt-function-type))
  (assert (smt-function-can-be-array? ft))
  (let ((domain-type (car (smt-function-domain ft)))
	(range-type (smt-function-range ft)))
    (make-array-type domain-type range-type)))
  

(defmethod can-be-bound-type? ((smtt smt-type))
  "Returns whether an expression of this type can be quantified."
  (not (contains-essentially-functional-type? smtt)))

;; - smt-boolean-type
;; - smt-uninterpreted-type
;; - smt-real-type
;; - smt-int-type
;; - smt-bitvector-type
;; - smt-predicate-subtype
;; - smt-function-type
;; - smt-array-type
;; - smt-tuple-type
;; - smt-record-type
;; - smt-scalar-type
;; - smt-inductive-datatype

;; smt-semantic-subtype? defines a relation
;; left <:s right
;;   iff
;; Every member of left (without reference to the semantic of any predicates!) is a member of right
;;  integer <:s reals
;;  pred(phi, t) <:s t
;;  Reflexive t <:s t
;;  <:s is transitive
;;  tuple(n,t_i) <:s tuple(n,u_i) iff t_i <:s s_i for all i
;; Similar for records

(defmethod smt-semantic-subtype? ((smtt smt-boolean-type) (rightt smt-type))
  (smt-boolean-type? rightt))

(defmethod smt-semantic-subtype? ((smtt smt-number-type) (rightt smt-type))
  (smt-number-type? rightt))

(defmethod smt-semantic-subtype? ((smtt smt-real-type) (rightt smt-type))
  (or (smt-number-type? rightt)
      (smt-real-type? rightt)))

(defmethod smt-semantic-subtype? ((smtt smt-integer-type) (rightt smt-type))
  (or (smt-integer-type? rightt)
      (smt-real-type? rightt)
      (smt-number-type? rightt)))

(defmethod smt-semantic-subtype? ((smtt smt-bitvector-type) (rightt smt-type))
  (and (smt-bitvector-type? rightt)
       (= (smt-bitvector-width smtt) (smt-bitvector-width rightt))))

(defmethod smt-semantic-subtype? ((smtt smt-predicate-subtype) (rightt smt-type))
  (or (eq smtt rightt)
      (smt-semantic-subtype? (smt-predicate-subtype-parent-type smtt) rightt)))

(defmethod smt-semantic-subtype? ((smtt smt-function-type)  (rightt smt-function-type))
  (eq smtt rightt))

(defmethod smt-semantic-subtype? ((smtt smt-array-type)  (rightt smt-type))
  (eq smtt rightt))

(defmethod smt-semantic-subtype? ((smtt smt-tuple-type)  (rightt smt-type))
  (and (smt-tuple-type? rightt)
       (smt-semantic-subtype?-lift (smt-tuple-elements smtt) (smt-tuple-elements rightt))))

(defmethod smt-semantic-subtype? ((smtt smt-record-type)  (rightt smt-type))
  (if (smt-record-type? rightt)
      (let ((left-rl (record-type-list smtt))
	    (right-rl (record-type-list smtt)))
	(push-warning (format t "This smt-semantic-subtype? relation between ~a <: ~a may be a bit too strong" smtt rightt))
	(smt-semantic-subtype?-lift left-rl right-rl))
      nil))

(defmethod smt-semantic-subtype? ((smtt smt-scalar-type)  (rightt smt-type))
  (eq smtt rightt))

(defmethod smt-semantic-subtype? ((smtt smt-inductive-datatype)  (rightt smt-type))
  (eq smtt rightt))

(defun smt-semantic-subtype?-lift (leftts rightts)
  (cond ((and (null leftts) (null rightts)) t)
	((null leftts) nil)
	((null rightts) nil)
	(t (let ((l-head (car leftts))
		 (r-head (car rightts)))
	     (and (smt-semantic-subtype? l-head r-head)
		  (smt-semantic-subtype?-lift (cdr leftts) (cdr rightts)))))))

(defmethod can-be-predicate-subtype? ((smtt smt-type))
  (push-warning (format t "The can-be-predicate-subtype? may be a bit too lax: ~a" smtt))
  (not (essentially-functional smtt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                     DECLARATIONS                        ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun translate-to-smt-scalar-constructors (constructors)
  (loop for constr in constructors collect (smt-name constr)))


(defmethod smt-scalar-to-inductive-datatype ((scalart smt-scalar-type))
  (let* ((identifiers (smt-scalar-identifiers scalart))
	 (name (smt-type-name scalart)))
    (make-smt-inductive-datatype name (loop for id in identifiers collect (make-constructor id nil)))))
	 
(defun elements-to-accessors (elements)
  (if elements
      (let ((head (car elements))
	    (rest (elements-to-accessors (cdr elements))))
	(acons (make-name "accessor") head rest))
      nil))

(defmethod smt-tuple-type-to-inductive-datatype ((tuplet smt-scalar-type))
  (let* ((elements (smt-tuple-elements tuplet))
	 (accessors (elements-to-accessors elements))
	 (constructor-name (make-name "constructor"))
	 (constructor (make-constructor constructor-name accessors))
	 (inductive-datatype-name (make-name "inductive-datatype-from-tuple")))
    (make-smt-inductive-datatype inductive-datatype-name (list constructor))))

(defmethod constructor-to-string ((c idt-constructor))
  (let* ((cname (constructor-name c))
	 (ats (accessor-type-list c))
	 (ats-strings (loop for at in ats collect (format nil "(~a ~a)" (car at) (cdr at)))))
    (if (nil ats)
	(format nil "~a" cname)
	(format nil "(~a ~{~a ~})" cname  ats-strings))))

(defmethod smt-inductive-datatype-to-string ((idt smt-inductive-datatype))
  (let* ((name (smt-type-name idt))
	 (constructors (smt-idt-constructors idt))
	 (constructor-strings (loop for c in constructors collect (constructor-to-string c))))
    (format nil "(~a {~a })" name constructor-strings)))

(defun declare-smt-inductive-datatypes (idatatypes)
  (let ((datatypes-as-strings (loop for idt in idatatypes collect (smt-inductive-datatype-to-string idt))))
    (format nil "(declare-datatypes () (~{~a ~})_)" datatypes-as-strings)))

(defmethod declare-smt-inductive-datatype ((idt smt-inductive-datatype))
  (declare-smt-inductive-datatypes (list idt)))

(defmethod declare-uninterpreted-sort ((ut smt-type))
  (let ((arity 0)
	(name (smt-type-name ut)))
    (format nil "(declare-sort ~a ~a)" name arity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                      UTILITIES                          ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-by-newline (string)
    "Returns a list of substrings of string
divided by newline characters space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them.
Adapted from: http://cl-cookbook.sourceforge.net/strings.html"
    (loop for i = 0 then (1+ j)
          as j = (position #\Newline string :start i)
          collect (subseq string i j)
          while j))

(defun smtlib2-print-comment (string)
  "Returns an smtlib2 formatted string with newlines handled appropraitely"
  (format nil ";; ~{~A~^#\Newline;;~} " (split-by-newline string)))

(defvar *source-string*
  "This query was automatically generated from PVS by the translate-to-smtlisp2 tool version v0.1
This tool was developed by Tim King (taking@cs.nyu.edu) and Natarajan Shankar (shankar@csl.sri.com).")

(defun smtlib2-set-info-source ()
  (format nil "(set-info :source |~a|)" *source-string*))

(define-condition unhandled-type-exception (error)
  ((text :initarg :text :reader text)
   (expr :initarg :expr :reader expr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                 Typechecking Pass                       ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; - smt-boolean-type
;; - smt-uninterpreted-type
;; - smt-real-type
;; - smt-int-type
;; - smt-bitvector-type
;; - smt-predicate-subtype
;; - smt-function-type
;; - smt-array-type
;; - smt-alias-type
;; - smt-tuple-type
;; - smt-record-type
;; - smt-scalar-type
;; - smt-inductive-datatype

(defmethod parent-type ((pst smt-predicate-subtype))
  (parent-type (smt-predicate-subtype-parent-type pst)))
(defmethod parent-type ((smtt smt-type)) smtt)

(defmethod freeze-features ()
  (assert (null (frozen? *smt-features*)))
  (setf (frozen? *smt-features*) t))
 
(defmethod enable-quantifiers ()
  (assert (null (frozen? *smt-features*)))
  (setf (quantified? *smt-features*) t))

(defmethod enable-arithmetic ()
  (assert (null (frozen? *smt-features*)))
  (setf (arithmetic? *smt-features*) t))

(defmethod enable-non-linear-arithmetic ()
  (assert (null (frozen? *smt-features*)))
  (setf (nonlinear-arithmetic? *smt-features*) t))

(defmethod flag-strictly-real-linear-terms ()
  (assert (null (frozen? *smt-features*)))
  (setf (strictly-real-linear-terms-appear? *smt-features*) t))

(defmethod enable-features-type ((ut smt-boolean-type))
  (assert (null (frozen? *smt-features*)))
  nil)

(defmethod enable-features-type ((ut smt-uninterpreted-type))
  (assert (null (frozen? *smt-features*)))
  (setf (uninterpreted-functions? *smt-features*) t))

(defmethod enable-features-type ((at smt-number-type))
  (assert (null (frozen? *smt-features*)))
  (enable-arithmetic))

(defmethod enable-features-type ((at smt-real-type))
  (assert (null (frozen? *smt-features*)))
  (enable-arithmetic)
  (setf (real-arithmetic? *smt-features*) t))

(defmethod enable-features-type ((it smt-integer-type))
  (assert (null (frozen? *smt-features*)))
  (enable-arithmetic)
  (setf (integer-arithmetic? *smt-features*) t))

(defmethod enable-features-type ((bt smt-bitvector-type))
  (assert (null (frozen? *smt-features*)))
  (setf (bitvectors? *smt-features*) t))

(defmethod enable-features-type ((pst smt-predicate-subtype))
  (assert (null (frozen? *smt-features*)))
  (enable-features-type (parent-type pst)))

(defmethod enable-features-type ((ft smt-function-type))
  (assert (null (frozen? *smt-features*)))
  (unless (smt-function-interpreted? ft)
    (setf (uninterpreted-functions? *smt-features*) t)))

(defmethod enable-features-type ((at smt-array-type))
  (assert (null (frozen? *smt-features*)))
  (setf (arrays-ex? *smt-features*) t))

(defmethod enable-features-type ((bt smt-tuple-type))
  (assert (null (frozen? *smt-features*)))
  (setf (inductive-datatypes? *smt-features*) t))

(defmethod enable-features-type ((rt smt-record-type))
  (assert (null (frozen? *smt-features*)))
  (setf (inductive-datatypes? *smt-features*) t))

(defmethod enable-features-type ((st smt-scalar-type))
  (assert (null (frozen? *smt-features*)))
  (setf (inductive-datatypes? *smt-features*) t))

(defmethod enable-features-type ((idt smt-inductive-datatype))
  (assert (null (frozen? *smt-features*)))
  (setf (inductive-datatypes? *smt-features*) t))

(defmethod enable-features-type ((unhandled t))
  (break "enable-features-type error"))

(defun smt-tc-type (type bindings)
  (smt-tc-type* type bindings))

(defmethod smt-tc-type* :around ((pvstype type-expr) bindings)
  (format t "(defmethod smt-tc-type* ((obj expr) bindings) ...) with ~%(~a,~a)~%" pvstype bindings)
  (or (gethash pvstype *pvs-type-to-smt-type*)
      (let ((smtt (call-next-method)))
	(setf (gethash pvstype *pvs-type-to-smt-type*) smtt)
	(push smtt *smt-type-declarations*)
	smtt)))

(defmethod smt-tc-type* ((pvs-type subtype) bindings)
  (cond ((tc-eq pvs-type *integer*) (make-integer-type))
	((tc-eq pvs-type *real*) (make-real-type))
	((tc-eq pvs-type *number_field*) (make-number-type))
	(t
	 (let ((super (smt-tc-type* (supertype pvs-type) bindings))
	       (pred (predicate pvs-type)))
	   (smt-tc-expr pred bindings)
	   ;; this is where lambda's need to be handled
	   ;; this is where judgements need to be handled
	   (if (can-be-predicate-subtype? super)
	       (make-predicate-subtype super pred)
	       (error 'unhandled-type-exception :text "the given type cannot be made a predicate subtype" :expr pvs-type)
	       )))))

(defmethod smt-tc-type* ((pvs-type enumtype) bindings)
  (declare (ignore bindings))
  (let ((constrs (constructors pvs-type)))
    (make-scalar-type (mapcar #'id constrs))))
  
(defmethod smt-tc-type* ((pvs-type funtype) bindings)
  (let ((dom (domain pvs-type))
	(ran (range pvs-type)))
    (cond
      ; Disabling this strictness for now
      ;((dep-binding? dom)
      ; (error 'unhandled-type-exception :text "Dependent types in function types are not supported" :expr pvs-type))
	  ;; catch the bitvectors
      ((and (tc-eq ran *boolean*)
	    (simple-below? dom)
	    (number-expr? (simple-below? dom)))
       (let ((width (simple-below? dom)))
	 (make-bitvector-type (number width))))
      (t
       (let* ((smt-dom (smt-tc-type* dom bindings))
	      (smt-doms (list smt-dom))
	      (smt-range (smt-tc-type* ran bindings)))
	 (if (non-nested-function-type? smt-doms smt-range)
	     (make-function-type smt-doms smt-range)
	     (error 'unhandled-type-exception :text "Unsupported domain and range types for functions" :expr pvs-type)
	     ))))))

(defun smt-tc-record-type (fields bindings &optional names-to-smtts)
  (if (null fields)
      (nreverse names-to-smtts)
      (let* ((head (car fields))
	     (tail (cdr fields))
	     (smthead (smt-tc-type* (type head) bindings))
	     (next-smtts (acons (id head) smthead names-to-smtts)))
	(smt-tc-tuple-type tail (acons head smthead bindings) next-smtts))))

(defmethod smt-tc-type* ((pvs-type recordtype) bindings)
  (let ((names-to-smtts (smt-tc-record-type (fields pvs-type) bindings)))
    (make-record-type names-to-smtts)))

(defun smt-tc-tuple-type (pvs-types bindings &optional smtts)
  (if (null pvs-types)
      (nreverse smtts)
      (let* ((head (car pvs-types))
	     (tail (cdr pvs-types))
	     (smthead (smt-tc-type* (if (dep-binding? head) (type head) head) bindings))
	     (next-smtts (cons smthead smtts)))
	(if (dep-binding? head)
	    (smt-tc-tuple-type tail (acons head smthead bindings) next-smtts)
	    (smt-tc-tuple-type tail bindings next-smtts)))))


(defmethod smt-tc-type* ((pvs-type tupletype) bindings)
  (let* ((ts (types pvs-type))
	 (smtts (smt-tc-tuple-type ts bindings)))
    (make-tuple-type smtts)))

(defmethod smt-tc-type* ((pvs-type type-name) bindings)
  (declare (ignore bindings))
  (cond ((tc-eq pvs-type *boolean*) (make-boolean-type))
	((datatype? pvs-type)
	 (error 'unhandled-type-exception :text "datatypes are unsupported right now" :expr pvs-type))
	((tc-eq pvs-type *number*)
	 (make-number-type))
	;; ((tc-eq pvs-type *number*)
	;;  (error 'unhandled-type-exception
	;; 	:text "We have encountered a number type that is not a subtype of real."
	;; 	:expr pvs-type))
	(t ; The only cases remaining are uninterpreted types
	 (let ((ut-name (smt-name pvs-type)))
	     (make-uninterpreted-type ut-name)))))

(defmethod smt-tc-type* ((unhandled t) bindings)
  (declare (ignore bindings))
  (error 'unhandled-type-exception :text "uncaught object in smt-tc-type*" :expr unhandled))

;; Psuedo code for smt-tc-expr
;; (defun smt-tc-expr (phi)
;;   (let* ((hashtype (lookup phi)))
;;     (if (hashtype)
;; 	hashtype
;; 	(progn
;; 	  (smt-tc-expr children of phi)
;; 	  (let* ((t (get the relevant type phi))
;; 	       (smtt (smt-tc-type t)))
;; 	    (logic-feature are raised if needed to express phi)
;; 	    (attempt to coerce subtype if needed)
;; 	    (add to hashtable phi smtt)
;;          smtt)))))

(defun smt-tc-expr (obj &optional bindings) (smt-tc-expr* obj bindings))

(defmethod smt-tc-expr* :around ((obj expr) bindings)
  "This is for looking up cached results."
  (format t "(defmethod smt-tc-expr* ((obj expr) bindings) ...) with ~%(~a,~a)~%" obj bindings)
  (let ((hashed-value (gethash obj *expr-to-smt-type*)))
    (or hashed-value ;; if this is in the hash-table, return it.
	(cdr (assoc obj bindings)) ;; if this is in the bindings, return it.
	;; otherwise
	(let* ((result (call-next-method))
	       (type-constraints (type-constraints obj :none)))
	  (enable-features-type result)
	  (if (and nil type-constraints) ;
	      ;; This is a predicate-subtype on top of the current type
	      (if (can-be-predicate-subtype? result)
		  (let* ((rtype-constraints (loop for fmla in type-constraints nconc (and+ fmla)))
			 (psubtype (make-predicate-subtype result rtype-constraints)))
		    (setf (gethash obj *expr-to-smt-type*) psubtype) ;; TODO check if this is okay...
		    psubtype)
		  (error 'unhandled-type-exception :text "predicate subtype problem" :expr obj))
	      ;; This is not a predicate subtype
	      (progn
		(if type-constraints
		    (break "Warning the type-constraints ~a were just ignored for ~a"  type-constraints obj))
		(setf (gethash obj *expr-to-smt-type*) result) ;; TODO check if this is okay...
		     result))))))


(defun smt-tc-bindings (bind-decls bindings)
  (if (null bind-decls)
      bindings
      (let* ((head (car bind-decls))
	     (tail (cdr bind-decls))
	     (smtt (smt-tc-expr* head bindings)))
	(cond ((can-be-bound-type? smtt)
	       (smt-tc-bindings tail (acons head smtt bindings)))
	      (t (error 'unhandled-type-exception
			:text "This type cannot be bound nor can it be coerced to a type that can be."
			:expr head))))))

(defmethod smt-tc-expr* ((binding-expr binding-expr) bindings)
  ;(break "(defmethod smt-tc-expr* ((expr binding-expr) bindings) ...)")
  (with-slots ((expr-bindings bindings) (subexpression expression)) binding-expr
    (cond ((lambda-expr? binding-expr)
	   (error 'unhandled-type-exception
		  :text "lambdas cannot be translated to smt. Remove these before invoking the translator."
		  :expr binding-expr))
	  ((or (forall-expr? binding-expr) (exists-expr? binding-expr))
	   (enable-quantifiers)
	   (let* ((newbindings (smt-tc-bindings expr-bindings bindings))
		  (result (smt-tc-expr* subexpression newbindings)))
	     (assert (smt-semantic-subtype? result (make-boolean-type))) ; The result must be a boolean type
	     result)))))

(defmethod smt-tc-expr* ((expr name-expr) bindings)
  (assert (null (assoc expr bindings)))
  (assert (null (gethash expr *expr-to-smt-type*)))
  (let ((smt-name  (gethash expr *smt-name-hash*))
	(smtt (smt-tc-type (type expr) bindings)))
    (when smt-name
      (let ((smt-name (smt-name expr)))
	(push *smt-uninterpreted-constants* smt-name)))
    smtt))


(defmethod smt-tc-expr* ((expr application) bindings)
  (with-slots (operator argument) expr
    (let* ((op* (operator* expr))
	   (op-id (when (name-expr? op*) (id op*))))
      (cond
	;; rem is curried and thus must be handled specially
	((and (eq op-id 'rem)
	      (eq (id (module-instance (resolution op*)))
		  'modulo_arithmetic)) 
	 (smt-tc-expr* (args1 expr) bindings)
	 (smt-tc-expr* (args1 (operator expr)) bindings)
	 (enable-non-linear-arithmetic)
	 (make-integer-type))
	;; '- is unary and thus must be handled specially
	((and (eq op-id '-)
	      (unary-application? expr)
	      (eq (id (module-instance (resolution op*))) '|number_fields|))
	 (smt-tc-expr* (argument expr) bindings))
	;; nat2bv is not handled natively atm
	((and (eq op-id 'nat2bv)
	      (number-expr? (expr (car (actuals (module-instance op*))))))
	 (error 'unhandled-type-exception
		:text "encountered nat2bv. rewrite this away before invoking the solver."
		:expr expr))
	((and (eq op-id '-)
	      (unary-application? expr)
	      (eq (id (module-instance (resolution op*))) '|bv_arithmetic_defs|))
	 (smt-tc-expr* (argument expr) bindings))
	((and (eq op-id '^)
	      (eq (id (module-instance (resolution op*))) '|bv_caret|)
	      (tuple-expr? argument)
	      (tuple-expr? (cadr (exprs argument)))
	      (number-expr? (car (exprs (cadr (exprs argument)))))
	      (number-expr? (cadr (exprs (cadr (exprs argument))))))
	 (let* ((m (number (car (exprs (cadr (exprs argument))))))
		(n (number (cadr (exprs (cadr (exprs argument))))))
		(childt (smt-tc-expr* (car (exprs argument)) bindings))
		(ctwidth (smt-bitvector-width childt)))
	   (assert (>= ctwidth m))
	   ;; pvs ensures that the caret operator is appropraite (assuming tccs)
	   (make-bitvector-type (+ 1 (- m n)))))
	((and (enum-adt? (find-supertype (type argument)))
	      (recognizer? operator))
	 (smt-tc-expr* argument bindings)
	 (make-boolean-type))
	((constructor? operator)
	 (error 'unhandled-type-exception
		:text "inductive datatypes are not yet handled by the type checker."
		:expr expr))
	(t
	 (let ((smt-interp (smt-interpretation operator)))
	   (if smt-interp
	       (let ((ranget (smt-tc-type (range (type op*)) bindings)))
		 (smt-tc-expr*-lift (arguments expr) bindings)
		 (when (and (is-non-linear-operator? smt-interp)
			    (not (smt-integer-coefficient-expression? expr)))
		   (if (smt-rational-coefficient-expression? expr)
		       (flag-strictly-real-linear-terms)
		       (enable-non-linear-arithmetic)))
		 ranget)
	       (smt-tc-expr*-translate-uninterpreted-function expr bindings))))))))


(defmethod smt-*-concrete-coefficient-term? (relevant-coefficient-check expr)
  (or (apply relevant-coefficient-check expr)
      (with-slots (operator argument) expr
	(let* ((op* (operator* expr))
	       (op-id (when (name-expr? op*) (id op*))))
       (and (eq op-id '*)
	    (or (apply relevant-coefficient-check (args1 expr))
		(apply relevant-coefficient-check (args2 expr))))))))

(defmethod smt-lia-concrete-coefficient-term? (expr)
  (smt-*-concrete-coefficient-term? 'smt-integer-coefficient-expression? expr))

(defmethod smt-lra-concrete-coefficient-term? (expr)
  (smt-*-concrete-coefficient-term? 'smt-rational-coefficient-expression? expr))

(defmethod smt-numeral-expression? (expr minimum)
  (and (number-expr? expr)
       (>= (number expr) minimum)))

(defmethod smt-integer-coefficient-expression? (expr)
  (or (smt-numeral-expression? expr 0)
      (with-slots (operator argument) expr
	(let* ((op* (operator* expr))
	       (op-id (when (name-expr? op*) (id op*))))
	  (and (eq op-id '-)
	       (unary-application? expr)
	       (smt-numeral-expression? argument 0))))))

(defmethod smt-rational-coefficient-expression? (expr)
  (or (smt-integer-coefficient-expression? expr)
      (with-slots (operator argument) expr
	(let* ((op* (operator* expr))
	       (op-id (when (name-expr? op*) (id op*))))
	  (and (eq op-id '/)
	       (smt-integer-coefficient-expression? (args1 expr))
	       (smt-numeral-expression? (args2 expr) 1))))))

(defun smt-tc-expr*-translate-uninterpreted-function (expr bindings)
  (with-slots (operator argument) expr
    (when (application? operator)
      (error 'unhandled-type-exception
	     :text "Higher order functions are not allowed for now."
	     :expr operator))
    (let* ((smt-op (smt-tc-expr* operator bindings))
	   (args (if (tuple-expr? argument)
		     (arguments expr)
		     (let ((stype (find-supertype (type argument))))
		       (if (tupletype? stype)
			   (if (and (variable? argument)
				    (assoc argument bindings
					   :test
					   #'same-declaration))
			       argument
			       (loop for index from 1 to (length (types stype))
				  collect (make-projection-application index argument)))
			     (list argument)))))
	     (sargs (if (and (variable? args)
			     (tupletype? (find-supertype (type argument))))
			(let ((bnd (assoc argument bindings
					  :test
					  #'same-declaration)))
			  (cdr bnd))
			(smt-tc-expr*-lift args bindings)))
	   (sargst (if (cdr sargs)
		       (make-tuple-type sargs)
		       (car sargs))))
      (assert (smt-function-type? smt-op))
      (assert (smt-semantic-subtype?-lift (list sargst) (smt-function-domain smt-op)))
      (smt-function-range smt-op))))

(defmethod smt-tc-expr* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (let ((argt (smt-tc-expr* argument bindings)))
      (assert (smt-tuple-type? argt))
      (nth (1- index) (smt-tuple-elements argt)))))
	 (defun smt-tc-expr*-lift (expr-list bindings &optional accum)
  (if (null expr-list)
      (nreverse accum)
      (let* ((head (car expr-list))
	     (tail (cdr expr-list))
	     (headt (smt-tc-expr* head bindings)))
	(smt-tc-expr*-lift tail bindings (cons headt accum)))))

(defmethod smt-tc-expr* ((n number-expr) bindings)
  (smt-tc-type (type n) bindings))

(defmethod smt-tc-expr* ((unhandled t) bindings)
  (declare (ignore bindings))
  (break "smt-tc-expr* has encountered an unhandled case: ~a" unhandled))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                     Translation                         ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tccs accumulated for the current 

(defun translate-pvs-expr-to-smt (expr &optional bindings)
  (break "Unimplemented ( translate-pvs-expr-to-smt ~a ~a)" expr bindings))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                         ;;
;;                      UTILITIES                          ;;
;;                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun smt-interpretation (name-expr)
  (when (name-expr? name-expr)
    (let* ((id-assoc (cdr (assoc (id name-expr) *smt-interpreted-names*)))
	   (mod-assoc (cdr (assoc (id (module-instance
				       (resolution name-expr)))
				  id-assoc))))
      mod-assoc)))

(defmethod generate-assertion ((fmla expr))
  (format nil "(assert ~a)" (translate-pvs-expr-to-smt fmla)))

(defmethod push-assertion ((fmla expr))
  (push (generate-assertion fmla) *smt-assertions*))

(defun debugging-output ()
  (format-if "~%ydefns = ~% ~{~a~%~}" nil))


(defun write-types-to-stream (stream)
  (let* ((selected-types (select-types-for-output *smt-type-declarations*))
	 (revtypes (reverse selected-types)))
    (loop for type in revtypes do (format stream "~a" (type-declaration type)))))

(defun write-query-to-stream (stream)
  (format stream "~{~a ~%~}" *smt-prefix*)
  (write-types-to-stream stream)
  (format stream "~{~a ~%~}" (reverse *smt-uninterpreted-constants*))
  (format stream "~{~a ~%~}" (reverse *smt-assertions*))
  (format stream "(check-sat)~%"))

(defun execute-callstring (tmp-file callstring)
  (with-open-file (out tmp-file :direction :output :if-exists :supersede)
    #+allegro
    (excl:run-shell-command
     callstring
     :input "//dev//null"
     :output out
     :error-output :output)
    #+sbcl
    (sb-ext:run-program
     callstring
     nil
     :input "//dev//null"
     :output out
     :error out)
    #+cmu
    (extensions:run-program
     callstring
     nil
     :input "//dev//null"
     :output out
     :error out)))

(defun issue-warnings ()
  "This gives the user warnings."
  (when *smt-warnings*
    (format t "~70,,,'*A" "")
    (loop for w in (reverse *smt-warnings*) do
	 (format t "~%Warning: ~a. ~%" w))
    (format t "~70,,,'*A" "")))

(defun react-to-smt-answer (status file tmp-file solver callstring)
  "Given the results of calling an SMT solver with callstring, this handles the resulting output of the call.
   This prepares a response to PVS and handles the files generated when making the query.
   If the query unambigiously succeeded, the generated files are deleted.
   If the query failed for some reason, the the user is shown the file names and the files are kept."
  (cond ((zerop status)
	 (let ((result (file-contents tmp-file)))
	   (break "smt result")
	   (delete-file tmp-file)
	   (delete-file file)
	   (format-if "~%Result = ~a" result)
	   (cond ((search "unsat"  result :from-end t)
		  (format-if "~%SMT translation of negation is unsatisfiable")
		  (values '! nil nil))
		 (t (format-if "~%SMT translation of negation is not known to be satisfiable or unsatisfiable")
		    (values 'X nil nil)))))
	(t (format t
		   "~%Error running SMT - you may need to do one or more of:~
                    ~% Make sure you can run the solver selected: ~a~
                    ~% Solver was run with the command line: ~a~
                    ~%The error message is:~% ~a in:~a"
		   solver
		   callstring
		   (file-contents tmp-file)
		   (namestring file))
	   (values 'X nil))))

(defun smt (sformnums &optional solver?)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (s-forms (select-seq (s-forms goalsequent) sformnums))
	     (fmlas (loop for sf in s-forms collect (negate (formula sf))))
	     (conjuct (make!-conjunction* fmlas)))
	(clear-smt)
	(smt-tc-expr conjuct)
	(freeze-features)
	(push-assertion conjuct)
	(debugging-output)
	(let ((file (make-pathname :defaults (working-directory)
				   :name (label ps) :type "smt2"))
	      (solver (select-solver-by-features *smt-features* solver?)))
	  (with-open-file (stream  file :direction :output :if-exists :supersede)
	    (write-query-to-stream stream))
	  (let ((status nil)
		(tmp-file (pvs-tmp-file))
		(callstring (generate-smt-solver-call-string solver (namestring file))))
	    (setq status (execute-callstring tmp-file callstring))
	    (issue-warnings)
	    (let ((result (react-to-smt-answer status file tmp-file solver callstring)))
	      (clear-smt) ;cleanup
	      result))))))

(addrule 'smt () ((fnums *) solver?)
  (smt fnums solver?)
  "Invokes an SMT solver as an endgame SMT solver to prove that the conjunction
of the negations of the selected formulas is unsatisfiable. "
  "~%Simplifying with smt,")
  
  
(defstep smt-with-rewrites
  (&optional (fnums *) defs theories rewrites exclude-theories exclude solver?)
  (then (simplify-with-rewrites fnums defs theories rewrites exclude-theories exclude)
	(smt fnums solver?))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums) followed
by (yices fnums), then turns off all the installed rewrites.  Examples:
 (smt-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (smt-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, applying Yices, and disabling installed rewrites")

(defstep smt-simp (&optional (fnums *) solver?)
  (then (skosimp*)(smt :fnums fnums :solver? solver?))
  "Repeatedly skolemizes and flattens, and then applies an smt solver"
  "Repeatedly skolemizing and flattening, and then invoking smt")
  

(defstep smt-grind (&optional (defs !); nil, t, !, explicit, or explicit!
			  theories
			  rewrites
			  exclude
			  (if-match t)
			  (updates? t)
			  polarity?
			  (instantiator inst?)
			  (let-reduce? t)
			  cases-rewrite?
			  quant-simp?
			  no-replace?
			  implicit-typepreds?
			  solver?)
  (then (install-rewrites$ :defs defs :theories theories
		      :rewrites rewrites :exclude exclude)
	(repeat* (bash$ :if-match if-match :updates? updates?
			:polarity? polarity? :instantiator instantiator
			:let-reduce? let-reduce?
			:quant-simp? quant-simp?
			:implicit-typepreds? implicit-typepreds?
			:cases-rewrite? cases-rewrite?))
	(smt :solver? solver?))
  "Core of GRIND: Installs rewrites, repeatedly applies BASH, and then
   invokes the smt solver, solver?.  See BASH for more explanation."
"Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization, Yices")

