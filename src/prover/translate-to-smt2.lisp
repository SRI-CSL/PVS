;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smt2.lisp -- 
;; Author          : K. Nukala and N. Shankar
;; Created On      : June 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 07/30/2023
;; Update Count    : 5
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

(defvar *smt2bindings* nil)
(defvar *smt2embeddedf* nil)
(defvar *smt2defns* nil)
;;(defvar *smt2datatype-warning* nil)
(defvar *smt2name-hash* (make-pvs-hash-table))
(defvar *translate-to-smt2-hash* (make-pvs-hash-table))
; (defvar *smt2-executable* nil)
(defvar *smt2-executable* "~/z3/build/z3") ;; TODO: find/set this dynamically
(defvar *smt2-flags* "--mode=one-shot")
(defvar *smt2-id-counter*)  ;;needs to be initialized in eproofcheck
(defvar *smt2-conditions* nil)
(defvar *smt2-subtype-constraints* nil)
(defvar *smt2-tuple-update-equalities* nil)
(defvar *smt2-record-datatype* nil)
(defvar *smt2-recordtype-hash* nil)

(newcounter *smt2-id-counter*)

(defparameter *smt2-interpreted-names*
  '((=  (|equalities| . =))
    (/=  (|notequal| . /=))
    (TRUE (|booleans| . true))
    (FALSE (|booleans| . false))
    (IMPLIES  (|booleans| . =>))
    (=>  (|booleans| . =>))
    (⇒ (|booleans| . =>))
    (<=> (|booleans| . =))
    (IFF (|booleans| . =))
    (⇔ (|booleans| . =))
    (AND (|booleans| . "and") (|bv_bitwise| . "bvand"))
    (∧ (|booleans| . "and"))
    (& (|booleans| . "and"))
    (OR  (|booleans| . "or") (|bv_bitwise| . "bvor"))
    (∨  (|booleans| . "or"))
    (NOT  (|booleans| . "not")(|bv_bitwise| . "bvnot"))
    (¬ (|booleans| . "not"))
    (+  (|number_fields| . +)(|bv_arith_nat_defs| . "bvadd"))
    (- (|number_fields| . -)(|bv_arithmetic_defs| . "bvsub"))
    (*   (|number_fields| . *))
    (/  (|number_fields| . /))
					;    (rem (|modulo_arithmetic| . mod))
					;    (ndiv (|modulo_arithmetic| . div))
    (< (|reals| . <)(|bv_arith_nat_defs| . "bvult"))
    (<=  (|reals| . <=)(|bv_arith_nat_defs| . "bvule"))
    (> (|reals| . >)(|bv_arith_nat_defs| . "bvugt"))
    (>=  (|reals| . >=)(|bv_arith_nat_defs| . "bvuge"))
    (O (|bv_concat_def| . "concat"))
    (& (|booleans| . "and")(|bv_bitwise| . "bvand"))
    (XOR  (|bv_bitwise| . "bv-xor"))
    ;; (^ (|bv_caret| .  "(_ extract ~a ~a)")
    ;; (sign_extend   (|bv_extend| . "(_ sign_extend ~a)")
    (|bv_slt| (|bv_arithmetic_defs| . "bvslt"))
    (|bv_sle| (|bv_arithmetic_defs| . "bvsle"))
    (|bv_sgt| (|bv_arithmetic_defs| . "bvsgt"))
    (|bv_sge| (|bv_arithmetic_defs| . "bvsge"))
    (|bv_splus| (|bv_arithmetic_defs| . "bvadd"))
    (|bv_stimes| (|bv_arithmetic_defs| . "bvmul"))
    ))

(defun clear-smt2 ()
  (setq *smt2defns* nil)
  (clrhash *smt2name-hash*)
  (clrhash *translate-to-smt2-hash*)
  (newcounter *smt2-id-counter*))

(defun smt2-name (expr &optional id) ;;expr must have id field,
  ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *smt2name-hash*)))
	(or entry
	    (let ((name (smt2-id-name (or id (id expr)))))
	      (setf (gethash expr *smt2name-hash*) name)
	      name)))))

(defun smt2-id-name (id)
  (intern
   (concatenate 'string
		(string (if (integerp id)
			    (format nil "~r"
				    id)
			    id))
		; "_"
		(princ-to-string
		 (funcall
		  *smt2-id-counter*))) :pvs))

(defun smt2-type-name (expr)
  (smt2-name expr))

(defmethod translate-to-smt* :around ((obj type-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      result)))))

(defmethod translate-to-smt2* ((ty type-name) bindings)
  (declare (ignore bindings))
  (let ((smt2name-hash (gethash ty *smt2name-hash*)))
    (or smt2name-hash 
	(cond ((enum-adt? ty)
	       ;; (let ((constructors (constructors ty))
	       ;; 	     (yname (yices2-name ty)))	;;bindings can be ignored
	       (error "yices2 tranlation does not support scalars")
	       ;;(translate-to-yices2-scalar yname constructors))
	       )
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "Bool"))
	      ((tc-eq ty *number*) "Real")
	      ;;else uninterpreted type
	      (t (let ((smt2name (smt2-name ty)))
		   (push (format nil "(declare-sort ~a)" smt2name) *smt2defns*)
		   smt2name))))))

(defmethod translate-to-smt2* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ;;((tc-eq ty *naturalnumber*) 'nat) ;;there is no nat
      ((tc-eq ty *integer*) "Int")
      ((tc-eq ty *real*) "Real")
      (t (translate-to-smt2* supertype bindings)))))

(defmethod translate-to-smt* ((ty tupletype) bindings)
  (with-slots (types) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* types bindings))))

(defmethod translate-to-smt2* ((ty recordtype) bindings)
  (with-slots (fields) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* fields bindings))))

(defmethod translate-to-smt2* ((ty field-decl) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty dep-binding) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty funtype) bindings)
  (with-slots (domain range) ty
    (if (bitvector-type? ty)
	(format nil "(_ BitVec ~a)" 32)
	;; TODO: generalize 32 to N (from bitvec[N])
	;; via (simple-below? domain)
	(format nil "(Array ~a ~a)"
		(translate-to-smt2* domain bindings)
		(translate-to-smt2* range bindings)))
    ))


(defmethod translate-to-smt2* ((list list) bindings)
  (cond ((consp list)
	 (cons (translate-to-smt2* (car list) bindings)
	       (translate-to-smt2* (cdr list) bindings)))
	(t nil)))


(defmethod translate-to-smt2* :around ((obj expr) bindings)
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let* ((result (call-next-method))
		   (type-constraints (type-constraints obj :none))
		   (rtype-constraints (loop for fmla in type-constraints
					    nconc (and+ fmla))))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      (loop for tc in rtype-constraints
		    do (let* ((smt2tc (translate-to-smt2* tc bindings))
			      (smt2clause (if *smt2-conditions*
					      (format nil "(assert (implies (and ~{ ~a~}) ~a))"
						      *smt2-conditions*
						      smt2tc)
					      (format nil "(assert ~a)" smt2tc))))
			 (push smt2clause *smt2-subtype-constraints*)))
	      result)))))

(defun smt2-recognizer (name bindings)
  (when (recognizer? name)
    (translate-to-smt2* (type name) bindings)
    (format nil "~a?" (translate-to-smt2* (constructor name) bindings) )))

(defmethod translate-to-smt2* ((expr name-expr) bindings)
  (let ((bpos (assoc expr bindings
		     :test #'same-declaration)))
    (if bpos (if (consp (cdr bpos))
		 (format nil "(Prod ~{ ~a~})"  (cdr bpos))
		 (cdr bpos))
	(let* ((smt2name-hashentry (gethash expr *smt2name-hash*)))
	  (or smt2name-hashentry
	      (smt2-interpretation expr)
					;(eta-expanded-smt2-interpretation expr)
	      (smt2-recognizer expr bindings)
	      (let* ((smt2type (translate-to-smt2* (type expr)
						   bindings))
		     (smt2name-hashentry (gethash expr *smt2name-hash*)))
		(or smt2name-hashentry
		    (let* ((smt2name (smt2-name expr))
			   (defn (format nil "(declare-const ~a ~a)"
					 smt2name
					 smt2type)))
		      (push defn
			    *smt2defns*)
		      (format-if "~%Adding definition: ~a" defn)
		      smt2name))))))))


(defmethod translate-to-smt2* ((expr constructor-name-expr) bindings)
  (call-next-method (lift-adt expr) bindings))

(defmethod translate-to-smt2* ((expr rational-expr) bindings)
  (declare (ignore bindings))
  (number expr))

(defmethod translate-to-smt2* ((ex string-expr) bindings)
  (declare (ignore bindings))
  (string->int (string-value ex)))

(defmethod translate-to-smt2* ((expr record-expr) bindings)
  (with-slots (assignments) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (sort-assignments (assignments expr)) bindings))))

(defmethod translate-to-smt2* ((expr tuple-expr) bindings)
  (with-slots (exprs) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (exprs expr) bindings))))

(defmethod translate-to-smt2* ((expr branch) bindings)
  (let ((smt2condition (translate-to-smt2* (condition expr) bindings)))
    (format nil "(ite ~a ~a ~a)"
	    smt2condition
	    (let ((*smt2-conditions* (push smt2condition *smt2-conditions*)))
	      (translate-to-smt2* (then-part expr) bindings))
	    (let ((*smt2-conditions* (push `("not" ,smt2condition) *smt2-conditions*)))
	      (translate-to-smt2* (else-part expr) bindings)))))

(defmethod translate-to-smt2* ((expr projection-expr) bindings)
  (break "Can't translate standalone projections"))

(defmethod translate-to-smt2* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (if (variable? argument)
	(let ((bnd (assoc argument bindings
			  :test #'same-declaration)))
	  (if (and bnd (consp (cdr bnd)))
	      (nth (1- index) (cdr bnd))
	      (format nil "((_ project ~a) ~a)"
		      (1+ (index expr))
		      (translate-to-smt2* argument bindings))))
	(format nil "((_ project ~a) ~a)"
		(1+ (index expr))
		(translate-to-smt2* argument bindings)))))

(defmethod translate-to-smt2* ((expr field-application) bindings)
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (pos (position id fields 
			  :key #'id)))
      (format nil "((_ project ~a) ~a)"
	      (1+ pos)
	      (translate-to-smt2* argument bindings)))))

(defmethod translate-to-smt2* ((expr application) bindings)
  (with-slots (operator argument) expr
    (let* ((op* (operator* expr))
	   (op-id (when (name-expr? op*) (id op*))))
      (cond ((and (eq op-id 'rem)
		  (eq (id (module-instance (resolution op*)))
		      'modulo_arithmetic))
	     (let ((denom (translate-to-smt2* (args1 (operator expr))
					      bindings))
		   (numer (translate-to-smt2* (args1 expr)
					      bindings)))
	       (format nil "(mod ~a ~a)" numer denom)))
	    ((and (eq op-id '-)  ;;NSH(4-19-10)
		  (unary-application? expr)
		  (eq (id (module-instance (resolution op*)))
		      '|number_fields|))
	     (format nil "(- ~a)" (translate-to-smt2* (argument expr) bindings)))
	    ((and (eq op-id 'nat2bv)
		  (number-expr? (expr (car (actuals (module-instance op*))))))
	     (let ((size (translate-to-smt2*
			  (expr (car (actuals (module-instance op*))))
			  bindings))
		   (num (translate-to-smt2*
			 (args1 expr) bindings)))
	       (format nil "(nat2bv[~a] ~a)" size num)))
	    ((and (eq op-id '-)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_arithmetic_defs|)
		  (not (tupletype? (domain (type op*)))))
	     (format nil "(bvneg ~a)"
		     (translate-to-smt2* (argument expr) bindings)))
	    ((and (eq op-id '^)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_caret|)
		  (tuple-expr? argument)
		  (tuple-expr? (cadr (exprs argument)))
		  (number-expr? (car (exprs (cadr (exprs argument)))))
		  (number-expr? (cadr (exprs (cadr (exprs argument))))))
	     (format nil "((_ extract ~a ~a) ~a)"
		     (number (car (exprs (cadr (exprs argument)))))
		     (number (cadr (exprs (cadr (exprs argument)))))
		     (translate-to-smt2* (car (exprs argument)) bindings)))
	    ((and (eq op-id 'sign_extend)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_extend_defs|)
		  (number-expr? (argument (operator expr))))
	     (format nil "((_ sign_extend ~a) ~a)"
		     (number (argument (operator expr)))
		     (translate-to-smt2* argument bindings)))
	    ((and (eq op-id 'zero_extend)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_extend_defs|)
		  (number-expr? (argument (operator expr))))
	     (format nil "((_ zero_extend ~a) ~a)"
		     (number (argument (operator expr)))
		     (translate-to-smt2* argument bindings)))
	    ((and (enum-adt? (find-supertype (type argument)))
		  (recognizer? operator))
	     (format nil "(= ~a ~a)"
		     (translate-to-smt2* argument bindings)
		     (translate-to-smt2* (constructor operator) bindings)))
	    ((constructor? operator)
	     (format nil "(~a ~{ ~a~})"
		     (translate-to-smt2* operator bindings)
		     (translate-to-smt2* (arguments expr) bindings)))
	    (t
	     (let ((smt2-interpretation
		    (smt2-interpretation operator)))
	       (if smt2-interpretation
		   (format nil "(~a ~{ ~a~})"
			   smt2-interpretation
			   (translate-to-smt2* (arguments expr) bindings))
		   (let* ((smt-op
			   (translate-to-smt2* operator bindings))
			  (arg (argument expr))
			  (args (if (tuple-expr? arg)
				    (arguments expr)
				    (let ((stype (find-supertype (type arg))))
				      (if (tupletype? stype)
					  (if (and (variable? arg)
						   (assoc arg bindings
							  :test
							  #'same-declaration))
					      arg
					      (loop for index from 1 to (length (types stype))
						    collect (make-projection-application index arg)))
					  (list arg)))))
			  (smt2args (if (and (variable? args)
					     (tupletype? (find-supertype (type arg))))
					(let ((bnd (assoc arg bindings
							  :test
							  #'same-declaration)))
					  (cdr bnd))
					(translate-to-smt2* args
							    bindings))))
		     (if (funtype? (find-supertype (type operator))) ;; do we need find-supertype?
			   (format nil "(select ~a ~{ ~a ~} )" smt-op smt2args)
			   (format nil "(~a ~{ ~a~})" smt-op smt2args))))))))))


(defun translate-smt2-bindings (bind-decls bindings prefix-string vartypepairs)
  (cond ((consp bind-decls)
	 (let ((smt2name (smt2-name (car bind-decls)))
	       (smt2type (translate-to-smt2* (type (car bind-decls)) bindings)))
	   (translate-smt2-bindings (cdr bind-decls)
				    (cons (cons (car bind-decls)
						smt2name)
					  bindings)
				    (format nil "~a (~a ~a)"
					    prefix-string smt2name smt2type
					    )
				    (cons (list smt2name smt2type) vartypepairs))))
	(t (values bindings prefix-string vartypepairs))))


(defmethod translate-to-smt2* ((expr binding-expr) bindings)
  (with-slots ((expr-bindings bindings) expression) expr
    (let ((stype (find-supertype (type (car expr-bindings)))))
      (multiple-value-bind (newbindings bindstring vartypepairs)
	  (translate-smt2-bindings expr-bindings bindings "" nil)
	(let ((smt2expression (translate-to-smt2* expression newbindings)))
	  (cond ;; ((lambda-expr? expr)) - don't care?
	    ((forall-expr? expr)
	     (loop for pair in vartypepairs
		   do
		   (push (format nil "(declare-const ~a ~a)" (car pair) (cadr pair))
			 *smt2defns*))
	     (format nil "(forall (~a) ~a)"
		     bindstring smt2expression)
	     )
	    ((exists-expr? expr)
	     (loop for pair in vartypepairs
		   do
		   (push (format nil "(declare-const ~a ~a)" (car pair) (cdr pair))
			 *smt2defns*))
	     (format nil "(exists (~a) ~a)"
		     bindstring smt2expression))))))))


(defmethod translate-to-smt2* ((expr update-expr) bindings)
  (translate-smt2-assignments (assignments expr)
			      (translate-to-smt2* (expression expr) bindings)
			      (type expr)
			      bindings))



(defun translate-smt2-assignments (assigns 
				   trbasis type bindings)
  (if assigns
      (translate-smt2-assignments (cdr assigns)
				  (translate-smt2-assignment (car assigns)
							     trbasis
							     type
							     bindings)
				  type
				  bindings)
      trbasis))


(defun translate-smt2-assignment (assign trbasis type bindings)
  (translate-smt2-assign-args (arguments assign)
			      (expression assign)
			      trbasis
			      (find-supertype type)
			      bindings))


(defmethod translate-smt2-assign-args (args value trbasis (type recordtype)
				       bindings)
  (if args
      (let* ((label (id (caar args)))
	     
	     (field-decls (fields type))
	     (field-types (loop for fld in field-decls collect (find-supertype (type field-decls))))
	     (super-field-types (loop for fld in field-decls
				      as ft in field-types
				      collect (mk-field-decl (id fld) ft)))
	     (dummy-record-type (mk-recordtype super-field-types nil))
	     (hashentry (gethash dummy-record-type *smt2-recordtype-hash*)))
	(unless hashentry
	  (let* ((smt2-recordtype-name (gentemp "smtrecord")))
	    (push (format nil "(declare-datatypes () ((~a (mk-~a ~{ ~a~}))))"
			  smt2-recordtype-name
			  (loop for fld in field-decls
				as fldtyp in super-field-types
				collect (format nil "(~a ~a)"
						(id fld)
						(translate-to-smt2* fldtyp bindings))))
		  *smt2-record-datatype*)
	    (setf (gethash dummy-record-type *smt2-recordtype-hash*&)
		  smt2-recordtype-name)))
	(format nil "((_ update-field ~a) ~a ~a)" label trbasis
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (type (find (id (caar args)) (fields type)
			     :test #'eq :key #'id))
		 bindings)))
      (translate-to-smt2* value bindings)))


;;recursion through the arguments while creating a new constant for the new tuple that
;;is equal to the old tuple except at the point of update where it is equal to the
;;translated value.  
(defmethod translate-smt2-assign-args (args value trbasis (type tupletype)
				       bindings)
  (if args
      (let* ((index (car (number (caar args))))
	     (types  (types type))
	     (super-types (loop for typ in types
				collect (find-supertype typ)))
	     (dummy-tuple-type (mk-tupletype super-types))
	     (hashentry (gethash dummy-tuple-type *smt2-recordtype-hash*)))
	(unless hashentry
	  (let* ((smt2-tupletype-name (gentemp "smttuple")))
	    (push (format nil "(declare-datatypes () ((~a (mk-~a ~{ ~a~}))))"
			  smt2-tupletype-name
			  (loop for fld in field-decls
				as typ in super-types
				as i from 1
				collect (format nil "(project_~a ~a)"
						i
						(translate-to-smt2* typ bindings))))
		  *smt2-record-datatype*)
	    (setf (gethash dummy-tuple-type *smt2-recordtype-hash*)
		  smt2-tupletype-name)))
	(format nil "((_ update-field project_~a) ~a ~a)" index trbasis
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (type (find (id (caar args)) (fields type)
			     :test #'eq :key #'id))
		 bindings)))
      (translate-to-smt2* value bindings)))


(defmethod translate-smt2-assign-args (args value trbasis (type funtype)
				       bindings)
  (if args
      (let* ((smt2args1 (translate-to-smt2* (car args) bindings))
	     (newtrbasis (format nil "(select ~a ~a)" trbasis smt2args1)) )
	(format nil "(store ~a ~a ~a)" trbasis smt2args1
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (range type) bindings)))
      (translate-to-smt2* value bindings)))



(defmethod translate-smt2-assign-args (args value trbasis (type t) bindings)
  (declare (ignore args trbasis))
  (translate-to-smt2* value bindings))


(defun smt2-interpretation (name-expr)
  (when (name-expr? name-expr)
    (let* ((id-assoc (cdr (assoc (id name-expr) *smt2-interpreted-names*)))
	   (mod-assoc (cdr (assoc (id (module-instance
				       (resolution name-expr)))
				  id-assoc))))
      mod-assoc)))



;; TODO: update this to arbitrary solver executables
(defun find-smt2-executable ()
  (or *smt2-executable*
      (cond ;; ((and (pvs-context-yices-executable) ;; what is this case?
	    ;; 	  (program-version (pvs-context-yices-executable) "1"))
	    ;;  (break "case1")
	    ;;  (setq *smt2-executable* "(pvs-context-smt2-executable)"))
	    ((program-version "z3 --version" "Z3")
	     (break "z3 case")
	     (setq *smt2-executable* "z3"))
	    ((program-version "cvc5 --version" "cvc5")
	     (break "cvc5 case")
	     (setq *smt2-executable* "cvc5"))
	    (t (format t "~%Neither z3 nor cvc5 can be found on your path.")))))


;; (defun get-yices2-executable-path (path)
;;   (cond ((program-version (concatenate 'string path " --version") "Yices 2")
;; 	 (setq *yices-executable* path))
;; 	(t (format t "~%Invalid path to Yices 2 executable")
;; 	   (let ((npath (pvs-dialog "~%Please enter the path to Yices 2: ")))
;; 	     (get-yices2-executable-path npath)))))


(defun smt2 (sformnums nonlinear?);;NSH(8-25-10) Added nonlinear? flag to use nlyices
  #'(lambda (ps)                   ;;this handles only arithmetic and uninterpreted
      ;;functions
      (let* ((goalsequent (current-goal ps))
	     (s-forms (select-seq (s-forms goalsequent) sformnums))
	     (*smt2defns* nil)
	     (*smt2datatype-warning* nil)
	     (*smt2-conditions* nil)
	     (*smt2-subtype-constraints* nil)
	     (*translate-to-smt2-hash* (make-pvs-hash-table))
	     (*smt2name-hash* (make-pvs-hash-table)))
;	(find-smt2-executable)
	(assert *smt2-executable*)
	;; (clear-smt2)	
	(let ((smt2-forms
	       (loop for sf in s-forms
		     collect
		     (let ((fmla (formula sf)))
		       (if (negation? fmla)
			   (format nil "(assert ~a)"
				   (translate-to-smt2* (args1 fmla) nil))
			   (format nil "(assert (not ~a))"
				   (translate-to-smt2* fmla nil))))))
	      (revdefns (nreverse *smt2defns*))
	      (file (make-pathname :defaults (working-directory)
				   :name (label ps) :type "smtlib2")))
	  (format-if "~%smt2defns = ~% ~{~a~%~}" revdefns)
	  (format-if "~%smt2subtypes = ~% ~{~a~%~}" *smt2-subtype-constraints*)
	  (format-if "~%smt2forms = ~% ~{~a~%~}" smt2-forms)
	  (with-open-file (stream  file :direction :output
				   :if-exists :supersede)
	    (format stream "~{~a ~%~}" revdefns)
	    (unless nil ;;nonlinear?
	      (format stream "~{~a ~%~}" *smt2-subtype-constraints*))
	    (format stream "~{~a ~%~}" smt2-forms)
	    (format stream "(check-sat)~%")
					;(unless nonlinear? (format stream "(status)"))
	    )

	  (format-if "~%Command: ~a" (format nil "~a ~a"
		       *smt2-executable*
		       (namestring file)))
	  
	  (multiple-value-bind (output err-output status)
	      (uiop:run-program
	       (let* ((options (cond ((eq *smt2-executable* "cvc5") "--lang smtlib2 --quiet")
				     (t ""))))
	       (format nil "~a ~a ~a"
		       *smt2-executable*
		       options
		       (namestring file)))
	       :output '(:string :stripped t)
	       :ignore-error-status t)
;;	    (break "run-smt2")
	    (cond ((zerop status)
		   (format-if "~%Result = ~a" output err-output)
		   (cond ((search "unsat" output)
			  (format-if "~%SMT translation of negation is unsatisfiable")

			   (format-if "~%Removing generated .smtlib2 queries")
			    (uiop:run-program
			     (format nil "rm *.smtlib2")
			     :input "//dev//null"
			     :output '(:string :stripped t)
			     :ignore-error-status t)

			  
			  (values '! nil nil))
			 (t (format-if "~%SMT translation of negation is not known to be satisfiable or unsatisfiable")
			    (values 'X nil nil))))
		  (t (format t
			     "~%Error running the solver - you may need to do one or more of:~
	                            ~% 1. Re-download/reconfigure the solver
	                            ~% 2. Add the solver executable to the path
	                            ~%The error message is:~% ~a"
			     err-output)
		     (values 'X nil))))))))



(addrule 'smt2 () ((fnums *) nonlinear?)
	 (smt2 fnums nonlinear?)
	 "Invokes an external endgame SMT solver to prove that the conjunction
of the negations of the selected formulas is unsatisfiable. "
	 "~%Simplifying with SMT,")




(defstep smt2-with-rewrites
    (&optional (fnums *) defs theories rewrites exclude-theories exclude)
  (then (simplify-with-rewrites fnums defs theories rewrites exclude-theories exclude)
	(smt2 fnums))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums) followed
by (smt2 fnums), then turns off all the installed rewrites.  Examples:
 (smt2-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (smt2-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, applying external SMT solver, and disabling installed rewrites")


(defstep smt2simp (&optional (fnums *) nonlinear?)
  (then (skosimp*)(smt2 :fnums fnums :nonlinear? nonlinear?))
  "Repeatedly skolemizes and flattens, and then applies an external SMT solver"
  "Repeatedly skolemizing and flattening, and then invoking an external SMT solver")



(defstep smt2grind (&optional (defs !); nil, t, !, explicit, or explicit!
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
			      nonlinear?)
  (then (install-rewrites$ :defs defs :theories theories
			   :rewrites rewrites :exclude exclude)
	(repeat* (bash$ :if-match if-match :updates? updates?
			:polarity? polarity? :instantiator instantiator
			:let-reduce? let-reduce?
			:quant-simp? quant-simp?
			:implicit-typepreds? implicit-typepreds?
			:cases-rewrite? cases-rewrite?))
	(smt2 :nonlinear? nonlinear?))
  "Core of GRIND: Installs rewrites, repeatedly applies BASH, and then
   invokes an external SMT solver.  See BASH for more explanation."
  "Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization, dispatch to external SMT solver")

