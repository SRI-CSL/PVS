;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-yices2.lisp -- 
;; Author          : N. Shankar
;; Created On      : Dec. 5, 2010
;; Last Modified By: N. Shankar
;; Last Modified On: 
;; Update Count    : 
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

;;NSH(12/5/10): Adapting translate-to-prove-yices to yices2 output. 

(defvar *y2bindings* nil)
(defvar *y2embeddedf* nil)
(defvar *y2defns* nil)
;;(defvar *y2datatype-warning* nil)
(defvar *y2name-hash* (make-pvs-hash-table))
(defvar *translate-to-yices2-hash* (make-pvs-hash-table))
(defvar *yices2-call* "yices2 --timeout=300")
(defvar *yices2-id-counter*)  ;;needs to be initialized in eproofcheck
(defvar *yices2-conditions* nil)
(defvar *yices2-subtype-constraints* nil)

(newcounter *yices2-id-counter*)

(defparameter *yices2-interpreted-names*
  '((=  (|equalities| . =))
    (/=  (|notequal| . /=))
    (TRUE (|booleans| . true))
    (FALSE (|booleans| . false))
    (IMPLIES  (|booleans| . =>))
    (=>  (|booleans| . =>))
    (AND (|booleans| . and) (|bv_bitwise| . bv-and))
    (OR  (|booleans| . or) (|bv_bitwise| . bv-or))
    (NOT  (|booleans| . not)(|bv_bitwise| . bv-not))
    (+  (|number_fields| . +)(|bv_arith_nat| . bv-add))
    (- (|number_fields| . -)(|bv_arithmetic_defs| . bv-sub))
    (*   (|number_fields| . *))
    (/  (|number_fields| . /))
;    (rem (|modulo_arithmetic| . mod))
;    (ndiv (|modulo_arithmetic| . div))
    (< (|reals| . <)(|bv_arith_nat| . bv-lt))
    (<=  (|reals| . <=)(|bv_arith_nat| . bv-le))
    (> (|reals| . >)(|bv_arith_nat| . bv-gt))
    (>=  (|reals| . >=)(|bv_arith_nat| . bv-ge))
    (O (|bv_concat_def| . bv-concat))
    (& (|booleans| . and)(|bv_bitwise| . bv-and))
    (XOR  (|bv_bitwise| . bv-xor))
    (^ (|bv_caret| .  bv-extract))
    (sign_extend   (|bv_extend| . bv-sign-extend))
    (|bv_slt| (|bv_arithmetic_defs| . bv-slt))
    (|bv_sle| (|bv_arithmetic_defs| . bv-sle))
    (|bv_sgt| (|bv_arithmetic_defs| . bv-sgt))
    (|bv_sge| (|bv_arithmetic_defs| . bv-sge))
    (|bv_splus| (|bv_arithmetic_defs| . bv-add))
    (|bv_stimes| (|bv_arithmetic_defs| . bv-mul))
    ))

(defun clear-yices2 ()
  (setq *y2defns* nil)
  (clrhash *y2name-hash*)
  (clrhash *translate-to-yices2-hash*)
  (newcounter *yices2-id-counter*))


;;We need to add constant declarations and type declarations.
;;Type definitions will be needed for scalars and datatypes.

;; (defstruct ytype (name def))
;; (defstruct yconst (name type))

(defun yices2-name (expr &optional id) ;;expr must have id field,
                                      ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *y2name-hash*)))
	(or entry
	    (let ((name (yices2-id-name (or id (id expr)))))
	      (setf (gethash expr *y2name-hash*) name)
	      name)))))

(defun yices2-id-name (id)
  (intern
   (concatenate 'string
     (string (if (integerp id)
		 (format nil "~r"
		   id)
		 id))
     "_"
     (princ-to-string
      (funcall
       *yices2-id-counter*)))))

;;Next, we translate PVS types to Yices types so that any enumerated
;;types or datatypes introduce names.

(defun yices2-type-name (expr)
  (yices2-name expr))

;;Yices2 doesn't have a datatypes theory. 

;; (defun translate-to-yices2-scalar-constructors (constructors)
;;   (loop for constr in constructors collect (yices2-name constr)))

;; (defun translate-to-yices2-scalar (yices-type-name constructors)
;;   (let ((yices-constructors
;; 	 (translate-to-yices2-scalar-constructors constructors)))
;;     (push (format nil "(define-type ~a (scalar ~{~a ~}))"
;; 	    yices-type-name yices-constructors)
;; 	  *y2defns*)
;;     (loop for cnstr in constructors
;; 	  do (let* ((recognizer (recognizer cnstr))
;; 		    (yices-recognizer (yices2-name recognizer)))
;; 	       (push (format nil "(define ~a::(-> x::~a bool) (lambda (x::~a) (= x ~a)))" 
;; 		       yices-recognizer
;; 		       yices-type-name
;; 		       yices-type-name
;; 		       (yices2-name cnstr))
;; 		     *y2defns*)))
;;     yices-type-name))


;; (defun translate-to-yices2-accessors (accessors bindings)
;;   (cond ((consp accessors)
;; 	 (format nil "~a::~a ~a"
;; 	   (yices2-name (car accessors))
;; 	   (translate-to-yices2* (range (type (car accessors))) bindings)
;; 	   (translate-to-yices2-accessors (cdr accessors) bindings)))
;; 	(t (format nil ""))))

;; (defun translate-to-yices2-constructor (constructor bindings)
;;   (let ((accessors (accessors constructor)))
;;     (if accessors 
;; 	(format nil "(~a ~a)"
;; 	  (yices2-name constructor)
;; 	  (translate-to-yices2-accessors (accessors constructor) bindings))
;; 	(format nil "~a" (yices2-name constructor)))))

;; (defun translate-to-yices2-constructors (constructors bindings)
;;   (cond ((consp constructors)
;; 	 (cons (translate-to-yices2-constructor (car constructors) bindings)
;; 	       (translate-to-yices2-constructors (cdr constructors) bindings)))
;; 	(t nil)))

;; (defun translate-to-yices2-subdatatype-constructors
;;   (constructors superconstructors bindings)
;;   (cond ((consp constructors)
;; 	 (cons (translate-to-yices2-subdatatype-constructor (car constructors)
;; 							   (car superconstructors)
;; 							   bindings)
;; 	       (translate-to-yices2-subdatatype-constructors (cdr constructors)
;; 							    (cdr superconstructors)
;; 							    bindings)))
;; 	(t nil)))

;; (defun translate-to-yices2-subdatatype-constructor
;;   (constructor superconstructor bindings)
;;   (let* ((cname (yices2-name constructor))
;; 	 (accessors (accessors constructor))
;; 	 (ctype (if (> (length accessors) 1)
;; 		    (format nil "(-> ~{~a ~}~a)"
;; 			  (translate-to-yices2* (types (domain (type constructor)))
;; 					       bindings)
;; 			  (translate-to-yices2* (range (type constructor))
;; 					       bindings))
;; 		    (translate-to-yices2* (type constructor) bindings)))
;; 	 (acc-ids (loop for acc in accessors
;; 			as i from 1
;; 			collect (format nil "x~a" i)))
;; 	 (cdef-arglist (if (> (length accessors) 1)
;; 			   (loop for acc-id in acc-ids
;; 			     as ty in (types (domain (type constructor)))
;; 			     collect (format nil "~a::~a"
;; 				       acc-id
;; 				       (translate-to-yices2* ty
;; 							    bindings)))
;; 			   (if (eql (length accessors) 1)
;; 			       (format nil "x::~a" (translate-to-yices2* (domain (type constructor)) bindings))
;; 			       nil)))
;; 	 (cdef (format nil "(lambda (~{~a ~}) (~a~{ ~a~}))" cdef-arglist
;; 		       (yices2-name superconstructor)
;; 		       acc-ids))
;; 	 (defn-string (format nil "(define ~a::~a ~a)"
;; 			cname
;; 			ctype
;; 			(if accessors cdef (yices2-name superconstructor))))
;; 	 (recognizer-defn-string (format nil "(define ~a::~a ~a)"
;; 			(format nil "~a?" cname)
;; 			(translate-to-yices2* (type (recognizer constructor)) bindings)
;; 			(format nil "~a?" (yices2-name superconstructor)))))
;;     (push recognizer-defn-string *y2defns*)
;;     (push defn-string *y2defns*)
;;     (translate-to-yices2-subdatatype-accessors accessors
;; 					      (accessors superconstructor)
;; 					      bindings)
;;     cname)  )

;; (defun translate-to-yices2-subdatatype-accessors (accessors superaccessors bindings)
;;   (cond ((consp accessors)
;; 	 (push (format nil "(define ~a::~a ~a)"
;; 		 (yices2-name (car accessors))
;; 		 (translate-to-yices2* (type (car accessors)) bindings)
;; 		 (yices2-name (car superaccessors)))
;; 	       *y2defns*)
;; 	 (translate-to-yices2-subdatatype-accessors (cdr accessors)
;; 						   (cdr superaccessors)
;; 						   bindings))
;; 	(t nil)))

(defmethod translate-to-yices2* ((ty type-name) bindings)
  (let ((yname-hash (gethash ty *y2name-hash*)))
    (or yname-hash 
	(cond ((enum-adt? ty)
	       (let ((constructors (constructors ty))
		     (yname (yices2-name ty)))	;;bindings can be ignored
		 (translate-to-yices2-scalar yname constructors) ))
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "bool"))
	      ((tc-eq ty *number*) "real")
		;;else uninterpreted type
	      (t (let ((yname (yices2-name ty)))
		  (push (format nil "(define-type ~a)" yname) *y2defns*)
		  yname))))))

;; (defmethod translate-to-yices2* ((ty datatype-subtype) bindings)
;;   (let ((yname-hash (gethash ty *y2name-hash*)))
;;     (or yname-hash 
;; 	(let* ((supertype (supertype ty))
;; 	       (ysupertype (translate-to-yices2* supertype bindings))
;; 	       (ytypename (yices-type-name ty))
;; 	       (constructors (constructors ty))
;; 	       (superconstructors (constructors supertype))
;; 	       (defn-string
;; 		 (format nil "(define-type ~a ~a)"
;; 		   ytypename ysupertype)))
;; 	  (push defn-string *y2defns*)
;; 	  (translate-to-yices2-subdatatype-constructors
;; 	   constructors superconstructors bindings)
;; 	  ytypename))))
    


(defun yices-format-tuple-list (id len)
  (loop for i  from 0 to (1- len)
	collect (format nil "(select ~a ~a)" id i)))


;;yices2 does not have predicate subtypes, so we should assert all the
;;subtype constraints on each subexpression.  
(defmethod translate-to-yices2* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ;;((tc-eq ty *naturalnumber*) 'nat) ;;there is no nat
	  ((tc-eq ty *integer*) 'int)
	  ((tc-eq ty *real*) 'real)
	  (t (translate-to-yices2* supertype bindings)))))

(defmethod translate-to-yices2* ((ty tupletype) bindings)
  (format nil "(tuple ~{~a ~})" (translate-to-yices2-list (types ty) nil nil bindings)))

;; (defmethod translate-to-yices2* ((ty list) bindings)
;;   (translate-to-yices2-list ty nil nil bindings))


(defun translate-to-yices2-funlist (list lastelem 
					domain  bindings)
  (translate-to-yices2-funlist* list nil 1 lastelem domain bindings))
  

(defun translate-to-yices2-funlist* (list accum num lastelem 
					domain bindings)	 
  (if (consp list)
      (let* ((ycar (translate-to-yices2* (car list)
				       bindings))
	     (ycarbnd ycar))
	(translate-to-yices2-funlist* (cdr list)
				     (cons ycarbnd
					   accum)
				     (1+ num)
				     lastelem
				     domain
				     bindings
				     ))
      (let ((accum (nreverse accum)))
      (format nil "(-> ~{ ~a~} ~a)"
	accum
	(translate-to-yices2* lastelem
			     bindings)))))


(defun translate-to-yices2-list (list accum lastelem bindings)
  (if (consp list)
      (translate-to-yices2-list (cdr list)
			       (cons (translate-to-yices2* (car list)
							  bindings)
				     accum)
			       lastelem
			       bindings)
      (if lastelem
	  (nreverse (cons lastelem
			  accum))
	  (nreverse accum))))

(defmethod translate-to-yices2* ((ty recordtype) bindings)
  (format nil "(tuple ~{~a ~})"
    (translate-to-yices2-list (fields ty) nil nil bindings)))

(defmethod translate-to-yices2* ((ty field-decl) bindings)
  (translate-to-yices2* (type ty) bindings))

(defmethod translate-to-yices2* ((ty dep-binding) bindings)
  (translate-to-yices2* (type ty) bindings))

(defmethod translate-to-yices2* ((ty funtype) bindings)
  (with-slots (domain range) ty
	      (let ((sdom (if (dep-binding? domain)  ;;NSH(4-16-11)
			      (find-supertype (type domain))
			    (find-supertype domain))))
	      (format nil "(->~{ ~a~})"
		      (translate-to-yices2-list (types sdom) nil
						(translate-to-yices2* range bindings)
						bindings)))))


;;name-exprs and binding-exprs are not hashed in binding contexts.
;;*bound-variables* are the bound variables in the calling context, and
;;*bindings* are the ones built up locally.
(defmethod translate-to-yices2* :around ((obj type-expr) bindings)
	   (if (or *bound-variables* *bindings*)
	       (call-next-method)
	       (let ((hashed-value (gethash obj *translate-to-yices2-hash*)))
		 (or hashed-value
		     (let ((result (call-next-method)))
		       (setf (gethash obj *translate-to-yices2-hash*)
			     result)
		       result)))))
    
(defmethod translate-to-yices2* :around ((obj expr) bindings)
	   (if (or *bound-variables* *bindings*)
	       (call-next-method)
	       (let ((hashed-value (gethash obj *translate-to-yices2-hash*)))
		 (or hashed-value
		     (let* ((result (call-next-method))
			    (type-constraints (type-constraints obj :none))
			    (rtype-constraints (loop for fmla in type-constraints
						     nconc (and+ fmla))))
		       (setf (gethash obj *translate-to-yices2-hash*)
			     result)
		       (loop for tc in rtype-constraints
			     do (let* ((ytc (translate-to-yices2* tc bindings))
				      (yclause (if *yices2-conditions*
						   (format nil "(assert (implies (and ~{ ~a~}) ~a))"
						       *yices2-conditions*
						       ytc)
						 (format nil "(assert ~a)" ytc))))
				  (push yclause *yices2-subtype-constraints*)))
		       result)))))


(defmethod translate-to-yices2* ((list list) bindings)
  (cond ((consp list)
	 (cons (translate-to-yices2* (car list) bindings)
	       (translate-to-yices2* (cdr list) bindings)))
	(t nil)))

;;doubt if this gets invoked
(defun yices2-recognizer (name bindings)
  (when (recognizer? name)
       (let ((ytype (translate-to-yices2* (type name)
						bindings)))
	 (format nil "~a?" (translate-to-yices2* (constructor name) bindings) ))))


(defmethod translate-to-yices2* ((expr name-expr) bindings)
  (let ((bpos (assoc expr bindings
		     :test #'same-declaration)))
    (if bpos (if (consp (cdr bpos))
		 (format nil "(mk-tuple ~{ ~a~})" (cdr bpos))
		 (cdr bpos))
	(let* ((yname-hashentry (gethash expr *y2name-hash*)))
	  (or yname-hashentry
	      (yices2-interpretation expr)
	      ;(eta-expanded-yices2-interpretation expr)
	      (yices2-recognizer expr bindings)
	      (let* ((ytype (translate-to-yices2* (type expr)
						bindings))
		     (yname-hashentry (gethash expr *y2name-hash*)))
		(or yname-hashentry
		    (let* ((yname (yices2-name expr))
			  (defn (format nil "(define ~a::~a)"
				  yname
				  ytype)))
		      (push defn
			    *y2defns*)
		      (format-if "~%Adding definition: ~a" defn)
		      yname))))))))



(defmethod translate-to-yices2* ((expr constructor-name-expr) bindings)
  (call-next-method (lift-adt expr) bindings))


(defmethod translate-to-yices2* ((expr number-expr) bindings)
      (number expr))

(defmethod translate-to-yices2* ((ex string-expr) bindings)
      (string->int (string-value ex)))

(defmethod translate-to-yices2* ((expr record-expr) bindings)
  (format nil "(mk-tuple ~{ ~a~})"
    (translate-to-yices2* (sort-assignment (assignments expr)) bindings)))

(defmethod translate-to-yices2* ((expr tuple-expr) bindings)
  (format nil "(mk-tuple ~{ ~a~})"
    (translate-to-yices2* (exprs expr) bindings)))

;;the conditions are pushed into *yices2-conditions* for generating
;;subtype constraint assertions to yices that are guarded by the conditions.
(defmethod translate-to-yices2* ((expr branch) bindings)
  (let ((ycondition (translate-to-yices2* (condition expr) bindings)))
  (format nil "(if ~a ~a ~a)"
    ycondition
    (let ((*yices2-conditions* (push ycondition *yices2-conditions*)))
      (translate-to-yices2* (then-part expr) bindings))
    (let ((*yices2-conditions* (push `(not ,ycondition) *yices2-conditions*)))
      (translate-to-yices2* (else-part expr) bindings)))))

(defmethod translate-to-yices2* ((expr cases-expr) bindings)
  (translate-to-yices2* (translate-cases-to-if expr) bindings))

;; can't translate standalone projection-exprs
(defmethod translate-to-yices2* ((expr projection-expr) bindings)
  (let* ((id (make-new-variable '|x| expr))
	 (yid (yices-id-name id))
	 (ytype (translate-to-yices2*
		(domain (find-supertype (type expr)))
		bindings)))
  (format nil "(select ~a ~a)"
    yid ytype yid (index expr))))


(defmethod translate-to-yices2* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (if (variable? argument)
	(let ((bnd (assoc argument bindings
			  :test #'same-declaration)))
	  (if (and bnd (consp (cdr bnd)))
	      (nth (1- index) (cdr bnd))
	      (format nil "(select ~a ~a)"
		(translate-to-yices2* argument bindings)
		(index expr))))
	(format nil "(select ~a ~a)"
		(translate-to-yices2* argument bindings)
		(index expr)))))

    

(defmethod translate-to-yices2* ((expr field-application) bindings)
  (with-slots (id argument type) expr
	      (let* ((fields (fields (find-supertype (type argument))))
		     (sfields   fields)
		     (pos (position id sfields :key #'id)))
    (format nil "(select ~a ~a)"
      (translate-to-yices2* argument bindings)
      pos))))

    ;;NSH(5.17.94): Complicated code to deal with tuple mismatch
    ;;between domain of operator and arguments.
    ;; op(a1,..., an) if dom(type(op)) = [t1,...,tn] ==>
    ;;                      (op' (tupcons a1' .. an'))
    ;; op(a1), if dom(type(op)) = t1,...,tn ==> (op (tupsel 0 a1)...)

(defmethod translate-to-yices2* ((expr application) bindings)
  (with-slots (operator argument) expr
    (let* ((op* (operator* expr))
	   (op-id (when (name-expr? op*) (id op*))))
      (cond ((and (eq op-id 'rem)
		  (eq (id (module-instance (resolution op*)))
		      'modulo_arithmetic))
	     (let ((denom (translate-to-yices2* (args1 (operator expr))
					       bindings))
		   (numer (translate-to-yices2* (args1 expr)
					       bindings)))
	       `(mod ,numer ,denom)))
	    ((and (eq op-id '-)  ;;NSH(4-19-10)
		  (unary-application? expr)
		  (eq (id (module-instance (resolution op*)))
		      '|number_fields|))
	     (format nil "(- 0 ~a)" (translate-to-yices2* (argument expr) bindings)))
	    ((and (eq op-id 'nat2bv)
		  (number-expr? (expr (car (actuals (module-instance op*))))))
	     (let ((size (translate-to-yices2*
		  (expr (car (actuals (module-instance op*))))
			  bindings))
		   (num (translate-to-yices2*
			 (args1 expr) bindings)))
	       `(mk-bv ,size ,num)))
	    ((and (eq op-id '-)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_arithmetic_defs|)
		  (not (tupletype? (domain (type op*)))))
	     (format nil "(bv-neg ~a)"
	       (translate-to-yices2* (argument expr) bindings)))
	    ((and (eq op-id '^)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_caret|)
		  (tuple-expr? argument)
		  (tuple-expr? (cadr (exprs argument)))
		  (number-expr? (car (exprs (cadr (exprs argument)))))
		  (number-expr? (cadr (exprs (cadr (exprs argument))))))
	     (format nil "(bv-extract ~a ~a ~a)"
	       (number (car (exprs (cadr (exprs argument)))))
	       (number (cadr (exprs (cadr (exprs argument)))))
	       (translate-to-yices2* (car (exprs argument)) bindings)))
	    ((and (enum-adt? (find-supertype (type argument)))
		  (recognizer? operator))
	     (format nil "(= ~a ~a)"
	       (translate-to-yices2* argument bindings)
	       (translate-to-yices2* (constructor operator) bindings)))
	    ((constructor? operator)
	     (format nil "(~a ~{ ~a~})"
		     (translate-to-yices2* operator bindings)
		     (translate-to-yices2* (arguments expr) bindings)))
	    (t
	     (let ((yices2-interpretation
		    (yices2-interpretation operator)))
	       (if yices2-interpretation
		   (format nil "(~a ~{ ~a~})"
		     yices2-interpretation
		     (translate-to-yices2* (arguments expr) bindings))
		   (let* ((yices-op (translate-to-yices2* operator bindings))
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
			  (yargs (if (and (variable? args)
					  (tupletype? (find-supertype (type arg))))
				     (let ((bnd (assoc arg bindings
							  :test
							  #'same-declaration)))
				       (cdr bnd))
				     (translate-to-yices2* args
							  bindings))))
		     (format nil "(~a ~{ ~a~})" yices-op yargs)))))))))


(defun translate-yices-bindings (bind-decls bindings prefix-string)
      (cond ((consp bind-decls)
	     (let ((yname (yices2-name (car bind-decls)))
		   (ytype (translate-to-yices2* (type (car bind-decls)) bindings)))
	       (translate-yices-bindings (cdr bind-decls)
					 (cons (cons (car bind-decls)
						     yname)
					       bindings)
					 (format nil "~a ~a::~a"
					   prefix-string yname ytype))))
	    (t (values bindings prefix-string))))

;;yices2 does not have binding expressions so these will be treated as
;;uninterpreted constants. 
(defmethod translate-to-yices2* ((expr binding-expr) bindings)
  (let ((entry (gethash expr *y2name-hash*)))
    (or entry 
	(let* ((yname (yices2-id-name "y2id"))
	       (ytype (translate-to-yices2* (type expr) bindings))
	       (defn (format nil "(define ~a::~a)" yname ytype)))
	  (push defn
		*y2defns*)
	  (format-if "~%Adding definition: ~a" defn)
	  yname))))
  
(defmethod translate-to-yices2* ((expr forall-expr) bindings)
  (call-next-method))

;; (if *yqexpand*
;;       (with-slots ((expr-bindings bindings) expression) expr
;; 	(translate-forall-to-yices expr-bindings expression bindings))
;;       (call-next-method) ; should call binding-expr
;;       ))



;;; Update expressions
;;; Translate expressions of the form
;;; A WITH [ (0) := 1 ],
;;;    where A is an array of type int->int, into
;;; (apply int ARRAYSTORE A 0 1)
;;;
;;; f WITH [ (0,0) := 0],
;;;    where f is a function of type int,int->int into
;;; (APPLY int UPDATE f (0 0) 0)
;;;
;;; g WITH [ (0) := h, (1) (x,y) := 0, (1) (x,y)' := 1 ]
;;;    where g and h are functions of type
;;;    T = [function[int -> function[state[T0],state[T0] -> int]]
;;;
;;; This generates the form
;;;
;;; (apply function[state[T0],state[T0] -> int]
;;;        update
;;;        (apply function[state[T0],state[T0] -> int]
;;;               update
;;;               (apply function[state[T0],state[T0] -> int]
;;;                      update
;;;                      g (0) h)
;;;               (1) (apply int update g(1) (x y) 0))
;;;        (1) (apply int update g(1) (x' y') 1))

(defmethod translate-to-yices2* ((expr update-expr) bindings)
  (translate-yices2-assignments (assignments expr)
			       (translate-to-yices2* (expression expr) bindings)
			       (type expr)
			       bindings))

(defun translate-yices-assignments (assigns 
				    trbasis type bindings)
  (if assigns
      (translate-yices2-assignments
       (cdr assigns)
					;basis
       (translate-yices2-assignment (car assigns) ;basis
				   trbasis type bindings)
       type
       bindings)
      trbasis))

(defun translate-yices2-assignment (assign trbasis type bindings)
  (translate-yices2-assign-args (arguments assign)
			       (expression assign)
			       trbasis
			       (find-supertype type)
			       bindings))

(defmethod translate-yices2-assign-args (args value trbasis (type recordtype)
					     bindings)
  (if args
      (let* ((label (id (caar args)))
	     (newtrbasis (format nil "(select ~a ~a)" trbasis label)))
	(format nil "(update ~a ~a ~a)" trbasis label
		(translate-yices2-assign-args
		 (cdr args) value newtrbasis
		 (type (find (id (caar args)) (fields type)
			     :test #'eq :key #'id))
		 bindings)))
      (translate-to-yices2* value bindings)))

(defmethod translate-yices2-assign-args (args value trbasis (type tupletype)
					     bindings)
  (if args
      (let* ((index (number (caar args)))
	     (newtrbasis (format nil "(~a ~{~a~})" trbasis index)) )
	(format nil "(update ~a (~a) ~a)" trbasis index
		(translate-yices2-assign-args
		 (cdr args) value newtrbasis
		 (type (nth (1- index) (types type))) bindings)))
      (translate-to-yices2* value bindings)))

(defmethod translate-yices2-assign-args (args value trbasis (type funtype)
					     bindings)
  (if args
      (let* ((yargs1 (translate-to-yices2* (car args) bindings))
	     (newtrbasis (format nil "(~a ~a)" trbasis yargs1)) )
	(format nil "(update ~a ~a ~a)" trbasis yargs1
		(translate-yices2-assign-args
		 (cdr args) value newtrbasis
		 (range type) bindings)))
      (translate-to-yices2* value bindings)))

(defmethod translate-yices2-assign-args (args value trbasis (type t) bindings)
  (translate-to-yices2* value bindings))


(defun yices2-interpretation (name-expr)
  (when (name-expr? name-expr)
    (let* ((id-assoc (cdr (assoc (id name-expr) *yices2-interpreted-names*)))
	   (mod-assoc (cdr (assoc (id (module-instance
				       (resolution name-expr)))
				  id-assoc))))
      mod-assoc)))
;;       (if (eq (id name-expr) '-)
;; 	  (and (tupletype? (find-supertype (domain (type name-expr))))
;; 	       mod-assoc)
;; 	  mod-assoc))))


;; (defparameter *interpreted-alist*
;;   (mapcar #'(lambda (x) (cons (id x) x)) *interpreted-names*))

;; (defun interpretation (name)
;;   (or (cdr (assoc (id name) *interpretations*))
;;       (id name)))

;; (defparameter *yices-interpretations*
;;   '((= . =)
;;     (/= . /=)
;;     (< . <)
;;     (<= . <=)
;;     (> . >)
;;     (>= . >=)
;;     (+ . +)
;;     (- . -)
;;     (* . *)
;;     (/ . /)
;;     (AND . and)
;;     (OR . or)
;;     (NOT . not)
;;     (IMPLIES . =>)
;;     ( . bv-concat)
;;     ( . bv-extract)
;;     ( . bv-and)
;;     ( . bv-or)
;;     ( . bv-xor)
;;     ( . bv-not)
;;     ( . bv-left-shift0)
;;     ( . bv-left-shift1)
;;     ( . bv-right-shift0)
;;     ( . bv-right-shift1)
;;     ( . bv-sign-extend)
;;     ( . bv-add)
;;     ( . bv-sub)
;;     ( . bv-mul)
;;     ( . bv-neg)
;;     ( . bv-lt)
;;     ( . bv-le)
;;     ( . bv-gt)
;;     ( . bv-ge)
;;     ( . bv-slt)
;;     ( . bv-sle)
;;     ( . bv-sgt)
;;     ( . bv-sge)))

(defun yices2 (sformnums nonlinear?);;NSH(8-25-10) Added nonlinear? flag to use nlyices
  #'(lambda (ps)                   ;;this handles only arithmetic and uninterpreted
                                   ;;functions
      (let* ((goalsequent (current-goal ps))
	     (s-forms (select-seq (s-forms goalsequent) sformnums))
	     (*y2defns* nil)
	     (*y2datatype-warning* nil)
	     (*yices2-conditions* nil)
	     (*yices2-subtype-constraints* nil))
	(clear-yices2)
	(let ((yices-forms
	       (loop for sf in s-forms
		     collect
		     (let ((fmla (formula sf)))
		       (if (negation? fmla)
			   (format nil "(assert ~a)"
			     (translate-to-yices2* (args1 fmla) nil))
			   (format nil "(assert (not ~a))"
			     (translate-to-yices2* fmla  nil))))))
	      (revdefns (nreverse *y2defns*))
	      (file (make-pathname :defaults (working-directory)
				   :name (label ps) :type "yices")))
	  (format-if "~%ydefns = ~% ~{~a~%~}" revdefns)
	  (format-if "~%ysubtypes = ~% ~{~a~%~}" *yices2-subtype-constraints*)
	  (format-if "~%yforms = ~% ~{~a~%~}" yices-forms)
	  (with-open-file (stream  file :direction :output
				   :if-exists :supersede)
	    (format stream "~{~a ~%~}" revdefns)
	    (unless nonlinear? (format stream "~{~a ~%~}" *yices2-subtype-constraints*))
	    (format stream "~{~a ~%~}" yices-forms)
	    (format stream "(check)~%")
	    (unless nonlinear? (format stream "(status)")))
	  (let ((status nil)
		(tmp-file (pvs-tmp-file)))
	    (with-open-file (out tmp-file
				 :direction :output :if-exists :supersede)
	      (cond (nonlinear? (format out (check-with-yices (namestring file) t)) (setq status 0))
		    (t (setq status
			     #+allegro
			     (excl:run-shell-command
			      (format nil "~a ~a" *yices-call* (namestring file))
			      :input "//dev//null"
			      :output out
			      :error-output :output)
			     #+sbcl
			     (sb-ext:run-program
			      (format nil "~a ~a" *yices-call* (namestring file))
			      nil
			      :input "//dev//null"
			      :output out
			      :error out)
			     #+cmu
			     (extensions:run-program
			      (format nil "~a ~a" *yices-call* (namestring file))
			      nil
			      :input "//dev//null"
			      :output out
			      :error out)))))
	    (when *y2datatype-warning*
	      (format t "~70,,,'*A" "")
	      (format t "~%Warning: The Yices datatype theory is not currently trustworthy.
Please check your results with a proof that does not rely on Yices. ~%")
	      (format t "~70,,,'*A" ""))
	    (cond ((zerop status)
		   (let ((result (file-contents tmp-file)))
;;		     (break "yices result")
		     (delete-file tmp-file)
		     (delete-file file)
		     (format-if "~%Result = ~a" result)
		     (cond ((search "unsat"  result :from-end t)
			    (format-if "~%Yices translation of negation is unsatisfiable")
			    (values '! nil nil))
			   (t (format-if "~%Yices translation of negation is not known to be satisfiable or unsatisfiable")
			      (values 'X nil nil)))))
		  (t (format t
			 "~%Error running yices - you may need to do one or more of:~
                          ~% 1. Download yices from http://yices.csl.sri.com~
                          ~% 2. add yices to your path and restart PVS.
                          ~%The error message is:~% ~a"
		       (file-contents tmp-file))
		     (values 'X nil))))))))

	
(addrule 'yices2 () ((fnums *) nonlinear?)
  (yices2 fnums nonlinear?)
  "Invokes Yices as an endgame SMT solver to prove that the conjunction
of the negations of the selected formulas is unsatisfiable. "
  "~%Simplifying with Yices,")
  
  
(defstep yices2-with-rewrites
  (&optional (fnums *) defs theories rewrites exclude-theories exclude)
  (then (simplify-with-rewrites fnums defs theories rewrites exclude-theories exclude)
	(yices2 fnums))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums) followed
by (yices fnums), then turns off all the installed rewrites.  Examples:
 (yices-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (yices-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, applying Yices, and disabling installed rewrites")

(defstep y2simp (&optional (fnums *) nonlinear?)
  (then (skosimp*)(yices2 :fnums fnums :nonlinear? nonlinear?))
  "Repeatedly skolemizes and flattens, and then applies the Yices2 SMT solver"
  "Repeatedly skolemizing and flattening, and then invoking Yices2")
  

(defstep y2grind (&optional (defs !); nil, t, !, explicit, or explicit!
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
	(yices2 :nonlinear? nonlinear?))
  "Core of GRIND: Installs rewrites, repeatedly applies BASH, and then
   invokes YICES.  See BASH for more explanation."
"Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization, Yices")

