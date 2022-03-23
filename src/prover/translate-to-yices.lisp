;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-yices.lisp -- 
;; Author          : N. Shankar
;; Created On      : 
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

;;NSH(3/17/06): Adapting translate-to-prove to yices output. 

(defvar *ybindings* nil)
(defvar *yembeddedf* nil)
(defvar *ydefns* nil)
(defvar *ydatatype-warning* nil)
(defvar *yname-hash* (make-pvs-hash-table))
(defvar *translate-to-yices-hash* (make-pvs-hash-table))
(defvar *yices-executable* nil)
(defvar *yices-flags* "-e -st -mi 2000")
(defvar *yices-id-counter*)  ;;needs to be initialized in eproofcheck
(newcounter *yices-id-counter*)

(defparameter *yices-interpreted-names*
  '((=  (|equalities| . =))
    (/=  (|notequal| . /=))
    (TRUE (|booleans| . true))
    (FALSE (|booleans| . false))
    (IMPLIES  (|booleans| . =>))
    (=>  (|booleans| . =>))
    (⇒ (|booleans| . =>))
    (IFF (|booleans| . =))
    (⇔ (|booleans| . =))
    (AND (|booleans| . and) (|bv_bitwise| . bv-and))
    (∧ (|booleans| . and))
    (& (|booleans| . and))
    (OR  (|booleans| . or) (|bv_bitwise| . bv-or))
    (∨  (|booleans| . or))
    (NOT  (|booleans| . not)(|bv_bitwise| . bv-not))
    (¬ (|booleans| . not))
    (+  (|number_fields| . +)(|bv_arith_nat| . bv-add))
    (- (|number_fields| . -)(|bv_arithmetic_defs| . bv-sub))
    (*   (|number_fields| . *))
    (/  (|number_fields| . /))
    (rem (|modulo_arithmetic| . mod))
    (ndiv (|modulo_arithmetic| . div))
    (< (|reals| . <)(|bv_arith_nat| . bv-lt))
    (<=  (|reals| . <=)(|bv_arith_nat| . bv-le))
    (> (|reals| . >)(|bv_arith_nat| . bv-gt))
    (>=  (|reals| . >=)(|bv_arith_nat| . bv-ge))
    (O (|bv_concat_def| . bv-concat))
    (& (|booleans| . and)(|bv_bitwise| . bv-and))
    (XOR  (|bv_bitwise| . bv-xor))
;    (^ (|bv_caret| .  bv-extract))
    (sign_extend   (|bv_extend| . bv-sign-extend))
    (|bv_slt| (|bv_arithmetic_defs| . bv-slt))
    (|bv_sle| (|bv_arithmetic_defs| . bv-sle))
    (|bv_sgt| (|bv_arithmetic_defs| . bv-sgt))
    (|bv_sge| (|bv_arithmetic_defs| . bv-sge))
    (|bv_splus| (|bv_arithmetic_defs| . bv-add))
    (|bv_stimes| (|bv_arithmetic_defs| . bv-mul))
    ))

(defun clear-yices ()
  (setq *ydefns* nil)
  (clrhash *yname-hash*)
  (clrhash *translate-to-yices-hash*)
  (newcounter *yices-id-counter*))


;;We need to add constant declarations and type declarations.
;;Type definitions will be needed for scalars and datatypes.

;; (defstruct ytype (name def))
;; (defstruct yconst (name type))

(defun yices-name (expr &optional id) ;;expr must have id field,
                                      ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *yname-hash*)))
	(or entry
	    (let ((name (yices-id-name (or id (id expr)))))
	      (setf (gethash expr *yname-hash*) name)
	      name)))))

(defun yices-id-name (id)
  (intern
   (concatenate 'string
     (string (if (integerp id)
		 (format nil "~r"
		   id)
		 id))
     "_"
     (princ-to-string
      (funcall
       *yices-id-counter*)))
   :pvs))

;;Next, we translate PVS types to Yices types so that any enumerated
;;types or datatypes introduce names.

(defun yices-type-name (expr)
  (yices-name expr))

(defun translate-to-yices-scalar-constructors (constructors)
  (loop for constr in constructors collect (yices-name constr)))

(defun translate-to-yices-scalar (yices-type-name constructors)
  (let ((yices-constructors
	 (translate-to-yices-scalar-constructors constructors)))
    (push (format nil "(define-type ~a (scalar ~{~a ~}))"
	    yices-type-name yices-constructors)
	  *ydefns*)
    (loop for cnstr in constructors
	  do (let* ((recognizer (recognizer cnstr))
		    (yices-recognizer (yices-name recognizer)))
	       (push (format nil "(define ~a::(-> x::~a bool) (lambda (x::~a) (= x ~a)))" 
		       yices-recognizer
		       yices-type-name
		       yices-type-name
		       (yices-name cnstr))
		     *ydefns*)))
    yices-type-name))


(defun translate-to-yices-accessors (accessors bindings)
  (cond ((consp accessors)
	 (format nil "~a::~a ~a"
	   (yices-name (car accessors))
	   (translate-to-yices* (range (type (car accessors))) bindings)
	   (translate-to-yices-accessors (cdr accessors) bindings)))
	(t (format nil ""))))

(defun translate-to-yices-constructor (constructor bindings)
  (let ((accessors (accessors constructor)))
    (if accessors 
	(format nil "(~a ~a)"
	  (yices-name constructor)
	  (translate-to-yices-accessors (accessors constructor) bindings))
	(format nil "~a" (yices-name constructor)))))

(defun translate-to-yices-constructors (constructors bindings)
  (cond ((consp constructors)
	 (cons (translate-to-yices-constructor (car constructors) bindings)
	       (translate-to-yices-constructors (cdr constructors) bindings)))
	(t nil)))

(defun translate-to-yices-subdatatype-constructors
  (constructors superconstructors bindings)
  (cond ((consp constructors)
	 (cons (translate-to-yices-subdatatype-constructor (car constructors)
							   (car superconstructors)
							   bindings)
		 (translate-to-yices-subdatatype-constructors (cdr constructors)
							      (cdr superconstructors)
							      bindings)))
	(t nil)))

(defun translate-to-yices-subdatatype-constructor
    (constructor superconstructor bindings)
  (let ((cname (yices-name constructor))
	(accessors (accessors constructor)))
    (if accessors;null constructors are unchanged from superdatatype
	(let* ((ctype (if (> (length accessors) 1)
			  (format nil "(-> ~{~a ~}~a)"
				  (translate-to-yices* (types (domain (type constructor)))
						       bindings)
				  (translate-to-yices* (range (type constructor))
						       bindings))
			(translate-to-yices* (type constructor) bindings)))
	       (acc-ids (loop for acc in accessors
			      as i from 1
			      collect (format nil "x~a" i)))
	       (cdef-arglist (if (> (length accessors) 1)
				 (loop for acc-id in acc-ids
				       as ty in (types (domain (type constructor)))
				       collect (format nil "~a::~a"
						       acc-id
						       (translate-to-yices* ty
									    bindings)))
			       (if (eql (length accessors) 1)
				   (format nil "x::~a" (translate-to-yices* (domain (type constructor)) bindings))
				 nil)))
	       (cdef (format nil "(lambda (~{~a ~}) (~a~{ ~a~}))" cdef-arglist
			     (yices-name superconstructor)
			     acc-ids))
	       (defn-string (format nil "(define ~a::~a ~a)"
				    cname
				    ctype
				    (if accessors cdef (yices-name superconstructor))))
	       (recognizer-defn-string (format nil "(define ~a::~a ~a)"
					       (format nil "~a?" cname)
					       (translate-to-yices* (type (recognizer constructor)) bindings)
					       (format nil "~a?" (yices-name superconstructor)))))
	  (push recognizer-defn-string *ydefns*)
	  (push defn-string *ydefns*)
	  (translate-to-yices-subdatatype-accessors accessors
						    (accessors superconstructor)
						    bindings)
	  cname)
      cname)))

(defun translate-to-yices-subdatatype-accessors (accessors superaccessors bindings)
  (cond ((consp accessors)
	 (push (format nil "(define ~a::~a ~a)"
		 (yices-name (car accessors))
		 (translate-to-yices* (type (car accessors)) bindings)
		 (yices-name (car superaccessors)))
	       *ydefns*)
	 (translate-to-yices-subdatatype-accessors (cdr accessors)
						   (cdr superaccessors)
						   bindings))
	(t nil)))

(defmethod translate-to-yices* ((ty type-name) bindings)
  (let ((yname-hash (gethash ty *yname-hash*)))
    (or yname-hash 
	(cond ((enum-adt? ty)
	       (let ((constructors (constructors ty))
		     (yices-name (yices-type-name ty)))	;;bindings can be ignored
		 (translate-to-yices-scalar yices-name constructors) ))
	      ((adt? ty)
	       (setq *ydatatype-warning* t)
	       (let ((constructors (constructors ty))
		      (yices-name (yices-type-name ty)))
		  (let ((defn-string
			  (format nil "(define-type ~a (datatype ~{~a ~}))"
			    yices-name
			    (translate-to-yices-constructors constructors bindings))))
		    (push defn-string *ydefns*)
		    yices-name)))
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "bool"))
	      ((tc-eq ty *number*) "real")
	      (
		;;else uninterpreted type
		(let ((yices-name (yices-type-name ty)))
		  (push (format nil "(define-type ~a)" yices-name) *ydefns*)
		  yices-name))))))

(defmethod translate-to-yices* ((ty datatype-subtype) bindings)
  (let ((yname-hash (gethash ty *yname-hash*)))
    (or yname-hash 
	(let* ((supertype (supertype ty))
	       (ysupertype (translate-to-yices* supertype bindings))
	       (ytypename (yices-type-name ty))
	       (constructors (constructors ty))
	       (superconstructors (constructors supertype))
	       (defn-string
		 (format nil "(define-type ~a ~a)"
		   ytypename ysupertype)))
	  (push defn-string *ydefns*)
	  (translate-to-yices-subdatatype-constructors
	   constructors superconstructors bindings)
	  ytypename))))
    


    ;;Not used
(defun subrange-range (type)
  (let ((below (simple-below? type)))
    (or (and below
	     (cons 0 below))
	(let ((upto (simple-upto? type)))
	  (or (and upto (cons 0 upto))
	      (simple-subrange? type))))))

(defun yices-format-tuple-list (id len)
  (loop for i  from 0 to (1- len)
	collect (format nil "(select ~a ~a)" id i)))

(defmethod translate-to-yices* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ((tc-eq ty *naturalnumber*) 'nat)
	  ((tc-eq ty *integer*) 'int)
	  ((tc-eq ty *real*) 'real)
	  (t (let* ((ypred (translate-to-yices* predicate bindings))
		    (ysupertype (translate-to-yices* supertype
						     bindings))
		    (maxtype (find-supertype supertype))
		    (subvar (yices-id-name 'subvar)))
	       (if (tupletype? maxtype)
		   (let* ((ldom (length (types maxtype)))
			  (subvarlist (yices-format-tuple-list subvar ldom)))
		     (format nil "(subtype (~a::~a) (~a ~{~a~}))"
		       subvar ysupertype ypred subvarlist))
		   (format nil "(subtype (~a::~a) (~a ~a))"
		     subvar ysupertype ypred subvar)))))))

(defmethod translate-to-yices* ((ty tupletype) bindings)
  (format nil "(tuple ~{~a ~})" (translate-to-yices-list (types ty) nil nil bindings)))

;; (defmethod translate-to-yices* ((ty list) bindings)
;;   (translate-to-yices-list ty nil nil bindings))


(defun translate-to-yices-funlist (list lastelem 
					domain  bindings)
  (translate-to-yices-funlist* list nil 1 lastelem domain nil bindings))
  

(defun translate-to-yices-funlist* (list accum num lastelem 
					domain dbindings bindings)	 
  (if (consp list)
      (let* ((ycar (translate-to-yices* (car list)
				       bindings))
	     (dbindflag (and (dep-binding? domain)
			     (not (dep-binding? (car list)))))
	     (ycar_id (if dbindflag
			  (yices-id-name
			   (format nil "~a_~a"
			     (id domain)
			     num))
			  (if (dep-binding? (car list))
			      (yices-name (car list))
			      nil)))
	     (ycarbnd (if dbindflag
			  (format nil "~a::~a" ycar_id ycar)
			  ycar)))
	(translate-to-yices-funlist* (cdr list)
				     (cons ycarbnd
					   accum)
				     (1+ num)
				     lastelem
				     domain
				     (if (dep-binding? domain)
					 (cons ycar_id dbindings)
					 dbindings)
				     (if (dep-binding? (car list))
					 (cons (cons (car list)
						     (yices-name (car list)))
					       bindings)
					 bindings)
				     ))
      (let ((accum (nreverse accum)))
      (format nil "(-> ~{ ~a~} ~a)"
	accum
	(translate-to-yices* lastelem
			     (if (dep-binding? domain)
				 (cons (cons domain
					     (nreverse dbindings))
				       bindings)
				 bindings))))))


(defun translate-to-yices-list (list accum lastelem bindings)
  (if (consp list)
      (translate-to-yices-list (cdr list)
			       (cons (translate-to-yices* (car list)
							  bindings)
				     accum)
			       lastelem
			       (if (binding? (car list))
				   (cons (cons (car list)
					       (yices-name (car list)))
					 bindings)
				   bindings))
      (if lastelem
	  (nreverse (cons (translate-to-yices* lastelem bindings)
			  accum))
	  (nreverse accum))))

(defmethod translate-to-yices* ((ty recordtype) bindings)
  (format nil "(record ~{~a ~})"
    (translate-to-yices-list (fields ty) nil nil bindings)))

(defmethod translate-to-yices* ((ty field-decl) bindings)
  (format nil "~a::~a" (id ty)
	  (translate-to-yices* (type ty) bindings)))

(defmethod translate-to-yices* ((ty dep-binding) bindings)
  (let ((yices-id (yices-name ty))
	(yices-type (translate-to-yices* (type ty) bindings)))
    (format nil "~a::~a" yices-id yices-type)))

(defmethod translate-to-yices* ((ty funtype) bindings)
  (with-slots (domain range) ty
    (let ((bv-size (simple-below? domain))
	  )
     (if (and (number-expr? bv-size)
	      (tc-eq (find-supertype range) *boolean*))
	 (format nil "(bitvector ~a)" (number bv-size))
	 (let ((sdom (find-supertype domain))
	       )
	   (if (tupletype? sdom)
	       (translate-to-yices-funlist (types sdom) range 
					   domain bindings)
	       (format nil "(-> ~a ~a)" 
		 (translate-to-yices* domain bindings)
		 (let ((newbindings (if (dep-binding? domain)
					(cons (cons domain
						    (yices-name domain))
					      bindings)
					bindings)))
		   (translate-to-yices* range
					newbindings)))))))))

;;Not used anywhere.
;; (defun bv-funtype? (x)
;;    (let ((y (simple-below? (domain x))))
;; 	 (and (number-expr? y)
;; 	      (number y)))))
		     

;;name-exprs and binding-exprs are not hashed in binding contexts.
;;*bound-variables* are the bound variables in the calling context, and
;;*bindings* are the ones built up locally.
(defmethod translate-to-yices* :around ((obj type-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-yices-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-yices-hash*)
		    result)
	      result)))))
    
(defmethod translate-to-yices* :around ((obj name-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-yices-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-yices-hash*)
		    result)
	      result)))))

(defmethod translate-to-yices* :around ((obj binding-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value
	     (gethash obj *translate-to-yices-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-yices-hash*)
		    result)
	      result)))))

(defmethod translate-to-yices* ((list list) bindings)
  (cond ((consp list)
	 (cons (translate-to-yices* (car list) bindings)
	       (translate-to-yices* (cdr list) bindings)))
	(t nil)))

(defun yices-recognizer (name bindings)
  (when (recognizer? name)
    (translate-to-yices* (type name) bindings)
    (format nil "~a?" (translate-to-yices* (constructor name) bindings))))

(defmethod translate-to-yices* ((expr name-expr) bindings)
  (let ((bpos (assoc expr bindings
		     :test #'same-declaration)))
    (if bpos (if (consp (cdr bpos))
		 (format nil "(mk-tuple ~{ ~a~})" (cdr bpos))
		 (cdr bpos))
	(let* ((yname-hashentry (gethash expr *yname-hash*)))
	  (or yname-hashentry
	      (eta-expanded-yices-interpretation expr)
	      (yices-recognizer expr bindings)
	      (let* ((ytype (translate-to-yices* (type expr)
						bindings))
		     (yname-hashentry (gethash expr *yname-hash*)))
		(or yname-hashentry
		    (let* ((yname (yices-name expr))
			  (defn (format nil "(define ~a::~a)"
				  yname
				  ytype)))
		      (push defn
			    *ydefns*)
		      (format-if "~%Adding definition: ~a" defn)
		      yname))))))))

;; (defun eta-expanded-yices-interpretation (name-expr)
;;   (let ((yy (yices-interpretation name-expr)))
;;     (when yy
;;       (if (funtype? (type name-expr))
;; 	  (let* ((ftype (type name-expr))
;; 		 (dtypes (types (domain ftype)))
;; 		 (etavars (loop for dt in dtypes
;; 			       as i from 1
;; 			       collect
;; 			       (format nil "etavar_~a" i)))
;; 		 (etabindings (loop for dt in dtypes
;; 			       as ev in etavars
;; 			       collect (format nil "~a::~a"
;; 					       ev (translate-to-yices* dt nil)))))
;; 	    (format nil "(lambda (~{ ~a~})(~a ~{ ~a~}))"
;; 		    etabindings yy etavars))
;; 	yy))))

(defmethod translate-to-yices* ((expr constructor-name-expr) bindings)
  (call-next-method (lift-adt expr) bindings))


(defmethod translate-to-yices* ((expr number-expr) bindings)
  (declare (ignore bindings))
  (number expr))

(defmethod translate-to-yices* ((ex string-expr) bindings)
  (declare (ignore bindings))
  (string->int (string-value ex)))

(defmethod translate-to-yices* ((expr record-expr) bindings)
  (format nil "(mk-record~{ ~a~})"
    (translate-to-yices* (assignments expr) bindings)))

(defmethod translate-to-yices* ((expr uni-assignment) bindings)
  (format nil "~a::~a"
    (id (caar (arguments expr)))
    (translate-to-yices* (expression expr) bindings)))

(defmethod translate-to-yices* ((expr tuple-expr) bindings)
  (format nil "(mk-tuple ~{ ~a~})"
    (translate-to-yices* (exprs expr) bindings)))
	
(defmethod translate-to-yices* ((expr branch) bindings)
  (format nil "(if ~a ~a ~a)"
    (translate-to-yices* (condition expr) bindings)
    (translate-to-yices* (then-part expr)bindings)
    (translate-to-yices* (else-part expr)bindings)))

(defmethod translate-to-yices* ((expr cases-expr) bindings)
  (translate-to-yices* (translate-cases-to-if expr) bindings))

(defmethod translate-to-yices* ((expr projection-expr) bindings)
  (let* ((id (make-new-variable '|x| expr))
	 (yid (yices-id-name id))
	 (ytype (translate-to-yices*
		(domain (find-supertype (type expr)))
		bindings)))
  (format nil "(lambda (~a::~a) (select ~a ~a))"
    yid ytype yid (index expr))))


(defmethod translate-to-yices* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (if (variable? argument)
	(let ((bnd (assoc argument bindings
			  :test #'same-declaration)))
	  (if (and bnd (consp (cdr bnd)))
	      (nth (1- index) (cdr bnd))
	      (format nil "(select ~a ~a)"
		(translate-to-yices* argument bindings)
		(index expr))))
	(format nil "(select ~a ~a)"
		(translate-to-yices* argument bindings)
		(index expr)))))

    

(defmethod translate-to-yices* ((expr field-application) bindings)
  (with-slots (id argument type) expr
    (format nil "(select ~a ~a)"
      (translate-to-yices* argument bindings)
      id)))

    ;;NSH(5.17.94): Complicated code to deal with tuple mismatch
    ;;between domain of operator and arguments.
    ;; op(a1,..., an) if dom(type(op)) = [t1,...,tn] ==>
    ;;                      (op' (tupcons a1' .. an'))
    ;; op(a1), if dom(type(op)) = t1,...,tn ==> (op (tupsel 0 a1)...)

(defmethod translate-to-yices* ((expr application) bindings)
  (with-slots (operator argument) expr
    (let* ((op* (operator* expr))
	   (op-id (when (name-expr? op*) (id op*))))
      (cond ((and (eq op-id 'rem)
		  (eq (id (module-instance (resolution op*)))
		      'modulo_arithmetic))
	     (let ((denom (translate-to-yices* (args1 (operator expr))
					       bindings))
		   (numer (translate-to-yices* (args1 expr)
					       bindings)))
	       `(mod ,numer ,denom)))
	    ((and (eq op-id '-)  ;;NSH(4-19-10)
		  (unary-application? expr)
		  (eq (id (module-instance (resolution op*)))
		      '|number_fields|))
	     (format nil "(- 0 ~a)" (translate-to-yices* (argument expr) bindings)))
	    ((and (eq op-id 'nat2bv)
		  (number-expr? (expr (car (actuals (module-instance op*))))))
	     (let ((size (translate-to-yices*
			  (expr (car (actuals (module-instance op*))))
			  bindings))
		   (num (translate-to-yices*
			 (args1 expr) bindings)))
	       `(mk-bv ,size ,num)))
	    ((and (eq op-id '-)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_arithmetic_defs|)
		  (not (tupletype? (domain (type op*)))))
	     (format nil "(bv-neg ~a)"
	       (translate-to-yices* (argument expr) bindings)))
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
	       (translate-to-yices* (car (exprs argument)) bindings)))
	    ((and (enum-adt? (find-supertype (type argument)))
		  (recognizer? operator))
	     (format nil "(= ~a ~a)"
	       (translate-to-yices* argument bindings)
	       (translate-to-yices* (constructor operator) bindings)))
	    ((constructor? operator)
	     (format nil "(~a ~{ ~a~})"
		     (translate-to-yices* operator bindings)
		     (translate-to-yices* (arguments expr) bindings)))
	    (t
	     (let ((yices-interpretation
		    (yices-interpretation operator)))
	       (if yices-interpretation
		   (format nil "(~a ~{ ~a~})"
		     yices-interpretation
		     (translate-to-yices* (arguments expr) bindings))
		   (let* ((yices-op (translate-to-yices* operator bindings))
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
				     (translate-to-yices* args
							  bindings))))
		     (format nil "(~a ~{ ~a~})" yices-op yargs)))))))))


(defun translate-yices-bindings (bind-decls bindings prefix-string)
      (cond ((consp bind-decls)
	     (let ((yname (yices-name (car bind-decls)))
		   (ytype (translate-to-yices* (type (car bind-decls)) bindings)))
	       (translate-yices-bindings (cdr bind-decls)
					 (cons (cons (car bind-decls)
						     yname)
					       bindings)
					 (format nil "~a ~a::~a"
					   prefix-string yname ytype))))
	    (t (values bindings prefix-string))))


(defmethod translate-to-yices* ((expr binding-expr) bindings)
  (with-slots ((expr-bindings bindings) expression) expr
    (let ((stype (find-supertype (type (car expr-bindings)))))
    (cond ((and (lambda-expr? expr)
		(singleton? expr-bindings)
		(tupletype? stype))
	   (let* ((ytypes (translate-to-yices-list
			   (types stype) nil nil bindings))
		  (lamvar (yices-id-name "lamvar"))
		  (lamlist (loop for i from 1 collect
				 (format nil "~a_~a" lamvar i)))
		  (yparams (loop for var in lamlist 
				 as ty in ytypes
				 collect (format nil "~a::~a" var  ty))))
	     (format nil "(lambda (~{ ~a~}) (let ((~a (mk-tuple ~{ ~a~}))) ~a))"
	       yparams lamvar
	       lamlist
	       (translate-to-yices* expression
				    (cons (cons (car expr-bindings) lamvar)
					  bindings)))))
	  (t (multiple-value-bind (newbindings bindstring)
		 (translate-yices-bindings  expr-bindings bindings "")
	       (let ((yexpression (translate-to-yices* expression newbindings)))
		 (cond ((lambda-expr? expr)
			(format nil "(lambda (~a) ~a)" bindstring yexpression))
		       ((forall-expr? expr)
			(format nil "(forall (~a) ~a)"
			  bindstring yexpression))
		       ((exists-expr? expr)
			(format nil "(exists (~a) ~a)"
			  bindstring yexpression)))))))))) ;;no else case

;; (defmethod translate-to-yices* ((expr forall-expr) bindings)
;;   (call-next-method))

;; (if *yqexpand*
;;       (with-slots ((expr-bindings bindings) expression) expr
;; 	(translate-forall-to-yices expr-bindings expression bindings))
;;       (call-next-method) ; should call binding-expr
;;       ))



(defun translate-forall-to-yices (expr-bindings expression bindings)
  (cond ((consp expr-bindings)
	 (let* ((bind1 (car expr-bindings))
		(typ1 (type bind1))
		;;(styp1 (find-supertype typ1))
		)
	   (cond ((enum-adt? (find-supertype typ1))
		  (format nil "(and ~{ ~a~})"
		    (multiple-value-bind (nbindings npreds)
			(collect-bindings-predicates bind1 nil)
		      (let ((ypreds
			     (translate-typepreds-to-yices nbindings bindings npreds)))
			(loop for const in (constructors typ1) collect
			     (if ypreds
				 (format nil "(implies ~a ~a)"
				   (format nil "(and ~{ ~a~})"
				     (loop for ypred in ypreds
					collect (format nil "~a(~a)" ypred
							const)))
				   (translate-forall-to-yices (cdr expr-bindings)
							      expression
							      (cons (cons bind1 const)
								    bindings)))
				 (translate-forall-to-yices (cdr expr-bindings)
							    expression
							    (cons (cons bind1 const)
								  bindings))))))))
		 )))))
				    

;;used to generate subtype predicate applications to expr
(defun translate-typepreds-to-yices (nbinding bindings npreds)
  (loop for pred in npreds collect
       (multiple-value-bind (newbindings bindstring)
	   (translate-yices-bindings (list nbinding) bindings "")
	 (format nil "(lambda (~a) ~a)"
	   bindstring
	   (translate-to-yices* pred
				(cons newbindings bindings))))))
		      


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

(defmethod translate-to-yices* ((expr update-expr) bindings)
  (translate-yices-assignments (assignments expr)
			       (translate-to-yices* (expression expr) bindings)
			       (type expr)
			       bindings))

(defun translate-yices-assignments (assigns 
				    trbasis type bindings)
  (if assigns
      (translate-yices-assignments
       (cdr assigns)
					;basis
       (translate-yices-assignment (car assigns) ;basis
				   trbasis type bindings)
       type
       bindings)
      trbasis))

(defun translate-yices-assignment (assign trbasis type bindings)
  (translate-yices-assign-args (arguments assign)
			       (expression assign)
			       trbasis
			       (find-supertype type)
			       bindings))

(defmethod translate-yices-assign-args (args value trbasis (type recordtype)
					     bindings)
  (if args
      (let* ((label (id (caar args)))
	     (newtrbasis (format nil "(select ~a ~a)" trbasis label)))
	(format nil "(update ~a ~a ~a)" trbasis label
		(translate-yices-assign-args
		 (cdr args) value newtrbasis
		 (type (find (id (caar args)) (fields type)
			     :test #'eq :key #'id))
		 bindings)))
      (translate-to-yices* value bindings)))

(defmethod translate-yices-assign-args (args value trbasis (type tupletype)
					     bindings)
  (if args
      (let* ((index (number (caar args)))
	     (newtrbasis (format nil "(~a ~{~a~})" trbasis index)) )
	(format nil "(update ~a (~a) ~a)" trbasis index
		(translate-yices-assign-args
		 (cdr args) value newtrbasis
		 (type (nth (1- index) (types type))) bindings)))
      (translate-to-yices* value bindings)))

(defmethod translate-yices-assign-args (args value trbasis (type funtype)
					     bindings)
  (if args
      (let* ((yargs1 (translate-to-yices* (car args) bindings))
	     (newtrbasis (format nil "(~a ~a)" trbasis yargs1)) )
	(format nil "(update ~a ~a ~a)" trbasis yargs1
		(translate-yices-assign-args
		 (cdr args) value newtrbasis
		 (range type) bindings)))
      (translate-to-yices* value bindings)))

(defmethod translate-yices-assign-args (args value trbasis (type t) bindings)
  (declare (ignore args trbasis))
  (translate-to-yices* value bindings))

(defun eta-expanded-yices-interpretation (name-expr)
  (let ((yy (yices-interpretation name-expr)))
    (when yy
      (if (funtype? (type name-expr))
	  (let* ((ftype (type name-expr))
		 (dtypes (types (domain ftype)))
		 (etavars (loop for dt in dtypes
			       as i from 1
			       collect
			       (format nil "etavar_~a" i)))
		 (etabindings (loop for dt in dtypes
			       as ev in etavars
			       collect (format nil "~a::~a"
					       ev (translate-to-yices* dt nil)))))
	    (format nil "(lambda (~{ ~a~})(~a ~{ ~a~}))"
		    etabindings yy etavars))
	yy))))


(defun yices-interpretation (name-expr)
  (when (name-expr? name-expr)
    (let* ((id-assoc (cdr (assoc (id name-expr) *yices-interpreted-names*)))
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

(defun find-yices-executable ()
  (or *yices-executable*
      (cond ((and (pvs-context-yices-executable)
		  (program-version (pvs-context-yices-executable) "1"))
	     (setq *yices-executable* "(pvs-context-yices-executable)"))
	    ((program-version "yices1 --version" "1")
	     (setq *yices-executable* "yices1"))
	    ((program-version "yices --version" "1")
	     (setq *yices-executable* "yices"))
	    (t (format t "~%Yices 1 cannot be found in your path")
	       (when (program-version "yices --version" "Yices 2")
		 (format t "~%\"yices\" is in your path, but it is version 2"))
	       (unless (pvs-yes-or-no-p
			"~%Use yices2 (i.e., \"yices2\" or \"yices2-with-rewrites\") instead? ")
		 (format t "~%If necessary, download and install Yices 1 from http://yices.csl.sri.com")
		 (let ((path (pvs-dialog "~%Please enter the path to Yices 1: ")))
		   (get-yices-executable-path path)))))))

(defun get-yices-executable-path (path)
  (cond ((program-version (concatenate 'string path " --version") "1")
	 (setq *yices-executable* path))
	(t (format t "~%Invalid path to Yices 1 executable")
	   (let ((npath (pvs-dialog "~%Please enter the path to Yices 1: ")))
	     (get-yices-executable-path npath)))))
	    

(defun yices (sformnums)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (s-forms (select-seq (s-forms goalsequent) sformnums))
	     (*ydefns* nil)
	     (*ydatatype-warning* nil))
	(find-yices-executable)
	(assert *yices-executable*)
	(clear-yices)
	(let ((yices-forms
	       (loop for sf in s-forms
		     collect
		     (let ((fmla (formula sf)))
		       (if (negation? fmla)
			   (format nil "(assert ~a)"
			     (translate-to-yices* (args1 fmla) nil))
			   (format nil "(assert (not ~a))"
			     (translate-to-yices* fmla  nil))))))
	      (revdefns (nreverse *ydefns*))
	      (file (make-pathname :defaults (working-directory)
				   :name (label ps) :type "yices")))
	  (format-if "~%ydefns = ~% ~{~a~%~}" revdefns)
	  (format-if "~%yforms = ~% ~{~a~%~}" yices-forms)
	  (with-open-file (stream  file :direction :output
				   :if-exists :supersede)
	    (format stream "~{~a ~%~}" revdefns)
	    (format stream "~{~a ~%~}" yices-forms)
	    (format stream "(check)~%"))
	  (let ((status nil)
		(tmp-file (pvs-tmp-file)))
	    (with-open-file (out tmp-file
				 :direction :output :if-exists :supersede)
	      (setq status
		    #+allegro
		    (excl:run-shell-command
		     (format nil "~a ~a ~a" *yices-executable* *yices-flags* (namestring file))
		     :input "//dev//null"
		     :output out
		     :error-output :output)
		    #+sbcl
		    (sb-ext:run-program
		     (format nil "~a ~a ~a" *yices-executable* *yices-flags* (namestring file))
		     nil
		     :input "//dev//null"
		     :output out
		     :error out)
		    #+cmu
		    (extensions:run-program
		     (format nil "~a ~a ~a" *yices-executable* *yices-flags* (namestring file))
		     nil
		     :input "//dev//null"
		     :output out
		     :error out)))
	    (when *ydatatype-warning*
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

	
(addrule 'yices () ((fnums *))
  (yices fnums)
  "Invokes Yices as an endgame SMT solver to prove that the conjunction
of the negations of the selected formulas is unsatisfiable. "
  "~%Simplifying with Yices,")
  
  
(defstep yices-with-rewrites
  (&optional (fnums *) defs theories rewrites exclude-theories exclude)
  (then (simplify-with-rewrites fnums defs theories
				:rewrites rewrites
				:exclude-theories exclude-theories
				:exclude exclude)
	(yices fnums))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums) followed
by (yices fnums), then turns off all the installed rewrites.  Examples:
 (yices-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (yices-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, applying Yices, and disabling installed rewrites")

(defstep ygrind (&optional (defs !); nil, t, !, explicit, or explicit!
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
			  implicit-typepreds?)
  (then (install-rewrites$ :defs defs :theories theories
		      :rewrites rewrites :exclude exclude)
	(repeat* (bash$ :if-match if-match :updates? updates?
			:polarity? polarity? :instantiator instantiator
			:let-reduce? let-reduce?
			:quant-simp? quant-simp?
			:implicit-typepreds? implicit-typepreds?
			:cases-rewrite? cases-rewrite?))
	(yices))
  "Core of GRIND: Installs rewrites, repeatedly applies BASH, and then
   invokes YICES.  See BASH for more explanation."
"Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization, Yices")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun translate-to-yices-deftype (type bindings)
;;   (let ((stype (find-supertype type)))
;;     (cond ((funtype? stype)
;; 	   (if (tupletype? (find-supertype (domain stype)))
;; 	       (


;; (defmethod translate-to-yices* ((expr name-expr) bindings)
;;   (let ((bpos (assoc expr bindings
;; 		     :test #'same-declaration)))
;;     (if bpos (cdr bpos)
;; 	(let* ((yname-hashentry (gethash expr *yname-hash*)))
;; 	  (or yname-hashentry
;; 	      (yices-interpretation expr)
;; 	      (yices-recognizer expr bindings)
;; 	      (let* ((ytype (translate-to-yices-deftype (type expr)
;; 						bindings))
;; 		     (yname-hashentry (gethash expr *yname-hash*)))
;; 		(or yname-hashentry
;; 		    (let* ((yname (yices-name expr))
;; 			  (defn (format nil "(define ~a::~a)"
;; 				  yname
;; 				  ytype)))
;; 		      (push defn
;; 			    *ydefns*)
;; 		      (format-if "~%Adding definition: ~a" defn)
;; 		      yname))))))))
	 
;; (defmethod translate-to-yices* ((expr application) bindings)
;;   (with-slots (operator argument) expr
;;     (let* ((op* (operator* expr))
;; 	   (op-id (when (name-expr? op*) (id op*))))
;;       (cond ((and (eq op-id 'rem)
;; 		  (eq (id (module-instance (resolution op*)))
;; 		      'modulo_arithmetic))
;; 	     (let ((denom (translate-to-yices* (args1 (operator expr))
;; 					       bindings))
;; 		   (numer (translate-to-yices* (args1 expr)
;; 					       bindings)))
;; 	       `(mod ,numer ,denom)))
;; 	    ((and (eq op-id 'nat2bv)
;; 		  (number-expr? (expr (car (actuals (module-instance op*))))))
;; 	     (let ((size (translate-to-yices*
;; 			  (expr (car (actuals (module-instance op*))))
;; 			  bindings))
;; 		   (num (translate-to-yices*
;; 			 (args1 expr) bindings)))
;; 	       `(mk-bv ,size ,num)))
;; 	    ((and (eq op-id '-)
;; 		  (eq (id (module-instance (resolution op*)))
;; 		      '|bv_arithmetic_defs|)
;; 		  (not (tupletype? (domain (type op*)))))
;; 	     (format nil "(bv-neg ~a)"
;; 	       (translate-to-yices* (argument expr) bindings)))
;; 	    ((and (eq op-id '^)
;; 		  (eq (id (module-instance (resolution op*)))
;; 		      '|bv_caret|)
;; 		  (tuple-expr? argument)
;; 		  (tuple-expr? (cadr (exprs argument)))
;; 		  (number-expr? (car (exprs (cadr (exprs argument)))))
;; 		  (number-expr? (cadr (exprs (cadr (exprs argument))))))
;; 	     (format nil "(bv-extract ~a ~a ~a)"
;; 	       (number (car (exprs (cadr (exprs argument)))))
;; 	       (number (cadr (exprs (cadr (exprs argument)))))
;; 	       (translate-to-yices* (car (exprs argument)) bindings)))
;; 	    ((and (enum-adt? (find-supertype (type argument)))
;; 		  (recognizer? operator))
;; 	     (format nil "(= ~a ~a)"
;; 	       (translate-to-yices* argument bindings)
;; 	       (translate-to-yices* (constructor operator) bindings)))
;; 	    ((constructor? operator)
;; 	     (format nil "(~a ~{ ~a~})"
;; 		     (translate-to-yices* operator bindings)
;; 		     (translate-to-yices* (arguments expr) bindings)))
;; 	    (t
;; 	     (let ((yices-interpretation
;; 		    (yices-interpretation operator)))
;; 	       (if yices-interpretation
;; 		   (format nil "(~a ~{ ~a~})"
;; 		     yices-interpretation
;; 		     (translate-to-yices* (arguments expr) bindings))
;; 		   (let* ((yices-op (translate-to-yices* operator bindings))
;; 			  (arg (argument expr))
;; 			  (args (if (tuple-expr? arg)
;; 				    (arguments expr)
;; 				    (let ((stype (find-supertype (type arg))))
;; 				      (if (tupletype? stype)
;; 					  (loop for index from 1 to (length (types stype))
;; 						collect (make-projection-application index arg))
;; 					  (list arg)))))
;; 			  (yargs (translate-to-yices* args
;; 						      bindings)))
;;		     (format nil "(~a ~{ ~a~})" yices-op yargs)))))))))
