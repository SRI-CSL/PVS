;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makes.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Jan  4 23:17:39 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:11:36 1998
;; Update Count    : 27
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(def-pvs-term not-operator "NOT" "booleans")
(def-pvs-term and-operator "AND" "booleans")
(def-pvs-term or-operator "OR" "booleans")
(def-pvs-term implies-operator "IMPLIES" "booleans")
(def-pvs-term iff-operator "IFF" "booleans")
(def-pvs-term plus-operator "+" "number_fields")
(def-pvs-term difference-operator "-" "number_fields" :expected "[number_field, number_field -> number_field]")
(def-pvs-term minus-operator "-" "number_fields" :expected "[number_field -> number_field]")
(def-pvs-term times-operator "*" "number_fields")
(def-pvs-term divides-operator "/" "number_fields")
(def-pvs-term greatereq-operator ">=" "reals")
(def-pvs-term greater-operator ">" "reals")
(def-pvs-term lesseq-operator "<=" "reals")
(def-pvs-term less-operator "<" "reals")
(def-pvs-term floor-operator "floor" "floor_ceil")
(def-pvs-term unary-minus-operator "-" "number_fields" :expected "[number_field -> number_field]")
(def-pvs-term integer_pred "integer_pred" "integers")
(def-pvs-term rational_pred "rational_pred" "rationals")
(def-pvs-term real_pred "real_pred" "reals")
(def-pvs-term number-field_pred "number_field_pred" "number_fields")

(def-pvs-term expt-operator "expt" "exponentiation")

(def-pvs-term upfrom-subtype "upfrom" "int_types" :nt type-expr)
(def-pvs-term below-subtype "below" "nat_types" :nt type-expr)
(def-pvs-term upto-subtype "upto" "nat_types" :nt type-expr)
(def-pvs-term subrange-subtype "subrange" "subrange_type" :nt type-expr)
(def-pvs-term above-subtype "above" "int_types" :nt type-expr)

(def-pvs-term add-operator "add" "strings" :expected "[nat, set[nat] -> set[nat]]")
(def-pvs-term remove-operator "remove" "strings" :expected "[nat, set[nat] -> set[nat]]")
(def-pvs-term singleton-operator "singleton" "strings" :expected "[nat -> set[nat]]")
(def-pvs-term union-operator "union" "strings" :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term intersection-operator "intersection" "strings" :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term set-difference-operator "difference" "strings" :expected "[set[nat], set[nat] -> set[nat]]")
(def-pvs-term emptyset-operator "emptyset" "strings" :expected "set[nat]")
(def-pvs-term fullset-operator "fullset" "strings" :expected "set[nat]")


(def-pvs-term fset-of-nats "finite_set[nat]" "strings" :nt type-expr)

(def-pvs-term empty-fset-of-nats "emptyset[nat]" "strings" :expected "finite_set[nat]")

(def-pvs-term add-to-fset "add[nat]" "strings" :expected "[nat, finite_set[nat] -> finite_set[nat]]")

(def-pvs-term minus1 "-" "naturalnumbers" :expected "[nat, nat -> nat]")
(def-pvs-term plus1  "+" "naturalnumbers" :expected "[nat, nat -> nat]")

(def-pvs-term number-cross-number "[number, number]" "reals" :nt type-expr)


(let ((*one-constant* nil))
  (defun one-constant ()
    (or *one-constant*
	(make!-number-expr 1))))
  
;;; This file provides make-class for the useful classes in classes.
;;; For each such class, it basically provides a nicer syntax to replace
;;; make-instance, along with a better interpretation of the arguments.
;;; There are two kinds of makes.  Those that begin with mk- simply
;;; generate an appropriate instance.  Those that begin with make- will
;;; attempt to typecheck the resulting instance.

(defun mk-datatype (id formals assuming constructors)
  (make-instance 'datatype
    'id id
    'formals formals
    'assuming assuming
    'constructors constructors))

(defun mk-module (id formals assuming exporting theory)
  (make-instance 'module
    'id id
    'formals formals
    'assuming assuming
    'exporting exporting
    'theory theory))

(defun mk-type-decl (id &optional (class 'type-decl) type-expr)
  (if (eq class 'type-decl)
      (make-instance 'type-decl
	'id id)
      (make-instance class
	'id id
	'type-expr type-expr)))
    

(defun mk-var-decl (id type &optional (declared-type type))
  (make-instance 'var-decl
    'id id
    'type type
    'declared-type declared-type))

(defun mk-const-decl (id type &optional definition formals dtype)
  (make-instance 'const-decl
    'id id
    'formals (if (every@ #'consp formals) formals (list formals))
    'declared-type (or dtype type)
    'type type
    'definition definition))

(defun mk-adt-constructor-decl (id type &optional num)
  (make-instance 'adt-constructor-decl
    'id id
    'declared-type type
    'ordnum num))

(defun mk-adt-recognizer-decl (id type &optional num)
  (make-instance 'adt-recognizer-decl
    'id id
    'declared-type type
    'ordnum num))

(defun mk-adt-accessor-decl (id type)
  (make-instance 'adt-accessor-decl
    'id id
    'declared-type type))

(defun mk-adt-def-decl (id type &optional definition formals dtype)
  (make-instance 'adt-def-decl
    'id id
    'formals (if (every@ #'consp formals) formals (list formals))
    'declared-type (or dtype type)
    'type type
    'definition definition
    'semi t))

(defun mk-inductive-decl (id type &optional definition formals dtype)
  (make-instance 'inductive-decl
    'id id
    'formals (if (every@ #'consp formals) formals (list formals))
    'declared-type (or dtype type)
    'type type
    'definition definition
    'semi t))

(defun mk-coinductive-decl (id type &optional definition formals dtype)
  (make-instance 'coinductive-decl
    'id id
    'formals (if (every@ #'consp formals) formals (list formals))
    'declared-type (or dtype type)
    'type type
    'definition definition
    'semi t))

(defun mk-proj-decl (id type &optional definition formals dtype)
  (make-instance 'proj-decl
    'id id
    'formals (if (every@ #'consp formals) formals (list formals))
    'declared-type (or dtype type)
    'type type
    'definition definition))

(defun mk-formula-decl (id expr &optional (spelling 'formula) kind)
  (make-instance 'formula-decl
    'id id
    'spelling spelling
    'kind kind
    'definition expr
    'semi t))

(defun mk-subtype-tcc (id expr)
  (make-instance 'subtype-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-termination-tcc (id expr)
  (make-instance 'termination-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-existence-tcc (id expr)
  (make-instance 'existence-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'existence
    'definition expr
    'semi t))

(defun mk-assuming-tcc (id expr theory-instance assuming-decl)
  (make-instance 'assuming-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'theory-instance theory-instance
    'generating-assumption assuming-decl
    'semi t))

(defun mk-mapped-axiom-tcc (id expr theory-instance axiom-decl)
  (make-instance 'mapped-axiom-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'theory-instance theory-instance
    'generating-axiom axiom-decl
    'semi t))

(defun mk-cases-tcc (id expr)
  (make-instance 'cases-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-well-founded-tcc (id expr)
  (make-instance 'well-founded-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-same-name-tcc (id expr)
  (make-instance 'same-name-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-cond-disjoint-tcc (id expr)
  (make-instance 'cond-disjoint-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-cond-coverage-tcc (id expr)
  (make-instance 'cond-coverage-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-monotonicity-tcc (id expr)
  (make-instance 'monotonicity-tcc
    'id id
    'spelling 'OBLIGATION
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-type-name (id &optional actuals mod-id resolution)
  (cond ((type-name? id)
	 (copy id))
	((name? id)
	 (make-instance 'type-name
	   'id (id id)
	   'actuals (actuals id)
	   'mod-id (mod-id id)
	   'parens (parens id)
	   'library (library id)
	   'place (place id)))
	(t (make-instance 'type-name
	     'id id
	     'actuals actuals
	     'mod-id mod-id
	     'resolutions (when resolution (list resolution))))))

(defun mk-dep-binding (id &optional type dtype)
  (assert (or dtype type))
  (make-instance 'dep-binding
		 'id id
		 'declared-type (or dtype type)
		 'type type))

(defun mk-subtype (supertype predicate)
  (make-instance 'subtype
    'supertype supertype
    'predicate predicate))

(defun mk-setsubtype (supertype predicate)
  (make-instance 'setsubtype
    'supertype supertype
    'predicate predicate
    'formals (if (lambda-expr? predicate)
		 (if (singleton? (bindings predicate))
		     (car (bindings predicate))
		     (bindings predicate))
		 (mk-name-expr
		     (make-new-variable '|t| (list supertype predicate))))
    'formula (if (lambda-expr? predicate)
		 (expression predicate)
		 (mk-application predicate
		   (mk-name-expr
		    (make-new-variable '|t|
				       (list supertype predicate)))))))

(defun mk-expr-as-type (expr)
  (make-instance 'expr-as-type 'expr expr))

(defmethod mk-funtype ((domain cons) range &optional (class 'funtype))
  (assert range)
  (mk-funtype (if (cdr domain)
		  (mk-tupletype domain)
		  (car domain))
	      range class))

(defmethod mk-funtype ((domain type-expr) range &optional (class 'funtype))
  (assert (typep range 'type-expr))
  (if (eq class 'funtype)
      (make-instance 'funtype 'domain domain 'range range)
      (make-instance class 'domain domain 'range range)))

(defmethod mk-funtype ((domain type-expr) (range dep-binding)
		       &optional (class 'funtype))
  (mk-funtype domain (type range) class))

(defmethod mk-funtype ((domain dep-binding) range &optional (class 'funtype))
  (if (eq class 'funtype)
      (make-instance 'funtype 'domain domain 'range range)
      (make-instance class 'domain domain 'range range)))
  
(defun mk-predtype (type)
  (mk-funtype type *boolean*))

(defun mk-tupletype (types)
  (assert (and (listp types) (cdr types)))
  (make-instance 'tupletype 'types types))

(defun mk-cotupletype (types)
  (assert (and (listp types) (cdr types)))
  (make-instance 'cotupletype 'types types))

(defun mk-recordtype (field-decls dependent?)
  (make-instance 'recordtype
    'fields (sort-fields field-decls dependent?)
    'dependent? dependent?))

(defun mk-field-decl (id dtype &optional type)
  (make-instance 'field-decl
    'id (if (and (syntax? id)
		 (slot-exists-p id 'id))
	    (id id) id)
    'type type
    'declared-type dtype))

;(defmethod mk-name-expr :around (obj &optional actuals mod res kind)
;  (let* ((nex (call-next-method))
;	 (nres (or res (car (resolutions nex)))))
;    (when nres
;      (change-name-expr-class-if-needed (declaration nres) nex))
;    nex))

(defmethod mk-name-expr ((id number) &optional actuals mod-id res)
  (if res
      (make!-name-expr id actuals mod-id res)
      (make-instance 'name-expr
	'id id
	'mod-id mod-id
	'actuals actuals)))

(defmethod mk-name-expr ((id symbol) &optional actuals mod-id res)
  (if res
      (make!-name-expr id actuals mod-id res)
      (make-instance 'name-expr
	'id id
	'mod-id mod-id
	'actuals actuals)))

(defmethod mk-name-expr ((obj bind-decl) &optional actuals mod-id res)
  (if res
      (make!-name-expr (id obj) actuals mod-id res)
      (make-instance 'name-expr
	'id (id obj)
	'mod-id mod-id
	'actuals actuals
	'type (type obj)
	'resolutions (list (mk-resolution obj
			     (current-theory-name) (type obj))))))

(defmethod mk-name-expr ((obj name) &optional actuals mod-id res)
  (if (or res (resolution obj))
      (make!-name-expr (id obj) actuals mod-id (or res (resolution obj)))
      (make-instance 'name-expr
	'id (id obj)
	'mod-id mod-id
	'actuals actuals
	'type (when (expr? obj) (type obj))
	'resolutions (when (name? obj) (resolutions obj)))))

(defmethod mk-name-expr (obj &optional actuals mod-id res)
  (if res
      (make!-name-expr (id obj) actuals mod-id res)
      (make-instance 'name-expr
	'id (id obj)
	'mod-id mod-id
	'actuals actuals)))

(defun mk-number-expr (num)
  (make-instance 'number-expr
    'number num))

(defun mk-record-expr (assignments)
  (make-instance 'record-expr
    'assignments assignments))

(defun mk-tuple-expr (exprs)
  (assert (cdr exprs))
  (make-instance 'tuple-expr
    'exprs exprs))

(defun mk-cases-expr (expr selections else)
  (make-instance 'cases-expr
    'expression expr
    'selections selections
    'else-part else))

(defun mk-selection (name-expr args expr)
  (make-instance 'selection
    'constructor name-expr
    'args args
    'expression expr))

(defun mk-application* (op arguments)
  (assert (listp arguments))
  (apply #'mk-application op arguments))      
		 
(defun mk-application (op &rest args)
  (let* ((class (application-class op args))
	 (operator (if (expr? op) op (mk-name-expr op)))
	 (argument (if (cdr args)
		       (make-instance 'arg-tuple-expr
			 'exprs args)
		       (car args)))
	 (appl (case class
		 (unary-application
		  (make-instance 'unary-application
		    'operator operator
		    'argument argument))
		 (infix-application
		  (make-instance 'infix-application
		    'operator operator
		    'argument argument))
		 (if-expr
		  (make-instance 'if-expr
		    'operator operator
		    'argument argument))
		 (application
		  (make-instance 'application
		    'operator operator
		    'argument argument)))))
    #+pvsdebug (assert (or (not (expr? op))
			   (null (ptypes op))
			   (every@ #'(lambda (ty)
				       (funtype? (find-supertype ty)))
				   (ptypes op))))
    appl))

(defun application-class (op args)
  (let ((id (if (and (syntax? op)
		     (slot-exists-p op 'id))
		(id op) op))
	(arg-list (if (and (singleton? args)
			   (tuple-expr? (car args)))
		      (exprs (car args))
		      args)))
    (cond ((and (symbolp id)
		(singleton? arg-list)
		(memq id *unary-operators*))
	   'unary-application)
	  ((and (symbolp id)
		(cdr arg-list)
		(null (cddr arg-list))
		(memq id *infix-operators*))
	   'infix-application)
	  ((and (symbolp id)
		(eq id 'IF)
		(= (length arg-list) 3))
	   'if-expr)
	  (t 'application))))

(defun mk-if-expr (cond then else)
  (mk-if-expr* 'if-expr cond then else))

(defun mk-chained-if-expr (cond then else)
  (mk-if-expr* 'chained-if-expr cond then else))

(defun mk-if-expr* (class cond then else)
  (if (eq class 'if-expr)
      (make-instance 'if-expr
	'operator (mk-name-expr 'IF)
	'argument (make-instance 'arg-tuple-expr
		    'exprs (list cond then else)))
      (make-instance 'chained-if-expr
	'operator (mk-name-expr 'IF)
	'argument (make-instance 'arg-tuple-expr
		    'exprs (list cond then else)))))

(defun mk-implication (ante succ)
  (mk-application 'IMPLIES ante succ))

(defun mk-iff (ante succ)
  (mk-application 'IFF ante succ))

(defun mk-conjunction (args)
  (mk-rec-application 'AND *true*
		      (remove-if #'(lambda (a)
				     (tc-eq a *true*))
			args)))

(defun mk-disjunction (args)
  (mk-rec-application 'OR *false*
		      (remove-if #'(lambda (a)
				     (tc-eq a *false*))
			args)))

(defun mk-negation (arg)
  (mk-application 'NOT arg))

(defun mk-rec-application (op base args)
  (cond ((null args)
	 base)
	((null (cdr args))
	 (car args))
	(t (mk-application op
	     (car args)
	     (mk-rec-application op base (cdr args))))))

(defun mk-rec-application-left (op base args)
  (cond ((null args) base)
	((null (cdr args)) (car args))
	(t (mk-rec-application-left
	    op base
	    (cons (mk-application op (car args)(cadr args))
		  (cddr args))))))

(defun mk-lambda-expr (vars expr)
  (make-instance 'lambda-expr
    'bindings (mk-bindings vars)
    'expression expr))

(defun mk-let-expr (bindings expr)
  (change-class
   (mk-application* (mk-lambda-expr (mapcar #'car bindings) expr)
     (mapcar #'cdr bindings))
   'let-expr))

(defun mk-coercion (expr type)
  (let ((var (makesym "x%~a" (funcall *coercion-var-counter*))))
    (make-instance 'coercion
      'operator (mk-lambda-expr (list (mk-bind-decl var type))
		  (mk-name-expr var))
      'argument expr)))

(defun mk-forall-expr (vars expr)
  (make-instance 'forall-expr
    'bindings (mk-bindings vars)
    'expression expr))

(defun mk-exists-expr (vars expr)
  (make-instance 'exists-expr
    'bindings (mk-bindings vars)
    'expression expr))

(defun mk-equation (lhs rhs)
  (mk-application '= lhs rhs))

(defun mk-update-expr (expr assignments)
  (make-instance 'update-expr
    'expression expr
    'assignments assignments))

(defun mk-update-expr-1 (expr index value)
  (let ((assignment (mk-assignment 'uni `((,index)) value)))
    (mk-update-expr expr (list assignment))))

(defun mk-greatereq (a1 a2)
  (mk-application (greatereq-operator) a1 a2))

(defun mk-greater (a1 a2)
  (mk-application (greater-operator) a1 a2))

(defun mk-lesseq (a1 a2)
  (mk-application (lesseq-operator) a1 a2))

(defun mk-less (a1 a2)
  (mk-application (less-operator) a1 a2))

(defun mk-floor (a1)
  (mk-application (floor-operator) a1))

(defun mk-null-expr ()
  (make-instance 'null-expr 'id '|null|))

(defun mk-list-expr (exprs)
  (mk-list-expr* (reverse exprs) (mk-null-expr)))

(defun mk-list-expr* (exprs result)
  (if (null exprs)
      result
      (mk-list-expr*
       (cdr exprs)
       (make-instance 'list-expr
	 'operator (make-instance 'name-expr 'id '|cons|)
	 'argument (make-instance 'arg-tuple-expr
		     'exprs (list (car exprs) result))))))

;;; Note that an expected type is unnecessary; bind-decls always
;;; complain if they don't uniquely typecheck.

(defun make-new-bind-decls (types &optional bind-decls)
  (if (null types)
      (nreverse bind-decls)
      (let* ((id (make-new-variable '|x| (list types bind-decls) 1))
	     (bd (make-bind-decl id (car types))))
	(make-new-bind-decls
	 (if (typep (car types) 'dep-binding)
	     (let ((nvar (make-variable-expr bd)))
	       (substit (cdr types)
		 (acons (car types) nvar nil)))
	     (cdr types))
	 (cons bd bind-decls)))))

(defun make-new-bind-decl (type)
  (let ((id (make-new-variable '|x| type)))
    (make-bind-decl id type)))

(defmethod make-bind-decl (id (type type-expr))
  (typecheck* (mk-bind-decl id (or (print-type type) type) type)
	      nil nil nil))

(defmethod make-bind-decl (id (type dep-binding))
  (make-bind-decl id (type type)))

(defun make-variable-expr (bd)
  (assert (typep bd 'binding))
  (mk-name-expr bd nil nil
		(make-resolution bd
		  (current-theory-name) (type bd))))

(defun mk-bindings (vars)
  (assert vars)
  (if (every@ #'bind-decl? vars)
      vars
      (mk-bindings* vars)))

(defun mk-bindings* (vars &optional result)
  (if (null vars)
      (mk-chained-bindings (nreverse result))
      (let* ((id (if (and (syntax? (car vars))
			  (slot-exists-p (car vars) 'id))
		     (id (car vars))
		     (car vars)))
	     (dtype (when (and (syntax? (car vars))
			       (slot-exists-p (car vars) 'declared-type)
			       (declared-type (car vars)))
		      (or (print-type (declared-type (car vars)))
			  (declared-type (car vars)))))
	     (type (when (and (syntax? (car vars))
			      (slot-exists-p (car vars) 'type))
		     (type (car vars))))
	     (bind (mk-bind-decl id dtype type)))
	(mk-bindings* (cdr vars) (cons bind result)))))

(defun mk-chained-bindings (bindings)
  (mapl #'(lambda (blist)
	    (when (and (cdr blist)
		       (declared-type (car blist))
		       (declared-type (cadr blist))
		       (ps-eq (declared-type (car blist))
			      (declared-type (cadr blist))))
	      (setf (chain? (car blist)) t)))
	bindings)
  bindings)


(defun mk-bind-decl (id dtype &optional type)
  (assert (symbolp id))
  (make-instance 'bind-decl
    'id id
    'declared-type (when dtype (or (print-type dtype) dtype))
    'type type))

(defun mk-arg-bind-decl (id dtype &optional type)
  (make-instance 'arg-bind-decl
    'id id
    'declared-type (when dtype (or (print-type dtype) dtype))
    'type type))

(defun mk-assignment (flag arguments expression)
  (if (eq flag 'uni)
      (make-instance 'uni-assignment
	'arguments arguments
	'expression expression)
      (make-instance 'assignment
	'arguments arguments
	'expression expression)))

(defun mk-maplet (flag arguments expression)
  (if (eq flag 'uni)
      (make-instance 'uni-maplet
	'arguments arguments
	'expression expression)
      (make-instance 'maplet
	'arguments arguments
	'expression expression)))

(defun mk-modname (id &optional actuals library mappings)
  (make-instance 'modname
    'id id
    'actuals actuals
    'library library
    'mappings mappings))

(defun mk-modname-no-tccs (id &optional actuals mappings)
  (make-instance 'modname-no-tccs
    'id id
    'actuals (mapcar #'(lambda (a) (make-instance 'actual
				     'expr (expr a)
				     'type-value (type-value a)))
		     actuals)
    'mappings mappings))

(defmethod mk-auto-rewrite-name ((decl declaration) theory-name always?)
  (case always?
    (!! (make-instance 'macro-rewrite-name
	  'id (id decl)
	  'actuals (actuals theory-name)
	  'mod-id (id theory-name)))
    ((nil) (make-instance 'lazy-rewrite-name
	     'id (id decl)
	     'actuals (actuals theory-name)
	     'mod-id (id theory-name)))
    (t (make-instance 'eager-rewrite-name
	     'id (id decl)
	     'actuals (actuals theory-name)
	     'mod-id (id theory-name)))))

(defun mk-name (id &optional actuals mod resolution)
  (make-instance 'name
    'id id
    'actuals actuals
    'mod-id mod
    'resolutions (list resolution)))

(defmethod mk-actual ((arg type-expr))
  (let ((expr (or (print-type arg) arg))
	(type-value (lcopy arg 'from-conversion nil)))
    (make-instance 'actual 'expr expr 'type-value type-value)))

(defmethod mk-actual ((arg type-name))
  (let ((expr (make-instance 'name-expr
		'id (id arg)
		'mod-id (mod-id arg)
		'actuals (actuals arg)
		'resolutions (resolutions arg)))
	(type-value (lcopy arg 'from-conversion nil)))
    (make-instance 'actual 'expr expr 'type-value type-value)))

(defmethod mk-actual ((arg expr))
  (make-instance 'actual 'expr arg))

(defun mk-mapping (lhs rhs)
  (make-instance 'mapping
    'lhs lhs
    'rhs (mk-mapping-rhs rhs)))

(defmethod mk-mapping-rhs ((ex type-expr))
  (make-instance 'mapping-rhs
    'expr (or (print-type ex) ex)
    'type-value (lcopy ex 'from-conversion nil)))

(defmethod mk-mapping-rhs ((ex expr))
  (make-instance 'mapping-rhs 'expr ex))

(defmethod mk-mapping-rhs ((ex mapping-rhs))
  ex)

(defun mk-proof-info (id description create-date run-date script status
			 refers-to real-time run-time interactive?
			 &optional decision-procedure)
  (make-instance 'proof-info
    'id id
    'description description
    'create-date create-date
    'run-date run-date
    'script (if (= (length script) 3)
		(append script (list nil))
		script)
    'status status
    'refers-to (typecase (car refers-to)
		 (declaration refers-to)
		 (declaration-entry
		  (mapcar #'get-declaration-entry-decl refers-to))
		 (t (mapcar #'get-referenced-declaration
		      (remove-if #'null refers-to))))
    'real-time real-time
    'run-time run-time
    'interactive? interactive?
    'decision-procedure-used decision-procedure))

(defun make-proof-info (script &optional id description)
  (assert (symbolp id))
  (assert (or (null description) (stringp description)))
  (assert (typep script '(or list justification)))
  ;;(assert (not (null script)))
  (make-instance 'proof-info
    'id id
    'description description
    'script (if (= (length script) 3)
		(append script (list nil))
		script)
    'create-date (get-universal-time)
    'status 'unchecked))

(defun make-recordtype (fields)
  #+pvsdebug (assert (every@ #'(lambda (fd)
				 (and (field-decl? fd)
				      (declared-type fd)
				      (type fd)))
			     fields))
  (make-instance 'recordtype
    'fields (sort fields #'string-lessp :key #'id)))

(defun make-tupletype (types)
  (make-instance 'tupletype 'types types 'generated? t))

(defun make-cotupletype (types)
  (make-instance 'cotupletype 'types types 'generated? t))

(defun make-domain-type-from-bindings (vars)
  (if (cdr vars)
      (make-tupletype-from-bindings vars)
      (type (car vars))))

(defun make-tupletype-from-bindings (vars &optional result)
  (if (null vars)
      (make-tupletype (nreverse result))
      (let* ((var (car vars))
	     (ntype (if (some #'(lambda (v)
				  (member var (freevars (type v))
					  :test #'same-declaration))
			      (cdr vars))
			(mk-dep-binding (id var)
					(type var)
					(declared-type var))
			(type var))))
	(make-tupletype-from-bindings
	 (if (typep ntype 'dep-binding)
	     (substit (cdr vars) (acons (car vars) ntype nil))
	     (cdr vars))
	 (cons ntype result)))))

(defun make-funtype (domain range) ;NSH(12.28.93)
  (mk-funtype domain range))

(defun make-predtype (type)
  (mk-predtype type))

(defun mk-resolution (decl modinst type)
  (make-instance 'resolution
    'declaration decl
    'module-instance modinst
    'type type))

;;; The following typecheck the results

(defun make-tuple-expr (exprs &optional expected)
  (cond (expected
	 (typecheck (mk-tuple-expr exprs) :expected expected))
	(t (assert (every #'type exprs))
	   (let ((tupex (mk-tuple-expr exprs))
		 (type (mk-tupletype (mapcar #'type exprs))))
	     (setf (type tupex) type)
	     tupex))))

(defun make-record-expr (assignments expected)
  (typecheck (mk-record-expr assignments) :expected expected))

(defun make-cases-expr (expr selections else)
  (let ((expr (mk-cases-expr expr selections else)))
    (break "make-cases-expr - need to handle expected type")
    (typecheck expr nil nil nil)))

(defun make-arg-tuple-expr (args)
  (assert (every #'type args))
  (if (cdr args)
      (let ((ttype (mk-tupletype (mapcar #'type args))))
	(make-instance 'arg-tuple-expr
	  'exprs args
	  'type ttype))
      (car args)))

(defun make-application* (op arguments)
  (apply #'make-application op arguments))
		 
(defmethod make-application (op &rest arguments)
  (let* ((args arguments)
	 (expr (mk-application* op args)))
    (assert *current-context*)
    (if (and (expr? op) (type op))
	(typecheck expr :expected (car (application-range-types expr)))
	(error op "Operator must be typechecked"))))

(defmethod make-application ((op field-assignment-arg) &rest arguments)
  (assert (singleton? arguments) ()
	  "A field name can only be applied to a sigle argument")
  (make-field-application op (car arguments)))

(defun make-projection-application (index arg)
  (assert (type arg))
  (let* ((stype (find-supertype (type arg)))
	 (projtype (projection-type* (types stype) index 1 arg (type arg))))
    (make-instance 'projappl
      'id (makesym "PROJ_~d" index)
      'index index
      'argument arg
      'type projtype)))

(defun make-field-application (field-name arg)
  (assert (and (type arg) (typep (find-supertype (type arg)) 'recordtype)))
  (let* ((ftype (field-application-type field-name (type arg) arg)))
    (make-instance 'fieldappl
      'id (ref-to-id field-name)
      'argument arg
      'type ftype)))


;(defun make-projection (expr index &optional type tuptype)
;  (let ((tuptype (find-supertype (or tuptype (type expr)))))
;    ;;(assert (tupletype? tuptype))
;    (let* ((rng (or type (nth (1- index) (types tuptype))))
;	   (rty (if (dep-binding? rng) (type rng) rng))
;	   (proj (make-instance 'projection-expr
;		   'id (makesym "PROJ_~d" index)
;		   'index index
;		   'type (mk-funtype (list (or tuptype (type expr))) rty))))
;      (make-application proj expr))))

(defun make-projections (expr &optional type)
  (let ((tuptype (find-supertype (or type (type expr)))))
    (assert (tupletype? tuptype))
    (assert (type expr))
    (make-projections* (types tuptype) expr 1 nil)))

(defun make-projections* (types arg index projapps)
  (assert (type arg))
  (if (null types)
      (nreverse projapps)
      (let* ((cartypes (if (typep (car types) 'dep-binding)
			   (type (car types))
			   (car types)))
	     (cdrtypes (if (typep (car types) 'dep-binding)
			   (substit (cdr types)
			     (acons (car types)
				    (make-projection-application index arg)
				    nil))
			   (cdr types)))
	     (projappl (make-instance 'projappl
			 'id (makesym "PROJ_~d" index)
			 'index index
			 'argument arg
			 'type cartypes)))
	(make-projections* cdrtypes arg (1+ index) (cons projappl projapps)))))


;;; projection-application-type finds the type of a projection
;;; application.  Note that the argument may not be fully typed, so we may
;;; need to create a fully-typed copy in order to make dependent types
;;; work.

(defun projection-application-type (projapp type)
  (let ((tuptype (find-supertype type)))
    (assert (typep tuptype 'tupletype))
    (projection-type* (types tuptype) (index projapp) 1 (argument projapp) type)))

(defun projection-type* (types index ctr arg type)
  (if (= index ctr)
      (if (typep (car types) 'dep-binding)
	  (type (car types))
	  (car types))
      (let* ((dep? (typep (car types) 'dep-binding))
	     (narg (if dep? (make-typed-copy arg type) arg))
	     (cdrtypes (if dep?
			   (substit (cdr types)
			     (acons (car types)
				    (make-projection-application ctr narg)
				    nil))
			   (cdr types))))
	(projection-type* cdrtypes index (1+ ctr) narg type))))

(defun make-typed-copy (expr type)
  (if (type expr)
      expr
      (let ((*generate-tccs* 'none))
	(typecheck* (copy-untyped expr) type nil nil))))


(defun field-application-types (types expr)
  (mapcar #'(lambda (ty) (field-application-type (id expr) ty (argument expr)))
	  types))

(defun field-application-type (field type arg)
  (let ((field-id (ref-to-id field))
	(rtype (find-supertype type)))
    (assert (typep rtype 'recordtype))
    (if (dependent? rtype)
	(field-application-type* (fields rtype) field-id arg)
	(type (find field-id (fields rtype)
		    :test #'(lambda (x y) (eq x (id y))))))))

(defun field-application-type* (fields field-id arg)
  (assert fields)
  (if (eq (id (car fields)) field-id)
      (type (car fields))
      (let* ((fapp (make-instance 'fieldappl
		    'id (id (car fields))
		    'argument arg
		    'type (type (car fields))))
	    (cdrfields (subst-rec-dep-type fapp (car fields) (cdr fields))))
	(field-application-type* cdrfields field-id arg))))


(defun make-if-expr (cond then else)
  (let ((if-expr (mk-if-expr cond then else)))
    (assert *current-context*)
    (if (and (type then) (type else))
	(let ((stype (compatible-type (type then) (type else))))
	  (if stype
	      (typecheck if-expr :expected stype)
	      (type-incompatible else (list (type else)) (type then))))
	(error "then and else must be typechecked"))))

(defun make-chained-if-expr (cond then else)
  (let ((if-expr (mk-chained-if-expr cond then else)))
    (assert *current-context*)
    (if (and (type then) (type else))
	(let ((stype (compatible-type (type then) (type else))))
	  (if stype
	      (typecheck if-expr :expected stype)
	      (type-incompatible else (list (type else)) then)))
	(error "then and else must be typechecked"))))


;;; make-equation makes an equality application and guarantees that the "real"
;;; = is used.  It does this by setting the mod-id and the actuals prior to
;;; typechecking, and then unsetting them when done so that it prints nicely.

(defun make-equation (lhs rhs)
  (if (and (eq *generate-tccs* 'none)
	   (type lhs)
	   (type rhs))
      (make!-equation lhs rhs)
      (let* ((type (find-supertype (or (type lhs) (type rhs))))
	     (res (mk-resolution (equality-decl)
		    (make-instance 'modname
		      'id '|equalities|
		      'actuals (list (mk-actual type)))
		    (mk-funtype (list type type) *boolean*)))
	     (eqname (make-instance 'name-expr
		       'id '=
		       'type (type res)
		       'resolutions (list res)))
	     (appl (mk-application eqname lhs rhs)))
	(typecheck* appl *boolean* nil nil)
	(setf (mod-id eqname) nil
	      (actuals eqname) nil)
	appl)))

(let ((equality-decl nil))
  (defun equality-decl ()
    (or equality-decl
	(setq equality-decl
	      (find-if #'(lambda (d)
			   (eq (id (module d)) '|equalities|))
		(gethash '= (current-declarations-hash))))))
  (defun reset-equality-decl ()
    (setq equality-decl nil)))

(let ((if-decl nil))
  (defun if-declaration ()
    (or if-decl
	(setq if-decl
	      (find-if #'(lambda (d)
			   (eq (id (module d)) '|if_def|))
		(gethash 'IF (current-declarations-hash))))))
  (defun reset-if-declaration ()
    (setq if-decl nil)))

(defun make-implication (ante succ)
  (if (and (eq *generate-tccs* 'none)
	   (type ante)
	   (tc-eq (find-supertype (type ante)) *boolean*)
	   (type succ)
	   (tc-eq (find-supertype (type succ)) *boolean*))
      (make!-implication ante succ)
      (let ((expr (mk-implication ante succ)))
	(assert *current-context*)
	(typecheck expr :expected *boolean*))))

(defun make-iff (ante succ)
  (let ((expr (mk-iff ante succ)))
    (assert *current-context*)
    (typecheck expr :expected *boolean*)))

(defun make-conjunction (args)
  (if (null args)
      *true*
      (if (and (eq *generate-tccs* 'none)
	       (every #'(lambda (a)
			  (and (type a)
			       (tc-eq (find-supertype (type a)) *boolean*)))
		      args))
	  (if (cdr args)
	      (make!-conjunction* (copy-list args))
	      (car args))
	  (let ((expr (mk-conjunction args)))
	    (assert *current-context*)
	    (typecheck expr :expected *boolean*)))))

(defun make-disjunction (args)
  (if (null args)
      *false*
      (if (and (eq *generate-tccs* 'none)
	       (every #'(lambda (a)
			  (and (type a)
			       (tc-eq (find-supertype (type a)) *boolean*)))
		      args))
	  (if (cdr args)
	      (make!-disjunction* (copy-list args))
	      (car args))
	  (let ((expr (mk-disjunction args)))
	    (assert *current-context*)
	    (typecheck expr :expected *boolean*)))))

(defun make-negation (arg)
  (if (and (eq *generate-tccs* 'none)
	   (type arg)
	   (tc-eq (find-supertype (type arg)) *boolean*)
	   (or (not (typep arg 'name-expr))
	       (resolution arg)))
      (make!-negation arg)
      (let* ((expr (mk-negation arg)))
	(assert *current-context*)
	(typecheck expr :expected *boolean*))))

(defun make-lambda-expr (vars expr)
  (let ((nexpr (mk-lambda-expr vars expr)))
    (assert *current-context*)
    (cond ((and (type expr)
		(every #'type (bindings nexpr)))
	   (typecheck nexpr
		      :expected (mk-funtype (mapcar #'type
						    (bindings nexpr))
					    (type expr))))
	  (t (error "Types not available in make-lambda-expr")))))

(defun make-forall-expr (vars expr)
  (let ((nexpr (mk-forall-expr vars expr)))
    (assert *current-context*)
    (typecheck nexpr :expected *boolean*)))

(defun make-exists-expr (vars expr)
  (let ((nexpr (mk-exists-expr vars expr)))
    (assert *current-context*)
    (typecheck nexpr :expected *boolean*)))

(defun make-null-expr (type)
  (typecheck* (mk-null-expr) type nil nil))

(defun make-list-expr (exprs &optional type)
  (assert (or type
	      (and exprs (every #'type exprs))))
  (if type
      (typecheck* (mk-list-expr exprs) type nil nil)
      (let ((ctype (reduce #'compatible-exprs-type exprs)))
	(typecheck* (mk-list-expr exprs) ctype nil nil))))

(let ((numhash (make-hash-table :test #'eql)))
  (pushnew 'clrnumhash *load-prelude-hook*)
  (defun make-number-expr (number)
    (or (gethash number numhash)
	(let ((expr (if (< number 0)
			(mk-application '- (make-number-expr (- 0 number)))
			(mk-number-expr number))))
	  (typecheck expr :expected
		     (if (< number 0) *integer* *naturalnumber*))
	  (setf (gethash number numhash) expr)
	  expr)))
  (defun clrnumhash ()
    (clrhash numhash))
  (defun show-numhash ()
    (ppr numhash)))

(defun make-difference (a1 a2 type)
  (typecheck (make-instance 'infix-application
	       'operator (difference-operator)
	       'argument (make!-arg-tuple-expr a1 a2))
    :expected type))

(defun mk-field-name-expr (id res)
  (make-instance 'field-name-expr
    'id id
    'type (when res (type res))
    'resolutions (when res (list res))))

(defmethod make-name ((res resolution))
  (make-instance 'name
    'id (id (declaration res))
    'mod-id (id (module-instance res))
    'actuals (actuals (module-instance res))))


(defmethod make-declared-type :around ((te type-expr))
  (or (print-type te)
      (call-next-method)))

(defmethod make-declared-type ((te type-expr))
  te)

(defmethod make-declared-type ((te dep-binding))
  (lcopy te 'type (make-declared-type (type te))))

(defmethod make-declared-type ((te subtype))
  (lcopy te 'supertype (make-declared-type (supertype te))))

(defmethod make-declared-type ((te funtype))
  (lcopy te 'domain (make-declared-type (domain te))
	 'range (make-declared-type (range te))))

(defmethod make-declared-type ((te tupletype))
  (lcopy te 'types (make-declared-type (types te))))

(defmethod make-declared-type ((te cotupletype))
  (lcopy te 'types (make-declared-type (types te))))

(defmethod make-declared-type ((list list))
  (let ((nlist (mapcar #'make-declared-type list)))
    (if (equal nlist list) list nlist)))


(defun mk-arg-tuple-expr* (args)
  (assert args)
  (assert (every #'(lambda (arg) (expr? arg)) args))
  (if (cdr args)
      (make-instance 'arg-tuple-expr
	'exprs args
	'place (merge-places (place (car args))
			     (place (car (last args)))))
      (car args)))

(defun mk-arg-tuple-expr (&rest args)
  (mk-arg-tuple-expr* args))

;(defun make-arg-tuple-expr* (args)
;  (assert args)
;  (assert (every #'(lambda (arg) (expr? arg)) args))
;  (assert (every #'type args))
;  (assert *current-context*)
;  (if (cdr args)
;      (let ((expr (make-instance 'arg-tuple-expr 'exprs args)))
;	(typecheck expr)
;	expr)
;      (car args)))
;
;(defun make-arg-tuple-expr (&rest args)
;  (make-arg-tuple-expr* args))

(defun mk-sum (a1 a2)
  (make!-plus a1 a2))

(defun mk-difference (a1 a2)
  (make!-difference a1 a2))

(defun mk-product (a1 a2)
  (make!-times a1 a2))

(defun mk-division (a1 a2)
  (make!-divides a1 a2))

(defun mk-implies-operator ()
  (implies-operator))

(defmethod make-assignment ((arg expr) expression)
  (mk-assignment 'uni (list (list arg)) expression))

(defmethod make-assignment ((arg name-expr) expression)
  (if (and (typep (declaration arg) 'field-decl)
	   (typep arg '(not field-assignment-arg)))
      (call-next-method (change-class (copy arg) 'field-assign)
			expression)
      (call-next-method)))

(defmethod make-assignment ((args list) expression)
  (if (every #'(lambda (a) (typep a 'expr)) args)
      (mk-assignment (unless (cdr args) 'uni) (list args) expression)
      (if (every #'(lambda (arg)
		     (and (listp arg)
			  (every #'(lambda (a) (typep a 'expr)) arg)))
		 args)
	  (mk-assignment nil args expression)
	  (error "make-assignment bad arguments: must be expression, list of exprs, or list of list of exprs"))))

(defun make-update-expr (expression assignments &optional expected)
  (typecheck (make-instance 'update-expr
			    'expression expression
			    'assignments assignments)
	     :expected expected))

(defun make-greatereq (x y)
  (typecheck (mk-greatereq x y) :expected *boolean* :tccs 'top))

(defun make-greater (x y)
  (typecheck (mk-greater x y) :expected *boolean* :tccs 'top))

(defun make-lesseq (x y)
  (typecheck (mk-lesseq x y) :expected *boolean* :tccs 'top))

(defun make-less (x y)
  (typecheck (mk-less x y) :expected *boolean* :tccs 'top))

(defun make-floor (x)
  (typecheck (mk-floor x) :expected *integer* :tccs 'top))

;;; make!- forms assume that the provided expressions are fully
;;; typechecked, and generate typed expressions accordingly.

;;; (make!-applications (operator* ex) (argument* ex)) == ex
;;; Note: args-list may be a simple list of arguments, or a list of
;;; argument-lists
(defun make!-applications (op args-list)
  (if args-list
      (make!-applications (if (consp (car args-list))
			      (make!-application* op (car args-list))
			      (make!-application op (car args-list)))
			  (cdr args-list))
      op))

(defun make!-application* (op arguments)
  (make!-application op
		     (if (cdr arguments)
			 (make!-arg-tuple-expr* arguments)
			 (car arguments))))

(defun make!-application (op &rest args)
  (assert (type op))
  (make!-reduced-application op
			     (if (cdr args)
				 (make!-arg-tuple-expr* args)
				 (car args))))

(defmethod make!-reduced-application ((op lambda-expr) (arg tuple-expr))
  (if (singleton? (bindings op))
      (call-next-method)
      (substit (expression op)
	(pairlis (bindings op) (exprs arg)))))

(defmethod make!-reduced-application ((op lambda-expr) arg)
  (if (singleton? (bindings op))
      (substit (expression op)
	(acons (car (bindings op)) arg nil))
      (substit (expression op)
	(pairlis (bindings op) (make!-projections arg)))))

(defmethod make!-reduced-application (op arg)
  (make!-application-internal op arg))

(defun make!-application-internal (op &rest args)
  (assert (type op))
  (let* ((appl (apply #'mk-application op args))
	 (ftype (find-supertype (type op)))
	 (rtype (if (dep-binding? (domain ftype))
		    (substit (range ftype)
				 (acons (domain ftype) (argument appl) nil))
		    (range ftype))))
    (setf (type appl) rtype)
    (change-application-class-if-needed appl)
    appl))

(defun make!-number-expr (number)
  (assert (typep number 'rational))
  (let* ((num (if (minusp number) (- number) number))
	 (nexpr (if (integerp num)
		    (make-instance 'number-expr
		      'number num
		      'type *real*)
		    (make!-application (divides-operator)
				       (make-instance 'number-expr
					 'number (numerator num)
					 'type *real*)
				       (make-instance 'number-expr
					 'number (denominator num)
					 'type *real*)))))
    (if (minusp number)
	(make!-minus nexpr)
	nexpr)))

(defun make!-name-expr (id actuals mod-id res)
  (assert res)
  (typecase (declaration res)
    (field-decl (make-instance 'field-name-expr
		  'id id
		  'actuals actuals
		  'mod-id mod-id
		  'type (type res)
		  'resolutions (list res)))
    (adt-constructor-decl (make-instance 'constructor-name-expr
			    'id id
			    'actuals actuals
			    'mod-id mod-id
			    'type (type res)
			    'resolutions (list res)))
    (adt-recognizer-decl (make-instance 'recognizer-name-expr
			   'id id
			   'actuals actuals
			   'mod-id mod-id
			   'type (type res)
			   'resolutions (list res)))
    (adt-accessor-decl (make-instance 'accessor-name-expr
			   'id id
			   'actuals actuals
			   'mod-id mod-id
			   'type (type res)
			   'resolutions (list res)))
    (t (make-instance 'name-expr
	 'id id
	 'actuals actuals
	 'mod-id mod-id
	 'type (type res)
	 'resolutions (list res)))))
    
(defun make!-equation (lhs rhs)
  (assert (and (type lhs) (type rhs)))
  (assert (compatible? (type lhs) (type rhs)))
  (let* ((type (find-supertype (type lhs)))
	 (res (mk-resolution (equality-decl)
		(mk-modname '|equalities| (list (mk-actual type)))
		(mk-funtype (list type type) *boolean*)))
	 (eqname (make-instance 'name-expr
		   'id '=
		   'type (type res)
		   'resolutions (list res)))
	 (arg (make!-arg-tuple-expr lhs rhs)))
    (if (tc-eq (find-supertype type) *boolean*)
	(make-instance 'infix-boolean-equation
	  'operator eqname
	  'argument arg
	  'type *boolean*)
	(make-instance 'infix-equation
	  'operator eqname
	  'argument arg
	  'type *boolean*))))

(defun make!-disequation (lhs rhs)
  (assert (and (type lhs) (type rhs)))
  (assert (compatible? (type lhs) (type rhs)))
  (let* ((type (find-supertype (type lhs)))
	 (res (mk-resolution (equality-decl)
		(mk-modname '|notequal| (list (mk-actual type)))
		(mk-funtype (list type type) *boolean*)))
	 (diseqname (make-instance 'name-expr
		      'id '/=
		      'type (type res)
		      'resolutions (list res)))
	 (arg (make!-arg-tuple-expr lhs rhs)))
    (make-instance 'infix-disequation
      'operator diseqname
      'argument arg
      'type *boolean*)))

(defun make!-if-expr (cond then else)
  (make!-if-expr* cond then else nil))

(defun make!-chained-if-expr (cond then else)
  (make!-if-expr* cond then else t))

(defun make!-if-expr* (cond then else chained?)
  (assert (and (type cond) (type then) (type else)))
  (assert (tc-eq (find-supertype (type cond)) *boolean*))
  (assert (compatible? (type then) (type else)))
  (let* ((stype (compatible-type (type then) (type else)))
	 (ifoptype (make-instance 'funtype
		     'domain (make-instance 'tupletype
			       'types (list *boolean* stype stype))
		     'range stype))
	 (if-res (mk-resolution (if-declaration)
		   (mk-modname '|if_def| (list (mk-actual stype)))
		   ifoptype))
	 (if-name (make-instance 'name-expr
		    'id 'IF
		    'type ifoptype
		    'resolutions (list if-res)))
	 (if-args (make-instance 'arg-tuple-expr
		    'exprs (list cond then else)
		    'type (make-instance 'tupletype
			    'types (list *boolean* (type then) (type else))))))
    (if chained?
	(make-instance 'chained-branch
	  'type stype
	  'operator if-name
	  'argument if-args)
	(make-instance 'mixfix-branch
	  'type stype
	  'operator if-name
	  'argument if-args))))

(defun make!-arg-tuple-expr (&rest args)
  (funcall #'make!-arg-tuple-expr* args))

(defun make!-arg-tuple-expr* (args)
  (assert (consp args))
  (assert (every #'type args))
  (if (cdr args)
      (let ((ttype (mk-tupletype (mapcar #'type args))))
	(make-instance 'arg-tuple-expr
	  'exprs args
	  'type ttype))
      (car args)))

(defun make!-projected-arg-tuple-expr (&rest args)
  (funcall #'make!-projected-arg-tuple-expr* args))

(defun make!-projected-arg-tuple-expr* (args)
  (assert (consp args))
  (assert (every #'type args))
  (if (cdr args)
      (let ((ttype (mk-tupletype (mapcar #'type args))))
	(make-instance 'projected-arg-tuple-expr
	  'exprs args
	  'type ttype))
      (car args)))

(defun make!-tuple-expr (&rest exprs)
  (apply #'make!-tuple-expr* exprs))

(defun make!-tuple-expr* (exprs)
  (assert (every #'type exprs))
  (let ((tupex (mk-tuple-expr exprs))
	(type (mk-tupletype (mapcar #'type exprs))))
    (setf (type tupex) type)
    tupex))

(defun make!-projections (expr)
  (assert (type expr))
  (let ((tuptype (find-supertype (type expr))))
    (assert (tupletype? tuptype))
    (make-projections* (types tuptype) expr 1 nil)))

(defun make!-projections* (types arg index projapps)
  (assert (type arg))
  (if (null types)
      (nreverse projapps)
      (let* ((cartype (if (typep (car types) 'dep-binding)
			  (type (car types))
			  (car types)))
	     (projappl (make-instance 'projappl
			 'id (makesym "PROJ_~d" index)
			 'index index
			 'argument arg
			 'type cartype))
	     (cdrtypes (if (typep (car types) 'dep-binding)
			   (substit (cdr types)
			     (acons (car types) projappl nil))
			   (cdr types))))
	(make-projections* cdrtypes arg (1+ index) (cons projappl projapps)))))

(defun make!-projection-application (index arg)
  (assert (type arg))
  (if (tuple-expr? arg)
      (nth (1- index) (exprs arg))
      (let* ((stype (find-supertype (type arg)))
	     (projtype (make!-projection-type* (types stype) index 1 arg)))
	(make-instance 'projappl
	  'id (makesym "PROJ_~d" index)
	  'index index
	  'argument arg
	  'type projtype))))

(defun make!-projection-type* (types index ctr arg)
  (let* ((dep? (typep (car types) 'dep-binding))
	 (cartype (if (typep (car types) 'dep-binding)
		      (type (car types))
		      (car types))))
    (if (= index ctr)
	cartype
	(let* ((proj (make-instance 'projappl
		       'id (makesym "PROJ_~d" index)
		       'index index
		       'argument arg
		       'type cartype))
	       (cdrtypes (if dep?
			     (substit (cdr types)
			       (acons (car types) proj nil))
			     (cdr types))))
	  (make!-projection-type* cdrtypes index (1+ ctr) arg)))))

(defun make!-injection-application (index arg type)
  (assert (type arg))
  (assert (cotupletype? (find-supertype type)))
  (assert (compatible? (type arg)
		       (nth (1- index) (types (find-supertype type)))))
  (let* ((cotuptype (find-supertype (type arg))))
    (make-instance 'injection-application
      'id (makesym "IN_~d" index)
      'index index
      'argument arg
      'type cotuptype)))

(defun make!-injection?-application (index arg)
  (assert (type arg))
  (assert (cotupletype? (find-supertype (type arg))))
  (make-instance 'injection?-application
    'id (makesym "IN?_~d" index)
    'index index
    'argument arg
    'type *boolean*))

(defun make!-extraction-application (index arg)
  (assert (type arg))
  (assert (cotupletype? (find-supertype (type arg))))
  (let* ((cotuptype (find-supertype (type arg))))
    (make-instance 'extraction-application
      'id (makesym "OUT_~d" index)
      'index index
      'argument arg
      'type (nth (1- index) (types cotuptype)))))

(defun make!-field-application (field-name arg)
  (assert (and (type arg) (typep (find-supertype (type arg)) 'recordtype)))
  (let ((fid (ref-to-id field-name)))
    (if (record-expr? arg)
	(let ((ass (find fid (assignments arg)
			 :key #'(lambda (a) (id (caar (arguments a)))))))
	  (assert ass)
	  (expression ass))
	(let ((ftype (make!-field-application-type fid (type arg) arg)))
	  (make-instance 'fieldappl
	    'id fid
	    'argument arg
	    'type ftype)))))

(defun make!-field-application-type (field-id type arg)
  (let ((rtype (find-supertype type)))
    (assert (typep rtype 'recordtype))
    (if (dependent? rtype)
	(make!-field-application-type* (fields rtype) field-id arg)
	(type (find field-id (fields rtype) :test #'eq :key #'id)))))

(defun make!-field-application-type* (fields field-id arg)
  (assert fields)
  (if (eq (id (car fields)) field-id)
      (type (car fields))
      (let* ((fapp (make-instance 'fieldappl
		    'id (id (car fields))
		    'argument arg
		    'type (type (car fields))))
	     (cdrfields (substit (cdr fields)
				     (acons (car fields) fapp nil))))
	(field-application-type* cdrfields field-id arg))))

(defun make!-update-expr (expression assignments)
  (assert (type expression))
  (assert (every #'(lambda (ass) (typep ass '(and assignment (not maplet))))
		 assignments))
  (make-instance 'update-expr
    'expression expression
    'assignments assignments
    'type (type expression)))


;;; The following create special forms that are used frequently

(defun make!-negation (ex)
  (assert (and (type ex) (tc-eq (find-supertype (type ex)) *boolean*)))
  (cond ((tc-eq ex *true*)
	 *false*)
	((tc-eq ex *false*)
	 *true*)
	(t (make-instance 'unary-negation
	     'operator (not-operator)
	     'argument ex
	     'type *boolean*))))

(defun make!-conjunction (ex1 ex2)
  (assert (and (type ex1) (type ex2)
	       (tc-eq (find-supertype (type ex1)) *boolean*)
	       (tc-eq (find-supertype (type ex2)) *boolean*)))
  (cond ((tc-eq ex1 *true*)
	 ex2)
	((tc-eq ex1 *false*)
	 *false*)
	((tc-eq ex2 *true*)
	 ex1)
	((tc-eq ex2 *false*)
	 *false*)
	(t (make-instance 'infix-conjunction
	     'operator (and-operator)
	     'argument (make!-arg-tuple-expr ex1 ex2)
	     'type *boolean*))))

(defun make!-conjunction* (exprs)
  (make!-conjunction** (reverse exprs) *true*))

(defun make!-conjunction** (exprs conj)
  (if (null exprs)
      conj
      (make!-conjunction**
       (cdr exprs)
       (make!-conjunction (car exprs) conj))))

(defun make!-disjunction (ex1 ex2)
  (assert (and (type ex1) (type ex2)
	       (tc-eq (find-supertype (type ex1)) *boolean*)
	       (tc-eq (find-supertype (type ex2)) *boolean*)))
  (cond ((tc-eq ex1 *true*)
	 *true*)
	((tc-eq ex1 *false*)
	 ex2)
	((tc-eq ex2 *true*)
	 *true*)
	((tc-eq ex2 *false*)
	 ex1)
	(t (make-instance 'infix-disjunction
	     'operator (or-operator)
	     'argument (make!-arg-tuple-expr ex1 ex2)
	     'type *boolean*))))

(defun make!-disjunction* (exprs)
  (make!-disjunction** (reverse exprs) *false*))

(defun make!-disjunction** (exprs disj)
  (if (null exprs)
      disj
      (make!-disjunction**
       (cdr exprs)
       (make!-disjunction (car exprs) disj))))

(defun make!-implication (ex1 ex2)
  (assert (and (type ex1) (type ex2)
	       (tc-eq (find-supertype (type ex1)) *boolean*)
	       (tc-eq (find-supertype (type ex2)) *boolean*)))
  (cond ((tc-eq ex1 *true*)
	 ex2)
	((tc-eq ex1 *false*)
	 *true*)
	((tc-eq ex2 *true*)
	 *true*)
	((tc-eq ex2 *false*)
	 (make!-negation ex1))
	(t (make-instance 'infix-implication
	     'operator (implies-operator)
	     'argument (make!-arg-tuple-expr ex1 ex2)
	     'type *boolean*))))

(defun make!-iff (ex1 ex2)
  (assert (and (type ex1) (type ex2)
	       (tc-eq (find-supertype (type ex1)) *boolean*)
	       (tc-eq (find-supertype (type ex2)) *boolean*)))
  (cond ((tc-eq ex1 *true*)
	 ex2)
	((tc-eq ex1 *false*)
	 (make!-negation ex2))
	((tc-eq ex2 *true*)
	 ex1)
	((tc-eq ex2 *false*)
	 (make!-negation ex1))
	(t (make-instance 'infix-iff
	     'operator (iff-operator)
	     'argument (make!-arg-tuple-expr ex1 ex2)
	     'type *boolean*))))

(defun make!-plus (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (plus-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))

(defun make!-difference (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  ;;(assert (eq *generate-tccs* 'none))
  (make-instance 'infix-application
    'operator (difference-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))

(defun make!-minus (ex)
  (assert (type ex))
  (assert (tc-eq (find-supertype (type ex)) *number*))
  (make-instance 'unary-application
    'operator (minus-operator)
    'argument ex
    'type *real*))

(defun make!-times (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (times-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))

(defun make!-divides (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (divides-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))

(defun make!-forall-expr (bindings expr)
  (assert (and (type expr) (tc-eq (find-supertype (type expr)) *boolean*)))
  (make-instance 'forall-expr
    'bindings bindings
    'expression expr
    'type *boolean*))

(defun make!-exists-expr (bindings expr)
  (assert (and (type expr) (tc-eq (find-supertype (type expr)) *boolean*)))
  (make-instance 'exists-expr
    'bindings bindings
    'expression expr
    'type *boolean*))

(defun make!-lambda-expr (bindings expr)
  (assert (every #'type bindings))
  (assert (type expr))
  (make-instance 'lambda-expr
    'bindings bindings
    'expression expr
    'type (make-formals-funtype (list bindings) (type expr))))

(defun make!-set-expr (bindings expr)
  (assert (every #'type bindings))
  (assert (type expr))
  (make-instance 'set-expr
    'bindings bindings
    'expression expr
    'type (make-formals-funtype (list bindings) (type expr))))

(defmethod make!-bind-decl (id (type type-expr))
  (mk-bind-decl id (or (print-type type) type) type))

(defmethod make!-bind-decl (id (type dep-binding))
  (make!-bind-decl id (type type)))

(defun make!-floor (ex)
  (assert (type ex))
  (assert (tc-eq (find-supertype (type ex)) *number*))
  (make-instance 'unary-application
    'operator (floor-operator)
    'argument ex
    'type *naturalnumber*))

(defun make!-succ (ex)
  (assert (type ex))
  (assert (tc-eq (find-supertype (type ex)) *number*))
  (make-instance 'infix-application
    'operator (plus-operator)
    'argument (make!-arg-tuple-expr ex (one-constant))
    'type (type ex)))

(defun make!-pred (ex)
  (assert (type ex))
  (assert (tc-eq (find-supertype (type ex)) *number*))
  (make-instance 'infix-application
    'operator (minus-operator)
    'argument (make!-arg-tuple-expr ex (one-constant))
    'type (type ex)))

(defun make!-expr-as-type (pred)
  (assert (funtype? (find-supertype (type pred))))
  (assert (compatible? (range (find-supertype (type pred))) *boolean*))
  (make-instance 'subtype
    'print-type (mk-expr-as-type pred)
    'supertype (domain (find-supertype (type pred)))
    'predicate pred))

(defun make!-unpack-expr (expr selections &optional else-part)
  (assert (cotupletype? (find-supertype (type expr))))
  (assert (every #'(lambda (sel) (type (expression sel))) selections))
  (assert (every #'(lambda (sel)
		     (compatible? (type (expression sel))
				  (type (expression (car selections)))))
		 (cdr selections)))
  (let ((type (reduce #'compatible-type selections
		      :key #'(lambda (sel) (type (expression sel))))))
    (make-instance 'unpack-expr
      'expression expr
      'selections selections
      'else-part else-part
      'type type)))

(defun make!-unary-minus (ex)
  (assert (typep ex 'expr))
  (assert (tc-eq (find-supertype (type ex)) *number*))
  (make-instance 'infix-application
    'operator (unary-minus-operator)
    'argument ex
    'type (type ex)))

(defun make!-less (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (less-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))

(defun make!-lesseq (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (lesseq-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *real*))
