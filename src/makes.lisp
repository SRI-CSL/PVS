;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; makes.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Jan  4 23:17:39 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Oct 29 22:49:54 1998
;; Update Count    : 25
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

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

(defun mk-context (mod decls using)
  (make-instance 'context
    'module mod
    'mod-name (when mod (mk-modname (id mod)))
    'local-decls (let ((ht (make-hash-table :test #'eq :size 10)))
		   (mapc #'(lambda (d) (put-decl d ht)) decls)
		   ht)
    'using using))

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
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-termination-tcc (id expr)
  (make-instance 'termination-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-existence-tcc (id expr)
  (make-instance 'existence-tcc
    'id id
    'spelling 'obligation
    'kind 'existence
    'definition expr
    'semi t))

(defun mk-assuming-tcc (id expr theory-instance assuming-decl)
  (make-instance 'assuming-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'theory-instance theory-instance
    'generating-assumption assuming-decl
    'semi t))

(defun mk-cases-tcc (id expr)
  (make-instance 'cases-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-well-founded-tcc (id expr)
  (make-instance 'well-founded-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-same-name-tcc (id expr)
  (make-instance 'same-name-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-cond-disjoint-tcc (id expr)
  (make-instance 'cond-disjoint-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-cond-coverage-tcc (id expr)
  (make-instance 'cond-coverage-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-monotonicity-tcc (id expr)
  (make-instance 'monotonicity-tcc
    'id id
    'spelling 'obligation
    'kind 'tcc
    'definition expr
    'semi t))

(defun mk-type-name (id &optional actuals mod-id resolution)
  (if (name? id)
      (let ((nname (change-class (copy id) 'type-name)))
	;;(setf (abstract-syntax nname) nil)
	nname)
      (make-instance 'type-name
	'id id
	'actuals actuals
	'mod-id mod-id
	'resolutions (when resolution (list resolution)))))

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
  (if (some #'(lambda (d)
		(and (typep d 'dep-binding)
		     (occurs-in (id d) range)))
	    domain)
      (break)
      (mk-funtype (if (cdr domain)
		      (mk-tupletype domain)
		      (car domain))
		  range class)))

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
  
(defmethod mk-funtype ((domain judgement-resolution) range
		       &optional (class 'funtype))
  (mk-funtype (judgement-type domain) range class))

(defun mk-predtype (type)
  (mk-funtype type *boolean*))

(defun mk-tupletype (types)
  (assert (and (listp types) (cdr types)))
  (make-instance 'tupletype 'types types))

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

(defmethod mk-name-expr :around (obj &optional actuals mod res kind)
  (let* ((nex (call-next-method))
	 (nres (or res (car (resolutions nex)))))
    (when nres
      (change-name-expr-class-if-needed (declaration nres) nex))
    nex))

(defmethod mk-name-expr ((obj symbol) &optional actuals mod res kind)
  (assert (or (null res) kind))
  (make-instance 'name-expr
    'id obj
    'mod-id mod
    'actuals actuals
    'type (when res (type res))
    'resolutions (when res (list res))
    'kind kind))

(defmethod mk-name-expr ((obj bind-decl) &optional actuals mod res kind)
  (make-instance 'name-expr
    'id (id obj)
    'mod-id mod
    'actuals actuals
    'type (if res
	      (type res)
	      (type obj))
    'kind 'variable
    'resolutions (if res
		     (list res)
		     (resolutions obj))))

(defmethod mk-name-expr (obj &optional actuals mod res kind)
  ;;(assert (or (null res) kind))
  (make-instance 'name-expr
    'id (id obj)
    'mod-id mod
    'actuals actuals
    'type (cond (res
		 (type res))
		((and (syntax? obj)
		      (slot-exists-p obj 'type))
		 (type obj)))
    'kind (or kind
	      (if (and (syntax? obj)
		       (slot-exists-p obj 'kind))
		  (kind obj)))
    'resolutions (if res
		     (list res)
		     (if (and (syntax? obj)
			      (slot-exists-p obj 'resolutions))
			 (resolutions obj)))))

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
		(id op) op)))
    (cond ((and (symbolp id)
		(singleton? args)
		(memq id *unary-operators*))
	   'unary-application)
	  ((and (symbolp id)
		(cdr args)
		(null (cddr args))
		(memq id *infix-operators*))
	   'infix-application)
	  (t 'application))))

(defun mk-if-expr (cond then else)
  (mk-if-expr* 'if-expr cond then else))

(defun mk-chained-if-expr (cond then else)
  (mk-if-expr* 'chained-if-expr cond then else))

(defun mk-if-expr* (class cond then else)
  (if (eq class 'if-expr)
      (make-instance 'if-expr
	'operator (mk-name-expr 'if)
	'argument (make-instance 'arg-tuple-expr
		    'exprs (list cond then else)))
      (make-instance 'chained-if-expr
	'operator (mk-name-expr 'if)
	'argument (make-instance 'arg-tuple-expr
		    'exprs (list cond then else)))))

(defun mk-implication (ante succ)
  (mk-application 'IMPLIES ante succ))

(defun mk-iff (ante succ)
  (mk-application 'IFF ante succ))

(defun mk-conjunction (args)
  (mk-rec-application 'and *true*
		      (remove-if #'(lambda (a)
				     (tc-eq a *true*))
			args)))

(defun mk-disjunction (args)
  (mk-rec-application 'or *false*
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

(defun mk-lambda-expr (vars expr &optional result-type)
  (make-instance 'lambda-expr
    'bindings (mk-bindings vars)
    'expression expr
    'result-type result-type))

(defun mk-let-expr (bindings expr)
  (change-class
   (mk-application* (mk-lambda-expr (mapcar #'car bindings) expr)
     (mapcar #'cdr bindings))
   'let-expr))

(defun mk-forall-expr (vars expr)
  (make-instance 'forall-expr
    'bindings (mk-bindings vars)
    'expression expr
    'parens 1))

(defun mk-exists-expr (vars expr)
  (make-instance 'exists-expr
    'bindings (mk-bindings vars)
    'expression expr
    'parens 1))

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
  (let ((var (mk-name-expr bd nil nil
			   (make-resolution bd nil (type bd)))))
    (setf (kind var) 'variable)
    var))
  

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
  (assert (or (null type) *current-context*))
  (let ((bd (make-instance 'bind-decl
	      'id id
	      'declared-type (when dtype (or (print-type dtype) dtype))
	      'type type)))
    (when type
      (setf (resolutions bd)
	    (list (make-resolution bd (mod-name *current-context*) type))))
    bd))

(defun mk-arg-bind-decl (id dtype &optional type)
  (change-class (mk-bind-decl id dtype type) 'arg-bind-decl))

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

(defun mk-modname (id &optional actuals library)
  (make-instance 'modname
    'id id
    'actuals actuals
    'library library))

(defun mk-modname-no-tccs (id &optional actuals mappings)
  (make-instance 'modname-no-tccs
    'id id
    'actuals (mapcar #'(lambda (a) (make-instance 'actual
				     'expr (expr a)
				     'type-value (type-value a)))
		     actuals)
    'mappings mappings))

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
		'resolutions (resolutions arg)
		'kind 'constant))
	(type-value (lcopy arg 'from-conversion nil)))
    (make-instance 'actual 'expr expr 'type-value type-value)))

(defmethod mk-actual ((arg expr))
  (make-instance 'actual 'expr arg))

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
    (make-instance 'projection-application
      'id (makesym "PROJ_~d" index)
      'index index
      'argument arg
      'type projtype)))

(defun make-field-application (field-name arg)
  (assert (and (type arg) (typep (find-supertype (type arg)) 'recordtype)))
  (let* ((ftype (field-application-type field-name (type arg) arg)))
    (make-instance 'field-application
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
	     (projappl (make-instance 'projection-application
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
      (let* ((fapp (make-instance 'field-application
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
  (let* ((type (find-supertype (or (type lhs) (type rhs))))
	 (res (make-instance 'resolution
		'declaration (equality-decl)
		'module-instance (make-instance 'modname
				   'id '|equalities|
				   'actuals (list (mk-actual type)))
		'type (mk-funtype (list type type) *boolean*)))
	 (eqname (make-instance 'name-expr
		   'id '=
		   'type (type res)
		   'resolutions (list res)
		   'kind 'constant))
	 (appl (mk-application eqname lhs rhs)))
    (if (and (eq *generate-tccs* 'none)
	     (type lhs)
	     (type rhs))
	(let ((argtype (mk-tupletype (list type type))))
	  (setf (type (argument appl)) argtype)
	  (setf (type appl) *boolean*))
	(typecheck* appl *boolean* nil nil))
    (setf (mod-id eqname) nil
	  (actuals eqname) nil)
    appl))

(let ((equality-decl nil))
    (defun equality-decl ()
      (or equality-decl
	  (setq equality-decl
		(car (gethash '= (declarations (get-theory "equalities")))))))
    (defun reset-equality-decl ()
      (setq equality-decl nil)))

(defun make-implication (ante succ)
  (if (and (eq *generate-tccs* 'none)
	   (type ante)
	   (tc-eq (type ante) *boolean*)
	   (type succ)
	   (tc-eq (type succ) *boolean*))
      (make-typechecked-implication ante succ)
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
			       (tc-eq (type a) *boolean*)))
		      args))
	  (if (cdr args)
	      (make-typechecked-conjunction* (copy-list args))
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
			       (tc-eq (type a) *boolean*)))
		      args))
	  (if (cdr args)
	      (make-typechecked-disjunction* (copy-list args))
	      (car args))
	  (let ((expr (mk-disjunction args)))
	    (assert *current-context*)
	    (typecheck expr :expected *boolean*)))))

(defun make-negation (arg)
  (if (and (eq *generate-tccs* 'none)
	   (type arg)
	   (tc-eq (type arg) *boolean*)
	   (or (not (typep arg 'name-expr))
	       (resolution arg)))
      (make-typechecked-negation arg)
      (let* ((expr (mk-negation arg)))
	(assert *current-context*)
	(typecheck expr :expected *boolean*))))

(defun make-lambda-expr (vars expr &optional result-type context)
  (let ((nexpr (mk-lambda-expr vars expr result-type))
	(*current-context* (or context *current-context*)))
    (assert *current-context*)
    (cond (result-type
	   (typecheck nexpr :expected result-type))
	  ((and (type expr)
		(every #'type (bindings nexpr)))
	   (typecheck nexpr
		      :expected (mk-funtype (mapcar #'type
						    (bindings nexpr))
					    (type expr))))
	  (t (error "Types not available in make-lambda-expr")))))

(defun make-forall-expr (vars expr)
  (let ((nexpr (mk-forall-expr vars expr)))
    (setf (parens nexpr) 1)
    (assert *current-context*)
    (typecheck nexpr :expected *boolean*)))

(defun make-exists-expr (vars expr)
  (let ((nexpr (mk-exists-expr vars expr)))
    (setf (parens nexpr) 1)
    (assert *current-context*)
    (typecheck nexpr :expected *boolean*)))

(let ((numhash (make-hash-table :test #'eql)))
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
    (setq numhash (make-hash-table :test #'eql)))
  (defun show-numhash ()
    (ppr numhash)))

(defun mk-field-name-expr (id res kind)
  (make-instance 'field-name-expr
    'id id
    'type (when res (type res))
    'resolutions (when res (list res))
    'kind kind))

(defmethod make-name-expr ((decl dep-binding))
  (let* ((type (type decl))
	 (res (make-resolution decl (mk-modname (id *current-theory*)) type)))
    (mk-name-expr (id decl) nil nil res 'variable)))

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

(defmethod make-declared-type ((list list))
  (let ((nlist (mapcar #'make-declared-type list)))
    (if (equal nlist list) list nlist)))


(defun mk-arg-tuple-expr* (args)
  (assert args)
  (assert (every #'(lambda (arg) (expr? arg)) args))
  (if (cdr args)
      (make-instance 'arg-tuple-expr 'exprs args)
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
  (mk-application (mk-plusop) a1 a2))

(let ((plusop nil))
  (defun mk-plusop ()
    (if plusop
	(copy plusop)
	(let ((plop (mk-name-expr '+ nil '|reals|)))
	  (typecheck plop)
	  (setf (mod-id plop) nil)
	  (setq plusop plop)
	  (copy plop))))
  (defun reset-plusop ()
    (setq plusop nil)))


(defun mk-difference (a1 a2)
  (mk-application (mk-diffop) a1 a2))

(let ((diffop nil))
  (defun mk-diffop ()
    (if diffop
	(copy diffop)
	(let ((diop (mk-name-expr '- nil '|reals|))
	      (expected (mk-funtype (list *real* *real*) *real*)))
	  (typecheck diop :expected expected)
	  (setf (mod-id diop) nil)
	  (setq diffop diop)
	  (copy diop))))
  (defun reset-diffop ()
    (setq diffop nil)))

(defun mk-product (a1 a2)
  (mk-application (mk-prodop) a1 a2))

(let ((prodop nil))
  (defun mk-prodop ()
    (if prodop
	(copy prodop)
	(let ((prop (mk-name-expr '* nil '|reals|)))
	  (typecheck prop)
	  (setf (mod-id prop) nil)
	  (setq prodop prop)
	  (copy prop))))
  (defun reset-prodop ()
    (setq prodop nil)))

(defun mk-division (a1 a2)
  (mk-application (mk-divop) a1 a2))

(let ((divop nil))
  (defun mk-divop ()
    (if divop
	(copy divop)
	(let ((dop (mk-name-expr '/ nil '|reals|)))
	  (typecheck dop)
	  (setf (mod-id dop) nil)
	  (setq divop dop)
	  (copy dop))))
  (defun reset-divop ()
    (setq divop nil)))

(defun mk-implies-operator ()
  (let ((op (mk-name-expr 'implies nil '|booleans|)))
    (typecheck op)
    (setf (mod-id op) nil)
    op))

(defstruct decl-ref
  mod-name
  decl-pos)

(defmethod make-assignment ((arg expr) expression)
  (mk-assignment 'uni (list (list arg)) expression))

(defmethod make-assignment ((arg name-expr) expression)
  (if (typep (declaration arg) 'field-decl)
      (call-next-method (change-class (copy arg) 'field-assignment-arg)
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

(defun make!-update-expr (expression assignments &optional expected)
  (let ((type (or expected (type expression))))
    (make-instance 'update-expr
      'expression expression
      'assignments assignments
      'type type
      ;;'types (list type)
      )))

(defun make!-application (op arg &optional type)
  (let* ((appl (mk-application op arg))
	 (atype (or type (range op))))
    (setf (type appl) atype)
    ;;(setf (types appl) (list atype))
    appl))

(defun make!-number-expr (num)
  (let ((nexpr (mk-number-expr num)))
    (setf (type nexpr) *number*)
    ;;(setf (types nexpr) (available-numeric-type num))
    nexpr))

(defun make!-negation (ex)
  (make-instance 'unary-application
    'operator (make-not-operator)
    'argument ex
    'type *boolean*))
