;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check-for-tccs.lisp -- This checks for TCCs when everything has already
;;                        been typechecked.  Called by set-type.
;; Author          : Sam Owre
;; Created On      : Fri Oct 14 02:00:04 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Jul 20 18:36:08 1998
;; Update Count    : 7
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

(defvar *skip-tcc-check-exprs* nil)

;;; check-for-tccs is called by set-type when the type is already set, but
;;; TCCs need to be checked for.  Thus it is effectively called from the
;;; prover.  This is similar to what typecheck and set-type do, but
;;; theoretically should be simpler since the types and resolutions are
;;; already known.

(defun check-for-tccs (expr expected &optional skip-exprs)
  (unless (compatible? (type expr) expected)
    (type-incompatible expr (list (type expr)) expected))
      (let ((*no-conversions-allowed* t)
	    (*skip-tcc-check-exprs* skip-exprs))
	(check-for-tccs* expr expected)))

(defvar *inner-check-for-tccs* nil)

(defmethod check-for-tccs* :around (ex expected)
   (declare (ignore ex expected))
   (if (eq *generate-tccs* 'top)
       (unless *inner-check-for-tccs*
	 (let ((*inner-check-for-tccs* t))
	   (call-next-method)))
       (call-next-method)))

(defmethod check-for-tccs* :around ((ex expr) expected)
   (call-next-method)
   (unless (or (typep ex '(or branch lambda-expr))
	       (memq ex *skip-tcc-check-exprs*))
     (check-for-subtype-tcc ex expected)))

(defmethod check-for-tccs* ((ex name-expr) expected)
  (declare (ignore expected))
  (check-type-actuals ex))

(defun check-type-actuals (expr)
  (let* ((modinst (module-instance expr))
	 (acts (actuals modinst)))
    (when acts
      (check-type-actuals* acts (formals-sans-usings (get-theory modinst)))
      (generate-assuming-tccs modinst expr)
      (generate-actuals-tccs (actuals expr) acts))))

(defun check-type-actuals* (actuals formals &optional alist)
  (when actuals
    (multiple-value-bind (nform nalist)
	(subst-actuals-in-next-formal (car actuals) (car formals) alist)
      (check-type-actual (car actuals) nform)
      (check-type-actuals* (cdr actuals) (cdr formals) nalist))))

(defmethod check-type-actual (act (formal formal-type-decl))
  (check-for-tccs* (type-value act) nil))

(defmethod check-type-actual (act (formal formal-subtype-decl))
  (call-next-method)
  (let* ((tact (type-value act))
	 (texp (type-value formal))
	 (vid (make-new-variable '|x| act))
	 (vb (make!-bind-decl vid tact))
	 (svar (mk-name-expr vid nil nil
			     (make-resolution vb nil tact))))
    (check-for-subtype-tcc svar (supertype texp))))

(defmethod check-type-actual (act (formal formal-const-decl))
  (check-for-tccs* (expr act) (type formal)))


(defmethod check-for-tccs* ((expr number-expr) expected)
  (declare (ignore expected))
  nil)

(defmethod check-for-tccs* ((expr tuple-expr) expected)
  (let* ((stype (find-supertype expected))
	 (types (types stype))
	 (exprs (exprs expr)))
    (check-tup-types exprs types)))

(defun check-tup-types (exprs expected)
  (when exprs
    (let ((type (if (dep-binding? (car expected))
		    (type (car expected))
		    (car expected))))
      (check-for-tccs* (car exprs) type)
      (check-tup-types (cdr exprs)
		       (if (dep-binding? (car expected))
			   (let* ((binding (car expected))
				  (nexpr (substit (cdr expected)
					   (acons binding (car exprs) nil))))
			     nexpr)
			   (cdr expected))))))


(defmethod check-for-tccs* ((expr cases-expr) expected)
  (check-for-tccs* (expression expr) (type (expression expr)))
  (let* ((atype (find-supertype (type (expression expr))))
	 (adt (adt atype)))
    (dolist (s (selections expr))
      (check-for-tcc-selection s expr expected))
    (if (else-part expr)
	(let ((*tcc-conditions* (push-tcc-conditions
				 (make-else-cases-conditions expr)
				 *tcc-conditions*)))
	  (check-for-tccs* (else-part expr) expected))
	(generate-selections-tccs expr (constructors adt) adt))))

(defun check-for-tcc-selection (s expr expected)
  (let* ((equality (make-selection-equality s expr))
	 (*bound-variables* (append (args s) *bound-variables*))
	 (*tcc-conditions* (push-tcc-condition equality *tcc-conditions*)))
    (check-for-tccs* (expression s) expected)))

(defmethod check-for-tccs* ((expr projection-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr injection-application) expected)
  (declare (ignore expected))
  (with-slots (index argument) expr
    (check-for-tccs* argument (type argument))))

(defmethod check-for-tccs* ((expr field-application) expected)
  (declare (ignore expected))
  (with-slots (id argument) expr
    (check-for-tccs* (argument expr) (type argument))))

(defmethod check-for-tccs* ((expr application) expected)
  (declare (ignore expected))
  (with-slots (operator argument type) expr
    (let ((optype (find-supertype (type operator))))
      (check-for-tccs* (argument expr) (domain optype))
      (let ((*appl-tcc-conditions*
			    (cons (appl-tcc-conditions operator argument)
				  *appl-tcc-conditions*)))
	(check-for-tccs* operator optype))))
  (check-for-recursive-tcc expr))

(defmethod check-for-tccs* ((ex conjunction) expected)
  (check-for-tccs* (args1 ex) expected)
  (let ((*tcc-conditions* (push-tcc-condition (args1 ex) *tcc-conditions*)))
    (check-for-tccs* (args2 ex) expected)))

(defmethod check-for-tccs* ((ex disjunction) expected)
  (check-for-tccs* (args1 ex) expected)
  (let ((*tcc-conditions* (push-tcc-condition (make!-negation (args1 ex))
					     *tcc-conditions*)))
    (check-for-tccs* (args2 ex) expected)))

(defmethod check-for-tccs* ((ex implication) expected)
  (check-for-tccs* (args1 ex) expected)
  (let ((*tcc-conditions* (push-tcc-condition (args1 ex) *tcc-conditions*)))
    (check-for-tccs* (args2 ex) expected)))

(defmethod check-for-tccs* ((expr branch) expected)
  (let ((econd (condition expr))
	(ethen (then-part expr))
	(eelse (else-part expr)))
    (check-for-tccs* econd *boolean*)
    (let ((*tcc-conditions* (push-tcc-condition econd *tcc-conditions*)))
      (check-for-tccs* ethen expected))
    (let ((*tcc-conditions* (push-tcc-condition (make!-negation econd)
					       *tcc-conditions*)))
      (check-for-tccs* eelse expected))))

(defmethod check-for-tccs* ((ex lambda-expr) expected)
  (let* ((sexpected (find-supertype expected))
	 (edomain (if (dep-binding? (domain sexpected))
		      (type (domain sexpected))
		      (domain sexpected)))
	 (erange (if (dep-binding? (domain sexpected))
		     (substit (range sexpected)
		       (acons (domain sexpected)
			      (let ((vars (mapcar #'make-variable-expr
					    (bindings ex))))
				(if (cdr vars)
				    (make!-tuple-expr vars)
				    (car vars)))
			      nil))
		     (range sexpected)))
	 (adomain (domain (find-supertype (type ex)))))
    (check-for-binding-expr-tccs (append (bindings ex)
					 (list (expression ex)))
				 (nconc (mapcar #'type (bindings ex))
					(list erange)))
    (unless (tc-eq adomain edomain)
      (let ((epreds (equality-predicates adomain edomain)))
	(when epreds
	  (generate-subtype-tcc ex expected (list epreds)))))
    (let ((toppreds (compatible-preds sexpected expected ex)))
      (when toppreds
	(generate-subtype-tcc ex expected toppreds)))))

(defun check-for-binding-expr-tccs (bindings expected-types)
  (if (cdr bindings)
      (let* ((dep? (typep (car expected-types) 'dep-binding))
	     (etype (if dep?
			(type (car expected-types))
			(car expected-types))))
	(check-for-tccs* (car bindings) etype)
	(let ((*bound-variables* (cons (car bindings) *bound-variables*))
	      (*tcc-conditions* (cons (car bindings) *tcc-conditions*)))
	  (check-for-binding-expr-tccs
	   (cdr bindings)
	   (if dep?
	       (substit (cdr expected-types)
		 (acons (car expected-types) (car bindings) nil))
	       (cdr expected-types)))))
      (let ((*tcc-conditions*
	     (append (car *appl-tcc-conditions*) *tcc-conditions*)))
	(check-for-tccs* (car bindings) (car expected-types)))))

(defmethod check-for-tccs* ((expr bind-decl) expected)
  (when (declared-type expr)
    (check-for-tccs* (declared-type expr) nil))
  (let ((*tcc-conditions* (cons expr *tcc-conditions*)))
    (check-for-subtype-tcc expr expected)))


(defmethod check-for-tccs* ((expr quant-expr) expected)
  (check-for-binding-expr-tccs (append (bindings expr)
				       (list (expression expr)))
			       (nconc (mapcar #'type (bindings expr))
				      (list expected))))

(defmethod check-for-tccs* ((expr record-expr) expected)
  (let* ((sexpected (find-supertype expected))
	 (ass (assignments expr))
	 (fields (fields sexpected)))
    (check-rec-assignment-types ass nil fields sexpected nil)))

(defmethod check-for-tccs* ((expr update-expr) (expected recordtype))
  (with-slots (expression assignments) expr
    (with-slots (fields) expected
      (check-for-tccs* expression (type expression))
      (let ((atype (find-supertype (type expression))))
	(check-rec-assignment-types (complete-assignments expr expected)
				    expression fields atype nil)))))

(defmethod check-for-tccs* ((expr update-expr) (expected tupletype))
  (with-slots (expression assignments) expr
    (with-slots ((etypes types)) expected
      (check-for-tccs* expression (type expression))
      (check-tup-assignment-types (complete-assignments expr expected)
				  expression etypes expected))))

(defun check-rec-assignment-types (assns expr fields rectype nfields)
  (when assns
    (let ((ass (find-if #'(lambda (a) (eq (id (caar (arguments a)))
					  (id (car fields))))
		 assns)))
      (when ass
	(check-assignment-types ass expr rectype))
      (let* ((dep? (and (dependent? rectype)
			(some #'(lambda (fld)
				  (member (car fields) (freevars fld)
					  :key #'declaration))
			      fields)))
	     (aexpr (when dep?
		      (make-assignment-subst-expr
		       ass (type (car fields)) expr)))
	     (subst-fields (if dep?
			       (subst-rec-dep-type aexpr (car fields)
						   (cdr fields))
			       (cdr fields)))
	     (rem-assns (remove ass assns))
	     (done-with-field?
	      (not (member (car fields) rem-assns
			   :test #'same-id
			   :key #'(lambda (a) (caar (arguments a))))))
	     (next-fields (if done-with-field?
			      (cons (car fields) nfields)
			      nfields)))
	(check-rec-assignment-types
	 rem-assns
	 expr
	 (if done-with-field? subst-fields (cons (car fields) subst-fields))
	 (if dep?
	     (make-instance 'recordtype
	       'fields (sort-fields (append (reverse next-fields)
					    subst-fields)
				    t)
	       'dependent? t)
	     rectype)
	 next-fields)))))

(defun check-tup-assignment-types (assns expr types tuptype)
  (when assns
    (let* ((ass (car assns))
	   (type (nth (1- (number (caar (arguments ass)))) types)))
      (check-assignment-types ass expr tuptype)
      (let* ((dep? (typep type 'dep-binding))
	     (aexpr (when dep?
		      (make-assignment-subst-expr ass (type type) expr)))
	     (subst-types (if dep?
			     (subst-tup-dep-type aexpr type types)
			     types)))
	(check-tup-assignment-types
	 (cdr assns)
	 expr
	 subst-types
	 (if dep?
	     (make-instance 'tupletype
	       'types subst-types)
	     tuptype))))))

(defmethod check-for-tccs* ((expr update-expr) (expected funtype))
  (with-slots (expression assignments) expr
    (check-for-tccs* expression (type expression))
    (check-assignment-types-for-funtype
     assignments expected expression (type expr))))

(defun check-assignment-types-for-funtype (assignments expected expression
						       utype)
  (check-assignment-types (car assignments) expression expected)
  (when (cdr assignments)
    (check-assignment-types-for-funtype
     (cdr assignments)
     expected
     (if (typep expression 'update-expr)
	 (copy expression
	   'assignments (append (assignments expression)
				(list (car assignments))))
	 (make-instance 'update-expr
	   'expression expression
	   'assignments (list (car assignments))
	   'type utype))
     utype)))

(defmethod check-for-tccs* ((expr update-expr) (expected subtype))
  (let ((stype (find-supertype expected)))
    (check-for-tccs* expr stype)))

(defmethod check-for-tccs* ((expr update-expr) (expected dep-binding))
  (check-for-tccs* expr (type expected)))

(defmethod check-for-tccs* ((ass assignment) expected)
  (with-slots (arguments expression) ass
    (assert (typep expected '(or funtype recordtype tupletype)))
    (check-assignment-types ass expression expected)))

(defun check-assignment-types (assignment expr expected)
  (assert assignment)
  (assert expected)
  (let ((args (arguments assignment)))
    (check-assignment-types*
     args
     (expression assignment)
     expected
     (maplet? assignment)
     (when (some-dependent-arg-domain-type args expected) expr))))

(defmethod check-assignment-types* ((args null) expr expected maplet? oexpr)
  (declare (ignore maplet? oexpr))
  (check-for-tccs* expr expected))

(defmethod check-assignment-types* (args expr (expected funtype) maplet? oexpr)
  (with-slots (domain range) expected
    (let* ((dtypes (domain-types expected))
	   (exprs (cond ((length= dtypes (car args))
			 (car args))
			((cdr dtypes)
			 (get-arguments-list (caar args)))
			(t (exprs (caar args))))))
      (check-tup-types exprs dtypes)
      (let* ((nrange (if (typep domain 'dep-binding)
			 (let ((arg (if (and (eq exprs (car args))
					     (cdr (car args)))
					(make-tuple-expr (car args))
					(caar args))))
			   (substit range
			     (acons domain arg nil)))
			 range))
	     (srange (find-supertype nrange)))
	(if (and oexpr
		 (typep srange '(or recordtype tupletype))
		 (dependent? srange))
	    (let* ((nexpr (apply #'make!-application oexpr (car args)))
		   (nupdate (make!-update-expr
			     nexpr
			     (list (make-instance 'assignment
				     'arguments (cdr args)
				     'expression expr))))
		   (nass (complete-assignments nupdate
					       (find-supertype nrange))))
	      (if (typep srange 'recordtype)
		  (check-rec-assignment-types nass nexpr (fields srange)
					    (type nexpr) nil)
		  (check-tup-assignment-types nass nexpr (types srange)
					    (type nexpr))))
	    (check-assignment-types*
	     (cdr args)
	     expr
	     nrange
	     maplet?
	     (when oexpr
	       (apply #'make!-application oexpr (car args)))))))))

(defmethod check-assignment-types* (args expr (expected recordtype)
					 maplet? oexpr)
  (let ((field (find-if #'(lambda (fld) (eq (id (caar args)) (id fld)))
		 (fields expected))))
    (assert field)
    (let* ((dep? (and oexpr
		      (freevars field)
		      (some #'(lambda (fd)
				(member fd (freevars field)
					:key #'declaration))
			    (fields expected))))
	   (ftype (if dep?
		      (field-application-type field expected oexpr)
		      (type field))))
      (check-assignment-types*
       (cdr args) expr ftype maplet?
       (when dep?
	 (make!-field-application field oexpr))))))

(defmethod check-assignment-types* (args expr (expected tupletype)
					 maplet? oexpr)
  (check-assignment-types*
   (cdr args)
   expr
   (nth (1- (number (caar args))) (types expected))
   maplet?
   (when oexpr
     (make!-projection-application (number (caar args)) oexpr))))

(defmethod check-assignment-types* (args expr (expected dep-binding)
					 maplet? oexpr)
  (check-assignment-types* args expr (type expected) maplet? oexpr))

(defmethod check-assignment-types* (args expr (expected subtype)
					 maplet? oexpr)
  (check-assignment-types* args expr (find-supertype expected) maplet? oexpr))


;;; Check-types for type expressions

(defmethod check-for-tccs* ((te type-name) expected)
  (declare (ignore expected))
  (when (actuals (module-instance te))
    (check-type-actuals te)))

(defmethod check-for-tccs* ((te type-application) expected)
  (declare (ignore expected))
  (check-for-tccs* (type te) nil))

(defmethod check-for-tccs* ((te subtype) expected)
  (when (supertype te)
    (check-for-tccs* (supertype te) expected))
  (when (predicate te)
    (check-for-tccs* (predicate te) (type (predicate te)))))

(defmethod check-for-tccs* ((te funtype) expected)
  (with-slots (domain range) te
    (check-for-tccs* domain expected)
    (check-for-tccs* range expected)))

(defmethod check-for-tccs* ((te dep-binding) expected)
  (check-for-tccs* (type te) expected))

(defmethod check-for-tccs* ((te tupletype) expected)
  (mapc #'(lambda (ty) (check-for-tccs* ty expected)) (types te)))

(defmethod check-for-tccs* ((te cotupletype) expected)
  (mapc #'(lambda (ty) (check-for-tccs* ty expected)) (types te)))

(defmethod check-for-tccs* ((te recordtype) expected)
  (mapc #'(lambda (fd) (check-for-tccs* (declared-type fd) expected))
	(fields te)))
