;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; substit.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Oct 27 00:15:26 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 16:54:32 1998
;; Update Count    : 6
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :pvs)

(defun substit (obj alist)
  ;; At some point, should verify that car of every element of the
  ;; alist is a declaration.
  (assert (every #'(lambda (a)
		     (and (typep (car a)
				 '(or simple-decl declaration))
			  (eq (declaration (car a)) (car a))))
		 alist))
  (if (null alist)
      obj
      (substit* obj alist)))

;(defmethod substit* :around (obj alist)
;  (let ((hobj (gethash obj *substit-hash*)))
;    (if hobj
;	(if (tc-eq obj hobj)
;	    obj
;	    hobj))
;    (let ((nobj (call-next-method)))
;      (setf (gethash obj *substit-hash*) nobj)
;      nobj)))

(defmethod substit* :around ((expr T) alist)
  (cond ((null (freevars expr));;NSH(2.21.96)
                            ;;removed (rassoc expr alist :test #'eq)
	 expr)
	((type-expr? expr)
	 (if *subst-type-hash*
	     (let ((result (lookup-subst-hash expr alist *subst-type-hash*)))
	       (if result result
		   (let ((result (call-next-method)))
		     (install-subst-hash expr alist result *subst-type-hash*)
		     result)))
	     (call-next-method)))
	(t ;;(when (funtype? expr) (break "substit"))
	   (call-next-method))))

;(defmethod substit* :around (expr alist)
;  (if (null (freevars expr)) ;; (substit-possible? expr alist)
;      expr
;      (call-next-method)))

(defun substit-possible? (expr alist)
  (substit-possible*? (freevars expr) alist))

(defun substit-possible*? (freevars alist)
  (when freevars
    (or (assq (declaration (car freevars)) alist)
	(substit-possible*? (cdr freevars) alist))))

(defmethod substit* ((expr name-expr) alist)
  (with-slots (resolutions type actuals) expr
    (let* ((decl (declaration (car resolutions)))
	   (binding (assq decl alist)))
      (cond ((null binding)
	     (let ((res (substit* resolutions alist))
		   (ntype (substit* type alist)))
	       (if (eq res resolutions)
		   (unless (eq ntype type)
		     (setq res (list (copy (car res) 'type ntype))))
		   (setf (type (car res)) ntype))
	       (lcopy expr
		 'type ntype
		 'actuals (substit* actuals alist)
		 'resolutions res)))
	    ((typep (cdr binding) 'binding)
	     (if (eq (car binding)(cdr binding))
		 expr
		 (let ((nex (if (typep (cdr binding) 'field-decl)
				(change-class (copy (cdr binding))
					      'field-name-expr)
				(change-class (copy (cdr binding))
					      'name-expr))))
		   (setf (parens nex) 0)
		   (setf (kind nex) 'VARIABLE)
		   (setf (resolutions nex)
			 (list (make-resolution (cdr binding)
				 (mod-name *current-context*)
				 (type (cdr binding)))))
		   nex)))
	    (t (cdr binding))))))

(defmethod substit* ((expr adt-name-expr) alist)
  (let ((nex (call-next-method)))
    (if (eq nex expr)
	nex
	(copy nex 'adt-type (substit* (adt-type expr) alist)))))

(defmethod substit* ((expr projection-application) alist)
  (with-slots (argument type index) expr
    (let ((narg (substit* argument alist)))
      (if (eq argument narg)
	  expr
	  (let ((ntype (projection-type* (types (find-supertype (type narg)))
					 index 1 narg (type narg))))
	    (lcopy expr
	      'argument narg
	      'type ntype))))))

(defmethod substit* ((expr field-application) alist)
  (with-slots (argument type) expr
    (let ((narg (substit* argument alist)))
      (if (eq argument narg)
	  expr
	  (let ((ntype (substit* type alist)))
	    (copy expr 'argument narg 'type ntype))))))

(defmethod substit* ((expr resolution) alist)
  (lcopy expr
    'module-instance (substit* (module-instance expr) alist)))

(defmethod substit* ((expr modname) alist)
  (lcopy expr 'actuals (substit* (actuals expr) alist)))


(defmethod substit* ((act actual) alist)
  (with-slots (expr type-value) act
    (let ((ntype (when type-value
		   (substit* type-value alist))))
      (lcopy act
	'expr (cond ((and ntype (eq ntype type-value))
		     expr)
		    ((and ntype (typep expr 'application))
		     (lcopy expr
		       'argument (substit* (argument expr) alist)))
		    (type-value (substit* expr alist))
		    (t (pseudo-normalize (substit* expr alist))))
	'type-value ntype))))

;(defmethod substit* ( (expr if-expr) alist) ;;NSH(7-30)get rid
;  (copy expr
;	'arguments
;	(substit* (arguments expr) alist)))
;    'condition (substit*  (condition expr) alist)
;    'then-part (substit* (then-part expr) alist)
;    'else-part (substit*  (else-part expr) alist)))


(defmethod substit* ((expr application) alist)
  (with-slots (operator argument) expr
    (let ((op (substit* operator alist))
	  (arg (substit* argument alist)))
      (if (and (eq op operator)
	       (eq arg argument))
	  expr
	  (let* ((stype (find-supertype (type op)))
		 (nex (lcopy expr
			'operator op
			'argument arg
			'type (if (typep (domain stype) 'dep-binding)
				  (substit (range stype)
				    (acons (domain stype) arg alist))
				  (range stype)))))
	    (unless (eq op operator)
	      (change-application-class-if-necessary expr nex))
	    nex)))))

(defmethod change-to-infix-appl? ((expr infix-application))
  nil)

(defmethod change-to-infix-appl? ((expr application))
  (with-slots (operator argument) expr
    (and (infix-op? operator)
	 (= (the fixnum (arg-length argument)) 2))))

(defmethod infix-op? (expr)
  (declare (ignore expr))
  nil)

(defmethod infix-op? ((expr name-expr))
  (with-slots (id) expr
    (memq id *infix-operators*)))

(defmethod arg-length (expr)
  (declare (ignore expr))
  1)

(defmethod arg-length ((expr tuple-expr))
  (with-slots (exprs) expr
    (length exprs)))

(defmethod substit* :around ((expr table-expr) alist)
  (let ((nexpr (call-next-method)))
    (if (eq expr nexpr)
	expr
	(lcopy nexpr
	  'row-expr (substit* (row-expr nexpr) alist)
	  'col-expr (substit* (col-expr nexpr) alist)
	  'row-headings (substit* (row-headings nexpr) alist)
	  'col-headings (substit* (col-headings nexpr) alist)
	  'table-entries (substit* (table-entries nexpr) alist)))))

(defmethod substit* ((expr record-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr tuple-expr) alist)
  (let* ((nexprs (substit* (exprs expr) alist))
	 (ntype (if (every #'(lambda (nex ex) (eq (type nex) (type ex)))
			   nexprs (exprs expr))
		    (type expr)
		    (mk-tupletype (mapcar #'type nexprs)))))
    (lcopy expr
      'exprs nexprs
      'type ntype)))

(defmethod substit* ((expr update-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'expression (substit* (expression expr) alist)
      'assignments (substit* (assignments expr) alist)
      'type ntype)))

(defmethod substit* ((expr assignment) alist)
  (lcopy expr
    'arguments (substit* (arguments expr) alist)
    'expression (substit* (expression expr) alist)))

(defmethod substit* ((expr list) alist)
  (let ((nlist (substit*-list expr alist nil)))
    (if (equal nlist expr) expr nlist)))

(defun substit*-list (expr alist result)
  (cond ((null expr)
	 (nreverse result))
	((typep (car expr) 'binding)
	 (let* ((newtype (substit* (type (car expr)) alist))
		(newcar (lcopy (car expr)
			  'type newtype
			  'declared-type
			  (substit* (declared-type (car expr))
				    alist))))
	   (cond ((eq newcar (car expr))
		  (substit*-list (cdr expr)
				 alist
				 (cons newcar result)))
		 (t (when (typep (car expr) '(or bind-decl dep-binding))
		      (setf (resolutions newcar)
			    (list (make-resolution newcar
				    (substit* (module-instance (car expr)) alist)
				    newtype))))
		    (substit*-list (cdr expr)
				   (cons (cons (car expr) newcar) alist)
				   (cons newcar result))))))
	(t (substit*-list (cdr expr) alist
			  (cons (substit* (car expr) alist)
				result)))))

(defmethod substit* ((expr binding-expr) alist)
  (if (not (substit-possible? expr alist))
      expr
      (let* ((new-bindings (make-new-bindings (bindings expr) alist))
	     (nalist (nconc (pairlis (bindings expr) new-bindings) alist))
	     (ntype (substit* (type expr) nalist))
	     (*bound-variables* (nconc (reverse new-bindings)
				       *bound-variables*)))
	(copy expr
	  'bindings new-bindings
	  'type ntype
	  'expression (substit* (expression expr) nalist)))))

(defun make-new-bindings (old-bindings alist)
  (make-new-bindings* old-bindings (alist-freevars alist) alist))

(defun alist-freevars (alist)
  (delete-duplicates (mapappend #'alist-freevars* alist)))

(defun alist-freevars* (alist-pair)
  (freevars (cdr alist-pair)))

(defun add-alist-freevars (expr alist-freevars)
  (add-alist-freevars* (freevars expr) alist-freevars))

(defun add-alist-freevars* (freevars alist-freevars)
  (if (null freevars)
      alist-freevars
      (add-alist-freevars*
       (cdr freevars)
       (if (memq (car freevars) alist-freevars)
	   alist-freevars
	   (cons (declaration (car freevars)) alist-freevars)))))

;;freevars must be the free variables in alist.

(defun make-new-bindings* (old-bindings freevars alist &optional nbindings)
  (if (null old-bindings)
      (nreverse nbindings)
      (let* ((bind (car old-bindings))
	     (res (resolution bind))
	     (btype (type bind))
	     (check (memq bind freevars))
	     (dec-type (or (declared-type bind)
			   (subst-mod-params
			    (declared-type (declaration res))
			    (declared-type-module-instance
			     (module-instance res)))
			   btype))
	     (new-binding
	      (if (not check)
		  (lcopy bind
		    'type (substit* btype alist)
		    'declared-type (substit* dec-type alist))
		  (copy bind
		    'id  (new-boundvar-id (id bind))
		    'type (substit* btype alist)
		    'declared-type (substit* dec-type alist)))))
	(unless res
	  (break "No resolution for binding"))
	(when (not (eq bind new-binding))
	  (setf (resolutions new-binding)
		(list (make-resolution new-binding
			(mod-name *current-context*)
			(type new-binding)))))
	(make-new-bindings*
	 (cdr old-bindings)
	 (add-alist-freevars new-binding alist)
	 (acons bind new-binding alist)
	 (cons new-binding nbindings)))))



(defmethod substit* ((expr cases-expr) alist)
  (let ((ntype (substit* (type expr) alist)))
    (lcopy expr
      'expression (substit* (expression expr) alist)
      'selections (mapcar #'(lambda (s) (substit* s alist))
			  (selections expr))
      'else-part (substit* (else-part expr) alist)
      'type ntype)))

(defmethod substit* ((expr selection) alist)
  (let ((new-bindings (make-new-bindings (args expr) alist)))
    (lcopy expr
      'constructor (constructor expr)
      'args new-bindings
      'expression (substit* (expression expr)
			    (nconc (pairlis (args expr)
					    new-bindings)
				   alist)))))

;(defmethod substit* ((expr coercion) alist)
;  (lcopy expr
;    'expression (substit* (expression expr) alist)
;    'declared-type (substit* (declared-type expr) alist)
;    'type (substit* (type expr) alist)))
  


;;2-21-91: remaining exprs are treated as unsubstitutable.
(defmethod substit* ((expr expr) alist)
  (declare (ignore alist))
  expr)

;;NSH:8-21-91: substit* for type-expressions.
(defmethod substit* ((texpr type-name) alist)
  (lcopy texpr
    'actuals (substit* (actuals texpr) alist)
    'resolutions (substit* (resolutions texpr) alist)
    'print-type (substit* (print-type texpr) alist)))


(defmethod substit* ((texpr subtype) alist)
  (with-slots (supertype predicate print-type) texpr
    (let ((npred (substit* predicate alist)))
      (if (eq npred predicate)
	  texpr
	  (let* ((spred (pseudo-normalize npred))
		 (stype (domain (find-supertype (type spred)))))
	    (copy texpr
	      'supertype stype
	      'predicate spred
	      'print-type (print-type stype)))))))

(defmethod substit* ((texpr setsubtype) alist)
  (let ((nexpr (call-next-method)))
    (unless (or (not (typep nexpr 'setsubtype))
		(eq (predicate texpr) (predicate nexpr)))
      ;;(assert (lambda-expr? (predicate nexpr)))
      (setf (formula nexpr) (expression (predicate nexpr))))
    nexpr))

(defmethod substit* ((texpr expr-as-type) alist)
  (let* ((npred (substit* (predicate texpr) alist))
	 (spred (if (eq npred (predicate texpr))
		    npred
		    (pseudo-normalize npred))))
    (lcopy texpr
      'supertype (substit* (supertype texpr) alist)
      'predicate spred
      'expr (substit* (expr texpr) alist)
      'print-type (substit* (print-type texpr) alist))))

(defmethod substit* ((texpr datatype-subtype) alist)
  (lcopy (call-next-method)
    'declared-type (substit* (declared-type texpr) alist)))

(defmethod substit* ((texpr funtype) alist)
  (let* ((typelist (list (domain texpr) (range texpr)))
	 (ntypelist (substit* typelist alist)))
    (lcopy texpr
      'domain (car ntypelist)
      'range (cadr ntypelist)
      'print-type (substit* (print-type texpr) alist))))

(defmethod substit* ((texpr tupletype) alist)
  (lcopy texpr
    'types (substit* (types texpr) alist)
    'print-type (substit* (print-type texpr) alist)))

(defmethod substit* ((te recordtype) alist)
  (let ((fields (substit* (fields te) alist)))
    (lcopy te
      'fields (sort-fields fields (dependent-fields? fields))
      'print-type (substit* (print-type te) alist))))

(defmethod substit* ((te type-application) alist)
  (lcopy te
    'type (substit* (type te) alist)
    'parameters (pseudo-normalize (substit* (parameters te) alist))
    'print-type (substit* (print-type te) alist)))

(defmethod substit* ((fd field-decl) alist)
  (let ((ntype (substit* (type fd) alist)))
    (if (eq (type fd) ntype)
	fd
	(copy fd 'type ntype 'declared-type ntype))))

(defmethod substit* ((db dep-binding) alist)
  (let ((ntype (substit* (type db) alist)))
	(lcopy db 'type ntype)))

(defmethod substit* ((sym symbol) alist)
  sym)

(defun pseudo-normalize* (expr)
  (gensubst expr #'pseudo-normalize! #'pseudo-normalize?))

(defmethod pseudo-normalize? (ex)
  (declare (ignore ex))
  nil)

(defmethod pseudo-normalize? ((ty subtype))
  t)

(defmethod pseudo-normalize? ((act actual))
  (not (type-value act)))

(defmethod pseudo-normalize! ((ty subtype))
  (lcopy ty 'predicate (pseudo-normalize (predicate ty))))

(defmethod pseudo-normalize! ((act actual))
  (lcopy act 'expr (pseudo-normalize (expr act))))
