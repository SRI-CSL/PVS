;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subst-mod-params.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec  9 13:10:41 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 14:19:51 1998
;; Update Count    : 72
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Substitutes the actuals for the formals in an expression or
;;; type-expr.  The result is an expr with all resolutions fully
;;; resolved (except possibly the current module).  If nothing changed,
;;; then the original expr is returned, otherwise a copy is made.  The
;;; actuals of modinst may have name-exprs where a type-expr is
;;; expected; the method for type-name handles this case by creating a
;;; new type-name.

;;; Ultimately only names get substituted for.  There are three ways in which
;;; this could happen:
;;;  1. The name is a formal parameter of the specified modinst.
;;;  2. The name is declared in the theory being substituted for.
;;;  3. The actuals could be substituted for.

;;; Complications come about because of declarations which point to
;;; themselves, missing module instances, 

(in-package 'pvs)


;;; This is the set of caches for subst-mod-params - it is a
;;; hash-table of hash-tables, indexed by module-instance.  This
;;; is initialized by reset-subst-mod-params-caches, and used and set by
;;; subst-mod-params.

(defvar *all-subst-mod-params-caches* nil)


;;; This is set by subst-mod-params to the cache found (or created) in
;;; looking up the modname in *all-subst-mod-params-caches*.

(defvar *subst-mod-params-cache* nil)


;;; Flag indicating whether to update the cache.  Needed because the expr
;;; slot of an actual may be substituted for even if it has no type or
;;; resolution (because it is actually a type), but we still want it
;;; printed correctly.

(defvar *smp-dont-cache* nil)


;;; This indicates whether subst-mod-params should include actuals in a
;;; name whose resolution has had actuals substituted for it.  It is let
;;; to t by make-resolution, and used in subst-mod-params* (name).

(defvar *smp-include-actuals* nil)

(defvar *smp-have-real-bindings* nil)

(defvar *free-bindings* nil)

;;; This resets the *all-subst-mod-params-caches* hash table.  It is
;;; called from the parser whenever a newly parsed theory replaces the old
;;; theory, to ensure that the garbage collector can remove the objects.

(defun reset-subst-mod-params-cache ()
  (if *all-subst-mod-params-caches*
      (clrhash *all-subst-mod-params-caches*)
      (setq *all-subst-mod-params-caches*
	    (make-hash-table :hash-function 'pvs-sxhash
				 :test 'strong-tc-eq)))
;  (if *subst-mod-params-cache*
;      (clrhash *subst-mod-params-cache*)
;      (setq *subst-mod-params-cache*
;	    (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq
;				 :size 5)))
  )

(defun remove-subst-mod-params-cache ()
  (setq *all-subst-mod-params-caches* nil)
  (setq *subst-mod-params-cache* nil))

(defun copy-subst-mod-params-cache ()
  (let ((new-hash (make-hash-table
		   :hash-function 'pvs-sxhash
		   :test 'tc-eq
		   :size (floor (hash-table-size *all-subst-mod-params-caches*)
				1.5384616))))
    (maphash #'(lambda (modname hash)
		 (setf (gethash modname new-hash)
		       (copy hash)))
	     *all-subst-mod-params-caches*)))


;;; Gets the cache associated with the given modinst, or creates a new one
;;; and returns it if necessary.

(defun get-subst-mod-params-cache (modinst)
  (unless *all-subst-mod-params-caches*
    (reset-subst-mod-params-cache))
  (let ((cache (gethash modinst *all-subst-mod-params-caches*)))
    (or cache
	(let ((ncache
	       (make-hash-table :hash-function 'pvs-sxhash
				:test 'strong-tc-eq)))
	  (setf (gethash modinst *all-subst-mod-params-caches*) ncache)
	  ncache))))


;;; The main entry point to subst-mod-params.

(defun subst-mod-params (obj modinst)
  (with-slots (actuals) modinst
    (assert *current-context*)
    (assert (modname? modinst))
    (let ((formals (formals-sans-usings (get-theory modinst))))
      (if (and actuals
	       (some #'(lambda (ofp) (memq ofp formals)) (free-params obj)))
	  (let* ((*generate-tccs* 'none)
		 (*subst-mod-params-cache*
		  (get-subst-mod-params-cache modinst))
		 (formals (formals-sans-usings (get-theory modinst)))
		 (bindings (make-subst-mod-params-bindings
			    modinst formals actuals nil))
		 (nobj (subst-mod-params* obj modinst bindings)))
	    #+pvsdebug (assert (or (eq obj nobj) (not (tc-eq obj nobj))))
	    #+pvsdebug (assert (equal bindings (pairlis formals actuals)))
	    #+pvsdebug (assert (or (typep nobj 'modname)
				   (fully-instantiated? nobj)))
	    nobj)
	  obj))))

(defun adt-modinst (modinst)
  (let* ((th (get-theory modinst))
	 (dth (get-theory* (generated-by th) (library modinst))))
    (if (and dth
	     (typep dth 'datatype))
	(adt-modinst* (positive-types dth) (actuals modinst)
		      (formals-sans-usings dth) modinst)
	modinst)))

(defun adt-modinst* (postypes acts formals modinst &optional nacts)
  (if (null acts)
      (let ((actuals (nreverse nacts)))
	(if (equal actuals (actuals modinst))
	    modinst
	    (change-class (copy modinst 'actuals actuals)
			  'datatype-modname)))
      (adt-modinst* postypes (cdr acts) (cdr formals) modinst
		    (cons (if (member (car formals) postypes
				      :test #'same-id
				      :key #'(lambda (x) (or (print-type x) x)))
			      (supertype-actual (car acts))
			      (car acts))
			  nacts))))

(defun supertype-actual (act)
  (let* ((aty (type-value act))
	 (ty (find-supertype aty)))
    (if (eq ty aty)
	act
	(mk-actual ty))))

;;; Create the formals to actuals bindings.  This would simply be a call
;;; to pairlis, but formal subtypes have an associated predicate that must
;;; be substituted for as well.

(defun make-subst-mod-params-bindings (modinst formals actuals bindings)
  (if (null formals)
      (nreverse bindings)
      (let ((pred-binding (make-subst-mod-params-pred-binding
			   modinst (car formals) (car actuals) bindings)))
	(make-subst-mod-params-bindings
	 modinst
	 (cdr formals)
	 (cdr actuals)
	 (let ((nbindings (acons (car formals) (car actuals) bindings)))
	   (if pred-binding
	       (cons pred-binding nbindings)
	       nbindings))))))

(defmethod make-subst-mod-params-pred-binding (modinst (formal formal-subtype-decl)
					       actual bindings)
  (let* ((subtype (type-value actual))
	 (sformal (when (typep (type-expr formal) 'type-name)
		    (cdr (assoc (declaration (type-expr formal)) bindings)))))
    (if sformal
	(cons (find-if #'(lambda (c) (typep c 'const-decl))
		(generated formal))
	      (make-instance 'actual
		'expr (subtype-pred subtype (type-value sformal))))
	(cons (find-if #'(lambda (c) (typep c 'const-decl))
		(generated formal))
	      (make-instance 'actual
		'expr (subtype-pred subtype
				    (subst-mod-params* 
				     (supertype (type-value formal))
				     modinst bindings)))))))

(defmethod make-subst-mod-params-pred-binding (modinst formal actual bindings)
  (declare (ignore modinst formal actual bindings))
  nil)


(defvar *caching-subst-mod-params?* t)

;;; This is the around method, which gets the expression from the cache,
;;; if possible, and otherwise calls the next method and adds the result
;;; to the cache.  In general, we try not to copy, so we check whether the
;;; hashed object is tc-eq to the original object, so we can return the
;;; original object.  Unfortunately, it is possible for an uninstantiated
;;; expression to be tc-eq to its instantiated form, so we also add a call
;;; to fully-instantiated? - we should try to fix this.  This can happen
;;; through projection applications and field applications, where tc-eq
;;; only cares about the argument type.

(defmethod subst-mod-params* :around (obj modinst bindings)
  (declare (type hash-table *subst-mod-params-cache*))
  (let ((hobj (gethash obj *subst-mod-params-cache*)))
    (or hobj
	(let ((nobj (if (fully-instantiated? obj)
			obj
			(call-next-method))))
	  (when (and (typep obj 'type-expr)
		     (typep nobj 'type-expr))
	    (when (print-type obj)
	      (let ((pte (subst-mod-params* (print-type obj)
					    modinst bindings)))
		(setq nobj (lcopy nobj 'print-type pte))))
	    (when (from-conversion obj)
	      (let ((fconv (subst-mod-params* (from-conversion obj)
					    modinst bindings)))
		(setq nobj (lcopy nobj 'from-conversion fconv)))))
	  (when (or (eq obj nobj)
		    (and (null (freevars nobj))
			 (fully-instantiated? nobj)))
	    (setf (gethash obj *subst-mod-params-cache*) nobj))
	  #+pvsdebug
	  (assert (every #'(lambda (fv)
			     (or (binding? fv)
				 (member fv (freevars obj)
					 :test #'same-declaration)
				 (member fv (freevars modinst)
					 :test #'same-declaration)))
			 (freevars nobj)))
	  nobj))))


(defmethod subst-mod-params* :around ((obj expr) modinst bindings)
  (declare (ignore modinst bindings))
  (with-slots (free-parameters) obj
    (cond (free-parameters
	   (let ((nobj (call-next-method)))
	     nobj))
	  (t #+pvsdebug (let ((nobj (call-next-method)))
			  (assert (eq nobj obj)))
	     obj))))

(defmethod subst-mod-params* ((mn modname) modinst bindings)
  (with-slots (id actuals) mn
    (if (eq id (id modinst))
	modinst
	(let ((nacts (subst-mod-params* actuals modinst bindings)))
	  (lcopy mn 'actuals nacts)))))


;;; Type Expressions

(defmethod subst-mod-params* ((list list) modinst bindings)
  (let ((nlist (subst-mod-params-list list modinst bindings)))
    (if (equal list nlist)
	list
	nlist)))

(defun subst-mod-params-list (list modinst bindings &optional rlist)
  (if (null list)
      (nreverse rlist)
      (let* ((nelt (subst-mod-params* (car list) modinst bindings)))
	(subst-mod-params-list (cdr list) modinst bindings
			       (cons nelt rlist)))))

(defmethod subst-mod-params* ((type type-name) modinst bindings)
  (let* ((res (car (resolutions type)))
	 (decl (declaration res))
	 (act (cdr (assq decl bindings))))
    #+pvsdebug (assert (or (null act) (fully-instantiated? (type-value act))))
    (if act
	(type-value act) ;; sufficient since we know
	;; type name was found on the bindings, and the corresponding
        ;; actual is there.  Here we actally do the substitution.
	(let* ((mi (module-instance res))
	       (nacts (subst-mod-params* (actuals mi) modinst bindings)))
	  #+pvsdebug (assert (not (and nacts (eq (id mi) (id modinst)))))
	  #+pvsdebug (assert (fully-instantiated? nacts))
	  (if nacts
	      (if (eq (actuals mi) nacts)
		  type
		  (let ((nmodinst (copy mi 'actuals nacts)))
		    (subst-mod-params-type-name type nmodinst)))
	      (if (eq (id mi) (id modinst))
		  (subst-mod-params-type-name type modinst)
		  type))))))

;;; just goes ahead and copies the type with the new module
;;; instance in the newly created resolution
(defun subst-mod-params-type-name (type modinst)
  (let* ((res (car (resolutions type)))
	 (nres (subst-mod-params-res res modinst))
	 (ntype (copy type
		  'resolutions (list nres)
		  'actuals (actuals (module-instance nres))
		  'print-type nil)))
    (setf (type nres) ntype)
    (adt-expand-positive-subtypes ntype)))

(defun adt-expand-positive-subtypes (type)
  (if (fully-instantiated? type)
      (gensubst type
	#'adt-expand-positive-subtypes!
	#'adt-expand-positive-subtypes?)
      type))

(defmethod adt-expand-positive-subtypes? ((ex type-name))
  #+lucid (restore-adt ex)
  (and (adt ex)
       (positive-types (adt ex))
       (not (every #'null (positive-types (adt ex))))))

(defmethod adt-expand-positive-subtypes? (ex)
  (declare (ignore ex))
  nil)

(defmethod adt-expand-positive-subtypes! ((type type-name))
  (let ((ntype (find-supertype type)))
    (if (eq ntype type)
	type
	(let* ((bd (make-new-bind-decl ntype))
	       (var (make-variable-expr bd))
	       (preds (make!-conjunction*
		       (adt-compatible-preds ntype type var nil)))
	       (stype (make-instance 'datatype-subtype
			'supertype ntype
			'predicate (make-lambda-expr (list bd) preds)
			'declared-type type)))
	  (setf (print-type stype) type)
	  stype))))

;;; just goes ahead and creates new resolution with the
;;; specified module instance
(defun subst-mod-params-res (res modinst)
  (with-slots (declaration module-instance) res
    (mk-resolution declaration modinst nil)))


(defmethod subst-mod-params* ((type type-application) modinst bindings)
  (let ((ntype (subst-mod-params* (type type) modinst bindings))
	(nparms (subst-mod-params* (parameters type) modinst bindings)))
    (lcopy type 'type ntype 'parameters nparms)))

(defmethod subst-mod-params* ((type dep-binding) modinst bindings)
  (let ((ntype (subst-mod-params* (type type) modinst bindings))
	(ndeclared-type (subst-mod-params* (declared-type type)
					   modinst bindings)))
    (if (and (eq (type type) ntype)
	     (eq (declared-type type) ndeclared-type))
	type
	(let ((ndep (copy type
		       'type ntype
		       'declared-type ndeclared-type)))
 	  (setf (resolutions ndep)
 		(list (mk-resolution ndep (current-theory-name) ntype)))
	  ndep))))

(defmethod subst-mod-params* ((type expr-as-type) modinst bindings)
  (let* ((ntype (call-next-method))
	 (nexpr (subst-mod-params* (expr ntype) modinst bindings)))
    (lcopy ntype 'expr nexpr)))

(defmethod subst-mod-params* ((type datatype-subtype) modinst bindings)
  (lcopy (call-next-method)
    'declared-type (subst-mod-params* (declared-type type) modinst bindings)))

(defmethod subst-mod-params* ((type subtype) modinst bindings)
  (let ((act (cdr (assoc type bindings
			 :test #'formal-subtype-binding-match))))
    (if act
	(type-value act)
	(let ((stype (subst-mod-params* (supertype type) modinst bindings))
	      (spred (subst-mod-params* (predicate type) modinst bindings)))
	  (lcopy type
	    'supertype stype
	    'predicate (pseudo-normalize spred))))))

(defmethod formal-subtype-binding-match (subtype (formal formal-subtype-decl))
  (tc-eq subtype (type-value formal)))

(defmethod formal-subtype-binding-match (subtype formal)
  (declare (ignore subtype formal))
  nil)

(defmethod subst-mod-params* ((type funtype) modinst bindings)
  (with-slots ((dom domain) (ran range)) type
    (let* ((ftypes (list dom ran))
	   (ntypes (subst-mod-params-type-list ftypes modinst bindings)))
      (if (equal ftypes ntypes)
	  type
	  (copy type
	    'domain (car ntypes)
	    'range (cadr ntypes))))))

(defmethod subst-mod-params* ((type tupletype) modinst bindings)
  (let ((ntypes (subst-mod-params-type-list (types type) modinst bindings)))
    (if (equal ntypes (types type))
	type
	(copy type 'types ntypes))))

(defun subst-mod-params-type-list (types modinst bindings &optional ntypes)
  (if types
      (let ((ntype (subst-mod-params* (car types) modinst bindings)))
	(subst-mod-params-type-list
	 (if (typep ntype 'dep-binding)
	     (substit (cdr types) (acons (car types) ntype nil))
	     (cdr types))
	 modinst bindings (cons ntype ntypes)))
      (nreverse ntypes)))

(defmethod subst-mod-params* ((type recordtype) modinst bindings)
  (with-slots (fields) type
    (let ((nfields (subst-mod-params-fields fields modinst bindings)))
      (if (equal nfields fields)
	  type
	  (let ((ntype (copy type
			 'fields (sort-fields nfields
					      (dependent-fields? nfields)))))
	    ntype)))))

(defun subst-mod-params-fields (fields modinst bindings &optional nfields)
  (if fields
      (let ((nfield (subst-mod-params* (car fields) modinst bindings)))
	(subst-mod-params-fields
	 (substit (cdr fields) (acons (car fields) nfield nil))
	 modinst bindings (cons nfield nfields)))
      (nreverse nfields)))

(defmethod subst-mod-params* ((field field-decl) modinst bindings)
  (with-slots (type declared-type) field
    (let ((ntype (subst-mod-params* type modinst bindings)))
      (if (eq ntype type)
	  field
	  (let ((ndt (subst-mod-params* declared-type modinst bindings)))
	    (lcopy field 'type ntype 'declared-type ndt))))))


;;; Expressions

(defmethod subst-mod-params* ((expr name-expr) modinst bindings)
  (declare (ignore modinst))
  (let* ((decl (declaration expr))
	 (act (cdr (assq decl bindings))))
    (if act
	(expr act)
	(let ((nexpr (call-next-method)))
	  (if (eq nexpr expr)
	      expr
	      (let ((ntype (type (resolution nexpr))))
		(lcopy nexpr 'type ntype)))))))

(defmethod subst-mod-params* ((expr adt-name-expr) modinst bindings)
  (let ((nexpr (call-next-method)))
    (cond ((eq expr nexpr)
	   expr)
	  (t (setf (adt-type nexpr)
		   (subst-mod-params* (adt-type expr) modinst bindings))
	     nexpr))))

(defmethod subst-mod-params* ((expr constructor-name-expr) modinst bindings)
  (declare (ignore modinst bindings))
  (let ((nexpr (call-next-method)))
    (if (eq nexpr expr)
	expr
	(lcopy nexpr
	  'recognizer-name nil
	  'accessor-names 'unbound))))

(defmethod subst-mod-params* ((expr recognizer-name-expr) modinst bindings)
  (declare (ignore modinst bindings))
  (let ((nexpr (call-next-method)))
    (if (eq nexpr expr)
	expr
	(lcopy nexpr
	  'constructor-name nil
	  'unit? 'unbound))))

(defmethod subst-mod-params* ((bd binding) modinst bindings)
  (let ((ntype (subst-mod-params* (type bd) modinst bindings))
	(ndeclared-type (subst-mod-params* (declared-type bd)
					   modinst bindings)))
    (if (and (eq (type bd) ntype)
	     (eq (declared-type bd) ndeclared-type))
	bd
	(let ((nbd (copy bd
		     'type ntype
		     'declared-type ndeclared-type)))
	  (setf (resolutions nbd)
		(list (mk-resolution nbd (current-theory-name) ntype)))
	  nbd))))

(defmethod subst-mod-params* ((expr projection-application) modinst bindings)
  (with-slots (argument type) expr
    (let ((narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
	    #+pvsdebug (assert (fully-instantiated? ntype))
	    (lcopy expr 'argument narg 'type ntype))))

(defmethod subst-mod-params* ((expr field-name-expr) modinst bindings)
  (with-slots (type resolutions) expr
    (let* ((decl (declaration (car resolutions)))
	   (fld (cdr (assq decl bindings))))
      (if fld
	  (copy expr
	    'type (type fld)
	    'resolutions (or (resolutions fld)
			     (list (mk-resolution fld
				     (current-theory-name) (type fld)))))
	  (let ((ntype (subst-mod-params* type modinst bindings)))
	    #+pvsdebug (assert (fully-instantiated? ntype))
	    (if (eq type ntype)
		expr
		(let* ((fields (fields (domain (find-supertype ntype))))
		       (nfld (car (member decl fields :test #'same-id)))
		       (nres (mk-resolution nfld
			       (current-theory-name) (type nfld))))
		  (copy expr
		    'type ntype
		    'resolutions (list nres)))))))))

(defmethod subst-mod-params* ((expr field-application) modinst bindings)
  (with-slots (argument type) expr
    (let ((narg (subst-mod-params* argument modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
	    #+pvsdebug (assert (fully-instantiated? ntype))
	    (lcopy expr 'argument narg 'type ntype))))

(defmethod subst-mod-params* ((expr number-expr) modinst bindings)
  (declare (ignore modinst bindings))
  expr)

(defmethod subst-mod-params* ((expr record-expr) modinst bindings)
  (let ((ass (subst-mod-params* (assignments expr) modinst bindings))
	(type (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr 'assignments ass 'type type)))

(defmethod subst-mod-params* ((expr tuple-expr) modinst bindings)
  (let ((nexprs (subst-mod-params* (exprs expr) modinst bindings))
	(ntype (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr 'exprs nexprs 'type ntype)))

(defmethod subst-mod-params* ((expr cases-expr) modinst bindings)
  (let ((nexpr (subst-mod-params* (expression expr) modinst bindings))
	(nsels (subst-mod-params* (selections expr) modinst bindings))
	(nelse (subst-mod-params* (else-part expr) modinst bindings))
	(type (subst-mod-params* (type expr) modinst bindings)))
    (lcopy expr
      'expression nexpr
      'selections nsels
      'else-part nelse
      'type type)))

(defmethod subst-mod-params* ((expr application) modinst bindings)
  (with-slots (operator argument) expr
    (let* ((op (subst-mod-params* operator modinst bindings))
	   (arg (subst-mod-params* argument modinst bindings)))
      (if (and (eq op operator)
	       (eq arg argument))
	  expr
	  (let* ((optype (find-supertype (type op)))
		 (rtype (if (and (not (eq arg argument))
				 (typep (domain optype) 'dep-binding))
			    (substit (range optype)
			      (acons (domain optype) arg nil))
			    (range optype)))
		 (nex (lcopy expr 'operator op 'argument arg 'type rtype)))
	    ;; Note: the copy :around (application) method takes care of
	    ;; changing the class if it is needed.
	    nex)))))

(defmethod subst-mod-params* :around ((expr table-expr) modinst bindings)
  (let ((nexpr (call-next-method)))
    (if (eq expr nexpr)
	expr
	(lcopy nexpr
	  'row-expr (subst-mod-params* (row-expr nexpr) modinst bindings)
	  'col-expr (subst-mod-params* (col-expr nexpr) modinst bindings)
	  'row-headings (subst-mod-params* (row-headings nexpr)
					   modinst bindings)
	  'col-headings (subst-mod-params* (col-headings nexpr)
					   modinst bindings)
	  'table-entries (subst-mod-params* (table-entries nexpr)
					    modinst bindings)))))

(defmethod subst-mod-params* ((sym symbol) modinst bindings)
  (declare (ignore modinst bindings))
  sym)

(defmethod subst-mod-params* ((expr binding-expr) modinst bindings)
  (with-slots ((ebindings bindings) expression type) expr
    (let* ((nebindings (subst-mod-params-bindings ebindings modinst bindings))
	   (nbindings (if (equal nebindings ebindings)
			  ebindings
			  nebindings))
	   (alist (unless (eq nbindings ebindings)
		    (pairlis ebindings nbindings)))
	   (nexpr (subst-mod-params* (if alist
					 (substit expression alist)
					 expression)
				     modinst bindings))
	   (ntype (subst-mod-params* type modinst bindings)))
      (lcopy expr
	'bindings nbindings
	'expression nexpr
	'type ntype))))

(defun subst-mod-params-bindings (ebindings modinst bindings
					    &optional nbindings)
  (if ebindings
      (let ((nbinding (subst-mod-params* (car ebindings) modinst bindings)))
	(subst-mod-params-bindings
	 (if (eq (car ebindings) nbinding)
	     (cdr ebindings)
	     (substit (cdr ebindings) (acons (car ebindings) nbinding nil)))
	 modinst bindings (cons nbinding nbindings)))
      (nreverse nbindings)))

(defmethod subst-mod-params* ((expr update-expr) modinst bindings)
  (with-slots (expression assignments type) expr
    (let ((nexpr (subst-mod-params* expression modinst bindings))
	  (ass (subst-mod-params* assignments modinst bindings))
	  (ntype (subst-mod-params* type modinst bindings)))
      (lcopy expr 'expression nexpr 'assignments ass 'type ntype))))

(defmethod subst-mod-params* ((ass assignment) modinst bindings)
  (with-slots (arguments expression) ass
    (let ((args (subst-mod-params* arguments modinst bindings))
	  (expr (subst-mod-params* expression modinst bindings)))
      (lcopy ass 'arguments args 'expression expr))))

(defmethod subst-mod-params* ((expr field-assignment-arg) modinst bindings)
  (declare (ignore modinst bindings))
  expr)

(defmethod subst-mod-params* ((sel selection) modinst bindings)
  (with-slots (constructor expression args) sel
    (let* ((name (subst-mod-params* constructor modinst bindings))
	   (nargs (subst-mod-params* args modinst bindings))
	   (alist (unless (eq nargs args)
		    (pairlis args nargs)))
	   (nexpr (subst-mod-params* (if alist
					 (substit expression alist)
					 expression)
				     modinst bindings)))
      (lcopy sel
	'constructor name
	'args nargs
	'expression nexpr))))

;;; subst-mod-params* for a name works as follows, where A and B are
;;; sequences of actuals a_1,... and b_1,...; f_i is the ith formal
;;; parameter of m.
;;;
;;; smp(f_i, m[A])    = a_i
;;; smp(m[]!x, m[A])  = m[A]!x
;;; smp(n[B]!x, m[A]) = n[smp(B, m[A])]!x

(defmethod subst-mod-params* ((name name) modinst bindings)
  (let* ((res (car (resolutions name)))
	 (nres (subst-mod-params* res modinst bindings)))
    (if (eq nres res)
	name
	(copy name
	  'actuals (when (or *smp-include-actuals*
			     (actuals name))
		     (mapcar #'copy (actuals (module-instance nres))))
	  'resolutions (list nres)))))

(defmethod subst-mod-params* ((act actual) modinst bindings)
  (with-slots (expr type-value) act
    (if (and (typep expr 'name)
	     (assq (declaration expr) bindings))
	(let ((nact (cdr (assq (declaration expr) bindings))))
	  (if (or (and type-value (type-value nact))
		  (and (null type-value) (null (type-value nact))))
	      nact
	      (lcopy nact
		'type-value (subst-mod-params* type-value modinst bindings))))
	(let ((ntype (when type-value
		       (subst-mod-params* type-value modinst bindings))))
	  (lcopy act
	    'expr (if ntype
		      (if (eq ntype type-value)
			  expr
			  (or (print-type ntype) ntype))
		      (pseudo-normalize
		       (subst-mod-params* expr modinst bindings)))
	    'type-value ntype)))))


;;; Checks whether all actuals are formal parameters (of the current theory)

(defun actuals-are-formals? (actuals)
  (or (null actuals)
      (let ((ex (actual-value (car actuals))))
	(and (typep ex 'name)
	     (typep (declaration ex) 'formal-decl)
	     #+pvsdebug (null (assert (memq (declaration ex)
					    (formals (current-theory)))))
	     ;; If the first one is, the rest should also be.
	     #+pvsdebug (actuals-are-formals? (cdr actuals))))))


;;; For resolutions, first check whether there are any actuals; if not,
;;; and the module-instance matches modinst, then we can simply use
;;; modinst in a newly created resolution.  Subst-declaration ensures
;;; that the local bindings are handled properly.  

(defmethod subst-mod-params* ((res resolution) modinst bindings)
  (with-slots ((decl declaration) (mi module-instance) type) res
    (let ((acts (actuals mi)))
      #+pvsdebug (assert (not (assq decl bindings)))
      #+pvsdebug (assert mi)
      (cond ((tc-eq mi modinst)
	     res)
	    ((and (eq (id mi) (id modinst))
		  (or (null acts)
		      (actuals-are-formals? acts)))
	     (let ((ntype (subst-mod-params* type modinst bindings)))
	       (mk-resolution decl modinst ntype)))
	    (t (let* ((nacts (subst-mod-params* acts modinst bindings)))
		 (if (eq nacts acts)
		     res
		     (mk-resolution decl
		       (mk-modname (id mi)
			 (if (memq (id mi) '(|equalities| |notequal|))
			     (list (mk-actual (find-supertype
					       (type-value (car nacts)))))
			     nacts)
			 (library modinst))
		       (subst-mod-params* type modinst bindings)))))))))

(defmethod make-resolution (decl modinst &optional type)
  (assert (modname? modinst))
  (let* ((*smp-include-actuals* t)
	 (rtype (if type
		    (subst-mod-params type modinst)
		    (typecase decl
		      (type-decl
		       (let ((*free-bindings*
			      (append (apply #'append (formals decl))
				      *free-bindings*)))
			 (subst-mod-params (type-value decl) modinst)))
		      ((or expname typed-declaration simple-decl)
		       (subst-mod-params (type decl) modinst))))))
    (mk-resolution decl modinst rtype)))

(defmethod make-resolution ((decl bind-decl) modinst &optional type)
  (assert (or modinst type))
  (mk-resolution decl
    (or modinst (current-theory-name))
    (if (and type modinst (not (eq modinst (current-theory-name))))
	(subst-mod-params type modinst)
	type)))

(defun theories-of-param-alist (alist)
  (let ((theories nil))
    (dolist (elt alist)
      (pushnew (module (car elt)) theories :test #'tc-eq))
    theories))

(defun theory-insts-of-param-alist (alist)
  (mapcar #'(lambda (th)
	      (mk-modname (id th)
		(mapcar #'(lambda (fm)
			    (let ((ex (cdr (assq fm alist))))
			      (assert ex)
			      (mk-actual ex)))
		  (formals-sans-usings th))))
    (theories-of-param-alist alist)))

(defun subst-mod-params-instances (ex theory-instances)
  (if (null theory-instances)
      ex
      (subst-mod-params-instances
       (subst-mod-params ex (car theory-instances))
       (cdr theory-instances))))

(defun subst-mod-params-alist (ex alist)
  (subst-mod-params-instances ex (theory-insts-of-param-alist alist)))
