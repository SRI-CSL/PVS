;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcdecls.lisp -- 
;; Author          : Sam Owre
;; Created On      : Mon Oct 18 22:45:21 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Jul 13 04:20:03 1994
;; Update Count    : 88
;; Status          : Beta testing
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Typecheck* methods for declarations - all of these methods have a
;;; declaration instance and a context for input, and they all check for
;;; duplication, set the type or type-value of the decl as appropriate,
;;; and return the decl.  Any other behavior is specific to the decl
;;; class, and described with the corresponding method below.


(in-package 'pvs)

(defun typecheck-decls (decls)
  (when decls
    (let ((decl (car decls)))
      (setf (declaration *current-context*) decl)
      (typecase decl
	(declaration (typecheck-decl decl))
	(using (tcdebug "~%    Processing using")
	       (let ((*generating-adt* nil))
		 (typecheck-usings (modules decl))))
	(datatype (unless (typechecked? decl)
		    (tcdebug "~%    Processing Datatype ~a" (id decl))
		    (unwind-protect
			(typecheck* decl nil nil nil)
		      (cleanup-datatype decl)))))
      (typecheck-decls (cdr decls)))))

(defun typecheck-decl (decl)
  (if (and (typechecked? decl)
	   (not (typep decl '(or mod-decl conversion-decl judgement))))
      (mapc #'(lambda (d) (add-decl d nil))
	    (generated decl))
      (unwind-protect
	  (progn
	    (assert (typep (module *current-context*)
			   '(or module datatype)))
	    (setf (module decl) (module *current-context*))
	    (tcdebug "~%    Typechecking ~a" decl)
	    (let ((stime (get-internal-run-time)))
	      (typecheck* decl nil nil nil)
	      (setf (typecheck-time decl)
		    (- (get-internal-run-time) stime)))
	    (setf (typechecked? decl) t))
	(unless (or *loading-prelude*
		    (typechecked? decl))
	  (cleanup-typecheck-decls decl))))
  (unless (and (type-def-decl? decl)
	       (enumtype? (type-expr decl)))
    (put-decl decl (local-decls *current-context*))))

(defun cleanup-datatype (adt)
  (unless (typechecked? adt)
    (dolist (decl (generated adt))
      (remove-generated-decl decl))
    (setf (generated adt) nil)
    (untypecheck-theory adt)))

(defun remove-generated-decl (decl)
  (mapc #'remove-generated-decl (generated decl))
  (cond ((member decl (theory *current-theory*))
	 (setf (theory *current-theory*)
	       (remove decl (theory *current-theory*))))
	((member decl (assuming *current-theory*))
	 (setf (assuming *current-theory*)
	       (remove decl (assuming *current-theory*)))))
  (setf (gethash (id decl) (declarations *current-theory*))
	(remove decl
		(gethash (id decl) (declarations *current-theory*)))))

(defun cleanup-typecheck-decls (decl)
  (cond ((and (type-def-decl? decl)
	      (enumtype? (type-expr decl)))
	 (cleanup-datatype decl)
	 (setf (generated decl) nil))
	((generated-by decl)
	 (remove-generated-decl decl))
	((not (typechecked? decl))
	 (untypecheck-theory decl)
	 (reset-typecheck-caches))))

(defmethod typecheck* :around ((decl declaration) expected kind args)
   (declare (ignore expected kind args))
   (call-next-method)
   (setf (typechecked? decl) t)
   decl)

;;; Typechecking formal declarations - if it is a type, a type-name
;;; instance is created, otherwise the declared type is typechecked.
;;; Note that the type slot is set to the result in either case (maybe
;;; there should be a type-value slot to handle TYPE parameters).

(defmethod typecheck* ((decl formal-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (let ((tn (mk-type-name (id decl))))
    (setf (uninterpreted? tn) t)
    (setf (resolutions tn)
	  (list (make-instance 'resolution
		  'declaration decl
		  'module-instance (mod-name *current-context*)
		  'type tn)))
    (setf (type decl) tn)
    (setf (type-value decl) tn))
  decl)

(defmethod typecheck* ((decl formal-nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl (local-decls *current-context*))
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl))
  decl)

(defmethod typecheck* ((decl nonempty-type-eq-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (check-nonempty-type-of decl)
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl formal-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (let* ((tn (mk-type-name (id decl)))
	 (res (make-instance 'resolution
		'declaration decl
		'module-instance (mod-name *current-context*)
		'type tn))
	 (tval (type-def-decl-value decl tn)))
    (setf (resolutions tn) (list res))
    (setf (uninterpreted? tn) t)
    (setf (type-value decl) tval))
  decl)

(defmethod typecheck* ((decl formal-nonempty-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl (local-decls *current-context*))
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl))
  decl)


(defmethod typecheck* ((decl formal-const-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (type decl)
	(typecheck* (declared-type decl) nil nil nil))
  (set-type (declared-type decl) nil)
  (if (free-params (type decl))
      (set-nonempty-type (type decl))
      (check-nonempty-type (type decl) decl))
  (check-duplication decl)
  decl)


;;; Library Declarations

(defmethod typecheck* ((decl lib-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  ;;;(check-duplication decl)
  (unless (library-of (library decl))
    (type-error decl "Library ~a does not exist"
		(lib-string decl))))


;;; Module declarations

(defmethod typecheck* ((decl mod-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  (check-duplication decl)
  (typecheck-usings (list (modname decl)))
  ;; Need to allow id to be used as abbreviation
  )


;;; Type-decl - there are three possibilities.  If there is no
;;; type-expression, then it is an uninterpreted type; the type-value
;;; slot is set to a newly created type-name.  If there is a type-expr,
;;; then it is typechecked.  If the defined? flag is set, then the
;;; type-value is set to the result, otherwise it is an uninterpreted
;;; subtype and a new subtype type-expression is created.  For example,
;;; if the decl is "t: type from int", then the subtype created is
;;; "lambda (x:int): x intype t".  See the type-expression methods for
;;; details on typechecking the type-expr slot of decl.

(defmethod typecheck* ((decl type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl
      "Uninterpreted types may not have parameters"))
  (check-duplication decl)
  (setf (type-value decl)
	(let ((tn (mk-type-name (id decl))))
	  (setf (uninterpreted? tn) t)
	  (when *generating-adt*
	    (setf (adt tn) *generating-adt*)
	    ;;(setf (nonempty? tn) t)
	    )
	  (setf (resolutions tn)
		(list (make-instance 'resolution
			'declaration decl
			'module-instance (mod-name *current-context*)
			'type tn)))
	  tn))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (set-nonempty-type (type-value decl))
  (unless (eq (id *current-theory*) '|booleans|)
    (put-decl decl (local-decls *current-context*))
    (generate-existence-axiom decl))
  decl)

(defun generate-existence-axiom (decl)
  (let* ((id (makesym "~a_nonempty" (id decl)))
	 (type (type-value decl))
	 (var (make-new-variable '|x| type))
	 (form (mk-exists-expr (list (mk-bind-decl var type)) *true*))
	 (tform (typecheck* form *boolean* nil nil))
	 (edecl (typecheck* (mk-formula-decl id tform 'AXIOM) nil nil nil)))
    (pvs-info "Added existence AXIOM for ~a:~%~a"
      (id decl) (unparse edecl :string t))
    (add-decl edecl)))
    

(defmethod typecheck* ((decl type-def-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-type-application-formals decl)
  (check-duplication decl)
  (let* ((tn (mk-type-name (id decl)))
	 (res (make-instance 'resolution
		'declaration decl
		'module-instance (mod-name *current-context*)
		'type tn))
	 (*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (ptype (if (formals decl)
		    (make-instance 'type-application
		      'type tn
		      'parameters (mapcar #'(lambda (x)
					      (let ((name (mk-name-expr x)))
						(setf (kind name) 'VARIABLE)
						name))
					  (car (formals decl))))
		    tn))
	 (tval (type-def-decl-value decl ptype)))
    (setf (resolutions tn) (list res))
    (setf (uninterpreted? tn) t)
    (setf (type-value decl) tval))
  (typecase (type-expr decl)
    (enumtype ;;(put-decl decl (local-decls *current-context*))
	      (typecheck* (type-expr decl) nil nil nil))
    (subtype (when (typep (predicate (type-expr decl)) 'expr)
	       (put-decl decl (local-decls *current-context*)))))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defun check-type-application-formals (decl)
  (when (formals decl)
    (when (cdr (formals decl))
      (type-error (cdar (formals decl))
	"Type applications may not be curried"))
    (typecheck* (formals decl) nil nil nil)
    (mapc #'(lambda (fmlist)
	      (mapc #'(lambda (fm)
			(unless (fully-instantiated? fm)
			  (type-error  (type fm)
			    "Could not determine the full theory instance")))
		    fmlist))
	  (formals decl))))

(defmethod typecheck* ((decl nonempty-type-def-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (let ((*bound-variables* (apply #'append (formals decl))))
    (check-nonempty-type-of decl))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl nonempty-type-eq-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (let ((*bound-variables* (apply #'append (formals decl))))
    (check-nonempty-type-of decl))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl nonempty-type-from-decl) expected kind arguments)
  (call-next-method))

(defmethod check-nonempty-type-of ((decl nonempty-type-def-decl))
  (let ((ctype (copy (type-value decl) 'print-type nil)))
    (check-nonempty-type ctype decl)
    (setf (nonempty? (type-value decl)) (nonempty? ctype))))

(defmethod check-nonempty-type-of ((decl nonempty-type-from-decl))
  (check-nonempty-type (supertype (type-value decl)) decl)
  (set-nonempty-type (type-value decl))
  (put-decl decl (local-decls *current-context*))
  (generate-existence-axiom decl))

(defun type-def-decl-value (decl tn)
  (cond ((type-from-decl? decl)
	 (when (enumtype? (type-expr decl))
	   (type-error decl
	     "Enumtype must be declared at top level"))
	 (check-type-application-formals decl)
	 (let* ((*bound-variables* (apply #'append (formals decl)))
		(stype (typecheck* (type-expr decl) nil nil nil))
		(utype (generate-uninterpreted-subtype decl stype)))
	   (set-type (type-expr decl) nil)
	   (setf (print-type utype) tn)
	   utype))
	((enumtype? (type-expr decl))
	 (setf (adt tn) (type-expr decl))
	 (set-nonempty-type tn)
	 tn)
	(t (let ((tval (typecheck* (type-expr decl) nil nil nil)))
	     (set-type (type-expr decl) nil)
	     (copy tval 'print-type tn)))))


;;; Var-decl

(defmethod typecheck* ((decl var-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
  (set-type (declared-type decl) nil)
  (unless (fully-instantiated? (type decl))
    (type-error (declared-type decl)
      "Could not determine the full theory instance"))
  (check-duplication decl)
  decl)


;;; Const-decl - if the constant has a definition, then it must be
;;; typechecked, and a formula must be generated which allows the
;;; definition to be referenced in proof declarations.

(defmethod typecheck* ((decl const-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (typecheck* (formals decl) nil nil nil)
    (mapc #'(lambda (fmlist)
	      (mapc #'(lambda (fm)
			(unless (fully-instantiated? fm)
			  (type-error  (type fm)
			    "Could not determine the full theory instance")))
		    fmlist))
	  (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (rtype (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (assert (null (freevars (type decl))))
    (unless (typep decl 'adt-constructor-decl)
      (if (definition decl)
	  (set-nonempty-type (type decl))
	  (check-nonempty-type (type decl) decl)))
    (check-duplication decl)
    (when (definition decl)
      (let ((*tcc-conditions* (add-formals-to-tcc-conditions (formals decl))))
	(typecheck* (definition decl) rtype nil nil))
      #+pvsdebug (assert (fully-instantiated? (definition decl)))
      (put-decl decl (local-decls *current-context*))
      (make-def-axiom decl)))
  decl)

(defun add-formals-to-tcc-conditions (formals &optional conditions)
  (if (null formals)
      conditions
      (add-formals-to-tcc-conditions
       (cdr formals)
       (add-bindings-to-tcc-conditions (car formals) conditions))))


;;; formals are generally of the form ((d11 ... d1n)...(dm1 ... dmk)),
;;; which generates a function type of the form
;;; [t11,...,t1n -> [... -> [tm1,...,tmn -> rtype] ...]]
;;; where the tij are the types of the dij.  Of course, dependencies
;;; have to be handled as well.

;(defun make-formals-funtype (formals rtype)
;  (if (null formals)
;      rtype
;      (let ((typeslist (mapcar #'(lambda (fm) (mapcar #'type fm)) formals)))
;	(make-formals-funtype* (car formals) (cdr formals) rtype
;			       (car typeslist) (cdr typeslist)
;			       nil nil))))

(defun make-formals-funtype (formals range &optional dont-typecheck)
  (let ((*generate-tccs* 'none))
    (if (null formals)
	range
	(let ((nrange (make-formals-funtype (cdr formals) range)))
	  (make-formals-funtype* (car formals) nrange dont-typecheck)))))

(defun make-formals-funtype* (formals range dont-typecheck)
  (if (some #'(lambda (ff) (occurs-in ff range)) formals)
      (let* ((ndom (make-formals-domain formals))
	     (nvar (if (cdr formals)
		       (make-new-variable '|d| range)
		       (id (car formals))))
	     (ndep (mk-dep-binding nvar ndom))
	     (nvar (mk-name-expr nvar nil nil
				 (make-resolution ndep
				   (mod-name *current-context*) ndom)
				 'variable))
	     (nrange (subst-formals-funtype formals range ndep nvar)))
	(mk-funtype ndep nrange))
      (mk-funtype (make-formals-domain formals) range)))

(defun subst-formals-funtype (formals range ndep nvar)
  (if (cdr formals)
      (subst-formals-funtype* formals range ndep nvar)
      (substit range (acons (car formals) nvar nil))))

(defun subst-formals-funtype* (formals range ndep nvar &optional (index 1))
  (if (null formals)
      range
      (if (occurs-in (car formals) range)
	  (let* ((nproj (make-projection-application index nvar))
		 (alist (acons (car formals) nproj nil))
		 (nrange (substit range alist)))
	    (subst-formals-funtype* (cdr formals) nrange ndep nvar (1+ index)))
	  (subst-formals-funtype* (cdr formals) range ndep nvar (1+ index)))))

(defun make-formals-domain (formals)
  (if (cdr formals)
      (make-formals-domain* formals)
      (type (car formals))))

(defun make-formals-domain* (formals &optional domtypes)
  (if (null formals)
      (mk-tupletype (nreverse domtypes))
      (if (occurs-in (car formals) (cdr formals))
	  (let* ((dbinding (mk-dep-binding (id (car formals))
					   (type (car formals))
					   (declared-type (car formals))))
		 (nvar (mk-name-expr (id (car formals)) nil nil
				     (make-resolution dbinding
				       (mod-name *current-context*)
				       (type (car formals)))
				     'variable)))
	    (make-formals-domain* (substit (cdr formals)
				   (acons (car formals) nvar nil))
				 (cons dbinding domtypes)))
	  (make-formals-domain* (cdr formals)
			       (cons (type (car formals)) domtypes)))))


;;; Def-decl - this is the class for recursive definitions.  Checks that
;;; the type is a function type, typechecks the measure and checks that
;;; it is also a function and has the same domain and its range is nat.
;;; The decl is then added to the context prior to typechecking the
;;; definition, so that references to the definition can be resolved.
;;; Finally, a formula is generated for the definition.

(defmethod typecheck* ((decl def-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  (set-formals-types (apply #'append (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (rtype (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    #+pvsdebug (assert (null (freevars (type decl))))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "Recursive definition must be a function type"))
    (unless (typep decl 'adt-def-decl)
      (typecheck-measure decl))
    (set-nonempty-type rtype)
    (put-decl decl (local-decls *current-context*))
    (let ((*recursive-tcc-names* nil)
	  (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl))))
      (typecheck* (definition decl) rtype nil nil)
      (unless (typep decl 'adt-def-decl)
	(check-recursive-tcc-completeness (definition decl))))
    (make-def-axiom decl))
  decl)

(defun set-formals-types (formals)
  (when formals
    (when (declared-type (car formals))
      (set-type (declared-type (car formals)) nil))
    (let ((*bound-variables* (cons (car formals) *bound-variables*)))
      (set-formals-types (cdr formals)))))


;;; This is not quite correct; it doesn't bother to check on recursive calls
;;; in a type, e.g.
;;;  foo(n): RECURSIVE nat =
;;;     if n = 0 then 0
;;;     elsif (FORALL (m:{i: i<foo(n-1)}) foo(m) = 1) then 1
;;;     else 2
;;; This hasn't yet come up - I want to do the termination differently
;;; anyway, and will fix this then.

(defun check-recursive-tcc-completeness (expr)
  (mapobject #'(lambda (ex)
		 (or (typep ex 'type-expr)
		     (when (and (name-expr? ex)
				(eq (declaration ex)
				    (declaration *current-context*))
				(not (memq ex *recursive-tcc-names*)))
		       (type-error ex
			 "Termination TCC could not be generated"))))
	     expr))

(defun typecheck-measure (decl)
  (let ((*bound-variables* (apply #'append (formals decl)))
	(*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	(stype (find-supertype (type decl)))
	(*generate-tccs* 'none))
    (typecheck* (declared-measure decl) nil nil nil)
    (if (some #'(lambda (pty)
		  (let ((sty (find-supertype pty)))
		    (and (typep sty 'funtype)
			 (compatible? (domain sty) (domain stype)))))
	      (ptypes (declared-measure decl)))
	(setf (measure decl) (declared-measure decl))
	(let ((nmeas (mk-measure-lambda-exprs (formals decl)
					      (declared-measure decl))))
	  (typecheck* nmeas nil nil nil)
	  (setf (place nmeas) (place (declared-measure decl)))
	  (setf (measure decl) nmeas)))
    (when (ordering decl)
      (typecheck* (ordering decl) nil nil nil)
      (let ((otypes (remove-if-not #'relation-type? (ptypes (ordering decl)))))
	(if otypes
	    (setf (types (ordering decl)) otypes)
	    (type-error (ordering decl)
	      "Order must be a relation (e.g. of type [T, T -> bool])")))))
  (let* ((stype (find-supertype (type decl)))
	 (ctypes (remove-if-not
		     #'(lambda (pty)
			 (let ((sty (find-supertype pty)))
			   (and (typep sty 'funtype)
				(compatible? (domain sty) (domain stype)))))
		   (ptypes (measure decl))))
	 (ftypes (or (remove-if-not
			 #'(lambda (pty)
			     (let ((sty (find-supertype pty)))
			       (tc-eq (domain sty) (domain stype))))
		       ctypes)
		     ctypes)))
    (if ftypes
	(set-measure-types (measure decl) ftypes (type decl))
	(if (types (measure decl))
	    (type-error (declared-measure decl) "Measure must be a function.")
	    (type-error (declared-measure decl)
	      "Could not determine the type of the measure function."))))
  (let* ((*bound-variables* nil)
	 (stype (find-supertype (type decl)))
	 (mtypes (mapcar #'find-supertype (ptypes (measure decl)))))
    (if (ordering decl)
	(typecheck-measure-with-ordering decl stype (measure decl) mtypes
					 (ordering decl))
	(typecheck-measure* decl stype (measure decl) mtypes))))

(defun mk-measure-lambda-exprs (formals dmeasure)
  (let ((mformals (measure-lambda-formals (reverse formals) dmeasure)))
    (mk-lambda-exprs mformals dmeasure)))

(defun measure-lambda-formals (formals dmeasure)
  (if (or (null (cdr formals))
	  (some #'(lambda (fm) (id-occurs-in (id fm) dmeasure))
		(car formals)))
      (reverse formals)
      (measure-lambda-formals (cdr formals) dmeasure)))

(defun set-measure-types (meas types dtype)
  (let ((mtypes (get-measure-types meas types (domain dtype)))
	(mreses (when (typep meas 'name-expr)
		  (get-measure-reses meas (resolutions meas) (domain dtype)))))
    (setf (types meas) mtypes)
    (when mreses
      (setf (resolutions meas) mreses))))

(defun get-measure-types (meas types dtype &optional result)
  (if (null types)
      (if (null result)
	  (type-error meas "Could not determine the full theory instance")
	  result)
      (get-measure-types
       meas
       (cdr types)
       dtype
       (if (fully-instantiated? (car types))
	   (cons (car types) result)
	   (let ((bindings (mapcar #'(lambda (ff)
				       (list (declaration ff)))
				   (free-formals meas))))
	     (tc-match dtype (domain (car types)) bindings)
	     (if (and bindings (every #'cdr bindings))
		 (cons (subst-for-formals (car types) bindings) result)
		 result))))))

(defun get-measure-reses (meas reses dtype &optional result)
  (if (null reses)
      (if (null result)
	  (type-error meas "Could not determine the full theory instance")
	  result)
      (get-measure-reses
       meas
       (cdr reses)
       dtype
       (if (fully-instantiated? (car reses))
	   (cons (car reses) result)
	   (let ((bindings (mapcar #'(lambda (ff) (list (declaration ff)))
				   (free-formals meas))))
	     (tc-match dtype (domain (find-supertype (type (car reses))))
		       bindings)
	     (if (and bindings (every #'cdr bindings))
		 (cons (subst-for-formals (car reses) bindings) result)
		 result))))))


;;; The recursion here is to handle the situation where the ordering is a
;;; higher-order function; e.g.,
;;; foo: RECURSIVE [int -> [int -> nat]] = (LAMBDA i: (LAMBDA j: j - i))
;;;   MEASURE (LAMBDA i: f(i)) BY <<
;;; We want to go down the measure until we hit the domain type of the
;;; ordering.

(defun typecheck-measure-with-ordering (decl type meas mtypes ordering
					     &optional doms)
  (unless (funtype? type)
    (type-error (measure decl)
      "Wrong number of arguments in measure"))
  (let* ((pranges (mapcar #'(lambda (pty)
			      (let ((sty (find-supertype pty)))
				(mk-funtype* doms
					     (copy type
					       'range (car (types
							    (domain sty)))))))
			  (ptypes ordering)))
	 (ctypes (remove-if-not #'(lambda (rty)
				    (some #'(lambda (ty) (compatible? ty rty))
					  pranges))
		   mtypes)))
    (cond ((singleton? ctypes)
	   (setf (measure-depth decl) (length doms))
	   (typecheck (measure decl) :expected (car ctypes) :tccs 'all)
	   (typecheck-ordering decl))
	  ((funtype? type)
	   (let ((ptypes (remove-if-not
			     #'(lambda (mty)
				 (and (typep mty 'funtype)
				      (compatible? (domain type) (domain mty))))
			   mtypes)))
	     (if ptypes
		 (typecheck-measure-with-ordering
		  decl (range type)
		  (when (lambda-expr? meas) (expression meas)) 
		  mtypes
		  ordering
		  (cons (domain type) doms))
		 (measure-incompatible decl type meas mtypes))))
	  (t (type-error (measure decl)
	       "Measure must have range compatible with the domain of the ordering")))))

(defun typecheck-ordering (decl)
  (let* ((ordering (ordering decl))
	 (measure (measure decl))
	 (mtype (range (find-supertype (type measure)))))
    ;; The measure has already been typechecked.
    (typecheck-ordering* decl ordering mtype)))

(defun typecheck-ordering* (decl ordering mtype)
  (let ((expected (mk-funtype (list mtype mtype) *boolean*)))
    (cond ((some #'(lambda (ty) (compatible? ty expected)) (types ordering))
	   (typecheck ordering :expected expected :tccs 'all)
	   (generate-well-founded-tcc decl mtype))
	  ((typep (find-supertype mtype) 'funtype)
	   (typecheck-ordering* decl ordering (range (find-supertype mtype))))
	  (t (type-error ordering
	       "Ordering is incompatible with the measure.")))))
	


(defun typecheck-measure* (decl type meas mtypes &optional doms)
  (unless (funtype? type)
    (type-error (measure decl)
      "Wrong number of arguments in measure"))
  (let ((natrange (mk-funtype* doms (copy type 'range *naturalnumber*)))
	(ordrange (when *ordinal*
		    (mk-funtype* doms (copy type 'range *ordinal*)))))
    (cond ((some #'(lambda (ty) (compatible? ty natrange))
		 mtypes)
	   (setf (measure-depth decl) (length doms))
	   (typecheck (measure decl) :expected natrange :tccs 'all))
	  ((and *ordinal*
		(some #'(lambda (ty) (compatible? ty ordrange))
		      mtypes))
	   (setf (measure-depth decl) (length doms))
	   (typecheck (measure decl) :expected ordrange :tccs 'all))
	  ((funtype? type)
	   (let ((ptypes (remove-if-not
			     #'(lambda (mty)
				 (and (typep mty 'funtype)
				      (compatible? (domain type)
						   (nth-domain mty doms))))
			   mtypes)))
	     (if ptypes
		 (typecheck-measure*
		  decl (range type)
		  (when (lambda-expr? meas) (expression meas))
		  ptypes
		  (cons (domain type) doms))
		 (measure-incompatible decl type meas mtypes))))
	  (t (type-error (measure decl)
	       "Measure must have range a naturalnumber or an ordinal")))))

(defun nth-domain (funtype doms)
  (if (null doms)
      (domain funtype)
      (nth-domain (range funtype) (cdr doms))))

(defmethod measure-incompatible (decl type (meas lambda-expr) mtypes)
  (declare (ignore decl))
  (let ((ftypes (remove-if-not #'(lambda (mty) (typep mty 'funtype))
		  mtypes)))
    (if ftypes
	(measure-incompatible* (domain-types type)
			       (mapcar #'domain-types ftypes)
			       (bindings meas))
	(type-error (measure decl)
	  "Wrong number of arguments in measure"))))

(defun measure-incompatible* (dom mdoms bindings)
  (if (and (cdr dom)
	   (some #'(lambda (mdom)
		     (compatible? (car dom) (car mdom)))
		 mdoms))
      (measure-incompatible* (cdr dom) (mapcar #'cdr mdoms) (cdr bindings))
      (type-incompatible (car bindings)
			 (mapcar #'car mdoms)
			 (car dom))))

(defmethod measure-incompatible (decl type meas mtypes)
  (declare (ignore meas))
  (let ((ftypes (remove-if-not #'(lambda (mty) (typep mty 'funtype))
		  mtypes)))
    (if ftypes
	(type-error (measure decl)
	  "Incompatible domain types~
     ~%     Found: ~{[~{~a~^, ~}]~%~^~12T~}  Expected: [~{~a~^, ~}]"
	  (mapcar #'domain mtypes) (domain type))
	(type-error (measure decl)
	  "Wrong number of arguments in measure"))))

(defun mk-funtype* (doms range)
  (if (null doms)
      range
      (mk-funtype* (cdr doms) (mk-funtype (car doms) range))))


;;; inductive-decl

(defmethod typecheck* ((decl inductive-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  (set-formals-types (apply #'append (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (rtype (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "Inductive definition must be a function type"))
    (unless (tc-eq (range* (type decl)) *boolean*)
      (type-error decl
	"Inductive definitions must have (eventual) range type boolean"))
    (set-nonempty-type rtype)
    (put-decl decl (local-decls *current-context*))
    (typecheck* (definition decl) rtype nil nil)
    (check-inductive-occurrences decl)
    (make-def-axiom decl)
    (make-inductive-def-inductions decl))
  decl)

(defun check-inductive-occurrences (decl)
  (check-inductive-occurrences* (definition-body decl) decl 1))

;;; parity is 1 for positive context, -1 for negative, and 0 for unknown

(defmethod check-inductive-occurrences* ((ex name-expr) decl parity)
  (declare (ignore parity))
  (when (eq (declaration ex) decl)
    (type-error ex
      "Cannot determine parity of this occurrence of the inductive definition")))

(defmethod check-inductive-occurrences* ((ex application) decl parity)
  (cond
    ((or (conjunction? ex)
	 (disjunction? ex))
     (check-inductive-occurrences* (arguments ex) decl parity))
    ((implication? ex)
     (check-inductive-occurrences* (args1 ex) decl (- parity))
     (check-inductive-occurrences* (args2 ex) decl parity))
    ((negation? ex)
     (check-inductive-occurrences* (args1 ex) decl (- parity)))
    ((inductive-occurrence? ex (find-supertype (type decl)) decl)
     (cond
       ((minusp parity)
	(type-error ex
	  "Negative occurrences of inductive definitions are not allowed."))
       ((zerop parity)
	(type-error ex
	  "Cannot determine parity of this occurrence of the inductive definition"))))
    (t (check-inductive-occurrences* (operator ex) decl 0)
       (check-inductive-occurrences* (arguments ex) decl 0))))

(defmethod check-inductive-occurrences* ((ex if-expr) decl parity)
  (check-inductive-occurrences* (condition ex) decl 0)
  (check-inductive-occurrences* (then-part ex) decl parity)
  (check-inductive-occurrences* (else-part ex) decl parity))

(defmethod inductive-occurrence? ((ex application) (type funtype) decl)
  (inductive-occurrence? (operator ex) (find-supertype (range type)) decl))

(defmethod inductive-occurrence? ((ex name-expr) (type type-name) decl)
  (eq (declaration ex) decl))

(defmethod inductive-occurrence? (ex type decl)
  (declare (ignore ex type decl))
  nil)

(defmethod check-inductive-occurrences* ((ex quant-expr) decl parity)
  (check-inductive-occurrences* (expression ex) decl parity))

(defmethod check-inductive-occurrences* ((list list) decl parity)
  (when list
    (check-inductive-occurrences* (car list) decl parity)
    (check-inductive-occurrences* (cdr list) decl parity)))

(defmethod check-inductive-occurrences* (ex decl parity)
  (declare (ignore parity))
  (let ((occ (occurs-in decl ex)))
    (when occ
      (type-error occ
	"Cannot determine parity of this occurrence of the inductive definition"))))


(defmethod range* ((te subtype))
  (range* (type te)))

(defmethod range* ((te dep-binding))
  (range* (type te)))

(defmethod range* ((te funtype))
  (range* (range te)))

(defmethod range* ((te type-expr))
  te)

(defun make-inductive-def-inductions (decl)
  (let* ((pid (make-new-variable 'P (definition decl)))
	 (wbd (typecheck* (mk-bind-decl pid (type decl) (type decl))
			  nil nil nil))
	 (sbd (typecheck* (mk-bind-decl pid (type decl) (type decl))
			  nil nil nil))
	 (wvar (mk-name-expr pid nil nil
			     (make-resolution wbd nil (type decl))
			     'variable))
	 (svar (mk-name-expr pid nil nil
			     (make-resolution sbd nil (type decl))
			     'variable))
	 (fixed-vars (fixed-inductive-variables decl))
	 (body (definition-body decl))
	 (wprem (subst-pred-for-ind wvar fixed-vars body decl))
	 (sprem (add-pred-to-ind svar fixed-vars body decl))
	 (wconc (make-inductive-conclusion wvar fixed-vars decl))
	 (sconc (make-inductive-conclusion svar fixed-vars decl))
	 (wform (universal-closure (make-implication wprem wconc)))
	 (sform (universal-closure (make-implication sprem sconc)))
	 (wid (makesym "~a_weak_induction" (id decl)))
	 (sid (makesym "~a_induction" (id decl)))
	 (wdecl (typecheck* (mk-formula-decl wid wform 'AXIOM) nil nil nil))
	 (sdecl (typecheck* (mk-formula-decl sid sform 'AXIOM) nil nil nil)))
    (add-decl wdecl)
    (add-decl sdecl)))

(defmethod definition-body ((decl const-decl))
  (definition-body (definition decl)))

(defmethod definition-body ((ex lambda-expr))
  (definition-body (expression ex)))

(defmethod definition-body ((ex expr))
  ex)

(defun fixed-inductive-variables (decl)
  (let* ((vars (ind-def-formals decl))
	 (fvars (copy-list vars)))
    (mapobject #'(lambda (ex)
		   (when (inductive-occurrence?
			  ex (find-supertype (type decl)) decl)
		     (mapc #'(lambda (v a)
			       (unless (and (typep a 'name-expr)
					    (eq (declaration a) v))
				 (setq fvars (delete v fvars))))
			   vars (ind-def-arguments ex))))
	       (definition decl))
    fvars))

(defun ind-def-formals (decl)
  (apply #'append
	 (butlast (append (formals decl)
			  (ind-def-formals* (definition decl))))))

(defmethod ind-def-formals* ((ex lambda-expr))
  (append (list (bindings ex))
	  (ind-def-formals* (expression ex))))

(defmethod ind-def-formals* (ex)
  (declare (ignore ex))
  nil)

(defmethod ind-def-arguments (ex)
  (ind-def-arguments* (operator ex)))

(defmethod ind-def-arguments* ((ex application))
  (append (ind-def-arguments* (operator ex)) (arguments ex)))

(defmethod ind-def-arguments* (ex)
  (declare (ignore ex))
  nil)

(defun subst-pred-for-ind (nvar fixed-vars expr decl)
  (let ((*keep-unbound* (cons (declaration nvar) fixed-vars)))
    (universal-closure
     (make-implication
      (subst-pred-for-ind* nvar expr decl)
      (inductive-pred-appl nvar decl)))))

(defun subst-pred-for-ind* (nvar expr decl)
  (gensubst expr
    #'(lambda (ex) (declare (ignore ex)) (copy nvar))
    #'(lambda (ex) (and (typep ex 'name-expr)
			(eq (declaration ex) decl)))))

(defun add-pred-to-ind (nvar fixed-vars expr decl)
  (let ((*keep-unbound* (cons (declaration nvar) fixed-vars))
	(*generate-tccs* 'none))
    (universal-closure
     (make-implication
      (gensubst expr
	#'(lambda (ex)
	    (make-conjunction (list (copy ex)
				    (subst-pred-for-ind* nvar ex decl))))
	#'(lambda (ex)
	    (and (typep ex 'application)
		 (inductive-occurrence?
		  ex (find-supertype (type decl)) decl))))
      (inductive-pred-appl nvar decl)))))

(defun inductive-pred-appl (var decl)
  (let ((pargs (mapcar #'(lambda (blist)
			   (mapcar #'mk-name-expr blist))
		       (append (formals decl)
			       (all-lambda-bindings
				(definition decl))))))
    (labels ((make-appl (op args)
	       (if (null args)
		   op
		   (make-appl (make-application* op (car args)) (cdr args)))))
      (make-appl (copy var) pargs))))

(defun make-inductive-conclusion (var fixed-vars decl)
  (let* ((*keep-unbound* (cons (declaration var) fixed-vars))
	 (dname (mk-name-expr (id decl) nil nil
			      (make-resolution decl
				(mod-name *current-context*)
				(type decl))
			      'constant))
	 (dargs (mapcar #'(lambda (blist)
			    (mapcar #'mk-name-expr blist))
			(append (formals decl)
				(all-lambda-bindings
				 (definition decl)))))
	 (pargs (copy dargs)))
    (universal-closure
     (labels ((make-appl (op args)
	        (if (null args)
		    op
		    (make-appl (make-application* op (car args)) (cdr args)))))
       (make-implication
	(make-appl dname dargs)
	(make-appl (copy var) pargs))))))

(defmethod all-lambda-bindings (expr)
  (declare (ignore expr))
  nil)

(defmethod all-lambda-bindings ((ex lambda-expr))
  (cons (bindings ex) (all-lambda-bindings (expression ex))))


;;; Formula-decl - the expected type is always boolean

(defmethod typecheck* ((decl formula-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  (check-duplication decl)
  (let ((cdecl (declaration *current-context*)))
    (setf (declaration *current-context*) decl)
    (typecheck* (definition decl) *boolean* nil nil)
    (setf (declaration *current-context*) cdecl))
  (setf (closed-definition decl)
	(universal-closure (definition decl)))
  (handle-existence-assuming-on-formals decl)
  decl)

(defun handle-existence-assuming-on-formals (decl)
  (when (and (eq (spelling decl) 'ASSUMPTION)
	     (typep (definition decl) 'exists-expr)
	     (tc-eq (expression (definition decl)) *true*)
	     (singleton? (bindings (definition decl)))
	     (typep (type (car (bindings (definition decl)))) 'type-name)
	     (memq (declaration (type (car (bindings (definition decl)))))
		   (formals *current-theory*)))
    (set-nonempty-type (type (car (bindings (definition decl)))))))


;;;  TYPE EXPRESSIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These typecheck* methods verify that the
;;; type-expr makes sense, and return the canonical type expression;
;;; which is essentially the same type expression  with all of the type-names
;;; replaced by their definitions.  In each case, an attempt is made to
;;; return the original type-expr if it is already in canonical form.


(defmethod typecheck* ((type enumtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (id type) (id (declaration *current-context*)))
  (call-next-method))


;;; TYPE-NAME - the name is resolved to a type-decl, and its type-value
;;; is returned.  If we are processing the prelude, then call
;;; set-prelude-types to set *boolean*, etc.

(defmethod typecheck* ((type type-name) expected kind arguments)
  (declare (ignore expected kind))
  (call-next-method type nil 'type arguments)
  ;;(set-type type nil)
  (let ((tval (type (resolution type))))
    (when (and (formals (declaration (resolution type)))
	       (null arguments))
      (type-error type "Type name must have arguments"))
    (assert tval)
    (when (typep tval 'type-name)
      (setf (uninterpreted? type) (uninterpreted? tval)))
    (if (eq tval 'type)
	type
	tval)))

(defmethod typecheck* ((type type-application) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (parameters type) nil nil nil)
  (let ((te (typecheck* (type type) nil 'type (parameters type))))
    (unless (formals (declaration (type type)))
      (type-error type "~a may not be used in a type application" te))
    (unless (length= (car (formals (declaration (type type))))
		     (parameters type))
      (type-error type "Wrong number of actuals in ~a" type))
    (let ((typeslist (make-formals-type-app
		      (subst-mod-params (formals (declaration (type type)))
					(module-instance
					 (resolution (type type)))))))
      (mapc #'set-type (parameters type) (car typeslist)))
    (substit te (pairlis (car (formals (declaration (type type))))
			 (parameters type)))))

(defun make-formals-type-app (formals)
  (let ((typeslist (mapcar #'(lambda (fm)
			       (mapcar #'type fm)) formals)))
    (make-formals-type-app* (car formals) (cdr formals)
			    (car typeslist) (cdr typeslist)
			    nil nil)))

(defun make-formals-type-app* (formals formalslist types typeslist
				       tup tuples)
  (cond ((and (null formals) (null formalslist))
	 (nreverse (cons (nreverse tup) tuples)))
	((null formals)
	 (make-formals-type-app* (car formalslist) (cdr formalslist)
				 (car typeslist) (cdr typeslist)
				 nil (cons (nreverse tup) tuples)))
	(t (let* ((type (if (or (occurs-in (car formals) (cdr formals))
				(occurs-in (car formals) formalslist))
			    (mk-dep-binding (id (car formals))
					    (car types))
			    (car types)))
		  (alist (when (dep-binding? type)
			   (list (cons (car formals) type))))
		  (cdrtypes (substit (cdr types) alist))
		  (ntypeslist (substit typeslist alist)))
	     (make-formals-type-app* (cdr formals) formalslist
				     cdrtypes ntypeslist
				     (cons type tup) tuples)))))


(defmethod typecheck* ((type expr-as-type) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (expr type) nil nil nil)
  (let* ((used-a-conversion? nil)
	 (ptypes (or (remove-if-not #'(lambda (ty)
					(and (not (from-conversion ty))
					     (let ((sty (find-supertype ty)))
					       (and (funtype? sty)
						    (tc-eq (range sty)
							   *boolean*)))))
		       (types (expr type)))
		     (prog1 (look-for-expected-from-conversion (expr type))
		       (setq used-a-conversion? t))))
	 (ftypes (remove-if-not #'fully-instantiated? ptypes))
	 (types (if (typep (expr type) 'name-expr)
		    (let ((nreses (filter-local-resolutions
				   (remove-if-not #'(lambda (r)
						      (some #'(lambda (fty)
								(tc-eq fty (type r)))
							    ftypes))
				     (resolutions (expr type))))))
		      (or (mapcar #'type nreses)
			  ftypes))
		    ftypes)))
    (unless (or (type (expr type))
		(singleton? types))
      (cond ((cdr types)
	     (type-ambiguity (expr type)))
	    (ptypes
	     (type-error type
	       "Could not determine the full theory instance"))
	    (t (type-error (expr type)
		 "Could not determine the predicate for this type: ~a"
		 type))))
    (unless used-a-conversion?
      (setf (types (expr type)) types))
    (let ((ftype (find-supertype (or (type (expr type))
				     (car types)))))
      (unless (and (funtype? ftype)
		   (tc-eq (range ftype) *boolean*))
	(type-error (expr type) "Does not resolve to a predicate"))
      (set-type (expr type) ftype)
      (let ((tval (mk-subtype (domain ftype)
		    (expr type))))
	(setf (print-type tval) (lcopy type 'contains nil))
	(when (contains type)
	  (typecheck* (contains type) tval nil nil)
	  (set-nonempty-type type)
	  (set-nonempty-type tval))
	tval))))

(defmethod look-for-expected-from-conversion ((expr application))
  (and (typep (operator expr) 'name-expr)
       (typep (resolution (operator expr)) 'lambda-conversion-resolution)
       (list (range (type (find-if #'(lambda (x) (not (null x)))
			    (conversion (resolution (operator expr)))))))))

(defmethod look-for-expected-from-conversion (expr)
  nil)


;;; SUBTYPE - typecheck* the supertype and the predicate, and return a
;;; subtype type-expr; either the original if typechecking the supertype
;;; returns the same, or a newly built one if it doesn't.  Note that the
;;; expected type for the predicate is canonical.  This is only for
;;; interpreted subtypes; uninterpreted subtypes are handled in
;;; typecheck* for type-decls.

(defmethod typecheck* ((type subtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (cond ((predicate type)
	 (unless (type (predicate type))
	   (typecheck* (predicate type) nil nil nil))
	 (let ((types (or (types (predicate type))
			  (list (type (predicate type))))))
	   (cond ((cdr types)
		  (type-ambiguity (predicate type)))
		 ((typep (find-supertype (car types)) 'funtype)
		  (let ((etype (mk-funtype (list (domain (find-supertype
							  (car types))))
					   *boolean*)))
		    (unless (type (predicate type))
		      (set-type (predicate type) etype))))
		 (t (type-error type "Predicate expected here"))))
	 (let* ((stype (domain (find-supertype (type (predicate type)))))
		(tval (mk-subtype stype (pseudo-normalize (predicate type)))))
	   (setf (supertype type) stype)
	   (when (contains type)
	     (typecheck* (contains type) tval nil nil)
	     (set-nonempty-type type)
	     (set-nonempty-type tval))
	   tval))
	(t (let ((stype (typecheck* (supertype type) nil nil nil)))
	     (if (named-type-expr? type)
		 (let ((tval (generate-uninterpreted-subtype
			      (declaration *current-context*)
			      stype)))
		   tval)
		 (type-error type
		   "Uninterpreted subtypes are only allowed at the top level")
		 )))))

(defmethod typecheck* ((type setsubtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((stype (typecheck* (supertype type) nil nil nil))
	 (bindings (if (consp (formals type))
		       (formals type)
		       (list (formals type)))))
    (if (supertype type)
	(if (singleton? bindings)
	    (setf (type (car bindings)) stype
		  ;;(declared-type (car bindings)) (supertype type)
		  (resolutions (car bindings))
		  (list (make-resolution (car bindings)
			  (mod-name *current-context*)
			  stype))
		  ;;(types (car bindings)) (list stype)
		  )
	    (if (tupletype? (find-supertype stype))
		(if (length= bindings (types (find-supertype stype)))
		    (mapcar #'(lambda (b ty)
				(setf (type b) ty
				      (resolutions b)
				      (list (make-resolution b
					      (mod-name *current-context*)
					      ty))))
			    bindings (types (find-supertype stype)))
		    (type-error type "Wrong number of variables"))))
	(progn
	  (typecheck* bindings nil nil nil)
	  (setf (supertype type)
		(if (singleton? bindings)
		    (type (car bindings))
		    (mk-tupletype (mapcar #'type bindings))))))
    (setf (predicate type)
	  (let ((expr (make-instance 'lambda-expr
			'bindings bindings
			'expression (formula type)))
		(*bound-variables* (append bindings *bound-variables*)))
	    (typecheck* expr (mk-funtype (mapcar #'type (bindings expr))
					 (copy *boolean*))
			nil nil)))
    (let ((tval (if (and stype
			 (eq stype (supertype type)))
		    type
		    (mk-subtype (or stype (supertype type))
		      (predicate type)))))
      (when (contains type)
	(typecheck* (contains type) tval nil nil)
	(set-nonempty-type type)
	(set-nonempty-type tval))
      tval)))


;;; Checks whether the subtype is the current declaration.

(defun named-type-expr? (type)
  (and (typep (declaration *current-context*) 'type-decl)
       (eq type (type-expr (declaration *current-context*)))))


;;; Generates a new predicate and a new subtype based on that predicate.
;;; Given the type-decl "T: type from T1", the predicate generated is of
;;; the form "T_pred: [T1->bool]" and the subtype is of the form
;;; "from T1 with T_pred".

(defun generate-uninterpreted-subtype (decl stype)
  (let* ((pname (makesym "~a_pred" (id decl)))
	 (pexpr (mk-name-expr pname))
	 (ftype (typecheck* (mk-funtype (list stype) *boolean*) nil nil nil))
	 (type (make-formals-funtype (formals decl) ftype))
	 (cdecl (declaration *current-context*))
	 (ndecl (typecheck* (mk-const-decl pname ftype nil
					   (mapcar #'(lambda (ff)
						       (mapcar #'copy ff))
					     (formals decl)))
			    nil nil nil)))
    (setf (declaration *current-context*) cdecl)
    (add-decl ndecl (not (formal-subtype-decl? decl)))
    (typecheck* pexpr type nil nil)
    (mk-subtype (make-formals-funtype (formals decl) stype) pexpr)))

;(defun generate-uninterpreted-subtype (decl tval)
;  (let* ((pname (makesym "~a_pred" (id decl)))
;	 (pexpr (mk-name-expr pname))
;	 (type (typecheck* (mk-funtype (list (find-supertype tval))
;				       *boolean*)
;			   nil nil nil))
;	 (cdecl (declaration *current-context*))
;	 (ndecl (typecheck* (mk-const-decl pname type) nil nil nil)))
;    (setf (declaration *current-context*) cdecl)
;    (add-decl ndecl (not (formal-subtype-decl? decl)))
;    (assert (typep (module ndecl) '(or module datatype)))
;    (typecheck* pexpr type nil nil)
;    (mk-subtype tval pexpr)))


;;; Tuple Types - in the following, the f_i are optional; any that are
;;; missing will not be used in typechecking the following types.

;;;  C  |-  T1 type,  ... ,  C,f_1:T1,...,f_n-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [ f_1:T1, ... f_n-1:Tn, Tn ] type

(defmethod typecheck* ((type tupletype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((types (typecheck-tuples (types type) nil))
	 (tuptype (if (equal types (types type))
		      type
		      (mk-tupletype types))))
    ;;(create-projections types tuptype 1)
    tuptype))

(defun create-projections (types tuptype num)
  (when types
    (let ((decl (mk-proj-decl (makesym "PROJ_~d" num)
		  (mk-funtype (list tuptype)
			      (if (dep-binding? (car types))
				  (type (car types))
				  (car types))))))
      (add-decl decl))
    (create-projections (cdr types) tuptype (1+ num))))

(defun typecheck-tuples (types result)
  (if (null types)
      (nreverse result)
      (let* ((ty (car types))
	     (ntype (typecheck* ty nil nil nil))
	     (*bound-variables* (if (dep-binding? ntype)
				    (cons ntype *bound-variables*)
				    *bound-variables*)))
	(typecheck-tuples (cdr types) (cons ntype result)))))


;;;  C  |-  T1 type,  ... ,  C,f1:T1,...,fn-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [# f1:T1, ... , fn:Tn #] type

(defmethod typecheck* ((type recordtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (cond ((every #'type (fields type))
	 type)
	(t (when (duplicates? (fields type) :test #'same-id)
	     (type-error type "Duplicate field names not allowed"))
	   (typecheck-fields (fields type))
	   (let* ((dependent? (dependent-fields? (fields type)))
		  (sorted-fields (sort-fields (fields type) dependent?))
		  (ntype (mk-recordtype sorted-fields dependent?)))
	     (setf (dependent? type) dependent?)
	     (tcdebug "~%~6tTypechecking Recordtype:~%~8t~a" type)
	     ntype))))

(defun sort-fields (fields &optional dependent?)
  (if dependent?
      (sort-dependent-fields fields nil)
      (sort (copy-list fields) #'string-lessp :key #'id)))

(defun sort-dependent-fields (fields sorted-fields)
  (if (null fields)
      (nreverse sorted-fields)
      (let ((nfield (next-best-field fields)))
	(sort-dependent-fields (remove nfield fields)
			       (cons nfield sorted-fields)))))

(defun next-best-field (fields)
  (let ((candidates (remove-if #'(lambda (fd)
				   (dependent-on-some? fd fields))
		      fields)))
    (lexical-min-field candidates)))

(defun lexical-min-field (fields &optional min)
  (cond ((null fields)
	 min)
	((null min)
	 (lexical-min-field (cdr fields) (car fields)))
	(t (lexical-min-field (cdr fields)
			      (if (string-lessp (id (car fields)) (id min))
				  (car fields)
				  min)))))
      

(defun dependent-on-some? (field fields)
  (and fields
       (or (and (not (eq field (car fields)))
		(dependent-on? field (car fields)))
	   (dependent-on-some? field (cdr fields)))))

(defun dependent-on? (fld1 fld2)
  (member fld2 (freevars (type fld1)) :test #'same-declaration))
	  

(defun dependent-fields? (fields)
  (when (cdr fields)
    (or (let ((dep? nil))
	  (mapobject #'(lambda (ex)
			 (or dep?
			     (when (and (name? ex)
					(eq (declaration ex) (car fields)))
			       (setq dep? t)
			       t)))
		     (cdr fields))
	  dep?)
	(dependent-fields? (cdr fields)))))

;;; The tricky part here is that a field has two identities:
;;;  1. Within a dependent record type, it appears as a simple name-expr,
;;;     whose type is the declared type of the field.
;;;  2. Outside the recordtype, references to the field should expect a
;;;     function type whose domain is the recordtype and range is the
;;;     declared-type.

(defun typecheck-fields (fields)
  (when fields
    (typecheck* (car fields) nil nil nil)
    (let* ((*bound-variables* (cons (car fields) *bound-variables*)))
      (typecheck-fields (cdr fields)))))


;;; Funtype - three rules are actually used here.  As in the tupletype
;;; case, the f_i are optional for the first two rules.

;;;  C  |-  T1 type,  ... ,  C,f1:T1,...,fn-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [ f1:T1, ...  -> Tn #] type
;;;          C  |-  [ [f1:T1, ... ] -> Tn #] type
;;;          C  |-  [ [# f1:T1, ... #] -> Tn #] type

(defmethod typecheck* ((type funtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((dom (typecheck* (domain type) nil nil nil))
	 (*bound-variables* (if (typep (domain type) 'dep-binding)
				(cons (domain type) *bound-variables*)
				*bound-variables*))
	 (rng (typecheck* (range type) nil nil nil)))
    (if (and (eq dom (domain type))
	     (eq rng (range type)))
	type
	(let ((tval (mk-funtype dom rng)))
	  tval))))

(defun get-funtype-dependencies (funtype types)
  (if (cdr types)
      (remove-if-not #'dep-binding? types)
      (typecase (domain funtype)
	(recordtype (fields (car types)))
	(tupletype (mapcar #'(lambda (db)
			       (mk-bind-decl (id db) (declared-type db)))
			   (remove-if-not #'dep-binding? (types (car types)))))
	(t (remove-if-not #'dep-binding? types)))))

(defmethod typecheck* ((decl field-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((type (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (setf (type decl) type))
  decl)


(defmethod typecheck* ((db dep-binding) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (type db) (typecheck* (declared-type db) nil nil nil))
  (set-type (declared-type db) nil)
  db)


;;; Judgements

(defmethod typecheck* ((decl subtype-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (subtype decl) (typecheck* (declared-subtype decl) nil nil nil))
  (set-type (declared-subtype decl) nil)
  (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
  (set-type (declared-type decl) nil)
  (when (subtype-of? (subtype decl) (type decl))
    (type-error (declared-subtype decl)
      "Type is already known to be a subtype"))
  (let* ((bd (make-new-bind-decl (subtype decl)))
	 (bvar (make-variable-expr bd)))
    (setf (place bvar) (place (declared-subtype decl)))
    (unless (compatible? (subtype decl) (type decl))
      (type-error decl
	"Types must be compatible."))
    (let* ((incs (compatible-preds (subtype decl) (type decl) bvar)))
      (cond (incs
	     (generate-subtype-tcc bvar (type decl) incs)
	     (add-to-known-subtypes (subtype decl) (type decl)))
	    (t (pvs-warning "Subtype judgement is superfluous."))))))

(defmethod typecheck* ((decl number-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (break "typecheck* number-judgement")
  (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
  (set-type (declared-type decl) nil)
  (typecheck* (number decl) (type decl) 'expr nil)
  (add-to-alist (number (number decl)) decl (judgements *current-context*)))

(defmethod typecheck* ((decl named-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (typecheck* (formals decl) nil nil nil)
    (mapc #'(lambda (fmlist)
	      (mapc #'(lambda (fm)
			(unless (fully-instantiated? fm)
			  (type-error  (type fm)
			    "Could not determine the full theory instance")))
		    fmlist))
	  (formals decl)))
  (let* ((*bound-variables* (reverse (apply #'append (formals decl)))))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
    (set-type (declared-type decl) nil)
    (let* ((*no-conversions-allowed* t)
	   (expr (if (formals decl)
		     (mk-application-for-formals (name decl) (formals decl))
		     (name decl))))
      (typecheck* expr (type decl) nil nil)))
  (if (formals decl)
      (add-to-alist (declaration (name decl)) decl
		    (application-judgements *current-context*))
      (add-to-alist (declaration (name decl)) decl
		    (judgements *current-context*))))

(defun mk-application-for-formals (expr formals)
  (if (null formals)
      expr
      (let ((names (mapcar #'mk-name-expr (car formals))))
	(mapc #'(lambda (nm fm)
		  (setf (place nm) (place fm)))
	      names (car formals))
	(mk-application-for-formals
	 (mk-application* expr names)
	 (cdr formals)))))

(defmethod typecheck* ((decl typed-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (break "typecheck* typed-judgement")
  (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
  (set-type (declared-type decl) nil)
  ;;(setf (id decl) (id (name decl)))
  (setf (kind (name decl)) 'constant)
  (let ((dtype (typecheck* (declared-name-type decl) nil nil nil)))
    (set-type (declared-name-type decl) nil)
    (typecheck* (name decl) nil 'expr nil)
    (let ((res (find-if #'(lambda (r) (tc-eq (type r) dtype))
		 (resolutions (name decl)))))
      (unless res
	(type-error (name decl)
	  "No declaration of specified type could be found"))
      (setf (resolutions (name decl)) (list res))
      (cond ((typep dtype 'funtype)
	     ;;(setf (type (name decl)) (type res))
	     (typecheck-funtype-judgement decl))
	    (t (typecheck* (name decl) (type decl) 'expr nil)))))
  (add-to-alist (declaration (name decl)) decl (judgements *current-context*)))

(defun typecheck-funtype-judgement (decl)
  (let* ((jtype (find-supertype (type decl)))
	 (jdom (domain-types jtype))
	 (jran (range jtype))
	 (ntype (find-supertype (type (resolution (name decl)))))
	 (ndom (domain-types ntype))
	 (nran (range ntype))
	 (bds (make-new-bind-decls jdom))
	 (bvars (mapcar #'make-variable-expr bds))
	 (*bound-variables* (append bds *bound-variables*)))
    (check-judgement-types jdom ndom bvars)
    (let ((appl (mk-application* (name decl) bvars))
	  (range (if (typep (domain (type decl)) 'dep-binding)
		     (substit (range (type decl))
		       (acons (domain (type decl))
			      (make-arg-tuple-expr bvars)
			      nil))
		     (range (type decl)))))
      (typecheck* appl range nil nil))
    (let* ((*generate-tccs* 'none)
	   (ctype (compatible-type (type decl)
				   (type (resolution (name decl)))))
	   (incs (compatible-preds ctype (type decl) (name decl))))
      (generate-subtype-tcc (name decl) (type decl) incs))
    (if (some #'(lambda (x) (typep x 'dep-binding))
	      ndom)
	(let ((ctype (make-judgement-dependent-type
		      ndom jdom jran bvars decl)))
	  (when ctype
	    (setf (comparison-type decl) ctype)))
	(add-domains-to-known-subtypes decl bvars jdom jran))))

(defun add-domains-to-known-subtypes (decl vars jdom jran)
  (let* ((op (name decl))
	 (res (resolution op))
	 (jtypes (get-judgements res))
	 (joptypes (find-best-judgement-optypes vars (type res) jtypes)))
    (dolist (joptype joptypes)
      (let ((boptype (type res)))
	(mapc #'add-to-known-subtypes jdom (domain-types boptype))))))

(defun make-judgement-dependent-type (ndom jdom jran bvars decl)
  (let ((newdom (make-judgement-dependent-domain ndom jdom)))
    (add-domains-to-known-subtypes decl bvars newdom jran)
    (unless (equal newdom jdom)
      (let* ((dtype (find-supertype (type decl)))
	     (tupdom (if (cdr newdom) (mk-tupletype newdom) (car newdom)))
	     (ftype (if (typep (domain dtype) 'dep-binding)
			(let ((dv (make-new-dep-binding-variable 'x tupdom)))
			  (mk-funtype (declaration dv)
				      (substit (range dtype)
					(acons (domain dtype) dv nil))))
			(mk-funtype tupdom (range dtype)))))
	(if (eq (type decl) dtype)
	    ftype
	    (let* ((nvar (make-new-variable-name-expr 'y ftype))
		   (appl (beta-reduce
			  (typecheck* (mk-application (predicate dtype) nvar)
				      *boolean* nil nil)))
		   (lam-expr (make-lambda-expr (list (declaration nvar))
			       appl)))
	      (mk-subtype ftype lam-expr)))))))

(defun make-new-dep-binding-variable (id type)
  (let ((bd (typecheck* (mk-dep-binding id type) nil nil nil)))
    (mk-name-expr id nil nil (make-resolution bd nil type)
		  'variable)))
	      
(defun make-judgement-dependent-domain (ndom jdom &optional ntypes bindings)
  (if (null ndom)
      (nreverse ntypes)
      (if (typep (car ndom) 'dep-binding)
	  (if (typep (car jdom) 'dep-binding)
	      (make-judgement-dependent-domain
	       (cdr ndom) (cdr jdom)
	       (cons (car jdom) ntypes)
	       (acons (car ndom) (car jdom) bindings))
	      (let ((jbind (mk-dep-binding (id (car ndom)) (car jdom))))
		(make-judgement-dependent-domain
		 (cdr ndom) (cdr jdom)
		 (cons jbind ntypes)
		 (acons (car ndom) jbind bindings))))
	  (if (and (some #'(lambda (fv)
			     (assoc (declaration fv) bindings))
			 (freevars (car ndom)))
		   (not (tc-eq-with-bindings (car ndom) (car jdom) bindings)))
	      (let ((ntype (make-judgement-dependent-domain*
			    (car ndom) (car jdom) bindings)))
		(make-judgement-dependent-domain
		 (cdr ndom) (cdr jdom) (cons ntype ntypes) bindings))
	      (make-judgement-dependent-domain
	       (cdr ndom) (cdr jdom) (cons (car jdom) ntypes) bindings)))))

(defmethod make-judgement-dependent-domain* ((ndom subtype) (jdom type-expr)
					   bindings)
  (let* ((*generate-tccs* 'none)
	 (id (if (typep (predicate ndom) 'lambda-expr)
		 (id (car (bindings (predicate ndom))))
		 (make-new-variable '|x| jdom)))
	 (nvar (make-new-variable-name-expr id jdom))
	 (npred (substit (predicate ndom) bindings))
	 (appl (beta-reduce
		(typecheck* (mk-application npred nvar)
			    *boolean* nil nil)))
	 (lam-expr (make-lambda-expr (list (declaration nvar)) appl)))
    (mk-setsubtype jdom lam-expr)))

(defun check-judgement-types (jdom ndom bvars)
  (let ((*no-conversions-allowed* t))
    (check-tup-types bvars ndom)))

;;; add-to-known-subtypes updates the known-subtypes of the current
;;; context.  It first checks to see whether the subtype relation is
;;; already known, in which case it does nothing.  If it needs to update the 

(defun add-to-known-subtypes (atype etype)
  (let ((aty (if (typep atype 'dep-binding) (type atype) atype))
	(ety (if (typep etype 'dep-binding) (type etype) etype)))
    (unless (subtype-of? aty ety)
      (let* ((entry (get-known-subtypes aty))
	     (atypes (or (cdr entry)
			 (get-direct-subtype-alist aty)))
	     (etypes (or (cdr (get-known-subtypes ety))
			 (get-direct-subtype-alist ety)))
	     (inc-types (mapcar #'(lambda (ety)
				    (cons (car ety) (1+ (cdr ety))))
				etypes))
	     (mtypes (sort (append inc-types
				   (remove-if #'(lambda (at)
						  (assoc (car at) inc-types
							 :test #'tc-eq))
				     atypes))
			   #'< :key #'cdr)))
	(when (> (length (known-subtypes *current-context*)) 20)
	  (break "Long subtypes list"))
	(when (> (length entry) 20) (break "Long subtype entry"))
	(when *subtype-of-hash*
	  (let ((ht (gethash aty *subtype-of-hash*)))
	    (when ht
	      (clrhash *subtype-of-hash*))))
	(if entry
	    (setf (cdr entry)
		  (acons ety 1 mtypes))
	    (set-known-subtypes aty (acons ety 1 mtypes)))))))

(defun get-known-subtypes (aty)
  (assoc aty (known-subtypes *current-context*) :test #'tc-eq-for-subtypes))

(defun set-known-subtypes (type subtypes)
  (push (cons type subtypes)
	(known-subtypes *current-context*)))
    

(defun copy-known-subtypes (known-subtypes)
  (copy-tree known-subtypes))

(defun merge-known-subtypes (direct-types transitive-types)
  (if (null direct-types)
      transitive-types
      (let* ((dtype (car direct-types))
	     (found (assoc (car dtype) transitive-types :test #'tc-eq)))
	(when (and found
		   (< (cdr dtype) (cdr found)))
	  (setf (cdr found) (cdr dtype)))
	(merge-known-subtypes (cdr direct-types)
			      (if found
				  transitive-types
				  (cons dtype transitive-types))))))

(defmethod get-direct-subtype-alist ((te subtype) &optional endtype)
  (get-direct-subtype-alist* (supertype te) 1 nil endtype))

(defmethod get-direct-subtype-alist ((te dep-binding) &optional endtype)
  (get-direct-subtype-alist* (type te) 1 nil endtype))

(defmethod get-direct-subtype-alist ((te type-expr) &optional endtype)
  nil)

(defmethod get-direct-subtype-alist* :around (te dist alist endtype)
  (if (and endtype
	   (tc-eq te endtype))
      (nreverse alist)
      (call-next-method)))

(defmethod get-direct-subtype-alist* ((te subtype) dist alist endtype)
  (get-direct-subtype-alist* (supertype te) (1+ dist)
			     (acons te dist alist)
			     endtype))

(defmethod get-direct-subtype-alist* ((te type-expr) dist alist endtype)
  (nreverse (acons te dist alist)))


;;; Conversions

(defmethod typecheck* ((decl conversion-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (name decl) nil 'expr nil)
  (unless (singleton? (resolutions (name decl)))
    (type-ambiguity (name decl)))
  (setf (type (name decl)) (type (resolution (name decl))))
  (typecheck-conversion decl))

(defmethod typecheck* ((decl typed-conversion-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((type (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (typecheck* (name decl) type 'expr nil))
  (typecheck-conversion decl))

(defun typecheck-conversion (decl)
  (if (typep (declaration (name decl)) 'const-decl)
      (setf (kind (name decl)) 'constant)
      (type-error (name decl)
	"Conversion must be a constant"))
  (let ((type (find-supertype (type (name decl)))))
;    (unless (and (typep type 'funtype)
;		 (singleton? (domain type)))
;      (type-error (name decl)
;	"Coercion is not a unary function:~%  ~a: ~a" (name decl) type))
    (when (strict-compatible? (domain type) (range type))
      (type-error (name decl)
	"The domain and range of this conversion are compatible;~%~
         the conversion will never be used:~%  ~a: ~a" (name decl) type))
    (setf (k-combinator? decl) (k-combinator? (declaration (name decl))))
    (push decl (conversions *current-context*))
    decl))

(defmethod type ((decl conversion-decl))
  (type (name decl)))
    

(defun check-duplication (decl)
  (when (projection? (id decl))
    (type-error decl "May not overload projection names"))
  (let ((decls (gethash (id decl) (local-decls *current-context*))))
    (mapc #'(lambda (d) (duplicate-decls decl d)) decls)))

(defun duplicate-decls (x y)
  (when (eq (kind-of x) (kind-of y))
    (if (eq (kind-of x) 'expr)
	(let ((tyx (type x))
	      (tyy (type y)))
	  (cond ((strict-compatible? tyx tyy)
		 (type-error x
		   "~a has previously been declared with type ~a, which is ambiguous"
		   (id x) (unparse tyx :string t)))
		((tc-unifiable? x y)
		 (type-error x
		   "~a has previously been declared with type ~a, which may lead to ambiguity"
		   (id x) (unparse (type y) :string t)))))
	(unless (and (typep y 'type-def-decl)
		     (typep (type-expr y) 'datatype))
	  (type-error x
	    "Name ~a already in use as a ~a" (id x) (kind-of x))))))

(defun tc-unifiable? (x y)
  nil)
	

(defun set-prelude-types (id type)
  (case id
    (|boolean| (setq *boolean* type))
    (|number| (setq *number* type))
    (|ordered_number| (setq *ordered_number* type))
    (|real| (setq *real* type))
    (|rational| (setq *rational* type))
    (|integer| (setq *integer* type))
    (|naturalnumber| (setq *naturalnumber* type))
    (|posint| (setq *posint* type))
    (|ordinal| (setq *ordinal* type))))

