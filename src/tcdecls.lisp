;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcdecls.lisp -- 
;; Author          : Sam Owre
;; Created On      : Mon Oct 18 22:45:21 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Nov  4 17:04:21 1998
;; Update Count    : 90
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

(export '(typecheck-decls typecheck-decl))

(defun typecheck-decls (decls)
  (when decls
    (let ((decl (car decls)))
      (setf (declaration *current-context*) decl)
      (typecase decl
	(declaration (typecheck-decl decl))
	(importing (tcdebug "~%    Processing importing")
		   (let ((*generating-adt* nil))
		     (typecheck* decl nil nil nil)))
	(recursive-type (unless (typechecked? decl)
			  (tcdebug "~%    Processing (Co)Datatype ~a"
				   (id decl))
			  (unwind-protect
			      (typecheck* decl nil nil nil)
			    (cleanup-datatype decl)))))
      (typecheck-decls (cdr decls)))))

(defun typecheck-decl (decl)
  (if (and (typechecked? decl)
	   (not (typep decl '(or theory-abbreviation-decl
			         mod-decl conversion-decl auto-rewrite-decl
				 judgement))))
      (mapc #'(lambda (d) (add-decl d nil))
	    (generated decl))
      (unwind-protect
	  (progn
	    (assert (typep (theory *current-context*)
			   '(or module recursive-type)))
	    (setf (module decl) (theory *current-context*))
	    (tcdebug "~%    Typechecking ~a" decl)
	    (let ((stime (get-run-time)))
	      (typecheck* decl nil nil nil)
	      (setf (typecheck-time decl) (runtime-since stime)))
	    (setf (typechecked? decl) t))
	(unless (or *loading-prelude*
		    (typechecked? decl))
	  (cleanup-typecheck-decls decl))))
  (unless (and (type-def-decl? decl)
	       (enumtype? (type-expr decl)))
    (put-decl decl (current-declarations-hash))))

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
;   (setf (gethash (id decl) (declarations *current-theory*))
; 	(remove decl
; 		(gethash (id decl) (declarations *current-theory*))))
  )

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
	  (list (mk-resolution decl (current-theory-name) tn)))
    (setf (type decl) tn)
    (setf (type-value decl) tn))
  decl)

(defmethod typecheck* ((decl formal-nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl (current-declarations-hash))
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl))
  decl)

(defmethod typecheck* ((decl formal-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (let* ((tn (mk-type-name (id decl)))
	 (res (mk-resolution decl (current-theory-name) tn))
	 (tval (type-def-decl-value decl tn)))
    (setf (resolutions tn) (list res))
    (setf (uninterpreted? tn) t)
    (setf (type-value decl) tval))
  decl)

(defmethod typecheck* ((decl formal-nonempty-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl (current-declarations-hash))
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl))
  decl)


(defmethod typecheck* ((decl formal-const-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl)
	  (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (if (free-params (type decl))
      (set-nonempty-type (type decl))
      (check-nonempty-type (type decl) (id decl)))
  (check-duplication decl)
  decl)

(defmethod typecheck* ((decl formal-theory-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; If we have an existing formal-theory-decl or mod-decl of the same
  ;; theory, and it has uninterpreted types, constants, or theories, then
  ;; we must create a separate copy.
  (let ((th (get-theory (id decl))))
    (when (and th (gethash th (current-using-hash)))
      (type-error decl
	"Identifier ~a is already in use as a theory" (id decl))))
  (typecheck-named-theory decl)
  (unless (fully-instantiated? (theory-name decl))
    (type-error (theory-name decl)
      "Actual parameters must be provided for theory declarations"))
  (put-decl decl (current-declarations-hash))
  (setf (saved-context decl) (copy-context *current-context*))
  decl)

(defun typecheck-named-theory (decl)
  (let ((theory-name (theory-name decl)))
    (when (and (null (library (theory-name decl)))
	       (eq (id (theory-name decl)) (id (current-theory))))
      (type-error (theory-name decl)
	"Formal theory declarations may not refer to the containing theory"))
    (let ((theory (get-typechecked-theory theory-name))
	  (abbr-info (if (mappings theory-name) ""
			 (format nil
			     "~%No mappings given in theory declaration ~a~%~
                              Perhaps this should be given the abbreviation form~
                              ~%  ~a"
			   (id decl)
			   (unparse (make-instance 'theory-abbreviation-decl
				      'id (id decl)
				      'theory-name theory-name)
			     :string t)))))
      (unless (interpretable? theory)
	(pvs-info
	    "Theory ~a has no interpretable types, constants, or theories"
	  theory-name))
      (unless (equal abbr-info "")
	(pvs-info abbr-info))
      (typecheck-named-theory* theory theory-name decl))))

(defmethod typecheck-named-theory* ((theory module) theory-name decl)
  (let ((*typecheck-using* theory-name))
    (cond ((actuals theory-name)
	   (unless (length= (formals-sans-usings theory) (actuals theory-name))
	     (type-error theory-name "Wrong number of actuals in ~a"
			 theory-name))
	   (typecheck-actuals theory-name)
	   (typecheck-mappings (mappings theory-name) theory-name)
	   (set-type-actuals theory-name)
	   (check-compatible-params (formals-sans-usings theory)
				    (actuals theory-name) nil))
	  (t (typecheck-mappings (mappings theory-name) theory-name)
	     (set-type-actuals theory-name)))
    (let ((local-ref (find-local-theory-reference theory-name)))
      (when local-ref
	(type-error local-ref
	  "Illegal reference to ~a~%May not use theory declarations with ~
      actuals or mappings that reference~%entities declared in this theory."
	  local-ref)))	
    (let ((interpreted-copy (make-interpreted-copy theory theory-name decl)))
      (push interpreted-copy (named-theories *current-context*))
      (typecheck-using* interpreted-copy (mk-modname (id interpreted-copy)))
      (setf (generated-theory decl) interpreted-copy))))

(defmethod typecheck-named-theory* ((theory datatype) theory-name decl)
  (typecheck-named-theory* (adt-theory theory)
			   (copy theory-name 'id (id (adt-theory theory)))
			   decl))

(defun find-local-theory-reference (expr &optional allowed-decls)
  (let ((local-ref nil)
	(place-ex nil))
    (mapobject #'(lambda (ex)
		   (or local-ref
		       (if (name? ex)
			   (setq place-ex nil)
			   (when (place ex)
			     (setq place-ex ex)
			     nil))
		       (when (and (name? ex)
				  (resolution ex)
				  (not (binding? (declaration ex)))
				  (not (var-decl? (declaration ex)))
				  (not (or (memq (declaration ex) allowed-decls)
					   (and (generated-by (declaration ex))
						(memq (generated-by (declaration ex))
						      allowed-decls))))
				  (eq (module (declaration ex))
				      (current-theory)))
			 (setq local-ref (or place-ex ex)))))
	       expr)
    local-ref))

;;; theory and theory-name are the theory to be mapped
;;; (current-theory) is the theory referencing theory-name
(defun make-interpreted-copy (theory theory-name decl)
  (let* ((*all-subst-mod-params-caches*
	  (if (every #'mapping-subst? (mappings theory-name))
	      *all-subst-mod-params-caches*
	      nil))
	 (stheory (subst-mod-params theory theory-name))
	 (itheory (add-referring-theory-importings theory-name stheory))
	 (ntheory (pc-parse (unparse (copy itheory 'id (id decl)) :string t)
		    'adt-or-theory))
	 (*generate-tccs* 'none)
	 (*current-context* (make-new-context ntheory)))
    (change-class ntheory 'theory-interpretation
		  'from-theory theory)
    (push ntheory (named-theories *current-context*))
    (typecheck* ntheory nil nil nil)
    (setf (generated-by ntheory) (id theory))
    (setf (mapping ntheory)
	  (get-interpreted-mapping theory ntheory theory-name))
    ntheory))

(defun add-referring-theory-importings (theory-name stheory)
  (let* ((impinstances (get-currently-imported-instances))
	 (thinstances (collect-mapping-rhs-imported-instances
		       theory-name impinstances)))
    (if thinstances
	(let ((imps (mapcar #'(lambda (thinst)
				(make-instance 'importing
				  'theory-name thinst))
		      thinstances)))
	  (if (assuming stheory)
	      (copy stheory
		'assuming
		(append imps
			(remove-if #'(lambda (x)
				       (and (importing? x)
					    (member (theory-name x) thinstances
						    :test #'tc-eq)))
			  (assuming stheory))))
	      (copy stheory
		'theory
		(append imps
			(remove-if #'(lambda (x)
				       (and (importing? x)
					    (member (theory-name x) thinstances
						    :test #'tc-eq)))
			  (theory stheory))))))
	stheory)))

(defun get-currently-imported-instances ()
  (mapcan #'get-currently-imported-instance
    (nreverse
     (cdr (memq (current-declaration)
		(reverse (all-decls (current-theory))))))))

(defmethod get-currently-imported-instance (x)
  (declare (ignore x))
  nil)

(defmethod get-currently-imported-instance ((imp importing))
  (list (theory-name imp)))

(defmethod get-currently-imported-instance ((imp mod-decl))
  (list (theory-name imp)))

(defmethod get-currently-imported-instance ((imp theory-abbreviation-decl))
  (list (theory-name imp)))

(defmethod get-currently-imported-instance ((imp formal-theory-decl))
  (list (theory-name imp)))

;;; We grab the instances from impinstances that are actually used
(defun collect-mapping-rhs-imported-instances (theory-name impinstances)
  (let ((thinstances nil))
    (mapobject #'(lambda (ex)
		   (when (and (name? ex)
			      (resolution ex)
			      (not (from-prelude? (declaration ex)))
			      (not (from-prelude-library? (declaration ex))))
		     (pushnew (module-instance ex) thinstances :test #'tc-eq)
		     nil))
	       (append (actuals theory-name)
		       (mapcar #'rhs (mappings theory-name))))
    ;; Now remove from impinstances those not in thinstances
    ;; We do this because we want the minimal set, otherwise might bring in
    ;; references to local decls that are not actually needed.
    (remove-if (complement
		#'(lambda (impinst)
		    (or (member impinst thinstances :test #'tc-eq)
			(and (null (actuals impinst))
			     (some #'(lambda (thinst)
				       (and (eq (id thinst) (id impinst))
					    (not (member thinst impinstances
							 :test #'tc-eq))))
				   thinstances)))))
      impinstances)))

(defun get-interpreted-mapping (theory interpretation theory-name)
  (let ((mapping (make-subst-mod-params-map-bindings
		  theory-name (mappings theory-name) nil))
	(int-decls (all-decls interpretation)))
    (dolist (decl (interpretable-declarations theory))
      (unless (assq decl mapping)
	(push (cons decl (find decl int-decls
			       :test #'(lambda (x y)
					 (and (declaration? y)
					      (same-id x y)))))
	      mapping)))
    (assert (every #'cdr mapping))
    mapping))
		     


;;; Library Declarations

(defmethod typecheck* ((decl lib-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;;;(check-duplication decl)
  (multiple-value-bind (ref msg)
      (get-lib-decl-lib-ref (lib-string decl))
    (when msg
      (type-error decl msg (lib-string decl)))
    (setf (lib-ref decl) ref)
    (dolist (d (gethash (id decl) (current-declarations-hash)))
      (when (and (lib-decl? d)
		 ;; FIXME - Should refine this
		 (not (string= ref (lib-ref d))))
	(if (eq (module d) (current-theory))
	    (type-error decl
	      "Library id ~a declared earlier in this theory (~a) ~
               with a different path."
	      (id decl) (id (current-theory)))
	    (pvs-warning
		"Library id ~a declared in imported theory ~a ~
               with a different path."
	      (id decl) (id (module d))))))))

(defun get-lib-decl-lib-ref (libstr)
  (let ((dirstr (if (char= (char libstr (1- (length libstr))) #\/)
		    libstr
		    (concatenate 'string libstr "/"))))
    (cond ((member (char dirstr 0) '(#\/ #\~) :test #'char=)
	   (if (file-exists-p dirstr)
	       dirstr
	       (values nil (format nil "Directory ~a does not exist" libstr))))
	  ((char= (char dirstr 0) #\.)
	   (if (file-exists-p (merge-pathnames dirstr *pvs-context-path*))
	       dirstr
	       (values nil (format nil "Directory ~a does not exist" libstr))))
	  (t (or (get-library-reference (intern libstr))
		 (values nil (format nil
				 "Directory ~a does not exist" libstr)))))))

;;; Module declarations

(defmethod typecheck* ((decl mod-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((th (get-theory (id decl))))
    (when (and th (gethash th (current-using-hash)))
      (type-error decl
	"Identifier ~a is already in use as a theory" (id decl))))
  (typecheck-named-theory decl)
  (unless (fully-instantiated? (modname decl))
    (type-error (modname decl)
      "Actual parameters must be provided for theory declarations"))
  (put-decl decl (current-declarations-hash))
  (setf (saved-context decl) (copy-context *current-context*))
  ;; Need to allow id to be used as abbreviation
  )

(defmethod typecheck* ((decl theory-abbreviation-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (if (typechecked? decl)
      (let* ((modinst (theory-name decl))
	     (mod (get-theory modinst)))
	 (add-exporting-with-theories mod modinst)
	 (add-to-using modinst))
      (typecheck-using (theory-name decl)))
  (put-decl decl (current-declarations-hash))
  (setf (saved-context decl) (copy-context *current-context*))
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
		(list (mk-resolution decl (current-theory-name) tn)))
	  tn))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (set-nonempty-type (type-value decl))
  (unless (eq (id *current-theory*) '|booleans|)
    (put-decl decl (current-declarations-hash))
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
	 (res (mk-resolution decl (current-theory-name) tn))
	 (*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (ptype (if (formals decl)
		    (make-instance 'type-application
		      'type tn
		      'parameters (mapcar #'mk-name-expr (car (formals decl))))
		    tn))
	 (tval (type-def-decl-value decl ptype)))
    (setf (resolutions tn) (list res))
    (setf (uninterpreted? tn) t)
    (setf (type-value decl) tval)
    (typecase (type-expr decl)
      (enumtype (typecheck* (type-expr decl) nil nil nil))
      (subtype (when (typep (predicate (type-expr decl)) 'expr)
		 (put-decl decl (current-declarations-hash)))))
    (when (and (nonempty-type-def-decl? decl)
	       (not (contains decl)))
      (let ((*bound-variables* (apply #'append (formals decl))))
	(check-nonempty-type-of decl)))
    (when (contains decl)
      (typecheck* (contains decl) tval nil nil)
      (set-nonempty-type (type-expr decl))
      (set-nonempty-type tval)))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defun check-type-application-formals (decl)
  (when (formals decl)
    (when (type-from-decl? decl)
      (type-error decl
	"Uninterpreted types may not have parameters"))
    (when (cdr (formals decl))
      (type-error (cdar (formals decl))
	"Type applications may not be curried"))
    (typecheck* (formals decl) nil nil nil)
    (set-formals-types (apply #'append (formals decl)))))

(defmethod check-nonempty-type-of ((decl nonempty-type-def-decl))
  (let ((ctype (copy (type-value decl) 'print-type nil)))
    (check-nonempty-type ctype decl)
    (setf (nonempty? (type-value decl)) (nonempty? ctype))))

(defmethod check-nonempty-type-of ((decl nonempty-type-from-decl))
  (check-nonempty-type (supertype (type-value decl)) decl)
  (set-nonempty-type (type-value decl))
  (put-decl decl (current-declarations-hash))
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
  (let ((*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (assert (null (freevars (type decl))))
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
    (set-formals-types (apply #'append (formals decl))))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil))))
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
      (put-decl decl (current-declarations-hash))
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

(defun make-formals-funtype (formals range)
  (let ((*generate-tccs* 'none))
    (if (null formals)
	range
	(let ((nrange (make-formals-funtype (cdr formals) range)))
	  (make-formals-funtype* (car formals) nrange)))))

(defun make-formals-funtype* (formals range)
  (if (some #'(lambda (ff)
		(member ff (freevars range) :test #'same-declaration))
	    formals)
      (let* ((ndom (make-formals-domain formals))
	     (nvar (if (cdr formals)
		       (make-new-variable '|d| range)
		       (id (car formals))))
	     (ndep (mk-dep-binding nvar ndom))
	     (nvar (mk-name-expr nvar nil nil
				 (make-resolution ndep
				   (theory-name *current-context*) ndom)))
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
				       (theory-name *current-context*)
				       (type (car formals))))))
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
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil)))
	 (*recursive-subtype-term* nil))
    (set-type (declared-type decl) nil)
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    #+pvsdebug (assert (null (freevars (type decl))))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "Recursive definition must be a function type"))
    (unless (typep decl 'adt-def-decl)
      (typecheck-measure decl)
      (setf (recursive-signature decl) (compute-recursive-signature decl)))
    (set-nonempty-type rtype)
    (put-decl decl (current-declarations-hash))
    (let ((*recursive-calls-without-enough-args*
	   (unless (adt-def-decl? decl)
	     (recursive-calls-without-enough-args decl)))
	  (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl))))
      (if *recursive-calls-without-enough-args*
	  (let ((cdef (copy-all (definition decl))))
	     (typecheck* (definition decl) rtype nil nil)
	     (let ((*generate-tccs* 'none))
	       (setf (definition decl) cdef)
	       (typecheck* (definition decl) rtype nil nil)))
	  (typecheck* (definition decl) rtype nil nil)))
    (make-def-axiom decl))
  decl)

(defun set-formals-types (formals)
  (when formals
    (when (declared-type (car formals))
      (set-type (declared-type (car formals)) nil))
    (let ((*bound-variables* (cons (car formals) *bound-variables*)))
      (set-formals-types (cdr formals)))))


;;; Given a recursive function of the form
;;; f(x1:X1,..,xk:Xk)(y1:Y1,..,ym:Ym)(z1:Z1,..,zn:Zn): RECURSIVE R =
;;;    ...
;;;  MEASURE M BY <
;;; Computes
;;;  [a:[X1,..,Xk] -> [b:[Y1,..,Ym] ->
;;;     [{c:[Z1,..,Zn] | M(a)(b)(c) < M(x1,..,xk)(y1,..,ym)(z1,..,zn)} -> R]]]
;;; In general, of course, there may be dependencies that we need to take
;;; care of.
(defun compute-recursive-signature (decl)
  (assert (measure-depth decl))
  (compute-recursive-signature* (type decl) (measure-depth decl) decl))

(defun compute-recursive-signature* (type depth decl &optional domtypes)
  (if (= depth 0)
      (let* ((dom (domain type))
	     (vid (make-new-variable '|z| (cons decl domtypes)))
	     (bd (make-bind-decl vid dom))
	     (avar (make-variable-expr bd))
	     (arg1 (make-lhs-measure-application
		    (measure decl) (reverse domtypes) avar decl))
	     (arg2 (make-rhs-measure-application decl))
	     (appl (if (ordering decl)
		       (make!-application (copy (ordering decl)) arg1 arg2)
		       (typecheck* (mk-application '< arg1 arg2)
				   *boolean* nil nil)))
	     (pred (make!-lambda-expr (list bd) appl))
	     (subtype (mk-setsubtype dom pred))
	     (fvars (mapcar #'declaration
		      (freevars (cons subtype (cons (range type) domtypes)))))
	     (clean-domtypes (mapcar #'(lambda (dtype)
					 (if (memq dtype fvars)
					     dtype
					     (type dtype)))
			       domtypes)))
	(setq *recursive-subtype-term* arg2)
	(if (dep-binding? dom)
	    (let ((dsubtype (mk-dep-binding (make-new-variable (id dom)
					      (freevars (range type)))
					    subtype)))
	      (mk-funtype* (cons dsubtype clean-domtypes)
			   (substit (range type)
			     (acons dom dsubtype nil))))
	    (mk-funtype* (cons subtype clean-domtypes) (range type))))
      (if (dep-binding? (domain type))
	  (let ((ndep (mk-dep-binding (make-new-variable (id (domain type))
					(freevars (range type)))
				      (type (domain type)))))
	    (compute-recursive-signature*
	     (substit (range type) (acons (domain type) ndep nil))
	     (1- depth) decl
	     (cons ndep domtypes)))
	  (compute-recursive-signature*
	   (range type) (1- depth) decl
	   (cons (mk-dep-binding (make-new-variable '|z| (cons decl domtypes))
				 (domain type))
		 domtypes)))))

(defun make-lhs-measure-application (meas domtypes var decl)
  (if (null domtypes)
      (make!-application meas var)
      (make-lhs-measure-application
       (let ((dvar (make-variable-expr (car domtypes))))
	 (make!-application meas dvar))
       (cdr domtypes) var decl)))

(defun make-rhs-measure-application (decl)
  (make!-recursive-application (measure decl) (outer-arguments decl)))

(defun recursive-calls-without-enough-args (decl)
  (let ((depth (measure-depth decl))
	(processed-names nil)
	(impoverished-names nil))
    (mapobject #'(lambda (ex)
		   (typecase ex
		     (name-expr
		      (when (and (eq (id ex) (id decl))
				 (not (memq ex processed-names)))
			(push ex impoverished-names)))
		     (application
		      (let ((op (operator* ex))
			    (args (argument* ex)))
			(when (and (name-expr? op)
				   (eq (id op) (id decl))
				   (not (memq op processed-names)))
			  (push op processed-names)
			  (unless (> (length args) depth)
			    (push op impoverished-names))))))
		   nil)
	       (definition decl))
    impoverished-names))

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

;;; decl is the def-decl
;;; type is the type of the decl (recurses on the range)
;;; meas is the measure of the decl (recurses on the expression if lambda-expr)
;;; mtypes is the possible types of the meas (recurses on the ranges)
;;; ordering is the ordering of the decl
;;; doms is the domains collected as we recurse on the ranges

(defun typecheck-measure-with-ordering (decl type meas mtypes ordering
					     &optional doms)
  (let ((ftype (find-supertype type)))
    (unless (funtype? ftype)
      (type-error (measure decl)
	"Wrong number of arguments in measure"))
    (let* ((pranges
	    (mapcar #'(lambda (pty)
			(let ((sty (find-supertype pty)))
			  (copy ftype 'range (car (types (domain sty))))))
	      (ptypes ordering)))
	   (ctypes
	    (remove-if-not #'(lambda (rty)
			       (some #'(lambda (ty) (compatible? ty rty))
				     pranges))
	      mtypes)))
      (cond ((singleton? ctypes)
	     (setf (measure-depth decl) (length doms))
	     (typecheck (measure decl)
	       :expected (mk-funtype* doms (car ctypes))
	       :tccs 'all)
	     (typecheck-ordering decl))
	    ((funtype? ftype)
	     (let ((ptypes
		    (remove-if-not
			#'(lambda (mty)
			    (and (typep mty 'funtype)
				 (compatible? (domain ftype) (domain mty))))
		      mtypes)))
	       (if ptypes
		   (typecheck-measure-with-ordering
		    decl (range ftype)
		    (when (lambda-expr? meas) (expression meas)) 
		    (mapcar #'range mtypes)
		    ordering
		    (cons (domain ftype) doms))
		   (measure-incompatible decl type meas mtypes))))
	    (t (type-error (measure decl)
		 "Measure must have range compatible with the domain of the ordering"))))))

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
				      (nth-domain mty doms)
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

(defmethod nth-domain ((ftype funtype) doms)
  (if (null doms)
      (domain ftype)
      (nth-domain (range ftype) (cdr doms))))

(defmethod nth-domain (ftype doms)
  (declare (ignore ftype doms))
  nil)

(defmethod measure-incompatible (decl type (meas lambda-expr) mtypes)
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
	  "Incompatible domain types for measure~
           ~%     Found: ~{~a~^, ~}~%  Expected: ~a"
	  (mapcar #'domain mtypes) (domain type))
	(type-error (measure decl)
	  "Wrong number of arguments in measure"))))

(defun mk-funtype* (doms range)
  (if (null doms)
      range
      (mk-funtype* (cdr doms) (mk-funtype (car doms) range))))


;;; inductive-decl

(defmethod typecheck* ((decl fixpoint-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  (set-formals-types (apply #'append (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil))))
    (set-type (declared-type decl) nil)
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "(Co)Inductive definition must be a function type"))
    (unless (tc-eq (find-supertype (range* (type decl))) *boolean*)
      (type-error decl
	"(Co)Inductive definitions must have (eventual) range type boolean"))
    (set-nonempty-type rtype)
    (put-decl decl (current-declarations-hash))
    (typecheck* (definition decl) rtype nil nil)
    (let* ((all-vars (ind-def-formals decl))
	   (fixed-vars (fixed-inductive-variables decl all-vars))
	   (rem-vars (remove-if #'(lambda (avar) (memq avar fixed-vars))
		       all-vars))
	   (pred-type (inductive-pred-type rem-vars rtype)))
      (check-inductive-occurrences decl pred-type fixed-vars rem-vars)
      (make-def-axiom decl)
      (make-inductive-def-inductions decl pred-type fixed-vars rem-vars)))
  decl)

(defun inductive-pred-type (vars rtype)
  (if (null vars)
      rtype
      (inductive-pred-type* vars rtype)))

(defun inductive-pred-type* (vars rtype &optional types)
  (if (null vars)
      (typecheck* (mk-funtype (nreverse types) rtype) nil nil nil)
      (let* ((var (car vars))
	     (vtype (or (declared-type var)
			(print-type (type var))
			(type var)))
	     (dep? (or (member var (freevars (cdr vars)) :key #'declaration)
		       (member var (freevars rtype) :key #'declaration))))
	(if dep?
	    (let* ((dep-type (mk-dep-binding (id var) (type var) vtype))
		   (alist (acons var dep-type nil)))
	      (inductive-pred-type*
	       (substit (cdr vars) alist)
	       (substit rtype alist)
	       (cons dep-type types)))
	    (inductive-pred-type* (cdr vars) rtype (cons vtype types))))))

(defun check-inductive-occurrences (decl pred-type fixed-vars rem-vars)
  (unless (check-inductive-occurrences* (definition-body decl) decl 1)
    (let* ((pid (make-new-variable 'P (cons (definition decl) (formals decl))))
	   (bd (make-bind-decl pid pred-type))
	   (var (make-variable-expr bd))
	   (body (definition-body decl))
	   (pbody (subst-pred-for-ind var fixed-vars body decl))
	   (lbody (make-lambda-expr (list bd)
		    (if rem-vars
			(make-lambda-expr rem-vars pbody)
			pbody)))
	   (mono (make-monotonic-application lbody))
	   (ubody (if fixed-vars
		      (make-forall-expr fixed-vars mono)
		      mono))
	   (ndecl (typecheck* (mk-monotonicity-tcc (make-tcc-name) ubody)
			      nil nil nil)))
      (insert-tcc-decl 'monotonicity (mk-name-expr (id decl)) nil ndecl))))

(defun make-monotonic-application (lbody)
  ;; When we have monotonic? in the prelude, we will use this
  ;;  (typecheck* (mk-application '|monotonic?| lbody) *boolean* nil nil)
  
  ;; Until then, need to expand monotonic? directly, i.e., given lbody of 
  ;; the form "LAMBDA (p:pred[T]): expr", we generate
  ;; FORALL (p1, p2: pred[T]):
  ;;   (FORALL (x:T): p1(x) => p2(x))
  ;;       => (FORALL (x:T): lbody(p1)(x) => lbody(p2)(x))
  (let* ((pid1 (make-new-variable 'P lbody 1))
	 (pid2 (make-new-variable 'P lbody 2))
	 (ptype (type (car (bindings lbody))))
	 (bd1 (make-bind-decl pid1 ptype))
	 (bd2 (make-bind-decl pid2 ptype))
	 (var1 (make-variable-expr bd1))
	 (var2 (make-variable-expr bd2)))
    (if (tc-eq (find-supertype ptype) *boolean*)
	(make-forall-expr (list bd1 bd2)
	  (make-implication (make-implication var1 var2)
			    (make-implication
			     (beta-reduce (make-application lbody var1))
			     (beta-reduce (make-application lbody var2)))))
	(let* ((xid1 (make-new-variable '|x| lbody))
	       (xid2 (make-new-variable '|x| lbody))
	       (xbd1 (make-bind-decl xid1 (domain ptype)))
	       (xbd2 (make-bind-decl xid2 (domain ptype)))
	       (xvar1 (make-variable-expr xbd1))
	       (xvar2 (make-variable-expr xbd2))
	       (ante (make-forall-expr (list xbd1)
		       (make-implication (make-application var1 xvar1)
					 (make-application var2 xvar1))))
	       (succ (make-forall-expr (list xbd2)
		       (make-implication
			(beta-reduce (make-application
					 (make-application lbody var1)
				       xvar2))
			(beta-reduce (make-application
					 (make-application lbody var2)
				       xvar2))))))
	  (make-forall-expr (list bd1 bd2)
	    (make-implication ante succ))))))

;;; parity is 1 for positive context, -1 for negative, and 0 for unknown
;;; Returns T if all occurrences are known to be positive.

(defmethod check-inductive-occurrences* ((ex name-expr) decl parity)
  (declare (ignore parity))
  (not (eq (declaration ex) decl)))

(defmethod check-inductive-occurrences* ((ex application) decl parity)
  (cond
    ((or (conjunction? ex)
	 (disjunction? ex))
     (and (check-inductive-occurrences* (args1 ex) decl parity)
	  (check-inductive-occurrences* (args2 ex) decl parity)))
    ((implication? ex)
     (and (check-inductive-occurrences* (args1 ex) decl (- parity))
	  (check-inductive-occurrences* (args2 ex) decl parity)))
    ((negation? ex)
     (check-inductive-occurrences* (args1 ex) decl (- parity)))
    ((inductive-occurrence? ex (type decl) decl)
     (or (= parity 1)
	 (when (= parity -1)
	   (type-error ex
	     "Negative occurrences of inductive definitions are not allowed."))))
    (t (and (check-inductive-occurrences* (operator ex) decl 0)
	    (check-inductive-occurrences* (arguments ex) decl 0)))))

(defmethod check-inductive-occurrences* ((ex branch) decl parity)
  (and (check-inductive-occurrences* (condition ex) decl 0)
       (check-inductive-occurrences* (then-part ex) decl parity)
       (check-inductive-occurrences* (else-part ex) decl parity)))

(defmethod check-inductive-occurrences* ((ex cases-expr) decl parity)
  (and (check-inductive-occurrences* (expression ex) decl parity)
       (check-inductive-occurrences* (selections ex) decl parity)
       (or (null (else-part ex))
	   (check-inductive-occurrences* (else-part ex) decl parity))))

(defmethod check-inductive-occurrences* ((ex selection) decl parity)
  (check-inductive-occurrences* (expression ex) decl parity))


;;; inductive-occurrence? checks that the expression is a fully applied
;;; form of the inductive constant.

(defmethod inductive-occurrence? (ex (type subtype) decl)
  (inductive-occurrence? ex (find-supertype type) decl))

(defmethod inductive-occurrence? ((ex application) (type funtype) decl)
  (inductive-occurrence? (operator ex) (find-supertype (range type)) decl))

(defmethod inductive-occurrence? ((ex name-expr) (type type-name) decl)
  (eq (declaration ex) decl))

(defmethod inductive-occurrence? (ex type decl)
  (declare (ignore ex type decl))
  nil)

;;; partial-inductive-occurrence? is like inductive-occurrence?, but does
;;; not require the occurrence to be fully applied.

(defmethod partial-inductive-occurrence? (ex (type subtype) decl)
  (partial-inductive-occurrence? ex (find-supertype type) decl))

(defmethod partial-inductive-occurrence? ((ex application) (type funtype) decl)
  (partial-inductive-occurrence? (operator ex) (find-supertype (range type)) decl))

(defmethod partial-inductive-occurrence? ((ex name-expr) type decl)
  (declare (ignore type))
  (eq (declaration ex) decl))

(defmethod partial-inductive-occurrence? (ex type decl)
  (declare (ignore ex type decl))
  nil)

(defmethod check-inductive-occurrences* ((ex quant-expr) decl parity)
  (and (not (occurs-in decl (bindings ex)))
       (check-inductive-occurrences* (expression ex) decl parity)))

(defmethod check-inductive-occurrences* ((list list) decl parity)
  (or (null list)
      (and (check-inductive-occurrences* (car list) decl parity)
	   (check-inductive-occurrences* (cdr list) decl parity))))

(defmethod check-inductive-occurrences* (ex decl parity)
  (declare (ignore parity))
  (not (occurs-in decl ex)))


(defun make-inductive-def-inductions (decl pred-type fixed-vars rem-vars)
  (let* ((pid (make-new-variable 'P (definition decl)))
	 (wbd (make-bind-decl pid pred-type))
	 (sbd (make-bind-decl pid pred-type))
	 (wvar (make-variable-expr wbd))
	 (svar (make-variable-expr sbd))
	 (body (definition-body decl))
	 (wprem (weak-induction-hypothesis wvar fixed-vars rem-vars body decl))
	 (sprem (strong-induction-hypothesis svar fixed-vars rem-vars body decl))
	 (wconc (make-inductive-conclusion wvar rem-vars decl))
	 (sconc (make-inductive-conclusion svar rem-vars decl))
	 (wform (make!-forall-expr (append fixed-vars (list wbd))
		  (make!-implication wprem wconc)))
	 (sform (make!-forall-expr (append fixed-vars (list sbd))
		  (make!-implication sprem sconc)))
	 (wid (makesym "~a_weak_~ainduction" (id decl)
		       (if (inductive-decl? decl) "" "co")))
	 (sid (makesym "~a_~ainduction" (id decl)
		       (if (inductive-decl? decl) "" "co")))
	 (wdecl (typecheck* (mk-formula-decl wid wform 'AXIOM) nil nil nil))
	 (sdecl (typecheck* (mk-formula-decl sid sform 'AXIOM) nil nil nil)))
    (add-decl wdecl)
    (add-decl sdecl)))

(defmethod definition-body ((decl const-decl))
  (definition-body (beta-reduce (definition decl))))

(defmethod definition-body ((ex lambda-expr))
  (definition-body (expression ex)))

(defmethod definition-body ((ex expr))
  ex)

(defun fixed-inductive-variables (decl vars)
  (let ((fvars (copy-list vars)))
    (mapobject #'(lambda (ex)
		   (when (partial-inductive-occurrence?
			  ex (find-supertype (type decl)) decl)
		     (let ((indargs (ind-def-arguments ex)))
		       (mapc #'(lambda (v a)
				 (unless (and (typep a 'name-expr)
					      (eq (declaration a) v))
				   (setq fvars (delete v fvars))))
			     vars indargs)
		       (dolist (v (nthcdr (length indargs) vars))
			 (setq fvars (delete v fvars))))
		     t))
	       (definition decl))
    fvars))

(defun ind-def-formals (decl)
  (apply #'append
	 (append (formals decl)
		 (ind-def-formals* (beta-reduce (definition decl))))))

(defmethod ind-def-formals* ((ex lambda-expr))
  (append (list (bindings ex))
	  (ind-def-formals* (expression ex))))

(defmethod ind-def-formals* (ex)
  (declare (ignore ex))
  nil)

(defmethod ind-def-arguments (ex)
  (declare (ignore ex))
  nil)

(defmethod ind-def-arguments ((ex application))
  (append (ind-def-arguments (operator ex)) (arguments ex)))

(defun weak-induction-hypothesis (nvar fixed-vars rem-vars expr decl)
  (if (null rem-vars)
      (if (inductive-decl? decl)
	  (make-ind-implication (subst-pred-for-ind nvar fixed-vars expr decl) nvar)
	  (make-ind-implication nvar (subst-pred-for-ind nvar fixed-vars expr decl)))
      (make-forall-expr rem-vars
	(if (inductive-decl? decl)
	    (make-ind-implication
	     (subst-pred-for-ind nvar fixed-vars expr decl)
	     (make-application* nvar (mapcar #'make-variable-expr rem-vars)))
	    (make-ind-implication
	     (make-application* nvar (mapcar #'make-variable-expr rem-vars))
	     (subst-pred-for-ind nvar fixed-vars expr decl))))))

(defun make-ind-implication (x y)
  (make-ind-implication* (type x) x y))

(defmethod make-ind-implication* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-forall-expr (list nbd)
      (make-ind-implication*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-implication* ((te subtype) x y)
  (make-ind-implication* (supertype te) x y))

(defmethod make-ind-implication* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make-implication x y))

(defun subst-pred-for-ind (nvar fixed-vars expr decl)
  (let ((*generate-tccs* 'none))
    (gensubst expr
      #'(lambda (ex)
	  (typecase ex
	    (application
	     (let ((rem-vars (remove-if #'(lambda (x)
					    (and (typep x 'name-expr)
						 (memq (declaration x)
						       fixed-vars)))
			       (apply #'append (arguments* ex)))))
	       (if (null rem-vars)
		   (copy nvar)
		   (make-application* nvar rem-vars))))
	    (name-expr (copy nvar))))
      #'(lambda (ex)
	  (typecase ex
	    (application (let ((op (operator* ex)))
			   (and (typep op 'name-expr)
				(eq (declaration op) decl))))
	    (name-expr (eq (declaration ex) decl)))))))

(defun strong-induction-hypothesis (nvar fixed-vars rem-vars expr decl)
  (let ((*generate-tccs* 'none))
    (if (null rem-vars)
	(if (inductive-decl? decl)
	    (make-ind-implication
	     (subst-strong-pred-for-ind nvar fixed-vars expr decl)
	     nvar)
	    (make-ind-implication
	     nvar
	     (subst-strong-pred-for-ind nvar fixed-vars expr decl)))
	(make-forall-expr rem-vars
	  (if (inductive-decl? decl)
	      (make-ind-implication
	       (subst-strong-pred-for-ind nvar fixed-vars expr decl)
	       (make-application* nvar (mapcar #'make-variable-expr rem-vars)))
	      (make-ind-implication
	       (make-application* nvar (mapcar #'make-variable-expr rem-vars))
	       (subst-strong-pred-for-ind nvar fixed-vars expr decl)))))))

(defun subst-strong-pred-for-ind (nvar fixed-vars expr decl)
  (let ((stype (find-supertype (type decl))))
    (gensubst expr
      #'(lambda (ex)
	  (let ((fmls (collect-decl-formals decl)))
	    (typecase ex
	      (application
	       (make-inductive-conjunction
		(copy ex)
		(make-ind-pred-application nvar ex fixed-vars fmls)
		(argument* ex)
		fmls
		(inductive-decl? decl)))
	      (name-expr
	       (make-inductive-conjunction ex nvar nil fmls
					   (inductive-decl? decl))))))
      #'(lambda (ex)
	  (typecase ex
	    (application
	     (partial-inductive-occurrence? ex stype decl))
	    (name-expr
	     (eq (declaration ex) decl)))))))

(defun make-ind-pred-application (pred ex fixed-vars fmls)
  (make-ind-pred-application* pred (argument* ex) fixed-vars fmls))

(defun make-ind-pred-application* (pred args fixed-vars formals
					&optional pargs)
  (if (null args)
      (if pargs
	  (make!-application* pred pargs)
	  pred)
      (make-ind-pred-application*
       pred (cdr args) fixed-vars (cdr formals)
       (nconc pargs
	      (mapcan #'(lambda (arg fml)
			  (unless (memq fml fixed-vars)
			    (list arg)))
		(if (and (cdr (car formals))
			 (tuple-expr? (car args)))
		    (exprs (car args))
		    (list (car args)))
		(car formals))))))

(defun collect-decl-formals (decl)
  (append (formals decl)
	  (lambda-bindings* (definition decl))))

(defmethod lambda-bindings* ((ex lambda-expr))
  (cons (bindings ex)
	(lambda-bindings* (expression ex))))

(defmethod lambda-bindings* (ex)
  (declare (ignore ex))
  nil)

(defun make-inductive-conjunction (inddef pred args fmls ind?)
  (cond ((null fmls)
	 (if ind?
	     (make-ind-conjunction inddef pred)
	     (make-ind-disjunction inddef pred)))
	(args
	 (make-inductive-conjunction inddef pred (cdr args) (cdr fmls) ind?))
	(t (let* ((bds (mapcar #'(lambda (x)
				   (typecheck* (mk-bind-decl (id x)
						 (or (declared-type x)
						     (type x)))
					       nil nil nil))
			 (car fmls)))
		  (bvars (mapcar #'make-variable-expr bds)))
	     (make!-set-expr bds
	       (make-inductive-conjunction
		(make!-application* inddef bvars)
		(make!-application* pred bvars)
		(cdr args)
		(cdr fmls)
		ind?))))))

(defun make-ind-conjunction (x y)
  (make-ind-conjunction* (type x) x y))

(defmethod make-ind-conjunction* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-lambda-expr (list nbd)
      (make-ind-conjunction*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-conjunction* ((te subtype) x y)
  (make-ind-conjunction* (supertype te) x y))

(defmethod make-ind-conjunction* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make!-conjunction x y))

(defun make-ind-disjunction (x y)
  (make-ind-disjunction* (type x) x y))

(defmethod make-ind-disjunction* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-lambda-expr (list nbd)
      (make-ind-disjunction*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-disjunction* ((te subtype) x y)
  (make-ind-disjunction* (supertype te) x y))

(defmethod make-ind-disjunction* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make!-disjunction x y))

(defun make-inductive-conclusion (var rem-vars decl)
  (let* ((dname (mk-name-expr (id decl) nil nil
			      (make-resolution decl
				(theory-name *current-context*)
				(type decl))))
	 (dargs (mapcar #'(lambda (blist)
			    (mapcar #'mk-name-expr blist))
			(append (formals decl)
				(all-lambda-bindings
				 (beta-reduce (definition decl))))))
	 (pargs (mapcar #'make-variable-expr rem-vars))
	 (impl (labels ((make-appl (op args)
		          (if (null args)
			      op
			      (make-appl (make-application* op (car args))
					 (cdr args)))))
		 (if (inductive-decl? decl)
		     (make-ind-implication
		      (make-appl dname dargs)
		      (if (null pargs)
			  (copy var)
			  (make-application* (copy var) pargs)))
		     (make-ind-implication
		      (if (null pargs)
			  (copy var)
			  (make-application* (copy var) pargs))
		      (make-appl dname dargs))))))
    (if (null rem-vars)
	impl
	(make-forall-expr rem-vars impl))))

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
  (let ((*generate-tccs* 'none))
    (setf (closed-definition decl)
	  (universal-closure (definition decl))))
  (when (eq (spelling decl) 'ASSUMPTION)
    ;;(remove-defined-type-names decl)
    ;; Check that no local decls are used in definition
    (let ((local-ref (find-local-theory-reference
		      (definition decl)
		      (formals-sans-usings (current-theory)))))
      (when local-ref
	(type-error local-ref
	  "Illegal reference to ~a~%May not use assumptions that reference ~
           entities declared in this theory."
	  local-ref)))
    (handle-existence-assuming-on-formals decl))
  decl)

(defun remove-defined-type-names (decl &optional (theory (current-theory)))
  (let ((refs (collect-references (definition decl))))
    (when (some #'(lambda (ref)
		    (and (type-def-decl? ref)
			 (eq (module ref) theory)))
		refs)
      ;;(change-class decl 'assuming-decl)
      ;;(setf (original-definition decl) (definition decl))
      (setf (definition decl)
	    (remove-defined-type-names* (definition decl) theory))
      (setf (closed-definition decl)
	    (remove-defined-type-names* (closed-definition decl) theory)))))

(defun remove-defined-type-names* (ex theory)
  (let ((*in-theory* theory))
    (declare (special *in-theory*))
    (gensubst ex
      #'remove-defined-type-name!
      #'remove-defined-type-name?)))

(defmethod remove-defined-type-name? ((ex type-expr))
  (declare (special *in-theory*))
  (and (type-name? (print-type ex))
       (let ((tdecl (declaration (print-type ex))))
	 (and (type-def-decl? tdecl)
	      (eq (module tdecl) *in-theory*)))))

(defmethod remove-defined-type-name! ((ex type-expr))
  (copy ex 'print-type nil))

(defmethod remove-defined-type-name? ((ex type-name))
  (declare (special *in-theory*))
  (let ((tdecl (declaration ex)))
    (and (type-def-decl? tdecl)
	 (eq (module tdecl) *in-theory*))))

(defmethod remove-defined-type-name! ((ex type-name))
  (copy-all (type-expr (declaration ex))))

(defmethod remove-defined-type-name? ((ex simple-decl))
  (declare (special *in-theory*))
  (and (type-name? (declared-type ex))
       (let ((tdecl (declaration (declared-type ex))))
	 (and (type-def-decl? tdecl)
	      (eq (module tdecl) *in-theory*)))))

(defmethod remove-defined-type-name! ((ex simple-decl))
  (declare (special *in-theory*))
  (copy ex
    'declared-type (remove-defined-type-names* (declared-type ex) *in-theory*)
    'type (remove-defined-type-names* (declared-type ex) *in-theory*)))

(defmethod remove-defined-type-name? (ex)
  (declare (ignore ex))
  nil)

(defun handle-existence-assuming-on-formals (decl)
  (when (and (typep (definition decl) 'exists-expr)
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
    (when (and (mod-id type)
	       (type-name? (print-type tval))
	       (not (mod-id (print-type tval))))
      (setq tval (copy tval
		   'print-type (copy (print-type tval)
				 'mod-id (mod-id type)))))
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
    (let ((*generate-tccs* 'none)
	  (typeslist (make-formals-type-app
		      (subst-mod-params (formals (declaration (type type)))
					(module-instance
					 (resolution (type type)))))))
      (set-type-for-application-parameters (parameters type) (car typeslist)))
    (let ((tval (substit te (pairlis (car (formals (declaration (type type))))
				     (parameters type)))))
      (unless (print-type tval)
	(setf (print-type tval) type))
      (when (or (nonempty-type-decl? (declaration (type type)))
		(nonempty? type))
	(setf (nonempty? tval) t)
	(setf (nonempty? type) t))
      tval)))

(defun set-type-for-application-parameters (parameters types)
  (when parameters
    (set-type (car parameters) (car types))
    (set-type-for-application-parameters
     (cdr parameters)
     (if (dep-binding? (car types))
	 (substit (cdr types)
	   (acons (car types) (car parameters) nil))
	 (cdr types)))))

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
						    (tc-eq (find-supertype
							    (range sty))
							   *boolean*)))))
		       (types (expr type)))
		     (prog1 (look-for-expected-from-conversion (expr type))
		       (setq used-a-conversion? t))))
	 (ftypes (remove-if-not #'fully-instantiated? ptypes))
	 (types (if (typep (expr type) 'name-expr)
		    (let* ((reses (resolutions (expr type)))
			   (bres (bound-variable-resolution reses))
			   (breses (if bres (list bres) reses))
			   (nreses (filter-local-resolutions
				   (remove-if-not
				       #'(lambda (r)
					   (some #'(lambda (fty)
						     (tc-eq fty (type r)))
						 ftypes))
				     breses))))
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
		   (tc-eq (find-supertype (range ftype)) *boolean*))
	(type-error (expr type) "Does not resolve to a predicate"))
      (set-type (expr type) ftype)
      (let ((tval (mk-subtype (domain ftype)
		    (expr type))))
	(setf (print-type tval) type)
	tval))))

(defmethod look-for-expected-from-conversion ((expr application))
  (and (typep (operator expr) 'name-expr)
       (typep (resolution (operator expr)) 'lambda-conversion-resolution)
       (list (k-conv-type (resolution (operator expr))))))

(defmethod look-for-expected-from-conversion (expr)
  (declare (ignore expr))
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
	   (setf (print-type tval) (print-type type))
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
    (typecheck* bindings nil nil nil)
    (unless (supertype type)
      (setf (supertype type)
	    (if (singleton? bindings)
		(type (car bindings))
		(make-tupletype-from-bindings bindings))))
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
	 (cdecl (declaration *current-context*))
	 (ndecl (typecheck* (mk-const-decl pname ftype nil nil)
			    nil nil nil)))
    (setf (declaration *current-context*) cdecl)
    (add-decl ndecl (not (formal-subtype-decl? decl)))
    (typecheck* pexpr ftype nil nil)
    (mk-subtype stype pexpr)))


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
    tuptype))

(defun typecheck-tuples (types result)
  (if (null types)
      (nreverse result)
      (let* ((ty (car types))
	     (ntype (typecheck* ty nil nil nil))
	     (*bound-variables* (if (dep-binding? ntype)
				    (cons ntype *bound-variables*)
				    *bound-variables*)))
	(typecheck-tuples (cdr types) (cons ntype result)))))

(defmethod typecheck* ((type cotupletype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((types (typecheck-tuples (types type) nil))
	 (cotuptype (if (equal types (types type))
			type
			(mk-cotupletype types))))
    cotuptype))


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
  (some #'(lambda (fld)
	    (some #'(lambda (fv)
		      (memq (declaration fv) fields))
		  (freevars (type fld))))
	fields))


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
  (let ((type (let ((*generate-tccs* 'none))
		(typecheck* (declared-type decl) nil nil nil))))
    (set-type (declared-type decl) nil)
    (setf (type decl) type))
  decl)


(defmethod typecheck* ((db dep-binding) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type db) (typecheck* (declared-type db) nil nil nil)))
  (set-type (declared-type db) nil)
  db)


;;; Judgements

(defmethod typecheck* :around ((decl judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (if (typechecked? decl)
      (add-judgement-decl decl)
      (call-next-method)))

(defmethod typecheck* ((decl subtype-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (subtype decl) (typecheck* (declared-subtype decl) nil nil nil))
  (set-type (declared-subtype decl) nil)
  (when (print-type (subtype decl))
    (setf (print-type (subtype decl))
	  (copy-judgement-subtype-without-types (declared-subtype decl))))
  (let ((*bound-variables*
	 (if (typep (declared-subtype decl) 'type-application)
	     (append (parameters (declared-subtype decl)) *bound-variables*)
	     *bound-variables*))
	(*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (if (subtype-of? (subtype decl) (type decl))
      (pvs-warning
	  "In judgement ~:[at~;~:*~a,~] Line ~d: ~a is already known to be a subtype of~%  ~a"
	(id decl) (line-begin (place decl))
	(declared-subtype decl) (declared-type decl))
      (let* ((bd (make-new-bind-decl (subtype decl)))
	     (bvar (make-variable-expr bd)))
	(setf (place bvar) (place (declared-subtype decl)))
	(unless (compatible? (subtype decl) (type decl))
	  (type-error decl
	    "~@[In judgement ~a: ~]subtype ~a is incompatible with ~a"
	    (id decl) (declared-subtype decl) (declared-type decl)))
	(let* ((*compatible-pred-reason*
		(acons bvar "judgement" *compatible-pred-reason*))
	       (incs (compatible-preds (subtype decl) (type decl) bvar)))
	  (cond (incs
		 (unless (eq *generate-tccs* 'none)
		   (generate-subtype-tcc bvar (type decl) incs))
		 (add-to-known-subtypes (subtype decl) (type decl)))
		(t (pvs-warning
		       "Subtype judgement is superfluous~@[ (on line ~d)~]:~
                        ~%  ~a"
		     (line-begin (place decl)) (unparse-decl decl))))))))

(defmethod copy-judgement-subtype-without-types ((te type-application))
  (lcopy te
    'parameters (mapcar #'(lambda (p)
			    (if (and (bind-decl? p)
				     (not (untyped-bind-decl? p)))
				(change-class (copy p) 'untyped-bind-decl)
				p))
		  (parameters te))))

(defmethod copy-judgement-subtype-without-types (te)
  te)
  

(defmethod typecheck* ((decl number-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (let ((*compatible-pred-reason*
	 (acons (number-expr decl) "judgement" *compatible-pred-reason*)))
    (typecheck* (number-expr decl) (type decl) 'expr nil))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod typecheck* ((decl name-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (when (funtype? (find-supertype (type decl)))
    (pvs-warning "Judgement at line ~d may lead to unprovable TCCs~
                  ~%See language document for details."
      (starting-row (place decl))))
  (set-type (declared-type decl) nil)
  (let ((*no-conversions-allowed* t)
	(*compatible-pred-reason*
	 (acons (name decl) "judgement" *compatible-pred-reason*)))
    (typecheck* (name decl) (type decl) nil nil))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod typecheck* ((decl application-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  (let ((fmlist (apply #'append (formals decl))))
    (let ((dup (duplicates? fmlist :key #'id)))
      (when dup
	(type-error dup
	  "May not use duplicate arguments in judgements")))
    (set-formals-types fmlist))
  (let* ((*bound-variables* (reverse (apply #'append (formals decl)))))
    (let ((*generate-tccs* 'none))
      (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (let* ((*no-conversions-allowed* t)
	   (expr (mk-application-for-formals (name decl) (formals decl)))
	   (*compatible-pred-reason*
	    (acons expr "judgement" *compatible-pred-reason*)))
      (unless (place expr)
	(setf (place expr) (place (name decl))))
      (typecheck* expr (type decl) nil nil)))
  (setf (judgement-type decl)
	(make-formals-funtype (formals decl) (type decl)))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod generic-judgement-warning ((decl number-judgement))
  (when (free-parameters (type decl))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the number."
      (id decl) (starting-row (place decl)) (id (module decl)))))

(defmethod generic-judgement-warning ((decl name-judgement))
  (unless (subsetp (free-params (type decl))
		   (free-params (name decl)))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the name ~a."
      (id decl) (starting-row (place decl)) (id (module decl)) (name decl))))

(defmethod generic-judgement-warning ((decl application-judgement))
  (unless (subsetp (free-params (type decl))
		   (free-params (name decl)))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the name ~a."
      (id decl) (starting-row (place decl)) (id (module decl)) (name decl))))


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


;;; Conversions

(defmethod typecheck* ((decl conversion-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (expr decl) nil nil nil)
    (mapobject #'(lambda (ex)
		   (when (and (expr? ex)
			      (not (type ex)))
		     (if (singleton? (types ex))
			 (setf (type ex) (car (types ex)))
			 (type-ambiguity ex))
		     nil))
	       (expr decl)))
  (check-conversion-applicability decl)
  (setf (k-combinator? decl) (k-combinator? (expr decl)))
  (let ((ndecl (if (fully-instantiated? (expr decl))
		   decl
		   (let ((th (module
			      (find-if #'(lambda (fp)
					   (not (memq fp (formals-sans-usings
							  (current-theory)))))
				(free-params (expr decl))))))
		     (copy decl 'module th)))))
    (push ndecl (conversions *current-context*)))
  decl)

(defun check-conversion-applicability (decl)
  (let* ((ctype (type (expr decl)))
	 (stype (find-supertype ctype)))
    (unless (typep stype 'funtype)
      (type-error (expr decl)
	"Conversion is not a function:~%  ~a: ~a" (expr decl) ctype))
    (when (strict-compatible? (domain stype) (range stype))
      (type-error (expr decl)
	"The domain and range of this conversion are compatible;~%~
         the conversion will never be used:~%  ~a: ~a" (name decl) ctype))
    ;; Note that conversions are similar to auto-rewrites, where the
    ;; free-params of a conversion correspond to the free variables of an
    ;; auto-rewrite.  This is a test that the LHS covers all the free
    ;; variables.
    (unless (fully-instantiated? (expr decl))
      (let* ((frees (remove-if #'(lambda (fp)
				   (memq fp (formals-sans-usings
					     (current-theory))))
		      (free-params (expr decl))))
	     (ftheory (module (car frees))))
	(unless (every #'(lambda (fp) (eq (module fp) ftheory))
		       (cdr frees))
	  (type-error (expr decl)
	    "Conversion expression has free parameters from different theories"
	    ))
	(unless (= (length frees) (length (formals-sans-usings ftheory)))
	  (type-error (expr decl)
	    "conversion does not determine all free parameters"))))))

(defmethod type ((decl conversion-decl))
  (type (expr decl)))

;;; Disabling conversions

(defmethod typecheck* ((decl conversionminus-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (expr decl) nil nil nil)
    (unless (singleton? (types (expr decl)))
      (type-ambiguity (expr decl)))
    (setf (type (expr decl)) (car (types (expr decl)))))
  (disable-conversion decl)
  decl)

(defun disable-conversion (decl)
  (let ((new-convs (remove-if #'(lambda (cd)
				  (tc-eq (expr decl) (expr cd)))
		     (conversions *current-context*))))
    (if (equal new-convs (conversions *current-context*))
		 ;;; Three possibilities in this case
	(let ((similar-convs
	       (remove-if-not #'(lambda (cd)
				  (same-declaration (expr decl)
						    (expr cd)))
		 (conversions *current-context*))))
	  ;; Don't need to do anything if the conversion doesn't
	  ;; appear.  Won't even complain since it may have been
	  ;; removed earlier by a similar declaration in a
	  ;; different theory.
	  (when similar-convs
	    ;; If we are removing the generic version, and
	    ;; instances are in the conversions list, simply
	    ;; remove them.
	    (if (not (fully-instantiated? (expr decl)))
		(setf (conversions *current-context*)
		      (remove-if #'(lambda (cd)
				     (memq cd similar-convs))
			(conversions *current-context*)))
		;; Otherwise, add it to the disabled-conversions list
		(pushnew decl (disabled-conversions *current-context*)
			 :test #'tc-eq :key #'expr))))
	(if (fully-instantiated? (expr decl))
	    (setf (conversions *current-context*) new-convs)
	    (setf (conversions *current-context*)
		  (if (name-expr? (expr decl))
		      (remove-if #'(lambda (cd)
				     (and (name-expr? (expr cd))
					  (same-declaration (expr decl)
							    (expr cd))))
			(conversions *current-context*))
		      (break)))))))
    

;;; auto-rewrite-decls

(defmethod typecheck* ((decl auto-rewrite-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (rewrite-names decl) nil nil nil))
  (pushnew decl (auto-rewrites *current-context*))
  decl)

(defmethod typecheck* ((decl auto-rewrite-minus-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (rewrite-names decl) nil nil nil))
  (pushnew decl (disabled-auto-rewrites *current-context*))
  decl)

(defmethod typecheck* ((rname rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((reses (formula-or-definition-resolutions rname)))
    (unless reses
      (resolution-error rname 'rewrite-name nil (not *in-checker*)))
    (setf (resolutions rname) reses))
  rname)

(defmethod typecheck* ((rname constant-rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type rname) (typecheck* (declared-type rname) nil nil nil)))
  (set-type (declared-type rname) nil)
  (let ((reses (definition-resolutions rname)))
    (unless reses
      (resolution-error rname 'rewrite-name nil (not *in-checker*)))
    (setf (resolutions rname) reses))
  rname)

(defmethod typecheck* ((rname formula-rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((reses (formula-resolutions rname)))
    (unless reses
      (resolution-error rname 'rewrite-name nil (not *in-checker*)))
    (setf (resolutions rname) reses))
  rname)

(defun check-duplication (decl)
  (when (projection? (id decl))
    (type-error decl "May not overload projection names"))
  (let* ((decls (gethash (id decl) (current-declarations-hash))))
    (mapc #'(lambda (d) (when (and (not (eq d decl))
				   (declaration? d)
				   (eq (module d) (current-theory)))
			  (duplicate-decls decl d)))
	  decls)))

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
	(unless (or (and (typep y 'type-def-decl)
			 (typep (type-expr y) 'recursive-type))
		    (and (typep y 'type-decl)
			 (not (types-with-same-signatures x y))))
	  (type-error x
	    "Name ~a already in use as a ~a" (id x) (kind-of x))))))

(defmethod types-with-same-signatures ((x type-decl) (y type-decl))
  (if (formals x)
      (and (formals y)
	   (= (length (car (formals x))) (length (car (formals y))))
	   (every #'(lambda (a b) (compatible? (type a) (type b)))
		  (car (formals x)) (car (formals y))))
      (null (formals y))))

(defun tc-unifiable? (x y)
  (declare (ignore x y))
  nil)
	

(defun set-prelude-types (id type)
  (case id
    (|boolean| (setq *boolean* type))
    (|number| (setq *number* type))
    (|number_field| (setq *number_field* type))
    (|real| (setq *real* type)
     (push-ignored-type-constraints *real*))
    (|rational| (setq *rational* type)
     (push-ignored-type-constraints *rational*))
    (|integer| (setq *integer* type)
     (push-ignored-type-constraints *integer*))
    (|naturalnumber| (setq *naturalnumber* type)
     (push-ignored-type-constraints *naturalnumber*))
    (|posint| (setq *posint* type))
    (|even_int| (setq *even_int* type))
    (|odd_int| (setq *odd_int* type))
    (|ordinal| (setq *ordinal* type))))
