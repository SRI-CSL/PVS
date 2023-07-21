;; -- PVS2Rust --
;; PVS2Rust translates PVS expressions, definitions, and theories to the Rust language. 
;; See my report for supported subset, or contact me : nathan[dot]gasc[at]gmx[dot]fr
;; Manual : https://carnets.binets.fr/szf64Md4QdqVJmGVLp6FEg?view

;; make mod per theory for export


(in-package :pvs)
(defvar *pvs2rust-preceding-theories* nil)
(defvar *ir-formal-consts* nil)
(defvar *functions* nil) ;; list of all fn names
;(defvar *output* nil) ;; datatype declaration must be re written to remove this
(defvar *header* nil) ;; rust code containing type, const, datatype & misc declarations
(defvar *type-defs* nil) ;; type, const & datatype names (as a list of str) 
(defvar *unique-rust-records* nil) ;; store the hashes of the records

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We first add the possible mutability of variables

(defcl ir-variable (ir-expr)
  ir-name
  ir-vtype
  ir-pvsid 
  ir-mutable) ; default to nil 

(defun mk-ir-variable (ir-name ir-type &optional ir-pvsid mutable) ; Can be kept 
  (make-instance 'ir-variable
		 :ir-name ir-name
		 :ir-vtype ir-type
		 :ir-pvsid ir-pvsid
		 :ir-mutable mutable))

;; Suppression of the null assignement, adding delete when needed
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-funtype) lhs rhs-irvar ir-exprvar bindings)
  (cond ((consp lhs)
	 (let* ((ir-exprvar11 (new-irvar))
		(ir-expr-type11 (ir-range ir-expr-type))
		(ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		(ir-rest-assignment (if (consp (cdr lhs))
					(pvs2ir-assignment1 ir-expr-type11
							    (cdr lhs)
							    rhs-irvar ir-expr-vartype11
							    bindings)
				      rhs-irvar))
		(lhs1 (caar lhs)))
	   (let ((lhs1-irvar (new-irvar))
		 (lhs1-ir (pvs2ir* lhs1 bindings nil)) ;;the index expression
		 (lhs1-ir-type (ir-domain ir-expr-type))
		 (ir-exprvar1 (new-irvar))
		 (ir-new-rhsvar (new-irvar))
		 (ir-var-post-delete (new-irvar)))
	     (let ((ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
		   (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		   (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11))
		   (ir-vartype-post-delete (mk-ir-variable ir-var-post-delete (ir-vtype ir-exprvar))))
	       (mk-ir-let ir-lhs1-vartype lhs1-ir
		   		(mk-ir-let ir-expr-vartype11
					(mk-ir-apply ir-exprvar (list ir-lhs1-vartype))
					(if (ir-reference-type? ir-expr-type11)
						(mk-ir-let ir-vartype-post-delete 
							(mk-ir-apply (mk-ir-primitive-function 'delete) (list ir-exprvar ir-lhs1-vartype))
							(mk-ir-let ir-new-rhsvartype
								ir-rest-assignment
								(mk-ir-update ir-vartype-post-delete ir-lhs1-vartype ir-new-rhsvartype)))
						(mk-ir-let ir-new-rhsvartype
							ir-rest-assignment
							(mk-ir-update ir-exprvar ir-lhs1-vartype ir-new-rhsvartype)))
					)) 
			))))
	(t ir-exprvar)))

;; Suppression of the null assignement
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-arraytype) lhs rhs-irvar ir-exprvar bindings)
	(cond ((consp lhs)
	 (let* ((ir-exprvar11 (new-irvar))
		(ir-expr-type11 (ir-range ir-expr-type))
		(ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		(ir-rest-assignment (if (consp (cdr lhs))
					(pvs2ir-assignment1 ir-expr-type11
							    (cdr lhs)
							    rhs-irvar ir-expr-vartype11
							    bindings)
				      rhs-irvar))
		(lhs1 (caar lhs)))
	   (let ((lhs1-irvar (new-irvar))
		 (lhs1-ir (pvs2ir* lhs1 bindings nil)) ;;the index expression
		 (lhs1-ir-type (ir-domain ir-expr-type))
		 (ir-exprvar1 (new-irvar))
		 (ir-new-rhsvar (new-irvar))
		 (ir-var-post-delete (new-irvar)))
	     (let ((ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
		   (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		   (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11))
		   (ir-vartype-post-delete (mk-ir-variable ir-var-post-delete (ir-vtype ir-exprvar))))
	       (mk-ir-let ir-lhs1-vartype lhs1-ir
		   		(mk-ir-let ir-expr-vartype11
					(mk-ir-lookup ir-exprvar ir-lhs1-vartype)
					(mk-ir-let ir-new-rhsvartype
						ir-rest-assignment
						(mk-ir-update ir-exprvar ir-lhs1-vartype ir-new-rhsvartype))
					)) 
			))))
	(t ir-exprvar))
)


(defmethod pvs2ir-assignment1 ((ir-expr-type ir-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (break "~% Should not be called anymore, see explanation in code.")) ;; GSC, see below

;; Why the function below ?
;; We always want recordtypes to be called behind their custom types (because we want to relate 
;; it to the correct struct in Rust). So we modified pvs2ir-assignement1 typename in order to 
;; call this function if it detects a record type behind a typename.
;; The typename is preserved.
;; With the new implementation, that may not be necessary anymore.
(defmethod pvs2ir-assignment1-custom-recordtype ((ir-expr-typename ir-typename) lhs rhs-irvar ir-exprvar bindings)
	(let* ((ir-expr-type (ir-type-defn ir-expr-typename)))
		(let* ((lhs1 (caar lhs)) ; i ;;lhs1 is a field-name-expr
			(lhs-exprvar (mk-ir-get ir-exprvar (id lhs1))) ; A[i] : get
			)
			(if (consp (cdr lhs))
				(pvs2ir-assignment1-custom-recordtype ir-expr-typename (cdr lhs) rhs-irvar lhs-exprvar bindings)
				(mk-ir-update 
					ir-exprvar
					(id lhs1)
					rhs-irvar
				)))))


;;I had to add method for ir-adt-recordtype since the type here is the adt and not the constructor,
;;whereas in the record case, the field-assign does not have a type so that ir-expr-type11 has to
;;be computed from the ir-type of the field.  
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-adt-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (cond ((consp lhs)
	   (let* ((lhs1 (caar lhs));;lhs1 is a field-name-expr
		  ;(ir-field-decl (find (id lhs1) (ir-field-types ir-expr-type) :key #'ir-id))
		  (ir-expr-type11 (pvs2ir-type (range (type lhs1)) bindings)) ;;(ir-ftype ir-field-decl)
		  (ir-exprvar11 (new-irvar))
		  (ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		  (ir-rest-assignment (if (consp (cdr lhs))
					  (pvs2ir-assignment1 ir-expr-type11 (cdr lhs)
							      rhs-irvar ir-expr-vartype11
							      bindings)
					rhs-irvar)))
	     (let* ((ir-exprvar1 (new-irvar))
		    (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		    (ir-new-rhsvar (new-irvar))
		    (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
	       (mk-ir-let ir-expr-vartype11
			  (mk-ir-get ir-exprvar (id lhs1));;directly get the field
			  (mk-ir-let ir-new-rhsvartype
						ir-rest-assignment
						(mk-ir-update ir-expr-vartype1 (id lhs1) ir-exprvar))))))
	  (t ir-exprvar)))


;; See explanation before pvs2ir-assignment1-custom-recordtype
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-typename) lhs rhs-irvar ir-exprvar bindings)
  (if (typep (ir-type-defn ir-expr-type) 'ir-recordtype)
	(pvs2ir-assignment1-custom-recordtype ir-expr-type lhs rhs-irvar ir-exprvar bindings)
	(pvs2ir-assignment1 (ir-type-defn ir-expr-type) lhs rhs-irvar ir-exprvar bindings)
  )
  )

;; I only changed the name of the recognzer function
(defun pvs2ir-adt-record-recognizer (rdecl index index-id index-type  
				    adt-type-name)
  (let* ((einfo (get-eval-info rdecl))
	 ;(ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
;    (or ir-einfo)
    (let* ((rid (pvs2ir-unique-decl-id rdecl))
	   (rargs (list (mk-ir-variable (new-irvar) adt-type-name)))
	   (index-expr (mk-ir-get (car rargs) index-id))
	   (index-expr-var (mk-ir-variable (new-irvar) index-type))
	   (index-var (mk-ir-variable (new-irvar) index-type))
	   (check-expr (mk-ir-apply (mk-ir-primitive-function '=) (list index-expr-var index-var)))
	   (rbody (mk-ir-let index-var (mk-ir-integer index)
			     (mk-ir-let index-expr-var index-expr
					check-expr)))
	   (recognizer-name (mk-ir-function (intern (format nil "~a" rid)) rdecl))
					;(rdefn (mk-ir-lambda rargs 'bool rbody))
	   )				;(break "recognizer")
      
      (setf (ir einfo)(mk-ir-defn recognizer-name rargs 'bool rbody)
	    (cdefn einfo) nil
	    (c-type-info-table einfo) nil)
      (ir einfo))))

;; I only changed the naming there
(defmethod pvs2ir-adt-accessor* ((adecl adt-accessor-decl) ; can also be kept : naming
				 constructor constructors index index-id adt-type-name)
  (declare (ignore index index-id constructors))
   (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 ;(ir-einfo (ir einfo))
	 (cinfo (get-eval-info (con-decl constructor)))
	 (ir-cinfo (ir cinfo))
	 (ctype (ir-constructor-type ir-cinfo))
	 (*var-counter* nil));(break "accessor*")
    (newcounter *var-counter*)
;    (or (ir einfo))
	(let* (;(aid (pvs2ir-unique-decl-id adecl))
	       (aargvar (mk-ir-variable (new-irvar) adt-type-name))
	       (accessor-ir-type (pvs2ir-type (range (type adecl))))
	       (cast-var (mk-ir-variable (new-irvar) ctype))
	       (project-expr (mk-ir-get cast-var adecl-id))
	       (abody (mk-ir-let cast-var aargvar
				 project-expr))
;	       (adefn (mk-ir-lambda (list aargvar) accessor-ir-type abody))
	       (new-value-var (mk-ir-variable (new-irvar) accessor-ir-type))
	       (accessor-name (mk-ir-function (format nil "~a__~a" (ir-type-id adt-type-name) adecl-id) adecl))
	       (update-name (mk-ir-function (format nil "~a__~a__update" (ir-type-id adt-type-name) adecl-id) adecl))
	       (ubody (mk-ir-let cast-var aargvar (mk-ir-constructor-update cast-var adecl-id new-value-var))))
	  (setf (ir einfo)
		(mk-ir-accessor-defn accessor-name (list aargvar) accessor-ir-type abody
				     (mk-ir-defn update-name (list aargvar new-value-var) ctype ubody))
		(cdefn einfo) nil (c-type-info-table einfo) nil (update-cdefn einfo) nil)
	  ;(format t "~%Adding definition for singular accessor: ~a" adecl-id)
	  (ir einfo))))

(defmethod pvs2ir-adt-accessor* ((adecl shared-adt-accessor-decl) ;idem
				 constructor constructors index index-id adt-type-name)
  (declare (ignore index constructor))
  (let* ((adecl-id (id adecl))
	 (einfo (get-eval-info adecl))
	 ;(ir-einfo (ir einfo))
	 (*var-counter* nil))
    (newcounter *var-counter*)
    ;(or ir-einfo)
    (let* ((acc-constructor-ids (constructors adecl))
	   (acc-constructor-index-decls (loop for cnstr in constructors
					      as idx from 0 
					      when (memq (id cnstr) acc-constructor-ids)
					      collect (cons idx (con-decl cnstr))))
					;(aid (pvs2ir-unique-decl-id adecl))
	   (aargvar (mk-ir-variable (new-irvar) adt-type-name))
	   (accessor-ir-type (pvs2ir-type (range (type adecl))))
	   (abody (pvs2ir-accessor-body adecl-id aargvar acc-constructor-index-decls index-id))
	   (new-value-var (mk-ir-variable (new-irvar) accessor-ir-type))
	   (accessor-name (mk-ir-function (format nil "~a__~a" (ir-type-id adt-type-name) adecl-id) adecl))
	   (update-name (mk-ir-function
			 (format nil "~a__~a__update" (ir-type-id adt-type-name)  adecl-id)
			 adecl))
	   (ubody (pvs2ir-accessor-update-body adecl-id aargvar new-value-var
					       acc-constructor-index-decls index-id)))
      (setf (ir einfo)
	    (mk-ir-accessor-defn accessor-name (list aargvar) accessor-ir-type abody
				 (mk-ir-defn update-name (list aargvar new-value-var) adt-type-name ubody)))
					;(format t "~%Adding definition for shared accessor: ~a" adecl-id)
      (ir einfo))))
	  
(defun pvs2ir-adt-decl (adt-decl);;only called with ir-type-value is empty ; only naming was changed
  (let* ((adt-type (type-value adt-decl))
	 (adt-adt-id (format nil "~a" adt-type))
	 (adt (adt adt-type))
	 (constructors (constructors adt))
	 (index-id (intern (format nil "~a_index" adt-adt-id)))
	 (index-type (mk-ir-subrange 0 (1- (length constructors))))
	 (adt-enum-or-record-type
	  (if (enumtype? adt)
	      index-type
	    (mk-ir-adt-recordtype (list (mk-ir-fieldtype index-id
		;; TODO Replace accessor unique decl by field type
		;; then do the required corrections in ir2c
		;; then use that to detect the used formals inside an ir-constructor (needed for constructor decl and call)
		;; use it for decl and call of datatype also
		;; OR use Phantom data in all constructors (easier but not beautifull)
							 index-type))
				  (loop for con in constructors
					collect (cons (pvs2ir-unique-decl-id (con-decl con))
						      (loop for acc in (acc-decls con)
							    collect (pvs2ir-unique-decl-id acc)))))))
	 (adt-type-name (mk-ir-typename adt-adt-id adt-enum-or-record-type nil nil adt-decl)));;need to add params/nil for now
    (push adt-type-name *ir-type-info-table*)
    (setf (ir-type-value adt-decl)
	  (mk-eval-type-info adt-type-name))
    (loop for constructor in constructors
	  as index from 0
	  do (pvs2ir-adt-constructor constructor index index-id index-type
				     adt-enum-or-record-type adt-type-name))
					;(break "before accessor")
    (unless (enumtype? adt)
      (loop for constructor in constructors
	    as index from 0
	    do (pvs2ir-adt-accessors (acc-decls constructor) constructor constructors
				     index index-id 
				     adt-type-name)))
    adt-type-name
    ))

;; Modified for reduction of a normal form
(defun pvs2ir-application (op args ir-expr-type bindings expected)
  (declare (ignore expected))
  (let* ((arg-names (new-irvars (length args)))
	 	 (args-ir (pvs2ir* args bindings nil))
	 	 (arg-types 
			(loop for arg in args
				as ir-arg in args-ir
				collect (let ((num (pvs-integer? arg)))
					(if num
					(mk-ir-subrange num num)
					(best-ir-subrange-pair (pvs2ir-expr-type arg bindings)
								(ir-arith-type ir-arg)))))) ;;NSH(3-20-16):
	 	 (op-arg-types arg-types) 
	 	 (ir-expr-type (ir-apply-op-type op op-arg-types ir-expr-type))
	 	 (apply-return-var (new-irvartype ir-expr-type))
		 )	
    (if (constant? op)
		(if (pvs2ir-primitive? op)
			(pvs2ir-primitive-application op arg-names op-arg-types args-ir ir-expr-type bindings)
		(let* ((opdecl (declaration op))
			(theory (module opdecl)))
			(if (memq (id theory) *primitive-prelude-theories*);;NSH(6-16-21)
			(progn ;(break "undefined primitive")
				(mk-ir-exit (format nil "Non-executable theory: ~a" (id theory)) "PVS2C_EXIT_ERROR"))
			(let* ((formals (formals-sans-usings (module opdecl)))
				(actuals (actuals (module-instance op))) ;;handling theory actuals
				(ref-actuals (loop for act in actuals ;;collect const actuals and ref actuals
						when (or (null (type-value act))
							(eq (check-actual-type (type-value act) bindings) 'ref))
						collect act))
				(ref-formals (if actuals ;if there are actuals, then collect only the type formals 
						(loop for fml in formals ;where the matching actual has a ref type
												;other non-ref type actuals are monomorphized
							as act in actuals ;;collect const actuals and ref actuals
							when (or (null (type-value act))
								(eq (check-actual-type (type-value act) bindings) 'ref))
							collect fml)
						formals)) ;otherwise, return all the formals. 
				(ir-formals (pvs2ir* ref-formals bindings nil))
				(ir-actuals (pvs2ir* ref-actuals bindings nil))
				(actvars (loop for fml in ir-formals
					as formal in ref-formals
					collect (mk-ir-variable (new-irvar)(ir-formal-type fml) (id formal))))
				(actual-types
				(loop for actual in ref-actuals
					as formal in ref-formals
					collect (if (formal-const-decl? formal)
						(pvs2ir-type (type (expr actual)) bindings)
						*type-actual-ir-name*)))
				(op-domain-vars (mk-variables-from-types (types (domain (find-supertype (type op)))) bindings))
				(op-ir (pvs2ir-constant-ir op bindings opdecl))
				(op-ir-function (ir-function-name op-ir))
				(op-ir-defn (ir-defn op-ir))
				(op-ir-args (ir-args op-ir))
				(op-arg-application-ir
				(make-ir-lett* op-domain-vars ; was arg-vartypes
						arg-types
						args-ir
						(mk-ir-let apply-return-var ;op-range-type
							(mk-ir-apply
							op-ir-function op-domain-vars
							actvars)
						;op-range-type
							apply-return-var)))
				)  ;(break "pvs2ir-application")
			(if formals
				(if ir-actuals ;;then generating code outside theory
				(make-ir-lett* actvars actual-types ir-actuals
						op-arg-application-ir)
				(make-ir-let* actvars ir-formals op-arg-application-ir));;generating code within theory
			
				(if (and op-ir-defn  args-ir (null op-ir-args))
				(let ((op-var (mk-ir-variable (new-irvar)(pvs2ir-type (type op) bindings))))
						;(break "pvs2ir-application")
				(make-ir-lett* op-domain-vars ;was arg-vartypes
						arg-types
						args-ir
						(make-ir-let op-var
								op-ir-function
								(mk-ir-let apply-return-var ;op-range-type
									(mk-ir-apply op-var op-domain-vars nil) ;op-range-type
									apply-return-var))))
				(make-ir-lett* op-domain-vars ;was arg-vartypes
						arg-types
						args-ir
						(mk-ir-let apply-return-var ;op-range-type
							(mk-ir-apply op-ir-function op-domain-vars nil) ;op-range-type
							apply-return-var))))))))
	(let* ((op-ir-type (pvs2ir-type (type op) bindings))
	       (op-var (new-irvartype op-ir-type))
	       (op-ir (pvs2ir* op bindings nil)); expected is nil 
	       (arg-vartypes (mk-variables-from-ir-domain-types op-ir-type nil))); (break "pvs2ir-application-2")
;;	  (when (not (eql (length arg-vartypes)(length arg-types))) (break "pvs2-ir-application: arg-vartypes: ~s, ~% argtypes: ~s" arg-vartypes arg-types))
	  (if (ir-array? op-ir-type)
	      (mk-ir-let op-var op-ir
			 (mk-ir-let (car arg-vartypes)(car args-ir)
				    (mk-ir-let apply-return-var ;op-range-type
					       (mk-ir-lookup op-var (car arg-vartypes))
					       apply-return-var)))
	      (if (ir-lambda? op-ir) ;;op-var is ignored, IR is beta-reduced
		  (with-slots (ir-vartypes ir-body) op-ir
		    (mk-ir-let* arg-vartypes args-ir
				(if (eql (length ir-vartypes)(length arg-vartypes))
				    (mk-ir-let* ir-vartypes arg-vartypes
						ir-body)
				    (if (eql (length arg-vartypes) 1)
					(let ((ir-projected-args
					       (loop for ir-vartype in ir-vartypes
						  as i from 1
						  collect (mk-ir-get (car arg-vartypes)
								     (intern (format nil "project_~a" i))))))

					  (mk-ir-let* ir-vartypes ir-projected-args ir-body))
					(let* ((ir-fields (loop for ir-vartype in arg-vartypes
							     as i from 1 
							     collect
							       (mk-ir-field (intern (format nil "project_~a" i))
									    ir-vartype)))
					       (ir-recordtype (ir-vtype (car ir-vartypes))))
					  (mk-ir-let (car ir-vartypes) (mk-ir-record ir-fields ir-recordtype)
						     ir-body))))))
		  (let* ((ir-var-vtype (with-slots (ir-name ir-vtype ir-pvsid) op-var ir-vtype)))
					(if (ir-funtype? ir-var-vtype) (with-slots (ir-domain ir-range) ir-var-vtype
						(if (typep ir-domain 'ir-recordtype) ;; in fact ir-tupletype
						(with-slots (ir-label ir-field-types) ir-domain 
							(let* ((record-arg-irvar (mk-ir-variable (new-irvar) ir-domain))
								(ir-fields (loop for ir-vartype in arg-vartypes
									as i from 1 
									collect
									(mk-ir-field (intern (format nil "project_~a" i))
											ir-vartype)))
								)
								(make-ir-let op-var op-ir 
									(make-ir-lett* arg-vartypes
										arg-types
										args-ir
										(mk-ir-let apply-return-var 
											(mk-ir-let record-arg-irvar (mk-ir-record ir-fields ir-domain) 
											(mk-ir-apply op-var (list record-arg-irvar) nil ))
											apply-return-var
										)
									)
								)
							))
						(make-ir-let op-var op-ir ;; arg is not record, we go the usual way
							(make-ir-lett* arg-vartypes
									arg-types
									args-ir
									(mk-ir-let apply-return-var ;op-range-type
										(mk-ir-apply op-var arg-vartypes nil) ;op-range-type
										apply-return-var)))
						)
					)(progn 
					(make-ir-let op-var op-ir ;; arg is not record, we go the usual way
							(make-ir-lett* arg-vartypes
									arg-types
									args-ir
									(mk-ir-let apply-return-var ;op-range-type
										(mk-ir-apply op-var arg-vartypes nil) ;op-range-type
										apply-return-var)))
					))
				)
		  
		  ))))))

;; ------- IR2RUST --------

(defmethod ir2rust* ((ir-expr ir-bool) return-type) 
  (with-slots (ir-boolval) ir-expr
    (if ir-boolval "true" "false")))

(defmethod ir2rust* ((ir-expr ir-exit) return-type) 
  (with-slots (ir-message ir-code) ir-expr 
    (format nil "panic!(\"~a\")" ir-message)))

(defmethod ir2rust* ((ir-expr ir-integer) return-type)
  (with-slots (ir-intval) ir-expr
	      (format nil "~ai64" ir-intval)))

(defmethod ir2rust* ((ir-expr ir-last) return-type)
  (with-slots (ir-var) ir-expr
    (with-slots (ir-name ir-vtype) ir-var ir-name))) ; no clone if last

(defmethod ir2rust* ((ir-expr ir-variable) return-type) 
  (with-slots (ir-name ir-vtype) ir-expr
    (if (typep ir-vtype 'ir-subrange) ir-name (format nil "~a.clone()" ir-name))))
		
(defmethod ir2rust* ((ir-expr ir-get) return-type) ; lookup of a record 
  (with-slots (ir-record ir-field) ir-expr
	      (let* ((ir-record-var (get-ir-last-var ir-record)))
            (if (ir-last? ir-record)
                (format nil "~a.~a" (ir-name ir-record-var) ir-field)
                (format nil "~a.~a.clone()" (ir-name ir-record-var) ir-field)))))

(defmethod ir2rust* ((ir-expr ir-lookup) return-type) ; lookup of an array 
  (with-slots (ir-array ir-index) ir-expr
	      (let* ((ir-array-var (get-ir-last-var ir-array)))
            (format nil "~a.lookup(~a)" (ir-name ir-array-var) (ir2rust* ir-index (mk-ir-subrange 0 0))))))

(defmethod ir2rust* ((ir-expr ir-apply) return-type) ; lookup of a function, can we have a apply on array ? no : only closures & primitives
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr 
	      (let* ((ir-func-var (get-ir-last-var ir-func))
		        (hi-array (when (ir-variable? ir-func-var) (ir-array? (ir-vtype ir-func-var)))))
          (if hi-array (break (format nil "Trying to call an array : ~a" ir-func)))
	      	(if (ir-primitive-function? ir-func)
				(ir2rust-primitive-apply return-type ir-func ir-args)
				(let* (
					(rust-args (format nil "~{~a,~}" (concat-lists (loop for formal in *ir-formal-consts* collect (ir2rust* formal nil)) 
										(loop for ir-arg in ir-args collect (ir2rust* ir-arg nil)))))) ;; change rust args
					(if (ir-function? ir-func)  
						(format nil "~a(~a)" (ir-fname ir-func) rust-args)
						(let* ( ;; so this is a closure
							(closure-type (ir-vtype ir-func-var))
							(domain (ir-domain closure-type))
							(record-name (ir2rust-type domain))
							(rust-args (if (eql (length ir-args) 1)
								(format nil "~a" (ir2rust* (car ir-args) nil))
								(if (typep domain 'ir-recordtype)
									(format nil "~a {~{~a : ~a~},}" record-name ;; not sure that works
								  		(loop for ir-field-type in (ir-field-types domain)
											collect (ir-id ir-field-type))
										(loop for arg in ir-args
											collect (ir2rust* arg nil)))
									(format nil "~{~a,~}" (loop for arg in ir-args collect
										(ir2rust* arg nil))))
							)))
							(format nil "~a.lookup(~a)" (ir-name ir-func-var) rust-args))
					))
				)
          )))

;(defun build-record-from-args (args)
;	(let* ((*hash* ""))
;		(loop for arg in args
;			collect (with-slots (ir-id ir-vtype) field 
;			(setf *hash* (format nil "~a~a~a" *hash* ir-id (ir2rust-type ir-vtype)))))
;		())
;)

(defun ir2rust-primitive-apply (return-type ir-function ir-args)
  (let* ((ir-function-name (when (ir-function? ir-function)(ir-fname ir-function))))
    (case ir-function-name
      (+ (format nil "~a + ~a" (ir2rust* (car ir-args) return-type) (ir2rust* (cadr ir-args) return-type)))
      (- (format nil "~a - ~a" (ir2rust* (car ir-args) return-type) (ir2rust* (cadr ir-args) return-type)))
      (* (format nil "~a * ~a" (ir2rust* (car ir-args) return-type) (ir2rust* (cadr ir-args) return-type)))
      ((/ ndiv) (format nil "~a / ~a" (ir2rust* (car ir-args) return-type) (ir2rust* (cadr ir-args) return-type)))
      (= (format nil "~a == ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      (/= (format nil "~a != ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      (nrem (format nil "~a % ~a" (ir2rust* (car ir-args) return-type) (ir2rust* (cadr ir-args) return-type)))
      (< (format nil "~a < ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
	  (<= (format nil "~a <= ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
	  (> (format nil "~a > ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
	  (>= (format nil "~a >= ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
    ;  (floor (list (format nil "~a = (~a_t)pvsfloor_~a_~a(~a)"
	;		   return-var (mppointer-type return-type)
	;		   (numtype-abbrev (car c-arg-types))
	;		   (numtype-abbrev return-type)
	;		   (car ir-arg-names))))
    ;  (ceiling (list (format nil "~a = (~a_t)pvsceiling_~a_~a(~a)"
	;		     return-var (mppointer-type return-type)
	;		     (numtype-abbrev (car c-arg-types))
	;		     (numtype-abbrev return-type)
	;		     (car ir-arg-names))))
    ;  (char? (list (format nil "~a = (~a_t)(~a < 0x110000)"
	;		   return-var return-type (car ir-arg-names))))
      (ord "0")
      ((NOT ¬) (format nil "! ~a" (ir2rust* (car ir-args) nil)))
      ((OR ∨) (format nil "~a || ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      ((AND & ∧) (format nil "~a && ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      ((IMPLIES => ⇒) (format nil "!(~a) || ~a" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      (WHEN (format nil "~a || !(~a)" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
    ;  ((IFF <=> ⇔) (list (format nil "~a = (~a || ! ~a) && ((!~a) ||  ~a)" return-var
	;		 (car ir-arg-names)  (cadr ir-arg-names)
	;		 (car ir-arg-names)  (cadr ir-arg-names))))
    ;  ((code char) (list (format nil "~a = (uint32_t) ~a" return-var (car ir-arg-names))))
	  (delete (format nil "~a.delete(~a)" (ir2rust* (car ir-args) nil) (ir2rust* (cadr ir-args) nil)))
      (t (list (format nil "~a(~{~a~^, ~})" ir-function-name ir-args)))
      )))

(defmethod ir2rust* ((ir-expr ir-let) return-type) ; later : change var name to pvsid ?
  (with-slots (ir-vartype ir-bind-expr ir-body) ir-expr 
    (with-slots (ir-name ir-vtype ir-pvsid ir-mutable) ir-vartype
        (if (is-var-type-actual ir-vartype) (ir2rust* ir-body return-type)
		(format nil "let~a ~a : ~a = {~a};~%~a" (if ir-mutable " mut" "") ir-name (ir2rust-type ir-vtype) (ir2rust* ir-bind-expr ir-vtype) (ir2rust* ir-body return-type)))  
    )))

(defmethod ir2rust* ((ir-expr ir-lett) return-type);;assume ir-bind-expr is an ir-variable ; TODO : complete
  (with-slots (ir-vartype ir-bind-type ir-bind-expr ir-body) ir-expr
	      (with-slots (ir-name ir-vtype ir-pvsid ir-mutable) ir-vartype
			  (let* ((rhs-var (get-ir-last-var ir-bind-expr))
			    (lhs-type (if (ir-typename? ir-vtype) (ir-type-defn ir-vtype) ir-vtype))
                (rust-rhs (rust-copy-type lhs-type rhs-var)))
			    (if rust-rhs 
					(format nil "let~a ~a : ~a = ~a;~a" (if ir-mutable " mut" "") ir-name (ir2rust-type ir-vtype) rust-rhs (ir2rust* ir-body return-type))
					(progn (setf (ir-name ir-vartype) (ir-name rhs-var))
						(ir2rust* ir-body return-type))
				)
			  ))))

(defmethod rust-copy-type ((lhs-vtype ir-subrange) rhs-var)
	(let* ((var-type (ir-vtype rhs-var))
		(rhs-vtype (if (typep var-type 'ir-typename) (ir-type-defn var-type) var-type)))
		(if (typep rhs-vtype 'ir-subrange)
			(ir-name rhs-var)
			(if (and (typep rhs-vtype 'symbol) (eq rhs-vtype 'mpq))
				(format nil "{~a}.into_inner() as i64" (ir-name rhs-var))
				(break (format nil "Cannot convert from ~a to ~a" (type-of rhs-vtype) (type-of lhs-vtype))))))
)

(defmethod rust-copy-type ((lhs-vtype ir-typename) rhs-var)
	(rust-copy-type (ir-type-defn lhs-vtype) rhs-var)
)

(defun rust-copy-fn-to-array (lhs-vtype rhs-var depth fn-name) ;; fn-name contains potential lookups
	(let* ((lhs-vtype-real (if (typep lhs-vtype 'ir-typename) (ir-type-defn lhs-vtype) lhs-vtype)))
	(with-slots (size high ir-domain ir-range) lhs-vtype-real 
		(if (typep (ir-range (ir-vtype rhs-var)) 'ir-funtype)
			(let* ((new-rhs-var (mk-ir-variable (format nil "tmp") (ir-range (ir-vtype rhs-var)) nil) )) ;; only for type checking
				(format nil "arraytype::new(Rc::new(move |j~a : usize| {let cl_var_~a = ~a.clone(); ~a}))" ;; the clone is required by the move
					depth depth (if (= depth 1) (ir-name rhs-var) (format nil "cl_var_~a" (- depth 1)))
					(rust-copy-fn-to-array ir-range new-rhs-var (+ depth 1) (format nil "~a.lookup(j~a as i64)" fn-name depth)))
			)
			(format nil "arraytype::new(Rc::new(move |j~a : usize| ~a~a.lookup(j~a as i64)))" 
					depth (if (= depth 1) (ir-name rhs-var) (format nil "cl_var_~a" (- depth 1))) 
					fn-name depth))
	))
)

(defmethod rust-copy-type ((lhs-vtype ir-arraytype) rhs-var)
	(with-slots (size high ir-domain ir-range) lhs-vtype 
		(if (typep (ir-vtype rhs-var) 'ir-funtype)
			(rust-copy-fn-to-array lhs-vtype rhs-var 1 "")
		(break "Cannot convert from non funtype to array, yet."))
		
	)
)

(defmethod rust-copy-type ((lhs-vtype symbol) rhs-var)
	(if (eq lhs-vtype 'mpq)
		(let* ((rhs-vtype (ir-vtype rhs-var)))
			(format nil "NotNan::new(~a as f32).unwrap()" (ir-name rhs-var))
		)
	(break "Cannot convert to this symbol, yet."))
)

(defmethod rust-copy-type ((lhs-vtype t) rhs-var)
	(let* ((rhs-vtype (ir-vtype rhs-var)))
		(break (format nil "Cannot convert from ~a to ~a" (type-of rhs-vtype) (type-of lhs-vtype))))
)

(defmethod ir2rust* ((ir-expr ir-lambda) return-type) ; only called at non-zero level
  (with-slots (ir-vartypes ir-lastvars ir-rangetype ir-body) ir-expr 
    (let* ((args-rust (format nil "~{~a,~}" 
			(loop for arg in ir-vartypes
			collect (format nil "~a~a : ~a" (if (ir-mutable arg) "mut " "") (ir-name arg) (ir2rust-type (ir-vtype arg))))))
           (body-rust (ir2rust* ir-body ir-rangetype)))
        (format nil "funtype::new(Rc::new(move |~a| {~a}))" args-rust body-rust) ; rust infers automatically the types
    )))


(defmethod ir2rust* ((ir-expr ir-ift) return-type)
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (let ((rust-then-instrs (ir2rust* ir-then return-type))
		    (rust-else-instrs (ir2rust* ir-else return-type)))
		(format nil "if ~a {~a} else {~a}" (ir-name (get-ir-last-var ir-condition)) rust-then-instrs rust-else-instrs))))

(defmethod ir2rust* ((ir-expr ir-release) return-type)
  (with-slots (pre-ir-vars post-ir-vars ir-body) ir-expr
		(let ((rust-body (ir2rust* ir-body return-type))
		    (pre-release-instrs (format nil "~{drop(~a);~%~}" (loop for var in pre-ir-vars when (not (is-var-type-actual var)) collect (ir-name var))))
		    ;;(post-release-instrs (format nil "~{drop(~a);~%~}" (loop for var in post-ir-vars collect (ir-name var))))
			) ;; no post release rn
		(format nil "~a~a" pre-release-instrs rust-body)))) ;; are they post release ? 


(defmethod ir2rust* ((ir-expr ir-update) return-type) 
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr
    (let* ((target-var (get-ir-last-var ir-target))
            (target-type (if (ir-typename? (ir-vtype target-var)) (ir-type-defn (ir-vtype target-var)) (ir-vtype target-var)))
            (target-var-name (ir-name target-var))
            )
    (case (type-of target-type)
        ('ir-arraytype (with-slots (size high ir-domain ir-range) target-type
            (let* ((rhs-rust (ir2rust* ir-rhs ir-range))
                    (lhs-rust (ir2rust* ir-lhs ir-domain))) ; add the required mutability
                (format nil "~a.update(~a, ~a)" target-var-name lhs-rust rhs-rust)
            )))
        ('ir-funtype (with-slots (ir-domain ir-range) target-type
            (let* ((target-var-rust (ir2rust* ir-target target-type))
                    (rhs-rust (ir2rust* ir-rhs ir-range))
                    (lhs-rust (ir2rust* ir-lhs ir-domain)))
                (format nil "~a.update(~a, ~a)" target-var-rust lhs-rust rhs-rust)
            )))
        ('ir-recordtype (with-slots (ir-label ir-field-types) target-type
            (let* ((target-var-rust (ir2rust* target-var target-type))
                    (rhs-rust (ir2rust* ir-rhs nil)))
				(setf (ir-mutable target-var) t)
                (format nil "~a.~a = ~a; ~a" target-var-rust ir-lhs rhs-rust target-var-name)
            )))
        ('ir-adt (break (format nil "~%TODO. irlhs ~a ir-rhs ~a" (print-ir ir-lhs) (print-ir ir-rhs))))
        (t (break (format nil "~%Cannot update. Invalid update type ~a" (type-of target-type))))
    ))
  )
)

(defmethod ir2rust* ((ir-expr ir-record) return-type)
	(with-slots (ir-fields ir-recordtype) ir-expr 
		(let* ((record-name (ir2rust-type ir-recordtype))
			(corrected-name (replace-all record-name "<" "::<"))
			(field-value-list (loop for field in ir-fields
								collect (format nil "~a : ~a," (ir-fieldname field) (ir2rust* (ir-value field) nil)))) ;we can give the type if required
			(fields-values (format nil "~{~a~}" field-value-list)))
			(format nil "~a {~a}" corrected-name fields-values)
		)))

(defmethod ir2rust* ((ir-expr ir-formal-typename) return-type)
  (format nil "~a" (ir-type-id ir-expr)))

(defmethod ir2rust* ((ir-expr ir-function) return-type) 
  (format nil "~a()" (ir-fname ir-expr)))

;; Missing offset, forall, exists, & actuals
(defmethod ir2rust* ((ir-expr t) return-type)
  (break (format nil "IR expr ~a cannot be translated into Rust: ~%~a" ir-expr (type-of ir-expr))))



;; -------- PVS2RUST ---------

(defun pvs2rust-preceding-theories (theory)
  (let ((*pvs2rust-preceding-theories* nil)
	(theory-defn (get-theory theory)))
    (pvs2rust-preceding-theories* theory-defn)
      *pvs2rust-preceding-theories*))

(defmethod pvs2rust-preceding-theories* ((theory datatype))
  (with-slots (adt-theory adt-map-theory adt-reduce-theory all-imported-theories) theory
    (unless (eq (all-imported-theories theory) 'unbound)
      (loop for thy in all-imported-theories
	    do (pvs2rust-preceding-theories* thy)))
    ;pushing is enough since the importings are already pushed in above, otherwise we have a circularity
    (when adt-theory (pushnew adt-theory *pvs2rust-preceding-theories* :test #'same-id))
	))

(defmethod pvs2rust-preceding-theories* ((theory module))
  (unless (eq (all-imported-theories theory) 'unbound)
    (loop for thy in (all-imported-theories theory)
	  do (pvs2rust-preceding-theories* thy)))
  (unless (from-prelude? theory)
    (pushnew theory *pvs2rust-preceding-theories* :test #'same-id)))


(defun print-rust-file (theory-id text)
  (let* ((file-string (format nil "~a.rs"  theory-id))
	 (file-path (format nil "~a" (working-directory))))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output "//Code generated using pvs2ir2rust")
      (format output "
#![allow(
    non_snake_case,
    dead_code,
    non_upper_case_globals,
    non_camel_case_types,
    unused_variables,
    unused_parens,
    unreachable_patterns,
    unused_imports,
    drop_copy
)]
mod pvs2rust {
    use fxhash::FxHasher;
    use ordered_float::NotNan;
    use std::borrow::Borrow;
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::hash::BuildHasherDefault;
    use std::hash::Hash;
    use std::mem::drop;
    use std::ops::Deref;
    use std::rc::Rc;

    pub fn Rc_unwrap_or_clone<T: Clone>(rc: Rc<T>) -> T {
        Rc::try_unwrap(rc).unwrap_or_else(|rc| (*rc).clone())
    }
    
    pub trait RegularOrd: Clone + PartialEq + Eq + Hash
    where
        Self: std::marker::Sized,
    {
    }
    
    impl<T> RegularOrd for T where T: Clone + PartialEq + Eq + Hash {}
    
    #[derive(Clone)]
    pub struct funtype<A: RegularOrd, V: RegularOrd> {
        explicit: Rc<dyn Fn(A) -> V>,
        hashtable: Rc<RefCell<HashMap<A, V, BuildHasherDefault<FxHasher>>>>,
    }
    
    impl<A: RegularOrd, V: RegularOrd> PartialEq for funtype<A, V> {
        fn eq(&self, other: &Self) -> bool {
            panic!(\"Can't test equality of two functions\")
        }
    }
    impl<A: RegularOrd, V: RegularOrd> Eq for funtype<A, V> {}
    
    impl<A: RegularOrd, V: RegularOrd> Hash for funtype<A, V> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            panic!(\"Can't have proper ordering for two functions\")
        }
    }
    
    impl<A: RegularOrd, V: RegularOrd> funtype<A, V> {
        pub fn new(explicit: Rc<dyn Fn(A) -> V>) -> funtype<A, V> {
            funtype {
                explicit,
                hashtable: Rc::new(RefCell::new(HashMap::default())),
            }
        }
        pub fn lookup(&self, a: A) -> V {
            match self.hashtable.deref().borrow().get(&a) {
                // explicit deref due to borrow method name collision
                Some(v) => v.clone(),
                None => (self.explicit)(a),
            }
        }
        pub fn delete(self, a: A) -> Self {
            match self.hashtable.borrow_mut().remove(&a) {
                Some(x) => {
                    drop(x);
                }
                None => {}
            };
            self
        }
        pub fn update(self, a: A, v: V) -> Self {
            self.hashtable.borrow_mut().insert(a, v);
            self
        }
    }
    
    #[derive(Clone, PartialEq, Eq)]
    pub struct arraytype<const N: usize, V: RegularOrd> {
        array: Rc<RefCell<[V; N]>>,
    }
    
    impl<const N: usize, V: RegularOrd> Hash for arraytype<N, V> {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.array.deref().borrow().hash(state)
        }
    }
    
    impl<const N: usize, V: RegularOrd> arraytype<N, V> {
        pub fn new(explicit: Rc<dyn Fn(usize) -> V>) -> arraytype<N, V> {
            arraytype {
                array: Rc::new(RefCell::new(core::array::from_fn::<_, N, _>(
                    explicit.deref(),
                ))),
            }
        }
        pub fn lookup(&self, a: i64) -> V {
            self.array.deref().borrow()[a as usize].clone()
        }
        pub fn delete(self, a: i64) -> Self {
            self
        }
        pub fn update(self, a: i64, v: V) -> Self {
            self.array.borrow_mut()[a as usize] = v;
            self
        }
    }
}
~%"  )
      (format output "~%mod ~a {~%use ordered_float::NotNan;use std::rc::Rc;use crate::pvs2rust::*;~%~a~%~%~a~%}~%~%//fn main(){}" 
	  theory-id *header* text)
      ;(prettier-rust-file file-string)
      (format t "~%Wrote ~a" file-string)
		;;(excl:run-shell-command ; not working, i don t know why
		;;	     (format nil "prettier --write ~a" file-string)
		;;	     :input "//dev//null"
		;;	     :output nil
		;;	     :error-output :output)
    )

    (concatenate 'string file-path file-string)
    ))

(defun concatString (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

;; Entry point
(defun pvs2rust (theoryref)
	(let* ((theory (get-typechecked-theory theoryref nil t))
		(force? t))
		(with-workspace theory
				(pvs2rust-theories (pvs2rust-preceding-theories theory) force?))))

(defun pvs2rust-theories (theories force?)
  (setf *header* "")
  (setf *unique-rust-records* (cons "" nil))
  (setf *type-defs* (cons "" nil))
  (mapcar #'(lambda (th) (pvs2rust-theory* th force?)) (reverse theories)))

(defun pvs2rust-theory* (theory &optional force?)
  (let* (
	(*datatypes* nil) ;; list of datatypes names
  ) (with-workspace theory
		  (pvs2rust-theory-body theory force?))
  )
)

(defun pvs2rust-theory-body (theory &optional force? indecl);;*theory-id* needs to be bound by caller
  (let* ((*theory-id* (simple-id (id theory))))
    (pvs2rust-theory-body-step theory force? indecl)))

(defun pvs2rust-theory-body-step (theory force? indecl)
  (let* ((*preceding-mono-theories* nil) ; monomorphised theory instances used in this theory
	 (*theory-formals* (formals-sans-usings theory))
	 (*ir-type-info-table* nil)
	 (*rust-functions* nil)
	 (*theory-type-formals* (loop for formal in *theory-formals* when  (formal-type-decl? formal) collect formal))
	 (*ir-theory-formals* (loop for formal in *theory-formals*
				    do (let ((*current-context* (decl-context formal))
					     (*var-counter* nil))
					 (newcounter *var-counter*)
					 (pvs2ir-decl* formal))
				    collect (cond ((formal-const-decl? formal)
						   (ir-defn (ir (eval-info formal))))
						  ((formal-type-decl? formal)
						   (mk-ir-const-formal (ir-type-id (ir-type-name (ir-type-value formal)))
								       *type-actual-ir-name*)))))
	 (*ir-theory-tbindings* (pairlis *theory-formals* *ir-theory-formals*))
	 )
    (let* ((outputs (loop for decl in (theory theory)
	    when (if (null indecl)
		     (or (const-decl? decl) 
			 (type-eq-decl? decl)(type-decl? decl)(adt-type-decl? decl))
		     (eq decl indecl))
	    collect (pvs2rust-decl decl force?)
	))
    (text (concatString outputs)))
    (print-rust-file *theory-id* text)
    )))

(defun pvs2rust-decl (decl force?) ;; Variable non-global but temporal scope
  (let ((*current-context* (decl-context decl))
	(*var-counter* nil)
	(*pvs2c-defn-actuals* nil)
	)
    (when force? (clear-decl decl))
    (newcounter *var-counter*)
    (pvs2rust-decl* decl)))

(defmethod pvs2rust-decl* ((decl type-eq-decl))
  (let ((typename (pvs2ir-decl decl)))
    (when  (and (ir-typename? typename) (ir-type-value decl))
      (setf *header* (format nil "~a~%type ~a = ~a;~%" *header* (ir-type-id typename) (ir-type-defn typename))))
	""))

(defmethod pvs2rust-decl* ((decl formal-type-decl))
	(break "formal-type-decl")
   (pvs2ir-decl decl))

(defmethod pvs2rust-decl* ((decl formal-const-decl))
	(break "formal-const-decl")
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl))))
    (ir2c-decl* ir decl)))
  
;;BEGIN CONST DECL
(defmethod pvs2rust-decl* ((decl const-decl))
    (pvs2ir-decl decl)
    (let ((ir (ir (eval-info decl))))
        (ir2rust-decl* ir decl))
)

(defmethod ir2rust-decl* ((ir ir-accessor-defn) decl)
  (format t "~%Skipping accessor : ~a" decl)
  (format nil ""))

(defmethod ir2rust-decl* ((ir ir-defn) decl) ;; pvs return type = (type decl)
  (if (adt-recognizer-decl? decl) (progn (format t "~%Skipping recognizer : ~a" decl) "") 
	(with-slots (ir-function-name ir-return-type ir-args ir-defn) ir
        (if (null ir-defn) 
		(let* ((ir-f-name (ir-fname ir-function-name))
				(ir-result-type ir-return-type)
				(rust-result-type (ir2rust-type ir-result-type))
	        	(rust-args (format nil "~{~a~}" (loop for arg in ir-args
		    	       collect (if (is-var-type-actual arg) ""
					(format nil "~a~a: ~a, " (if (ir-mutable arg) "mut " "") (ir-name arg) (ir2rust-type (ir-vtype arg)))))))
				(rust-out (format nil "~%fn ~a (~a) -> ~a {exit(); // extern function}~%" ir-function-name rust-args rust-result-type)))
			(format t "~%Function ~a" ir-f-name)
			(format t "~%IR : ~%~a" (print-ir ir))
			(format t "~%Rust : ~%~a" rust-out)
			rust-out)
		(let* ((ir-f-name (ir-fname ir-function-name))
	        (pre-ir (if ir-args
			    (mk-ir-lambda ir-args ir-return-type ir-defn)
			    ir-defn))
	        (post-ir (preprocess-ir pre-ir))

			(ir-body (if (ir-lambda? post-ir) ; OK
			    (ir-body post-ir)
			    post-ir))

			(print-asap (progn (format t "~%Function ~a ~a" ir-f-name ir-defn )
						(format t "~%IR : ~%~a" (print-ir ir-body))))

	        (ir-args (when (ir-lambda? post-ir) (ir-vartypes post-ir)))
			(pvs-return-type (type decl))

	        (ir-result-type (if (ir-lambda? post-ir)
				(ir-rangetype post-ir)
				(pvs2ir-type pvs-return-type)))

	        (rust-result-type (ir2rust-type ir-result-type)) 

			(ir-formal-types (loop for arg in *ir-theory-formals* when (is-var-type-actual arg) collect arg))
			(*ir-formal-consts* (loop for arg in *ir-theory-formals* when (not (is-var-type-actual arg)) collect arg))
			(rust-formal-types (format nil "<~{~a: RegularOrd + 'static,~}>" (loop for ft in ir-formal-types collect (ir-name ft))))

	        (rust-args (format nil "~{~a~}" (loop for arg in ir-args
		    	       collect (if (is-var-type-actual arg) ""
					(format nil "~a~a: ~a, " (if (ir-mutable arg) "mut " "") (ir-name arg) (ir2rust-type (ir-vtype arg)))))))

	        (rust-body (ir2rust* ir-body ir-result-type))

			(rust-fn (format nil "pub fn ~a~a (~a) -> ~a {~a}~%~%" ir-f-name rust-formal-types
				rust-args rust-result-type rust-body)))
			(format t "~%Rust : ~%~a" rust-fn)
            rust-fn
	    ))
      ))
)

(defmethod ir2rust-decl* ((ir ir-constructor-defn) decl)
  (format t "~%Skipping constructor : ~a" decl)
  (format nil ""))

;; END CONST DECL

(defmethod pvs2rust-decl* ((decl type-decl)) ;; TODO : rewrite in a more LISP way
  (if (adt-type-decl? decl)
	(let* (
		(name (type-value decl))
		(thname (intern (format nil "~a" *theory-id*)))
		(adt-type (type-value decl))
		(adt (adt adt-type))
		(constructors (constructors adt))
		(ir-formal-types (loop for arg in *ir-theory-formals* when (is-var-type-actual arg) collect arg))
		(rust-formal-types (format nil "<~{~a: RegularOrd + 'static,~}>" (loop for ft in ir-formal-types collect (ir-name ft))))
		(rhs-generics (format nil "<~{~a,~}>" (loop for ft in ir-formal-types collect (ir-name ft)))) ;; <btree__t>
		(*output* "")
	) 
    (setf *functions* (cons "" nil))
	(setf *type-defs* (cons (format nil "~a" name) *type-defs*))

	;; --- ENUM CONSTRUCTION ---
	(setf *output* (format nil "~a~%#[derive(Clone, PartialEq, Eq, Hash)]~%pub enum ~a~a{" *output* name rust-formal-types))
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(setf *output* (format nil "~a~%~a(~a~a)," *output* cname cname rhs-generics))
		)
	)
	(setf *output* (format nil "~a~%}~%" *output*))

	;; --- STRUCTS CONSTRUCTION ---
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn 
				(setf *output* (format nil "~a~%#[derive(Clone, PartialEq, Eq, Hash)]~%pub struct ~a~a {" *output* cname rust-formal-types))
				(loop for accessor in (acc-decls constructor)
					collect (progn 
                    (setf *output* (format nil "~a~%~a: Rc<~a>," *output* (decl-id accessor) (ir2rust-type (pvs2ir-type (range (type accessor)))))))
				)
                (setf *output* (format nil "~a~%}~%" *output* cname))
			)
		)
	)

	;; --- CONSTRUCTORS --- 
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn 
				(setf *output* (format nil "~a~%pub fn ~a__~a~a(" *output* name cname rust-formal-types))
				(loop for accessor in (acc-decls constructor)
					collect (setf *output* (format nil "~a~a : ~a, " *output* (decl-id accessor) (ir2rust-type (pvs2ir-type (range (type accessor))))))
				)
				(setf *output* (format nil "~a) -> ~a~a {~%" *output* name rhs-generics))
				(setf *output* (format nil "~a~a::~a(~a{" *output* name cname cname))
				(loop for accessor in (acc-decls constructor)
					collect (setf *output* (format nil "~a~a: Rc::new(~a), " *output* (decl-id accessor) (decl-id accessor)))
				)
				(setf *output* (format nil "~a})~%}~%" *output*))
                (setf *functions* (cons (format nil "~a__~a" name cname) *functions*))
			)
		)
	)

	;; --- RECOGNIZERS ---
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn
				(setf *output* (format nil "~a~%pub fn ~a__~ap~a(arg : ~a~a) -> bool {~%match arg {~%" *output* thname cname rust-formal-types name rhs-generics))
				(loop for constructor2 in constructors
					collect (let* ((cname2 (decl-id (con-decl constructor2))))
						(if (eq cname cname2)
							(setf *output* (format nil "~a~a::~a(_) => true,~%" *output* name cname2))
							(setf *output* (format nil "~a~a::~a(_) => false,~%" *output* name cname2))
						)
					)
				)
				(setf *output* (format nil "~a}~%}~%" *output*))
                (setf *functions* (cons (format nil "~a__~ap" name cname) *functions*))
			)
		)
	)

	;; --- ACCESSORS ---
    ; It is very important that same accessor name for different constructors implies that the accessor types are the same
    ; This is not enforced by the compiler and thus an error can appear in the rust code.
    ; There is the same issue in PVS2C.
    (loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn
				(loop for accessor in (acc-decls constructor)
					collect (if (member (format nil "~a__~a" name (decl-id accessor)) *functions* :test #'equal)
                    nil ;; the function already exists
                    (progn
                        (setf *output* (format nil "~a~%pub fn ~a__~a~a(arg : ~a~a) -> ~a {~%match arg{" *output* name (decl-id accessor) rust-formal-types name rhs-generics (ir2rust-type (pvs2ir-type (range (type accessor))))))
                        (loop for constructor2 in constructors 
                            collect (let* ((cname2 (decl-id (con-decl constructor2))))
                            (if (member accessor (acc-decls constructor2))
                                (setf *output* (format nil "~a~%~a::~a(~a) => Rc_unwrap_or_clone(~a.~a)," 
                                                *output* name cname2 cname2 cname2 (decl-id accessor)))
                            )
                            )
                        )
                        (setf *output* (format nil "~a~%_ => unreachable!()~%}~%}~%" *output*))
                        (setf *functions* (cons (format nil "~a__~a" name (decl-id accessor)) *functions*))
                    )
                    )
				)
			)
		)
	)

    ;; --- UPDATE FUNCTIONS ---
    (loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn
				(loop for accessor in (acc-decls constructor)
					collect (if (member (format nil "~a__~a__update" name (decl-id accessor)) *functions* :test #'equal)
                    nil ;; the function already exists
                    (progn
                        (setf *output* (format nil "~a~%pub fn ~a__~a__update~a(arg : ~a~a, ~a : ~a) -> ~a~a {~%match arg{" *output* 
							name (decl-id accessor) rust-formal-types name rhs-generics (decl-id accessor) (ir2rust-type (pvs2ir-type (range (type accessor)))) name rhs-generics))
                        (loop for constructor2 in constructors 
                            collect (let* ((cname2 (decl-id (con-decl constructor2)))) ; WIP
                            (if (member accessor (acc-decls constructor2))
                                (progn
                                    (setf *output* (format nil "~a~%~a::~a(~a) => ~a::~a(~a{" 
                                                *output* name cname2 cname2 name cname2 cname2))
                                    (loop for accessor2 in (acc-decls constructor2)
                                        collect (if (eq (decl-id accessor2) (decl-id accessor))
                                            (setf *output* (format nil "~a~a: Rc::new(~a)," *output* (decl-id accessor2) (decl-id accessor)))
                                            (setf *output* (format nil "~a~a: ~a.~a," *output* (decl-id accessor2) cname2 (decl-id accessor2)))
                                        )
                                    )
                                    (setf *output* (format nil "~a})," *output*))
                                )
                            )
                            )
                        )
                        (setf *output* (format nil "~a~%_ => unreachable!()~%}~%}~%" *output*))
                        (setf *functions* (cons (format nil "~a__~a__update" name (decl-id accessor)) *functions*))
                    )
                    )
				)
			)
		)
	)
	*output*)
  (break "Type decl is not adt-type-decl")))

;; IR 2 RUST TYPES , takes ir-type returns string, it also adds the type definition to *header* if needed

(defmethod ir2rust-type ((ir-typ ir-subrange))
  (format nil "i64"))

(defmethod ir2rust-type ((ir-type symbol)) ;not. mpq
  (case ir-type
    (boolean "bool")
    (mpq "NotNan<f32>")
    (t ir-type)))

(defmethod ir2rust-type ((ir-type ir-typename)) ; if recordtype build struct, else makes a type declaration
  (with-slots (ir-type-id ir-type-defn type-declaration ir-actuals) ir-type
	(let* ((used-formals (remove-duplicates (used-type-formals ir-type-defn nil) :test 'string=))
			(formals-rust-defs (if (consp used-formals) (format nil "<~{~a : RegularOrd + 'static,~}>" used-formals) ""))
			(formals-rust (if (consp used-formals) (format nil "<~{~a,~}>" used-formals) "")))
		(if (member ir-type-id *type-defs* :test 'string=) nil
        (progn (setf *type-defs* (cons ir-type-id *type-defs*))
            (if (typep ir-type-defn 'ir-recordtype)
                (progn (setf *header* (format nil "~a~%#[derive(Clone, PartialEq, Eq, Hash)]~%pub struct ~a~a {~%" *header* ir-type-id formals-rust-defs))
                    (with-slots (ir-label ir-field-types) ir-type-defn
                        (loop for field in ir-field-types 
                            collect (with-slots (ir-id ir-name ir-vtype) field 
                            (setf *header* (format nil "~a~%~a : ~a,~%" *header* ir-id (ir2rust-type ir-vtype))))
                        )
                    )
                    (setf *header* (format nil "~a~%}~%" *header*))
                )
				(let* ((inner-type (ir2rust-type ir-type-defn)))
				(setf *header* (format nil "~a~%type ~a~a = ~a;" *header* ir-type-id formals-rust-defs inner-type)))
            )))
		(format nil "~a~a" ir-type-id formals-rust)
	)))

(defmethod ir2rust-type ((ir-typ ir-funtype))
  (with-slots (ir-domain ir-range) ir-typ
    (format nil "funtype<~a,~a>" (ir2rust-type ir-domain) (ir2rust-type ir-range))
  ))

(defmethod ir2rust-type ((ir-typ ir-recordtype)) 
	(with-slots (ir-label ir-field-types) ir-typ
		(let* ((*hash* "")
			(used-formals (remove-duplicates (used-type-formals ir-typ nil) :test 'string=))
			(formals-rust-defs (if (consp used-formals) (format nil "<~{~a : RegularOrd + 'static,~}>" used-formals) ""))
			(formals-rust (if (consp used-formals) (format nil "<~{~a,~}>" used-formals) "")))
			(loop for field in ir-field-types 
				collect (with-slots (ir-id ir-vtype) field 
					(setf *hash* (format nil "~a~a~a" *hash* ir-id (ir2rust-type ir-vtype)))))
			(if (position *hash* *unique-rust-records* :test 'string=)
				(format nil "record_~a~a" (position *hash* *unique-rust-records* :test 'string=) formals-rust)
				(progn (setf *unique-rust-records* (nconc *unique-rust-records* (list *hash*))) ;; adding the hash to the list 
					(setf *header* (format nil "~a~%#[derive(Clone, PartialEq, Eq, Hash)]~%pub struct ~a~a {~%" 
						*header* (format nil "record_~a" (position *hash* *unique-rust-records* :test 'string=)) formals-rust-defs)) ;; adding the declaration to the header
					(loop for field in ir-field-types 
						collect (with-slots (ir-id ir-vtype) field 
						(setf *header* (format nil "~a~%~a : ~a," *header* ir-id (ir2rust-type ir-vtype))))
					)
					(setf *header* (format nil "~a~%}~%" *header*))
					(format nil "record_~a~a" (position *hash* *unique-rust-records*) formals-rust) ;; returning the name
				))
		))) 

(defmethod ir2rust-type ((ir-typ ir-arraytype)) 
  (with-slots (size high ir-domain ir-range) ir-typ
    (format nil "arraytype<~a, ~a>" (+ size 1) (ir2rust-type ir-range) )))

(defmethod ir2rust-type ((ir-typ ir-formal-typename)) 
  (with-slots (ir-type-id) ir-typ
    (format nil "~a" ir-type-id)))

(defmethod ir2rust-type ((ir-typ t))
  (format t "~%~a ~a ~a" ir-typ (type-of ir-typ) (print-ir ir-typ))
  (break "Unsupported type"))


(defmethod used-type-formals ((ir-typ ir-funtype) recursion-guard) 
	(with-slots (ir-domain ir-range) ir-typ 
		(concat-lists (used-type-formals ir-domain recursion-guard) (used-type-formals ir-range recursion-guard))))

(defmethod used-type-formals ((ir-typ ir-arraytype) recursion-guard) 
	(with-slots (ir-range) ir-typ)
		(used-type-formals ir-range recursion-guard))

(defmethod used-type-formals ((ir-typ ir-typename) recursion-guard) 
	(used-type-formals (ir-type-defn ir-typ) recursion-guard))

(defmethod used-type-formals ((ir-typ ir-recordtype) recursion-guard) 
	(let* ((used-formals nil))
		(with-slots (ir-field-types) ir-typ
			(loop for field in ir-field-types collect 
				(setf used-formals (concat-lists used-formals 
					(used-type-formals (ir-vtype field) recursion-guard))
				)
			)
		)
		used-formals
	)		
)

(defmethod used-type-formals ((ir-typ ir-adt-recordtype) recursion-guard) 
	(loop for constructor in (ir-constructors ir-typ) collect
		(loop for accessor in constructor collect
			(format t "~%AAAAA ~a ~a" accessor (type-of accessor))))
	(if recursion-guard nil 
	(let* ((utf nil))
		(loop for constructor in (ir-constructors ir-typ)
			collect (loop for accessor in (acc-decls constructor)
				collect (setf utf (concat-lists utf (used-type-formals (pvs2ir-type (range (type accessor))) recursion-guard)))
			)
		)
	utf
	))
)


(defmethod used-type-formals ((ir-typ ir-formal-typename) recursion-guard) 
	(with-slots (ir-type-id) ir-typ
		(list ir-type-id)))

(defmethod used-type-formals ((ir-typ t) recursion-guard)
	nil)

;;TODO
(defun used-type-formals-constructor (constructor))

(defun concat-lists (seq1 seq2)
  (if (null seq1)
      seq2
      (cons (car seq1) (concat-lists (cdr seq1) seq2))))

(defun is-var-type-actual (var)
	(and (typep var 'ir-variable)
	(typep (ir-vtype var) 'ir-typename)
	(string= (ir-type-id (ir-vtype var)) "type_actual"))
)

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))