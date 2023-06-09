;;pvs2ir translates PVS expressions, definitions, and theories to an intermediate
;;representation (IR).  The IR consists of variables, tuples, function applications,
;;lambda-expressions, if-expressions, lets, and updates.

(in-package :pvs)
(defvar *pvs2rust-preceding-theories* nil)
(defvar *current-pvs2rust-theory* nil)
(defvar *output* nil) ;; tmp used for output construction in pvs2rust(decl  functions (not elsewhere) 
(defvar *functions* nil) ;; list of all fn names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-ir ((ir-expr ir-apply))
  (with-slots (ir-func ir-params ir-args ir-atype) ir-expr
	      `(,(print-ir ir-func) ,@(print-ir ir-params) ,@(print-ir ir-args))))

(defmethod print-ir ((ir-expr ir-record ))
  (with-slots (ir-fields ir-recordtype) ir-expr
	      `(record ,(print-ir ir-recordtype) ,(print-ir ir-fields))))

(defmethod print-ir ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn type-declaration) ir-type
    (format nil "(~a : ~a)"
	    ir-type-id (print-ir ir-type-defn))))

(defmethod print-ir ((ir-type ir-arraytype)) 
  (with-slots (size high ir-range) ir-type
	      (format nil "(array ~a [~d/~a])" (print-ir ir-range) size (print-ir high))))

(defmethod print-ir ((ir-type ir-adt-recordtype))
  (with-slots (ir-field-types ir-constructors) ir-type
	      `adt-recordtype))

(defmethod print-ir :around ((ir-type ir-type))
  (with-slots (renamings) ir-type
    (let ((print-renames (loop for (x . y) in renamings collect (cons (print-ir x)(print-ir y)))))
      (if print-renames
	  `,(call-next-method)
	  ;;`(rename ,print-renames in ,(call-next-method))
	(call-next-method)))))


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
		 (ir-new-rhsvar (new-irvar)))
	     (let ((ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
		   (ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-type))
		   (ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
	       (mk-ir-let  ir-lhs1-vartype lhs1-ir
			   (mk-ir-let ir-expr-vartype11
				      (mk-ir-apply ir-exprvar (list ir-lhs1-vartype));;was lookup
					  (mk-ir-let ir-new-rhsvartype
							ir-rest-assignment
							(mk-ir-update ir-exprvar ir-lhs1-vartype ir-new-rhsvartype))
				      ))
			))))
	(t ir-exprvar)))

;; Gasc : i made a new way to change arrays to optimize array update in pvs2rust
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-arraytype) lhs rhs-irvar ir-exprvar bindings)
	(let* ((lhs1 (caar lhs)) ; i
		   (lhs1-ir (pvs2ir* lhs1 bindings nil)) 
		   (lhs-exprvar (mk-ir-lookup ir-exprvar (list lhs1-ir))) ; A[i] : lookup

		   (lhs1-irvar (new-irvar))
		   (lhs1-ir-type (ir-domain ir-expr-type))
		   (ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
		  )
		  (if (consp (cdr lhs))
			  (pvs2ir-assignment1 ir-expr-type (cdr lhs) rhs-irvar lhs-exprvar bindings)
			  (mk-ir-let ir-lhs1-vartype lhs1-ir
				(mk-ir-update 
			  	  ir-exprvar
				  ir-lhs1-vartype
				  rhs-irvar
				)
			  )
		  )
	)
)


(defmethod pvs2ir-assignment1 ((ir-expr-type ir-recordtype) lhs rhs-irvar ir-exprvar bindings)
    (break "~% Should not be called anymore, see explanation in code.")) ;; GSC, see below

;; Why the function below ?
;; We always want recordtypes to be called behind their custom types (because we want to relate 
;; it to the correct struct in Rust). So we modified pvs2ir-assignement1 typename in order to 
;; call this function if it detects a record type behind a typename.
;; The typename is preserved.
(defmethod pvs2ir-assignment1-custom-recordtype ((ir-expr-typename ir-typename) lhs rhs-irvar ir-exprvar bindings)
	(let* ((ir-expr-type (ir-type-defn ir-expr-typename)))
		(let* ((lhs1 (caar lhs)) ; i ;;lhs1 is a field-name-expr
			(lhs-exprvar (mk-ir-get ir-exprvar (id lhs1))) ; A[i] : get

			;;(lhs1-irvar (new-irvar))
			;;(lhs1-ir-type (ir-domain ir-expr-type))
			;;(ir-lhs1-vartype (mk-ir-variable lhs1-irvar lhs1-ir-type))
			)
			(if (consp (cdr lhs))
				(pvs2ir-assignment1-custom-recordtype ir-expr-typename (cdr lhs) rhs-irvar lhs-exprvar bindings)
				(mk-ir-update 
					ir-exprvar
					(id lhs1)
					rhs-irvar
				)
			))))


;;had to add method for ir-adt-recordtype since the type here is the adt and not the constructor,
;;whereas in the record case, the field-assign does not have a type s othat ir-expr-type11 has to
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
			  (mk-ir-let ir-expr-vartype1
				     (if (ir-reference-type? ir-expr-type11)
					 (let* ((ir-nullvar (new-irvar))
						(ir-nullvartype (mk-ir-variable ir-nullvar ir-expr-type11)))
					   (mk-ir-let ir-nullvartype
						      (mk-ir-nil)
						      (mk-ir-update ir-exprvar (id lhs1) ir-nullvartype)))
				       ir-exprvar)
				     (mk-ir-let ir-new-rhsvartype
						ir-rest-assignment
						(mk-ir-update ir-expr-vartype1 (id lhs1) ir-new-rhsvartype)))))))
	  (t ir-exprvar)))


;; See explanation before pvs2ir-assignment1-custom-recordtype
(defmethod pvs2ir-assignment1 ((ir-expr-type ir-typename) lhs rhs-irvar ir-exprvar bindings)
  (if (typep (ir-type-defn ir-expr-type) 'ir-recordtype)
	(pvs2ir-assignment1-custom-recordtype ir-expr-type lhs rhs-irvar ir-exprvar bindings)
	(pvs2ir-assignment1 (ir-type-defn ir-expr-type) lhs rhs-irvar ir-exprvar bindings)
  )
  )

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

(defmethod pvs2ir-adt-accessor* ((adecl adt-accessor-decl)
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

(defmethod pvs2ir-adt-accessor* ((adecl shared-adt-accessor-decl)
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
	  
(defun pvs2ir-adt-decl (adt-decl);;only called with ir-type-value is empty
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

(defmethod pvs2ir-type* ((type arraytype) tbinding)
  (with-slots (domain range) type
    (let* ((ir-dom (pvs2ir-type* domain tbinding))
	   (new-tbinding (if (binding? domain)
			     (acons domain (mk-ir-variable (new-irvar)
							   ir-dom)
				    tbinding)
			   tbinding))
	   (ptype (print-type type))
	   (ir-actuals (loop for act in (actuals ptype)
			   collect (if (type-value act)
				       (mk-ir-type-actual (pvs2ir-type (type-value act) bindings))
				     (mk-ir-const-actual (pvs2ir* act bindings nil)))))
	   (defined-ir-type-value (when (type-name? ptype)
			     (ir-type-value-with-actuals (declaration ptype) ir-actuals)))
	   (defined-ir-domtype (when (and defined-ir-type-value
					  (ir-arraytype? (ir-type-defn (ir-type-name defined-ir-type-value))))
				 (ir-domain (ir-type-defn (ir-type-name defined-ir-type-value)))))
	   (ir-type (let ((ir-dom-subrange (is-ir-subrange? ir-dom)))
		      (if (ir-subrange? ir-dom-subrange)
			  (with-slots (ir-low ir-high ir-high-expr) ir-dom-subrange
			    (let ((check-range (check-arraytype-ir-subrange ir-low ir-high)))
			      (if check-range
				  (mk-ir-arraytype check-range (or ir-high-expr check-range)
						   ir-dom (pvs2ir-type* range new-tbinding))
				(if defined-ir-domtype
				    (with-slots ((ir-defined-low ir-low) (ir-defined-high ir-high)) defined-ir-domtype
				      (let ((check-defined-range (check-arraytype-ir-subrange ir-defined-low ir-defined-high)))
					(if check-defined-range 
					    (mk-ir-arraytype check-defined-range (or ir-high-expr check-defined-range)
							     ir-dom (pvs2ir-type* range new-tbinding))
					  (mk-ir-funtype ir-dom
							 (pvs2ir-type* range new-tbinding)))))
				  (mk-ir-funtype ir-dom
						 (pvs2ir-type* range new-tbinding))))))
		      (mk-ir-funtype ir-dom
				     (pvs2ir-type* range new-tbinding))))))
    ;;  (when (ir-funtype? ir-type)(break "pvs2ir-type*(arraytype)")
	;;	(format t "~%Unable to translate ~a as an array; using function type instead" type))
      ir-type)))


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
					)(progn ;(format t "~%ir var type ~a print ~a" ir-var-vtype (print-ir ir-var-vtype))
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

(defun pvs2ir-constant (expr bindings)
  (let* ((decl (declaration expr)))
    (cond ((pvs2ir-primitive? expr) ;;borrowed from pvseval-update.lisp
	   (cond ((eq (id expr) 'TRUE) (mk-ir-bool t))
		 ((eq (id expr) 'FALSE) (mk-ir-bool nil))
		 (t (mk-ir-primitive-function (id expr) decl))));for primitives, types are derived from args
	  (t (let ((ir-defn (pvs2ir-constant-ir expr bindings decl)))
	       (ir-function-name ir-defn))))))

(defun pvs2ir-constant-ir (expr bindings decl)
  (if (adt-expr? expr)
      (let ((adt (adt expr)));(break "pvs2ir-constant-ir(adt)")
	(pvs2ir-adt adt)
	(ir (eval-info decl)))
    (let* ((theory (module decl))
	   (thinst (module-instance expr))
	   (actuals (actuals thinst))
	   (type-actuals (loop for act in actuals
			       when (type-value act)
			       collect (check-actual-type (type-value act) bindings)))
	   (nonref-actuals (loop for actlabel in type-actuals
				 when (not (eq actlabel 'ref))
				 collect actlabel))
	   (new-theory-id (format nil "~a_~{~a~^_~}" (simple-id (id theory))
					type-actuals))
	   (intern-theory-id (intern new-theory-id))
	   ) (when nonref-actuals (break "nonref-actuals"))
      (cond ((and nonref-actuals (not (eq theory *current-pvs2rust-theory*)))  ;; was (eq intern-theory-id *theory-id*)))
	     (let* ((*theory-id* intern-theory-id)
		    (monoclones (ht-instance-clone theory))
		    (dummy (when (null monoclones)(format t "~% No monoclones")))
		    (thclone (and monoclones (gethash  intern-theory-id monoclones)))
		    (theory-instance (or thclone
					 (let ((new-instance (subst-mod-params theory thinst)))
					   (setf (id new-instance) *theory-id*)
					   new-instance)))
		    (dummy2 (when thclone (format t "~%Found thclone")))
		    (instdecl (find  decl (theory theory-instance) :key #'generated-by))
		    ) ;(break "nonref-actuals")			;place information matches
	       (cond (thclone
		      (format t "~%Pushing ~a" intern-theory-id)
		      (pushnew theory-instance *preceding-mono-theories*)
		      (ir (eval-info instdecl)))
		     (t (unless monoclones (setf (ht-instance-clone theory)(make-hash-table :test #'eq)))
			(setf (gethash intern-theory-id (ht-instance-clone theory)) theory-instance)
			(pvs2rust-theory-body-step theory-instance t nil) ;;NSH(3/20/22): Need to clear inherited eval-info
			(format t "~%Pushing ~a" intern-theory-id)
			(pushnew theory-instance *preceding-mono-theories*)
			;; (if (memq theory *preceding-prelude-theories*)
			;; 	 (push theory-instance *preceding-prelude-theories*)
			;;   (push theory-instance *preceding-theories*))
			(ir (eval-info instdecl))))))
	    (t (ir (eval-info decl)))))))

;; ------- IR2C --------

(defun ir2c-function-apply (return-var return-type ir-function ir-args); GASC changed for allowing direct ivar < 1 and avoid let ivar2 = 1
	(let* ((ir-arg-vars (loop for ir-var in ir-args
			      collect (if (typep ir-var 'fixnum) (mk-ir-subrange ir-var ir-var) (get-ir-last-var ir-var))))
	   (ir-arg-var-names (loop for ir-var in ir-args
				   collect (if (typep ir-var 'fixnum) ir-var (ir-name (get-ir-last-var ir-var)))))
	   (ir-function-name (when (ir-function? ir-function)(ir-fname ir-function))));;(break "ir2c-function-apply")
	  (if (ir-primitive-function? ir-function)
	  (let ((instrs (ir2c-primitive-apply return-var return-type ir-function-name ir-arg-vars ir-arg-var-names)))
	    instrs
	    ;; (if (mpnumber-type? return-type)
	    ;; 	(cons (format nil "~a = safe_malloc(sizeof(~a_t))" return-var  return-type)
	    ;; 	      (cons (format nil "~a_init(~a)" return-type return-var) instrs))
	    ;;   instrs)
	    )
	(let* ((ir-funvar (get-ir-last-var ir-function))
	       (c-return-type (add-c-type-definition (ir2c-type return-type))))
	  (if (ir-variable? ir-funvar)
	      (let* ((arg-string (format nil "~{~a~^, ~}" ir-arg-var-names))
		     (ir-fun-name (if (eql (length ir-arg-vars) 1)
				      (format nil "~a->ftbl->fptr" (ir-name ir-funvar))
				    (format nil "~a->ftbl->mptr" (ir-name ir-funvar))))
		     (apply-instr
		      (mk-c-assignment return-var c-return-type
				       (format nil "~a(~a, ~a)" ir-fun-name (ir-name ir-funvar) arg-string)
				       (add-c-type-definition (ir2c-type (get-range (ir-vtype ir-funvar))))))
		      ;; (case c-return-type
		      ;; 	    ((mpz mpq)
		      ;; 	     (format nil "~a(~a, ~a, ~a)" ir-fun-name (ir-name ir-funvar) return-var arg-string))
		      ;; 	    (t (format nil "~a = (~a_t)~a(~a, ~a)"  return-var  c-return-type
		      ;; 		       ir-fun-name (ir-name ir-funvar) arg-string)))
		     (closure-release-instrs (when (ir-last? ir-function)
					       (list (format nil "~a->ftbl->rptr(~a)"
							     (ir-name ir-funvar)
							     (ir-name ir-funvar))))))
		(cons apply-instr closure-release-instrs))
	    (let* ((fdecl (declaration ir-function))
		   (function-ir (ir (eval-info fdecl)))
		   (ir-return-type (ir-return-type function-ir))
		   (function-has-closure-value? (and (null (ir-args function-ir))
						     (or (ir-funtype? ir-return-type)
							 (and (ir-typename? ir-return-type)
							      (ir-funtype? (ir-type-defn ir-return-type)))))))
	      (if function-has-closure-value?
		  (let* ((tmp (gentemp "tmp"))
			 (funvar (mk-ir-variable tmp ir-return-type))
			 (new-ir (mk-ir-let funvar ir-function
					    (mk-ir-apply (mk-ir-last funvar) ir-args))))
		    (ir2c* new-ir return-var return-type))
		(let* ((ir-funarg-types (or (and (cdefn (eval-info fdecl))
						 (op-arg-types (cdefn (eval-info fdecl))))
					    (loop for ir-formal in (ir-args function-ir)
						  collect (add-c-type-definition (ir2c-type (ir-vtype ir-formal))))))
		       (c-fun-range-type (or (and (cdefn (eval-info fdecl))
						  (op-return-type (cdefn (eval-info fdecl))))
					     (add-c-type-definition (ir2c-type ir-return-type))))
		   
		       (arg-pairs (loop for argtype in ir-funarg-types
					as arg in ir-arg-var-names
					collect (case argtype
						  ((mpz mpq)(format nil "~a" arg))
						  (t (format nil "(~a_t)~a" argtype arg)))))
		       (arg-string (format nil "~{~a~^, ~}" arg-pairs)))
;	      (when (ir-function? ir-function-name) (break "ir-function"))
	       (make-c-application-assignment return-var c-return-type
					      ir-function-name arg-string
					      c-fun-range-type)))))))))


(defun ir2c-primitive-apply (return-var return-type ir-function-name ir-args ir-arg-names)
; GASC now the two last ones can be numbers
;  (cond ((ir-primitive-function-name? ir-function-name);
  (let* ((arg-types (loop for ir-var in ir-args
			  collect (ir2c-type ( if (typep ir-var 'ir-subrange) ir-var (ir-vtype ir-var)))))
	 (c-arg-types (loop for ir-type in arg-types
			   collect (add-c-type-definition ir-type)))
	(c-return-type (add-c-type-definition (ir2c-type return-type)))
					;(arity (length ir-args))
	)
    (case ir-function-name
      (+ (ir2c-addition return-var c-return-type ir-arg-names c-arg-types))
      (- (ir2c-subtraction return-var c-return-type ir-arg-names c-arg-types))
      (* (ir2c-multiplication  return-var c-return-type ir-arg-names c-arg-types))
      (/ (ir2c-division return-var c-return-type ir-arg-names c-arg-types))
      (= (ir2c-equality return-var c-return-type ir-arg-names c-arg-types arg-types))
      (/= (ir2c-disequality return-var c-return-type ir-arg-names c-arg-types arg-types))
      ((ndiv nrem) (ir2c-divrem ir-function-name return-var c-return-type ir-arg-names c-arg-types))
      ((< <= > >=) (ir2c-arith-relations ir-function-name ;(tweak-equal ir-function-name)
					 return-var
					 ir-arg-names c-arg-types))
      (floor (list (format nil "~a = (~a_t)pvsfloor_~a_~a(~a)"
			   return-var (mppointer-type return-type)
			   (numtype-abbrev (car c-arg-types))
			   (numtype-abbrev return-type)
			   (car ir-arg-names))))
      (ceiling (list (format nil "~a = (~a_t)pvsceiling_~a_~a(~a)"
			     return-var (mppointer-type return-type)
			     (numtype-abbrev (car c-arg-types))
			     (numtype-abbrev return-type)
			     (car ir-arg-names))))
      (char? (list (format nil "~a = (~a_t)(~a < 0x110000)"
			   return-var return-type (car ir-arg-names))))
      (ord (list (format nil "~a = (~a_t) 0"
			   return-var return-type)))
      ((NOT ¬) (list (format nil "~a = !~a" return-var (car ir-arg-names) )))
      ((OR ∨)
       (list (format nil "~a = ~a || ~a" return-var (car ir-arg-names) 
		     (cadr ir-arg-names))))
      ((AND & ∧)
       (list (format nil "~a = ~a && ~a" return-var (car ir-arg-names) 
		     (cadr ir-arg-names))))
      ((IMPLIES => ⇒) (list (format nil "~a = (!~a) ||  ~a" return-var (car ir-arg-names)
			     (cadr ir-arg-names))))
      (WHEN (list (format nil "~a = ~a || ! ~a" return-var (car ir-arg-names)
			  (cadr ir-arg-names))))
      ((IFF <=> ⇔) (list (format nil "~a = (~a || ! ~a) && ((!~a) ||  ~a)" return-var
			 (car ir-arg-names)  (cadr ir-arg-names)
			 (car ir-arg-names)  (cadr ir-arg-names))))
      ((code char) (list (format nil "~a = (uint32_t) ~a" return-var (car ir-arg-names))))
      (t (list (format nil "~a = ~a(~{~a~^, ~})" return-var ir-function-name ir-arg-names)))
      )))


(defmethod ir2c* ((ir-expr ir-ift) return-var return-type)
  (with-slots (ir-condition ir-then ir-else) ir-expr
	      (let ((c-then-instrs (ir2c* ir-then return-var return-type))
		    (c-else-instrs (ir2c* ir-else return-var return-type)))
		(list (mk-if-instr 
		       (ir-name (get-ir-last-var ir-condition)) ; GASC changed for allowing direct ivar < 1 and avoid let ivar2 = 1
			   ;(if (typep ir-condition ir-variable) (ir-name (get-ir-last-var ir-condition)) (ir2c* ir-condition return-var return-type))
		       c-then-instrs
		       c-else-instrs)))))


(defmethod ir2c* ((ir-expr ir-update) return-var return-type)
  (with-slots (ir-target ir-lhs ir-rhs) ir-expr ; ir-target can be lookup
		  (if (typep ir-target 'ir-lookup)
		  	  (cons (format nil "Error : this version does not intend to create c code") nil)
			  (let* ((target-var (get-ir-last-var ir-target))
						(target-var-name (ir-name target-var))
						(rhs-var (get-ir-last-var ir-rhs))
						(rhs-var-name (ir-name rhs-var))
						(ir-ctype (ir2c-type (ir-vtype target-var)))
						(ctype (add-c-type-definition ir-ctype))
						(creturn-type (add-c-type-definition (ir2c-type return-type)))
						(target-last (ir-last? ir-target))
						(rhs-last (and (not (ir-constructor-update? ir-expr))
								(ir-last? ir-rhs)
								(ir-reference-type? (ir-vtype rhs-var))))
						(rhs-last-instr (if rhs-last
								(list (format nil "if (~a != NULL) ~a->count--"
										rhs-var-name rhs-var-name))
								nil))) ;(break "ir2c*(ir-update)")
					;(when (ir-constructor-update? ir-expr)(break "ir2c*(ir-constructor-update)"))
					(if (ir-lvariable? ir-lhs)  ;;this doesn't work with names
												;;(ir-arraytype? (get-ir-type-value ir-ctype))
						(let* ((lhs-var (get-ir-last-var ir-lhs))
						(lhs-var-name (ir-name lhs-var)))
						(if target-last
						(cons (format nil "~a = (~a_t)update_~a(~a, ~a, ~a)"
								return-var creturn-type ctype target-var-name lhs-var-name rhs-var-name)
							rhs-last-instr)
						(cons (format nil "{~a = (~a_t)copy_~a(~a); update_~a(~a, ~a, ~a)}"
								return-var creturn-type ctype target-var-name ctype
								return-var  lhs-var-name rhs-var-name)
							rhs-last-instr)))
					;;else we're assuming it's a record
					(if target-last
						(cons (format nil "~a = (~a_t)update_~a_~a(~a, ~a)"
								return-var creturn-type ctype ir-lhs target-var-name rhs-var-name)
							rhs-last-instr)
						(cons (format nil "{~a = (~a_t)copy_~a(~a); ~a = (~a_t)update_~a_~a(~a, ~a);}"
							return-var creturn-type ctype target-var-name
							return-var creturn-type ctype ir-lhs return-var rhs-var-name)
						rhs-last-instr))))
		  )
	      ))

(defun ir2c-division-step (return-var c-return-type arg1 arg1-c-type arg2 arg2-c-type)
  (case  c-return-type
    (mpq (case arg1-c-type
	   (mpq
	    (case arg2-c-type
	      (mpq (list (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 arg2)))
	      (mpz (let ((arg2-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg2-mpq-var)
			   (format nil "mpq_init(~a)" arg2-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 arg2-mpq-var)
			   (format nil "mpq_clear(~a)" arg2-mpq-var)
			   )))
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (list (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
			     (gmp-ui-or-si arg2-c-type)
			     return-var (uint-or-int arg2-c-type) arg2)
		     (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1 return-var)))
	      (t (format nil "Division step error ir2c.lisp" ))))
	   (mpz
	    (case arg2-c-type
	      (mpq (let ((arg1-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg1-mpq-var)
			   (format nil "mpq_init(~a)" arg1-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var arg2)
			   (format nil "mpq_clear(~a)" arg1-mpq-var)
			   )))
	      (mpz (let ((arg1-mpq-var (gentemp "tmp"))
			 (arg2-mpq-var (gentemp "tmp")))
		     (list (format nil "mpq_t ~a" arg1-mpq-var)
			   (format nil "mpq_init(~a)" arg1-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
			   (format nil "mpq_t ~a" arg2-mpq-var)
			   (format nil "mpq_init(~a)" arg2-mpq-var)
			   (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			   (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var arg2-mpq-var)
			   (format nil "mpq_clear(~a)" arg1-mpq-var)
			   (format nil "mpq_clear(~a)" arg2-mpq-var)
			   )))
	      ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	       (let ((arg1-mpq-var (gentemp "tmp")))
		 (list (format nil "mpq_t ~a" arg1-mpq-var)
		       (format nil "mpq_init(~a)" arg1-mpq-var)
		       (format nil "mpq_set_z(~a, ~a)" arg1-mpq-var arg1)
		       (format nil "mpq_mk_set_~a(~a, (~a_t)~a, 1)"
			       (gmp-ui-or-si arg1-c-type)
			       return-var (uint-or-int arg1-c-type) arg2)
		       (format nil "mpq_mk_div(~a, ~a, ~a)" return-var
			       arg1-mpq-var return-var)
		       (format nil "mpq_clear(~a)" arg1-mpq-var)
		       )))
	      (t (format nil "Division step error ir2c.lisp" ))))
	   ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
	     (case arg2-c-type
	       (mpq (let ((tmp (gentemp "tmp")))
		      (list (format nil "mpq_t ~a" tmp)
			    (format nil "mpq_init(~a)" tmp)
			    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)"
				  (gmp-ui-or-si arg1-c-type)
				  tmp (uint-or-int arg1-c-type) arg1)
			  (format nil "mpq_mk_div(~a, ~a, ~a)" return-var tmp arg2))))
	       (mpz (let ((arg1-mpq-var (gentemp "tmp"))
			  (arg2-mpq-var (gentemp "tmp")))
		      (list (format nil "mpq_t ~a" arg1-mpq-var)
			    (format nil "mpq_init(~a)" arg1-mpq-var)
			    (format nil "mpq_t ~a" arg2-mpq-var)
			    (format nil "mpq_init(~a)" arg2-mpq-var)
			    (format nil "mpq_set_~a(~a, (~a64_t)~a, 1)" (gmp-ui-or-si arg1-c-type)
				    arg1-mpq-var (uint-or-int arg1-c-type) arg1)
			    (format nil "mpq_set_z(~a, ~a)" arg2-mpq-var arg2)
			    (format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var
				    arg2-mpq-var)
			    (format nil "mpq_clear(~a)" arg1-mpq-var)
			    (format nil "mpq_clear(~a)" arg2-mpq-var)
			    )))
	       ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
		(let ((arg1-mpq-var (gentemp "tmp"))
		      (arg2-mpq-var (gentemp "tmp")))
		  (list (format nil "mpq_t ~a" arg1-mpq-var)
			(format nil "mpq_init(~a)" arg1-mpq-var)
			(format nil "mpq_t ~a" arg2-mpq-var)
			(format nil "mpq_init(~a)" arg2-mpq-var)
			(format nil "mpq_set_~a(~a, (~a64_t)~a, 1)" (gmp-ui-or-si arg1-c-type)
				arg1-mpq-var (uint-or-int arg1-c-type) arg1)
			(format nil "mpq_set_~a(~a, ~a, 1)" (gmp-ui-or-si arg2-c-type)
				arg2-mpq-var arg2)
			(format nil "mpq_mk_div(~a, ~a, ~a)" return-var arg1-mpq-var
				arg2-mpq-var)
			(format nil "mpq_clear(~a)" arg1-mpq-var)
			(format nil "mpq_clear(~a)" arg2-mpq-var)
			)))
	       (t (format nil "Division step error ir2c.lisp" ))))
	   (t (format nil "Division step error ir2c.lisp" ))))
    ((__int128 __uint128) (format nil "Division step error ir2c.lisp" ))
    (mpz (let* ((mpq-return-var (gentemp "tmp"))
		(mpq-instrs (ir2c-division-step mpq-return-var "mpq" arg1 arg1-c-type arg1 arg2-c-type)))
	   (cons (format nil "mpq_t ~a" mpq-return-var)
		 (nconc mpq-instrs
			(list (format nil "mpz_mk_set_q(~a, ~a)" return-var mpq-return-var)
			      (format nil "mpq_clear(~a)" mpq-return-var)
			      )))))
    ((uint8 uint16 uint32 uint64 int8 int16 int32 int64)
     (let* ((mpz-return-var (gentemp "tmp"))
	    (mpz-instrs (ir2c-division-step mpz-return-var "mpz" arg1 arg1-c-type arg1 arg2-c-type)))
       (cons (format nil "mpz_t ~a" mpz-return-var)
	     (nconc mpz-instrs
		    (list (format nil "mpz_mk_set_q(~a, ~a)" return-var mpz-return-var)
			  (format nil "mpq_clear(~a)" mpz-return-var)
			  )))))
    (t (format nil "Division step error ir2c.lisp" ))))

(defmethod ir2c* ((ir-expr ir-apply) return-var return-type)
  (with-slots (ir-func ir-params ir-args) ir-expr
	      (let* ((ir-func-var (get-ir-last-var ir-func))
		     (hi-array (when (ir-variable? ir-func-var)
				 (ir-array? (ir-vtype ir-func-var))))
		     (ir-index-var (when hi-array (get-ir-last-var (car ir-args)))))
	      (if hi-array;;if operator is an array
		  (let* ((assign-instr (mk-c-assignment 
					       return-var
					       (add-c-type-definition (ir2c-type return-type))
					       (format nil "~a->elems[~a]"
						       (ir-name ir-func-var)(ir-name ir-index-var))
					       (add-c-type-definition (get-range (ir-vtype ir-func-var)))))
			 (refcount-lookup-instr	;if lookup is a reference, first bump its count
			  (when (ir-reference-type? (ir-range (get-ir-type-value (ir-vtype ir-func-var))))
			    (list (format nil "~a->count++"  return-var))))
			 (release-array-instr
			  (when (ir-last? ir-func) ;if last, then release the array
			    (list (release-last-var ir-func-var)))))
		    ;(break "ir-apply")
		    (cons assign-instr (append refcount-lookup-instr release-array-instr)))
		;;otherwise, it's a function call
		(let* ((ir-params-args (append ir-params ir-args))
		       (invoke-instrs (ir2c-function-apply return-var return-type ir-func ir-params-args
							   ))
		     ;; (rhs-string (format nil "~a(~{~a~^, ~})" ir-func-var ir-arg-vars))
		     ;; (invoke-instr (format nil "~a = ~a" return-var rhs-string))
		     (release-instrs (loop for ir-var in ir-params-args ;;bump the counts of non-last references by one
					   when (and (not (ir-last? ir-var))(ir-reference-type? (if (typep ir-var 'fixnum) nil (ir-vtype (get-ir-last-var ir-var)))))
					   collect (format nil "~a->count++" (ir-name ir-var))))
		     ;; (mp-release-instrs (loop for ir-var in ir-params-args
		     ;; 			      when (and (ir-last? ir-var)
		     ;; 					;(not (member (get-ir-last-var ir-var) *mpvar-parameters*))
		     ;; 					(mpnumber-type? (ir2c-type (ir-vtype (get-ir-last-var ir-var)))))
		     ;; 			      collect (format nil "~a_clear(~a)"
		     ;; 					      (ir2c-type (ir-vtype (get-ir-last-var ir-var)))
		     ;; 					      (ir-name (get-ir-last-var ir-var)))))
		     )
		;(break "ir-apply")
		  (nconc release-instrs invoke-instrs) ;nconc invoke-instrs mp-release-instrs
					       )))))



;; -------- PVS2C ---------

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


(defmethod pvs2c-decl* ((decl const-decl))
  (if (or (adt-constructor-decl? decl)	;;these are automatically generated from the adt type decl
			   (adt-recognizer-decl? decl)
			   (adt-accessor-decl? decl)) nil 
			   (let* (;(thid (simple-id (id (module decl))))
					(declid (simple-id (id decl)))
					(ir-function-id (intern (format nil "~a__~a" *theory-id* declid)))
					(hashentry (gethash ir-function-id *c-primitive-attachments-hash*)))
					(cond (hashentry
					(let* ((einfo (or (eval-info decl)
								(let* ((new-einfo (make-instance 'eval-info))
									)	;all nil for now
								(setf (eval-info decl) new-einfo)
								new-einfo)))
						(new-ir-function (mk-ir-function ir-function-id decl))
						(new-ir (mk-ir-defn new-ir-function nil nil nil)))
						(setf (ir einfo) new-ir)
						(setf (cdefn einfo) hashentry)
						(op-name hashentry)))
					(t   (unless (or (adt-constructor-decl? decl)	;;these are automatically generated from the adt type decl
							(adt-recognizer-decl? decl)
							(adt-accessor-decl? decl))
						(pvs2ir-decl decl))
						(let ((ir (ir (eval-info decl)))) ;(break "pvs2c-decl*/const-decl")
						(ir2c-decl* ir decl)
						))))
			   )
  )

(defun make-c-defn-info (ir pvs-return-type) ;(break "make-c-defn-info")
  (with-slots
	(ir-function-name ir-return-type ir-args ir-defn) ir
    ;; (when (eq (id decl) 'coef)(break "coef"))
    (if (null ir-defn)
	(let* ((ir-function-name (ir-fname ir-function-name))
	       (ir-result-type ir-return-type) ;(pvs2ir-type (type decl))
	       (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	       ;; (dummy (when (null c-result-type) (break "make-c-defn-info")))
	       ;;might need to adjust c-header when result type is gmp
	       (c-header (format nil "extern ~a_t ~a(~a)"
			   (mppointer-type c-result-type) ir-function-name
			   (c-args-string ir-args))))
	  (unless *to-emacs* ;; causes problems
	    (format nil "~%No definition for ~a" ir-function-name))
	  nil)
	  ;; (mk-c-defn-info ir-function-name (format nil "~a;" c-header) nil nil
	  ;; 		  c-result-type)
	(let* ((ir-function-name (ir-fname ir-function-name))
	       (pre-ir (if ir-args
			   (mk-ir-lambda ir-args ir-return-type ir-defn)
			   ir-defn))
	       (post-ir (preprocess-ir pre-ir))
	       (ir-args (when (ir-lambda? post-ir)
			  (ir-vartypes post-ir)))
					;	     (ir-decl-type (add-c-type-definition (ir2c-type (pvs2ir-type (declared-type decl)))))
	       (ir-result-type (if (ir-lambda? post-ir)
				   (ir-rangetype post-ir) ;(pvs2ir-type (range (find-supertype (type decl))))
				   (pvs2ir-type pvs-return-type)))
	       (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	       (c-arg-types (loop for arg in ir-args
			       collect (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype arg))))))
	       (*mpvar-parameters*  (loop for arg in ir-args
				       as c-arg-type in c-arg-types
				       when (mpnumber-type? c-arg-type)
				       collect arg))
	       (c-args-string (c-args-string ir-args))
	       (c-header (format nil "extern ~a_t ~a(~a)" (mppointer-type c-result-type) ir-function-name c-args-string))
	       ;; (case c-result-type
	       ;; 		 ((mpz mpq)
	       ;; 		  (if (consp c-args)
	       ;; 		      (format nil "extern void ~a(~a_t ~a, ~a)"
	       ;; 			      ir-function-name c-result-type 'result c-args-string)
	       ;; 		    (format nil "extern void ~a(~a_t ~a)"
	       ;; 			      ir-function-name c-result-type 'result)))
	       ;; 		 (t (format nil "extern ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	       (ir-body (if (ir-lambda? post-ir)
			    (ir-body post-ir)
			    post-ir))
	       (c-body (print2c (ir2c ir-body ir-result-type)))
	       (c-body (if ir-args c-body
			 (format nil "~%~8Tstatic bool_t defined = false;~%~8Tif (!defined){~%~12T~a~%~8Tdefined = true;};" c-body)))
	       (static? (if ir-args "" " static "))
	       (c-result-decl (if (mpnumber-type? c-result-type)
				  (let ((mptype (mppointer-type c-result-type)))
				    (format nil "~a_t ~a result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
				      mptype static?; c-result-type c-result-type
				      ))
				  (format nil "~a_t ~a result;" c-result-type static?)))
	       (c-defn  (format nil "~a{~%~8T~a~%~a~%~8T~%~8Treturn result;~%}"
			  c-header 
			  c-result-decl
			  c-body
			  ))
	       )
	  (unless *suppress-output* ;*to-emacs* ;; causes problems
	    ;;(format t "~% PRE IR ~a" (print-ir pre-ir))
	    (format t "~%$~a"  ir-function-name)   
	    (format t "~%@~a~%" (print-ir post-ir))
		)
	  (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn c-arg-types
			  c-result-type)))))

(defun make-c-closure-defn-info (ir-lambda-expr ir-function-name)
  (let* ((ir-args (ir-vartypes ir-lambda-expr))
	 (ir-result-type (ir-rangetype ir-lambda-expr)) ;(pvs2ir-type (range (find-supertype (type decl))))
	 (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	 (fvars (pvs2ir-freevars* ir-lambda-expr))
	 (args-fvarargs (append ir-args fvars))
	 (c-arg-types (loop for arg in args-fvarargs
		       collect (add-c-type-definition (ir2c-type (freevar-type arg)))))
	 (c-args (loop for arg in args-fvarargs
		       collect (format nil "~a_t ~a"
				       (mppointer-type (add-c-type-definition (ir2c-type (freevar-type arg))))
				       (freevar-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void")))
	 (c-header (format nil "extern ~a_t ~a(~a)" (mppointer-type c-result-type)
			   ir-function-name c-args-string))
	 ;; (case c-result-type
	 ;; 	     ((mpz mpq)
	 ;; 	      (if (consp c-args)
	 ;; 		  (format nil "extern void ~a(~a_t ~a, ~a)"
	 ;; 		      ir-function-name c-result-type 'result c-args-string)
	 ;; 		  (format nil "extern void ~a(~a_t ~a)"
	 ;; 		      ir-function-name c-result-type 'result)))
	 ;; 	     (t (format nil "extern ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	 (c-defn-arg-types c-arg-types)
	 ;; (case c-result-type
	 ;; 		     ((mpz mpq)(cons c-result-type c-arg-types))
	 ;; 		     (t c-arg-types))
	 (c-defn-result-type (mppointer-type c-result-type))
	     ;; (case c-result-type
	     ;; 		       ((mpz mpq) 'void)
	     ;; 		       (t c-result-type))
	 (ir-body (ir-body ir-lambda-expr))
	 (*mpvar-parameters*  (loop for arg in ir-args
				    as c-arg-type in c-arg-types 
				    when (mpnumber-type? c-arg-type)
				    collect arg))
	 (c-body (print2c (ir2c ir-body ir-result-type)))
	 (c-result-decl (case c-result-type
			  ((mpq mpz)
			   (format nil "~a_t result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
				   (mppointer-type c-result-type); c-result-type c-result-type
				   ))
			  (t (format nil "~a_t result;" c-result-type))))
	 (c-defn  (format nil "~a{~%~8T~a~%~a~%~8Treturn result;~%}"
			  c-header 
			  c-result-decl
			  c-body))
	 )
    (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn
		    c-defn-arg-types c-defn-result-type)))

(defun print-header-file (theory-id theory)
  (let* (;(theory-id (id theory)) ;made this a parameter
	 (file-string (format nil "~a_c.h" theory-id))
	 (preceding-theories (pvs2c-preceding-theories theory))
	 (preceding-prelude-theories (pvs2c-preceding-prelude-theories theory)))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
		    (format output "//Code generated using pvs2ir")
		    (format output "~%#ifndef _~a_h ~%#define _~a_h" theory-id theory-id)
		    (format output "~%~%#include <stdio.h>")
		    (format output "~%~%#include <stdlib.h>")
		    (format output "~%~%#include <inttypes.h>")
		    (format output "~%~%#include <stdbool.h>")
		    (format output "~%~%#include <stdarg.h>")		    
		    (format output "~%~%#include <string.h>")
		    (format output "~%~%#include <fcntl.h>")		    
		    (format output "~%~%#include <math.h>")
		    (format output "~%~%#include <sys/mman.h>")
		    (format output "~%~%#include <sys/stat.h>")
		    (format output "~%~%#include <sys/types.h>")
		    (format output "~%~%#include <gmp.h>")
		    (format output "~%~%#include \"pvslib.h\"")
		    (loop for thy in  preceding-prelude-theories
			  when (not (same-id thy theory-id))
			  do (format output "~%~%#include \"~a_c.h\"" (id thy)))
		    (loop for thy in  preceding-theories
			  when (not (same-id thy theory-id))
			  do (format output "~%~%#include \"~a_c.h\"" (id thy)))
		    (loop for thy in  *preceding-mono-theories*
			  do (format output "~%~%#include \"~a_c.h\"" (id thy)))
		    (format output "~%~%//cc -O3 -Wall -o ~a" theory-id )
		    (format output " -I ~a/include" *pvs-path*)
		    (format output " ~a/lib/pvslib.c " *pvs-path*)
		    (format output " -I ~alib" *pvs-path*)
		    (loop for thy in preceding-prelude-theories
			  do (format output " ~alib/~a_c.c" *pvs-path* (id thy)))
		    (loop for thy in preceding-theories
			  do (format output " ~a_c.c" (id thy)))
		    (loop for thy in  *preceding-mono-theories*
			  do (format output " ~a_c.c" (id thy)))
		    (format output " -lgmp ")
		    (loop for formal in (formals theory)
		      	  when (formal-type-decl? formal)
		      	  do (format output "~%~%typedef pointer_t ~a_t;" (ir-type-id (ir-type-name (ir-type-value formal)))))
		    (print-type-info-headers-to-file output *c-type-info-table*)
		    (loop for decl in (theory theory)
		     	  when (and (const-decl? decl)(eval-info decl)(cdefn (eval-info decl)))
			  do (print-header-decl decl output))
		    (format output "~%#endif")
		    (id theory))))

(defun pvs2rust (theoryref)
	(let* ((theory (get-typechecked-theory theoryref nil t))
		(force? t))
		(with-workspace theory
				(pvs2rust-theories (pvs2rust-preceding-theories theory) force?))))

(defun pvs2rust-theories (theories force?)
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
  (let* ((*current-pvs2rust-theory* theory)
	 (*preceding-mono-theories* nil) ; monomorphised theory instances used in this theory
	 (*theory-formals* (formals-sans-usings theory))
	 (*ir-type-info-table* nil)
	 (*c-type-info-table* nil)
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
    (loop for decl in (theory theory)
	    when (if (null indecl)
		     (or (const-decl? decl) 
			 (type-eq-decl? decl)(type-decl? decl)(adt-type-decl? decl))
		     (eq decl indecl))
	    do (progn ;(format t "~%decl ~a th ~a" decl theory)
				  (pvs2rust-decl decl force?))
	)
	))

(defun pvs2rust-decl (decl force?)
  (let (;;(saved-c-type-info-table *c-type-info-table*)
	;;(*pvs2c-current-decl* decl) ;;used to save auxilliary type defns in c-type-info-table
	;;(*current-context* (decl-context decl))
	;;(*var-counter* nil)
	;;(*pvs2c-defn-actuals* nil)
	)
    (when force? (clear-decl decl))
    ;;(newcounter *var-counter*)
    (pvs2rust-decl* decl)))

(defmethod pvs2rust-decl* ((decl type-eq-decl))
	(break "Type eq decl")
  (let ((typename (pvs2ir-decl decl)))
    ;(break "type-eq-decl")
    (when  (and (ir-typename? typename)
		(ir-type-value decl));some type definitions translate to no-ops
      (add-c-type-definition typename)))); (ir2c-type (ir-type-defn typename))(ir-type-id typename)))))

(defmethod pvs2rust-decl* ((decl formal-type-decl))
	(break "formal-type-decl")
   (pvs2ir-decl decl))

(defmethod pvs2rust-decl* ((decl formal-const-decl))
	(break "formal-const-decl")
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl))))
    (ir2c-decl* ir decl)))
  
;;conversion for a definition
(defmethod pvs2rust-decl* ((decl const-decl))
	(break "const-decl")
  (let* (;(thid (simple-id (id (module decl))))
	 (declid (simple-id (id decl)))
	 (ir-function-id (intern (format nil "~a__~a" *theory-id* declid)))
	 (hashentry (gethash ir-function-id *c-primitive-attachments-hash*)))
    (cond (hashentry
	   (let* ((einfo (or (eval-info decl)
			     (let* ((new-einfo (make-instance 'eval-info))
				    )	;all nil for now
			       (setf (eval-info decl) new-einfo)
			       new-einfo)))
		  (new-ir-function (mk-ir-function ir-function-id decl))
		  (new-ir (mk-ir-defn new-ir-function nil nil nil)))
	     (setf (ir einfo) new-ir)
	     (setf (cdefn einfo) hashentry)
	     (op-name hashentry)))
	  (t   (unless (or (adt-constructor-decl? decl)	;;these are automatically generated from the adt type decl
			   (adt-recognizer-decl? decl)
			   (adt-accessor-decl? decl))
		 (pvs2ir-decl decl))
	       (let ((ir (ir (eval-info decl)))) ;(break "pvs2c-decl*/const-decl")
		 (ir2c-decl* ir decl))))))

(defmethod pvs2rust-decl* ((decl type-decl)) ;;has to be an adt-type-decl, returns a string
  (if (adt-type-decl? decl)
	(let* (
		(name (type-value decl))
		(thname (intern (format nil "~a" *theory-id*)))
		(adt-type (type-value decl))
		(adt (adt adt-type))
		(constructors (constructors adt))
	) 
    (setf *output* "")
    (setf *functions* "")
	; (let* ((*output* (format nil "~a~%" *output*))))

	;; --- ENUM CONSTRUCTION ---
	(setf *output* (format nil "~a~%#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]~%enum ~a{" *output* name))
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(setf *output* (format nil "~a~%~a(~a)," *output* cname cname))
		)
	)
	(setf *output* (format nil "~a~%}~%" *output*))

	;; --- STRUCTS CONSTRUCTION ---
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn 
				(setf *output* (format nil "~a~%#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]~%struct ~a {" *output* cname))
				(loop for accessor in (acc-decls constructor)
					collect (setf *output* (format nil "~a~%~a: Rc<~a>," *output* (decl-id accessor) (ir2rust-type (pvs2ir-type (range (type accessor))))))
				)
			)
		)
	)

	;; --- CONSTRUCTORS --- 
	(loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn 
				(setf *output* (format nil "~a~%fn ~a__~a(" *output* name cname))
				(loop for accessor in (acc-decls constructor)
					collect (setf *output* (format nil "~a~a : ~a, " *output* (decl-id accessor) (ir2rust-type (pvs2ir-type (range (type accessor))))))
				)
				(setf *output* (format nil "~a) -> ~a {~%" *output* name))
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
				(setf *output* (format nil "~a~%fn ~a__~ap(arg : ~a) -> bool {~%match arg {~%" *output* name cname name))
				(loop for constructor2 in constructors
					collect (let* ((cname2 (decl-id (con-decl constructor2))))
						(if (eq cname cname2)
							(setf *output* (format nil "~a~a::~a(ref ~a) => true,~%" *output* name cname2 cname2))
							(setf *output* (format nil "~a~a::~a(ref ~a) => false,~%" *output* name cname2 cname2))
						)
					)
				)
				(setf *output* (format nil "~a}~%}~%" *output*))
                (setf *functions* (cons (format nil "~a__~ap" name cname) *functions*))
			)
		)
	)

	;; --- ACCESSORS ---
    (loop for constructor in constructors
		collect (let* ((cname (decl-id (con-decl constructor))))
			(progn
				(loop for accessor in (acc-decls constructor)
					collect (if (member (format nil "~a__~a" name (decl-id accessor)) *functions*)
                    nil ;; the function already exists
                    (progn
                        (setf *output* (format nil "~a~%fn ~a__~a<T>(arg : ~a) -> T {~%match arg{" *output* name (decl-id accessor) name))
                        (loop for constructor2 in constructors 
                            collect (let* ((cname2 (decl-id (con-decl constructor2))))
                            (if (member accessor (acc-decls constructor2))
                                (setf *output* (format nil "~a~%~a::~a(ref ~a) => unsafe{std::mem::transmute_copy(&Rc_unwrap_or_clone(~a.~a.clone()))}," 
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
					collect (if (member (format nil "~a__~a__update" name (decl-id accessor)) *functions*)
                    nil ;; the function already exists
                    (progn
                        (setf *output* (format nil "~a~%fn ~a__~a__update<T>(arg : ~a, ~a : T) -> ~a {~%match arg{" *output* name (decl-id accessor) name (decl-id accessor) name))
                        (loop for constructor2 in constructors 
                            collect (let* ((cname2 (decl-id (con-decl constructor2)))) ; WIP
                            (if (member accessor (acc-decls constructor2))
                                (progn
                                    (setf *output* (format nil "~a~%~a::~a(ref ~a) => ~a::~a(~a{" 
                                                *output* name cname2 cname2 name cname2 cname2))
                                    (loop for accessor2 in (acc-decls constructor2)
                                        collect (if (eq (decl-id accessor2) (decl-id accessor))
                                            (setf *output* (format nil "~a~a: Rc::new( unsafe{std::mem::transmute_copy(&~a)})," *output* (decl-id accessor2) (decl-id accessor)))
                                            (setf *output* (format nil "~a~a: ~a.~a.clone()," *output* (decl-id accessor2) cname2 (decl-id accessor2)))
                                        )
                                    )
                                    (setf *output* (format nil "~a~%})," *output*))
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



	)
  (break "Type decl is not adt-type-decl"))
  (format t "Output : ~a" *output*)
  (break "END")
  ;;
  (let* (
	 (declid (simple-id (id decl)))
	 (thname (intern (format nil "~a" *theory-id*)))
	 (hashentry (gethash thname *c-primitive-type-attachments-hash*)))
    (cond (hashentry
	   (unless *to-emacs*
	     (format t "~% attaching definition for type ~a" declid))
	   (push-type-info-to-decl hashentry decl)
	   thname)

	  (t (let* ((adt-type (type-value decl))
			(adt (adt adt-type))
			(typename (pvs2ir-adt-decl decl))
			(constructors (constructors adt))
			)
			(format t "~%$DATATYPE ~a THEORY ~a~%@(" adt-type thname)
			(loop for con in constructors
				collect (progn (
					format t "~%(CONSTRUCTOR ~a" (decl-id (con-decl con))
				)(
					loop for acc in (acc-decls con)
							    collect (if (shared-adt-accessor-decl? acc)
								(format t "~%(SHARED_ACCESSOR ~a ~a)" (decl-id acc) (print-ir (pvs2ir-type (range (type acc))))) 
								(format t "~%(ACCESSOR ~a ~a)" (decl-id acc) (print-ir (pvs2ir-type (range (type acc))))) 
								)  
				)(format t ")"))
			)
			(format t ")")
			(add-c-type-definition (ir2c-type (ir-type-defn typename))(ir-type-id typename))
			))
)))



;; IR 2 RUST TYPES , takes ir-type returns string

(defmethod ir2rust-type ((ir-typ ir-subrange))
  (format nil "i32"))

(defmethod ir2rust-type ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn type-declaration ir-actuals) ir-type
    (format nil "~a" ir-type-id)))

(defmethod ir2rust-type ((ir-typ t))
  (format t "~%~a" ir-typ)
  (break "Unsupported type"))

;; MISC

(defun adt-type-decl? (decl)
  (and (type-decl? decl)
       (adt-type-name? (type-value decl))))