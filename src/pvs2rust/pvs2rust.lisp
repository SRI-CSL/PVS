;;pvs2ir translates PVS expressions, definitions, and theories to an intermediate
;;representation (IR).  The IR consists of variables, tuples, function applications,
;;lambda-expressions, if-expressions, lets, and updates.

(in-package :pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-ir ((ir-type ir-typename))
  (with-slots (ir-type-id ir-type-defn type-declaration) ir-type
    (format nil "(~a : ~a)"
	    ir-type-id (print-ir ir-type-defn))))

(defmethod print-ir ((ir-type ir-arraytype)) 
  (with-slots (size high ir-range) ir-type
	      (format nil "(array ~a [~d/~a])" (print-ir ir-range) size (print-ir high))))



(defun pvs2ir-primitive-application (op arg-names op-arg-types args-ir ir-expr-type bindings)
  (let ((ir-op (pvs2ir-constant op bindings)))
    (with-slots (ir-fname) ir-op
      (let ((arg-var-type1 (mk-ir-variable (car arg-names)(car op-arg-types))))
	(case ir-fname
	  ((AND & ∧)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (cadr args-ir)
				 (mk-ir-bool nil))))
	  ((OR ∨)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (mk-ir-bool t)
				 (cadr args-ir))))
	  ((IMPLIES => ⇒)
	   (mk-ir-let arg-var-type1 (car args-ir)
		      (mk-ir-ift arg-var-type1
				 (cadr args-ir)
				 (mk-ir-bool t))))
	(t (let ((arg-vars (loop for ir-var in arg-names
				     as ir-typ in op-arg-types
				     collect (mk-ir-variable ir-var ir-typ))))

			(let ((real-vars (loop for arg-var in arg-vars
		 						   for arg-ir in args-ir 
								when (not (typep arg-ir 'ir-integer)) collect arg-var
							 ))
				  (corresponding-ir (loop for arg-ir in args-ir 
									when (not (typep arg-ir 'ir-integer)) collect arg-ir ;BUG ici
									))
				  (full-args (loop for arg-var in arg-vars
		 						   for arg-ir in args-ir 
								collect (if (typep arg-ir 'ir-integer) (slot-value arg-ir 'ir-intval) arg-var)
							 ))
				 )
				(mk-ir-let* real-vars
			 				corresponding-ir
			 				(mk-ir-apply (pvs2ir-constant op bindings) full-args nil ir-expr-type))
			 ))))))))

(defun pvs2ir-application (op args ir-expr-type bindings expected) ; GASC c ici qu il va falloir travailler
  (declare (ignore expected))

  (let* ((arg-names (new-irvars (length args))) ; ici que les variables sont crees
	 ;;(dummy (format t "arg-types"))
	 (args-ir (pvs2ir* args bindings nil))
	 (arg-types 
	  (loop for arg in args
	     as ir-arg in args-ir
	     collect (let ((num (pvs-integer? arg)))
		       (if num
			   (mk-ir-subrange num num)
			   (best-ir-subrange-pair (pvs2ir-expr-type arg bindings)
						  (ir-arith-type ir-arg)))))) ;;NSH(3-20-16):
	 (op-arg-types arg-types) ;;NSH(3-22-18)
	 (ir-expr-type (ir-apply-op-type op op-arg-types ir-expr-type))
					;(op-range-type (pvs2ir-type (range (find-supertype (type op)))))
	 (apply-return-var (new-irvartype ir-expr-type))
					;(dummy (format t "arg-vartypes"))
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
		       (ref-formals (loop for fml in formals
					  as act in actuals ;;collect const actuals and ref actuals
					  when (or (null (type-value act))
						   (eq (check-actual-type (type-value act) bindings) 'ref))
					  collect fml))
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
		       )
		  (if formals
		      (if ir-actuals ;;then generating code outside theory
			  (make-ir-lett* actvars actual-types ir-actuals
					 op-arg-application-ir)
			op-arg-application-ir);;generating code within theory
		  
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
		  (make-ir-let op-var op-ir 
			       (make-ir-lett* arg-vartypes
					      arg-types
					      args-ir
					      (mk-ir-let apply-return-var ;op-range-type
							 (mk-ir-apply op-var arg-vartypes nil) ;op-range-type
							 apply-return-var)))))))))


(defmethod pvs2ir* ((expr if-expr) bindings expected)
  (cond ((branch? expr)
	 (if (last-cond-expr? expr)
	     (pvs2ir* (then-part expr) bindings expected)
	   (let ((ifvar (mk-ir-variable (new-irvar) 'boolean)) ;GASC
		 (cond-ir (pvs2ir* (condition expr) bindings nil))
		 (then-ir (pvs2ir* (then-part expr) bindings expected))
		 (else-ir (pvs2ir* (else-part expr) bindings expected)))
	     (mk-ir-let  ifvar cond-ir
		 	 (mk-ir-ift ifvar then-ir else-ir))))) ; GASC changed for allowing direct ivar < 1 and avoid let ivar2 = 1
		;(mk-ir-ift cond-ir then-ir else-ir))))
	(t (call-next-method))))


(defun pvs2ir-assignments (lhs-list rhs-irvar-list
			       ir-exprvar ir-expr-type bindings)
  (cond ((consp lhs-list)
	 (let ((ir-assignment1
		(pvs2ir-assignment1 ir-expr-type
				    (car lhs-list)
				    (car rhs-irvar-list)
				    ir-exprvar bindings))
	       (ir-var1 (new-irvar)))
	   (let ((ir-vartype1 (mk-ir-variable ir-var1 ir-expr-type)))
	     (mk-ir-let  ir-vartype1 ir-assignment1
			 (pvs2ir-assignments (cdr lhs-list)(cdr rhs-irvar-list)
					     ir-vartype1 ir-expr-type bindings)))))
	(t ir-exprvar)))

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
	;;(format t "~%lhs ~a  rhs-ivar ~a ir-exprvar ~a ~%bindings ~a" lhs (print-ir rhs-irvar) ir-exprvar bindings)
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

;; TODO : Delete the null assignement
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
			)
		)
		;;(let* ((lhs1 (caar lhs));;lhs1 is a field-name-expr
		;;	(ir-field-decl (find (id lhs1) (ir-field-types ir-expr-type) :key #'ir-id))
		;;	(ir-expr-type11 (ir-vtype ir-field-decl)) ;;(pvs2ir-type (range (type lhs1)))
		;;	(ir-exprvar11 (new-irvar))
		;;	(ir-expr-vartype11 (mk-ir-variable ir-exprvar11 ir-expr-type11))
		;;	(ir-rest-assignment (if (consp (cdr lhs))
		;;				(pvs2ir-assignment1 ir-expr-type11 (cdr lhs)
		;;							rhs-irvar ir-expr-vartype11
		;;							bindings)
		;;				rhs-irvar)))
		;;	(let* ((ir-exprvar1 (new-irvar))
		;;		(ir-expr-vartype1 (mk-ir-variable ir-exprvar1 ir-expr-typename))
		;;		(ir-new-rhsvar (new-irvar))
		;;		(ir-new-rhsvartype (mk-ir-variable ir-new-rhsvar ir-expr-type11)))
		;;	(mk-ir-let ir-expr-vartype11
		;;		(mk-ir-get ir-exprvar (id lhs1));;directly get the field
		;;		(mk-ir-let ir-expr-vartype1
		;;				ir-exprvar
		;;				(mk-ir-let ir-new-rhsvartype
		;;					ir-rest-assignment
		;;					(mk-ir-update ir-expr-vartype1 (id lhs1) ir-new-rhsvartype))))))
	)
    )


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




(defun ir2c-function-apply (return-var return-type ir-function ir-args); GASC changed for allowing direct ivar < 1 and avoid let ivar2 = 1
	(let* ((ir-arg-vars (loop for ir-var in ir-args
			      collect (if (typep ir-var 'fixnum) (mk-ir-subrange ir-var ir-var) (get-ir-last-var ir-var))))
	   (ir-arg-var-names (loop for ir-var in ir-args
				   collect (if (typep ir-var 'fixnum) ir-var (ir-name (get-ir-last-var ir-var)))))
	   (ir-function-name (when (ir-function? ir-function)(ir-fname ir-function))));;(break "ir2c-function-apply")
	  (if (ir-primitive-function? ir-function)
	  (let ((instrs (ir2c-primitive-apply return-var return-type ir-function-name ir-arg-vars ir-arg-var-names)))
					;(format t "~%~a" instrs)
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
					;(format t "~%c-arg-types: ~a" c-arg-types)
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
		  ;;(format t "~%*mpvar-parameters* = ~{~a, ~}" (print-ir *mpvar-parameters*))
		;(break "ir-apply")
		  (nconc release-instrs invoke-instrs) ;nconc invoke-instrs mp-release-instrs
					       )))))


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
		  ;;(format t "~%*mpvar-parameters* = ~{~a, ~}" (print-ir *mpvar-parameters*))
		;(break "ir-apply")
		  (nconc release-instrs invoke-instrs) ;nconc invoke-instrs mp-release-instrs
					       )))))



;; PVS2C

;; Uses pvs2ir and ir2c to translate pvs code to c

(in-package :pvs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pvs2c-decl (decl force?)
  (let (;;(saved-c-type-info-table *c-type-info-table*)
	(*pvs2c-current-decl* decl) ;;used to save auxilliary type defns in c-type-info-table
	(*current-context* (decl-context decl))
	(*var-counter* nil)
	(*pvs2c-defn-actuals* nil))
    (when force? (clear-decl decl))
    (newcounter *var-counter*)
    (handler-case (pvs2c-decl* decl) (pvs2c-error (c) (declare (ignore c))(format t "~%No C code generated for ~a" (id decl))))))


    ;;NSH(8/6/2016): Closures are handled now. 
    ;; (handler-case
    ;;  (pvs2c-decl* decl)
    ;;  (pvs2c-error (condition) (format t "~%closures not handled")
    ;; 		  (setq *c-type-info-table* saved-c-type-info-table)))))
  
(defmethod pvs2c-decl* ((decl type-eq-decl))
  (let ((typename (pvs2ir-decl decl)))
    ;(break "type-eq-decl")
    (when  (and (ir-typename? typename)
		(ir-type-value decl));some type definitions translate to no-ops
      (add-c-type-definition typename)))); (ir2c-type (ir-type-defn typename))(ir-type-id typename)))))

(defmethod pvs2c-decl* ((decl type-decl)) ;;has to be an adt-type-decl
  (let* (;(thid (simple-id (id (module decl))))
	 (declid (simple-id (id decl)))
	 (thname (intern (format nil "~a__~a" *theory-id* declid)))
	 (hashentry (gethash  thname *c-primitive-type-attachments-hash*)))
    (cond (hashentry
	   (unless *to-emacs*
	     (format t "~% attaching definition for type ~a" declid))
	   (push-type-info-to-decl hashentry decl)
	   thname)
	  (t (let ((typename (pvs2ir-decl* decl)))
	       (if (adt (type-value decl))
		   (add-c-type-definition (ir2c-type (ir-type-defn typename))(ir-type-id typename))
		 (let* ((ir-type-id (ir-type-id typename))
			(c-type-info (mk-simple-c-type-info nil ir-type-id
							    (format nil "//uninterpreted type~%typedef void * ~a_t;" ir-type-id) nil (format nil "~s" (id *pvs2c-current-decl*)))))
		   (push-type-info-to-decl c-type-info decl)
		   ir-type-id)))))))

(defmethod pvs2c-decl* ((decl formal-type-decl))
   (pvs2ir-decl decl))

(defmethod pvs2c-decl* ((decl formal-const-decl))
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl))))
    (ir2c-decl* ir decl)))
  
;;conversion for a definition
(defmethod pvs2c-decl* ((decl const-decl))
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

(defun c-args-string (ir-vars)
  (let* ((c-arg-types (loop for arg in ir-vars
			    collect (add-c-type-definition (ir2c-type (ir-vtype arg)))))
	 (c-args (loop for arg in ir-vars
		       as c-arg-type in c-arg-types
		       collect (format nil "~a_t ~a"
				       (mppointer-type c-arg-type)
				       (ir-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void"))))
    c-args-string))

(defun make-c-defn-info (ir pvs-return-type) ;(break "make-c-defn-info")
  (with-slots
	(ir-function-name ir-return-type ir-args ir-defn) ir
    ;; (when (eq (id decl) 'coef)(break "coef"))
    ;;   (format t "~%ir-defn~% =~a" (print-ir ir-defn))
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
	    (format t "~%No definition for ~a" ir-function-name))
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
			  ;; (if (ir-reference-type? ir-result-type)
			  ;;     "result->count++;"
			  ;;   "")
			  ))
	       ;; (case c-result-type
	       ;; 	       ((mpq mpz)
	       ;; 		(format nil "~a{~%~a~%}"
	       ;; 		     c-header 
	       ;; 		     c-body))
	       ;; 	       (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	       ;; 		     c-header 
	       ;; 		     (mppointer-type c-result-type)
	       ;; 		     c-body)))
	       )
	  (unless *suppress-output* ;*to-emacs* ;; causes problems
	    (format t "~%$~a"  ir-function-name)
	    ;(format t "~%MPvars ~{~a, ~}" (print-ir *mpvar-parameters*))
	    ;(format t "~%Before  preprocessing = ~%~a" (print-ir pre-ir))	   
	    (format t "~%@~a~%" (print-ir post-ir))
	    ;(format t "~%Generates C definition = ~%~a" c-defn)
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
	 ;; (case c-result-type
	 ;; 	   ((mpq mpz)
	 ;; 	    (format nil "~a{~%~a~%}" c-header c-body))
	 ;; 	   (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	 ;; 		      c-header
	 ;; 		      c-result-type
	 ;; 		      c-body)))
	 )
    (unless *suppress-output* ;;*to-emacs*
      ;(format t "~%Closure After preprocessing = ~%~a" (print-ir ir-lambda-expr))
      ;(format t "~%Generates C definition = ~%~a" c-defn)
	  )
;    (break "make-c-closure-defn")
    (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn
		    c-defn-arg-types c-defn-result-type)))


(defmethod ir2c-decl* ((ir ir-accessor-defn) decl)
  (let ((cdefn (make-c-defn-info ir (type decl)))
	(udefn (make-c-defn-info (ir-update-defn ir) nil)))
    (setf (cdefn (eval-info decl)) cdefn
	  (update-cdefn (eval-info decl)) udefn)
    (op-name cdefn)))

(defmethod ir2c-decl* ((ir ir-defn) decl)
  (let ((cdefn (make-c-defn-info ir (type decl))));(break "ir2c-decl*")
    (setf (cdefn (eval-info decl)) cdefn)
    (or (and cdefn (op-name cdefn))
	(ir-uname (ir-function-name ir)))))

(defun ir-uname (fname)
  (format nil "undef_~a" fname))

;; (defmethod ir2c-decl* ((ir ir-formal-const-defn) decl)
;;   (with-slots (ir-defn) ir
;; 	      (
;; 	      (add-c-type-declaration (ir-vtype ir-defn) decl)))

(defmethod ir2c-decl* ((ir ir-constructor-defn) decl)
  (declare (ignore decl))
  (with-slots (ir-constructor-type) ir
    (with-slots (ir-type-id ir-type-defn) ir-constructor-type
      (add-c-type-definition (ir2c-type ir-type-defn) ir-type-id))
    (call-next-method)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printing out the C code in a block-structured manner
;;the translated code is a list of instructions, where each
;;instruction is either a regular C instruction or an if-instr

(defun print2c (c-instrs)
  (cond ((consp c-instrs)
	 (cond ((if-instr? (car c-instrs))
		(with-slots (if-cond then-instr else-instr) (car c-instrs)
			    (let* ((then-part
				    (let ((*c-scope-string*
					   (format nil "~a~a" "~~8T" *c-scope-string*)))
				      (print2c then-instr)))
				   (else-part
				    (let ((*c-scope-string*
					   (format nil "~a~a" "~~8T" *c-scope-string*)))
				      (print2c else-instr)))
				
				   (if-string (format nil "~aif (~a){~a~%~a} else {~%~a~%~a}"
						      *c-scope-string*
						      if-cond
						      *c-scope-string*
						      then-part
						      *c-scope-string*
						      else-part)))
			      (format nil "~a;~%~a" (format nil if-string)
				      (print2c (cdr c-instrs))))))
	       ((for-instr? (car c-instrs))
		(let* ((for-body (for-body (car c-instrs)))
		       (for-index (for-index (car c-instrs)))
		       (c-for-body (let ((*c-scope-string*
					  (format nil "~a~a" "~~8T" *c-scope-string*)))
				     (print2c for-body))))
		  (format nil (format nil "~afor (~a){~%~a~a};~%~a"
				      *c-scope-string* for-index
				      c-for-body *c-scope-string*
				      (print2c (cdr c-instrs))))))
	       ((consp (car c-instrs));;NSH(4/5/22): Added block structure
		;(break "nested block")
		(format nil
			(format nil "~a{~%~a~a};~%~a"
				*c-scope-string*
				(let ((*c-scope-string* (format nil "~8T~a" *c-scope-string*)))
				  (print2c (car c-instrs)))
				*c-scope-string*
				(print2c (cdr c-instrs)))))
	       (t (format nil (format nil "~a~a;~%~a" *c-scope-string* (car c-instrs)
				      (print2c (cdr c-instrs)))))))
	(t (format nil ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;print the header file from the information in *c-type-info-table*
;;If the type has a PVS definition, the type-info is saved with this declaration.
;;Otherwise, the type information is generated for the type expression in the global.

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
		    ;;(when (formals theory) (format t "~%typedef pointer_t"))
		    (loop for formal in (formals theory)
		      	  when (formal-type-decl? formal)
		      	  do (format output "~%~%typedef pointer_t ~a_t;" (ir-type-id (ir-type-name (ir-type-value formal)))))
		    (print-type-info-headers-to-file output *c-type-info-table*)
		    (loop for decl in (theory theory)
		     	  when (and (const-decl? decl)(eval-info decl)(cdefn (eval-info decl)))
			  do (print-header-decl decl output))
		    (format output "~%#endif")
		    ;;(unless *to-emacs*
		    ;;  (format t "~%Wrote ~a" file-string))
		    (id theory))))

(defun print-header-decl (decl output)
  (let ((einfo (eval-info decl)))
    (when (accessor-eval-info? einfo)
      (format output "~%~%~a" (op-header (update-cdefn (eval-info decl)))))
    (format output "~%~%~a" (op-header (cdefn (eval-info decl))))))

(defun print-type-info-headers-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-headers-to-file output (cdr type-info-stack))
	 (print-type-defn-headers (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defn-headers ((type-info simple-c-type-info) output)
  (when (comment-string type-info) (format output "~%//~a" (comment-string type-info)))
  (format output "~%~%~a~%~%"
	  (tdefn type-info)))

(defmethod print-type-defn-headers ((type-info closure-c-type-info) output)
  (with-slots (tdecl tdefn ftable-type-defn release-info hash-entry-type-defn hash-type-defn
		     copy-info lookup-info dupdate-info update-info equal-info json-info comment-string)
      type-info
    (when comment-string (format output "~%//~a" comment-string))
    (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	    tdecl ftable-type-defn hash-entry-type-defn hash-type-defn tdefn)
    (format output "~a~%~%" (op-header release-info))
    (format output "~a~%~%" (op-header copy-info)) ;copy is required, but functions below are optional
    (when lookup-info (format output "~a~%~%" (op-header lookup-info)))
    (when dupdate-info (format output "~a~%~%" (op-header dupdate-info)))
    (when update-info (format output "~a~%~%" (op-header update-info)))
    (when equal-info (format output "~a~%~%" (op-header equal-info)))
    (when json-info (format output  "~a~%~%" (op-header json-info)))
    ))

(defmethod print-type-defn-headers ((type-info c-type-info) output)
  (when (comment-string type-info) (format output "~%//~a" (comment-string type-info)))
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~a~%~%~a~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdefn type-info)
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  (op-header (equal-info type-info))
	  (if (json-info type-info)(op-header (json-info type-info))(format nil " "))
	  (if (act-defn type-info)(ir-actual-type-defn (act-defn type-info))(format nil " "))
	  (if (release-ptr-info type-info)(op-header (release-ptr-info type-info)) (format nil " "))
	  (if (equal-ptr-info type-info)(op-header (equal-ptr-info type-info)) (format nil " "))
	  (if (json-ptr-info type-info)(op-header (json-ptr-info type-info)) (format nil " "))	  
	  (if (act-defn type-info)(ir-actual-fun-header (act-defn type-info)))(format nil " ")) 
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-header t-info)))
  (loop for t-info in (upgrade-info type-info)
	do (format output "~a~%~%" (op-header t-info))))

(defmethod print-type-defn-headers ((type-info c-closure-info) output)
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdecl type-info)
	  (tdefn type-info)
	  (op-header (fdefn type-info))
	  (op-header (mdefn type-info))
	  (op-header (hdefn type-info))	  
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  ))

(defun print-type-info-defns-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-defns-to-file output (cdr type-info-stack))
	 (print-type-defns (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defns ((type-info simple-c-type-info) output)
  (declare (ignore output))
  nil); do nothing

(defmethod print-type-defns ((type-info closure-c-type-info) output)
  (with-slots (release-info copy-info lookup-info dupdate-info update-info equal-info json-info) type-info
	      (format output "~%~%~a" (op-defn release-info))
	      (format output "~%~%~a" (op-defn copy-info))
	      (when lookup-info (format output "~%~%~a" (op-defn lookup-info)))
	      (when dupdate-info (format output "~%~%~a" (op-defn dupdate-info)))
	      (when update-info (format output "~%~%~a" (op-defn update-info)))
	      (when equal-info (format output "~%~%~a" (op-defn equal-info)))
	      (when json-info (format output "~%~%~a" (op-defn json-info)))))


(defmethod print-type-defns ((type-info c-type-info) output)
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (if (release-ptr-info type-info)(op-defn (release-ptr-info type-info))(format nil " "))
	  (op-defn (copy-info type-info))
	  (op-defn (equal-info type-info))
	  (if (json-info type-info)(op-defn (json-info type-info))(format nil " "))
	  (if (equal-ptr-info type-info)(op-defn (equal-ptr-info type-info))(format nil " "))
	  (if (json-ptr-info type-info)(op-defn (json-ptr-info type-info))(format nil " "))
	  (if (act-defn type-info)(ir-actual-fun-defn (act-defn type-info))(format nil " ")))
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-defn t-info)))
  (loop for t-info in (upgrade-info type-info)
	do (format output "~a~%~%" (op-defn t-info))))

(defmethod print-type-defns ((type-info c-closure-info) output);(break "closure-info")
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a"
	  (op-defn (fdefn type-info))
	  (op-defn (mdefn type-info))	  
	  (op-defn (hdefn type-info))
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (op-defn (copy-info type-info))
))

(defun print-body-file (theory-id theory)
  "Generates the .c file for the given theory, returning the C file name if
successful."
  (let* ((file-string (format nil "~a_c.c"  theory-id))
	 (file-path (format nil "~a" (working-directory))))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output "//Code generated using pvs2ir2c")
      (format output "~%#include \"~a_c.h\""  theory-id)
      (print-type-info-defns-to-file output *c-type-info-table*)
      (loop for decl in (theory  theory)
	 do (let ((einfo (and (const-decl? decl)(eval-info decl))))
	      (when (and einfo (cdefn einfo))
		(let ((op-defn (op-defn (cdefn einfo))))
		  (when op-defn 
		    (when (accessor-eval-info? einfo)
		      (format output "~%~%~a" (op-defn (update-cdefn (eval-info decl)))))
		    (format output "~%~%~a" (op-defn (cdefn (eval-info decl)))))))))
      ;;(unless *to-emacs*
		;;(format t "~%Wrote ~a" file-string))
      (concatenate 'string file-path file-string))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun adt-operation? (decl)
  (typep decl
	 '(or adt-constructor-decl adt-recognizer-decl
	      adt-accessor-decl adt-def-decl)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pvs2c-theory computes definitions for the entire theory while
;;stacking type definitions in *c-type-info-table*

(defun adt-type-decl? (decl)
  (and (type-decl? decl)
       (adt-type-name? (type-value decl))))


;;commented theories are the ones that are supported by C code generation
(defparameter *primitive-prelude-theories*
  '(booleans equalities notequal if_def boolean_props xor_def quantifier_props
	     if_def equalities naturalnumbers numbers number_fields reals rational integers 
	     defined_types exists1 equality_props if_props functions functions_alt ;transpose, restrict
	     restrict_props extend_props extend_func_props ; restrict
	     extend ;K_conversion
	     K_props ;identity
	     identity_props relations orders orders_alt restrict_order_props extend_order_props
	     wf_induction measure_induction epsilons decl_params ;sets
	     sets_lemmas function_inverse_def function_inverse function_inverse_alt
	     function_image function_props function_props_alt function_props2 relation_defs
	     relation_props relation_props2 relation_converse_props indexed_sets
	     operator_defs numbers number_fields reals real_axioms bounded_real_defs
	     bounded_real_defs_alt real_types rationals integers naturalnumbers min_nat ;real_defs
	     real_props extra_real_props extra_tegies rational_props integer_props floor_ceil
	     exponentiation ;euclidean_division
	     divides modulo_arithmetic subrange_inductions bounded_int_inductions bounded_nat_inductions
	     subrange_type int_types nat_types ;exp2 integertypes
	     nat_fun_props finite_sets restrict_set_props extend_set_props function_image_aux 
					;function_iterate sequences
	     seq_functions ;finite_sequences more_finseq
					;ordinals lex2 lex3 lex4
	     list_adt
	     list_adt_map
	     list_props
	     map_props more_map_props ; filters
	     list2finseq
	     list2set
	     character_adt
	     disjointness; file strings gen_strings charstrings bytestrings
	     mucalculus ctlops fairctlops Fairctlops ; bit bv bv_concat_def
	     bv_bitwise bv_nat ; empty_bv bv_caret integer_bv_ops
	     mod  ;bv_arith_nat_defs  bv_int_defs bv_arithmetic_defs bv_extend_defs
	     infinite_sets_def finite_sets_of_sets
	     EquivalenceClosure QuotientDefinition KernelDefinition QuotientKernelProperties
	     QuotientSubDefinition QuotientExtensionProperties QuotientDistributive QuotientIteration
	     PartialFunctionDefinitions PartialFunctionComposition stdlang stdexc stdcatch stdprog
	     stdglobal stdpvs stdmath stdstr stdio stdfmap stdindent stdtokenizer stdpvsio stdsys
	     ))

(defun pvs2c-prelude ()
  (let ((*suppress-output* t))
    (dolist (theory *prelude-theories*)
      (let ((*current-context* (current-pvs-context)))
	(when (not (memq (id theory)
			 *primitive-prelude-theories*))
	  (let ((main-theory (if (datatype? theory)(adt-theory theory) theory)))
	    (pvs2c-theory* main-theory t)))))))


;; (defun used-prelude-theory-names (theory &optional prelude-theory-names)
;;   (let ((th (get-theory theory)))
;;     (unless th
;;       (error "Theory ~a not found - may need to typecheck first" theory))
;;     (dolist (decl (all-decls th))
;;       (when (typep decl '(or type-decl const-decl))
;; 	(dolist (d (refers-to decl))
;; 	  (when (and (typep d '(or type-decl const-decl))
;; 		     (from-prelude? d))
;; 	    (unless (member (module d) prelude-theory-names :test #'same-id)
;; 	      (setq prelude-theory-names
;; 		    (used-prelude-theory-names (module d)
;; 					       (cons (id (module d)) ;(mk-modname (id (module d)))
;; 						     prelude-theory-names))))))))
;;     prelude-theory-names))

(defun prelude-theory? (theory)
  (memq theory *prelude-theories*))

(defun pvs2c-preceding-prelude-theories (theory);theory should not be a prelude theory
  (let ((*preceding-prelude-theories* nil)
	(theory-defn (get-theory theory)))
    (unless (memq (id theory-defn) *primitive-prelude-theories*)
      (pvs2c-preceding-prelude-theories* theory-defn));(break "precedprelude")
    *preceding-prelude-theories*))

;; (defun pvs2c-preceding-prelude-theories-root (theory)
;;   (loop for thy in (used-prelude-theories theory)
;; 	when (not (memq (id thy) *primitive-prelude-theories*))
;; 	do (pvs2c-preceding-prelude-theories* thy))
;;   (when (prelude-theory? theory)
;;     (pushnew theory *preceding-prelude-theories* :test #'same-id)))

(defun pvs2c-preceding-prelude-theories* (theory)
  (loop for thy in (used-prelude-theories theory)
	when (not (memq (id thy) *primitive-prelude-theories*))
	do (pvs2c-preceding-prelude-theories* thy))
  (unless (from-prelude? theory)
    (let ((all-imported-theories (all-imported-theories theory)))
      (when (listp all-imported-theories);i.e., not 'unbound
	(loop for thy in all-imported-theories
	      when (not (from-prelude? thy))
	      do (pvs2c-preceding-prelude-theories* thy)))))
  (when (from-prelude? theory)
    (pushnew theory *preceding-prelude-theories* :test #'same-id)))

(defun pvs2c-preceding-theories (theory)
  (let ((*pvs2c-preceding-theories* nil)
	(theory-defn (get-theory theory)))
    (pvs2c-preceding-theories* theory-defn)
      *pvs2c-preceding-theories*))

(defun pvs2c-preceding-theories* (theory)
  (unless (eq (all-imported-theories theory) 'unbound)
    (loop for thy in (all-imported-theories theory)
	  do (pvs2c-preceding-theories* thy)))
  (unless (from-prelude? theory)
    (pushnew theory *pvs2c-preceding-theories* :test #'same-id)))

(defun pvs2c-pvs-file (fileref)
  (let ((theories (typecheck-file fileref nil nil nil t))
	(*pvs2c-preceding-theories* nil))
    (dolist (theory theories)
      (pvs2c-preceding-theories* theory))
    (pvs2c-theories *pvs2c-preceding-theories* t)))

(defun pvs2c-theory (theoryref &optional force?)
  (let* ((theory (get-typechecked-theory theoryref nil t))
	 (*context-path* (namestring (context-path theory))))
    (with-workspace theory
		    (pvs2c-theories (pvs2c-preceding-theories theory) force?))))

(defun pvs2c-theories (theories force?)
  (mapcar #'(lambda (th) (pvs2c-theory* th force?)) (reverse theories)))

(defun ir-theory-formal-type (formal)
    (if (formal-type-decl? formal)
	'type_actual
      (pvs2ir-type (type formal))))

(defun pvs2c-theory* (theory &optional force?)
  (with-workspace theory
		  (pvs2c-theory-body theory force?)))


(defun pvs2c-theory-body (theory &optional force? indecl);;*theory-id* needs to be bound by caller
  (let* ((*theory-id* (simple-id (id theory))))
    (pvs2c-theory-body-step theory force? indecl)))

(defun pvs2c-theory-body-step (theory force? indecl)
  (let* ((*current-pvs2c-theory* theory)
	 (*preceding-mono-theories* nil) ; monomorphised theory instances used in this theory
	 (*theory-formals* (formals-sans-usings theory))
	 (*ir-type-info-table* nil)
	 (*c-type-info-table* nil)
	 ;;(*closure-info-table* nil) ;;not currently used
	 ;;(formal-ids (loop for decl in *theory-formals* do (pvs2ir-decl decl)))
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
	 ;; (*c-theory-formals*
	 ;;  (ir2c-theory-formals *ir-theory-formals* *theory-formals*))
	 )
    ;(when force? (setf (ht-instance-clone theory) nil))
    (if (eq *theory-id* '|modulo_arithmetic|)
	(loop for decl in (theory theory) 
	      when (eq (id decl) '|rem|)
	      do (pvs2c-decl decl force?))
      (loop for decl in (theory theory)
	    when (if (null indecl)
		     (or (const-decl? decl) ;(or (adt-operation? decl)(def-axiom decl))
			 (type-eq-decl? decl)(type-decl? decl)(adt-type-decl? decl))
		     (eq decl indecl))
	    do (pvs2c-decl decl force?)))
	(print-header-file *theory-id* theory)
	(print-body-file *theory-id* theory)))
