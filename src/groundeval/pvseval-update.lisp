(in-package 'pvs)


(defstep eval (expr &optional destructive?)
  (let ((tc-expr (pc-typecheck (pc-parse expr 'expr)))
	(cl-expr (let ((*destructive?* destructive?))
		   (pvs2cl tc-expr)))
	(format-string
	 (format nil "~%~a ~%translates to ~s~%evaluates to ~% ~a"
	   expr cl-expr "~a") )
	(val (time (catch 'undefined (eval cl-expr))))
	(dummy (format t format-string
		 val)))
    (skip))
  "Ground evaluation of expression expr."
  "")

(defmacro pvs-funcall (fun &rest args)
  `(let ((funval ,fun))
     (if (arrayp funval)
	 (svref funval ,@args)
	 (if (pvs-outer-array-p funval)
	     (pvs-outer-array-lookup funval ,@args) 
	     (funcall funval ,@args)))))

(defvar *destructive?* nil)
(defvar *output-vars* nil)
(defvar *external* NIL)


;need to exploit the fact that at most one makes sense
;if external needs actuals, then internal will crash.

(defun make-eval-info (decl)
  (unless (eval-info decl)
    (setf (eval-info decl)
	  (make-instance 'eval-info
	    'internal (make-instance 'eval-defn-info
			'unary (make-instance 'eval-defn)
			'multiary (make-instance 'eval-defn)
			'destructive (make-instance 'eval-defn))
	    'external (make-instance 'eval-defn-info
			'unary (make-instance 'eval-defn)
			'multiary (make-instance 'eval-defn)
			'destructive (make-instance 'eval-defn))))))


(defun undefined (expr)
  (let* ((fname (gentemp "undefined"))
	 (fbody `(defun ,fname (&rest x)
		   (declare (ignore x))
		   (throw 'undefined (values nil 
			  (format nil "Hit uninterpreted term ~a during evaluation"
			    (ref-to-id ,expr)))))))
    (eval fbody)
    (compile fname)
    fname))

(defmacro trap-undefined (expr)
  `(catch 'undefined ,expr))

(defun trap-undefined-list (list)
  (loop for x in list collect `(trap-undefined ,x)))

(defmacro install-definition (expr restore body)
  `(let* ((result  (catch  'no-defn ,body))
	 (throw-string
	  (when (stringp result)
	    (format nil "~%Definition of ~a failed." ,expr))))
     (cond ((stringp result)
	    (format t "~a  ~a" throw-string result)
	    ,restore
	    (throw 'no-defn throw-string))
	   (t result))))
    

(defstruct pvs-array
  contents diffs size)

(defstruct pvs-outer-array
  inner-array offset diffs size)

(defun insert-array-diffs (diffs array)
  (insert-array-diffs* diffs array))

(defun insert-array-diffs* (diffs array)  
      (if (consp diffs)
	  (let ((array (insert-array-diffs* (cdr diffs) array)))
	    (setf (svref array (caar diffs))(cdar diffs))
	    array)
	  array))

(defun copy-pvs-array! (array arraysize)
  (cond ((pvs-array-p array) (copy-pvs-array array))
	((simple-vector-p array)
	 (let ((arr (make-array arraysize :initial-element 0)))
	   (loop for i from 0 to (1- arraysize) do
		 (setf (svref arr i)(svref array i)))
	   (make-pvs-array :contents arr :size 0)))
	(t (make-pvs-array
	    :contents (mk-fun-array array arraysize)
	    :size 0))))

(defun mk-pvs-array! (array arraysize)
  (cond ((and (pvs-array-p array) (zerop (pvs-array-size array)))
	 array)
	(t (make-pvs-array
	    :contents (mk-fun-array array arraysize)
	    :size 0))))

(defun copy-pvs-outer-array! (array arraysize)
  (cond ((pvs-outer-array-p array) (copy-pvs-outer-array array))
	((simple-vector-p array)
	 (let ((arr (make-array arraysize :initial-element 0)))
	   (loop for i from 0 to (1- arraysize) do
		 (setf (svref arr i)(svref array i)))
	   (make-pvs-outer-array
	    :inner-array (make-pvs-array :contents arr :size 0)
	    :offset 0
	    :size 0)))
	(t (make-pvs-outer-array
	    :inner-array (make-pvs-array
			  :contents (mk-fun-array array arraysize)
			  :size 0)
	    :offset 0
	    :size 0))))

(defun pvs-array-update (pvs-array at with arraysize)
  (let* ((x (copy-pvs-array! pvs-array arraysize))
	 (diffs (pvs-array-diffs x))
	 (contents (pvs-array-contents x)))
    (cond ((> (pvs-array-size x)
	      (sqrt (array-total-size contents)))
	   (let ((newarray (copy-seq contents)))
	     (setf (pvs-array-contents x)
		   (insert-array-diffs (cons (cons at with)
					     diffs)
				       newarray)
		   (pvs-array-diffs x) nil
		   (pvs-array-size x) 0)
	     x))
	  ((consp diffs)
	   (push (cons at with) (pvs-array-diffs x))
	   (incf (pvs-array-size x))
	   x)
	  (t (when (pvs-array-p pvs-array)
	       (push (cons at (svref contents at))
		     (pvs-array-diffs pvs-array))
	       (incf (pvs-array-size pvs-array)))
	     (setf (svref (pvs-array-contents x) at) with)
	     x))))

(defun size-limit (x) (sqrt x))

(defun pvs-outer-array-update (pvs-outer-array at with arraysize)
  (let* ((x (copy-pvs-outer-array! pvs-outer-array arraysize))
	 (diffs (pvs-outer-array-diffs x))
	 (offset (pvs-outer-array-offset x))
	 (outer-size (pvs-outer-array-size x))
	 (inner-array (pvs-outer-array-inner-array x))
	 (inner-size (pvs-array-size inner-array))
	 (inner-diffs (pvs-array-diffs inner-array))
	 (contents (pvs-array-contents inner-array))
	 (oldval (svref contents at))
	 (main? (eql offset inner-size)))
    (cond (main?
	   (assert (not diffs))
	   (push (cons at oldval) (pvs-array-diffs inner-array))
	   (incf (pvs-array-size inner-array))
	   (setf (svref contents at) with
		 (pvs-outer-array-offset x)
		 (pvs-array-size inner-array))
	   x)
	  (t
	    (push (cons at with) (pvs-outer-array-diffs x))
	    (incf (pvs-outer-array-size x))
	    (when (> (+ inner-size
			outer-size)
		     (size-limit (array-total-size contents)))
	      (let ((newarray (copy-seq contents)))
		(loop for (x . y) in inner-diffs ;restore inner values
		      as i from (1+ offset) to inner-size
		      do (setf (svref newarray x) y))
		(setf (pvs-outer-array-inner-array pvs-outer-array)
		      (make-pvs-array ;;restore outer diffs
			:contents 
			(insert-array-diffs
			 diffs
			 newarray)
		      :size 0)
		      (pvs-outer-array-diffs pvs-outer-array) nil
		      (pvs-outer-array-offset pvs-outer-array) 0
		      (pvs-outer-array-size pvs-outer-array)  0)))
	    x))))

(defun assoc-last (index alist count)
  (if (and (consp alist)(> count 0))
      (let ((rest-assoc (assoc-last index (cdr alist) (1- count))))
	(if (null rest-assoc)
	    (if (eql index (caar alist))
		(car alist)
		nil)
	    rest-assoc))
      nil))

(defun pvs-outer-array-lookup (outer-array index)
  (let* ((arr outer-array)
	  (ind index)
	  (inner-array (pvs-outer-array-inner-array arr)))
     (or (and (null (pvs-outer-array-diffs arr))
	      (null (pvs-array-diffs inner-array))
	      (svref (pvs-array-contents inner-array) ind))
	 (let ((lookup-diffs (assoc ind (pvs-outer-array-diffs arr))))
	   (and lookup-diffs
		(cdr lookup-diffs)))
	 (let ((lookup-inner-diffs
		(assoc-last ind (pvs-array-diffs
				inner-array)
			    (- (pvs-array-size inner-array)
			       (pvs-outer-array-offset arr)))))
	   (when lookup-inner-diffs (cdr lookup-inner-diffs)))
	 (svref (pvs-array-contents inner-array) ind))))

(defmacro pvs-array-lookup (pvs-array val)
  `(let ((arr ,pvs-array)
	 (ind ,val))
     (let ((lookup-diffs (assoc ind (pvs-array-diffs arr))))
    (or (and lookup-diffs
	     (cdr lookup-diffs))
	(svref (pvs-array-contents arr) ind)))))

;  lisp-function
;  lisp-function2
;  lisp-definition
;  lisp-definition2
;  external-lisp-function
;  external-lisp-definition
;  external-lisp-function2
;  external-lisp-definition2)


;;initialize context.  If ground expression then translate to common-lisp,
;;evaluate, then translate back.  
(defun norm (expr &optional context)
  (let ((context (or context *current-context*)))
	(cl2pvs (eval (pvs2cl expr)) (type expr) context)))

;;initialize context and translate to common-lisp with null bindings
;;and live-variables, i.e., still active in yet-to-be-evaluated expressions.  
(defun pvs2cl (expr &optional context)
  (let ((*current-context*
	 (if context context *current-context*))
	(*current-theory* (theory *current-context*))
	(*generate-tccs* 'NONE))
    (catch 'no-defn (pvs2cl_up* expr nil nil))))

;;Should be a macro.  applied function name to argument list.
(defun mk-funapp (fun args)
  `(,fun ,@args))

;;Function application for non-name functions.
;;If function is an array, then svref, else funcall


;;mk-funcall defined to be pvs-funcall for backward compatibility.
(defun mk-funcall (fun args)
  `(pvs-funcall ,fun ,@args))

(defun check-output-vars (output-vars alist livevars)
  (if (consp output-vars)
      (let* ((var (caar output-vars))
	     (lvars (cdar output-vars))
	     (vterm (cdr (assoc var alist :test #'same-declaration)))
	     (lterms (mapcar #'(lambda (x) (cdr (assoc x alist :test #'same-declaration)))
		       lvars))
	     (vterm-updateables (updateable-output-vars vterm))
	     ;;NSH(6.10.99) livevars should also be checked (g5 in arrays.pvs)
	     (lterm-updateables (append (updateable-free-formal-vars lterms)
					livevars)))
	(cond ((and (null (intersection vterm-updateables
					lterm-updateables
					:test #'same-declaration))
		    (check-output-vars (cdr output-vars) alist livevars))
	       (push-output-vars vterm-updateables lterm-updateables)
			;	 *output-vars*
	       T)
	      (t NIL)))
      T))


(defmethod pvs2cl_up* :around ((expr expr) bindings livevars)
	   (declare (ignore livevars bindings))
	   (let ((lisp-type (pvs2cl-lisp-type (type expr))))
	     (if lisp-type
		 `(the ,lisp-type
		    ,(call-next-method))
		 (call-next-method))))

(defmethod pvs2cl_up* ((expr string-expr) bindings livevars)
  (declare (ignore bindings livevars))
  (string-value expr))
  
(defun pvs2cl-operator2 (op actuals arguments livevars bindings)
  (declare (ignore bindings))
  (pvs2cl-resolution2 op)
  (let ((decl (declaration op)))
    (if *destructive?*
	(let* (;;(decl (declaration op))
	       (module-formals
		(when actuals
		  (loop for x in (formals (module decl))
			when (formal-const-decl? x)
			collect x)))
	       (defn (args2 (car (last (def-axiom decl)))))
	       (def-formals (when (lambda-expr? defn)
			      (bindings defn)))
	       (formals (if actuals
			    (append module-formals
				    def-formals)
			    def-formals))
	       (eval-defn (if actuals
			      (ex-defn-d (declaration op))
			      (in-defn-d (declaration op))))
	       (output-vars (output-vars eval-defn))
	       (check (check-output-vars
		       output-vars 
		       (pairlis
			formals
			(append actuals
				arguments))
		       livevars)))
	  (when (and (null check)
		     *eval-verbose*)
	    (format t "~%Destructive update check failed on ~a[~{~a,~}](~{~a,~})" op actuals arguments))
	  (if (or actuals
		  (eq *external* (module decl)))
	      (if check
		  (external-lisp-function-d (declaration op))
		  (external-lisp-function2 (declaration op)))
	      (if check
		  (lisp-function-d (declaration op))
		  (lisp-function2 (declaration op)))))
	(if (or actuals
		(eq *external* (module decl)))
	    (external-lisp-function2 (declaration op))
	    (lisp-function2 (declaration op))))))

(defmethod make-constant-from-decl ((decl const-decl))
  (let* ((type (type decl))
	 (res (make-resolution decl
	       (mk-modname (id (module decl)) nil)
	       type)))
    (mk-name-expr (id decl) nil nil res))) ;; 'constant

(defmethod make-constant-from-decl ((decl formal-const-decl))
  (let* ((type (type decl))
	 (res (make-resolution decl
	       (mk-modname (id (module decl)) nil)
	       type)))
    (mk-name-expr (id decl) nil nil res))) ;; 'constant
    

;;External application.  Actuals, if any, are appended to the
;;front of argument list.  
(defun mk-fun2-application (op arguments bindings livevars)
  (let* ((decl (declaration op))
	 (actuals (expr-actuals  (module-instance op)))
	 (internal-actuals
	  (or actuals
	      (and (eq (module decl) *external*)
		   (loop for x in (formals (module decl))
			 when (formal-const-decl? x)
			 collect (make-constant-from-decl x)))))
	 (args-free-formals (updateable-vars arguments))
	 (args-livevars (append args-free-formals livevars)))
    (if internal-actuals
	(mk-funapp (pvs2cl-operator2 op actuals arguments
				     livevars bindings)
	     (append (pvs2cl_up* internal-actuals
				     bindings
				     args-livevars)
		      (pvs2cl_up* arguments bindings
				 (append (mapcan #'updateable-free-formal-vars
					   actuals);;only updateable
					 ;;parameters of already evaluated
					 ;;exprs needed.
					 livevars))))
	(mk-funapp (pvs2cl-operator2 op NIL arguments livevars bindings)
		   ;;(pvs2cl-resolution2 op)
		   (pvs2cl_up* arguments  bindings livevars)))))

;;If primitive app, use pvs2cl-primitive-app.
;;If datatype constant, then ignore actuals and use pvs2cl-resolution.
;;If exactly one argument, invoke pvs2cl_up* recursive,
;; else use mk-fun2-application
;;If op is not a constant, then use recursion.
;;NSH(8.31.98): Operator free variables should be live in the arguments
;;in case these appear in the operator closure that is evaluated flg. the
;;arguments.
(defmethod pvs2cl_up* ((expr application) bindings livevars)
  (let ((op (operator expr)))
    (if (constant? op)
      (if (pvs2cl-primitive? op)
	  (pvs2cl-primitive-app expr bindings livevars)
	  (if (datatype-constant? op)
	      (mk-funapp (pvs2cl-resolution op)
			 (pvs2cl_up* (arguments expr) bindings livevars))
	      (let* ((defax (def-axiom (declaration op)))
		     (defbody (when defax (args2 (car (last defax)))))
		     (defbindings (when  (lambda-expr? defbody)
				   (bindings defbody))))
	      (if (and defax
		       (or (> (length (arguments expr)) 1)
			   (eql (length (arguments expr))
				(length defbindings))))
		  (mk-fun2-application op (arguments expr)
				       bindings livevars)
		  (let ((clop (pvs2cl_up* op bindings ;in case of actuals
				       (append (updateable-vars (argument expr))
					       livevars)))
			(clargs (list (pvs2cl_up* (argument expr)
						  bindings
						  (append ;;check if this should
						   ;;be updateable-vars
						   (updateable-free-formal-vars op)
							  livevars)))))
		    (if (and (symbolp clop) ;;if operator is a bound variable.
			     (not (member clop bindings
					  :key #'cdr)))
			(mk-funapp clop clargs)
			(mk-funcall clop clargs)))))))
      (mk-funcall (pvs2cl_up* op bindings
			      (append (updateable-vars (argument expr))
				      livevars))
		  (list (pvs2cl_up* (argument expr) bindings
				    (append (updateable-free-formal-vars op)
					     livevars)))))))

(defun pvs2cl-let-pairs (let-bindings argument declaration
				      cl-expression bindings livevars)
  (let ((args (argument-list argument)))
    (if (= (length let-bindings) (length args))
	(let* ((clargs (pvs2cl_up* args bindings livevars))
	       (let-pairs (loop for bnd in let-bindings
			    as clarg in clargs
			    collect (list bnd clarg))))
	  (if declaration
	  `(let ,let-pairs ,declaration ,cl-expression)
	  `(let ,let-pairs ,cl-expression)))
	(if (= (length let-bindings) 1)
	    (let ((let-pairs
		   (list (list (car let-bindings)
			       argument))))
	      (if declaration
		  `(let ,let-pairs ,declaration ,cl-expression)
		  `(let ,let-pairs ,cl-expression)))
	    (let* ((arg (pvs2cl_up* (car args) bindings livevars))
		   (argvar (gentemp "A"))
		   (let-pairs 
		    (loop for bnd in let-bindings
			  as i from 1
			  collect (list bnd `(project ,i ,argvar)))))
	      (if declaration
		  `(let ((,argvar ,arg))
		     (let ,let-pairs ,declaration ,cl-expression))
		  `(let ((,argvar ,arg))
		     (let ,let-pairs ,cl-expression))))))))

;;above change to pvs2cl-let-pairs simpler but untested.
;  (if (consp let-bindings)
;      (let ((cl-bnd1 (pvs2cl_up* (car args) bindings)))
;	(cons (list (cdar let-bindings) cl-bnd1)
;	      (pvs2cl-let-pairs (cdr let-bindings)(cdr args)
;				(cons (car let-bindings)
;				      bindings))))
;      nil))

;;NSH: pay attention to pattern-matching LETs.

(defun pvs2cl-let-expression (expression bindings livevars
					 ;let-variables
					 let-pairs
					 let-bindings)
  (cond (*destructive?*
	 (multiple-value-bind
	     (cl-expression output-vars)
	     (let* (;(bvar-expr-pairs (pairlis let-bindings args))
		    ;(var-expr-pairs (pairlis let-variables args))
		    (*output-vars* nil)
		    (cl-expression (pvs2cl_up* expression
					       bindings
					       livevars))
		    (check (check-output-vars
			    *output-vars*
			    let-pairs
			    livevars))
		    (output-vars
		     (loop for (x . y) in *output-vars*
			   when (not (member x let-bindings
					     :test #'same-declaration))
			   collect
			   (cons x
				 (set-difference y let-bindings
						 :test #'same-declaration)))))
	       (if check
		   (values cl-expression output-vars)
		   (let* ((*destructive?* nil)
			  (cl-expression (pvs2cl_up* expression
						     bindings
						     livevars)))
							
		     (values cl-expression nil))))
	   (loop for (x . y) in output-vars
		 do (push-output-var x y)) ; *output-vars*
	 cl-expression))
	(t (pvs2cl_up* expression bindings livevars))))

(defmethod pvs2cl_up* ((expr formal-const-decl) bindings livevars)
  (declare (ignore livevars))
  (let ((result (assoc expr bindings)))
    (if result (cdr result)
	(throw 'no-defn (format nil "No binding for ~a" expr)))))

(defun pvs2cl-declare-vars (vars exprs)
  (let ((decls (pvs2cl-declare-vars* vars exprs)))
    (when decls
      `(declare ,@decls))))

(defun pvs2cl-declare-vars* (vars exprs)
  (when (consp vars)
    (let* ((var (car vars))
	     (expr (car exprs))
	     (expr-lisp-type (pvs2cl-lisp-type (type expr))))
	(if expr-lisp-type
	    (cons (list 'type expr-lisp-type var)
		  (pvs2cl-declare-vars* (cdr vars)(cdr exprs)))
	    (pvs2cl-declare-vars* (cdr vars)(cdr exprs))))))

(defun pvs2cl-declare-vars-types (vars types)
  (let ((decls (loop for var in vars
		     as typ in types
		     collect (list 'type (pvs2cl-lisp-type typ) var))))
    `(declare ,@decls)))

						    
(defmethod pvs2cl_up* ((expr let-expr) bindings livevars)
  (let* ((let-bindings (bindings (operator expr)))
	(arg (argument expr))
	(args (arguments expr))
	(arg-type (find-supertype (type arg))) 
	(expression (expression (operator expr))))
    (let* ((let-variables
	    (pvs2cl-make-bindings  let-bindings bindings))
	   (let-binding-pairs (pairlis let-bindings let-variables))
	   (let-vars-args-pairs
	    (if (= (length let-bindings)(length args))
		(pairlis let-bindings args)
		(loop for x in let-bindings
		      collect (cons x arg))))
	   (declaration (pvs2cl-declare-vars-types
			 let-variables
			 (if (tupletype? arg-type)
			     (types arg-type)
			     (list (type arg)))))
	   (cl-expression (pvs2cl-let-expression
			   expression (append let-binding-pairs bindings)
			    livevars
			   let-vars-args-pairs
			   let-bindings)))
      (pvs2cl-let-pairs
	    let-variables
	    arg declaration
	    cl-expression
	    bindings;;operator free-formal-vars
	    ;;are always updateable.
	    (append (updateable-vars (operator expr))
		    livevars)))))


;;let* not needed, nested anyway.

(defmethod pvs2cl_up* ((expr if-expr) bindings livevars)
  (cond ((branch? expr)
	 (let ((condition (condition expr))
	       (then-part (then-part expr))
	       (else-part (else-part expr)))
	 `(if ,(pvs2cl_up* condition bindings
			   (append (updateable-vars then-part)
				   (append (updateable-vars else-part)
					   livevars)))
	      ,(pvs2cl_up* (then-part expr) bindings livevars)
	      ,(pvs2cl_up* (else-part expr) bindings livevars))))
	(t (call-next-method))))

(defmethod pvs2cl_up* ((expr cases-expr) bindings livevars)
  (pvs2cl_up* (mk-translate-cases-to-if expr) bindings livevars))

(defmethod pvs2cl_up* ((expr lambda-expr) bindings livevars)
  (declare (ignore livevars))
  (pvs2cl-lambda (bindings expr) (expression expr) bindings
		 (updateable-vars expr))) ;;unsafe to update any freevar

(defmethod pvs2cl_up* ((expr forall-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (sub (simple-subrange? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (mk-forall-expr
				  bind-rest
				body)
			      body)))
	  (cond ((enum-adt? typ1)
		 `(every ,(pvs2cl-lambda (list bind1)
					expr-rest
					bindings
					(append (updateable-vars expr)
						livevars))
			(list ,@(pvs2cl_up* (constructors typ1)
					    bindings livevars))))
		(sub
		  (let ((i (gentemp)))
		    `(loop for ,i
			   from ,(car sub)
			   to ,(cdr sub)
			   always
			   (pvs-funcall ,(pvs2cl-lambda (list bind1)
							expr-rest
							bindings
							(append (updateable-vars expr)
								livevars))
					,i))))
		(t (throw 'no-defn
			  "Cannot handle non-scalar/subrange quantifiers."))))
	(pvs2cl_up* body bindings livevars))))

(defmethod pvs2cl_up* ((expr exists-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (sub (simple-subrange? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (mk-forall-expr
				  bind-rest
				body)
			      body)))
	  (cond ((enum-adt? typ1)
		 `(some ,(pvs2cl-lambda (list bind1)
					expr-rest
					bindings
					(append (updateable-vars expr)
						livevars))
			(list ,@(pvs2cl_up* (constructors typ1)
					    bindings livevars))))
		(sub
		  (let ((i (gentemp)))
		    `(loop for ,i
			   from ,(car sub)
			   to ,(cdr sub)
			   thereis
			   (pvs-funcall ,(pvs2cl-lambda (list bind1)
							expr-rest
							bindings
							(append (updateable-vars expr)
								livevars))
					,i))))
		(t (throw 'no-defn
			  "Cannot handle non-scalar/subrange quantifiers."))))
	(pvs2cl_up* body bindings livevars))))
			

(defmethod pvs2cl_up* ((expr name-expr) bindings livevars)
  (let* ((decl (declaration expr))
	 (bnd (assoc  decl bindings :key #'declaration)))
    (assert (not (and bnd (const-decl? decl))))
    (if bnd
	(if (const-decl? decl)
	    (mk-funapp (cdr bnd)
			(pvs2cl-actuals (expr-actuals (module-instance expr))
					bindings livevars))
	    (cdr bnd)) 
	(if (const-decl? decl)
	    (pvs2cl-constant expr bindings livevars)
	    (let ((str (format NIL "~%~a is not translateable." expr)))
	      (throw 'no-defn str)
	      )))))

(defmethod pvs2cl_up* ((expr list) bindings livevars)
  (if (consp expr)
      (cons (pvs2cl_up* (car expr) bindings
			(append (updateable-vars (cdr expr)) livevars))
	    (pvs2cl_up* (cdr expr) bindings  ;;need car's freevars
			(append (updateable-free-formal-vars (car expr)) ;;f(A, A WITH ..)
				livevars)))
      nil))

(defmethod pvs2cl_up* ((expr number-expr) bindings livevars)
  (declare (ignore bindings livevars))
  (number expr))

(defmacro pvs2cl_tuple (&rest args)
  (let ((protected-args (loop for x in args collect `(trap-undefined ,x))))
    `(vector ,@protected-args)))

(defmacro pvs2cl_record (&rest args)
  (let ((protected-args (loop for x in args collect `(trap-undefined ,x))))
    `(vector ,@protected-args)))

(defmethod pvs2cl_up* ((expr tuple-expr) bindings livevars)
  (let ((args (pvs2cl_up* (exprs expr) bindings livevars)))
    `(pvs2cl_tuple ,@args)))

(defmethod pvs2cl_up* ((expr record-expr) bindings livevars)
  (let ((args (pvs2cl_up* (mapcar #'expression
			    (sort-assignments (assignments expr)))
			  bindings livevars)))
    `(pvs2cl_record ,@args)))

(defmethod pvs2cl_up* ((expr projection-application) bindings livevars)
    `(project ,(index expr) ,(pvs2cl_up* (argument expr) bindings livevars)))

(defun sorted-fields (typ)                
  (let ((restructure   (copy-list (fields typ))))     
    (sort restructure                            
          #'string-lessp                         
          :key #'(lambda (x) (id x)))))

(defun get-field-num (id typ)
  (position id (sorted-fields typ)
            :test #'(lambda (x y) (eq x (id y)))))

(defmethod pvs2cl_up*  ((expr field-application) bindings livevars)
  (let* ((clarg (pvs2cl_up* (argument expr) bindings livevars))
	 (argtype (find-supertype (type (argument expr))))
	 (nonstr `(project ,(1+ (get-field-num (id expr) argtype)) ,clarg)))
    (if (finseq-type? argtype)
	(if (eq (id expr) '|length|)
	    `(if (stringp ,clarg) (length ,clarg) ,nonstr)
	    `(if (stringp ,clarg) (lambda (x) (schar ,clarg x)) ,nonstr))
	nonstr)))


(defmethod no-livevars? ((expr update-expr) livevars)
  (no-livevars? (expression expr) livevars))

(defmethod no-livevars? ((expr T) livevars)
  (let ((updateable-outputs
	 (updateable-outputs expr)))
    (and (null (intersection  updateable-outputs
			 livevars
			 :test #'same-declaration))
	 (cons updateable-outputs livevars))))

(defvar *fun-lhs* nil)
(defvar *update-args-bindings* nil)
(defvar *lhs-args* nil)

(defun array-bound (type)
  (and (funtype? type)
       (let ((x (simple-below? (domain type))))
	 (or x
	     (let ((y (upto? (domain type))))
	       (or (and y (1+ y) )
		   (let ((stype (find-supertype type)))
		     (and (enum-adt? stype)
			  (1- (length (constructors stype)))))))))))

(defun pvs2cl-update (expr assigns bindings livevars)
  (let* ((expr-type (find-supertype (type expr)))
	 (cl-expr (pvs2cl_up* expr bindings
			      ;;RHS vars needed here in case
			      ;;expr tries to update one of them.
			      (append (loop for x in assigns
					   nconc
					   (updateable-free-formal-vars
					    (expression x)))
				     livevars))))
    (pvs2cl-update* expr-type cl-expr
		    assigns bindings
		    (append (updateable-vars expr) livevars)))) 

(defun pvs2cl-update* (type cl-expr assigns bindings livevars)
    (if (consp assigns)
      (let* ((args (arguments (car assigns)))
	    (rhs-expr (expression (car assigns)))
	    (rhsvar (gentemp "RHS"))
	    (rhs (pvs2cl_up* rhs-expr bindings
			     livevars))
	    (cl-expr (if (funtype? type)
			 (let* ((bound (array-bound type))
				(cl-bound (pvs2cl_up* bound
						      bindings livevars)))
			   `(mk-fun-array ,cl-expr ,cl-bound))
			 cl-expr))
	    (exprvar (gentemp "E")))
	(let* ((*lhs-args* nil)
	       (new-cl-expr
		(pvs2cl-update-assign-args type exprvar args rhsvar
					   bindings
					   (append (updateable-vars rhs-expr)
						   livevars)))
	       (lhs-bindings (nreverse *lhs-args*))
	       (new-expr-body `(let ((,rhsvar ,rhs)
				     (,exprvar ,cl-expr))
				 ,new-cl-expr
				 ,exprvar))
	       (newexpr-with-let
		(if lhs-bindings 
		    `(let ,lhs-bindings ,new-expr-body)
		    new-expr-body))
		   )
	  (pvs2cl-update*
	   type newexpr-with-let (cdr assigns) bindings
	   ;;because later assignments are evaluated first.
	   (append (updateable-vars (car assigns))
			   livevars))))
      cl-expr))

;;main point is that nested translations for arrays must be
;;nondestructive since there is a possibility of aliasing.
(defmethod pvs2cl-update-assign-args ((type funtype) cl-expr args rhs
					 bindings livevars)
      (let* ((args1 (car args))
	     (cl-args1 (pvs2cl_up* (car args1) bindings
					   livevars))
	     (lhsvar (gentemp "LHS"))
	     (bound (array-bound type))
	     (cl-bound (pvs2cl_up* bound
				   bindings livevars))
	     (cl-expr `(mk-fun-array ,cl-expr ,cl-bound))
	     (cl-expr-var (gentemp "E"))
	     (newexpr `(svref ,cl-expr-var ,lhsvar))
	     (newrhs (if (null (cdr args))
		      rhs
		      (pvs2cl-update-nd-type
		      (range type) newexpr (cdr args) rhs
		      bindings livevars))))
	(push (list lhsvar cl-args1) *lhs-args*)
	`(let ((,cl-expr-var ,cl-expr))
	   (setf ,newexpr ,newrhs)
	   ,cl-expr-var)))

(defmethod pvs2cl-update-assign-args ((type subtype) cl-expr args rhs
					 bindings livevars)
  (pvs2cl-update-assign-args (find-supertype type) cl-expr args
			     rhs bindings livevars))

(defmethod pvs2cl-update-assign-args ((type recordtype) cl-expr args rhs
					 bindings livevars)
  (let* ((args1 (car args))
	 (id (id (car args1)))
	 (field-num (get-field-num  id type))
	 (cl-expr-var (gentemp "E"))	 
	 (newexpr `(svref ,cl-expr-var ,field-num))
	 (field-type (type (find id (fields type) :key #'id) ))
	 (other-updateable-types
	  (loop for fld in (fields type)
		when (not (eq id (id fld)))
		nconc (top-updateable-types (type fld) nil)))
	 (newrhs  (if (null (cdr args))
		      rhs
		      (if (member field-type
				  other-updateable-types
				  :test #'compatible?)
			  (pvs2cl-update-nd-type
			   field-type newexpr (cdr args) rhs
			   bindings livevars)
			  (pvs2cl-update-assign-args
			   field-type newexpr (cdr args) rhs
			   bindings livevars)))))
    `(let ((,cl-expr-var ,cl-expr))
       (setf ,newexpr ,newrhs)
       ,cl-expr-var)))

(defmethod pvs2cl-update-assign-args ((type tupletype) cl-expr args rhs
					 bindings livevars)
  (let* ((args1 (car args))
	 (num (number (car args1)))
	 (cl-expr-var (gentemp "E"))
	 (newexpr `(svref ,cl-expr-var ,(1- num)))
	 (tupsel-type (nth (1- num)(types type)))
	 (other-updateable-types
	  (loop for fld in (types type)
		as pos from 1  
		when (not (eql pos num))
		nconc (top-updateable-types fld nil)))
	 (newrhs  (if (null (cdr args))
		      rhs
		      (if (member tupsel-type
			      other-updateable-types
			      :test #'compatible?)
		      (pvs2cl-update-nd-type
		       tupsel-type newexpr (cdr args) rhs
		       bindings livevars)
		      (pvs2cl-update-assign-args
		       tupsel-type newexpr (cdr args) rhs
		       bindings livevars)))))
    `(let ((,cl-expr-var ,cl-expr))
       (setf ,newexpr ,newrhs)
       ,cl-expr-var)))
	      
  
	
(defun mk-fun-array (expr size) 
  (if (or (simple-vector-p expr)(null size))
      expr
      (cond ((pvs-outer-array-p expr)
	     (let* ((arr (make-array size :initial-element 0))
		    (inner-array (pvs-outer-array-inner-array expr))
		    (contents (pvs-array-contents inner-array))
		    (inner-size (pvs-array-size inner-array))
		    (offset (pvs-outer-array-offset expr))
		    (outer-diffs (pvs-outer-array-diffs expr))
		    (inner-diffs (pvs-array-diffs inner-array)))
	       (loop for i from 0 to (1- size) do
		     (setf (svref arr i)(svref contents i)))
	       (loop for (x . y) in inner-diffs
		     as i from (1+ offset) to inner-size
		     do (setf (svref arr x) y))
	       (insert-array-diffs outer-diffs arr)))
	    (t (let ((arr (make-array size :initial-element 0)))
		 (loop for i from 0 to (1- size) do
		       (setf (svref arr i)(funcall expr i)))
		 arr)))))

(defun push-output-vars (updateable-vars livevars)
    (loop for x in updateable-vars
	  do (push-output-var x livevars))
    *output-vars*)

(defun push-output-var (updateable-var livevars )
  (let ((x-output (assoc (declaration updateable-var) *output-vars*
			 :key #'declaration)))
    (if x-output
	(loop for y in livevars
	      do (pushnew y (cdr x-output)
			  :test #'same-declaration))
	(push (cons updateable-var livevars) *output-vars*))))

(defmethod pvs2cl-update-nondestructive
    ((expr update-expr) bindings livevars)
  (with-slots (type expression assignments) expr
    (let* ((assign-exprs (mapcar #'expression assignments))
	   (cl-expr (pvs2cl_up* expression bindings
				(append (updateable-free-formal-vars
					 assign-exprs)
					;;assign-args can be ignored
					livevars))))
    (pvs2cl-update-nondestructive* (type expression)
				   cl-expr
				   (mapcar #'arguments assignments)
				   assign-exprs
				   bindings
				   (append (updateable-vars expression)
					   livevars)))))

;;recursion over updates in an update expression
(defun pvs2cl-update-nondestructive*
    (type expr assign-args assign-exprs bindings livevars)
  (if (consp assign-args)
      (let* ((*lhs-args* nil)
	     (exprvar (gentemp "E"))
	     (assign-exprvar (gentemp "N"))
	     (cl-assign-expr
	      (pvs2cl_up* (car assign-exprs)
			  bindings
			  (append (updateable-vars (cdr assign-exprs))
				  (append (updateable-vars (cdr assign-args))
					  livevars))))
	     (newexpr (pvs2cl-update-nd-type
		       type exprvar
		       (car assign-args)
		       assign-exprvar
		       bindings
		       (append (updateable-vars (car assign-exprs))
			       (append (updateable-vars (cdr assign-exprs))
				       (append (updateable-vars (cdr assign-args))
					       livevars)))))
	     (lhs-bindings (nreverse *lhs-args*))
	     (newexpr-with-let
	      `(let ,lhs-bindings
		 (let ((,assign-exprvar ,cl-assign-expr)
		       (,exprvar ,expr))
		   ,newexpr))))
	(pvs2cl-update-nondestructive*
	 type 
	 newexpr-with-let
	 (cdr assign-args)(cdr assign-exprs) bindings
	 (append (updateable-free-formal-vars (car assign-exprs))
		 livevars)))
      expr))


;;recursion over nested update arguments in a single update.
(defun pvs2cl-update-nd-type (type expr args assign-expr
					   bindings livevars)
  (if (consp args)
      (pvs2cl-update-nd-type* type expr (car args) (cdr args) assign-expr
			      bindings livevars)
      assign-expr))

(defmethod pvs2cl-update-nd-type* ((type funtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((bound (array-bound type))
	 (cl-bound (pvs2cl_up* bound bindings livevars))
	 (arg1var (gentemp "A"))
	 (cl-arg1 (pvs2cl_up*  (car arg1) bindings
			      (append (updateable-vars restargs)
				      livevars)))
	 (newexpr (pvs2cl-update-nd-type 
		   (range type) (mk-funcall expr (list arg1var))
		   restargs assign-expr bindings livevars)))
    (push (list arg1var cl-arg1) *lhs-args*)
    `(pvs-outer-array-update ,expr ,arg1var ,newexpr ,cl-bound)))

(defmacro nd-rec-tup-update (rec fieldnum newval)
  `(let ((val ,newval)
	(newrec  (copy-seq ,rec)))
    (setf (svref newrec ,fieldnum) val)
    newrec))

(defmethod pvs2cl-update-nd-type* ((type recordtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((id (id (car arg1)))
	 (field-num (get-field-num  id type))
	 (new-expr `(svref ,expr ,field-num))	 
	 (field-type (type (find id (fields type) :key #'id) ))
	 (newval (pvs2cl-update-nd-type field-type new-expr
					restargs assign-expr bindings
					livevars)))
    `(nd-rec-tup-update ,expr ,field-num ,newval)))

(defmethod pvs2cl-update-nd-type* ((type tupletype)  expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((num (number (if (consp arg1) (car arg1) arg1)))
	 (new-expr `(svref ,expr ,(1- num)))
	 (tupsel-type (nth (1- num)(types type)))
	 (newval (pvs2cl-update-nd-type tupsel-type new-expr
					restargs assign-expr bindings
					livevars)))
    `(nd-rec-tup-update ,expr ,num ,newval)))

(defmethod pvs2cl-update-nd-type* ((type subtype)  expr arg1 restargs
				   assign-expr bindings livevars)
  (pvs2cl-update-nd-type* (find-supertype type) expr arg1 restargs
			  assign-expr bindings livevars))


;;assign-arg-livevars can be ignored since args are evaluated before
;;expression and have no updateable results. 		 
;;even the RHS-livevars can be ignored since the nested updates
;;are done nondestructively.  
(defmethod pvs2cl_up* ((expr update-expr) bindings livevars)
  (if (updateable? (type (expression expr)))
      (if (and *destructive?*
	       (not (some #'maplet? (assignments expr))))
	  (let* ((expression (expression expr))
		 (assignments (assignments expr))
		 (updateable-livevars
		  (no-livevars? expression livevars)))
	    ;;very unrefined: uses all
	    ;;freevars of eventually updated expression.
	    (cond (updateable-livevars
		   (push-output-vars (car updateable-livevars)
				     (cdr updateable-livevars))
		   (pvs2cl-update expression
				  assignments
				  bindings livevars))
		  (t (pvs2cl-update-nondestructive  expr
						    bindings livevars))))
	  (pvs2cl-update-nondestructive expr bindings livevars))
      (pvs2cl_up* (translate-update-to-if! expr)
		  bindings livevars)))

(defun pvs2cl-newid (id bindings)
  (loop for i from 0
	thereis
	(let ((tmp
	       (makesym "~a#~a" id i)))
	  (and (null (rassoc tmp bindings))
	       tmp))))
    
(defun pvs2cl-lambda (bind-decls expr bindings livevars)
  (let* ((bind-ids (pvs2cl-make-bindings bind-decls bindings))
	 (cl-body (pvs2cl_up* expr
			   (append (pairlis bind-decls bind-ids)
				   bindings)
			   livevars)))
    (if (eql (length bind-ids) 1)
	`(function (lambda ,bind-ids ,cl-body))
	(let* ((lamvar
		(pvs2cl-newid 'lamvar bindings))
	       (letbind
		(loop for bind in bind-ids
		      as i from 1
		      collect
		      `(,bind (project ,i ,lamvar))))
	       (let-decls (pvs2cl-declare-vars  bind-ids bind-decls)))
	  (if let-decls
	      `(function
		(lambda (,lamvar)
		  (let ,letbind ,let-decls ,cl-body)))
	      `(function
		(lambda (,lamvar)
		  (let ,letbind ,cl-body))))))))



(defun pvs2cl-make-bindings (bind-decls bindings)
  (if (consp bind-decls)
      (let* ((bb (car bind-decls))
	     (id (id bb))
	     (newid (if (null (rassoc id bindings))
			id
			(pvs2cl-newid (id bb) bindings))))
	(cons newid (pvs2cl-make-bindings (cdr bind-decls) bindings)))
      nil))
      

(defun pvs2cl-bindings (bind-decls bindings)
  (if (consp bind-decls)
      (let* ((bind (car bind-decls))
	     (newid
	      (if (rassoc bind bindings)
		  (pvs2cl-newid (id bind) bindings)
		  (id bind))))
	(pvs2cl-bindings (cdr bind-decls)
		      (cons (cons bind newid) bindings)))
      bindings))

(defparameter *primitive-constants* '(TRUE FALSE |null|))

(defun pvs2cl-constant (expr bindings livevars)
  (cond ((pvs2cl-primitive? expr)
	 (if (memq (id expr) *primitive-constants*) ;the only constants
	     (pvs2cl-primitive expr)	;else need to return closures.
	     `(function ,(pvs2cl-primitive expr))))
	(t (pvs2cl-resolution expr)
	   (if (datatype-constant? expr)
	       (if (scalar-constant? expr)
		   (lisp-function (declaration expr))
		   (let ((fun (lisp-function (declaration expr))))
		     (if (not (funtype? (find-supertype (type expr))))
			 (mk-funapp fun nil)
			 `(function ,fun))));;actuals irrelevant for datatypes
	       (let* ((actuals (expr-actuals (module-instance expr)))
		      (decl (declaration expr))
		      (defns (def-axiom decl))
		      (defn (when defns(args2 (car (last (def-axiom decl))))))
		      (def-formals (when (lambda-expr? defn)
				     (bindings defn)))
		      (fun (if defns
			       (if def-formals
				   (if actuals
				       (external-lisp-function (declaration expr))
				       (lisp-function (declaration expr)))
				   (pvs2cl-operator2 expr actuals nil
						     livevars bindings))
			       (if actuals
				   (external-lisp-function (declaration expr))
				   (lisp-function (declaration expr))))))
	    (mk-funapp fun (pvs2cl_up* actuals
				       bindings livevars)))))))

(defun expr-actuals (modinst)
  (loop for act in (actuals modinst)
	when (null (type-value act))
	collect (expr act)))

(defun pvs2cl-actuals (actuals bindings livevars)
  (if (consp actuals)
      (if (type-value (car actuals))
	  (pvs2cl-actuals (cdr actuals) bindings livevars)
	  (cons (pvs2cl_up* (expr (car actuals)) bindings livevars)
		(pvs2cl-actuals (cdr actuals) bindings livevars)))
      nil))

(defun datatype-constant? (expr)
  (adt-name-expr? expr))

(defun pvs2cl-resolution2 (expr)
  (pvs2cl-resolution expr)
  (if (or (expr-actuals (module-instance expr))
	  (eq (module (declaration expr)) *external*)) 
      (external-lisp-function2 (declaration expr))
      (lisp-function2 (declaration expr))))

(defun lisp-function (decl)
  (or (in-name decl)(in-name-m decl)))

(defun lisp-function2 (decl)
  (or (in-name-m decl)(in-name decl)))

(defun external-lisp-function (decl)
  (or (ex-name decl)(ex-name-m decl)))

(defun external-lisp-function2 (decl)
  (or (ex-name-m decl)(ex-name decl)))

(defun lisp-function-d (decl)
  (or (in-name-d decl)(lisp-function2 decl)))

(defun external-lisp-function-d (decl)
  (or (ex-name-d decl)(external-lisp-function2 decl)))

(defun pvs2cl-resolution (expr)
  (let* ((decl (declaration expr))
	 (*current-context* (saved-context (module decl)))
	 (*current-theory* (theory *current-context*)))
    (unless (eval-info decl)
      (make-eval-info decl))
    (if (datatype-constant? expr)
	(or (lisp-function (declaration expr))
	    (pvs2cl-datatype expr))
	(if (or (eq (module decl) *external*)
		(expr-actuals (module-instance expr)));;datatype-subtype?
	    (or (external-lisp-function (declaration expr))
		(catch 'no-defn
		  (pvs2cl-external-lisp-function (declaration expr))))
	    (or (lisp-function (declaration expr))
		(catch 'no-defn (pvs2cl-lisp-function (declaration expr)))))
	)))

(defun mk-newsymb (x &optional (counter 0))
  (let ((str (format nil "~a_~a" x counter)))
    (if (find-symbol str)
	(mk-newsymb x (1+ counter))
	(intern str))))


(defun pvs2cl-external-lisp-function (decl)
  (let* ((defax (def-axiom decl))
	 (*external* (module decl))
	 (*current-theory* (module decl)))
    (cond ((null defax)
	   (make-eval-info decl)
	   (let ((undef (undefined decl)))
	     (setf (ex-name decl) undef
		   (ex-name-m decl) undef
		   (ex-name-d decl) undef)
	     undef))
	  (t (let
		 ((formals (loop for x in (formals (module decl))
				 when (formal-const-decl? x)
				 collect x)))
	       (if (null formals)
		   (or (lisp-function decl)
		       (pvs2cl-lisp-function decl))
		   (let* ((id (mk-newsymb (format nil "~a_~a"
					    (id (module decl))
					    (id decl))))
			  (id-d (mk-newsymb (format nil "~a!~a"
					      (id (module decl))
					      (id decl))))
			  (formal-ids (loop for x in formals
					    collect (id x)))
			  (bindings (pairlis formals formal-ids))
			  (defn (args2 (car (last defax))))
			  (defn-bindings (when (lambda-expr? defn)
					   (bindings defn)))
			  (defn-expr (if (lambda-expr? defn)
					 (expression defn)
					 defn))
			  (defn-binding-ids
			    (pvs2cl-make-bindings defn-bindings bindings))
			  (formal-ids2 (append formal-ids
					       defn-binding-ids))
			  (declarations
			   (pvs2cl-declare-vars formal-ids2
						(append formals defn-bindings))))
						    
		     (make-eval-info decl)
		     (setf (ex-name decl) id)
		     (let ((id2 (mk-newsymb (format nil "~a__~a"
					      (id (module decl))
					      (id decl))))
			   )
		       (install-definition
			(id decl)
			(setf (ex-name-m decl) 'no-defn)
			(progn (setf (ex-name-m decl) id2)
			       (let ((*destructive?* nil))
				 (setf (definition (ex-defn-m decl))
				       `(defun ,id2 ,formal-ids2
					  ,@(append (when declarations
						      (list declarations))
						    (list 
						     (pvs2cl_up* defn-expr
								 (append (pairlis
									  defn-bindings
									  defn-binding-ids)
									 bindings)
								 nil))))))
			       (eval (definition (ex-defn-m decl)))
			       (assert id2)
			       (compile id2)
			       id2)))
		     (install-definition
		      (id decl)
		      (setf (ex-name-d decl) 'no-defn)
		      (progn (setf (ex-name-d decl) id-d)
			     (let ((*destructive?* T)
				   (*output-vars* NIL))
			       (setf (definition (ex-defn-d decl))
				     `(defun ,id-d ,formal-ids2
					,declarations
					,@(append (when declarations
						    (list declarations))
						  (list 
						   (pvs2cl_up* defn-expr
							       (append (pairlis defn-bindings
										defn-binding-ids)
								       bindings)
							       nil)))))
			       (setf (output-vars (ex-defn-d decl))
				     *output-vars*))
			     (eval (definition (ex-defn-d decl)))
			     (assert id-d)
			     (compile id-d)
			     id-d))
		     (install-definition
		      (id decl)
		      (setf (ex-name decl) 'no-defn)
		      (progn
			(let ((*destructive?* NIL)
			      (declarations (pvs2cl-declare-vars formal-ids formals)))
			  (setf (definition (ex-defn decl))
				`(defun ,id ,formal-ids
				   ,@(append (when declarations
					       (list declarations))
					     (list 
					      (pvs2cl_up* defn  bindings nil))))))
			(eval (definition (ex-defn decl)))
			(assert id)
			(compile id)
			id)))))))))

(defun pvs2cl-lisp-function (decl)
  (let* ((defax (def-axiom decl))
	 (*external* nil))
    (cond ((null defax)
	   (let ((undef (undefined decl)))
	     (setf (in-name decl) undef
		   (in-name-m decl) undef
		   (in-name-d decl) undef)
	     undef))
	  (t (let* 
		 ((id (mk-newsymb  (id decl)))
		  (id-d (mk-newsymb (format nil "~a!" (id decl))))
		  (defn (args2 (car (last (def-axiom decl)))))
		  (defn-bindings (when (lambda-expr? defn)
				   (bindings defn)))
		  (defn-body (if (lambda-expr? defn)
				 (expression defn)
				 defn))
		  (defn-binding-ids (mapcar #'id defn-bindings))
		  (declarations (pvs2cl-declare-vars defn-binding-ids
						     defn-bindings)))
	       (setf (in-name decl) id)
	       (let ((id2 (mk-newsymb (format nil "_~a" (id decl))))
		     )
		 (install-definition
		  (id decl)
		  (setf (in-name-m decl) 'no-defn)
		  (progn
		    (when *eval-verbose*
		      (format t "~%~a <internal_app> ~a" (id decl) id2))
		    (setf (in-name-m decl) id2)
		    (let ((*destructive?* nil))
		      (setf (definition (in-defn-m decl))
			    `(defun ,id2 ,defn-binding-ids
			       ,@(append (when declarations
					   (list declarations))
					 (list 
					  (pvs2cl_up* defn-body
						      (pairlis defn-bindings
							       defn-binding-ids)
						      nil))))))
		    (eval (definition (in-defn-m decl)))
		    (assert id2)
		    (compile id2)
		    id2)))
	       (install-definition
		(id decl)
		(setf (in-name-d decl) 'no-defn)
		(progn (when *eval-verbose*
			 (format t "~%~a <internal_dest> ~a" (id decl) id-d))
		       (setf (in-name-d decl) id-d)
		       (let ((*destructive?* T)
			     (*output-vars* NIL))
			 (setf (definition (in-defn-d decl))
			       `(defun ,id-d ,defn-binding-ids
				  ,@(append (when declarations
					      (list declarations))
					    (list 
					     (pvs2cl_up* defn-body
							 (pairlis defn-bindings
								  defn-binding-ids)
							 nil)))))
			 (setf (output-vars (in-defn-d decl))
			       *output-vars*))
		       (eval (definition (in-defn-d decl)))
		       (assert id-d)
		       (compile id-d)
		       id-d))
	       (install-definition
		(id decl)
		(setf (in-name decl) 'no-defn)
		(progn
		  (when *eval-verbose*
		    (format t "~%~a <internal_0> ~a" (id decl) id))
		  (let ((*destructive?* NIL))
		    (setf (definition (in-defn decl))
			  `(defun ,id ()
			     ,(pvs2cl_up* defn nil nil))))
		  (eval (definition (in-defn decl)))
		  (assert id)
		  (compile id)
		  id)))))))

(defun pvs2cl-theory (theory)
  (let* ((theory (get-theory theory))
	 (*current-theory* theory)
	 (*current-context* (context theory)))
    (loop for decl in (theory theory)
	  do (cond ((type-eq-decl? decl)
		    (let ((dt (find-supertype (type-value decl))))
		      (when (adt-type-name? dt)
			(pvs2cl-constructors (constructors dt) dt))))
		    ((const-decl? decl)
		     (unless (eval-info decl)
		       (progn
			 (make-eval-info decl)
			 (catch 'no-defn
			   (or (external-lisp-function decl)
			       (pvs2cl-external-lisp-function decl)))
			 (catch 'no-defn
			   (or (lisp-function decl)
			       (pvs2cl-lisp-function decl))))))
		    (t nil)))))


(defun pvs2cl-datatype (expr)
  (let* ((dt (adt expr))
	 (constructors (constructors dt)))
    (pvs2cl-constructors constructors dt)
    (lisp-function (declaration expr))))

(defun pvs2cl-constructors (constrs datatype)
  (if (consp constrs)
      (cons (pvs2cl-constructor (car constrs) datatype)
	    (pvs2cl-constructors (cdr constrs) datatype))
      nil))

(defun mk-newconstructor (id accessor-ids &optional (counter 0))
  (let* ((const-str (format nil "~a_~a" id counter))
	 (const-str? (find-symbol const-str))
	 (mk-str? (find-symbol (format nil "MAKE-~a" const-str)))
	 (rec-str? (find-symbol (format nil "~a-P" const-str)))
	 (acc-strs? (loop for acc in accessor-ids
			 thereis
			 (find-symbol (format nil "~a_~a"
					const-str
					acc)))))
    (if (or const-str? mk-str? rec-str? acc-strs?)
	(mk-newconstructor id accessor-ids (1+ counter))
	(intern const-str))))
			 

(defun pvs2cl-constructor (constructor datatype) ;;fix multi-constructor accessors
  (cond ((enum-adt? datatype)
	 (let* ((pos (position (id constructor)
			       (constructors (adt datatype))
			       :test #'eq :key #'id))
		(decl (declaration constructor))
		(recognizer (recognizer constructor))
		(rec-decl (declaration recognizer))
		(rec-id (mk-newsymb (id recognizer)))
		(rec-defn `(defun ,rec-id (x) (eql x ,pos))))
	   (make-eval-info decl)
	   (setf (in-name decl) pos)
	   (make-eval-info rec-decl)
	   (setf (in-name rec-decl) rec-id)
	   (setf (definition (in-defn rec-decl))
		 rec-defn)
	   (eval rec-defn)
	   (compile rec-id)
	   pos))
	(t  (let* ((id (id constructor))
		   (accessors (accessors constructor))
		   (accessor-ids (mapcar #'id accessors))
		   (struct-id (mk-newconstructor id
						 accessor-ids))
		   (constructor-symbol (makesym "MAKE-~a" struct-id))
		   (defn `(defstruct (,struct-id (:constructor ,constructor-symbol 
							       ,accessor-ids))
			    ,@accessor-ids)))
	      (make-eval-info (declaration constructor))
	      (setf (definition (in-defn (declaration constructor)))
		    defn)
	      (setf (in-name (declaration constructor))
		    constructor-symbol)
	      (make-eval-info (declaration (recognizer constructor)))
	      (setf (in-name (declaration (recognizer constructor)))
		    (makesym "~a-P" struct-id))
	      (eval defn)
	      (loop for x in accessors
		    do (progn (make-eval-info (declaration x))
			      (setf (in-name (declaration x))
				    (makesym "~a-~a" struct-id (id x)))))
	      ))))
	  
(defun pvs2cl-primitive-app (expr bindings livevars)
  (let ((op (operator expr))
	(args (arguments expr)))
    (if (> (length args) 1)
	(mk-funapp (pvs2cl-primitive2 op)
		   (pvs2cl_up* args bindings livevars))
	(mk-funapp (pvs2cl-primitive op)
		    (list (pvs2cl_up* (argument expr) bindings livevars)))))
)
(defparameter *pvs2cl-primitives*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'TRUE nil '|booleans|)
	(mk-name 'FALSE nil '|booleans|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name '=> nil '|booleans|)
	(mk-name '<=> nil '|booleans|)
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
	(mk-name 'OR nil '|booleans|)
 	(mk-name 'NOT nil '|booleans|)
	(mk-name 'WHEN nil '|booleans|)
	(mk-name 'IFF nil '|booleans|)
	(mk-name '+ nil '|reals|)
	(mk-name '- nil '|reals|)
	(mk-name '* nil '|reals|)
	(mk-name '/ nil '|reals|)
	(mk-name '< nil '|reals|)
	(mk-name '<= nil '|reals|)
	(mk-name '> nil '|reals|)
	(mk-name '>= nil '|reals|)
	(mk-name '|floor| nil '|floor_ceil|)
	(mk-name '|ceiling| nil '|floor_ceil|)
	(mk-name '|rem| nil '|rem|)
	(mk-name '|div| nil '|div|)
	(mk-name '|cons| nil '|list_adt|)
	(mk-name '|car| nil '|list_adt|)
	(mk-name '|cdr| nil '|list_adt|)
	(mk-name '|cons?| nil '|list_adt|)
	(mk-name '|null| nil '|list_adt|)
	(mk-name '|null?| nil '|list_adt|)
	(mk-name '|restrict| nil '|restrict|)
	(mk-name '|length| nil '|list_props|)
	(mk-name '|member| nil '|list_props|)
	(mk-name '|nth| nil '|list_props|)
	(mk-name '|append| nil '|list_props|)
	(mk-name '|reverse| nil '|list_props|)
	))

(defun same-primitive?  (n i)
  (and (same-id n i)
       (module-instance n)
       (eq (mod-id i)
	   (id (module-instance n)))))

(defmethod pvs2cl-primitive? ((expr name-expr))
   (member expr *pvs2cl-primitives*
	       :test #'same-primitive?))

;
; This is sound as we've already gone through pvs2cl-primitve?
; by this point.  However - should do it properly at some point
; as there could be overloading of the names of primitives (assuming
; different theory)
;
(defun pvs2cl-primitive (expr)
  (cond ((eq (id expr) 'TRUE) T)
	((eq (id expr) 'FALSE) NIL)
	((eq (id expr) '|null|) NIL)
	((and (eq (id expr) '-) ;;hard to distinguish unary,binary.
	      (tupletype? (domain (find-supertype (type expr)))))
	 (intern (format nil "PVS_--")))
	(t (intern (format nil "PVS_~a" (id expr))))))

(defun pvs2cl-primitive2 (expr)
  (intern (format nil "PVS__~a" (id expr))))

;;;
;;; this clearing is now done automatically by untypecheck
;;;

(defun my-unintern (name)
  (when (and name (symbolp name)) (unintern name)))

(defun clear-dependent-theories (theory)
  (let ((module  (get-theory theory)))
    (when module
      (clear-theory theory)
      (loop for (th . insts) in (all-usings module)
	    do (clear-theory th))
      (loop for inst in (assuming-instances module)
	    do (clear-theory inst)))))

(defun clear-theory (theory)
  (let ((module (get-theory theory)))
    (when module
      (loop for dec in (theory module)
	    when (and (const-decl? dec)
		      (eval-info dec))
	    do
	    (progn (my-unintern (in-name dec))
		   (my-unintern (in-name-m dec))
		   (my-unintern (in-name-d dec))
		   (my-unintern (ex-name dec))
		   (my-unintern (ex-name-m dec))
		   (my-unintern (ex-name-d dec))
		   (setf (eval-info dec) nil))))))

(defun write-defn (defn output)
  (when defn
    (format output "~%")
    (cond (output 
	   (write defn :level nil :length nil
	       :pretty t :stream output))
	  (t (terpri)(terpri) (ppr defn)))))

(defun write-decl-defns (dec output)
  (write-defn (definition (in-defn dec)) output)
    (write-defn (definition (in-defn-m dec)) output)
      (write-defn (definition (in-defn-d dec)) output)
        (write-defn (definition (ex-defn dec)) output)
	  (write-defn (definition (ex-defn-m dec)) output)
	    (write-defn (definition (ex-defn-d dec)) output))

(defun print-lisp-defns (theory &optional file-string supersede?)
  (if file-string
      (with-open-file (output file-string :direction :output
			      :if-exists
			      (if supersede? :supersede :append)
			      :if-does-not-exist :create)
	(when supersede? (format output "(in-package 'PVS)~%"))
	(loop for dec in (theory (get-theory theory))
	      when (and (const-decl? dec)(eval-info dec))
	      do (write-decl-defns dec output)))
      (loop for dec in (theory (get-theory theory))
	      when (and (const-decl? dec)(eval-info dec))
	      do (write-decl-defns dec NIL))))

(defun pvs2cl-lisp-type (type)
  (pvs2cl-lisp-type* type))

(defmethod pvs2cl-lisp-type* ((type funtype))
  nil)

(defmethod pvs2cl-lisp-type* ((type tupletype))
  '(simple-array *))

(defmethod pvs2cl-lisp-type*  ((type recordtype))
  (if (string-type? type)
      'string
      '(simple-array *)))

(defun subrange-index (type)
  (let ((below (simple-below? type)))
    (if below (list 0 (if (number-expr? below)
			  (1- (number below))
			  '*))
	(let ((upto (simple-upto?  type)))
	  (or (and upto (if (number-expr? upto)
			    (list 0 (number upto))
			    (list 0 '*)))
	      (let ((x (simple-subrange? type)))
		(when (let ((lo (if (number-expr? (car x))
				    (number (car x))
				    '*))
			    (hi (if (number-expr? (cdr x))
				    (number (cdr x))
				    '*)))
			(list lo hi)))))))))

(defun simple-upfrom? (type)
  (let* ((bindings (make-empty-bindings (free-params (upfrom-subtype))))
	 (subst (tc-match type (upfrom-subtype) bindings)))
    (cdr (assoc '|m| subst :test #'same-id))))

(defun simple-upto? (type)
  (let* ((bindings (make-empty-bindings (free-params (upto-subtype))))
	 (subst (tc-match type (upto-subtype) bindings)))
    (cdr (assoc '|m| subst :test #'same-id))))

(defun simple-below? (type)
  (let* ((bindings (make-empty-bindings (free-params (below-subtype))))
	 (subst (tc-match type (below-subtype) bindings)))
    (cdr (assoc '|m| subst :test #'same-id))))

(defun simple-subrange? (type)
  (let* ((bindings (make-empty-bindings (free-params (subrange-subtype))))
	 (subst (tc-match type (subrange-subtype) bindings)))
    (cons (cdr (assoc '|m| subst :test #'same-id))
	  (cdr (assoc '|n| subst :test #'same-id)))))

(defun simple-above? (type)
  (let* ((bindings (make-empty-bindings (free-params (above-subtype))))
	 (subst (tc-match type (above-subtype) bindings)))
    (cdr (assoc '|m| subst :test #'same-id))))

(defun make-empty-bindings (formals)
  (mapcar #'list formals))

(defmethod pvs2cl-lisp-type* ((type subtype))
  (cond ((tc-eq type *naturalnumber*) '(integer 0))
	((tc-eq type *integer*) 'integer)
	(t (let ((sub (subrange-index type)))
	     (if sub
		 `(integer ,@sub)
		 (pvs2cl-lisp-type* (supertype type)))))))

(defmethod pvs2cl-lisp-type* ((type adt-type-name))
  (cond ((eq (id type) '|list|)
	 'list)
	(t nil)))

(defmethod pvs2cl-lisp-type* ((type T))
  (cond ((eq type *boolean*) 'boolean)
	(t nil)));;was T, but need to distinguish proper types.





