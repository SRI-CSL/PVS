(in-package 'pvs)

;(defun gqread (prompt)
;  (format t "~%~a"  prompt)
;  (force-output)
;  (let ((input (ignore-errors (read))))
;    (cond ((member input '(quit q exit (quit)(exit)(q))
;		   :test #'equal)
;	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
;	       (throw 'quit nil)
;	       (gqread prompt)))
;	  ((eq input 'abort)
;	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
;	       (throw 'abort T)
;	       (gqread prompt)))
;	  (t 
;	     input))))
;
;(defun ground-rep ()
;  (let ((result
;	 (catch 'quit 
;	   (catch 'abort
;	    (let* ((input (ignore-errors (gqread "Eval>")))
;		   (pr-input (pc-parse input 'expr))
;		   (tc-input (pc-typecheck pr-input)))
;	      (format t "~%==> ~%  ~a" (norm tc-input))
;	      T)))))
;  (if result  
;      (ground-rep)
;      (restore))))

(defstep eval (expr &optional destructive?)
  (let ((tc-expr (pc-typecheck (pc-parse expr 'expr)))
	(cl-expr (let ((*destructive?* destructive?))
		   (pvs2cl tc-expr)))
	(format-string
	 (format nil "~%~a ~%translates to ~s~%evaluates to ~% ~a"
	   expr cl-expr "~a") )
	(val (time (eval cl-expr)))
	(dummy (format t format-string
		 val)))
    (skip))
  "Ground evaluation of expression expr."
  "")

(defvar *destructive?* nil)
(defvar *output-vars* nil)
(defvar *external* NIL)

 ;need to exploit the fact that at most one makes sense
            ;if external needs actuals, then internal will crash.

(defcl eval-defn-info ()
  unary  ;;defn needed for returning functional values.
  multiary ;;fully applied form
  destructive) ;;destructive version of multiary form

(defcl eval-defn ()
  name
  definition
  output-vars) ;;These are the input vars that structure share with output.

(defcl eval-info ()
  internal  ;both are eval-defn-info 
  external)

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

(defmacro in-info (decl)
  `(internal (eval-info ,decl)))

(defmacro ex-info (decl)
  `(external (eval-info ,decl)))

(defmacro in-defn (decl)
  `(unary (in-info ,decl)))

(defmacro in-defn-m (decl)
  `(multiary (in-info ,decl)))

(defmacro in-defn-d (decl)
  `(destructive (in-info ,decl)))

(defmacro ex-defn (decl)
  `(unary (ex-info ,decl)))

(defmacro ex-defn-m (decl)
  `(multiary (ex-info ,decl)))

(defmacro ex-defn-d (decl)
  `(destructive (ex-info ,decl)))

(defmacro in-name (decl)
  `(name (in-defn ,decl)))

(defmacro in-name-m (decl)
  `(name (in-defn-m ,decl)))

(defmacro in-name-d (decl)
  `(name (in-defn-d ,decl)))

(defmacro ex-name (decl)
  `(name (ex-defn ,decl)))

(defmacro ex-name-m (decl)
  `(name (ex-defn-m ,decl)))

(defmacro ex-name-d (decl)
  `(name (ex-defn-d ,decl)))

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
    

(defcl destructive-eval-defn (eval-defn)
  side-effects) ;;alist of updated variables/live vars when updated.


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
;    (if (ground? expr)
	(cl2pvs (eval (pvs2cl expr)) (type expr) context)))
;	expr

;;initialize context and translate to common-lisp with null bindings
;;and live-variables, i.e., still active in yet-to-be-evaluated expressions.  
(defun pvs2cl (expr &optional context)
  (let ((*current-context*
	 (if context context *current-context*))
	(*generate-tccs* 'NONE))
    (catch 'no-defn (pvs2cl_up* expr nil nil))))

;;Should be a macro.  applied function name to argument list.
(defun mk-funapp (fun args)
  `(,fun ,@args))

;;Function application for non-name functions.
;;If function is an array, then svref, else funcall
(defmacro pvs-funcall (fun &rest args)
  `(let ((funval ,fun))
     (if (arrayp funval)
	 (svref funval ,@args)
	 (funcall funval ,@args))))

;;mk-funcall defined to be pvs-funcall for backward compatibility.
(defun mk-funcall (fun args)
  `(pvs-funcall ,fun ,@args))

(defun check-output-vars (output-vars alist)
  (if (consp output-vars)
      (let* ((var (caar output-vars))
	     (lvars (cdar output-vars))
	     (vterm (cdr (assoc var alist :test #'same-declaration)))
	     (lterms (mapcar #'(lambda (x) (cdr (assoc x alist :test #'same-declaration)))
		       lvars))
	     (vterm-updateables (updateable-vars vterm))
	     (lterm-updateables (updateable-vars lterms)))
	(cond ((and (null (intersection vterm-updateables
					lterm-updateables
					:test #'same-declaration))
		    (check-output-vars (cdr output-vars) alist))
	       (push-output-vars vterm-updateables lterm-updateables)
			;	 *output-vars*
	       T)
	      (t NIL)))
      T))

;Putting (THE TYPE ..) does not add speed and slows translation/compilation.
(defmethod pvs2cl_up* :around ((expr expr) bindings livevars)
;	   (call-next-method)) 
	   (let ((lisp-type (pvs2cl-lisp-type (type expr))))
	     (if lisp-type
		 `(the ,lisp-type
		    ,(call-next-method))
		 (call-next-method))))

(defun pvs2cl-operator2 (op actuals arguments bindings)
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
					;	     (term-actuals
					;	      (if actuals 
					;		  (loop for act in actuals
					;			when (null (type-value act))
					;
					;			collect (expr act))
					;		  (loop for fm in module-formals
					;			collect (cdr (assoc fm bindings))))
					;		      )
	       (eval-defn (if actuals
			      (ex-defn-d (declaration op))
			      (in-defn-d (declaration op))))
	       (output-vars (output-vars eval-defn))
	       (check (check-output-vars output-vars
					 (pairlis
					  formals
					  (append actuals
						  arguments)))))
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
    (mk-name-expr (id decl) nil nil res 'constant)))

(defmethod make-constant-from-decl ((decl formal-const-decl))
  (let* ((type (type decl))
	 (res (make-resolution decl
	       (mk-modname (id (module decl)) nil)
	       type)))
    (mk-name-expr (id decl) nil nil res 'constant)))
    

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
	 (args-free-formals (free-formal-vars arguments))
	 (args-livevars (append args-free-formals livevars)))
    (if internal-actuals
	(mk-funapp (pvs2cl-operator2 op actuals arguments bindings)
		   ;;(pvs2cl-resolution2 op)
	     (append (pvs2cl_up* internal-actuals
				     bindings
				     args-livevars)
		     (pvs2cl_up* arguments bindings
				 (append (mapcan #'updateable-free-formal-vars
					   actuals);;only updateable
					 ;;parameters of already evaluated
					 ;;exprs needed.
					 livevars))))
	(mk-funapp (pvs2cl-operator2 op NIL arguments bindings)  ;;(pvs2cl-resolution2 op)
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
				       (append (free-formal-vars (argument expr))
					       livevars)))
			(clargs (list (pvs2cl_up* (argument expr)
						  bindings
						  (append (free-formal-vars op)
							  livevars)))))
		    (if (and (symbolp clop)
			     (not (member clop bindings
					  :key #'cdr)))
			(mk-funapp clop clargs)
			(mk-funcall clop clargs)))))))
      (mk-funcall (pvs2cl_up* op bindings
			      (append (free-formal-vars (argument expr))
				      livevars))
		  (list (pvs2cl_up* (argument expr) bindings
				    (append (free-formal-vars op)
					     livevars)))))))

(defun pvs2cl-let-pairs (let-bindings args bindings livevars)
  (if (= (length let-bindings) (length args))
      (let ((clargs (pvs2cl_up* args bindings livevars)))
	(loop for bnd in let-bindings
	    as clarg in clargs
	    collect (list bnd clarg)))
      (let ((arg (pvs2cl_up* (car args) bindings livevars)))
	(loop for bnd in let-bindings
	      as i from 1
	      collect (list bnd `(project ,i ,arg))))))

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
					 args
					 let-bindings)
  (cond (*destructive?*
	 (multiple-value-bind
	     (cl-expression output-vars)
	     (let* ((bvar-expr-pairs (pairlis let-bindings args))
		    ;(var-expr-pairs (pairlis let-variables args))
		    (*output-vars* nil)
		    (cl-expression (pvs2cl_up* expression
					       bindings
					       livevars))
		    (check (check-output-vars
			    *output-vars*
			    bvar-expr-pairs))
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
						    
(defmethod pvs2cl_up* ((expr let-expr) bindings livevars)
  (let ((let-bindings (bindings (operator expr)))
	(args (arguments expr))
	(expression (expression (operator expr))))
    (let* ((let-variables
	    (pvs2cl-make-bindings  let-bindings bindings))
	   (let-binding-pairs (pairlis let-bindings let-variables))
;	   (bvar-expr-pairs (pairlis let-bindings args))
;	   (var-expr-pairs (pairlis let-variables args))
	   (let-pairs (pvs2cl-let-pairs
		       let-variables
		       args bindings ;;operator free-formal-vars
		                     ;;are always updateable.
		       (append (free-formal-vars (operator expr))
			       livevars)))
	   (declaration (pvs2cl-declare-vars let-variables args))
	   (cl-expression (pvs2cl-let-expression
			   expression (append let-binding-pairs bindings)
			    livevars
			   args
			   let-bindings)))
      (if declaration
	  `(let ,let-pairs ,declaration ,cl-expression)
	  `(let ,let-pairs ,cl-expression)))))
;;let* not needed, nested anyway.

(defmethod pvs2cl_up* ((expr if-expr) bindings livevars)
  (cond ((branch? expr)
	 (let ((condition (condition expr))
	       (then-part (then-part expr))
	       (else-part (else-part expr)))
	 `(if ,(pvs2cl_up* condition bindings
			   (append (free-formal-vars then-part)
				   (append (free-formal-vars else-part)
					   livevars)))
	      ,(pvs2cl_up* (then-part expr) bindings livevars)
	      ,(pvs2cl_up* (else-part expr) bindings livevars))))
	(t (call-next-method))))

(defmethod pvs2cl_up* ((expr cases-expr) bindings livevars)
  (pvs2cl_up* (mk-translate-cases-to-if expr) bindings livevars))

(defmethod pvs2cl_up* ((expr lambda-expr) bindings livevars)
  (pvs2cl-lambda (bindings expr) (expression expr) bindings
		 (free-formal-vars expr))) ;;unsafe to update any freevar

(defmethod pvs2cl_up* ((expr forall-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (styp1 (find-supertype typ1))
	       (sub (sub-range? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (mk-forall-expr
				  bind-rest
				body)
			      body)))
	  (cond ((enum-adt? styp1)
		 `(every ,(pvs2cl-lambda (list bind1)
					expr-rest
					bindings livevars)
			(list ,@(pvs2cl_up* (constructors styp1)
					    bindings livevars))))
		(sub
		  (let ((i (gentemp)))
		    `(loop for ,i
			   from ,(car sub)
			   to ,(cdr sub)
			   always
			   (pvs-funcall ,(pvs2cl-lambda (list bind1)
							expr-rest
							bindings livevars)
					i))))
		(t (throw 'no-defn
			  "Cannot handle non-scalar/subrange quantifiers."))))
	(pvs2cl_up* body bindings livevars))))

(defmethod pvs2cl_up* ((expr exists-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (styp1 (find-supertype typ1))
	       (sub (sub-range? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (mk-forall-expr
				  bind-rest
				body)
			      body)))
	  (cond ((enum-adt? styp1)
		 `(some ,(pvs2cl-lambda (list bind1)
					expr-rest
					bindings livevars)
			(list ,@(pvs2cl_up* (constructors styp1)
					    bindings livevars))))
		(sub
		  (let ((i (gentemp)))
		    `(loop for ,i
			   from ,(car sub)
			   to ,(cdr sub)
			   thereis
			   (pvs-funcall ,(pvs2cl-lambda (list bind1)
							expr-rest
							bindings livevars)
					i))))
		(t (throw 'no-defn
			  "Cannot handle non-scalar/subrange quantifiers."))))
	(pvs2cl_up* body bindings livevars))))
			

(defmethod pvs2cl_up* ((expr name-expr) bindings livevars)
;;  (when (memq (id expr) '(< N))(break))
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
			(append (free-formal-vars (cdr expr)) livevars))
	    (pvs2cl_up* (cdr expr) bindings  ;;need car's freevars
			(append (updateable-free-formal-vars (car expr)) ;;f(A, A WITH ..)
				livevars)))
      nil))

(defmethod pvs2cl_up* ((expr number-expr) bindings livevars)
  (number expr))

(defmethod pvs2cl_up* ((expr tuple-expr) bindings livevars)
  `(vector ,@(pvs2cl_up* (exprs expr) bindings livevars)))

(defmethod pvs2cl_up* ((expr record-expr) bindings livevars)
  `(vector ,@(pvs2cl_up* (mapcar #'expression
			   (sort-assignments (assignments expr)))
			 bindings livevars)))

(defmethod pvs2cl_up*((expr projection-expr) bindings livevars)
  `(project ,(index expr) ,(pvs2cl_up* (argument expr) bindings livevars)))


(defun sort-assignments (assigns)                
  (let ((restructure   (copy-list assigns)))     
    (sort restructure                            
          #'string-lessp                         
          :key #'(lambda (x) (id (caar (arguments x)))))))

(defun sorted-fields (typ)                
  (let ((restructure   (copy-list (fields typ))))     
    (sort restructure                            
          #'string-lessp                         
          :key #'(lambda (x) (id x)))))

(defun get-field-num (id typ)
  (position id (sorted-fields typ)
            :test #'(lambda (x y) (eq x (id y)))))

(defmethod pvs2cl_up*  ((expr field-application) bindings livevars)
  (let ((index (get-field-num (id expr)(type (argument expr)))))
    `(project ,(1+ index) ,(pvs2cl_up* (argument expr) bindings livevars))))

;working on dest. updates.
;(defmethod pvs2cl_up* ((expr update-expr) bindings)
;  (pvs2cl-update (expression expr)
;		 (assignments expr)
;		 bindings))

(defmethod no-livevars? ((expr update-expr) livevars)
  (no-livevars? (expression expr) livevars))

(defmethod no-livevars? ((expr T) livevars)
  (let ((updateable-vars
	 (loop for var in (free-formal-vars expr)
	       when (contains-updateable? (type var))
	       collect var)))
    (and (null (intersection  updateable-vars
			 livevars
			 :test #'same-declaration))
	 (cons updateable-vars livevars))))

(defvar *fun-lhs* nil)

(defun array-bound (type)
  (and (funtype? type)
       (let ((x (below? (domain type))))
	 (or x
	     (let ((y (upto? (domain type))))
	       (and y (1+ y) ))))))

(defun pvs2cl-update (expr assigns bindings livevars)
 (let* ((expr-type (type expr))
	(cl-expr (pvs2cl_up* expr bindings livevars))
	;;the livevars are irrelevant and can be set to nil since
	;;we have already check expr in pvs2cl_up*(update-expr).
	(expr-stype (find-supertype expr-type))
	(bound (array-bound expr-stype))
	(cl-expr-array (if bound
			   `(the simple-array
			      (mk-fun-array ,cl-expr
					    ,(pvs2cl_up* bound bindings livevars)))
			   cl-expr))
	(var (gentemp))
	(expr-livevars (updateable-free-formal-vars expr))
	(cl-assign-exprs
	 (pvs2cl-assign-exprs assigns bindings
			      (append expr-livevars livevars)))
	(assign-var-bindings
	 (loop for x in cl-assign-exprs collect
	       (list (gentemp)
		     x)))
	(assign-expr-livevars
	 (loop for x in assigns append
	       (updateable-free-formal-vars (expression x)))) 
	(*fun-lhs* nil)
	(cl-assigns (pvs2cl-assigns expr-stype var assigns
				    assign-var-bindings 
				    bindings
				    (append assign-expr-livevars
					    (append expr-livevars
						    livevars))))
	(cl-mk-array (loop for (x . y) in *fun-lhs*
			   nconc (list x `(mk-fun-array ,x ,y))))
	(cl-setf (cons 'setf (append cl-mk-array
				     (loop for x in cl-assigns nconc x)))))
   `(let ,assign-var-bindings (let ((,var ,cl-expr-array)) ,cl-setf ,var))))

(defun pvs2cl-assigns (type exprvar assigns  assign-var-bindings
			    bindings
			    livevars)
  (when (consp  assigns)
      (cons (pvs2cl-assign type exprvar (car assigns)
			   (car assign-var-bindings)
			   bindings
			   (append (free-formal-vars (cdr assigns))
				   livevars))
	    (pvs2cl-assigns type exprvar (cdr assigns)
			    (cdr assign-var-bindings)
			    bindings livevars)))) ;;check livevars here

(defun pvs2cl-assign-exprs (assigns bindings livevars)
  (if (consp assigns)
      (cons
       (pvs2cl_up* (expression (car assigns)) bindings
		   (append (free-formal-vars (arguments (car assigns)))
			   (append (free-formal-vars (cdr assigns)) livevars)))
       (pvs2cl-assign-exprs (cdr assigns) bindings
			    (append (updateable-free-formal-vars
				     (expression (car assigns)))
				    livevars)))
      nil))

(defun pvs2cl-assign (type exprvar assign assign-var-binding bindings livevars)
  (pvs2cl-assign-args type exprvar (arguments assign)
		      (car assign-var-binding) bindings livevars)) 

(defun pvs2cl-assign-args (type exprvar args uexpr bindings livevars)
  (list (pvs2cl-type-assign-args type
				 exprvar
				 args
				 bindings livevars)
	uexpr ;(pvs2cl_up* uexpr bindings livevars)
	))

(defun pvs2cl-type-assign-args (type expr args bindings livevars)
  (if (consp args)
      (pvs2cl-type-assign-args* (find-supertype type)
			       expr args bindings livevars)
      expr))

(defmethod pvs2cl-type-assign-args* ((type recordtype) expr args
				     bindings livevars) 
  (let* ((id (id (caar args)))
	 (field-num (get-field-num  id type))
	 (new-expr `(svref ,expr ,field-num))
	 (field-type (type (find id (fields type) :key #'id) )))
    (pvs2cl-type-assign-args field-type new-expr (cdr args)
			     bindings livevars)))

(defmethod pvs2cl-type-assign-args* ((type tupletype) expr args
				     bindings livevars) 
  (let* ((num (caar args))
	 (new-expr `(svref ,expr ,(1- num)))
	 (tupsel-type (nth (1- num)(types type))))
    (pvs2cl-type-assign-args tupsel-type new-expr (cdr args) bindings
			     livevars)))

(defun mk-fun-array (expr size) 
  (if (or (simple-vector-p expr)(null size))
      expr
      (let* ((arr (make-array size :initial-element 0)))
	(loop for i from 0 to (1- size) do
	      (setf (svref arr i)(funcall expr i)))
	arr)))


(defmethod pvs2cl-type-assign-args* ((type funtype) expr args
				     bindings livevars)
  (let* ((carargs (car args))
	 (args1 (if (> (length carargs) 1)
		    (pvs2cl_up* (mk-tuple-expr carargs) bindings livevars)
		    (pvs2cl_up* (car carargs) bindings livevars)))
	 (hi (or (below? (domain type))(upto? (domain type))))
	 (hi-cl (when hi (pvs2cl_up* hi bindings livevars)))
	 (new-expr `(svref ,expr  ,args1))
	 (rangetype (range type)))
    (when (and (not (symbolp expr)) hi)
      (pushnew (cons expr hi-cl) *fun-lhs* :test #'equal :key #'car))
    (pvs2cl-type-assign-args rangetype new-expr (cdr args) bindings livevars)))

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
		   

(defmethod pvs2cl_up* ((expr update-expr) bindings livevars)
  (if (and *destructive?*
	   (updateable? (type (expression expr))))
      (let* ((expression (expression expr))
	     (assignments (assignments expr))
	     (assign-arg-livevars (loop for asgn in assignments
					append (free-formal-vars
						(arguments asgn))))
	     (assign-expr-livevars (loop for asgn in assignments
					append (updateable-free-formal-vars
						(expression asgn))))
	     (assign-livevars (free-formal-vars assignments))
	     (updatable-livevars
	     (no-livevars? expression
			   (append assign-expr-livevars
				   (append assign-arg-livevars livevars)))));;very unrefined: uses all
	;;freevars of eventually updated expression.
	(cond (updatable-livevars
	       (push-output-vars (car updatable-livevars)
				 (cdr updatable-livevars))
;				 *output-vars*
	       (pvs2cl-update expression
			      assignments
			      bindings livevars))
	      (t (pvs2cl_up* (translate-update-to-if! expr)
			     bindings livevars))))
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
	   (let ((fun (if (expr-actuals (module-instance expr))
			  (external-lisp-function (declaration expr))
			  (lisp-function (declaration expr)))))
	(if (datatype-constant? expr)
	    (if (scalar-constant? expr)
		(mk-funapp fun nil)
		`(function ,fun)) ;;actuals irrelevant for datatypes
	    (mk-funapp fun (pvs2cl_up* (expr-actuals (module-instance expr))
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

;was
;(or (constructor? expr)
;	  (accessor? expr)
;	  (recognizer? expr)))

(defun pvs2cl-resolution2 (expr)
  (pvs2cl-resolution expr)
  (if (or (expr-actuals (module-instance expr))
	  (eq (module (declaration expr)) *external*)) 
      (external-lisp-function2 (declaration expr))
      (lisp-function2 (declaration expr))))

(defun lisp-function (decl)
  (let ((name (or (in-name decl)(in-name-m decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	name)))

(defun lisp-function2 (decl)
  (let ((name (or (in-name-m decl)(in-name decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	name)))

(defun external-lisp-function (decl)
  (let ((name (or (ex-name decl)(ex-name-m decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	name)))

(defun external-lisp-function2 (decl)
  (let ((name (or (ex-name-m decl)(ex-name decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	(if *destructive?*
	    (ex-name-d decl)
	    name))))

(defun lisp-function-d (decl)
  (let ((name (or (in-name-d decl)(lisp-function2 decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	name)))

(defun external-lisp-function-d (decl)
  (let ((name (or (ex-name-d decl)(external-lisp-function2 decl))))
    (if (eq name 'no-defn)
	(throw 'no-defn nil)
	name)))

(defun pvs2cl-resolution (expr)
  (let ((decl (declaration expr)))
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
  (let* (;(decl (declaration expr))
	 (defax (def-axiom decl))
	 (*external* (module decl))
	 (*current-theory* (module decl)))
    (if (null defax)
	(throw 'no-defn (format nil "~%No definition for ~a" (id decl)))
	(let
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
		;;    (when (> (length defn-bindings) 1) ;no longer need this restriction
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
			  (unless id2 (break "id2x"))
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
			(unless id-d (break "id-dx"))
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
		   (unless id (break "id"))
		   (compile id)
		   id))))))))

(defun pvs2cl-lisp-function (decl)
  (let* (;(decl (declaration expr))
	 (defax (def-axiom decl))
	 (*external* nil))
    (if (null defax)
	(throw 'no-defn (format nil "~%No definition for ~a" (id decl)))
	(let* 
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
	  ;;    (when (> (length defn-bindings) 1))
	  (let ((id2 (mk-newsymb (format nil "_~a" (id decl))))
		)
	    (install-definition
	     (id decl)
	     (setf (in-name-m decl) 'no-defn)
	     (progn
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
	       (unless id2 (break "id2i"))
	       (compile id2)
	       id2)))
	  (install-definition
	   (id decl)
	   (setf (in-name-d decl) 'no-defn)
	   (progn (setf (in-name-d decl) id-d)
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
		  (unless id-d (break "id-di"))
		  (compile id-d)
		  id-d))
	  (install-definition
	   (id decl)
	   (setf (in-name decl) 'no-defn)
	   (progn
	     (let ((*destructive?* NIL))
	       (setf (definition (in-defn decl))
		     `(defun ,id ()
			,(pvs2cl_up* defn nil nil))))
	     (eval (definition (in-defn decl)))
	     (unless id (break "id"))
	     (compile id)
	     id))))))

;;This should be in the patch.  *match-cache* is initialized
;;if needed since it is called in checking sub-type?.  
(defun match (expr instance bind-alist subst)
  (let* ((*match-cache* (or *match-cache* ;;for initialization
			                   ;;not shadowing
			    (make-hash-table :test #'eq)))
	  (hashed-table-expr
	  (unless *no-bound-variables-in-match* ;;NSH(10.19.95)
	      (gethash expr *match-cache*)))  ;;should not cache or lookup
	 (hashed-value (when hashed-table-expr
			 (gethash instance hashed-table-expr)))
	 (hashed-result (when hashed-value (car hashed-value)))
	 (hashed-modsubst (when hashed-value (cdr hashed-value)))
	 (*dont-cache-match?* nil)
	 (*remaining-actuals-matches* nil)) ;;(break "match1")
    (cond ((and hashed-value
		(eq hashed-result 'FAIL))
	   'FAIL)
	  (hashed-value
	   (setq *modsubst* hashed-modsubst)
	   (if subst
	       (merge-subst subst hashed-result)
	       hashed-result))
	  (t (let* ((frees (freevars expr)) ;just to set no-freevars?
		    (res (match* expr instance bind-alist subst))
		    (result
		     (if (or (eq res 'fail)
			     (not (subsetp frees res :test
					   #'(lambda (x y) ;;nsh(7.12.95)
					       ;;was tc-eq.
					       (same-declaration x (car y))))))
			 'fail
			 (match-remaining-actuals
			  *remaining-actuals-matches* res)))) ;;(break "match2")
	       (when (and (null subst)
			  (null *dont-cache-match?*)
			  (null *no-bound-variables-in-match*))
		 (if hashed-table-expr
		   (setf (gethash instance hashed-table-expr)
			 (cons result *modsubst*))
		   (setf (gethash expr *match-cache*)
			 (let ((hash (make-hash-table :test #'eq)))
			   (setf (gethash instance hash)
				 (cons result *modsubst*))
			   hash))))
	       result)))))

(defun pvs2cl-theory (theory)
  (let* ((theory (get-theory theory))
	 (*current-context* (context theory)))
    (loop for decl in (theory theory)
	  when (const-decl? decl)
	  do (progn
	       (make-eval-info decl)
	       (catch 'no-defn
		 (or (external-lisp-function decl)
		     (pvs2cl-external-lisp-function decl)))
	       (catch 'no-defn
		 (or (lisp-function decl)
		     (pvs2cl-lisp-function decl)))))))


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
  (let* ((id (id constructor))
	 (accessors (accessors constructor))
	 (accessor-ids (mapcar #'id accessors))
	 (struct-id (mk-newconstructor id
				       accessor-ids))
	 (defn `(defstruct ,struct-id ,@accessor-ids)))
    (setf (struct-name (adt-type-name datatype)) struct-id)
    (make-eval-info (declaration constructor))
    (setf (definition (in-defn (declaration constructor)))
	  defn)
    (setf (in-name (declaration constructor))
	  (makesym "MAKE-~a" struct-id))
    (make-eval-info (declaration (recognizer constructor)))
    (setf (in-name (declaration (recognizer constructor)))
	  (makesym "~a-P" struct-id))
    (eval defn)
    (loop for x in accessors
	  do (progn (make-eval-info (declaration x))
		    (setf (in-name (declaration x))
			  (makesym "~a-~a" struct-id (id x)))))
    ))
	  
(defun pvs2cl-primitive-app (expr bindings livevars)
  (let ((op (operator expr))
	(args (arguments expr)))
    (if (> (length args) 1)
	(mk-funapp (pvs2cl-primitive2 op)
		   (pvs2cl_up* args bindings livevars))
	(mk-funapp (pvs2cl-primitive op)
		    (list (pvs2cl_up* (argument expr) bindings livevars))))))

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
	))

(defun same-primitive?  (n i)
  (and (same-id n i)
       (module-instance n)
       (eq (mod-id i)
	   (id (module-instance n)))))



(defmethod pvs2cl-primitive? ((expr name-expr))
   (member expr *pvs2cl-primitives*
	       :test #'same-primitive?))

(defun pvs2cl-primitive (expr)
  (cond ((eq (id expr) 'TRUE) T)
	((eq (id expr) 'FALSE) NIL)
	((eq (id expr) '|null|) NIL)
	((and (eq (id expr) '-) ;;hard to distinguish unary,binary.
	      (tupletype? (domain (type expr))))
	 (intern (format nil "PVS_--")))
	(t (intern (format nil "PVS_~a" (id expr))))))

(defun pvs2cl-primitive2 (expr)
  (intern (format nil "PVS__~a" (id expr))))

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
;	(let ((lf (lisp-function dec))
;	      (elf (external-lisp-function dec))
;	      (lf2 (lisp-function2 dec))
;	      (elf2 (external-lisp-function dec)))
;	  (setf (lisp-function dec) nil)
;	  (setf (lisp-definition dec) nil)
;	  (setf (external-lisp-function dec) nil)
;	  (setf (external-lisp-definition dec) nil)
;	  (setf (lisp-function2 dec) nil)
;	  (setf (lisp-definition2 dec) nil)
;	  (setf (external-lisp-definition2 dec) nil)
;	  (setf (external-lisp-function2 dec) nil)
;	  (when lf (unintern lf))
;	  (when elf (unintern elf))
;	  (when lf2 (unintern lf2))
;	  (when elf2 (unintern elf2))
;	  )))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pvs2cl-lisp-type (type)
  (pvs2cl-lisp-type* type))

(defmethod psv2cl-lisp-type* ((type funtype))
  `(function ,(pvs2cl-lisp-type* (domain type))
	    ,(pvs2cl-lisp-type* (range type))))

(defmethod pvs2cl-lisp-type* ((type tupletype))
  '(simple-array *))

(defmethod pvs2cl-lisp-type*  ((type recordtype))
  '(simple-array *))

(defun subrange-index (type)
  (let ((below (below? type)))
    (if below (list 0 (if (number-expr? below)
			  (1- (number below))
			  '*))
	(let ((upto (upto?  type)))
	  (or (and upto (if (number-expr? upto)
			    (list 0 (number upto))
			    (list 0 '*)))
	      (let ((x (subrange? type)))
		(when (let ((lo (if (number-expr? (car x))
				    (number (car x))
				    '*))
			    (hi (if (number-expr? (cdr x))
				    (number (cdr x))
				    '*)))
			(list lo hi)))))))))
		    

(defmethod pvs2cl-lisp-type* ((type subtype))
  (cond ((tc-eq type *naturalnumber*) '(integer 0 *))
	((tc-eq type *integer*) 'integer)
	(t (let ((sub (subrange-index type)))
	     (if sub
		 `(integer ,@sub)
		 (pvs2cl-lisp-type* (supertype type)))))))

(defcl adt-type-name (type-name)
  recognizer-names
  struct-name)


(defmethod pvs2cl-lisp-type* ((type adt-type-name))
  (cond ((eq (id type) '|list|)
	 'list)
	(t (struct-name type))))

(defmethod pvs2cl-lisp-type* ((type T))
  (cond ((eq type *boolean*) 'boolean)
	(t nil)));;was T, but need to distinguish proper types.




