;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;;The ground-eval step evaluates a ground PVS expression returning its value,
;;but has no effect on the proof
;;Modified such that printf is used insted of dummy format (Feb 20 2015) [CM]
;;[CM] This strategy have been replaced by (eval) in extrategies
;;(defstep ground-eval (expr &optional destructive?)
;;  (let ((tc-expr (pc-typecheck (pc-parse expr 'expr)))
;;	(cl-expr (let ((*destructive?* destructive?))
;;		   (pvs2cl tc-expr)))
;;	(val (time (handler-case (eval cl-expr)
;;		     (pvseval-error (condition) (format t "~%~a" condition))))))
;;    (printf "~%~a ~%translates to ~s~%evaluates to ~%~a" expr cl-expr val))
;;  "Ground evaluation of expression EXPR."
;;  "Ground evaluating ~a")

;;; Instead of (throw 'undefined), use (error 'pvseval-error :expr expr :fmt-str str)
;;; Note that the error happens at evaluation time, so the use is something like
;;;   (let ((cl-form (pvs2cl expr)))
;;;     (handler-case (eval cl-form)
;;;       (pvseval-error (condition) (format t "~%~a" condition))))

(define-condition pvseval-error (groundeval-error)
  ((expr :accessor expr :initarg :expr)
   (fmt-str :accessor fmt-str :initarg :fmt-str))
  (:report
   (lambda (condition stream)
     (format stream
	 (or (fmt-str condition)
	     "Hit uninterpreted term ~a during evaluation (pvseval-error)")
       (expr condition)))))

(defun uninterpreted (expr fmt-str)
  (error 'pvseval-error :expr expr :fmt-str fmt-str))

;;Added function uninterpreted-fun and modified function undefined (Feb 20 2015) [CM]
(defun uninterpreted-fun (expr &optional fmt-str)
  #'(lambda (&rest x)
      (declare (ignore x))
      (uninterpreted expr fmt-str)))

;;These variables are special
(defvar *destructive?* nil)  ;;tracks if the translation is in the destructive mode
(defvar *output-vars* nil) ;;
(defvar *external* nil)

(defvar *pvs2cl-decl* nil
  "Tracks the current declaration being compiled, for help in debugging undefined, etc.")

;;lisp-id generates a Lisp identifier for a PVS identifier that
;;doesn't clash with existing Lisp constants and globals.

(defvar *lisp-id-hash* (make-hash-table :test #'eq))
(defun lisp-id (id) ;;id must be a symbol
  (if (special-variable-p id) 
      (let ((lid (gethash id *lisp-id-hash*)))
	(or lid
	    (let ((new-lid (intern (symbol-name (gensym (string id))) :pvs)))
	      (setf (gethash id *lisp-id-hash*) new-lid)
	      new-lid)))
      id))


;need to exploit the fact that at most one makes sense
;if external needs actuals, then internal will crash.

(defun make-eval-info (decl)
  (cond ((eval-info decl)
	 (unless (internal (eval-info decl))
	   (setf (internal (eval-info decl))
		 (make-instance 'eval-defn-info
				'unary (make-instance 'eval-defn)
				'multiary (make-instance 'eval-defn)
				'destructive (make-instance 'eval-defn))))
	 (unless (external (eval-info decl))
	   (setf (external (eval-info decl))
		 (make-instance 'eval-defn-info
				'unary (make-instance 'eval-defn)
				'multiary (make-instance 'eval-defn)
				'destructive (make-instance 'eval-defn))))
	 (eval-info decl))
	(t (setf (eval-info decl)
		 (make-instance 'eval-info
				'internal (make-instance 'eval-defn-info
							 'unary (make-instance 'eval-defn)
							 'multiary (make-instance 'eval-defn)
							 'destructive (make-instance 'eval-defn))
				'external (make-instance 'eval-defn-info
							 'unary (make-instance 'eval-defn)
							 'multiary (make-instance 'eval-defn)
							 'destructive (make-instance 'eval-defn))))
	   (eval-info decl))))

;; Return module when expr is a PVSio global variable, i.e., of type stdprog.Global or stdglobal.Global
(defun pvsio-global-variable (expr)
  (let* ((decl       (declaration expr))
	 (decltype   (when decl (declared-type decl))))
    (and (type-name? decltype)
	 (eq '|Global| (id decltype))
	 (let* ((resolution (when decltype (car (resolutions decltype))))
		(mod        (when resolution (module-instance resolution))))
	   (when (and mod (member (id mod) '(|stdprog| |stdglobal|)))
	     mod)))))

(defun undefined (expr &optional message)
  "Creates and compiles a new function, returning the name.  If the expr is
a const-decl, of type 'Global', creates an attachment instance (part of
PVSio).  Otherwise creates an 'undefined' function, which invokes an error
if called."
  (let* ((th       (string (if (declaration? expr)
			       (id (module expr))
			     (id (current-theory)))))
	 (nm       (when (const-decl? expr) (string (id expr))))
	 (nargs    (when nm (arity expr)))
	 (mod      (and nm (= nargs 0) (pvsio-global-variable expr))))
    (if mod
	(let* ((fname (gentemp "global"))
	       (act   (actuals mod))
	       (doc (format nil "Global mutable variable ~a of type ~a" 
			    nm (car act)))
	       (arg (if (eq '|stdprog| (id mod))
			`(pvsio_new_gvar ,nm)
		      `(pvsio_ref_gvar ,(pvs2cl (expr (cadr act))) ,nm)))
	       (fbody `(progn 
			 (defparameter ,fname ,arg)
			 (defattach-th-nm ,th ,(id expr) () ,doc ,fname))))
	  (eval fbody)
	  (makesym "pvsio_~a_~a_~a" th nm nargs))
      (let* ((fname (gentemp "undefined"))
	     (msg-fmt
	      (or message
		  "Hit uninterpreted term ~a during evaluation (undefined)"))
	     (fbody (if (and nargs (> nargs 0))
			`(defun ,fname (&rest x)
			   (declare (ignore x))
			   (uninterpreted-fun ,expr ,msg-fmt))
		      `(defun ,fname (&rest x)
			 (declare (ignore x))
			 (uninterpreted ,expr ,msg-fmt)))))
	(eval fbody)
	(compile fname)
	fname))))

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
	(*generate-tccs* 'none))
    (pvs2cl_up* expr nil nil)))

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
	       t)
	      (t nil)))
      t))

;;wraps (the type ..) around the translated lisp when the type is known
(defmethod pvs2cl_up* :around ((expr expr) bindings livevars)
  (declare (ignore livevars bindings))
  (let ((lisp-type (unless (rational-expr? expr)
		     (pvs2cl-lisp-type (type expr)))))
    (if lisp-type
	(if (eq lisp-type 'string)
	    (let ((exprval (gentemp "stringval")))
	    `(the string
		  (let ((,exprval ,(call-next-method)))
		    (if (stringp ,exprval) ,exprval
		      (make-pvslisp-string (svref ,exprval 0)(svref ,exprval 1))))))
	  `(the ,lisp-type
		,(call-next-method)))
	(call-next-method))))

;;String literals are translated directly to strings. 
(defmethod pvs2cl_up* ((expr string-expr) bindings livevars)
  ;;  (call-next-method)
  (declare (ignore bindings livevars))
;  (break "string-expr")
  (let ((str (string-value expr)))
    str))
;;   `(pvs2cl_record (length ,str) ,str)))
;;;    (concatenate 'string "\"" (string-value expr) "\"")


  
(defun pvs2cl-operator2 (op actuals arguments def-formals livevars bindings)
  (declare (ignore bindings))
  (pvs2cl-resolution2 op)
  (let ((decl (declaration op)))
    (if *destructive?*
	(let* (;;(decl (declaration op))
	       (module-formals
		(loop for x in (formals (module decl))
			when (formal-const-decl? x)
			collect x))
	       ;; (defn (args2 (car (last (def-axiom decl)))))
	       ;; (def-formals (when (lambda-expr? defn)
	       ;;                (bindings defn)))
	       (formals (if (or actuals *external*)
			    (append module-formals
				    def-formals)
			    def-formals))
	       (eval-defn (if (or actuals *external*)
			      (ex-defn-d (declaration op))
			      (in-defn-d (declaration op))))
	       (output-vars (output-vars eval-defn))
	       (fargs (if (length= formals arguments)
			  arguments
			  (append (or actuals module-formals)
				  arguments)))
	       (check (check-output-vars
		       output-vars 
		       (pairlis formals fargs)
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
(defun mk-fun2-application (op arguments arg-formals bindings livevars)
  (let* ((decl (declaration op))
	 (actuals (expr-actuals  (module-instance op)))
	 (internal-actuals
	  (or actuals
	      (and (eq (module decl) *external*)
		   (loop for x in (formals (module decl))
			 when (formal-const-decl? x)
			 collect (make-constant-from-decl x)))))
	 (args-free-formals (updateable-vars arguments))
	 (args-livevars (append args-free-formals livevars))
	 ;; (args (if (= (length (bindings (car (last (butlast (def-axiom decl))))))
	 ;; 		(length arguments))
	 ;; 	     arguments
	 ;; 	     (list (make!-tuple-expr* arguments))))
	 )
    (if internal-actuals
	(mk-funapp
	 (pvs2cl-operator2 op actuals arguments arg-formals livevars bindings)
	 (append (pvs2cl_up* internal-actuals
			     bindings
			     args-livevars)
		 (pvs2cl_up* arguments bindings
			     (append (mapcan #'updateable-free-formal-vars
				       actuals);;only updateable
				     ;;parameters of already evaluated
				     ;;exprs needed.
				     livevars))))
	(mk-funapp (pvs2cl-operator2 op nil arguments arg-formals
				     livevars bindings)
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
;;Modified treatment of PVSio primitives (Feb 20 2015) [CM]
(defun pvs2cl-primitive-app (expr bindings livevars &optional pvsiosymb expr-actuals)
  (let* ((op    (operator* expr))
	 (args* (arguments* expr));;NSH(1-17-17)
	 (args  (append expr-actuals (loop for arg in args* append arg)))
	 (xtra  (when pvsiosymb (list (type op))))
	 (nargs (length args)))
    (cond (pvsiosymb (mk-funapp pvsiosymb (append (pvs2cl_up* args bindings livevars) xtra)))
	  ((> nargs 1)
	   (if (equality-disequality? op)
	       (pvs2cl-equality op args bindings livevars)
	     (mk-funapp (pvs2cl-primitive2 op)
		        (pvs2cl_up* args bindings livevars))))
	  (t (mk-funapp (pvs2cl-primitive op)
			(pvs2cl_up* args bindings livevars))))))

(defun pvs2cl-equality (op args bindings livevars);;op is either = or /=
  (let* ((id (id op))
	 (modinst  (module-instance op))
	 (acts (when modinst (actuals modinst)))
	 (stype (find-supertype (type-value (car acts))))
	 (arg1 (car args))
	 (arg2 (cadr args)))
    (cond ((tc-eq  (type-value (car acts)) *number*) `(,id ,(pvs2cl_up* arg1 bindings livevars)
							  ,(pvs2cl_up* arg2 bindings livevars)))
	  ((funtype? stype)
	   (let* ((xid (make-new-variable '|x| nil))
		  (xbnd (make-bind-decl xid (domain stype)))
		  (xvar (make-variable-expr xbnd))
		  (equality (make-equation (make-application arg1 xvar)
					   (make-application arg2 xvar)))
		  (allexpr (if (eq id '=)
			       (make-forall-expr (list xbnd) equality)
			     (make-exists-expr  (list xbnd) (make-negation equality)))))
	     (pvs2cl_up* allexpr bindings livevars)))
	  (t (mk-funapp (pvs2cl-primitive2 op)
			(pvs2cl_up* args bindings livevars))))))


(defun equality-disequality? (op)
  (let ((id (id op))
	(modinst (module-instance op)))
    (or (and (eq id '=)(eq (id modinst) '|equalities|))
	(and (eq id '/=)(eq (id modinst) '|notequal|)))))

(defmethod reverse-list-expr ((expr list-expr) accum)
  (reverse-list-expr (args2 expr) (cons (args1 expr) accum)))

(defmethod reverse-list-expr ((expr t) accum)
  accum)

(defun pvs2cl-reverse-list-expr (rlist bindings livevars accum)
  (cond ((consp rlist)
     (pvs2cl-reverse-list-expr (cdr rlist) bindings
                   (append (updateable-free-formal-vars (car rlist))
                       livevars)
                   (cons (pvs2cl_up* (car rlist) bindings livevars)
                     accum)))
    (t accum)))

(defmethod pvs2cl_up* ((expr list-expr) bindings livevars)
  (when *eval-verbose*
    (format t "IN PVS2CL_UP*: expr is ~a~%" expr))
  (let ((reverse-expr (reverse-list-expr expr nil)))
    `(list ,@(pvs2cl-reverse-list-expr reverse-expr bindings livevars nil))))

(defmethod pvs2cl_up* ((expr application) bindings livevars)
  (with-slots (operator argument) expr
    (let ((op* (operator* expr)))
      (if (constant? op*) ;;assuming all primitive/datatype ops are
	  ;;not curried.
	  (let* ((actuals (expr-actuals (module-instance op*)))
		 (pvsiosymb (when (name-expr? op*)
			      (pvsio-symbol op* (+ (length actuals)(length (arguments expr)))))))
	    (when (mappings (module-instance op*))
	      (pvs2cl-mappings (mappings (module-instance op*))))
	    (if (or (pvs2cl-primitive? op*) pvsiosymb)
		(pvs2cl-primitive-app expr bindings livevars pvsiosymb actuals)
		(if (datatype-constant? operator)
		    (pvs2cl-datatype-application operator expr bindings livevars)
		    ;; (mk-funapp (pvs2cl-resolution operator)
		    ;;	  (pvs2cl_up* (arguments expr) bindings livevars))
		    (pvs2cl-defn-application op* expr bindings livevars))))
	  (mk-funcall (pvs2cl_up* operator bindings
				  (append (updateable-vars
					   argument)
					  livevars))
		      (list (pvs2cl_up* argument bindings
					(append
					 (updateable-free-formal-vars operator)
					 livevars))))))))

(defmethod pvs2cl_up* ((expr equation) bindings livevars)
  (cond ((and (set-list-expr? (args1 expr))
	      (set-list-expr? (args2 expr)))
	 (let ((set1 (pvs2cl_up* (exprs (args1 expr)) bindings livevars))
	       (set2 (pvs2cl_up* (exprs (args2 expr)) bindings livevars)))
	   `(and (subsetp ,set1 ,set2 :test #'equalp)  ;;roll into pvs_equalp
		 (subsetp ,set2 ,set1 :test #'equalp))))
	(t (call-next-method))))

(defmethod pvs2cl_up* ((expr disequation) bindings livevars)
  (cond ((and (set-list-expr? (args1 expr))
	      (set-list-expr? (args2 expr)))
	 (let ((set1 (pvs2cl_up* (exprs (args1 expr)) bindings livevars))
	       (set2 (pvs2cl_up* (exprs (args2 expr)) bindings livevars)))
	   `(not (and (subsetp ,set1 ,set2 :test #'equalp) ;;roll into pvs_equalp
		      (subsetp ,set2 ,set1 :test #'equalp)))))
	(t (call-next-method))))

(defun pvs2cl-datatype-application (operator expr bindings livevars)
  (let ((opdecl (declaration operator)))
    (or (and (eval-info opdecl)
	     (lisp-function opdecl)) ;;generate code if needed
	(pvs2cl-datatype operator))
    (let* ((domtyp (domain (declared-type (declaration operator))))
	   (args (if (and (or (type-name? domtyp)
			      (and (tupletype? (find-supertype domtyp))
				   (type-name? (print-type domtyp))))
			  (> (length (arguments expr)) 1))
		     (argument* expr)
		   (arguments expr))) ;; (arguments expr)) Takes care of tuples June 29, 2019 [CM]
	   (clargs (pvs2cl_up* args bindings livevars)))
      (if (constructor? operator);;i.e., also a co-constructor
	  (if (not (eql (length args)(arity operator)))
	      (mk-funcall (mk-funapp (lisp-function opdecl) nil)
			  clargs)
	    (mk-funapp (lisp-function2 opdecl)
		       (if (co-constructor? operator)
			   (loop for arg in clargs
				 collect `(delay ,arg))
			 clargs)))
	(mk-funapp (lisp-function2 opdecl)
		   clargs)))))

(defun codatatype-expression? (expr)
  (codatatype? (adt (find-supertype (type expr)))))
  
      

(defun pvs2cl-defn-application (op* expr bindings livevars)
  (with-slots (operator argument) expr
    (if (constant? op*)
	(let* ((defax (def-axiom (declaration op*)))
	       (defrhs (when defax (args2 (car (last defax)))))
	       (defbindings* (when (lambda-expr? defrhs)
			       (bindings* defrhs)))
	       (args* (loop for ar in (argument* expr)
			 collect ar)))
	  (if (and defax
		   (= (length defbindings*)
		      (length args*)))
	      ;; (loop for bn in defbindings*
	      ;; 	 as ar in args*
	      ;; 	 always (= (length bn)(length ar))))
	      ;;NSH(6.26.02) This still doesn't handle multiple
	      ;;tuple arguments for curried application.
	      (mk-fun2-application op*
				   (loop for ar in args*
				      as bn in defbindings*
				      append
					(cond ((= (length bn) 1)
					       (list ar))
					      ((and (> (length bn) 1)
						    (not (arg-tuple-expr? ar)))
					       (make-projections ar))
					      (t (argument-list ar))))
				   (loop for bn in defbindings*
				      append bn)
				   bindings livevars)
	      (pvs2cl-application operator argument bindings livevars)))
	(pvs2cl-application operator argument bindings livevars))))

(defun pvs2cl-application (operator argument bindings livevars)	
  (let ((clop (pvs2cl_up* operator bindings ;in case of actuals
			  (append (updateable-vars argument)
				  livevars)))
	(clargs (list (pvs2cl_up* argument
				  bindings
				  (append;;check if this should
				   ;;be updateable-vars
				   (updateable-free-formal-vars operator)
				   livevars)))))
    (if (and (symbolp clop);;if operator is a bound variable.
	     (not (member clop bindings
			  :key #'cdr)))
	(mk-funapp clop clargs)
	(mk-funcall clop clargs))))

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
	    `(let ,let-pairs ,cl-expression)))  ;;use ,@declaration
      (if (= (length let-bindings) 1)
	  (let* ((clargs (pvs2cl_up* args bindings livevars))
		 (let-pairs
		  (list (list (car let-bindings)
			      (cons 'pvs2cl_tuple clargs)))))
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
    (if result
	(cdr result)
	(let ((undef (undefined expr "Hit untranslatable term: no binding for ~a")))
	  `(funcall ',undef)))))

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
	    (if (= (length let-bindings)(length args));;shouldn't the lengths be equal
		(pairlis let-bindings args)
		(loop for x in let-bindings  ;;can this case occur?
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
  (with-slots (expression selections else-part) expr
    ;;NSH(10/10/18): modified to let-lift expression to avoid duplicated computation
    (if (variable? expression)
	(pvs2cl_up* (mk-translate-cases-to-if expr) bindings livevars)
      (let* ((newbd (make-new-bind-decl (type expression)))
	     (newvar (make-variable-expr newbd))
	     (new-cases-expr (mk-cases-expr newvar selections else-part))
	     (new-body (mk-translate-cases-to-if new-cases-expr))
	     (new-let-expr (make!-let-expr (list (cons newbd expression)) new-body)))
	(pvs2cl_up* new-let-expr bindings livevars)))))

(defmethod pvs2cl_up* ((expr lambda-expr) bindings livevars)
  (with-slots ((expr-bindings bindings) expression) expr
      (if (and *destructive?*
	       (singleton? expr-bindings)
	       (array-bound (type expr))
	       (application? expression)
	       (variable? (argument expression))
	       (same-declaration (car expr-bindings)
			  (argument expression))
	       (not (member (car expr-bindings)
			    (freevars (operator expression))
			    :test #'same-declaration)))
	  (pvs2cl_up* (operator expression) bindings livevars)
	  (pvs2cl-lambda (bindings expr) (expression expr) bindings))))
    ;;was (updateable-vars expr))) ;;unsafe to update any freevar

(defmethod pvs2cl_up* ((expr forall-expr) bindings livevars)
  (let* ((binds (bindings expr))
	 (body (expression expr)))
    (if binds
	(let* ((bind1 (car binds))
	       (typ1 (type bind1))
	       (sub (simple-subrange? typ1))
	       (bind-rest (cdr binds))
	       (expr-rest (if bind-rest
			      (make!-forall-expr bind-rest body)
			      body)))
	  (cond ((enum-adt? (find-supertype typ1))
		 (if (subtype? typ1)
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (find-supertype typ1)))
				 bindings livevars)
		     `(every ,(pvs2cl-lambda (list bind1)
					     expr-rest
					     bindings)
					     ;;(append (updateable-vars expr)
						;;     livevars)
			     (list ,@(pvs2cl_up* (constructors typ1)
						 bindings livevars)))))
		(sub
		 (if (and (subtype? typ1)
			  (simple-subrange? (supertype typ1)))
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (supertype typ1)))
				 bindings livevars)
		     (let  ((lfrom (if (rational-expr? (car sub))
				       (number (car sub))
				       (or (pvs2cl_up* (car sub) bindings livevars)
					   (let ((undef (undefined (car sub))))
					     `(funcall ',undef)))))
			    (lto (if (rational-expr? (cdr sub))
				     (number (cdr sub))
				     (or (pvs2cl_up* (cdr sub) bindings livevars)
					 (let ((undef (undefined (cdr sub))))
					   `(funcall ',undef)))))
			    (i (gentemp)))
		       `(loop for ,i
			      from ,lfrom
			      to ,lto
			      always
			      (pvs-funcall ,(pvs2cl-lambda (list bind1)
							   expr-rest
							   bindings)
							   ;;(append (updateable-vars expr)
								;;   livevars)
					   ,i)))))
		(t (let ((undef (undefined
				 bind1
				 (format nil
				     "In ~@[~a.~]~a: Hit non-scalar/subrange variable ~a in ~% ~a"
				   (when (and *pvs2cl-decl* (module *pvs2cl-decl*))
				     (id (module *pvs2cl-decl*)))
				   (when *pvs2cl-decl* (id *pvs2cl-decl*))
				   bind1 expr))))
		   `(funcall ',undef)))))
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
			      (make!-exists-expr bind-rest body)
			      body)))
	  (cond ((enum-adt? (find-supertype typ1))
		 (if (subtype? typ1)
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (find-supertype typ1)))
				 bindings livevars)
		     `(some ,(pvs2cl-lambda (list bind1)
					    expr-rest
					    bindings)
			    ;;(append (updateable-vars expr)
			    ;;    livevars)
			    (list ,@(pvs2cl_up* (constructors typ1)
						bindings livevars)))))
		(sub
		 (if (and (subtype? typ1)
			  (simple-subrange? (supertype typ1)))
		     (pvs2cl_up* (lift-predicates-in-quantifier expr (list (supertype typ1)))
				 bindings livevars)
		     (let ((lfrom (if (rational-expr? (car sub))
				      (number (car sub))
				      (or (pvs2cl_up* (car sub) bindings livevars)
					  (let ((undef (undefined (car sub))))
					    `(funcall ',undef)))))
			   (lto (if (rational-expr? (cdr sub))
				    (number (cdr sub))
				    (or (pvs2cl_up* (cdr sub) bindings livevars)
					(let ((undef (undefined (cdr sub))))
					  `(funcall ',undef)))))
			   (i (gentemp)))
		       `(loop for ,i
			      from ,lfrom
			      to ,lto
			      thereis
			      (pvs-funcall ,(pvs2cl-lambda (list bind1)
							   expr-rest
							   bindings)
					   ;;(append (updateable-vars expr)
					   ;;   livevars)
					   ,i)))))
		(t
		 (let ((undef (undefined expr "Hit non-scalar/subrange quantifier in ~% ~a")))
		   `(funcall ',undef)))))
	(pvs2cl_up* body bindings livevars))))
			

(defmethod pvs2cl_up* ((expr name-expr) bindings livevars)
  (let* ((decl (declaration expr))
	 (thinst (module-instance expr))
	 (bnd (assoc decl bindings :key #'declaration)))
    (assert (not (and bnd (const-decl? decl))))
    (when (mappings thinst)
      (pvs2cl-mappings (mappings thinst)))
    (if bnd
	(if (const-decl? decl)
	    (mk-funapp (cdr bnd)
		       (pvs2cl-actuals (expr-actuals (module-instance expr))
				       bindings livevars))
	    (cdr bnd)) 
	(if (const-decl? decl)
	    (pvs2cl-constant expr bindings livevars)
	    (let ((undef (undefined expr "Hit untranslateable expression ~a")))
	      `(funcall ',undef))))))

(defmethod pvs2cl_up* ((expr list) bindings livevars)
  (if (consp expr)
      (cons (pvs2cl_up* (car expr) bindings
			(append (updateable-vars (cdr expr)) livevars))
	    (pvs2cl_up* (cdr expr) bindings  ;;need car's freevars
			(append (updateable-free-formal-vars (car expr)) ;;f(A, A WITH ..)
				livevars)))
      nil))

(defmethod pvs2cl_up* ((expr rational-expr) bindings livevars)
  (declare (ignore bindings livevars))
  (number expr))

(defmethod pvs2cl_up* ((expr tuple-expr) bindings livevars)
  (let ((args (pvs2cl_up* (exprs expr) bindings livevars)))
    `(pvs2cl_tuple ,@args)))

(defun make-pvslisp-string (size expr)
  (let ((charlist (loop for i from 0 to (1- size) collect (pvs-funcall expr i))))
  (coerce charlist 'string)))
  

(defmethod pvs2cl_up* ((expr record-expr) bindings livevars)
  ;;add special case for strings
  (let ((args (pvs2cl_up* (mapcar #'expression
			    (sort-assignments (assignments expr)))
			  bindings livevars))
					;(stype (find-supertype (type expr)))(break "record-expr: string = ~a" (tc-eq stype *string-type*))
	)
        (if (compatible? (type expr) *string-type*)
	    `(make-pvslisp-string ,(car args) ,(cadr args))
	  (if (finseq-type? stype)
	      `(pvs2cl_finseq ,(car args) ,(cadr args))
	    `(pvs2cl_record ,@args)))))

(defmethod pvs2cl_up* ((expr projection-application) bindings livevars)
    `(project ,(index expr) ,(pvs2cl_up* (argument expr) bindings livevars)))

(defmethod pvs2cl_up* ((expr injection-application) bindings livevars)
    `(inject ,(index expr) ,(pvs2cl_up* (argument expr) bindings livevars)))

(defun sorted-fields (typ)                
  (let ((restructure   (copy-list (fields typ))))     
    (sort restructure                            
          #'string-lessp                         
          :key #'(lambda (x) (id x)))))

(defun get-field-num (id typ)
   (position id (sort-fields (fields typ))
             :test #'(lambda (x y) (eq x (id y)))))

(defmethod pvs2cl_up*  ((expr field-application) bindings livevars)
  (with-slots (argument) expr
    (let* ((clarg (pvs2cl_up* argument bindings livevars))
	   (argtype (find-supertype (type argument)))
	   (fieldnum (get-field-num (id expr) argtype)))
      (if (finseq-type? argtype)
	  (let* ((argvar (gentemp "arg"));;NSH(1/12/25: pvs-funcall handles string literals
		 (fldapp (if (zerop fieldnum) `(length ,argvar) argvar))) 
	    `(let ((,argvar ,clarg))
	       (if (stringp ,argvar)
		   ,fldapp
		   (project ,(1+ fieldnum) ,argvar))))
	  `(project ,(1+ fieldnum) ,clarg))
      ;; )
      )))



(defmethod no-livevars? ((expr update-expr) livevars assignments)
  (no-livevars? (expression expr) livevars
		(append (assignments expr) assignments)))

(defmethod no-livevars? ((expr t) livevars assignments)
  (let* ((updateable-outputs
	  (updateable-outputs expr))
	 (check-updateables
	  (null (intersection  updateable-outputs
			       livevars
			       :test #'same-declaration)))
	 (rhs-list (loop for asgn in assignments
			 collect (expression asgn)))
	 (check-rhs (check-update-argument rhs-list updateable-outputs))
	 (rhs-updateables (updateable-free-formal-vars rhs-list))
	 (live-rhs-updateables
	  (set-difference rhs-updateables updateable-outputs
			  :test #'same-declaration)))
    (and check-updateables
	 check-rhs
	 (cons updateable-outputs (append live-rhs-updateables
					  livevars)))))

(defvar *fun-lhs* nil)
(defvar *update-args-bindings* nil)
(defvar *lhs-args* nil)

(defun array-bound (type)
  (and (funtype? type)
       (let ((x (simple-below? (domain type))))
	 (or x
	     (let ((y (simple-upto? (domain type))))
	       (or (and y
			(make!-succ y))
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
			 (let* ((bound (array-bound type)));;NSH(9-19-12)
			   (if bound ;;then cl-expr is an array
			       (let ((cl-bound (pvs2cl_up* bound
							   bindings livevars)))
				 `(if (<= ,cl-bound ,*eval-array-bound*)
				      (mk-fun-array ,cl-expr ,cl-bound)
				    (make-closure-hash ,cl-expr)))
			     `(make-closure-hash ,cl-expr)))
			     ;;else it is a function and we make a closure-hash
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
				 ;;(declare ((simple-array t) ,exprvar))
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
	     (cl-bound (when bound (pvs2cl_up* bound
					       bindings livevars)))
	     (cl-expr (if (symbolp cl-expr)
			  cl-expr
			(if bound
			    `(mk-fun-array ,cl-expr ,cl-bound ,lhsvar)
			  `(make-closure-hash ,cl-expr))))
	     (cl-expr-var (gentemp "E"))
	     (newexpr (if bound `(aref ,cl-expr-var ,lhsvar)
			`(pvs-closure-hash-lookup ,cl-expr-var ,lhsvar)))
	     (newrhs (if (null (cdr args))
		      rhs
		      (pvs2cl-update-nd-type
		      (range type) newexpr (cdr args) rhs
		      bindings livevars))))
	(push (list lhsvar cl-args1) *lhs-args*)
	(if bound
	    `(let ((,cl-expr-var ,cl-expr));;NSH(10.21.14): without let, the cl-expr-var is unbound
	       (pvs-setf ,cl-expr-var ,lhsvar ,newrhs))
	  `(let ((,cl-expr-var ,cl-expr))(pvs-function-update ,cl-expr-var ,lhsvar ,newrhs)))))

#+sbcl
(sb-ext:define-hash-table-test pvs_equalp sxhash)

(defun make-closure-hash (expr)	;;NSH(9-19-12)
  (if (pvs-closure-hash-p expr)
      expr
      (mk-pvs-closure-hash (make-hash-table :test 'pvs_equalp) expr)))

(defmethod pvs2cl-update-assign-args ((type subtype) cl-expr args rhs
					 bindings livevars)
  (pvs2cl-update-assign-args (find-supertype type) cl-expr args
			     rhs bindings livevars))

(defmethod pvs2cl-update-assign-args ((type recordtype) cl-expr args rhs
					 bindings livevars)
  (let* ((args1 (car args))
	 (id (id (car args1)))
	 (fields (sort-fields (fields type)))
	 (cl-expr-var (gentemp "E"))
	 ;;(rhs-var (gentemp "R"))
	 )
    (if (tc-eq type *string-type*)
	(if (eq id 'seq)
	    (if (cdr args) ;;if the index i of the string is being updated
		(let ((index-var (gentemp "I"))
		      (index-expr (pvs2cl_up* (caadr args) bindings
					      livevars)))
		  `(let ((,cl-expr-var ,cl-expr)
			 (,index-var ,index-expr))
		     (setf (aref ,cl-expr-var ,index-var) ,rhs)
		     ,cl-expr-var))
	      `(coerce ,rhs 'string)) ;replacing the entire seq field
	  cl-expr); can't update length field of a string in isolation
      (let* ((field-num (position  id fields :test #'(lambda (x y) (eq x (id y)))))
	     (newexpr `(svref ,cl-expr-var ,field-num))
	     (field-type (type (find id fields :key #'id) ))
 	     (dep-fields (sort-fields (fields type) t))	;;dependent sort
 	     (new-bindings (pvs2cl-add-dep-field-bindings  dep-fields fields id
							   cl-expr-var bindings))
	     (other-updateable-types ;;to prevent updates to aliases
	      (loop for fld in (fields type)
		    when (not (eq id (id fld)))
		    nconc (top-updateable-types (type fld) nil)))
	     (rhs-var (gentemp "R"))
	     (newrhs  (if (null (cdr args))
			  rhs
			(if (member field-type
				    other-updateable-types
				    :test #'compatible?)
			    (pvs2cl-update-nd-type
			     field-type newexpr (cdr args) rhs
			     new-bindings livevars)
			  (pvs2cl-update-assign-args
			   field-type newexpr (cdr args) rhs
			   new-bindings livevars)))))
	`(let* ((,cl-expr-var ,cl-expr)
		(,rhs-var ,newrhs))
	   (rec-tup-update ,cl-expr-var ,field-num ,rhs-var)
	   ,cl-expr-var)))))
;     `(let ((,cl-expr-var ,cl-expr))
;        (declare (simple-array ,cl-expr-var))
;        (setf ,newexpr ,newrhs)
;        ,cl-expr-var)))

(defmethod pvs2cl-update-assign-args ((type tupletype) cl-expr args rhs
					 bindings livevars)
  (let* ((args1 (car args))
	 (num (number (car args1)))
	 (cl-expr-var (gentemp "E"))
	 (newexpr `(project ,cl-expr-var ,num))
	 (types (types type))
	 (tupsel-type (nth (1- num) types))
	 (other-updateable-types
	  (loop for fld in types
		as pos from 1  
		when (not (eql pos num))
		nconc (top-updateable-types fld nil)))
	 (new-bindings (pvs2cl-add-dep-tuple-bindings types (1- num) 0 cl-expr-var
						      bindings))
	 (rhs-var (gentemp "R"))
	 (newrhs  (if (null (cdr args))
		      rhs
		      (if (member tupsel-type
			      other-updateable-types
			      :test #'compatible?)
		      (pvs2cl-update-nd-type
		       tupsel-type newexpr (cdr args) rhs
		       new-bindings livevars)
		      (pvs2cl-update-assign-args
		       tupsel-type newexpr (cdr args) rhs
		       new-bindings livevars)))))
    `(let* ((,cl-expr-var ,cl-expr)
	   (,rhs-var ,newrhs))
       (setf (svref ,cl-expr ,(1- num)) ,rhs-var)
       ,cl-expr-var)))

(defun pvs2cl-add-dep-tuple-bindings (dep-tuple-types position index expr bindings)
  (if (eql position index)
      bindings
	(if (binding? (car dep-tuple-types))
	    (pvs2cl-add-dep-tuple-bindings  (cdr dep-tuple-types) position (1+ index)
					    expr
					      (acons (car dep-tuple-types)
						     `(svref ,expr ,position) bindings))
	  (pvs2cl-add-dep-tuple-bindings  (cdr dep-tuple-types)
					      position (1+ index)  expr
					      bindings))))
	
(defun mk-fun-array (expr size &optional update-index) ;;okay to use aref here instead of pvs-setf
  (cond ((or (simple-vector-p expr) 
	  (hash-table-p expr) (null size))
	 expr)
	((vectorp expr)
	 (let ((arraysize (array-total-size expr)))
	   (if (< arraysize size);;then resize, else do nothing
	     (adjust-array expr (resize-newsize arraysize size)))
	   (setf (fill-pointer expr) size)
	   expr))
	((pvs-outer-array-p expr)
	 (let* ((arr (make-array size :initial-element 0
				      :fill-pointer size
				      :adjustable t))
		(inner-array (pvs-outer-array-inner-array expr))
		(contents (pvs-array-contents inner-array))
		(inner-size (pvs-array-size inner-array))
		(offset (pvs-outer-array-offset expr))
		(outer-diffs (pvs-outer-array-diffs expr))
		(inner-diffs (pvs-array-diffs inner-array)))
	   (loop for i from 0 to (1- size)
		 unless (eql i update-index)
		 do (setf (aref arr i)(aref contents i)))
	   (loop for (x . y) in inner-diffs
		 as i from (1+ offset) to inner-size
		 do (setf (aref arr x) y))
	   (insert-array-diffs outer-diffs arr)))
	(t (let ((arr (make-array size :initial-element 0
				       :fill-pointer size
				       :adjustable t)))
	     (loop for i from 0 to (1- size);;unless avoids evaluating expr on 
		   unless (eql i update-index) do ;;newly expanded index
		   (setf (aref arr i)(funcall expr i)))
	     arr))))

(defun resize-newsize (arraysize newsize)
  (if (< arraysize newsize)
      (resize-newsize (* 2 (max arraysize 1)) newsize)
    arraysize))

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
;;    (break "outer-array-update")
    (push (list arg1var cl-arg1) *lhs-args*)
    (if bound 
	`(pvs-outer-array-update ,expr ,arg1var ,newexpr ,cl-bound)
      `(pvs-function-update (mkcopy-pvs-closure-hash ,expr)  ,arg1var ,newexpr))))

(defmethod pvs2cl-update-nd-type* ((type recordtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((id (id (car arg1)))
	 (fields  (sort-fields (fields type)))
	 (field-num (position  id fields :test #'(lambda (x y) (eq x (id y)))))
	 (cl-expr-var (gentemp "E"))	 
	 (new-expr `(svref ,cl-expr-var ,field-num))	 
	 (field-type (type (find id fields :key #'id) ))
 	 (dep-fields (sort-fields (fields type) t));;dependent sort	 
	 (new-bindings (pvs2cl-add-dep-field-bindings  dep-fields fields id
						  cl-expr-var bindings))
	 (newval (pvs2cl-update-nd-type field-type new-expr
					restargs assign-expr new-bindings
					livevars)))
    `(let ((,cl-expr-var ,expr))
       (nd-rec-tup-update ,expr ,field-num ,newval))))

(defun pvs2cl-add-dep-field-bindings (dep-field-types field-types field-id expr bindings)
  (if (consp dep-field-types)
      (if (eq (id (car dep-field-types)) field-id)
	  bindings
	(if (binding? (car field-types))
	    (let ((index (position (id (car dep-field-types)) field-types
				   :test #'(lambda (x y)(eq x (id y))))))
	      (pvs2cl-add-dep-field-bindings  (cdr dep-field-types)
					      field-types field-id  expr
					      (acons (car dep-field-types)
						     `(svref ,expr ,index) bindings)))
	  (pvs2cl-add-dep-field-bindings  (cdr dep-field-types)
					      field-types field-id  expr
					      bindings)))
      bindings))

(defmethod pvs2cl-update-nd-type* ((type tupletype)  expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((num (number (if (consp arg1) (car arg1) arg1)))
	 (new-expr `(svref ,expr ,(1- num)))
	 (pos (1- num))
	 (tupsel-type (nth pos (types type)))
	 (newval (pvs2cl-update-nd-type tupsel-type new-expr
					restargs assign-expr bindings
					livevars)))
    `(nd-rec-tup-update ,expr ,pos ,newval)))

(defmethod pvs2cl-update-nd-type* ((type subtype)  expr arg1 restargs
				   assign-expr bindings livevars)
  (pvs2cl-update-nd-type* (find-supertype type) expr arg1 restargs
			  assign-expr bindings livevars))

(defmethod pvs2cl-update-nd-type* ((type adt-type-name) expr arg1 restargs
				   assign-expr bindings livevars)
  (break "Need to fix this"))


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
		  (no-livevars? expression livevars assignments))
; 		 (check-assign-types
; 		  (loop for assgn in assignments
; 			always
; 			(not (contains-possible-closure?
; 			      (type (expression assgn))))))
		 )
	    ;;very unrefined: uses all
	    ;;freevars of eventually updated expression.
	    (cond (updateable-livevars ;; check-assign-types
		   (push-output-vars (car updateable-livevars)
				     (cdr updateable-livevars))
		   (pvs2cl-update expression
				  assignments
				  bindings livevars))
		  (t
		   (when (and *eval-verbose* (not updateable-livevars))
		     (format t "~%Update ~s translated nondestructively.
 Live variables ~s present" expr livevars))
; 		   (when (and *eval-verbose* (not check-assign-types))
; 		     (format t "~%Update ~s translated nondestructively.
; Assignment right-hand sides contain possible function types that can
; trap references." expr))
		   (pvs2cl-update-nondestructive  expr
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
    
(defun pvs2cl-lambda (bind-decls expr bindings) ;;removed livevars
  (let* ((*destructive?* nil)
	 (bind-ids (pvs2cl-make-bindings bind-decls bindings))
	 (cl-body (pvs2cl_up* expr
			   (append (pairlis bind-decls bind-ids)
				   bindings)
			   nil))) ;;NSH(6-26-02) was livevars
    (if (eql (length bind-ids) 1)
	(let ((size (array-bound (type (car bind-decls))))
	      (fun `(function (lambda ,bind-ids ,cl-body))))
	  (if size
	      `(mk-pvs-array-closure ,(pvs2cl_up* size bindings nil) ,fun)
	    fun))
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
			(lisp-id id)
			(pvs2cl-newid (id bb) bindings))))
	(cons newid (pvs2cl-make-bindings (cdr bind-decls) bindings)))
      nil))
      

(defun pvs2cl-bindings (bind-decls bindings)
  (if (consp bind-decls)
      (let* ((bind (car bind-decls))
	     (newid
	      (if (rassoc bind bindings)
		  (pvs2cl-newid (id bind) bindings)
		  (lisp-id (id bind)))))
	(pvs2cl-bindings (cdr bind-decls)
		      (cons (cons bind newid) bindings)))
      bindings))

(defparameter *primitive-constants* '(TRUE FALSE |null|))

;;Added support for PVSio's constants (Feb 20 2015) [CM]
(defun pvs2cl-constant (expr bindings livevars)
  (cond ((pvs2cl-primitive? expr)
	 (if (memq (id expr) *primitive-constants*) ;the only constants
	     (pvs2cl-primitive expr)	;else need to return closures.
	     `(function ,(pvs2cl-primitive expr))))
	((lazy-random-function? expr)
	 (generate-lazy-random-lisp-function expr))
	((pvs2cl-mapped? expr)
	 (generate-pvs2cl-mapped-function expr))
	(t 
	 (let* ((nargs (if (funtype? (type expr)) (arity expr) 0))
		(pvsiosymb (pvsio-symbol expr nargs)))
	   (cond ((and pvsiosymb (= nargs 0))
		  (mk-funapp pvsiosymb (list (type expr)))) ; it's a PVSio constant
		 (pvsiosymb `(function ,pvsiosymb)) ; need to return a closure (it's a PVSio function)
		 (t
		  (pvs2cl-resolution expr)
		  (if (datatype-constant? expr)
		      (if (scalar-constant? expr)
			  (lisp-function (declaration expr))
			(let ((fun (or (lisp-function (declaration expr))
				       (pvs2cl-lisp-function (declaration expr)))))
			  (assert fun)
			  (if (constructor? expr)
			      (mk-funapp fun nil)
			    `(function ,fun))
			  ;; (if (not (funtype? (find-supertype (type expr))))
			  ;; 	 (mk-funapp fun nil)
			  ;; 	 `(function ,fun))
			  ));;actuals irrelevant for datatypes
		    (let* ((actuals (expr-actuals (module-instance expr)))
			   (decl (declaration expr))
			   (internal-actuals
			    (or actuals
				(and (eq (module decl) *external*)
				     (loop for x in (formals (module decl))
					   when (formal-const-decl? x)
					   collect (make-constant-from-decl x)))))
			   (defns (def-axiom decl))
			   (defn (when defns(args2 (car (last (def-axiom decl))))))
			   (def-formals (when (lambda-expr? defn)
					  (bindings defn)))
			   (fun (if defns
				    (if def-formals
					(if internal-actuals
					    (external-lisp-function (declaration expr))
					  (lisp-function (declaration expr)))
				      (pvs2cl-operator2 expr actuals nil nil
							livevars bindings))
				    (if internal-actuals
					(external-lisp-function (declaration expr))
					(lisp-function (declaration expr))))))
		      (assert fun)
		      (mk-funapp fun (pvs2cl_up* internal-actuals
						 bindings livevars))))))))))
	 
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

(defvar *pvs2cl-mappings*)

(defun pvs2cl-mappings (mappings)
  (when mappings
    (let* ((lhs (lhs (car mappings)))
	   (rhs (rhs (car mappings)))
	   (pmap (assoc lhs *pvs2cl-mappings* :test #'tc-eq)))
      (cond ((null pmap)
	     (push (cons lhs rhs) *pvs2cl-mappings*))
	    ((not (tc-eq (cdr pmap) rhs))
	     (break "~a mapped in more than one way" lhs)))
      (pvs2cl-mappings (cdr mappings)))))

(defun pvs2cl-mapped? (expr)
  (and (boundp '*pvs2cl-mappings*)
       (assoc expr *pvs2cl-mappings* :test #'tc-eq)))

(defun generate-pvs2cl-mapped-function (expr)
  (let ((body (expr (cdr (assoc expr *pvs2cl-mappings* :test #'tc-eq)))))
    (assert (null (type-value body)))
    (break "generate-pvs2cl-mapped-function")))

(defun datatype-constant? (expr)
  (and (adt-name-expr? expr)
       (not (pvs2cl-primitive? expr))))

(defun pvs2cl-resolution2 (expr) ;(break "pvs2cl-resolution2")
  (pvs2cl-resolution expr)
  (if (datatype-constant? expr)
      (lisp-function2 (declaration expr))
      (if (or (expr-actuals (module-instance expr))
	      (eq (module (declaration expr)) *external*)) 
	  (external-lisp-function2 (declaration expr))
	(lisp-function2 (declaration expr)))))

(defun lisp-function (decl)
  (and (in-info decl)(or (in-name decl)(in-name-m decl))))

(defun lisp-function2 (decl)
  (and (in-info decl)(or (in-name-m decl)(in-name decl))))

(defun external-lisp-function (decl)
  (and (ex-info decl)(or (ex-name decl)(ex-name-m decl))))

(defun external-lisp-function2 (decl)
  (and (ex-info decl)(or (ex-name-m decl)(ex-name decl))))

(defun lisp-function-d (decl)
  (and (ex-info decl)(or (in-name-d decl)(lisp-function2 decl))))

(defun external-lisp-function-d (decl)
  (and (ex-info decl)(or (ex-name-d decl)(external-lisp-function2 decl))))

(defun pvs2cl-resolution (expr)
  (let* ((decl (declaration expr))
	 (*current-context* (saved-context (module decl))))
    (make-c-eval-info decl)
    (if (datatype-constant? expr)
	(or (lisp-function2 (declaration expr))
	    (pvs2cl-datatype expr))
	(if (or (eq (module decl) *external*)
		(expr-actuals (module-instance expr)));;datatype-subtype?
	    (or (external-lisp-function (declaration expr))
		(pvs2cl-external-lisp-function (declaration expr)))
	    (or (lisp-function (declaration expr))
		(pvs2cl-lisp-function (declaration expr)))))))

(defun mk-newsymb (x &optional (counter 0))
  (let ((str (format nil "~a_~a" x counter)))
    (if (find-symbol str)
	(mk-newsymb x (1+ counter))
      (intern str :pvs))))

;;dummy function to initialize symbol-function of a new function symbol
(defun init-undef (&rest x)
  (declare (ignore x))
  0)

(defun mk-newfsymb (x &optional counter)
  (handler-case
      (let ((fsym (intern (format nil "~a~@[_~a~]" x counter) :pvs)))
	(cond ((fboundp fsym)
	       (mk-newfsymb x (if counter (1+ counter) 0)))
	      (t (setf (symbol-function fsym) #'init-undef) fsym)))
    #+sbcl (sb-ext:symbol-package-locked-error () (mk-newfsymb x 0))
    ;; Not sure the allegro error is necessary, but it's here just in case
    #+allegro (excl:package-locked-error () (mk-newfsymb x 0))))

(defun pvs2cl-id (x)
  (if (eq (id x) 'O)
      'o
      (id x)))

;;Local variable undef moved to case where defax is null (Feb 20 2015) [CM]
(defun pvs2cl-external-lisp-function (decl)
  (let* ((defax (def-axiom decl))
	 (*external* (module decl))
	 (*pvs2cl-decl* decl))
    (cond ((null defax)
	   (let ((undef (undefined decl)))
	     (make-c-eval-info decl)
	     (setf (ex-name decl) undef
		   (ex-name-m decl) undef
		   (ex-name-d decl) undef)
	     undef))
	  (t (let ((ctheory (current-theory))
		   (formals (loop for x in (formals (module decl))
			       when (formal-const-decl? x)
			       collect x)))
	       (setf (current-theory) (module decl))
	       (unwind-protect
		    (if (null formals)
			(or (lisp-function decl)
			    (pvs2cl-lisp-function decl))
			(let* ((id (mk-newfsymb (format nil "~a_~a"
						  (id (module decl))
						  (pvs2cl-id decl))))
			       (id-d (mk-newfsymb (format nil "~a!~a"
						    (id (module decl))
						    (pvs2cl-id decl))))
			       (formal-ids (loop for x in formals
					      collect (lisp-id (id x))))
			       (bindings (pairlis formals formal-ids))
			       (defn (args2 (car (last defax))))
			       (defn-bindings (when (lambda-expr? defn)
						(loop for bnd in
						     (bindings* defn)
						   append bnd)))
			       (defn-expr (body* defn))
			       (defn-binding-ids
				(make-binding-ids-without-dups defn-bindings nil))
			       (formal-ids2 (append formal-ids
						    defn-binding-ids))
			       (declarations
				(pvs2cl-declare-vars formal-ids2
						     (append formals defn-bindings))))
			  (make-c-eval-info decl)
			  (setf (ex-name decl) id)
			  (let ((id2 (mk-newfsymb (format nil "~a__~a"
						    (id (module decl))
						    (pvs2cl-id decl)))))
			    (setf (ex-name-m decl) id2)
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
			    )
			  (setf (ex-name-d decl) id-d)
			  (let ((*destructive?* t)
				(*output-vars* nil))
			    (setf (definition (ex-defn-d decl))
				  `(defun ,id-d ,formal-ids2
				     ,declarations
				     ,@(append (when declarations
						 (list declarations))
					       (list 
						(pvs2cl-till-output-stable
						 (ex-defn-d decl)
						 defn-expr
						 (append (pairlis defn-bindings
								  defn-binding-ids)
							 bindings)
						 nil)))))
			    ;;setf output-vars already in
			    ;;pvs2cl-till-output-stable
			    (setf (output-vars (ex-defn-d decl)) *output-vars*))
			  (eval (definition (ex-defn-d decl)))
			  (assert id-d)
			  (compile id-d)
			  (let ((*destructive?* nil)
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
			  ))
		 (setf (current-theory) ctheory)))))))

(defun pvs2cl-till-output-stable (defn-slot expr bindings livevars)
  (let ((old-outputvars (copy-list *output-vars*))
	(cl-expr (pvs2cl_up* expr bindings livevars)))
    (cond ((equal old-outputvars *output-vars*)
	   cl-expr)
	  (t (setf (output-vars defn-slot) *output-vars*)
	     (pvs2cl-till-output-stable defn-slot expr bindings livevars)))))
    

(defmethod body* ((expr lambda-expr))
  (body* (expression expr)))

(defmethod body* ((expr t))
  expr)

(defun make-binding-ids-without-dups (bindings aux)
  (if (consp bindings)
      (let ((id (id (car bindings))))
	(if (or (memq id aux)
		(special-variable-p id)
		(constantp id)) ; SO 2002-07-31 - lisp constants are also
					; problematic (e.g., t or nil).
	    (make-binding-ids-without-dups
	     (cdr bindings)
	     (cons (gentemp (string id)) aux))
	    (make-binding-ids-without-dups
	     (cdr bindings)
	     (cons id aux))))
    (nreverse aux)))

;;NSH(6/11/2016): pvs-defconstants can have their definitions precomputed and cached.
(defun pvs-defconstant? (expr)
  (and (name-expr? expr)
       (pvs-defconstant-decl? (declaration expr))))

(defun const-params (decl)
  (loop for x in (formals (module decl))
	when (formal-const-decl? x)
	collect (make-constant-from-decl x)))


(defun pvs-defconstant-decl?  (decl)
  (let ((defax (def-axiom decl)))
    (and (const-decl? decl)
	 (null (const-params decl))
	 (not (null defax))
	 (not (lambda-expr? (args2 (car (last defax))))))))
	 

;;
;; BD: allegro goes nuts if we try to compile things like 
;;  (define x () <long-list of pairs of integers>)
;; but it takes no time to evaluate 'x'.
;;
;; This code tries to detect when a pvs definition looks like
;; a long list and prevents compilation in this case.
;; This is a temporary hack for the DoubleHelix project.
;;
(defmethod expr_is_long_list ((expr list-expr) len_so_far)
  (or (>= len_so_far 200) (expr_is_long_list (args2 expr) (+ len_so_far 1))))

(defmethod expr_is_long_list ((expr t) len_so_far)
  (>= len_so_far 200))

(defun skip_compile (bindings defn)
  (declare (ignore bindings))
  (expr_is_long_list defn 0))
;;
;; End of Hack
;;

;;Local variable undef moved to case where defax is null (Feb 20 2015) [CM]
(defun pvs2cl-lisp-function (decl)
  (let* ((defax (def-axiom decl))
	 (*external* nil)
	 (*pvs2cl-decl* decl))
    (cond ((null defax)
	   (let ((undef (undefined decl)))
	     (setf (in-name decl) undef
		   (in-name-m decl) undef
		   (in-name-d decl) undef)
	     undef))
	  (t (let* ((id (mk-newfsymb (format nil "~@[~a_~]~a"
					     (generated-by decl) (pvs2cl-id decl))))
		    ;; (idc (mk-newfsymb (format nil "~@[~a_~]~a_c"
		    ;; 			     (generated-by decl) (pvs2cl-id decl))) )
		    (id-d (mk-newfsymb (format nil "~@[~a_~]~a!"
					       (generated-by decl)
					       (pvs2cl-id decl))))
		    (defn (args2 (car (last (def-axiom decl)))))
		    (defn-bindings (when (lambda-expr? defn)
				     (loop for bnd in
					   (bindings* defn)
					   append bnd)))
		    (defn-body (body* defn))
		    (skip-compile (skip_compile defn-bindings defn-body))
		    (defn-binding-ids
		      (make-binding-ids-without-dups defn-bindings nil))
		    (declarations (pvs2cl-declare-vars defn-binding-ids
						       defn-bindings)))
	       (setf (in-name decl) id)
	       (let ((id2 (mk-newfsymb (format nil "_~@[~a_~]~a"
					 (generated-by decl)
					 (pvs2cl-id decl)))))
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
		 (when *eval-verbose*
		   (format t "~%IN pvs2cl-lisp-function: compile ~a,~_   args = ~a,~_   long-list: ~a~%"
		     id2 defn-binding-ids (expr_is_long_list defn-body 0)))
		 (or skip-compile (compile id2)))
	       (when *eval-verbose*
		 (format t "~%~a <internal_dest> ~a" (id decl) id-d))
	       (setf (in-name-d decl) id-d)
	       (let ((*destructive?* t)
		     (*output-vars* nil))
		 (setf (definition (in-defn-d decl))
		       `(defun ,id-d ,defn-binding-ids
			  ,@(append (when declarations
				      (list declarations))
				    (list 
				     (pvs2cl-till-output-stable
				      (in-defn-d decl)
				      defn-body
				      (pairlis defn-bindings
					       defn-binding-ids)
				      nil)))))
		 ;;setf output-vars already in
		 ;;pvs2cl-till-output-stable
		 (when (and *eval-verbose* *output-vars*)
		   (format t "~%~a: output-vars: ~a" (id decl) *output-vars*))
		 (setf (output-vars (in-defn-d decl));;already done in pvs2cl-till-output-stable?
		       *output-vars*))
	       (eval (definition (in-defn-d decl)))
	       (assert id-d)
	       (or skip-compile (compile id-d))
	       ;;	       (compile id-d)
	       (when *eval-verbose*
		 (format t "~%~a <internal_0> ~a" (id decl) id))
	       (let* ((*destructive?* nil)
		      (in-defn `(defun ,id ()
				  ,(pvs2cl_up* defn nil nil)))
		      ;; (defconst (when (null defn-bindings);same as pvs-defconstant? test
		      ;; 		  `(defconstant ,idc ,(car (last (definition (in-defn-d decl)))))))
		      )
		 (setf (definition (in-defn decl))
			 in-defn))
	       (eval (definition (in-defn decl)))
	       (assert id);;(break "pvs2cl-lisp-function")
	       (or skip-compile (compile id)))))))
;;	       (compile id))))))

(defun pvs2cl-theory (theory)
  (with-context theory
    (cond ((datatype? (current-theory))
	   (let ((adt (adt-type-name (current-theory))))
	     (pvs2cl-constructors (constructors adt) adt))
	   (pvs2cl-theory (adt-theory (current-theory)))
	   (let ((map-theory (adt-map-theory (current-theory)))
		 (reduce-theory (adt-reduce-theory (current-theory))))
	     (when map-theory (pvs2cl-theory (adt-map-theory (current-theory))))
	     (when reduce-theory (pvs2cl-theory (adt-reduce-theory (current-theory))))))
	  (t (loop for decl in (theory (current-theory))
		   do (cond ((type-eq-decl? decl)
			     (let ((dt (find-supertype (type-value decl))))
			       (when (adt-type-name? dt)
				 (pvs2cl-constructors (constructors dt) dt))))
			    ((datatype? decl)
			     (let ((adt (adt-type-name decl)))
			       (pvs2cl-constructors (constructors adt) adt)))
			    ((const-decl? decl)
			     (unless (and (eval-info decl)
					  (or (internal (eval-info decl))
					      (external (eval-info decl))))
			       (progn
				 (unless (eval-info decl)(make-c-eval-info decl))
				 (or (external-lisp-function decl)
				     (pvs2cl-external-lisp-function decl))
				 (or (lisp-function decl)
				     (pvs2cl-lisp-function decl)))))
			    (t nil)))))))

(defun pvs2cl-datatype (expr)
  (let* ((dt (adt expr))
	 (constructors (constructors dt)))
    (pvs2cl-constructors constructors dt)
    (lisp-function (declaration expr))))

(defun pvs2cl-constructors (constrs datatype)
  ;; First create the structures, in case there are shared accessors
  (let ((structs (unless (enum-adt? datatype) (mk-newconstructors constrs))))
    (pvs2cl-constructors* constrs structs structs datatype)))

(defun pvs2cl-constructors* (constrs structs all-structs datatype)
  (when constrs
    (cons (pvs2cl-constructor (car constrs) (car structs) all-structs datatype)
	  (pvs2cl-constructors* (cdr constrs) (cdr structs)
				all-structs datatype))))

(defun mk-newconstructors (constrs)
  (mapcar #'(lambda (constructor)
	      (let* ((id (id constructor))
		     (accessors (accessors constructor))
		     (accessor-ids (mapcar #'id accessors))
		     (struct-id (mk-newconstructor id accessor-ids))
		     (recognizer-id (makesym "~a?" struct-id))
		     (make-struct (makesym "~a-~a" 'make struct-id))
		     (mk-struct (makesym "mk-~a" struct-id))
		     ;; Can't use :constructor in SBCL without losing
		     ;; ability to read #s(...) forms. Instead we let SBCL
		     ;; create the make- form as usual, and create a mk-
		     ;; function that is used positionally.
		     (defs `(defstruct (,struct-id
					(:predicate ,recognizer-id))
			      ,@accessor-ids))
		     (defn `(defun ,mk-struct ,accessor-ids
			      (,make-struct ,@(mapcan #'(lambda (a)
							  (list (intern (string a) :keyword)
								a))
						accessor-ids)))))
		(eval defs)
		(eval defn)
		(cons struct-id defs)))
    constrs))

(defun mk-newconstructor (id accessor-ids &optional (counter 0))
  (let* ((const-str (format nil "~a_~a" id counter))
	 (const-str? (find-symbol const-str))
	 (mk-str? (find-symbol (format nil "mk-~a" const-str)))
	 (rec-str? (find-symbol (format nil "~a?" const-str)))
	 (acc-strs? (loop for acc in accessor-ids
			 thereis
			 (find-symbol (format nil "~a_~a"
					const-str
					acc)))))
    (if (or const-str? mk-str? rec-str? acc-strs?)
	(mk-newconstructor id accessor-ids (1+ counter))
	(intern const-str :pvs))))

(defun co-constructor? (fn)
  (codatatype? (adt (adt fn))))

(defun pvs2cl-constructor (constructor struct all-structs datatype)
  (let ((decl (declaration constructor)))
    (or (and (eval-info decl)
	     (lisp-function decl))
      ;;above test duplicated in pvs2cl-resolution
      (cond ((enum-adt? datatype)
	     (let* ((pos (position (id constructor)
				   (constructors (adt datatype))
				   :test #'eq :key #'id))
		    (recognizer (recognizer constructor))
		    (rec-decl (declaration recognizer))
		    (rec-id (mk-newsymb (id recognizer)))
		    (rec-defn `(defun ,rec-id (x) (eql x ,pos))))
	       (unless (eval-info decl) (make-c-eval-info decl))
	       (setf (in-name decl) pos)
	       (unless (eval-info rec-decl)(make-c-eval-info rec-decl))
	       (setf (in-name rec-decl) rec-id)
	       (setf (definition (in-defn rec-decl))
		     rec-defn)
	       (eval rec-defn)
	       (compile rec-id)
	       pos))
	    (t (let* ((accessors (accessors constructor))
		      (struct-id (car struct))
		      (constructor-symbol (makesym "mk-~a" struct-id))
		      (defn (cdr struct))
		      (xvar (gentemp "x"))
		      )
;;		 (break "pvs2cl-constructor")
		 (unless (eval-info (declaration constructor))
		   (make-c-eval-info (declaration constructor)))
		 (setf (definition (in-defn-m (declaration constructor)))
		       defn)
		 (setf (in-name-m (declaration constructor))
		       constructor-symbol)
		 (when accessors
		   ;; Added :pvs to intern (Feb 20 2015) [CM]
		   (let* ((uname (intern 
				  (format nil "~a_~a" constructor-symbol "unary") :pvs))
			  (unary-binding (when accessors
					   (if (cdr accessors)
					       (loop for ac in accessors
						     as i from 0
						     collect (list (id ac)
								   `(svref ,xvar ,i)))
					     (list (list (id (car accessors)) xvar))))) ;; Let requires a list of list (March 17 2015) [CM] 
			  ;;NSH(2-4-2014): delay co-constructor arguments
			  (unary-form (when accessors
					`(lambda (,xvar) (let ,unary-binding
							   (,constructor-symbol
							    ,@(loop for ac in accessors
								    collect (id ac)))))))
			  (udefn (when accessors `(defun ,uname () ,unary-form))))
		     (setf (definition (in-defn (declaration constructor)))
			   udefn)
		     (eval udefn)
		     (setf (in-name (declaration constructor))
			   uname)))
		 (unless (eval-info (declaration (recognizer constructor)))
		   (make-c-eval-info (declaration (recognizer constructor))))
		 (setf (in-name (declaration (recognizer constructor)))
		       (makesym "~a?" struct-id))
		 (loop for x in accessors
		    do (unless (and (eval-info (declaration x))
				    (lisp-function (declaration x)))
			 (make-c-eval-info (declaration x)))
		    do (pvs2cl-accessor-defn*
			      (declaration x) constructor
			      struct-id all-structs
			      datatype))
		 ))))))

(defmethod pvs2cl-accessor-defn* ((acc adt-accessor-decl)
				 constructor
				 struct-id all-structs datatype)
  (declare (ignore all-structs datatype))
  (let ((acc-id (makesym "~a-~a" struct-id (id acc))))
    (cond ((co-constructor? constructor);co-accessor forces evaluation of delayed arg
	   (let* ((co-accessor (makesym "co-~a" acc-id))
		  (co-accessor-defn `(defun ,co-accessor (x)(force (,acc-id x)))))
	     (setf (in-name acc) co-accessor)
	     (setf (definition (in-defn acc)) co-accessor-defn)
	     (eval co-accessor-defn)
	     ))
	  (t (setf (in-name acc) acc-id)))))


(defmethod pvs2cl-accessor-defn* ((acc shared-adt-accessor-decl) constructor struct-id
				 all-structs datatype)
  (declare (ignore struct-id))
  (let* ((acc-id (makenewsym "~a-~a" (id datatype) (id acc)))
	 (var (gentemp))
	 (constr-structs (pairlis (constructors datatype) all-structs))
	 (defn `(defun ,acc-id (,var)
		  (typecase ,var
		    ,@(mapcar #'(lambda (cid)
				  (let* ((struct (cdr (assoc cid constr-structs
							    :key #'id)))
					 (access-expr (list (makesym "~a-~a"
							 (car struct) (id acc))
							    var))
					 (co-access-expr
					  (if (co-constructor? constructor)
					      `(force ,access-expr)
					    access-expr)))
				    (assert struct)
				    (list (car struct)
					  co-access-expr)))
			(constructors acc))))))
    (setf (in-name acc) acc-id);;NSH(2-9-14): Added the setfs which were missing
    (setf (definition (in-defn acc)) defn)
    (eval defn)
    acc-id))
    
(defparameter *pvs2cl-primitives*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'TRUE nil '|booleans|)
	(mk-name 'FALSE nil '|booleans|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name '=> nil '|booleans|)
	(mk-name '⇒ nil '|booleans|)	
	(mk-name '<=> nil '|booleans|)
	(mk-name '⇔ nil '|booleans|)	
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
	(mk-name '∧ nil '|booleans|)	
	(mk-name 'OR nil '|booleans|)
	(mk-name '∨ nil '|booleans|)	
 	(mk-name 'NOT nil '|booleans|)
	(mk-name '¬ nil '|booleans|)
	(mk-name 'WHEN nil '|booleans|)
	(mk-name 'IFF nil '|booleans|)
	(mk-name 'XOR nil '|xor_def|)	
	(mk-name '+ nil '|number_fields|)
	(mk-name '- nil '|number_fields|)
	(mk-name '* nil '|number_fields|)
	(mk-name '/ nil '|number_fields|)
	(mk-name '|number_field_pred| nil '|number_fields|)
	(mk-name '< nil '|reals|)
	(mk-name '<= nil '|reals|)
	(mk-name '> nil '|reals|)
	(mk-name '>= nil '|reals|)
	(mk-name '|real_pred| nil '|reals|)
	(mk-name '|integer_pred| nil '|integers|)
	(mk-name '|integer?| nil '|integers|)
	(mk-name '|rational_pred| nil '|rationals|)
	(mk-name '|floor| nil '|floor_ceil|)
	(mk-name '|ceiling| nil '|floor_ceil|)
	(mk-name '|nrem| nil '|modulo_arithmetic|)	
	(mk-name '|rem| nil '|modulo_arithmetic|)
	(mk-name '|ndiv| nil '|modulo_arithmetic|)
	(mk-name '|even?| nil '|integers|)
	(mk-name '|odd?| nil '|integers|)
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
	(mk-name '|char| nil '|character_adt|)
	(mk-name '|code| nil '|character_adt|)
	(mk-name '|char?| nil '|character_adt|)
	(mk-name '|u8xor| nil '|integer_bv_ops|)
	(mk-name '|u16xor| nil '|integer_bv_ops|)
	(mk-name '|u32xor| nil '|integer_bv_ops|)
	(mk-name '|u64xor| nil '|integer_bv_ops|)
	(mk-name '|u8and| nil '|integer_bv_ops|)
	(mk-name '|u16and| nil '|integer_bv_ops|)
	(mk-name '|u32and| nil '|integer_bv_ops|)
	(mk-name '|u64and| nil '|integer_bv_ops|)
	(mk-name '|u8or| nil '|integer_bv_ops|)
	(mk-name '|u16or| nil '|integer_bv_ops|)
	(mk-name '|u32or| nil '|integer_bv_ops|)
	(mk-name '|u64or| nil '|integer_bv_ops|)
	(mk-name '|u8not| nil '|integer_bv_ops|)
	(mk-name '|u16not| nil '|integer_bv_ops|)
	(mk-name '|u32not| nil '|integer_bv_ops|)
	(mk-name '|u64not| nil '|integer_bv_ops|)
	))

(defun same-primitive?  (n i)
  (and (same-id n i)
       (module-instance n)
       (eq (mod-id i)
	   (id (module-instance n)))))

;;Modified treatment of PVSio primitives (Feb 20 2015) [CM]
(defmethod pvs2cl-primitive? ((expr name-expr))
  (member expr *pvs2cl-primitives*
	  :test #'same-primitive?))

(defmethod pvs2cl-primitive? ((expr expr))
  nil)

;
; This is sound as we've already gone through pvs2cl-primitve?
; by this point.  However - should do it properly at some point
; as there could be overloading of the names of primitives (assuming
; different theory)
;
(defun pvs2cl-primitive (expr)
  (cond ((eq (id expr) 'TRUE) t)
	((eq (id expr) 'FALSE) nil)
	((eq (id expr) '|null|) nil)
	((and (eq (id expr) '-) ;;hard to distinguish unary,binary.
	      (tupletype? (domain (find-supertype (type expr)))))
	 (intern (format nil "pvs_--") :pvs))
	(t (intern (format nil "pvs_~a" (id expr)) :pvs))))

(defun pvs2cl-primitive2 (expr) ;;assuming expr is an id but not = or /=
  (let* ((id (id expr)))
    (intern (format nil "pvs__~a" id) :pvs)))

;;;
;;; this clearing is now done automatically by untypecheck
;;;

(defun my-unintern (name)
  (when (and name (symbolp name)) (unintern name)))

(defmethod clear-dependent-theories ((theory module))
  (clear-theory theory)
  (loop for (th . nil) in (all-usings theory)
	do (clear-theory th))
  (loop for (inst . nil) in (assuming-instances theory)
	do (clear-theory inst)))

(defmethod clear-dependent-theories (theory)
  (let ((module  (get-theory theory)))
    (when module
      (clear-dependent-theories module))))

(defmethod clear-theory ((theory module))
  (loop for dec in (theory theory)
	when (and (const-decl? dec)
		  (eval-info dec))
	do (remove-eval-info dec)))

(defmethod clear-theory (theory)
  (let ((module (get-theory theory)))
    (when module
      (clear-theory module))))

(defun write-defn (defn output)
  (when defn
    (cond (output 
	   (format output "~%")
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

(defun write-decl-symbol-table (dec output)
  (when (and (definition (in-defn dec))
	     (not (eq (id dec) (name (in-defn dec)))))
    (format output "~%;;;   ~a - ~a" (id dec) (name (in-defn dec))))
  (when (and (definition (in-defn-m dec))
	     (not (eq (makesym "_~a" (id dec)) (name (in-defn-m dec)))))
    (format output "~%;;;   ~a - ~a" (id dec) (name (in-defn-m dec))))
  (when (and (definition (in-defn-d dec))
	     (not (eq (makesym "~a!" (id dec)) (name (in-defn-d dec)))))
    (format output "~%;;;   ~a - ~a" (id dec) (name (in-defn-d dec))))
  (when (and (definition (ex-defn dec))
	     (not (eq (id dec) (name (ex-defn dec)))))
    ;(break "ex")
    (format output "~%;;;   ~a - ~a" (id dec) (name (ex-defn dec))))
  (when (and (definition (ex-defn-m dec))
	     (not (eq (id dec) (name (ex-defn-m dec)))))
    ;(break "ex-m")
    (format output "~%;;;   ~a - ~a" (id dec) (name (ex-defn-m dec))))
  (when (and (definition (ex-defn-d dec))
	     (not (eq (id dec) (name (ex-defn-d dec)))))
    ;(break "ex-d")
    (format output "~%;;;   ~a - ~a" (id dec) (name (ex-defn-d dec)))))

(defun print-lisp-defns (theory &optional file-string supersede?)
  (if file-string
      (with-open-file (output file-string :direction :output
			      :if-exists
			      (if supersede? :supersede :append)
			      :if-does-not-exist :create)
	(when supersede? (format output "(in-package :pvs)~%"))
	(print-lisp-defns-to-output (get-theory theory) output))
      (print-lisp-defns-to-output (get-theory theory) nil)))

(defun print-lisp-defns-to-output (theory output)
  (cond ((datatype? theory)
         (print-lisp-defns-to-output (adt-theory theory) output)
	 (when (adt-map-theory theory)
	   (print-lisp-defns-to-output (adt-map-theory theory) output))
	 (when (adt-reduce-theory theory)
	   (print-lisp-defns-to-output (adt-reduce-theory theory) output)))
	(t (loop for dec in (theory (get-theory theory))
	      when (and (const-decl? dec)(eval-info dec))
	      do (write-decl-defns dec output)))))

(defun pvs2cl-lisp-type (type)
  (pvs2cl-lisp-type* type))

(defmethod pvs2cl-lisp-type* ((type funtype))
  nil)

(defmethod pvs2cl-lisp-type* ((type tupletype))
  '(simple-array t))

(defmethod pvs2cl-lisp-type* ((type cotupletype))
  nil)

(defmethod pvs2cl-lisp-type*  ((type recordtype))
  (if (compatible? type *string-type*)
      'string
    '(simple-array t)))

(defun subrange-index (type)
  (let ((below (simple-below? type)))
    (if below
	(list 0 (if (number-expr? below)
		    (1- (number below))
		    '*))
	(let ((upto (simple-upto? type)))
	  (or (and upto (if (number-expr? upto)
			    (list 0 (number upto))
			    (list 0 '*)))
	      (let ((x (simple-subrange? type)))
		(when x
		  (let ((lo (if (number-expr? (car x))
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
  (when (and (subtype? type)
	     (tc-eq (type (predicate type))
		    (type (predicate (upto-subtype)))))
    (let* ((bindings (make-empty-bindings (free-params (upto-subtype))))
	   (subst (tc-match type (upto-subtype) bindings)))
      (cdr (assoc '|m| subst :test #'same-id)))))

(defun simple-below? (type)
  (when (and (subtype? type)
	     (tc-eq (type (predicate type))
		    (type (predicate (below-subtype)))))
    (let ((pred (predicate type)))      
      (if (and (lambda-expr? pred)(tc-eq (expression pred) *false*))
	  (make-number-expr 0)
	(let* ((bindings (make-empty-bindings (free-params (below-subtype))))
	       (subst (tc-match type (below-subtype) bindings)))
	  (cdr (assoc '|m| subst :test #'same-id)))))))

;;; Here we want to treat below and upto as special cases of subranges
;;; Note that from and to are not necessarily number-exprs.
(defun simple-subrange? (type)
  (let* ((bindings (make-empty-bindings (free-params (subrange-subtype))))
	 (subst (tc-match type (subrange-subtype) bindings))
	 (from (cdr (assoc '|m| subst :test #'same-id)))
	 (to (cdr (assoc '|n| subst :test #'same-id))))
    (if (and from to)
	(cons from to)
	(let ((upto (simple-upto? type)))
	  (if upto
	      (cons (mk-number-expr 0) upto)
	      (let ((below (simple-below? type)))
		(when below
		  (cons (make!-number-expr 0)
			(if (number-expr? below)
			    (make!-number-expr (1- (number below)))
			    (make!-difference below (make!-number-expr 1)))))))))))

(defun simple-above? (type)
  (let* ((bindings (make-empty-bindings (free-params (above-subtype))))
	 (subst (tc-match type (above-subtype) bindings)))
    (cdr (assoc '|m| subst :test #'same-id))))

(defun make-empty-bindings (formals)
  (mapcar #'list formals))

(defmethod pvs2cl-lisp-type* ((type subtype))
  (cond ((tc-eq type *naturalnumber*) '(integer 0))
	((tc-eq type *integer*) 'integer)
	((tc-eq type *int8*) 'fixnum)
	((tc-eq type *int16*) 'fixnum)
	((tc-eq type *int32*) 'fixnum)
	((tc-eq type *int64*) 'fixnum)
	((tc-eq type *uint8*) 'fixnum)
	((tc-eq type *uint16*) 'fixnum)
	((tc-eq type *uint32*) 'fixnum)
	((tc-eq type *uint64*) 'fixnum)
	((subtype-of? type *integer*)
	 (let ((sub (subrange-index type)))
	     (if sub
		 (if (and (integerp (car sub))
			  (integerp (cadr sub))
			  (>= (car sub)
			      most-negative-fixnum)
			  (<= (cadr sub)
			      most-positive-fixnum))
		     'fixnum
		     `(integer ,@sub))
		 (pvs2cl-lisp-type* (supertype type)))))
	(t (pvs2cl-lisp-type* (supertype type)))))

(defmethod pvs2cl-lisp-type* ((type adt-type-name))
  (cond ((eq (id type) '|list|)
	 'list)
	(t nil)))

(defmethod pvs2cl-lisp-type* ((type t))
  (cond ((tc-eq (find-supertype type) *boolean*) 'boolean)
	(t nil)));;was T, but need to distinguish proper types.

(defun sort-assignments (assignments)
  (sort (copy-list assignments)
	#'string-lessp
	:key #'(lambda (assignment) (id (caar (arguments assignment))))))
