;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                PVS to C translator
;;
;;     Author: Gaspard ferey
;;
;;  -> https://github.com/Gaspi/pvs2c.git
;;  -> Please read  "main.lisp"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Main functions :
;;
;;    pvs2C expr bindings livevars type
;;      -> C expression (Ccode) representing a C-var with the given type
;;         and with the translation of expr as a value.
;;
;;    pvs2C* expr bindings livevars
;;      -> As below but might have a "hole" (unnamed variable) for the result
;;
;;    pvs2C2 expr bindings livevars variable [need-malloc]
;;      -> Ccode with the necessary instructions to let the variable be set
;;         to a C expression representing expr.
;;         If need-malloc flag is t, also initializes the variable.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO flattennning free variables
;; f(a , a(0), a )
;; -> x = GC( a )
;; -> y = GC( a )
;; -> z = y(0)
;; -> free(y)     ;; to last line <=> apply(y, 0)   (so y is freed)
;; -> f(a, x ,z)    ;; and everything is freed
;;
;; Bump up the reference count by one so that no argument used as a mutable variable
;; Move the free to just after the last occurence of the variable
;;
;; Every Function has the responsability to free it's array arguments !!!
;;
;; 
;; (lambda(x): a with [0 = x(0) ]  )( a with [0=0] )
;; -> x = GC( a )
;; -> y = a with [0=0]    ;; done non destructively
;; -> 
;;
;;Page 5 Guarded Optim
;; frec doesn't terminate
;; and the C update doesn't seems to be done destructively
;;



(in-package :pvs)

(defvar *C-livevars-table* nil)
(defvar *C-record-defns* nil)

(defvar *C-definitions* nil)
(defun reset-definitions () (C-reset *C-definitions*))
(defun add-definition (definition)
  (setq *C-definitions* (append *C-definitions* (list definition))))


(defmacro pvs2C-error (msg &rest args)
  `(format t ,msg ,@args))


(defvar *C-nondestructive-hash* (make-hash-table :test #'eq))
(defvar *C-destructive-hash* (make-hash-table :test #'eq))

;; TODO stop using this structure. Rely only on the definition class
(defstruct C-info
  id type-out type-arg C-code definition analysis)

(defun C-hashtable (&optional (destr *destructive?*))
  (if destr *C-destructive-hash* *C-nondestructive-hash*))

(defun C_id (op)
  (let ((hashentry (gethash (declaration op) (C-hashtable))))
    (when hashentry (C-info-id hashentry))))

(defun C_nondestructive_id (op)
  (let ((hashentry (gethash (declaration op) *C-nondestructive-hash*)))
    (when hashentry (C-info-id hashentry))))

(defun C_analysis (op)
  (let ((hashentry (gethash (declaration op) (C-hashtable))))
    (when hashentry (C-info-analysis hashentry))))




;; Return a trivial C-expr if possible
(defun getConstantName (expr bindings)
  (when (name-expr? expr)
    (let ((e (assoc (declaration expr) bindings :key #'declaration)))
      (when e (mk-C-expr (pvs2C-type (type (car e))) (format nil "~a" (cdr e)))))))


;; Get the name of a variable according to the bindings
(defmethod getBindName ((l list) bindings)
  (when (consp l) (cons (getBindName (car l) bindings)
			(getBindName (cdr l) bindings))))
(defmethod getBindName ((bd bind-decl) bindings)
  (let ((e (assoc bd bindings :key #'declaration)))
    (getBindName (if e (cdr e) (id bd)) bindings)))
(defmethod getBindName ((n name-expr) bindings)
  (getBindName (declaration n) bindings))
(defmethod getBindName (e bindings) (format nil "~a" e))


  
;; Returns a C-expr
;; type is the list of the types
;; name is the list of the names
;; instr and destr are the concatenation of the instrs and destrs
(defmethod pvs2C ((expr list) bindings livevars &optional (exp-type (cons nil nil)))
  (if (consp expr)
      (let ((hd (pvs2C (car expr) bindings
		       (append (updateable-vars (cdr expr)) livevars)
		     (car exp-type)))
	    (tl (pvs2C (cdr expr) bindings
		       (append (updateable-vars (car expr)) livevars)
		       (cdr exp-type))))
	(C-expr (cons   (var   hd) (var   tl))
		(append (instr hd) (instr tl))
		(append (destr hd) (destr tl))))
    (C-expr nil )))



;; If expressions
;; Return a C-expr
;; type is given type
;; name is nil
;; destr contains the destructions for the condition
(defmethod pvs2C-if ((expr if-expr) bindings livevars exp-type)  ;result
  (let* ((condition (condition expr))
	 (then-part (then-part expr))
	 (else-part (else-part expr))
	 (newlivevars (append (updateable-vars then-part)
			      (append (updateable-vars else-part)
				      livevars)))
	 (C-cond-part (pvs2C condition bindings newlivevars *C-int*))
	 (C-then-part (pvs2C2 then-part bindings livevars (C-var exp-type)))
	 (C-else-part (pvs2C2 else-part bindings livevars (C-var exp-type))))
    (C-expr (C-var exp-type)
	    (append
	     (instr C-cond-part)
	     (list (Cif (var C-cond-part)
			(append (instr C-then-part)
				(destr C-then-part))
			(append (instr C-else-part)
				(destr C-else-part)))))
	    (destr C-cond-part))))

(defmethod pvs2C* ((expr if-expr) bindings livevars)
  (let* ((type (pvs2C-type expr)))    ;; TODO could be improved to find a smaller type
    (pvs2C-if expr bindings livevars type)))



;; --- Should be renamed "pvs2C-set" ---
;; Returns a C-expr with type = (type var)  and  name = (name var)
(defmethod pvs2C2 ((expr list) bindings livevars var &optional need-malloc)
  (if (consp expr)
      (let ((hd (pvs2C2 (car expr) bindings
			(append (updateable-vars (cdr expr)) livevars)
			(car var) need-malloc))
	    (tl (pvs2C2 (cdr expr) bindings
			(append (updateable-vars (car expr)) livevars)
			(cdr var) need-malloc)))
	(C-expr (cons   (var   hd) (var   tl))
		(append (instr hd) (instr tl))
		(append (destr hd) (destr tl))))
    (C-expr nil )))



;; Record expressions are treated seperately
(defmethod pvs2C2 ((expr record-expr) bindings livevars var &optional need-malloc)
  (C-expr var
	  (append (when need-malloc (list (Cdecl var)))
		  (list (Crecord-init var))
		  (loop for a in (assignments expr)
			collect (let ((e (assoc (id (caar (arguments a)))
						(args (type var)))))
				  (instr (pvs2C2 (expression a) bindings livevars
						 (Crecord-get var (car e)))))))
	  (append (list (Cfree var)))))

(defmethod pvs2C ((expr record-expr) bindings livevars &optional (exp-type (pvs2C-type expr)))
  (pvs2C2 expr bindings livevars (gen-C-var exp-type "aux") t))



(defmethod pvs2C2 (expr bindings livevars var &optional need-malloc)
  (let* ((type (type var))
	 (e (if (C-base? type)
		(pvs2C expr bindings livevars type)
	      (pvs2C* expr bindings livevars))))
    (if (and (unnamed? e) (type= (type e) type))  ;; Perfect case
	(progn (define-name e var)
	       (when need-malloc
		 (app-instr e (list (Cdecl var)) t)
		 (app-destr e (list (Cfree var))))
	       e)
      (progn (when (unnamed? e)
	       (let ((n (gen-C-var (type e) "set")))
		 (define-name e n)
		 (set-instr e (append (list (Cdecl n)) (instr e) (destr e)))
		 (set-destr e (list (Cfree n)))))
	     (set-var-to-C-expr var e need-malloc)))))

(defmethod set-var-to-C-expr ((var Ccode) (e C-expr) &optional need-malloc)
  (C-expr var
	  (append (instr e)
		  (when need-malloc (list (Cdecl var)))
		  (list (if (bang (type var)) (Ccopy var (var e))
			                      (Cset  var (var e))))
		  (destr e))
	  (when need-malloc (list (Cfree var)))))


;; Return a C-expr (  name : "if" name  ,  destr : only free(name)  )
(defmethod pvs2C ((expr if-expr) bindings livevars &optional (exp-type (pvs2C-type expr)))
  (let* ((type (smaller-type (pvs2C-type expr) exp-type))
	 (if-bloc (pvs2C-if expr bindings livevars type))
	 (if-name (gen-C-var type "if")))
    (define-name if-bloc if-name)
    (set-instr if-bloc
	       (append (list (Cdecl if-name))
		       (instr if-bloc)
		       (destr if-bloc)))
    (set-destr if-bloc (list (Cfree if-name)))
    (convert exp-type if-bloc)))


;; Returns a C-expr
;; name is a variable of a simple expression
;; destr is only  free(name)
(defmethod pvs2C (expr bindings livevars &optional (exp-type (pvs2C-type expr)))
  (let ((cst-C-expr (getConstantName expr bindings)))
    (if cst-C-expr (convert exp-type cst-C-expr)
      (let ((e (pvs2C* expr bindings livevars))) ;; Returns an instructions (ends with *)
	(if (unnamed? e)
	    (let ((n (gen-C-var (type e) "aux")))
	      (define-name e n)
	      (set-instr e (append (list (Cdecl n))
				   (instr e)
				   (destr e)))
	      (set-destr e (list (Cfree n)))
	      (convert exp-type e))
	  (convert exp-type e))))))

;; number-expr is a non negative integer
(defmethod pvs2C* ((expr number-expr) bindings livevars)
  (declare (ignore bindings livevars))
  (let ((type (pvs2C-type expr)))
    (if (C-gmp? type)
	(C-expr (C-var type)
		(list (Cfuncall-mp "mpz_set_str"
				   (list (C-var type)
					 (C-var nil (format "\"~a\"" (number expr))))
				   type)))
      (C-expr (C-var type (number expr) t)))))

;; UNSAFE !!
;; Should be modified to create a different tuple according to the type of the args
(defmethod pvs2C* ((expr tuple-expr) bindings livevars)
  (let* ((elts (exprs expr))
	 (args (pvs2C elts bindings livevars
		      (mapcar #'pvs2C-type elts))))
    (set-name args
	      (Cfuncall "createTuple" (cons "~a" args)))
    args))

;; UNSAFE !!
(defmethod pvs2C* ((expr projection-application) bindings livevars)
  (let* ((e  (pvs2C (argument expr) bindings livevars)))
    (set-var e (Crecord-get (var e) (format nil "f~a" (index expr))))
    e))

;; UNSAFE !! When is this function called ??  (not working)
(defmethod pvs2C* ((expr list) bindings livevars)
  (if (consp expr)
      (cons (pvs2C (car expr) bindings
		   (append (updateable-vars (cdr expr)) livevars) )
	    (pvs2C (cdr expr) bindings  ;;need car's freevars
		   (append (updateable-vars (car expr)) ;;f(A, A WITH ..)
			   livevars)))
    nil))

(defmethod pvs2C*  ((expr field-application) bindings livevars)
  (let* ((clarg (pvs2C (argument expr) bindings livevars))
	 (id (id expr)))
    (set-var clarg (Crecord-get (var clarg) id))
    clarg))


;; pairlis with three lists
(defun pair3lis (l1 l2 l3)
  (when (consp l1)
    (cons (list (car l1) (car l2) (car l3))
	  (pair3lis (cdr l1) (cdr l2) (cdr l3)))))

(defun mk-C-expr-funcall (type op args)
  (if (C-gmp? type)
      (progn   ;; Rq this should only be called with named arguments
	(app-instr args (set-C-pointer op (var args) type))
	(set-var args (C-var type)))
    (set-var args (Cfuncall op (var args) type)))
  args)

(defmethod pvs2C* ((expr application) bindings livevars)
  (with-slots (operator argument) expr
  (if (rem-application? operator) (pvs2C-remappli expr bindings livevars)
    (if (constant? operator)
	(if (pvs2cl-primitive? operator)
	    (pvs2C*-primitive-app expr bindings livevars)
	  (if (datatype-constant? operator)
	      (mk-C-expr-funcall (pvs2C-type (range (type operator)))
				 (pvs2C-resolution operator)
				 (pvs2C (arguments expr)
					bindings
					livevars))
	    (pvs2C-defn-application expr bindings livevars)))
      (if (lambda? operator)
	  (let* ((bind-decls (bindings operator))
		 (bind-ids (pvs2cl-make-bindings bind-decls bindings))
		 (newbind (append (pairlis bind-decls bind-ids) bindings))
		 (type-args (C-type-args operator))
		 (C-arg (pvs2C2 (if (tuple-expr? argument)
				    (exprs argument)
				  (list argument))
				newbind   ;; or bindings... ??
				(append (updateable-free-formal-vars operator) livevars)
				(C-var-list type-args (getBindName bind-decls newbind)) t))
		 ;;		 (deb (break))
		 (C-expr (pvs2C* (expression operator) newbind nil)))
	    (app-instr C-expr (instr C-arg) t)
	    (app-destr C-expr (destr C-arg))
	    C-expr)
	(let* ((type-op (pvs2C-type (type operator)))
	       (type (pvs2C-type (range (type operator))))
	       (C-arg (pvs2C argument bindings
			     (append (updateable-free-formal-vars operator) livevars)))
	       (C-op (pvs2C operator bindings
			    (append (updateable-vars argument) livevars)
			    type-op)))
	  (if (C-array? type-op) ;; if the operator is an array
	      (C-expr (Carray-get (var C-op) (var C-arg))
		      (append (instr C-op) (instr C-arg))
		      (append (destr C-op) (destr C-arg)))
	    (if (C-gmp? type)
		(C-expr (C-var type)
			(append (instr C-op) (instr C-arg)
				(set-C-pointer C-op C-arg type))
			(append (destr C-op) (destr C-arg)))
	      (C-expr (Cfuncall C-op C-arg type)
		      (append (instr C-op) (instr C-arg))
		      (append (destr C-op) (destr C-arg)))))))))))


(defun pvs2C-defn-application (expr bindings livevars)
  (with-slots (operator argument) expr
    (pvs2C-resolution operator)
    (let* ((actuals (expr-actuals (module-instance operator)))
	   (op-decl (declaration operator))
	   (args (arguments expr))
	   (type-args (C-type-args op-decl))
	   (ret-type (pvs2C-type (range (type op-decl))))
	   (C-args (pvs2C (append actuals args) bindings livevars type-args)))
      (if *destructive?*
	  (let* ((ret-type (pvs2C-type (range (type operator))))
		 (defns (def-axiom op-decl))
		 (defn (when defns (args2 (car (last (def-axiom op-decl))))))
		 (def-formals (when (lambda-expr? defn)
				(bindings defn)))
		 (module-formals (unless (eq (module op-decl) (current-theory))
				   (constant-formals (module op-decl))))
		 (alist (append (pairlis module-formals actuals)
				(when def-formals
				  (pairlis def-formals args))))
		 (analysis (C_analysis operator))
		 (check (check-output-vars analysis alist livevars))
		 (id-op (if check (C_id operator)
			          (C_nondestructive_id operator))))
	    (mk-C-expr-funcall ret-type (declaration operator) C-args))
	(mk-C-expr-funcall ret-type (declaration operator) C-args)))))
	;;     (mk-C-expr-funcall ret-type id-op C-args))
	;; (mk-C-expr-funcall ret-type (C_id operator) C-args)))))

(defun pvs2C-resolution (op)
  (let* ((op-decl (declaration op)))
    (pvs2C-declaration op-decl)))

(defun pvs2C-declaration (op-decl)
  (let ((nd-hashentry (gethash op-decl *C-nondestructive-hash*)))
					;enough to check one hash-table. 
    (when (null nd-hashentry)
      (let ((op-id   (gen-name (id op-decl) nil))
	    (op-d-id (gen-name (id op-decl) t  )))
	(setf (gethash op-decl *C-nondestructive-hash*)
	      (make-C-info :id op-id))
	(setf (gethash op-decl *C-destructive-hash*)
	      (make-C-info :id op-d-id))
	(let* ((defns (def-axiom op-decl))
	       (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	       (def-formals (when (lambda-expr? defn)
			      (bindings defn)))
	       (def-body (if (lambda-expr? defn) (expression defn) defn))
	       (module-formals (constant-formals (module op-decl)))
	       (range-type (if def-formals (range (type op-decl))
			     (type op-decl))))
	  (pvs2C-resolution-nondestructive op-decl (append module-formals def-formals)
					   def-body range-type)
	  (pvs2C-resolution-destructive op-decl (append module-formals def-formals)
					def-body range-type)
	  (when *C-analysis* (C-analysis op-decl)))))))

(defun pvs2C-resolution-destructive (op-decl formals body range-type)
  (let ((*destructive?* t))
    (pvs2C-resolution* op-decl formals body range-type)))
	 

(defun pvs2C-resolution-nondestructive (op-decl formals body range-type)
  (let ((*destructive?* nil))
    (pvs2C-resolution* op-decl formals body range-type)))


(defun pvs2C-resolution* (op-decl formals body range-type)
  (let* ((bind-ids (pvs2cl-make-bindings formals nil))
	 (id-map (pairlis formals bind-ids))
	 (C-type-out (pvs2C-type range-type))
	 (return-void (C-gmp? C-type-out))
	 (result-var (C-var C-type-out "result"))
	 (C-args (loop for var in formals
		       collect (C-var (pvs2C-type (type var))
				      (cdr (assoc var id-map)))))
	 (C-type-arg (append (when return-void (list (C-var C-type-out "result")))
			     C-args))
	 (hash-entry (gethash op-decl (C-hashtable)))
	 (C-body (pvs2C2 body id-map nil result-var (not return-void))))
             ;; If we don't return void, we need to malloc the result
    (when *destructive?*   ;; We only show one of the two defintions
      (debug (format nil "Defining ~a with type:  ~a -> ~a"
		     (id op-decl) (mapcar #'type C-type-arg) C-type-out)))
    (when *eval-verbose* (format t "~%as :~%~{~a~%~}" (instr C-body)))
    (setf (C-info-type-out hash-entry) (if return-void "void" C-type-out)
	  (C-info-type-arg hash-entry) C-type-arg
	  (C-info-C-code   hash-entry) C-body
	  (C-info-definition hash-entry)
	  (Cfun-decl (C-info-id hash-entry)
		     (C-info-type-out hash-entry)
	             (append (instr C-body)
			     (loop for a in C-args collect (Cfree a))
			     (if return-void (destr C-body)
			                     (list (Creturn result-var))))
		     C-type-arg))))

(defmethod pvs2C* ((expr name-expr) bindings livevars)
  (let* ((type-e (pvs2C-type expr))
	 (decl (declaration expr))
	 (bnd (assoc decl bindings :key #'declaration))
	 (prim (pvs2cl-primitive? expr)))
    (assert (not (and bnd (const-decl? decl))))
    (cond (bnd
	   (C-expr (C-var type-e (format nil "~a" (cdr bnd)))))
	  (prim (pvs2C*-primitive-cste expr bindings livevars))
	  ((const-decl? decl)
	        (pvs2C*-constant expr decl bindings livevars))
	  (t
	        (let ((undef (undefined expr "Hit untranslateable expression ~a")))
		  `(funcall ',undef))))))

(defun pvs2C*-constant (expr op-decl bindings livevars)
  (let* ((defns (def-axiom op-decl))
	 (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	 (def-formals (when (lambda-expr? defn)
			(bindings defn))))
    (pvs2C-resolution expr)
    (if def-formals  ;; I don't understand this...
	(let ((eta-expansion
	       (make!-lambda-expr def-formals
		    (make!-application* expr
		       (loop for bd in def-formals
			     collect (mk-name-expr bd))))))
	  (pvs2C* eta-expansion bindings livevars))
      (let ((actuals (expr-actuals (module-instance expr))))
;;	(break) ;;Is that where functions from theory are called ?
	(mk-C-expr-funcall (pvs2C-type (type expr))
			   (declaration expr)
;;			  (C_nondestructive_id expr)
			  (pvs2C actuals bindings livevars))))))


(defmethod pvs2C* ((expr lambda-expr) bindings livevars)
  (declare (ignore livevars))
  (with-slots (type expression) expr
    (let ((bind-decls (bindings expr)))
      (if (and (C-updateable? type) (funtype? type))  ;; If it could be represented as an array
	  (let* (
;;		 (Ctype (pvs2C-type type))        ;; Should we built Ctype from that ?
	         (Ctype (pvs2C-type expr))        ;; Should we built Ctype from that ?
		 (range-type (target Ctype))
		 (i (gen-C-var *C-int* "i"))
		 (new-bind (append (pairlis bind-decls ;; bind-decls must have length 1
					    (list i))
				   bindings))
		 (body (pvs2C2 expression
			       new-bind
			       nil  ;;removed livevars (why ?)
			       (Carray-get (C-var Ctype) i)
			       nil)))
	    (C-expr (C-var Ctype)
		       (list (Carray-init (C-var Ctype) (instr body) i))
		       (when (destr body)
			 (append
			  (list (format nil "for(~a = 0; ~a < ~a;;; ~a++) {"
					i i (array-bound type) i))
			  (indent (destr body))
			  (list "}")))))
	(pvs2C-lambda bind-decls expression bindings)))))

;; (list (format nil "if ( GC_count( ~~a ) == 1) {")
;;       (format nil "  ~~a = GC( ~a );" )
;;       ....


;; Creates a closure (not working now...)
(defun pvs2C-lambda (bind-decls expr bindings) ;;removed livevars
  (let* ((*destructive?* nil)
	 (range-type (pvs2C-type (type expr)))
	 (bind-ids (pvs2cl-make-bindings bind-decls bindings))
	 (new-bind (append (pairlis bind-decls bind-ids) bindings))
	 (C-body (pvs2C expr
			 new-bind
			 nil range-type))
	 (name (gentemp "fun"))
	 (fv (freevars expr))
	 (nb-args (length fv))
	 (var-ind (pairlis fv (range-arr nb-args)))
	 (param-def (append-lists (mapcar #'(lambda (x) (load-void x "env"))
					  var-ind)))
	 (assign (loop for x in bindings
		       when (assoc (cdr x) var-ind :test #'(lambda (x y) (eq x (id y)) ))
		       collect it))
	 ;; Carefull !! eq x (id y) is not enough, (duplication variable id)
	 (env-name (gentemp "env")))
    (add-definition (format nil "~a ~a(void *env) {~%~{  ~a~%~}  return ~a;~%}"
			    range-type name
			    (append param-def (instr C-body) (destr C-body))
			    (name C-body)))
    (C-expr (C-var (make-instance 'C-closure))
	    (append (list (format nil "void ~a[~d];" env-name nb-args))
		    (append-lists (mapcar
				   #'(lambda (x) (save-void x env-name))
				   assign))
		    (list (format nil "makeClosure(~~a, ~a, ~a);" name env-name)))
	    nil)))




;; Doesn't work  / Should look like if-expr
(defmethod pvs2C* ((expr cases-expr) bindings livevars)
  (let ((type (pvs2C-type expr)))
    (C-expr (C-var type)
	    (append (list (format nil "switch( ~a ) {"
				  (pvs2C (expression expr) bindings livevars type)))
		    (pvs2C-cases (selections expr) (else-part expr) bindings livevars)
		    (list "}")))))

;; Doesn't work / Should look like if-expr
(defun pvs2C-cases (selections else-part bindings livevars)
  (let ((selections-C
	 (loop for entry in selections
	       collect
	       (let* ((bind-decls (args entry))
		      (bind-ids (pvs2cl-make-bindings bind-decls bindings)))
		 (format nil "  case ~a ~{~a ~} : ~a; break;"
			 (pvs2C (constructor entry) bindings livevars)
			 bind-ids
			 (pvs2C (expression entry)
				(append (pairlis bind-decls bind-ids) bindings)
				livevars)))))))
  (append selections-C
	  (when else-part
	    (list (format nil "  default: ~a"
			  (pvs2C (expression else-part) bindings livevars))))
	  (list "}")))



(defmethod pvs2C* ((expr update-expr) bindings livevars)
  (with-slots (expression assignments) expr
  (if (C-updateable? (type expression))
      (progn
	(when (and *destructive?* (not (some #'maplet? assignments)))
	  (let* ((*C-livevars-table* 
		  (no-livevars? expression livevars assignments)))
	    ;;very unrefined: uses all freevars of eventually updated expression.
	    (cond (*C-livevars-table* ;; check-assign-types
		   (push-output-vars (car *C-livevars-table*)
				     (cdr *C-livevars-table*)))
		  ((and *eval-verbose* (not *C-livevars-table*))
		   (format t "~%Update ~s translated nondestructively. Live variables ~s present"
			   expr livevars)))))
	(pvs2C-update expr bindings livevars))
    (pvs2C (translate-update-to-if! expr) bindings livevars)))) ;; When does that happen ?



(defun pvs2C-update (expr bindings livevars)
  (with-slots (type expression assignments) expr
    (let* ((C-type (pvs2C-type type))
	   (assign-exprs (mapcar #'expression assignments))
	   (C-expr (pvs2C2 expression bindings
			   (append (updateable-free-formal-vars
				    assign-exprs) ;;assign-args can be ignored
				   livevars)
			   (bang-type (C-var C-type)) nil))
	   (C-update (pvs2C-update* (type expression)
				   (mapcar #'arguments assignments)
				   assign-exprs
				   bindings
				   (append (updateable-vars expression)
					   livevars))))
      (set-name C-expr nil) ;; type is already C-type
      (set-instr C-expr
		 (append (instr C-update)  ;; Defining update key / value
			 (instr C-expr)    ;; Getting an updateable array
			 (name C-update)   ;; Performing the update (destructively)
			 (destr C-update)));; Destroying
      C-expr)))  ;; destr of C-expr is still to be destroyed


;;recursion over updates in an update expression
(defun pvs2C-update* (type assign-args assign-exprs bindings livevars)
  (if (consp assign-args)
    (let ((C-car (pvs2C-update-nd-type
	        type "~a"
		(car assign-args )
		(car assign-exprs)
		bindings
		(append (append (updateable-vars (cdr assign-exprs))
				(updateable-vars (cdr assign-args ))
				livevars))))
	  (C-cdr (pvs2C-update* type
			   (cdr assign-args)
			   (cdr assign-exprs)
			   bindings
			   (append (updateable-free-formal-vars (car assign-exprs))
				   livevars))))
      (set-name  C-cdr (append (name  C-car) (name  C-cdr)))
      (app-instr C-cdr (instr C-car) t)
      (app-destr C-cdr (destr C-car) t)
      C-cdr)
    (C-expr (C-var nil))))


;; ---- Recursion over nested update arguments in a single update ----
(defun pvs2C-update-nd-type (type exprvar args assign-expr bindings livevars)
  (if (consp args)
      (pvs2C-update-nd-type* type exprvar (car args) (cdr args) assign-expr
			      bindings livevars)
    (break))) ;; Should never happen... (a priori)

(defmethod pvs2C-update-nd-type* ((type funtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((dom-type (pvs2C-type (domain type))) ;; Should really be int...
	 (arg1var (gen-C-var dom-type "L"))
	 (Ctype (pvs2C-type type))
	 (C-arg1 (pvs2C2 (car arg1) bindings
			 (append (updateable-vars restargs)
				 (updateable-free-formal-vars assign-expr)
				 livevars)
			 arg1var t)))
    (if (consp restargs)
	(let* ((C-range-type (pvs2C-type (range type)))
	       (exprvar (gen-C-var C-range-type "E"))
	       (C-expr (pvs2C-update-nd-type (range type) exprvar
					     restargs assign-expr
					     bindings livevars)))
	  (mk-C-expr nil
		     (append (list (Cdecl exprvar))
			     (list (Ccopy exprvar
					  (Carray-get (C-var Ctype expr) arg1var)))
			     (name C-expr)
			     (list (Cset (Carray-get (C-var Ctype expr) arg1var) exprvar)))
		    (append (instr C-expr) (instr C-arg1))
		    (append (destr C-arg1) (destr C-expr))))
      (let* ((res (gen-C-var (target Ctype) "res"))
	     (C-expr (pvs2C2 assign-expr bindings livevars res t)))
	(mk-C-expr nil
		   (list (Cset (Carray-get (C-var Ctype expr) arg1var) res))
		   (append (instr C-arg1) (instr C-expr))
		   (append (destr C-arg1) (destr C-expr)))))))


(defmethod pvs2C-update-nd-type* ((type recordtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (let* ((id (id (car arg1)))
	 (field-type (type (find id (fields type) :key #'id) ))
	 (C-type (pvs2C-type type))
	 (C-target-type (cdr (assoc id (args C-type)))))
    ;;	 (C-target-type (pvs2C-type field-type)))
    (if (consp restargs)
	(let ((exprvar (gen-C-var C-target-type "E"))
	      (C-expr (pvs2C-update-nd-type field-type exprvar restargs assign-expr
				      bindings livevars)))
	  (set-name C-expr
		    (append (list (Ccopy exprvar
					 (Crecord-get (C-var C-type expr) id)))
			    (name C-expr)
			    (list (Cset (Crecord-get (C-var C-type expr) id)
					exprvar))))
	  C-expr)
      (let* ((res (gen-C-var C-target-type "res"))
	     (C-expr (pvs2C2 assign-expr bindings livevars res t)))
	(mk-C-expr nil (list (Cset (Crecord-get (C-var C-type expr) id)
				    res))
		   (instr C-expr)
		   (destr C-expr))))))


(defmethod pvs2C-update-nd-type* ((type subtype) expr arg1 restargs
				   assign-expr bindings livevars)
  (pvs2C-update-nd-type* (find-supertype type) expr arg1 restargs
			  assign-expr bindings livevars))


;;C-updateable? is used to check if the type of an updated expression
;;is possibly destructively. 
(defmethod C-updateable? ((texpr tupletype))
  (C-updateable? (types texpr)))

;;this is the only case where C-updateable? can be false, because
;;the given function type is not an array.  
(defmethod C-updateable? ((texpr funtype)) ;;add enum types, subrange.
  (and (or (simple-below? (domain texpr))
	   (simple-upto? (domain texpr)))
       (C-updateable? (range texpr))))

(defmethod C-updateable? ((texpr recordtype))
  (C-updateable? (mapcar #'type (fields texpr))))

(defmethod C-updateable? ((texpr subtype))
  (C-updateable? (find-supertype texpr)))

(defmethod C-updateable? ((texpr list))
  (or (null texpr)
      (and (C-updateable? (car texpr))
	   (C-updateable? (cdr texpr)))))

(defmethod C-updateable? ((texpr t)) t)
;; It is okay to say  C-updateable? for uninterpreted
;; or actuals since these will not be updated destructively or otherwise.



(defun pvs2C-theory (theory)
  (let* ((theory (get-theory theory))
	 (*current-theory* theory)
	 (*current-context* (context theory)))
    (cond ((datatype? theory)
	   (pvs2C-datatype theory))
	   ;;(pvs2C-theory (adt-theory theory))
	   ;;(let ((map-theory (adt-map-theory theory))
	   ;;   (reduce-theory (adt-reduce-theory theory)))
	   ;;   (when map-theory (pvs2C-theory (adt-map-theory theory)))
	   ;;   (when reduce-theory (pvs2C-theory (adt-reduce-theory theory))))
	  (t (loop for decl in (theory theory)
		   do (cond ((type-eq-decl? decl)
			     (let ((dt (find-supertype (type-value decl))))
			       (when (adt-type-name? dt)
				 (pvs2C-constructors (constructors dt) dt))))
			    ((datatype? decl)
			     (let ((adt (adt-type-name decl)))
			       (pvs2C-constructors (constructors adt) adt)))
			    ((const-decl? decl)
			     (unless (eval-info decl)
			       (pvs2C-declaration decl)))
			    (t nil)))))))

;;; maps to AlgebraicTypeDef
(defun pvs2C-datatype (dt)
  (let* ((typevars (mapcar #'(lambda (fm)
			       (pvs2C-datatype-formal fm dt))
		     (formals dt)))
	 (constructors (pvs2C-constructors
			(constructors dt) dt
			(mapcar #'cons (formals dt) typevars))))
    (format nil "::~a~{ ~a~} = ~{~a~^ | ~}"
      (id dt) typevars constructors)))

(defun pvs2C-datatype-formal (formal dt)
  (if (formal-type-decl? formal)
      (let ((id-str (string (id formal))))
	(if (lower-case-p (char id-str 0))
	    (id formal)
	    (make-new-variable (string-downcase id-str :end 1) dt)))
      (break "What to do with constant formals?")))

(defun pvs2C-constructors (constrs datatype tvars)
  (pvs2C-constructors* constrs datatype tvars))

(defun pvs2C-constructors* (constrs datatype tvars)
  (when constrs
    (cons (pvs2C-constructor (car constrs) datatype tvars)
	  (pvs2C-constructors* (cdr constrs) datatype tvars))))

;;; Maps to ConstructorDef
(defun pvs2C-constructor (constr datatype tvars)
  (format nil "~a~{ ~a~}" (id constr)
	  (mapcar #'(lambda (arg) (pvs2C-type (type arg) tvars))
	    (arguments constr))))

(defun clear-C-hash ()
  (clrhash *C-nondestructive-hash*)
  (clrhash *C-destructive-hash*))

(defun generate-C-for-pvs-file (filename &optional force?)
  (setq *C-record-defns* nil)
  (clrhash *C-nondestructive-hash*)
  (clrhash *C-destructive-hash*)
  (let ((theories (cdr (gethash filename *pvs-files*))))
    (dolist (theory theories)
      (pvs2C-theory theory))  ;; Fill the hash-tables with definitions
    (with-open-file (outputH (format nil "~a.h" filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
    (with-open-file (output (format nil "~a.c" filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output "~{~a~%~}" (define-name (list
	    "// ---------------------------------------------"
	    "//        C file generated from ~a.pvs"
	    "// ---------------------------------------------"
	    "//   Make sure to link GC.c and GMP in compilation:"
	    "//      gcc -o ~a ~:*~a.c GC.c -lgmp"
	    "//      ./~a"
	    "// ---------------------------------------------"
	    "~%#include<stdio.h>"
	    "#include<gmp.h>"
	    "#include \"GC.h\""
	    "#include \"~a.h\""
	    "~%#define TRUE 1"
	    "#define FALSE 0"
	    "~%int main(void) {"
	    "  GC_start();"
	    "  printf(\"Executing ~a ...\\n\");"
	    "  // Insert code here"
	    "  GC_quit();"
	    "  return 0;~%}") filename))
      (format output "~{~2%~a~}" *C-definitions*)
      (format outputH "// C file generated from ~a.pvs" filename)
      (when *Crename-uli*
	(format outputH "~2%typedef unsigned long int uli;~%"))
      (dolist (rec-def *C-record-defns*)
	(format outputH "~a~%" (caddr rec-def)))
      (dolist (theory theories)
	(dolist (decl (theory theory))
	  (let ((ndes-info (gethash decl *C-nondestructive-hash*))
		(des-info (gethash decl *C-destructive-hash*)))
	    (when ndes-info
	      (let ((def (C-info-definition ndes-info)))
		;; First the signature
		(print-signature def outputH)
		;; Then the defn
		(print-definition def output)))
	    (when des-info
	      (let ((def (C-info-definition des-info)))
		;; First the signature
		(print-signature def outputH)
		;; Then the defn
		(print-definition def output))))))))))




;; TODO handle the duplication of variable
;; f(x:int) = let x = 2 in x
;; x#0 generated  -> x_0 for isntance


