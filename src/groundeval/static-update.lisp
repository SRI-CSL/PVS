(in-package 'pvs)

;;The operations in this file are meant to address two concerns:
;;1. What variables in an expression might possibly be destructively updated?
;;2. What variables are live when these variables are destructively updated.

;;For (1), it is sufficient to look at the free variables in the expression
;;that are destructively updateable.
;;For (2), it is sufficient to consider the possibly destructively
;;updateable free variables in the still active expressions.

;;updateable? ensures that a type is definitely destructively
;;updateable, wherever it is updateable.  In contrast, the
;;method contains-updateable? is true whenever the type has
;;any destructively updateable content.  Destructive updates
;;can only be safely invoked when all the content is updateable
;;content is destructively updateable since it would otherwise
;;be necessary to analyze the actual updates to make sure that
;;they all correspond to destructively updateable types.  This is
;;worth considering in a future extension, but it is improbable that
;;there'll be examples where some fields are destructively updateable
;;and others are only nondestructively updateable.

;;updateable? is used to check if the type of an updated expression
;;is possibly destructively. 

(defmethod updateable? ((texpr tupletype))
  (updateable? (types texpr)))

;;this is the only case where updateable? can be false, because
;;the given function type is not an array.  
(defmethod updateable? ((texpr funtype)) 
  (and (or (below? (domain texpr))(upto? (domain texpr)))
       (updateable? (range texpr))))


;  (let ((lo-hi (sub-range? (domain texpr))))
;    (and lo-hi
;	 (zerop (car lo-hi))
;	 (<= (cdr lo-hi) *eval-array-bound*)))


(defmethod updateable? ((texpr recordtype))
  (updateable? (mapcar #'type (fields texpr))))

(defmethod updateable? ((texpr subtype))
  (updateable? (find-supertype texpr)))

;(defmethod updateable? ((texpr adt-type-name))
;  (some #'(lambda (constr)
;	    (and (funtype? (type constr))
;		 (loop for ty in (types (domain (type constr)))
;		       
;		       thereis (and (not (tc-eq (find-supertype ty)
;					texpr))
;				    (updateable? ty)))))
;	(constructors texpr)))


(defmethod updateable? ((texpr list))
  (or (null texpr)
      (and (updateable? (car texpr))
	   (updateable? (cdr texpr)))))

;;This is subsumed by fall-through case.
;(defmethod updateable? ((texpr type-name))
;  (not (or (eq texpr *boolean*)
;	   (eq texpr *number*))))



;(defmethod updateable? ((texpr actual))
;  (updateable? (type-value texpr)))

(defmethod updateable? ((texpr T))
  T) ;;It is okay to say  updateable? for uninterpreted
;;or actuals since these will not be updated destructively or otherwise.

;;For use in pvs2cl-update* (recordtypes)
(defmethod top-updateable-types ((type tupletype) accum)
  (top-updateable-types (types type) (cons type accum)))

(defmethod top-updateable-types ((type recordtype) accum)
  (top-updateable-types (mapcar #'type (fields type))
			(cons type accum)))

(defmethod top-updateable-types ((type subtype) accum)
  (top-updateable-types (find-supertype type) accum))

(defmethod top-updateable-types ((type list) accum)
  (if (consp type)
      (top-updateable-types (cdr type)
			    (top-updateable-types (car type) accum))
      accum))

(defmethod top-updateable-types ((type funtype) accum)
  (if (or (below? (domain type))(upto? (domain type)))
      (cons type accum) ;;no need to go into range since 
      accum))         ;;dest. updates stop with funtypes.

(defmethod top-updateable-types ((type T) accum)
  accum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod contains-updateable? ((texpr tupletype))
  T)

(defmethod contains-updateable? ((texpr funtype))
  (or (below? (domain texpr))(upto? (domain texpr))
       (contains-updateable? (range texpr))))

;  (let ((lo-hi (sub-range? (domain texpr))))
;    (and lo-hi
;	 (zerop (car lo-hi))
;	 (<= (cdr lo-hi) *eval-array-bound*)))


(defmethod contains-updateable? ((texpr recordtype))
  T)

(defmethod contains-updateable? ((texpr subtype))
  (contains-updateable? (find-supertype texpr)))

(defmethod contains-updateable? ((texpr adt-type-name))
  (some #'(lambda (constr)
	    (and (funtype? (type constr))
		 (loop for ty in (types (domain (type constr)))
		       
		       thereis (and (not (tc-eq (find-supertype ty)
					texpr))
				    (contains-updateable? ty)))))
	(constructors texpr)))


(defmethod contains-updateable? ((texpr list))
  (when (consp texpr)
    (or (contains-updateable? (car texpr))
	(contains-updateable? (cdr texpr)))))

;;This is subsumed by fall-through case.
;(defmethod contains-updateable? ((texpr type-name))
;  (not (or (eq texpr *boolean*)
;	   (eq texpr *number*))))



(defmethod contains-updateable? ((texpr actual))
  (contains-updateable? (type-value texpr)))

(defmethod contains-updateable? ((texpr T))
  NIL) ;;It is okay to say not updateable? for uninterpreted
;;since these cannot be updated if nothing is known
;;about their type.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod contains-possibly-updateable? ((texpr tupletype))
  T)

(defmethod contains-possibly-updateable? ((texpr funtype))
  (or (below? (domain texpr))(upto? (domain texpr))
       (contains-possibly-updateable? (range texpr))))


(defmethod contains-possibly-updateable? ((texpr recordtype))
  T)

(defmethod contains-possibly-updateable? ((texpr subtype))
  (contains-possibly-updateable? (find-supertype texpr)))

(defmethod contains-possibly-updateable? ((texpr adt-type-name))
  (some #'(lambda (constr)
	    (and (funtype? (type constr))
		 (let* ((dom (domain (type constr)))
			(types (if (tupletype? dom)
				 (types dom)
				 (list dom))))
		 (loop for ty in types
		       thereis (and (not (tc-eq (find-supertype ty)
					texpr))
				    (contains-possibly-updateable? ty))))))
	(constructors texpr)))


(defmethod contains-possibly-updateable? ((texpr list))
  (when (consp texpr)
    (or (contains-possibly-updateable? (car texpr))
	(contains-possibly-updateable? (cdr texpr)))))

;;This is subsumed by fall-through case.
;(defmethod contains-possibly-updateable? ((texpr type-name))
;  (not (or (eq texpr *boolean*)
;	   (eq texpr *number*))))



(defmethod contains-possibly-updateable? ((texpr actual))
  (contains-possibly-updateable? (type-value texpr)))

(defmethod contains-possibly-updateable? ((texpr type-name))
  (formal-type-decl? (declaration texpr)))
;;In the context, formal type parameters can later become
;;updateable.  

(defmethod contains-possibly-updateable? ((texpr T))
  NIL) ;;It is okay to say not updateable? for uninterpreted
;;since these cannot be updated if nothing is known
;;about their type.  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;free-lambda-vars is unused and should be deleted.
;(defmethod free-lambda-vars ((expr application))
;  (with-slots (operator argument)
;      expr
;    (union (free-lambda-vars operator)
;	   (free-lambda-vars argument))))

;(defmethod free-lambda-vars ((expr lambda-expr))
;  (free-formal-vars expr))

;(defmethod free-lambda-vars ((expr binding-expr))
;  (set-difference (free-lambda-vars (expression expr))
;		  (bindings expr)
;		  :test #'same-declaration))

;(defmethod free-lambda-vars ((expr projection-expr))
;  (free-lambda-vars (argument expr)))

;(defmethod free-lambda-vars ((expr field-application))
;  (free-lambda-vars (argument expr)))

;(defmethod free-lambda-vars ((expr list))
;  (when (consp expr)
;    (union (free-lambda-vars (car expr))
;	   (free-lambda-vars (cdr expr)))))

;(defmethod free-lambda-vars ((expr tuple-expr))
;  (free-lambda-vars (exprs expr)))

;(defmethod free-lambda-vars ((expr record-expr))
;  (free-lambda-vars (assignments expr)))

;(defmethod free-lambda-vars ((expr update-expr))
;  (union (free-lambda-vars (expression expr))
;	 (free-lambda-vars (assignments expr))))

;(defmethod free-lambda-vars ((expr assignment))
;  (free-lambda-vars (expression expr)))

;(defmethod free-lambda-vars ((expr actual))
;  (unless (type-value expr)
;    (free-lambda-vars (expr expr))))

;(defmethod free-lambda-vars ((expr T))
;  NIL)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defmethod output-vars* ((expr name-expr))
;  (cond ((variable? expr) expr)
;	((pvs2cl-primitive? expr)
;	 (output-prim-vars expr))
;	((datatype-constant? expr)
;	 (if (recognizer? expr)
;	     nil
;	     (loop for i from 0 to (1- (arity expr))
;		   collect i)))
;	((defined-constant? expr)
;	 (


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;free-formal-vars returns the free variables, and when external, 
;;the constant theory parameters. 
(defun free-formal-vars (expr)
  (if *external*
      (let* ((params (free-params expr))
	     (const-params (loop for prm in params
				 when (constant? prm)
				 collect prm))
	     )
	(append const-params (freevars expr)))
      (freevars expr)))

;;updateable-free-formal-vars returns the possibly updateable
;;variables in a term that has a possibly updateable type.
;;This is used to indicate the still live variables in an
;;already evaluated expressions as with f in f(a), when translating
;; the expression a.
(defmethod updateable-free-formal-vars ((expr list))
  (loop for ex in expr nconc (updateable-free-formal-vars ex)))

(defmethod updateable-free-formal-vars ((expr T))
  (when (contains-possibly-updateable? (type expr))
    (updateable-vars expr)))  ;was free-formal-vars

(defmethod updateable-output-vars ((expr list))
  (loop for ex in expr nconc (updateable-output-vars ex)))

(defmethod updateable-output-vars ((expr T))
  (when (contains-updateable? (type expr))
    (updateable-outputs expr)))
			     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This is the list of possibly updateable free formals. 
(defun updateable-vars (expr)
  (loop for var in (free-formal-vars expr)
	when (contains-possibly-updateable? (type var))
	collect var))

(defun updateable-outputs (expr)
  (loop for var in (free-formal-vars expr)
	when (contains-updateable? (type var))
	collect var))
