;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-prove.lisp -- 
;; Author          : N. Shankar
;; Created On      : Fri Apr  3 22:08:11 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 03:38:54 1998
;; Update Count    : 5
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)


;;NSH(3/3/93)commented out conversion of boolean equality to iff overriding
;;corresponding code in process.
(defun needs-bool-lifting1 (exp)
  (cond
    ((atom exp) nil)
    ((memq (funsym exp) '(AND OR IMPLIES NOT IF IFF))
     (throw 'bool-lifting t))
    ;; the next case probably wont occur -  need to check ***
;NSH    ((and (eq (funsym exp) 'EQUAL) (eq (prtype (arg1 exp)) 'bool))
;     (throw 'bool-lifting t))
    ((eq (funsym exp) 'LAMBDA) nil)
    (t (loop for subexp in (argsof exp) thereis (needs-bool-lifting1 subexp)))))
 
;;need to load prmacros, process, 
;;arith, q.lisp, etc. 

(defvar needed-if* nil)
(defvar *bindings* nil)
(defvar *embeddedf* nil)


;;; Translate into list representation for passing to the prover.
;;; This also sets up the global variables typealist, and inserts
;;; subtype information.

;(defmethod translate-to-prove :around (obj)
;  (let ((hashed-value (gethash obj *translate-to-prove-hash*)))
;    (or hashed-value
;	(let ((result (call-next-method)))
;	  (unless (or *bound-variables* *bindings*)
;	    (setf (gethash obj *translate-to-prove-hash*) result))
;	  result))))

(defmethod translate-to-prove :around ((obj name-expr))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-prove-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-prove-hash*)
		    result)
	      result)))))

(defmethod translate-to-prove :around ((obj binding-expr))
	   (if (or *bound-variables* *bindings*)
	       (call-next-method)
	       (let ((hashed-value
		      (gethash obj *translate-to-prove-hash*)))
		 (or hashed-value
		     (let ((result (call-next-method)))
		       (setf (gethash obj *translate-to-prove-hash*)
			     result)
		       result)))))

(defun conjunction (list)
  (if (cdr list)
      (cons 'and list)
      list))

(defun disjunction (list)
  (if (cdr list)
      `(OR ,(car list) ,(disjunction (cdr list)))
      (car list)))

(defmethod translate-to-prove ((list list))
  (mapcar #'translate-to-prove list))

(defmethod translate-to-prove ((expr name-expr))
  (let* ((pos (position expr *bindings*	;(NSH:4-5-91)
			:test #'same-declaration))
	 (apos (position expr *bound-variables*
			 :test #'same-declaration))
	 (bpos (when apos (- (length *bound-variables*)
			     apos)))) 
    (cond ((and (null pos)(null bpos))
	   (unique-prover-name expr))
	  (bpos (let ((id (makesym "*~a_~a*" (id expr) bpos)))
		  (add-to-local-typealist id expr)
		  id)) ;;NSH(4.2.95) id was (id expr) and unsound!
	  ;;eg. proved (FORALL i, (j|j<i): (FORALL (j| j>i): j<i).
	  (t (let ((nvar (makesym "*~a*" (1+ pos))))
	       (list (get-subtype-coercion expr) nvar))))))

(defmethod translate-to-prove ((expr constructor-name-expr))
  (call-next-method (lift-adt expr)))

(defun unique-prover-name (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	 (if (and (constructor? expr)
		  (enum-adt? (find-supertype (type expr))))
	     (position (id expr)
		       (constructors (adt (find-supertype (type expr))))
		       :test #'eq :key #'id)
	     (let* ((norm-expr (normalize-name-expr-actuals expr))
		    (id-hash (gethash norm-expr *translate-id-hash*))
		    (newconst (or id-hash
				  (when (tc-eq expr *true*) '(TRUE))
				  (when (tc-eq expr *false*) '(FALSE))
				  (list (intern (concatenate 'string
						  (string (id expr))
						  "_"
						  (princ-to-string
						   (funcall
						    *translate-id-counter*))))))))
	       (unless id-hash
		 (setf (gethash norm-expr *translate-id-hash*)
		       newconst)
		 ;;(format t "~%adding ~a to typealist" (car newconst))
		 (add-to-typealist (car newconst) expr))
	       newconst)))
	(t (add-to-local-typealist (id expr) expr)
	   (id expr))))

(defmethod normalize-name-expr-actuals ((expr name-expr))
  (with-slots (resolutions) expr
    (lcopy expr
      'resolutions (normalize-name-expr-actuals resolutions))))

(defmethod normalize-name-expr-actuals ((list list))
  (let ((nlist (mapcar #'normalize-name-expr-actuals list)))
    (if (equal list nlist) list nlist))) 

(defmethod normalize-name-expr-actuals ((res resolution))
  (with-slots (module-instance) res
    (lcopy res
      'module-instance (normalize-name-expr-actuals module-instance))))

(defmethod normalize-name-expr-actuals ((mname modname))
  (with-slots (actuals) mname
    (lcopy mname
      'actuals (normalize-name-expr-actuals actuals))))

(defmethod normalize-name-expr-actuals ((act actual))
  (with-slots (expr type-value) act
    (if type-value
	act
	(lcopy act 'expr (pseudo-normalize expr)))))

(defun get-subtype-coercion (expr)
  (let ((stname (cdr (assoc (type expr) *subtype-names* :test #'tc-eq))))
    (or stname
	(let ((name (makesym "*~a*" (gentemp "subtype"))))
	  (push (cons (type expr) name) *subtype-names*)
	  name))))
	

(defun add-to-typealist (id expr &optional type)
  (let ((prtype (prover-type (or type (type expr) (car (ptypes expr))))))
    (unless (or (not prtype)
		;;(lambda-variable? expr)
		(assoc id typealist))
      (push (cons id prtype) typealist))))

(defun add-to-local-typealist (id expr &optional type)
  (let ((prtype (prover-type (or type (type expr) (car (ptypes expr))))))
    (unless (or (not prtype)
		;;(lambda-variable? expr)
		(assoc id *local-typealist*))
      (push (cons id prtype) *local-typealist*))))

(defmethod translate-to-prove ((expr number-expr))
  (number expr))

(defmethod translate-to-prove ((expr record-expr))
  ;; Creates a dummy record and an update of it.
  (let* ((dummy (get-rec-type-dummy-name (type expr)))
	 (decl (mk-const-decl dummy (type expr)))
	 (res (make-resolution decl (mod-name *current-context*)
			       (type expr)))
	 (name (mk-name dummy nil nil res))
	 (nexpr (mk-name-expr name nil nil nil 'CONSTANT)))
    (setf (type nexpr) (type expr))
    (translate-assignments (assignments expr)
			   ;nexpr ;NSH(3.16.97)
			   dummy
			   (type expr))))

(defun get-rec-type-dummy-name (rectype)
  (let ((rtype (find-supertype rectype)))
    (or (cdr (assoc rtype *rec-type-dummies* :test #'tc-eq))
	(let ((name (makesym "*~a*" (gentemp "%%dummy"))))
	  (push (cons rtype name) *rec-type-dummies*)
	  name))))

(defmethod translate-to-prove ((expr tuple-expr))
  (cons 'TUPCONS (translate-to-prove (exprs expr))))
	
;(defmethod translate-to-prove ((expr coercion))
;  (translate-to-prove (expression expr)))

(defmethod translate-to-prove ((expr if-expr))
  (if (eq (id (module-instance (resolution (operator expr))))
	  '|if_def|)
      (list 'IF
	    (translate-to-prove (condition expr))
	    (translate-to-prove (then-part expr))
	    (translate-to-prove (else-part expr)))
      (call-next-method)))

(defmethod translate-to-prove ((expr cases-expr))
  (let ((name (cdr (assoc expr *named-exprs* :test #'tc-eq))))
    (or name
	(let ((newid (gentemp)))
	  (add-to-typealist newid nil (type expr))
	  (push (list expr newid) *named-exprs*)
	  (list newid)))))

(defun translate-args (arguments expected)
  (if (eql (length expected)
	   (length arguments));;no tuple mismatch
      (translate-to-prove arguments)
      (if (and (singleton? expected)
	       (tupletype? expected))
	  `(tupcons ,(translate-to-prove arguments))
	  (if (singleton? arguments)
	      (if (tuple-expr? (car arguments))
		  (translate-to-prove
		   (exprs (car arguments)))
		  (if (tupletype?
		       (type (car arguments)))
		      (let ((translated-arg
			     (translate-to-prove
			      (car arguments))))
			(loop for I from 0
			      upto (1- (length expected))
			      collect
			      `(,(makesym "TUPSEL-~a"
					  (or (prover-type
					       (nth I (types (type
							      (car arguments)))))
					      ""))
				,I
				,translated-arg)))
		      (translate-to-prove arguments)))
	      (translate-to-prove arguments)))))

(defmethod translate-to-prove ((expr projection-application))
  (let ((arg (translate-to-prove (argument expr))))
    `(,(makesym "TUPSEL-~a" (or (prover-type (type expr)) ""))
      ,(1- (index expr)) ,arg)))

(defmethod translate-to-prove ((expr field-application))
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (sfields (sort-fields fields))
	   (pos (position id sfields
			  :test #'(lambda (x y) (eq x (id y))))))
      (list (make-apply-name (mk-funtype (type argument) type))
	    (translate-to-prove argument)
	    pos))))

;;NSH(5.17.94): Complicated code to deal with tuple mismatch
;;between domain of operator and arguments.
;; op(a1,..., an) if dom(type(op)) = [t1,...,tn] ==>
;;                      (op' (tupcons a1' .. an'))
;; op(a1), if dom(type(op)) = t1,...,tn ==> (op (tupsel 0 a1)...)

(defmethod translate-to-prove ((expr application))
  (with-slots (operator argument) expr
    (let* ((dom (domain-types (type operator)))
	   (args (translate-args (arguments expr) dom)))
      (cond ((interpreted? operator)
	     (let ((op (interpretation operator)))
	       (cond ((and (eq op 'difference) (singleton? args))
		      (cons 'minus args))
		     ((eq op '/=)
		      (list 'not (cons 'equal args)))
		     ((and (eq op 'NOT)
			   (consp (car args))
			   (eq (caar args) 'NOT))
		      (cadar args))
		     (t (cons op args)))))
	    ((and (or (not (typep operator 'name-expr))
		      (not (typep (declaration operator) 'const-decl)))
		  (function-non-functional? operator))
	     (add-to-typealist (id operator) operator (type expr))
	     (cons (id operator)
		   args))
	    (t (let ((op (translate-to-prove (lift-adt operator t))))
		 (cons (make-apply-name (type operator))
		       (cons (if (symbolp op)
				 (list op) op)
			     args))))))))

(defmethod lift-adt ((ex constructor-name-expr) &optional op?)
  (if op?
      (call-next-method)
      (let* ((adt (adt (adt ex)))
	     (constr (find (id ex) (constructors adt) :test #'eq :key #'id)))
	(if (arguments constr)
	    ex
	    (call-next-method ex t)))))

(defmethod lift-adt ((ex adt-name-expr) &optional op?)
  (if op?
      (let ((res (make-resolution (declaration ex)
		   (module-instance (resolution (find-supertype (adt ex)))))))
	(mk-name-expr (id ex) nil nil res 'constant))
      ex))

(defmethod lift-adt (ex &optional op?)
  ex)


(defmethod interpreted? (expr)
  (declare (ignore expr))
  nil)

(defmethod function-non-functional? (expr)
  (declare (ignore expr))
  nil)

(defmethod function-non-functional? ((expr name-expr))
  (and (typep (type expr) 'funtype)
       (not (typep (declaration expr) 'var-decl))
       (not (typep (range (type expr)) 'funtype)))
  nil)


(defmethod interpreted? ((expr name-expr))
  (member expr *interpreted-names*
	  :test #'(lambda (n i)
		    (and (eq (id n) (id i))
			 (module-instance n)
			 (eq (mod-id i)
			     (id (module-instance n)))))))

(defun interpretation (expr)
  (or (cdr (assoc (id expr) *interpretations*))
      (id expr)))

(defmethod operator ((expr lambda-expr))
  'lambda)

(defmethod operator ((expr forall-expr))
  'forall)

(defmethod operator ((expr exists-expr))
  'exists)

(defvar *integer-pred* nil)

(defvar *top-translate-to-prove-hash*
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))

(defun top-translate-to-prove (expr)
  (let ((*bindings* nil)
	(*generate-tccs* 'NONE))
    (cond ((hash-table-p *translate-to-prove-hash*)
	   (when *integer*
	     (setq *integer-pred* (translate-to-ground (predicate *integer*))))
	   (when *rational*
	     (setq *rational-pred*
		   (translate-to-ground (predicate *rational*))))
	   (when *real*
	     (setq *real-pred* (translate-to-ground (predicate *real*))))
	   (when *newdc*
	     (setf (dp::node-initial-type *integer-pred*) 'dp::integer-pred)
	     (setf (dp::node-initial-type *rational-pred*) 'dp::rational-pred)
	     (setf (dp::node-initial-type *real-pred*) 'dp::real-pred))
	   (translate-to-ground (unit-derecognize expr)))
	  (t (translate-with-new-hash
	       (unless *integer-pred*
		 (setq *integer-pred*
		       (when *integer*
			 (translate-to-ground (predicate *integer*)))))
	       (translate-to-ground (unit-derecognize expr)))))))

(defmethod translate-to-prove ((expr binding-expr))
  (let* ((*bindings* (append (bindings expr) *bindings*))
	 (binding-vars (mapcar #'make-variable-expr (bindings expr)))
	 (type (type expr))
	 (prtype (prover-type type))
	 (tr-bndvars (translate-to-prove binding-vars))
	 ;;bound variables can't be ignored; subtyp constraints needed
	 (tr-freevars (translate-to-prove (freevars expr)))
	 ;;freevars of the expr are needed for connectives.
	 (tr-vars (append tr-bndvars tr-freevars))
	 (connective? (connective-occurs? (expression expr)))
	 (tr-expr
	  (if connective?
	      (let ((name (cdr (assoc expr *named-exprs* :test #'tc-eq))))
		(or name;;NSH(11.4.95): Fixes unsoundness caused
		    ;;by translating (LAMBDA : IF B ...) to (LAMBDA 1: CC)
		    ;;by generating (LAMBDA 1 : CC(*1*)) instead.
		    ;;Note that two tc-eq such LAMBDAs will generate
		    ;;different translations so that ASSERT might miss some
		    ;;syntactic equalities when there are IFs in LAMBDAs.
		    (let* ((newid (gentemp))
				 
					;(freevars (freevars (expression expr)))
			   )
		      ;;NSH(2.5.96) (freevars (expr..)) instead of
		      ;;(freevars expr).
		      (cond
		       (*bindings*
			(let* ((apname (makesym "APPLY-~d-~a"
						(length tr-vars)
						(or prtype "")))
			       (apform (cons apname (cons newid tr-vars))))
			  (unless (or (null prtype)
				      (assoc apname typealist))
			    (push (cons apname prtype) typealist)
			    (push apname applysymlist))
			  (push (cons expr apform) *named-exprs*)
			  apform))
		       (t 
			(add-to-typealist newid nil (type expr))
			(push (list expr newid) *named-exprs*)
			(list newid))))))
	      (translate-to-prove (expression expr))))
	 (tr-lambda-expr
	  (cons 'LAMBDA
		(cons (length (bindings expr))
		      (if (or connective? (null tr-freevars))
			  (list tr-expr)
			  (list tr-expr
				(list
				 (makesym "APPLY-~d-" (length tr-bndvars))
				 (operator expr)
				 tr-bndvars)))))))
    (if (lambda-expr? expr)
	tr-lambda-expr
	 (list (makesym "APPLY-~d-~a" 1 (or prtype ""))
	       (operator expr)
	       tr-lambda-expr))))

;;; Update expressions
;;; Translate expressions of the form
;;; A WITH [ (0) := 1 ],
;;;    where A is an array of type int->int, into
;;; (APPLY int ARRAYSTORE A 0 1)
;;;
;;; f WITH [ (0,0) := 0],
;;;    where f is a function of type int,int->int into
;;; (APPLY int UPDATE f (0 0) 0)
;;;
;;; g WITH [ (0) := h, (1) (x,y) := 0, (1) (x,y)' := 1 ]
;;;    where g and h are functions of type
;;;    T = [function[int -> function[state[T0],state[T0] -> int]]
;;;
;;; This generates the form
;;;
;;; (APPLY function[state[T0],state[T0] -> int]
;;;        UPDATE
;;;        (APPLY function[state[T0],state[T0] -> int]
;;;               UPDATE
;;;               (APPLY function[state[T0],state[T0] -> int]
;;;                      UPDATE
;;;                      g (0) h)
;;;               (1) (APPLY int UPDATE g(1) (x y) 0))
;;;        (1) (APPLY int UPDATE g(1) (x' y') 1))

(defmethod translate-to-prove ((expr update-expr))
  (translate-assignments (assignments expr)
			 ;(expression expr)
			 (translate-to-prove (expression expr))
			 (type expr)))

(defun translate-assignments (assigns ;basis
			      trbasis type)
  (if assigns
      (translate-assignments
       (cdr assigns)
       ;basis
       (translate-assignment (car assigns) ;basis
			     trbasis type)
       type)
      trbasis))

(defun translate-assignment (assign ;basis
			     trbasis type)
  (translate-assign-args (arguments assign)
			 (expression assign)
			 ;basis
			 trbasis
			 (find-supertype type)))

(defun translate-assign-args (args value ;basis
				   trbasis type)
  (if args
      (list 'UPDATE
	    trbasis
	    (typecase type
	      (recordtype
	       (position (id (caar args)) (sort-fields (fields type))
			 :test #'eq :key #'id))
	      (tupletype
	       (1- (number (caar args))))
	      (t (if (singleton? (car args))
		     (translate-to-prove (caar args))
		     (cons 'tupcons (translate-to-prove (car args))))))
	    (let* ((ntrbasis-type
		    (find-supertype 
		     (typecase type
		       (recordtype
			(type (find (id (caar args)) (fields type)
				    :test #'eq :key #'id)))
		       (tupletype
			(nth (1- (number (caar args)))
				(types type)))
		       (t (range type)))))
		    (ntrbasis
		     (typecase type
		       (recordtype
			(make-tr-field-application
			 (mk-funtype type ntrbasis-type)
			 (position (id (caar args))
				   (sort-fields (fields type))
				   :test #'eq :key #'id)
			 trbasis))
		       (tupletype
			(make-tr-projection-application
			 ntrbasis-type (number (caar args)) trbasis))
		       (t (make-tr-assign-application
			   type
			   trbasis
			   (if (singleton? (car args))
			       (translate-to-prove (caar args))
			       (cons 'TUPCONS (translate-to-prove (car args)))))))))
	      (translate-assign-args (cdr args)
				     value
				     ;nbasis
				     ntrbasis
				     ntrbasis-type)))
      (translate-to-prove value)))

(defun make-tr-field-application (field-accessor-type fieldnum tr-expr)
  (list (make-apply-name field-accessor-type)
	tr-expr fieldnum))

(defun make-tr-projection-application (type number expr)
  `(,(makesym "TUPSEL-~a" (or (prover-type type) ""))
      ,(1- number) ,expr))

(defun make-tr-assign-application (fun-type expr args)
  (list (make-apply-name fun-type)
	expr args))

(defun mk-assign-application (op args)
  (let ((appl (mk-application* op args)))
    (setf (type appl) (range (type op)))
    appl))

(defmethod make-apply-name ((te type-expr))
  (let* ((type (find-supertype te))
	 (name (makesym "APPLY-~d-~a"
			(length (domain-types type))
			(or (prover-type (range type)) "")))
	 (prtype (prover-type (range type))))
    (unless (or (not prtype)
		(assoc name typealist))
      (push (cons name prtype) typealist)
      (push name applysymlist))
    name))

(defmethod prover-type ((type type-expr))
  (cond ((tc-eq type *boolean*) 'bool)
	((member type `(,*integer* ,*naturalnumber*) :test #'tc-eq) 'integer)
	((tc-eq type *number*) 'number)
;;	((typep type 'tupletype) 'tuple)
;;	((typep type 'arraytype) 'array)
	((typep type 'funtype) 'functional)
	((typep type 'recordtype) 'array ;'tuple
	 )
	((typep type 'subtype)
	 (prover-type (supertype type)))
	;;((print-name type))
	;;((slot-exists-p (name type) 'id) (id (name type)))
	;;(error "No name available to use in prover-type")
	))

(defmethod prover-type ((te dep-binding))
  (prover-type (type te)))


(defparameter *interpreted-names*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name 'AND nil '|booleans|)
	(mk-name 'OR nil '|booleans|)
	(mk-name 'NOT nil '|booleans|)
	(mk-name '+ nil '|reals|)
	(mk-name '- nil '|reals|)
	(mk-name '* nil '|reals|)
	(mk-name '/ nil '|reals|)
	(mk-name '< nil '|reals|)
	(mk-name '<= nil '|reals|)
	(mk-name '> nil '|reals|)
	(mk-name '>= nil '|reals|)
;	(mk-name '+ nil '|reals|)
;	(mk-name '- nil '|reals|)
;	(mk-name '* nil '|reals|)
;	(mk-name '/ nil '|reals|)
;	(mk-name '+ nil '|rationals|)
;	(mk-name '- nil '|rationals|)
;	(mk-name '* nil '|rationals|)
;	(mk-name '/ nil '|rationals|)
;	(mk-name '+ nil '|integers|)
;	(mk-name '- nil '|integers|)
;	(mk-name '* nil '|integers|)
;	(mk-name '/ nil '|integers|)
;	(mk-name '+ nil '|naturalnumbers|)
;	(mk-name '- nil '|naturalnumbers|)
;	(mk-name '* nil '|naturalnumbers|)
;	(mk-name '/ nil '|naturalnumbers|)
	))

(defparameter *interpretations*
  '((= . equal)
    (< . lessp)
    (<= . lesseqp)
    (> . greaterp)
    (>= . greatereqp)
    (+ . plus)
    (- . difference)
    (* . times)
    (/ . divide)))
