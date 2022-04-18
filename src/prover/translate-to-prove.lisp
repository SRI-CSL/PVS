;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-prove.lisp -- 
;; Author          : N. Shankar
;; Created On      : Fri Apr  3 22:08:11 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 22:23:25 2004
;; Update Count    : 6
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;NSH(3/3/93)commented out conversion of boolean equality to iff overriding
;;corresponding code in process.
;;SO (2002-07-31) Made this change to needs-bool-lifting1 in porcess.lisp
;;                to keep from having duplicate functions.

 
;;need to load prmacros, process, 
;;arith, q.lisp, etc. 

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

;;NSH(7/8/13): added  caching when obj has no free variables so that
;;expressions in bound context can have their translation reused if they
;;have no free variables. 
(defmethod translate-to-prove :around ((obj name-expr))
  (if (and (freevars obj) (or *bound-variables* *bindings*))
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-prove-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-prove-hash*)
		    result)
	      result)))))

(defmethod translate-to-prove :around ((obj binding-expr))
  (if (and (freevars obj) (or *bound-variables* *bindings*))
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
      `(or ,(car list) ,(disjunction (cdr list)))
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
	  (bpos (let ((id (intern
			   (concatenate 'string
			     "*" (string (id expr)) "_"
			     (princ-to-string bpos) "*") :pvs)
			  ;;(makesym "*~a_~a*" (id expr) bpos)
			  ))
		  (add-to-local-typealist id expr)
		  id)) ;;NSH(4.2.95) id was (id expr) and unsound!
	  ;;eg. proved (FORALL i, (j|j<i): (FORALL (j| j>i): j<i).
	  (t (let* ((nvar (intern (concatenate 'string
					       "*" (princ-to-string (1+ pos)) "*") :pvs)
			  ;;(makesym "*~a*" (1+ pos))
			 
			  )
		    (expr-type (type expr))
		    (vars (intersection (freevars expr-type)
					*bindings*
					:test #'same-declaration))
		    (tr-vars (translate-to-prove vars))
				   
		    (st-name (get-subtype-coercion expr-type))
		    (st-expr (cons st-name (cons nvar tr-vars))))
	       st-expr)))))

(defmethod translate-to-prove ((expr constructor-name-expr))
  (call-next-method (lift-adt expr)))

(defun unique-prover-name (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	 (cond ((and (constructor? expr)
		     (enum-adt? (find-supertype (type expr))))
		(position (id expr)
			  (constructors (adt (find-supertype (type expr))))
			  :test #'eq :key #'id))
	       ((tc-eq expr *true*) '(true))
	       ((tc-eq expr *false*) '(false))
	       (t (let* ((norm-expr (normalize-name-expr-actuals expr))
			 (id-hash (gethash norm-expr *translate-id-hash*))
			 (newconst (or id-hash
				       (list
					(makesym "~a_~d"
						 (if (integerp (id expr))
						     (format nil "~r" (id expr))
						     (id expr))
						 (funcall *translate-id-counter*)))))
			 (expr-freevars (freevars expr))
			 (expr-bindings (intersection expr-freevars *bindings*
						      :test #'same-declaration))
			 )
		    (unless id-hash
		      (setf (gethash norm-expr *translate-id-hash*)
			    newconst)
		      ;;(format t "~%adding ~a to typealist" (car newconst))
					;(add-to-typealist (car newconst) expr)
		      )
		    (cond (expr-bindings
			   (let* ((tr-vars (translate-to-prove expr-bindings))
				  (prtype (prover-type (or (type expr) (car (ptypes expr)))))
				  (apname (make-apply-symbol (length tr-vars) prtype)))
			     (cons apname (cons newconst tr-vars))))
			  (t (add-to-typealist (car newconst) expr)
			     newconst))))))
	(t (add-to-local-typealist (id expr) expr)
	   (id expr))))

;;; normalize-name-expr-actuals goes down the actuals, pseudo-normalizing
;;; any exprs found inside the actuals - ignores type-values.

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

(defun get-subtype-coercion (expr-type)
  (let* ((name (cdr (assoc expr-type *subtype-names* :test #'tc-eq))))
    (or name
	(let* ((name (intern (concatenate 'string
					  "*" (string (gentemp "subtype")) "*") :pvs)
		     ;;(makesym "*~a*" (gentemp "subtype"))
		     )
	       )
	  (push  (cons expr-type name) *subtype-names*)
	  name))))

(defmethod add-explicit-prover-type ((ex application))
  (when (and (name-expr? (operator ex))
	     (memq (id (operator ex))
		   '(|integer_pred| |rational_pred| |real_pred|))
	     (from-prelude? (declaration (operator ex))))
    (let ((type (case (id (operator ex))
		  (|integer_pred| 'integer)
		  (t 'number))))
      (if (name-expr? (argument ex))
	  (let* ((norm-expr (normalize-name-expr-actuals (argument ex)))
		 (id-hash (gethash norm-expr *translate-id-hash*))
		 (newconst
		  (or id-hash
		      (list (intern
			     (concatenate 'string
			       (string (if (integerp (id (argument ex)))
					   (format nil "~r" (id (argument ex)))
					   (id (argument ex))))
			       "_"
			       (princ-to-string
				(funcall *translate-id-counter*))) :pvs)))))
	    (unless id-hash
	      (setf (gethash norm-expr *translate-id-hash*)
		    newconst))
	    (add-to-sequent-typealist (car newconst) type))
	  (let ((name (cdr (assoc (argument ex) *named-exprs*
				  :test #'tc-eq))))
	    (unless name
	      (let ((newid (gentemp)))
		(add-to-sequent-typealist newid type)
		(push (list (argument ex) newid) *named-exprs*))))))))

(defun add-to-sequent-typealist (id type)
  (let ((entry (assq id *sequent-typealist*)))
    (if entry
	(when (or (not (memq (cdr entry) '(integer number)))
		  (and (eq type 'integer)
		       (not (eq (cdr entry) 'integer))))
	  (setf (cdr entry) type))
	(push (cons id type) *sequent-typealist*))))

(defmethod add-explicit-prover-type (ex)
  (declare (ignore ex))
  nil)

(defun add-to-typealist (id expr &optional type)
  (unless (assoc id typealist)
    (let ((prtype (prover-type (or type (type expr) (car (ptypes expr))))))
      (unless (or (not prtype)
		  ;;(lambda-variable? expr)
		  )
	(push (cons id prtype) typealist)))))

(defun add-to-local-typealist (id expr &optional type)
  (unless (assoc id *local-typealist*)
    (let ((prtype (prover-type (or type (type expr) (car (ptypes expr))))))
      (unless (or (not prtype)
		  ;;(lambda-variable? expr)
		  )
	(push (cons id prtype) *local-typealist*)))))

(defmethod translate-to-prove ((expr number-expr))
  (number expr))

(defmethod translate-to-prove ((expr rational-expr))
  ;; SBCL has a problem with the generic function NUMBER, even though it's been shadowed
  `(DIVIDE ,(numerator (slot-value expr 'number)) ,(denominator (slot-value expr 'number))))

(defmethod translate-to-prove ((ex string-expr))
  (string->int (string-value ex)))

(defun string->int (string)
  (let ((value 0))
    (loop for i from 0 to (1- (length string))
	  do (setq value (+ (ash value 8) (char-int (char string i)))))
    value))

(defun int->string (int &optional chars)
  (if (zerop int)
      (concatenate 'string chars)
      (int->string (ash int -8)
		   (cons (int-char (ldb (byte 8 0) int)) chars))))

(defmethod translate-to-prove ((expr record-expr))
  ;; Creates a dummy record and an update of it.
  (let* ((dummy (get-rec-type-dummy-name (type expr)))
	 (decl (mk-const-decl dummy (type expr)))
	 (res (mk-resolution decl (current-theory-name) (type expr)))
	 (nexpr (make!-name-expr dummy nil nil res)))
    (setf (type nexpr) (type expr))
    (translate-assignments (assignments expr)
			   ;nexpr ;NSH(3.16.97)
			   dummy
			   (type expr))))

(defun get-rec-type-dummy-name (rectype)
  (let ((rtype (find-supertype rectype)))
    (or (cdr (assoc rtype *rec-type-dummies* :test #'tc-eq))
	(let ((name (intern (concatenate 'string
			      "*" (string (gentemp "%%dummy")) "*") :pvs)
		    ;;(makesym "*~a*" (gentemp "%%dummy"))
		    ))
	  (push (cons rtype name) *rec-type-dummies*)
	  name))))

(defmethod translate-to-prove ((expr tuple-expr))
  (cons 'tupcons (translate-to-prove (exprs expr))))
	
;(defmethod translate-to-prove ((expr coercion))
;  (translate-to-prove (expression expr)))

(defmethod translate-to-prove ((expr branch))
  (list '|if|
	(translate-to-prove (condition expr))
	(translate-to-prove (then-part expr))
	(translate-to-prove (else-part expr))))

(defmethod translate-to-prove ((expr cases-expr))
  (translate-to-prove (translate-cases-to-if expr))) ;;was named, not translated to if.

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
			(loop for i from 0
			      upto (1- (length expected))
			      collect
			      `(,(intern (concatenate 'string
					   "TUPSEL-"
					   (string
					    (or (prover-type
					       (nth i (types (type
							      (car arguments)))))
						'||))) :pvs)
				,i
				,translated-arg)))
		      (translate-to-prove arguments)))
	      (translate-to-prove arguments)))))

(defmethod translate-to-prove ((expr projection-expr))
  (let* ((id (make-new-variable '|x| expr))
	 (bd (make-bind-decl id (domain (find-supertype (type expr)))))
	 (varex (make-variable-expr bd)))
    (translate-to-prove
     (make!-lambda-expr (list bd)
       (make!-projection-application (index expr) varex)))))

(defmethod translate-to-prove ((expr injection-expr))
  (let* ((id (make-new-variable '|x| expr))
	 (cotuptype (find-supertype (range (find-supertype (type expr)))))
	 (bd (make-bind-decl id (domain (find-supertype (type expr)))))
	 (varex (make-variable-expr bd)))
    (translate-to-prove
     (make!-lambda-expr (list bd)
       (make!-injection-application (index expr) varex cotuptype)))))

(defmethod translate-to-prove ((expr injection?-expr))
  (let* ((id (make-new-variable '|x| expr))
	 (bd (make-bind-decl id (domain (find-supertype (type expr)))))
	 (varex (make-variable-expr bd)))
    (translate-to-prove
     (make!-lambda-expr (list bd)
       (make!-injection?-application (index expr) varex)))))

(defmethod translate-to-prove ((expr extraction-expr))
  (let* ((id (make-new-variable '|x| expr))
	 (bd (make-bind-decl id (domain (find-supertype (type expr)))))
	 (varex (make-variable-expr bd)))
    (translate-to-prove
     (make!-lambda-expr (list bd)
       (make!-extraction-application (index expr) varex)))))

(defmethod translate-to-prove ((expr projection-application))
  (let ((arg (translate-to-prove (argument expr))))
    `(,(intern (concatenate 'string
		 "TUPSEL-" (string (or (prover-type (type expr)) '||))) :pvs)
      ,(1- (index expr)) ,arg)))

(defmethod translate-to-prove ((expr injection-application))
  (let ((arg (translate-to-prove (argument expr))))
    (list (make-apply-name (mk-funtype (type (argument expr)) (type expr)))
	  (id expr)
	  arg)))

(defmethod translate-to-prove ((expr extraction-application))
  (let ((arg (translate-to-prove (argument expr))))
    (list (make-apply-name (mk-funtype (type (argument expr)) (type expr)))
	  (id expr)
	  arg)))

(defmethod translate-to-prove ((expr injection?-application))
  (let ((arg (translate-to-prove (argument expr))))
    (list (make-apply-name (mk-funtype (type (argument expr)) (type expr)))
	  (id expr)
	  arg)))

(defmethod translate-to-prove ((expr field-application))
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (pos (position id fields :key #'id)))
      ;;(assert (equal fields (sort-fields fields)))
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
      (cond ((and (interpreted? operator)
		  (or (not (memq (id operator) '(+ - * /)))
		      (every #'(lambda (arg)
				 (some #'(lambda (jty)
					   (subtype-of? jty *real*))
				       (judgement-types+ arg)))
			     (arguments expr))))
	     (let ((op (interpretation operator)))
	       (cond ((and (eq op 'DIFFERENCE) (singleton? args))
		      (cons 'MINUS args))
		     ((memq op '(/= ≠))
		      (list 'not (cons 'equal args)))
		     ((and (eq op 'not)
			   (consp (car args))
			   (eq (caar args) 'not))
		      (cadar args))
		     ((eq op 'when)(list 'implies (car args)(cadr args)))
		     (t (cons op args)))))
	    ((and (or (not (typep operator 'name-expr))
		      (not (typep (declaration operator) 'const-decl)))
		  (function-non-functional? operator))
	     (add-to-typealist (id operator) operator (type expr))
	     (cons (id operator) args))
	    (t (let* ((lifted-op (lift-adt operator t))
		      (op (translate-to-prove lifted-op)))
		 (cons (make-apply-name (type operator))
		       (cons (if (symbolp op)
				 (list op) op)
			     args))))))))

(defmethod lift-adt ((ex injection-expr) &optional op?)
  (declare (ignore op?))
  ex)

(defmethod lift-adt ((ex constructor-name-expr) &optional op?)
  (if op?
      (call-next-method)
      (let* ((adt (adt (adt ex)))
	     (constr (find (id ex) (constructors adt)
			   :test #'id-suffix :key #'id)))
	(if (arguments constr)
	    ex
	    (call-next-method ex t)))))

(defmethod lift-adt ((ex adt-name-expr) &optional op?)
  (if op?
      (let* ((mi (module-instance (resolution (find-supertype (adt ex)))))
	     (res (make-resolution (declaration ex)
		   (lcopy mi :library (or (library mi)
					  (library (module-instance ex)))))))
	(mk-name-expr (id ex) nil nil res))
      ex))

(defmethod lift-adt ((ex name-expr) &optional op?)
  (declare (ignore op?))
  (let ((decl (declaration ex)))
    (if (and (const-decl? decl)
	     (listp (positive-types decl))
	     (not (every #'null (positive-types decl))))
	(let* ((mi (module-instance (resolution ex)))
	       (th (module decl))
	       (nmi (lcopy mi
		      :actuals (mapcar #'(lambda (act fml)
					   (if (positive-type-in-decl? fml decl)
					       (let* ((ty (type-value act))
						      (sty (find-supertype ty)))
						 (if (eq sty ty)
						     act
						     (mk-actual sty)))
					       act))
				 (actuals mi) (formals-sans-usings th))
		      :dactuals (mapcar #'(lambda (act fml)
					    (if (positive-type-in-decl? fml decl)
						(let* ((ty (type-value act))
						       (sty (find-supertype ty)))
						  (if (eq sty ty)
						      act
						      (mk-actual sty)))
						act))
				  (dactuals mi) (decl-formals decl))
		      :library (or (library mi)
				   (library (module-instance ex)))))
	       (res (make-resolution decl nmi)))
	  (mk-name-expr (id ex) nil nil res))
	ex)))

(defmethod lift-adt (ex &optional op?)
  (declare (ignore op?))
  ex)

(defun lift-adt-actual (act)
  (assert (type-value act))
  (mk-actual (find-supertype (type-value act))))


(defmethod interpreted? (expr)
  (declare (ignore expr))
  nil)

(defmethod interpreted? ((expr name-expr))
  (with-slots (id resolutions) expr
    (let ((interp (cdr (assq id *interpreted-alist*))))
      (and interp
	   (let ((mi (module-instance (car resolutions))))
	     (and mi (eq (mod-id interp) (id mi))))))))

(defmethod function-non-functional? (expr)
  (declare (ignore expr))
  nil)

(defmethod function-non-functional? ((expr name-expr))
  (and (typep (type expr) 'funtype)
       (not (typep (declaration expr) 'var-decl))
       (not (typep (range (type expr)) 'funtype)))
  nil)


(defmethod operator ((expr lambda-expr))
  (or (op expr) 'LAMBDA))

(defmethod operator ((expr forall-expr))
  (or (op expr) 'FORALL))

(defmethod operator ((expr exists-expr))
  (or (op expr) 'EXISTS))

(defvar *integer-pred* nil)

(defvar *top-translate-to-prove-hash*
  (make-pvs-hash-table))

(defun top-translate-to-prove (expr &optional no-explicit?)
  (let ((*bindings* nil)
	(*generate-tccs* 'none))
    (cond ((hash-table-p *translate-to-prove-hash*)
	   (when *integer*
	     (setq *integer-pred* (translate-to-ground (predicate *integer*))))
	   (when *rational*
	     (setq *rational-pred*
		   (translate-to-ground (predicate *rational*))))
	   (when (and *real*
		      (subtype? *real*))
	     (setq *real-pred* (translate-to-ground (predicate *real*))))
	   (unless no-explicit?
	     (add-explicit-prover-type expr))
	   (translate-to-ground (unit-derecognize expr)))
	  (t (translate-with-new-hash
	       (unless *integer-pred*
		 (setq *integer-pred*
		       (when *integer*
			 (translate-to-ground (predicate *integer*)))))
	       (unless no-explicit?
		 (add-explicit-prover-type expr))
	       (translate-to-ground (unit-derecognize expr)))))))

(defmethod translate-to-prove ((expr binding-expr))
  (let* ((*bindings* (append (bindings expr) *bindings*))
	 (type (type expr))
	 (prtype (prover-type type))
	 (body (expression expr))
	 (exprtype (prover-type (type body)))
	 (connective? (connective-occurs? body))
	 (tr-expr
	  (if connective?
	      ;;NSH(11.4.95): Fixes unsoundness caused
		    ;;by translating (LAMBDA : IF B ...) to (LAMBDA 1: CC)
		    ;;by generating (LAMBDA 1 : CC(*1*)) instead.
		    ;;Note that two tc-eq such LAMBDAs will generate
		    ;;different translations so that ASSERT might miss some
		    ;;syntactic equalities when there are IFs in LAMBDAs.
	      (cond
	       (*bindings*
		(let* ((newid (gentemp))
		       (body-freevars (freevars body))
		       (body-bindings (intersection body-freevars *bindings*
						    :test #'same-declaration))
		       ;;bound variables can't be ignored; subtyp constraints needed
		       ;;freevars of the expr are needed for connectives.
		       (tr-vars (translate-to-prove body-bindings))
		       (apname (make-apply-symbol (length tr-vars) exprtype))
		       (apform (cons apname (cons newid tr-vars))))
		  (unless (or (null exprtype)
			      (assoc apname typealist))
		    (push (cons apname exprtype) typealist)
		    (pushnew apname applysymlist))
		  apform))
	       (t (let ((name (cdr (assoc body *named-exprs* :test #'tc-eq))))
		    ;;if there are no *bindings* then use the cached translation in *named-exprs*
		    (or name
			(let ((newid (gentemp)))
			  (add-to-typealist newid nil (type expr))
			  (push (list body newid) *named-exprs*)
			  (list newid))))))
	      (translate-to-prove (expression expr))))
	 (tr-operator (unique-binding-operator expr))
	 (tr-lambda-expr
	  (cons 'lambda
		(cons (length (bindings expr))
		      (list tr-expr)))))
    (if (lambda-expr? expr)
	tr-lambda-expr
	 (list (make-apply-symbol 1 prtype)
	       tr-operator
	       tr-lambda-expr))))

(defmethod unique-binding-operator ((ex quant-expr))
  (let ((key (cons (operator ex) (mapcar #'type (bindings ex)))))
    (or (gethash key *translate-id-hash*)
	(setf (gethash key *translate-id-hash*)
	      (intern (concatenate 'string
			(string (operator ex))
			"_"
			(princ-to-string
			 (funcall *translate-id-counter*))) :pvs)))))

(defmethod unique-binding-operator ((ex lambda-expr))
  'lambda)

;;; Update expressions
;;; Translate expressions of the form
;;; A WITH [ (0) := 1 ],
;;;    where A is an array of type int->int, into
;;; (apply int ARRAYSTORE A 0 1)
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
;;; (apply function[state[T0],state[T0] -> int]
;;;        update
;;;        (apply function[state[T0],state[T0] -> int]
;;;               update
;;;               (apply function[state[T0],state[T0] -> int]
;;;                      update
;;;                      g (0) h)
;;;               (1) (apply int update g(1) (x y) 0))
;;;        (1) (apply int update g(1) (x' y') 1))

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

(defun translate-assignment (assign trbasis type)
  (translate-assign-args (arguments assign)
			 (expression assign)
			 trbasis
			 (find-supertype type)))

(defun translate-assign-args (args value trbasis type)
  (if args
      (let ((sorted-fields (when (record-or-struct-subtype? type)
			     ;;(sort-fields (fields type))
			     (fields type))))
	(list 'update
	      trbasis
	      (typecase type
		(record-or-struct-subtype
		 (position (id (caar args)) sorted-fields
			   :test #'eq :key #'id))
		(tupletype
		 (1- (number (caar args))))
		(funtype
		 (if (singleton? (car args))
		     (translate-to-prove (caar args))
		     (cons 'tupcons (translate-to-prove (car args)))))
		(t
		 (translate-to-prove (caar args))))
	      (let* ((ntrbasis-type
		      (find-supertype 
		       (typecase type
			 (record-or-struct-subtype
			  (type (find (id (caar args)) (fields type)
				      :test #'eq :key #'id)))
			 (tupletype
			  (nth (1- (number (caar args)))
			       (types type)))
			 (funtype
			  (range type))
			 (t
			  (range (type (caar args)))))))
		     (ntrbasis
		      (typecase type
			(record-or-struct-subtype
			 (make-tr-field-application
			  (mk-funtype type ntrbasis-type)
			  (position (id (caar args)) sorted-fields
				    :test #'eq :key #'id)
			  trbasis))
			(tupletype
			 (make-tr-projection-application
			  ntrbasis-type (number (caar args)) trbasis))
			(funtype
			 (make-tr-assign-application
			  type
			  trbasis
			  (if (singleton? (car args))
			      (translate-to-prove (caar args))
			      (cons 'tupcons (translate-to-prove (car args))))))
			(t
			 (make-tr-assign-application
			  (type (caar args))
			  (translate-to-prove (caar args))
			  trbasis)))))
		(translate-assign-args (cdr args)
				       value
				       ntrbasis
				       ntrbasis-type))))
      (translate-to-prove value)))

(defun make-tr-field-application (field-accessor-type fieldnum tr-expr)
  (list (make-apply-name field-accessor-type)
	tr-expr fieldnum))

(defun make-tr-projection-application (type number expr)
  `(,(intern (concatenate 'string
	       "TUPSEL-" (string (or (prover-type type) '||))) :pvs)
    ,(1- number) ,expr))

(defun make-tr-assign-application (fun-type expr args)
  (list (make-apply-name fun-type)
	expr args))

(defun make-apply-symbol (arity type)
  (let ((name (intern (concatenate 'string
				   "APPLY-"
				   (princ-to-string arity)
				   "-" (string (or type '||))) :pvs)))
    (unless (or (not type)
		(assoc name typealist))
      (push (cons name type) typealist)
      (pushnew name applysymlist))
    name))

(defmethod make-apply-name ((te type-expr))
  (let* ((type (find-supertype te))
	 (prtype (prover-type (range type)))
	 (name (make-apply-symbol (length (domain-types type))
				  prtype)))
    name))

(defmethod prover-type ((type type-expr))
  (cond ((tc-eq (find-supertype type) *boolean*) 'bool)
	((member type `(,*integer* ,*naturalnumber*) :test #'tc-eq) 'integer)
	((tc-eq type *real*) 'number)
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
	(mk-name '≠ nil '|notequal|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name '=> nil '|booleans|)
	(mk-name '⇒ nil '|booleans|)
	(mk-name 'WHEN nil '|booleans|)		
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
	(mk-name '∧ nil '|booleans|)	
	(mk-name 'OR nil '|booleans|)
	(mk-name '∨ nil '|booleans|)	
	(mk-name 'NOT nil '|booleans|)
	(mk-name '¬ nil '|booleans|)
	(mk-name '<=> nil '|booleans|)
	(mk-name '⇔ nil '|booleans|)
	(mk-name 'IFF nil '|booleans|)		
	(mk-name '+ nil '|number_fields|)
	(mk-name '- nil '|number_fields|)
	(mk-name '* nil '|number_fields|)
	(mk-name '/ nil '|number_fields|)
	(mk-name '< nil '|reals|)
	(mk-name '<= nil '|reals|)
	(mk-name '> nil '|reals|)
	(mk-name '>= nil '|reals|)
	))

(defparameter *interpreted-alist*
  (mapcar #'(lambda (x) (cons (id x) x)) *interpreted-names*))

(defun interpretation (name)
  (or (cdr (assoc (id name) *interpretations*))
      (id name)))

(defparameter *interpretations*
  '((= . equal)
    (< . lessp)
    (<= . lesseqp)
    (> . greaterp)
    (>= . greatereqp)
    (+ . PLUS)
    (- . DIFFERENCE)
    (* . TIMES)
    (/ . DIVIDE)
    (AND . and)
    (OR . or)
    (NOT . not)
    (IMPLIES . implies)
    (¬ . not)
    (∨ . or)
    (∧ . and)
    (& . and)
    (⇒ . implies)
    (iff . equal)
    (<=> . equal)
    (⇔ . equal)
    (WHEN . when)));; need to deal with WHEN
