;modified from prover/translate-to-prove.
(in-package 'pvs)

(proclaim '(optimize (compilation-speed 0) (space 1)
		     (safety 1) (speed 3)
		     #+allegro (debug 1)))

(defvar *translate-to-dc-hash* (make-hash-table :hash-function 'pvs-sxhash
						:test 'tc-eq))

(defvar *translate-from-dc-hash* (dp::dp-make-eq-hash-table))

(defvar *dc-named-exprs* (make-hash-table :hash-function 'pvs-sxhash
					  :test 'tc-eq))
(defvar *dc-translate-id-counter* nil)
(newcounter *dc-translate-id-counter*)

(defvar *dc-translate-id-hash* (make-hash-table :hash-function 'pvs-sxhash
						:test 'tc-eq))
(defvar *prtype-hash* (make-hash-table))
(defvar *local-prtype-hash* (make-hash-table))


(defun reset-translate-to-dc ()
  (clrhash *translate-to-dc-hash*)
  (clrhash *translate-from-dc-hash*)
  (clrhash *dc-named-exprs*)
  (clrhash *dc-translate-id-hash*)
  (clrhash *prtype-hash*)
  (clrhash *local-prtype-hash*)
  (newcounter *dc-translate-id-counter*))

(defvar *interpret-bit-vectors* t)
(defvar *translate-rewrite-rule* nil)
(defvar *translate-to-bdd* nil)
(defvar *translate-iff-to-implies* nil)
(defvar *translate-set-expr* t)
(defvar *translate-quantifiers* nil)

(defvar *dc-interpreted-names* nil)

(defun dc-interpreted-names ()
  (or *dc-interpreted-names*
  `((= |equalities| ,dp::*=*)
    (/= |notequal| ,dp::*nequal*)
    (IMPLIES |booleans| ,dp::*implies*)
    (AND  |booleans| ,dp::*and*)
    (& |booleans| ,dp::*and*)
    (OR |booleans| ,dp::*or*)
    (NOT |booleans| ,dp::*not*)
    (+ |reals| ,dp::*plus*)
    (- |reals| ,dp::*minus*)
    (* |reals| ,dp::*times*)
    (/ |reals| ,dp::*divide*)
    (< |reals| ,dp::*lessp*)
    (<= |reals| ,dp::*lesseqp*)
    (> |reals| ,dp::*greaterp*)
    (>= |reals| ,dp::*greatereqp*)
    (|floor| |floor_ceil| ,dp::*floor*)
    (^ |bv_caret| ,dp::*bv-extract*)
    (|bv2nat| |bv_nat| ,dp::*bv2nat*)
    (|nat2bv| |bv_nat| ,dp::*nat2bv*)
    (|o| |bv_concat| ,dp::*bv-concat*)
    (O |bv_concat| ,dp::*bv-concat*)
    (|empty_bv|  |empty_bv| ,dp::*bv-epsilon*)
    (|bvec0|  |bvec_const| ,dp::*bv-zero*)
    (|bvec1|  |bvec_const| ,dp::*bv-one*)
    (+ |bvec_plus| ,dp::*bv-plus*)
    (OR |bv_bitwise| ,dp::*bv-or*)
    (AND |bv_bitwise| ,dp::*bv-and*)
    (XOR |bv_bitwise| ,dp::*bv-xor*))))
    
;; List of associations
;; (id . ((pvs-name1 . dp-sym1)...(pvs-namek . dp-symk))).
;; The id entry is used for fast fail and a list of assocs
;; is needed to allow for overloading."

(defvar *dc-interpreted-alist* nil)

(defun dc-interpreted-alist ()
  (or *dc-interpreted-alist* 
      (let ((*alist* nil))
	(loop for (id mod-id dp-sym) in (dc-interpreted-names)
	      do (let* ((pvs-name (mk-name id nil mod-id))
			(translations (assq id *alist*)))
		   (if translations
		       (push (cons pvs-name dp-sym)
			     (cdr translations))
		       (push (cons id (acons pvs-name dp-sym nil))
			     *alist*))))
	*alist*)))

(defun interpreted-bv-op? (expr)
  (and *interpret-bit-vectors*
       (let ((actuals (actuals (module-instance expr))))
	 (every #'number-expr? actuals))))

(defun choose-interpretation (id mod-id alist)
  (cond ((null alist) nil)
	((null (cdr alist))
	 (cdr (car alist)))
	(t (let* ((assoc (car alist))
		  (name (car assoc)))
	     (if (and (eq (id name) id)
		      (eq (mod-id name) mod-id))
		 (cdr assoc)
		 (choose-interpretation id mod-id (cdr alist)))))))

(defmethod interpretation ((expr name-expr))
      (with-slots (id resolutions) expr
	(let ((alist (cdr (assq id (dc-interpreted-alist)))))
	  (and alist (let ((mi (module-instance (car resolutions))))
		       (and mi (let ((dp-sym (choose-interpretation id (id mi) alist)))
				 (when dp-sym
				   (if (eq (dp::node-initial-type dp-sym) 'bv-op)
				       (and (interpreted-bv-op? expr) dp-sym)
				       dp-sym))))))))))

(defmethod interpretation (expr)
  nil)

;;; Translate into list representation for passing to the prover.
;;; This also sets up the global variables typealist, and inserts
;;; subtype information.


(defun set-inverse-translation (expr inverse)
  (if *use-translate-from-dc-hash*
      (setf (gethash expr *translate-from-dc-hash*) inverse)
      (setf (dp::node-external-info expr) inverse)))

(defmethod translate-to-dc :around (obj)
  (if (or *bound-variables* *bindings*)
      (let ((result (call-next-method)))
	(when (dp::node-p result)
	  (set-inverse-translation result obj))
	result)
      (let ((hashed-value (gethash obj *translate-to-dc-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (unless (or *bound-variables* *bindings*)
		(when (dp::node-p result)
		  (setf (gethash obj *translate-to-dc-hash*) result)))
	      (when (dp::node-p result)
		(set-inverse-translation result obj))
	      result)))))

(defmethod translate-to-dc ((list list))
  (mapcar #'translate-to-dc list))

(defmethod translate-to-dc ((expr field-decl))
  (let ((newconst (dc-unique-prover-name expr)))
    (add-to-prtype-hash newconst expr (type expr))
    newconst))

(defmethod translate-to-dc ((expr name-expr))
  (let* ((pos (position expr *bindings*	;(NSH:4-5-91)
			:test #'same-declaration))
	 (apos (position expr *bound-variables*
			 :test #'same-declaration))
	 (bpos (when apos (- (length *bound-variables*)
			     apos))))
    (cond ((and (null pos)(null bpos))
	   (translate-name-expr-with-actuals-to-dc expr))
	  (bpos (let* ((id (dp::mk-constant
			     (makesym "*~a_~a*" (id expr) bpos)))
		       (bvlength (fixed-length-bitvector-type? (type expr)))
		       (id (if bvlength
			       (dp::mk-bv-const id (dp::mk-constant bvlength))
			       id)))
		  (add-to-local-prtype-hash id expr)
		  (install-node-type expr id)
		  id)) ;;NSH(4.2.95) id was (id expr) and unsound!
	               ;;eg. proved (FORALL i, (j|j<i): (FORALL (j| j>i): j<i).
	  (t (let ((nvar (makesym "*~a*" (1+ pos))))
	       (dp::mk-term
		(list (dp::mk-constant
		       (get-subtype-coercion expr))
		      (dp::mk-constant nvar)));;coercion needed 
		)))))  ;;regardless of type:(all (x:foo):..)/=(all (x:bar)..)


(defun translate-name-expr-with-actuals-to-dc (expr)
  (let* ((dc-base-name (dc-unique-prover-name expr))
	 (actuals (actuals (module-instance expr)))
	 (dc-actuals (translate-to-dc-actuals actuals)))
    (cond ((and (constructor? expr)
		(enum-adt? (find-supertype (type expr))))
	   dc-base-name)
	  ((eq dc-base-name dp::*bv-one*)
	   (dp::mk-bv-one (car dc-actuals)))
	  ((eq dc-base-name dp::*bv-zero*)
	   (dp::mk-bv-zero (car dc-actuals)))
	  ((eq dc-base-name dp::*bv-concat*)
	   *bv-concat*)
	  ((eq dc-base-name dp::*bv-extract*)
	   *bv-extract*)
	  (t (if dc-actuals
		 (dp::mk-term (cons dp::*th-app*
				    (cons dc-base-name dc-actuals))
			      (dp::node-initial-type dc-base-name))
		 dc-base-name)))))
	

(defun list-of-decl-and-type-actuals (expr)
  (let ((actuals (actuals (module-instance expr))))
    (cons (declaration expr) (remove-if-not #'type-value actuals))))

(defun translate-to-dc-actuals (actuals)
  (if (consp actuals)
      (if (type-value (car actuals))
	  (translate-to-dc-actuals (cdr actuals))
	  (cons (translate-to-dc (expr (car actuals)))
		(translate-to-dc-actuals (cdr actuals))))
      nil))

(defun fixed-length-bitvector-type? (type)
  (let* ((type (find-supertype type))
	 (ptype (print-type type)))
    (and ptype
	 (eq (id ptype) '|bvec|)
	 (eq (id (module-instance ptype)) '|bv|)
	 (let ((actual (car (actuals (module-instance ptype)))))
	   (and (number-expr? (expr actual))
		(number (expr actual)))))))

(defmethod translate-to-dc ((expr constructor-name-expr))
  (call-next-method (lift-adt expr)))

(defun dc-set-constructor-type (expr dp-const)
  (let ((dc-accessors (translate-to-dc (accessors expr))))
    (setf (dp::node-constructor-accessors dp-const) dc-accessors)
    (setf (dp::node-interpreted? dp-const) t)
    (setf (dp::node-constructor? dp-const) t)))

(defun make-constructor-indices-for-accessor (accessor)
  (let* ((constructors (constructor accessor)))
    (loop for constructor in constructors
	  for arguments = (accessors constructor)
	  for index = (position accessor
				arguments
				:test #'tc-eq)
	  when index
	  collect (cons (translate-to-dc constructor) index))))

(defun dc-set-accessor-type (expr dp-const)
  (let ((indices (make-constructor-indices-for-accessor  expr)))
    (setf (dp::node-interpreted? dp-const) t)
    (setf (dp::node-accessor-indices dp-const) indices)))

(defun dc-unique-prover-name (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	(let* ((hash-list (list-of-decl-and-type-actuals expr))
	       (id-hash (gethash hash-list *dc-translate-id-hash*))
	       (dc-expr
		(or id-hash
		    (when (tc-eq expr *true*) dp::*true*)
		    (when (tc-eq expr *false*) dp::*false*)
		    (interpretation expr)
		    (let* ((type (type expr))
			   (range-type (if (typep type 'funtype)
					   (range type)
					   type))
			   (dc-range-type (dc-prover-type range-type))
			   (bvlength (fixed-length-bitvector-type? type))
			   (new-const1
			    (dp::mk-constant
			     (intern (format nil "~a_~a"
				       (id expr) (funcall *dc-translate-id-counter*)))
			     dc-range-type))
			   (new-const (if bvlength
					  (dp::mk-bv-const new-const1
							   (dp::mk-constant bvlength))
					  new-const1)))
		      (dc-add-to-reverse-prover-name new-const expr)
		      (dc-add-to-pvs-typealist new-const expr)
		      new-const))))
	   (unless id-hash
	     (setf (gethash hash-list *dc-translate-id-hash*)
		   dc-expr)
	     (add-to-prtype-hash dc-expr expr)
	     (cond
	      ((constructor? expr)
	       (dc-set-constructor-type expr dc-expr))
	      ((accessor? expr)
	       (dc-set-accessor-type expr dc-expr))
	      (t nil)))
	   dc-expr))
	((typep expr 'field-decl)
	 (let* ((id (id expr))
		(id-hash (gethash id
				  *dc-translate-id-hash*))
		(newconst
		 (or id-hash
		     (dp::mk-constant
			    (intern (format nil "~a-~a"
				      id
				      (funcall
				       *dc-translate-id-counter*)))))))
	   (unless id-hash
	     (setf (gethash id *dc-translate-id-hash*)
		   newconst))
	   newconst))
	(t ;(add-to-local-prtype-hash (id expr) expr)
	   (dc-add-to-reverse-prover-name (id expr) expr)
	   (dc-add-to-pvs-typealist (id expr) expr)
	   (let ((sym (if *translate-rewrite-rule*
			  (dp::mk-variable (id expr) (dc-prover-type (type expr)))
			  (dp::mk-constant (id expr) (dc-prover-type (type expr))))))
	     (install-node-type expr sym)
	     sym))))
   
(defun install-node-type (expr sym)
   (cond ((tc-eq (type expr) *integer*)
	  (setf (dp::node-initial-type sym) dp::*integer*))
	 ((or (tc-eq (type expr) *rational*)
	      (tc-eq (type expr) *real*))
	  (setf (dp::node-initial-type sym) dp::*number*))))
	
(defun add-to-prtype-hash (id expr &optional type)
  (setf (dp::node-initial-type id)
	(or type
	    (and expr (dc-prover-type (type expr)))))
  (let ((entry (gethash id *prtype-hash*)))
    (unless entry
      (setf (gethash id *prtype-hash*)
	    (or type
		(and expr (dc-prover-type (type expr))))))))

(defun add-to-local-prtype-hash (id expr)
  (setf (dp::node-initial-type id)
	(dc-prover-type (type expr)))
  (let ((entry (gethash id *local-prtype-hash*)))
    (unless entry
      (setf (gethash id *local-prtype-hash*)
	    (dc-prover-type (type expr))))))

(defmethod translate-to-dc ((expr number-expr))
  (let ((cnstnt (dp::mk-constant (number expr))))
    (install-node-type expr cnstnt)
    cnstnt))

(defmethod translate-to-dc ((expr record-expr))
  (translate-dc-assignments-to-record (assignments expr)
				      (find-supertype (type expr))))

(defun sort-assignments (assignments)
  (sort (copy-list assignments)
	#'string-lessp
	:key #'(lambda (assignment)
		 (id (caar (arguments assignment))))))

(defun dc-sort-fields (fields)
  (sort (copy-list fields)
	#'string-lessp
	:key #'id))

(defun translate-dc-record-constructor (type)
  (let* ((sorted-fields (dc-sort-fields (fields type)))
	 (tr-sorted-fields (mapcar #'translate-to-dc
				   sorted-fields)))
    (dp::mk-term
     (cons dp::*record* tr-sorted-fields)
     dp::'record-op)))

(defun translate-dc-assignments-to-record (assignments type)
  (let* ((sorted-assignments (sort-assignments assignments))
	 (tr-sorted-values (mapcar #'(lambda (ass)
				       (translate-to-dc (expression ass)))
				   sorted-assignments))
	 (tr-record-constructor (translate-dc-record-constructor type)))
    (dp::mk-term (cons tr-record-constructor tr-sorted-values))))

(defmethod translate-to-dc ((expr tuple-expr))
  (dp::mk-term (cons dp::*tuple* (translate-to-dc (exprs expr)))))
	
(defmethod translate-to-dc ((expr coercion))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-dc reduced-expr))))

(defmethod translate-to-dc ((expr cases-expr))
  (translate-to-dc (translate-cases-to-if expr)))

(defmethod translate-to-dc ((expr let-expr))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* expr)))))
      (translate-to-dc reduced-expr))))

(defmethod translate-to-dc ((expr projection-application))
  (let* ((dc-type (dc-prover-type (type expr)))
	 (arg (translate-to-dc (argument expr)))
	 (result
	  (dp::sigproject
	   (dp::mk-term
	    (list dp::*project*;;might need to generate a unique one
		  ;;according to the prtype and set prtype.
		  (dp::mk-constant (1- (index expr))) arg)))))
    (when dc-type
      (setf (dp::node-initial-type result) dc-type))
    result))

(defmethod translate-to-dc ((expr field-application))
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   (sfields (sort-fields fields))
	   (pos (position id sfields
			  :test #'(lambda (x y) (eq x (id y)))))
	   (dc-type (dc-prover-type (type expr)))
	   (result (dp::sigproject
		    (dp::mk-term
		     (list dp::*project* (dp::mk-constant pos)
			   (translate-to-dc argument))))))
      (when dc-type
	(setf (dp::node-initial-type result) dc-type))
      result)))

;;NSH(5.17.94): Complicated code to deal with tuple mismatch
;;between domain of operator and arguments.
;; op(a1,..., an) if dom(type(op)) = [t1,...,tn] ==>
;;                      (op' (tupcons a1' .. an'))
;; op(a1), if dom(type(op)) = t1,...,tn ==> (op (tupsel 0 a1)...)

(defmethod translate-to-dc ((expr application))
  (with-slots (operator argument) expr
    (let* ((args (translate-to-dc (arguments expr)))
	   (op (interpretation operator)))
      (cond (op
	     (cond ((and (eq op dp::*difference*) (singleton? args))
		    (dp::mk-term (cons dp::*minus* args)))
		   ((eq op dp::*nequal*)
		    (dp::mk-term (list dp::*not*
				       (dp::mk-equality (car args)
							(cadr args)))))
		   ((and (eq op dp::*not*)
			 (dp::negation-p (car args)))
		    (dp::arg 1 (car args)))
		   ((eq op dp::*bv-zero*)
		    (let ((actual (car (actuals operator)))
			  (width (translate-to-dc actual)))
		      (dp::mk-bv-zero width)))
		   ((eq op dp::*bv-one*)
		    (let ((actual (car (actuals operator)))
			  (width (translate-to-dc actual)))
		      (dp::mk-bv-one width)))
		   (*translate-rewrite-rule*
		    (dp::sigma (dp::mk-term (cons op args))
			       *init-dp-state*))
		   (t (dp::mk-term (cons op args)))))
	    ((and (or (not (typep operator 'name-expr))
		      (not (typep (declaration operator) 'const-decl)))
		  (function-non-functional? operator))
	     (break))
	    (t (let* ((op (translate-to-dc (lift-adt operator t)))
		      (translated-expr
		       (mk-typed-term (cons op args) (dc-prover-type (type expr))))
		      (bvlength (fixed-length-bitvector-type? (type expr))))
		 (if bvlength
		     (dp::mk-bv-const translated-expr (dp::mk-constant bvlength))
		     translated-expr)))))))

(defun mk-typed-term (args type)
  (let ((result (dp::mk-term args)))
    (setf (dp::node-initial-type result) type)
    result))


(defmethod translate-to-dc ((expr binding-expr))
  (let* ((*bindings* (append (bindings expr) *bindings*))
	 (binding-vars (mapcar #'make-variable-expr (bindings expr)))
	 (type (type expr))
	 (prtype (dc-prover-type type))
	 (tr-bndvars (translate-to-dc binding-vars))
	 ;;bound variables can't be ignored; subtyp constraints needed
	 (tr-freevars (translate-to-dc (freevars expr)))
	 ;;freevars of the expr are needed for connectives.
	 (tr-vars (append tr-bndvars tr-freevars))
	 (connective? (connective-occurs? (expression expr)))
	 (tr-expr
	  (if connective?
	      (let ((name (gethash expr *dc-named-exprs*)))
		(or name;;NSH(11.4.95): Fixes unsoundness caused
		    ;;by translating (LAMBDA : IF B ...) to (LAMBDA 1: CC)
		    ;;by generating (LAMBDA 1 : CC(*1*)) instead.
		    ;;Note that two tc-eq such LAMBDAs will generate
		    ;;different translations so that ASSERT might miss some
		    ;;syntactic equalities when there are IFs in LAMBDAs.
		    (let* ((newid (dp::mk-constant (gentemp)))
			   ;;(freevars (freevars (expression expr)))
			   )
		      ;;NSH(2.5.96) (freevars (expr..)) instead of
		      ;;(freevars expr).
		      (cond
		       (*bindings*
			(let ((apform (dp::mk-term (cons newid
							 tr-vars))))
			  (setf (gethash expr *dc-named-exprs*)  apform)
			  apform))
		       (t 
			(add-to-prtype-hash newid expr prtype)
			(setf (gethash expr *dc-named-exprs*)  newid)
			newid)))))
	      (translate-to-dc (expression expr))))
	 (tr-lambda-expr
	  (dp::mk-term
	   (cons dp::*lambda*
		 (cons (dp::mk-constant (length (bindings expr)))
		       (if (or connective? (null tr-freevars))
			   (list tr-expr)
			   (list tr-expr
				 (dp::mk-term
				  (cons (dp::mk-constant (operator expr))
					tr-bndvars)))))))))
    (if (lambda-expr? expr)
	tr-lambda-expr
	(dp::mk-term (list (dp::mk-constant (operator expr))
			   tr-lambda-expr)))))


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

(defmethod translate-to-dc ((expr update-expr))
  (translate-dc-assignments (assignments expr)
			 (translate-to-dc (expression expr))
			 (type expr)))

(defun translate-dc-assignments (assigns trbasis type)
  (if assigns
      (translate-dc-assignments
       (cdr assigns)
       (translate-dc-assignment (car assigns) trbasis type)
       type)
      trbasis))

(defun translate-dc-assignment (assign trbasis type)
  (translate-dc-assign-args (arguments assign)
			 (expression assign)
			 trbasis
			 (find-supertype type)))

(defun translate-dc-assign-args (args value trbasis type)
  (if args
      (dp::mk-term (list dp::*update*
		     trbasis
		     (typecase type
		       (recordtype
			(dp::mk-constant
			 (position (caar args) (sort-fields (fields type))
				   :test #'same-id)))
		       (tupletype
			(dp::mk-constant (1- (number (caar args)))))
		       (t (if (singleton? (car args))
			      (translate-to-dc (caar args))
			      (dp::mk-term
			       (cons (dp::mk-constant dp::*tuple*)
				     (translate-to-dc (car args)))))))
		     (let* ((ntrbasis-type
			     (find-supertype 
			      (typecase type
				(recordtype
				 (type (find (caar args)(fields type)
					     :test #'same-id)))
				(tupletype
				 (nth (1- (number (caar args)))
				      (types type)))
				(t (range type)))))
			    (ntrbasis
			     (typecase type
			       (recordtype
				(make-dc-field-application
				 (mk-funtype type ntrbasis-type)
				 (position (caar args)
					   (sort-fields (fields type))
					   :test #'same-id)
				 trbasis))
			       (tupletype
				(make-dc-projection-application
				 ntrbasis-type (number (caar args)) trbasis))
			       (t (make-dc-assign-application
				   type
				   trbasis
				   (if (singleton? (car args))
				       (translate-to-dc (caar args))
				       (dp::mk-term (cons dp::*tuple* (translate-to-dc (car args))))))))))
		       (translate-dc-assign-args (cdr args)
				     value
				     ntrbasis
				     ntrbasis-type))))
  (translate-to-dc value)))

(defun translate-dc-assign-args (args value trbasis type)
  (if args
      (typecase type
	(recordtype (translate-dc-assign-args-record args value trbasis type))
	(tupletype (translate-dc-assign-args-tuple args value trbasis type))
	(t (translate-dc-assign-args-update args value trbasis type)))
      (translate-to-dc value)))

(defun translate-dc-assign-args-record (args value trbasis type)
  (let* ((tr-record-constructor
	  (if (dp::record-p trbasis)
	      (dp::funsym trbasis)
	      (translate-dc-record-constructor type)))
	 (position (position (caar args) (dc-sort-fields (fields type))
			     :test #'same-id))
	 (new-value 
	  (let* ((ntrbasis-type
		  (find-supertype 
		   (type (find (caar args)(fields type)
			       :test #'same-id))))
		 (ntrbasis
		  (make-dc-field-application
		   (mk-funtype type ntrbasis-type)
		   (position (caar args)
			     (sort-fields (fields type))
			     :test #'same-id)
		   trbasis)))
	    (translate-dc-assign-args (cdr args)
				      value
				      ntrbasis
				      ntrbasis-type)))
	 (args
	  (if (dp::record-p trbasis)
	      (loop for i from 0
		    for old-value in (dp::funargs trbasis)
		    collect (if (= i position) new-value old-value))
	      (translate-dc-record-name-to-record-args trbasis type
						       position
						       new-value))))
    (dp::mk-term (cons tr-record-constructor args))))

(defun translate-dc-record-name-to-record-args (trbasis type position value)
  (loop for i from 0 below (length (fields type))
	collect (if (= i position)
		    value
		    (dp::mk-term (list dp::*project*
				       (dp::mk-constant i)
				       trbasis)))))

(defun translate-dc-assign-args-tuple (args value trbasis type)
  (let* ((position (1- (number (caar args))))
	 (new-value (let* ((ntrbasis-type
			     (find-supertype 
			      (nth (1- (number (caar args)))
				   (types type))))
			    (ntrbasis
			     (make-dc-projection-application
			      ntrbasis-type (number (caar args)) trbasis)))
		       (translate-dc-assign-args (cdr args)
						 value
						 ntrbasis
						 ntrbasis-type)))
	 (args
	  (if (dp::tuple-p trbasis)
	      (loop for i from 0
		    for old-value in (dp::funargs trbasis)
		    collect (if (= i position) new-value old-value))
	      (translate-dc-to-tuple-args trbasis type
					  position
					  new-value))))
      (dp::mk-term (cons dp::*tuple* args))))

(defun translate-dc-to-tuple-args (trbasis type position value)
  (loop for i from 0 below (length (types type))
	collect (if (= i position)
		    value
		    (dp::mk-term (list dp::*project*
				       (dp::mk-constant i)
				       trbasis)))))

(defun translate-dc-assign-args-update (args value trbasis type)
      (dp::mk-term (list dp::*update*
		     trbasis
		     (if (singleton? (car args))
			 (translate-to-dc (caar args))
			 (dp::mk-term
			  (list (dp::mk-constant dp::*tuple*)
				(translate-to-dc (car args)))))
		     (let* ((ntrbasis-type
			     (find-supertype 
			      (range type)))
			    (ntrbasis
			     (make-dc-assign-application
			      type
			      trbasis
			      (if (singleton? (car args))
				  (translate-to-dc (caar args))
				  (dp::mk-term
				   (cons dp::*tuple*
					 (translate-to-dc (car args))))))))
		       (translate-dc-assign-args (cdr args)
						 value
						 ntrbasis
						 ntrbasis-type)))))


(defun make-dc-field-application (field-accessor-type fieldnum dc-expr)
  (let ((appl (dp::sigproject
	       (dp::mk-term (list dp::*project*
				  (dp::mk-constant fieldnum) dc-expr)))))
    ;;(setf (dp::node-initial-type appl) field-accessor-type)
    appl))

(defun make-dc-projection-application (type number expr)
  (let ((appl (dp::sigproject
	       (dp::mk-term `(,dp::*project*
			      ,(dp::mk-constant (1- number)) ,expr)))))
    ;;(setf (dp::node-initial-type appl) (prover-type type))
    appl))

(defun make-dc-assign-application (fun-type expr args)
  (let ((appl (dp::mk-term (list expr args))))
    ;;(setf (dp::node-initial-type appl) fun-type)
    appl))

(defun mk-dc-assign-application (op args)
  (let ((appl (dp::mk-term (cons op args))))
    (setf (type appl) (range (type op)))
    appl))

(defun mk-assign-application (op args)
  (let ((appl (mk-application* op args)))
    (setf (type appl) (range (type op)))
    appl))

(defmethod dc-prover-type ((type type-expr))
  (cond ((tc-eq type *boolean*) dp::*boolean*)
	((or (tc-eq type *integer*)
	     (tc-eq type *naturalnumber*))
	 dp::*integer*)
	((tc-eq type *number*) dp::*number*)
	((typep type 'funtype)
	 (if (tc-eq (range type) *boolean*)
	     (if (simple-below? (domain type))
		 dp::*bv*
	         dp::*set*)
	     dp::*functional*))
	((typep type 'recordtype) dp::*array*)
	((typep type 'subtype)
	 (dc-prover-type (supertype type)))))

(defmethod dc-prover-type ((te dep-binding))
  (dc-prover-type (type te)))

(defmethod translate-to-dc ((expr quant-expr))
  (call-next-method))

(defmethod translate-to-dc ((expr set-expr))
  (unless *translate-set-expr*
    (call-next-method))
  (let* ((bndng (car (bindings expr)))
	 (body (expression bndng))
	 (type (type expr))
	 (prtype (dc-prover-type type))
	 (*bound-variables* (cons bndng *bound-variables*))
	 (x (translate-to-dc bndng)))
    (dp::mk-lambda x (translate-to-dc body))))

(defun lift-quantifier (fml)
  (let ((stop-types (list *integer* *rational* *real*)))
    (lift-predicates-in-quantifier fml stop-types)))

(defmethod translate-to-dc ((expr iff-or-boolean-equation))
  (if *translate-iff-to-implies*
      (let ((lhs (translate-to-dc (args1 expr)))
	    (rhs (translate-to-dc (args2 expr))))
	(dp::mk-conjunction (dp::mk-implication lhs rhs)
			    (dp::mk-implication rhs lhs)))
      (call-next-method)))

(defmethod translate-to-dc ((expr branch))
  (let ((trm1 (translate-to-dc (condition expr)))
	(trm2 (translate-to-dc (then-part expr)))
	(trm3 (translate-to-dc (else-part expr)))) 
    (if (tc-eq (range (type expr)) *boolean*)
	(dp::mk-ite trm1 trm2 trm3 'predicate)
	(dp::mk-ite trm1 trm2 trm3))))

(defmethod translate-to-dc ((expr implication))
  (let ((lhs (translate-to-dc (args1 expr)))
	(rhs (translate-to-dc (args2 expr))))
    (dp::mk-implication lhs rhs)))

(defmethod translate-to-dc ((expr conjunction))
  (let ((lhs (translate-to-dc (args1 expr)))
	(rhs (translate-to-dc (args2 expr))))
    (dp::mk-conjunction lhs rhs)))

(defmethod translate-to-dc ((expr disjunction))
  (let ((lhs (translate-to-dc (args1 expr)))
	(rhs (translate-to-dc (args2 expr))))
    (dp::mk-disjunction lhs rhs)))

(defmethod translate-to-dc ((expr negation))
  (let ((arg (translate-to-dc (args1 expr))))
    (dp::mk-negation arg)))







