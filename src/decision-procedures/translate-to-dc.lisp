;;modified from prover/translate-to-prove.
(in-package 'pvs)

(proclaim '(optimize (compilation-speed 0) (space 1)
		     (safety 1) (speed 3)
		     #+allegro (debug 1)))

(defvar *newdc* nil) ;;needs to be set to T to get new ground prover
;;(defvar *bindings* nil)

(defvar *translate-rewrite-rule* nil)


(defparameter *dc-interpreted-names*
  (list (mk-name '= nil '|equalities|)
	(mk-name '/= nil '|notequal|)
	(mk-name 'IMPLIES nil '|booleans|)
	(mk-name 'AND nil '|booleans|)
	(mk-name '& nil '|booleans|)
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
	))

(defvar *dc-interpretations*
  `((true . ,dp::*true*)
    (false . ,dp::*false*)
    (= . ,dp::*=*)
    (/= . ,dp::*nequal*)
    (< . ,dp::*lessp*)
    (<= . ,dp::*lesseqp*)
    (> . ,dp::*greaterp*)
    (>= . ,dp::*greatereqp*)
    (+ . ,dp::*plus*)
    (- . ,dp::*difference*) 
    (* . ,dp::*times*)
    (/ . ,dp::*divide*)
    (and . ,dp::*and*)
    (& . ,dp::*and*)
    (or . ,dp::*or*)
    (not . ,dp::*not*)
    (if . ,dp::*if*)
    (update . ,dp::*update*)))

(defvar *translate-to-dc-hash* (make-hash-table :hash-function 'pvs-sxhash
						:test 'tc-eq))
(defvar *dc-named-exprs* (make-hash-table :hash-function 'pvs-sxhash
					  :test 'tc-eq))
(defvar *dc-translate-id-counter* nil)
(newcounter *dc-translate-id-counter*)
(defvar *dc-translate-id-hash* (make-hash-table :hash-function 'pvs-sxhash
						:test 'tc-eq))
(defvar *prtype-hash* (make-hash-table))
(defvar *local-prtype-hash*  (make-hash-table))

(defun reset-translate-to-dc ()
  (clrhash *translate-to-dc-hash*)
  (clrhash *dc-translate-id-hash*)
  (clrhash *prtype-hash*)
  (clrhash *local-prtype-hash*)
  (clrhash *dc-named-exprs*)
  (newcounter *dc-translate-id-counter*)
  (setq *dc-interpretations*
	`((true . ,dp::*true*)
	  (false . ,dp::*false*)
	  (= . ,dp::*=*)
	  (/= . ,dp::*nequal*)
	  (< . ,dp::*lessp*)
	  (<= . ,dp::*lesseqp*)
	  (> . ,dp::*greaterp*)
	  (>= . ,dp::*greatereqp*)
	  (+ . ,dp::*plus*)
	  (- . ,dp::*difference*) 
	  (* . ,dp::*times*)
	  (/ . ,dp::*divide*)
	  (and . ,dp::*and*)
	  (& . ,dp::*and*)
	  (or . ,dp::*or*)
	  (not . ,dp::*not*)
	  (if . ,dp::*if*)
	  (update . ,dp::*update*))))

(defmethod interpreted? ((expr name-expr))
  (member expr (if *newdc*
		   *dc-interpreted-names*
		   *interpreted-names*)
	  :test #'(lambda (n i)
		    (and (same-id n i)
			 (module-instance n)
			 (eq (mod-id i)
			     (id (module-instance n)))))))

(defun interpretation (name)
  (or (cdr (assoc (id name)
		   (if *newdc*
		       *dc-interpretations*
		       *interpretations*)))
       (if *newdc*
	   (dp::mk-constant (id name))
	   (id name))))

(defmacro translate-to-ground (expr)
  `(if *newdc*
      (translate-to-dc ,expr)
      (translate-to-prove ,expr)))

(defmacro translate-with-new-hash (&rest body)
  `(if *newdc*
       (let ((*translate-to-dc-hash*
	      (make-hash-table  ;;NSH(2.5.95)
	       :hash-function 'pvs-sxhash ;;hash to pvs-hash
	       :test 'tc-eq)))
	 ,@body)
      (let ((*translate-to-prove-hash*
	      (make-hash-table  ;;NSH(2.5.95)
	       :hash-function 'pvs-sxhash ;;hash to pvs-hash
	       :test 'tc-eq)))
	 ,@body)))

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


;;; Translate into list representation for passing to the prover.
;;; This also sets up the global variables typealist, and inserts
;;; subtype information.

(defmethod translate-to-dc :around (obj)
  (let ((hashed-value (gethash obj *translate-to-dc-hash*)))
    (or hashed-value
	(when (and nil (typep obj 'name-expr)
		   (eq (id obj) '^))
	  (break))
	(let ((result (call-next-method)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (gethash obj *translate-to-dc-hash*) result)
	    (setf (gethash result *translate-from-dc-hash*) obj))
	  result))))


(defmethod translate-to-dc ((list list))
  (mapcar #'(lambda (l) (translate-to-dc l)) list))

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
	  (bpos (let ((id (dp::mk-constant
			   (makesym "*~a_~a*" (id expr) bpos))))
		  (add-to-local-prtype-hash id expr)
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
    (if dc-actuals
	(dp::mk-term (cons dp::*th-app*
			   (cons dc-base-name dc-actuals))
		     (dp::node-type dc-base-name))
	dc-base-name)))
	

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

(defun dc-unique-prover-name (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	(let* ((hash-list (list-of-decl-and-type-actuals expr))
	       (id-hash (gethash hash-list *dc-translate-id-hash*))
	       (dc-expr
		(or ;(when (eq (id expr) *se*) (break))
		    id-hash
		    (when (tc-eq expr *true*) dp::*true*)
		    (when (tc-eq expr *false*) dp::*false*)
		    (when (interpreted? expr)
		      (interpretation expr))
		    (let* ((type (type expr))
			   (range-type (if (typep type 'funtype)
					   (range type)
					   type))
			   (dc-range-type (dc-prover-type range-type))
			   (new-const (dp::mk-constant
				       (intern (format nil "~a_~a"
						 (id expr)
						 (funcall
						  *dc-translate-id-counter*)))
				       dc-range-type)))
		      (dc-add-to-reverse-prover-name new-const expr)
		      (dc-add-to-pvs-typealist new-const expr)
		      new-const))))
	   (unless id-hash
	     (setf (gethash hash-list *dc-translate-id-hash*)
		   dc-expr)
	     ;;(format t "~%adding ~a to typealist" (car newconst))
	     (add-to-prtype-hash dc-expr expr))
	   dc-expr))
	((typep expr 'field-decl)
	 (let* ((id-hash (gethash expr
				      *dc-translate-id-hash*))
		(newconst
		 (or id-hash
		     (dp::mk-constant
			    (intern (format nil "~a-~a"
				      (id expr)
				      (funcall
				       *dc-translate-id-counter*)))))))
	   (unless id-hash
	     (setf (gethash expr *dc-translate-id-hash*)
		   newconst))
	   newconst))
	(t (add-to-local-prtype-hash (id expr) expr)
	   (dc-add-to-reverse-prover-name (id expr) expr)
	   (dc-add-to-pvs-typealist (id expr) expr)
	   (if *translate-rewrite-rule*
	       (dp::mk-variable (id expr) (dc-prover-type (type expr)))
	       (dp::mk-constant (id expr) (dc-prover-type (type expr)))))))

;(defmethod normalize-name-expr-actuals ((expr name-expr))
;  (with-slots (resolutions) expr
;    (lcopy expr
;      'resolutions (normalize-name-expr-actuals resolutions))))
;
;(defmethod normalize-name-expr-actuals ((list list))
;  (let ((nlist (mapcar #'normalize-name-expr-actuals list)))
;    (if (equal list nlist) list nlist))) 
;
;(defmethod normalize-name-expr-actuals ((res resolution))
;  (with-slots (module-instance) res
;    (lcopy res
;      'module-instance (normalize-name-expr-actuals module-instance))))
;
;(defmethod normalize-name-expr-actuals ((mname modname))
;  (with-slots (actuals) mname
;    (lcopy mname
;      'actuals (normalize-name-expr-actuals actuals))))
;
;(defmethod normalize-name-expr-actuals ((act actual))
;  (with-slots (expr type-value) act
;    (if type-value
;	act
;	(lcopy act 'expr (pseudo-normalize expr)))))

;(defun get-subtype-coercion (expr)
;  (let ((stname (cdr (assoc (type expr) *subtype-names* :test #'tc-eq))))
;    (or stname
;	(let ((name (makesym "*~a*" (gentemp "subtype"))))
;	  (push (cons (type expr) name) *subtype-names*)
;	  name))))

	
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
  (let ((entry (gethash id *local-prtype-hash*)))
    (unless entry
      (setf (gethash id *local-prtype-hash*)
	    (dc-prover-type (type expr))))))

(defmethod translate-to-dc ((expr number-expr))
  (dp::mk-constant (number expr)))

(defmethod translate-to-dc ((expr record-expr))
  (translate-dc-assignments-to-record (assignments expr)
				      (find-supertype (type expr)))))

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
					(argument* argument)))))
      (translate-to-dc reduced-expr))))

(defmethod translate-to-dc ((expr if-expr))
  (if (eq (id (module-instance (resolution (operator expr))))
	  '|if_def|)
      (dp::mk-if-then-else
       (translate-to-dc (condition expr))
       (translate-to-dc (then-part expr))
       (translate-to-dc (else-part expr)))
      (call-next-method)))

;(defmethod translate-to-dc ((expr cases-expr))
;  (let ((name (gethash expr *dc-named-exprs*)))
;    (or name
;        (let ((newid (dp::mk-constant (gentemp))))
;          (add-to-prtype-hash newid nil (type expr))
;          (setf (gethash expr *dc-named-exprs*) newid)
;          newid))))

(defmethod translate-to-dc ((expr cases-expr))
  (translate-to-dc (translate-cases-to-if expr)))

(defmethod translate-to-dc ((expr let-expr))
  (with-slots (operator argument) expr
    (let ((reduced-expr (substit (expression operator)
			  (pairlis-args (bindings operator)
					(argument* argument)))))
      (translate-to-dc reduced-expr))))

;(defun translate-dc-args (arguments expected)
;  (if (eql (length expected)
;	   (length arguments));;no tuple mismatch
;      (translate-to-dc arguments)
;      (if (and (singleton? expected)
;	       (tupletype? expected))
;	  `(tupcons ,(translate-to-dc arguments))
;	  (if (singleton? arguments)
;	      (if (tuple-expr? (car arguments))
;		  (translate-to-dc
;		   (exprs (car arguments)))
;		  (if (tupletype?
;		       (type (car arguments)))
;		      (let ((translated-arg
;			     (translate-to-dc
;			      (car arguments))))
;			(loop for I from 0
;			      upto (1- (length expected))
;			      collect
;			      `(,(makesym "TUPSEL-~a"
;					  (or (prover-type
;					       (nth I (types (type
;							      (car arguments)))))
;					      ""))
;				,I
;				,translated-arg)))
;		      (translate-to-dc arguments)))
;	      (translate-to-dc arguments)))))

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
			  :test #'(lambda (x y) (eq x (id y))))))
      (dp::sigproject
       (dp::mk-term
	(list dp::*project* (dp::mk-constant pos);;might need to set prtype.
	      (translate-to-dc argument)))))))

;;NSH(5.17.94): Complicated code to deal with tuple mismatch
;;between domain of operator and arguments.
;; op(a1,..., an) if dom(type(op)) = [t1,...,tn] ==>
;;                      (op' (tupcons a1' .. an'))
;; op(a1), if dom(type(op)) = t1,...,tn ==> (op (tupsel 0 a1)...)

(defmethod translate-to-dc ((expr application))
  (with-slots (operator argument)
      expr
    (let* ((args (translate-to-dc (arguments expr))))
      (cond ((interpreted? operator)
	     (let ((op (interpretation operator))
		   )
	       (cond ((and (eq op dp::*difference*) (singleton? args))
		      (dp::mk-term (cons dp::*minus* args)))
		     ((eq op dp::*nequal*)
		      (dp::mk-term (list dp::*not*
					 (dp::mk-equality (car args)
							  (cadr args)))))
		     ((and (eq op dp::*not*)
			   (dp::negation-p (car args)))
		      (dp::arg 1 (car args)))
		     (*translate-rewrite-rule*
		      (dp::sigma (dp::mk-term (cons op args))
				 *init-dp-state*))
		     (t (dp::mk-term (cons op args))))))
	    ((and (or (not (typep operator 'name-expr))
		      (not (typep (declaration operator) 'const-decl)))
		  (or (function-non-functional? operator)
		      ;;(skolem-application? expr)
		      ))
	     (break))
	    (t (dc-mk-typed-term (cons (translate-to-dc operator) args)
				 (dc-prover-type (type expr))))))))

(defun dc-mk-typed-term (args dc-type)
  (let ((result (dp::mk-term args)))
    (setf (dp::node-initial-type result) dc-type)
    result))

(defmethod make-dc-op (op (te type-expr))
  (let* ((type (find-supertype te))
	 (name (makesym "APPLY-~d-~a"
			(length (domain-types type))
			(or (dc-prover-type (range type)) "")))
	 (prtype (dc-prover-type (range type))))
    (unless (or (not prtype)
		(assoc name typealist))
      (push (cons name prtype) typealist)
      (push name applysymlist))
    name))


;(defmethod interpreted? (expr)
;  (declare (ignore expr))
;  nil)
;
;(defmethod function-non-functional? (expr)
;  (declare (ignore expr))
;  nil)
;
;(defmethod function-non-functional? ((expr name-expr))
;  (and (typep (type expr) 'funtype)
;       (not (typep (declaration expr) 'var-decl))
;       (not (typep (range (type expr)) 'funtype)))
;  nil)
;
;
;(defmethod interpreted? ((expr name-expr))
;  (member expr *interpreted-names*
;	  :test #'(lambda (n i)
;		    (and (same-id n i)
;			 (module-instance n)
;			 (eq (mod-id i)
;			     (id (module-instance n)))))))
;
;(defmethod interpretation ((expr name-expr))
;  (or (cdr (assoc (id expr) *interpretations*))
;      (id expr)))
;
;(defmethod operator ((expr lambda-expr))
;  'lambda)
;
;(defmethod operator ((expr forall-expr))
;  'forall)
;
;(defmethod operator ((expr exists-expr))
;  'exists)

;(defvar *integer-pred* nil)

;(defun top-translate-to-dc (expr)
;  (let ((*bindings* nil)
;	(*generate-tccs* 'NONE))
;    (cond ((typep *translate-to-dc-hash* 'ht)
;	   (setq *integer-pred* (translate-to-dc (predicate *integer*)))
;	   (translate-to-dc (unit-derecognize expr)))
;	  (t (let ((*translate-to-dc-hash*
;		    (make-hash-table  ;;NSH(2.5.95)
;				    :hash-functiono #'pvs-sxhash ;;hash to hash
;				    :test #'tc-eq)))
;	       (unless *integer-pred*
;		 (setq *integer-pred*
;		       (when *integer*
;			 (translate-to-dc (predicate *integer*)))))
;	       (translate-to-dc (unit-derecognize expr)))))))


(defmethod translate-to-dc ((expr binding-expr))
  (let ((*bindings* (append (bindings expr) *bindings*)))
    ;;(NSH:4-5-91)(9.14.94)
    (dp::mk-term
	(list
	 (dp::mk-constant
	  (intern (format nil "~a_~a" (operator expr)
			  (length (bindings expr)))))
	 (dp::mk-constant (length (bindings expr)))
	 (if (connective-occurs? (expression expr))
	     (let ((name (gethash expr *dc-named-exprs*)))
	       (or name;;NSH(11.4.95): Fixes unsoundness caused
		   ;;by translating (LAMBDA : IF B ...) to (LAMBDA 1: CC)
		   ;;by generating (LAMBDA 1 : CC(*1*)) instead.
		   ;;Note that two tc-eq such LAMBDAs will generate
		   ;;different translations so that ASSERT might miss some
		   ;;syntactic equalities when there are IFs in LAMBDAs.
		   (let* ((newid (dp::mk-constant (gentemp)))
			  (type (type expr))
			  (prtype (dc-prover-type type))
			  (freevars (freevars (expression expr))))
		     ;;NSH(2.5.96) (freevars (expr..)) instead of
		     ;;(freevars expr).
		     (cond
		      (freevars
		       (let* ((tr-freevars (translate-to-dc freevars))
			      (apform (dp::mk-term (cons (dp::mk-constant newid)
						     tr-freevars))))
			 (setf (gethash expr *dc-named-exprs*) apform)
			 apform))
		      (t
		       (add-to-prtype-hash newid (type expr))
		       (setf (gethash expr *dc-named-exprs*)  newid)
		       newid)))))
	     (translate-to-dc (expression expr)))))))




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
	      (let ((old-args (dp::funargs trbasis)))
		(setf (nth position old-args)
		      new-value)
		old-args)
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
      (dp::mk-term (list dp::*update*
		     trbasis
		     (dp::mk-constant (1- (number (caar args))))
		     (let* ((ntrbasis-type
			     (find-supertype 
			      (nth (1- (number (caar args)))
				   (types type))))
			    (ntrbasis
			     (make-dc-projection-application
			      ntrbasis-type (number (caar args)) trbasis)))
		       (translate-dc-assign-args (cdr args)
						 value
						 ntrbasis
						 ntrbasis-type)))))

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
    ;;(setf (dp::node-type appl) field-accessor-type)
    appl))

(defun make-dc-projection-application (type number expr)
  (let ((appl (dp::sigproject
	       (dp::mk-term `(,dp::*project*
			      ,(dp::mk-constant (1- number)) ,expr)))))
    ;;(setf (dp::node-type appl) (prover-type type))
    appl))

(defun make-dc-assign-application (fun-type expr args)
  (let ((appl (dp::mk-term (list expr args))))
    ;;(setf (dp::node-type appl) fun-type)
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
	((member type `(,*integer* ,*naturalnumber*) :test #'tc-eq)
	 dp::*integer*)
	((tc-eq type *number*) dp::*number*)
;;	((typep type 'tupletype) 'tuple)
;;	((typep type 'arraytype) 'array)
	((typep type 'funtype) dp::*functional*)
	((typep type 'recordtype) dp::*array* ;'tuple
	 )
	((typep type 'subtype)
	 (dc-prover-type (supertype type)))
	;;((print-name type))
	;;((slot-exists-p (name type) 'id) (id (name type)))
	;;(error "No name available to use in prover-type")
	))

(defmethod dc-prover-type ((te dep-binding))
  (dc-prover-type (type te)))

;(defmethod make-apply-name ((te type-expr))
;  (let* ((type (find-supertype te))
;	 (name (makesym "APPLY-~d-~a"
;			(length (domain-types type))
;			(or (prover-type (range type)) "")))
;	 (prtype (prover-type (range type))))
;    (unless (or (not prtype)
;		(assoc name typealist))
;      (push (cons name prtype) typealist)
;      (push name applysymlist))
;    name))
;
;(defmethod prover-type ((type type-expr))
;  (cond ((tc-eq type *boolean*) 'bool)
;	((member type `(,*integer* ,*naturalnumber*) :test #'tc-eq) 'integer)
;	((tc-eq type *number*) 'number)
;;;	((typep type 'tupletype) 'tuple)
;;;	((typep type 'arraytype) 'array)
;	((typep type 'funtype) 'functional)
;	((typep type 'recordtype) 'array ;'tuple
;	 )
;	((typep type 'subtype)
;	 (prover-type (supertype type)))
;	;;((print-name type))
;	;;((slot-exists-p (name type) 'id) (id (name type)))
;	;;(error "No name available to use in prover-type")
;	))

;(defmethod prover-type ((te dep-binding))
;  (prover-type (type te)))
;
;
;(defparameter *interpreted-names*
;  (list (mk-name '= nil '|equalities|)
;	(mk-name '/= nil '|notequal|)
;	(mk-name 'IMPLIES nil '|booleans|)
;	(mk-name 'AND nil '|booleans|)
;	(mk-name 'OR nil '|booleans|)
;	(mk-name 'NOT nil '|booleans|)
;	(mk-name '+ nil '|reals|)
;	(mk-name '- nil '|reals|)
;	(mk-name '* nil '|reals|)
;	(mk-name '/ nil '|reals|)
;	(mk-name '< nil '|reals|)
;	(mk-name '<= nil '|reals|)
;	(mk-name '> nil '|reals|)
;	(mk-name '>= nil '|reals|)
;;	(mk-name '+ nil '|reals|)
;;	(mk-name '- nil '|reals|)
;;	(mk-name '* nil '|reals|)
;;	(mk-name '/ nil '|reals|)
;;	(mk-name '+ nil '|rationals|)
;;	(mk-name '- nil '|rationals|)
;;	(mk-name '* nil '|rationals|)
;;	(mk-name '/ nil '|rationals|)
;;	(mk-name '+ nil '|integers|)
;;	(mk-name '- nil '|integers|)
;;	(mk-name '* nil '|integers|)
;;	(mk-name '/ nil '|integers|)
;;	(mk-name '+ nil '|naturalnumbers|)
;;	(mk-name '- nil '|naturalnumbers|)
;;	(mk-name '* nil '|naturalnumbers|)
;;	(mk-name '/ nil '|naturalnumbers|)
;	))
