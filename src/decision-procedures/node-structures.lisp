(in-package dp)

(eval-when (eval compile load)
  (defvar *develop* t)
  (defvar *node-type* 'array)
  (defvar *argument-type* 'list))

(declaim (special *dp-changed*))
(defvar *dp-changed* nil)
(declaim (special *contradiction*))
(defvar *contradiction* nil)

(defvar *max-node-index* 0)


(eval-when (eval compile load)
  (defun extend-with-named-type (name-and-options)
    (if (consp name-and-options)
	(cons (car name-and-options)
	      (append `((:type ,*node-type*) (:named))
		      (cdr name-and-options)))
	(cons name-and-options
	      `((:type ,*node-type*) (:named)))))

  (defun name-from-name-and-options (name-and-options)
    (if (consp name-and-options)
	(car name-and-options)
	name-and-options))
  
  (defun across-or-in ()
    (if (eq *argument-type* 'list)
	'in
	'across)))

(defmacro defdpstruct (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS)
  (if *develop*
      `(defstruct ,NAME-AND-OPTIONS ,@SLOT-DESCRIPTIONS)
      `(progn
	 (defstruct ,(extend-with-named-type NAME-AND-OPTIONS)
	   ,@SLOT-DESCRIPTIONS)
	 (deftype ,(name-from-name-and-options NAME-AND-OPTIONS)
	   () ,(if (eq *node-type* 'list)
		   ''list
		   ''simple-vector)))))
  

(defdpstruct node
  (interpreted? nil :type boolean)
  (initial-type nil :type symbol)
  (type initial-type :type symbol)
  (constructor? nil :type boolean)
  (external-info nil :type list)
  (index :type integer)
  (sxhash nil :type (integer 0 65535)))

(defdpstruct (leaf (:include node)
		 (:print-function
		  (lambda (l s k)
		    (declare (ignore k))
		    (format s "~A" (leaf-id l)))))
  id)

(defun print-leaf (l s)
  (format s "~A" (leaf-id l)))

(defdpstruct (constant (:include leaf)))

(defdpstruct (dp-variable (:include leaf)))

(defdpstruct (application (:include node)
			(:print-function
			 (lambda (a s k)
			   (declare (ignore k))
			   (format s "~A"
			     (if (eq *argument-type* 'list)
				 (application-arguments a)
				 (array-to-list (application-arguments a)))))))
  (arguments nil :type (or simple-vector list)))

(defun print-application (a s)
  (format s "~A" (array-to-list (application-arguments a))))

(defmacro defdpfield (name)
  (let ((accessor (intern (concatenate 'string "NODE-" (string name))))
	(setter (intern (concatenate 'string ".INV-NODE-" (string name))))
	(key (intern name :keyword)))
    `(progn
       (defun ,accessor (node) (cdr (assoc ,key (node-external-info node))))
       (progn (defun ,setter (node value)
		(let* ((external-info (node-external-info node))
		       (item (assoc ,key external-info)))
		  (if item
		      (setf (cdr item) value)
		      (setf (node-external-info node)
			    (acons ,key value external-info)))))
	      (defsetf ,accessor ,setter)))))

(defun array-to-list (array)
  (declare (type simple-vector array))
  (loop for i from 0 below (length array)
	collect (svref array i)))

(defun node-to-list (node)
  (cond
   ((leaf-p node)
    (leaf-id node))
   (t (arguments-to-list (application-arguments node)))))

(defun arguments-to-list (args)
  (if (eq *argument-type* 'list)
      (mapcar #'node-to-list args)
      (loop for a across args
	    collect (node-to-list a))))

(defun equal-array (args1 args2)
  (or (eq args1 args2)
      (when (and (numberp args1) (numberp args2))
	(= args1 args2))
      (and (arrayp args1)
	   (arrayp args2)
	   (let ((l1 (length (the simple-vector args1)))
		 (l2 (length (the simple-vector args2))))
	     (and (= l1 l2)
		  (loop for i from 0 below (the fixnum l1)
			always (eq (svref (the simple-vector args1)
					  (the fixnum i))
				   (svref (the simple-vector args2)
					  (the fixnum i)))))))))

(defun equal-list (args1 args2)
  (or (eq args1 args2)
      (when (and (numberp args1) (numberp args2))
	(= args1 args2))
      (and (consp args1)
	   (consp args2)
	   (eq (car args1) (car args2))
	   (equal-list (cdr args1) (cdr args2)))))

(defun dp-eq-sxhash (term)
  (if (node-p term)
      (node-sxhash (the node term))
      (sxhash term)))

(defun dp-sxhash (args)
  (let ((result (cond
		 ((arrayp args)
		  (mod
		   (loop for a across (the simple-vector args)
			 sum (dp-eq-sxhash a))
		   65536))
		 ((listp args)
		  (mod
		   (loop for a in (the list args)
			 sum (dp-eq-sxhash a))
		   65536))
		 ((atom args) (dp-eq-sxhash args))
		 (t (break)
		  (dp-eq-sxhash args)))))
    result))

(defvar *pvs-hash* nil)


(eval-when (compile load eval)
  ;(setq pvs::*fast-hash-copy* nil)
  #+(or allegro-v4.3 allegro-v5.0) (setq *pvs-hash* nil)
  #-(or allegro-v4.3 allegro-v5.0) (setq *pvs-hash* t))

(defmacro dp-make-hash-table (&rest args)
  (if *pvs-hash*
      `(make-hash-table ,@(subst ':hashfn ':hash-function args))
      `(make-hash-table ,@args)))

(defmacro dp-make-eq-hash-table ()
  (if *pvs-hash*
      '(pvs::make-pvs-hash-table :test 'eq :hashfn 'dp-eq-sxhash)
      '(make-hash-table :test #'eq :hash-function 'dp-eq-sxhash)))

(defmacro dp-gethash (key hash-table &optional default)
  (if *pvs-hash*
      `(pvs::pvs-gethash ,key ,hash-table ,default)
      `(gethash ,key ,hash-table ,default)))

(defmacro dp-clrhash (hash-table)
  (if *pvs-hash*
      `(pvs::pvs-clrhash ,hash-table)
      `(clrhash ,hash-table)))

(defvar *term-hash*)

(eval-when (load eval)
  (setq *term-hash* (dp-make-hash-table :test (if (eq *argument-type* 'list)
						  'equal-list
						  'equal-array)
					:hash-function 'dp-sxhash)))

(defvar *mk-term-adduse* t)

(defun mk-arg-array (arguments)
  (make-array (length arguments)
	      :element-type 'node
	      :initial-contents arguments))

(defun mk-term (arguments &optional (type nil))
  (if (eq *argument-type* 'list)
      (mk-term-list arguments type)
      (let ((arg-array (mk-arg-array arguments)))
	(mk-term-array arg-array type))))

(defun mk-term-array (arg-array &optional (type nil))
  (let ((hashed-term (dp-gethash arg-array *term-hash*)))
    (if hashed-term
	(if type
	    (progn (setf (node-type hashed-term) type)
		   hashed-term)
	    hashed-term)
	(setf (dp-gethash arg-array *term-hash*)
	      (mk-term* arg-array type)))))

(defun mk-term-list (arg-list &optional (type nil))
  (let ((hashed-term (dp-gethash arg-list *term-hash*)))
    (if hashed-term
	(if type
	    (progn (setf (node-type hashed-term) type)
		   hashed-term)
	    hashed-term)
	(setf (dp-gethash arg-list *term-hash*)
	      (mk-term* arg-list type)))))

(defun mk-term* (arguments type)
  (let ((new-node (make-application :sxhash (dp-sxhash arguments)
				    :index (incf *max-node-index*)
				    :initial-type type
				    :arguments arguments)))
    new-node))

(defun mk-constant (id &optional (type nil))
  (let ((hashed-constant (dp-gethash id *term-hash*)))
    (if hashed-constant
	(if type
	    (progn (setf (node-type hashed-constant) type)
		   hashed-constant)
	    hashed-constant)
	(setf (dp-gethash id *term-hash*)
	      (mk-constant* id type)))))

(defun mk-constant* (id type)
  (let ((new-constant (make-constant :sxhash (dp-sxhash id)
				     :index (incf *max-node-index*)
				     :initial-type type
				     :id id)))
    new-constant))

(defun mk-new-constant (id type)
  (mk-constant* id type))

(defun mk-variable (id &optional (type nil))
  (let ((hashed-variable (dp-gethash id *term-hash*)))
    (if hashed-variable
	(if type
	    (progn (setf (node-type hashed-variable) type)
		   hashed-variable)
	    hashed-variable)
	(setf (dp-gethash id *term-hash*)
	      (mk-variable* id type)))))

(defun mk-variable* (id type)
  (let ((new-variable (make-dp-variable :sxhash (dp-sxhash id)
					:index (incf *max-node-index*)
					:initial-type type
					:id id)))
    new-variable))

(defun mk-interpreted-constant (sym &optional type)
  (let ((result (mk-constant sym type)))
    (setf (node-interpreted? result) t)
    result))

(defun lisp-to-dp (list)
  (cond
   ((consp list)
    (mk-term (loop for l in list collect (lisp-to-dp l))))
   (t (mk-constant list))))

(defmacro arity (application)
  `(1- (length (the ,*argument-type* (application-arguments ,application)))))

(defmacro arg (num application)
  (if (eq *argument-type* 'list)
      `(nth (the (integer 0 100) ,num)
	    (the list
	      (application-arguments (the application ,application))))
      `(svref (the ,*argument-type*
		(application-arguments (the application ,application)))
	      (the (integer 0 100) ,num))))

(defmacro map-args (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the ,*argument-type*
			      (application-arguments ,application))
	   for arg ,(across-or-in) (the ,*argument-type* arguments)
	   do (funcall ,fn arg .,more-args)))

(defmacro map-args-args (fn application1 application2 &rest more-args)
  `(declare (type application ,application1)
	    (type application ,application2)
	    (type function ,fn))
  `(loop with arguments1 = (the ,*argument-type*
			     (application-arguments ,application1))
	 with arguments2 = (the ,*argument-type*
			     (application-arguments ,application2))
	 for arg1 ,(across-or-in) (the ,*argument-type* arguments1)
	 for arg2 ,(across-or-in) (the ,*argument-type* arguments2)
	 do (funcall ,fn arg1 arg2 .,more-args)))

(defmacro map-args-list (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the ,*argument-type*
			   (application-arguments ,application))
	 for arg ,(across-or-in) (the simple-vector arguments)
	 collect (funcall ,fn arg .,more-args)))

(defmacro map-args-array (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the ,*argument-type*
			   (application-arguments ,application))
	 with length = (length (the ,*argument-type* arguments))
	 with array = (make-array (the fixnum length) :element-type 'node)
	 for i from 0 below (the fixnum length)
	 do (setf (svref (the simple-vector array)
			 (the fixnum i))
		  (funcall ,fn ,(if (eq *argument-type* 'list)
				    `(nth (the fixnum i)
					  (the list arguments))
				    `(svref (the simple-vector arguments)
					    (the fixnum i)))
			   .,more-args))
			 
	 finally (return array)))

(defun expand-map-funargs (fn application more-args)
  (if (eq *argument-type* 'list)
      `(loop with arguments = (the list
				(application-arguments ,application))
	     for arg in (cdr arguments)
	     do (funcall ,fn arg
			 .,more-args))
      `(loop with arguments = (the simple-vector
				(application-arguments ,application))
	     for i from 1 to (arity ,application)
	     do (funcall ,fn (svref (the simple-vector arguments) i)
			 .,more-args))))

(defmacro map-funargs (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs fn application more-args))

(defun expand-map-funargs-list (fn application more-args)
  (if (eq *argument-type* 'list)
      `(loop with arguments = (the list
				(application-arguments ,application))
	     for arg in (cdr arguments)
	     collect (funcall ,fn arg
		       .,more-args))
      `(loop with arguments = (the simple-vector
				(application-arguments ,application))
	     for i from 1 to (arity ,application)
	     collect (funcall ,fn (svref (the simple-vector arguments) i)
			    .,more-args))))

(defmacro map-funargs-list (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs-list fn application more-args))

(defun expand-map-funargs-array (fn application more-args)
  (if (eq *argument-type* 'list)
      (expand-map-funargs-array-for-list fn application more-args)
      (expand-map-funargs-array-for-array fn application more-args)))

(defun expand-map-funargs-array-for-list (fn application more-args)
  `(loop with arguments = (the ,*argument-type*
			    (application-arguments ,application))
	 with length = (length (the list arguments))
	 with array = (make-array (the fixnum length) :element-type 'node)
	 for i from 1 below (the fixnum length)
	 for arg in (cdr (the list arguments))
	 do (setf (svref (the simple-vector array)
			 (the fixnum i))
		  (funcall ,fn arg
			   .,more-args))
	 finally (setf (svref (the simple-vector array)
			      0)
		       (car arguments))
	 (return array)))

(defun expand-map-funargs-array-for-array (fn application more-args)
  `(loop with arguments = (the ,*argument-type*
			    (application-arguments ,application))
	 with length = (length (the simple-vector arguments))
	 with array = (make-array (the fixnum length) :element-type 'node)
	 for i from 1 below (the fixnum length)
	 do (setf (svref (the simple-vector array)
			 (the fixnum i))
		  (funcall ,fn (svref (the simple-vector arguments)
				      (the fixnum i))
			   .,more-args))			 
	 finally (setf (svref (the simple-vector array)
			      0)
		       (svref (the simple-vector arguments)
			      0))
	 (return array)))

(defmacro map-funargs-array (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs-array fn application more-args))

(defmacro funsym (application)
  `(arg 0 (the application ,application)))

(defun funargs (application)
  (declare (type application application))
  (if (eq *argument-type* 'list)
      (cdr (application-arguments application))
      (loop with arguments = (the simple-vector
			       (application-arguments application))
	    for i from 1 below (length (the simple-vector arguments))
	    collect (svref (the simple-vector arguments) i))))


(defun congruence-class-of (term cs)
  (let ((repr (dp-find term cs))
	(*terms* nil))
    (maphash #'(lambda (key val)
		 (declare (ignore key))
		 (when (eq (dp-find val cs) repr)
		   (setf *terms* (adjoin val *terms*))))
	     *term-hash*)
    *terms*))

(defun mk-predicate-sym (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result) 'predicate)
    (setf (node-interpreted? result) t)
    result))

(defvar *=* (mk-predicate-sym '=))

(defmacro mk-equality (t1 t2)
  `(mk-term (list *=* ,t1 ,t2)))

(defmacro lhs (equality)
  `(arg 1 ,equality))

(defmacro rhs (equality)
  `(arg 2 ,equality))

(defun equality-p (term)
  (and (application-p term)
       (eq (funsym term) *=*)))

(defvar *true* (mk-constant 'TRUE))
(defvar *false* (mk-constant 'FALSE))

(defmacro true-p (term)
  `(eq ,term *true*))

(defmacro false-p (term)
  `(eq ,term *false*))

(defun mk-arith-operator (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'arith-op)
    (setf (node-interpreted? result) t)
    result))

(defun arith-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'arith-op))

(defvar *plus* (mk-arith-operator 'plus))
(defvar *times* (mk-arith-operator 'times))
(defvar *divide* (mk-arith-operator 'divide))
(defvar *minus* (mk-arith-operator 'minus))
(defvar *difference* (mk-arith-operator 'difference))
(defvar *sup* (mk-arith-operator 'sup))
(defvar *inf* (mk-arith-operator 'inf))
(defvar *floor* (mk-arith-operator 'floor))
(defvar *ceiling* (mk-arith-operator 'ceiling))

;;interpreted arithmetic function symbols
(defvar *arithfuns*
  (list *plus* *times* *divide* *minus* *difference* *sup* *inf*))

(defun mk-arith-pred (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'arith-pred)
    (setf (node-interpreted? result) t)
    result))

(defvar *lesseqp* (mk-arith-pred 'lesseqp))
(defvar *lessp* (mk-arith-pred 'lessp))
(defvar *greatereqp* (mk-arith-pred 'greatereqp))
(defvar *greaterp* (mk-arith-pred 'greaterp))

;;interpreted arithmetic predicate symbols
(defvar *arithpreds*
  (list *lesseqp* *lessp* *greaterp* *greatereqp*))

;;is the top fun. symb. of term arithmetic.
(defun arith-p (term)
  (declare (type node term))
  (cond
   ((application-p term)
    (arith-op-p (the node (funsym (the application term)))))
   ((constant-p term)
    (numberp (constant-id (the constant term))))
   (t nil)))

(defvar *number-type-preds* '(integer-pred rational-pred real-pred))

(defun number-type-p (term)
  (and (application-p term)
       (member (node-initial-type (funsym term)) *number-type-preds*
	       :test #'eq)))

(defvar *not* (mk-predicate-sym 'not))
(defvar *nequal* (mk-predicate-sym 'nequal))

(defvar *and* (mk-predicate-sym 'and))
(defvar *or* (mk-predicate-sym 'or))


(defvar *preds*
  (list *not* *and* *or* *nequal*
	*lesseqp* *lessp* *greaterp* *greatereqp*))

(defun bool-p (term)
  (declare (type node term))
  (cond
   ((application-p term)
    (or (eq (node-type (the node (funsym (the application term))))
	    'predicate)
	(eq (node-type (the node (funsym (the application term))))
	    'arith-pred)))
   ((constant-p term)
    (or (eq term *true*)
	(eq term *false*)))
   (t nil)))

(defun arith-pred-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'arith-pred))

(defun arith-bool-p (term)
  (declare (type node term))
  (and (application-p term)
       (arith-pred-p (the node (funsym (the application term))))))

(defun negation-p (term)
  (declare (type node term))
  (and (application-p term)
       (eq (funsym (the application term)) *not*)))

(defun mk-negation (term)
  (mk-term (list *not* term)))

(defun neq-p (term)
  (and (equality-p term)
       (eq (rhs term) *false*)
       (equality-p (lhs term))))

(defun mk-nequality (lhs rhs)
  (mk-equality (mk-equality lhs rhs) *false*))

(defun nequal-p (term)
  (declare (type node term))
  (and (application-p term)
       (eq (funsym (the application term)) *nequal*)))

(defvar *if* (mk-interpreted-constant 'if))

(defun mk-if-then-else (cond then else)
  (mk-term (list *IF* cond then else)))

(defun if-p (term)
  (and (application-p term)
       (eq (funsym term) *if*)))

(defun if-cond (term)
  (arg 1 term))

(defun if-then (term)
  (arg 2 term))

(defun if-else (term)
  (arg 3 term))

(defun and-p (term)
  (and (application-p term)
       (eq (funsym term) *and*)))

(defun or-p (term)
  (and (application-p term)
       (eq (funsym term) *or*)))

(defun dp-numberp (term)
  (declare (type node term))
  (and (constant-p term)
       (numberp (constant-id (the constant term)))))

(defun dp-integerp (term)
  (declare (type node term))
  (and (constant-p term)
       (integerp (constant-id (the constant term)))))

(defun mk-dp-number (num)
  (mk-constant num))

(defvar *zero* (mk-dp-number 0))
(defvar *one* (mk-dp-number 1))
(defvar *neg-one* (mk-dp-number -1))

(defmacro dp-zerop (term)
  `(eq ,term *zero*))

(defmacro dp-onep (term)
  `(eq ,term *one*))

(defmacro dp-minusp (const)
  `(minusp (constant-id (the constant ,const))))

(defun mk-array-operator (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'array-op)
    (setf (node-interpreted? result) t)
    result))

(defun array-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'array-op))

(defvar *update* (mk-array-operator 'update))

(defvar *tuple* (mk-interpreted-constant 'tuple 'tuple-op))

(defun tuple-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'tuple-op))

(defvar *record* (mk-interpreted-constant 'record 'record-op))

(defun record-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'record-op))

(defvar *project* (mk-interpreted-constant 'project 'project-op))

(defun project-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'project-op))

(defvar *th-app* (mk-interpreted-constant 'th-app 'th-app-op))

(defun th-app-op-p (constant)
  (declare (type node constant))
  (eq (node-type constant) 'th-app-op))

(defun th-app-p (term)
  (and (application-p term)
       (th-app-op-p (funsym term))))

(defvar *print-polyhedron* nil)

(defdpstruct (polyhedral-structure
	      (:print-function
	       (lambda (ps s k)
		 (declare (ignore k))
		 (if *print-polyhedron*
		     (domain_print (polyhedral-structure-polyhedron ps))
		     (format s "<~A slack variables, ~A dimension>"
		       (polyhedral-structure-ineq-var-count ps)
		       (polyhedron-dimension
			(polyhedral-structure-polyhedron ps)))))))
  (max-vars *max-ineq-vars* :type fixnum)
  (max-rays *max-rays* :type fixnum)
  (ineq-var-count 0 :type fixnum)
  (projection-matrix (initial-projection-matrix))
  (ineq-var-to-index-hash (dp-make-eq-hash-table)
			  :type hash-table)
  (ineq-var-index-array (make-ineq-var-index-array *max-ineq-vars*)
			:type array)
  (epsilon-poly (initial-epsilon-poly) :type integer)
  (polyhedron (universal-polyhedral-domain) :type integer)
  (input-eqns nil :type list)
  (input-neqs nil :type list)
  (aux-polyhedrons nil :type list)
  (aux-input-eqns nil :type list)
  (equalities nil :type list))

(defun copy-hash-table (new-hash-table old-hash-table)
  (maphash #'(lambda (key val)
	       (setf (gethash key new-hash-table)
		     val))
	   old-hash-table)
  new-hash-table)

(defun copy-array (new-array old-array)
  (simple-copy-array new-array old-array))

(defun simple-copy-array (new-array old-array)
  (let* ((new-array-size (array-total-size new-array))
	 (old-array-size (array-total-size old-array))
	 (adjusted-new-array (if (< new-array-size old-array-size)
				 (make-array  old-array-size)
				 new-array)))
    (loop for i from 0 below old-array-size
	  do
	  (setf (svref adjusted-new-array i)
		(svref old-array i)))
    adjusted-new-array))

(defun general-copy-array (new-array old-array)
  (let* ((new-array-size (array-total-size new-array))
	 (old-array-size (array-total-size old-array))
	 (adjusted-new-array (if (< new-array-size old-array-size)
				 (adjust-array new-array old-array-size)
				 new-array)))
    (loop for i from 0 below old-array-size
	  do
	  (setf (row-major-aref adjusted-new-array i)
		(row-major-aref old-array i)))
    adjusted-new-array))

(defun copy-latest-polyhedral-structure (new-poly-s old-poly-s)
  (setf (polyhedral-structure-max-vars new-poly-s)
	(polyhedral-structure-max-vars old-poly-s))
  (setf (polyhedral-structure-max-rays new-poly-s)
	(polyhedral-structure-max-rays old-poly-s))
  (setf (polyhedral-structure-ineq-var-count new-poly-s)
	(polyhedral-structure-ineq-var-count old-poly-s))
  (setf (polyhedral-structure-projection-matrix new-poly-s)
	(polyhedral-structure-projection-matrix old-poly-s))
  (copy-hash-table (polyhedral-structure-ineq-var-to-index-hash new-poly-s)
		   (polyhedral-structure-ineq-var-to-index-hash old-poly-s))
  (setf (polyhedral-structure-ineq-var-index-array new-poly-s)
	(copy-array (polyhedral-structure-ineq-var-index-array new-poly-s)
		    (polyhedral-structure-ineq-var-index-array old-poly-s)))
  (setf (polyhedral-structure-epsilon-poly new-poly-s)
	(polyhedral-structure-epsilon-poly old-poly-s))
  (setf (polyhedral-structure-polyhedron new-poly-s)
	(polyhedral-structure-polyhedron old-poly-s))
  (setf (polyhedral-structure-aux-polyhedrons new-poly-s)
	(polyhedral-structure-aux-polyhedrons old-poly-s))
  (setf (polyhedral-structure-input-eqns new-poly-s)
	(polyhedral-structure-input-eqns old-poly-s))
  (setf (polyhedral-structure-aux-input-eqns new-poly-s)
	(polyhedral-structure-aux-input-eqns old-poly-s))
  (setf (polyhedral-structure-equalities new-poly-s)
	(polyhedral-structure-equalities old-poly-s))
  ;(break)
  new-poly-s)

(defdpstruct (fourier-motzkin
	      (:print-function
	       (lambda (fm s k)
		 (declare (ignore k))
		 (format s "<~A inequalities, ~A equalities>"
		   (length (fourier-motzkin-inequalities fm))
		   (length (fourier-motzkin-equalities fm))))))
  (max-vars *max-ineq-vars* :type fixnum)
  (ineq-var-count 0 :type fixnum)
  (ineq-var-to-index-hash (dp-make-eq-hash-table)
			  :type hash-table)
  (ineq-var-index-array (make-ineq-var-index-array *max-ineq-vars*)
			:type array)
  (ineq-vars nil :type list)
  (use (dp-make-eq-hash-table) :type hash-table)
  (input-eqns nil :type list)
  (input-neqs nil :type list)
  (inequalities nil :type list)
  (equalities nil :type list))

(defun copy-fourier-motzkin (new-f-m old-f-m)
  (setf (fourier-motzkin-max-vars new-f-m)
	(fourier-motzkin-max-vars old-f-m))
  (setf (fourier-motzkin-ineq-var-count new-f-m)
	(fourier-motzkin-ineq-var-count old-f-m))
  (copy-hash-table (fourier-motzkin-ineq-var-to-index-hash new-f-m)
		   (fourier-motzkin-ineq-var-to-index-hash old-f-m))
  (setf (fourier-motzkin-ineq-var-index-array new-f-m)
	(copy-array (fourier-motzkin-ineq-var-index-array new-f-m)
		    (fourier-motzkin-ineq-var-index-array old-f-m)))
  (setf (fourier-motzkin-input-eqns new-f-m)
	(fourier-motzkin-input-eqns old-f-m))
  (setf (fourier-motzkin-inequalities new-f-m)
	(fourier-motzkin-inequalities old-f-m))
  (setf (fourier-motzkin-equalities new-f-m)
	(fourier-motzkin-equalities old-f-m))
  ;(break)
  new-f-m)

(defun initial-fourier-motzkin ()
  (make-fourier-motzkin))

(defun clr-fourier-motzkin (f-m)
  (clrhash (fourier-motzkin-ineq-var-to-index-hash f-m))
  (setf (fourier-motzkin-input-eqns f-m) nil)
  (setf (fourier-motzkin-inequalities f-m) nil)
  (setf (fourier-motzkin-equalities f-m) nil)
  ;(break)
  f-m)

(defdpstruct (rewrite-rules
	      (:print-function
	       (lambda (rr s k)
		 (declare (ignore k))
		 (format s "<~D Rewrite rules>"
		   (+ (length (rewrite-rules-rules rr))
		      (length (rewrite-rules-rules! rr)))))))
  (rules nil)
  (rules! nil)
  (hash (dp-make-eq-hash-table))
  (index-hash (dp-make-eq-hash-table)))

(defun copy-rewrite-rules-and-hash (new-rewrite-rules old-rewrite-rules)
  (setf (rewrite-rules-rules new-rewrite-rules)
	(rewrite-rules-rules old-rewrite-rules))
  (setf (rewrite-rules-rules! new-rewrite-rules)
	(rewrite-rules-rules! old-rewrite-rules))
  (copy-hash-table (rewrite-rules-index-hash new-rewrite-rules)
		   (rewrite-rules-index-hash old-rewrite-rules))
  new-rewrite-rules)

(defdpstruct (forward-chains)
  (orig-rules nil :type list)
  (partial-rules nil :type list)
  (pred-use (dp-make-eq-hash-table) :type hash-table)
  (term-use (dp-make-eq-hash-table) :type hash-table))


(defvar *initial-forward-chains* (make-forward-chains))

(defun initial-forward-chains ()
  (make-forward-chains))

(defun clr-forward-chains (fcs)
  (setf (forward-chains-orig-rules fcs) nil)
  (setf (forward-chains-partial-rules fcs) nil)
  (clrhash (forward-chains-pred-use fcs))
  (clrhash (forward-chains-term-use fcs))
  fcs)

(defun copy-forward-chains-and-hash (new-forward-chains old-forward-chains)
  (setf (forward-chains-orig-rules new-forward-chains)
	(forward-chains-orig-rules old-forward-chains))
  (setf (forward-chains-partial-rules new-forward-chains)
	(forward-chains-partial-rules old-forward-chains))
  ;(copy-hash-table (forward-chains-hash new-forward-chains)
;		   (forward-chains-hash old-forward-chains))
  new-forward-chains)

(defdpstruct (cong-state*
	      (:print-function
	       (lambda (cs* s k)
		 (declare (ignore k))
		 (format s "<~D: ~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type, ~D rewrites, ~D forward-chains, ~D schmas>"
		   (cong-state*-id cs*)
		   (length (cong-state*-assertions cs*))
		   (hash-table-count (cong-state*-use-hash cs*))
		   (hash-table-count (cong-state*-find-hash cs*))
		   (hash-table-count (cong-state*-sig-hash cs*))
		   (length (cong-state*-neq-list cs*))
		   (hash-table-count (cong-state*-type-hash cs*))
		   (+ (length (rewrite-rules-rules!
			       (cong-state*-rewrite-rules cs*)))
		      (length (rewrite-rules-rules
			       (cong-state*-rewrite-rules cs*))))
		   (length (forward-chains-orig-rules
			    (cong-state*-forward-chains cs*)))
		   (hash-table-count (cong-state*-schema-hash cs*)))))) ; added -h2 3/99
  (assertions nil)
  (canon-hash (dp-make-eq-hash-table))
  (seen-hash (dp-make-eq-hash-table))
  (find-hash (dp-make-eq-hash-table))
  (use-hash (dp-make-eq-hash-table))
  (sig-hash (dp-make-eq-hash-table))
  (neq-list nil)
  (type-hash (dp-make-eq-hash-table))
  (polyhedral-structure (initial-polyhedral-structure))
  (fourier-motzkin (initial-fourier-motzkin))
  (rewrite-rules (initial-rewrite-rules))
  (forward-chains (initial-forward-chains))
  (id 0)
  (schema-hash (dp-make-eq-hash-table)))    ; added -hr 2/99

(defun print-cong-state* (cs* s)
  (format s "<~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type, ~D rewrites, ~D forward-chains, ~D schemas>"
    (length (cong-state*-assertions cs*))
    (hash-table-count (cong-state*-use-hash cs*))
    (hash-table-count (cong-state*-find-hash cs*))
    (hash-table-count (cong-state*-sig-hash cs*))
    (length (cong-state*-neq-list cs*))
    (hash-table-count (cong-state*-type-hash cs*))
    (+ (length (rewrite-rules-rules! (cong-state*-rewrite-rules cs*)))
       (length (rewrite-rules-rules (cong-state*-rewrite-rules cs*))))
    (length (forward-chains-orig-rules
	     (cong-state*-forward-chains cs*)))
   (hash-table-count (cong-state*-schema-hash cs*)))) ; added -hr 3/99

(defun clear-cong-state* (cong-state*)
  (setf (cong-state*-assertions cong-state*) nil)
  (dp-clrhash (cong-state*-canon-hash cong-state*))
  (dp-clrhash (cong-state*-seen-hash cong-state*))
  (dp-clrhash (cong-state*-find-hash cong-state*))
  (dp-clrhash (cong-state*-use-hash cong-state*))
  (dp-clrhash (cong-state*-sig-hash cong-state*))
  (setf (cong-state*-neq-list cong-state*) nil)
  (dp-clrhash (cong-state*-type-hash cong-state*))
  (clr-polyhedral-structure (cong-state*-polyhedral-structure cong-state*))
  (clr-fourier-motzkin (cong-state*-fourier-motzkin cong-state*))
  (clr-rewrite-rules (cong-state*-rewrite-rules cong-state*))
  (clr-forward-chains (cong-state*-forward-chains cong-state*))
  (dp-clrhash (cong-state*-schema-hash cong-state*))         ; added -hr 2/99
  cong-state*)

(defdpstruct (made-cong-states
	      (:print-function
	       (lambda (mcs s k)
		 (declare (ignore k))
		 (format s "<~D used cong states, ~D free cong states>"
		   (length (made-cong-states-used mcs))
		   (length (made-cong-states-free mcs))))))
  (used nil :type list)
  (free nil :type list))

(defun print-made-cong-states (mcs s)
  (format s "<~D used cong states, ~D free cong states>"
    (length (made-cong-states-used mcs))
    (length (made-cong-states-free mcs))))

(defun get-cong-state (made-cong-states)
  (if (made-cong-states-free made-cong-states)
      (get-free-cong-state made-cong-states)
      (get-cong-state* made-cong-states)))

(defun get-free-cong-state (made-cong-states)
  (let* ((result (pop (made-cong-states-free made-cong-states))))
    ;(clear-cong-state* result)
    (push result (made-cong-states-used made-cong-states))
    result))

(defun get-cong-state* (made-cong-states)
  (let* ((result (make-cong-state*)))
    (setf (cong-state*-id result)
	  (+ (length (made-cong-states-used made-cong-states))
	     (length (made-cong-states-free made-cong-states))))
    (push result (made-cong-states-used made-cong-states))
    result))

(defun return-cong-state* (cong-state* made-cong-states)
  ;;(assert (member cong-state* (made-cong-states-used made-cong-states)
  ;;                :test #'eq))
  ;(assert (not (eq cong-state* pvs::*ics*)))
  (when (member cong-state* (made-cong-states-used made-cong-states)
		:test #'eq)
    (clear-cong-state* cong-state*)
    (setf (made-cong-states-used made-cong-states)
	  (delete cong-state* (made-cong-states-used made-cong-states)
		  :test #'eq))
    (push cong-state* (made-cong-states-free made-cong-states))))

(defun return-all-cong-states (made-cong-states)
  (loop for cong-state* in (made-cong-states-used made-cong-states)
	do (return-cong-state* cong-state* made-cong-states)))

(defun return-all-cong-states* (made-cong-states)
  (when (double-cons-back (made-cong-states-free made-cong-states))
    (setf (made-cong-states-free made-cong-states)
	  (double-cons-back (made-cong-states-free made-cong-states)))
    (return-all-cong-states* made-cong-states)))

(defvar *made-cong-states* (make-made-cong-states :used nil :free nil))

(defvar *print-cong-state* nil)
(defvar *print-cong-state-stack* t)

(defdpstruct (cong-state
	    (:print-function
	     (lambda (cs s k)
	       (declare (ignore k))
	       (cond
		(*print-cong-state*
		 (format s "~A"
		   (cong-state-used-assertions cs)))
		(*print-cong-state-stack*
		 (format s "<~A>"
		   (loop for cs* in (cong-state-stack cs)
			 collect (cong-state*-id cs*))))
		(t
		 (format s "<~A used assertions>"
		   (length (cong-state-used-assertions cs))))))))
  (stack nil :type list)
  (reverse nil :type list)
  (used-assertions nil :type list))

(defmethod print-cong-state (cs s)
  (if *print-cong-state*
      (format s "~A"
	(cong-state-stack cs))
      (format s "<~A used assertions>"
	(length (cong-state-used-assertions cs)))))

(defmacro top (cong-state-stack)
  `(car ,cong-state-stack))

(defmacro stack-rest (cong-state-stack)
  `(cdr ,cong-state-stack))

(defmacro previous (reverse-cong-state-stack)
  `(cdr ,reverse-cong-state-stack))


;(defvar *cong-state* (null-single-cong-state))

(defun pop-cong-state (cong-state)
  (make-cong-state :stack (cdr (cong-state-stack cong-state))
		   :reverse (reverse (cdr (cong-state-stack cong-state)))
		   ))

(defun npop-cong-state (cong-state)
  (let* ((old-reverse (cong-state-reverse cong-state))
	 (popped-cong-state* (pop (cong-state-stack cong-state)))
	 (new-reverse (nbutlast old-reverse)))
    (setf (cong-state-reverse cong-state)
	  new-reverse)
    (when (domain-eq (polyhedral-structure-polyhedron
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-polyhedron
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-polyhedron (cong-state*-polyhedral-structure
					      popped-cong-state*))
	    (universal-polyhedral-domain)))
    (when (domain-eq (polyhedral-structure-epsilon-poly
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-epsilon-poly
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-epsilon-poly
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-epsilon-poly)))
    (when (eq (polyhedral-structure-projection-matrix
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-projection-matrix
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-projection-matrix
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-projection-matrix)))
    (when (eq (polyhedral-structure-ineq-var-to-index-hash
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-ineq-var-to-index-hash
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-ineq-var-to-index-hash
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-ineq-var-to-index-hash)))
    (when (eq (polyhedral-structure-ineq-var-index-array
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-ineq-var-index-array
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-ineq-var-index-array
	     (cong-state*-polyhedral-structure popped-cong-state*))
	    (initial-ineq-var-index-array)))
    (return-cong-state* popped-cong-state* *made-cong-states*)
    cong-state))

(defun push-new-cong-state (cong-state)
  (declare (type cong-state cong-state))
  (let ((cong-state* (get-cong-state *made-cong-states*)))
    ;(break "pncs")
    (push-cong-state* cong-state* cong-state)))

(defmacro nprotecting-cong-state ((new-cong-state old-cong-state)
				  &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-cong-state (push-new-cong-state ,old-cong-state))
	   (,resultsym nil))
       (unwind-protect
	   (setq ,resultsym
		 (multiple-value-list (progn ,@body)))
	 (npop-cong-state ,new-cong-state))
       (values-list ,resultsym))))

(defun push-cong-state* (cong-state* cong-state)
  (setf (cong-state*-neq-list cong-state*) (nequals cong-state))
  (setf (cong-state*-polyhedral-structure cong-state*)
	(copy-latest-polyhedral-structure
	 (cong-state*-polyhedral-structure cong-state*)
	 (polyhedral-structure cong-state)))
  (setf (cong-state*-fourier-motzkin cong-state*)
	(copy-fourier-motzkin
	 (cong-state*-fourier-motzkin cong-state*)
	 (fourier-motzkin cong-state)))
  (setf (cong-state*-rewrite-rules cong-state*)
	(copy-rewrite-rules-and-hash
	 (cong-state*-rewrite-rules cong-state*)
	 (rewrite-rules cong-state)))
  (setf (cong-state*-forward-chains cong-state*)
	(copy-forward-chains-and-hash
	 (cong-state*-forward-chains cong-state*)
	 (forward-chains cong-state)))
  ;(format t "~%**(setf (cong-state*-polhedral-domain ~A) ~A)**"
  ;   cong-state*
  ;  (polyhedral-domain cong-state))
  ;(break)
  (let* ((new-stack (cons cong-state* (cong-state-stack cong-state)))
	 (new-reverse (reverse new-stack)))
    (make-cong-state :stack new-stack :reverse new-reverse
		     :used-assertions
		     (cong-state-used-assertions cong-state))))

(defun push-assertion (assertion cong-state)
  (declare (type cong-state cong-state))
  (let ((cong-state* (get-cong-state *made-cong-states*)))
    (setf (cong-state*-assertions cong-state*)
	  (list assertion))
    (push-cong-state* cong-state* cong-state)))

(defun add-assertion (assertion cong-state)
  (declare (type cong-state cong-state))
  (setf (cong-state*-assertions (top (cong-state-stack cong-state)))
	(cons assertion (cong-state*-assertions
			 (top (cong-state-stack cong-state)))))
  (setf (cong-state-used-assertions cong-state)
	(cons assertion (cong-state-used-assertions cong-state)))
  cong-state)

(defun dp-changed (old-cong-state new-cong-state)
  (not (eq (cong-state-used-assertions old-cong-state)
	   (cong-state-used-assertions new-cong-state))))

#||
(defmethod print-object :around ((expr vector)) stream)
  (cond
   ((leaf-p (print-leaf expr stream)))
   ((application-p (print-application expr stream)))
   ((cong-state*-p (print-cong-state* expr stream)))
   ((double-cons-p (print-double-cons expr stream)))
   ((made-cong-states-p (print-made-cong-states expr stream)))
   ((cong-state-p (print-cong-state expr stream)))
   (t (call-next-method))))
||#


(defun setf-canon-hash* (term cong-state* term-canon)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (setf (dp-gethash term (cong-state*-canon-hash cong-state*))
	term-canon))

(defsetf canon-hash* setf-canon-hash*)

(defun canon-hash (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (canon-from-stack term (cong-state-stack cong-state)))

(defun canon-from-stack (term cong-state-stack)
  (declare (type node term)
	   (type list cong-state-stack))
  (if cong-state-stack
      (multiple-value-bind (canon found)
	  (canon-hash* term (top cong-state-stack))
	(if found
	    canon
	    (setf (canon-hash* term (the cong-state* (top cong-state-stack)))
		  (canon-from-stack
		   term
		   (rest cong-state-stack)))))
      nil))

(defun canon-hash* (term cong-state*)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-gethash term (cong-state*-canon-hash cong-state*)))

(defun setf-canon-hash (term cong-state term-canon)
  (declare (type node term)
	   (type cong-state cong-state))
  (setf (canon-hash* term (top (cong-state-stack cong-state)))
	term-canon))

(defsetf canon-hash setf-canon-hash)

(defun setf-seen* (term cong-state* term-seen)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (setf (dp-gethash term (cong-state*-seen-hash cong-state*))
	term-seen))

(defsetf seen* setf-seen*)

(defun seen (term cong-state)
  (declare (type node term)
	   (type cong-state cong-state))
  (seen-from-stack term (cong-state-stack cong-state)))

(defun seen-from-stack (term cong-state-stack)
  (declare (type node term)
	   (type list cong-state-stack))
  (if cong-state-stack
      (multiple-value-bind (seen found) (seen* term (top cong-state-stack))
	(if found
	    seen
	    (setf (seen* term (the cong-state* (top cong-state-stack)))
		  (seen-from-stack
		   term
		   (rest cong-state-stack)))))
      nil))

(defun seen* (term cong-state*)
  (declare (type node term)
	   (type cong-state* cong-state*))
  (dp-gethash term (cong-state*-seen-hash cong-state*)))

(defun setf-seen (term cong-state term-seen)
  (declare (special *dp-changed*)
	   (type node term)
	   (type cong-state cong-state))
  (setq *dp-changed* t)
  (setf (seen* term (top (cong-state-stack cong-state)))
	term-seen))

(defsetf seen setf-seen)

(defun nequals (cong-state)
  (declare (type cong-state cong-state))
  (nequals-from-stack (cong-state-stack cong-state)))

(defun nequals-from-stack (cong-state-stack)
  (if cong-state-stack
      (nequals* (top cong-state-stack))
      nil))

(defun nequals* (cong-state*)
  (cong-state*-neq-list cong-state*))

(defun setf-nequals (cong-state new-nequals)
  (setf (cong-state*-neq-list (top (cong-state-stack cong-state)))
	new-nequals))

(defsetf nequals setf-nequals)

(defun polyhedral-structure (cong-state)
  (declare (type cong-state cong-state))
  (polyhedral-structure-from-stack (cong-state-stack cong-state)))

(defun polyhedral-structure-from-stack (cong-state-stack)
  (if cong-state-stack
      (polyhedral-structure* (top cong-state-stack))
      (initial-polyhedral-structure)))

(defun polyhedral-structure* (cong-state*)
  (cong-state*-polyhedral-structure cong-state*))

(defun setf-polyhedral-structure (cong-state new-poly)
  (setf (cong-state*-polyhedral-structure (top (cong-state-stack cong-state)))
	new-poly))

(defsetf polyhedral-structure setf-polyhedral-structure)

(defun fourier-motzkin (cong-state)
  (declare (type cong-state cong-state))
  (fourier-motzkin-from-stack (cong-state-stack cong-state)))

(defun fourier-motzkin-from-stack (cong-state-stack)
  (if cong-state-stack
      (fourier-motzkin* (top cong-state-stack))
      (initial-fourier-motzkin)))

(defun fourier-motzkin* (cong-state*)
  (cong-state*-fourier-motzkin cong-state*))

(defun setf-fourier-motzkin (cong-state new-fourier-motzkin)
  (setf (cong-state*-fourier-motzkin (top (cong-state-stack cong-state)))
	new-fourier-motzkin))

(defsetf fourier-motzkin setf-fourier-motzkin)

(defun rewrite-rules (cong-state)
  (declare (type cong-state cong-state))
  (rewrite-rules-from-stack (cong-state-stack cong-state)))

(defun rewrite-rules-from-stack (cong-state-stack)
  (if cong-state-stack
      (rewrite-rules* (top cong-state-stack))
      (initial-rewrite-rules)))

(defun rewrite-rules* (cong-state*)
  (cong-state*-rewrite-rules cong-state*))

(defun setf-rewrite-rules (cong-state new-rewrite-rules)
  (setf (cong-state*-rewrite-rules (top (cong-state-stack cong-state)))
	new-rewrite-rules))

(defsetf rewrite-rules setf-rewrite-rules)

(defun forward-chains (cong-state)
  (declare (type cong-state cong-state))
  (forward-chains-from-stack (cong-state-stack cong-state)))

(defun forward-chains-from-stack (cong-state-stack)
  (if cong-state-stack
      (forward-chains* (top cong-state-stack))
      (initial-forward-chains)))

(defun forward-chains* (cong-state*)
  (cong-state*-forward-chains cong-state*))

(defun setf-forward-chains (cong-state new-forward-chains)
  (setf (cong-state*-forward-chains (top (cong-state-stack cong-state)))
	new-forward-chains))

(defsetf forward-chains setf-forward-chains)

(defvar *reverse-find* nil)

(defun setf-find* (term cong-state* term-find)
  ;(break "sf")
  (setf (dp-gethash term (cong-state*-find-hash cong-state*))
	term-find))

(defsetf find* setf-find*)

(defun dp-find (term cong-state)
  (if *reverse-find*
      (find-from-reverse-stack term
			       (cong-state-reverse cong-state))
      (find-from-stack-top term (cong-state-stack cong-state))))

(defun dp-find-dbg (term cong-state)
  (let ((rev-find (find-from-reverse-stack term
			       (cong-state-reverse cong-state)))
	(for-find (find-from-stack-top term (cong-state-stack cong-state))))
    (assert (eq rev-find for-find))
    (if *reverse-find*
	rev-find
	for-find)))

(defun find-from-stack-top (term cong-state-stack)
  (let ((find (find-from-stack term cong-state-stack cong-state-stack)))
    (or find
	(setf (find* term cong-state-stack)
	      find))))

(defun find-from-stack (term cong-state-stack top-cong-state-stack)
  (if cong-state-stack
      (let ((find (find-for* term (top cong-state-stack))))
	(if find
	    (if (or (eq cong-state-stack top-cong-state-stack)
		    (eq find term))
		find
		(find-from-stack-top find top-cong-state-stack))
	    (let ((rest-find (find-from-stack term (rest cong-state-stack)
					      cong-state-stack)))
	      (cond
	       (rest-find
		(setf (find* term (top cong-state-stack))
		      rest-find)
		(find-from-stack-top rest-find top-cong-state-stack))
	       (t (setf (find* term (top cong-state-stack))
			term))))))
      nil))

(defun find-from-reverse-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((new-term (find+ term (top cong-state-stack))))
	(find-from-reverse-stack new-term (previous cong-state-stack)))
      term))

(defun find-for* (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-find-hash cong-state*))))
    (if hash-term
	(if (not (eq hash-term term))
	    (find+ hash-term cong-state*)
	    hash-term)
	nil)))

(defun find* (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-find-hash cong-state*))))
    (if hash-term
	(find* hash-term cong-state*)
	term)))

(defun find+ (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-find-hash cong-state*))))
    (if (and hash-term (not (eq hash-term term)))
	(find+ hash-term cong-state*)
	term)))

(defun setf-schema* (term cong-state* term-schema)
  (setf (dp-gethash term (cong-state*-schema-hash cong-state*))
        term-schema))

(defsetf schema* setf-schema*)

(defun dp-schema (term cong-state)
  (if *reverse-find*
      (schema-from-reverse-stack term
                               (cong-state-reverse cong-state))
      (schema-from-stack-top term (cong-state-stack cong-state))))

(defun schema-from-stack-top (term cong-state-stack)
  (let ((schema (schema-from-stack term cong-state-stack cong-state-stack)))
    (or schema
        (setf (schema* term cong-state-stack)
              schema))))

(defun schema-from-stack (term cong-state-stack top-cong-state-stack)
  (if cong-state-stack
      (let ((schema (schema-for* term (top cong-state-stack))))
        (if schema
            (if (or (eq cong-state-stack top-cong-state-stack)
                    (eq schema term))
                schema
                (schema-from-stack-top schema top-cong-state-stack))
            (let ((rest-schema (schema-from-stack term (rest cong-state-stack)
                                              cong-state-stack)))
              (cond
               (rest-schema
                (setf (schema* term (top cong-state-stack))
                      rest-schema)
                (schema-from-stack-top rest-schema top-cong-state-stack))
               (t (setf (schema* term (top cong-state-stack))
                        term))))))
      nil))

(defun schema-from-reverse-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((new-term (schema+ term (top cong-state-stack))))
        (schema-from-reverse-stack new-term (previous cong-state-stack)))
      term))

(defun schema-for* (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-schema-hash cong-state*))))
    (if hash-term
        (if (not (eq hash-term term))
            (schema+ hash-term cong-state*)
            hash-term)
        nil)))

(defun schema* (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-schema-hash cong-state*))))
    (if hash-term
        (schema* hash-term cong-state*)
        term)))

(defun schema+ (term cong-state*)
  (let ((hash-term (dp-gethash term (cong-state*-schema-hash cong-state*))))
    (if (and hash-term (not (eq hash-term term)))
        (schema+ hash-term cong-state*)
        term)))

(defun dp-union (term1 term2 cong-state)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (union* term1 term2 (top (cong-state-stack cong-state))))

(defun union* (term1 term2 cong-state*)
  (when (dp-variable-p (schema+ term2 cong-state*))                     ; added -hr 2/99
    (setf (schema* (schema+ (find+ term2 cong-state*) cong-state*) cong-state*)
	  (schema+ (find+ term1 cong-state*) cong-state*)))
  (setf (find* (find+ term1 cong-state*) cong-state*)
	(find+ term2 cong-state*)))


(defun setf-use* (term cong-state* term-use)
  (assert term-use)
  (setf (dp-gethash term (cong-state*-use-hash cong-state*))
	term-use))

(defsetf use* setf-use*)

(defun use (term cong-state)
  (use-from-stack term (cong-state-stack cong-state)))

(defun use-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((use (use* term (top cong-state-stack))))
	(or use
	    (let ((rest-use (use-from-stack term (rest cong-state-stack))))
	      (when rest-use
		(setf (use* term (top cong-state-stack))
		      rest-use)))))
      nil))

(defun use* (term cong-state*)
  (let ((hash-use (dp-gethash term (cong-state*-use-hash cong-state*))))
    hash-use))

(defun setf-use (term cong-state term-use)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (use* term (top (cong-state-stack cong-state)))
	term-use))

(defsetf use setf-use)

(defun add-top-use (arg term cong-state)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (use arg cong-state) ; This is to ensure that the uses have been
                       ; copied to all the stacked hash-tables.
  (add-top-use-to-stack arg term (cong-state-stack cong-state)))

(defun add-top-use-to-stack (arg term cong-state-stack)
  (when cong-state-stack
    (add-use* arg term (top cong-state-stack))
    (add-top-use-to-stack arg term (rest cong-state-stack))))

(defun add-use* (arg term cong-state*)
  (assert (not (equality-p term)))
  (setf (use* arg cong-state*)
	(adjoin term (use* arg cong-state*) :test #'eq)))

(defun add-use (arg term cong-state)
  ;(assert (eq term (sigma term cong-state)))
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (use* arg (top (cong-state-stack cong-state)))
	(adjoin term (use arg cong-state) :test #'eq)))

;  (add-use* arg term (top (cong-state-stack cong-state)))


(defun setf-sig* (term cong-state* term-sig)
  (setf (dp-gethash term (cong-state*-sig-hash cong-state*))
	term-sig))

(defsetf sig* setf-sig*)

(defun sig (term cong-state)
  (sig-from-stack term (cong-state-stack cong-state)))

(defun sig-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((sig (sig* term (top cong-state-stack))))
	(or sig (setf (sig* term (top cong-state-stack))
		      (sig-from-stack term (rest cong-state-stack)))))
      term))

(defun sig* (term cong-state*)
  (let ((hash-sig (dp-gethash term (cong-state*-sig-hash cong-state*))))
    hash-sig))

(defun setf-sig (term cong-state term-sig)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (sig* term (top (cong-state-stack cong-state)))
	term-sig))

(defsetf sig setf-sig)

(defun setf-type* (term cong-state* term-type) ;(break)
  (setf (dp-gethash term (cong-state*-type-hash cong-state*))
	term-type))

(defsetf type* setf-type*)

(defun dp-type (term cong-state)
  (type-from-stack term (cong-state-stack cong-state)))

(defun type-from-stack (term cong-state-stack)
  (if cong-state-stack
      (let ((type (type* term (top cong-state-stack))))
	(or type (setf (type* term (top cong-state-stack))
		       (type-from-stack term (rest cong-state-stack)))))
      (node-initial-type term)))

(defun type* (term cong-state*)
  (let ((hash-type (dp-gethash term (cong-state*-type-hash cong-state*))))
    hash-type))

(defun setf-dp-type (term cong-state term-type)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (setf (type* term (top (cong-state-stack cong-state)))
	term-type))

(defsetf dp-type setf-dp-type)

(defun type-union (term1 term2 cong-state)
  (let ((type1 (dp-type term1 cong-state))
	(type2 (dp-type term2 cong-state)))
    (let ((merged-type (merge-type type1 type2)))
      ;(when (eq term1 *t1*) (break))
      (setf (dp-type term1 cong-state) merged-type
	    (dp-type term2 cong-state) merged-type))))
	      
(defun merge-type (type1 type2)
  (or type1 type2))

(defun findalist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-find-hash cong-state*)
	using (hash-value find)
	unless (eq term find)
	collect (cons term find)))

(defun findalist (cong-state)
  (findalist-from-stack (cong-state-stack cong-state)))

(defun findalist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (findalist* (top cong-state-stack))
	     (findalist-from-stack (rest cong-state-stack)))
      nil))

(defun usealist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-use-hash cong-state*)
	using (hash-value use)
	collect (cons term use)))

(defun usealist (cong-state)
  (usealist-from-stack (cong-state-stack cong-state)))

(defun usealist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (usealist* (top cong-state-stack))
	     (usealist-from-stack (rest cong-state-stack)))
      nil))

(defun sigalist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-sig-hash cong-state*)
	using (hash-value sig)
	collect (cons term sig)))

(defun sigalist (cong-state)
  (sigalist-from-stack (cong-state-stack cong-state)))

(defun sigalist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (sigalist* (top cong-state-stack))
	     (sigalist-from-stack (rest cong-state-stack)))
      nil))

(defun typealist* (cong-state*)
  (loop for term being the hash-key in (cong-state*-type-hash cong-state*)
	using (hash-value type)
	collect (cons term type)))

(defun typealist (cong-state)
  (typealist-from-stack (cong-state-stack cong-state)))

(defun typealist-from-stack (cong-state-stack)
  (if cong-state-stack
      (nconc (typealist* (top cong-state-stack))
	     (typealist-from-stack (rest cong-state-stack)))
      nil))



(defun null-cong-state ()
  (make-cong-state :stack nil
		   :reverse nil
		   :used-assertions nil))

(defun single-cong-state (cong-state*)
  (let ((stack (list cong-state*)))
    (setf (cong-state*-polyhedral-structure cong-state*)
	  (initial-polyhedral-structure))
    (make-cong-state :stack stack
		     :reverse stack)))

(defun null-single-cong-state ()
  (single-cong-state (get-cong-state *made-cong-states*)))


(defun init-dp-0 (&optional strong)
  (when strong
    (dp-clrhash *term-hash*)
  ;(setq *max-node-index* 0)
    (setq *made-cong-states* (make-made-cong-states :used nil :free nil)))
  ;(return-all-cong-states *made-cong-states*)
  (setq *ineq-var-count* 0)
  (setq *epsilon* (make-epsilon))
  (setq *universal-polyhedral-domain*
	(make-universal-polyhedron *max-ineq-vars* *max-rays*))
  (setq *epsilon-leq-0-poly*
	(make-epsilon-leq-0-polyhedron *max-ineq-vars* *max-rays*))
  (setq *=* (mk-predicate-sym '=))
  (setq *true* (mk-constant 'TRUE))
  (setq *false* (mk-constant 'FALSE))
  (setq *plus* (mk-arith-operator 'plus))
  (setq *times* (mk-arith-operator 'times))
  (setq *divide* (mk-arith-operator 'divide))
  (setq *minus* (mk-arith-operator 'minus))
  (setq *difference* (mk-arith-operator 'difference))
  (setq *sup* (mk-arith-operator 'sup))
  (setq *inf* (mk-arith-operator 'inf))
  (setq *floor* (mk-arith-operator 'floor))
  (setq *ceiling* (mk-arith-operator 'ceiling))
  (setq *arithfuns*
	(list *plus* *times* *divide* *minus* *difference* *sup* *inf*))
  (setq *lesseqp* (mk-arith-pred 'lesseqp))
  (setq *lessp* (mk-arith-pred 'lessp))
  (setq *greatereqp* (mk-arith-pred 'greatereqp))
  (setq *greaterp* (mk-arith-pred 'greaterp))
  (setq *arithpreds*
	(list *lesseqp* *lessp* *greaterp* *greatereqp*))
  (setq *not* (mk-predicate-sym 'not))
  (setq *nequal* (mk-predicate-sym 'nequal))
  (setq *if* (mk-interpreted-constant 'if))
  (setq *and* (mk-predicate-sym 'and))
  (setq *or* (mk-predicate-sym 'or))
  (setq *preds*
	(list *not* *and* *or* *nequal*
	      *lesseqp* *lessp* *greaterp* *greatereqp*))
  (setq *zero* (mk-dp-number 0))
  (setq *one* (mk-dp-number 1))
  (setq *neg-one* (mk-dp-number -1))
  (setq *update* (mk-array-operator 'update))
  (setq *tuple* (mk-interpreted-constant 'tuple 'tuple-op))
  (setq *record* (mk-interpreted-constant 'record 'record-op))
  (setq *project* (mk-interpreted-constant 'project 'project-op))
  (setq *th-app* (mk-interpreted-constant 'th-app 'th-app-op)))

;; Miscellaneous Functions

(defun vars-of (trm)
  (cond ((dp-variable-p trm)
        (list trm))
       ((application-p trm)
	(mapcan #'vars-of
	  (application-arguments trm)))
       (t nil)))

(defun occurs-p (x trm)
  (cond ((dp-variable-p trm)
	 (eq x trm))
	((constant-p trm)
	 (eq x trm))
	((application-p trm)
	 (some #'(lambda (arg)
		       (occurs-p x arg))
		   (application-arguments trm)))
	(t nil)))

(defun replace-by (trm subst)
  (let ((*subst* subst))
    (declare (special *subst*))
    (replace-by* trm)))

(defun replace-by* (trm)
  (declare (special *subst*))
  (cond ((leaf-p trm)
	 (let ((res (lookup trm *subst*)))
	   (if res (cdr res) trm)))
	((application-p trm)
	 (mk-term (mapcar #'replace-by*
		    (application-arguments trm))))
	(t trm)))

(defun lookup (trm subst)
  (assoc trm subst))

(defun occurs-in-scope-of-uninterp-p (x trm)
  (and (application-p trm)
       (if (or (node-interpreted? (funsym trm))
	       (equality-p trm))
	   (some #'(lambda (arg)
		     (occurs-in-scope-of-uninterp-p x arg))
		 (application-arguments trm))
         (some #'(lambda (arg)
	           (occurs-p x arg))
	       (application-arguments trm)))))

(defun well-formed-node-p (trm)
  (or (leaf-p trm)
      (and (application-p trm)
	   (every #'well-formed-node-p
		  (application-arguments trm)))))

;; Additional Symbols, Recognizers etc.

(defvar *implies* (mk-predicate-sym 'implies))

(setf *preds* (cons *implies* *preds*))

(defun mk-conjunction (trm1 trm2)
  (declare (type node trm))
  (mk-term (list *and* trm1 trm2)))

(defun mk-disjunction (trm1 trm2)
  (mk-term (list *or* trm1 trm2)))

(defun mk-implication (trm1 trm2)
  (mk-term (list *implies* trm1 trm2)))

(defun conjunction-p (term)
  (and (application-p term)
       (eq (funsym term) *and*)))

(defun disjunction-p (term)
  (and (application-p term)
       (eq (funsym term) *or*)))

(defun implication-p (term)
  (and (application-p term)
       (eq (funsym term) *implies*)))

