(in-package dp)

(defvar *develop* t)

(declaim (special *dp-changed*))
(defvar *dp-changed* nil)
(declaim (special *contradiction*))
(defvar *contradiction* nil)

(defvar *max-node-index* 0)

(defun extend-with-named-type (name-and-options)
  (if (consp name-and-options)
      (cons (car name-and-options)
	    (append '((:type vector) (:named))
		    (cdr name-and-options)))
      (cons name-and-options
	    '((:type vector) (:named)))))

(defun name-from-name-and-options (name-and-options)
  (if (consp name-and-options)
      (car name-and-options)
      name-and-options))

(defmacro defdpstruct (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS)
  (if *develop*
      `(defstruct ,NAME-AND-OPTIONS ,@SLOT-DESCRIPTIONS)
      `(progn
	 (defstruct ,(extend-with-named-type NAME-AND-OPTIONS)
	   ,@SLOT-DESCRIPTIONS)
	 (deftype ,(name-from-name-and-options NAME-AND-OPTIONS)
	   () 'simple-vector))))
  

(defdpstruct node
  (initial-type nil :type symbol)
  (type initial-type :type symbol)
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
			     (array-to-list (application-arguments a))))))
  (arguments nil :type simple-vector))

(defun print-application (a s)
  (format s "~A" (array-to-list (application-arguments a))))

(defun array-to-list (array)
  (declare (type simple-vector array))
  (loop for i from 0 below (length array)
	collect (svref array i)))

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

(defun dp-eq-sxhash (term)
  (if (node-p term)
      (node-sxhash (the node term))
      (sxhash term)))

(defun dp-sxhash (args)
  (let ((result (if (arrayp args)
		    (mod
		     (loop for a across (the simple-vector args)
			   sum (dp-eq-sxhash a))
		     65536)
		    (dp-eq-sxhash args))))
    result))

(defvar *pvs-hash*)


(eval-when (compile load eval)
  ;(setq pvs::*fast-hash-copy* nil)
  #+allegro-v4.3(setq *pvs-hash* nil)
  #-allegro-v4.3(setq *pvs-hash* t))

(defmacro dp-make-hash-table (&rest args)
  (if *pvs-hash*
      `(pvs::make-pvs-hash-table ,@(subst ':hashfn ':hash-function args))
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
  (setq *term-hash* (dp-make-hash-table :test 'equal-array
					:hash-function 'dp-sxhash)))

(defvar *mk-term-adduse* t)

(defun mk-arg-array (arguments)
  (make-array (length arguments)
	      :element-type 'node
	      :initial-contents arguments))

(defun mk-term (arguments)
  (let ((arg-array (mk-arg-array arguments)))
    (mk-term-array arg-array)))

(defun mk-term-array (arg-array)
  (let ((hashed-term (dp-gethash arg-array *term-hash*)))
    (or hashed-term; (break)
	(setf (dp-gethash arg-array *term-hash*)
	      (mk-term* arg-array)))))

(defun mk-term* (arg-array)
  (let ((new-node (make-application :sxhash (dp-sxhash arg-array)
				    :index (incf *max-node-index*)
				    :arguments arg-array)))
    new-node))

(defun mk-constant (id)
  (let ((hashed-constant (dp-gethash id *term-hash*)))
    (or hashed-constant
	(setf (dp-gethash id *term-hash*)
	      (mk-constant* id)))))

(defun mk-constant* (id)
  (let ((new-constant (make-constant :sxhash (dp-sxhash id)
				     :index (incf *max-node-index*)
				     :id id)))
    new-constant))

(defun mk-variable (id)
  (let ((hashed-variable (dp-gethash id *term-hash*)))
    (or hashed-variable
	(setf (dp-gethash id *term-hash*)
	      (mk-variable* id)))))

(defun mk-variable* (id)
  (let ((new-variable (make-dp-variable :sxhash (dp-sxhash id)
					:index (incf *max-node-index*)
					:id id)))
    new-variable))

(defmacro arity (application)
  `(1- (length (the simple-vector (application-arguments ,application)))))

(defmacro arg (num application)
  `(svref (the simple-vector
	    (application-arguments (the application ,application)))
	 (the (integer 0 100) ,num)))

(defmacro map-args (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the simple-vector
			      (application-arguments ,application))
	   for arg across (the simple-vector arguments)
	   do (funcall ,fn arg .,more-args)))

(defmacro map-args-list (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the simple-vector
			   (application-arguments ,application))
	 for arg across (the simple-vector arguments)
	 collect (funcall ,fn arg .,more-args)))

(defmacro map-args-array (fn application &rest more-args)
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the simple-vector
			   (application-arguments ,application))
	 with length = (length (the simple-vector arguments))
	 with array = (make-array (the fixnum length) :element-type 'node)
	 for i from 0 below (the fixnum length)
	 do (setf (svref (the simple-vector array)
			 (the fixnum i))
		  (funcall ,fn (svref (the simple-vector arguments)
				      (the fixnum i))
			   .,more-args))
			 
	 finally (return array)))

(defun map-funargs (fn application &rest more-args)
  (declare (type application application)
	   (type function fn))
  (loop with arguments = (the simple-vector
			   (application-arguments application))
	for i from 1 to (arity application)
	do (apply fn (svref (the simple-vector arguments) i)
		  more-args)))

(defun map-funargs-list (fn application &rest more-args)
  (declare (type application application)
	   (type function fn))
  (loop with arguments = (the simple-vector
			   (application-arguments application))
	for i from 1 to (arity application)
	collect (apply fn (svref (the simple-vector arguments) i)
		       more-args)))

(defmacro funsym (application)
  `(arg 0 (the application ,application)))

(defun funargs (application)
  (declare (type application application))
  (loop with arguments = (the simple-vector
			   (application-arguments application))
	for i from 1 below (length (the simple-vector arguments))
	collect (svref (the simple-vector arguments) i)))

(defun mk-predicate-sym (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result) 'predicate)
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

;;interpreted arithmetic function symbols
(defvar *arithfuns*
  (list *plus* *times* *divide* *minus* *difference* *sup* *inf*))

(defun mk-arith-pred (sym)
  (let ((result (mk-constant sym)))
    (setf (node-type result)
	  'arith-pred)
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

(defvar *preds*
  (list *not* *nequal* *lesseqp* *lessp* *greaterp* *greatereqp*))

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

(defun nequal-p (term)
  (declare (type node term))
  (and (application-p term)
       (eq (funsym (the application term)) *nequal*)))

(defun dp-numberp (term)
  (declare (type node term))
  (and (constant-p term)
       (numberp (constant-id (the constant term)))))

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
  (equalities nil :type list))

(defun copy-hash-table (new-hash-table old-hash-table)
  (maphash #'(lambda (key val)
	       (setf (gethash key new-hash-table)
		     val))
	   old-hash-table)
  new-hash-table)

(defun copy-array (new-array old-array)
  (loop for i from 0 below (array-total-size old-array)
	do
	(setf (row-major-aref new-array i)
	      (row-major-aref old-array i)))
  new-array)

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
  (copy-array (polyhedral-structure-ineq-var-index-array new-poly-s)
	      (polyhedral-structure-ineq-var-index-array old-poly-s))
  (setf (polyhedral-structure-epsilon-poly new-poly-s)
	(polyhedral-structure-epsilon-poly old-poly-s))
  (setf (polyhedral-structure-polyhedron new-poly-s)
	(polyhedral-structure-polyhedron old-poly-s))
  (setf (polyhedral-structure-input-eqns new-poly-s)
	(polyhedral-structure-input-eqns old-poly-s))
  (setf (polyhedral-structure-equalities new-poly-s)
	(polyhedral-structure-equalities old-poly-s))
  new-poly-s)

(defdpstruct (cong-state*
	      (:print-function
	       (lambda (cs* s k)
		 (declare (ignore k))
		 (format s "<~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type>"
		   (length (cong-state*-assertions cs*))
		   (hash-table-count (cong-state*-use-hash cs*))
		   (hash-table-count (cong-state*-find-hash cs*))
		   (hash-table-count (cong-state*-sig-hash cs*))
		   (length (cong-state*-neq-list cs*))
		   (hash-table-count (cong-state*-type-hash cs*))))))
  (assertions nil)
  (seen-hash (dp-make-eq-hash-table))
  (find-hash (dp-make-eq-hash-table))
  (use-hash (dp-make-eq-hash-table))
  (sig-hash (dp-make-eq-hash-table))
  (neq-list nil)
  (type-hash (dp-make-eq-hash-table))
  (polyhedral-structure (initial-polyhedral-structure)))

(defun print-cong-state* (cs* s)
  (format s "<~D assertions, ~D use, ~D find, ~D sig, ~D neq, ~D type>"
    (length (cong-state*-assertions cs*))
    (hash-table-count (cong-state*-use-hash cs*))
    (hash-table-count (cong-state*-find-hash cs*))
    (hash-table-count (cong-state*-sig-hash cs*))
    (length (cong-state*-neq-list cs*))
    (hash-table-count (cong-state*-type-hash cs*))))

(defun clear-cong-state* (cong-state*)
  (setf (cong-state*-assertions cong-state*) nil)
  (dp-clrhash (cong-state*-seen-hash cong-state*))
  (dp-clrhash (cong-state*-find-hash cong-state*))
  (dp-clrhash (cong-state*-use-hash cong-state*))
  (dp-clrhash (cong-state*-sig-hash cong-state*))
  (setf (cong-state*-neq-list cong-state*) nil)
  (dp-clrhash (cong-state*-type-hash cong-state*))
  (clr-polyhedral-structure (cong-state*-polyhedral-structure cong-state*))
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
    (push result (made-cong-states-used made-cong-states))
    result))

(defun return-cong-state* (cong-state* made-cong-states)
  (assert (member cong-state* (made-cong-states-used made-cong-states)
		  :test #'eq))
  ;(assert (not (eq cong-state* pvs::*ics*)))
  (clear-cong-state* cong-state*)
  (setf (made-cong-states-used made-cong-states)
	(delete cong-state* (made-cong-states-used made-cong-states)
		:test #'eq))
  (push cong-state* (made-cong-states-free made-cong-states)))

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

(defdpstruct (cong-state
	    (:print-function
	     (lambda (cs s k)
	       (declare (ignore k))
	       (if *print-cong-state*
		   (format s "~A"
		     (cong-state-used-assertions cs))
		   (format s "<~A used assertions>"
		     (length (cong-state-used-assertions cs)))))))
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

(defun null-cong-state ()
  (make-cong-state :stack nil
		   :reverse nil
		   :used-assertions nil))

(defun single-cong-state (cong-state*)
  (let ((stack (list cong-state*)))
    (make-cong-state :stack stack
		     :reverse stack)))

(defun null-single-cong-state ()
  (single-cong-state (get-cong-state *made-cong-states*)))

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
    (when (eq (polyhedral-structure-polyhedron
	       (cong-state*-polyhedral-structure popped-cong-state*))
	      (polyhedral-structure-polyhedron
	       (polyhedral-structure cong-state)))
      ;;; so that it is not freed when popped-cong-state* is returned.
      (setf (polyhedral-structure-polyhedron (cong-state*-polyhedral-structure
					      popped-cong-state*))
	    (universal-polyhedral-domain)))
    (when (eq (polyhedral-structure-epsilon-poly
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

(defun push-cong-state* (cong-state* cong-state)
  (setf (cong-state*-neq-list cong-state*) (nequals cong-state))
  (setf (cong-state*-polyhedral-structure cong-state*)
	(copy-latest-polyhedral-structure
	 (cong-state*-polyhedral-structure cong-state*)
	 (polyhedral-structure cong-state)))
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

(defun dp-union (term1 term2 cong-state)
  (declare (special *dp-changed*))
  (setq *dp-changed* t)
  (union* term1 term2 (top (cong-state-stack cong-state))))

(defun union* (term1 term2 cong-state*)
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
      nil))

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

(defun init-dp-0 ()
  ;(dp-clrhash *term-hash*)
  ;(setq *max-node-index* 0)
  ;(setq *made-cong-states* (make-made-cong-states :all nil :free nil))
  ;(return-all-cong-states *made-cong-states*)
  (setq *ineq-var-count* 0)
  (setq *epsilon* (make-epsilon))
  ;(setq *universal-polyhedral-domain* (make-universal-polyhedron))
  (make-epsilon-leq-0-polyhedron *max-ineq-vars* *max-rays*)
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

  (setq *preds*
	(list *not* *nequal* *lesseqp* *lessp* *greaterp* *greatereqp*))
  (setq *zero* (mk-dp-number 0))
  (setq *one* (mk-dp-number 1))
  (setq *neg-one* (mk-dp-number -1)))
