;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-structures.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:54:23
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp)

(eval-when (eval compile load)
  (defvar *develop* t)
  (defvar *node-type* 'array)
  (defvar *argument-type* 'list)
  (defvar *use-alists* t))


;;; When *develop* is nil, the dp-datatstructures are implemented
;;; as :named *node-types*, which are much faster than simple defstructs,
;;; but don't provide the print-functions and types.

;;; The arguments are kept either as arrays or lists
;;; depending on *argument-type*
;;; In my experience (DAC) lists are found to be superior.

;;; The congruence-closure/union-find data-structures (such as find, use, ...)
;;; are stored either as alists or hash-tables depending on whether
;;; *use-alists* is set or not.


(declaim (special *dp-changed*))
(defvar *dp-changed* nil)
(declaim (special *contradiction*))
(defvar *contradiction* nil)

;;; When the decision procedures detect that something substantial has
;;; changed, such as a find being updated, it sets *dp-changed* to t.
;;; If *dp-changed remains nil after an assertion has been processed
;;; then we know that the assertion was already entailed.

;;; When any part of the decision procedures detects a contradiction
;;; *contradiction* is set to t.


(defvar *max-sxhash* 65535) ;;; Max hash value (look at lisp manual)


(defvar *max-node-index* 0) ;;; keeps track of the number of terms in the
                            ;;;*term-hash*

(defvar *fresh-prefix* "d") ;;; the prefix to be used when creating
                            ;;; new constants.


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
    "For use in generating the correct loop macro."
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


;;; node is the basic type for dp terms.
;;; The idea behind initial-type is that this is what is initially
;;; given to the node from the external translation.
;;; type on the other hand, might be inferred in the course of
;;; processing.  For example we might determine dynamically
;;; that something is > 0 or an integer, etc.
;;; I have also used a type list (or hash-table) as part of the cong-state
;;; data-structure so that the dynamic type info is kept on the stack and
;;; can be undone.

;;; external-info is an alist that allows an external implementor
;;; to add new fields to the node data-structure without having
;;; to modify this file.  See defdpfield.

(defdpstruct node
  (interpreted? nil :type boolean)
  (initial-type nil :type symbol)
  (type initial-type :type symbol)
  (constructor? nil :type boolean)
  (constructor-accessors nil :type list)
  (accessor-indices nil :type list)
  (fresh? nil :type boolean)
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
  (format s "~A" 
    (if (eq *argument-type* 'list)
	(application-arguments a)
	(array-to-list (application-arguments a)))))


(defmacro defdpfield (name)
  "Defines two new functions: accessor for node-name and setf for node-name."
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
  "Used as the hash-function for the *term-hash*"
  ;;; Should look into whether equal might work better.
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
  "Used as the hash-function for the *term-hash*"
  ;;; Should look into whether equal might work better.
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
		   (1+ *max-sxhash*)))
		 ((listp args)
		  (mod
		   (loop for a in (the list args)
			 sum (dp-eq-sxhash a))
		   (1+ *max-sxhash*)))
		 ((atom args) (dp-eq-sxhash args))
		 (t (break)
		  (dp-eq-sxhash args)))))
    result))

(defmacro dp-make-eq-hash-table ()
  '(make-hash-table :test #'eq :hash-function 'dp-eq-sxhash))

(defvar *term-hash*)

(eval-when (load eval)
  (setq *term-hash* (make-hash-table :test (if (eq *argument-type* 'list)
					       'equal-list
					       'equal-array)
				     :hash-function 'dp-sxhash)))

(defvar *mk-term-adduse* t) ;;; doesn't seem to be used.
                            ;;; The idea might have been that
                            ;;; on a mk-term adduse would be called.
                            ;;; But not all hash-consed terms
                            ;;; should be in the universe.
                            ;;; Perhaps worth revisiting.

(defun mk-arg-array (arguments)
  (make-array (length arguments)
	      :element-type 'node
	      :initial-contents arguments))

(defun mk-term (arguments &optional (type nil))
  "The api function for creating terms that are hash-consed.
Only this, mk-constant, mk-interpreted-constant and mk-variable
should be called."
  (if (eq *argument-type* 'list)
      (mk-term-list arguments type)
      (let ((arg-array (mk-arg-array arguments)))
	(mk-term-array arg-array type))))

(defun mk-term-array (arg-array &optional (type nil))
  (let ((hashed-term (gethash arg-array *term-hash*)))
    (if hashed-term
	(if type
	    (progn (setf (node-type hashed-term) type)
		   hashed-term)
	    hashed-term)
	(setf (gethash arg-array *term-hash*)
	      (mk-term* arg-array type)))))

(defun mk-term-list (arg-list &optional (type nil))
  (let ((hashed-term (gethash arg-list *term-hash*)))
    (cond
     (hashed-term
      (if type
	  (progn (setf (node-type hashed-term) type)
		 hashed-term)
	  hashed-term))
     (t (setf (gethash arg-list *term-hash*)
	      (mk-term* arg-list type))))))

(defun mk-term* (arguments type)
  (let ((new-node (make-application :sxhash (dp-sxhash arguments)
				    :index (incf *max-node-index*)
				    :initial-type type
				    :arguments arguments)))
    new-node))

(defun mk-constant (id &optional (type nil))
  "The api function for creating constants that are hash-consed."
  (let ((hashed-constant (gethash id *term-hash*)))
    (if hashed-constant
	(if type
	    (progn (setf (node-type hashed-constant) type)
		   hashed-constant)
	    hashed-constant)
	(setf (gethash id *term-hash*)
	      (mk-constant* id type)))))

(defun mk-constant* (id type)
  (let ((new-constant (make-constant :sxhash (dp-sxhash id)
				     :index (incf *max-node-index*)
				     :initial-type type
				     :id id)))
    new-constant))

(defun mk-new-constant (id type)
  "Should not exist. Use mk-fresh-constant"
  (mk-constant* id type))

(defun mk-fresh-constant ()
  (let* ((fresh-id (gentemp *fresh-prefix*))
	 (res (mk-constant fresh-id)))
    (setf (node-fresh? res) t)
    res))

(defun mk-variable (id &optional (type nil))
  "The api function for creating variables that are hash-consed."
  (let ((hashed-variable (gethash id *term-hash*)))
    (if hashed-variable
	(if type
	    (progn (setf (node-type hashed-variable) type)
		   hashed-variable)
	    hashed-variable)
	(setf (gethash id *term-hash*)
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
  "Takes as lisp list and makes a dp term from it."
  (cond
   ((consp list)
    (mk-term (loop for l in list collect (lisp-to-dp l))))
   (t (mk-constant list))))



;;; The following are functions and macros for accessing and traversing
;;; dp terms.

;;; Of note: only use arg to access arguments.
;;; funsym is arg 0.
;;; General applications would not distinguish the funsym and the
;;; rest of the arguments, since you might want to do congruence
;;; closure on the function position also incase the terms are higher order.

;;; But for interpreted terms like plus, ...
;;; the function position is distinguished.
;;; For this we introduce the accessor funargs, which takes the
;;; cdr of application-arguments for dealing with these interpreted cases.
;;; The mapping macros have both funarg and normal arg forms.


(defmacro arity (application)
  `(1- (length (the ,*argument-type* (application-arguments ,application)))))

(defmacro arg (num application)
  "Accesses the ith argument. Arg 0 is usually the function position."
  (if (eq *argument-type* 'list)
      `(nth (the (integer 0 100) ,num)
	    (the list
	      (application-arguments (the application ,application))))
      `(svref (the ,*argument-type*
		(application-arguments (the application ,application)))
	      (the (integer 0 100) ,num))))

(defmacro map-args (fn application &rest more-args)
  "Applys fn to each argument of application.
Passes more-args to every call of fn."
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the ,*argument-type*
			      (application-arguments ,application))
	   for arg ,(across-or-in) (the ,*argument-type* arguments)
	   do (funcall ,fn arg .,more-args)))

(defmacro map-args-args (fn application1 application2 &rest more-args)
  "Like map-args, but fn is applied to arguments from two applications."
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
  "Like map-args, but returns a list of the fn results."
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the ,*argument-type*
			   (application-arguments ,application))
	 for arg ,(across-or-in) (the simple-vector arguments)
	 collect (funcall ,fn arg .,more-args)))

(defmacro map-args-array (fn application &rest more-args)
  "Like map-args, but returns an array of the fn results."
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
  "Like map-args, but doesn't apply fn to arg 0."
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
  "Like map-args-list, but doesn't apply fn to arg 0."
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs-list fn application more-args))

(defun expand-map-funargs-nconc (fn application more-args)
  (if (eq *argument-type* 'list)
      `(loop with arguments = (the list
				(application-arguments ,application))
	     for arg in (cdr arguments)
	     nconc (funcall ,fn arg
		       .,more-args))
      `(loop with arguments = (the simple-vector
				(application-arguments ,application))
	     for i from 1 to (arity ,application)
	     nconc (funcall ,fn (svref (the simple-vector arguments) i)
			    .,more-args))))

(defmacro map-funargs-nconc (fn application &rest more-args)
  "Nconc version of map-funargs-list."
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs-nconc fn application more-args))

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
  "Like map-args-list, but doesn't apply fn to arg 0."
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

(defun replace-by (trm *subst*)
  (declare (special *subst*))
  (replace-by* trm))

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

(defun destructure-application (trm &optional args)
  (if (and (application-p trm)
	   (not (record-p (funsym trm))))
      (destructure-application (funsym trm)
                               (append (funargs trm) args))
    (values trm args)))




