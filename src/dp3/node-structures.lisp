;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node-structures.lisp -- 
;; Author          : David Cyrluk
;; Created On      : 1998/06/12 22:54:23
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

;;; Miscellaneous

(defun initial-node-structures (foo)
  T)

(defconstant *fixnum-byte* (byte 29 0))

(defvar *false-list* (list *false*))

(deftype nonneg-fixnum () `(integer 0 ,most-positive-fixnum))

(eval-when (eval compile load)
  (defvar *develop* T)
  (defvar *node-type* 'vector))

;;; When *develop* is nil, the dp-datastructures are implemented
;;; as :named *node-types*, which are much faster than simple defstructs,
;;; but don't provide the print-functions and types.
;;; The arguments are kept either as lists.
;;; The congruence-closure/union-find data-structures (such as find, use, ...)
;;; are stored as alists.

(declaim (special *dp-changed*))
(defvar *dp-changed* nil)

(declaim (special *contradiction*))
(defvar *contradiction* nil)

;;; When the decision procedures detect that something substantial has
;;; changed, such as a find being updated, it sets *dp-changed* to t.
;;; If *dp-changed remains nil after an assertion has been processed
;;; then we know that the assertion was already entailed.
;;;
;;; When any part of the decision procedures detects a contradiction
;;; *contradiction* is set to t.

(defvar *term-hash*)
(defvar *vars-hash*)

(defun init-node-structures (&optional strong)
  (when strong
    (when *term-hash* (clrhash *term-hash*))
    (when *vars-hash* (clrhash *vars-hash*)))
  (setf *dp-changed* nil)
  (setf *contradiction* nil))

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
	name-and-options)))

(defmacro defdpstruct (NAME-AND-OPTIONS &REST SLOT-DESCRIPTIONS)
  (if *develop*
      `(defstruct ,NAME-AND-OPTIONS ,@SLOT-DESCRIPTIONS)
      `(progn
	 (defstruct ,(extend-with-named-type NAME-AND-OPTIONS)
	   ,@SLOT-DESCRIPTIONS)
	 (deftype ,(name-from-name-and-options NAME-AND-OPTIONS)
	   () ,(if (eq *node-type* 'list) ''list ''simple-vector)))))

;; Node is the basic type for dp terms.
;; The idea behind initial-type is that this is what is initially
;; given to the node from the external translation.
;; type on the other hand, might be inferred in the course of
;; processing.  For example we might determine dynamically
;; that something is > 0 or an integer, etc.
;; I have also used a type list (or hash-table) as part of the cong-state
;; data-structure so that the dynamic type info is kept on the stack and
;; can be undone.
;;
;; external-info is an alist that allows an external implementor
;; to add new fields to the node data-structure without having
;; to modify this file.  See defdpfield.

(defdpstruct node
  (interpreted? nil :type boolean)
  (initial-type nil :type symbol)
  (external-info nil :type list)
  (sxhash :type nonneg-fixnum))

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
			   (format s "~A" (application-arguments a)))))
  (arguments nil :type (or simple-vector list)))

(defun print-application (a s)
  (format s "~A" (application-arguments a)))

;; Hook for adding new fields

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


;; Hash consing of nodes

(proclaim '(ftype (function (T) nonneg-fixnum) dp-sxhash))

(defun dp-sxhash (obj)
  (if (listp obj)
      (dp-sxhash-list (the list obj)
		      (the nonneg-fixnum 0))
      (dp-eq-sxhash obj)))

(defun dp-eq-sxhash (obj)
  (if (node-p obj)
      (node-sxhash (the node obj))
      (sxhash obj)))

(defun dp-sxhash-list (l &optional (acc 0))
  (declare (type list l)
	   (type nonneg-fixnum acc))
  (if (null l) acc
     (dp-sxhash-list (cdr l)
		     (the nonneg-fixnum
		       (ldb *fixnum-byte*
			    (+ (the nonneg-fixnum (dp-eq-sxhash
						   (car (the list l))))
			       (the nonneg-fixnum acc)))))))

(defmacro dp-make-eq-hash-table ()
  '(make-hash-table :test 'eq
		    :hash-function 'dp-eq-sxhash))

(defun shallow-equal (args1 args2)
  "Used as the hash-function for the *term-hash* (faster than equal???)"
  (or (eql args1 args2)
      (and (consp args1)
	   (consp args2)
	   (eq (car args1) (car args2))
	   (shallow-equal (the list (cdr args1))
			  (the list (cdr args2))))))

(eval-when (load eval)
  (setq *term-hash* (make-hash-table :test 'equal
				     :hash-function 'dp-sxhash)))

;;; Node constructors

(defun mk-term (args &optional (type nil))
  "The api function for creating terms that are hash-consed.
   Only this, mk-constant, mk-interpreted-constant and mk-variable
   should be called."
  (declare (type list args)
	   (type symbol type))
  #+dbg(assert (every #'node-p args)
	       (symbolp type))
  (let ((hashed (gethash args *term-hash*)))
    (cond (hashed
	   (when type
	     (setf (node-initial-type hashed) type))
	   hashed)
	  (t (setf (gethash args *term-hash*)
		   (make-application :sxhash (dp-sxhash-list (the list args))
				     :initial-type type
				     :arguments args))))))

(defun mk-constant (id &optional (type nil))
  "The api function for creating constants that are hash-consed."
  (declare (type (or symbol rational) id))
  #+dbg(assert (or (symbolp id) (rationalp id)))
  #+dbg(assert (symbolp type))
  (let ((hashed (gethash id *term-hash*)))
    (if hashed
	(if type
	    (progn (setf (node-initial-type hashed) type)
		   hashed)
	    hashed)
	(setf (gethash id *term-hash*)
	      (make-constant :sxhash (sxhash (the (or symbol rational) id))
			     :initial-type type
			     :id id)))))

(defdpfield fresh?)

(defun mk-fresh-constant (&optional type)
  (let* ((fresh-id (the symbol (gentemp *fresh-prefix*)))
	 (res (mk-constant fresh-id)))
    (setf (node-fresh? res) t)
    (when type
      (setf (node-initial-type res) type))
    res))

(defun mk-variable (id &optional (type nil))
  "The api function for creating variables that are hash-consed."
  (declare (type symbol id)
	   (type symbol type))
  #+dbg(assert (symbolp id))
  #+dbg(assert (symbolp type))
  (let ((hashed (gethash id *term-hash*)))
    (cond (hashed
	   (when type (setf (node-initial-type hashed) type))
	   hashed)
	  (t (setf (gethash id *term-hash*)
		   (make-dp-variable :sxhash (sxhash (the symbol id))
				     :initial-type type
				     :id id))))))

(defun lisp-to-dp (list)
  "Takes as lisp list and makes a dp term from it."
  (if (consp list)
      (mk-term (loop for l in list collect (lisp-to-dp l)))
      (mk-constant list)))

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


(defmacro args (trm)
  `(the list (application-arguments
	      (the application ,trm))))

(defmacro arity (trm)
  `(1- (length (the list (args ,trm)))))

(defmacro arg (num trm)
  "Accesses the ith argument. Arg 0 is usually the function position."
  `(the node (nth (the nonneg-fixnum ,num)
		  (the list (args ,trm)))))

(defun lhs (trm)
  (declare (type application trm))
  #+dbg(assert (application-p trm))
  (the node (arg 1 trm)))

(defun rhs (trm)
  (declare (type application trm))
  #+dbg(assert (application-p trm))
  (the node (arg 2 trm)))

(defmacro funsym (trm)
  `(the node (arg 0 (the application ,trm))))

(defun funargs (trm)
  (declare (type application trm))
  (the list (cdr (the list (args trm)))))

;;; The mapping macros have both funarg and normal arg forms.

(defmacro map-args (fn trm &rest more-args)
  "Applys fn to each argument of application.
   Passes more-args to every call of fn."
  `(declare (type application ,trm)
	    (type function ,fn))
  `(loop with arguments = (the list (args ,trm))
	   for arg in (the list arguments)
	   do (funcall ,fn arg .,more-args)))

(defmacro map-args-list (fn application &rest more-args)
  "Like map-args, but returns a list of the fn results."
  `(declare (type application ,application)
	    (type function ,fn))
  `(loop with arguments = (the list (args ,application))
	 for arg in (the list arguments)
	 collect (funcall ,fn arg .,more-args)))

(defmacro map-funargs (fn application &rest more-args)
  "Like map-args, but doesn't apply fn to arg 0."
  `(declare (type application ,application)
	    (type function ,fn))
  (expand-map-funargs fn application more-args))

(defun expand-map-funargs (fn application more-args)
  `(loop with arguments = (the list (args ,application))
	 for arg in (cdr arguments)
	 do (funcall ,fn arg .,more-args)))

(defmacro map-funargs-list (fn trm &rest more-args)
  "Like map-args-list, but doesn't apply fn to arg 0."
  `(declare (type application ,trm)
	    (type function ,fn))
  (expand-map-funargs-list fn trm more-args))

(defun expand-map-funargs-list (fn trm more-args)
  `(loop with arguments = (the list (args ,trm))
	 for arg in (cdr arguments)
	 collect (funcall ,fn arg .,more-args)))


;; Miscellaneous functions

(eval-when (load eval)
  (setq *vars-hash* (dp-make-eq-hash-table)))

(defun vars-of (trm)
  (let ((vars (gethash trm *vars-hash*)))
    (or vars
	(setf (gethash trm *vars-hash*)
	      (vars-of* trm)))))

(defun vars-of* (trm)
  (cond ((dp-variable-p trm)
	 (list trm))
	((application-p trm)
	 (mapcan #'vars-of* (args trm)))
	(t nil)))

(defun occurs-p (x trm)
  (if (application-p trm)
      (some #'(lambda (arg)
		(occurs-p x arg))
	    (args trm))
      (eq x trm)))

(defun solved-form (x eqn)
  #+dbg(assert (node-p eqn))
  (and (equality-p eqn)
       (or (and (eq x (lhs eqn))
		(not (occurs-p x (rhs eqn)))
		(rhs eqn))
	   (and (eq x (rhs eqn))
		(not (occurs-p x (lhs eqn)))
		(lhs eqn)))))

(defmacro solved-equality-p (x trm)
  `(solved-form ,x ,trm))


(defmacro lookup (trm subst)
  `(assoc ,trm ,subst))

(defun replace-by (trm *subst*)
  (declare (special *subst*))
  (replace-by* trm))

(defun replace-by* (trm)
  (declare (special *subst*))
  (cond ((leaf-p trm)
	 (let ((res (lookup trm *subst*)))
	   (if res (cdr res) trm)))
	((application-p trm)
	 (mk-term (mapcar #'replace-by* (args trm))))
	(t trm)))

(defun well-formed-node-p (trm)
  (declare (type node trm))
  (or (leaf-p trm)
      (and (application-p trm)
	   (every #'well-formed-node-p
		  (args trm)))))

(defun destructure-application (trm &optional args)
  (if (and (application-p trm)
	   (not (record-p (funsym trm))))
      (destructure-application (funsym trm)
                               (append (funargs trm) args))
    (values trm args)))

(defun head-symbol (trm)
  (declare (type node trm))
  (cond ((constant-p trm)
	 trm)
        ((application-p trm)
	 (head-symbol (funsym trm)))))

;; Following only used in polyhedron. Get rid of it?

(defun expand-map-funargs-nconc (fn trm more-args)
  `(loop with arguments = (the list (args ,trm))
	 for arg in (cdr arguments)
	 nconc (funcall ,fn arg .,more-args)))

(defmacro map-funargs-nconc (fn trm &rest more-args)
  "Nconc version of map-funargs-list."
  `(declare (type application ,trm)
	    (type function ,fn))
  (expand-map-funargs-nconc fn trm more-args))







