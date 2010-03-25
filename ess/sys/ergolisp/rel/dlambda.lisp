;;; -*- Mode: Lisp; Package: DLAMBDA -*-
;;;
;;; Support for ML-like "destructuring" lambda, let, etc.
;;;
;;; See .../doc/dlambda.txt or ERGO report 87-034 for documentation.
;;;
;;; Author: Conal.  Last Modified Wed Feb 15 16:54:17 1989
;;;

#-gcl
(defpackage :dlambda #+sbcl (:use :common-lisp :ergolisp))
(in-package :dlambda) #-sbcl (use-package :ergolisp)

(eexport '(dlambda fdlambda ddefun dlet dlet* dcase))

(eexport '(declare-constructor defreconstr all-constructors))

(defvar *constructors-table* (make-hash-table)
  "The constructor hashtable.  Maps a constructor name to a list containing
two elements:  (1) the list of selector function names, or a single
multiple-value-producing destructor function, and (2) the discriminator function
name.")

(defmacro get-constructor-info (constr)
  "Get the info associated with CONSTR, or :no-info if none."
  ;; This is a macro so that setf will work for declare-constructor
  `(gethash ,constr *constructors-table* :no-info))

(defconstant-if-unbound *reserved-constrs* '(:as)
  "List of symbols that may not be used as constructors.")

(defmacro defreconstr (constr argcnt &key equal)
  "Define a spine-copying reconstruction function called `re-CONSTR'.  The
first argument is the old object.  The next ARGCNT will are the old parts
 (components).  The last are the new parts.  If the old parts are the same as
the new parts, according to the symbols EQUAL-FUNS (which defaults to a list of
EQs), then the function will return the old object.  For now, this is a
function, but it may become either an inline-expanded function, or a macro
later.  The keyword argument :equal is a list of equality function names to
use.  If absent, defaults to a list of EQ's.  If a single name is given, it is
re-interpreted as a list of ARGCNT instances of that name."
  ;; First "normalize" the EQUAL argument.
  (unless equal (setq equal 'EQ))
  (if (listp equal)
      (assert (= (length equal) argcnt) ()
	      "defreconstr: ARGCNT (~d) not equal to length of EQUAL ~s"
	      argcnt equal)
    (let ((equal-name equal))
      (setq equal ())
      (dotimes (i argcnt) (push equal-name equal))))
  (if (> argcnt 0)
      (let ((obj-arg (gensym)))
	(multiple-value-bind (old-args new-args)
	    (do ((argcnt-left argcnt (1- argcnt-left))
		 (olds () (cons (gensym) olds))
		 (news () (cons (gensym) news)))
		((= argcnt-left 0) (values olds news)))
	  `(defun ,(intern (concatenate 'string "RE-" (symbol-name constr)))
	     (,obj-arg ,@old-args ,@new-args)
	     (if (and ,@(mapcar #'(lambda (old new equal)
				    `(,equal ,old ,new))
				old-args new-args equal))
		 ,obj-arg
		 (,constr ,@new-args)))))
      `(progn)))

;;; For example, try macroexpanding the following:

;;; (defreconstr blort 2)
;;; (defreconstr blort 2 :equal (foo bar))
;;; (defreconstr blort 2 :equal (foo bar baz))
;;; (defreconstr blort 2 :equal foo)

(defmacro declare-constructor (constr selectors &rest rest)
  "Declare that CONSTR has SELECTORS and DISCRIM as list of selector functions
and single discriminator function.  The latter defaults to CONSTRp or CONSTR-p
depending on whether CONSTR contains a hyphen, as is Common Lisp custom.  As a
special case, if the first member of SELECTORS is :values, then the second
must be the name of a multiple-value-producing destructor function, and the
third must be the number of components.  The keyword argument :equal is a list
of equality function names to use.  If absent, defaults to a list of EQ's.  If
a single name is given, it is re-interpreted as a list of that name."
  ;; If no rest, or starts with a keyword, ....  I wish the discrim were
  ;; mandatory, but don't want to change all of its uses. - Conal
  (if (or (null rest)
	  (keywordp (first rest)))
      `(do-declare-constructor 
	,constr ,selectors 
	,(intern (format nil
			 (if (position #\-
				       (symbol-name constr))
			     "~:@(~a~)-P"
			     "~:@(~a~)P")
			 constr))
	,@rest)
      `(do-declare-constructor
	,constr ,selectors
	,(first rest)
	,@(rest rest))))

(defmacro do-declare-constructor (constr selectors discrim &key equal)
  "Helper macro for declare-constructor, in which the discrim argument is
mandatory.  EQUAL keyword argument defaults to EQ"
  (assert (and (symbolp constr)
	       (listp selectors))
	  ()
	  "Ill-formed use of declare-constructor: ~s ~s ~s"
	  constr selectors discrim)
  (assert (not (member constr *reserved-constrs*))
	  () "Attempt to declare constructor ~a, which is a reserved name"
	  constr)
  ;; I don't think this value stuff really works right.
  (multiple-value-bind (sel-spec arg-cnt)
      (if (and (> (length selectors) 0)
	       (eq (first selectors) :values))
	  (values (second selectors) (third selectors))
	  (values selectors (length selectors)))
    `(progn
       (defreconstr ,constr ,arg-cnt :equal ,equal)
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (setf (get-constructor-info ',constr)
	       (list ',sel-spec
		     ',discrim))))))

(defun get-constructor-info-must (constr pat)
  "Get the constructor info, and signal an error if it's not there"
  (let ((cinfo (get-constructor-info constr)))
    (assert (not (eq cinfo :no-info)) ()
	    "No info declared for constructor ~s in pattern ~s."
	    constr pat)
    cinfo))


(defun all-constructors (&aux (all ()))
  "Returns an alist of all declared selectors"
  (maphash #'(lambda (type sels)
	       (push (cons type sels) all))
	   *constructors-table*)
  all)

;;; For an example, do "(macroexpand-1 '(dlambda (x (1+ y) _) (+ x y)))"

(defmacro dlambda (pats . doc-body)
  "Destructuring lambda.  Generalizes regular lambda."
  (dlambda-expand pats doc-body))

(defmacro fdlambda (pats . doc-body)
  "Like #'(dlambda PATS . DOC-BODY), but doesn't make the reader choke."
  `#',(dlambda-expand pats doc-body))

(defun dlambda-expand (pats doc-body)
  (if (stringp (first doc-body))		; Doc string
      (dlambda-expand-help pats (list (first doc-body)) (rest doc-body))
      (dlambda-expand-help pats () doc-body)))

(defun dlambda-expand-help (pats doc-strings body)
  "Expand into a lambda.  doc-strings an empty or singleton list of strings."
  (multiple-value-bind (params ignored-params bind-pairs)
      (args-binds pats)
    `(lambda ,params
       ,@doc-strings
       ,@(mapcar #'(lambda (x) `(declare (ignore ,x))) ignored-params)
       ,@(if (null bind-pairs)
	     body
	     `((let ,bind-pairs
		 ,@body))))))

(defun args-binds (pats)
  "Given PATS, return three values: (a) the list of formal parameters, (b) a
list of ignored parameters, and (c) the list of binding pairs.  Optimized for
simple variable pattern."
  (if (null pats)
      (values () () ())
      (let* ((first-pat (first pats))
	     (first-gensym (gensym))
	     (bind-pairs (expand-bind first-pat first-gensym)))
	(multiple-value-bind (rest-params rest-ignored rest-bind-pairs)
	    (args-binds (rest pats))
	  (cond ((null bind-pairs)		; ignored param
		 (values (cons first-gensym rest-params)
			 (cons first-gensym rest-ignored)
			 rest-bind-pairs))
		((and (= (length bind-pairs) 1)	; simple var pat
		      (eq (first (first bind-pairs)) first-pat)
		      (eq (second (first bind-pairs)) first-gensym))
		 (values (cons first-pat rest-params)
			 rest-ignored
			 rest-bind-pairs))
		(t (values (cons first-gensym rest-params) ; general case
			   rest-ignored
			   (nconc bind-pairs rest-bind-pairs))))))))

(defmacro ddefun (func pats . body)
  "The destructuring defun."
  ;; Call dlambda-expand and extract the bound variables
  ;; and body.
  (let ((dflam (dlambda-expand pats body)))
    `(defun ,func ,(second dflam)
       ,@(rest (rest dflam)))))

(defmacro dlet (bind-pairs . body)
  "Destructuring let.  Generalizes regular let.  See fdlambda and
declare-constructor."
  (assert (every #'(lambda (pair)
		     (and (consp pair)
			  (= (length pair) 2)))
		 bind-pairs) ()
		 "Bad format for dlet: ~s.  Must be list of pairs." bind-pairs)
  (let ((pats (mapcar #'first bind-pairs))
	(bind-values (mapcar #'second bind-pairs)))
    ;; Optimize for single (values) pattern.
    (if (and (= (length pats) 1)
	     (consp (first pats))
	     (eq (first (first pats)) 'values))
	(multiple-value-bind (params ignored-params bind-pairs)
	    (args-binds (rest (first pats)))
	  `(multiple-value-bind ,params ,(first bind-values)
	     ,@(mapcar #'(lambda (x) `(declare (ignore ,x))) ignored-params)
	     ,@(if (null bind-pairs)
		   body
		   `((let ,bind-pairs
		       ,@body)))))
	(multiple-value-bind (pats bind-values)
	    (fix-value-pats pats bind-values)
	  `(funcall (fdlambda ,pats ,@body)
	     ,@bind-values)))))

(defmacro dlet* (bind-pairs . body)
  "Like dlet, but binds variables sequentially like let*, rather than in 
parallel."
  (if (null bind-pairs)
      `(progn ,@body)
      `(dlet (,(first bind-pairs))
	 (dlet* ,(rest bind-pairs)
	   ,@body))))

(defun fix-value-pats (pats bind-values)
  "Wherever we see (values p1 ... pn) in PATS, replace it by (list p1 ... pn)
and replace the corresponding value expression v in BIND-VALUES with 
 (multiple-value-list v)."
  (if (null pats)
      (values () ())
      (multiple-value-bind (fixed-rest-pats fixed-rest-values)
	  (fix-value-pats (rest pats) (rest bind-values))
	(if (and (consp (first pats))
		 (eq (first (first pats)) 'values))
	    (progn
	      (warn "Inefficient code produced when \"values\" dlet pattern binding is used in parallel with other bindings.")
	      (values `((list ,@(rest (first pats))) ,@fixed-rest-pats)
		      `((multiple-value-list ,(first bind-values))
			,@fixed-rest-values)))
	    (values `(,(first pats) ,@fixed-rest-pats)
		    `(,(first bind-values) ,@fixed-rest-values))))))


;;; The main workhorse.

(defun expand-bind (pat value)
  ;; Construct a list of let-style binding pairs
  (if (constantp pat)
      `()
      (etypecase pat
	(symbol
	 (if (string-equal (symbol-name pat) "_")
	     `()
	     `((,pat ,value))))
	(cons
	 (case (first pat)
	   ;; (:as v pat)
	   (:as
	    (assert (and (= (length pat) 3) ; right number of subforms.
			 (symbolp (second pat))	; a legal variable.
			 (not (constantp (second pat))))
		    () "Malformed layered pattern ~s.  Must be of the form (:as variable pattern)" pat)
			 
	    `((,(second pat) ,value)
	      ,@(expand-bind (third pat) value)))
	   ;; (mapcar fun-expr l1 ... ln)
	   (mapcar
	    (assert (and (consp (second pat))
			 (member (first (second pat))
				 '(function quote)
				 :test #'eq)
			 (consp (rest (second pat))))
		    () "Malformed function ~s should start with #' or ' in ~s"
		    (second pat) pat)
	    (let ((arg-pats (rest (rest pat)))
		  (confun (second (second pat))))
	      (let ((lambda-confun	; eta-expand
		     (cond ((symbolp confun)
			    (let ((eta-vars (mapcar #'(lambda (arg)
							(declare (ignore arg))
							(gensym))
						    arg-pats)))
			      `(lambda ,eta-vars
				 (,confun ,@eta-vars))))
			   ((and (consp confun)
				 (eq (first confun) 'lambda)
				 (= (length confun) 3))
			    confun)
			   (t (error "Malformed function ~a in pattern ~a"
				     confun pat)))))
		(let ((inverses (confun-inverses lambda-confun)))
		  (mapcan #'(lambda (inv arg)
			      (expand-bind arg `(mapcar #',inv ,value)))
			  inverses arg-pats)))))
	   ;; (list a b ... c)
	   ;; I wanted to have this user-definable, but it is too useful not to
	   ;; have.
	   (list
	    (expand-bind (reduce #'(lambda (hd tl)
				     `(cons ,hd ,tl))
				 (rest pat)
				 :initial-value `nil
				 :from-end t)
			 value))
	   (t (let ((sels (first (get-constructor-info-must (first pat) pat))))
		;; check whether single destructor or list of selectors.
		(if (symbolp sels)
		    (expand-bind `(list ,@(rest pat))
				 `(multiple-values-list (,sels ,value)))
		    (progn
		      (unless (= (length (rest pat)) (length sels))
			(warn "There are ~a pattern arguments in ~a, but ~a selectors declared for ~a."
			      (length (rest pat)) pat
			      (length sels) (first pat)))
		      (mapcan #'(lambda (sub-pat sel)
				  (expand-bind sub-pat `(,sel ,value)))
			      (rest pat) sels))))))))))

(defun confun-inverses (lambda-confun)
  "Construct expressions denoting the inverses of the function \vars.body."
  (let ((new-lambda-var (gensym))
	(vars (second lambda-confun))
	(body (third lambda-confun)))
    (rearrange-bind-pairs-to-funs (expand-bind body new-lambda-var)
				  vars new-lambda-var
				  lambda-confun)))

(defun rearrange-bind-pairs-to-funs (bind-pairs vars new-lambda-var con-exp)
  "Given ((x1 e1) ... (xn en)) and (xi1 ... xin), return
 ((xi1 ei1) ... (xin ein).  Error if VARS are not a permutation of the variables
 in BIND-PAIRS."
  ;; Alters the bind-pairs list destructively!!  Could use remove instead
  ;; if this ever causes trouble.
  (assert (or (and (null bind-pairs)
		   (null vars))
	      (and (not (null vars))
		   (not (null bind-pairs))))
	  () "~a Number of bound variables do not match those in the body."
	  con-exp)
  (if (null bind-pairs)
      ()
      (let ((first-inv-body (cadr (assoc (first vars) bind-pairs))))
	(assert first-inv-body
		() "~a Variable ~a does not appear in the function ~a."
		con-exp (first vars))
	(cons (lambda-expr-optimize new-lambda-var first-inv-body)
	      (rearrange-bind-pairs-to-funs
	       (delete (first vars) bind-pairs
		       :key #'car
		       :test #'eq)
	       (rest vars)
	       new-lambda-var con-exp)))))
	      

(defun lambda-expr-optimize (lambda-var lambda-body)
  "Make a lambda expression, optimizing (lambda (x) (f x)) to f."
  (if (and (consp lambda-body)
	   (eq (second lambda-body) lambda-var))
      (first lambda-body)
      `(lambda (,lambda-var) ,lambda-body)))

(defmacro dcase (object . clauses)
  "The destructuring case.  Not as general as ML.  Chooses clause based only on
the constructor, which is taken to be a type name as well."
  (let* ((new-sym (gensym))
	 (cond-clauses
	  (mapcar #'(lambda (clause)
		      (let* ((pat (first clause))
			     (body (rest clause))
			     (disc-pat (skip-layered pat))
			     (discrim
			      (cond ((and (symbolp disc-pat)
					  (string=
					   (symbol-name disc-pat) "_"))
				     't)
				    ((constantp disc-pat)
				     (constant-discriminator disc-pat
							     new-sym))
				    (t `(,(second (get-constructor-info-must
						   (if (symbolp disc-pat)
						       disc-pat
						       (first disc-pat))
						   disc-pat))
					 ,new-sym)))))
			`(,discrim
			  ,@(if (symbolp pat)
				body
				`((dlet ((,pat ,new-sym))
				    ,@body))))))
		  clauses)))
    `(let ((,new-sym ,object))
       (cond
	;; If the last clause pattern is not an _, put in a t cond clause.
	,@(let ((last-disc-pat (skip-layered (first (first (last clauses))))))
	    (if (and (symbolp last-disc-pat)
		     (string= (symbol-name last-disc-pat) "_"))
		cond-clauses
		`(,@cond-clauses
		  (t (error "Ran out of cases for dcase given ~a"
			    ,new-sym)))))))))

(defun skip-layered (pat)
  "Skip over the layered (:as) variables in a pattern."
  (if (and (consp pat)
	   (eq (first pat) :as))
      (skip-layered (third pat))
      pat))

(defun constant-discriminator (pat object)
  "Build a condition that tests whether the constant pattern PAT
matches the OBJECT"
  `(,(typecase pat
       (number '=)
       (character 'char=)
       (string 'string=)
       (cons 'eq)			; (quote foo)
       (symbol 'eq)			; nil, t, pi, etc.
       (t (error "I don't know how to discriminate for the constant pattern ~s"
		 pat)))
    ,object ,pat))
