;;; -*- Mode: Lisp; Package: GTERM -*-
;;;
;;; Author: Conal Elliott
;;;
;;; Sccs Id 9/26/89 @(#)gterm.lisp	1.23
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;
;;;
;;; Generic terms, supporting multiple representations.
;;;
;;; Author: Conal.  Last Modified Wed Feb 15 09:32:12 1989
;;;
;;; Implementation of term construction, i.e. "(term op args)" is based on
;;; the sort of OP.  Implementation of term-op and term-args is based on the
;;; Lisp type of the representation.
;;;
;;; Set up for the destructuring macros dlet, ddefun, etc.  For instance,
;;; say (dlet (( (mk-term my-op my-args) my-term)) ... )
;;;
;;; We let the user decide how to cache attributes at the same time he
;;; decides how to implement his terms.  TSF.
;;;
;;; I think all the mk-term implementations should take an operator symbol
;;; instead of a real operator.
;;; 

(in-package "TERM" :nicknames '("GTERM")) (use-package :ergolisp)

(export '(Term  termp mk-term ds-term term-op term-args
		term-attr set-term-attr))
(export '(*default-mk-term-impl* *default-ds-term-impl*
	  *default-get-term-attr-impl* *default-set-term-attr-impl*))
(export '(nil-get-term-attr-impl nil-set-term-attr-impl))

(export '(mk-term-impl ds-term-impl))
(export '(defrep defrep-case defrep-cases))
(export '(defrep-oneway defrep-case-oneway defrep-cases-oneway))
(export '(expand-constr-cases))
(export '(trace-rep-functions untrace-rep-functions))
(export '(term-attr get-term-attr-impl set-term-attr-impl))

(export '(*cache-ds-term-p* clear-ds-term)) ; ds-term caching stuff

(export '(op-arglist op-args))

(export '(oper-sort))		; should be elsewhere

;;;  Primitive Term support

(deftype Term ()
  "Generic terms, supporting multiple representations.  See termp."
  `(satisfies termp))

(defun termp (obj)
  "Is OBJ a term?  True iff there has been a ds-term-impl installed
for (type-of OBJ)."
  (declare-ftype termp (T) Boolean)
  (not (eq (ds-term-impl (type-of obj))
	   'ds-bogus-term)))

(deftype term-attr ()
  "The type for values of the attribute slot.  Don't know the real
value of this, so I use T."
  T)

(defun mk-term (op args)
  "Construct a term out of OP and ARGS"
  (declare-ftype mk-term (oper:Oper (List-of Term)) Term)
  ;; The function "oper-sort" is fictitious.  I don't know how we are
  ;; supposed to get the language and/or sort.
  (funcall (mk-term-impl (oper-sort op))
	   op
	   args))

(defun term-attr (term)
  "Get the attribute slot of the term."
  (declare-ftype term-attr (Term) term-attr)
  (funcall (get-term-attr-impl (type-of term)) term))

(defun set-term-attr (term slot)
  "Set the attribute slot of the term."
  (declare-ftype set-term-attr (Term term-attr))
  (funcall (set-term-attr-impl (type-of term)) term slot)
  (values))

(defsetf term-attr set-term-attr)



(defun nil-get-term-attr-impl (term)
  "A do-nothing implementation of get-term-attr.  Bind
*default-get-term-attr-impl* to this to turn off default attribute hashing."
  (declare (ignore term))
  nil)

(defun nil-set-term-attr-impl (term slot)
  "A do-nothing implementation of set-term-attr.  Bind
*default-set-term-attr-impl* to this to turn off default attribute hashing."
  (declare (ignore term slot)))

(defsetf term-attr set-term-attr)  

;;; This implementation of ds-term optionally does caching.  This is extremely
;;; useful with the current implementation of the SB, whose generated unparsers
;;; can call term-op many many many times on the same term while deciding which
;;; branch of an alternation to use.

(defparameter *cache-ds-term-p* nil
  "Whether to cache the results of ds-term")

(defvar *ds-term-alist* ()
  "The alist used for ds-term caching.  Maps a Term to a cons of an op
and list of args.")

(defun ds-term (term)
  "Get the operator and args (as multi values) of a generic term.  This version
does caching if the variable *cache-ds-term-p* is true."
  (if *cache-ds-term-p*
      (let ((cached (cdr (assoc term *ds-term-alist* :test #'EQ))))
	(if cached
	    (values (car cached) (cdr cached))
	    (progn
	      (dlet ( ((values op args) (do-ds-term term)) )
		(push (cons term (cons op args)) *ds-term-alist*)
		(values op args)))))
      (do-ds-term term)))
  
;;; The following function is only called by ds-term, but makes it possible
;;; to better trace the effort expended.
(defun do-ds-term (term)
  "Do the work of ds-term, calling the established implementation."
  (declare-ftype do-ds-term (Term) oper:Oper (List-of Term))
  (funcall (ds-term-impl (type-of term))
	   term))

(defun clear-ds-term ()
  "Clear out all cached results of ds-term."
  (setq *ds-term-alist* ()))

(defun term-op (term)
  "Get the operator of a generic term"
  (declare-ftype term-op (Term) oper:Oper)
  (multiple-value-bind (op args) (ds-term term)
    (declare (ignore args))
    op))

(defun term-args (term)
  "Get the args of a generic term"
  (declare-ftype term-args (Term) (List-of Term))
  (multiple-value-bind (op args) (ds-term term)
    (declare (ignore op))
    args))


;;; Declare selectors, so that dlet, etc will work.  There ought to be
;;; a way to only end up calling ds-term once when destructuring a term,
;;; but how to do it?  See notes at end of dlambda.txt.

(declare-constructor mk-term (term-op term-args))

;;;; Multi-representation support.

(defvar *default-mk-term-impl* 'mk-default-term
  "The default mk-term implementation.")

(defvar *default-ds-term-impl* 'ds-bogus-term
  "The default ds-term implementation.")

(defvar *default-get-term-attr-impl* 'get-hash-attribute)

(defvar *default-set-term-attr-impl* 'set-hash-attribute)

;;; Functions stored on property lists.  Could instead be in hash tables.

(defun mk-term-impl (sort-sym)
  "Given a sort, return the term construction function.  Setf-able."
  (declare-ftype mk-term-impl
    (Symbol) (Afunction (oper:Oper (List-of Term)) Term))
  (get sort-sym 'mk-term-impl *default-mk-term-impl*))

(defun ds-term-impl (type-spec)
  "Given a lisp type, return the term op extraction function.  Setf-able if the
type is a symbol."
  (declare-ftype ds-term-impl (T) (Afunction (Term) oper:Oper))
  (if (symbolp type-spec)
      (get type-spec 'ds-term-impl *default-ds-term-impl*)
      *default-ds-term-impl*))

(defsetf mk-term-impl (sort-sym) (fun)
  `(setf (get ,sort-sym 'mk-term-impl) ,fun))

(defsetf ds-term-impl (type-sym) (fun)
  `(setf (get ,type-sym 'ds-term-impl) ,fun))

(defun get-term-attr-impl (sort-sym)
  "Given a sort, return a function to fetch the attribute slot.  Setf-able."
  (declare-ftype set-term-attr-impl (Symbol) (Afunction (Term) T))
  (get sort-sym 'get-term-attr-impl *default-get-term-attr-impl*))

(defsetf get-term-attr-impl (sort-sym) (fun)
  `(setf (get ,sort-sym 'get-term-attr-impl) ,fun))

(defun set-term-attr-impl (sort-sym)
  "Given a sort, return a function to fetch the attribute slot.  Setf-able."
  (declare-ftype set-term-attr-impl (Symbol) (Afunction (Term T)))
  (get sort-sym 'set-term-attr-impl *default-set-term-attr-impl*))

(defsetf set-term-attr-impl (sort-sym) (fun)
  `(setf (get ,sort-sym 'set-term-attr-impl) ,fun))

(defun ds-bogus-term (obj)
  "Complain about trying to call ds-term on an object whose type hasn't been
established as a term representation."
  (error "Trying to call ds-term on object ~S of type ~A not established
as a term representation."
	 obj (type-of obj)))


;;;; Default representation

(defstruct (Default-Term (:predicate default-termp)
			 (:print-function print-default-term)
			 (:constructor mk-default-term (op args)))
  op					; The operator (:Oper)
  args					; List of arguments (:(List-of Term)).
  attribute-slot
  )

(proclaim-ftype mk-default-term (oper:Oper (List-of Term)) Default-Term)

(defun print-default-term (term stream depth)
  ;; Simple-minded default-term-printer.  Haven't been able to get
  ;; term-to-sexp to work with mixed terms.  Why?
  (if (not *disable-auto-unparsing*)
      (sbrt-print-term-hook term stream depth)
      (standard-print-term term stream depth)))

(defun standard-print-term (term stream depth)
  "The standard printing function for terms"
  (declare (ignore depth))
  (write-string "#!" stream)
  (write (term-to-sexp term) :stream stream :pretty nil))

(defun term-to-sexp (term)
  "Convert term into a sexp."
  (if (default-termp term)
      (cons (oper:oper-to-sexp (term-op term))
	    (mapcar #'term-to-sexp (term-args term)))
      term))

(defun ds-default-term (term)
  "default-term version of getting the operator and arguments of a term."
  (declare-ftype ds-default-term
    (Default-Term) oper:Oper (List-of Term))
  (values (default-term-op term)
	  (default-term-args term)))

;;; Interface to generic terms.  This sets up things so that any term
;;; represented by the lisp type defined here, default-term:Term, will be
;;; destructed correctly.

(setf (ds-term-impl 'Default-Term) 'ds-default-term)

(setf (get-term-attr-impl 'Default-Term) 'get-default-attribute-slot)

(setf (set-term-attr-impl 'Default-Term) 'set-default-attribute-slot)

(defun get-default-attribute-slot (term)
  (default-term-attribute-slot term))

(defun set-default-attribute-slot (term value)
  (setf (default-term-attribute-slot term) value))
					 

;;;; Macros for defining term representations.

(defun op-sym-arg () (intern "OP-SYM"))
(defun args-arg () (intern "ARGS"))

(defun constr-name (sort-sym)
  (intern (format nil "~:@(~a~)-MK-TERM-IMPL" sort-sym)))

(defun ds-name (sort-sym)
  (intern (format nil "~:@(~a~)-DS-TERM-IMPL" sort-sym)))

(defvar *rep-functions* ()
  "List of all defined representation functions.  Used by
(trace-rep-functions) and (untrace-rep-functions)")

(defmacro defrep (sort-sym rep-type constr-body ds-body)
  "Establish a term representation.  Associate a mk-term function with the
sort SORT-SYM that binds \"op-sym\" to the operator's symbol part and
\"args\" with the term args, and then evaluates CONSTR-BODY.  Associate
a ds-term function with REP-TYPE that binds the value of SORT-SYM to
the term and evaluates DS-BODY.  Also, add the new mk-term function to
list used by trace-rep-functions and untrace-rep-functions."
  (let ((mk (constr-name sort-sym))
	(ds (ds-name sort-sym)))
    `(progn
       (ddefun ,mk ((oper:mk-sim-op ,(op-sym-arg)) ,(args-arg))
	 (ergo-ignore-if-unused ,(op-sym-arg) ,(args-arg))
	 ,constr-body)
       (defun ,ds (,sort-sym)
	 ,ds-body)
       (setf (mk-term-impl ',sort-sym) ',mk)
       (setf (ds-term-impl ',rep-type) ',ds)
       (pushnew ',mk *rep-functions*)
       ;; Next line commented out because in printing its argument,
       ;; a trace of ds-term calls ds-term
       ;; (pushnew ',ds *rep-functions*)
       t)))

(defmacro defrep-oneway (sort-sym constr-body)
  "Version of defrep that declares how to make but not destruct terms."
  `(defrep ,sort-sym BogusRep
     ,constr-body
     (ds-default-term ,sort-sym)))

(defmacro trace-rep-functions ()
  "Turn on tracing of all representation functions"
  `(trace ,@*rep-functions*))

(defmacro untrace-rep-functions ()
  "Turn off tracing of all representation functions"
  `(untrace ,@*rep-functions*))

(ddefun expand-constr-case ( (cons arg-pats body) )
  "Expand one constr case."
  `(dlet (( (list ,@arg-pats)
	    ,(args-arg)))
     ,@body))

(ddefun expand-constr-cases (cases default-constr)
  `(case ,(op-sym-arg)
     ,@(mapcar (fdlambda ((cons (cons op-sym arg-pats) body))
		 `(,op-sym
		   ,(expand-constr-case `(,arg-pats ,@body))))
	       cases)
     (t (funcall ,default-constr (oper:mk-sim-op ,(op-sym-arg)) ,(args-arg)))))

(defmacro defrep-case (sort-sym rep-type constr-spec ds-spec)
  `(defrep ,sort-sym ,rep-type
     ,(expand-constr-case constr-spec)
     (dcase ,sort-sym
       ,ds-spec)))

(defmacro defrep-cases (sort-sym rep-type constr-cases ds-dcases
				 &optional (ds-default 'ds-default-term))
  `(defrep ,sort-sym ,rep-type
     ,(expand-constr-cases constr-cases '*default-mk-term-impl*)
     (dcase ,sort-sym
       ,@ds-dcases
       (_ (,ds-default ,sort-sym)))))



(defmacro defrep-cases-oneway (sort-sym &rest constr-cases)
  "Version of defrep-cases that declares how to make but not destruct terms."
  `(defrep-cases ,sort-sym BogusRep
     ,constr-cases
     ()))


(defmacro defrep-case-oneway (sort-sym &rest constr-spec)
  "Version of defrep-case that declares how to make but not destruct terms."
  `(defrep-case ,sort-sym BogusRep
     ,constr-spec
     ()))

;;;; Misc useful utilities

;;; Standard Gterm support

(defun op-arglist (opsym args)
  "Returns as two values (1) a simple operator made from OP-SYM, and (2) the
list ARGS of term args."
  (values (oper:mk-sim-op opsym) args))

(defun op-args (opsym &rest args)
  "Like op-arglist, but takes the term args as a &rest argument."
  (op-arglist opsym args))


;;;; Misc other stuff

(defun oper-sort (op)
  "Map an operator to the symbol part of its result sort.  Returns silly
sort if none known"
  (declare-ftype oper-sort (oper:Oper) Symbol)
  (let ((opsig (sort:opsig-table-lookup op)))
    (if opsig
	(let ((sort (sort:ds-sort-ttype (sort:opsig-output opsig))))
	  (if (symbolp sort)
	      sort
	      'nonsymbol-sort-I-realize-this-is-a-hack))
	'unknown-sort-I-realize-this-is-a-hack)))

#| (do-declare-ftypes) Don't know what this is supposed to do.  TSF. |#

#| Test case for attributes.
(progn
  ;; For default terms, the attributes should be stored in the slots.
  (setq foo (mk-term #^foo nil))
  (format t "Default term looks like ~s.~%" foo)
  (setf (term-attr foo) 3)
  (attr-clear-all)
  (unless (equal (term-attr foo) 3)
    (error "Attributes for default terms are broken."))
  ;; Make a term implementation.
  (defun mk-cons-term (oper args) (list* oper nil args))
  (defun ds-cons-term (term)
    (values (car term) (cddr term)))
  (defun get-cons-term-attr (term) (cadr term))
  (defun set-cons-term-attr (term value)
    (setf (cadr term) value))
  (setf (mk-term-impl 'cons) 'mk-cons-term)
  (setf (ds-term-impl 'cons) 'ds-cons-term)
  (sort:opsig-table-insert
   (oper:mk-sim-op 'cons)
   (sort:mk-opsig "silly-test" (list (sort:mk-sort-ttype 'cons)
				     (sort:mk-sort-ttype 'cons))
		  (sort:mk-sort-ttype 'cons)))
  (setq term (mk-term #^cons (list 5 6)))
  (format t "Made the term ~s.~%" term)
  (setf (term-attr term) 8) ;; Should store it in the hash table; no
			    ;; effect on the term.
  (format t "After setting the undefined slot, term is ~s.~%" term)
  (format t "Fetching undefined slot, value is ~s.~%"
	  (term-attr term))
  (attr-clear-one term)
  (format t
	  "Fetching undefined slot after clearing the table, value is ~s.~%"
	  (term-attr term))
  (setf (get-term-attr-impl 'cons) 'get-cons-term-attr)
  (setf (set-term-attr-impl 'cons) 'set-cons-term-attr)
  (setf (term-attr term) 3)
  ;; Now it should be doing destructive operations on the term.
  (format t "Set slot to 3, term is ~s.~%" term)
  (format t "Attribute slot is ~s.~%" (term-attr term)))
|#
