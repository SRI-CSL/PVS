;;; -*- Mode: Lisp;  Package: oper; Log: oper-changes.log  -*-
;;; Sccs Id @(#)opers.lisp	1.8 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Operator ADT originally defined for the Syntax Box. 

;;; Scott Dietzen, Thu Apr 23 00:16:13 1987

#-gcl
(defpackage :oper #+sbcl (:use :common-lisp :ergolisp))
(in-package :oper) #-sbcl (use-package :ergolisp)


;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed, so a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy is necessary so that unnecessary recompilation does
;;; not insue every time this code is revised.  Particularly since so many
;;; clients depend on it. 



(export '(oper operp
	  check-oper-type
          oper-to-sexp sexp-to-oper 
	  oper-equal

	  mk-sim-op is-sim-op ds-sim-op
	  mk-leaf-op is-leaf-op ds-leaf-op leaf-op-kind leaf-op-value

	  mk-id-op  is-id-op  ds-id-op
	  mk-cid-op is-cid-op ds-cid-op
	  mk-num-op is-num-op ds-num-op
	  mk-str-op is-str-op ds-str-op
	  mk-lit-op is-lit-op ds-lit-op
	  mk-key-op is-key-op ds-key-op
	  ))




;;; Operators are given a sexp printed representation.  Sexps prefixed with
;;; #^ are operators. 




;;; The representation. 

(defstruct (oper-struct (:predicate operp)
			(:print-function print-oper))
  kind
  arg
  )

#+lcl4.1
(defmethod make-load-form ((obj oper-struct))
  (make-load-form-saving-slots obj))

#+(or allegro cmu sbcl)
(defmethod make-load-form ((obj oper-struct) &optional environment)
  (make-load-form-saving-slots obj))

(deftype oper ()
  "The type of operators"
  'oper-struct)


(defun check-oper-type (op)
  "Make sure OP is an operator, otherwise give an error message."
  (check-type op oper "an operator"))


  


;;; Primitive ADT operators:

;;; The following where left macros because they are not exported, and
;;; hopefully it will allow the removal of most type checks. 


(defun mk-oper (kind arg)
  "Build an oper from a kind and an arg."
  (declare (type keyword kind))
  (make-oper-struct :kind kind :arg arg))

(defun oper-kind (oper)
  "Get an oper's kind."
  (declare (type oper oper))
  (oper-struct-kind oper))

(defun oper-arg (oper)
  "Get an oper's argument."
  (declare (type oper oper))
  (oper-struct-arg oper))


(declare-constructor mk-oper (oper-kind oper-arg))



;;; ADT routines 

;;; Regular symbol operators for terms:

(defun mk-sim-op (value)
  "Makes a simple symbol or string operators."
  (assert (or (symbolp value)
	      (stringp value)))
  (mk-oper :op value))
(defun is-sim-op (x)
  "Is the argument a simple operator."
  (and (operp x)
       (eq (oper-kind x) :op)))
(defun ds-sim-op (op)
  "Return the symbol or string associated with the simple operator."
  (oper-arg op))

(declare-constructor mk-sim-op (ds-sim-op) is-sim-op)



(defun mk-leaf-op (kind value)
  "Constructs an operator for a term tree leaf (i.e. lexical terminal)."
  (mk-oper kind value))
(defun is-leaf-op (x)
  "Is the argument an operator of leaf nodes in the term tree?"
  (and (operp x)
       (not (eq (oper-kind x) :op))))
(defun ds-leaf-op (op)
  "Return the lisp value associated with the leaf operator."
  (values (oper-kind op)
	  (oper-arg op)))
(defun leaf-op-kind (op)
  "Return the kind associated with the leaf operator."
  (oper-kind op))
(defun leaf-op-value (op)
  "Return the lisp value associated with the leaf operator."
  (oper-arg op))

(declare-constructor mk-leaf-op (leaf-op-kind leaf-op-value) is-leaf-op)



(defun oper-equal (op1 op2)
  "Determine if two operators are equivalent."
  (and (eq (oper-kind op1)
	   (oper-kind op2))
       (equal (oper-arg op1)
	      (oper-arg op2))))





;;; Leaf operators for term trees: 

 
(defun mk-id-op (id)
  "Make an identifier operator."
  (declare (type symbol id))
  (mk-oper :id id))
(defun is-id-op (x)
  "Is the argument an identifier operator?"
  (and (operp x)
       (eq (oper-kind x) :id)))
(defun ds-id-op (op)
  "Return the symbol associated with the identifier operator."
  (oper-arg op))
(declare-constructor mk-id-op (ds-id-op) is-id-op)

(defun mk-cid-op (cid)
  "Make a case-sensitive identifier operator."
  (declare (type string cid))
  (mk-oper :cid cid))
(defun is-cid-op (x)
  "Is the operator argument a case-sensitive identifier operator?"
  (and (operp x)
       (eq (oper-kind x) :cid)))
(defun ds-cid-op (op)
  "Return the string associated with the case-sensitive identifier operator."
  (oper-arg op))
(declare-constructor mk-cid-op (ds-cid-op) is-cid-op)

(defun mk-num-op (num)
  "Make a number operator."
  (declare (type number num))
  (mk-oper :num num))
(defun is-num-op (x)
  "Is the argument a number operator?"
  (and (operp x)
       (eq (oper-kind x) :num)))
(defun ds-num-op (op)
  "Return the symbol associated with the number operator."
  (oper-arg op))
(declare-constructor mk-num-op (ds-num-op) is-num-op)


;;; Leaf operators for term trees (continued).


(defun mk-str-op (str)
  "Make a string operator."
  (declare (type string str))
  (mk-oper :str str))
(defun is-str-op (x)
  "Is the argument a string operator?"
  (and (operp x)
       (eq (oper-kind x) :str)))
(defun ds-str-op (op)
  "Return the symbol associated with the string operator."
  (oper-arg op))
(declare-constructor mk-str-op (ds-str-op) is-str-op)

(defun mk-lit-op (lit)
  "Make a literal operator."
  (declare (type symbol lit))
  (mk-oper :lit lit))
(defun is-lit-op (x)
  "Is the argument a literal operator?"
  (and (operp x)
       (eq (oper-kind x) :lit)))
(defun ds-lit-op (op)
  "Return the symbol associated with the literal operator."
  (oper-arg op))
(declare-constructor mk-lit-op (ds-lit-op) is-lit-op)

(defun mk-key-op (key)
  "Make a keyword operator."
  ;;(declare (type symbol key))
  (mk-oper :key key))
(defun is-key-op (x)
  "Is the argument a keyword operator?"
  (and (operp x)
       (eq (oper-kind x) :key)))
(defun ds-key-op (op)
  "Return the symbol associated with the keyword operator."
  (oper-arg op))
(declare-constructor mk-key-op (ds-key-op) is-key-op)




(defun oper-to-sexp (oper)
  "Convert an oper into a sexp."
  (case (oper-kind oper)
    (:op
     (ds-sim-op oper))
    (t
     (list (oper-kind oper)
	   (oper-arg oper)))))


(defun sexp-to-oper (x)
  "Convert an sexp into an oper."
  (cond ((symbolp x)
	 (mk-sim-op x))
	((stringp x)
	 (mk-sim-op x))
	((listp x)
	 (case (car x)
	   (:op (mk-sim-op (cadr x)))
	   (:id (mk-id-op (cadr x)))
	   (:cid (mk-cid-op (cadr x)))
	   (:num (mk-num-op (cadr x)))
	   (:str (mk-str-op (cadr x)))
	   (:lit (mk-lit-op (cadr x)))
	   (:key (mk-key-op (cadr x)))
	   (t
	    (mk-leaf-op (car x) (cadr x)))))
	(t
	 (error "The sexp for an operator must be a symbol or list."))))



(defun print-oper (oper stream depth)
  "Uses oper-to-sexp to show a nice printed representation of OPER."  
  (declare (ignore depth))
  (write-string "#^" stream)
  (write (oper-to-sexp oper) :stream stream :pretty nil))

(defun read-sexp-to-oper (stream subchar arg)
  (declare (ignore subchar arg))
  (sexp-to-oper (read stream)))

(defun ergolisp::\#^ ()
  (set-dispatch-macro-character #\# #\^ #'read-sexp-to-oper))

(eval-when (load eval)
  (ergolisp::\#^))


