;;; -*- Mode: Lisp;  Package: term; Log: term-changes.log  -*-
;;; Sccs Id @(#)termop.lisp	1.7 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Term and Operator Routines

;;; Scott Dietzen, Tue May 26 13:36:26 1987

(defpackage "TERM")
(in-package "TERM") (use-package :ergolisp)


(use-package '("OPER" "OCC"))


;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed. Therefore a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy prevents costly and unnecessary client recompilation
;;; every time this code is revised (particularly since there will be multiple
;;; complex clients).


(export '(term-equal
	  search-list-for-term 
	  search-list-for-term-with-subterm

	  mk-sim-term is-sim-term sim-term-op 
	  mk-leaf-term is-leaf-term leaf-term-kind leaf-term-value

	  mk-id is-id ds-id 
	  mk-cid is-cid ds-cid 
	  mk-number is-number ds-number 
	  mk-string is-string ds-string 
	  mk-literal is-literal ds-literal 
	  mk-keyword is-keyword ds-keyword 

	  is-op ck-term ck-term-op
	  is-sop ck-term-sop memq-sop))




;;; General utility.


(defun term-equal (t1 t2)
  "See if two terms are equal"
  (and (oper:oper-equal (term-op t1) (term-op t2))
       (every #'term-equal (term-args t1) (term-args t2))))

(defun search-list-for-term (list funct)
  "Given a LIST of terms, return the first term such that FUNCT is true."
  (do ((L list (cdr L)))
      ((or (null L)
	   (and (term:termp (car L))
		(funcall funct (car L))))
       (car L))))

(defun search-list-for-term-with-subterm (list funct n)
  "Given a LIST of terms, return the first term such that FUNCT is true of 
   that term's Nth subterm."
  (do ((L list (cdr L)))
      ((or (null L)
	   (and (term:termp (car L))
		(term:termp (term:term-argn (car L) n))
		(funcall funct (term:term-argn (car L) n))))
       (car L))))




(defun mk-sim-term (s args)
  "Make a standard term with operator S (symbol or string) and argument ARGS."
  (mk-term (mk-sim-op s) args))
(defun is-sim-term (x)
  "Is X a standard term with a symbol operator?"
  (and (termp x)
       (is-sim-op (term-op x))))
(defun sim-term-op (x)
  "Returns the symbol which was the operator of a term."
  (ds-sim-op (term-op x)))
(declare-constructor mk-sim-term (sim-term-op term-args) is-sim-term)

(defun mk-leaf-term (kind value)
  "Construct term leaf (i.e. lexical terminal) with operator from KIND 
   and VALUE."
  (mk-term (mk-leaf-op kind
		       value)
	   nil))
(defun is-leaf-term (x)
  "Is X a leaf term (i.e. lexical terminal)?"
  (and (termp x)
       (is-leaf-op (term-op x))))
(defun leaf-term-kind (x)
  "Return the kind of the operator of TERM."
  (leaf-op-kind (term-op x)))
(defun leaf-term-value (x)
  "Return the value of the operator of TERM."
  (leaf-op-value (term-op x)))
(declare-constructor mk-leaf-term (leaf-term-kind leav-term-value)
		     is-leaf-term)
 
(defun mk-id (symbol)
  "Make an id-term from SYMBOL."
  (mk-term (mk-id-op symbol) ()))
(defun is-id (x)
  "Is X an id-term?"
  (and (termp x)
       (is-id-op (term-op x))))
(defun ds-id (x)
  "Return the symbol associated with id-term X."
  (ds-id-op (term-op x)))
(declare-constructor mk-id (ds-id) is-id)

(defun mk-cid (string)
  "Make an cid-term from STRING."
  (mk-term (mk-cid-op string) ()))
(defun is-cid (x)
  "Is X an cid-term?"
  (and (termp x)
       (is-cid-op (term-op x))))
(defun ds-cid (x)
  "Return the string associated with cid-term X."
  (ds-cid-op (term-op x)))
(declare-constructor mk-cid (ds-cid) is-cid)

(defun mk-number (num)
  "Make a number-term from NUMBER."
  (mk-term (mk-num-op num) ()))
(defun is-number (x)
  "Is X a number-term?"
  (and (termp x)
       (is-num-op (term-op x))))
(defun ds-number (x)
  "Return the number associated with number-term X."
  (ds-num-op (term-op x)))
(declare-constructor mk-number (ds-number) is-number)

(defun mk-string (str)
  "Make a string-term from STR."
    (mk-term (mk-str-op str) ()))
(defun is-string (x)
  "Is X a string-term?"
  (and (termp x)
       (is-str-op (term-op x))))
(defun ds-string (x)
  "Return the string associated with string-term X."
  (ds-str-op (term-op x)))
(declare-constructor mk-string (ds-string) is-string)

(defun mk-literal (symbol)
  "Make a literal-term from SYMBOL."
  (mk-term (mk-lit-op symbol) ()))
(defun is-literal (x)
  "Is X a literal-term?"
  (and (termp x)
       (is-lit-op (term-op x))))
(defun ds-literal (x)
  "Return the symbol associated with literal-term X."
  (ds-lit-op (term-op x)))
(declare-constructor mk-literal (ds-literal) is-literal)

					; Only used for meta grammar. 
(defun mk-keyword (symbol)
  "Make a keyword-term from SYMBOL."
  (mk-term (mk-key-op symbol) ()))
(defun is-keyword (x)
  "Is X a keyword-term?"
  (and (termp x)
       (is-key-op (term-op x))))
(defun ds-keyword (x)
  "Return the symbol associated with the keyword-term X."
  (ds-key-op (term-op x)))
(declare-constructor mk-keyword (ds-keyword) is-keyword)



;;  Functions for ADT ease of use.

(defun is-op (op term)
  (if (oper-equal (term-op term) op)
      term))

(defun ck-term (funct term)   
  (assert (funcall funct term))
  term)

(defun ck-term-op (op term)
  (assert (is-op op term))
  term)

(defun is-sop (sop term)
  (if (eq (sim-term-op term) sop)
      term))

(defun ck-term-sop (sop term)
  (assert (is-sop sop term))
  term)

(defun memq-sop (term sym-list)
  (member (sim-term-op term)
	  sym-list
	  :test #'eq))

