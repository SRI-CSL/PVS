;;; -*- Mode: Lisp;  Package: term; Log: term-changes.log  -*-
;;; Sccs Id @(#)terms.lisp	1.14 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Term ADT.

;;; Author: Conal Elliott 
;;; Extensively Revised: Scott Dietzen, Tue May 26 13:36:26 1987

;;; Conal Mon Jun 29 14:13:49 1987
;;;   You can now define your own printing function.  Just define a function 
;;;   called "term:print-term-hook" (taking term,stream,depth as usual).  It
;;;   may decide call the standard function "term:standard-print-term" to do
;;;   the work, e.g. if the term is not in your language, or if *print-escape*
;;;   is on and you don't want to override it.
;;;   
;;; Conal  Mon Oct 31 09:24:46 1988
;;;   Deleted code for primitives: mk-term, term-op, term-args, term-attr,
;;;   set-term-attr and put them in old-term-prims.lisp.  These have been
;;;   replaced by gterm.
;;;
;;; fp  Mon Jan  2 10:03:33 1989
;;;   Took out the use-package of :oper and :occ.  It lead to problems
;;;   since this file is compiled after other files which do NOT have a
;;;   use-package.

#-gcl
(defpackage "TERM")
(in-package "TERM") (use-package :ergolisp)

;;; (use-package '("OPER" "OCC"))  ; take out. fp Mon Jan  2 10:04:59 1989.
;;; In this files uses of the above packages are still prefixed for
;;; recognition. 

;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed. Therefore a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy prevents costly and unnecessary client recompilation
;;; every time this code is revised (particularly since there will be multiple
;;; complex clients).


(export '(term-argn term-arity
	  term-arg0 term-arg1 term-arg2 term-arg3 term-arg4
	  term-arg5 term-arg6 term-arg7 term-arg8 term-arg9
	  term-arity0? term-arity1? term-arity2? term-arity3? term-arity4?
	  term-arity5? term-arity6? term-arity7? term-arity8? term-arity9?
	  sexp-to-term term-to-sexp
	  print-term-hook sbrt-print-term-hook standard-print-term
	  *disable-auto-unparsing*
	  get-subterm replace-subterm replace-argn))

(import '(*print-level* *print-length*))


;;; Terms have a sexp printed representation.  Sexp's prefixed with #! are
;;; terms. 


(defvar *disable-auto-unparsing* nil)




;;; Auxiliary functions:

(defun term-argn (term n)
  "Get the Nth argument (child) of a term"
  (declare (type term term))
  (let ((args (term-args term)))
    (assert (> (length args) n) (n)
	    "The term ~S does not have a ~dth argument" term n)
    (elt args n)))

(eval-when (compile eval load)
  (defmacro term-arg0 (term)
    "Get the 0th argument (child) of a term"
    `(term-argn ,term 0))
  (defmacro term-arg1 (term)
    "Get the 1st argument (child) of a term"
    `(term-argn ,term 1))
  (defmacro term-arg2 (term)
    "Get the 2nd argument (child) of a term"
    `(term-argn ,term 2))
  (defmacro term-arg3 (term)
    "Get the 3rd argument (child) of a term"
    `(term-argn ,term 3))
  (defmacro term-arg4 (term)
    "Get the 4th argument (child) of a term"
    `(term-argn ,term 4))
  (defmacro term-arg5 (term)
    "Get the 5th argument (child) of a term"
    `(term-argn ,term 5))
  (defmacro term-arg6 (term)
    "Get the 6th argument (child) of a term"
    `(term-argn ,term 6))
  (defmacro term-arg7 (term)
    "Get the 7th argument (child) of a term"
    `(term-argn ,term 7))
  (defmacro term-arg8 (term)
    "Get the 8th argument (child) of a term"
    `(term-argn ,term 8))
  (defmacro term-arg9 (term)
    "Get the 9th argument (child) of a term"
    `(term-argn ,term 9))
) ;; eval-when




;;; Auxiliary functions:

(defun term-arity (term)
  "Number of TERM's arguments (sons)."
  (length (term-args term)))

(eval-when (compile eval load)
  (defmacro term-arity0? (term)
    "Does TERM have 0 arguments?"
    `(eql (term-arity ,term) 0))
  (defmacro term-arity1? (term)
    "Does TERM have 1 argument?"
    `(eql (term-arity ,term) 1))
  (defmacro term-arity2? (term)
    "Does TERM have 2 arguments?"
    `(eql (term-arity ,term) 2))
  (defmacro term-arity3? (term)
    "Does TERM have 3 arguments?"
    `(eql (term-arity ,term) 3))
  (defmacro term-arity4? (term)
    "Does TERM have 4 arguments?"
    `(eql (term-arity ,term) 4))
  (defmacro term-arity5? (term)
    "Does TERM have 5 arguments?"
    `(eql (term-arity ,term) 5))
  (defmacro term-arity6? (term)
    "Does TERM have 6 arguments?"
    `(eql (term-arity ,term) 6))
  (defmacro term-arity7? (term)
    "Does TERM have 7 arguments?"
    `(eql (term-arity ,term) 7))
  (defmacro term-arity8? (term)
    "Does TERM have 8 arguments?"
    `(eql (term-arity ,term) 8))
  (defmacro term-arity9? (term)
    "Does TERM have 9 arguments?"
    `(eql (term-arity ,term) 9))

) ;; eval-when





;;; Terms and occurences

(defun get-subterm (term occur)
  "Get the subterm of a term at an occurence"
  (get-subterm-aux term (occ:occ-path occur)))

(defun get-subterm-aux (term path)
  (if (null path)
      term
      (get-subterm-aux (term-argn term (car path))
		       (cdr path))))


(defun replace-subterm (term occur new-subterm)
  "Replace the subterm of a term at an occurence by the given one"
  (replace-subterm-aux term (occ:occ-path occur) new-subterm))

(defun replace-subterm-aux (term path new-subterm)
  (if (null path)
      new-subterm
      (replace-argn term
		    (car path)
		    (replace-subterm-aux
		     (term-argn term (car path))
		     (cdr path)
		     new-subterm))))

(defun replace-argn (term n new-argn)
  "Replace the nth arg of a term"
  (declare (type term new-argn))
  (mk-term (term-op term)
	   (replace-elt (term-args term) n new-argn)))


;;; General utility functions

(defun replace-elt (seq n x)
  "Replace the nth element of a seq by x nondestructively"
  (let ((new-seq (copy-seq seq)))
    (setf (elt new-seq  n) x)
    new-seq))





(defun sexp-to-term (sexp)
  "Convert an sexp into a term."
  (mk-term (oper:sexp-to-oper (car sexp)) (mapcar #'sexp-to-term (cdr sexp))))

(defun term-to-sexp (term)
  "Convert an sexp into a term."
  (cons (oper:oper-to-sexp (term-op term))
	(mapcar #'term-to-sexp (term-args term))))


(defun term-to-sexp-aux (term depth)
  "Convert a term into an sexp with ellipsis."
  (cond ((and *print-level*
	      (> depth *print-level*))
	 '|...|)
	(t
	 (cons (oper:oper-to-sexp (term-op term))
	       (do ((args (term-args term) (cdr args))
		    (length 0 (1+ length))
		    (result nil
			    (cons (if (and *print-length*
					   (> length *print-length*))
				      '|...|
				      (term-to-sexp-aux (car args) (1+ depth)))
				  result)))
		   ((or (null args)
			(and *print-length*
			     (> length *print-length*)))
		    (nreverse result)))))))

(defun new-print-term (term stream depth)
  "Uses term-to-sexp to show a nice printed representation of TERM."  
  (declare (ignore depth))
  (if (fboundp 'print-term-hook)
      (print-term-hook term stream depth)
      (if (and (not *disable-auto-unparsing*)
	       (fboundp 'sbrt-print-term-hook))
	  (sbrt-print-term-hook term stream depth)
	  (standard-print-term term stream depth))))

(defun standard-print-term (term stream depth)
  "The standard printing function for terms"
  (write-string "#!" stream)
  (write (term-to-sexp-aux term depth) :stream stream :pretty nil))

(defun read-sexp-to-term (stream subchar arg)
  (declare (ignore subchar arg))
  (sexp-to-term (read stream)))

(defun ergolisp::\#! ()
  (set-dispatch-macro-character #\# #\! #'read-sexp-to-term))

(eval-when (load eval)
  (ergolisp::\#!))

