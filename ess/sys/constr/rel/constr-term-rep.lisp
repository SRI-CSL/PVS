;;; -*- Mode: Lisp; Package: CONSTR-TERM-REP -*-
;;;
;;; Syntactic representation for constr-gr.txt
;;;
;;; Author: Conal Elliott.  Last Modified Mon Sep 25 09:51:28 1989
;;;
;;; Sccs Id @(#)constr-term-rep.lisp	1.6 9/25/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

#-gcl
(defpackage :constr-term-rep #+sbcl (:use :common-lisp :ergolisp))
(in-package :constr-term-rep)
#-sbcl (use-package :ergolisp)

(ddefun mk-items (_ items)
  `(progn ,@items))

(ddefun mk-item (_ (list arg))
  arg)

(ddefun mk-abbrev (_ (list (term:mk-id name) ptype))
  `(deftype ,(reintern name) () ',ptype))

(ddefun mk-datatype ((oper:mk-sim-op op-sym) args)
  (ecase op-sym
    (mconstr
     (dlet ( ((list (term:mk-id name) (term:mk-sim-term _ constrs)) args) )
       `(defmulticonstr ,(reintern name)
	  ,constrs)))
    (constr
     (dlet ( ((list constr) args) )
       `(defconstr ,@constr)))))

(ddefun mk-dtarm (_ (list (term:mk-id name) (term:mk-sim-term _ parts)))
  `(,(reintern name) ,parts))

(ddefun mk-part (_ (list (term:mk-id part-name) ptype))
  `(,(reintern part-name) . ,(fix-prods ptype)))

(ddefun mk-ptype ((oper:mk-sim-op op-sym) args)
  (ecase op-sym
    (ptype-con
     (dlet ( ((list (term:mk-id (:as symbol (intern name _)))) args) )
	     ;; Look in the name for a colon, and, if found, generate an intern
	     ;; expression.   Otherwise, return just the symbol.
	     (let ((colon-pos (position #\: name)))
	       (if colon-pos
		   (intern (subseq name (1+ colon-pos))
			   (subseq name 0 colon-pos))
		   (reintern symbol)))))
    (ptype-appl
     (dlet ( ((list rator rand) args) )
       ;; Add rand to the end, after making rator a list if it isn't yet.
       ;; Could be more clever, building up the lisp types backwards and
       ;; then reversing them where a real type is used.
       (let ((rator (if (listp rator)
			rator
			(list rator))))
	 (append rator (list rand)))))
    (ptype-fun
     (dlet ( ((list (term:mk-sim-term _ domains)) args) )
       `(Function-of ,domains)))
    (ptype-tv
      (dlet ( ((list (term:mk-id tvar-name)) args) )
	;; For now, don't do anything interesting with type variables.
	;;`(Tv ,tvar-name)
	(declare (ignore tvar-name))
	`T
	))
    (ptype-members
     `(member ,@args))))

(defun reintern (symbol)
  "Re-intern SYMBOL into the current package if it is not one of 
 (arrow unit prod)"
  (case symbol
    ((arrow unit prod)
     symbol)
    (t (intern (symbol-name symbol)))))


(defun fix-prods (type)
  "Go through TYPE, to change `A1 # .. # An -> B1 # ... # Bm' to 
`Function-of (A1 ... An) B1 ... Bm', (including `1' where n or m is 0)."
  (labels ((fix-rec (subtype)
	     (cond
	      ((consp subtype)
	       (case (first subtype)
		 (arrow
		  `(Function-of ,(fix-arrow-prods (second subtype))
				,@(fix-arrow-prods (third subtype))))
		 (prod
		  (warn "Product type ~a appears not on the left or right of an arrow in the type ~a"
			subtype type))
		 (t (cons (first subtype)
			  (mapcar #'fix-rec (rest subtype))))))
	      ((eq subtype 'unit)
	       (warn "A unit type appears not on the left or right of an arrow in ~a"
		     type))
	      (t subtype)))
	   (fix-arrow-prods (subtype)    
	     ;; Convert top-level product and unit types in SUBTYPE to a list
	     ;; of types.
	     (cond ((consp subtype)
		    (case (first subtype)
		      (prod
		       (cons (fix-rec (second subtype))
			     (fix-arrow-prods (third subtype))))
		      (t (cons (first subtype)
			       (mapcar #'fix-rec (rest subtype))))))
		   ((eq subtype 'unit)
		    ())
		   (t `(,subtype)))))
    (fix-rec type)))



(setf (gterm:mk-term-impl 'items) 'mk-items)
(setf (gterm:mk-term-impl 'item) 'mk-item)
(setf (gterm:mk-term-impl 'abbrev) 'mk-abbrev)
(setf (gterm:mk-term-impl 'datatype) 'mk-datatype)
(setf (gterm:mk-term-impl 'dtarm) 'mk-dtarm)
(setf (gterm:mk-term-impl 'part) 'mk-part)
(setf (gterm:mk-term-impl 'ptype) 'mk-ptype)
