;;; -*- Mode: Lisp;  Package: occ; Log: occ-changes.log  -*-
;;; Sccs Id @(#)occur.lisp	1.5 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, The ADT of occurences

;;; Scott Dietzen, Tue May 26 14:30:09 1987

#-gcl
(defpackage "OCC")
(in-package "OCC")  (use-package :ergolisp)


;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed. Therefore a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy prevents costly and unnecessary client recompilation
;;; every time this code is revised (particularly since there will be multiple
;;; complex clients).


(export '(occ occp check-occ-type
	  make-empty-occ is-empty-occ
	  nil-occ null-occ
	  occ-path occ-parent occ-top occ-rest
	  occ-push 
	  occ-push0 occ-push1 occ-push2 occ-push3 occ-push4
	  occ-push5 occ-push6 occ-push7 occ-push8 occ-push9
	  occ-equal
	  occ-to-list list-to-occ
	  occ-diff
	  ))



;;; The representation --

;;; Occurrences are represented as a single tree with the unique empty
;;; occurrence at the root.  Equal occurrences are consed once and then
;;; inserted into the initial occurrence tree.  Later calls which would
;;; construct equal occurrences merely reference the values already existing in
;;; the tree.  This tree will grow to accomodate arbitrarily deep occurrences.

;;; The rationale -- 

;;; Term traversal's will require building occurrences (Term routines like
;;; get-subterm and replace-subterm require them.).  We would rather not cons
;;; these structures everytime as they are identical.  Instead by paying a
;;; small amount of overhead for initial accesses, we should gain overall
;;; performance.  Further, this single occurrence tree allows us fast
;;; occurrence comparisons (eq) and gives us a place to store the reversed
;;; occurrence (the path from root to term).  The last point is important in
;;; that we really want two forms of the occurrence: 
;;;	1. You want to add a son to a parent occurrence (stored son to root).
;;;	2. You want to get a path from root to subterm (stored root to son).
;;; Achieving these two goals without frequently reversing and appending lists
;;; requires keeping both forward and reversed occurrences. 

;;; Occurrences are given a sexp printed representation.  Lists prefixed with
;;; #p (for path) are occurrences.  They are always printed in descending order
;;; (that is from root to subterm). 



;;; The representation

(defstruct (occ-struct (:predicate occp)
		       (:print-function print-occ))
  (index)				; current (lowest) son index
  (parent)				; parent occ
  (path)				; list of indexes from root to term
  (sons (make-array '(0)		; occ sons of this occ
		    :initial-element :absent
		    :adjustable t
		    :fill-pointer t)))


(deftype occ ()
  "The type of occurences."
  'occ-struct)



(defun check-occ-type (occ)
  "Make sure X is an occurence, otherwise give an error message."
  (check-type occ occ "an occurence"))





;;; Representation

;;; Empty Occurrence
  
(defparameter empty-occ (make-occ-struct :path nil
					 :index nil
					 :parent nil)
  "The unique empty occurrence.")


(defun make-empty-occ ()
  "Returns the empty occurrence."
  empty-occ)

(defun is-empty-occ (x)
  "Is X the empty occurrence."
  (eq x empty-occ))




(eval-when (compile eval load)

  (defmacro nil-occ ()
    "Returns the empty occurrence."
    `(make-empty-occ))
  
  (defmacro null-occ (x)
    "Is X the empty occurrence."
    `(is-empty-occ ,x)))



;;; Internal representation

 
(defun make-occ (son parent-occ)
  "Internal to OCC: Makes a son occurrence from the SON index and PARENT-OCC."
  (declare (type (integer 0 *) son))
  (let ((sons (occ-struct-sons parent-occ)))
    (do ()
	((> (fill-pointer sons) son))
      (vector-push-extend :absent sons))
    (cond ((not (eq (aref sons son) :absent))
	   (aref sons son))
	  (t
	   (let ((new-occ
		  (make-occ-struct :path (append (occ-path parent-occ)
						 (list son))
				   :index son
				   :parent parent-occ)))
	     (setf (aref sons son) new-occ))))))



;;; Primitive functions

(defun occ-path (occ)
  "A list of son indexes from root to subterm of OCC."
  (declare (type occ occ))
  (occ-struct-path occ))

(defun occ-parent (occ)
  "The parent of OCC."
  (declare (type occ occ))
  (occ-struct-parent occ))

(defun occ-top (occ)
  "The index of the son deepest in the tree."
  (declare (type occ occ))
  (occ-struct-index occ))

(defun occ-push (n parent-occ)
  "Make an occ for the Nth son of PARENT-OCC."
  (declare (type occ parent-occ))
  (make-occ n parent-occ))


(defun occ-equal (occ1 occ2)
  "See if two occurences are equal"
  (eq occ1 occ2))			; For reasons behind EQ see initial
					; doc. 


(eval-when (compile eval load)
  (defmacro occ-rest (occ)
    "Synonym for occ-parent."
    `(occ-parent ,occ))

  (defmacro occ-push0 (occ)
    "Make an occ for the 0th son of the parent OCC."
    `(occ-push ,occ 0))
  (defmacro occ-push1 (occ)
    "Make an occ for the 1st son of the parent OCC."
    `(occ-push ,occ 1))
  (defmacro occ-push2 (occ)
    "Make an occ for the 2nd son of the parent OCC."
    `(occ-push ,occ 2))
  (defmacro occ-push3 (occ)
    "Make an occ for the 3rd son of the parent OCC."
    `(occ-push ,occ 3))
  (defmacro occ-push4 (occ)
    "Make an occ for the 4th son of the parent OCC."
    `(occ-push ,occ 4))
  (defmacro occ-push5 (occ)
    "Make an occ for the 5th son of the parent OCC."
    `(occ-push ,occ 5))
  (defmacro occ-push6 (occ)
    "Make an occ for the 6th son of the parent OCC."
    `(occ-push ,occ 6))
  (defmacro occ-push7 (occ)
    "Make an occ for the 7th son of the parent OCC."
    `(occ-push ,occ 7))
  (defmacro occ-push8 (occ)
    "Make an occ for the 8th son of the parent OCC."
    `(occ-push ,occ 8))
  (defmacro occ-push9 (occ)
    "Make an occ for the 9th son of the parent OCC."
    `(occ-push ,occ 9))

) ;; eval-when



(defun occ-to-list (occ)
  "Convert OCC into a list of indices from root to subterm."
  (occ-path occ))


(defun list-to-occ (list)
  "Convert a LIST of indices from root to subterm into an occurence"
  (list-to-occ-aux (reverse list)))

(defun list-to-occ-aux (list)
  (if (null list)
      (make-empty-occ)
      (occ-push (first list) (list-to-occ-aux (rest list)))))

(defun occ-diff (above below)
  "if below points to a subterm of the term above points to,
      return the occurrence of the below term in the above term.
   otherwise return nil."
  (let ((alist (occ-to-list above))
	(blist (occ-to-list below)))
    (if (and (>= (length blist) (length alist))
	     (equal (subseq blist 0 (length alist)) alist))
	(list-to-occ (subseq blist (length alist)))
	nil)))

#|
(assert (occ-equal (occ-diff #p(1 2 3) #p(1 2 3 4 5)) #p(4 5)))
(assert (eq (occ-diff #p(1 2 3) #p(1 2)) nil))
(assert (eq (occ-diff #p(1 2 3) #p(1 2 5)) nil)))
|#

;;; Printing functions

(defun print-occ (occ stream depth)
  "Generates printed representation of OCC."  
  (declare (ignore depth))
  (format stream "#p~S" (occ-path occ)))

(defun read-list-to-occ (stream subchar arg)
  (declare (ignore subchar arg))
  (list-to-occ (read stream)))

(defun ergolisp::\#p ()
  (set-dispatch-macro-character #\# #\p #'read-list-to-occ))

(eval-when (load eval)
  (ergolisp::\#p))





