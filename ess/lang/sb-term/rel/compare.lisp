;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)compare.lisp	1.3 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Anne Rogers 
;;; Revised, Scott Dietzen, Mon Oct 13 16:05:43 1986


;;;    Basic Function: Look ahead set comparison functions

(in-package 'syntax-box)   (use-package :ergolisp)

(use-package '(sb-runtime))




; Compare-fs: Look ahead set x Look ahead set --> boolean
;    Compare-fs returns true if the two look ahead sets overlap.
; 

(defun compare-fs (fs1 fs2)
	 
  (do ((list1 fs1 (cdr list1)) (result) (temp))
      ((null list1) result)

        ; find this first symbol in the other list.
    (setq temp (assoc (caar list1) fs2))
        ; do their seconds overlap?
    (setq result (or result (intersection (cdar list1) (cdr temp))))))



; Make-seq-fs: Look ahead set x Look ahead set --> Look-ahead set
;   Given the front and back look ahead sets for a sequence build the
;   sequence look ahead set.

(defun make-seq-fs (front back)
  (let ((initials (make-initial-list back))
	(epsilon-flag (leading-epsilon-p front))
	 result)
    
       
    (do ((list1 (remove '(epsilon) front :test #'equal) (cdr list1))
	 (e-note) (new-second))
	((null list1))
	   
          ; If epsilon is a possible second symbol then add the 
          ; initials from back to the seconds (make
          ; sure that they don't overlap with existing seconds)

      (cond ((second-epsilon-p (car list1))
	     (Cond ((setq e-note
			  (intersection (remove 'epsilon (cdar list1)) initials))
		    (my-error 12 e-note)
		        ; Build an entry anyway.  It may point out errors later 
		        ; in the pattern
		    (setq new-second
			  (union (remove 'epsilon (cdar list1)) initials)))

		   (T (setq new-second
			    (append (remove 'epsilon (cdar list1)) initials))))
		     
	     (push (cons (caar list1) new-second) result))

	        ; implicit epsilon.
	    ((eq (length (car list1)) 1)
	     (push (append (car list1) initials) result))

	    (T (push (car list1) result))))
       

        ; If first can go to epsilon then we have to add the back fs-list
	; to the result -- they can be the starting ops for the whole seq.
    (cond (epsilon-flag
	   (do ((list2 back (cdr list2)) (temp) (new-entry))
	       ((null list2))
		     
	         ; If first symbols overlap make sure that the 
	         ; seconds don't.
	     (Cond ((setq temp (assoc (caar list2) result))
		    (setq result (remove temp result))

		    (if (setq new-entry (compare-build temp (car list2) 12))
			(push new-entry result)))
		   
		       ; Save if no overlap in first symbols
		   (T (push (car list2) result))))))
    result))






; make-initial-list: given an fs-list produce a list of first symbols.

(defun make-initial-list (fs)
  (mapcar #'(lambda (x) (car x)) fs))

    

; Compare-fs-list:  Look ahead set* --> Look ahead set.
;    Check to make sure we can choose one of the sets in the list
;    within two symbols.  And combine them to build a new look ahead set.

(defun compare-fs-list (lists)
  (let ((result (car lists))
	temp final-result new-fs)
	 
    (do ((fs-lists (cdr lists) (cdr fs-lists)))
	((null fs-lists))
	     
      (do ((fs-temp (car fs-lists) (cdr fs-temp)))
	  ((null fs-temp))
		 
	    ; If there is an overlap in first symbols then
	    ; check the seconds.  Complain if there is one.
	(cond ((setq temp (assoc (caar fs-temp) result))
	       (setq result (remove temp result))
	       (cond ((setq new-fs (compare-build (car fs-temp) temp 13))
		      (push new-fs result))

		     (T (setq final-result 'bad))))

	      (T (push (car fs-temp) result)))))

    (cond ((eq final-result 'bad) NIL)
	  (T result))))





; Compare-build:  Look ahead set x Look ahead set x Number -> Look ahead set entry
; Given two fs-list entries -- Combine them if their
; seconds do not overlap.  Note: two overlapping 'epsilons 

(defun compare-build (l1 l2 error-number)
  (let (e-note)
        ; overlapping epsilons are illegal.
    (cond ((or (and (null (cdr l1)) (null (cdr l2)))
	       (and (null (cdr l1)) (member 'epsilon (cdr l2)))
	       (and (null (cdr l2)) (member 'epsilon (cdr l1))))
	   (my-error error-number '(epsilon)))
	
	     ; l1 has no seconds...don't forget the epsilon.
	  ((null (cdr l1)) (append `(,(car l1) epsilon) (cdr l2)))

	     ; l2 has no seconds...don't forget the epsilon.
	  ((null (cdr l2)) (append `(,(car l2) epsilon) (cdr l1)))

	     ; check for overlap.  
	  ((setq e-note (intersection (cdr l1) (cdr l2)))
	   (my-error error-number (cons (car l1) e-note)))

	  (T (cons (car l1) (append (cdr l1) (cdr l2)))))))



(Defun compare-two-fs-lists (l1 l2)
  (compare-fs-list (list l1 l2)))




; Compare-medial-fs-list: Look ahead set* --> symbols*
;   Medials must be distinguishable within one symbol.  
;   The result is a list of the first synbols for the productions.

(defun compare-medial-fs-list (lists)
  (let (temp1 temp2 result)

    (do ((fs-lists lists (cdr fs-lists)))
	((null fs-lists))

      (setq temp1 (mapcar #'car (car fs-lists)))
      (setq result (or (intersection temp1 temp2) result))
      (setq temp2 (append temp1 temp2)))

    (cond (result (my-error 29 result)))

    temp2))

