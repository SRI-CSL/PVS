;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)access-par.lisp	1.5 11/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.


;;; Access macros for the grammar abstract syntax (output of the
;;;    sb-parser).  These are used in parser construction.  They
;;;    duplicate functionality in sb-gr-access.lisp. (Kept for
;;;    modularity). 
;;; Anne Rogers 
;;; Revised Scott Dietzen, Mon Oct 13 15:32:09 1986


(in-package 'syntax-box)  (use-package :ergolisp)



(defun get-sons (pat)
  (if (pattern-p pat)
      (cond ((not (eq (pattern-kind pat) 'seq))
	     (pattern-sons pat))
	    ((get-pattern-expand-seq-rep pat))
            (t
             (set-pattern-expand-seq-rep pat
                                         (expand-seq (pattern-sons pat)))))))

;;; @@@ Document 'internal-seq.
;;; @@@ This is a HACK!!!

(defun expand-seq (son-list)
  (cond ((= (length son-list) 2)
	 son-list)
	((> (length son-list) 2)
	 (list (car son-list)
	       (make-pattern :kind 'internal-seq
			     :sons (expand-seq (cdr son-list)))))
	(t
	 (sb-system-error))))



(eval-when (compile eval load)

(defmacro atomic (pat)
  `(memq (get-kind ,pat)
	 '(nonterminal ukeyword just-as ext-nonterminal jux)))

(defmacro get-first-son (pat)
  `(let ((pat ,pat))
     (if (pattern-leaf-p pat)
	 (pattern-leaf-ds pat)
	 (car (get-sons pat)))))

(defmacro get-second-son (pat)
  `(let ((pat ,pat))
     (if (pattern-leaf-p pat)
	 (multiple-value-bind (first second)
	     (pattern-leaf-ds pat)
	   (declare (ignore first))
	   second)
	 (cadr (get-sons pat)))))

(defmacro get-first-pat-leaf-value (pat)
  `(pattern-leaf-ds ,pat))

(defmacro get-second-pat-leaf-value (pat)
  `(multiple-value-bind (first second)
       (pattern-leaf-ds ,pat)
     (declare (ignore first))
     second))


(defmacro get-subpat (pat)
  `(car (get-sons ,pat)))		; srd revised.

(defmacro get-separator (pat)
  `(cadr (get-sons ,pat)))		; srd revised. 
    
(defmacro get-as (pat)
  `(get-pattern-augment ,pat))

(defmacro get-result (pat)
  `(get-pattern-slot ,pat))

(defmacro get-kind (pat)
  `(pattern-kind ,pat))

(defmacro get-name (pat)
  `(pattern-name ,pat))

(defmacro get-initial (pat)
  `(get-pattern-initial ,pat))

(defmacro set-initial (pat value)
  `(set-pattern-initial ,pat ,value))

(defmacro get-medial (pat)
  `(get-pattern-medial ,pat))

(defmacro set-medial (pat value)
  `(set-pattern-medial ,pat ,value))



(defmacro get-as-first-son (as-pat)
  `(let ((as-pat ,as-pat))
     (if (augment-leaf-p as-pat)
	 (augment-leaf-ds as-pat)
	 (car (augment-args as-pat)))))
  
(defmacro get-aug-leaf-value (aug)
  `(augment-leaf-ds ,aug))

(defmacro get-as-second-son (as-pat)
  `(let ((as-pat ,as-pat))
     (if (augment-leaf-p as-pat)
	 (multiple-value-bind (first second)
	     (augment-leaf-ds as-pat)
	   (declare (ignore first))
	   second)
	 (cadr (augment-args as-pat)))))

(defmacro get-as-kind (as-pat)
  `(augment-kind ,as-pat))

(defmacro get-as-result (as-pat)
  `(get-augment-result-slot ,as-pat))

(defmacro get-as-key (as-pat)
  `(get-augment-key-slot ,as-pat))

(defmacro get-as-args (as-pat)
  `(augment-args ,as-pat))

(defmacro lt-p (pat gram-struct)
  `(let ((pat ,pat))
     (and (eq (get-kind pat) 'nonterminal)
	  (is-lexical-terminal (get-first-son pat) ,gram-struct))))

(defmacro get-nt-list (gram-struct)
  `(grammar-nonterminal-definitions ,gram-struct))


  )					; eval-when





