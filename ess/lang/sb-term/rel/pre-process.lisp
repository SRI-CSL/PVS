;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)pre-process.lisp	1.4 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Sat Oct 11 15:53:19 1986

;;; Access macros for the grammar abstract syntax (output of the
;;;    sb-parser). 

(in-package "SYNTAX-BOX")  (use-package :ergolisp)


(defvar *pre-process-error* ())




(defun pre-process (grammar lang)
  (let ((nt-name-list (mapcar #'nt-name
                              (grammar-nonterminal-definitions grammar))))
    (setq *pre-process-error* nil)
    (check-lexical-nts-and-nts grammar nt-name-list)
    (check-duplicate-nts nt-name-list)
    (check-brackets-and-nts grammar nt-name-list)
    (check-prec-and-nts grammar nt-name-list)
    (check-name grammar lang)
    (check-external-grammars grammar lang)
    (values grammar *pre-process-error*)))

 

(defun check-external-grammars (grammar lang)
  (declare (ignore grammar lang))
  ;;; @@@ Write
  ())

(defun check-name (grammar lang)
  (declare (ignore grammar lang))
  ;;; @@@ Write
  ())


(defun check-lexical-nts-and-nts (grammar nt-name-list)
  (let ((lexical-nts (grammar-lexical-terminals grammar)))
    (do ((nt-name-runner nt-name-list (cdr nt-name-runner)))
        ((null nt-name-runner))
      (if (memq (car nt-name-runner)
                lexical-nts)
          (pre-process-error
	   (format  nil
	       "Overlap between nonterminals and lexical terminal ~A~%"
	     (car nt-name-runner))
	   t)))))



(defun check-duplicate-nts (nt-name-list)
  "Checks for multiple nonterminal defintions for a single name."
  (do ((nt-name-runner nt-name-list (cdr nt-name-runner)))
      ((null nt-name-runner))
    (if (memq (car nt-name-runner)
              (cdr nt-name-runner))
	(pre-process-error
	 (format  nil
	     "Duplicate nonterminal ~A~%."
	   (car nt-name-runner))
	 t))))



(defun check-brackets-and-nts (grammar nt-name-list)
  (let ((bracket-nt-name-list
         (mapcar #'(lambda (entry)
                     (ds-id (term-arg0 (ck-term-sop 'bracket-entry entry))))
		 (grammar-bracket-information grammar))))
    (do ((bracket-nt-runner bracket-nt-name-list (cdr bracket-nt-runner)))
        ((null bracket-nt-runner))
      (if (not (memq (car bracket-nt-runner)
                     nt-name-list))
	(pre-process-error
	 (format  nil
	     "The nonterminal ~A is undefined, but it was mentioned ~%~
	      in the bracket list.~%"
	   (car bracket-nt-runner))
	 t))
      (if (memq (car bracket-nt-runner)
                (cdr bracket-nt-runner))
	(pre-process-error
	 (format  nil
	     "Multiple bracket entry for nonterminal ~A.~%" 
	   (car bracket-nt-runner))
	 t)))))


(defun check-prec-and-nts (grammar nt-name-list)
  (let ((prec-nt-name-list
         (mapcar #'(lambda (entry)
                     (ds-id (term-arg0 (ck-term-sop 'prec-entry entry))))
		 (grammar-precedence-information grammar))))
    (do ((prec-nt-runner prec-nt-name-list (cdr prec-nt-runner)))
        ((null prec-nt-runner))
      (if (not (memq (car prec-nt-runner)
                     nt-name-list))
	  (pre-process-error
	   (format  nil
	       "The nonterminal ~A is undefined, but it was mentioned ~%~
	        in the precedence list.~%" 
	     (car prec-nt-runner))
	   t))
      (if (memq (car prec-nt-runner)
                (cdr prec-nt-runner))
	  (pre-process-error
	   (format  nil
	       "Multiple precedence entry for nonterminal ~A.~%"
	     (car prec-nt-runner))
	   t)))))




;;; This checking not currently done because it is expensive and probably not
;;; worht a whole lot. 

;;;  @@@ How much of this checking is worth revising and calling? 

;;; Juxtaposition checking. 

(defun check-pattern (pat)
  (check-pat-mult-tags-augments pat nil nil)
  ;; Make sure no multiple tags, augments, formatting info in pattern
  (case (pattern-kind pat)
    (jux
     (let ((nt1 (car (pattern-sons pat)))
           (nt2 (cadr (pattern-sons pat))))
       (cond ((or (not (memq (pattern-kind nt1) '(nonterminal ext-nonterminal)))
		  (not (memq (pattern-kind nt2) '(nonterminal ext-nonterminal))))
              (pre-process-error 
	       (format nil
		   "The patterns adjacent to a jux must be either nonterminals~%~
                    or external nonterminals, but this one was ~A."
		 (if (not (memq (pattern-kind nt1)
				'(nonterminal ext-nonterminal)))
		     (pattern-kind nt1)
		     (pattern-kind nt2)))
	       t)))))
    (t
     ())))


(defun check-augment (aug)
  (check-aug-mult-tags aug nil)
  ;; Make sure no multiple tags in augments.
  (case (augment-kind aug)
    (opt
     (if (not (= 2 (length (augment-sons aug))))
         ;; Augment optionals should have exactly two sons. 
         (pre-process-error 
	  (format nil
	      "Error in abstract syntax constructors: ~%~
	       Optional augment does not have two branches. ~%~S~%"
	    aug)
	  t)))
    (t
     ())))


(defun check-pat-mult-tags-augments (pat no-tag no-aug)
  (cond ((and (is-sop 'tag pat)
              no-tag)
	 (pre-process-error 
	  (format nil "A single pattern has received multiple tags.~%~S~%"
	    pat)
	  t))
        ((is-sop 'tag pat)
         (check-pat-mult-tags-augments
          (term-arg0 pat) t no-aug))
        ((and (is-sop 'augment pat)
              no-aug)
	 (pre-process-error 
	  (format nil "A single pattern has received multiple augments.~%~S~%"
	    pat)
	  t))
        ((is-sop 'aug pat)
         (check-pat-mult-tags-augments
          (term-arg0 pat) no-tag t))))


(defun check-aug-mult-tags (aug no-tag)
  (cond ((and (is-sop 'tag-aug aug)
              no-tag)
	 (pre-process-error 
	  (format nil "A single augment has received multiple tags.~%~S~%"
	    aug)
	  t)) 
        ((is-sop 'tag-aug aug)
         (check-aug-mult-tags (term-arg0 aug) t))))




(defun pre-process-error (string error?)
  (terpri)
  (if error?
      (format t "Pre-Process phase Error: ~%")
      (format t "Pre-Process phase Warning: ~%"))
  (format t string)
  (if (not *pre-process-error*)
      (setq *pre-process-error* error?)))
