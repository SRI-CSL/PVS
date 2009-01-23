;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)unp-code-revise.lisp	1.7 9/26/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; Ergo Syntax Box

;;; This code modifies unparser generator code.
;;; Scott Dietzen, Wed Nov 11 15:38:39 1987

(in-package :SB)   (use-package :ergolisp)


(defun unp-code-revision (routines)
  (do ((runner routines (cdr runner))
       (new-routines nil
		     (nconc new-routines
			    (unp-break-alt (car runner)))))
      ((null runner)
       new-routines)))




(defvar *break-top-alts* t)

(defun unp-break-alt (routine)
  (let* ((routine-name (cadr routine))
	 (body (cdddr routine))		; (let ...)
	 (let-body (cddar body))
	 (locals (cddadr (car body)))	; (v2 v3 ...)
	 (main-body (caddr let-body))	; (nt-unp ...)
	 (alt-cand (cdr (cadddr main-body)))
					; ((dis-alt ...) (unp-alt-aug ...)
					;  (unp-alt ...))
	 (unp-alt-code (cadr alt-cand))
	 (is-top-alt? (and (listp (car alt-cand))
			   (eq (caar alt-cand) 'dis-alt)
			   (eq (cadar alt-cand) 'v0)
			   (eq (car unp-alt-code) 'unp-alt-aug)
			   (do ((clauses (caddr unp-alt-code) (cdr clauses))
				(result t (and result
					       (equal (car clauses)
						      `((unp-name 'nil v1))))))
			       ((or (null clauses)
				    (null result))
				result)))))
    (if (and *break-top-alts*
	     is-top-alt?)
	(let* ((unp-alt-branch (caddr alt-cand)))
	  (do ((i 1 (1+ i))
	       (alt-branches (caddr unp-alt-branch)
			     (cdr alt-branches))
	       (branches nil
			 (cons `((,(make-routine-name routine-name i)
				  v0 v1))
			       branches))			 
	       (routines nil
			 (cons `(defun ,(make-routine-name routine-name i)
				  (v0 v1)
				  ;; suppresses compiler warnings if available
				  (ergo-ignore-if-unused v0 v1)
			          (let ,locals
				    ;; suppresses compiler warnings if available
				    (ergo-ignore-if-unused ,@locals)
				    ,@(car alt-branches)))
			       routines)))
	      ((null alt-branches)
	       (let ((branches
		      (nreverse branches))
		     (routines (nreverse routines)))
		 (setf (cadr alt-cand)
		       `(unp-name 'nil v1))
		 (setf (caddr unp-alt-branch)
		       branches)	; side effect routine.
		 (cons routine routines)))))
	(list routine))))


	  
(defun make-routine-name (routine-name i)
  (sb::sb-intern-ncase
   (string-upcase (concatenate 'string
		    (symbol-name routine-name)
		    "-ALT-"
		    (princ-to-string i)))))
	  

  
