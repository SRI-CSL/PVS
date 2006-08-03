;;; -*- Mode: LISP; Package: VERSE; Syntax: Common-lisp -*-

; arrays.lsp         (Author: Allen Van Gelder, Aug 14, 1984)
;                    (Modified: N. Shankar, Nov, 20, 1989)

; decision procedure for arrays.
; array selector function looks like (APPLY-1-type s i)
; array store (assignment) function is (update s i v)

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------


(defvar *arrayflg* nil  "Print debugging msgs if t")

; ------------------------------------------------------------------------
; sigma for arrays
;
; arraysel(arraystore(s, i, val), i) => val
; arraysel(arraystore(s, i, val), j) => arraysel(s, j),  where j ne i.
;
; Returns an if* form if it cant tell whether i=j at present.
;

(defun is-array-apply? (name)
  (let* ((chars (symbol-to-charlist name))
	 (first6 (loop for i from 1 to 6
		       for c in chars
		       collect c))
	 (arity (car (nthcdr 6 chars))))
;					    (format t "~%first6= ~a" first6)
;					    (format t "~%arity= ~a" arity)
    (and (equal first6 '(A P P L Y -))
	 (equal arity '|1|))))

;forms (apply-n-type a args)
(defun make-ary-apply (ret-type array args)
  (cons (applysym1
	 ret-type
	 (length args))
	(cons array args)))
;arraysel replaced by unary function application (NSH)
;arrayrest removed and arraystore changed to update(NSH)

(defmacro argrest (n args) `(nthcdr ,n ,args))

(defun sigapply (term)
  ; term is assumed to be of the form (apply-N-type fun args)
  (when *arrayflg* (terpri) (princ "sigapply: ") (pprint term))
;  (prog (ans array0 array1 i j val ijeq))
 (let ((array0 (arg1 term))
       (jlst (nthcdr 2 term)))
   (cond ( (consp array0)					
	   (case (funsym array0)					
	     (update   (let ((array1 (arg1 array0))
			     (i    (arg2 array0))
			     (val  (arg3 array0) ))
			 (let ((ijeq
				(if (consp (cdr jlst)) ;multiary
				    (newcontext
				     (process-no-canon
				      `(equal ,i
					      ,(cons 'tupcons jlst))))
				    (newcontext
				     (process-no-canon
				      `(equal ,i ,(car jlst)))))))
			   (cond ((eq ijeq 'true)  val)
				 ((eq ijeq 'false)
				  (sigapply (cons (funsym term)
						 (cons array1 jlst)) ))
				 (t
				  (setq needed-if* t)
				  `(if* (equal ,i ,(if (consp (cdr jlst))
						       (cons 'tupcons jlst)
						       (car jlst)))
					,val
					,(cons (funsym term)
					       (cons array1 jlst) )))))))
	     (t            term) ))
	 (t term) )))
	

(defun sigupdate (term)
;term is assumed to be a list whose funsym is 'update
  (when *arrayflg* (terpri) (princ "sigupdate: ") (pprint term))
  (let
      ((array0 (arg1 term))
       (j (arg2 term)))
    (cond ((not (consp array0))				       
	   (when *arrayflg* (terpri) (princ "       ==> ") (pprint term))
	   term)
	  (t
	   (case (funsym array0)
	     (update (let* ((array1 (arg1 array0))
			    (i    (arg2 array0))
			    (val  (arg3 array0) )
			    (ieqj (newcontext
					    (process-no-canon
					     `(equal ,i ,j)))))
		       (cond ((eq ieqj 'true)
			      (list 'update
				    array1
				    j
				    (arg3 term)))
			     ((eq ieqj 'false)
			      ; DAC: 3-18-91
			      ; when i > j have to call sigupdate recursively
			      ; so that if there is an index j' in array1
			      ; s.t. j = j', that value at j` will be removed.
			      (if (expr< i j)
				  `(update (update ,array1 ,i ,val)
					    ,j ,(arg3 term))
				`(update ,(sigupdate `(update ,array1 ,j ,(arg3 term)))
					 ,i ,val)))
			     ((expr< i j)
			      (setq needed-if* t)
			      `(if* (equal ,i ,j)
				    (update ,array1 ,j ,(arg3 term))
				    (update (update ,array1 ,i ,val)
					    ,j ,(arg3 term))))
			     (t (setq needed-if* t)
				`(if* (equal ,j ,i)
				      (update ,array1 ,j ,(arg3 term))
				      (update (update ,array1 ,j ,(arg3 term))
					    ,i ,val))))))


	     (t term))))))

;NSH(4-90): removed from else case in sigupdate above
;(let ((redund (newcontext;;check for redundant
;			       (process;; update (NSH)
;				`(EQUAL
;				  ,(make-ary-apply
;				    (type (arg3 term))
;				    array0
;				    (if (eq (type j) 'TUPLE)
;					(cdr j)
;				      (list j)))
;				  ,(arg3 term) )))))
;		  (cond ((eq redund 'TRUE)
;			 array0)
;			(t term)))



; ------------------------------------------------------------------------
; arraysolve:  solver for positive predicates over arrays
;
; Argument is a canonized equation (atf) whose sides are type array.
; Returns a list of atfs.

;NSH: arraystorep changed to updatep
(defmacro updatep (term)
  `(and (consp ,term) (eq (funsym ,term) 'update)) )

; DAC 8/13/92: in arraysolve in the case of non-update leftside and update rightside,
; needed to add check for bottoming out of recursion when the new-rightside is no longer
; an update. This is so that arraysolve does not return FALSE if the leftside and new-rightside
; happen to have been asserted non-equal earlier. If they were asserted non-equal then
; this returning false would cause the original arraysolve to erroneously return false
; which would cause an unsoundness! (see ~cyrluk/bugs/update_unsound.spec for an example.)

(defun process-result2 (leftside new-rightside)
  (let ((result (newcontext
		 (process-no-canon
		  `(equal ,leftside ,new-rightside)))))
    (if (not (updatep new-rightside))
	(if (equal result 'true) 'true nil)
	result)))

(defun arraysolve (atf) 
  (when *arrayflg* (terpri) (princ "arraysolve: ") (pprint atf))
  (let
    ((ans
      (case (funsym atf)						
        (equal
	  (let ((leftside (lside atf))
		(rightside (rside atf)) )
	    (cond
	       ((equal leftside rightside) '(true))
	       ((and (updatep leftside) (not (updatep rightside)))
		(arraysolve `(equal ,rightside ,leftside)) )
	       ((and (updatep leftside) (updatep rightside))
		  ; "do" goes thru 3 args of update in reverse order,
		  ; because last is most likely to achieve a shortcut
		  ; by causing (retfalse).
		(let* ((largs (argsof leftside))
		       (rargs (argsof rightside))
		       (resarray (newcontext
				 (process-no-canon
				  `(equal ,(car largs) ,(car rargs)))))
		       (resindex (newcontext
				  (process-no-canon
				   `(equal ,(arg2 leftside)
					   ,(arg2 rightside)))))
		       (resval nil))
		  (cond ((and (eq resarray 'true)(eq resindex 'true))
			 (setq resval
			       (newcontext
				(process-no-canon
				 `(equal ,(arg3 leftside)
					 ,(arg3 rightside)))))
			 (cond ((null resval) (list `(equal ,(arg3 leftside)
							    ,(arg3 rightside))))
			       ((memq resval '(true false))
				(ncons resval))
			       (t resval)))
			((eq resarray 'true)
			 (let* ((leftarg2
				 (if (eq (prtype (arg2 leftside)) 'tuple)
					  (cdr (arg2 leftside))
					  (list (arg2 leftside))))
				(rightarg2
				 (if (eq (prtype (arg2 rightside)) 'tuple)
					  (cdr (arg2 rightside))
					  (list (arg2 rightside))))
				(res1
				(newcontext
				 (process-no-canon
				  `(equal 
				    ,(make-ary-apply
				      (prtype (arg3 rightside))
				      (arg1 leftside)
				      leftarg2)
				    ,(arg3 rightside)))))
			       (res2
				(newcontext
				 (process-no-canon
				  `(equal 
				    ,(make-ary-apply
				      (prtype (arg3 leftside))
				      (arg1 rightside)
				      rightarg2)
				    ,(arg3 leftside))))))
			   (cond ((or (eq res1 'false)(eq res2 'false))
				  (list `(equal ,(arg2 leftside)
						,(arg2 rightside))
					`(equal ,(arg3 leftside)
						,(arg3 rightside))))
				 ((and (null res1)(null res2))
				  (ncons atf))
				 ((eq res1 'true)
				  (if (eq res2 'true) (ncons 'true)
				    res2))
				 ((eq res2 'true) res1)
				 (t (append res1 res2)))))
			(t (ncons atf)))))
	       ((and (not (updatep leftside))
		     (updatep rightside) )
		  ; If leftside occurs in rightside,
		  ; expand leftside x => update(x, i, x(i))
		  ; choosing i to match right side, in effect;
		  ; otherwise just return the eqn in a singleton list.		
		(cond ((or t (subtermof leftside rightside))
		       (let (
			     (result1   ;
			      (newcontext
			      (process-no-canon
			       `(equal ,(make-ary-apply
					  (prtype (arg3 rightside))
					  leftside
					  (if (eq (prtype (arg2 rightside)) 'tuple)
					      (cdr (arg2 rightside))
					      (list (arg2 rightside))))
					 ,(arg3 rightside) ))))
			     (result2 (process-result2 leftside (arg1 rightside))))
			     ; DAC see comment above for process-result2.
			 (cond ((eq result1 'false)
				'(false))
			       ((eq result2 'false)
				; DAC (12/6/94): Fixed unsoundness bug:
				; B(i) = v & A = B WITH [i := u] & u \= v
				; & A(i) \= v was found inconsistent.
				;
				; The fix makes use of the following fact:
				;   B(i) = A(i) & NOT FORALL j: A(j) = B(j)
				; IMPLIES NOT FORALL (j \= i): A(j) = B(j)
				; Which implies NOT A = B WITH [i := v]
				;
				; The unsoundness bug was the result of leaving
				; out the B(i) = A(i) clause from above.
				; This corresponds to the check:
				; (eq test-equal-at-right-index 'true) below:
				(let ((test-equal-at-right-index
				       (newcontext
					(process-no-canon
					 `(equal
					   ,(make-ary-apply
					     (prtype (arg3 rightside))
					     leftside
					     (if (eq (prtype (arg2 rightside))
						     'tuple)
						 (cdr (arg2 rightside))
						 (list (arg2 rightside))))
					   ,(make-ary-apply
					     (prtype (arg3 rightside))
					     (arg1 rightside)
					     (if (eq (prtype (arg2 rightside))
						     'tuple)
						 (cdr (arg2 rightside))
						 (list (arg2 rightside)))))))))
				  (if (eq test-equal-at-right-index 'true)
				      '(false)
				      (ncons atf))))
			       ((eq result1 'true)
				(if (eq result2 'true)
				    '(true)
				    (or result2 (ncons atf))))
			       ((eq result2 'true)
				(or result1
				    (ncons
				     `(equal ,(make-ary-apply
					  (prtype (arg3 rightside))
					  leftside
					  (if (eq (prtype (arg2 rightside)) 'tuple)
					      (cdr (arg2 rightside))
					      (list (arg2 rightside))))
					 ,(arg3 rightside) ))))
			       ((and (null result1)(null result2))
				(ncons atf))
			       (t (append result2 result1)))))
		      (t (ncons atf)) ))
	       (t (ncons atf)))))
	(t `((equal ,atf true))) ) ))
    (when *arrayflg* (terpri) (princ "      ==> ") (pprint ans))
    ans ))

; ------------------------------------------------------------------------
; arraynsolve:  solver for negative predicates over arrays
; (NOT has been removed)
; 24-10-89 using function common to both tuples and arrays (see tuples.lisp)

(defun arraynsolve (atf)
  (when *arrayflg* (terpri) (princ "arraynsolve: ") (pprint atf))
  (let ((ans (common-nsolve atf)))
    (when *arrayflg* (terpri) (princ "       ==> ") (pprint ans))
    ans ))

;------------------------------------------------------------------
;NSH(4/90): Expression ordering for normalizing UPDATEs.
; 
(defun expr< (exp1 exp2)
  (if (consp exp1)
      (if (consp exp2)
	  (or (expr< (car exp1)(car exp2))
	      (expr< (cdr exp1)(cdr exp2)))
	nil)
    (if (consp exp2)
	t
      (atom< exp1 exp2))))

(defun atom< (exp1 exp2)
  (if (numberp exp1)
      (if (numberp exp2)
	  (< exp1 exp2)
	t)
    (if (numberp exp2)
	nil
      (string< exp1 exp2))))
