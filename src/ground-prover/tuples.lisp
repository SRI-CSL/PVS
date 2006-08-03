;;; -*- Mode: LISP; Package: VERSE; Syntax: Common-lisp -*-

; tuples.lsp         (Author: Allen Van Gelder, Sep 12, 1984)

; decision procedure for tuples.
; tuple selector functions look like (tupsel0 t), (tupsel 1 t), ...
; tuple constructor function is (tupcons e0 e1 e2 ... en)

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


(defvar *tupleflg* nil     " enables tuples debug msgs if T ")

; ------------------------------------------------------------------------
; sigma for tuples
;
; tupsel(k, tupcons(x0, ... xk, ... )) => xk

(defvar *tupsel-list* '(TUPSEL- TUPSEL-integer TUPSEL-number TUPSEL-bool))

(defun is-tupsel-n (name)
  (memq name *tupsel-list*))

(defun is-tupsel (term)
  (and (consp term)
       (is-tupsel-n (funsym term))))

(defun make-tupsel (type)
  (intern (if type (format nil "TUPSEL-~a" type) (format nil "TUPSEL-"))))

(defun sigtupsel (term)
  ; currently an error if term is an atom.
  (when *tupleflg* (terpri) (princ "sigtupsel: ") (pprint  term))
 ; (prog (ans tuple1)
;    (setq ans
	  (cond
	    ((is-tupsel-n (funsym term))
	     (sigtupselterm (arg1 term)(arg2 term) term (funsym term)))
	    (t       term) ))
;    (when *tupleflg* (terpri) (princ "       ==> ") (pprint ans))
;    (return ans) ))

(defun sigtupselterm (n term tupselterm tupselop)
  (cond ((not (consp term))
	 (maketupselterm n term tupselterm tupselop))
	((eq (funsym term) 'tupcons)
	 (nth n (argsof term)))
	((eq (funsym term) 'update)
	 (if (eql n (arg2 term))
	     (arg3 term)
	     (sigtupselterm n (arg1 term) nil tupselop)))
	(t (maketupselterm n term tupselterm tupselop))))

(defun maketupselterm (n term tupselterm tupselop)
  (or tupselterm
      `(,tupselop ,n ,term)))


; ------------------------------------------------------------------------
; tupsolve:  solver for positive predicates over tuples
;
; Argument is a canonized equation (atf) whose sides are type tuple.
; Returns a list of atfs.

(defmacro tupconsp (term)
  `(and (consp ,term) (eq (funsym ,term) 'tupcons)) )			

(defun tupsolve (atf)
  (let ((fun (funsym atf))
	(leftside (lside atf))
	(rightside (rside atf)))
    (case fun
      (equal
       (cond ((equal leftside rightside) '(true))
	     ((and (tupconsp leftside)
		   (not (tupconsp rightside)) )
	      (tupsolve `(equal ,rightside ,leftside)) )
	     ((and (tupconsp leftside)
		   (tupconsp rightside) )
	      (do ((lcols (argsof leftside) (cdr lcols))
		   (rcols (argsof rightside) (cdr rcols))
		   (results '(true))
		   (result) )
		  ((null rcols) results)
		(let ((eqn `(equal ,(car lcols) ,(car rcols))))
		  (setq result
			(newcontext (process-no-canon eqn) ))
		  ; Following relies on the fact that process does
		  ; (retfalse) if its result is false, and only
		  ; non-list value returned is 'true.  See below too.
		  (cond ((eq result 'false)(retfalse))
			((null result)
			 (setq results
			       (if (equal results '(true))
				   (solve eqn)
				   (append (solve eqn) results))))
			((listp result)
			 (setq results (if (equal results '(true))
					   result
					   (append result results)) ))))))
	     ((and (not (tupconsp leftside))
		   (tupconsp rightside) )
					; expand leftside x => tupcons(tupsel(0,x), tupsel(1,x),...)
					; in effect.
	      (cond ((or t (subtermof leftside rightside))
		     (do ((selnum 0 (1+ selnum))
			  (rcols (argsof rightside) (cdr rcols))
			  (results '(true))
			  (result) )
			 ((null rcols) results)
		       ;;; DAC 6/12/98: Reversed the orientation of
		       ;;; the generated equation. See bug 182.
		       (let* ((newrhs (car rcols))
			      (newlhs (canonsig ;;NSH(10-13-02)
				       `(,(make-tupsel
					  (prtype (car rcols)))
					,selnum ,leftside)
				       'dont-add-use))
			      (eqn (if (or (qnumberp newrhs)
					   (subtermof newrhs newlhs))
				       `(equal ,newlhs
					       ,newrhs)
				       `(equal ,newrhs
					       ,newlhs))))
			 (setq result
			       (newcontext
				(process-no-canon eqn)))
			 (cond ((eq result 'false)(retfalse))
			       ((null result)
				(setq results
				      (if (equal results '(true))
					  (ncons eqn)
					  (cons eqn results))))
			       ((listp result)
				(setq results
				      (if (equal results '(true))
					  result
					  (append result results)) ))))))
		    (t (ncons atf)) ))
	     (t (ncons atf))))
      ;;NSH: tuples are only used internally for array/function/record access
      ;;so it is assumed that both args are tupconses.
      (lessp (tupineqprocess (argsof leftside)(argsof rightside) 'lessp))
      (lesseqp (tupineqprocess (argsof leftside)(argsof rightside) 'lesseqp))
      (greaterp (tupineqprocess (argsof rightside)(argsof leftside) 'lessp))
      (greatereqp (tupineqprocess (argsof rightside)(argsof leftside) 'lesseqp))
      (t `((equal ,atf true))) )))




; ------------------------------------------------------------------------
; tupnsolve:  solver for negative predicates over tuples (NOT has been removed
; 24-10-89 using function common to both tuples and arrays

(defun tupnsolve (atf)
  (when *tupleflg* (terpri) (princ "tupnsolve: ") (pprint atf))
  (let ((ans (common-nsolve atf)))
    (when *tupleflg* (terpri) (princ "       ==> ") (pprint ans))
    ans ))


(defun common-nsolve (atf)
  (let ((leftside (lside atf))
	    (rightside (rside atf)) )
    (case (funsym atf)
      (equal
       (if (equal leftside rightside)  ;NSH eq->equal
	   '(false)
	   (let ((result (newcontext (process-no-canon atf))))
	     (cond ((eq result 'false) '(true))
		   ((eq result 'true)  '(false))
		   (t `((nequal ,leftside ,rightside))) ))))
      ((lessp lesseqp greaterp greatereqp)
       (let ((result (newcontext (process-no-canon atf))))
	 (cond ((eq result 'false) '(true))
	       ((eq result 'true) '(false))
	       (t (list (negineq atf))))))
      (t `((equal ,atf false)) ))))

;;_____________________________________________________________________________

(defun tupineqcheck (exp)        ;;checks if exp is a tuple inequality
  (and (eq (prtype (arg1 exp)) 'tuple)
       (eq (prtype (arg2 exp)) 'tuple)))

;called by sigupdate
(defun tupineqprocess0 (left right rel)  ;converts (true) and (false)
                                         ;to true and false, resp
  (let ((result0 (tupineqprocess left right rel))) 
    (cond ((equal result0 '(false)) 'false)
	  ((equal result0 '(true)) 'true)
	  (t result0))))

(defun tupineqprocess (left right rel) ;;generates process for 
  (if (and (consp left)(consp right))  ;;lex ineq. <,<=, on tups.
      (let ((lex1 (newcontext
		   (prove1 (list `(lessp ,(car left) ,(car right))) nil))))
	(case  lex1
	  (true '(true))
	  (false
	   (let ((lexeq1
		    (newcontext
		     (process-no-canon `(equal ,(car left) ,(car right))))))
	       (case lexeq1
		 (true
		  (tupineqprocess (cdr left)(cdr right) rel))
		 (false (retfalse))
		 (t `((equal (,rel ,(cons 'tupcons left)
				   ,(cons 'tupcons right))
			     true))))))
	  (t `((equal (,rel ,(cons 'tupcons left)
				   ,(cons 'tupcons right))
			     true)))))
      (case rel          ;;rel is either lessp or lesseqp
	(lessp '(false))
	(t '(true)))))     ;;left and right assumed same length


