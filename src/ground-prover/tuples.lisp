;;; -*- Mode: LISP; Package: VERSE; Syntax: Common-lisp -*-

; tuples.lsp         (Author: Allen Van Gelder, Sep 12, 1984)

; decision procedure for tuples.
; tuple selector functions look like (tupsel 0 t), (tupsel 1 t), ...
; tuple constructor function is (tupcons e0 e1 e2 ... en)


(defvar *tupleflg* nil     " enables tuples debug msgs if T ")

; ------------------------------------------------------------------------
; sigma for tuples
;
; tupsel(k, tupcons(x0, ... xk, ... )) => xk

(defvar *tupsel-list* '(tupsel- tupsel-integer tupsel-number tupsel-bool))

(defun is-tupsel-n (name)
  (memq name *tupsel-list*))

(defun is-tupsel (term)
  (and (consp term)
       (is-tupsel-n (funsym term))))

(defun make-tupsel (type)
  (makesym "TUPSEL-~a" type))

(defun sigtupsel (term)
  ; currently an error if term is an atom.
  (when *tupleflg* (TERPRI) (princ "sigtupsel: ") (pprint  term))
  (prog (ans tuple1)
    (setq ans
	  (cond
	    ((is-tupsel-n (funsym term))
	     (setq tuple1 (arg2 term))
	     (cond ( (and (consp tuple1)			
			  (eq (funsym tuple1) 'tupcons) )
		     (nth (arg1 term) (argsof tuple1)) )
		   (t term) ))
	    (t       term) ))
    (when *tupleflg* (TERPRI) (princ "       ==> ") (pprint ans))
    (return ans) ))


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
      (EQUAL
       (cond ((equal leftside rightside) '(TRUE))
	     ((and (tupconsp leftside)
		   (not (tupconsp rightside)) )
	      (tupsolve `(EQUAL ,rightside ,leftside)) )
	     ((and (tupconsp leftside)
		   (tupconsp rightside) )
	      (do ((lcols (argsof leftside) (cdr lcols))
		   (rcols (argsof rightside) (cdr rcols))
		   (results '(TRUE))
		   (result) )
		  ((null rcols) results)
		(let ((eqn `(EQUAL ,(car lcols) ,(car rcols))))
		  (setq result
			(newcontext (process eqn) ))
		  ; Following relies on the fact that process does
		  ; (RETFALSE) if its result is FALSE, and only
		  ; non-list value returned is 'TRUE.  See below too.
		  (cond ((eq result 'FALSE)(RETFALSE))
			((null result)
			 (setq results
			       (if (equal results '(TRUE))
				   (solve eqn)
				   (append (solve eqn) results))))
			((listp result)
			 (setq results (if (equal results '(TRUE))
					   result
					   (append result results)) ))))))
	     ((and (not (tupconsp leftside))
		   (tupconsp rightside) )
					; expand leftside x => tupcons(tupsel(0,x), tupsel(1,x),...)
					; in effect.
	      (cond ((or t (subtermof leftside rightside))
		     (do ((selnum 0 (1+ selnum))
			  (rcols (argsof rightside) (cdr rcols))
			  (results '(TRUE))
			  (result) )
			 ((null rcols) results)
		       (let ((eqn `(EQUAL (,(make-tupsel
					     (prtype (car rcols)))
					   ,selnum ,leftside)
					  ,(car rcols) )))
			 (setq result
			       (newcontext
				(process eqn)))
			 (cond ((eq result 'FALSE)(RETFALSE))
			       ((null result)
				(setq results
				      (if (equal results '(TRUE))
					  (ncons eqn)
					  (cons eqn results))))
			       ((listp result)
				(setq results
				      (if (equal results '(TRUE))
					  result
					  (append result results)) ))))))
		    (t (ncons atf)) ))
	     (t (ncons atf))))
      ;;NSH: tuples are only used internally for array/function/record access
      ;;so it is assumed that both args are tupconses.
      (LESSP (tupineqprocess (argsof leftside)(argsof rightside) 'LESSP))
      (LESSEQP (tupineqprocess (argsof leftside)(argsof rightside) 'LESSEQP))
      (GREATERP (tupineqprocess (argsof rightside)(argsof leftside) 'LESSP))
      (GREATEREQP (tupineqprocess (argsof rightside)(argsof leftside) 'LESSEQP))
      (t `((EQUAL ,atf TRUE))) )))




; ------------------------------------------------------------------------
; tupnsolve:  solver for negative predicates over tuples (NOT has been removed
; 24-10-89 using function common to both tuples and arrays

(defun tupnsolve (atf)
  (when *tupleflg* (TERPRI) (princ "tupnsolve: ") (pprint atf))
  (let ((ans (common-nsolve atf)))
    (when *tupleflg* (TERPRI) (princ "       ==> ") (pprint ans))
    ans ))


(defun common-nsolve (atf)
  (let ((leftside (lside atf))
	    (rightside (rside atf)) )
    (case (funsym atf)
      (EQUAL
       (if (equal leftside rightside)  ;NSH eq->equal
	   '(FALSE)
	   (let ((result (newcontext (process atf))))
	     (cond ((eq result 'FALSE) '(TRUE))
		   ((eq result 'TRUE)  '(FALSE))
		   (t `((NEQUAL ,leftside ,rightside))) ))))
      ((LESSP LESSEQP GREATERP GREATEREQP)
       (let ((result (newcontext (process atf))))
	 (cond ((eq result 'FALSE) '(TRUE))
	       ((eq result 'TRUE) '(FALSE))
	       (t (list (negineq atf))))))
      (t `((EQUAL ,atf FALSE)) ))))

;;_____________________________________________________________________________

(defun tupineqcheck (exp)        ;;checks if exp is a tuple inequality
  (and (eq (prtype (arg1 exp)) 'tuple)
       (eq (prtype (arg2 exp)) 'tuple)))

;called by sigupdate
(defun tupineqprocess0 (left right rel)  ;converts (TRUE) and (FALSE)
                                         ;to TRUE and FALSE, resp
  (let ((result0 (tupineqprocess left right rel))) 
    (cond ((equal result0 '(FALSE)) 'FALSE)
	  ((equal result0 '(TRUE)) 'TRUE)
	  (t result0))))

(defun tupineqprocess (left right rel) ;;generates process for 
  (if (and (consp left)(consp right))  ;;lex ineq. <,<=, on tups.
      (let ((lex1 (newcontext
		   (prove1 (list `(LESSP ,(car left) ,(car right))) NIL))))
	(case  lex1
	  (TRUE '(TRUE))
	  (FALSE
	   (let ((lexeq1
		    (newcontext
		     (process `(EQUAL ,(car left) ,(car right))))))
	       (case lexeq1
		 (TRUE
		  (tupineqprocess (cdr left)(cdr right) rel))
		 (FALSE (RETFALSE))
		 (t `((EQUAL (,rel ,(cons 'TUPCONS left)
				   ,(cons 'TUPCONS right))
			     TRUE))))))
	  (t `((EQUAL (,rel ,(cons 'TUPCONS left)
				   ,(cons 'TUPCONS right))
			     TRUE)))))
      (case rel          ;;rel is either LESSP or LESSEQP
	(LESSP '(FALSE))
	(t '(TRUE)))))     ;;left and right assumed same length


