(in-package 'pvs)
;;; Patch file for the ground prover -- David Cyrluk

(defun adduse(u v)
  (unless (and (consp u)
	       (or (equal (car u) 'equal)
		   (equal (car u) 'if)
		   (equal (car u) 'if*)))
    (prog(ul)
       (or (member u (setq ul (use v)) :test #'equal)
	   (push (cons v (cons u ul)) usealist)
	   )
	;  (checkusealist "adduse")
       )
    )
)

(defun canon-find (w)
  (let ((pr-w (pr-find w)))
    (cond
      ((symbolp pr-w) pr-w)
      ((integerp pr-w) pr-w)
      ((uninterp pr-w) pr-w)
      (t (sigma (cons (funsym pr-w)
		      (mapcar #'canon-find (argsof pr-w))))))))

;(defun arithord1(u v)
;    (cond
;     ((null u) nil)
;     ((null v) t)
;     ((symbolp u)
;      (cond
;       ((symbolp v)
;	(cond
;	 ((eq u 'times) t)
;	 ((eq v 'times) nil)
;	 (t (alphalessp u v))))
;       ((qnumberp v) nil)
;       ((eq u 'times) t)
;       ((and (consp v) (eq (car v) 'times)) nil)
;       (t t)))
;     ((symbolp v) (or (qnumberp u)
;		      (and (consp u) (eq (car u) 'times))))
;     ((qnumberp u)
;      (cond
;       ((qnumberp v)(qlessp u v))
;       (t t)))
;     ((qnumberp v) nil)
;     ((equal (car u)(car v))(arithord1 (cdr u)(cdr v)))
;     (t (arithord1 (car u)(car v)))))

;;; 7/25/94: DAC added *split-on-if* flag so that in pvs array solver
;;; would do less work

(defvar *split-on-if-in-solve* nil)

(defvar *lift-if-in-solve* nil)

(defun solve (atf) ;(break)
  (let ((atf (if needed-if* (liftif* atf) atf)))
    (cond
     ((atom atf)
      (ncons (case atf
	       (true  'true)
	       (false 'false)
	       (t     `(equal ,atf true))
	       )))
     ((and *lambda-reduced* (needs-bool-lifting atf))	; added for bool lifting (sj).
      (cond
       (needed-if*
	(ncons (lift-bools (liftif atf))))
       (t (ncons (lift-bools atf))))) 
     ((memq (funsym atf) '(if if*))
      (if *split-on-if-in-solve*
	  (let ((leftsolve (catch 'context (solve (arg2 atf))))
		(rightsolve (catch 'context (solve (arg3 atf)))))
	    (if (equal leftsolve rightsolve)
		(if (member leftsolve '(false true))
					; Sept 14, 1990: DAC
					; solve should always return a list
					; but catching 'context might not.
		    (ncons leftsolve)
		  leftsolve)
	      (ncons atf)))
	(if (equal (arg2 atf) (arg3 atf))
	    (solve (arg2 atf))
	  (ncons atf))))
     ((memq (funsym atf) '(lessp lesseqp greaterp greatereqp))
      (arithsolve atf))
     (t
      ;; KLUDGE -- look at arg2 if arg1 is nil
      (case (or (prtype (arg1 atf)) (prtype (arg2 atf)))
	((nil 
	  functional
	  bool )
	 (ncons (case (funsym atf)		
		  (equal  (cond ((equal (arg1 atf) (arg2 atf)) 'true)
				(t atf)
				))
		  ;;		   ((lessp nequal lesseqp greaterp greatereqp) atf) ;NSH
		  (t      `(equal ,atf true)) )))
	(number  (arithsolve atf))
	(integer (arithsolve atf))
;	(set     (setsolve atf))
	(tuple   (tupsolve atf))
	(array   (arraysolve atf))
	(t       (error "No solver for type " (prtype (arg1 atf))))
	)))))

; ------------------------------------------------------------------
; nsolve:  solver function for negative atomic formula (NOT has been removed)
;
; Argument is an atomic formula (atf).  E.g., p(x, y), x=y+2, x<y+2, etc.
; Returns a list of atfs.
;
; "needed-if*" is checked to see if atf has if*. If so, if*'s are
; lifted to the top and the negated result is returned as a singleton list.
;
; nsolve may do a *throw instead of returning, via RETFALSE.
; called functions also may do this.

(defun nsolve (atf)
  (let* ((atf (if needed-if* (liftif* atf) atf)))
    (cond
     ((atom atf)
      (ncons (case atf
	       (true  'false)
	       (false 'true)
	       (t     `(equal ,atf false)) )))
     ((and *lambda-reduced* (needs-bool-lifting atf)) ; added for bool lifting (sj).
      (cond
       (needed-if*
	(ncons (list 'not (lift-bools (liftif atf)))))
       (t (ncons (list 'not (lift-bools atf))))))
     ((memq (funsym atf) '(if if*))
      (if *split-on-if-in-solve*
	  (let ((leftnsolve (catch 'context (nsolve (arg2 atf))))
		(rightnsolve (catch 'context (nsolve (arg3 atf)))))
	    (if (equal leftnsolve rightnsolve)
		(if (member leftnsolve '(false true))
					; Sept 14, 1990: DAC
					; nsolve should always return a list
					; but catching 'context might not.
		    (ncons leftnsolve)
		  leftnsolve)
	      (ncons (list 'not (liftif* atf)))))
	(if (equal (arg2 atf) (arg3 atf))
	    (nsolve (arg2 atf))
	  (ncons (list 'not (liftif* atf))))))
     ((memq (funsym atf) '(lessp lesseqp greaterp greatereqp))
      (arithnsolve atf))
     (t
					; KLUDGE -- look at arg2 if arg1 is nil
      (case (or (prtype (arg1 atf)) (prtype (arg2 atf)))
	((bool				; uninterpreted negative literal
	  nil
	  functional ) 
	 (ncons (case (funsym atf)		
		  (equal  (cond
			   ((equal (arg1 atf) (arg2 atf)) 'false)
			   (t
			    (prog (result)
			      (setq result
				    (newcontext (process1 (ncons atf)))
				    )
			      (return
			       (cond ((eq result 'true) (retfalse))
				     ((eq result 'false) 'true)
				     (t  `(nequal ,(arg1 atf)
						  ,(arg2 atf)
						  ))))))))
		  (nequal (cond ((equal (arg1 atf)(arg2 atf)) 'true)
				(t `(equal ,(arg1 atf) ,(arg2 atf)))))
		  ((lessp lesseqp greaterp greatereqp) (negineq atf)) ;NSH
		  (t      `(equal ,atf false)) )))
	(number  (arithnsolve atf))
	(integer (arithnsolve atf))
;	(set     (setnsolve atf))
	(tuple   (tupnsolve atf))
	(array   (arraynsolve atf))
	(t       (error "No nsolve for type "
			(or (prtype (arg1 atf)) (prtype (arg2 atf))) ))
	)))))

(defun addneq(neqn)
   (prog(fnsym term1 term2 number_ineq t1>t2 t2>t1)
     (setq term1 (lside neqn))
     (setq term2 (rside neqn))
     (setq number_ineq (member (or (prtype term1) (prtype term2))
			       '(integer number)))
     (when number_ineq
       (setq t1>t2 (newcontext (process `(greatereqp ,term1 ,term2))))
       (setq t2>t1 (newcontext (process `(greatereqp ,term2 ,term1)))))
     ;(setq t1>t2 (or (equal (pr-find `(greatereqp ,term1 ,term2)) 'true)
     ;                (equal (pr-find `(lesseqp ,term2 ,term1)) 'true)))
     ;(setq t2>t1 (or (equal (pr-find `(greatereqp ,term2 ,term1)) 'true)
     ;                (equal (pr-find `(lesseqp ,term1 ,term2)) 'true)))
     (when number_ineq
       (cond ((and
	       (equal t1>t2 'true)
	       (equal t2>t1 'true)) (retfalse))
	     ((equal t1>t2 'true)
	      (setq s (append (solve `(greaterp ,term1 ,term2)) s)))
	     ((equal t2>t1 'true)
	      (setq s (append (solve `(greaterp ,term2 ,term1)) s)))))
     (setq fnsym (cons term1 term2))
     (adduse (list fnsym term1) term1)
     (adduse (list fnsym term2) term2)))
