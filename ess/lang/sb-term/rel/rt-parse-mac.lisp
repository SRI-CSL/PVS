;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-parse-mac.lisp	1.5 11/3/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.

;;; Anne Rogers

;;; Parser Runtime Macros

;;; Revised, Scott Dietzen, New term representation added for the results of
;;; parsing repetitive patterns. 


(in-package :sb-runtime)  (use-package :ergolisp)

(export '(leading-epsilon-p make-var-name      ;; rt-second-epsilon-p
	  epsilon full-la-match
	  la-match lam look-ahead-match gobble-to-slot constructor-to-slot
	  slot-to-slot cons-to-slot code-to-slot toss-value as-to-slot
	  value-to-slot initials medials initials-only alt-parse
	  make-slot-array opt-parse
	  star-parse gen-star-parse plus-parse doublestar-parse
	  doubleplus-parse parser-error push-bracket pop-bracket bracket-list
	  top-bracket empty-bracket-list not-empty-bracket-list stack-brackets
	  eat-brackets clean-variables

	  make-sb-term
	  mk-temp-rept ds-temp-rept mk-temp-star mk-temp-plus ck-rept-ref
	  ))

;;; This is necessary because some of these macros do not take all the
;;; arguments they should, and this is the minimal effort, if not safest,
;;; fix.
(export '(rbp))


(eval-when (:compile-toplevel :execute :load-toplevel)

;;; Determine whether this entry in a fs-list allows an epsilon for the second 
;;; token. The "rt" in the name is to avoid a name conflict with an SB routine
;;; doing the same thing.

(defmacro rt-second-epsilon-p (fs-entry)
  `(member 'epsilon (cdr ,fs-entry)))

;;; Look Ahead match macros 
;;; See if any entry in the fs-list matches (peek-first) and (peek-second).
  
(defmacro la-match (fs-list) `(la-match-fun ',fs-list))

) ;; eval-when 


(defun la-match-fun (fs-list)
  (if (assoc 'epsilon fs-list) t            ; Everything matches epsilon.
      (let ((temp (assoc (peek-first) fs-list)))
	 (cond (temp
		(or (rt-second-epsilon-p temp)
		    (null (cdr temp))
		    (member (peek-second) (cdr temp))))))))

;;; If code isn't null, execute code if the next two tokens match fs-list,
;;; otherwise error. If code is null, return true if the things match,
;;; otherwise error and return nil.  It isn't clear whether the original code
;;; always returned nil when an error occurred.

(eval-when (:compile-toplevel :execute :load-toplevel)

(defmacro lam (fs-list &body code)
  (if code
      `(if (la-match ,fs-list) (progn . ,code)
	   (lam-error (quote ,fs-list)))
      `(if (la-match ,fs-list) t
	   (progn (lam-error (quote ,fs-list)) nil))))

;;; Slot management macros.

(defmacro gobble-to-slot (slot)
  (let ((res (gensym)))
    `(let ((,res (gettoken)))
       (if (symbolp ,res)
	   (setq ,slot (intern (symbol-name ,res) *abs-syn-package*))
	   (setq ,slot ,res)))))

(defmacro constructor-to-slot (slot code)
  `(setq ,slot ,code))

(defmacro slot-to-slot (slot1 slot2)
  `(setq ,slot1 ,slot2))

(defmacro cons-to-slot (slot1 slot2)
  `(push ,slot2 ,slot1))

(defmacro code-to-slot (slot code)
  `(setq ,slot ,code))

(defmacro toss-value (code) code)

(defmacro as-to-slot (slot as)
  `(setq ,slot ,as))

(defmacro value-to-slot (slot value)
  `(setq ,slot ,value))


  
;;; General macros.
(defmacro initials (&rest clauses)
  `(cond ,@clauses))


(defmacro medials (&rest clauses)
  `(cond ,@clauses))


(defmacro initials-only (&rest clauses)
  `(cond ,@clauses))


(defmacro alt-parse (&rest clauses)
  `(cond ,@clauses))

(defmacro make-slot-array (size)
  `(make-array ,size))

  
  ;; Code macros for Specification language constructs

(defmacro make-sb-term (sim-op args)
  `(term:mk-sim-term ,sim-op ,args))

(defmacro mk-temp-rept (args)
  `(mk-sim-term 'temp-rept-sb-internal
		,args))
(defmacro ds-temp-rept (x)
  `(term-args ,x))

(defmacro mk-temp-star (args)
  `(mk-temp-rept ,args))

(defmacro mk-temp-plus (args)
  `(mk-temp-rept ,args))

(defmacro ck-rept-ref (as)
  `(let ((as ,as))
     (if (null as)
	 (mk-temp-rept nil)
	 as)))


(defmacro opt-parse (fs-list code flag)
  `(cond ((la-match ,fs-list)
	  (value-to-slot ,flag 1)
	  ,code)
	 (t (value-to-slot ,flag 0))))



(defmacro star-parse (fs-list code result partial)
  `(cond ((la-match ,fs-list)
	  (setq ,result nil)
	  (loop
	   ,code
	   (cons-to-slot ,result ,partial)
	   (when (not (la-match ,fs-list)) (return nil)))
	  (setq ,result (mk-temp-star (nreverse ,result))))
	 (t
	  (setq ,result (mk-temp-star ())))))
                                        ; srd revised. 
  
  ;; GEN-STAR-PARSE is applied to loops which have been partial unrolled (so
  ;; PREV-RESULT may already contain a List of partial results).  This macro
  ;; gets the rest of the partials (possible empty) and then makes a list term
  ;; out of them.  (srd revised).
(defmacro gen-star-parse (fs-list code prev-result partial)
  `(let ()
     (cond ((la-match ,fs-list)
	    (loop
	     ,code
	     (cons-to-slot ,prev-result ,partial)
	     (when (not (la-match ,fs-list)) (return nil)))))
     (setq ,prev-result (mk-temp-star (nreverse ,prev-result)))))
					;srd revised. 
  
(defmacro plus-parse (fs-list code result partial)
  `(cond ((la-match ,fs-list)
	  (setq ,result nil)
	  (loop
	   ,code
	   (cons-to-slot ,result ,partial)
	   (when (not (la-match ,fs-list)) (return nil)))
	  (setq ,result (mk-temp-plus (nreverse ,result))))
					; srd revised. 
	 (t (initial-error (quote ,fs-list)))))


(defmacro doublestar-parse (fs-list code result partial sep)
  `(cond ((la-match ,fs-list)
	  (setq ,result nil)
	  (loop
	   ,code
	   (cons-to-slot ,result ,partial)
	   (when (not (eq (peek-first) ,sep))
	     (return nil))
	   (gobble-token))
	  (setq ,result (mk-temp-star (nreverse ,result))))
	 (t
	  (setq ,result (mk-temp-star ())))))
                                        ; srd revised. 
  
(defmacro doubleplus-parse (fs-list code result partial sep)
  `(cond ((la-match ,fs-list)
	  (setq ,result nil)
	  (loop
	   ,code
	   (cons-to-slot ,result ,partial)
	   (when (not (eq (peek-first) ,sep))
	     (return nil))
	   (gobble-token))
	  (setq ,result (mk-temp-star (nreverse ,result))))
	 (t
	  (initial-error (quote ,fs-list)))))

                                        ; srd revised. 


  
;;; Bracket macros
;;; an entry on the bracket stack looks like (opening-bracket rbp nt-name)
;;; This guy inherits bracket-list!!!
(defmacro push-bracket (entry)
  `(push ,entry (car bracket-list)))

;;; the result of pop bracket is a list containing the rbp
;;; from the bracket stack and the name of the nonterminal associated with
;;; it.
;;; This guy inherits bracket-list!!!
(defmacro pop-bracket ()
  `(prog1
       (cdr (caar bracket-list))
     (setf (car bracket-list) (cdar bracket-list))))

;;; This guy inherits bracket-list!!!
(defmacro top-bracket ()
  `(caaar bracket-list))

(defmacro empty-bracket-list ()
  `(list nil))

;;; Poor style here.  This predicate should be in positive form and be named
;;; empty-bracket-list-p.
;;; This guy inherits bracket-list!!!
(defmacro not-empty-bracket-list ()
  `(not (equal bracket-list (empty-bracket-list))))

;;; This guy takes rbp by inheritance!!!
;;; This guy inherits bracket-list!!!
(defmacro stack-brackets (leading-bracket nt)
  `(loop
    (if (eq (peek-first) (quote ,leading-bracket))
	(progn (push-bracket (list (peek-first) rbp (quote ,nt)))
	       (setq rbp 0)
	       (gobble-token))
	(return nil))))

;;; This guy takes rbp by inheritance!!!
;;; This guy inherits bracket-list!!!
(defmacro eat-brackets (brackets nt)
  `(loop
    (if (and (not-empty-bracket-list)
	     (eq (peek-first) (quote ,(car brackets)))
	     (eq (top-bracket) (quote ,(cadr brackets))))
	(let ((temp (pop-bracket)))
	  (if (eq (cadr temp) (quote ,nt))
	      (setq rbp (car temp)))
	  (gobble-token))
	(return nil))))

;;; Set all of the vars in var-list to nil.  
(defmacro clean-variables (var-list)
  `(progn . ,(mapcar #'(lambda (var) `(setq ,var nil)) var-list)))


) ;; eval-when

