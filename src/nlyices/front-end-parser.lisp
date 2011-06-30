;; /* Copyright (c) SRI International 2003, 2004. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; front-end-parser.lisp --
;; Author          : Ashish Tiwari
;; Created On      : ?? 
;; Last Modified By: Ashish Tiwari
;; Last Modified On: Wed Apr 26, 2006
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;; Simple parser for conjunction of nonlinear constraints given in a stream
;; Grammar: 
;;   FILE --> Var-Decl Problem-Instance
;;   Var-Decl --> Var *
;;   Problem-Instance --> Fmla *
;;   Fmla --> Poly RELOP Poly
;;   Poly --> Term *
;;   Term --> [SGN] [Cst] [PP]
;;   PP   --> ExpTerm *
;;   ExpTerm --> Var EXPOP Cst
;;   RELOP --> < | > | <= | >= | =
;;   SGN   --> + | -
;;   EXPOP --> ^
;; Sample File: "x1 \n x1 < 5 \n x1 > 6 \eof"
;; Sample Fmla: 4 x1^2 x2 - x2 + 23 x2^4 - 4 + 23 = 0
;; Sample parsed fmla: (= ((23) (-4) (23 (x2 . 4)) (-1 (x2 . 1)) (4 (x2 . 1) (x1 . 2))) ((0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'gb-front-end)		;; Front end for the GB based decision proc
(cl:defpackage "gb-front-end"
  (:nicknames "gbfe")
  (:export "parse-main")
  (:use "cl" "user" "clos"))

(in-package "gb-front-end")	;; The simple parser

(defparameter SGN (list "+" "-"))
(defparameter EXPOP (list "^"))
(defparameter RELOP (list "<" ">" "<=" ">=" "="))
(defparameter white-space '(#\Space #\Tab #\Newline ( )))

(defvar *dlevel* 3)

(defun set-debug-level (n)
  (setf *dlevel* n))

(defmacro print-debug (level &rest others)
  `(if (and *dlevel* (> ,level *dlevel*))
       (funcall #'format ,@others)))

(defun parse-main (str)
  "Parse stream str for a string specified by the Grammar FILE:
   FILE --> Var-Decl Problem-Instance
   Var-Decl --> Var * "
  (multiple-value-bind (line eof) (read-line str)
    (multiple-value-bind (VARS line) (parse-varlist line) ;; Parse Var-Decl
      (declare (ignore line eof));; anything other than varlist is ignored
      (declare (special VARS))
      (multiple-value-bind (E R S) (parse-main* str) ;; Parse Problem-Instance
	(values VARS E R S)))))

(defun parse-varlist (str &optional (res nil))
  "Parse string str for a string specified by the Grammar Var-Decl :
   Var-Decl --> Var * "
  (multiple-value-bind (X str2) (parse-setX str)
    (if (null X) (values (nreverse res) str)
	(parse-varlist str2 (cons X res)))))
  
(defun parse-setX (str)
  "Parse string str for a string specified by the Grammar Var :
   Var --> white-space* [~white-space]* {white-space|end_of_str} "
  (let* ((str2 (remove-white-space str))
	 (pred #'(lambda(x) (member x white-space)))
	 (ind1  (position-if pred str2))
	 (index (if (null ind1) (length str2) ind1)))  
    (if (eq index 0) (values nil str2)
	(values (subseq str2 0 index) (subseq str2 index)))))

(defun parse-main* (str &optional (E nil) (R nil) (S nil))
  "Parse stream str for a string specified by the Grammar Problem-Instance:
   Problem-Instance --> Fmla * "
  (multiple-value-bind (line eof) (read-line str nil "")
    (multiple-value-bind (fmla str2) (parse-fmla line)
      (declare (ignore str2))	;; anything other than 1 formula ignored
      (cond ((and (null fmla) eof)
	     (values E R S))
	    ((null fmla)
	     (parse-main* str E R S))
	    (t
	     (multiple-value-bind (E1 R1 S1) (fmla2ERS fmla E R S)
	    	(if eof (values E1 R1 S1)
		    (parse-main* str E1 R1 S1))))))))
  
(defun fmla2ERS (fmla E R S)
  "Put formula in either E,R,or S depending on if it has =, >, or >=."
  (flet ((nneg (p) (loop for i in p do (setf (car i) (- 0 (car i)))) p))
  (let ((p1 (cadr fmla))
	(p2 (caddr fmla))
	(op (car fmla)))
    (cond ((string= op "=")
	   (values (cons (nconc p1 (nneg p2)) E) R S)) 
	  ((string= op ">")
	   (values E (cons (nconc p1 (nneg p2)) R) S)) 
	  ((string= op ">=")
	   (values E R (cons (nconc p1 (nneg p2)) S))) 
	  ((string= op "<")
	   (values E (cons (nconc (nneg p1) p2) R) S))
	  ((string= op "<=")
	   (values E R (cons (nconc (nneg p1) p2) S)))
	  (t (format t "Unidentified operator~A~%" (car fmla)) (break))))))


(defun remove-white-space (str)
  "Remove white spaces from string str."
  (string-left-trim white-space str))

;; parse a coeff: integer only
(defun parse-coef (str)
  "Parse an integer from string str."
  (let ((str1 (remove-white-space str)))
  (multiple-value-bind (c ind)
  	(parse-integer str1 :junk-allowed t)
    (values c (subseq str1 ind)))))

;; parse a terminal from a finite set Y
(defun parse-elmt (str Y)
  "Parse string str for a string in a set Y."
  (let ((str1 (remove-white-space str)))
    (multiple-value-bind (ans substr) 
      (loop for i in Y
	if (and (>= (length str1) (length i))
		(string= str1 i :end1 (length i)))
	return (values i (subseq str1 (length i))))
      (if ans (values ans substr)
  	  (values nil str1)))))
  
;; parse a variable: from VARS
(defun parse-X (str)
  "Parse string str for a variable name."
  (declare (special VARS))
  (parse-elmt str VARS))

;; parse a sign +,-
(defun parse-sign (str)
  "Parse stream str for a string specified by the Grammar SGN :
   SGN   --> + | - "
  (parse-elmt str SGN))

;; parse a relop <, >, ...
(defun parse-relop (str)
  "Parse stream str for a string specified by the Grammar RELOP :
   RELOP --> < | > | <= | >= | = "
  (parse-elmt str RELOP))

;; parse a exp-op ^
(defun parse-exp-op (str)
  "Parse stream str for a string specified by the Grammar EXPOP :
   EXPOP --> ^ "
  (parse-elmt str EXPOP))

;; parse a X^4
(defun parse-exp (str)
  "Parse stream str for a string specified by the Grammar ExpTerm :
   ExpTerm --> Var EXPOP Cst "
  (multiple-value-bind (X str2) (parse-X str)
    (if (null X) (values nil 0 str)
	(multiple-value-bind (eop str3) (parse-exp-op str2)
	  (if (null eop) (values X 1 str2)
	      (multiple-value-bind (c str4) (parse-coef str3)
		(if (null str4) (print-debug 10 t "Exp Parse error ~a~%" str2)
		    (values X c str4))))))))

;; parse X^3 Y^2 ...
(defun parse-PP (str &optional (result nil))
  "Parse stream str for a string specified by the Grammar PP :
   PP   --> ExpTerm * "
  (multiple-value-bind (X e str2) (parse-exp str)
    (if (null X) (values result str)
	(parse-PP str2 (acons X e result)))))

;; parse +- c X^3 Y^4 ...
(defun parse-term (str)
  "Parse stream str for a string specified by the Grammar Term :
   Term --> [SGN] [Cst] [PP] "
  (flet ((pos? (sgn) (or (null sgn) (string= sgn "+"))))
  (multiple-value-bind (sgn str1) (parse-sign str)
  (multiple-value-bind (c str2) (parse-coef str1)
  (multiple-value-bind (pp str3) (parse-PP str2)
    (cond ((and (pos? sgn) (null c) (null pp)) 
	   (values nil str))
	  ((and (pos? sgn) (null c) pp) 
	   (values (cons 1 pp) str3))
	  ((and (pos? sgn) c (null pp)) 
	   (values (cons c nil) str2))
	  ((and (pos? sgn) c pp) 
	   (values (cons c pp) str3))
	  ((and (null c) (null pp)) 
	   (print-debug 10 t "term Parse error ~a~%" str))
	  ((and (null c) pp) 
	   (values (cons -1 pp) str3))
	  ((and c (null pp)) 
	   (values (cons (- 0 c) nil) str2))
	  ((and c pp) 
	   (values (cons (- 0 c) pp) str3))))))))

;; parse a polynomial = sum of terms = +- c X^3 Y^4 +- c X^2 Y^2 ...
(defun parse-poly (str &optional (result nil))
  "Parse stream str for a string specified by the Grammar Poly :
   Poly --> Term * "
  (multiple-value-bind (term str2) (parse-term str)
    (if (null term) (values result str)
	(parse-poly str2 (cons term result)))))

;; parse atomic formula, poly RELOP poly
(defun parse-fmla (str)
  "Parse stream str for a string specified by the Grammar FMLA:
   Fmla --> Poly RELOP Poly "
  (multiple-value-bind (p1 str2) (parse-poly str)
    (if (null p1) (values nil str)
	(multiple-value-bind (rop str3) (parse-relop str2)
	  (if (null rop) (print-debug 10 t "fmla Parse error ~a~%" str)
	      (multiple-value-bind (p2 str4) (parse-poly str3)
	  	(if (null p2) (print-debug 10 t "fmla Parse error ~a~%" str)
		    (values (list rop p1 p2) str4))))))))

