;; /* Copyright (c) SRI International 2003, 2004. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; input2polyrep.lisp --
;; Author          : Ashish Tiwari
;; Created On      : ?? 
;; Last Modified By: Ashish Tiwari
;; Last Modified On: Wed Apr 26, 2006
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;; Convert parsed input into our internal representation (polyrep.lisp)
;; and call the "sos" semi-decision procedure
;; Interface: (solve "<filename>")
;; Output: Maybe satisfiable or  Unsatisfiable.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:defpackage :cl-user (:use "polynomial-representation-core" "gb" "gbfe"))
(in-package "user") 

(defun var2var (str)
  "String variable name --> symbol for that variable"
  (multiple-value-bind (var status) (intern str)
    (if (null status) 
	(progn (format t "Found undeclared variable. Aborting.~%") 
	       (exit))
	var)))

(defun PP2prep (pp &optional (res nil))
  "Parsed power-product --> power-product in polynomial representation"
  (if (null pp) res
      (let* ((xdotc (car pp))
	     (x (var2var (car xdotc))))
	(PP2prep (cdr pp)
		 (prep:polyrepMultiplyPP (acons x (cdr xdotc) nil) res)))))

(defun poly2prep (poly &optional (res nil))
  "Parsed polynomial --> polynomial in local representation"
  (if (null poly) res
      (let* ((pp (car poly))
	     (newpp (PP2prep (cdr pp))))
	(poly2prep (cdr poly)
		   (if (eq (car pp) 0) res
		   (prep:polyrepAddPoly (list (cons (car pp) newpp)) res))))))

(defun  solve (filename)
  "Read problem instance from file and test its satisfiability.
   File --> Var-Decl Prob
   Var-Decl --> var1 var2 ...
   Prob is specified by providing one (nonlinear) constraint on each line."
  (flet ((check (ans name) 
	   (format t "~a: ~a~a.~%" name (if ans "maybe " "un") "satisfiable")))
  (let* ((str (open filename)))
    (multiple-value-bind (vstr E R S) (gbfe::parse-main str)
      (close str)
      (prep:set-variables (mapcar #'(lambda(x)(intern x)) vstr))
      (prep:set-parameters nil )
      (if (null gb::*dlevel*) (gb:set-debug-level 7))
      (gb::set-newU nil)
      (gb::set-newV nil)
      (let ((E1 (mapcar #'poly2prep E))
	    (R1 (mapcar #'poly2prep R))
	    (S1 (mapcar #'poly2prep S)))
        (check (sos E1 R1 S1) filename))))))
