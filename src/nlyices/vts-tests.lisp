;; /* Copyright (c) SRI International 2011. */
;;;;;;;;;;;;;;;;;;;;;;;;;;* -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vim: syntax=lisp
;; vts-tests.lisp --
;; Author          : Tim King
;; Created On      : Thu Jul 08, 2011
;; Last Modified By: Tim King
;; Last Modified On: 
;; Status          : Unknown, use with caution
;;
;; HISTORY :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(cl:defpackage :cl-user (:use "polynomial-representation-core" "vts"))
;;(in-package "user") 

;;(eval (use-package "polynomial-representation-core"))
;;(eval (use-package "decision-procedure-core"))

; (defvar *order* nil)
; (defvar *parameters* nil)
; (setq *order* '(x1 x2 x3 x4 x5))
; (setq *parameters* '(c1 c2 c3 c4 c5))

;; TODO List
;; (test12)
;; (test13)

(defun mk-sal-nameexpr (flag name)
  (let ((sal (find-package "sal")))
    (if sal (funcall (find-symbol "mk-sal-nameexpr" sal) flag name)
	    name)))

(defvar x1 nil)
(defvar x2 nil)
(defvar x3 nil)
(defvar x4 nil)
(defvar x5 nil)
(defvar c1 nil)
(defvar c2 nil)
(defvar c3 nil)
(defvar c4 nil)
(defvar c5 nil)

(setq x1 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x1) 1)))))
(setq x2 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x2) 1)))))
(setq x3 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x3) 1)))))
(setq x4 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x4) 1)))))
(setq x5 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x5) 1)))))

(setq c1 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c1) 1)))))
(setq c2 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c2) 1)))))
(setq c3 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c3) 1)))))
(setq c4 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c4) 1)))))
(setq c5 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c5) 1)))))

(defun toPolyRepCnst (x)
  (list (list x)))


(defun compile-and-load (name)
  (progn (compile-file (format nil "~a.lisp" name))
	 (load name)))

(defun cal-vts () (compile-and-load "vts"))

(defun init-test ()
  (prep:set-variables (copy-list '(x1 x2 x3 x4 x5 c1 c2 c3 c4 c5)))
  (prep:set-parameters nil )
  (vts:reset-ids))

(defvar *print-tree* nil)
(defvar *ignore-status* nil)

(defun check (cs status-test name)
  (multiple-value-bind
	(status root candidate-sat-nodes) (vts:is-consistent cs)
    (let ((success (funcall status-test status)))
      (if *print-tree* (format t "~% Root ~%~a~%" root))
      (format t "~%Test \"~a\" had status ~a success: ~a~%" name status success)
      (if (not *ignore-status*) (assert success))
      (format t "~%Test \"~a\" succeeded!~%" name)
      (format t "Summary: ~a~%" (vts:summarize-node root)))))

(defun test-list ()
  (let ((prev *ignore-status*))
    (setf *ignore-status* t)
    (test1)
    (test2)
    (test3)
    (test4)
    (test5)
    (test6)
    (test7)
    (test8)
    (test9)
    (test10)
    (test11)
    (test12)
    (test13)
    (test14)
    (test15)
    (test16)
    (test17)
    (test18)
    (test19)
    (test20)
    (test21)
    (test22)
    (test23)
    (test24)
    (test25)
    (test26)
    (test27)
    (test28)
    (test29)
    (test20)
    (test21)
    (test22)
    (test23)
    ;;(test24)
    (test25)
    (test26)
    (test27)
    (test28)
    (test29)
    (test30)
    (test31)
    (test32)
    (test33)
    (test34)
    (test35)
    (test36)
    (test37)
    (test38)
    ;;(test39)
    (test40)
    (test41)
    (test42)
    (test43)
    (test44)
    (test45)
    (test46)
    (test47)
    (test48)
    ;;(test49)
    ;;(test50)
    (setf *ignore-status* prev)))

(defun test-all ()
 (loop for i upfrom 1 do
   (let ((real-t0 (get-internal-real-time))
	 (run-t0 (get-internal-run-time))
	 (ans (funcall (find-symbol (format nil "test~A" i))))
	 (run-t1 (get-internal-run-time))
	 (real-t1 (get-internal-real-time)))
     (format t "Test~A: real-time = ~A, run-time = ~A~%" i (- real-t1 real-t0) (- run-t1 run-t0)))
   while  (< i 46)))

(defun test-poly (c)
  (init-test)
  (let ((lc (list c)))
    (list 
     (vts:is-consistent (vts:poly-lists-to-constraint-set lc nil nil nil))
     (vts:is-consistent (vts:poly-lists-to-constraint-set nil lc nil nil))
     (vts:is-consistent (vts:poly-lists-to-constraint-set nil nil lc nil))
     (vts:is-consistent (vts:poly-lists-to-constraint-set nil nil nil lc)))))

(defun test-constants ()
  (append (test-poly nil) (test-poly (toPolyRepCnst 1)) (test-poly (toPolyRepCnst -1))))

;; x1 = 0
(defun test-singleton1 ()
  (init-test)
  (let* ((E (list x1))
	 (NZ nil)
	 (R nil)
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "test-singleton1")))

;; x1 != 0
(defun test-singleton2 ()
  (init-test)
  (let* ((E nil)
	 (NZ (list x1))
	 (R nil)
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "test-singleton2")))

;; x1 > 0
(defun test-singleton3 ()
  (init-test)
  (let* ((E nil)
	 (NZ nil)
	 (R (list x1))
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "test-singleton3")))

;; x1 >= 0
(defun test-singleton4 ()
  (init-test)
  (let* ((E nil)
	 (NZ nil)
	 (R nil)
	 (NN (list x1)))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "test-singleton4")))

;; x2 != 0 and x2 > 2
;; The constraint x2 != 0 should be subsumed by x2 > 2
;; Look at the tree to confirm the test works!
(defun test-sub1 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ (list x2))
	 (R (list x2minus2)))
    (check (vts:poly-lists-to-constraint-set E NZ R nil) #'vts:is-sat? "Test-Sub1")))

;; x2 != 0 and -2 > x2
;; The constraint x2 != 0 should be subsumed by x2 < -2
;; Look at the tree to confirm the test works!
(defun test-sub2 ()
  (init-test)
  (let* ((two-minus-x2 (prep:polyrepNegativePoly  (prep:polyrepAddPoly x2 (toPolyRepCnst 2))))
  	 (E nil)
	 (NZ (list x2))
	 (R (list two-minus-x2)))
    (check (vts:poly-lists-to-constraint-set E NZ R nil) #'vts:is-sat? "Test-Sub2")))

;; x2 = 2 and x2 < 3 and x2 > 1
;; The constraint x2 != 0 should be subsumed by x2 < -2
;; Look at the tree to confirm the test works!
(defun test-sub3 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
	 (three-minus-x2 (prep:polyrepNegativePoly  (prep:polyrepAddPoly x2 (toPolyRepCnst -3))))
	 (x2minus1 (prep:polyrepAddPoly x2 (toPolyRepCnst -1)))
  	 (E (list x2minus2))
	 (NZ nil)
	 (R (list three-minus-x2 x2minus1)))
    (check (vts:poly-lists-to-constraint-set E NZ R nil) #'vts:is-sat? "Test-Sub3")))

;; x2 != 2 and x2 <= 2 should be replaced by x2 < 2
;; Look at the tree to confirm the test works!
(defun test-imp1 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ (list x2minus2))
	 (R nil)
	 (NN (list x2minus2)))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "Test-imp1")))

;; x2 = 2 and x2 = 2 should be replaced by x2 = 2
;; Look at the tree to confirm the test works!
(defun test-imp2 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E (list x2minus2 x2minus2))
	 (NZ nil)
	 (R nil)
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "Test-imp2")))

;; x2 != 2 and x2 != 2 should be replaced by x2 != 2
;; Look at the tree to confirm the test works!
(defun test-imp3 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ (list x2minus2 x2minus2))
	 (R nil)
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "Test-imp3")))

;; x2 > 2 and x2 > 2 should be replaced by x2 > 2
;; Look at the tree to confirm the test works!
(defun test-imp4 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ nil)
	 (R (list x2minus2 x2minus2))
	 (NN nil))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "Test-imp4")))

;; x2 >= 2 and x2 >= 2 should be replaced by x2 >= 2
;; Look at the tree to confirm the test works!
(defun test-imp5 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ nil)
	 (R nil)
	 (NN (list x2minus2 x2minus2)))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-sat? "Test-imp5")))

;; x2 = 2 and x2 < -3
;; Pairwise conflict.
;; Look at the tree to confirm the test works!
(defun test-con1 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
	 (three-minus-x2 (prep:polyrepNegativePoly  (prep:polyrepAddPoly x2 (toPolyRepCnst 3))))
  	 (E (list x2minus2))
	 (NZ nil)
	 (R (list three-minus-x2)))
    (check (vts:poly-lists-to-constraint-set E NZ R nil) #'vts:is-unsat? "Test-Con1")))

;; x2 = 2 and x2 != 2
;; Pairwise conflict.
;; Look at the tree to confirm the test works!
(defun test-con2 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E (list x2minus2))
	 (NZ (list x2minus2))
	 (R nil))
    (check (vts:poly-lists-to-constraint-set E NZ R nil) #'vts:is-unsat? "Test-Con2")))

;; x2 <= 2 and x2 >=2 and x2 != 2
;; Pairwise conflict.
;; Look at the tree to confirm the test works!
(defun test-con3 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (NZ (list x2minus2))
	 (R nil)
	 (NN (list x2minus2 (prep:polyrepNegativePoly x2minus2))))
    (check (vts:poly-lists-to-constraint-set E NZ R NN) #'vts:is-unsat? "Test-Con3")))

;; x1 > 5 and x1 < 4
(defun test1 ()
  (init-test)
  (let* ((Z nil)
	 (NZ nil)
	 (NN nil)
  	 (x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
  	 (x1minus5 (prep:polyrepAddPoly x1 (toPolyRepCnst -5)))
	 (P (list x1minus5 (toPolyRepCnst 1) (prep:polyrepNegativePoly x1minus4 )))
	 (cs (vts:poly-lists-to-constraint-set Z NZ P NN)))
    (check cs #'vts:is-unsat? "test1")))

;; x1 = 5 and x1 = 4
(defun test2 ()
  (init-test)
  (let* ((Z nil)
	 (NZ nil)
	 (NN nil)
  	 (x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
  	 (x1minus5 (prep:polyrepAddPoly x1 (toPolyRepCnst -5)))
	 (P (list x1minus5 (prep:polyrepNegativePoly x1minus4 ) (toPolyRepCnst -1)))
	 (cs (vts:poly-lists-to-constraint-set Z NZ P NN)))
    (check cs #'vts:is-unsat? "test2")))


;; x1.x2 = 5 and x1 = 4 and x2 = 2
(defun test3 ()
  (init-test)
  (let* ((x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
	 (x1x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 x2) (toPolyRepCnst -5)))
  	 (x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E (list x1x2minus5 x1minus4 x2minus2)))
    (check (vts:poly-lists-to-constraint-set E nil nil nil) #'vts:is-unsat? "Test3")))
;;With mixed-projection-case-split disabled:
;;Test Test3 succeeded!
;;((STATUS-LEAF . 0) (STATUS-PROJECTION . 9) (STATUS-SUBSTITUTION . 99)
;; (STATUS-SIMPLIFICATION . 98) (STATUS-UNSAT . 223) (STATUS-SAT . 0)
;; (STATUS-BLOCKED . 0))
;;With mixed-projection-case-split enabled:
;;((STATUS-LEAF . 0) (STATUS-PROJECTION . 3) (STATUS-SUBSTITUTION . 3)
;; (STATUS-SIMPLIFICATION . 5) (STATUS-UNSAT . 7) (STATUS-SAT . 0)
;; (STATUS-BLOCKED . 0))

;; x1.x2 = 5 and x1 > 4 and x2 > 2
(defun test4 ()
  (init-test)
  (let* ((x1x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 x2) (toPolyRepCnst -5)))
	 (E (list x1x2minus5))
  	 (x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
  	 (x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
	 (R (list x1minus4 x2minus2)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test4")))


;; x1.x2 < 5 and x1 > 4 and x2 > 2
(defun test5 ()
  (init-test)
  (let* ((x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
	 (x1x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 x2) (toPolyRepCnst -5)))
  	 (x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
  	 (E nil)
	 (R (list (prep:polyrepNegativePoly x1x2minus5) x1minus4 x2minus2)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test5")))


;; Grobner Basis: x1.x2 = 5, x1.x2.x2 = 5, x2 = 2
(defun test6 ()
  (init-test)
  (let* ((x1x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 x2) (toPolyRepCnst -5)))
  	 (x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
	 (x1x2x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 (prep:polyrepMultiplyPoly x2 x2)) (toPolyRepCnst -5)))
  	 (E (list x1x2minus5 x1x2x2minus5 x2minus2))
	 (R nil))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test6")))

;; Grobner Basis: x1.x2.x2 = 5, x1.x2 = 5, x1 = 4
(defun test7 ()
  (init-test)
  (let* ((x1minus4 (prep:polyrepAddPoly x1 (toPolyRepCnst -4)))
	 (x1x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 x2) (toPolyRepCnst -5)))
	 (x1x2x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 (prep:polyrepMultiplyPoly x2 x2)) (toPolyRepCnst -5)))
  	 (E (list x1x2x2minus5 x1x2minus5 x1minus4))
	 (R nil))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test7")))

;; Grobner Basis: x1.x2.x2 = 5, x1.x1.x2 = 2, x2 = 2
(defun test8 ()
  (init-test)
  (let* ((x2minus2 (prep:polyrepAddPoly x2 (toPolyRepCnst -2)))
	 (x1x2x2minus5 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 (prep:polyrepMultiplyPoly x2 x2)) (toPolyRepCnst -5)))
	 (x1x1x2minus2 (prep:polyrepAddPoly (prep:polyrepMultiplyPoly x1 (prep:polyrepMultiplyPoly x1 x2)) (toPolyRepCnst -2)))
  	 (E (list x1x2x2minus5 x1x1x2minus2 x2minus2))
	 (R nil))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test8")))

;; Linear inequalities test
;; x1 - x2 < 0, x2 - x3 < 0, x3 - x4 < 0, x4 - x1 < 0
(defun test9 ()
  (init-test)
  (let ((E nil)
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2))
		(prep:polyrepAddPoly x2 (prep:polyrepNegativePoly x3))
		(prep:polyrepAddPoly x3 (prep:polyrepNegativePoly x4))
		(prep:polyrepAddPoly x4 (prep:polyrepNegativePoly x1)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test9")))

;; x1 - x2 > 0 AND x2 - x1 > 0
(defun test10 ()
  (init-test)
  (let ((E nil)
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2))
		(prep:polyrepAddPoly x2 (prep:polyrepNegativePoly x1)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test10")))


;; -x - 2y = 0 AND x = 0  AND x - y > 0
(defun test11 ()
  (init-test)
  (let ((E (list x1 (prep:polyrepAddPoly
			  (prep:polyrepAddPoly (prep:polyrepNegativePoly x1)
				(prep:polyrepNegativePoly x2))
				(prep:polyrepNegativePoly x2))))
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test11")))

;; x1 - x2 > 0 AND x2 > 0 AND x1 > 0
(defun test12 ()
  (init-test)
  (let ((E nil)
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)) x2 x1)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test12")))

;; test12 with repeated assertions of same fact.
(defun test13 ()
  (init-test)
  (let ((E nil)
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)) x2 x1 x2 (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test13")))

;; pzzzz and nzzzz:  x-y,y,x,y+x,-y-2x
(defun test14 ()
  (init-test)
  (let ((E (list x2 x1 (prep:polyrepAddPoly x1 x2) 
		(prep:polyrepAddPoly (prep:polyrepNegativePoly x1) 
			(prep:polyrepAddPoly (prep:polyrepNegativePoly x2)
					(prep:polyrepNegativePoly x2)))))
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test14")))

;; pzzzz and nzzzz:  x-y,y,x,y+x,-y-2x
;; Witness returned is LARGER, because we SWAP equations in GB when processing new eqns.
(defun test15 ()
  (init-test)
  (let ((E (list x2 x1 (prep:polyrepAddPoly x1 x2) 
		(prep:polyrepAddPoly (prep:polyrepNegativePoly x1) 
			(prep:polyrepAddPoly (prep:polyrepNegativePoly x2)
					(prep:polyrepNegativePoly x2)))))
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set (nreverse E) nil R nil) #'vts:is-unsat? "Test15")))

;; -x2 - x1 - x1 > 0 AND x2 + x1 > 0 AND x2 > 0 AND x1 > 0
(defun test16 ()
  (init-test)
  (let ((E (list (prep:polyrepAddPoly (prep:polyrepNegativePoly x2) 
			(prep:polyrepAddPoly (prep:polyrepNegativePoly x1)
					(prep:polyrepNegativePoly x1)))
		(prep:polyrepAddPoly x2 x1) x2 x1))
	(R (list (prep:polyrepAddPoly x2 (prep:polyrepNegativePoly x1)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test16")))

;; x3-x4>0; x3>0; x4>0; x1>0; x2>0; x1>0; x1.x2+x1.x3-10<0
(defun test17 ()
  (init-test)
  (let ((E nil)
	(R (list (prep:polyrepAddPoly x3 (prep:polyrepNegativePoly x4)) x3 x4
		x1 x2 x1 (prep:polyrepNegativePoly (prep:polyrepAddPoly (prep:polyrepAddPoly
			(prep:polyrepMultiplyPoly (prep:polyrepNegativePoly x1) x2)
			(prep:polyrepMultiplyPoly (prep:polyrepNegativePoly x1) x3))
	    (list (list 10)))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test17")))

;; R,R,E,R in the original test
;; x2 = 0 AND -x1*x2 - x1*x3 > 0 AND x2 > 0
(defun test18 ()
  (init-test)
  (let* ((E (list x2))
	 (R1 (list (prep:polyrepAddPoly 
		(prep:polyrepMultiplyPoly (prep:polyrepNegativePoly x1) x2)
		(prep:polyrepMultiplyPoly (prep:polyrepNegativePoly x1) x3))))
	 (R21 (append R1 (list x2)))
	 (R22 (append R1 (list (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set E nil R21 nil) #'vts:is-unsat? "Test18.2")))

;; R > I AND c1-iI-IR = 0 AND c2-rR-IR=0 AND c1>c2 AND i<r
;; R=x1, I=x2; c1=c1, c2=c2, r=c3, i=c4
;; c4>0; c3-c4>0; c1-c2>0; x2>0; -x1.x2-x1.c3+c2=0; -x1.x2-x2.c4+c1=0; x1-x2>0
(defun test19 ()
  (init-test)
  (let ((R (list c4 (prep:polyrepAddPoly c3 (prep:polyrepNegativePoly c4))
		(prep:polyrepAddPoly c1 (prep:polyrepNegativePoly c2))
		x2
		(prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2))))
	(E (list
		(prep:polyrepAddPoly c2 (prep:polyrepAddPoly
		(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly c3 x1))
		(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly x1 x2))))
		(prep:polyrepAddPoly c1 (prep:polyrepAddPoly
		(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly c4 x2))
		(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly x1 x2)))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test19")))


;; c4>0; c3-c4>0; c1-c2>0; x1>0; x2>0; x1-x2>0; -c3.x1+c4.x2-c1+c2=0; -x1.x2-x2.c4+c1=0;
(defun test20 ()
  (init-test)
  (let ((R (list c4 (prep:polyrepAddPoly c3 (prep:polyrepNegativePoly c4))
		(prep:polyrepAddPoly c1 (prep:polyrepNegativePoly c2))
		x1
		x2
		(prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2))))
	(E (list
	 (prep:polyrepAddPoly
	  (prep:polyrepNegativePoly (prep:polyrepMultiplyPoly c3 x1))
	  (prep:polyrepAddPoly (prep:polyrepMultiplyPoly c4 x2)
	    (prep:polyrepAddPoly (prep:polyrepNegativePoly c1) c2))) 
	(prep:polyrepAddPoly c1 
	(prep:polyrepAddPoly
	(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly c4 x2))
	(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly x1 x2)))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test20")))

;; deltaI1=c1; deltaR=c2; deltaI2=c3; lambdaR=c4; lambdaI=c5
;; 
;; x1>0; x2>0;x2-x1>0;-x1.c3+x2.c4-c1+c2=0;-x1.x2-x2.c4+c1=0;
(defun test21 ()
  (init-test)
  (let* ((deltaI1 c5)
	(deltaR  c4)
	(deltaI2 c3)
	(lambdaR c2)
	(lambdaI c1)
	(sinR x1)
	(sinI x2)
	(R (list (prep:polyrepAddPoly lambdaR (prep:polyrepNegativePoly lambdaI))
	lambdaI
	lambdaR
	(prep:polyrepAddPoly deltaR (prep:polyrepNegativePoly deltaI2))
	(prep:polyrepAddPoly deltaI1 (prep:polyrepNegativePoly deltaR))
	deltaI2 
	sinI
	sinR 
	(prep:polyrepAddPoly sinI (prep:polyrepNegativePoly sinR))))
	(E (list (prep:polyrepAddPoly
	  (prep:polyrepNegativePoly (prep:polyrepMultiplyPoly sinR lambdaR))
	  (prep:polyrepAddPoly (prep:polyrepMultiplyPoly sinI lambdaI)
	    (prep:polyrepAddPoly (prep:polyrepNegativePoly deltaI1) deltaR)))
	(prep:polyrepAddPoly
	  (prep:polyrepNegativePoly (prep:polyrepMultiplyPoly sinR sinI))
	  (prep:polyrepAddPoly 
		(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly sinI lambdaI))
	    	deltaI1)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test21")))

;; r1 + 5 r2 r2 + 20 r2 = 0 AND r1 > 0 AND r2 > 0
(defun test22 ()
  (init-test)
  (let* ((r1 x1)
	 (r2 x2)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (E (list (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 twentyr2))))
	 (R (list r1 r2)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test22")))

(defun test23()
  (init-test)
  (let* ((r1 x1)
	 (r2 x2)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (E (list (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly 25 r2cube)
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))))
	 (R (list r1 r2)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test23")))

;; This is a non-terminating example?!
;; r1 + 5*r2*r2 - 20*r2 = 0 AND 25*r2^3-100*r2^2+20*r2-1=0 
;; AND r1 > 0 AND r2 > 0 AND gap - r1*v - r2*a = 0 AND gap-4a-4v>0
;; AND -1<a<1 AND 4<v<4 and gap<10
(defun test24()
  (init-test)
  (let* ((r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 (foura (prep:polyrepMultiplyCstPoly -4 a))
	 (fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (E (list (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a))))
	 (R (list r1 r2 
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly v) (list (list 4)))
	  (prep:polyrepAddPoly v (list (list 4)))
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly a) (list (list 1)))
	  (prep:polyrepAddPoly a (list (list 1)))
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly gap) (list (list 10)))
	  (prep:polyrepAddPoly gap (prep:polyrepAddPoly foura fourv)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test24")))

;; r1 + 5*r2*r2 - 20*r2 = 0 AND 25*r2^3-100*r2^2+20*r2-1=0 
;; AND r1 > 0 AND r2 > 0 AND gap - r1*v - r2*a > 0 
;; AND -5.r2.gap + (20.r2-1)v + (20.r2-r1)a > 0
(defun test25()
  (init-test)
  (let* ((r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 ;(foura (prep:polyrepMultiplyCstPoly -4 a))
	 ;(fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (fiver2gap (prep:polyrepMultiplyCstPoly -5 
			(prep:polyrepMultiplyPoly r2 gap)))
	 (twentyr2minus1 (prep:polyrepAddPoly twentyr2 (list (list -1))))
	 (twentyr2minusr1 (prep:polyrepAddPoly twentyr2 (prep:polyrepNegativePoly r1)))
	 (E (list 
	  (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))))
	 (R (list r1  r2 
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a))
	  (prep:polyrepAddPoly fiver2gap
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyPoly twentyr2minus1 v)
		(prep:polyrepMultiplyPoly twentyr2minusr1 a))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test25")))


;; same at test25 but last inequality is replaced by an equation.
(defun test26()
  (init-test)
  (let* ((r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 ;(foura (prep:polyrepMultiplyCstPoly -4 a))
	 ;(fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (fiver2gap (prep:polyrepMultiplyCstPoly -5 
			(prep:polyrepMultiplyPoly r2 gap)))
	 (twentyr2minus1 (prep:polyrepAddPoly twentyr2 (list (list -1))))
	 (twentyr2minusr1 (prep:polyrepAddPoly twentyr2 (prep:polyrepNegativePoly r1)))
	 (E (list 
	  (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))
	  (prep:polyrepAddPoly fiver2gap
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyPoly twentyr2minus1 v)
		(prep:polyrepMultiplyPoly twentyr2minusr1 a)))))
	 (R (list r1  r2  
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test26")))

;; Same as test26 but with swapping the equation and inequation.
(defun test27()
  (init-test)
  (let* ((r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 ;(foura (prep:polyrepMultiplyCstPoly -4 a))
	 ;(fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (fiver2gap (prep:polyrepMultiplyCstPoly -5 
			(prep:polyrepMultiplyPoly r2 gap)))
	 (twentyr2minus1 (prep:polyrepAddPoly twentyr2 (list (list -1))))
	 (twentyr2minusr1 (prep:polyrepAddPoly twentyr2 (prep:polyrepNegativePoly r1)))
	 (E (list
	  (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a))))
	 (R (list r1  r2 
	  (prep:polyrepAddPoly fiver2gap
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyPoly twentyr2minus1 v)
		(prep:polyrepMultiplyPoly twentyr2minusr1 a))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test27")))

;; Same as test25 but with swapping the inequality directions.
;; r1 + 5*r2*r2 - 20*r2 = 0 AND 25*r2^3-100*r2^2+20*r2-1=0 
;; AND r1 > 0 AND r2 > 0 AND gap - r1*v - r2*a < 0 
;; AND -5.r2.gap + (20.r2-1)v + (20.r2-r1)a < 0
(defun test28()
  (init-test)
  (let* ((r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 ;(foura (prep:polyrepMultiplyCstPoly -4 a))
	 ;(fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (fiver2gap (prep:polyrepMultiplyCstPoly -5 
			(prep:polyrepMultiplyPoly r2 gap)))
	 (twentyr2minus1 (prep:polyrepAddPoly twentyr2 (list (list -1))))
	 (twentyr2minusr1 (prep:polyrepAddPoly twentyr2 (prep:polyrepNegativePoly r1)))
	 (E (list
	  (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1)))))))
	 (R (list r1  r2  
	  (prep:polyrepNegativePoly
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a)))
	  (prep:polyrepNegativePoly
	  (prep:polyrepAddPoly fiver2gap
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyPoly twentyr2minus1 v)
		(prep:polyrepMultiplyPoly twentyr2minusr1 a)))))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test28")))

;; gap-r1.v-r2.a >= 0 AND r1,r2>0, v>0, a>0, and gap<=0
(defun test29 ()
  (init-test)
  (let* ((gap x1)
	 (v   x2)
	 (a   x3)
	 (r1  c1)
	 (r2  c2)
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 (R (list
		(prep:polyrepAddPoly gap
		(prep:polyrepAddPoly r1v r2a)) 
		r1  r2  v  a 
	(prep:polyrepNegativePoly gap) )))
    (check (vts:poly-lists-to-constraint-set nil nil R nil) #'vts:is-unsat? "Test29")))


;; gap-r1.v-r2.a >= 0, a>-1, r1, r2>0, r2 < 4, gap=-10, v>0
(defun test30 ()
  (init-test)
  (let* ((gap x1)
	 (v   x2)
	 (a   x3)
	 (r1  c1)
	 (r2  c2)
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 (R (list
		(prep:polyrepAddPoly gap
		(prep:polyrepAddPoly r1v r2a)) 
		(prep:polyrepAddPoly a (list (list 1))) 
		r1  r2 
	(prep:polyrepAddPoly (prep:polyrepNegativePoly r2) (list (list 4))) v ))
	(E (list
	(prep:polyrepAddPoly gap (list (list 10))) )))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test30")))

;; 8l^3+12l^2+7l+1=0 AND r2 + r3 + 1 = 0 AND r3 = 8ll + 12l AND
;; r4 = 8l
;; 4l + 1 > 0 AND -l > 0 AND r4 + 2 > 0 AND -3*r4-4 > 0 AND
;; 2*r3 + 5 > 0 AND -9*r3 - 16 > 0 AND 9*r2 > 7 AND -2*r2 + 3 > 0 
;; v > 0 AND a+2 > 0 AND -a > -5
;; gap + r2*vf + r3*v + r4*a > 0 IFF
;; 1/8 gap r4 + (3/4 r4 + 1) vf - (7/8 r4 + 1)v + (-3/2r4+r3)a < 0
(defun test31 ()
  (init-test)
  (prep:set-variables '(gap v a vf r2 r3 r4 l))
  (prep:set-parameters nil)
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r2  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r2) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (l8 (prep:polyrepMultiplyCstPoly 8 l))
	 ;(lll (prep:polyrepMultiplyPoly ll l))
	 (ll8 (prep:polyrepMultiplyCstPoly 8 ll))
	 (l12 (prep:polyrepMultiplyCstPoly 12 l))
	 (ll8plusl12 (prep:polyrepAddPoly ll8 l12))
	 (lll8plusll12 (prep:polyrepMultiplyPoly ll8plusl12 l))
	 (l7plus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 7 l) (list (list 1))))
	 (e1 (prep:polyrepAddPoly lll8plusll12 l7plus1))
	 (e2 (prep:polyrepAddPoly r2 (prep:polyrepAddPoly r3 (list (list 1)))))
	 (e3 (prep:polyrepAddPoly ll8plusl12 (prep:polyrepNegativePoly r3)))
	 (e4 (prep:polyrepAddPoly r4 (prep:polyrepNegativePoly l8)))
	 ;; 4l + 1 > 0 AND -l > 0 AND r4 + 2 > 0 AND -3*r4-4 > 0 AND
	 ;; 2*r3 + 5 > 0 AND -9*r3 - 16 > 0 AND 9*r2 > 7 AND -2*r2 + 3 > 0 
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 l) (list (list 1))))
	 (p2 (prep:polyrepNegativePoly l))			;; -l 
	 (p3 (prep:polyrepAddPoly r4 (list (list 2))))	;; r4+2
	 (p4 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 r4) (list (list -4))))
	 (p5 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 r3) (list (list 5))))
	 (p6 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -9 r3) (list (list -16))))
	 (p7 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 9 r2) (list (list -7))))
	 (p8 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -2 r2) (list (list 3))))
	 ;; v > 0 AND a+2 > 0 AND -a > -5
	 (p10 (prep:polyrepAddPoly a (list (list 2))))
	 (p11 (prep:polyrepAddPoly (prep:polyrepNegativePoly a) (list (list 5))))
	 ;; gap + r2*vf + r3*v + r4*a > 0 IFF
	 (r2vf (prep:polyrepMultiplyPoly r2 vf))
	 (r3v  (prep:polyrepMultiplyPoly r3 v))
	 (r4a  (prep:polyrepMultiplyPoly r4 a))
	 (p20  (prep:polyrepAddPoly gap r2vf))
	 (p21  (prep:polyrepAddPoly p20 r3v))
	 (p22  (prep:polyrepAddPoly p21 r4a))
	 ;; 1/8 gap r4 + (3/4 r4 + 1) vf - (7/8 r4 + 1)v + (-3/2r4+r3)a < 0
	 (gapr4 (prep:polyrepMultiplyPoly gap r4))
	 (p30 (prep:polyrepMultiplyCstPoly (/ 1 8) gapr4))
	 (r4vf (prep:polyrepMultiplyPoly r4 vf))
	 (p31 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ 3 4) r4vf) vf))
	 (r478plus1 (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly (/ 7 8) r4) (list (list 1))))
	 (p32 (prep:polyrepNegativePoly (prep:polyrepMultiplyPoly r478plus1 v)))
	 (r4by2 (prep:polyrepMultiplyCstPoly (/ -3 2) r4))
	 (p33 (prep:polyrepMultiplyPoly (prep:polyrepAddPoly r4by2 r3) a))
	 (p34 (prep:polyrepAddPoly p30 (prep:polyrepAddPoly p31
		(prep:polyrepAddPoly p32 p33))))
	 (R (list p1 p2 p3 p4 p5 p6 p7 p8 p10 p11 p22 p34))
	 (E (list e1 e2 e3 e4)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test31")))

;; E1: r3 - 8ll - 12l ; r4-8l; lll+1.5ll+7/8l+1/8
;; R1: l+1/4; -l; r4+2; -3r4-4; -r3; vf; v; a+2;
;; Just do a saturation on *paramtypes*, i.e.E1+R1
;; and see how long it takes!
;; See if -v+vf changes R destructively.
;; r3 - 8*l*l - 12*l = 0 AND r4 - 8*l = 0 AND l*l*l + 5*l*l + 7/8*l + 1/8 = 0
;; AND l + 1/4 > 0 AND -l > 0 AND r4+2 > 0 AND -3*r4 - 4 > 0 AND -r3 > 0 
;; AND vf > 0 AND  v > 0 AND  a+2 > 0
(defun test32 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil)
  (let* ((v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 ;(gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (l8 (prep:polyrepMultiplyCstPoly 8 l))
	 ;(lll (prep:polyrepMultiplyPoly ll l))
	 (ll8 (prep:polyrepMultiplyCstPoly 8 ll))
	 (l12 (prep:polyrepMultiplyCstPoly 12 l))
	 (ll8plusl12 (prep:polyrepAddPoly ll8 l12))
	 (lll8plusll12 (prep:polyrepMultiplyPoly ll8plusl12 l))
	 (l7plus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 7 l) (list (list 1))))
	 (e1 (prep:polyrepAddPoly lll8plusll12 l7plus1))
	 (e2 (prep:polyrepAddPoly r3 (prep:polyrepNegativePoly ll8plusl12)))
	 (e3 (prep:polyrepAddPoly r4 (prep:polyrepNegativePoly l8)))
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 l) (list (list 1))))
	 (p2 (prep:polyrepNegativePoly l))
	 (p3 (prep:polyrepAddPoly a (list (list 2))))
	 (p4 (prep:polyrepAddPoly r4 (list (list 2))))
	 (p5 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 r4) (list (list -4))))
	 (E (list e1 e2 e3))
	 (R (list p1 p2 p3 p4 v vf 
  		(prep:polyrepNegativePoly r3) p5
  		(prep:polyrepAddPoly vf (prep:polyrepNegativePoly v)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-sat? "Test32")))
;; last two asnwer should be different...actual test.

;; (= = > > > =) over (*E0* *R0*) gives nil in *R*
;; (g1 g2 g3 g4 g5 g17)
;; g1: -v+vf; g2: adot=gap-12a-7v+6vf; g3: a; g4: gap; g5: gap+10; 
;; g17: eigenvector=agap-r3vf+r3v-vf+r4a
;; -v+vf = 0 AND gap-12a-7v+6vf = 0 AND a > 0 AND gap > 0 AND gap+10 > 0 
;; AND gap - r3*vf + r3*v - vf + r4*a = 0
(defun test33 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (l8 (prep:polyrepMultiplyCstPoly 8 l))
	 ;(lll (prep:polyrepMultiplyPoly ll l))
	 (ll8 (prep:polyrepMultiplyCstPoly 8 ll))
	 (l12 (prep:polyrepMultiplyCstPoly 12 l))
	 (ll8plusl12 (prep:polyrepAddPoly ll8 l12))
	 (lll8plusll12 (prep:polyrepMultiplyPoly ll8plusl12 l))
	 (l7plus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 7 l) (list (list 1))))
	 (e1 (prep:polyrepAddPoly lll8plusll12 l7plus1))
	 (e2 (prep:polyrepAddPoly r3 (prep:polyrepNegativePoly ll8plusl12)))
	 (e3 (prep:polyrepAddPoly r4 (prep:polyrepNegativePoly l8)))
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 l) (list (list 1))))
	 (p2 (prep:polyrepNegativePoly l))
	 (p3 (prep:polyrepAddPoly a (list (list 2))))
	 (p4 (prep:polyrepAddPoly r4 (list (list 2))))
	 (p5 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 r4) (list (list -4))))
  	 ;; -v+vf = 0; gap-12a-7v+6vf = 0; gap-r3.vf+r3.v-vf+r4.a=0
  	 ;; a > 0; gap > 0; gap+10 > 0
	 (ee1 (prep:polyrepAddPoly vf (prep:polyrepNegativePoly v)))
	 (ee2 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -12 a)
		(prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -7 v)
			  	(prep:polyrepMultiplyCstPoly 6 vf)))))
	 (ee3 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly (prep:polyrepMultiplyPoly r3 v)
		(prep:polyrepAddPoly (prep:polyrepMultiplyPoly r4 a)
		(prep:polyrepNegativePoly
			(prep:polyrepAddPoly 
				(prep:polyrepMultiplyPoly r3 vf) vf))))))
	 (pp1 a) (pp2 gap) (pp3 (prep:polyrepAddPoly gap (list (list 10))))
	 (E (list e1 e2 e3))
	 (R (list p1 p2 p3 p4 v vf
  		(prep:polyrepNegativePoly r3) p5
  		(prep:polyrepAddPoly vf (prep:polyrepNegativePoly v))))
	 (ER (list ee3 '= pp3 '> pp2 '> pp1 '> ee2 '= ee1 '=))
	 (E1 (loop for i in E nconc (list i '=)))
	 (R1 (loop for i in R nconc (list i '>)))
	 (E2 (nconc E (list ee3 ee2 ee1)))
	 (R2 (nconc R (list pp2 pp1)))
	 (ER1 (append E1 R1 ER)))
    (check (vts:poly-lists-to-constraint-set E2 nil R2 nil) #'vts:is-unsat? "Test33")))

;; gap - r3*vf + r3*v - vf + r4*a >= 0
;; gap = -10; v-vf > 0; r3 < 0; vf > 0; a > -2; -2 < r4 < 0
(defun test34 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (vfneg (prep:polyrepNegativePoly vf))
	 (r3vfminus (prep:polyrepMultiplyPoly r3 vfneg))
	 (r3v (prep:polyrepMultiplyPoly r3 v))
	 (r4a (prep:polyrepMultiplyPoly r4 a))
	 (rr1 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly r3vfminus
		(prep:polyrepAddPoly r3v
		(prep:polyrepAddPoly vfneg r4a))))) 		;; gap-r3*vf+... 
	 (ee1 (prep:polyrepAddPoly gap (list (list 10))))	;; gap + 10
	 (rr2 (prep:polyrepAddPoly v vfneg))			;; v - vf
	 (rr3 (prep:polyrepNegativePoly r3))			;; -r3
	 (rr4 (prep:polyrepAddPoly a (list (list 2))))	;; a + 2
	 (rr5 (prep:polyrepAddPoly r4 (list (list 2))))	;; r4 - 2
	 (E (list ee1))
	 (R (list rr1 rr2 rr3 vf (prep:polyrepNegativePoly r4) rr4 rr5)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test34")))

;; test34, but within a context.
(defun test35 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (l8 (prep:polyrepMultiplyCstPoly 8 l))
	 ;(lll (prep:polyrepMultiplyPoly ll l))
	 (ll8 (prep:polyrepMultiplyCstPoly 8 ll))
	 (l12 (prep:polyrepMultiplyCstPoly 12 l))
	 (ll8plusl12 (prep:polyrepAddPoly ll8 l12))
	 (lll8plusll12 (prep:polyrepMultiplyPoly ll8plusl12 l))
	 (l7plus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 7 l) (list (list 1))))
	 (e1 (prep:polyrepAddPoly lll8plusll12 l7plus1))			; 8l^3 + 12l^2 + 7 l + 1 
	 (e2 (prep:polyrepAddPoly r3 (prep:polyrepNegativePoly ll8plusl12)))	; r3 - 8l^2 - 12 l
	 (e3 (prep:polyrepAddPoly r4 (prep:polyrepNegativePoly l8)))		; r4 - 8 l
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 l) (list (list 1))))	; 4l + 1
	 (p2 (prep:polyrepNegativePoly l))						; -l
	 (p3 (prep:polyrepAddPoly a (list (list 2))))				; a + 2
	 (p4 (prep:polyrepAddPoly r4 (list (list 2))))				; r4 + 2
	 (p5 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 r4) (list (list -4)))) 	; -3 r4 - 4
	 (E (list e1 e2 e3))
	 (R (list p1 p2 p3 p4 v vf (prep:polyrepNegativePoly r3) p5)))
  	 ;; -v+vf = 0; gap-12a-7v+6vf = 0; gap-r3.vf+r3.v-vf+r4.a=0
  	 ;; a > 0; gap > 0; gap+10 > 0
  (let* ((vfneg (prep:polyrepNegativePoly vf))
	 (r3vfminus (prep:polyrepMultiplyPoly r3 vfneg))
	 (r3v (prep:polyrepMultiplyPoly r3 v))
	 (r4a (prep:polyrepMultiplyPoly r4 a))
	 (rr1 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly r3vfminus
		(prep:polyrepAddPoly r3v
		(prep:polyrepAddPoly vfneg r4a))))) 		;; gap-r3*vf+... 
	 (ee1 (prep:polyrepAddPoly gap (list (list 10))))	;; gap + 10
	 (rr2 (prep:polyrepAddPoly v vfneg))			;; v - vf
	 (ER0 (list ee1 '= rr1 '>))
	 (E1 (loop for i in E nconc (list i '=)))
	 (R1 (loop for i in R nconc (list i '>)))
	 (ER1 (append E1 R1 ER0))
	 (ER2 (append ER1 (list rr2 '>))))
    ; (check (sos (cons ee1 E) (cons rr1 R)) 1 "Test35")
    (check (vts:poly-lists-to-constraint-set (cons ee1 E) nil (cons rr2 (cons rr1 R)) nil) #'vts:is-unsat? "Test35.1"))))

;; g16=pos; g4=zero; g3=neg; g2=pos; g1=zero; g0=neg;
;; g16 --> ((((gap(this) - r3 * vf) + r3 * v(this)) - vf) + r4 * a(this))
;; g0 --> (-)(1) * v(this) + 1 * vf
;; g1 --> 1 * gap(this) + (-)(12) * a(this) + (-)(7) * v(this) + 6 * vf
;; g2 --> a(this)
;; g3 --> gap(this)
;; g4 --> (gap(this) + 10) - 0
(defun test36()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (l8 (prep:polyrepMultiplyCstPoly 8 l))
	 ;(lll (prep:polyrepMultiplyPoly ll l))
	 (ll8 (prep:polyrepMultiplyCstPoly 8 ll))
	 (l12 (prep:polyrepMultiplyCstPoly 12 l))
	 (ll8plusl12 (prep:polyrepAddPoly ll8 l12))
	 (lll8plusll12 (prep:polyrepMultiplyPoly ll8plusl12 l))
	 (l7plus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 7 l) (list (list 1))))
	 (e1 (prep:polyrepAddPoly lll8plusll12 l7plus1))
	 (e2 (prep:polyrepAddPoly r3 (prep:polyrepNegativePoly ll8plusl12)))
	 (e3 (prep:polyrepAddPoly r4 (prep:polyrepNegativePoly l8)))
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 l) (list (list 1))))
	 (p2 (prep:polyrepNegativePoly l))
	 (p3 (prep:polyrepAddPoly a (list (list 2))))
	 (p4 (prep:polyrepAddPoly r4 (list (list 2))))
	 (p5 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 r4) (list (list -4))))
	 (E0 (list e1 e2 e3))
	 (R0 (list  p1 p2 p3 p4 v vf (prep:polyrepNegativePoly r3) p5)))
  	 ;; -v+vf = 0; gap-12a-7v+6vf = 0; gap-r3.vf+r3.v-vf+r4.a=0
  	 ;; a > 0; gap > 0; gap+10 > 0
;; g0 --> (-)(1) * v(this) + 1 * vf
;; g1 --> 1 * gap(this) + (-)(12) * a(this) + (-)(7) * v(this) + 6 * vf
;; g2 --> a(this)
;; g3 --> gap(this)
;; g4 --> (gap(this) + 10) - 0
;; g16 --> ((((gap(this) - r3 * vf) + r3 * v(this)) - vf) + r4 * a(this))
  (let* ((vfneg (prep:polyrepNegativePoly vf))
	 (r3vfminus (prep:polyrepMultiplyPoly r3 vfneg))
	 (r3v (prep:polyrepMultiplyPoly r3 v))
	 (r4a (prep:polyrepMultiplyPoly r4 a))
	 (g16 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly r3vfminus
		(prep:polyrepAddPoly r3v
		(prep:polyrepAddPoly vfneg r4a))))) 		;; gap-r3*vf+... 
	 (g4 (prep:polyrepAddPoly gap (list (list 10))))	;; gap + 10
	 (g0 (prep:polyrepAddPoly v vfneg))			;; v - vf
	 (a12n (prep:polyrepMultiplyCstPoly -12 a))
	 (v7n (prep:polyrepMultiplyCstPoly -7 v))
	 (vf6 (prep:polyrepMultiplyCstPoly 6 vf))
	 (g1 (prep:polyrepAddPoly gap
		(prep:polyrepAddPoly a12n
		(prep:polyrepAddPoly v7n vf6))))
	 (g2 a)
	 (g3 gap)
	 (ER0 (list g16 '> g4 '= 
  		(prep:polyrepNegativePoly g3) '>
  		g2 '> g1 '= g0 '>))
	 (E1 (loop for i in E0 nconc (list i '=)))
	 (R1 (loop for i in R0 nconc (list i '>)))
	 (E2 (nconc E0 (list g4 g1))) 
	 (R2 (nconc R0 (list g16  (prep:polyrepNegativePoly g3) g2  g0 )))
	 (ER1 (append E1 R1 ER0)))
;; g16=pos; g4=zero; g3=neg; g2=pos; g1=zero; g0=neg;
    (check (vts:poly-lists-to-constraint-set E2 nil R2 nil) #'vts:is-unsat? "Test36"))))
 
;; Test24: satisfiable + a trivial inconsistency
(defun test37()
  (init-test)
  (let* ((SigmaH x4)
	 (plus-half (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 SigmaH) '((1/2))))
	 (plus-one  (prep:polyrepAddPoly SigmaH '((-1))))
  	 (r1 c1)
	 (r2 c2)
	 (gap x1)
	 (v x2)
	 (a x3)
	 (r2r2 (prep:polyrepMultiplyPoly r2 r2))
	 (fiver2r2 (prep:polyrepMultiplyCstPoly 5 r2r2))
	 (twentyr2 (prep:polyrepMultiplyCstPoly 20 r2))
	 (r2cube (prep:polyrepMultiplyPoly r2r2 r2))
	 (twentyfiver2cube (prep:polyrepMultiplyCstPoly 25 r2cube))
	 (r1v (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r1) v))
	 (r2a (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly r2) a))
	 (foura (prep:polyrepMultiplyCstPoly -4 a))
	 (fourv (prep:polyrepMultiplyCstPoly -4 v))
	 (E (list (prep:polyrepAddPoly r1 (prep:polyrepAddPoly fiver2r2 
		(prep:polyrepNegativePoly twentyr2)))  
	  (prep:polyrepAddPoly twentyfiver2cube
	  (prep:polyrepAddPoly 
		(prep:polyrepMultiplyCstPoly -100 r2r2)
	   (prep:polyrepAddPoly twentyr2 (list (list -1))))) 
	  (prep:polyrepAddPoly gap
	  (prep:polyrepAddPoly r1v r2a)) ))
	 (R (list
	r1  r2 
	  SigmaH 
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly v) (list (list 4))) 
	  (prep:polyrepAddPoly v (list (list 4))) 
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly a) (list (list 1))) 
	  plus-one 
	  (prep:polyrepAddPoly a (list (list 1))) 
	  (prep:polyrepAddPoly (prep:polyrepNegativePoly gap) (list (list 10))) 
	  (prep:polyrepAddPoly gap (prep:polyrepAddPoly foura fourv)) 
	  plus-half )))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test37")))

;; Copied from Test15: pzzzz and nzzzz:  x-y,y,x,y+x,-y-2x
(defun test38 ()
  (init-test)
  (let ((E (list (prep:polyrepAddPoly x1 x2) 
		(prep:polyrepAddPoly (prep:polyrepNegativePoly x1) 
			(prep:polyrepAddPoly (prep:polyrepNegativePoly x2)
					(prep:polyrepNegativePoly x2)))))
	(R (list (prep:polyrepAddPoly x1 (prep:polyrepNegativePoly x2)))))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test38")))

;; -x2 - x1 - x1 > 0 AND x2 + x1 > 0 AND x2 > 0 AND x1 > 0
;;(defun test38()
  ;;(prep:set-variables '(Spo0FP Spo0BP Spo0A Spo0AP RapA Vba Vbap Spo0E v7 SigmaH))
  ;;(prep:set-parameters '())
  ;;(let* ((SigmaH (list (cons 1 (list (cons (mk-sal-nameexpr nil 'SigmaH) 1)))))
	 ;;(Spo0BP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Spo0BP) 1)))))
	 ;;(Spo0FP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Spo0FP) 1)))))
	 ;;(Spo0A  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Spo0A) 1)))))
	 ;;(RapA   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'RapA) 1)))))
	 ;;(Vba  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vba) 1)))))
	 ;;(Vbap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vbap) 1)))))
	 ;;(v7   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v7) 1)))))
	 ;;(plus-half (prep:polyrepAddPoly SigmaH '((-1/2))))
	 ;;(plus-one  (prep:polyrepAddPoly SigmaH '((-1))))
	 ;;(g26 (prep:polyrepAddPoly Spo0AP '((-1/3))))
	 ;;(g25 (prep:polyrepAddPoly Spo0AP '((-2/3))))
	 ;;(g24 (prep:polyrepAddPoly RapA '((-1/2))))
	 ;;(g23 (prep:polyrepAddPoly Spo0A '((-1/4))))
	 ;;(g22 (prep:polyrepAddPoly Spo0BP '((-1/2))))
	 ;;(g21 (prep:polyrepAddPoly Spo0FP '((-1/2))))
	 ;;(g20 (prep:polyrepAddPoly Spo0E '((-1/2))))
	 ;;(g19 Spo0E)
	 ;;(g18 RapA)
	 ;;(g17 Spo0FP)
	 ;;(g16 Spo0BP)
	 ;;(g15 Spo0AP)
	 ;;(g14 Spo0A))))
;;%% g12 --> ((-1 (Spo0BP . 1) (Spo0A . 1) (Vba . 1)) (-1 (Spo0BP . 1) (Spo0AP . 1) (Vbap . 1)) (-1 (Spo0A . 1)) (1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (1 (Spo0AP . 1) (Vbap . 1)) (2/3))
;;%% g11 --> ((-1 (Spo0BP . 1) (Spo0A . 1) (Vba . 1)) (-1 (Spo0BP . 1) (Spo0AP . 1) (Vbap . 1)) (-1 (Spo0A . 1)) (1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (1 (Spo0AP . 1) (Vbap . 1)) (1/3))
;;%% g10 --> ((-1 (Spo0BP . 1) (Spo0A . 1) (Vba . 1)) (-1 (Spo0BP . 1) (Spo0AP . 1) (Vbap . 1)) (-1 (Spo0A . 1)) (1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (1 (Spo0AP . 1) (Vbap . 1)))
;;%% g9 --> ((1 (Spo0BP . 1) (Spo0A . 1) (Vba . 1)) (1 (Spo0BP . 1) (Spo0AP . 1) (Vbap . 1)) (-1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (-1 (Spo0AP . 1) (Vbap . 1)))
;;%% g8 --> ((-1 (Spo0FP . 1) (Spo0BP . 1) (Vbm . 1)) (1 (Spo0FP . 1) (Spo0BP . 1) (Vbmp . 1)) (1 (Spo0FP . 1) (Vbm . 1)) (-1 (Spo0BP . 1) (Spo0A . 1) (Vba . 1)) (-1 (Spo0BP . 1) (Spo0AP . 1) (Vbap . 1)) (-1 (Spo0BP . 1) (Vbmp . 1)) (1 (Spo0AP . 1) (Vbap . 1)))
;;%% g7 --> ((1 (Spo0FP . 1) (Spo0BP . 1) (Vbm . 1)) (-1 (Spo0FP . 1) (Spo0BP . 1) (Vbmp . 1)) (-1 (Spo0FP . 1) (RapA . 1) (d . 1)) (-1 (Spo0FP . 1) (b . 1)) (-1 (Spo0FP . 1) (Vbm . 1)) (1 (Spo0BP . 1) (Vbmp . 1)) (1 (b . 1)))
;;%% g6 --> ((1 (Spo0FP . 1) (Spo0BP . 1) (Vbm . 1)) (-1 (Spo0FP . 1) (Spo0BP . 1) (Vbmp . 1)) (-1 (Spo0FP . 1) (RapA . 1) (d . 1)) (-1 (Spo0FP . 1) (Vbm . 1)) (1 (Spo0BP . 1) (Vbmp . 1)))
;;%% g5 --> ((-1 (RapA . 1) (k12 . 1)) (-1 (RapA . 1)) (1))
;;%% g4 --> ((-1 (RapA . 1)) (1))
;;%% g3 --> ((-1 (RapA . 1) (k12 . 1)) (-1 (RapA . 1)))
;;%% g2 --> ((-1 (RapA . 1) (k12 . 1)) (-1 (RapA . 1)) (1/2))
;;%% g1 --> ((-1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (-1 (Spo0E . 1)))
;;%% g0 --> ((-1 (Spo0AP . 1) (Spo0E . 1) (v7 . 1)) (-1 (Spo0E . 1)) (1))
;;
;; Vba.Spo0BP.Spo0A - Vbap.(1-Spo0BP).Spo0AP - v7.Spo0E.Spo0AP < 0
;; Spo0BP >= 1/2; Spo0A >= 1/4; Spo0AP = 2/3; Spo0E <= 1/2; Vba > 2*v7 + 4*Vbap; everything is >= 0
;; (defun test39 ()
;;   (init-test)
;;   (let* ((Spo0AP x1)
;; 	 (Spo0BP x2)
;; 	 (Spo0A x3)
;; 	 (Spo0E x4)
;; 	 (v7 c2)
;; 	 (Vba c1)
;; 	 (Vbap c3)
;; 	 (E (list (prep:polyrepAddPoly Spo0AP '((-2/3)))))
;; 	 (R (list (prep:polyrepAddPoly Spo0BP '((-1/2))) 
;; 		  (prep:polyrepAddPoly Spo0A '((-1/4))) 
;; 		  (prep:polyrepAddPoly '((1/2)) (prep:polyrepNegativePoly Spo0E))
;; 		  v7 Vba Vbap
;; 		  (prep:polyrepAddPoly Vba (prep:polyrepNegativePoly
;; 		    (prep:polyrepAddPoly (prep:polyrepMultiplyPoly '((8/3)) v7) (prep:polyrepMultiplyPoly '((8/3)) Vbap))))))
;; 	 (S (list x1 x2 x3 x4))
;; 	 (ER (list (prep:polyrepAddPoly 
;; 			(prep:polyrepMultiplyPoly v7 (prep:polyrepMultiplyPoly Spo0E Spo0AP))
;; 		    (prep:polyrepAddPoly
;; 			(prep:polyrepMultiplyPoly Vbap Spo0AP)
;; 		    (prep:polyrepAddPoly (prep:polyrepNegativePoly
;; 			(prep:polyrepMultiplyPoly Vbap (prep:polyrepMultiplyPoly Spo0BP Spo0AP)))
;; 			(prep:polyrepNegativePoly (prep:polyrepMultiplyPoly Vba (prep:polyrepMultiplyPoly Spo0BP Spo0A))))))
;; 		  gb::'>)))
;;     (check (vts:poly-lists-to-constraint-set E nil (cons (car ER) R) S) #'vts:is-unsat? "Test39")))


(defun test40 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (lll (prep:polyrepMultiplyPoly ll l))
	 (ll3 (prep:polyrepMultiplyCstPoly 3 ll))
	 (l4  (prep:polyrepMultiplyCstPoly 4 l))
	 (e1 (prep:polyrepAddPoly lll (prep:polyrepAddPoly ll3 (prep:polyrepAddPoly l4 '((1)))))) 	;; l*l*l + 3*l*l + 4*l + 1 = 0 AND 
	 (e2 (prep:polyrepAddPoly ll (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 3 l) (prep:polyrepNegativePoly r3))));; r3 = l*l + 3*l AND
	 (e3 (prep:polyrepAddPoly l (prep:polyrepNegativePoly r4))) 						;; r4 = l AND
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 l) (list (list 1)))) 		;; 0 < 2*l + 1
	 (p2 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -4 l) (list (list -1)))) 		;; 4*l + 1  < 0 AND
	 (p3 vf) 									;; vf > 0 AND 
	 (p4 v)										;; v > 0 AND 
	 (p5 (prep:polyrepAddPoly a '((2))))							;; 0 < a+2
	 (E (list e1 e2 e3))
	 (R (list p1 p2 p3 p4 p5)))
  (let* ((vneg (prep:polyrepNegativePoly v))
	 (vfneg (prep:polyrepNegativePoly vf))
	 (pp1 (prep:polyrepAddPoly vf vneg))			;; - v + vf
	 (pp2 (prep:polyrepAddPoly gap (list (list 10))))	;; gap + 10
	 (r3vfminus (prep:polyrepMultiplyPoly r3 vfneg))
	 (r3v (prep:polyrepMultiplyPoly r3 v))
	 (r4a (prep:polyrepMultiplyPoly r4 a))
	 (pp3 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly r3vfminus
		(prep:polyrepAddPoly r3v
		(prep:polyrepAddPoly vfneg r4a))))) 	;; gap - r3*vf + r3*v - vf + r4*a
	 (e (nconc E (list pp2)))
	 (r (nconc R (list (prep:polyrepNegativePoly pp1) pp3 a))))
    (check (vts:poly-lists-to-constraint-set e nil r nil) #'vts:is-unsat? "Test40"))))

(defun test41 ()
  (init-test)
  (prep:set-variables '(gap v a vf r3 r4 l))
  (prep:set-parameters nil )
  (let* ((gap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'gap) 1)))))
	 (v   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v) 1)))))
	 (a   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'a) 1)))))
	 (vf  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'vf) 1)))))
	 (r3  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r3) 1)))))
	 (r4  (list (cons 1 (list (cons (mk-sal-nameexpr nil 'r4) 1)))))
	 (l   (list (cons 1 (list (cons (mk-sal-nameexpr nil 'l) 1)))))
	 (ll (prep:polyrepMultiplyPoly l l))
	 (lll (prep:polyrepMultiplyPoly ll l))
	 (ll3 (prep:polyrepMultiplyCstPoly 3 ll))
	 (l4  (prep:polyrepMultiplyCstPoly 4 l))
	 (e1 (prep:polyrepAddPoly lll (prep:polyrepAddPoly ll3 (prep:polyrepAddPoly l4 '((1)))))) 	;; l*l*l + 3*l*l + 4*l + 1 = 0 AND 
	 (e2 (prep:polyrepAddPoly ll (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 3 l) (prep:polyrepNegativePoly r3))));; r3 = l*l + 3*l AND
	 (e3 (prep:polyrepAddPoly l (prep:polyrepNegativePoly r4))) 						;; r4 = l AND
	 (p1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 l) (list (list 1)))) 		;; 0 < 2*l + 1
	 (p2 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -4 l) (list (list -1)))) 		;; 4*l + 1  < 0 AND
	 (p3 vf) 									;; vf > 0 AND 
	 (p4 v)										;; v > 0 AND 
	 (p5 (prep:polyrepAddPoly a '((2))))							;; 0 < a+2
	 (E (list e1 e2 e3))
	 (R (list p1 p2 p3 p4 p5)))
  (let* ((vneg (prep:polyrepNegativePoly v))
	 (vfneg (prep:polyrepNegativePoly vf))
	 (pp1 (prep:polyrepAddPoly vf vneg))			;; - v + vf
	 (pp2 (prep:polyrepAddPoly gap (list (list 10))))	;; gap + 10
	 (r3vfminus (prep:polyrepMultiplyPoly r3 vfneg))
	 (r3v (prep:polyrepMultiplyPoly r3 v))
	 (r4a (prep:polyrepMultiplyPoly r4 a))
	 (pp3 (prep:polyrepAddPoly gap 
		(prep:polyrepAddPoly r3vfminus
		(prep:polyrepAddPoly r3v
		(prep:polyrepAddPoly vfneg r4a))))) 	;; gap - r3*vf + r3*v - vf + r4*a
	 (e (nconc E (list pp2)))
	 (r (nconc R (list (prep:polyrepNegativePoly pp1) pp3))))
    (check (vts:poly-lists-to-constraint-set e nil r nil) #'vts:is-unsat? "Test41"))))

;; 1/2 < z < 1; 1/3 < x < 1/2; 1 < y; -xy -xz +x > 0;
(defun test42 ()
  (init-test)
  (prep:set-variables '(x y z))
  (let* ((x (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x) 1)))))
	 (y (list (cons 1 (list (cons (mk-sal-nameexpr nil 'y) 1)))))
	 (z (list (cons 1 (list (cons (mk-sal-nameexpr nil 'z) 1)))))
	 (x3minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 3 x) (list (list -1)))) 		;; 0 < 2*l + 1
	 (z2minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 z) (list (list -1)))) 		;; 0 < 2*l + 1
	 (yminus1 (prep:polyrepAddPoly y (list (list -1))))
	 (oneminus2x (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -2 x) (list (list 1)))) 		;; 0 < 2*l + 1
	 (oneminusz (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 z) (list (list 1)))) 		;; 0 < 2*l + 1
	 (ypluszminus1 (prep:polyrepAddPoly y (prep:polyrepAddPoly z (list (list -1)))))
	 (xtimesypluszminus1 (prep:polyrepMultiplyPoly (prep:polyrepNegativePoly x) ypluszminus1))
	 (R0 (list x3minus1 z2minus1 yminus1 oneminus2x oneminusz xtimesypluszminus1)))
    (check (vts:poly-lists-to-constraint-set nil nil R0 nil) #'vts:is-unsat? "Test42")))

;; 1/4 > y; 0 < x < 1/2; 1/2 < z < 1; y + x(1-z) > 0; (UNIT-TEST for BOUNDs inference rule)
(defun test43 ()
  (init-test)
  (prep:set-variables '(x y z))
  (let* ((x (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x) 1)))))
	 (y (list (cons 1 (list (cons (mk-sal-nameexpr nil 'y) 1)))))
	 (z (list (cons 1 (list (cons (mk-sal-nameexpr nil 'z) 1)))))
	 (y4minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 y) (list (list -1))))
	 (oneminus2x (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -2 x) (list (list 1)))) 		;; 0 < 2*l + 1
	 (oneminusz (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 z) (list (list 1)))) 		;; 0 < 2*l + 1
	 (z2minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 z) (list (list -1)))) 		;; 0 < 2*l + 1
	 (xtimes1minusz (prep:polyrepMultiplyPoly x oneminusz))
	 (minusyplusxtimes1minusz (prep:polyrepAddPoly (prep:polyrepNegativePoly y) xtimes1minusz))
	 (R0 (list y4minus1 x oneminus2x z2minus1 oneminusz minusyplusxtimes1minusz)))
    (check (vts:poly-lists-to-constraint-set nil nil R0 nil) #'vts:is-unsat? "Test43")))

;; c > 0; 1/4 < y < 1; 0 < x < 1/2; 1/2 < z < 1; -cy + cx(1-z) > 0; (UNIT-TEST for BOUNDs inference rule)
(defun test44 ()
  (init-test)
  (prep:set-variables '(x y z c d))
  (prep:set-parameters nil )
  (let* ((x (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x) 1)))))
	 (y (list (cons 1 (list (cons (mk-sal-nameexpr nil 'y) 1)))))
	 (z (list (cons 1 (list (cons (mk-sal-nameexpr nil 'z) 1)))))
	 (c (list (cons 1 (list (cons (mk-sal-nameexpr nil 'c) 1)))))
	 (y4minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 4 y) (list (list -1))))
	 (oneminusy (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 y) (list (list 1))))
	 (oneminus2x (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -2 x) (list (list 1)))) 		;; 0 < 2*l + 1
	 (oneminusz (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 z) (list (list 1)))) 		;; 0 < 2*l + 1
	 (z2minus1 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 z) (list (list -1)))) 		;; 0 < 2*l + 1
	 (xtimes1minusz (prep:polyrepMultiplyPoly x oneminusz))
	 (minusyplusxtimes1minusz (prep:polyrepAddPoly (prep:polyrepNegativePoly y) xtimes1minusz))
	 (minuscyplusxtimes1minusz (prep:polyrepMultiplyPoly c minusyplusxtimes1minusz))
	 (R0 (list y4minus1 x oneminusy c oneminus2x z2minus1 oneminusz minuscyplusxtimes1minusz)))
    (check (vts:poly-lists-to-constraint-set nil nil R0 nil) #'vts:is-unsat? "Test44")))

;; v7>0, Vba>0, Vbap>0, 3 - 6*Vba + 12*v7 + 8*Vbap > 0,
;; 0 < BP < 2/3; 1 > OAP > 2/3; ==> OAdot =  1 - 3*Vba*BP + 6*v7*AP + 12*Vbap*(1-BP)*AP > 0
(defun test45 ()
  (init-test)
  (prep:set-variables '(BP AP OA Vba v7 Vbap))
  (prep:set-parameters nil )
  (let* ((BP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'BP) 1)))))
	 (AP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'AP) 1)))))
	 (OA (list (cons 1 (list (cons (mk-sal-nameexpr nil 'OA) 1)))))
	 (Vba (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vba) 1)))))
	 (v7 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v7) 1)))))
	 (Vbap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vbap) 1)))))
	 (Vbap8 (prep:polyrepMultiplyCstPoly 8 Vbap))
	 (v712 (prep:polyrepMultiplyCstPoly 12 v7))
	 (minusVba6 (prep:polyrepMultiplyCstPoly -6 Vba))
	 (constraint (prep:polyrepAddPoly '((3)) (prep:polyrepAddPoly minusVba6 (prep:polyrepAddPoly v712 Vbap8))))
	 (BP3minus2 (prep:polyrepAddPoly '((2)) (prep:polyrepMultiplyCstPoly -3 BP )))
	 (AP3minus2 (prep:polyrepAddPoly '((-2)) (prep:polyrepMultiplyCstPoly 3 AP )))
	 (oneminusAP (prep:polyrepAddPoly '((1)) (prep:polyrepMultiplyCstPoly -1 AP )))
	 (VbaBP3 (prep:polyrepMultiplyCstPoly 3 (prep:polyrepMultiplyPoly Vba BP)))
	 (v7AP6 (prep:polyrepMultiplyCstPoly -6 (prep:polyrepMultiplyPoly v7 AP)))
	 (VbapAP12 (prep:polyrepMultiplyCstPoly -12 (prep:polyrepMultiplyPoly Vbap AP)))
	 (VbapAP121minusBP (prep:polyrepMultiplyPoly (prep:polyrepAddPoly '((1)) (prep:polyrepNegativePoly BP)) VbapAP12))
	 (OAdot (prep:polyrepAddPoly VbapAP121minusBP (prep:polyrepAddPoly v7AP6 (prep:polyrepAddPoly VbaBP3 '((-1))))))
	 (R0 (list v7 Vba Vbap constraint BP BP3minus2 AP3minus2 oneminusAP OAdot)))
    (check (vts:poly-lists-to-constraint-set nil nil R0 nil) #'vts:is-unsat? "Test45")))

;E=(Spo0E - 1/2)
;R=(	Vba - 3*v7 - 4*Vbap + 1;;  Vba - 4*v7 - 16/3*Vbap;;  Vba;;  Vba - 2*Vbap -1*Vba + 2*v7 + 4/3*Vbap + 1/2 Vbap v7 - 1/3*Vbap + 1/4 -1*v7 - 2*Vbap + 1/4 Vbm - 1/2*Vba - 1/2*Vbmp + 4/3*Vbap -1*Vbm + 1/2*Vba - Vbap -1*Vbmp + 2/3*Vbap v7 -1*Vbap + 1/8 Vba - 3*v7 - 4*Vbap + 1
;S=(Spo0BP Spo0AP -1*Spo0A + 1 Spo0A -1*Spo0AP + 1 -1*Spo0BP + 1)
;BP < 2/3 AND AP > 2/3 AND OA=1/4 => OAdot > 0
(defun test46 ()
  (init-test)
  (prep:set-variables '(BP AP OA Vbm Vba Vbmp v7 Vbap))
  (prep:set-parameters nil )
  (let* ((BP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'BP) 1)))))
	 (AP (list (cons 1 (list (cons (mk-sal-nameexpr nil 'AP) 1)))))
	 (OA (list (cons 1 (list (cons (mk-sal-nameexpr nil 'OA) 1)))))
	 (Vba (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vba) 1)))))
	 (v7 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'v7) 1)))))
	 (Vbap (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vbap) 1)))))
	 (Vbm (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vbm) 1)))))
	 (Vbmp (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Vbmp) 1)))))
	 (oneminusAP (prep:polyrepAddPoly '((1)) (prep:polyrepMultiplyCstPoly -1 AP )))
	 (oneminusOA (prep:polyrepAddPoly '((1)) (prep:polyrepMultiplyCstPoly -1 OA )))
	 (oneminusBP (prep:polyrepAddPoly '((1)) (prep:polyrepMultiplyCstPoly -1 BP )))
	 (OA4minus1 (prep:polyrepAddPoly '((-1)) (prep:polyrepMultiplyCstPoly 4 OA )))

	 (r1 (prep:polyrepAddPoly Vba (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 v7) 
				 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -4 Vbap) '((1))))))
	 (r2 (prep:polyrepAddPoly Vba (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -4 v7) 
				 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ -16 3) Vbap) nil))))
	 (r3 Vba)
	 (r4 (prep:polyrepAddPoly Vba (prep:polyrepMultiplyCstPoly -2 Vbap))) 
	 (r5 (prep:polyrepAddPoly (prep:polyrepNegativePoly Vba) (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly 2 v7) 
				 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ 4 3) Vbap) (list (list (/ 1 2)))))))
	 (r6 Vbap)
	 (r7 (prep:polyrepAddPoly v7 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ -1 3) Vbap) (list (list (/ 1 4))))))
	 (r8 (prep:polyrepAddPoly (prep:polyrepNegativePoly v7) (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -2 Vbap) (list (list (/ 1 4))))))
	 (r9 (prep:polyrepAddPoly Vbm (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ -1 2) Vba)
				 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ -1 2) Vbmp)
						 (prep:polyrepMultiplyCstPoly (/ 4 3) Vbap)))))
	 (r10 (prep:polyrepAddPoly (prep:polyrepNegativePoly Vbm) (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly (/ 1 2) Vba) 
				 (prep:polyrepMultiplyCstPoly -1 Vbap))))
	 (r11 (prep:polyrepAddPoly (prep:polyrepNegativePoly Vbmp) (prep:polyrepMultiplyCstPoly (/ 2 3) Vbap))) 
	 (r12 v7)
	 (r13 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -1 Vbap) (list (list (/ 1 8)))))
	 (r14 (prep:polyrepAddPoly Vba (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -3 v7) 
				 (prep:polyrepAddPoly (prep:polyrepMultiplyCstPoly -4 Vbap) '((1))))))
	 (BP3minus2 (prep:polyrepAddPoly '((2)) (prep:polyrepMultiplyCstPoly -3 BP )))
	 (AP3minus2 (prep:polyrepAddPoly '((-2)) (prep:polyrepMultiplyCstPoly 3 AP )))
	 (VbaBP3 (prep:polyrepMultiplyCstPoly 3 (prep:polyrepMultiplyPoly Vba BP)))
	 (v7AP6 (prep:polyrepMultiplyCstPoly -6 (prep:polyrepMultiplyPoly v7 AP)))
	 (VbapAP12 (prep:polyrepMultiplyCstPoly -12 (prep:polyrepMultiplyPoly Vbap AP)))
	 (VbapAP121minusBP (prep:polyrepMultiplyPoly (prep:polyrepAddPoly '((1)) (prep:polyrepNegativePoly BP)) VbapAP12))
	 (OAdot (prep:polyrepAddPoly VbapAP121minusBP (prep:polyrepAddPoly v7AP6 (prep:polyrepAddPoly VbaBP3 '((-1))))))
	 (E0 nil)
	 (R0 (list r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14))
	 (S0 (list BP AP oneminusOA OA oneminusAP oneminusBP))
	 (E1 (list OA4minus1))
	 (R1 (list BP3minus2 AP3minus2 OAdot)))
    (check (vts:poly-lists-to-constraint-set E1 nil (nconc R1 R0) S0) #'vts:is-unsat? "Test46")))

(defun test47 ()
  (init-test)
  (let* ((x13 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x1) 3)))))
  	 (x13minusx1 (prep:polyrepAddPoly x13 (prep:polyrepNegativePoly x1)))
  	 (x1x2 (prep:polyrepMultiplyPoly x1 x2))
  	 (x1x2minus1 (prep:polyrepAddPoly x1x2 (toPolyRepCnst -1)))
	 (minus2x22 (list (cons -2 (list (cons (mk-sal-nameexpr nil 'x2) 2)))))
  	 (minus2x22plus1 (prep:polyrepAddPoly minus2x22 (toPolyRepCnst 1)))
	 (E (list x13minusx1))
	 (R (list x1x2minus1 minus2x22plus1)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unsat? "Test47")))

(defun test48 ()
  (init-test)
  (let* ((x13 (list (cons 1 (list (cons (mk-sal-nameexpr nil 'x1) 3)))))
  	 (x13minusx1 (prep:polyrepAddPoly x13 (prep:polyrepNegativePoly x1)))
  	 (x1x2 (prep:polyrepMultiplyPoly x1 x2))
  	 (x1x2minus1 (prep:polyrepAddPoly x1x2 (toPolyRepCnst -1)))
	 (minus2x22 (list (cons -2 (list (cons (mk-sal-nameexpr nil 'x2) 2)))))
  	 (minus2x22plus1 (prep:polyrepAddPoly minus2x22 (toPolyRepCnst 1)))
	 (E (list x13minusx1))
	 (R (list x1x2minus1 minus2x22plus1)))
    (check (vts:poly-lists-to-constraint-set nil nil R0 nil) #'vts:is-unsat? "Test48")))

;; ;; proof obligations resulting from the model-simplification....
;; ;; E = Tc*TetA - 6/7; TetA-20/11; Tc*TetR-12/10000*TetRTc; Tc*TetR-15/40000*TetRTc-3/2000
;; (defun test49 ()
;;   (init-test)
;;   (prep:set-variables '(Tc TetA TetRTc TetR))
;;   (prep:set-parameters '())
;;   (let* ((Tc (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Tc) 1)))))
;; 	 (TetA (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetA) 1)))))
;; 	 (TetRTc (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetRTc) 1)))))
;; 	 (TetR (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetR) 1)))))
;; 	 (tcta (prep:polyrepMultiplyPoly Tc TetA)) 
;; 	 (tctr (prep:polyrepMultiplyPoly Tc TetR)) 
;; 	 (tctaminus6by7 (prep:polyrepAddPoly tcta (toPolyRepCnst (/ -6 7))))
;; 	 (ta20by11 (prep:polyrepAddPoly TetA (toPolyRepCnst (/ -20 11))))
;; 	 (tctrminus12by10000rc (prep:polyrepAddPoly tctr 
;; 		(prep:polyrepMultiplyPoly (toPolyRepCnst (/ -12 10000)) TetRTc)))
;; 	 (tctrminus15by40000rc (prep:polyrepAddPoly tctr 
;; 		(prep:polyrepMultiplyPoly (toPolyRepCnst (/ -15 40000)) TetRTc)))
;; 	 (tctrminus15by40000rcminus3by2000 (prep:polyrepAddPoly tctrminus15by40000rc 
;; 		(toPolyRepCnst (/ -3 2000))))
;; 	 (E (list tctaminus6by7 ta20by11 tctrminus12by10000rc tctrminus15by40000rcminus3by2000))
;; 	 (r1 (prep:polyrepAddPoly TetR (prep:polyrepNegativePoly TetA)))
;; 	 (r2 (prep:polyrepAddPoly TetRTc (toPolyRepCnst (/ -12000 5))))
;; 	 (r3 (prep:polyrepAddPoly TetR (toPolyRepCnst (/ -2 11)))))
;;     (check (sos (copy-list E) (list r1)) -1 "Test49")
;;     (check (sos (copy-list E) (list r2)) -1 "Test49")
;;     (check (sos E (list r3)) -1 "Test49")))

;; ;; proof obligations resulting from the model-simplification....
;; ;; E = Tc*TetA - 6/7+2/21*Tc*tetR-1/28000*TetRTc+3/700*TetR; TetA-20/11; 
;; ;; E = Tc*TetR-12/10000*TetRTc; Tc*TetR-15/40000*TetRTc-3/2000+11/40000*TetR
;; (defun test50 ()
;;   (init-test)
;;   (prep:set-variables '(Tc TetA TetRTc TetR))
;;   (prep:set-parameters '())
;;   (let* ((Tc (list (cons 1 (list (cons (mk-sal-nameexpr nil 'Tc) 1)))))
;; 	 (TetA (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetA) 1)))))
;; 	 (TetRTc (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetRTc) 1)))))
;; 	 (TetR (list (cons 1 (list (cons (mk-sal-nameexpr nil 'TetR) 1)))))
;; 	 (tcta (prep:polyrepMultiplyPoly Tc TetA)) 
;; 	 (tctr (prep:polyrepMultiplyPoly Tc TetR)) 
;; 	 (tctaminus6by7 (prep:polyrepAddPoly tcta (toPolyRepCnst (/ -6 7))))
;; 	 (tctaminus6by7n (prep:polyrepAddPoly (prep:polyrepAddPoly tctaminus6by7 
;; 			(prep:polyrepMultiplyPoly (toPolyRepCnst (/ 2 21)) tctr))
;; 			(prep:polyrepAddPoly
;; 			(prep:polyrepMultiplyPoly (toPolyRepCnst (/ -1 28000)) TetRTc)
;; 			(prep:polyrepMultiplyPoly (toPolyRepCnst (/ 3 700)) TetR))))
;; 	 (ta20by11 (prep:polyrepAddPoly TetA (toPolyRepCnst (/ -20 11))))
;; 	 (tctrminus12by10000rc (prep:polyrepAddPoly tctr 
;; 		(prep:polyrepMultiplyPoly (toPolyRepCnst (/ -12 10000)) TetRTc)))
;; 	 (tctrminus15by40000rc (prep:polyrepAddPoly tctr 
;; 		(prep:polyrepMultiplyPoly (toPolyRepCnst (/ -15 40000)) TetRTc)))
;; 	 (tctrminus15by40000rcminus3by2000 (prep:polyrepAddPoly tctrminus15by40000rc 
;; 		(toPolyRepCnst (/ -3 2000))))
;; 	 (n4 (prep:polyrepAddPoly tctrminus15by40000rcminus3by2000 
;; 			(prep:polyrepMultiplyPoly (toPolyRepCnst (/ 11 4000)) TetR)))
;; 	 (E (list tctaminus6by7n ta20by11 tctrminus12by10000rc n4))
;; 	 (r1 (prep:polyrepAddPoly TetR (prep:polyrepNegativePoly TetA)))
;; 	 (r2 (prep:polyrepAddPoly TetRTc (toPolyRepCnst (/ -12000 5))))
;; 	 (r3 (prep:polyrepAddPoly TetR (toPolyRepCnst (/ -2 11)))))
;;     (check (sos (copy-list E) (list r1 TetRTc)) -1 "Test50")
;;     (check (sos (copy-list E) (list r2 TetRTc)) -1 "Test50")
;;     (check (sos E (list r3 TetRTc)) -1 "Test50")))


;; Bug from (call-yices "../examples/john8.ys")
;;   y_8^2 + y_6^2 - 1 = 0
;;   y_3^2 + y_1^2 - 1 = 0
;;   y_8*y_3 + y_6*y_1 - 1 > 0
(defun test-bug-john8 ()
  (init-test)
  (let* ((y_8  x1)
	 (y_6  x2)
	 (y_3  x3)
	 (y_1  x4)
	 (sum1
	  (prep:polyrepAddPoly
	   (prep:polyrepAddPoly
	    (prep:polyrepMultiplyPoly y_8 y_8)
	    (prep:polyrepMultiplyPoly y_6 y_6)
	   )
	   (toPolyRepCnst -1)))
	 (sum2
	  (prep:polyrepAddPoly
	   (prep:polyrepAddPoly
	    (prep:polyrepMultiplyPoly y_3 y_3)
	    (prep:polyrepMultiplyPoly y_1 y_1)
	   )
	   (toPolyRepCnst -1)))
	 (sum3
	  (prep:polyrepAddPoly
	   (prep:polyrepAddPoly
	    (prep:polyrepMultiplyPoly y_8 y_3)
	    (prep:polyrepMultiplyPoly y_6 y_1)
	   )
	   (toPolyRepCnst -1)))
	 (E (list sum1 sum2))
	 (R (list sum3)))
    (check (vts:poly-lists-to-constraint-set E nil R nil) #'vts:is-unknown? "test-bug-john8")))