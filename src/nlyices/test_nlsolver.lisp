(load-nl "polyrep-totdeglex.lisp")
(load-nl "decide3_2a.lisp")
(load-nl "nlsolver.lisp")
(load-nl "named-callbacks")
(load-nl "fcpo")

(use-package "nlsolver")

;; variables
(prep:set-variables (copy-list '(x1 x2 x3 x4 x5 c1 c2 c3 c4 c5)))
(prep:set-parameters nil)

(defconstant x1 (fcpo:polyrepVarToPoly 'x1))
(defconstant x2 (fcpo:polyrepVarToPoly 'x2))
(defconstant x3 (fcpo:polyrepVarToPoly 'x3))
(defconstant x4 (fcpo:polyrepVarToPoly 'x4))
(defconstant x5 (fcpo:polyrepVarToPoly 'x5))

(defconstant c1 (fcpo:polyrepVarToPoly 'c1))
(defconstant c2 (fcpo:polyrepVarToPoly 'c2))
(defconstant c3 (fcpo:polyrepVarToPoly 'c3))
(defconstant c4 (fcpo:polyrepVarToPoly 'c4))
(defconstant c5 (fcpo:polyrepVarToPoly 'c5))

(defconstant two (fcpo:polyrepConstToPoly 2))
(defconstant three (fcpo:polyrepConstToPoly 3))
(defconstant four (fcpo:polyrepConstToPoly 4))
(defconstant five (fcpo:polyrepConstToPoly 5))

;; x1 > 0 and x1 = 0
(defun test1 ()
  (let ((solver (create)))
    (assert-pos solver x1)  ; x1 > 0
    (assert-zero solver x1) ; x1 = 0
    (print-solver solver)
    (check solver)))

;; x1 > 5 and x1 < 4
(defun test2 ()
  (let ((solver (create)))
    (assert-gt solver x1 five)
    (assert-lt solver x1 four)
    (print-solver solver)
    (check solver)))


;; x1 > 5 and x2 < 4
(defun test3 ()
  (let ((solver (create)))
    (assert-gt solver x1 five)
    (assert-lt solver x2 four)
    (print-solver solver)
    (check solver)))



;; x1^2 < 0
(defun test4 ()
  (let ((solver (create)))
    (assert-neg solver (prep:polyrepExpPolyCst x1 2))
    (print-solver solver)
    (check solver)))

;; x1^2 > 0, x1^3 <= 0
(defun test5 ()
  (let ((solver (create)))
    (assert-pos solver (prep:polyrepExpPolyCst x1 2))
    (assert-nonpos solver (prep:polyrepExpPolyCst x1 3))
    (print-solver solver)
    (check solver)))


;; x1^2 > 0, x1^4 <= 0
(defun test6 ()
  (let ((solver (create)))
    (assert-pos solver (prep:polyrepExpPolyCst x1 2))
    (assert-nonpos solver (prep:polyrepExpPolyCst x1 4))
    (print-solver solver)
    (check solver)))



;; test incremental solver x1 > 0, x1 = 0
(defun itest1 ()
  (let ((solver (create)))
    (format t "First assertion~%")
    (assert-pos solver x1)
    (print-solver solver)
    (let ((s1 (incremental-check solver)))
      (format t "Result: ~a~%" s1)
      (if s1
	  (progn (format t "Second assertion~%")
		 (assert-zero solver x1)
		 (print-solver solver)
		 (format t "Result: ~a~%" (incremental-check solver)))))))
	
    
;; test incremental solver: x1^2 > 0, x1^4 <= 0
(defun itest6 ()
  (let ((solver (create)))
    (format t "First assertion~%")
    (assert-pos solver (prep:polyrepExpPolyCst x1 2))
    (print-solver solver)
    (let ((s1 (incremental-check solver)))
      (format t "Result: ~a~%" s1)
      (if s1
	  (progn (format t "Second assertion~%")
		 (assert-nonpos solver (prep:polyrepExpPolyCst x1 4))
		 (print-solver solver)
		 (format t "Result: ~a~%" (incremental-check solver)))))))
	
    
