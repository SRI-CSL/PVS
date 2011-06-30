;;======================================================
;; Solver for non-linear arithmetic
;; This provides an API for using Ashish's non-linear
;; decision procedure that's easier to interface
;; with C than what's in decide3.2.lisp
;;======================================================

(defpackage :nlsolver
  (:use :common-lisp :gb :prep)
  (:export :create :reset :assert-zero :assert-pos :assert-nonneg
	   :assert-neg :assert-nonpos :assert-eq :assert-geq
	   :assert-leq :assert-gt :assert-lt :check :incremental-check
	   :is-witness? :print-solver :print-state :print-witness
	   :print-ids :verbose :quiet))

(in-package :nlsolver)



;;---------------------------------------------------
;; If verbose is on: print the solver state before
;; every call to check or incremental check
;;---------------------------------------------------

(defvar *verbose* t)

(defun verbose () (setf *verbose* t))

(defun quiet () (setf *verbose* nil))



;;--------------------------------------------------------
;; The solver records 3 lists of polynomials
;; - zeros = polynomials asserted to be 0
;; - positives = asserted to be > 0
;; - non-negatives = asserted to be >= 0 
;; - state = solver state for incremental processing
;; - witness = list of polynomials (unsat witness)
;; - assertions = array of polynomials for explanations
;; 
;; For constructing explanations:
;; - an assertion consists of a polynomial p, a sign-constraint,
;;   and an optional integer id 
;; - if the id is given, then we store assertions[i] := p.
;; - to reconstruct the unsat witness, the C-layer solver
;;   can query each assertion to know whether or not it
;;   occurs in the witness.
;;
;; This is simple but possibly inefficient. That should be OK for
;; now, since we can't handle large problems anyway. The C-layer will
;; ensure that the assertion ids are small integers in the range 
;; [0 ... n-1] where n is small (less than 100).
;; ---------------------------------------------------------

(defstruct solver zeros positives non-negatives state witness assertions)

;;---------------------------------------
;; Construct an initial (empty) solver
;;---------------------------------------

(defun create () 
  (make-solver :zeros nil 
	       :positives nil 
	       :non-negatives nil 
	       :state nil 
	       :witness nil
	       :assertions (make-array 0 :initial-element nil :adjustable t)))



;;---------------
;; Reset solver
;;---------------

(defun reset (solver)
  (setf (solver-zeros solver) nil)
  (setf (solver-positives solver) nil)
  (setf (solver-non-negatives solver) nil)
  (setf (solver-state solver) nil)
  (setf (solver-witness solver) nil)
  ;; reset assertions to the empty array
  (setf (solver-assertions solver) 
	(adjust-array (solver-assertions solver) 0 :initial-element nil)))




;;------------------------------------------
;; Add polynomials to the assertions array
;;------------------------------------------

;; make the array large enough to store something at index i
(defun extend-array (array i) 
  (let ((n (array-dimension array 0)))
    (cond ((< i n) array)
	  ((< i (* 2 n)) (adjust-array array (* 2 n) :initial-element nil))
	  ((< i 100) (adjust-array array 100 :initial-element nil))
	  (t (adjust-array array (+ 1 i) :initial-element nil)))))
     

;; store p at index i in array and return the resulting 
;; array. Make the array larger if needed.
(defun add-assertion (array i p)
  (let ((a (extend-array array i)))
    (setf (aref a i) p)
    a))

;; add p as assertion i
(defun save-assertion (solver i p)
  (setf (solver-assertions solver) (add-assertion (solver-assertions solver) i p)))


;; get assertion of index i (nil if it does not exists
(defun get-assertion (solver i) 
  (let* ((a (solver-assertions solver))
	 (n (length a)))
    (if (< i n) (aref a i) nil)))


;;-------------------------------------------------------------
;; Assertions add a polynomial p to one of the solver's lists
;;-------------------------------------------------------------

;; p = 0
(defun assert-zero (solver p &optional id) 
  (if id (save-assertion solver id p))
  (setf (solver-zeros solver) (cons p (solver-zeros solver))))

;; p > 0
(defun assert-pos (solver p &optional id)
  (if id (save-assertion solver id p))
  (setf (solver-positives solver) (cons p (solver-positives solver))))

;; p >= 0
(defun assert-nonneg (solver p &optional id)
  (if id (save-assertion solver id p))
  (setf (solver-non-negatives solver) (cons p (solver-non-negatives solver))))

;; p <= 0
(defun assert-nonpos (solver p &optional id) 
  (assert-nonneg solver (prep:polyrepNegativePoly p) id))

;; p < 0
(defun assert-neg (solver p &optional id)
  (assert-pos solver (prep:polyrepNegativePoly p) id))



;; build p - q
(defun subpoly (p q) 
  (prep:polyrepAddPoly p (prep:polyrepNegativePoly q)))

;; p1 = p2
(defun assert-eq (solver p1 p2 &optional id)
  (assert-zero solver (subpoly p1 p2) id))

;; p1 > p2
(defun assert-gt (solver p1 p2 &optional id)
  (assert-pos solver (subpoly p1 p2) id))

;; p1 >= p2
(defun assert-geq (solver p1 p2 &optional id)
  (assert-nonneg solver (subpoly p1 p2) id))

;; p1 < p2
(defun assert-lt (solver p1 p2 &optional id) 
  (assert-gt solver p2 p1 id))

;; p1 <= p2
(defun assert-leq (solver p1 p2 &optional id)
  (assert-geq solver p2 p1 id))



;;------------------------
;; Print the assertions
;;------------------------

(defun print-constraint (p constraint)
  (format t "   ~a ~a~%" (prep:polyrepPrint p) constraint))

(defun print-constraint-list (l constraint)
  (dolist (p l) (print-constraint p constraint)))

(defun print-solver (solver)
  (print-constraint-list (solver-zeros solver) "= 0")
  (print-constraint-list (solver-positives solver) "> 0")
  (print-constraint-list (solver-non-negatives solver) ">= 0"))



;;---------------------------
;; Print state and witness
;;---------------------------

(defun print-state (solver) 
  (gb:show-basis (solver-state solver)))

(defun print-witness (solver)
  (gb:show-witness (solver-witness solver)))



;;------------------------------------------------------------
;; Print the assertion ids and the corresponding polynomials
;;------------------------------------------------------------

(defun print-ids (solver)
  (print-assertion-array (solver-assertions solver)))

(defun print-assertion-array (array)
  (print-id-poly-pairs* array 0 (length array)))

(defun print-id-poly-pairs* (array i n)
  (when (< i n)
    (let ((p (aref array i)))
      (if p (format t "  assertion[~a]: ~a~%" i (prep:polyrepPrint p))))
    (print-id-poly-pairs* array (+ 1 i) n)))



;;-------------------------------------------------------
;; Non-incremental satisfiability check
;; - the non-zero constraints are ignored
;; - the state is ignored and is not changed
;; - if the result is unsat, then the unsatisfiability
;;   witness is stored
;;-------------------------------------------------------

(defun check (solver)
  (if *verbose* 
      (progn 
	(format t "~%CHECK~%")
	(print-solver solver)
	(format t "~%")))
  (multiple-value-bind
   (status polys)
   (gb:sos (solver-zeros solver)
	   (solver-positives solver)
	   (solver-non-negatives solver))
   (if (null status) ;; unsat
       (setf (solver-witness solver) polys))
   status))





;;-----------------------------------------------------------
;; Incremental check
;; - check satisfiability of all assertions + current state
;; - update the state and reset all assertion lists to nil
;; - if the result is unsat, store the unsatisfiability 
;;   witness
;;-----------------------------------------------------------

;; Check whether there's any new assertion
(defun new-assertions (solver)
  (or (solver-zeros solver) (solver-positives solver) 
      (solver-non-negatives solver)))

(defun incremental-check (solver)
  (if (new-assertions solver)
      (progn
	(if *verbose* 
	    (progn 
	      (format t "~%INCREMENTAL CHECK~%")
	      (print-solver solver)
	      (format t "~%")))
	(multiple-value-bind
	 (status polys)
	 (gb:sos-cheap (solver-zeros solver)
		       (solver-positives solver)
		       (solver-non-negatives solver)
		       (solver-state solver))
	 (setf (solver-zeros solver) nil)
	 (setf (solver-positives solver) nil)
	 (setf (solver-non-negatives solver) nil)
	 ;; update state or witness
	 (if status 
	     (setf (solver-state solver) polys)  ;; sat: polys is the new stat
	   (setf (solver-witness solver) polys)) ;; unsat: polys is the witness
	 status)
	)
    t))  ;; assumes the solver current status is "sat'





;;------------------------------------------------------------
;; Witness collection
;; - check assertion i belongs to the witness list
;; - this checks whether polynomial stored in assertions[i]
;;   occurs in the witness list
;; - return nil if  assertions[i] is nil 
;;------------------------------------------------------------

(defun is-witness? (solver id)
  (let ((p (get-assertion solver id)))
    (if p (gb:poly-in-POL-list? p (solver-witness solver)) nil)))

