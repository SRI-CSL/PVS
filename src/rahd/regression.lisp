;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Regression suite for RAHD testing and benchmarking **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         07-Oct-2008,
;;;            last updated on  23-Oct-2008.

(in-package RAHD)

;;;
;;; *REGRESSION-SUITE*: A list of problems for RAHD testing and benchmarking.
;;;

(defparameter *regression-suite*
  '( ((((> f 0)) 
       ((<= f 100))
       ((>= v 0))
       ((<= v 100))
       ((> (* 100 f)
	   (* 100 (+ f v))))) 
      :UNSAT)

     ((((>= A 0))
       ((< 3 (* 2 X)))
       ((< (+ (* 3 X) (* 7 A)) 4))) 
      :UNSAT)

     ((((NOT (>= (- (* A1 A2) (* B1 B2)) 0)))
       ((= (+ (* A1 A1) (* A2 A2))
	   (+ (* B1 B1) (+ (* B2 B2) 2))))
       ((= (+ (* A1 B1) (* A2 B2)) 0))
       ((>= A1 0))
       ((>= A2 0))) 
      :UNSAT)

     ((((NOT (< A 0)))
       ((< 3 (* 2 X)))
       ((< (+ (* 3 X) (* 7 A)) 4))) 
      :UNSAT)

     ((((= (+ (* A (* X X)) (+ (* B X) C)) 0))
       ((< (* B B) (* 4 (* A C))))) 
      :UNSAT)

     ((((NOT (>= (* B B) (* 4 (* A C)))))
       ((= (+ (* A (* X X)) (+ (* B X) C)) 0))) 
      :UNSAT)

     ((((NOT (< (+ (* X X) (* Y Y)) 1)))
       ((NOT (< (+ (* X X) (* (- Y 1) (- Y 1))) 1)))
       ((NOT (< (+ (* (- X 1) (- X 1)) (* Y Y)) 1)))
       ((NOT (< (+ (* (- X 1) (- X 1))
		   (* (- Y 1) (- Y 1)))
		1)))
       ((<= X 1))
       ((<= Y 1))
       ((<= 0 X))
       ((<= 0 Y))) 
      :UNSAT)

     ((((NOT (<= (* A C) (* Y X))))
       ((= (* X X) C))
       ((= (* Y Y) (+ (* (* A A) C) B)))
       ((<= 0 B))
       ((<= 0 C))
       ((<= 0 X))
       ((<= 0 Y)))
      :UNSAT)

     ((((NOT (>= (+ (* X Y) (+ (* X Z) (* Y Z)))
		 (* 3 (* X (* Y Z))))))
       ((<= 0 X))
       ((<= 0 Y))
       ((<= 0 Z))
       ((<= (+ X (+ Y Z)) 3)))
      :UNSAT)

     ((((NOT (<= (* (+ X (+ Y Z)) (+ X (+ Y Z))) 3)))
       ((= (+ (* X X) (+ (* Y Y) (* Z Z))) 1)))
      :UNSAT)

     ((((NOT (>= (- (* 2 (+ (* X Z) (+ (* X Y) (* Y Z))))
		    (+ (* X X) (+ (* Y Y) (* Z Z))))
		 0)))
       ((<= X 125841/50000))
       ((<= Y 125841/50000))
       ((<= Z 125841/50000))
       ((<= 2 X))
       ((<= 2 Y))
       ((<= 2 Z)))
      :UNSAT)

     ((((NOT (<= 0
		 (- (* 2 (+ (* X Z) (+ (* X Y) (* Y Z))))
		    (+ (* X X) (+ (* Y Y) (* Z Z)))))))
       ((<= X 4))
       ((<= Y 4))
       ((<= Z 4))
       ((<= 2 X))
       ((<= 2 Y))
       ((<= 2 Z)))
      :UNSAT)

     ((((NOT (<= 12
		 (- (* 2 (+ (* X Z) (+ (* X Y) (* Y Z))))
		    (+ (* X X) (+ (* Y Y) (* Z Z)))))))
       ((<= X 4))
       ((<= Y 4))
       ((<= Z 4))
       ((<= 2 X))
       ((<= 2 Y))
       ((<= 2 Z)))
      :UNSAT)

     ((((NOT (<= (* (* (* X (* Y Z)) (* X (* Y Z)))
		    (* (+ X (+ Y Z))
		       (* (+ X (+ Y Z)) (+ X (+ Y Z)))))
		 (* (+ (* (* X X) Z)
		       (+ (* (* Y Y) X) (* (* Z Z) Y)))
		    (* (+ (* (* X X) Z)
			  (+ (* (* Y Y) X) (* (* Z Z) Y)))
		       (+ (* (* X X) Z)
			  (+ (* (* Y Y) X) (* (* Z Z) Y))))))))
       ((<= 0 X))
       ((<= 0 Y))
       ((<= 0 Z)))
      :UNSAT)

     ((((NOT (>= (+ (* (- 1 (* (* A A) (* B B)))
		       (* (- 1 (* C D))
			  (* (- (* A D) (* B C))
			     (- (* A D) (* B C)))))
		    (+ (* 2
			  (* A
			     (* B
				(* (- (* C D) (* A B))
				   (* (- 1 (* A B))
				      (* (- C D) (- C D)))))))
		       (* (- (* (* A A) (* B B))
			     (* (* C C) (* D D)))
			  (* (- 1 (* C D)) (* (- A B) (- A B))))))
		 0)))
       ((<= A 1))
       ((<= B 1))
       ((<= C 1))
       ((<= D 1))
       ((<= 0 A))
       ((<= 0 B))
       ((<= 0 C))
       ((<= 0 D)))
      :UNSAT)

     ((((NOT (>= X 0)))
       ((> A 0))
       ((> (* A X) 0)))
      :UNSAT)

     ((((NOT (< (- A (* K B)) B)))
       ((= (* A A) (* (+ (* K K) 1) (* B B))))
       ((<= 1 A))
       ((<= 1 B))
       ((<= 1 K)))
      :UNSAT)

     ((((NOT (>= (+ (* A D) (+ (* C B) (* B D))) 0)))
       ((>= D 0))
       ((>= (+ (* A A) (- (* A B) (* B B))) 0))
       ((>= (+ (* 2 A) B) 0))
       ((<= (+ (* C C) (- (* C D) (* D D))) 0)))
      :UNSAT)

     ((((= (+ (* C C) (+ (* D C) (* -1 (* D D)))) 0))
       ((> (+ (* 2 A) B) 0))
       ((> (+ (* A A) (+ (* B A) (* -1 (* B B)))) 0))
       ((> A 0))
       ((> D 0)) 
       ((> C 0))
       ((= B 0) (< B 0) (> B 0))
       ((< (+ (* C B) (+ (* D A) (* D B))) 0)))
      :UNSAT)

     ((((NOT (> (+ (* A D) (+ (* C B) (* B D))) 0)))
       ((> D 0))
       ((> (+ (* A A) (- (* A B) (* B B))) 0))
       ((> (+ (* 2 A) B) 0))
       ((< (+ (* C C) (- (* C D) (* D D))) 0)))
      :UNSAT)

     ((((NOT (> (+ (* A D) (+ (* C B) (* B D))) 0)))
       ((>= B 0))
       ((>= C 0))
       ((>= D 1))
       ((>= (+ (* A A) (- (* A B) (* B B))) 1))
       ((>= (+ (* 2 A) B) 1))
       ((<= (+ (* C C) (+ (- (* C D) (* D D)) 1))
	    0))) 
      :UNSAT)

     ((((NOT (> (+ (* A D) (+ (* C B) (* B D))) 0)))
       ((>= C 0))
       ((>= D 1))
       ((>= (+ (* A A) (- (* A B) (* B B))) 1))
       ((>= (+ (* 2 A) B) 1))
       ((<= B 0))
       ((<= (+ (* C C) (+ (- (* C D) (* D D)) 1))
	    0)))
      :UNSAT)

     ((((= (+ (* C C) (+ (* D C) (+ (* -1 (* D D)) 1))) 0))
       ((< B 0)) 
       ((> (+ (* 2 A) (+ B -1)) 0))
       ((> (+ (* A A) (+ (* B A) (+ (* -1 (* B B)) -1))) 0))
       ((> D 1)) ((> C 0))
       ((< (+ (* C B) (+ (* D A) (* D B))) 0)))
      :UNSAT)

     ((((NOT (>= (+ (* X (* X (* X X)))
		    (+ (* 2 (* (* X X) Z))
		       (+ (- (* X X) (* 2 (* X (* Y Z))))
			  (+ (* 2 (* (* Y Y) (* Z Z)))
			     (+ (* 2 (* Y (* Z Z)))
				(+ (- (* 2 (* Z Z)) (* 2 X))
				   (+ (* 2 (* Y Z)) 1)))))))
		 0))))
      :UNSAT)

     ((((= (+ (* 2 A) (+ B -1)) 0))
       ((= (+ (* A A) (+ (* B A) (+ (* -1 (* B B)) -1))) 0))
       ((= (+ (* C B) (+ (* D A) (* D B))) 0))
       ((< (+ (* -5/3 (* D D)) 1) 0)) ((> D 1)) ((> C 0))
       ((> B 0))) 
      :UNSAT)

     ((((NOT (<= (+ X1
		    (+ X2
		       (+ X3 (- X4 (* X1 (* X2 (* X3 X4)))))))
		 (- 4 1))))
       ((<= X1 1))
       ((<= X2 1))
       ((<= X3 1))
       ((<= X4 1))
       ((<= 0 X1))
       ((<= 0 X2))
       ((<= 0 X3))
       ((<= 0 X4)))
      :UNSAT)

     ((((= (+ (* x x) (* y y) 1) 0))
       ((= (+ (* x2 x2) (* y2 y2)) (+ (* x x) (* y y) 1)))
       ((= (+ (* x3 x4) (* y3 y4)) (+ (* x2 x2) (* y2 y2))))
       ((= (+ (* x3 x4) (* y3 y4)) (+ (* x x) (* y y) 2))))
      :UNSAT)

     ((((>= (+ (* x x) y) 0))
       ((< y 0))
       ((> (* y y) (* x x x x))))
      :UNSAT)

     ((((>= (+ (* x x x x) (* y y y)) 0))
       ((< (* y y y) 0))
       ((> (* y y y y y y) (* x x x x x x x x))))
      :UNSAT)

     ((((>= (+ (* x x x x x x x x) (* y y y y y)) 0))
       ((< (* y y y y y) 0))
       ((> (* y y y y y y y y y y) (* x x x x x x x x x x x x x x x x))))
      :UNSAT)

     ((((>= a 0)) ((>= b 0)) ((>= c 0))
       ((>= d 0)) ((<= a 1)) ((<= b 1))
       ((<= c 1)) ((<= d 1))
       ((< (+ (* (- 1 (* a a b b)) (- 1 (* c d)) (- (* a d) (* b c))
		 (- (* a d) (* b c)))
	      (* (* 2 a b) (- (* c d) (* a b)) (- 1 (* a b)) (- c d) (- c d))
	      (* (- (* a a b b) (* c c d d)) (- 1 (* c d)) (- a b) (- a b)))
	   0)))
      :UNSAT)

     ((((< (+ (* (* b (* b (* b b))) (* z (* z (* z z))))
	      (+ (* -8 (* c (* (* b b) (* a (* z (* z (* z z)))))))
		 (+ (* 16 (* (* c c) (* (* a a) (* z (* z (* z z))))))
		    (+ (* (* a a) (* x (* x (* x x))))
		       (+ (* 2 (* b (* a (* x (* x x)))))
			  (+ (* (* b b) (* x x))
			     (+ (* 2 (* (* b b) (* z z)))
				(+ (* 2 (* c (* a (* x x))))
				   (+ (* -8 (* c (* a (* z z))))
				      (+ (* 2 (* c (* b x)))
					 (+ (* c c) 1))))))))))) 0)))
      :UNSAT)
     

     ((((NOT (< X 0)) (NOT (< Y 0)) (NOT (< 0 (* X Y))))
       ((NOT (< 0 X)) (NOT (< 0 Y)) (NOT (< 0 (* X Y))))
       ((< X 0) (< 0 X) (< 0 (* X Y)))
       ((< X 0) (< 0 Y) (< 0 (* X Y)))
       ((< Y 0) (< 0 X) (< 0 (* X Y)))
       ((< Y 0) (< 0 Y) (< 0 (* X Y))))
      :UNSAT)

     ;; This linear one below we solve completely using RCR-SVARS + new demod-lin

     ((((= X Y)) ((= Y Z)) ((= Z W)) ((= W V)) ((= V A)) ((= A B)) ((> A X)))
      :UNSAT)

     ;; Below is an especially nice one for motivating an RCR-SVARS enhancement for
     ;; monic indeterminate equal power reductions, and for directed linear demodulation.
     ;; 23-Oct-2008

     ((((= (* X X X) (* Y Y Y))) ((= (* Y Y Y) (* Z Z Z)))
       ((= (* Z Z Z) (* W W W))) ((= (* W W W) (* V V V)))
       ((= (* V V V) (* A A A))) ((= (* A A A) (* B B B)))
       ((= (* C C C) (* B B B))) ((= (* D D D) (* C C C))) ((> A X)))
      :UNSAT)
     
     ;; A nice 11-dimensional one beyond the reach of QEPCAD/SOS
     ;; I comment this out because Sam and folk won't have CoCoA and this will take a long
     ;; time without it.

;     ((((>= (+ (* X3 12 X7 G1) (* G1 G1 -3 G2)) (* X (+ Y 11))))
;       ((= (* X X X Y Y) Z)) ((= (* Z W) A)) ((= A 0)) ((>= X 1)) ((>= Y X)) ((< W -1)) 
;       ((> G (+ 82 G1 G2))) ((= (* 5 X Y 9 (+ 1 D)) G)))
;      :UNSAT :GBASIS-USE-COCOA)

     ((((NOT (>= (+ (* A D) (+ (* C B) (* B D))) 0)))
       ((>= D 0))
       ((>= (+ (* A A) (- (* A B) (* B B))) 0))
       ((>= (+ (* 2 A) B) 0))
       ((<= (+ (* C C) (- (* C D) (* D D))) 0)))
      :UNSAT)

     ((((= (* X X X X X X X X X X X X X X X X) 1)) 
       ((>= Y 2)) ((>= X (+ 1 W))) 
       ((>= W (* Y Y Y))))
      :UNSAT)

     ((((NOT (>= (- (* A1 A2) (* B1 B2)) 0)))
       ((= (+ (* A1 A1) (* A2 A2))
	   (+ (* B1 B1) (+ (* B2 B2) 2))))
       ((= (+ (* A1 B1) (* A2 B2)) 0))
       ((>= A1 0))
       ((>= A2 0)))
      :UNSAT)

     ))

;;;
;;; RAHD-REGRESSION-SUITE: Run the entire regression suite and make sure RAHD solves problems
;;;  appropriately.
;;;
;;; Note: We currently only call the waterfall as-is.  We will want to eventually
;;;  store hints in the regression suite for those goals which need them.
;;;

(defun rahd-regression-suite (&optional &key no-pervasive-gbasis-cache gbasis-use-cocoa 
					use-cpc pervasive-cpc)
  (let ((i 0) 
	(report-str "")
	(start-time 0))
    (when use-cpc (setq *canon-poly-use-cache* t))
    (dolist (r *regression-suite*)
      (let ((r-cnf (car r))
	    (r-decision (cadr r))
	    (r-params (if (consp (cddr r)) (caddr r) nil)))
	(rahd-reset-state)
	(when no-pervasive-gbasis-cache (setq *gbasis-cache* (make-hash-table :test 'equal)))
	(if (or gbasis-use-cocoa
		(eql r-params ':GBASIS-USE-COCOA))
		(setq *gbasis-use-cocoa* t)
	  (setq *gbasis-use-cocoa* nil))
	(g r-cnf)
	(setq start-time (get-internal-real-time))
	(let ((live-decision (go! :do-not-reset-cpc pervasive-cpc)))
	  (setq report-str 
		(format nil "~A ~A" 
			report-str
			(if (and live-decision
				 (eq r-decision ':UNSAT))
			    (format nil
				    "~% >> [RAHD-REGRESSION-SUITE]: *SUCCESS* :: Regression suite entry ~A successfully proved in approximately ~D seconds." 
				    i (float (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
			  (format nil
				  "~% >> [RAHD-REGRESSION-SUITE]: *FAILURE* :: Regression suite entry ~A FAILED." i)))))
      (setq i (1+ i))))
    (format *standard-output* report-str))
  t)