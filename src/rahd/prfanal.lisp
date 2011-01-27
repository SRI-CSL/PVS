;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; Some tools for analysing RAHD proofs and gathering statistics from them.
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         28-Feb-2009
;;;            last updated on  22-Nov-2009.
;;;

(in-package :rahd)

(defparameter *cur-prob* nil)

;;;
;;; PRINT-PROOF-ANALYSIS: Print the contents of *PROOF-ANALYSIS-CACHE*.
;;;

(defun print-proof-analysis (&optional f)
  (declare (ignore f))
  (if (= (hash-table-count *proof-analysis-cache*) 0)
      (fmt 0 "~%~% *** No proof information has been recorded for analysis.  See :DOC *ENABLE-PROOF-ANALYSIS*.~%~%")
    (let ((pac-lst nil)
	  (processed-goal-no-ss (ef-to-cnf (f-to-div-free-explicit-fof (expand-formula *g*)))) ; before subsumption checking
	  (processed-goal (with-rahd-verbosity 0 (f-to-div-free-cnf (expand-formula *g*)))))
      (fmt 0 "~%~% >> Proof analysis report:~%~%")
      (fmt 0 "     Dimension:      ~A~%     Max Degree:     ~A~%     Division?:      ~A~%     # Polynomials:  ~16A (in {=,<,>}-split RCF translation of goal)~%     # Monomials:    ~16A (in {=,<,>}-split RCF translation of goal)~%     # EIL Clauses:  ~A~%     # RCF Clauses:  ~16A (before subsumption reduction)~%                     ~16A (after subsumption reduction)~%     # GS Cases:     ~16A (before subsumption reduction)~%                     ~16A (after subsumption reduction)~%     # PT Leaves:    ~A~%~%     Tactic breakdown:~%~%"
	   (processed-goal-dim processed-goal) 
	   (processed-goal-deg processed-goal) 
	   (div-formula? *g*)
	   (processed-goal-num-polys processed-goal)
	   (processed-goal-num-mons processed-goal)
	   (length *g*) 
	   (length processed-goal-no-ss)
	   (length processed-goal)
	   (num-dnf-cases processed-goal-no-ss)
	   *gs-size*
	   (num-leaves 0))
      (maphash #'(lambda (x y)
		   (setq pac-lst (cons (list x y) pac-lst)))
	       *proof-analysis-cache*)
      (setq pac-lst (sort pac-lst #'(lambda (x y)
				      (> (cadadr x) (cadadr y)))))
      (dolist (tl pac-lst) 
	(fmt 0 "         Tactic: ~20A       # Contributions: ~10A     Time: ~8A~%"
	     (car tl) (caadr tl) (cadadr tl)))

      (fmt 0 "~%")

      ;;;
      ;;; Should we make a LaTeX easy report?
      ;;;

      (when *enable-proof-analysis-latex*
	(group-tactic-data 
	 pac-lst
	 :problem (format nil "P~A" *cur-prob*)
	 :dim (processed-goal-dim processed-goal)
	 :deg (processed-goal-deg processed-goal)
	 :div (div-formula? *g*)
	 :size-gs *gs-size*
	 :size-pt (num-leaves 0)
	 :polys (processed-goal-num-polys processed-goal)
	 :mons (processed-goal-num-mons processed-goal)))

      t)))

;;;
;;; GROUP-TACTIC-DATA: Group the *PROOF-ANALYSIS-CACHE* tactic data into 
;;;  sections based upon those tactics that share similar properties.
;;;  This is for easily making nice tables describing RAHD proofs in
;;;  academic papers and presentations (where we don't have enough room
;;;  to list data about *every* tactic).
;;;

(defun group-tactic-data (pac-lst &key problem dim deg div size-gs size-pt polys mons)
  (let ((group-hash (make-hash-table :test 'equal))
	(groupings '((G-SIMP-ARITH (SIMP-GLS DEMOD-NUM CONTRA-EQS SIMP-ZRHS SIMP-ARITH SIMP-TVS))
		     (G-SIMP-GB    (RCR-INEQS CANON-TMS TRIV-IDEALS))
		     (G-SOS        (FERT-TSOS SIMP-REAL-NULL INT-DOM-ZPB))
		     (G-STURM      (UNIV-STURM-INEQS))
		     (G-RAD-IDEALS (RCR-SVARS))
		     (G-OPEN-CAD   (OPEN-EX-INF-CAD OPEN-FRAG-EX-INF-CAD))
		     (G-GEN-CAD    (GEN-EX-CAD)))))
    (dolist (group groupings)
      (let ((cur-group (car group))
	    (cur-group-contribs 0)
	    (cur-group-time 0))
	(dolist (tactic (cadr group))
	  (let ((find-tactic-in-pac-lst (assoc tactic pac-lst)))
	    (when find-tactic-in-pac-lst
	      (setq cur-group-contribs (+ cur-group-contribs (caadr find-tactic-in-pac-lst)))
	      (setq cur-group-time (+ cur-group-time (cadadr find-tactic-in-pac-lst))))))
	(setf (gethash cur-group group-hash)
	      cur-group-contribs))) ; Right now, we only log contribs for this table generation
	   (fmt 1/2 " ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & ~6A & & \\\\~%"
		problem
		dim
		deg
		(if div "Y" "N")
		size-gs
		size-pt
		polys
		mons
		(gethashval 'G-SIMP-ARITH group-hash)
		(gethashval 'G-SIMP-GB group-hash)
		(gethashval 'G-SOS group-hash)
		(gethashval 'G-STURM group-hash)
		(gethashval 'G-RAD-IDEALS group-hash)
		(gethashval 'G-OPEN-CAD group-hash)
		(gethashval 'G-GEN-CAD group-hash))))

;;;
;;; GETHASHVAL
;;;

(defun gethashval (k h)
  (multiple-value-bind (v e?) (gethash k h)
    (when e? v)))
      


;;;
;;; PROCESSED-GOAL-DIM:
;;; Given a *processed* RAHD goal formula (in CNF), return its dimension.
;;; 

(defun processed-goal-dim (f)
  (let ((vs-in-f nil))
    (dolist (c f)
      (setq vs-in-f (union vs-in-f (all-vars-in-conj c))))
    (length vs-in-f)))

;;;
;;; GOAL-DIM:
;;; Given an input RAHD goal, process it and count the result's dimension.
;;;

(defun goal-dim (f)
  (processed-goal-dim (f-to-div-free-cnf (expand-formula f))))

;;;
;;; PROCESSED-GOAL-DEG:
;;; Given a *processed* RAHD goal formula (in CNF), return the maximal
;;; multivariate total degree of all polynomials appearing in the goal.
;;;

(defun processed-goal-deg (f)
  (let ((max-deg 0))
    (dolist (c f)
      (dolist (l c)
	(let ((deg-x (poly-deg (poly-prover-rep-to-alg-rep (cadr l))))
	      (deg-y (poly-deg (poly-prover-rep-to-alg-rep (caddr l)))))
	  (setq max-deg (max max-deg deg-x deg-y)))))
    max-deg))

;;;
;;; GOAL-DEG:
;;; Given an input RAHD goal, process it and count the result's max mvt deg.
;;;

(defun goal-deg (f)
  (processed-goal-deg (f-to-div-free-cnf (expand-formula f))))

;;;
;;; PROCESSED-GOAL-NUM-POLYS:
;;; Given a *processed* RAHD goal formula (in CNF), return the number of
;;;  polynomials appearing in the problem.  This will be twice the number
;;;  of atomic relations that appear.
;;;

(defun processed-goal-num-polys (f)
  (let ((num-polys 0))
    (dolist (c f)
      (setq num-polys (+ num-polys (* 2 (length c)))))
    num-polys))

;;;
;;; GOAL-NUM-POLYS:
;;; Given an input RAHD goal, process it and count the number of polys 
;;;  it contains.
;;;

(defun goal-num-polys (f)
  (processed-goal-num-polys (f-to-div-free-cnf (expand-formula f))))

;;;
;;; PROCESSED-GOAL-NUM-MONS:
;;; Given a *processed* RAHD goal formula (in CNF), return the total
;;; number of monomials appearing in the problem.
;;;

(defun processed-goal-num-mons (f)
  (let ((num-mons 0))
    (dolist (c f)
      (dolist (l c)
	(let ((mons-x (poly-length (poly-prover-rep-to-alg-rep (cadr l))))
	      (mons-y (poly-length (poly-prover-rep-to-alg-rep (caddr l)))))
	  (setq num-mons (+ mons-x mons-y num-mons)))))
    num-mons))

;;;
;;; GOAL-NUM-MONS:
;;; Given an input RAHD goal, process it and count the result's dimension.
;;;

(defun goal-num-mons (f)
  (processed-goal-num-mons (f-to-div-free-cnf (expand-formula f))))

;;;
;;; POLY-LENGTH: The number of monomials in a given polynomial
;;;  given in alg-rep.
;;;
;;; Note that a NIL polynomial is 0 (so it has one monomial).

(defun poly-length (p)
  (if p (length p) 1))

;;;
;;; NUM-DNF-CASES: Given a formula in CNF, count the number of cases
;;;  it would generate in DNF.
;;;

(defun num-dnf-cases (f)
  (let ((num-cases 1))
    (dolist (c f)
      (setq num-cases (* num-cases (length c))))
    num-cases))
  
;;;
;;; NUM-LEAVES: Count the number of leaves in the active proof tree.
;;;

(defun num-leaves (goal-key)
  (with-rahd-verbosity 0 (swap-to-goal goal-key))
  (let ((lc 0))
    (loop for i from 0 to (1- *gs-size*) do
	  (let ((cur-gs-unsat-just (aref *gs* i 2)))
	    (setq lc
		  (+ lc
		     (if (equal (cadr cur-gs-unsat-just)
				':DISCHARGED-BY-SUBGOAL)
			 (let ((out (num-leaves (caddr cur-gs-unsat-just))))
			   (with-rahd-verbosity 0 (swap-to-goal goal-key))
			   out)
		       1)))))
    lc))
			  
		
;;;
;;; DIV-FORMULA?: Does a given formula contain the division operator?
;;;

(defun div-formula? (f)
  (div-formula?* (expand-formula f)))

(defun div-formula?* (f)
  (let ((out nil))
    (dolist (c f)
      (dolist (l c)
	(let ((x (one-div (cadr l)))
	      (y (one-div (caddr l))))
	  (setq out 
		(or out 
		    (when (consp x) (equal (car x) '/)) 
		    (when (consp y) (equal (car y) '/)))))))
    out))

;;;
;;; ARRANGE-LST: Given a list and an explicit list of its indices, return
;;;  the list arranged in the order of the indices as given.
;;;

(defun arrange-lst (lst indices)
  (let ((out nil))
    (dolist (i indices)
      (setq out (cons (nth i lst) out)))
    (reverse out)))
