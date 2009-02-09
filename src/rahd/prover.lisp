;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Core prover interface routines **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         29-July-2008,
;;;            last updated on  16-Nov-2008.
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; To Compile/Load:
;;;
;;;  :cload rahd
;;;  (rahd-reboot)
;;;
;;; Once loaded, to attempt to prove a goal automatically:
;;;
;;;  (g GOAL-IN-CNF) ; Install GOAL-IN-CNF as top-level goal (0).
;;;  (go!)           ; Invoke the waterfall upon GOAL-IN-CNF.
;;;
;;;
;;; To wipe your session and begin work on a new proof obligation without rebooting, 
;;; eval (rahd-reset-state).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package RAHD)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *RAHD-DEBUG*: Set the debug flag.  Also, see: (WITH-RAHD-DEBUGGING ...).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rahd-debug* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *RAHD-VERBOSITY*: Set the VERBOSITY level.  This determines how much live information
;;;  is printed out during proof search.  All calls to (FMT ...) are governed by this
;;;  value.
;;;
;;;   Key:   0   No output whatsoever during proof search,
;;;          1   Standard operation with information printed for every tactic that
;;;               made progress on the installed goal + a final report,
;;;          2   Enhanced version of 1 that prints the following characters during
;;;               tactic execution: 
;;;                                . - Tactic executed on a case in GOAL-SET and
;;;                                     no progress was made for that case,
;;;                                ! - Tactic executed on a case in GOAL-SET and
;;;                                     succeeded in refuting it,
;;;                                $ - Tactic executed on a case in GOAL-SET and
;;;                                     succeeded in reducing (but not refuting)
;;;                                     it,
;;;                                @ - Tactic executed on a case in GOAL-SET and
;;;                                     proved it was satisfiable, thus yielding 
;;;                                     a counter-example to the installed goal.
;;;
;;;          9  Proof debug mode.  Everything I've marked for possible printing that is of
;;;              direct relevance to proof search is printed.
;;;         10  System debug mode.  An enhancement of 9 that also prints information
;;;              pertaining to sys-calls (spawns, forks, and so on).
;;;
;;;  This verbosity level is cummulative, so that a setting of N causes all information
;;;  marked as level n (0 <= n <= N) to be printed (to *standard-output*).
;;;
;;;  Also, see: (WITH-RAHD-VERBOSITY n ...).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *rahd-verbosity* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GOAL-STACK-KEYS*: the collection of goal-keys for members of the goal-stack.
;;;
;;;  A goal-key for a goal X is always either 0 (meaning X is the top-level goal), or of 
;;;   the form (PARENT-GOAL-KEY . CASE), which signifies that the waterfall spawned 
;;;   goal X while it was working on case CASE of the goal with key PARENT-GOAL-KEY, Y,
;;;   and if X is refuted, then case CASE of goal Y will be refuted.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *goal-stack-keys* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GOAL-STACK-DATA*: A hash-table for (sub)goal meta-data, key'd by goal-key.
;;;
;;; Each member of the *GOAL-STACK-DATA* hash-table is an array of size five:
;;;
;;;  key:goal-key   (goal-in-cnf   goal-set-size   goal-set-unknown-size   goal-set-max-dim
;;;                  goal-set-tactic-replay)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *goal-stack-data* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GOAL-SETS*: A hash-table pairing each goal-key with its set of CTRs (cases to refute).
;;;
;;; Each member of the hash-table is an array with each element an array of the form:
;;;
;;;    (CASE-ID  CASE  STATUS)
;;;
;;; where CASE-ID is an array-index, CASE is a case to be refuted in CNF, and
;;; STATUS is either :UNKNOWN (must still be refuted) or :UNSAT (with a justification).
;;;
;;; Note: The GOAL-SETS record for the current goal under question is bound to *GS*.
;;;
;;; Note: Once a new goal is installed, its GOAL-SET entry is nil until it is DRILLED-DOWN
;;;  using BUILD-GOAL-SET.  No tactics can be applied until BUILD-GOAL-SET has been run.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *goal-sets* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CURRENT-GOAL-KEY: The key of the goal currently under focus.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *current-goal-key* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** LOCAL COPIES OF GOAL-STACK-DATA and GOAL-SET for CURRENT GOAL **
;;; 
;;; The following globals are swapped in and out when a user switches proof state
;;; between different active goals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *G*: The current top-level goal in CNF.  This is to be refuted.
;;;      All variables are interpreted as being existentially quantified.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *g* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GS*: The set of cases that must be each refuted in order
;;; to refute *g*.
;;;
;;; This is an array with each element an array of the form:
;;;
;;;    (CASE-ID  CASE  STATUS)
;;;
;;; where CASE-ID is an array-index, CASE is a case to be refuted in CNF, and
;;; STATUS is either :UNKNOWN (must still be refuted) or :UNSAT (with a justification).
;;;
;;; We set the *GS* to nil until the user has installed a goal and DRILLED-DOWN
;;; to create the goal-set of cases.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gs* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GS-SIZE*: The total number of cases in *GS*.  Once a goal-set is
;;; built for a goal, this should never be changed.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gs-size* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; *GS-UNKNOWN-SIZE*: The total number of cases currently with status :UNKNOWN
;;; in the goal-set (e.g. these are the cases that still remain to be refuted).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gs-unknown-size* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GS-MAX-DIM: The maximal dimension of the goals in the goal-set.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gs-max-dim* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TACTIC-REPLAY: A list of the sequence of tactic invocations that have taken place
;;; during the current verification session.  This is reset everytime (G ...) is invoked.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *tactic-replay* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LAST-TACTIC-MADE-PROGRESS: Did the last tactic evaluated make any progress?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *last-tactic-made-progress* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** GLOBALS AVAILABLE TO ALL GOALS **
;;;
;;; The following globals are available to all goals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GBASIS-CACHE: A hash table for caching Groebner basis calculations across cases and
;;;  case revisions / subgoals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gbasis-cache* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CANON-POLY-CACHE: A hash table for caching polynomial canonicalizations.  This should
;;;  be especially helpful for goals with large goal-sets.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *canon-poly-cache* (make-hash-table :test 'equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GBASIS-USE-COCOA: Should we use the CoCoA commutative algebra system to do Groebner
;;;  bases calculations (instead of our own internal routines)?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gbasis-use-cocoa* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CANON-POLY-USE-CACHE: Should we cache polynomial canonicalizations?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *canon-poly-use-cache* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EXACT-REAL-ARITH-USED: Was exact real arithmetic used in the current proof session?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *exact-real-arith-used* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CURRENT-TACTIC-CASE: The current case in the current goal's goal-set being examined in 
;;; the GENERIC-TACTIC loop.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *current-tactic-case* nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RESET-STATE: Reset entire (local and global) proof state.
;;; Note: if :keep-hashes is t, then we only reset local proof state.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rahd-reset-state (&optional no-output &key keep-hashes)
  (when (not keep-hashes)
      (progn
	(clrhash *gbasis-cache*)
	(clrhash *canon-poly-cache*)
	(setq *goal-stack-keys* nil)
	(clrhash *goal-stack-data*)
	(clrhash *goal-sets*)))
  (setq *current-goal-key* nil)
  (setq *last-tactic-made-progress* nil)
  (setq *tactic-replay* nil)
  (setq *exact-real-arith-used* nil)
  (setq *g* nil)
  (setq *gs* nil)
  (setq *gs-size* 0)
  (setq *gs-unknown-size* 0)
  (setq *gs-max-dim* 0)
  (when (not no-output)
    (fmt 1 "~% >> RAHD-RESET-STATE: ~A."
	 (if keep-hashes "Local goal state successfully reset, but global structures unchanged"
	   "Full RAHD system state successfully reset")))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SAVE-CURRENT-GOAL: Save the local data for the current goal in the
;;; GOAL-STACK-DATA and GOAL-SETS hash tables.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-current-goal (&optional no-output)
  (set-goal-stack-data *current-goal-key*
		       :goal-set-size *gs-size*
		       :goal-set-unknown-size *gs-unknown-size*
		       :goal-set-max-dim *gs-max-dim*
		       :goal-set-tactic-replay *tactic-replay*)
  (setf (gethash *current-goal-key* *goal-sets*) *gs*)
  (when (not no-output)
    (fmt 1 "~% >> SAVE-CURRENT-GOAL: Proof state for GOAL ~A has been successfully saved." 
	 (format-goal-key *current-goal-key*)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LOAD-GOAL: Load a goal from GOAL-STACK-DATA and GOAL-SETS into the active
;;; globals, so that it can be worked on tactically.
;;;
;;; Note: This does not change the order of GOAL-STACK-KEYS.
;;;       Also, this does not save the current state.  SAVE-CURRENT-GOAL should
;;;       be used for this (all wrapped together in SWAP-TO-GOAL).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-goal (goal-key &optional no-output)
  (multiple-value-bind 
   (goal-stack-record stack-rec-exists?)
   (gethash goal-key *goal-stack-data*)
   (if stack-rec-exists?
       (multiple-value-bind
	(goal-set-record set-rec-exists?)
	(gethash goal-key *goal-sets*)
	(if set-rec-exists?
	    (progn
	      
	      ;; Load GOAL-STACK data record

	      (setq *g* (aref goal-stack-record 0))
	      (setq *gs-size* (aref goal-stack-record 1))
	      (setq *gs-unknown-size* (aref goal-stack-record 2))
	      (setq *gs-max-dim* (aref goal-stack-record 3))
	      (setq *tactic-replay* (aref goal-stack-record 4))
	      
	      ;; Load GOAL-SET record
	      
	      (setq *gs* goal-set-record)
	      
	      ;; Set the current GOAL-KEY reference
	      
	      (setq *current-goal-key* goal-key)
	      
	      ;; Synchronization complete
	      
	      (when (not no-output)
		(fmt 1 "~% >> LOAD-GOAL: GOAL ~A successfully loaded into local bindings."
		     (format-goal-key goal-key)))
	      t)
	  (break (error-string 'g-no-goal-to-load `(,goal-key ',*goal-stack-keys*)))))
	(break (error-string 'g-no-goal-to-load `(,goal-key ',*goal-stack-keys*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SWAP-TO-GOAL: Save the proof state for the current goal, then load the proof state
;;;  for the goal whose key is GOAL-KEY.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun swap-to-goal (goal-key &optional no-output)
  (save-current-goal no-output)
  (load-goal goal-key no-output)
  (when (not no-output)
    (fmt 1 "~% >> SWAP-TO-GOAL: GOAL ~A successfully swapped in as the active goal." 
	 (format-goal-key goal-key)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; G: Install new current top-level goal.
;;;
;;; f: Goal in CNF to be added to goal-stack.
;;; goal-key: Key to assign to this goal.
;;;
;;; Keyword parameters:
;;;  :abandon-ok   -- Is it OK to abandon current proof session
;;;                    and start a new one?
;;;  :overwrite-ok -- Is it OK to overwrite the data of a goal 
;;;                    that already exists in the GOAL-STACK?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun g (f &optional (goal-key 0) &key abandon-ok overwrite-ok)
  (if (not f) (break (error-string 'g-empty-goal)))
  (cond ((and (or *current-goal-key* *goal-stack-keys*)
	      (equal goal-key 0)
	      (not (or abandon-ok overwrite-ok)))
	 (break (error-string 'g-abandon)))
	((and (not (equal goal-key 0))
	      (member goal-key *goal-stack-keys*)
	      (not overwrite-ok))
	 (break (error-string 'g-overwrite `(,goal-key))))
	(t (if (or abandon-ok (not *goal-stack-keys*)) 

	       ;;
	       ;; Either the first goal of a session or the
	       ;; the first goal after the abandonment of a session,
	       ;; thus we reset the entire system state.
	       ;;

	       (rahd-reset-state) 

	     ;;
	     ;; Otherwise, we are adding a new goal on top of a 
	     ;; running session, so we save the current goal.
	     ;;

	     (save-current-goal))
	   (if *goal-stack-keys* (rahd-reset-state nil :keep-hashes t))
	   (push goal-key *goal-stack-keys*)
	   (remove-duplicates *goal-stack-keys*)
	   (setf (gethash goal-key *goal-stack-data*) (make-array 5))
	   (set-goal-stack-data goal-key :goal-in-cnf f)
	   (setq *g* f)
	   (setq *current-goal-key* goal-key)
	   (fmt 1 "~% >> G: GOAL ~A successfully installed on GOAL-STACK and locally bound to *G*. ~%~%" 
		(format-goal-key goal-key))
	   t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SET-GOAL-STACK-DATA: 
;;;
;;;  (goal-key   <array: goal-in-cnf, goal-set-size, goal-set-unknown-size, goal-set-max-dim,
;;;      |               goal-set-tactic-replay>)
;;;      |
;;;   hash key for GOAL-STACK-DATA.
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-goal-stack-data (goal-key &key goal-in-cnf goal-set-size goal-set-unknown-size
				     goal-set-max-dim goal-set-tactic-replay)
  (let ((goal-stack-record (gethash goal-key *goal-stack-data*)))
    (assert goal-stack-record)
    (if goal-in-cnf (setf (aref goal-stack-record 0) goal-in-cnf))
    (if goal-set-size (setf (aref goal-stack-record 1) goal-set-size))
    (if goal-set-unknown-size (setf (aref goal-stack-record 2) goal-set-unknown-size))
    (if goal-set-max-dim  (setf (aref goal-stack-record 3) goal-set-max-dim))
    (if goal-set-tactic-replay (setf (aref goal-stack-record 4) goal-set-tactic-replay))
  t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUILD-GOAL-SET: Build the goal-set of cases to refute for the top goal in goal-stack.
;;;
;;; Expanded with DIVISION support on 10-Dec-2008.
;;; Note that denominators are assumed to be checked for non-nullity (PVS guarantees this).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun build-goal-set (&optional &key do-not-process-div)
  (let ((cs (drill-down 
	     (if do-not-process-div (expand-formula *g*)
	       (f-to-div-free-cnf (expand-formula *g*))))))
    (let ((num-cases (length cs)))
      (set-goal-stack-data *current-goal-key* 
			   :goal-set-size num-cases
			   :goal-set-unknown-size num-cases)
      (setf (gethash *current-goal-key* *goal-sets*)
	    (make-array `(,num-cases 3)))
      (let ((gset (gethash *current-goal-key* *goal-sets*))
	    (i 0))
	(dolist (c cs)
	  (setf (aref gset i 0) i)
	  (setf (aref gset i 1) c)
	  (setf (aref gset i 2) '(:UNKNOWN))
	  (setq i (1+ i)))
	(setq *gs* gset))
      (setq *gs-unknown-size* num-cases)
      (setq *gs-size* num-cases)
      (fmt 1 "~% >> BUILD-GOAL-SET: GOAL-SET for GOAL ~A successfully built and locally bound to *GS*." 
	   (format-goal-key *current-goal-key*))
      (prgs))))

(defun build-gs ()
  (build-goal-set))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CREATE-SUBGOAL-AND-INVOKE-PROOF-PROC (f proc): A higher-order functional for making
;;;  recursive waterfall invocations.
;;;
;;; Given a formula f (in top-level CNF, just as the formulas passed to (G ...)), and a
;;; proof procedure that operates on the local proof state (such as #'waterfall, or #'go!),
;;; do the following:
;;;
;;;   (i) Save the current proof state (done by (G ...)),
;;;   (ii) Create a new subgoal for f, named `(,*current-goal-key* ,*current-tactic-case*),
;;;   (iii) Place this new subgoal on the top of the stack (done by (G ...)),
;;;   (iv) Build a goal-set for it,
;;;   (v) Invoke proc upon it,
;;;   (vi) Return t if #'proc refutes f, otherwise save the state of this
;;;        new subgoal and swap back to the parent goal (a user can then work on this
;;;        new subgoal interactively if desired).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-subgoal-and-invoke-proof-proc (f proc &optional &key explicit-key)
  (let ((new-subgoal-key (or explicit-key `(,*current-goal-key* ,*current-tactic-case*)))
	(parent-goal-key *current-goal-key*)
	(vars-table *vars-table*))
    (g f new-subgoal-key)
    ;(build-gs) ;;; For now, we let PROC handle build-gs for us.
    (funcall proc)
    (let ((subgoal-refuted? (= *gs-unknown-size* 0)))
      (swap-to-goal parent-goal-key)
      (setq *vars-table* vars-table)
      (if subgoal-refuted? t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ** Tactics **
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONTRA-EQS: Clear simply inconsistent goals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun contra-eqs (&optional &key case from to)
  (GENERIC-TACTIC #'simply-incons* 
		  'CONTRA-EQS 
		  "simple equality reasoning"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEMOD-NUM: Numerically demodulate goals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demod-num (&optional &key case from to)
  (GENERIC-TACTIC #'demodulate-numerically
		  'DEMOD-NUM
		  "numerical demodulation"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMP-GLS: Simplify ground literals.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-gls (&optional &key case from to)
  (GENERIC-TACTIC #'simplify-ground-lits
		  'SIMP-GLS
		  "ground literal simplification"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMP-TVS: Simplify truth values.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-tvs (&optional &key case from to)
  (GENERIC-TACTIC #'remove-truth-vals*
		  'SIMP-TVS
		  "truth value simplification"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMP-ARITH: Simplify terms by simple polynomial arithmetic.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-arith (&optional &key case from to)
  (GENERIC-TACTIC #'arith-simplify-case
		  'SIMP-ARITH
		  "polynomial arithmetic simplification"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FERT-TSOS: Fertilize trivial sums of squares with PSD inequalities.
;;;  * This also finds an unsatisfiable witness if a conjunct is of the
;;;    form (< p 0) where p is a trivial square.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fert-tsos (&optional &key case from to)
  (GENERIC-TACTIC #'fertilize-trivial-squares
		  'FERT-TSOS
		  "inequality fertilization for trivial sums of squares"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; UNIV-STURM-INEQS: Use univariate sturm theory to refute systems of univariate
;;; polynomial inequalities, provided that the single variable is constrained to
;;; take values in an explicitly given open interval.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun univ-sturm-ineqs (&optional &key case from to)
  (GENERIC-TACTIC #'open-interval-univ-ineq
		  'UNIV-STURM-INEQS
		  "sturm sequence sign-change analysis for univariate open-interval systems"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPEN-CAD: Use the EX-INF-MANY quantifier relaxation for open conjunctions via QEPCAD-B.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-ex-inf-cad (&optional &key case from to)
  (GENERIC-TACTIC #'open-cad
		  'OPEN-EX-INF-CAD
		  "cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GEN-EX-CAD: Generic use of QEPCAD-B.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gen-ex-cad (&optional &key case from to)
  (GENERIC-TACTIC #'gen-cad
		  'GEN-EX-CAD
		  "generic cylindrical algebraic decomposition via QEPCAD-B"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CANON-TMS: Canonicalize all terms.  This is more expensive than SIMP-ARITH, but
;;;  does much more.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun canon-tms (&optional &key case from to)
  (GENERIC-TACTIC #'canonize-terms
		  'CANON-TMS
		  "polynomial canonicalization, arithmetic, and simplification"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ZERO-RHS: Make the RHS of all non-stable formulas zero.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-zrhs (&optional &key case from to)
  (GENERIC-TACTIC #'zero-rhs
		  'SIMP-ZRHS
		  "RHS zeroing with polynomial canonicalization"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TRIV-IDEALS: Check for trivial ideals generated by equational constraints.
;;;              (this uses reduced Groebner bases.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun triv-ideals (&optional &key case from to)
  (GENERIC-TACTIC #'trivial-ideal
		  'TRIV-IDEALS
		  "ideal triviality checking via reduced Groebner bases"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RESIDUE-CLASS-RING-INEQS: Reduce all terms in strict inequalities to their
;;;  canonical representatives in the residue class ring induced by the
;;;  ideal generated by the equational constraints in the case.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun residue-class-ring-ineqs (&optional &key case from to)
  (GENERIC-TACTIC #'ineqs-over-quotient-ring
		  'RESIDUE-CLASS-RING-INEQS
		  "reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints"
		  :case case :from from :to to))

(defun rcr-ineqs (&optional &key case from to)
  (GENERIC-TACTIC #'ineqs-over-quotient-ring
		  'RCR-INEQS
		  "reduction of terms in inequalities to canonical rep's in residue class ring induced by equational constraints"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPEN-FRAG-EX-INF-CAD: Extract the strict inequalities from a constraint and check, 
;;;  via OPEN-EX-INF-CAD, if the resulting conjunction is :UNSAT over the reals.
;;;  
;;; Note: We cannot trust :SAT answers here, as equality constraints in the conjunction 
;;;  have been ignored.  The OPEN-FRAG-CAD function takes care of this and ignores them.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-frag-ex-inf-cad (&optional &key case from to)
  (GENERIC-TACTIC #'open-frag-cad
		  'OPEN-FRAG-EX-INF-CAD
		  "fragmented cylindrical algebraic decomposition with EX-INF-MANY relaxation for open predicates via QEPCAD-B"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SIMP-REAL-NULLSTELLENSATZ: Check to see if any equational constraint is an explicit real 
;;;  nullstellensatz refutation certificate.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun simp-real-null (&optional &key case from to)
  (GENERIC-TACTIC #'simp-real-nullstellensatz
		  'SIMP-REAL-NULL
		  "extraction of simple real nullstellensatz refutation certificates from equational constraints"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RESIDUE-CLASS-RING-FERT-SCALAR-VARS: Heuristically search to see if any indeteriminates 
;;;  in the residue class ring of a case are implied to be scalar valued in that ring.  
;;;  
;;;  We do this by examining a bounded sequence of powers of each residue class ring var-
;;;  iable, and using the fact that if the pth power of a variable v is a scalar c in a res-
;;;  idue class ring, then the variable v itself is equal to the pth root of c in the res-
;;;  idue class ring (and thus in the case being examined).  
;;;
;;;  In the case of c=0 for some v^k, it follows by the property that every RCF is an 
;;;  integral domain that v=0.  In the case of c=q for some v^k with q rational, we use 
;;;  exact real arithmetic to set v = (expt q 1/k) if k is odd, and to (+/-)(expt q 1/k) 
;;;  if k is even.  If k is even, we then recursively split on these two cases, placing them
;;;  on the goal-stack and invoking a new waterfall upon the subgoal (and its two cases) 
;;;  induced by the disjunction (GENERIC-TACTIC takes care of this by recognizing the :DISJ
;;;  disjunctive waterfall signifier).
;;;
;;;  If k is odd, then we just reduce the current case to the positive exact exponent
;;;  case mentioned above.
;;;
;;;  *** Note: We currently only do this when (expt q 1/k) is rational; irrational exact
;;;       real arithmetic support isn't yet complete.  
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rcr-svars (&optional &key case from to)
  (GENERIC-TACTIC #'fertilize-scalar-vars-over-quotient-ring
		  'RCR-SVARS
		  "scalar-valued indeterminate fertilization via bounded indeterminate power sequence reduction over residue class ring induced by equational constraints"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INTEGRAL-DOMAIN-ZERO-PRODUCT-BRANCH: If any equations of the form (= (* A B) 0) for 
;;; any variables A,B in the polynomial ring exist, then we pick the first one and invoke
;;; a waterfall disjunction to branch upon (:OR (= A 0) (= B 0)).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun int-dom-zpb (&optional &key case from to)
  (GENERIC-TACTIC #'integral-domain-zero-product-branch
		  'INT-DOM-ZPB
		  "integral domain zero product branching for explicit zero indeterminate products"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DERIVE-PARTIAL-DEMOD-LINS:.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun demod-lin (&optional &key case from to)
  (GENERIC-TACTIC #'derive-partial-demod-lins
		  'DEMOD-LIN
		  "partial linear demodulator derivation and application"
		  :case case :from from :to to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WATERFALL: A heuristic procedure tying together the above tactics.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun waterfall ()
  (simp-zrhs)
  (contra-eqs)
  (demod-num)
  (simp-gls)
  (simp-tvs)
  (simp-arith)
  (simp-gls)
  (simp-tvs)
  (simp-real-null)
  (fert-tsos)
  (univ-sturm-ineqs)
  (open-ex-inf-cad)
  (triv-ideals)
  (canon-tms)
  (rcr-ineqs)
  (simp-gls)
  (simp-tvs)
  (contra-eqs)
  (fert-tsos)
  (open-frag-ex-inf-cad)
  (rcr-svars)
  (simp-gls)
  (demod-num)
  (simp-tvs)
  (simp-arith)
  (simp-gls)
  (demod-num)
  (simp-tvs)
  (simp-arith)
  (int-dom-zpb)
  (gen-ex-cad)
  (when *exact-real-arith-used*
    (fmt 1 "[NOTICE] Exact real arithmetic was used."))
  (if (= *gs-unknown-size* 0) t nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; WATERFALL-TACTICS: A copy of the waterfall structure as a list of tactics. 
;;;  This is to allow users the ability to use the :HANDS-OFF directive together 
;;;  with the (GO! ...) function to remove tactics from the waterfall.
;;;
;;; Note: I decided not to make (waterfall) just loop through evaluating these,
;;;  as (GO! ...) does if a user passes in a tactic-replay or uses :hands-off,
;;;  because that would add needless overhead for normal (waterfall) calls.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *waterfall*
  '(SIMP-ZRHS CONTRA-EQS DEMOD-NUM SIMP-GLS SIMP-TVS SIMP-ARITH SIMP-GLS SIMP-TVS 
    SIMP-REAL-NULL FERT-TSOS UNIV-STURM-INEQS OPEN-EX-INF-CAD TRIV-IDEALS CANON-TMS 
    RCR-INEQS SIMP-GLS SIMP-TVS CONTRA-EQS FERT-TSOS OPEN-FRAG-EX-INF-CAD RCR-SVARS 
    SIMP-GLS DEMOD-NUM SIMP-TVS SIMP-ARITH SIMP-GLS DEMOD-NUM SIMP-TVS SIMP-ARITH
    INT-DOM-ZPB GEN-EX-CAD))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GO!: A top-level waterfall invoker.
;;; 
;;; Keywords:
;;;
;;;  :TACTIC-REPLAY l, where l is a list of tactics that will be applied in order,
;;;  :DO-NOT-REBUILD-GS t, causes the goal-set to not be rebuilt,
;;;  :DO-NOT '(t1 ... tk), causes the listed k tactics to be removed either from
;;;    the waterfall or from the :TACTIC-REPLAY value (if one is given), with the
;;;    resulting pruned list used as if it were a :TACTIC-REPLAY parameter.
;;;  :DO-NOT-RESET-CPC t, causes the canonicalized poly cache to not be reset.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun go! (&optional &key tactic-replay reset-tactic-replay do-not-rebuild-gs do-not 
		      do-not-reset-cpc verbosity)
  (let ((*rahd-verbosity* (if verbosity verbosity *rahd-verbosity*)))
    (when (not do-not-rebuild-gs) 
      (progn 
	(build-goal-set 
	 :do-not-process-div 
	 (if (not (equal *current-goal-key* 0)) t nil)) 
	(setq *vars-table* nil)))
    (when (and *canon-poly-use-cache* (not do-not-reset-cpc))
      (clrhash *canon-poly-cache*))
    (setq *exact-real-arith-used* nil)
    (when reset-tactic-replay (setq *tactic-replay* nil))
    (if (or tactic-replay do-not)
	(let ((adj-tactic-replay
	       (remove-if #'(lambda (x) (member x do-not)) 
			  (or tactic-replay *waterfall*))))
	  (mapcar #'(lambda (tactic) (eval `(,tactic))) adj-tactic-replay))
      (waterfall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; REPEAT-UNTIL-STABLE: Given a sequence of tactics (given as a list), repeat their 
;;;  execution until the proof state is stable under their execution.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun repeat-until-stable (tactics &optional &key case from to)
  (let ((last-tactic-made-progress? t))
    (while last-tactic-made-progress?
      (setq last-tactic-made-progress? nil)
      (dolist (tac tactics)
	(eval `(,tac :case ,case :from ,from :to ,to))
	(when (and (not last-tactic-made-progress?)
		   *last-tactic-made-progress*)
	  (setq last-tactic-made-progress? t))))
  t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; GENERIC-TACTIC: A generic higher-order tactic functional
;;; that can be used to quickly create a new tactic from a 
;;; case manipulation function.
;;;
;;; fcn-case-manip : a case manipulation function
;;; fcn-symbol     : a pretty printable name for fcn-case-manip, used both
;;;                  in printing and in justification.  This must be a symbol.
;;; fcn-desc       : a nice operational description of the tactic.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun GENERIC-TACTIC (fcn-case-manip fcn-symbol fcn-desc &optional &key case from to)
  (cond ((= *gs-unknown-size* 0) (progn (setq *last-tactic-made-progress* nil) t))
	(t (let* ((num-changed 0)
		  (num-refuted 0)
		  (start-time (get-internal-real-time))
		  (case-lb* (if case case (if from from 0)))
		  (case-ub* (if case case (if to to (1- *gs-size*))))
		  (case-lb (max case-lb* 0))
		  (case-ub (min case-ub* (1- *gs-size*))))
	     (when (or case from to) 
	       (fmt 1 "~% >> ~D: *** Thanks for the hint!  ~D is being applied only to cases in the range [~D...~D]." 
		    fcn-symbol fcn-symbol case-lb case-ub))
	     (loop for i from case-lb to case-ub do
		   (setq *current-tactic-case* i) ; Used for naming spawned subgoals.
		   (let ((c        (aref *gs* i 1))
			 (c-status (aref *gs* i 2)))
		     (if (equal (car c-status) ':UNKNOWN)
			 (if (not c) 

			     ;;
			     ;; The current case has been reduced to an empty conjunction 
			     ;; -- an implicit t -- Thus, we've found a counter-example.
			     ;;

			     (setf (aref *gs* i 2) `(:SAT :CASE-REDUCED-TO-EMPTY-CONJUNCTION ,(cdr c-status)))
			   (let ((fcn-result (funcall fcn-case-manip c)))
			     (if (not (equal c fcn-result)) ; The current tactic actually did something.
				 (if (consp fcn-result)
				     (case (car fcn-result)

				       (:UNSAT 
					(setf (aref *gs* i 2)
					      `(:UNSAT ,(append (cdr c-status) (cons fcn-symbol (cdr fcn-result)))))
					(setq num-refuted (1+ num-refuted))
					(fmt 2 "!"))

				       (:SAT 
					(setf (aref *gs* i 2)
					      `(:SAT nil ,(append (cdr c-status) (cons fcn-symbol (cdr fcn-result)))))
					(fmt 0 " >> COUNTER-EXAMPLE: CASE ~D of GOAL ~A is satisfiable." 
					     i (format-goal-key *current-goal-key*))
					(fmt 2 "@")
					(when *rahd-debug* 
					  (fmt 0 " >> Press RET to continue.")
					  (read-line t)))

				       (:DISJ
					(let ((new-subgoal-key `(,*current-goal-key* ,i))
					      (new-subgoal-formula (waterfall-disj-to-cnf (cdr fcn-result))))
					  (setf (aref *gs* i 2) `((:UNKNOWN-WITH-SPAWNED-SUBGOAL ,new-subgoal-key) 
								  ,(append (cdr c-status) `(,fcn-symbol))))
					  (fmt 1 "~% ::>> Waterfall disjunction: Spawning SUBGOAL ~A as a sufficient condition for CASE ~A of GOAL ~A.~%"
					       (format-goal-key new-subgoal-key) 
					       i
					       (format-goal-key *current-goal-key*))
					  (let ((result-of-waterfall-on-subgoal
						 (create-subgoal-and-invoke-proof-proc new-subgoal-formula
										       #'go!
										       :explicit-key new-subgoal-key)))
					    (if result-of-waterfall-on-subgoal
						(progn
						  (fmt 1 "~% ::>> Waterfall disjunction: SUBGOAL ~A for GOAL ~A successfully discharged, thus discharging CASE ~A of GOAL ~A.~%"
						       (format-goal-key new-subgoal-key) 
						       (format-goal-key *current-goal-key*)
						       i
						       (format-goal-key *current-goal-key*))
						  (setq num-refuted (1+ num-refuted))
						  (setf (aref *gs* i 2)
							`(:UNSAT :DISCHARGED-BY-SUBGOAL ,new-subgoal-key
								 ,(append (cdr c-status) (cons fcn-symbol (list (cdr fcn-result)))))))
					      
					      ;;
					      ;; If the subgoal isn't automatically discharged, we'll just leave it for the user to attack manually,
					      ;; and we will leave the formula for the current case unchanged.
					      ;;

					      (setq fcn-result c)

					      ))))
					  
				       (otherwise 
					(setf (aref *gs* i 1) fcn-result)
					(setf (aref *gs* i 2) (append c-status `(,fcn-symbol)))
					(setq num-changed (1+ num-changed))
					(fmt 2 "$"))))

			       ;;; Tactic execution on case i did nothing, so we print `.' at verbosity level 2.

			       (fmt 2 ".")))))))

	     (if (or (> num-changed 0) (> num-refuted 0))
		 (progn 
		   (setq *last-tactic-made-progress* t)
		   (setq *tactic-replay* (cons fcn-symbol *tactic-replay*))
		   (setq *gs-unknown-size* (- *gs-unknown-size* num-refuted))
		   (let ((fcn-str (write-to-string fcn-symbol)))
		     (fmt 1 "~% >> ~D: ~A :UNKNOWN cases successfully refuted by ~A." fcn-symbol num-refuted fcn-desc)
		     (fmt 1 "~% >> ~D: ~A :UNKNOWN cases successfully reduced (but not refuted) by ~A." fcn-symbol num-changed fcn-desc)
		     (fmt 1 "~% >> ~D: Tactic execution completed in approximately ~D seconds." 
			  fcn-symbol (float (/ (- (get-internal-real-time) start-time) internal-time-units-per-second)))
		     (prgs)))
	       (progn 
		 (setq *last-tactic-made-progress* nil)
		 (fmt 2 "~%~%")
		 (fmt 3 "*** Tactic executed but did not make progress: ~D ~%" (write-to-string fcn-symbol)) 
		 t))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PUG: Print :UNKNOWN Goals reamining on GOAL-STACK.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pug (&optional bound)
  (if (> *gs-unknown-size* 0)
      (progn 
	(fmt 0 "~% >> PUG: Print goals in GOAL-SET (GOAL ~A) marked :UNKNOWN (awaiting refutation). ~%         Printing ~D of the remaining ~R goals.~%" 
		(format-goal-key *current-goal-key*)
		(if (not bound) "all" (format nil "the first ~R" bound)) *gs-unknown-size*)
	(fmt 0 "~% -------     -------------------------------------------------------")
	(fmt 0 "~% CASE-ID     CASE")
	(fmt 0 "~% -------     -------------------------------------------------------~%")
	(let ((num-printed 1))
	  (dotimes (i *gs-size*)
	    (let ((c-id     (aref *gs* i 0))
		  (c        (aref *gs* i 1))
		  (c-status (aref *gs* i 2)))
	      (if (and (or (equal (car c-status) ':UNKNOWN)
			   (and (consp (car c-status))
				(equal (caar c-status) ':UNKNOWN-WITH-SPAWNED-SUBGOAL)))
		       (or (not bound)
			   (<= num-printed bound)))
		  (progn (fmt 0 "~% ~7D     ~D    ~D ~%" 
			      c-id 
			      c
			      c-status)
			 (setq num-printed (1+ num-printed)))
		t))))
	(fmt 0 "~%~% >> PUG: Printing complete. ~%~%")))

  ;; If a user invokes (PUG) at any verbosity level, it's clear we should visibly (PRGS).
  ;; So, we dynamically bind *RAHD-VERBOSITY* to 1 for the call below.

  (let ((*rahd-verbosity* 1)) (prgs)))


;;; A short (PUG ...) alias: (P ...).

(defun p (&optional bound) (pug bound))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Print remaing goals status (done at the end of every tactic output).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prgs ()
  (cond ((equal *g* nil)
	 (fmt 1 "~%~%         *** No goal is currently installed.  Use (G <goal-in-cnf>) to do so. ***~%~%"))
	((<= *gs-size* 0)
	 (fmt 1 "~%~%         *** No cases installed in GOAL-SET (*GS*).  Use (BUILD-GS) to do so. *** ~%~%"))
	(t (fmt 1 "~%~%         ***~6D cases remain in GOAL-SET (GOAL ~A) awaiting refutation      ***~%~%" 
		*gs-unknown-size* 
		(format-goal-key *current-goal-key*))
	   (if *exact-real-arith-used*
	       (fmt 1 "~%~%     *** NOTE: EXACT REAL ARITHMETIC HAS BEEN USED ***~%~%"))
	   (if (= *gs-unknown-size* 0)
	       (fmt 1 "~%~%                  *** THEOREM (GOAL ~A) PROVED ***~%~%" 
		    (format-goal-key *current-goal-key*)) t)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Print the tactic replay of the current session.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun tactic-replay ()
  (reverse *tactic-replay*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Extract non-refuted goals (both those marked `:UNKNOWN' or `:SAT') together with 
;;; their possible witnesses in the following form:
;;;
;;;     `(  (FORMULA_i  WITNESS_i) ).
;;;
;;; This is for PVS integration.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun extract-non-refuted-cases ()
  (let ((nrc-lst nil))
    (dotimes (i *gs-size*)
      (let ((c        (aref *gs* i 1))
	    (c-status (aref *gs* i 2)))
	(let ((c-mark (car c-status)))
	  (if (or (eq c-mark ':UNKNOWN)
		  (eq c-mark ':SAT)
		  (and (consp c-mark)
		       (eq (car c-mark) ':UNKNOWN-WITH-SPAWNED-SUBGOAL)))
	      (let ((nrc-case
		     (cons c (if (equal c-mark ':SAT) (cadr c-status) nil))))
		(setq nrc-lst (cons nrc-case nrc-lst)))))))
    nrc-lst))

