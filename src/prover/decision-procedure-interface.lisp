(in-package 'pvs)

;;; The name for this decision procedure
;;; init is a function taking no arguments and producing no useful result
;;; Used to initialize the dp
;;; reset is like init, but may be faster
;;; empty-state takes no arguments and returns a state
;;; process takes a state and a term and returns two values:
;;;    a new state and a result.
;;; valid? takes a state and a term and returns a result
;;; restore-state takes a state and returns a state
;;; copy-state takes a state and returns a copy of that state.

(defvar *current-decision-procedure* nil)

(defvar *default-decision-procedure* 'shostak)

(defvar *decision-procedures*
  (list 'shostak 'cyrluk 'ics))

(defmacro dpi-init (prove-body)
  `(dpi-init* *current-decision-procedure* ,prove-body))

(defmacro dpi-reset ()
  `(dpi-reset* *current-decision-procedure*))

(defmacro dpi-empty-state ()
  `(dpi-empty-state* *current-decision-procedure*))

(defmacro dpi-process (dp-expr state)
  `(dpi-process* *current-decision-procedure* ,dp-expr ,state))

(defmacro dpi-valid? (dp-expr state)
  `(dpi-translate-from* *current-decision-procedure* ,dp-expr ,state))

(defmacro dpi-push-state (state)
  `(dpi-push-state* *current-decision-procedure* ,state))

(defmacro dpi-pop-state (state)
  `(dpi-pop-state* *current-decision-procedure* ,state))

(defmacro dpi-copy-state (state)
  `(dpi-copy-state* *current-decision-procedure* ,state))

(defmacro dpi-state-changed? (old-state new-state)
  `(dpi-state-changed?* *current-decision-procedure* ,old-state ,new-state))

(defmacro dpi-cleanup-proof (proofstate)
  `(dpi-cleanup-proof* *current-decision-procedure* ,proofstate))

;;; This is how we can test for the existence of particular eql methods
;; (find-method #'dpi-reset* nil (list (intern-eql-specializer 'shostak)))

;;; The generic versions - leave out those that must be specialized

(defmethod dpi-cleanup-proof* (dp proofstate)
  )

;;; Now define the interfaces for the individual decision procedures

(defmethod dpi-init* ((dp (eql 'shostak)) (prove-body function))
  (let* ((*translate-id-counter* nil)
	 (*translate-id-hash* (init-if-rec *translate-id-hash*))
	 (*translate-to-prove-hash* (init-if-rec *translate-to-prove-hash*))
	 (typealist primtypealist)
	 (*subtype-names* nil)
	 (*named-exprs* nil)
	 (*rec-type-dummies* nil)
	 (*prtype-hash* (init-if-rec *prtype-hash*))
	 (*local-prtype-hash* (init-if-rec *local-prtype-hash*))
	 (*local-typealist* *local-typealist*)
	 (applysymlist nil)
	 (sigalist sigalist)
	 (usealist usealist)
	 (findalist findalist))
    (initprover)
    (newcounter *translate-id-counter*)
    (funcall prove-body)))

(defmethod dpi-reset* ((dp (eql 'shostak)))
  )

(defmethod dpi-empty-state* ((dp (eql 'shostak)))
  (make-instance 'dpinfo
    'dpinfo-sigalist nil
    'dpinfo-findalist nil
    'dpinfo-usealist *init-usealist*))

(defmethod dpi-process* ((dp (eql 'shostak)) pvs-expr state)
  (let* ((sigalist (dpinfo-sigalist state))
	 (findalist (dpinfo-findalist state))
	 (usealist (dpinfo-usealist state))
	 (dp-expr (top-translate-to-old-prove pvs-expr))
	 (typealist (append *local-typealist* typealist))
	 (result (translate-from-prove-list (invoke-process dp-expr))))
    (setf (dpinfo-sigalist state) sigalist
	  (dpinfo-findalist state) findalist
	  (dpinfo-usealist state) usealist)
    (values result state)))

(defmethod dpi-valid?* ((dp (eql 'shostak)) state pvs-expr)
  (let* ((sigalist (dpinfo-sigalist state))
	 (findalist (dpinfo-findalist state))
	 (usealist (dpinfo-usealist state))
	 (dp-expr (top-translate-to-old-prove pvs-expr))
	 (typealist (append *local-typealist* typealist)))
    (translate-from-prove-list (invoke-process dp-expr))))

(defmethod dpi-push-state* ((dp (eql 'shostak)) state)
  (copy state))

(defmethod dpi-pop-state* ((dp (eql 'shostak)) state)
  state)

(defmethod dpi-copy-state* ((dp (eql 'shostak)) state)
  (make-instance 'dpinfo
    'dpinfo-sigalist (dpinfo-sigalist state)
    'dpinfo-findalist (dpinfo-findalist state)
    'dpinfo-usealist (dpinfo-usealist state)))

(defmethod dpi-restore-state* ((dp (eql 'shostak)) state)
  state)

(defmethod dpi-state-changed?* ((dp (eql 'shostak)) old-state new-state)
  (not (and (eq (dpinfo-usealist old-state)
		(dpinfo-usealist new-state))
	    (eq (dpinfo-findalist old-state)
		(dpinfo-findalist new-state))
	    (eq (dpinfo-sigalist old-state)
		(dpinfo-sigalist new-state)))))

;;; Dave Cyrluk's decision procedure

(defmethod dpi-init* ((dp (eql 'cyrluk)) prove-body)
  (let* ((*dc-named-exprs* (init-if-rec *dc-named-exprs*))
	 (*translate-to-dc-hash* (init-if-rec *translate-to-dc-hash*))
	 (*dc-translate-id-hash* (init-if-rec *dc-translate-id-hash*))
	 (*dc-translate-id-counter* nil)
	 (*newdc* t))
    (newcounter *dc-translate-id-counter*)
    (dp::init-dp-0)
    (dp::return-all-cong-states dp::*made-cong-states*)
    (reset-translate-from-dc)
    (reset-translate-to-dc)
    (funcall prove-body)))

(defmethod dpi-reset* ((dp (eql 'cyrluk)))
  (dp::init-dp-0 t)
  (dp::return-all-cong-states dp::*made-cong-states*)
  (reset-translate-from-dc)
  (reset-translate-to-dc))

(defmethod dpi-empty-state* ((dp (eql 'cyrluk)))
  (dp::null-single-cong-state))

(defmethod dpi-process* ((dp (eql 'cyrluk)) pvs-expr state)
  (let* ((dp-expr (top-translate-to-dc pvs-expr))
	 (result (translate-from-dc (dp::invoke-process dp-expr state))))
    (values result state)))

(defmethod dpi-valid?* ((dp (eql 'cyrluk)) state pvs-expr)
  (let* ((dp-expr (top-translate-to-dc pvs-expr))
	 (result (translate-from-dc (dp::invoke-process dp-expr state))))
    result))

(defmethod dpi-push-state* ((dp (eql 'cyrluk)) state)
  (dp::push-new-cong-state state))

(defmethod dpi-pop-state* ((dp (eql 'cyrluk)) state)
  (dp::npop-cong-state state))

(defmethod dpi-copy-state* ((dp (eql 'cyrluk)) state)
  (dp::copy-cong-state state))

(defmethod dpi-restore-state* ((dp (eql 'cyrluk)) state)
  (dp::npop-cong-state state))

(defmethod dpi-state-changed?* ((dp (eql 'cyrluk)) old-state new-state)
  (dp::dp-changed old-state new-state))

(defmethod dpi-cleanup-proof* ((dp (eql 'cyrluk)) proofstate)
  (let* ((dp-state (dp-state proofstate))
	 (dp-stack (dp::cong-state-stack dp-state))
	 (done-subgoals (done-subgoals proofstate)))
    (loop for ps in done-subgoals
	  for subgoal-dp-state = (dp-state ps)
	  for subgoal-stack = (dp::cong-state-stack subgoal-dp-state)
	  unless (eq dp-stack subgoal-stack)
	  do (dp::npop-cong-state subgoal-dp-state))))

;;; ICS interface

(defmethod dpi-init* ((dp (eql 'ics)) prove-body)
  (let* ()
    (funcall prove-body)))

(defmethod dpi-reset* ((dp (eql 'ics)))
  )

(defmethod dpi-empty-state* ((dp (eql 'ics)))
  )

(defmethod dpi-process* ((dp (eql 'ics)) pvs-expr state)
  )

(defmethod dpi-valid?* ((dp (eql 'ics)) state pvs-expr)
  )

(defmethod dpi-push-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-pop-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-copy-state* ((dp (eql 'ics)) state)
  )

(defmethod dpi-restore-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-state-changed?* ((dp (eql 'ics)) old-state new-state)
  (alists-changed old-state new-state))

