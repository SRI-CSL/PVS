(in-package 'pvs)

;;; The following macros allow new decision procedures to be plugged in,
;;; as long as they provide corresponding methods (with the same name with
;;; a '*' siffix, and an additional eql argument to specify the decision
;;; procedure) for each of the macros.

;;; dpi-init initializes the given dp; it is invoked by pvs-init at the
;;;   beginning of the PVS session.
;;; dpi-start sets up LETs for the given dp and funcalls the PROVE-BODY
;;;   argument within the LET.  It is invoked by prove-decl at the start
;;;   of each proof.
;;; dpi-empty-state takes no arguments and returns a state.  It is invoked
;;;   by prove-decl and pseudo-normalize.  It should satisfy
;;;      (not (dpi-state-changed? (dpi-empty-state) (dpi-empty-state)))
;;; dpi-process takes a state and a term and returns
;;;    a result and a new state.  The result is either true, false, or a
;;;    list of terms whose conjunction is equivalent to the original term.
;;;    Note that the result terms are NOT pvs terms.  There are usually
;;;    two methods defined for dpi-process*, one for PVS expressions, and
;;;    one for decision-procedure terms.  This is invoked by call-process,
;;;    which in turn is invoked in many places in assert.lisp, and by
;;;    skolem-step.  Note that pseudo-normalize invokes
;;;    assert-if-simplify, which in turn may invoke call-process.
;;; dpi-valid? takes a state and a term and returns a result.  This is
;;;    similar to dpi-process but is invoked at the end of 
;;; dpi-restore-state takes a state and returns a state
;;; dpi-copy-state takes a state and returns a copy of that state.

(defvar *current-decision-procedure* nil)

(defvar *default-decision-procedure* 'shostak)

(defvar *decision-procedures*
  (list 'shostak 'cyrluk 'ics))

(defmacro dpi-init ()
  `(dpi-init* *current-decision-procedure*))

(defmacro dpi-start (prove-body)
  `(dpi-start* *current-decision-procedure* ,prove-body))

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

(defmacro dpi-end (proofstate)
  `(dpi-end* *current-decision-procedure* ,proofstate))

(defmacro dpi-disjunction? (term)
  `(dpi-disjunction?* *current-decision-procedure* ,term))

(defmacro dpi-proposition? (term)
  `(dpi-proposition?* *current-decision-procedure* ,term))

(defmacro dpi-term-arguments (term)
  `(dpi-term-arguments* *current-decision-procedure* ,term))

(defmacro dpi-canon (term state)
  `(dpi-canon* *current-decision-procedure* ,term))


;;; Initialization - invoked from init-pvs.

(defun initialize-decision-procedures ()
  (dolist (*current-decision-procedure* *decision-procedures*)
    (dpi-init)))

;;; This is how we can test for the existence of particular eql methods
;; (find-method #'dpi-start* nil (list (intern-eql-specializer 'shostak)))

;;; The generic versions - leave out those that must be specialized

(defmethod dpi-end* (dp proofstate)
  )

;;; Now define the interfaces for the individual decision procedures

(defmethod dpi-init* ((dp (eql 'shostak)))
  )

(defmethod dpi-start* ((dp (eql 'shostak)) (prove-body function))
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

(defmethod dpi-empty-state* ((dp (eql 'shostak)))
  (make-instance 'dpinfo
    'dpinfo-sigalist nil
    'dpinfo-findalist nil
    'dpinfo-usealist *init-usealist*))

(defmethod dpi-process* ((dp (eql 'shostak)) (pvs-expr expr) state)
  (let* ((sigalist (dpinfo-sigalist state))
	 (findalist (dpinfo-findalist state))
	 (usealist (dpinfo-usealist state))
	 (dp-expr (top-translate-to-old-prove pvs-expr))
	 (typealist (append *local-typealist* typealist))
	 (result (translate-from-prove-list (invoke-process dp-expr)))
	 (nstate (if (and (eq (dpinfo-sigalist state) sigalist)
			  (eq (dpinfo-findalist state) findalist)
			  (eq (dpinfo-usealist state) usealist))
		     state
		     (make-instance 'dpinfo
		       'dpinfo-sigalist sigalist
		       'dpinfo-findalist findalist
		       'dpinfo-usealist usealist))))
    (values result nstate)))

(defmethod dpi-process* ((dp (eql 'shostak)) dp-expr state)
  (let* ((sigalist (dpinfo-sigalist state))
	 (findalist (dpinfo-findalist state))
	 (usealist (dpinfo-usealist state))
	 (typealist (append *local-typealist* typealist))
	 (result (translate-from-prove-list (invoke-process dp-expr)))
	 (nstate (if (and (eq (dpinfo-sigalist state) sigalist)
			  (eq (dpinfo-findalist state) findalist)
			  (eq (dpinfo-usealist state) usealist))
		     state
		     (make-instance 'dpinfo
		       'dpinfo-sigalist sigalist
		       'dpinfo-findalist findalist
		       'dpinfo-usealist usealist))))
    (values result nstate)))

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

(defmethod dpi-disjunction?* ((dp (eql 'shostak)) term)
  (and (consp term)
       (eq (car term) 'or)))

(defmethod dpi-proposition?* ((dp (eql 'shostak)) term)
  (and (consp term)
       (memq (car term) '(if if* implies not and iff))))

(defmethod dpi-term-arguments* ((dp (eql 'shostak)) term)
  (when (consp term) (cdr term)))


;;; Dave Cyrluk's decision procedure

(defmethod dpi-init* ((dp (eql 'cyrluk)))
  (dp::init-dp-0 t)
  (dp::return-all-cong-states dp::*made-cong-states*)
  (reset-translate-from-dc)
  (reset-translate-to-dc))

(defmethod dpi-start* ((dp (eql 'cyrluk)) prove-body)
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

(defmethod dpi-empty-state* ((dp (eql 'cyrluk)))
  (dp::null-single-cong-state))

(defmethod dpi-process* ((dp (eql 'cyrluk)) (pvs-expr expr) state)
  (let* ((dp-expr (top-translate-to-dc pvs-expr))
	 (result (translate-from-dc (dp::invoke-process dp-expr state))))
    (values result state)))

(defmethod dpi-process* ((dp (eql 'cyrluk)) dp-expr state)
  (let ((result (translate-from-dc (dp::invoke-process dp-expr state))))
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

(defmethod dpi-end* ((dp (eql 'cyrluk)) proofstate)
  (let* ((dp-state (dp-state proofstate))
	 (dp-stack (dp::cong-state-stack dp-state))
	 (done-subgoals (done-subgoals proofstate)))
    (loop for ps in done-subgoals
	  for subgoal-dp-state = (dp-state ps)
	  for subgoal-stack = (dp::cong-state-stack subgoal-dp-state)
	  unless (eq dp-stack subgoal-stack)
	  do (dp::npop-cong-state subgoal-dp-state))))

(defmethod dpi-disjunction?* ((dp (eql 'cyrluk)) term)
  (break))

(defmethod dpi-proposition?* ((dp (eql 'cyrluk)) term)
  (break))

(defmethod dpi-term-arguments* ((dp (eql 'cyrluk)) term)
  (break))


;;; ICS interface

(defmethod dpi-init* ((dp (eql 'ics)))
  (ics-init 1))

(defmethod dpi-start* ((dp (eql 'ics)) prove-body)
  (let ((*pvs-to-ics-hash* (init-if-rec *pvs-to-ics-hash*))
	(*ics-to-pvs-hash* (init-if-rec *ics-to-pvs-hash*))
	(*ics-types-to-tags-hash* (init-if-rec *ics-types-to-tags-hash*))
	(*ics-tags-to-types-hash* (init-if-rec *ics-tags-to-types-hash*))
	(*ics-tags-to-types-counter* 0))
    (funcall prove-body)))

(defmethod dpi-end* ((dp (eql 'ics)) proofstate)
  (declare (ignore proofstate))
  (clrhash *pvs-to-ics-hash*)
  (clrhash *ics-to-pvs-hash*)
  (clrhash *ics-types-to-tags-hash*)
  (clrhash *ics-tags-to-types-hash*)
  (ics_reset))

(defmethod dpi-empty-state* ((dp (eql 'ics)))
  (ics-empty-state))

(defmethod dpi-process* ((dp (eql 'ics)) (pvs-expr expr) state)
  (let* ((ics-expr (translate-to-ics pvs-expr))
	 (ics-value (ics-process state ics-expr))
	 (new-state nil))
    ;;(format t "~%Processing ~a~%" pvs-expr)
    ;;(ics_pp_state state)
    ;;(ics_flush)
    (cond ((not (zerop (ics_is_consistent ics-value)))
	   (values *true* (ics-d-consistent ics-value)))
	  ((not (zerop (ics_is_redundant ics-value)))
	   (values nil state))
	  (t (values *false* state)))))

(defmethod dpi-process* ((dp (eql 'ics)) ics-expr state)
  (let* ((ics-value (ics_process state ics-expr))
	 (new-state nil))
    ;;(format t "~%Processing ~a~%" pvs-expr)
    ;;(ics_pp_state state)
    ;;(ics_flush)
    (cond ((not (zerop (ics_is_consistent ics-value)))
	   (values *true* (ics-d-consistent ics-value)))
	  ((not (zerop (ics_is_redundant ics-value)))
	   (values nil state))
	  (t (values *false* state)))))

(defmethod dpi-valid?* ((dp (eql 'ics)) state pvs-expr)
  (not (zerop (ics_is_valid state (ics_process state ics-expr)))))

(defmethod dpi-push-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-pop-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-copy-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-restore-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-state-changed?* ((dp (eql 'ics)) old-state new-state)
  (not (eq old-state new-state)))

(defmethod dpi-canon* ((dp (eql 'ics)) term state)
  (ics-canon state term))
