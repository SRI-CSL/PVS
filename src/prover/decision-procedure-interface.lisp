(in-package :pvs)

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
  (list 'shostak 'ics))

(defvar *decision-procedure-descriptions*
  '((ics . "ICS (alpha)")))

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
  `(dpi-canon* *current-decision-procedure* ,term ,state))


;;; Initialization - invoked from init-pvs.

(defun initialize-decision-procedures ()
  (dolist (*current-decision-procedure* *decision-procedures*)
    (dpi-init)))

(defmethod dpi-init* (dp)
  (setq *decision-procedures* (delete dp *decision-procedures*))
  (pvs-message
      "Decision procedure ~a is unknown and removed from *decision-procedures*"
    dp))

;;; This is how we can test for the existence of particular eql methods
;; (find-method #'dpi-start* nil (list (intern-eql-specializer 'shostak)))

;;; The generic versions - leave out those that must be specialized

(defmethod dpi-end* (dp proofstate)
  (declare (ignore dp proofstate)))

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
	 ;;(*prtype-hash* (init-if-rec *prtype-hash*))
	 ;;(*local-prtype-hash* (init-if-rec *local-prtype-hash*))
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


;;; ICS interface

(defmethod dpi-init* ((dp (eql 'ics)))
  (ics-init nil))

(defmethod dpi-start* ((dp (eql 'ics)) prove-body)
  (pvs-to-ics-reset)
  (funcall prove-body))

(defmethod dpi-end* ((dp (eql 'ics)) proofstate)
  (declare (ignore proofstate))
  (pvs-to-ics-reset)
  (ics_reset))

(defmethod dpi-empty-state* ((dp (eql 'ics)))
  (ics-empty-state))

(defmethod dpi-process* ((dp (eql 'ics)) (pvs-expr expr) state)
  (let ((result (ics-process state pvs-expr)))
    (cond ((eq result :unsat)
	   (values *false* state))
	  ((eq result :valid)
	   (values *true* state))
	  (t
	   (values nil result)))))

(defmethod dpi-process* ((dp (eql 'ics)) ics-expr state)
  (break "Hypothesis: only called when ICS returns disjunction"))
	 
(defmethod dpi-valid?* ((dp (eql 'ics)) state (pvs-expr expr))
  (ics-is-valid state pvs-expr))

(defmethod dpi-push-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-pop-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-copy-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-restore-state* ((dp (eql 'ics)) state)
  state)

(defmethod dpi-state-changed?* ((dp (eql 'ics)) old-state new-state)
  (ics-state-unchanged? old-state new-state))

;;; Comparison interface


(defmethod dpi-init* ((dp (eql 'shostak-and-ics)))
  (dpi-init* 'shostak)
  (dpi-init* 'ics))

(defmethod dpi-start* ((dp (eql 'shostak-and-ics)) (prove-body function))
  ;; This one does a let and invokes prove-body, so we can't call both
  (pvs-to-ics-reset)
  (dpi-start* 'shostak prove-body))

(defmethod dpi-empty-state* ((dp (eql 'shostak-and-ics)))
  (cons (dpi-empty-state* 'shostak)
	(dpi-empty-state* 'ics)))

(defmethod dpi-process* ((dp (eql 'shostak-and-ics)) expr state)
  (multiple-value-bind (shostak-result shostak-nstate)
      (dpi-process* 'shostak expr (car state))
    (multiple-value-bind (ics-result ics-nstate)
	(dpi-process* 'ics expr (cdr state))
      (if (tc-eq shostak-result ics-result)
	  (format t "~%ICS-SHOSTAK dpi-process*: same results on~%  ~a:~%    Result: ~a"
	    expr shostak-result)
	  (format t "~%ICS-SHOSTAK-DIFFERENCE dpi-process*: shostak and ICS differ on~%  ~a:~%    Shostak: ~a~%        ICS: ~a"
	    expr shostak-result ics-result))
      (values shostak-result (cons shostak-nstate ics-nstate)))))

(defmethod dpi-valid?* ((dp (eql 'shostak-and-ics)) state pvs-expr)
  (let ((shostak-result (dpi-valid?* 'shostak (car state) pvs-expr))
	(ics-result (dpi-valid?* 'ics (cdr state) pvs-expr)))
    (unless (tc-eq shostak-result ics-result)
      (format t "~%ICS-SHOSTAK-DIFFERENCE dpi-valid?* shostak and ICS differ on~%  ~a:~%    Shostak: ~a~%        ICS: ~a"
	pvs-expr shostak-result ics-result))
    shostak-result))

(defmethod dpi-push-state* ((dp (eql 'shostak-and-ics)) state)
  (cons (dpi-push-state* 'shostak (car state))
	(dpi-push-state* 'ics (cdr state))))

(defmethod dpi-pop-state* ((dp (eql 'shostak-and-ics)) state)
  state)

(defmethod dpi-copy-state* ((dp (eql 'shostak-and-ics)) state)
  (cons (dpi-copy-state* 'shostak (car state))
	(dpi-copy-state* 'ics (cdr state))))

(defmethod dpi-restore-state* ((dp (eql 'shostak-and-ics)) state)
  state)

(defmethod dpi-state-changed?* ((dp (eql 'shostak-and-ics)) old-state new-state)
  (let ((shostak-result (dpi-state-changed?* 'shostak
					     (car old-state) (car new-state)))
	(ics-result (dpi-state-changed?* 'ics
					 (cdr old-state) (cdr new-state))))
    (if (eq shostak-result ics-result)
	(format t "~%ICS-SHOSTAK dpi-state-changed?* no difference: result = ~a"
	  shostak-result)
	(format t "~%ICS-SHOSTAK-DIFFERENCE dpi-state-changed?* shostak and ICS differ:~% Shostak: ~a~%     ICS: ~a"
	  shostak-result ics-result))
    shostak-result))
