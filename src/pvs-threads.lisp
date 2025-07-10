;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-threads.lisp -- code for using threads in PVS
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;;; Sessions are a representation of REPLS running on threads in PVS.
;;; The prover is the primary target, but PVSio should also work.

;;; Sessions have an id, input and output queues, and a pointer to the REPL
;;; thread. Once started, the thread is expected to read from the
;;; input-queue and push results to the outputs slot. On the next read, the
;;; outputs slot will be used to construct the output for the output-queue.
;;; On exit, one final entry will be pushed to the output-queue, and the
;;; session will wait till it's been read before exiting.

;;; This is supported with functions from the lparallel.queue package, which
;;; has the advantage that it has a pop that does a non-busy wait.

;;; One problem with threads is that by default they all share
;;; *standard-input* and *standard-output*. This gets very confusing,
;;; as threads will tend to alternate control of these streams.

;;; API:
;;;  session - class
;;;  session-state - class
;;;  (make-session id repl-fun input-fun output-fun final-fun interrupt-fun)
;;;    id should be new, and prove-formula is an example of a repl-fun.
;;;    
;;;    Returns the outputs of the repl-fun.
;;;  (step-session id step)      ;; step is expected input

;;; API:
;;;  (make-proof-session formula) returns an id and initial proofstate
;;;  (pr-step id cmd) returns the proofstate or error
;;;  (pr-interrupt id &optional (after :restore)) ; :quit :break
;;;

(declaim (ftype (function () t) in-the-debugger))
(declaim (ftype (function (&optional t))  iso8601-date))
(declaim (ftype (function (t) t)
		all-formulas get-formula-decl provable-formulas quit-command-p
		pvs-jsonrpc:pvs2json-response))
(declaim (ftype (function (t t) t) unique-symbol*))
(declaim (ftype (function (t &optional t) t) write-to-temp-file))
;; (declaim (ftype (function (t &key (t t)) t) prove-formula))
;; (declaim (ftype (function (t &optional t t) t) get-typechecked-theory))
(defgeneric get-typechecked-theory (tref &optional theories quiet?))
	  
;;;  proof-session: class

(defcl session ()
  id
  ;;stdout
  thread
  input-queue
  output-queue
  outputs
  logs)

;; Need to be able to return the following, note that only the commentary is
;; not derivable from the ps.
;;  commentary - *prover-commentary*, which is collected as the prover runs
;;  action  - (format-printout (parent-proofstate ps))
;;  num-subgoals - (proofstate-num-subgoals ps)
;;  label - (label ps)
;;  commend - (commend ps)
;;  sequent - (pvs2json-seq current-goal (parent-proofstate ps))

(defmethod print-object ((ses session) stream)
  (format stream "#<session ~a>" (id ses)))

(defvar *all-sessions* nil
  "list of sessions")

(defvar *done-sessions* nil
  "An alist of (id . status) pairs, status is quit, proved, or aborted")

(defvar *multiple-proof-default-behavior* :ask
  "Defines how to handle when a proof finishes:
 :ask = asks whether to save the proof, and if yes, whether to overwrite, etc.
 :noquestions = no questions, automatically overwrites if the proof is different
 :overwrite = same as :noquestions, but sys that it is overwriting")

(defvar *pvs-message-hook* nil)
(defvar *pvs-warning-hook* nil)
(defvar *pvs-buffer-hook* nil)
(defvar *pvs-y-or-n-hook* nil)
(defvar *pvs-query-hook* nil)
(defvar *pvs-dialog-hook* nil)
(defvar *prover-input-hooks* nil)

(defun clear-pvs-hooks ()
  (setq *pvs-message-hook* nil
	*pvs-warning-hook* nil
	*pvs-buffer-hook* nil
	*pvs-y-or-n-hook* nil
	*pvs-query-hook* nil
	*pvs-dialog-hook* nil))

(defvar *pvs-emacs-interface* nil
  "Set to t by Emacs in pvs-load - affects how pvs-emacs functions work")

;;; The primary method (i.e., call-next-method) simply prints the proofstate in the
;;; *pvs* buffer.  This around method allows other displays, currently Emacs and
;;; Websocket clients

(defvar *pvs-emacs-output-proofstate-p* nil
  "Set to t to try the Emacs frame interface - stiil in progress.")

(defun make-session (id fun)
  (let ((sess
	 (make-instance 'session
	   :id id
	   ;; :stdout (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t)
	   :input-queue (make-queue)
	   :output-queue (make-queue)))
	;; Make sure we're in the PVS package on this thread
	(bt:*default-special-bindings* '((*package* . (find-package :pvs)))))
    ;; Make sure the session is available before starting the thread
    (push sess *all-sessions*)
    (setf (thread sess) (bt:make-thread fun :name (string id)))
    sess))

(defun session-alive-p (&optional (sess (current-session)))
  (cond (sess
	 ;;(format t "~%checking if session is alive")
	 (bt:thread-alive-p (thread sess)))
	(t (format t "~%no session?"))))

(defun session-kill (sess)
  (when (session-alive-p sess)
    (bt:destroy-thread (thread sess)))
  (setq *all-sessions* (remove sess *all-sessions*)))

(defun get-session (obj)
  "Given a thread or id, will return the corresponding session"
  (if (bt:threadp obj)
      (find obj *all-sessions* :key #'thread)
      (find obj *all-sessions* :key #'id :test #'string-equal)))

(defun quit-all-proof-sessions ()
  "Attempts to quit all proof sessions in *all-sessions*
Sends a quit, waits half a sec, then kills the thread, and moves the session to "
  (dolist (sess *all-sessions*)
    (if (session-alive-p sess)
	(let ((id (id sess)))
	  (handler-case (bt:with-timeout (3) (prover-step id "(quit)"))
	    (bt:timeout () (session-kill sess))))
	(session-kill sess))))

;; Current-session returns the session associated with the current-thread
(defun current-session ()
  (get-session (bt:current-thread)))

(defun session-output (output)
  "If in a session, pushes the output to the outputs list, returning output.
otherwise returns nil."
  (let ((sess (current-session)))
    (when sess
      (let ((outp (if (stringp output)
		      (string-trim '(#\Space #\Tab #\Newline) output)
		      output)))
	(unless (typep outp '(or string proofstate simple-condition))
	  (break "Strange output: ~a" outp))
	(push outp (outputs sess))))))

(defun session-read ()
  "When in a session, push the outputs list to the output-queue, wait on
the input-queue, and return the lisp form of the string."
  (let ((sess (current-session)))
    (when sess
      (when (outputs sess)
	;;(format t "~%session-read: have ~d outputs" (length (outputs sess)))
	(push-queue (outputs sess) (output-queue sess))
	(push (outputs sess) (logs sess))
	(setf (outputs sess) nil))
      ;;(format t "~%session-read: waiting on input-queue")
      (session-read* sess))))

(defun session-finish (&optional sess)
  "This should be called from the thread, to notify the output queue that
the session is finished after this."
  (let ((sess (if sess sess (current-session))))
    (when sess
      (push-queue (outputs sess) (output-queue sess))
      (push (outputs sess) (logs sess))
      (loop until (queue-empty-p (output-queue sess))
	    do (progn ;;(format t "~%finish-proofstate: waiting for output-queue to empty")
		      (sleep .1))))))

(defun session-error (string &rest args)
  (let ((sess (current-session)))
    (when sess
      ;; Let the session error handler deal with this.
      (push `("error" ,string ,@args) (outputs sess)))))

;;; Proof sessions

(defcl proof-session (session)
  formula-decl
  ;; A single proof command can have multiple calls to output-proofstate,
  ;; e.g., (rerun) or (grind$). So output-proofstate pushes them onto the outputs list,
  ;; and prover-read uses it to construct the output for the output-queue.
  outputs)

(defmethod print-object ((ses proof-session) stream)
  (format stream "#<proof-session ~a>" (id ses)))

(defmethod session-read* ((sess proof-session))
  (let ((input (pop-queue (input-queue sess))))
    ;; (format t "~%session-read*: input = ~s" input)
    (multiple-value-bind (inp err)
	(ignore-errors (read-from-string input))
      (cond (inp
	     (push inp (logs sess))
	     (if (in-the-debugger)
		 (if (and (consp inp)
			  (symbolp (car inp))
			  (string-equal (car inp) "lisp"))
		     inp
		     (list '(restore) inp))
		 (if (quit-command-p inp) '(quit) inp)))
	    (t (push err (logs sess))
	       err)))))

(defun all-proof-sessions ()
  (remove-if-not #'proof-session? *all-sessions*))

(defun all-proof-session-status ()
  "Checks the status of the proof sessions: active or inactive.
Returns a list of the form ((id . status) (id . status) ...)"
  ;; Should we allow for the Lisp Listener here?
  (let ((prf-sessions (all-proof-sessions)))
    (when prf-sessions
      (mapcar #'(lambda (sess)
		  (cons (id sess)
			(if (session-alive-p sess) :active :inactive)))
	prf-sessions))))

(defvar *sessions-to-be-interrupted* nil)

(defun interrupt-session (id)
  (pushnew id *sessions-to-be-interrupted*)
  (format t "~%interrupt-session for ~a" id)
  "Interrupted")

(defun handle-interrupt ()
  (let* ((sess (current-session))
	 (id (when sess (id sess))))
    (when (member id *sessions-to-be-interrupted* :test #'string=)
      (setq *sessions-to-be-interrupted*
	    (remove id *sessions-to-be-interrupted* :test #'string=))
      (when (session-alive-p sess)
	(format t "~%In handle-interrupt about to restore")
	(if (find-restart 'prover-abort)
	    (invoke-restart 'prover-abort)
	    (throw 'abort nil))))))

(defun prove-formula-in-session (id fdecl rerun? input-hook)
  "This is the function run in a proof-session thread."
  ;; For some reason, a waiting thread in Allegro is difficult to delete,
  (let ((*multiple-proof-default-behavior* :noquestions)
	(*pvs-emacs-interface* nil)
	(*prover-input-hooks* (if input-hook
				  (cons input-hook *prover-input-hooks*)
				  *prover-input-hooks*))
	(*pvs-message-hook* (list #'session-output)))
    ;;(sys:set-stack-cushion 1219182)
    ;;(format t "~%stack-cushion = ~a" (sys:stack-cushion))
    (session-output (format nil "~%~a started at ~a" id (iso8601-date)))
    ;; trivial-timer should be used for Allegro
    #+sbcl
    (let ((timer (sb-ext:make-timer #'handle-interrupt)))
      (sb-ext:schedule-timer timer 1 :repeat-interval 1)
      (prove-formula fdecl :rerun? rerun?)
      (sb-ext:unschedule-timer timer))
    #-sbcl
    ;; (progn (trivial-timer:initialize-timer)
    ;; 	   (let ((timer (trivial-timer:register-timer-recurring-call
    ;; 			 1000 #'handle-interrupt)))
    ;; 	     (prove-formula fdecl :rerun? rerun?)
    ;; 	     (trivial-timer:cancel-timer-call timer)))
    (prove-formula fdecl :rerun? rerun?)
    ))


;;; Both prover-init and prover-step return alists of the form
;;; ((:ps proofstate) (:log log) (:id id) (:status status))
;;;   where:
;;;     proofstate is the resulting proofstate, 
;;;     id is the proof-session identifier returned by prover-init and needed for prover-step
;;;     log is a list of all the output one sees running the prover in the Emacs interface.
;;;         It's actually more, as it is an interleaving of proofstates and string commentary
;;;         The following will create a list of strings from this.
;;;           (mapcar #'(lambda (c) (if (stringp c) c (format nil "~a" c))) log)
;;;     status is one of:
;;;       :qed - proof is finished successfully
;;;       :quit - proof ended by the user
;;;       :waiting - prover is waiting for input
;;;      Note that if it's not waiting, then the session is gone

(defun prover-init (formref &key rerun? input-hook)
  "Creates a proof-session, and returns (values proofstate log id status)."
  (let* ((fdecl (get-formula-decl formref))
	 (id (get-new-session-id (id fdecl)))
	 (sess (make-session id #'(lambda ()
				    (prove-formula-in-session id fdecl rerun? input-hook)))))
    (change-class sess 'proof-session :formula-decl fdecl)
    ;; Wait for output-proofstate to produce a new proofstate
    (let ((out (pop-queue (output-queue sess))))
      (assert (typep (car out) 'proofstate))
      (let* ((ps (car out))
	     (commentary (reverse (cdr out)))
	     (status (case (status-flag ps)
		       (! "proved")
		       (quit "quit")
		       (t "init")))) ;; prover-step adds
	(unless (string-equal status "init")
	  (push (cons (id sess) status) *done-sessions*)
	  (loop while (session-alive-p sess)
		do (progn ;; (format t "~%Waiting for thread to die")
			  (sleep .1)))
	  ;;(format t "~%Thread is done")
	  (setf *all-sessions* (remove sess *all-sessions*)))
	`(("id" . ,(string id))
	  ("proofstate" . ,ps)
	  ("status" . ,(string-downcase status))
	  ("commentary" . ,commentary))))))

(defun prover-step (id cmd)
  "Takes a step for proof session id. Returns a proofstate, error, or finished message."
  (let ((sess (get-session id)))
    (unless sess
      (error "Proof ~a not found; ~:[no proofs are running~;current proofs are:~%  ~:*~{~a~^, ~}~]"
	     id (mapcar #'id *all-sessions*)))
    ;; While debugging, sometimes outputs are not popped, so we clean up here.
    ;; Should allow multiple inputs to pile up, however
    (dotimes (i (queue-count (output-queue sess)))
      (let ((outp (pop-queue (output-queue sess))))
	(format t "output-queue unhandled element ~d: ~a" i outp)
	(push outp (outputs sess))))
    (push-queue cmd (input-queue sess))
      ;; (format t "~%pr-step: pushed ~a to input-queue, waiting for output" cmd)
    (let ((outs (pop-queue (output-queue sess)))) ;; Waits on output-queue
      (if (typep (car outs) 'proofstate)
	  (let* ((ps (car outs))
		 (commentary (reverse (cdr outs)))
		 (status (cond ((typep ps 'top-proofstate)
				(case (status-flag ps)
				  (! 'proved)
				  (quit 'quit)
				  (t 'waiting)))
			       ((eq (status-flag ps) 'quit)
				;; shouldn't happen
				;; (format t ~%Found quit on non-top-proofstate)
				'quit)
			       (t 'waiting)))
		 (err (find-if #'(lambda (c) (typep c 'error)) commentary)))
	    (when err (setq commentary (remove err commentary)))
	    (unless (eq status 'waiting)
	      (loop while (session-alive-p sess)
		    do (progn ;; (format t "~%Waiting for thread to die")
			 (sleep .1)))
	      (setf *all-sessions* (remove sess *all-sessions*))
	      (push (cons (id sess) status) *done-sessions*))
	    (let* ((prf-result `(("id" . ,(string id))
				 ("command" . ,cmd)
				 ("proofstate" . ,ps)
				 ("status" . ,(string-downcase status))
				 ("commentary" . ,commentary)
				 ,@(when err
				     `(("error" . ,(format nil "~a" err))))
				 ))
		   ;;(prf-alist (pvs-jsonrpc::pvs2json-response prf-result))
		   ;;(prf-json (json:encode-json-to-string prf-alist))
		   )
	      prf-result))
	  (error "Caught an error: ~%~a" outs))
	;; ((and (listp result)
	;; 	    (eq (car result) :error))
	;;  (error (apply #'error (cadr result) (cddr result))))
	;;(t (break "bad result from session"))
	)))

(defun get-new-session-id (id)
  (unique-symbol* id 0))

;;; prover-read, output-proofstate, and finish-proofstate are all called from the prover,
;;; hence in the prover thread

;;; Replaced prover-read with session-read in qread

(defun finish-proofstate (ps)
  (let* ((proved? (and (typep ps 'top-proofstate)
		       (eq (status-flag ps) '!)))
	 ;;(done-str (if proved? "Q.E.D." "Unfinished"))
	 )
    ;;(format t "~%finish-proofstate: called")
    (when (and *pvs-emacs-interface*
	       *pvs-emacs-output-proofstate-p*)
      (format nil ":pvs-prfst ~a :end-pvs-prfst"
	(write-to-temp-file (if proved? "true" "false"))))
    (unless (eq (status-flag ps) '!)
      (setf (status-flag ps) 'quit))
    (session-output ps)
    (session-finish)
    (write-prover-log)
    ps))

(defun prove-prelude-to-json ()
  (let ((json-path (format nil "~a/lib/pvsjson/" *pvs-path*)))
    (ensure-directories-exist json-path)
    (dolist (th *prelude-theories*)
      (let ((th-path (format nil "~a~a.json" json-path (id th)))
	    (prfs (mapcar #'(lambda (fmla) (prover-init fmla :rerun? t))
		    (all-formulas th))))
	(when prfs
	  (with-open-file (out th-path :direction :output :if-exists :supersede)
	    (json:encode-json
	     `(("theory" . ,(string (id th)))
	       ("proofs" . ,(mapcar #'pvs-jsonrpc:pvs2json-response prfs)))
	      out)))))))

(defun prover-status (&optional proof-id)
  "Checks the status of the prover for the given proof-id, or all if none given.
If proof-id is given, returns \"active\" or \"inactive\", otherwise returns a list
of objects of the form {\"proofId\":id,\"status\":status}"
  ;; (format t "~%pvs:prover-status: *top-proofstate* ~a, *last-proof* ~a, *ps* ~a~%"
  ;;   (and top-ps t) (and last-proof t) (and ps t))
  (if proof-id
      (let ((sess (get-session proof-id)))
	(if sess
	    (list `(("proof-id" . ,(string proof-id))
		    ("status" . ,(if sess
				     (if (session-alive-p sess) "active" "inactive")
				     "missing"))))
	    (let ((done (assoc proof-id *done-sessions* :test #'string=)))
	      (list `(("proof-id" . ,(string proof-id))
		      ("status" . ,(if done
				       (string-downcase (cdr done))
				       "missing")))))))
      (mapcar #'(lambda (sess)
		  `(("proof-id" . ,(string (id sess)))
		    ("status" . ,(if (session-alive-p sess) "active" "inactive"))))
	*all-sessions*)))

(defun proof-sessions-for-theory (theory-ref)
  (with-theory (theory) theory-ref
    (let ((results nil))
      (dolist (fmla (provable-formulas theory))
	(let ((result (prover-init fmla)))
	  (push result results)))
      results)))

;; (defcl pvsio-session (session)
;;   context)

;; (defun pvsio-in-session (theoryref input tccs? banner?)
;;   (evaluation-mode-pvsio theoryref input tccs? banner?))

;; (defun make-pvsio-session (theoryref &optional input tccs? banner?)
;;   (with-theory (theory) theoryref
;;     (let* ((id (get-new-session-id (id theory)))
;; 	   (sess (make-session id
;; 			       #'(lambda ()
;; 				   (pvsio-in-session theoryref input tccs? banner?)))))
;;       (change-class sess 'pvsio-session)
;;       (push sess *all-sessions*)
;;       ;; Wait for output-proofstate to produce a new proofstate
;;       ;; Releases the lock and waits on outvar
;;       (format t "~%PVS: waiting for outvar")
;;       ;;(bt:condition-wait (outcvar sess) nil)
;;       (let ((out nil ;;(q:qpop (output-queue sess))
;; 	      ))
;; 	(format t "~%PVS: have output ~a for ~a" out id)
;; 	(list id out)))))

(defcl pvsio-session (session)
  context
  history)

(defmethod print-object ((ses pvsio-session) stream)
  (format stream "#<pvsio-session ~a>" (id ses)))

(defmethod session-read* ((sess pvsio-session))
  (let ((input (pop-queue (input-queue sess))))
    ;; (format t "~%session-read*: input = ~s" input)
    ;;(push input (logs sess))
    input))

(defun pvsio-init (theoryref &key input-hook tccs?)
  (let*((theory (get-typechecked-theory theoryref))
	(context (context theory)))
    (let* ((id (get-new-session-id (id theory)))
	   (sess (make-session id #'(lambda ()
				      (pvsio-in-session id context input-hook tccs?)))))
      (change-class sess 'pvsio-session :context context)
      `(("id" . ,(string id)) ("promptIn" . ,*pvsio-promptin*) ("version" . ,*pvsio-version*)))))

(defun pvsio-eval (session-id pvs-ex kind)
  (let ((sess (get-session session-id)))
    (unless sess
      (error "PVSio session ~a not found" session-id))
    (push-queue `(,(if (string= kind "lisp") 'lisp 'pvs) . ,pvs-ex) (input-queue sess))
    (let* ((out (pop-queue (output-queue sess))))
      (push (cons pvs-ex out) (history sess))
      out)))

(defvar *pvsio-input-hooks* nil)

(defun pvsio-in-session (session-id context input-hook &optional tccs?)
  (let ((*pvsio-input-hooks* (if input-hook
				 (cons input-hook *pvsio-input-hooks*)
				 *pvsio-input-hooks*))
	(*pvs-message-hook* (list #'session-output))
	(sess (get-session session-id)))
    (with-context context
      #+sbcl
      (let ((timer (sb-ext:make-timer #'handle-interrupt)))
	(sb-ext:schedule-timer timer 1 :repeat-interval 1)
	(let ((*generate-tccs* (if tccs? 'all 'none))
	      (*current-context* (or context (context nil)))
	      (*suppress-msg* t)
	      (*in-evaluator* t)
	      (*destructive?* t)
	      (*eval-verbose* nil)
	      (*compile-verbose* nil)
	      (*convert-back-to-pvs* t)
	      (*disable-gc-printout* t)
	      (*pvstrace-level* 0)
	      (*eval-untranslatable* nil)
	      (*pvs2cl-mappings* nil)
	      (*pvs-message-hook* (lambda (str) (format t "~a~%" str))))
	  (restart-case
	      (evaluate-in-session sess)
	    (pvsio-quit () (format t "Finished PVSio session ~a~%" session-id) nil)))
	(sb-ext:unschedule-timer timer))
      #-sbcl
      ;; (progn (trivial-timer:initialize-timer)
      ;; 	     (let ((timer (trivial-timer:register-timer-recurring-call
      ;; 			   1000 #'handle-interrupt)))
      ;; 	       (evaluate-in-session sess)
      ;; 	       (trivial-timer:cancel-timer-call timer)))
      nil
      )))

(defun evaluate-in-session (sess)
  (let ((input (session-read))) ;; Waits for input
    ;; INPUT is assumed to be a pair (<kind> . <expr>)
    ;;  where <kind> is either the symbols 'PVS or 'LISP, and
    ;;        <expr> is a string
    (let*((kind (car input))
	  (expr  (cdr input))
	  (result `(("sessionId" . ,(string (id sess)))
		    ("inputKind" . ,kind)
		    ("inputExpr" . ,expr))))
      (push-queue
       (cond
	 ((member expr '("quit" "q" "(quit)" "exit" "(exit)") :test #'string-equal)
	  (setq result "bye!"))
	 ((member expr '("help") :test #'string-equal)
	  (nconc result `(("debugMsg" . ,(with-output-to-string (*standard-output*)(help-pvsio))))))
	 ((member expr '("debug") :test #'string-equal)
	  (setq *evaluator-debug* t)
	  (nconc result `(("debugMsg" . ,(format nil "~%Enabled debugging; breaks on errors, more information printed~%")))))
	 ((member expr '("nodebug") :test #'string-equal)
	  (setq *evaluator-debug* nil)
	  (nconc result `(("debugMsg" . ,(format nil "~%Debugging disabled~%")))))
	 ((member expr '("timing") :test #'string-equal)
	  (setq *pvs-eval-do-timing* t)
	  (nconc result `(("debugMsg" . ,(format nil "~%Printing of timing information enabled~%")))))
	 ((member expr '("notiming") :test #'string-equal)
	  (setq *pvs-eval-do-timing* nil)
	  (nconc result `(("debugMsg" . ,(format nil "~%Printing of timing information disabled~%")))))
	 ((member expr '("tccs") :test #'string-equal)
	  (setq *generate-tccs* 'all)
	  (nconc result `(("debugMsg" . ,(format nil "~%Enabled TCCs generation~%")))))
	 ((member expr '("notccs") :test #'string-equal)
	  (setq *generate-tccs* 'none)
	  (nconc result `(("debugMsg" . ,(format nil "~%Disabled TCCs generation~%")))))
	 ((member expr '("pvsio-version" "pvsio_version")
		  :test #'string-equal)
	  (nconc result `(("debugMsg" . ,(format nil "~%~a~%" *pvsio-version*)))))
	 ((member expr '("list-pvs-attachments" "list_pvs_attachments")
		  :test #'string-equal)
	  (nconc result `(("debugMsg" . ,(with-output-to-string (*standard-output*) (list-pvs-attachments))))))
	 ((member expr '("load-pvs-attachments" "load_pvs_attachments")
		  :test #'string-equal)
	  (let ((stdout-stream (make-string-output-stream))
		(error-stream (make-string-output-stream)))
	    (let ((*standard-output* stdout-stream)
		  (*error-output* error-stream))
	      (load-pvs-attachments t)
	      (format t "~&Done loading attachements.~%")
	      (force-output))
	    (nconc result `(("errOut" . ,(get-output-stream-string error-stream))
			    ("stdOut" . ,(get-output-stream-string stdout-stream))))))
	 ((eq kind 'PVS)
	  (let ((res (make-string-output-stream))
		(deb (make-string-output-stream))
		(tcc (make-string-output-stream))
		(std (make-string-output-stream))
		(err (make-string-output-stream)))
	    (evaluate-pvsio* expr res std deb tcc err nil)
	    (nconc result `(("promptOut" . ,(format nil *pvsio-promptout*)) ; prompts could have format metadata
			    ("promptIn" . ,(format nil *pvsio-promptin*))
			    ("stdOut" . ,(get-output-stream-string std))
			    ("pvsResult" . ,(get-output-stream-string res))
			    ("debugMsg" . ,(get-output-stream-string deb))
			    ("tccMsg" . ,(get-output-stream-string tcc))
			    ("errOut" . ,(get-output-stream-string err))))))
	 ((eq kind 'LISP)
	  (let ((res (make-string-output-stream))
		(std (make-string-output-stream))
		(err (make-string-output-stream)))
	    (let ((*standard-output* std)
		  (*error-output* err))
	      (handler-case
		  (format res "~s" (eval (read-from-string expr)))
		(error (condition) (format err "~a" condition))))
	    (nconc result `(("stdOut" . ,(get-output-stream-string std))
			    ("errOut" . ,(get-output-stream-string err))
			    ("lispResult" . ,(get-output-stream-string res)))))))
       (output-queue sess))
      (if (member expr '("quit" "q" "(quit)" "exit" "(exit)") :test #'string-equal)
	  (invoke-restart 'pvsio-quit)
	  (evaluate-in-session sess)))))

(defun isvoid (ex)
  (and ex
       (type-name? (type ex))
       (eq (id (type ex)) '|void|)))


(defun evaluate-in-session* (input result)
  (let ((*tccforms* nil))
    (multiple-value-bind (tc-input tc-err)
	(ignore-errors (tc-expr input))
      (cond (tc-input
	     (nconc result `(("tc-input" . ,tc-input)))
	     (multiple-value-bind (cl-input cl-err)
		 (ignore-errors (pvs2cl tc-input))
	       (cond (cl-input
		      (nconc result `(("cl-input" . ,cl-input)))
		      (multiple-value-bind (cl-eval eval-err)
			  (if nil ;;*pvs-eval-do-timing*
			      (time (eval cl-input))
			      (eval cl-input))
			(cond (cl-eval
			       (nconc result `(("cl-eval" . ,cl-eval)))
			       (if (isvoid tc-input)
				   (nconc result '(("pvs-val" . "void")))
				   (multiple-value-bind (pvs-val pvs-val-err)
				       (cl2pvs cl-eval (type tc-input))
				     (if pvs-val
					 (nconc result `(("pvs-val" . ,pvs-val)))
					 (let ((verr (format nil "~a" pvs-val-err)))
					   (nconc result `(("pvs-val-err" . ,verr))))))))
			      (t (nconc result `(("eval-err" . ,(format nil "~a" eval-err))))))))
		     (t (nconc result `(("cl-err" . ,(format nil "~a" cl-err))))))))
	    (t (nconc result `(("tc-error" . ,(format nil "~a" tc-err)))))))))
				     
(defmacro message-periodically-till-finished ((period msg) &body body)
  (let ((thread (gentemp))
	(pd (gentemp)))
    `(let* ((,pd ,period)
	    (,thread (bt:make-thread #'(lambda ()
					 (let ((time 0))
					   (loop while t
						 do (progn (sleep ,pd)
							   (incf time ,pd)
							   (pvs-message
							       (format nil "(~a sec): ~a"
								 time ,msg)))))))))
       (unwind-protect (progn ,@body)
	 (when (bt:thread-alive-p ,thread) (bt:destroy-thread ,thread))))))
