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

;;;  proof-session: class

(defcl session ()
  id
  ;;stdout
  thread
  input-queue
  output-queue)

;; Need to be able to return the following, note that only the commentary is
;; not derivable from the ps.
;;  commentary - *prover-commentary*, which is collected as the prover runs
;;  action  - (format-printout (parent-proofstate ps))
;;  num-subgoals - (proofstate-num-subgoals ps)
;;  label - (label ps)
;;  commend - (commend ps)
;;  sequent - (pvs2json-seq current-goal (parent-proofstate ps))

(defmethod print-object ((ses session) stream)
  (format stream "#<proof-session ~a>" (id ses)))

(defvar *all-sessions* nil
  "list of sessions")

(defun make-session (id fun)
  (let ((sess
	 (make-instance 'session
	   :id id
	   ;; :stdout (make-array 100 :element-type 'character :fill-pointer 0 :adjustable t)
	   :input-queue (make-queue)
	   :output-queue (make-queue))))
    ;; Make sure the session is available before starting the thread
    (push sess *all-sessions*)
    (setf (thread sess) (bt:make-thread fun :name (string id)))
    sess))

(defun session-alive-p (&optional (sess (current-session)))
  (bt:thread-alive-p (thread sess)))

(defun get-session (obj)
  "Given a thread or id, will return the corresponding session"
  (if (bt:threadp obj)
      (find obj *all-sessions* :key #'thread)
      (find obj *all-sessions* :key #'id :test #'string-equal)))

(defun current-session ()
  (get-session (bt:current-thread)))

(defun session-output (output)
  "If in a session, pushes the output to the outputs list, returning output.
otherwise returns nil."
  (let ((sess (current-session)))
    (when sess
      (push output (outputs sess)))))

(defun session-read ()
  "When in a session, push the outputs list to the output-queue, wait on
the input-queue, and return the lisp form of the string."
  (let ((sess (current-session)))
    (when sess
      (when (outputs sess)
	;;(format t "~%session-read: have ~d outputs" (length (outputs sess)))
	(push-queue (outputs sess) (output-queue sess))
	(setf (outputs sess) nil))
      ;;(format t "~%session-read: waiting on input-queue")
      (let ((input (pop-queue (input-queue sess))))
	(format t "~%session-read: have input ~a" input)
	(handler-case (read-from-string input)
	  (error (c)
	    (format t "~%session-read: pushing error to output-queue")
	    (break "this sucks!!")
	    (push-queue c (output-queue sess))
	    (session-read)))))))

(defun session-finish ()
  "This should be called from the thread, to notify the output queue that
the session is finished after this."
  (let ((sess (current-session)))
    (when sess
      (push-queue (outputs sess) (output-queue sess))
      (loop until (queue-empty-p (output-queue sess))
	    do (progn (format t "~%finish-proofstate: waiting for output-queue to empty")
		      (sleep 1))))))

(defun session-error (string &rest args)
  (let ((sess (current-session)))
    (when sess
      ;; Let the session error handler deal with this.
      (push `(:error ,string ,@args) (outputs sess)))))

;;; Proof sessions

(defcl proof-session (session)
  formula-decl
  ;; A single proof command can have multiple calls to output-proofstate,
  ;; e.g., (rerun) or (grind$). So output-proofstate pushes them onto the outputs list,
  ;; and prover-read uses it to construct the output for the output-queue.
  outputs)

(defun all-proof-sessions ()
  (remove-if-not #'proof-session? *all-sessions*))

(defun prover-status ()
  "Checks the status of the proof sessions: active or inactive.
Returns a list of the form ((id . status) (id . status) ...)"
  ;; Should we allow for the Lisp Listener here?
  (let ((prf-sessions (all-proof-sessions)))
    (when prf-sessions
      (mapcar #'(lambda (sess)
		  (cons (id sess)
			(if (session-alive-p sess) :active :inactive)))
	prf-sessions))))

(defun prove-formula-in-session (id fdecl rerun?)
  "This is the function run in a proof-session thread."
  ;; For some reason, a waiting thread in Allegro is difficult to delete,
  (catch :top-level-reset ;; probably only allegro, but won't cause problems o.w.
    (let ((sess (current-session)))
      (handler-bind
	  ((error
	    #'(lambda (err)
		(format t "~%prove-formula-in-session: pushing error ~a to output-queue" err)
		(push-queue err (output-queue sess))
		err)))
	(let ((*multiple-proof-default-behavior* :noquestions)) ;; not working
	  (session-output (format nil "~%~a started at ~a" id (iso8601-date)))
	  ;; (let ((*standard-output* (stdout ses)))
	  ;;   (with-output-to-string (*standard-output* (stdout ses))
	  (prove-formula fdecl :rerun? rerun?)
	  ;;))
	  )))))


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

(defun prover-init (formref &optional rerun?)
  "Creates a proof-session, and returns (values proofstate log id status)."
  (let* ((fdecl (get-formula-decl formref))
	 (id (get-new-session-id (id fdecl)))
	 (sess (make-session id #'(lambda () (prove-formula-in-session id fdecl rerun?)))))
    (change-class sess 'proof-session
      :formula-decl fdecl)
    ;; Wait for output-proofstate to produce a new proofstate
    ;;(format t "~%make-proof-session: waiting for output-queue")
    (let ((out (pop-queue (output-queue sess))))
      ;;(format t "~%make-proof-session: have output ~a for ~a" out id)
      (assert (proofstate? (car out)))
      (let* ((ps (car out))
	     (log (reverse (cdr out)))
	     (status (if (eq (status-flag ps) '!) :proved :init))) ;; prover-step adds
	(when (eq status :proved)
	  (loop while (session-alive-p sess)
		do (progn (format t "~%Waiting for thread to die") (sleep 1)))
	  (setf *all-sessions* (remove sess *all-sessions*)))
	`((:id . ,id)
	  (:proofstate . ,ps)
	  (:status . ,status)
	  (:log . ,log))))))

(defun prover-step (id cmd)
  "Takes a step for proof session id. Returns a proofstate, error, or finished message."
  (let ((sess (get-session id)))
    (unless sess
      (error "Proof ~a not found; ~:[no proofs are running~;current proofs are:~%  ~:*~{~a~^, ~}~]"
	     id (mapcar #'id *all-sessions*)))
    ;; While debugging, sometimes outputs are not popped, so we clean up here.
    ;; Should allow multiple inputs to pile up, however
    (dotimes (i (queue-count (output-queue sess)))
      (format t "output-queue unhandled element ~d: ~a"
	i (pop-queue (output-queue sess))))
    (push-queue cmd (input-queue sess))
    ;; (format t "~%pr-step: pushed ~a to input-queue, waiting for output" cmd)
    (let ((outs (pop-queue (output-queue sess)))) ;; Waits on output-queue
      (if (proofstate? (car outs))
	  (let* ((ps (car outs))
		 (log (reverse (cdr outs)))
		 (status (if (top-proofstate? ps)
			     (case (status-flag ps)
			       (! 'proved)
			       (quit 'quit)
			       (t 'waiting))
			     (if (eq (status-flag ps) 'quit) ;; shouldn't happen
				 'quit 'waiting))))
	    (unless (eq status 'waiting)
	      (loop while (session-alive-p sess)
		    do (progn (format t "~%Waiting for thread to die") (sleep 1)))
	      (setf *all-sessions* (remove sess *all-sessions*)))
	    `((:id . ,id)
	      (:command . ,cmd)
	      (:proofstate . ,ps)
	      (:status . ,status)
	      (:log . ,log)))
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
    (when (and *pvs-emacs-interface*
	       *pvs-emacs-output-proofstate-p*)
      (format nil ":pvs-prfst ~a :end-pvs-prfst"
	(write-to-temp-file (if proved? "true" "false"))))
    (dolist (hook *finish-proofstate-hooks*)
      (funcall hook ps))
    (unless (eq (status-flag ps) '!)
      (setf (status-flag ps) 'quit))
    (session-output ps)
    (session-finish)
    ps))

(defun prove-prelude-to-json ()
  (let ((json-path (format nil "~a/lib/pvsjson/" *pvs-path*)))
    (ensure-directories-exist json-path)
    (dolist (th *prelude-theories*)
      (let ((th-path (format nil "~a~a.json" json-path (id th)))
	    (prfs (mapcar #'(lambda (fmla) (prover-init fmla t))
		    (all-formulas th))))
	(when prfs
	  (with-open-file (out th-path :direction :output :if-exists :supersede)
	    (json:encode-json-alist
	      `((:theory . ,(id th))
		(:proofs . ,(mapcar #'pvs-jsonrpc:pvs2json-response prfs)))
	      out)))))))
      

(defcl pvsio-session (session)
  context)

(defun pvsio-in-session (theoryref input tccs? banner?)
  (evaluation-mode-pvsio theoryref input tccs? banner?))

(defun make-pvsio-session (theoryref &optional input tccs? banner?)
  (with-theory (thname) theoryref
    (let ((theory (get-typechecked-theory thname)))
      (when theory
	(let* ((id (get-new-session-id (id theory)))
	       (sess (make-session id
				   #'(lambda ()
				       (pvsio-in-session theoryref input tccs? banner?)))))
	  (change-class sess 'pvsio-session)
	  (push sess *all-sessions*)
	  ;; Wait for output-proofstate to produce a new proofstate
	  ;; Releases the lock and waits on outvar
	  (format t "~%PVS: waiting for outvar")
	  ;;(bt:condition-wait (outcvar sess) nil)
	  (let ((out nil ;;(q:qpop (output-queue sess))
		  ))
	    (format t "~%PVS: have output ~a for ~a" out id)
	    (list id out)))))))
