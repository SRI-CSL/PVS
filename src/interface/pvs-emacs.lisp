;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-emacs.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec 16 02:42:01 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Dec 17 01:30:40 2012
;; Update Count    : 23
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013 SRI International.  All Rights Reserved.

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

(defvar *to-emacs* nil)
(defvar *output-to-emacs* "")
(defvar *prover-invoking-commands*
  '(prove-formulas-importchain
    prove-formulas-importchain-subtree
    prove-formulas-pvs-file
    prove-formulas-theory 
    prove-usingchain
    prove-file-at
    prove-proofchain
    prove-pvs-file
    prove-tccs-importchain
    prove-tccs-pvs-file
    prove-tccs-theory
    prove-pvs-theories
    prove-theory
    prove-untried-importchain
    prove-untried-pvs-file
    prove-untried-theory
    prove-with-checkpoint))

;;; Called from pvs-send* in Emacs.  Name comes from ilisp's "ilisp-errors",
;;; which is their interface to Emacs.

#-(or akcl harlequin-common-lisp)
(defmacro pvs-errors (form)
  "Handle PVS errors when evaluating form"
  `(progn
     (princ " ")			; Make sure we have output
     (handler-case
	 (let ((*to-emacs* t))
	   (setq *print-length* nil)
	   (setq *print-level* nil)
	   (if (and #-(or multiprocessing mp) nil
		    *noninteractive*
		    *noninteractive-timeout*
		    ,(not (and (listp form)
			       (memq (car form) *prover-invoking-commands*))))
	       #-(or multiprocessing mp sbcl) nil
	       #+(or multiprocessing mp)
	       (mp:with-timeout (*noninteractive-timeout*
				 (format t "Timed out!"))
		   ,form)
	       #+sbcl
	       (sb-ext:with-timeout *noninteractive-timeout*
		 (handler-case ,form
		   (sb-ext:timeout () (format t "Timed out!"))))
	       ,form))
       (pvs-error (err)
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-err ~a&~a&~a&~a&~d ~d :end-pvs-err"
		   (when (file-name err) (protect-emacs-output (namestring (file-name err))))
		   (unless *from-buffer*
		     (protect-emacs-output (namestring *default-pathname-defaults*)))
		   (protect-emacs-output (message err))
		   (write-to-temp-file (error-string err))
		   (line-begin (place err)) (col-begin (place err)))))
	   (to-emacs)
	   (if *in-checker*
	       (restore)
	       (pvs-abort)))))))

#+(or akcl harlequin-common-lisp)
(defmacro pvs-errors (form)
  "Handle errors when evaluating FORM."
  `(progn
    (princ " ")			;Make sure we have output
    (let ((*to-emacs* t)
	  (*print-length* nil)
	  (*print-level* nil))
      ,form)))

(defmacro lisp (form)
  `,form)

(define-condition pvs-error (simple-error)
  ((message :accessor message :initarg :message)
   (error-string :accessor error-string :initarg :error-string)
   (file-name :accessor file-name :initarg :file-name)
   (place :accessor place :initarg :place))
  (:report (lambda (c stream)
	     (with-slots (error-string) c
	       (format stream "~a" error-string)))))

(defvar *old-result* nil)

;;; This replaces ilisp-restore in pvs-init
(defun pvs-ilisp-restore ()
  "Restore the old result history."
  #-sbcl (declare (special / // + ++ * **))
  (setq // (pop *old-result*)
	** (first //)
	/  (pop *old-result*)
	*  (first /)
	++  (pop *old-result*)
	+   (pop *old-result*))
  (setq *old-result* nil)
  nil)

(defun pvs-ilisp-save ()
  #-sbcl (declare (special / // /// + ++ +++))
  (unless *old-result*
    (setq *old-result* (list /// // +++ ++))))

;;; For JSON requests

(defmethod declaration-kind ((th module))
  'theory)

(defmethod declaration-kind ((decl type-decl))
  'type-decl)

(defmethod declaration-kind ((decl formal-type-decl))
  'formal-type-decl)

(defmethod declaration-kind ((decl formal-const-decl))
  'formal-const-decl)

(defmethod declaration-kind ((decl formal-theory-decl))
  'formal-theory-decl)

(defmethod declaration-kind ((decl lib-decl))
  'lib-decl)

(defmethod declaration-kind ((decl mod-decl))
  'theory-decl)

(defmethod declaration-kind ((decl var-decl))
  'var-decl)

(defmethod declaration-kind ((decl def-decl))
  'recursive-decl)

(defmethod declaration-kind ((decl conversion-decl))
  'conversion-decl)

(defmethod declaration-kind ((decl conversionminus-decl))
  'conversion-minus-decl)

(defmethod declaration-kind ((decl auto-rewrite-decl))
  'auto-rewrite-decl)

(defmethod declaration-kind ((decl auto-rewrite-minus-decl))
  'auto-rewrite-minus-decl)

(defmethod declaration-kind (decl)
  (class-name (class-of decl)))

(defmethod decl-id ((decl datatype))
  (id decl))

(defun pvs-message (ctl &rest args)
  (let ((str (format nil "~?" ctl args)))
    (if *pvs-message-hook*
	;;(format t "~%Calling message hook ~a" *pvs-message-hook*)
	;; Note that hooks are mostly for display, so we call them all.
	(if (listp *pvs-message-hook*)
	    (dolist (hook *pvs-message-hook*)
	      (funcall hook str))
	    (funcall *pvs-message-hook* str))
	(unless *suppress-msg*
	  (if *to-emacs*
	      (let* ((*print-pretty* nil)
		     (*output-to-emacs*
		      (protect-emacs-output
		       (format nil ":pvs-msg ~a :end-pvs-msg" str))))
		(to-emacs))
	      (format t "~%~a" str)))))
  nil)

(defun pvs-message-with-context (obj ctl &rest args)
  (assert *current-context*)
  (pvs-message "~%In ~a.~a:~a:~%  ~?"
    (id (current-theory))
    (when (current-declaration)
      (if (importing? (current-declaration))
	  (format nil "IMPORTING ~a" (theory-name (current-declaration)))
	  (id (current-declaration))))
    (if (place obj)
	(format nil "(at line ~d, column ~d) "
	  (starting-row (place obj)) (starting-col (place obj)))
	(format nil "(no place for ~a)" obj))
    ctl args))


;;; Collect messages until the end of parsing/typechecking, and provide
;;; them to a buffer.

(defun pvs-warning (ctl &rest args)
  (when *pvs-warning-hook*
    (funcall *pvs-warning-hook* (format nil "~?" ctl args)))
  (pvs-log "~% ~?~%" ctl args)
  (when (and (current-theory)
	     (not *in-checker*)
	     (not *in-evaluator*))
    (let ((warning (format nil "~?" ctl args)))
      (if (warnings (current-theory))
	  (unless (member warning (warnings (current-theory))
			  :key #'cdr :test #'string=)
	    (nconc (warnings (current-theory))
		   (list (cons (current-declaration) warning))))
	  (setf (warnings (current-theory))
		(list (cons (current-declaration) warning))))))
  nil)

;;; Collect messages until the end of parsing/typechecking, and provide
;;; them to a buffer.

(defun show-theory-warnings (theoryref)
  (let ((theory (get-theory theoryref)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryref))
	  ((null (warnings theory))
	   (pvs-message "Theory ~a has no warning messages" theoryref))
	  (t (pvs-buffer "PVS Warnings"
	       (format nil "Warnings for theory ~a:~2%~{~a~^~2%~}"
		 theoryref (mapcar #'cdr (warnings theory)))
	       t t)))))

(defun show-pvs-file-warnings (fileref)
  (with-pvs-file (filename) fileref
    (let ((theories (get-theories filename)))
      (if theories
	  (if (some #'warnings theories)
	      (pvs-buffer "PVS Warnings"
		(format nil
		    "Warnings for file ~a.pvs:~
               ~:{~2%Warnings for theory ~a:~@{~{~2%~a~}~}~}"
		  filename
		  (mapcar #'(lambda (th)
			      (list (id th) (mapcar #'cdr (warnings th))))
		    theories))
		t t)
	      (pvs-message "No warnings associated with ~a.pvs" filename))
	  (pvs-message "~a.pvs has not been typechecked" filename)))))

(defun pvs-info (ctl &rest args)
  (let ((info (if args
		  (format nil "~?" ctl args)
		  ctl)))
    (when (and *noninteractive*
	       (> *pvs-verbose* 1))
      (pvs-message "~% ~a~%" info))
    (when (and (current-theory)
	       (not *in-checker*)
	       (not *in-evaluator*))
      (if (info (current-theory))
	  (nconc (info (current-theory))
		 (list (cons (current-declaration) info)))
	  (setf (info (current-theory))
		(list (cons (current-declaration) info))))))
  nil)

(defun show-theory-messages (theoryref)
  (let ((theory (get-theory theoryref)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryref))
	  ((null (info theory))
	   (pvs-message "Theory ~a has no informational messages" theoryref))
	  (t (pvs-buffer "PVS Messages"
	       (format nil
		   "Messages for theory ~a:~
                    ~2%Use M-x show-theory-conversions to see the conversions.~
                    ~2%~{~a~^~2%~}"
		 theoryref
		 (mapcar #'cdr (info theory)))
	       t t)))))

(defun show-pvs-file-messages (fileref)
  (with-pvs-file (filename) fileref
    (let ((theories (get-theories filename)))
      (if theories
	  (if (some #'info theories)
	      (pvs-buffer "PVS Messages"
		(format nil
		    "Messages for file ~a.pvs:~
                   ~2%Use M-x show-theory-conversions to see the conversions.~
               ~:{~2%Messages for theory ~a:~@{~{~2%~a~}~}~}"
		  filename
		  (mapcar #'(lambda (th)
			      (list (id th) (mapcar #'cdr (info th))))
		    theories))
		t t)
	      (pvs-message "No messages associated with ~a.pvs" filename))
	  (pvs-message "~a.pvs has not been typechecked" filename)))))

;;; Conversions are treated separately

(defvar *print-conversions* t)

(defun pvs-conversion-msg (ctl &rest args)
  (when (and *print-conversions*
	     (or *noninteractive* *in-checker* *in-evaluator*))
    (pvs-message "~% ~?~%" ctl args)
    ;;(format-if "~% ~?~%" ctl args)
    )
  (when (and (current-theory)
	     (not *in-checker*)
	     (not *in-evaluator*))
    (let ((cmsg (format nil "~?" ctl args)))
      (if (conversion-messages (current-theory))
	  (nconc (conversion-messages (current-theory))
		 (list (cons (current-declaration) cmsg)))
	  (setf (conversion-messages (current-theory))
		(list (cons (current-declaration) cmsg))))))
  nil)

(defun show-theory-conversions (theoryref)
  (let ((theory (get-theory theoryref)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryref))
	  ((null (conversion-messages theory))
	   (pvs-message "Theory ~a has no conversions" theoryref))
	  (t (pvs-buffer "PVS Conversions"
	       (format nil
		   "Conversions for theory ~a:~
                    ~2%Use pretty-print-expanded (M-x ppe) to see the conversions as used in the theory.
                    ~2%~{~a~^~2%~}"
		 theoryref
		 (mapcar #'cdr (conversion-messages theory)))
	       t t)))))

(defun show-pvs-file-conversions (fileref)
  (with-pvs-file (filename) fileref
    (let ((theories (get-theories filename)))
      (if theories
	  (if (some #'conversion-messages theories)
	      (pvs-buffer "PVS Conversions"
		(format nil
		    "Conversions for file ~a.pvs:~
                   ~2%Use pretty-print-expanded (M-x ppe) to see the conversions as used in the theory.
               ~:{~2%Conversions for theory ~a:~@{~{~2%~a~}~}~}"
		  filename
		  (mapcar #'(lambda (th)
			      (list (id th)
				    (mapcar #'cdr (conversion-messages th))))
		    theories))
		t t)
	      (pvs-message "No conversions associated with ~a.pvs" filename))
	  (pvs-message "~a.pvs has not been typechecked" filename)))))

;;; Used to put messages in a log file

(defvar *pvs-log-stream* nil)

(defvar *pvs-log-directory* "~/.pvslog/")

(defun pvs-log (ctl &rest args)
  (multiple-value-bind (sec min hour date month year)
      (decode-universal-time (get-universal-time))
    ;; First time will set the log file and open it for writing
    (unless *pvs-log-stream*
      (let ((fname (format nil "~apvs-~4,'0d-~2,'0d-~2,'0d.log"
		     *pvs-log-directory* year month date)))
	(ensure-directories-exist *pvs-log-directory*)
	(setq *pvs-log-stream*
	      (open (uiop:native-namestring fname) :direction :output
		    :if-exists :append :if-does-not-exist :create))))
    ;; (unless (open-p *pvs-log-stream*)
    ;;   (setq *pvs-log-stream*
    ;; 	    (open *pvs-log-stream* :direction :output
    ;; 		  :if-exists :append :if-does-not-exist :create)))
    (format *pvs-log-stream* "[~2,'0d:~2,'0d:~2,'0d] ~?" hour min sec ctl args)
    ;;(uiop:finish-outputs *pvs-log-stream*)
    ))

(defun verbose-msg (ctl &rest args)
  ;; Writes out a message.  The message should fit on one line, and
  ;; should contain no newlines.  For Emacs, it is intended to write to
  ;; the minibuffer.
  (when *pvs-verbose*
    (if *to-emacs*
	(let* ((*print-pretty* nil)
	       (*output-to-emacs*
		(protect-emacs-output
		 (format nil ":pvs-msg ~? :end-pvs-msg" ctl args))))
	  (to-emacs))
	(format t "~%~?" ctl args))))

(defun pvs-output (ctl &rest args)
  (unless *suppress-msg*
    (if *to-emacs*
	(when *noninteractive*
	  (let* ((*print-pretty* nil)
		 (*output-to-emacs*
		  (format nil ":pvs-out ~a :end-pvs-out"
		    (write-to-temp-file (format nil "~?" ctl args)))))
	    (to-emacs)))
	(format t "~?" ctl args))))

(defun in-the-debugger ()
  ;; Calls wrapped in "lisp" so it works in or out of the prover
  #+sbcl (lisp sb-debug:*in-the-debugger*)
  #+allegro (lisp (plusp (debug:debug-break-level))))

(defun pvs-error (msg err &optional file-name place)
  ;; Indicates an error; no recovery possible.
  (assert (or (null file-name)
	      *from-buffer*
	      (file-exists-p file-name)))
  (let* ((place (if *adt-decl* (place *adt-decl*) place))
	 (buff (if *adt-decl*
		   (or (filename *generating-adt*)
		       (and (current-theory)
			    (filename (current-theory)))
		       *current-file*)
		   (or *from-buffer* file-name))))
    (cond (*rerunning-proof*
	   (restore))
	  (t (error 'pvs-error
		    :message msg
		    :error-string err
		    :file-name buff
		    :place place)))))

(defun pvs-abort ()
  #-allegro (abort)
  #+allegro (tpl::reset-command))

(defun pvs-y-or-n-p (msg &rest args)
  (pvs-yn (apply #'format nil msg args) nil nil))

(defun pvs-yes-or-no-p (msg &rest args)
  (pvs-yn (apply #'format nil msg args) t nil))

(defun pvs-y-or-n-p-with-timeout (msg &rest args)
  (pvs-yn (apply #'format nil msg args) nil t))

(defun pvs-yes-or-no-p-with-timeout (msg &rest args)
  (pvs-yn (apply #'format nil msg args) t t))

(defun pvs-yn (msg full? timeout?)
  (cond (*pvs-y-or-n-hook*
	 ;;(format t "~%Calling y-or-n hook ~a" *pvs-y-or-n-hook*)
	 (funcall *pvs-y-or-n-hook* msg full? timeout?))
	(*noninteractive* t)
	((and *pvs-emacs-interface* *to-emacs*)
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-yn ~a&~a&~a :end-pvs-yn"
		   (protect-emacs-output
		    (protect-format-string msg))
		   (if full? "t" "nil")
		   (if timeout? "t" "nil"))))
	   (to-emacs)
	   (let ((val (read)))
	     (when (eq val :abort)
	       (pvs-message "Aborting")
	       (pvs-abort))
	     val)))
	(t (let* ((prompt (concatenate 'string (protect-format-string msg)
				       #-sbcl (if full? "(Yes or No) " "(Y or N) ")))
		  (answer (if full?
			      (yes-or-no-p prompt)
			      (y-or-n-p prompt))))
	     (session-output (format nil "~a~:[~:[No~;Yes~]~;~:[N~;Y~]~]"
			       prompt full? answer))
	     answer))))

(defun pvs-dialog (prompt &rest args)
  (cond (*pvs-dialog-hook*
	 ;;(format t "~%Calling dialog hook ~a" *pvs-dialog-hook*)
	 (funcall *pvs-dialog-hook* (format nil "~?" prompt args)))
	(*to-emacs*
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-dia ~a:end-pvs-dia"
		   (protect-emacs-output
		    (format nil "~?" prompt args)))))
	   (to-emacs)
	   (let ((val (read)))
	     (when (eq val :abort)
	       (pvs-message "Aborting")
	       (pvs-abort))
	     val)))
	(t (format t "~?" prompt args)
	   #+sbcl (terpri)
	   (read-line))))

;;;        Returns t, nil, :auto, or aborts
;;; Corresponds to y, n,   !,    and q (C-g) on Emacs side.
(defun pvs-query (prompt &rest args)
  (cond (*pvs-query-hook*
	 ;;(format t "~%Calling query hook ~a" *pvs-query-hook*)
	 (funcall *pvs-query-hook* (format nil "~?" prompt args)))
	(*to-emacs*
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-qry ~a:end-pvs-qry"
		   (protect-emacs-output
		    (format nil "~?" prompt args)))))
	   (to-emacs)
	   (let ((val (read)))
	     (when (eq val :abort)
	       (pvs-message "Aborting")
	       (pvs-abort))
	     val)))
	(t (pvs-query* (format nil "~?" prompt args)))))

(defun pvs-query* (prompt)
  (format t "~a [Type y, n, q or !]~%" prompt)
  (case (read)
    (y t)
    (n nil)
    (q (pvs-abort))
    (! :auto)
    (t (pvs-query* prompt))))

;; (defun pvs-ask-user (prompt &optional pred reprompt)
;;   "Asks user for input using prompt.  If pred is given, and pred(input) is
;; false, then asks again using the reprompt, or prompt string if not provided.
;; If *pvs-ask-user-hook* is set, it is funcalled with prompt, pred, and
;; reprompt as args.  Note that if there are many possible GUIs, the first that
;; provides an answer that satisfies pred is the one accepted."
  
;;   (cond (*pvs-ask-user-hook*
;; 	 ;;(format t "~%Calling ask-user hook ~a" *pvs-ask-user-hook*)
;; 	 ;; pred should just be a name that is interpreted by the hook
;; 	 (funcall *pvs-ask-user-hook* prompt pred reprompt))
;; 	(*to-emacs*
;; 	 (let* ((*print-pretty* nil)
;; 		(*output-to-emacs*
;; 		 (format nil ":pvs-ask ~a:end-pvs-ask"
;; 		   (protect-emacs-output
;; 		    (format nil "~?" prompt pred reprompt)))))
;; 	   (to-emacs)
;; 	   (let ((val (read)))
;; 	     (when (eq val :abort)
;; 	       (pvs-message "Aborting")
;; 	       (pvs-abort))
;; 	     val)))
;; 	(t (pvs-ask-user* (format nil "~?" prompt args)))))

;; (defun pvs-ask-user* (prompt)
;;   (format t "~a [Type y, n, q or !]~%" prompt)
;;   (case (read)
;;     (y t)
;;     (n nil)
;;     (q (pvs-abort))
;;     (! :auto)
;;     (t (pvs-ask-user* prompt))))

(defun pvs-prompt (type msg &rest args)
  (if (and *pvs-emacs-interface* *to-emacs*)
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-pmt ~a&~? :end-pvs-pmt"
		type (protect-emacs-output msg) args)))
	(to-emacs)
	(read))
      (progn (format t "PVS> ~?" msg args)
	     (read))))

(defun pvs-emacs-eval (form)
  (when (and *pvs-emacs-interface*
	     (not (current-session)))
    (let* ((*print-pretty* nil)
	   (*output-to-emacs*
	    (format nil ":pvs-eval ~a :end-pvs-eval" form)))
      (to-emacs)
      (read))))

(defmacro pvs-eval (form)
  `(list :pvs-value ,form))


(defun query (theory msg query place)
  (declare (ignore theory place))
  (format t "~%~a~{~%~{~a: ~a~*~}~}~%choice? " msg query)
  (read-choice query))

(defun read-choice (query)
  (let* ((in (read))
	 (val (caddr (assoc in query :test #'equal))))
    (or val
	(format t "~%Not a valid choice - try again~%choice? ")
	(read-choice query))))

(defun emacs-output-proofstate (ps-string)
  (let* ((*output-to-emacs*
	  ;; action & result & label & sequent
	  (format nil ":pvs-prfst ~a :end-pvs-prfst"
	    (write-to-temp-file ps-string))))
    (to-emacs)))

;;; Creates a json form:
;;;   {"commentary" : [ strings ],
;;;    "action" : string,
;;;    "result" : ,
;;;    "label" : ,
;;;    "comment" : ,
;;;    "sequent" : {"antecedents" : [ s-formulas ],
;;;                 "succedents" : [ s-formulas ]}}

(defvar *proofstate-width* 80)
(defvar *proofstate-indent* 6)

(defun strim (string)
  (string-trim '(#\Space #\Tab #\Newline) string))

(defun proofstate-num-subgoals (ps)
  (let ((pps (parent-proofstate ps)))
    (when (and pps (eq (status-flag pps) '?))
      (cond ((remaining-subgoals pps)
	     (1+ (length (remaining-subgoals pps))))
	    ((not (typep (car (remaining-subgoals pps))
			 'strat-proofstate))
	     1)
	    (t (break))))))

;;; Similar to display-proof-from (wish.lisp)
;;;  delete-proof-subtree
;;;   proof-num-children  \
;;;   [proof-rule]        |
;;;   [proof-sequent]     > repeat down the tree
;;;   [proof-done]        |
;;;   [proof-tcc]         |
;;;   proof-show          /
;;;  layout-proof

;; (defstruct (prooftree-info (:conc-name pt-info-))
;;   delete
;;   status
;;   formula-id
;;   theory-id)

;; (defun proofstate-tree-info (ps)
;;   (let* ((top-ps (or *top-proofstate* *last-proof*))
;;   	 (path (path-to-subgoal (wish-top-proofstate) ps)) ; list of nats
;;   	 (pathstr (format nil "top~{.~a~}" path))
;;   	 (fdecl (declaration top-ps))
;;   	 (fid (id fdecl))
;;   	 (thid (id (module fdecl))))
;;     (unless (member nil path)
;;       (make-prooftree-info
;;        :delete path
;;        :status (proofstate-tree-status ps path)
;;        :formula-id fid
;;        :theory-id thid)
;;       ;;(write-proof-status ps path)
;;       ;;(format t "layout-proof ~a ~a 1~%" fid thid)
;;       )))

;;; From write-proof-status in wish.lisp
;; (defun proofstate-tree-status (ps path)
;;   (let* ((tcl-path (path-to-tcl-path path))
;; 	 (subs (x-subgoals ps))
;; 	 (rule (sexp-unparse (wish-current-rule ps)))
;; 	 (sequent (path-subgoal (wish-top-proofstate) path))
;; 	 (fdecl (declaration (or *top-proofstate* *last-proof*)))
;; 	 (fid (id fdecl))
;; 	 (thid (id (module fdecl))))
;;     (if (and (null rule) (eql (length subs) 1))
;; 	(proofstate-tree-status (car subs) path)
;; 	(let* ((nsubs (length subs))
;; 	       (prule (when rule
;; 			(let ((*print-case* :downcase))
;; 			  (format nil "~@[+~*~]~s" (null (current-rule ps))
;; 				  rule))))
;; 	       (seq (pvs2json sequent))
;; 	       (done? (eq (status-flag ps) '!))
;; 	       (tcc? (typep ps 'tcc-proofstate)))
;; 	  (format t "proof-num-children ~a ~a ~a ~a 1~%"
;; 	    fid thid tcl-path (length subs))
;; 	  (when rule
;; 	    (format t "proof-rule ~a ~a ~a {~a} 1~%" fid thid tcl-path
;; 		    (let ((*print-case* :downcase))
;; 		      (format nil "~@[+~*~]~s" (null (current-rule ps))
;; 			      rule))))
;; 	  (when sequent
;; 	    (format t "proof-sequent ~a ~a ~a {~a} {~a}~%"
;; 	      fid thid tcl-path (label sequent) sequent))
;; 	  (when (eq (status-flag ps) '!)
;; 	    (format t "proof-done ~a ~a ~a 1~%" fid thid tcl-path))
;; 	  (when (typep ps 'tcc-proofstate)
;; 	    (format t "proof-tcc ~a~%" tcl-path))
;; 	  (format t "proof-show ~a ~a ~a 1~%" fid thid tcl-path)
;; 	  (dotimes (i (length subs))
;; 	    (proofstate-tree-status (nth i subs) (append path (list i))))))))

(defun pvs-buffer (name contents &optional display? read-only? append? kind)
  (when *pvs-buffer-hook*
    (funcall *pvs-buffer-hook* name contents display? read-only? append? kind))
  (cond (*pvs-websocket-interface*
	 (format nil "~a" contents))
	(*to-emacs*
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-buf ~a&~a&~a&~a&~a&~a :end-pvs-buf"
		   name (when contents (write-to-temp-file contents))
		   display? read-only? append? kind)))
	   (to-emacs)))
	(display?
	 (format t "~&~a" contents))))

(defun pvs-display (name instance type value)
  ;; note no test for *to-emacs* 
  (let* ((*print-pretty* nil)
	 (*output-to-emacs*
	  (format nil ":pvs-dis ~a&~a&~a&~a :end-pvs-dis"
	    (protect-emacs-output name)
	    (protect-emacs-output instance)
	    (protect-emacs-output type)
	    (protect-emacs-output value))))
    (to-emacs)))

(defun to-emacs ()
  (format t "~a~%" *output-to-emacs*))

(defun beep ()
  (if *to-emacs*
      (format t ":pvs-bel :end-pvs-bel~%")))

(defun protect-emacs-output (string)
  "Escapes the &, \", and \\ characters, putting backslash in front."
  (if (stringp string)
      (with-output-to-string (str)
	(loop for ch across string
	      do (case ch
		   ((#\& #\" #\\)
		    (write-char #\\ str) (write-char ch str))
		   (#\newline
		    (write-char #\\ str) (write-char #\n str))
		   (t (write-char ch str)))))
      string))

(defun protect-emacs-output* (string pos &optional result)
  (if (< pos (length string))
      (protect-emacs-output*
       string
       (1+ pos)
       (case (char string pos)
	 (#\& (append '(#\& #\\) result))
	 (#\\ (append '(#\\ #\\) result))
	 (#\" (append '(#\" #\\) result))
	 (#\newline (append '(#\n #\\) result))
	 (t   (cons (char string pos) result))))
      (coerce (nreverse result) 'string)))

(defun protect-string-output (string)
  "Escapes backslash and double-quote if argument is a string, returns it otherwise."
  (if (stringp string)
      (with-output-to-string (str)
	(loop for ch across string
	      do (case ch
		   (#\\ (write-char #\\ str) (write-char #\\ str))
		   (#\" (write-char #\\ str) (write-char #\" str))
		   (t   (write-char ch str)))))
      string))

(defun protect-string-output* (string pos &optional result)
  "Escapes backslash and double-quote."
  (if (< pos (length string))
      (protect-string-output*
       string
       (1+ pos)
       (case (char string pos)
	 (#\\ (append '(#\\ #\\) result))
	 (#\" (append '(#\" #\\) result))
	 (t   (cons (char string pos) result))))
      (coerce (nreverse result) 'string)))

(defun protect-format-string (string &optional (pos 0) result)
  (if (< pos (length string))
      (protect-format-string
       string
       (1+ pos)
       (case (char string pos)
	 (#\~ (append '(#\~ #\~) result))
	 (t   (cons (char string pos) result))))
      (coerce (nreverse result) 'string)))

(defun parse-error (obj message &rest args)
  ;;(assert (or *in-checker* *current-file*))
  (cond (*parse-error-catch* ; From with-no-parse-errors macro
	 (throw *parse-error-catch*
	   (values nil
		   (if args
		       (format nil "~?" message args)
		       message)
		   obj)))
	((and (or *to-emacs*
		  (null *pvs-emacs-interface*))
	      (or (not *in-checker*)
		  (not *in-evaluator*)
		  *tc-add-decl*))
	 (let ((file-name
		(when *current-file*
		  (if (pathnamep *current-file*)
		      (namestring *current-file*)
		      (truename (make-specpath *current-file*))))))
	   (pvs-error "Parser error"
	     (format nil "~?~@[~%In file ~a~]~@[~a~]"
	       message args file-name
	       (when (place obj)
		 (format nil " (line ~a, col ~a)"
		   (line-begin (place obj))
		   (col-begin (place obj)))))
	     file-name
	     (place obj))))
	((and (or *in-checker*
		  *in-evaluator*)
	      (not (current-session))
	      (not *tcdebug*))
	 (format t "~%~?~@[~a~]" message args
		 (when (place obj)
		   (format nil " (line ~a, col ~a)"
		     (line-begin (place obj))
		     (col-begin (place obj)))))
	 (format t "~%Restoring the state.")
	 (restore))
	((pvs-ws:ws-current-connection)
	 (error "~?~@[~%In file ~a~]~@[~a~]"
	   message args
	   (when *current-file* (pathname-name *current-file*))
	   (when (place obj)
	     (format nil " (line ~a, col ~a)"
	       (line-begin (place obj))
	       (col-begin (place obj))))))
	((null *pvs-emacs-interface*)
	 (format t "~%<pvserror msg=\"parse-error\">~%\"~?~@[~%In file ~a~]~@[~a~]\"~%</pvserror>"
	   message args
	   (when *current-file* (pathname-name *current-file*))
	   (when (place obj)
	     (format nil " (line ~a, col ~a)"
	       (line-begin (place obj))
	       (col-begin (place obj)))))
	 (pvs-abort))
	(t (let* ((file-name (when *current-file* (pathname-name *current-file*)))
		  (message-str (format nil "~?" message args))
		  (error-str (format nil "~a~@[~%In file ~a~]~@[~a~]"
			       message-str file-name
			       (when (place obj)
				 (format nil " (line ~a, col ~a)"
				   (line-begin (place obj))
				   (col-begin (place obj)))))))
	     (error 'pvs-error
		    :message "Parser error"
		    :error-string error-str
		    :file-name file-name
		    :place (place obj))))))

(defvar *type-error* nil)
(defvar *type-error-argument* nil)
(defvar *skip-all-conversion-checks* nil)

(define-condition tcerror (simple-error)
  ((term :accessor term :initarg :term)
   (msg :accessor msg :initarg :msg)))

(defun type-error (obj message &rest args)
  (let ((errmsg (type-error-for-conversion obj message args)))
    (cond (*type-error-catch*
	   (throw *type-error-catch*
		  (values nil
			  (if (and (not *typechecking-actual*)
				   (or *in-checker*
				       *in-evaluator*))
			      (set-strategy-errors
			       (format nil "~a" errmsg))
			      (format nil "~a" errmsg))
			  obj)))
	  ((and *to-emacs*
		(or (not *in-checker*)
		    *tc-add-decl*))
	   (pvs-error "Typecheck error"
	     errmsg
	     (let ((fname (or (and (current-theory)
				   (not (from-prelude? (current-theory)))
				   (filename (current-theory)))
			      *current-file*)))
	       (when fname
		 (namestring (truename (make-specpath fname)))))
	     (if (and *adt*
		      (or (not (place obj))
			  (and (place (current-declaration))
			       (not (within-place (coerce (place obj) 'list)
						  (place (current-declaration)))))))
		 (place *adt*)
		 (place obj))))
	  ((and *in-checker* (not *tcdebug*) (not (current-session)))
	   (format t "~%~a" errmsg)
	   (format t "~%Restoring the state.")
	   (restore))
	  ((and *in-evaluator* (not *evaluator-debug*))
	   (error 'tcerror :term obj :msg errmsg))
	  (t (let* ((file-name (when *current-file*
				 (let ((len (length *current-file*)))
				   (if (and (> len 4)
					    (string= (subseq *current-file* (- len 4))
						     ".pvs"))
				       (format nil "~a~a" (current-path) *current-file*)
				       (format nil "~a~a.pvs" (current-path) *current-file*)))))
		    (message-str (format nil "~?" message args))
		    (error-str (format nil "~a~@[~%In file ~a~]~@[~a~]"
				 message-str file-name
				 (when (place obj)
				   (format nil " (line ~a, col ~a)"
				     (line-begin (place obj))
				     (col-begin (place obj)))))))
	       (error 'pvs-error
		      :message "Typecheck error"
		      :error-string error-str
		      :file-name file-name
		      :place (place obj)))))))

(defun plain-type-error (obj message &rest args)
  (let ((*skip-all-conversion-checks* t))
    (apply #'type-error obj message args)))

(defun type-error-noconv (obj message &rest args)
  (let ((*skip-k-conversion-check* t))
    (apply #'type-error obj message args)))

(defun type-error-for-conversion (obj message args)
  (let ((error (format nil
		   "~?~@[~%~a~]~:[~;~%You may need to add a semicolon (;) ~
                    to the end of the previous declaration~]"
		 (if args
		     message
		     (protect-format-string message))
		 (let ((*visible-only* t))
		   (mapcar #'(lambda (a)
			       (if (fully-typed? a) (full-name a) a))
		     args))
		 *type-error* *in-coercion*))
	(obj-conv? (unless *skip-all-conversion-checks*
		     (conversion-occurs-in? obj))))
    (cond ((and *typechecking-module*
		(not *skip-all-conversion-checks*)
		(null *type-error-catch*)
		(or obj-conv?
		    (and *type-error-argument*
			 (conversion-occurs-in? *type-error-argument*))))
	   (let* ((ex (if obj-conv?
			  obj
			  (mk-application* obj
			    (argument-list *type-error-argument*))))
		  (*type-error*
		   (format nil
		       "--------------~%With conversions, ~
                                    it becomes the expression ~%  ~a~%~
                                    and leads to the error:~%  ~a"
		     ex error))
		  (*no-conversions-allowed* t)
		  (etype (when (expr? obj)
			   (if obj-conv?
			       (type obj)
			       (when  (and (type obj)
					   (not (dep-binding?
						 (domain (type obj)))))
				 (range (type obj)))))))
	     (untypecheck-theory ex)
	     (if (expr? obj)
		 (if etype
		     (typecheck ex :expected etype)
		     (typecheck-uniquely ex))
		 (typecheck* ex nil nil nil))))
	  ((check-if-k-conversion-would-work obj nil)
	   (format nil
	       "~a~%Enabling K_conversion before this declaration might help"
	     error))
	  (t error))))

(defun check-if-k-conversion-would-work (ex arguments)
  (and (not *skip-all-conversion-checks*)
       (not *skip-k-conversion-check*)
       (gethash "K_conversion" *prelude*)
       *typechecking-module*
       (null *type-error-catch*)
       (not (some #'k-combinator? (conversions *current-context*)))
       (let ((*type-error-catch* 'type-error)
	     (cdecl (make-instance 'conversion-decl
		      :id '|K_conversion|
		      :module (get-theory "K_conversion")
		      :k-combinator? t
		      :expr (mk-name-expr '|K_conversion|))))
	 (typecheck* cdecl nil nil nil)
	 (unwind-protect
	     (progn
	       (push cdecl (conversions *current-context*))
	       (let ((tex (catch 'type-error
			    (ignore-errors
			      (typecheck* ex nil nil arguments)))))
		 (and tex (conversion-occurs-in? tex))))
	   (pop (conversions *current-context*))))))

(defun conversion-occurs-in? (obj)
  (let ((conv? nil))
    (mapobject #'(lambda (ex)
		   (or (type-expr? ex)
		       (when (typep ex '(or argument-conversion
					    implicit-conversion
					    lambda-conversion))
			 (setq conv? ex))))
	       obj)
    conv?))

(defun clear-hooks ()
  (setq *pvs-message-hook* nil)
  (setq *pvs-warning-hook* nil)
  (setq *pvs-buffer-hook* nil)
  (setq *pvs-y-or-n-hook* nil)
  (setq *pvs-query-hook* nil)
  (setq *pvs-dialog-hook* nil))

(defmethod place ((obj cons))
  (let ((start (place (car obj)))
	(end (place (car (last obj)))))
    (when (and start end)
      (vector (starting-row start) (starting-col start)
	      (ending-row end) (ending-col end)))))

(defmethod place ((obj sbrt::place))
  (vector (sbrt::place-linenumber obj)
	  (sbrt::place-charnumber obj)
	  (sbrt::place-linenumber obj)
	  (sbrt::place-charnumber obj)))

(defmethod place ((obj vector))
  obj)

(defmethod place ((obj string))
  nil)

(defun place-list (obj)
  (coerce (place obj) 'list))

(defmethod place ((obj term::default-term))
  (term-place obj))

(defmethod place ((obj actual))
  (place (expr obj)))

(defmethod place (obj)
  (declare (ignore obj))
  nil)

(defmethod id-place-list ((ex fieldappl) &optional nplace)
  ;; id is first in name, but after the backquote in fieldappls
  ;; We basically go from the end
  (let ((place (or nplace (place ex))))
    (list (svref place 2) ; End row
	  (- (svref place 3)
	     (length (if (numberp (id ex))
			 (format nil "~a" (id ex))
			 (string (id ex)))))
	  (svref place 2)
	  (svref place 3))))


(defmethod id-place-list (name &optional nplace)
  (let ((place (or nplace (place name))))
    (list (svref place 0)
	  (svref place 1)
	  (svref place 0)
	  (+ (svref place 1)
	     (length (if (numberp (id name))
			 (format nil "~a" (id name))
			 (string (id name))))))))

(defun merge-places (place1 place2)
  (if (and place1 place2)
      (vector (min (starting-row place1) (starting-row place2))
	      (min (starting-col place1) (starting-col place2))
	      (max (ending-row place1) (ending-row place2))
	      (max (ending-col place1) (ending-col place2)))
      (or place1 place2)))

(defun concat-places (place1 place2)
  (if (and place1 place2)
      (vector (starting-row place1)
	      (starting-col place1)
	      (ending-row place2)
	      (ending-col place2))
      (or place1 place2)))

(defmethod place* ((ex application))
  (or (place ex)
      (place* (operator ex))))

(defmethod place* ((ex infix-application))
  (or (place ex)
      (place* (args1 ex))))

(defmethod place* (ex)
  (or (place ex)
      (and *place-error-flag*
	   (break "No place?"))))

(defun place-string (ex)
  (let ((place (place ex)))
    (when place
      (format nil "(at line ~d, column ~d)"
	(starting-row place) (starting-col place)))))

(defun valid-place? (place)
  (and (vectorp place)
       (>= (length place) 4)
       (let ((sr (svref place 0)) (sc (svref place 1))
	     (er (svref place 2)) (ec (svref place 3)))
	 (and (integerp sr) (integerp sc) (integerp er) (integerp ec)
	      (or (< sr er) (and (= sr er) (<= sc ec))))))) ; Empty string has sc = ec

(defun place< (place1 place2)
  (assert (and (valid-place? place1) (valid-place? place2)))
  (flet ((pt< (r1 c1 r2 c2) (or (< r1 r2) (and (= r1 r2) (< c1 c2)))))
    (let ((sr1 (svref place1 0)) (sc1 (svref place1 1))
	  (er1 (svref place1 2)) (ec1 (svref place1 3))
	  (sr2 (svref place2 0)) (sc2 (svref place2 1))
	  (er2 (svref place2 2)) (ec2 (svref place2 3)))
      (when (pt< sr1 sc1 sr2 sc2)
	(cond ((pt< er1 ec1 sr2 sc2) (values t :strict))
	      ((pt< er1 ec1 er2 ec2) (values t :overlap))
	      (t (values t :contains)))))))

(defun type-ambiguity (obj)
  (if (and (slot-exists-p obj 'resolutions)
	   (resolutions obj))
      (let ((reses (mapcar #'format-resolution (resolutions obj))))
	(type-error obj
	  "~a does not uniquely resolve - one of:~{~2%  ~a~^,~}"
	  (unparse obj :string t) reses))
      (let ((obstr (unparse obj :string t)))
	(type-error obj
	  "~a~:[ ~;~%~]does not have a unique type - one of:~{~%  ~a~^,~}~% ~a"
	  obstr
	  (or (> (length obstr) 20)
	      (find #\newline obstr))
	  (unique-full-names (ptypes obj))
	  (if (some #'fully-instantiated? (ptypes obj))
	      (if (not (every #'fully-instantiated? (ptypes obj)))
		  "(Some of these are not fully instantiated)"
		  "")
	      (if (= (length (ptypes obj)) 2)
		  "(Neither of these is fully instantiated)"
		  "(None of these are fully instantiated)"))))))

(defun unique-full-names (names &optional depth)
  "Uses full-name on names till a difference shows up.  This is potentially
very slow, really only good for error messages over small lists of names."
  (let ((strings (mapcar #'(lambda (name)
			     (str (if depth (full-name name depth) name)))
		   names)))
    (if (duplicates? strings :test #'string=)
	(unique-full-names names (if depth (1+ depth) 1))
	strings)))

(defun format-resolution (res)
  (format nil "~@[~a@~]~@[~a~]~@[[~{~a~^,~}]~]~@[~I~<{{~;~@{~W~^, ~:_~}~;}}~:>~].~a~@[ : ~a~]"
    (let* ((decl (declaration res))
	   (th (if (and (recursive-type? decl)
			(not (inline-recursive-type? decl)))
		   decl
		   (module (declaration res)))))
      (when (lib-datatype-or-theory? th)
	(get-library-id (context-path th))))
    (when (module-instance res)
      (id (module-instance res)))
    (mapcar #'(lambda (act) (unparse (full-name act 1) :string t))
      (actuals (module-instance res)))
    (when (module-instance res)
      (mappings (module-instance res)))
    (id (declaration res))
    (when (type-expr? (type res))
      (unparse (type res) :string t))))

(defun type-incompatible (expr types expected &optional argument)
  (let ((rtypes (when types
		  (mapcar #'(lambda (fn) (unpindent fn 12 :string t))
		    (full-name (remove-if #'symbolp types) 1))))
	(etype (when types
		 (unpindent (full-name expected 1) 12 :string t)))
	(*type-error-argument* argument))
    (cond (rtypes
	   (when (some #'(lambda (rtype) (string= rtype etype)) rtypes)
	     ;; They print the same - find the differences - hopefully in print-types
	     (multiple-value-bind (atypes eetype)
		 (expose-type-differences types expected)
	       (setq rtypes (mapcar #'(lambda (fn) (unpindent fn 12 :string t))
			      atypes))
	       (setq etype (unpindent eetype 12 :string t))))
	   (type-error expr
	     "Incompatible types for ~a~%     Found: ~{~a~%~^~12T~}  Expected: ~a"
	     expr rtypes etype))
	  (t
	   (type-error expr "Type provided where an expression is expected")))))

(defun expose-type-differences (types etype &optional atypes eetype)
  (if (null types)
      (values (nreverse atypes) eetype)
      (multiple-value-bind (atype eetype)
	  (expose-type-diffs (car types) etype)
	(expose-type-differences (cdr types) etype (cons atype atypes) eetype))))

(defun expose-type-diffs (type etype)
  (expose-type-diffs* type etype))

(defmethod expose-type-diffs* :around ((atype type-expr) (etype type-expr))
  (if (tc-eq atype etype)
      (values atype etype)
      (let ((natype (lcopy atype :print-type nil))
	    (netype (lcopy etype :print-type nil)))
	(if (string= (str natype) (str netype))
	    (call-next-method)
	    (values natype netype)))))

(defmethod expose-type-diffs* ((type tupletype) (etype tupletype))
  (multiple-value-bind (atypes etypes)
      (expose-type-diff-typelists (types type) (types etype))
    (values (mk-tupletype atypes) (mk-tupletype etypes))))

(defun expose-type-diff-typelists (types etypes &optional atypes eetypes)
  (if (null types)
      (values (nreverse atypes) (nreverse eetypes))
      (multiple-value-bind (atype etype)
	  (expose-type-diffs* (car types) (car etypes))
	(expose-type-diff-typelists
	 (cdr types) (cdr etypes) (cons atype atypes) (cons etype eetypes)))))

(defmethod expose-type-diffs* ((type funtype) (etype funtype))
  (multiple-value-bind (dtype detype)
      (expose-type-diffs* (domain type) (domain etype))
    (multiple-value-bind (rtype retype)
	(expose-type-diffs* (range type) (range etype))
      (values (mk-funtype dtype rtype) (mk-funtype detype retype)))))

(defmethod expose-type-diffs* (type etype)
  (values type etype))

(defun pvs-locate (theory obj &optional loc)
  (let ((place (coerce (or loc (place obj)) 'list)))
    (assert place)
    (when *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-loc ~a&~a&~a :end-pvs-loc"
		(cond ((lib-datatype-or-theory? theory)
		       (get-library-id (context-path theory)))
		      ((datatype-or-module? theory)
		       (if (from-prelude? theory)
			   (format nil "~a/lib/" *pvs-path*)
			   (protect-emacs-output
			    (shortname *default-pathname-defaults*)))))
		(if (datatype-or-module? theory)
		    (or (filename theory)
			"prelude.pvs")
		    theory)
		place)))
	(to-emacs)))))

(defun pvs-insert-declaration (decl after-decl &optional buf)
  (let* ((theory (module after-decl))
	 (aplace (place after-decl))
	 (indent (cadr aplace))
	 (dplace (list (1+ (caddr aplace)) 0 (1+ (caddr aplace)) 0))
	 (text (unpindent decl indent :string t))
	 (itext (format nil "~@[~%  % ~a~%~]~%~vt~a~%"
		  (and (slot-exists-p decl 'generated-by)
		       (generated-by decl)
		       (car (newline-comment decl)))
		  indent text)))
    (pvs-modify-buffer (unless buf (shortname *default-pathname-defaults*))
		       (or buf (id theory))
		       dplace
		       itext)))

(defun pvs-modify-buffer (dir name place contents)
  (if *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-mod ~a&~a&~a&~a :end-pvs-mod"
		(protect-emacs-output dir)
		name
		(place-list place)
		(when contents
		  (write-to-temp-file contents)))))
	(to-emacs))
      (format t "~%~a~%" contents)))

(defun set-pvs-tmp-file ()
  (let ((counter 0)
	(tmp-file-default (format nil "/tmp/pvs-~a"
			    (random 100000 (make-random-state t)))))
    (setq *pvs-tmp-file*
	  #'(lambda ()
	      (labels ((tmp-file ()
			 (let ((tfile (make-pathname
				       :type (format nil "p~d" (incf counter))
				       :defaults tmp-file-default)))
			   (if (file-exists-p tfile)
			       (tmp-file)
			       tfile))))
		(tmp-file))))))

(defun pvs-tmp-file ()
  (unless *pvs-tmp-file*
    (set-pvs-tmp-file))
  (funcall *pvs-tmp-file*))

(defmacro with-output-to-temp-file (&body body)
  (let ((tmp-file (gensym)))
    `(progn
      (unless *pvs-tmp-file* (set-pvs-tmp-file))
      (let ((,tmp-file (funcall *pvs-tmp-file*))
	    (*disable-gc-printout* t))
	(with-open-file (*standard-output* ,tmp-file :direction :output
					   :if-exists :error)
	  ,@body)
	(namestring ,tmp-file)))))

(defun write-to-temp-file (contents &optional escape)
  (with-output-to-temp-file
    (write contents :escape escape :pretty nil :level nil :length nil)))

(defun write-json-to-temp-file (contents)
  (let ((jstr (if (stringp contents)
		  contents
		  (pvs-encode-json-to-string contents))))
    (when (stringp contents)
      (assert (pvs-decode-json-from-string contents)))
    (write-to-temp-file jstr t)))

(in-package :ilisp)
(export '(ilisp-eval))
(defun ilisp-eval (form package filename)
  (let ((*package* (find-package package)))
    filename
    (eval (read-from-string form))))

(in-package :pvs)
