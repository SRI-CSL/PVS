;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-emacs.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec 16 02:42:01 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun May 28 03:13:00 1995
;; Update Count    : 19
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(export '(pvs-message pvs-error))

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

#-(or akcl harlequin-common-lisp)
(defmacro pvs-errors (form)
  "Handle PVS errors when evaluating form"
  `(progn
    (princ " ")				; Make sure we have output
    (handler-case
	(let ((*to-emacs* t))
	  (setq *print-length* nil)
	  (setq *print-level* nil)
	  (if (and *noninteractive*
		   *noninteractive-timeout*
		   ,(not (and (listp form)
			      (memq (car form) *prover-invoking-commands*))))
	      (mp:with-timeout (*noninteractive-timeout*
				(format t "Timed out!"))
			       ,form)
	      ,form))
	(string (error)
		(with-output-to-string (string)
		  (format string "PVS: ~a" error))))))

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

;;; This replaces ilisp-restore in pvs-init
(defun pvs-ilisp-restore ()
  "Restore the old result history."
  (declare (special / // /// + ++ +++ * ** - ilisp::*ilisp-old-result*))
  (setq // (pop ilisp::*ilisp-old-result*)
	** (first //)
	/  (pop ilisp::*ilisp-old-result*)
	*  (first /)
	++  (pop ilisp::*ilisp-old-result*)
	+   (pop ilisp::*ilisp-old-result*)
	-   (pop ilisp::*ilisp-old-result*))
  (pop ilisp::*ilisp-old-result*)
  nil)

;;; Writes out a message.  The message should fit on one line, and
;;; should contain no newlines.  For Emacs, it is intended to write to
;;; the minibuffer.

(defun pvs-message (ctl &rest args)
  (unless *suppress-msg*
    (if *to-emacs*
	(let* ((*print-pretty* nil)
	       (*output-to-emacs*
		(protect-emacs-output
		 (format nil ":pvs-msg ~? :end-pvs-msg" ctl args))))
	  (to-emacs))
	(format t "~%~?" ctl args)))
  nil)


;;; Collect messages until the end of parsing/typechecking, and provide
;;; them to a buffer.

(defun pvs-warning (ctl &rest args)
  (if *noninteractive*
      (pvs-message "~% ~?~%" ctl args)
      (format t "~% ~?~%" ctl args))
  (when (and (current-theory)
	     (not *in-checker*)
	     (not *in-evaluator*))
    (let ((warning (format nil "~?" ctl args)))
      (if (warnings (current-theory))
	  (nconc (warnings (current-theory)) (list warning))
	  (setf (warnings (current-theory)) (list warning)))))
  nil)

;;; Collect messages until the end of parsing/typechecking, and provide
;;; them to a buffer.

(defun show-theory-warnings (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  ((null (warnings theory))
	   (pvs-message "Theory ~a has no warning messages" theoryname))
	  (t (pvs-buffer "PVS Warnings"
	       (format nil "Warnings for theory ~a:~2%~{~a~^~2%~}"
		 theoryname (warnings theory))
	       t t)))))

(defun show-pvs-file-warnings (filename)
  (let ((theories (get-theories filename)))
    (if theories
	(if (some #'warnings theories)
	    (pvs-buffer "PVS Warnings"
	      (format nil
		  "Warnings for file ~a.pvs:~
               ~:{~2%Warnings for theory ~a:~@{~{~2%~a~}~}~}"
		filename
		(mapcar #'(lambda (th) (list (id th) (warnings th))) theories))
	      t t)
	    (pvs-message "No warnings associated with ~a.pvs" filename))
	(pvs-message "~a.pvs has not been typechecked" filename))))

(defun pvs-info (ctl &rest args)
  (let ((info (if args
		  (format nil "~?" ctl args)
		  ctl)))
    (when *noninteractive*
      (pvs-message "~% ~a~%" info)
      ;;(format t "~% ~a~%" info)
      )
    (when (and (current-theory)
	       (not *in-checker*)
	       (not *in-evaluator*))
      (if (info (current-theory))
	  (nconc (info (current-theory)) (list info))
	  (setf (info (current-theory)) (list info)))))
  nil)

(defun show-theory-messages (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  ((null (info theory))
	   (pvs-message "Theory ~a has no informational messages" theoryname))
	  (t (pvs-buffer "PVS Messages"
	       (format nil
		   "Messages for theory ~a:~
                    ~2%Use M-x show-theory-conversions to see the conversions.~
                    ~2%~{~a~^~2%~}"
		 theoryname
		 (info theory))
	       t t)))))

(defun show-pvs-file-messages (filename)
  (let ((theories (get-theories filename)))
    (if theories
	(if (some #'info theories)
	    (pvs-buffer "PVS Messages"
	      (format nil
		  "Messages for file ~a.pvs:~
                   ~2%Use M-x show-theory-conversions to see the conversions.~
               ~:{~2%Messages for theory ~a:~@{~{~2%~a~}~}~}"
		filename
		(mapcar #'(lambda (th) (list (id th) (info th))) theories))
	      t t)
	    (pvs-message "No messages associated with ~a.pvs" filename))
	(pvs-message "~a.pvs has not been typechecked" filename))))

;;; Conversions are treated separately

(defun pvs-conversion-msg (ctl &rest args)
  (when *noninteractive*
    (pvs-message "~% ~?~%" ctl args)
    ;;(format-if "~% ~?~%" ctl args)
    )
  (when (and (current-theory)
	     (not *in-checker*)
	     (not *in-evaluator*))
    (let ((cmsg (format nil "~?" ctl args)))
      (if (conversion-messages (current-theory))
	  (nconc (conversion-messages (current-theory)) (list cmsg))
	  (setf (conversion-messages (current-theory)) (list cmsg)))))
  nil)

(defun show-theory-conversions (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  ((null (conversion-messages theory))
	   (pvs-message "Theory ~a has no conversions" theoryname))
	  (t (pvs-buffer "PVS Conversions"
	       (format nil
		   "Conversions for theory ~a:~
                    ~2%Use pretty-print-expanded (M-x ppe) to see the conversions as used in the theory.
                    ~2%~{~a~^~2%~}"
		 theoryname (conversion-messages theory))
	       t t)))))

(defun show-pvs-file-conversions (filename)
  (let ((theories (get-theories filename)))
    (if theories
	(if (some #'conversion-messages theories)
	    (pvs-buffer "PVS Conversions"
	      (format nil
		  "Conversions for file ~a.pvs:~
                   ~2%Use pretty-print-expanded (M-x ppe) to see the conversions as used in the theory.
               ~:{~2%Conversions for theory ~a:~@{~{~2%~a~}~}~}"
		filename
		(mapcar #'(lambda (th) (list (id th) (conversion-messages th)))
		  theories))
	      t t)
	    (pvs-message "No conversions associated with ~a.pvs" filename))
	(pvs-message "~a.pvs has not been typechecked" filename))))

;;; Used to put messages in a log file

(defun pvs-log (ctl &rest args)
  (unless *suppress-msg*
    (let* ((*print-pretty* nil)
	   (*output-to-emacs*
	    (protect-emacs-output
	     (format nil ":pvs-log ~? :end-pvs-log" ctl args))))
      (to-emacs))))

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

(defun pvs-error (msg err &optional itheory iplace)
  ;; Indicates an error; no recovery possible.
  (if *rerunning-proof*
      (restore)
      (if *to-emacs*
	  (let* ((place (if *adt-decl* (place *adt-decl*) iplace))
		 (buff (if *adt-decl*
			   (or (filename *generating-adt*)
			       (and (current-theory)
				    (filename (current-theory)))
			       *current-file*)
			   (or *from-buffer* itheory)))
		 (*print-pretty* nil)
		 (*output-to-emacs*
		  (format nil ":pvs-err ~a&~a&~a&~a&~d ~d :end-pvs-err"
		    buff
		    (unless *from-buffer*
		      (protect-emacs-output (namestring *pvs-context-path*)))
		    (protect-emacs-output msg)
		    (write-to-temp-file err)
		    (line-begin place) (col-begin place))))
	    (to-emacs)
	    (if *in-checker*
		(restore)
		(pvs-abort)))
	  (progn (format t "~%~%~a~%~a" msg err)
		 (if *in-checker*
		     (restore)
		     (error "PVS error"))))))

(defun pvs-abort ()
  #-allegro (abort)
  #+allegro (tpl::reset-command))

(defmacro y-or-n-p-with-prompt (msg args)
  (clear-input)
  #+lucid `(y-or-n-p ,msg ,args)
  #-lucid `(y-or-n-p (format nil "~?(Y or N): " ,msg ,args)))

(defmacro yes-or-no-p-with-prompt (msg args)
  (clear-input)
  #+lucid `(apply #'yes-or-no-p ,msg ,args)
  #-lucid `(yes-or-no-p (format nil "~?(Yes or No) " ,msg ,args)))

(defun pvs-y-or-n-p (msg &rest args)
  (pvs-yn (apply #'format nil msg args) nil nil))

(defun pvs-yes-or-no-p (msg &rest args)
  (pvs-yn (apply #'format nil msg args) t nil))

(defun pvs-y-or-n-p-with-timeout (msg &rest args)
  (pvs-yn (apply #'format nil msg args) nil t))

(defun pvs-yes-or-no-p-with-timeout (msg &rest args)
  (pvs-yn (apply #'format nil msg args) t t))

(defun pvs-yn (msg full? timeout?)
  (cond (*noninteractive* t)
	((and *pvs-emacs-interface* *to-emacs*)
	 (let* ((*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-yn ~a&~a&~a :end-pvs-yn"
		   (protect-emacs-output msg)
		   (if full? "t" "nil")
		   (if timeout? "t" "nil"))))
	   (to-emacs)
	   (let ((val (read)))
	     (when (eq val :abort)
	       (pvs-message "Aborting")
	       (pvs-abort))
	     val)))
	(full? (yes-or-no-p-with-prompt msg nil))
	(t (y-or-n-p-with-prompt msg nil))))

(defun pvs-query (theory msg query place)
  (if *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-qry ~a&~a&~a&~a&~d ~d :end-pvs-qry"
		theory
		(protect-emacs-output (namestring *pvs-context-path*))
		(protect-emacs-output msg)
		(protect-emacs-output query)
		(if place (sbrt::place-linenumber place))
		(if place (sbrt::place-charnumber place)))))
	(to-emacs))
      (query theory msg query place)))

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
  (when *pvs-emacs-interface*
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

(defun pvs-buffer (name contents &optional display? read-only? append?)
  (if *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-buf ~a&~a&~a&~a&~a :end-pvs-buf"
		name (when contents (write-to-temp-file contents))
		display? read-only? append?)))
	(to-emacs))
      (if display?
	  (format t "~%~a" contents))))

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

; #-akcl
; (define-condition pvs-error (error)
;   (place)
;   (:report (lambda (condition stream)
; 	     (format stream "There is a PVS error at ~a"
; 		     (pvs-error-place condition)))))

(defun protect-emacs-output (string)
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
  (if (stringp string)
      (with-output-to-string (str)
	(loop for ch across string
	      do (case ch
		   (#\\ (write-char #\\ str) (write-char #\\ str))
		   (#\" (write-char #\\ str) (write-char #\" str))
		   (t   (write-char ch str)))))
      string))

(defun protect-string-output* (string pos &optional result)
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
  (cond (*parse-error-catch*
	 (throw *parse-error-catch*
		(values nil
			(if args
			    (format nil "~?" message args)
			    message)
			obj)))
	((and *to-emacs*
	      (or (not *in-checker*)
		  (not *in-evaluator*)
		  *tc-add-decl*))
	 (pvs-error "Parser error"
	   (if args
	       (format nil "~?" message args)
	       message)
	   *current-file*
	   (place obj)))
	((and (or *in-checker*
		  *in-evaluator*)
	      (not *tcdebug*))
	 (if args
	     (format t "~%~?" message args)
	     (format t "~%~a" message))
	 (format t "~%Restoring the state.")
	 (restore))
	(t (if args
	       (format t "~%~?~a~a"
		 message
		 args
		 (if *current-file*
		     (format nil "~%In file ~a" (pathname-name *current-file*))
		     "")
		 (if (place obj)
		     (format nil " (line ~a, col ~a)"
		       (line-begin (place obj))
		       (col-begin (place obj)))
		     ""))
	       (format t "~%~a~a~a"
		 message
		 (if *current-file*
		     (format nil "~%In file ~a" (pathname-name *current-file*))
		     "")
		 (if (place obj)
		     (format nil " (line ~a, col ~a)"
		       (line-begin (place obj))
		       (col-begin (place obj)))
		     "")))
	   (error "Parse error"))))


(defvar *type-error* nil)
(defvar *type-error-argument* nil)

(defun type-error (obj message &rest args)
  (let ((error (type-error-for-conversion obj message args)))
    (cond (*type-error-catch*
	   (throw *type-error-catch*
		  (values nil
			  (if (and (not *typechecking-actual*)
				   (or *in-checker*
				       *in-evaluator*))
			      (set-strategy-errors
			       (format nil "~a" error))
			      (format nil "~a" error))
			  obj)))
	  ((and *to-emacs*
		(or (not *in-checker*)
		    *tc-add-decl*))
	   (pvs-error "Typecheck error"
	     error
	     (or (and (current-theory)
		      (filename (current-theory)))
		 *current-file*)
	     (place obj)))
	  ((and *in-checker* (not *tcdebug*))
	   (format t "~%~a" error)
	   (format t "~%Restoring the state.")
	   (restore))
	  ((and *in-evaluator* (not *evaluator-debug*))
	   (format t "~%~a" error)
	   (format t "~%Try again.")
	   (throw 'tcerror t))
	  (t (format t "~%~a" error)
	     (error "Typecheck error")))))

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
		 args *type-error* *in-coercion*))
	(obj-conv? (conversion-occurs-in? obj)))
    (cond ((and *typechecking-module*
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
		  (etype (if obj-conv?
			     (type obj)
			     (when  (and (type obj)
					 (not (dep-binding? (domain (type obj)))))
			       (range (type obj))))))
	     (untypecheck-theory ex)
	     (if etype
		 (typecheck ex :expected etype)
		 (typecheck-uniquely ex))))
	  ((check-if-k-conversion-would-work obj nil)
	   (format nil
	       "~a~%Enabling K_conversion before this declaration might help"
	     error))
	  (t error))))

(defun check-if-k-conversion-would-work (ex arguments)
  (and (gethash "K_conversion" *prelude*)
       (not *skip-k-conversion-check*)
       *typechecking-module*
       (null *type-error-catch*)
       (not (some #'k-combinator? (conversions *current-context*)))
       (let ((*type-error-catch* 'type-error)
	     (cdecl (make-instance 'conversion-decl
		      'id '|K_conversion|
		      'module (get-theory "K_conversion")
		      'k-combinator? t
		      'expr (mk-name-expr '|K_conversion|))))
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

(defun term-place (absyn)
  (getf (term:term-attr absyn) :place))

(defmethod place ((obj actual))
  (place (expr obj)))

(defmethod place (obj)
  (declare (ignore obj))
  nil)

(defun merge-places (place1 place2)
  (if (and place1 place2)
      (vector (min (starting-row place1) (starting-row place2))
	      (min (starting-col place1) (starting-col place2))
	      (max (ending-row place1) (ending-row place2))
	      (max (ending-col place1) (ending-col place2)))
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
	  (mapcar #'(lambda (ty) (unparse (full-name ty 1) :string t)) (ptypes obj))
	  (if (some #'fully-instantiated? (ptypes obj))
	      (if (not (every #'fully-instantiated? (ptypes obj)))
		  "(Some of these are not fully instantiated)"
		  "")
	      (if (= (length (ptypes obj)) 2)
		  "(Neither of these is fully instantiated)"
		  "(None of these are fully instantiated)"))))))

(defun format-resolution (res)
  (format nil "~@[~a@~]~@[~a~]~@[[~{~a~^,~}]~].~a~@[ : ~a~]"
    (let ((th (module (declaration res))))
      (when (typep th 'library-theory)
	(libref-to-libid (lib-ref th))))
    (when (module-instance res)
      (id (module-instance res)))
    (mapcar #'(lambda (act) (unparse (full-name act 1) :string t))
      (actuals (module-instance res)))
    (id (declaration res))
    (when (type-expr? (type res))
      (unparse (type res) :string t))))

(defun type-incompatible (expr types expected &optional argument)
  (let ((rtypes (remove-if #'symbolp types))
	(*type-error-argument* argument))
    (if rtypes
	(type-error expr
	  "Incompatible types for ~a~%     Found: ~{~a~%~^~12T~}  Expected: ~a"
	  expr
	  (mapcar #'(lambda (fn) (unpindent fn 12 :string t))
		  (full-name rtypes 1))
	  (unpindent (full-name expected 1) 12 :string t))
	(type-error expr "Type provided where an expression is expected"))))

(defun pvs-locate (theory obj &optional loc)
  (let ((place (coerce (or loc (place obj)) 'list)))
    (assert place)
    (when *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-loc ~a&~a&~a :end-pvs-loc"
		(cond ((typep theory '(or library-theory library-datatype))
		       (library theory))
		      ((datatype-or-module? theory)
		       (if (from-prelude? theory)
			   (format nil "~a/lib/" *pvs-path*)
			   (protect-emacs-output
			    (shortname *pvs-context-path*)))))
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
    (pvs-modify-buffer (unless buf (shortname *pvs-context-path*))
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
