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


(in-package 'pvs)

(defvar *to-emacs* nil)
(defvar *output-to-emacs* "")

#-(or akcl harlequin-common-lisp)
(defmacro pvs-errors (form)
  "Handle Pvs errors when evaluating form"
  `(progn
    (princ " ")				; Make sure we have output
    (handler-case
	(let ((*to-emacs* t))
	  (setq *print-length* nil)
	  (setq *print-level* nil)
	  ,form)
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
	(format t "~%~?" ctl args))))


;;; Collect messages until the end of parsing/typechecking, and provide
;;; them to a buffer.

(defun pvs-warning (ctl &rest args)
  (if *noninteractive*
      (pvs-message "~% ~?~%" ctl args)
      (format t "~% ~?~%" ctl args))
  (when (and (current-theory)
	     (not *in-checker*))
    (let ((warning (format nil "~?" ctl args)))
      (if (warnings (current-theory))
	  (nconc (warnings (current-theory)) (list warning))
	  (setf (warnings (current-theory)) (list warning)))))
  nil)

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
  (if *noninteractive*
      (pvs-message "~% ~?~%" ctl args)
      (format t "~% ~?~%" ctl args))
  (when (and (current-theory)
	     (not *in-checker*))
    (let ((info (format nil "~?" ctl args)))
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
               ~:{~2%Messages for theory ~a:~@{~{~2%~a~}~}~}"
		filename
		(mapcar #'(lambda (th) (list (id th) (info th))) theories))
	      t t)
	    (pvs-message "No messages associated with ~a.pvs" filename))
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
  (cond (*in-pvs-batch* t)
	(*to-emacs*
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
  (if *to-emacs*
      (let* ((*print-pretty* nil)
	     (*output-to-emacs*
	      (format nil ":pvs-pmt ~a&~? :end-pvs-pmt"
		type (protect-emacs-output msg) args)))
	(to-emacs)
	(read))
      (progn (format t "PVS> ~?" msg args)
	     (read))))

(defun pvs-emacs-eval (form)
  (let* ((*print-pretty* nil)
	 (*output-to-emacs*
	  (format nil ":pvs-eval ~a :end-pvs-eval" form)))
    (to-emacs)
    (read)))

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
      (protect-emacs-output* string 0)
      string))

(defun protect-emacs-output* (string pos &optional result)
  (if (< pos (length string))
      (protect-emacs-output*
       string
       (1+ pos)
       (case (char string pos)
	 (#\& (append '(#\& #\\) result))
	 (#\\ (append '(#\\ #\\) result))
	 (#\newline (append '(#\n #\\) result))
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
  (cond ((and *to-emacs*
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
	       (format t "~%~?" message args)
	       (format t "~%~a" message))
	   (error "Parse error"))))


(defvar *type-error* nil)

(defun type-error (obj message &rest args)
  (let ((error (type-error-for-conversion obj message args)))
    (cond (*type-error-catch*
	   (throw *type-error-catch*
		  (values nil (set-strategy-errors
			       (format nil "~%~a" error)))))
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
	  (*in-evaluator*
	   (format t "~%~a" error)
	   (format t "~%Try again.")
	   (throw 'tcerror t))
	  (t (format t "~%~a" error)
	     (error "Typecheck error")))))

(defun type-error-for-conversion (obj message args)
  (let ((error (format nil
		   "~?~@[~%~a~]~:[~;~%You may need to add a semicolon (;) ~
                    to the end of the previous declaration~]"
		 message args *type-error* *in-coercion*)))
    (if (conversion-occurs-in? obj)
	(let* ((*type-error*
		(format nil
		    "--------------~%With conversions, ~
                                    it becomes the expression ~%  ~a~%~
                                    and leads to the error:~%  ~a"
		  obj error))
	       (*no-conversions-allowed* t)
	       (etype (type obj)))
	  (untypecheck-theory obj)
	  (typecheck obj :expected etype))
	error)))

(defun conversion-occurs-in? (obj)
  (let ((conv? nil))
    (mapobject #'(lambda (ex)
		   (when (typep ex '(or argument-conversion
					implicit-conversion
					lambda-conversion))
		     (setq conv? t)))
	       obj)
    conv?))

(defmethod place ((obj cons))
  (let ((start (place (car obj)))
	(end (place (car (last obj)))))
    (vector (starting-row start) (starting-col start)
	    (ending-row end) (ending-col end))))

(defmethod place ((obj sbrt::place))
  (vector (sbrt::place-linenumber obj)
	  (sbrt::place-charnumber obj)
	  (sbrt::place-linenumber obj)
	  (sbrt::place-charnumber obj)))

(defmethod place ((obj vector))
  obj)

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
	  (mapcar #'(lambda (ty) (unparse ty :string t)) (ptypes obj))
	  (if (some #'fully-instantiated? (ptypes obj))
	      (if (not (every #'fully-instantiated? (ptypes obj)))
		  "(Some of these are not fully instantiated)")
	      (if (= (length (ptypes obj)) 2)
		  "(Neither of these is fully instantiated)"
		  "(None of these are fully instantiated)"))))))

(defun format-resolution (res)
  (format nil "~@[~a~]~@[~a~]~@[[~{~a~^,~}]~].~a~@[ : ~a~]"
    (let ((th (module (declaration res))))
      (when (typep th 'library-theory)
	(or (cdr (assoc (library th) *library-alist* :test #'equal))
	    (namestring (library th)))))
    (when (module-instance res)
      (id (module-instance res)))
    (mapcar #'(lambda (act) (unparse (full-name act 1) :string t))
      (actuals (module-instance res)))
    (id (declaration res))
    (when (type-expr? (type res))
      (unparse (type res) :string t))))

(defun type-incompatible (expr types expected)
  (let ((rtypes (remove-if #'symbolp types)))
    (if rtypes
	(type-error expr
	  "Incompatible types~%     Found: ~{~a~%~^~12T~}  Expected: ~a"
	  (mapcar #'(lambda (fn) (unpindent fn 12 :string t))
		  (full-name types 1))
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
      (let ((,tmp-file (funcall *pvs-tmp-file*)))
	(with-open-file (*standard-output* ,tmp-file :direction :output
					   :if-exists :error)
	  ,@body)
	(namestring ,tmp-file)))))

(defun write-to-temp-file (contents &optional escape)
  (with-output-to-temp-file
    (write contents :escape escape :pretty nil :level nil :length nil)))
