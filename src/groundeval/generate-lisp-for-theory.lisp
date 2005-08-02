;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(defvar *pvs-eval-do-timing* nil)
(defvar *convert-back-to-pvs* nil)

(defun generate-lisp-for-prelude (&optional force?)
  (with-open-file (output (format nil "~a/lib/prelude.lisp" *pvs-path*)
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format output ";;; Lisp file generated from PVS prelude~2%")
    (format output "(in-package :pvs)~%")
    (dolist (theory *prelude-theories*)
      (pvs2cl-theory theory force?)
      (dolist (decl (theory theory))
	(when (and (const-decl? decl) (eval-info decl))
	  (write-decl-symbol-table decl output)))
      (dolist (decl (theory theory))
	(when (and (const-decl? decl) (eval-info decl))
	  (write-decl-defns decl output))))))

(defun generate-lisp-for-pvs-file (filename &optional force?)
  (let ((theories (cdr (gethash filename *pvs-files*))))
    (with-open-file (output (format nil "~a.lisp" filename)
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output ";;; Lisp file generated from ~a.pvs~2%" filename)
      (format output
	  ";;; In general for a definiton foo in an ~
               unparameterized~%;;; theory th, the names are:~
           ~%;;;    foo  - takes no arguments, returns a unary closure~
           ~%;;;   _foo  - the nondestructive version of the function~
           ~%;;;    foo! - the destructive version of the function")
      (format output
	  ";;; If the definition appears in a parameterized theory th, ~
               additional functions are generated ~%;;; that take arguments ~
               corresponding to the theory parameters, take names are:~
           ~%;;;    th_foo  - takes no arguments, returns a unary closure~
           ~%;;;   _th_foo  - the nondestructive version of the function~
           ~%;;;    th_foo! - the destructive version of the function")
      (format output
	  "~%;;; Function names must be unique, so a number may be appended, ~
            and the type~%;;; is included for functions associated with ~
            datatypes.~%;;; For these functions, the mappings are given here.")
      (dolist (theory theories)
	(pvs2cl-theory theory force?))
      (dolist (theory theories)
	(dolist (decl (theory theory))
	  (when (and (const-decl? decl) (eval-info decl))
	    (write-decl-symbol-table decl output))))
      (format output "(in-package :pvs)~%")
      (dolist (theory theories)
	(dolist (decl (theory theory))
	  (when (and (const-decl? decl) (eval-info decl))
	    (write-decl-defns decl output)))))))
  

(defun generate-lisp-for-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  (t (pvs2cl-theory theory)
	     (print-lisp-defns theoryname (format nil "~a.lisp" theoryname)
			       t)))))

(defun evaluation-mode (theoryname)
  (let ((theory (get-theory theoryname)))  
    (unwind-protect
	(if theory
	    (let ((*current-theory* theory)
		  (*generate-tccs* 'all)
		  (*current-context* (or (saved-context theory)
					 (context nil)))
		  (*in-evaluator* t)
		  (*pvs-eval-do-timing* t)
		  (*destructive?* t)
		  (*convert-back-to-pvs* t))
	      (format t "~%~%PVS Ground Evaluation.~%Enter a ground expression in quotes at the <GndEval> prompt.~%Type help for a list of commands.~%")
	      (format t "~%*CAVEAT*: evaluation of expressions which depend on unproven TCCs may be~%unsound, and result in the evaluator crashing into lisp, running out of~%stack, or worse.  If you crash into lisp, type (restore) to resume.~%")
	      (evaluate))
	    (pvs-message "Theory ~a is not typechecked" theoryname))
      (pvs-emacs-eval "(pvs-evaluator-ready)"))))

(defun gqread ()
  (format t "~%<GndEval> ")
  (force-output)
  (let ((input (ignore-errors (read))))
    (cond ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
	       (throw 'quit nil)
	       (gqread)))
	  ((and (consp input)
		(member (car input) '(lisp pvs::lisp)))  ;; allow for us to be interrupted
	   (format t "~%~s~%" (eval (cadr input)))
	   (gqread))
	  ((member input '(h help)
		   :test #'equal)
	   (format t "~%Evaluator commands:~%")
	   (format t "  h,help         : print this message~%")
	   (format t "  q,quit         : exit the evaluator~%")
	   (format t "  timing         : print timing information for each evaluation~%")
	   (format t "  notiming       : turn off printing of timing information~%")
	   (format t "  destructive    : use destructive evaluation where possible~%")
	   (format t "  nondestructive : don't use destructive evaluation~%")
	   (format t "  convert        : convert resulting expressions back to PVS syntax~%")
	   (format t "  noconvert      : don't convert resulting expressions~%")
	   (format t "  verbose        : enable verbose compilation messages~%")
	   (format t "  quiet          : disable verbose compilation messages~%")
	   (format t "~%Current values are: ")
	   (unless *pvs-eval-do-timing* (format t "no"))
	   (format t "timing, ")
	   (unless *destructive?* (format t "non"))
	   (format t "destructive, ")
	   (unless *convert-back-to-pvs* (format t "no"))
	   (format t "convert, ")
	   (if *eval-verbose* (format t "verbose.") (format t "quiet."))
	   (format t "~%~%Use M-x pvs-lisp-theory to see the generated lisp for a PVS theory")
	   (format t "~%~%Evaluator input should be enclosed in double quotes~%")
	   (gqread))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (throw 'abort t)
	       (gqread)))
	  ((eq input 'timing)
	   (setq *pvs-eval-do-timing* t)
	   (format t "Enabled printing of timing information")
	   (gqread))
	  ((eq input 'notiming)
	   (setq *pvs-eval-do-timing* nil)
	   (format t "Disabled printing of timing information")
	   (gqread))
	  ((eq input 'destructive)
	   (format t "Using destructive evaluation where possible")
	   (setq *destructive?* t)
	   (gqread))
	  ((eq input 'nondestructive)
	   (format t "Disabled use of destructive evaluation")
	   (setq *destructive?* nil)
	   (gqread))
	  ((eq input 'convert)
	   (format t "Enabled conversion of result back to PVS syntax")
	   (setq *convert-back-to-pvs* t)
	   (gqread))
	  ((eq input 'noconvert)
	   (format t "Disabled conversion of result back to PVS syntax")
	   (setq *convert-back-to-pvs* nil)
	   (gqread))
	  ((eq input 'verbose)
	   (format t "Enabled verbose compiler messages")
	   (setq *eval-verbose* t)
	   (gqread))
	  ((eq input 'quiet)
	   (format t "Disabled verbose compiler messages")
	   (setq *eval-verbose* nil)
	   (gqread))
	  (t
	   input))))

(defparameter *default-random-test-size* 100)

(defparameter *default-random-test-count* 10)

(defun evaluate ()
  (let ((result
	 (catch 'abort
	   (catch 'quit
	     (catch 'tcerror
	       (let* ((raw-input (ignore-errors (gqread)))
		      (test? (and (consp raw-input)
				  (eq (car raw-input) 'test)))
		      (input (if test?
				 (cadr raw-input)
				 raw-input))
		      (pr-input (pc-parse input 'expr))
		      (*tccforms* nil)
		      (tc-input (pc-typecheck pr-input)))
		 (when *evaluator-debug*
		   (format t "typechecks to:~%")
		   (show tc-input))
		 (when *tccforms*
		   (format t "~%Typechecking ~s produced the following TCCs:~%"
		     input)
		   (let ((unproved-tccs (evaluate-tccs)))
		     (when unproved-tccs
		       (format t "~%~%Evaluating in the presence of unproven TCCs may be unsound~%")
		       (unless (pvs-y-or-n-p "Do you wish to proceed with evaluation?")
			 (throw 'abort t)))))
		 (when test?
		   (unless (or (null (cddr raw-input))
			       (posnat? (caddr raw-input)))
		     (format t "test count must be a positive integer")
		     (throw 'abort t))
		   (unless (or (null (cdddr raw-input))
			       (posnat? (cadddr raw-input)))
		     (format t "test size must be a positive integer")
		     (throw 'abort t)))
		 (if test?
		     (run-random-test
		      tc-input
		      (or (third raw-input) *default-random-test-count*)
		      (or (fourth raw-input) *default-random-test-size*)
		      (fifth raw-input)
		      (sixth raw-input)
		      (seventh raw-input))
		     (multiple-value-bind (cl-input error)
			 (catch 'undefined (pvs2cl tc-input))
		       (when (eq cl-input 'cant-translate)
			 (format t "~s could not be translated:~%~a" input error)
			 (throw 'abort t))
		       (when *evaluator-debug*
			 (format t "~a translates to~% ~s~%" tc-input cl-input))
		       (multiple-value-bind (cl-eval error)
			   (catch 'undefined
			     (if *pvs-eval-do-timing*
				 (time (eval cl-input))
				 (eval cl-input)))
			 (if (not error)
			     (let ((clval (if *convert-back-to-pvs*
					      (catch 'cant-translate
						(cl2pvs cl-eval (type tc-input)))
					      cl-eval)))
			       (format t "~%==> ~%")
			       (cond ((and clval *convert-back-to-pvs*)
				      (unparse clval))
				     (t
				      (when *convert-back-to-pvs*
					(format t "Result not ground.  Cannot convert back to PVS."))
				      (format t "~%~a" cl-eval))))
			     (format t "~%~a" error)))))
		 t))))))
    (when result
      (evaluate))))

(defun evaluate-tccs ()
  (let ((unproved nil))
    (dolist (tcc *tccforms*)
      (format t "~%~a TCC for ~a: ~a"
	(tccinfo-kind tcc) (tccinfo-expr tcc) (tccinfo-formula tcc))
      (multiple-value-bind (tcc-input error)
	  (catch 'undefined (pvs2cl (tccinfo-formula tcc)))
	(cond ((eq tcc-input 'cant-translate)
	       (push tcc unproved)
	       (format t "TCC could not be translated:~%~a" error))
	      (t (when *evaluator-debug*
		   (format t "~a translates to~% ~s~%"
		     (tccinfo-formula tcc) tcc-input))
		 (multiple-value-bind (tcc-eval error)
		     (catch 'undefined (eval tcc-input))
		   (cond ((not error)
			  (let ((tccval (catch 'cant-translate
					  (cl2pvs tcc-eval *boolean*))))
			    (format t "~%==> ~%")
			    (unparse tccval)
			    (unless (tc-eq tccval *true*)
			      (push tcc unproved))))
			 ((eq tcc-eval 'cant-translate)
			  (push tcc unproved)
			  (format t "~%TCC could not be translated:~%~a"
			    error))
			 (t (format t "~%~a" error)
			    (push tcc unproved))))))))
    unproved))

(defun evaluator-print-tccs (tccforms)
  (when tccforms
    (let ((tccform (car tccforms)))
      (format t "~%~a TCC for ~a: ~a"
	(tccinfo-kind tccform)
	(tccinfo-expr tccform)
	(tccinfo-formula tccform))
      (evaluator-print-tccs (cdr tccforms)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These operations evaluate a list of PVS expression strings and
;;print the values to a file.   Requested by Ajay Chander (3/23/03).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun eval-to-file (theoryname list-of-exprs filename)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (format t "~%Theory ~a is not typechecked." theoryname))
	  (t (let ((*current-context* (saved-context theory)))
	       (eval-to-file* list-of-exprs filename))))))

(defun eval-to-file* (exprs filename)
  (cond ((consp exprs)
	 (eval-expr-to-file (car exprs) filename)
	 (eval-to-file* (cdr exprs) filename))
	(t nil)))

(defun eval-expr-to-file (expr filename)      
  (let* ((*generate-tccs* 'all)
	 (*tccforms* nil)
	 (expr (pc-parse expr 'expr))
	 (expr (pc-typecheck expr)))
    (with-open-file (out filename
			 :direction :output
			 :if-exists :append
			 :if-does-not-exist :create)
      (format out "~%Evaluating: ~a" expr)
;;NSH: can be turned on if TCCs must be printed.      
;       (when *tccforms*
; 	(format out "~%Generated TCCs: ")
; 	(evaluator-print-tccs *tccforms*))
      (multiple-value-bind (cl-input error)
	  (catch 'undefined (pvs2cl expr))
	(cond ((eq cl-input 'cant-translate)
	       (format out "~%Expression ~s could not be translated: ~%~a"
		 expr error))
	      (t (multiple-value-bind (cl-eval error)
		     (eval cl-input)
		   (if (not error)
		       (let ((pvsval (catch 'cant-translate
				       (cl2pvs cl-eval (type expr)))))
			 (cond (pvsval
				(format out "~%Value: ~a~%~%" pvsval))
			       (t (format out "~%Can't convert back to PVS.")
				  (format out "Common Lisp value: ~s" cl-eval))))
		       (format out "~%~a" error)))))))))
