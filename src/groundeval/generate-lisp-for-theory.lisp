(in-package 'pvs)

(defvar *pvs-eval-do-timing* nil)
(defvar *convert-back-to-pvs* nil)


(defun generate-lisp-for-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  (t (pvs2cl-theory theory)
	     (print-lisp-defns theoryname (format nil "~a.lisp" theoryname) T)))))

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
	       (throw 'abort T)
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

(defun evaluate ()
  (let ((result
	 (catch 'abort
	   (catch 'quit
	     (catch 'tcerror
	       (let* ((input (ignore-errors (gqread)))
		      (pr-input (pc-parse input 'expr))
		      (*tccforms* nil)
		      (tc-input (pc-typecheck pr-input)))
		 (when *evaluator-debug*
		   (format t "typechecks to:~%")
		   (show tc-input))
		 (when *tccforms*
		   (format t "~%Typechecking ~s produced the following TCCs:~%" input)
		   (evaluator-print-tccs *tccforms*)
		   (format t "~%~%Evaluating in the presence of unproven TCCs may be unsound~%")
		   (unless (pvs-y-or-n-p "Do you wish to proceed with evaluation?")
		     (throw 'abort t)))
		 (multiple-value-bind (cl-input error)
		     (catch 'no-defn (pvs2cl tc-input))
		   (when (eq cl-input 'cant-translate)
		     (format t "~s could not be translated:~%~a" input error)
		     (throw 'abort t))
		   (when *evaluator-debug*
		     (format t "~a translates to ~a~%" tc-input cl-input))
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
			 (format t "~%~a" error))))
		 t))))))
    (when result
      (evaluate))))

(defun evaluator-print-tccs (tccforms)
  (when tccforms
    (let ((tccform (car tccforms)))
      (format t "~%~a TCC for ~a: ~a"
	(tccinfo-kind tccform)
	(tccinfo-expr tccform)
	(tccinfo-formula tccform))
      (evaluator-print-tccs (cdr tccforms)))))









