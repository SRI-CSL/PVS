(in-package 'pvs)

(defun generate-lisp-for-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  (t (pvs2cl-theory theory)
	     (print-lisp-defns theoryname (format nil "~a.lisp" theoryname) T)))))

(defun evaluation-mode (theoryname)
  (let ((theory (get-theory theoryname)))  
    (if theory
	(unwind-protect
	    (let ((*current-theory* theory)
		  (*generate-tccs* 'none)
		  (*current-context* (or (saved-context theory)
					 (context nil)))
		  (*in-evaluator* t))
	      (format t "~%~%PVS Ground Evaluation.~%Enter a ground expression in quotes at the <GndEval> prompt~%")
	      (evaluate))
	  (pvs-emacs-eval "(pvs-evaluator-ready)"))
	(pvs-message "Theory ~a is not typechecked" theoryname)))
  (pvs-emacs-eval "(pvs-evaluator-ready)"))

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
	   (format t "~%~%Evaluator input should be enclosed in double quotes~%")
	   (gqread))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (throw 'abort T)
	       (gqread)))
	  ((eq input 'timing)
	   (setq *pvs-eval-do-timing* t)
	   (gqread))
	  ((eq input 'notiming)
	   (setq *pvs-eval-do-timing* nil)
	   (gqread))
	  ((eq input 'destructive)
	   (setq *destructive?* t)
	   (gqread))
	  ((eq input 'nondestructive)
	   (setq *destructive?* nil)
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
		      (tc-input (pc-typecheck pr-input)))
		 (if (ground-expr? tc-input)
		     (format t "~%==> ~%  ~a"
		       (if *pvs-eval-do-timing*
			   (time (eval (pvs2cl tc-input)))
			   (eval (pvs2cl tc-input))))
		     (format t "~%Not a ground expression. Cannot evaluate"))
		 T))))))
    (when result
      (evaluate))))















