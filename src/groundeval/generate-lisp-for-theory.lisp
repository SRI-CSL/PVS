(in-package 'pvs)

(defun generate-lisp-for-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond ((null theory)
	   (pvs-message "Theory ~a is not typechecked" theoryname))
	  (t (pvs2cl-theory theory)
	     (print-lisp-defns theoryname (format nil "~a.lisp" theoryname) T)))))

(defun evaluation-mode (theoryname &optional symbolic?)
  (let ((theory (get-theory theoryname)))  
    (if theory
      (let ((*current-theory* theory)
	    (*generate-tccs* 'none)
	    (*current-context* (or (saved-context theory)
				   (context nil)))
	    (*in-evaluator* t))
	(format t "~%~%PVS Ground Evaluation.~%Enter a ground expression in quotes at the <GndEval> prompt~%")
	(evaluate symbolic?))
      (pvs-message "Theory ~a is not typechecked" theoryname)))
  (pvs-emacs-eval "(pvs-evaluator-ready)"))

(defun gqread (prompt)
  (format t "~%~a"  prompt)
  (force-output)
  (let ((input (ignore-errors (read))))
    (cond ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
	       (throw 'quit nil)
	       (gqread prompt)))
	  ((and (consp input)
		(member (car input) '(lisp pvs::lisp)))  ;; allow for us to be interrupted
	   (format t "~%~s~%" (eval (cadr input)))
	   (gqread prompt))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (throw 'abort T)
	       (gqread prompt)))
	  ((eq input 'timing)
	   (setq *pvs-eval-do-timing* t)
	   (gqread prompt))
	  ((eq input 'notiming)
	   (setq *pvs-eval-do-timing* nil)
	   (gqread prompt))
	  ((eq input 'destructive)
	   (setq *destructive?* t)
	   (gqread prompt))
	  ((eq input 'nondestructive)
	   (setq *destructive?* nil)
	   (gqread prompt))
	  ((eq input 'ground)
	   (evaluate)
	   (throw 'quit nil))
	;  ((eq input 'symbolic)
	;   (evaluate t)
	;   (throw 'quit nil))
	  (t
	   input))))

(defun evalprompt (&optional symbolic?)
  (if symbolic?
      "<SymEval> "
      "<GndEval> "))

(defun evaluate (&optional symbolic?)
  (let ((result
	 (catch 'abort
	   (catch 'quit
	     (catch 'tcerror
	       (let* ((input (ignore-errors (gqread (evalprompt symbolic?))))
		      (pr-input (pc-parse input 'expr))
		      (tc-input (pc-typecheck pr-input)))
		 (if (ground-type? (type tc-input))
		     (format t "~%==> ~%  ~a"
		       (if *pvs-eval-do-timing*
			   (time (if symbolic?
				     (norm tc-input)
				     (eval (pvs2cl tc-input))))
			   (eval (if symbolic?
				     (norm tc-input)
				     (pvs2cl tc-input)))))
		     (format t "~%Not a ground expression. Cannot evaluate"))
		 T))))))
	(when result
	  (evaluate symbolic?))))












