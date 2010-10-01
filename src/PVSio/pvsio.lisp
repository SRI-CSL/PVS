;; pvsio.lisp
;; PVSio interface to the ground evaluator
;; Release : PVSio-2.c (09/16/05)

;; New PVSio interface

(in-package :pvs)

;;(defun arity (expr)
;;   (if (funtype? (type expr))
;;       (length (types (domain (type expr))))
;;     0))

(defun help-pvsio ()
  (format 
   t 
   "~%Enter a PVS ground expression followed by ';' at the <PVSio> prompt")
  (format t "~%  OR ")
  (format 
   t 
   "~%Enter a Lisp expression followed by a '!' at the <PVSio> prompt~%")
  (format t "~%The following special commands can be followed by either ';' or '!':
  help                 : Print this message
  quit                 : Exit the evaluator with confirmation
  exit                 : Exit the evaluator without confirmation
  timing               : Turn on timing information per evaluation
  notiming             : Turn off timing information
  tccs                 : Turn on TCCs generation per evaluation 
  notccs               : Turn off TCCs generation
  load_pvs_attachments : Force a reload .pvs-attachments and pvs-attachments
  pvsio_version        : Show current version of PVSio
  list_attachments     : List semantic attachments loaded in the current 
                         context

Display help for <attachment>:
  (help_pvs_attachment <attachment>)!
  help_pvs_attachment(<attachment>);

Display help for semantic attachments in <theory>:
  (help_pvs_theory_attachments <theory>)!
  help_pvs_theory_attachments(<theory>);

ACKNOWLEDGMENT
PVS is a software developed, maintained, and licensed by SRI International.
PVSio is a freely available extension to the PVS Ground Evaluator developed 
by Cesar Munoz at the National Institute of Aerospace.

"))

(defun evaluation-mode-pvsio (theoryname 
			      &optional input tccs?  
			      append? (banner? t))
  ;;(load-pvsio-library-if-needed)
  (let ((theory (get-typechecked-theory theoryname)))
    (format t "~%Generating ~a.log~%" theoryname)
    (with-open-file 
	(*error-output*
	 (merge-pathnames (format nil "~a.log" theoryname))
	 :direction :output 
	 :if-does-not-exist :create
	 :if-exists (if append? :append :supersede))
      (unwind-protect
	  (if theory
	      (let ((*current-theory* theory)
		    (*generate-tccs* (if tccs? 'all 'none))
		    (*current-context* (or (saved-context theory)
					   (context nil)))
		    (*suppress-msg* t)
		    (*in-evaluator* t)
		    (*destructive?* t)
		    (*eval-verbose* nil)
		    (*compile-verbose* nil)
		    (*convert-back-to-pvs* t)
		    (input-stream (when input 
				    (make-string-input-stream input))))
		(when banner?
		  (format t "
+---- 
| ~a
|
| Enter a PVS ground expression followed by a symbol ';' at the <PVSio> prompt.
| Enter a Lisp expression followed by a symbol '!' at the <PVSio> prompt.
|
| Enter help! for a list of commands and quit! to exit the evaluator.
|
| *CAVEAT*: evaluation of expressions which depend on unproven TCCs may be 
| unsound, and result in the evaluator crashing into lisp, running out of 
| stack, or worse. If you crash into lisp, type (restore) to resume.
|
+----~%" *pvsio-version*))
		(evaluate-pvsio input-stream))
	      (pvs-message "Theory ~a is not typechecked" theoryname))
	(pvs-emacs-eval "(pvs-evaluator-ready)")))))

(defun read-expr (input-stream)
  (catch 'pvsio-command
    (do ((have-real-char nil)
	 (have-first-space nil)
	 (instr nil)
	 (fstr  (make-string-output-stream))
	 (c     (read-char input-stream nil nil)
		(read-char input-stream nil nil)))
	((and (eq c #\;)
	      (not instr))
	 (string-trim '(#\Space #\Tab #\Newline)
		      (get-output-stream-string fstr)))
      (when (null c) (throw 'quit nil))
      (when (and (not instr)
		 have-real-char
		 (not have-first-space)
		 (member c '(#\Space #\Newline #\Tab) :test #'char=))
	(let ((pref (get-output-stream-string fstr)))
	  (cond ((member pref '("(lisp" "(pvs::lisp")
			 :test #+allegro #'string= #-allegro #'string-equal)
		 (let ((input (read input-stream nil nil)))
		   (format t "~%~s~2%<PVSio> " (eval input)))
		 (loop until (or (null c) (char= c #\)))
		       do (setq c (read-char-no-hang input-stream nil nil)))
		 (setq c #\Space)
		 (setq have-real-char nil
		       have-first-space nil))
		(t (loop for ch across pref do (write-char ch fstr))
		   (setq havespace t)))))
      (when (and (eq c #\!) (not instr))
	(clear-input)
	(throw 'pvsio-command 
	       (read-from-string 
		(get-output-stream-string fstr))))
      (if have-real-char
	  (write-char c fstr)
	  (unless (member c '(#\Space #\Newline #\Tab) :test #'char=)
	    (write-char c fstr)
	    (setq have-real-char t)))
      (when (eq c #\") (setq instr (not instr))))))

(defun evaluate-pvsio (input-stream)
  (let ((result
	 (multiple-value-bind 
	  (val err)
	  (progn ;;ignore-errors
	   (catch 'abort
	     (catch 'quit
	       (catch 'tcerror
		 (let* ((input ;;(ignore-errors (read-pvsio input-stream))
			       (read-pvsio input-stream))
			(pr-input (pc-parse input 'expr))
			(*tccforms* nil)
			(tc-input (pc-typecheck pr-input))
			(isvoid (and tc-input 
				     (type-name? (type tc-input))
				     (string= "void" 
					      (format 
					       nil "~a" 
					       (print-type (type tc-input)))))))
		   (when *evaluator-debug*
		     (format t "typechecks to:~%")
		     (show tc-input))
		   (when *tccforms*
		     (format t "~%Typechecking ~s produced the following TCCs:~%" input)
		     (evaluator-print-tccs *tccforms*)
		     (format 
		      t 
		      "~%~%Evaluating in the presence of unproven TCCs may be unsound~%")
		     (clear-input)
		     (unless (pvs-y-or-n-p "Do you wish to proceed with evaluation?")
		       (throw 'abort t)))
		   (multiple-value-bind 
		    (cl-input error)
		    (catch 'no-defn (pvs2cl tc-input))
		    (when (eq cl-input 'cant-translate)
		      (format t "~s could not be translated:~%~a" input error)
		      (throw 'abort t))
		    (when *evaluator-debug*
		      (format t "~a translates to~% ~s~%" tc-input cl-input))
		    (multiple-value-bind 
		     (cl-eval error)
		     (catch 'undefined
		       (if *pvs-eval-do-timing*
			   (time (eval cl-input))
			 (eval cl-input)))
		     (if (not error)
			 (let ((clval (if *convert-back-to-pvs*
					  (catch 'cant-translate
					    (cl2pvs cl-eval (type tc-input)))
					cl-eval)))
			   (when (not isvoid)
			     (fresh-line)
			     (format t "==> ~%")
			     (cond ((and clval *convert-back-to-pvs*)
				    (unparse clval))
				   (t
				    (when *convert-back-to-pvs*
				      (format 
				       t 
				       "Result not ground. Cannot convert back to PVS."))
				    (format t "~%~a" cl-eval)))))
		       (format t "~%~a" error))))
		   t)))))
	  (if err
	      (progn (format t "~%~a~%" err)
		     (null input-stream))
	    val))))
    (when result
      (format t "~%")
      (evaluate-pvsio input-stream))))

(defun read-pvsio (input-stream)
  (when (not input-stream)
    (format t "~%<PVSio> ")
    (force-output))
  (let ((input ;;(ignore-errors (read-expr input-stream))
	       (read-expr input-stream)))
    (cond ((member input '(quit (quit) "quit") :test #'equal)
	   (clear-input)
	   (when (pvs-y-or-n-p "Do you really want to quit?  ")
	     (throw 'quit nil))
	   (read-pvsio input-stream))
	  ((member input '(exit (exit) "exit") :test #'equal)
	   (throw 'quit nil))
	  ((member input '(help "help") :test #'equal)
	   (help-pvsio)
	   (read-pvsio input-stream))
	  ((member input '(timing "timing") :test #'equal)
	   (setq *pvs-eval-do-timing* t)
	   (format t "Enabled printing of timing information~%")
	   (read-pvsio input-stream))
	  ((member input '(notiming "notiming") :test #'equal)
	   (setq *pvs-eval-do-timing* nil)
	   (format t "Disabled printing of timing information~%")
	   (read-pvsio input-stream))
	  ((member input '(tccs "tccs") :test #'equal)
	   (setq *generate-tccs* 'all)
	   (format t "Enabled TCCs generation~%")
	   (read-pvsio input-stream))
	  ((member input '(notccs "notccs") :test #'equal)
	   (setq *generate-tccs* 'none)
	   (format t "Disabled TCCs generation~%")
	   (read-pvsio input-stream))
          ((member input '(pvsio-version pvsio_version "pvsio_version") 
		   :test #'equal)
	   (format t "~a~%" *pvsio-version*)
	   (read-pvsio input-stream))
	  ((member input '(list-attachments list_attachments 
					    "list_attachments") 
		   :test #'equal)
	   (list-attachments)
	   (read-pvsio input-stream))
	  ((member input '(load-pvs-attachments load_pvs_attachments 
						"load_pvs_attachments") 
		   :test #'equal)
	   (load-pvs-attachments t)
	   (read-pvsio input-stream))
	  ((stringp input) input)
	  (t (format t "~a" (eval input))
	     (fresh-line)
	     (read-pvsio input-stream)))))

(defun load-pvsio-library-if-needed ()
  (unless (assoc "PVSio/" *prelude-libraries-files* :test #'string=)
    (if (pvs-y-or-n-p
	 "The PVSio library should be loaded first - do that now? ")
	(let ((*suppress-msg* t))
	  (load-prelude-library "PVSio"))
	(error "PVSio library not loaded"))))
