;;
;; pvsio.lisp
;; Release: PVSio-6.0 (12/12/12)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/PVSio
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; PVSio interface to the ground evaluator
;;

(in-package :pvs)

(defparameter *pvsio-promptin* "<PVSio> ")
(defparameter *pvsio-promptout* "==>~%")

(defun help-pvsio ()
  (format 
   t 
   "~%Enter a PVS ground expression followed by ';' at the prompt '~a'" *pvsio-promptin*)
  (format t "~%  OR ")
  (format 
   t 
   "~%Enter a Lisp expression followed by '!' at the prompt '~a'~%" *pvsio-promptin*)
  (format t "~%The following special commands can be followed by either ';' or '!':
  help                 : Print this message
  quit                 : Exit the evaluator with confirmation
  exit                 : Exit the evaluator without confirmation
  timing               : Turn on timing information per evaluation
  notiming             : Turn off timing information
  tccs                 : Turn on TCCs generation per evaluation 
  notccs               : Turn off TCCs generation
  load_pvs_attachments : Force a reload .pvs-attachments and pvs-attachments
  list_pvs_attachments : List semantic attachments loaded in the current 
                         context
  pvsio_version        : Show current version of PVSio

To display help for <attachment>:
  help_pvs_attachment(\"<attachment>\");

To display help for semantic attachments in <theory>:
  help_pvs_theory_attachments(\"<theory>\");

To change input prompt '~a':
  set_promptin(\"<newprompt>\");

To change output prompt '~a':
  set_promptout(\"<newprompt>\");
" *pvsio-promptin* *pvsio-promptout*))

(defun evaluation-mode-pvsio (theoryname 
			      &optional input tccs?  
			      append? (banner? t))
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
| Enter a PVS ground expression followed by ';' at the prompt '~a'.
| Enter a Lisp expression followed by '!' at the prompt '~a'.
|
| Enter 'help' for help and 'exit' to exit the evaluator. Follow
| these commands with either ';' or '!'.
|
| *CAVEAT*: evaluation of expressions which depend on unproven TCCs may be 
| unsound, and result in the evaluator crashing into Lisp, running out of 
| stack, or worse. If you crash into Lisp, type (restore) to resume.
|
+----~%" *pvsio-version* *pvsio-promptin* *pvsio-promptin*))
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
		   (format t "~%~s~2%~a" (eval input) *pvsio-promptin*))
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
	  (ignore-errors ;; Change to progn for debugging
	   (catch 'abort
	     (catch 'quit
	       (catch 'tcerror
		 (let* ((input (read-pvsio input-stream))
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
		     (unless (pvs-y-or-n-p "Do you wish to proceed with evaluation? ")
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
			     (format t *pvsio-promptout*)
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
    (format t "~%~a" *pvsio-promptin*)
    (force-output))
  (let ((input (read-expr input-stream)))
    (cond ((member input '(quit (quit) "quit") :test #'equal)
	   (clear-input)
	   (when (pvs-y-or-n-p "Do you really want to quit? ")
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
	  ((member input '(list-pvs-attachments list_pvs_attachments 
					    "list_pvs_attachments") 
		   :test #'equal)
	   (list-pvs-attachments)
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

(defun run-pvsio ()
  (let ((file (environment-variable "PVSIOFILE"))
	(time (read-from-string (environment-variable "PVSIOTIME")))
	(theory (environment-variable "PVSIOTHEORY"))
	(packlist (read-from-string (environment-variable "PVSIOPACK")))
	(verb (read-from-string (environment-variable "PVSIOVERB")))
	(tccs (read-from-string (environment-variable "PVSIOTCCS")))
	(main (read-from-string (environment-variable "PVSIOMAIN")))
	(*pvsio-promptin* (environment-variable "PVSIOPROMPTIN"))
	(*pvsio-promptout* (environment-variable "PVSIOPROMPTOUT"))
	(*pvsio-version* (environment-variable "PVSIOVERSION")))
    (unless (probe-file (format nil "~a.pvs" file))
      (format t "File not found: ~a.pvs~%" file)
      (bye 1))
    (when time (setq *pvs-eval-do-timing* t))
    (multiple-value-bind 
	(val err)
	(progn 
	  (with-open-file 
	      (*standard-output*
	       (format nil "~a~a.log" (directory-namestring file) theory)
	       :direction :output
	       :if-does-not-exist :create
	       :if-exists :supersede)
	    (change-context (directory-namestring file))
	    (dolist (pack packlist) (load-prelude-library pack))
	    (load-pvs-attachments)
	    (when (null verb)
	      (format t "~%Typechecking")
	      (unwind-protect
		  (typecheck-file (file-namestring file) nil nil nil t)
		(fresh-line)
		(finish-output))))
	  (when verb
	    (typecheck-file (file-namestring file) nil nil nil t))
	  (evaluation-mode-pvsio theory main tccs t (null main))
	  t)
      (when err (format t "~%~a (~a.pvs). See ~a~a.log~%"
		  err file (directory-namestring file) theory)))
    (fresh-line)
    (bye 0)))

