;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-hi.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.


;;;
;;; ILISP high level interface functions Lisp <-> Emacs
;;;

;;;%Eval/compile
(defun lisp-send-region (start end switch message status format
			       &optional handler)
  "Given START, END, SWITCH, MESSAGE, STATUS, FORMAT and optional
HANDLER send the region between START and END to the lisp buffer and
execute the command defined by FORMAT on the region, its package and
filename.  If called with a positive prefix, the results will be
inserted at the end of the region.  If SWITCH is T, the command will
be sent and the buffer switched to the inferior LISP buffer.  if
SWITCH is 'call, a call will be inserted.  If SWITCH is 'result the
result will be returned without being displayed.  Otherwise the
results will be displayed in a popup window if lisp-wait-p is T and
the current-prefix-arg is not '- or if lisp-wait-p is nil and the
current-prefix-arg is '-.  If not displayed in a pop-up window then
comint-handler will display the results in a pop-up window if they are
more than one line long, or they are from an error.  STATUS will be
the process status when the command is actually executing.  MESSAGE is
a message to let the user know what is going on."
  (if (= start end) (error "Region is empty"))
  (let ((sexp (lisp-count-pairs start end ?\( ?\)))
	(string (buffer-substring start end)))
    (setq string
	  (format (ilisp-value format)
		  (lisp-slashify
		   (if (= sexp 1)
		       string
		       (format (ilisp-value 'ilisp-block-command) string)))
		  (lisp-buffer-package) (buffer-file-name)))
    (let ((result 
	   (ilisp-send
	    string message status
	    (cond ((memq switch '(t call)) switch)
		  ((or (not (eq lisp-wait-p (lisp-minus-prefix))) 
		       current-prefix-arg
		       (eq switch 'result)) nil)
		  (t 'dispatch))
	    handler)))

      (if result
	  (if current-prefix-arg
	      (save-excursion
		(goto-char end)
		(insert ?\n)
		(insert result))))
	    ;; Display the output in the usual way.
	    ;;(lisp-display-output result)))
      result)))

;;;%%Eval
(defun eval-region-lisp (start end &optional switch message status handler)
  "Evaluate the current region."
  (interactive "r")
  (setq message (or message 
		    (concat "Evaluate " (lisp-region-name start end))))
  (let ((defvar (ilisp-value 'ilisp-defvar-regexp t)))
    (if (and defvar
	     (save-excursion
	       (goto-char start)
	       (skip-chars-forward " \t\n")
	       (and (let ((case-fold-search t)) (looking-at defvar))
		    (progn (forward-sexp) (skip-chars-forward " \t\n" end)
			   (= (point) end)))))
	(lisp-send-region start end switch message (or status 'defvar)
			  'ilisp-defvar-command handler)
	(lisp-send-region start end switch message (or status 'eval)
			  'ilisp-eval-command handler))))

;;;
(defun eval-next-sexp-lisp (&optional switch)
  "Evaluate the next sexp."
  (interactive)
  (let (start end)
    (save-excursion
      (setq start (point))
      (forward-sexp)
      (setq end (point)))
    (eval-region-lisp start end switch
		      (format "Evaluate %s" (buffer-substring start end)))))

;;;
(defun eval-defun-lisp (&optional switch)
  "Evaluate the current form."
  (interactive)
  (let* ((form (lisp-defun-region-and-name))
	 (result
	  (eval-region-lisp (car form) (car (cdr form)) (or switch 'result)
			    (format "Evaluating %s" (car (cdr (cdr form)))))))))
    ;; Display the returned value. -fmw
    ;; no we don't... dave_sc ... (lisp-display-output result)))


;;;%%%And go
(defun eval-region-and-go-lisp (start end)
  "Evaluate the current region and switch to the current ILISP buffer."
  (interactive "r")
  (eval-region-lisp start end t))

(defun eval-next-sexp-and-go-lisp (&optional switch)
  "Evaluate the next sexp and switch to the current ILISP buffer."
  (interactive)
  (eval-next-sexp-lisp t))

(defun eval-defun-and-go-lisp ()
  "Evaluate the current defun and switch to the current ILISP buffer.
With prefix, insert a call as well."
  (interactive)
  (eval-defun-lisp (if current-prefix-arg 
		       (progn
			 (setq current-prefix-arg nil)
			 'call)
		       t)))

;;;%%Compile
(defun compile-region-lisp (start end &optional switch message status handler)
  "Compile the current region."
  (interactive "r")
  (lisp-send-region
   start end
   (or switch 'result)			; Default to return the result.
   (or message (concat "Compile " (lisp-region-name start end)))
   (or status 'compile)
   'ilisp-compile-command 
   handler))

    
;;;
(defun compile-defun-lisp (&optional switch)
  "Compile the current defun or the last command in the input-ring of
an ILISP buffer if no current defun."
  (interactive)
  (let* ((form (lisp-defun-region-and-name))
	 (start (car form))
	 (end (car (cdr form))))
    (if (and (= start end) (memq major-mode ilisp-modes))
	(save-excursion
	  (let ((form (ring-ref (ilisp-get-input-ring) 
				(ilisp-input-ring-index))))
	    (set-buffer "*ilisp-send*")
	    (delete-region (point-min) (point-max))
	    (insert form)
	    (compile-defun-lisp)))
      ;; Display the value returned by the compilation. -fmw
      (let* ((thing (car (cdr (cdr form))))
	     (result (compile-region-lisp start end (or switch 'result)
					  (format "Compiling %s" thing))))
	(lisp-display-output result)))))

;;;%%%And-go
(defun compile-region-and-go-lisp (start end)
  "Compile the current region and switch to the current ILISP buffer."
  (interactive "r")
  (compile-region-lisp start end t))

(defun compile-defun-and-go-lisp ()
  "Compile the current defun and switch to the current ILISP buffer."
  (interactive)
  (compile-defun-lisp 
   (if current-prefix-arg
       (progn
	 (setq current-prefix-arg nil)
	 'call)
       t)))

;;;
(defun compile-file-lisp (file-name &optional extension)
  "Compile a Lisp file in the current inferior LISP and go there."
  (interactive (comint-get-source
		"Compile Lisp file: " lisp-prev-l/c-dir/file
		lisp-source-modes nil))
  (comint-check-source file-name)	; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (ilisp-init t)
  ;; Ivan's hack for ange-ftp pathnames...
  (let ((file-name
	 (if (string-match "/.*?@.*:" file-name)
	     (substring file-name (match-end 0))
	   file-name)))
    (ilisp-send
     (format (ilisp-value 'ilisp-compile-file-command) file-name
	     (or extension (ilisp-value 'ilisp-binary-extension)))
     (concat "Compile " file-name) 'compile
     t)))


;;;
(defun ilisp-compile-inits ()
  "Compile the initialization files for the current inferior LISP dialect."
  (interactive)
  (ilisp-init t)
  (let ((files (ilisp-value 'ilisp-load-inits t)))
    (while files
      (compile-file-lisp 
       (expand-file-name (cdr (car files)) ilisp-directory)
       (ilisp-value 'ilisp-init-binary-extension t))
      (setq files (cdr files)))))


;;;
(defun close-and-send-lisp ()
  "Close and indent the current sexp then send it to the inferior
LISP." 
  (interactive)
  (reindent-lisp)
  (if (memq major-mode ilisp-modes)
      (return-ilisp)
      (eval-defun-lisp)))

;;;%Special commands
(defun describe-lisp (sexp)
  "Describe the current sexp using ilisp-describe-command.  With a
negative prefix, prompt for the expression.  If in an ILISP buffer,
and there is no current sexp, describe ilisp-last-command."
  (interactive
   (list
    (if (lisp-minus-prefix)
	(ilisp-read "Describe: " (lisp-previous-sexp t))
	(if (memq major-mode ilisp-modes)
	    (if (= (point)
		   (process-mark (get-buffer-process (current-buffer))))
		(or (ilisp-value 'ilisp-last-command t)
		    (error "No sexp to describe."))
		(lisp-previous-sexp t))
	    (lisp-previous-sexp t)))))
  (let ((result
	 (ilisp-send
	  (format (ilisp-value 'ilisp-describe-command) 
		  (lisp-slashify sexp) (lisp-buffer-package))
	  (concat "Describe " sexp)
	  'describe)))
    (lisp-display-output result)))

;;;
(defun inspect-lisp (sexp)
  "Inspect the current sexp using ilisp-inspect-command.  With a
prefix, prompt for the expression.  If in an ILISP buffer, and there
is no current sexp, inspect ilisp-last-command."
  (interactive
   (list
    (if current-prefix-arg
	(ilisp-read "Inspect: " (lisp-previous-sexp t))
	(if (memq major-mode ilisp-modes)
	    (if (= (point)
		   (process-mark (get-buffer-process (current-buffer))))
		(or (ilisp-value 'ilisp-last-command t)
		    (error "No sexp to inspect."))
		(lisp-previous-sexp t))
	    (lisp-previous-sexp t)))))
  (ilisp-send
   (format (ilisp-value 'ilisp-inspect-command) 
	   (lisp-slashify sexp) (lisp-buffer-package))
   (concat "Inspect " sexp)
   'inspect t))

;;;
(defun arglist-lisp (symbol)
  "Return the arglist of the currently looked at function.  With a
numeric prefix, the arglist will be inserted.  With a negative one,
the symbol will be prompted for."
  (interactive
   (let* ((function (lisp-function-name)))
     (list (if (lisp-minus-prefix)
	       (ilisp-read-symbol
		(format "Arglist [%s]: " (lisp-buffer-symbol function))
		function t)
	       function))))
  (if (null symbol)
      (error "No symbol")
      (let* ((arglist
	      (ilisp-send
	       (format (ilisp-value 'ilisp-arglist-command)
		       (lisp-symbol-name symbol) 
		       (lisp-symbol-package symbol))
	       nil
	       'args))
	     (position (string-match "(" arglist)))
	;; Insert just the stuff after the open paren,
	;; but display everything the inferior lisp prints.
	(cond ((and (not (ilisp-value 'comint-errorp t))
		    current-prefix-arg position)
	       (let ((temp (point)))
		 (insert (substring arglist (1+ position)))
		 (goto-char temp)))

	      (t
	       (lisp-display-output arglist))))))


;;;
(defun documentation-lisp (symbol type)
  "Return the documentation of the previous symbol using
ilisp-documentation-command.  If the symbol is at the start of a list,
it is assumed to be a function, otherwise variable documentation is
searched for.  With a minus prefix, prompt for the symbol and type.
With a numeric prefix always return the current function call
documentation."
  (interactive
   (if (lisp-minus-prefix)
       (let* ((symbol-info (lisp-previous-symbol))
	      (symbol (car symbol-info))
	      (doc (ilisp-read-symbol 
		    (format "Documentation [%s]: " 
			    (lisp-buffer-symbol symbol))
		    symbol))
	      (default (if (car (cdr symbol-info))
			   'function
			   'variable))
	      (types (ilisp-value 'ilisp-documentation-types t))
	      (type
	       (if types
		   (ilisp-completing-read
		    (if default
			(format "Type [%s]: " default)
			"Type: ")
		    types
		    default))))
	 (list doc (if (stringp type) (read type) type)))
       (if current-prefix-arg
	   (list (lisp-function-name) 'function)
	   (let* ((symbol-info (lisp-previous-symbol)))
	     (list (car symbol-info)
		   (if (car (cdr symbol-info))
		       'function
		       'variable))))))
  (lisp-display-output
   (ilisp-send
    (format (ilisp-value 'ilisp-documentation-command)
	    (lisp-symbol-name symbol) (lisp-symbol-package symbol) type)
    (format "Documentation %s %s" type (lisp-buffer-symbol symbol))
    'doc)))

;;;%%Macroexpand
(defun lisp-macroexpand-form ()
  "Return the next form for macroexpanding."
  (save-excursion
    (skip-chars-forward " \t\n")
    (let* ((begin (point))
	   (end (progn (forward-sexp) (point)))
	   (form (buffer-substring begin end)))
      (list
       (if (lisp-minus-prefix)
	   (ilisp-read "Macroexpand: " form)
	   form)))))

;;;
(defun macroexpand-lisp (form &optional top)
  "Macroexpand the next sexp until it is no longer a macro.  With a
prefix, insert into buffer."
  (interactive (lisp-macroexpand-form))
  (if (string-match "(\\([^ \t\n)]*\\)" form)
      (let ((message (concat "Macroexpand"
			     (if top "-1 " " ")
			     (substring form
					(match-beginning 1)
					(match-end 1))))
	    result)
	(setq result
	      (ilisp-send
	       (format
		(ilisp-value
		 (if top
		     'ilisp-macroexpand-1-command
		     'ilisp-macroexpand-command))
		(lisp-slashify form)
		(lisp-buffer-package)
		(buffer-file-name))
	       message 'expand))
	(if current-prefix-arg
	    (save-excursion (forward-sexp) (insert ?\n) (insert result))
	    (lisp-display-output result)))
      (error "Not a form: %s" form)))

(defun macroexpand-1-lisp (form)
  "Macroexpand the next sexp once.  With a prefix, insert into buffer."
  (interactive (lisp-macroexpand-form))
  (macroexpand-lisp form t))



;;;%%Trace
(defun trace-defun-lisp-break (function)
  "Trace FUNCTION without arg, untrace with.  Prompt for function with
negative prefix.  Default function is the current defun.  
Trace with :break set."
  (interactive
   (let ((function (lisp-defun-name)))
     (if (lisp-minus-prefix)
	 (list (ilisp-read-symbol
		(format (if current-prefix-arg 
			    "Untrace [%s]: "
			    "Trace [%s]: ")
			(lisp-buffer-symbol function))
		function
		t))
	 (list function))))
  (trace-defun-lisp-internal function (not current-prefix-arg)))

(defun trace-defun-lisp (function)
  "Trace FUNCTION without arg, untrace with.  Prompt for function with
negative prefix.  Default function is the current defun."
  (interactive
   (let ((function (lisp-defun-name)))
     (if (lisp-minus-prefix)
	 (list (ilisp-read-symbol
		(format (if current-prefix-arg 
			    "Untrace [%s]: "
			    "Trace [%s]: ")
			(lisp-buffer-symbol function))
		function
		t))
	 (list function))))
  (trace-defun-lisp-internal function nil))

(defun trace-defun-lisp-internal (function breakp)
  (cond (function
	  (let ((result
		  (ilisp-send
		    (if current-prefix-arg
			(format (ilisp-value 'ilisp-untrace-command)
				(lisp-symbol-name function)
				(lisp-symbol-package function))
		      (format (ilisp-value 'ilisp-trace-command)
			      (lisp-symbol-name function)
			      (lisp-symbol-package function)
			      breakp))
		    (format "%srace %s" (if current-prefix-arg "Unt" "T") 
			    (lisp-buffer-symbol function))
		    (if current-prefix-arg 'untrace 'trace)
		    ;; Change to always wait, so we can see the result.  -fmw, 10/13/93
		    ;; (if lisp-wait-p nil 'dispatch)
		    nil)))
	    ;; Display the value returned -fmw
	    (lisp-display-output result)))
	(t
	  (error "No function to %strace" (if current-prefix-arg "un" "")))))



;;;%%Default-directory
(defun default-directory-lisp (&optional buffer)
  "Set the inferior LISP default directory to the default directory of
optional BUFFER.  If you are in an inferior LISP buffer, set the
default directory to the current directory of the LISP."
  (interactive)
  (if (and (not buffer) (memq major-mode ilisp-modes))
      (let ((dir
	     (ilisp-send
	      (ilisp-value 'ilisp-directory-command)
	      (format "Getting LISP directory")
	      'dir)))
	(if (ilisp-value 'comint-errorp t)
	    (progn
	      (lisp-display-output dir)
	      (error "Error getting directory"))
	    (setq default-directory (read dir)
		  lisp-prev-l/c-dir/file (cons default-directory nil))
	    (message "Default directory is %s" default-directory)))
      (let ((directory (save-excursion
			 (set-buffer (or buffer (current-buffer)))
			 default-directory)))
	(ilisp-send 
	 (format (ilisp-value 'ilisp-set-directory-command) directory)
	 (format "Set %s's directory to %s" 
		 (buffer-name (ilisp-buffer)) directory)
	 'dir
	 ;; (if lisp-wait-p nil 'dispatch)
	 ;; The above line might cause problems with Lispworks.
	 ;; I just set the default to 'nil'. It shouldn't harm.
	 ;; Marco Antoniotti: Jan 2 1995.
	 ))))
  

;;;
(defun load-file-lisp (file-name)
  "Load a lisp file into the current inferior LISP and go there."
  (interactive (comint-get-source "Load Lisp file: " lisp-prev-l/c-dir/file
				  lisp-source-modes nil))
  (comint-check-source file-name)	; Check to see if buffer needs saved.
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (ilisp-init t)
  (let* ((extension (ilisp-value 'ilisp-binary-extension t))
	 (binary (lisp-file-extension file-name extension)))
    (save-excursion
      (set-buffer (ilisp-buffer))
      (if (not (eq comint-send-queue comint-end-queue))
	  (if (y-or-n-p "Abort commands before loading? ")
	      (abort-commands-lisp)
	      (message "Waiting for commands to finish")
	      (while (not (eq comint-send-queue comint-end-queue))
		(accept-process-output)
		(sit-for 0))))
      (if (and (car (comint-send-variables (car comint-send-queue)))
	       (y-or-n-p "Interrupt top level? "))
	  (let ((result (comint-send-results (car comint-send-queue))))
	    (interrupt-subjob-ilisp)
	    (while (not (cdr result))
	      (accept-process-output)
	      (sit-for 0)))))
    (if (file-newer-than-file-p file-name binary)
	(if (and (not ilisp-load-no-compile-query)
		 extension (y-or-n-p "Compile first? "))
	    ;; Load binary if just compiled
	    (progn
	      (message "")
	      (compile-file-lisp file-name)
	      (setq file-name binary)))
	;; Load binary if it is current
	(if (file-readable-p binary) (setq file-name binary)))
    (switch-to-lisp t t)

	;; Ivan's hack for ange-ftp pathnames...
	(let ((file-name
		   (if (string-match "/.*?@.*:" file-name)
			   (substring file-name (match-end 0))
			   file-name)))
	  (comint-sender
	   (ilisp-process)
	   (format (ilisp-value 'ilisp-load-command) file-name))
	  (message "Loading %s" file-name))))



;;;%Source
;;;%File operations
;;;
(defun lisp-find-file (file &optional pop no-name)
  "Find FILE, optionally POPping.
If optional NO-NAME is nil, and there is a buffer with a name that is
the same as the final pathname component, select that instead of
reading the file associated with the full path name.  If the expanded
name of FILE and buffer match, select that buffer."  

  (let* ((buffers (buffer-list))
	 (position 0)
	 (expand-symlinks t)
	 (expanded (expand-file-name file))
	 filename)
    (if (not no-name)
	(progn (while (string-match "/" file position)
		 (setq position (match-end 0)))
	       (setq filename (substring file position))))
    (while buffers
      (save-excursion 
	(set-buffer (car buffers))
	(let* ((name (and (not no-name) (buffer-name)))
	       (buffer-file (buffer-file-name))
	       (buffer-expanded
		(cdr 
		 (if (string-equal buffer-file (car lisp-buffer-file)) 
		     lisp-buffer-file
		     (setq lisp-buffer-file
			   (cons buffer-file 
				 (expand-file-name buffer-file)))))))
	  (if (or (and name (string-equal filename name))
		  (string-equal expanded buffer-expanded))
	      (setq file buffer-file
		    buffers nil)
	      (setq buffers (cdr buffers)))))))
  (if pop
      (lisp-pop-to-buffer (find-file-noselect file))
      (find-file file)))

;;;
(defun find-file-lisp (file-name)
  "Find a file.
If point is on a string that points to an existing
file, that will be the default.  If the buffer is one of
lisp-source-modes, the buffer file will be the default.  Otherwise,
the last file used in a lisp-source-mode will be used."
  (interactive
   (comint-get-source "Find file: "
		      lisp-prev-l/c-dir/file
		      lisp-source-modes nil))
  (setq lisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				     (file-name-nondirectory file-name)))
  (lisp-find-file file-name nil t))
