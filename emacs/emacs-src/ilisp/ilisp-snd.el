;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-snd.el --

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
;;; ILISP send and support.
;;;


;;;%% Package / Symbol support
;;;
(defun lisp-buffer-package ()
  "Return the package for this buffer.  The package name is a string.
If there is none, return NIL.  This caches the package unless
ilisp-dont-cache-package is non-nil, so calling this more than once
is cheap."
  (cond ((and (not (eq buffer-package 'not-yet-computed))
	      (null lisp-dont-cache-package)) 
	 buffer-package)
	(ilisp-completion-package ilisp-completion-package)
	(lisp-dont-cache-package
	 ;; Refind the package each time.
	 (let ((package (lisp-buffer-package-internal nil)))
	   (message "")
	   (setq buffer-package 'not-yet-computed)
	   (if package
	       (setq mode-name
		     (concat 
		      (or buffer-mode-name 
			  (setq buffer-mode-name mode-name))
		      ":" package)))
	   package))
	((or lisp-buffer-package 
	     (memq major-mode ilisp-modes)
	     (not (memq major-mode lisp-source-modes)))
	 nil)
	(t
	 (make-local-variable 'buffer-package)
	 (make-local-variable 'buffer-mode-name)
	 (let ((package (lisp-buffer-package-internal t)))
	   (message "")
	   (setq buffer-package package)
	   ;; Display package in mode line
	   (if package 
	       (setq mode-name
		     (concat (or buffer-mode-name
				 (setq buffer-mode-name mode-name))
			     ":" buffer-package)))
	   buffer-package))))

(defun lisp-buffer-package-internal (search-from-start)
  "Returns the package of the buffer.  If SEARCH-FROM-START is T then
will search from the beginning of the buffer, otherwise will search
backwards from current point."
  (setq mode-line-process 'ilisp-status)
  (let* ((lisp-buffer-package t)
	 (case-fold-search t)
	 (regexp (ilisp-value 'ilisp-package-regexp t))
	 (spec
	  (if regexp
	      (save-excursion
		(if (or (and search-from-start
			     (goto-char (point-min))
			     (re-search-forward regexp nil t))
			(re-search-backward regexp nil t))
		    (buffer-substring (match-beginning 0)
				      (progn 
					(goto-char (match-beginning 0))
					(forward-sexp)
					(point)))))))
	 (str  (format (ilisp-value 'ilisp-package-command) spec))
	 (package
	  (if spec
	      (ilisp-send 
	       str
	       "Finding buffer package"
	       'pkg))))
    (if (ilisp-value 'comint-errorp t)
	(progn
	  (lisp-display-output package)
	  (error "No package"))
	(if (and package 
		 ;; There was a bug here, used to have the second *
		 ;; outside of the parens.
		 (string-match "[ \n\t:\"]*\\([^ \n\t\"]*\\)" package))
	    (setq package
		  (substring package
			     (match-beginning 1) (match-end 1)))))
    package))

;;;
(defun package-lisp ()
  "Show current inferior LISP package."
  (interactive)
  (message "Inferior LISP package is %s"
	   (ilisp-send (ilisp-value 'ilisp-package-name-command)
		       "Finding inferior LISP package" 'pkg)))

;;;
(defun set-package-lisp (package)
  "Set inferior LISP to package of buffer or a named package with prefix."
  (interactive 
   (let ((default (lisp-buffer-package)))
     (if (or current-prefix-arg (null default))
	 (let ((name
		(read-string
		 (format "Package [%s]: " (lisp-buffer-package)) "")))
	   (list (if (equal name "") default name)))
	 (list default))))
  (if package
      (ilisp-send (format (ilisp-value 'ilisp-in-package-command) package)
		  (format "Set %s's package to %s" 
			  (buffer-name (ilisp-buffer))
			  package)
		  'pkg 'dispatch)
      (error "No package")))

;;;
(defun set-buffer-package-lisp (package)
  "Reset the current package of the current buffer.  With prefix
specify manually."
  (interactive (if current-prefix-arg
		   (list (read-from-minibuffer "Package: " ))
		   (list nil)))
  (if package
      (setq buffer-package package
	    mode-name (concat (or buffer-mode-name mode-name) ":" package))
      (setq buffer-package 'not-yet-computed)
      (lisp-buffer-package)))



;;;%Interface functions
;;;%%Symbols
(defun lisp-string-to-symbol (string)
  "Convert STRING to a symbol, (package delimiter symbol) where the
package is either package:symbol or from the current buffer."
  (let* ((start (string-match ":+" string))
	 (end (if start (match-end 0))))
    (if start
	(lisp-symbol
	 (if (= start 0)
	     ""
	     (substring string 0 start))
	 (substring string start end)
	 (substring string end))
	(let ((package (lisp-buffer-package)))
	  (lisp-symbol package (if package "::") string)))))

;;;
(defun lisp-symbol-to-string (symbol)
  "Convert SYMBOL to a string."
  (apply 'concat symbol))

;;;
(defun lisp-buffer-symbol (symbol)
  "Return SYMBOL as a string qualified for the current buffer."
  (let ((symbol-name (lisp-symbol-name symbol))
	(pkg (lisp-symbol-package symbol))
	(delimiter (lisp-symbol-delimiter symbol)))
    (cond ((string= pkg (lisp-buffer-package)) symbol-name)
	  ((string= pkg "") (concat ":" symbol-name))
	  (pkg (concat pkg delimiter symbol-name))
	  (t symbol-name))))

;;;
(defun lisp-previous-symbol (&optional stay)
  "Return the immediately preceding symbol as ((package delimiter symbol)
function-p start end).  If STAY is T, the end of the symbol will be point."
  (save-excursion
    (if (or (and (memq major-mode ilisp-modes)
		 (= (point) (process-mark (get-buffer-process
					   (current-buffer)))))
	    (progn
	      (skip-chars-backward " \t\n")
	      (or (bobp) (memq (char-after (1- (point))) '(?\) ?\")))))
	nil
	(let* ((delimiters (ilisp-value 'ilisp-symbol-delimiters))
	       (end (progn
		      (if (not stay) (skip-chars-forward delimiters))
		      (point)))
	       (start (progn
			(skip-chars-backward delimiters)
			(point)))
	       (prefix (if (not (bobp)) (1- start)))
	       (function-p
		(and prefix
		     (or (eq (char-after prefix) ?\()
			 (and (eq (char-after prefix) ?')
			      (not (bobp))
			      (eq (char-after (1- prefix)) ?#)))
		     (not (looking-at "[^: \t\n]*:*\\*[^ \t\n]")))))
	  (cons (lisp-string-to-symbol (buffer-substring start end))
		(list function-p start end))))))


;;;
(defun lisp-function-name ()
  "Return the previous function symbol.  This is either after a #' or
at the start of the current sexp.  If there is no current sexp, return
nil."
  (save-excursion
    (let ((symbol (lisp-previous-symbol)))
      (if (car (cdr symbol))
	  (car symbol)
	  (condition-case ()
	      (if (and (memq major-mode ilisp-modes)
		       (= (point)
			  (process-mark 
			   (get-buffer-process (current-buffer)))))
		  nil
		  (backward-up-list 1)
		  (down-list 1)
		  (lisp-string-to-symbol
		   (buffer-substring (point) 
				     (progn (forward-sexp 1) (point)))))
	    (error nil))))))


;;;
(defun lisp-defun-name ()
  "Return the name of the current defun."
  (save-excursion
    (lisp-defun-begin)
    (lisp-string-to-symbol (lisp-def-name t))))


;;;%% ILISP initializations
;;;
(defun ilisp-initialized ()
  "Return T if the current inferior LISP has been initialized."
  (memq (buffer-name (ilisp-buffer)) ilisp-initialized))

;;;
(defun ilisp-load-init (dialect file)
  "Add FILE to the files to be loaded into the inferior LISP when
dialect is initialized.  If FILE is NIL, the entry will be removed."
  (let ((old (assoc dialect ilisp-load-inits)))
    (if file
	(if old
	    (rplacd old file)
	    (setq ilisp-load-inits (nconc ilisp-load-inits 
					  (list (cons dialect file)))))
	(if old (setq ilisp-load-inits (delq old ilisp-load-inits))))))

;;;
(defun ilisp-binary (init var)
  "Initialize VAR to the result of INIT if VAR is NIL."
  (if (not (ilisp-value var t))
      (let ((binary (ilisp-value init t)))
	(if binary
	    (comint-send 
	     (ilisp-process) binary
	     t nil 'binary nil 
	     (` (lambda (error wait message output last)
		  (if (or error
			  (not (string-match "\"[^\"]*\"" output)))
		      (progn
			(lisp-display-output output)
			(abort-commands-lisp "No binary"))
		      (setq (, var)
			    (substring output
				       (1+ (match-beginning 0))
				       (1- (match-end 0))))))))))))

;;;
(defun ilisp-done-init ()
  "Make sure that initialization is done and if not dispatch another check."
  (if ilisp-load-files
      (comint-send-code (get-buffer-process (current-buffer))
			'ilisp-done-init)
      (if ilisp-initializing
	  (progn
	    (unless (and noninteractive (= pvs-verbose 0))	    
	      (message "Finished initializing %s" (car ilisp-dialect)))
	    (setq ilisp-initializing nil
		  ilisp-initialized
		  (cons (buffer-name (current-buffer)) ilisp-initialized))))))

;;;
(defun ilisp-init-internal (&optional sync)
  "Send all of the stuff necessary to initialize."
  (unwind-protect
       (progn
	 (if sync
	     (comint-sync
	      (ilisp-process)
	      "\"Start sync\""  "[ \t\n]*\"Start sync\""
	      "\"End sync\""    "\"End sync\""))
	 (ilisp-binary 'ilisp-binary-command 'ilisp-binary-extension)
	 (ilisp-binary 'ilisp-init-binary-command 
		       'ilisp-init-binary-extension)
	 ;; This gets executed in the process buffer                              ;;; ILISP-INIT FIXME
	 (comint-send-code
	  (ilisp-process)
	  (function (lambda ()
	    (let ((files ilisp-load-inits)
		  (done nil))
	      (unwind-protect
		   (progn
		     (if (not ilisp-init-binary-extension)
			 (setq ilisp-init-binary-extension 
			       ilisp-binary-extension))
		     (while files
		       (ilisp-load-or-send
			(expand-file-name 
			 (cdr (car files)) ilisp-directory))
		       (setq files (cdr files)))
		     (comint-send-code (ilisp-process)
				       'ilisp-done-init)
		     (setq done t))
		(if (not done)
		    (progn
		      (setq ilisp-initializing nil)
		      (abort-commands-lisp))))))))

	 (set-ilisp-value 'ilisp-initializing t)) ; progn

    (if (not (ilisp-value 'ilisp-initializing t))
	(abort-commands-lisp))))

;;;
(defun ilisp-init (&optional waitp forcep sync)
  "Initialize the current inferior LISP if necessary by loading the
files in ilisp-load-inits.  Optional WAITP waits for initialization to
finish.  When called interactively, force reinitialization.  With a
prefix, get the binary extensions again."  
  (interactive 
   (list (if current-prefix-arg
	     (progn
	       (set-ilisp-value 'ilisp-init-binary-extension nil)
	       (set-ilisp-value 'ilisp-binary-extension nil)
	       nil))
	 t))
  (if (or forcep (not (ilisp-initialized)))
      (progn
	(unless (and noninteractive (= pvs-verbose 0))
	  (message "Started initializing ILISP"))
	(if (not ilisp-directory)
	    (setq ilisp-directory (or (ilisp-directory "ilisp.elc" load-path)
				      (ilisp-directory "ilisp.el" load-path))))
	(if (not (ilisp-value 'ilisp-initializing t))
	    (ilisp-init-internal sync))
	(if waitp
	    (while (ilisp-value 'ilisp-initializing t)
	      (accept-process-output)
	      (sit-for 0))))))

;;;
(defun ilisp-init-and-sync ()
  "Synchronize with the inferior LISP and then initialize."
  (ilisp-init nil nil t))



;;;
(defun call-defun-lisp (arg)
  "Put a call of the current defun in the inferior LISP and go there.
If it is a \(def* name form, look up reasonable forms of name in the
input history unless called with prefix ARG. If not found, use \(name
or *name* as the call.  If is not a def* form, put the whole form in
the buffer."
  (interactive "P")
  (if (save-excursion (lisp-defun-begin) (looking-at "(def"))
      (let* ((symbol (lisp-defun-name))
	     (name (lisp-symbol-name symbol))
	     (package (if (lisp-symbol-package symbol)
			  (concat "\\("
				  (lisp-symbol-package symbol) ":+\\)?")))
	     (variablep (string-match "^\\*" name))
	     (setfp (string-match "(setf \\([^\)]+\\)" name)))
	(switch-to-lisp t t)
	(cond (setfp 
	       (setq name 
		     (substring name (match-beginning 1) (match-end 1)))
	       (lisp-match-ring (if (not arg)
				    (concat "(setf[ \t\n]*(" 
					    package name "[ \t\n]"))
				(concat "(setf (" name)))
	      (variablep (lisp-match-ring (if (not arg) 
					      (concat package name))
					  name))
	      (t
	       (let ((fun (concat "(" name)))
		 (setq name (regexp-quote name))
		 (or (lisp-match-ring 
		      (if (not arg) (concat "(" package name "[ \t\n\)]"))
		      fun 
		      (not arg))
		     (lisp-match-ring (concat "(" package
					      "[^ \t\n]*-*" name)
				      fun))))))
    (let ((form 
	   (save-excursion
	     (buffer-substring (lisp-defun-begin) 
			       (lisp-end-defun-text t)))))
      (switch-to-lisp t t)
      (comint-kill-input)
      (insert form))))



;;;
(defun ilisp-send (string &optional message status and-go handler)
  "Send STRING to the ILISP buffer, print MESSAGE set STATUS and
return the result if AND-GO is NIL, otherwise switch to ilisp if
and-go is T and show message and results.  If AND-GO is 'dispatch,
then the command will be executed without waiting for results.  If
AND-GO is 'call, then a call will be generated. If this is the first
time an ilisp command has been executed, the lisp will also be
initialized from the files in ilisp-load-inits.  If there is an error,
comint-errorp will be T and it will be handled by HANDLER."
  (ilisp-init t)
  (let ((process (ilisp-process))
	(dispatch (eq and-go 'dispatch)))
    (if message
	(message "%s" (if dispatch
			  (concat "Started " message)
			  message)))
    ;; No completion table
    (setq ilisp-original nil)
    (if (memq and-go '(t call))
	(progn (comint-send process string nil nil status message handler)
	       (if (eq and-go 'call)
		   (call-defun-lisp nil)
		   (switch-to-lisp t t))
	       nil)
	(let* ((save (ilisp-value 'ilisp-save-command t))
	       (result
		(comint-send 
		 process
		 (if save (format save string) string)
		 ;; Interrupt without waiting
		 t (if (not dispatch) 'wait) status message handler)))
	  (if save 
	      (comint-send
	       process
	       (ilisp-value 'ilisp-restore-command t)
	       t 'dispatch 'restore "Restore" t t))          ;;; nil -> 'dispatch by SO.
	  (if (not dispatch)
	      (progn
		(while (not (cdr result))
		  (sit-for 0)                  ;;; SO had this commented out
		  (accept-process-output))
		(comint-remove-whitespace (car result))))))))



;;;
(defun ilisp-load-or-send (file)
  "Try to load FILE into the inferior LISP.  If the file is not
accessible in the inferior LISP as determined by
ilisp-load-or-send-command, then visit the file and send the file over
the process interface."
  (let* ((command
	  (format (ilisp-value 'ilisp-load-or-send-command) 
		  (lisp-file-extension
		   file 
		   (ilisp-value 'ilisp-init-binary-extension t))
		  file)))
    (set-ilisp-value 'ilisp-load-files 
		     (nconc (ilisp-value 'ilisp-load-files t) (list file)))
    (comint-send
     (ilisp-process) command t nil 'load
     (format "Loading %s" file)
     (function (lambda (error wait message output last)
       (let* ((file (lisp-last ilisp-load-files))
	      (process (get-buffer-process (current-buffer)))
	      (case-fold-search t))
	 (if (and output 
		  (string-match "nil" (car (lisp-last-line output))))
	     (let* ((old-buffer (get-file-buffer file))
		    (buffer (find-file-noselect file))
		    (string (save-excursion
			      (set-buffer buffer)
			      (buffer-string))))
	       (if (not old-buffer) (kill-buffer buffer))
	       (if (string= "" string)
		   (abort-commands-lisp (format "Can't find file %s" file))
		   (comint-send
		    process
		    (format ilisp-block-command string)
		    t nil 'send (format "Sending %s" file)
		    (function (lambda (error wait message output last)
		      (if error
			  (progn 
			    (comint-display-error output)
			    (abort-commands-lisp
			     (format "Error sending %s"
				     (lisp-last ilisp-load-files))))
			  (setq ilisp-load-files
				(delq (lisp-last ilisp-load-files)
				      ilisp-load-files))))))))
	       (if error (ilisp-handler error wait message output last))
	       (setq ilisp-load-files (delq file ilisp-load-files)))))))))
