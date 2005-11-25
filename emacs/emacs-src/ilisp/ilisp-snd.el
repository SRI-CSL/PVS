;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-snd.el --
;;; ILISP send and support.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;%% Package / Symbol support

;;;---------------------------------------------------------------------------
;;; Package hacks by Martin Atzmueller
;;;
;;; 19990824 Marco Antoniotti

(defvar *ILISP-default-package* "COMMON-LISP-USER")

(defun ilisp-add-set-package-hook ()	; Was: add-set-package-hook
  (interactive)
  (add-hook 'lisp-mode-hook
	    '(lambda ()
	       ;; ilisp-buffer must exist and be ready
	       (if (and (boundp 'ilisp-buffer)
			(not (equal ilisp-buffer "*nil*"))
			(not (ilisp-value 'ilisp-initializing t))
			(ilisp-value 'ilisp-status) " :ready")

		   ;; and check the Package when in ILISP
		   (lisp-buffer-package)))))

;;; ilisp-check-package-advanced --
;;; treat DEFPACKAGE before IN-PACKAGE.

(defun ilisp-check-package-advanced (hash-defpackage-forms-list 
				     hash-in-package-forms-list)
  "Advanced check for packages in buffer.
It hanldes the special case of read-time conditionals - i.e. hash plus
or minus forms - as well as normal IN-PACKAGE or DEFPACKAGE forms."
  (let* ((string
          (apply #'concat (nconc hash-defpackage-forms-list
                                 hash-in-package-forms-list)))
         (string
	  (format (ilisp-value 'ilisp-package-command)
		  (format (ilisp-value 'ilisp-block-command) string)))
         (package
             ;;; (string &optional message status and-go handler)
           (let ((the-package
                   (ilisp-send
                    string "Finding Buffer package with hash-forms" 'pkg nil t)))
             ;; there may have been an error: fix it.
             (if (string-match (ilisp-value 'ilisp-error-regexp t) the-package)
               (progn
                 (comint-send (ilisp-process) (ilisp-value 'comint-fix-error) t t 'fix "Fixing erroneous package-situation in inferior-lisp." t nil)
                 "nil")
               (if the-package
                 the-package
                 "nil"))))
         (case-fold-search t)
         (npic-regexp (ilisp-value 'ilisp-no-package-in-core-regexp t)))
    (if (and npic-regexp (string-match npic-regexp package))
      (progn
        (message (format "Buffer package not found. Using fallback-package: %s"
                         (ilisp-value 'ilisp-fallback-package)))
        (values (ilisp-value 'ilisp-fallback-package) t))
      (progn
        (message "Buffer package: %s" package)
        (values package nil)))))

;;;
(defun lisp-find-hash-form ()		; Was: find-hash-form.
  "Tries to find either an hash-form, e.g. #{+|-}-form, or a regular
 in-package or defpackage form."
  (let ((hash-form-regexp (ilisp-value 'ilisp-hash-form-regexp))
	(in-package-regexp (ilisp-value 'ilisp-in-package-command-string t))
	(defpackage-regexp (ilisp-value 'ilisp-defpackage-command-string t)))
    (ignore-errors (backward-char))
    (and (re-search-forward hash-form-regexp)
	 (let ((found (buffer-substring
		       (progn
			 (match-end 0)	; this is due to problems with
			 (backward-char) ; the #{+|-}-regexp.
			 (backward-sexp) ; now we are in front of the exp
			 (point))
		       (match-end 0))))
	   
	   (cond ((or (prog1
                          (string-match in-package-regexp found)
                        (setq in-package-found-p t))
		      (string-match defpackage-regexp found))
		  (backward-char)
		  (buffer-substring (point) (progn (forward-sexp) (point))))
		 (t
		  (buffer-substring
		   (point)
		   (progn
		     (let ((post-form (buffer-substring (point)
							(progn
							  (forward-sexp)
							  (point))))
			   )
		       ;; This is for the case  '#{+|-}({'and'|'or'} expr*)'
		       (unless (string-match "^\\(#+[^(].\\)\\|\\(#-[^(].\\)"
					     post-form)
			 (forward-sexp))) ; after the #{+|-}-form
		     (forward-sexp)	; after the *real* Lisp-exp
		     (point)))))))))


;;; lisp-buffer-package-internal -- This function does not work well
;;; for Scheme yet.  After all the module system of Scheme is a system
;;; of very high  entropy.
;;;
;;; 19990824 Marco Antoniotti

(defun lisp-buffer-package-internal ()
  "Returns the package of the buffer.
If SEARCH-FROM-START is T then will search from the beginning of the
buffer, otherwise will search backwards from current point.  This
function also tries to correctly handle read-time
conditionals and the relative order of DEFPACKAGE and IN-PACKAGE for
Common Lisp."
  (interactive)
  (setq mode-line-process 'ilisp-status)
  (let* ((lisp-buffer-package t)
	 (case-fold-search t)
	 (hash-form-regexp (ilisp-value 'ilisp-hash-form-regexp t))
	 (in-package-regexp (ilisp-value 'ilisp-in-package-command-string t))
	 (defpackage-regexp (ilisp-value 'ilisp-defpackage-command-string t))
	 (hash-in-package-forms-list nil)
	 (hash-defpackage-forms-list nil)
         (in-package-found-p nil)
	 (package nil)
         (should-not-cache-p nil))
    (if (not hash-form-regexp)
        (values nil nil)
      (save-excursion
        (goto-char (point-min))

        (while
            (let* ((hash-expr
                    (ignore-errors (lisp-find-hash-form)))
                   (sub-expr
                    (and hash-expr
                         (string-match hash-form-regexp hash-expr)
                         (substring hash-expr (match-beginning 0)))))
              (when hash-expr
                (cond ((string-match "(in-package\\s-*)" hash-expr)
                       (setq should-not-cache-p t)
                       nil)
                      (t
                       (when (and sub-expr (string-match in-package-regexp sub-expr))
                         (setq in-package-found-p t)
                         (push hash-expr hash-in-package-forms-list))
                       (when (and sub-expr (string-match defpackage-regexp sub-expr))
                         (push hash-expr hash-defpackage-forms-list))
                        t)))))

        (multiple-value-bind (package package-not-in-core-p)
            (ilisp-check-package-advanced
             (nreverse hash-defpackage-forms-list) 
             (nreverse hash-in-package-forms-list))
          (let ((should-not-cache-p (or should-not-cache-p package-not-in-core-p)))
          ;;; RED? (when (ilisp-value 'comint-errorp t)
          ;;;  (lisp-display-output package)
          ;;;  (error "No package"))
          
            (when (and package
                       ;; There was a bug here, used to have the second *
                       ;; outside of the parens.
                       ;; CMUCL needs just that WITHIN the double-quotes
                       ;; the old regexp is (string-match "[ \n\t:\"]*\\([^
                       ;; \n\t\"]*\\)" package))
                       (string-match "\\([\"].[^\"]*[\"]\\)" package))
	  
              (setq package
                    (substring package
                               (1+ (match-beginning 1)) (1- (match-end 1)))))
            ;; => without double-quotes

            (values package should-not-cache-p)))))))

;;;
(defun set-package-lisp-always ()
  "Set inferior LISP to a named package.
The package is set whether the buffer has a package or not!"
  (interactive)
  (let* ((default *ILISP-default-package*)
	 (name
	  (read-string
	   (format "Package [%s]: " (lisp-buffer-package)) ""))
	 (package (and (equal name "") default name)))
    
    (if package
	(ilisp-send (format (ilisp-value 'ilisp-in-package-command) package)
		    (format "Set %s's package to %s" 
			    (buffer-name (ilisp-buffer))
			    package)
		    'pkg 'dispatch)
      (error "No package"))))
	      
;;; Martin Atzmueller code ends here.
;;; --------------------------------------------------------------------------

;;;
(defun lisp-buffer-package ()
  "Return the package for this buffer.
The package name is a string. If there is none, return NIL.  This
caches the package unless 'ILISP-DONT-CACHE-PACKAGE' is non-nil, so
calling this more than once is cheap."
  (cond ((and (not (eq buffer-package 'not-yet-computed))
	      (null lisp-dont-cache-package))
	 buffer-package)
	(ilisp-completion-package ilisp-completion-package)
	((or lisp-buffer-package 
	     (memq major-mode ilisp-modes)
	     (not (memq major-mode lisp-source-modes)))
	 nil)
	(t
	 (make-local-variable 'buffer-package)
	 (make-local-variable 'buffer-mode-name)
         (multiple-value-bind (package should-not-cache-p)
             (lisp-buffer-package-internal)
	   (setq buffer-package (if (or should-not-cache-p lisp-dont-cache-package)
                                    'not-yet-computed package))
	   ;; Display package in mode line
	   (when package 
	     (setq mode-name
		   (concat (or buffer-mode-name
			       (setq buffer-mode-name mode-name))
			   ":" package)))
	   package))))


;;;
(defun package-lisp ()
  "Show current inferior LISP package."
  (interactive)
  (message "Inferior LISP package is %s"
	   (ilisp-send (ilisp-value 'ilisp-package-name-command)
		       "Finding inferior LISP package" 'pkg)))

;;; set-package-lisp --
;;; Left the original version, instead of M. Atzmueller's.
;;;
;;; 19990824 Marco Antoniotti
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
  "Convert STRING to a symbol, (package delimiter symbol).
'package' is either package:symbol or from the current buffer."
  (let* ((start (if (ilisp-value 'ilisp-package-separator-regexp t)
		    (string-match (ilisp-value 'ilisp-package-separator-regexp t)
                                  string)))
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
  "Return the immediately preceding symbol encoding.
The result is encoded as ((package delimiter symbol) function-p start end).
If STAY is T, the end of the symbol will be point."
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
  "Return the previous function symbol.
This is either after a #' or at the start of the current sexp.  If there
is no current sexp, return NIL."
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
  "Add FILE to the files to be loaded into the inferior LISP.
The file(s) are 'init' files to be loaded when dialect is initialized.
If FILE is NIL, the entry will be removed."
  (let ((old (assoc dialect ilisp-load-inits)))
    (if file
        (if old
            (rplacd old file)
          (setq ilisp-load-inits (nconc ilisp-load-inits 
                                        (list (cons dialect file)))))
      (when old
        (setq ilisp-load-inits (delq old ilisp-load-inits))))))

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
  "Make sure that initialization is done, and, if not, dispatch another check."
  (if ilisp-load-files
      (comint-send-code (get-buffer-process (current-buffer))
			'ilisp-done-init)
    (when ilisp-initializing
      (unless comint-errorp
	(message "Finished initializing %s" (car ilisp-dialect)))
      (setq ilisp-initializing nil
	    ilisp-initialized
	    (cons (buffer-name (current-buffer)) ilisp-initialized)))))

;;;

(defun comint-send-code-init-function ()
  "Stuff that is executed for initialization in the ilisp process buffer."
  (let ((files ilisp-load-inits)
        (done nil))
    (unwind-protect
      (progn
        (when (not ilisp-init-binary-extension)
          (setq ilisp-init-binary-extension 
                  ilisp-binary-extension))

        (dolist (file files)
          (let ((load-file 
                  (let ((source
                          (expand-file-name (cdr file)
                                            ilisp-*directory*))
                        (binary
                          (expand-file-name
                           (lisp-file-extension (cdr file)
                                                ilisp-binary-extension)
                           ilisp-*directory*)))
                    (if (file-newer-than-file-p binary source)
                      binary
                      source))))
            (ilisp-load-or-send (file-name-hack load-file))))
        (comint-send-code (ilisp-process)
                          'ilisp-done-init)
        (setq done t))                         
      (unless done
        (setq ilisp-initializing nil)
        (abort-commands-lisp)))))

(defun ilisp-init-internal (&optional sync)
  "Send all of the stuff necessary to initialize."
  (unwind-protect
      (progn
	(when sync
	  (comint-sync (ilisp-process)
		       "\"Start sync\""  "[ \t\n]*\"Start sync\""
		       "\"End sync\""    "\"End sync\""))
	(ilisp-binary 'ilisp-binary-command 'ilisp-binary-extension)
	(ilisp-binary 'ilisp-init-binary-command 
		      'ilisp-init-binary-extension)

	;; This gets executed in the process buffer
	(comint-send-code
	 (ilisp-process)
         (function comint-send-code-init-function))
        (set-ilisp-value 'ilisp-initializing t)) ; progn
    
    (unless (ilisp-value 'ilisp-initializing t)
      (abort-commands-lisp))))


;;;
(defun ilisp-init (&optional waitp forcep sync)
  "Initialize the current inferior LISP.
If necessary load the files in 'ilisp-load-inits'.  Optional WAITP
waits for initialization to finish.  When called interactively, force
reinitialization.  With a prefix, get the binary extensions again."
  (interactive 
   (list (if current-prefix-arg
	     (progn
	       (set-ilisp-value 'ilisp-init-binary-extension nil)
	       (set-ilisp-value 'ilisp-binary-extension nil)
	       nil))
	 t))
  (when (or forcep (not (ilisp-initialized)))
    (unless (and noninteractive (= pvs-verbose 0))
      (message "Started initializing ILISP"))
    (unless ilisp-*directory*
      (setq ilisp-*directory* (or (ilisp-directory "ilisp.elc" load-path)
                                  (ilisp-directory "ilisp.el" load-path))))
    (unless (ilisp-value 'ilisp-initializing t)
      (ilisp-init-internal sync))
    (when waitp
      (while (ilisp-value 'ilisp-initializing t)
	(accept-process-output)
	(sit-for 0)))))

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
  "Send STRING to the ILISP buffer.
Also print MESSAGE set STATUS and return the result if AND-GO is NIL,
otherwise switch to ilisp if and-go is T and show message and results.
If AND-GO is 'dispatch, then the command will be executed without
waiting for results.  If AND-GO is 'call, then a call will be
generated. If this is the first time an ilisp command has been
executed, the lisp will also be initialized from the files in
ilisp-load-inits.  If there is an error, comint-errorp will be T and
it will be handled by HANDLER."
  (ilisp-init t)
  (let ((process (ilisp-process))
	(dispatch (eq and-go 'dispatch)))
    (when message
      (message "%s" (if dispatch
			(concat "Started " message)
		      message)))
    ;; No completion table
    (setq ilisp-original nil)
    (cond ((memq and-go '(t call))
	   (comint-send process string nil nil status message handler)
	   (if (eq and-go 'call)
	       (call-defun-lisp nil)
	       (switch-to-lisp t t))
	   nil)
	  (t
	   (let* ((save (ilisp-value 'ilisp-save-command t))
		  (result
		   (comint-send 
		    process
		    (if save (format save string) string)
		    ;; Interrupt without waiting
		    t (unless dispatch 'wait) status message handler)))
	     (when save 
	       (comint-send
		process
		(ilisp-value 'ilisp-restore-command t)
		;; Martin Atzmueller 2000-01-22
		;; this was necessary to have it work in Emacs 20.3 smoothly
                ;; old one: t nil 'restore "Restore" t t
		;; mew experimental:
		;; t (unless dispatch 'wait) 'restore "Restore" t t))
		t 'dispatch 'restore "Restore" t t)) ;;; (unless dispatch 'wait) -> 'dispatch by SO.
	     (unless dispatch
	       (while (not (cdr result))
		 ;; (sit-for 0)  ; 19990912 Pretty useless.
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
       (let* ((file (first (last ilisp-load-files)))
	      (process (get-buffer-process (current-buffer)))
	      (case-fold-search t))
	 (if (and output 
		  (string-match "nil" (car (lisp-last-line output))))
	     (let* ((old-buffer (get-file-buffer file))
		    (buffer (find-file-noselect file))
		    (string (save-excursion
			      (set-buffer buffer)
			      (buffer-string))))
	       (unless old-buffer (kill-buffer buffer))
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
				     (first (last ilisp-load-files)))))
			  (setq ilisp-load-files
				(delq (first (last ilisp-load-files))
				      ilisp-load-files))))))))
	       (when error (ilisp-handler error wait message output last))
	       (setq ilisp-load-files (delq file ilisp-load-files)))))))))

;;; end of file -- ilisp-snd.el --
