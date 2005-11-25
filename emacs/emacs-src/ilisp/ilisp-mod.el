;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mod.el --
;;; ILISP mode top level definitions.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;;;%ilisp-mode

(defun ilisp-byte-code-to-list (function)
  "Returns a list suitable for passing to make-byte-code from FUNCTION."
  (let ((function-object 
	 (if (symbolp function)
	     (symbol-function function)
	   function)))
    (if (fboundp 'compiled-function-arglist)
	;; XEmacs
	(read (concat "("
		      (substring (let ((print-readably t))
				   (prin1-to-string function-object))
				 2 -1)
		      ")"))
      ;; FSFmacs
      (append function-object nil))))

;;;
(defun ilisp-set-doc (function string)
  "Set the documentation of the symbol FUNCTION to STRING."
  (let* ((old-function (symbol-function function)))
    (cond ((listp old-function)
	   ;; Probe to test whether function is in preloaded read-only
	   ;; memory, and if so make writable copy:
	   (condition-case nil
	       (setcar old-function (car old-function))
	     (error
	      (setq old-function (copy-sequence old-function)) ; shallow copy only
	      (fset function old-function)))
	   (let ((ndoc-cdr (nthcdr 2 old-function)))
	     (if (stringp (car ndoc-cdr))
		 ;; Replace the existing docstring.
		 (setcar ndoc-cdr string)
	       ;; There is no docstring.  Insert the overwrite msg.
	       (setcdr ndoc-cdr (cons (car ndoc-cdr) (cdr ndoc-cdr)))
	       (setcar ndoc-cdr string))))
	  (t
	   ;; it's an emacs19 compiled-code object
	   (let ((new-code (ilisp-byte-code-to-list old-function)))
	     (if (nthcdr 4 new-code)
		 (setcar (nthcdr 4 new-code) string)
	       (setcdr (nthcdr 3 new-code) (cons string nil)))
	     (fset function (apply 'make-byte-code new-code)))))))
    


;;;
(defun ilisp-mode ()
  (interactive)
  (run-ilisp))

(ilisp-set-doc 'ilisp-mode ilisp-documentation)
(ilisp-set-doc 'lisp-mode ilisp-documentation)

;;;%%ILISP
(defun lisp-command-args (command-line)
  "Break up COMMAND-LINE into (command args ...)"
  (condition-case nil
      (loop with start = 0
            while start
            for pos = (string-match "\\S-" command-line start)
            while pos
            if (char-equal (aref command-line pos) ?\")
            collect (let ((str+end-pos (read-from-string command-line pos)))
                      (setq start (cdr str+end-pos))
                      (car str+end-pos))
            else collect (let ((end-pos (string-match "\\s-" command-line pos)))
                           (setq start end-pos)
                           (substring command-line pos end-pos)))
    (error (error "Invalid inferior Lisp program command line"))))



;;;
(defun ilisp (name setup)
  "Run an inferior LISP process NAME, input and output via buffer *name*.
If there is a process already running in *name*, just switch to that buffer.
Takes the program name from the variable ilisp-program.
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (set-buffer ilisp-buffer)
  (if (not (comint-check-proc ilisp-buffer))
      (let* ((dialect (car ilisp-dialect))
	     (program ilisp-program)
	     (args (lisp-command-args program))
	     ;; Use pipes so that strings can be long
	     (process-connection-type nil)
	     (names (format "%s" name))
	     start)
	(apply 'make-comint name (car args) nil (cdr args))
	(comint-setup-ipc)
	;; Because comint-mode kills all buffer-local variables in
	;; fsf-19 we have to re-call the setup here.
	(funcall setup name)
	(setq major-mode 'ilisp-mode
	      mode-name "ILISP")
	(use-local-map ilisp-mode-map) ;;; SO.
	(rplaca (car comint-send-queue) 
		(function (lambda ()
			    (run-hooks 'ilisp-init-hook-local))))
	(setq ilisp-initialized (delete* ilisp-buffer ilisp-initialized
					 :test #'equal))
	(unless (member* names ilisp-buffers :key #'car)
	  (setq ilisp-buffers (cons (list names) ilisp-buffers)))
	;;;;;;;;;;;;(lisp-pop-to-buffer ilisp-buffer)  ;;;; evil
	(setq start (window-start (selected-window))
	      ilisp-program program)
	(goto-char (point-max))
	(insert (format "Starting %s ...\n" ilisp-program))
	(set-marker (process-mark (ilisp-process)) (point))
	(funcall comint-update-status 'start)
	
	(when ilisp-motd
	  (lisp-display-output (format ilisp-motd ilisp-*version*))
	  (sleep-for 3)
	  (set-window-start (selected-window) start))

	(unless ilisp-*prefix-match* (require 'completer)))

      ;;;;;;;;;;;;;;(lisp-pop-to-buffer ilisp-buffer) ;;;;;;;;;evil
      )
  (use-local-map ilisp-use-map)
  ;; This is necessary to get mode documentation to come out right
  (set-default 'ilisp-use-map ilisp-use-map))


;;;%Manual
(autoload 'fi:clman         "fi/clman" 
	  "Look up SYMBOL in the online manual with completion." t)
(autoload 'fi:clman-apropos "fi/clman" 
	  "Do an apropos search in online manual for STRING." t)

;;;%Bridges
(autoload 'install-bridge "bridge" "Install process bridge." t)

;;;%Modes
(set-default 'auto-mode-alist
	     (append '(("\\.cl$" . lisp-mode) ("\\.lisp$" . lisp-mode))
		     auto-mode-alist))
(setq completion-ignored-extensions 
      (append '(".68fasl" ".sfasl" ".ifasl" ".pfasl" 
		".68fasl4" ".sfasl4" ".ifasl4" ".pfasl4" 
		".sbin")
	      completion-ignored-extensions))
