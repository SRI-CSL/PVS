;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-dia.el --

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


;;;%%CUSTOMIZING DIALECTS
;;;
;;; ILISP is already set up with support for a number of dialects.
;;; Each dialect has a command NAME that will start an inferior LISP
;;; of that dialect.  NAME-hook is a hook that will run after the
;;; default settings for NAME are set up.  NAME-program is the default
;;; program for NAME. A prefix when starting a dialect will cause you
;;; to be prompted for the buffer name and the program.  When setting
;;; something in a hook, you should use the most general dialect that
;;; makes sense. Dialect definitions and their hooks are executed from
;;; least specific to most specific.  They will be executed before the
;;; inferior LISP is started.
;;;
;;; These are the currently supported dialects.  The dialects
;;; are listed so that the indentation correponds to the hierarchical
;;; relationship between dialects.
;;; clisp
;;;   allegro
;;;   Clisp     (Haible and Stoll)
;;;   lispworks (Harlequin)
;;;   lucid
;;;   cmulisp
;;;   kcl
;;;     akcl
;;;     ibcl
;;;     ecl
;;;     gcl
;;; scheme
;;;   oaklisp
;;;   Scheme->C (still "in fieri")
;;;
;;; If anyone figures out support for other dialects I would be happy
;;; to include it in future releases.
;;;
;;; ;;; Example of local changes and extensions to ilisp mode
;;; (setq ilisp-load-hook
;;;       '(lambda ()
;;;         ;; Change the allegro lisp program
;;;         (setq allegro-program "/usr/misc/bin/lisp")
;;;         ;; Add a new key binding
;;;         (defkey-ilisp "\C-\M-a" 'arglist-lisp)
;;;         ;; Define a new subdialect to run on another machine.
;;;         (defdialect cmlisp "Connection Machine LISP."
;;;           lucid
;;;           (setq ilisp-program
;;;            "rsh power /usr/local/cm/bin/starlisp"))))
;;;
;;; ;;; Automatically load a new subdialect
;;; (autoload 'cmlisp "ilisp" "Run an inferior CM lisp." t)
;;;
;;; To define a new dialect use the macro defdialect.  For examples,
;;; look at the dialect definitions in this file.  There are hooks and
;;; variables for almost anything that you are likely to need to
;;; change.  The relationship between dialects is hierarchical with
;;; the root values being defined in setup-ilisp.  For a new dialect,
;;; you only need to change the variables that are different than in
;;; the parent dialect.


;;;
;;; ILISP dialect definition code.
;;;

;;;%Dialects
(defun lisp-add-dialect (dialect)
  "Add DIALECT as a supported ILISP dialect."
  (if (not (lisp-memk dialect ilisp-dialects 'car))
      (setq ilisp-dialects
	    (cons (list dialect) ilisp-dialects))))

;;;
(defun ilisp-start-dialect (buffer program setup)
  ;; Allow dialects to be started from command line
  (if (eq current-prefix-arg 0) (setq current-prefix-arg nil))
  (setq ilisp-last-buffer (current-buffer)
	buffer (if current-prefix-arg
		   (read-from-minibuffer "Buffer: " buffer)
		   buffer))
  (funcall setup buffer)
  (setq ilisp-program
	(or program 
	    (if current-prefix-arg
		(lisp-read-program "Program: " ilisp-program)
		ilisp-program)))
  (ilisp buffer setup))

;;;
(defmacro defdialect (dialect full-name parent &rest body)
  "Define a new ILISP dialect.  DIALECT is the name of the function to
invoke the inferior LISP. The hook for that LISP will be called
DIALECT-hook.  The default program will be DIALECT-program.  FULL-NAME
is a string that describes the inferior LISP.  PARENT is the name of
the parent dialect."
  (let ((setup (read (format "setup-%s" dialect)))
	(hook (read (format "%s-hook" dialect)))
	(program (read (format "%s-program" dialect)))
	(dialects (format "%s" dialect)))
    (`
     (progn
       (defvar (, hook) nil (, (format "*Inferior %s hook." full-name)))
       (defvar (, program) nil
	 (, (format "*Inferior %s default program." full-name)))
       (defun (, setup) (buffer)
	 (, (format "Set up for interacting with %s." full-name))
	 (, (read (format "(setup-%s buffer)" parent)))
	 (,@ body)
	 (setq ilisp-program (or (, program) ilisp-program)
	       ilisp-dialect (cons '(, dialect) ilisp-dialect))
	 (run-hooks '(, (read (format "%s-hook" dialect)))))
       (defun (, dialect) (&optional buffer program)
	 (, (format "Create an inferior %s.  With prefix, prompt for buffer and program."
		   full-name))
	 (interactive (list nil nil))
	 (ilisp-start-dialect (or buffer (, dialects)) 
			      program 
			      '(, setup))
	 (setq (, program) ilisp-program))
       (lisp-add-dialect (, dialects))))))

;;;%%ilisp
(defun setup-ilisp (buffer)
  "Set up for interacting with an inferior LISP."
  (set-buffer (get-buffer-create "*ilisp-send*"))
  (kill-all-local-variables)
  (lisp-mode)
  (setq ilisp-buffer (format "*%s*" buffer))
  (set-buffer (get-buffer-create ilisp-buffer))
  (setq major-mode 'ilisp-mode
	mode-name "ILISP")
  (lisp-mode-variables t)
  ;; Set variables to nil
  (let ((binary ilisp-binary-extension)
	(init ilisp-init-binary-extension)
	(vars ilisp-locals))
    (while (not (null vars))
      (make-local-variable (car vars))
      (set (car vars) nil)
      (setq vars (cdr vars)))
    ;; Preserve from initialization
    (if binary (setq ilisp-binary-extension binary))
    (if init (setq ilisp-init-binary-extension init)))
  ;; Comint defaults
  (set-ilisp-input-ring-size 200)
  (setq comint-prompt-regexp "^[^<> ]*>+:? *"

	comint-get-old-input 'ilisp-get-old-input
	comint-input-sentinel (function ignore)
	comint-input-filter 'ilisp-input-filter
	comint-input-sender 'comint-default-send
	comint-eol-on-send t)
  ;; Comint-ipc defaults
  (setq comint-send-newline t
	comint-always-scroll nil
	comint-output-buffer " *Output*"
	comint-error-buffer " *Error Output*"
	comint-error-regexp "^\"ILISP:"
	comint-output-filter (function identity)
	comint-interrupt-start 'comint-interrupt-start
	comint-handler 'ilisp-handler
	comint-update-status 'ilisp-update-status
	comint-prompt-status 'comint-prompt-status
	comint-abort-hook 'ilisp-abort-handler)
  (setq ilisp-use-map ilisp-mode-map
	ilisp-init-hook '((lambda () (ilisp-init nil nil t)))
	ilisp-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)*\\)?\\s *\\'"
	ilisp-filter-length 3
	ilisp-error-filter 'ilisp-error-filter
	ilisp-error-regexp ".*" 
	ilisp-symbol-delimiters "^ \t\n\('\"#.\)<>"
	ilisp-program "lisp"
	ilisp-locator 'lisp-locate-ilisp
	ilisp-calls-locator 'lisp-locate-calls)
  (run-hooks 'ilisp-mode-hook))

(defun run-ilisp ()
  "Create an inferior LISP prompting for dialect.  With prefix, prompt
for buffer name as well."
  (interactive)
  (let ((dialect (completing-read "Dialect: " ilisp-dialects nil t)))
    (if (not (zerop (length dialect)))
	(call-interactively (read dialect)))))

