;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-def.el --

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
;;; ILISP defvar's
;;;

;;;%Variables
;;;%%Deflocal
(defvar ilisp-locals '(comint-prompt-regexp 
		       input-ring-size
		       comint-get-old-input
		       comint-input-sentinel
		       comint-input-filter
		       comint-input-sender
		       comint-eol-on-send
		       comint-send-newline
		       comint-always-scroll
		       comint-fix-error
		       comint-continue
		       comint-interrupt-regexp
		       comint-error-regexp
		       comint-output-filter
		       comint-interrupt-start
		       comint-handler
		       comint-update-status
		       comint-prompt-status
		       comint-abort-hook)
  "List of ilisp local variables.")
(defun lisp-deflocal (local)
  (if (not (memq local ilisp-locals))
      (setq ilisp-locals (cons local ilisp-locals))))


;;;
(defmacro deflocal (variable default &optional documentation)
  "Define an ilisp local variable."
  (` (progn (lisp-deflocal '(, variable))
	    (defvar (, variable) (, default) (, documentation)))))

;;;%%Simple customization
(defvar ilisp-prefix "\C-z" "Prefix sequence for ilisp commands.")

(deflocal ilisp-program nil
  "*Program and arguments for invoking an inferior LISP.  The program
can be an rsh to run on a remote machine.  If there is not a common
file system, the interface files will be sent down the pipe instead.
The value of this variable is set from DIALECT-program, or inherited
from a less specific dialect if DIALECT-program is nil.")

(defvar ilisp-motd 
  "ILISP V%s  Use M-x ilisp-bug for problems and suggestions."
  "*Message of the day format string for ILISP given VERSION. To
prevent any message from being printed, set this to nil.")

(defvar lisp-wait-p nil
  "*T if LISP eval/compile commands should wait for the result.  A
minus prefix to the command will change the sense of this switch for
just the next command.")

(defvar lisp-no-popper 'message
  "*T if you want all output in the inferior LISP rather than in a
pop-up window.  'message if you want output of one line to go to the
message window (or to the inferior LISP if more).  You should probably
also set comint-always-scroll to T as well so that output is always visible.")

(defvar lisp-show-status t 
  "*Set to nil to stop showing process status in lisp-mode buffers.")

(defvar ilisp-prefix-match nil
  "*Set to T to match only as a prefix when completing through the
inferior LISP.  This will speed up completion, but you no longer get
partial completion.") 

(deflocal ilisp-filter-regexp nil
  "*What not to save on an inferior LISP's input history.
Input matching this regexp is not saved on the input history in ilisp
mode.")

(deflocal ilisp-filter-length 3
  "*Do not save strings less than this in the command history.")

(deflocal ilisp-other-prompt nil
  "*Regexp to recognise prompts in the inferior LISP that are prompts
of non-(read/eval/print) top-levels so that bol-ilisp skips them.")

(deflocal ilisp-raw-echo nil
  "*Set this to T to cause echoing in raw keyboard mode.")

(deflocal ilisp-load-no-compile-query nil
  "*Set this to T to stop load querying about compile.")

;;;%%%Hooks
(defvar ilisp-site-hook nil
  "Hook for site customization of ilisp mode when it is loaded.")

(defvar ilisp-load-hook '()
  "Hook for customizing ilisp mode when it is loaded.")

(defvar ilisp-mode-hook '()
  "Hook for customizing ilisp mode.")

(deflocal ilisp-init-hook nil
  "Hook of functions to call on first prompt in inferior LISP.")

;;;%%Advanced customization
;;;%%%Commands
(deflocal ilisp-reset nil
  "String for resetting the top-level of the inferior LISP.")

(deflocal ilisp-load-or-send-command nil
  "Format string for loading BINARY if possible otherwise loading
FILE.  If you can't load either, return NIL.")

(deflocal ilisp-package-regexp nil
  "Regular expression for finding a package specification in a buffer.
The entire sexp starting with this pattern will be passed to
ilisp-package-command to find the package.")

(deflocal ilisp-package-command nil
  "Format string to find the package given PACKAGE.")

(deflocal ilisp-package-name-command nil
  "Format string to return the name of the current package.")

(deflocal ilisp-in-package-command nil
  "Format string to set the package given PACKAGE.")

(deflocal ilisp-last-command nil
  "Format string for getting the last returned value.")

(deflocal ilisp-save-command nil
  "Format string for saving result history given FORM.")

(deflocal ilisp-restore-command nil
  "Format string for restoring result history.")

(deflocal ilisp-block-command nil
  "Format string for grouping FORMS into one.")

(deflocal ilisp-eval-command nil
  "Format string for evaluating FORM in PACKAGE from FILE.")

(deflocal ilisp-defvar-regexp nil
  "Regular expression for identifying a defvar form.")

(deflocal ilisp-defvar-command nil
  "Format string for re-evaluating DEFVAR in PACKAGE from FILE.")

(deflocal ilisp-describe-command nil
  "Format string for describing FORM in PACKAGE.")

(deflocal ilisp-compile-command nil
  "Format string for compiling FORM in PACKAGE.")

(deflocal ilisp-inspect-command nil
  "Format string for inspecting FORM in PACKAGE.")

(deflocal ilisp-arglist-command nil
  "Format string for arglist of SYMBOL in PACKAGE.")

(deflocal ilisp-documentation-types nil
  "((\"type\") ...) possible LISP documentation types.")

(deflocal ilisp-documentation-command nil
  "Format string for documentation given SYMBOL in PACKAGE and TYPE.")

(deflocal ilisp-macroexpand-1-command nil
  "Format string for top-level macroexpand given FORM and PACKAGE.")

(deflocal ilisp-macroexpand-command  nil
  "Format string for macroexpand given FORM and PACKAGE.")

(deflocal ilisp-complete-command nil
  "Format string for finding possibly matching symbols given SYMBOL,
PACKAGE, FUNCTIONP, EXTERNALP and PARTIAL-MATCHP.  It should print
((string) (string) ...).")

(deflocal ilisp-callers-command nil
  "Format for finding the callers of SYMBOL in PACKAGE.  The function
should print out callers with one per line.")

(deflocal ilisp-trace-command nil
  "Format for tracing SYMBOL in PACKAGE.")
(deflocal ilisp-untrace-command nil
  "Format for untracing SYMBOL in PACKAGE.")

(deflocal ilisp-directory-command nil
  "Format for getting default DIRECTORY.")
(deflocal ilisp-set-directory-command nil
  "Format for setting default DIRECTORY.")

(deflocal ilisp-binary-command nil
  "Command to return the extension for binary files.")

(deflocal ilisp-binary-extension nil
  "*The extension to use for LISP binaries.  If there is an
ilisp-binary-command, this string will be determined at initilization time.")

(deflocal ilisp-init-binary-command nil
  "Command to return the extension for initialization binary files.")

(deflocal ilisp-init-binary-extension nil
  "The extension for initialization binary files.  If there is an
ilisp-init-binary-command, this string will be determined at
initilization time.")

(deflocal ilisp-load-command nil
  "Format string for loading a file in LISP given FILE.")

(deflocal ilisp-compile-file-command nil
  "Format string for compiling a file in LISP given FILE and EXTENSION.")

;;;%%%%Source
(deflocal ilisp-source-types nil
  "Alist of strings for source types.  The strings can be either
symbols or list expressions since the input accepts symbols or open
ended lists as type specifiers.")

(deflocal ilisp-find-source-command nil
  "Format string for finding the source file that defined SYMBOL in
PACKAGE.  It should return NIL if no source is found.")

(deflocal ilisp-locator nil
  "Function \(SYMBOL TYPE FIRST-P BACK-P) that finds the next SYMBOL TYPE
definition in the current buffer.  FIRST-P is T the first time it is
called in a buffer.  BACK-P is T to move backwards.")

(deflocal ilisp-calls-locator nil
  "Function \(SYMBOL TYPE FIRST-P BACK-P ) that finds calls to SYMBOL
in the current buffer.  FIRST-P is T the first time it is called in a
buffer.  BACK-P is T to move backwards.")

(deflocal ilisp-source-directory-fixup-alist
    nil
  "*An alist of (REGEXP . FIXUP-FUNCTION) which will be applied to
lists of source filenames to be used with edit-definitions-list.
FIXUP-FUNCTION takes no arguments and should use replace-match to fix
the filenames.")


;;;%%%Misc
(deflocal ilisp-use-map nil "Keymap to use in ILISP mode.")

(defvar ilisp-bugs-to "ilisp@naggum.no" "Who to send bug reports to.")

(defvar ilisp-modes '(ilisp-mode) "List of all inferior ilisp modes.")
(defvar lisp-source-modes '(lisp-mode scheme-mode)
  "Used to determine if a buffer contains Lisp source code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a lisp source file by find-file-lisp, load-file-lisp and
compile-file-lisp. Used by these commands to determine defaults.")

(deflocal ilisp-no-newline nil
  "Set to T to stop ilisp from inserting a newline after a command.")

(deflocal ilisp-error-filter nil "Function to filter error output.")
(deflocal ilisp-error-regexp nil "Regular expression to match error.")

(deflocal ilisp-symbol-delimiters nil
  "Delimiters found around symbols.")

;;;%%Program
(defvar ilisp-completion-map nil "Keymap for reading ilisp readers.")
(defvar ilisp-epoch-running (and (boundp 'epoch::version) epoch::version)
  "Non-nil if epoch is running.")
(defvar ilisp-version 
  "5.8" ;; ILISP-VERSION marker
  "Interface version.")
(defvar ilisp-directory nil "The directory that ilisp is found in.")
(defvar ilisp-mode-map nil "Key map for ILISP.")
(defvar ilisp-raw-map  nil
  "Keyboard map for sending characters directly to the inferior LISP.")
(defvar ilisp-raw-message "Raw keyboard mode until C-g"
  "Message for how to stop raw mode.")
(defvar ilisp-buffer nil "Name of selected ilisp buffer.")
(defvar ilisp-status nil "Status string for selected ilisp buffer.")
(defvar ilisp-buffers nil "List of ILISP buffers.")
(defvar ilisp-dialects nil "List of ILISP dialects.")

(deflocal ilisp-load-inits nil
  "Alist of dialect files to load when initializing an inferior LISP.
By default the file will be loaded from the ilisp-directory.")

;;; This is useful to have a clause in ilisp code like:
;;; (if (memq 'allegro (ilisp-value 'ilisp-dialect)) 
;;;     allegro-code
;;;     normal-code)
(deflocal ilisp-dialect nil
  "List of the dialects that defined the current inferior LISP.")

(defvar ilisp-initialized nil
  "List of buffer names that have been initialized.")
(deflocal ilisp-initializing nil
  "Set to T while waiting for inferior LISP to get initialized.")

(deflocal ilisp-load-files nil "List of files being loaded.")

(defvar lisp-changes nil
  "List of markers for changed forms.")
(deflocal ilisp-pending-changes nil
  "List of changes that are pending, but have not been confirmed yet.")

;;;%%%Completion
;;; Dynamically bound variables for controlling reading
(defvar ilisp-complete nil "T if in minibuffer completion mode.")
(defvar ilisp-no-complete nil "T if incomplete symbols are allowed.")
(defvar ilisp-table nil "Completion table for ilisp readers.")
(defvar ilisp-paren nil "T if paren is allowed in ilisp readers.")
(defvar ilisp-completion-package nil 
  "Package of buffer requesting completion.")
(defvar ilisp-completion-function-p nil
  "T if only symbols with function values are allowed.")

;;; State variables for ilisp reading
(defvar ilisp-mini-prefix nil "Package and qualification from minibuffer.")
(defvar ilisp-original nil "Original string for ilisp completion.")
(defvar ilisp-original-function-p nil "Function-p for ilisp completion.")
(defvar ilisp-original-table nil "Completion table for ilisp-original.")

;;;%Buffer
;;;%Packages
(defvar buffer-package 'not-yet-computed "Cached package name.")
(defvar buffer-mode-name nil "Original mode name.")
(defvar lisp-buffer-package nil "T if in lisp-buffer-package.")
(defvar lisp-dont-cache-package nil 
  "If Non-Nil then refind the most recent in-package each time.")

;;;%Globals from ilisp-ext.el
;;;
(defvar ilisp-ext-load-hook nil "Hook to run when extensions are loaded.")
(defvar left-delimiter "\(" "*Left delimiter for find-unbalanced.")
(defvar right-delimiter "\)" "*Right delimiter for find-unbalanced.")

;;; Copies of ilisp var definitions
(defvar ilisp-complete nil "T when ilisp is in completion mode.")
(defvar ilisp-modes '(ilisp-mode) "List of all inferior ilisp modes.")
(defvar lisp-fill-marker (make-marker)
  "Keeps track of point so that it does not move during a reindent-lisp.")
(defvar ilisp-comment-marker (make-marker)
  "Marker for end of a comment region.")


(defvar lisp-buffer-file nil 
  "Cons of buffer-file-name and the expanded name.")
(make-variable-buffer-local 'lisp-buffer-file)

(defvar ilisp-last-message nil)
(defvar ilisp-last-prompt nil)

(defvar lisp-prev-l/c-dir/file nil
  "Saves the (directory . file) pair used in the last find-file-lisp,
load-file-lisp or compile-file-lisp command. Used for determining the
default in the next one.")

(defvar ilisp-last-buffer nil
  "The last used LISP buffer.")
