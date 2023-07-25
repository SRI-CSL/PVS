;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-ilisp.el -- defines the interface between PVS and ILISP
;; Author          : Sam Owre
;; Created On      : Fri Dec 17 13:08:13 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 22:42:31 2004
;; Update Count    : 41
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(eval-when-compile (require 'comint))
(require 'cl-lib)
(require 'ilisp)
(require 'completion)
(eval-when-compile (require 'pvs-macros))

;;; FIXME - this may be related to changes in easymenu.el ???
(when (featurep 'xemacs)
  (add-hook 'ilisp-mode-hook
    '(lambda ()
       (add-submenu nil pvs-mode-menus ""))))

; better history mode for the ilisp buffer
;
(defkey-ilisp "\C-\\s" 'comint-previous-matching-input-from-input)
(defkey-ilisp "\M-s" 'comint-previous-matching-input-from-input)

(add-hook 'comint-mode-hook
    '(lambda ()
       (defun comint-previous-input (arg)
	 "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match.
Modified from original to not delete earlier command if some other command
intervenes."
	 (interactive "*p")
	 (setq arg (comint-search-arg arg))
	 (unless (memq last-command
		       '(comint-previous-input
			 comint-next-input))
	   (set-marker comint-last-input-start (point)))
	 (let ((pos (comint-previous-matching-input-string-position "." arg)))
	   ;; Has a match been found?
	   (if (null pos)
	       (error "Not found")
	       (setq comint-input-ring-index pos)
	       (message "History item: %d" (1+ pos))
	       (delete-region 
		;; Can't use kill-region as it sets this-command
		comint-last-input-start (point))
	       (insert (ring-ref comint-input-ring pos)))))))

;; (add-hook 'comint-input-filter-functions
;;   'update-pvs-history-file)
;; (defun update-pvs-history-file (input)
;; )

;;; The following variables must be set correctly for the ilisp interface
;;; to work correctly:
;;;   comint-prompt-regexp
;;;   comint-fix-error - string used to pop up one level from a break
;;;   ilisp-reset - string used to abort to the top level
;;;   comint-continue - string used to continue after an interrupt
;;;   comint-interrupt-regexp - a regexp that recognizes an interrupt
;;;   ilisp-error-regexp - used to filter error output; if an error message
;;;          matches, then only that much will be output in the popup buffer
;;;          if no match then it all will be displayed.
;;;   comint-prompt-status - should evaluate to a function that given
;;;          an old line and a new line and the name of a function, will
;;;          return 'error if new line is an error, T if it is a prompt
;;;          as determined by comint-prompt-regexp or nil otherwise.
;;;          This is usually set to
;;;            (lambda (old new)
;;;               (comint-prompt-status old line 'foo-check-prompt))
;;;          Where foo is the dialect, and foo-check-prompt is a function
;;;          that determines from old and new whether the new is an error.

(defvar pvs-error)
(defvar in-pvs-emacs-eval nil)
(defvar *default-char-width* 80)
(defvar pvs-message-delay 300
  "Time for which a PVS message is displayed, in milliseconds")

(defvar pvs-gc-end-regexp nil)

;;;(defvar pvs-lucid-gc-end-regexp
;;;  "[0-9]* words \\[[0-9]* bytes\\] of dynamic storage in use.
;;; [0-9]* words \\[[0-9]* bytes\\] of free storage available before a GC.
;;; [0-9]* words \\[[0-9]* bytes\\] of free storage available if GC is disabled.")

;;; defdialect is defined in ilisp.el

(defvar pvs-initialized nil)
(defvar pvs-lisp nil)

(defun pvs-init ()
  (setq ilisp-prefix-match t)
  (setq pvs-lisp (intern (getenv "PVSLISP")))
  (cl-case pvs-lisp
    (allegro (pvsallegro "pvs" nil))
    (cmulisp (pvscmulisp "pvs" nil))
    (sbclisp (pvssbclisp "pvs" nil))
    (t (error "Unknown lisp - %s" (getenv "PVSLISP"))))
  (with-current-buffer (ilisp-buffer)
    (setq ilisp-package-command "(pvs::lisp (let ((*package* *package*)) %s (package-name *package*)))"
	  ilisp-package-name-command "(pvs::lisp (package-name *package*))"
	  ilisp-in-package-command "(pvs::lisp (in-package \"%s\"))"
	  ilisp-save-command "(pvs::lisp (progn (pvs::pvs-ilisp-save) %s\n))"
	  ilisp-restore-command "(pvs::lisp (pvs::pvs-ilisp-restore))"
	  ilisp-block-command "(pvs::lisp (progn %s\n))"
	  ;;ilisp-eval-command "(pvs::lisp (ILISP:ilisp-eval \"%s\" \"%s\" \"%s\"))"
	  ilisp-display-output-function 'ilisp-display-output-adaptively))
  (set-process-filter (ilisp-process) 'pvs-process-filter) 
  (define-key ilisp-mode-map "\C-c\C-c" 'pvs-interrupt-subjob)
  (define-key ilisp-mode-map "]" 'self-insert-command)
  (define-pvs-key-bindings (ilisp-buffer))
  (setq *default-char-width* (window-width))
  (add-hook 'kill-emacs-hook
    '(lambda () (when (and (funcall ilisp-buffer-function) (ilisp-process))
		  (kill-process (ilisp-process)))))
  ;; Set in pvs-init from the lisp image
  ;;(setq pvs-initialized t)
  )

(defun pvs-program ()
  pvs-image)

(defvar pvs-fix-error nil)
(defvar pvs-top-regexp nil)

(defdialect pvsallegro "pvs"
  allegro
  (pvs-comint-init)
  ;;(setq comint-send-newline nil)
  (setq ilisp-binary-extension (pvs-allegro-binary-extension))
  (setq ilisp-init-binary-extension ilisp-binary-extension)
  (setq ilisp-load-inits nil)
  (setq ilisp-program (format "%s -qq" (pvs-program)))
  (let* ((prompt-pre "\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\|\\[ldb.*\\] \\)?")
	 (old-prompt "\\(<?[-A-Za-z]* ?[0-9]*>\\)") ;  'yices >', '<PVSio>', etc
	 (new-prompt "\\([-A-Za-z0-9]+([0-9]+):\\)")
	 (pvs-added "Rule\\?\\|.*(Y or N)\\|.*(Yes or No)\\|.*process\\?\\|Please enter.*:"))
    (setq comint-prompt-regexp
	  (format "^\\(%s\\(%s\\|%s\\|%s\\)\\)+:? "
	      prompt-pre old-prompt new-prompt pvs-added))
    (setq pvs-top-regexp
	  (format "^\\(%s\\(%s\\|%s\\) \\)+" prompt-pre old-prompt new-prompt)))
  (setq comint-interrupt-regexp  "Error: [^\n]* interrupt\)")
  (setq ilisp-error-regexp
	"^[ \t]\\(Error:[^\n]*\\)\\|\\(Break:[^\n]*\\)")
  (setq pvs-gc-end-regexp ";;; Finished GC")
  ;; (setq font-lock-defaults
  ;; 	`((pvs-lisp-font-lock-keywords
  ;; 	   pvs-lisp-font-lock-keywords-1 pvs-lisp-font-lock-keywords-2)
  ;; 	  nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
  ;; 	  (font-lock-mark-block-function . mark-defun)
  ;; 	  (font-lock-syntactic-face-function
  ;; 	   . lisp-font-lock-syntactic-face-function)))
  )

(defdialect pvscmulisp "pvs-cmulisp"
  cmulisp
  (pvs-comint-init)
  ;;(setq comint-send-newline nil)
  (setq ilisp-binary-extension (pvs-cmulisp-binary-extension))
  (setq ilisp-init-binary-extension ilisp-binary-extension)
  (setq ilisp-load-inits nil)
  (setq ilisp-program (format "%s -quiet -noinit" (pvs-program)))
  (setq comint-prompt-regexp
	"^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) \\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|(Y or N)\\|(Yes or No)\\|Please enter")
  (setq comint-interrupt-regexp  "^Interrupted at")
  (setq ilisp-error-regexp "^[ \t]Restarts:$")
  (setq pvs-top-regexp
	"^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) ")
  (setq pvs-gc-end-regexp ";;; Finished GC"))

(defdialect pvssbclisp "pvs-sbclisp"
  cmulisp
  (pvs-comint-init)
  ;;(setq comint-send-newline nil)
  (setq ilisp-binary-extension (pvs-sbclisp-binary-extension))
  (setq ilisp-init-binary-extension ilisp-binary-extension)
  (setq ilisp-load-inits nil)
  (setq ilisp-program (format "%s --no-userinit" (pvs-program)))
  (setq ilisp-reset ":abort")
  (let* ((prompt-pre "\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\|\\[ldb.*\\] \\)?")
	 (old-prompt "\\(<?[-A-Za-z]* ?[0-9]*>\\)") ;  'yices >', '<PVSio>', etc
	 (new-prompt "\\([-A-Za-z0-9]+([0-9]+):\\)")
	 (sbcl-prompt "\\([0-9]+\\]+\\|[0-9]+\\[[0-9]+\\]\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\)")
	 (pvs-added "Rule\\?\\|.*(Y or N)\\|.*(Yes or No)\\|.*process\\?\\|Please enter.*:"))
    (setq comint-prompt-regexp
	  (format "^\\(%s\\(%s\\|%s\\|%s\\|%s\\)\\)+:? "
	      prompt-pre old-prompt new-prompt sbcl-prompt pvs-added))
    (setq pvs-top-regexp
	  (format "^\\(%s\\(%s\\|%s\\) \\)+" prompt-pre old-prompt new-prompt)))
  ;; (setq comint-prompt-regexp
  ;; 	"^\\([0-9]+\\]+\\|[0-9]+\\[[0-9]+\\]\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) \\|Rule\\? \\|<GndEval> \\|<PVSio> \\|yices > \\|(Y or N)\\|(Yes or No)\\|Please enter\\|Defining.*\\* ")
  ;; (setq pvs-top-regexp
  ;; 	"^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) ")
  (setq comint-interrupt-regexp  "^  Interactive interrupt at")
  (setq comint-continue ":continue")
  (setq ilisp-error-regexp "^0\\(\\[[0-9]+\\)?\\] $")
  (setq pvs-gc-end-regexp ";;; Finished GC"))

(defun pvs-allegro-binary-extension ()
  "fasl")

(defun pvs-cmulisp-binary-extension ()
  "fasl")

(defun pvs-sbclisp-binary-extension ()
  "fasl")

(defun pvs-comint-init ()
  (setq ilisp-motd nil)
  (setq pvs-fix-error comint-fix-error)
  (setq comint-handler 'pvs-handler)
  (setq pvs-partial-line ""))

(deflocal pvs-partial-line ""
  "The part of the pvs line which has been received so far.")

(defvar *pvs-output-pos* nil
  "The marker for the place point should move to after this operation.")

(defvar pvs-recursive-process-filter nil
  "True if currently inside a call to pvs-process-filter.")

;;; PVS send commands

(defvar pvs-aborted nil)


(defun pvs-file-send-and-wait (string &optional message status expected)
  (let* ((fstring (format "(pvs:write-to-temp-file %s t)" string))
	 (file (pvs-send-and-wait fstring message status 'tmp-file))
	 (buf (find-file-noselect file))
	 (raw-value (read buf))
	 (value (unless (member raw-value '(nil NIL)) raw-value)))
    (unless (cl-typep value expected)
      (pop-to-buffer buf)
      (error "Expect %s return values in file %s" expected file))
    (delete-file file)
    (kill-buffer buf)
    value))

(defun pvs-send-and-wait-for-json (string &optional message status)
  (let* ((fstring (format "(pvs:write-json-to-temp-file %s)" string))
	 (file (pvs-send-and-wait fstring message status 'tmp-file)))
    (with-temp-buffer
      (insert-file-contents file)
      ;;(setq jjj (buffer-string))
      (goto-char (point-min))
      (json-read))))

(defun pvs-send-and-wait (string &optional message status expected)
  (let* ((msg (when message (format "%s...please wait" message)))
	 (pvs-aborted nil)
	 (output (pvs-send* string msg status)))
    (if pvs-aborted
	(setq quit-flag t)
	(unless (and (boundp 'pvs-error) pvs-error)
	  (pvs-extract-expected-from-output output expected)))))

(defun pvs-extract-expected-from-output (output expected)
  (unless (or in-pvs-emacs-eval
	      (eq expected 'dont-care))
    (save-match-data
      (let* ((str output)
	     (val (pvs-read-from-string str)))
	(unless (member val '(nil NIL))
	  val)))))

(defun pvs-read-from-string (string)
  (condition-case ()
      (car (read-from-string string))
    (end-of-file (pvs-log-message
		  "Emacs Error"
		  (format "End of file during parsing of\n     %s" string))
		 (error "Error in reading PVS output - try again"))))

(defun pvs-send (string &optional message status)
  (pvs-send* string message status t)
  (sit-for 1))

(defun pvs-send* (string &optional message status and-go)
  (when pvs-in-evaluator
    (ilisp-switch-to-lisp t)
    (error "Must exit the ground evaluator first"))
  (with-current-buffer (ilisp-buffer)
    (setq buffer-read-only nil))
  (let (*pvs-output-pos*)
    (comint-log (ilisp-process) (format "\nsent:{%s}\n" string))
    (prog1
	(let ((cursor-in-echo-area t))
	  (if in-pvs-emacs-eval
	      (comint-simple-send (ilisp-process) string)
	      (ilisp-send (format "(pvs::lisp (pvs::pvs-errors %s))" string)
			  message status (when and-go
					   ;;(and (null noninteractive)
					   ;;     and-go)
					   'dispatch))))
      ;; Disabling 'dispatch above really slows down batch mode for
      ;; some reason, so we use this instead.  It ensures that batch
      ;; commands are allowed to complete before exiting.
      (when noninteractive
	(pvs-wait-for-it))
      (when *pvs-output-pos*
	(switch-to-buffer (marker-buffer *pvs-output-pos*))
	(goto-char (marker-position *pvs-output-pos*))))))

(defvar *pvs-maximize-proof-display* t  
  "Controls whether to keep the prover prompts at the bottom.  You may
want to set this to nil for slow terminals, or connections over a modem.")

;;; PDL nov 29, 1994 
(defvar *pvs-is-garbage* nil 
  "A flag set when pvs notices that lisp is garbage collecting")


;;; If anything called by pvs-process-filter does
;;; sit-for, sleep-for, or accept-process-output, then pvs-process-filter
;;; can get called recursively.  If it does, then it can't actually
;;; process the output that it gets; that would allow things to happen
;;; out of order.  Instead, it appends the output to the string
;;; which is currently being processed by the top-level pvs-process-filter.

(defvar pvs-process-output)

(defun pvs-process-filter (process output)
  (when comint-log-verbose
    (comint-log (ilisp-process) (format "\nrec:{%s}\n" output)))
  (setq output (pvs-process-gc-messages output))
  (if pvs-recursive-process-filter
      (setq pvs-process-output (concat pvs-process-output output))
      (let ((pvs-recursive-process-filter t))
	(setq output (concat (ilisp-value 'pvs-partial-line) output))
	(let ((pvs-process-output output)
	      line-end)
	  (while (string-match "\n" pvs-process-output)
	    (setq line-end (match-end 0))
	    ;; Note that pvs-process-output can get longer
	    ;; during this next call.
	    (let ((pvs-output (pvs-output-filter
			       (substring pvs-process-output 0 line-end))))
	      (when (and noninteractive
			 pvs-in-checker
			 (not (string-match "^\\(nil\\|NIL\\)$" pvs-output)))
		(when pvs-validating
		  (princ pvs-output))
		(when (> pvs-verbose 2)
		  (princ pvs-output 'external-debugging-output)))
	      (comint-process-filter process pvs-output))
	    (setq pvs-process-output
		  (substring pvs-process-output line-end)))
	  (if (string-match (ilisp-value 'comint-prompt-regexp)
			    pvs-process-output)
	      (progn
		(when (and noninteractive
			   (string-match "(Y or N):\\|(Yes or No)"
					 pvs-process-output))
		  (pvs-message "Answering yes to\n  %s" pvs-process-output)
		  (process-send-string
		   (ilisp-process)
		   (if (string-match "(Y or N):" pvs-process-output)
		       "y\n" "yes\n")))
		(set-ilisp-value 'comint-fix-error
				 (if pvs-in-checker "(restore)" pvs-fix-error))
		(when (and (string-match "(Y or N):\\|(Yes or No)"
					 pvs-process-output)
			   (not (eq (current-buffer) (ilisp-buffer))))
		  (ilisp-switch-to-lisp t)
		  (message "Please answer Yes or No"))
		(comint-process-filter process pvs-process-output)
		(set-ilisp-value 'pvs-partial-line "")
		(when (and *pvs-maximize-proof-display*
			   (eq (current-buffer) (ilisp-buffer))
			   (string-match "Rule\\? " pvs-process-output))
		  (save-selected-window
		    (switch-to-buffer "*pvs*")
		    (recenter -1))))
	      (set-ilisp-value 'pvs-partial-line
			       pvs-process-output))))))

(defun pvs-output-filter (output)
  (if (string-match
       ":pvs-\\(msg\\|log\\|warn\\|out\\|err\\|qry\\|buf\\|sqt\\|yn\\|bel\\|loc\\|mod\\|pmt\\|dis\\|wish\\|eval\\|addecl\\|\\prfst\\|\\prfthd\\) "
       output)
      (let* ((orig-string-end (match-beginning 0))
	     (beg (match-end 0))
	     (kind (substring output (match-beginning 1) (match-end 1)))
	     (end (string-match (format " :end-pvs-%s" kind) output)))
	(if end
	    (let ((out (unless (string-equal kind "bel")
			 (substring output beg end))))
	      ;;(message (format "Pvs out: %s" out))
	      (cond ((string-equal kind "msg")
		     (cond (noninteractive
			    (when pvs-validating
			      (princ-nl (remove-esc out)))
			    (when (> pvs-verbose 0)
			      (princ-nl (remove-esc out)
					'external-debugging-output)))
			   (t (message (remove-esc out))
			      (if (featurep 'xemacs)
				  (sit-for (/ pvs-message-delay 1000.0))
				  (sit-for 0 pvs-message-delay))
			      (pvs-log-message 'MSG (remove-esc out)))))
		    ((string-equal kind "log")
		     (pvs-log-log (remove-esc out)))
		    ((string-equal kind "warn")
		     (pvs-warning out))
		    ((string-equal kind "err")
		     (pvs-error out)
		     ;(pushw)
		     )
		    ((string-equal kind "out")
		     (pvs-output out))
		    ((string-equal kind "qry")
		     (pvs-query out))
		    ((string-equal kind "buf")
		     (pvs-buffer out))
		    ((string-equal kind "sqt")
		     (pvs-sequent out))
		    ((string-equal kind "yn")
		     (apply 'pvs-y-or-n (parse-pvs-message out)))
		    ((string-equal kind "bel")
		     (beep t)
		     (sit-for 1))
		    ((string-equal kind "loc")
		     (pvs-locate out))
		    ((string-equal kind "mod")
		     (pvs-modify-buffer out))
		    ((string-equal kind "pmt")
		     (pvs-prompt (parse-pvs-message out)))
		    ((string-equal kind "dis")
		     (pvs-display (parse-pvs-message out)))
		    ((string-equal kind "wish")
		     (ensure-pvs-wish)
		     (tcl-send-string* (car (parse-pvs-message out))))
		    ((string-equal kind "eval")
		     (apply 'pvs-emacs-eval (parse-pvs-message out)))
		    ((string-equal kind "addecl")
		     (apply 'add-declaration-to-file (parse-pvs-message out)))
		    ((string-equal kind "prfst")
		     (apply 'pvs-proofstate (parse-pvs-message out)))
		    ((string-equal kind "prfthd")
		     (apply 'pvs-proof-thread (parse-pvs-message out)))
		    (t (error "%s not handled" kind))
		    )
	      (sit-for 0)
	      (let* ((remout (substring output 0 orig-string-end))
		     (len (length remout)))
		(if (string-equal kind "err")
		    (concat remout "\n")
		    remout)))
	    output))
      (if (string-match ">>Error:[ \n\t]+TO-EMACS:" output)
	  ""
	  output)))

(defun princ-nl (string &optional stream)
  (let ((start 0))
    (save-match-data
      (while (string-match "\\\\n" string start)
	(let ((pos (match-end 0)))
	  (princ (substring string start (- pos 2)) stream)
	  (terpri stream)
	  (setq start pos)))
      (princ (substring string start) stream)
      (terpri stream))))

(defun pvs-process-gc-messages (output)
  (when (string-match ";;; GC:" output)
    (setq output (pvs-remove-substring ";;; GC:" output))
    (message "PVS is garbage collecting...")
    (when (string-match "lucid4.0" pvs-image)
      (setq *pvs-is-garbage* t)))
  (when (string-match pvs-gc-end-regexp output)
    (setq output (pvs-remove-substring pvs-gc-end-regexp output))
    (message "Finished garbage collecting")
    (setq *pvs-is-garbage* nil))
  (when (string-match "^Stack pointer before interrupt was on signal stack.$"
		      output)
    (sit-for 2)
    (continue-process (ilisp-process))
    (message "Hit interrupt bug in Lucid CL - type M-x pvs to restart"))
  (when (string-match
	 "there is no current context!  Disabling eval-form-in-context!"
	 output)
    (setq output
	  (pvs-remove-substring
	   "there is no current context!  Disabling eval-form-in-context!"
	   output)))
  output)


;;; pvs-remove-substring calls string-match on regexp and string, and
;;; returns the result of cutting this string out if it succeeds, nil
;;; otherwise.

(defun pvs-remove-substring (regexp string)
  (save-match-data
    (when (string-match regexp string)
      (let ((start (match-beginning 0))
	    (end (match-end 0)))
	(concat (substring string 0 start) (substring string end))))))

(defun pvs-log-log (msg)
  (let ((buf (current-buffer)))
    (unwind-protect
	 (let* ((cpoint (point))
		(at-end (= cpoint (point-max))))
	   (set-buffer (get-buffer-create "PVS Log"))
	   (define-pvs-key-bindings (current-buffer))
	   (goto-char (point-max))
	   (insert (format "LOG(%s): %s\n"
		       (substring (current-time-string) 4 19)
		     msg))
	   (unless at-end
	     (goto-char cpoint)))
      (set-buffer buf))))

(defun pvs-warning (msg)
  (with-current-buffer (get-buffer-create " *pvs-warn*")
    (define-pvs-key-bindings (current-buffer))
    (goto-char (point-max))
    (insert (format "(%s): %s\n"
	      (substring (current-time-string) 4 19)
	      msg))))

(defun pvs-error (output)
  (apply 'pvs-error* (parse-pvs-message output)))

(defun pvs-error* (file dir msg err place)
  (when (boundp 'pvs-error)
    (setq pvs-error t))
  (if noninteractive
      (progn
	(when pvs-validating
	  (princ (with-current-buffer (find-file-noselect err)
		   (prog1 (format "%s: %s\n" msg (buffer-string))
		     (kill-buffer (current-buffer))))))
	(when (> pvs-verbose 0)
	  (princ (with-current-buffer (find-file-noselect err)
		   (prog1 (format "%s: %s\n" msg (buffer-string))
		     (kill-buffer (current-buffer))))
		 'external-debugging-output))
	(delete-file err)
	(setq pvs-waiting nil))
    (let* ((pos (pvs-get-place place))
	   (buf (when pos
		  (if (lnull dir)
		      (pvs-display-buffer file pos)
		    (pvs-display-file file dir pos))
		  (delete-other-windows))))
      (comint-display-file-output err "PVS Error")
      (delete-file err)
      (when pos
	(with-selected-window (get-buffer-window buf)
	  (recenter '(nil))))
      (message msg)
      t)))

(defun pvs-output (file)
  (when (and noninteractive
	     (not (equal file "NIL")))
    (let* ((buf (get-buffer-create "temp"))
	   (bufstr (with-current-buffer buf
		     (erase-buffer)
		     (insert-file-contents file nil)
		     (buffer-string))))
      (when pvs-validating
	(princ bufstr)
	(terpri))
      (when (> pvs-verbose 0)
	(princ bufstr 'external-debugging-output)
	(terpri 'external-debugging-output))))
  (unless (equal file "NIL")
    (delete-file file)))

(defun comint-display-file-output (file buffer)
  "Put TEXT of file in optional BUFFER and show it in a small temporary
window."
  (setq buffer (or buffer ilisp-output-buffer))
  (with-output-to-temp-buffer buffer
    (with-current-buffer buffer
      (insert-file-contents file)
      (set-buffer-modified-p nil)))
  (save-excursion
    (ilisp-show-output (get-buffer buffer)))
    (set-buffer buffer)
    (when noninteractive
      (when pvs-validating
	(princ (buffer-string))
	(terpri))
      (when (> pvs-verbose 1)
	(princ (buffer-string) 'external-debugging-output)
	(terpri 'external-debugging-output)))
  file)
  
(defvar *pvs-buffers-to-bury* '("PVS Error"
				"Declaration"
				" *Error Output*"
                                "PVS Warnings"
                                "PVS Messages"
				"*Completions*"))

(defvar-local pvs-buffer-kind nil)

(defun pvs-buffer (output)
  (apply 'pvs-buffer* (parse-pvs-message output)))

(defun pvs-buffer* (bufname file display read-only &optional append kind)
  (if noninteractive
      (pvs-buffer-noninteractive bufname file display read-only append kind)
      (let ((obuf (current-buffer)))
	(if (lnull file) ;; Delete the buffer
	    (when (get-buffer bufname)
	      (delete-windows-on bufname)
	      (kill-buffer bufname))
	    (save-excursion
	      (let* ((buf (get-buffer-create bufname))
		     (cpoint (with-current-buffer buf (point)))
		     (append-p (not (lnull append)))
		     (at-end (with-current-buffer buf
			       (= (point) (point-max)))))
		(with-current-buffer buf
		  (unless (lnull kind)
		    (setq-local pvs-buffer-kind kind))
		  (define-pvs-key-bindings buf)
		  (let ((at-end (= (point) (point-max)))
			(contents (buffer-string)))
		    (pvs-set-buffer-mode)
		    (setq buffer-read-only nil)
		    (unless append-p
		      (erase-buffer))
		    (goto-char (point-max))
		    (insert-file-contents file nil)
		    (let ((same (equal (buffer-string) contents)))
		      (delete-initial-blank-lines)
		      (when same
			(set-buffer-modified-p nil)))
		    (when (not (lnull read-only))
		      (set-buffer-modified-p nil)
		      (setq buffer-read-only t))
		    (when (eq major-mode 'fundamental-mode)
		      (pvs-view-mode))
		    (when (and append-p at-end)
		      (goto-char (point-max)))))
		(if (= (point-min) (point-max))
		    (message "PVS sent an empty buffer")
		    (cl-case (intern display)
		      ((nil NIL) nil)
		      ((popto POPTO)
		       (pop-to-buffer buf)
		       (cond (append-p
			      (when at-end
				(goto-char (point-max))))
			     (t (goto-char cpoint)
				(beginning-of-line))))
		      ((temp TEMP)
		       (with-output-to-temp-buffer bufname
			 (set-buffer bufname)
			 (insert-file-contents file nil))
		       (ilisp-show-output buf)
		       (pvs-add-to-buffer-list bufname)
		       (let ((rh (substitute-command-keys "\\[pvs-bury-output]")) 
			     (s (substitute-command-keys "\\[ilisp-scroll-output]")))
			 (message
			  (format 
			      "%s removes help window, %s scrolls, M-- %s scrolls back"
			      rh s s))))
		      (t
		       (when (member (intern display) '(t T))
			 (pop-to-buffer buf)
			 (ilisp-show-output buf)
			 (cond (append-p
				(when at-end
				  (goto-char (point-max))))
			       (t (goto-char (point-min)) ;was cpoint
				  (beginning-of-line)))
			 (sit-for 0)
			 ;;(pop-to-buffer obuf)
			 ))))))
	    (when (file-exists-p file)
	      (delete-file file))))))

(defun pvs-buffer-noninteractive (bufname file display read-only append kind)
  (when (not (lnull file))
    (let* ((buf (get-buffer-create bufname))
	   (bufstr (with-current-buffer buf
		     (erase-buffer)
		     (insert-file-contents file nil)
		     (define-pvs-key-bindings buf)
		     (pvs-set-buffer-mode)
		     (when (eq major-mode 'fundamental-mode)
		       (pvs-view-mode))
		     (buffer-string))))
      (when pvs-validating
	(princ bufstr)
	(terpri))
      (when (> pvs-verbose 1)
	(princ bufstr 'external-debugging-output)
	(terpri 'external-debugging-output)))))

(defun pvs-add-to-buffer-list (bufname)
    "Add to the given buffer name to the list of buffers
     that should be buried with pvs-bury-output."
    (or (member bufname *pvs-buffers-to-bury*)
        (push bufname *pvs-buffers-to-bury*)))

(defvar *pvs-buffer-mode-alist*
  '(("Proof" . edit-proof-mode)
    ("Proofs" . pvs-show-proofs-mode)
    ("Display Proofs" . pvs-browse-proofs-mode)
    ("Declaration" . pvs-view-mode))
  "Associates buffer names with modes, e.g. ((\"Foo\" . foo-mode))")

(defun pvs-set-buffer-mode ()
  (let ((mode (cdr (assoc (or pvs-buffer-kind
			      (buffer-name (current-buffer)))
			  *pvs-buffer-mode-alist*))))
    (when mode (funcall mode))))

(defvar pvs-proof-window-configuration nil)

(defun set-pvs-proof-window-configuration (&optional default-p)
  (interactive)
  (when default-p
    (let ((seqbuf (get-buffer-create "Current Goal"))
	  (prfbuf (get-buffer-create "Proof"))
	  (rulebuf (get-buffer "*pvs*")))
      (switch-to-buffer seqbuf)
      (delete-other-windows)
      (split-window-below)
      (goto-char (point-max))
      (other-window 1)
      (switch-to-buffer rulebuf)
      (split-window nil -3 'above)
      (switch-to-buffer prfbuf nil t)))
  (setq pvs-proof-window-configuration (current-window-configuration)))

(defun pvs-sequent (output)
  (apply 'pvs-sequent* (parse-pvs-message output)))

(defun pvs-sequent* (printout result label comment sequent-file)
  (let* ((sqtbuf (get-buffer-create "Current Goal")))
    (set-buffer sqtbuf)
    (erase-buffer)
    (unless (member printout '("nil" "NIL"))
      (insert-file-contents printout))
    (unless (member result '("nil" "NIL"))
      (goto-char (point-max))
      (insert (format "\n%s\n" result)))
    (goto-char (point-max))
    (insert (format "\n%s :" label))
    (unless (member comment '("nil" "NIL"))
      (goto-char (point-max))
      (insert (format "\n%s" comment)))
    (goto-char (point-max))
    (insert-file-contents sequent-file nil)
    (setq-local font-lock-defaults '(pvs-sequent-font-lock-keywords nil t))
    (goto-char (point-max))
    (unless pvs-proof-window-configuration
      (set-pvs-proof-window-configuration t))
    (set-window-configuration pvs-proof-window-configuration)
    (other-window -1)
    (goto-char (point-max))
    ))

(defun pvs-y-or-n (msg &optional yesno-p timeout-p)
  (if noninteractive
      (progn (message msg)
	     (comint-simple-send (ilisp-process) "t")
	     t)
      (let ((inhibit-quit nil))
	(condition-case ()
	    (if (if (and (not (lnull timeout-p))
			 (fboundp 'with-timeout))
		    (with-timeout (pvs-default-timeout t)
		      (if (equal yesno-p "nil")
			  (yes-or-no-p msg)
			  (y-or-n-p msg)))
		    (if (equal yesno-p "nil")
			(yes-or-no-p msg)
			(y-or-n-p msg)))
		(comint-simple-send (ilisp-process) "t")
		(comint-simple-send (ilisp-process) "nil"))
	  (quit (if (not pvs-initialized)
		    (comint-simple-send (ilisp-process) "nil")
		    (comint-simple-send (ilisp-process) ":abort")
		    (when (boundp 'pvs-error)
		      (setq pvs-error t))
		    (setq pvs-aborted t)
		    (keyboard-quit)))))))


;;; Allow PVS to call a Emacs form and get back the result.

(defun pvs-emacs-eval (form)
  (let ((in-pvs-emacs-eval t)
	(inhibit-quit nil))
    (condition-case ()
	(let ((val (eval (car (read-from-string form)))))
	  (comint-simple-send (ilisp-process)
			      (pvs-convert-to-lisp-string val)))
      (quit (comint-simple-send (ilisp-process) ilisp-reset)
	    (keyboard-quit)))))

(defun pvs-convert-to-lisp-string (value)
  (format "\"%s\""
      (pvs-protect-strings-for-lisp (format "%s" value))))

(defun pvs-protect-strings-for-lisp (string)
  (let ((pos (- (length string) 1))
	(chars nil))
    (while (>= pos 0)
      (let ((ch (aref string pos)))
	(push ch chars)
	(when (= ch ?\")
	  (push ?\\ chars))
	(cl-decf pos)))
    (concat chars)))

(defun resize-info-buffer ()
  (unless (one-window-p t)
    (let* ((maxsize (/ (frame-height) 2))
	   (cursize (1- (window-height)))
	   (lines (real-number-of-lines))
	   (size (min maxsize lines)))
      (enlarge-window (- size cursize)))))


;;; Returns the actual number of lines needed to display the buffer,
;;; allowing for folded lines.

(defun real-number-of-lines ()
  (let ((lines (count-lines (point-min) (point-max)))
	(width (1- (window-width)))
	(num 0))
    (save-excursion
      (goto-char (point-min))
      (dotimes (i lines)
	(end-of-line)
	(when (> (current-column) width)
	  (cl-incf num))
	(beginning-of-line)
	(forward-line 1)))
    (+ lines num)))

(defun pvs-get-place (place)
  (if (string-match "\\([0-9]+\\) \\([0-9]+\\)" place)
      (list (read (substring place (match-beginning 1) (match-end 1)))
	    (read (substring place (match-beginning 2) (match-end 2))))))

(defun pvs-display-file (file dir pos)
  ;; If the file is the current buffer, put point at pos.
  ;; o.w. if there is one window, make two and display file
  ;; If there are two windows, display file in non-current one.
  (let ((buf (if (lnull dir)
		 (get-buffer file)
		 (if (file-equal dir pvs-current-directory)
		     (get-pvs-file-buffer file)
		     (find-file-noselect
		      (format "%s%s.pvs" dir (pathname-name file)))))))
    (pop-to-buffer buf)
    (when pos
      (condition-case ()
	  (progn (goto-char (point-min))
		 (forward-line (1- (car pos)))
		 (forward-char (cadr pos))
		 (setq *pvs-output-pos* (point-marker)))
	(error (goto-char (point-min)))))
    buf))

(defun pvs-display-buffer (bufname pos)
  (pop-to-buffer bufname)
  (when pos
    (condition-case ()
	(progn (goto-char (point-min))
	       (forward-line (1- (car pos)))
	       (forward-char (cadr pos))
	       (setq *pvs-output-pos* (point-marker)))
      (error (goto-char (point-min)))))
  bufname)


(defun parse-pvs-message (output &optional result)
  (let ((pos (string-match "[^\\\\]&" output)))
    (if pos
	(parse-pvs-message (substring output (+ pos 2))
			   (cons (remove-esc
				  (substring output 0 (+ pos 1)))
				 result))
	(reverse (cons (remove-esc output) result)))))

(defun remove-esc (string)
  (let ((new-string ""))
    (while (string-match "\\\\" string)
      (let* ((pos (match-beginning 0))
	     (char (string-to-char (substring string (1+ pos)))))
	(setq new-string
	      (concat
	       new-string
	       (substring string 0 pos)
	       (cond
		 ((eq char ?\&) "&")
		 ((eq char ?\\) "\\")
		 ((eq char ?n) "\n")
		 (t (char-to-string char)))))
	(setq string (substring string (+ 2 pos)))))
    (concat new-string string)))

(defun pvs-query (out)
  (apply 'pvs-query* (parse-pvs-message out)))

(defun pvs-query* (prompt)
  (if noninteractive
      (progn (message prompt)
	     (comint-simple-send (ilisp-process) "t")
	     t)
      (let ((inhibit-quit nil)
	    (query nil))
	(pvs-emacs-query 'query prompt)
	(comint-simple-send
	 (ilisp-process)
	 (cl-case query (?! ":auto") (?y "t") (t "nil"))))))

(defun pvs-make-items (item-list)
  (mapcar #'(lambda (x) (cons (cadr x) (caddr x)))
	  (car (read-from-string item-list))))

(defun pvs-handler (error-p wait-p message output prompt)
  "Given ERROR-P, WAIT-P, MESSAGE, OUTPUT and PROMPT, show the message
and output if there is an error or the output is multiple lines and
let the user decide what to do."
  (if (and (stringp output)
	   (let ((case-fold-search nil))
	     (string-match (ilisp-value 'ilisp-error-regexp) output)))
      (if (and (or (not wait-p) error-p)
	       (setq output (comint-remove-whitespace output))
	       (or error-p (string-match "\n" output)))
	  (let* ((buffer (ilisp-output-buffer))
		 (out (if error-p 
			  (funcall ilisp-error-filter output)
			  output))
		 (key
		  (if (and error-p (not (comint-interrupted)))
		      (if noninteractive
			  ?b
			  (comint-handle-error
			   out
			   "SPC-scroll, I-ignore, K-keep, A-abort sends and keep or B-break: "
			   '(?i ?k ?a ?b)))
		      (if noninteractive
			  ?i
			  (comint-handle-error 
			   out 
			   "SPC-scroll, I-ignore, K-keep or A-abort sends and keep: "
			   '(?i ?k ?a)))))
		 (clear comint-queue-emptied))
	    (if (= key ?i)
		(progn
		  (message "Ignore message")
		  (if buffer 
		      (if (ilisp-temp-buffer-show-function)
			  (funcall (ilisp-temp-buffer-show-function)
				   buffer)
			  (view-buffer buffer))
		      (pvs-bury-output))
		  t)
		(with-current-buffer (get-buffer-create "*Errors*")
		  (define-pvs-key-bindings (current-buffer))
		  (if clear (delete-region (point-min) (point-max)))
		  (goto-char (point-max))
		  (if message
		      (insert message))
		  (insert ?\n)
		  (insert out) 
		  (insert "\n\n"))
		(if clear (setq comint-queue-emptied nil))
		(if (= key ?a)
		    (progn 
		      (message "Abort pending commands and keep in *Errors*")
		      (comint-abort-sends)
		      t)
		    (if (= key ?b)
			(progn
			  (comint-insert
			   (concat comment-start comment-start comment-start
				   message "\n"
				   output "\n" prompt))
			  (if noninteractive
			      (if pvs-validating
				  (progn (pvs-message "ERROR: %s" out) t)
				  (error out))
			      (message "Preserve break") nil))
			(message "Keep error in *Errors* and continue")
			t))))
	  t)
      t))

(defun simple-status-pvs ()
  (with-current-buffer (ilisp-buffer)
    (comint-send-description (car comint-send-queue))))

(defpvs pvs-status environment ()
  "Find out if PVS is busy

This displays information about the PVS command queue in the minibuffer."
  (interactive)
  (with-current-buffer (ilisp-buffer)
    (let* ((queue (pvs-queued-commands comint-send-queue))
	   (stat (comint-send-description (car comint-send-queue))))
      (cond ((string= stat "Done")
	     (message "No commands are running"))
	    ((null queue)
	     (message "stat = %s - may need to reset pvs (M-x reset-pvs)" stat))
	    (t (message "Command %s(%s) is running: %d in queue"
			(caar queue) (cadar queue) (- (length queue) 1)))))))
		   
(defun pvs-queued-commands (queue)
  (let ((pvs-cmds nil))
    (dolist (send queue)
      (when (stringp (car send))
	(let ((form (car (read-from-string (car send)))))
	  (when (and (consp form)
		     (eq (car form) 'progn)
		     (eq (car (caddr form)) 'pvs-errors))
	    (push (list (cl-case (car (cadr (caddr form)))
			  (parse-file "ps")
			  (typecheck-file "tc")
			  (typecheckall "tca")
			  (prove-file-at "pr")
			  (prove-file "prm")
			  (proveall "pra"))
			(cadr (cadr (caddr form))))
		  pvs-cmds)))))
    (nreverse pvs-cmds)))

(defun pvs-prompt (out)
  (condition-case ()
      (cl-case (car (read-from-string (car out)))
	((directory DIRECTORY)
	 (comint-simple-send (ilisp-process)
			     (format "\"%s\""
				 (read-file-name (cadr out)))))
	(t (error "Unknown prompt type - %s" (car out))))
    (error (comint-simple-send (ilisp-process) ":abort")
	   (keyboard-quit))
    (quit  (comint-simple-send (ilisp-process) ":abort")
	   (keyboard-quit))))


;;; pvs-modify-buffer takes a directory, a name, a place, and a contents
;;; and modifies the specified buffer accordingly.  If the directory is
;;; not NIL, then it is assumed to be a .pvs file and the name is the
;;; theory name; otherwise the name is the buffer name.  The place is of
;;; the form (start-line start-col end-line end-col).  The contents is
;;; either a file or NIL; if a file then it is inserted, otherwise
;;; nothing is inserted.

;;; The action taken by this function is to delete the region determined
;;; by the place and to insert the contents if it is file; if it is not,
;;; then this command simply deletes the region.  If the start and end
;;; of the region are the same, then this command simply inserts the
;;; contents.

(defun pvs-modify-buffer (output)
  (apply 'pvs-modify-buffer* (parse-pvs-message output)))

(defun pvs-modify-buffer* (dir file pos textfile)
  (let ((place (car (read-from-string pos))))
    (cond ((lnull dir)
	   (with-current-buffer file
	     (apply 'kill-region (pvs-region place))
	     (unless (lnull textfile)
	       (insert-file-contents textfile))
	     (goto-char (point-min))))
	  (t (pvs-display-file file dir place)
	     (let ((beg (point)))
	       (pvs-display-file file dir (cddr place))
	       (unless (and (not (lnull textfile))
			    (equal (buffer-substring beg (point))
				   (save-excursion
				     (let ((buf (find-file-noselect textfile)))
				       (set-buffer buf)
				       (prog1 (buffer-string)
					 (kill-buffer buf))))))
		 (kill-region beg (point))
		 (unless (lnull textfile)
		   (insert-file-contents textfile)
		   ;;(forward-char -1)
		   ;;(when (looking-at "\n")
		   ;;  (delete-char 1))
		 )))))
    (unless (lnull textfile)
      (delete-file textfile))))

;(defun pvs-modify-buffer (output)
;  (apply 'pvs-mod-buffer (parse-pvs-message output)))
;
;;; let lisp destructure the list for us
;(defun pvs-mod-buffer (dir file pos new-text)
;  (let ((place (pvs-get-place pos)))
;    (pvs-display-file file dir place)
;    (let ((beg (point)))
;      (pvs-display-file file dir (cddr place))
;      (kill-region beg (point))
;      (insert new-text))))


(defun pvs-display (out)
  (let* ((proof (car out))
	 (instance (cadr out))
	 (type (caddr out))
	 (value (cadddr out))
	 (buffer-name (format "*status-%s*" proof))
	 (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (define-pvs-key-bindings buffer)
      (outline-mode)
      (setq-local outline-regexp "[^ \n\r]+")
      (cond ((string-equal type "CREATE")
	     (erase-buffer)
	     (insert "\n" proof " (    )" value)
	     (delete-char -1)
	     (goto-char (point-min))
	     (outline-hide-body))
	    ((string-equal type "NEW-STATE")
	     (goto-char (point-min))
	     (re-search-forward (format "[\n\r]%s ([^\)]*)"
					(regexp-quote instance)))
	     (let ((inst-beg (point))
		   (hidden (looking-at "\r")))
	       (if (re-search-forward "[\n\r][^ \n\r]" nil 'end)
		   (backward-char 2))
	       (let ((end (point)))
		 (re-search-backward "[\n\r] Current state:" inst-beg t)
		 (delete-region (point) end)
		 (let ((beg (point)))
		   (insert "\n Current state:" value)
		   (delete-char -1)
		   (if hidden
		       (subst-char-in-region beg (point) ?\n ?\r t))))))
	    ((string-equal type "STATUS")
	     (goto-char (point-min))
	     (re-search-forward (format "[\n\r]%s (\\([^\)]*\\))"
					(regexp-quote instance)))
	     (goto-char (match-beginning 1))
	     (delete-region (match-beginning 1)
			    (match-end 1))
	     (insert (cond ((string-equal value "?") "work")
			   ((string-equal value "!") "done")
			   ((string-equal value "X") "fail")
			   ((string-equal value "*") "wait")
			   ((lnull value) "    ")
			   (t "????"))))
	    ((string-equal type "CHILD")
	     (goto-char (point-min))
	     (let* ((last-dot (progn
				(string-match ".*\\." instance)
				(1- (match-end 0))))
		    (parent (substring instance 0 last-dot)))
	       (re-search-forward (format "[\n\r]%s ([^\)]*)"
					  (regexp-quote parent)))
	       (let ((whole-hidden
		      (save-excursion
			(goto-char (match-beginning 0))
			(looking-at "\r")))
		     (body-hidden (looking-at "\r")))
		 (if (re-search-forward "[\n\r][^ \n\r]" nil 'end)
		     (backward-char 2))
		 (if whole-hidden
		     (insert "\r")
		     (insert "\n"))
		 (let ((beg (point)))
		   (insert instance " (    )" value)
		   (delete-char -1)
		   (if body-hidden
		       (subst-char-in-region beg (point) ?\n ?\r t))))))
	    ((string-equal type "HIDE-ALL")
	     (goto-char (1+ (point-min)))
	     (outline-hide-subtree))
	    ((string-equal type "SHOW")
	     (goto-char (point-min))
	     (re-search-forward (format "[\n\r]%s (\\([^\)]*\\))"
					(regexp-quote instance)))
	     (goto-char (match-beginning 0))
	     (subst-char-in-region (point) (1+ (point)) ?\r ?\n t)
	     (forward-char 1)
	     (outline-show-entry))
	    (t (error "Unknown display type %s" type))))))	   

(defun pvs-locate (output)
  (let* ((message (parse-pvs-message output))
	 (dir (car message))
	 (file (cadr message))
	 (pos (pvs-get-place (caddr message))))
    (pvs-display-file file dir pos)))

(defpvs pvs-set-linelength prettyprint (&optional length)
  "If called with no argument sets the PVS prettyprinter line length to be the
width of the current Emacs frame.  Otherwise, sets the line length to the
specified argument."
  (interactive "p")
  (setq *default-char-width* (or length
				 (frame-width (selected-frame))))
  (pvs-send (format "(setq *default-char-width* %d)" *default-char-width*)))


(defun pvs-auto-set-linelength (frame)
  "A function suitable for putting in the window-size-change-functions
hook.  For example:

(when (and (boundp 'window-size-change-functions)
           (not noninteractive))
  (push 'pvs-auto-set-linelength window-size-change-functions))

This hook is not available with all versions of [X]Emacs, so is
not automatically used in PVS.  See pvs-set-linelength function
for the interactive function."
  (unless (= (frame-width frame)
	     *default-char-width*)
    (pvs-set-linelength (frame-width frame))))


;;; This is return-ilisp modified to put an extra backslash in front of any
;;; given ones.  Put
;;;  (define-key ilisp-mode-map "\C-m" 'return-ilisp-pvs)
;;; in your .pvsemacs file.

(defun return-ilisp-pvs ()
  "Grab the current expression with comint-get-old-input.  If we have
a complete sexp, send it.  Otherwise, indent appropriately."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (input1 (ilisp-get-old-input))
	       (input (protect-backslash-for-lisp input1)))
	(if input
	    (progn 
	      (if (>= (point) pmark)
		  (goto-char (point-max))
		(goto-char pmark)
		(insert input))
	      (if (not ilisp-no-newline) (insert ?\n))
	      (if (and (funcall comint-input-filter input)
		       (or (ring-empty-p (ilisp-get-input-ring))
			   (not (string= (ring-ref (ilisp-get-input-ring) 0)
					 input))))
		  (ilisp-ring-insert (ilisp-get-input-ring) input))
	      (run-hook-with-args 'comint-input-filter-functions input)
	      ;; Ugh, comint changing under my feet....
	      ;; Note: This used to be
	      ;;        (eq ilisp-emacs-version-id 'gnu-19)
	      ;;       25/11/94 Marco Antoniotti
	      (when (or (eq +ilisp-emacs-version-id+ 'fsf-19)
			  (eq +ilisp-emacs-version-id+ 'fsf-20)
			  (eq +ilisp-emacs-version-id+ 'fsf-21))
		  (setq comint-input-ring-index nil))
	      ;; Nuke symbol table
	      (setq ilisp-original nil)
	      (funcall comint-input-sender proc input)
	      (set-marker (process-mark proc) (point))
	      (set-marker comint-last-input-end (point))
	      (goto-char (point-max)))
	  (if (= pmark (point-max)) 
	      (let ((comint-send-newline t))
		(when (not ilisp-no-newline) (insert ?\n))
		(set-marker (process-mark proc) (point))
		(funcall comint-input-sender proc ""))
	    (insert ?\n)
	    (save-restriction
	      (narrow-to-region pmark (point-max))
	      (funcall indent-line-function))))))))

(defun protect-backslash-for-lisp (string)
  (let ((pos (- (length string) 1))
	(chars nil))
    (while (>= pos 0)
      (let ((ch (aref string pos)))
	(when (= ch ?\\)
	  (push ch chars))
	(push ch chars)
	(cl-decf pos)))
    (concat chars)))


;;; reset-pvs

(defpvs reset-pvs interrupts ()
  "Reset the inferior LISP top level

The reset-pvs command aborts any ongoing activity in PVS; its effects
depend on whether it is issued from the *pvs* buffer or from some other
buffer.  In the former case, reset-pvs simply interrupts PVS, invoking the
lisp debugger.  The options here are to abort to the top level (by typing
:reset in Allegro, and :a in Lucid), or, in the prover, to type (restore)
which restores the proof to the state prior to the last atomic prover
command.  If reset-pvs is issued somewhere other than the *pvs* buffer,
you are asked whether to reset PVS in case the command was typed
accidentally; if not, the current command is aborted and the command queue
is emptied."
  (interactive)
  (if (ilisp-process)
      (if (and (not noninteractive)
	       (not (y-or-n-p "Reset PVS? ")))
	  (message "")
	  (pvs-bury-output)
	  (message "Resetting PVS")
	  (when pvs-in-checker
	    (comint-simple-send (ilisp-process) (format "(quit)y\nno")))
	  (comint-send (ilisp-process) ":reset")
 	  (sleep-for 1)
	  (interrupt-process (ilisp-process))
	  (with-current-buffer (ilisp-buffer)
	    (comint-setup-ipc t)
	    (goto-char (point-max))
	    (set-marker (process-mark (ilisp-process)) (point))
	    (set-marker comint-last-input-end (point))
	    (setq comint-handler 'pvs-handler)
	    (setq comint-output-filter 'pvs-output-filter))
	  (sit-for 1)
	  (setq pvs-recursive-process-filter nil)
	  (set-process-filter (ilisp-process) 'pvs-process-filter)
	  (comint-simple-send (ilisp-process) (ilisp-value 'ilisp-reset))
	  (setq pvs-in-checker nil)
	  (setq pvs-in-evaluator nil)
	  (message "PVS has been reset"))
      (message "PVS is no longer running - exit and start again")))

;;(defvar pvs-in-tcp nil)
  
(defpvs pvs-interrupt-subjob environment ()
  "Interrupt PVS lisp."
  (interactive)
  (when *pvs-is-garbage*
    (message "PVS disallows interrupts during GC in lucid4.0"))
  (unless *pvs-is-garbage*;; PDL nov94 to stop interrupt during gc
    (with-current-buffer (ilisp-buffer)
      (comint-interrupt-subjob)
      ;; (if (and pvs-in-tcp
      ;; 	       (yes-or-no-p "tcp interrupted: continue with next formula?"))
      ;; 	  (comint-simple-send (ilisp-process) "(restore)")
      ;; 	  ;; (cl-case pvs-lisp
      ;; 	  ;;   (allegro (comint-simple-send (ilisp-process ":reset")))
      ;; 	  ;;   (sbclisp (comint-simple-send (ilisp-process ":abort"))))
      ;; 	  (setq pvs-in-tcp nil)
      (setq comint-send-queue
	    (list (list nil nil nil 'run "Top Level"
			nil t nil 0 (cons nil nil)))
	    comint-end-queue comint-send-queue)
      (set-marker (process-mark (ilisp-process)) (point-max))
      (lisp-pop-to-buffer (ilisp-buffer))
      (goto-char (point-max))))
  ;;)
  t)

(defun pvs-bury-output ()
  "Bury all temporary windows"
  (interactive)
  (when expanded-form-overlay
    (delete-overlay expanded-form-overlay)
    (setq expanded-form-overlay nil))
  (unless (one-window-p t)
    (ilisp-bury-output)
    (mapcar #'ilisp-bury-output *pvs-buffers-to-bury*)))

(defun lnull (obj)
  (or (null obj) (member obj '("nil" "NIL"))))
