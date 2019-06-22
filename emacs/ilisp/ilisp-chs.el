;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-chs.el --
;;; CLISP Common Lisp by Bruno Haible and Michael Stoll dialect definition.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(require 'cl)

;;; clisp-hs-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

;;; (defun clisp-hs-check-prompt (old new)
;;;  "Compare the break level printed at the beginning of the prompt."
;;;  (let* ((was-in-break (and old (string-match "Break>" old)))
;;; 	 (old-level (if was-in-break
;;; 			(- (match-end 0) (match-beginning 0))
;;; 			0))
;;; 	 (is-in-break (string-match "Break>" new))
;;; 	 (new-level (if is-in-break
;;; 			(- (match-end 0) (match-beginning 0))
;;; 			0)))
;;;    (<= new-level old-level)))

;;; clisp-hs-check-prompt -- New version
;;; (according to the description in comint-ipc, this should be the
;;; correct behavior)
;;;
;;; 19990912 Martin Atzmuller

(defun clisp-hs-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in (and old
		      (string-match "Break" old)
		      (string-match "[0-9]+" old)))
	 (old-level (if was-in
 			(string-to-number
			 (substring old (match-beginning 0)
				    (match-end 0)))
		      0))
	 (is-in (and
		 (string-match "Break" new)
		 (string-match "[0-9]+" new)))
	 (new-level (if is-in
 			(string-to-number
			 (substring new (match-beginning 0)
				    (match-end 0)))
		      0)))
    (<= new-level old-level)))


;;; clisp-hs --
;;;
;;; Notes:
;;; 19991219 Martin Atzmueller
;;; replaced clisp-hs-check-prompt again.
;;;
;;; 19990912 Martin Atzmueller
;;; replaced clisp-hs-check-prompt with new function
;;;
;;; 19990828 Paolo Amoroso
;;; Added initial support for ILD and modified COMINT-PROMPT-REGEXP (the
;;; previous value didn't take into account the space which is the last
;;; character of the prompt).
;;;
;;; 19990806 Martin Atzmueller
;;; Various changes to make the dialect definition friendlier.
;;;
;;; 19990806 Marco Antoniotti
;;; Since I changed the name of the main dialect, I could conceivably
;;; change the name of the CLISP dialect.

(defvar ilisp-clisp-hs-init-file "cl-chs-init.lisp")

(defdialect clisp-hs "CLISP H.S." common-lisp
  (ilisp-load-init 'clisp-hs ilisp-clisp-hs-init-file)
  (setq
   ilisp-load-or-send-command "(and (or (print \"%s\") t) (load \"%s\"))"
   ilisp-error-regexp
   "\\(ILISP:[^\"]*\\)\\|\\(\\*\\*[^\n]*\\)"

   ilisp-reset "(sys::debug-unwind)"
   ilisp-block-command "(progn %s)"
   ilisp-find-source-command nil
   ilisp-callers-command nil

   ;; Note:
   ;; 19990920
   ;; The global definition should now take care to find out the
   ;; proper extension.  See file 'ilisp-cl.el'.
   ;; ilisp-binary-extension "fas"

   comint-prompt-regexp "^\\([0-9]+\\. Break \\[[0-9]+\\]> \\|[^>]*> \\)"
   comint-interrupt-regexp "\\(\\*\\*\\* - [A-Za-z]*: User break\\)"
   comint-fix-error "(sys::debug-unwind)"
   comint-continue "continue"
   comint-prompt-status
   (function
    (lambda (old line)
     (comint-prompt-status old line 'clisp-hs-check-prompt))))

  ;; ILD Support. NIL values mean that more work is needed or that the
  ;; particular command is not available

  (setq ild-abort-string "(sys::debug-unwind)"
	ild-continue-string "continue"
	ild-next-string "up"
	ild-next-string-arg nil
	ild-previous-string "down"
	ild-previous-string-arg nil
	ild-top-string "top"
	ild-bottom-string "bottom"
	ild-backtrace-string "backtrace"
	ild-locals-string nil
	ild-local-string-arg nil
	ild-return-string "return"
	ild-retry-string "redo"
	ild-trap-on-exit-string "break+" ; I'm not sure about this
	))

(unless clisp-hs-program
  (setq clisp-hs-program "clisp -ansi -I")) ; ANSI mode, ILISP friendly

(provide 'ilisp-chs)

;;; end of file -- ilisp-chs.el --
