;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; ilisp-kil.el --
;;; ILISP Panic/Reset/Status commands.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.

(declare-function return-ilisp "ilisp-xfr")

;;;%% Panic/Reset/Status commands 
;;;
(defun status-lisp (showp)
  "Show the message of the current command being executed in the
inferior LISP.  With a prefix show pending sends as well."  
  (interactive "P")
  (with-current-buffer (ilisp-buffer)
    (comint-current-send showp)))


;;;
(defun reset-ilisp ()
  "Reset the inferior LISP top level."
  (interactive)
  (message "Reset LISP to top level")
  (comint-simple-send (ilisp-process) (ilisp-value 'ilisp-reset)))

;;;
(defun abort-commands-lisp (&optional message)
  "Abort the commands sent to the current ilisp."
  (interactive)
  (if (ilisp-value comint-aborting t)
      (message "Already aborted commands")
      (beep)
      (message (or message "Aborted commands"))
      (comint-abort-sends (ilisp-process))))

;;;
(defun panic-lisp ()
  "Panic reset for the inferior LISP."
  (interactive)
  (save-excursion
    (if (y-or-n-p "Panic reset LISP? ")
	(with-current-buffer (ilisp-buffer)
	  (comint-setup-ipc t)
	  (message "LISP is reset, state is unknown"))
	(message ""))))

;;;
(defun repair-ilisp ()
  "Try this if ilisp is not listening to you in the lisp interaction buffer."
  (interactive)
  (set-buffer (ilisp-buffer))
  (comint-setup-ipc t)
  (goto-char (point-max))
  (insert "()")
  (return-ilisp)
  (message "ILISP is working again (maybe)"))


;;;
(defun interrupt-subjob-ilisp ()
  "Interrupt the current top level command in the inferior LISP."
  (interactive)
  (if (not (eq comint-send-queue comint-end-queue))
      (if (y-or-n-p "Abort commands before interrupting top level? ")
	  (abort-commands-lisp)
	  (message "Waiting for commands to finish")
	  (while (not (eq comint-send-queue comint-end-queue))
	    (accept-process-output)
	    (sit-for 0))))
  (message "Interrupted top level")
  (comint-interrupt-subjob))
