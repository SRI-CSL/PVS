;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-kil.el --

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
;;; ILISP Panic/Reset/Status commands
;;;

;;;%% Panic/Reset/Status commands 
;;;
(defun status-lisp (showp)
  "Show the message of the current command being executed in the
inferior LISP.  With a prefix show pending sends as well."  
  (interactive "P")
  (save-excursion
    (set-buffer (ilisp-buffer))
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
	(save-excursion
	  (set-buffer (ilisp-buffer))
	  (comint-setup-ipc t)
	  (message "LISP is reset, state is unknown"))
	(message ""))))

;;;
(defun repair-ilisp ()
  "If ilisp is not listening to you in the lisp interaction buffer, you might try this."
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
