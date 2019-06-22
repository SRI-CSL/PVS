;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-hnd.el --
;;; ILISP Error handler
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;; Do not handle errors by default.
(defvar ilisp-handle-errors nil)

;;;
(defun ilisp-handler (error-p wait-p message output prompt)
  "Given ERROR-P, WAIT-P, MESSAGE, OUTPUT and PROMPT, show the message
and output if there is an error or the output is multiple lines and
let the user decide what to do."
  (if (not ilisp-handle-errors)
      (progn
	(if message
	    (progn
	      (setq ilisp-last-message message
		    ilisp-last-prompt prompt)
	      (if (not wait-p) (lisp-display-output output))))
	nil)
    (if (and (not wait-p)
	     (setq output (comint-remove-whitespace output))
	     (or error-p (string-match "\n" output)))
	(let* ((buffer (ilisp-output-buffer t))
	       (out (if error-p 
			(funcall ilisp-error-filter output)
		      output))
	       (key
		(if (and error-p (not (comint-interrupted)))
		    (comint-handle-error
		     out
     "SPC-scroll, I-ignore, K-keep, A-abort sends and keep or B-break: "
		     '(?i ?k ?a ?b))
		  (comint-handle-error 
		   out 
	   "SPC-scroll, I-ignore, K-keep or A-abort sends and keep: "
		   '(?i ?k ?a))))
	       (clear comint-queue-emptied))
	  (if (= key ?i)
	      (progn
		(message "Ignore message")
		(if buffer 
		    (funcall
		     (ilisp-temp-buffer-show-function)
		     buffer)
		  (ilisp-bury-output))
		t)
	    (save-excursion
	      (set-buffer (get-buffer-create "*Errors*"))
	      (if clear (delete-region (point-min) (point-max)))
	      (goto-char (point-max))
	      (insert message)
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
		    (message "Preserve break") nil)
		(message "Keep error in *Errors* and continue")
		t))))
      t)))

;;;
(defun ilisp-abort-handler ()
  "Handle when the user aborts commands."
  (setq ilisp-initializing nil
	ilisp-load-files nil)
  (let ((add nil))
    (while ilisp-pending-changes
      (if (not (memq (car ilisp-pending-changes) lisp-changes))
	  (setq add (cons (car ilisp-pending-changes) add)))
      (setq ilisp-pending-changes (cdr ilisp-pending-changes)))
    (setq lisp-changes (nconc lisp-changes add))))
