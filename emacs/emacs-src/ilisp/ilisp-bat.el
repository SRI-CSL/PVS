;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-bat.el --

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
;;; Inferior LISP interaction package batch submodule.

;;; See ilisp.el for more information.
(defun mark-change-lisp (arg)
  "Mark the current defun as being changed so that lisp-eval-changes,
or lisp-compile-changes will work on it.  With a prefix, unmark."
  (interactive "P")
  (let (point name)
    (save-excursion
      (setq point (lisp-defun-begin)
	    name (lisp-def-name)))
    (if arg
	(let ((marker (car (lisp-memk point lisp-changes 'marker-position))))
	  (message "%s marked as unchanged" name)
	  (setq lisp-changes (delq marker lisp-changes)))
	(message "%s marked as changed" name)
	(if (not (lisp-memk point lisp-changes 'marker-position))
	    (let ((new (make-marker)))
	      (set-marker new point)
	      (setq lisp-changes (cons new lisp-changes)))))))

;;;
(defun list-changes-lisp ()
  "List the name of LISP forms currently marked as being changed."
  (interactive)
  (let ((names (reverse (mapcar (function
				 (lambda (change)
				  (save-excursion
				    (set-buffer (marker-buffer change))
				    (goto-char change)
				    (lisp-def-name))))
				lisp-changes))))
    (if names
	(with-output-to-temp-buffer "*Changed-Definitions*"
	  (display-completion-list names)
	  (save-excursion
	    (set-buffer "*Changed-Definitions*")
	    (goto-char (point-min))
	    (kill-line)
	    (insert "Changed LISP forms:")))
	(error "No changed definitions"))))

;;;
(defun clear-changes-lisp ()
  "Clear the list of LISP forms currently marked as being changed."
  (interactive)
  (message "Cleared changes")
  (setq lisp-changes nil))

;;;
(defun lisp-change-handler (&rest args)
  "Handle an error during a batch process by keeping the change on the
list and passing it on to the normal error handler." 
  (let ((change (car ilisp-pending-changes)))
    (if (and comint-errorp
	     (not (lisp-memk change lisp-changes 'marker-position)))
	(setq lisp-changes (nconc lisp-changes (cons change nil)))))
  (setq ilisp-pending-changes (cdr ilisp-pending-changes))
  (apply comint-handler args))

;;;
(defun lisp-changes (command message)
  "Apply COMMAND to each of the changes and use MESSAGE to print a
message given the name of the change.  If there is a positive prefix,
the change list will not be changed."
  (save-excursion
    (set-buffer (ilisp-buffer))
    (let ((keep (and current-prefix-arg (not (eq current-prefix-arg '-))))
	  (changes (reverse lisp-changes))
	  (lisp-wait-p nil))
      (setq ilisp-pending-changes (nconc ilisp-pending-changes changes)
	    current-prefix-arg nil)	;Prevent buffer insertion
      (if comint-queue-emptied 
	  (save-excursion
	    (setq comint-queue-emptied nil)
	    (set-buffer (get-buffer-create "*Errors*"))
	    (delete-region (point-min) (point-max))))
      (while changes
	(let* ((change (car changes))
	       name)
	  (set-buffer (marker-buffer change))
	  (goto-char change)
	  (setq name (lisp-def-name))
	  (forward-sexp)
	  (funcall command change (point) nil (format message name)
		   nil 'lisp-change-handler)
	  (setq changes (cdr changes))))
      (comint-send-code
       (ilisp-process)
       (function (lambda ()
	 (save-excursion
	   (set-buffer (get-buffer-create "*Last-Changes*"))
	   (delete-region (point-min) (point-max))
	   (insert (save-excursion
		     (set-buffer "*Errors*")
		     (buffer-string)))))))
      (if keep
	  (message "Started, but keeping changes")
	  (message "Started changes")
	  (setq lisp-changes nil)))))

;;;
(defun eval-changes-lisp ()
  "Evaluate the forms marked as being changed.  With prefix, do not
clear the change list."
  (interactive)
  (lisp-changes 'eval-region-lisp "Evaluate changed %s"))

;;;
(defun compile-changes-lisp ()
  "Compile the forms marked as being changed.  With prefix, do not
clear the change list."
  (interactive)
  (lisp-changes 'compile-region-lisp "Compile changed %s"))
