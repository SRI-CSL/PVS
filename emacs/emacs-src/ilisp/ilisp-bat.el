;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-bat.el --
;;; Inferior LISP interaction package batch submodule.
;;; See ilisp.el for more information.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(defun mark-change-lisp (arg)
  "Mark the current defun as being changed.
This is to make 'lisp-eval-changes' or 'lisp-compile-changes' work on
it.  With a prefix, unmark."
  (interactive "P")
  (let (point name)
    (save-excursion
      (setq point (lisp-defun-begin)
	    name (lisp-def-name)))
    (if arg
	(let ((marker (car (member* point lisp-changes
				    :test #'equal
				    :key #'marker-position))))
	  (message "%s marked as unchanged" name)
	  (setq lisp-changes (delq marker lisp-changes)))
	(message "%s marked as changed" name)
	(if (not (member* point lisp-changes
			  :test #'equal
			  :key #'marker-position))
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
    (when (and comint-errorp
	       (not (member* change lisp-changes
			     :test #'equal
			     :key #'marker-position)))
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
