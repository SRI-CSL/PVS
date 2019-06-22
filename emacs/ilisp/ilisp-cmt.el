;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cmt.el --
;;; ILISP comint interface code.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;%Process interface
;;;%%Comint 
(defun ilisp-get-old-input ()
  "Snarf the sexp starting at the nearest previous prompt, or NIL if none."
  (save-excursion
    (let* ((begin (lisp-defun-begin))
	   (pmark (process-mark (get-buffer-process (current-buffer))))
	   (once (if (< (point) pmark)
		     (save-excursion (end-of-line) (point))))
	   (end nil)
	   (done nil))
      (condition-case ()
	  (while (and (not done) (< (point) (point-max)))
	    (forward-sexp)
	    (setq end (point))
	    (skip-chars-forward " \t\n")
	    (if (and once (>= (point) once)) (setq done t)))
	(error (setq end nil)))
      (if end (buffer-substring begin end)))))

;;;
(defun ilisp-input-filter (str)
  "Don't save anything matching ilisp-filter-regexp or less than
ilisp-filter-length long."
  (and (not (string-match ilisp-filter-regexp str))
       (> (length str) ilisp-filter-length)))

;;;
(defun ilisp-error-filter (output)
  "Keep from OUTPUT only what matches ilisp-error-regexp or everything
if there is no match."
  (if (string-match (ilisp-value 'ilisp-error-regexp) output)
      (substring output (match-beginning 0) (match-end 0))
      output))



(defun newline-and-indent-lisp ()
  "If at the end of the buffer, send the string back to the process
mark with no newline.  Otherwise, insert a newline, then indent.  In
an ilisp buffer the region is narrowed first.  See newline-and-indent
for more information."
  (interactive "*")
  (if ilisp-complete
      (exit-minibuffer)
      (let (input)
	(if (and (= (point) (point-max)) 
		 (memq major-mode ilisp-modes)
		 (setq input (ilisp-get-old-input)))
	    (let ((process (ilisp-process))
		  (comint-send-newline (not comint-send-newline)))
	      (funcall comint-input-sender process input)
	      (set-marker (process-mark process) (point)))
	    (save-restriction
	      (if (memq major-mode ilisp-modes)
		  (narrow-to-region (save-excursion (lisp-input-start))
				    (point-max)))
	      (newline-and-indent))))))

