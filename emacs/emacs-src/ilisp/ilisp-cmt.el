;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cmt.el --

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
;;; ILISP comint interface code.
;;;
;;;


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

