;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-inp.el --

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
;;; ILISP input functions
;;;

;;;%%Input 
(defun lisp-at-start ()
  "Return the point if you are at the start of an input expression in
an inferior Lisp."
  (save-excursion
    (let ((point (point)))
      (beginning-of-line)
      (comint-skip-prompt)
      (if (= point (point))
	  point))))

;;;
(defun lisp-input-start ()
  "Go to the start of the input region."
  (let* ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (if (>= (point) pmark)
	(goto-char pmark)
	(progn 
	  (end-of-line)
	  (if (re-search-backward comint-prompt-regexp (point-min) 'stay)
	      (comint-skip-prompt)
	      (point))))))
