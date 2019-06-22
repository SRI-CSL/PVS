;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-inp.el --
;;; ILISP input functions
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;;;%%Input 
(defun lisp-at-start ()
  "Return `point' when at start of an input expression in an inferior Lisp."
  (save-excursion
    (let ((point (point)))
      (beginning-of-line)
      (comint-skip-prompt)
      (if (= point (point))
	  point))))


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

;;; end of file -- ilisp-inp.el --
