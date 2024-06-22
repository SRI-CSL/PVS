;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; ilxemacs.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;;;============================================================================
;;; Variables

;;; XEmacs 19.14 doesn't have comint-input-chunk-size but earlier
;;; versions do, so we define here if needed. (toy@rtp.ericsson.se)

(if (not (boundp 'comint-input-chunk-size))
    (setq comint-input-chunk-size 512))



;;;============================================================================
;;; Functions

(defun ilisp-get-input-ring ()
  "Use instead of get-input-ring coming-input-ring or input-ring."
  comint-input-ring)


(defun ilisp-ring-insert (ring input)
  (ring-insert ring input))


(defun ilisp-temp-buffer-show-function-symbol ()
  'temp-buffer-show-function)


(defun set-ilisp-temp-buffer-show-function (val)
  (setq temp-buffer-show-function val))


(defun ilisp-temp-buffer-show-function ()
  temp-buffer-show-function)


(defun ilisp-input-ring-index ()
  comint-input-ring-index)


(defun set-ilisp-input-ring-index (n)
  (setq comint-input-ring-index n))


(defun ilisp-input-ring-size ()
  comint-input-ring-size)

(defun set-ilisp-input-ring-size (n)
  (setq comint-input-ring-size n))

(defun get-event ()
  (let ((event (next-command-event)))
    (if (key-press-event-p event)
	(downcase (event-to-character event))
	0)))

(defun ilisp-byte-code-to-list (function)
  "Returns a list suitable for passing to make-byte-code from FUNCTION."
  (let ((function-object 
	 (if (symbolp function)
	     (symbol-function function)
	   function)))
    (read (concat "("
		  (substring (let ((print-readably t))
			       (prin1-to-string function-object))
			     2 -1)
		  ")"))))

(provide 'ilxemacs)
