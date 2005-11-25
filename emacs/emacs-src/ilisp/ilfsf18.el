;;; -*- Mode: Emacs-Lisp -*-

;;; ilfsf18.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;============================================================================
;;; Prologue

(if (string-match "2\.03" comint-version)
    (fset 'comint-mem 'member))


;;;============================================================================
;;; Functions

(defun add-hook (hook function)
  " Add FUNCTION to HOOK's list.
Arguments are HOOK and FUNCTION. FUNCTION is not added if it's already
on the list."
  (set hook
       (if (boundp hook)
	   (let ((value (symbol-value hook)))
	     (if (and value (or (not (consp value)) (eq (car value) 'lambda)))
		 (setq value (cons value nil)))
	     (if (not (comint-mem function value))
		 (setq value (append value (list function))))
	     value)
	 (list function))))


(defun ilisp-get-input-ring ()
  "Use instead of get-input-ring coming-input-ring or input-ring."
  input-ring)


(defun ilisp-ring-insert (ring input)
  "See 'ring-insert'."
  (ring-insert ring input))


(defun ilisp-temp-buffer-show-function-symbol ()
  "See 'temp-buffer-show-hook'."
  'temp-buffer-show-hook)


(defun set-ilisp-temp-buffer-show-function (val)
  "See 'temp-buffer-show-hook' set function."
  (setq temp-buffer-show-hook val))


(defun ilisp-temp-buffer-show-function ()
  "See 'temp-buffer-show-hook'."
  temp-buffer-show-hook)


(defun ilisp-input-ring-index ()
  "See 'input-ring-index'."
  input-ring-index)


(defun set-ilisp-input-ring-index (n)
  "See 'input-ring-index' set function."
  (setq input-ring-index n))


(defun ilisp-input-ring-size ()
  "See 'input-ring-size'."
  input-ring-size)


(defun set-ilisp-input-ring-size (n)
  "See 'input-ring-size' set function."
  (setq input-ring-size n))


;;;============================================================================
;;; Epilogue

(provide 'compat-fsf18)

;;; end of file -- il-fsf18.el --
