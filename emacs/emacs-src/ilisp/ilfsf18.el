;;; -*- Mode: Emacs-Lisp -*-

;;; ilfsf18.el --

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
