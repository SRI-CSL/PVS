;;; -*- Mode: Emacs-Lisp -*-

;;; ilfsf19.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


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

(defmacro defgroup (&rest args))

(defmacro defcustom (symbol value doc &rest args)
  `(defvar ,symbol ,value ,doc))


;;;============================================================================
;;; Epilogue

(provide 'compat-fsf-19)

;;; end of file -- fsf19.el --
