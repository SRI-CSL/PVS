;;; -*- Mode: Emacs-Lisp -*-

;;; illuc19.el --

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
  (setq debug-on-error t)
  (lkjsdlfkjsdlkjflksdj)
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


;;============================================================================
;;; Epilogue

(provide 'il-xemacs)

;;; end of file -- il-luc19.el --

