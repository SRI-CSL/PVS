;;; -*- Mode: Emacs-Lisp -*-

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
  (if (eq +ilisp-emacs-version-id+ 'lucid-19)
      (get-input-ring)
      ;; else lucid-19-new
      comint-input-ring))


(defun ilisp-ring-insert (ring input)
  (if (eq +ilisp-emacs-version-id+ 'lucid-19)
      (ring-insert-new ring input)
      (ring-insert ring input)))


(defun ilisp-temp-buffer-show-function-symbol ()
  'temp-buffer-show-function)


(defun set-ilisp-temp-buffer-show-function (val)
  (setq temp-buffer-show-function val))


(defun ilisp-temp-buffer-show-function ()
  temp-buffer-show-function)


(defun ilisp-input-ring-index ()
  (if (eq +ilisp-emacs-version-id+ 'lucid-19-new)
      comint-input-ring-index
      input-ring-index))


(defun set-ilisp-input-ring-index (n)
  (if (eq +ilisp-emacs-version-id+ 'lucid-19-new)
      (setq comint-input-ring-index n)
      (setq input-ring-index n)))


(defun ilisp-input-ring-size ()
  (if (eq +ilisp-emacs-version-id+ 'lucid-19-new)
      comint-input-ring-size
      input-ring-size))


(defun set-ilisp-input-ring-size (n)
  (if (eq +ilisp-emacs-version-id+ 'lucid-19-new)
      (setq comint-input-ring-size n)
      (setq input-ring-size n)))


;;============================================================================
;;; Epilogue

(provide 'il-luc19)

;;; end of file -- ilxemacs.el --
