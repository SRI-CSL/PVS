;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mnb.el --

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




;(require 'ilisp-key)

(defvar lisp-general-menu-map (make-sparse-keymap "Lisp")
  "Keymap for main LISP menu")


(defkey-ilisp [menu-bar lisp]
    (cons "Lisp" lisp-general-menu-map))


(defkey-ilisp [menu-bar lisp repair]
    '("Repair Connection" . repair-ilisp))

(defkey-ilisp [menu-bar lisp reset]
    '("Reset Connection" . reset-ilisp))

(defkey-ilisp [menu-bar lisp comment-region]
  '("Comment Region" . comment-region))

;;; (defkey-ilisp [menu-bar lisp sep-1]
;;;   '("-" . ilisp-nop))

(defkey-ilisp [menu-bar lisp macroexpand]
  '("Macroexpand" . macroexpand-lisp))

(defkey-ilisp [menu-bar lisp macroexpand-1]
  '("Macroexpand 1" . macroexpand-1-lisp))

(defkey-ilisp [menu-bar lisp set-package]
  '("Set Lisp Package" . set-package-lisp))

(defkey-ilisp [menu-bar lisp set-buffer-package]
  '("Set Buffer Package" . set-buffer-package-lisp))
   
(defkey-ilisp [menu-bar lisp arglist]
  '("Arglist" . arglist-lisp))

(defkey-ilisp [menu-bar lisp documentation]
  '("Documentation" . documentation-lisp))

(defkey-ilisp [menu-bar lisp describe]
  '("Describe" . describe-lisp))

(defkey-ilisp [menu-bar lisp inspect]
  '("Inspect" . inspect-lisp))

(defkey-ilisp [menu-bar lisp eval-defun]
  '("Eval Defun" . eval-defun-and-go-lisp))

(defkey-ilisp [menu-bar lisp start-inferior-lisp]
    '("Start Lisp"
      "Starts an inferior lisp asking for a dialect name"
      . run-ilisp))


(defun ilisp-nop () nil)

;;; Make sure the menu items are properly marked.
;;; Checking for 'ilisp-buffer' is very crufty, but I think it is OK
;;; for the time being. The function 'ilisp-initialized' is not very
;;; good for this.

(put 'macroexpand-lisp 'menu-enable 'ilisp-buffer)
(put 'macroexpand-1-lisp 'menu-enable 'ilisp-buffer)
(put 'set-package-lisp 'menu-enable 'ilisp-buffer)
(put 'arglist-lisp 'menu-enable 'ilisp-buffer)
(put 'documentation-lisp 'menu-enable 'ilisp-buffer)
(put 'inspect-lisp 'menu-enable 'ilisp-buffer)
(put 'describe-lisp 'menu-enable 'ilisp-buffer)
(put 'eval-defun-and-go-lisp 'menu-enable 'ilisp-buffer)
(put 'run-ilisp 'menu-enable '(null ilisp-buffer))
(put 'reset-ilisp 'menu-enable 'ilisp-buffer)
(put 'repair-ilisp 'menu-enable 'ilisp-buffer)

(put 'comment-region 'menu-enable 'mark-active)


;;; ilisp-update-menu --
;;; Update the status of the menu "Lisp".

;; This variable should disappear!

(defvar ilisp-process-active-p nil
  "Kludge to keep track whether the Inf. Lisp is active or not.")

(defun ilisp-update-menu (status)
  (if (eq status 'exit)
      (progn
	;; (setq ilisp-process-active-p nil)
	(put 'macroexpand-lisp 'menu-enable 'ilisp-buffer)
	(put 'macroexpand-1-lisp 'menu-enable 'ilisp-buffer)
	(put 'set-package-lisp 'menu-enable 'ilisp-buffer)
	(put 'arglist-lisp 'menu-enable 'ilisp-buffer)
	(put 'documentation-lisp 'menu-enable 'ilisp-buffer)
	(put 'inspect-lisp 'menu-enable 'ilisp-buffer)
	(put 'describe-lisp 'menu-enable 'ilisp-buffer)
	(put 'eval-defun-and-go-lisp 'menu-enable 'ilisp-buffer)
	;; (put 'run-ilisp 'menu-enable '(and (null ilisp-buffer)
	;;                                ilisp-process-active-p)
	(put 'run-ilisp 'menu-enable (null ilisp-buffer))
	(put 'reset-ilisp 'menu-enable 'ilisp-buffer)
	(put 'repair-ilisp 'menu-enable 'ilisp-buffer)
	)
    ;; no-op otherwise
    ))

;;;(setplist 'lisp-command-menu nil)
;;;(def-menu 'lisp-command-menu
;;;    "Lisp"
;;;  "These ILISP commands are available on the menu:"
;;;  '(
;;;    ("Break        Interupt current lisp."  
;;;     (progn (switch-to-lisp t)
;;;	    (interrupt-subjob-ilisp)))
;;;    ("Doc          Menu of commands to get help on variables, etc."
;;;     documentation-lisp-command-menu)
;;;    ("Xpand        macroexpand-lisp."        macroexpand-lisp)
;;;    ("Eval         Eval the surrounding defun." eval-defun-lisp)
;;;    ("1E&G         Eval defun and goto Inferior LISP." eval-defun-and-go-lisp)
;;;    (";            Comment the region."   comment-region-lisp)
;;;    (")            find-unbalanced-lisp parens." find-unbalanced-lisp)
;;;    ("]            close-all-lisp parens that are open." close-all-lisp)
;;;    ("Trace        Traces the previous function symbol." trace-lisp)
;;;    )
;;;  )

;;;(setplist 'documentation-lisp-command-menu nil)
;;;(def-menu 'documentation-lisp-command-menu
;;;    "Lisp help"
;;;  "These commands are available for examining Lisp structures:"
;;;  '(
;;;    ("UDoc         Get user's documentation string." documentation-lisp)
;;;    ("Rglist       Get the arglist for function." arglist-lisp)
;;;    ("Insp         Inspect the current sexp." inspect-lisp)
;;;    ("1Insp        Prompts for something to inspect." (inspect-lisp -4))
;;;    ("Descr        Describe the current sexp." describe-lisp)
;;;    ("1Descr       Prompts for something to describe." (describe-lisp -4))
;;;    )
;;;  )

(provide 'ilisp-mnb)
