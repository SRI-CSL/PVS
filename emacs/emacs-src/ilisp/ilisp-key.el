;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-key.el --

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
;;; ILISP keybinding definitions.
;;;


;;; ilisp-where-is --
;;; New version provided by yusuf@SPD-13.ils.nwu.edu (Yusuf Pisan)
;;; Note: this used to be in 'ilisp-cpat'. Its definition did not make
;;;       much sense. Yusuf noted this and I decided to move it in
;;;       this file (where I think is more approriate).
;;;       11/24/94: Marco Antoniotti

(defun ilisp-where-is (command)
  (let ((cmd (where-is-internal command nil t)))
    (if cmd (key-description cmd))))


;;;
;;;%Bindings
(defun ilisp-defkey (keymap key command)
  "Define KEYMAP ilisp-prefix+KEY as command."
  (let ((prefix-map (lookup-key keymap ilisp-prefix)))
    (if (not (keymapp prefix-map))
	(setq prefix-map
	      (define-key keymap ilisp-prefix (make-sparse-keymap))))
    (define-key prefix-map key command)))

(defun defkey-ilisp (key command)
  "Define KEY as COMMAND in ilisp-mode-map and lisp-mode-map"
  (if (not ilisp-mode-map) (ilisp-bindings))
  (define-key ilisp-mode-map key command)
  (define-key lisp-mode-map key command))

;;;
(defun lisp-bindings (keymap &optional inferior-p)
  "Set up the bindings for interacting with an inferior LISP in
KEYMAP."
  (if inferior-p
      (progn (define-key keymap "\C-m" 'return-ilisp)
	     (define-key keymap "\C-a" 'bol-ilisp)
	     (define-key keymap "\C-c\C-c" 'interrupt-subjob-ilisp)
	     (define-key keymap "\C-d" 'delete-char-or-pop-ilisp)
	     (ilisp-defkey keymap "#" 'raw-keys-ilisp))
      (ilisp-defkey keymap "\C-c" 'compile-defun-and-go-lisp)
      (define-key keymap "\C-m" 'newline-and-indent-lisp))

  (define-key   keymap "]"        'close-all-lisp)
  (define-key   keymap "\M-q"     'reindent-lisp)
  (define-key   keymap "\C-]"     'close-and-send-lisp)
  (define-key   keymap "\t"       'indent-line-ilisp)
  (define-key   keymap "\n"       'newline-and-indent-lisp)
  (define-key   keymap "\M-\C-q"  'indent-sexp-ilisp)
  (ilisp-defkey keymap ";"        'comment-region-lisp)
  (ilisp-defkey keymap ")"        'find-unbalanced-lisp)
  (define-key   keymap "\M-\C-a"  'beginning-of-defun-lisp)
  (define-key   keymap "\M-\C-e"  'end-of-defun-lisp)
  (define-key   keymap "\C-\M-r"  'reposition-window-lisp)
  (ilisp-defkey keymap "i"        'describe-lisp)
  (ilisp-defkey keymap "I"        'inspect-lisp)
  (ilisp-defkey keymap "a"        'arglist-lisp)
  (ilisp-defkey keymap "d"        'documentation-lisp)
  (ilisp-defkey keymap "m"        'macroexpand-1-lisp)
  (ilisp-defkey keymap "M"        'macroexpand-lisp)
  (define-key   keymap "\M-,"     'next-definition-lisp)
  (define-key   keymap "\M-."     'edit-definitions-lisp)
  (define-key   keymap "\M-?"     'search-lisp)
  (define-key   keymap "\M-\""    'replace-lisp)
  (ilisp-defkey keymap "^"        'edit-callers-lisp)
  (define-key   keymap "\M-`"     'next-caller-lisp)
  (define-key   keymap "\M-\t"    'complete-lisp)
  (define-key   keymap "\M-\C-m"  'complete)
  (ilisp-defkey keymap "r"        'eval-region-lisp)
  (define-key   keymap "\M-\C-x"  'eval-defun-lisp) ; Gnu convention
  (ilisp-defkey keymap "e"        'eval-defun-lisp)
  (ilisp-defkey keymap "n"        'eval-next-sexp-lisp)
  (ilisp-defkey keymap "p"        'package-lisp)
  (ilisp-defkey keymap "P"        'set-package-lisp)
  (ilisp-defkey keymap "w"        'compile-region-lisp)
  (ilisp-defkey keymap "c"        'compile-defun-lisp)
  (ilisp-defkey keymap "\C-r"     'eval-region-and-go-lisp)
  (ilisp-defkey keymap "\C-e"     'eval-defun-and-go-lisp)
  (ilisp-defkey keymap "\C-n"     'eval-next-sexp-and-go-lisp)
  (ilisp-defkey keymap "\C-w"     'compile-region-and-go-lisp)
  (ilisp-defkey keymap "t"        'trace-defun-lisp)
  (ilisp-defkey keymap "!"        'default-directory-lisp)
  (ilisp-defkey keymap " "        'mark-change-lisp)
  (let ((ilisp-prefix (concat ilisp-prefix "*")))
    (ilisp-defkey keymap "l"      'list-changes-lisp)
    (ilisp-defkey keymap "e"      'eval-changes-lisp)
    (ilisp-defkey keymap "c"      'compile-changes-lisp)
    (ilisp-defkey keymap "0"      'clear-changes-lisp))
  (ilisp-defkey keymap "b"        'switch-to-lisp)
  (ilisp-defkey keymap "y"        'call-defun-lisp)
  (ilisp-defkey keymap "z"        'reset-ilisp)
  (ilisp-defkey keymap "g"        'abort-commands-lisp)
  (ilisp-defkey keymap "s"        'status-lisp)
  (ilisp-defkey keymap "S"        'select-ilisp)
  (define-key   keymap "\C-x\C-f" 'find-file-lisp)
  (ilisp-defkey keymap "l"        'load-file-lisp)
  (ilisp-defkey keymap "k"        'compile-file-lisp)
  (ilisp-defkey keymap "A"        'fi:clman-apropos)
  (ilisp-defkey keymap "D"        'fi:clman))



;;
(defun ilisp-lispm-bindings ()
  "Setup additional Lisp Machine-like bindings for some ilisp commands"
  (interactive)
  ;; Note: Changed the 'ilisp-emacs-version-id' to
  ;;       '+ilisp-emacs-version-id+' and the 'gnu-*' to 'fsf-*'.
  ;;       25/11/94 Marco Antoniotti
  (cond ((memq +ilisp-emacs-version-id+ '(fsf-19 fsf-20))
	 (defkey-ilisp (read "[?\\S-\\C-a]") 'arglist-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-c]") 'compile-defun-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-d]") 'documentation-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-e]") 'eval-defun-lisp)
	 (defkey-ilisp (read "[?\\S-\\C-m]") 'macroexpand-1-lisp)
	 (defkey-ilisp (read "[?\\M-M]") 'macroexpand-lisp))
	(t
	 (defkey-ilisp '(control A) 'arglist-lisp)
	 (defkey-ilisp '(control C) 'compile-defun-lisp)
	 (defkey-ilisp '(control D) 'documentation-lisp)
	 (defkey-ilisp '(control E) 'eval-defun-lisp)
	 (defkey-ilisp '(control M) 'macroexpand-1-lisp)
	 (defkey-ilisp '(meta M) 'macroexpand-lisp))))

;; Unfortunately, the read kludges are needed for this function to work
;; for GNU emacs 19 when it was compiled by Lucid.




;;;
(defun ilisp-bindings ()
  "Set up the key bindings for LISP and ILISP buffers."
  (if (fboundp 'set-keymap-parent) 
      (progn 
	(setq ilisp-mode-map (make-sparse-keymap))
	(set-keymap-parent ilisp-mode-map comint-mode-map))
    (setq ilisp-mode-map (copy-keymap comint-mode-map)))

  ;; Remove stop and quit subjob from comint
  (define-key ilisp-mode-map "\C-c\C-z" nil)
  (define-key ilisp-mode-map "\C-c\C-\\" nil)
  (if (fboundp 'lisp-mode-commands)
      (lisp-mode-commands ilisp-mode-map))
  (lisp-bindings ilisp-mode-map t)
  (if (boundp 'lisp-mode-map) 
      (lisp-bindings lisp-mode-map))
  (if (boundp 'scheme-mode-map) 
      (lisp-bindings scheme-mode-map))
  (ilisp-defkey emacs-lisp-mode-map ";" 'comment-region-lisp)

  (ilisp-defkey global-map "\C-t" 'trace-defun-lisp-break)
  (ilisp-defkey global-map "b" 'switch-to-lisp)

  ;; Globally defined output-control commands.
  (ilisp-defkey global-map "1" 'ilisp-bury-output)
  (ilisp-defkey global-map "v" 'ilisp-scroll-output)
  (ilisp-defkey global-map "G" 'ilisp-grow-output)

  (if (not (boundp 'fi:clman-mode-map))
      (setq fi:clman-mode-map (make-sparse-keymap)))
  (ilisp-defkey fi:clman-mode-map "D" 'fi:clman)
  (ilisp-defkey fi:clman-mode-map "A" 'fi:clman-apropos))
