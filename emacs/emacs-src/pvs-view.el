;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-view.el -- 
;; Author          : Sam Owre
;; Created On      : Sun Dec 24 16:14:03 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Mon Dec 25 17:00:20 1995
;; Update Count    : 1
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Key bindings for PVS view mode

(defvar pvs-view-mode-map nil)
(if pvs-view-mode-map
    nil
    (setq pvs-view-mode-map (make-keymap))
    (suppress-keymap pvs-view-mode-map t)
    (define-key pvs-view-mode-map "q"
      '(lambda () (interactive) (remove-buffer (current-buffer))))
    (define-key pvs-view-mode-map "\M-." 'show-declaration)
    (define-key pvs-view-mode-map "\M-," 'find-declaration)
    (define-key pvs-view-mode-map "\M-;" 'whereis-declaration-used)
    (define-key pvs-view-mode-map "\M-:" 'list-declarations)
    (define-key pvs-view-mode-map [(control ?.)] 'show-expanded-form)
    (define-key pvs-view-mode-map "<" 'beginning-of-buffer)
    (define-key pvs-view-mode-map ">" 'end-of-buffer)
    (define-key pvs-view-mode-map " " 'scroll-up)
    (define-key pvs-view-mode-map "n" 'scroll-up)
    (define-key pvs-view-mode-map "\177" 'scroll-down)
    (define-key pvs-view-mode-map "p" 'scroll-down)
    (define-key pvs-view-mode-map "h" 'describe-mode)
    (define-key pvs-view-mode-map "?" 'describe-mode)
    (define-key pvs-view-mode-map "l" 'pvs-maybe-print-buffer)
    (define-key pvs-view-mode-map "s" 'save-pvs-buffer)
    (define-key pvs-view-mode-map "\C-x\C-s" 'save-pvs-buffer)
    (define-key pvs-view-mode-map "\C-x\C-w" 'save-pvs-buffer)
    ;;(define-key pvs-view-mode-map "\C-z" (make-keymap))
    (define-key pvs-view-mode-map "\C-z1" 'pvs-bury-output)
    (define-key pvs-view-mode-map "\C-zv" 'ilisp-scroll-output)
    (define-key pvs-view-mode-map "\C-zg" 'ilisp-grow-output)
    )

 
(defun pvs-view-mode ()
  "Pvs View Mode provides commands for viewing information
but not editing it.  Letters do not insert themselves; instead
these commands are provided.

Key bindings are:
\\{pvs-view-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map pvs-view-mode-map)
  (setq major-mode 'pvs-view-mode)
  (setq mode-name "PVS View")
  (set-syntax-table pvs-mode-syntax-table)
  (run-hooks 'pvs-view-mode-hook))

(defun pvs-view-file (file-name)
  (find-file-other-window file-name)
  (pvs-view-mode))

(defun pvs-maybe-print-buffer ()
  "Prompts for whether to print the current buffer."
  (interactive)
  (when (yes-or-no-p (format "Print buffer %s? " (buffer-name)))
    (pvs-print-buffer)))
