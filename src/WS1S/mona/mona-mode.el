;;; mona-mode.el -- Emacs MONA mode

;; MONA is Copyright (C) 1997-1998 BRICS. All rights reserved. 
;; 
;; Reproduction of all or part of this software is permitted for
;; educational or research use on condition that this copyright notice is
;; included in any copy. This software comes with no warranty of any
;; kind. In no event will BRICS be liable for any damages resulting from
;; use of this software.

;; Usage:
;; Insert the following into your .emacs file:
;;
;;   (load "<directory>/mona-mode")
;;   (global-font-lock-mode t)
;;
;; where <directory> is the name of the directory containing this file.
;; If font-lock is already enabled you don't need the second line.

(require 'font-lock)
(if (<= 20 emacs-major-version)
    (defun make-regexp (a b c) (regexp-opt a b))
  (require 'make-regexp))

(setq mona-mode-font-lock-keywords
      (list
       (list 
	(concat "\\<" 
		(make-regexp
		 '("all0" "all1" "all2" "ex0" "ex1" "ex2" "true" "false"
		   "let0" "let1""let2" "pred" "macro" "assert" "inter" "sub"
		   "union" "in" "notin" "var0" "var1" "var2" "linear" "tree"
		   "universe" "root" "guide" "const" "defaultwhere1"
		   "defaultwhere2" "empty" "m2l-str" "m2l-tree" "lastpos"
		   "where" "max" "min" "include" "export" "prefix" "execute"
		   "ws1s" "ws2s" "import") t t)
		"\\>")
	1 'font-lock-keyword-face nil)))

(defvar mona-mode-syntax-table nil
  "Syntax table in use in mona-mode buffers.")

(if mona-mode-syntax-table
    ()
  (setq mona-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?/ ". 14b" mona-mode-syntax-table)
  (modify-syntax-entry ?* ". 23b" mona-mode-syntax-table)
  (modify-syntax-entry ?# "<" mona-mode-syntax-table)
  (modify-syntax-entry ?\n ">" mona-mode-syntax-table)
  (modify-syntax-entry ?\f ">" mona-mode-syntax-table)
  (modify-syntax-entry ?' "w" mona-mode-syntax-table)
  (modify-syntax-entry ?@ "w" mona-mode-syntax-table)
  (modify-syntax-entry ?$ "w" mona-mode-syntax-table)
)

(setq font-lock-defaults-alist
      (cons (cons 'mona-mode 
                  '(mona-mode-font-lock-keywords
                    nil nil nil backward-paragraph
                    (font-lock-comment-start-regexp . "/[*]\\|#")))
            font-lock-defaults-alist))

(defun mona-mode ()
  "Major mode for editing MONA files"

  (interactive)

  (kill-all-local-variables)

  (setq mode-name "MONA")
  (setq major-mode 'mona-mode)
  (set-syntax-table mona-mode-syntax-table)
  (run-hooks 'mona-mode-hook))


(or (assoc "\\.mona$" auto-mode-alist)
    (setq auto-mode-alist (cons '("\\.mona$" . mona-mode)
				auto-mode-alist)))

(provide 'mona-mode)
