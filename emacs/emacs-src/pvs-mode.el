;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-mode.el -- 
;; Author          : Sam Owre
;; Created On      : Sun Apr 30 13:46:32 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Nov 21 18:14:50 1995
;; Update Count    : 4
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'pvs-macros))

;;; pvs-mode.el

(pushnew '("\\.pvs\\'" . pvs-mode) auto-mode-alist)
(pushnew '("\\.ppe\\'" . pvs-mode) auto-mode-alist)
(pushnew '("\\.tccs\\'" . pvs-mode) auto-mode-alist)
(pushnew '("pvs-strategies\\'" . lisp-mode) auto-mode-alist)
(pushnew ".prf" completion-ignored-extensions)

(defvar *pvs-menu-type* 'simple)

(defvar pvs-mode-map nil)
(if pvs-mode-map () 
    (setq pvs-mode-map (make-sparse-keymap))
    (define-key pvs-mode-map "\C-c\C-af" 'alltt-pvs-file)
    (define-key pvs-mode-map "\C-c\C-at" 'alltt-theory)
    (define-key pvs-mode-map "\C-c\C-ai" 'alltt-importchain)
    (define-key pvs-mode-map "\C-c\C-ap" 'alltt-proof)
    (define-key pvs-mode-map "\C-c\C-i"  'install-proof)
    (define-key pvs-mode-map "\C-cs"  'install-and-step-proof)
    (define-key pvs-mode-map "\C-cx"  'install-and-x-step-proof)
    (define-key pvs-mode-map "\C-c\C-lf" 'latex-pvs-file)
    (define-key pvs-mode-map "\C-c\C-lt" 'latex-theory)
    (define-key pvs-mode-map "\C-c\C-li" 'latex-importchain)
    (define-key pvs-mode-map "\C-c\C-lp" 'latex-proof)
    (define-key pvs-mode-map "\C-c\C-lv" 'latex-theory-view)
    (define-key pvs-mode-map "\C-c\C-lP" 'latex-proof-view)
    (define-key pvs-mode-map "\C-c\C-ls" 'latex-set-linelength)
    (define-key pvs-mode-map "\C-cp"     'prove)
    (define-key pvs-mode-map "\C-c\C-pp" 'prove)
    (define-key pvs-mode-map "\C-c\C-p\C-p" 'prove)
    (define-key pvs-mode-map "\C-c\C-px" 'x-prove)
    (define-key pvs-mode-map "\C-c\C-pX" 'x-step-proof)
    (define-key pvs-mode-map "\C-c\C-pf" 'prove-pvs-file)
    (define-key pvs-mode-map "\C-c\C-pc" 'prove-proofchain)
    (define-key pvs-mode-map "\C-c\C-pt" 'prove-theory)
    (define-key pvs-mode-map "\C-c\C-pi" 'prove-importchain)
    (define-key pvs-mode-map "\C-c\C-pn" 'prove-next-unproved-formula)
    (define-key pvs-mode-map "\C-c\C-ps" 'step-proof)
    (define-key pvs-mode-map "\C-c\C-pr" 'redo-proof)
    (define-key pvs-mode-map "\C-c\C-pu" 'prove-untried-theory)
    (define-key pvs-mode-map "\C-c\C-pU" 'prove-untried-pvs-file)
    (define-key pvs-mode-map "\C-c\C-qs" 'show-tccs)
    (define-key pvs-mode-map "\C-c\C-qe" 'prettyprint-expanded)    
    (define-key pvs-mode-map "\C-c\C-qf" 'prettyprint-pvs-file)    
    (define-key pvs-mode-map "\C-c\C-qt" 'prettyprint-theory)
    (define-key pvs-mode-map "\C-c\C-qr" 'prettyprint-region)
    (define-key pvs-mode-map "\C-c\C-qd" 'prettyprint-declaration)
    (define-key pvs-mode-map "\C-c\C-qi" 'prettyprint-theory-instance)
    (define-key pvs-mode-map "\C-c\C-sf" 'status-pvs-file)
    (define-key pvs-mode-map "\C-c\C-st" 'status-theory)
    (define-key pvs-mode-map "\C-c\C-si" 'status-importchain)
    (define-key pvs-mode-map "\C-c\C-sb" 'status-importbychain)
    (define-key pvs-mode-map "\C-c\C-sp" 'status-proof)
    (define-key pvs-mode-map "\C-c]"     'find-unbalanced-pvs)
    (define-key pvs-mode-map "\C-c\C-t"	 'typecheck)
    (define-key pvs-mode-map "\C-x\C-s"  'save-pvs-file)
    (define-key pvs-mode-map "\M-,"      'find-declaration)
    (define-key pvs-mode-map "\M-."      'show-declaration)
	(define-key pvs-mode-map [(control meta ?.)]  'goto-declaration)
    (define-key pvs-mode-map "\M-;"      'whereis-declaration-used)
	(define-key pvs-mode-map
	  [(control meta ?;)] 'whereis-identifier-used)
    (define-key pvs-mode-map "\M-:"      'list-declarations)
    (define-key pvs-mode-map [(control ?.)] 'show-expanded-form)
    (define-key pvs-mode-map "\M-{"      'backward-theory)
    (define-key pvs-mode-map "\M-}"      'forward-theory)
    (define-key pvs-mode-map "\e\034"    'prettyprint-region)
    (define-key pvs-mode-map "\e\C-q"    'prettyprint-declaration)
    (define-key pvs-mode-map "\C-c\C-c"  'pvs-interrupt-subjob)
    (if (memq pvs-emacs-system '(xemacs21 xemacs20 xemacs19))
	(define-key pvs-mode-map [(shift button2)] 'mouse-show-declaration)
	(if (memq pvs-emacs-system '(emacs20 emacs19))
	    (define-key pvs-mode-map [S-mouse-2] 'mouse-show-declaration))))

(defvar pvs-mode-syntax-table nil  "Syntax table used while in pvs mode.")
(if pvs-mode-syntax-table ()
    (let ((st (syntax-table)))
      (unwind-protect
	   (progn
	     (setq pvs-mode-syntax-table (make-syntax-table))
	     (set-syntax-table pvs-mode-syntax-table)
	     (modify-syntax-entry ?_ "w")
	     (modify-syntax-entry ?\? "w")
	     (modify-syntax-entry ?: ".")
	     (modify-syntax-entry ?% "<")
	     (modify-syntax-entry ?\f ">")
	     (modify-syntax-entry ?\n ">")
	     (modify-syntax-entry ?\r ">"))
	(set-syntax-table st))))

(defpvs pvs-mode environment ()
  "Major mode for PVS specification files.

The pvs-mode command puts the current buffer in PVS mode.  This command is
not normally needed; buffers with a .pvs extension and buffers created by
PVS are automatically put in the proper mode."
  (interactive)
  (kill-all-local-variables)
  (use-local-map pvs-mode-map)
  ;; fix up comment handling
  (make-local-variable 'comment-start)
  (setq comment-start "%")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "%+ *")
  (setq require-final-newline t)
  (setq major-mode 'pvs-mode)
  (setq mode-name "PVS")
  (setq mode-line-process 'ilisp-status)
  (set-syntax-table pvs-mode-syntax-table)
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'pvs-mode-hook))

;;; Each item is a (string . value) pair.

(defun pvs-menu (query items)
  (case *pvs-menu-type*
    (simple (pvs-simple-menu query items))
    (x-menu (pvs-x-menu query items))))

(defun pvs-simple-menu (query items)
  (save-excursion
    (let ((buf (get-buffer-create "*PVS-menu*")))
      (set-buffer buf)
      (erase-buffer)
      (dolist (item items)
	(insert (format "%s\t%s\n" (cdr item) (car item))))
      (goto-char (point-min))
      (pop-to-buffer buf)
      (optimize-window-height)
      (let ((v (completing-read query
				(mapcar '(lambda (x) (list (cdr x))) items)
				nil t)))
	(pvs-bury-output)
	v))))


(defun current-line-number ()
  "Return the current line number (in the buffer) of point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun delete-current-window ()
  "Delete current window; if only window, put into background"
  (interactive)
  (condition-case nil
      (delete-window)
    (error (previous-buffer))))
 
(defun remove-buffer (buf)
  (when (equal buf (process-buffer (ilisp-process)))
    (error "Attempting to remove *pvs* buffer"))
  (save-excursion
    (set-buffer buf)
    (delete-current-window)
    (kill-buffer buf))
  (other-window 1)
  )


;;; Makes the window as large (or small) as necessary for the number of
;;; lines in the buffer.  Will take up the entire window if necessary.

(defun optimize-window-height ()
  (ilisp-shrink-wrap-window (selected-window))
  (goto-char (point-min)))

(provide 'pvs-mode)
