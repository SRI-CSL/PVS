;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-mode.el -- 
;; Author          : Sam Owre
;; Created On      : Sun Apr 30 13:46:32 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 22:47:34 2004
;; Update Count    : 5
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(eval-when-compile (require 'pvs-macros))

;;; pvs-mode.el

(pushnew '("\\.pvs\\'" . pvs-mode) auto-mode-alist)
(pushnew '("\\.ppe\\'" . pvs-mode) auto-mode-alist)
(pushnew '("\\.tccs\\'" . pvs-mode) auto-mode-alist)
(pushnew '("pvs-strategies\\'" . lisp-mode) auto-mode-alist)
(pushnew ".prf" completion-ignored-extensions)

(defgroup pvs nil
  "PVS Interaction"
  :prefix "pvs-"
  :group 'applications)

(defcustom pvs-menu-type 'simple
  "Type of menu to popup"
  :type '(choice (const simple) (const x-menu))
  :group 'pvs)

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
    (if (featurep 'xemacs)
	(define-key pvs-mode-map [(shift button2)] 'mouse-show-declaration)
	(define-key pvs-mode-map [S-mouse-2] 'mouse-show-declaration)))

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
  (unless default-input-method
    (setq default-input-method "PVS-TeX"))
  (setq parse-sexp-ignore-comments t)
  (run-hooks 'pvs-mode-hook))

;;; Each item is a (string . value) pair.

(defun pvs-menu (query items)
  (case pvs-menu-type
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
    (error (switch-to-buffer nil))))
 
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

;;; Speedbar - patterned after Info-speedbar

(eval-when-compile (require 'speedbar))

(defvar pvs-speedbar-key-map nil
  "Keymap used when in the pvs display mode.")

(defun pvs-install-speedbar-variables ()
  "Install those variables used by speedbar for PVS Library support."
  
  ;;; It would be nice if there was a better (i.e., buffer-specific) way to do this
  ;;; speedbar-special-mode-expansion-list seems to have some possibilities,
  ;;; but not trivial to understand.
  (speedbar-disable-update)
  (if pvs-speedbar-key-map
      nil
      (setq pvs-speedbar-key-map (speedbar-make-specialized-keymap))

      ;; Basic tree features
      (define-key pvs-speedbar-key-map "e" 'speedbar-edit-line)
      (define-key pvs-speedbar-key-map "\C-m" 'speedbar-edit-line)
      (define-key pvs-speedbar-key-map "+" 'speedbar-expand-line)
      (define-key pvs-speedbar-key-map "-" 'speedbar-contract-line)
      )

  (speedbar-add-expansion-list '("pvs" pvs-speedbar-menu-items
				 pvs-speedbar-key-map
				 pvs-speedbar-library-buttons)))

(defvar pvs-speedbar-menu-items
  '(["Browse Library" speedbar-edit-line t]
    ["Expand Library" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Library" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; Make sure our special speedbar major mode is loaded
(if (featurep 'speedbar)
    (pvs-install-speedbar-variables)
    (add-hook 'speedbar-load-hook 'pvs-install-speedbar-variables))

;;; pvs library display method
;;;###autoload
(defun pvs-speedbar-browser ()
  "Initialize speedbar to display a pvs library browser.
This will add a speedbar major display mode."
  (interactive)
  (require 'speedbar)
  ;; Make sure that speedbar is active
  (speedbar-frame-mode 1)
  ;; Now, throw us into pvs mode on speedbar.
  (speedbar-change-initial-expansion-list "pvs")
  )

(defun pvs-speedbar-library-buttons (directory depth &optional node)
  "Display a pvs directory hierarchy in speedbar.
DIRECTORY is the current directory in the attached frame.
DEPTH is the current indentation depth.
NODE is an optional argument that is used to represent the
specific node to expand."
  (if (and (not node)
	   (save-excursion (goto-char (point-min))
			   (let ((case-fold-search t))
			     (looking-at "PVS Libraries:"))))
      ;; Update our "current libraries" maybe?
      nil
    ;; We cannot use the generic list code, that depends on all leaves
    ;; being known at creation time.
    (if (not node)
	(speedbar-with-writable (insert "PVS Libraries:\n")))
    (let ((libraries (pvs-speedbar-fetch-library-entries)))
      (speedbar-select-attached-frame)
      (select-frame (speedbar-current-frame))
      (speedbar-with-writable
	(dolist (dirpair libraries)
	  (speedbar-make-tag-line 'angle ?+ 'pvs-speedbar-expand-library
				  dirpair
				  (car dirpair)
				  'pvs-speedbar-goto-library
				  dirpair
				  'pvs-function-type-face depth))
	t))))

(defun pvs-speedbar-goto-library (text node indent)
  "When user clicks on TEXT, go to a PVS Library.
The INDENT level is ignored."
  (speedbar-select-attached-frame)
  (let* ((buff (or (get-buffer "*info*")
		   (progn (info) (get-buffer "*info*"))))
	 (bwin (get-buffer-window buff 0)))
    (if bwin
	(progn
	  (select-window bwin)
	  (raise-frame (window-frame bwin)))
      (if speedbar-power-click
	  (let ((pop-up-frames t)) (select-window (display-buffer buff)))
	(speedbar-select-attached-frame)
	(switch-to-buffer buff)))
    (if (not (string-match "^(\\([^)]+\\))\\([^.]+\\)$" node))
	(error "Invalid node %s" node)
      (pvs-find-node (match-string 1 node) (match-string 2 node))
      ;; If we do a find-node, and we were in info mode, restore
      ;; the old default method.  Once we are in info mode, it makes
      ;; sense to return to whatever method the user was using before.
      (if (string= speedbar-initial-expansion-list-name "pvs")
	  (speedbar-change-initial-expansion-list
	   speedbar-previously-used-expansion-list-name)))))

;;; E.g., text: "[+]",
;;;       token: ("while" . "/homes/owre/pvs-validation/libraries/LaRC/lib")
;;;       indent 1
(defun pvs-speedbar-expand-library (text token indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
TOKEN is data related to this node (LIBNAME . DIRECTORY).
INDENT is the current indentation depth."
  (cond ((string-match "+" text)	;we have to expand this library
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	      (save-excursion
		(end-of-line) (forward-char 1)
		(pvs-speedbar-library-files token (1+ indent))))
	     (speedbar-change-expand-button-char ?-)
	   (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun pvs-speedbar-library-files (token indent)
  (let ((filesinfo (pvs-library-subdir-files token)))
    (speedbar-select-attached-frame)
    (select-frame (speedbar-current-frame))
    (speedbar-with-writable
      (dolist (fileinfo filesinfo)
	(speedbar-make-tag-line 'bracket ?+ 'pvs-speedbar-expand-declarations
				fileinfo
				(car fileinfo)
				'pvs-speedbar-goto-file
				fileinfo
				'pvs-function-type-face indent))
      t)))

(defun pvs-library-subdir-files (dirname-path)
  (let ((dir (concat (cdr dirname-path) "/" (car dirname-path))))
    (assert (file-directory-p dir))
    (mapcar '(lambda (file) (cons file dir))
      (directory-files dir nil ".*\.pvs$"))))

(defun pvs-speedbar-goto-file (text fileinfo indent)
  (let ((fname (concat (cdr fileinfo) "/" (car fileinfo))))
    (if (not (file-exists-p fname))
	(error "%s does not exist." fname)
	(find-file-other-window fname)
	(unless buffer-read-only (toggle-read-only))
	(pvs-mode))))

(defun pvs-speedbar-expand-declarations (text token indent)
  (cond ((string-match "+" text)	;we have to expand this library
	 (speedbar-change-expand-button-char ?-)
	 (if (speedbar-with-writable
	       (save-excursion
		 (end-of-line) (forward-char 1)
		 (pvs-speedbar-declarations token (1+ indent))))
	     (speedbar-change-expand-button-char ?-)
	     (speedbar-change-expand-button-char ??)))
	((string-match "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun pvs-speedbar-declarations (token indent)
  (let ((declsinfo (pvs-library-file-declarations token)))
    (speedbar-select-attached-frame)
    (select-frame (speedbar-current-frame))
    (speedbar-with-writable
      (dolist (declinfo declsinfo)
	(speedbar-make-tag-line 'statictag ?? nil
				declinfo
				(car declinfo)
				'pvs-speedbar-goto-file
				declinfo
				'pvs-function-type-face indent))
      t)))

(defun pvs-speedbar-fetch-library-entries ()
  "Fetch the library entries."
  (reverse (pvs-library-path-subdirs pvs-library-path)))

;;; pvs mode node listing
;; This is called by `speedbar-add-localized-speedbar-support'
(defun pvs-speedbar-buttons (buffer)
  "Create a speedbar display to help navigation in an pvs file.
BUFFER is the buffer speedbar is requesting buttons for."
  (if (save-excursion (goto-char (point-min))
		      (let ((case-fold-search t))
			(not (looking-at "PVS Libraries:"))))
      (erase-buffer))
  (pvs-speedbar-library-buttons nil 0))

;; (dolist (mess '("^First node in file$"
;; 		"^No `.*' in index$"
;; 		"^No cross-reference named"
;; 		"^No cross.references in this node$"
;; 		"^No current pvs node$"
;; 		"^No menu in this node$"
;; 		"^No more items in menu$"
;; 		"^No more nodes$"
;; 		"^No pointer \\(?:forward\\|backward\\) from this node$"
;; 		"^No previous `i' command$"
;; 		"^No previous items in menu$"
;; 		"^No previous nodes$"
;; 		"^No such item in menu$"
;; 		"^No such node or anchor"
;; 		"^Node has no"
;; 		"^Point neither on reference nor in menu item description$"
;; 		"^This is the \\(?:first\\|last\\) pvs node you looked at$"
;; 		search-failed))
;;   (add-to-list 'debug-ignored-errors mess))

(register-input-method
 "PVS-TeX" "UTF-8" 'quail-use-package
 "⊢" "LaTeX-like input method for PVS."
 "pvs-ltx")

;;; Various faces used by PVS

;;; facep works differently in XEmacs - always returns nil for a symbol
;;; find-face is used there instead, but red and blue faces are already
;;; defined anyway.
(unless (featurep 'xemacs)
  (unless (facep 'red)
    (make-face 'red)
    (set-face-foreground 'red "red"))
  (unless (facep 'blue)
    (make-face 'blue)
    (set-face-foreground 'blue "blue")))

;;(set-face-background 'font-lock-pvs-record-parens-face "lightcyan")
(defface font-lock-pvs-record-parens-face
    `()
  "Face for PVS record parens."
  :group 'pvs)

(defface font-lock-pvs-checkpoint-face
    '((((class color) (background light))
       (:background "red"))
      (((class color) (background dark))
       (:background "red")))
  "Face for proof checkpoint indicators"
  :group 'pvs)

;;(set-face-background 'font-lock-pvs-set-brace-face "Yellow")
(defface font-lock-pvs-set-brace-face
    `()
  "Face for PVS set braces."
  :group 'pvs)

;;(set-face-foreground 'font-lock-pvs-parens-face "Magenta")
(defface font-lock-pvs-parens-face
    `()
  "Face for PVS parens."
  :group 'pvs)

;;(set-face-background 'font-lock-pvs-table-face "Yellow")
(defface font-lock-pvs-table-face
    `()
  "Face for PVS tables."
  :group 'pvs)

;;(set-face-background 'font-lock-pvs-function-type-face "thistle1")
(defface font-lock-pvs-function-type-face
    `()
  "Face for PVS tables."
  :group 'pvs)

(defface font-lock-pvs-symbol-face
    `()
  "Face for PVS symbols."
  :group 'pvs)

(defface proofstate-commentary-face
    `()
  "Face for proofstate commentaries."
  :group 'pvs)

(defface proofstate-action-face
    `()
  "Face for proofstate actions."
  :group 'pvs)

(defface proofstate-yields-face
    `()
  "Face for proofstate yields messages."
  :group 'pvs)

(defface proofstate-label-face
    `()
  "Face for proofstate labels."
  :group 'pvs)

(defface proofstate-formula-face
    `()
  "Face for proofstate formulas."
  :group 'pvs)

(defface proofstate-formula-changed-label-face
    `((((class color) (background light))
       (:foreground "red"))
      (((class color) (background dark))
       (:foreground "red")))
  "Face for proofstate formula changed labels."
  :group 'pvs)

(defface proofstate-formula-unchanged-label-face
    `()
  "Face for proofstate formula unchanged labels."
  :group 'pvs)

;;; PVS Font Lock support

(require 'font-lock)

(defvar pvs-keywords
  '("AND" "ANDTHEN" "ARRAY" "AS" "ASSUMING" "ASSUMPTION" "AUTO_REWRITE"
    "AUTO_REWRITE+" "AUTO_REWRITE-" "AXIOM" "BEGIN" "BUT" "BY" "CASES"
    "CHALLENGE" "CLAIM" "CLOSURE" "CODATATYPE" "COINDUCTIVE" "COND"
    "CONJECTURE" "CONTAINING" "CONVERSION" "CONVERSION+" "CONVERSION-"
    "COROLLARY" "DATATYPE" "ELSE" "ELSIF" "END" "ENDASSUMING" "ENDCASES"
    "ENDCOND" "ENDIF" "ENDTABLE" "EXISTS" "EXPORTING" "FACT" "FALSE"
    "FORALL" "FORMULA" "FROM" "FUNCTION" "HAS_TYPE" "IF" "IFF" "IMPLIES"
    "IMPORTING" "IN" "INDUCTIVE" "JUDGEMENT" "LAMBDA" "LAW" "LEMMA" "LET"
    "LIBRARY" "MACRO" "MEASURE" "NONEMPTY_TYPE" "NOT" "O" "OBLIGATION" "OF"
    "OR" "ORELSE" "POSTULATE" "PROPOSITION" "RECURSIVE" "SUBLEMMA" "SUBTYPES"
    "SUBTYPE_OF" "TABLE" "THEN" "THEOREM" "THEORY" "TRUE" "TYPE" "TYPE+"
    "VAR" "WHEN" "WHERE" "WITH" "XOR"))

(defvar pvs-operators
  '(;;"!" "!!"
    "#" "##" "\\$" "\\$\\$" "&" "&&"
    "\\*" "\\*\\*" "\\+" "\\+\\+" "-" "/"
    "//" "/=" "/\\\\" "::" ":=" "<" "<-" "<<" "<<=" "<="
    "<=>" "<>" "<|" "=" "==" "==>" "=>" ">" ">=" ">>" ">>=" "@" "@@"
    "\\[\\]" "\\\\/" "\\^" "\\^\\^" "|-" "|->" "|=" "|>" "~"))

(defun pvs-keyword-match (keyword)
  (let ((regexp "")
	(index 0)
	(len (length keyword)))
    (while (< index len)
      (let ((c (aref keyword index)))
	(setq regexp
	      (concat regexp (format "[%c%c]" (downcase c) (upcase c))))
	(setq index (+ index 1))))
    (format "\\b%s\\b" regexp)))

(unless (featurep 'xemacs)
  (add-hook 'pvs-mode-hook
    '(lambda ()
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-defaults '(pvs-font-lock-keywords nil t))))
  (add-hook 'pvs-view-mode-hook
    '(lambda ()
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-defaults '(pvs-font-lock-keywords nil t)))))


(defun pvs-minimal-decoration ()
  (interactive)
  (setq pvs-font-lock-keywords pvs-font-lock-keywords-1))

(defun pvs-maximal-decoration ()
  (interactive)
  (setq pvs-font-lock-keywords pvs-font-lock-keywords-2))

(defconst pvs-font-lock-keywords-1
  (purecopy
   (list
    (mapconcat 'pvs-keyword-match pvs-keywords "\\|")
    ;; These have to come first or they will match too soon.
    (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
	  1 'font-lock-function-name-face)
    (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 0 'font-lock-keyword-face)
    (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-keyword-face)
    (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-keyword-face)
    (list "\\({\\||\\|}\\)" 1 'font-lock-keyword-face)
    (list "\\(\\[\\|->\\|\\]\\)" 1 'font-lock-keyword-face)
    (list (concat "\\("
		  (mapconcat 'identity pvs-operators "\\|")
		  "\\)")
	  1 'font-lock-function-name-face)))
  "Additional expressions to highlight in PVS mode.")

(defconst pvs-font-lock-keywords-2
  (purecopy
   (list
    (mapconcat 'pvs-keyword-match pvs-keywords "\\|")
    ;; These have to come first or they will match too soon.
    (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
	  1 'font-lock-function-name-face)
    (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 0 'font-lock-pvs-record-parens-face)
    (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-pvs-parens-face)
    (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-pvs-table-face)
    (list "\\({\\||\\|}\\)" 1 'font-lock-pvs-set-brace-face)
    (list "\\(\\[\\|->\\|\\]\\)" 1 'font-lock-pvs-function-type-face)
    (list (concat "\\("
		  (mapconcat 'identity pvs-operators "\\|")
		  "\\)")
	  1 'font-lock-function-name-face)))
  "Additional expressions to highlight in PVS mode.")

(defconst pvs-sequent-font-lock-keywords-1
  (purecopy
   (list
    (mapconcat 'pvs-keyword-match pvs-keywords "\\|")
    '("\\({[^}]*}\\)" 1 font-lock-warning-face)
    '("\\(\\[[^]]*\\]\\)" 1 font-lock-comment-face)
    ;; These have to come first or they will match others too soon.
    '("\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
      1 font-lock-function-name-face)
    '("\\((#\\|#)\\|\\[#\\|#\\]\\)" 0 font-lock-keyword-face)
    '("\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 font-lock-keyword-face)
    '("\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 font-lock-keyword-face)
    '("\\({\\||\\|}\\)" 1 font-lock-keyword-face)
    '("\\(\\[\\|->\\|\\]\\)" 1 font-lock-keyword-face)
    (list (concat "\\("
		  (mapconcat 'identity pvs-operators "\\|")
		  "\\)")
	  1 'font-lock-function-name-face)))
  "Additional expressions to highlight in PVS mode.")

(defconst pvs-sequent-font-lock-keywords-2
  (purecopy
   (list
    (mapconcat 'pvs-keyword-match pvs-keywords "\\|")
    ;; These have to come first or they will match too soon.
    (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
	  1 'font-lock-function-name-face)
    (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 0 'font-lock-pvs-record-parens-face)
    (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-pvs-parens-face)
    (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-pvs-table-face)
    (list "\\({\\||\\|}\\)" 1 'font-lock-pvs-set-brace-face)
    (list "\\(\\[\\|->\\|\\]\\)" 1 'font-lock-pvs-function-type-face)
    (list (concat "\\("
		  (mapconcat 'identity pvs-operators "\\|")
		  "\\)")
	  1 'font-lock-function-name-face)))
  "Additional expressions to highlight in PVS mode.")

(defvar pvs-sequent-font-lock-keywords pvs-sequent-font-lock-keywords-1)

(defconst pvs-lisp-font-lock-keywords-1
  (append
   `((("^  |-------$") 0 info-title-1))
   lisp-font-lock-keywords-1))

(defconst pvs-lisp-font-lock-keywords-2
  (append pvs-lisp-font-lock-keywords-1
	  (list (regexp-opt '("^  |-------$")))))

(defvar pvs-lisp-font-lock-keywords pvs-lisp-font-lock-keywords-1
  "Default expressions to highlight in pvs-lisp-modes (*pvs* buffer)")

(unless (or (featurep 'xemacs)
	    (boundp 'font-lock-maximum-decoration))
  (defvar font-lock-maximum-decoration nil))

(defconst pvs-font-lock-keywords
  (if font-lock-maximum-decoration
      pvs-font-lock-keywords-2
      pvs-font-lock-keywords-1))

;; (defconst pvs-lisp-font-lock-keywords-1
;;   (purecopy
;;    (list
;;     ("\\(Rule\\?\\|pvs([0-9]+):\\)" (0 font-lock-warning-face)
;;      (cons "\{-?[0-9]+\}\\|\[-?[0-9]+\]" 'font-lock-builtin-face)
;;     (mapconcat 'pvs-keyword-match pvs-keywords "\\|")
;;     ;; These have to come first or they will match too soon.
;;     (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\|/\\\\\\)"
;; 	  1 'font-lock-function-name-face)
;;     (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 0 'font-lock-pvs-record-parens-face)
;;     (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-pvs-parens-face)
;;     (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-pvs-table-face)
;;     (list "\\({\\||\\|}\\)" 1 'font-lock-pvs-set-brace-face)
;;     (list "\\(\\[\\|->\\|\\]\\)" 1 'font-lock-pvs-function-type-face)
;;     (list (concat "\\("
;; 		  (mapconcat 'identity pvs-operators "\\|")
;; 		  "\\)")
;; 	  1 'font-lock-function-name-face))))

;; (defconst pvs-lisp-font-lock-keywords-1
;;   (purecopy
;;    (list
;;     (cons 'pvs-lisp-font-lock-matcher 'font-lock-warning-face))))

(provide 'pvs-mode)
