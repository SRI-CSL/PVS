;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-menu.el -- Provides menu and font-lock support for GNU Emacs 19 and
;;                XEmacs
;; Author          : Sam Owre
;; Created On      : Sun Sep 25 14:18:08 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Nov 21 18:11:48 1995
;; Update Count    : 10
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'easymenu)

(defconst pvs-mode-menus
  '("PVS"
    ("Getting Help"
     ["help-pvs" help-pvs t]
     ["help-pvs-language" help-pvs-language t]
     ["help-pvs-prover" help-pvs-prover t]
     ["help-pvs-prover-command" help-pvs-prover-command t]
     ["help-pvs-prover-strategy" help-pvs-prover-strategy t]
     ["help-pvs-prover-emacs" help-pvs-prover-emacs t])
    ("Editing PVS Files"
     ["forward-theory" forward-theory (current-pvs-file t)]
     ["backward-theory" backward-theory (current-pvs-file t)]
     ["find-unbalanced-pvs" find-unbalanced-pvs (current-pvs-file t)]
     ["comment-region" comment-region (current-pvs-file t)])
    ("Parsing and Typechecking"
     ["parse" parse (current-pvs-file t)]
     ["typecheck" typecheck (current-pvs-file t)]
     ["typecheck-importchain" typecheck-importchain (current-pvs-file t)]
     ["typecheck-prove" typecheck-prove (current-pvs-file t)]
     ["typecheck-prove-importchain" typecheck-prove-importchain (current-pvs-file t)])
    ("Prover Invocation"
     ["prove" prove (pvs-valid-formula-buffer)]
     ["x-prove" x-prove (pvs-valid-formula-buffer)]
     ["step-proof" step-proof (pvs-valid-formula-buffer)]
     ["x-step-proof" x-step-proof (pvs-valid-formula-buffer)]
     ["redo-proof" redo-proof (pvs-valid-formula-buffer)]
     ["prove-theory" prove-theory (current-pvs-file t)]
     ["prove-pvs-file" prove-pvs-file (current-pvs-file t)]
     ["prove-importchain" prove-importchain (current-pvs-file t)]
     ["prove-proofchain" prove-proofchain (pvs-valid-formula-buffer)])
    ("Proof Editing"
     ["edit-proof" edit-proof (pvs-valid-formula-buffer)]
     ["install-proof" install-proof (or (pvs-valid-formula-buffer)
					(equal (buffer-name) "Proof"))]
     ["revert-proof" revert-proof (pvs-valid-formula-buffer)]
     ["remove-proof" remove-proof (pvs-valid-formula-buffer)]
     ["show-proof-file" show-proof-file (current-pvs-file t)]
     ["show-orphaned-proofs" show-orphaned-proofs t]
     ["show-proofs-theory" show-proofs-theory (current-pvs-file t)]
     ["show-proofs-pvs-file" show-proofs-pvs-file (current-pvs-file t)]
     ["show-proofs-importchain" show-proofs-importchain (current-pvs-file t)]
     ["install-pvs-proof-file" install-pvs-proof-file t]
     ["load-pvs-strategies" load-pvs-strategies t]
     ["set-print-depth" set-print-depth t]
     ["set-print-length" set-print-length t]
     ["set-rewrite-depth" set-rewrite-depth t]
     ["set-rewrite-length" set-rewrite-length t])
    ("Proof Information"
     ["show-current-proof" show-current-proof pvs-in-checker]
     ["explain-tcc" explain-tcc pvs-in-checker]
     ["show-last-proof" show-last-proof pvs-in-checker]
     ["ancestry" ancestry pvs-in-checker]
     ["siblings" siblings pvs-in-checker]
     ["show-hidden-formulas" show-hidden-formulas pvs-in-checker]
     ["show-auto-rewrites" show-auto-rewrites pvs-in-checker]
     ["show-expanded-sequent" show-expanded-sequent pvs-in-checker]
     ["show-skolem-constants" show-skolem-constants pvs-in-checker])
    ("Adding and Modifying Declarations"
     ["add-declaration" add-declaration (current-pvs-file t)]
     ["modify-declaration" modify-declaration (current-pvs-file t)])
    ("Prettyprint"
     ["prettyprint-theory" prettyprint-theory (current-pvs-file t)]
     ["prettyprint-pvs-file" prettyprint-pvs-file (current-pvs-file t)]
     ["prettyprint-declaration" prettyprint-declaration (current-pvs-file t)]
     ["prettyprint-region" prettyprint-region (current-pvs-file t)])
    ("Viewing TCCs"
     ["show-tccs" show-tccs (current-pvs-file t)]
     ["prettyprint-expanded" prettyprint-expanded (current-pvs-file t)])
    ("Files and Theories"
     ["find-pvs-file" find-pvs-file t]
     ["find-theory" find-theory t]
     ["view-prelude-file" view-prelude-file t]
     ["view-prelude-theory" view-prelude-theory t]
     ["view-library-file" view-library-file t]
     ["view-library-theory" view-library-theory t]
     ["new-pvs-file" new-pvs-file t]
     ["new-theory" new-theory t]
     ["import-pvs-file" import-pvs-file t]
     ["import-theory" import-theory t]
     ["delete-pvs-file" delete-pvs-file t]
     ["delete-theory" delete-theory t]
     ["save-pvs-file" save-pvs-file t]
     ["save-some-pvs-files" save-some-pvs-files t]
     ["smail-pvs-files" smail-pvs-files (current-pvs-file t)]
     ["rmail-pvs-files" rmail-pvs-files t]
     ["dump-pvs-files" dump-pvs-files (current-pvs-file t)]
     ["undump-pvs-files" undump-pvs-files t]
     ["save-pvs-buffer" save-pvs-buffer t])
    ("Printing"
     ["pvs-print-buffer" pvs-print-buffer t]
     ["pvs-print-region" pvs-print-region t]
     ["print-theory" print-theory (current-pvs-file t)]
     ["print-pvs-file" print-pvs-file (current-pvs-file t)]
     ["print-importchain" print-importchain (current-pvs-file t)]
     ["alltt-theory" alltt-theory (current-pvs-file t)]
     ["alltt-pvs-file" alltt-pvs-file (current-pvs-file t)]
     ["alltt-importchain" alltt-importchain (current-pvs-file t)]
     ["alltt-proof" alltt-proof t]
     ["latex-theory" latex-theory (current-pvs-file t)]
     ["latex-pvs-file" latex-pvs-file (current-pvs-file t)]
     ["latex-importchain" latex-importchain (current-pvs-file t)]
     ["latex-proof" latex-proof t]
     ["latex-theory-view" latex-theory-view (current-pvs-file t)]
     ["latex-set-linelength" latex-set-linelength t])
    ("Display Commands"
     ["x-theory-hierarchy" x-theory-hierarchy (current-pvs-file t)]
     ["x-show-proof" x-show-proof (pvs-valid-formula-buffer)]
     ["x-show-current-proof" x-show-current-proof pvs-in-checker]
     ["x-prover-commands" x-prover-commands t])
    ("Context"
     ["list-pvs-files" list-pvs-files t]
     ["list-theories" list-theories t]
     ["change-context" change-context t]
     ["save-context" save-context t]
     ["pvs-remove-bin-files" pvs-remove-bin-files t]
     ["pvs-dont-write-bin-files" pvs-dont-write-bin-files t]
     ["pvs-do-write-bin-files" pvs-do-write-bin-files t]
     ["context-path" context-path t])
    ("Browsing"
     ["show-declaration" show-declaration (current-pvs-file t)]
     ["find-declaration" find-declaration (current-pvs-file t)]
     ["whereis-declaration-used" whereis-declaration-used (current-pvs-file t)]
     ["list-declarations" list-declarations (current-pvs-file t)])
    ("Status"
     ["status-theory" status-theory (current-pvs-file t)]
     ["status-pvs-file" status-pvs-file (current-pvs-file t)]
     ["status-importchain" status-importchain (current-pvs-file t)]
     ["status-importbychain" status-importbychain (current-pvs-file t)]
     ["status-proof" status-proof (pvs-valid-formula-buffer)]
     ["status-proof-theory" status-proof-theory (current-pvs-file t)]
     ["status-proof-pvs-file" status-proof-pvs-file (current-pvs-file t)]
     ["status-proof-importchain" status-proof-importchain (current-pvs-file t)]
     ["status-proofchain" status-proofchain (pvs-valid-formula-buffer)]
     ["status-proofchain-theory" status-proofchain-theory (current-pvs-file t)]
     ["status-proofchain-pvs-file" status-proofchain-pvs-file (current-pvs-file t)]
     ["status-proofchain-importchain" status-proofchain-importchain (current-pvs-file t)])
    ("Environment"
     ["whereis-pvs" whereis-pvs t]
     ["pvs-version" pvs-version t]
     ["pvs-log" pvs-log t]
     ["pvs-mode" pvs-mode t]
     ["status-display" status-display t]
     ["pvs-status" pvs-status t]
     ["remove-popup-buffer" remove-popup-buffer t]
     ["pvs" pvs t]
     ["pvs-load-patches" pvs-load-patches t]
     ["pvs-interrupt-subjob" pvs-interrupt-subjob t]
     ["reset-pvs" reset-pvs t])
    ["report-pvs-bug" report-pvs-bug t]
    ("Exiting"
     ["suspend-pvs" suspend-pvs t]
     ["exit-pvs" exit-pvs t])))

;(when (memq pvs-emacs-system '(emacs20 emacs19))

;  (defvar easy-menu-fast-menus nil)

;  (defun easy-menu-create-keymaps-old (menu-name menu-items)
;    (let ((menu (make-sparse-keymap menu-name)))
;      ;; Process items in reverse order,
;      ;; since the define-key loop reverses them again.
;      (setq menu-items (reverse menu-items))
;      (while menu-items
;	(let* ((item (car menu-items))
;	       (callback (if (vectorp item) (aref item 1)))
;	       command enabler name)
;	  (cond ((stringp item)
;		 (setq command nil)
;		 (setq name (if (string-match "^-+$" item) "" item)))
;		((consp item)
;		 (setq command (easy-menu-create-keymaps (car item) (cdr item)))
;		 (setq name (car item)))
;		((vectorp item)
;		 (setq command
;		       (if easy-menu-fast-menus
;			   callback
;			   (make-symbol (format "menu-function-%d"
;					    easy-menu-item-count))))
;		 (setq easy-menu-item-count (1+ easy-menu-item-count))
;		 (put command 'menu-enable (aref item 2))
;		 (setq name (aref item 0))	       
;		 (if (keymapp callback)
;		     (setq name (concat name " ...")))
;		 (if (not easy-menu-fast-menus)
;		     (if (symbolp callback)
;			 (fset command callback)
;			 (fset command (list 'lambda () '(interactive) callback))))))
;	  (if (null command)
;	      ;; Handle inactive strings specially--allow any number
;	      ;; of identical ones.
;	      (setcdr menu (cons (list nil name) (cdr menu)))
;	      (if name 
;		  (define-key menu (vector (intern name)) (cons name command)))))
;	(setq menu-items (cdr menu-items)))
;      menu))

;  (let ((easy-menu-fast-menus t))
;    (easy-menu-define PVS global-map "PVS menus" pvs-mode-menus))
;  )

;(when (memq pvs-emacs-system '(xemacs19 xemacs20))
;  (add-submenu nil pvs-mode-menus "")
;  (add-hook 'pvs-mode-hook
;    '(lambda ()
;       (add-submenu nil pvs-mode-menus ""))))

(require 'font-lock)

(defvar pvs-keywords
  '("ALL" "AND" "ANDTHEN" "ARRAY" "ASSUMING" "ASSUMPTION" "AXIOM" "BEGIN"
    "BUT" "BY" "CASES" "CHALLENGE" "CLAIM" "CLOSURE" "COND" "CONJECTURE"
    "CONTAINING" "CONVERSION" "COROLLARY" "DATATYPE" "ELSE" "ELSIF" "END"
    "ENDASSUMING" "ENDCASES" "ENDCOND" "ENDIF" "ENDTABLE" "EXISTS"
    "EXPORTING" "FACT" "FALSE" "FORALL" "FORMULA" "FROM" "FUNCTION"
    "HAS_TYPE" "IF" "IFF" "IMPLIES" "IMPORTING" "IN" "INDUCTIVE"
    "JUDGEMENT" "LAMBDA" "LAW" "LEMMA" "LET" "LIBRARY" "MEASURE"
    "NONEMPTY_TYPE" "NOT" "O" "OBLIGATION" "OF" "OR" "ORELSE" "POSTULATE"
    "PROPOSITION" "RECURSIVE" "SUBLEMMA" "SUBTYPES" "SUBTYPE_OF" "TABLE"
    "THEN" "THEOREM" "THEORY" "TRUE" "TYPE" "TYPE+" "VAR" "WHEN" "WHERE"
    "WITH" "XOR"))

(defvar pvs-operators
  '("!" "!!" "#" "##" "\\$" "\\$\\$" "&" "&&"
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

(when (memq pvs-emacs-system '(emacs20 emacs19))
  (add-hook 'pvs-mode-hook
    '(lambda ()
       (make-local-variable 'font-lock-defaults)
       (setq font-lock-defaults '(pvs-font-lock-keywords nil t)))))

(defvar font-lock-pvs-record-parens-face 'font-lock-pvs-record-parens-face)
(make-face 'font-lock-pvs-record-parens-face)
;;(set-face-background 'font-lock-pvs-record-parens-face "lightcyan")

(defvar font-lock-pvs-set-brace-face 'font-lock-pvs-set-brace-face)
(make-face 'font-lock-pvs-set-brace-face)
;;(set-face-background 'font-lock-pvs-set-brace-face "Yellow")

(defvar font-lock-pvs-parens-face 'font-lock-pvs-parens-face)
(make-face 'font-lock-pvs-parens-face)
;;(set-face-foreground 'font-lock-pvs-parens-face "Magenta")

(defvar font-lock-pvs-table-face 'font-lock-pvs-table-face)
(make-face 'font-lock-pvs-table-face)
;;(set-face-background 'font-lock-pvs-table-face "Yellow")

(defvar font-lock-pvs-function-type-face 'font-lock-pvs-function-type-face)
(make-face 'font-lock-pvs-function-type-face)
;;(set-face-background 'font-lock-pvs-function-type-face "thistle1")

;(make-face 'font-lock-pvs-symbol-face)
;(set-face-background 'font-lock-pvs-symbol-face "Green")
;(set-face-font
; 'font-lock-pvs-symbol-face
; "-adobe-symbol-medium-r-normal--12-*-*-*-p-*-adobe-fontspecific")

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
    (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\)"
	  1 'font-lock-function-name-face)
    (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 1 'font-lock-keyword-face)
    (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-keyword-face)
    (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-keyword-face)
    (list "\\({\\||\\|}\\)" 1 'font-lock-keyword-face)
    (list "\\(\\[\\|\\->\\|\\]\\)" 1 'font-lock-keyword-face)
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
    (list "\\(<|\\||-\\||->\\||=\\||>\\|\\[\\]\\)"
	  1 'font-lock-function-name-face)
    (list "\\((#\\|#)\\|\\[#\\|#\\]\\)" 1 'font-lock-pvs-record-parens-face)
    (list "\\((:\\|:)\\|(|\\||)\\|(\\|)\\)" 1 'font-lock-pvs-parens-face)
    (list "\\(\\[|\\||\\]\\||\\[\\|\\]|\\|||\\)" 1 'font-lock-pvs-table-face)
    (list "\\({\\||\\|}\\)" 1 'font-lock-pvs-set-brace-face)
    (list "\\(\\[\\|\\->\\|\\]\\)" 1 'font-lock-pvs-function-type-face)
    (list (concat "\\("
		  (mapconcat 'identity pvs-operators "\\|")
		  "\\)")
	  1 'font-lock-function-name-face)))
  "Additional expressions to highlight in PVS mode.")

(when (and (memq pvs-emacs-system '(emacs20 emacs19))
	   (not (boundp 'font-lock-maximum-decoration)))
  (defvar font-lock-maximum-decoration nil))

(defconst pvs-font-lock-keywords
  (if (if (memq pvs-emacs-system '(emacs20 emacs19))
	  font-lock-maximum-decoration
	  font-lock-use-maximal-decoration)
      pvs-font-lock-keywords-2
      pvs-font-lock-keywords-1))
