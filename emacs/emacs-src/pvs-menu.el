;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-menu.el -- Provides menu and font-lock support for GNU Emacs 19 and
;;                XEmacs
;; Author          : Sam Owre
;; Created On      : Sun Sep 25 14:18:08 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Nov 21 18:11:48 1995
;; Update Count    : 10
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013, SRI International.  All Rights Reserved.

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

(require 'easymenu)

(defconst pvs-mode-menus
  '("PVS"
    ("Getting Help"
     ["help-pvs" help-pvs t]
     ["help-pvs-language" help-pvs-language t]
     ["help-pvs-bnf" help-pvs-bnf t]
     ["help-pvs-prover" help-pvs-prover t]
     ["help-pvs-prover-command" help-pvs-prover-command t]
     ["help-pvs-prover-strategy" help-pvs-prover-strategy t]
     ["help-pvs-prover-emacs" help-pvs-prover-emacs t]
     ["pvs-release-notes" pvs-release-notes t])
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
     ["prove-theories" prove-theories (current-pvs-file t)]
     ["prove-pvs-file" prove-pvs-file (current-pvs-file t)]
     ["prove-importchain" prove-importchain (current-pvs-file t)]
     ["prove-proofchain" prove-proofchain (pvs-valid-formula-buffer)])
    ("Proof Editing"
     ["edit-proof" edit-proof (pvs-valid-formula-buffer)]
     ["install-proof" install-proof (or (pvs-valid-formula-buffer)
					(equal (buffer-name) "Proof"))]
     ["display-proofs-formula" display-proofs-formula
      (or (pvs-valid-formula-buffer) (equal (buffer-name) "Proof"))]
     ["display-proofs-theory" display-proofs-theory (current-pvs-file t)]
     ["display-proofs-pvs-file" display-proofs-pvs-file (current-pvs-file t)]
     ["revert-proof" revert-proof (pvs-valid-formula-buffer)]
     ["remove-proof" remove-proof (pvs-valid-formula-buffer)]
     ["show-proof-file" show-proof-file (current-pvs-file t)]
     ["show-orphaned-proofs" show-orphaned-proofs t]
     ["show-proofs-theory" show-proofs-theory (current-pvs-file t)]
     ["show-proofs-pvs-file" show-proofs-pvs-file (current-pvs-file t)]
     ["show-proofs-importchain" show-proofs-importchain (current-pvs-file t)]
     ["install-pvs-proof-file" install-pvs-proof-file t]
     ["load-pvs-strategies" load-pvs-strategies t]
     ["toggle-proof-prettyprinting" toggle-proof-prettyprinting t]
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
     ["show-skolem-constants" show-skolem-constants pvs-in-checker]
     ["pvs-set-proof-parens" pvs-set-proof-parens t]
     ["pvs-set-proof-prompt-behavior" pvs-set-proof-prompt-behavior t]
     ["pvs-set-proof-default-description" pvs-set-proof-default-description t])
    ("Adding and Modifying Declarations"
     ["add-declaration" add-declaration (current-pvs-file t)]
     ["modify-declaration" modify-declaration (current-pvs-file t)])
    ("Prettyprint"
     ["prettyprint-theory" prettyprint-theory (current-pvs-file t)]
     ["prettyprint-pvs-file" prettyprint-pvs-file (current-pvs-file t)]
     ["prettyprint-declaration" prettyprint-declaration (current-pvs-file t)]
     ["prettyprint-region" prettyprint-region (current-pvs-file t)]
     ["prettyprint-theory-instance" prettyprint-theory-instance (current-pvs-file t)])
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
     ["latex-set-linelength" latex-set-linelength t]
     ["html-pvs-file" html-pvs-file t]
     ["html-pvs-files" html-pvs-files t])
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
     ["list-declarations" list-declarations (current-pvs-file t)]
     ["show-expanded-form" show-expanded-form (current-pvs-file t)]
     ["unusedby-proof-of-formula" unusedby-proof-of-formula (current-pvs-file t)]
     ["unusedby-proofs-of-formulas" unusedby-proofs-of-formulas (current-pvs-file t)])
    ("Status"
     ["status-theory" status-theory (current-pvs-file t)]
     ["status-pvs-file" status-pvs-file (current-pvs-file t)]
     ["status-importchain" status-importchain (current-pvs-file t)]
     ["status-importbychain" status-importbychain (current-pvs-file t)]
     ["show-theory-warnings" show-theory-warnings (current-pvs-file t)]
     ["show-pvs-file-warnings" show-pvs-file-warnings (current-pvs-file t)]
     ["show-theory-messages" show-theory-messages (current-pvs-file t)]
     ["show-pvs-file-messages" show-pvs-file-messages (current-pvs-file t)]
     ["show-theory-conversions" show-theory-conversions (current-pvs-file t)]
     ["show-pvs-file-conversions" show-pvs-file-conversions (current-pvs-file t)]
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

(unless (featurep 'xemacs)

  (defvar easy-menu-fast-menus nil)

  (let ((easy-menu-fast-menus t))
    (easy-menu-define PVS global-map "PVS menus" pvs-mode-menus)
    (easy-menu-add PVS global-map))
  )

(when (featurep 'xemacs)
  (add-submenu nil pvs-mode-menus "")
  (add-hook 'pvs-mode-hook
    '(lambda ()
       (add-submenu nil pvs-mode-menus ""))))

(require 'json)
(require 'button)
(require 'ring)

(define-button-type 'pvs-decl
    'action 'pvs-decl-button-action
    'face 'default)

(defun pvs-decl-button-action (button)
  (pvs-goto-file-location
   (button-get button 'decl-file)
   (button-get button 'decl-place)))

(defvar pvs-place-ring (make-ring 200))

(defun pvs-goto-file-location (file place)
  (let ((elt (cons (buffer-file-name) (point))))
    (unless (and (not (ring-empty-p pvs-place-ring))
		 (equal elt (ring-ref pvs-place-ring 0)))
      (ring-insert pvs-place-ring elt))
    (find-file file)
    (let ((row (elt place 0))
	  (col (elt place 1)))
      (goto-line row)
      (forward-char col))))

(defun pvs-backto-last-location ()
  (interactive)
  (let* ((elt (ring-remove pvs-place-ring))
	 (file (car elt))
	 (point (cdr elt)))
    (find-file file)
    (goto-char point)))

(global-set-key (kbd "<M-left>") 'pvs-backto-last-location)

(defun pvs-add-tooltips (fname)
  (interactive (list (current-pvs-file)))
  (let* ((dlist-json
	  (pvs-file-send-and-wait (format "(collect-pvs-file-decls-info \"%s\" t)"
				      fname)
				  nil 'get-decls '(or string null)))
	 (dlist (when dlist-json (json-read-from-string dlist-json))))
    (if dlist
	(with-silent-modifications
	  (dotimes (i (length dlist))
	    (let* ((delt (elt dlist i))
		   (region (place-to-region (cdr (assq 'place delt))))
		   (msg (cdr (assq 'decl delt)))
		   (decl-file (cdr (assq 'decl-file delt)))
		   (decl-place (cdr (assq 'decl-place delt))))
	      (add-text-properties
	       (car region) (cdr region)
	       `(mouse-face highlight help-echo ,msg))
	      (make-text-button
	       (car region) (cdr region)
	       'type 'pvs-decl 'decl-file decl-file 'decl-place decl-place)))
	  (message "Tooltips set"))
	(message "Tooltips not set - is file typechecked?"))))

(defun place-to-region (place &optional relrow relcol)
  (let* ((rr (or relrow 0))
	 (prow (elt place 0))
	 (rc (if (and (= prow 1) relcol) relcol 0))
	 (srow (+ prow rr))
	 (scol (+ (elt place 1) rc))
	 (erow (+ (elt place 2) rr))
	 (ecol (+ (elt place 3) rc)))
    (setq ppp (list place relrow relcol))
    (setq rrr (list srow scol erow ecol))
    (cons (row-col-to-point srow scol)
	  (row-col-to-point erow ecol))))

(defun row-col-to-point (row col)
  (save-excursion
    (goto-line row)
    (forward-char col)
    (point)))

(defpvs select-pvs-subterm browse (fname row col)
  "Select a subterm containing point"
  (interactive (list (current-pvs-file)
		     (current-line-number)
		     (current-column)))
  (let* ((slist-json
	  (pvs-file-send-and-wait (format "(get-subterms-at-place \"%s\" %d %d t)"
				      fname row col)
				  nil 'subterms '(or string null)))
	 (slist (when slist-json (json-read-from-string slist-json))))
    (if slist
	(let ((sterm (x-popup-menu
		      t (list "Subterms"
			      (cons "Subterms"
				    (cl-mapcar (lambda (x) (cons x x))
					       slist))))))))))
