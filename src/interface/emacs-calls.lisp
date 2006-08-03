;
; emacs-calls.lisp: dave_sc, 12/19/98
;
; A dummy file that gives information to Allegro's xref so that
; you can use "who-calls" to get information on which emacs files
; call functions in the PVS lisp image.

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

(in-package "PVS")

#-runtime
(declaim (special *dont-write-object-files* *pvs-verbose* *latex-linelength*
		  *dump-sequents-to-file* *number-of-proof-backups*))

#-runtime
(defun pvs-browser-el ()
  (show-declaration nil nil nil)
  (goto-declaration nil nil nil)
  (find-declaration nil)
  (whereis-declaration-used nil nil nil)
  (whereis-identifier-used nil)
  (list-declarations nil)
  (usedby-proofs nil nil nil))

#-runtime
(defun pvs-cmds-el ()
  (parse-file nil nil)
  (typecheck-file nil nil nil nil)
  (prettyprint-region nil nil nil)
  (prettyprint-theory nil nil)
  (prettyprint-pvs-file nil)
  (prettyprint-expanded nil)
  (show-tccs nil)
  (show-theory-warnings nil)
  (show-pvs-file-warnings nil)
  (show-theory-messages nil)
  (show-pvs-file-messages nil)
  (delete-pvs-file nil nil)
  (delete-theory nil)
  (get-patch-version)
  (change-context nil)
  (change-context nil)
  (save-context)
  (load-prelude-library nil)
  (remove-prelude-library nil)
  (prelude-libraries)
  (exit-pvs)
  (let ((foo *pvs-version*)) (declare (ignore foo))  nil)
  (get-patch-version)
  (when (fboundp 'get-patch-test-version)
    (get-patch-test-version))
  (when (fboundp 'get-patch-exp-version)
    (get-patch-exp-version))
  (lisp-implementation-type)
  (lisp-implementation-version)
  (let ((foo *pvs-version*)) (declare (ignore foo)) nil)
  (status-theory nil)
  (status-pvs-file nil)
  (status-importchain nil)
  (status-importbychain nil)
  (proof-status-at nil nil nil)
  (status-proof-theory nil)
  (status-proof-pvs-file nil)
  (proofchain-status-at nil nil nil)
  (status-proofchain-theory nil)
  (status-proofchain-pvs-file nil)
  (status-proofchain-importchain nil)
  (load-pvs-patches)
  (x-module-hierarchy nil)
  (let ((foo *dont-write-object-files*)) (declare (ignore foo)) nil)
  (typecheck-formula-decl nil nil)
  (prove-formula-decl nil nil nil)  ;; ilisp-send
  (get-prove-formula-proof))

#-runtime
(defun pvs-ilisp-el ()
  (let ((foo *default-char-width*)) (declare (ignore foo)) nil))

#-runtime
(defun pvs-file-list-el ()
  (context-files-and-theories nil)
  (library-files)
  (current-libraries))

#-runtime
(defun pvs-load-el ()
  (let ((foo *pvs-path*)) (declare (ignore foo)) nil)
  (pvs-init)
  (let ((foo *noninteractive*)
	(bar *pvs-verbose*))
    (declare (ignore foo bar))
    nil))
		 
#-runtime
(defun pvs-print-el ()
  (context-usingchain nil)
  (context-usingchain nil)
  (when *last-proof*
    (label *last-proof*))
  (alltt-proof nil nil)
  (latex-theory nil nil)
  (latex-pvs-file nil)
  (latex-usingchain nil nil)
  (latex-theory-view nil nil nil)
  (when *last-proof*
    (label *last-proof*))
  (latex-proof-view nil nil nil)
  (when *last-proof*
    (label *last-proof*))
  (latex-proof nil nil)
  (let ((foo *latex-linelength*)) (declare (ignore foo)) nil))

#-runtime
(defun pvs-prover-el ()
  (typecheck-file nil nil nil nil nil)
  (typechecked? nil)
  (typecheck-file nil nil nil nil nil)
  (typechecked? nil)
  (rerun-proof-at? nil nil nil nil)
  (prove-file-at nil nil nil nil nil nil nil nil) ;; ilisp-send
  (prove-theory nil nil nil)
  (prove-pvs-file nil nil)
  (prove-pvs-theories nil nil)
  (prove-usingchain nil nil)
  (prove-proofchain nil nil nil nil nil)
  (typecheck-file nil nil nil nil nil)
  (typechecked? nil)
  (let ((foo *in-checker*)) (declare (ignore foo)) nil)
  (edit-proof-at nil nil nil nil nil nil nil)
  (prove-file-at nil nil nil nil nil nil nil nil) ;; ilisp-send
  (let ((foo *in-checker*)) (declare (ignore foo)) nil)
  (remove-proof-at nil nil nil nil)
  (install-pvs-proof-file nil)
  (show-proof-file nil nil)
  (show-orphaned-proofs)
  (show-proofs-importchain nil)
  (show-proofs-pvs-file nil)
  (show-proofs-theory nil)
  (pvs-select-proof nil)
  (pvs-view-proof nil)
  (pvs-delete-proof nil)
  (add-declaration-at nil nil)
  (typecheck-add-declaration nil nil)
  (modify-declaration-at nil nil)
  (typecheck-mod-declaration nil nil)
  (reset-parsed-date nil)
  (help-prover)
  (show-last-proof nil)
  (read-strategies-files)
  (let ((foo *rewrite-print-depth*)
	(bar *rewrite-print-length*)
	(taz *prover-print-depth*)
	(baz *prover-print-length*)
	(gaz *prover-print-lines*))
    (declare (ignore foo bar taz baz gaz))
    nil)
  (typecheck-file nil nil nil nil nil)
  (typechecked? nil)
  (prove-untried-importchain nil nil nil nil)
  (prove-untried-pvs-file nil nil nil)
  (prove-untried-theory nil nil nil nil)
  (prove-formulas-importchain nil nil nil nil)
  (prove-formulas-pvs-file nil nil nil)
  (prove-formulas-theory nil nil nil nil)
  (prove-tccs-importchain nil nil nil nil)
  (prove-tccs-pvs-file nil nil nil)
  (prove-tccs-theory nil nil nil nil)
  (edit-proof-at nil nil nil nil nil nil nil)
  (call-ancestry)
  (call-siblings)
  (call-show-proof)
  (call-x-show-proof)
  (call-x-show-proof-at nil nil nil nil)
  (call-show-hidden)
  (show-auto-rewrites)
  (show-expanded-sequent nil)
  (show-skolem-constants)
  (call-explain-tcc)
  (help-prover nil)
  (show-strategy nil)
  (collect-strategy-names)
  (x-prover-commands)
  (toggle-proof-prettyprinting)
  (prove-usingchain nil nil nil)
  (prove-pvs-theories nil nil)
  (typecheck-file nil nil nil nil nil)
  (typechecked? nil)
  (let ((foo *dump-sequents-to-file*)) (declare (ignore foo)) nil)
  (display-proofs-formula-at nil nil nil nil)
  (display-proofs-theory nil)
  (display-proofs-pvs-file nil)
  (set-proofs-default nil)
  (proofs-rename nil nil)
  (proofs-show-proof nil)
  (proofs-change-description nil nil)
  (proofs-rerun-proof nil) ;; ilisp-send
  (proofs-delete-proof nil)
  (proofs-edit-proof nil)
  (install-proof nil nil nil nil nil nil nil)
  (let ((foo *number-of-proof-backups*)) (declare (ignore foo)) nil)
  (prove-proof-at nil nil nil)  ;; ilisp-send
  (prove-file-at nil nil nil nil nil nil nil nil) ;; ilisp-send
  (set-decision-procedure nil)
  (let ((foo *default-decision-procedure*)
	(bar *decision-procedures*)
	(baz *show-parens-in-proof*))
    (declare (ignore foo bar baz))
    nil)
  )

#-runtime
(defun pvs-utils-el ()
  (file-and-place (get-theory nil))
  (parsed? (get-theory nil))
  (typechecked? (get-theory nil))
  (library-file? nil)
  (library-files)
  (library-theories)
  (pvs-current-directory)
  (collect-theories)
  (collect-file-usings nil)
  (get-pvs-file-dependencies nil)
  (pvs-emacs-eval "(setq pvs-waiting nil)"))
