;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-cmds.el -- 
;; Author          : Sam Owre
;; Created On      : Wed Sep 15 17:53:20 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Sep 19 14:50:21 1996
;; Update Count    : 27
;; Status          : Beta testing
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; parse                   - parse-file
;; typecheck               - typecheck-file
;; typecheck-prove         - typecheck-file
;; prettyprint-declaration - prettyprint-region
;; prettyprint-region      - prettyprint-region
;; prettyprint-theory      - prettyprint-theory
;; prettyprint-pvs-file    - prettyprint-pvs-file
;; prettyprint-expanded    - prettyprint-expanded
;; show-tccs               - show-tccs
;; view-prelude-file       - N/A
;; view-prelude-theory     - N/A
;; find-pvs-file           - N/A
;; find-theory             - N/A
;; new-pvs-file            - N/A
;; new-theory              - N/A
;; import-pvs-file         - N/A
;; import-theory           - N/A
;; delete-pvs-file         - delete-pvs-file
;; delete-theory           - delete-theory
;; save-some-pvs-files     - N/A
;; mail-pvs-files (unfinished)
;; dump-pvs-files          - get-pvs-file-dependencies
;; undump-pvs-files        - N/A
;; change-context          - change-context
;; save-context            - save-context
;; context-path            - N/A
;; help-pvs                - N/A
;; help-pvs-language       - N/A
;; suspend-pvs             - N/A
;; exit-pvs                - exit-pvs
;; pvs-log                 - N/A
;; pvs-version             - *pvs-version*
;; save-pvs-buffer         - N/A
;; status-theory           - status-theory
;; status-pvs-file         - status-pvs-file
;; status-importchain      - status-importchain
;; show-importchain        - using-status
;; show-importbychain      - usedby-status
;; status-proof            - proof-status-at
;; status-proof-theory     - proof-status-theory
;; status-proof-pvs-file   - proof-status-pvs-file
;; status-proof-importchain- proof-status-usingchain
;; status-proofchain       - proofchain-status-at
;; whereis-pvs             - N/A
;; load-patches            - load-pvs-patches
;; status-display          - N/A
;; change-declaration-name (unfinished)
;; x-theory-hierarchy	   - x-module-hierarchy
;; pvs-dont-write-bin-files - N/A
;; pvs-do-write-bin-files  - N/A
;; pvs-remove-bin-files    - N/A

(require 'cl)
(eval-when-compile (require 'pvs-macros))
(provide 'pvs-cmds)

;;;----------------------------------------------------------------------
;;; Basic Commands


;;; Parsing

(defpvs parse parse (filename)
  "Parse the PVS file in the current buffer.

The parse command parses the PVS file in the current buffer.  With an
argument, it forces reparsing of the file."
  (interactive (list (current-pvs-file)))
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send (format "(parse-file \"%s\" %s)"
		filename (and current-prefix-arg t))
	    nil (pvs-get-abbreviation 'parse)))


;;; Typechecking

(defpvs typecheck typecheck (filename)
  "Typecheck the PVS file in the current buffer.

The typecheck command typechecks the PVS file in the current buffer.  If
necessary, the file is parsed as well.  With an argument, forces reparsing
and retypechecking of the file."
  (interactive (list (current-pvs-file)))
  (pvs-typecheck-file filename nil nil 'typecheck))

(defpvs typecheck-importchain typecheck (filename)
  "Typecheck the importchain of all the theories in the current buffer.

Typechecks the entire importchain of the theories of the current buffer.
With an argument, forces reparsing and retypechecking of the entire
importchain."
  (interactive (list (current-pvs-file)))
  (pvs-typecheck-file filename nil t 'typecheck-importchain))

(defpvs typecheck-prove typecheck (filename)
  "Typecheck the PVS file in the current buffer, and attempts to prove TCCs.

Typechecks the PVS file of the current buffer, proving TCCs using the
default TCC strategy.  If the file is already typechecked, simply tries
to prove any TCCs which have not already been attempted.  With an argument,
forces retypechecking of the file."
  (interactive (list (current-pvs-file)))
  (pvs-typecheck-file filename t nil 'typecheck-prove))

(defpvs typecheck-prove-importchain typecheck (filename)
  "Typecheck the importchain of all the theories in the current buffer, and
attempt to prove the TCCs.

Typechecks the theories of the current buffer, and attempts to prove the
TCCs of the import chain of the theories.  With an argument, forces
reparsing and retypechecking of the entire importchain."
  (interactive (list (current-pvs-file)))
  (pvs-typecheck-file filename t t 'typecheck-prove-importchain))

(defun pvs-typecheck-file (filename prove-tccs-p importchain-p cmd)
  (save-some-pvs-buffers)
  (cond ((file-exists-p (pvs-file-name filename))
	 (pvs-bury-output)
	 (pvs-send (format "(typecheck-file \"%s\" %s %s %s)"
		       filename (and current-prefix-arg t)
		       prove-tccs-p importchain-p)
		   nil (pvs-get-abbreviation cmd)))
	(t (message "PVS file %s does not exist" filename))))


;;; Prettyprinting

(defpvs prettyprint-declaration prettyprint ()
  "Prettyprints the declaration at the cursor.

The prettyprint-declaration command prettyprints the declaration at the
cursor.  Use undo (C-x u or C-_) or M-x revert-buffer to return to the
old version."
  (interactive)
  (prettyprint-region (point) (point) t))


(defpvs prettyprint-region prettyprint (beg end &optional declp)
  "Prettyprints all declarations contained in the current region.

The prettyprint-region command prettyprints all the declarations that are
(partially) contained in the specified region.  Use undo (C-x u or C-_) or
M-x revert-buffer to return to the old version."
  (interactive "r")
  (pvs-bury-output)
  (let ((filename (current-pvs-file)))
    (save-pvs-file filename)
      (save-excursion
	(goto-char beg)
	(let ((bline (current-line-number))
	      (bcol (real-current-column)))
	  (goto-char end)
	  (pvs-send-and-wait
	   (format "(prettyprint-region \"%s\" '(%d %d) '(%d %d))"
	       filename bline bcol
	       (current-line-number) (real-current-column))
	   "Prettyprint" (if declp 'ppd 'ppr) 'dont-care)
	  (if (buffer-modified-p)
	      (message
	       "prettyprinted %s - use C-x u to undo changes."
	       (if declp "declaration" "region"))
	      (message "No changes were made"))))))


(defpvs prettyprint-theory prettyprint (theoryname)
  "Prettyprints the specified theory

The prettyprint-theory command prettyprints the specified theory and
replaces the old theory in the current buffer.  Can use undo (C-x u or
C-_) or M-x revert-buffer to return to the old version."
  (interactive (complete-theory-name "Prettyprint theory name: "))
  (pvs-bury-output)
  (save-some-pvs-buffers t)
  (let ((file (when (member-equal theoryname (buffer-theories))
		(current-pvs-file))))
    (pvs-send-and-wait (format "(prettyprint-theory \"%s\" %s)"
			   theoryname (when file (format "\"%s\"" file)))
		       "Prettyprinting"
		       (pvs-get-abbreviation 'prettyprint-theory)
		       'dont-care))
  (if (buffer-modified-p)
      (message
       "prettyprinted theory - use C-x u to undo changes.")
      (message "No changes were made")))


(defpvs prettyprint-pvs-file prettyprint (filename)
  "Prettyprints the PVS file in the current buffer

The prettyprint-pvs-file command prettyprints the PVS file in the current
buffer.  The buffer is modified to show the prettyprinted version.  Use
undo (C-x u or C-_) or M-x revert-buffer to return to the old version."
  (interactive (list (current-pvs-file)))
  (pvs-bury-output)
  (save-pvs-file filename)
  (pvs-send-and-wait (format "(prettyprint-pvs-file \"%s\")" filename)
		     "Prettyprinting"
		     (pvs-get-abbreviation 'prettyprint-pvs-file)
		     'dont-care)
  (if (buffer-modified-p)
      (message
       "prettyprinted file - use C-x u to undo changes.")
      (message "No changes were made")))


;;; prettyprint-expanded

(defvar pvs-context-sensitive nil
  "Indicates whether the buffer should be deleted when the context is
changed.")
(make-variable-buffer-local 'pvs-context-sensitive)

(defvar pvs-show-formulas-map nil)
(if pvs-show-formulas-map ()
    (setq pvs-show-formulas-map (copy-keymap pvs-view-mode-map))
    (define-key pvs-show-formulas-map "\C-c\C-i" 'install-proof)
    (define-key pvs-show-formulas-map "\C-cp" 'prove)
    (define-key pvs-show-formulas-map "\C-c\C-pp" 'prove)
    (define-key pvs-show-formulas-map "\C-c\C-p\C-p" 'prove)
    (define-key pvs-show-formulas-map "\C-c\C-px" 'x-prove)
    (define-key pvs-show-formulas-map "\C-c\C-pX" 'x-step-proof)
    (define-key pvs-show-formulas-map "\C-c\C-pc" 'prove-proofchain)
    (define-key pvs-show-formulas-map "\C-c\C-ps" 'step-proof)
    (define-key pvs-show-formulas-map "\C-c\C-pr" 'redo-proof))

(defpvs prettyprint-expanded tcc (theory)
  "View the expanded form of a theory

The prettyprint-expanded command provides a view of the entire
theory, with the TCCs inserted in a buffer named \"THEORY.ppe\".  The buffer
is read-only, and may not be parsed or typechecked, although proofs of any
displayed TCCs or other formulas may be initiated in the usual way, simply
by moving the cursor to the formula to be proved and invoking the prove
command."
  (interactive (complete-theory-name "Prettyprint-expanded theory named: "))
  (pvs-bury-output)
  (message "Creating the %s.ppe buffer..." theory)
  (pvs-send-and-wait (format "(prettyprint-expanded \"%s\")" theory)
		     nil (pvs-get-abbreviation 'prettyprint-expanded)
		     'dont-care)
  (message "")
  (let ((buf (get-buffer (format "%s.ppe" theory))))
    (when buf
      (save-excursion
	(set-buffer buf)
	(setq pvs-context-sensitive t)
	(pvs-view-mode)
	(use-local-map pvs-show-formulas-map)))))

(defpvs show-tccs tcc (theory)
  "Shows all the TCCs of the indicated theory

The show-tccs command pops up a buffer with the name \"THEORY.tccs\".
displaying just the TCCs of the theory.  Proofs of any displayed TCCs may
be initiated in the usual way, simply by moving the cursor to the formula
to be proved and invoking the prove command."
  (interactive (complete-theory-name "Show TCCs of theory named: "))
  (message "Creating the %s.tccs buffer..." theory)
  (pvs-send-and-wait (format "(show-tccs \"%s\" %s)"
			 theory (and current-prefix-arg t))
		     nil (pvs-get-abbreviation 'show-tccs)
		     'dont-care)
  (let ((buf (get-buffer (format "%s.tccs" theory))))
    (when buf
      (save-excursion
	(message "")
	(set-buffer buf)
	(setq pvs-context-sensitive t)
	(pvs-view-mode)
	(use-local-map pvs-show-formulas-map)))))


;;; Show-theory-warnings

(defpvs show-theory-warnings theory-status (theoryname)
  "Displays the warnings associated with THEORYNAME"
  (interactive (complete-theory-name "Show warnings of theory named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(show-theory-warnings \"%s\")" theoryname) nil
		     (pvs-get-abbreviation 'show-theory-warnings)
		     'dont-care))

(defpvs show-pvs-file-warnings theory-status (filename)
  "Displays the warnings associated with FILENAME"
  (interactive (complete-pvs-file-name "Show warnings of PVS file named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(show-pvs-file-warnings \"%s\")" filename) nil
		     (pvs-get-abbreviation 'show-pvs-file-warnings)
		     'dont-care))

(defpvs show-theory-messages theory-status (theoryname)
  "Displays the informational messages associated with THEORYNAME"
  (interactive (complete-theory-name "Show messages of theory named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(show-theory-messages \"%s\")" theoryname) nil
		     (pvs-get-abbreviation 'show-theory-warnings)
		     'dont-care))

(defpvs show-pvs-file-messages theory-status (filename)
  "Displays the informational messages associated with FILENAME"
  (interactive (complete-pvs-file-name "Show messages of PVS file named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(show-pvs-file-messages \"%s\")" filename) nil
		     (pvs-get-abbreviation 'show-pvs-file-warnings)
		     'dont-care))


;;; View Prelude

(defvar pvs-prelude nil
  "Line number indicating where the buffer is in the prelude (local).")
(make-variable-buffer-local 'pvs-prelude)

(defpvs view-prelude-file find-files ()
  "Views the prelude file

The view-prelude-file command displays the prelude file in a buffer in
read-only mode.  Although you may not modify the prelude, you may prove
any of the prelude formulas simply by placing the cursor at the formula
and invoking the prove command."
  (interactive)
  (let ((fname (format "%s/lib/prelude.pvs" pvs-path)))
    (if (not (file-exists-p fname))
	(error "%s does not exist." fname)
	(find-file fname)
	(setq pvs-prelude 0)
	(unless buffer-read-only (toggle-read-only))
	(pvs-mode))))


(defpvs view-prelude-theory find-files (theoryname)
  "Views the specified prelude theory

The view-prelude-theory command displays the specified prelude theory in a
buffer in read-only mode.  Although you may not modify the prelude, you
may prove any of the prelude formulas simply by placing the cursor at the
formula and invoking the prove command."
  (interactive (complete-prelude-name))
  (let* ((freg (get-prelude-file-and-region theoryname))
	 (fname (car freg)))
    (if (not (file-exists-p fname))
	(error "%s does not exist." fname)
	(let* ((buf (find-file-noselect fname))
	       (region (cdr freg)))
	  (if region
	      (let ((pbuf (get-buffer-create theoryname)))
		(set-buffer pbuf)
		(setq pvs-prelude (save-excursion
				    (set-buffer buf)
				    (goto-char (car region))
				    (- (current-line-number) 1)))
		(when buffer-read-only (toggle-read-only))
		(erase-buffer)
		(insert-buffer-substring buf (car region) (cadr region))
		(unless buffer-read-only (toggle-read-only))
		(goto-char (point-min))
		(pop-to-buffer pbuf)
		(pvs-mode))
	      (error "%s is not in the prelude."))))))

(defvar *prelude-files-and-regions*
  (mapcar
      '(lambda (x)
	 (cons (format "%s/lib/%s" pvs-path (car x)) (cdr x)))
    '(("character_adt.pvs"
       ("character_adt" 1 2096) ("character_adt_reduce" 2096 2650))
      ("list_adt.pvs"
       ("list_adt" 1 4227) ("list_adt_map" 4227 4680)
       ("list_adt_reduce" 4680 5550))
      ("ordstruct_adt.pvs"
       ("ordstruct_adt" 1 3916) ("ordstruct_adt_reduce" 3916 5243))
      ("prelude.pvs"
       ("booleans" 1 1515) ("equalities" 1515 1728) ("notequal" 1728 1826)
       ("if_def" 1826 2099) ("boolean_props" 2099 3154) ("xor_def" 3154 3458)
       ("quantifier_props" 3458 4390) ("defined_types" 4390 4692)
       ("exists1" 4692 5309) ("equality_props" 5309 5802)
       ("if_props" 5802 6173) ("functions" 6173 7251) ("restrict" 7251 7693)
       ("extend" 7693 8089) ("extend_bool" 8089 8352) ("identity" 8352 8710)
       ("relations" 8710 9372) ("orders" 9372 10780)
       ("wf_induction" 10780 11123) ("measure_induction" 11123 11559)
       ("epsilons" 11559 12197) ("sets" 12197 13602)
       ("sets_lemmas" 13602 17452) ("function_inverse" 17452 18695)
       ("function_image" 18695 20068) ("function_props" 20068 20950)
       ("function_props2" 20950 21293) ("relation_defs" 21293 21868)
       ("relational_composition" 21868 22134) ("operator_defs" 22134 22977)
       ("numbers" 22977 23465) ("reals" 23465 24566)
       ("real_axioms" 24566 25906) ("real_types" 25906 26977)
       ("rationals" 26977 28865) ("integers" 28865 31281)
       ("naturalnumbers" 31281 32508) ("real_defs" 32508 35543)
       ("real_props" 35543 50168) ("rational_props" 50168 50573)
       ("integer_props" 50573 51109) ("floor_ceil" 51109 52814)
       ("exponentiation" 52814 58086) ("subrange_inductions" 58086 58731)
       ("bounded_int_inductions" 58731 59515)
       ("bounded_nat_inductions" 59515 60338) ("subrange_type" 60338 60498)
       ("int_types" 60498 60708) ("nat_types" 60708 60914)
       ("function_iterate" 60914 61578) ("sequences" 61578 63037)
       ("seq_functions" 63037 63359) ("finite_sequences" 63359 64803)
       ("ordstruct" 64803 65011) ("ordinals" 65011 66873)
       ("lex2" 66873 67381) ("list" 67381 67525) ("list_props" 67525 68914)
       ("map_props" 68914 69299) ("filters" 69299 70196)
       ("list2finseq" 70196 70719) ("list2set" 70719 70976)
       ("character" 70976 72301) ("strings" 72301 73118)
       ("tuple_prelude_example" 73118 73829)
       ("record_prelude_example" 73829 74312)))))

(defun set-prelude-files-and-regions ()
  (unless *prelude-files-and-regions*
    (let* ((files (directory-files (format "%s/lib" pvs-path)
				   t "^prelude\\.pvs$\\|.*_adt\\.pvs$")))
      (setq *prelude-files-and-regions*
	    (mapcar '(lambda (file)
		      (save-excursion
			(let ((noninteractive t)) ;; Shut up about read-only
			  (set-buffer (find-file-noselect file)))
			(cons file (theory-regions*))))
		    files)))))

(defun get-prelude-file-and-region (theoryname)
  (let ((freg nil)
	(pfregs *prelude-files-and-regions*))
    (while (and (null freg)
		pfregs)
      (let ((reg (cdr (assoc theoryname (cdr (car pfregs))))))
	(if reg
	    (setq freg (cons (caar pfregs) reg))
	    (setq pfregs (cdr pfregs)))))
    freg))


(defun complete-prelude-name ()
  (set-prelude-files-and-regions)
  (let* ((names (apply 'append
		      (mapcar 'cdr *prelude-files-and-regions*)))
	 (th (completing-read "View prelude theory named: " names nil t)))
    (if (equal th "")
	(error "No theory specified")
	(list th))))


;;; View library commands

(defpvs view-library-file find-files (library-file)
  "Views a library file

The view-library-file command displays a library file in a buffer of the
same name.  Only files from libraries that have been imported (either
directly or using the load-prelude-library command) are available.  With
an argument, includes the distributed libraries even if they are not
imported."
  (interactive (list (pvs-complete-library-name "View library file: "
						current-prefix-arg)))
  (let ((default-directory *pvs-current-directory*))
    (switch-to-buffer (find-file-noselect (concat library-file ".pvs")))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)))

(defpvs view-library-theory find-files (library-file-and-place)
  "Views a library theory

The view-library-theory command displays a library file with the cursor at
the beginning of the specified theory."
  (interactive
   (list (pvs-complete-library-theory-name "View library theory: ")))
  (let ((default-directory *pvs-current-directory*))
    (switch-to-buffer (find-file-noselect
		       (concat (car library-file-and-place) ".pvs")))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (goto-line (car (cadr library-file-and-place)))))
  


;;;------------------------------------------------
;;; File and Theory Commands

;;; find-pvs-file

(defpvs find-pvs-file find-files (filename)
  "Finds the specified PVS file

The find-pvs-file command reads the specified PVS file into a buffer.  It
is very much like the Emacs find-file command, but restricts completion to
PVS files (i.e., those with extension '.pvs'"
  (interactive (complete-pvs-file-name "Find PVS file named: " t))
  (switch-to-buffer (get-pvs-file-buffer filename)))


;;; find-theory

(defpvs find-theory find-files (theoryname)
  "Finds the PVS file containing the specified theory

The find-theory command finds the PVS file associated with the specified
theory, and reads in that file."
  (interactive (complete-theory-name
		"Find PVS file containing theory named: "))
  (let ((tbuf (get-theory-buffer theoryname)))
    (if (null tbuf)
	(message "%s is not in the current context" theoryname)
	(set-buffer tbuf)
	(let ((treg (theory-region-from-buffer theoryname)))
	  (when treg (goto-char (cadr treg))))
	(switch-to-buffer tbuf))))


;;; new-pvs-file

(defpvs new-pvs-file new-files (theoryname)
  "Create a new PVS file

The new-pvs-file command creates a new file of the specified name, and
inserts a theory skeleton with the same name.  Current buffer becomes
associated with the new file.  With an argument, only a minimal skeleton
is created."
  (interactive (new-pvs-file-name "New file name: "))
  (unless (valid-theory-name theoryname)
    (error "%s is not valid as a theoryname" theoryname))
  (pvs-bury-output)
  (find-file (pvs-file-name theoryname))
  (pvs-insert-theory-template theoryname nil (and current-prefix-arg t)))

(defun pvs-insert-theory-template (theoryname &optional end min)
  (cond (min
	 (insert (format "%s: THEORY" theoryname))
	 (insert "\nBEGIN\n ")
	 (save-excursion
	   (insert (format "\nEND %s\n%s" theoryname (or end "")))))
	(t
	 (insert (format "%s  %% [ parameters ]\n\t\t: THEORY" theoryname))
	 (insert "\n\n  BEGIN\n\n  % ASSUMING\n   % assuming declarations\n")
	 (insert "  % ENDASSUMING\n\n  ")
	 ;;(insert "\n\n  % Body of theory\n  ")
	 (save-excursion
	   (insert (format "\n\n  END %s\n%s" theoryname (or end "")))))))


;;; new-theory

(defpvs new-theory new-files (theoryname)
  "Create a new theory

The new-theory command creates a new theory skeleton and inserts it in the
current PVS buffer after the current theory.  With an argument, only a
minimal skeleton is produced."
  (interactive (when (current-pvs-file)
		 (new-theory-name "New theory name: ")))
  (pvs-bury-output)
  (unless (forward-theory t)
    (goto-char (point-max))
    (insert "\n\n"))
  (pvs-insert-theory-template theoryname "\n\n" (and current-prefix-arg t)))


;;; import-pvs-file

(defvar pvs-last-import-dir nil)

(defpvs import-pvs-file import (file name)
  "Import a text file as a PVS file

The import-pvs-file command prompts for a source file and a target file
and copies the former to the latter, and places the file in the current
context.  In addition, the corresponding proof file is copied."
  (interactive
   (let ((path (car (pvs-complete-file-name
		     "Import PVS file from: " pvs-last-import-dir))))
     (cons path
	   (new-pvs-file-name "Import to file name: "
			      (pathname-name path) t))))
  (pvs-bury-output)
  (unless (file-exists-p file)
    (error "File does not exist"))
  (setq pvs-last-import-dir (pathname-directory file))
  (let* ((nfile (pvs-file-name name))
	 (pfile (format "%s%s.prf" (pathname-directory file) name))
	 (npfile (pvs-file-name name "prf")))
    (when (or (not (file-exists-p nfile))
	      (y-or-n-p "File already exists - overwrite? "))
      (copy-file file nfile t)
      (cond ((and (file-exists-p pfile)
		  (or (not (file-exists-p npfile))
		      (y-or-n-p "Proof file exists - overwrite? ")))
	     (copy-file pfile npfile t)
	     (message "Imported %s and %s" file pfile))
	    (t (message "Imported %s" file)))
      (find-pvs-file name))))


(defpvs import-theory import ()
  "Import a theory into the current buffer

The import-theory command prompts for a source file and a theory within
the source; the theory is copied after the current theory in the current
PVS buffer.  It is an error to invoke this command from any buffer other
than a '.pvs' buffer."
  (interactive)
  (pvs-bury-output)
  (let* ((file (current-pvs-file))
	 (fromfile (read-file-name "Import theory from file: "
				   pvs-last-import-dir nil t))
	 (fbuf (find-file-noselect fromfile))
	 (theoryname (save-excursion
		       (set-buffer fbuf)
		       (car (complete-theory-name-in-buffer
			     "Import theory named: ")))))
    (save-excursion
      (unless (forward-theory t)
	(goto-char (point-max))
	(insert "\n\n"))
      (let ((treg (save-excursion
		    (set-buffer fbuf)
		    (theory-region-from-buffer theoryname))))
	(apply 'insert-buffer-substring fbuf (cdr treg))))))
	

;;; delete-pvs-file

(defpvs delete-pvs-file delete-file (file)
  "Delete PVS file from the context

The delete-pvs-file command deletes the specified PVS file from the
current context.  With an argument (e.g. C-u or M-0) the file is also
deleted from the directory."
  (interactive (complete-pvs-file-name "Delete PVS file named: "
				       nil nil t))
  (kill-pvs-buffer file)
  (when (or (null current-prefix-arg)
	    (yes-or-no-p
	     "The file will be deleted from the directory - sure? "))
    (pvs-send-and-wait (format "(delete-pvs-file \"%s\" %s)"
			   file (and current-prefix-arg t))
		       nil (pvs-get-abbreviation 'delete-pvs-file)
		       'dont-care)))


;;; delete-theory

(defpvs delete-theory delete-file (theoryname)
  "Delete theory from the current buffer

The delete-theory command deletes the specified theory from the current
buffer, which must be associated with a PVS file."
  (interactive (complete-theory-name "Delete theory named: " t))
  (let ((tbuf (get-theory-buffer theoryname)))
    (save-excursion
      (set-buffer tbuf)
      (let ((treg (theory-region theoryname)))
	(apply 'kill-region (cdr treg)))
      (pvs-send-and-wait (format "(delete-theory \"%s\")" theoryname)
			 nil (pvs-get-abbreviation 'delete-theory)
			 'dont-care)
      (pvs-msg "Theory %s deleted from %s" theoryname
	       (pvs-buffer-file-name tbuf)))))


;;; save-some-pvs-files

(defpvs save-some-pvs-files save-files (&optional arg exiting)
  "Save some modified file-visiting theory buffers

The save-some-pvs-files searches for modified buffers that are associated
with a PVS file and asks, for each one, whether to save it.  With an
argument, all modified PVS buffers are saved with no questions.  If
exiting PVS, buffers not saved will be deleted."
  (interactive)
  (dolist (buf (buffer-list))
    (condition-case ()
	(and (buffer-modified-p buf)
	     (let ((thname (pvs-buffer-file-name buf)))
	       (and thname
		    (or arg
			(y-or-n-p (format "Save theory %s? " thname)))
		    (save-excursion (set-buffer buf) (save-buffer)))))
      (error nil))))


;;; mail-pvs-files

(defvar pvs-last-email-address "pvs-bugs@csl.sri.com")
(defvar pvs-email-info-string
  "\n\nType your message and C-c C-c to send.\nThe PVS files will be sent separately.")

(defpvs rmail-pvs-files mail-files (file)
  "Unpack files mailed with smail-pvs-files.

To use this, first create a new directory in which to install the files,
and using your favorite mailer, copy the files to the new directory with
extensions corresponding to the message order, e.g., mail.01, mail.02,
etc.  If there is just one file, leave the extension off.  Then invoke
M-x rmail-pvs-files and give the root file name when prompted (e.g., mail)."
  (interactive "FRoot name of PVS mail files: ")
  (let ((default-directory (pathname-directory file)))
    (shell-command (format "untarmail %s*"
		       (file-name-nondirectory file)))))

(defpvs smail-pvs-files mail-files (pvs-file libraries-p to cc subject)
  "Dump files in transitive closure of import lists to a tar file, uuencode
it, and mail it using tarmail.

The smail-pvs-files command prompts for a PVS file name, an email address,
a CC address, and a subject.  It then creates a tar file containing all
the files needed to recreate the context of a given file.  It then
uuencodes the file and mails it using tarmail.  Included in the tar
file are the specification files, the associated proof files, the
.pvscontext, pvs-strategies, and pvs-tex.sub files, and a newly created
subdirectory named PVSHOME that contains the .pvsemacs, pvs-strategies,
and pvs-tex.sub files from your home directory."
  (interactive (append (complete-pvs-file-name
			"Name of root file (CR for this one): ")
		       (list (y-or-n-p "Include libraries? "))
		       (list (read-from-minibuffer
			      (format "Mail to: " pvs-last-email-address)
			      pvs-last-email-address))
		       (list (read-string "CC: "))
		       (list (read-string "Subject: "))))
  (unless (equal to pvs-last-email-address)
    (setq pvs-last-email-address to))
  (unless (mail)
    (error "Can't send files until mail buffer is empty"))
  (insert to)
  (unless (or (null cc) (equal cc ""))
    (insert (format "\nCc: %s" cc)))
  (search-forward "Subject: ")
  (insert subject)
  (search-forward (concat "\n" mail-header-separator "\n"))
  (save-excursion
    (insert pvs-email-info-string))
  (let* ((lkeymap (copy-keymap (current-local-map)))
	 (file-string (pvs-dump-files-string pvs-file libraries-p))
	 (cmd (format "tarmail -b 100000 %s \"%s\" %s"
		  to subject file-string)))
    (define-key lkeymap "\C-c\C-c"
      (` (lambda ()
	   (interactive)
	   (pvs-mail-send-and-exit (, cmd)))))
    (use-local-map lkeymap)))

(defun pvs-mail-send-and-exit (cmd)
  (let ((homedir (or (pvs-copy-home-directory-files) "")))
    (goto-char (point-min))
    (when (search-forward pvs-email-info-string nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-max))
    (insert (format "\n\n Patch version: %s" (get-pvs-version-information)))
    (mail-send-and-exit nil)
    (let ((default-directory *pvs-current-directory*))
      (shell-command (format "%s %s" cmd homedir)))))


;;; Returns a string that expands to the files to be dumped, for example
;;;   ".pvscontext pvs-tex.sub reals.{pvs,prf}"

(defun pvs-dump-files-string (pvs-file libraries-p)
  (let ((filenames (get-pvs-file-dependencies pvs-file libraries-p))
	(default-directory *pvs-current-directory*))
    (concat (if (file-exists-p "pvs-strategies")
		"pvs-strategies " "")
	    (if (file-exists-p ".pvscontext")
		".pvscontext " "")
	    (if (file-exists-p "pvs-tex.sub")
		"pvs-tex.sub " "")
	    (mapconcat 'pvs-dump-file-string filenames " "))))

(defun pvs-dump-file-string (fn)
  (let ((pvs-file (format "%s.pvs" fn))
	(prf-file (format "%s.prf" fn)))
    (format "%s %s"
	(if (file-exists-p pvs-file) pvs-file "")
      (if (file-exists-p prf-file) prf-file ""))))

(defun pvs-copy-home-directory-files ()
  (let ((files (pvs-home-directory-files))
	(default-directory *pvs-current-directory*))
    (when files
      (let ((dname (pvs-create-local-home-directory-name)))
	(dolist (ff files)
	  (copy-file (expand-file-name (format "~/%s" ff))
		     (format "%s/%s" dname ff)))
	dname))))

(defun pvs-home-directory-files ()
  (let* ((default-directory (expand-file-name "~")))
    (remove-if '(lambda (ff) (not (file-exists-p ff)))
      '(".pvsemacs" "pvs-strategies" "pvs-tex.sub" ".pvs.lisp"))))

(defun pvs-create-local-home-directory-name (&optional num)
  (let ((dname "PVSHOME"))
    (if (file-exists-p dname)
	(if (file-directory-p dname)
	    (dolist (ff (directory-files dname))
	      (unless (file-directory-p ff)
		(delete-file (format "%s/%s" dname ff))))
	    (error "Cannot create directory PVSHOME since it is a file"))
	(make-directory (expand-file-name dname)))
    dname))

;;; dump-pvs-files

(defpvs dump-pvs-files dump-files (pvs-file libraries-p dump-file)
  "Dump files in transitive closure of import lists to file

The dump-pvs-files command creates a new file that contains all the
specifications and associated proofs for the import chain of the specified
PVS file.  If given an argument, includes library files as well."
  (interactive (append (complete-pvs-file-name
			"Name of root file (CR for this one): ")
		       (list (y-or-n-p "Include libraries? "))
		       (list (read-file-name
			      "Dump theories into file name: "))))
  (when (or (not (file-exists-p dump-file))
	    (yes-or-no-p (format "File %s exists - overwrite? "
			     dump-file)))
    (let ((dump-buffer (get-buffer-create "*pvs-temp-buffer*")))
      (save-excursion
	(set-buffer dump-buffer)
	(erase-buffer)
	(dump-pvs-files-to-current-buffer pvs-file libraries-p)
	(write-file dump-file)
	(kill-buffer dump-buffer)))))

(defpvs edit-pvs-dump-file dump-files (dump-file)
  "Edit a PVS dump file

The edit-pvs-dump-file command makes it easier to edit dump files by
making use of outline mode."
  (interactive "fPVS Dump file: ")
  (unless (file-exists-p dump-file)
    (error "Dump file does not exist"))
  (find-file dump-file)
  (setq outline-regexp "\\$")
  (outline-mode)
  (hide-body)
  (message "You are in outline-mode; type C-h m for help"))

(defun dump-pvs-files-to-current-buffer (pvs-file libraries-p)
  (insert-pvs-patch-information)
  (let ((default-directory *pvs-current-directory*))
    (when (file-exists-p "pvs-strategies")
      (insert (format "\n$$$pvs-strategies\n"))
      (insert-file-contents "pvs-strategies")
      (goto-char (point-max)))
    (when (file-exists-p "~/pvs-strategies")
      (insert (format "\n$$$PVSHOME/pvs-strategies\n"))
      (insert-file-contents "~/pvs-strategies")
      (goto-char (point-max)))
    (when (file-exists-p "~/.pvs.lisp")
      (insert (format "\n$$$PVSHOME/.pvs.lisp\n"))
      (insert-file-contents "~/.pvs.lisp")
      (goto-char (point-max)))
    (dolist (x (get-pvs-file-dependencies pvs-file libraries-p))
      (let ((file (expand-file-name (format "%s.pvs" x))))
	(when (file-exists-p file)
	  (insert (format "\n$$$%s.pvs\n" x))
	  (insert-file-contents file)
	  (goto-char (point-max))
	  (when (file-exists-p (expand-file-name (format "%s.prf" x)))
	    (insert (format "\n$$$%s.prf\n" x))
	    (insert-file-contents (expand-file-name (format "%s.prf" x))))
	  (goto-char (point-max)))))))

(defun insert-pvs-patch-information ()
  (let ((versions (get-pvs-version-information)))
    (unless (eq (cadr versions) 'NIL)
      (insert (format "%%Patch files loaded: patch2 version %s\n"
		  (cadr versions))))
    (unless (eq (caddr versions) 'NIL)
      (insert (format "%%                    patch2-test version %s\n"
		  (caddr versions))))
    (unless (eq (cadddr versions) 'NIL)
      (insert (format "%%                    patch2-exp version %s\n"
		  (cadddr versions))))))

(defpvs undump-pvs-files dump-files (filename directory)
  "Break dump files into separate PVS files.

The undump-pvs-files command performs the inverse of dump-pvs-files.  It
splits the file up into PVS files in the current directory.  Prompts for
the file and the directory to dump it into, and asks for confirmation
prior to overwriting an existing file unless given a command argument
e.g., C-u or M-0."
  (interactive "fUndump file: \nDInto directory: ")
  (let ((buf (find-file-noselect filename))
	(dname (file-name-as-directory directory))
	(pvs-libdirs nil)
	(overwrite nil))
    (unless (file-name-absolute-p dname)
      (error "Directory to undump into must be absolute, not relative"))
    (save-excursion
      (set-buffer buf)
      (setq default-directory dname)
      (goto-char (point-min))
      (let ((found-one (re-search-forward "^[$][$][$].*$" nil t)))
	(while found-one
	  (forward-line)
	  (let* ((fname (buffer-substring (+ (match-beginning 0) 3)
					  (match-end 0)))
		 (ndir (pvs-get-dump-directory (or (file-name-directory fname)
						   dname)))
		 (filename (when ndir
			     (short-file-name
			      (concat (file-name-as-directory ndir)
				      (file-name-nondirectory fname)))))
		 (help-form '(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." filename))
		 (start (point)))
	    (setq found-one (re-search-forward "^[$][$][$].*$" nil t))
	    (cond ((and (stringp ndir)
			(file-directory-p ndir))
		   (pvs-msg "Undumping %s" filename)
		   (let ((end (if found-one
				  (- (match-beginning 0) 1)
				  (point-max)))
			 (data (match-data)))
		     (when (or (and current-prefix-arg t)
			       (not (file-exists-p filename))
			       (pvs-emacs-query
				'overwrite "Overwrite `%s'?" fname))
		       (unwind-protect
			   (write-region start end filename)
			 (store-match-data data)))))
		  ((and (stringp ndir)
			(file-exists-p ndir))
		   (pvs-msg "%s is not a directory!" ndir))
		  (t (pvs-msg "Skipping %s" fname)))))
	(goto-char (point-min)))
      (pvs-bury-output)
      (pvs-msg "Finished undumping %s" filename)
      (kill-buffer buf))))

(defun pvs-get-dump-directory (dir)
  (save-excursion
    (or (cdr (assoc dir pvs-libdirs))
	(let ((ldir dir)
	      (tried nil))
	  (while (and (stringp ldir)
		      (not (and (file-exists-p ldir)
				(file-directory-p ldir))))
	    (cond ((and (not (file-exists-p ldir))
			(directory-writable-p ldir)
			(fboundp 'make-directory)
			(let ((prompt
			       (format
				   "Directory `%s' does not exist: create it?"
				   ldir)))
			  (if (> (length prompt) (window-width))
			      (with-output-to-temp-buffer "undump-pvs-files"
				(princ prompt)
				(yes-or-no-p "Create above directory? "))
			      (yes-or-no-p prompt))))
		   (make-directory ldir t)
		   (pvs-msg (format "Created directory %s" ldir)))
		  ((and (not (file-exists-p ldir))
			(let ((prompt
			       (format
				   "Directory `%s' does not exist: use a different one?"
				   ldir)))
			  (cond ((> (length prompt) (window-width))
				 (with-output-to-temp-buffer "undump-pvs-files"
				   (princ prompt))
				 (prog1 (yes-or-no-p
					 "Use a different directory? ")
				   (pvs-bury-output)))
				(t (prog1
				       (yes-or-no-p prompt)
				     (pvs-bury-output))))))
		   (setq ldir (short-file-name
			       (read-file-name "New directory to use: "
					       (pvs-current-directory)))))
		  ((not (file-exists-p ldir))
		   (pvs-msg (format "Files in library %s will be ignored"
				dir))
		   (setq ldir nil))))
	  (push (cons dir ldir) pvs-libdirs)
	  ldir))))


;;;---------------------------------------------
;;; Context Commands

;;; list-pvs-files and list-theories are in pvs-file-list.el

;;; change-context

(defpvs change-context context (dir)
  "Switch to a new PVS context

The change-context command is similar to the `cd' command in Unix; it
saves the context and changes the working directory to the specified one.
The PVS Welcome buffer is then displayed indicating the new directory.  If
the requested directory does not exist, and the Emacs you are running
supports \texttt{make-directory}, then PVS offers to make a new one,
including parent directories if they do not exist.  If the command fails
for any reason, then the current PVS context is not changed."
  (interactive (let ((cdir (pvs-current-directory t)))
		 (confirm-not-in-checker)
		 (list (read-file-name
			"(Change context to) directory path: " cdir cdir))))
  (unless (file-exists-p dir)
    (if (and (fboundp 'make-directory)
	     (yes-or-no-p (format "%s does not exist - create it? " dir)))
	(make-directory dir t)
	(error "Directory does not exist")))
  (unless (file-directory-p dir)
    (error "Must specify a directory, not a file"))
  (pvs-bury-output)
  (save-some-pvs-files)
  (setq dir (expand-file-name dir))
  (unless (string-match "/$" dir)
    (setq dir (concat dir "/")))
  (if (string= dir *pvs-current-directory*)
      (message "Already in context %s" dir)
      (let ((ndir (pvs-send-and-wait
		   (format "(change-context \"%s\")" dir) nil 'cc 'string)))
	(pvs-remove-old-context-buffers)
	(if (and (stringp ndir)
		 (file-exists-p ndir))
	    (setq *pvs-current-directory* ndir)
	    (pvs-current-directory t))
	;; Update the PVS Welcome buffer (and force the display)
	(unless noninteractive
	  (pvs-welcome t))
	(run-hooks 'change-context-hook))))

(defun pvs-remove-old-context-buffers ()
  (dolist (b (buffer-list))
    (save-excursion
      (set-buffer b)
      (when pvs-context-sensitive
	(set-buffer-modified-p nil)
	(kill-buffer b)))))

(defun init-change-context (dir)
  (setq dir (expand-file-name dir))
  (unless (string-match "/$" dir)
    (setq dir (concat dir "/")))
  (let ((ndir (pvs-send-and-wait (format "(change-context \"%s\")" dir)
				 nil nil 'dont-care)))
    (if (and (stringp ndir)
	     (file-exists-p ndir))
	(setq *pvs-current-directory* ndir)
	(pvs-current-directory t))))


;;; save-context

(defpvs save-context context ()
  "Save the state of the current PVS context

The save-context command saves the state of the current PVS context to the
.pvscontext file in the current directory.  This command is not normally
needed, as PVS automatically saves the context as needed."
  (interactive)
  (message "Processing save-context command...")
  (pvs-send-and-wait "(save-context)" nil
		     (pvs-get-abbreviation 'save-context) 'dont-care)
  (message "Context saved"))


;;; Show pathname of current working directory

(defpvs context-path context ()
  "Show the pathname of the current context

The context-path command displays the current pathname (directory) in the
echo area."
  (interactive)
  (let ((dir (pvs-current-directory)))
    (message "Context Path: %s" (short-file-name dir))))


;;; load-prelude-library

(defpvs load-prelude-library library (dir)
  "Extend the prelude with the specified context

The load-prelude-library command prompts for a context pathname (i.e.,
directory), and extends the prelude with all of the theories that make up
that context.  Note that the theories that make up the context are defined
by the .pvscontext file in the associated directory - there may be
specification files in the same directory that are not a part of the
context.  The files that make up the context are typechecked if necessary,
and internally the prelude is extended.  All of the theories of the
current context are untypechecked, as they may not typecheck the same way
in the extended prelude.  The PVS context is updated to reflect that the
prelude has been extended.  Thus the next time this context is entered,
the prelude will automatically be extended (by typechecking the libraries
if necessary)."
  (interactive (let ((cdir (pvs-current-directory t)))
		 (list (read-file-name
			"(Load prelude library from) directory path: "
			cdir nil t))))
  (let ((default-directory (pvs-current-directory t)))
    (unless (file-directory-p dir)
      (error "Must specify an existing directory"))
    (confirm-not-in-checker)
    (pvs-bury-output)
    (save-some-pvs-files)
    (unless (string-match "/$" dir)
      (setq dir (concat dir "/")))
    (if (file-equal dir *pvs-current-directory*)
	(message "Cannot use current context as a prelude")
	(pvs-send (format "(load-prelude-library \"%s\")" dir)))))

(defpvs remove-prelude-library library (dir)
  "Remove the specified context from the prelude

The remove-prelude-library command removes the specified library from the
prelude.  It reverts all the theories of the context to untypechecked to
guarantee that no theories depend on the removed library.  Note that the
built-in prelude may not be removed this way."
  (interactive (pvs-complete-prelude-library))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-files)
  (setq dir (expand-file-name dir))
  (unless (string-match "/$" dir)
    (setq dir (concat dir "/")))
  (pvs-send (format "(remove-prelude-library \"%s\")" dir)))


(defun pvs-complete-prelude-library ()
  (let ((prelude-libs (pvs-send-and-wait "(prelude-libraries)"
					 nil nil 'list)))
    (if (null prelude-libs)
	(error "There are no prelude libraries in this context")
	(list (completing-read "Remove library: "
		(mapcar 'list prelude-libs) nil 't nil)))))


;;;---------------------------------------------
;;; ENVIRONMENT COMMANDS

;;; help-pvs

(defpvs help-pvs help ()
  "Display help for the PVS system commands

The help-pvs command displays a summary of PVS commands in the PVS Help
buffer.  Help may be obtained for an individual command by typing C-h f
followed by the command or its abbreviation, or by typing C-h k followed
by the key sequence that invokes the command."
  (interactive)
  (let ((buf (get-buffer-create "PVS Help")))
    (set-buffer buf)
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert-file-contents (concat pvs-path "/lib/pvs.help"))
    (goto-char (point-min))
    (unless buffer-read-only (toggle-read-only))
    (pop-to-buffer buf)
    (pvs-view-mode)))

(defpvs help-pvs-language help ()
  "Display help for the PVS language.

Provides an example specification, along with the PVS grammar."
  (interactive)
  (let ((buf (get-buffer-create "Language Help")))
    (set-buffer buf)
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert-file-contents (concat pvs-path "/lib/pvs-language.help"))
    (goto-char (point-min))
    (unless buffer-read-only (toggle-read-only))
    (pop-to-buffer buf)
    (pvs-view-mode)))

(defpvs help-pvs-bnf help ()
  "Display the PVS language grammar."
  (interactive)
  (let ((buf (get-buffer-create "PVS bnf")))
    (set-buffer buf)
    (if buffer-read-only (toggle-read-only))
    (erase-buffer)
    (insert-file-contents (concat pvs-path "/lib/pvs.bnf"))
    (goto-char (point-min))
    (unless buffer-read-only (toggle-read-only))
    (pop-to-buffer buf)
    (pvs-view-mode)))
  

;;; suspend-pvs

(defpvs suspend-pvs exiting ()
  "Suspend PVS.

Suspends the PVS system, prompting whether to save the context.
Has no effect under X windows."
  (interactive)
  (if (eq window-system 'x)
      (error "Cannot suspend Emacs running in X-windows")
      (when (y-or-n-p "Save context first? ")
	(save-context))
      (if (getenv "IN_EMACSTOOL")		; Set by emacstool
	  (suspend-emacstool)
	  (suspend-emacs))))

;;; exit-pvs

(defpvs exit-pvs exiting ()
  "Terminate the PVS session.

Exit PVS, saving the context."
  (interactive)
  (cond ((and ilisp-buffer
	      (get-buffer ilisp-buffer)
	      (ilisp-process)
	      (eq (process-status (ilisp-process)) 'run))
	 (confirm-not-in-checker)
	 (when (or noninteractive
		   (y-or-n-p "Do you want to exit PVS? "))
	   (when (and ilisp-buffer (get-buffer ilisp-buffer))
	     (let ((process (ilisp-process)))
	       (when (and process (equal (process-status process) 'run))
		 (save-some-buffers nil t)
		 (pvs-send "(exit-pvs)")
		 (while (equal (process-status process) 'run)
		   (sleep-for 1)))))
	   (save-buffers-kill-emacs nil)))
	(t (save-buffers-kill-emacs nil))))

(defpvs pvs-log environment ()
  "Display the PVS Log buffer

The pvs-log command pops up the PVS Log buffer, which is used to log
timestamped messages from PVS."
  (interactive)
  (pop-to-buffer "PVS Log"))


;;; remove-popup-buffer

;;(fset 'remove-popup-buffer 'popper-bury-output)


;;; pvs-version

(defpvs pvs-version environment ()
  "Display current version of PVS and underlying Lisp

The pvs-version command displays the current-version of PVS and the
underlying Lisp in the minibuffer."
  (interactive)
  (let ((vers (get-pvs-version-information)))
    (message "%s - %s %s"
	     (pvs-version-string) (cadr (cdddr vers)) (cadr (cddddr vers)))))

(defun pvs-version-string ()
  (let ((vers (get-pvs-version-information)))
    (format "PVS Version %s (%s)"
	(car vers)
      (if (and (eq (cadr vers) 'NIL)
	       (eq (caddr vers) 'NIL)
	       (eq (cadddr vers) 'NIL))
	  "No patches loaded"
	  (format "patch level %s%s%s"
	      (if (eq (cadr vers) 'NIL)
		  ""
		  (format "%s" (cadr vers)))
	      (if (eq (caddr vers) 'NIL)
		  ""
		  (format ", %s" (caddr vers)))
	      (if (eq (cadddr vers) 'NIL)
		  ""
		  (format ", %s" (cadddr vers))))))))

(defvar *pvs-version-information* nil)

(defun get-pvs-version-information ()
  (or *pvs-version-information*
      (let ((vlist (pvs-send-and-wait
		    "(list *pvs-version*
                           (get-patch-version)
                           (when (fboundp 'get-patch-test-version)
                             (get-patch-test-version))
                           (when (fboundp 'get-patch-exp-version)
                             (get-patch-exp-version))
                           (lisp-implementation-type)
                           (lisp-implementation-version))"
		    nil nil 'list)))
	(if (consp vlist)
	    (setq *pvs-version-information* vlist)
	    (comint-log (ilisp-process) (format "version = {%s}" vlist))
	    (get-pvs-version-information)))))

(defun pvs-major-version-number ()
  (if *pvs-version-information*
      (string-to-int (car *pvs-version-information*))
      (string-to-int (pvs-send-and-wait "*pvs-version*" nil nil))))

;;; Replay

;;; Save PVS Buffer

(defpvs save-pvs-buffer save-files (filename)
  "Saves current buffer (modified or not) into FILENAME

The save-pvs-buffer command copies the contents of the current buffer to
the specified file name, without renaming the buffer.  This command should
be used instead of the Emacs write-file command, which does rename the
buffer."
  (interactive "FSave PVS buffer into file name: ")
  (write-region (point-min) (point-max) filename nil nil))


;;;---------------------------------------------
;;; STATUS COMMANDS

(defpvs status-theory theory-status (theory)
  "Gives a one-line description of the status of the specified theory

The status-theory command provides a description of the status of the
specified theory in the minibuffer."
  (interactive (complete-theory-name "Status of theory named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(status-theory \"%s\")" theory) nil
		     (pvs-get-abbreviation 'status-theory)
		     'dont-care))


(defpvs status-pvs-file theory-status (filename)
  "Displays the status of the theories in the specified file in the
PVS Status buffer

The status-pvs-file command provides a description of the status of each
theory within the PVS file in the PVS Status buffer."
  (interactive (complete-pvs-file-name "Status of PVS file named: "))
  (pvs-bury-output)
  (pvs-send-and-wait (format "(status-pvs-file \"%s\")" filename)
		     nil (pvs-get-abbreviation 'status-pvs-file)
		     'dont-care))

(defpvs status-importchain theory-status (theory)
  "Displays the status of the theories in the import chain of the
specified theory

The status-importchain command provides description of the status of each
theory within the importchain of the specified theory in the PVS Status
buffer."
  (interactive (complete-theory-name "Importchain for theory named: "))
  (pvs-send (format "(status-importchain \"%s\")" theory) nil
	    (pvs-get-abbreviation 'show-importchain)))

(defpvs status-importbychain theory-status (theory)
  "Displays the status of the closure of the theories which import the
specified theory

The status-importchain command provides description of the status of each
theory which imports the specified theory in the current context in the
PVS Status buffer."
  (interactive (complete-theory-name "Importbychain for theory named: "))
  (pvs-send (format "(status-importbychain \"%s\")" theory) nil
	    (pvs-get-abbreviation 'show-importbychain)))


(defpvs status-proof proof-status ()
  "Display the status of the proof at the cursor

The status-proof command uses the minibuffer to display the proof status
of the formula at the cursor.  The status can be one of proved, untried,
unfinished, or unchecked.  Untried means that the proof hash not yet been
attempted.  Unfinished means that the proof has been attempted, but is not
complete.  Unchecked means that the proof was successful at one point, but
that some changes have been made that may invalidate the proof."
  (interactive)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (filename (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (save-some-pvs-buffers)
    (pvs-send (format "(proof-status-at \"%s\" %d \"%s\")"
		       filename
		(+ (current-line-number)
		   (if (equal origin "prelude")
		       pvs-prelude 0))
		origin)
	       nil (pvs-get-abbreviation 'status-proof))))


(defpvs status-proof-theory proof-status (theoryname)
  "Returns a summary of the status of the proofs in the current theory

The status-proof-theory command provides a summary of the status of the
proofs in the current theory in the PVS Status buffer."
  (interactive (complete-theory-name "Proof status for theory named: "))
  (pvs-send-and-wait (format "(status-proof-theory \"%s\")" theoryname)
		     nil (pvs-get-abbreviation 'status-proof-theory)
		     'dont-care))

(defpvs status-proof-pvs-file proof-status (filename)
  "Returns a summary of the status of the proofs in the current file

The status-proof-pvs-file command provides a summary of the status of the
proofs in the current PVS file in the PVS Status buffer."
  (interactive (list (current-pvs-file)))
  (pvs-send-and-wait (format "(status-proof-pvs-file \"%s\")" filename)
		     nil (pvs-get-abbreviation 'status-proof-pvs-file)
		     'dont-care))

(defpvs status-proof-importchain proof-status (theoryname)
  "Return proof status for theories in IMPORT chain

The status-proof-importchain command provides a summary of the status of
the proofs of the importchain of the current theory in the PVS Status
buffer."
  (interactive (complete-theory-name
		"Proof status for import chain of theory named: "))
  (pvs-send-and-wait (format "(status-proof-importchain \"%s\")" theoryname)
		     nil (pvs-get-abbreviation 'status-proof-importchain)
		     'dont-care))


(defpvs status-proofchain proof-status ()
  "Analyzes the proof chain of the proof closest to (moving forward) the
cursor

The status-proofchain command provides a proof chain analysis of the
formula at the cursor and displays it in the PVS Status buffer.  The
proof chain analysis indicates whether the formula has been proved, and
analyses the formulas used in the proof to insure that the proof is
complete; lemmas used in the proof are proved, and sound, i.e. there are
no circularities."
  (interactive)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (filename (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (save-some-pvs-buffers)
    (pvs-send-and-wait (format "(proofchain-status-at \"%s\" %d \"%s\")"
			   filename
			 (+ (current-line-number)
			    (if (equal origin "prelude")
				pvs-prelude 0))
			 origin)
		nil (pvs-get-abbreviation 'status-proofchain)
		'dont-care)))

(defpvs status-proofchain-theory proof-status (theoryname)
  "Returns a summary of the proofchains of the formulas in the current theory

The status-proofchain-theory command provide a proofchain analysis for
each formula of the specified theory in the PVS Status buffer."
  (interactive (complete-theory-name "Proofchain status for theory named: "))
  (pvs-send-and-wait (format "(status-proofchain-theory \"%s\")"
			 theoryname)
		     nil (pvs-get-abbreviation 'status-proofchain-theory)
		     'dont-care))

(defpvs status-proofchain-pvs-file proof-status (filename)
  "Returns a summary of the status of the proofchain of all formulas in the
current file

The status-proofchain-pvs-file command provide a proofchain analysis for
each formula of the specified PVS file in the PVS Status buffer."
  (interactive (list (current-pvs-file)))
  (pvs-send-and-wait
   (format "(status-proofchain-pvs-file \"%s\")" filename)
   nil (pvs-get-abbreviation 'status-proofchain-pvs-file)
   'dont-care))

(defpvs status-proofchain-importchain proof-status (theoryname)
  "Return proofchain status for theories in IMPORT chain

The status-proofchain-importchain command provide a proofchain analysis
for each formula of the import chain of the specified theory in the PVS
Status buffer."
  (interactive (complete-theory-name
		"Proofchain status for import chain of theory named: "))
  (pvs-send (format "(status-proofchain-importchain \"%s\")"
		theoryname)
	     nil (pvs-get-abbreviation 'status-proofchain-importchain)))

(defpvs whereis-pvs environment ()
  "Display the location of the PVS system

The whereis-pvs command is used to determine the directory where the PVS
system resides along with example specifications and files which may be
copied and modified to customize the PVS environment."
  (interactive)
  (message "The PVS system is in %s" pvs-path))

(defpvs pvs-load-patches environment ()
  "Load the PVS patches

The pvs-load-patches command may be used to load any patches available t
your site.  Normally this command is not needed, as the patches are
automatically loaded on startup.  This command is used if new patches have
been obtained since starting your session and you want to load them
without restarting."
  (interactive)
  (if (< (pvs-major-version-number) 2)
      (load (format "%s/patch" pvs-path) t t)
      (load (format "%s/patch%d" pvs-path (pvs-major-version-number)) t t))
  (pvs-send-and-wait "(load-pvs-patches)" "Loading PVS patches" 'patch
		     'dont-care)
  (message "PVS patches loaded"))

(defpvs status-display environment ()
  "Display PVS Status buffer in another window

The status-display command displays the PVS Status buffer.  This is used
to make the status buffer visible again when it has been buried by
processing other commands."
  (interactive)
  (if (get-buffer "PVS Status")
      (switch-to-buffer-other-window "PVS Status")
      (error "The PVS Status buffer does not exist")))

(defun change-declaration-name (from-name to-id root-theory)
  "Changes the specified declaration name from from-name to to-name"
  (interactive (list (complete-declaration-name "Change name: ")
		     (complete-symbol "To id: ")
		     (complete-theory-name "Root Theory: ")))
  (let ((change-info (pvs-send-file-and-wait
		      (format "(change-declaration-name \"%s\" \"%s\" \"%s\")"
			  from-name to-id root-theory)
		      nil nil 'list)))
    (when change-info
      (pop-to-buffer "Change Info")
      (insert change-info)
      (goto-char (point-min))
      (toggle-read-only))))

(defpvs x-theory-hierarchy x-display (theoryname)
  "Show the theory hierarchy starting from THEORYNAME

The x-theory-hierarchy command prompts for a theory name and displays the
IMPORTING hierarchy rooted at that theory.  The display uses Tcl/Tk, click
on the Help button at the bottom of the display for more information."
  (interactive (complete-theory-name "Show theory hierarchy from theory:"))
  (if (getenv "DISPLAY")
      (pvs-send (format "(x-module-hierarchy \"%s\")" theoryname))
      (message
       "DISPLAY variable not set, cannot popup theory hierarchy display")))

(defpvs pvs-dont-write-bin-files context ()
  "Inhibit writing or loading of .bin files

The pvs-dont-write-bin-files command inhibits the writing of .bin files.
These files can greatly speed up retypechecking, especially for library
theories.  However, they do use up a fair amount of disk space, and if
that is a problem you can use this command.  In that case it is probably
best to add it to your ~/.pvsemacs file."
  (interactive)
  (pvs-send-and-wait "(setq *dont-write-object-files* t)"
		     nil nil 'dont-care))

(defpvs pvs-do-write-bin-files context ()
  "Allow writing or loading of .bin files

The pvs-do-write-bin-files is used to allow the writing and loading of
.bin files. This is only needed if the pvs-dont-write-bin-files command
was issued, either directly or through the ~/.pvsemacs or ~/.emacs files."
  (interactive)
  (pvs-send-and-wait "(setq *dont-write-object-files* nil)"
		     nil nil 'dont-care))

(defpvs pvs-remove-bin-files context ()
  "Remove all .bin files in the current context

The pvs-remove-bin-files command removes all the .bin files of the current
context."
  (interactive)
  (let ((files (directory-files *pvs-current-directory* t "\.bin$")))
    (when noninteractive
      (princ "Removing all .bin files\n")
      (princ "Removing all .bin files\n" 'external-debugging-output))
    (if files
	(when (or noninteractive
		  (y-or-n-p "Remove all .bin files? "))
	  (dolist (file files)
	    (delete-file file))
	  (when noninteractive
	    (princ (format "%d files removed\n" (length files))))
	  (message "%d files removed" (length files)))
	(when noninteractive
	  (princ "No bin files found\n"))
	(message "No bin files found"))))
	

(defun pvs-find-lisp-file (filename)
  (interactive (list (pvs-complete-lisp-file-name)))
  (find-file filename))

(defun pvs-complete-lisp-file-name ()
  (let ((files-alist nil))
    (dolist (dir (list "src" "src/prover" "src/utils" "ground-prover" "MU" ""))
      (setq files-alist
	    (nconc files-alist
		   (pairlis (directory-files (format "%s/%s" pvs-path dir)
					     nil ".*\.lisp$")
			    (directory-files (format "%s/%s" pvs-path dir)
					     t ".*\.lisp$")))))
    (let ((file (completing-read "Find lisp file: " files-alist)))
      (cdr (assoc file files-alist)))))

(defun typecheck-formula (formula-decl &optional theory-name)
  (let ((*pvs-error* nil)
	(tname (or theory-name (current-theory))))
    (pvs-bury-output)
    (pvs-busy)
    (pvs-send-and-wait (format "(typecheck-formula-decl \"%s\" %s)"
			   formula-decl (when tname (format "\"%s\"" tname)))
		       nil nil 'dont-care)
    (pvs-ready)
    (not *pvs-error*)))

(defun prove-formula (formula-decl &optional theory-name proof)
  (when (typecheck-formula formula-decl theory-name)
    (let ((tname (or theory-name (current-theory))))
      (pvs-busy)
      (ilisp-send (format "(prove-formula-decl \"%s\" %s %s)"
		      formula-decl
		    (when tname (format "\"%s\"" tname))
		    (when proof (format "'%S" proof)))
		  nil 'pr t))))

(defun get-prove-formula-proof ()
  (pvs-file-send-and-wait "(get-prove-formula-proof)" nil nil 'list))

(defvar pvs-busy-hook nil)
(defvar pvs-ready-hook nil)

(defun pvs-busy ()
  (pushw)
  (run-hooks 'pvs-busy-hook))

(defun pvs-ready ()
  (popw)
  (run-hooks 'pvs-ready-hook))

;(add-hook 'pvs-busy-hook
;  (function (lambda ()
;	      (ensure-pvs-wish)
;	      (tcl-eval "pvs-busy"))))
;
;(add-hook 'pvs-ready-hook
;  (function (lambda ()
;	      (ensure-pvs-wish)
;	      (tcl-eval "pvs-ready"))))

(defun pvs-garbage-collect ()
  "Forces PVS to perform a global garbage collection.
With an argument, only performs a scavenge and not a full gc."
  (interactive)
  (message "Forcing garbage collection")
  (pvs-send-and-wait
   (format "(excl:gc %s)" (and current-prefix-arg t))
   nil nil 'dont-care)
  (message "Garbage collection completed"))


