;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-prover.el -- 
;; Author          : Sam Owre
;; Created On      : Fri Nov 12 11:49:20 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Oct 15 02:33:07 1995
;; Update Count    : 21
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; prove	        - prove-file-at
;; redo-proof           - prove-file-at
;; prove-theory         - prove-theory
;; prove-pvs-file       - prove-pvs-file
;; prove-importchain    - prove-usingchain
;; prove-proofchain     - prove-proofchain
;; edit-proof           - edit-proof
;; install-proof        -
;; remove-proof         - remove-proof-at
;; install-pvs-proof-file -
;; show-proof-file      -
;; show-orphaned-proofs -
;; show-proofs-pvs-file -
;; add-declaration      -
;; modify-declaration   -
;; ancestry             -
;; siblings             -
;; show-proof           -
;; x-show-proof		-
;; show-hidden-formulas -
;; help-pvs-prover      -
;; show-auto-rewrites   -
;; show-last-proof      -
;; show-expanded-sequent-
;; load-pvs-strategies  -
;; explain-tcc          -
;; set-print-depth      -
;; set-print-length	-
;; pvs-get-prove-input	-
;; step-proof           -

(eval-when-compile (require 'pvs-macros))

(defvar pvs-in-checker nil
  "Indicates whether the proof checker is currently running.
This is set and unset in prove-decl.")

(defvar pvs-x-show-proofs nil
  "Set to t to always invoke x-show-proofs for M-x prove")

;;; Proof commands

(defpvs prove prove (&optional rerun)
  "Invokes the prover on the formula closest to the current cursor position

The prove command invokes the prover on the next formula at or beyond the
current cursor position.  If the formula is already proved, you will be
asked whether to continue.  If the formula already has a proof, you will
be asked whether to go ahead and run it or to start anew.  Note that
starting a new proof will not delete the old proof unless you allow the
prover to overwrite it at the end of the proof session."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (let ((*pvs-error* nil))
      (cond ((equal origin "pvs")
	     (save-some-pvs-buffers)
	     (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
				    name)
				nil 'tc 'dont-care))
	    ((member origin '("ppe" "tccs"))
	     (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" name)
					nil 'tc nil)
	       (error "%s is not typechecked" name))))
      (unless *pvs-error*
	(pvs-prove-formula name origin rerun nil pvs-x-show-proofs)
	(unless *pvs-error*
	  (switch-to-lisp t t))))))

(defpvs x-prove prove (&optional rerun)
  "Invokes the prover, with display, on the formula closest to the cursor

The x-prove command invokes the prover on the next formula at or beyond
the current cursor position, and starts up the proof display - see
x-show-current-proof."
  (interactive)
  (cond ((getenv "DISPLAY")
	 (let ((pvs-x-show-proofs t))
	   (prove rerun)))
	(t (message "DISPLAY variable not set, cannot popup proof display")
	   (beep)
	   (sleep-for 1)
	   (prove rerun))))

(defpvs redo-proof prove ()
  "Redo the proof of the formula at the cursor

The redo-prove command invokes the prover on the formula at or beyond the
current cursor position.  It starts the proof whether or not the formula
has already been proved, and automatically runs any proof associated with
the formula.  With an argument, runs the proof in the background."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (let ((*pvs-error* nil))
      (cond ((equal origin "pvs")
	     (save-some-pvs-buffers)
	     (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
				    name)
				nil 'tc 'dont-care))
	    ((member origin '("ppe" "tccs"))
	     (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" name)
					nil 'tc nil)
	       (error "%s is not typechecked" name))))
      (unless *pvs-error*
	(pvs-prove-formula name origin 'T (and current-prefix-arg t)
			   pvs-x-show-proofs)))))


;;; This is the function that proves the formula at the cursor.  The
;;; origin indicates the kind of the current buffer, and is one of pvs,
;;; prelude, prelude-theory, tccs, or ppe.  The name is generally the
;;; buffer name without the extension.  When the proof is done, the system
;;; will attempt to get back to the beginning of the formula that was
;;; attempted.

(defun pvs-prove-formula (name origin &optional rerun-proof background display)
  (let* ((prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	 (line (+ (current-line-number) prelude-offset))
	 (rerun (pvs-send-and-wait
		 (format "(rerun-proof-at? \"%s\" %d \"%s\" %s)"
		     name line origin rerun-proof)
		 nil nil "T\\|NIL")))
    (if rerun
	(ilisp-send
	 (format "(prove-file-at \"%s\" %d %s \"%s\" \"%s\" %d %s %s)"
	     name line (eq rerun 'T) origin (buffer-name)
	     prelude-offset background display)
	 nil 'pr (not background))
	(setq *pvs-error* t))))

(defpvs prove-next-unproved-formula prove ()
  "Invokes the prover on the next unproved formula at or beyond the
current cursor position.  If the formula already has a proof, you will
be asked whether to go ahead and run it or to start anew.  Note that
starting a new proof will not delete the old proof unless you allow the
prover to overwrite it at the end of the proof session."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin))
	 (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	 (line (+ (current-line-number) prelude-offset)))
    (let ((*pvs-error* nil))
      (cond ((equal origin "pvs")
	     (save-some-pvs-buffers)
	     (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
				    name)
				nil 'tc 'dont-care))
	    ((member origin '("ppe" "tccs"))
	     (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" name)
					nil 'tc nil)
	       (error "%s is not typechecked" name))))
      (unless *pvs-error*
	(ilisp-send
	 (format "(prove-next-unproved-formula \"%s\" %d %s \"%s\" \"%s\" %d)"
	     name line (and current-prefix-arg t) origin (buffer-name)
	     prelude-offset)
	 nil 'pru t)
	(unless *pvs-error*
	  (switch-to-lisp t t))))))

(defpvs prove-theory prove (theory)
  "Attempt to prove all the formulas of a theory.

Attempts all unproved formulas in the specified theory which have an
associated proof script.  With an argument (e.g., C-u or M-0) will rerun
all proof scripts, including those already proved."
  (interactive (complete-theory-name "Prove theory named: "))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  ;;; (pvs-bury-output)
  (pvs-send (format "(prove-theory \"%s\" %s %s)"
		theory (and current-prefix-arg t)
		(when (equal theory (current-theory))
		  (format "\"%s\"" (current-pvs-file t))))
	    nil 'prt))

(defpvs prove-theories prove (&rest theories)
  "Attempt to prove all formulas in each of the THEORIES

With an argument (e.g., C-u or M-0) will rerun all proof scripts,
including those already proved."
  (interactive (complete-theory-name-list "Theory: "))
  (if (null theories)
      (message "No theories given")
      (confirm-not-in-checker)
      (save-some-pvs-buffers)
      (pvs-send (format "(prove-pvs-theories '%s %s)"
		    (mapcar '(lambda (x) (format "\"%s\"" x)) theories)
		  (and current-prefix-arg t))
		nil 'prts)))

(defpvs prove-pvs-file prove (file)
  "Attempt to prove all the formulas in the specified PVS file.

Attempts all unproved formulas in the specified PVS file which have an
associated proof script.  With an argument (e.g., C-u or M-0) will rerun
all proof scripts, including those already proved."
  (interactive (complete-pvs-file-name "Prove PVS file named: "))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-pvs-file \"%s\" %s)"
		file (and current-prefix-arg t))
	    nil 'prf))


(defpvs prove-importchain prove (theory)
  "Attempt to prove all formulas in the import chain of the theory.

Attempt to prove all unproved formulas in the theories of the transitive
closure of the IMPORTs of the specified theory.  With an argument (e.g.,
C-u or M-0) will rerun all proof scripts, including those already
proved."
  (interactive (complete-theory-name
		"Prove theories in IMPORT of theory named: "))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-usingchain \"%s\" %s)"
		theory (and current-prefix-arg t))
	    nil 'pri))

(defpvs prove-importchain-subtree prove (theory &rest exclude)
  "Attempt to prove all formulas in the import chain of the theory,
excluding those in the EXCLUDE list.

Attempt to prove all unproved formulas in the theories of the transitive
closure of the IMPORTs of the specified theory.  With an argument (e.g.,
C-u or M-0) will rerun all proof scripts, including those already
proved."
  (interactive (append (complete-theory-name
			"Prove theories in IMPORT of theory named: ")
		       (complete-theory-name-list "Theory to exclude: ")))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-usingchain \"%s\" %s '%s)"
		theory (and current-prefix-arg t)
		(mapcar '(lambda (x) (format "\"%s\"" x)) exclude))
	    nil 'pri))


(defpvs prove-proofchain prove ()
  "Attempt to prove all formulas in the proof chain of the formula.

Attempt to prove all unproved formulas in the proof chain of the
specified formula.  With an argument (e.g., C-u or M-0) will rerun all
proof scripts, including those already proved."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (pvs-send (format "(prove-proofchain \"%s\" %d '%s %s)"
		  name (+ (current-line-number)
			  (if (equal origin "prelude")
			      (or pvs-prelude 0) 0))
		  origin (and current-prefix-arg t))
	      nil (pvs-get-abbreviation 'prove-proofchain))))

(defpvs prove-theory-using-default-dp prove (theory)
  "Attempt to prove all the formulas of a theory, using the current default
decision procedures (see new-decision-procedures, old-decision-procedures).

Attempts all unproved formulas in the specified theory which have an
associated proof script.  With an argument (e.g., C-u or M-0) will rerun
all proof scripts, including those already proved."
  (interactive (complete-theory-name "Prove theory named: "))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  ;;; (pvs-bury-output)
  (pvs-send (format "(prove-theory \"%s\" %s %s t)"
		theory (and current-prefix-arg t)
		(when (equal theory (current-theory))
		  (format "\"%s\"" (current-pvs-file t))))
	    nil 'prt))

(defpvs prove-theories-using-default-dp prove (&rest theories)
  "Attempt to prove all formulas in each of the THEORIES

With an argument (e.g., C-u or M-0) will rerun all proof scripts,
including those already proved."
  (interactive (complete-theory-name-list "Theory: "))
  (if (null theories)
      (message "No theories given")
      (confirm-not-in-checker)
      (save-some-pvs-buffers)
      (pvs-send (format "(prove-pvs-theories '%s %s t)"
		    (mapcar '(lambda (x) (format "\"%s\"" x)) theories)
		  (and current-prefix-arg t))
		nil 'prts)))

(defpvs prove-pvs-file-using-default-dp prove (file)
  "Attempt to prove all the formulas in the specified PVS file.

Attempts all unproved formulas in the specified PVS file which have an
associated proof script.  With an argument (e.g., C-u or M-0) will rerun
all proof scripts, including those already proved."
  (interactive (complete-pvs-file-name "Prove PVS file named: "))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-pvs-file \"%s\" %s t)"
		file (and current-prefix-arg t))
	    nil 'prf))


(defpvs prove-importchain-using-default-dp prove (theory)
  "Attempt to prove all formulas in the import chain of the theory.

Attempt to prove all unproved formulas in the theories of the transitive
closure of the IMPORTs of the specified theory.  With an argument (e.g.,
C-u or M-0) will rerun all proof scripts, including those already
proved."
  (interactive (complete-theory-name
		"Prove theories in IMPORT of theory named: "))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-usingchain \"%s\" %s nil t)"
		theory (and current-prefix-arg t))
	    nil 'pri))

(defpvs prove-importchain-subtree-using-default-dp prove (theory &rest exclude)
  "Attempt to prove all formulas in the import chain of the theory,
excluding those in the EXCLUDE list.

Attempt to prove all unproved formulas in the theories of the transitive
closure of the IMPORTs of the specified theory.  With an argument (e.g.,
C-u or M-0) will rerun all proof scripts, including those already
proved."
  (interactive (append (complete-theory-name
			"Prove theories in IMPORT of theory named: ")
		       (complete-theory-name-list "Theory to exclude: ")))
  (confirm-not-in-checker)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-usingchain \"%s\" %s '%s t)"
		theory (and current-prefix-arg t)
		(mapcar '(lambda (x) (format "\"%s\"" x)) exclude))
	    nil 'pri))

(defpvs prove-proofchain-using-default-dp prove ()
  "Attempt to prove all formulas in the proof chain of the formula.

Attempt to prove all unproved formulas in the proof chain of the
specified formula.  With an argument (e.g., C-u or M-0) will rerun all
proof scripts, including those already proved."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (pvs-send (format "(prove-proofchain \"%s\" %d '%s %s t)"
		  name (+ (current-line-number)
			  (if (equal origin "prelude")
			      (or pvs-prelude 0) 0))
		  origin (and current-prefix-arg t))
	      nil (pvs-get-abbreviation 'prove-proofchain))))

;;; pvs-formula-origin returns a list containing a name and the kind of
;;; buffer involved.  The name is the buffer file name (without the
;;; extension), and the kind is one of prelude-theory, prelude, pvs, ppe,
;;; or tccs.

(defun pvs-formula-origin ()
  (let ((file (current-pvs-file t))
	(ext (pathname-type (buffer-name))))
    (cond ((file-equal (format "%s/lib/prelude.pvs" pvs-path)
		       (buffer-file-name))
	   (list (pathname-name (buffer-name)) "prelude"))
	  (pvs-prelude
	   (list (pathname-name (buffer-name)) "prelude-theory"))
	  (file
	   (list file "pvs"))
	  ((member-equal ext '("ppe" "tccs"))
	   (list (pathname-name (buffer-name)) ext))
	  ((equal ext "pvs")
	   (error "File is not in the current context"))
	  (t (error "Buffer must end in .pvs, .ppe, or .tccs, or come from the prelude")))))


(defvar pvs-valid-formula-buffer 'unbound)
(make-variable-buffer-local 'pvs-valid-formula-buffer)

(defun pvs-valid-formula-buffer ()
  (if (eq pvs-valid-formula-buffer 'unbound)
      (let ((file (current-pvs-file t))
	    (ext (pathname-type (buffer-name))))
	(setq pvs-valid-formula-buffer
	      (or pvs-prelude
		  (file-equal (format "%s/lib/prelude.pvs" pvs-path)
			      (buffer-file-name))
		  file
		  (member-equal ext '("ppe" "tccs")))))
      pvs-valid-formula-buffer))

;;; Editing proofs

(defvar edit-proof-mode-map nil)
(if edit-proof-mode-map ()
    (setq edit-proof-mode-map (copy-keymap lisp-mode-map))
    (define-key edit-proof-mode-map "\C-c\C-i" 'install-proof)
    (define-key edit-proof-mode-map "\C-c\C-c" 'install-proof)
    (define-key edit-proof-mode-map "\C-x\C-s" 'install-proof)
    (define-key edit-proof-mode-map "\C-cs" 'install-and-step-proof)
    (define-key edit-proof-mode-map "\C-cx" 'install-and-x-step-proof)
    (define-key edit-proof-mode-map "\C-c\C-q"
      '(lambda () (remove-buffer (current-buffer))))
    (define-key edit-proof-mode-map "\C-cq"
      '(lambda () (remove-buffer (current-buffer))))
    ;; Undefine some of the lisp keys
    (define-key edit-proof-mode-map "\C-]" nil)
    (define-key edit-proof-mode-map "]" 'self-insert-command)
    (define-key edit-proof-mode-map "\M-," 'find-declaration)
    (define-key edit-proof-mode-map "\M-;" 'whereis-declaration-used)
    (define-key edit-proof-mode-map "\M-:" 'list-declarations)
    (define-key edit-proof-mode-map "\C-c\C-l" nil)
    (define-key edit-proof-mode-map "\C-zk" nil)
    (define-key edit-proof-mode-map "\C-zl" nil)
    (define-key edit-proof-mode-map "\C-zS" nil)
    (define-key edit-proof-mode-map "\C-zD" nil)
    (define-key edit-proof-mode-map "\C-zA" nil)
    (define-key edit-proof-mode-map "\C-zs" nil)
    (define-key edit-proof-mode-map "\C-zy" nil)
    (define-key edit-proof-mode-map "\C-zb" nil)
    (define-key edit-proof-mode-map "\C-z*" nil)
    (define-key edit-proof-mode-map "\C-z " nil)
    (define-key edit-proof-mode-map "\C-z!" nil)
    (define-key edit-proof-mode-map "\C-zt" nil)
    (define-key edit-proof-mode-map "\C-z\C-w" nil)
    (define-key edit-proof-mode-map "\C-z\C-n" nil)
    (define-key edit-proof-mode-map "\C-z\C-e" nil)
    (define-key edit-proof-mode-map "\C-z\C-r" nil)
    (define-key edit-proof-mode-map "\C-zc" nil)
    (define-key edit-proof-mode-map "\C-zw" nil)
    (define-key edit-proof-mode-map "\C-zP" nil)
    (define-key edit-proof-mode-map "\C-zp" nil)
    (define-key edit-proof-mode-map "\C-zn" nil)
    (define-key edit-proof-mode-map "\C-ze" nil)
    (define-key edit-proof-mode-map "\C-zr" nil)
    (define-key edit-proof-mode-map "\C-z^" nil)
    (define-key edit-proof-mode-map "\C-zM" nil)
    (define-key edit-proof-mode-map "\C-zm" nil)
    (define-key edit-proof-mode-map "\C-zd" nil)
    (define-key edit-proof-mode-map "\C-za" nil)
    (define-key edit-proof-mode-map "\C-zI" nil)
    (define-key edit-proof-mode-map "\C-zi" nil)
    (define-key edit-proof-mode-map "\C-z\C-c" nil)
    (define-key edit-proof-mode-map "\M-\C-x" nil)
    ;; C-c C-c, C-c C-i, C-c C-q, C-c q are already taken
    (define-key edit-proof-mode-map "\C-ca" 'add-proof-checkpoint)
    (define-key edit-proof-mode-map "\C-cp" 'install-proof-checkpoint)
    (define-key edit-proof-mode-map "\C-cr" 'remove-proof-checkpoint)
    (define-key edit-proof-mode-map "\C-c\177" 'remove-all-proof-checkpoints))
    

(defun edit-proof-mode ()
  "Mode for the \"Proof\" buffer, used for editing and installing proofs

The \"Proof\" buffer is used to edit proofs and install them on formulas.
In addition to the following, all of the browsing and prover emacs (TAB)
commands are available.
\\<edit-proof-mode-map>
  install-proof (\\[install-proof])
  install-and-step-proof (\\[install-and-step-proof])
  install-and-x-step-proof (\\[install-and-x-step-proof])
  add-proof-checkpoint (\\[add-proof-checkpoint])
  install-proof-checkpoint (\\[install-proof-checkpoint])
  remove-proof-checkpoint (\\[remove-proof-checkpoint])
  remove-all-proof-checkpoints (\\[remove-all-proof-checkpoints])
  quit (C-c q)"
  (use-local-map edit-proof-mode-map)
    ;; fix up comment handling
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";")
  (setq major-mode 'edit-proof-mode)
  (setq mode-name "Edit Proof")
  (setq mode-line-process 'ilisp-status)
  (set-syntax-table pvs-mode-syntax-table))


(defpvs edit-proof edit-proof ()
  "Edit a proof in the \"Proof\" buffer

Displays the proof script of the formula closest to (moving forward) the
current cursor position.  The current buffer must be a PVS buffer, a TCC
buffer, a ppe buffer, or a prelude (file or theory) buffer.  See the
documentation for edit-proof-mode for more information."
  (interactive)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (let* ((*pvs-error* nil)
	   (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	   (line (+ (current-line-number) prelude-offset)))
      (cond ((equal origin "pvs")
	     (save-some-pvs-buffers)
	     (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
				    name)
				nil 'tc 'dont-care))
	    ((member origin '("ppe" "tccs"))
	     (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" name)
					nil 'tc nil)
	       (error "%s is not typechecked" name))))
      (unless *pvs-error*
	(pvs-send-and-wait
	 (format "(edit-proof-at \"%s\" %d \"%s\" \"%s\" %d %s)"
	     name line origin (buffer-name) prelude-offset
	     (and current-prefix-arg t))
	 nil 'EditProof 'dont-care)
	(cond ((eq (current-buffer) (get-buffer "Proof"))
	       (fix-edit-proof-comments)
	       (setq buffer-modified-p nil)
	       (goto-char (point-min))
	       (pvs-prover-goto-next-step)
	       (hilit-next-prover-command)
	       (when (pvs-send-and-wait "(when *in-checker* t)"
					nil nil "T\\|NIL")
		 (other-window 1)
		 (switch-to-buffer (ilisp-buffer))))
	      (t (pop-to-buffer (get-buffer-create "Proof"))))))))

(defun fix-edit-proof-comments ()
  (when (eq (current-buffer) (get-buffer "Proof"))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\";;;[^\"]*\"" nil t)
	(unless (looking-at "[ \t]*$")
	  (save-excursion
	    (insert "\n")
	    (indent-line-ilisp)))
	(delete-char -1))
      (goto-char (point-min))
      (while (looking-at ";;;")
	(forward-line 1))
      (while (re-search-forward ";;;" nil t)
	(unless (eq (point) (point-min))
	  (forward-char -2)
	  (delete-char -2))))))
	

(defvar *pvs-tmp-file* (concat "/tmp/" (make-temp-name "PVS")))

(defpvs install-proof edit-proof (&optional step)
  "Installs the edited proof

Installs the edited proof in the Proof buffer to the formula at the
cursor, or the original formula if the current buffer is the \"Proof\"
buffer."
  (interactive)
  (confirm-not-in-checker)
  (let ((pbuf (or (get-buffer "Proof"))))
    (unless pbuf
      (error "No proof is currently being edited"))
    (when (and (current-pvs-file t)
	       (buffer-modified-p))
      (error "Buffer has been modified"))
    (let ((buf (current-buffer)))
      (set-buffer pbuf)
      (if (condition-case err
	      (let ((ilisp-complete t))
		(find-unbalanced-lisp nil)
		(find-unbalanced-pvs-in-strings))
	    (error (message (cadr err))))
	  (pop-to-buffer pbuf)
	  (set-buffer buf)
	  (if (equal (buffer-name) "Proof")
	      (install-proof* nil nil step)
	      (let* ((name-and-origin (pvs-formula-origin))
		     (name (car name-and-origin))
		     (origin (cadr name-and-origin)))
		(install-proof* name origin step)))))))

(defpvs install-and-step-proof edit-proof ()
  (interactive)
  (install-proof t))

(defpvs install-and-x-step-proof edit-proof ()
  (interactive)
  (cond ((getenv "DISPLAY")
	 (let ((pvs-x-show-proofs t))
	   (install-proof t)))
	(t (message "DISPLAY variable not set, cannot popup proof display")
	   (beep)
	   (sleep-for 1)
	   (install-proof t))))

(defun find-unbalanced-pvs-in-strings ()
  (save-excursion
    (goto-char (point-min))
    (let ((region (pvs-find-next-string)))
      (while region
	(find-unbalanced-region-pvs (car region) (cdr region))
	(setq region (pvs-find-next-string))))))

(defun pvs-find-next-string ()
  (let ((start (progn (re-search-forward "\"" nil t)
		      (point)))
	(lisp-comment-p (or (save-excursion
			      (forward-word -1)
			      (looking-at "COMMENT"))
			    (looking-at "[ \t\n]*;"))))
    (when start
      (let ((end nil))
	(while (and (null end)
		    (re-search-forward "\"" nil t))
	  (unless (save-excursion
		    (forward-char -3)
		    (looking-at "[^\\\\]\\\\"))
	    (setq end (point))))
	(if lisp-comment-p
	    (pvs-find-next-string)
	    (when end
	      (cons start (- end 1))))))))

(defpvs remove-proof edit-proof ()
  "Removes the proof associated with the specified formula

The remove-proof command removes the proof associated with the specified
formula."
  (interactive)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin))
	 (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	 (line (+ (current-line-number) prelude-offset)))
    (pvs-send
     (format "(remove-proof-at \"%s\" %d \"%s\")"
	 name line origin))))

(defpvs install-pvs-proof-file edit-proof (filename)
  "Installs the specified proof file

The install-pvs-proof-file command installs the specified proof file.
This is needed when a proof file is directly edited or copied into the
current context, since otherwise the system assumes that the internal
proofs are the `real' ones."
  (interactive (complete-pvs-file-name "Find PVS file named: "))
  (pvs-send (format "(install-pvs-proof-file \"%s\")" filename))
  (message "Proof file installed."))

(defvar pvs-show-proofs-map nil)
(if pvs-show-proofs-map ()
    (setq pvs-show-proofs-map (copy-keymap pvs-view-mode-map))
    (define-key pvs-show-proofs-map "s" 'pvs-select-proof)
    (define-key pvs-show-proofs-map "v" 'pvs-view-proof)
    (define-key pvs-show-proofs-map "d" 'pvs-delete-proof)
    (define-key pvs-show-proofs-map "q" 'pvs-quit-show-proofs)
    (define-key pvs-show-proofs-map "h" 'pvs-show-proofs-help)
    (define-key pvs-show-proofs-map "?" 'pvs-show-proofs-help))

(defun pvs-show-proofs-mode ()
  "Mode for the \"Proofs\" buffer, used for displaying a set of proofs.

The \"Proofs\" buffer displays the proofs for all the formulas of a PVS
file or theory (M-x show-proofs-pvs-file or M-x show-proofs-theory), or
the orphaned proofs (M-x show-orphaned-proofs).

Key bindings are:
\\{pvs-show-proofs-map}"
  (interactive)
  (use-local-map pvs-show-proofs-map)
  (setq major-mode 'pvs-show-proofs-mode)
  (setq mode-name "PVS Proofs")
  (set-syntax-table pvs-mode-syntax-table)
  )

(defpvs show-proof-file edit-proof (context filename)
  "Display proofs of a PVS file

The show-proof-file command displays the proofs of the specified PVS file
in a buffer that allows them to selected or deleted.  A selected proof is
displayed in the Proof buffer and may be installed on a formula - see the
edit-proof command."
  (interactive
   (let* ((cdir (pvs-current-directory))
	  (dir (read-file-name "Show proof file of context: "
			       cdir cdir t)))
     (unless (file-directory-p dir)
       (error "%s is not a directory." dir))
     (unless (string-match "/$" dir)
       (setq dir (concat dir "/")))
     (cons dir (complete-pvs-file-name-in-dir
		"Show proof file for PVS file: " dir))))
  (when (pvs-send-and-wait (format "(show-proof-file \"%s\" \"%s\")"
			       context filename)
			   nil nil 'bool)
    (pop-to-buffer "Proofs")))

(defpvs show-orphaned-proofs edit-proof ()
  "Show the orphaned proofs file

The show-orphaned-proofs command displays the orphaned proofs in a
\"Proofs\" buffer that allows them to selected or deleted.  A selected
proof is displayed in the \"Proof\" buffer and may be installed on a
formula - see the edit-proof command."
  (interactive)
  (when (pvs-send-and-wait "(show-orphaned-proofs)" nil nil 'bool)
    (pop-to-buffer "Proofs")))

(defpvs show-proofs-importchain edit-proof (theoryname)
  "Displays all the proofs of the importchain

The show-proofs-importchain command displays all the proofs of the
importchain with the specified theory at the root."
  (interactive (complete-theory-name
		"Show proofs of importchain of theory named: "))
  (when (pvs-send-and-wait (format "(show-proofs-importchain \"%s\")"
			       theoryname)
			   nil nil 'bool))
    (pop-to-buffer "Show Proofs")
    (goto-char (point-min))
    (use-local-map pvs-show-proofs-map))

(defpvs show-proofs-pvs-file edit-proof (filename)
  "Displays all the proofs of the PVS file

The show-proofs-pvs-file command displays all the proofs of the specified
PVS file."
  (interactive (complete-pvs-file-name "Show proofs of PVS file named: "))
  (when (pvs-send-and-wait (format "(show-proofs-pvs-file \"%s\")" filename)
			   nil nil 'bool))
    (pop-to-buffer "Show Proofs")
    (goto-char (point-min))
    (use-local-map pvs-show-proofs-map))

(defpvs show-proofs-theory edit-proof (theoryname)
  "Displays all the proofs of the PVS file

The show-proofs-theory command displays all the proofs of the specified
theory."
  (interactive (complete-theory-name "Show proofs of theory named: "))
  (when (pvs-send-and-wait (format "(show-proofs-theory \"%s\")" theoryname)
			   nil nil 'bool))
    (pop-to-buffer "Show Proofs")
    (goto-char (point-min))
    (use-local-map pvs-show-proofs-map))


(defun pvs-select-proof ()
  "Display the proof in a \"Proof\" buffer, so that it may be edited and
installed on a formula."
  (interactive)
  (cond ((<= (current-line-number) 2)
	 (error "Select from one of the entries below."))
	(t (pvs-send-and-wait (format "(pvs-select-proof %d)"
				  (- (current-line-number) 3))
			      nil nil 'dont-care))))

(defun pvs-view-proof ()
  "Display the proof in a \"View Proof\" buffer allowing it to be viewed
without effecting the \"Proof\" buffer."
  (interactive)
  (cond ((<= (current-line-number) 2)
	 (error "Select from one of the entries below."))
	(t (pvs-send-and-wait (format "(pvs-view-proof %d)"
				  (- (current-line-number) 3))
			      nil nil 'dont-care))))

(defun pvs-delete-proof ()
  "Delete the proof from the buffer and associated file."
  (interactive)
  (if (<= (current-line-number) 2)
      (error "Select from one of the entries below.")
      (if (yes-or-no-p "Delete proof? ")
	  (pvs-send-and-wait (format "(pvs-delete-proof %d)"
				 (- (current-line-number) 3))
			     nil nil 'dont-care))))

(defun pvs-quit-show-proofs ()
  "Remove the \"Proofs\" buffer."
  (interactive)
  (when (get-buffer "Proofs")
    (remove-buffer "Proofs")))

(defun pvs-show-proofs-help ()
  "Provide help for the \"Proofs\" buffer that is used by the
show-proof-file and show-orphaned-proofs commands."
  (interactive)
  (with-output-to-temp-buffer "*Help*"
    (princ
     "The show orphaned proofs buffer displays a list of proofs, including
the formula name, theory name, and file name associated with each orphaned
proof.  In addition to the pvs-view-mode key bindings (displayed by
describe-mode) , the following are available:
  s      -  pvs-select-proof
  v      -  pvs-view-proof
  d      -  pvs-delete-proof
  q      -  pvs-quit-show-proofs
  h      -  pvs-show-proofs-help
  C-h m  -  describe-mode")))


;;; add-declaration

(defvar *add-decl-window-config* nil
  "Saves the window configuration when add-declaration or modify-declaration
are invoked")

(defvar add-declaration-mode-map nil)
(if add-declaration-mode-map ()
    (setq add-declaration-mode-map (copy-keymap pvs-mode-map))
    (define-key add-declaration-mode-map "\C-c\C-c"
      '(lambda () (interactive) (install-add-declaration))))

(defun add-declaration-mode ()
  "Major mode for editing declarations"
  ;;(interactive)
  (use-local-map add-declaration-mode-map)
  ;; fix up comment handling
  (make-local-variable 'comment-start)
  (setq comment-start "% ")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "% ")
  (setq major-mode 'add-declaration-mode)
  (setq mode-name "AddDecl")
  (setq mode-line-process 'ilisp-status)
  (set-syntax-table pvs-mode-syntax-table))

(defpvs add-declaration add-decl ()
  "Adds new declaration(s) before the declaration at the cursor

The add-declaration command allows you to add new declarations to the
current PVS buffer.  The new declarations are added above the declaration
at the cursor.  A new buffer is popped up.  Enter declarations in the
buffer; when finished, type C-c C-c to install the new declaration(s).  If
there is a typecheck error, the error will be displayed and the
declarations will not be installed."
  (interactive)
  (pvs-bury-output)
  (let ((file (current-pvs-file)))
    (when (buffer-modified-p (get-file-buffer file))
      (error "%s is not parsed" file))
    (when (pvs-send-and-wait (format "(lisp (add-declaration-at \"%s\" %d))"
				 file (current-line-number))
			     nil nil 'bool)
      (setq *add-decl-window-config* (current-window-configuration))
      (let ((buf (get-buffer-create "Add Declaration")))
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (set-buffer-modified-p nil)
	  (add-declaration-mode))
	(pop-to-buffer buf)
	(message "Add declarations - type C-c C-c to install.")))))

(defun install-add-declaration ()
  "Installs the edited declarations."
  (pvs-bury-output)
  (cond ((get-buffer "Add Declaration")
	 (save-excursion
	   (set-buffer "Add Declaration")
	   (goto-char (point-min))
	   (delete-initial-blank-lines)
	   (add-final-newline)
	   (write-region (point-min) (point-max) *pvs-tmp-file* nil 'notnil))
	 (let ((ret (pvs-send-and-wait
		     (format "(typecheck-add-declaration \"%s\" t)"
			 *pvs-tmp-file*)
		     nil nil 'list)))
	   (when (and ret (consp ret))
	     (let* ((file (car ret))
		    (place (cadr ret))
		    (decls (save-excursion
			     (set-buffer "Add Declaration")
			     (goto-char (point-max))
			     (delete-blank-lines)
			     (substring (buffer-string) 0 -1))))
	       (find-pvs-file file)
	       (goto-line (car place))
	       (insert-decls decls (cadr place) t)
	       (save-buffer)
	       (pvs-send-and-wait
		(format "(reset-parsed-date \"%s\")" file) nil nil 'dont-care)
	       (remove-buffer "Add Declaration")
	       (when *add-decl-window-config*
		 (set-window-configuration *add-decl-window-config*)
		 (setq *add-decl-window-config* nil))))))
	(t (error "No declaration is currently being edited"))))

(defun insert-decls (decls indent &optional crs)
  (let ((start (point))
	(indstr (make-string indent ? )))
    (insert decls)
    (save-excursion
      (if (> indent 0)
	  (while (and (>= (point) start)
		      (progn (beginning-of-line)
			     (insert-string indstr)
			     (= (forward-line -1) 0))))))
    (when crs (insert "\n\n"))))


;;; modify-declaration


(defvar modify-declaration-mode-map nil)
(if modify-declaration-mode-map ()
    (setq modify-declaration-mode-map (copy-keymap pvs-mode-map))
    (define-key modify-declaration-mode-map "\C-c\C-c"
      '(lambda () (interactive) (install-modified-declaration))))

(defun modify-declaration-mode ()
  "Major mode for editing proofs"
  ;;(interactive)
  (use-local-map modify-declaration-mode-map)
  ;; fix up comment handling
  (make-local-variable 'comment-start)
  (setq comment-start "% ")
  (make-local-variable 'comment-end)
  (setq comment-end "\n")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "% ")
  (setq major-mode 'modify-declaration-mode)
  (setq mode-name "ModDecl")
  (setq mode-line-process 'ilisp-status)
  (set-syntax-table pvs-mode-syntax-table))

(defpvs modify-declaration add-decl ()
  "Modifies the declaration at the cursor

The modify-declaration command creates a new buffer containing the
declaration at the cursor.  The body of the declaration may be edited, and
will replace the original when C-c C-c is typed."
  (interactive)
  (pvs-bury-output)
  (let ((file (current-pvs-file)))
    (when (buffer-modified-p (get-file-buffer file))
      (error "~a is not parsed" file))
    (when (pvs-send-and-wait (format "(lisp (modify-declaration-at \"%s\" %d))"
				 file (current-line-number))
			     nil nil 'bool)
      (setq *add-decl-window-config* (current-window-configuration))
      (let ((buf (get-buffer-create "Modify Declaration")))
	(save-excursion
	  (set-buffer buf)
	  (set-buffer-modified-p nil)
	  (modify-declaration-mode))
	(pop-to-buffer buf)
	(message "Modify declaration - type C-c C-c when finished.")))))

(defun install-modified-declaration ()
  "Installs the modified declaration"
  (pvs-bury-output)
  (cond ((get-buffer "Modify Declaration")
	 (save-excursion
	   (set-buffer "Modify Declaration")
	   (add-final-newline)
	   (delete-initial-blank-lines)
	   (write-region (point-min) (point-max)
			 *pvs-tmp-file* nil 'notnil))
	 (let ((ret (pvs-send-and-wait
		     (format "(typecheck-mod-declaration \"%s\" t)"
			 *pvs-tmp-file*)
		     nil nil 'list)))
	   (when (and ret (consp ret))
	     (let* ((file (car ret))
		    (place (cadr ret))
		    (decl (save-excursion
			    (set-buffer "Modify Declaration")
			    (goto-char (point-max))
			    (delete-blank-lines)
			    (substring (buffer-string) 0 -1))))
	       (find-pvs-file file)
	       (goto-line (car place))
	       (forward-char (cadr place))
	       (let ((start (point)))
		 (goto-line (caddr place))
		 (end-of-line)
		 ;;(forward-char (+ (cadddr place) 1))
		 (delete-region start (point)))
	       (beginning-of-line)
	       (insert-decls decl (cadr place))
	       (save-buffer)
	       (pvs-send-and-wait
		(format "(reset-parsed-date \"%s\")" file) nil nil 'dont-care))
	     (remove-buffer "Modify Declaration")
	     (when *add-decl-window-config*
	       (set-window-configuration *add-decl-window-config*)
	       (setq *add-decl-window-config* nil)))))
	(t (error "No declaration is currently being modified"))))

(defpvs help-pvs-prover help ()
  "Display help for the PVS prover commands

The help-pvs-prover command displays help for the PVS prover commands in
the Prover Help buffer."
  (interactive)
  (pvs-send-and-wait "(help-prover)" nil nil 'dont-care))

(defpvs show-last-proof proof-display ()
  "Display the last proof

The show-last-proof command displays a printout of the most recent proof
in the Proof Display buffer.  With an argument, displays it in brief form,
in which many sequents are suppressed and those that are displayed have
elisions replacing formulas which have not changed from the previous
sequent display."
  (interactive)
  (pvs-send-and-wait (format "(show-last-proof %s)"
			 (and current-prefix-arg t))
		     nil nil 'dont-care))

(defpvs load-pvs-strategies edit-proof ()
  "Load the pvs-strategies files

The load-pvs-strategies command loads the pvs-strategies files, from the
current context, your home directory, and the PVS path.  This is only
needed when a strategy is created while running a proof."
  (interactive)
  (pvs-send-and-wait "(read-strategies-files)" nil nil 'dont-care))


(defpvs set-rewrite-depth edit-proof (depth)
  "Controls the amount of output produced in rewrite commentaries

The set-rewrite-depth command controls the amount of output produced in
rewrite commentaries by setting the depth of expressions displayed.
Normally, both the rule name and the expression being rewritten are
displayed in the proof commentary when an auto-rewrite is triggered.
Set-rewrite-depth controls how deep the expression may get.  DEPTH
should be a whole number or nil.  If it is a positive number, then any
subexpression at that depth will be replaced by a pair of periods (..).
If it is 0 (zero), then only the rule name is displayed.  If it is nil,
then there is no depth bound."
  (interactive
   "sEnter nil (no bound; the default) or a number (0 = terse): ")
  (let ((dep (cond ((or (natnump depth)
			(null depth))
		    depth)
		   ((and (stringp depth)
			 (or (string-match "^[ \t]*$" depth)
			     (string-match "^[ \t]*[Nn][Ii][Ll][ \t]*$" depth)))
		    nil)
		   ((and (stringp depth)
			 (string-match "^[ \t]*[0-9]+[ \t]*$" depth))
		    (string-to-int depth))
		   (t (error "set-rewrite-depth: %s is not a number or nil"
			     depth)))))
    (pvs-send (format "(setq *rewrite-print-depth* %s)" dep))))


(defpvs set-rewrite-length edit-proof (length)
  "Controls the amount of output produced in rewrite commentaries

The set-rewrite-length command controls the amount of output produced in
rewrite commentaries by setting the length of expressions displayed.
Normally, both the rule name and the expression being rewritten are
displayed in the proof commentary when an auto-rewrite is triggered.
Set-rewrite-depth controls how long the expression may get.  LENGTH
should be a whole number or nil.  If it is a positive number, then any
subexpression at that length will be replaced by three periods (...).  If
it is 0 (zero), then only the rule name is displayed.  If it is nil, then
there is no bound on the length."
  (interactive
   "sEnter nil (no bound; the default) or a number (0 = terse): ")
  (let ((len (cond ((natnump length)
		    length)
		   ((and (stringp length)
			 (or (string-match "^[ \t]*$" length)
			     (string-match "^[ \t]*[Nn][Ii][Ll][ \t]*$"
					   length)))
		    nil)
		   ((and (stringp length)
			 (string-match "^[ \t]*[0-9]+[ \t]*$" length))
		    (string-to-int length))
		   (t (error "set-rewrite-length: %s is not an integer or nil"
			     length)))))
    (pvs-send (format "(setq *rewrite-print-length* %s)" len))))


(defpvs set-print-depth edit-proof (depth)
  "Set the print depth for expressions displayed in a sequent

The set-print-depth command sets the print depth for expressions displayed
in a sequent.  DEPTH should be a whole number.  If it is a positive number,
then any subexpression at that depth will be replaced by two periods (..).
If it is 0 (zero), then there is no bound on the depth."
  (interactive "sEnter a number (default: 0 = no depth bound): ")
  (let ((dep (cond ((natnump depth)
		    depth)
		   ((and (stringp depth)
			 (string-match "^[ \t]*$" depth))
		    nil)
		   ((and (stringp depth)
			 (string-match "^[ \t]*[0-9]+[ \t]*$" depth))
		    (string-to-int depth))
		   (t (error "set-print-depth: %s is not an integer" depth)))))
    (pvs-send (format "(setq *prover-print-depth* %s)"
		  (when (plusp dep) dep)))))


(defpvs set-print-length edit-proof (length)
  "Set the print length for expressions displayed in a sequent

The set-print-length command sets the print length for expressions
displayed in a sequent.  LENGTH should be a whole number.  If it is a
positive number, then any subexpression at that length will be replaced by
three periods (...).  If it is 0 (zero), then there is no bound on the
length."
  (interactive "sEnter a number (default: 0 = no length bound): ")
  (let ((len (cond ((natnump length)
		    length)
		   ((and (stringp length)
			 (string-match "^[ \t]*$" length))
		    nil)
		   ((and (stringp length)
			 (string-match "^[ \t]*[0-9]+[ \t]*$" length))
		    (string-to-int length))
		   (t (error "set-print-length: %s is not an integer"
			     length)))))
    (pvs-send (format "(setq *prover-print-length* %s)"
		  (when (plusp len) len)))))

(defpvs set-print-lines edit-proof (lines)
  "Set the print lines for formulas displayed in a sequent

The set-print-lines command sets the number of lines to display for formulas
in a sequent.  LINES should be a whole number.  If it is a positive number,
then any lines after that number will be replaced by two periods (..).
If it is 0 (zero), then the whole formula is printed."
  (interactive "sEnter a number (default: 0 = print everything): ")
  (let ((dep (cond ((natnump lines)
		    lines)
		   ((and (stringp lines)
			 (string-match "^[ \t]*$" lines))
		    nil)
		   ((and (stringp lines)
			 (string-match "^[ \t]*[0-9]+[ \t]*$" lines))
		    (string-to-int lines))
		   (t (error "set-print-lines: %s is not an integer" lines)))))
    (pvs-send (format "(setq *prover-print-lines* %s)"
		  (when (plusp dep) dep)))))

(defun pvs-get-prove-input ()
  "Gets the proof input of the prove command.  This is used primarily for
debugging."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin)))
    (let ((input (format "(prove-file-at \"%s\" %d %s \"%s\" \"%s\" %d %s)"
		     name (+ (current-line-number)
			     (if (equal origin "prelude")
				 (or pvs-prelude 0) 0))
		     'T origin
		     (buffer-name) (if (equal origin "prelude")
				       (or pvs-prelude 0) 0)
		     nil)))
      (switch-to-lisp t t)
      (insert input))))

(defpvs step-proof prove ()
  "Sets up a proof for using the proof stepper

The step-proof command invokes the prover and sets up proof stepping
through using the edit-proof command."
  (interactive)
  (confirm-not-in-checker)
  (delete-other-windows)
  (let* ((name-and-origin (pvs-formula-origin))
	 (name (car name-and-origin))
	 (origin (cadr name-and-origin))
	 (line (current-line-number))
	 (bname (buffer-name)))
    (let ((*pvs-error* nil))
      (cond ((equal origin "pvs")
	     (save-some-pvs-buffers)
	     (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
				    name)
				nil 'tc 'dont-care))
	    ((member origin '("ppe" "tccs"))
	     (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" name)
					nil 'tc nil)
	       (error "%s is not typechecked" name))))
      (unless *pvs-error*
	(let* ((prelude-offset (if (equal origin "prelude-theory")
				   pvs-prelude 0))
	       (line (+ (current-line-number) prelude-offset)))
	  (when (get-buffer "Proof")
	    (kill-buffer "Proof"))
	  (pvs-send-and-wait
	   (format "(edit-proof-at \"%s\" %d \"%s\" \"%s\" %d %s)"
	       name line origin (buffer-name) prelude-offset
	       (and current-prefix-arg t))
	   nil 'EditProof 'dont-care))
	(when (get-buffer "Proof")
	  (pop-to-buffer (get-buffer "Proof"))
	  (fix-edit-proof-comments)
	  (setq buffer-modified-p nil)
	  (goto-char (point-min))
	  (pvs-prover-goto-next-step)
	  (hilit-next-prover-command))
	(ilisp-send
	 (format "(lisp (prove-file-at \"%s\" %d %s \"%s\" \"%s\" %d %s %s))"
	     name (+ line
		     (if (equal origin "prelude")
			 (or pvs-prelude 0) 0))
	     nil origin bname
	     (if (equal origin "prelude")
		 (or pvs-prelude 0) 0)
	     nil pvs-x-show-proofs)
	 nil 'pr t)))))

(defpvs x-step-proof prove ()
  "Starts the prover, the proof-stepper and the proof display

The x-step-proof command starts the prover, proof-stepper and x-proof
display for the indicated formula"
  (interactive)
  (cond ((getenv "DISPLAY")
	 (let ((pvs-x-show-proofs t))
	   (step-proof)))
	(t (message "DISPLAY variable not set, cannot popup proof display")
	   (beep)
	   (sleep-for 1)
	   (step-proof))))

(defvar default-untried-strategy "(grind)")

(defpvs prove-untried-pvs-file prove (file strategy)
  "Attempt to prove the untried formulas in the specified PVS FILE using
the given STRATEGY.

Attempts all untried formulas in the specified PVS FILE which have no
associated proofs, using the specified STRATEGY which defaults to (GRIND).
With an argument (e.g., M-0 or C-u) proves untried TCCs as well."
  (interactive (append (complete-pvs-file-name
			"Prove untried in PVS file named: ")
		       (list (read-from-minibuffer
			      "Strategy to use: "
			      default-untried-strategy))))
  (let ((end (cdr (condition-case nil
		      (read-from-string strategy)
		    (error (error "invalid strategy syntax"))))))
    (unless (= end (length strategy))
      (error "invalid strategy syntax")))
  (unless (equal strategy default-untried-strategy)
    (setq default-untried-strategy strategy))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-untried-pvs-file \"%s\" '%s %s)"
		file strategy (and current-prefix-arg t))
	    nil))

(defpvs prove-untried-theory prove (theory strategy)
  "Attempt to prove the untried formulas in the specified THEORY using
the given STRATEGY.

Attempts all untried formulas in the specified THEORY which have no
associated proofs, using the specified STRATEGY which defaults to (GRIND).
With an argument (e.g., M-0 or C-u) proves untried TCCs as well."
  (interactive (append (complete-theory-name
			"Prove untried in theory named: ")
		       (list (read-from-minibuffer
			      "Strategy to use: "
			      default-untried-strategy))))
  (let ((end (cdr (condition-case nil
		      (read-from-string strategy)
		    (error (error "invalid strategy syntax"))))))
    (unless (= end (length strategy))
      (error "invalid strategy syntax")))
  (unless (equal strategy default-untried-strategy)
    (setq default-untried-strategy strategy))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (pvs-send (format "(prove-untried-theory \"%s\" '%s %s)"
		theory strategy (and current-prefix-arg t))
	    nil))


;;; These all pertain only to a proof in progress

(defpvs ancestry proof-display ()
  "Displays ancestry of current sequent

The ancestry command displays the ancestry of the current sequent branch,
indicating the sequents and commands the led to the current sequent."
  (interactive)
  (pvs-send-and-wait "(call-ancestry)" nil nil 'dont-care))

(defpvs siblings proof-display ()
  "Displays siblings of current sequent

The siblings command displays the siblings of the current sequent."
  (interactive)
  (pvs-send-and-wait "(call-siblings)" nil nil 'dont-care))

(defpvs show-current-proof proof-display ()
  "Displays the proof in progress

The show-current-proof command displays the proof script for the proof in
progress.  The buffer looks like an edit-proof buffer, but the proof
editing commands are not available for it."
  (interactive)
  (pvs-send-and-wait "(call-show-proof)" nil nil 'dont-care))

(defpvs x-show-current-proof x-display ()
  "Displays the proof in progress using X

The x-show-current-proof command displays the proof in progress using the
Tcl/Tk proof display facility."
  (interactive)
  (if (getenv "DISPLAY")
      (pvs-send "(call-x-show-proof)")
      (message "DISPLAY variable not set, cannot popup proof display")))

(defpvs x-show-proof x-display ()
  "Displays the proof of the formula at the cursor

The x-show-proof command displays the proof of the formula at or beyond
the current cursor position using the Tcl/Tk proof display facility."
  (interactive)
  (if (getenv "DISPLAY")
      (let* ((name-and-origin (pvs-formula-origin))
	     (name (car name-and-origin))
	     (origin (cadr name-and-origin)))
	(pvs-send (format "(call-x-show-proof-at \"%s\" %d \"%s\")"
		      name (+ (current-line-number)
			      (if (equal origin "prelude")
				  (or pvs-prelude 0) 0))
		      origin)))
      (message "DISPLAY variable not set, cannot popup proof display")))

(defpvs show-hidden-formulas proof-display ()
  "Displays the hidden formulas of the current proof

The show-hidden-formulas command displays the hidden formulas of the
current proof."
  (interactive)
  (pvs-send-and-wait "(call-show-hidden)" nil nil 'dont-care))

(defpvs show-auto-rewrites proof-display ()
  "Displays the currently used auto-rewrite rules

The show-auto-rewrites command displays the currently used auto-rewrite
rules."
  (interactive)
  (message "Creating the *Auto-Rewrites* buffer...")
  (pvs-send-and-wait "(show-auto-rewrites)" nil nil 'dont-care)
  (message ""))

(defpvs show-expanded-sequent proof-display ()
  "Displays expanded form of the current sequent

The show-expanded-sequent displays expanded form of the current sequent.
By default, names from the prelude are not expanded, but with an argument
they are expanded."
  (interactive)
  (pvs-send-and-wait (format "(show-expanded-sequent %s)"
			 (and current-prefix-arg t))
		     nil nil 'dont-care))

(defpvs show-skolem-constants proof-display ()
  "Displays the Skolem constants of the current sequent

The show-skolem-constants command displays the Skolem constants of the
current sequent."
  (interactive)
  (pvs-send-and-wait "(show-skolem-constants)" nil nil 'dont-care))

(defpvs explain-tcc proof-display ()
  "Explains the source of a TCC subgoal in a proof

The explain-tcc command explains the source of a TCC subgoal in a proof."
  (interactive)
  (pvs-send-and-wait "(call-explain-tcc)" nil nil 'dont-care))

(defpvs help-pvs-prover-command help (command)
  "Displays the help documentation for the command

The help-pvs-prover-command displays help for the specified prover command
in the Prover Help buffer."
  (interactive (complete-strategy-name "Help for prover command: "))
  (pvs-send-and-wait (format "(help-prover \"%s\")" command)
		     nil nil 'dont-care))

(defpvs help-pvs-prover-strategy help (strategy)
  "Displays the specified strategy

The help-pvs-prover-strategy command displays the strategy associated with
the specified prover command in the `Strategy Display' buffer"
  (interactive (complete-strategy-name "Show strategy: "))
  (pvs-send-and-wait (format "(show-strategy \"%s\")" strategy)
		     nil nil 'dont-care))

(defvar minibuffer-pvs-strategy-must-match-map
  (let ((kmap (copy-keymap minibuffer-local-must-match-map)))
    (define-key kmap "?" nil)
    kmap))

(defun complete-strategy-name (prompt)
  (let* ((minibuffer-local-must-match-map
	  minibuffer-pvs-strategy-must-match-map)
	 (strategies (pvs-file-send-and-wait
		      (format "(collect-strategy-names %s)"
			  (and current-prefix-arg t))
		      nil nil 'list))
	 (strategy (completing-read prompt (mapcar 'list strategies) nil t)))
    (if (equal strategy "")
	(error "Must provide a strategy name")
	(list strategy))))

(defpvs x-prover-commands x-display ()
  "Displays the PVS prover commands in a Tcl/Tk window

The x-prover-commands command pops up a Tcl/Tk window with a list of all
the PVS prover commands.  Click on the Help button on the bottom of the
display for more information."
  (interactive)
  (pvs-send "(x-prover-commands)"))

(defpvs toggle-proof-prettyprinting edit-proof ()
  "Toggles whether to prettyprint the proof files

The toggle-proof-prettyprinting command toggles whether to prettyprint
the proof file (with extension .prf) associated with a PVS file.
Prettyprinted files are easier to read, edit, and email, but they take
a lot longer to generate.  By default, proof files are prettyprinted."
  (interactive)
  (let ((flag (pvs-send-and-wait "(toggle-proof-prettyprinting)"
				 nil nil 'bool)))
    (message "%s prettyprinting proof files"
	     (if flag "Now" "No longer"))))

(defun complete-theory-name-list (prompt)
  (let* ((theories (pvs-collect-theories))
	 (theory (completing-read prompt theories nil t))
	 (list nil))
    (while (not (equal theory ""))
      (push theory list)
      (setq theory (completing-read prompt theories nil t)))
    (nreverse list)))

(defpvs dump-sequents edit-proof (flag)
  "Determines whether to dump the unproved sequents at the end of a proof"
  (interactive (list (yes-or-no-p
		      "Do you want unproved sequents to be dumped to file? ")))
  (pvs-send-and-wait (format "(setq *dump-sequents-to-file* %s)" flag)
		     nil nil 'dont-care))

(defvar pvs-browse-proofs-mode-map nil)
(if pvs-browse-proofs-mode-map ()
    (setq pvs-browse-proofs-mode-map (make-keymap))
    (suppress-keymap pvs-browse-proofs-mode-map t)
    (define-key pvs-browse-proofs-mode-map "c"
      '(lambda (description)
	 (interactive "sEnter the new description: ")
	 (pvs-proofs-change-description description)))
    (define-key pvs-browse-proofs-mode-map "d"
      '(lambda () (interactive) (pvs-proofs-set-default)))
    (define-key pvs-browse-proofs-mode-map "p"
      '(lambda () (interactive) (pvs-proofs-rerun-proof)))
    (define-key pvs-browse-proofs-mode-map "q"
      '(lambda () (interactive) (pvs-browse-quit)))
    (define-key pvs-browse-proofs-mode-map "r"
      '(lambda (id)
	 (interactive "sEnter the new id for the proof: ")
	 (pvs-proofs-rename id)))
    (define-key pvs-browse-proofs-mode-map "s"
      '(lambda () (interactive) (pvs-proofs-show)))
    (define-key pvs-browse-proofs-mode-map "e"
      '(lambda () (interactive) (pvs-proofs-edit-proof)))
    (define-key pvs-browse-proofs-mode-map "\t"
      '(lambda () (interactive) (scroll-left 16)))
    (define-key pvs-browse-proofs-mode-map "\M-\t"
      '(lambda () (interactive) (scroll-left -16)))
    (define-key pvs-browse-proofs-mode-map "\177"
      '(lambda () (interactive) (pvs-proofs-delete-proof)))
    )

(defun pvs-browse-proofs-mode ()
  "Major mode for browsing a list of proofs

Each line describes one of the proofs for a given formula
Letters do not insert themselves; instead, they are commands:
  TAB   Scroll-left
  M-TAB Scroll-right
  c     Change description: add or change the description for the proof
  d     Default proof: set the default to the specified proof
  e     Edit proof: invoke edit-proof on the specified proof
          The proof may then be applied to other formulas as usual
  p     Prove: rerun the specified proof (makes it the default)
  q     Quit: exits the Proofs buffer
  r     Rename proof: rename the specified proof
  s     Show proof: Shows the specified proof in a Proof:<id> buffer
  DEL   Delete proof: deletes the specified proof from the formula"
  (kill-all-local-variables)
  (use-local-map pvs-browse-proofs-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'pvs-browse-proofs-mode)
  (setq mode-name "Proofs"))

; (defpvs display-proofs-formula browse ()
;   (interactive)
;   (pvs-bury-output)
;   (let* ((name-and-origin (pvs-formula-origin))
; 	 (name (car name-and-origin))
; 	 (origin (cadr name-and-origin))
; 	 (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
; 	 (line (+ (current-line-number) prelude-offset)))
;     (pvs-send-and-wait
;      (format "(display-proofs-formula-at \"%s\" \"%s\" %d)"
; 	 name origin line)
;      nil 'proofs 'dont-care)))

; (defpvs display-proofs-theory browse (theoryname)
;   "Show the proofs for all formulas of the specified theory"
;   (interactive (complete-theory-name "Show proofs for theory named: "))
;   (pvs-bury-output)
;   (pvs-send-and-wait (format "(display-proofs-theory \"%s\")" theoryname)
; 		     nil (pvs-get-abbreviation 'display-proofs-theory)
; 		     'dont-care))

; (defpvs display-proofs-pvs-file browse (filename)
;   "Show the proofs for all formulas of the specified PVS file"
;   (interactive (complete-pvs-file-name "Show proofs for PVS file named: "))
;   (pvs-bury-output)
;   (pvs-send-and-wait (format "(display-proofs-pvs-file \"%s\")" filename)
; 		     nil (pvs-get-abbreviation 'display-proofs-pvs-file)
; 		     'dont-care))

;;; These are invoked from key bindings from the Proofs buffer

(defun pvs-proofs-set-default ()
  (pvs-send-and-wait (format "(set-proofs-default %d)" (current-line-number))))

(defun pvs-proofs-rename (id)
  (pvs-send-and-wait (format "(proofs-rename %d \"%s\")"
			 (current-line-number) id)))

(defun pvs-proofs-show ()
  (pvs-send-and-wait (format "(proofs-show-proof %d)" (current-line-number))))

(defun pvs-proofs-change-description (description)
  (pvs-send-and-wait (format "(proofs-change-description %d \"%s\")"
			 (current-line-number) description)))

(defun pvs-proofs-rerun-proof ()
  (confirm-not-in-checker)
  (ilisp-send (format "(proofs-rerun-proof %d)" (current-line-number))
	      nil 'pr t))

(defun pvs-proofs-delete-proof ()
  (if (yes-or-no-p "Delete this proof? ")
      (pvs-send-and-wait (format "(proofs-delete-proof %d)"
			     (current-line-number)))
      (message "")))

(defun pvs-proofs-edit-proof ()
  (pvs-send-and-wait (format "(proofs-edit-proof %d)" (current-line-number)))
  (when (eq (current-buffer) (get-buffer "Proof"))
    (fix-edit-proof-comments)))


  (make-face 'font-lock-pvs-checkpoint-face)
  (set-face-background 'font-lock-pvs-checkpoint-face "red")

(defun install-proof-checkpoint ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer "Proof"))
    (error "Must be in a Proof buffer to make a checkpoint"))
  (add-proof-checkpoint)
  (install-proof))

(defun add-proof-checkpoint ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer "Proof"))
    (error "Must be in a Proof buffer to make a checkpoint"))
  (pvs-prover-goto-next-step)
  (if (eq (point) (point-max))
      (error "At end of buffer")
      (let ((start (point)))
	(cond ((memq pvs-emacs-system '(xemacs19 xemacs20))
	       (insert-face "!!" 'font-lock-pvs-checkpoint-face))
	      (t (insert "!!")
		 (overlay-put (make-overlay start (point))
			      'face 'font-lock-pvs-checkpoint-face))))))

(defun remove-proof-checkpoint ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer "Proof"))
    (error "Must be in a Proof buffer to make a checkpoint"))
  (pvs-prover-goto-next-step)
  (forward-char -2)
  (if (looking-at "!!")
      (delete-char 2)
      (error "Not at a checkpoint mark")))

(defun remove-all-proof-checkpoints ()
  (interactive)
  (unless (eq (current-buffer) (get-buffer "Proof"))
    (error "Must be in a Proof buffer to make a checkpoint"))
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "!!" nil t)
      (delete-char -2))))

(defun prove-with-checkpoint ()
  (interactive)
  (confirm-not-in-checker)  
  (unless (eq (current-buffer) (get-buffer "Proof"))
    (error "Must be run from a Proof buffer"))
  (pvs-prover-goto-next-step)
  (let ((proof (concat (buffer-substring-without-properties
			(point-min) (point))
		       "!!"
		       (buffer-substring-without-properties
			(point) (point-max)))))
    (ilisp-send (format "(prove-with-checkpoint %s)" proof))))

(defun install-proof* (name origin &optional step)
  (save-excursion
    (set-buffer "Proof")
    (goto-char (point-min))
    (while (search-forward "!!" nil t)
      (replace-match "(checkpoint)" nil t))
    (write-region (point-min) (point-max) *pvs-tmp-file* nil 'notnil)
    (goto-char (point-min))
    (while (search-forward "(checkpoint)" nil t)
      (replace-match "!!" nil t)))
  (let* ((*pvs-error* nil)
	 (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	 (line (+ (current-line-number) prelude-offset))
	 (installed (pvs-send-and-wait
		     (format "(install-proof \"%s\" %s %d %s \"%s\" %d)"
			 *pvs-tmp-file* (when name (format "\"%s\"" name))
			 line (when origin (format "\"%s\"" origin))
			 (buffer-name) prelude-offset)
		     nil nil 'bool)))
    (when (and (not *pvs-error*)
	       installed
	       (or step
		   (save-excursion
		     (set-buffer "Proof")
		     (goto-char (point-min))
		     (re-search-forward "!!" nil t))
		   (y-or-n-p "Run the proof? ")))
      (when step
	(pop-to-buffer (get-buffer "Proof"))
	(fix-edit-proof-comments)
	(setq buffer-modified-p nil)
	(goto-char (point-min))
	(pvs-prover-goto-next-step)
	(hilit-next-prover-command))
      (if (null name)
	  (ilisp-send (format "(prove-proof-at %d %s %s)"
			  (current-line-number) step pvs-x-show-proofs)
		      "" 'pr t)
	  (ilisp-send (format
			  "(prove-file-at \"%s\" %d %s '%s \"%s\" %d nil %s)"
			  name line (not step) origin (buffer-name)
			  prelude-offset pvs-x-show-proofs)
		      "" 'pr t)))))

(defpvs set-proof-backup-number prove (num)
  "Set the PVS proof backup number.

The set-proof-backup-number command indicates the number of backups to
keep for proof files.  This should probably be put in the ~.pvsemacs file.
If NUM is 0, then no backups are kept.  If NUM is 1, then a single backups
is kept (e.g., foo.prf~).  If NUM is a larger number, then that number of
backup files will be kept.  For example, if it is 3, the backup files
might be foo.prf.~3~, foo.prf.~4~, and foo.prf.~5~.  When the next backup
is created, foo.prf.~3~ is removed and foo.prf.~6~ is added."
  (interactive "sEnter a number (default: 1): ")
  (let ((n (cond ((natnump num)
		  num)
		 ((and (stringp num)
		       (string-match "^[ \t]*$" num))
		  1)
		 ((and (stringp num)
		       (string-match "^[ \t]*[0-9]+[ \t]*$" num))
		  (string-to-int num))
		 (t (error "set-proof-backup-number: %s is not an integer"
			   num)))))
    (pvs-send (format "(setq *number-of-proof-backups* %s)" n))))

(defpvs show-proof-backup-number prove ()
  "Show the PVS proof backup number.

See \\[set-proof-backup-number] for details."
  (interactive)
  (message (format "The proof backup number is %s"
	       (pvs-send-and-wait "*number-of-proof-backups*"))))

(defpvs new-decision-procedures prove ()
  "Sets the default to the new decision procedures"
  (interactive)
  (pvs-send-and-wait "(new-decision-procedures)"))

(defpvs old-decision-procedures prove ()
  "Sets the default to the old decision procedures"
  (interactive)
  (pvs-send-and-wait "(old-decision-procedures)"))
