;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;; Strategy-trace features may be invoked via TAB shortcuts using
;; the following Emacs Lisp functions.  These shortcuts are
;; entirely optional.

;; Invokes the command in the *pvs* buffer except that it wraps the
;; command with (trace-strat ... <depth>) first.  Depth argument
;; may be supplied to limit trace depth.  Place a complete command
;; after the "Rule? " prompt, then invoke TAB-$ with optional
;; depth argument (using C-u).

(defvar pvs-in-checker)
(defvar pvs-prover-helps-map)

(declare-function hilit-next-prover-command "pvs-prover-helps")
(declare-function pvs-prover-goto-next-step "pvs-prover-helps")

(defun pvs-prover-trace-strat (&optional depth)
  "Invokes prover strategy with trace enabled to DEPTH levels."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (goto-char (point-max))   ; (end-of-buffer)
  (backward-sexp 1)
  (insert "(trace-strat ")
  (goto-char (point-max))   ; (end-of-buffer)
  (when depth (insert (format " %s" depth)))
  (insert ")")
  (return-ilisp))

;; Same as the proof stepper function pvs-prover-run-proof-step (TAB-1),
;; except that it wraps the command with (trace-strat ... <depth>)
;; first.  Invoke with TAB-0 (zero) and optional depth argument.

(defun pvs-prover-trace-proof-step (&optional depth)
  "Execute one step of proof with strategy trace enabled to DEPTH levels."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let (;;(pvsbuf (current-buffer))
	(editprfbuf (get-buffer "Proof"))
	(cmd nil))
    (unless editprfbuf
      (error "Must have a Proof Buffer."))
    (when pvs-in-checker
      (with-current-buffer editprfbuf
	(pvs-prover-goto-next-step)
	(let ((beg (point))
	      (end (progn (forward-sexp 1) (point))))
	  (pvs-prover-goto-next-step)
	  (let ((editprfwin (get-buffer-window editprfbuf)))
	    (if editprfwin
		(set-window-point editprfwin (point))))
	  (setq cmd (buffer-substring beg end))))
      (goto-char (point-max))
      (insert cmd)
      (pvs-prover-trace-strat depth)
      (return-ilisp)
      (hilit-next-prover-command))))

;;; Key Bindings

(define-key pvs-prover-helps-map "$"   'pvs-prover-trace-strat)
(define-key pvs-prover-helps-map "0"   'pvs-prover-trace-proof-step)
