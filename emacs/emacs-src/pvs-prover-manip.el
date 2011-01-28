;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; pvs-prover-manip.el -- Expression manipulation aids for PVS prover 
;; Author          : Ben Di Vito <b.divito@nasa.gov>
;; Created On      : 8 Apr 2001
;; Last Modified By: 
;; Last Modified On: 31 Oct 2001 (v0.9)
;; Last Modified On:  6 Feb 2002 (v1.0)
;; Last Modified On: 27 Jan 2003 (v1.1)
;; Last Modified On: 29 Nov 2005 (v1.2-beta)
;; Last Modified On: 17 Nov 2007 (v1.2)
;; Status          : Stable
;; Version         : 1.2
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is a U.S. Government work and thus not protected by U.S. copyright.
;; As a courtesy, please retain the header lines and introductory comments
;; in any derivative works.  Problem reports, suggestions and enhancements
;; are encouraged.  Send them to the address above.
;;
;; ----------------------------------------------------------------------------
;;
;; This file introduces PVS prover shortcuts helpful for manually
;; manipulating sequents.  It provides ways to streamline interactive
;; strategy invocation by assisting with certain types of argument
;; entry.  It adds features similar to those of the PVS prover helps
;; package originally developed by C. Michael Holloway of NASA Langley.
;; Also included are various utilities for PVS proof maintenance and
;; several miscellaneous operations and interface features.  A few
;; start-up actions and variable bindings are included for managing the
;; strategies that reside in the Common Lisp process.
;;
;; =========================== End of preamble ================================



;; ================ Start-up actions ================

(defvar pvs-prover-manip-version "1.2")

;; Account for differences between Emacs and XEmacs functions.

(eval-when-compile
  (if (featurep 'xemacs)
      (defun buffer-substring-text (beg end)
	(buffer-substring beg end))
      (defun buffer-substring-text (beg end)
	(buffer-substring-no-properties beg end))))


;; ================ Prover command entry shortcuts  ================

;; The basic usage pattern is as follows:
;;   - TAB-z initiates the command entry sequence.  The user is prompted
;;       for the name of a strategy (or rule) to invoke.
;;   - The user will be prompted for inputs according to the formal
;;       argument list of the chosen rule or strategy.
;;   - To supply a value for an argument, the user has the choice of
;;       either entering text in the minibuffer, or selecting a region
;;       of text in the prover's Emacs buffer, either by a mouse dragging
;;       selection or any other means that sets point and mark.
;;   - A minibuffer text argument is terminated by a CR in the usual way.
;;       For a text region selection, TAB-, causes the text region to be
;;       grabbed and added to the list of strategy arguments.
;;   - Quotes are added automatically to selected text but not to typed
;;       text because it might contain numbers or other constants.
;;   - The user repeats the text entries or region selections until all
;;       required arguments have been supplied.  If there are &optional
;;       arguments, the user is prompted for these as well and may enter
;;       them using the same methods.  Optional argument keywords need
;;       not be typed.
;;   - Entering a null string in the minibuffer for an optional argument
;;       selects its default value.
;;   - Entering a ";" for any optional argument causes the rest of the
;;       optionals to be skipped and will proceed to the &rest argument
;;       phase if such an argument exists.  Otherwise, ";" terminates
;;       argument entry.  A TAB-; typed after a region selection has
;;       the same effect of moving to the next phase.
;;   - Rest argument entry proceeds for as many values as the user
;;       wishes to supply.
;;   - Entering the string "\" rolls back to the previous argument.
;;   - Argument entry may be terminated at any time in the &optional or
;;       &rest phases by supplying the value "." in the minibuffer.
;;       Typing TAB-. after a region selection has the same effect.
;;   - After the desired sequence of arguments has been gathered, the
;;       completed rule or strategy command is sent to the prover.
;; This sequence may be abandoned at any point before completion using
;; C-g and the partially constructed command will be deleted from the
;; end of the Emacs buffer.
;;


;; ---------- Main interactive commands ----------

(defun pvs-prover-manip-select ()
  "Invokes prover manipulation strategies on user-demarcated
expressions from current proof buffer."
  (interactive)
  (goto-pvs-proof-buffer)
  (goto-char (point-max))
  (let ((cmd-mark (point-marker)) (done nil))
    (unwind-protect
	(let ((strat (read-from-minibuffer "Strategy or rule to invoke: ")))
	  (insert "(" strat)
	  (collect-mixed-strat-args strat)
	  (setf done t))
      (unless done (kill-region cmd-mark (point))))
    (set-marker cmd-mark nil))
  (goto-pvs-proof-buffer)
  (goto-char (point-max))
  (insert ")")
  (return-ilisp))


;; ---------- Emacs display declarations ----------

;; Set up face definitions for XEmacs and conditionally introduce
;; an appropriate function for highlighting text.

(defvar manip-fg-colors
  '("red" "dark green" "blue" "magenta" "orange" "brown"))

(defvar manip-fg-face-names nil)

(when (featurep 'xemacs)
  (dotimes (i (length manip-fg-colors))
    (let ((expr-face (make-symbol (format "manip-expr-face%s" i))))
      (setf manip-fg-face-names 
	    (cons expr-face manip-fg-face-names))
      (copy-face 'default expr-face)
      (set-face-background expr-face "white")
      (set-face-foreground expr-face (nth i manip-fg-colors))))
  (setf manip-fg-face-names (reverse manip-fg-face-names)))

;; The following definitions are a little stilted because intermediate
;; function variables are introduced to silence some annoying warnings
;; from the byte compiler.

(eval-when-compile
  (if (featurep 'xemacs)
      (progn (defvar set-extent-face-fn #'set-extent-face)
	     (defvar make-extent-fn #'make-extent))
      (progn (defvar set-extent-face-fn nil)
	     (defvar make-extent-fn nil))))

;(when (featurep 'xemacs)
;  (defvar set-extent-face-fn #'set-extent-face)
;  (defvar make-extent-fn #'make-extent))

(eval-when-compile
  (if (featurep 'xemacs)
      (defun highlight-text-region (index beg end)
	(let ((face-index (min index (- (length manip-fg-face-names) 1))))
	  (funcall set-extent-face-fn
		   (funcall make-extent-fn beg end)
		   (nth face-index manip-fg-face-names))))
      (defun highlight-text-region (index beg end)
	(overlay-put (make-overlay beg end)
		     'face (cons 'foreground-color (car manip-fg-colors))))))

(defvar deactivate-region-fn
  (if (featurep 'xemacs) #'zmacs-deactivate-region #'deactivate-mark))

(defun collapse-active-region ()
  (funcall deactivate-region-fn))


;; ---------- Supporting functions ----------

(defvar pvs-prover-manip-arg-index 0)

;; Collect the command arguments according to rule/strategy signature.

(defun collect-mixed-strat-args (strat)
  (let ((arglist (rule-formal-args strat)) (arg-val nil)
	(markers (list (point-marker))) (index 0))
   (flet ((emit-arg (incr str)
	    (when incr (incf index))
	    (goto-pvs-proof-buffer)
	    (goto-char (point-max))
	    (push (point-marker) markers)
	    (insert str)))
    (do ((done nil) (mode 'collecting))
	(done (dolist (m markers) (set-marker m nil)))
      (let* ((arg-spec (next-rule-arg index arglist)))
	(cond ((not arg-spec) (setf done t))
	      ((eq mode 'skipping)
	       (if (and (listp arg-spec) (eq (car arg-spec) 'opt))
		   (emit-arg t (format " %s" (caddr arg-spec)))
	           (setf mode 'collecting)))
	      (t (setf arg-val (get-mixed-strat-arg index arglist))
		 (cond ((consp arg-val)
			(case (car arg-val)
			  ((next-arg) (incf index))
			  ((skip-opt) (incf index) (setf mode 'skipping))
			  ((done) (setf done t)))
			(emit-arg nil (format " \"%s\"" (cdr arg-val))))
		       ((equal arg-val "\\")
			(unless (zerop index)
			  (kill-region (car markers) (point))
			  (set-marker (pop markers) nil)
			  (decf index)))
		       ((equal arg-val ".") (setf done t))
		       ((equal arg-val ";") (setf mode 'skipping))
		       ((equal arg-val "")
			(emit-arg t
		          (if (and (listp arg-spec) (eq (car arg-spec) 'opt))
			      (format " %s" (caddr arg-spec))
			      " \"Skipped non-optional argument\"")))
		       (t (emit-arg t (format " %s" arg-val)))))))))))

(defun get-mixed-strat-arg (index arglist)
  (let* ((arg-prompt (rule-arg-prompt index arglist))
	 (full-prompt (format "Type or select %s: " arg-prompt)))
    (setf pvs-prover-manip-arg-index index)
    (catch 'highlighted-text-region
      (read-from-minibuffer full-prompt ""))))

;; Extract and highlight a text region, then push it as a value
;; on the command list.

(defun fetch-manip-expr (event-type)
  (let* ((p (point)) (q (mark))
	 (expr (purge-newlines (buffer-substring-text p q))))
    (highlight-text-region pvs-prover-manip-arg-index p q)
    (collapse-active-region)
    (throw 'highlighted-text-region (cons event-type expr))))

(defun purge-newlines (str)
  (let* ((strings (split-string str "\n"))
	 (butlast-str (butlast strings))
	 (last-str (car (last strings))))
    (concat (apply #'concat (mapcar #'(lambda (s) (concat s " ")) butlast-str))
	    last-str)))

;; Collect the region just selected and indicate continuation type.

(defun pvs-prover-manip-next-expr ()
  (interactive)
  (fetch-manip-expr 'next-arg))

(defun pvs-prover-manip-last-optional ()
  (interactive)
  (fetch-manip-expr 'skip-opt))

(defun pvs-prover-manip-last-expr ()
  (interactive)
  (fetch-manip-expr 'done))


;; Split formal argument list into required, optional and rest lists.

(defun rule-formal-args (name)
  (let ((formals (pvs-send-and-wait 
		   (format "(rule-or-strategy-signature '%s)" name))))
    (if formals
	(parse-rule-formals (car (read-from-string formals)))
        '(nil nil args))))

(defun parse-rule-formals (formals)
  (let* ((len (length formals))
	 (num-req-args
	   (or (position-if #'(lambda (a) (member a '(&optional &rest)))
			    formals)
	       len))
	 (req-args (butlast formals (- len num-req-args)))
	 (has-rest (and (> len 1) (eq (nth (- len 2) formals) '&rest)))
	 (rest-arg (if has-rest (nth (- len 1) formals) nil))
	 (opt-len (- len num-req-args (if has-rest 3 1)))
	 (opt-args (cond ((<= opt-len 0) nil)
			 (has-rest
			  (butlast (nthcdr (1+ num-req-args) formals) 2))
			 (t (nthcdr (1+ num-req-args) formals)))))
    (list req-args opt-args rest-arg)))

;; Extract the category and name (optionals only) for the nth argument.

(defun next-rule-arg (n formals)
  (let ((req-args (car formals))
	(opt-args (cadr formals))
	(rest-arg (caddr formals)))
    (if (< n (length req-args))
	'req
        (let ((n2 (- n (length req-args))))
	  (if (< n2 (length opt-args))
	      (cons 'opt (raw-opt-var-val (nth n2 opt-args)))
	      (if rest-arg 'rest nil))))))

;; Construct the variable part of the prompt text for the nth argument.

(defun rule-arg-prompt (n formals)  ;; combine with above later
  (let ((req-args (car formals))
	(opt-args (cadr formals))
	(rest-arg (caddr formals)))
    (if (< n (length req-args))
	(format "argument %s" (upcase (symbol-name (nth n req-args))))
        (let ((n2 (- n (length req-args))))
	  (if (< n2 (length opt-args))
	      (let ((opt (raw-opt-var-val (nth n2 opt-args))))
		(format "optional :%s (default %s)"
			(upcase (symbol-name (car opt))) (cadr opt)))
	      (format "item %s of %s" (- n2 (length opt-args) -1)
		      (upcase (symbol-name rest-arg))))))))

(defun raw-opt-var-val (raw-opt)
  (let* ((opt (if (listp raw-opt) raw-opt (list raw-opt nil)))
	 (opt-var (car opt))
	 (raw-val (cadr opt))
	 (opt-val (if (and (listp raw-val) (> (length raw-val) 1)
			   (eq (car raw-val) 'quote))
		      (cadr raw-val)
		      raw-val)))
    (list opt-var opt-val)))


;; -------- Creating syntax matching patterns (version 1.2) --------

;; Extract and highlight a text region representing a PVS subexpression
;; in the current sequent.  Find an equivalent extended expression
;; notation to refer to it and insert that into the *pvs* buffer.
;; Requires support functions in manip-strategies.

(defun fetch-and-map-expr (ee-form)
  "Find an equivalent syntax-matching pattern for the selected PVS
expression and insert it into the *pvs* buffer."
  (interactive)
  (let* ((p (point)) (q (mark))
;	 (start (setq manip-tab-time-start (current-time)))  ;;;;;;;;;;
	 (lookup-args (find-and-highlight-user-expr p q)))
    (collapse-active-region)
    ;; search for [n] or {n} to retrieve formula number
    (let ((fmla-start (search-backward-regexp "^\\(\\[\\|{\\)"))
	  (fnum nil))
      (when fmla-start
	(forward-char 1)
	(skip-chars-forward "-0-9")
	(setq fnum (buffer-substring-text (1+ fmla-start) (point))))
      (goto-char (point-max))   ;; (end-of-buffer)
      (let* ((all-args (append lookup-args (list fnum ee-form nil)))
	     (find-form (format "%S" `(find-any-subexpr-plus ,@all-args))))
	(pvs-send* find-form nil nil t)))))

;; CL process uses following two functions as callbacks to return
;; results after an invocation via pvs-send*.

(defun accept-find-any-status (status)
  (case status
    (bad-syntax (message "No syntactically valid subexpressions found."))
    (no-match   (message "Not a valid subexpression of current sequent.")))
  nil)

(defvar find-any-segments nil)

;; Text results are sent as multiple string segments to avoid problems
;; from long strings.  The final call to accept-find-any provides the
;; pattern type as a symbol.

(defun accept-find-any (segment)
  (if (symbolp segment)
      (let ((result (apply #'concat (reverse find-any-segments)))
	    (patt-format (case segment
			   (plain   " %S")
			   (ee-form " (~ %S)"))))
	(when patt-format (insert (format patt-format result)))
	(setq find-any-segments nil))
      (setq find-any-segments (cons segment find-any-segments)))
  nil)


;; Search for a syntactically valid (parsable) text string in the region
;; from p to q.  Adjust endpoints over a scan region to find a parsable
;; expression.  Region is bounded by occurrence of interesting characters.

(defun find-and-highlight-user-expr (p q)
  (let* ((left-index  (min p q))
	 (right-index (max p q))
	 (right-adj (progn (goto-char right-index)
			   (save-excursion
			     (search-backward-regexp "[^ \\n\\t]")
			     (forward-char 1)
			     (point))))
	 (scan-endpt-left (save-excursion
			    (search-backward-regexp "[^\])} \\n\\t]")
			    (forward-char 1)  ;;; added 8 Aug 05
			    (point)))
	 (scan-endpt-right
	  (save-excursion (search-forward-regexp "[^\])}0-9A-Za-z!?_ \\n\\t]")
			  (point))))
    (if (> right-adj left-index)
	(list (buffer-substring-text left-index scan-endpt-right)
	      left-index
	      (max 0 (- scan-endpt-left left-index))
	      (- right-adj left-index))
        nil)))


;;; Given a formula number, generate an extended expression based on
;;; syntax matching that evaluates to the entire formula.

(defun fetch-and-map-fnum (&optional num)
  "Retrieve formula by number(s) and insert an extended expression suitable
for matching it (them).  Prompts for the formula to use.  Prefix arg may
also be used to give a formula number."
  (interactive "P")
  (goto-pvs-proof-buffer)
  (let* ((fnums (if (null num)
		    (read-from-minibuffer "Formula number(s) for pattern: ")
		    (int-to-string (prefix-numeric-value num)))))
;    (setq manip-tab-time-start (current-time))
    (pvs-send* (format "(find-any-subexpr-fmla '(%s))" fnums) nil nil t)))


;;============== Utilities for PVS proof maintenance ===============


;; Buffers that can contain proof trees.  Flag indicates go to end.

(defvar pvs-prover-manip-proof-buffer-names
  '(("Proof" nil) ("*Proof*" t)))


;; Find proof node (TAB-y).  Position the cursor at the beginning of a
;; proof label such as "tan_increasing_imp.3.2.2" as found in the prover
;; buffer "*pvs*".  The label will be parsed and the cursor moved to the
;; first selectable proof buffer at the first step of the branch
;; determined by the label.  Currently searches for these buffers:
;;     Proof, *Proof*
;; Preference is given to a buffer already displayed.
;; The "Proof" buffer is created by the edit-proof command.
;; The "*Proof*" buffer is created by the show-current-proof command.

;; Note: it is now possible to use labels found in prover messages such as:
;;     This completes the proof of tan_increasing_imp.3.2.2.
;; The period at the end is recognized and discarded by the label parser.

(defun pvs-prover-manip-find-node ()
  "Sets cursor on node of proof buffer indicated by label from current proof buffer."
  (interactive)
  (unwind-protect
      (let* ((start (point))
	     (split (progn (skip-chars-forward "a-zA-Z_0-9?.")
			   (split-string
			    (buffer-substring-text start (point)) "\\.")))
	     (items (if (equal (last split) '("")) (butlast split) split)))
	(let* ((existing (mapcan #'(lambda (b) (and (car b) (list b)))
				 (mapcar #'(lambda (s)
					     (list (get-buffer (car s))
						   (cadr s)))
					 pvs-prover-manip-proof-buffer-names)))
	       (displayed (mapcan #'(lambda (b)
				      (and (get-buffer-window (car b))
					   (list b)))
				  existing)))
	  (if (or displayed existing)
	      (let ((buf (if displayed (car displayed) (car existing))))
		(pop-to-buffer (car buf))
		(goto-char (point-max))
		(backward-sexp 1)
		(goto-proof-node (cdr items))
		(when (cadr buf)   ;; go to last s-expr, typically (postpone)
		  (up-list 1)
		  (backward-char 1)
		  (backward-sexp 1)))
	      (message "No suitable proof buffer exists."))))
    nil))  ;; no recovery actions right now

;; Cursor placed at beginning of s-expr for (sub)proof

(defun goto-proof-node (items)
  (let ((next-step '(cmd)))
    (down-list 1)
    (forward-sexp 2)
    (backward-sexp 1)      ;; cursor at first s-expr after label
    (unless (null items)
      (do ()               ;; skip ahead to branching point
	  ((not (symbolp (car next-step))))
	(setf next-step (read (current-buffer))))
      (backward-sexp 1)
      (down-list 1)
      (forward-sexp (- (string-to-number (car items)) 1))
      (goto-proof-node (cdr items)))))


;; Interactive function to "expand" the strategy steps of a proof file.
;; Each proof rule is checked against a list of base rules found in
;; the core PVS distribution.  Any strategy name not found there is
;; append with a `$' character so that its next proof will expand into
;; steps found only in the core rule base.  A backup file of the
;; original proofs is saved in a ".sprf" version of the proof file.
;; Finally, the revised proof file is installed to make it current.

(defun expand-strategy-steps (pvs-file)
  (interactive (progn (confirm-not-in-checker)
		      (complete-pvs-file-name
		        "Expand steps for PVS file named: ")))
  (let ((prf-file  (concat pvs-file ".prf"))
	(sprf-file (concat pvs-file ".sprf")))
    (if (file-exists-p prf-file)
	(progn (message "Expanding...")
	       (pvs-send-and-wait 
		 (format "(expand-strategy-steps \"%s\" \"%s\")"
			 prf-file sprf-file))
	       (install-pvs-proof-file pvs-file)
	       (message (format "%s expanded; old proofs saved in %s."
				prf-file sprf-file)))
        (message "No proof file to expand."))))

;; Interactive function to restore the strategy-steps version of a
;; proof file as saved in the ".sprf" file.
;; The restored proof file is installed to make it current.

(defun restore-strategy-steps (pvs-file)
  (interactive (progn (confirm-not-in-checker)
		      (complete-pvs-file-name
		        "Restore steps for PVS file named: ")))
  (let ((prf-file  (concat pvs-file ".prf"))
	(sprf-file (concat pvs-file ".sprf")))
    (if (file-exists-p sprf-file)
	(progn (message "Restoring...")
	       (pvs-send-and-wait 
		 (format "(restore-strategy-steps \"%s\" \"%s\")"
			 prf-file sprf-file))
	       (install-pvs-proof-file pvs-file)
	       (message (format "%s restored to proofs saved in %s."
				prf-file sprf-file)))
        (message "No strategy-steps proof file to restore."))))


;;=========== Alternative PVS commands (Elisp functions) ============

;;; Provide alternative install-proof function that omits check
;;; for unbalanced PVS strings.  Used when editing proofs that
;;; contain `%' characters in parameterized commands or other
;;; sorts of control strings.

(when edit-proof-mode-map
  (define-key edit-proof-mode-map "\C-x\C-s" 'install-proof!))

(defpvs install-proof! edit-proof (&optional step)
  "Installs the edited proof

Installs the edited proof in the Proof buffer to the formula at the
cursor, or the original formula if the current buffer is the \"Proof\"
buffer.  Bypasses checks for unbalanced PVS strings."
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
;;;		(find-unbalanced-pvs-in-strings))  ;; suppress check
		)
	    (error (message (cadr err))))
	  (pop-to-buffer pbuf)
	  (set-buffer buf)
	  (if (equal (buffer-name) "Proof")
	      (install-proof* nil nil step)
	      (let* ((fref (pvs-formula-origin))
		     (fname (pvs-fref-file fref))
		     (fmla (pvs-fref-formula fref))
		     (kind (pvs-fref-kind fref)))
		(install-proof* fname fmla kind step)))))))


;; ---------- Misc. utility functions ----------

;; Given a list of times as returned by (current-time), converted to a
;; floating number of seconds.  If only t0 supplied, return t0's value;
;; otherwise, return all times relative to t0.

(defun time-real-secs (t0 &rest times)
  (let* ((t0-real (+ (cadr t0) (* 0.000001 (caddr t0))))
	 (times-real (list (if times 0.0 t0-real))))
    (dolist (tn times)
      (push (- (+ (cadr tn) (* 0.000001 (caddr tn))) t0-real) times-real))
    (reverse times-real)))


;; ---------- Error recovery facilities ----------

(defun pvs-prover-manip-restore ()
  "Restores state after a Lisp error in prover."
  (interactive)
  (goto-pvs-proof-buffer)
  (goto-char (point-max))
  (insert "(restore)")
  (return-ilisp))


;; ---------- Key Bindings ----------

(define-key pvs-prover-helps-map "z"     'pvs-prover-manip-select)
(define-key pvs-prover-helps-map ","     'pvs-prover-manip-next-expr)
(define-key pvs-prover-helps-map ";"     'pvs-prover-manip-last-optional)
(define-key pvs-prover-helps-map "."     'pvs-prover-manip-last-expr)

(define-key pvs-prover-helps-map "^"     'fetch-and-map-fnum)
(define-key pvs-prover-helps-map "%"
  (lambda () (interactive) (fetch-and-map-expr nil)))
(define-key pvs-prover-helps-map "~"
  (lambda () (interactive) (fetch-and-map-expr t)))

(define-key pvs-prover-helps-map "y"     'pvs-prover-manip-find-node)

(define-key pvs-prover-helps-map "]"     'pvs-prover-manip-restore)


;; ---------- Test only ----------

;(defun pvs-prover-manip-test1 () ...

;(define-key pvs-prover-helps-map "w"     'pvs-prover-manip-test1)


(provide 'pvs-prover-manip)
