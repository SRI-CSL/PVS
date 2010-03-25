;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-mov.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;%%Movement
(defun bol-ilisp (arg)
  "Goes to the beginning of line, then skips past the prompt, if any.
If a prefix argument is given (\\[universal-argument]), then no prompt skip 
-- go straight to column 0.

The prompt skip is done by skipping text matching the regular expression
comint-prompt-regexp or ilisp-other-prompt, both buffer local variables."
  (interactive "P")
  (beginning-of-line)
  (if (null arg) 
      (or (comint-skip-prompt)
	  (if ilisp-other-prompt
	      (let ((comint-prompt-regexp ilisp-other-prompt))
		(comint-skip-prompt))))))

;;;
(defun beginning-of-defun-lisp (&optional stay)
  "Go to the next left paren that starts at the left margin or after a
prompt in an ILISP buffer.  If optional STAY, then do not move to
prior defun if at the start of one in an ilisp mode."
  (interactive)
  (if (memq major-mode ilisp-modes)
      (let ((point (point)))
	(if (and (not stay) (= point (lisp-input-start)))
	    (progn (forward-line -1) (lisp-input-start))))
      (beginning-of-defun)))

;;;
(defun end-of-defun-lisp ()
  "Go to the next left paren that starts at the left margin or after a
prompt in an ILISP buffer and go to the end of the expression."
  (interactive)
  (let ((point (point)))
    (if (memq major-mode ilisp-modes)
	(beginning-of-defun-lisp t)
	(if (or (lisp-in-string)
		(progn (beginning-of-line)
		       (re-search-forward "^[ \t\n]*[^; \t\n]" nil t)
		       (back-to-indentation)
		       (not (bolp))))
	    (beginning-of-defun-lisp t)))
    (lisp-end-defun-text t)
    (if (= point (point))		;Already at end so move to next end
	(progn
	  (if (memq major-mode ilisp-modes)
	      (re-search-forward comint-prompt-regexp (point-max) t)
	      (lisp-skip (point-max)))
	  (if (not (or (eobp)
		       (= (char-after (point)) ?\n)))
	      (lisp-end-defun-text t))))))

;;;
(defun lisp-defun-begin ()
  "Go to the start of the containing defun and return point."
  (let (begin)
    (if (memq major-mode ilisp-modes)
	(lisp-input-start)
	(if (or (eobp) (not (and (bolp) (= (char-after (point)) ?\())))
	    (beginning-of-defun))
	(point))))

;;;
(defun lisp-defun-end (&optional no-errorp at-beginp)
  "Go to the end of the containing defun and return point or nil if
there is no end."
  (if (not at-beginp) (lisp-defun-begin))
  (condition-case ()
      (progn
	(lisp-skip (point-max))		;To skip comments on defun-end
	(forward-sexp)
	(point))
    (error (if no-errorp nil (error "Unbalanced parentheses")))))

;;;
(defun lisp-find-next-start ()
  "Find the start of the next line at the left margin that starts with
a character besides whitespace, a \) or ;;; and return the
point."
  (if (eobp)
      (point-max)
      (save-excursion
	(forward-char)
	(if (re-search-forward "^\\(\\(;;;\\)\\|\\([^ \t\n\);]\\)\\)" nil t)
	    (match-beginning 0)
	    (point-max)))))

;;;
(defun lisp-end-defun-text (&optional at-start)
  "Go the end of the text associated with the current defun and return
point.  The end is the last character before whitespace leading to
a left paren or ;;; at the left margin unless it is in a string."
  (if (not at-start) (lisp-defun-begin))
  (let ((point (point))
	(boundary (lisp-find-next-start))
	(final (save-excursion
		 (condition-case ()
		     (progn (forward-sexp) (point))
		   (error (point-max))))))
    ;; Find the next line starting at the left margin and then check
    ;; to see if it is in a string. 
    (while (progn
	     (skip-chars-forward "^\"" boundary) ;To the next string
	     (if (= (point) boundary)	
		 nil			;No quote found and at limit
		 (let ((string-boundary ;Start of next defun
			(save-excursion
			  (if (re-search-forward "^\(\\|^;;;" nil t)
			      (match-beginning 0)
			      (point-max)))))
		   (if (condition-case ()
			   (progn (forward-sexp) t)
			 (error (goto-char string-boundary) nil))
		       (if (>= (point) boundary)
			   ;; Boundary was in string
			   (if (> (point) string-boundary)
			       (progn	;String ended in next defun
				 (goto-char string-boundary)
				 nil)
			       (if (> (setq boundary
					    (lisp-find-next-start))
				      final)
				   ;; Normal defun
				   (progn (goto-char final) nil)
				   t))
			   t)
		       ;; Unclosed string
		       nil)))))
    (re-search-backward  "^[^; \t\n]\\|^[^;\n][ \t]*[^ \t\n]" point t)
    (end-of-line)
    (skip-chars-backward " \t")
    (if (< (point) point)
	(goto-char point)
	(if (save-excursion
	      (let ((point (point)))
		(beginning-of-line)
		(if comment-start (search-forward comment-start point t))))
	    (progn (forward-line 1) (indent-line-ilisp)))
	(point))))

;;;
(defun lisp-in-comment (test)
  "Return T if you are in a comment."
  (beginning-of-line)
  (and (looking-at test)
       (not (= (match-end 0)
	       (progn (end-of-line) (point))))))

;;;
(defun lisp-in-string (&optional begin end)
  "Return the string region that immediately follows/precedes point or
that contains point in optional region BEGIN to END.  If point is in
region, T will be returned as well."
  (save-excursion
    (if (not begin)
	(save-excursion
	  (setq end (lisp-end-defun-text)
		begin (lisp-defun-begin))))
    (let* ((point (progn (skip-chars-forward " \t") (point)))
	   (done nil))
      (goto-char begin)
      (while (and (< (point) end) (not done))
	(skip-chars-forward "^\"" end)
	(setq begin (point))
	(if (< begin end)
	    (if (and (not (bobp)) (= (char-after (1- begin)) ??))
		(forward-char)
		(if (condition-case () (progn (forward-sexp) (<= (point) end))
		      (error nil))
		    (progn		;After string
		      (skip-chars-forward " \t")
		      (if (or (= begin point) (= point (point)))
			  (setq done (list begin (point) nil))
			  (if (and (< begin point) (< point (point)))
			      (setq done (list begin (point) t)))))
		    ;; In string at end of buffer
		    (setq done (list begin end t))))))
      done)))
