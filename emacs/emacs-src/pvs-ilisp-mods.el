
;;; This is return-ilisp modified to put an extra backslash in front of any
;;; given ones.  Put
;;;  (define-key ilisp-mode-map "\C-m" 'return-ilisp-pvs)
;;; in your .pvsemacs file.


;;;;;;;;;;;THIS ALSO DEFINED IN pvs-ilisp.el

(defun return-ilisp-pvs ()
  "Grab the current expression with comint-get-old-input.  If we have
a complete sexp, send it.  Otherwise, indent appropriately."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc)
	(error "Current buffer has no process")
	(let* ((pmark (process-mark proc))
	       (input1 (ilisp-get-old-input))
	       (input (protect-backslash-for-lisp input1)))
	  (if input
	      (let ((input-ring (ilisp-get-input-ring)))
		(if (>= (point) pmark)
		    (goto-char (point-max))
		    (goto-char pmark)
		    (insert input))
		(if (not ilisp-no-newline) (insert ?\n))
		(if (and (funcall comint-input-filter input)
			 (or (ring-empty-p input-ring)
			     (not (string= (ring-ref input-ring 0) input))))
		    (ilisp-ring-insert input-ring input))
		(funcall comint-input-sentinel input)
		;; Ugh, comint changing under my feet....
		(if (memq ilisp-emacs-version-id '(fsf-19 fsf-20))
		    (setq comint-input-ring-index nil))
		;; Nuke symbol table
		(setq ilisp-original nil)
		(funcall comint-input-sender proc input)
		(set-marker (process-mark proc) (point))
		(set-marker comint-last-input-end (point))
		(goto-char (point-max)))
	      (if (= pmark (point-max)) 
		  (let ((comint-send-newline t))
		    (if (not ilisp-no-newline) (insert ?\n))
		    (set-marker (process-mark proc) (point))
		    (funcall comint-input-sender proc ""))
		  (insert ?\n)
		  (save-restriction
		    (narrow-to-region pmark (point-max))
		    (funcall indent-line-function))))))))

(defun protect-backslash-for-lisp (string)
  (let ((pos (- (length string) 1))
	(chars nil))
    (while (>= pos 0)
      (let ((ch (aref string pos)))
	(when (and (= ch ?\\)
		   ;; \/ but not \\/
		   (or (and (= (car chars) ?/)
			    (or (= pos 0)
				(/= (aref string (1- pos)) ?\\)))
		       ;; /\ but not /\\
		       (and (/= (car chars) ?\\)
			    (> pos 0)
			    (= (aref string (1- pos)) ?/))))
	  (push ch chars))
	(push ch chars)
	(decf pos)))
    (concat chars)))

  (defkey-ilisp "\C-\\s" 'comint-previous-matching-input-from-input)
  (defkey-ilisp "\M-s" 'comint-previous-matching-input-from-input)

  (add-hook 'comint-mode-hook
    '(lambda ()
       (defun comint-previous-input (arg)
	 "Search backwards through input history for match for REGEXP.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, find the next or Nth next match.
Modified from original to not delete earlier command if some other command
intervenes."
	 (interactive "*p")
	 (setq arg (comint-search-arg arg))
	 (unless (memq last-command
		       '(comint-previous-input
			 comint-next-input))
	   (set-marker comint-last-input-start (point)))
	 (let ((pos (comint-previous-matching-input-string-position "." arg)))
	   ;; Has a match been found?
	   (if (null pos)
	       (error "Not found")
	       (setq comint-input-ring-index pos)
	       (message "History item: %d" (1+ pos))
	       (delete-region 
		;; Can't use kill-region as it sets this-command
		comint-last-input-start (point))
	       (insert (ring-ref comint-input-ring pos)))))))
 