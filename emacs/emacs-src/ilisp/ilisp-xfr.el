;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-xfr.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;;
;;; ILISP transfer commands Lisp <-> Emacs
;;;


;; It's too bad that this function copies so much code from comint-send-input.
;; It ought to be a wrapper around it, instead.

(defun return-ilisp ()
  "Grab the current expression with comint-get-old-input.  If we have
a complete sexp, send it.  Otherwise, indent appropriately."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if (not proc) (error "Current buffer has no process")
      (let* ((pmark (process-mark proc))
	     (input (ilisp-get-old-input)))
	(if input
	    (progn 
	      (if (>= (point) pmark)
		  (goto-char (point-max))
		(goto-char pmark)
		(insert input))
	      (if (not ilisp-no-newline) (insert ?\n))
	      (if (and (funcall comint-input-filter input)
		       (or (ring-empty-p (ilisp-get-input-ring))
			   (not (string= (ring-ref (ilisp-get-input-ring) 0)
					 input))))
		  (ilisp-ring-insert (ilisp-get-input-ring) input))
	      (funcall comint-input-sentinel input)
	      ;; Ugh, comint changing under my feet....
	      ;; Note: This used to be
	      ;;        (eq ilisp-emacs-version-id 'gnu-19)
	      ;;       25/11/94 Marco Antoniotti
	      (if (memq +ilisp-emacs-version-id+ '(fsf-19 fsf-20))
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

;;;%%Keyboard mode
(defun raw-keys-ilisp ()
  "Start using raw keyboard mode to send each character typed to the
inferior LISP until a key bound to interactive-keys-ilisp is
encountered.  See also io-bridge-ilisp." 
  (interactive)
  (if (not ilisp-raw-map)
      (let ((map (make-keymap)))
	(fillarray map 'ilisp-send-char)
	(if (string-match "Lucid" emacs-version)
	    ;; not necessary, but friendlier.
 	    (progn
 	      (setq ilisp-completion-map (make-keymap))
;;	      (set-keymap-name ilisp-completion-map 'ilisp-completion-map)
;; 	      (set-keymap-parent ilisp-completion-map lisp-mode-map)
	      ))
	(define-key map "\C-g" 'interactive-keys-ilisp)
	(setq ilisp-raw-map map)))
  (use-local-map ilisp-raw-map)
  (message ilisp-raw-message))

;;;
(defun interactive-keys-ilisp ()
  "Go back to interactive keyboard interactions in the inferior LISP."
  (interactive)
  (use-local-map ilisp-use-map)
  (message "Interactive keyboard mode"))

;;;
(defun ilisp-send-char ()
  "Send the last typed character to the current inferior LISP echoing
if ilisp-raw-echo is T."
  (interactive)
  (if (ilisp-value 'ilisp-raw-echo t)
      (progn
	(goto-char (point-max))
	(insert last-input-char)
	(set-marker (process-mark (ilisp-process)) (point))
	(set-marker comint-last-input-end (point))))
  (process-send-string (ilisp-process) 
		       (make-string 1 last-input-char))
  (message ilisp-raw-message))

;;;
(defun ilisp-raw-handler (process output)
  "Turn on raw keyboard mode."
  (raw-keys-ilisp))
(defun ilisp-interactive-handler (process output)
  "Turn on interactive keyboard mode."
  (interactive-keys-ilisp))

;;;
(defun io-bridge-ilisp ()
  "Set up so that the inferior LISP can turn on EMACS raw mode by
sending ^[1^] and turn it off by sending ^[0^]."
  (interactive)
  (require 'bridge)
  (install-bridge)
  (setq bridge-handlers (cons '("1" . ilisp-raw-handler)
			      (cons '("0" . ilisp-interactive-handler)
				    bridge-handlers))))

;;;%%Debugger interface
(defun delete-char-or-pop-ilisp (arg &optional killflag)
  "Delete ARG characters, or pop break level if at end of buffer.  
Optional second arg KILLFLAG non-nil means kill instead (save in kill ring).
Interactively, ARG is the prefix arg, and KILLFLAG is set if
ARG was explicitly specified."
  (interactive "p")
  (if (eobp)
      (progn
	(message "Pop LISP one level")
	(comint-simple-send (ilisp-process) (ilisp-value 'comint-fix-error)))
      (call-interactively 'delete-char (list arg killflag))))
