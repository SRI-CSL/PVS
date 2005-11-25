;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cmp.el --
;;; ILISP completion
;;; The basic idea behind the completion stuff is to use as much of
;;; the standard Emacs stuff as possible.  The extensions here go out
;;; to the inferior LISP to complete symbols if necessary.  
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(defun ilisp-display-choices (symbol choices)
  "Display the possible choices for SYMBOL in alist CHOICES."
  (with-output-to-temp-buffer "*Completions*"
    (display-completion-list
     (sort 
      (all-completions (lisp-symbol-name symbol) choices)
      'string-lessp))))

;;;%%ilisp-can-complete
(defun ilisp-can-complete (symbol function-p)
  "Return T if ilisp completion can complete SYMBOL from the current table."
  (and ilisp-original 
       (string= (lisp-symbol-package ilisp-original) 
		(lisp-symbol-package symbol))
       (string= (lisp-symbol-delimiter ilisp-original)
		(lisp-symbol-delimiter symbol))
       (lisp-prefix-p (lisp-symbol-name ilisp-original)
		      (lisp-symbol-name symbol))
       (eq function-p ilisp-original-function-p)))

;;;%%ilisp-complete
(defun ilisp-complete (symbol &optional function-p)
  "Return the possible completions for symbol from the inferior LISP.
The type of the result is a list.  If FUNCTION-P is T, only symbols
with function bindings will be considered.  If no package is specified
the buffer package will be used."
  (let* ((choices-string
	  (ilisp-send 
	   (format  (ilisp-value 'ilisp-complete-command) 
		    (lisp-symbol-name symbol) (lisp-symbol-package symbol)
		    function-p
		    (string= (lisp-symbol-delimiter symbol) ":")
		    ilisp-*prefix-match*)
	   (if (not ilisp-complete)
	       (concat "Complete " 
		       (if function-p "function ")
		       (lisp-buffer-symbol symbol)))
	   'complete))
	 choices)
    (if (or (ilisp-value 'comint-errorp t)
	    (ignore-errors (string-match ilisp-error-regexp choices-string)))
	(setq choices 'error)
      (setq choices (read choices-string)
	    choices (if (eq choices 'NIL) nil choices)))
    (unless (listp choices)
      (lisp-display-output choices-string)
      (error "Error completing %s" (lisp-buffer-symbol symbol)))
    (setq ilisp-original symbol
	  ilisp-original-function-p function-p
	  ilisp-original-table choices)))

;;;%%ilisp-completion-table
(defun ilisp-completion-table (symbol function-p)
  "Return the completion table for SYMBOL trying to use the current one.
If FUNCTION-P is T, only symbols with function cells will be returned."
  (if (ilisp-can-complete symbol function-p) 
      ilisp-original-table
      (ilisp-complete symbol function-p)))

;;;%%Minibuffer completion
(defun ilisp-restore-prefix ()
  "Restore the prefix from ilisp-mini-prefix at the start of the minibuffer."
  (if ilisp-mini-prefix
      (save-excursion
	(goto-char (ilisp-minibuffer-prompt-end))
	(insert ilisp-mini-prefix)
	(setq ilisp-mini-prefix nil))))

;;; Support for Emacs 21 minibuffer prompt
(defun ilisp-minibuffer-prompt-end ()
  (if (fboundp 'minibuffer-prompt-end)
      (minibuffer-prompt-end)
    (point-min)))	       

;;;
(defun ilisp-current-choice ()
  "Set up the minibuffer completion table for the current symbol.
If there is a paren at the start of the minibuffer, or there is not an
ilisp-table, this will be from the inferior LISP.  Otherwise, it will
be the ilisp-table."
  (if (or (null ilisp-table) (eq (char-after (ilisp-minibuffer-prompt-end)) ?\())
      (progn
	(let* ((symbol-info (lisp-previous-symbol))
	       (symbol (car symbol-info)))
	  (setq minibuffer-completion-table 
		(ilisp-completion-table symbol ilisp-completion-function-p)))
	(save-excursion 
	  (skip-chars-backward "^: \(")
	  (setq ilisp-mini-prefix (buffer-substring (ilisp-minibuffer-prompt-end) (point)))
	  (delete-region (ilisp-minibuffer-prompt-end) (point)))
	;; Nothing can match this table
	(if (not minibuffer-completion-table)
	    (setq minibuffer-completion-table '((" ")))))
      (setq minibuffer-completion-table ilisp-table
	    minibuffer-completion-predicate nil)))

;;;%%Commands
(defvar ilisp-completion-help
  (lookup-key minibuffer-local-must-match-map "?"))
(defun ilisp-completion-help ()
  "Inferior LISP minibuffer completion help."
  (interactive)
  (ilisp-current-choice) 
  (funcall ilisp-completion-help)
  (ilisp-restore-prefix))

;;;
(defvar ilisp-completion
  (lookup-key minibuffer-local-must-match-map "\t"))
(defun ilisp-completion ()
  "Inferior LISP minibuffer complete."
  (interactive)
  (ilisp-current-choice)
  (funcall ilisp-completion)
  (ilisp-restore-prefix))

;;;
(defvar ilisp-completion-word
  (lookup-key minibuffer-local-must-match-map " "))
(defun ilisp-completion-word ()
  "Inferior LISP minibuffer complete word."
  (interactive)
  (if (eq (char-after (ilisp-minibuffer-prompt-end)) ?\()
      (insert " ")
      (ilisp-current-choice)
      (funcall ilisp-completion-word)
      (ilisp-restore-prefix)))

;;;
(defun ilisp-completion-paren ()
  "Only allow a paren if ilisp-paren is T."
  (interactive)
  (if ilisp-paren 
      (if (or (eq last-input-char ?\() (eq (char-after (ilisp-minibuffer-prompt-end)) ?\())
	  (insert last-input-char)
	  (beep))
      (beep)))
      
;;; 
(defvar ilisp-completion-exit 
  (lookup-key minibuffer-local-must-match-map "\n"))
(defun ilisp-completion-exit ()
  "Inferior LISP completion complete and exit."
  (interactive)
  (if (eq (char-after (ilisp-minibuffer-prompt-end)) ?\()
      (progn (find-unbalanced-lisp nil)
	     (exit-minibuffer))
      (if ilisp-no-complete
	  (exit-minibuffer)
	  (if (= (ilisp-minibuffer-prompt-end) (point-max))
	      (exit-minibuffer)
	      (ilisp-current-choice)
	      (unwind-protect (funcall ilisp-completion-exit)
		(ilisp-restore-prefix))))))

;;;%%ilisp-completer
(defun ilisp-completer (symbol function-p)
  "Complete SYMBOL from the inferior LISP.
If FUNCTION-P is T, only function symbols are returned.
Return (SYMBOL LCS-SYMBOL CHOICES UNIQUEP)."
  (let* ((name (lisp-symbol-name symbol))
	 (table (ilisp-completion-table symbol function-p))
	 (choice (and table (try-completion name table))))
    (cond ((eq choice t)		; Name is it
	   (list symbol symbol nil t))
	  ((string= name choice)	; Name is LCS
	   (list symbol symbol (all-completions name table) nil))
	  (choice			; New LCS
	   (let ((symbol
		  (lisp-symbol (lisp-symbol-package symbol) 
			       (lisp-symbol-delimiter symbol)
			       choice)))
	     (list symbol symbol (all-completions choice table) nil)))
	  ((and (not ilisp-*prefix-match*) table)	;Try partial matches
	   (let ((matches
		  (completer name table nil (regexp-quote completer-words))))
	     (cons (lisp-symbol (lisp-symbol-package symbol)
				(lisp-symbol-delimiter symbol)
				(car matches))
		   (cons  (lisp-symbol (lisp-symbol-package symbol)
				       (lisp-symbol-delimiter symbol)
				       (car (cdr matches)))
			  (cdr (cdr matches)))))))))


;;;%%ilisp-read
(defun ilisp-completion-map ()
  "Set up the ilisp-completion-map from lisp-mode-map for the ilisp
readers and return it."
  (if (not ilisp-completion-map)
      (progn
	(if (fboundp 'set-keymap-parent)
	    (progn
	      (setq ilisp-completion-map (make-sparse-keymap))
	      (set-keymap-parent ilisp-completion-map lisp-mode-map))
	  (setq ilisp-completion-map (copy-keymap lisp-mode-map)))
	(define-key ilisp-completion-map " "  'ilisp-completion-word)
	(define-key ilisp-completion-map "\t" 'ilisp-completion)
	(define-key ilisp-completion-map "?" 'ilisp-completion-help)
	(define-key ilisp-completion-map "\M-\t" 'ilisp-completion)
	(define-key ilisp-completion-map "\n" 'ilisp-completion-exit)
	(define-key ilisp-completion-map "\r" 'ilisp-completion-exit)
	(define-key ilisp-completion-map "\C-g" 'abort-recursive-edit)
	(define-key ilisp-completion-map "(" 'ilisp-completion-paren)
	(define-key ilisp-completion-map ")" 'ilisp-completion-paren)
	(define-key ilisp-completion-map "'" nil)
	(define-key ilisp-completion-map "#" nil)
	(define-key ilisp-completion-map "\"" nil)))
  ilisp-completion-map)

;;;
(defun ilisp-read (prompt &optional initial-contents)
  "PROMPT in the minibuffer and return the result.
The optional INITIAL-CONTENTS may be specified as an initial value
Completion of symbols though the inferior LISP is allowed."
  (let ((ilisp-complete t)
	(ilisp-paren t)
	(ilisp-no-complete t)
	(ilisp-completion-package (lisp-buffer-package)))
    (read-from-minibuffer prompt initial-contents
			  (ilisp-completion-map))))

;;;%%lisp-read-program
(defvar lisp-program-map nil
  "Minibuffer map for reading a program and arguments.")


;;;
(defun lisp-read-program (prompt &optional initial)
  "Read a program with PROMPT and INITIAL.
TAB or Esc-TAB will complete filenames."
  (unless lisp-program-map
    (if (fboundp 'set-keymap-parent)
	(progn
	  (setq lisp-program-map (make-sparse-keymap))
	  (set-keymap-parent lisp-program-map minibuffer-local-map))
      (setq lisp-program-map (copy-keymap minibuffer-local-map)))
    (define-key lisp-program-map "\M-\t" 'comint-dynamic-complete)
    (define-key lisp-program-map "\t" 'comint-dynamic-complete)
    (define-key lisp-program-map "?" 'comint-dynamic-list-completions))
  (read-from-minibuffer prompt initial lisp-program-map))


;;;%%ilisp-read-symbol
(defun ilisp-read-symbol (prompt &optional default function-p no-complete)
  "PROMPT in the minibuffer and return a symbol from the inferior LISP.
PROMPT may use an optional DEFAULT. If FUNCTION-P is T, only symbols with
function values will be returned.  If NO-COMPLETE is T, then
uncompleted symbols will be allowed."
  (let* ((ilisp-complete t)
	 (ilisp-no-complete no-complete)
	 (ilisp-completion-package (lisp-buffer-package))
	 (ilisp-completion-function-p function-p)
	 (string (read-from-minibuffer prompt nil (ilisp-completion-map))))
    (if (equal string "")
	default
	(lisp-string-to-symbol string))))

;;;%%ilisp-completing-read
(defun ilisp-completing-read (prompt table &optional default)
  "Read with PROMPT from an alist of TABLE.  No input returns DEFAULT.
Symbols are from table, other specs are in parentheses."
  (let* ((ilisp-complete t)
	 (ilisp-table table)
	 (ilisp-completion-package (lisp-buffer-package))
	 (ilisp-paren
	  (let ((entry table) (done nil))
	    (while (and entry (not done))
	      (setq done (= (elt (car (car entry)) 0) ?\()
		    entry (cdr entry)))
	    done))
	 (string (read-from-minibuffer prompt nil (ilisp-completion-map))))
    (if (string= string "") default string)))



;;;%%complete-lisp
(autoload 'complete "completion" "Complete previous symbol." t)
(defun complete-lisp (mode)
  "Complete the current symbol using information from the current ILISP buffer.
If in a string, complete as a filename.  If called with
a positive prefix force all symbols to be considered.  If called with
a negative prefix, undo the last completion.  Partial completion is
allowed unless ilisp-*prefix-match* is T.  If a symbol starts after a
left paren or #', then only function symbols will be considered.
Package specifications are also allowed and the distinction between
internal and exported symbols is considered."
  (interactive "P")
  (if (< (prefix-numeric-value mode) 0)
      (completer-undo)
      (let* ((filep (save-excursion
		      (skip-chars-backward "^ \t\n")
		      (= (or (char-after (point)) ?\") ?\")))
	     )
	(if filep
	    (comint-dynamic-complete)
	    ;; (ilisp-pathname-complete)
	    (let* ((symbol-info (lisp-previous-symbol))
		   (symbol (car symbol-info))
		   (name (lisp-symbol-name symbol))
		   (choice (ilisp-completer 
			    symbol 
			    (if (not mode) (car (cdr symbol-info)))))
		   (match (lisp-buffer-symbol (car choice)))
		   (lcs (lisp-buffer-symbol (car (cdr choice))))
		   (choices (car (cdr (cdr choice))))
		   (unique (car (cdr (cdr (cdr choice))))))
	      (skip-chars-backward " \t\n")
	      (completer-goto match lcs choices unique 
			      (ilisp-value 'ilisp-symbol-delimiters)
			      completer-words)))
	(message "Completed"))))


;;; ilisp-pathname-complete --
;;; Incomplete :) function.  You are most welcome to provide an
;;; implementation complying with the IIR #1 appeared on ILISP@CONS.ORG.
;;; 19990709 Marco Antoniotti

(defun ilisp-pathname-complete ()
  "Completes the filename, trying to translate LOGICAL-PATHNAMES as well."
  (let ((maybe-logical-pathname-p (save-excursion
				    (skip-chars-backward "^ :\t\n")
				    (= (char-after (point)) ?\:)))
	)
    (if (not maybe-logical-pathname-p)
	(comint-dynamic-complete)
	())))

;;; end of file -- ilisp-cmp.el --
