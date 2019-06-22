;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-out.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-imenu.el,v 1.5 2002/01/31 14:56:45 mna Exp $

(require 'cl)
(require 'imenu)

;;; modified for a better display of function+arglist! 
;;; let tokens contain spaces and test with string-equal.

(defun imenu--completion-buffer (index-alist &optional prompt)
  "Let the user select from INDEX-ALIST in a completion buffer with PROMPT.

Returns t for rescan and otherwise a position number."
  ;; Create a list for this buffer only when needed.
  (let ((name (thing-at-point 'symbol))
	choice)
    (cond (prompt)
	  ((and name (imenu--in-alist name index-alist))
	   (setq prompt (format "Index item (default %s): " name)))
	  (t (setq prompt "Index item: ")))
    (if (if (featurep 'xemacs)
	    (eq imenu-always-use-completion-buffer-p 'never)
	  (null imenu-use-popup-menu))
  	(setq name (completing-read prompt
  				    index-alist
 				    nil t nil 'imenu--history-list name))
      (save-window-excursion
	;; Display the completion buffer
	(with-output-to-temp-buffer "*Completions*"
	  (display-completion-list
	   (all-completions "" index-alist )))
	(let ((minibuffer-setup-hook
	       (function
		(lambda ()
		  (let ((buffer (current-buffer)))
		    (save-excursion
		      (set-buffer "*Completions*")
		      (setq completion-reference-buffer buffer)))))))
	  ;; Make a completion question
	  (setq name (completing-read prompt
				      index-alist
				      #'string-equal
                                      t nil 'imenu--history-list name)))))
    (cond ((not (stringp name))
	   nil)
	  ((string= name (car imenu--rescan-item))
	   t)
	  (t
	   (setq choice (assoc name index-alist))
	   (if (imenu--subalist-p choice)
	       (imenu--completion-buffer (cdr choice) prompt)
	     choice)))))
;;;---

;;;
;;; Patch for ilisp-imenu
;;; 

;; Intent is to allow users to customize what forms can
;; define types, variables, etc.  At the moment, this is
;; hardcoded in ilisp-imenu-create-lisp-index.
;; This file replaces and enhances that. function.

(defvar ilisp-*defining-form-regexp* "^(def"
  "Regular expression indicating that the form will define something.")

(defvar ilisp-*type-defining-forms*
  '(deftype defstruct defclass define-condition)
  "Symbols that announce the definition of a new lisp type.
Don't change this variable -- rather
customize ilisp-*user-type-defining-forms*")

(defvar ilisp-*user-type-defining-forms* nil
  "*List of user defined symbols which define new lisp types.")

(defvar ilisp-*variable-defining-forms*
  '(defvar defconstant defparameter)
  "Symbols that announce the definition of a lisp variable.
Don't change this variable -- rather customize
ilisp-*user-variable-defining-forms*")

(defvar ilisp-*user-variable-defining-forms* nil
  "*List of user defined symbols which define new lisp variables.")
 
(defvar ilisp-*function-defining-forms* '(defun defmethod defmacro defgeneric)
  "Symbols that announce the definition of a new new lisp function.
Don't change this variable -- rather customize
ilisp-*user-function-defining-forms*")


(defvar ilisp-*user-function-defining-forms* nil
  "*List of user defined symbols which define new lisp functions.")


(defun ilisp-build-optimal-regexp (key)
  "Build an optimal regular expression to match tokens used to define
things of class KEY, which can be `:types' or `:variables'."
  (regexp-opt (mapcar #'symbol-name
		      (remove-duplicates
		       (ecase key
			 (:types (append ilisp-*type-defining-forms*
					 ilisp-*user-type-defining-forms*))
			 (:variables (append ilisp-*variable-defining-forms*
					     ilisp-*user-variable-defining-forms*))
			 (:functions (append ilisp-*function-defining-forms*
					     ilisp-*user-function-defining-forms*)))))))


(defun ilisp-imenu-create-lisp-index ()
  ;; `imenu-create-index-function' is set to this.
  ;; generates a nested index of definitions.
  (let ((index-fun-alist '())
	(index-var-alist '())
        (index-const-alist '())
	(index-type-alist '())
	(index-unknown-alist '())
	(prev-pos nil)
	)
    (goto-char (point-max))
    (imenu-progress-message prev-pos 0)

    ;; This will be a bit slower at runtime, but hey, we don't
    ;; rebuild the index very often, and at least this way,
    ;; we'll get it right.  [ap 13/5/2001]
    (let ((type-defining-form-regexp (ilisp-build-optimal-regexp :types))
	  (variable-defining-form-regexp (ilisp-build-optimal-regexp :variables))
	  (function-defining-form-regexp (ilisp-build-optimal-regexp :functions)))
      ;; Search for the function
      (while (beginning-of-defun)
	(imenu-progress-message prev-pos nil t)
	(save-match-data
	  (and (looking-at ilisp-*defining-form-regexp*)
	       (save-excursion
		 (down-list 1)
		 (cond ((looking-at variable-defining-form-regexp)
			(forward-sexp 2)
			(push (ilisp-imenu-general--name-and-position)
			      index-var-alist))
		       ((looking-at type-defining-form-regexp)
			(forward-sexp 2)
			(push (ilisp-imenu-general--name-and-position)
			      index-type-alist)) 
		       ((looking-at function-defining-form-regexp)
			(forward-sexp 2)
			(push (ilisp-imenu-function--name-and-position)
			      index-fun-alist)) 
		       (t
			(forward-sexp 2)
			(push (ilisp-imenu-general--name-and-position)
			      index-unknown-alist)))))))
      (imenu-progress-message prev-pos 100)
      (when index-var-alist
	(push (cons "Variables" index-var-alist) index-fun-alist))
      (when index-type-alist
	(push (cons "Types" index-type-alist) index-fun-alist))
      (when index-unknown-alist
	(push (cons "Syntax-unknown" index-unknown-alist) index-fun-alist))

      index-fun-alist)))


;; Return the previous+current sexp and the location of the sexp (its
;; beginning) without moving the point.
(defun ilisp-imenu-function--name-and-position ()
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let* ((beg (if imenu-use-markers (point-marker) (point)))
           (end (progn (forward-sexp) (point)))
           (name (buffer-substring beg end))
           (beg2 (progn (forward-sexp) (forward-sexp -1) (point)))
           (end2 (progn (forward-sexp) (point)))
           (args (buffer-substring beg2 end2)))
      (cons (concat name " " args) 
	    beg))))


(defun ilisp-imenu-general--name-and-position ()
  (save-excursion
    (forward-sexp -1)
    ;; [ydi] modified for imenu-use-markers
    (let ((beg (if imenu-use-markers (point-marker) (point)))
	  (end (progn (forward-sexp) (point))))
      (cons (buffer-substring beg end)
	    beg))))


(defun ilisp-imenu-extract-index-name ()
  ;; `imenu-extract-index-name-function' is set to this.
  ;; generates a flat index of definitions in a lisp file.
  (save-match-data
    (and (looking-at "(def")
	 (condition-case nil
	     (progn
	       (down-list 1)
	       (forward-sexp 2)
	       (let ((beg (point))
		     (end (progn (forward-sexp -1) (point))))
		 (buffer-substring beg end)))
	   (error nil)))))

;;;---

;;;###autoload
(defun ilisp-imenu-add-menubar-index ()
  "Add an Imenu \"Index\" entry on the menu bar for the current buffer.

A trivial interface to `imenu-add-to-menubar' suitable for use in a hook."
  (interactive)
  (imenu-add-to-menubar "Index"))


(add-hook 'lisp-mode-hook
          	  (lambda () 
                    (when (featurep 'imenu)
                      (setq imenu-extract-index-name-function
                            'ilisp-imenu-extract-index-name)
                      (setq imenu-create-index-function
                            'ilisp-imenu-create-lisp-index)
                      (ilisp-imenu-add-menubar-index))))

;;; end of file -- ilisp-imenu.el --
