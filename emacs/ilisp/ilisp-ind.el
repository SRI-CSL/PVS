;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-ind.el --
;;; ILISP indentation
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$


;;;%Indentation
(defun indent-line-ilisp (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one.  This is restricted to the current buffer input."
  (interactive "P")
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (lisp-indent-line whole-exp)))

;;;
(defun indent-sexp-ilisp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (save-restriction
    (if (memq major-mode ilisp-modes)
	(narrow-to-region (save-excursion (lisp-input-start)) (point-max)))
    (indent-sexp)))

;;; end of file -- ilisp-ind.el --
