;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-ind.el --

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
;;; ILISP indentation
;;;


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
