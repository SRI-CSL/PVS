;;; -*- Mode: Emacs-Lisp -*-

;;; ilcompat.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(require 'cl)

;;; Global definitions/declarations


(defconst +ilisp-emacs-version-id+
  (cond ((string-match "XEmacs 21" (emacs-version))
         'xemacs-20)
	((string-match "XEmacs 20" (emacs-version))
         'xemacs-20)
        ((string-match "XEmacs 19" (emacs-version))
         'xemacs-19)
	((string-match "XEmacs " (emacs-version))
	 (message "ILISP: Unknown XEmacs.  Assuming XEmacs 20 - best of luck!")
         'xemacs-20)
        ((string-match "Emacs 21" (emacs-version))
         'fsf-20)
	((string-match "Emacs 20" (emacs-version))
         'fsf-20)
        ((string-match "Emacs 19" (emacs-version))
         'fsf-19)
        (t
         (message "ILISP: Unknown Emacs.  Assuming Emacs 20 - best of luck!")
	 'fsf-20))
  "The version of Emacs ILISP is running in.
Declared as '(member xemacs-20 xemacs-19 fsf-20 fsf-19).
Set in ilcompat.el")

(defconst +ilisp-emacs-minor-version-number+
  (cond ((eq +ilisp-emacs-version-id+ 'fsf-18) 59)
	((or  (eq +ilisp-emacs-version-id+ 'lucid-19)
	      (eq +ilisp-emacs-version-id+ 'lucid-19-new)
	      )
	 12)			      ; Does emacs-minor-version work?
	((eq +ilisp-emacs-version-id+ 'xemacs) 14)
	(t emacs-minor-version))
  "The minor version of (X)Emacs ILISP is running in.
Set in ilcompat.el.")


;;; Load Emacs version specific compatibility modules

(cond ((memq +ilisp-emacs-version-id+ '(xemacs-19 xemacs-20))
       (load "ilxemacs" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-19)
       (load "ilfsf19" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-20)
       (load "ilfsf20" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-21)
       (load "ilfsf21" nil noninteractive))
      (t
       ;; fall through assumption to Emacs 20
       (load "ilfsf20" nil noninteractive))
      )

;;; Misc. bug work-arounds and compatibility bindings

(unless (eval-when-compile (ignore-errors (last '(a . b))))
  ;; From Emacs 19.34's cl.el.
  (defun last (x &optional n)
    "Returns the last link in the list LIST.
With optional argument N, returns Nth-to-last link (default 1)."
    (if n
        (let ((m 0) (p x))
          (while (consp p) (incf m) (pop p))
          (if (<= n 0) p
            (if (< n m) (nthcdr (- m n) x) x)))
      (while (consp (cdr x)) (pop x))
      x)))


;;; Epilogue

(provide 'ilcompat)

;;; end of file -- ilcompat.el --
