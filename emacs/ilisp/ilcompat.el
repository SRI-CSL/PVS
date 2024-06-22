;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; ilcompat.el --
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.

(require 'cl-lib)

(defconst +ilisp-emacs-version-id+
  (cond ((string-match "XEmacs 2" (emacs-version))
         'xemacs-20)
        ((string-match "XEmacs 19" (emacs-version))
         (error "XEmacs 19 no longer supported"))
        ((string-match "GNU Emacs [23][[:digit:]]" (emacs-version))
         'fsf-20)
        ((string-match "Emacs 19" (emacs-version))
         (error "Emacs 19 no longer supported"))
        (t (error "ILISP: Unknown Emacs.")))
  "The version of Emacs ILISP is running in.
Declared as (member \\='(xemacs-20 fsf-20)); Set in ilcompat.el")

;;; Load Emacs version specific compatibility modules
(cond ((eq +ilisp-emacs-version-id+ 'xemacs-20)
       (load "ilxemacs" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-20)
       (load "ilfsf20" nil noninteractive)))

(provide 'ilcompat)
