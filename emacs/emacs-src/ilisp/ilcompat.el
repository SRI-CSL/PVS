;;; -*- Mode: Emacs-Lisp -*-

;;; ilcompat.el --

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


;;;============================================================================
;;; Global definitions/declarations


(defconst +ilisp-emacs-version-id+
  (cond ((string-match "XEmacs 20" (emacs-version))
         'xemacs-20)
        ((string-match "XEmacs 19" (emacs-version))
         'xemacs-19)
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

;;;============================================================================
;;; Code

(cond ((memq +ilisp-emacs-version-id+ '(xemacs-19 xemacs-20))
       (load "ilxemacs" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-19)
       (load "ilfsf19" nil noninteractive))
      ((eq +ilisp-emacs-version-id+ 'fsf-20)
       (load "ilfsf20" nil noninteractive))
      (t
       (load "ilfsf20" nil noninteractive)) ;; fall through assumption to Emacs 20
      )

;;;============================================================================
;;; Epilogue

(provide 'compat)

;;; end of file -- compat.el --





