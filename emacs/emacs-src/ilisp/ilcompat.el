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


;
; dave_sc - the following needs analysis of where it is used.
;
;(defconst +ilisp-emacs-minor-version-number+
;    (cond ((eq +ilisp-emacs-version-id+ 'fsf-18) 59)
;	  ((or  (eq +ilisp-emacs-version-id+ 'lucid-19)
;		(eq +ilisp-emacs-version-id+ 'lucid-19-new)
;		)
;	   12)				; Does emacs-minor-version work?
;	  ((eq +ilisp-emacs-version-id+ 'xemacs) 14)
;	  (t emacs-minor-version)))


;;;============================================================================
;;; Code

(cond ((memq +ilisp-emacs-version-id+ '(xemacs-19 xemacs-20))
       (load "ilxemacs"))
      ((eq +ilisp-emacs-version-id+ 'fsf-19)
       (load "ilfsf19"))
      ((eq +ilisp-emacs-version-id+ 'fsf-20)
       (load "ilfsf20"))
      (t
       (load "ilfsf20")) ;; fall through assumption to Emacs 20
      )

;;;============================================================================
;;; Epilogue

(provide 'compat)

;;; end of file -- compat.el --





