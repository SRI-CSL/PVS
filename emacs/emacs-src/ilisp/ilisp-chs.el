;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-chs.el --

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
;;; CLISP Common Lisp by Bruno Haible and XX Stoll dialect definition
;;;

;;; clisp-hs-check-prompt doesn't after the first break because the
;;; number of ">" characters doesn't increase.

(defun clisp-hs-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "Break>" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "Break>" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect clisp-hs "CLISP H.S." clisp
  (setq comint-prompt-regexp "^\\([0-9]+\\. Break>\\|>\\)"
        ilisp-error-regexp "^\\*\\* "
        ilisp-binary-extension "fas"
        comint-fix-error "Abort"
        comint-continue "Continue"
	comint-prompt-status
	(function
	 (lambda (old line)
	   (comint-prompt-status old line 'clisp-hs-check-prompt)))))

(if (not clisp-hs-program) (setq clisp-hs-program "clisp"))

(provide 'ilisp-chs)
