;;; -*-Mode: Emacs-Lisp-*-

;;; ilisp-xls.el --

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
;;; ILISP Xlisp and Xlisp-Stat dialect definition
;;;

;;; Thanks to John Walker for supplying this file.


(defdialect xlisp "Xlisp" ilisp
  (setq ilisp-load-command "(load \"%s\")"
        ilisp-last-command "*")
  )

(if (not xlisp-program) (setq xlisp-program "xlisp"))

;;;%%Xlisp-Stat

(defdialect xlispstat "Xlisp-Stat" xlisp
  (setq ilisp-binary-extension "fsl"
	;; ilisp-describe-command "(help %s)"))
	))

(if (not xlispstat-program) (setq xlispstat-program "xlispstat"))

;;; endo of file -- ilisp-xls.el --
