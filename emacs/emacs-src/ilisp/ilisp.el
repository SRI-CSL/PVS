;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp.el --

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

     

;;; Author: Chris McConnell <ccm@cs.cmu.edu>
;;; Maintainer: The Net <ilisp@naggum.no>
;;; Created: 14 Jun 1994
;;; Version: 5.8
;;; Keywords: lisp common-lisp scheme comint

;;; This file may become part of GNU Emacs in the near future.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; *****************************************************************
;;; Please read the texinfo file (via m-x info in emacs or tex it and
;;; print it out) for installation instructions.
;;; *****************************************************************

;;; This file defines a generic LISP interface that can be customized
;;; to match a specific LISP dialect.  Support is already provided for
;;; a number of common LISP dialects.  Lucid, Allegro and CMU are
;;; fully supported.  Other LISP dialects are missing features like
;;; arglist and find-source.

;;; Since this is built on top of the general command-interpreter-in-
;;; a-buffer mode (comint mode), it shares a common base
;;; functionality, and a common set of bindings, with all modes
;;; derived from comint mode.  This makes it easier to use.

;;; For documentation on the functionality provided by comint mode,
;;; and the hooks available for customizing it, see the file
;;; comint.el.

;;; Throughout this file you will find comment lines with %'s on them.
;;; These lines define sections for outline mode which I use while
;;; programming to temporarily hide code.

;;; See the documentation for ILISP mode, or read texinfo document for
;;; information.  All of the EMACS function names begin or end with
;;; lisp or ilisp to separate ilisp functions from functions in other
;;; packages.  Functions that work only in lisp buffers or that work
;;; in both lisp buffers and inferior lisp buffers use lisp, all other
;;; functions use ilisp.  If a function is intended to be used
;;; interactively, then the lisp or ilisp comes at the end of the
;;; function name, otherwise at the start.

;;;%%KNOWN BUGS
;;; 
;;; If you type multiple things to the top level before you get a
;;; prompt, the LISP may be running with the status light indicating
;;; ready.  This is because I have no way to distinguish between input
;;; to a program and that to the top level.
;;;
;;; When running a lisp on Ultrix, you need to set ilisp-program to
;;; "/bin/sh -c your/path/your-lisp-image".
;;; 
;;; If you get lisp output breaking up in weird places it almost
;;; certainly means that comint-prompt-regexp is not precise enough.
;;;
;;; I would like to eat Lucid's return from break in the process
;;; filter, but I can't tell how many newlines to eat after.


;;;%%CONTRIBUTORS

;; Recent contributors include (in alphabetical order):

;; Marco Antoniotti, Robert P. Goldman, Larry Hunter, Eyvind Ness, 
;; Ivan Vazquez, Fred White



(load "ilcompat" nil noninteractive)                       ; emacs version specific stuff
(load "comint-ipc" nil noninteractive)                     ; comint IPC extensions

(load "ilisp-def" nil noninteractive)
(load "ilisp-el" nil noninteractive)
(load "ilisp-sym" nil noninteractive)
(load "ilisp-inp" nil noninteractive)
(load "ilisp-ind" nil noninteractive)

(load "ilisp-prc" nil noninteractive)
(load "ilisp-val" nil noninteractive)
(load "ilisp-out" nil noninteractive)
(load "ilisp-mov" nil noninteractive)
(load "ilisp-key" nil noninteractive)
(load "ilisp-prn" nil noninteractive)
(load "ilisp-low" nil noninteractive)
(load "ilisp-doc" nil noninteractive)
(load "ilisp-ext" nil noninteractive)	
(load "ilisp-mod" nil noninteractive)
(load "ilisp-dia" nil noninteractive)
(load "ilisp-cmt" nil noninteractive)
(load "ilisp-rng" nil noninteractive)
(load "ilisp-hnd" nil noninteractive)
(load "ilisp-utl" nil noninteractive)
(load "ilisp-cmp" nil noninteractive)
(load "ilisp-kil" nil noninteractive)
(load "ilisp-snd" nil noninteractive)
(load "ilisp-xfr" nil noninteractive)
(load "ilisp-hi" nil noninteractive)
(load "ilisp-aut" nil noninteractive)

(load "ilisp-cl" nil noninteractive)
(load "ilisp-acl" nil noninteractive)

;;; Create the keymaps before running the hooks.
;;; This is necessary if you want the lispm bindings in the load
;;; hook. Otherwise you need to put it AFTER the running of the hooks

;;; (if (not ilisp-mode-map) (ilisp-bindings))


;;; Now run the hooks.

(run-hooks 'ilisp-site-hook)
(run-hooks 'ilisp-load-hook)		; It seem s more reasonable.

(if (not ilisp-mode-map) (ilisp-bindings))

;;; Optional:
; (load "ilisp-menu")
;(if (not (member +ilisp-emacs-version-id+ '(xemacs lucid-19 lucid-19-new)))
;    (load "ilisp-mnb"))

(provide 'ilisp)
