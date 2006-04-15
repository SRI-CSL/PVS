;;; Ilisp-easy-menu.el --- (easy)menu's on Emacs for Ilisp

;;; Copyright (C) 1996 Holger Schauer
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

;; Author: Holger Schauer <Holger.Schauer@gmd.de>
;; Maintainer: Holger.Schauer@gmd.de
;; Keywords: lisp ilisp extensions menu easymenu
;; Version 0.2
;; Status: Should work with any Emacs and easymenu.el (as by Per Abrahamsen)
;; Created: 1996-10-08
;; Last modified: 1996-10-15 (that's 15th October 1996, folks)

;; Where to get it: http://www.uni-koblenz.de/~schauer/uniemacs.html

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file and it's extensions have been named ilisp-xemacs-menu before. 
;; As it is ensured now to work with any Emacs with easymenu the file
;; itself and all relevant names (of commands and variables) are renamed. 

;; Use this file as an extension to Ilisp 5.7/5.8 
;; (seee http://www.cs.cmu.edu/~campbell/ilisp/index.html)
;; Put it in a place where (X)Emacs can find it and augment your
;; .emacs like the following to use it.
;; (load-library "ilisp-easy-menu")
;; This needs to be loaded prior to Ilisp !
;; This should give you the menu in any source code buffer and any inferior 
;; ilisp buffer. Be careful: the menu is initialized with add-hook
;; on ilisp-mode-hook and lisp-mode-hook, so if you setq these two
;; hooks afterwards you won't get the menu.
;;
;; If you want to use it with Emacs (not XEmacs) you will want to get rid
;; of the old menu. You can do so by applying the following patches (to
;; Ilisp 5.8 - for 5.7 check for ilisp-update-menu and the loading of the
;; menu, i.e. ilisp-menu and ilisp-mnb)
;; For ilisp.el:
;; 185c185,187
;< (if (not (member +ilisp-emacs-version-id+ '(xemacs lucid-19 lucid-19-new)))
;---
;> (if (and
;>      (not (member +ilisp-emacs-version-id+ '(xemacs lucid-19 lucid-19-new)))
;>      (not (featurep 'ilisp-easy-menu)))
;; For ilisp-utl.el:
;127c127,129
;<   (if (not (member +ilisp-emacs-version-id+ '(xemacs lucid-19 lucid-19-new)))
;---
;>   (if (and (not 
;> 	    (member +ilisp-emacs-version-id+ '(xemacs lucid-19 lucid-19-new)))
;> 	   (not (featurep 'ilisp-easy-menu)))

(require 'easymenu)

;; (eval-when (load compile eval)
;;   (when (and ilisp-*use-hyperspec-interface-p*
;; 	     (not (featurep 'hyperspec)))
;;     (load-library "extra/hyperspec")))

(defvar ilisp-cl-easy-menu
  `("Ilisp"
    [ "Load File" load-file-lisp t ]
    [ "Run Ilisp" run-ilisp t ]
    "--"
    ("Evaluate"
     [ "Eval region" eval-region-lisp t ]
     [ "Eval defun" eval-defun-lisp t ]
     [ "Eval next sexp" eval-next-sexp-lisp t ]
     ;; [ "Eval last sexp" eval-last-sexp-lisp t ]
     [ "Eval changes" eval-changes-lisp t ]
     )
    ("Evaluate and Go"
     [ "Eval region" eval-region-and-go-lisp t ]
     [ "Eval defun" eval-defun-and-go-lisp t ]
     [ "Eval next sexp" eval-next-sexp-and-go-lisp t ]
     ;; [ "Eval last sexp" eval-last-sexp-and-go-lisp t ]
     )
    ("Compile"
     [ "File" compile-file-lisp t ]
     [ "Defun" compile-defun-lisp t ]
     [ "Defun and go" compile-defun-and-go-lisp t ]
     [ "Region" compile-region-lisp t ]
     [ "Region and go" compile-region-and-go-lisp t ]
     [ "Compile changes" compile-changes-lisp t ]
     )
    "--"
    ("Editing"
     [ "Edit definitions" edit-definitions-lisp t ]
     [ "Edit next def." next-definition-lisp t ]
     [ "Edit callers" edit-callers-lisp t ]
     [ "Edit next caller" next-caller-lisp t ]
     [ "Insert arguments" arglist-lisp t ]
     "--"
     [ "Find unbalanced paren" find-unbalanced-lisp t ]
     [ "Close all parens" close-all-lisp t ]
     [ "Close and send lisp" close-and-send-lisp t ]
     "--"
     [ "Reindent" reindent-lisp t ]
     [ "Indent sexp" indent-sexp-ilisp t ]
     [ "Indent for comment" lisp-indent-for-comment t ]
     [ "Comment region" comment-region-lisp t ]
     "--"
     [ "Search in Files" search-lisp t ] 
     "--"
     [ "Kill sexp" kill-sexp t ] 
     [ "Kill last sexp" backward-kill-sexp t ]
     "--"
     [ "Macroexpand" macroexpand-lisp t ]
     [ "Macroexpand-1" macroexpand-1-lisp t ]
     "--"
     [ "Begin of def" beginning-of-defun-lisp t ]
     [ "End of defun" end-of-defun-lisp t ]
     )
    ("Documentation"
     [ "Documentation" documentation-lisp t ]
     [ "Describe" describe-lisp t ]
     [ "Inspect" inspect-lisp t ]
     [ "Argument List" arglist-lisp t ]	; Just a test.
     "--"
     ;; With which var can I test if 'fi' is really loaded? Can I just
     ;; use FEATUREP?
     [ "Clman-apropos" fi:clman-apropos nil ] 
     [ "Hyperspec - apropos"
       hyperspec-lookup
       ,ilisp-*use-hyperspec-interface-p* ]
     )
    "--"
    ("Package"
     "--"
     [ "Package" package-lisp t ]
     [ "Set Lisp Package" set-package-lisp t ]
     [ "Set Buffer Package" set-buffer-package-lisp t ]
     )
    "--"
    ("Misc"
     ;; [ "Reset Ilisp" reset-ilisp t ]
     [ "Select Ilisp" select-ilisp t ]
     [ "Switch to lisp" switch-to-lisp t ]
     [ "Abort commands" abort-commands-lisp t ]
     [ "Status of Lisp" status-lisp t ]  
     "--"
     [ "Mark change" mark-change-lisp t ]
     [ "List changes" list-changes-lisp t ]
     [ "Clear changes" clear-changes-lisp t ]
     "--"
     [ "Trace defun" trace-defun-lisp t ]
     )
    "--"
    [ "Reset Ilisp Connection" reset-ilisp t ]
    [ "Repair Ilisp Connection" repair-ilisp t ]
    )
  )

;;; insert "Debug" Menu if ilisp-*enable-ild-support-p*
;;; enable the commands only if inside the debugging loop
;;;
;;; 2000-10-10 17:34:05 rurban
(defconst ilisp-ild-easy-menu
    `("Debug"
     [ "Abort" 	  ild-abort 	(ilisp-ild-p) ]
     [ "Continue" ild-continue 	(ilisp-ild-p) ]
     [ "Next" 	  ild-next 	(ilisp-ild-p) ]
     [ "Previous" ild-previous 	(ilisp-ild-p) ]
     [ "Top" 	  ild-top 	(ilisp-ild-p) ]
     [ "Bottom"   ild-bottom 	(ilisp-ild-p) ]
     [ "Backtrace" ild-backtrace (ilisp-ild-p) ]
     [ "Locals"   ild-locals 	(ilisp-ild-p) ]
     [ "Local"    ild-local 	(ilisp-ild-p) ]
     [ "Return"   ild-return 	(ilisp-ild-p) ]
     [ "Retry"    ild-retry 	(ilisp-ild-p) ]
     [ "Trap on exit" ild-trap-on-exit (ilisp-ild-p) ]
     "--"
     [ "Fast lisp" fast-lisp t ]
     [ "Slow lisp" slow-lisp t ]
     )
    )

(defun ilisp-ild-p ()
  t)

(defun ilisp-insert-menu (menu where what)
  "Insert WHAT into MENU after WHERE"
  (if (position what menu)
      menu
    (let ((i (position (assoc where menu) menu)))
      (setq i    (1+ i)
	    menu (append (butlast menu (- (length menu) i))
			 (list what)
			 (nthcdr i menu))))))
  
(if ilisp-*enable-ild-support-p*
  (setq ilisp-cl-easy-menu 
	(ilisp-insert-menu ilisp-cl-easy-menu "Misc" ilisp-ild-easy-menu)))

;;; ilisp-update-menu
;;;
;;; 19990818 Marco Antoniotti

(defun ilisp-update-menu (status)
  ;; Backward compatibility with old keymap based menus.
  ;; A no-op for the time being.
  )

(defun ilisp-redefine-menu ()
  (easy-menu-remove ilisp-cl-easy-menu)
  (easy-menu-define menubar-ilisp ilisp-mode-map 
		    "Ilisp commands"
		    ilisp-cl-easy-menu)
  (easy-menu-add ilisp-cl-easy-menu 'ilisp-mode-map)
  )

(provide 'ilisp-cl-easy-menu)

;;; Hooks to add the menu.
;;;
;;; Notes:
;;; 19990818 Marco Antoniotti
;;; Since I could have installed a Scheme menu before a CL one, I
;;; could be forced to remove the previous menu.  Now the code does not do
;;; this, but it should.

(add-hook 'ilisp-mode-hook
	  (lambda () 
	    (when (featurep 'easymenu)       
	      (easy-menu-define menubar-ilisp
				ilisp-mode-map 
				"Ilisp commands"
				ilisp-cl-easy-menu) 
	      (easy-menu-add ilisp-cl-easy-menu 'ilisp-mode-map)
	      )))
	 

(add-hook 'lisp-mode-hook
	  (lambda () 
	    (when (featurep 'easymenu)       
	      (easy-menu-define menubar-lisp-ilisp
				lisp-mode-map 
				"lisp commands"
				ilisp-cl-easy-menu) 
	      (when (boundp 'lisp-menu)
		(easy-menu-remove lisp-menu))
	      (easy-menu-add ilisp-cl-easy-menu 'lisp-mode-map)
	      )))

;;; end of file-- ilisp-cl-easy-menu.el --
