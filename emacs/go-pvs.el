;;;
;;; go-pvs.el dave_sc 12/1/98
;;;
;;; Try to determine the version of Emacs being run, setup the load-path,
;;; and load the real pvs-load file from the byte compiled directory.

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(setq debug-on-error t)

(defconst pvs-emacs-system
  (cond ((or (string-match "XEmacs 21" (emacs-version))
	     (string-match "^21\..*XEmacs" emacs-version))
	 'xemacs21)
	((string-match "XEmacs 20" (emacs-version))
	 'xemacs20)
	((string-match "XEmacs 19" (emacs-version))
	 'xemacs19)
	((string-match "Emacs 22" (emacs-version))
	 'emacs22)
	((string-match "Emacs 2" (emacs-version))
	 'emacs20)
	((string-match "Emacs 19" (emacs-version))
	 'emacs19)
	(t
	 (message "Your Emacs version is not known by PVS - assuming Emacs 20")
         'emacs20))
  "This is deprecated - it's difficult to keep up with version changes.
Instead use, e.g.,
      (and (string-match \"GNU Emacs\" (emacs-version))
	   (boundp 'emacs-major-version)
	   (>= emacs-major-version 20)
This is kept in case users reference it.")

(if (getenv "PVSPATH")
    (defconst pvs-path (if (string-match "/$" (getenv "PVSPATH"))
			(substring (getenv "PVSPATH") 0 -1)
			(getenv "PVSPATH"))
      "The pathname of the PVS directory.
       Set in go-pvs.el to be the environment variable PVSPATH,
       which is set by the pvs shell script")
    (error "PVSPATH environment variable must be set"))

(setq load-path
  (append (list pvs-path
	        (concat pvs-path "/emacs/"
			(prin1-to-string pvs-emacs-system)))
	  (when (file-exists-p (concat pvs-path "/emacs/emacs-src"))
		(list (concat pvs-path "/emacs/emacs-src")
		      (concat pvs-path "/emacs/emacs-src/ilisp")))
          load-path))

;;; Maybe check at this point for the correct byte compilation of
;;; pvs-load.elc - if not, can complain now and give instructions for
;;; re-compilation.
 
(load "pvs-load" nil noninteractive nil)
