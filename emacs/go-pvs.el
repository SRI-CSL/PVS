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
(setq inhibit-startup-screen t)

(defconst pvs-emacs-system
  (let ((emacsvers (emacs-version)))
    (string-match "\\(X?\\)Emacs \\([0-9][0-9]\\)" emacsvers)
    (intern (format "%semacs%s"
		(downcase (match-string 1 emacsvers))
	      (match-string 2 emacsvers)))))

(if (getenv "PVSPATH")
    (defconst pvs-path (if (string-match "/$" (getenv "PVSPATH"))
			(substring (getenv "PVSPATH") 0 -1)
			(getenv "PVSPATH"))
      "The pathname of the PVS directory.
       Set in go-pvs.el to be the environment variable PVSPATH,
       which is set by the pvs shell script")
    (error "PVSPATH environment variable must be set"))

(defvar pvs-original-load-path load-path)

(setq load-path
  (append (list pvs-path
	        (concat pvs-path "/emacs/"
			(prin1-to-string pvs-emacs-system)))
	  (when (file-exists-p (concat pvs-path "/emacs"))
		(list (concat pvs-path "/emacs")
		      (concat pvs-path "/emacs/ilisp")))
          load-path))

;;; Maybe check at this point for the correct byte compilation of
;;; pvs-load.elc - if not, can complain now and give instructions for
;;; re-compilation.
 
(load "pvs-load" nil noninteractive nil)
