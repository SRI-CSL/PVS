;;;
;;; pvs-byte-compile.el         dave_sc, 12/7/98
;;;
;;; byte compile all the files necessary for PVS 

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

(message "PVS: byte compilation starting")

; compile in the current directory
(setq load-path (cons "." load-path))

; we want *all* of the byte compilation warnings
(setq byte-compile-warnings t)

; For debugging
;(setq byte-compile-generate-call-tree t)

(defun pvs-compile (filename)
  (let ((elisp-file (format "%s.el" filename))
	(call-file (format "%s.call" filename)))
    (byte-compile-file elisp-file)))

;    (when (get-buffer "*Call-Tree*")
;      (save-excursion
;	(set-buffer "*Call-Tree*")
;	(write-file call-file nil)))))

(let ((pvsfiles '(pvs-abbreviations
		  pvs-browser
		  pvs-cmds
		  pvs-eval
		  pvs-file-list
		  pvs-ilisp
		  pvs-load
		  pvs-macros
		  pvs-menu
		  pvs-mode
		  pvs-print
		  pvs-prover-helps
		  pvs-prover
		  pvs-tcl
		  pvs-utils
		  pvs-view
		  ;;tcl
		  )))
      (mapcar 
	#'(lambda (a) (pvs-compile a))
	pvsfiles))

(message "PVS: byte compilation done")

;;; the rest of this is modified from ilisp-mak.el


(load "ilcompat.el")		; Need to load this beforehand
				; to use the +ilisp-emacs-version-id+
				; constant.

(message ";;; ILISP Compilation for Emacs Version %s" +ilisp-emacs-version-id+)

;; Compile compatibility files
    (progn
      (cond ((memq +ilisp-emacs-version-id+ '(xemacs-19 xemacs-20))
	     (byte-compile "ilxemacs.el"))
	    ((eq +ilisp-emacs-version-id+ 'fsf-19)
	     (byte-compile "ilfsf19.el"))
	    ((eq +ilisp-emacs-version-id+ 'fsf-20)
	     (byte-compile "ilfsf20.el"))
	    (t (error "ILISP Compilation: unrecogninized Emacs version %s"
		      +ilisp-emacs-version-id+)))
      (byte-compile "ilcompat.el"))

;; Other files in the distribution.

    (let ((files '(completer
		   comint-ipc
		   ilisp-def
		   ;;ilisp-el
		   ilisp-sym
		   ilisp-inp
		   ilisp-ind
		   ilisp-prc
		   ilisp-val
		   ilisp-out
		   ilisp-mov
		   ilisp-key
		   ilisp-prn
		   ilisp-low
		   ilisp-doc
		   ilisp-ext
		   ilisp-mod
		   ilisp-dia
		   ilisp-cmt
		   ilisp-rng
		   ilisp-hnd
		   ilisp-utl
		   ilisp-cmp
		   ilisp-kil
		   ilisp-snd
		   ilisp-xfr
		   ilisp-hi
		   ilisp-aut

		   ;; Dialects.
		   ilisp-cl
		   ilisp-acl
		   )))
      (while files
	(byte-compile-file (format "%s.el" (car files)) 0)
	(load (format "%s" (car files)))
	(setq files (cdr files))))

(message "ILISP: byte compilation Done")

;;; end of file -- ilisp-mak.el --

