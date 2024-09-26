;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;; PVS evaluator - emacs end.  dave_sc 10/12/98

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

(defvar pvs-in-checker)

(declare-function pvs-ready "pvs-cmds")
(declare-function pvs-busy "pvs-cmds")
(declare-function pvs-send-and-wait "pvs-ilisp")
(declare-function pvs-bury-output "pvs-ilisp")

(defpvs pvs-lisp-theory typecheck (theoryname)
  "Generates the Lisp code for a given theory and displays it in a buffer"
  (interactive (complete-theory-name "Generate lisp for theory: "))
  (pvs-bury-output)
  (message "Generating Lisp for theory...")
  (pvs-send-and-wait (format "(generate-lisp-for-theory \"%s\")"
			 theoryname) nil nil 'dont-care)
  (let ((buf (pvs-find-lisp-file theoryname)))
    (when buf
      (message "")
      (with-current-buffer buf
	(set (make-local-variable 'pvs-context-sensitive) t)
	(lisp-mode)))))

(defun pvs-find-lisp-file (theoryref)
  (let ((theoryname (car (last (split-string theoryref "#")))))
    (let ((buf (get-buffer (format "%s.lisp" theoryname))))
      (when buf
	(kill-buffer buf)))
    (let ((lisp-file (format "%s%s.lisp" pvs-current-directory theoryname)))
      (when (file-exists-p lisp-file)
	(find-file-read-only-other-window lisp-file)))))

(defpvs pvs-c-theory typecheck (theoryref)
  "Generates the C code for a given theory and displays it in a buffer"
  (interactive (complete-theory-name "Generate C code for theory: "))
  (pvs-bury-output)
  (pvs-message "Generating C code for theory %s..." theoryref)
  (let* ((c-files (pvs-send-and-wait (format "(pvs2c-theory \"%s\")" theoryref)
				     nil nil 'list))
	 (buf (find-file-noselect (car c-files) t)))
    (when buf
      (with-current-buffer buf
	(set (make-local-variable 'pvs-context-sensitive) t)
	(c-mode))
      (pvs-message "Generated C-file %s" (car c-files))
      (pop-to-buffer buf))))

(defpvs pvs-c-file find-file (filename)
  "Generates the C code for a given file"
  (interactive (pvs-complete-file-name "Generate C for file: " (current-pvs-file)))
  (pvs-bury-output)
  (pvs-message (format "Generating C for PVS file %s ..." filename))
  (let* ((pvs-error nil)
	 (c-files (pvs-send-and-wait (format "(pvs2c-pvs-file \"%s\")" filename)
				     nil nil 'list)))
    (ignore c-files)
    (pvs-message "")
    (unless pvs-error
      ;; Ignoring c-files for now, as debugging code gets in the way
      ;; (dolist (c-file (reverse c-files))
      ;; 	(cl-assert (file-exists-p c-file))
      ;; 	(let ((buf (find-file-noselect c-file t)))
      ;; 	  (with-current-buffer buf
      ;; 	    (set (make-local-variable 'pvs-context-sensitive) t)
      ;; 	    (c-mode))))
      ;; (pvs-message "Generated C files %s" c-files)
      (pvs-message "Generated C files")
      )))

(defun pvs-find-C-file (filename)
  (let ((buf (get-buffer (format "%s.c" filename))))
    (when buf (kill-buffer buf)))
  (let ((C-file (format "%s.c" filename)))
    (when (file-exists-p C-file)
      (find-file-read-only-other-window C-file)
      (get-buffer C-file))))




(defpvs pvs-ground-evaluator prove (theory)
  "Invokes the ground evaluator in the context of the given PVS theory"
  (interactive (complete-theory-name "Use context of theory: "))
  (confirm-not-in-checker)
  (unless (pvs-send-and-wait (format "(typechecked\? \"%s\")" theory)
			     nil 'tc nil)
    (error "%s is not typechecked" theory))
  (pvs-evaluator-busy)
  (save-some-pvs-buffers)
  (pvs-bury-output)
  (ilisp-send (format "(evaluation-mode \"%s\")" theory) nil 'pr t))

(defvar pvs-in-evaluator nil)

(defun confirm-not-in-checker ()
  (when (or pvs-in-checker
	    pvs-in-evaluator)
    (unless (equal (buffer-name) ilisp-buffer)
      (pop-to-buffer ilisp-buffer)
      (goto-char (point-max)))
    (error (format "Must exit the %s first"
	       (if pvs-in-checker "checker" "evaluator")))))

(defun pvs-evaluator-busy ()
  (setq pvs-in-evaluator t)
  (pvs-busy))

(defun pvs-evaluator-ready ()
  (setq pvs-in-evaluator nil)
  (pvs-ready))
