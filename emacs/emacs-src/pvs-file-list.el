;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-file-list.el -- 
;; Author          : Sam Owre
;; Created On      : Fri Mar 18 01:53:07 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Oct 15 02:03:20 1995
;; Update Count    : 3
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;; PVS File List Mode

(eval-when-compile (require 'pvs-macros))

(defvar *pvs-list-files-directory* nil)

(defvar pvs-file-list-mode-map nil "")
(if pvs-file-list-mode-map ()
    (setq pvs-file-list-mode-map (make-keymap))
    (suppress-keymap pvs-file-list-mode-map t)
    (define-key pvs-file-list-mode-map "s" 'pvs-file-list-select)
    (define-key pvs-file-list-mode-map "v" 'pvs-file-list-view)
    (define-key pvs-file-list-mode-map "i" 'pvs-file-list-import)
    (define-key pvs-file-list-mode-map "n" 'next-line)
    (define-key pvs-file-list-mode-map "p" 'previous-line)
    (define-key pvs-file-list-mode-map " " 'pvs-file-list-forward)
    (define-key pvs-file-list-mode-map "f" 'pvs-file-list-forward)
    (define-key pvs-file-list-mode-map "\C-i" 'pvs-file-list-forward)
    (define-key pvs-file-list-mode-map "b" 'backward-word)
    (define-key pvs-file-list-mode-map "\177" 'backward-word)
    (define-key pvs-file-list-mode-map "h" 'describe-mode)
    (define-key pvs-file-list-mode-map "?" 'describe-mode)
    (define-key pvs-file-list-mode-map "q"
      '(lambda () (interactive) (remove-buffer (current-buffer)))))


(defun pvs-file-list-mode ()
  "Major mode for editing a list of files.
Each line describes one of the files in context.
Letters do not insert themselves; instead, they are commands.
s -- select file
v -- view file
n - new file
q - quit
Precisely,\\{pvs-file-list-mode-map}"
  (kill-all-local-variables)
  (use-local-map pvs-file-list-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'pvs-file-list-mode)
  (setq mode-name "Files")
  (set-syntax-table pvs-mode-syntax-table))

;;; list-pvs-files

(defpvs list-pvs-files context (context)
  "Display a list of PVS files in a PVS context

The list-pvs-files command prompts for a directory and if there is a PVS
context in the given directory, lists the PVS files of that context.  The
resulting buffer is in a special mode, which allows the file to be viewed
by typing a `v', selected by typing a `s' or imported by typing an
`i'.  A file or theory may only be selected if it is in the current
context, and may only be imported if it is not."
  (interactive (pvs-complete-file-name "List PVS files of context named: "))
  (unless (equal (substring context -1) "/")
    (setq context (format "%s/" context)))
  (let ((files (context-files context)))
    (setq *pvs-list-files-directory* (expand-file-name context))
    (pop-to-buffer (make-pvs-file-list-buffer
		    "File List"
		    (make-files-listing files)))
    (optimize-window-height)
    (pvs-file-list-mode)))


;;; list-theories

(defvar *pvs-list-theories-and-files* nil)

(defpvs list-theories context (context)
  "Display a list of theories in a PVS context

The list-theories command prompts for a directory and if there is a
PVS context in the given directory, lists the theories of that context.
The resulting buffer is in a special mode, which allows the file to be
viewed (by typing a `v'), selected (by typing a `s') or imported (by
typing an `i').  A file or theory may only be selected if it is in the
current context, and may only be imported if it is not.  Importing a
theory from the list of theories will import the associated file."
  (interactive (pvs-complete-file-name "List theories of context named: "))
  (unless (equal (substring context -1) "/")
    (setq context (format "%s/" context)))
  (setq *pvs-list-theories-and-files*
	(context-files-and-theories context))
  (setq *pvs-list-files-directory* (expand-file-name context))
  (let ((tnames (mapcar 'car *pvs-list-theories-and-files*)))
    (if tnames
	(let ((buf (get-buffer-create "PVS Theories")))
	  (set-buffer buf)
	  (if buffer-read-only (toggle-read-only))
	  (erase-buffer)
	  (insert (make-files-listing tnames))
	  (goto-char (point-min))
          (set-buffer-modified-p nil)
	  (toggle-read-only)
	  (pop-to-buffer buf)
	  (use-local-map pvs-file-list-mode-map)
	  (message "Theories of %s" context))
	(message "No theories found"))))

(defun context-files-and-theories (context)
  (pvs-file-send-and-wait 
   (format "(context-files-and-theories \"%s\")" context)
   nil nil 'list))

(defun context-files (context)
  (let ((default-directory *pvs-current-directory*))
    (mapcar
	(function
	 (lambda (fname)
	   (cond ((cdr *pvs-file-extensions*)
		  fname)
		 (t (string-match "^\\(.*\\)\\.pvs$" fname)
		    (substring fname (match-beginning 1) (match-end 1))))))
      (directory-files context nil (pvs-extensions-regexp)))))

(defun library-files ()
  (pvs-file-send-and-wait "(library-files)" nil nil 'list))

(defun libraries ()
  (pvs-file-send-and-wait "(current-libraries)" nil nil 'list))

(defun make-pvs-file-list-buffer (buffer-name contents)
  (let ((buf (get-buffer-create buffer-name)))
    (save-excursion
      (set-buffer buf)
      (if buffer-read-only (toggle-read-only))
      (erase-buffer)
      (insert contents)
      (goto-char (point-min))
      (toggle-read-only)
      buf)))

(defun make-files-listing (files)
  (if (member files '(nil NIL))
      (error "No files in context %s" *pvs-list-files-directory*)
      (let* ((colsize (+ (max-string-length files) 2))
	     (numcols (/ (window-width) colsize))
	     (div (/ (length files) numcols))
	     (mod (% (length files) numcols))
	     (numrows (+ div (if (= mod 0) 0 1)))
	     (listing ""))
	(dotimes (r numrows)
	  (setq listing
		(concat listing
			(make-listing-row files r numcols colsize div mod))))
	listing)))

(defun make-listing-row (list row numcols colsize div mod)
  (let ((lrow "\n")
	(spaces (make-string colsize ? )))
    (dotimes (i numcols)
      (let* ((c (- numcols i 1))
	     (elt (if (and (= row div) (>= c mod))
		      ""
		      (nth (+ (* row numcols) c)
			   list))))
	(setq lrow
	      (concat (format "%s%s" elt
			(if (or (equal elt "")
				(= i 0))
			    ""
			    (substring spaces (length elt))))
		      lrow))))
    lrow))

(defun pvs-file-list-name ()
  "Return file name described by this position of file list."
  (word-pointed-at))

(defun word-pointed-at ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
        (progn (forward-char 1)
               (buffer-substring (point)
                                 (progn (forward-sexp -1)
                                        (while (looking-at "\\s'")
                                          (forward-char 1))
                                        (point))))
      nil)))

(defun pvs-file-list-select ()
  "Select this line's file in full screen."
  (interactive)
  (if (file-equal *pvs-list-files-directory* (pvs-current-directory))
      (let ((buf (current-buffer)))
	(if (equal (buffer-name) "PVS Theories")
	    (find-pvs-file (cadr (assoc (pvs-file-list-name)
					*pvs-list-theories-and-files*)))
	    (find-pvs-file (pvs-file-list-name)))
	(kill-buffer buf))
      (message "This %s is not in the current context - type i to import"
	       (if (equal (buffer-name) "PVS Theories")
		   "theory" "PVS file"))))

(defun pvs-file-list-view ()
  "View file on current line in File list."
  (interactive)
  (let ((filename (if (equal (buffer-name) "PVS Theories")
		      (cadr (assoc (pvs-file-list-name)
				    *pvs-list-theories-and-files*))
		      (format "%s.pvs" (pvs-file-list-name)))))
    (if filename
	(pvs-view-file (format "%s%s" *pvs-list-files-directory* filename))
	(ding)
	(message "File %s does not exist in %s!" filename
		 *pvs-list-files-directory*)
	(sit-for 4))))

(defun pvs-file-list-import ()
  "Import a file."
  (interactive)
  (let ((filename (if (equal (buffer-name) "PVS Theories")
		      (cadr (assoc (pvs-file-list-name)
				   *pvs-list-theories-and-files*))
		      (pvs-file-list-name))))
    (if (file-equal *pvs-list-files-directory* (pvs-current-directory))
	(error "This %s is already in the current context - type s to select"
	       (if (equal (buffer-name) "PVS Theories")
		   "theory" "PVS file"))
	(if filename
	    (let* ((path (format "%s%s.pvs" *pvs-list-files-directory*
			   filename))
		   (name (car (new-pvs-file-name "(As file) file name: "
						 filename))))
	      (remove-buffer (current-buffer))
	      (import-pvs-file path name))
	    (ding)
	    (message "File %s does not exist in %s!" filename
		     *pvs-list-files-directory*)
	    (sit-for 4)))))

(defun pvs-file-list-forward ()
  (interactive)
  (forward-word 2)
  (backward-word 1))
