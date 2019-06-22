;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-utl.el --
;;; ILISP misc tools.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(defun lisp-show-send (string)
  "Show STRING in the *ilisp-send* buffer."
  (save-excursion
    (if (ilisp-buffer)
	(set-buffer "*ilisp-send*")
	(error "The PVS process has died.  Please start a new one with M-x pvs"))
    (erase-buffer)
    (insert string)
    string))


;;;
(defun lisp-slashify (string)
  "Put string in the *ilisp-send* buffer, backslashifying troublesome chars.
I.e. put backslashes before quotes and backslashes and return the resulting
string."
  (save-excursion
    (lisp-show-send string)
    (set-buffer "*ilisp-send*")
    (goto-char (point-min))
    (while (search-forward "\\" nil t)
      (delete-char -1)
      (insert "\\\\"))
    (goto-char (point-min))
    (while (search-forward "\"" nil t)
      (backward-char)
      (insert ?\\)
      (forward-char))
    (buffer-substring (point-min) (point-max))))


;;;%%String
(defun lisp-prefix-p (s1 s2)
  "Returns t if S1 is a prefix of S2.
It considers all non alphanumerics as word delimiters."
  (let ((len1 (length s1)))
    (and (<= len1 (length s2))
	 (let ((start 0)
	       (start2 0) 
	       end
	       (match t))
	   (while
	       (if (setq end (string-match "[^a-zA-Z0-9]" s1 start))
		   ;; Found delimiter
		   (if (string= (substring s1 start end)
			(substring s2 start2 (+ start2 (- end start))))
		       ;; Words are the same
		       (progn (setq start (match-end 0))
			      (if (string-match
				   (regexp-quote (substring s1 end start))
				   s2 start2)
				  (setq start2 (match-end 0)) ;OK
				(setq match nil))) ;Can't find delimiter
		     (setq match nil))	;Words don't match 
		 nil))			;Ran out of delimiters in s1
	   (and match
		(string= (substring s1 start len1)
		 (substring s2 start2 (+ start2 (- len1 start)))))))))


;;;
(defun lisp-last-line (string)
  "Return the last line of STRING with everything else."
  (let* ((position 0))
    (while (string-match "\\(\n+\\)[^\n]" string position)
      (setq position (match-end 1)))
    (cons (substring string position)
	  (substring string 0 position))))


;;;%%File
;;;
(defun lisp-file-extension (file extension)
  "Return FILE with new EXTENSION."
  (concat (substring file 0 (string-match ".[^.]*$" file))
	  "." extension))

(defun ilisp-directory (file &optional dirs)
  "Return the directory of DIRS that FILE is found in.
By default 'load-path' is used for the directories."
  (let* ((dirs (or dirs (cons "" load-path)))
	 (dir (car dirs)))
    (while (and dir (not (file-exists-p (expand-file-name file dir))))
      (setq dirs (cdr dirs)
	    dir (car dirs)))
    dir))


;;; ilisp-update-status --
;;;
;;; Notes:
;;;
;;; 19970412 Marco Antoniotti
;;; Changed in order to propagate the status change in the
;;; underlying process to the menu.
;;;
;;; 19990806 Martin Atzmueller
;;; Added test for FEATUREP ILISP-EASY-MENU.

(defun ilisp-update-status (status)
  "Update process STATUS of the whole ILISP system.
It updates the STATUS of the current buffer and let all lisp mode
buffers know as well.  Also, do some 'exterior' things like make sure
that the menubar is in a consistent state."
  (setq ilisp-status (if lisp-show-status (format " :%s" status)))
;;   (when (and (not (member +ilisp-emacs-version-id+
;; 			  '(xemacs lucid-19 lucid-19-new)))
;; 	     (not (featurep 'ilisp-easy-menu)))
;;     (ilisp-update-menu status))
  (comint-update-status status))

(defun ilisp-last-input-char ()
  (if (featurep 'xemacs)
      (event-to-character last-input-event)
    last-input-event))

;;; end of file -- ilisp-utl.el --
