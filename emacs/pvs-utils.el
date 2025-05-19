;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;; pvs-utils.el -- 
;; Author          : Sam Owre
;; Created On      : Wed Sep 15 17:51:30 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 22:48:18 2004
;; Update Count    : 18
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(require 'cl-lib)
(require 'compare-w)
(eval-when-compile (require 'pvs-macros))

(defvar comint-status)
(defvar ilisp-complete)
(defvar ilisp-buffer)
(defvar pvs-prelude)
(defvar pvs-buffer-kind)

(declare-function princ-nl "pvs-ilisp")
(declare-function pvs-formula-origin "pvs-prover")
(declare-function typecheck "pvs-cmds")
(declare-function find-pvs-file "pvs-cmds")
(declare-function pvs-remove-bin-files "pvs-cmds")
(declare-function pvs-send "pvs-ilisp")
(declare-function prove-pvs-file "pvs-prover")
(declare-function prove-theory "pvs-prover")
(declare-function prove-importchain "pvs-prover")
(declare-function set-rewrite-depth "pvs-prover")
(declare-function context-files "pvs-file-list")
(declare-function pvs-bury-output "pvs-ilisp")
(declare-function save-some-pvs-files "pvs-cmds")
(declare-function pvs-send-and-wait "pvs-ilisp")

(declare-function ilisp-value "ilisp/ilisp-val")
(declare-function ilisp-process "ilisp/ilisp-prc")
(declare-function ilisp-send "ilisp/ilisp-snd")
(declare-function ilisp-switch-to-lisp "ilisp/ilisp-out")
(declare-function comint-remove-whitespace "ilisp/comint-ipc")
(declare-function comint-send "ilisp/comint-ipc")



(defvar *pvs-file-extensions* '("pvs"))
(defvar pvs-default-timeout 10)
(defvar pvs-path) ; Set in pvs-go.el
(defvar pvs-library-path)
(defvar pvs-in-checker)
(defvar pvs-reserved-words-regexp)
(defvar pvs-string-positions)

(defvar pvs-search-syntax-table nil  "Syntax table used while in search mode.")
(if pvs-search-syntax-table ()
    (let ((st (syntax-table)))
      (unwind-protect
	   (progn
	     (setq pvs-search-syntax-table (make-syntax-table))
	     (set-syntax-table pvs-search-syntax-table)
	     (modify-syntax-entry ?_ "w")
	     (modify-syntax-entry ?\? "w")
	     (modify-syntax-entry ?: ".")
	     (modify-syntax-entry ?% "<")
	     (modify-syntax-entry ?\f ">")
	     (modify-syntax-entry ?\n ">")
	     (modify-syntax-entry ?\r ">"))
	(set-syntax-table st))))

(defun pvs-prelude-file-p (file)
  (or (file-equal (format "%s/lib/prelude.pvs" pvs-path) file)
      (file-equal (format "%s/lib/pvsio_prelude.pvs" pvs-path) file)))

(defun pvs-kind-of-buffer (&optional buffer)
  (let* ((buf (or buffer (current-buffer)))
	 (bname (buffer-name buf))
	 (bfile (buffer-file-name buf))
	 (ext (when bfile (pathname-type bfile))))
    (cond ((and bfile
		(file-equal (format "%s/lib/prelude.pvs" pvs-path) bfile))
	   'prelude)
	  ((and bfile
		(file-equal (format "%s/lib/pvsio_prelude.pvs" pvs-path) bfile))
	   'pvsio_prelude)
	  (pvs-prelude
	   'prelude-theory)
	  ((equal ext "pvs")
	   'pvs)
	  ((equal bname "PVS Status")
	   'proof-status)
	  ((equal ext "tccs")
	   'tccs)
	  ((equal ext "ppe")
	   'ppe)
	  ((or (equal pvs-buffer-kind "Declaration")
	       (equal bname "Declaration"))
	   ;; From M-x show-declaration
	   'declaration)
	  (t (error "Not a known PVS buffer")))))

;;; Define this first, so we can start logging right away.

(defun pvs-log-message (kind msg)
  (let ((buf (current-buffer)))
    (unwind-protect
	 (let* ((cpoint (point))
		(at-end (= cpoint (point-max))))
	   (set-buffer (get-buffer-create "PVS Log"))
	   (goto-char (point-max))
	   (insert (format "%s(%s): %s\n"
		       kind
		     (substring (current-time-string) 4 19)
		     msg))
	   (unless at-end
	     (goto-char cpoint)))
      (set-buffer buf))))

(defun pvs-msg (msg &rest args)
  (let ((m (apply 'format msg args)))
    (cond (noninteractive
	   (princ m)
	   (princ m 'external-debugging-output)
	   (terpri))
	  (t
	   (pvs-log-message 'MSG m)
	   (message "%s" m)))))

(unless (fboundp 'memql)
  (defun memql (elt list)
    (and (not (null list))
	 (or (eql elt (car list))
	     (memql elt (cdr list))))))

(defun pvs-getenv (var)
  (let ((val (getenv var)))
    (if (equal val "") nil val)))

(defvar pvs-verbose
  (condition-case ()
      (car (read-from-string (pvs-getenv "PVSVERBOSE")))
    (error 0)))

(defvar pvs-validating nil
  "non-nil if PVS is running in batch mode")

(defvar pvs-current-directory (file-truename default-directory)
  "Pathname of the current PVS context.")

(defvar start-pvs t)

;;; Misc functions 

(defpvs forward-theory editing (&optional nomsg)
  "Move forward to the beginning of the next theory

The forward-theory command moves forward to the beginning of the next
theory, if there is one."
  (interactive)
  (let ((opoint (point))
	(moved t))
    (if (beginning-of-theory)
	(when (= opoint (point))
	  (find-theory-or-datatype-forward)
	  (unless (beginning-of-theory (point))
	    (goto-char opoint)
	    (setq moved nil)
	    (unless nomsg (message "No more theories"))))
	(goto-char opoint)
	(setq moved nil)
	(unless nomsg (message "No more theories")))
    moved))

(defpvs backward-theory editing (&optional no-message-p)
  "Move backward to the beginning of previous theory

The backward-theory command moves to the beginning of the current theory,
unless it is at the beginning of the theory in which case it moves to the
beginning of the previous one."
  (interactive)
  (if (find-theory-or-datatype-backward)
      (beginning-of-theory)
      (unless no-message-p
	(message "No earlier theories"))))

(defun beginning-of-theory (&optional start)
  (if (find-theory-or-datatype-forward start)
      (progn
	(re-search-backward-ignoring-comments ":" nil t)
	(backward-sexp)
	(while (in-pvs-comment) (backward-sexp))
	(when (looking-at "\\[")
	  (backward-sexp)
	  (while (in-pvs-comment) (backward-sexp)))
	t)
      (goto-char (or start (point)))
      nil))


;;; Finds the next THEORY or DATATYPE keyword that does not occur in a
;;; comment, square braces, or BEGIN-END pair.  Puts point immediately
;;; after the keyword and returns t if successful, otherwise leaves point
;;; unchanged and returns nil

(defun find-theory-or-datatype-forward (&optional start)
  (let* ((opoint (point))
	 (case-fold-search t)
	 (found (re-search-forward "\\(\\btheory\\b\\|\\bdatatype\\b\\)"
				   nil t)))
    (while (and found
		(or (in-pvs-comment-or-string)
		    (in-square-braces (or start (point-min)))
		    (in-begin-end (or start (point-min)))))
      (setq found (re-search-forward "\\(\\btheory\\b\\|\\bdatatype\\b\\)"
				     nil t)))
    (if found
	t
	(goto-char opoint)
	nil)))

(defun find-theory-or-datatype-backward (&optional start)
  (let* ((opoint (point))
	 (case-fold-search t)
	 (found (re-search-backward "\\(\\btheory\\b\\|\\bdatatype\\b\\)"
				    nil t)))
    (while (and found
		(or (in-pvs-comment-or-string)
		    (in-square-braces (or start (point-min)))
		    (in-begin-end (or start (point-min)))))
      (setq found (re-search-backward "\\(\\btheory\\b\\|\\bdatatype\\b\\)"
				      nil t)))
    (if found
	t
	(goto-char opoint)
	nil)))

(defun current-theory-region ()
  (with-syntax-table pvs-search-syntax-table
    (or (current-prelude-theory-region)
	(let ((case-fold-search t))
	  (save-match-data
	    (unless (= (point) (point-max))
	      (forward-char 1))
	    (backward-theory t)
	    (unless (beginning-of-theory)
	      (error "Can't determine theory boundaries"))
	    (looking-at "\\w+")
	    (let ((start (point))
		  (th-id (buffer-substring-no-properties
			  (match-beginning 0) (match-end 0))))
	      (find-theory-or-datatype-forward)
	      (unless (beginning-of-theory (point))
		(goto-char (point-max)))
	      (if (< start (point))
		  (list th-id start (point))
		  nil)))))))

(defun current-prelude-theory-region ()
  (let ((prelude-file (format "%s/lib/prelude.pvs" pvs-path)))
    (when (file-equal (buffer-file-name) prelude-file)
      (let ((prelude-regions
	     (cdr (assoc prelude-file (get-prelude-files-and-regions)))))
	(while (and prelude-regions
		    (< (caddr (car prelude-regions)) (point)))
	  (pop prelude-regions))
	(car prelude-regions)))))
      
      

(defun theory-regions ()
  (when (current-pvs-file t)
    (with-syntax-table pvs-search-syntax-table
      (condition-case err
	  (find-unbalanced-region-pvs (point-min) (point-max))
	(error (error "Can't determine theory boundaries: %s" (cadr err))))
      (theory-regions*))))

(defun theory-regions* (&optional verbose)
  (with-syntax-table pvs-search-syntax-table
    (let ((case-fold-search t))
      (save-excursion
	(goto-char (point-min))
	(let ((tregs nil)
	      (pname nil)
	      (opoint nil))
	  (while (beginning-of-theory opoint)
	    (let ((tpoint (point)))
	      (back-over-comments)
	      (when pname (push (list pname opoint (point)) tregs))
	      (setq opoint (point))
	      (goto-char tpoint)
	      (looking-at "\\w+")
	      (setq pname (buffer-substring-no-properties
			   (match-beginning 0) (match-end 0)))
	      (when verbose (princ pname) (princ " "))
	      (find-theory-or-datatype-forward (point))))
	  (when pname
	    (when verbose (pvs-message "Found region for %s" pname))
	    (push (list pname opoint (point-max)) tregs))
	  (nreverse tregs))))))

(defun back-over-comments ()
  (let ((opoint (point))
	(case-fold-search t))
    (beginning-of-line)
    (forward-line -1)
    (if (not (looking-at "\\(^[ \t]*%.*$\\|^[ \t]*$\\)"))
	(goto-char opoint)
	(while (and (looking-at "\\(^[ \t]*%.*$\\|^[ \t]*$\\)")
		    (not (= (point) (point-min))))
	  (forward-line -1))
	(if (search-forward "%" opoint t)
	    (search-backward "%")
	    (goto-char opoint)))))

(defun re-search-forward-ignoring-comments (regexp &optional
						   limit noerror repeat)
  (let ((case-fold-search t))
    (re-search-forward regexp limit noerror repeat)
    (while (in-pvs-comment)
      (re-search-forward regexp limit noerror repeat))))

(defun re-search-backward-ignoring-comments (regexp &optional
						    limit noerror repeat)
  (let ((case-fold-search t))
    (re-search-backward regexp limit noerror repeat)
    (while (in-pvs-comment)
      (re-search-backward regexp limit noerror repeat))))


;;; Given a theoryname, theory-region returns a filename and a region.
;;; It first tries to get the information from PVS; if must-match is T,
;;; then the theory must be in an unmodified buffer and the
;;; corresponding file must have been parsed.  Otherwise, failing that,
;;; it gets the region directly from the buffer, using the current
;;; buffer if not even the filename is available.  If neither pan out,
;;; calls error if the optional argument is given.  If PVS returns a
;;; filename that no longer exists, invokes an error.  Note that this
;;; does not modify any buffers and wraps everything in a
;;; save-excursion.

(defun theory-region (theoryname &optional must-match error)
  (or (unless must-match
	(or (theory-region-from-buffer theoryname)
	    ;;(theory-region-from-some-file theoryname)
	    ))
      (theory-region-from-pvs theoryname must-match)
      (when error
	(error "Can't find theory named %s" theoryname))))

(defun theory-region-from-pvs (theoryname must-match)
  (let ((fandp (pvs-send-and-wait
		(format "(file-and-place (get-theory \"%s\"))" theoryname)
		nil nil 'list)))
    (when fandp
      (cond ((not (file-exists-p (car fandp)))
	     (error "Theory %s was in %s which no longer exists"
		    theoryname (car fandp)))
	    ((or (null (cdr fandp))
		 (buffer-modified-p (find-file-noselect (car fandp))))
	     (unless must-match
	       (with-current-buffer (find-file-noselect (car fandp))
		 (theory-region-from-buffer theoryname))))
	    (t (file-and-place-to-region fandp))))))

(defun theory-region-from-buffer (theoryname)
  (let ((theory-region nil))
    (dolist (buf (buffer-list))
      (when (and (null theory-region)
		 (pvs-buffer-file-name buf))
	(with-current-buffer buf
	  (let ((region (cdr (assoc theoryname (theory-regions)))))
	    (when region
	      (setq theory-region (cons (buffer-file-name) region)))))))
    theory-region))

(defun theory-region-from-some-file (theoryname)
  (let ((files (directory-files (pvs-current-directory) t
				(pvs-extensions-regexp)))
	(thregs nil))
    (save-excursion
      (dolist (file files)
	(let* ((buf (find-file-noselect file)))
	  (set-buffer buf)
	  (message "Processing %s" file)
	  (let ((threg (theory-region-from-buffer theoryname)))
	    (when threg
	      (push (cons file threg) thregs))))))
    thregs))

(defun file-and-place-to-region (file-and-place)
  (let ((file (car file-and-place))
	(place (cdr file-and-place)))
    (with-current-buffer (find-file-noselect (car file-and-place))
      (cons file (pvs-region place)))))

(defun buffer-theories ()
  (let ((file (current-pvs-file t)))
    (when file
      (mapcar 'car (theory-regions)))))

(defun in-pvs-comment-or-string ()
  (or (in-pvs-comment) (in-pvs-string)))

(defun in-pvs-comment ()
  (let ((limit (point)))
    (save-excursion
      (beginning-of-line)
      (let ((found (search-forward "%" limit t)))
	(while (and found (in-pvs-string))
	  (setq found (search-forward "%" limit t)))
	found))))

(defun in-pvs-string ()
  (set-pvs-string-positions)
  (cl-some #'(lambda (strpos)
	       (and (< (car strpos) (point))
		    (>= (cdr strpos) (point))))
	   pvs-string-positions))

(defpvs find-unbalanced-pvs editing (arg)
  "Find unbalanced PVS delimiters

The find-unbalanced-pvs command goes to the point in buffer where PVS
delimiters become unbalanced.  Point will be on the offending delimiter.
If called with a prefix, use the current region."
  (interactive "P")
  (if arg
      (call-interactively 'find-unbalanced-region-pvs)
      (find-unbalanced-region-pvs (point-min) (point-max)))
  (unless ilisp-complete (message "All delimiters balance")))

(defun find-unbalanced-region-pvs (start end)
  "Go to the point in region where PVS delimiters become unbalanced

The find-unbalanced-region-pvs command goes to the point in region where
PVS delimiters become unbalanced.  Point will be on the offending
delimiter."
  (interactive "r")
  (set-pvs-string-positions)
  (pvs-count-char-pairs start end "[" "]")
  (pvs-count-char-pairs start end "(" ")")
  (pvs-count-char-pairs start end "{" "}")
  (pvs-count-string-pairs start end "begin" "end"))

(defun set-pvs-string-positions ()
  (unless (assq 'pvs-string-positions (buffer-local-variables))
    (save-excursion
      (goto-char (point-min))
      (let ((string-positions nil)
	    (string-pos nil))
	(while (setq string-pos (pvs-next-string-position))
	  (push string-pos string-positions))
	(setq-local pvs-string-positions
		    (nreverse string-positions))))
    (setq-local after-change-functions
		;; Called after buffer hash been changed
		(cons '(lambda (beg end len)
			(makunbound 'pvs-string-positions))
		      after-change-functions))))

(defun pvs-next-string-position ()
  (let ((string-start (re-search-forward "\"" nil t)))
    (while (and string-start (simple-in-pvs-comment))
      (setq string-start (re-search-forward "\"" nil t)))
    (when string-start
      (let ((string-end (re-search-forward "\"" nil t)))
	(while (and string-end
		    (save-excursion (forward-char -2)
				    (and (looking-at "\\\\")
					 (forward-char -1)
					 (not (looking-at "\\\\")))))
	  (setq string-end (re-search-forward "\"" nil t)))
	(unless string-end
	  (goto-char string-start)
	  (error "Extra \""))
	(cons (1- string-start) (1- string-end))))))

(defun simple-in-pvs-comment ()
  (let ((limit (point)))
    (save-excursion
      (beginning-of-line)
      (search-forward "%" limit t))))

(defun pvs-count-char-pairs (start end sdel edel)
  (let ((mismatch (pvs-count-char-pairs* start end sdel edel)))
    (when mismatch
      (goto-char (car mismatch))
      (error "Extra %s" (if (cadr mismatch) sdel edel)))))

(defun pvs-count-char-pairs* (start end sdel edel)
  (let ((case-fold-search t))
    (save-excursion
      (let ((err nil)
	    (pairstack nil))
	(goto-char start)
	(while (and (not err)
		    ;; Don't reverse sdel and edel or square braces won't work
		    (re-search-forward (format "[%s%s]" edel sdel) end t))
	  (unless (in-pvs-comment-or-string)
	    (if (equal (char-to-string (preceding-char)) sdel)
		(push (point) pairstack)
		(if pairstack
		    (pop pairstack)
		    (setq err (list (point) nil))))))
	(or err
	    (when pairstack
	      (list (car pairstack) t)))))))

(defun pvs-count-string-pairs (start end sdel edel &optional wbstart)
  (let ((mismatch (pvs-count-string-pairs* start end sdel edel wbstart)))
    (when mismatch
      (goto-char (car mismatch))
      (forward-word -1)
      (error "Extra %s" (if (cadr mismatch) sdel edel)))))

(defun pvs-count-string-pairs* (start end sdel edel wbstart)
  (let* ((case-fold-search t)
	 (regexp (if (null wbstart)
		     (format "\\b%s\\b\\|\\b%s\\b" sdel edel)
		     (format "%s\\b\\|%s\\b" sdel edel))))
    (save-excursion
      (let ((err nil)
	    (pairstack nil))
	(goto-char start)
	(while (and (not err)
		    (re-search-forward regexp end t))
	  (unless (in-pvs-comment-or-string)
	    (if (save-excursion
		  (re-search-backward regexp nil t)
		  (looking-at sdel))
		(push (point) pairstack)
		(if pairstack
		    (pop pairstack)
		    (setq err (list (point) nil))))))
	(or err
	    (when pairstack
	      (list (car pairstack) t)))))))

(defun in-square-braces (&optional start)
  (save-excursion
    (let ((count 0)
	  (limit (point)))
      (goto-char (or start (point-min)))
      (while (re-search-forward "[][]" limit t)
	(unless (in-pvs-comment-or-string)
	  (setq count
		(+ count (if (eq (preceding-char) ?\[) 1 -1)))))
      (not (= count 0)))))

(defun in-delimiters (sdel edel &optional start)
  (let ((case-fold-search t))
    (save-excursion
      (let ((count 0)
	    (limit (point)))
	(goto-char (or start (point-min)))
	(while (re-search-forward (format "\\b%s\\b\\|\\b%s\\b" sdel edel)
				  limit t)
	  (unless (in-pvs-comment-or-string)
	    (setq count
		  (+ count (save-excursion
			     (forward-word -1)
			     (if (looking-at sdel) 1 -1))))))
	(not (= count 0))))))

(defun in-begin-end (&optional start)
  (in-delimiters "begin" "end" start))

(defun parsed-p (theory)
 (pvs-send-and-wait (format "(and (parsed? (get-theory \"%s\")) t)" theory)
		    nil nil 'bool))

(defun typechecked-p (theory)
  (pvs-send-and-wait
   (format "(and (typechecked? (get-theory \"%s\")) t)" theory)
   nil nil 'bool))

(defun typechecked-file-p (file)
  (pvs-send-and-wait
   (format "(and (typechecked-file? \"%s\") t)" file)
   nil nil 'bool))

(defun pathname-directory (file-name)
  (or (file-name-directory file-name) ""))

(defun pathname-name (file-name)
  (let* ((name (file-name-nondirectory file-name))
	 (pos (last-period name)))
    (if pos
	(substring name 0 (- pos 1))
	name)))

(defun pathname-type (file-name)
  (let* ((name (file-name-nondirectory file-name))
	 (pos (last-period name)))
    (if pos
	(substring name pos)
	"")))

(defun last-period (name)
  (if (string-match "\\." name)
      (let ((npos (match-end 0)))
	(while (string-match "\\." name npos)
	  (setq npos (match-end 0)))
	npos)
      nil))

(defun theory-buffer-p (buf)
  (with-current-buffer buf
    (and (buffer-file-name)
	 (string-match (pvs-extensions-regexp) (buffer-file-name)))))

(defun pvs-buffer-file-name (&optional buf)
  (unless buf (setq buf (current-buffer)))
  (with-current-buffer buf
    (and (buffer-file-name)
	 (string-match (pvs-extensions-regexp) (buffer-file-name))
	 (pathname-name (buffer-file-name)))))

(defun save-some-pvs-buffers (&optional dontask)
  "Saves the current PVS buffer automatically, asks about others"
  (let ((filename (current-pvs-file t)))
    (when filename
      (save-pvs-file filename)))
  (save-some-pvs-files dontask))

(defpvs save-pvs-file save-files (filename)
  "Save PVS file in current buffer

The save-pvs-file command saves the PVS file of the current buffer."
  (interactive (list (current-pvs-file t)))
  (if filename
      (let ((buff (get-pvs-file-buffer filename)))
	(if buff
	    (if (buffer-modified-p buff)
		(with-current-buffer buff
		  (save-buffer)
		  ;;(setq buffer-modified nil)
		  ))
	    (save-buffer))
	buff)
      (save-buffer)))

(defun kill-pvs-buffer (filename)
  (let ((buff (get-pvs-file-buffer filename)))
    (when buff
      (kill-buffer buff))))

(defun get-theory-buffer (theoryref)
  (let* ((theoryname (car (last (split-string theoryref "#"))))
	 (filoc (cdr (assoc theoryname (pvs-collect-theories))))
	 (filename (car filoc))
	 (place (cadr filoc)))
    (when filename
      (with-current-buffer (find-file-noselect (expand-file-name filename))
	(goto-char (point-min))
	(when place
	  (forward-line (1- (car place))))
	(current-buffer)))))

(defun get-pvs-file-buffer (fname)
  (let* ((name (pathname-name fname))
	 (ext (pathname-type fname))
	 (pdir (pathname-directory fname))
	 (dir (if (equal pdir "")
		  pvs-current-directory
		  pdir)))
    (if (and ext (member ext *pvs-file-extensions*))
	(or (get-buffer (format "%s.%s" name ext))
	    (let ((filename (format "%s%s.%s" dir name ext)))
	      (find-file-noselect filename noninteractive)))
	(let ((files nil))
	  (dolist (pext *pvs-file-extensions*)
	    (let ((filename (if (and ext (not (equal ext "")))
				(format "%s%s.%s.%s" dir name ext pext)
				(format "%s%s.%s" dir name pext))))
	      (when (file-exists-p filename)
		(push filename files))))
	  (cond ((cdr files)
		 (error "%s is ambiguous: one of %s" name files))
		((car files)
		 (find-file-noselect (car files) noninteractive)))))))

(defun pvs-file-name (filename &optional ext)
  (format "%s%s.%s" pvs-current-directory filename
	  (if (and ext
		   (not (equal ext "")))
	      ext
	      "pvs")))

(defun clear-buffer (buffer)
  (with-current-buffer buffer
    (erase-buffer)))

(defvar-local current-pvs-file nil)
(defvar-local pvs-lib-p nil)

(defun current-pvs-file (&optional no-error)
  (if (or (not (buffer-file-name))
	  (not (member-equal (pathname-type (buffer-file-name))
			     *pvs-file-extensions*)))
      (unless no-error
	(error "%s is not a valid PVS file" (buffer-name)))
      (buffer-file-name)))

(defun associated-pvs-file (&optional buf)
  (unless buf
    (setq buf (current-buffer)))
  (with-current-buffer buf
    (or (current-pvs-file t)
	(and (boundp 'from-pvs-theory)
	     (cadr (assoc from-pvs-theory (pvs-collect-theories))))
	(and (member-equal (pathname-type (buffer-name)) '("tccs" "ppe"))
	     (cadr (assoc (pathname-name (buffer-name)) (pvs-collect-theories)))))))

(defun pvs-library-file-p (filename)
  (pvs-send-and-wait (format "(library-file? \"%s\")" filename)
		     nil nil 'bool))

(defun pvs-library-file (filename)
  (and (pvs-library-file-p filename)
       (concat (pathname-directory filename) (pathname-name filename))))


;;; File-equal compares files according to attributes, not names.
;;; Note that this doesn't work with hard links.

(defun file-equal (file1 file2)
  (and file1 file2
       (let* ((default-directory pvs-current-directory)
	      (attr1 (file-attributes* (expand-file-name file1)))
	      (attr2 (file-attributes* (expand-file-name file2))))
	 (equal attr1 attr2))))

(defun file-attributes* (file)
  (let ((attr (file-attributes file)))
    (if (stringp (car attr))
	(file-attributes* (car attr))
	attr)))

(defun short-file-name (file)
  (if (file-exists-p file)
      (let* ((dirnames (split-string file "/"))
	     (shortname file)
	     (lname file)
	     (hname file))
	(while dirnames
	  (setq dirnames (cdr dirnames))
	  (cond ((file-equal file hname)
		 (setq shortname hname))
		((file-equal file lname)
		 (setq shortname lname)))
	  (setq lname (apply 'concat
			(mapcar #'(lambda (x) (concat "/" x))
			  dirnames)))
	  (setq hname (concat "~" lname)))
	shortname)
      file))

(defun pvs-remove-whitespace (string)
  (let ((str (comint-remove-whitespace string)))
    (if (string-match "^\\(nil\\|NIL\\)" str)
	(substring str 0 (- (match-beginning 0) 1))
	str)))

(defun new-pvs-file-name (prompt &optional initial exists-noerror)
  ;;(setq pvs-current-directory (pvs-current-directory))
  (let* ((filename (read-from-minibuffer prompt initial))
	 (theoryname (pathname-name filename))
	 (ext (pathname-type filename)))
    (unless (equal (pathname-directory filename) "")
      (error "Do not include the directory"))
    (unless (member-equal ext (cons "" *pvs-file-extensions*))
      (error "Filename extension must be omitted or one of %s"
	     *pvs-file-extensions*))
    (when (equal theoryname "")
      (error "Must enter a filename."))
    (if (and (not exists-noerror)
	     (file-exists-p (pvs-file-name theoryname ext)))
	(error "%s already exists." (pvs-file-name theoryname ext))
	(list theoryname))))

(defun new-theory-name (prompt &optional initial)
  (let ((theoryname (read-from-minibuffer prompt initial)))
    (if (assoc theoryname (pvs-collect-theories))
	(error "Theory %s already exists." theoryname)
	(if (valid-theory-name theoryname)
	    (list theoryname)
	    (error "%s is not a valid theory name." theoryname)))))

(defun valid-theory-name (name)
  (string-match "^[a-zA-Z][a-zA-Z0-9_]*" name))

(defun pvs-complete-file-name (prompt &optional defdir)
  "Perform completion on file names"
  (pvs-bury-output)
  (let* ((dir (or defdir (pvs-current-directory)))
	 (default-directory dir))
    (list (read-file-name prompt dir dir t))))

;;; Provides completion for a library path.  The complication here is that
;;; if the environment variable PVS_LIBRARY_PATH contains .:~/foo:/bar
;;; then completion must allow for normal file completion as well as
;;; things like "bar", where bar is a subdirectory of ., ~/foo, /bar, or
;;; the pvs-path/lib.  The return value is an existing directory.

(defvar pvs-library-path-completions nil)

(defun pvs-get-library-path (value &optional noerr)
  ;; The noninteractive form, used when load-prelude-library is called
  ;; directly from Emacs, rather than as a command
  (let* ((pvs-library-path-completions
	  (pvs-library-path-subdirs pvs-library-path))
	 (asdir (assoc value pvs-library-path-completions)))
    (if asdir
	(concat (cdr asdir) "/" (car asdir))
	(if (file-directory-p value)
	    value
	    (unless noerr
	      (error "Not a valid library directory"))))))
  
(defun pvs-complete-library-path (prompt)
  (pvs-bury-output)
  (let* ((pvs-library-path-completions
	  (pvs-library-path-subdirs pvs-library-path))
	 (value (completing-read prompt 'pvs-library-path-completion))
	 (asdir (assoc value pvs-library-path-completions)))
    (if asdir
	(concat (cdr asdir) "/" (car asdir))
	(if (file-directory-p value)
	    value
	    (error "Not a valid library directory")))))

(defun pvs-library-path-completion (string predicate flag)
  (cond ((eq flag 'nil)
	 ;; `nil' specifies `try-completion'.  The completion function should
	 ;; return the completion of the specified string, or `t' if the
	 ;; string is an exact match already, or `nil' if the string matches
	 ;; no possibility.
	 (cond ((equal string "")
		(try-completion string pvs-library-path-completions
				  predicate))
	       ((member (aref string 0) '(?. ?/ ?~))
		(let* ((fstring (if (member string '("." ".." "~"))
				    ""
				    (file-name-nondirectory string)))
		       (fdir (if (member string '("." ".." "~"))
				 (concat string "/")
				 (file-name-directory string)))
		       (fname (file-name-completion fstring fdir)))
		  (concat fdir fname)))
	       (t (try-completion string pvs-library-path-completions
				  predicate))))
	((eq flag 't)
	 ;; `t' specifies `all-completions'.  The completion function should
	 ;; return a list of all possible completions of the specified string.
	 (cond ((equal string "")
		(all-completions string pvs-library-path-completions
				   predicate))
	       ((member (aref string 0) '(?. ?/ ?~))
		(let* ((fstring (if (member string '("." ".." "~"))
				    ""
				    (file-name-nondirectory string)))
		       (fdir (if (member string '("." ".." "~"))
				 (concat string "/")
				 (file-name-directory string)))
		       (fnames (file-name-all-completions fstring fdir)))
		  (mapcar #'(lambda (fname) (concat fdir fname)) fnames)))
	       (t (all-completions string pvs-library-path-completions
				   predicate))))
	((eq flag 'lambda)
	 ;; `lambda' specifies a test for an exact match.  The completion
	 ;; function should return `t' if the specified string is an exact
	 ;; match for some possibility; `nil' otherwise.
	 (if (and (not (equal string ""))
		  (if (member (aref string 0) '(?. ?/ ?~))
		      (file-directory-p string)
		      (assoc string pvs-library-path-completions)))
	     t))))

;; Note that earlier paths shadow later ones, in particular, the pvs-path/lib
;; directory is first.
(defun pvs-library-path-subdirs (dirs)
  (let ((dirname-paths nil))
    (dolist (dir (cons (format "%s/lib" pvs-path) dirs))
      (when (file-directory-p dir)
	(dolist (subdir (directory-files dir))
	  (unless (or (member subdir '("." ".."))
		      (assoc subdir dirname-paths))
	    (when (and (file-directory-p (concat dir "/" subdir))
		       (or (file-exists-p
			    (concat dir "/" subdir "/.pvscontext"))
			   (file-exists-p
			    (concat dir "/" subdir "/pvs-lib.lisp"))
			   (file-exists-p
			    (concat dir "/" subdir "/pvs-lib.el"))))
	      (push (cons subdir dir) dirname-paths))))))
    dirname-paths))

(defun pvs-complete-library-name (prompt &optional distributed-p)
  (pvs-bury-output)
  (let* ((lfiles (relativize-pvs-filenames
		  (pvs-send-and-wait "(library-files)" nil nil 'list)))
	 (ldirs (cl-delete-duplicates (mapcar #'file-name-directory lfiles)
				      :test 'string-equal))
	 (dfiles (when distributed-p
		   (collect-all-distributed-library-files ldirs)))
	 (allfiles (append lfiles dfiles)))
    (if allfiles
	(completing-read prompt (mapcar 'list allfiles) nil 't nil)
	(with-output-to-temp-buffer "*Error*"
	  (princ "No imported library files in this context.\n")
	  (princ "May need to (re)typecheck some files."))
	(error "No library files"))))

(defvar pvs-library-file-and-place nil)

(defun pvs-complete-library-theory-name (prompt)
  (pvs-bury-output)
  (let ((theories
	 (mapcar #'(lambda (tf)
		     (list (relativize-pvs-filename (car tf))
			   (relativize-pvs-filename (cadr tf))
			   (caddr tf)))
	   (pvs-send-and-wait "(library-theories)" nil nil 'list))))
    (if theories
	(let ((theory (completing-read prompt theories nil 't nil)))
	  (cdr (assoc theory theories)))
	(with-output-to-temp-buffer "*Error*"
	  (princ "No imported library theories in this context.\n")
	  (princ "May need to (re)typecheck some files."))
	(error "No library theories"))))
  

(defun collect-all-distributed-library-files (&optional exclude-dirs)
  (let* ((short-pvs-path (short-file-name pvs-path))
	 (default-directory (concat short-pvs-path "/lib"))
	 (files nil))
    (dolist (dir (directory-files default-directory))
      (when (and (not (string-equal dir "."))
		 (file-directory-p dir)
		 (file-exists-p (concat dir "/.pvscontext"))
		 (not (member-pvs-file-equal
		       (concat pvs-path "/lib/" dir) exclude-dirs)))
	(dolist (file (directory-files dir))
	  (when (string-equal (pathname-type file) "pvs")
	    (push (concat short-pvs-path "/lib/" dir "/" (pathname-name file))
		  files)))))
    (nreverse files)))


;;; Get the current directory if the current value is invalid.
;;; Note that we don't expand-file-name; this is because we want the
;;; short form for printing

(defun pvs-current-directory (&optional reset)
  (unless (and (null reset)
	       (stringp pvs-current-directory)
	       (file-exists-p pvs-current-directory))
    (let ((ndir (pvs-send-and-wait "(pvs-current-directory)"
				   nil nil 'string)))
      (while (not (and (stringp ndir)
		       (file-exists-p ndir)))
	(setq ndir (pvs-send-and-wait "(pvs-current-directory)"
				      nil nil 'string)))
      (unless (string-equal ndir "/dev/null")
	(setq pvs-current-directory ndir))))
  pvs-current-directory)

(defun complete-pvs-file-name (prompt &optional no-default-p dir no-timeout
				      with-prelude-p)
  "Perform completion on PVS file names"
  (pvs-bury-output)
  (pvs-current-directory)
  (let ((file-list (append (current-pvs-file t)
			   (context-files (or dir pvs-current-directory))
			   (when with-prelude-p
			     (list (format "%s/prelude" pvs-path))))))
    (if (member file-list '(nil NIL))
	(error "No PVS files in current workspace %s" pvs-current-directory)
	(let* ((default (unless no-default-p (current-pvs-file t)))
	       (dprompt (if default
			    (format "%s(default %s) " prompt default)
			    prompt))
	       (file (if (and (not no-timeout)
			      (fboundp 'with-timeout)
			      default)
			 (with-timeout (pvs-default-timeout default)
			   (completing-read dprompt
			     (mapcar 'list file-list) nil 't nil))
			 (completing-read dprompt
			   (mapcar 'list file-list) nil 't nil))))
	  (if (equal file "")
	      (if default
		  (list default)
		  (error "Must enter a PVS filename."))
	      (list file))))))

(defun complete-pvs-file-name-in-dir (prompt dir &optional no-timeout)
  "Perform completion on PVS file names in the specified directory"
  (pvs-bury-output)
  (let ((file-list (context-files dir)))
    (if (member file-list '(nil NIL))
	(error "No files in context")
	(let* ((default (current-pvs-file t))
	       (dprompt (if default
			    (format "%s(default %s) " prompt default)
			    prompt))
	       (file (if (and (not no-timeout)
			      (fboundp 'with-timeout)
			      default)
			 (with-timeout (pvs-default-timeout default)
			   (completing-read dprompt
			     (mapcar 'list file-list) nil 't nil))
			 (completing-read dprompt
			   (mapcar 'list file-list) nil 't nil))))
	  (if (equal file "")
	      (if default
		  (list default)
		  (error "Must enter a PVS filename."))
	      (list file))))))

(defun real-completing-read (prompt alist-or-obarray
				    &optional predicate require-match initial)
  (let ((string (completing-read prompt alist-or-obarray
				 predicate require-match initial)))
    (if (and require-match (= (length string) 0))
	(real-completing-read prompt alist-or-obarray
			      predicate require-match initial)
	string)))


;;; Returns a theory name, allowing completion on the theories known to
;;; the context and those in the current buffer.  Has the side effect of
;;; setting pvs-current-directory, to cut down on
;;; the number of calls to Lisp.

(defun complete-theory-name (prompt &optional no-timeout with-prelude-p)
  "Perform completion on PVS theories - returns list of filename, place, and
theoryname."
  (pvs-bury-output)
  (let ((default (or (current-theory)
		     (and with-prelude-p
			  (and (boundp 'pvs-prelude) pvs-prelude)
			  (buffer-name))))
	(theories (pvs-collect-theories)))
    (if (null theories)
	(error "No theories in context.")
	(let ((theory (if (and (not no-timeout)
			       (fboundp 'with-timeout)
			       default)
			  (with-timeout (pvs-default-timeout default)
			    (completing-read
				(format "%s%s " prompt
				  (if default
				      (format "(default %s)" default)
				      ""))
			      theories nil t))
			  (completing-read
			      (format "%s%s " prompt
				(if default
				    (format "(default %s)" default)
				    ""))
			    theories nil t))))
	  (if (equal theory "")
	      (if default
		  (list default)
		  (error "Must specify a theory name"))
	      (list theory))))))

(defun complete-theory-name-in-buffer (prompt)
  "Perform completion on PVS theories in current buffer."
  (pvs-bury-output)  
  (let ((theories (buffer-theories)))
    (if (null theories)
	(error "No theories available.")
	(let ((theory (completing-read prompt (mapcar 'list theories) nil t)))
	  (if (equal theory "")
	      (error "Must specify a theory name")
	      (list theory))))))

(defun complete-theory-element-id (prompt thid)
  "Perform completion on PVS theory element id within the given theory."
  (let ((eltids (pvs-send-and-wait (format "(collect-element-ids \"%s\")" thid))))
    (if (null eltids)
	(error (format "Empty list of element ids - might need to typecheck %s."
		   thid))
	(let ((eltid (completing-read prompt (mapcar 'list eltids) nil t)))
	  (if (equal eltid "")
	      (error "Must specify an element id")
	      (list eltid))))))
  

(defun current-theory ()
  (let ((file (current-pvs-file t)))
    (if file
	(let ((thname (save-excursion (car (current-theory-region)))))
	  (when thname
	    (concat file "#" thname)))
	(if (member-equal (pathname-type (buffer-name)) '("ppe" "tccs"))
	    (pathname-name (buffer-name))))))

(defun find-current-theory-region (trs)
  (cond ((null (cdr trs))
	 (car trs))
	((<= (point) (caddr (car trs)))
	 (car trs))
	(t (find-current-theory-region (cdr trs)))))

(defun get-theory-modtime (theoryref)
  (let* ((thbuf (get-theory-buffer theoryref)))
    (if thbuf
	(with-current-buffer thbuf
	  (visited-file-modtime))
	(message "Theory %s not found" theoryref))))

;;; pvs-collect-theories returns an assoc list of the theory names and
;;; their associated PVS filenames.  The primary list
;;; comes from PVS, and reflects the current pvs context (the .pvscontext
;;; file).  This is augmented with the current file, if necessary.  This
;;; is because the current file may not yet have been parsed, so its
;;; current state is unknown in the pvs context, but it is still a valid
;;; choice for many pvs commands.

(defun pvs-collect-theories ()
  "Generates an alist of the form ((th file place) ...)"
  (let* ((theory-alist (pvs-send-and-wait "(collect-theories nil)" nil nil 'list))
	 (file (current-pvs-file t))
	 (current-theories
	  ;; We include the current buffer theories if a PVS file and not in the alist  
	  (unless (or (null file)
		      (cl-find file theory-alist :key 'cadr :test 'file-equal))
	    (pvs-current-theories))))
    ;; (setq pvs-current-directory (car dir-and-theories))
    (append current-theories
	    theory-alist
	    (pvs-get-prelude-theories-alist))))

(defun pvs-current-theories ()
  (or (pvs-current-prelude-theories)
      (let* ((cur-file (current-pvs-file t))
	     (cur-theories (when cur-file (buffer-theories))))
	(when cur-theories
	  (mapcar #'(lambda (th) (list th cur-file)) cur-theories)))))

(defvar pvs-prelude-theories-alist nil)

(defun pvs-get-prelude-theories-alist ()
  (or pvs-prelude-theories-alist
      (setq pvs-prelude-theories-alist
	    (pvs-send-and-wait "(collect-prelude-theories)" nil nil 'list))))

(defun pvs-current-prelude-theories ()
  (let ((prelude-file (format "%s/lib/prelude.pvs" pvs-path)))
    (when (file-equal (buffer-file-name) prelude-file)
      (let ((prelude-regions
	     (cdr (assoc prelude-file (get-prelude-files-and-regions)))))
	(mapcar 'car prelude-regions)))))

(defvar prelude-files-and-regions nil)

(defun get-prelude-files-and-regions ()
  (or prelude-files-and-regions
      (let* ((files (directory-files (concat pvs-path "/lib")
				     t "^prelude\\.pvs$\\|^pvsio_prelude\\.pvs$\\|.*_adt\\.pvs$"))
	     (fregs (mapcar #'(lambda (file)
				(save-excursion
				  (let ((noninteractive t)) ;; Shut up about read-only
				    (set-buffer (find-file-noselect file)))
				  (cons (file-name-nondirectory file) (theory-regions*))))
		      files)))
	(setq prelude-files-and-regions fregs)
	fregs)))

(defun write-prelude-files-and-regions-el ()
  "Ensures pvs-prelude-files-and-regions.el exists and has been loaded"
  (let ((prfile (format "%s/lib/pvs-prelude-files-and-regions.el" pvs-path)))
    (unless (file-exists-p prfile)
      (let ((pfregs (get-prelude-files-and-regions)))
	(with-current-buffer (find-file-noselect (concat pvs-path "/" prfile))
	  (erase-buffer)
	  (insert ";; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;;; Generated automatically - do not edit

(defvar pvs-path)

(defvar prelude-files-and-regions
  (mapcar
      #'(lambda (x)
	 (cons (format \"%s/lib/%s\" pvs-path (car x)) (cdr x)))
    '")
	  (insert (format "%S" pfregs))
	  (insert "))")
	  (write-file (buffer-file-name)))))))

(defun force-completing-read (prompt list)
  (let ((val (completing-read prompt (mapcar 'list list) nil t)))
    (if (equal val "")
	(force-completing-read prompt list)
	(format "%s" val))))


;(defun display-theory-at-location (dir theory loc)
;  (let ((place (car (read-from-string loc)))
;	(file (format "%s%s.pvs" dir theory)))
;    (find-file file)
;    (if (and (integerp (car place)) (integerp (cadr place)))
;	(progn
;	  (goto-line (car place))
;	  (forward-char (cadr place)))
;	(error "Display theory given a bad location"))))

(defun pvs-file-using-chain (filename)
  (pvs-send-and-wait (format "(collect-file-usings \"%s\")" filename)
		     nil nil 'list))

(defun get-pvs-file-dependencies (filename libraries-p)
  (let ((fnames (pvs-send-and-wait (format "(get-pvs-file-dependencies \"%s\")"
				       filename)
				   nil nil 'list)))
    (if libraries-p
	(relativize-pvs-filenames (remove-distributed-pvs-libraries fnames))
	(cl-remove-if #'file-name-directory fnames))))

(defun remove-distributed-pvs-libraries (fnames)
  (cl-remove-if #'in-distributed-pvs-library-p fnames))

(defun in-distributed-pvs-library-p (fname)
  (let ((match-p nil)
	(fdir (file-name-directory fname))
	(libdir (format "%s/lib" pvs-path)))
    (while (and fdir (not match-p))
      (if (file-equal fdir libdir)
	  (setq match-p t)
	  (setq fdir (file-name-directory (substring fdir 0 -1)))))
    match-p))

(defun relativize-pvs-filenames (fnames)
  (mapcar 'relativize-pvs-filename fnames))

(defvar pvs-relativized-directories nil)

(defun relativize-pvs-filename (fname)
  (let ((dir (file-name-directory fname))
	(file (file-name-nondirectory fname)))
    (if (or (null dir)
	    (file-equal dir pvs-current-directory))
	file
	(let ((reldir (cdr (assoc dir pvs-relativized-directories))))
	  (if reldir
	      (concat reldir file)
	      (let* ((dchain1 (pvs-directory-chain (short-file-name dir)))
		     (dchain2 (pvs-directory-chain))
		     (mdir (cl-find-if #'(lambda (d)
					   (member-pvs-file-equal d dchain2))
			     dchain1))
		     (rdir (if mdir
			       (let ((dist (1- (length (member-pvs-file-equal
							mdir
							(reverse dchain2))))))
				 (if (and (<= dist 2)
					  (string-match mdir (car dchain1)))
				     (let ((sdir (substring (car dchain1)
							    (match-end 0))))
				       (if (= dist 0)
					   (concat "./" sdir)
					   (if (= dist 1)
 					       (concat "../" sdir)
					       (concat "../../" sdir))))
				     (car dchain1)))
			       (car dchain1))))
		(push (cons dir rdir) pvs-relativized-directories)
		(concat rdir file)))))))

(defun member-pvs-file-equal (file file-list)
  (when file-list
    (if (file-equal file (car file-list))
	file-list
	(member-pvs-file-equal file (cdr file-list)))))

(defun pvs-directory-chain (&optional dir)
  (let ((fdir (short-file-name (or dir pvs-current-directory)))
	(dchain nil))
    (while fdir
      (if (file-equal fdir pvs-path)
	  (setq fdir nil)
	  (push fdir dchain)
	  (setq fdir (file-name-directory (substring fdir 0 -1)))))
    (nreverse dchain)))

(defun pvs-region (place)
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- (car place)))
    (forward-char (cadr place))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (1- (caddr place)))
      (forward-char (cadddr place))
      (list beg (point)))))

(defun make-command-from-paths (command paths)
  (if (null paths)
      (error "Command %s not found" command)
      (let ((file (concat (car paths)
			  (if (string-equal (substring (car paths) -1) "/")
			      "" "/")
			  command)))
	(if (file-exists-p file)
	    file
	    (make-command-from-paths command (cdr paths))))))


;;; Shamelessly stolen from tags.el, and modified to just look for words.

;; Return a default name to search for, based on the text at point.
(defun find-name-default ()
  (let ((case-fold-search t))
    (save-excursion
      (while (and (not (looking-at "\\sw\\|\\s_"))
		  (not (= (point) (point-max))))
	(forward-char 1))
      (cond ((looking-at "\\sw")
	     (find-name-default* "\\sw"))
	    ((looking-at "\\s_")
	     (find-name-default* "\\s_"))))))

(defun find-name-default* (chclass)
  ;; Get to beginning of word
  (while (and (looking-at chclass)
	      (not (= (point) (point-min))))
    (forward-char -1))
  (unless (looking-at chclass)
    (forward-char 1))
  ;; Now save point, get to end, and return substring
  (let ((start (point)))
    (while (looking-at chclass)
      (forward-char 1))
    (buffer-substring-no-properties start (point))))

(defun find-pvs-name (string)
  (let* ((default (find-name-default))
	 (spec (read-string
		(if default
		    (format "%s(default %s) " string default)
		  string))))
    (list (if (equal spec "")
	      default
	    spec))))

(defun member-equal (item list)
  (cl-member item list :test 'equal))

(defun pvs-abbreviate (cmd abbrevs)
  (let ((abbrs (cl-remove-if #'fboundp
		 (if (listp abbrevs)
		     abbrevs
		     (list abbrevs)))))
    (mapc #'(lambda (a) (fset a cmd)) abbrs)
    (put cmd 'abbreviations abbrs)))

(defun pvs-get-abbreviation (cmd)
  (or (car (get cmd 'abbreviations))
      cmd))

(defun add-final-newline ()
  (save-excursion
    (unless (equal (char-after (1- (point-max))) ?\n)
      (goto-char (point-max))
      (insert ?\n))))

(defun first-nonblank-line ()
  (save-excursion
    (goto-char (point-min))
    (let ((la (looking-at "\\(^[ \t\n]*$\\)")))
      (if la (+ (match-end 0) 1) 1))))

(defun delete-initial-blank-lines ()
  (save-excursion
    (goto-char (point-min))
    (when (and (> (point-max) (point-min))
	       (looking-at "\\(^[ \t\n]*$\\)"))
      (delete-blank-lines)
      (delete-initial-blank-lines))))

(defun pvs-extensions-regexp ()
  (mapconcat #'(lambda (x) (format "\\.%s$" x))
	     *pvs-file-extensions*
	     "\\|"))


;;; Window configuration handling minipackage, courtesy of Tim Winkler
(defvar window-configurations nil)

;;; push current configuration onto stack 
(defun pushw ()
  (setq window-configurations
	(cons (current-window-configuration) window-configurations)))

;;; restore and remove top of stack 
(defun popw ()
  (if window-configurations
      (let ((config (car window-configurations)))
	(setq window-configurations (cdr window-configurations))
	(set-window-configuration config))
    ;;(princ "nothing to restore")
    ))

;;; restore top of stack without deleting it
(defun setw ()
  (if window-configurations
      (let ((config (car window-configurations)))
	(setq window-configurations (cdr window-configurations))
	(set-window-configuration config)
	(setq window-configurations
	      (cons (current-window-configuration) window-configurations)))
    ;;(princ "nothing to restore")
    ))

;;; delete top of stack without restoring it
(defun tossw ()
  (if window-configurations
      (setq window-configurations (cdr window-configurations))
    (princ "nothing to discard")))

;;; clear stack
(defun resetw ()
  (setq window-configurations nil))

(defun pvs:make-listing (strings)
  (let* ((colsize (+ (max-string-length strings) 2))
	 (numcols (/ (window-width) colsize))
	 (div (/ (length strings) numcols))
	 (mod (% (length strings) numcols))
	 (numrows (+ div (if (= mod 0) 0 1)))
	 (listing ""))
    (dotimes (r numrows)
      (setq listing
	    (concat listing
		    (pvs:make-listing-row strings r numcols colsize div mod))))
    listing))

(defun pvs:make-listing-row (list row numcols colsize div mod)
  (let ((lrow "\n")
	(spaces (make-string colsize ? )))
    (dotimes (i numcols)
      (let* ((c (- numcols i 1))
	     (elt (if (and (= row div) (>= c mod))
		      ""
		      (nth (+ (* row numcols) c)
			   list))))
	(setq lrow
	      (concat (format "%s%s" elt (substring spaces (length elt)))
		      lrow))))
    lrow))

(defun max-string-length (strings &optional l)
  (dolist (str strings)
    (let ((ls (length (if (stringp str) str (format "%s" str)))))
      (if (or (null l) (> ls l)) (setq l ls))))
  l)

;;; end of window config

(setq pvs-reserved-words-regexp
  "\\bassuming\\b\\|\\baxiom\\b\\|\\baccept\\b\\|\\bchanges\\b\\|\\ball\\b\\|\\band\\b\\|\\barray\\b\\|\\bbegin\\b\\|\\bby\\b\\|\\bcase\\b\\|\\bdeclare\\b\\|\\bdefinition\\b\\|\\belse\\b\\|\\belsif\\b\\|\\bendif\\b\\|\\bendassuming\\b\\|\\bendcase\\b\\|\\bend\\b\\|\\bexists\\b\\|\\bexporting\\b\\|\\bexit\\b\\|\\bforall\\b\\|\\bfunction\\b\\|\\bformula\\b\\|\\bfrom\\b\\|\\bif\\b\\|\\biff\\b\\|\\bimplies\\b\\|\\bimporting\\b\\|\\bin\\b\\|\\bis\\b\\|\\blambda\\b\\|\\blemma\\b\\|\\bloop\\b\\|\\bmapping\\b\\|\\bmeasure\\b\\|\\bmodule\\b\\|\\bnot\\b\\|\\bnothing\\b\\|\\bof\\b\\|\\bonto\\b\\|\\bobligation\\b\\|\\bopspec\\b\\|\\bor\\b\\|\\bproof\\b\\|\\bprove\\b\\|\\brecursive\\b\\|\\bresult\\b\\|\\btheorem\\b\\|\\btheory\\b\\|\\busing\\b\\|\\bvar\\b\\|\\bvariable\\b\\|\\brecord\\b\\|\\bverify\\b\\|\\bwhere\\b\\|\\bthen\\b\\|\\btype\\b\\|\\bwhen\\b\\|\\bwhile\\b\\|\\bwith\\b\\|\\blet\\b\\|\\bsetvariable\\b\\|\\[#\\|#\\]\\|[(]#\\|#[)]")

(defmacro pvs-find-face (name)
  (if (featurep 'xemacs)
      `(find-face ,name)
    `(facep ,name)))

(defun find-unbalanced-pvs-tex (arg)
  "Go to the point in buffer where PVS delimiters become unbalanced.
Point will be on the offending delimiter.  If called with a prefix, use
the current region."
  (interactive "P")
  (if arg
      (call-interactively 'find-unbalanced-region-pvs-tex)
      (find-unbalanced-region-pvs-tex (point-min) (point-max)))
  (unless ilisp-complete (message "All delimiters balance")))

(defun find-unbalanced-region-pvs-tex (start end)
  "Go to the point in region where PVS delimiters become unbalanced.
Point will be on the offending delimiter."
  (interactive "r")
  (pvs-count-char-pairs start end "[" "]")
  (pvs-count-char-pairs start end "(" ")")
  (pvs-count-char-pairs start end "{" "}")
  (pvs-count-string-pairs start end "\\\\ii" "\\\\oo" t)
  (pvs-count-string-pairs start end "\\\\zi" "\\\\zo" t))

(defun define-pvs-key-bindings (buffer)
  (with-current-buffer buffer
    (let ((lmap (current-local-map)))
      (unless lmap
	(setq lmap (make-sparse-keymap))
	(use-local-map lmap))
      (define-key lmap "\M-," 'find-declaration)
      (define-key lmap "\M-;" 'whereis-declaration-used)
      (define-key lmap "\M-:" 'list-declarations)
      (define-key lmap [(control ?.)] 'show-expanded-form))))

(defun all-pvs-commands ()
  (let ((pvs-commands nil))
    (mapatoms (function
	       (lambda (sym)
		 (if (get sym 'pvs-command)
		     (setq pvs-commands (cons sym pvs-commands))))))
    pvs-commands))

(defun pvs-command-classes ()
  (let ((cmds (all-pvs-commands))
	(classes nil))
    (dolist (cmd cmds)
      (let ((class (get cmd 'pvs-command)))
	(cl-pushnew class classes)))
    classes))

(defun pvs-commands-of-class (class)
  (let ((cmds (all-pvs-commands))
	(commands nil))
    (dolist (cmd cmds)
      (when (eq (get cmd 'pvs-command) class)
	(push cmd commands)))
    (sort commands 'string<)))

(defun pvs-short-documentation (command)
  (save-match-data
    (let ((doc (documentation command)))
      (string-match "^.*$" doc)
      (substring doc 0 (match-end 0)))))

(defun pvs-command-class (command)
  (get command 'pvs-command))

(defun goto-pvs-proof-buffer ()
  (if pvs-in-checker
      (unless (equal (buffer-name) ilisp-buffer)
	(pop-to-buffer ilisp-buffer)
	(goto-char (point-max)))
      (error "No proof is currently running")))

(defun pvs-emacs-query (prompt &rest args)
  (let* ((qprompt (concat (format prompt args)
			  " [Type y(es), n(o), q(uit) or !(always)] "))
	 (char-list (append "ynq!" nil))
	 (char (read-char-from-minibuffer qprompt char-list)))
    char))

(defun directory-writable-p (dirname)
  (let* ((edir (expand-file-name dirname))
	 (dirnames (reverse (split-string edir "/")))
	 (dname edir))
    (while dirnames
      (if (file-exists-p dname)
	  (setq dirnames nil)
	  (setq dname (apply 'concat
			(mapcar #'(lambda (x) (concat "/" x))
			  (reverse dirnames))))
	  (setq dirnames (cdr dirnames))))
    (and (stringp dname)
	 (file-exists-p dname)
	 (file-directory-p dname)
	 (file-writable-p dname))))

(define-advice rename-buffer (:around (oldfun newname &optional unique))
  (prog1 (funcall oldfun newname unique)
    (makunbound 'current-pvs-file)))

(defun real-current-column ()
  (- (point) (save-excursion (beginning-of-line) (point))))


;; NB - log file is deliberately in default-directory rather than the
;; desired context, to allow for "remote" validation, potentially without
;; write permission on the directory

;;; pvs-expected-output should be set to a regexp within pvs-validate.
;;;   This regexp is then looked for and an ERROR message is generated if it
;;;   is not found.
;;; pvs-unexpected-output is also a regexp, but the error message is
;;;   generated if the regexp is found - this is needed because there is no
;;;   easy way to negate a regexp.
(defvar pvs-expected-output nil)
(defvar pvs-unexpected-output nil)

(defmacro pvs-validate (file directory &rest body)
  `(let* ((start-time (current-time))
	  (logfile (concat default-directory ,file)))
       (pvs-backup-logfile logfile)
       (let ((logbuf (find-file-noselect logfile t)))
	 (unwind-protect
	     (save-excursion
	       ;;(fset 'pvs-handler-orig 'pvs-handler)
	       ;;(fset 'pvs-handler 'pvs-validate-handler)
	       (setq comint-handler 'pvs-validate-handler)
	       (fset 'ask-user-about-lock-orig 'ask-user-about-lock)
	       (fset 'ask-user-about-lock 'pvs-log-ignore-lock)
	       (set-buffer logbuf)
	       (when buffer-read-only (toggle-read-only))
	       (clear-buffer logbuf)
	       (let ((standard-output logbuf)
		     (pvs-validating t)
		     (default-directory default-directory))
		 (pvs-message (pvs-version-string))
		 (let ((pvs-disable-messages nil))
		   (change-workspace ,directory))
		 (condition-case err
		     (progn ,@body)
		   (error (pvs-message "ERROR: Emacs: %s %s"
			    (car err) (cdr err))))
		 (pvs-wait-for-it)
		 (pvs-message "Validation completed in %.2f seconds"
		   (time-to-seconds (time-since start-time))))
	       ;;(save-buffer 0) ;writes a message-use the following 3 lines
	       (set-buffer logbuf)	; body may have changed active buffer
	       (when pvs-expected-output
		 (let ((standard-output logbuf)
		       (expected-list (pvs-expected-output-regexps
				       pvs-expected-output))
		       (last-point (point-min))
		       (count 0))
		   (dolist (exp expected-list)
		     (pvs-message "Checking for%s expected output"
		       (if (null (cdr expected-list))
			   ""
			   (cl-case (cl-incf count)
			     (1 " 1st") (2 " 2nd") (3 " 3rd")
			     (t (format " %dth" count)))))
		     (let ((foundit nil))
		       (save-excursion
			 (goto-char last-point)
			 ;;(pvs-message "Before searching for \n%s" exp)
			 (setq foundit
			       (let ((search-spaces-regexp "[ \t\n]+"))
				 (re-search-forward exp nil t)))
			 (when foundit (setq last-point foundit)))
		       (if foundit
			   (pvs-message "[32;1mFound expected output[0m")
			   (pvs-message "[31;1mERROR: %s: expected output not found[0m"
			     logfile))))))
	       (when pvs-unexpected-output
		 (let ((standard-output logbuf)
		       (unexpected-list (pvs-expected-output-regexps
					 pvs-unexpected-output))
		       (last-point (point-min)))
		   (dolist (exp unexpected-list)
		     (pvs-message "Checking for%s unexpected output"
		       (if (null (cdr unexpected-list))
			   ""
			   (let ((pos (position exp unexpected-list
						:test 'equal)))
			     (cl-case pos
			       (0 " 1st") (1 " 2nd") (2 " 3rd")
			       (t (format " %dth" (+ pos 1)))))))
		     (let ((foundit nil))
		       (save-excursion
			 (goto-char last-point)
			 (setq foundit
			       (re-search-forward-lax-whitespace exp nil t))
			 (when foundit (setq last-point foundit)))
		       (if foundit
			   (pvs-message "[31;1mERROR: %s: unexpected output found[0m"
			     logfile)
			   (pvs-message "[32;1mDid not find unexpected output[0m"))))))
	       (write-region (point-min) (point-max) (buffer-file-name) nil 'nomsg)
	       (set-buffer-modified-p nil)
	       (clear-visited-file-modtime)
	       ;; (if (file-exists-p "baseline.log")
	       ;; 	   (let ((baseline-log (find-file-noselect "baseline.log")))
	       ;; 	     (if (pvs-same-validation-buffers-p logbuf baseline-log)
	       ;; 		 (pvs-message "No significant changes since baseline")
	       ;; 		 (pvs-message "WARNING: Differences found - check %s" logfile)))
	       ;; 	   (progn
	       ;; 	     (pvs-message "NO BASELINE - using this run to create baseline.log")
	       ;; 	     (copy-file (buffer-file-name) "baseline.log")))
	       )
	   ;;(fset 'pvs-handler 'pvs-handler-orig)
	   (setq comint-handler 'pvs-handler)
	   ;;(fset 'ask-user-about-lock 'ask-user-about-lock-orig)
	   ))
       (with-current-buffer ilisp-buffer
	 (setq kill-buffer-query-functions nil)
	 ;;(setq kill-emacs-hook nil)
	 ;;(setq confirm-kill-processes nil)
	 (kill-buffer))
       (kill-emacs)))

(defun validation-log-file ()
  (format "%s-%s-%s.log"
      (pvs-git-description)
    (car (process-lines "pvs-platform"))
    (getenv "PVSLISP")))

(defun pvs-git-description ()
  "E.g., pvs7.0-647-g8c1572bb"
  (car (split-string (call-process-to-string "git" "-C" pvs-path "describe"))))
  
(defun call-process-to-string (program &rest args)
  (with-temp-buffer
    (apply #'call-process program nil (current-buffer) nil args)
    (buffer-string)))

;;; This function provides the most basic form of test, removing bin
;;; files, typechecking a file, then running prove-importchain on it.
;;; After, it runs any functions in pvs-validate-hooks.
(defun pvs-validate-typecheck-and-prove (filename &optional show-proofp
						  importchainp theory formula)
  (pvs-validate-typecheck filename)
  (set-rewrite-depth 0)
  (let ((current-prefix-arg t)
	(overbose pvs-verbose))
    (when (and show-proofp
	       (< pvs-verbose 3))
      (pvs-message
	  "Resetting verbose level to 3 for the proof of this validation")
      (setq pvs-verbose 3)
      (pvs-send-and-wait "(setq *pvs-verbose* 3)"))
    (if importchainp
	(if formula
	    (pvs-message "Don't set importchainp and include formula")
	    (if theory
		(prove-importchain theory)
		(prove-importchain filename)))
	(if formula
	    (let ((prvform (format "(prove-formula \"%s#%s\" :rerun? t)"
			       (or theory filename) formula)))
	      ;;(pvs-message "Proving formula %s" prvform)
	      (ilisp-send prvform nil 'pr t 'pvs-handler)
		   (sleep-for 1)
		   (pvs-wait-for-it)
		   (pvs-validate-quit-prover)
		   (when pvs-in-checker
		     (interrupt-process (ilisp-process))
		     (comint-send (ilisp-process) ":reset")))
	    (if theory
		(prove-theory theory)
		(prove-pvs-file filename))))
    (setq pvs-verbose overbose)
    (pvs-send (format "(setq *pvs-verbose* %s)" overbose)))
  (run-hooks 'pvs-validate-proof-hooks))

(defun pvs-validate-typecheck (filename)
  (pvs-remove-bin-files)
  (find-pvs-file filename)
  (typecheck filename)
  (run-hooks 'pvs-validate-hooks))

(defun pvs-validate-start-proof (&optional rerun filename formula)
  ;; Starts a proof - by default in the current filename at the cursor
  ;; position, and the proof is displayed.
  (let* ((fref (pvs-formula-origin))
	 (kind (pvs-fref-kind fref))
	 (fname (pvs-fref-file fref))
	 (buf (pvs-fref-buffer fref))
	 (line (pvs-fref-line fref))
	 ;; (poff (pvs-fref-prelude-offset fref))
	 (theory (pvs-fref-theory fref))
	 (fmla (pvs-fref-formula fref))
	 (fmlastr (when fmla (format "\"%s\"" fmla)))
	 (pvs-error nil))
    (if formula
	(ilisp-send (format "(prove-formula \"%s\" \"%s\" %s)"
			filename formula rerun)
		    nil 'pr t 'pvs-handler)
	(ilisp-send
	 (format "(prove-file-at \"%s\" %s %d %s \"%s\" \"%s\")"
	     (or fname theory) fmlastr line rerun kind buf)
	 nil 'pr))
    (unless pvs-error
      (ilisp-switch-to-lisp t t)
      (pvs-wait-for-it)
      (unless pvs-in-checker
	(pvs-message "ERROR: proof wasn't started for some reason")))))

(defun pvs-validate-send-prover-command (string)
  (when pvs-in-checker
    (comint-send (ilisp-process) string)
    (pvs-wait-for-it)))

(defun pvs-validate-quit-prover ()
  (when pvs-in-checker
    (comint-send (ilisp-process) "(quit)y\n")
    (pvs-wait-for-it)))

(defun pvs-validate-show-buffer (bufname)
  (with-current-buffer bufname
    (princ-nl (buffer-string))))

(defun pvs-expected-output-regexps (output-regexps)
  (if (stringp output-regexps)
      (list output-regexps)
      (if (pvs-valid-output-regexps output-regexps)
	  output-regexps
	  (error "pvs-expected-output should be a string or list of strings"))))

(defun pvs-valid-output-regexps (output-regexps)
  (or (null output-regexps)
      (and (listp output-regexps)
	   (stringp (car output-regexps))
	   (pvs-valid-output-regexps (cdr output-regexps)))))

(defun pvs-validate-handler (error-p wait-p message output prompt)
  ;; (message "pvs-validate-handler called: %s %s %s %s %s"
  ;; 	   error-p wait-p message output prompt)
  (ignore error-p wait-p message prompt)
  (cond ((and (stringp output)
	      (string-match (ilisp-value 'ilisp-error-regexp) output))
	 (pvs-message "Lisp ERROR: %s\n" (comint-remove-whitespace output))
	 ;;(reset-pvs)
	 t)
	(t t)))

(defmacro pvs-validate-and-die (file directory &rest body)
  `(progn (pvs-validate ,file ,directory ,@body)
	  (kill-process (ilisp-process))))

(defun pvs-log-ignore-lock (file opponent)
  (ignore file opponent)
  nil)

(defvar *pvs-backup-logfiles* 4
  "Number of backup logfiles to keep after a validation run.")

(defun pvs-backup-logfile (logfile)
  (or (not (file-exists-p logfile))
      (let ((logbuf (find-file-noselect logfile t)))
	(with-current-buffer logbuf
	  (let ((delete-old-versions t)
		(kept-old-versions 0)
		(kept-new-versions *pvs-backup-logfiles*)
		(version-control t)
		(backup-inhibited nil))
	    (backup-buffer))))))

(defvar pvs-validation-regexp
  "^PVS Version.*$\\|^Validating /.*$\\|[0-9]+ files removed$\\|Deleted file /.*\\|Proving \..*\\|No bin files found\\|\\(\( ?\\)?[0-9]+\\(\.[0-9]+\\)? s\)\\|[0-9]+\\(\.[0-9]+\\)?\\( seconds\\|s:\\)\\|[0-9]+\\(\.[0-9]+\\)?\\( real,\\| cpu seconds\\)\\| \(library /.*\)$\\|[ \t\n]"
  "Regexp used in pvs-compare-windows-whitespace.  Because of the way
pvs-compare-windows-skip-whitespace is defined, it must either match at
the point where the buffers would otherwise differ, or match at the
beginning of the line.  The default is to consider patch levels, bin file
existence and time differences to be whitespace")

(defun pvs-same-validation-buffers-p (log1 log2)
  (not (pvs-find-validation-buffers-mismatch log1 log2)))


;; derived from compare-windows 
(defun pvs-compare-validation-windows ()
  (interactive)
  (let ((p1 (point))
	(b1 (current-buffer))
	(p1max (point-max))
	p2 b2 p2max
	(w2 (next-window (selected-window))))
    (if (eq w2 (selected-window))
	(setq w2 (next-window (selected-window) nil 'visible)))
    (if (eq w2 (selected-window))
	(error "No other window"))
    (setq p2 (window-point w2)
	  b2 (window-buffer w2))
    (with-current-buffer b2
      (setq p2max (point-max))
      (push-mark p2 t))
    (push-mark)
    
    (let ((mismatch (pvs-find-validation-buffers-mismatch b1 b2 p1 p2)))
      (cond (mismatch
	     (goto-char (car mismatch))
	     (set-window-point w2 (cadr mismatch))
	     (message "Window difference found")
	     (ding))
	    (t
	     (goto-char p1max)
	     (set-window-point w2 p2max)
	     (message "Windows match"))))))

(defun pvs-find-validation-buffers-mismatch (log1 log2 &optional spos1 spos2)
  (let* ((pos1 (or spos1 1))
	 (pos2 (or spos2 1))
	 (end1 (with-current-buffer log1 (point-max)))
	 (end2 (with-current-buffer log2 (point-max)))
	 (dpos (compare-buffer-substrings log1 pos1 end1 log2 pos2 end2))
	 (match t)
	 ipos1 ipos2)
    (while (and (/= dpos 0) match)
      (setq pos1 (+ (abs dpos) pos1 -1) ipos1 pos1)
      (setq pos2 (+ (abs dpos) pos2 -1) ipos2 pos2)
      (if (or (let ((p1match (with-current-buffer log1
			       (goto-char pos1)
			       (when (looking-at pvs-validation-regexp)
				 (setq pos1
				       (if (eq (match-end 0) (line-end-position))
					   (1+ (match-end 0))
					   (match-end 0)))))))
		(or (with-current-buffer log2
		      (goto-char pos2)
		      (when (looking-at pvs-validation-regexp)
			(setq pos2
			      (if (eq (match-end 0) (line-end-position))
				  (1+ (match-end 0))
				  (match-end 0)))))
		    p1match))
	      ;; the following allows for diffs where something in the regexp
	      ;; matches the *entire* line.
	      (let ((p1match (with-current-buffer log1
			       (goto-char ipos1)
			       (beginning-of-line)
			       (when (and (looking-at pvs-validation-regexp)
					  (eq (match-end 0) (line-end-position)))
				 (setq pos1 (1+ (match-end 0)))))))
		(or (with-current-buffer log2
		      (goto-char ipos2)
		      (beginning-of-line)
		      (when (and (looking-at pvs-validation-regexp)
				 (eq (match-end 0) (line-end-position)))
			(setq pos2 (1+ (match-end 0)))))
		    p1match)))
	  (setq dpos (compare-buffer-substrings log1 pos1 end1 log2 pos2 end2))
	  (setq match nil)))
    (unless match
      (list pos1 pos2))))

(defvar pvs-waiting nil)

;;; pvs-wait-for-it waits until the *pvs* buffer is at the prompt,
;;; i.e., with a :ready or :error status.  Used in pvs-validate to
;;; ensure that pending commands are complete before checking for the
;;; (un)expected regexp.
(defun pvs-wait-for-it (&optional timeout)
  (sleep-for 1)
  (while (and (ilisp-process)
	      (or (null timeout)
		  (> timeout 0))
	      (not (with-current-buffer ilisp-buffer
		     ;; comint-status is buffer-local
		     ;;(message "Waiting...%s %s %s"
			;;      comint-status (process-status (ilisp-process))
			;;      timeout)
		     ;; (pvs-message "pvs-wait-for-it: %s" comint-status)
		     (member comint-status '(" :ready" " :error")))))
;;     (when (and timeout (= timeout 90)
;; 	       (eq (process-status (ilisp-process)) 'signal))
;;       (comint-send (ilisp-process) ":bt\n")
;;       (save-excursion
;; 	(set-buffer "pvs")
;; 	(goto-char (point-min))
;; 	(message (buffer-string))))
    (when timeout (cl-decf timeout))
    (sleep-for 1)))

(defun pvs-message (control-string &rest data)
  (princ (apply 'format control-string data))
  (terpri)
  (princ (apply 'format control-string data) 'external-debugging-output)
  (terpri 'external-debugging-output))

(when noninteractive
(define-advice message (:around (oldmsg format-string &rest args))
  (unless (and noninteractive (= pvs-verbose 0))
    (apply oldmsg format-string args)))

(define-advice yes-or-no-p (:around (oldfun prompt))
  (if noninteractive
      (let ((pvs-verbose 1))
	(message prompt)
	t)
      (apply oldfun prompt)))

(define-advice y-or-n-p (:around (oldfun prompt))
  (if noninteractive
      (let ((pvs-verbose 1))
	(message prompt)
	t)
      (apply oldfun prompt)))

;; (prompter actor list &optional help action-alist no-cursor-in-echo-area)
(define-advice save-some-buffers (:around (oldfun &optional arg pred))
  (ignore oldfun arg pred)
  (save-window-excursion
    (dolist (buf (buffer-list))
      (when (and (buffer-modified-p buf)
		 (not (buffer-base-buffer buf))
		 (buffer-file-name buf))
	(set-buffer buf)
	(save-buffer)))))
  

;;; Can't use kill-emacs-hook instead, as in batch mode the hook is ignored.
;; (defadvice kill-emacs (before pvs-batch-control activate)
;;   (fset 'pvs-handler 'pvs-validate-handler)
;;   (if (and ilisp-buffer
;; 	   (get-buffer ilisp-buffer)
;; 	   (ilisp-process)
;; 	   (eq (process-status (ilisp-process)) 'run))
;;       (progn
;; 	(save-excursion
;; 	  (set-buffer (ilisp-buffer))
;; 	  (message "Status before: %s" comint-status))
;; 	(comint-send (ilisp-process) "(save-context)")
;; 	(message "Waiting for save-context...")
;; 	(pvs-wait-for-it 100)
;; 	(let ((ctr 10))
;; 	  (save-some-buffers nil t)
;; 	  (while (and (not (save-excursion
;; 			     (set-buffer (ilisp-buffer))
;; 			     (member comint-status '(" :ready" " :error"))))
;; 		      (> ctr 0))
;; 	    (setq ctr (- ctr 1))
;; 	    (sleep-for 1))))
;;       (message "PVS not running - context not saved"))
;;   (message "PVS Exited"))

)

(defun trailing-components (directory num)
  (let ((comps (nreverse (split-string directory "/")))
	(sdir "")
	(n 0))
    (while (and (< n num) comps)
      (unless (equal (car comps) "")
	(if (equal sdir "")
	    (setq sdir (car comps))
	    (setq sdir (format "%s/%s" (car comps) sdir)))
	(setq n (+ n 1)))
      (setq comps (cdr comps)))
    sdir))

(defun remove-trailing-slash (dir)
  (if (= (aref dir (1- (length dir))) ?/)
      (substring dir 0 (1- (length dir)))
      dir))

(defun pvs-title-string ()
  nil)

(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame  (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
      (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(cond ((featurep 'xemacs)
       (defun pvs-update-window-titles ()
	 (unless noninteractive
	   (let ((title (pvs-title-string)))
	     (when title
	       (setq frame-title-format title)
	       (setq frame-icon-title-format title))))))
      (t
       (defun pvs-update-window-titles ()
	 (unless noninteractive
	   (let ((title (pvs-title-string))
		 (pvs-frame (cl-find-if #'(lambda (fr)
					 (string= (get-frame-name fr) "pvs"))
			      (frame-list))))
	     (when (and pvs-frame title
			(not (equal (frame-parameter pvs-frame 'title)
				    "Proofstate")))
	       (modify-frame-parameters pvs-frame
					(list (cons 'icon-name title)
					      (cons 'title title)))))))))

(add-hook 'change-workspace-hook 'pvs-update-window-titles)

(provide 'pvs-utils)
