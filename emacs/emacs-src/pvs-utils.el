;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-utils.el -- 
;; Author          : Sam Owre
;; Created On      : Wed Sep 15 17:51:30 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Oct 15 02:57:25 1995
;; Update Count    : 17
;; Status          : Beta test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'pvs-macros))

(defvar *pvs-theories* nil)
(defvar *pvs-current-directory* nil)
(defvar *pvs-file-extensions* '("pvs"))
(defvar pvs-default-timeout 10)

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
  (let ((limit (point)))
    (if (find-theory-or-datatype-forward start)
	(let ((opoint (point)))
	  (re-search-backward-ignoring-comments ":" nil t)
	  (backward-sexp)
	  (while (in-comment) (backward-sexp))
	  (when (looking-at "\\[")
	    (backward-sexp)
	    (while (in-comment) (backward-sexp)))
	  t)
	(goto-char (or start (point)))
	nil)))


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
		(or (in-comment)
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
		(or (in-comment)
		    (in-square-braces (or start (point-min)))
		    (in-begin-end (or start (point-min)))))
      (setq found (re-search-backward "\\(\\btheory\\b\\|\\bdatatype\\b\\)"
				      nil t)))
    (if found
	t
	(goto-char opoint)
	nil)))

(defun current-theory-region ()
  (let ((case-fold-search t))
    (save-match-data
      (unless (= (point) (point-max))
	(forward-char 1))
      (backward-theory t)
      (unless (beginning-of-theory)
	(error "Can't determine theory boundaries"))
      (looking-at "\\w+")
      (let ((start (point))
	    (th-id (buffer-substring (match-beginning 0) (match-end 0))))
	(find-theory-or-datatype-forward)
	(unless (beginning-of-theory (point))
	  (goto-char (point-max)))
	(if (< start (point))
	    (list th-id start (point))
	    nil)))))

(defun theory-regions ()
  (when (current-pvs-file t)
    (condition-case err
	(find-unbalanced-region-pvs (point-min) (point-max))
      (error (error "Can't determine theory boundaries: %s" (cadr err))))
    (theory-regions*)))

(defun theory-regions* ()
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
	    (setq pname (buffer-substring (match-beginning 0) (match-end 0)))
	    (find-theory-or-datatype-forward)))
	(when pname
	  (push (list pname opoint (point-max)) tregs))
	(nreverse tregs)))))

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
    (while (in-comment)
      (re-search-forward regexp limit noerror repeat))))

(defun re-search-backward-ignoring-comments (regexp &optional
						    limit noerror repeat)
  (let ((case-fold-search t))
    (re-search-backward regexp limit noerror repeat)
    (while (in-comment)
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
	     (error "Theory ~a was in ~a which no longer exists"
		    theoryname (car fandp)))
	    ((or (null (cdr fandp))
		 (buffer-modified-p (find-file-noselect (car fandp))))
	     (unless must-match
	       (save-excursion
		 (set-buffer (find-file-noselect (car fandp)))
		 (theory-region-from-buffer theoryname))))
	    (t (file-and-place-to-region fandp))))))

(defun theory-region-from-buffer (theoryname)
  (let ((theory-region nil))
    (dolist (buf (buffer-list))
      (when (and (null theory-region)
		 (pvs-buffer-file-name buf))
	(save-excursion
	  (set-buffer buf)
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
	(let* ((bufp (get-file-buffer file))
	       (buf (find-file-noselect file)))
	  (set-buffer buf)
	  (message "Processing %s" file)
	  (let ((threg (theory-region-from-buffer theoryname)))
	    (when threg
	      (push (cons file threg) thregs))))))
    thregs))

(defun file-and-place-to-region (file-and-place)
  (let ((file (car file-and-place))
	(place (cdr file-and-place)))
    (save-excursion
      (set-buffer (find-file-noselect (car file-and-place)))
      (cons file (pvs-region place)))))

(defun buffer-theories ()
  (let ((file (current-pvs-file t)))
    (when file
      (mapcar 'car (theory-regions)))))

(defun get-file-buffer (filename)
  "Modified get-file-buffer for correctly handling automount names"
  (if (file-exists-p filename)
      (let ((fbuf nil)
	    (attr (file-attributes filename))
	    (nname (file-name-nondirectory filename))
	    (blist (buffer-list)))
	(while (and (null fbuf) blist)
	  (let ((fname (buffer-file-name (car blist))))
	    (if (and fname
		     (file-exists-p fname)
		     (equal (file-name-nondirectory fname) nname)
		     (equal attr (file-attributes fname)))
		(setq fbuf (car blist)))
	    (setq blist (cdr blist))))
	fbuf)))

(defun in-comment ()
  (let ((limit (point)))
    (save-excursion
      (beginning-of-line)
      (search-forward "%" limit t))))

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
  (pvs-count-char-pairs start end "[" "]")
  (pvs-count-char-pairs start end "(" ")")
  (pvs-count-char-pairs start end "{" "}")
  (pvs-count-string-pairs start end "begin" "end"))

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
	  (unless (in-comment)
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
	  (unless (in-comment)
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
	(unless (in-comment)
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
	  (unless (in-comment)
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

(defun all-theories-parsed-p (root)
  (pvs-send-and-wait
   (format "(and (all-theories-parsed? (get-theory \"%s\")) t)" root)
   nil nil 'bool))

(defun typechecked-p (theory)
  (pvs-send-and-wait
   (format "(and (typechecked? (get-theory \"%s\")) t)" theory)
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
  (save-excursion
    (set-buffer buf)
    (and (buffer-file-name)
	 (string-match (pvs-extensions-regexp) (buffer-file-name)))))

(defun pvs-buffer-file-name (&optional buf)
  (unless buf (setq buf (current-buffer)))
  (save-excursion
    (set-buffer buf)
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
		(save-excursion
		  (set-buffer buff)
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

(defun get-theory-buffer (theoryname)
  (let ((filename (cadr (assoc theoryname *pvs-theories*))))
    (when filename (get-pvs-file-buffer filename))))

(defun get-pvs-file-buffer (fname)
  (let* ((name (pathname-name fname))
	 (ext (pathname-type fname))
	 (pdir (pathname-directory name))
	 (dir (if (equal pdir "")
		  *pvs-current-directory*
		  pdir)))
    (if (and ext (not (equal ext "")))
	(let ((filename (format "%s%s.%s" dir name ext)))
	  (find-file-noselect filename))
	(let ((files nil))
	  (dolist (ext *pvs-file-extensions*)
	    (let ((filename (format "%s%s.%s" dir name ext)))
	      (when (file-exists-p filename)
		(push filename files))))
	  (cond ((cdr files)
		 (error "%s is ambiguous: one of %s" name files))
		((car files)
		 (find-file-noselect (car files))))))))

(defun pvs-file-name (filename &optional ext)
  (format "%s%s.%s" *pvs-current-directory* filename
	  (if (and ext
		   (not (equal ext "")))
	      ext
	      "pvs")))

(defun clear-buffer (buffer)
  (save-excursion
    (set-buffer buffer)
    (erase-buffer)))

(defvar current-pvs-file 'unbound)
(make-variable-buffer-local 'current-pvs-file)

(defun current-pvs-file (&optional no-error)
  (if (and no-error
	   (not (eq current-pvs-file 'unbound)))
      current-pvs-file
      (pvs-current-directory)
      (cond ((or (not (buffer-file-name))
		 (not (member-equal (pathname-type (buffer-file-name))
				    *pvs-file-extensions*)))
	     (unless no-error
	       (error "%s is not a valid PVS file" (buffer-name))))
	    ((file-equal (buffer-file-name)
			 (format "%s/lib/prelude.pvs" pvs-path))
	     (setq current-pvs-file (pathname-name (buffer-file-name))))
	    ((file-equal (buffer-file-name)
			 (format "%s%s"
			     *pvs-current-directory*
			   (file-name-nondirectory (buffer-file-name))))
	     (setq current-pvs-file (pathname-name (buffer-file-name))))
	    ((pvs-library-file (buffer-file-name)))
	    (t (unless no-error
		 (error "%s is not in the current context"
			(buffer-file-name)))))))

(defun pvs-library-file (filename)
  (and (pvs-send-and-wait (format "(library-file? \"%s\")" filename)
			  nil nil 'bool)
       (concat (pathname-directory filename) (pathname-name filename))))


;;; File-equal compares files according to attributes, not names.
;;; Note that this doesn't work with hard links.

(defun file-equal (file1 file2)
  (and file1 file2
       (let* ((default-directory *pvs-current-directory*)
	      (attr1 (file-attributes* (expand-file-name file1)))
	      (attr2 (file-attributes* (expand-file-name file2))))
	 (equal attr1 attr2))))

(defun file-attributes* (file)
  (let ((attr (file-attributes file)))
    (if (stringp (car attr))
	(file-attributes* (car attr))
	attr)))

(defun string-split (ch string)
  (let ((chars nil)
	(strings nil)
	(pos 0))
    (while (< pos (length string))
      (let ((nch (aref string pos)))
	(if (= nch ch)
	    (let ((nstr (apply (function concat)
			       (mapcar (function char-to-string)
				       (nreverse chars)))))
	      (setq strings (cons nstr strings))
	      (setq chars nil))
	    (setq chars (cons nch chars))))
      (setq pos (1+ pos)))
    (let ((nstr (apply (function concat)
		       (mapcar (function char-to-string)
			       (nreverse chars)))))
      (nreverse (cons nstr strings)))))

(defun short-file-name (file)
  (if (file-exists-p file)
      (let* ((efile (expand-file-name file))
	     (dirnames (string-split ?/ file))
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
			(mapcar '(lambda (x) (concat "/" x)) dirnames)))
	  (setq hname (concat "~" lname)))
	shortname)
      file))

(defun pvs-remove-whitespace (string)
  (let ((str (comint-remove-whitespace string)))
    (if (string-match "^NIL" str)
	(substring str 0 (- (match-beginning 0) 1))
	str)))

(defun new-pvs-file-name (prompt &optional initial exists-noerror)
  ;;(setq *pvs-current-directory* (pvs-current-directory))
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
  (pvs-collect-theories)
  (let ((theoryname (read-from-minibuffer prompt initial)))
    (if (assoc theoryname *pvs-theories*)
	(error "Theory %s already exists." theoryname)
	(if (valid-theory-name theoryname)
	    (list theoryname)
	    (error "%s is not a valid theory name." theoryname)))))

(defun valid-theory-name (name)
  (string-match "^[a-zA-Z][a-zA-Z0-9_]*" name))

(defun pvs-complete-file-name (prompt &optional defdir)
  "Perform completion on file names"
  (ilisp-bury-output)
  (let* ((dir (or defdir (pvs-current-directory)))
	 (default-directory dir))
    (list (read-file-name prompt dir dir t))))

(defun pvs-complete-library-name (prompt &optional distributed-p)
  (ilisp-bury-output)
  (let* ((lfiles (relativize-pvs-filenames
		  (pvs-send-and-wait "(library-files)" nil nil 'list)))
	 (ldirs (delete-duplicates (mapcar 'file-name-directory lfiles)
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
  (ilisp-bury-output)
  (let ((theories
	 (mapcar '(lambda (tf)
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
	       (stringp *pvs-current-directory*)
	       (file-exists-p *pvs-current-directory*))
    (let ((ndir (pvs-send-and-wait "(pvs-current-directory)"
				   nil nil 'string)))
      (while (not (and (stringp ndir)
		       (file-exists-p ndir)))
	(setq ndir (pvs-send-and-wait "(pvs-current-directory)"
				      nil nil 'string)))
      (unless (string-equal ndir "/dev/null")
	(setq *pvs-current-directory* ndir))))
  *pvs-current-directory*)

(defun complete-pvs-file-name (prompt &optional no-default-p dir no-timeout
				      with-prelude-p)
  "Perform completion on PVS file names"
  (ilisp-bury-output)
  (pvs-current-directory)
  (let ((file-list (append (context-files (or dir *pvs-current-directory*))
			   (when with-prelude-p
			     (list (format "prelude" pvs-path))))))
    (if (eq file-list 'NIL)
	(error "No files in context")
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
  (ilisp-bury-output)
  (let ((file-list (context-files dir)))
    (if (eq file-list 'NIL)
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
;;; setting *pvs-current-directory* and *pvs-theories*, to cut down on
;;; the number of calls to Lisp.

(defun complete-theory-name (prompt &optional no-timeout with-prelude-p)
  "Perform completion on PVS theories"
  (ilisp-bury-output)
  (let ((default (or (current-theory)
		     (and with-prelude-p
			  pvs-prelude
			  (buffer-name))))
	(theories (append (pvs-collect-theories)
			  (when with-prelude-p
			    (set-prelude-files-and-regions)
			    (apply 'append
			      (mapcar 'cdr *prelude-files-and-regions*))))))
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
  (ilisp-bury-output)  
  (let ((theories (buffer-theories)))
    (if (null theories)
	(error "No theories available.")
	(let ((theory (completing-read prompt (mapcar 'list theories) nil t)))
	  (if (equal theory "")
	      (error "Must specify a theory name")
	      (list theory))))))

(defun remove-duplicates (list)
  (let ((nlist nil))
    (dolist (e list)
      (unless (member-equal e nlist)
	(push e nlist)))
    (nreverse nlist)))

(defun current-theory ()
  (let ((file (current-pvs-file t)))
    (if file
	(save-excursion (car (current-theory-region)))
	(if (member-equal (pathname-type (buffer-name)) '("ppe" "tccs"))
	    (pathname-name (buffer-name))))))

(defun find-current-theory-region (trs)
  (cond ((null (cdr trs))
	 (car trs))
	((<= (point) (caddr (car trs)))
	 (car trs))
	(t (find-current-theory-region (cdr trs)))))


;;; pvs-collect-theories returns an assoc list of the theory names and
;;; their associated PVS filenames.  The filename is accessed using cadr,
;;; and does not contain the directory or extension.  The primary list
;;; comes from PVS, and reflects the current pvs context (the .pvscontext
;;; file).  This is augmented with the current file, if necessary.  This
;;; is because the current file may not yet have been parsed, so its
;;; current state is unknown in the pvs context, but it is still a valid
;;; choice for many pvs commands.

(defun pvs-collect-theories ()
  (let* ((dir-and-theories (pvs-send-and-wait "(collect-theories)"
					      nil nil 'list))
	 (file (current-pvs-file t))
	 (current-theories (pvs-current-theories)))
;;    (when (not (consp dir-and-theories))
;;      (error "collect-theories did not return a list"))
    (setq *pvs-current-directory* (car dir-and-theories))
    (setq *pvs-theories*
	  (append current-theories
		  (remove-if '(lambda (x)
				(assoc (car x) current-theories))
		    (cdr dir-and-theories))))))

(defun pvs-current-theories ()
  (let* ((cur-file (current-pvs-file t))
	 (cur-theories (when cur-file (buffer-theories))))
    (when cur-theories
      (mapcar '(lambda (th) (list th cur-file)) cur-theories))))

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
	(remove-if 'file-name-directory fnames))))

(defun remove-distributed-pvs-libraries (fnames)
  (remove-if 'in-distributed-pvs-library-p fnames))

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
	    (file-equal dir *pvs-current-directory*))
	file
	(let ((reldir (cdr (assoc dir pvs-relativized-directories))))
	  (if reldir
	      (concat reldir file)
	      (let* ((dchain1 (pvs-directory-chain (short-file-name dir)))
		     (dchain2 (pvs-directory-chain))
		     (mdir (find-if '(lambda (d)
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
  (let ((fdir (short-file-name (or dir *pvs-current-directory*)))
	(dchain nil))
    (while fdir
      (if (file-equal fdir pvs-path)
	  (setq fdir nil)
	  (push fdir dchain)
	  (setq fdir (file-name-directory (substring fdir 0 -1)))))
    (nreverse dchain)))

(defun pvs-region (place)
  (save-excursion
    (goto-line (car place))
    (forward-char (cadr place))
    (let ((beg (point)))
      (goto-line (caddr place))
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
    (buffer-substring start (point))))

(defun find-pvs-name (string)
  (let* ((default (find-name-default))
	 (spec (read-string
		(if default
		    (format "%s(default %s) " string default)
		  string))))
    (list (if (equal spec "")
	      default
	    spec))))

(defun kill-pvs-buffers ()
  (dolist (buf (buffer-list))
    (condition-case ()
	(when (theory-name buf)
	  (kill-buffer buf))
      (error nil))))

(defun member-equal (item list)
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (endp ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))


(defun pvs-abbreviate (cmd abbrevs)
  (let ((abbrs (remove-if 'fboundp
		 (if (listp abbrevs)
		     abbrevs
		     (list abbrevs)))))
    (mapcar '(lambda (a) (fset a cmd)) abbrs)
    (put cmd 'abbreviations abbrs)))

(defun pvs-get-abbreviation (cmd)
  (or (car (get cmd 'abbreviations))
      cmd))

(defun remove-if (pred list)
  (let ((nlist nil))
    (dolist (e list)
      (unless (funcall pred e)
	(push e nlist)))
    (nreverse nlist)))

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
  (mapconcat '(lambda (x) (format "\\.%s$" x))
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
    (princ "nothing to restore")))

;;; restore top of stack without deleting it
(defun setw ()
  (if window-configurations
      (let ((config (car window-configurations)))
	(setq window-configurations (cdr window-configurations))
	(set-window-configuration config)
	(setq window-configurations
	      (cons (current-window-configuration) window-configurations)))
    (princ "nothing to restore"))
  )

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

(setq *demo-font*
  "-adobe-courier-medium-r-normal--24-240-75-75-m-150-iso8859-1")

(defvar *demo-mode* nil)

(defun demo-mode ()
  (interactive)
  (if (eq window-system 'x)
      (let ((nfont (if *demo-mode* (x-get-default "Font") *demo-font*)))
	(x-set-font nfont)
	(setq *demo-mode* (not *demo-mode*)))))

(setq pvs-reserved-words-regexp
  "\\bassuming\\b\\|\\baxiom\\b\\|\\baccept\\b\\|\\bchanges\\b\\|\\ball\\b\\|\\band\\b\\|\\barray\\b\\|\\bbegin\\b\\|\\bby\\b\\|\\bcase\\b\\|\\bdeclare\\b\\|\\bdefinition\\b\\|\\belse\\b\\|\\belsif\\b\\|\\bendif\\b\\|\\bendassuming\\b\\|\\bendcase\\b\\|\\bend\\b\\|\\bexists\\b\\|\\bexporting\\b\\|\\bexit\\b\\|\\bforall\\b\\|\\bfunction\\b\\|\\bformula\\b\\|\\bfrom\\b\\|\\bif\\b\\|\\biff\\b\\|\\bimplies\\b\\|\\bimporting\\b\\|\\bin\\b\\|\\bis\\b\\|\\blambda\\b\\|\\blemma\\b\\|\\bloop\\b\\|\\bmapping\\b\\|\\bmeasure\\b\\|\\bmodule\\b\\|\\bnot\\b\\|\\bnothing\\b\\|\\bof\\b\\|\\bonto\\b\\|\\bobligation\\b\\|\\bopspec\\b\\|\\bor\\b\\|\\bproof\\b\\|\\bprove\\b\\|\\brecursive\\b\\|\\bresult\\b\\|\\btheorem\\b\\|\\btheory\\b\\|\\busing\\b\\|\\bvar\\b\\|\\bvariable\\b\\|\\brecord\\b\\|\\bverify\\b\\|\\bwhere\\b\\|\\bthen\\b\\|\\btype\\b\\|\\bwhen\\b\\|\\bwhile\\b\\|\\bwith\\b\\|\\blet\\b\\|\\bsetvariable\\b\\|\\[#\\|#\\]\\|[(]#\\|#[)]")

(defvar *pvs-reserved-words*
  '("assuming" "axiom" "accept" "changes" "all" "and" "array" "begin" "by"
    "case" "declare" "definition" "else" "elsif" "endif" "endassuming"
    "endcase" "end" "exists" "exporting" "exit" "forall" "function"
    "formula" "from" "if" "iff" "implies" "importing" "in" "is" "lambda"
    "lemma" "loop" "mapping" "measure" "module" "not" "nothing" "of" "onto"
    "obligation" "opspec" "or" "proof" "prove" "recursive" "result"
    "theorem" "theory" "using" "var" "variable" "record" "verify" "where"
    "then" "type" "when" "while" "with" "let" "setvariable"
    "[#" "#]" "(#" "#)"))


(defun highlight-pvs ()
  (interactive)
  (unless (internal-find-face 'pvs-keyword)
    (make-face 'pvs-keyword)
    (set-face-foreground 'pvs-keyword "Blue")
    (set-face-font 'pvs-keyword "*courier-bold-r-normal--12*"))
  ;;(highlight-keywords)
  (save-excursion
    (goto-char (point-min))
    (let* ((case-fold-search t)
	   (found (re-search-forward pvs-reserved-words-regexp nil t)))
      (while found
	(let ((e (make-extent (match-beginning 0) (match-end 0))))
	  (set-extent-face e 'pvs-keyword)
	  (setq found (re-search-forward pvs-reserved-words-regexp nil t)))))))


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
  (save-excursion
    (set-buffer buffer)
    (let ((lmap (current-local-map)))
      (unless lmap
	(setq lmap (make-sparse-keymap))
	(use-local-map lmap))
      (define-key lmap "\M-," 'find-declaration)
      (define-key lmap "\M-;" 'whereis-declaration-used)
      (define-key lmap "\M-:" 'list-declarations))))

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
	(pushnew class classes)))
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


;;; This is taken from dired-aux.el

(defvar pvs-query-keymap nil)
(if pvs-query-keymap ()
    (setq pvs-query-keymap (copy-keymap minibuffer-local-map))
    (define-key pvs-query-keymap "y" 'self-insert-and-exit)
    (define-key pvs-query-keymap " " 'self-insert-and-exit)
    (define-key pvs-query-keymap "n" 'self-insert-and-exit)
    (define-key pvs-query-keymap "\C-?" 'self-insert-and-exit)
    (define-key pvs-query-keymap "!" 'self-insert-and-exit)
    (define-key pvs-query-keymap "q" 'self-insert-and-exit)
    (define-key pvs-query-keymap "n" 'self-insert-and-exit)
    (unless (memq pvs-emacs-system '(xemacs20 xemacs19))
      (define-key pvs-query-keymap "\e" 'self-insert-and-exit))
    ;;(define-key pvs-query-keymap [help-char] 'self-insert-and-exit)
    ;;(define-key pvs-query-keymap "\C-g" 'keyboard-quit)
    )
    

(defvar pvs-query-alist
  '((?\y . y) (?\040 . y)		; `y' or SPC means accept once
    (?n . n) (?\177 . n)		; `n' or DEL skips once
    (?! . yes)				; `!' accepts rest
    (?q. no) (?\e . no)			; `q' or ESC skips rest
    ;; None of these keys quit - use C-g for that.
    ))

(defun pvs-emacs-query (qs-var qs-prompt &rest qs-args)
  ;; Query user and return nil or t.
  ;; Store answer in symbol VAR (which must initially be bound to nil).
  ;; Format PROMPT with ARGS.
  ;; Binding variable help-form will help the user who types the help key.
  (let* ((char (symbol-value qs-var))
	 (action (cdr (assoc char pvs-query-alist))))
    (cond ((eq 'yes action)
	   t)				; accept, and don't ask again
	  ((eq 'no action)
	   nil)				; skip, and don't ask again
	  (t;; no lasting effects from last time we asked - ask now
	   (let ((qprompt (concat qs-prompt
				  (if help-form
				      (format " [Type yn!q or %s] "
					      (key-description
					       (char-to-string help-char)))
				    " [Type y, n, q or !] ")))
		 (minibuffer-help-form help-form)
		 result elt)
	     ;;(apply 'message qprompt qs-args)
	     ;;(setq char (set qs-var (read-char)))
	     (setq char
		   (set qs-var
			(string-to-char
			 (read-from-minibuffer (format qprompt qs-args) nil
					       pvs-query-keymap))))
	     (while (not (setq elt (assoc char pvs-query-alist)))
	       (message "Invalid char - type %c for help." help-char)
	       (ding)
	       (sit-for 1)
	       ;;(apply 'message qprompt qs-args)
	       ;;(setq char (set qs-var (read-char)))
	       (setq char
		     (set qs-var
			  (string-to-char
			   (read-from-minibuffer (format qprompt qs-args) nil
						 pvs-query-keymap)))))
	     (memq (cdr elt) '(t y yes)))))))

(defun directory-writable-p (dirname)
  (let* ((edir (expand-file-name dirname))
	 (dirnames (reverse (string-split ?/ edir)))
	 (dname edir))
    (while dirnames
      (if (file-exists-p dname)
	  (setq dirnames nil)
	  (setq dname (apply 'concat
			(mapcar '(lambda (x) (concat "/" x))
			  (reverse dirnames))))
	  (setq dirnames (cdr dirnames))))
    (and (stringp dname)
	 (file-exists-p dname)
	 (file-directory-p dname)
	 (file-writable-p dname))))

(defadvice rename-buffer (after rename-buffer-pvs activate)
  (setq current-pvs-file 'unbound))

(defun real-current-column ()
  (- (point) (save-excursion (beginning-of-line) (point))))


;;; The following doesn't work - after invoking the real debugger (with
;;; ad-do-it), the code following isn't executed until the debugger is
;;; exited, at which point the *Backtrace* buffer is gone.

;(defadvice debug (around noninteractive-debug activate)
;  ad-do-it
;  (save-excursion
;    (set-buffer "*Backtrace*")
;    (print (buffer-string))))

;(defadvice yes-or-no-p (around noninteractive activate)
;  (if noninteractive
;      (setq ad-return-value t)
;      ad-do-it))
;
;(defadvice y-or-n-p (around noninteractive activate)
;  (if noninteractive
;      (setq ad-return-value t)
;      ad-do-it))

;;; Not all is lost, however.  To gain information when the debugger is
;;; entered while in batch mode, type the following right after the
;;; Entering debugger message:
;;;  e (progn (set-buffer "*Backtrace*") (buffer-string))

(autoload 'vc-checkout-writable-buffer "vc" "" t)
(autoload 'vc-admin "vc" "" t)

(defmacro pvs-validate (file directory &rest body)
  (` (let* ((logfile (concat default-directory (, file)))
	    (rcs-file-exists (vc-name logfile)))
       (pvs-ensure-rcs-directory logfile)
       (when (file-exists-p logfile)
	 (delete-file logfile))
       (unless rcs-file-exists
	 (save-excursion
	   (set-buffer (find-file-noselect logfile t))
	   (vc-register t nil)
	   ;;(vc-admin logfile nil)
	   (kill-buffer nil)))
       (when (file-exists-p logfile)
	 (delete-file logfile))
       (vc-checkout-writable-buffer logfile)
       (let ((logbuf (find-file-noselect logfile t)))
	 (save-excursion
	   (set-buffer logbuf)
	   (when buffer-read-only (toggle-read-only))
	   (clear-buffer logbuf)
	   (let ((standard-output logbuf)
		 (pvs-validating t)
		 (default-directory default-directory))
	     (pvs-message (pvs-version-string))
	     (let ((pvs-disable-messages nil))
	       (change-context (, directory)))
	     (pvs-send-and-wait "(setq *pvs-verbose* t)" nil nil 'dont-care)
	     (,@ body))
	   (pvs-wait-for-it)
	   (pvs-send-and-wait "(setq *pvs-verbose* nil)" nil nil 'dont-care)
	   ;;(save-buffer 0) ;writes a message-use the following 3 lines
	   (write-region (point-min) (point-max) (buffer-file-name) nil 'nomsg)
	   (set-buffer-modified-p nil)
	   (clear-visited-file-modtime)
	   (if rcs-file-exists
	       (let* ((version (vc-latest-version buffer-file-name))
		      (filename (concat buffer-file-name ".~" version "~")))
		 (unless (file-exists-p filename)
		   (vc-backend-checkout logfile nil version filename))
		 (let ((prevlog (find-file-noselect filename)))
		   (cond ((pvs-same-validation-buffers-p logbuf prevlog)
			  (delete-file filename)
			  (pvs-message "No significant changes in %s" (, file)))
			 (t
			  (pvs-message "Differences found since last run:")
			  (pvs-message "  Compare %s to %s" (, file)
				       (file-name-nondirectory filename))))))
	       (pvs-message "Nothing to compare %s to" (, file)))
	   (vc-checkin logfile nil
		       (format "Validation run for %s:\n%s"
			   (current-time-string)
			 (quote (, body)))))))))

(defun pvs-ensure-rcs-directory (file)
  (let ((rcsdir (concat (file-name-directory file) "RCS")))
    (unless (file-exists-p rcsdir)
      (pvs-message "Creating RCS directory: %s" rcsdir)
      (condition-case nil
	  (make-directory rcsdir)
	(error (pvs-message "Could not create RCS directory"))))))

(defvar pvs-validation-regexp
  "[ \t\n]\\|^PVS Version.*$\\|[0-9]+\\(\.[0-9]+\\)? ?s\\(ec\\|,\\| \\|:\\)"
  "Regexp used in pvs-compare-windows-whitespace.  Because of the way
pvs-compare-windows-skip-whitespace is defined, it must either match at
the point where the buffers would otherwise differ, or match at the
beginning of the line.  The default is to consider patch levels and time
differences to be whitespace")

(defun pvs-same-validation-buffers-p (log1 log2)
  (not (pvs-find-validation-buffers-mismatch log1 log2)))

;;; Derived from compare-windows-skip-whitespace in
;;; <gnu-emacs>/lisp/compare-w.el.  In addition to the default behavior,
;;; it checks whether there is a match at the beginning of the current
;;; line.
(defun pvs-compare-windows-skip-whitespace (start)
  (let ((end (point))
	(beg (point))
	(opoint (point)))
    (while (or (and (looking-at pvs-validation-regexp)
		    (<= end (match-end 0))
		    ;; This match goes past END, so advance END.
		    (progn (setq end (match-end 0))
			   (> (point) start)))
	       (and (/= (point) start)
		    ;; Consider at least the char before point,
		    ;; unless it is also before START.
		    (= (point) opoint)))
      ;; keep going back until whitespace
      ;; doesn't extend to or past end
      (forward-char -1))
    (setq beg (point))
    (beginning-of-line)
    (if (and (looking-at pvs-validation-regexp)
	     (<= end (match-end 0)))
	(setq end (match-end 0)))
    (goto-char end)
    (or (/= beg opoint)
	(/= end opoint))))

(defun pvs-compare-validation-windows ()
  (interactive)
  (let ((compare-windows-whitespace 'pvs-compare-windows-skip-whitespace))
    (compare-windows t)))

(defun pvs-find-validation-buffers-mismatch (log1 log2 &optional spos1 spos2)
  (let* ((pos1 (or spos1 1))
	 (pos2 (or spos2 1))
	 (end1 (save-excursion (set-buffer log1) (point-max)))
	 (end2 (save-excursion (set-buffer log2) (point-max)))
	 (dpos (compare-buffer-substrings log1 pos1 end1 log2 pos2 end2))
	 (match t))
    (while (and (/= dpos 0) match)
      (if (and (save-excursion
		 (set-buffer log1)
		 (goto-char (+ (abs dpos) pos1 -1))
		 (when (looking-at pvs-validation-regexp)
		   (setq pos1 (match-end 0))))
	       (save-excursion
		 (set-buffer log2)
		 (goto-char (+ (abs dpos) pos2 -1))
		 (when (looking-at pvs-validation-regexp)
		   (setq pos2 (match-end 0)))))
	  (setq dpos
		(compare-buffer-substrings log1 pos1 end1 log2 pos2 end2))
	  (setq match nil)))
    (unless match
      (list (+ (abs dpos) pos1 -1) (+ (abs dpos) pos2 -1)))))

(defvar pvs-waiting nil)

(defun pvs-wait-for-it ()
  (setq pvs-waiting t)
  (pvs-send "(pvs-emacs-eval \"(setq pvs-waiting nil)\")")
  (while pvs-waiting
    (accept-process-output)
    (sit-for 0)))

(defun pvs-message (control-string &rest data)
  (princ (apply 'format control-string data))
  (terpri)
  (princ (apply 'format control-string data) 'external-debugging-output)
  (terpri 'external-debugging-output))

(when noninteractive
(defadvice message (around pvs-batch-control activate)
    (unless (and noninteractive (= pvs-verbose 0)) ad-do-it))

(defadvice yes-or-no-p (around pvs-batch-control activate)
  (when noninteractive
    (let ((pvs-verbose 1))
      (message (ad-get-arg 0))))
  ad-do-it)
)

(defun list-lisp-declarations (filename)
  (interactive "fList lisp declarations for file: ")
  (pvs-send-and-wait
   (format "(list-lisp-entities-of-file \"%s\" \"%s entities\")"
       filename (pathname-name filename))))

(defun list-lisp-duplicates (filename1 filename2)
  (interactive "fFile1: \nfFile2: ")
  (pvs-send-and-wait
   (format "(print-duplicate-lisp-entities \"%s\" \"%s\" \"%s/%s duplicates\")"
       filename1 filename2
       (pathname-name filename1) (pathname-name filename2))))

(defun trailing-components (directory num)
  (let ((comps (nreverse (string-split ?/ directory)))
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

(defun pvs-title-string ()
  nil)

(case pvs-emacs-system
  ((emacs19 emacs20)
   (defun pvs-update-window-titles ()
     (let ((title (pvs-title-string)))
       (when title
	 (modify-frame-parameters (car (frame-list))
				  (list (cons 'icon-name title)
					(cons 'title title)))))))
  ((xemacs19 xemacs20)
   (defun pvs-update-window-titles ()
     (let ((title (pvs-title-string)))
       (when title
	 (setq frame-title-format title)
	 (setq frame-icon-title-format title)))))
  (t (defun pvs-update-window-titles ()
       nil)))

(add-hook 'change-context-hook 'pvs-update-window-titles)
