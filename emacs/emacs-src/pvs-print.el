;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-print.el -- 
;; Author          : Sam Owre
;; Created On      : Fri Mar 18 01:57:06 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Oct 15 01:44:44 1995
;; Update Count    : 3
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; pvs-print-buffer
;;; pvs-print-region
;;; print-theory
;;; print-pvs-file
;;; print-importchain
;;; alltt-theory
;;; alltt-pvs-file
;;; alltt-importchain
;;; alltt-proof
;;; latex-theory
;;; latex-pvs-file
;;; latex-importchain
;;; latex-theory-view
;;; latex-proof
;;; latex-proof-view
;;; latex-set-linelength

(eval-when-compile (require 'pvs-macros))

;;; The first few functions were extracted from the GNU distribution file
;;; lpr.el, because we needed more control over the switches.  (The
;;; default always adds the -T switch, which only makes sense to lpr, so
;;; other print commands will fail).

(defvar pvs-print-command "lpr"
  "Specifies the command to use for printing.
See the pvs-print-buffer command for details.")
(defvar pvs-print-switches nil
  "Specifies a list of strings to use as switches to the print command.
See the pvs-print-buffer command for details.")
(defvar pvs-print-title-switches '("-J" "-T")
  "Specifies the switches used for producing titles in the pvs-print-command.
See the pvs-print-buffer command for details.")
(defvar pvs-print-name nil "The name to use in headers.")

(defpvs pvs-print-buffer print-buffer ()
  "Print buffer contents as with Unix command `lpr'

The pvs-print-buffer command prints out the current buffers contents.  Its
behavior depends on the following variables:
  `pvs-print-command' is a variable which specifies the shell command to
                      use for printing (default \"lpr\")
  `pvs-print-switches' is a list of extra switches (strings) to pass to lpr.
  `pvs-print-title-switches' is the switch which specifies the title to use
                  for the print command.  If nil, then no title is produced.

For example, to use enscript to print in gaudy mode producing two column
rotated output, add the following to your ~/.emacs file:
    (setq pvs-print-command \"enscript\")
    (setq pvs-print-switches '(\"-G\" \"-2\" \"-r\"))
    (setq pvs-print-title-switches '(\"-b\" \"-J\"))"
  (interactive)
  (pvs-print-region-1 (point-min) (point-max) pvs-print-switches))

(defpvs pvs-print-region print-buffer (start end)
  "Print region contents as with Unix command `lpr'

The pvs-print-buffer command prints out the current region.  Its behavior
depends on the following variables:
  `pvs-print-command' is a variable which specifies the shell command to
                      use for printing (default \"lpr\")
  `pvs-print-switches' is a list of extra switches (strings) to pass to lpr.
  `pvs-print-title-switches' is the switch which specifies the title to use
                  for the print command.  If nil, then no title is produced.

For example, to use enscript to print in gaudy mode producing two column
rotated output, add the following to your ~/.emacs file:
    (setq pvs-print-command \"enscript\")
    (setq pvs-print-switches '(\"-G\" \"-2\" \"-r\"))
    (setq pvs-print-title-switches '(\"-b\" \"-J\"))"
  (interactive "r")
  (pvs-print-region-1 start end pvs-print-switches))

(defun pvs-print-region-1 (start end switches)
  (let ((name (or pvs-print-name (concat (buffer-name) " Emacs buffer")))
	(width tab-width))
    (save-excursion
     (message "Spooling...")
     (if (/= tab-width 8)
	 (let ((oldbuf (current-buffer)))
	  (set-buffer (get-buffer-create " *spool temp*"))
	  (widen) (erase-buffer)
	  (insert-buffer-substring oldbuf start end)
	  (setq tab-width width)
	  (untabify (point-min) (point-max))
	  (setq start (point-min) end (point-max))))
     (apply 'call-process-region
	    (nconc (list start end pvs-print-command
			 nil (get-buffer-create "pvs-print") nil)
		   (nconc (apply 'nconc
				 (mapcar '(lambda (ts)
					   (list ts name))
					 pvs-print-title-switches))
			  switches)))
     (message "Spooling...done"))))

;;;---------------------------------------------
;;; Printing Commands

;;; Print Theory

(defpvs print-theory print-file (theoryname)
  "Print theory using pvs-print-region command

The print-theory command prints the specified theory using the
pvs-print-region command."
  (interactive (complete-theory-name "Print theory named: " nil t))
  (unless (interactive-p) (pvs-collect-theories))
  (let* ((filename (cadr (assoc theoryname *pvs-theories*))))
    (cond (filename
	   (save-excursion
	     (set-buffer (get-theory-buffer theoryname))
	     (let ((pvs-print-name (format "Theory %s" theoryname)))
	       (apply 'pvs-print-region (cdr (theory-region theoryname))))))
	  ((print-prelude-theory theoryname))
	  (message "No theories found."))))

(defun print-prelude-theory (theoryname)
  (let* ((freg (get-prelude-file-and-region theoryname))
	 (fname (car freg)))
    (when (file-exists-p fname)
      (let* ((buf (find-file-noselect fname))
	     (region (cdr freg)))
	(when region
	  (let ((pbuf (get-buffer-create theoryname)))
	    (save-excursion
	      (set-buffer pbuf)
	      (setq pvs-prelude (save-excursion
				  (set-buffer buf)
				  (goto-char (car region))
				  (- (current-line-number) 1)))
	      (when buffer-read-only (toggle-read-only))
	      (erase-buffer)
	      (insert-buffer-substring buf (car region) (cadr region))
	      (unless buffer-read-only (toggle-read-only))
	      (pvs-print-buffer))))))))


;;; Print PVS File

(defpvs print-pvs-file print-file (filename)
  "Print file using pvs-print-buffer command

The print-pvs-file command prints the specified PVS file using the
pvs-print-buffer command."
  (interactive (complete-pvs-file-name "Print PVS file named: "
				       nil nil t t))
  (let ((buf (get-pvs-file-buffer filename)))
    (if buf
	(save-excursion
	  (set-buffer buf)
	  (save-pvs-file filename)
	  (let ((pvs-print-name (format "PVS File %s" filename)))
	    (pvs-print-buffer)))
	(when (equal filename "prelude")
	  (let ((fname (format "%s/lib/prelude.pvs" pvs-path)))
	    (if (not (file-exists-p fname))
		(error "%s does not exist." fname)
		(save-excursion
		  (set-buffer (find-file-noselect fname))
		  (setq pvs-prelude 0)
		  (unless buffer-read-only (toggle-read-only))
		  (pvs-mode)
		  (pvs-print-buffer))))))))


;;; Print IMPORT Chain

(defpvs print-importchain print-file (theoryname)
  "Print theories in IMPORT chain using pvs-print-buffer command

The print-pvs-importchain command prints the importchain of the specified
theory using the pvs-print-buffer command."
  (interactive (complete-theory-name "Print theories in imports of: "))
  (unless (interactive-p) (pvs-collect-theories))
  (let ((print-buffer (get-buffer-create " *pvs-temp-buffer*"))
	(theories
	 (pvs-send-and-wait
	  (format "(context-usingchain \"%s\")" theoryname) nil nil 'list))
	(first t))
    (clear-buffer print-buffer)
    (save-excursion
      (dolist (th theories)
	(set-buffer (get-theory-buffer th))
	(let ((string (apply 'buffer-substring
			     (cdr (theory-region th)))))
	  (set-buffer print-buffer)
	  (insert (if first "" "\f") string)
	  (setq first nil)))
      (let ((pvs-print-name (format "Import Chain for Theory %s" theoryname)))
	(set-buffer print-buffer)
	(pvs-print-buffer)))))



;;;---------------------------------------------
;;; Alltt Commands

;;; Alltt Theory

(defpvs alltt-theory alltt (theoryname)
  "Generate LaTeX file in alltt format for the current theory

The alltt-theory command generates a LaTeX file in alltt format for the
specified theory.  If the theory name is foo, the name of the generated
file is foo-alltt.tex."
  (interactive (complete-theory-name "Generate alltt file for theory named: "))
  (unless (interactive-p) (pvs-collect-theories))
  (let* ((buf (get-theory-buffer theoryname))
	 (reg (save-excursion (set-buffer buf) (theory-region theoryname)))
	 (file (format "%s%s-alltt.tex" *pvs-current-directory* theoryname))
	 (nbuf (find-file-noselect file)))
    (save-excursion
      (set-buffer nbuf)
      (erase-buffer)
      (message "Generating %s..." file)
      (insert-buffer-substring buf (cadr reg) (min (caddr reg)
						   (save-excursion
						     (set-buffer buf)
						     (point-max))))
      (goto-char (point-min))
      (replace-regexp "{\\|}" "\\\\\\&")
      (goto-char (point-min))
      (replace-string "^" "\\verb|^|")
      (goto-char (point-min))
      (replace-string "\\/" "\\verb|\\/|")
      (goto-char (point-min))
      (replace-string "/\\" "\\verb|/\\|")
      (untabify (point-min) (point-max))
      (goto-char (point-max))
      (delete-blank-lines)
      (delete-blank-lines)
      (save-buffer)
      (message "Finished generating %s" file))))


;;; Alltt PVS File

(defpvs alltt-pvs-file alltt (filename)
  "Generate LaTeX files in alltt format for theories in specified PVS file

The alltt-theory command generates a LaTeX file in alltt format for each
theory of the specified PVS file.  If a theory name is foo, the name of
the corresponding generated file is foo-alltt.tex."
  (interactive (complete-pvs-file-name
		"Generate alltt file for PVS file named: "))
  (save-excursion
    (set-buffer (get-pvs-file-buffer filename))
    (let ((tregs (theory-regions)))
      (setq *pvs-theories*
	    (mapcar '(lambda (treg)
			(list (car treg) filename))
		    tregs))
      (dolist (treg tregs)
	(alltt-theory (car treg)))))
  (message "Finished generating alltt files"))


;;; Alltt prelude

(defun alltt-prelude ()
  "Generate LaTeX files in alltt format for the PVS prelude."
  ;;(interactive)
  (unless *prelude-files-and-regions*
    (complete-prelude-name))
  (dolist (freg *prelude-files-and-regions*)
    (let ((buf (find-file-noselect (car freg))))
      (dolist (treg (cdr freg))
	(let* ((file (format "%s%s-alltt.tex" *pvs-current-directory*
		       (car treg)))
	       (nbuf (find-file-noselect file)))
	  (save-excursion
	    (set-buffer nbuf)
	    (erase-buffer)
	    (message "Generating %s..." file)
	    (apply 'insert-buffer-substring buf (cdr treg))
	    (goto-char (point-min))
	    (replace-regexp "{\\|}" "\\\\\\&")
	    (untabify (point-min) (point-max))
	    (goto-char (point-max))
	    (delete-blank-lines)
	    (delete-blank-lines)
	    (save-buffer)
	    (message "Finished generating %s" file))))))
  (message "Finished generating alltt files"))


;;; Alltt IMPORT Chain

(defpvs alltt-importchain alltt (theoryname)
  "Generate LaTeX files in alltt format for theories in import chain

The alltt-importchain command generates a LaTeX file in alltt format for
each theory in the importchain of the specified theory.  If a theory name
is foo, the name of the corresponding generated file is foo-alltt.tex."
  (interactive (complete-theory-name
		"Generate alltt file for IMPORT chain of theory named: "))
  (unless (interactive-p) (pvs-collect-theories))
  (let ((theories (pvs-send-and-wait
		   (format "(context-usingchain \"%s\")" theoryname)
		   nil nil 'list)))
    (dolist (th theories)
      (alltt-theory th)))
  (message "Finished generating alltt files"))


;;; Alltt proof

(defpvs alltt-proof alltt (filename)
  "Generate an alltt LaTeX file for the last proof

The alltt-proof command generates an alltt LaTeX file summarizing the last
proof processed during the current session.  Given an argument (M-0 or
C-u), will provide a brief output, hiding many of the details."
  (interactive (let ((fname (pvs-send-and-wait
			     "(when *last-proof* (label *last-proof*))"
			     nil nil 'string)))
		 (unless fname
		   (error "Must run a proof first"))
		 (let ((spec (read-string
			      (format
				  "%s alltt of last proof into file (default %s-alltt.tex): "
				  (if current-prefix-arg "Terse" "Verbose")
				fname))))
		   (list (if (equal spec "")
			     (format "%s-alltt" fname)
			     spec)))))
  (let* ((default-directory *pvs-current-directory*)
	 (texfile (if (equal (pathname-type filename) "tex")
		     filename
		     (concat filename ".tex"))))
    (when (or (not (file-exists-p texfile))
	      (y-or-n-p "File already exists - overwrite? "))
      (message "Generating...")
      (pvs-send-and-wait (format "(alltt-proof \"%s\" %s)"
			     texfile (not (not current-prefix-arg)))
			 nil nil 'dont-care)
      (if (not (file-exists-p texfile))
	  (error "Alltt file %s not generated" texfile)
	  (let ((buf (find-file-noselect texfile)))
	    (save-excursion
	      (set-buffer buf)
	      (goto-char (point-min))
	      (while (search-forward "{" nil t)
		(replace-match "\\{" nil t))
	      (goto-char (point-min))
	      (while (search-forward "}" nil t)
		(replace-match "\\}" nil t))
	      (save-buffer)))
	  (message "Generated alltt proof file %s" texfile)))))


;;; LaTeX Theory

(defpvs latex-theory latex (theoryname)
  "Generates a LaTeX file for the specified theory

The latex-theory command generates a LaTeX file for the specified theory.
The generated file is named THEORYNAME.tex.  In addition, generates the 
pvs-files.tex file in the current directory.  This file may be LaTeXed and
printed or viewed, or it may serve as an example for including
THEORYNAME.tex in a document."
  (interactive (complete-theory-name "Generate LaTeX for theory named: "))
  (unless (interactive-p) (pvs-collect-theories))
  (let ((file (when (member-equal theoryname (buffer-theories))
		(current-pvs-file))))
    (pvs-send (format "(latex-theory \"%s\" %s)"
		  theoryname (when file (format "\"%s\"" file)))
	      nil 'ltt)))


;;; LaTeX PVS File

(defpvs latex-pvs-file latex (file)
  "Generates a LaTeX file for each theory in the specified file.

The latex-pvs-file command generates a LaTeX file for each theory in the
specified PVS file.  Each theory in the PVS file generates a <theory>.tex
file.  Automatically generates pvs-files.tex in the current directory.
This file may be LaTeXed and printed or viewed, or it may serve as an
example for including <theory>.tex in a document."
  (interactive (complete-pvs-file-name "Generate LaTeX for PVS file named: "))
  (pvs-send (format "(latex-pvs-file \"%s\")" file) nil 'ltf))


;;; LaTeX IMPORT Chain

(defpvs latex-importchain latex (theoryname)
  "LaTeX-prints theories in closure of IMPORT chain to files

The latex-pvs-file command generates a LaTeX file for each theory in the
specified PVS file.  Each theory in the PVS file generates a <theory>.tex
file.  Automatically generates pvs-files.tex in the current directory.
This file may be LaTeXed and printed or viewed, or it may serve as an
example for including <theory>.tex in a document."
  (interactive (complete-theory-name "Name of root theory: "))
  (unless (interactive-p) (pvs-collect-theories))
  (let ((file (when (member-equal theoryname (buffer-theories))
		(current-pvs-file))))
    (pvs-send (format "(latex-usingchain \"%s\" %s)"
		  theoryname (when file (format "\"%s\"" file)))
	      "LaTeX-printing..." 'lti)))


;;; LaTeX Print View

(defvar pvs-latex-viewer nil
  "Viewer for LaTeX.")

(defpvs latex-theory-view latex (theoryname)
  "LaTeX and view the PVS theory in the current buffer

The latex-theory-view command generates the LaTeX file for the specified
theory, LaTeXs it, and previews it using the viewer given by the Emacs
variable pvs-latex-viewer.  If this is not set, you will be prompted for a
viewer."
  (interactive (complete-theory-name "LaTeX and view theory named: "))
  (unless (interactive-p) (pvs-collect-theories))
  (unless (and pvs-latex-viewer
	       (or (and (pathname-directory pvs-latex-viewer)
			(file-exists-p pvs-latex-viewer))
		   (pvs-find-file-in-exec-path pvs-latex-viewer)))
    (setq pvs-latex-viewer (pvs-get-latex-viewer-name)))
  (if pvs-latex-viewer
      (let ((file (when (member-equal theoryname (buffer-theories))
		    (current-pvs-file))))
	(pvs-send (format "(latex-theory-view \"%s\" %s \"%s\")"
		      theoryname (when file (format "\"%s\"" file))
		      pvs-latex-viewer)
		  nil 'ltv))
      (error "Must first M-x set-variable `pvs-latex-viewer'")))

(defpvs latex-proof-view latex (filename)
  "LaTeX and view the last proof

The latex-proof-view command generates a LaTeX file summarizing the steps
of the last proof that was processed, then LaTeXs it and previews it using
the viewer given by pvs-latex-viewer.  If this is not set, you will be
prompted for a viewer.  Given an argument (M-0 or C-u), the output is more
brief, hiding many of the details."
  (interactive (let ((fname (pvs-send-and-wait
			     "(when *last-proof* (label *last-proof*))"
			     nil nil 'string)))
		 (unless fname
		   (error "Must run a proof first"))
		 (let ((spec (read-string
			      (format
				  "%s LaTeX of last proof into file (default %s.tex): "
				  (if current-prefix-arg "Terse" "Verbose")
				fname))))
		   (list (if (equal spec "") fname spec)))))
  (unless (and pvs-latex-viewer
	       (or (and (pathname-directory pvs-latex-viewer)
			(file-exists-p pvs-latex-viewer))
		   (pvs-find-file-in-exec-path pvs-latex-viewer)))
    (setq pvs-latex-viewer (pvs-get-latex-viewer-name)))
  (if pvs-latex-viewer
      (let ((texfile (if (equal (pathname-type filename) "tex")
			 filename
			 (concat filename ".tex"))))
	(pvs-send (format "(latex-proof-view \"%s\" \"%s\" %s)"
		      texfile pvs-latex-viewer (not (not current-prefix-arg)))
		  nil 'lpv))
      (error "Must first M-x set-variable `pvs-latex-viewer'")))

(defun pvs-get-latex-viewer-name ()
  (let ((viewer (read-string "What viewer should be used? " "xdvi")))
    (if (or (and (pathname-directory viewer)
		 (file-exists-p viewer))
	    (pvs-find-file-in-exec-path viewer))
	viewer
	(message "%s cannot be found - check your PATH" viewer)
	(sit-for 2)
	(pvs-get-latex-viewer-name))))


;;; LaTeX proof

(defpvs latex-proof latex (filename)
  "Generate a LaTeX file for the last proof

The latex-proof command generates a LaTeX file summarizing the steps of
the last proof that was processed.  Given an argument (M-0 or C-u), will
provide a brief output, hiding many of the details."
  (interactive (let ((fname (pvs-send-and-wait
			     "(when *last-proof* (label *last-proof*))"
			     nil nil 'string)))
		 (unless fname
		   (error "Must run a proof first"))
		 (let ((spec (read-string
			      (format
				  "%s LaTeX of last proof into file (default %s.tex): "
				  (if current-prefix-arg "Terse" "Verbose")
				fname))))
		   (list (if (equal spec "") fname spec)))))
  (let ((texfile (if (equal (pathname-type filename) "tex")
		     filename
		     (concat filename ".tex"))))
    (pvs-send-and-wait (format "(latex-proof \"%s\" %s)"
			   texfile (not (not current-prefix-arg)))
		       nil nil 'dont-care)))

(defpvs latex-set-linelength latex (length)
  "Set the linelength for LaTeX text

The latex-set-linelength command sets the linelength to be used for generating LaTeX files.  The right setting depends on the page size, margins, and fonts used, so cannot be determined by PVS.  The default is 100."
  (interactive "nSet Length to: ")
  (pvs-send-and-wait (format "(setq *latex-linelength* %d)" length)
		     nil nil 'dont-care)
  (message "LaTeX linelength set to %d" length))

(defun pvs-find-file-in-exec-path (prog)
  ;; Search exec-path, a list of directory names, for prog.
  ;; Returns the element of exec-path that contains prog, or nil if not found.
  (let ((path exec-path))
    (while (and path
		(not (file-exists-p (expand-file-name prog (car path)))))
      (setq path (cdr path)))
    (car path)))


(defpvs html-pvs-file html (filename)
  "Generate HTML for a PVS file, with names pointing to their declarations.

Generates an HTML web corresponding to the filename, which must already be typechecked.  A pvshtml subdirectory of the current context is created if necessary, and the pvshtml/filename.html file is created.  If the file is already there, and newer than the source file, it is not regenerated.

NOTE: There is no corresponding html-theory, as links to the given theory would become ambiguous."
  (interactive (complete-pvs-file-name "Generate HTML for PVS file named: "))
  (pvs-send (format "(html-pvs-file \"%s\" %s nil)"
		filename (and current-prefix-arg t))
	    nil))

(defpvs html-pvs-files html (filename)
  "Generate HTML for a PVS file, with names pointing to their declarations.

Generates an HTML web corresponding to the filename, which must already be typechecked.  A pvshtml subdirectory of the current context is created if necessary, and the pvshtml/filename.html file is created.  If the file is already there, and newer than the source file, it is not regenerated."
  (interactive (complete-pvs-file-name "Generate HTML files rooted at PVS file: "))
  (pvs-send (format "(html-pvs-file \"%s\" %s t)"
		filename (and current-prefix-arg t))
	    nil))
