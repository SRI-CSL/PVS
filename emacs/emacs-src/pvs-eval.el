; PVS evaluator - emacs end.  dave_sc 10/12/98

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
      (save-excursion
	(set-buffer buf)
	(setq pvs-context-sensitive t)
	(lisp-mode)))))

(defun pvs-find-lisp-file (theoryname)
  (let ((buf (get-buffer (format "%s.lisp" theoryname))))
    (when buf
      (kill-buffer buf)))
  (let ((lisp-file (format "%s%s.lisp" *pvs-current-directory* theoryname)))
    (when (file-exists-p lisp-file)
      (find-file-read-only-other-window lisp-file))))


(defpvs pvs-ground-evaluator prove (theory)
  "Invokes the ground evaluator in the context of the given PVS theory"
  (interactive (complete-theory-name "Use context of theory: "))
  (confirm-not-in-checker)
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





