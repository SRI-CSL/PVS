;; pvs-lib.el
;; Release: ProofLite-4.2 (01/10/10)

(defpvs install-prooflite-scripts-theory edit-proof ()
  "Install ProofLite scripts of non-proved formulas in theory

Installs ProofLite scripts in the current theory as default proofs except
for formulas that are already proved."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let* ((theory (current-theory))
	 (file   (current-pvs-file))
	 (*pvs-error* nil))
    (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			   file)
		       nil 'tc 'dont-care)
    (unless *pvs-error*
      (pvs-send-and-wait 
       (format "(install-prooflite-scripts \"%s\" \"%s\" 0 nil)"
	   file theory)))))

(defpvs install-prooflite-script edit-proof ()
  "Install ProofLite script at the cursor position of non-proved formula

Installs the ProofLite script at the current cursor position unless 
the formula is already proved."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let* ((theory (current-theory))
	 (file (current-pvs-file))
	 (line (current-line-number))
	 (*pvs-error* nil))
    (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			   file)
		       nil 'tc 'dont-care)
    (unless *pvs-error*
      (pvs-send-and-wait 
       (format "(install-prooflite-scripts \"%s\" \"%s\" %s nil)"
	   file theory line)))))

(defpvs install-prooflite-scripts-theory! edit-proof ()
  "Install ProofLite scripts of formulas in theory

Installs ProofLite scripts of all formulas in the current theory as 
default proofs even if formulas are already proved."

  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let* ((theory (current-theory))
	 (file   (current-pvs-file))
 	 (*pvs-error* nil))
    (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			   file)
		       nil 'tc 'dont-care)
    (unless *pvs-error*
      (pvs-send-and-wait 
       (format "(install-prooflite-scripts \"%s\" \"%s\" 0 t)"
	   file theory)))))

(defpvs install-prooflite-script! edit-proof ()
  "Install ProofLite script at the cursor position 

Installs the ProofLite script at the current cursor position even if
the formula is already proved."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let* ((theory (current-theory))
	 (file (current-pvs-file))
	 (line (current-line-number))
 	 (*pvs-error* nil))
    (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			   file)
		       nil 'tc 'dont-care)
    (unless *pvs-error*
      (pvs-send-and-wait 
       (format "(install-prooflite-scripts \"%s\" \"%s\" %s t)"
	   file theory line)))))
  
(defun prooflite-buffer (&optional mssg)
  (let ((buffer (get-buffer "ProofLite")))
    (save-excursion
      (set-buffer buffer)
      (goto-char (point-min))
      (when mssg 
	(insert mssg)
	(newline 2))
      (while (< (point) (point-max))
	(insert "%|- ")
	(forward-line)))))

(defpvs insert-prooflite-script edit-proof ()
  "Insert ProofLite script into the current theory

Inserts the ProofLite script of the default proof of the formula closest
to (moving forward) the current cursor position into the working theory."
  (interactive)
  (confirm-not-in-checker)
  (pvs-bury-output)
  (let* ((fref (pvs-formula-origin))
	 (kind (pvs-fref-kind fref))
	 (file (pvs-fref-file fref))
	 (line (pvs-fref-line fref))
	 (*pvs-error* nil))
    (when (eq kind 'pvs)
      (save-some-pvs-buffers)
      (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			     file)
			 nil 'tc 'dont-care)
      (unless *pvs-error*
	(when (get-buffer "ProofLite")
	  (kill-buffer "ProofLite"))
	(set-proof-script-font-lock-keywords)
	(let ((line (pvs-send-and-wait 
		     (format 
			 "(proof-to-prooflite-script \"%s\" %d)" file line)
		     nil nil 'list)))
	  (when line
	    (prooflite-buffer)
	    (goto-line (+ (car line) 1))
	    (newline)
	    (insert-buffer "ProofLite")))))))

(defun complete-formula-name ()
  (let* ((formulas (pvs-send-and-wait 
		    (format "(all-decl-names \"%s\")" (current-theory))
				      nil nil 'list))
	 (formula  (completing-read "Formula: " 
				    (mapcar 'list formulas) nil t)))
    (if (equal formula "")
	(error "Must provide a formula name")
      (list formula))))

(defpvs display-prooflite-script edit-proof (formula)
  "Display ProofLite script of the default proof of formula 

Displays the ProofLite script of the default proof of formula
in the \"ProofLite\" buffer."
  (interactive (complete-formula-name))
  (confirm-not-in-checker)
  (pvs-bury-output)
  (save-some-pvs-buffers)
  (let* ((file (current-pvs-file))
	 (theory (current-theory))
	 (*pvs-error* nil))
    (pvs-send-and-wait (format "(typecheck-file \"%s\" nil nil nil t)"
			       file)
		       nil 'tc 'dont-care)
    (unless *pvs-error*
      (when (get-buffer "ProofLite")
	(kill-buffer "ProofLite"))
      (set-proof-script-font-lock-keywords)
      (pvs-send-and-wait 
       (format "(display-prooflite-script \"%s\" \"%s\")" theory formula)
       nil nil 'dont-care)
      (prooflite-buffer 
       (format "%%%%%% To install this ProofLite script copy it to the theory %s %%%%%%"
	       theory))
      (display-buffer "ProofLite"))))
    
(define-key pvs-mode-map "\C-cit" 'install-prooflite-scripts-theory)
(define-key pvs-mode-map "\C-cip" 'install-prooflite-script)
(define-key pvs-mode-map "\C-c!t" 'install-prooflite-scripts-theory!)
(define-key pvs-mode-map "\C-c!p" 'install-prooflite-script!)
(define-key pvs-mode-map "\C-c2p" 'insert-prooflite-script)
(define-key pvs-mode-map "\C-cdp" 'display-prooflite-script)



