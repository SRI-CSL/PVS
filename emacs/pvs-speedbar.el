;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;; pvs-speedbar.el --

;; Creates a PVS speedbar mode
;; Each line consists of a button and title
;; A button with a '+' can be expanded; '-' contracts.
;; Titles have hover text and a 'goto' action when left-clicked
;; Top level is directories and libraries

;;; (add-hook 'buffer-list-update-hook 'set-speedbar-mode)
;;;     if we want to have foo invoked when an Emacs window gets focus

(eval-when-compile (require 'speedbar))

(declare-function speedbar-add-expansion-list "speedbar" (new-list))
(declare-function speedbar-add-mode-functions-list "speedbar" (new-list))
(declare-function speedbar-center-buffer-smartly "speedbar" ())
(declare-function speedbar-change-expand-button-char "speedbar" (char))
(declare-function speedbar-change-initial-expansion-list "speedbar" (new-default))
(declare-function speedbar-delete-subblock "speedbar" (indent))
(declare-function speedbar-disable-update "speedbar" ())
(declare-function speedbar-make-button "speedbar"
		  (start end face mouse function &optional token))
(declare-function speedbar-make-specialized-keymap "speedbar" ())
(declare-function speedbar-make-tag-line "speedbar"
                  (exp-button-type exp-button-char exp-button-function
                   exp-button-data tag-button tag-button-function
                   tag-button-data tag-button-face depth))

(defpvs pvs-speedbar find-files (&optional arg)
  "pvs-speedbar makes it easy to browse the PVS libraries"
  (interactive "P")
  (speedbar arg))

;; defezimage (and defimage) are macros 
(eval `(defezimage pvs-speedbar-check-mark
	   ((:type xpm :file ,(format "%s/emacs/check-mark.xpm" pvs-path)
		   :scale default :ascent center))
	 "Check mark"))
(eval `(defezimage pvs-speedbar-cross
	   ((:type xpm :file ,(format "%s/emacs/cross.xpm" pvs-path)
		   :scale default :ascent center))
	 "Cross"))

(defvar pvs-speedbar-key-map nil
  "Keymap used when in the pvs display mode.")

(defun pvs-install-speedbar-variables ()
  "Install those variables used by speedbar for PVS support."
  (ignore-errors (speedbar-disable-update))
  (if pvs-speedbar-key-map
      nil
      (setq pvs-speedbar-key-map (speedbar-make-specialized-keymap))
      ;; Basic tree features
      (define-key pvs-speedbar-key-map "e" 'speedbar-edit-line)
      (define-key pvs-speedbar-key-map "\C-m" 'speedbar-edit-line)
      (define-key pvs-speedbar-key-map "+" 'speedbar-expand-line)
      (define-key pvs-speedbar-key-map "-" 'speedbar-contract-line)
      (define-key pvs-speedbar-key-map "?" 'pvs-speedbar-help)
      )
  (setq speedbar-initial-expansion-list-name "PVS Files")
  ;; sets speedbar-initial-expansion-mode-alist
  ;; pvs-speedbar-list-libdirs sets it all up
  (speedbar-add-expansion-list '("PVS Files" pvs-speedbar-menu-items
				 pvs-speedbar-key-map
				 pvs-speedbar-list-libdirs))
  (speedbar-add-mode-functions-list
   '("PVS Files"
     (speedbar-item-info . pvs-speedbar-item-info)
     (speedbar-line-directory . pvs-speedbar-line-directory)))
  (cl-pushnew '("<V>" . pvs-speedbar-check-mark)
	   speedbar-expand-image-button-alist :test 'equal)
  (cl-pushnew '("<X>" . pvs-speedbar-cross)
	   speedbar-expand-image-button-alist :test 'equal)
  (define-key speedbar-mode-map "p"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "PVS Files")))
  (define-key speedbar-mode-map "i"
      (lambda () (interactive)
        (speedbar-change-initial-expansion-list "Info"))))

(with-eval-after-load 'speedbar
  (pvs-install-speedbar-variables))

(defun pvs-speedbar-help ()
  (interactive)
  (let ((help-file (format "%s/lib/pvs-speedbar.org" pvs-path)))
    ;; (display-buffer-pop-up-frame
    ;;  help-buf
    ;;  `((dedicated . t) (unsplittable . t) (modeline . t) (use-frame-synchronization)
    ;;    (tool-bar-lines . 0) (tab-bar-lines . 0) (menu-bar-lines . 0)
    ;;    (bottom-divider-width . 0) (right-divider-width . 0) (internal-border-width . 0)
    ;;    (undecorated) (border-width . 0)
    ;;    (name . "Speedbar Help") (minibuffer . nil)))
    (speedbar-find-file-in-frame help-file)
    ))

(defvar pvs-speedbar-kind-char
  '(("importing" . "I")
    ("type" . "T")
    ("variable" . "V")
    ("const" . "C")
    ("formula" . "F")
    ("tcc" . "O") ; for Obligation
    ("proof" . "P")
    ("judgement" . "J")
    ("conversion" . ">")
    ("datatype" . "d")
    ("codatatype" . "c")
    ("constructor" . "+")
    ("auto-rewrite" . "A")
    ("exporting" . "")))

(defun pvs-speedbar-item-info ()
  (save-excursion
    (dframe-message "%s" (get-text-property (point) 'path))))

(defun pvs-speedbar-line-directory (&optional depth)
  "Retrieve the directory name associated with the current line.
This may require traversing backwards from DEPTH and combining the default
directory with these items.  This function is replaceable in
`speedbar-mode-functions-list' as `speedbar-line-directory'."
  (ignore depth)
  (save-restriction
    (widen)
    default-directory))

;; (cl-pushnew '(speedbar-fetch-dynamic-pvs . speedbar-insert-theory-list)
;; 	    speedbar-dynamic-tags-function-list
;; 	    :test 'equal)

;; Need this for PVS files to display
;; lists is of the form (NAME MENU KEYMAP FN1 FN2 ...)
;; (speedbar-add-expansion-list '("pvs" pvs-speedbar-menu-items
;;  			       pvs-speedbar-key-map))

(defvar pvs-speedbar-menu-items
  '(["Edit PVS File" speedbar-edit-line t]
    ["Expand PVS File" speedbar-expand-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.\\+. "))]
    ["Contract Library" speedbar-contract-line
     (save-excursion (beginning-of-line)
		     (looking-at "[0-9]+: *.-. "))]
    )
  "Additional menu-items to add to speedbar frame.")

;; list-libdirs
;;   expand-libdir        expand-libdir
;;     list-pvs-files
;;       expand-pvs-file
;;         list-theories
;;           expand-theory
;;             list-decls
;;               expand-decl
;;                 list-decl-contents
;;                   expand-mult-proofs
;;                     list-mult-proofs
;;                   expand-tcc
;;                     list-tcc-contents
;;                       expand-mult-proofs

;; `bracket', `angle', `curly', `expandtag', `statictag', t, or nil.

(defun pvs-speedbar-list-libdirs (&rest args)
  (ignore args)
  (let ((libdirs (get-pvs-libdir-contents)))
    (dolist (libdir libdirs)
      (let ((dir (car libdir))
	    (lib (cdr libdir))
	    (start (point)))
	(speedbar-make-tag-line 'angle ?+ 'pvs-speedbar-expand-libdir
				libdir (or lib dir)
				'pvs-speedbar-expand-libdir
				libdir
				'speedbar-directory-face 0)
	(put-text-property start (point) 'path dir)))))

(defun pvs-speedbar-expand-libdir (text libdir indent)
  ;; E.g., text = "[+]", libdir = ("/home/owre/pvslib/PVS0/" . "PVS0"), indent = 0
  (cond ((string-search "+" text)	;we have to expand this theory
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (pvs-speedbar-list-pvs-files libdir (1+ indent))))
	 (speedbar-change-expand-button-char ?-))
	((string-search "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "pvs-speedbar-expand-libdir: text = %s" text)))
  (speedbar-center-buffer-smartly))

(defun pvs-speedbar-list-pvs-files (libdir indent)
  (let* ((all-files (directory-files (car libdir) t "^[^.]"))
	 (all-subdirs (cl-remove-if-not #'file-directory-p all-files))
	 (subdirs (cl-remove-if #'(lambda (sd) (string-match "/pvsbin$" sd)) all-subdirs))
	 (pvs-files (cl-remove-if-not #'(lambda (fn) (string-match ".*\.pvs$" fn))
		      all-files)))
    (dolist (subdir subdirs)
      (let ((subname (concat (file-name-nondirectory subdir) "/"))
	    (start (point)))
	(speedbar-make-tag-line 'angle ?+ 'pvs-speedbar-expand-libdir
				(list subdir) subname
				'pvs-speedbar-expand-libdir
				(list subdir)
				'speedbar-directory-face indent)
	(put-text-property start (point) 'path subdir)))
    (dolist (pvs-file pvs-files)
      (let ((fname (file-name-nondirectory pvs-file))
	    (start (point)))
	(speedbar-make-tag-line 'bracket ?+ 'pvs-speedbar-expand-pvs-file
				pvs-file fname
				'pvs-speedbar-goto-pvs-file
				(list pvs-file) ;; No place
				'speedbar-file-face indent)
	(put-text-property start (point) 'path pvs-file)
	(when (pvs-send-and-wait (format "(typechecked-file\? \"%s\")" pvs-file))
	  (forward-line -1)
	  ;;(beginning-of-line)
	  (speedbar-add-indicator "<V>")
	  (forward-line 1))))
    (unless (or subdirs pvs-files)
      (message "%s has no pvs-files or subdirectories" (car libdir)))))

(defun pvs-speedbar-remove-indicators ()
  (speedbar-with-writable
    (save-excursion
      (beginning-of-line)
      (while (re-search-forward " <\\(V\\|X\\)>" nil t)
	(delete-region (match-beginning 0) (match-end 0))))))

(defun pvs-speedbar-expand-pvs-file (text pvs-file indent)
  (cond ((string-search "+" text)
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (let ((theory-specs (get-pvs-file-contents pvs-file)))
	       (pvs-speedbar-remove-indicators)
	       (cond ((assq 'error theory-specs)
		      (let ((errmsg (cdr (assq 'error theory-specs))))
			(speedbar-add-indicator "<X>")
			;; This doesn't work
			(end-of-line)
			(forward-char -2)
			(let ((start (point)))
			  (put-text-property start (+ start 2) 'path errmsg))
			))
		     (t	(speedbar-add-indicator "<V>")
			(end-of-line) (forward-char 1)
			(pvs-speedbar-list-theories (1+ indent) pvs-file theory-specs)
			(speedbar-change-expand-button-char ?-)))))))
	((string-search "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "pvs-speedbar-expand-pvs-file: text = %s" text))))

(defun pvs-speedbar-goto-pvs-file (_text file-place _indent)
  ;; (when (eq (selected-frame) speedbar-frame)
  ;;   (next-frame))
  (let ((file (car file-place))
	(place (cdr file-place)))
    (speedbar-find-file-in-frame file)
    (goto-char (point-min))
    (forward-line (1- (car place)))
    (forward-char (cadr place))
    (recenter 5 t)))

(defun pvs-speedbar-list-theories (indent pvs-file theory-specs)
  (speedbar-with-writable
    (dolist (theory-spec theory-specs)
      (let ((id (cdr (or (assq 'theory-id theory-spec)
			 (assq 'adt-id theory-spec))))
	    (start (point)))
	(speedbar-make-tag-line 'curly ?+ 'pvs-speedbar-expand-theory
				theory-spec id
				'pvs-speedbar-goto-pvs-file
				(cons pvs-file (cdr (assq 'place theory-spec)))
				'blue indent)
	(put-text-property start (point) 'path (format "%s#%s" pvs-file id))))))

(defun pvs-speedbar-expand-theory (text theory-spec indent)
  "Expand the node the user clicked on.
TEXT is the text of the button we clicked on, a + or - item.
THEORY-SPEC is data related to this theory.
INDENT is the current indentation depth."
  (cond ((string-search "+" text)	;we have to expand this theory
	 (speedbar-change-expand-button-char ?-)
	 (speedbar-with-writable
	   (save-excursion
	     (end-of-line) (forward-char 1)
	     (pvs-speedbar-list-decls theory-spec (1+ indent)))))
	((string-search "-" text)	;we have to contract this node
	 (speedbar-change-expand-button-char ?+)
	 (speedbar-delete-subblock indent))
	(t (error "Ooops... not sure what to do")))
  (speedbar-center-buffer-smartly))

(defun pvs-speedbar-list-decls (theory-spec indent)
  (let* ((path (cdr (assoc 'path theory-spec)))
	 (decl-specs (append (cdr (assq 'formals theory-spec))
			     (cdr (assq 'assuming theory-spec))
			     (cdr (assq 'theory theory-spec))
			     (let ((exp (cdr (assq 'exporting theory-spec))))
			       (when exp (list exp))))))
    (dolist (decl-spec decl-specs)
      (let* ((id (or (cdr (assq 'decl-id decl-spec))
		     (let ((imp (cdr (assq 'importing decl-spec))))
		       (or imp
			   (error "No id?")))))
	     (kind (cdr (assq 'kind decl-spec)))
	     (kchar (cdr (assoc kind pvs-speedbar-kind-char #'string=)))
	     (kid (format "%s %s" kchar id))
	     (dbody (cdr (assq 'decl-str decl-spec)))
	     (proved (assq 'proved decl-spec))
	     (subs? (or (assq 'tccs decl-spec)
			(cdr (assq 'proofs decl-spec))))
	     (ch (if subs? ?+ ?=))
	     (exp (when subs? 'pvs-speedbar-expand-decl))
	     (file-place (cons path (cdr (assq 'place decl-spec))))
	     (start (point)))
	(speedbar-make-tag-line 'expandtag ch exp
				(list path theory-spec decl-spec) kid
				'pvs-speedbar-goto-pvs-file
				file-place
				'purple indent)
	(when proved
	  (save-excursion
	    (goto-char start)
	    (pvs-speedbar-remove-indicators)
	    (if (cdr proved)
		(speedbar-add-indicator "<V>")
		;;(speedbar-add-indicator "<X>")
		)))
	(put-text-property start (point) 'path dbody)
	(save-excursion
	  (end-of-line)
	  (let ((start (search-backward kid)))
	    (overlay-put (make-overlay start (+ start 1))
			 'face 'org-checkbox)))))))

(defun pvs-speedbar-expand-decl (text path-decl-spec indent)
  (let ((path (car path-decl-spec))
	(theory-spec (cadr path-decl-spec))
	(decl-spec (caddr path-decl-spec)))
    (cond ((string-search "+" text)	;we have to expand this theory
	   (speedbar-change-expand-button-char ?-)
	   (speedbar-with-writable
	     (save-excursion
	       (end-of-line) (forward-char 1)
	       (pvs-speedbar-list-decl-contents path theory-spec decl-spec (1+ indent)))))
	  ((string-search "-" text)	;we have to contract this node
	   (speedbar-change-expand-button-char ?+)
	   (speedbar-delete-subblock indent))
	  (t (error "Ooops... not sure what to do")))
    (speedbar-center-buffer-smartly)))

(defun pvs-speedbar-list-decl-contents (path theory-spec decl-spec indent)
  ;; First do proof, if any
  (let ((prf-specs (cdr (assq 'proofs decl-spec))))
    (when prf-specs
      (let* ((dprf (cl-find-if #'(lambda (prf) (cdr (assoc 'default prf))) prf-specs))
	     (mproofs (when (cdr prf-specs) (remove dprf prf-specs)))
	     (ch (if mproofs ?+ ? ))
	     (exp (when mproofs 'pvs-speedbar-expand-mult-proofs))
	     (id (cdr (assq 'prf-id dprf)))
	     (pid (format "P %s" id))
	     (script (cdr (assq 'script dprf)))
	     (start (point)))
	(speedbar-make-tag-line 'bracket ch exp
				(list path theory-spec decl-spec mproofs) pid
				'pvs-speedbar-goto-proof
				(list path theory-spec decl-spec dprf)
				'purple indent)
	(put-text-property start (point) 'path script)
	(save-excursion
	  (end-of-line)
	  (let ((start (search-backward pid)))
	    (overlay-put (make-overlay start (+ start 1))
			 'face 'org-warning))))))
  (let ((tcc-specs (cdr (assq 'tccs decl-spec))))
    (dolist (tcc-spec tcc-specs)
      (let* ((id (cdr (assq 'decl-id tcc-spec)))
	     (tid (format "O %s" id))
	     (dbody (cdr (assq 'decl-str tcc-spec)))
	     (path-spec (list path theory-spec decl-spec tcc-spec))
	     (start (point)))
	(speedbar-make-tag-line 'bracket ?+ 'pvs-speedbar-expand-tcc
				path-spec tid
				'pvs-speedbar-goto-tcc
				path-spec
				'purple indent)
	(put-text-property start (point) 'path dbody)
	(save-excursion
	  (end-of-line)
	  (let ((start (search-backward tid)))
	    (overlay-put (make-overlay start (+ start 1))
			 'face 'org-warning)))))))

(defun pvs-speedbar-expand-tcc (text path-decl-spec indent)
  (let ((path (car path-decl-spec))
	(theory-spec (cadr path-decl-spec))
	(decl-spec (caddr path-decl-spec))
	(tcc-spec (cadddr path-decl-spec)))
    (cond ((string-search "+" text)	;we have to expand this tcc
	   (speedbar-change-expand-button-char ?-)
	   (speedbar-with-writable
	     (save-excursion
	       (end-of-line) (forward-char 1)
	       (pvs-speedbar-list-tcc-contents path theory-spec decl-spec tcc-spec (1+ indent)))))
	  ((string-search "-" text)	;we have to contract this node
	   (speedbar-change-expand-button-char ?+)
	   (speedbar-delete-subblock indent))
	  (t (error "Ooops... not sure what to do")))))

(defun pvs-speedbar-list-tcc-contents (path theory-spec decl-spec tcc-spec indent)
  (let ((prf-specs (cdr (assq 'proofs tcc-spec))))
    (when prf-specs
      (let* ((dprf (cl-find-if #'(lambda (prf) (cdr (assoc 'default prf))) prf-specs))
	     (mproofs (when (cdr prf-specs) (remove dprf prf-specs)))
	     (ch (if mproofs ?+ ? ))
	     (exp (when mproofs 'pvs-speedbar-expand-mult-proofs))
	     (id (cdr (assq 'prf-id dprf)))
	     (pid (format "P %s" id))
	     (script (cdr (assq 'script dprf)))
	     (start (point)))
	(speedbar-make-tag-line 'bracket ch exp
				(list path theory-spec tcc-spec mproofs) pid
				'pvs-speedbar-goto-proof
				(list path theory-spec tcc-spec dprf)
				'purple indent)
	(put-text-property start (point) 'path script)
	(save-excursion
	  (end-of-line)
	  (let ((start (search-backward pid)))
	    (overlay-put (make-overlay start (+ start 1))
			 'face 'org-warning)))))))

(defun pvs-speedbar-goto-tcc (_text path-spec _indent)
  (let* ((path (car path-spec))
	 (theory-spec (cadr path-spec))
	 ;; (decl-spec (caddr path-spec))
	 (tcc-spec (cadddr path-spec))
	 (thpath (format "%s#%s" path (cdr (assq 'theory-id theory-spec)))))
    (show-tccs thpath)
    (goto-char (point-min))
    (re-search-forward (cdr (assq 'decl-id tcc-spec)))
    (beginning-of-line)))

(defun pvs-speedbar-expand-mult-proofs (text mprfs-spec indent)
  (let ((path (car mprfs-spec))
	(theory-spec (cadr mprfs-spec))
	(decl-spec (caddr mprfs-spec))
	(mproofs (cadddr mprfs-spec)))
    (cond ((string-search "+" text)
	   (speedbar-change-expand-button-char ?-)
	   (speedbar-with-writable
	     (save-excursion
	       (end-of-line) (forward-char 1)
	       (pvs-speedbar-list-mult-proofs mproofs path theory-spec decl-spec (1+ indent)))))
	  ((string-search "-" text)	;we have to contract this node
	   (speedbar-change-expand-button-char ?+)
	   (speedbar-delete-subblock indent))
	  (t (error "Ooops... not sure what to do")))
    (speedbar-center-buffer-smartly)))

(defun pvs-speedbar-list-mult-proofs (mproofs path theory-spec decl-spec indent)
  (dolist (prf-spec mproofs)
    (let* ((id (cdr (assq 'prf-id prf-spec)))
	   (pid (format "P %s" id))
	   (script (cdr (assq 'script prf-spec)))
	   (start (point)))
      (speedbar-make-tag-line 'bracket ?  nil ; 'pvs-speedbar-expand-proof
			      prf-spec pid
			      'pvs-speedbar-goto-proof
			      (list path theory-spec decl-spec prf-spec)
			      'blue indent)
      (put-text-property start (point) 'path script))))

;; (defun pvs-speedbar-expand-proof (text tcc-spec indent)
;;   nil)

(defun pvs-speedbar-goto-proof (_text prf-path-spec _indent)
  (let* ((path (car prf-path-spec))
	 (theory-spec (cadr prf-path-spec))
	 (decl-spec (caddr prf-path-spec))
	 (prf-spec (cadddr prf-path-spec))
	 (th-id (cdr (assq 'theory-id theory-spec)))
	 (decl-id (cdr (assq 'decl-id decl-spec)))
	 (fpath (format "%s#%s#%s" path th-id decl-id))
	 (script (cdr (assq 'script prf-spec)))
	 (pbuf (get-buffer-create "Proof")))
    (with-current-buffer pbuf
      (clear-buffer pbuf)
      (insert script)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (pvs-prover-goto-next-step)
      (hilit-next-prover-command)
      (pop-to-buffer pbuf))
    (prove-formula-ref fpath)))

(defun prove-formula-ref (formula-ref)
  (pvs-busy)
  (ilisp-send (format "(prove-formula \"%s\")" formula-ref)
	      nil 'pr t))

(defun get-pvs-libdir-contents ()
  (let ((json-key-type 'string))
    (json-read-from-string
     (pvs-send-and-wait-for-json "(pvs-libdir-contents)"))))

(defun get-pvs-file-contents (file)
  (let ((json-array-type 'list))
    (json-read-from-string
     (pvs-send-and-wait-for-json (format "(pvs-file-contents \"%s\")" file)))))

;; (defun speedbar-fetch-dynamic-pvs (file)
;;   ""
;;   (if (string= (pathname-type file) "pvs")
;;       (condition-case nil
;; 	  (let ((theory-specs (get-pvs-file-contents file)))
;; 	    theory-specs)
;; 	(error t))
;;       t))
	
