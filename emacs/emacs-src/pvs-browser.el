;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-browser.el -- 
;; Author          : Sam Owre
;; Created On      : Fri Mar 18 01:51:02 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Mar 18 01:52:26 1994
;; Update Count    : 1
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when-compile (require 'pvs-macros))

;;; PVS browse mode

;;; PVS browse mode key bindings

(defvar pvs-browse-mode-map nil)
(if pvs-browse-mode-map ()
    (setq pvs-browse-mode-map (make-keymap))
    (suppress-keymap pvs-browse-mode-map t)
    (define-key pvs-browse-mode-map "s" 'pvs-browse-select)
    (define-key pvs-browse-mode-map "v" 'pvs-browse-view)
    (define-key pvs-browse-mode-map "q" 'pvs-browse-quit)
    ;;(define-key pvs-browse-mode-map "u" 'usedby-proofs)
    (define-key pvs-browse-mode-map "\M-," 'find-declaration)
    (define-key pvs-browse-mode-map "\M-;" 'whereis-declaration-used)
    (define-key pvs-browse-mode-map "\M-:" 'list-declarations)
    (define-key pvs-browse-mode-map [(control meta ?.)]  'goto-declaration)
    (define-key pvs-browse-mode-map
	  [(control meta ?;)] 'whereis-identifier-used)
    (define-key pvs-browse-mode-map " " 'next-line)
    (define-key pvs-browse-mode-map "n" 'next-line)
    (define-key pvs-browse-mode-map "p" 'previous-line)
    (define-key pvs-browse-mode-map "\177" 'previous-line)
    (define-key pvs-browse-mode-map "h" 'pvs-browse-help)
    (define-key pvs-browse-mode-map "?" 'pvs-browse-help))


(defun pvs-browse-mode ()
  "Major mode for browsing a list of declarations
Each line describes one of the declarations in the current context.
The most useful key bindings for this mode are:
  s -- pvs-browse-select
  v -- pvs-browse-view
  h -- pvs-browse-help
  q -- pvs-browse-quit

The complete set of bindings is:
\\{pvs-browse-mode-map}"
  (kill-all-local-variables)
  (use-local-map pvs-browse-mode-map)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq major-mode 'pvs-browse-mode)
  (setq mode-name "Browse"))


;;; show-declaration, find-declaration, whereis-declaration-used, and
;;; list-declarations are the interface to this mode

(defpvs show-declaration browse (bufname origin line column)
  "Show declaration of symbol at cursor

The show-declaration command is used to determine the declaration
associated with a name.  Positioning the cursor on a name in the
specification and typing M-. yields a pop-up buffer displaying the
declaration.  This command is useful to determine the type of a name,
or the resolution determined by the typechecker for an overloaded name."
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			     (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin))
	  (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	  (line (+ (current-line-number) prelude-offset)))
     (list bufname origin line (real-current-column))))
  (save-some-pvs-files)
  (pvs-bury-output)
  (if (member-equal origin '("tccs" "ppe"))
      (message
       "The show-declaration command is not available in this buffer.")
    (progn 
      (pvs-send-and-wait (format "(show-declaration \"%s\" \"%s\" '(%d %d))"
				 bufname origin line (real-current-column))
			 nil 'declaration 'dont-care))))
     
(defpvs goto-declaration browse (bufname origin line column)
  "Go to declaration of symbol at cursor

The goto-declaration command goes to the declaration associated with
the symbol or name at the cursor.  It pops up a buffer containing the
theory associated with the declaration, and positions the cursor at the
declaration."
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			     (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin))
	  (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	  (line (+ (current-line-number) prelude-offset)))
     (list bufname origin line (real-current-column))))
  (pvs-bury-output)
  (save-some-pvs-files)
  (pvs-send-and-wait (format "(goto-declaration \"%s\" \"%s\" '(%d %d))"
			     bufname origin line (real-current-column))
		     nil 'declaration 'dont-care))

(defpvs find-declaration browse (symbol)
  "Search for declarations of given symbol

The find-declaration takes a name and returns a list of all the
declarations with that name, the default name is the one under the
cursor. Each row in the display specifies the declaration name, its
kind/type, and the theory to which it belongs.  Declarations in this list
may be viewed by placing the cursor on the row of interest and typing `v'.
Typing `s' will read in the associated file and position the cursor at the
declaration.  A `q' quits and removes the declaration buffer."
  (interactive (find-pvs-name "List declarations named: "))
  (save-some-pvs-files)
  (let ((pvs-decls (pvs-file-send-and-wait
		    (format "(find-declaration \"%s\")" symbol)
		    "Listing..." 'listing 'list)))
    (unless pvs-decls
      (error "No declarations matching %s were found" symbol))
    (setq *pvs-decls* pvs-decls)
    (pop-to-buffer (pvs-make-browse-buffer))
    (optimize-window-height)
    (goto-line 3)
    (pvs-browse-mode)))

(defpvs whereis-declaration-used browse (bufname origin line column)
  "Search for declarations which reference the declaration at point

The whereis-declaration-used command generates a list of declarations
which reference the specified declaration.  Each row in the display
specifies the declaration name, its kind/type, and the theory to which it
belongs.  Declarations in this list may be viewed by placing the cursor on
the row of interest and typing `v'.  Typing `s' will read in the
associated file and position the cursor at the declaration.  A `q' quits
and removes the declaration buffer."
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			       (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin))
	  (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	  (line (+ (current-line-number) prelude-offset)))
     (list bufname origin line (real-current-column))))
  (save-some-pvs-files)
  (pvs-bury-output)
  (if (member-equal origin '("tccs" "ppe"))
      (message
       "The show-declaration command is not available in this buffer.")
      (let ((pvs-decls
	     (pvs-file-send-and-wait
	      (format "(whereis-declaration-used \"%s\" \"%s\" '(%d %d))"
		  bufname origin line (real-current-column))
	      "Listing..." 'listing 'list)))
	(unless pvs-decls
	  (error "No declarations using were found"))
	(setq *pvs-decls* pvs-decls)
	(pop-to-buffer (pvs-make-browse-buffer))
	(optimize-window-height)
	(goto-line 3)
	(pvs-browse-mode))))

(defpvs whereis-identifier-used browse (symbol)
  "Search for declarations which reference symbol

The whereis-declaration-used command generates a list of declarations
which reference the specified name.  Each row in the display specifies the
declaration name, its kind/type, and the theory to which it belongs.
Declarations in this list may be viewed by placing the cursor on the row
of interest and typing `v'.  Typing `s' will read in the associated file
and position the cursor at the declaration.  A `q' quits and removes the
declaration buffer."
  (interactive (find-pvs-name "List declarations containing symbol: "))
  (save-some-pvs-files)
  (let ((pvs-decls (pvs-file-send-and-wait
		    (format "(whereis-identifier-used \"%s\")" symbol)
		    "Listing..." 'listing 'list)))
    (unless pvs-decls
      (error "No declarations using %s were found" symbol))
    (setq *pvs-decls* pvs-decls)
    (pop-to-buffer (pvs-make-browse-buffer))
    (optimize-window-height)
    (goto-line 3)
    (pvs-browse-mode)))

(defpvs list-declarations browse (theory)
  "Produce list of declarations in import chain

The list-declarations command generates a listing of all the declarations
in the import chain of the specified theory.  Each row in the display
specifies the declaration name, its kind/type, and the theory to which it
belongs.  Declarations in this list may be viewed by placing the cursor on
the row of interest and typing `v'.  Typing `s' will read in the
associated file and position the cursor at the declaration.  A `q' quits
and removes the declaration buffer."
  (interactive (complete-theory-name
		"List declarations for theory named: "))
  (unless (interactive-p) (pvs-collect-theories))
  (save-some-pvs-files)
  (let ((pvs-decls (pvs-file-send-and-wait
		    (format "(list-declarations \"%s\")" theory)
		    "Listing..." 'listing 'list)))
    (unless pvs-decls
      (error "No declarations in theory %s were found" theory))
    (setq *pvs-decls* pvs-decls)
    (pop-to-buffer (pvs-make-browse-buffer))
    (optimize-window-height)
    (goto-line 3)
    (pvs-browse-mode)))


;;; Functions to support PVS browse mode

(defun pvs-make-browse-buffer ()
  (let ((buf (get-buffer-create "Browse")))
    (save-excursion
      (set-buffer buf)
      (if buffer-read-only (toggle-read-only))
      (erase-buffer)
      (pvs-insert-declarations)
      (goto-char (point-min))
      (toggle-read-only)
      buf)))

(defun pvs-insert-declarations ()
  (insert "Declaration               Type                      Theory\n\n")
  (dolist (decl *pvs-decls*)
    (insert (car decl))
    (insert "\n")))

(defun pvs-browse-select ()
  "Select this line's declaration in full screen."
  (interactive)
  (if (<= (current-line-number) 2)
      (error "Please select from list of choices below."))
  (let* ((entry (nth (- (current-line-number) 3) *pvs-decls*))
	 (file (unless (eq (fourth entry) 'NIL)
		 (format "%s.pvs" (fourth entry))))
	 (loc (fifth entry)))
    (if (eq loc 'NIL)
	(message "Cannot select this entry")
	(pvs-browse-quit)
	(cond ((null file)
	       (set-prelude-files-and-regions)
	       (let* ((freg (get-prelude-file-and-region (third entry)))
		      (line (save-excursion
			      (let ((noninteractive t));; Shut up about read-only
				(set-buffer (find-file-noselect (car freg))))
			      (goto-char (cadr freg))
			      (- (current-line-number) 1))))
		 (view-prelude-theory (third entry))
		 (goto-line (- (car loc) line))
		 (forward-char (cadr loc))))
	      (t (find-file file)
		 (goto-line (car loc))
		 ;;(recenter)
		 (forward-char (cadr loc)))))))


(defun pvs-browse-view ()
  "View declaration on current line in Declaration list.
Returns to Declaration List when done."
  (interactive)
  (if (<= (current-line-number) 2)
      (error "Please select from list of choices below."))
  (let* ((cbuf (current-buffer))
	 (entry (nth (- (current-line-number) 3) *pvs-decls*))
	 (decl (sixth entry)))
    (let ((buf (get-buffer-create "Browse View")))
      (set-buffer buf)
      (if buffer-read-only (toggle-read-only))
      (erase-buffer)
      (insert decl)
      (set-buffer-modified-p nil)
      (toggle-read-only)
      (goto-char (point-min))
      ;;(popper-other-window 1)
      ;;(popper-show buf)
      ;;(pop-to-buffer cbuf)
      )))

(defun pvs-browse-quit ()
  (interactive)
  (remove-buffer (current-buffer))
  (pvs-bury-output)
  )

(defun pvs-browse-help ()
  (interactive)
  (other-window 1)
  (with-output-to-temp-buffer "*Help*"
    (princ (documentation 'pvs-browse-mode)))
  (pop-to-buffer "Browse"))

(defun mouse-show-declaration (event)
  "Show declaration of symbol at mouse pointer

The mouse-show-declaration command is used to determine the declaration
associated with a name.  Positioning the mouse pointer on a name in the
specification and typing S-mouse-2 yields a pop-up buffer displaying the
declaration.  This command is useful to determine the type of a name,
or the resolution determined by the typechecker for an overloaded name."
  (interactive "e")
  (mouse-minibuffer-check event)
  ;; Use event-end in case called from mouse-drag-region.
  ;; If EVENT is a click, event-end and event-start give same value.
  (let ((posn (event-end event)))
    (if (not (windowp (posn-window posn)))
	(error "Cursor not in text area of window"))
    (select-window (posn-window posn))
    (if (numberp (posn-point posn))
	(save-excursion
	  (goto-char (posn-point posn))
	  (call-interactively 'show-declaration)))))

(defpvs usedby-proofs browse (bufname origin line column)
  "Show a list of formulas whose proofs refer to the declaration at point"
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			       (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin))
	  (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	  (line (+ (current-line-number) prelude-offset)))
     (list bufname origin line (real-current-column))))
  (save-some-pvs-files)
  (pvs-bury-output)
  (if (member-equal origin '("tccs" "ppe"))
      (message
       "The usedby-proofs command is not available in this buffer.")
      (let ((pvs-decls (pvs-file-send-and-wait
			(format "(usedby-proofs \"%s\" \"%s\" %d)"
			    bufname origin line)
			"Listing..." 'listing 'list)))
	(when pvs-decls
	  (setq *pvs-decls* pvs-decls)
	  (pop-to-buffer (pvs-make-browse-buffer))
	  (optimize-window-height)
	  (goto-line 3)
	  (pvs-browse-mode)))))
