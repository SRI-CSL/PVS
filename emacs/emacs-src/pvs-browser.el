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
    (define-key pvs-browse-mode-map [(control ?.)] 'show-expanded-form)
    (define-key pvs-browse-mode-map [(control meta ?.)]  'goto-declaration)
    (define-key pvs-browse-mode-map
	  [(control meta ?;)] 'whereis-identifier-used)
    (define-key pvs-browse-mode-map " " 'next-line)
    (define-key pvs-browse-mode-map "n" 'next-line)
    (define-key pvs-browse-mode-map "p" 'previous-line)
    (define-key pvs-browse-mode-map "\177" 'previous-line)
    (define-key pvs-browse-mode-map "h" 'describe-mode)
    (define-key pvs-browse-mode-map "?" 'describe-mode))


(defun pvs-browse-mode ()
  "Major mode for browsing a list of declarations
Each line describes one of the declarations in the current context.
The most useful key bindings for this mode are:
  s -- pvs-browse-select
  v -- pvs-browse-view
  h -- describe-mode
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
      (pvs-send-and-wait (format "(show-declaration \"%s\" \"%s\" '(%d %d))"
			     bufname origin line (real-current-column))
			 nil 'declaration 'dont-care)))
     
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
    (pvs-make-browse-buffer)))

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
	(pvs-make-browse-buffer))))

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
    (pvs-make-browse-buffer)))

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
    (pvs-make-browse-buffer)))

(defpvs unusedby-proof-of-formula browse (bufname origin line column)
  "Produce list of declarations unused by the proof of the formula at point

The unusedby-proof-of-formula command creates a 'Browse' buffer
listing all the declarations that are unused in the proof of the given
formula.  Removing all these declarations and those that follow the
given formula should give a theory that typechecks and for which the
proofchain is still complete, if it was in the full theory."
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			       (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin))
	  (prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	  (line (+ (current-line-number) prelude-offset)))
     (list bufname origin line (real-current-column))))
  (if (member-equal origin '("tccs" "ppe"))
      (message
       "The unusedby-proof-of-formula command is not available in this buffer.")
      (let ((pvs-decls (pvs-file-send-and-wait
			(format "(unusedby-proof-of-formula \"%s\" \"%s\" %d)"
			    bufname origin line)
			"Collecting..." 'unusedby 'list)))
	(unless pvs-decls
	  (error "No nuused declarations found for formula %s" ))
	(setq *pvs-decls* pvs-decls)
	(pvs-make-browse-buffer))))

(defpvs unusedby-proofs-of-formulas browse (formulas theory)
  "Produce list of declarations unused by the proofs of the given formulas

The unusedby-proofs-of-formulas command prompts for a list of formulas (just
hit 'Return' when done) and a root theory, and creates a 'Browse' buffer
listing all the declarations that are unused in the proofs of the given
formulas.  Removing all these declarations and those that follow the given
formula should give a theory that typechecks and for which the proofchain is
still complete, if it was in the full theory."
  (interactive
   (append (complete-formula-name-list "Formula: ")
	   (complete-theory-name "Root theory to use as context: ")))
  (let ((pvs-decls (pvs-file-send-and-wait
		    (format "(unusedby-proofs-of-formulas '%s \"%s\")"
			(mapcar '(lambda (x) (format "\"%s\"" x)) formulas)
		      theory)
		    "Collecting..." 'unusedby 'list)))
    (unless pvs-decls
      (error "No nuused declarations found for formula %s" ))
    (setq *pvs-decls* pvs-decls)
    (pvs-make-browse-buffer)))


;;; Functions to support PVS browse mode

(defun pvs-make-browse-buffer ()
  (let ((buf (get-buffer-create "Browse")))
    (save-excursion
      (set-buffer buf)
      (if buffer-read-only (toggle-read-only))
      (erase-buffer)
      (pvs-insert-declarations)
      (goto-line 3)
      (set-buffer-modified-p nil)
      (toggle-read-only)
      (pvs-browse-mode)
      (pvs-display-browse-buffer buf)
      buf)))

(defun pvs-insert-declarations ()
  (insert "Declaration               Type                      Theory\n\n")
  (dolist (decl *pvs-decls*)
    (insert (car decl))
    (insert "\n")))

(defun pvs-browse-select ()
  "Select this line's declaration in full screen."
  (interactive)
  (unless (string-equal (buffer-name) "Browse")
    (error "The pvs-browse-view command is not available in this buffer"))
  (if (<= (current-line-number) 2)
      (error "Please select from list of choices below."))
  (let* ((entry (nth (- (current-line-number) 3) *pvs-decls*))
	 (file (unless (member (fourth entry) '(nil NIL))
		 (format "%s.pvs" (fourth entry))))
	 (loc (fifth entry)))
    (if (member loc '(nil NIL))
	(let* ((bufname (format "%s.%s" (third entry) (second entry)))
	       (buf (get-buffer-create bufname)))
	  (save-excursion
	    (message "")
	    (set-buffer buf)
	    (if buffer-read-only (toggle-read-only))
	    (erase-buffer)
	    (insert (sixth entry))
	    (set-buffer-modified-p nil)
	    (toggle-read-only)
	    (pvs-view-mode))
	  (pop-to-buffer buf))
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
		 (forward-char (cadr loc))))
	(delete-other-windows)))
  (recenter))


(defun pvs-browse-view ()
  "View declaration on current line in Declaration list.
Returns to Declaration List when done."
  (interactive)
  (unless (string-equal (buffer-name) "Browse")
    (error "The pvs-browse-view command is not available in this buffer"))
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
      (pvs-view-mode)
      (goto-char (point-min))
      (let ((view-window (get-buffer-window-list buf)))
	(cond (view-window
	       (ilisp-shrink-wrap-window (car view-window)))
	      (t
	       (other-window 1)
	       (split-window-vertically (ilisp-desired-height buf))
	       (set-window-buffer (selected-window) buf)
	       (other-window -1)))))))

(defun pvs-browse-quit ()
  (interactive)
  (remove-buffer (current-buffer))
  (pvs-bury-output)
  (when (and (not *pvs-popup-windows*)
	     (window-configuration-p *pvs-popup-old-window-configuration*))
    (set-window-configuration *pvs-popup-old-window-configuration*)
    (setq *pvs-popup-old-window-configuration* nil)))

(defun pvs-browse-help ()
  (interactive)
  (other-window 1)
  (with-output-to-temp-buffer "*Help*"
    (princ (documentation 'pvs-browse-mode)))
  (pop-to-buffer "Browse"))

(defun mouse-show-declaration (event)
  "Show declaration of symbol at mouse pointer.

The mouse-show-declaration command is used to determine the declaration
associated with a name.  Positioning the mouse pointer on a name in the
specification and typing S-mouse-2 yields a pop-up buffer displaying the
declaration.  This command is useful to determine the type of a name,
or the resolution determined by the typechecker for an overloaded name."
  (interactive "e")
  (cond ((memq pvs-emacs-system '(xemacs21 xemacs20 xemacs19))
	 ;; This code is courtesy Jerry James (james@ittc.ku.edu)
	 (if (and (mouse-event-p event) (event-over-text-area-p event))
	     (progn
	       (select-window (event-window event))
	       (save-excursion
		 (goto-char (event-point event) (event-buffer event))
		 (call-interactively 'show-declaration)))
	     (error "Cursor not in text area of window")))
	(t (mouse-minibuffer-check event)
	   ;; Use event-end in case called from mouse-drag-region.
	   ;; If EVENT is a click, event-end and event-start give same value.
	   (let ((posn (event-end event)))
	     (if (not (windowp (posn-window posn)))
		 (error "Cursor not in text area of window"))
	     (select-window (posn-window posn))
	     (if (numberp (posn-point posn))
		 (save-excursion
		   (goto-char (posn-point posn))
		   (call-interactively 'show-declaration)))))))

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
	  (pvs-make-browse-buffer)))))

(defvar expanded-form-face 'expanded-form-face)
(make-face 'expanded-form-face)
(set-face-background 'expanded-form-face "violet")
(defvar expanded-form-overlay nil)

(defpvs show-expanded-form browse (bufname origin beg end)
  "Shows the expanded form of the specified region

The show-expanded-form command displays the expanded form of the given
region.  By default, names from the prelude are not expanded, but with an
argument they are expanded as well."
  (interactive
   (let* ((name-and-origin (if (string-equal (buffer-name) "Declaration")
			       (list nil "Declaration")
			       (pvs-formula-origin)))
	  (bufname (car name-and-origin))
	  (origin (cadr name-and-origin)))
     (list bufname origin
	   (if (mark t) (region-beginning) (point))
	   (if (mark t) (region-end) (point)))))
  (let* ((prelude-offset (if (equal origin "prelude-theory") pvs-prelude 0))
	 (pos1 (save-excursion
		 (goto-char beg)
		 (list (+ (current-line-number) prelude-offset)
		       (real-current-column))))
	 (pos2 (save-excursion
		 (goto-char end)
		 (list (+ (current-line-number) prelude-offset)
		       (real-current-column)))))
    (save-some-pvs-files)
    (pvs-bury-output)
    (let ((place (pvs-send-and-wait
		  (format "(show-expanded-form \"%s\" \"%s\" '%s '%s %s)"
		      bufname origin pos1 pos2
		      (and current-prefix-arg t))
		  nil 'expanded-form 'list)))
      (unless noninteractive
	(when place
	  (let ((tbeg (save-excursion
			(goto-line (- (car place) prelude-offset))
			(forward-char (cadr place))
			(point)))
		(tend (save-excursion
			(goto-line (- (caddr place) prelude-offset))
			(forward-char (cadddr place))
			(point))))
	    (setq expanded-form-overlay (make-overlay tbeg tend))
	    (overlay-put expanded-form-overlay 'face 'expanded-form-face)))))))

(defvar *pvs-popup-windows* nil
  "Controls behavior of browser functions.
nil    = use current frame
'frame = use a (potentially new) browser frame
'x     = use dedicated X windows popups")

(defvar *pvs-popup-browse-frame* nil
  "The frame to use for browse windows when *pvs-popup-windows* is 'frame")

(defvar *pvs-popup-old-window-configuration* nil
  "The window configuration to pop back to after quitting a brose window
when *pvs-popup-windows* is nil")


(defun pvs-display-browse-buffer (buf)
  "Popup a top level buffer, dependent on the value of *pvs-popup-windows*"
  (cond ((not *pvs-popup-windows*)
	 (setq *pvs-popup-old-window-configuration* (current-window-configuration))
	 (delete-other-windows)
	 (let ((top-window (selected-window))
	       (bottom-window (split-window-vertically (ilisp-desired-height buf))))
	   (set-window-buffer top-window buf)
	   (select-window bottom-window)))
	 ((eq *pvs-popup-windows* 'frame)
	 (unless (frame-live-p *pvs-popup-browse-frame*)
	   (setq *pvs-popup-browse-frame* (make-frame))))
	((eq *pvs-popup-windows* 'x)
	 (error "*pvs-popup-windows* as x not yet implemented"))
	(t (error "*pvs-popup-windows* is not one of nil, 'frame or 'x"))))
