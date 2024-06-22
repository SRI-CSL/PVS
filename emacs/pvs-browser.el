;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
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

(eval-when-compile (require 'pvs-macros))
(require 'json)

(defvar pvs-path)

(declare-function remove-buffer "pvs-mode")
(declare-function view-prelude-theory "pvs-cmds")
(declare-function get-prelude-file-and-region "pvs-cmds")
(declare-function pvs-view-mode "pvs-view")
(declare-function complete-formula-name-list "pvs-prover")
(declare-function pvs-formula-origin "pvs-prover")
(declare-function pvs-send-and-wait-for-json "pvs-ilisp")
(declare-function current-line-number "pvs-mode")
(declare-function pvs-send-and-wait "pvs-ilisp")
(declare-function pvs-bury-output "pvs-ilisp")
(declare-function save-some-pvs-files "pvs-cmds")

;;; PVS browse mode

;;; find-declaration (symbol)
;;; whereis-declaration-used ()
;;; whereis-identifier-used (symbol)
;;; list-declarations (theory)
;;; unusedby-proof-of-formula ()
;;; unusedby-proofs-of-formulas (formulas theory)
;;; usedby-proofs ()

;;; PVS browse mode key bindings
(defvar pvs-declarations)

(defvar pvs-popup-windows nil
  "Controls behavior of browser functions.
  nil    = use current frame
  \\='frame = use a (potentially new) browser frame
  \\='x     = use dedicated X windows popups")

(defvar pvs-popup-browse-frame nil
  "The frame to use for browse windows when pvs-popup-windows is \\='frame")

(defvar pvs-popup-old-window-configuration nil
  "The window configuration to pop back to after quitting a brose window
when pvs-popup-windows is nil")


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
	  [(control meta ?\;)] 'whereis-identifier-used)
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

(defpvs show-declaration browse ()
  "Show declaration of symbol at cursor

The show-declaration command is used to determine the declaration
associated with a name.  Positioning the cursor on a name in the
specification and typing M-. yields a pop-up buffer displaying the
declaration.  This command is useful to determine the type of a name,
or the resolution determined by the typechecker for an overloaded name."
  (interactive)
  (let* ((kind (pvs-kind-of-buffer))
	 (file (buffer-file-name)))
    (save-some-pvs-files)
    (pvs-bury-output)
    (pvs-send-and-wait (format "(show-declaration \"%s\" \"%s\" '(%d %d) nil)"
			   (or file (buffer-name)) kind
			   (current-line-number) (real-current-column))
		       nil 'declaration 'dont-care)))
     
(defpvs goto-declaration browse ()
  "Go to declaration of symbol at cursor

The goto-declaration command goes to the declaration associated with
the symbol or name at the cursor.  It pops up a buffer containing the
theory associated with the declaration, and positions the cursor at the
declaration."
  (interactive)
  (let* ((kind (pvs-kind-of-buffer))
	 (file (buffer-file-name)))
    (pvs-bury-output)
    (save-some-pvs-files)
    (pvs-send-and-wait (format "(goto-declaration \"%s\" \"%s\" '(%d %d))"
			   (or file (buffer-name)) kind
			   (current-line-number) (real-current-column))
		       'declaration 'dont-care)))

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
  (let ((pvs-decls (pvs-send-and-wait-for-json
		    (format "(find-declaration \"%s\")" symbol)
		    "Listing..." 'listing)))
    (unless pvs-decls
      (error "No declarations matching %s were found" symbol))
    (pvs-make-browse-buffer "Browse" pvs-decls)))

(defpvs whereis-declaration-used browse ()
  "Search for declarations which reference the declaration at point

The whereis-declaration-used command generates a list of declarations
which reference the specified declaration.  Each row in the display
specifies the declaration name, its kind/type, and the theory to which it
belongs.  Declarations in this list may be viewed by placing the cursor on
the row of interest and typing `v'.  Typing `s' will read in the
associated file and position the cursor at the declaration.  A `q' quits
and removes the declaration buffer."
  (interactive)
  (let* ((kind (pvs-kind-of-buffer))
	 (file (buffer-file-name)))
    (save-some-pvs-files)
    (pvs-bury-output)
    (let ((pvs-decls
	   (pvs-send-and-wait-for-json
	    (format "(whereis-declaration-used \"%s\" \"%s\" '(%d %d) nil)"
		(or file (buffer-name)) kind
		(current-line-number) (real-current-column))
	    "Listing..." 'listing)))
      (unless pvs-decls
	(error "No declarations using were found"))
      (pvs-make-browse-buffer "Browse" pvs-decls))))

(defpvs whereis-identifier-used browse (symbol)
  "Search for declarations which reference symbol

The whereis-identifier-used command generates a list of declarations
which reference the specified name.  Each row in the display specifies the
declaration name, its kind/type, and the theory to which it belongs.
Declarations in this list may be viewed by placing the cursor on the row
of interest and typing `v'.  Typing `s' will read in the associated file
and position the cursor at the declaration.  A `q' quits and removes the
declaration buffer."
  (interactive (find-pvs-name "List declarations containing symbol: "))
  (save-some-pvs-files)
  (let ((pvs-decls (pvs-send-and-wait-for-json
		    (format "(whereis-identifier-used \"%s\")" symbol)
		    "Listing..." 'listing)))
    (unless pvs-decls
      (error "No declarations using %s were found" symbol))
    (pvs-make-browse-buffer "Browse" pvs-decls)))

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
		"List declarations for theory named: " nil t))
  (save-some-pvs-files)
  (let ((pvs-decls (pvs-send-and-wait-for-json
		    (format "(list-declarations \"%s\")" theory)
		    "Listing..." 'listing)))
    (unless pvs-decls
      (error "No declarations in theory %s were found" theory))
    (pvs-make-browse-buffer "Browse" pvs-decls)))

(defpvs unusedby-proof-of-formula browse ()
  "Produce list of declarations unused by the proof of the formula at point

The unusedby-proof-of-formula command creates a \\='Browse\\=' buffer
listing all the declarations that are unused in the proof of the given
formula.  Removing all these declarations and those that follow the
given formula should give a theory that typechecks and for which the
proofchain is still complete, if it was in the full theory."
  (interactive)
  (let* ((fref (pvs-formula-origin))
	 (kind (pvs-fref-kind fref))
	 (fname (pvs-fref-file fref))
	 (lib (pvs-fref-library fref))
	 (buf (pvs-fref-buffer fref))
	 (poff (pvs-fref-prelude-offset fref))
	 (line (+ (pvs-fref-line fref) poff)))
    (if (memq kind '(tccs ppe))
	(message
	 "The unusedby-proof-of-formula command is not available in this buffer.")
	(let ((pvs-decls (pvs-send-and-wait-for-json
			  (format "(unusedby-proof-of-formula \"%s\" \"%s\" %d %s)"
			      (or fname buf) kind line
			      (when lib (format "\"%s\"" lib)))
			  "Collecting..." 'unusedby)))
	  (unless pvs-decls
	    (error "No unused declarations found for formula"))
	  (pvs-make-browse-buffer "Browse" pvs-decls)))))

(defpvs unusedby-proofs-of-formulas browse (formulas theory)
  "Produce list of declarations unused by the proofs of the given formulas

The unusedby-proofs-of-formulas command prompts for a list of formulas (just
hit \\='Return\\=' when done) and a root theory, and creates a \\='Browse\\=' buffer
listing all the declarations that are unused in the proofs of the given
formulas.  Removing all these declarations and those that follow the given
formula should give a theory that typechecks and for which the proofchain is
still complete, if it was in the full theory."
  (interactive
   (append (complete-formula-name-list "Formula: ")
	   (complete-theory-name "Root theory to use as context: ")))
  (let ((pvs-decls (pvs-send-and-wait-for-json
		    (format "(unusedby-proofs-of-formulas '%s \"%s\")"
			(mapcar #'(lambda (x) (format "\"%s\"" x)) formulas)
		      theory)
		    "Collecting..." 'unusedby)))
    (unless pvs-decls
      (error "No unused declarations found for given formulas"))
    (pvs-make-browse-buffer "Browse" pvs-decls)))


;;; Functions to support PVS browse mode

(defun pvs-get-declaration-lines (pvs-decls)
  (let* ((json-array-type 'list)
	 (decl-forms (json-read-from-string pvs-decls))
	 (col-sizes (pvs-get-browse-column-sizes decl-forms))
	 (decl-strings nil))
    (dotimes (i (length decl-forms))
      (let ((decl-form (elt decl-forms i)))
	(let ((declname (cdr (assq 'declname decl-form)))
	      (type (cdr (assq 'type decl-form)))
	      (theoryid (cdr (assq 'theoryid decl-form))))
	  (push (concat (fixed-size-str declname (car col-sizes)) " "
			(fixed-size-str type (cadr col-sizes)) " "
			(fixed-size-str theoryid (caddr col-sizes)))
		decl-strings))))
    (reverse decl-strings)))

(defun pvs-make-browse-buffer (buf-name pvs-decls)
  "pvs-decls is now a JSON string."
  (let* ((buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (pvs-browse-mode)
      (let* ((json-array-type 'list)
	     (decls (json-read-from-string pvs-decls)))
	(setq-local pvs-declarations decls))
      (let ((inhibit-read-only t))
	(erase-buffer)
	(if pvs-declarations
	    (pvs-insert-declarations)
	    (insert "No decls found\n"))
	(goto-char (point-min))
	(forward-line 2)
	(set-buffer-modified-p nil))
      (unless noninteractive
	(pvs-display-browse-buffer buf))
      buf)))

(defun fixed-size-str (str size)
  (cond ((= (length str) size)
	 str)
	((< (length str) size)
	 (concat str (make-string (- size (length str)) ? )))
	(t (concat (substring str 0 (- size 1)) "$"))))

(defun pvs-insert-declarations ()
  (let* ((cols (pvs-get-browse-column-sizes))
	 (ds (car cols))
	 (tys (cadr cols))
	 (ths (caddr cols)))
    (insert (concat (fixed-size-str "Decl" ds) " "
		    (fixed-size-str "Type" tys) " "
		    (fixed-size-str "Thid" ths) "\n\n"))
    (dotimes (i (length pvs-declarations))
      (let* ((decl-form (elt pvs-declarations i))
	     (declname (cdr (assq 'declname decl-form)))
	     (type (cdr (assq 'type decl-form)))
	     (theoryid (cdr (assq 'theoryid decl-form))))
	(insert (concat (fixed-size-str declname ds) " "
			(fixed-size-str type tys) " "
			(fixed-size-str theoryid ths) "\n"))))))

(defun pvs-get-browse-column-sizes (&optional decl-forms)
  ;; pvs-declarations is a buffer-local variable
  (let ((dforms (or decl-forms pvs-declarations))
	(decl-size 4) (type-size 4) (thid-size 4)
	(win-width (max 80 (window-body-width))))
    (unless dforms
      (error "No decl-forms provided"))
    (dotimes (i (length dforms))
      (let ((decl-form (elt dforms i)))
	(let ((declname (cdr (assq 'declname decl-form)))
	      (type (cdr (assq 'type decl-form)))
	      (theoryid (cdr (assq 'theoryid decl-form))))
	  (setq decl-size (max decl-size (length declname))
		type-size (max type-size (length type))
		thid-size (max thid-size (length theoryid))))))
    (when (> (+ decl-size type-size thid-size 3) win-width)
      ;; Unlikely theoryid is the problem
      ;; This is a quick-and-dirty heuristic
      (cond ((< (* 2 decl-size) type-size)
	     (setq type-size (- win-width (+ decl-size thid-size 3))))
	    ((< (* 2 type-size) decl-size)
	     (setq decl-size (- win-width (+ type-size thid-size 3))))
	    (t (let ((size (floor (- win-width (+ thid-size 3)) 2)))
		 (setq decl-size size type-size size)))))
    (list decl-size type-size thid-size)))

(defun pvs-browse-select ()
  "Select this line's declaration in full screen."
  (interactive)
  (unless (string-equal (buffer-name) "Browse")
    (error "The pvs-browse-select command is not available in buffer %s"
	   (buffer-name)))
  (if (<= (current-line-number) 2)
      (error "Please select from list of choices below."))
  (let* ((decl-form (elt pvs-declarations (- (current-line-number) 3)))
	 (declname (cdr (assq 'declname decl-form)))
	 ;; (type (cdr (assq 'type decl-form)))
	 (theoryid (cdr (assq 'theoryid decl-form)))
	 (place (cdr (assq 'place decl-form)))
	 (filename (cdr (assq 'filename decl-form)))
	 (decl-ppstring (cdr (assq 'decl-ppstring decl-form))))
    (cond ((member place '(nil NIL))
	   (let* ((bufname (format "%s.%s" theoryid declname))
		  (buf (get-buffer-create bufname)))
	     (message "")
	     (with-current-buffer buf
	       (let ((inhibit-read-only t))
		 (erase-buffer)
		 (insert decl-ppstring)
		 (set-buffer-modified-p nil))
	       (pvs-view-mode))
	     (pop-to-buffer buf)))
	  (t				;(pvs-browse-quit)
	   (let ((prelude-file (format "%s/lib/prelude.pvs" pvs-path)))
	     (cond ((file-equal filename prelude-file)
		    (let* ((freg (get-prelude-file-and-region theoryid))
			   (line (when freg
				   (save-excursion
				     (let ((noninteractive t)) ;; Shut up about read-only
				       (set-buffer (find-file-noselect (car freg))))
				     (goto-char (cadr freg))
				     (- (current-line-number) 1)))))
		      (view-prelude-theory theoryid)
		      (when line
			(goto-char (point-min))
			(forward-line (1- (- (elt place 0) line)))
			(forward-char (elt place 1)))))
		   (t (find-file filename)
		      (goto-char (point-min))
		      (forward-line (1- (elt place 0)))
		      (forward-char (elt place 1))))))))
  (delete-other-windows)
  (recenter))


(defun pvs-browse-view ()
  "View declaration on current line in Declaration list.
Returns to Declaration List when done."
  (interactive)
  (unless (string-equal (buffer-name) "Browse")
    (error "The pvs-browse-view command is not available in buffer %s"
	   (buffer-name)))
  (if (<= (current-line-number) 2)
      (error "Please select from list of choices below."))
  (let* (;;(cbuf (current-buffer))
	 (buf (get-buffer-create "Browse View"))
	 (decl-form (elt pvs-declarations (- (current-line-number) 3)))
	 ;; (declname (cdr (assq 'declname decl-form)))
	 ;; (type (cdr (assq 'type decl-form)))
	 ;; (theoryid (cdr (assq 'theoryid decl-form)))
	 (decl-ppstring (cdr (assq 'decl-ppstring decl-form))))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert decl-ppstring)
      (set-buffer-modified-p nil))
    (pvs-view-mode)
    (goto-char (point-min))
    (let ((view-window (get-buffer-window-list buf)))
      (cond (view-window
	     (ilisp-shrink-wrap-window (car view-window)))
	    (t
	     (other-window 1)
	     (when (< (ilisp-desired-height buf) (window-height))
	       (split-window-vertically (ilisp-desired-height buf))
	       (set-window-buffer (selected-window) buf)
	       (other-window -1)))))))

(defun pvs-browse-quit ()
  (interactive)
  (remove-buffer (current-buffer))
  (pvs-bury-output)
  (when (and (not pvs-popup-windows)
	     (window-configuration-p pvs-popup-old-window-configuration))
    (set-window-configuration pvs-popup-old-window-configuration)
    (setq pvs-popup-old-window-configuration nil)))

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
  (cond ((featurep 'xemacs)
	 ;; This code is courtesy Jerry James (loganjerry@gmail.com)
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


(defpvs usedby-proofs browse ()
  "Show a list of formulas whose proofs refer to the declaration at point"
  (interactive)
  (let* ((kind (pvs-kind-of-buffer))
	 (file (buffer-file-name)))
    (save-some-pvs-files)
    (pvs-bury-output)
    (let ((pvs-decls (pvs-send-and-wait-for-json
		      (format "(usedby-proofs \"%s\" \"%s\" '%d)"
			  (or file (buffer-name)) kind
			  (current-line-number))
		      "Listing..." 'listing)))
      (when pvs-decls
	(pvs-make-browse-buffer "Browse" pvs-decls)))))

(defvar expanded-form-face 'expanded-form-face)
(make-face 'expanded-form-face)
(set-face-background 'expanded-form-face "violet")
(defvar expanded-form-overlay nil)

(defpvs show-expanded-form browse ()
  "Shows the expanded form of the specified region

The show-expanded-form command displays the expanded form of the given
region.  By default, names from the prelude are not expanded, but with an
argument they are expanded as well."
  (interactive)
  (let* ((kind (pvs-kind-of-buffer))
	 (file (buffer-file-name))
	 (beg (if (mark t) (region-beginning) (point)))
	 (end (if (mark t) (region-end) (point)))
	 (pos1 (save-excursion
		 (goto-char beg)
		 (list (current-line-number) (real-current-column))))
	 (pos2 (save-excursion
		 (goto-char end)
		 (list (current-line-number)
		       (real-current-column)))))
    (save-some-pvs-files)
    (pvs-bury-output)
    (let ((place (pvs-send-and-wait
		  (format "(show-expanded-form \"%s\" \"%s\" '%s '%s %s)"
		      (or file (buffer-name)) kind pos1 pos2
		      (and current-prefix-arg t))
		  nil 'expanded-form 'list)))
      (unless noninteractive
	(when place
	  (let ((tbeg (save-excursion
			(goto-char (point-min))
			(forward-line (car place))
			(forward-char (cadr place))
			(point)))
		(tend (save-excursion
			(goto-char (point-min))
			(forward-line (caddr place))
			(forward-char (cadddr place))
			(point))))
	    (setq expanded-form-overlay (make-overlay tbeg tend))
	    (overlay-put expanded-form-overlay 'face 'expanded-form-face)))))))

(defun pvs-display-browse-buffer (buf)
  "Popup a top level buffer, dependent on the value of pvs-popup-windows"
  (cond ((not pvs-popup-windows)
	 (setq pvs-popup-old-window-configuration (current-window-configuration))
	 (delete-other-windows)
	 (let ((top-window (selected-window))
	       (bottom-window (split-window-vertically (ilisp-desired-height buf))))
	   (set-window-buffer top-window buf)
	   (select-window bottom-window)))
	 ((eq pvs-popup-windows 'frame)
	 (unless (frame-live-p pvs-popup-browse-frame)
	   (setq pvs-popup-browse-frame (make-frame))))
	((eq pvs-popup-windows 'x)
	 (error "pvs-popup-windows as x not yet implemented"))
	(t (error "pvs-popup-windows is not one of nil, 'frame or 'x"))))
