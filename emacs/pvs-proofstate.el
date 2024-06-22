;; -*- Mode: Emacs-Lisp; lexical-binding: t -*- ;;
;; pvs-proofstate.el -- Shows the proofstate, providing different displays
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

    
(require 'json nil :noerror)

(defvar pvs-in-checker)

(declare-function pvs-mode "pvs-mode")
(declare-function place-to-region "pvs-menu")
(declare-function current-line-number "pvs-mode")

(defvar default-proofstate-display-style nil)
(defvar current-proofstate-display-style nil)
(defvar proofstate-display-styles
  '((no-frame . pvs-proofstate-no-frame)
    (0-frame . pvs-proofstate-0-frame)
    (1-frame . pvs-proofstate-1-frame)
    (2-frame . pvs-proofstate-2-frame)
    (3-frame . pvs-proofstate-3-frame)
    (4-frame . pvs-proofstate-4-frame)))

(defun pvs-proofstate (fname)
  ;; proofstate is:
  ;; ((action . string)     - optional
  ;;  (num-subgoals . int)  - optional (in initial proofstate)
  ;;  (label . string)
  ;;  (comment . string)    - optional
  ;;  (sequent (antecedents . [s-formulas])        - all are optional
  ;;           (succedents . [s-formulas])
  ;;           (hidden-antecedents . [s-formulas])
  ;;           (hidden-succedents . [s-formulas])))
  ;;
  ;; s-formula is:
  ;; ((labels . [strings])
  ;;  (changed . boolean)
  ;;  (formula . string)
  ;;  (names-info . names-info) - see
  (let ((ps (json-read-file fname)))
    (if (listp ps)
	(let* ((commentary (cdr (assq 'commentary ps)))
	       (action (cdr (assq 'action ps)))
	       (num-subgoals (cdr (assq 'num-subgoals ps)))
	       (label (cdr (assq 'label ps)))
	       (comment (cdr (assq 'comment ps)))
	       (sequent (cdr (assq 'sequent ps)))
	       ;;(prframe (get-proofstate-frame))
	       )
	  (pvs-proofstate-display)
	  ;;(select-frame prframe)
	  (display-proofstate-commentary commentary)
	  (when (get-buffer "Proofstate")
	    (with-current-buffer "Proofstate"
	      (erase-buffer)
	      (when action
		(put-text-property 0 (length action)
				   'face 'proofstate-action-face action)
		(insert action)
		(insert "\n"))
	      (when num-subgoals
		(let ((yields (if (= num-subgoals 1)
				  "this simplifies to:"
				  (format "this yields %d subgoals:" num-subgoals))))
		  (put-text-property 0 (length yields)
				     'face 'proofstate-yields-face yields)
		  (insert yields))
		(insert "\n\n"))
	      (let ((labelstr (format "%s :" label)))
		(put-text-property 0 (length labelstr)
				   'face 'proofstate-label-face labelstr)
		(insert labelstr))
	      (when comment
		(put-text-property 0 (length comment)
				   'face 'proofstate-comment-face comment)
		(insert comment))
	      (insert "\n")
	      (insert-proofstate-sequent sequent)
	      (goto-char (point-min)))))
	(finish-proofstate (not (eq ps :json-false))))
    (delete-file fname)))

(defun finish-proofstate (proved)
  (unless (eq current-proofstate-display-style 'no-frame)
    (remove-proofstate-frames))
  (message (if proved "Proved" "Unfinished")))

(defun display-proofstate-commentary (commentary)
  (when (and commentary
	     (get-buffer "Commentary"))
    (let* ((frame-names-alist (make-frame-names-alist))
	   (com-frame (or (cdr (assoc "Proofcommentary" frame-names-alist))
			  (cdr (assoc "Proofstate" frame-names-alist))
			  (cdr (assoc "pvs" frame-names-alist))))
	   ;;(com-windows (window-list com-frame))
	   ;;(com-buffers (mapcar #'window-buffer com-windows))
	   )
    (save-selected-window
      (select-frame com-frame)
      (let ((cwin (get-buffer-window "Commentary" com-frame)))
	(if cwin
	    (select-window cwin)
	    (switch-to-buffer "Commentary"))
	(goto-char (point-max))
	(insert "\n")
	(dotimes (i (length commentary))
	  (let ((comm (elt commentary i)))
	    (when (and (= i 0) (= (elt comm 0) ?\n))
	      (setq comm (cl-subseq comm 1)))
	    (put-text-property 0 (length comm)
			       'face 'proofstate-commentary-face comm)
	    (insert comm)
	    (recenter -1))))))))
	  

(defun insert-proofstate-sequent (sequent)
  (let ((antecedents (cdr (assq 'antecedents sequent)))
	(succedents (cdr (assq 'succedents sequent))))
    (insert-proofstate-formulas antecedents)
    (insert "\n  |-------")
    (insert-proofstate-formulas succedents)))

(defun insert-proofstate-formulas (s-fmlas)
  (when s-fmlas
    (dotimes (i (length s-fmlas))
      (let* ((s-fmla (elt s-fmlas i))
	     (labels (cdr (assq 'labels s-fmla)))
	     (changed (equal (cdr (assq 'changed s-fmla)) "true"))
	     (formula (cdr (assq 'formula s-fmla)))
	     (names-info (cdr (assq 'names-info s-fmla))))
	(insert "\n")
	(let ((labelstr (concat (if changed "{" "[")
				(format "%s" (elt labels 0)))))
	  (when (> (length labels) 1)
	    (dotimes (j (1- (length labels)))
	      (setq labelstr (concat labelstr ", "
				     (elt labels (1+ j))))))
	  (setq labelstr (concat labelstr (if changed "}" "]")))
	  (put-text-property 0 (length labelstr)
			     'face (if changed
				       'proofstate-formula-changed-label-face
				       'proofstate-formula-unchanged-label-face)
			     labelstr)
	  (insert labelstr))
	(insert " ")
	(let ((srow (1- (current-line-number)))
	      (scol (current-column)))
	  (put-text-property 0 (length formula)
			     'face 'proofstate-formula-face
			     formula)
	  (insert formula)
	  (dotimes (i (length names-info))
	    (let* ((delt (elt names-info i))
		   (place (cdr (assq 'place delt)))
		   (region (place-to-region place srow scol))
		   (msg (cdr (assq 'decl delt)))
		   (decl-file (cdr (assq 'decl-file delt)))
		   (decl-place (cdr (assq 'decl-place delt))))
	      (add-text-properties
	       (car region) (cdr region)
	       `(mouse-face highlight help-echo ,msg))
	      (make-text-button
	       (car region) (cdr region)
	       'type 'pvs-decl 'decl-file decl-file 'decl-place decl-place))))))))

;;; Display setup - windows, frames, etc.

;;; Proofstate displays four main windows: Commentary, Proofstate, Proofscript,
;;; and Proof Input.  These can be displayed as different combinations of frames
;;; and windows next-proofstate-display-style cycles through:
;;;  no-frame - original pvs proof interface
;;;   0-frame - puts windows into same frame as pvs
;;;   1-frame - puts windows into one separate frame
;;;   2-frame - proofstate, proofscript, and input in one frame, commentary in another
;;;   3-frame - proofstate and input in one, and proofscript and commentary get own frame
;;;   4-frame - proofstate, proofscript, input, and commentary each get their own frame
;;; Note that proofscript may not be there, in which case it is not displayed
;;; though M-x show-current-proof would then display it.

(defpvs refresh-proofstate prove ()
  "Refreshes the display, so that Commentary, Proofstate, Proof, and *pvs*
windows are displayed properly."
  (interactive)
  (pvs-proofstate-display t))

(defpvs set-proofstate-display-style prove (style)
  (interactive (list (intern (completing-read "Display style: "
			       proofstate-display-styles))))
  (message "Setting proof display style to %s" style)
  (setq default-proofstate-display-style style)
  (setq current-proofstate-display-style style))

(defun pvs-proofstate-display (&optional reset)
  (cl-case current-proofstate-display-style
    (0-frame (pvs-proofstate-0-frame reset))
    (1-frame (pvs-proofstate-1-frame reset))
    (2-frame (pvs-proofstate-2-frame reset))
    (3-frame (pvs-proofstate-3-frame reset))
    (4-frame (pvs-proofstate-4-frame reset))
    (t (pvs-proofstate-no-frame reset))))

(defun remove-proofstate-frames ()
  (let* ((frame-names-alist (make-frame-names-alist))
	 ;; (pvs-frame (or (cdr (assoc "pvs" frame-names-alist))
	 ;; 		;; Should be unlikely, but just take the first one if no pvs
	 ;; 		(cdar frame-names-alist)))
	 (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	 (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	 (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	 (scr-frame (cdr (assoc "Proofscript" frame-names-alist))))
    (when ps-frame (delete-frame ps-frame))
    (when com-frame (delete-frame com-frame))
    (when cmd-frame (delete-frame cmd-frame))
    (when scr-frame (delete-frame scr-frame))))

(defpvs pvs-proofstate-no-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (pvs-frame (or (cdr (assoc "pvs" frame-names-alist))
			  ;; Should be unlikely, but just take the first one if no pvs
			  (cdar frame-names-alist)))
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist)))
	   (doit (or reset (not (eq current-proofstate-display-style 'no-frame))
		     ps-frame com-frame cmd-frame scr-frame)))
      (when doit
	(when ps-frame (delete-frame ps-frame))
	(when com-frame (delete-frame com-frame))
	(when cmd-frame (delete-frame cmd-frame))
	(when scr-frame (delete-frame scr-frame))
	(let* ((windows (window-list pvs-frame))
	       (buffers (mapcar #'window-buffer windows)))
	  (dolist (win windows)
	    (set-window-dedicated-p win nil))
	  (when (member (get-buffer "Proofstate") buffers)
	    (kill-buffer "Proofstate"))
	  (when (member (get-buffer "Commentary") buffers)
	    (kill-buffer "Commentary"))
	  (switch-to-buffer "*pvs*")
	  (delete-other-windows)
	  (when (get-buffer "Proof")
	    (split-window)
	    (other-window 1 nil)
	    (switch-to-buffer "Proof")
	    (other-window 1 nil)
	    (goto-char (point-max))
	    (recenter -1)
	    ;;(dolist (win windows)
	    ;;(set-window-dedicated-p win t))
	    )))
      (setq current-proofstate-display-style 'no-frame))))

(defpvs pvs-proofstate-0-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (pvs-frame (or (cdr (assoc "pvs" frame-names-alist))
			  ;; Unlikely, but just take the first one if no pvs
			  (cdar frame-names-alist)))
	   (pvs-windows (window-list pvs-frame))
	   (pvs-buffers (mapcar #'window-buffer pvs-windows)) ;; Visible buffers
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist)))
	   (doit (or reset (not (eq current-proofstate-display-style '0-frame))
		     ps-frame com-frame cmd-frame scr-frame
		     (not (member (get-buffer "*pvs*") pvs-buffers))
		     (not (member (get-buffer "Proofstate") pvs-buffers))
		     ;; Others may be intensionally buried
		     )))
      (when doit
	(when ps-frame (delete-frame ps-frame))
	(when com-frame (delete-frame com-frame))
	(when cmd-frame (delete-frame cmd-frame))
	(when scr-frame (delete-frame scr-frame))
	(let* ((windows (window-list pvs-frame))
	       (buffers (mapcar #'window-buffer windows)))
	  (dolist (win windows)
	    (set-window-dedicated-p win nil))
	  (unless (and (member (get-buffer "Proofstate") buffers)
		       (member (get-buffer "*pvs*") buffers)
		       (member (get-buffer "Commentary") buffers)
		       (or (null (get-buffer "Proof"))
			   (member (get-buffer "Proof") buffers)))
	    (let ((psbuf (get-buffer-create "Proofstate")))
	      (delete-other-windows)
	      (switch-to-buffer psbuf)
	      ;;(pvs-mode)
	      (split-window nil 5)
	      (switch-to-buffer (get-buffer-create "Commentary"))
	      (other-window 1 nil)
	      (split-window nil -3)
	      (when (get-buffer "Proof")
		(split-window)
		(other-window 1 nil)
		(switch-to-buffer "Proof"))
	      (other-window 1 nil)
	      (switch-to-buffer (get-rule-input-buffer))
	      ;;(dolist (win windows)
	      ;;(set-window-dedicated-p win t))
	      ))))
      (setq current-proofstate-display-style '0-frame))))

(defpvs pvs-proofstate-1-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (pvs-frame (or (cdr (assoc "pvs" frame-names-alist))
			  ;; Unlikely, but just take the first one if no pvs
			  (cdar frame-names-alist)))
	   ;; (pvs-windows (window-list pvs-frame))
	   ;; (pvs-buffers (mapcar #'window-buffer pvs-windows)) ;; Visible buffers
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (ps-windows (when ps-frame (window-list ps-frame)))
	   (ps-buffers (mapcar #'window-buffer ps-windows))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist)))
	   (doit (or reset (not (eq current-proofstate-display-style '1-frame))
		     (null ps-frame) com-frame cmd-frame scr-frame
		     (not (member (get-buffer "*pvs*") ps-buffers))
		     (not (member (get-buffer "Proofstate") ps-buffers))
		     ;; Others may be intensionally buried
		     )))
      (when doit
	(unless ps-frame 
	  (setq ps-frame (make-proofstate-frame)))
	(when com-frame (delete-frame com-frame))
	(when cmd-frame (delete-frame cmd-frame))
	(when scr-frame (delete-frame scr-frame))
	(when (get-buffer "Proofstate")
	  (delete-windows-on "Proofstate" pvs-frame))
	(when (get-buffer "Proof")
	  (delete-windows-on "Proof" pvs-frame))
	(when (get-buffer "Commentary")
	  (delete-windows-on "Commentary" pvs-frame))
	;; Now want com-buffer at top, proofstate middle, cmd bottom
	(let* ((windows (window-list ps-frame))
	       (buffers (mapcar #'window-buffer windows)))
	  (dolist (win windows)
	    (set-window-dedicated-p win nil))
	  (unless (and (member (get-buffer "Proofstate") buffers)
		       (member (get-buffer "*pvs*") buffers)
		       (member (get-buffer "Commentary") buffers)
		       (or (null (get-buffer "Proof"))
			   (member (get-buffer "Proof") buffers)))
	    (select-frame ps-frame)
	    (let ((psbuf (get-buffer-create "Proofstate")))
	      (delete-other-windows)
	      (switch-to-buffer psbuf)
	      (split-window nil 5)
	      (switch-to-buffer (get-buffer-create "Commentary"))
	      (other-window 1 nil)
	      (split-window nil -3)
	      (when (get-buffer "Proof")
		(split-window)
		(other-window 1 nil)
		(switch-to-buffer "Proof"))
	      (other-window 1 nil)
	      (switch-to-buffer (get-rule-input-buffer))
	      ;;(dolist (win windows)
	      ;;(set-window-dedicated-p win t))
	      ))))
      (setq current-proofstate-display-style '1-frame))))

(defpvs pvs-proofstate-2-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (pvs-frame (or (cdr (assoc "pvs" frame-names-alist))
			  ;; Unlikely, but just take the first one if no pvs
			  (cdar frame-names-alist)))
	   ;; (pvs-windows (window-list pvs-frame))
	   ;; (pvs-buffers (mapcar #'window-buffer pvs-windows)) ;; Visible buffers
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (ps-windows (when ps-frame (window-list ps-frame)))
	   (ps-buffers (mapcar #'window-buffer ps-windows))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist)))
	   (doit (or reset (not (eq current-proofstate-display-style '2-frame))
		     (null ps-frame) (null com-frame) cmd-frame scr-frame
		     (not (member (get-buffer "*pvs*") ps-buffers))
		     (not (member (get-buffer "Proofstate") ps-buffers))
		     ;; Others may be intensionally buried
		     )))
      (when doit
	(unless ps-frame 
	  (setq ps-frame (make-proofstate-frame)))
	(when cmd-frame (delete-frame cmd-frame))
	(when scr-frame (delete-frame scr-frame))
	(delete-windows-on "Proofstate" pvs-frame)
	(delete-windows-on "Proof" pvs-frame)
	(when (get-buffer "Commentary")
	  (delete-windows-on "Commentary" pvs-frame)
	  (delete-windows-on "Commentary" ps-frame))
	;; Now want separate commentary & proofstate frames
	(let* ((com-frame (or com-frame (make-proofcommentary-frame)))
	       (com-windows (window-list com-frame))
	       (com-buffers (mapcar #'window-buffer com-windows)))
	  (unless (member (get-buffer "Commentary") com-buffers)
	    (let ((combuf (get-buffer-create "Commentary")))
	      (select-frame com-frame)
	      (delete-other-windows)
	      (switch-to-buffer combuf)
	      (dolist (win (window-list com-frame))
		(set-window-dedicated-p win t)))))
	;; Now the proofstate frame
	(let* ((ps-frame (or ps-frame (make-proofstate-frame)))
	       (ps-windows (window-list ps-frame))
	       (ps-buffers (mapcar #'window-buffer ps-windows)))
	  ;; Proofstate at top, cmd bottom of ps-frame
	  (select-frame ps-frame)
	  (unless (and (member (get-buffer "Proofstate") ps-buffers)
		       (member (get-buffer "*pvs*") ps-buffers)
		       (or (null (get-buffer "Proof"))
			   (member (get-buffer "Proof") ps-buffers)))
	    (let ((psbuf (get-buffer-create "Proofstate")))
	      (delete-other-windows)
	      (switch-to-buffer psbuf)
	      ;;(pvs-mode)
	      (split-window nil -3)
	      (when (get-buffer "Proof")
		(split-window)
		(other-window 1 nil)
		(switch-to-buffer "Proof"))
	      (other-window 1 nil)
	      (switch-to-buffer (get-rule-input-buffer))
	      ;;(dolist (win ps-windows)
	      ;;(set-window-dedicated-p win t)))))
	      ;;(select-frame-set-input-focus ps-frame)
	      ))))
      (setq current-proofstate-display-style '2-frame))))

(defpvs pvs-proofstate-3-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist))))
      (when scr-frame (delete-frame scr-frame))
      (unless (or reset
		  (and (eq current-proofstate-display-style '3-frame)
		       ps-frame com-frame cmd-frame))
	(setq com-frame (or com-frame (make-proofcommentary-frame)))
	(setq ps-frame (or ps-frame (make-proofstate-frame)))
	(setq cmd-frame (or cmd-frame (make-proofcommand-frame)))
	(let* ((ps-windows (window-list ps-frame))
	       (ps-buffers (mapcar #'window-buffer ps-windows))
	       (cmd-windows (window-list cmd-frame))
	       (cmd-buffers (mapcar #'window-buffer cmd-windows))
	       (com-windows (window-list com-frame))
	       (com-buffers (mapcar #'window-buffer com-windows)))
	  (unless (member (get-buffer "Proofcommentary") com-buffers)
	    (let ((combuf (get-buffer-create "Commentary")))
	      (select-frame com-frame)
	      (delete-other-windows)
	      (switch-to-buffer combuf)
	      (dolist (win (window-list com-frame))
		(set-window-dedicated-p win t))))
	  (unless (member (get-buffer "Proofstate") ps-buffers)
	    (let ((psbuf (get-buffer-create "Proofstate")))
	      (select-frame ps-frame)
	      (delete-other-windows)
	      (switch-to-buffer psbuf)
	      (pvs-mode)
	      (dolist (win (window-list ps-frame))
		(set-window-dedicated-p win t))))
	  (unless (member (get-buffer "Rule Input") cmd-buffers)
	    (let ((cmdbuf (get-rule-input-buffer)))
	      (select-frame cmd-frame)
	      (delete-other-windows)
	      (switch-to-buffer cmdbuf)
	      (cl-dolist (win (window-list cmd-frame))
		(set-window-dedicated-p win t)))))
	(select-frame-set-input-focus cmd-frame)))
    (setq current-proofstate-display-style '2-frame)))

(defpvs pvs-proofstate-4-frame prove (&optional reset)
  (interactive)
  (when pvs-in-checker
    (let* ((frame-names-alist (make-frame-names-alist))
	   (ps-frame (cdr (assoc "Proofstate" frame-names-alist)))
	   (com-frame (cdr (assoc "Proofcommentary" frame-names-alist)))
	   (cmd-frame (cdr (assoc "Proofcommand" frame-names-alist)))
	   (scr-frame (cdr (assoc "Proofscript" frame-names-alist))))
      (unless (or reset
		  (and (eq current-proofstate-display-style '4-frame)
		       ps-frame com-frame cmd-frame))
	(setq com-frame (or com-frame (make-proofcommentary-frame)))
	(setq ps-frame (or ps-frame (make-proofstate-frame)))
	(setq cmd-frame (or cmd-frame (make-proofcommand-frame)))
	(setq scr-frame (or scr-frame (make-proofscript-frame)))
	(let* ((ps-windows (window-list ps-frame))
	       (ps-buffers (mapcar #'window-buffer ps-windows))
	       (cmd-windows (window-list cmd-frame))
	       (cmd-buffers (mapcar #'window-buffer cmd-windows))
	       (com-windows (window-list com-frame))
	       (com-buffers (mapcar #'window-buffer com-windows))
	       (scr-windows (window-list scr-frame))
	       (scr-buffers (mapcar #'window-buffer scr-windows)))
	  (unless (member (get-buffer "Proofcommentary") com-buffers)
	    (let ((combuf (get-buffer-create "Commentary")))
	      (select-frame com-frame)
	      (delete-other-windows)
	      (switch-to-buffer combuf)
	      (dolist (win (window-list com-frame))
		(set-window-dedicated-p win t))))
	  (unless (member (get-buffer "Proofstate") ps-buffers)
	    (let ((psbuf (get-buffer-create "Proofstate")))
	      (select-frame ps-frame)
	      (delete-other-windows)
	      (switch-to-buffer psbuf)
	      (pvs-mode)
	      (dolist (win (window-list ps-frame))
		(set-window-dedicated-p win t))))
	  (unless (member (get-buffer "Rule Input") cmd-buffers)
	    (let ((cmdbuf (get-rule-input-buffer)))
	      (select-frame cmd-frame)
	      (delete-other-windows)
	      (switch-to-buffer cmdbuf)
	      (cl-dolist (win (window-list cmd-frame))
		(set-window-dedicated-p win t))))
	  (unless (member (get-buffer "Proof") scr-buffers)
	    (let ((scrbuf (get-buffer "Proof")))
	      (select-frame scr-frame)
	      (delete-other-windows)
	      (switch-to-buffer scrbuf)
	      (cl-dolist (win (window-list scr-frame))
		(set-window-dedicated-p win t)))))
	(select-frame-set-input-focus cmd-frame)))
    (setq current-proofstate-display-style '4-frame)))

(defun get-rule-input-buffer ()
  (get-buffer "*pvs*")
  ;; Doesn't seem to work with ilisp
  ;;(or (get-buffer "Rule Input")
    ;;  (make-indirect-buffer "*pvs*" "Rule Input" t))
  )

(defun make-proofstate-frame ()
  (make-frame '((name . "Proofstate")
		(title . "Proofstate")
		(tool-bar-lines . 0)
		(menu-bar-lines . 0)
		(unsplittable . t))))

(defun make-proofcommentary-frame ()
  (make-frame '((name . "Proofcommentary")
		(title . "Proof Commentary")
		(tool-bar-lines . 0)
		(menu-bar-lines . 0)
		(width . 80)
		(height . 20)
		(unsplittable . t))))

(defun make-proofcommand-frame ()
  (make-frame '((name . "Proofcommand")
		(title . "Rule Input")
		(tool-bar-lines . 0)
		(menu-bar-lines . 0)
		(width . 80)
		(height . 5)
		(unsplittable . t))))

(defun make-proofscript-frame ()
  (make-frame '((name . "Proofscript")
		(title . "Proof")
		(tool-bar-lines . 0)
		(menu-bar-lines . 0)
		(width . 80)
		(height . 40)
		(unsplittable . t))))

;; (defun get-proofstate-frame ()
;;   (or (dolist (fr (frame-list))
;; 	(when (equal (frame-parameter fr 'name) "Proofstate")
;; 	  (return fr)))
;;       (make-frame '((name . "Proofstate")
;; 		    (title . "Proofstate")
;; 		    (tool-bar-lines . 0)
;; 		    (menu-bar-lines . 0)
;; 		    (width . 80)
;; 		    (height . 50)
;; 		    (unsplittable . t)
;; 		    )

(defun next-proofstate-display-style ()
  (interactive)
  (let* ((curmem (assoc current-proofstate-display-style
			proofstate-display-styles))
	 (next (or (cadr (member curmem proofstate-display-styles))
		   (car proofstate-display-styles))))
    (if pvs-in-checker
	(funcall (cdr next))
	(message "Not in proof checker, setting display style to %s"
		 (setq current-proofstate-display-style (car next))))))
