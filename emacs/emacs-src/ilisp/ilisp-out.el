;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-out.el --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.



;;;
;;; ILISP output, including a popper replacement.
;;;

(defvar ilisp-output-buffer " *Output*")
(defvar ilisp-output-buffer-major-mode 'lisp-mode
  "*The major mode for the ilisp typeout window.")
(defvar ilisp-output-min-height 2
  "*The minimum height of the typeout window used to display ilisp output.")
(defvar ilisp-output-max-height 25
  "*The maximum height of the typeout window used to display ilisp output.")
(defvar ilisp-display-output-function 'ilisp-display-output-default
  "The name of a function to display all ilisp output.  The function gets a 
 single argument, a string.")


;; Minor mode (just to get a pretty mode line).
(defvar ilisp-output-mode-line nil)
(defvar ilisp-output-mode nil "If T, then we are in the ilisp-output minor mode.")
(make-variable-buffer-local 'ilisp-output-mode)

(or (assq 'ilisp-output-mode minor-mode-alist)
    (setq minor-mode-alist
	  (cons '(ilisp-output-mode ilisp-output-mode-line) minor-mode-alist)))


(defun ilisp-output-buffer (&optional create-p)
  (let ((buffer (if create-p
		    (get-buffer-create ilisp-output-buffer)
		  (get-buffer ilisp-output-buffer))))
    (or ilisp-output-mode-line
	(setq ilisp-output-mode-line
	      (list (format 
		     " %s bury, %s scroll" 
		     (ilisp-where-is 'ilisp-bury-output)
		     (ilisp-where-is 'ilisp-scroll-output)))))
    buffer))
  
(defun ilisp-output-window ()
  (let ((buffer (get-buffer ilisp-output-buffer)))
    (if buffer
	(get-buffer-window buffer))))


(defun lisp-display-output (output)
  "Display OUTPUT in the appropriate place.
 This calls the function given by the value of ilisp-display-output-function in
 order to do the real work."
  (cond ((null output))
	(t
	 ;; Bugcheck
	 (if (not (stringp output))
	     (error "bug: not a string in lisp-display-output"))

	 (if (ilisp-value 'comint-errorp t)
	     (setq output (funcall (ilisp-value 'ilisp-error-filter)
				   output)))
	 (funcall ilisp-display-output-function output))))



;;; Popper replacement


(defun ilisp-bury-output (&optional buffer)
  "Delete the typeout window, if any"
  (interactive)
  (let* ((buffer (or (and buffer
			  (get-buffer buffer))
		     (ilisp-output-buffer)))
	 (window (and buffer (get-buffer-window buffer))))
    (if buffer
	(bury-buffer buffer))
    (if window
	(ilisp-delete-window window))))


(defun ilisp-show-output (&optional buffer)
  "Make typeout visible, if it is not already."
  (interactive)
  (let ((buffer (or buffer (ilisp-output-buffer))))
    (if buffer
	(ilisp-display-buffer-in-typeout-window buffer))))


(defun ilisp-delete-window (window)
  "Delete a window with minimal redisplay."
  (let ((height (window-height window))
	(lower-window (ilisp-find-lower-window window)))
    (delete-window window)
    (if (and lower-window
	     (not (eq lower-window window)))
	(let ((old-window (selected-window)))
	  (save-excursion
	    (select-window lower-window)
	    (set-buffer (window-buffer))
	    (goto-char (window-start))
	    (vertical-motion (- height))
	    (set-window-start lower-window (point)))
	  (select-window old-window)))))


(defun ilisp-scroll-output (&optional lines)
  "Scroll the typeout-window, if any."
  (interactive "P")
  (let* ((buffer (ilisp-output-buffer))
	 (window (and buffer (get-buffer-window buffer)))
	 (old-window (selected-window)))
    (if window
	(unwind-protect
	     (progn
	       (select-window window)
	       (set-buffer buffer)
	       (scroll-up lines))
	  (select-window old-window)))))


(defun ilisp-grow-output (&optional n)
  "Grow the typeout window by ARG (default 1) lines."
  (interactive "p")
  (let* ((buffer (ilisp-output-buffer))
	 (window (and buffer (get-buffer-window buffer)))
	 (old-window (selected-window)))
    (if window
	(unwind-protect
	  (progn
	    (select-window window)
	    (enlarge-window n))
	  (if (ilisp-window-live-p old-window)
	      (select-window old-window))))))


(defun ilisp-trim-blank-lines ()
  ;; Delete leading blank lines
  (goto-char (point-min))
  (if (looking-at "\n+")
      (replace-match ""))
  ;; Delete trailing blank lines
  (goto-char (point-max))
  (skip-chars-backward "\n")
  (if (< (point) (point-max))
      (delete-region (1+ (point)) (point-max))))


(defun ilisp-write-string-to-buffer (buffer string)
  (save-excursion
    (set-buffer buffer)
    ;; Maybe an option to keep the old output?
    (erase-buffer)
    ;; New: select mode for the output buffer.
    (if (not (eq major-mode ilisp-output-buffer-major-mode))
	(funcall ilisp-output-buffer-major-mode))
    (setq ilisp-output-mode t)
    (princ string buffer)
    (ilisp-trim-blank-lines)
    (goto-char (point-min))))


(defun ilisp-desired-height (buffer-or-window)
  (let ((height
	 (cond ((bufferp buffer-or-window)
		(ilisp-needed-buffer-height buffer-or-window))
	       ((windowp buffer-or-window)
                (ilisp-needed-buffer-height (window-buffer buffer-or-window))))))
    (min (/ (frame-height) 2)
	 height)))

;; A first guess at the height needed to display this buffer.
(defun ilisp-needed-buffer-height (buffer)
  (save-excursion
    (set-buffer buffer)
    (1+ (count-lines (point-min) (point-max)))))
    
;; The height this window must be to display its entire buffer.
(defun ilisp-needed-window-height (window)
  (save-window-excursion
    (select-window window)
    (save-excursion
      (set-buffer (window-buffer))
      (save-excursion 
	(goto-char (point-min))
	;; Any upper bound on the height of an emacs window will do
	;; here.  How about 1000.
	(vertical-motion 1000)))))

(defun ilisp-shrink-wrap-window (window)
  (let ((previously-selected-window (selected-window))
	(buffer (window-buffer window)))
    
    (select-window window)
    (let* ((current-height (window-height window))
	   (desired-height (ilisp-desired-height window))
	   (delta (- desired-height current-height)))
      (enlarge-window delta)
      (set-buffer buffer)
      (goto-char (point-min))
      
      ;; Now repair damage to the window below us, if it still exists.
      (let ((lower-window (ilisp-find-lower-window window)))
	(when lower-window
	  (select-window lower-window)
	  (when (or (> (window-height lower-window)
		       (ilisp-needed-buffer-height (window-buffer lower-window)))
		    (<= (point) (window-start)))
	    (let ((old-point (point)))
	      (goto-char (window-start))
	      (vertical-motion (- desired-height))
	      (set-window-start lower-window (point))
	      (goto-char old-point)))))
      
      ;;(if (not (pos-visible-in-window-p old-point))
      ;;    (recenter 0))))))
      ;; If there was no lower window, then we ought to preserve
      ;; the start of the window above us, if any.

      (if (ilisp-window-live-p previously-selected-window)
	  (select-window previously-selected-window)))))



(defun ilisp-window-live-p (window)
  (let* ((initial-window (selected-window))
	 (win initial-window)
	 (found nil))
    (while win
      (cond ((eq window win)
	     (setq found t
		   win nil))
	    (t
	     (setq win (next-window win 'no))
	     (if (eq win initial-window)
		 (setq win nil)))))
    found))

;; XEmacs change -- window-edges is gone in 19.12+ so use
;; next-vertical-window instead.
(defun ilisp-find-lower-window (window)
  "Find the window directly below us, if any.  This is probably the 
 window from which enlarge-window would steal lines."
  (if (or (not (string-match "XEmacs" emacs-version))
	  (and (= emacs-major-version 19)
	       (< emacs-minor-version 12)))
      (let* ((bottom (nth 3 (window-edges window)))
	     (window* nil)
	     (win window))
	(while (not (eq (setq win (next-window win 'no))
			window))
	  (if (and (= (nth 1 (window-edges win))
		      bottom)
		   (null window*))
	      (setq window* win)))
	window*)
      (next-vertical-window window)))

;; XEmacs change -- There is now a primitive to do this.
(defun ilisp-find-top-left-most-window ()
  "Return the leftmost topmost window on the current screen."
  (if (or (not (string-match "XEmacs" emacs-version))
	  (and (= emacs-major-version 19)
	       (< emacs-minor-version 12)))
      (let* ((window* (selected-window))
	     (edges* (window-edges window*))
	     (win nil)
	     (edges nil)
	     (start-window window*))
	(while (not (eq (setq win (next-window win 'no))
			start-window))
	  (setq edges (window-edges win))
	  (if (or (< (car (cdr edges)) (car (cdr edges*))) ; top
		  (and (= (car (cdr edges)) (car (cdr edges*)))
		       (< (car edges) (car edges*)))) ; left
	      (setq window* win
		    edges* edges)))
	window*)
      (frame-highest-window (selected-frame) 0)))


;; This causes the typeout window to be created by splitting or using the
;; top-left-most window on the current screen.  That is different behavior
;; from the popper, which always split the current window.
(defun ilisp-window-to-use-for-typeout ()
  (ilisp-find-top-left-most-window))

(defun ilisp-display-buffer-in-typeout-window (buffer)
  "Display buffer in a window at the top of the screen."
  (let ((window (get-buffer-window buffer)))
    (when window
      (delete-window window)))

  ;; Otherwise, find a window to split.
  (let* ((top-window (ilisp-window-to-use-for-typeout))
	 (new-window nil)
	 (previously-selected-window (selected-window))
	 (desired-height (ilisp-desired-height buffer)))

    ;; The new window is always the lower one.
    (select-window top-window)

    ;; Always minimize redisplay (except in emacs 18).
    (let ((split-window-keep-point nil))
      ;; If the top window is not big enough to split, commandeer it
      ;; entirely.
      (cond ((> desired-height (- (window-height) window-min-height))
	     (setq new-window top-window))
	    (t
	     (setq new-window (split-window-vertically desired-height)))))
    
    (set-window-buffer top-window buffer)
    ;; The height is already correct, unless there was line wrapping.
    ;; Account for that here.
    (ilisp-shrink-wrap-window top-window)

    ;; Restore selected window.
    (if (eq previously-selected-window top-window)
	(select-window new-window)
	(select-window previously-selected-window))))









;;; Various functions to which to bind ilisp-display-output-function.

;; This function does what ilisp used to do, except that we use the
;; new "popper".

(defun ilisp-display-output-default (output)
  "Dispatch on the value of lisp-no-popper:
 lisp-no-popper = nil:  display output in a typeout window.
 lisp-no-popper = t:  display output in the ilisp buffer
 otherwise: display one-line output in the echo area, multiline output in the ilisp buffer."
  (cond ((null lisp-no-popper)
	 (ilisp-display-output-in-typeout-window output))
	((eq lisp-no-popper t)
	 (ilisp-display-output-in-lisp-listener output))
	(t
	 (ilisp-display-output-in-typeout-window output ))))
;
; davesc
;	 (ilisp-display-output-adaptively output))))
;


;; This is the display function I like to use.

;; Another trick which might be useful is to dispatch on the value
;; this-command here, to make output from different ilisp commands
;; go to different places.

(defun ilisp-display-output-adaptively (output)
  "Display one-liners in the echo area, others in the typeout window"
  (cond ((or (string-match "\n" output)
	     (> (length output) (window-width (minibuffer-window))))
	 (message "See above.")
	 (ilisp-display-output-in-typeout-window output))
	(t
	 (ilisp-display-output-in-echo-area output))))


(defun ilisp-display-output-in-typeout-window (output)
  "Display output in a shrink-wrapped window at the top of the screen."
  (let ((buffer (ilisp-output-buffer t)))
    (ilisp-write-string-to-buffer buffer output)
    (ilisp-display-buffer-in-typeout-window buffer)))


(defun ilisp-display-output-in-echo-area (output)
  "Display output as a message in the echo area."
  ;; First clear any existing typeout so as to not confuse the user.
  (or (eq (selected-window) (ilisp-output-window))
      (ilisp-bury-output))
  
  ;; v5.7: Patch suggested by hunter@work.nlm.nih.gov (Larry Hunter)
  ;; If output contains '%', 'message' loses.
  ;; (message (ilisp-quote-%s output))
  ;; An alternative here could be '(princ output)', as suggested by
  ;; Christopher Hoover <ch@lks.csi.com>
  ;; (princ output)

  ;; v5.7b: Patch suggested by fujieda@jaist.ac.jp (Kazuhiro Fujieda)
  ;; Best one for FSF Emacs 19.2[89].
  (message "%s" output)
  )


;;; ilisp-quote-%s --
;;; Patch suggested by hunter@work.nlm.nih.gov (Larry Hunter)

(defun ilisp-quote-%s (string)
  "Quote all the occurences of ?% in STRING in an ELisp fashion."
  (mapconcat '(lambda (char)
		(if (char-equal char ?%)
		    "%%"
		  (char-to-string char)))
	     string ""))


(defun ilisp-display-output-in-temp-buffer (output)
  (with-output-to-temp-buffer ilisp-output-buffer
    (princ output)))


(defun ilisp-display-output-in-lisp-listener (output)
  "Display output in the ilisp buffer"
  (let ((buffer (current-buffer))
	(window (selected-window)))
    (unwind-protect
	(progn
	  (lisp-pop-to-buffer (ilisp-buffer))
	  (if (not (eq (current-buffer) buffer))
	      (setq ilisp-last-buffer buffer))
	  (comint-insert 
	   (concat 
	    (if ilisp-last-message
		(concat ";;; " ilisp-last-message "\n"))
	    (comint-remove-whitespace output)
	    "\n"
	    ilisp-last-prompt))
	  (setq ilisp-last-message nil))
      (if (window-point window)
	  (progn (select-window window)
		 (set-buffer buffer))))))



;;; Changed according to suggestions by Robert P. Goldman
(defun lisp-pop-to-buffer (buffer)
  "Like pop-to-buffer, but select a screen that buffer was shown in."
  (let ((ilisp-window (if ilisp-epoch-running
			  (epoch::get-buffer-window buffer)
			(get-buffer-window buffer))))
    (if ilisp-window
	(select-window ilisp-window)
      ;; It is not currently displayed, so find some place to display
      ;; it.
      (progn
	(cond (ilisp-epoch-running
	       ;; Select a screen that the buffer has been displayed in before
	       ;; or the current screen otherwise.
	       (epoch::select-screen
		;; allowed-screens in epoch 3.2, was called screens before that
		(or (car (save-excursion
			   (set-buffer buffer)
			   (symbol-value 'allowed-screens)))
		    (epoch::current-screen))))

	      ;; Next clauses patterned after a suggestion by R. P. Goldman.
	      ((memq +ilisp-emacs-version-id+ '(fsf-19 fsf-20))
	       (let* ((window (get-buffer-window buffer t))
		      (frame (if window (window-frame window))))
		 (if (eq 'x (framep frame))
		     (progn
		       (raise-frame frame)
		       (select-frame frame)))))
	      (t nil))			; fsf-18, but also lucid and
					; xemacs.
					; I do not know how to make
					; them work
					; Marco Antoniotti, Jan 4th 1995
	(ilisp-bury-output)
	(pop-to-buffer buffer))))
  (set-buffer buffer))

;(defun lisp-pop-to-buffer (buffer)
;  "Like pop-to-buffer, but select a screen that buffer was shown in.
; Also, first bury any typeout-window."
;  (let ((ilisp-window (if ilisp-epoch-running
;			  (epoch::get-buffer-window buffer)
;			  (get-buffer-window buffer))))
;    (if ilisp-window
;	(select-window ilisp-window)
;	;; It is not currently displayed, so find some place to display it.
;	(if ilisp-epoch-running
;	    ;; Select a screen that the buffer has been displayed in before
;	    ;; or the current screen otherwise.
;	    (epoch::select-screen
;	     ;; allowed-screens in epoch 3.2, was called screens before that
;	     (or (car (save-excursion
;			(set-buffer buffer)
;			(symbol-value 'allowed-screens)))
;		 (epoch::current-screen))))
;	;; Do not pop to the output buffer.
;	(ilisp-bury-output)
;	(pop-to-buffer buffer)))
;  (set-buffer buffer))


;;;
(defun switch-to-lisp (eob-p &optional ilisp-only)
  "If in an ILISP buffer, switch to the buffer that last switched to
an ILISP otherwise, switch to the current ILISP buffer.  With
argument, positions cursor at end of buffer.  If you don't want to
split windows, set pop-up-windows to NIL."
  (interactive "P")
  (if (and (not ilisp-only) ilisp-last-buffer 
	   (memq major-mode ilisp-modes))
      (lisp-pop-to-buffer ilisp-last-buffer)
      (if (not (memq major-mode ilisp-modes))
	  (setq ilisp-last-buffer (current-buffer)))
      (lisp-pop-to-buffer (ilisp-buffer))
      (cond (eob-p (goto-char (point-max))))))
