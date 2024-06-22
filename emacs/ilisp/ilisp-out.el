;;; -*- Mode: Emacs-Lisp; lexical-binding: t -*-

;;; ilisp-out.el --
;;; ILISP output, including a popper replacement.
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.

(declare-function ilisp-where-is "ilisp-key")

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
  (delete-window window))

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
  (with-current-buffer buffer
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
    (if height
	(min (/ (frame-height) 2) height)
	(/ (frame-height) 2))))

;; A first guess at the height needed to display this buffer.
(defun ilisp-needed-buffer-height (buffer)
  (with-current-buffer buffer
    (let ((stdheight (1+ (count-screen-lines (point-min) (point-max)))))
      (if (featurep 'xemacs)
	  (1+ stdheight)
	  stdheight))))
    
;; The height this window must be to display its entire buffer.
(defun ilisp-needed-window-height (window)
  (save-window-excursion
    (select-window window)
    (with-current-buffer (window-buffer)
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

(defun ilisp-display-buffer-in-typeout-window (buffer)
  "Display buffer in a window at the top of the screen."
  (let ((window (get-buffer-window buffer)))
    (when (and window
	       (> (count-windows) 1))
      (delete-window window)))

  ;; Otherwise, find a window to split.
  (let* ((top-window (frame-first-window))
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
 otherwise: display one-line output in the echo area,
            multiline output in the ilisp buffer."
  (cond ((eq lisp-no-popper t)
	 (ilisp-display-output-in-lisp-listener output))
	(t (ilisp-display-output-adaptively output))))
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
  (ilisp-display-output-in-echo-area output))

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
  (mapconcat #'(lambda (char)
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
  (let ((ilisp-window (get-buffer-window buffer)))
    (if ilisp-window
	(select-window ilisp-window)
      ;; It is not currently displayed, so find some place to display
      ;; it.
      (progn
	(cond ;; Next clauses patterned after a suggestion by R. P. Goldman.
	      ((memq +ilisp-emacs-version-id+ '(fsf-19 fsf-20))
	       (let* ((window (get-buffer-window buffer t))
		      (frame (if window (window-frame window))))
		 (if (eq 'x (framep frame))
		     (progn
		       (raise-frame frame)
		       (select-frame frame)))))
	      (t nil))
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
(defun ilisp-switch-to-lisp (eob-p &optional ilisp-only)
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

(unless (fboundp 'count-screen-lines)
(defun count-screen-lines (&optional beg end count-final-newline window)
  "Return the number of screen lines in the region.
The number of screen lines may be different from the number of actual lines,
due to line breaking, display table, etc.

Optional arguments BEG and END default to `point-min' and `point-max'
respectively.

If region ends with a newline, ignore it unless optional third argument
COUNT-FINAL-NEWLINE is non-nil.

The optional fourth argument WINDOW specifies the window used for obtaining
parameters such as width, horizontal scrolling, and so on.  The default is
to use the selected window's parameters.

Like `vertical-motion', `count-screen-lines' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.  This makes possible to use
`count-screen-lines' in any buffer, whether or not it is currently displayed
in some window."
  (unless beg
    (setq beg (point-min)))
  (unless end
    (setq end (point-max)))
  (if (= beg end)
      0
    (save-excursion
      (save-restriction
        (widen)
        (narrow-to-region (min beg end)
                          (if (and (not count-final-newline)
                                   (= ?\n (char-before (max beg end))))
                              (1- (max beg end))
                            (max beg end)))
        (goto-char (point-min))
        (1+ (vertical-motion (buffer-size) window))))))
)
