;;; -*-Emacs-Lisp-*-
;;;
;;;
;;;%Header
;;;
;;; Rcs_Info: comint-ipc.el,v 1.20 1993/09/03 02:05:07 ivan Rel $
;;;
;;; IPC extensions for comint
;;; Copyright (C) 1990 Chris McConnell, ccm@cs.cmu.edu.
;;;
;;; Send mail to ilisp@naggum.no if you have problems.
;;;
;;; Send mail to ilisp-request@naggum.no if you want to be on the
;;; ilisp mailing list.

;;; This file is part of GNU Emacs.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY.  No author or distributor
;;; accepts responsibility to anyone for the consequences of using it
;;; or for whether it serves any particular purpose or works at all,
;;; unless he says so in writing.  Refer to the GNU Emacs General Public
;;; License for full details.

;;; Everyone is granted permission to copy, modify and redistribute
;;; GNU Emacs, but only under the conditions described in the
;;; GNU Emacs General Public License.   A copy of this license is
;;; supposed to have been given to you along with GNU Emacs so you
;;; can know your rights and responsibilities.  It should be in a
;;; file named COPYING.  Among other things, the copyright notice
;;; and this notice must be preserved on all copies.

;;; This file contains extensions to multiplex the single channel of
;;; an inferior process between multiple purposes.  It provides both
;;; synchronous and asynchronous sends with error handling.  

;;; USAGE: Load this file and call comint-setup-ipc in a comint
;;; buffer.  This is not a standalone application.  For an example of
;;; it being used see ilisp.el.

;;; CUSTOMIZATION: See the parameters and hooks below.  

;;; INTERFACE.  See the function documentation and code for more information.
;;;
;;; PROCESS INPUT: comint-send, comint-send-code, comint-default-send,
;;; comint-sync, comint-abort-sends
;;;
;;; PROCESS OUTPUT: comint-display-output, comint-display-error-output


;;;%Parameters
(defvar comint-log nil
  "If T, then record all process input and output in a buffer called
process name.")

(defvar comint-send-newline t 
  "If T then add a newline to string in comint-default-send.")

(defvar comint-always-scroll nil
  "If T then process output will always be visible in first window on buffer.")

(defvar comint-fix-error nil
  "String to send to send to the command interpreter to fix errors.")

(defvar comint-continue nil
  "String to send to continue an interrupted job.")

(defvar comint-interrupt-regexp nil
  "Regular expression for the start of an interrupt in process output.")

(defvar comint-error-regexp nil
  "Regular expression for setting comint-errorp if found in process output.")

(defvar comint-output-buffer " *Output*"
  "Name of the output buffer.")

(defvar comint-error-buffer " *Error Output*" 
  "Name of the error output buffer.")

(defvar comint-show-status t
  "Set to nil to inhibit status redisplay.")

;;;%%Hooks
(defvar comint-output-filter (function identity)
  "Given the complete OUTPUT of a send, return the result of the send.")

(defvar comint-interrupt-start 'comint-interrupt-start
  "Return the start in OUTPUT of the text printed by
comint-interrupt-subjob in the inferior process.")

(defvar comint-handler 'comint-error-popup
  "Default handler for sends.  When a send completes, the handler is
called with error-p, wait-p, message, output and prompt.")

(defvar comint-update-status 'comint-update-status
  "Function to update the STATUS of the inferior process.  It should
set comint-status to a status string in addition to whatever else it
does.")

(defvar comint-prompt-status 'comint-prompt-status
  "Given the previous prompt and the last line output, return 'error
if an error, T if a prompt and nil otherwise.  If it is a prompt, also
funcall comint-update-status to set the status.  If old is nil, then
just return T if last line is a prompt.")

;;;
(defvar comint-abort-hook nil 
  "List of hooks to run after sends are aborted.")

;;;%Globals
(defvar comint-send-queue nil 
  "List of currently pending IPC send requests.  The first element in
the queue is where output to the process will be stored.
A send record is a list of: 

string -- The string sent to the process.

no-insert -- nil to insert output into the process buffer.  If this is
being done, the results will only contain the very last line.

wait-p -- nil if not waiting, non-nil if waiting.  If it is a string,
results are inserted in the buffer until a result matches the string
as a regexp.

status -- A symbol for the process status while the send is running.

message -- A message to be displayed when an asynchronous send is
popped up by the handler.

handler -- A function that given error-p, wait-p, message, output and
prompt decides if the user should be notified.  If it is nil or
returns nil, then no error processing will be done.

running -- nil if a send is waiting, T if it is running, another send
if interrupting and a string with pending output if the send was
interrupted.

old-prompt -- The prompt before the send was sent.  If it is nil, then
errors will not be detected.

line -- The start of the last line in the results.

result -- Cons of the output and the prompt after the send.")

(defvar comint-end-queue nil "Pointer to the end of comint-send-queue.")
(defvar comint-queue-emptied t 
  "Set to T each time send queue empties.")

(defvar comint-output nil
  "Set to the output of the last send.  This is useful when ilisp code
is put in the send stream.")
(defvar comint-errorp nil
  "Set to T if the last send was an error.")

(defvar comint-status " :run" "The current comint status.")
(defvar comint-original-buffer nil 
  "The original buffer when there was output to a comint buffer.")

(defvar comint-last-send nil "Last send that was put in queue.")

(defvar comint-aborting nil
  "Set to T if we are aborting commands.")

;;;%Utils
;;;
(defun comint-remove-whitespace (string)
  "Remove leading and trailing whitespace in STRING."
  (if string
      (let* ((start (if (string-match "[^ \t\n]" string)
			(match-beginning 0)
			0))
	     (end start))
	(while (string-match "[ \t\n]*[^ \t\n]+" string end)
	  (setq end (match-end 0)))
	(substring string start end))))

;;;
;(defun comint-log (process string &optional output)
;  "Log to PROCESS, STRING marking as optional OUTPUT."
;  (if comint-log
;      (save-excursion
;	(set-buffer (get-buffer-create (process-name process)))
;	(goto-char (point-max))
;	(if output
;	    (progn
;	      (insert "{") (insert string) (insert "}"))
;	    (insert string)))))

(defvar comint-log-size 15000)
(defvar comint-log-verbose t)
(defvar comint-noise-regexp
  "> \\|(ILISP:ilisp-restore)\\|NIL\\|^[\n\t ]*$")

(defun comint-log (process string &optional output)
  "Log to PROCESS, STRING marking as optional OUTPUT."
  (if (and comint-log
	   (or comint-log-verbose
	       (not (string-match comint-noise-regexp string))))
      (save-excursion
	(set-buffer (get-buffer-create (process-name process)))
	(goto-char (point-max))
	(if output
	    (progn
	      (insert "{") (insert string) (insert "}"))
	    (insert string))
	(if (> (buffer-size) comint-log-size)
	    (delete-region (point-min) (- (buffer-size) comint-log-size))))))


;;; v5.7b Removed by suggestion of erik@naggum.no (Erik Naggum).

;;; (defun comint-send-string (proc str)
;;;   "Send PROCESS the contents of STRING as input.
;;; This is equivalent to process-send-string, except that long input strings
;;; are broken up into chunks of size comint-input-chunk-size. Processes
;;; are given a chance to output between chunks. This can help prevent
;;; processes from hanging when you send them long inputs on some OS's."
;;;   (comint-log proc str)
;;;   (let* ((len (length str))
;;; 	 (i (min len comint-input-chunk-size)))
;;;     (process-send-string proc (substring str 0 i))
;;;     (while (< i len)
;;;       (let ((next-i (+ i comint-input-chunk-size)))
;;; 	(accept-process-output)
;;; 	(process-send-string proc (substring str i (min len next-i)))
;;; 	(setq i next-i)))))

;;; v5.7b See above
;(defun comint-sender (process string)
;  "Send to PROCESS STRING with newline if comint-send-newline."
;  ;; (comint-send-string process string)
;  (process-send-string process string)
;  (if comint-send-newline
;      (progn
;	(comint-log process "\n")
;	(process-send-string process "\n"))))

(defun comint-sender (process string)
  "Send to PROCESS STRING with newline if comint-send-newline."
  (if comint-send-newline
      (process-send-string process (concat string "\n"))
      (process-send-string process string)))

;;;
(defun comint-interrupt-subjob ()
  "Interrupt the current subjob."
  (interactive)
  (comint-log (get-buffer-process (current-buffer)) "")
  (interrupt-process nil comint-ptyp))

;;;
(defun comint-send-variables (send)
  "Return a pointer to the start of the variables for SEND.  It
returns \(running old-prompt line \(output . prompt))."
  (cdr (cdr (cdr (cdr (cdr (cdr send)))))))

;;;
(defun comint-send-results (send)
  "Return the results of SEND which are \(output . prompt).  If there is
an error, the prompt will be a list."
  (car (cdr (cdr (cdr (comint-send-variables send))))))

;;;
(defun comint-send-description (send)
  "Return a description of SEND."
  (let* ((status (cdr (cdr (cdr send)))))
    (or (car (cdr status))		;Message
	(and (stringp (car send)) (car send)) ;String
	(and (car status) (symbol-name (car status))))))
  
;;;
(defun comint-interrupted ()
  "Return T if there is an interrupted send."
  (let ((send comint-send-queue)
	(done nil))
    (while (and send (not done))
      (if (stringp (car (comint-send-variables (car send))))
	  (setq done t)
	  (setq send (cdr send))))
    done))
      

;;;%Default hooks
(defun comint-process-sentinel (process status)
  "Update PROCESS STATUS by funcalling comint-update-status."
  (setq status (process-status process))
  (save-excursion
    (if (buffer-name (process-buffer process))
	(set-buffer (process-buffer process)))
    (funcall comint-update-status status)))

;;;
(defun comint-interrupt-start (output)
  "Return the start of comint-interrupt-regexp in OUTPUT."
  (if (and comint-interrupt-regexp 
	   (string-match comint-interrupt-regexp output))
      (match-beginning 0)))

;;;
(defun comint-update-status (status)
  "Update the process STATUS of the current buffer."
  (setq comint-status (format " :%s" status))
  (if comint-show-status
      (progn
	(save-excursion (set-buffer (other-buffer)))
	(sit-for 0))))

;;;
(defun comint-prompt-status (old line &optional equal)
  "Called by comint-process filter with OLD and LINE, return 'error if
LINE is an error, T if it is a prompt as determined by
comint-prompt-regexp or nil otherwise.  Also set the status
appropriately by funcalling comint-update-status.  If specified EQUAL
will be called with old and line and should return T if line is not an
error.  OLD will be nil for the first prompt."
  (if (string-match comint-prompt-regexp line)
      (let ((error (or (if equal
			   (funcall equal old line)
			   (or (null old) (string-equal old line)))
		       'error)))
	(funcall comint-update-status (if (eq error 'error) error 'ready))
	error)
      nil))

;;;
(defun comint-insert (output)
  "Insert process OUTPUT into the current buffer."
  (if output
      (let* ((buffer (current-buffer))
	     (process (get-buffer-process buffer))
	     (mark (process-mark process))
	     (window (selected-window))
	     (at-end nil))
	(if (eq (window-buffer window) buffer)
	    (setq at-end (= (point) mark))
	    (setq window (get-buffer-window buffer)))
	(save-excursion
	  (goto-char mark)
	  (insert output)
	  (set-marker mark (point)))
	(if window 
	    (progn
	      (if (or at-end comint-always-scroll) (goto-char mark))
	      (if (not (pos-visible-in-window-p (point) window))
		  (let ((original (selected-window)))
		    (save-excursion
		      (select-window window)
		      (recenter '(center))
		      (select-window original)))))))))

;;;
(defun comint-handle-error (output prompt keys &optional delay)
  "Handle an error by beeping, displaying OUTPUT and then waiting for
the user to pause.  Once there is pause, PROMPT until one of the
characters in KEYS is typed.  If optional DELAY is specified, it is
the number of seconds that the user must pause.  The key found will be
returned."
  (save-excursion
    (setq delay (or delay 1))
    (beep t)
    (comint-display-error output)
    (set-buffer comint-original-buffer)
    (unless (memq +ilisp-emacs-version-id+ '(xemacs-20 xemacs-19))
      (while (not (sit-for delay nil))
	(execute-kbd-macro (read-key-sequence nil))))
    (if (not (get-buffer-window (get-buffer comint-error-buffer)))
	(comint-display-error output))
    (let ((cursor-in-echo-area t)
	  (echo-keystrokes 0)
	  char)
      (while (progn (message prompt)
		    (let ((event (read-event)))
		      (not (memq (setq char (if (integerp event)
						(downcase event)
						0))
				 keys))))
	(if (= char ? ) 
	    (ilisp-scroll-output)
	    (setq quit-flag nil)
	    (beep)))
      char)))

;;;
(defun comint-error-popup (error wait-p message output prompt)
  "If there is an ERROR pop up a window with MESSAGE and OUTPUT.
Nothing is done with PROMPT or WAIT-P."
  (if error
      (save-excursion
	(with-output-to-temp-buffer comint-output-buffer
	  (set-buffer comint-output-buffer)
	  (if message (insert message))
	  (insert ?\n)
	  (insert output)
	  (beep t))))
  t)

;;;
(defun comint-process-filter (process output)
  "Filter PROCESS OUTPUT.  See comint-send for more information.  The
first element of the comint-send-queue is the current send entry.  If
the entry has a nil no-insert flag, insert the results into the
process buffer.

If the send is an interrupt, comint-interrupt-start is funcalled on
the output and should return the start of the output of an interrupt.

comint-prompt-status is called with the old prompt and the last line.
It should return 'error if the last line is an error, T if it is a
prompt and nil otherwise.  It should also update the process status by
funcalling comint-update-status.

If there is a send handler, it is called with \(error-p wait-p message
output prompt) and should determine what sort of notification is
appropriate and return T if errors should be fixed and NIL otherwise.

If the prompt is an error, then comint-fix-error will be sent to fix
the error. 

When there is a prompt in the output stream, the next send will be
dispatched unless the wait flag for the send is a string.  If it is a
string, then results will be discarded until one matches the string as
a regexp.

Output to the process should only be done through the functions
comint-send or comint-default-send, or results will be mixed up."
  (let* ((inhibit-quit t)
	 (window (selected-window))
	 (comint-original-buffer (prog1 (current-buffer)
				   (set-buffer (process-buffer process))))
	 (match-data (match-data))
	 (send (car comint-send-queue))
	 (no-insert (cdr send))
	 (wait-p (cdr no-insert))
	 (messagep (cdr (cdr wait-p)))
	 (handler (cdr messagep))
	 (running (cdr handler))
	 (old-prompt (cdr running))
	 (line (cdr old-prompt))
	 (result (car (cdr line)))
	 (old-result (car result))
	 (no-insert (car no-insert))
	 (message (car messagep))
	 (wait-p (car wait-p))
	 (sync (stringp wait-p)))
    ;;; (comint-log process output t)   ;; leave logging up to PVS
    ;; Remove leading whitespace
    (if (and (null old-result)
	     (save-excursion (goto-char (process-mark process)) (bolp))
	     (eq (string-match "[ \t]*\n" output) 0))
	(setq output (substring output (match-end 0))))
    (rplaca result (concat old-result output))
    (while (string-match "\n" (car result) (car line))
      (rplaca line (match-end 0)))
    (if (not (or sync no-insert))
	(progn
	  (comint-insert output)
	  ;; Throw away output if storing in buffer
	  (rplaca result (substring (car result) (car line)))
	  (rplaca line 0)))
    (if (consp (car running))		;Waiting for interrupt
	(let ((split (funcall comint-interrupt-start (car result))))
	  (if split
	      (let ((interrupted (car running)))
		;; Store output to previous send
		(rplaca (comint-send-variables interrupted) 
			(substring (car result) 0 split))
		(rplaca result (substring (car result) (car line)))
		(rplaca line 0)
		(rplaca running t)))))
    (if (not (consp (car running)))	;Look for prompt
	(let* ((last (substring (car result) (car line)))
	       (is-prompt
		(funcall comint-prompt-status (car old-prompt) last)))
	  (if is-prompt
	      (let* ((output
		      (if (or no-insert sync)
			  (funcall comint-output-filter 
				   (substring (car result) 0 (car line)))))
		     (handler (car handler))
		     (error (eq is-prompt 'error)))
		(setq old-result (car result))
		(rplaca result output)
		(rplacd result (if error (list last) last))
		(setq comint-output (car result)
		      comint-errorp 
		      (or error
			  (and comint-error-regexp
			       comint-output
			       (string-match comint-error-regexp
					     comint-output))))
		(unwind-protect
		     ;; (if handler
		     ;;	    (setq handler
		     ;;		 (funcall handler comint-errorp wait-p
		     ;;		          message output last)))

		     ;; v5.7b Patch suggested by fujieda@jaist.ac.jp
		     ;; (Kazuhiro Fujieda). Here is his comment.

		     ;; "When the 'handler' is called, the current
		     ;; buffer may be changed. 'comint-process-filter'
		     ;; accesses some buffer-local variables, for
		     ;; example 'comint-send-queue' and
		     ;; 'comint-end-queue'.  If the current buffer is
		     ;; changed in the 'handler', the entities of
		     ;; these buffer-local variables is replaced, and
		     ;; corrupt successive behaviors."

		     ;; The code hereafter fixes the problem.

		     (if handler
			 (save-excursion
			   (setq handler
				 (funcall handler comint-errorp wait-p
					  message output last))))

		  (if (and error handler no-insert comint-fix-error)
		      (setq comint-send-queue 
			    (cons (list comint-fix-error t nil 'fix
					"Fixing error" nil
					nil nil 0 (cons nil nil))
				  ;; We may have aborted
				  (or (cdr comint-send-queue)
				      comint-send-queue))))
		  (if sync
		      (let ((match (string-match wait-p old-result)))
			(if match
			    (progn
			      (rplaca
			       (cdr (cdr (cdr (cdr (car comint-end-queue)))))
			       "Done")
			      (if (not no-insert)
				  (comint-insert 
				   (concat 
				    (substring old-result 0 match)
				    (substring old-result (match-end 0)))))
			      (rplaca result (substring old-result
							match (car line)))
			      (rplaca messagep "Done")
			      (rplaca running nil)
			      (comint-dispatch-send process))))
		      ;; Not waiting
		      (rplaca messagep "Done")
		      (rplaca running nil)
		      (comint-dispatch-send process))))
	      (rplacd result nil))))
    (store-match-data match-data)
    (set-buffer comint-original-buffer)))

;;;
(defun comint-dispatch-send (process)
  "Dispatch the next send in PROCESS comint-send-queue, popping the
current send if done."
  (let* ((send (car comint-send-queue))
	 (results (comint-send-results send))
	 (prompt (cdr results)))
    ;; Never pop the last record
    (cond ((and (null comint-send-queue) ; Catch a bug.
		(null comint-end-queue)))

	  ((eq comint-send-queue comint-end-queue)
	   (let ((init (car send))
		 (running (comint-send-variables send)))
	     (setq comint-queue-emptied t)
	     ;; Set old prompt to prompt
	     (if prompt
		 (rplaca (cdr (comint-send-variables send)) 
			 (if (consp prompt) (car prompt) prompt)))
	     (rplaca send nil)
	     (if init
		 (funcall init)
	       (if (stringp (car running))
		   ;; Continue if interrupted.  There is no way to
		   ;; sense if the interrupted command actually
		   ;; started, so it is possible that a command will
		   ;; get lost.  
		   (progn (funcall comint-update-status 
				   (car (cdr (cdr (cdr send)))))
			  (comint-sender process comint-continue)
			  (comint-process-filter process (car running))
			  (rplaca running t))))))
	  (t
	   (if prompt
	       ;; Pop
	       (setq comint-send-queue (cdr comint-send-queue)
		     send (car comint-send-queue))
	     ;; Set prompt to top-level prompt
	     (setq prompt (cdr (comint-send-results (car comint-end-queue)))))
	   (let* ((top-level (eq comint-send-queue comint-end-queue))
		  (string (car send))
		  (no-insert (cdr send))
		  (wait-p (cdr no-insert))
		  (status (cdr wait-p))
		  (message (cdr status))
		  (status (car status))
		  (no-insert (car no-insert))
		  (message (car message))
		  (running (comint-send-variables send)))
	     (if top-level
		 (rplaca send nil)
	       (if (stringp string) (funcall comint-update-status status)))
	     (if (and message (not no-insert) (not (stringp (car wait-p)))
		      (not top-level))
		 ;; Display message on first output
		 (comint-insert
		  (concat comment-start comment-start comment-start
			  message comment-end "\n")))
	     (if (and string (not (stringp string)))
		 ;; Elisp code
		 (progn 
		   (rplacd (comint-send-results (car comint-send-queue))
			   (if (consp prompt) (car prompt) prompt))
		   (funcall string)
		   (comint-dispatch-send process))
	       (if (stringp (car running))
		   ;; Continue interrupted send
		   (let ((output (car running)))
		     (if (or top-level (car (comint-send-results send))
			     (not (string-equal output "")))
			 ;; Continue old command
			 (progn
			   (rplaca running t)
			   (funcall comint-update-status status)
			   (comint-sender process comint-continue)
			   (comint-process-filter process output)
			   ;; Send queued default sends
			   (if (and top-level string)
			       (comint-sender process string)))
		       ;; Assume we have to restart the command since
		       ;; there is no output.  There is no way to
		       ;; sense whether or not the inferior has
		       ;; started processing the previous send.  This
		       ;; is a problem only if the original did start
		       ;; and had side effects.
		       (rplaca running nil)
		       (setq comint-send-queue 
			     (cons (list comint-fix-error t nil 'fix
					 "Fixing error" nil
					 nil nil 0 (cons nil nil))
				   comint-send-queue))
		       (comint-dispatch-send process)))
		 (if (not top-level)
		     ;; New send, set old prompt to the prompt of previous
		     (rplaca (cdr (comint-send-variables send)) 
			     (if (consp prompt) (car prompt) prompt)))
		 (if string
		     (progn
		       (rplaca running t)
		       (comint-sender process string))))))))))

;;;
(defun comint-interrupt (process send)
  "Interrupt PROCESS to send SEND if comint-continue is defined and
the current send is not waiting.  Otherwise, SEND will be the next
send."
  (if (and comint-continue (not (car (cdr (cdr (car comint-send-queue))))))
      (let* ((current (car comint-send-queue))
	     (interrupt
	      ;; string no-insert wait-p status message handler
	      (list nil t nil 'interrupt "Interrupt" nil
		    ;; running old-prompt line (output . prompt)
		    current nil 0 (cons nil nil))))
	(setq comint-send-queue (cons interrupt (cons send comint-send-queue)))
	(funcall comint-update-status 'interrupt)
	(comint-interrupt-subjob))
      (if (eq comint-send-queue comint-end-queue)
	  (setq comint-send-queue
		(cons (car comint-send-queue)
		      (cons send comint-send-queue)))
	  (rplacd comint-send-queue (cons send (cdr comint-send-queue))))))

;;;%Interface
(defun comint-setup-ipc (&optional force)
  "Setup for IPC in the current buffer.  If called interactively,
force comint-send-queue to be initialized."
  (interactive "p")
  (make-local-variable 'comint-send-newline)
  (make-local-variable 'comint-always-scroll)
  (make-local-variable 'comint-fix-error)
  (make-local-variable 'comint-continue)
  (make-local-variable 'comint-interrupt-regexp)
  (make-local-variable 'comint-error-regexp)
  (make-local-variable 'comint-output-filter)
  (make-local-variable 'comint-interrupt-start)
  (make-local-variable 'comint-handler)
  (make-local-variable 'comint-update-status)
  (make-local-variable 'comint-prompt-status)
  (make-local-variable 'comint-send-queue)
  (make-local-variable 'comint-end-queue)
  (make-local-variable 'comint-queue-emptied)
  (make-local-variable 'comint-output)
  (make-local-variable 'comint-errorp)
  (make-local-variable 'comint-status)
  (make-local-variable 'comint-aborting)
  (if (or force (not comint-send-queue))
      (setq comint-send-queue 
	    (list (list nil nil nil 'run "Top Level"
			nil t nil 0 (cons nil nil)))
	    comint-end-queue comint-send-queue))
  (let ((process (get-buffer-process (current-buffer))))
    (set-process-filter process 'comint-process-filter)
    (set-process-sentinel process 'comint-process-sentinel))
  (setq mode-line-process 'comint-status))

;;;%%Input
(defun comint-send (process string 
			    &optional 
			    no-insert
			    wait
			    status 
			    message
			    handler
			    after)
  "Do a send to PROCESS of STRING.  Optionally specify NO-INSERT,
WAIT, STATUS, MESSAGE, HANDLER and AFTER.  Without optional arguments,
this is just like process-send-string.  If STRING is not a string,
then it is assumed to be an elisp function and will be called when
encountered in the send queue.  The send will be the next one if WAIT,
after the last send if AFTER, otherwise it will be put at the end of
the queue. If WAIT is non-NIL or on the first send to a busy inferior,
the inferior will be interrupted if possible, see comint-interrupt for
more information.  Once the send is sent, the process status will be
STATUS or 'run.  Output of the send will be inserted into the process
buffer unless NO-INSERT.  This function returns a list of \(result .
prompt).  If WAIT is a string, output will be inserted until one
matches the string as a regexp.  If WAIT is T, then PROMPT will have
the prompt when finished and RESULT will have the output.  If PROMPT
is a list, then there was an error.  If WAIT is not T, then the list
returned will change when the send has been sent and is finished.  If
HANDLER is nil it will be set to comint-handler.  If it is T, errors
will be ignored.  When a send is finished, it calls handler with
\(error-p WAIT MESSAGE output prompt) which decides what to do with
the output.

VARIABLES:

comint-always-scroll will cause all process output to be visible.

comint-fix-error is the string used to fix errors.

comint-continue is the string used to continue after an interrupt.

comint-interrupt-regexp is the default regexp to use in finding the
start of the interrupt text.  

comint-error-regexp will set comint-errorp if found in the process output.  

FUNCTIONS:  Each of the functions in these variables is called with
the buffer set to the appropriate process buffer and
comint-original-buffer bound to the buffer current when the process
filter was called.  

comint-update-status is a function \(status) that is called each time
the process status changes.

comint-prompt-status is called with the old prompt and the last line.
It should return 'error if the last line is an error, T if it is a
prompt and nil otherwise.  It should also update the process status by
funcalling comint-update-status.

comint-output-filter is a function \(output) for sends with NO-INSERT.
It should return the output string.

comint-interrupt-start is a function \(output) that returns the start
of the interrupt text in output using comint-interrupt-regexp to find it."
  (save-excursion
    (set-buffer (process-buffer process))
    (let* ((inhibit-quit t)
	   (send (list string 
		       no-insert
		       wait
		       (or status 'run)
		       message 
		       (if (eq handler t) nil (or handler comint-handler))
		       ;; running, old-prompt, line
		       nil nil 0
		       ;; (output . prompt)
		       (cons nil nil)))
	   (pointer (comint-send-results send))
	   (top-level (eq comint-send-queue comint-end-queue))
	   (end (car comint-end-queue))
	   (current (car comint-send-queue))
	   (prompt (cdr (comint-send-results current)))
	   (ok nil))
      (setq comint-aborting nil)
      (if (and top-level (or (stringp wait) prompt))
	  ; the following were here in pvs-ilisp-mods.
	  ; not sure if they are necessary now...
	  ;(equal comint-status " :ready")
	  ;(or (stringp wait) prompt comint-queue-emptied)
	  ;
	  
	  (progn
	    (setq comint-send-queue (cons send comint-send-queue))
	    (comint-dispatch-send process))
	  (if (or (and wait (not after) (not prompt)) top-level)
	      (comint-interrupt process send)
	      (let ((looking t) 
		    (next comint-send-queue))
		(if after
		    (while (and looking next)
		      (if (eq (car next) comint-last-send)
			  (progn
			    (rplacd next (cons send (cdr next)))
			    (setq looking nil)))
		      (setq next (cdr next))))
		(if looking
		    (progn
		      (rplaca comint-end-queue send)
		      (setq comint-end-queue
			    (rplacd comint-end-queue (cons end nil))))))))
      (setq comint-last-send send)
      (unwind-protect
	   (let ((inhibit-quit nil))
	     (if (eq wait t)
		 (while (not (cdr pointer))
		   (accept-process-output)
		   (sit-for 0)))
	     (setq ok pointer))
	(if (not ok)
	    (if (eq send (car comint-send-queue))
		(let ((interrupt 
		       ;; string no-insert wait status message handler
		       (list nil t nil 'interrupt "Interrupt" nil
			     ;; running old-prompt line (output . prompt)
			     send (car (cdr (comint-send-variables send)))
			     nil (cons nil nil)))) 
		  (setq comint-send-queue
			(cons interrupt (cdr comint-send-queue)))
		  (comint-interrupt-subjob))
		(setq comint-send-queue (delq send comint-send-queue))))))))

;;;
(defun comint-send-code (process code)
  "Execute after the previous send in PROCESS queue CODE. You do not
want to execute synchronous sends in the code or it will lock up. " 
  (comint-send process code nil nil nil nil nil t))

;;;
(defun comint-default-send (process string)
  "Send to PROCESS top-level, STRING."  
  (save-excursion
    (set-buffer (process-buffer process))
    (let* ((top (car comint-end-queue))
	   (old (car top)))
      (rplaca (cdr (cdr (cdr (cdr (car comint-end-queue))))) string)
      (if (eq comint-send-queue comint-end-queue)
	  (progn (funcall comint-update-status 'run)
		 (rplaca (comint-send-variables (car comint-send-queue)) t)
		 (rplacd (comint-send-results (car comint-send-queue)) nil)
		 (comint-sender process string))
	  (rplaca top
		  (if old
		      (concat old (if comint-send-newline "\n") string)
		      string))))))

;;;
(defun comint-sync (process start start-regexp end end-regexp)
  "Synchronize with PROCESS output stream.  START will be sent with
each prompt received until START-REGEXP shows up in the stream.  Then
END will be sent and all output will be discarded until END-REGEXP
shows up in the output stream."
  (comint-send 
   process
   start
   nil start-regexp 'sync "Start sync" 
   (function (lambda (error-p wait message output prompt)
     (if (not (string-match wait output))
	 (comint-sender 
	  (get-buffer-process (current-buffer))
	  (car (car comint-send-queue))))
     nil)))
  (comint-send
   process
   end
   t end-regexp 'sync "End sync"
   (function (lambda (&rest args) nil))))

;;;
(defun comint-abort-sends (&optional process)
  "Abort all of the pending sends for optional PROCESS and show their
messages in *Aborted Commands*."
  (interactive)
  (save-excursion
    (setq process (or process (get-buffer-process (current-buffer))))
    (set-buffer (process-buffer process))
    (setq comint-aborting t)
    (if (not (eq comint-send-queue comint-end-queue))
	(let* ((inhibit-quit t)
	       (send (car comint-send-queue))
	       (vars (comint-send-variables send))
	       (pointer comint-send-queue)
	       (new nil)
	       (interrupt (and (car vars) 
			       (not (cdr (comint-send-results send))))))
	  (if interrupt
	      (progn			;Sent, but no prompt 
		(if (consp (car vars))
		    (progn (setq new (list send))
			   (rplaca (cdr (cdr (cdr (cdr (cdr send)))))
				   (function (lambda (&rest args) t))))
		    (setq new
			  (list
			   (list nil t nil 'interrupt "Interrupt"
				 (function (lambda (&rest args) t))
				 send (car (cdr (comint-send-variables send)))
				 nil (cons nil nil))))
		    (comint-interrupt-subjob)))) ;Already interrupting
	  (save-excursion
	    (set-buffer (get-buffer-create "*Aborted Commands*"))
	    (delete-region (point-min) (point-max)))
	  (while (not (eq pointer comint-end-queue))
	    (let ((send (car pointer)))
	      (if (car (cdr (cdr (cdr (cdr send))))) ;Message
		  (save-excursion
		    (set-buffer "*Aborted Commands*")
		    (insert (comint-send-description send))
		    (insert "\n\n")))
	      (if (and comint-fix-error
		       (stringp (car (comint-send-variables send))))
		  ;; Interrupted 
		  (setq new (cons 
			     (list comint-fix-error t nil 'fix
				   "Fixing error" nil
				   nil nil 0 (cons nil nil))
			     new)))
	      (setq pointer (cdr pointer))))
	  (bury-buffer "*Aborted Commands*")
	  (rplaca (car comint-end-queue) nil)
	  (setq comint-send-queue 
		(reverse (cons (car comint-end-queue) new))
		comint-end-queue 
		(let ((pointer comint-send-queue))
		  (while (cdr pointer)
		    (setq pointer (cdr pointer)))
		  pointer))
 	  (run-hooks 'comint-abort-hook)
	  (if (not interrupt) (comint-dispatch-send process))))))

;;;
(defun comint-current-send (showp)
  "Show the message of the current send in the minibuffer."
  (interactive "P")
  (if showp
      (with-output-to-temp-buffer comint-output-buffer
	(let ((send comint-send-queue))
	  (save-excursion
	    (set-buffer comint-output-buffer)
	    (insert "Pending commands:\n")
	    (while send
	      (let ((message (car (cdr (cdr (cdr (cdr (car send))))))))
		(if message (insert (concat message "\n"))))
	      (setq send (cdr send)))))))
  (message
   (concat "Command: "
	   (or (comint-send-description (car comint-send-queue))
	       "Unknown"))))


;;;
(defun comint-display-output (text &optional buffer)
  "Put TEXT in optional BUFFER and show it in a small temporary window."
  (setq buffer (or buffer comint-output-buffer))
  (with-output-to-temp-buffer buffer
    (save-excursion
      (set-buffer buffer)
      (insert text)
      (set-buffer-modified-p nil)))
  (ilisp-show-output (get-buffer buffer))
  text)
;; Perhaps this should use ilisp-display-output.

;;;
(defun comint-display-error (text)
  "Put TEXT in the comint-error-buffer and display it."
  (comint-display-output text comint-error-buffer))

(provide 'comint-ipc)
