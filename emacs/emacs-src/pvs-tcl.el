;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Emacs-Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-tcl.el -- 
;; Author          : Carl Witty
;; Created On      : Wed Feb 28 23:07:48 1996
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Feb 28 23:08:10 1996
;; Update Count    : 1
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
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

;; Try to get the default tcl, if it exists
;; Otherwise get the one from the PVS path
(let ((load-path (cons nil load-path)))
  (require 'tcl))

(setq tcl-prompt-regexp "^% ")

;; Set this less than 249 to work around a bug in GNU Emacs 19.24/25.
(setq comint-input-chunk-size 200)

(defvar pvs-wish-cmd "wish"
  "The name of the wish binary for PVS.")

;; This is an edited version of inferior-tcl from tcl.el
(defun pvs-wish ()
  "Run inferior Tcl process for PVS."
  (unless (comint-check-proc "*tcl-pvs*")
    (condition-case nil
	(set-buffer (apply (function make-comint) "tcl-pvs"
			   pvs-wish-cmd nil
			   (if (pvs-getenv "DISPLAY") 
			       (append '("-name" "pvs") tcl-command-switches)
			       tcl-command-switches)))
      (file-error
       (error "Cannot find %s in your path; X-display commands will not work."
	      pvs-wish-cmd)))
    (inferior-tcl-mode))
  ;;(pvs-check-tcl-tk-versions)
  (make-local-variable 'tcl-application)
  (setq tcl-application pvs-wish-cmd)
  (set-process-filter (get-process "tcl-pvs") 'pvs-tcl-process-filter)
  (process-kill-without-query (get-process "tcl-pvs"))
  (setq inferior-tcl-buffer "*tcl-pvs*")
  (setq *pvs-tcl-partial-line* "")
  ;;  (comint-setup-ipc)
  ;; (pvs-check-tcl-tk-versions)
  (tcl-load-file (concat pvs-path "/wish/pvs-support.tcl"))
  inferior-tcl-buffer)


;;; The set of versions that PVS was tested against - keep it sorted from
;;; oldest version to newest

(defvar pvs-tested-tcl-versions '((7 3) (7 4)))

(defvar pvs-tested-tk-versions '((3 6) (4 0)))

(defun pvs-check-tcl-tk-versions ()
  (pvs-check-versions "Tcl" (tcl-eval "info tclversion")
		      pvs-tcl-tested-versions)
  (pvs-check-versions "Tk" (tcl-eval "set tk_version")
		      pvs-tk-tested-versions))

(defun pvs-check-versions (program-name version expected)
  (if (not (member-equal version expected))
      (let ((version-numbers (pvs-parse-version-numbers version))
	    (expected-numbers (pvs-parse-version-numbers (car expected))))
	(cond ((or (< (car version-numbers) (car expected-numbers))
		   (and (= (car version-numbers) (car expected-numbers))
			(< (cadr version-numbers) (cadr expected-numbers))))
	       (beep)
	       (comint-display-output
		(format "PVS was developed and tested for %s versions %s,\nbut you are using the earlier version %s.\nPVS will go ahead and try to use it, but please have %s updated\non your system before reporting any related bugs."
		    program-name expected version program-name)
		"PVS Error"))
	      (t (comint-display-output
		  (format "PVS was developed and tested for %s versions %s,\nbut you are using version %s.\nThis is unlikely to cause problems, as it is a later release."
		      program-name expected version program-name)
		  "PVS Warning"))))))

(defun pvs-parse-version-numbers (vnum)
  (mapcar (function string-to-number)
	  (string-split ?. vnum)))

(defun ensure-pvs-wish ()
  "Make sure an inferior wish is running for PVS."
  (if (not (comint-check-proc "*tcl-pvs*"))
      (pvs-wish)))

(defvar *pvs-tcl-partial-line* ""
  "The part of the pvs line which has been received so far.")

(defvar *pvs-tcl-recursive-process-filter* nil
  "True if currently inside a call to pvs-process-filter.")

(defvar *pvs-tcl-process-output* nil
  "Pending output from the pvs tcl process.")

;;; If anything called by pvs-process-filter does
;;; sit-for, sleep-for, or accept-process-output, then pvs-process-filter
;;; can get called recursively.  If it does, then it can't actually
;;; process the output that it gets; that would allow things to happen
;;; out of order.  Instead, it appends the output to the string
;;; which is currently being processed by the top-level pvs-process-filter.
(defun pvs-tcl-process-filter (process output)
  (comint-log process (format "\nrec:{%s}\n" output))
  (if *pvs-tcl-recursive-process-filter*
      (setq *pvs-tcl-process-output* (concat *pvs-tcl-process-output* output))
      (let ((*pvs-tcl-recursive-process-filter* t))
	(setq output (concat *pvs-tcl-partial-line* output))
	(let ((*pvs-tcl-process-output* output)
	      line-end)
	  (while (string-match "\n" *pvs-tcl-process-output*)
	    (setq line-end (match-end 0))
	    ;; Note that *pvs-tcl-process-output* can get longer
	    ;; during this next call.
;	    (comint-process-filter
;	     process (pvs-tcl-output-filter
;		      (substring *pvs-tcl-process-output* 0 line-end)))
	    (tcl-filter
	     process (pvs-tcl-output-filter
		      (substring *pvs-tcl-process-output* 0 line-end)))
	    (setq *pvs-tcl-process-output*
		  (substring *pvs-tcl-process-output* line-end)))
	  (if (string-match tcl-prompt-regexp
			    *pvs-tcl-process-output*)
	      (progn
;		(comint-process-filter process *pvs-tcl-process-output*)
		(tcl-filter process *pvs-tcl-process-output*)
		(setq *pvs-tcl-partial-line* ""))
	      (setq *pvs-tcl-partial-line* *pvs-tcl-process-output*))))))

(defun pvs-tcl-output-filter (output)
  (if (string-match
       ":pvs-\\(msg\\|log\\|err\\|qry\\|buf\\|yn\\|bel\\|loc\\|mod\\|pmt\\|dis\\|eval\\|evaln\\) "
       output)
      (let* ((orig-string-end (match-beginning 0))
	     (beg (match-end 0))
	     (kind (substring output (match-beginning 1) (match-end 1)))
	     (end (string-match (format " :end-pvs-%s" kind) output)))
	(if end
	    (let ((out (unless (string-equal kind "bel")
			 (substring output beg end))))
	      ;;(message (format "Pvs out: %s" out))
	      (cond ((string-equal kind "msg")
		     (message out)
		     (sit-for 1)
		     (pvs-log-message 'MSG out))
		    ((string-equal kind "log")
		     (pvs-log-log out))
		    ((string-equal kind "err")
		     (pvs-error out)
		     (pushw))
		    ((string-equal kind "qry")
		     (pvs-query out))
		    ((string-equal kind "buf")
		     (pvs-buffer out))
		    ((string-equal kind "yn")
		     (apply 'pvs-y-or-n (parse-pvs-message out)))
		    ((string-equal kind "bel")
		     (beep t)
		     (sit-for 1))
		    ((string-equal kind "loc")
		     (pvs-locate out))
		    ((string-equal kind "mod")
		     (pvs-modify-buffer out))
		    ((string-equal kind "pmt")
		     (pvs-prompt (parse-pvs-message out)))
		    ((string-equal kind "dis")
		     (pvs-display (parse-pvs-message out)))
		    ((string-equal kind "eval")
		     (apply 'pvs-tcl-emacs-eval (parse-pvs-message out)))
		    ((string-equal kind "evaln")
		     (apply 'pvs-tcl-emacs-eval-nowait (parse-pvs-message out)))
		    (t (error "%s not handled" kind))
		    )
	      (sit-for 0)
	      (substring output 0 orig-string-end))
	    output))
      (if (string-match ">>Error:[ \n\t]+TO-EMACS:" output)
	  ""
	  output)))

(defun tcl-send-string* (string)
  (comint-send-string (inferior-tcl-proc) (concat string "\n")))

(defvar tcl-eval-output nil)

(defun tcl-eval (string)
  (save-excursion
    (set-buffer inferior-tcl-buffer)
    (let* ((proc (get-buffer-process inferior-tcl-buffer))
	   (tcl-eval-output "")
	   ;; This one is used outside of Emacs19
	   (comint-output-filter '(lambda (str)
				    (setq tcl-eval-output str)))
	   ;; This one is used in Emacs19
	   (comint-output-filter-functions
	    (cons '(lambda (str)
		     (unless (string-match str comint-prompt-regexp)
		       (setq tcl-eval-output
			     (concat tcl-eval-output str))))
		  comint-output-filter-functions)))
      (tcl-send-string* string)
      (accept-process-output proc)
      (if (string-equal tcl-eval-output "")
	  (tcl-eval string)
	  tcl-eval-output))))

(defun pvs-tcl-emacs-eval (form)
  (let ((inhibit-quit nil))
    (condition-case ()
	(let ((val (eval (car (read-from-string form)))))
	  (tcl-send-string* (pvs-convert-to-lisp-string val)))
      (quit (tcl-send-string* ":abort")
	    (keyboard-quit)))))

(defun pvs-tcl-emacs-eval-nowait (form)
  (eval (car (read-from-string form))))
