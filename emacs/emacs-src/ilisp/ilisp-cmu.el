;;; -*- Mode: Emacs-Lisp -*-

;;; ilisp-cmu.el --
;;; ILISP CMU Common Lisp dialect definition
;;;
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id$

(defvar cmulisp-source-directory-regexp 
  "\\/afs\\/cs\\.cmu\\.edu\\/project\\/clisp\\/src\\/[0-9]*\\/"
  "*Regexp to match cmulisp source code directory.")

(defvar cmulisp-local-source-directory
  nil
  "*Where the cmulisp sources really are.")

(defvar ilisp-cmulisp-init-file "cmulisp")

(defun cmulisp-source-directory-fixup-function ()
  (if cmulisp-local-source-directory
      (replace-match cmulisp-local-source-directory)))

(defun cmulisp-check-prompt (old new)
  "Compare the break level printed at the beginning of the prompt."
  (let* ((was-in-break (and old (string-match "]+" old)))
 	 (old-level (if was-in-break
 			(- (match-end 0) (match-beginning 0))
 			0))
 	 (is-in-break (string-match "]+" new))
 	 (new-level (if is-in-break
 			(- (match-end 0) (match-beginning 0))
 			0)))
    (<= new-level old-level)))

;;;
(defdialect cmulisp "CMU Common LISP"
  common-lisp
  (ilisp-load-init 'cmu ilisp-cmulisp-init-file)
  (if cmulisp-local-source-directory
      (setq ilisp-source-directory-fixup-alist
	    (list 
	     (cons cmulisp-source-directory-regexp
		   cmulisp-local-source-directory)))
    (message "cmulisp-local-source-directory not set."))
  (setq comint-prompt-regexp "^\\([0-9]+\\]+\\|\\*\\|[-a-zA-Z0-9]*\\[[0-9]+\\]:\\) "
	ilisp-trace-command "(ILISP:cmulisp-trace \"%s\" \"%s\" \"%s\")"
	comint-prompt-status 
	(function (lambda (old line)
		    (comint-prompt-status old line 'cmulisp-check-prompt)))

	ilisp-error-regexp "ILISP:[^\"]*\\|Error [^\n]*\n\n"
	;; The above regexp has been suggested by
	;; hunter@work.nlm.nih.gov (Larry Hunter)


	;; 19991219 Marco Antoniotti
	;; Changed the very low level call to ARGLIST to the more
	;; sophisticated ILISP-ARGLIST.

	;; ilisp-arglist-command "(ILISP:arglist \"%s\" \"%s\")"
	ilisp-arglist-command "(ILISP:ilisp-arglist \"%s\" \"%s\")"

        ilisp-directory-command "(namestring (ext:default-directory))"
        ilisp-set-directory-command "(setf (ext:default-directory) \"%s\")"

	ilisp-find-source-command "(ILISP:source-file \"%s\" \"%s\" \"%s\")"

	comint-fix-error ":pop"

	comint-continue ":go"

	ilisp-reset ":q"

	comint-interrupt-regexp "Interrupted at"

	;; Note:
	;; 19990920
	;; The global definition should now take care to find out the
	;; proper extension.  See file 'ilisp-cl.el'.
	;;
	;; 19990806 Marco Antoniotti
	;; As Martin Atzmueller has pointed out, these hardcoded
	;; constraints are very nasty.
	;; However, before hacking the code right here, I'd like to
	;; see an all-out solution to the binary file extension problem.

	;; ilisp-binary-extension "sparcf"
	;; ilisp-init-binary-extension "sparcf"
	;; ilisp-binary-command "\"sparcf\""
	)

  ;; ILD Support

  (setq ild-abort-string ":abort"
	ild-continue-string ":go"
	ild-step-string ":step"
	ild-step-string-arg nil
	ild-next-string ":down"
	ild-next-string-arg nil		; needs work
	ild-previous-string ":up"
	ild-previous-string-arg nil	; needs work
	ild-top-string ":bottom"
	ild-bottom-string ":top"
	ild-backtrace-string ":backtrace"
	ild-locals-string ":l"
	ild-local-string-arg "(debug:arg %s)"
	ild-return-string nil		; needs work (debug:debug-return x)
	ild-retry-string nil		; needs work
	ild-trap-on-exit-string nil	; needs work
	)
  )

;;; end of file -- ilisp-cmu.el --
