;;; These rely on cl-json, which is available using darcs:
;;;  darcs get http://common-lisp.net/project/cl-json/darcs/cl-json
;;; Once this is downloaded, you must get the asdf.fasl file from
;;; Allegro CL.  You can find this in:
;;;  ~owre/acl82/code/asdf.fasl -- 32 bit Linux
;;;  ~owre/acl82.64/code/asdf.fasl -- 64 bit Linux
;;;  ~owre/acl82.64.mac/code/asdf.fasl -- 64 bit Mac
;;; Then do (this can be put in ~/.pvs.lisp)
;;;   (load "<ASDFDIR>/asdf")
;;;   (load "<CLJSONDIR>/cl-json/cl-json.asd")
;;;   (asdf:load-system :cl-json)
;;;   (load "<PVSDIR>/eclipse/eclipse-patches")

(defvar *pvs-json-interface* nil)
(defvar *pvs-json-id* nil)

(defun pvs-json (json-string)
  (let* ((request (json:decode-json-from-string json-string))
	 (*pvs-json-id* (cdr (assq :ID request)))
	 (rawcmd (cdr (assq :RAWCOMMAND request)))
	 (cmd (cdr (assq :COMMAND request)))
	 (params (cdr (assq :PARAMETERS request)))
	 (*pvs-json-interface* t))
    (if rawcmd
	(eval rawcmd)
	(apply cmd params))))

(defun pvs-error (msg err &optional itheory iplace)
  ;; Indicates an error; no recovery possible.
  (cond (*rerunning-proof*
	 (restore))
	((and *pvs-emacs-interface*
	      *to-emacs*)
	 (let* ((place (if *adt-decl* (place *adt-decl*) iplace))
		(buff (if *adt-decl*
			  (or (filename *generating-adt*)
			      (and (current-theory)
				   (filename (current-theory)))
			      *current-file*)
			  (or *from-buffer* itheory)))
		(*print-pretty* nil)
		(*output-to-emacs*
		 (format nil ":pvs-err ~a&~a&~a&~a&~d ~d :end-pvs-err"
		   (when buff (protect-emacs-output (namestring buff)))
		   (unless *from-buffer*
		     (protect-emacs-output (namestring *pvs-context-path*)))
		   (protect-emacs-output msg)
		   (write-to-temp-file err)
		   (line-begin place) (col-begin place))))
	   (to-emacs)
	   (if *in-checker*
	       (restore)
	       (pvs-abort))))
	((null *pvs-emacs-interface*)
	 (if *pvs-json-interface*
	     (format t "~%{~%\"id\": ~a, \"error\": \"~a\">~%\"~a\"~%}~%"
	       *pvs-json-id* (protect-emacs-output msg)
	       (protect-emacs-output err))
	     ;; Otherwise it's XML
	     (format t "~%<pvserror msg=\"~a\">~%\"~a\"~%</pvserror>"
	       (protect-emacs-output msg) (protect-emacs-output err)))
	 (pvs-abort))
	(t
	 (format t "~%~%~a~%~a" msg err)
	 (if *in-checker*
	     (restore)
	     (error "PVS error")))))

(defun parse-error (obj message &rest args)
  ;;(assert (or *in-checker* *current-file*))
  (cond (*parse-error-catch*
	 (throw *parse-error-catch*
		(values nil
			(if args
			    (format nil "~?" message args)
			    message)
			obj)))
	((and (or *to-emacs*
		  (null *pvs-emacs-interface*))
	      (or (not *in-checker*)
		  (not *in-evaluator*)
		  *tc-add-decl*))
	 (pvs-error "Parser error"
	   (if args
	       (format nil "~?" message args)
	       message)
	   *current-file*
	   (place obj)))
	((and (or *in-checker*
		  *in-evaluator*)
	      (not *tcdebug*))
	 (if args
	     (format t "~%~?" message args)
	     (format t "~%~a" message))
	 (format t "~%Restoring the state.")
	 (restore))
	((null *pvs-emacs-interface*)
	 (if *pvs-json-interface*
	     (format t "~%{~%\"id\": ~a, \"error\":\"parse-error:~a\"~%}~%"
	       *pvs-json-id*
	       (protect-emacs-output
		(if args
		    (format t "~%~?" message args)
		    (format t "~%~a" message))))
	     ;; Otherwise XML
	     (format t "~%<pvserror msg=\"parse-error\">~%\"~a\"~%</pvserror>"
	       (protect-emacs-output
		(if args
		    (format t "~%~?" message args)
		    (format t "~%~a" message)))))
	 (pvs-abort))
	(t (if args
	       (format t "~%~?~a~a"
		 message
		 args
		 (if *current-file*
		     (format nil "~%In file ~a" (pathname-name *current-file*))
		     "")
		 (if (place obj)
		     (format nil " (line ~a, col ~a)"
		       (line-begin (place obj))
		       (col-begin (place obj)))
		     ""))
	       (format t "~%~a~a~a"
		 message
		 (if *current-file*
		     (format nil "~%In file ~a" (pathname-name *current-file*))
		     "")
		 (if (place obj)
		     (format nil " (line ~a, col ~a)"
		       (line-begin (place obj))
		       (col-begin (place obj)))
		     "")))
	   (error "Parse error"))))
