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

(defun json-error (errstr)
  (format nil "~%{~%\"id\": ~a, \"error\": \"~a\"~%}~%"
    *pvs-json-id*
    errstr))

(defun json-check-form (str key)
  (multiple-value-bind (form err)
      (ignore-errors (read-from-string str))
    (if (typep err 'error)
	(values nil
		(json-error 
		 (format nil "Bad ~a \\\"~a\\\": ~a"
		   key
		   (protect-emacs-output str)
		   (protect-emacs-output (format nil "~a" err)))))
	form)))

(defun json-eval-form (form key)
  (multiple-value-bind (result err)
      (ignore-errors (eval form))
    (if (typep err 'error)
	(values nil
		(json-error
		 (format nil "Eval ~a \\\"~a\\\": ~a"
		   key
		   (protect-emacs-output form)
		   (protect-emacs-output (format nil "~a" err)))))
	result)))

(defun pvs-json (json-string)
  (let* ((request (json:decode-json-from-string json-string))
	 (*pvs-json-id* (cdr (assq :ID request)))
	 (rawcmd (cdr (assq :RAWCOMMAND request)))
	 (cmdstr (cdr (assq :COMMAND request)))
	 (params (cdr (assq :PARAMETERS request)))
	 (*pvs-json-interface* t)
	 (*print-pretty* nil))
    (if rawcmd
	(multiple-value-bind (form errstr)
	    (json-check-form rawcmd "rawcommand")
	  (if errstr
	      (format t "~a" errstr)
	      (multiple-value-bind (result errstr)
		  (json-eval-form form "rawcommand")
		(if errstr
		    (format t "~a" errstr)
		    (format t "~%{~%\"id\": ~a, \"result\": ~a~%}~%"
		      *pvs-json-id*
		      (with-output-to-string (*standard-output*)
			(json:encode-json result)))))))
	(multiple-value-bind (cmd errstr)
	    (json-check-form cmdstr "command")
	  (if errstr
	      (format t "~a" errstr)
	      (if (not (symbolp cmd))
		  (format t "~a"
		    (json-error "cmd ~a must be a symbol string"))
		  (if (not (listp params))
		      (format t "~a"
			(json-error
			 (format nil "parameters ~a must be a list" params)))
		      (multiple-value-bind (result errstr)
			  (json-eval-form (cons cmd params) "command")
			(if errstr
			    (format t "~a" errstr)
			    (format t "~%{~%\"id\": ~a, \"result\": ~a~%}~%"
			      *pvs-json-id*
			      (with-output-to-string (*standard-output*)
				(json:encode-json result))))))))))))

(defmethod json:encode-json ((obj datatype-or-module) &optional stream)
  (format stream "\"~a\"" (id obj)))

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

(defmethod declaration-kind ((th module))
  'theory)

(defmethod declaration-kind ((decl type-decl))
  'type-decl)

(defmethod declaration-kind ((decl formal-type-decl))
  'formal-type-decl)

(defmethod declaration-kind ((decl formal-const-decl))
  'formal-const-decl)

(defmethod declaration-kind ((decl formal-theory-decl))
  'formal-theory-decl)

(defmethod declaration-kind ((decl lib-decl))
  'lib-decl)

(defmethod declaration-kind ((decl mod-decl))
  'theory-decl)

(defmethod declaration-kind ((decl var-decl))
  'var-decl)

(defmethod declaration-kind ((decl def-decl))
  'recursive-decl)

(defmethod declaration-kind ((decl conversion-decl))
  'conversion-decl)

(defmethod declaration-kind ((decl conversionminus-decl))
  'conversion-minus-decl)

(defmethod declaration-kind ((decl auto-rewrite-decl))
  'auto-rewrite-decl)

(defmethod declaration-kind ((decl auto-rewrite-minus-decl))
  'auto-rewrite-minus-decl)

(defmethod declaration-kind (decl)
  (class-name (class-of decl)))

(defmethod decl-id ((decl datatype))
  (id decl))

(defun json-all-theories-info (&optional file prelude?)
  (if (null file)
      (let ((theory-alist nil))
	(maphash #'(lambda (id th)
		     (push (json-file-theories-info id th)
			   theory-alist))
		 (if prelude? *prelude* *pvs-modules*))
	theory-alist
	;;theory-alist
	)
      (json-pvs-file-info file)))

(defun json-pvs-file-info (file)
  (assert (stringp file))
  (list (cons 'file file)
	(cons 'theories
	      (mapcar #'json-pvs-theory-info (cdr (gethash file *pvs-files*))))))

(defun json-pvs-theory-info (th)
  (list (cons 'id (id th))
	(cons 'declarations
	      (mapcar #'(lambda (d)
			  (list (cons 'id (decl-id d))
				(cons 'kind (class-name (class-of d)))
				(cons 'place (or (place d) 'None))))
		(all-decls th)))))
