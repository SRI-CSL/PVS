;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-html.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Dec 21 13:52:53 1999
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 21 14:35:57 1999
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002-2004 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(defun html-pvs-file (file-name &optional all? force?)
  (cond ((not *pvs-context-writable*)
	 (pvs-message "You do not have write permission in this context"))
	((not (ensure-html-subdirectory))
	 nil)
	((typechecked-file? file-name)
	 (html-pvs-file* file-name force?)
	 (when all?
	   (dolist (dep (file-dependencies file-name))
	     (html-pvs-file* dep force?))))
	(t (pvs-message "File ~a has not been typechecked" file-name))))

(defun html-prelude ()
  (let ((html-file (format nil "~a/lib/prelude.html" *pvs-path*))
	(pvs-file (format nil "~a/lib/prelude.pvs" *pvs-path*))
	(*html-theories* *prelude-theories*))
    (with-open-file (*html-out* html-file
				:direction :output
				:if-exists :supersede)
      (html-theories "prelude" pvs-file))))

(defvar *html-out* nil)

(defvar *html-theories*)

(defun fmt-html (ctl &rest args)
  (format *html-out* "~?" ctl args))

(defun html-pvs-file* (file-name force?)
  (let ((html-file (make-htmlpath file-name))
	(pvs-file (make-specpath file-name)))
    (when (or force?
	      (not (file-exists-p html-file))
	      (file-older html-file pvs-file))
      (let ((*html-theories* (get-theories file-name)))
	(with-open-file (*html-out* html-file
				    :direction :output
				    :if-exists :supersede)
	  (html-theories file-name pvs-file))))))

(defun html-theories (file-name pvs-file)
  (fmt-html
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"")
  (fmt-html
   "~%    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  (fmt-html
   "~%<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">")
  (fmt-html "~%<html>~%<head>~%  <title>PVS File: ~a</title>" file-name)
  (fmt-html "~%  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>")
  (fmt-html
   "~%  <link rel=\"stylesheet\" href=\"~a/lib/pvs-style.css\" type=\"text/css\"/>"
   *pvs-path*)
  (fmt-html "~%</head>~%<body>")
  (fmt-html "~%<h2><a name=\"~a.pvs\">~a.pvs</a></h2>" file-name file-name)
  (when (cdr *html-theories*)
    (fmt-html "~%<br>~%<h3>Theories:</h3>~%<ol>")
    (dolist (theory *html-theories*)
      (fmt-html "~%<li><a href=\"#~a\">~a</a></li>" (id theory) (id theory))))
  (fmt-html "~%</ol>~%<hr>~%<pre>~%")
  (let* ((anchors (collect-html-anchors *html-theories*)))
    (with-open-file (in pvs-file :direction :input)
      (write-pvs-html-file in anchors))))

(defun write-pvs-html-file (in anchors)
  (let ((frow 1)
	(fcol 0)
	(started nil)
	(in-comment nil)
	(rentries (cdr (assoc 1 anchors :test #'=))))
    (loop for ch = (read-char in nil 'eof)
	  until (eq ch 'eof)
	  finally (when started
		    (break "something wrong, elements left unended"))
	  do (let ((centries (cdr (assoc fcol rentries :test #'=))))
	       (dolist (e centries)
		 (when (eq (car e) :begin)
		   (typecase (cdr e)
		     (datatype-or-module
		      (fmt-html "<a name=\"~a\">" (id (cdr e))))
		     (declaration
		      (fmt-html "<a name=\"~a-~a\">"
			(id (module (cdr e)))
			(decl-to-declname (cdr e))))
		     (modname
		      (fmt-html "<a href=\"~@[~a~]#~a\">"
			(html-pvs-file-reference (get-theory (cdr e)))
			(id (cdr e))))
		     (t (fmt-html "<a href=\"~@[~a~]#~a-~a\">"
			  (html-pvs-file-reference
			   (module (declaration (cdr e))))
			  (id (module-instance (cdr e)))
			  (decl-to-declname (declaration (cdr e))))))
		   (push (cdr e) started)))
	       (when (and (char= ch #\%)
			  (not in-comment))
		 (fmt-html "<em>")
		 (setq in-comment t))
	       (when (and (char= ch #\newline)
			  in-comment)
		 (fmt-html "</em>")
		 (setq in-comment nil))
	       (write-char ch *html-out*)
	       (dolist (e centries)
		 (when (eq (car e) :end)
		   (assert (memq (cdr e) started))
		   (setq started (delete (cdr e) started :count 1))
		   (fmt-html "</a>")))
	       (case ch
		 (#\linefeed
		  (incf frow)
		  (setq fcol 0)
		  (setq rentries (cdr (assoc frow anchors :test #'=))))
		 (t (incf fcol)))))))

(defun html-pvs-file-reference (theory)
  (unless (memq theory *html-theories*)
    (cond ((from-prelude? theory)
	   (format nil "~a/lib/prelude.html" *pvs-path*))
	  ((library-datatype-or-theory? theory)
	   (break "html-pvs-file-reference to library"))
	  (t (concatenate 'string (filename theory) ".html")))))

(defvar *html-anchors* nil)

(defun collect-html-anchors (theories)
  (let ((*html-anchors* nil)
	(*parsing-or-unparsing* t))
    (dolist (theory theories)
      (let ((*current-context* (saved-context theory)))
	(assert *current-context*)
	(mapobject #'collect-html-anchors* theory)))
    *html-anchors*))

(defmethod collect-html-anchors* ((theory module))
  (let* ((place (place theory))
	 (srow (starting-row place))
	 (scol (starting-col place))
	 (erow srow)
	 (ecol (+ scol (length (string (id theory))) -1))
	 (srentry (cdr (assoc srow *html-anchors* :test #'=))))
    (if srentry
	(let ((scentry (cdr (assoc scol srentry :test #'=))))
	  (if scentry
	      (nconc scentry `((:begin . ,theory)))
	      (nconc srentry `((,scol (:begin . ,theory))))))
	(setq *html-anchors*
	      (nconc *html-anchors*
		     `((,srow (,scol (:begin . ,theory)))))))
    (let ((erentry (cdr (assoc erow *html-anchors* :test #'=))))
      (if erentry
	  (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
	    (if ecentry
		(nconc ecentry `((:end . ,theory)))
		(nconc erentry `((,ecol (:end . ,theory))))))
	  (setq *html-anchors*
		(nconc *html-anchors*
		       `((,erow (,ecol (:end . ,theory))))))))
    nil))

(defmethod collect-html-anchors* ((ex declaration))
  (cond ((and (not (binding? ex))
	      (or ;;(chain? ex)
		  (generated-by ex)))
	 t)
	(t (let* ((place (place ex))
		  (srow (starting-row place))
		  (scol (starting-col place))
		  (erow srow)
		  (ecol (+ scol (length (string (id ex))) -1))
		  (srentry (cdr (assoc srow *html-anchors* :test #'=))))
	     (if srentry
		 (let ((scentry (cdr (assoc scol srentry :test #'=))))
		   (if scentry
		       (nconc scentry `((:begin . ,ex)))
		       (nconc srentry `((,scol (:begin . ,ex))))))
		 (setq *html-anchors*
		       (nconc *html-anchors*
			      `((,srow (,scol (:begin . ,ex)))))))
	     (let ((erentry (cdr (assoc erow *html-anchors* :test #'=))))
	       (if erentry
		   (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
		     (if ecentry
			 (nconc ecentry `((:end . ,ex)))
			 (nconc erentry `((,ecol (:end . ,ex))))))
		   (setq *html-anchors*
			 (nconc *html-anchors*
				`((,erow (,ecol (:end . ,ex)))))))))
	   (call-next-method))))

(defmethod collect-html-anchors* ((ex implicit-conversion))
  (collect-html-anchors* (argument ex))
  t)

(defmethod collect-html-anchors* ((ex modname))
  (call-next-method))

(defmethod collect-html-anchors* ((ex name))
  (when (and (place ex)
	     (or (modname? ex)
		 (and (declaration ex)
		      (not (binding? (declaration ex)))
		      (not (generated-by (declaration ex))))))
    (let* ((place (place ex))
	   (srow (starting-row place))
	   (scol (starting-col place))
	   (erow srow)
	   (ecol (+ scol (length (string (id ex))) -1))
	   (srentry (cdr (assoc srow *html-anchors* :test #'=))))
      (assert (and srow scol erow ecol))
      (if srentry
	  (let ((scentry (cdr (assoc scol srentry
				     :test #'=))))
	    (if scentry
		(nconc scentry `((:begin . ,ex)))
		(nconc srentry `((,scol (:begin . ,ex))))))
	  (setq *html-anchors*
		(nconc *html-anchors*
		       `((,srow (,scol (:begin . ,ex)))))))
      (let ((erentry (cdr (assoc erow *html-anchors* :test #'=))))
	(if erentry
	    (let ((ecentry (cdr (assoc ecol erentry
				       :test #'=))))
	      (if ecentry
		  (nconc ecentry `((:end . ,ex)))
		  (nconc erentry `((,ecol (:end . ,ex))))))
	    (setq *html-anchors*
		  (nconc *html-anchors*
			 `((,erow (,ecol (:end . ,ex)))))))))
    nil))

(defmethod collect-html-anchors* (ex)
  (declare (ignore ex))
  nil)

(defun ensure-html-subdirectory ()
  (let ((subdir (make-pathname
		 :defaults *pvs-context-path*
		 :name "pvshtml")))
    (if (file-exists-p subdir)
	(if (directory-p subdir)
	    subdir
	    (pvs-message "~a is a regular file,~%  and can't be used as a~
                          subdirectory for html files unless it is moved."
	      subdir))
	(multiple-value-bind (result error)
	    (ignore-errors (excl:make-directory subdir))
	  (declare (ignore result))
	  (cond (error
		 (pvs-message "Error creating ~a: ~a" subdir error))
		(t (pvs-message "Created directory ~a" subdir)
		   subdir))))))

(defmethod make-htmlpath ((name symbol))
  (make-htmlpath (string name)))

(defmethod make-htmlpath ((name string))
  (make-pathname :defaults *pvs-context-path*
		 :directory (append (pathname-directory *pvs-context-path*)
				    (list "pvshtml"))
		 :name name
		 :type "html"))
