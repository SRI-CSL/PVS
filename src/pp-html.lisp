;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-html.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Dec 21 13:52:53 1999
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 21 14:35:57 1999
;; Update Count    : 1
;; Status          : Beta test
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

(in-package :pvs)

(defvar *include-prelude-operators*)

(defvar *pvs-url-mapping* nil)

(defvar *pvs-html-hrefs*)

(defvar *pvs-html-dirs*)

(defvar *force-dirs*)

(defun html-pvs-file (file-name
		      &optional include-prelude-operators? all? force?)
    (cond ((and (null *pvs-url-mapping*)
		(not *pvs-context-writable*))
	   (pvs-message "You do not have write permission in this context"))
	  ((typechecked-file? file-name)
	   (check-pvs-url-mapping)
	   (let ((*include-prelude-operators* include-prelude-operators?)
		 (*pvs-html-hrefs* (make-hash-table :test #'eq))
		 (*pvs-html-dirs* (make-hash-table :test #'equal))
		 (*force-dirs* nil))
	     (html-directory)
	     (html-pvs-file* file-name force?)
	     (when all?
	       (html-prelude include-prelude-operators? force?)
	       (html-prelude-libraries include-prelude-operators? force?)
	       (dolist (dep (file-dependencies file-name))
		 (html-pvs-file* dep force?)))))
	  (t (pvs-message "File ~a has not been typechecked" file-name))))

(defun check-pvs-url-mapping (&optional (mappings *pvs-url-mapping*))
  (when mappings
    (unless (and (listp mappings)
		 (stringp (car mappings))
		 (stringp (cadr mappings))
		 (every #'(lambda (m) (and (listp m) (= (length m) 3)))
			(cddr mappings)))
      (html-pvs-error 
	"*pvs-url-mapping* must be a list of the form~%  ~
       '(\"baseurl\" \"basedir\" (\"specdir\" \"url\" \"htmldir\") ...)"))
    (check-valid-url (car mappings))
    (unless (file-exists-p (cadr mappings))
      (html-pvs-error
	"*pvs-url-mapping* element ~a must refer to an existing directory"
	(cadr mappings)))
    (unless (directory-p (cadr mappings))
      (html-pvs-error
	"*pvs-url-mapping* element ~a must refer to a directory"
	(cadr mappings)))
    (check-pvs-url-mappings (cddr mappings) (cadr mappings))
    (copy-support-html-files)))

(defun copy-support-html-files ()
  (when *pvs-url-mapping*
    (unless (file-exists-p (concatenate 'string
			     (ensure-trailing-slash (cadr *pvs-url-mapping*))
			     "pvs-style.css"))
      (copy-file (concatenate 'string *pvs-path* "/lib/pvs-style.css")
		 (cadr *pvs-url-mapping*)))))

(defun check-valid-url (string)
  ;; Fill this in sometime...
  (unless (valid-url? string)
    (html-pvs-error
      "*pvs-url-mapping* element ~a must refer to a valid URL"
      string))
  t)

(defun valid-url? (string)
  ;; It would be nice to actually check if the URI is real, instead of a
  ;; simple syntax check.
  #+allegro (ignore-errors (net.uri:parse-uri string))
  #-allegro t)

(defun check-pvs-url-mappings (mappings basedir)
  (when mappings
    (let ((map (car mappings)))
      (unless (directory-p (car map))
	(html-pvs-error
	  "Invalid *pvs-url-mapping* entry: ~a does not exist"
	  (car map)))
      (check-valid-url (cadr map))
      (let ((htmldir (if (char= (char (caddr map) 0) #\/)
			 (caddr map)
			 (concatenate 'string basedir (caddr map)))))
	(if (file-exists-p htmldir)
	    (unless (directory-p htmldir)
	      (html-pvs-error
		"Invalid *pvs-url-mapping* entry: ~a is not a directory"
		htmldir))
	    (make-directory-path htmldir))
	(unless (write-permission? htmldir)
	  (html-pvs-error
	    "Invalid *pvs-url-mapping* entry: no write permission for ~a"
	    htmldir))))
    (check-pvs-url-mappings (cdr mappings) basedir)))

(defun html-prelude (&optional include-prelude-operators? force?)
  (let ((*include-prelude-operators* include-prelude-operators?)
	(html-file (make-htmlpath "prelude"
				  (concatenate 'string *pvs-path* "/lib/")))
	(pvs-file (format nil "~a/lib/prelude.pvs" *pvs-path*))
	(*html-theories* *prelude-theories*))
    (when (or force?
	      (not (file-exists-p html-file))
	      (file-older html-file pvs-file))
      (with-open-file (*html-out* html-file
				  :direction :output
				  :if-exists :supersede)
	(html-theories "prelude" pvs-file)))))

(defun html-prelude-libraries (&optional include-prelude-operators? force?)
  (unless (zerop (hash-table-count *prelude-libraries*))
    (let ((*include-prelude-operators* include-prelude-operators?))
      (maphash
       #'(lambda (lib-ref files&theories)
	   (with-pvs-context lib-ref
	     (restore-context)
	     (let* ((*pvs-files* (car files&theories))
		    (*pvs-modules* (cadr files&theories))
		    ;;(dir (libref-to-pathname lib-ref))
		    )
	       (maphash
		#'(lambda (file theories)
		    (let ((html-file (make-htmlpath file))
			  (pvs-file (make-specpath file))
			  (*html-theories* (cdr theories)))
		      (assert (file-exists-p pvs-file))
		      (when (or force?
				(not (file-exists-p html-file))
				(file-older html-file pvs-file))
			(with-open-file (*html-out* html-file
						    :direction :output
						    :if-exists :supersede)
			  (html-theories file pvs-file))
			(pvs-message "~a generated" html-file))))
		*pvs-files*))))
       *prelude-libraries*))))

(defvar *html-out* nil)

(defvar *html-theories*)

(defun fmt-html (ctl &rest args)
  (format *html-out* "~?" ctl args))

(defun html-pvs-file* (file-name force?)
  (let ((pos (position #\/ file-name :from-end t)))
    (if pos
	(let ((libref (subseq file-name 0 (1+ pos)))
	      (orig-context-path *pvs-context-path*))
	  (with-pvs-context libref
	    (restore-context)
	    (multiple-value-bind (*pvs-files* *pvs-modules*)
		(get-imported-files-and-theories libref)
	      (relativize-imported-libraries
	       libref orig-context-path
	       (let* ((dir (libref-to-pathname libref))
		      (file (subseq file-name (1+ pos)))
		      (html-file (make-htmlpath file dir))
		      (pvs-file (let ((*pvs-context-path* dir))
				  (make-specpath file))))
		 (assert (file-exists-p pvs-file))
		 (when (or force?
			   (not (file-exists-p html-file))
			   (file-older html-file pvs-file))
		   (let ((*html-theories* (get-theories file)))
		     (with-open-file (*html-out* html-file
						 :direction :output
						 :if-exists :supersede)
		       (html-theories file-name pvs-file))
		     (pvs-message "~a generated" html-file))))))))
	(let ((html-file (make-htmlpath file-name))
	      (pvs-file (make-specpath file-name)))
	  (when (or force?
		    (not (file-exists-p html-file))
		    (file-older html-file pvs-file))
	    (let ((*html-theories* (get-theories file-name)))
	      (with-open-file (*html-out* html-file
					  :direction :output
					  :if-exists :supersede)
		(html-theories file-name pvs-file))
	      (pvs-message "~a generated" html-file)))))))

(defun html-theories (file-name pvs-file)
  (fmt-html
   "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"")
  (fmt-html
   "~%    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
  (fmt-html
   "~%<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">")
  (fmt-html "~%<html>~%<head>~%  <title>PVS File: ~a</title>" file-name)
  (when (car *pvs-url-mapping*)
    (fmt-html "~%  <base href=\"~a\" />" (car *pvs-url-mapping*)))
  (fmt-html "~%  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=ISO-8859-1\"/>")
  (fmt-html
   "~%  <link rel=\"stylesheet\" href=\"~apvs-style.css\" type=\"text/css\"/>"
   (if *pvs-url-mapping*
       ""
       (concatenate 'string *pvs-path* "/lib/")))
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
		      (let ((th (get-theory (cdr e))))
			(assert th)
			(fmt-html "<a href=\"~@[~a~]#~a\">"
				  (html-pvs-file-reference th)
				  (id th))))
		     (t (let ((decl (declaration (cdr e))))
			  (fmt-html "<a href=\"~@[~a~]#~a-~a\" title=\"~a\">"
			    (html-pvs-file-reference (module decl))
			    (id (module-instance (cdr e)))
			    (decl-to-declname decl)
			    (typecase decl
			      (type-decl "Type Declaration")
			      (typed-declaration
			       (let ((*print-pretty* nil))
				 (format nil "~a" (type decl))))
			      (lib-decl "library-declaration")
			      (mod-decl "theory-declaration")
			      (theory-abbreviation-decl
			       "theory-abbreviation-declaration")
			      (assuming-decl "assuming-declaration")
			      (tcc-decl "tcc-declaration")
			      (formula-decl "formula-declaration")
			      (judgement "judgement-declaration")
			      (conversion-decl "conversion-declaration")
			      (auto-rewrite-decl "auto-rewrite-declaration")
			      (t (break "What?")))))))
		   (push (cdr e) started))
		 (unless (or (not (eq in-comment 'maybe))
			     (member ch '(#\space #\tab #\page #\return
					  #\newline #\linefeed)
				     :test #'char=))
		   (fmt-html "</span comment>")
		   (setq in-comment nil))
		 (when (eq (car e) :begin-span)
		   (typecase (cdr e)
		     (datatype-or-module
		      (fmt-html "<span class=\"~a\" id=\"~a\">"
			(typecase (cdr e)
			  (module "theory")
			  (datatype "datatype")
			  (codatatype "codatatype"))
			(id (cdr e))))
		     (declaration
		      (fmt-html "<span class=\"~a\" id=\"~a\">"
			(typecase (cdr e)
			  (type-decl "type-declaration")
			  (formal-decl "formal-declaration")
			  (lib-decl "library-declaration")
			  (mod-decl "theory-declaration")
			  (theory-abbreviation-decl
			   "theory-abbreviation-declaration")
			  (var-decl "variable-declaration")
			  (macro-decl "macro-declaration")
			  (def-decl "recursive-declaration")
			  (inductive-decl "inductive-declaration")
			  (coinductive-decl "coinductive-declaration")
			  (const-decl "constant-declaration")
			  (assuming-decl "assuming-declaration")
			  (tcc-decl "tcc-declaration")
			  (formula-decl "formula-declaration")
			  (judgement "judgement-declaration")
			  (conversion-decl "conversion-declaration")
			  (auto-rewrite-decl "auto-rewrite-declaration")
			  (t (break "What?")))
			(ref-to-id (cdr e)))))
		   (push (cdr e) started)))
	       (when (char= ch #\%)
		 (unless in-comment ;; could be t or 'maybe
		   (fmt-html "<span class=\"comment\">"))
		 (setq in-comment t))
	       (when (char= ch #\newline)
		 (when (eq in-comment t)
		   (setq in-comment 'maybe)))
	       (write-char ch *html-out*)
	       (dolist (e centries)
		 (when (eq (car e) :end)
		   (assert (memq (cdr e) started))
		   (setq started (delete (cdr e) started :count 1))
		   (fmt-html "</a>"))
		 (when (eq (car e) :end-span)
		   (assert (memq (cdr e) started))
		   (setq started (delete (cdr e) started :count 1))
		   (fmt-html "</span>")))
	       (case ch
		 (#\linefeed
		  (incf frow)
		  (setq fcol 0)
		  (setq rentries (cdr (assoc frow anchors :test #'=))))
		 (t (incf fcol)))))))

(defun html-pvs-file-reference (theory)
  (or (gethash theory *pvs-html-hrefs*)
      (if (from-prelude? theory)
	  (concatenate 'string
	    (html-relative-reference (concatenate 'string *pvs-path*
						  "/lib"))
	    "prelude.html")
	  (let ((fname (concatenate 'string (filename theory) ".html")))
	    (setf (gethash theory *pvs-html-hrefs*)
		  (concatenate 'string
		    (html-relative-reference
		     (if (library-datatype-or-theory? theory)
			 (libref-to-pathname (lib-ref theory))
			 *pvs-context-path*))
		    fname))))))
  

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
	      (nconc scentry `((:begin-span . ,theory) (:begin . ,theory)))
	      (nconc srentry
		     `((,scol (:begin-span . ,theory) (:begin . ,theory))))))
	(setq *html-anchors*
	      (nconc *html-anchors*
		     `((,srow
			(,scol (:begin-span . ,theory) (:begin . ,theory)))))))
    ;; Get end of anchor
    (let ((erentry (cdr (assoc erow *html-anchors* :test #'=))))
      (if erentry
	  (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
	    (if ecentry
		(nconc ecentry `((:end . ,theory)))
		(nconc erentry `((,ecol (:end . ,theory))))))
	  (setq *html-anchors*
		(nconc *html-anchors*
		       `((,erow (,ecol (:end . ,theory))))))))
    ;; Get end of span
    (let* ((erow (ending-row place))
	   (ecol (ending-col place))
	   (erentry (cdr (assoc erow *html-anchors* :test #'=))))
      (if erentry
	  (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
	    (if ecentry
		(nconc ecentry `((:end-span . ,theory)))
		(nconc erentry `((,ecol (:end-span . ,theory))))))
	  (setq *html-anchors*
		(nconc *html-anchors*
		       `((,erow (,ecol (:end-span . ,theory))))))))
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
		       (nconc scentry `((:begin-span . ,ex) (:begin . ,ex)))
		       (nconc srentry
			      `((,scol (:begin-span . ,ex) (:begin . ,ex))))))
		 (setq *html-anchors*
		       (nconc *html-anchors*
			      `((,srow (,scol (:begin-span . ,ex)
					      (:begin . ,ex)))))))
	     (let ((erentry (cdr (assoc erow *html-anchors* :test #'=))))
	       (if erentry
		   (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
		     (if ecentry
			 (nconc ecentry `((:end . ,ex)))
			 (nconc erentry `((,ecol (:end . ,ex))))))
		   (setq *html-anchors*
			 (nconc *html-anchors*
				`((,erow (,ecol (:end . ,ex))))))))
	     (let* ((erow (ending-row place))
		    (ecol (ending-col place))
		    (erentry (cdr (assoc erow *html-anchors* :test #'=))))
	       (if erentry
		   (let ((ecentry (cdr (assoc ecol erentry :test #'=))))
		     (if ecentry
			 (nconc ecentry `((:end-span . ,ex)))
			 (nconc erentry `((,ecol (:end-span . ,ex))))))
		   (setq *html-anchors*
			 (nconc *html-anchors*
				`((,erow (,ecol (:end-span . ,ex)))))))))
	   (call-next-method))))

(defmethod collect-html-anchors* ((ex implicit-conversion))
  (collect-html-anchors* (argument ex))
  t)

(defmethod collect-html-anchors* ((ex modname))
  (call-next-method))

(defparameter *html-ignored-prelude-names*
  '(bool boolean TRUE FALSE NOT AND & OR IMPLIES => WHEN IFF <=>
	 = /= IF + - * / < <= > >= O ~))

(defmethod collect-html-anchors* ((ex name))
  (when (and (place ex)
	     (or (modname? ex)
		 (and (declaration ex)
		      (not (binding? (declaration ex)))
		      (not (generated-by (declaration ex)))
		      (or *include-prelude-operators*
			  (not (and (memq (id ex) *html-ignored-prelude-names*)
				    (from-prelude?
				     (module (declaration ex)))))))))
    (let* ((place (place ex))
	   (srow (starting-row place))
	   (scol (starting-col place))
	   (erow srow)
	   (ecol (+ scol
		    (if (library ex) (length (string (library ex))) -1)
		    (length (string (id ex)))))
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

(defun make-htmlpath (name &optional (pvs-dir *pvs-context-path*))
  (make-pathname :directory (html-directory pvs-dir)
		 :name name
		 :type "html"))

;;; This walks through the mappings looking for the place to put something
;;; in the 
(defun html-directory (&optional (pvs-dir *pvs-context-path*))
  (or (gethash pvs-dir *pvs-html-dirs*)
      (setf (gethash pvs-dir *pvs-html-dirs*)
	    (if (null *pvs-url-mapping*)
		(let ((html-dir (concatenate 'string
				  (ensure-trailing-slash (namestring pvs-dir))
				  "pvshtml/")))
		  (cond ((file-exists-p html-dir)
			 (if (write-permission? html-dir)
			     html-dir
			     (html-pvs-error
			      "Do not have write permission for ~a" html-dir)))
			(t (make-directory-path html-dir)
			   html-dir)))
		(html-directory*
		 (cddr *pvs-url-mapping*)
		 (ensure-trailing-slash (namestring pvs-dir))
		 (ensure-trailing-slash (cadr *pvs-url-mapping*)))))))

;;; Checks to see if the *pvs-context-path* is a subdirectory of one of the
;;; mappings, returning it if so.  Otherwise returns the *pvs-context-path*
;;; concatenated with "pvshtml".
(defun html-directory* (mappings pvs-dir base-html-dir)
  (if (null mappings)
      (html-pvs-error
	"~a is not a subdirectory of any of the mappings of *pvs-url-mapping*"
	pvs-dir)
      (let* ((map (car mappings))
	     (subdir (subdirectoryp pvs-dir (car map))))
	(cond (subdir
	       (let* ((dir (if (member (char (caddr map) 0) '(#\/ #\~)
				       :test #'char=)
			       (caddr map)
			       (concatenate 'string
				 base-html-dir (caddr map))))
		      (html-dir (concatenate 'string dir subdir)))
		 (cond ((file-exists-p html-dir)
			(if (write-permission? html-dir)
			    html-dir
			    (html-pvs-error
			      "Do not have write permission for ~a" html-dir)))
		       (t (make-directory-path html-dir)
			  html-dir))))
	      (t (html-directory* (cdr mappings) pvs-dir base-html-dir))))))

;;; Given a pvs directory (context), returns the URL reference relative to the
;;; base URL, if it exists; otherwise returns a full URL.
(defun html-relative-reference (pvs-dir)
  (unless (file-exists-p pvs-dir)
    (html-pvs-error "~a does not exist" pvs-dir))
  (unless (directory-p pvs-dir)
    (html-pvs-error "~a is not a directory" pvs-dir))
  (if (null *pvs-url-mapping*)
      (concatenate 'string
	(ensure-trailing-slash (namestring pvs-dir))
	"pvshtml/")
      (html-relative-reference*
       (cddr *pvs-url-mapping*)
       (ensure-trailing-slash (namestring pvs-dir))
       (car *pvs-url-mapping*))))

(defun html-relative-reference* (mappings pvs-dir base-url)
  (if (null mappings)
      (html-pvs-error "~a is not mapped to anything" pvs-dir)
      (let* ((map (car mappings))
	     (subdir (subdirectoryp pvs-dir (car map))))
	(if subdir
	    (concatenate 'string
	      (ensure-trailing-slash (cadr map))
	      (if (string= subdir "")
		  ""
		  (ensure-trailing-slash
		   (if (char= (char subdir 0) #\/)
		       (subseq subdir 1)
		       subdir))))
	    (html-relative-reference* (cdr mappings) pvs-dir base-url)))))

(defun make-directory-path (html-dir)
  (dolist (dir (directory-path html-dir))
    (unless (file-exists-p dir)
      (cond ((or *force-dirs*
		 (let ((ans (pvs-query "Create directory ~a? " dir)))
		   (prog1 ans
		     (when (eq ans :auto)
		       (setq *force-dirs* t)))))
	     #+allegro (excl:make-directory dir)
	     #+cmu (unix:unix-mkdir dir #o777)
	     #+sbcl (sb-unix:unix-mkdir dir #o777)
	     (pvs-message "Directory ~a created" dir))
	    (t (html-pvs-error "Directory ~a not created" dir))))))

(defun pvshtml-url-and-directory ()
  (let ((entry (assoc *pvs-context-path* *pvs-url-mapping*
		      :test #'subdirectoryp)))
    (if entry
	(values (cadr entry) (caddr entry))
	(values ""
		(append (pathname-directory *pvs-context-path*)
			    (list "pvshtml"))))))

(defun html-pvs-error (err &rest args)
  (pvs-error "Html-pvs-file error"
    (format nil "~?" err args)))
