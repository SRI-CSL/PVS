;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-unp-tex.lisp	1.8 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Tue Jul 26 13:35:17 1988

(in-package :sb-runtime)  (use-package :ergolisp)





(defparameter spaces-to-em 0.6
  "Conversion factor from spaces to TeX em units.")


(defvar *s* nil
  "Outstring for accumulated tokens")

(defvar *indent-sequence* nil
  "Sequence of indents or unindents to be added at the end of the current
   line.") 

(defvar *indent-count* 0
  "Temporary count to avoid writing redundant indents and unindents.")



(defun map-aw-to-tex (aw)
  (let ((*print-pretty* nil)		; To avoid weirdness.
	(*s* (make-string-output-stream))
	(*indent-sequence* nil)
	(*indent-count* 0))
    (format *s* " \\begin{program} ~%")
    (map-aw-to-tex-aux aw)
    (format *s* " \\\\ ~%")
    (write-indents)
    (format *s* " \\end{program}")
    (get-output-stream-string *s*)))



(defun map-aw-to-tex-aux (aw)
  (let ((*escape-character* #\\))	; for TeX.
    (do ((sons (aw-sons-form aw) (cdr sons)))
	((null sons) aw)
      (if (aw-p (car sons))
	  (map-aw-to-tex-aux (car sons))
	  (let ((token (car sons)))
	    (assert (token-p token))
	    (ecase (token-kind token)
	      (:keyword
	       (ecase (token-subkind token)
		 (:jux)
		 ((nil)
		  (format *s* "~A"
		    (tex-keyword token)))))
	      (:lt
	       (ecase (token-subkind token)
		 ((:id :identifier)
		  (format *s* "~A"
		    (tex-id token)))
		 ((:number)
		  (format *s* " ~A"
		    (if (token-str-value token)
			(token-str-value token)
			(token-value token))))
		 ((:string)
		  (format *s* " ~A"
		    (tex-string token)))))
	      (:whitespace
	       (ecase (token-subkind token)
		 (:sp
		  (let* ((spaces (sb-pixels-to-chars (token-value token)))
			 (spaces (* spaces-to-em spaces)))
		    (if (> spaces 0)
			(format *s* " \\hspace{~Aem}" spaces))))
		 (:cr
		  (format *s* " \\\\~%")
		  (commit-indents)
		  (write-indents))
		 (:indent
		  (incf *indent-count*))
		 (:unindent
		  (decf *indent-count*))    
		 (:tab-left
		  (commit-indents)
		  (format *s* "\\ii"))
		 (:tab-right
		  (commit-indents)
		  (format *s* "\\ii"))
		 (:untab
		  (commit-indents)
		  (format *s* "\\oo"))))))))))




(defun commit-indents ()
  (setq *indent-sequence*
	(cons *indent-count* *indent-sequence*))
  (setq *indent-count* 0))


(defun write-indents ()
  (do ((indent-counts (reverse *indent-sequence*) (cdr indent-counts)))
      ((null indent-counts))
    (do ((i 0 (1+ i)))
	((>= i (car indent-counts)))
      (format *s* "\\zi"))
    (do ((i 0 (1- i)))
	((<= i (car indent-counts)))
      (format *s* "\\zo"))
    (setq *indent-sequence* nil)))

	    


;;; Inserting escapes is a fairly expensive operation.  We do not want to do it
;;; repeatedly. 

(defparameter TeX-restricted-chars '(#\# #\$ #\% #\_ #\{ #\} #\^ #\~ #\\))
;;; Must handle #\^ #\~ and #\\ separately.

(defun special-fix (str)
     ;; This routine is a HACK.  It assumes that the following special symbols
     ;; are only one character.   
  (cond ((and (char= (elt str 0) #\\)
	      (char= (elt str 1) #\\))
	 "\\verb!\\!")
	((and (char= (elt str 0) #\\)
	      (char= (elt str 1) #\^))
	 "\\verb!^!")
	((and (char= (elt str 0) #\\)
	      (char= (elt str 1) #\~))
	 "\\verb!~!")
	(t str)))

(defvar tex-keywords (make-hash-table :test #'eq))
(defvar tex-ids (make-hash-table :test #'eq))
(defvar tex-strings (make-hash-table :test #'equal))


(defun tex-keyword (token)
  (cond ((gethash (token-value token) tex-keywords))
	(t
	 (setf (gethash (token-value token) tex-keywords)
	       (let* ((str (if (token-str-value token)
			       (token-str-value token)
			       (symbol-name (token-value token))))
		      (str (if (and (not *case-sensitive*)
				    (eq :downcase *print-case*))
			       (string-downcase str)
			       str)))
		 (format nil (if (alpha-char-p (elt str 0))
				 " \\.~A."
				 " ~A")
		   (if (alpha-char-p (elt str 0))
		       (let ((*restricted-chars*
			      (cons #\. TeX-restricted-chars)))
			 (insert-escapes str))
		       (let ((*restricted-chars* TeX-restricted-chars))
			 (special-fix (insert-escapes str))))))))))

(defun tex-id (token)
  (cond ((gethash (token-value token) tex-ids))
	(t
	 (setf (gethash (token-value token) tex-ids)
	       (let* ((str (if (token-str-value token)
			       (token-str-value token)
			       (symbol-name (token-value token))))
		      (str (if (and (not *case-sensitive*)
				    (eq :downcase *print-case*))
			       (string-downcase str)
			       str)))
		 (format nil " ~A"
		   (let ((*restricted-chars* TeX-restricted-chars))
		     (insert-escapes str))))))))


(defun tex-string (token)
  (cond ((gethash (token-value token) tex-strings))
	(t
	 (setf (gethash (token-value token) tex-strings)
	       (let* ((str (if (token-str-value token)
			       (token-str-value token)
			       (symbol-name (token-value token)))))
		 (format nil " ~A" 
		   (let ((*restricted-chars* TeX-restricted-chars))
		     (insert-escapes str))))))))

