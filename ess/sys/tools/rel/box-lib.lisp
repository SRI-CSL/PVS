;;; -*- Mode: Lisp; Package: TOOLS -*-
;;;
;;; Functions to exploit the information in the box-defs file
;;; for releasing part of the Ergo Support System.
;;;
;;; Author: Frank Pfenning.  Last Modified Fri Oct  6 16:30:01 1989
;;;
;;; Sccs Id "@(#)box-lib.lisp	1.10 10/6/89
;;; ******************************************************************* ;;;
;;;         (c) Copyright 1989 by Carnegie Mellon University.           ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

(in-package :TOOLS) (use-package :ergolisp)

(export '(list-box-files list-boxes-in-crates))
(export '(list-all-crates list-tar-crates))

(defvar *write-file-line-hook* nil
  "NIL, or function to be called on a filename, when it would try to write
it out into a file list.")

(defun list-some-files (full-filename pred absolute-fun
				      &optional (extra-files ()))
  (let ((filelist '()))
    (maphash #'(lambda (key efile)
		 (declare (ignore key))
		 (when (funcall pred efile)
		   (push (funcall absolute-fun
				  (efile-name efile)
				  (car (last (box-path (efile-box efile)))))
			 filelist)))
	     *efiletable*)
    (with-open-file (outstream full-filename :direction :output
			       :if-exists :supersede)
      (boxmsg "Writing file ~S." full-filename)
      (dolist (f (append (sort filelist #'string<)
			 (sort extra-files #'string<)))
	(if *write-file-line-hook*
	    (funcall *write-file-line-hook* f outstream)
	    (format outstream "~A~%" f))))))

(defun box-absolute (file-name box-directory)
  (namestring (merge-pathnames file-name box-directory)))

(defun box-relative (file-name box-directory)
  (declare (ignore box-directory))
  file-name)

(defun readme-filelist (box &optional (absolute #'box-absolute))
  (let ((box-directory (car (last (box-path box)))))
    (mapcar #'(lambda (filestring) (funcall absolute filestring box-directory))
	    (box-readme box))))


(defun list-box-files (box &optional (absolute #'box-relative) (filename nil))
  "Create a list of all files in the box BOX and write them to FILENAME.
ABSOLUTE indicates if pathnames should be absolute or relative to the
relevant directory.  Even though this say `ALL', it still excludes
compiled files, since only one set of compiled files is accessible
per Lisp core image."
  (let* ((box (get-existent-box box))
	 (boxname (box-name box))
	 (filename (if filename filename
		       (concatenate 'string boxname "-ALL")))
	 (relevant-directory (car (last (box-path box))))
	 (readme-files (readme-filelist box absolute))
	 (full-filename
	  (merge-pathnames filename relevant-directory)))
    (list-some-files full-filename
		     #'(lambda (efile)
			 (and (eq (efile-box efile) box)
			      (not (string= *lisp-compiled-extension*
					    (pathname-type (efile-name efile))))))
		     absolute
		     readme-files)))

(defun list-boxes-in-crates (crates)
  "Creates a files with a list of files for all boxes in CRATES."
  (when (not (listp crates))
    (setq crates (list crates)))
  (setq crates (mapcar #'get-existent-crate crates))
  (dolist (c crates)
    (dolist (b (crate-boxes c))
      (list-box-files b #'box-absolute))))


(defun list-all-boxes-files (boxes filename)
  "Lists the name of all source files in BOXES into FILENAME."
  (list-some-files filename
		   #'(lambda (efile)
		       (and (member (efile-box efile) boxes)
			    (not (string= *lisp-compiled-extension*
					  (pathname-type (efile-name efile))))))
		   #'box-absolute
		   (mapcan #'readme-filelist boxes)
		   ))

(defun list-all-crates (crates all-filename)
  "Lists the name of all source files in CRATES into FILENAME.  Useful for
making BIG tags files."
  (when (not (listp crates))
    (setq crates (list crates)))
  (setq crates (mapcar #'get-existent-crate crates))
  (let ((boxes (mapcar #'get-existent-box (apply #'append (mapcar #'crate-boxes crates)))))
    (list-all-boxes-files boxes all-filename)))

(defun write-tar-line (file-name outstream)
  (let ((file-write-time (file-written file-name))
	(file-write-string "Absent"))
    (when (not (= file-write-time 0))
      (multiple-value-bind (sec min hour date month year doy dst zone)
	  (decode-universal-time file-write-time)
	(declare (ignore zone dst doy))
	(setq file-write-string
	      (format nil "~S/~S/~S ~d:~2,'0d:~2,'0d"
		      month date year hour min sec))))
    (when (= file-write-time 0)
      (warn "File ~S does not exist." file-name))
    (format outstream "~A ~30T(~S ~D)~%"
	    file-name file-write-string file-write-time)))

(defun list-tar-crates (dir feature-alist)
  "Makes feature.list files for all features advertised in feature-alist."
  (let ((*write-file-line-hook* 'write-tar-line))
    (dolist (feat feature-alist)
      (list-all-crates (append (second feat) (third feat))
		       (concatenate 'string (directorify dir)
				    (string-downcase (first feat))
				    ".list")))))
