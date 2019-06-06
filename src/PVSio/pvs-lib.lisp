;;
;; pvs-lib.lisp
;; Release: PVSio-6.0.10 (xx/xx/xx)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/PVSio
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; This code implements the functionality to load PVSio semantic
;; attachments from the files pvs-attachments and ~/.pvs-attachments
;;

(in-package :pvs)

(defparameter *pvsio-version* "PVSio-6.0.10 (xx/xx/xx)")
(defparameter *pvsio-imported* nil)
(defparameter *pvsio-update-files* (make-hash-table :test #'equal))

(defun load-update-attachments (dir filename &optional force (verbose t))
  (let* ((file (merge-pathnames dir filename)))
    (when (probe-file file)
      (let ((date    (gethash file *pvsio-update-files*))
	    (newdate (file-write-date file)))
	(when (or force (not date) (not newdate) (< date newdate))
	  (setf (gethash file *pvsio-update-files*) newdate)
	  (compile-file file
			:load-after-compile t
			:verbose verbose))))))
		
(defun libload-attachments (dir file &optional force (verbose t))
  (if (probe-file (merge-pathnames dir file))
      (load-update-attachments dir file force verbose)
    (some #'(lambda (path) 
	      (let ((lib (format nil "~a~a" path dir)))
		(load-update-attachments lib file force verbose)))
	  *pvs-library-path*)))

(defun load-imported-attachments (dir &optional force (verbose t))
  (when (or (and (not (member dir *pvsio-imported* :test #'file-equal))
		 (push dir *pvsio-imported*))
	    force)
    (libload-attachments dir "pvs-attachments" force verbose)))

(defun load-pvs-attachments (&optional force (verbose t))
  (when verbose (pvs-message "Loading semantic attachments~%"))
  (when force (initialize-prelude-attachments))
  ;;(load-imported-attachments (current-prelude-libraries) force verbose)
  ;; Was: (load-imported-attachments *all-workspace-sessions* force verbose)
  (dolist (ws *all-workspace-sessions*)
    (when (or (and (not (member (path ws) *pvsio-imported* :test #'file-equal))
		   (push (path ws) *pvsio-imported*))
	      force)
      (load-imported-attachments (path ws) force verbose)))
  (load-update-attachments "~/" ".pvs-attachments" force verbose)
  (load-update-attachments *default-pathname-defaults* "pvs-attachments" force verbose))

