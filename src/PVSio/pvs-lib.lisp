;;
;; pvs-lib.lisp
;; Release: PVSio-6.0 (12/12/12)
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

(defparameter *pvsio-version* "PVSio-6.0 (12/12/12)")
(defparameter *pvsio-imported* nil)
(defparameter *pvsio-update-files* (make-hash-table :test #'equal))

(defun load-update (file &optional force (verbose t))
  (when (and (> (length file) 0)
	     (probe-file file))
    (let ((date    (gethash file *pvsio-update-files*))
	  (newdate (file-write-date file)))
      (when (or force
		(not date)
		(not newdate)
		(< date newdate))
	(setf (gethash file *pvsio-update-files*) newdate)
	(load file :verbose verbose)))))
		
(defun libload-update (file &optional force (verbose t))
  (when (> (length file) 0)
    (cond ((or (char= (elt file 0) #\/) 
	       (char= (elt file 0) #\~) 
	       (string= (directory-namestring file) "./"))
	   (load-update file force verbose))
	  (t
	   (some #'(lambda (path) 
		     (load-update (format nil "~a~a" path file) force verbose))
		 *pvs-library-path*)))))

(defun load-imported (h &optional force (verbose t))
  (maphash #'(lambda (k e) 
	       (when (or (and (not (member k *pvsio-imported* :test #'string=))
			      (push k *pvsio-imported*)
			      (equal e e))
			 force)
		 (libload-update (format nil "~apvs-attachments" k) 
				 force verbose)))
	   h))

(defun load-pvs-attachments (&optional force (verbose t))
  (load-imported *prelude-libraries* force verbose)
  (load-imported *imported-libraries* force verbose)
  (load-update "~/.pvs-attachments" force verbose)
  (load-update (format nil "~apvs-attachments" *pvs-context-path*) 
	       force verbose))

