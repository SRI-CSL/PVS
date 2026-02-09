;;
;; pvs-lib.lisp
;; Release: PVSio-8.0 (10/13/2023)
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

(defparameter *pvsio-version* "8.0 (December 31, 2025)")

(defun pvsio-version ()
  (pvs-message *pvsio-version*))

(defvar *pvsio-loaded-files* (make-hash-table :test #'equal)
  "Hash table of loadedfile's indexed by their path")

(defstruct loadedfile
  ;; UTC time when the file was loaded.
  date
  ;; Association list of theory id and tc-time (if type-checked), of theories
  ;; with attachments in this file. Theory identifiers are strings with a fully qualified
  ;; theory id, i.e., "<lib>@<theory>".
  theories
)

(defvar *pvs-attachment-source-file* nil
  "The value of this global variable should always be nil, except when locally
used by load-pvs-attachment. In this case, it is set to a pair where the
first elelement is the source file and the second element is either nil (instructing
PVSio to reload the attachments in the file) or the association list in the field theories,
which is used by PVSio to only load the attachement of theories that have been
re-typechecked since the last load.")

(defun outdated-sourcefile (source)
  "Update source file information in *pvsio-loaded-files* if currently loaded file is outdated.
Assume source is not NIL."
  (let ((newdate    (file-write-date source))
	(loadedfile (gethash source *pvsio-loaded-files*)))
    (cond ((null loadedfile)
	   (setf (gethash source *pvsio-loaded-files*)
		 (make-loadedfile :date newdate :theories nil))
	   t)
	  ((< (loadedfile-date loadedfile) newdate)
	   (setf (loadedfile-date loadedfile) newdate)
	   (setf (loadedfile-theories loadedfile) nil)
	   t))))

(defun outdated-theories (source)
  "Return association list of theories and type-check times, if one of them is outdated.
Assume source is not NIL."
  (let ((loadedfile (gethash source *pvsio-loaded-files*)))
    (when loadedfile
      (let ((theories (loadedfile-theories loadedfile)))
	(when (some (lambda (theo-date)
		      (let* ((theory (extra-get-theory (car theo-date)))
			     (date   (cdr theo-date)))
			(when theory
			  (or (null date)
			      (< date (typecheck-time theory))))))
		    theories)
	  (setf (loadedfile-theories loadedfile) nil)
	  theories)))))

(defun outdated-theory (theory theories)
  "Return T if theory is not in theories or is outdated with respect to type-check time.
Asume theory is not NIL."
  (let* ((theo-date (assoc (extra-qid-theory theory) theories :test #'string=))
	 (date      (cdr theo-date))
	 (newdate   (typecheck-time theory)))
    (or (null date) (< date newdate))))

(defun add-updated-theory (theory loadedfile)
  "Add qid of theory to loadfile with current type-check time.
Assume theory is not NIL."
  (when loadedfile
    (let* ((theoryid  (extra-qid-theory theory))
	   (theories  (loadedfile-theories loadedfile))
	   (theo-date (assoc theoryid theories :test #'string=)))
      (unless theo-date
	(let ((new-theories (acons theoryid (typecheck-time theory) theories)))
	  (setf (loadedfile-theories loadedfile) new-theories))))))

(defun add-dangling-theory (theoryid loadedfile)
  "Add theoryid to loadfile, without type-check time.
Assume loadedfile is not null and there is not theory with theoryid in current context."
  (let* ((theories  (loadedfile-theories loadedfile))
	 (theo-date (assoc theoryid theories :test #'string=)))
    (unless theo-date
      (let ((new-theories (acons theoryid nil theories)))
	(setf (loadedfile-theories loadedfile) new-theories)))))

(defun load-pvs-attachment (file &optional force (verbose t))
  "Load file containing PVS attachment. FILE is provided as an absolute path, e.g.,
using make-pathname or merge-pathnames."
  (let ((source (probe-file file)))
    (when source
      (let ((outdated-src (outdated-sourcefile source)))
	(if (or force outdated-src)
	    (let ((*pvs-attachment-source-file* (cons source nil)))
	      (load (compile-file source :verbose verbose)))
	    (let ((outdated-ths (outdated-theories source)))
	      (when outdated-ths
		(let ((*pvs-attachment-source-file* (cons source outdated-ths)))
		  (load (compile-file source :verbose verbose)))))))
      (list source))))

(defun reset-pvs-attachments ()
  "Initialize all global variables related to PVSio attachments."
  (clrhash *pvsio-loaded-files*)
  (clrhash *pvsio-attachments*)
  (setq *pvs-attachment-source-file* nil))

(defun pvsio-internal-theories (theories)
  (mapcan (lambda (theodate)
	    (let ((theory (car theodate))
		  (date   (if (cdr theodate) (date-string (cdr theodate))
			      "<non-typechecked>")))
	      (list theory date)))
	  theories))

(defun print-pvsio-internals ()
  (cond ((= (hash-table-count *pvsio-loaded-files*) 0)
	 (format t "No loaded pvs-attachment files~%"))
	(t
	 (format t "Loaded pvs-attachment files:~%")
	 (maphash (lambda (source loadedfile)
		    (let ((date     (date-string (loadedfile-date loadedfile)))
			  (theories (pvsio-internal-theories (loadedfile-theories loadedfile))))
		      (format t "  ~a (~a)~@[:~{~%    ~a - ~a~}~]~%" source date theories)))
		  *pvsio-loaded-files*))))

(defun purge-attachment-files (found-files)
  "Remove attachment files from *pvsio-loaded-files* that are no longer found."
  (maphash #'(lambda (key attach)
	       (declare (ignore attach))
	       (unless (member key found-files :test #'equal)
		 (remhash key *pvsio-loaded-files*)))
	   *pvsio-loaded-files*))

(defun attachment-source-not-found (source)
  "Return T if source is not NIL and doesn't exist in *pvsio-loaded-files*."
  (when source
    (not (gethash source *pvsio-loaded-files*))))

(defun purge-pvsio-attachments ()
  "Remove attachments that are outdated because the theory has been re-typechecked
since they were loaded."
  (maphash #'(lambda (key attach)
	       (let ((theory (extra-get-theory (attachment-theo-qid attach))))
		 (when (or (attachment-source-not-found (attachment-source attach))
			   (not theory)
			   (< (attachment-tc-time attach) (typecheck-time theory)))
                   (remhash key *pvsio-attachments*))))
             *pvsio-attachments*))


(defun all-imported-paths ()
  (let ((current-th (current-theory)))
    (when current-th
      (append (remove-duplicates (mapcar #'context-path (all-imported-theories current-th))
				 :test #'file-equal)
	      (list (context-path current-th))))))

(defun load-pvs-attachments (&optional force (verbose t))
  (when verbose
    (pvs-message "Loading semantic attachments ~:[~;(force: t)~]~%" force))
  (when force
    (reset-pvs-attachments))
  (when (= (hash-table-count *pvsio-attachments*) 0)
    (initialize-prelude-attachments))
  (let ((found-files
	 (append
	  (loop for path in (all-imported-paths)
		append (load-pvs-attachment
			(merge-pathnames "pvs-attachments" path) force verbose))
	  (load-pvs-attachment (merge-pathnames ".pvs-attachments" "~/") force verbose))))
    (purge-attachment-files found-files)
    (purge-pvsio-attachments)))
