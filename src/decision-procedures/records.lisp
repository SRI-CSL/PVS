(in-package dp)

(defun record-p (term)
  (and (application-p term)
       (record-op-p (funsym term))))

(defun tuple-p (term)
  (and (application-p term)
       (tuple-op-p (funsym term))))

(defun project-p (term)
  (and (application-p term)
       (project-op-p (funsym term))))

(defun sigproject (term &optional cong-state)
  (let ((record (arg 2 term))
	(index (arg 1 term)))
    (cond ((record-p record)
	   (arg (1+ (constant-id index)) record))
	  ((tuple-p record)
	   (arg (1+ (constant-id index)) record))
	  (t term))))
