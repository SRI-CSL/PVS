(in-package dp3)

(defun copy-hash-table (ht)
  (let* ((test (hash-table-test ht))
	 (size (floor (hash-table-size ht) 1.5384616))
	 (new-ht (if (memq test '(eq eql equal equalp))
		     (make-hash-table :test test :size size)
		     (make-hash-table :test test :size size
				      :hash-function 'dp-sxhash))))
    (maphash #'(lambda (id data)
		 (setf (gethash id (the hash-table new-ht)) data))
	     (the hash-table ht))
    new-ht))

(defun copy-array (arr)
  (let ((size (array-total-size new-array))
	(new-arr (make-array size)))
    (loop for i from 0 below size
	  do (setf (svref (the array new-arr) i) (svref (the array arr) i)))
    new-arr))

