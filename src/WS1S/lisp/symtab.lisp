(in-package :pvs)

(let ((*index* 0)
      (*size* 0)
      (*bvars* nil)
      (*symtab* nil))

  (defun symtab-init ()
    (setf *index* 0)
    (setf *size* 0)
    (setf *bvars* nil)
    (setf *symtab* nil))

  (defun symtab () *symtab*)
  
  (defun size () *size*)
	
  (defun symtab-new-index ()
     (setf *index* (1+ *index*)))
  
  (defun symtab-index (expr)
    (let ((result (rassoc expr *symtab* :test #'tc-eq)))
      (if result
	  (values (car result) :lookup)
	(values (symtab-add-index expr) :new))))

  (defun symtab-expr (index)
    (cdr (assoc index *symtab* :test #'=)))
  
  (defun symtab-add-index (expr)
    (prog1
      (setf *index* (1+ *index*))
      (setf *size* (1+ *size*))
      (setf *symtab* (acons *index* expr *symtab*))))

  (defun symtab-shadow (expr)
    (prog1
      (setf *index* (1+ *index*))
      (push expr *bvars*)
      (setf *symtab* (acons *index* expr *symtab*))))
   
  (defun symtab-unshadow (expr)
    (flet ((test (expr1) (tc-eq expr expr1)))
      (pop *bvars*)
      (setf *symtab* (delete-if #'test *symtab* :key #'cdr :count 1))))

  (defun symtab-remove (expr)
    (setf *size* (1- *size*))
    (setf *symtab* (remove-if #'(lambda (entry) (tc-eq (cdr entry) expr)) *symtab*)))
  
  (defun symtab-strip ()
    (let ((offsets (make-array *size* :element-type 'fixnum))
	  (fvars   (make-array *size* :element-type 'string))
	  (types   (make-string *size*))
	  (i       0))
      (loop for (index . expr) in *symtab* do
		   (setf (elt offsets i) index)
		   (setf (elt fvars i) (format nil "~a" expr))
                   (let* ((level (level expr))
			  (char (cond ((eql level 0) #\0)
				      ((eql level 1) #\1)
				      ((eql level 2) #\2))))
			 (setf (elt types i) char))
		   (setf i (1+ i)))
      (values *symtab* *size* offsets fvars types)))

  (defun shielding? (expr)
    nil)
     ; (intersection *bvars* (freevars expr) :test #'tc-eq))

  (defun fvars (l &optional acc)
    (if (null l)
	(nreverse acc)
      (let* ((entry (car l))
	     (newacc (cons (cdr entry) acc)))
	(fvars (cdr l) newacc))))
)
