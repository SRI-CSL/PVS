(in-package :pvs)

(let ((*index* 0)
      (*size* 0)
      (*bvars* nil)
      (*symtab* nil))

  (defun symtab-init ()
    (setf *index* 0)
    (setf *size* 0)
    (setf *bvars* nil)
    (if *symtab*
	(clrhash *symtab*)
	(setf *symtab* (make-hash-table :hash-function 'pvs-sxhash
					:test 'tc-eq))))
  
  (defun symtab-new-index ()
     (setf *index* (1+ *index*)))
  
  (defun symtab-index (expr)
    (or (gethash expr *symtab*)
	(symtab-add-index expr)))
	
  (defun symtab-add-index (expr)
    (prog1
      (setf *index* (1+ *index*))
      (setf *size* (1+ *size*))
      (setf (gethash *symtab* expr) *index*)))

  (defun symtab-shadow (expr)
    (assert (typep expr 'name-expr))
    (prog1
      (symtab-add-index expr)
      (push expr *bvars*)))

  (defun symtab-shadow* (exprs &optional acc)
    (if (null exprs)
	(nreverse acc)
      (symtab-shadow* (cdr exprs)
		      (cons (symtab-shadow (car exprs)) acc))))
   
  (defun symtab-unshadow ()
    (pop *bvars*))

  (defun symtab-unshadow* (n)
    (loop for i from 1 upto n
       do (symtab-unshadow)))
  
  (defun symtab-strip ()
    (assert (null *bvars*))
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
    (some #'(lambda (bvar)
	      (occurs-in bvar expr))
	  *bvars*))

  (defun fvars (l &optional acc)
    (if (null l)
	(nreverse acc)
      (let* ((entry (car l))
	     (newacc (cons (cdr entry) acc)))
	(fvars (cdr l) newacc))))
)

