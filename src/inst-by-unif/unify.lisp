(in-package :dp)

;; Top-Level Unification

(defun unify (t1 t2 cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (catch 'fail
      (progn
	(unif-closure t1 t2)
	(consistent? t1 *cs*)
	*cs*))))

;; Unification Closure

(defun dp-union* (t1 t2)
  (let ((equality (canon (mk-equality t1 t2) *cs*)))
    (if (false-p equality)
	(throw 'fail :inconsistent)
      (let ((solved-form (solve equality *cs*)))
	(if (false-p solved-form)
	    (throw 'fail :inconsistent)
	    (loop for eqn in solved-form
		  do (when (equality-p eqn)
		       (dp-union (lhs eqn) (rhs eqn) *cs*))))))))

(defun unif-closure (t1 t2)
  (let ((s1 (dp-find (canon t1 *cs*) *cs*))
	(s2 (dp-find (canon t2 *cs*) *cs*)))
    (unless (eq s1 s2)
      (let ((r1 (dp-schema s1 *cs*))
	    (r2 (dp-schema s2 *cs*)))
	(cond ((and (application-p r1)
		    (application-p r2))
	       (multiple-value-bind (op1 args1)
		   (destructure-application r1)
		 (multiple-value-bind (op2 args2)
		     (destructure-application r2)
		   (if (funsym= op1 op2)
		       (progn
			 (dp-union* s1 s2)
			 (loop for a1 in args1
			       as a2 in args2
			       do (unif-closure a1 a2)))
		       (throw 'fail :clash)))))
	      ((and (constant-p r1)
		    (constant-p r2))
	       (unless (eq (constant-id r1)
			   (constant-id r2))
		 (throw 'fail :clash)))
	      ((and (application-p t1) (uninterp? t1)
		    (constant-p t2))
	       (throw 'fail :clash))
	      ((and (application-p t2) (uninterp? t2)
		    (constant-p t1))
	       (throw 'fail :clash))
	      (t
	       (dp-union* s1 s2)))))))

(defun destructure-application (trm &optional args)
  (if (application-p trm)
      (destructure-application (funsym trm)
			       (append (funargs trm) args))
    (values trm args)))

(defun funsym= (f1 f2)
  (eq f1 f2))

;; Finding a solution from the unification closure

(defun consistent? (s cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (unwind-protect
	(progn
	  (init-visited)
	  (init-acyclic)
	  (consistent* s))
      (progn
	(init-visited)
	(init-acyclic)))))

(defun consistent* (s)
  (when (constant-p s)
    (return-from consistent*))
  (let ((s (dp-schema (dp-find (canon s *cs*) *cs*) *cs*)))
    (when (constant-p s)
      (return-from consistent*))
    (when (gethash s *acyclic*)
       (return-from consistent*))
    (when (gethash s *visited*)
       (throw 'fail :cycle))
    (when (application-p s)
      (setf (gethash s *visited*) T)
      (loop for a in (funargs s)
	    do (consistent* a))
      (setf (gethash s *visited*) NIL))
    (setf (gethash s *acyclic*) T)))

(defvar *visited* nil)
(defvar *acyclic* nil)

(defun init-visited ()
  (if *visited*
      (clrhash *visited*)
      (setf *visited* (dp-make-eq-hash-table))))

(defun init-acyclic ()
  (if *acyclic*
      (clrhash *acyclic*)
      (setf *acyclic* (dp-make-eq-hash-table))))

;; Generating an mgu

(defun make-subst (xs cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (make-subst* xs nil)))

(defun make-subst* (xs subst)
  (if (null xs)
      (nreverse subst)
    (make-subst* (cdr xs)
		 (let* ((x (car xs))
			(e (instance x *cs*)))
		   (if (eq x e) subst
		       (acons x e subst))))))

(defun instance (e cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (instance* e)))

(defun instance* (e)
;  (assert (consistent? e *cs*))
  (let ((s (dp-schema (dp-find (canon e *cs*) *cs*) *cs*)))
    (cond ((dp-variable-p s) s)
	  ((application-p s)
	   (mk-term (cons (funsym s)
			  (mapcar #'instance*
			      (funargs s)))))
	  (t s))))

(defun instantiated? (e cs)
  (let ((*cs* cs))
    (declare (special *cs*))
    (null (vars-of (instance* e)))))

(defun subst= (xs cs1 cs2)
  (every #'(lambda (x)
	     (eq (instance x cs1)
		 (instance x cs2)))
	 xs))

