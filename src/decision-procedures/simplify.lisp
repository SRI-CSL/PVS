(in-package 'dp)


(defun cond-rewrite-+ (cond)
  (cond
   ((null cond) (break))
   ((not (consp cond)) (break))
   ((eq (funsym cond) 'equal)
    (if (subtermof (arg1 cond) (arg2 cond))
	`(equal ,(arg2 cond) ,(arg1 cond))
	cond))
   (t (or t (break))
      `(equal ,cond TRUE))))

(defun enumeration-equality (atf)
  (and (consp atf)
       (eq (funsym atf) 'equal)
       (cond
	((some #'(lambda (distinct-list)
		   (member (arg2 atf) distinct-list :test #'equal))
	       *distinct-lists*)
	 atf)
	((some #'(lambda (distinct-list)
		   (member (arg2 atf) distinct-list :test #'equal))
	       *distinct-lists*)
	 `(equal ,(arg2 atf) ,(arg1 atf)))
	(t nil))))

(defun cond-rewrite-- (cond)
  (cond
   ((null cond) (break))
   ((not (consp cond)) (break))
   (t `(equal ,cond FALSE))))

(defun normalize-term* (term rewrites)
  (let ((*rew-hash* (make-hash-table :test #'equal)))
    (normalize-term term rewrites)))

(defun simplify-ite-top (ite rewrites &optional (depth-lim -1))
  (simplify-ite ite rewrites depth-lim))

(defun simplify-ite (ite rewrites depth-lim)
  (cond
   ((= depth-lim 0) ite)
   ((not (application-p ite)) ite)
   ((not (eq (funsym ite) *IF*))
    (cons (funsym ite)
	  (simplify-ite-args (funargs ite) rewrites depth-lim)))
   (t (let* ((cond (arg1 ite))
	     (cond+ (cond-rewrite-+ cond))
	     (rewrites+ (cons cond+ rewrites))
	     (norm-then (normalize-term* (arg2 ite) rewrites+))
	     (simp-then (simplify-ite norm-then rewrites+ (1- depth-lim)))
	     (cond- (cond-rewrite-- cond))
	     (rewrites- (cons cond- rewrites))
	     (norm-else (normalize-term* (arg3 ite) rewrites-))
	     (simp-else (simplify-ite norm-else rewrites- (1- depth-lim))))
	(if (equal simp-then simp-else)
	    simp-then
	    (make-if-then-else cond simp-then simp-else))))))

(defun simplify-ite-args (ite-args rewrites depth-lim)
  (loop for arg in ite-args
	collect (simplify-ite arg rewrites depth-lim)))

(defun simplify-ite-fast (ite rewrites depth-lim)
  (cond
   ((= depth-lim 0) ite)
   ((not (consp ite)) ite)
   ((not (eq (funsym ite) 'IF)) ite)
   (t (let* ((cond (arg1 ite))
	     (cond+ (cond-rewrite-+ cond))
	     (rewrites+ (cons cond+ rewrites))
	     (norm-then (normalize-term* (arg2 ite) rewrites+))
	     (simp-then (simplify-ite norm-then rewrites+ (1- depth-lim)))
	     (cond- (cond-rewrite-- cond))
	     (rewrites- (cons cond- rewrites))
	     (norm-else (normalize-term* (arg3 ite) rewrites-))
	     (simp-else (simplify-ite norm-else rewrites- (1- depth-lim))))
	(if (eq simp-then simp-else)
	    simp-then
	    (make-if-then-else cond simp-then simp-else))))))
