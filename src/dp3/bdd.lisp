;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bdd.lisp -- 
;; Author          : Harald Ruess
;; Created On      : 1999/09/16
;;
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package dp3)

(defun ite-args-sxhash (array)
  (declare (type simple-vector array))
  (ldb (byte 29 0)
       (+ (the fixnum (node-sxhash (the node (svref array 0))))
	  (the fixnum (node-sxhash (the node (svref array 1))))
	  (the fixnum (node-sxhash (the node (svref array 2)))))))

(defvar *bdd-hash* (make-hash-table :test #'equal
				    :hash-function 'ite-args-sxhash))

(defun init-bdd ()
  (clrhash *bdd-hash*))

(defmacro with-bdd-environment ((bdd0 bdd1 type) &body body)
  "Setting up the environment for calling bdd functions."
  (let ((result (gensym)))
    `(let ((*bdd0* ,bdd0)
	   (*bdd1* ,bdd1)
	   (*type* ,type))
       (declare (special *bdd0*))
       (declare (special *bdd1*))
       (declare (special *type*))
       (assert *bdd-hash*)
       (let ((,result (multiple-value-list (progn ,@body))))
	 (values-list ,result)))))

(defun term<= (trm1 trm2)
  (declare (type node trm1)
	   (type node trm2))
  (<= (node-sxhash trm1)
      (node-sxhash trm2)))

(defun bdd-p (trm)
  (declare (type node trm)
           (special *bdd1*)
	   (special *bdd0*))
  (or (eq *bdd1* trm)
      (eq *bdd0* trm)
      (and (ite-p trm)
	   (multiple-value-bind (var pos neg)
	       (destructure-ite trm)
	     (and (not (ite-p var))
		  (bdd-p pos)
		  (bdd-p neg))))))

(defun bdd1 ()
  (declare (special *bdd1*))
  *bdd1*)

(defun bdd0 ()
  (declare (special *bdd0*))
  *bdd0*)

(defmacro bdd1-p (trm) `(eq ,trm *bdd1*))
(defmacro bdd0-p (trm) `(eq ,trm *bdd0*))

(defmacro bdd-poslit-p (trm)
  `(and (ite-p ,trm)
	(bdd1-p (if-then ,trm))
	(bdd0-p (if-else ,trm))))

(defmacro bdd-neglit-p (trm)
  `(and (ite-p ,trm)
	(bdd0-p (if-then ,trm))
	(bdd1-p (if-else ,trm))))

(defun lit-to-bdd (trm)
  (cond ((or (bdd0-p trm) (bdd1-p trm) (ite-p trm))
	 trm)
	((negation-p trm)
	 (mk-ite trm *bdd0* *bdd1* *type*))
	(t (mk-ite trm *bdd1* *bdd0* *type*))))

(defun bdd-ite (t1 t2 t3)
  (declare (type node t1)
	   (type node t2)
	   (type node t3))
  #+dbg(assert *bdd-hash*)
  (let* ((args (vector t1 t2 t3))
	 (trm (gethash args *bdd-hash*)))
    (or trm (setf (gethash args *bdd-hash*)
		  (let ((t1 (lit-to-bdd t1))
                        (t2 (lit-to-bdd t2))
                        (t3 (lit-to-bdd t3)))
		    (bdd-ite* t1 t2 t3))))))

(defun bdd-ite* (t1 t2 t3)
  (declare (type node t1)
	   (type node t2)
	   (type node t3)
           (special *bdd1*)
	   (special *bdd0*)
	   (special *type*))
  (assert (bdd-p t1))
  (assert (bdd-p t2))
  (assert (bdd-p t3))
  (cond ((eq *bdd1* t1) t2)
	((eq *bdd0* t1) t3)
	((and (eq *bdd1* t2) (eq *bdd0* t3)) t1)
	((eq t2 t3) t3)
	(t (let ((x (topvar t1 t2 t3)))
	     (multiple-value-bind (pos1 neg1)
		 (cofactors t1 x)
	       (multiple-value-bind (pos2 neg2)
		   (cofactors t2 x)
		 (multiple-value-bind (pos3 neg3)
		     (cofactors t3 x)
		   (let ((pos (bdd-ite pos1 pos2 pos3))
			 (neg (bdd-ite neg1 neg2 neg3)))
		     (if (eq pos neg) pos
			 (mk-ite x pos neg *type*))))))))))

(defun cofactors (trm x)
  (declare (type node trm)
	   (type node x)
           (special *bdd1*)
	   (special *bdd0*))
  (if (or (eq *bdd1* trm)
	  (eq *bdd0* trm))
      (values trm trm)
      (multiple-value-bind (y pos neg)
	  (destructure-ite trm)
	(if (eq y x)
	    (values pos neg)
	    (values trm trm)))))

(defun topvar (t1 t2 t3)
  (let ((x (arg 1 t1)))
    (if (ite-p t2)
	(let ((y (arg 1 t2)))
	  (if (ite-p t3)
	      (let ((z (arg 1 t3)))
		(if (term<= x y)
		    (if (term<= y z) z y)
		    (if (term<= x z) z y)))
	      (if (term<= x y) y x)))
	(if (ite-p t3)
	    (let ((z (arg 1 t3)))
	      (if (term<= x z) z x))
	    x))))

(defun mk-ite (cond then else &optional type)
  (cond ((bdd1-p cond) then)
	((bdd0-p cond) else)
	((eq then else) else)
	(t (let ((trm (mk-if-then-else cond then else)))
	     (when type
	       (setf (node-initial-type trm) type))
	     trm))))

(defun bdd-var (trm) (mk-ite trm *bdd1* *bdd0* *type*))
(defun bdd-not (trm) (bdd-ite trm *bdd0* *bdd1*))
(defun bdd-and (t1 t2) (bdd-ite t1 t2 *bdd0*))
(defun bdd-or  (t1 t2) (bdd-ite t1 *bdd1* t2))
(defun bdd-xor (t1 t2) (bdd-ite t1 (bdd-not t2) t2))
(defun bdd-implies (t1 t2) (bdd-ite t1 t2 *bdd1*))
(defun bdd-iff (t1 t2) (bdd-ite t1 t2 (bdd-not t2)))

(defun bdd-iterate (c0 c1 fn-ite bdd)
  (declare (type node bdd)
	   (type function fn-ite)
	   (special c0)
	   (special c1)
	   (special fn-ite))
  (bdd-iterate* bdd))

(defun bdd-iterate* (bdd)
  (declare (special c0)
	   (special c1)
	   (special fn-ite))
  (cond ((bdd0-p (the node bdd)) c0)
	((bdd1-p (the node bdd)) c1)
	(t (multiple-value-bind (trm pos neg)
	       (destructure-ite (the node bdd))
	     (funcall fn-ite (the node trm)
		      (bdd-iterate* pos)
		      (bdd-iterate* neg))))))

(defun bdd-every-ite (trm res1 res0)
  (declare (special fn))
  (and (funcall fn trm) res1 res0))

(defun bdd-every (fn bdd)
  (declare (special fn))
  (bdd-iterate *true* *true* #'bdd-every-ite bdd))

(defun bdd-some (fn bdd)
  (declare (special fn))
  (bdd-iterate *false* *false* #'bdd-some-ite bdd)))

(defun bdd-some-ite (trm res1 res0)
  (declare (special fn))
  (or (funcall fn trm) res1 res0))

(defun conjunct-to-bdd (trms &optional (acc *bdd1*))
  (declare (special *bdd1*))
  (assert (every #'node-p trms))
  (if (null trms) acc
      (let* ((trm (car trms))
	     (bdd (if (negation-p trm)
		      (bdd-not trm)
		      (bdd-var trm))))
	(conjunct-to-bdd (cdr trms)
			 (bdd-and bdd acc)))))

(defun disjunct-to-bdd (trms &optional (acc *bdd0*))
  (declare (special *bdd0*))
  (assert (every #'node-p trms))
  (if (null trms) acc
      (let* ((trm (car trms))
	     (bdd (if (negation-p trm)
		      (bdd-not trm)
		      (bdd-var trm))))
	(disjunct-to-bdd (cdr trms) (bdd-or bdd acc)))))

(defun bdd-solve (bdd)
  (declare (type node bdd))
  #+dbg(assert (bdd-p bdd))
  "Returns either an equivalent terminal bdd or
   a triangular form where the solved equations
   are represented by associations (var . bdd);
   hereby, var is represented as a bdd that does not
   occur in bdd."
  (catch 'fastexit
    (bdd-triangular-solve* bdd)))

(defun bdd-triangular-solve* (bdd &optional assocs)
  (declare (type node bdd)
	   (type list assocs)
	   (special *bdd1*)
	   (special *bdd0*))
  (cond ((eq bdd *bdd0*)
	 (throw 'fastexit bdd))
	((eq bdd *bdd1* bdd)
	 assocs)
	(t (multiple-value-bind (x pos neg)
	       (destructure-ite bdd)
	     (let ((new-assocs (let ((sbdd (solved-bdd pos neg)))
				 (if (eq x sbdd) assocs
				     (acons x sbdd assocs)))))
	       (bdd-triangular-solve* (bdd-or pos neg)
				      new-assocs))))))

(defun solved-bdd (pos neg)
  (declare (type node pos)
	   (type node neg)
	   (special *type*))
  (let ((delta (the node (mk-fresh-constant))))
    (setf (node-initial-type delta) *type*)
    (bdd-and pos (bdd-implies neg delta))))


;; Simplification

(defun bdd-simplify (bdd subst)
  (with-pushed-cong-state ()
      (loop for (x . e) in subst
	    do (invoke-process (mk-equality x e) (cs)))
	(bdd-simplify* bdd)))

(defun bdd-simplify* (bdd)
  (declare (special *type*))
  (if (or (bdd0-p bdd) (bdd1-p bdd)) bdd
      (multiple-value-bind (var pos neg)
	  (destructure-ite bdd)
	(let ((new-var (canon trm (cs))))
	  (cond ((bdd0-p new-var) (bdd0))
		((bdd1-p new-var) (bdd1))
		(t (mk-ite new-trm
			   (bdd-simplify* pos)
			   (bdd-simplify* neg)
			   *type*)))))))
	  









