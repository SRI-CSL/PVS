(in-package dp)

;; Interface

(defmacro bdd-0 () '(pvs::bdd_0))
(defmacro bdd-1 () '(pvs::bdd_1))

(defmacro bdd-create-var (n) `(pvs::bdd_create_var ,n))
(defmacro bdd-not (b) `(pvs::bdd-not ,b))
(defmacro bdd-ite (b1 b2 b3) `(pvs::bdd-ite ,b1 ,b2 ,b3))
(defmacro bdd-and (b1 b2) `(pvs::bdd-and ,b1 ,b2))
(defmacro bdd-or (b1 b2) `(pvs::bdd-or ,b1 ,b2))
(defmacro bdd-implies (b1 b2) `(pvs::bdd-implies ,b1 ,b2))
(defmacro bdd-equiv (b1 b2) `(pvs::bdd-equiv ,b1 ,b2))

(defmacro bdd-and* (l) `(pvs::bdd-and* ,l))
(defmacro bdd-or* (l) `(pvs::bdd-or* ,l))

(defmacro bdd-0? (b) `(pvs::bdd-0? ,b))
(defmacro bdd-1? (b) `(pvs::bdd-1? ,b))
(defmacro bdd-X? (b) `(pvs::bdd-X? ,b))
(defmacro bdd-void? (b) `(pvs::bdd-void? ,b))
(defmacro bdd-lit? (b) `(pvs::bdd-lit? ,b))
(defmacro bdd-poslit? (b) `(pvs::bdd-poslit? ,b))
(defmacro bdd-neglit? (b) `(pvs::bdd-neglit? ,b))
(defmacro bdd-term? (b) `(pvs::bdd-term? ,b))

(defmacro bdd-equal? (b1 b2) `(pvs::bdd-equal? ,b1 ,b2))

(defmacro bdd-varid (b) `(pvs::bdd_varid ,b))
(defmacro bdd-then (b) `(pvs::bdd_then ,b))
(defmacro bdd-else (b) `(pvs::bdd_else ,b))
(defmacro bdd-top-var (b) `(pvs::bdd_top_var ,b))
(defmacro bdd-cofactor-pos (b) `(pvs::bdd_cofactor_pos_ ,b))
(defmacro bdd-cofactor-neg (b) `(pvs::bdd_cofactor_neg_ ,b))

(defmacro list-first (l) `(pvs::list_first ,l))
(defmacro list-next (l) `(pvs::list_next ,l))
(defmacro elem-contents (l) `(pvs::elem_contents ,l))
(defmacro bdd-sum-of-cubes (b irredundant?) `(pvs::bdd_sum_of_cubes ,b ,irredundant?))

(defmacro bdd-quit () '(pvs::bdd_quit))

(defun destructure-bdd (bdd)
  (values (bdd-top-var bdd)
	  (bdd-cofactor-pos bdd)
	  (bdd-cofactor-neg bdd)))

(defun bdd-some (pred b)
  "Does some node in the bdd b fulfill predicate pred"
  (if (or (bdd-1? b) (bdd-0? b)) nil
      (multiple-value-bind (var pos neg)
	  (destructure-bdd b)
	(or (funcall pred (bdd-varid var))
	    (bdd-some pred pos)
	    (bdd-some pred neg)))))

(defun list-map (fn l &optional acc)
  (if (zerop l)
      (nreverse acc)
      (list-map fn (list-next l)
		(cons (funcall fn (elem-contents (list-first l)))
		      acc))))

;; Solving BDDs

(defun bdd-solve (bdd *new-constant-fn*)
  "Returns either an equivalent terminal bdd or
   a triangular form where the solved equations
   are represented by associations (var . bdd);
   hereby, var is represented as a bdd that does not
   occur in bdd."
  (declare (special *new-constant-fn*))
  (catch 'fastexit
    (bdd-triangular-solve* bdd)))

(defun bdd-triangular-solve* (bdd &optional assocs)
  (cond ((bdd-0? bdd)
	 (throw 'fastexit bdd))
	((bdd-1? bdd)
	 assocs)
	(t (multiple-value-bind (var pos neg)
	       (destructure-bdd bdd)
	     (let ((new-assocs (let ((sbdd (solved-bdd pos neg)))
				 (if (bdd-equal? var sbdd) assocs
				     (acons var sbdd assocs)))))
	       (bdd-triangular-solve* (bdd-or pos neg)
				      new-assocs))))))

(defun solved-bdd (pos neg)
  (declare (special *new-constant-fn*))
  (let ((delta (bdd-create-var
		(funcall *new-constant-fn*))))
    (bdd-and pos (bdd-implies neg delta))))


;; Symbol table (Alists)

(defvar *trm-bdd-table* nil)
(defvar *bdd-trm-table* nil)

(defun bdd-init ()
  (unless pvs::*bdd-initialized*
    (pvs::bdd_init))
  (pvs::set_bdd_do_dynamic_ordering 0)
  (setf *trm-bdd-table* nil)
  (setf *bdd-trm-table* nil))

(defun new-id ()
  (funcall pvs::*bdd-counter*))

(defun add-translation (trm id)
  (push (cons trm id) *trm-bdd-table*)
  (push (cons id trm) *bdd-trm-table*))

(defun bdd-var (trm)
  (assert (node-p trm))
  (let ((id (or (lookup-id-of trm)
		(let ((newid (new-id)))
		  (add-translation trm newid)
		  newid))))
    (bdd-create-var id)))

(defun new-bdd-id ()
  (let ((newid (new-id))
	(newconst (mk-fresh-constant)))
    (add-translation newconst newid)
    newid))

(defun lookup-trm-of (id)
  (let ((trm (assoc id *bdd-trm-table*)))
    (assert trm)
    (cdr trm)))

(defun lookup-id-of (trm)
  (cdr (assoc trm *trm-bdd-table*)))











