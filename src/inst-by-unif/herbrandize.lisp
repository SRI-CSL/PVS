(in-package :pvs)

(defun herbrandize (fmlas &optional xs renamings always-skolemize relativize)
  #+dbg(assert (every #'expr? fmlas))
  (let ((*always-skolemize* always-skolemize)
	(*relativize* relativize)
	(*vars* nil))
    (declare (special *always-skolemize*)
	     (special *relativize*)
	     (special *vars*))
    (herbrandize* fmlas xs renamings)))

(defmethod herbrandize* ((fmlas null) xs subst)
  nil)

(defmethod herbrandize* ((fmlas cons) xs subst)
  (multiple-value-bind (term xs1 subst1)
      (herbrandize* (car fmlas) xs subst)
    (multiple-value-bind (terms xs2 subst2)
	(herbrandize* (cdr fmlas) xs1 subst1)
      (values (cons term terms) xs2 subst2))))

(defmethod herbrandize* ((fmla negation) xs subst)
  (multiple-value-bind (term ys subst1)
      (skolemize* (args1 fmla) xs subst)
    (values (make!-negation term) ys subst1)))

(defmethod herbrandize* ((fmla disjunction) xs subst)
  (multiple-value-bind (term1 xs1 subst1)
      (herbrandize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(herbrandize* (args2 fmla) xs1 subst1)
      (values (make!-disjunction term1 term2) xs2 subst2))))

(defmethod herbrandize* ((fmla conjunction) xs subst)
  (multiple-value-bind (term1 xs1 subst1)
      (herbrandize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(herbrandize* (args2 fmla) xs1 subst1)
      (values (make!-conjunction term1 term2) xs2 subst2))))

(defmethod herbrandize* ((fmla iff-or-boolean-equation) xs subst)
    (herbrandize* (expand-iff fmla) xs subst))

(defmethod herbrandize* ((fmla implication) xs subst)
 (multiple-value-bind (term1 xs1 subst1)
      (skolemize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(herbrandize* (args2 fmla) xs1 subst1)
      (values (make!-implication term1 term2) xs2 subst2))))

(defmethod herbrandize* ((fmla expr) xs subst)
  (values (substit fmla subst) xs subst))

(defmethod herbrandize* ((fmla forall-expr) xs subst)
  (declare (special *relativize*))
  (let* ((fmla (relativize-quantifier fmla *relativize*))
	 (bndngs (bindings fmla)))
    (unless (safe-to-skolemize? bndngs)
      (throw 'not-skolemizable nil))
    (let ((assocs (mapcar #'(lambda (bndng)
			      (cons bndng (fresh-skoterm bndng xs)))
		    bndngs)))
      (herbrandize* (expression fmla)
		    xs
		    (append assocs subst)))))

(defmethod herbrandize* ((fmla exists-expr) xs subst)
  (declare (special *relativize*))
  (let* ((fmla (relativize-quantifier fmla *relativize*))
	 (bndngs (bindings fmla)))
    (let* ((domain (mapcar #'car subst))
	   (newvars (mapcar #'(lambda (bndng)
				(fresh-variable bndng domain))
		      bndngs)))
      (herbrandize* (expression fmla)
		    (union newvars xs)
		    (append (pairlis bndngs newvars) subst)))))
	
(defmethod skolemize* ((fmla expr) xs subst)
    (values (substit fmla subst) xs subst))

(defmethod skolemize* ((fmla negation) xs subst)
  (multiple-value-bind (term xs1 subst1)
      (herbrandize* (args1 fmla) xs subst)
    (values (make!-negation term) xs1 subst1)))

(defmethod skolemize* ((fmla disjunction) xs subst)
  (multiple-value-bind (term1 xs1 subst1)
      (skolemize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(skolemize* (args2 fmla) xs1 subst1)
      (values (make!-disjunction term1 term2) xs2 subst2))))

(defmethod skolemize* ((fmla conjunction) xs subst)
  (multiple-value-bind (term1 xs1 subst1)
      (skolemize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(skolemize* (args2 fmla) xs1 subst1)
      (values (make!-conjunction term1 term2) xs2 subst2))))

(defmethod skolemize* ((fmla iff-or-boolean-equation) xs subst)
    (skolemize* (expand-iff fmla) xs subst))

(defmethod skolemize* ((fmla implication) xs subst)
  (multiple-value-bind (term1 xs1 subst1)
      (herbrandize* (args1 fmla) xs subst)
    (multiple-value-bind (term2 xs2 subst2)
	(skolemize* (args2 fmla) xs1 subst1)
      (values (make!-implication term1 term2) xs2 subst2))))

(defmethod skolemize* ((fmla forall-expr) xs subst)
  (declare (special *relativize*))
  (let* ((fmla (relativize-quantifier fmla *relativize*))
	 (bndngs (bindings fmla))
         (domain (mapcar #'car subst))
	 (newvars (mapcar #'(lambda (bndng)
			      (fresh-variable bndng domain))
		    bndngs)))
    (skolemize* (expression fmla)
		(union newvars xs)
		(append (pairlis bndngs newvars) subst))))
		      
(defmethod skolemize* ((fmla exists-expr) xs subst)
  (declare (special *relativize*))
  (let* ((fmla (relativize-quantifier fmla *relativize*))
	 (bndngs (bindings fmla)))
    (unless (safe-to-skolemize? bndngs)
      (throw 'not-skolemizable nil))
    (let ((assocs (mapcar #'(lambda (bndng)
			      (cons bndng (fresh-skoterm bndng xs)))
		      bndngs)))
      (skolemize* (expression fmla)
		  xs
		  (append assocs subst)))))

(defun safe-to-skolemize? (bndngs)
  (declare (special *always-skolemize*))
  (or *always-skolemize*
      (every #'(lambda (bndng)
		 (nonempty? (type bndng)))
	     bndngs)))

(defun relativize-quantifier (fmla &optional relativize)
  (if relativize
      (lift-predicates-in-quantifier fmla
				     (list *naturalnumber* *integer*))
    fmla))

(defun expand-iff (fmla)
  (make!-conjunction (make!-implication (args1 fmla) (args2 fmla))
		     (make!-implication (args2 fmla) (args1 fmla))))

;; Make a new variable

(defun fresh-variable (bndng &optional bndngs)
  (let ((id (unique-id (id bndng) #\? bndngs)))
     (mk-name-expr id nil nil
		   (make-resolution bndng
		     (current-theory-name) (type bndng)))))

(defun fresh-skoterm (bndng &optional args)
  #+dbg(assert (typep bndng 'bind-decl))
  #+dbg(assert (and (listp args) (every #'variable? args)))
  (let ((type (if (null args) (type bndng)
		  (mk-funtype (mapcar #'type args) (type bndng)))))
    (if (null args)
	(fresh-skofun bndng type)
      (make!-application* (fresh-skofun bndng type) args))))

(defun fresh-skofun (bndng type)
   (let ((id (unique-id (id bndng) #\! nil)))
     (put-decl (make-instance 'const-decl    ; destructive update of current context
		              'id id
			      'type type
			      'module (module *current-context*))
	    (declarations-hash *current-context*))
     (change-class (typecheck (pc-parse id 'expr)
		     :expected type
		     :context *current-context*)
		'name-expr)))

;; Fresh identifier

(defvar *counter* 0)

(defun unique-id (name ch bndngs)
  (let ((*bndngs* bndngs))
    (declare (special *bndngs*))
    (unique-id* name ch)))

(defun unique-id* (name ch)
  "Generating a fresh (relative to a context) identifier of the form 'name^ch^counter'"
  (let ((id (generate-id name ch)))
    (if (or (declared? id *current-context*)
	    (some #'(lambda (bndng)
		      (eq (id bndng) id))
		  *bndngs*))
        (unique-id* name ch)
       id)))

(defun generate-id (name ch)
  (flet ((special-char-p (x)
	     (member x (list ch #\_))))
    (let* ((str (string (op-to-id name)))
	   (pos (position-if #'special-char-p str :from-end T))
	   (prefix (subseq str 0 pos))
	   (suffix (when pos (subseq str (1+ pos))))
	   (prestr (if (and pos (every #'digit-char-p suffix))
			  prefix
                        str)))
      (prog1
	(intern (format nil "~a~a~a" prestr ch *counter*))
	(setf *counter* (1+ *counter*))))))
