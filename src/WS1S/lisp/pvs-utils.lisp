(in-package :pvs)


(defun mk-expr (str &key expected)
    (if expected
        (let ((typ (if (stringp expected)
                      (pc-typecheck (pc-parse expected 'type-expr))
                    expected)))
          (pc-typecheck (pc-parse str 'expr) :expected typ))
      (pc-typecheck (pc-parse str 'expr))))

(defun bindings-to-var (bndngs)
  (mapcar #'make-variable-expr bndngs))

(defun destructure-binding (bndng &key exclude)
  (assert (typep bndng 'bind-decl))
  (let ((x (make-variable-expr bndng)))
    (multiple-value-bind (supertype preds)
	(destructure-type x :exclude exclude)
      (values x supertype preds))))
	
(defun destructure-type (expr &key exclude (all? 'T))
  (let ((jtypes (judgement-types+ expr))
	(*subtypes-seen* nil)
	(*expr* expr)
	(*all* all?)
	(*stop-types* (if (listp exclude) exclude (list exclude))))
    (declare (special *expr*)  
             (special *subtypes-seen*)
	     (special *all*)
	     (special *stop-types*))
    (destructure-type* (or jtypes (type *expr*)) nil)))

(defmethod destructure-type* ((types cons) preds)
  (destructure-type-list types nil preds))

(defun destructure-type-list (types type preds)
  (if (null types)
      (values type preds)
      (multiple-value-bind (car-type car-preds)
	  (destructure-type* (car types) nil)
	(destructure-type-list
	 (cdr types)
	 (if type
	     (compatible-type car-type type)
	     car-type)
	 (nconc preds car-preds)))))

(defmethod destructure-type* ((texpr subtype) preds)
  (if (some #'(lambda (sty) (subtype-of? sty texpr)) *stop-types*)
      (values texpr preds)
      (unless (or (member texpr *subtypes-seen* :test #'tc-eq)
		  (and (not *all*) (ignored-type-constraint texpr)))
	(push texpr *subtypes-seen*)
	(let ((supertype (supertype texpr))
	      (pred (make!-reduced-application (predicate texpr) *expr*)))
           (destructure-type* supertype (cons pred preds))))))

(defmethod destructure-type* ((texpr dep-binding) preds)
   (destructure-type* (type texpr) preds))

(defmethod destructure-type* (texpr preds)
  (values texpr (nreverse preds)))


;; (incomplete) check if the expression is a finite set of 
;; natural numbers

(defmethod finite-set-of-nat? ((ex expr))
  (some #'finite-set-of-nat? (judgement-types+ ex)))

(defmethod finite-set-of-nat? ((te subtype))
  (or (and (is-finite-of-nat? (predicate te))
	   (set-of-nat? (supertype te)))
      (finite-set-of-nat? (supertype te))))

(defmethod finite-set-of-nat? (te)
  (declare (ignore te))
  nil)

(defmethod is-finite-of-nat? ((ex name-expr))
  (and (eq (declaration ex) (is-finite-decl))
       (subtype-of? (type-value (car (actuals (module-instance ex))))
		    *naturalnumber*)))

(defmethod is-finite-of-nat? (ex)
  (declare (ignore ex))
  nil)

(defmethod set-of-nat? ((te funtype))
  (and (subtype-of? (domain te) *naturalnumber*)
       (tc-eq (range te) *boolean*)))

(defmethod set-of-nat? ((te subtype))
  (set-of-nat? (supertype te)))

(defmethod set-of-nat? (te)
  (declare (ignore te))
  nil)

(let ((is-finite-decl nil))
  (defun is-finite-decl ()
    (or is-finite-decl
	(setq is-finite-decl
	      (find-if #'(lambda (d)
			   (eq (id (module d)) '|finite_sets_def|))
		(gethash '|is_finite| (current-declarations-hash))))))
  (defun reset-is-finite-decl ()
    (setq is-finite-decl nil)))

; some recognizers for arithmetic relations

(defmacro is? (op sym mod)
  (let ((id (gensym))
	(resolutions (gensym)))
    `(with-slots (,id ,resolutions) ,op
	(and (eq ,id ,sym)
         (eq (,id (module-instance (car ,resolutions))) ,mod)))))


(defmethod number-lt-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|<|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod number-lt-op? (op)
  (declare (ignore op))
  nil)

(defmethod number-le-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|<=|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod number-le-op? (op)
  (declare (ignore op))
  nil)

(defmethod number-ge-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|>=|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod number-ge-op? (op)
  (declare (ignore op))
  nil)

(defmethod minus-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|-|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod minus-op? (op)
  (declare (ignore op))
  nil)

(defmethod plus-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|+|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod plus-op? (op)
  (declare (ignore op))
  nil)

(defmethod number-gt-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|>|)
         (eq (id (module-instance (car resolutions))) '|reals|))))

(defmethod number-gt-op? (op)
  (declare (ignore op))
  nil)

(defmethod the-op? ((op name-expr))
  (with-slots (id resolutions) op
    (and (eq id '|the|)
         (eq (id (module-instance (car resolutions))) '|sets|)))) 
          
(defmethod the-op? (op)
  (declare (ignore op))
  nil)


