(in-package :pvs)

;; Delays

(defmacro delay (x)
  `(function (lambda () ,x)))

(defmacro force (f)
  `(funcall ,f))

;; Additional Method

(defun strip (e &optional acc)
  (if (application? e)
      (strip (operator e) (append (arguments e) acc))
    (values e acc)))

(defmethod mk-funtype ((domain null) range &optional (class 'funtype))
  (declare (ignore class))
  range)

(defmethod make-lambda ((vars null) expr)
  expr)

(defmethod make-lambda ((vars cons) expr)
  (make-lambda-expr vars expr))

(defmethod make-app (expr (vars null))
  expr)

(defmethod make-app (expr (vars cons))
  (make-application* expr vars))
		     
(defun lcopy-negation (orig arg)
  (if (eq (args1 orig) arg) orig
    (make!-negation arg)))

(defun lcopy-conjunction (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1)
	   (eq (args2 orig) arg2))
      orig
    (make!-conjunction arg1 arg2)))

(defun lcopy-disjunction (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1)
	   (eq (args2 orig) arg2))
      orig
    (make!-disjunction arg1 arg2)))

(defun lcopy-implication (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-implication arg1 arg2)))

(defun lcopy-iff (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-iff arg1 arg2)))

(defun lcopy-equality (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-equation arg1 arg2)))

(defun lcopy-disequality (orig arg1 arg2)
  (if (and (eq (args1 orig) arg1) (eq (args2 orig) arg2)) orig
    (make!-negation (make!-equation arg1 arg2))))

;; Tests if a quantified formula is of universal or existential strengths


(defmethod universal? ((fml forall-expr))
  fml)

(defmethod universal? ((fml negation))
  (existential? (args1 fml)))

(defmethod universal? ((fml expr))
  nil)

(defmethod existential? ((fml exists-expr))
  fml)

(defmethod existential? ((fml negation))
  (universal? (args1 fml)))

(defmethod existential? ((fml expr))
  nil)


;; Destructering of universal and existential-strength quantifications
;; in a list of bindings and the body

(defmethod destructure-existential ((sf s-formula) &optional bndngs negative?)
  (destructure-existential (formula sf) bndngs negative?))

(defmethod destructure-existential ((fml exists-expr) &optional bndngs negative?)
  (let ((new-fml (lift-predicates-in-quantifier fml (list *naturalnumber* *integer*))))
    (destructure-existential (expression new-fml)
			     (union (bindings new-fml) bndngs :test #'tc-eq)
			     negative?)))

(defmethod destructure-existential ((fml negation) &optional bndngs negative?)
  (destructure-universal (args1 fml) bndngs (toggle-flag negative?)))

(defmethod destructure-existential ((fml expr) &optional bndngs negative?)
  (values bndngs (if negative? (make!-negation fml) fml)))

(defmethod destructure-universal ((sf s-formula) &optional bndngs negative?)
  (destructure-universal (formula sf) bndngs negative?))

(defmethod destructure-universal ((fml forall-expr) &optional bndngs negative?)
  (let ((new-fml (lift-predicates-in-quantifier fml (list *naturalnumber* *integer*))))
    (destructure-universal (expression new-fml)
			   (union (bindings new-fml) bndngs :test #'tc-eq)
			   negative?)))

(defmethod destructure-universal ((fml negation) &optional bndngs negative?)
  (destructure-existential (args1 fml) bndngs (toggle-flag negative?)))

(defmethod destructure-universal ((fml expr) &optional bndngs negative?)
  (values bndngs (if negative? (make!-negation fml) fml)))

(defun toggle-flag (flag)
  (if flag nil T))

;; Composition of functions

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
    #'identity))



		    
