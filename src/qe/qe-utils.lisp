(in-package :pvs)

;; is a certain variable in the scope of a some operator

(defmethod in-scope-of? ((bndngs list) (expr expr) op &optional inside)
  (some #'(lambda (bndng)
	    (in-scope-of? bndng expr op inside))
	expr))

(defmethod in-scope-of? ((bndng bind-decl) (exprs list) op &optional inside)
  (some #'(lambda (expr)
	    (in-scope-of? bndng expr op inside))
	exprs))

(defmethod in-scope-of? ((bndng bind-decl) (expr name-expr) op &optional inside)
  (declare (ignore op))
  (and inside (tc-eq bndng (declaration expr))))

(defmethod in-scope-of? ((bndng bind-decl) (expr number-expr) op &optional inside)
  (declare (ignore op) (ignore inside))
  nil)

(defmethod in-scope-of? ((bndng bind-decl) (expr application) op &optional inside)
  (if (tc-eq (operator expr) op)
      (in-scope-of? bndng (arguments expr) op 'T)
    (in-scope-of? bndng (arguments expr) op inside)))

(defmethod in-scope-of? ((bndng bind-decl) (expr projection-application) op &optional inside)
  (in-scope-of? bndng (argument expr) op inside))

(defmethod in-scope-of? ((bndng bind-decl) (expr binding-expr) op &optional inside)
  (in-scope-of? bndng (expression expr) op inside))

;; Construction of simplified versions of operators

(defmethod simplified-negation ((fml expr))
  (cond ((tc-eq fml *true*) *false*)
	((tc-eq fml *false*) *true*)
	(t (make!-negation fml))))

(defmethod simplified-negation ((fml negation))
  (args1 fml))

(defmethod simplified-negation ((fml disequation))
   (make!-equation* (argument fml)))

(defmethod simplified-negation ((fml equation))
   (make!-disequation* (argument fml)))

(defmethod simplified-negation ((fml application))
  (let ((op (operator fml))
	(arg (argument fml)))
    (cond ((tc-eq op (less-operator))
	   (make!-ge* arg))
	  ((tc-eq op (greatereq-operator))
	   (make!-lt* arg))
	  ((tc-eq op (greater-operator))
	   (make!-le* arg))
	  ((tc-eq op (lesseq-operator))
	   (make!-gt* arg))
	  (t (make!-negation fml)))))

(defun simplified-disjunction (lhs rhs)
  (cond ((or (tc-eq lhs *true*) (tc-eq rhs *true*)) *true*)
	((tc-eq lhs *false*) rhs)
	((tc-eq rhs *false*) lhs)
	((and (negation? lhs)
	      (tc-eq (argument lhs) rhs)) *true*)
	((and (negation? rhs)
	      (tc-eq (argument rhs) lhs)) *true*)
	((tc-eq lhs rhs) rhs)
	(t (make!-disjunction lhs rhs))))

(defun simplified-conjunction (lhs rhs)
  (cond ((or (tc-eq lhs *false*)
	     (tc-eq rhs *false*)) *false*)
	((tc-eq lhs *true*) rhs)
	((tc-eq rhs *true*) lhs)
	((and (negation? lhs)
	      (tc-eq (argument lhs) rhs)) *false*)
	((and (negation? rhs)
	      (tc-eq (argument rhs) lhs)) *false*)
	((tc-eq lhs rhs) lhs)
	(t (make!-conjunction lhs rhs))))

(defun simplified-branch (condition then-part else-part)
  (cond ((tc-eq condition *true*) then-part)
	((tc-eq condition *false*) else-part)
	((tc-eq then-part else-part) then-part)
	(t (make!-if-expr condition then-part else-part))))

; (defun make!-disequation (lhs rhs)
;  (assert (and (type lhs) (type rhs)))
;  (assert (compatible? (type lhs) (type rhs)))
;  (make!-negation (make!-equation lhs rhs)))

(defmethod destructure-disequation ((fml disequation))
  (values (args1 fml) (args2 fml)))

(defmethod destructure-disequation ((fml negation))
  (let ((arg (args1 fml)))
    (when (equation? arg)
      (values (args1 arg) (args2 arg)))))

(defun make!-lt* (ex)
  (assert (type ex))
  (assert (compatible? (find-supertype (type ex)) (number-cross-number)))
  (make-instance 'infix-application
    'operator (less-operator)
    'argument ex
    'type *boolean*))

(defun make!-le* (ex)
  (assert (type ex))
  (assert (compatible? (find-supertype (type ex)) (number-cross-number)))
  (make-instance 'infix-application
    'operator (lesseq-operator)
    'argument ex
    'type *boolean*))

(defun make!-gt* (ex)
  (assert (type ex))
  (assert (compatible? (find-supertype (type ex)) (number-cross-number)))
  (make-instance 'infix-application
    'operator (greater-operator)
    'argument ex
    'type *boolean*))


(defun make!-ge* (ex)
  (assert (type ex))
  (assert (compatible? (find-supertype (type ex)) (number-cross-number)))
  (make-instance 'infix-application
    'operator (greatereq-operator)
    'argument ex
    'type *boolean*))

(defun make!-lt (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (less-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *boolean*))

(defun make!-gt (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (greater-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *boolean*))

(defun make!-ge (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (greatereq-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *boolean*))

(defun make!-le (ex1 ex2)
  (assert (type ex1))
  (assert (type ex2))
  (assert (tc-eq (find-supertype (type ex1)) *number*))
  (assert (tc-eq (find-supertype (type ex2)) *number*))
  (make-instance 'infix-application
    'operator (lesseq-operator)
    'argument (make!-arg-tuple-expr ex1 ex2)
    'type *boolean*))

(defun make!-equation* (args)
  (assert (type args))
  (let* ((type (find-supertype (type args)))
         (res (mk-resolution (equality-decl)
                (mk-modname '|equalities| (list (mk-actual type)))
                (mk-funtype (list type) *boolean*)))
         (eqname (make-instance 'name-expr
                   'id '=
                   'type (type res)
                   'resolutions (list res)))
         (arg args))
    (if (compatible? type *boolean*)
        (make-instance 'infix-boolean-equation
          'operator eqname
          'argument arg
          'type *boolean*)
        (make-instance 'infix-equation
          'operator eqname
          'argument arg
          'type *boolean*))))

(defun make!-disequation* (args)
  (assert (type args))
  (make!-negation (make!-equation* args)))

;; Recognizers

(defun less? (expr)
  (and (application? expr)
       (tc-eq (operator expr) (less-operator))))

(defun lesseq? (expr)
  (and (application? expr)
       (tc-eq (operator expr) (lesseq-operator))))

(defun greater? (expr)
  (and (application? expr)
       (tc-eq (operator expr) (greater-operator))))

(defun greatereq? (expr)
  (and (application? expr)
       (tc-eq (operator expr) (greatereq-operator))))

(defun strict-ineq? (expr)
  (or (less? expr) (greater? expr)))

(defun nonstrict-ineq? (expr)
  (or (lesseq? expr) (greatereq? expr)))






