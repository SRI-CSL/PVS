(in-package :pvs)

(defun make!-disequation (lhs rhs)
  (assert (and (type lhs) (type rhs)))
  (assert (compatible? (type lhs) (type rhs)))
  (make!-negation (make!-equation lhs rhs)))

(defmethod destructure-disequality ((fml disequation))
  (values (args1 fml) (args2 fml)))

(defmethod destructure-disequality ((fml negation))
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






