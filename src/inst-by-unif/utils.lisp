(in-package :pvs)

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

