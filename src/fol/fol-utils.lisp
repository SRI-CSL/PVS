(in-package :pvs)

(defmethod mk-funtype ((domain null) range &optional (class 'funtype))
  range)

(defmethod make-app (expr (vars null))
  expr)

(defmethod make-app (expr (vars cons))
  (make-application* expr vars))
