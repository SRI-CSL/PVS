(in-package :pvs)

;; Translation of Logical Variables

(defmethod translate-to-dc ((expr lvar))
  (let* ((pos (position expr *bindings* ;(NSH:4-5-91)
                        :test #'same-declaration))
         (apos (position expr *bound-variables*
                         :test #'same-declaration))
         (bpos (when apos (- (length *bound-variables*)
                             apos))))
    (cond ((and (null pos)(null bpos))
           (translate-lvar-with-actuals-to-dc expr))
          (bpos (let ((id (dp::mk-variable
                           (makesym "*~a?~a*" (id expr) bpos))))
                  (add-to-local-prtype-hash id expr)
                  id))
          (t (let ((nvar (makesym "*~a*" (1+ pos))))
               (dp::mk-term (list (dp::mk-variable
				   (get-subtype-coercion expr))
				  (dp::mk-variable nvar))))))))


(defun translate-lvar-with-actuals-to-dc (expr)
  (let* ((dc-base-name (dc-unique-prover-lvar expr))
         (actuals (actuals (module-instance expr)))
         (dc-actuals (translate-to-dc-actuals actuals)))
    (if dc-actuals
        (dp::mk-term (cons dp::*th-app*
                           (cons dc-base-name dc-actuals))
                     (dp::node-type dc-base-name))
        dc-base-name)))

(defun dc-unique-prover-lvar (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
                          ;;soundness bug where actuals are ignored.
        (let* ((hash-list (list-of-decl-and-type-actuals expr))
               (id-hash (gethash hash-list *dc-translate-id-hash*))
               (dc-expr
                (or ;(when (eq (id expr) *se*) (break))
                    id-hash
                    (when (tc-eq expr *true*) dp::*true*)
                    (when (tc-eq expr *false*) dp::*false*)
                    (when (interpreted? expr)
                      (interpretation expr))
                    (let* ((type (type expr))
                           (range-type (if (typep type 'funtype)
                                           (range type)
                                           type))
                           (dc-range-type (dc-prover-type range-type))
                           (new-const (dp::mk-variable
                                       (intern (format nil "~a?~a"
                                                 (id expr)
                                                 (funcall
                                                  *dc-translate-id-counter*)))
                                       dc-range-type)))
                      (dc-add-to-reverse-prover-name new-const expr)
                      (dc-add-to-pvs-typealist new-const expr)
                      new-const))))
           (unless id-hash
             (setf (gethash hash-list *dc-translate-id-hash*)
                   dc-expr)
             ;;(format t "~%adding ~a to typealist" (car newconst))
             (add-to-prtype-hash dc-expr expr))
           dc-expr))
        ((typep expr 'field-decl)
         (let* ((id-hash (gethash expr
                                      *dc-translate-id-hash*))
                (newconst
                 (or id-hash
                     (dp::mk-variable
                            (intern (format nil "~a-~a"
                                      (id expr)
                                      (funcall
                                       *dc-translate-id-counter*)))))))
           (unless id-hash
             (setf (gethash expr *dc-translate-id-hash*)
                   newconst))
           newconst))
        (t (add-to-local-prtype-hash (id expr) expr)
           (dc-add-to-reverse-prover-name (id expr) expr)
           (dc-add-to-pvs-typealist (id expr) expr)
           (if *translate-rewrite-rule*
               (dp::mk-variable (id expr) (dc-prover-type (type expr)))
               (dp::mk-variable (id expr) (dc-prover-type (type expr)))))))


;; Translation of Substitutions

(defun translate-from-dc-subst (alist)
  (loop for (x . trm) in alist
     collect (cons (translate-from-dc x)
                   (translate-from-dc trm))))
