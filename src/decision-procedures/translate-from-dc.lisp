(in-package pvs)


(defvar *dc-reverse-prover-name* nil)
(defvar *dc-pvs-typealist* nil)
(defvar *translate-from-dc-hash* (dp::dp-make-eq-hash-table))

(defun reset-translate-from-dc ()
  (setq *dc-reverse-prover-name* nil)
  (setq *dc-pvs-typealist* nil)
  (dp::dp-clrhash *translate-from-dc-hash*))


(defmethod translate-to-dc :around (obj)
  (let ((hashed-value (pvs-gethash obj *translate-to-dc-hash*)))
    (or hashed-value
	(let ((result (call-next-method)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (pvs-gethash obj *translate-to-dc-hash*) result))
	  (setf (gethash result *translate-from-dc-hash*) obj)
	  result))))

(defun dc-add-to-reverse-prover-name (const expr)
  (unless (assoc const *dc-reverse-prover-name*)
    (push (cons const (id expr)) *dc-reverse-prover-name*)))

(defun dc-add-to-pvs-typealist (id expr &optional (type (or (type expr)
							 (car (ptypes expr)))))
  (unless (assoc id *dc-pvs-typealist*)
    (push (cons id type) *dc-pvs-typealist*)))

(defun dc-unique-prover-name (expr)
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	 (let* ((id-hash (pvs-gethash (normalize-name-expr-actuals expr)
				      *dc-translate-id-hash*))
		(newconst
		 (or id-hash
		     (when (tc-eq expr *true*) dp::*true*)
		     (when (tc-eq expr *false*) dp::*false*)
		     (when (interpreted? expr)
		       (interpretation expr))
		     (let ((te (find-supertype (type expr))))
		       (if (and (typep te 'funtype)
				(tc-eq (range te) *boolean*))
			   (dp::mk-predicate-sym
			    (intern (format nil "~a_~a"
				      (id expr)
				      (funcall
				       *dc-translate-id-counter*))))
			   (dp::mk-constant
			    (intern (format nil "~a_~a"
				      (id expr)
				      (funcall
				       *dc-translate-id-counter*)))))))))
	   (dc-add-to-reverse-prover-name newconst expr)
	   (dc-add-to-pvs-typealist newconst expr)
	   (unless id-hash
	     (setf (pvs-gethash expr *dc-translate-id-hash*)
		   newconst)
	     ;;(format t "~%adding ~a to typealist" (car newconst))
	     (add-to-prtype-hash  newconst expr))
	   newconst))
	(t (add-to-local-prtype-hash (id expr) expr)
	   (dc-add-to-reverse-prover-name (id expr) expr)
	   (dc-add-to-pvs-typealist (id expr) expr)
	   (dp::mk-variable (id expr)))))

(defun dc-get-pvs-name (func)
  (or (cdr (assoc func *dc-reverse-prover-name*))
      (dp::constant-id func)))

(defun dc-get-pvs-type (func)
  (cdr (assoc func *dc-pvs-typealist*)))

(defun translate-from-dc-args (list)
  (mapcar #'translate-from-dc-expr list))

(defun translate-from-dc-name (var)
  (cond ((dp::true-p var) *true*)
	((dp::false-p var) *false*)
	((dp::dp-numberp var) (mk-number-expr (dp::constant-id var)))
	(t (let ((dc-infix (dc-infix-fun var)))
	     (or dc-infix
		 (mk-name-expr (dc-get-pvs-name var)))))))

(defun translate-from-dc-if (if-expr)
  (mk-if-expr
   (translate-from-dc-expr (dp::arg 1 if-expr))
   (translate-from-dc-expr (dp::arg 2 if-expr))
   (translate-from-dc-expr (dp::arg 3 if-expr))))

(defun translate-from-dc-update (translated-args)
  (let* ((array (nth 0 translated-args))
	 ;;(array-type (translate-from-dc-type array))
	 (array-type (type array))
	 (index (translate-from-dc-index (nth 1 translated-args)
					 array-type))
	 (value (nth 2 translated-args)))
    (mk-update-expr-1 array index value)))

(defun translate-from-dc-type (expr)
  (cond
   ((and (dp::application-p expr) (eq (dp::constant-id (dp::funsym expr)) 'IF))
    (translate-from-dc-type (arg 1 expr)))
   ((and (dp::application-p expr) (eq (dp::constant-id (dp::funsym expr)) 'UPDATE))
    (translate-from-dc-type (arg 1 expr)))
   ((and (dp::application-p expr)
	 (memq (dp::constant-id (dp::funsym expr))
	       '(AND NOT OR IMPLIES EQUAL LESSP LESSEQP GREATERP GREATEREQP)))
    (translate-from-dc-type (arg 1 expr)))
   ((and (dp::application-p expr) (= (dp::arity expr) 0))
    (translate-from-dc-type (dp::funsym expr)))
   ((dc-get-pvs-type expr))
   (t (translate-from-dc-type* expr))))

(defun translate-from-dc-type* (expr)
  (type (translate-from-dc expr)))

(defun translate-from-dc-index (index type)
  (cond
   ((typep index 'number-expr)
    (let* ((fields (fields (find-supertype type)))
	   (sfields (sort-fields fields))) ;(break)
      (mk-name-expr (id (nth (number index) sfields)))))
   (t (translate-from-dc-expr index))))

(defun translate-from-dc-func (func)
  (translate-from-dc-expr func))

(defun translate-from-dc-apply (translated-func translated-args)
  (let* ((func-type (type translated-func)))
    (cond
     ((typep func-type 'recordtype)
      (translate-from-dc-field-apply translated-func
				     (nth 0 translated-args)
				     func-type))
     ((nth 0 translated-args)
      (mk-application* translated-func translated-args))
     (t translated-func))))

(defun translate-from-dc-field-apply (translated-func
				      translated-index &optional
				      (type (type translated-func)))
  (let ((new-func (translate-from-dc-index translated-index type)))
    (mk-application new-func translated-func)))

(defun translate-from-dc-infix (term)
  (mk-rec-application-left (dc-infix-fun (dp::funsym term)) 0
    (translate-from-dc-args (dp::application-arguments term))))

(defun mk-pvs-application-from-translated-args (translated-args)
  (let* ((op (id (car translated-args)))
	 (infix-op (dc-infix-fun op))
	 (pvs-args (cdr translated-args)))
    (cond
     (infix-op (mk-rec-application-left infix-op 0 pvs-args))
     ((eq op 'IF) (mk-if-expr (nth 0 pvs-args)
			      (nth 1 pvs-args)
			      (nth 2 pvs-args)))
     ((eq op 'update) (translate-from-dc-update pvs-args))
     (t (translate-from-dc-apply (car translated-args)
				 pvs-args)))))

(defun translate-from-dc-application (expr)
  (let ((translated-args (dp::map-args-list #'translate-from-dc-expr expr)))
    (mk-pvs-application-from-translated-args translated-args)))

(defun translate-from-dc-constant (expr)
  (translate-from-dc-expr (dc-get-pvs-name (dp::funsym expr))))

(defun translate-from-dc-expr (expr)
  (let ((hashed-value (gethash expr *translate-from-dc-hash*)))
    (or hashed-value
	(let ((result (translate-from-dc-expr* expr)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (gethash expr *translate-from-dc-hash*) result))
	  result))))

(defun translate-from-dc-expr* (expr)
  (cond
   ((dp::application-p expr) (translate-from-dc-application expr))
   (t (translate-from-dc-name expr))))

(defun translate-from-dc (expr)
  (when expr
    (let* ((struct (translate-from-dc-expr expr))
	   (eqlty (mk-equation struct struct))
	   (typed-eqlty (typecheck eqlty :expected *boolean*)))
      (args1 typed-eqlty))))

(defun translate-from-dc-list (list)
  (if (listp list)
      (mapcar #'translate-from-dc list)
      (translate-from-dc list)))

(defvar *dc-infix-trans-table*
  '((dp::*=* . =)
    (dp::*lessp* . <)
    (dp::*lesseqp* . <=)
    (dp::*greaterp* . >)
    (dp::*greatereqp* . >=)
    (dp::*plus* . +)
    (dp::*difference* . -)
    (dp::*times* . *)
    (dp::*divide* . /)
    (dp::*quo* . /)))

(defun dc-infix-fun (func)
  (cdr (assoc func *infix-trans-table*)))
