(in-package pvs)

(defun mk-equation (lhs rhs)
  (mk-application '= lhs rhs))

(defun mk-update-expr (expr assignments)
  (make-instance 'update-expr
    'expression expr
    'assignments assignments))

(defun mk-update-expr-1 (expr index value)
  (let ((assignment (mk-assignment 'uni `((,index)) value)))
    (mk-update-expr expr (list assignment))))

(defvar *reverse-prover-name* nil)
(defvar *pvs-typealist* nil)
(defvar *translate-from-prove-hash* (make-hash-table :test #'equal))

(defmethod translate-to-prove :around (obj)
  (let ((hashed-value (pvs-gethash obj *translate-to-prove-hash*)))
    (or hashed-value
	(let ((result (call-next-method)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (pvs-gethash obj *translate-to-prove-hash*) result))
	  (setf (gethash result *translate-from-prove-hash*) obj)
	  result))))

(defun add-to-reverse-prover-name (id expr)
  (unless (assoc id *reverse-prover-name*)
    (push (cons id (id expr)) *reverse-prover-name*)))

(defun add-to-pvs-typealist (id expr &optional (type (or (type expr)
							 (car (ptypes expr)))))
  (unless (assoc id *pvs-typealist*)
    (push (cons id type) *pvs-typealist*)))

(defun unique-prover-name (expr)
  ;(when (equal (id expr) '|stall_issue|) (break))
  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
	                  ;;soundness bug where actuals are ignored.
	 (let* ((id-hash (pvs-gethash (normalize-name-expr-actuals expr)
				      *translate-id-hash*))
		(newconst (or id-hash
			      (when (true-p expr) 'TRUE)
			      (when (false-p expr) 'FALSE)
			      (list (intern (concatenate 'string
					      (string (id expr))
					      "_"
					      (princ-to-string
					       (funcall
						*translate-id-counter*))))))))
	   (when (consp newconst)
	     (add-to-reverse-prover-name (car newconst) expr)
	     (add-to-pvs-typealist (car newconst) expr))
	   (unless id-hash
	     (setf (pvs-gethash expr *translate-id-hash*)
		   newconst)
	     ;;(format t "~%adding ~a to typealist" (car newconst))
	     (when (consp newconst)
	       (add-to-typealist (car newconst) expr)
	       (add-to-reverse-prover-name (car newconst) expr)
	       (add-to-pvs-typealist (car newconst) expr)))
	   newconst))
	(t (add-to-local-typealist (id expr) expr)
	   (add-to-reverse-prover-name (id expr) expr)
	   (add-to-pvs-typealist (id expr) expr)
	   (id expr))))

(defun get-pvs-name (func)
  (or (cdr (assoc func *reverse-prover-name*))
      func))

(defun get-pvs-type (func)
  (cdr (assoc func *pvs-typealist*)))

(defun translate-from-prove-args (list)
  (mapcar #'translate-from-prove-expr list))

(defun translate-from-prove-var (var)
  (cond ((eq var 'true) *true*)
	((eq var 'false) *false*)
	((numberp var) (mk-number-expr var))
	(t (mk-name-expr (get-pvs-name var) nil nil nil 'variable))))

(defun translate-from-prove-if (if-expr)
  (mk-if-expr
   (translate-from-prove-expr (arg1 if-expr))
   (translate-from-prove-expr (arg2 if-expr))
   (translate-from-prove-expr (arg3 if-expr))))

(defun translate-from-prove-update (update)
  (let* ((array (translate-from-prove-expr (arg1 update)))
	 (array-type (translate-from-prove-type (arg1 update)))
	 (index (translate-from-prove-index (arg2 update)
					    array-type))
	 (value (translate-from-prove-expr (arg3 update))))
    (mk-update-expr-1 array index value)))

(defun translate-from-prove-type (expr)
  (cond
   ((and (consp expr) (eq (funsym expr) 'IF))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr) (eq (funsym expr) 'UPDATE))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr)
	 (memq (funsym expr)
	       '(AND NOT OR IMPLIES EQUAL LESSP LESSEQP GREATERP GREATEREQP)))
    (translate-from-prove-type (arg1 expr)))
   ((and (consp expr) (null (argsof expr)))
    (translate-from-prove-type (car expr)))
   ((get-pvs-type expr))
   (t (translate-from-prove-type* expr))))

(defun translate-from-prove-type* (expr)
  (type (translate-from-prove expr)))

(defun translate-from-prove-index (index type)
  (cond
   ((numberp index)
    (let* ((fields (fields (find-supertype type)))
	   (sfields (sort-fields fields))) ;(break)
      (mk-name-expr (id (nth index sfields)))))
   (t (translate-from-prove-expr index))))

(defun translate-from-prove-func (func)
  (translate-from-prove-expr func))

(defun translate-from-prove-apply (apply)
  (let* ((func (arg1 apply))
	 (func-type (translate-from-prove-type func)))
    (cond
     ((typep func-type 'recordtype)
      (translate-from-prove-field-apply func (arg2 apply) func-type))
     ((arg2 apply) (mk-application* (translate-from-prove-func func)
		     (translate-from-prove-args (cdr (argsof apply)))))
     (t (translate-from-prove-expr func)))))

(defun translate-from-prove-field-apply (func
					 index &optional
					 (type (translate-from-prove-type
						func)))
  (let ((new-func (translate-from-prove-index index type)))
    (mk-application new-func
      (translate-from-prove-expr func))))

(defun translate-from-prove-infix (term)
  (mk-rec-application-left (infix-fun (funsym term)) 0
    (translate-from-prove-args (argsof term))))

(defun translate-from-prove-cons (expr)
  (let ((func (funsym expr)))
    (cond
     ((eq func 'update) (translate-from-prove-update expr))
     ((is-apply-n-x func) (translate-from-prove-apply expr))
     ((eq func 'if) (translate-from-prove-if expr))
     ((argsof expr) (translate-from-prove-infix expr))
     (t (translate-from-prove-constant expr)))))

(defun translate-from-prove-constant (expr)
  (translate-from-prove-expr (get-pvs-name (funsym expr))))

(defun translate-from-prove-expr (expr)
  (let ((hashed-value (gethash expr *translate-from-prove-hash*)))
    (or hashed-value
	(let ((result (translate-from-prove-expr* expr)))
	  (unless (or *bound-variables* *bindings*)
	    (setf (gethash expr *translate-from-prove-hash*) result))
	  result))))

(defun translate-from-prove-expr* (expr)
  (cond
   ((consp expr) (translate-from-prove-cons expr))
   (t (translate-from-prove-var expr))))

(defun translate-from-prove (expr)
  (let* ((struct (translate-from-prove-expr expr))
	 (eqlty (mk-equation struct struct))
	 (typed-eqlty (typecheck eqlty :expected *boolean*)))
    (args1 typed-eqlty)))

(defun translate-from-prove-list (list)
  (if (listp list)
      (mapcar #'translate-from-prove list)
      (translate-from-prove list)))

(defvar *infix-trans-table*
  '((equal . =)
    (lessp . <)
    (lesseqp . <=)
    (greaterp . >)
    (greatereqp . >=)
    (plus . +)
    (difference . -)
    (times . *)
    (divide . /)
    (quo . /)))

(defun infix-fun (func)
  (or (cdr (assoc func *infix-trans-table*))
      func))

(defun ground-arithsimp (term)
  (cond
   ((symbolp term) term)
   ((integerp term) term)
   ((interp term)
    (let ((newterm
	   (sigma (cons (funsym term)
			(loop for arg in (argsof term)
			      collect (ground-arithsimp arg) )))))
      newterm ))
   (T term) ))

(defun arithsimp (expr)
  (let ((ground-expr (top-translate-to-prove expr)))
    (translate-from-prove (ground-arithsimp ground-expr))))
