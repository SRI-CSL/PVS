(in-package pvs)


(defvar *use-translate-from-dc-hash* t)

(defvar *dc-reverse-prover-name* nil)
(defvar *dc-pvs-typealist* nil)

(defun get-inverse-translation (expr)
  (if *use-translate-from-dc-hash*
      (gethash expr *translate-from-dc-hash*)
      (dp::node-external-info expr)))

(defun reset-translate-from-dc ()
  (setq *dc-reverse-prover-name* nil)
  (setq *dc-pvs-typealist* nil)
  (dp::dp-clrhash *translate-from-dc-hash*))

(defun dc-add-to-reverse-prover-name (const expr)
  (when (symbolp expr) (break))
  (unless (assoc const *dc-reverse-prover-name*)
    (push (cons const expr) *dc-reverse-prover-name*)))

(defun dc-add-to-pvs-typealist (id expr &optional (type (or (type expr)
							 (car (ptypes expr)))))
  (unless (assoc id *dc-pvs-typealist*)
    (push (cons id type) *dc-pvs-typealist*)))

;(defun dc-unique-prover-name (expr)
;  (cond ((constant? expr) ;;NSH(2.16.94): changed to deal with Ricky's
;                          ;;soundness bug where actuals are ignored.
;         (let* ((id-hash (pvs-gethash (normalize-name-expr-actuals expr)
;                                      *dc-translate-id-hash*))
;                (newconst
;                 (or id-hash
;                     (when (tc-eq expr *true*) dp::*true*)
;                     (when (tc-eq expr *false*) dp::*false*)
;                     (when (interpreted? expr)
;                       (interpretation expr))
;                     (let ((te (find-supertype (type expr))))
;                       (if (and (typep te 'funtype)
;                                (tc-eq (range te) *boolean*))
;                           (dp::mk-predicate-sym
;                            (intern (format nil "~a_~a"
;                                      (id expr)
;                                      (funcall
;                                       *dc-translate-id-counter*))))
;                           (dp::mk-constant
;                            (intern (format nil "~a_~a"
;                                      (id expr)
;                                      (funcall
;                                       *dc-translate-id-counter*)))))))))
;           (dc-add-to-reverse-prover-name newconst expr)
;           (dc-add-to-pvs-typealist newconst expr)
;           (unless id-hash
;             (setf (pvs-gethash expr *dc-translate-id-hash*)
;                   newconst)
;             ;;(format t "~%adding ~a to typealist" (car newconst))
;             (add-to-prtype-hash  newconst expr))
;           newconst))
;        (t (add-to-local-prtype-hash (id expr) expr)
;           (dc-add-to-reverse-prover-name (id expr) expr)
;           (dc-add-to-pvs-typealist (id expr) expr)
;           (if *translate-rewrite-rule*
;               (dp::mk-variable (id expr))
;               (dp::mk-constant (id expr))))))

(defun make-name-expr (symb)
  (typecheck (mk-name-expr symb)))

(defun dc-get-pvs-name (func)
  (or (cdr (assoc func *dc-reverse-prover-name*))
      (mk-name-expr (dp::constant-id func))))

(defun dc-get-pvs-type (func)
  (cdr (assoc func *dc-pvs-typealist*)))

(defun translate-from-dc-args (list)
  (mapcar #'translate-from-dc-expr list))

(defun translate-from-dc-name (var)
  (cond ((dp::true-p var) *true*)
	((dp::false-p var) *false*)
	((dp::dp-numberp var) (make-number-expr (dp::constant-id var)))
	(t (let ((dc-infix (dc-infix-fun (dp::constant-id var))))
	     (if dc-infix
		 (mk-name-expr dc-infix)
		 (dc-get-pvs-name var))))))

(defun translate-from-dc-if (if-expr)
  (make-if-expr
   (translate-from-dc-expr (dp::arg 1 if-expr))
   (translate-from-dc-expr (dp::arg 2 if-expr))
   (translate-from-dc-expr (dp::arg 3 if-expr))))

(defun translate-from-dc-record (translated-args)
  ;;;Translated args of the form ((record f1 f2 ..) e1 e2 ..)
  (let* ((fields (cdar translated-args))
	 (expressions (cdr translated-args))
	 (assignments (loop for f in fields
			    for e in expressions
			    collect
			    (make-assignment (mk-name-expr (id f)) e))))
    (make-record-expr assignments nil)))

(defun translate-from-dc-record-fields (translated-args)
  translated-args)

(defun translate-from-dc-update (translated-args)
  (let* ((array (nth 0 translated-args))
	 ;;(array-type (translate-from-dc-type array))
	 (array-type (type array))
	 (index (translate-from-dc-index (nth 1 translated-args)
					 array-type))
	 (value (nth 2 translated-args)))
    (make-simple-update-expr array index value)))

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
	   (sfields (dc-sort-fields fields))) ;(break)
      (mk-name-expr (id (nth (number index) sfields)))))
   (t index)))

(defun translate-from-dc-func (func)
  (translate-from-dc-expr func))

(defun translate-from-dc-apply (translated-func translated-args)
  (let* ((func-type1 (type translated-func))
	 (func-type (when func-type1
		      (find-supertype func-type1))))
    (cond
     ((typep func-type 'recordtype)
      (translate-from-dc-field-apply translated-func
				     (nth 0 translated-args)
				     func-type))
     ((nth 0 translated-args)
      (make-application* translated-func translated-args))
     (t translated-func))))

(defun translate-from-dc-field-apply (translated-func
				      translated-index &optional
				      (type (type translated-func)))
  (let ((new-func (translate-from-dc-index translated-index type)))
    (make-field-application new-func translated-func)))

(defun translate-from-dc-infix (term)
  (mk-rec-application-left (dc-infix-fun (dp::constant-id (dp::funsym term))) 0
    (translate-from-dc-args (dp::application-arguments term))))

(defun mk-pvs-application-from-translated-args (translated-args)
  (let* ((func (car translated-args))
	 (op (when (typep func 'name-expr)
	       (id (car translated-args))))
	 (infix-op-id (dc-infix-fun op))
	 (infix-op (when infix-op-id (make-name-expr infix-op-id)))
	 (pvs-args (cdr translated-args)))
    (cond
     ((type func) (translate-from-dc-apply (car translated-args) pvs-args))
     ((or (eq op '+)
	  (eq infix-op-id '+))
      (make-sum pvs-args nil))
     ((or (eq op '*)
	  (eq infix-op-id '*))
      (make-prod pvs-args nil))
     ((or (eq op 'and)
	  (eq infix-op-id 'and))
      (make-conjunction pvs-args))
     ((or (eq op 'or)
	  (eq infix-op-id 'or))
      (make-disjunction pvs-args))
     ((or (eq op '>=)
	  (eq infix-op-id '>=))
      (make-greatereq (car pvs-args) (cadr pvs-args)))
     ((or (eq op '>)
	  (eq infix-op-id '>))
      (make-greater (car pvs-args) (cadr pvs-args)))
     ((or (eq op '<=)
	  (eq infix-op-id '<=))
      (make-lesseq (car pvs-args) (cadr pvs-args)))
     ((or (eq op '<)
	  (eq infix-op-id '<))
      (make-less (car pvs-args) (cadr pvs-args)))
     ((or (eq op '|floor|)
	  (eq infix-op-id '|floor|))
      (make-floor (car pvs-args)))
     ((eq op '=) (make-equation (nth 0 pvs-args)
				(nth 1 pvs-args)))
     ((eq op 'NOT) (make-negation (nth 0 pvs-args)))
     ((eq op 'IF) (make-if-expr (nth 0 pvs-args)
				(nth 1 pvs-args)
				(nth 2 pvs-args)))
     ((eq op 'dp::update) (translate-from-dc-update pvs-args))
     ((eq op 'dp::project) (translate-from-dc-project pvs-args))
     ((eq op 'dp::record) (translate-from-dc-record-fields translated-args))
     ((eq op 'dp::tuple) (translate-from-dc-tuple pvs-args))
     ((eq op 'dp::th-app) (translate-from-dc-th-app pvs-args))
     ((and (consp func) (typep (car func) 'name-expr)
	   (eq (id (car func)) 'dp::record))
      (translate-from-dc-record translated-args))
     (infix-op (mk-rec-application-left infix-op 0 pvs-args))
     (t (translate-from-dc-apply (car translated-args)
				 pvs-args)))))

(defun translate-from-dc-tuple (translated-args)
  (make-tuple-expr translated-args))

(defun translate-from-dc-th-app (translated-args)
  ;;The first argument is the pvs constant that has theory parameters
  ;;that need to be instantiated.
  ;;The remaining arguments are the actuals that will instantiate the
  ;;theory parameters. These are only the expression parameters.
  ;;The type parameters are already in func.
  (let* ((func (car translated-args))
	 (actuals (cdr translated-args))
	 (old-actuals (actuals (module-instance func)))
	 (new-actuals (replace-non-type-actuals-in-list old-actuals actuals)))
    (make-instantiated-func-from-new-actuals func new-actuals)))

(defun make-instantiated-func-from-new-actuals (func new-actuals)
  (let* ((old-resolution (car (resolutions func)))
	 (old-module-instance (module-instance old-resolution))
	 (new-module-instance (lcopy old-module-instance
				'actuals new-actuals))
	 (new-resolution (lcopy old-resolution
			  'module-instance new-module-instance)))
    (lcopy func
      'resolutions (list new-resolution))))

(defun replace-non-type-actuals-in-list (old-actuals new-actuals)
  (if old-actuals
      (if (type-value (car old-actuals))
	  (cons (car old-actuals)
		(replace-non-type-actuals-in-list
		 (cdr old-actuals) new-actuals))
	  (cons (mk-actual (car new-actuals))
		(replace-non-type-actuals-in-list
		 (cdr old-actuals) (cdr new-actuals))))
      nil))

(defun translate-from-dc-project (translated-args)
  (let* ((index (nth 0 translated-args))
	 (record (nth 1 translated-args))
	 (record-type1 (type record))
	 (record-type (when record-type1
		      (find-supertype record-type1))))
    (translate-from-dc-field-apply record
				   (nth 0 translated-args)
				   record-type)))

(defun translate-from-dc-application (expr)
  (let ((translated-args (dp::map-args-list #'translate-from-dc-expr expr)))
    (mk-pvs-application-from-translated-args translated-args)))

(defun translate-from-dc-constant (expr)
  (dc-get-pvs-name (dp::funsym expr)))

(defun translate-from-dc-expr (expr)
  (let ((hashed-value (get-inverse-translation expr)))
    (or hashed-value
	(let ((result (translate-from-dc-expr* expr)))
	  (assert (not (symbolp result)))
	  (unless (or *bound-variables* *bindings*)
	    (set-inverse-translation expr result))
	  result))))

(defun translate-from-dc-expr* (expr)
  (cond
   ((dp::application-p expr) (translate-from-dc-application expr))
   (t (translate-from-dc-name expr))))

(defun translate-from-dc (expr)
  (cond
   ((dp::true-p expr) *true*)
   ((dp::false-p expr) *false*)
   (t (when expr
	(let* ((struct (translate-from-dc-expr expr))
	       (eqlty (mk-equation struct struct))
	       (typed-eqlty (typecheck eqlty :expected *boolean*)))
	  (args1 typed-eqlty))))))

(defun translate-from-dc-list (list)
  (if (listp list)
      (mapcar #'translate-from-dc list)
      (translate-from-dc list)))

(defvar *dc-struct-infix-trans-table*
  '((dp::*=* . =)
    (dp::*lessp* . <)
    (dp::*lesseqp* . <=)
    (dp::*greaterp* . >)
    (dp::*greatereqp* . >=)
    (dp::*floor* . |floor|)
    (dp::*plus* . +)
    (dp::*difference* . -)
    (dp::*times* . *)
    (dp::*divide* . /)
    (dp::*quo* . /)))

(defvar *dc-infix-trans-table*
  '((dp::= . =)
    (dp::lessp . <)
    (dp::lesseqp . <=)
    (dp::greaterp . >)
    (dp::greatereqp . >=)
    (dp::floor . |floor|)
    (dp::plus . +)
    (dp::difference . -)
    (dp::times . *)
    (dp::divide . /)
    (dp::quo . /)))

(defun dc-infix-fun (func)
  (let ((find (cdr (assoc func *dc-infix-trans-table*))))
    find))

(defun make-simple-update-expr (expression index value &optional expected)
  (let* ((assignments (list (make-assignment index value)))
	 (type (or expected (type expression)))
	 (nexpr (make-instance 'update-expr
		  'expression expression
		  'assignments assignments
		  'type type)))
    (typecheck nexpr :expected type)))
