(in-package :pvs)

;; Logical Variables and Skolem Functions

(defcl lvar-const-decl (const-decl))
(defcl lvar (name-expr))

(defcl param-const-decl (const-decl))
(defcl param (name-expr))


(defun mk-lvar (id type)
  (put-decl (make-instance 'lvar-const-decl             ; destructive update of current context
			   'id id
			   'type type
			   'module (module *current-context*))
	    (declarations-hash *current-context*))
  (change-class (typecheck (pc-parse id 'expr)
			   :expected type
			   :context *current-context*)
		'lvar))

(defvar *lvar-counter* 0)

(defun mk-new-lvar (id type)
  (multiple-value-bind  (fresh new-counter)
      (unique-id id #\? *lvar-counter* *current-context*)
    (setf *lvar-counter* new-counter)
    (mk-lvar fresh type)))

(defun mk-new-lvars (bndngs)
  (mapcar #'(lambda (bndng)
	      (mk-new-lvar (id bndng) (type bndng)))  ; not quite ok...
	  bndngs))
   
(defun mk-new-skofun (bndng vars)
  (make-app (mk-new-param (id bndng)
			  (mk-funtype (mapcar #'type vars)
				      (type bndng))
			  nil)
	    vars))

(defun mk-new-skofuns (bndngs vars)
  (mapcar #'(lambda (bndng)
	      (mk-new-skofun bndng vars))
	  bndngs))



(defun mk-param (id type lvars)
  (declare (ignore lvars)) ; ???
  (put-decl (make-instance 'param-const-decl       ; destructive update of current context
			   'id id
			   'type type
			   'module (module *current-context*))
	    (declarations-hash *current-context*))
  (let ((param (change-class (typecheck (pc-parse id 'expr)
					:expected type
					:context *current-context*)
			     'param)))
    param))

(defvar *param-counter* 0)

(defun mk-new-param (id type lvars)
  (multiple-value-bind  (fresh new-counter)
      (unique-id id #\! *param-counter* *current-context*)
    (setf *param-counter* new-counter)
    (mk-param fresh type lvars)))

;; Fresh identifier

(defun unique-id (name ch counter context)
  "Generating a fresh (relative to a context) identifier of the form 'name^ch^counter'"
  (multiple-value-bind (id new-counter)
      (generate-id name ch counter)
    (if (declared? id context)
        (unique-id name ch new-counter context)
      (values id new-counter))))

(defun generate-id (name ch counter)
  (flet ((special-char-p (x)
	     (member x (list ch #\_))))
    (let* ((str (string (op-to-id name)))
	   (pos (position-if #'special-char-p str :from-end T))
	   (prefix (subseq str 0 pos))
	   (suffix (when pos (subseq str (1+ pos))))
	   (prestr (if (and pos (every #'digit-char-p suffix))
			  prefix
                        str)))
      (values (intern (format nil "~a~a~a" prestr ch counter))
	      (1+ counter)))))

