;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; judgements.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 22:40:53 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Oct 29 22:41:02 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defvar *expr-judgements* (make-hash-table))

(defmethod judgement-types ((ex expr))
  (multiple-value-bind (types there?)
      (gethash ex *expr-judgements*)
    (if there?
	types
	(setf (gethash ex *expr-judgements*)
	      (get-judgement-types ex)))))

(defmethod (setf judgement-types) (jtypes (ex expr))
  (setf (gethash ex *expr-judgements*) jtypes)) 


;;; get-judgement-types

(defun get-judgement-types (ex)
  (get-judgement-types* ex))

(defmethod get-judgement-types* ((ex number-expr))
  (cons (available-numeric-type (number ex))
	(mapcar #'type (get-judgements ex))))

(defmethod get-judgement-types* ((ex if-expr))
  (if (eq (id *current-theory*) '|if_def|)
      (let* ((then (then-part ex))
	     (else (else-part ex))
	     (jthen (judgement-types then))
	     (jelse (judgement-types else))
	     (ctypes (when (or jthen jelse)
		       (all-compatible-types (or jthen (list (type then)))
					     (or jelse (list (type else)))))))
	(setf (judgement-types ex)
	      (delete-if #'(lambda (cty) (tc-eq cty (type ex)))
		ctypes)))))

(defmethod get-judgement-types* ((ex lambda-expr))
  (when (judgement-types (expression ex))
    (setf (judgement-types ex)
	  (mapcar #'(lambda (jty)
		      (make-formals-funtype (list (bindings ex)) jty))
	    (judgement-types (expression ex))))))

(defun all-compatible-types (types1 types2 &optional ctypes)
  (if (null types1)
      (minimal-types ctypes)
      (let ((car-ctypes (all-compatible-types* (car types1) types2)))
	(all-compatible-types (cdr types1) types2
			      (nconc car-ctypes ctypes)))))

(defun all-compatible-types* (type types &optional ctypes)
  (if (null types)
      ctypes
      (let ((ctype (compatible-type type (car types))))
	(all-compatible-types*
	 type (cdr types)
	 (if (some #'(lambda (cty) (subtype-of? cty ctype)) ctypes)
	     ctypes
	     (cons ctype ctypes))))))

(defmethod get-judgement-types* ((res resolution))
  (with-slots (declaration module-instance type) res
    (let ((judgements (get-judgements declaration)))
      (delete-duplicates
       (mapcar #'(lambda (j)
		   (subst-mod-params (type j) module-instance))
	 judgements)
       :test #'tc-eq))))

(defmethod get-application-judgements ((ex name-expr) &optional (currynum 0))
  (assert *current-context*)
  (let ((judgements (cdr (assq (declaration ex)
			       (application-judgements *current-context*)))))
    (remove-if-not #'(lambda (j) (= (length (formals j)) currynum))
      judgements)))

(defmethod get-application-judgements ((ex constructor-name-expr)
				       &optional (currynum 0))
  (let ((judgements (call-next-method))
	(adt (adt (adt ex))))
;    (when (formals adt)
;      (break))
    judgements))

(defmethod get-application-judgements ((ex application) &optional (currynum 0))
  (assert *current-context*)
  (get-application-judgements (operator ex) (1+ currynum)))

(defmethod get-application-judgements (ex &optional currynum)
  nil)

(defmethod get-judgements ((list list))
  (mapappend #'get-judgements list))

(defmethod get-judgements ((ex number-expr))
  (remove-duplicates
      (append (cdr (assoc (number ex) (judgements *current-context*)))
	      (mapappend #'(lambda (u)
			     (cdr (assoc (number ex) (judgements (car u)))))
			 (using *current-context*)))
    :test #'same-judgement-types))

(defmethod same-judgement-types ((j1 judgement-resolution)
				 (j2 judgement-resolution))
  (and (eq (declaration j1) (declaration j2))
       (eq (judgement j1) (judgement j2))
       (tc-eq (module-instance j1) (module-instance j2))))

(defmethod same-judgement-types ((j1 resolution) (j2 judgement-resolution))
  nil)

(defmethod same-judgement-types ((j1 judgement-resolution) (j2 resolution))
  nil)

(defmethod same-judgement-types ((j1 named-judgement) (j2 named-judgement))
  (and (tc-eq (name j1) (name j2))
       (tc-eq (type j1) (type j2))))

(defmethod same-judgement-types (j1 j2)
  (and (eq (declaration j1) (declaration j2))
       (tc-eq (module-instance j1) (module-instance j2))))

(defmethod get-judgements ((decl declaration))
  (assert *current-context*)
  (cdr (assq decl (judgements *current-context*))))

(defmethod get-judgements ((decl field-decl))
  (assert *current-context*)
  (cdr (assq decl (judgements *current-context*))))

(defmethod get-judgements ((decl bind-decl))
  (assert *current-context*)
  (cdr (assq decl (judgements *current-context*))))

(defmethod get-judgements ((decl dep-binding))
  nil)

(defmethod get-judgements ((name name-expr))
  (get-judgements (resolution name)))

