;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; norm-groundp.lisp -- 
;; Author          : Stephan Pfab
;; Created On      : April 1998
;; Last Modified By: 
;; Last Modified On: 
;; Update Count    : 
;; Status          : 
;; Description     :
;; HISTORY         :
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ground?
;;  if the expression is ground and does not result in a function
;;  temporary results may be functions, but not the endresult.
;;
(defun ground? (expr)
  (and (ground?* expr)
       (ground-type? (type expr))))


(let ((*norm-ground*  (make-hash-table
                       :hash-function 'pvs-sxhash
                       :test 'tc-eq)))
  
  (defun norm-clr-ground-hash ()
    (clrhash *name-ground*))
  
  (defun ground?-memo (expr ground?-new)
    (multiple-value-bind (is-ground? old?)
        (gethash expr *norm-ground*)   
      (if old?             
          is-ground?
                                        ; set groundness to T for recursion
                                        ;(must be T)
        (progn (setf (gethash expr *norm-ground*) T) 
               (let ((is-ground? (funcall ground?-new expr)))
                 (setf (gethash expr *norm-ground*) is-ground?)
                 is-ground?)))))
    )   



;(defmethod ground?* :around (obj)
;  (describe obj)
;   (setf oo obj)
;  (call-next-method))

;(defmethod ground-type? :around (obj)
;  (describe obj)
;  (setf oo obj)
;  (call-next-method))



(defmethod ground?* (obj)
  (print (format nil "ground?* fallback on ~A" obj))
  (describe obj)
  nil)

(defmethod ground?* ((expr number-expr))
  T)

(defmethod ground?* ((expr name-expr))
  (if ; (boolean-equality-op? expr) returns nil on = ??
      (equality-operator? expr)
      (ground-type? (car (domain-types (type expr))))
    (or (builtin-op? expr)
        (is-variable? expr)
        (and (not (has-no-definition? expr))
             (ground?-memo expr #'ground?-new-name-expr)))))

(defun ground?-new-name-expr (expr)
  (ground?* (norm-instantiate expr)))


(defmethod ground?* ((expr lambda-expr))
  (ground?* (expression expr)))

(defmethod ground?* ((expr adt-name-expr))
  T)

(defmethod ground?* ((expr tuple-expr))
  (every #'ground?* (exprs expr)))

(defmethod ground?* ((expr record-expr))
  (every #'ground?* (assignments expr)))

(defmethod ground?* ((expr application))
  (and (or (boolean-op? (operator expr) '(AND & \\/ OR /\\ IMPLIES => WHEN))
           (this-name-expr? (operator expr) '|if_def| 'IF) 
           (ground?* (operator expr)))
       (ground?* (arguments expr))))  

(defmethod ground?* ((expr list))
  (every #'ground?* expr))


(defmethod ground?* ((expr uni-assignment))
  (ground?* (expression expr)))

(defmethod ground?* ((expr field-application))
  (ground?* (argument expr)))

(defmethod ground?* ((expr projection-application))
  (ground?* (argument expr)))

(defmethod ground?* ((expr update-expr))
  (ground?* (translate-update-to-if! expr)))

(defmethod ground?* ((expr cases-expr))
  (ground?* (translate-cases-to-if expr)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if I don't know what it is, it is probably not what I want
;;
(defmethod ground-type? (obj)
  (print (format nil "ground-type fallback on ~A" obj))
  (describe obj)
  nil)

(defmethod ground-type? ((typ subtype))
  (ground-type? (find-supertype typ)))

(defmethod ground-type? ((typ funtype))
  nil)

(defmethod ground-type? ((typ recordtype))  
  (every #'ground-type? (fields typ)))


(defmethod ground-type? ((typ tupletype))
  (every #'ground-type? (types typ)))

(defmethod ground-type? ((typ number))
  T)

;(defmethod ground-type? ((typ adt-type-name))
;  (ground?-memo typ #'ground-type?-new-adt))

(defun ground-type?-new-adt (typ)
  (every #'(lambda (cnstr)
             (every #'ground-type?
                    (if (arity0? cnstr)
                        nil
                      (domain-types (type cnstr)))))
         (constructors typ)))
                 
;(defmethod ground-type? ((typ uninterpreted-type-name)) ; boolean, number
;  (cond ((tc-eq typ *number*)
;         T)
;        ((tc-eq typ *boolean*)
;         T)
;        (T
;         nil)))

(defmethod  ground-type? ((typ type-name)) ; number checked here. why ??
  (cond ((tc-eq typ *number*)   ; unint and adt set but not subclass
         T)
        ((tc-eq typ *boolean*)
         T)
        ((adt? typ)
         (ground?-memo typ #'ground-type?-new-adt))         
        (T
         nil)))
  
(defmethod ground-type? ((typ dep-binding))
  (ground-type? (type typ)))

(defmethod ground-type? ((typ field-decl))
  (ground-type? (type typ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun builtin-op? (expr)
  (or (tc-eq expr *true*)
      (tc-eq expr *false*)
      (this-name-expr? expr '|reals| '-)
      (this-name-expr? expr '|reals| '+)
      (this-name-expr? expr '|reals| '*)
      (this-name-expr? expr '|reals| '/)
      (this-name-expr? expr '|reals| '<)
      (this-name-expr? expr '|reals| '>)
      (this-name-expr? expr '|reals| '|real_pred|)
      (this-name-expr? expr '|rationals| '|rational_pred|)
      (this-name-expr? expr '|integers| '|integer_pred|)
      (this-name-expr? expr '|floor_ceil| '|floor|)
      (this-name-expr? expr '|floor_ceil| '|ceiling|)
      (this-name-expr? expr '|booleans| '|not|)))

;; not the boolean operators that decide recursive calls

