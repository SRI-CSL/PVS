;;===================================================
;; Make the polynomial operations defined in "prep"
;; callable from foreign code
;;====================================================

(defpackage "foreign-callable-polynomial-operations"
  (:nicknames "fcpo")
  (:export "get-callbacks"))

(in-package "fcpo")

(require :foreign)
(use-package "foreign-functions")
(use-package "prep")

;;-------------------------------------------------------------
;; We keep a list of pairs (string . id) in *callback-list*
;; - each string identifies a foreign callable function
;; - id is the index of that function in allegro's table 
;;   (as returned by register-foreign-callable)
;;
;; The function get-callbacks converts *callbacklist* into
;; an array of pairs [native-strings, id] in a structure
;; understood by C.
;;-------------------------------------------------------------

(defvar *callback-list*)

(setq *callback-list* nil)

;; register foreign-callable function f
;; name = alias for f used in C
(defun add-callback (f name)
  (let ((id (nth-value 1 (register-foreign-callable f :reuse t))))
    (setq *callback-list* (cons (cons name id) *callback-list*))))



;;-------------------------------------------------------
;; Allocate and initialize a foreign object to contain 
;; the callback description.
;;-------------------------------------------------------

;; this should correspond to the C type
;; but there may be alignment issues?
;; struct { 
;;   char *name; 
;;   int id;
;; }
;;
(def-foreign-type callback-desc (:struct (name (* :char)) (id :int)))

(defun get-callbacks () 
  (let* ((n (length *callback-list*))
	 (fobj (allocate-fobject (list :array 'callback-desc n) :foreign-static-gc)))
    (dotimes (i n)
      (let ((cb (nth i *callback-list*)))	     
	(setf (fslot-value fobj i 'name) (excl:string-to-native (car cb)))
	(setf (fslot-value fobj i 'id) (cdr cb))))
    fobj))
	 





;;-----------------------------------------------------------------
;; Each object known by the C-layer is registered as a lisp value
;; and is identified by its index in the corresponding table.
;; This function allows the C code to delete an object
;;-----------------------------------------------------------------

(defun-foreign-callable delete-object ((index :int))
  (unregister-lisp-value index))

(add-callback 'delete-object "delete-object")



;;----------------------------------------------------------
;; Variable declaration:
;; - *var_prefix* is a the prefix used for variable names
;; - each variable is a symbol 'prefix<id>
;;----------------------------------------------------------

(defvar *var-prefix* "x")

;; set/change the prefix
;; s must be a native string in C's conventions 
;; (i.e., array of char, terminated by '\0')
(defun-foreign-callable set-prefix ((s (* :char)))
  (setq *var-prefix* (excl:native-to-string s)))

(add-callback 'set-prefix "set-prefix")


;; create a variable with the given id
;; - the variable is added to the list maintained in prep
(defun-foreign-callable new-var ((id :int))
  (let ((x (intern (format nil "~a~a" *var-prefix* id))))
    (set-variables (cons x (get-variables))) ;; update prep's variable list    
    (register-lisp-value x)))

(add-callback 'new-var "new-var")



