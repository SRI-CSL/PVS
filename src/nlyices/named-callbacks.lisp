;;===============================================
;; Support for accessing lisp callbacks by name
;;===============================================

(defpackage :named-callbacks
  (:nicknames :ncb)
  (:export :add-callback :get-callbacks :free-callbacks :num-callbacks
	   :show-callback-names :callback-desc)
  (:use :foreign-functions :common-lisp))

(in-package :ncb)


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


;;----------------------------------
;; Number of registerd callabacks
;;----------------------------------

(defun num-callbacks () (length *callback-list*))


;;----------------------------------------
;; register foreign-callable function f
;; f = symbol = Lisp function name
;; name = alias for f used in C
;;----------------------------------------

(defun add-callback (f name)
  (let ((id (nth-value 1 (register-foreign-callable f :reuse t))))
    (setq *callback-list* (cons (cons name id) *callback-list*))))



;;-------------------------------------------------------
;; Allocate and initialize a foreign object to contain 
;; the callback descriptors.
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
	#+allegro
	(setf (fslot-value fobj i 'name) (excl:string-to-native (car cb)))
	(setf (fslot-value fobj i 'id) (cdr cb))))
    fobj))
	 



;;--------------------------------------------------------------------
;; Delete a callback descriptor constructed by the previous function
;; Warning: make sure *callback-list* does not change
;; between the call to get-callbacks and free-callbacks.
;; (i.e., don't reload "fcpo.lisp" between)
;;--------------------------------------------------------------------

(defun free-callbacks (fobj)
  (let ((n (length *callback-list*)))
    (dotimes (i n) (excl:aclfree (fslot-value fobj i)))
    (excl:aclfree fobj))
  nil)


;;------------------------------
;; Print all the callback names 
;;------------------------------

(defun show-callback-names () 
  (dolist (c *callback-list*) (format t "   ~a~%" (car c))))


