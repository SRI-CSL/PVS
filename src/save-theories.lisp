;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save-theories.lisp -- provides the facilities for saving PVS theories.
;; Author          : Carl Witty (with mods by Sam Owre)
;; Created On      : Sun Aug 14 14:44:02 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:36:10 1998
;; Update Count    : 5
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defun save-theories (file)
  ;;(update-stored-mod-depend)
  (let ((theories (get-theories-to-save file)))
    (format t "~%Saving ~a" (mapcar #'id theories))
    (store-object-to-file (cons *binfile-version* theories)
			  (make-binpath file))))

(defmethod get-theories-to-save (file)
  (mapcan #'get-theories-to-save (get-theories file)))

(defmethod get-theories-to-save ((th module))
  (list th))

(defmethod get-theories-to-save ((adt datatype))
  (append (adt-generated-theories adt) (list adt)))

(defun get-theories-from-file (filename)
  (let* ((file (make-binpath filename))
	 (vtheories (fetch-object-from-file file)))
    (if (and (listp vtheories)
	     (integerp (car vtheories))
	     (= (car vtheories) *binfile-version*))
	(let ((theories nil))
	  (dolist (th (cdr vtheories))
	    (unless (and (generated-by th)
			 (member (generated-by th) (cdr vtheories)
				 :test #'(lambda (x y) (eq x (id y)))))
	      (push th theories)))
	  (dolist (th theories)
	    (when (typep th 'datatype)
	      (setf (gethash (string (id (adt-theory th))) *pvs-files*)
		    (cons (generated-file-date th)
			  (adt-generated-theories th)))))
	  (nreverse theories))
	(error "Bin file version is out of date"))))

(defun list-of-modules ()
  (let ((theory-list nil))
    (maphash #'(lambda (id mod)
		 (declare (ignore id))
		 (push mod theory-list))
	     *pvs-modules*)
    theory-list))

(defvar *stored-mod-depend* (make-hash-table :test #'eq))

(defun update-stored-mod-depend ()
  (clrhash *stored-mod-depend*)
  (dolist (mod (list-of-modules))
    (setf (gethash (id mod) *stored-mod-depend*)
	  (nconc (when (typep mod '(and datatype
					(not enumtype)
					(not inline-datatype)))
		   (mapcar #'id (adt-generated-theories mod)))
		 (mapcan #'(lambda (m)
			     (if (typep m '(and datatype
						(not enumtype)
						(not inline-datatype)))
				 (nconc (mapcar #'id
					  (adt-generated-theories m))
					(list (id m)))
				 (list (id m))))
		   (all-importings mod))))))


;; While we're saving a particular theory, we don't want to save any
;; other modules it might refer to, or any declarations that belong to
;; other modules.  *saving-theory* is bound to the theory being saved.
(defvar *saving-theory* nil)

(defmethod store-object* :around ((obj datatype-or-module))
  (if (typep obj '(not (or inline-datatype enumtype)))
      (if *saving-theory*
	  (reserve-space 2
	    (push-word (store-obj 'moduleref))
	    (push-word (store-obj (id obj))))
	  (let ((*saving-theory* obj))
	    (call-next-method)))
      (call-next-method)))

(setf (get 'moduleref 'fetcher) 'fetch-moduleref)
(defun fetch-moduleref ()
  (let* ((mod-name (fetch-obj (stored-word 1)))
	 (theory (get-theory mod-name)))
    (unless theory
      ;;(break "Attempt to fetch unknown theory ~s" mod-name)
      (error "Attempt to fetch unknown theory ~s" mod-name))
    theory))

(defmethod update-fetched :around ((obj datatype-or-module))
  (call-next-method)
  (unless (typep obj 'inline-datatype)
    (assert (filename obj))
    (pushnew (id obj) *bin-theories-set*)
    (setf (gethash (id obj) *pvs-modules*) obj)))

(defmethod store-object* :around ((obj declaration))
  (with-slots (module) obj
    (if (and module (not (eq module *saving-theory*)))
	(reserve-space 3
	  (unless (or (from-prelude? module)
		      (typep module '(or library-theory library-datatype))
		      (memq (id module)
			    (gethash (id *saving-theory*) *stored-mod-depend*)))
	    (error "Attempt to store declaration in illegal theory"))
	  (push-word (store-obj 'declref))
	  (push-word (store-obj (id module)))
	  (push-word (position obj (all-decls module))))
	(call-next-method))))

(setf (get 'declref 'fetcher) 'fetch-declref)
(defun fetch-declref ()
  (let* ((mod-name (fetch-obj (stored-word 1)))
	 (theory (get-theory mod-name)))
    (unless theory
      ;;(break "Attempt to fetch declaration from unknown theory ~s" mod-name)
      (error "Attempt to fetch declaration from unknown theory ~s" mod-name))
    (let* ((decl-pos (stored-word 2))
	   (decl (nth decl-pos (all-decls theory))))
      (unless decl
	(error "Declaration was not found"))
      decl)))

(defmethod store-object* :around ((obj type-name))
  (when (typep (adt obj) 'datatype)
    (setf (adt obj) (id (adt obj))))
  (call-next-method))

;;; methods for *integer*, etc. have been moved to prelude-methods.lisp

(defmethod store-object* ((obj vector))
  (let ((len (length obj)))
    (reserve-space (+ len 2)
      (push-word (store-obj 'vector))
      (push-word len)
      (dotimes (i len)
	(push-word (store-obj (svref obj i)))))))

(setf (get 'vector 'fetcher) 'fetch-vector)
(defun fetch-vector ()
  (let* ((len (stored-word 1))
	 (vec (make-array len)))
    (dotimes (i len)
      (setf (svref vec i) (fetch-obj (stored-word (+ 2 i)))))
    vec))

(defmethod store-object* ((obj context-entry))
  (reserve-space 8
    (push-word (store-obj 'context-entry))
    (push-word (store-obj (ce-file obj)))
    (push-word (store-obj (ce-write-date obj)))
    (push-word (store-obj (ce-proofs-date obj)))
    (push-word (store-obj (ce-object-date obj)))
    (push-word (store-obj (ce-dependencies obj)))
    (push-word (store-obj (ce-theories obj)))
    (push-word (store-obj (ce-extension obj)))))

(setf (get 'context-entry 'fetcher) 'fetch-context-entry)
(defun fetch-context-entry ()
  (make-context-entry
   :file (fetch-obj (stored-word 1))
   :write-date (fetch-obj (stored-word 2))
   :proofs-date (fetch-obj (stored-word 3))
   :object-date (fetch-obj (stored-word 4))
   :dependencies (fetch-obj (stored-word 5))
   :theories (fetch-obj (stored-word 6))
   :extension (fetch-obj (stored-word 7))))

(defmethod store-object* ((obj theory-entry))
  (reserve-space 5
    (push-word (store-obj 'theory-entry))
    (push-word (store-obj (te-id obj)))
    (push-word (store-obj (te-status obj)))
    (push-word (store-obj (te-dependencies obj)))
    (push-word (store-obj (te-formula-info obj)))))

(setf (get 'theory-entry 'fetcher) 'fetch-theory-entry)
(defun fetch-theory-entry ()
  (make-theory-entry
   :id (fetch-obj (stored-word 1))
   :status (fetch-obj (stored-word 2))
   :dependencies (fetch-obj (stored-word 3))
   :formula-info (fetch-obj (stored-word 4))))

(defmethod store-object* ((obj formula-entry))
  (reserve-space 4
    (push-word (store-obj 'formula-entry))
    (push-word (store-obj (fe-id obj)))
    (push-word (store-obj (fe-status obj)))
    (push-word (store-obj (fe-proof-refers-to obj)))))

(setf (get 'formula-entry 'fetcher) 'fetch-formula-entry)
(defun fetch-formula-entry ()
  (make-formula-entry
   :id (fetch-obj (stored-word 1))
   :status (fetch-obj (stored-word 2))
   :proof-refers-to (fetch-obj (stored-word 3))))

(defmethod store-object* ((obj declaration-entry))
  (reserve-space 5
    (push-word (store-obj 'declaration-entry))
    (push-word (store-obj (de-id obj)))
    (push-word (store-obj (de-class obj)))
    (push-word (store-obj (de-type obj)))
    (push-word (store-obj (de-theory-id obj)))))

(setf (get 'declaration-entry 'fetcher) 'fetch-declaration-entry)
(defun fetch-declaration-entry ()
  (make-declaration-entry
   :id (fetch-obj (stored-word 1))
   :class (fetch-obj (stored-word 2))
   :type (fetch-obj (stored-word 3))
   :theory-id (fetch-obj (stored-word 4))))
