;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; store-object.lisp -- 
;; Author          : Carl Witty
;; Created On      : Sat Sep 17 13:46:44 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:41:52 1998
;; Update Count    : 2
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

;;; This package provides functions for efficiently storing Lisp
;;; objects in a file.  It correctly handles circularity and sharing.
;;; It also provides a CLOS protocol with which users of the package
;;; can specify and modify its behavior.  It uses highly efficient I/O
;;; and does very little consing.

;;; The main entry points are store-object-to-file and
;;; fetch-object-from-file.  The function store-object-to-file takes
;;; an object and a filename, and stores the object in that file; the
;;; function fetch-object-from-file takes a filename and returns the
;;; object which was stored in that file.

;;; Currently, the following types are supported by store-object:
;;; cons, hash-table, integer, package, string, symbol.  The methods
;;; for package and symbol assume that the package structure will be
;;; the same on reading and writing, and that the current package uses
;;; the common-lisp package.

;;; To add new supported types (or modify the behavior on existing
;;; types), you can add methods on store-object*.  The existing
;;; methods may be overridden (except for the method for symbols).

;;; A store-object* method takes an object and decides how many words
;;; of storage it will take to store it (where a word is an
;;; (unsigned-byte 32).  It then calls the macro reserve-space, giving
;;; it the number of words of storage required and a body which
;;; actually stores the object by calling push-word for every word.
;;; Within the body of a reserve-space macro, the function store-obj
;;; can be called, which takes an object and returns a word
;;; identifying that object.  Strings can be stored with push-string;
;;; (push-string-length str) words of storage must be reserved.

;;; The first word stored by a store-object* method must be returned
;;; by store-obj on a symbol, where that symbol will be used to
;;; determine how to reconstruct the object.

;;; To handle circularity, object reconstruction takes place in two
;;; stages.  In the first stage, the object is created; in the second
;;; stage, the object is modified to correctly point to other objects.
;;; (Objects like symbols, integers, and strings, which cannot contain
;;; arbitrary other objects, are reconstructed in one step.)

;;; To handle the first stage, object creation, the type identifier
;;; (the symbol which was the first word pushed by the corresponding
;;; store-object*) is looked up, and a (get type-identifier 'fetcher)
;;; is done.  If this exists, it must be a function of no arguments
;;; which returns an object which will be eql to the final desired
;;; object.  It can do this by returning an already-existing object
;;; (symbols and packages do this), by creating the exact desired
;;; final object (integers do this), or by creating an object which
;;; will be updated later to contain the correct other objects
;;; ("structured" objects, which may contain other objects, must do
;;; this, unless you know that there are no circularities involved).

;;; In the second stage, object updating, the objects which were
;;; registered during the first stage as needing to be updated are
;;; updated by calling (update-fetched object) on each one.

;;; The fetcher and the update-fetched method may use the stored-word
;;; macro to find the nth word pushed by store-object* and the
;;; fetch-obj function to find the object corresponding to a given
;;; word.  (Don't call fetch-obj from the fetcher unless you know how
;;; this system works and you've thought about circularities.)  The
;;; fetcher may also call register-for-updating if update-fetched
;;; needs to be called on this object.  Strings that were stored with
;;; push-string may be retrieved with fetch-temp-string, which takes
;;; one argument and returns a temporary string (it might be modified
;;; by further calls to fetch-temp-string).

;;; There's a special case for CLOS objects: if there is no fetcher
;;; for a type identifier, then the object is created using something
;;; equivalent to (make-instance type-identifier), and it is
;;; registered for updating.  This will be more efficient if you
;;; execute the macro register-make-instance on the type identifier.

;; This hash table is never deleted, only cleared; so it grows to the
;; right size and stays there.
(defvar *store-object-hash* (make-hash-table :test #'eq))

(defvar *store-object-ptr*)
(defvar *store-object-substs*)
(defvar *restore-object-parent*)
(defvar *restore-object-parent-slot*)

;; The following macro is used to help avoid consing.  If a
;; non-reentrant function needs a temporary vector of a given size,
;; and the function is called often enough that you don't want to cons
;; a new array every time, you can use this macro to make sure that
;; the array is large enough.  This will never shrink the array; if
;; you call it once with a large size, the array will remain large
;; forever.

;; I don't use adjustable arrays because I'm worried that they might
;; be less efficient on some Lisps.
(defmacro ensure-vector-size (array current-size size)
  `(let ((required-size ,size))
    (when (> required-size ,current-size)
      (let* ((new-current-size
	      (max (ceiling (* 1.2 ,current-size)) required-size))
	     (new-array (make-array new-current-size
				    :element-type (array-element-type ,array)
				    :fill-pointer
				    (array-has-fill-pointer-p
				     ,array))))
	(setf (subseq new-array 0 ,current-size)
	      ,array)
	(setf ,array new-array)
	(setf ,current-size new-current-size)))))

(defvar *store-object-store*
  (make-array 10000 :element-type '(unsigned-byte 32)))
(defvar *store-object-store-size* 10000)

(defmacro object-store (addr)
  `(aref (the (vector (unsigned-byte 32) *) *store-object-store*)
    (the fixnum ,addr)))

(defvar *storing-or-fetching* nil)

(defun store-object (obj)
  (when *storing-or-fetching*
    (error "Attempt to store object while in ~s~@
            (sorry, these functions are not reentrant)" *storing-or-fetching*))
  (clrhash *store-object-hash*)
  (let ((*package* (find-package :pvs))
	(*store-object-ptr* 2)
	(*storing-or-fetching* 'store-object))
    (store-obj obj)
    (setf (object-store 0) 1)
    (setf (object-store 1) *store-object-ptr*)))

(defun store-object-to-file (obj file)
  (let ((*store-object-substs* nil))
    (store-object obj)
    (with-open-file (f file
		       :direction :output
		       :element-type '(unsigned-byte 32)
		       :if-exists :supersede)
      (write-array f *store-object-store* 0 (object-store 1))))
  t)

(defun store-obj (obj)
  (or (gethash obj *store-object-hash*)
      (prog1
	  (setf (gethash obj *store-object-hash*)
		*store-object-ptr*)
	(store-object* obj))))

(defvar *reserve-space-ptr*)

(defmacro reserve-space (amt &body body)
  `(let ((*reserve-space-ptr* *store-object-ptr*))
    (incf *store-object-ptr* ,amt)
    (ensure-vector-size *store-object-store* *store-object-store-size*
     *store-object-ptr*)
    ,@body))

(defun push-word (word)
  (let ((sword (or (cdr (assoc word *store-object-substs* :test #'=))
		   word)))
    (setf (object-store *reserve-space-ptr*) sword))
  (incf *reserve-space-ptr*))

(defmethod store-object* ((obj t))
  (break "Attempt to store object ~S of unhandled type." obj)
  (cerror "Store NIL instead."
	  "Attempt to store object ~S of unhandled type."
	  obj)
  (store-object* nil))


;; It would be easy to change push-string-length, push-string,
;; fetch-temp-string, and fetch-string to store strings 4 characters
;; to the word instead of 1.

(defun push-string-length (str)
  (+ 1 (length str)))

(defun push-string (str)
  (let ((len (length str)))
    (push-word len)
    (dotimes (i len)
      ;; char-int isn't standard Common Lisp (it was removed from the
      ;; language), but it works in Lucid and Allegro
      (push-word (char-int (char str i))))))

;; Symbols get special treatment because we need to break the
;; recursion of every object requiring a type-identifier which is a
;; symbol.
(defmethod store-object* ((obj symbol))
  (let* ((name (symbol-name obj))
	 (namelen (push-string-length name)))
    (if (eq obj (find-symbol name))
	(reserve-space (1+ namelen)
	  (push-word 0)
	  (push-string name))
	(reserve-space (+ 2 namelen)
	  (push-word (store-obj 'packaged-symbol))
	  (push-word (store-obj (symbol-package obj)))
	  (push-string name)))))

#+lucid
(defun fetch-monitoring ()
  (monitor fetch-object fetch-obj fetch-symbol fetch-integer fetch-string
	   fetch-temp-string fetch-packaged-symbol fetch-moduleref
	   fetch-declref update-fetched fast-make-instance fill)
  (monitor-methods 'update-fetched))

(defvar *fetch-object-table* (make-array '10000))
(defvar *fetch-object-table-size* 10000)
(defvar *fetch-object-not-present*)

;; Communications between fetch-object, fetch-obj, update-fetched, and
;; fetchers are done (in large part) with global variables.  This is
;; so the body of fetchers and update-fetched methods can be as simple
;; as possible.

;; This is a stack for register-for-updating.  I use parallel arrays
;; (yuck!) for speed.
(defvar *fetch-object-update-obj* (make-array 1000))
(defvar *fetch-object-update-obj-size* 1000)
(defvar *fetch-object-update-ptr* (make-array 10000))
(defvar *fetch-object-update-ptr-size* 1000)
(defvar *fetch-object-update-end*)
(defvar *fetch-object-case-ok?*)

(defvar *fetch-object-ptr*)

(defun fetch-object (start)
  (when *storing-or-fetching*
    (error "Attempt to fetch object while in ~s~@
            (sorry, these functions are not reentrant)" *storing-or-fetching*))
  (let ((*package* (find-package :pvs))
	(*fetch-object-not-present* (cons nil nil))
	(*fetch-object-update-end* 0)
	(*storing-or-fetching* 'fetch-object)
	(*fetch-object-case-ok?* nil))
    (ensure-vector-size *fetch-object-table* *fetch-object-table-size*
			(object-store start))
    (fill *fetch-object-table* *fetch-object-not-present*
	  :end (object-store start))
    (prog1
	(fetch-obj (1+ start))
      (block loop
	(loop
	 (when (= *fetch-object-update-end* 0)
	   (return-from loop nil))
	 (decf *fetch-object-update-end*)
	 (let ((*fetch-object-ptr* (svref *fetch-object-update-ptr*
					  *fetch-object-update-end*)))
	   (update-fetched (svref *fetch-object-update-obj*
				  *fetch-object-update-end*))))))))

#+allegro
(defun fetch-object-from-file (file)
  (with-open-file (f file :direction :input :element-type '(unsigned-byte 32))
    (setf (object-store 0) (read-byte f))
    (setf (object-store 1) (read-byte f))
    (let* ((reverse-endian (and (not (= (object-store 0) 1))
				(= (reverse-endian (object-store 0)) 1)))
	   (size (cond ((= (object-store 0) 1)
			(object-store 1))
		       (reverse-endian
			(reverse-endian (object-store 1)))
		       (t (object-store 0)))))
      (ensure-vector-size *store-object-store* *store-object-store-size* size)
      (with-open-file (f file :direction :input
			 :element-type '(unsigned-byte 32))
	(lisp:read-sequence *store-object-store* f
			    :start 0 :end size))
      (when reverse-endian
	(dotimes (i size)
	  (setf (object-store i)
		(reverse-endian (object-store i)))))))
  (fetch-object (if (or (= (object-store 0) 1)
			(= (reverse-endian (object-store 0)) 1))
		    1 0)))

#+lucid
(defun fetch-object-from-file (file)
  (with-open-file (f file :direction :input :element-type '(unsigned-byte 32))
    (setf (object-store 0) (read-byte f))
    (ensure-vector-size *store-object-store* *store-object-store-size*
			(object-store 0))
    (read-array f *store-object-store* 1 (object-store 0)))
  (fetch-object))

#+gcl
(defun fetch-object-from-file (file)
  (with-open-file (f file :direction :input :element-type '(unsigned-byte 32))
    (setf (object-store 0) (read-byte f))
    (ensure-vector-size *store-object-store* *store-object-store-size*
			(object-store 0)))
  (with-open-file (f file :direction :input :element-type '(unsigned-byte 32))
    (read-sequence *store-object-store* f (object-store 0)))
  (fetch-object))

#+gcl
(defun read-sequence (sequence stream end)
  (pcl::dotimes (i (1- end))
		(setf (aref sequence i) (read-byte f))))

(defmacro queue-update-fetched (obj ptr)
  `(progn
    (ensure-vector-size *fetch-object-update-obj*
     *fetch-object-update-obj-size* (1+ *fetch-object-update-end*))
    (ensure-vector-size *fetch-object-update-ptr*
     *fetch-object-update-ptr-size* (1+ *fetch-object-update-end*))
    (setf (svref *fetch-object-update-obj* *fetch-object-update-end*)
     ,obj)
    (setf (svref *fetch-object-update-ptr* *fetch-object-update-end*)
     ,ptr)
    (incf *fetch-object-update-end*)))

(defvar *registered-for-updating*)

(defun register-for-updating ()
  (setf *registered-for-updating* t))

(defun fetch-obj (ptr)
  (let ((cached-val (svref *fetch-object-table* ptr)))
    (if (not (eq cached-val *fetch-object-not-present*))
	cached-val
	(let* ((*registered-for-updating* nil)
	       (*fetch-object-ptr* ptr)
	       (val (if (= (object-store ptr) 0)
			(fetch-symbol)
			(let* ((type (fetch-obj (object-store ptr)))
			       (fetcher (get type 'fetcher)))
			  (if fetcher
			      (funcall fetcher)
			      (progn
				(register-for-updating)
				(fast-make-instance type)))))))
	  (when *registered-for-updating*
	    (queue-update-fetched val ptr))
	  (setf (svref *fetch-object-table* ptr) val)
	  val))))

(defmacro stored-word (offset)
  `(object-store (+ *fetch-object-ptr* ,offset)))

;; Lucid and Allegro have different (CLtL1 and CLtL2) types for
;; characters in strings.  This is a (hopefully) portable way of
;; handling the problem.
(defvar *temp-string*
  (make-array 10
	      :element-type (array-element-type "")
	      :fill-pointer t))
(defvar *temp-string-len* 10)

(defun fetch-temp-string (offset)
  (let ((len (stored-word offset)))
    (ensure-vector-size *temp-string* *temp-string-len* len)
    (setf (fill-pointer *temp-string*) len)
    ;; int-char isn't standard Common Lisp any more, but it works in
    ;; Lucid and Allegro
    (dotimes (i len)
      (setf (aref *temp-string* i) (int-char (stored-word (+ offset 1 i)))))
    *temp-string*))

#+allegro
(defun int-char (int)
  (code-char int))

(defun fetch-symbol ()
  (let ((str (fetch-temp-string 1)))
    (unless *fetch-object-case-ok?*
      (if (every #'(lambda (ch)
		     (or (not (alpha-char-p ch))
			 (upper-case-p ch)))
		 str)
	  (setq *fetch-object-case-ok?* :lower)
	  (setq *fetch-object-case-ok?* t)))
    (if (eq *fetch-object-case-ok?* :lower)
	(intern (convert-fetched-string-to-lowercase str))
	(intern str))))

(defun convert-fetched-string-to-lowercase (str)
  (let ((in-vertbars nil))
    (dotimes (i (length str))
      (let ((ch (char str i)))
	(cond ((char= ch #\|)
	       (setf in-vertbars (not in-vertbars)))
	      (in-vertbars nil)
	      ((upper-case-p ch)
	       (setf (char str i) (char-downcase ch)))))))
  str)
	
    
;;; Here's the definition of fast-make-instance.  I can't believe this
;;; is a performance win, but it is (at least in Lucid).

(defvar *fast-make-instance-makers* (make-hash-table :test #'eq))

(defun fast-make-instance (type)
  (let ((maker (or (gethash type *fast-make-instance-makers*)
		   (setf (gethash type *fast-make-instance-makers*)
			 (compile nil `(lambda () (make-instance
						      ',type)))))))
    (funcall maker)))

(defmacro declare-make-instance (type)
  `(setf (gethash ',type *fast-make-instance-makers*)
    (compile nil '(lambda () (make-instance ',type)))))

;;; From here on down are the methods to handle the various supported
;;; types.  They use the published interfaces exclusively, except that
;;; fetch-string relies on the format used by push-string.

(defmethod store-object* ((obj string))
  (let ((len (push-string-length obj)))
    (reserve-space (1+ len)
      (push-word (store-obj 'string))
      (push-string obj))))

(setf (get 'string 'fetcher) 'fetch-string)
(defun fetch-string ()
  (let* ((len (stored-word 1))
	 (str (make-string len)))
    (dotimes (i len)
      (setf (char str i) (int-char (stored-word (+ 2 i)))))
    str))

;; (defmethod store-object* ((obj pathname))
;;   (store-object* (namestring obj)))

;; It would be easy to reduce the storage required for integers
;; by storing them with a radix higher than 10.
(defmethod store-object* ((obj integer))
  (let* ((str (format nil "~d" obj))
	 (len (push-string-length str)))
    (reserve-space (1+ len)
      (push-word (store-obj 'integer))
      (push-string str))))

(setf (get 'integer 'fetcher) 'fetch-integer)
(defun fetch-integer ()
  (let ((str (fetch-temp-string 1)))
    (parse-integer str)))

(defmethod store-object* ((obj cons))
  (if (true-listp obj)
      (reserve-space (+ (length obj) 2)
	(push-word (store-obj 'list))
	(push-word (length obj))
	(let ((i 0))
	  (dolist (x obj)
	    (when (and (not *saving-theory*)
		       (typep x 'datatype-or-module))
	      (remhash x *store-object-hash*))
	    (push-word (store-obj x))
	    (incf i))))
      (reserve-space 3
	(push-word (store-obj 'cons))
	(push-word (store-obj (car obj)))
	(push-word (store-obj (cdr obj))))))

(defun true-listp (obj)
  (or (null obj)
      (and (listp obj)
	   (true-listp (cdr obj)))))

(setf (get 'list 'fetcher)
      #'(lambda ()
	  (register-for-updating)
	  (make-list (stored-word 1))))

(setf (get 'cons 'fetcher)
      #'(lambda ()
	  (register-for-updating)
	  (cons 0 0)))

(defmethod update-fetched ((obj cons))
  (cond ((equal obj '(0 . 0))
	 (setf (car obj) (fetch-obj (stored-word 1)))
	 (setf (cdr obj) (fetch-obj (stored-word 2))))
	(t (let ((size (stored-word 1)))
	     (dotimes (i size)
	       (setf (nth i obj) (fetch-obj (stored-word (+ i 2)))))))))

#-gcl
(defmethod store-object* ((obj package))
  (let* ((name (package-name obj))
	 (len (push-string-length name)))
    (reserve-space (1+ len)
      (push-word (store-obj 'package))
      (push-string name))))

(setf (get 'package 'fetcher) 'fetch-package)
(defun fetch-package ()
  (let ((name (fetch-temp-string 1)))
    (find-package name)))

(setf (get 'packaged-symbol 'fetcher) 'fetch-packaged-symbol)
(defun fetch-packaged-symbol ()
  (let ((package (fetch-obj (stored-word 1)))
	(name (fetch-temp-string 2)))
    (intern name package)))

#-gcl
(defmethod store-object* ((obj hash-table))
  (let ((entries nil))
    (maphash #'(lambda (key val)
		 (push (cons key val) entries))
	     obj)
    (reserve-space (+ 2 (* 2 (length entries)))
      (push-word (store-obj 'hash-table))
      (push-word (length entries))
      (dolist (ent entries)
	(push-word (store-obj (car ent)))
	(push-word (store-obj (cdr ent)))))))

(setf (get 'hash-table 'fetcher) 'fetch-hash-table)
(defun fetch-hash-table ()
  (register-for-updating)
  ;; make hash-table with size 1 larger than number of elements
  ;; to guard against size 0
  (make-hash-table :test #'eq :size (1+ (stored-word 1))))

#-gcl
(defmethod update-fetched ((obj hash-table))
  (let ((size (stored-word 1)))
    (dotimes (i size)
      (setf (gethash (fetch-obj (stored-word (* 2 (1+ i)))) obj)
	    (fetch-obj (stored-word (1+ (* 2 (1+ i)))))))))


#+allegro
(defun read-array (stream array &optional start end)
  (lisp:read-sequence array stream :start start :end end))

#+allegro
(defun write-array (stream array &optional start end)
  (lisp:write-sequence array stream :start start :end end))

(defun remove-store-object-caches ()
  (setq *store-object-hash* nil)
  (setq *fetch-object-table* nil)
  (setq *fetch-object-update-obj* nil)
  (setq *fetch-object-update-ptr* nil)
  (setq *temp-string* nil))

(defun reset-store-object-caches ()
  (setq *store-object-hash* (make-hash-table :test #'eq
					     ;;:rehash-size 2.0
					     ))
  (setq *store-object-store*
	(make-array 10000 :element-type '(unsigned-byte 32)))
  (setq *store-object-store-size* 10000)
  (setq *fetch-object-table* (make-array '10000))
  (setq *fetch-object-table-size* 10000)
  (setq *fetch-object-update-obj* (make-array 1000))
  (setq *fetch-object-update-obj-size* 1000)
  (setq *fetch-object-update-ptr* (make-array 10000))
  (setq *fetch-object-update-ptr-size* 1000)
  (setq *temp-string*
	(make-array 10
		    :element-type (array-element-type "")
		    :fill-pointer t))
  (setq *temp-string-len* 10))

(defun show-store-object-cache-sizes ()
  (format t "~2%Describe *store-object-hash*")
  (describe *store-object-hash*)
  (format t "~2%Describe *store-object-store*")
  (describe *store-object-store*)
  (format t "~2%Describe *fetch-object-table*")
  (describe *fetch-object-table*)
  (format t "~2%Describe *fetch-object-update-obj*")
  (describe *fetch-object-update-obj*)
  (format t "~2%Describe *fetch-object-update-ptr*")
  (describe *fetch-object-update-ptr*))

(defun reverse-endian (i)
  (declare (type (unsigned-byte 32) i))
  (+ (* 16777216 (ldb (byte 8 0) i))
     (* 65536 (ldb (byte 8 8) i))
     (* 256 (ldb (byte 8 16) i))
     (ldb (byte 8 24) i)))
