;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; linked-hash-table.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Sep  1 03:24:33 2004
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Sep  1 03:26:08 2004
;; Update Count    : 1
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(export '(make-lhash-table copy-lhash-table get-lhash map-lhash lhash-table
			   lhash-next))

(defstruct (linked-hash-table (:conc-name lhash-))
  (table nil :type hash-table)
  (next nil :type (or symbol linked-hash-table)))

(defun make-lhash-table (&key (test 'eql) (size 67)
				 (rehash-size 1.2) (rehash-threshold 0.6407767)
				 (hash-function nil) (values t)
				 (weak-keys nil))
  (make-linked-hash-table
   :table
   #+allegro
   (make-hash-table :test test :size size :rehash-size rehash-size
		    :rehash-threshold rehash-threshold
		    :hash-function hash-function :values values
		    :weak-keys weak-keys)
   #+(or cmu sbcl)
   (if (memq test '(eq eql equal equalp))
       (make-hash-table :test test :size size :rehash-size rehash-size
			:rehash-threshold rehash-threshold
			:weak-p weak-keys)
       (make-pvs-hash-table
	:strong-eq? (eq test 'strong-tc-eq)
	:weak-p weak-keys
	:size size :rehash-size rehash-size
	:rehash-threshold rehash-threshold))))

(defun copy-lhash-table (lht &key (size 67) (rehash-size 1.2)
			     (rehash-threshold 0.6407767))
  (let ((ht (if (linked-hash-table-p lht)
		(lhash-table lht)
		lht)))
    (assert (hash-table-p ht))
    (make-linked-hash-table
     :table
     #+allegro
     (make-hash-table
      :test (hash-table-test ht)
      :size size
      :rehash-size rehash-size
      :rehash-threshold rehash-threshold
      :hash-function (excl:hash-table-hash-function ht)
      :values (excl:hash-table-values ht)
      :weak-keys (excl:hash-table-weak-keys ht))
     #-allegro
     (let* ((test (hash-table-test ht))
	    (newht
	     (if (memq test '(eq eql equal equalp))
		 (make-hash-table
		  :test test
		  :size size
		  :rehash-size rehash-size
		  :rehash-threshold rehash-threshold
		  :weak-p (lisp::hash-table-weak-p ht))
		 (make-pvs-hash-table :strong-eq? (eq test 'strong-tc-eq)
				      :weak-keys? (lisp::hash-table-weak-p ht)
				      :size size
				      :rehash-size rehash-size
				      :rehash-threshold rehash-threshold
				      :table (lisp::hash-table-table ht)))))
       (declare (inline maphash))
       (maphash #'(lambda (x y) (setf (gethash x newht) y))
		ht)
       newht)
     :next (if (linked-hash-table-p lht)
	       lht
	       (make-linked-hash-table
		:table ht)))))

(defun get-lhash (key lhashtable &optional default)
  (if (hash-table-p (lhash-table lhashtable))
      (multiple-value-bind (value there?)
	  (gethash key (lhash-table lhashtable))
	(if there?
	    (values value there?)
	    (if (lhash-next lhashtable)
		(get-lhash key (lhash-next lhashtable) default)
		(values default nil))))
      (if (lhash-next lhashtable)
	  (get-lhash key (lhash-next lhashtable) default)
	  (values default nil))))

(defsetf get-lhash (key lhashtable &optional default) (value)
  `(setf-get-lhash ,key ,lhashtable ,default ,value))

(defun setf-get-lhash (key lhashtable default value)
  (if (hash-table-p (lhash-table lhashtable))
      (setf (gethash key (lhash-table lhashtable) default) value)
      (let ((ht (funcall (lhash-table lhashtable))))
	(setf (lhash-table lhashtable) ht)
	(setf (gethash key ht default) value))))

;;; Similar to maphash, but makes sure not to revisit keys duplicated
;;; at lower levels.
(defvar *map-lhash-keys-visited*)

(defun map-lhash (function lhash)
  (let ((*map-lhash-keys-visited* nil))
    (map-lhash* function lhash)))

(defun map-lhash* (function lhash)
  (let ((ht (lhash-table lhash)))
    (when (hash-table-p ht)
      (maphash #'(lambda (x y)
		   (unless (member x *map-lhash-keys-visited*
				   :test (hash-table-test ht))
		     (when (lhash-next lhash)
		       (push x *map-lhash-keys-visited*))
		     (funcall function x y)))
	       (lhash-table lhash)))
    (when (lhash-next lhash)
      (map-lhash* function (lhash-next lhash)))))
