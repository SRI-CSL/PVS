;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; restore-theories.lisp -- 
;; Author          : Sam Owre
;; Created On      : Fri Oct 30 11:33:43 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:33:59 1998
;; Update Count    : 1
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

;;; Restores the state of the system from information provided in the
;;; context.  The context has the form
;;;   (version (mod1 src-date1 obj-date1) ... )

(defun restore-theories (context)
  (mapc #'restore-theory (cdr context)))

(defun restore-theory (entry)
  (let ((mod (restore-parsed (first entry) (second entry) (fourth entry))))
    (when (and mod (member 'typechecked (fourth entry)))
      (restore-typechecked mod))))

(defun restore-parsed (mod-id src-date status)
  (when (member 'parsed status)
    (let ((modfile (make-specpath mod-id)))
      (cond ((not (probe-file modfile))
	     (pvs-message
	      "File ~a no longer exists - deleting theory from context"
	      (truename modfile)))
	    ((null src-date))
	    ((not (= src-date (file-write-date modfile)))
	     (pvs-message
	      "File ~a was modified since last parsed - status set to unparsed"
	      (truename modfile)))
	    (t (parse-theory mod-id))))))

(defun restore-typechecked (mod)
  (let ((type-info (with-open-file (str (make-objpath (id mod))) (read))))
    (restore mod type-info))
  (typecheck-theory mod))

(defmethod restore ((mod module) type-info)
  (dolist (info type-info)
    (restore (restored-part mod (car info)) (cdr info))))
