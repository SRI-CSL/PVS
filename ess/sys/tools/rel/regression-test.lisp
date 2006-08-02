;;; -*- package: ergo-lisp -*-
#-gcl
(defpackage :ergo-lisp #+sbcl (:use :common-lisp))
(in-package :ergo-lisp)
(export '(regression-test regression-test-only regression-test-opt
			  next-key next-value *regression-testing-p*
			  move-script regressible-error regressible-warn))

;;; Puzzle for the devoted:  How can I rewrite regression-test so that
;;; it can regression-test itself?  I would like to be able to
;;; automatically run the test cases commented out below at load time
;;; and automatically verify that they generate the correct output.

#-sbcl (use-package '(:ergo-lisp) :lisp)
#-sbcl (export '(regression-test regression-test-only regression-test-opt
				 regressible-error regressible-warn)
	       (find-package :lisp))

(defvar *regression-testing-p* nil)

(defun regression-test (&key (name "Anonymous test")
			     form (form-predicate #'identity) script endp)
  (declare (special name script endp))
  (let ((*regression-testing-p* t))
    (catch 'script-ended
      (let ((formval (if (functionp form) (funcall form) (eval form))))
	(unless
	    (funcall form-predicate formval)
	  (error "In the regression test named ~s, the form returned ~s."
		 name formval)))))
  (when script
      (error "In the regression test named ~s, there was leftover script:~%~
              ~s" name script))
  (format t "Passed regression test ~s.~%" name)
  (values))

(defun next-key ()
  (declare (special script))
  (caar script))

(defun next-value ()
  (declare (special script))
  (cadr (car script)))

(defun move-script ()
  (declare (special name script endp))
  (when (null script)
    (if endp
	(throw 'script-ended nil)
	(error "Regression test ~s fell off of end of script." name)))
  (pop script)
  (when (and (null script) endp) (throw 'script-ended nil))
  (values))

(defmacro regression-test-only (key &body body)
  (let ((keysym (gensym)))
    `(let ((,keysym ,key))
       (if *regression-testing-p*
	   (if (equal (next-key) ,keysym)
	       (prog1 (next-value) (move-script))
	       (error "In regression-test-only for test named ~s,~%~
                   expected script key was ~s, real script key was ~s.~%"
		      name ,keysym (next-key)))
	   (progn . ,body)))))

(defmacro regression-test-opt (key &body body)
  `(if (and *regression-testing-p* (equal (next-key) ,key))
       (prog1 (next-value) (move-script))
       (progn . ,body)))
       
(labels
    ((general-regressible-problem (key string format-args namestring fn)
       (declare (special name))
       (if *regression-testing-p*
	   (if (equal (next-key) key)
	       (let ((expected (next-value))
		     (true (apply #'format nil string format-args)))
		 (unless (equal expected true)
		   (error
		    "In ~a for test ~s,~%~
                     true string was:     ~s~%~
                     expected string was: ~s~%"
		    namestring name true expected))
		 (move-script))
	       (apply fn string format-args))
	   (apply fn string format-args))))
  (defun regressible-error (key string &rest format-args)
    (general-regressible-problem
     key string format-args "regressible-error" #'error))
  (defun regressible-warn (key string &rest format-args)
    (general-regressible-problem
     key string format-args "regressible-warn" #'warn)))
	  
#+regression
(progn
  (defun foo (x)
    (regression-test-opt
     :foo
     (bar x)))
  (defun bar (x)
    (cons
     (regression-test-only
      :readline
      (format t "What should bar return?")
      (read-line))
     x))
  (regression-test
   :name "Test should pass 1"
   :form '(equal (foo 3) 4)
   :script `((:foo 4)))
  (regression-test
   :name "Test should pass 2"
   :form #'(lambda () (equal (foo 3) '(7 . 3)))
   :script '((:readline 7)))
  (defun baz (x)
    (regression-test-opt
     :baz
     3))
  (regression-test
   :name "Regression-test-opt is okay at end of script."
   :form '(baz 3)
   :form-predicate #'(lambda (x) (equal 3 x))
   :script '()))

#|
(foo 3)

(regression-test
 :name "Predicate should fail"
 :form #'(lambda () (equal (foo 3) 5))
 :script `((:foo 4)))

(regression-test
 :name "Script should be bad"
 :form '(foo 3)
 :script '((:bozzo 4)))

(regression-test
   :name "Script is too long."
   :form '(equal (foo 3) 4)
   :script `((:foo 4)
	     (:bozzo 17)))

(regressible-error :test "Error message ~s." 'foo)

(regression-test
 :name "Regressible-error script too short."
 :form '(regressible-error (regressible-error :test "Error message ~s." 'foo))
 :script '())

(regression-test
 :name "Regressible-error wrong key."
 :form '(regressible-error (regressible-error :test "Error message ~s." 'foo))
 :script '((:foo 3)))

(regression-test
 :name "Regressible-error wrong string."
 :form '(regressible-error (regressible-error :test "Error message ~s." 'foo))
 :script '((:test "hi")))
|#

#+regression
(regression-test
 :name "Regressible-error should succeed."
 :form '(regressible-error :test "Error message ~s." 'foo)
 :script '((:test "Error message FOO."))
 :endp t)

#+regression
(regression-test
 :name "Regressible-warn should succeed."
 :form '(progn (regressible-warn :test "Error message ~s." 'foo) t)
 :script '((:test "Error message FOO.")))
