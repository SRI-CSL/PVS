;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Strings for user interaction **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         24-Sept-2008,
;;;            last updated on  23-Oct-2008.
;;;

(in-package :rahd)

;;;
;;; TERMINAL-WIDTH: The number of characters to print per line to the user.
;;;

(defparameter *terminal-width* 80)

;;;
;;; ERROR-STRINGS: Hash table for error strings.
;;;

(defparameter *error-strings* (make-hash-table :test 'equal))

;;;
;;; FMT verbosity-level str params.
;;;

(defun fmt (verbosity-level &rest rst)
  (when (>= *rahd-verbosity* verbosity-level)
    (apply #'format (append '(t) rst))))

;;;
;;; The actual error strings.
;;;

(setf (gethash 'g-abandon *error-strings*)
      "An attempt was made to install a new top-level goal during the progress of an already
existing RAHD proof session.  If you really mean to do this, which will abandon the
current proof session, wipe the session memory, and start a new proof session, call (G ...)
as follows: ~%
(G <cnf-formula> 0 :ABANDON-OK t).")

(setf (gethash 'g-overwrite *error-strings*)
      "An attempt was made to install a new goal that already exists by name on the goal-stack.
The goal in question has key ~A.  If you really mean to do this, which will cause the afore-
mentioned goal to have its data overwritten (and lead to possible unsoundness), then call
(G ...) with the final keyword parameter :OVERWRITE-OK t.")

(setf (gethash 'g-no-goal-to-load *error-strings*)
      "An attempt was made to load a goal that does not exist.
~%Goal queried: ~A
Available goals: ~A.")

(setf (gethash 'g-empty-goal *error-strings*)
      "An attempt was made to call (G ...) with no goal formula.")

;;;
;;; WORD-WRAP-FMT-CMD: A command to cause word wrapping to take place, parameterized
;;; by *terminal-width*.  I learned the static version of this fmt command sequence 
;;; from Gene Stover's wonderful ``format recipes.''
;;;

(defun word-wrap-fmt-string ()
  (format nil "~A~A~A"
	  "~{~<~%~1," *terminal-width* ":;~A~> ~}"))

;;;
;;; ERROR-STRING: Strings displayed during system errors.
;;;

(defun error-string (id &optional params)
  (eval (append `(format nil ,(gethash id *error-strings*))
		params)))

(defun error-string* (id &optional params)
  (eval (append `(format 
		  nil 
		  ,(eval (append `(format nil ,(word-wrap-fmt-string))
				 `(',(make-word-lst (gethash id *error-strings*))))))
		params)))

;;;
;;; MAKE-WORD-lst: Take a string and explode it into a list of words, with delimitations
;;; drawn via spaces and newlines.
;;;

(defun make-word-lst (s)
  (let ((s-remaining s)
	(s-word-lst nil))
    (while s-remaining
      (let ((spc (omin (position #\Space s-remaining :start 0)
		       (position #\Newline s-remaining :start 0))))
	(let ((next-word (subseq s-remaining 0 (or spc (length s-remaining)))))
	  (setq s-word-lst (cons next-word s-word-lst))
	  (setq s-remaining 
		(if spc (subseq s-remaining (1+ spc) (length s-remaining))
		  nil)))))
  (reverse s-word-lst)))

(defun omin (x y)
  (let ((x? (numberp x)) (y? (numberp y)))
    (cond ((and x? y?) (min x y))
	  (x? x)
	  (y? y)
	  (t nil))))

;;;
;;; FORMAT-GOAL-KEY: Given a goal key, return its formatted string representation.
;;;

(defun format-goal-key (k)
  (if (listp k)
      (format nil "~{~a~^.~}" k)
    (write-to-string k)))

;;;
;;; WORK-PATHIFY (f-str): Given a file string, return the result of placing it in the
;;; work path.  f-str is assumed to have trailing `/'.
;;;

(defun work-pathify (f-str)
  (format nil "~A~A" *rahd-work-path* f-str))


