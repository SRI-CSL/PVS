;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-parse-fixes.lisp -- Modifications to functions produced by Ergo
;; Author          : Sam Owre
;; Created On      : Sat May 27 18:11:09 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Sat May 27 18:14:12 1995
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

;;; The following two functions allow things like {x:[t,t]|p} to be
;;; correctly parsed, by only allowing ]| when |[ has been seen.

(defvar *table-bracket-counter* 0)

(defun lex-\|
       (stream symbol)
       (declare (ignore symbol))
       (let (holdchar)
         (setf holdchar
               (lexical-read-char stream :eof))
         (if (and pvs-escape-char
                  (eql holdchar pvs-escape-char))
             (setf holdchar
                   (lexical-read-char stream :eof)))
         (cond ((eql holdchar #\-)
                (setf holdchar
                      (lexical-read-char stream :eof))
                (if (and pvs-escape-char
                         (eql holdchar pvs-escape-char))
                    (setf holdchar
                          (lexical-read-char stream :eof)))
                (cond ((eql holdchar #\>) 'sbst::\|->)
                      (t (lexical-unread-char stream) 'sbst::\|-)))
               ((eql holdchar #\]) 'sbst::\|])
               ((eql holdchar #\[)
		(incf *table-bracket-counter*)
		'sbst::\|[)
               ((eql holdchar #\|) 'sbst::\|\|)
               ((eql holdchar #\=) 'sbst::\|=)
               ((eql holdchar #\>) 'sbst::\|>)
               (t (lexical-unread-char stream) 'sbst::\|))))

(defun lex-]
       (stream symbol)
       (declare (ignore symbol))
       (let (holdchar)
         (setf holdchar
               (lexical-read-char stream :eof))
         (if (and pvs-escape-char
                  (eql holdchar pvs-escape-char))
             (setf holdchar
                   (lexical-read-char stream :eof)))
         (cond ((and (eql holdchar #\|)
		     (plusp *table-bracket-counter*))
		(decf *table-bracket-counter*)
		'sbst::]\|)
               (t (lexical-unread-char stream) 'sbst::]))))
