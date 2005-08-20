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

(defun LEX-\|
       (stream symbol)
       (declare (ignore symbol))
       (let (holdchar)
         (setf holdchar
               (lexical-read-char stream :eof))
         (if (and PVS-ESCAPE-CHAR
                  (eql holdchar PVS-ESCAPE-CHAR))
             (setf holdchar
                   (lexical-read-char stream :eof)))
         (cond ((eql holdchar #\-)
                (setf holdchar
                      (lexical-read-char stream :eof))
                (if (and PVS-ESCAPE-CHAR
                         (eql holdchar PVS-ESCAPE-CHAR))
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
	       ((eql holdchar #\)) 'sbst::\|\))
	       ((eql holdchar #\}) 'sbst::\|\})
               (t (lexical-unread-char stream) 'sbst::\|))))

(defun LEX-]
       (stream symbol)
       (declare (ignore symbol))
       (let (holdchar)
         (setf holdchar
               (lexical-read-char stream :eof))
         (if (and PVS-ESCAPE-CHAR
                  (eql holdchar PVS-ESCAPE-CHAR))
             (setf holdchar
                   (lexical-read-char stream :eof)))
         (cond ((and (eql holdchar #\|)
		     (plusp *table-bracket-counter*))
		(decf *table-bracket-counter*)
		'sbst::]\|)
               (t (lexical-unread-char stream) 'sbst::]))))

;;; This one allows :- to be treated as two tokens instead of one.
;;; Got this way when ':->' was added to the grammar.

(defvar sbrt::*holding-char* nil)

(defun |LEX-:| (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((eql holdchar #\=) 'SBST::|:=|)
          ((eql holdchar #\:)
           (setf holdchar (lexical-read-char stream :eof))
           (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
               (setf holdchar (lexical-read-char stream :eof)))
           (cond ((eql holdchar #\=) 'SBST::|::=|)
                 (t (lexical-unread-char stream) 'SBST::|::|)))
          ((eql holdchar #\)) 'SBST::|:)|)
          ((eql holdchar #\}) 'SBST::|:}|)
          ((eql holdchar #\-)
           (setf holdchar (lexical-read-char stream :eof))
           (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
               (setf holdchar (lexical-read-char stream :eof)))
           (cond ((eql holdchar #\>) 'SBST::|:->|)
                 (t
                  (lexical-unread-char stream)
		  (setq sbrt::*holding-char* #\-)
                  'SBST::|:|)))
          (t (lexical-unread-char stream) 'SBST::|:|))))
