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

(DEFUN LEX-\|
       (STREAM SYMBOL)
       (DECLARE (IGNORE SYMBOL))
       (LET (HOLDCHAR)
         (SETF HOLDCHAR
               (LEXICAL-READ-CHAR STREAM :EOF))
         (IF (AND PVS-ESCAPE-CHAR
                  (EQL HOLDCHAR PVS-ESCAPE-CHAR))
             (SETF HOLDCHAR
                   (LEXICAL-READ-CHAR STREAM :EOF)))
         (COND ((EQL HOLDCHAR #\-)
                (SETF HOLDCHAR
                      (LEXICAL-READ-CHAR STREAM :EOF))
                (IF (AND PVS-ESCAPE-CHAR
                         (EQL HOLDCHAR PVS-ESCAPE-CHAR))
                    (SETF HOLDCHAR
                          (LEXICAL-READ-CHAR STREAM :EOF)))
                (COND ((EQL HOLDCHAR #\>) 'SBST::\|->)
                      (T (LEXICAL-UNREAD-CHAR STREAM) 'SBST::\|-)))
               ((EQL HOLDCHAR #\]) 'SBST::\|])
               ((EQL HOLDCHAR #\[)
		(incf *table-bracket-counter*)
		'SBST::\|[)
               ((EQL HOLDCHAR #\|) 'SBST::\|\|)
               ((EQL HOLDCHAR #\=) 'SBST::\|=)
               ((EQL HOLDCHAR #\>) 'SBST::\|>)
               (T (LEXICAL-UNREAD-CHAR STREAM) 'SBST::\|))))

(DEFUN LEX-]
       (STREAM SYMBOL)
       (DECLARE (IGNORE SYMBOL))
       (LET (HOLDCHAR)
         (SETF HOLDCHAR
               (LEXICAL-READ-CHAR STREAM :EOF))
         (IF (AND PVS-ESCAPE-CHAR
                  (EQL HOLDCHAR PVS-ESCAPE-CHAR))
             (SETF HOLDCHAR
                   (LEXICAL-READ-CHAR STREAM :EOF)))
         (COND ((and (EQL HOLDCHAR #\|)
		     (plusp *table-bracket-counter*))
		(decf *table-bracket-counter*)
		'SBST::]\|)
               (T (LEXICAL-UNREAD-CHAR STREAM) 'SBST::]))))
