;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-parse-fixes.lisp -- Modifications to functions produced by Ergo
;; Author          : Sam Owre
;; Created On      : Sat May 27 18:11:09 1995
;; Last Modified By: Sam Owre
;; Last Modified On: Sat May 27 18:14:12 1995
;; Update Count    : 1
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

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

;;; Need to deal with the situation where '}}' is really two tokens,
;;; e.g., "{ts1: set[T] | ts1 = {t: T | ts(t)}}"

(defvar *double-braces-counter* 0)

(defun LEX-{ (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((eql holdchar #\:) 'SBST::|{:|)
          ((eql holdchar #\{)
	   (incf *double-braces-counter*)
	   'SBST::{{)
          ((eql holdchar #\|)
           (setf holdchar (lexical-read-char stream :eof))
           (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
               (setf holdchar (lexical-read-char stream :eof)))
           (cond ((eql holdchar #\|)
                  (setf holdchar (lexical-read-char stream :eof))
                  (if (and PVS-ESCAPE-CHAR
                           (eql holdchar PVS-ESCAPE-CHAR))
                      (setf holdchar (lexical-read-char stream :eof)))
                  (cond ((eql holdchar #\}) 'SBST::{\|\|})
                        (t
                         (lexical-unread-char stream)
                         (illegal-token-error "{||")
                         :illegal-token)))
                 (t (lexical-unread-char stream) 'SBST::{\|)))
          (t (lexical-unread-char stream) 'SBST::{))))

(defun LEX-} (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and PVS-ESCAPE-CHAR (eql holdchar PVS-ESCAPE-CHAR))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((and (eql holdchar #\})
		(plusp *double-braces-counter*))
	   (decf *double-braces-counter*)
	   'SBST::}})
          (t (lexical-unread-char stream) 'SBST::}))))
