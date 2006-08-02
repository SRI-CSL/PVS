;;;
;;; pvs-macros.el   dave_sc, 12/3/98
;;;
;;; Defines macros which are used in other pvs emacs files.
;;; These are defined here so they are available in byte compilation
;;;

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

(require 'cl)

;;;
;;; originally in pvs-load.el
;;;
(if (not (fboundp 'save-match-data))
    (defmacro save-match-data (&rest body)
  "Execute the BODY forms, restoring the global value of the match data."
  (let ((original (make-symbol "match-data")))
    (list 'let (list (list original '(match-data)))
	  (list 'unwind-protect
		(cons 'progn body)
		(list 'store-match-data original))))))

(defmacro defpvs (name class arglist docstring &rest body)
  "(defpvs NAME CLASS ARGLIST DOCSTRING BODY...):
define NAME as a PVS command."
  (list 'progn
	(list 'put (list 'quote name) ''pvs-command (list 'quote class))
	(cons 'defun (cons name (cons arglist (cons docstring body))))))


;; This is courtesy of Jerry James.
(when (string-match "XEmacs" (emacs-version))
    
(defun with-timeout-handler (tag)
  (throw tag 'timeout))

(defmacro with-timeout (list &rest body)
  "Run BODY, but if it doesn't finish in SECONDS seconds, give up.
If we give up, we run the TIMEOUT-FORMS and return the value of the last one.
The call should look like:
 (with-timeout (SECONDS TIMEOUT-FORMS...) BODY...)
The timeout is checked whenever Emacs waits for some kind of external
event \(such as keyboard input, input from subprocesses, or a certain time);
if the program loops without waiting in any way, the timeout will not
be detected."
  (let ((seconds (car list))
	(timeout-forms (cdr list)))
    `(let ((with-timeout-tag (cons nil nil))
	   with-timeout-value with-timeout-timer)
      (if (catch with-timeout-tag
	    (progn
	      (setq with-timeout-timer
		    (start-itimer "internal-itimer" #'with-timeout-handler
				  ,seconds nil t t with-timeout-tag))
	      (setq with-timeout-value (progn ,@body))
	      nil))
	  (progn ,@timeout-forms)
	(delete-itimer with-timeout-timer)
	with-timeout-value))))
)

(provide 'pvs-macros)



