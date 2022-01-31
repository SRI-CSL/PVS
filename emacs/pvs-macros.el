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

(require 'cl-lib)

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

(defmacro defpvs (name menupath arglist docstring &rest body)
  "(defpvs NAME MENUPATH ARGLIST DOCSTRING BODY...):
define NAME as a PVS command."
  (declare (indent defun))
  `(progn
     (put ',name 'pvs-command ',menupath)
     (defun ,name ,arglist ,docstring ,@body)))

;; defun (e.g., (4 &lambda &body) ) doesn't work, as defpvs has an extra argument.
(put 'defpvs 'lisp-indent-function '(4 0 &lambda &body))


;; This is courtesy of Jerry James.
(when (featurep 'xemacs)

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

(cl-defstruct (pvs-formula-reference (:conc-name pvs-fref-))
  "Created by pvs-formula-origin"
  kind    ; symbol: pvs, prelude, prelude-theory, proof-status, tccs, ppe
  formula ; string: name of formula (or decl)
  theory  ; string: name of theory
  file    ; string: pvs file name (no extension)
  library ; string: pvs directory when a library theory
  buffer  ; buffer name
  line    ; current line number in buffer
  (prelude-offset 0) ; line offset within the prelude
                     ; (0 unless kind is prelude-theory)
  )

(if (not (fboundp 'setq-local))
(defmacro setq-local (var val)
  "Set variable VAR to value VAL in current buffer."
  ;; Can't use backquote here, it's too early in the bootstrap.
  (list 'set (list 'make-local-variable (list 'quote var)) val)))

(provide 'pvs-macros)
