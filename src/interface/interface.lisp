;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interface.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Jun  6 11:44:00 2012
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Jun  6 12:42:40 2012
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The generic interface to the PVS lisp image; currently supports Emacs
;;; and JSON interfaces.  See pvs-emacs.lisp and pvs-json.lisp

(defvar *pvs-interface* nil)
(defvar *pvs-buffer-hooks* nil)
(defvar *pvs-message-hooks* nil)
(defvar *pvs-error-hooks* nil)

(defmethod prover-read :around ()
  (if *pvs-interface*
      (prover-read* *pvs-interface*)
      (call-next-method)))

(defmethod output-proofstate :around (proofstate)
  (call-next-method))

(defmethod output-proofstate* ((ifc null) proofstate)
  (output-proofstate proofstate))
