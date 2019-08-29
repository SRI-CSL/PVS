;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-json-methods.lisp -- provides json-rpc methods for PVS
;; Author          : Sam Owre
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013 SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

(in-package :pvs-json)

;;; The requests

(defrequest list-methods ()
  "List all available methods"
  (sort (mapcar #'car *pvs-request-methods*) #'string-lessp))

(defrequest list-client-methods ()
  "List methods clients need to support"
  (list "info" "warning" "debug" "buffer" "yes-no" "dialog"))

(defrequest help (methodname)
  "Get help for the specified methodname -
   provides the docstring and the argument spec"
  (multiple-value-bind (reqfun argspec docstring)
      (get-json-request-function methodname)
    (if reqfun
	`((:argspec ,argspec)
	  (:docstring . ,docstring))
	(error "~a not found" methodname))))

(defrequest lisp (string)
  "Just evaluate the string in lisp"
  (let ((*package* (find-package :pvs)))
    (format nil "~a" (eval (read-from-string string)))))

(defrequest change-workspace (dir)
  "Change PVS workspace"
  (pvs:change-workspace dir))

(defrequest change-context (dir)
  "Change PVS workspace"
  (pvs:change-workspace dir))

(defrequest parse (filename)
  "Parse a file"
  (let ((theories (pvs:parse-file filename)))
    (pvs:save-context)
    (xmlrpc-theories theories)))

(defrequest typecheck (filename)
  "Typecheck a file"
  (let ((theories (pvs:typecheck-file filename)))
    (pvs:save-context)
    (xmlrpc-theories theories)))

(defstruct theory-struct
  id
  decls)

(defun xmlrpc-theories (theories &optional thstructs)
  (if (null theories)
      (nreverse thstructs)
      (let ((thstruct (xmlrpc-theory (car theories))))
	(xmlrpc-theories (cdr theories) (cons thstruct thstructs)))))

(defun xmlrpc-theory (theory)
  (make-theory-struct
   :id (pvs:id theory)
   :decls (xmlrpc-theory-decls (pvs:all-decls theory))))

(defmethod json:encode-json ((ts theory-struct) &optional (stream json:*json-output*))
  (json:with-object (stream)
    (json:as-object-member (:id stream)
      (json:encode-json (theory-struct-id ts) stream))
    (json:as-object-member (:decls stream)
      (json:encode-json (theory-struct-decls ts) stream))))

(defun xmlrpc-theory-decls (decls &optional thdecls)
  (if (null decls)
      (nreverse thdecls)
      (let ((thdecl (xmlrpc-theory-decl (car decls))))
	(xmlrpc-theory-decls
	 (cdr decls)
	 (if thdecl
	     (cons thdecl thdecls)
	     thdecls)))))

(defvar *idgensymctr*)

(defun xmlrpc-theory-decl (decl)
  (let ((*idgensymctr* nil))
    (xmlrpc-theory-decl* decl)))

(defmethod xmlrpc-theory-decl* ((decl pvs:var-decl))
  nil)

(defmethod xmlrpc-theory-decl* ((imp pvs:importing))
  `((:importing . ,(pvs:str (pvs:theory-name imp)))
    (:kind . :importing)
    (:place . ,(pvs:place-list imp))))

(defmethod xmlrpc-theory-decl* ((decl pvs:typed-declaration))
  (let ((id (or (pvs:id decl)
		(gen-id (pvs:kind-of decl)))))
    `((:id . ,id)
      (:kind . ,(pvs:kind-of decl))
      (:type . ,(pvs:str (pvs:type decl)))
      (:place . ,(pvs:place-list decl)))))

(defmethod xmlrpc-theory-decl* ((decl pvs:declaration))
  (let ((id (or (pvs:id decl)
		(gen-id (pvs:kind-of decl)))))
    `((:id . ,id)
      (:kind . ,(pvs:kind-of decl))
      (:place . ,(pvs:place-list decl)))))
  
(defun gen-id (kind)
  (let ((num (gen-id-number kind)))
    (format nil "~a_~d" kind num)))

(defun gen-id-number (kind)
  (let ((entry (assq kind *idgensymctr*)))
    (cond (entry
	   (incf (cdr entry)))
	  (t (push (cons kind 0) *idgensymctr*)
	     0))))

(defmethod xmlrpc-theory-decl* ((decl pvs:formula-decl))
  (let* ((proved? (not (null (pvs:proved? decl))))
	 (complete? (and proved?
			 (string= (pvs:pc-complete decl)
				  "complete")))
	 (has-proofscript? (not (null (pvs:justification decl)))))
    `((:id . ,(pvs:id decl))
      (:kind . :formula)
      (:proved? . ,proved?)
      (:complete? . ,complete?)
      (:has-proofscript? . ,has-proofscript?)
      (:place . ,(pvs:place-list decl)))))

;;; Info

(defrequest names-info (filename)
  "Get the names-tooltip information for a PVS file"
  (assert (stringp filename) () "Filename must be a string: ~a" filename)
  (when (pvs:typecheck-file filename)
    (pvs:collect-pvs-file-decls-info filename)))

(defrequest find-declaration (id)
  "Searches for all declarations with the given id"
  (pvs:find-declaration id))

(defrequest reset ()
  "Resets PVS"
  (let ((proc (mp:process-name-to-process "Initial Lisp Listener")))
    (mp:process-interrupt proc #'pvs:pvs-abort)))

(defrequest interrupt ()
  "Interrupts PVS."
  (let ((proc (mp:process-name-to-process "Initial Lisp Listener")))
    (mp:process-interrupt proc #'(lambda () (break "Interrupt:")))))

;;; Prover interface

;; (defrequest prove-formula (formula theory)
;;   "Starts interactive proof of a formula from a given theory"
;;   (let* ((gate (mp:make-gate nil))
;; 	 (lock (mp:make-process-lock)))
;;     (setq pvs:*ps-control-info*
;; 	  (pvs:make-psinfo
;; 	   :lock lock
;; 	   :gate gate
;; 	   :process (mp:process-run-function
;; 		     "PVS Prover"
;; 		     #'pvs:prove-formula theory formula nil)))
;;     (format t "~%prove-formula: Waiting...~%")
;;     (mp:process-wait "Waiting for initial Proofstate" #'mp:gate-open-p gate)
;;     (format t "~%prove-formula: Done waiting...~%")
;;     (mp:with-process-lock (lock)
;;       (let ((psjson (pvs:psinfo-psjson pvs:*ps-control-info*)))
;; 	(setf (pvs:psinfo-psjson pvs:*ps-control-info*) nil)
;; 	psjson))))

(defrequest prove-formula (formula theory)
  "Starts interactive proof of a formula from a given theory

Creates a ps-control-info struct to control the interaction.  It has slots
  command
  json-result
  lock
  cmd-gate
  res-gate
"
  (let ((res-gate (mp:make-gate nil))
	(cmd-gate (mp:make-gate nil))
	(lock (mp:make-process-lock))
	(proc (mp:process-name-to-process "Initial Lisp Listener")))
    (setq pvs:*ps-control-info*
	  (pvs:make-ps-control-info
	   :lock lock
	   :res-gate res-gate
	   :cmd-gate cmd-gate))
    ;;(format t "~%prove-formula: Waiting...~%")
    ;; (mp:process-interrupt proc #'pvs:prove-formula theory formula nil)
    (mp:process-interrupt proc #'pvs:prove-formula theory formula nil)
    (mp:process-wait "Waiting for initial Proofstate" #'mp:gate-open-p res-gate)
    ;;(format t "~%prove-formula: Done waiting...~%")
    (mp:with-process-lock (lock)
      (let ((json-result (pvs:psinfo-json-result pvs:*ps-control-info*)))
	;;(format t "~%prove-formula: returning json-result ~a~%" json-result)
	(setf (pvs:psinfo-json-result pvs:*ps-control-info*) nil)
	(mp:close-gate (pvs:psinfo-res-gate pvs:*ps-control-info*))
	json-result))))

(defrequest proof-command (form)
  "Sends a command to the prover"
  (assert (not (mp:gate-open-p (pvs:psinfo-cmd-gate pvs:*ps-control-info*))))
  (assert (null (pvs:psinfo-command pvs:*ps-control-info*)))
  (assert (not (mp:gate-open-p (pvs:psinfo-res-gate pvs:*ps-control-info*))))
  (assert (null (pvs:psinfo-json-result pvs:*ps-control-info*)))
  (setf (pvs:psinfo-command pvs:*ps-control-info*) form)
  (mp:open-gate (pvs:psinfo-cmd-gate pvs:*ps-control-info*))
  (mp:process-wait "Waiting for next Proofstate"
		   #'mp:gate-open-p (pvs:psinfo-res-gate pvs:*ps-control-info*))
  (assert (not (mp:gate-open-p (pvs:psinfo-cmd-gate pvs:*ps-control-info*))))
  (assert (null (pvs:psinfo-command pvs:*ps-control-info*)))
  (mp:with-process-lock ((pvs:psinfo-lock pvs:*ps-control-info*))
    (let ((psjson (pvs:psinfo-json-result pvs:*ps-control-info*)))
      (setf (pvs:psinfo-json-result pvs:*ps-control-info*) nil)
      (mp:close-gate (pvs:psinfo-res-gate pvs:*ps-control-info*))
      psjson)))
