;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-json-rpc.lisp -- support for json-rpc server in PVS
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

(defpackage :pvs-json (:use :json :cl-user :common-lisp :pvs))

(in-package :pvs-json)

(export '(process-json-request json-message *interrupted-rpc* update-ps-control-info-result))

(define-condition pvs-error (simple-error)
  ((code :accessor code :initarg :code)
   (kind :accessor kind :initarg :kind)
   (error-string :accessor error-string :initarg :error-string)
   (file-name :accessor file-name :initarg :file-name)
   (place :accessor place :initarg :place)))

(defmethod print-object ((err pvs-error) stream)
  (format stream "#<pvs-error ~a>" (error-string err)))

;;; With-pvs-hooks sets up callbacks when the url is provided
;;; Used primarily for informative messages that are not part of the result
;;; E.g., "foo typechecked in 3 sec", garbage collecting, ...
;;; But also needed for pvs-y-or-n and pvs-dialog

;;; Note that we can't do a let on, e.g., *pvs-message-hook* directly, as
;;; that would bind it in the current XML-RPC process, and the main lisp
;;; process would not see the values.

(defmacro with-pvs-hooks (url &rest body)
  (let ((gurl (gentemp)))
    `(let ((,gurl ,url)
	   (error-hook pvs:*pvs-error-hook*)
	   (message-hook pvs:*pvs-message-hook*)
	   (warning-hook pvs:*pvs-warning-hook*)
	   (buffer-hook pvs:*pvs-buffer-hook*)
	   (y-or-n-hook pvs:*pvs-y-or-n-hook*)
	   (query-hook pvs:*pvs-query-hook*)
	   (dialog-hook pvs:*pvs-dialog-hook*))
       (unwind-protect
	    (progn
	      ;; Error-hook is different, as it is the end result
	      ;; url is not needed
	      (setf pvs:*pvs-error-hook* #'json-make-pvs-error)
	      (when ,gurl
		(setf pvs:*pvs-message-hook*
		      #'(lambda (msg) (json-message msg ,gurl))
		      pvs:*pvs-warning-hook*
		      #'(lambda (msg) (json-message msg ,gurl "warning"))
		      pvs:*pvs-buffer-hook*
		      #'(lambda (name contents display? read-only? append? kind)
			  (json-buffer name contents display? read-only?
				       append? kind ,gurl))
		      pvs:*pvs-y-or-n-hook*
		      #'(lambda (msg full? timeout?)
			  ;;(json-y-or-n msg full? timeout? ,gurl)
			  (declare (ignore msg full? timeout?))
			  t)
		      pvs:*pvs-query-hook*
		      #'(lambda (prompt)
			  (json-query prompt ,gurl))
		      pvs:*pvs-dialog-hook*
		      #'(lambda (prompt)
			  (json-dialog prompt ,gurl))))
	      ,@body)
	 (setf pvs:*pvs-error-hook* error-hook
	       pvs:*pvs-message-hook* message-hook
	       pvs:*pvs-warning-hook* warning-hook
	       pvs:*pvs-buffer-hook* buffer-hook
	       pvs:*pvs-y-or-n-hook* y-or-n-hook
	       pvs:*pvs-query-hook* query-hook
	       pvs:*pvs-dialog-hook* dialog-hook)))))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :xml-rpc)
  (use-package :net.xml-rpc)
  (use-package :net.aserve)
  )

;; Requires cl-json

(defvar *last-request* nil)
(defvar *last-response* nil)
(defvar *json-rpc-id-ctr* 0)

(defvar *interrupted-rpc* nil "Indicates if the last request was interrupted by the user of the system (signals).") ;; M3 [Sept 2020]

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *pvs-request-methods* nil
    "An assoc-list with entries (method fun sig ret descr)"))

(defmacro defrequest (methodname args docstring &rest body)
  "Creates a JSON-RPC request form"
  (assert (symbolp methodname) () "defrequest method must be a symbol")
  (assert (listp args) () "defrequest args must be a list")
  (assert (stringp docstring) () "defrequest docstring must be a string")
  (let* ((pname (intern (format nil "jsonrpc-~a" methodname) :pvs-json)))
    `(progn
       (let ((req (assoc ',methodname *pvs-request-methods*)))
	 (format t "~%Adding ~a" ',methodname)
	 (when req (setq *pvs-request-methods*
			 (remove req *pvs-request-methods*))))
       (setq *pvs-request-methods*
	     (acons ',methodname (list ',pname ',args ,docstring)
		    *pvs-request-methods*))
       (defun ,pname ,args
	 ,docstring
	 ,@body))))

(defun get-json-request-function (method)
  (let ((entry (assoc method *pvs-request-methods* :test #'string-equal)))
    (if entry
	(values-list (cdr entry))
	(error "Method ~a not found" method))))

;;; Called from pvs-xml-rpc:xmlrpc-pvs-request

;;; If pvs-error is invoked, it calls json-make-pvs-error from
;;; *pvs-error-hook*, which creates a pvs-error condition, invoking
;;; json-pvs-error, which in turn builds the jsonrpc error object.
(defun process-json-request (method params id url)
  (if id
      (handler-case (process-json-request* method params id url)
	(pvs-error (c) (json-pvs-error id c))
	(error (c) (jsonrpc-error id c)))
      (process-json-request* method params id url)))

(defun no-url? (url)
  (member url '("" "nil" "none")
	  :test #'string-equal)) ; case-insensitive

(defun process-json-request* (method params id url)
  (unless (stringp method)
    (error "method ~a must be a symbol string" method))
  (unless (listp params)
    (error "parameters ~a must be a list" params))
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function method)
    (check-params params reqsig)
    (setq *last-request* (cons reqfun params))
    (setq *last-response* nil)
    (let ((result
	   (if (no-url? url)
	       (apply reqfun params)
	       (with-pvs-hooks url
		 (apply reqfun params)))))
      (setq *last-response* result)
      (jsonrpc-result result id))))

(defun check-params (params signature)
  (declare (ignore params signature))
  ;;; for now, assume ok
  ;; (error "Params ~a don't match signature ~a" params reqsig)
  )

;; Called as a *pvs-error-hook* from the pvs-error function in
;; pvs-emacs.lisp, Invokes error to create the pvs-error condition.
;; process-json-request then handles the error by calling json-pvs-error.
(defun json-make-pvs-error (msg errstring fbname place)
  (let ((code (cond ((string= msg "Parser error") 1)
		    ((string= msg "Typecheck error") 2)
		    ((string= msg "Prover error") 3)
		    ((string= msg "Change Workspace") 4)
		    ((string= msg "Init error") 5)
		    (t 0)))
	(plist (pvs:place-list place)))
    (cond (*in-checker*
	   ;; Add the error to the commentary string, as calling error does
	   ;; not work correctly while in the prover.
	   (pvs:commentary (format nil "Error: ~a" errstring)) ; M3 Add 'Error' prefix to commentary [Sept 2020]
	   (pvs:restore))
	  (t (error 'pvs-error
		    :code code
		    :kind msg
		    :error-string errstring
		    :file-name (when fbname (namestring fbname))
		    :place plist)))))

(defun json-pvs-error (id c)
  (with-slots (code kind error-string file-name place) c
    (assert (or (null file-name)
		(file-exists-p file-name)))
    (let* ((err-object `((:code . ,code)
			 (:message . ,kind)
			 (:data . ((:error_string . ,error-string)
				   ,@(when file-name
				       `((:file_name . ,file-name)))
				   ,@(when place `((:place . ,place)))))))
	   (jerr `((:error . ,err-object)
		   (:id . ,id)
		   (:jsonrpc . "2.0"))))
      jerr)))

(defun json-message (msg &optional url (level "info"))
  "url is typically something like 'http://localhost:22335/RPC2'"
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (jmsg (json:encode-json-alist-to-string
		`((:method . ,level)
		  (:params . (,msg))
		  (:jsonrpc . "2.0")))))
    (if (no-url? url)
	jmsg
	(xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun json-buffer (name contents display? read-only? append? kind url)
  (unless (no-url? url)
    (let* ((json:*lisp-identifier-name-to-json* #'identity)
	   (jmsg (json:encode-json-alist-to-string
		  `((:method . "buffer")
		    (:params . ,(list name contents display? read-only? append? kind))
		    (:jsonrpc . "2.0")))))
      (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

;;; JSONRPC Response object:
;;;   { "jsonrpc" : "2.0"
;;;     result | error
;;;     "id" : number }
;;;  where:
;;;   result = "result" : defined by API
;;;   error = "error" : { "code" : integer,
;;;                       "message" : string,
;;;                       "data" : API }

(defun json-response (id response)
  (let* ((json:*json-identifier-name-to-lisp* #'identity)
	 (xmlrpc-res (json:decode-json-from-string response))
	 (json-res (cdr (assoc :jsonrpc_result xmlrpc-res :test #'string-equal)))
	 (rid (cdr (assoc :id json-res :test #'string-equal)))
	 (result (assoc :result json-res :test #'string-equal))
	 (err (cdr (assoc :error json-res :test #'string-equal))))
    (cond ((not (string= id rid))
	   (error "Mismatching ids"))
	  ((and err result)
	   (error "Both result and error given"))
	  (err
	   (error "~a" err))
	  (result
	   (cdr result))
	  (t (error "Neither result nor error given")))))

(defun json-y-or-n (msg full? timeout? url)
  (unless (no-url? url)
    (let* ((json:*lisp-identifier-name-to-json* #'identity)
	   (id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
	   (jmsg (json:encode-json-alist-to-string
		  `((:method . "yes_no")
		    (:params . ,(list msg full? timeout?))
		    (:id . ,id)
		    (:jsonrpc . "2.0")))))
      (string-equal
       (json-response id (xml-rpc-call (encode-xml-rpc-call :request jmsg)
				       :url url))
       "yes"))))

(defun json-query (prompt url)
  "Only currently used by make-directory-path"
  (declare (ignore prompt url))
  ;; (or (null url)
  ;;     (let* ((json:*lisp-identifier-name-to-json* #'identity)
  ;; 	     (id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
  ;; 	     (jmsg (json:encode-json-alist-to-string
  ;; 		    `((:method . "query")
  ;; 		      (:params . ,(list prompt))
  ;; 		      (:id . ,id)
  ;; 		      (:jsonrpc . "2.0")))))
  ;; 	(json-response id (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))
  t ;; For now, always return t
  )

(defun json-dialog (prompt url)
  (declare (ignore prompt url))
  ;; (if (null url)
  ;;     ""
  ;;     (let* ((json:*lisp-identifier-name-to-json* #'identity)
  ;; 	     (id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
  ;; 	     (jmsg (json:encode-json-alist-to-string
  ;; 		    `((:method . "dialog")
  ;; 		      (:params . ,(list prompt))
  ;; 		      (:id . ,id)
  ;; 		      (:jsonrpc . "2.0")))))
  ;; 	(json-response id (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))
  ""
  )

(defun jsonrpc-result (result id)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (jresult `((:result . ,result)
		    (:jsonrpc . "2.0")
		    (:id . ,id))))
    (setq *last-response* jresult)
    jresult))

(defun jsonrpc-error (id c)
  (let* ((result (format nil "~a" c))
	 (json:*lisp-identifier-name-to-json* #'identity)
	 (sresult (json:encode-json-to-string result))
	 (jresult `((:error . ((:code . 1)
				(:message . ,sresult)))
		    (:id . ,id))))
    (setq *last-response* jresult)
    jresult))

;; (defmethod json:encode-json ((th pvs:datatype-or-module) &optional
;; 			     (stream json:*json-output*))
;;   (json:encode-json (pvs:id th) stream))


;; M3: this function adds the information about the current proofstate to the
;;     rpc result. [Sept 2020]
(defun update-ps-control-info-result (ps)
  (when (and pvs:*ps-control-info* (not pvs::*in-apply*))
    (let* ((json:*lisp-identifier-name-to-json* #'identity)
	   (ps-json (pvs:pvs2json ps)))
      (pvs:xmlrpc-output-proofstate (list ps-json)))
    (when *interrupted-rpc*
	    (mp:open-gate (pvs:psinfo-res-gate pvs:*ps-control-info*))
	    (mp:process-wait "Waiting for next Proofstate[update-ps-control-info-result]"
			     #'(lambda () (not (mp:gate-open-p (pvs:psinfo-res-gate pvs:*ps-control-info*))))))))

;; M3: hook for finishing proofstates [Sept 2020].
(defun finish-proofstate-rpc-hook (ps)
  "Prepares the result of the rpc request and closes it."
  (when pvs:*ps-control-info*
    (let*((commentary (cons (format nil "~:[Proof attempt aborted~;Q.E.D.~]" (and (typep ps 'pvs::top-proofstate)
										     (eq (pvs::status-flag ps) '!))) *prover-commentary*))
	  (ps-json
	   `((("label" . ,(pvs::label ps))
	      ("commentary" . ,commentary)))))
      (pvs:add-psinfo pvs:*ps-control-info* ps-json))
    ;; M3 save script as last attempted [Sept 2020]
    (let ((script (pvs::extract-justification-sexp
		   (pvs::collect-justification *top-proofstate*)))
	  (decl (pvs:declaration ps)))
      (setf *last-attempted-proof* (list decl script)))
    (mp:open-gate (pvs:psinfo-res-gate pvs:*ps-control-info*))))

;; M3: hook for successfully closed branches.
(defun rpc-output-notify-proof-success (proofstate)
  (when (and pvs:*ps-control-info* (not pvs::*in-apply*))
    (if (eq 'propax (car(pvs::current-rule proofstate)))
	(let ((ps-json (pvs:pvs2json proofstate)))
	  (pvs:add-psinfo pvs:*ps-control-info* (list ps-json)))
      (let*((prev-cmd (pvs::wish-current-rule proofstate))
	    (label-ps (pvs::label proofstate))
	    (ps-json
	     `(("label" . ,label-ps)
	       ("commentary" . ,(list (format nil "This completes the proof of ~a." label-ps)))
	       ("prev-cmd" . ,(format nil "~s" prev-cmd)))))
	(pvs:add-psinfo pvs:*ps-control-info* (list ps-json))))))
