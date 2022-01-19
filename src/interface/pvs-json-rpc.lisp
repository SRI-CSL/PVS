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

(defpackage :pvs-jsonrpc (:use :json :cl-user :common-lisp))

(in-package :pvs-jsonrpc)

;; (export '(process-jsonrpc *interrupted-rpc*
;; 	  update-ps-control-info-result rpc-output-notify-proof-success
;; 	  finish -proofstate-rpc-hook update-ps-control-info-result))

(defun process-jsonrpc (message conn)
  ;; (format t "~%process-jsonrpc: message = ~a" message)
  (setq pvs::mmm message)
  (handler-case
      (let* ((json:*json-identifier-name-to-lisp* #'identity)
	     (msg-str (pvs:bytestring-to-string message))
	     (msg (json:decode-json-from-string msg-str))
	     (msg-obj (jsonrpc-object msg)))
	(process-jsonrpc-object msg-obj conn))
    (error (c)
      ;; No id available if we got to this handler.
      (let* ((err (format nil "~a" c))
	     (jerror (json:encode-json-to-string
		      `((:error . ((:code . -32700) (:message . "Json-RPC Error") (:data . ,err)))
			(:id . nil) (:jsonrpc . "2.0")))))
	;;(format t "~%Have error for id ~a: ~%  ~a" (jreq-id req) jresult)
	(format t "~%process-jsonrpc: have error without id: ~a" jerror)
	(wsd:send conn jerror)))))

(defstruct (jsonrpc-request (:conc-name jreq-))
  method
  params
  id)

(defstruct (jsonrpc-notification (:conc-name jnot-))
  method
  params)

(defstruct (jsonrpc-result (:conc-name jres-))
  result
  id)

(defstruct (jsonrpc-error (:conc-name jerr-))
  code
  message
  data
  id)

(defvar *current-jsonrpc-request* nil)

(defmethod process-jsonrpc-object ((req jsonrpc-request) conn)
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function (jreq-method req))
    (check-params (jreq-params req) reqsig)
    (format t "~%process-jsonrpc request: ~a" reqfun)
    ;; With id available we can handle json-rpc errors.
    (handler-case
	(let ((*current-jsonrpc-request* req))
	  ;; Used by the corresponding send
	  (cond (pvs:*in-checker*
		 (unless (string-equal (jreq-method req) "proof-command")
		   (error "In the prover, only proof-command requests are accepted"))
		 ;; We don't expect a result, prover hooks will return the sequent
		 (apply reqfun (jreq-params req)))
		(pvs:*in-evaluator*
		 (unless (string-equal (jreq-method req) "pvsio-command")
		   (error "In PVSio, only pvsio-command requests are accepted"))
		 (apply reqfun (jreq-params req)))
		;; Otherwise, expect a result and send it back
		(t (let* ((result (apply reqfun (jreq-params req)))
			  (jresult (json:encode-json-alist-to-string
				    `((:result . ,result)
				      (:id . ,(jreq-id req))
				      (:jsonrpc . "2.0")))))
		     (format t "~%Have result for id ~a: ~%  ~a" (jreq-id req) jresult)
		     (wsd:send conn jresult)))))
      (pvs:pvs-error (c)
	(let* (;;(json:*lisp-identifier-name-to-json* #'identity)
	       (jerror (json:encode-json-alist-to-string
			`(("error" . (("code" . 1)
				      ("message" . ,(pvs::message c))
				      ("data" . (("error_string" . ,(pvs::error-string c))
						 ("file_name" . ,(pvs::file-name c))
						 ("place" . ,(pvs::place c))))))
			  ("id" . ,(jreq-id req)) ("jsonrpc" . "2.0")))))
	  (format t "~%Have pvs-error: ~a" jerror)
	  (wsd:send conn jerror)))
      (error (c)
	(setq cc1 c)
	(let* ((err (format nil "~a" c))
	       (jerror (json:encode-json-alist-to-string
			`((:error . ((:code . -32700) (:message . "PVS Error") (:data . ,err)))
			  (:id . ,(jreq-id req)) (:jsonrpc . "2.0")))))
	  ;;(format t "~%Have error for id ~a: ~%  ~a" (jreq-id req) jresult)
	  (format t "~%Have error: ~a" jerror)
	  (wsd:send conn jerror))))))

(defun send-response (obj)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (ps-json (pvs:pvs2json obj))
	 (jresult (json:encode-json-alist-to-string
		   `((:result . ,ps-json)
		     (:id . ,(jreq-id *current-jsonrpc-request*))
		     (:jsonrpc . "2.0"))))
	 (ws-conn (pvs-ws:ws-current-connection)))
    (assert ws-conn)
    (wsd:send ws-conn jresult)))

(defmethod process-jsonrpc-object ((req jsonrpc-notification) conn)
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function (jreq-method req))
    (check-params (jreq-params req) reqsig)
    (apply reqfun params)))

(defmethod process-jsonrpc-object ((req jsonrpc-result) conn)
  (break "Needs defining"))

(defmethod process-jsonrpc-object ((req jsonrpc-error) conn)
  (break "Needs defining"))

(defun check-jsonrpc-object-msg (msg)
  (assert (and (listp msg)
	       (every #'(lambda (elt)
			  (and (consp elt)
			       (typep (car elt) '(or symbol string))
			       (member (car elt)
				       '(jsonrpc method params id result error)
				       :test 'string-equal)))
		      msg)) () "Badly formed message of type ~a:~%  ~s" (type-of msg) msg)
  (assert (let ((vers (cdr (assoc 'jsonrpc msg :test #'string-equal))))
	    (string= vers "2.0"))
	  () "Must be a jsonrpc message, with version 2.0")
  )

(defun jsonrpc-object (msg)
  "Checks the message is valid, and returns a corresponding struct"
  ;;(format t "~%Checking jsonrpc for message~%  ~s" msg)
  (check-jsonrpc-object-msg msg)
  (cond ((let ((method (cdr (assoc "method" msg :test #'string=)))
	       (bad-elt (find-if-not #'(lambda (elt)
					 (member (car elt) '(jsonrpc method params id)
						 :test #'string-equal))
			  msg)))
	   (when bad-elt
	     (error "Method cannot have member ~a : ~a" (car bad-elt) (cdr bad-elt)))
	   (unless (stringp method)
	     (error "Method must be a string: ~a" method))
	   (let ((params (cdr (assoc 'params msg :test #'string-equal)))
		 (id (cdr (assoc :id msg :test #'string-equal))))
	     (unless (listp params)
	       (error "Params must be a list: ~a" params))
	     (unless (or (null id) (integerp id) (stringp id))
	       (error "Id must be a string: ~s: ~a" id (type-of id)))
	     (if id
		 (make-jsonrpc-request
		  :method method
		  :params params
		  :id id)
		 (make-jsonrpc-notification
		  :method method
		  :params params)))))
	((let ((result (cdr (assoc "result" msg :test #'string=))))
	   (when result
	     (let ((bad-elt (find-if-not #'(lambda (elt)
					     (member (car elt) '("jsonrpc" "result" "id")
						     :test #'string=))
			      msg)))
	       (when bad-elt
		 (error "Result cannot have member ~a : ~a" (car bad-elt) (cdr bad-elt))))
	     (let ((id (cdr (assoc :id msg :test #'string-equal))))
	       (unless (stringp id)
		 (error "Id must be a string: ~a" id))
	       (make-jsonrpc-result
		:result result
		:id id)))))
	((let ((err (cdr (assoc "error" msg :test #'string=))))
	   (when err
	     (let ((bad-elt (find-if-not #'(lambda (elt)
					     (member (car elt) '("jsonrpc" "error" "id")
						     :test #'string=))
			      msg)))
	       (when bad-elt
		 (error "Error cannot have member ~a : ~a" (car bad-elt) (cdr bad-elt))))
	     (unless (and (listp err)
			  (every #'(lambda (elt)
				     (and (consp elt) (stringp (car elt))) err)
				 err))
	       (error "Bad error object: ~a" err))
	     (let ((bad-err (find-if-not #'(lambda (elt)
					     (member (car elt) '("code" "message" "data")
						     :test #'string=))
			      err)))
	       (error "Error object cannot have member ~a : ~a" (car bad-err) (cdr bad-err)))
	     (let ((id (cdr (assoc :id msg :test #'string-equal)))
		   (code (cdr (assoc "code" err :test #'string=)))
		   (message (cdr (assoc "message" err :test #'string=)))
		   (data (cdr (assoc "data" err :test #'string=))))
	       (unless (stringp id)
		 (error "Id must be a string: ~a" id))
	       (unless (integerp code)
		 (error "Code must be an integer: ~a" code))
	       (unless (stringp message)
		 (error "Message must be a string: ~a" message))
	       (make-jsonrpc-error
		:code result
		:message message
		:data data
		:id id)))))
	(t (error "Invalid jsonrpc message: ~a" msg))))

;;;;;;

;; Requires cl-json

(defvar *interrupted-rpc* nil "Indicates if the last request was interrupted by the user of the system (signals).")

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

(defun check-params (params signature)
  (declare (ignore params signature))
  ;;; for now, assume ok
  ;; (error "Params ~a don't match signature ~a" params reqsig)
  )

;;; JSONRPC Response object:
;;;   { "jsonrpc" : "2.0"
;;;     result | error
;;;     "id" : number }
;;;  where:
;;;   result = "result" : defined by API
;;;   error = "error" : { "code" : integer,
;;;                       "message" : string,
;;;                       "data" : API }
