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

;; (defpackage :pvs-jsonrpc (:use :json :cl-user))

(in-package :pvs-jsonrpc)

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

(defstruct (json-object (:conc-name jo-))
  object)

(defstruct (json-array (:conc-name ja-))
  array)

(defun obj (alist)
  (make-json-object :object alist))

(defun arr (list)
  (make-json-array :array list))

(defmethod json:encode-json ((obj json-object) &optional (stream json:*json-output*))
  (json:encode-json-alist (jo-object obj) stream))

(defmethod json:encode-json ((arr json-array) &optional (stream json:*json-output*))
  (json:encode-json (ja-array arr) stream))

(defvar *current-jsonrpc-request* nil)

(defvar *json-rpc-id-ctr* 0)

(defun process-jsonrpc (message conn)
  ;;(format t "~%[process-jsonrpc] message = ~a" message)
  (handler-case
      (let* ((msg-str (pvs:bytestring-to-string message))
	     (cl-json:*identifier-name-to-key* #'string-downcase)
	     (msg (json:decode-json-from-string msg-str))
	     (msg-obj (jsonrpc-object msg))
	     )
	(process-jsonrpc-object msg-obj conn))
    (error (c)
      ;; No id available if we got to this handler.
      (let* ((err (format nil "~a" c))
	     (jerror (json:encode-json-alist-to-string
		      `(("error" . (("code" . -32700)
				    ("message" . "Json-RPC Error")
				    ("data" . ,err)))
			("id" . nil) ("jsonrpc" . "2.0")))))
	(format t "~%process-jsonrpc: have error without id: ~a" jerror)
	(wsd:send conn jerror)))))

(defmethod process-jsonrpc-object ((req jsonrpc-request) conn)
  "req has jreq-id, jreq-method, jreq-params"
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function (jreq-method req))
    (check-params (jreq-params req) reqsig)
    ;; (format t "~&[pvs-json-rpc.process-jsonrpc-object] received request: ~a~%" req) ;; #debug
    ;; (format t "~&[process-jsonrpc-object] pvs:*pvs-message-hook* ~a~%" pvs:*pvs-message-hook*) ;; #debug
    ;; With id available we can handle json-rpc errors.
    (handler-case
	(let ((*current-jsonrpc-request* req))
	  ;; will have jreq-id, jreq-method, and jreq-params
	  ;; used in pvs-message, etc.
	  (cond (pvs:*in-evaluator*
		 (unless (string-equal (jreq-method req) "pvsio-command")
		   (error "In PVSio, only pvsio-command requests are accepted"))
		 (apply reqfun (jreq-params req)))
		;; Otherwise, expect a result and send it back
		(t (let* ((result (apply reqfun (jreq-params req)))
			  (jresult (json:encode-json-alist-to-string
				    `(("result" . ,result)
				      ("id" . ,(jreq-id req))
				      ("jsonrpc" . "2.0")))))
		     ;;(format t "~%[process-jsonrpc-object] Have result for id ~a: ~%  ~s" (jreq-id req) result)
		     (wsd:send conn jresult)))))
      (pvs:pvs-error (c)
	;;(format t "~%Error (jsonrpc-object): req ~s~%  ~a" req c)
	(let* (;;(json:*lisp-identifier-name-to-json* #'identity)
	       (fname (when (pvs:file-name c)
			(acons "file_name" (namestring (pvs:file-name c)) nil)))
	       (place (when (pvs:place c)
			(acons "place" (pvs:place c) nil)))
	       (msg (pvs:message c))
	       (lerr `(("error" . (("code" . 1)
				   ("message" . ,msg)
				   ("data" . (("error_string" . ,(pvs::error-string c))
					      ,@fname
					      ,@place))))
		       ("id" . ,(jreq-id req)) ("jsonrpc" . "2.0")))
	       (jerror (json:encode-json-alist-to-string lerr)))
	  ;; (format t "~%[jsonrpc-object] Have pvs-error: ~s" jerror)
	  (wsd:send conn jerror)))
      (error (c)
	(let* ((err (format nil "~a" c))
	       (message (if (typep c 'pvs:pvs-error)
			    (pvs:message c)
			    "PVS Error"))
	       (jerror (json:encode-json-alist-to-string
			`(("error" . (("code" . -32700)
				      ("message" . ,message)
				      ("data" . ,err)))
			  ("id" . ,(jreq-id req))
			  ("jsonrpc" . "2.0")))))
	  ;;(break "jsonrpc-object error: ~a" err)
	  ;;(format t "~%[jsonrpc-object] Have error ~%  ~a" jerror)
	  (wsd:send conn jerror))))))

;; (defun send-response (obj)
;;   (let* ((json:*lisp-identifier-name-to-json* #'identity)
;; 	 (ps-json (pvs:pvs2json-response obj))
;; 	 (jresult (json:encode-json-alist-to-string
;; 		   `((:result . ,ps-json)
;; 		     (:id . ,(jreq-id *current-jsonrpc-request*))
;; 		     (:jsonrpc . "2.0"))))
;; 	 (ws-conn (pvs-ws:ws-current-connection)))
;;     (assert ws-conn)
;;     (wsd:send ws-conn jresult)))

(defmethod process-jsonrpc-object ((notif jsonrpc-notification) conn)
  (declare (ignore conn))
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function (jnot-method notif))
    (check-params (jnot-params notif) reqsig)
    (apply reqfun (jnot-params notif))))

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

(defun find-bad-elt (msg valid-keys)
  (find-if-not #'(lambda (elt) (member (car elt) valid-keys :test #'string-equal)) msg))

(defun jsonrpc-object (msg)
  "Checks the message is valid, and returns a corresponding struct"
  ;;(format t "~%jsonrpc-object:~%    ~s" msg)
  (check-jsonrpc-object-msg msg)
  (cond ((let ((method (cdr (assoc "method" msg :test #'string=)))
	       (bad-elt (find-bad-elt msg '("jsonrpc" "method" "params" "id"))))
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
	     (let ((bad-elt (find-bad-elt msg '("jsonrpc" "result" "id"))))
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
	     (let ((bad-elt (find-bad-elt msg '("jsonrpc" "error" "id"))))
	       (when bad-elt
		 (error "Error cannot have member ~a : ~a" (car bad-elt) (cdr bad-elt))))
	     (unless (and (listp err)
			  (every #'(lambda (elt)
				     (and (consp elt) (stringp (car elt))) err)
				 err))
	       (error "Bad error object: ~a" err))
	     (let ((bad-err (find-bad-elt err '("code" "message" "data"))))
	       (when bad-err
		 (error "Error object cannot have member ~a : ~a" (car bad-err) (cdr bad-err))))
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
		:code code
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
  (let* ((pname (intern (format nil "jsonrpc-~a" methodname) :pvs-jsonrpc)))
    `(progn
       (let ((req (assoc ',methodname *pvs-request-methods*)))
	 ;;(format t "~%Adding ~a" ',methodname)
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

(defun pvs-message-hook (msg)
  (when *current-jsonrpc-request*
    (let* ((id (jreq-id *current-jsonrpc-request*))
	   (lreq `((:method . :pvs-message)
		   ;; We include an id so it doesn't have to be broadcast
		   ;; to all waiting requests
		   ("message" . ,msg)
		   ("id" . ,id)
		   ("jsonrpc" . "2.0")))
	   (jreq (json:encode-json-alist-to-string lreq)))
      (format t "~&[pvs-json-rpc.pvs-message-hook] sending message >>> ~a <<<~%" jreq) ;; #debug
      (wsd:send (pvs-ws:ws-current-connection) jreq))))

(defun pvs-y-or-n-hook (msg &optional yesno-p timeout-p)
  (let* ((id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
         (jmsg (json:encode-json-alist-to-string
                `(("method" . "yes_no")
                  ("params" . (("message" . ,msg)
			       ("fullp" . ,yesno-p)
			       ("timemoutp" . ,timeout-p)))
                  ("id" . ,id)
                  ("jsonrpc" . "2.0")))))
    (wsd:send (pvs-ws:ws-current-connection) jmsg)
    ;; (string-equal
    ;;  (json-response id)
    ;;  "yes")
    ))
