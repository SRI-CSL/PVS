
(defpackage :pvs-json-rpc (:use :json :cl-user :common-lisp :pvs))

(in-package :pvs-json-rpc)

(export '(process-json-request))

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

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *pvs-request-methods* nil
    "An assoc-list with entries (method fun sig ret descr)"))

(defmacro defrequest (methodname args docstring &rest body)
  "Creates a JSON-RPC request form"
  (assert (symbolp methodname) () "defrequest method must be a symbol")
  (assert (listp args) () "defrequest args must be a list")
  (assert (stringp docstring) () "defrequest docstring must be a string")
  (let* ((pname (intern (format nil "jsonrpc-~a" methodname) :pvs-json-rpc)))
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
(defun process-json-request (method params id url)
  (if id
      (handler-case (process-json-request* method params id url)
	(error (c) (jsonrpc-error id c)))
      (process-json-request* method params id url)))

(defun process-json-request* (method params id url)
  (unless (stringp method)
    (error "method ~a must be a symbol string" method))
  (unless (listp params)
    (error "parameters ~a must be a list" params))
  (multiple-value-bind (reqfun reqsig)
      (get-json-request-function method)
    (setq *last-response* nil)
    (check-params params reqsig)
    (let ((result (with-pvs-hooks url (apply reqfun params))))
      (setq *last-response* result)
      (jsonrpc-result result id))))

(defun check-params (params signature)
  (declare (ignore params signature))
  ;;; for now, assume ok
  ;; (error "Params ~a don't match signature ~a" params reqsig)
  )

;;; With-pvs-hooks sets up callbacks when the url is provided
;;; Used primarily for informative messages that are not part of the result
;;; E.g., "foo typechecked in 3 sec", garbage collecting, ...

(defmacro with-pvs-hooks (url &rest body)
  (let ((gurl (gentemp)))
    `(let ((,gurl ,url))
       (if ,gurl
	   (let ((pvs:*pvs-message-hooks*
		  (cons #'(lambda (msg) (json-message msg ,gurl))
			pvs:*pvs-message-hooks*))
		 (pvs:*pvs-warning-hooks*
		  (cons #'(lambda (msg) (json-message msg ,gurl :warning))
			pvs:*pvs-warning-hooks*))
		 (pvs:*pvs-buffer-hooks*
		  (cons #'(lambda (name contents display? read-only? append? kind)
			    (json-buffer name contents display? read-only? append? kind
					 ,gurl))
			pvs:*pvs-buffer-hooks*))
		 (pvs:*pvs-y-or-n-hook*
		  #'(lambda (msg full? timeout?)
		      (json-y-or-n msg full? timeout? ,gurl))))
	     ,@body)
	   (progn ,@body)))))

(defun json-message (msg url &optional (level :info))
  (when url
    (let* ((json:*lisp-identifier-name-to-json* #'identity)
	   (jmsg (json:encode-json-to-string
		  `((:method . ,level)
		    (:params . (,msg))
		    (:jsonrpc . "2.0")))))
      (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun json-buffer (name contents display? read-only? append? kind url)
  (when url
    (let* ((json:*lisp-identifier-name-to-json* #'identity)
	   (jmsg (json:encode-json-to-string
		  `((:method . :buffer)
		    (:params . ,(list name contents display? read-only? append? kind))
		    (:jsonrpc . "2.0")))))
      (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun json-y-or-n (msg full? timeout? url)
  (or (null url)
      (let* ((json:*lisp-identifier-name-to-json* #'identity)
	     (jmsg (json:encode-json-to-string
		    `((:method . :yes-no)
		      (:params . ,(list msg full? timeout?))
		      (:id . ,(pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
		      (:jsonrpc . "2.0")))))
	(xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun jsonrpc-result (result id)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (jresult `((:result . ,result)
		    (:jsonrpc . "2.0")
		    (:id . ,id))))
    (setq *last-response* jresult)
    jresult))

(defun jsonrpc-error (result id)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (sresult (json:encode-json-to-string result))
	 (jresult (json:encode-json-to-string
		   `((:result . ,sresult)
		     (:id . ,id)))))
    (setq *last-response* jresult)
    jresult))

;; (defmethod json:encode-json ((th pvs:datatype-or-module) &optional
;; 			     (stream json:*json-output*))
;;   (json:encode-json (pvs:id th) stream))
