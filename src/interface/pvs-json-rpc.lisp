
(defpackage :pvs-json-rpc (:use :json :cl-user :common-lisp :pvs))

(in-package :pvs-json-rpc)

(export '(process-json-request))

(define-condition pvs-error (simple-error)
  ((message :accessor message :initarg :message)
   (error-file :accessor error-file :initarg :error-file)
   (file-or-buffer-name :accessor file-or-buffer-name :initarg :file-or-buffer-name)
   (place :accessor place :initarg :place)))

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
		      #'(lambda (msg) (json-message msg ,gurl :warning))
		      pvs:*pvs-buffer-hook*
		      #'(lambda (name contents display? read-only? append? kind)
			  (json-buffer name contents display? read-only?
				       append? kind ,gurl))
		      pvs:*pvs-y-or-n-hook*
		      #'(lambda (msg full? timeout?)
			  (json-y-or-n msg full? timeout? ,gurl))
		      pvs:*pvs-dialog-hook*
		      #'(lambda (prompt)
			  (json-dialog prompt ,gurl))))
	      ,@body)
	 (setf pvs:*pvs-error-hook* error-hook
	       pvs:*pvs-message-hook* message-hook
	       pvs:*pvs-warning-hook* warning-hook
	       pvs:*pvs-buffer-hook* buffer-hook
	       pvs:*pvs-y-or-n-hook* y-or-n-hook
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
	(pvs-error (c) (json-pvs-error id c))
	(error (c) (jsonrpc-error id c)))
      (process-json-request* method params id url)))

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
    (let ((result (with-pvs-hooks url (apply reqfun params))))
      (setq *last-response* result)
      (jsonrpc-result result id))))

(defun check-params (params signature)
  (declare (ignore params signature))
  ;;; for now, assume ok
  ;; (error "Params ~a don't match signature ~a" params reqsig)
  )

;; Called from pvs-error, but this is the response, unlike, e.g., pvs-message
(defun json-make-pvs-error (msg errfile fbname place)
  (let ((plist (pvs:place-list place)))
    (error 'pvs-error
	   :message msg
	   :error-file errfile
	   :file-or-buffer-name (when fbname (namestring fbname))
	   :place plist)))

(defun json-pvs-error (id c)
  (with-slots (message error-file file-or-buffer-name place) c
    (let* ((jerr `((:error . ((:code . 0)
			      (:message . ,message)
			      (:data . ((:error_file . ,error-file)
					,@(when file-or-buffer-name
						`((:theory . ,file-or-buffer-name)))
					,@(when place
						`((:begin . ,(list (car place)
								   (cadr place)))))))))
		   (:id . ,id)
		   (:jsonrpc . "2.0"))))
      jerr)))

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
  (or (null url)
      (let* ((json:*lisp-identifier-name-to-json* #'identity)
	     (id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
	     (jmsg (json:encode-json-to-string
		    `((:method . :yes_no)
		      (:params . ,(list msg full? timeout?))
		      (:id . ,id)
		      (:jsonrpc . "2.0")))))
	(json-response id (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url)))))

(defun json-dialog (prompt url)
  (or (null url)
      (let* ((json:*lisp-identifier-name-to-json* #'identity)
	     (id (pvs:makesym "pvs_~d" (incf *json-rpc-id-ctr*)))
	     (jmsg (json:encode-json-to-string
		    `((:method . :dialog)
		      (:params . ,(list prompt))
		      (:id . ,id)
		      (:jsonrpc . "2.0")))))
	(json-response id (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url)))))

(defun jsonrpc-result (result id)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (jresult `((:result . ,result)
		    (:jsonrpc . "2.0")
		    (:id . ,id))))
    (setq *last-response* jresult)
    jresult))

(defun jsonrpc-error (id result)
  (let* ((json:*lisp-identifier-name-to-json* #'identity)
	 (sresult (json:encode-json-to-string result))
	 (jresult (json:encode-json-to-string
		   `((:error . ((:code . 1)
				(:message . ,sresult)))
		     (:id . ,id)))))
    (setq *last-response* jresult)
    jresult))

;; (defmethod json:encode-json ((th pvs:datatype-or-module) &optional
;; 			     (stream json:*json-output*))
;;   (json:encode-json (pvs:id th) stream))
