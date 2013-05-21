;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-xml-rpc.lisp -- makes pvs an xml-rpc server/client
;; Author          : Sam Owre
;; Created On      : Fri Feb  8 13:03:25 2013
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Feb 12 22:40:02 2013
;; Update Count    : 3
;; Status          : Unknown, Use with caution!;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :pvs-xml-rpc (:use :cl-user :common-lisp :pvs))

(in-package :pvs-xml-rpc)

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :xml-rpc)
  (use-package :net.xml-rpc)
  (use-package :net.aserve)
  )

#-allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :s-xml-rpc)
  (use-package :s-xml-rpc))

(defvar *pvs-url* "http://localhost:55223/RPC2")

(defvar *pvs-xmlrpc-server*)

;; To test from Python:
;; import xmlrpclib
;; s = xmlrpclib.ServerProxy('http://localhost:55223')
;; print s.system.listMethods()

;; The idea is to use xml-rpc to do what is currently done in Emacs/PVS by
;; simply looking for patterns in standard output.  For this, Emacs sends a
;; lisp form to be evaluated as a request, and waits for the prompt, roughly
;; treating everything output upto the prompt as the response.  However,
;; special forms may be matched on, that look like :pvs-foo ... :end-pvs-foo
;; These are not responses, but things like GC messages, buffer popups, etc.

;; Unfortunately, xml-rpc is based on the simple request/response model, so
;; this cannot be easily done on a single channel.  The approach taken here
;; is to open up two ports, thus PVS is both a server and client, as is the UI.

;; In each case, the server side waits for a method call, processes the request,
;; and returns a response.  The input request is a JSON form (based on JSON-RPC)
;;   {"method": "function", "params": [param1, ..., paramn], "id": id}
;; The server then returns a form such as
;;   {"result": result, "error": error, "id": id}
;; with only one of result or error.
;; The input request may omit the id, in which case the request does not
;; expect a response - GC messages are good examples of this.

#+allegro
(defun pvs-server (&key (cmdport 55223))
  (let ((cmdsrv (make-xml-rpc-server 
		 :start (list 
			 ;; :host "locahost" 
			 :port cmdport))))
    (export-xml-rpc-method cmdsrv
	'("pvs.request" xmlrpc-pvs-request t "Request a PVS method.")
      :string :string :string)
    (values cmdsrv)))

(defvar *client-url* nil)

(defvar *last-request* nil)
(defvar *last-response* nil)

;;; This is an xml-rpc method - the json-string is the json-rpc part
;;; the client-url is to satisfy acting like json-rpc, by providing a
;;; URL for callbacks.  The return is of the form
;;; {"context": dir, "mode": mode, "jsonrpc-result": jsonrpc, "error": error}
;;; Only one of error or jsonrpc-result is returned, depending of if an id
;;; was included in the request.
(defun xmlrpc-pvs-request (json-string client-url)
  (format t "~%pvs.request: ~s~%  from ~s~%" json-string client-url)
  (handler-case
      (let* ((request (json:decode-json-from-string json-string))
	     (json-id (cdr (assoc :id request :test #'string-equal)))
	     (method (cdr (assoc :method request :test #'string-equal)))
	     (params (cdr (assoc :params request :test #'string-equal)))
	     (*print-pretty* nil))
	(setq *last-request* (list json-id method params))
	(cond ((not (stringp method))
	       (xmlrpc-error (format nil "method ~a must be a symbol string"
			       method) json-id))
	      ((not (listp params))
	       (xmlrpc-error (format nil "parameters ~a must be a list"
			     params) json-id))
	      (t (format t "~%Calling process-pvs-request: ~a ~a ~a ~a"
		   json-id method params client-url)
		 (process-pvs-request json-id method params client-url))))
    ;; No json-id available in this case
    (cl-json:json-syntax-error (c) (format nil "~a" c))
    (error (c) (format nil "~a" c))))

;;; These are the valid methods for the API
;;; (method (lisp-function r1 ... rn &optional o1 ... om) 
(defparameter *pvs-request-methods*
  '((typecheck (xmlrpc-pvs-typecheck-file
		(filename &optional forced? prove-tccs? importchain? nomsg?)))
    (show-tccs (show-tccs
		(theoryname &optional flag)))
    (prove (prove-file-at
	    (name declname line rerun?
		  &optional origin buffer prelude-offset
		  background? display? unproved?)))))

(defmacro with-pvs-hooks (url &rest body)
  (let ((gurl (gentemp)))
    `(let ((,gurl ,url))
       (if ,gurl
	   (let ((*pvs-message-hooks*
		  (cons #'(lambda (msg) (json-message msg ,gurl))
			*pvs-message-hooks*))
		 (*pvs-warning-hooks*
		  (cons #'(lambda (msg) (json-message msg ,gurl :warning))
			*pvs-warning-hooks*))
		 (*pvs-buffer-hooks*
		  (cons #'(lambda (name contents display? read-only? append? kind)
			    (json-buffer name contents display? read-only? append? kind
					 ,gurl))
			*pvs-buffer-hooks*)))
	     ,@body)
	   (progn ,@body)))))

(defun json-message (msg url &optional (level :info))
  (when url
    (let ((jmsg (json:encode-json-alist-to-string
		 `((:method . ,level)
		   (:params . (,msg))
		   (:jsonrpc . "2.0")))))
      (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun json-buffer (name contents display? read-only? append? kind url)
  (when url
    (let ((jmsg (json:encode-json-alist-to-string
		 `((:method . :buffer)
		   (:params . ,(list name contents display? read-only? append? kind))
		   (:jsonrpc . "2.0")))))
      (xml-rpc-call (encode-xml-rpc-call :request jmsg) :url url))))

(defun process-pvs-request (id method params url)
  (let ((entry (assoc method *pvs-request-methods*
		      :test #'string-equal)))
    (setq *last-response* nil)
    (if entry
	(let* ((spec (cadr entry))
	       (fun (car spec))
	       (sig (cadr spec))
	       (ret (caddr spec)))
	  (if (check-params params sig)
	      (if t
		  (let ((result (with-pvs-hooks url (apply fun params))))
		    (setq *last-response* result)
		    (xmlrpc-result result id))
		  (handler-case
		      (let ((result (with-pvs-hooks url (apply fun params))))
			(setq *last-response* result)
			(xmlrpc-result result id))
		    (error (c) (xmlrpc-error (format nil "~a" c) id))))
	      (xmlrpc-error "Params don't match" id)))
	(xmlrpc-error (format nil "Method ~a not found" method) id))))

(defun xmlrpc-error (msg id)
  (if id
      (let ((jsonrpc-error
	     (json:with-explicit-encoder
		 (json:encode-json-to-string
		  (cons :object
			`((:error . ,msg)
			  (:id . ,id)))))))
	(xmlrpc-value jsonrpc-error nil))
      (xmlrpc-value nil msg)))

(defun xmlrpc-result (result id)
  (let ((jsonrpc-result
	 (when id
	   (json:with-explicit-encoder
	       (json:encode-json-to-string
		(cons :object
		      `((:result . ,result)
			(:id . ,id))))))))
    (xmlrpc-value jsonrpc-result nil)))

(defun xmlrpc-value (jsonrpc-value err)
  (let ((result (cond (jsonrpc-value (cons :jsonrpc jsonrpc-value))
		      (err (cons :error err)))))
    (json:with-explicit-encoder
	(json:encode-json-to-string
	 (cons :object
	       `((:mode . ,(pvs-current-mode))
		 (:context . ,(current-context-path))
		 ,result))))))

(defun pvs-current-mode ()
  (cond (*in-checker* :prover)
	(*in-evaluator* :evaluator)
	(t :lisp)))

(defun json-result (result id)
  (let* ((sresult (json:encode-json-to-string result))
	 (jresult (json:with-explicit-encoder
		   (json:encode-json-to-string
		    (cons :object
			  `((:result . ,sresult)
			    (:id . ,id)))))))
    (setq *last-response* jresult)
    jresult))

(defmethod json:encode-json ((th datatype-or-module) &optional
			     (stream json:*json-output*))
  (json:encode-json (id th) stream))

;; (defun json-result (result id)
;;   (setq *last-response* result)
;;   ;;jresult
;;   nil)

(defun check-params (params signature)
  ;;; for now, assume ok
  t)

(defun json-eval-form (form)
  (if (or *in-checker* *in-evaluator*)
      form
      (eval form)))

(defun pvs-subscribe (json-string)
  (let ((subscription (json:decode-json-from-string json-string)))
    (cond ((and (listp subscription)
		(assq :URL subscription))
	   (pushnew (cdr (assq :URL subscription)) *pvs-subscribers*
		    :test #'string=)
	   (json-result "t")))))

;; #-allegro
;; (defun pvs-server (&optional (port 55223))
;;   (let ((s-xml-rpc:*xml-rpc-debug* t)
;; 	(s (s-xml-rpc:start-xml-rpc-server :port port)))
;;     (sleep 1)
;;     (export-xml-rpc-method s
;; 	'("pvs.csl.sri.com" xmlrpc-pvs-typecheck-file t)
;;       :string :struct)
;;     (setf *pvs-xmlrpc-server* s)))

(defun xmlrpc-pvs-typecheck-file (filename &optional optargs)
  ;; filename is a string
  ;; optargs is a struct of form
  ;; {"forced?" :bool "prove-tccs?" :bool "importchain?" :bool "nomsg?" :bool}
  (let ((theories (typecheck-file filename)))
    (if (null theories
    (cons :array (xmlrpc-theories theories))))

(defun xmlrpc-theories (theories &optional thdecls)
  (if (null theories)
      (nreverse thdecls)
      (let ((thdecl (xmlrpc-theory (car theories))))
	(xmlrpc-theories (cdr theories) (cons thdecl thdecls)))))

(defun xmlrpc-theory (theory)
  (cons :object
	(acons :theory (id theory)
	       (acons :decls (cons :array
				   (xmlrpc-theory-decls (all-decls theory)))
		      nil))))

(defun xmlrpc-theory-decls (decls &optional thdecls)
  (if (null decls)
      (nreverse thdecls)
      (let ((thdecl (xmlrpc-theory-decl (car decls))))
	(xmlrpc-theory-decls
	 (cdr decls)
	 (if thdecl
	     (cons thdecl thdecls)
	     thdecls)))))

(defun xmlrpc-theory-decl (decl)
  (xmlrpc-theory-decl* decl))

(defmethod xmlrpc-theory-decl* ((decl var-decl))
  nil)

(defmethod xmlrpc-theory-decl* ((imp importing))
  (cons :object
	`((:importing . ,(str (theory-name imp)))
	  (:kind . :importing)
	  (:place . ,(cons :array (place-list imp))))))

(defmethod xmlrpc-theory-decl* ((decl typed-declaration))
  (cons :object
	`((:id . ,(id decl))
	  (:kind . ,(kind-of decl))
	  (:type . ,(str (type decl)))
	  (:place . ,(cons :array (place-list decl))))))

(defmethod xmlrpc-theory-decl* ((decl formula-decl))
  (cons :object
	`((:id . ,(id decl))
	  (:kind . :formula)
	  (:place . ,(cons :array (place-list decl))))))
