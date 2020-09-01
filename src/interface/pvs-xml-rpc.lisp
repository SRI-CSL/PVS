;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-xml-rpc.lisp -- makes pvs an xml-rpc server/client
;; Author          : Sam Owre
;; Created On      : Fri Feb  8 13:03:25 2013
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Feb 12 22:40:02 2013
;; Update Count    : 3
;; Status          : Unknown, Use with caution!;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defpackage :pvs-xml-rpc (:use :cl-user :common-lisp))

(in-package :pvs-xml-rpc)

(export '(pvs-server))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :xml-rpc)
  (use-package :net.xml-rpc)
  (use-package :net.aserve)
  )

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Future use
  (require :sb-bsd-sockets))

;; #-allegro
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :s-xml-rpc)
;;   (use-package :s-xml-rpc))

(defvar *pvs-xmlrpc-server*)
;;; net.aserve:*wserver* will give the port

(defparameter *pvs-top-level-globals*
  '(pvs:*ps* pvs:*top-proofstate* pvs:*last-proof*))

#+allegro
(defun pvs-server (&key (port 22334))
  ;; (dolist (glbl *pvs-top-level-globals*)
  ;;   (unless (assoc glbl excl:*required-top-level-bindings*)
  ;;     (push (cons glbl glbl) excl:*required-top-level-bindings*))
  ;;   (unless (assoc glbl excl:*required-thread-bindings*)
  ;;     (push (cons glbl glbl) excl:*required-thread-bindings*)))
  (let ((cmdsrv (make-xml-rpc-server 
		 :start (list 
			 ;; :host "locahost" 
			 :port port))))
    (export-xml-rpc-method cmdsrv
	'("pvs.request" xmlrpc-pvs-request t "Request a PVS method.")
      :string :string :string)
    (setq *pvs-xmlrpc-server* cmdsrv)
    (setq pvs:*pvs-lisp-process* mp:*current-process*)
    t))

(defvar *client-url* nil)

;;; This is an xml-rpc method - the json-request is the json-rpc part
;;; the client-url is to satisfy acting like json-rpc, by providing a
;;; URL for callbacks.  The return is of the form
;;; {"context": dir, "mode": mode, "jsonrpc-result": jsonrpc, "error": error}
;;; Only one of error or jsonrpc-result is returned, depending on if an id
;;; was included in the request.
(defun xmlrpc-pvs-request (json-request client-url)
  ;;(format t "~%pvs.request: ~s~%  from ~s~%" json-request client-url)
  (handler-case
      (let* ((json:*json-identifier-name-to-lisp* #'identity)
	     (request (json:decode-json-from-string json-request))
	     (id (cdr (assoc :id request :test #'string-equal)))
	     (method (cdr (assoc :method request :test #'string-equal)))
	     (params (cdr (assoc :params request :test #'string-equal)))
	     (*print-pretty* nil))
	;; (setq *last-request* (list method params id client-url))
	;; (apply #'pvs-json:process-json-request pvs-xml-rpc::*last-request*)
	;; (format t "~%xmlrpc-pvs-request: current-process = ~a~%   request = ~a~%"
	;;   mp:*current-process* request)
	(let* ((result (pvs-json:process-json-request method params id client-url))
	       (jresult (xmlrpc-result result id)))
	  ;; (format t "~%xmlrpc-pvs-request end: current-process = ~a~%   result = ~s~%"
	  ;;   mp:*current-process* jresult)
	  jresult))
    ;; Note: id will not be available if this error is hit
    (error (c) (xmlrpc-error (format nil "~a" c)))))

(defun xmlrpc-error (msg)
  ;; id not available, else would be caught.
  (let ((json:*lisp-identifier-name-to-json* #'identity))
    (json:encode-json-to-string msg)))

(defun xmlrpc-result (result id)
  (declare (ignore id))
  (let ((json:*lisp-identifier-name-to-json* #'identity))
    (json:encode-json-to-string result)))

(defun pvs-current-mode ()
  (let ((proc (mp:process-name-to-process "Initial Lisp Listener")))
    (multiple-value-bind (inchecker cbound?)
	(mp:symeval-in-process 'pvs:*in-checker* proc)
      (declare (ignore cbound?))
      (multiple-value-bind (ineval ebound?)
	  (mp:symeval-in-process 'pvs:*in-evaluator* proc)
	(declare (ignore ebound?))
	(cond (inchecker "prover")
	      (ineval "evaluator")
	      (t "lisp"))))))
