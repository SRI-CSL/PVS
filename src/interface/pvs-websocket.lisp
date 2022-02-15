;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-websocket.lisp -- makes pvs a websocket over json-rpc server/client
;; Author          : Sam Owre
;; Created On      : Fri Feb  8 13:03:25 2013
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Feb 12 22:40:02 2013
;; Update Count    : 3
;; Status          : Unknown, Use with caution!;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defines a websocket over json-rpc server for PVS.  The websocket protocol
;; basically provides bi-directional persistent connection and waits for one of four
;; possible events sent from clients:
;;   open: establish a new connection (many connections may be opened)
;;   message: a message is sent from the client
;;   error: an error was detected, e.g., the connection was interrupted
;;   close: the client wants to close the connection

;; Assume the client uses the Javascript ws module, then a typical
;; interaction is as follows:
;; JS: creates new WebSocket ---> PVS: gets open event, establishing connection
;; JS: sends message --> PVS: Gets message event \
;; PVS: sends message --> JS: Gets message event / any number, in any order
;; JS: closes connection --> PVS: gets close event, shuts down connection

;; Websocket error events should be handled on both sides, but are not normally sent

;; The JSON-RPC subprotocol is used only within Websocket messages.  It
;; consists of request objects and response objects; note that these may be
;; sent from either the server or the client.
;;   request: a JSON object of the form
;;            { "jsonrpc" : "2.0",
;;              "method" : "rpc.foo",
;;              "params" : [...],
;;              "id" : string }
;;            params may be an array or object, or omitted if the method doesn't require them
;;            id may be omitted, if no response is wanted/expected (a notification)
;;   response: a successful response is a JSON object of the form
;;             { "jsonrpc" : "2.0",
;;               "result" : JSON,
;;               "id" : "1234" }
;;             an error response is the same, but with "error" replacing "result"
;;             and its value has the form
;;               "error" : { "code" : integer,
;;                           "message" : string,
;;                           "data" : JSON }

;; When PVS receives a message, it first determines if it is a request or a notification.
;; Notifications are simply acted on by PVS, with no response given.
;; Requests need a response, but this should be done in such a way that
;; notifications may be given before the final response is provided.

;; 

;; Thus when a request is made, the current thread is associated with the
;; jsonrpc id. When PVS wants to issue a pvs-message on that thread, it sends a
;; websockets notification, which will be sent on the associated connection

;; See pvs-json-rpc.lisp for details on the protocol, and
;; pvs-json-methods.lisp for the methods provided by PVS.

;; Uses websocket-server (https://github.com/fukamachi/websocket-driver)
;; Designed to work with clack web application environment (https://github.com/fukamachi/clack)
;; Which builds on the Hunchentoot server by default (https://edicl.github.io/hunchentoot/)
;; All of these are loaded using quicklisp

;; (require :sb-cltl2 "~/sbcl/obj/sbcl-home/contrib/sb-cltl2")

(defpackage :pvs-websocket
  (:use :cl-user :common-lisp)
  (:nicknames :pvs-ws))

(in-package :pvs-websocket)

;; Uses websocket-driver package, abbreviated :wsd

;; To stop then server, run (clack:stop *websocket-server*)
;;   (clack.handler::handler-acceptor *websocket-server*)
;; Doesn't seem to work completely, try
;;   (sb-thread:terminate-thread (fourth (sb-thread:list-all-threads)))
;;   where fourth here is then hunchentoot-listener

(defvar *websocket-server*)

(defvar *ws-pvs-connections* nil
  "Assigns a generated id to the connection")

(defvar *ws-thread-connections* nil
  "Keeps association between threads and connections")

(defvar *ws* nil "The websocket structure")

(defun start-pvs-server (&key (port 23456))
  (setq *websocket-server* (clack:clackup #'websocket-pvs-server :port port)))

(defun stop-pvs-server ()
  (clack:stop *websocket-server*)
  (hunchentoot:stop *websocket-server*))

(defun websocket-pvs-server (env)
  (let ((ws (wsd:make-server env)))
    (wsd:on :open ws (lambda () (ws-open ws)))
    (wsd:on :message ws (lambda (msg) (ws-message ws msg)))
    (wsd:on :close ws (lambda (&key code reason) (ws-close ws code reason)))
    (wsd:on :error ws (lambda (errmsg) (ws-error ws errmsg)))
    (set-pvs-hooks)
    (setq *ws* ws)
    (lambda (responder)
      (declare (ignore responder))
      (wsd:start-connection ws))))

;; (defun websocket-proof-server (env)
;;   (let ((ws (wsd:make-server env)))
;;     (wsd:on :open ws (lambda () (ws-proof-open ws)))
;;     (wsd:on :message ws (lambda (msg) (ws-proof-message ws msg)))
;;     (wsd:on :close ws (lambda (&key code reason) (ws-proof-close ws code reason)))
;;     (wsd:on :error ws (lambda (errmsg) (ws-proof-error ws errmsg)))
;;     (lambda (responder)
;;       (declare (ignore responder))
;;       (wsd:start-connection ws))))

(defun ws-open (conn)
  ;;(format t "~%ws-open called~%")
  (push (cons conn (gentemp "conn-")) *ws-pvs-connections*)
  (push (cons (bt:current-thread) conn) *ws-thread-connections*))

(defun ws-message (conn message)
  ;; (format t "~%Got WS message of type ~a from ~a:~%  ~a"
  ;;   (type-of message) (cdr (assoc conn *ws-pvs-connections*)) message)
  ;;(wsd:send conn (format nil "Got PVS message: ~a" message))
  (let ((pvs::*pvs-emacs-interface* nil))
    (if t
	(pvs-jsonrpc:process-jsonrpc message conn)
	(handler-case
	    (pvs-jsonrpc:process-jsonrpc message conn)
	  (error (c) (format t "~a" c))))))

(defun ws-close (conn code reason)
  (let ((telt (assoc (bordeaux-threads:current-thread) *ws-thread-connections*))
	(celt (assoc conn *ws-pvs-connections*)))
    (assert telt)
    (assert (eq (cdr telt) conn))
    (assert celt)
    (format t "~%Closing PVS connection for ~a, code: ~a, reason: ~a"
      (cdr (assoc conn *ws-pvs-connections*)) code reason)
    (setf *ws-thread-connections* (remove telt *ws-thread-connections*))
    (setf *ws-pvs-connections* (remove celt *ws-pvs-connections*))))

(defun ws-error (conn errmsg)
  ;; Happens when the connection is dropped without sending close
  (format t "~%Got error from ~a:~%  ~a" (assoc conn *ws-pvs-connections*) errmsg)
  ;; (defgeneric send (ws data &key start end type code callback))
  (wsd:send conn errmsg))

(defun ws-current-connection ()
  (cdr (assoc (bt:current-thread) *ws-thread-connections*)))

;; (defvar *ws-proof-connections* nil)

;; (defun ws-proof-open (conn)
;;   (format t "~%ws-proof-open called~%")
;;   (push (cons conn (gentemp "conn-")) *ws-pvs-connections*)
;;   (push (cons (bordeaux-threads:current-thread) conn) *ws-thread-connections*))

;; (defun ws-proof-message (conn message)
;;   (format t "~%Got proof message of type ~a from ~a:~%  ~a"
;;     (type-of message) (assoc conn *ws-pvs-connections*) message)
;;   ;;(wsd:send conn (format nil "Got PVS message: ~a" message))
;;   (format t "~%Got proof request from ~a:~%  ~a" (assoc con *ws-proof-connections*) json-request)
;;   (if t
;;       (wsd:send con (format nil "Got proof request: ~a" json-request))
;;       (handler-case
;; 	  (let* ((json:*json-identifier-name-to-lisp* #'identity)
;; 		 (req-str (pvs:bytestring-to-string json-request))
;; 		 (request (json:decode-json-from-string req-str))
;; 		 (id (assoc conn *ws-proof-connections*))
;; 		 (method (cdr (assoc :method request :test #'string-equal)))
;; 		 (params (cdr (assoc :params request :test #'string-equal)))
;; 		 (*print-pretty* nil))
;; 	    ;; (setq *last-request* (list method params id client-url))
;; 	    ;; (apply #'pvs-json:process-json-request pvs-xml-rpc::*last-request*)
;; 	    ;; (format t "~%xmlrpc-pvs-request: current-process = ~a~%   request = ~a~%"
;; 	    ;;   mp:*current-process* request)
;; 	    (let* ((result (pvs-json:process-json-request method params id client-url))
;; 		   (jresult (json:encode-json-to-string result)))
;; 	      ;; (format t "~%xmlrpc-pvs-request end: current-process = ~a~%   result = ~s~%"
;; 	      ;;   mp:*current-process* jresult)
;; 	      (wsd:send con jresult)))
;; 	;; Note: id will not be available if this error is hit
;; 	(error (c) (-error (format nil "~a" c))))))

;; (defun ws-proof-close (conn code reason)
;;   (let ((elt (assoc conn *ws-proof-connections*)))
;;     (format t "~%Closing proof connection for ~a:~%  code: ~a~%  reason: ~a"
;;       (cdr elt) code reason)
;;     (setf *ws-proof-connections* (delete elt *ws-proof-connections*))))

;; (defun ws-proof-error (conn errmsg)
;;   ;; Happens when the connection is dropped
;;   (format t "~%Got proof error from ~a:~%  ~a"
;;     (cdr (assoc conn *ws-proof-connections*)) errmsg)
;;   ;; (defgeneric send (ws data &key start end type code callback))
;;   (wsd:send conn errmsg))

;; In websocket-driver, a WebSocket connection is an instance of the ws
;; class, which exposes an event-driven API. You register event handlers by
;; passing your WebSocket instance as the second argument to a method called
;; on. For example, calling (on :message my-websocket
;; #'some-message-handler) would invoke some-message-handler whenever a new
;; message arrives.

;; The websocket-driver API provides handlers for the following events:

;;  :open: When a connection is opened. Expects a handler with zero arguments.
;;  :message When a message arrives. Expects a handler with one argument, the message received.
;;  :close When a connection closes. Expects a handler with two keyword
;;         args, a “code” and a “reason” for the dropped connection.
;;  :error When some kind of protocol level error occurs. Expects a handler
;;         with one argument, the error message.

;; On open, we set up all the hooks for
;;  pvs-error (msg err &optional file-name place)
;;  pvs-message (str)
;;  pvs-warning (str)
;;  pvs-buffer (name contents &optional display? read-only? append? kind)
;;  pvs-y-or-n (msg &optional yesno-p timeout-p)
;;  pvs-query (prompt &rest args)
;;  pvs-dialog (prompt &rest args)
;; This is needed in order to send notifications, etc. to the corresponding WS connection.
;; so on open, we set up all the hooks, and remove them on close.

;; When a pvs-message is invoked, it needs to know which connection it's
;; associated with, if any. Since each connection is associated with a
;; unique thread, We store the (thread . connection) pair in
;; *ws-thread-connections* When, e.g., pvs-message is invoked, the hook looks
;; up the connection from the current-thread, and if it is there, uses it to
;; send a notification. Hence the server sets up the hooks, and open and close maintain
;; *ws-thread-connections*.

(defun set-pvs-hooks ()
  ;;(setf pvs:*pvs-error-hook* #'ws-pvs-error-hook)
  (setf pvs:*pvs-message-hook* #'ws-pvs-message-hook)
  ;; (setf pvs:*pvs-warning-hook* #'ws-pvs-warning-hook)
  ;; (setf pvs:*pvs-buffer-hook* #'ws-pvs-buffer-hook)
  ;; (setf pvs:*pvs-y-or-n-hook* #'ws-pvs-y-or-n-hook)
  ;; (setf pvs:*pvs-query-hook* #'ws-pvs-query-hook)
  ;; (setf pvs:*pvs-dialog-hook* #'ws-pvs-dialog-hook)
  )

;; (defun ws-pvs-error-hook (msg err &optional file-name place)
;;   (let ((conn (cdr (assoc (bordeaux-threads:current-thread) *ws-thread-connections*))))
;;     (unless conn
;;       (format t "~%ws-pvs-error-hook: couldn't find thread ~a in *ws-thread-connections*~%"
;; 	(bordeaux-threads:current-thread)))
;;     ;; Need to be careful here; this could be a result of a request, in
;;     ;; which case we need to make sure it gets returned correctly.
;;     ;; Errors should be handled by the condition-handler of the open event
;;     (when conn
;;       (let ((jerror (json:encode-json-to-string
;; 		     `((:error . ((:code . -32700) (:message . "PVS Error") (:data . ,err)))
;; 		       (:id . nil) (:jsonrpc . "2.0")))))
;; 	(websocket-driver:send conn jerr)
;; 	(format t "~%ws-pvs-error-hook: shouldn't be here~%")))))
    
(defun ws-pvs-message-hook (msg)
  (unless (string= (bt:thread-name (bt:current-thread))
		   "Initial Lisp Listener")
    ;; (format t "~%ws-pvs-message-hook: ~a" msg)
    (assert (stringp msg))
    (let ((conn (cdr (assoc (bordeaux-threads:current-thread) *ws-thread-connections*))))
      (unless conn
	(format t "~%ws-pvs-message-hook: couldn't find thread ~a in *ws-thread-connections*~%"
	  (bordeaux-threads:current-thread)))
      (when conn
	(let* ((notif `((:method . :pvs-message)
			(:params .  (,msg))))
	       (jnotif (json:encode-json-alist-to-string notif)))
	  (wsd:send conn jnotif))))))

