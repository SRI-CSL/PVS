;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-json.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Jun  6 11:05:15 2012
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 18 21:06:29 2012
;; Update Count    : 5
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2012, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;; (import '(excl:def-fwrapper excl:call-next-fwrapper excl:fwrap
;; 			    excl:funwrap excl:fwrap-order))

(defvar *json-id* nil)

;; Based loosely on json-rpc
;; requests are of the form
;;  {"method": "function", "params": [param1, ..., paramn], "id": id}
;; where function is the name of a lisp function, params and id are optional.
;; The optional params are themselves JSON forms; in particular, a function
;; such as
;; (defun foo (a &optional b &key c) ...)
;; could have parameters [1], [1, 2], [1, {"c": 3}], [1, 2, {"c": 3}]
;; The id is optional, if it is included then a result will be returned
;; with a corresponding id.  If the id is missing, no response is returned
;; (this is a json-rpc notification).
;; A response is of the form
;; {"result": result, "error": error, "id": id}
;; where one and only one of result or error is included, and id is required.
;; result and error are arbitrary json forms.

(defvar *pvs-json-initialized* nil)

(defun pvs-init-json ()
  (unless *pvs-json-initialized*
    (pushnew 'output-json-proofstate *proofstate-hooks*)
    (pushnew 'json-pvs-buffer *pvs-buffer-hooks*)
    (pushnew 'json-pvs-message *pvs-message-hooks*)
    (pushnew 'json-pvs-error *pvs-error-hooks*)
    (setq *pvs-json-initialized* t)))

(defun pvs-json (json-string)
  (pvs-init-json)
  (let* ((request (json:decode-json-from-string json-string))
	 (*pvs-interface* 'json)
	 (*json-id* (cdr (assq :ID request)))
	 (cmdstr (cdr (assq :METHOD request)))
	 (params (cdr (assq :PARAMS request)))
	 (*print-pretty* nil))
    (multiple-value-bind (cmd errstr)
	(json-check-form cmdstr "request")
      (cond (errstr
	     (json-error errstr))
	    ((not (symbolp cmd))
	     (json-error "cmd ~a must be a symbol string"))
	    ((not (listp params))
	     (json-error (format nil "parameters ~a must be a list" params)))
	    (t (multiple-value-bind (result errstr)
		   (json-eval-form (cons cmd params))
		 (if errstr
		     (json-error errstr)
		     (json-result result))))))))

(defun json-error (err &optional (id *json-id*))
  ;; No id means this was a notification - just drop the error
  (when id
    (let ((jerr (json:encode-json-alist-to-string
		 `((:error . ,(json:encode-json-to-string err))
		   (:id . ,id)))))
      (setq *json-id* nil)
      ;; The beginning and end braces must stand alone, making them easy to find
      (format t "~%{~%~a~%}~%" (subseq jerr 1 (1- (length jerr)))))))

(defun json-result (result &optional (id *json-id*))
  (when id
    (let* ((sresult (json:encode-json-to-string result))
	   (jresult (json:with-explicit-encoder
		     (json:encode-json-to-string
		      (cons :object
			    `((:result . ,sresult)
			      (:id . ,id)))))))
      (setq *json-id* nil)
      ;; The beginning and end braces must stand alone, making them easy to find
      (format t "~%{~%~a~%}~%" (subseq jresult 1 (1- (length jresult)))))))

(defun json-check-form (str key)
  (multiple-value-bind (form err)
      (ignore-errors (read-from-string str))
    (if (typep err 'error)
	(values nil
		(json-error 
		 (format nil "Bad ~a \\\"~a\\\": ~a"
		   key
		   (protect-emacs-output str)
		   (protect-emacs-output (format nil "~a" err)))))
	form)))

(defun json-eval-form (form)
  (if (or *in-checker* *in-evaluator*)
      form
      (eval form)))

(defmethod json:encode-json ((obj datatype-or-module) &optional stream)
  (format stream "\"~a\"" (id obj)))

(defun json-all-theories-info (&optional file prelude?)
  (if (null file)
      (let ((theory-alist nil))
	(maphash #'(lambda (id th)
		     (push (json-file-theories-info id th)
			   theory-alist))
		 (if prelude? *prelude* *pvs-modules*))
	theory-alist
	;;theory-alist
	)
      (json-pvs-file-info file)))

(defun json-pvs-file-info (file)
  (assert (stringp file))
  (list (cons 'file file)
	(cons 'theories
	      (mapcar #'json-pvs-theory-info (cdr (gethash file *pvs-files*))))))

(defun json-pvs-theory-info (th)
  (list (cons 'id (id th))
	(cons 'declarations
	      (mapcar #'(lambda (d)
			  (list (cons 'id (decl-id d))
				(cons 'kind (class-name (class-of d)))
				(cons 'place (or (place d) 'None))))
		(all-decls th)))))

(defun json-typecheck-file (filename &optional forced? prove-tccs?
				     importchain? nomsg?)
  (let ((theories (typecheck-file filename forced? prove-tccs?
				  importchain? nomsg?)))
    (list (cons 'theories theories))))

;;; Writes out a message.  The message should fit on one line, and
;;; should contain no newlines.  For Emacs, it is intended to write to
;;; the minibuffer.

(defun json-notification (method params)
  (let* ((explist (cons :object
			`((:method . ,method)
			  ,@(when params (list (cons :params
						     (if (consp params)
							 (cons :array params)
							 (list :array params))))))))
	 (jnotif (json:with-explicit-encoder
		  (json:encode-json-to-string explist))))
    (format t "~%{~%~a~%}~%" (subseq jnotif 1 (1- (length jnotif))))))

(defun json-pvs-message (string)
  (json-notification :pvsmessage (list string)))

(defun json-pvs-buffer (name contents display? read-only? append? kind)
  (json-notification
   :pvsbuffer (list (list :object
			  (cons :name name)
			  (cons :contents contents)
			  (cons :append append?)
			  (cons :kind kind)))))
   
(defun json-pvs-error (kind err itheory iplace)
  (json-notification
   :pvserror (list :object
		   (cons :kind kind)
		   (cons :error err)
		   (cons :theory itheory)
		   (cons :place iplace))))


;;; The interactive prover and ground evaluator require special treatment.
;;; As lisp functions, they only return a value when they are exited, but
;;; from the client perspective, it should return a value for each command.
;;; To do this, there is a hook in the prover to call the output-proofstate
;;; method, that simply prints the proofstate as usual.  In interface.lisp
;;; is an around method that checks if *pvs-interface* is set (e.g., to json)
;;; and calls output-proofstate* accordingly.

(defun json-prove-formula (theory formula &optional rerun?)
  (if (or *in-checker* *in-evaluator*)
      (json-error (if *in-checker*
		      (format nil "Currently proving ~a"
			(id (declaration *top-proofstate*)))
		      "Currently in the ground evaluator"))
      (let ((*current-json-ps* nil))
	(prove-formula theory formula rerun?)
	nil)))

(defmethod prover-read* ((ifc (eql 'json)))
  (let ((cmd (ignore-errors (read))))
    (cond ((and (listp cmd) (eq (car cmd) 'pvs-json))
	   (apply #'pvs-json (cdr cmd)))
	  ((and (listp cmd) (eq (car cmd) 'json-prove))
	   (let* ((request (json:decode-json-from-string (cadr cmd)))
		  (json-id (cdr (assq :ID request)))
		  (cmdstr (cdr (assq :METHOD request)))
		  (params (cdr (assq :PARAMS request))))
	     (setq *json-id* json-id)
	     (cons (intern cmdstr :pvs) params)))
	  (t cmd))))

(defun proofstate-yields (proofstate)
  (let* ((ps (parent-proofstate proofstate))
	 (status (when ps (status-flag ps))))
    (when (eq status '?)
      (cond ((> (length (remaining-subgoals ps)) 1)
	     (format nil "~%this yields  ~a subgoals: "
	       (length (remaining-subgoals post-proofstate))))
	    ((not (typep (car (remaining-subgoals ps))
			 'strat-proofstate))
	     (format nil "~%this simplifies to: "))))))

(defun proofstate-printout (proofstate)
  (let* ((ps (parent-proofstate proofstate))
	 (printout (when ps (printout ps))))
    (when printout
      (apply #'format nil (car printout)
	     (mapcar #'(lambda (x)
			 (if (stringp x)
			     (protect-format-string x)
			     x))
	       (cdr printout))))))

(defvar *current-json-ps* nil)

(defun output-json-proofstate (ps)
  (unless *in-apply*
    (let ((changed (or (null *current-json-ps*)
		       (not (eq *current-json-ps* ps)))))
      (when changed
	(if *json-id*
	    (let* ((json-id *json-id*)
		   (par-sforms (when (parent-proofstate ps)
				 (s-forms (current-goal
					   (parent-proofstate ps)))))
		   (comment (comment ps))
		   (printout (proofstate-printout ps))
		   (yields (proofstate-yields ps))
		   (label (label ps))
		   (sequent (json-sequent (current-goal ps) par-sforms))
		   (result (cons :object
				 (nconc (when printout `((:printout . ,printout)))
					(when yields `((:yields . ,yields)))
					`((:label . ,label))
					(when comment `((:comment . ,comment)))
					`((:sequent . ,sequent))))))
	      (setq *current-json-ps* ps)
	      (json-result result json-id)))))))

(defun json-sequent (sequent par-sforms)
  (let ((antes (json-sforms (neg-s-forms sequent) -1 par-sforms))
	(succs (json-sforms (pos-s-forms sequent) 1 par-sforms)))
    (cons :object
	  `(,@(when antes (list (cons :antecedents (cons :list antes))))
	    ,@(when succs (list (cons :succedents (cons :list succs))))))))

(defun json-sforms (sforms num par-sforms &optional jforms)
  (if (null sforms)
      (nreverse jforms)
      (let* ((changed (not (memq (car sforms) par-sforms)))
	     (jform (json-sform (car sforms) num changed)))
	(json-sforms (cdr sforms) (if (minusp num) (1- num) (1+ num))
		     par-sforms (cons jform jforms)))))

(defun json-sform (sform num changed)
  (let ((fstring (format nil "~a" sform)))
    (setf (view sform) (make-view :string fstring))
    (cons :object
	  (list (cons :fnum num)
		(cons :formula (format nil "~a" sform))
		(cons :changed changed)))))
  

;; (defmethod json:encode-json ((obj syntax) &optional (stream *json-output*))
;;   (json:encode-json (list "foo") stream))
