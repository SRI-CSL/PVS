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

(defmethod xmlrpc-theory-decl* ((decl pvs:inline-recursive-type))
  `((:id . ,(id decl))
    (:kind . ,(pvs:kind-of decl))
    (:constructors . ,(mapcar #'xmlrpc-theory-decl*
			(constructors decl)))
    (:place . ,(pvs:place-list decl))))

(defmethod xmlrpc-theory-decl* ((decl pvs:adt-constructor))
  `((:id . ,(pvs:id decl))
    (:recognizer . ,(pvs:recognizer decl))
    (:accessors . ,(mapcar #'xmlrpc-theory-decl*
		     (pvs:arguments decl)))
    (:place . ,(pvs:place-list decl))))

(defmethod xmlrpc-theory-decl* ((decl pvs:adtdecl))
  `((:id . ,(pvs:id decl))
    (:type . ,(pvs:str (pvs:type decl)))
    (:place . ,(pvs:place-list decl))))

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
      ,@(when (pvs:generated-by decl)
	  `((:generated-by . ,(pvs:generated-by decl))))
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

;;; This is tricky, RPC is really for function calls, but this is an
;;; interactive session.  This function is being called from a particular
;;; XML-RPC thread.  We can't start the prover on this thread, as later
;;; commands will come in on other threads.

;;; The tricky thing here is that we are running the prover in the initial
;;; thread, from an XML-RPC thread.  

(defrequest prove-formula (formula theory &optional rerun?)
  "Starts interactive proof of a formula from a given theory

Creates a ps-control-info struct to control the interaction.  It has slots
  command
  json-result
  lock
  cmd-gate
  res-gate
"
  ;; We do this in this thread, as error messages are easier to deal with.
  ;; Thus we make sure we're not in the checker, that the theory typechecks,
  ;; and that the formula exists in that theory.
  (when pvs:*in-checker*
    (pvs-error "Prove-formula error" "Must exit the prover first"))
  (pvs:get-formula-decl theory formula)
  ;;   ;; FIXME - may want to save the proof, or ask what to do
  ;;   (format t "~%About to quit prover~%")
  ;;   (throw 'pvs:quit nil)
  ;;   (format t "~%After quitting prover~%"))
  (let ((res-gate (mp:make-gate nil))
	(cmd-gate (mp:make-gate nil))
	(lock (mp:make-process-lock))
	(proc (mp:process-name-to-process "Initial Lisp Listener")))
    (setq pvs:*multiple-proof-default-behavior* :noquestions)
    (setq pvs:*prover-commentary* nil)
    (setq pvs:*ps-control-info*
	  (pvs:make-ps-control-info
	   :lock lock
	   :res-gate res-gate
	   :cmd-gate cmd-gate))
    ;;(format t "~%prove-formula: Waiting...~%")
    ;; (mp:process-interrupt proc #'pvs:prove-formula theory formula nil)
    ;; process-interrupt interrupts the main pvs process proc, and invokes
    ;; prove-formula
    (mp:process-interrupt proc #'pvs:prove-formula theory formula rerun?)
    (mp:process-wait "Waiting for initial Proofstate" #'mp:gate-open-p res-gate)
    ;;(format t "~%prove-formula: Done waiting...~%")
    (mp:with-process-lock (lock)
      (let ((json-result (pvs:psinfo-json-result pvs:*ps-control-info*)))
	;;(format t "~%prove-formula: returning json-result ~a~%" json-result)
	(setf (pvs:psinfo-json-result pvs:*ps-control-info*) nil)
	(mp:close-gate (pvs:psinfo-res-gate pvs:*ps-control-info*))
	json-result))))
  

;;; This can't work, as it never returns from pvs:prove-formula till the
;;; prover quits.
;; (defrequest prove-formula2 (formula theory &optional rerun?)
;;   "Starts interactive proof of a formula from a given theory"
;;   (let ((pvs:*multiple-proof-default-behavior* :noquestions))
;;     (setq pvs:*prover-commentary* nil)
;;     (pvs:prove-formula theory formula rerun?)))

(defrequest proof-command (form)
  "Sends a command to the prover"
  (assert (not (mp:gate-open-p (pvs:psinfo-cmd-gate pvs:*ps-control-info*))))
  (assert (null (pvs:psinfo-command pvs:*ps-control-info*)))
  (assert (not (mp:gate-open-p (pvs:psinfo-res-gate pvs:*ps-control-info*))))
  (assert (null (pvs:psinfo-json-result pvs:*ps-control-info*)))
  (multiple-value-bind (input err)
      (ignore-errors (read-from-string form))
    ;; note that read-from-string returns multiple vales;
    ;; err will be the number of chars read in this case.
    (cond ((typep err 'error)
	   (let ((com (format nil "bad proof command: ~a"
			(if (typep err 'end-of-file)
			    "eof encountered, probably mismatched parens, quotes, etc."
			    err)))
		 (ps (mp:symeval-in-process 'pvs:*ps* *pvs-lisp-process*))
		 (json:*lisp-identifier-name-to-json* #'identity))
	     (setq pvs:*prover-commentary* (list com))
	     (unwind-protect
		  (pvs:pvs2json ps)
	       (setq pvs:*prover-commentary* nil))))
	  ((and (consp input)
		;; check-arguments sets *prover-commentary*
		(not (pvs:check-arguments input)))
	   (unwind-protect
		(let* ((ps (mp:symeval-in-process 'pvs:*ps* *pvs-lisp-process*))
		       (json:*lisp-identifier-name-to-json* #'identity))
		  (pvs:pvs2json ps))
	     (setq pvs:*prover-commentary* nil)))
	  (t 
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
	       psjson))))))

(defrequest prover-status ()
  "Checks the status of the prover: active or inactive."
  (let ((top-ps (mp:symeval-in-process '*top-proofstate* *pvs-lisp-process*))
	(ps (mp:symeval-in-process '*top-proofstate* *pvs-lisp-process*)))
    (pvs:prover-status top-ps ps)
    ;; (pvs:prover-status (sys:global-symbol-value 'pvs:*ps*)
    ;; 		     (sys:global-symbol-value 'pvs:*top-proofstate*)
    ;; 		     (sys:global-symbol-value 'pvs:*last-proof*))
    ))

(defrequest proof-status (formref &optional formname)
  "Checks the status of the given formula, proved, unchecked, unfininshed,
or unproved."
  (pvs:get-proof-status formref formname))

(defrequest proof-script (fname formula)
  "Returns the proof script, as with the show-proof command."
  (pvs:get-proof-script fname formula))

(defrequest show-tccs (fname)
  "Returns the tccs, as with the show-tccs command."
  (pvs:get-tccs fname))


;;; In PVS, there are frequent references to workspaces, files, theories,
;;; declarations, etc.  Toward this end a pvsref will be defined as one of:
;;; 1. a string, when a file (with workspace and extension) is referenced.
;;;    - workspace is optional and defaults to current
;;;    - extension is optional and defaults to ".pvs"
;;; 2. a JSON object (alist in Lisp) with fields
;;;   pathName: as above, the usual dir/file.ext string.
;;;   workspace: directory string
;;;   fileName:  filename string without extension
;;;   fileExtension: string
;;;   theoryName: string
;;;   declName: string
;;;   place: number or list of numbers

;;; Obviously, giving a pathName and a fileName together is unnecessary;
;;; similarly for other fields.

;;; Using Paolo's JSON objects, with fields contextFolder, fileName,
;;; fileExtension, theoryName, formulaName, location, rerun.

;;; 1. Prefer 'workspace' to 'contextFolder'

;;; 2. Allow pathName form: ;;; "dir/file.pvs", and leave out workspace,
;;;    fileName, fileExtension.

;;; 3. Also allow URL form: "dir/file.pvs#thry#decl" or
;;;    "dir/file.pvs#place", which are simple strings.

;;; Thus a PVS reference accept JSON objects with fields

(defun json-ref-to-string (thref)
  (typecase thref
    (cons ;; should be assoc list with cars
     ;; :fileName, :fileExtension, :theoryName, :contextFolder
     (unless (assq :fileName thref)
       (error "bad thref: ~a" thref))
     (format nil "~a/~a~a#~a"
       (cdr (assq :contextFolder thref))
       (cdr (assq :fileName thref))
       (cdr (assq :fileExtension thref))
       (cdr (assq :theoryName thref))))
    (pathname (namestring thref))
    (string thref)))

(defrequest prove-tccs (fname)
  "Attempts proof of TCCs, as with then typecheck-prove (tcp) command."
  (pvs:prove-tccs fname))

(defrequest term-at (file place &optional typecheck?)
  "Returns information about the term at the specific place, a (row, col)
tuple.  The file will be parsed, if necessary, and typechecked if the
typecheck? flag is set.  Note that the type can only be provided if it is
typechecked, and unless typechecked, identifiers only give a best guess as
to the associated declaration."
  (when (stringp place)
    (setq place (read-from-string place nil :eof)))
  (unless (and (listp place)
	       (member (length place) '(2 4) :test #'=)
	       (every #'integerp place))
    (error "term-at: Bad place, should be list of 2 or 4 nats"))
  (let* ((loc1 (list (car place) (cadr place)))
	 (loc2 (if (caddr place)
		   (list (caddr place) (cadddr place))
		   loc1)))
    (multiple-value-bind (term containing-terms)
	(pvs:get-term-at file loc1 loc2 typecheck?)
      (json-term term))))

(defun json-term (term)
  (json-term* term))

(defmethod json-term* ((term module))
  `((:kind . :theory)
    (:id . ,(id term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term recursive-type))
  `((:kind . :recursive-type)
    (:id . ,(id term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term declaration))
  (pvs:json-decl-list term (pvs:ptype-of term) (module term)))

(defmethod json-term* ((term type-expr))
  `((:kind . :type)
    (:string . ,(str term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term expr))
  `((:kind . :expr)
    (:string . ,(str term))
    (:place . ,(place-list (place term)))))
