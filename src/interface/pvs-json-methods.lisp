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

(in-package :pvs-jsonrpc)

;;; The requests

(defrequest list-methods ()
  "List all available methods"
  (sort (mapcar #'(lambda (r) (string-downcase (car r))) *pvs-request-methods*) #'string-lessp))

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

(defrequest typecheck (filename &optional force?)
  "Typecheck a file"
  (format t "~%Request tc ~s" filename)
  (let ((theories (pvs:typecheck-file filename force?)))
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
  `((:id . ,(pvs:id decl))
    (:kind . ,(pvs:kind-of decl))
    (:constructors . ,(mapcar #'xmlrpc-theory-decl*
			(pvs:constructors decl)))
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
      ;;(:class . ,(class-name (class-of decl)))
      (:type . ,(pvs:str (pvs:type decl)))
      ,@(when (pvs:generated-by decl)
	  `((:generated-by . ,(pvs:ref-to-id (pvs:generated-by decl)))))
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
  #+allegro
  (let ((proc (mp:process-name-to-process "Initial Lisp Listener")))
    (mp:process-interrupt proc #'pvs:pvs-abort))
  #-allegro
  (break "Fix me"))

;; Modified by M3 to allow restore to Rule? prompt when in-checker [Sept 2020]
(defrequest interrupt ()
  "Interrupts PVS."
  (break "interrupt")
  )

;;; Prover interface

(defrequest prove-formula (formula-ref &optional rerun?)
  "Starts interactive proof of a formula from a given theory.  First it
determines the formula declaration, then creates a new proof-session,
returning the unique id (within a PVS session)."
  (format t "~%prove-formula: ~s" formula-ref)
  (let* ((fdecl (pvs:get-formula-decl formula-ref)))
    (format t "~%  decl = ~s" fdecl)
    (let* ((prf-result (pvs:prover-init fdecl rerun?))
	   (json:*lisp-identifier-name-to-json* 'identity))
      (pvs2json-response prf-result))))

(defun pvs2json-response (prv-result)
  "prv-result is an alist of the form ((:proofstate ...) (:log ...) (:id ...) (:status ...)."
  (let* ((id (cdr (assoc :id prv-result)))
	 (ps (cdr (assoc :proofstate prv-result)))
	 (log (cdr (assoc :log prv-result)))
	 (status (cdr (assoc :status prv-result)))
	 (ps-json (pvs2json-ps ps log id status)))
    (format t "~%pvs2json-prover-result:~%  ~s~%" ps-json)
    ps-json))

(defmethod pvs2json-ps (ps log id status)
  (with-slots (label comment current-goal (pps parent-proofstate)) ps
    (let* ((pps (parent-proofstate ps))
	   (action (when pps (strim (format-printout pps t))))
	   (num-subgoals (proofstate-num-subgoals ps))
	   (sequent (pvs2json-seq (current-goal ps) pps))
	   (prev-cmd (let ((wish-rule (wish-current-rule ps)))
		       (cond (wish-rule (format nil "~s" wish-rule))
			     (pps (format nil "~s" (current-rule pps)))
			     (t nil))))
	   (commentary (mapcar #'(lambda (e)
				   (strim (if (stringp e) e (format nil "~a" e))))
			 log)))
      `(("id" . ,id)
	("status" . ,status)
	,@(when commentary
	    `(("commentary" . ,commentary)))
	,@(when action `(("action" . ,action)))
	,@(when num-subgoals `(("num-subgoals" . ,num-subgoals)))
	("label" . ,(label ps))
	,@(when prev-cmd `( ;;("last-cmd" . ,prev-cmd)
			   ("prev-cmd" . ,prev-cmd)))
	,@(when (comment ps) `(("comment" . ,(comment ps))))
	("path" . ,(format nil "~{~a~^.~}" (path-from-top ps)))
	("sequent" . ,sequent)))))

(defstruct seqstruct
  antecedents
  succedents
  hidden-antecedents
  hidden-succedents
  info)

(defmethod json:encode-json ((ss seqstruct) &optional (stream json:*json-output*))
  (json:with-object (stream)
    (when (seqstruct-antecedents ss)
      (json:as-object-member ("antecedents" stream)
	(json:encode-json (seqstruct-antecedents ss) stream)))
    (when (seqstruct-succedents ss)
      (json:as-object-member ("succedents" stream)
	(json:encode-json (seqstruct-succedents ss) stream)))
    (when (seqstruct-hidden-antecedents ss)
      (json:as-object-member ("hidden-antecedents" stream)
	(json:encode-json (seqstruct-hidden-antecedents ss) stream)))
    (when (seqstruct-hidden-succedents ss)
      (json:as-object-member ("hidden-succedents" stream)
	(json:encode-json (seqstruct-hidden-succedents ss) stream)))))

(defmethod pvs2json-seq (seq parent-ps)
  (let* ((par-sforms (when parent-ps
		       (s-forms (current-goal parent-ps))))
	 (hidden-s-forms (hidden-s-forms seq))
	 (hn-sforms (neg-s-forms* hidden-s-forms))
	 (hp-sforms (pos-s-forms* hidden-s-forms)))
    (make-seqstruct 
     :antecedents (pvs2json-sforms (neg-s-forms seq) t par-sforms)
     :succedents (pvs2json-sforms (pos-s-forms seq) nil par-sforms)
     :hidden-antecedents (pvs2json-sforms hn-sforms t par-sforms)
     :hidden-succedents (pvs2json-sforms hp-sforms nil par-sforms)
     :info (info seq))))

(defun pvs2json-sforms (sforms neg? par-sforms)
  (let ((c 0))
    (mapcar #'(lambda (sf)
		(let* ((fnum (if neg? (- (incf c)) (incf c))))
		  (pvs2json-sform sf fnum par-sforms)))
      sforms)))

;; Note that this has the side effect of setting the view of the sform,
;; Which is a cons of the string and its view (computed lazily).
(defun pvs2json-sform (sform fnum par-sforms)
  (let* ((nf (formula sform))
	 (frm (if (negation? nf) (args1 nf) nf)))
    (unless (view sform)
      (multiple-value-bind (frmstr frmview)
	  (pp-with-view frm *proofstate-indent* *proofstate-width*)
	(setf (view sform) (list frmstr frmview))))
    (let ((names-info (names-info-proof-formula sform)))
      `(("labels" . ,(cons fnum (label sform)))
	("changed" . ,(if (memq sform par-sforms) "false" "true"))
	("formula" . ,(car (view sform)))
	("names-info" . ,names-info)))))

;; It gets confusing when qread from the prover uses *standard-input*,
;; instead we use bordeaux-thread condition variables to wait for entries in
;; the queue. After this, proof-command will find this entry, add to the queue, and
;; call bt:condition-notify

(defrequest proof-command (proof-id form)
  "Sends a command to the prover thread."
  (format t "~%proof-command: ~a~%  ~a" proof-id form)
  (let* ((prf-result (pvs:prover-step proof-id form))
	 (json:*lisp-identifier-name-to-json* 'identity))
    (pvs2json-response prf-result)))

(defrequest prover-status ()
  "Checks the status of the proof sessions: active or inactive.
Returns a list of the form ((id . status) (id . status) ...)"
  (or (pvs:prover-status)
      :null))

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
     (unless (assoc :fileName thref)
       (error "bad thref: ~a" thref))
     (format nil "~a/~a~a#~a"
       (cdr (assoc :contextFolder thref))
       (cdr (assoc :fileName thref))
       (cdr (assoc :fileExtension thref))
       (cdr (assoc :theoryName thref))))
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
      (declare (ignore containing-terms)) ; might be useful later
      (json-term term))))

;; M3: Request to save proofs to prf file [Sept 2020]
(defrequest save-all-proofs (theoryref)
  "Stores the declaration proofs into the corresponding PRF file"
  (let ((theory (pvs:get-typechecked-theory theoryref)))
    (unless theory
      (pvs:pvs-error "Save-all-proofs error" (format nil "Theory ~a cannot be found" theoryref)))
    (pvs:save-all-proofs theory)))

;; M3: Request to store the script for the last attempted proof into the corresponding declaration [Sept 2020]
(defrequest store-last-attempted-proof (formula theory &optional overwrite? new-script-id new-script-desc)
  "Store the last attempted proof script in the provided formula, only if the script was produced for it."
  (unless pvs:*last-attempted-proof*
    (pvs-error "store-last-attempted-proof error" "There is no attempted proof script to be saved."))
  (error "store-last-attempted-proof called")
  (let ((dst-decl (pvs:get-formula-decl theory formula)))
    (if (equal dst-decl (car pvs:*last-attempted-proof*))
	(let ((script (cdr pvs:*last-attempted-proof*)))
	  (if overwrite?
	      (setf (pvs:script (pvs:default-proof dst-decl)) (car script))
	    (let ((id (or new-script-id (pvs:next-proof-id dst-decl)))
		  (description (or new-script-desc "")))
	      (setf (pvs:default-proof dst-decl)
		    (pvs:make-default-proof dst-decl (car script) id description)))))
      (pvs-error "store-last-attempted-proof error"
		 (format nil "Last attempted proof script was not meant for provided decl (script attempted for ~a, decl provided is ~a)."
			 (car pvs:*last-attempted-proof*) dst-decl)))))

(defrequest add-pvs-library (string)
  "Just evaluate the string in lisp"
  (let ((*package* (find-package :pvs)))
    (push string pvs::*pvs-library-path*)))


(defrequest get-proof-scripts (pvsfilename)
  "Returns the proof scripts extracted from the .prf file associated with the given filename.

Given a pvsfilename string, which may include an absolute or relative (to
current workspace) directory, and may or may not include the .pvs extension,
Returns JSON of the form:
 { \"pvsFile\": \"foo.pvs\",
   \"theoryScripts\": [{ \"theoryId\" : \"thid1\",
                         \"formulaScripts\" : [{ \"formulaId\": \"fid\",
                                                \"formulaScript\" : \"script\" },
                                              ... }]
                       ... }]
 }
"
  (pvs:get-proof-scripts pvsfilename))

(defun json-term (term)
  (json-term* term))

(defmethod json-term* ((term pvs:module))
  `((:kind . :theory)
    (:id . ,(id term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term pvs:recursive-type))
  `((:kind . :recursive-type)
    (:id . ,(id term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term pvs:declaration))
  (pvs:json-decl-list term (pvs:ptype-of term) (module term)))

(defmethod json-term* ((term pvs:type-expr))
  `((:kind . :type)
    (:string . ,(str term))
    (:place . ,(place-list (place term)))))

(defmethod json-term* ((term pvs:expr))
  `((:kind . :expr)
    (:string . ,(str term))
    (:place . ,(place-list (place term)))))

(defmethod json:encode-json (any &optional (stream json:*json-output*))
  ;; Just return nil in this case, there's a problem on some Macs that this deals with.
  (declare (ignore any stream))
  nil)
