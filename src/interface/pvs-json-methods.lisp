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
	`(("argspec" ,argspec)
	  ("docstring" . ,docstring))
	(error "~a not found" methodname))))

(defrequest lisp (string)
  "Just evaluate the string in lisp"
  (let ((*package* (find-package :pvs)))
    (let ((result (eval (read-from-string string))))
      (when result (format nil "~a" result)))))

(defrequest change-workspace (dir)
  "Change PVS workspace"
  (pvs:change-workspace dir))

(defrequest clear-workspace (&optional workspace
				       empty-pvs-context
				       delete-binfiles
				       dont-load-prelude-libraries)
  "Clear the workspace(s)"
  (pvs:clear-workspace
   :workspace (cond ((member workspace '("all" ":all" "t") :test #'string-equal)
		     "all")
		    ((string-equal workspace "nil") nil)
		    (t workspace))
   :empty-pvs-context (unless (string-equal empty-pvs-context "nil") t)
   :delete-binfiles (unless (string-equal delete-binfiles "nil") t)
   :dont-load-prelude-libraries (unless (string-equal dont-load-prelude-libraries "nil") t)))

(defrequest change-context (dir)
  "Change PVS workspace"
  ;;(format t "~%change-context: ~s" dir)
  (pvs:change-workspace dir))

(defrequest parse (filename)
  "Parse a file"
  (let ((theories (pvs:parse-file filename)))
    (pvs:save-context)
    (json-theories theories)))

(defrequest typecheck (filename &optional content force?)
  "Typecheck a file"
  ;; (format t "~%Request tc ~s force? ~s" filename force?) ;; debug
  (when (and content (< 0 (length content)))
    ;; Try to write the file
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (write content :stream out :escape nil)))
  (let* ((theories (pvs:typecheck-file filename force?))
	 (jtheories (json-theories theories)))
    ;; (format t "~%Saving context for tc ~s" filename)
    ;;(pvs:save-context)
    ;;(format t "~%Returning theories for ~s" filename)
    jtheories))

(defun json-theories (theories)
  (json-theories* theories nil))

(defun json-theories* (theories &optional thstructs)
  (if (null theories)
      (nreverse thstructs)
      (let ((thstruct (json-theory (car theories))))
	(json-theories* (cdr theories) (cons thstruct thstructs)))))

(defun json-theory (theory)
  `(("id" . ,(string (pvs:id theory)))
    ("fileName" . ,(let ((fname (pvs::pvs-filename theory)))
			   (if (and (> (length fname) 4)
				    (string= (subseq fname (- (length fname) 4)) ".pvs"))
			       fname
			       (format nil "~a.pvs" fname))))
    ("decls" . ,(json-theory-decls (pvs:all-decls theory)))))

(defun json-theory-decls (decls &optional thdecls)
  (if (null decls)
      (nreverse thdecls)
      (let ((thdecl (json-theory-decl (car decls))))
	(json-theory-decls
	 (cdr decls)
	 (if thdecl
	     (cons thdecl thdecls)
	     thdecls)))))

(defvar *idgensymctr*)

(defun json-theory-decl (decl)
  (let ((*idgensymctr* nil))
    (json-theory-decl* decl)))

(defmethod json-theory-decl* ((decl pvs:var-decl))
  nil)

(defmethod json-theory-decl* ((decl pvs:inline-recursive-type))
  (obj `(("id" . ,(string (pvs:id decl)))
	 ("kind" . ,(string-downcase (pvs:kind-of decl)))
	 ("constructors" . ,(arr (mapcar #'json-theory-decl*
				   (pvs:constructors decl))))
	 ("place" . ,(pvs:place-list decl)))))

(defmethod json-theory-decl* ((decl pvs:adt-constructor))
  (obj `(("id" . ,(string (pvs:id decl)))
	 ("recognizer" . ,(string (pvs:recognizer decl)))
	 ,@(when (pvs:arguments decl)
	     `(("accessors" . ,(arr (mapcar #'json-theory-decl*
				      (pvs:arguments decl))))))
	 ("place" . ,(pvs:place-list decl)))))

(defmethod json-theory-decl* ((decl pvs:adtdecl))
  (obj `(("id" . ,(string (pvs:id decl)))
	 ("type" . ,(pvs:str (pvs:type decl)))
	 ("place" . ,(pvs:place-list decl)))))

(defmethod json-theory-decl* ((imp pvs:importing))
  (obj `(("importing" . ,(pvs:str (pvs:theory-name imp)))
	 ("kind" . "importing")
	 ("place" . ,(pvs:place-list imp)))))

(defmethod json-theory-decl* ((decl pvs:typed-declaration))
  (let ((id (or (pvs:id decl)
		(gen-id (string-downcase (pvs:kind-of decl))))))
    (obj `(("id" . ,(string id))
	   ("kind" . ,(string-downcase (pvs:kind-of decl)))
	   ;;("class" . ,(class-name (class-of decl)))
	   ("type" . ,(pvs:str (pvs:type decl)))
	   ,@(when (pvs:generated-by decl)
	       `(("generated-by" . ,(string (pvs:ref-to-id (pvs:generated-by decl))))))
	   ("place" . ,(pvs:place-list decl))))))

(defmethod json-theory-decl* ((decl pvs:declaration))
  (let ((id (or (pvs:id decl)
		(gen-id (string-downcase (pvs:kind-of decl))))))
    (obj `(("id" . ,(string id))
	   ("kind" . ,(string-downcase (pvs:kind-of decl)))
	   ("place" . ,(pvs:place-list decl))))))
  
(defun gen-id (kind)
  (let ((num (gen-id-number kind)))
    (format nil "~a_~d" kind num)))

(defvar *idgensymctr* nil)

(defun gen-id-number (kind)
  (let ((entry (assoc kind *idgensymctr*)))
    (cond (entry
	   (incf (cdr entry)))
	  (t (push (cons kind 0) *idgensymctr*)
	     0))))

(defmethod json-theory-decl* ((decl pvs:formula-decl))
  (let* ((proved? (not (null (pvs:proved? decl))))
	 (complete? (and proved?
			 (string= (pvs:pc-complete decl)
				  "complete")))
	 (has-proofscript? (not (null (pvs:justification decl)))))
    (obj `(("id" . ,(string (pvs:id decl)))
	   ("kind" . "formula")
	   ("proved?" . ,proved?)
	   ("complete?" . ,complete?)
	   ("provable" . ,(pvs::provable-formula? decl))
	   ("has-proofscript?" . ,has-proofscript?)
	   ("place" . ,(pvs:place-list decl))))))

;;; Info

(defrequest names-info (filename)
  "Get the names-tooltip information for a PVS file"
  (assert (stringp filename) () "Filename must be a string: ~a" filename)
  (when (pvs:typecheck-file filename)
    (pvs:collect-pvs-file-decls-info filename)))

(defrequest find-declaration (id)
  "Searches for all declarations with the given id"
  (arr (pvs:find-declaration id)))

(defrequest reset ()
  "Resets PVS"
  #+allegro
  (let ((proc (mp:process-name-to-process "Initial Lisp Listener")))
    (mp:process-interrupt proc #'pvs:pvs-abort))
  #-allegro
  (break "Fix me"))

(defrequest interrupt ()
  "Interrupts PVS."
  (break "interrupt")
  ;; @M3;; (loop for session in (pvs:all-proof-sessions) do (pvs:interrupt-session (pvs:id session)))
  )

(defrequest interrupt-proof (id)
  "Interrupts proof session."
  (format t "Trying to interrupt ~s" id)
  (pvs:interrupt-session id))

;;; Prover interface

(defrequest quit-all-proof-sessions ()
  "Tries to quit from all existing proof sessions"
  (pvs:quit-all-proof-sessions))

(defrequest pvsio-start (theory-ref)
  "Starts PVSio on the given theory"
  (pvs::pvsio-init theory-ref))

(defrequest pvsio-eval (session-id expr kind)
  "Evaluates EXPR in the pvsio session SESSION-ID"
  (pvs::pvsio-eval session-id expr kind))

(defrequest prove-formula (formula-ref &optional rerun?)
  "Starts interactive proof of a formula from a given theory.  First it
determines the formula declaration, then creates a new proof-session,
returning the unique id (within a PVS session)."
  ;; (pvs:pvs-log "~%prove-formula: ~s" formula-ref)
  ;;(format t "~%prove-formula: (prove-formula ~a)" formula-ref)
  (let* ((prf-result (pvs:prover-init formula-ref :rerun? rerun?))
	 (prf-alist (pvs2json-response prf-result))
	 (prf-json (json:encode-json-to-string prf-alist)))
    ;;(format t "~%prove-formula: response = ~s" prf-alist)
    (values prf-alist prf-json)))

;; It gets confusing when qread from the prover uses *standard-input*,
;; instead we use bordeaux-thread condition variables to wait for entries in
;; the queue. After this, proof-command will find this entry, add to the queue, and
;; call bt:condition-notify

(defrequest proof-command (proof-id form)
  "Sends a command to the prover thread."
  (let* ((prf-result (pvs:prover-step proof-id form))
	 (prf-alist (pvs2json-response prf-result))
	 (prf-json (json:encode-json-to-string prf-alist)))
    (values prf-alist prf-json)))

(defrequest proof-help (cmd)
  "Gets help for the given cmd"
  (pvs:prover-help cmd))

(defun pvs2json-response (prv-result)
  "prv-result is an alist of the form
 ((\"proofstate\" ...) (\"commentary\" ...) (\"id\" ...) (\"status\" ...))"
  (let* ((id (cdr (assoc "id" prv-result :test #'string-equal)))
	 (ps (cdr (assoc "proofstate" prv-result :test #'string-equal)))
	 (err (cdr (assoc "error" prv-result :test #'string-equal)))
	 (errstr (when err (concatenate 'string "Error: " (strim-all err))))
	 (com (cdr (assoc "commentary" prv-result :test #'string-equal)))
	 (commentary (if err (cons errstr com) com))
	 (status (cdr (assoc "status" prv-result :test #'string-equal)))
	 (ps-json (pvs2json-ps ps commentary id status)))
    ;; (format t "~%pvs2json-response: ps-json =~%  ~s~%" ps-json)
    ps-json))

(defun strim-all (string)
  (let ((trstr (pvs:strim string))
	(last-space nil))
    (with-output-to-string (str)
      (loop for ch across trstr
	    do (case ch
		 ((#\space #\newline #\tab)
		  (unless last-space
		    (write-char #\space str)
		    (setq last-space t)))
		 (t (write-char ch str)
		    (setq last-space nil)))))))

(defun pvs2json-ps (ps commentary id status)
  (arr (pvs2alist-ps ps commentary id status)))

(defun pvs2alist-ps  (ps commentary id status)
  (with-slots (pvs:label pvs:comment pvs:current-goal (pps pvs:parent-proofstate)) ps
    (let* ((action (pvs:strim (pvs:format-printout ps t)))
	   (num-subgoals (pvs:proofstate-num-subgoals ps))
	   (final-sequent (pvs2json-seq pvs:current-goal pps))
	   (curr-cmd (let ((curr-rule (pvs:wish-current-rule ps)))
		       (when curr-rule (remove #\Newline (format nil "~s" curr-rule)))))
	   (prev-cmd (let ((parent-ps (pvs:wish-parent-proofstate ps)))
	   	       (when parent-ps
	   	     	 (let ((parent-rule (pvs:wish-current-rule parent-ps)))
	   	     	   (when parent-rule (remove #\Newline (format nil "~s" parent-rule)))))))
	   (comntry (let*((prev-com)
			  (length-commentary-1 (1- (length commentary))))
		      (loop for com in commentary
			    for i from 0 to length-commentary-1
			    if (stringp com)
			    collect com
			    else if (or (not (and (= i length-commentary-1) (pvs:proofstate? com)))
				     (not
				      (and
				       (pvs:proofstate? prev-com)
				       (pvs:proofstate? com)
				       (string= (ps-display-id prev-com) (ps-display-id com)))))
			    collect com
			    do (setq prev-com com))))
	   ;; (debug (format t "{pvs2alist-ps} filtered comntry (~a)~%" (length comntry))) ;; debug
	   ;; (debug (format t "~{~&{pvs2alist-ps} ~a ~}~%" comntry)) ;; debug	   
	   (intermediate-ps (when commentary
			      (read-intermediate-proof-states
			       (if (string= status "proved")
				   ;; when status is 'proved', pvs returns the top-proofstate, adding it
				   ;; to commentary would provoke to send all the proofstates in the proof
				   ;; to the client.
				   commentary
				 (append commentary (list ps)))
			       id)))
	   ;; (debug (format t "{pvs2alist-ps} filtered comntry 2 (~a)~%" (length comntry))) ;; debug
	   ;; (debug (format t "~{~&{pvs2alist-ps} ~a ~}~%" comntry)) ;; debug	   
	   )
      (or intermediate-ps
	  (list
	   (let((final-ps 
		 `(("id" . ,(string id))
		   ("unique-ps-id" . ,(pvs:unique-ps-id ps))
		   ("display-id" . ,(ps-display-id ps))
		   ("parent-display-id" . ,(let ((parent-proofstate (pvs:wish-parent-proofstate ps)))
					     (if parent-proofstate
						 (ps-display-id parent-proofstate)
					       "None")))
	      ("status" . ,(string status))
	      ,@(when comntry `(("commentary" . ,comntry)))
	      ,@(when action `(("action" . ,action)))
	      ,@(when num-subgoals `(("num-subgoals" . ,num-subgoals)))
	      ("label" . ,pvs:label)
	      ,@(when prev-cmd `(("prev-cmd" . ,prev-cmd)))
	      ,@(when curr-cmd `(("curr-cmd" . ,curr-cmd)))
	      ,@(when pvs:comment `(("comment" . ,pvs:comment)))
	      ("path" . ,(format nil "~{~a~^.~}" (pvs:path-from-top ps)))
	      ("sequent" . ,final-sequent)
	      ("children" . ,(loop for child in
				   (pvs:x-subgoals ps)
				   collect (ps-display-id child))))))
		  final-ps))))))

;; This function provides an unique id on the proof states that
;; should be shown on a graphical display of the proof tree (like
;; the whish printer or the vscode-pvs GUI).
(defun ps-display-id (ps &optional (label (pvs:label ps)) (num 0))
  (let ((par-ps (pvs:wish-parent-proofstate ps)))
    (if (or (null par-ps) (not (string= (pvs:label par-ps) label)))
	(format nil "~a-~d" label num)
	(ps-display-id par-ps label (1+ num)))))

(defun read-intermediate-proof-states (commentary id)
  (let ((comntry (mapcar #'(lambda (e)
				(pvs:strim (cond
					    ((stringp e) e)
					    ((pvs:proofstate? e)
					     (format nil "~a" e))
					    (t (format nil "~a" e)))))
			 commentary)))
    (let* ((intermediate-ps (filter-intermediate-proof-states-from-commentary commentary))
	   ;; need to add the parent of the first reported proof state (the ps on
	   ;; which the first proof command has been applied) when not reporting
	   ;; 'done' and first proof state has no children @M3
	   (intermediate-alists (when intermediate-ps
				  (loop for ps in (let* ((first-ps (car intermediate-ps))
							 (parent-of-first-ps
							  (unless (and (string= (pvs:status-flag first-ps) "!")
								       (null (pvs:x-subgoals first-ps)))
							    (pvs:parent-proofstate first-ps))))
						    (if parent-of-first-ps
							(cons parent-of-first-ps intermediate-ps)
							intermediate-ps))
					append (pvs2alist-ps ps nil id (pvs:status-flag ps))))))
      (when (and comntry intermediate-alists)
	(setf (car (last intermediate-alists))
	      (acons "commentary" comntry (car (last intermediate-alists)))))
      intermediate-alists)))

(defun filter-intermediate-proof-states-from-commentary (commentary)
  (when commentary
      (let ((current-proof-state (when (pvs:proofstate? (car commentary)) (car commentary))))
	(if (and current-proof-state
		 (or (pvs:wish-current-rule current-proof-state)
		     (pvs:current-rule current-proof-state)
		     (null (cdr commentary))))
	    (let ((result (filter-intermediate-proof-states-from-commentary
			   (append (pvs:x-subgoals current-proof-state) (cdr commentary)))))
	      (pushnew
	       current-proof-state
	       result
	       :key #'pvs:unique-ps-id
	       :test 'string=))
	  (filter-intermediate-proof-states-from-commentary (cdr commentary))))))

(defun pvs2json-seq (seq parent-ps)
  (let* ((par-sforms (when parent-ps (pvs:s-forms (pvs:current-goal parent-ps))))
	 (hidden-s-forms (pvs:hidden-s-forms seq))
	 (hn-sforms (pvs:neg-s-forms* hidden-s-forms))
	 (hp-sforms (pvs:pos-s-forms* hidden-s-forms)))
    (obj `(("antecedents" . ,(arr (pvs2json-sforms (pvs:neg-s-forms seq) t par-sforms)))
	   ("succedents" . ,(arr (pvs2json-sforms (pvs:pos-s-forms seq) nil par-sforms)))
	   ("hidden-antecedents" . ,(arr (pvs2json-sforms hn-sforms t par-sforms)))
	   ("hidden-succedents" . ,(arr (pvs2json-sforms hp-sforms nil par-sforms)))
	   ("info" . ,(pvs:info seq))))))

(defun pvs2json-sforms (sforms neg? par-sforms)
  (let ((c 0))
    (mapcar #'(lambda (sf)
		(let* ((fnum (if neg? (- (incf c)) (incf c))))
		  (pvs2json-sform sf fnum par-sforms)))
      sforms)))

;; Note that this has the side effect of setting the view of the sform,
;; Which is a cons of the string and its view (computed lazily).
(defun pvs2json-sform (sform fnum par-sforms)
  (let* ((nf (pvs:formula sform))
	 (frm (if (pvs:negation? nf) (pvs:args1 nf) nf)))
    (unless (pvs:view sform)
      (multiple-value-bind (frmstr frmview)
	  (pvs:pp-with-view frm pvs:*proofstate-indent* pvs:*proofstate-width*)
	(setf (pvs:view sform) (list frmstr frmview))))
    (let ((names-info (pvs:names-info-proof-formula sform)))
      (obj `(("labels" . ,(arr (cons fnum (mapcar #'symbol-name (pvs:label sform)))))
	     ("changed" . ,(if (member sform par-sforms) "false" "true"))
	     ("formula" . ,(car (pvs:view sform)))
	     ("names-info" . ,names-info))))))

(defun null-equivalent (obj)
  (member obj '("" "nil" "null" "none" "()") :test #'string-equal))

(defrequest prover-status (&optional proof-id)
  "Checks the status of the proof sessions: active or inactive.
Returns a list of the form ((id . status) (id . status) ...)"
  (pvs:prover-status (unless (null-equivalent proof-id) proof-id)))

(defrequest proof-status (formref)
  "Checks the status of the given formula, proved, unchecked, unfininshed,
or unproved."
  (pvs:get-proof-status formref))

(defrequest proof-script (formref)
  "Returns the proof script, as with the show-proof command."
  (pvs:get-proof-script formref))

(defrequest show-tccs (fname)
  "Returns the tccs, as with the show-tccs command."
  (pvs:get-tccs fname))

(defrequest collect-theory-usings (thref &optional exclude (in-context? t))
  "Returns the collection of theories in the transitive closure of importings."
  (pvs::with-theory (theory) thref
    (let*((all-usings (pvs::collect-theory-usings theory exclude))
          (result (if in-context? 
                    (remove-if-not 
                      #'(lambda (th) (pvs::file-equal (pvs::context-path th) (pvs::context-path theory)))
                      all-usings)
                    all-usings)))
      (json-theories result))))

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

(defun pvs2alist-proof (proof)
  `(("id" . ,(string (pvs:id proof)))
    ("description" . ,(pvs:description proof))
    ("script" . ,(pvs:script proof))
    ("status" . ,(pvs:status proof))))

(defrequest all-proofs-of-formula (form-ref)
  "Returns all the proofs associated with the given formula."
  (let* ((fdecl (pvs:get-formula-decl form-ref))
	 (proofs (pvs:proofs fdecl)))
    (mapcar #'pvs2alist-proof proofs)))

(defrequest delete-proof-of-formula (form-ref proof-id)
  "Deletes the proof-id of the formula."
  (let ((fdecl (pvs:get-formula-decl form-ref)))
    (unless fdecl
      (error "formula ~a not found" form-ref))
    (let*((proofs (pvs:proofs fdecl))
	  (proof (find proof-id proofs :test #'string= :key #'pvs:id)))
      (unless proof
	(error "proof-id ~s not found in formula ~a" proof-id form-ref))
      (let ((nproofs (remove proof proofs)))
	(when (eq proof (pvs:default-proof fdecl))
	  (setf (pvs:default-proof fdecl) (car (last nproofs)))) ; could be nil
	(mapcar #'pvs2alist-proof
		(setf (pvs:proofs fdecl) nproofs))))))

(defrequest mark-proof-as-default (form-ref proof-id)
  "Mark the proof with id PROOF-ID as the default proof for the formula identified by FORM-REF."
  (let ((fdecl (pvs:get-formula-decl form-ref)))
    (unless fdecl
      (error "formula ~a not found" form-ref))
    (let*((proofs (pvs:proofs fdecl))
	  (proof (find proof-id proofs :test #'string= :key #'pvs:id)))
      (unless proof
	(error "proof-id ~s not found in formula ~a" proof-id form-ref))
      (setf (pvs:default-proof fdecl) proof))))

;; M3: Request to save proofs to prf file [Sept 2020]
(defrequest save-all-proofs (theoryref)
  "Stores the declaration proofs into the corresponding PRF file"
  (let ((theory (pvs:get-typechecked-theory theoryref)))
    (unless theory
      (pvs:pvs-error "Save-all-proofs error" (format nil "Theory ~a cannot be found" theoryref)))
    (pvs:save-all-proofs theory)))

(defrequest add-pvs-library (string)
  "Just evaluate the string in lisp"
  (let ((*package* (find-package :pvs)))
    (push string pvs:*pvs-library-path*)))


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
  (obj `(("kind" . "theory")
	 ("id" . ,(string (pvs:id term)))
	 ("place" . ,(pvs:place-list (pvs:place term))))))

(defmethod json-term* ((term pvs:recursive-type))
  (obj `(("kind" . "recursive-type")
	 ("id" . ,(string (pvs:id term)))
	 ("place" . ,(pvs:place-list (pvs:place term))))))

(defmethod json-term* ((term pvs:declaration))
  (pvs:json-decl-list term (pvs:ptype-of term) (pvs:module term)))

(defmethod json-term* ((term pvs:type-expr))
  (obj `(("kind" . "type")
	 ("string" . ,(pvs:str term))
	 ("place" . ,(pvs:place-list (pvs:place term))))))

(defmethod json-term* ((term pvs:expr))
  (obj `(("kind" . "expr")
	 ("string" . ,(pvs:str term))
	 ("place" . ,(pvs:place-list (pvs:place term))))))

(defmethod json:encode-json (any &optional (stream json:*json-output*))
  ;; Just return nil in this case, there's a problem on some Macs that this deals with.
  (declare (ignore any stream))
  nil)

;; BEGIN LaTeX Generation -------------------------------------------------

(defrequest latex-pvs-file (filename &optional content)
  "Generate LaTeX from file"
  (when content
    ;; Try to write the file
    (with-open-file (out filename :direction :output :if-exists :supersede)
      (write content :stream out :escape nil)))
  (let ((main-tex-file (pvs::latex-pvs-file filename)))
    (when main-tex-file
	`(("main-file" . ,(format nil "~a" main-tex-file))))))

(defrequest latex-theory (theoryref)
  "Generate LaTeX from theory"
  (let ((main-tex-file (pvs::latex-theory theoryref)))
    (when main-tex-file
      `(("main-file" . ,(format nil "~a" main-tex-file))))))

(defrequest latex-importchain (theoryref)
  "Generate LaTeX from theory"
  (let ((main-tex-file (pvs::latex-usingchain theoryref)))
    (when main-tex-file
      `(("main-file" . ,(format nil "~a" main-tex-file))))))

;; END LaTeX Generation ---------------------------------------------------
