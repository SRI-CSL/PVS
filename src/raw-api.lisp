(in-package :pvs)

;;; The raw API - no Emacs interface needed for these functions.
;;; Creates a working theory, which allows declarations, IMPORTINGs,
;;; etc. to be incrementally added using typecheck-declarations.
;;; prove-as-black-box may then be used to prove a specified formula.

;;; (add-working-decls declstring)
;;; Given a declstring, e.g.
;;;   (add-working-decls "IMPORTING reals@sqrt; f: FORMULA 4 = sqrt(16)")
;;; This will be parsed and typechecked, and if successful will update
;;; the working theory by adding these declarations at the end.
;;; The formula may then be proved with
;;;   (prove-as-black-box "f")
;;; See below for more details on prove-as-black-box.

;;; The declarations are added to the end of the working theory.
;;; Everything may be reset using (clear-working-theory)

(defvar *working-current-context* nil)
(defvar *working-pvs-files* nil)
(defvar *working-pvs-modules* nil)
(defvar *working-theory* nil)
(defvar *pvs-black-box-proof* nil)

(defmacro with-working-context (&rest forms)
  `(let ((*pvs-files* (get-working-pvs-files))
	 (*pvs-modules* (get-working-pvs-modules))
	 (*current-context* (get-working-current-context))
	 (*generate-tccs* 'all))
     ,@forms))

(defun get-working-pvs-files ()
  (or *working-pvs-files*
      (setq *working-pvs-files*
	    (make-hash-table :test #'equal))))

(defun get-working-pvs-modules ()
  (or *working-pvs-modules*
      (let ((th (get-working-theory))
	    (ht (make-hash-table :test #'eq :size 20 :rehash-size 10)))
	(setf (gethash (id th) ht) th)
	(setq *working-pvs-modules* ht))))

(defun get-working-current-context ()
  (or *working-current-context*
      (setq *working-current-context*
	    (context (get-working-theory)))))

(defun get-working-theory ()
  (or *working-theory*
      (let ((exp (make-instance 'exporting
		   'kind 'default)))
	(setq *working-theory*
	      (make-instance 'module
		   'id 'working-theory
		   'exporting exp)))))

(defun clear-working-theory ()
  (setf *working-theory* nil
	*working-current-context* nil)
  (when *working-pvs-files*
    (clrhash *working-pvs-files*))
  (when *working-pvs-modules*
    (clrhash *working-pvs-modules*)))

(defun add-working-decls (declstring)
  (with-working-context
   (let* ((decls (pc-parse declstring 'theory-part)))
     (dolist (decl decls)
       (typecheck-decls (list decl))
       (if (theory (current-theory))
	   (nconc (theory (current-theory)) (list decl))
	   (setf (theory (current-theory)) (list decl)))))))

(defun show-working-theory ()
  (with-working-context
   (unparse (current-theory))))

(defun get-working-formulas ()
  (with-working-context
   (dolist (d (theory (current-theory)))
     (when (formula-decl? d)
       (unparse d)
       (terpri)))))

(defun get-working-formula-names ()
  (with-working-context
   (mapcar #'id (remove-if-not #'formula-decl? (theory (current-theory))))))

(defun prove-declaration (formula-name &optional strategy)
  (with-working-context
   (let* ((*current-context* (copy *current-context*))
	  (fdecl (find-if #'(lambda (d) (and (formula-decl? d)
					     (string= (id d) formula-name)))
		   (theory (current-theory)))))
     (when strategy
       (multiple-value-bind (strat err)
	   (if (stringp strategy)
	       (ignore-errors (values (read-from-string strategy)))
	       strategy)
	 (let ((just (unless err
		       (or (revert-justification strat)
			   (revert-justification (list "" strat))
			   strat))))
	   (unless just
	     (type-error strategy "Bad form for strategy~%  ~s" strategy))
	   (setf (justification fdecl) just))))
     (read-strategies-files)
     (unwind-protect
	 (prove-decl fdecl :strategy (when strategy
				       '(then (rerun) (quit))))))))


;; Function Name:  prove-as-black-box

;; Description: Provides an interface to the PVS prover, allowing a
;; proof to be attempted on a given formula.  The formula is typechecked,
;; then the proof is attempted, and possibly a proof of the negated form
;; is attempted.  By default, the context of the formula is the prelude,
;; including extensions gotten using M-x load-prelude-library.  If a file
;; is given, then it forms the context, and if a library is also given,
;; then the file is taken from that library, and forms the context.

;; Arguments: 
;;   name              string (no whitespace)
;; &optional
;;   pos-strat         string or list (default nil)
;;   neg-strat         string or list (default nil)
;; &key
;;   interactive       t or nil (default)

;; Errors:
;;         Parse Error             :pvs-parse-err
;;                                 (location in Lemma)
;;                                 Error message
;;                                 :end-pvs-parse-err
;;         Typecheck Error         :pvs-tc-err
;;                                 (location in Lemma)
;;                                 Error Message
;;                                 :end-pvs-tc-err
;;         Lisp Error              :pvs-lisp-err
;;                                 Debug information
;;                                 error report instructions
;;                                 :end-pvs-lisp-err

;; Returns:
;;         :proved                  If successful
;;         :disproved               If negation successful
;;         :unknown                 Unsuccessful, negation not attempted
;;         :unproved                Neither proof successful
;; Side Effects:
;;         *pvs-black-box-proof* - set to the proof resulting from the
;;             proof attempt.  If there is a successful proof, this will
;;             be set to it.  Otherwise it is set to the failed pos-strat
;;             proof.

;; Notes:
;; ------
;; name - the identifier used for the formula declaration
;;        if a neg-strat is given, the negated formula is given the
;;        identifier with "_negated" appended.
;; pos-strat - a strategy to be used for the positive formula.  If nil,
;;        and interactive is nil, defaults to (grind).
;; neg-strat - the strategy to be used for the negated formula.  If nil,
;;        the negated formula is not generated.  If t, the pos-strat is
;;        also applied to the negated formula.
;; interactive - if nil, the proof is run on the formula and if it doesn't
;;        complete, it is terminated, and the negated formula, if any, is
;;        attempted.  If t, pos-strat is attempted interactively.  When
;;        the proof completes, if not successful, the negated formula, if
;;        any, is attempted.

;; Examples:
;;  For f1: FORMULA 1 + 2 = 3
;;      f2: FORMULA 1 + 2 = 2
;;      f3: FORMULA FORALL (n: nat): n < n*n
;;  (prove-as-black-box "f1")
;;       Attempts proof using (grind), returns :proved
;;  (prove-as-black-box "f2")
;;       Attempts proof using (grind), returns :unknown
;;  (prove-as-black-box "f2" nil t)
;;       Attempts proof using (grind), on both pos and neg, returns :disproved
;;  (prove-as-black-box "f3" nil t)
;;       Attempts proof using (grind), on both pos and neg, returns :unproved

(defun prove-as-black-box (name &optional lemma pos-strat neg-strat
				&key interactive
				(failure-response :return) depth)
  (unless (stringp name)
    (error "name must be a string"))
  (unless (valid-pvs-id* name)
    (error "name must be a valid identifier"))
  (unless (or (null lemma) (stringp lemma))
    (error "lemma must be nil or a string"))
  (unless (memq failure-response '(:return :negation))
    (error "failure-response must be one of :return or :negation"))
  (unless (or (null depth) (plusp depth))
    (error "depth must be nil or a positive integer"))
  (with-working-context
   (let* ((fdecl (when (and lemma (not (string= lemma "")))
		   (mk-formula-decl (intern name) (pc-parse lemma 'expr))))
	  (edecl (find-if #'(lambda (d)
			      (and (formula-decl? d)
				   (eq (module d) (current-theory))))
		   (get-declarations (intern name)))))
     (when fdecl
       (when edecl
	 (setf (theory (current-theory))
	       (delete edecl (theory (current-theory))))
	 (delete-declaration edecl))
       (typecheck-decls (list fdecl))
       (if (theory (current-theory))
	   (nconc (theory (current-theory)) (list fdecl))
	   (setf (theory (current-theory)) (list fdecl))))
     (unless (or fdecl edecl) (error "Formula ~a not found" lemma))
     (prove-as-black-box* (or fdecl edecl) pos-strat neg-strat interactive
			  failure-response depth))))

(defun prove-as-black-box* (fdecl pos-strat neg-strat interactive
				  failure-response depth)
  (let* ((neg-fdecl nil))
    (when neg-strat
      (setq neg-fdecl (copy fdecl
			'id (makesym "~a_negated" (id fdecl))
			'definition (negate! (definition fdecl))
			'closed-definition nil))
      (setf (theory (current-theory))
	    (let ((cdecls (memq fdecl (theory (current-theory)))))
	      (assert cdecls)
	      (nconc (ldiff (theory (current-theory)) (cdr cdecls))
		     (list neg-fdecl)
		     (cdr cdecls)))))
    (add-strategies-to-formula-decls fdecl neg-fdecl pos-strat neg-strat
				     interactive)
    (prove-as-black-box-prove fdecl neg-fdecl interactive
			      failure-response depth)))

(defun add-strategies-to-formula-decls (fdecl neg-fdecl pos-strat neg-strat
					      interactive)
  (read-strategies-files)
  (multiple-value-bind (pos neg err)
      (prove-as-black-box-strategies pos-strat neg-strat interactive)
    (when pos
      (setf (justification fdecl) pos))
    (when neg
      (setf (justification neg-fdecl) neg))))

;;; Try to prove fdecl, if it fails and neg-fdecl is given, try to prove it.
;;; if either proof succeeds, set *pvs-black-box-proof* to the result.

;;; That's the summary, now for the details.  Attempt to prove fdecl.  If
;;; neg-fdecl is nil and interactive is t, run interactively, otherwise
;;; run noninteractively.  Either way, if the proof succeeds, save in
;;; *pvs-black-box-proof* and return.  If it does not succeed, and
;;; neg-fdecl is not nil, try to prove neg-fdecl, interactively if so
;;; indicated.  If it succeeds, save in *pvs-black-box-proof*.

(defun prove-as-black-box-prove (fdecl neg-fdecl interactive
				       failure-response depth)
  (let* ((*multiple-proof-default-behavior* :noquestions)
	 (*noninteractive* (not interactive))
	 (proof (prove-as-black-box-prove* fdecl interactive)))
    (cond ((eq (status-flag proof) '!)
	   (setq *pvs-black-box-proof* proof)
	   :proved)
	  ((null neg-fdecl)
	   (setq *pvs-black-box-proof* proof)
	   :unknown)
	  (t 
	   (let ((nproof (prove-as-black-box-prove* neg-fdecl interactive)))
	     (cond ((eq (status-flag nproof) '!)
		    (setq *pvs-black-box-proof* nproof)
		    :disproved)
		   (t (setq *pvs-black-box-proof* proof)
		      :unproved)))))))

(defun prove-as-black-box-prove* (fdecl interactive)
  (prove-decl fdecl
	      :strategy (if interactive
			    '(then (rerun) (query*))
			    '(then (rerun) (quit)))))

(defun prove-as-black-box-strategies (pos-strat neg-strat interactive)
  (let ((pos nil)
	(neg nil))
    (if pos-strat
	(multiple-value-bind (strat err)
	    (if (stringp pos-strat)
		(ignore-errors (values (read-from-string pos-strat)))
		pos-strat)
	  (unless err
	    (setq pos
		  (or (ignore-errors (revert-justification strat))
		      (ignore-errors (revert-justification (list "" strat)))))
	    (unless pos
	      (type-error pos-strat "Bad form for strategy~%  ~s" pos-strat))))
	(unless interactive
	  (setq pos '("" (grind)))))
    (when neg-strat
      (if (eq neg-strat t)
	  (setq neg pos)
	  (multiple-value-bind (strat err)
	      (if (stringp neg-strat)
		  (ignore-errors (values (read-from-string neg-strat)))
		  neg-strat)
	    (unless err
	      (setq neg
		    (or (ignore-errors (revert-justification strat))
			(ignore-errors (revert-justification
					(list "" strat)))))
	      (unless neg
		(type-error neg-strat "Bad form for strategy~%  ~s"
			    neg-strat))))))
    (values pos neg)))
