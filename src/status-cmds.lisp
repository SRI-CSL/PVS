;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; status-cmds.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Feb 19 21:23:43 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:40:40 1998
;; Update Count    : 10
;; Status          : Alpha test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

(defvar *unproved-dependings* nil)
(defvar *proved-dependings* nil)
(defvar *defn-dependings* nil)
(defvar *axiom-dependings* nil)
(defvar *assumption-dependings* nil)
(defvar *dependings* (init-symbol-table))
(defvar *depending-chain* nil)
(defvar *depending-cycles* nil)

;;; Status Commands - these will generally interrupt PVS to report on the
;;; status, and tend to use the context information rather than the
;;; internal information.  Thus the proof status can be obtained without
;;; loading or retypechecking any theories.


;;; Status Theory - called from Emacs

(defun status-theory (theoryref)
  (pvs-message (theory-status-string theoryref)))


;;; Status PVS File - called from Emacs

(defun status-pvs-file (filename)
  (let ((theories (get-context-theory-names filename)))
    (if theories
	(pvs-buffer "PVS Status"
	  (format nil "~{~a~%~}"
	    (mapcar #'theory-status-string theories))
	  t t)
	(pvs-message "PVS file ~a is not in the current context"
	  filename))))


;;; Provides the status of the specified theory; returns a string.

(defun theory-status-string (theoryref)
  (let ((theory (get-theory theoryref)))
    (format nil "~a is ~a~@[~a~]~@[~a~]"
      theoryref
      (cond ((null theory)
	     (if (context-file-of theoryref)
		 "not yet parsed"
		 "not in the current context"))
	    ((typechecked? theory) "typechecked")
	    ((parsed? theory) "parsed")
	    (t "not yet parsed"))
      (when theory
	(final-proof-summary theory))
      (when theory
	(format nil "; ~d warning~:p; ~d msg~:p"
	  (length (warnings theory)) (length (info theory)))))))

(defun final-proof-summary (theory)
  (let ((formnum 0) (tccnum 0) (provenum 0) (tccprovenum 0))
    (dolist (d (append (assuming theory) (theory theory)))
      (when (and (formula-decl? d)
		 (not (member (spelling d)
			      '(axiom postulate assumption))))
	(cond ((tcc? d)
	       (incf tccnum)
	       (when (proved? d)
		 (incf tccprovenum)))
	      (t (incf formnum)
		 (when (proved? d)
		   (incf provenum))))))
    (format nil ": ~d formula~:*~p, ~d proved; ~d TCC~:*~p, ~d proved"
      formnum provenum tccnum tccprovenum)))

(defun partially-typechecked? (theory)
  (and (parsed? theory)
       (not (typechecked? theory))
       (some@ #'(lambda (d) (and (typep d 'declaration) (typechecked? d)))
	     (append (assuming theory) (theory theory)))))

;;; Using Status

(defun status-importchain (theory)
  (let ((te (get-context-theory-entry theory)))
    (if te
	(let ((*modules-visited* nil))
	  (pvs-buffer "PVS Status"
	    (with-output-to-string (*standard-output*)
	      (status-importchain* (id te)))
	    t))
	(pvs-message "~a is not in the current context" theory))))

(defun status-importchain* (tid &optional (indent 0))
  (let* ((th (get-theory tid)))
    (cond ((null th)
	   (format t "~a is not parsed" tid))
	  ((member tid *modules-visited*)
	   (format t "~vT... ~a already described~%" indent tid))
	  (t (let ((usings (when th (get-theory-dependencies tid))))
	       (when th (push tid *modules-visited*))
	       (format t "~vTTheory ~a~%~vT  It uses ~?~%"
		 indent (theory-status-string (id th))
		 indent *andusingctl* usings)
	       (mapc #'(lambda (m) (status-importchain* m (+ indent 2)))
		     usings))))))


;;; Usedby Status

(defun status-importbychain (theory)
  (let ((*modules-visited* nil))
    (pvs-buffer "PVS Status"
      (with-output-to-string (*standard-output*)
	(status-importbychain* (ref-to-id theory)))
      t)))

(defun status-importbychain* (tid &optional (indent 0))
  (let ((th (get-theory tid)))
    (if (member tid *modules-visited*)
	(format t "~vT... ~a already described~%" indent tid)
	(let ((usedbys (find-all-usedbys tid)))
	  (push tid *modules-visited*)
	  (format t "~vTTheory ~a~%~vT  It is used by ~?~%"
	    indent (theory-status-string (id th))
	    indent *andusingctl* usedbys)
	  (mapc #'(lambda (m) (status-importbychain* m (+ indent 2)))
		usedbys)))))


;;; Proof Status

(defun proof-status-at (filename line &optional (origin "pvs"))
  (let ((fdecl (formula-decl-to-prove filename line origin)))
    (if fdecl
	(pvs-message "~a is ~a." (id fdecl) (proof-status-string fdecl))
	(pvs-message "Unable to find formula declaration"))))


;;; Status Proof Theory

(defun status-proof-theory (theoryname &optional times?)
  (let ((theory (or (get-theory theoryname)
		    (get-context-theory-entry theoryname))))
    (if theory
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summary (id theory) times?))
	  t)
	(pvs-message "Theory ~a is not in the current context."
	  theoryname))))


;;; Status Proof PVS File

(defun status-proof-pvs-file (filename &optional times?)
  (let ((theories (get-context-theory-names filename)))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summaries theories times?))
	  t)
	(pvs-message "File ~a.pvs is not in the current context"
	  filename))))

(defun status-proof-theories (theories &optional times?)
  (if theories
      (pvs-buffer "PVS Status"
	(with-output-to-string (*standard-output*)
	  (proof-summaries theories times?))
	t)
      (pvs-message "No theories given")))

;;; Status Proof Importchain

(defun status-proof-importchain (theoryname &optional times?)
  (update-pvs-context)
  (let ((theories (context-usingchain theoryname)))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summaries theories times?))
	  t)
	(pvs-message "Theory ~a is not in the current context"
	  theoryname))))


(defun proof-summaries (theory-ids &optional times? filename)
  (let ((tot 0) (proved 0) (unfin 0) (untried 0))
    (when filename
      (format t "~2%Proof summary for file ~a.pvs" filename))
    (dolist (theory theory-ids)
      (multiple-value-bind (to pr uf ut)
	  (proof-summary theory times? (when filename 2))
	(incf tot to) (incf proved pr) (incf unfin uf) (incf untried ut)))
    (if filename
	(format t "~2%  Totals for ~a.pvs: " filename)
	(format t "~2%Grand Totals: "))
    (format t "~d proofs, ~d attempted, ~d succeeded."
	    tot (+ proved unfin) proved)
    (values tot proved unfin untried)))

(defun proof-summary (theory-id &optional times? (indent 0))
  (format t "~2%~vTProof summary for theory ~a" indent (ref-to-id theory-id))
  (let ((tot 0) (proved 0) (unfin 0) (untried 0))
    (let* ((theory (get-theory theory-id))
	   (valid? (or (and theory
			    (from-prelude? theory))
		       (valid-proofs-file (context-entry-of theory-id)))))
      (if (and theory
	       (typechecked? theory))
	  (mapc #'(lambda (decl)
		    (format t "~%    ~55,1,0,'.a~10a"
		      (id decl)
		      (proof-status-string decl))
		    (when times?
		      (if (proof-time decl)
			  (format t
			      "~%      Run time: ~6,2F s, Real time: ~6,2F s - ~a"
			    (/ (car (proof-time decl))
			       internal-time-units-per-second)
			    (/ (cadr (proof-time decl))
			       internal-time-units-per-second)
			    (if (caddr (proof-time decl))
				"interactive" "background"))
			  (format t "~%      Proof times unavailable")))
		    (incf tot)
		    (cond ((proved? decl)
			   (incf proved))
			  ((justification decl) (incf unfin))
			  (t (incf untried))))
		(provable-formulas theory))
	  (let ((te (get-context-theory-entry theory-id)))
	    (mapc #'(lambda (fe)
		      (let ((status (fe-status fe)))
			(format t "~%    ~55,1,0,'.a~(~10a~)"
			  (fe-id fe)
			  (fe-proof-status-string fe valid?))
			(incf tot)
			(case status
			  ((proved-complete proved-incomplete)
			   (if valid?
			       (incf proved)
			       (incf unfin)))
			  ((unchecked unfinished)
			   (incf unfin))
			  (t (incf untried)))))
		  (te-formula-info te)))))
    (format t "~%    Theory totals: ~d formulas, ~d attempted, ~d succeeded."
	tot (+ proved unfin) proved)
    (values tot proved unfin untried)))

(defmethod provable-formulas ((theory module))
  (provable-formulas (append (assuming theory) (theory theory))))

(defmethod provable-formulas ((adt datatype))
  nil)

(defmethod provable-formulas ((decls list))
  (labels ((pfs (decls result)
	     (if (null decls)
		 (nreverse result)
		 (pfs (cdr decls)
		      (if (provable-formula? (car decls))
			  (cons (car decls) result)
			  result)))))
    (pfs decls nil)))

(defmethod provable-formula? ((decl formula-decl))
  (or (not (memq (spelling decl) '(ASSUMPTION AXIOM)))
      (justification decl)))

(defmethod provable-formula? (obj)
  (declare (ignore obj))
  nil)

;;; ProofChain Status, Module ProofChain Status, Formula Status and
;;; Module Formula Status

(defun proofchain-status-at (filename line &optional (origin "pvs"))
  (if (gethash filename *pvs-files*)
      (let ((fdecl (formula-decl-to-prove filename line origin)))
	(if fdecl
	    (let ((*current-theory* (slot-value fdecl 'module)))
	      (pvs-buffer "PVS Status"
		(with-output-to-string (*standard-output*)
		  (pc-analyze fdecl))
		t))
	    (pvs-message "Unable to find formula declaration")))
      (pvs-message "~a.pvs has not been typechecked" filename)))

(defun status-proofchain-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (if theory
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (pc-analyze theory))
	  t)
	(pvs-message "~a has not been typechecked" theoryname))))

(defun status-proofchain-pvs-file (filename)
  (let ((theories (get-theories filename)))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (mapc #'pc-analyze theories))
	  t)
	(pvs-message "~a.pvs has not been typechecked" filename))))

(defun status-proofchain-importchain (theoryname)
  (if (get-theory theoryname)
      (let ((theories (collect-theory-usings theoryname)))
	(if theories
	    (pvs-buffer "PVS Status"
	      (with-output-to-string (*standard-output*)
		(mapc #'pc-analyze theories))
	      t)
	    (pvs-message "Theory ~a is not in the current context"
	      theoryname)))
      (pvs-message "~a has not been typechecked" theoryname)))


(defun full-status-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (if theory
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (full-status-theory* theory))
	  t)
	(pvs-message "Theory ~a has not been typechecked"
	  theoryname))))

(defun full-status-theory* (theory)
  (let ((decls (remove-if-not #'(lambda (d) (typep d 'declaration))
		 (append (formals theory) (assuming theory) (theory theory)))))
    (format t "Theory ~a has ~d declarations:"
      (id theory) (length decls))
    (format t "~%  Name~25TGenerated-by~50TTime (s)")
    (format t "~%  ----~25T------------~50T----")
    (full-status-theory-decls decls)))

(defun full-status-theory-decls (decls)
  (when decls
    (when (typep (car decls) 'declaration)
      (format t "~%  ~a~25T~@[~a~]~50T~@[~d~]"
	(id (car decls))
	(when (generated-by (car decls))
	  (if (typep (generated-by (car decls)))
	      (generated-by (car decls))
	      (id (generated-by (car decls)))))
	(when (typecheck-time (car decls))
	  (/ (typecheck-time (car decls))
	     internal-time-units-per-second 1.0))))
    (full-status-theory-decls (cdr decls))))
    
;;; The proof status is kept with a formula in the proof-status slot, and
;;; with a pvs-context's formula-entry in the fe-status slot.

;;; The proof-status slot of a formula-decl can be one of the symbols
;;; NIL (untried), unfinished, unchecked, or proved.  fe-status is one of
;;; untried, unfinished, unchecked, proved-incomplete, or proved-complete.

(defun fe-proof-status-string (fe valid?)
  (case (fe-status fe)
    (proved-complete (if valid?
			 "proved - complete"
			 "unchecked"))
    ((proved-incomplete proved) (if valid?
				    "proved - incomplete"
				    "unchecked"))
    (unchecked "unchecked")
    (unfinished "unfinished")
    (t "untried")))

(defun proof-status-symbol (decl)
  (cond ((eq (proof-status decl) 'unchecked) 'unchecked)
	((proved? decl)
	 (let ((complete (pc-complete decl)))
	   (if (string= complete "complete")
	       'proved-complete
	       'proved-incomplete)))
	((justification decl) 'unfinished)
	(t 'untried)))

(defun proof-status-string (decl)
  (cond ((eq (proof-status decl) 'unchecked) "unchecked")
	((proved? decl)
	 (let ((complete (pc-complete decl)))
	   (if (string= complete "complete")
	       "proved - complete"
	       "proved - incomplete")))
	((justification decl) "unfinished")
	(t "untried")))

(defun pc-complete (decl)
  (let* ((*dependings* (init-symbol-table))
	 (*proved-dependings* nil)
	 (*unproved-dependings* nil)
	 (*defn-dependings* nil)
	 (*axiom-dependings* nil)
	 (*assumption-dependings* nil)
	 (*depending-chain* nil)
	 (*depending-cycles* nil))
    (pc-analyze* (union (union (refers-to decl)
			       (proof-refers-to decl))
			(assuming-tccs decl)))
    (maphash #'(lambda (x y)
		 (declare (ignore x))
		 (cond ((typep y 'formula-decl)
			(if (axiom? y)
			    (push y *axiom-dependings*)
			    (if (assumption? y)
				(push y *assumption-dependings*)
				(if (proved? y)
				    (push y *proved-dependings*)
				    (push y *unproved-dependings*)))))
		       ((and (or (typep y 'const-decl)
				 (typep y 'def-decl))
			     (definition y))
			(push y *defn-dependings*))
		       (t)))
	     *dependings*)
    (cond (*depending-cycles* "circular")
	  ((and (null *unproved-dependings*)
		(proved? decl))
	   "complete")
	  (t "incomplete"))))

(defun show-proofs-pvs-file (file)
  (let ((proofs (read-pvs-file-proofs file)))
    (cond (proofs
	   (setq *displayed-proofs* proofs)
	   (pvs-buffer "Show Proofs"
	     (with-output-to-string (outstr)
	       (format outstr "Proof scripts for file ~a.pvs:" file)
	       (let* ((ce (context-entry-of file))
		      (valid? (and ce (valid-proofs-file ce))))
		 (show-all-proofs-file proofs outstr valid?)))
	     'popto t)
	   t)
	  (t (pvs-message "No proofs found in this file")))))

(defun show-proofs-theory (theoryname)
  (let ((proofs (read-pvs-file-proofs (context-file-of theoryname))))
    (cond (proofs
	   (setq *displayed-proofs* proofs)
	   (pvs-buffer "Show Proofs"
	     (with-output-to-string (outstr)
	       (format outstr "Proof scripts for theory ~a:" theoryname)
	       (let ((valid? (valid-proofs-file (context-entry-of theoryname)))
		     (thproofs (assq (intern theoryname) proofs)))
		 (show-all-proofs-theory (car thproofs) (cdr thproofs)
					 outstr valid?)))
	     'popto t)
	   t)
	  (t (pvs-message "No proofs found in this theory")))))

(defun show-proofs-importchain (theoryname)
  (let* ((imports (context-usingchain theoryname))
	 (files (delete-duplicates (mapcar #'context-file-of imports)
				   :test #'string=))
	 (valid? (every #'(lambda (ff)
			    (let ((ce (context-entry-of ff)))
			      (and ce (valid-proofs-file ce))))
			files))
	 (proofs (get-importchain-proofs theoryname imports files)))
    (cond (proofs
	   (setq *displayed-proofs* proofs)
	   (pvs-buffer "Show Proofs"
	     (with-output-to-string (outstr)
	       (format outstr "Proof scripts for importchain of theory ~a:"
		 theoryname)
	       (show-all-proofs-file proofs outstr valid?))
	     'popto t)
	   t)
	  (t (pvs-message "No proofs found in this file")))))

(defun get-importchain-proofs (theoryname imports files)
  (let ((all-proofs (mapappend #'read-pvs-file-proofs files)))
    (remove-if-not #'(lambda (pr)
		       (member (car pr) imports
			       :test #'(lambda (x y) (eq x (intern y)))))
      all-proofs)))

(defun show-all-proofs-file (proofs outstr valid?)
  (when proofs
    (show-all-proofs-theory (caar proofs) (cdar proofs) outstr valid?)
    (show-all-proofs-file (cdr proofs) outstr valid?)))

(defun show-all-proofs-theory (theory proofs outstr valid?)
  (let* ((te (get-context-theory-entry theory))
	 (finfo (when te (te-formula-info te)))
	 (th (get-theory theory)))
    (cond (th
	   (show-all-proofs-theory* outstr proofs (all-decls th) th))
	  (finfo
	   (show-all-proofs-theory-ctx outstr proofs finfo theory valid?))
	  (t
	   (show-all-proofs-nostatus outstr theory proofs)))))

(defun show-all-proofs-nostatus (outstr theoryid proofs)
  (dolist (prf proofs)
    (format outstr "~3%~a.~a~2%"
      theoryid (car prf))
    (write (editable-justification (cdr prf))
	   :stream outstr :pretty t :escape t :level nil :length nil
	   :pprint-dispatch *proof-script-pprint-dispatch*)))

(defun show-all-proofs-theory* (outstr proofs decls theory)
  (dolist (prf proofs)
    (let ((decl (find-if #'(lambda (d)
			     (and (typep d 'formula-decl)
				  (eq (id d) (car prf))))
		  decls)))
      (when decl
	(format outstr "~3%~a.~a: ~a~2%"
	  (id theory) (id decl) (proof-status-string decl))
	(write (editable-justification (cdr prf))
	       :stream outstr :pretty t :escape t :level nil
	       :length nil :pprint-dispatch *proof-script-pprint-dispatch*)))))

(defun show-all-proofs-theory-ctx (outstr proofs finfo thid valid?)
  (dolist (prf proofs)
    (let* ((fe (car (member (car prf) finfo
			    :test #'(lambda (x y) (eq x (fe-id y))))))
	   (status (or (and fe (fe-proof-status-string fe valid?))
		       "unchecked")))
      (format outstr "~3%~a.~a: ~a~2%" thid (car prf) status)
      (write (editable-justification (cdr prf))
	     :stream outstr :pretty t :escape t :level nil :length nil
	     :pprint-dispatch *proof-script-pprint-dispatch*))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Proof chain analysis
;;;;;;;;;;;;;;;;
;;note: need to modify to divert PCA output to stream, and
;;to avoid repeatedly printing name of current module.

(defmethod pc-analyze ((mod module))
  (dolist (decl (all-decls mod))
    (when (typep decl 'formula-decl)
      (pc-analyze decl)
      (format t "~2%"))))

(defmethod pc-analyze ((decl formula-decl))
  (let* ((*dependings* (init-symbol-table))
	 (*proved-dependings* nil)
	 (*unproved-dependings* nil)
	 (*defn-dependings* nil)
	 (*axiom-dependings* nil)
	 (*assumption-dependings* nil)
	 (*depending-chain* nil)
	 (*depending-cycles* nil))
    (pc-analyze* (union (union (refers-to decl)
			       (proof-refers-to decl))
			(assuming-tccs decl)))
    (maphash #'(lambda (x y)
		 (cond ((typep y 'formula-decl)
			(if (axiom? y)
			    (push y *axiom-dependings*)
			    (if (assumption? y)
				(push y *assumption-dependings*)
				(if (proved? y)
				    (push y *proved-dependings*)
				    (push y *unproved-dependings*)))))
		       ((and (or (typep y 'const-decl)
				 (typep y 'def-decl))
			     (definition y))
			(push y *defn-dependings*))
		       (t)))
	     *dependings*)
    (if (axiom? decl)
	(format t "~%~a.~a is an axiom." (id (module decl)) (id decl))
	(if (proved? decl)
	    (format t "~%~a.~a has been PROVED." (id (module decl)) (id decl))
	    (format t "~%~a.~a is UNPROVED." (id (module decl)) (id decl))))
    (when *depending-cycles*
      (format t "~%~%***Warning***: The proof chain for ~a is CIRCULAR in the following:"
	(id decl))
      (loop for x in *depending-cycles*
	    do (format t "~%   ~a.~a" (id (module x))(id x))))
    (cond
      ((and (null *unproved-dependings*) (proved? decl))
       (format t "~%~%  The proof chain for ~a is COMPLETE." (id decl)))
      ((proved? decl)
       (format t "~%~%  The proof chain for ~a is INCOMPLETE.~
                  ~%  It depends on the following unproved conjectures:"
	 (id decl))
       (loop for x in *unproved-dependings*
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x)))))
    (when *proved-dependings*
      (format t "~%~%  ~a depends on the following proved theorems:"
	(id decl))
      (loop for x in *proved-dependings*
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *axiom-dependings*
      (format t "~%~%  ~a depends on the following axioms:"
	(id decl))
      (loop for x in *axiom-dependings*
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *defn-dependings*
      (format t "~%~%  ~a depends on the following definitions:"
	(id decl))
      (loop for x in *defn-dependings*
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *assumption-dependings*
      (format t "~%~%  ~a depends on the following assumptions:"
	(id decl))
      (loop for x in *assumption-dependings*
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))))

(defun assuming-tccs (decl)
  (let ((theory-decls (all-decls (module decl))))
    (remove-if-not #'assuming-tcc?
      (ldiff theory-decls (cdr (memq decl theory-decls))))))

(defmethod pc-analyze ((decl T))
  nil)

(defun axiom-or-defn? (x)
    (or (axiom? x)
	(and (typep x 'const-decl)
	     (definition x))))
		   
(defun axiom? (x)
  (and (typep x 'formula-decl)
       (memq (spelling x) '(AXIOM POSTULATE))))


(defun assumption? (x)
  (and (typep x 'formula-decl)
       (eq (spelling x) 'ASSUMPTION)))
    


(defmethod pc-analyze* ((fdecl formula-decl))
  (let ((*depending-chain* *depending-chain* ))
    (cond ((memq fdecl *depending-chain*)
	   (pushnew fdecl *depending-cycles*)
	   *dependings*)
	  ((gethash fdecl *dependings*)
	   *dependings*)
	  (t (setf (gethash fdecl *dependings*) fdecl)
	     (push fdecl *depending-chain*)
	     (cond ((or (axiom? fdecl)
			(assumption? fdecl))
		    (pc-analyze* (remove-if-not #'tcc? (generated fdecl))))
		   ((or (from-prelude? fdecl)
			(not (proved? fdecl)))
		    *dependings*)
;		   ((eq (kind fdecl) 'tcc)
;		    (pc-analyze* (proof-refers-to fdecl)))

		   (t (pc-analyze* (union (refers-to fdecl)
					    (proof-refers-to fdecl)))))))))


(defmethod pc-analyze* ((decl declaration) )
  (let ((*depending-chain* *depending-chain* ))
    (cond ((memq decl *depending-chain*)
	   (pushnew decl *depending-cycles*)
	   *dependings*)
	  ((gethash decl *dependings*)
	   *dependings*)
	  (t (setf (gethash decl *dependings*) decl)
	     (push decl *depending-chain*)
	     (pc-analyze* (union (refers-to decl)
				 (remove-if-not
				     #'tcc?
				   (generated decl))))))))

(defmethod pc-analyze* ((list list))
  (cond ((null list)
	 *dependings*)
	(t (pc-analyze* (car list))
	   (pc-analyze* (cdr list)))))

(defun usedby-proofs (bufname origin line)
  (let ((udecl (get-decl-at-origin bufname origin line)))
    (if udecl
	(let ((decls (declaration-used-by-proofs-of udecl)))
	  (if decls
	      (mapcar #'(lambda (d)
			  (format-decl-list d (ptype-of d) (module d)))
		decls)
	      (pvs-message "No proofs use ~a" (id udecl))))
	(pvs-message "No declaration found near point"))))

(defun get-decl-at-origin (bufname origin line)
  (if (and (member origin '("ppe" "tccs") :test #'string=)
	   (not (get-theory bufname)))
      (pvs-message "~a is not typechecked" bufname)
      (case (intern (string-upcase origin))
	(ppe (let* ((theories (ppe-form (get-theory bufname)))
		    (decl (get-decl-at line T theories)))
	       (values (find-if #'declaration?
			 (gethash (id decl)
				  (declarations (get-theory bufname))))
		       (place decl))))
	(tccs (let* ((decls (tcc-form (get-theory bufname)))
		     (decl (find-if #'(lambda (d)
					(>= (line-end (place d)) line))
			     decls)))
		(values (find-if #'declaration?
			  (gethash (id decl)
				   (declarations (get-theory bufname))))
			(place decl))))
	(prelude (let* ((theory (get-theory bufname))
			(theories (if (and theory (generated-by theory))
				      (list theory)
				      (remove-if #'generated-by
					(nreverse (mapcar #'car
						    *prelude-names*)))))
			(decl (get-decl-at line T theories)))
		   (values decl (place decl))))
	(t (let* ((theories (list (get-theory bufname)))
		  (decl (get-decl-at line T theories)))
	     (values decl (when decl (place decl))))))))

(defun declaration-used-by-proofs-of (udecl)
  (let ((usedbys nil))
    (do-all-theories
     #'(lambda (theory)
	 (dolist (decl (all-decls theory))
	   (when (and (typep decl 'formula-decl)
		      (memq udecl (proof-refers-to decl)))
	     (push decl usedbys)))))
    usedbys))
