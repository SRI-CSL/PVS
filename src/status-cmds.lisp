;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; status-cmds.lisp -- support for the various status commands
;; Author          : Sam Owre
;; Created On      : Sat Feb 19 21:23:43 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri May 21 04:10:26 2004
;; Update Count    : 13
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

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

(defvar *unproved-dependings* nil)
(defvar *proved-dependings* nil)
(defvar *defn-dependings* nil)
(defvar *axiom-dependings* nil)
(defvar *assumption-dependings* nil)
(defvar *dependings* nil)
(defvar *depending-chain* nil)
(defvar *depending-cycles* nil)
(defvar *possible-judgements* nil)

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
    (format nil "~a is ~a~@[~a~]~@[~a~]~@[~a~]"
      theoryref
      (cond ((null theory)
	     (if (context-file-of theoryref)
		 "not yet parsed"
		 "not in the current context"))
	    ((typechecked? theory) "typechecked")
	    ((parsed? theory) "parsed")
	    (t "not yet parsed"))
      (when (and theory (memq 'modified (status theory)))
	" (decls added)")
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
			      '(AXIOM POSTULATE ASSUMPTION))))
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

(defun show-importchain (theory)
  (let ((te (get-context-theory-entry theory)))
    (if te
	(let ((*modules-visited* nil)
	      (*disable-gc-printout* t))
	  (pvs-buffer "PVS Status"
	    (with-output-to-string (*standard-output*)
	      (show-importchain* (id te)))
	    t))
	(pvs-message "~a is not in the current context" theory))))

(defun show-importchain* (tid &optional (indent 0))
  (let* ((th (get-theory tid)))
    (cond ((null th)
	   (format t "~a is not parsed" tid))
	  ((member tid *modules-visited*)
	   (format t "~vT... ~a already described~%" indent tid))
	  (t (let ((usings (when th (get-immediate-usings th))))
	       (when th (push tid *modules-visited*))
	       (format t "~vTTheory ~a~%~vT  It uses ~?~%"
		 indent (theory-status-string (id th))
		 indent *andusingctl* usings)
	       (mapc #'(lambda (m) (show-importchain* m (+ indent 2)))
		     usings))))))

(defun status-importchain (theory &optional brief?)
  (let ((te (get-context-theory-entry theory)))
    (if te
	(let ((*modules-visited* nil)
	      (*disable-gc-printout* t))
	  (pvs-buffer "PVS Status"
	    (with-output-to-string (*standard-output*)
	      (if brief?
		  (brief-status-importchain (id te))
		  (status-importchain* (id te))))
	    t))
	(pvs-message "~a is not in the current context" theory))))

(defun brief-status-importchain (tname &optional (indent 0))
  (let* ((th (get-theory tname)))
    (cond ((null th)
	   (format t "~%~a is not parsed" tname))
	  ((member tname *modules-visited* :test #'same-id)
	   (format t "~%~vT~a ... shown above" indent (id tname)))
	  (t (let ((usings (when th (get-immediate-usings th))))
	       (when (and th usings) (push tname *modules-visited*))
	       (format t "~%~vT~a" indent (id th))
	       (mapc #'(lambda (m) (brief-status-importchain m (+ indent 2)))
		     usings))))))

(defun status-importchain* (tid &optional (indent 0))
  (let* ((th (get-theory tid)))
    (cond ((null th)
	   (format t "~a is not parsed" tid))
	  ((member tid *modules-visited*)
	   (format t "~vT... ~a already described~%" indent tid))
	  (t (let ((usings (when th (get-immediate-usings th))))
	       (when th (push tid *modules-visited*))
	       (format t "~vTTheory ~a~%~vT  It uses ~?~%"
		 indent (theory-status-string (id th))
		 indent *andusingctl* usings)
	       (mapc #'(lambda (m) (status-importchain* m (+ indent 2)))
		     usings))))))


;;; Usedby Status

(defun status-importbychain (theory &optional brief?)
  (let ((te (get-context-theory-entry theory)))
    (if te
	(let ((*modules-visited* nil)
	      (*disable-gc-printout* t))
	  (pvs-buffer "PVS Status"
	    (with-output-to-string (*standard-output*)
	      (if brief?
		  (brief-status-importbychain (id te))
		  (status-importbychain* (ref-to-id theory))))
	    t))
	(pvs-message "~a is not in the current context" theory))))

(defun brief-status-importbychain (tname &optional (indent 0))
  (let* ((th (get-theory tname)))
    (cond ((null th)
	   (format t "~a is not parsed" tname))
	  ((member tname *modules-visited*)
	   (format t "~vT~a... shown above~%" indent tname))
	  (t (let ((usedbys (find-all-usedbys tname)))
	       (push tname *modules-visited*)
	       (format t "~%~vT~a" indent (id th))
	       (mapc #'(lambda (m) (brief-status-importbychain m (+ indent 2)))
		     usedbys))))))

(defun status-importbychain* (tid &optional (indent 0))
  (let ((th (get-theory tid)))
    (cond ((null th)
	   (format t "~a is not parsed" tid))
	  ((member tid *modules-visited*)
	   (format t "~vT... ~a already described~%" indent tid))
	  (t (let ((usedbys (find-all-usedbys tid)))
	       (push tid *modules-visited*)
	       (format t "~vTTheory ~a~%~vT  It is used by ~?~%"
		 indent (theory-status-string (id th))
		 indent *andusingctl* usedbys)
	       (mapc #'(lambda (m) (status-importbychain* m (+ indent 2)))
		     usedbys))))))


;;; Proof Status

(defun proof-status-at (filename declname line &optional (origin "pvs"))
  (let ((fdecl (formula-decl-to-prove filename declname line origin)))
    (if fdecl
	(pvs-message "~a is ~a." (id fdecl) (proof-status-string fdecl))
	(pvs-message "Unable to find formula declaration"))))


;;; Status Proof Theory

(defun status-proof-theory (theoryname)
  (let ((theory (or (get-theory theoryname)
		    (get-context-theory-entry theoryname)))
	(*disable-gc-printout* t))
    (if theory
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summary (id theory)))
	  t)
	(pvs-message "Theory ~a is not in the current context."
	  theoryname))))


;;; Status Proof PVS File

(defun status-proof-pvs-file (filename)
  (let ((theories (get-context-theory-names filename))
	(*disable-gc-printout* t))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summaries theories))
	  t)
	(pvs-message "File ~a.pvs is not in the current context"
	  filename))))

(defun status-proof-theories (theories)
  (if theories
      (let ((*disable-gc-printout* t))
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summaries theories))
	  t))
      (pvs-message "No theories given")))

;;; Status Proof Importchain

(defun status-proof-importchain (theoryname)
  (update-pvs-context)
  (let ((theories (context-usingchain theoryname))
	(*disable-gc-printout* t))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (proof-summaries theories))
	  t)
	(pvs-message "Theory ~a is not in the current context"
	  theoryname))))


(defun proof-summaries (theory-ids &optional filename)
  (let ((tot 0) (proved 0) (unfin 0) (untried 0) (time 0))
    (when filename
      (format t "~2%Proof summary for file ~a.pvs" filename))
    (dolist (theory theory-ids)
      (multiple-value-bind (to pr uf ut tm)
	  (proof-summary theory (when filename 2))
	(incf tot to) (incf proved pr) (incf unfin uf) (incf untried ut)
	(incf time tm)))
    (if filename
	(format t "~2%  Totals for ~a.pvs: " filename)
	(format t "~2%Grand Totals: "))
    (format t "~d proofs, ~d attempted, ~d succeeded (~,2f s)"
      tot (+ proved unfin) proved time)
    (values tot proved unfin untried time)))

(defun proof-summary (theory-id &optional (indent 0))
  (format t "~2%~vTProof summary for theory ~a" indent (ref-to-id theory-id))
  (let* ((tot 0) (proved 0) (unfin 0) (untried 0) (time 0)
	 (theory (get-theory theory-id))
	 (valid? (or (and theory
			  (from-prelude? theory))
		     (valid-proofs-file (context-entry-of theory-id)))))
    (if (and theory
	     (typechecked? theory))
	(let* ((fdecls (provable-formulas theory))
	       (maxtime (/ (reduce #'max fdecls
				   :key #'(lambda (d)
					    (or (run-proof-time d) 0))
				   :initial-value 0)
			   internal-time-units-per-second))
	       (statuslength 20) ; "proved - incomplete "
	       (dplength (+ (apply #'max
			      (mapcar #'(lambda (x) (length (string x)))
				*decision-procedures*))
			    2))
	       (timelength (length (format nil "~,2f" maxtime)))
	       (idlength (- 79 4 statuslength dplength timelength 4 3)))
	  (dolist (decl fdecls)
	    (let ((tm (if (run-proof-time decl)
			  (/ (run-proof-time decl)
			     internal-time-units-per-second 1.0)
			  0)))
	      (incf tot)
	      (cond ((proved? decl)
		     (incf proved))
		    ((justification decl) (incf unfin))
		    (t (incf untried)))
	      (incf time tm)
	      (format t "~%    ~v,1,0,'.a...~19a [~a](~a s)"
		idlength
		(id decl)
		(proof-status-string decl)
		(if (justification decl)
		    (decision-procedure-used decl)
		    "Untried")
		(if (run-proof-time decl)
		    (format nil "~v,2f" timelength tm)
		    (format nil "~v<n/a~>" timelength))))))
	(let ((te (get-context-theory-entry theory-id)))
	  (mapc #'(lambda (fe)
		    (let ((status (fe-status fe)))
		      (format t "~%    ~52,1,0,'.a...~(~10a~)"
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
		(te-formula-info te))))
    (format t "~%    Theory totals: ~d formulas, ~d attempted, ~d succeeded ~
               (~,2f s)"
	tot (+ proved unfin) proved
	(/ (reduce #'+ (provable-formulas theory)
		    :key #'(lambda (d) (or (run-proof-time d) 0))
		    :initial-value 0)
	    internal-time-units-per-second))
    (values tot proved unfin untried time)))

(defun run-proof-time (decl)
  (let ((dpr (default-proof decl)))
    (when dpr (run-time dpr))))

(defun real-proof-time (decl)
  (let ((dpr (default-proof decl)))
    (when dpr (run-time dpr))))

(defmethod provable-formulas ((theory module))
  (provable-formulas (append (assuming theory) (theory theory))))

(defmethod provable-formulas ((adt recursive-type))
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
      (proved? decl)))

(defmethod provable-formula? (obj)
  (declare (ignore obj))
  nil)

;;; ProofChain Status, Module ProofChain Status, Formula Status and
;;; Module Formula Status

(defun proofchain-status-at (filename declname line &optional (origin "pvs"))
  (if (or (gethash filename *pvs-files*)
	  (and (member origin '("ppe" "tccs") :test #'string=)
	       (get-theory filename)))
      (let ((fdecl (formula-decl-to-prove filename declname line origin))
	    (*disable-gc-printout* t))
	(if fdecl
	    (let ((*current-theory* (slot-value fdecl 'module)))
	      (pvs-buffer "PVS Status"
		(with-output-to-string (*standard-output*)
		  (pc-analyze fdecl))
		t))
	    (pvs-message "Unable to find formula declaration")))
      (pvs-message "~a.pvs has not been typechecked" filename)))

(defun status-proofchain-theory (theoryname)
  (let ((theory (get-theory theoryname))
	(*disable-gc-printout* t))
    (if theory
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (pc-analyze theory))
	  t)
	(pvs-message "~a has not been typechecked" theoryname))))

(defun status-proofchain-pvs-file (filename)
  (let ((theories (get-theories filename))
	(*disable-gc-printout* t))
    (if theories
	(pvs-buffer "PVS Status"
	  (with-output-to-string (*standard-output*)
	    (mapc #'pc-analyze theories))
	  t)
	(pvs-message "~a.pvs has not been typechecked" filename))))

(defun status-proofchain-importchain (theoryname)
  (let ((th (get-theory theoryname)))
    (if th
	(let* ((*current-theory* th)
	       (*current-context* (saved-context th))
	       (theories (collect-theory-usings theoryname))
	       (*disable-gc-printout* t))
	  (if theories
	      (pvs-buffer "PVS Status"
		(with-output-to-string (*standard-output*)
		  (mapc #'pc-analyze theories))
		t)
	      (pvs-message "Theory ~a is not in the current context"
		theoryname)))
	(pvs-message "~a has not been typechecked" theoryname))))


(defun full-status-theory (theoryname)
  (let ((theory (get-theory theoryname))
	(*disable-gc-printout* t))
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
      (format t "~%  ~a~25T~@[~a~]~50T~@[~,2,-3f~]"
	(id (car decls))
	(when (generated-by (car decls))
	  (if (declaration? (generated-by (car decls)))
	      (generated-by (car decls))
	      (id (generated-by (car decls)))))
	(typecheck-time (car decls))))
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
  (cond ((and (eq (proof-status decl) 'unchecked))
	 (not (and (mapped-formula-decl? decl)
		   (proved? decl)))
	 "unchecked")
	((proved? decl)
	 (if (mapped-formula-decl? decl)
	     "proved - by mapping"
	     (let ((complete (pc-complete decl)))
	       (if (string= complete "complete")
		   "proved - complete"
		   "proved - incomplete"))))
	((justification decl) "unfinished")
	(t "untried")))

(defun pc-complete (decl)
  (let* ((*dependings* nil)
	 (*proved-dependings* nil)
	 (*unproved-dependings* nil)
	 (*defn-dependings* nil)
	 (*axiom-dependings* nil)
	 (*assumption-dependings* nil)
	 (*depending-chain* nil)
	 (*depending-cycles* nil)
	 (*in-checker* nil)
	 ;;(*current-context* (context decl))
	 (*possible-judgements* (possible-judgements decl))
	 (fdecls (union (union (refers-to decl)
			       (proof-refers-to decl))
			(assuming-tccs decl)))
	 (decls (union fdecls *possible-judgements*)))
    (pc-analyze* decls)
    (mapc #'(lambda (y)
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
		     (push y *defn-dependings*))))
	     *dependings*)
    (cond (*depending-cycles* "circular")
	  ((and (null *unproved-dependings*)
		(proved? decl))
	   "complete")
	  (t "incomplete"))))

(defun show-proofs-pvs-file (file &optional all?)
  (let* ((all-proofs (read-pvs-file-proofs file))
	 (proofs (if all-proofs
		     (if all?
			 all-proofs
			 (proofs-with-associated-decls file all-proofs))
		     (collect-theories-proofs
		      (cdr (gethash file *pvs-files*))))))
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
	  (all-proofs
	   (pvs-message "None of the proofs in this file have a formula -~
                         include an argument to see them anyway"))
	  (t (pvs-message "No proofs found in this file")))))

(defun proofs-with-associated-decls (file proofs)
  (let ((alist (theory-formula-alist file))
	(aproofs nil))
    (dolist (thproof proofs)
      (let ((entry (assq (car thproof) alist)))
	(when entry
	  (let ((fproofs (remove-if #'(lambda (fpr)
					(not (memq (car fpr) (cdr entry))))
			   (cdr thproof))))
	    (when fproofs
	      (push (cons (car thproof) fproofs) aproofs))))))
    (nreverse aproofs)))

(defun theory-formula-alist (file)
  (let* ((theories (cdr (gethash file *pvs-files*)))
	 (ce (unless theories (context-entry-of file))))
    (cond (theories
	   (mapcar #'(lambda (th)
		       (cons (id th)
			     (mapcar #'id
			       (remove-if-not #'(lambda (d)
						  (provable-formula? d))
				 (all-decls th)))))
	     theories))
	  (ce
	   (mapcar #'(lambda (te)
		       (cons (te-id te)
			     (mapcar #'fe-id (te-formula-info te))))
	     (ce-theories ce))))))

(defun show-proofs-theory (theoryname &optional all?)
  (let* ((file (context-file-of theoryname))
	 (all-proofs (when file (read-pvs-file-proofs file)))
	 (proofs (if all-proofs
		     (if all?
			 all-proofs
			 (proofs-with-associated-decls file all-proofs))
		     (when file
		       (let ((th (get-theory theoryname)))
			 (when th
			   (collect-theories-proofs (list th))))))))
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
	  (file (pvs-message "No proofs found in this theory"))
	  (t (pvs-message "Theory not found in context; may need to retypecheck.")))))

(defun show-proofs-importchain (theoryname &optional all?)
  (let* ((imports (context-usingchain theoryname))
	 (files (delete-duplicates (mapcar #'context-file-of imports)
				   :test #'string=))
	 (valid? (every #'(lambda (ff)
			    (let ((ce (context-entry-of ff)))
			      (and ce (valid-proofs-file ce))))
			files))
	 (proofs (get-importchain-proofs theoryname imports files all?)))
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

(defun get-importchain-proofs (theoryname imports files &optional all?)
  (declare (ignore theoryname))
  (let ((all-proofs nil))
    (dolist (file files)
      (let* ((proofs (read-pvs-file-proofs file))
	     (vproofs (if proofs
			  (if all?
			      proofs
			      (proofs-with-associated-decls file proofs))
			  (collect-theories-proofs
			   (cdr (gethash file *pvs-files*))))))
	(setq all-proofs
	      (nconc all-proofs
		     (remove-if-not #'(lambda (pr)
					(member (car pr) imports
						:test #'(lambda (x y)
							  (eq x (intern y)))))
		       vproofs)))))
    all-proofs))

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

;; (defun show-all-proofs-nostatus (outstr theoryid proofs)
;;   (dolist (prf proofs)
;;     (format outstr "~3%~a.~a~2%"
;;       theoryid (car prf))
;;     (write (editable-justification
;; 	    (if (and (listp (cadr prf))
;; 		     (keywordp (caadr prf)))
;; 		(cddr prf)
;; 		(cdr prf)))
;; 	   :stream outstr :pretty t :escape t :level nil :length nil
;; 	   :pprint-dispatch *proof-script-pprint-dispatch*)))

(defun show-all-proofs-theory* (outstr proofs decls theory)
  (dolist (prf proofs)
    (let ((decl (find-if #'(lambda (d)
			     (and (typep d 'formula-decl)
				  (eq (id d) (car prf))))
		  decls)))
      (when decl
	(format outstr "~3%~a.~a: ~a [~a](~a s)~2%"
	  (id theory) (id decl)
	  (proof-status-string decl)
	  (if (justification decl) (decision-procedure-used decl) "Untried")
	  (if (run-proof-time decl)
	      (format nil "~,2,-3f" (run-proof-time decl))
	      (format nil "n/a")))
	(write (get-editable-justification
		(convert-proof-form-to-lowercase prf))
	       :stream outstr :pretty t :escape t :level nil
	       :length nil :pprint-dispatch *proof-script-pprint-dispatch*)))))

(defun show-all-proofs-theory-ctx (outstr proofs finfo thid valid?)
  (dolist (prf proofs)
    (let* ((fe (car (member (car prf) finfo
			    :test #'(lambda (x y) (eq x (fe-id y))))))
	   (status (or (and fe (fe-proof-status-string fe valid?))
		       "unchecked")))
      (format outstr "~3%~a.~a: ~a~2%" thid (car prf) status)
      (write (get-editable-justification
	      (convert-proof-form-to-lowercase prf))
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
  (let* ((*dependings* nil)
	 (*proved-dependings* nil)
	 (*unproved-dependings* nil)
	 (*defn-dependings* nil)
	 (*axiom-dependings* nil)
	 (*assumption-dependings* nil)
	 (*depending-chain* nil)
	 (*depending-cycles* nil)
	 (*current-context* (context decl))
	 (*possible-judgements* (possible-judgements decl))
	 (fdecls (union (union (refers-to decl)
			       (proof-refers-to decl))
			(assuming-tccs decl)))
	 (decls (union fdecls *possible-judgements*)))
    (pc-analyze* decls)
    (mapc #'(lambda (y)
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
       (loop for x in (pc-sort *unproved-dependings*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x)))))
    (when *proved-dependings*
      (format t "~%~%  ~a depends on the following proved theorems:"
	(id decl))
      (loop for x in (pc-sort *proved-dependings*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *axiom-dependings*
      (format t "~%~%  ~a depends on the following axioms:"
	(id decl))
      (loop for x in (pc-sort *axiom-dependings*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *defn-dependings*
      (format t "~%~%  ~a depends on the following definitions:"
	(id decl))
      (loop for x in (pc-sort *defn-dependings*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *assumption-dependings*
      (format t "~%~%  ~a depends on the following assumptions:"
	(id decl))
      (loop for x in (pc-sort *assumption-dependings*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))
    (when *possible-judgements*
      (format t "~%~%  ~a may depend on the following judgements:"
	(id decl))
      (loop for x in (pc-sort *possible-judgements*)
	     do
	     (format t "~%    ~a.~a" (id (module x)) (id x))))))

;; Lexical ordering; th1.id1 < th2.id2 if th1 precedes th2 in the
;; all-importings list, or th1 = th2 and (string id1) < (strnig id2)
(defun pc-sort (decls &optional theory)
  (assert (or theory *current-context*))
  (let* ((th (or theory (current-theory)))
	 (imps (cons th (complete-importings th))))
    (assert (every #'(lambda (x)
		       (or (from-prelude? x)
			   (from-prelude-library? x)
			   (memq (module x) imps)))
		   decls))
    (sort decls
	  #'(lambda (x y)
	      (cond ((eq (module x) (module y))
		     (string< (id x) (id y)))
		    ((from-prelude? y)
		     (or (not (from-prelude? x))
			 (string< (id (module x)) (id (module y)))))
		    ((from-prelude-library? y)
		     (and (not (from-prelude? x))
			  (or (not (from-prelude-library? x))
			      (string< (id (module x)) (id (module y))))))
		    (t (and (not (from-prelude? x))
			   (memq (module y) (memq (module x) imps)))))))))

(defun complete-importings (th)
  (multiple-value-bind (imps impnames)
      (all-importings th)
    (add-generated-adt-theories
     (cons th imps)
     (cons (mk-modname (id th)) impnames))))
    

(defun assuming-tccs (decl)
  (let ((theory-decls (all-decls (module decl))))
    (remove-if-not #'assuming-tcc?
      (ldiff theory-decls (cdr (memq decl theory-decls))))))

(defun possible-judgements (decl)
  ;;; We collect all judgements of the current context that might be
  ;;; involved in the proof of the decl.  This includes all subtype and
  ;;; number judgements, as well as any name or application judgements
  ;;; whose decl is in decls.
  ;;; [owre - 2005-09-11] Now just collect subtype judgements.
  (let ((ctx-jdecls (if (from-prelude? decl)
			(judgements-in-context decl)
			(remove-if #'from-prelude?
			  (judgements-in-context decl))))
	(jtccs nil))
    (dolist (jdecl ctx-jdecls)
      (let ((tcc (get-judgement-tcc jdecl decl)))
	(when tcc (push tcc jtccs))))
;;     (do-all-declarations #'(lambda (decl)
;; 			     (let ((tcc (get-judgement-tcc decl decls)))
;; 			       (when tcc (push tcc jtccs)))))
    jtccs))

(defun judgements-in-context (decl)
  ;; Equivalent to (judgements (context decl)), but this is much too slow
  (let* ((theory (module decl))
	 (all-decls (reverse (all-decls theory)))
	 (prev-decls (or (memq decl all-decls) (cons decl all-decls)))
	 (prev-imp (find-if #'mod-or-using? prev-decls))
	 (rem-decls (if (and prev-imp (saved-context prev-imp))
			(ldiff prev-decls (memq prev-imp prev-decls))
			prev-decls))
	 (rem-jdecls (remove-if-not #'judgement? rem-decls))
	 (ctx (cond ((and prev-imp (saved-context prev-imp))
		     (saved-context prev-imp))
		    ((from-prelude? decl)
		     (let ((prevp
			    (cadr (memq theory
					(reverse *prelude-theories*)))))
		       (saved-context
			(if (datatype? prevp)
			    (or (adt-reduce-theory prevp)
				(adt-map-theory prevp)
				(adt-theory prevp))
			    prevp))))
		    (t (or *prelude-library-context*
			   *prelude-context*))))
	 (ctx-jdecls (judgement-declarations (judgements ctx))))
    (append rem-jdecls ctx-jdecls)))
  

(defmethod get-judgement-tcc ((jdecl subtype-judgement) decl)
  ;; This one is difficult, since it is not obvious when the judgement comes
  ;; into play.  Just collect them all.
  (if (generated-by jdecl)
      (get-judgement-tcc (generated-by jdecl) decl)
      (find-if #'judgement-tcc? (generated jdecl))))

(defmethod get-judgement-tcc ((jdecl number-judgement) decl)
  ;; Similarly, don't really know when a number-judgement kicked in.
;;   (if (generated-by jdecl)
;;       (get-judgement-tcc (generated-by jdecl) decls)
;;       (find-if #'judgement-tcc? (generated jdecl)))
  )

(defmethod get-judgement-tcc ((jdecl name-judgement) decl)
  ;; Ignore it, if the associated declaration is not in decls
;;  (when (memq (declaration (name jdecl)) decls)
;;     (if (generated-by jdecl)
;; 	(get-judgement-tcc (generated-by jdecl) decls)
;; 	(when (memq (declaration (name jdecl)) decls)
;; 	  (find-if #'judgement-tcc? (generated jdecl)))))
  )

(defmethod get-judgement-tcc ((jdecl application-judgement) decl)
;;   (when (memq (declaration (name jdecl)) decls)
;;     (if (generated-by jdecl)
;; 	(get-judgement-tcc (generated-by jdecl) decls)
;; 	(find-if #'judgement-tcc? (generated jdecl))))
  )

(defmethod get-judgement-tcc ((jtcc judgement-tcc) decl)
  (get-judgement-tcc (generated-by jtcc) decl))
  
(defmethod get-judgement-tcc (decl fdecl)
  (declare (ignore decl fdecl))
  nil)
  
(defmethod pc-analyze ((decl t))
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
  (let ((*depending-chain* *depending-chain*))
    (cond ((and (not (judgement-tcc? fdecl))
		(memq fdecl *depending-chain*))
	   (pushnew fdecl *depending-cycles*)
	   *dependings*)
	  ((memq fdecl *dependings*)
	   *dependings*)
	  (t (push fdecl *dependings*)
	     (push fdecl *depending-chain*)
	     (cond ((from-prelude? fdecl)
		    *dependings*)
		   ((or (axiom? fdecl)
			(assumption? fdecl))
		    ;; No need to include proof-refers-to in this case
		    (let ((decls (union (refers-to fdecl)
					(remove-if-not #'tcc?
					  (generated fdecl)))))
		      (pc-analyze* (union decls (possible-judgements fdecl)))))
		   (t (let ((decls (union (union (refers-to fdecl)
						 (proof-refers-to fdecl))
					  (remove-if-not #'tcc?
					    (generated fdecl)))))
			(pc-analyze* (union decls (possible-judgements fdecl))))))))))


(defmethod pc-analyze* ((decl declaration))
  (let ((*depending-chain* *depending-chain*))
    (cond ((and (not (typep decl '(or def-decl fixpoint-decl)))
		(memq decl *depending-chain*))
	   (pushnew decl *depending-cycles*)
	   *dependings*)
	  ((memq decl *dependings*)
	   *dependings*)
	  (t (push decl *dependings*)
	     (push decl *depending-chain*)
	     (if (from-prelude? decl)
		 *dependings*
		 (pc-analyze* (union (refers-to decl)
				     (remove-if-not #'tcc?
				       (generated decl)))))))))

(defmethod pc-analyze* ((theory module))
  ;; From theory mappings
  nil)

(defmethod pc-analyze* ((list list))
  (cond ((null list)
	 *dependings*)
	(t (pc-analyze* (car list))
	   (pc-analyze* (cdr list)))))

(defun usedby-proofs (bufname origin line)
  (let ((udecl (get-decl-at-origin bufname origin line)))
    (when udecl
      (let ((decls (declaration-used-by-proofs-of udecl)))
	(if decls
	    (mapcar #'(lambda (d)
			(format-decl-list d (ptype-of d) (module d)))
	      decls)
	    (pvs-message "No proofs use ~a" (id udecl)))))))

(defun get-decl-at-origin (bufname origin line)
  (if (and (member origin '("ppe" "tccs") :test #'string=)
	   (not (get-theory bufname)))
      (pvs-message "~a is not typechecked" bufname)
      (case (intern (#+allegro string-downcase #-allegro string-upcase origin))
	(ppe (let* ((theories (ppe-form (get-theory bufname)))
		    (decl (get-decl-at line t theories)))
	       (values (find-if #'(lambda (d) (and (declaration? d)
						   (eq (id d) (id decl))))
			 (all-decls (get-theory bufname)))
		       (place decl))))
	(tccs (let* ((decls (tcc-form (get-theory bufname)))
		     (decl (find-if #'(lambda (d)
					(>= (line-end (place d)) line))
			     decls)))
		(values (find-if #'(lambda (d) (and (declaration? d)
						    (eq (id d) (id decl))))
			  (all-decls (get-theory bufname)))
			(place decl))))
	(prelude (let* ((theory (get-theory bufname))
			(theories (if (and theory (generated-by theory))
				      (list theory)
				      (remove-if #'generated-by
					*prelude-theories*)))
			(decl (get-decl-at line t theories)))
		   (values decl (place decl))))
	(t (if (pathname-directory bufname)
	       (let* ((lpath (get-library-reference
			      (namestring (make-pathname
					   :directory
					   (pathname-directory bufname)))))
		      (files&theories
		       (or (gethash lpath *prelude-libraries*)
			   (gethash lpath *imported-libraries*))))
		 (if files&theories
		     (let* ((name (pathname-name bufname))
			    (theories (cdr (gethash name
						    (car files&theories))))
			    (decl (get-decl-at line t theories)))
		       (values decl (when decl (place decl))))
		     (pvs-message "Library ~a is not imported" bufname)))
	       (let* ((theories (typecheck-file bufname nil nil nil t))
		      (decl (get-decl-at line t theories)))
		 (values decl (when decl (place decl)))))))))

(defun declaration-used-by-proofs-of (udecl)
  (let ((usedbys nil))
    (do-all-theories
     #'(lambda (theory)
	 (dolist (decl (all-decls theory))
	   (when (and (typep decl 'formula-decl)
		      (memq udecl (proof-refers-to decl)))
	     (push decl usedbys)))))
    usedbys))

;;; Support for browsing proofs

;;; The *show-proofs-info* variable contains the header and proofs.  The
;;; car is the string representing the header, and the cdr is the list of
;;; proofs.

(defvar *show-proofs-info* nil)

(defun display-proofs-formula-at (name declname origin line)
  (let ((fdecl (formula-decl-to-prove name declname line origin)))
    (if (null fdecl)
	(pvs-message "Not at a formula declaration")
	(let ((proofs (nontrivial-proofs fdecl)))
	  (cond ((null proofs)
		 (pvs-message "Formula ~a does not have any proofs" (id fdecl)))
		(t (setq *show-proofs-info*
			 (cons 'formula
			       (cons fdecl
				     (mapcar #'(lambda (p) (cons fdecl p))
				       proofs))))
		   (display-proofs-buffer)))))))

(defun nontrivial-proofs (decl)
  (remove-if #'(lambda (prf) (null (script prf)))
    (proofs decl)))

(defun display-proofs-theory (theoryname)
  (let ((theory (get-theory theoryname)))
    (cond (theory
	   (setq *show-proofs-info*
		 (cons 'theory
		       (cons theory
			     (mapcan #'(lambda (d)
					 (when (typep d 'formula-decl)
					   (mapcar #'(lambda (p) (cons d p))
					     (nontrivial-proofs d))))
			       (all-decls theory)))))
	   (display-proofs-buffer))
	  (t (pvs-message "~a has not been typechecked" theoryname)))))

(defun display-proofs-pvs-file (filename)
  (let ((theories (get-theories filename)))
    (cond (theories
	   (setq *show-proofs-info*
		 (cons 'pvs-file
		       (cons filename
			     (mapcan
				 #'(lambda (theory)
				     (mapcan #'(lambda (d)
						 (when (typep d 'formula-decl)
						   (mapcar #'(lambda (p)
							       (cons d p))
						     (nontrivial-proofs d))))
				       (all-decls theory)))
			       theories))))
	   (display-proofs-buffer))
	  (t (pvs-message "PVS file ~a is not in the current context"
	       filename)))))

(defun display-proofs-buffer (&optional line)
  (let ((idsize (max 8
		     (if (cddr *show-proofs-info*)
			 (apply #'max
			   (mapcar #'(lambda (fs)
				       (length (string (id (cdr fs)))))
			     (cddr *show-proofs-info*)))
			 0)))
	(declsize (max 11
		       (if (cddr *show-proofs-info*)
			   (apply #'max
			     (mapcar #'(lambda (fs)
					 (length (string (id (car fs)))))
			       (cddr *show-proofs-info*)))
			   0)))
	(thsize (max 6
		     (if (cddr *show-proofs-info*)
			 (apply #'max
			   (mapcar #'(lambda (fs)
				       (length (string (id (module (car fs))))))
			     (cddr *show-proofs-info*)))
			 0))))
    (pvs-buffer "Display Proofs"
      (format nil "~a~%~{~a~%~}"
	(display-proofs-header (car *show-proofs-info*)
			       (cadr *show-proofs-info*)
			       idsize declsize thsize)
	(proofs-formula-strings (car *show-proofs-info*)
				(cddr *show-proofs-info*)
				idsize declsize thsize))
      t t))
  (when line
    (pvs-locate "Display Proofs" nil (list line 0))))

(defparameter *proofs-format-string*
  "~a~va ~:[~*~;~:*~va ~]~:[~*~;~:*~va ~]~10a ~17a ~a")

(defun display-proofs-header (type obj idsize declsize thsize)
  (declare (ignore obj))
  (concatenate 'string
    (format nil "~?" *proofs-format-string*
	    (list "  "
		  idsize
		  "Proof Id"
		  (unless (eq type 'formula) declsize)
		  (unless (eq type 'formula) "Declaration")
		  (when (eq type 'pvs-file) thsize)
		  (when (eq type 'pvs-file) "Theory")
		  "Status"
		  "Date"
		  "Description"))
    (format nil "~%~?" *proofs-format-string*
	    (list "  "
		  idsize
		  "--------"
		  (unless (eq type 'formula) declsize)
		  (unless (eq type 'formula) "----")
		  (when (eq type 'pvs-file) thsize)
		  (when (eq type 'pvs-file) "------")
		  "------"
		  "----"
		  "----------------"))))

(defun proofs-formula-strings (type proofs idsize declsize thsize)
  (mapcar #'(lambda (prf)
	      (proof-formula-string type (car prf) (cdr prf)
				    idsize declsize thsize))
    proofs))

(defun proof-formula-string (type fdecl prf idsize declsize thsize)
  (format nil "~?" *proofs-format-string*
	  (list (if (eq prf (default-proof fdecl)) "+ " "  ")
		idsize
		(id prf)
		(unless (eq type 'formula) declsize)
		(unless (eq type 'formula) (id fdecl))
		(when (eq type 'pvs-file) thsize)
		(when (eq type 'pvs-file) (id (module fdecl)))
		(string-downcase (string (status prf)))
		(if (run-date prf)
		    (date-string (run-date prf))
		    "")
		(or (description prf) ""))))

(defun proofs-get-proof-at (line)
  (let ((pair (proofs-get-pair-at line)))
    (cond ((and (car pair) (cdr pair))
	   (values (car pair) (cdr pair)))
	  (t (pvs-message "line ~d out of range" line)
	     nil))))

(defun proofs-get-pair-at (line)
  (let ((n (+ (case (car *show-proofs-info*)
		(formula -3)
		(theory -3)
		(pvs-file -3))
	      line)))
    (when (and (<= 0 n)
	       (< n (length (cddr *show-proofs-info*))))
      (nth n (cddr *show-proofs-info*)))))

(defun set-proofs-default (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (when prf
      (setf (default-proof fdecl) prf)
      (save-all-proofs (module fdecl))
      (display-proofs-buffer line))))

(defun proofs-delete-proof (line)
  (let* ((pair (proofs-get-pair-at line))
	 (fdecl (car pair))
	 (prf (cdr pair)))
    (setf (cdr *show-proofs-info*)
	  (delete pair (cdr *show-proofs-info*)))
    (setf (proofs fdecl) (delete prf (proofs fdecl)))
    (when (eq prf (default-proof fdecl))
      (setf (default-proof fdecl) (car (proofs fdecl))))
    (save-all-proofs (module fdecl))
    (display-proofs-buffer line)))

(defun proofs-rename (line id)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (when prf
      (setf (id prf) id)
      (save-all-proofs (module fdecl))
      (display-proofs-buffer line))))

(defun proofs-show-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (when prf
      (pvs-buffer (format nil "Proof:~a" (id prf))
	(with-output-to-string (out)
	  (format out "Id: ~a~%Description: ~a~%Status: ~a~%~
                     Formula Declaration: ~a~%Decision Procedures: ~a~%~
                     Creation Date: ~a~%~
                     Date Last Run: ~a~%~
                     Run Time: ~:[Unknown~;~:*~,2,-3f seconds~]~%Proof:~%"
	    (id prf)
	    (or (description prf) "None")
	    (string-downcase (status prf))
	    (id fdecl)
	    (if (justification fdecl) (decision-procedure-used fdecl) "None")
	    (if (create-date prf)
		(date-string (create-date prf))
		"Unknown")
	    (if (run-date prf)
		(date-string (run-date prf))
		"Unknown")
	    (run-time prf))
	  (write (editable-justification (script prf))
		 :stream out :pretty t :escape t
		 :level nil :length nil
		 :pprint-dispatch *proof-script-pprint-dispatch*))
	t))))

(defun proofs-change-description (line description)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (declare (ignore fdecl))
    (when prf
      (setf (description prf) description)
      (display-proofs-buffer line))))

(defun proofs-rerun-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (when prf
      (setf (default-proof fdecl) prf)
      (let ((*current-theory* (module fdecl)))
	(read-strategies-files)
	(auto-save-proof-setup fdecl)
	(setq *last-proof* (prove (id fdecl) :strategy '(rerun)))
	(unless (from-prelude? fdecl)
	  (save-all-proofs *current-theory*)
	  ;; If the proof status has changed, update the context.
	  (update-context-proof-status fdecl))
	(remove-auto-save-proof-file))
      (let ((*to-emacs* t))
	(display-proofs-buffer line)))))

(defun proofs-edit-proof (line)
  (multiple-value-bind (fdecl prf)
      (proofs-get-proof-at line)
    (when prf
      (setq *edit-proof-info* (list fdecl (place fdecl) "Display Proofs" 0))
      (pvs-buffer "Proof"
	(with-output-to-string (out)
	  (write (editable-justification (script prf))
		 :stream out :pretty t :escape t
		 :level nil :length nil
		 :pprint-dispatch *proof-script-pprint-dispatch*))
	'popto))))

(defun show-all-proofs-nostatus (outstr theoryid proofs)
  (dolist (prf proofs)
    (format outstr "~3%~a.~a~2%"
      theoryid (car prf))
    (write (get-editable-justification
	    (convert-proof-form-to-lowercase prf))
	   :stream outstr :pretty t :escape t :level nil :length nil
	   :pprint-dispatch *proof-script-pprint-dispatch*)))

(defun get-editable-justification (prf)
  (if (integerp (cadr prf))
      (let ((mprf (nth (cadr prf) (cddr prf))))
	(editable-justification
	 (if (> (length mprf) 9) (fifth mprf) (fourth mprf))))
      ;; Old style proof - need to remove (:new-ground? t) entry if there
      (if (consp (car (cdr prf)))
	  (editable-justification (cddr prf))
	  (editable-justification (cdr prf)))))

(defmethod formula-proof-dependencies ((decl formula-decl))
  (let* ((*dependings* nil)
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
    (nreverse *dependings*)))

(defmethod formula-unused-declarations ((decl formula-decl))
  (set-difference (all-decls (module decl))
		  (cons decl (formula-proof-dependencies decl))))

(defun unusedby-proofs-of-formulas (formularefs theoryref)
  (let* ((theory (get-theory theoryref))
	 (*current-context* (context theory))
	 (fdecls (mapcar #'(lambda (fref)
			     (let* ((fname (pc-parse fref 'name))
				    (reses (formula-resolutions fname)))
			       (cond ((null reses)
				      (pvs-message "Cannot resolve ~a" fref))
				     ((cdr reses)
				      (pvs-message
					  "Multiple resolutions for ~a"
					fref))
				     (t (declaration (car reses))))))
		   formularefs))
	 (unused (unused-by-proofs-of fdecls)))
    (unless (some #'null fdecls)
      (if unused
	  (let ((flist (mapcar #'(lambda (d)
				   (format-decl-list
				    d (ptype-of d) (module d)))
			 unused)))
	    (write-declaration-info flist))
	  (pvs-message "No unused declarations found for ~a" formularefs)))))

(defun unused-by-proofs-of (fdecls)
  (let* ((used-rewrites (used-auto-rewrites fdecls))
	 (used (append (collect-proof-used-declarations fdecls) used-rewrites))
	 (unused nil))
    (do-all-declarations #'(lambda (d)
			     (let ((th (module d)))
			       (assert th)
			       (unless (or (from-prelude? th)
					   (library-datatype-or-theory? th)
					   (memq d used))
				 (pushnew d unused)))))
    (sort unused #'(lambda (x y)
		     (if (string= (id (module x)) (id (module y)))
			 (string< (id x) (id y))
			 (string< (id (module x)) (id (module y))))))))


(defun unusedby-proof-of-formula (bufname origin line)
  (let ((udecl (get-decl-at-origin bufname origin line)))
    (when udecl
      (let ((decls (unused-by-proof-of udecl)))
	(if decls
	    (let ((flist (mapcar #'(lambda (d)
				     (format-decl-list
				      d (ptype-of d) (module d)))
			   decls)))
	      (write-declaration-info flist))
	    (pvs-message "No unused declarations found for ~a" (id udecl)))))))

(defun unused-by-proof-of (decl)
  (let* ((fdecl (if (judgement? decl) (car (generated decl)) decl))
	 (*current-context* (context decl))
	 (used-rewrites (used-auto-rewrites fdecl))
	 (used (collect-proof-used-declarations fdecl))
	 (unused nil))
    (do-all-declarations #'(lambda (d)
			     (let ((th (module d)))
			       (assert th)
			       (unless (or (from-prelude? th)
					   (library-datatype-or-theory? th)
					   (memq d used))
				 (pushnew d unused)))))
    (dolist (ar (auto-rewrites *current-context*))
      (let ((th (module ar)))
	(unless (or (from-prelude? th)
		    (library-datatype-or-theory? th)
		    (memq ar used-rewrites))
	  (push ar unused))))
    (sort unused #'(lambda (x y)
		     (if (string= (id (module x)) (id (module y)))
			 (string< (id x) (id y))
			 (string< (id (module x)) (id (module y))))))))
						  

(defun used-auto-rewrites (fdecl)
  (let* ((rewrites+ (mapappend #'rewrite-names
			       (auto-rewrites *current-context*)))
	 (rewrites- (mapappend #'rewrite-names
			       (disabled-auto-rewrites *current-context*)))
	 (rewrites (set-difference rewrites+ rewrites- :test #'tc-eq)))
    (used-auto-rewrites* rewrites
			 (if (listp fdecl)
			     (remove-duplicates
				 (mapappend #'proof-refers-to fdecl))
			     (proof-refers-to fdecl)))))

(defun used-auto-rewrites* (rewrites used &optional used-rewrites)
  (if (null rewrites)
      (remove-if (complement
		  #'(lambda (ar)
		      (some #'(lambda (nm) (memq nm used-rewrites))
			    (rewrite-names ar))))
	(auto-rewrites *current-context*))
      (used-auto-rewrites*
       (cdr rewrites) used
       (if (used-auto-rewrite (car rewrites) used)
	   (cons (car rewrites) used-rewrites)
	   used-rewrites))))

(defun used-auto-rewrite (rewrite used)
  (used-auto-rewrite* (resolutions rewrite) used))

(defun used-auto-rewrite* (reses used)
  (and reses
       (or (memq (declaration (car reses)) used)
	   (used-auto-rewrite* (cdr reses) used))))

(defmethod collect-proof-used-declarations ((decl-list list))
  (collect-proof-used-decl-list decl-list nil))

(defun collect-proof-used-decl-list (decl-list used)
  (if (null decl-list)
      used
      (collect-proof-used-decl-list
       (cdr decl-list)
       (collect-proof-used-declarations* (car decl-list) used))))

(defmethod collect-proof-used-declarations ((decl formula-decl))
  (collect-proof-used-declarations* decl nil))

(defmethod collect-proof-used-declarations* ((decl declaration) used)
  (if (or (from-prelude? decl)
	  (memq decl used))
      used
      (collect-proof-used-declarations*
       (union (refers-to decl) (generated decl))
       (cons decl used))))

(defmethod collect-proof-used-declarations* ((decl formula-decl) used)
  (if (or (from-prelude? decl)
	  (memq decl used))
      used
      (collect-proof-used-declarations*
       (union (refers-to decl) (union (proof-refers-to decl) (generated decl)))
       (cons decl used))))

(defmethod collect-proof-used-declarations* (obj used)
  (declare (ignore obj used))
  (break "collect-proof-used-declarations*"))

(defmethod collect-proof-used-declarations* ((list list) used)
  (if (null list)
      used
      (collect-proof-used-declarations*
       (cdr list)
       (collect-proof-used-declarations* (car list) used))))
