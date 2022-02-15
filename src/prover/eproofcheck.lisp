;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproofcheck.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Apr  2 21:12:58 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Dec 15 00:31:15 2012
;; Update Count    : 12
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

(export '(pc-parse pc-typecheck check-arguments
	  *multiple-proof-default-behavior* *last-attempted-proof*
	  extract-justification-sexp collect-justification))

(defvar *subgoals* nil)

(defvar *multiple-proof-default-behavior* :ask
  "Defines how to handle when a proof finishes:
 :ask = asks whether to save the proof, and if yes, whether to overwrite, etc.
 :noquestions = no questions, automatically overwrites if the proof is different
 :overwrite = same as :noquestions, but sys that it is overwriting")

(defvar *default-proof-description* nil)

(defvar *record-undone-proofstate* nil)

(defvar *last-attempted-proof* nil
  "When in server mode, it stores a list (DECL SCRIPT) where DECL is the formula declaration whose proof was last attempted and SCRIPT is the proof script of such attempt.")

(defmethod prove (name &key  strategy)
  (let ((decl (get-formula (current-theory)
			   (if (stringp name)(intern name :pvs) name))))
    (if decl
	(prove-decl decl
		    :strategy
		    (when strategy `(then ,strategy (query*))))
	(error-format-if "~%No such formula.  Try again."))))

(defmethod prove ((decl formula-decl) &key  strategy)
  (prove-decl decl
	      :strategy
	      (when strategy `(then ,strategy (query*)))))

; dave_sc
(defvar *first-strategy-error* nil)
(defvar *last-strategy-error* nil)

(defvar *tccs-proved* nil)

(defun explain-errors ()
  (when *first-strategy-error*
    ;; M3 Add 'Error' prefix to commentary [Sept 2020]
    (when *last-strategy-error*
      (commentary "~%Error: The following issues occurred within the strategy:~%"))
    (commentary "~:[~;Error:~] ~a~%" (not *last-strategy-error*) *first-strategy-error*)
    (when *last-strategy-error*
      (commentary "~a~%" *last-strategy-error*))
    (clear-strategy-errors)))

(defun set-strategy-errors (message)
  (if *first-strategy-error*
      (setq *last-strategy-error* message)
      (setq *first-strategy-error* message)))

(defun clear-strategy-errors ()
  (setq *first-strategy-error* nil
        *last-strategy-error*  nil))


(defvar *steps* (init-symbol-table))
(defvar *rules* (init-symbol-table))
(defvar *dependent-decls* nil)
(defvar *rule-args-alist* nil)

(defvar *ops* (init-symbol-table))

(defun simplify-expression (expr &key module-name (strategy '(grind))
				 display? (id 'simplify-expr) interactive?)
  (assert (or module-name *current-context*))
  (let* ((*suppress-printing* (not interactive?))
	 (*printproofstate* nil)
	 (*proving-tcc* (not interactive?))
	 (*generate-tccs* t)
	 (*subgoals* t)
	 (ctheory (if module-name
		      (get-theory module-name)
		      (current-theory)))
	 (*current-context* (if module-name
				(context ctheory)
				*current-context*))
	 (expr (typecheck-uniquely (pc-parse expr 'expr)))
	 (sk-id 'F!1)
	 (bexpr (if (tc-eq (find-supertype (type expr)) *boolean*)
		    expr
		    (let* ((ftype (mk-funtype (type expr) *boolean*))
			   (skdecl (make-instance 'skolem-const-decl
				     :id sk-id
				     :type ftype
				     :module ctheory))
			   (*in-checker* t))
		      (copy-prover-context)
		      (setf (declarations-hash *current-context*)
			    (copy (current-declarations-hash)))
		      (put-decl skdecl)
		      (make!-application (typecheck (pc-parse sk-id 'expr)
					   :expected ftype)
					 expr))))
	 (cexpr (universal-closure bexpr))
	 (expr-decl (make-instance 'formula-decl
		      :id (ref-to-id id)
		      :module ctheory
		      :spelling 'formula
		      :definition cexpr
		      :closed-definition cexpr))
	 (*start-proof-display* display?)
	 ;; The next section needed for translate-to-prove, which is used
	 ;; by arith-order, make-prod, and arith-ord-translate.  This will
	 ;; be removed once term-lt is written.
	 (*translate-id-hash* (init-if-rec *translate-id-hash*))
	 (*translate-id-counter* nil)
	 (*subtype-names* nil)
	 (*local-typealist* *local-typealist*)
	 (*rec-type-dummies* nil)
	 (*named-exprs* nil))
    (newcounter *translate-id-counter*)
    (initprover)
    ;; remove above once term-lt is written
    (prove-decl expr-decl
		:strategy (unless interactive?
			    `(then (then ,strategy (postpone)) (quit)))
		:context *current-context*)
    (let ((mform (merge-subgoals *subgoals*)))
      (remove-skolem-constants
       (gensubst mform
	 #'(lambda (ex) (argument ex))
	 #'(lambda (ex) (and (application? ex)
			     (name-expr? (operator ex))
			     (eq (id (operator ex)) sk-id))))))))

(defun remove-skolem-constants (expr)
  (let* ((alist nil)
	 (nex (gensubst expr
		#'(lambda (ex)
		    (or (cdr (assoc ex alist :test #'tc-eq))
			(let* ((str (string (id ex)))
			       (id (intern (subseq str 0 (position #\! str))
					   :pvs))
			       (nid (make-new-variable id expr))
			       (nbd (mk-bind-decl nid (type ex))))
			  (push (cons ex nbd) alist)
			  nbd)))
		#'(lambda (ex)
		    (and (name-expr? ex)
			 (skolem-constant? (declaration ex)))))))
    (universal-closure nex)))

(defun simplify-expr (expr module-name strategy
			   &optional display? (id 'simplify-expr))
  ;;in the context of decl, simplify the boolean expr using strategy
  (let* ((*suppress-printing* t)
	 (*printproofstate* nil)
	 (*proving-tcc* t)
	 (*generate-tccs* t)
	 (*subgoals* t)
	 (ctheory (get-theory module-name))
	 (*current-context* (context ctheory))
	 (expr (typecheck (pc-parse expr 'expr)
		 :expected *boolean*))
	 (closed-expr (universal-closure expr))
	 (expr-decl (make-instance 'formula-decl
		      :id (ref-to-id id)
		      :module ctheory
		      :spelling 'formula
		      :definition closed-expr))
	 (*start-proof-display* display?))
    (prove-decl expr-decl :strategy `(then (then ,strategy (postpone)) (quit))
		:context *current-context*)
    *subgoals*))

(defun merge-subgoals (subgoals)
  (make!-conjunction*
   (mapcar #'(lambda (subgoal)
	       (let ((antes (mapcar #'(lambda (x)
					(negate (formula x)))
			      (neg-s-forms subgoal)))
		     (succs (mapcar #'formula (pos-s-forms subgoal))))
		 (if (null antes)
		     (make!-disjunction* succs)
		     (if (null succs)
			 (negate (make!-conjunction* antes))
			 (make!-implication
			  (make!-conjunction* antes)
			  (make!-disjunction* succs))))))
     (reverse subgoals))))

(defvar *force-dp* nil)

(defvar *dump-sequents-to-file* nil)
(defvar *show-parens-in-proof* nil)

(defmethod prove-decl :around ((decl formula-decl) &key strategy context)
  (declare (ignore strategy context))	   
  (if (or *proof-timeout*
	  (and *noninteractive*
	       *noninteractive-timeout*))
      (let ((timeout (or *proof-timeout* *noninteractive-timeout*)))
	#-sbcl
	(mp:with-timeout (timeout (pvs-message "Interrupted: ~a sec timeout"
				    timeout))
			 (call-next-method))
	#+sbcl
	(sb-ext:with-timeout timeout
	  (handler-case (call-next-method)
	    (sb-ext:timeout ()
	      (pvs-message "Interrupted: ~a sec timeout" timeout)))))
      (call-next-method)))


(defmethod prove-decl ((decl formula-decl) &key strategy context)
  (ensure-default-proof decl)
  (unless (closed-definition decl)
      (let ((*current-context* (context decl))
	    (*generate-tccs* 'none))
	(with-current-decl decl
	  (setf (closed-definition decl)
		(universal-closure (definition decl))))))
  (let* ((init-real-time (get-internal-real-time))
	 (init-run-time (get-run-time))
	 (*skovar-counter* nil)
	 (*skofun-counter* nil)
	 (*bind-counter* nil)
	 (*recursive-prove-decl-call* *in-checker*)
	 (*displaying-proof* nil)
	 (*current-displayed* nil)
	 (*flush-displayed* nil)
	 (*auto-rewrites-names* nil)
	 (*auto-rewrites-off* nil)
	 (*assert-typepreds* nil)
	 (*record-undone-proofstate* nil)
	 (*pvs-bdd-hash* nil)
	 (*bdd-pvs-hash* nil)
	 (*bdd-counter* *bdd-counter*)
	 (*track-rewrites* nil)
	 (*context-modified* nil)
	 (*generate-tccs* 'none)
	 (*rewrite-msg-off* *rewrite-msg-off*)
	 (*ruletrace* nil)
	 (*ruletracedepth* 0)
	 ;; Hash tables
	 (*assert-if-arith-hash* (make-hash-table :test #'eq))
	 (*auto-rewrites* (init-if-rec *auto-rewrites*))
	 (*auto-rewrites-ops* (init-if-rec *auto-rewrites-ops*))
	 (*all-rewrites-names* nil)
	 ;;(*beta-cache* (init-if-rec *beta-cache*))
	 ;;(*let-reduce-beta-cache* (init-if-rec *let-reduce-beta-cache*))
	 (*match-cache* (init-if-rec *match-cache*))
	 (*subtype-of-hash* (init-if-rec *subtype-of-hash*))
	 (*create-formulas-cache* (init-if-rec *create-formulas-cache*))
	 (*term-print-strings* (init-if-rec *term-print-strings*))
	 ;;
	 (*current-context* (or context (context decl)))
	 (*in-checker* t) ; Make sure this follows setting of *current-context*
	 (*current-decision-procedure* (determine-decision-procedure decl))
	 (auto-rewrites-info (with-current-decl decl (initialize-auto-rewrites)))
	 ;; The next section needed for translate-to-prove, which is used
	 ;; by arith-order, make-prod, and arith-ord-translate.  This will
	 ;; be removed once term-lt is written.
	 (*translate-id-hash* (init-if-rec *translate-id-hash*))
	 (*translate-id-counter* nil)
	 (*subtype-names* nil)
	 (*local-typealist* *local-typealist*)
	 (*rec-type-dummies* nil)
	 (*named-exprs* nil)
	 (*tccs-proved* nil))
    (reset-pseudo-normalize-caches)
    (reset-beta-cache)
    (newcounter *translate-id-counter*)
    (initprover)
    ;; remove above once term-lt is written
    (newcounter *skovar-counter*)
    (newcounter *skofun-counter*)
    (newcounter *bind-counter*)
    (with-current-decl decl
      (let* ((top-formula (closed-definition decl))
	     (s-form (make-instance 's-formula :formula top-formula))
	     (sequent (make-instance 'sequent :s-forms (list s-form)))
	     (*init-dp-state* (dpi-empty-state))
	     (*dp-state* (dpi-push-state *init-dp-state*))
	     (*top-dp-state* (dpi-push-state *init-dp-state*))
	     (*top-proofstate*
	      (make-instance 'top-proofstate
		:current-goal sequent
		:label (string (id decl))
		:strategy (if strategy
			      strategy
			      (query*-step))
		:context *current-context*
		:dp-state *dp-state*
		:justification (justification decl)
		:declaration decl
		:current-auto-rewrites auto-rewrites-info))
	     (cur-all-subst-mod-params-caches
	      (all-subst-mod-params-caches *workspace-session*))
	     (new-all-subst-mod-params-caches
	      (copy-subst-mod-params-cache)))
	(before-prove*)
	(setf (all-subst-mod-params-caches *workspace-session*)
	      new-all-subst-mod-params-caches)
	(unwind-protect
	     (dpi-start #'prove-decl-body)
	  (setf (all-subst-mod-params-caches *workspace-session*)
		cur-all-subst-mod-params-caches)
	  (after-prove*)
	  (dpi-end *top-proofstate*)
	  (unless *recursive-prove-decl-call*
	    (save-proof-info decl init-real-time init-run-time))
	  (finish-proofstate *top-proofstate*))
	*top-proofstate*))))

(defun determine-decision-procedure (decl)
  (or (if (or *force-dp*
	      *recursive-prove-decl-call*
	      (eq *default-decision-procedure* (decision-procedure-used decl))
	      (and *proving-tcc* *use-default-dp?*)
	      (and (not *proving-tcc*)
		   (pvs-yes-or-no-p
		    "~%This proof was originally done with the ~a ~
                   decision procedure,~%which is not the default.~%~
                   Do you want to use the default ~a instead? "
		    (decision-procedure-used decl)
		    *default-decision-procedure*)))
	  (car (member *default-decision-procedure* *decision-procedures*))
	  (car (member (decision-procedure-used decl) *decision-procedures*)))
      (progn (format t
		 "Can't find the ~a decision procedure, using shostak instead"
	       (decision-procedure-used decl))
	     'shostak)))

(defmethod prove-decl ((decl declaration) &key strategy)
  (declare (ignore strategy))
  (error-format-if "~%Couldn't find formula ~a in module ~a."
	     (id decl) (id (current-theory))))

(defun before-prove* ()
  (when *start-proof-display*
    (let ((*ps* *top-proofstate*))
      (call-x-show-proof)))
  (when (or (not *proving-tcc*)
	    *noninteractive*)
    (pvs-emacs-eval "(pvs-checker-busy)")))

(defun current-auto-rewrite-names ()
  (let* ((rewrites+ (mapappend #'get-rewrite-names
			       (auto-rewrites *current-context*)))
	 (rewrites- (mapappend #'get-rewrite-names
			       (disabled-auto-rewrites *current-context*)))
	 (rewrites (set-difference rewrites+ rewrites- :test #'tc-eq)))
    rewrites))

(defun initialize-auto-rewrites ()
  (let ((rewrites (current-auto-rewrite-names)))
    (catch 'abort
      (auto-rewrite rewrites *top-proofstate*)))
  (make-instance 'auto-rewrites-info
    :rewrites *auto-rewrites*
    :auto-rewrites-names *auto-rewrites-names*
    :auto-rewrites!-names *auto-rewrites!-names*
    :macro-names *macro-names*
    :all-rewrites-names *all-rewrites-names*))

(defun get-rewrite-names (rewrite-decl)
  (if (lib-datatype-or-theory? (module rewrite-decl))
      (mapcar #'(lambda (rn)
		  ;; Note that in general we can't add a library to rn,
		  ;; as it may have diverse resolutions
		  (lcopy rn
		    :resolutions
		    (mapcar #'(lambda (res)
				(let ((rth (module (declaration res))))
				  (if (lib-datatype-or-theory? rth)
				      (let ((libid (get-library-id (context-path rth))))
					(assert (and libid (symbolp libid)))
					(lcopy res
					  :module-instance
					  (lcopy (module-instance res)
					    :library libid)))
				      res)))
		      (resolutions rn))))
	(rewrite-names rewrite-decl))
      (rewrite-names rewrite-decl)))

(defun prove-decl-body ()
  (unwind-protect
      (catch 'quit			;to quit proofs
	(if *please-interrupt*
	    (prove* *top-proofstate*)
	    (with-interrupts-deferred
	     (prove* *top-proofstate*))))))

(defun after-prove* ()
  (unless *recursive-prove-decl-call*
    (when (or (not *proving-tcc*)
	      *noninteractive*)
      (pvs-emacs-eval "(pvs-checker-ready)")
      (display-proofstate nil))
    (when *dump-sequents-to-file*
      (dump-sequents-to-file *top-proofstate*)))
  (when *subgoals*
    (setq *subgoals*
	  (mapcar #'current-goal
	    (collect-all-remaining-subgoals *top-proofstate*))))
  
  (unless *recursive-prove-decl-call*
    (clear-proof-hashes)))

(defun dump-sequents-to-file (ps)
  (let* ((*prover-print-length* nil)
	 (*prover-print-depth* nil)
	 (*prover-print-lines* nil)
	 (decl (declaration ps))
         (theory (current-theory))
         (file (format nil "~a-~a.sequents" (id theory) (id decl))))
    (if (eq (status-flag *top-proofstate*) '!)
        (when (file-exists-p file)
          (delete-file file))
        (with-open-file (out file
                             :direction :output
                             :if-exists :supersede)
          (format out "~{~%~a~}" (collect-all-remaining-subgoals ps))))))

(defun clear-proof-hashes ()
  (setq *assert-if-arith-hash* (make-hash-table :test #'eq))
  (clrhash *auto-rewrites*)
  (clrhash *auto-rewrites-ops*)
  ;;(setq *beta-cache* (make-hash-table :test #'eq))
  (reset-beta-cache)
  (clrhash *match-cache*)
  (clrhash *subtype-of-hash*)
  (clrhash *create-formulas-cache*)
  (clrhash *translate-id-hash*)
  (clrhash *translate-to-prove-hash*)
  ;;(clrhash *dc-named-exprs*)
  ;;(clrhash *translate-to-dc-hash*)
  ;;(clrhash *dc-translate-id-hash*)
  ;;(clrhash *prtype-hash*)
  ;;(clrhash *local-prtype-hash*)
  (clrhash *term-print-strings*))

(defun script-structure-changed? (prinfo script)
  ;; Script of prinfo may have been installed, and not reflect
  ;; proof structure.  E.g., prinfo has 
  ;; ("" step1 step2 step3)
  ;; While script is
  ;; ("" step1 (("1" step2) ("2" step3)))
  ;; Interactively, these are the same, but rerun treats them differently
  (let ((scr-old (editable-justification (script prinfo)))
	(scr-new (editable-justification script)))
    ;; Easier to compare the editable justification, e.g.,
    ;; As is in the Emacs Proof buffer.
    (unless (equal scr-old scr-new)
      (script-structure-changed?* scr-old scr-new))))

(defun script-structure-changed?* (scr-old scr-new)
  ;; A simple test - if the old one has no branching, but the new one does
  ;; Then return t
  ;; This may be made more elaborate later
  (assert (or (equalp (car scr-old) "")
	      (and (stringp (car scr-old))
		   (char= (char (car scr-old) 0) #\;))))
  (assert (equalp (car scr-new) ""))
  (and (every #'(lambda (step)
		  (and (listp step)
		       (not (listp (car step)))))
	      (cdr scr-old))
       (not (every #'(lambda (step)
		       (and (listp step)
			    (not (listp (car step)))))
		   (cdr scr-new)))))

;; Modified by MM to inclue auto-fix [February 19, 2020]
(defun save-proof-info (decl init-real-time init-run-time)
  (let* ((prinfo (let ((sess (current-session)))
		   (if sess
		       (make-prf-info decl nil (id sess) "")
		       (default-proof decl))))
	 (script (extract-justification-sexp
		  (collect-justification *top-proofstate*)))
	 (auto-fixed-prf
	  ;; if the prf was rerun in *auto-fix-on-rerun* mode and it ended proved, save it.
	  (and *auto-fix-on-rerun*
	       (eq (status-flag *top-proofstate*) '!)
	       (not *context-modified*))))	
    (cond ((or (null (script prinfo))
	       (equal (script prinfo) '("" (postpone) nil nil))
	       (and (tcc-decl? decl)
		    (equal (script prinfo) (tcc-strategy decl)))
	       (and (eq (status prinfo) 'proved)
		    (eq (status-flag *top-proofstate*) '!)
		    (or
		     ;; next check is added to avoid crashing on malformed prf files.
		     ;; should be handled another way (TODO)
		     (not (or (equalp (car (script prinfo)) "")  
			      (and (stringp (car (script prinfo)))
				   (char= (char (car (script prinfo)) 0) #\;))))
		     (script-structure-changed? prinfo script))))
	   (setf (script prinfo) script))
	  ((and (or (not *proving-tcc*) auto-fixed-prf)
		(or (not *noninteractive*)
		    auto-fixed-prf)
		script
		(not (equal script '("" (postpone) nil nil)))
		(not (equal (script prinfo) script))
		(or (eq *multiple-proof-default-behavior* :noquestions)
		    auto-fixed-prf
		    (let ((ids (mapcar #'id
				 (remove-if-not
				     #'(lambda (prinfo)
					 (equal (script prinfo) script))
				   (proofs decl)))))
		      (pvs-yes-or-no-p
		       "~@[This proof is already associated with this formula ~
                       as ~{~a~^, ~}~%~]~
                    Would you like the proof to be saved~@[ anyway~]? "
		       ids ids))))
	   (cond ((and (not auto-fixed-prf)
		       (or (memq *multiple-proof-default-behavior*
				 '(:overwrite :noquestions))
			   (and (eq *multiple-proof-default-behavior* :ask)
				(pvs-yes-or-no-p
				 "Would you like to overwrite the current proof (named ~a)? "
				 (id prinfo)))))
		  (when (eq *multiple-proof-default-behavior* :overwrite)
		    (format t "Overwriting proof named ~a" (id prinfo)))
		  (setf (script prinfo) script))
		 ((let ((sess (current-session)))
		    (when sess
		      ;; Note that it isn't made the derault
		      (setq prinfo
			    (make-prf-info decl script (id sess) "")))))
		 (t (let ((id (read-proof-id (next-proof-id decl)))
			  (description (read-proof-description)))
		      (setq prinfo
			    (make-default-proof decl script id
						description)))))))
    ;; For some reason, this can go negative.  One possibility is that the proof
    ;; starts on one core, and finishes on another, each with it's own counter
    ;; We make sure it is not negative
    (setf (real-time prinfo) (realtime-since init-real-time))
    (setf (run-time prinfo) (runtime-since init-run-time))
    (setf (run-date prinfo) (get-universal-time))
    (when *use-default-dp?*
      (setf (decision-procedure-used prinfo) *default-decision-procedure*))
    (setf (proof-status decl)
	  (if (and (eq (status-flag *top-proofstate*) '!)
		   (not *context-modified*))
	      'proved
	      'unfinished))
    (format-nif "~%~%Run time  = ~,2,-3F secs." (run-time prinfo))
    (format-nif "~%Real time = ~,2,-3F secs.~%" (real-time prinfo))
    (when (and *context-modified*
	       (eq (proof-status decl) 'proved))
      (setf (proof-status decl) 'unfinished)
      (when (and (not *proving-tcc*)
		 (pvs-yes-or-no-p
		  "~%Context was modified in mid-proof.  ~
                     Would you like to rerun the proof?~%"))
	(let ((*in-checker* nil))
	  (prove-decl decl :strategy '(then (rerun) (query*))))))))

;; Modified by MM to inclue auto-fix [February 19, 2020]
(defun read-proof-id (default)
  (cond ((and (not *auto-fix-on-rerun*)(eq *multiple-proof-default-behavior* :ask))
	 (let ((id (pvs-dialog "Please enter an id (default ~a): " default)))
	   (cond ((equal id "") default)
		 ((valid-proof-id id) (intern id :pvs))
		 (t (format t "~a is not a legal proof identifier:~%" id)
		    (read-proof-id default)))))
	(t (format t "Saving proof as ~a" default)
	   default)))

(defun valid-proof-id (str)
  (and (alpha-char-p (char str 0))
       (every #'(lambda (ch)
		  (or (alpha-char-p ch)
		      (digit-char-p ch)
		      (member ch '(#\_ #\? #\-) :test #'char=)))
	      (subseq str 1))))

;; Modified by MM to inclue auto-fix [February 19, 2020]
(defun read-proof-description ()
  (cond ((and (not *auto-fix-on-rerun*)(eq *multiple-proof-default-behavior* :ask))
	 (let ((descr (pvs-dialog "Please enter a description~@[ (default ~s)~]: "
				  *default-proof-description*)))
	   (if (string-equal descr "")
	       (or *default-proof-description* descr)
	       descr)))
	(t (or *default-proof-description* ""))))

(defun rerun-prove (decl)
  (if (and *noninteractive*
	   (integerp *pvs-verbose*)
	   (> *pvs-verbose* 2))
      (let ((*suppress-printing* nil)
	    (*proving-tcc* t))
	(prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))
      (let ((*suppress-printing* t)
	    (*printproofstate* nil)
	    (*proving-tcc* t))
	(prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))))

(defun recheck-prove (decl)
    (let ((*suppress-printing* t)
	  (*printproofstate* nil)
	  (*proving-tcc* t)
	  (justif (justification decl))
	  )
      (cond ((typep justif 'justification)
	     (prove-decl decl
			 :strategy
			 `(then (then (rerun :recheck? t)
				 (postpone))
			   (quit))))
	    (t (let ((*proving-tcc* 'tcc));;to save new justification.
		 (rerun-prove decl))
	       (prove-decl decl
			 :strategy
			 `(then (then (rerun :recheck? t)
				 (postpone))
			   (quit)))))))


(defun prove* (proofstate)
  (let ((*current-context* (context proofstate)))
    (prove*-int proofstate)))

(defun prove*-int (proofstate)
  (setf *current-context* (context proofstate))
  (when (and (null *printproofstate*)
	     (null *suppress-printing*)
	     (fresh? proofstate)
	     (not (typep (strategy proofstate) 'strategy))
	     ;;(not (typep proofstate 'strat-proofstate))
	     ;;(not (null (strategy proofstate)))
	     ;;(NSH:11.17.94): commented out
	     )
    ;; If in a proof-session, push the proofstate to the outputs list
    (or (session-output proofstate)
	(print-proofstate-if proofstate))
    (clear-strategy-errors))
  (let ((nextstate (proofstepper proofstate)))
    (cond ((null (parent-proofstate proofstate))
	   (cond ((eq (status-flag proofstate) '!)
		  (when (typep proofstate 'top-proofstate)
		    (let ((decl (declaration proofstate)))
		      (setf (proof-refers-to decl)
			    (dependent-decls proofstate)
			    (proof-status decl) 'proved)) 
		    (format-if "~%Q.E.D.~%"))
		  proofstate)
		 ((memq (status-flag proofstate) '(X XX))
		  (when (typep proofstate 'top-proofstate)
		    (format-if "~%Failed!~%"))
		  proofstate)
		 ((and (or (fresh? proofstate)
			   (eq (status-flag proofstate) '*))
		       (null (strategy proofstate))
		       (not (typep proofstate 'top-proofstate))) 
		  (unless *suppress-printing*
		    (commentary "Subgoal completed"))
		  (setf (status-flag proofstate) '*)
		  proofstate)
		 (t (prove*-int nextstate))))
	  (t (prove*-int nextstate)))))

;;; This was pulled out to be able to capture the output for JSON (i.e., Eclipse)
;;; print-proofstate-if is a macro that eventually expands to a format, and
;;; provides no hook to work with.
(defmethod output-proofstate (proofstate)
  (print-proofstate-if proofstate))

;;12/16: need to fix prove* to save the proof, etc.

(defun check-command-arguments (cmd keywords arguments has-rest? &optional expect-key?)
  (or (null arguments)
      (if (or expect-key?
	      (keywordp (car arguments)))
	  (let ((sig (formals (prover-command-entry cmd))))
	    (cond ((not (keywordp (car arguments)))
		   (error-format-if "~%Found ~a when expecting a keyword in argument to ~a~%  signature: ~a"
				    (car arguments) cmd sig)
		   nil)
		   ((not (memq (car arguments) keywords))
		    (error-format-if "~%~a is not a valid keyword for ~a~%  signature: ~a"
				     (car arguments) cmd sig)
		    nil)
		   ((not (cdr arguments))
		    (error-format-if "~%Keyword ~a (of ~a) requires an argument"
				     (car arguments) cmd)
		    nil)
		   ((keywordp (cadr arguments))
		    (error-format-if "~%Argument to ~a (of ~a) may not be a keyword (given as ~a)"
				     (car arguments) cmd (cadr arguments))
		    nil)
		   ((and has-rest?
			 (not (cdr (memq (car arguments) keywords)))
			 (some #'keywordp (cdr arguments)))
		    (error-format-if "~%Keywords are not allowed after the start of an &rest argument")
		    nil)
		   ((memq (car arguments) (cdr arguments))
		    (error-format-if "~%Keyword ~a was specified more than once to ~a" (car arguments) cmd)
		    nil)
		   (t
		    (check-command-arguments cmd keywords (cddr arguments) has-rest? t))))
	  (check-command-arguments cmd keywords (cdr arguments) has-rest?))))

(defun prover-command-entry (cmd)
  (or (gethash cmd *rulebase*)
      (gethash cmd *rules*)
      (gethash cmd *steps*)))

(defun check-arguments (pcmd)
  (let* ((keylist (assq (car pcmd) *prover-keywords*))
	 (keywords (cddr keylist))
	 (has-rest? (cadr keylist)))
    (cond ((not keylist)
	   ;; The user may have used a $ form for a rule, remove it and check
	   ;; so we can give a more informative error message.
	   (let ((rawform (rule-rawname (car pcmd))))
	     (if (and (not (eq rawform (car pcmd)))
		      (assq rawform *prover-keywords*))
		 (error-format-if "~%~s is a rule, not a strategy, so does not have a $ form"
				  rawform)
		 (error-format-if "~%~s is not a valid prover command" (car pcmd))))
	   nil)
	  ((and (not keywords)
		(cdr pcmd))
	   (error-format-if "~%~a takes no arguments, but was given argument(s) ~a"
			    (car pcmd) (cdr pcmd))
	   nil)
	  ((singleton? pcmd))
	  (t
	   (check-command-arguments (car pcmd) keywords (cdr pcmd) has-rest?)))))

;;; This provides a hook for, e.g., JSON
(defmethod prover-read ()
  (multiple-value-bind (input err)
      (ignore-errors (read))
    (when err
      (commentary "~%~a" err))
    input))

(defun qread (prompt)
  (cond ((session-read))
	(t (format t "~%~a" prompt)
	   ;;(force-output)
	   (let ((input (prover-read)))
	     (cond ((and (consp input)
			 (eq (car input) 'lisp))
		    (format t "~%~s~%"  (eval (cadr input)))
		    (qread prompt))
		   ((member input '(quit q exit (quit) (exit) (q))
			    :test #'equal)
		    (if (or (eq *multiple-proof-default-behavior* :noquestions)
			    (pvs-y-or-n-p "~%Do you really want to quit? "))
			(throw 'quit nil)
			(qread prompt)))
		   ((eq input 'abort)
		    (if (or (eq *multiple-proof-default-behavior* :noquestions)
			    (pvs-y-or-n-p "~%Do you really want to abort? "))
			(throw 'abort nil)
			(qread prompt)))
		   ((eq input :reset) ;; from M-x reset-pvs 
		    (throw 'quit 'pvs-reset))
		   (t (auto-save-proof)
		      (if (consp input)
			  (if (check-arguments input)
			      input
			      (qread prompt))
			  input)))))))

(defvar *proof-strategy-stack* nil)

(defmethod proofstepper ((proofstate proofstate))

;;The key part of the proofstate for the stepper is the strategy field.
;;It indicates which rule to apply to the current goal, how to proceed
;;with the subgoals generated, and how to deal with failures.
;;We have to be careful to ensure that the strategies do not meddle with
;;logical things, i.e., they merely indicate which rules are applied
;;where.  So, it might be better if the strategy merely indicated the
;;top-level rule and the subgoal strategies.
;;a rule-application yields a signal to pop with failure, pop with
;;success, no change (if the rule wasn't applicable), or in the most
;;usual case, a list of subgoals.  To achieve some measure of type
;;correctness, we have a class of rules, and rule application is a
;;method for this class.
;;The defn. below is tentative.  It needs to be cleaned up later.

;;(NSH:4-10-91) I want to use strategies in two ways.  One is as a
;;strategy for applying patterns of rules, and the other is as a rule
;;itself.  The first one is invoked as change-strategy and the second
;;one as apply-strategy.  The second one generates lifts all the pending
;;subgoals back to the current proofstate and thus behaves as a derived
;;rule.  It also won't print out any output.  In replaying a proof, the
;;apply-strategies will be replayed but the change-strategies will not.
;;
  (cond
    ((fresh? proofstate)   ;;new state
     (let ((post-proofstate ;;check if current goal is prop-axiom.
	    (cond ((eq (check-prop-axiom (s-forms (current-goal proofstate)))
		       '!) ;;set flag to proved! and update fields.
		   (setf (status-flag proofstate) '!      
			 (current-rule proofstate) '(propax)
			 (printout proofstate)
			 (format nil "~%which is trivially true.")
			 (justification proofstate)
			 (make-instance 'justification
			   :label (label-suffix (label proofstate))
			   :rule '(propax)))
		   proofstate)	    ;;else display goal, 
		  ;;eval strategy, invoke rule-apply
		  (t (catch-restore ;;in case of restore/enable interrupts
		      (progn
			(when ;;(not (strat-proofstate? proofstate))
			    ;;NSH(8.19.95): display-proofstate called only
			    ;;when current state is root, or has rule/input.
			    ;;in-apply taken care of in display-proofstate. 
			    (and (not (strat-proofstate? proofstate))
				 (or (null (parent-proofstate proofstate))
				     (current-rule (parent-proofstate proofstate))
				     (current-input (parent-proofstate proofstate))))
			  (apply-proofstate-hooks proofstate))
			(let* ((*rule-args-alist* nil)
			       (strategy
				(strat-eval*
				 (if (strategy proofstate)
				     (strategy proofstate)
				     '(postpone t)) ;;if no strat, move on.
				 proofstate))
			       (*proof-strategy-stack*
				(cons strategy *proof-strategy-stack*)))
			  (setf (strategy proofstate) strategy)
			  (rule-apply strategy proofstate))))))))
       ;;rule-apply returns a revised proofstate.
       (cond ((null post-proofstate) ;;hence aborted
	      (let ((nps	     ;;NSH(7.18.94) for proper restore
		     (nonstrat-parent-proofstate
		      proofstate)))
		(setf (status-flag nps) nil
		      (remaining-subgoals nps) nil
		      (current-subgoal nps) nil
		      (pending-subgoals nps) nil
		      (done-subgoals nps) nil
		      (strategy nps)
		      (query*-step)) ;;unevaluated
		;;(query*-step)
		;;is okay here.
		nps))
	     ((eq (status-flag post-proofstate) '?)	 ;;subgoals created
	      (format-printout post-proofstate)		 ;;print commentary
	      (cond ((> (length (remaining-subgoals post-proofstate)) 1)
		     (when (and *rerunning-proof*
				(integerp *rerunning-proof-message-time*)
				(> (realtime-since *rerunning-proof-message-time*)
				   3000)) ;;print mini-buffer msg
		       (setq *rerunning-proof* (format nil "~a." *rerunning-proof*))
		       (setq *rerunning-proof-message-time*
			     (get-internal-real-time))
		       (pvs-message *rerunning-proof*))
		     (format-nif "~%this yields  ~a subgoals: "
				 (length (remaining-subgoals post-proofstate))))
		    ((not (typep (car (remaining-subgoals post-proofstate))
				 'strat-proofstate))
		     (format-nif "~%this simplifies to: ")))
	      post-proofstate)
	     ((eq (status-flag post-proofstate) '!) ;;rule-apply proved
	      ;; M3: call hooks for success [Sept 2020]
	      (dolist (hook *success-proofstate-hooks*)
		(funcall hook proofstate)) 
	      (format-printout post-proofstate)
	      (wish-done-proof post-proofstate)
	      (dpi-end post-proofstate)
					;		       (when (printout post-proofstate)
					;			 (format-if (printout post-proofstate)))
	      post-proofstate)
	     (t  post-proofstate))))
    ;;if incoming goal has subgoals
    ((eq (status-flag proofstate) '?) ;;working on subgoals
     (cond ((null (remaining-subgoals proofstate))
	    (cond ((null (pending-subgoals proofstate))
		   (success-step proofstate)) ;;no more subgoals,declare success
		  (t			      ;;pending subgoals
		   (post-processing-step proofstate))))
	   (t ;;subgoals remain
	    (let ((newps (pop (remaining-subgoals proofstate))))
	      (setf ;;(parent-proofstate newps) proofstate
	       (current-subgoal proofstate) newps)
	      ;; (substitution newps)
	      ;; (if (null (out-substitution proofstate))
	      ;;     (substitution proofstate)
	      ;;     (out-substitution proofstate))
	      ;; (context newps)
	      ;; (if (null (out-context proofstate))
	      ;;     (context proofstate)
	      ;;     (out-context proofstate))
	      
	      ;; (when (eq (status-flag newps) '*)
	      ;;   (if (null (remaining-subgoals newps));;leaf node
	      ;;       (setf (status-flag newps) nil;;make it fresh
	      ;;             (strategy newps)
	      ;;             (strategy proofstate))
	      ;;             (post-subgoal-strat (strategy proofstate))
	      ;;             nil
	      ;;       (setf (status-flag newps) '?
	      ;;       (strategy newps)
	      ;;       (strategy proofstate))
	      ;;       (post-subgoal-strat (strategy proofstate))
	      ;;       nil
	      ;;   )
	      ;;   (setf (strategy proofstate);;zero out strategy
	      ;;         (if (null (remaining-subgoals proofstate))
	      ;;             nil
	      ;;             (strategy proofstate))))
	      newps))))
   ((eq (status-flag proofstate) '*)  ;;pending goals, but proceeding
    (next-proofstate proofstate))
   ((memq (status-flag proofstate) '(X XX))  ;;failure
    (format-if "~%~%Attempted proof of ~a failed." (label proofstate))
    (next-proofstate proofstate))
   ((eq (status-flag proofstate) '!)  ;;success
    ;;(format t "~%~%Proved ~a." (label proofstate))
    (next-proofstate proofstate))
   (t (next-proofstate proofstate)))) ;;just in case

;; Allows external functions to be called, as for the wish display
;; Used for JSON output - *proofstate-hooks* are not used; see output-proofstate
(defun apply-proofstate-hooks (proofstate)
  ;; Should make this a hook instead at some point
  (display-proofstate proofstate)
  (dolist (hook *proofstate-hooks*)
    (funcall hook proofstate)))

(defun nonstrat-parent-proofstate (ps) ;;NSH(7.18.94): for restore in
				       ;;proofstepper. 
  (if (strat-proofstate? ps)
      (nonstrat-parent-proofstate (parent-proofstate ps))
      ps))

(defun format-printout (ps &optional quiet-flag)
  (let ((pp (printout ps)))
    (when (and pp
	       (or quiet-flag (not *suppress-printing*)))
      (let ((pp (if (consp pp)
		    (apply #'format nil
			   (car pp)
			   (mapcar #'(lambda (x)
				       (if (stringp x)
					   (protect-format-string x)
					   x))
				   (cdr pp)))
		    pp)))
	(if quiet-flag
	    pp
	    (unless *suppress-printing*
	      (commentary pp)))))))


(defun if-form? (x) (and (typep x 'sequence)
			 (not (null x))
			 (eq (car x) 'if)
			 (> (length x) 3)))
(defun cond-expr (x) (when (or (try-form? x)(if-form? x))(cadr x)))
(defun then-expr (x)(when (or (try-form? x)(if-form? x)) (caddr x)))
(defun else-expr (x)(when (or (try-form? x)(if-form? x))(cadddr x)))

(defun try-form? (x)(and (typep x 'sequence)
			 (not (null x))
			 (eq (car x) 'try)
			 (> (length x) 3)))

(defun defop (name formals definition)
    (add-symbol-entry name (list name formals definition) *ops*))


(defun expr-definition (name)
  (gethash name *ops*))
(defun step-definition (name)
  (gethash name *steps*))
(defun rule-definition (name)
  (gethash name *rules*))
(defun primitive-rule (name)
  (gethash name *rulebase*))

(defun new-formula-nums (goal par-goal)
  (when goal
    (new-formula-nums* (s-forms goal) (when par-goal (s-forms par-goal))
		       1 -1)))

(defun new-formula-nums* (s-forms parent-s-forms pos neg &optional result)
  (if (null s-forms)
      (nreverse result)
      (let ((sign (not (negation? (formula (car s-forms))))))
	(new-formula-nums*
	 (cdr s-forms)
	 parent-s-forms
	 (if sign (1+ pos) pos)
	 (if sign neg (1- neg))
	 (if (memq (car s-forms) parent-s-forms)
	     result
	     (cons (if sign pos neg) result))))))
	  
(defun get-parent-proofstate (ps)
  (let ((parent-ps (parent-proofstate ps)))
    (if parent-ps
	(if (current-rule parent-ps)
	    parent-ps
	    (get-parent-proofstate parent-ps))
	(if (apply-proofstate? ps)
	    (let ((apply-parent (apply-parent-proofstate ps)))
	      (if (current-rule apply-parent)
		  apply-parent
		  (get-parent-proofstate apply-parent)))
	    nil))))

(defun strat-eval* (strat ps)
  (let* ((*ps* ps)
	 (*par-ps* (get-parent-proofstate ps))
	 (* '*) ;; Causes problems when debugging, but needed to interpret :fnums *
	 (*goal* (current-goal ps))
	 (*label* (label (current-goal ps)))
					;	 (*subgoalnum* (subgoalnum ps))
	 (*+* (mapcar #'formula (pos-s-forms (current-goal ps))))
	 (*-* (mapcar #'(lambda (x) (args1 (formula x)))
		      (neg-s-forms (current-goal ps))))
	 (*par-label* (when *par-ps* (label *par-ps*)))
	 ;(label (parent-proofstate ps))
	 (*par-goal* (when *par-ps*
		       (current-goal *par-ps*)))
	 (*new-fmla-nums* (new-formula-nums *goal* *par-goal*))
	 (*current-context* *current-context*)
	 (*module-context* (copy-prover-context)))
    (strat-eval strat)))

(defun step-or-rule-defn (name)
  (or (step-definition name)(rule-definition name)))


(defun strat-eval (strat)
  (cond ((typep strat 'strategy) strat)
	((null strat) (get-rule '(skip) *ps*))
	((not (consp strat))
	 (error-format-if "~%Ill-formed rule/strategy: ~s " strat)
	 (get-rule '(skip) *ps*))
	((quote? strat)(strat-eval (cadr strat)))
	((if-form? strat) ;;(break "strat-eval: if")
	 (if (expr-eval (cadr strat))
	     (strat-eval (caddr strat))
	     (strat-eval (cadddr strat))))
	((try-form? strat)
	 (make-instance 'strategy
	   :topstep (strat-eval (cond-expr strat))
	   :subgoal-strategy (then-expr strat)
	   :failure-strategy (else-expr strat)))
	((let-form? strat)
	 (let ((let-value (let-eval (let-bindings strat))))
	   (strat-eval (subst-stratexpr
			(let-body strat)
			let-value
			(reverse let-value)))))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (let ((*suppress-printing* nil))
			    (pair-formals-args (formals def)
					       (cdr strat)
					       (car strat))))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect
			    (if (consp x)
				;;was ignoring args, otherwise.
				(cdr (assoc (car x) subalist))
				(cdr (assoc x subalist)))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr ;;2/91:so that rules are APPLYed.
		 `(apply ,def-expr))
		(result (strat-eval new-def-expr)))
	   (setq *rule-args-alist*
		 (collect-prover-input-strings args *rule-args-alist*))
	   (setf (rule-input result)
		 strat
		 (rule-format result)
		 (when (format-string (rule-definition (car strat)))
		   (cons (format-string (rule-definition (car strat))) args)
		   )) 
	   result))
	((step-definition (car strat))
	 (let* ((def (step-definition (car strat)))
		(alist (let ((*suppress-printing* nil))
			 (pair-formals-args (formals def)
					    (cdr strat)
					    (car strat))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    alist
			    (reverse alist)
			    )))
	   (strat-eval def-expr)))
	((primitive-rule (car strat))
	 (get-rule strat *ps*))
	(t (error-format-if "~%Ill-formed rule/strategy: ~s " strat)
	   (get-rule '(skip) *ps*))))

(defun let-eval (let-list &optional alist alist-as-let-binding)
  (cond ((null let-list)  alist)
	(t (let* ((letbind alist-as-let-binding)
		  (let-value
		   (expr-eval `(let* ,letbind ,(cadar let-list)))))
	     (let-eval (cdr let-list)  ;;NSH(9.12.94)
		       (append alist
			      (list 
			       (cons (caar let-list)
				     let-value)))
		       (append alist-as-let-binding
			      (list (list (caar let-list)
					  (kwote let-value)))))))))


;(defun fix-quote (expr)
;  (cond ((consp expr)
;	 (if (or (gethash (car expr) *rulebase*)
;		 (step-or-rule-defn (car expr))
;		 (not (functionp (car expr))))
;	     (list 'quote expr)
;	     expr))
;	(t (if (or (numberp expr)
;		   #+lucid (system:proclaimed-special-p expr)
;		   #+kcl (system:specialp expr))
;	       expr
;	       (list 'quote expr)))))

(defun check-keyword (key optionals)
  (cond ((null optionals) nil)
	((and (eq (car optionals) '&rest)
	      (equal (string key)
		     (string (prover-keyarg (cadr optionals)))))
	 (cadr optionals))
	((equal (string key)(string (prover-keyarg (car optionals))))
	 (car optionals))
	(t (check-keyword key (cdr optionals)))))

;;get-keyword-arg returns the keyword-arg pair and the remaining args
(defun find-keyword-arg-pair (formal args)
  (let ((akey (member formal args :test #'keyword-formal-arg-match)))
    (when akey
      (values (cons (prover-keyarg formal) (cadr akey))
	      (append (ldiff args akey) (cddr akey))))))

	 
;;; Given a list of formals of the form (a &optional (b b1) &key (c c1) &rest d)
;;; and a list of arguments of the form (x1 x2 ... xn)
;;; returns an alist of the form ((a . x1) (b . X) (c . Y) (d . Z))
;;; It does this by walking down the formals.
;;; The required args are simple, but once they are paired, the remainder
;;; could be optional by position, optional by key, simple key, or fall
;;; into the rest.  Hence valid args could be
;;;  (1)            ==> ((a . 1) (b . b1) (c . c1) (d . nil))
;;;  (1 2)          ==> ((a . 1) (b . 2)  (c . c1) (d . nil))
;;;  (1 2 3)        ==> ((a . 1) (b . 2)  (c . c1) (d . (3)))
;;;  (1 2 :c 3)     ==> ((a . 1) (b . 2)  (c . 3)  (d . nil))
;;;  (1 2 3 4 :c 5) ==> ((a . 1) (b . 2)  (c . 5)  (d . (3 4)))
;;;  (1 2 3 :c 4 5) ==> ((a . 1) (b . 2)  (c . 4)  (d . (3 5))) ???
;;;  (1 :b 2)       ==> ((a . 1) (b . 2)  (c . c1) (d . nil))
;;;  (1 :c 2)       ==> ((a . 1) (b . b1) (c . 2)  (d . nil))
;;;  (1 :c 2 :b 3)  ==> ((a . 1) (b . 3)  (c . 2)  (d . nil))
;;;  (1 2 :p 3)     ==> ((a . 1) (b . 2)  (c . c1) (d . (:p 3))) Bad keyword warning

(defun pair-formals-args (formals args strat)
  (let ((alist (pair-formals-args* formals args strat nil nil))
	;; (old-alist (unless (memq '&key formals)
	;; 	     (pair-formals-args-old formals args)))
	)
    ;; (unless (or (null old-alist)
    ;; 		(set-equal alist old-alist :test #'equal))
    ;;   (break "Mismatch in pair-formals-args and pair-formals-args-old"))
    alist))

(defun pair-formals-args* (formals args strat &optional argsec result)
  (cond ((null formals)
	 (if args
	     (progn
	       (error-format-if "~%Too many arguments ~a for prover command ~a."
				args strat)
	       (restore))
	     (nreverse result)))
	((memq (car formals) '(&optional &key &rest))
	 ;; Change argsec accordingly
	 (pair-formals-args* (cdr formals) args strat (car formals) result))
	((null argsec) ;; Required args
	 (pair-formals-required-arg formals args strat result))
	((eq argsec '&optional)
	 (pair-formals-optional-arg formals args strat result))
	((eq argsec '&key)
	 (pair-formals-key-arg formals args strat result))
	(t
	 (assert (eq argsec '&rest))
	 (pair-formals-rest-arg formals args result))))

(defun pair-formals-required-arg (formals args strat result)
  (cond ((null args)
	 (error-format-if
	  "~%Not enough arguments for prover command.")
	 (restore))
	(t (when (keywordp (car args))
	     (commentary "~%Warning: keyword ~a used for required arg"
	       (car args)))
	   (pair-formals-args*
	    (cdr formals) (cdr args) strat nil
	    (acons (prover-keyarg (car formals)) (car args) result)))))

(defun pair-formals-optional-arg (formals args strat result)
  (if (null args)
      (pair-formals-defaults formals result)
      (multiple-value-bind (pair rem-args)
	  (find-keyword-arg-pair (car formals) args)
	(cond (pair
	       ;; No longer accept positional optionals in recursive calls
	       (pair-formals-args* (cdr formals) rem-args strat '&key
				  (cons pair result)))
	      ((member (car args) formals :test #'keyword-arg-formal-match)
	       ;; Take the default, save arg for later match
	       (pair-formals-args* (cdr formals) args strat '&key
			       (cons (formal-default-pair (car formals))
				     result)))
	      (t (when (keywordp (car args))
		   (commentary "~%Warning: keyword ~a bound to ~a"
		     (car args) (prover-keyarg (car formals))))
		 (pair-formals-args* (cdr formals) (cdr args) strat '&optional
				    (acons (prover-keyarg (car formals))
					   (car args)
					   result)))))))

(defun pair-formals-key-arg (formals args strat result)
  (if (null args)
      (pair-formals-defaults formals result)
      (multiple-value-bind (pair rem-args)
	  (find-keyword-arg-pair (car formals) args)
	(if pair
	    (pair-formals-args* (cdr formals) rem-args strat '&key
				(cons pair result))
	    (pair-formals-args* (cdr formals) args strat '&key
				(cons (formal-default-pair (car formals))
				      result))))))

(defun pair-formals-rest-arg (formals args result)
  (assert (null (cdr formals)))
  (nreverse
   (cond ((null args)
	  (acons (prover-keyarg (car formals)) nil result))
	 ((keyword-formal-arg-match (car formals) (car args))
	  (when (cddr args)
	    (error-format-if
	     "~%Too many arguments for prover command.")
	    (restore))
	  (acons (prover-keyarg (car formals))
		 (if (listp (cadr args))
		     (cadr args)
		     (list (cadr args)))
		 result))
	 ((member (car formals) (cdr args) :test #'keyword-formal-arg-match)
	  (progn (error-format-if
		  "~%rest keyword :~a appears after some unmatched args"
		  (prover-keyarg (car formals)))
		 (restore)))
	 (t (acons (prover-keyarg (car formals)) args result)))))

;; No more args - pair the rest of the formals with their defaults
(defun pair-formals-defaults (formals result)
  (if (null formals)
      (nreverse result)
      (pair-formals-defaults (cdr formals)
			     (cons (formal-default-pair (car formals))
				   result))))

(defun formal-default-pair (formal)
  (cons (prover-keyarg formal)
	(prover-keydefault formal)))

;;; DELETE to next DELETE
;;; These are old, and are only here during transition to new forms
;;; that allow &key, &inherit args
;; (defun pair-formals-args-old (formals args &optional opt-flag)
;;   (cond ((null formals) nil)
;; 	((eq (car formals) '&rest)
;; 	 (if (and (consp args)
;; 		  (keywordp (car args)))
;; 	     (let ((pair (get-keyword-arg (car args)
;; 				formals
;; 				(cdr args))))
;; 	       (if (null pair)
;; 		   (commentary "~%Bad keyword in argument: ~a" (car args)))
;; 	       (if pair
;; 		   (list pair)
;; 		   (if (consp (cdr formals))
;; 		       (list (list (cadr formals)))
;; 		       nil)))
;; 	     (when (consp (cdr formals))
;; 	       (list (cons (prover-keyarg
;; 			    (cadr formals))
;; 			   (when (listp args) args))))))
;; 	((eq (car formals) '&optional)
;; 	 (pair-formals-args-old (cdr formals) args t))
;; 	((null args) (if opt-flag
;; 			 (collect-default-values formals)
;; 			 (progn (error-format-if
;; 				 "~%Not enough arguments for prover command.")
;; 				(restore))))
;; 	((and opt-flag
;; 	      (keywordp (car args)))
;; 	 (let ((pair
;; 		(get-keyword-arg (car args) formals (cdr args))))
;; 	   (if (null pair)
;; 	       (commentary "~%Bad keyword in argument: ~a" (car args)))
;; 	   (if  pair
;; 	       (cons pair
;; 		     (pair-formals-args-old (remove (car pair) formals
;; 						:key #'prover-keyarg)
;; 					(cddr args)  opt-flag))

;; 	       (cons (cons (prover-keyarg (car formals))
;; 			   (car args)) ;;NSH(3.16.95)
;; 		     (pair-formals-args-old (cdr formals)(cdr args)
;; 					opt-flag)))))
;; 	(t (cons (cons (prover-keyarg (car formals))  (car args))
;; 		 (pair-formals-args-old (cdr formals)(cdr args) opt-flag)))))

;; (defun collect-default-values (formals)
;;   (when (consp formals)
;;     (if (eq (car formals) '&rest)
;; 	(when (consp (cdr formals))
;; 	  (list (list (prover-keyarg (cadr formals)))))
;; 	(cons (cons (prover-keyarg (car formals))
;; 		    (default (car formals)))
;; 	      (collect-default-values (cdr formals))))))

;; (defun get-keyword-arg (key optionals args)
;;   (cond ((null optionals) nil)
;; 	((and (eq (car optionals) '&rest)
;; 	      (consp (cdr optionals))
;; 	      (equal (string key)
;; 		     (string (prover-keyarg (cadr optionals)))))
;; 	 (when (consp args)
;; 	   (if (listp (car args))
;; 	       (cons (prover-keyarg (cadr optionals)) (car args))
;; 	       (cons (prover-keyarg (cadr optionals))
;; 		     (list (car args))))))
;; 	((equal (string key)(string (prover-keyarg (car optionals))))
;; 	 (if (consp args)
;; 	     (cons (prover-keyarg (car optionals))(car args))
;; 	     (cons (prover-keyarg (car optionals))
;; 		   (default (car optionals)))))
;; 	(t (get-keyword-arg key (cdr optionals) args))))

;; (defun default (x)(when (and (consp x)(consp (cdr x)))(cadr x)))
;; (defun quoted? (x) (and (consp x)(eq (car x) 'quote)))
;;; DELETE

(defun subst-stratexpr (expr alist reverse-alist)
  (cond ((symbolp expr)
	 (let ((entry (assoc expr reverse-alist)))
	   (if (null entry) expr
	       (cdr entry))))
	#+sbcl ;; for e.g., ",x" allegro would have the cons "(excl::bq-comma x)"
	;; SBCL has a comma struct
	((sb-int:comma-p expr)
	 (let* ((ex (sb-int:comma-expr expr))
		(sub (subst-stratexpr ex alist reverse-alist)))
	   (if (eq sub ex)
	       expr
	       (sb-int:unquote sub))))
	((not (consp expr)) expr)
	((if-form? expr)  ;;NSH(9.12.94)
	 `(if ,(let ((letbind (loop for (x . y) in alist
			       collect (list x (kwote y)))))
		 (mk-let* letbind  (cadr expr))) 
	   ,(subst-stratexpr (caddr expr) alist reverse-alist)
	   ,(subst-stratexpr (cadddr expr) alist reverse-alist)))
	((let-form? expr);;;(break "subst-let")
	 (let ((letbind  (loop for (x . y) in alist
			       collect (list x (kwote y)))))
	   (mk-let (append letbind (let-bindings expr))
		   (let-body expr))
	   ))
	((and (consp expr)
	      (or (step-or-rule-defn (car expr))
		  (gethash (car expr) *rulebase*)))
	 (cons (car expr)
	       (loop for x in (cdr expr) collect (subst-stratexpr x alist reverse-alist))))
	((consp expr)
	 (loop for x in expr collect (subst-stratexpr x alist reverse-alist))) 
	(t expr)))
		     


(defun let-form? (x)
  (and (typep x 'list)
       (not (null x))
       (typep (cdr x) 'list)
       (> (length x) 2)
       (eq (car x) 'let)
       (loop for y in (cadr x)
	     always (and (not (null y))(typep (car y) 'symbol)))))

(defun let-bindings (x) (when (let-form? x)(cadr x)))
(defun let-body (x) (when (let-form? x)(caddr x)))

(defun quote? (x)(and (consp x)(eq (car x) 'quote)))
(defvar *ps-globals* '(*label* *subgoalnum* *goal* *par-label* *par-goal*
		       *current-context* *module-context*))
(defun global? (x)
  (memq x *ps-globals*))

(defun global-value (expr ps)
  (case expr
     (*label* (label ps))
     (*subgoalnum* (subgoalnum ps))
     (*goal* (current-goal ps))
     (*+* (mapcar #'formula (pos-s-forms ps)))
     (*-* (mapcar #'formula (neg-s-forms ps)))
     (*par-label* (when (parent-proofstate ps)
		    (label (parent-proofstate ps))))
     (*par-goal* (when (parent-proofstate ps)
		    (current-goal (parent-proofstate ps))))
     (*current-context* *current-context*)
     (*module-context* (copy-prover-context))))

(defun isfun? (sym)
  (and (symbolp sym)
       (or (fboundp sym)
	   ;;(special-form-p sym)
	   )))

(defun expr-eval (expr)
  (handler-bind ((warning #'(lambda (c)
			      (declare (ignore c))
			      (muffle-warning))))
      (cond ((consp expr)
	     (if (or (gethash (car expr) *rulebase*)
		     (step-or-rule-defn (car expr))
		     (not (isfun? (car expr))))
		 expr
		 (eval expr)))
	    (t (if (or (numberp expr)
		       (special-variable-p expr))
		   (eval expr)
		   expr)))))

(defun label-suffix (label)
  (let ((pos (position #\. label :from-end t)))
    (if pos
	(let* ((suffix (subseq label (1+ pos)))
	       (lcpos (1- (length suffix))))
	  (if (char= (char suffix lcpos) #\T)
	      (setq suffix (subseq suffix 0 lcpos)))
	  (if (every #'digit-char-p suffix)
	      suffix
	      ""))
	"")))



(defun success-step (proofstate)
  (wish-done-proof proofstate)
  (dpi-end proofstate)
  (setf (status-flag proofstate) '!
	(done-subgoals proofstate)
	(sort (done-subgoals proofstate)
	      #'mystring<= :key #'label)
	(current-subgoal proofstate) nil  ;;NSH(9.19.95) was retaining old value.
	(justification proofstate)
	(cond ((current-rule proofstate)

	       (make-instance 'justification
		 :label (label-suffix (label proofstate))
		 :rule  (sexp-unparse (current-rule proofstate))
		 :xrule (current-xrule proofstate)
		 :comment (new-comment proofstate)
		 :subgoals (mapcar #'(lambda (x) (justification x))
			     (done-subgoals proofstate))))
	      (t (justification (car (done-subgoals proofstate)))))
	)
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (y)
			  (pushnew y
				   (dependent-decls
				    proofstate)))
		      (dependent-decls x)))
	  (done-subgoals proofstate))
;;   (format t "~%decls = ~a" (dependent-decls proofstate))
  proofstate)

(defun set-decision-procedure (name)
  (assert (or (stringp name) (symbolp name)))
  (let* ((id (if (stringp name)
		 (intern (string-downcase name) :pvs)
		 name))
	 (dp (car (member id *decision-procedures*))))
    (if dp
	(if (eq id *default-decision-procedure*)
	    (pvs-message "~a is already the default decision procedure" id)
	    (progn
	      (pvs-message "~a is now the default decision procedure"
		(or (cdr (assoc id *decision-procedure-descriptions*)) id))
	      (setq *default-decision-procedure* id)
	      (when *in-checker*
		(pvs-message
		    "You must restart the current proof to use the new default"))))
	(pvs-message "~a is not a known decision procedure" id))))

(defun non-strat-subgoal-proofstate (ps)
  (if (or (not (singleton? (done-subgoals ps)))
	  (not (strat-proofstate? (car (done-subgoals ps)))))
      ps
      (non-strat-subgoal-proofstate (car (done-subgoals ps)))))

(defun report-proof (proofstate)
  (let ((*report-mode* t)
	(*top-proofstate* proofstate)
	(ps (non-strat-subgoal-proofstate proofstate)))
    (report-proof* ps)
    (when (and (typep proofstate 'top-proofstate)
	       (eq (status-flag proofstate) '!))
      (commentary "~%Q.E.D."))))

(defun collect-all-remaining-subgoals (proofstate)
  (if (eq (status-flag proofstate) '!) nil
      (let* ((subgoals  (append (pending-subgoals proofstate)
				(remaining-subgoals proofstate)))
	     (subgoals (if (current-subgoal proofstate)
			   (cons (current-subgoal proofstate) subgoals)
			   subgoals)))
	(if subgoals
	    (mapcan #'collect-all-remaining-subgoals
	      (sort subgoals #'<
		    :key #'(lambda (x)(safe-parse-integer (label x)))))
	    (list proofstate)))))

(defun bump-report-flag (flag ps)
  (if (printout ps)
      (if (null flag) 1
	  (if (eql flag 2) 0
	      (1+ flag)))
      flag))

(defun print-descendants (ps)
  (let ((done (done-subgoals ps)))
    (if (null done)
	nil
	(if (> (length done) 1)
	    done
	    (print-descendants1 (car done))))))

(defun print-descendants1 (ps)
  (let ((done (done-subgoals ps)))
    (if (null done) nil
	(if (printout ps)
	    (if (> (length done) 1)
		(list ps)
		(print-descendants2 (car done)))
	    (print-descendants1 (car done))))))

(defun print-descendants2 (ps)
  (let ((done (done-subgoals ps)))
    (if (null done) nil
	(if (printout ps)
	    done
	    (print-descendants2 (car done))))))

(defun all-subgoals (proofstate)
  (append (done-subgoals proofstate)
	  (append (pending-subgoals proofstate)
		  (if (current-subgoal proofstate)
		      (cons  (current-subgoal proofstate)
			     (remaining-subgoals proofstate))
		      (remaining-subgoals proofstate)))))

(defun all-subgoals-sorted (proofstate)
  (sort (all-subgoals proofstate) #'mystring<= :key #'label))

(defun report-proof* (proofstate &optional flag)
  (let ((all-subgoals (all-subgoals-sorted proofstate)))
    (when (or (and (or (null flag)(eql flag 0))(printout proofstate))
	      (null all-subgoals)) ;(done-subgoals proofstate)
      (let* ((ps (if (null flag) *top-proofstate* proofstate))
	     (*print-descendants*
	      (print-descendants ps)))
      (format t "~a" ps)))
	;  (when (current-rule proofstate)
	;    (format t "~%~VT~a" *prover-indent* (current-rule proofstate)))
    (when (printout proofstate)
      (if (not (null all-subgoals))
	  (if (eql flag 1)
	      (format t " then")
	      (if (eql flag 2)
		  (format t " and then"))))
      (format t "~VT~a" *prover-indent* (format-printout proofstate t)))
    (let ((*print-ancestor*
	   (if (and (or (null flag)(eql flag 0))(printout proofstate))
	       proofstate
	       *print-ancestor*))
	  (next-flag (bump-report-flag flag proofstate)))
      (if  (null all-subgoals)
	   (if (eq (status-flag proofstate) '!)
	       (commentary "~%~VTThis completes the proof of ~a.~%"
			   *prover-indent* (label proofstate))
	       (commentary "~%~VTPostponing the proof of ~a.~%"
			   *prover-indent* (label proofstate)))
	   (cond ((> (length all-subgoals) 1)
		  (commentary "~%~VTwe get ~a subgoals:"
		    *prover-indent*
		    (length all-subgoals))
		  (loop for x in  all-subgoals ;;(done-subgoals proofstate)
			do (report-proof* x 0)))
		 ((> (length (all-subgoals (car all-subgoals))) 1)
		  (commentary "~%~VTthis simplifies to:" *prover-indent*)
		  (report-proof* (car all-subgoals) 0))
		 (t (when (and (or (null next-flag)(eql next-flag 0))
			       (printout proofstate))
		      (commentary "~%~VTthis simplifies to:" *prover-indent*))
		    (report-proof* (car all-subgoals)
				   next-flag)))))))

(defun collect-justification (proofstate)
  (cond ((null (current-rule proofstate))
	 (if (done-subgoals proofstate)
	     (collect-justification (car (done-subgoals proofstate)))
	     (if (pending-subgoals proofstate)
		 (collect-justification (car (pending-subgoals
					      proofstate)))
		 (if (remaining-subgoals proofstate)
		     (collect-justification (car (remaining-subgoals
						  proofstate)))
		     (if (current-subgoal proofstate)
			 (collect-justification (current-subgoal
						 proofstate))
			 (make-instance 'justification
			   :label (label-suffix (label proofstate))
			   :comment (new-comment proofstate)
			   :rule '(postpone)))))))
	((eq (status-flag proofstate) '!)
	 (or (justification proofstate)
	     (make-instance 'justification
	       :label (label-suffix (label proofstate))
	       :rule (sexp-unparse (current-rule proofstate))
	       :xrule (current-xrule proofstate)
	       :comment (new-comment proofstate)
	       :subgoals
	       (sort 
		(mapcar #'collect-justification
		  (done-subgoals proofstate))
		#'<
		:key #'(lambda (x)(safe-parse-integer (label x)))))))
	((memq (status-flag proofstate) '(? *))
	 (make-instance 'justification
	   :label (label-suffix (label proofstate))
	   :rule (sexp-unparse (current-rule proofstate))
	   :xrule (current-xrule proofstate)
	   :comment (new-comment proofstate)
	   :subgoals
	   (let* ((current (when (current-subgoal proofstate)
			     (collect-justification (current-subgoal proofstate))))
		  (done (mapcar #'collect-justification
			  (done-subgoals proofstate)))
		  (pending (mapcar #'collect-justification
			     (pending-subgoals proofstate)))
		  (remaining (mapcar #'collect-justification
			       (remaining-subgoals proofstate)))
		  (all-but-current (append done pending remaining))
		  (all (if (and current (not (eq (status-flag proofstate) '*))
				(not (member (label current)
					     all-but-current
					     :test #'equal
					     :key #'(lambda (x) (label x)))))
			   (cons current all-but-current)
			   all-but-current)))
	     (sort all #'< :key #'(lambda (x) (safe-parse-integer (label x)))))))
	(t (make-instance 'justification
	     :label (label-suffix (label proofstate))
	     :rule '(postpone)
	     :comment (new-comment proofstate)))))



(defun mapunion (fn list)
  (cond ((null list) nil)
	(t (union (funcall fn (car list))
		  (mapunion fn (cdr list))))))

(defmethod collect-dependents ((proofstate proofstate)
			        accum)
  (mapc #'(lambda (x) (pushnew x accum :test #'eq))
	(dependent-decls proofstate))
  (collect-dependents
   (current-subgoal proofstate)
   (collect-dependents
    (done-subgoals proofstate)
    (collect-dependents
     (pending-subgoals proofstate)
     (collect-dependents
      (remaining-subgoals proofstate)
      accum)))))

(defmethod collect-dependents ((list list) accum)
  (cond ((consp list)
	 (collect-dependents (cdr list)
				  (collect-dependents (car list)
							   accum)))
	(t accum)))

(defmethod collect-dependents ((x t)  accum)
  accum)

(defun collect-dependent-decls (proofstate)
  (when proofstate
    (let ((present (dependent-decls proofstate))
	  (current (collect-dependent-decls
		    (current-subgoal proofstate)))
	  (done (mapunion #'collect-dependent-decls
			(done-subgoals proofstate)))
	  (pending (mapunion #'collect-dependent-decls
			   (pending-subgoals proofstate)))
	  (remaining (mapunion #'collect-dependent-decls
			     (remaining-subgoals proofstate))))
      (union present (union current (union done (union pending remaining)))))))



;;these make sure that the default values returned for the ops below
;;are nil.
(defmethod topstep (x) (declare (ignore x)) nil)
(defmethod subgoal-strategy (x) (declare (ignore x)) nil)
(defmethod failure-strategy (x) (declare (ignore x)) nil)

(defun post-processing-step (proofstate)
  (cond ((null (subgoal-strategy 
		(strategy proofstate)))
;	 (when (or (null (parent-proofstate
;			  proofstate))
;		   (not (equal
;			 (label proofstate)
;			 (label (parent-proofstate
;				 proofstate)))))
;	   (format-if
;		   "~%~%Unproved subgoals remain in ~a."
;		   (label proofstate)))
	 (setf (status-flag proofstate) '*);;move
	 (next-proofstate proofstate))
	(t  (setf 
	     (remaining-subgoals proofstate)
	     (transfer-subgoal-strat
	      (pending-subgoals proofstate)
	      (strategy proofstate))
	     (pending-subgoals proofstate) nil)
	    (when  (not (typep proofstate 'top-proofstate))
		(setf (strategy proofstate) nil))
	    proofstate)
	))

(defun transfer-subgoal-strat (proofstates strategy &optional accum)
  (cond ((null proofstates)
	 accum)
	((null (pending-subgoals (car proofstates)))
	 (setf (strategy (car proofstates))
	       (subgoal-strategy strategy)
	       (status-flag (car proofstates)) nil)
	 (transfer-subgoal-strat (cdr proofstates) strategy
				 (cons (car proofstates) accum)))
	(t (setf (strategy (car proofstates)) strategy
		 (status-flag (car proofstates)) '?)
	   (transfer-subgoal-strat (cdr proofstates) strategy
				 (cons (car proofstates) accum)))))
	
	

(defun get-post-strat (proofstate)
  (if (or (null (strategy proofstate))
	  (null (post-subgoal-strat proofstate)))
      (if (strategy-name? (car (current-rule proofstate)))
	  (let ((par-ps (parent-proofstate proofstate)))
	    (if (or (null par-ps)
		    (null (strategy par-ps)))
		nil
		(subgoal-strategy-function par-ps)))
	  nil)
      (post-subgoal-strat proofstate)))

(defmethod set-post-strat ((ps proofstate) stratfun)
  (cond ((null (remaining-subgoals ps))
	 (setf (status-flag ps) nil
	       (strategy ps)
	       (funcall stratfun ps)))
	(t   (setf (status-flag ps) '?
		   (strategy ps) nil)
	     (set-post-strat (remaining-subgoals ps) stratfun)))
  ps)

(defmethod set-post-strat ((pslist list) stratfun)
  (loop for ps in pslist do (set-post-strat ps stratfun)))


(defun get-subgoal-strat (subgoal proofstate)
  (let ((strategy-fun (subgoal-strategy-function (strategy proofstate))))
    (if (null strategy-fun) (query*-step)
	(funcall strategy-fun subgoal))))





;;The idea now is that the proofstepper employs the strategy to select
;;and apply rule.  The next-proofstate is employed to construct the
;;output the proofstepper.  The difficulty now is that when a subgoal
;;fails, there are two possibilities: one, is to try something else on
;;that subgoal, and the other is to try some other rule on the current
;;goal.  Trying something else on the subgoal should be a part of the
;;strategy for that subgoal, so the only option is to either signal
;;failure for the current goal, try some other rule, or proceed.  Should
;;I try to distinguish between a failed subgoal and one that has just
;;been set aside?  I think so, since calling it a failure might cause
;;some higher step to be redone.  



(defmethod next-proofstate ((ps proofstate))
  (let ((par-ps (parent-proofstate ps))
	(sflag (status-flag ps)))
    (cond ((null par-ps)    ;;hence root proof state
	   (if  (eq sflag '*)
		(setf (status-flag ps) 0
		      (strategy ps)
		      (subgoal-strategy (strategy ps))))
	   ps)  ;;else, we have failed or succeeded.
	  (t (cond ((memq sflag '(X XX))   ;;record failure or try something else.
		    (if (or (null (failure-strategy (strategy par-ps)))
			    (eq sflag 'XX))
			(setf (status-flag par-ps) 'X)
			(setf (status-flag par-ps) 0
			      (strategy par-ps)
			      (failure-strategy (strategy par-ps))
			      (done-subgoals par-ps) nil
			      (pending-subgoals par-ps) nil
			      (remaining-subgoals par-ps) nil
			      (current-subgoal par-ps) nil)))
		   ((eq sflag '*)
		    (cond ((and (typep ps 'strat-proofstate)
				(null (pending-subgoals ps)))
			   (setf (status-flag par-ps) 0
				 (strategy par-ps)
				 (failure-strategy (strategy par-ps))
				 (current-subgoal par-ps) nil))
			  (t (push ps (pending-subgoals par-ps))
			     (setf (current-subgoal par-ps) nil))))
;		    (setf  ;; don't need this (status-flag par-ps) '?
;			  (out-substitution par-ps)(out-substitution ps)
;			  (out-context par-ps)(out-context ps)
;			  ;;[12-28](status-flag ps) nil ;;make it fresh
;			  )
;			(push ps (pending-subgoals par-ps))
		   ((eq  sflag '!)
		    (if (not (equal (label par-ps) (label ps)))
			(format-if "~%~%This completes the proof of ~a.~%"
				   (label ps)))
		    ;; (when (tcc-sequent? ps)
		    ;;   (push ps *proved-tccs*))
;		    (setf (out-substitution par-ps)(out-substitution ps)
;			  (out-context par-ps)(out-context ps))
		    (push ps (done-subgoals par-ps))))
                     ;;this last case should not arise.
		   ;;(t (setf (substitution par-ps)(substitution ps)))
	     par-ps))))

;;Okay, we now look at rules before we look at strategies.  Remember
;;that rules require caution, whereas strategies can only determine
;;which rule to apply when, and where to go from there.  A rule takes a
;;proofstate and returns a signal, a list of subgoal sequents, and a
;;substitution.  The rule-apply function takes these and forms a new
;;proofstate.  


(defun make-updates (updates ps)
  (cond ((null updates) ps)
	(t (let ((field (car updates)))
	     (make-update field (cadr updates) ps))
	   (make-updates (cddr updates) ps))))

(defun make-update (field value ps)
  ;;(break)
  (case field
    (status-flag (setf (status-flag ps) value))
    (strategy (setf (strategy ps) value))
    (context (setf (context ps) value))
    (dp-state (setf (dp-state ps) value))
    (current-auto-rewrites (setf (current-auto-rewrites ps) value))
    (rewrite-hash (setf (rewrite-hash ps) value))
    (subtype-hash (setf (subtype-hash ps) value))
    (dependent-decls (setf (dependent-decls ps) ;;NSH(4.21.95):special
			   (union value (dependent-decls ps))))
    (justification (setf (justification ps) 
			 value)) ;;NSH(3.16.97) added for checkpointing
    (current-xrule (setf (current-xrule ps) value))
    (comment (setf (comment ps) value))))

(defun get-rule (rule ps)
  (let* ((rule-name (if (consp rule)(car rule) rule))
	 (rule-args (when (consp rule)(cdr rule)))
	 (entry (gethash rule-name *rulebase*)))
    (cond ((null entry)
	   (get-rule '(skip) ps))
	  ((< (length rule-args) (length (required-args entry)))
	   (cond
	    (*noninteractivemode* (get-rule '(skip) ps))
	    (t
	     (let ((formals (if (optional-args entry)
				(append (required-args entry)
					(cons '&optional
					      (optional-args entry)))
				(required-args entry))))
	       (format-if "~%Rule ~a has argument list:~%~a"
			  rule-name  formals)
	       (get-rule '(skip) ps)))))
	  (t (let* ((match (match-formals-with-actuals (required-args entry)
						       (optional-args entry)
						       rule-args rule-name))
		    (args (nconc (loop for x in (required-args entry)
				       collect
				       (cdr (assoc x match)))
				 (extract-optionals
				  (optional-args entry)
				  match))))
	       (setq *rule-args-alist*
		     (collect-prover-input-strings rule-args
						   *rule-args-alist*))
	       (make-instance 'rule-instance
		 :rule (apply (rule-function entry) args)
		 :rule-input rule
		 :rule-format (when (format-string entry)
				(cons (format-string entry)
				      args))))))))

(defun extract-optionals (optionals match)
  (cond ((null optionals) nil)
	((eq (car optionals) '&rest)
	 (when (cdr optionals)
	   (cdr (assoc (prover-keyarg (cadr optionals)) match))))
	(t (cons (cdr (assoc (prover-keyarg (car optionals))
			     match))
		 (extract-optionals (cdr optionals) match)))))

;	   (rulefun-entry (get-rule (funcall
;				     (apply (rulefun rulefun-entry)
;					    rule-args)
;				     ps)
;				    ps))
;	   (strategy-entry (make-instance
;			    'strategy-instance
;			    :strategy-fun
;			    (funcall (apply
;				      (strategy-fun strategy-entry)
;				      rule-args)
;				     ps)
;			    :strategy-input rule))
					;	   (t )

(defun assert-tccforms (tccforms ps)
  (when tccforms
    (let* ((dp-state (dp-state ps)))
      (nprotecting-cong-state
       ((*dp-state* dp-state))
       ;;(break "atc")
       (let ((*rewrite-hash* (copy (rewrite-hash ps)))
	     (*subtype-hash* (copy (subtype-hash ps)))
	     (*use-rationals* t))
	 (assert-tccforms* tccforms ps))))))

(defun assert-tccforms* (tccforms ps)
  (if (null tccforms) nil
      (multiple-value-bind (sig value)
	  (let ((*sequent-typealist* nil))
	    (assert-if (tccinfo-formula (car tccforms))))
	(cond ((tc-eq value *true*)
	       (when *report-tccs*
		 (print-tccinfo (car tccforms) t))
	       (assert-tccforms* (cdr tccforms) ps))
	      ((eq sig 'X)
	       (when *report-tccs*
		 (print-tccinfo (car tccforms) nil))
	       (cons (car tccforms)
		     (assert-tccforms*
		      (cdr tccforms) ps)))
	      (t (when (tc-eq value *false*)
		   (error-format-if
		    "~%*WARNING*: TCC ~a ~%~11Tsimplifies to FALSE"
		    (tccinfo-formula (car tccforms))))
		 (setf (tccinfo-formula (car tccforms)) value)
		 (when *report-tccs*
		   (print-tccinfo (car tccforms) nil))
		 (cons (car tccforms)
		       (assert-tccforms* (cdr tccforms) ps)))))))

(defun rule-apply (step ps)
  (let* ((*ps* ps)
	 (* '*) ;; Causes problems when debugging, but needed to interpret :fnums *
	 (*goal* (current-goal ps))
	 (*label* (label  ps))
	 ;; (*subgoalnum* (subgoalnum ps))
	 (*+* (mapcar #'formula (pos-s-forms (current-goal ps))))
	 (*-* (mapcar #'formula (neg-s-forms (current-goal ps))))
	 (*par-label* (when (parent-proofstate ps)
			(label (parent-proofstate ps))))
	 (*par-goal* (when (parent-proofstate ps)
		       (current-goal (parent-proofstate ps))))
	 (*current-context* *current-context*)
	 (*module-context* (copy-prover-context))
	 (*auto-rewrites* (rewrites ps))
	 (*all-rewrites-names* (all-rewrites-names ps))
	 (*auto-rewrites-names* (auto-rewrites-names ps))
	 (*auto-rewrites!-names* (auto-rewrites!-names ps))
 	 (*macro-names* (macro-names ps))	 
	 (*rewrite-hash* (rewrite-hash ps))
	 (*dp-state* (dp-state ps)))
    (cond ((typep step 'rule-instance);;if step is a rule, then
	   ;;reinvoke rule-apply with corresponding strategy. 
	   (rule-apply (make-instance 'strategy
			 :topstep step)
		       ps));;else step is a strategy
	  ((typep (topstep step) 'rule-instance)
	   (let* ((*tccforms* nil)
		  (topstep (topstep step))
		  (name (if (consp (rule-input topstep))
			    (car (rule-input topstep))
			    topstep))
		  (*suppress-printing* (or *suppress-printing*
					   (eq name 'lisp))))
	     (when (memq name *ruletrace*)
	       (commentary "~%~vTEnter: ~a" *ruletracedepth*
		       (rule-input topstep))
	       (incf *ruletracedepth*))
	     (multiple-value-bind (signal subgoals updates)
		 (funcall (rule topstep) ps) ;;(break "rule-ap")
	       (cond ((eq signal '!)	     ;;success
		      (when (memq name *ruletrace*)
			(decf *ruletracedepth*)
			(commentary "~%~vT Exit: ~a -- Proved subgoal"
			  *ruletracedepth* name ))
		      (when (eq (car (rule-input topstep)) 'apply)
			(let ((timeout-sect
			       (memq :timeout (rule-input topstep))))
			  (when timeout-sect
			    (setf (rule-input topstep)
				  (append (ldiff (rule-input topstep)
						 timeout-sect)
					  (cddr timeout-sect))))))
		      (setf (status-flag ps) '!      
			    (current-rule ps) (rule-input topstep)
			    (parsed-input ps) (sublis (remove-if #'null
							*rule-args-alist*
							:key #'cdr)
						      (rule-input topstep))
			    (printout ps) (sublis (remove-if #'null
						    *rule-args-alist*
						    :key #'cdr)
						  (rule-format topstep))
			    (justification ps) (make-instance 'justification
						 :label (label-suffix (label ps))
						 :rule (rule-input topstep)
						 :comment (new-comment ps)))
		      (make-updates updates ps)
		      ps)
		     ((eq signal '?) ;;subgoals generated
		      (make-updates updates ps)
		      ;;(NSH:5/1/99)make-updates should be above
		      ;;make-subgoal-proofstates
		      ;;so that the proof-dependent-decls can be computed.
		      (let* ((*tccforms* (remove-duplicates *tccforms*
					   :test #'tc-eq
					   :key #'tccinfo-formula))
			     (tcc-hash-counter 0)
			     (*tccforms*
			      (loop for tcc in *tccforms*
				 when (or
				       (null (gethash
					      (tccinfo-formula tcc)
					      (tcc-hash ps)))
				       (and (incf tcc-hash-counter)
					    nil))
				 collect tcc))
			     (new-tcc-hash
			      (if *tccforms*
				  (copy (tcc-hash ps))
				  (tcc-hash ps)))
			     (tccforms (assert-tccforms *tccforms* ps))
			     (false-tccforms
			      (loop for tccform in *tccforms*
				 as assert-tccform in tccforms
				 when (tc-eq (tccinfo-formula assert-tccform)
					     *false*)
				 collect tccform)))
			(cond (false-tccforms
			       ;;treated as a skip
			       (unless *suppress-printing*
				 (commentary "~%No change. False TCCs: ~%~{  ~a, ~%~}"
				   (mapcar #'tccinfo-formula false-tccforms)))
			       (setf (status-flag ps) nil ;;start afresh
				     (strategy ps)
				     (failure-strategy step))
			       ps)
			      (t (let* ((tcc-subgoals
					 (make-tcc-subgoals tccforms ps))
					(subgoal-proofstates
					 (make-subgoal-proofstates
					  ps
					  (subgoal-strategy step)
					  subgoals
					  tcc-subgoals
					  ;;updates;must be attached to subgoals.
					  )))
				   ;;cleaning up (NSH 7.27.94)
				   ;;1. convert main subgoals of tccs into
				   ;;non-tccs.
				   ;;2. hash the new tccs into new-tcc-hash
				   ;;3. set tcc-hash of main subgoals as
				   ;;new-tcc-hash
				   (when (> tcc-hash-counter 0)
				     (format-if "~%Ignoring ~a repeated TCCs."
						tcc-hash-counter))
				   (loop for tcc in *tccforms*
				      do
					(setf (gethash (tccinfo-formula tcc)
						       new-tcc-hash)
					      t))
				   (assert
				    (every #'(lambda (sps)
					       (every #'(lambda (sfmla)
							  (null (freevars (formula sfmla)))
							  )
						      (s-forms (current-goal sps))))
					   subgoal-proofstates))
				   (loop for sps in subgoal-proofstates
				      when (not (tcc-proofstate? sps))
				      do (setf (tcc-hash sps)
					       new-tcc-hash))
				   (when (memq name *ruletrace*)
				     (decf *ruletracedepth*)
				     (commentary "~%~vT Exit: ~a -- ~a subgoal(s) generated."
				       *ruletracedepth* name (length subgoal-proofstates)))
				   (push-references *tccforms* ps)
				   (when (eq (car (rule-input topstep)) 'apply)
				     (let* ((rinput (rule-input topstep))
					    (mtimeout (memq :timeout rinput)))
				       (if mtimeout
					   ;; Given by keyword
					   (setf (rule-input topstep)
						 (append (ldiff rinput mtimeout)
							 (cddr mtimeout)))
					   ;; Positional
					   (when (and (not (some #'keywordp rinput))
						      (> (length rinput) 4))
					     (assert (= (length rinput) 5))
					     (setf (rule-input topstep)
						   (butlast rinput))))))
				   (setf (status-flag ps) '?
					 (current-rule ps) (rule-input topstep)
					 (parsed-input ps) (sublis (remove-if #'null
								     *rule-args-alist*
								     :key #'cdr)
								   (rule-input topstep))
					 (printout ps) (sublis (remove-if #'null
								 *rule-args-alist*
								 :key #'cdr)
							       (rule-format topstep))
					 (remaining-subgoals ps) subgoal-proofstates)
				   (unless (typep ps 'top-proofstate)
				     (setf (strategy ps) nil))
				   ps)))))
		     ((eq signal 'X)
		      (when (memq name *ruletrace*)
			(decf *ruletracedepth*)
			(commentary "~%~vT Exit: ~a -- No change."
			  *ruletracedepth* name))
		      (unless *suppress-printing*
			(explain-errors)
			(commentary "~%No change on: ~s" (rule-input topstep)))
		      (setf (status-flag ps) nil ;;start afresh
			    (strategy ps)
			    (failure-strategy step))
		      (make-updates updates ps)
		      ps)
		     ((eq signal 'XX) ;;marks the current goal a failure
		      (setf (status-flag ps) 'XX)
		      ps)
		     ((eq signal '*)
		      (setf (status-flag ps) '*)
		      ps)
		     (t (undo-proof signal ps))))))
	  ((typep (topstep step) 'strategy)
	   (setf (status-flag ps) '?
		 ;;		 (current-rule ps) (strategy-input rule)
		 (remaining-subgoals ps)
		 (make-subgoal-proofstates ps
					   (topstep step)
					   (list (current-goal ps))))
					;nil
	   ps)
	  (t (commentary "~%Bad rule: ~a~%" step);;treated as skip
					;(break)
	     (setf (status-flag ps) nil;;start afresh
		   (strategy ps)
		   (failure-strategy (strategy ps)))
	     ps))))

(defun make-tcc-subgoals (tccforms ps)
  (mapcar #'(lambda (x)
	      (let ((y (change-class
			   (copy (current-goal ps)
			     's-forms (cons
				       (make-instance
					   's-formula
					 :formula
					 (tccinfo-formula x))
				       (s-forms
					(current-goal ps))))
			   'tcc-sequent)))
		(setf (tcc y) (tccinfo-formula x)
		      (reason y) (tccinfo-reason x)
		      (expr y) (tccinfo-expr x)
		      (kind y) (tccinfo-kind x)
		      (type y) (tccinfo-type x))
		y))
    tccforms))

(defun assert-test-list (fmla-list ps)
  (let* ((dp-state (dp-state ps)))
    (nprotecting-cong-state
     ((*dp-state* dp-state))
     (let ((*rewrite-hash* (copy (rewrite-hash ps)))
	   (*subtype-hash* (copy (subtype-hash ps))))
       (loop for fmla in fmla-list
	     nconc
	     (multiple-value-bind (sig value)
		 (assert-if fmla)
	       (cond ((tc-eq value *true*) nil)
		     ((eq sig 'X) (list fmla))
		     (t (list value)))))))))




(defun undo-proof (info ps)
  (if (eq info 'undo)
      (cond ((and *record-undone-proofstate*
		  (eq ps (car *record-undone-proofstate*)))
	     (let ((oldps (cadr *record-undone-proofstate*))
		   (newps (caddr *record-undone-proofstate*)))
	       (commentary "~%Restoring the proof to state prior to UNDO, ")
	       (setf (justification ps)
		     (justification oldps)
		     (status-flag ps) (status-flag oldps)
		     (remaining-subgoals ps) (remaining-subgoals oldps)
		     (pending-subgoals ps) (pending-subgoals oldps)
		     (done-subgoals ps) (done-subgoals oldps)
		     (current-subgoal ps) (current-subgoal oldps)
		     (current-rule ps) (current-rule oldps)
		     (current-xrule ps) (current-xrule oldps)
		     (printout ps) (printout oldps)
		     (strategy ps) (strategy oldps))
	       (setf (strategy newps)(query*-step))
	       newps))
	    (t (if *record-undone-proofstate*
		   (commentary "~%Undo operations must be immediately undone.")
		   (commentary "~%No undo to undo."))
	       (setf (strategy ps) (query*-step));;(NSH:5/8/99)
	       ps))
      (let ((newps (findps info ps)))
	(cond ((null newps)
	       (commentary "~%Sorry. Couldn't find such a proof state.")
	       (setf (strategy ps) (query*-step))
	       ps)
	      ((eq ps newps)
	       (commentary "~%No change.")
	       (setf (strategy ps)(query*-step))
	       ps)
	      (t (let ((response
			(or (eq *multiple-proof-default-behavior* :noquestions)
			    (pvs-y-or-n-p "~%This will undo the proof to: ~a~%Sure? "
					  newps))))
		   (cond (response
			  (when *displaying-proof*
			    (setf *flush-displayed* newps))
			  (setq *record-undone-proofstate*
				(list newps (copy newps) ps))
			  (setf (justification newps)
				(collect-justification newps)
				(status-flag newps) nil
				(remaining-subgoals newps) nil
				(pending-subgoals newps) nil
				(done-subgoals newps) nil
				(current-subgoal newps) nil
				(dependent-decls newps) nil;;NSH(12.14.94)
				;;d-d was commented, now uncommented(4.9.99)
				(current-rule newps) nil
				(current-xrule newps) nil
				(printout newps) nil
				(strategy newps) (query*-step))
			  newps)
			 (t (setf (strategy ps) (query*-step))
			    ps))))))))

(defun findps (info ps)
  (cond ((numberp info)
	 (if (typep ps 'top-proofstate)
	     ps
	     (if (strat-proofstate? ps)  ;;NSH(7.12.94) ignores
		 ;;strat-proofstates in the
		 ;;counting. 
		 (findps info (parent-proofstate ps))
		 (if (<= info 0)
		     ps
		     (findps (1- info)(parent-proofstate ps))))))
	((or (equal info (label ps))
	     (equal info (current-input ps))
	     (and (consp (current-input ps))
		  (equal info (car (current-input ps))))
	     (equal info (current-rule ps))
	     (and (consp (current-rule ps))
		  (equal info (car (current-rule ps)))))
	 (nonstrat-parent-proofstate ps))
	((typep ps 'top-proofstate)
	 nil)
	(t (and (parent-proofstate ps)
		(findps info (parent-proofstate ps))))))


;;find-ps-info returns the undo distance between target and
;;current proofstates.  Returns nil, if target isn't found.
(defun find-ps-info (target-ps current-ps &optional (info 0))
  (cond ((eq target-ps current-ps)
	 info)
	((or (top-proofstate? current-ps)
	     (null (parent-proofstate current-ps)))
	 nil)
	((strat-proofstate? current-ps)
	 (find-ps-info target-ps (parent-proofstate current-ps) info))
	(t (find-ps-info target-ps
			 (parent-proofstate current-ps)
			 (1+ info)))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod remove-dble-negation ((ex negation))
  (or (remove-dble-negation* (argument ex))
      ex))

(defmethod remove-dble-negation (ex)
  ex)

(defmethod remove-dble-negation* ((ex negation))
  (remove-dble-negation (argument ex)))

(defmethod remove-dble-negation* ((ex expr))
  nil)
  
(defun clean-goal (goal)
  (let ((new-s-forms (loop for sf in (s-forms goal)
			   when (not (or (and (negation? (formula sf))
					      (tc-eq (args1 (formula sf))
						     *true*))
					 (tc-eq (formula sf) *false*)))
			   collect
			   (lcopy sf 'formula
				  (remove-dble-negation (formula sf))))))
    (if (equal (s-forms goal) new-s-forms)
	goal
	(lcopy goal 's-forms new-s-forms))))

;;generates the sequence of subgoal proofstates to be placed in
;;remaining subgoals.
;;2/5/92: each subgoal can either be a goal or a
;;list consisting of a goal followed by updates.
;;1.31.94: special treatment for tcc-proofstates.
(defun make-subgoal-proofstates (proofstate strategy subgoals
					    &optional tcc-subgoals)
;  (when (and (tcc-proofstate? proofstate)
;	     (not (tcc-sequent? (current-goal proofstate))))
;    (break "bad ps"))
  (let ((allsubgoals (append subgoals tcc-subgoals))
	(proof-dependent-decls
	 (union (dependent-decls proofstate)
		(proof-dependent-decls proofstate))))
;;    (cond ((consp allsubgoals)))
    (loop for goal in allsubgoals
	  as goalnum from 1
	  collect
	  (let* ((sequent
		  (if (consp goal)(car goal) goal))
		 (tcc-to-sequent? (and (memq goal subgoals)
				       (tcc-sequent? sequent)
				       (tcc-sequent?;;NSH(11.3.94)
					(current-goal proofstate))
				       (eq (tcc (current-goal proofstate))
					   (tcc sequent))))
		 (goalstate
		  (make-instance
		      (if (typep strategy 'strategy)
			  'strat-proofstate
			  (if (and (tcc-sequent? sequent)
				   (not tcc-to-sequent?))
			      'tcc-proofstate
			      'proofstate))
		    :current-goal
		    (let* ((sequent
			    (if tcc-to-sequent? ;;NSH(10.3.95)
				;;added copy since sequents are shared
				;;so destructive change-class is bad.
				(change-class (copy sequent) 'sequent)
				sequent)))
		      (clean-goal sequent))
		    :context (copy-prover-context (context proofstate))
		    :strategy  strategy
		    :label
		    (if (= (length allsubgoals) 1)
			(label proofstate)
			(format nil "~a.~a~@[T~]" (label proofstate)
				goalnum (memq goal tcc-subgoals)))
		    :subgoalnum (1- goalnum)
		    :proof-dependent-decls proof-dependent-decls
		    :dependent-decls (dependent-decls proofstate)
		    ;;d-d can be nil but inheriting from parent is okay.
		    :dp-state (dpi-copy-state (dp-state proofstate))
		    :current-auto-rewrites
		    (current-auto-rewrites proofstate)
		    :rewrite-hash (rewrite-hash proofstate)
		    :subtype-hash (subtype-hash proofstate)
		    :tcc-hash (tcc-hash proofstate) ;;NSH(10.30.01)
		    :parent-proofstate proofstate
		    :comment (comment proofstate))))
	    (if (consp goal)
		(make-updates (cdr goal)
			      goalstate)
		goalstate)))))

(defmethod top-rule-function ((ps proofstate))
  (if (null (strategy ps)) nil
      (top-rule-function (strategy ps))))

(defmethod subgoal-strategy-function ((ps proofstate))
  (if (null (strategy ps)) nil
      (subgoal-strategy-function (strategy ps))))

(defmethod strat-if-a-subgoal-fails ((ps proofstate))
  (if (null (strategy ps)) nil
      (strat-if-a-subgoal-fails (strategy ps))))

(defmethod post-subgoal-strat ((ps proofstate))
  (if (null (strategy ps)) nil
      (post-subgoal-strat (strategy ps))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-to-number (string &optional (radix 10) (pos 0)(accum 0))
  (cond ((>= pos (length string)) accum)
	((not (digit-char-p (char string pos) radix))
	 0)
	(t (string-to-number string radix (1+ pos)
			     (+ (* radix accum)
				(digit-char-p (char string pos) radix))))))
				

(defun get-goalnum (ps)
  (let* ((label (string (label ps)))
	 (pos (position  #\.  label :from-end t))
	 (par-ps (parent-proofstate ps)))
    (cond ((or (null par-ps) (null pos)
	       (equal (label par-ps) (label ps)))
	   1)
	  (t (string-to-number (label-suffix label))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(addrule 'add-decl (string) () (add-declaration string)
;	 "(add-decl string):  Adds a declaration to the end of the current module")

;;NSH(10.20.94) : not used anywhere.
;(defun add-declaration (decl)
;  #'(lambda (ps)
;      (declare (ignore ps))
;  (let* (
;	 (parsed-decl
;	  (pc-parse decl 'theory))
;	 (tc-decl (typecheck parsed-decl
;			       :context (current-context))))
;    (format-if "~%Adding declaration to current module.
;Other than that,")
;    (when (consp tc-decl)
;      (loop for decl in tc-decl
;	    do (add-decl decl (current-context))))
;  (values 'X nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is the proofchecker's interface to the parser;
;;it checks whether the input needs to be parsed.

(defmethod pc-parse ((input string) nt)
  (let ((x (parse :string input :nt nt))
	(rule-arg (assq input *rule-args-alist*)))
    (when rule-arg
      (setf (cdr rule-arg) x))
    x))

(defmethod pc-parse (input nt)
  (parse :string (format nil "~a" input) :nt nt))

(defmethod pc-parse ((input integer) nt)
  (parse :string (format nil "~a" input) :nt nt))

(defmethod pc-parse ((input syntax) nt)
  (declare (ignore nt))
  input)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retypecheck-sexp (sexp)
  (if (consp sexp)
      (if (eq (car sexp) 'typechecked)
	  (typecheck (parse :string (cadr sexp) :nt 'expr)
	    :expected
	    (typecheck (parse :string (caddr sexp)
			      :nt 'type-expr))
	    :tccs 'none ;;NSH(10.20.94) tccs generated
	    ;;at rule-application
	    )
	  (cons (retypecheck-sexp (car sexp))
		(retypecheck-sexp (cdr sexp))))
      sexp))
					       
(defun mystring<= (st1 st2)
  (or (< (length st1)(length st2))
      (and (eql (length st1)(length st2))
	   (string<= st1 st2))))
      
;; Modified by MM to inclue auto-fix [February 19, 2020]
(defun rerun-step (justif  recheck? &optional break? default auto-fix?)
  (let ((auto-fix? (if auto-fix?
		       (if (and (integerp auto-fix?) (> auto-fix? 0)) auto-fix? 1)
		     nil)))
    (cond ((or (typep justif 'justification)
	       (consp justif))
	   (let* ((top-rule-in (if (and recheck?
					(typep justif 'justification)
					(xrule justif))
				   (xrule justif)
				 (rule justif))))
	     (format-if "~%Rerunning step: ~s" (format-rule top-rule-in t))
	     (if nil ;;(and (consp top-rule-in)
		 ;;     (not (step-or-rule-defn (car top-rule-in))))
	       (rerun-step (when (subgoals justif)
			     (car (subgoals justif)))
			   recheck? break?)
	       (let ((top-rule `(let ((x (retypecheck-sexp (quote ,top-rule-in))))
				  x))
		     (subgoal-strategy
		      (loop for subjustif in
			    (sort (subgoals justif) #'mystring<=
				  :key #'label)
			    collect (let((subgoal-step
					  (let ((original-step
						 `(let ((strat
							 (rerun-step
							  (quote ,subjustif)
							  ,recheck?
							  ,break?
							  (quote ,default)
							  ,auto-fix?)))
						    strat)))
					    (if default
						`(then ,original-step ,default)
					      original-step))))
				      (if auto-fix?
					  `(then (finalize ,subgoal-step) (try-siblings-proofs ,auto-fix?))
					subgoal-step)))))
		 (if break?
		     `(spread! ,top-rule ,subgoal-strategy)
		   `(spread@ ,top-rule ,subgoal-strategy))))))
	  (t (query*-step)))))

;; (defun rerun-step (justif  recheck? &optional break? default)
;;   (cond ((or (typep justif 'justification)
;; 	     (consp justif))
;; 	 (let* ((top-rule-in (if (and recheck?
;; 				      (typep justif 'justification)
;; 				      (xrule justif))
;; 				 (xrule justif)
;; 				 (rule justif))))
;; 	   (format-if "~%Rerunning step: ~s" (format-rule top-rule-in t))
;; 	   (if nil ;;(and (consp top-rule-in)
;; 	           ;;     (not (step-or-rule-defn (car top-rule-in))))
;; 	       (rerun-step (when (subgoals justif)
;; 			     (car (subgoals justif)))
;; 			   recheck? break?)
;; 	       (let ((top-rule `(let ((x (retypecheck-sexp (quote ,top-rule-in))))
;; 				  x))
;; 		     (subgoal-strategy
;; 		      (loop for subjustif in
;; 			    (sort (subgoals justif) #'mystring<=
;; 				  :key #'label)
;; 			    collect (if default
;; 					`(then
;; 					  ;; (finalize
;; 					  (try
;; 					   (try 
;; 					    (then (let ((strat (rerun-step (quote
;; 									    ,subjustif) ,recheck? ,break? (quote ,default))))
;; 						    strat)
;; 						  ,default)
;; 					    (fail)
;; 					    (skip))
;; 					   (skip) 
;; 					   (try-siblings-proofs)))
;; 					`(let ((strat (rerun-step (quote
;; 								   ,subjustif) ,recheck? ,break?)))
;; 					   strat))))
;; 		     )
;; 		 (if break?
;; 		     `(spread! ,top-rule ,subgoal-strategy)
;; 		     `(spread@ ,top-rule ,subgoal-strategy))))))
;; 	(t (query*-step))))
	
(defmethod extract-justification-sexp ((justification justification))
  (list (label justification)
	(sexp-unparse (rule justification))
	(extract-justification-sexp (subgoals justification))
	(comment justification)))

(defun sexp-unparse (form)
  (cond ((consp form)(cons (sexp-unparse (car form))
			   (sexp-unparse (cdr form))))
	((or (null form)(symbolp form)(numberp form) (stringp form))
	 form)
	((typep form 'justification)
	 (copy form 'label (sexp-unparse (label form))
	       'rule (sexp-unparse (rule form))
	       'subgoals (sexp-unparse (subgoals form))))
	((or (and (slot-exists-p form 'declared-type)(declared-type form))
	     ;;(when (slot-exists-p form 'type)
	       ;;(type-value (type form)))
	     (when (slot-exists-p form 'type)
	       (type form)))
	 (list 'typechecked (unparse form
				     :string t)
	       (unparse (raise-actuals
			 (or ;;(type-value (type form))
			     (and (slot-exists-p form 'declared-type)
				  (declared-type form))
			     (type form)))
			:string t)))
	(t (unparse form :string t))))

(defmethod extract-justification-sexp ((list list))
  (cond ((null list) nil)
	(t (cons (extract-justification-sexp (car list))
		 (extract-justification-sexp (cdr list))))))

(defmethod extract-justification-sexp (x)
  x)

;;; editable-justification ::=
;;;   '(' [label] [comment]
;;;       ( editable-justification | '(' editable-justification++ ')' ) ')'
;;; ++ here means two or more.

(defun editable-justification (justif &optional
				      label xflag full-label no-escape?)
  ;;NSH(1.3.98) if full-label is given, then the full label is
  ;;printed rather than just the branch numbers.
  (unless (null justif)
    (let ((jlabel (label justif))
	  (rule (rule justif)))
      (if (and (consp rule)
	       (eq (car rule) 'rerun)
	       (equal (label (cadr rule)) jlabel))
	  (cadr rule)
	  (let* ((top-step (if (and xflag (xrule justif))
			       (format-rule (xrule justif) no-escape?)
			       (format-rule rule no-escape?)))
		 (full-label (if (and full-label
				      (not (equal jlabel label))
				      (> (length jlabel) 0))
				     (format nil "~a.~a" full-label jlabel)
				 full-label))
		 (ejustif (cons top-step
				(editable-justification* (subgoals justif)
							 jlabel
							 xflag
							 full-label
							 no-escape?)))
		 (ejustif (if (comment justif)
			      (cons (comment justif) ejustif)
			      ejustif)))
	    (if (equal jlabel label)
		ejustif
		(cons (or full-label jlabel) ejustif)))))))

(defun editable-justification* (justifs &optional
					label xflag full-label no-escape?)
  (unless (null justifs)
    (if (singleton? justifs)
	(editable-justification (car justifs) label xflag full-label
				no-escape?)
	(list (loop for justif in justifs
		    collect (editable-justification
			       justif nil xflag full-label no-escape?))))))

; DAVESC
(defun check-edited-justification (ejustif &optional label)
  (cond ((null ejustif)
	 nil)
	((not (consp ejustif))
	 (values "Proof must be a list" ejustif))
	((null label)
	 (if (and (stringp (car ejustif))
		  (every #'digit-char-p (car ejustif)))
	     (check-edited-justification (cdr ejustif) (car ejustif))
	     (values "Label must be a string of numbers." (car ejustif))))
	(t
	 (if (and (consp (car ejustif)) (consp (caar ejustif)))
	     (some #'check-edited-justification (car ejustif))
	     (if (consp (car ejustif))
		 (check-edited-justification (cdr ejustif) label)
		 (if (stringp (car ejustif))
		     (if (valid-comment-string? (car ejustif))
			 (check-edited-justification (cdr ejustif) label)
			 (values "Invalid comment string" (car ejustif)))
		     (values "A rule application must be a list."
			     (car ejustif))))))))

(defun numberof-steps (ejustif &optional count)
  (cond ((null ejustif)
	 count)
	((null count)
	 (numberof-steps (cdr ejustif) 0))
	(t
	 (if (and (consp (car ejustif)) (consp (caar ejustif)))
	     (reduce #'+ (mapcar #'(lambda (st)
				     (numberof-steps st))
			   (car ejustif))
		     :initial-value count)
	     (if (consp (car ejustif))
		 (numberof-steps (cdr ejustif) (1+ count))
		 (1+ count))))))

(defun valid-comment-string? (string &optional (pos 0) (after-newline? t)
				     (len (length string)))
  (or (>= pos len)
      (case (char string pos)
	(#\newline (valid-comment-string? string (1+ pos) t len))
	((#\space #\tab)
	 (valid-comment-string? string (1+ pos) after-newline? len))
	(#\; (valid-comment-string? string (1+ pos) nil))
	(t (unless after-newline?
	     (valid-comment-string? string (1+ pos) nil len))))))

;;; revert-justification converts an editable-justification to
;;; a justification-sexp.  It adds missing labels, moves comments
;;; to the end, and adds parens around singleton justifications.
;;; label should only be given in the recursive call.
(defun revert-justification (ejustif &optional label)
  (unless (null ejustif)
    (if (null label)
	(if (listp (cadr ejustif))	; Not a comment
	    (list (car ejustif)		; Assumes this is a label
		  (unformat-rule (cadr ejustif))
		  (revert-justification (cddr ejustif) (car ejustif)))
	    (let ((rjust (revert-justification (cddr ejustif) (car ejustif))))
	      (assert (singleton? rjust))
	      ;; Need to put the comment in the right place
	      (append (car rjust) (list (cadr ejustif)))))
	(if (and (consp (car ejustif)) (consp (caar ejustif)))
	    (mapcar #'revert-justification (car ejustif))
	    ;; Singleton editable-justification
	    (if (consp (car ejustif)) ; No label or comment - must be a rule
		(list (list label
			    (unformat-rule (car ejustif))
			    (revert-justification (cdr ejustif) label)))
		(if (every #'digit-char-p (car ejustif))
		    (revert-justification (cdr ejustif) label)
		    (append (revert-justification (cdr ejustif) label)
			    (list (car ejustif)))))))))
  

(defmethod pp-justification ((justification justification))
  (pp-justification* (extract-justification-sexp justification) nil))

(defmethod pp-justification (justification)
  (pp-justification* justification nil))


(defun pp-justification* (justification label)
  (cond ((null justification)
	 nil)
	(t (when (comment justification)
	     (commentary "~%~a" (comment justification)))
	   (cond ((equal (label justification) label)
		  (commentary "~%")
		  (commentary "~V@T" (+ 3 (length (string label)))))
		 (t (commentary "~%~a : " (label justification))))
	   (commentary "~a" (format-rule (rule justification)) :pretty t)
	   (loop for entry in (reverse (subgoals justification))
		 do (pp-justification* entry (car justification))))))

(defun format-rule (sexp &optional no-escape?)
  (cond ((null sexp) nil)
	((and (null no-escape?)
	      (stringp sexp))
	 (protect-string-output sexp))
	((not (consp sexp)) sexp)
	((eq (car sexp) 'typechecked)
	 (list (cadr sexp) '!type (caddr sexp)))
	(t (cons (format-rule (car sexp) no-escape?)
		 (format-rule (cdr sexp) no-escape?)))))

(defun unformat-rule (sexp)
  (when sexp
    (cond ((not (consp sexp)) sexp)
	  ((and (consp (cdr sexp))
		(consp (cddr sexp))
		(stringp (car sexp))
		(memq (cadr sexp) '(:type !type))
		(null (cdddr sexp)))
	   (list 'typechecked (car sexp)(caddr sexp)))
	  (t (cons (unformat-rule (car sexp))
		   (unformat-rule (cdr sexp)))))))
  
(defmethod decision-procedure-used ((list list))
  (or (and (listp (car list))
	   (cdr (assq :decision-procedure-used (car list))))
      *default-decision-procedure*))
(defmethod label ((list list))
  (if (listp (car list)) (cadr list) (car list)))
(defmethod rule ((list list))
  (if (listp (car list)) (caddr list) (cadr list)))
(defmethod subgoals ((list list))
  (if (listp (car list)) (cadddr list) (caddr list)))

;;catchall methods for label, rule, subgoals
(defmethod decision-procedure-used ((x t)) *default-decision-procedure*)
(defmethod label ((x t)) "9999")
(defmethod rule ((x t)) '(skip))
(defmethod subgoals ((x t)) nil)

		 
(defun collect-module-proofs (module)
  (loop for decl in (theory module)
	when (and (typep decl 'formula-decl)
		  (justification decl))
	 collect
	 (cons (id decl) (extract-justification-sexp
			  (justification decl)))))

(defun collect-proofs (modnames)
  (let ((proofs nil))
    (maphash #'(lambda (key mod)
		 (when (or (null modnames)
			   (memq key modnames))
		   (push (cons key (collect-module-proofs mod))
			 proofs)))
	     (current-pvs-theories))
    (nreverse proofs)))

(defun write-proofs (filestring &optional modnames)
  (let ((proofs (collect-proofs modnames)))
    (with-open-file (output filestring :direction :output
			    :if-exists :supersede)
      (loop for x in proofs do
	    (write x  :length nil :level nil :escape t :stream output)))
    ;(format t "~%Saved proofs in ~a" filestring)
    ))

(defun load-proofs (filestring)
  (with-open-file (input filestring :direction :input)
    (load-module-proofs input filestring)))

(defun load-module-proofs (input filestring)
  (let ((modproofs (read input nil nil)))
    (cond ((null modproofs) ;(format t "~%Read proofs from ~a"
				;	    filestring)
	   nil)
	  (t (let* (
		   (mod (car modproofs))
		   (proofs (cdr modproofs))
		   (module (gethash mod (current-pvs-theories))))
	       (unless (null module)
		 (mapc #'(lambda (decl)
			   (when (typep decl 'formula-decl)
			     (let ((prf-entry (assoc (id decl) proofs
						     :test #'eql)))
			       (when (not (null prf-entry))
				 (setf (justification decl)
				       (cdr prf-entry))))))
		       (theory module)))
	       (load-module-proofs input filestring))))))
			 

(defun keyword? (key optionals)
  (and (keywordp key)
       (find key optionals
	     :test #'(lambda (x y)
		       (if (equal (string x)(string (prover-keyarg y)))
			   (prover-keyarg y)
			   nil)))))

(defun match-formals-with-actuals (required optional actuals strat)
  (let* ((formals (append required
			  (if (and optional
				   (not (eq (car optional) '&rest)))
			      (cons '&optional optional)
			      optional)))
	 (alist (pair-formals-args formals actuals strat)))
    ;; (unless (set-equal alist (pair-formals-args-old formals actuals)
    ;; 		       :test #'equal)
    ;;   (break "match-formals-with-actuals: mismatch"))
    alist))
;  (cond ((consp required) ;;assuming enough actuals for required.
;	 (cons (cons (car required)(car actuals))
;	       (match-formals-with-actuals (cdr required) optional
;					   (cdr actuals))))
;	((and (consp optional)
;	      (eq (car optional) '&rest))
;	 (when (cdr optional)
;	   (list (cons (cadr optional) actuals))))
;	((consp actuals)
;	 (let ((entry (keyword? (car actuals) optional)))
;	   (if entry
;	       (let ((result (match-formals-with-actuals
;			    required
;			    (remove entry optional) 
;			    (cddr actuals))))
;		 (if (consp (cdr actuals))
;		     (cons (cons (prover-keyarg entry) (cadr actuals))
;			   result)
;		     (cons (cons (prover-keyarg entry) (default entry))
;			   result)))
;	     (if (consp optional)
;		 (cons (cons (prover-keyarg (car optional))(car actuals))
;		       (match-formals-with-actuals required (cdr optional)
;						   (cdr actuals)))
;		 nil))))
;	(t (loop for x in optional when (not (eq x '&rest))
;		 collect (cons (prover-keyarg x) (default x)))))
				 
	
;(defun check-rule (input-rule ps) ;;returns two values: final rule and
;				  ;;input step.  
;  (let* ((rule-name (if (consp input-rule)(car input-rule) input-rule))
;	 (rule-args (if (consp input-rule)(cdr input-rule) nil))
;	 (rule-entry (gethash rule-name *rulebase*))
;	 (rulefun-entry (gethash rule-name *rulefunbase*))
;	 (strategy-entry (gethash rule-name *strategies*))
;	 (entry (cond (rule-entry)(rulefun-entry)(strategy-entry)
;		      (t nil))))
;    (cond ((null entry)
;	   (format t "~%No rule, rule-function, or strategy: ~a.~%"
;		   rule-name)
;	   nil)
;	  ((< (length rule-args) (length (required-args entry)))
;	   (cond
;	     (*noninteractivemode* '(skip))
;	     (t
;	      (let ((formals (if (optional-args entry)
;					(append (required-args entry)
;						(cons '&optional
;						      (optional-args entry)))
;					(required-args entry))))
;	      (format-if "~%Rule ~a has argument list:~%~a"
;			 rule-name  formals)
;	      (format-if "~%Please supply a list corresponding to:~%~a"
;			 (nthcdr (length rule-args) formals))
;	      (let ((more-args (qread "~%List of arguments? ")))
;		(check-rule (cons rule-name (nconc rule-args more-args))))))))
;	  ((> (length rule-args)(+ (length (required-args entry))
;				   (length (optional-args entry))))
;	   (format-if "~%Too many arguments. ")
;	   (values nil nil))
;	  ;;	  ((typep rule-body 'rule) rule-body)
;	  (rulefun-entry
;	   (values (check-rule (funcall (apply (rulefun entry)
;				     rule-args)
;			      ps)
;		       ps)
;		   (cons (name entry) rule-args)))
;	  (t (values (cons (name entry) rule-args)(cons (name entry) rule-args))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

					;(defun simp-rule () (make-instance 'rule
					;		      :rule-part #'simp
					;		      :rule-input '(simp)))


;(addrule 'simp nil nil (simp-rule)
;	 "(simp), simp: Flattens out a disjunction (+ve: OR, IMPLIES,
;NOT; -ve: AND, IFF, NOT)")
;
;(defun simp-rule () #'simp)
;
;(defun simp (ps)
;  (if (loop for s-form in (s-forms (current-goal ps))
;	    thereis (or+form? (formula s-form)))
;      (values '?
;	      (disj-simplifier (current-goal ps)))
;      ;;(substitution ps)
;      (values 'X nil nil)))

(defun disj-simplifier (goalsequent)
  (list (copy goalsequent
	      's-forms
	      (loop for s-form in (s-forms goalsequent) 
		    append
		    (mapcar #'(lambda (x) (copy s-form 'formula x))
			    (or+ (list (formula s-form))))))))


(defun or+form? (exp)
  (and (typep exp 'application)
       (or (disjunction? exp)(implication? exp)
	   (and (negation? exp)
		(let ((arg (car (arguments exp))))
		  (and (typep arg  'application)
		       (or (negation? arg)(conjunction? arg))))))))

(defun or+ (forms)
					;(break "in or+")
  (loop for disjunct in forms
	append
	(if (and (typep disjunct 'application)
		 (typep (operator disjunct) 'name-expr))
	    (cond  
	      ((disjunction? disjunct)
	       (or+ (arguments disjunct)))
	      ((implication? disjunct)
	       (or+ (list (negate
			   (car (arguments disjunct)))
			  (cadr (arguments disjunct)))))
	      ((negation? disjunct)
	       (if (and (typep (car (arguments disjunct))
			       'application) 
			(typep (operator (car (arguments disjunct)))
			       'name-expr))
		   (let ((args2 (arguments (car (arguments disjunct)))))
		     (cond 
		       ((conjunction? (args1 disjunct))
			(or+ (mapcar #'(lambda (x)(negate x))
				     args2)))
		       ((negation? (args1 disjunct))
			(or+ (car args2)))
		       ((iff? (args1 disjunct))
			(or+ (list (negate (make-implication (car args2)
							     (cadr args2)))
				   (negate (make-implication
					    (cadr args2)
					    (car args2))))))
		       (t (list disjunct))))
		   (if (and (typep (car (arguments disjunct)) 'name-expr)
			    (equal (args1 disjunct) *true*))
		       nil
		       (list disjunct))))
	      (t (list disjunct)))
	    (if (and (typep disjunct 'name-expr)
		     (equal  disjunct *false*))
		nil
		(list disjunct)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cases? (expr) (typep expr 'cases-expr))

;;;and+: takes a single formula and splits it into a list of conjuncts.
;;;EG: (and+ "NOT (A AND B IMPLIES C) AND NOT (E OR F)") is
;;;(NOT C #<Name-Expr A> #<Name-Expr B> NOT E NOT F).
(defun and+ (form &optional depth)
  (and++ form depth))

(defmethod and++ :around (form (depth (eql 0)))
  (list form))

(defmethod and++ (form depth)
  (declare (ignore depth))
  (list form))

(defmethod and++ ((form conjunction) depth)
  (nconc (and++ (args1 form) (when depth (1- depth)))
	 (and++ (args2 form) (when depth (1- depth)))))

(defmethod and++ ((form iff) depth)
  (declare (ignore depth))
  (list (make!-implication (args1 form) (args2 form))
	(make!-implication (args2 form) (args1 form))))

(defmethod and++ ((form cases-expr) depth)
  (and++ (translate-cases-to-if form) (when depth (1- depth))))

(defmethod and++ ((form branch) depth)
  (declare (ignore depth))
  (list (make!-implication (condition form) (then-part form))
	(make!-implication (negate! (condition form)) (else-part form))))

(defmethod and++ ((form negation) depth)
  (or (and+-negation (argument form) depth)
      (list form)))

(defmethod and+-negation ((form implication) depth)
  (nconc (and++ (negate! (args2 form)) (when depth (1- depth)))
	 (and++ (args1 form) (when depth (1- depth)))))

(defmethod and+-negation ((form disjunction) depth)
  (nconc (and++ (negate! (args1 form)) (when depth (1- depth)))
	 (and++ (negate! (args2 form)) (when depth (1- depth)))))

(defmethod and+-negation ((form negation) depth)
  (and++ (args1 form) (when depth (1- depth))))

(defmethod and+-negation ((form cases-expr) depth)
  (and++ (negate! (translate-cases-to-if form)) (when depth (1- depth))))

(defmethod and+-negation ((form branch) depth)
  (declare (ignore depth))
  (list (make!-negation (make!-conjunction (condition form) (then-part form)))
	(make!-negation (make!-conjunction (make!-negation (condition form))
					   (else-part form)))))

(defmethod and+-negation (form depth)
  (declare (ignore form depth))
  nil)

(defun and+form? (exp)
  (or (branch?  exp)
      (and (typep exp 'application)
	   (typep (operator exp) 'name-expr)
	   (or (conjunction? exp)(iff? exp)
	       (and (negation? exp)
		    (let ((arg (car (arguments exp))))
		      (or (branch? arg)
			  (and (typep arg  'application)
			       (typep (operator arg) 'name-expr)
			       (or (negation? arg)
				   (disjunction? arg)
				   (implication? arg))))))))))

(defun split-rule-fun (sformnum &optional depth labels)
  #'(lambda (ps) (split-step sformnum ps depth labels)))

(defun split-step (sformnum ps depth labels)
  (let* ((goalsequent (current-goal ps))
	 (sformnum (find-sform (s-forms goalsequent) sformnum
			       #'(lambda (sf)(and+form? (formula sf)))))
	 (selected-sform (select-seq (s-forms goalsequent) (list sformnum))))
    (cond ((not (or (null depth) (integerp depth)))
	   (error-format-if "~%depth should be an integer, not ~a:~%  ~a~%"
			    (type-of depth) depth)
	   (values 'X nil nil))
	  ((or (null selected-sform)
	       (not (and+form? (formula (car selected-sform))))
	       (eql depth 0))
	   (values 'X nil nil))
	  (t (let* ((sel-sform (car selected-sform))
		    (new-sforms (delete-seq (s-forms goalsequent) (list sformnum)))
		    (conjuncts (and+ (formula sel-sform) depth))
		    (conjunct-sforms
		     (mapcar #'(lambda (x)
				 (copy sel-sform
				   'formula x))
		       conjuncts))
		    (labelled-conjunct-sforms
		     (label-sforms conjunct-sforms labels))
		    (new-sequents
		     (mapcar #'(lambda (x)
				 (copy goalsequent
				   's-forms
				   (cons x new-sforms)))
		       labelled-conjunct-sforms)))
	       (values '? new-sequents ;;(substitution ps)
		       ))))))

(defun label-sforms (sforms labels)
  (cond ((null labels) sforms)
	((null (cdr labels))
	 (mapcar #'(lambda (x) (setf (label x) (car labels)))
		 sforms))
	(t (setf (label (car sforms)) (car labels))
	   (cons (car sforms)
		 (label-sforms (cdr sforms)(cdr labels))))))
				      

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    


	


(defun ask-rule ()
       (format-if "~%~%Rule?  ")
       (read))

     ;;(defun apply-rule (rule proofstate)
     ;; (check-rule-and-apply rule proofstate))

;;;One design philosophy is to do things simply and straightforwardly
;;;first, and then bring in sophistication through some actual
;;;experience.  

;;; There'll be an alist of rule-names and functions which when
;;;applied to the proof state yield a new proofstate.
;;;One design criterion would be to make it so that the 
;;;various combinations of rules actually yielded a single
;;;rule, i.e., they did not create all the intermediate proof
;;;states.  To achieve this, each rule is defined to take
;;;certain components of the proofstate and return certain
;;;updates to the proof state.   These updates could be of
;;;the form: delete, add, split, etc.  Each rule application returns a
;;;new proof state and a signal indicating success or failure.  The
;;;signal is used to control the proof so that we could have tacticals
;;;like s1 ELSE s2, REPEAT s1, s1 THEN s2, MAP s1.  Each
;;;rule itself takes a sequent and returns a list of sequents, with
;;;certain other updates.  My current thinking that matters are
;;;considerably simplified by considering a sequent as a disjunction
;;;rather 
;;;than as consisting of right and left parts.  That separation can be a
;;;matter for the pretty printer.  

;;;Given a list of sforms, seq, and fnums, nums, select-seq returns all
;;;sforms in seq corresponding to nums.
;;;delete-seq will delete all sforms in seq corresponding to nums.
;;;gather-seq returns all sforms in yesnums but not in nonums satisfying pred.
;;;gather-fnums is like gather-seq, but returns fnums

(defun cleanup-fnums (fnums)
  (let ((cfnums (cleanup-fnums* fnums)))
    ;;(extract-fnums-arg cfnums)
    cfnums
    ))

(defun cleanup-fnums* (fnums)
  (cond ((consp fnums)
	 (if (eq (car fnums) 'quote)
	     (cleanup-fnums* (cadr fnums))
	     (append (cleanup-fnums* (car fnums)) (cleanup-fnums* (cdr fnums)))))
	((null fnums) nil)
	((stringp fnums) (list (intern fnums :pvs)))
	(t (list fnums))))

(defun select-seq (seq nums)
  (let ((nums (cleanup-fnums nums)))
    ;; cleanup-fnums always returns a flattened list
    (if (memq '* nums)
	seq
	(select-seq1 seq nums +1 -1 nil))))

(defun select-seq1 (seq nums pos neg selected)
  (if (null seq)
      (nreverse selected)
      (if (negation? (formula (car seq)))
	  (select-seq1 (cdr seq) nums pos (1- neg)
		       (if (or (memq '- nums)
			       (memq neg nums)
			       (and (label (car seq))
				    (some #'(lambda (lbl) (memq lbl nums))
					  (label (car seq)))))
			   (cons (car seq) selected)
			   selected))
	  (select-seq1 (cdr seq) nums (1+ pos) neg
		       (if (or (memq '+ nums)
			       (memq pos nums)
			       (and (label (car seq))
				    (some #'(lambda (lbl) (memq lbl nums))
					  (label (car seq)))))
			   (cons (car seq) selected)
			   selected)))))

(defun delete-seq (seq nums)
  (let ((nums (cleanup-fnums nums)))
    (if (memq '* nums)
	nil
	(delete-seq1 seq nums +1 -1 nil))))

(defun delete-seq1 (seq nums pos neg selected)
  (if (null seq)
      (nreverse selected)
      (if (negation? (formula (car seq)))
	  (delete-seq1 (cdr seq) nums pos (1- neg)
		       (if (or (memq '- nums)
			       (memq neg nums)
			       (and (label (car seq))
				    (some #'(lambda (lbl) (memq lbl nums))
					  (label (car seq)))))
			   selected
			   (cons (car seq) selected)))
	  (delete-seq1 (cdr seq) nums (1+ pos) neg
		       (if (or (memq '+ nums)
			       (memq pos nums)
			       (and (label (car seq))
				    (some #'(lambda (lbl) (memq lbl nums))
					  (label (car seq)))))
			   selected
			   (cons (car seq) selected))))))

;;; gather-seq takes a list of s-formulas, two lists of numbers, and a
;;; predicate on s-formulas, and returns the list of s-formulas that
;;; satisfy the predicate.

(defun gather-seq (seq yesnums nonums &optional (pred #'always-true))
  (let ((yesnums (cleanup-fnums yesnums))
	(nonums (cleanup-fnums nonums)))
    (gather-seq* seq yesnums nonums pred 1 -1 nil)))

(defun gather-seq* (seq yesnums nonums pred pos neg selected)
  (if (null seq)
      (nreverse selected)
      (if (negation? (formula (car seq)))
	  (gather-seq* (cdr seq) yesnums nonums pred pos (1- neg)
		       (if (and (in-sformnums? (car seq) pos neg yesnums)
				(not (in-sformnums? (car seq) pos neg nonums))
				(funcall pred (car seq)))
			   (cons (car seq) selected)
			   selected))
	  (gather-seq* (cdr seq) yesnums nonums pred (1+ pos) neg
		       (if (and (in-sformnums? (car seq) pos neg yesnums)
				(not (in-sformnums? (car seq) pos neg nonums))
				(funcall pred (car seq)))
			   (cons (car seq) selected)
			   selected)))))

;;; Similar to gather-seq, but returns fnums rather than s-formulas

(defun gather-fnums (sforms yesnums nonums &optional (pred #'always-true))
  (let ((yesnums (cleanup-fnums yesnums))
	(nonums (cleanup-fnums nonums)))
    (gather-fnums* sforms yesnums nonums pred 1 -1 nil)))

(defun gather-fnums* (seq yesnums nonums pred pos neg selected)
  (if (null seq)
      (nreverse selected)
      (if (negation? (formula (car seq)))
	  (gather-fnums* (cdr seq) yesnums nonums pred pos (1- neg)
			 (if (and (in-sformnums? (car seq) pos neg yesnums)
				  (not (in-sformnums? (car seq) pos neg nonums))
				  (funcall pred (car seq)))
			     (cons neg selected)
			     selected))
	  (gather-fnums* (cdr seq) yesnums nonums pred (1+ pos) neg
			 (if (and (in-sformnums? (car seq) pos neg yesnums)
				  (not (in-sformnums? (car seq) pos neg nonums))
				  (funcall pred (car seq)))
			     (cons pos selected)
			     selected)))))

;;; Similar to gather-fnums, but the predicate here is on exprs rather
;;; than s-formulas.
(defun find-all-sformnums (sforms sformnums pred)
  (find-all-sformnums* sforms (cleanup-fnums sformnums) pred 1 -1 nil))

(defun find-all-sformnums* (sforms sformnums pred pos neg acc)
  (if (null sforms)
      (nreverse acc)
      (let* ((sign (not (negation? (formula (car sforms)))))
	     (newpos (if sign (1+ pos) pos))
	     (newneg (if sign neg (1- neg)))
	     (newacc (if (and (in-sformnums? (car sforms) pos neg sformnums)
			      (funcall pred (formula (car sforms))))
			 (cons (if sign pos neg) acc)
			 acc)))	;;(break "find-all")
	(find-all-sformnums* (cdr sforms) sformnums pred
			     newpos newneg newacc))))

(defun find-remaining-sformnums (sforms sformnums sub-sformnums)
  (gather-fnums sforms sformnums sub-sformnums))

(defun find-sform (sforms sformnum &optional (pred #'always-true))
  (find-sform* sforms sformnum pred 1 -1))

(defun find-sform* (sforms sformnum pred pos neg)
  (when sforms
    (if (negation? (formula (car sforms)))
	(if (and (or (memq sformnum '(* -))
		     (equal sformnum neg)
		     (and (label (car sforms))
			  (or (symbolp sformnum)
			      (stringp sformnum))
			  (memq (if (symbolp sformnum)
				    sformnum
				    (intern sformnum :pvs))
				(label (car sforms)))))
		 (funcall pred (car sforms)))
	    neg
	    (find-sform* (cdr sforms) sformnum pred pos (1- neg)))
	(if (and (or (memq sformnum '(* +))
		     (equal sformnum pos)
		     (and (label (car sforms))
			  (or (symbolp sformnum)
			      (stringp sformnum))
			  (memq (if (symbolp sformnum)
				    sformnum
				    (intern sformnum :pvs))
				(label (car sforms)))))
		 (funcall pred (car sforms)))
	    pos
	    (find-sform* (cdr sforms) sformnum pred (1+ pos) neg)))))

(defun always-t (x)
  (declare (ignore x))
  t)

(defmethod nth-arg ((expr application) num)
       (nth num (arguments expr)))

(defmethod print-object ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*)))
	 (*pp-print-parens* *show-parens-in-proof*))
    (if *debugging-print-object*
	(call-next-method)
	(if (comment ps)
	    (format stream "~%~a : ~%~a~%~a"
	      (label ps)
	      (comment ps)
	      (current-goal ps))
	    (format stream "~%~a :  ~%~a"  (label ps) 
		    (current-goal ps))))))

(defmethod print-object ((ps tcc-proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*)))
	 (*pp-print-parens* *show-parens-in-proof*))
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~%~a (TCC):   ~%~a"  (label ps)
	      (current-goal ps)))))

(defun pos-s-forms (s-forms)
  (pos-s-forms* s-forms))

(defun neg-s-forms (s-forms)
  (neg-s-forms* s-forms))

(defmethod pos-s-forms* ((seq sequent))
  (with-slots (p-sforms) seq
    (if (eq p-sforms 'unbound)
	(setf p-sforms (pos-s-forms* (s-forms seq)))
	p-sforms)))

(defmethod pos-s-forms* (s-forms)
  (loop for sf in s-forms
	when (not (negation? (formula sf)))
	collect sf))

(defmethod neg-s-forms* ((seq sequent))
  (with-slots (n-sforms) seq
    (if (eq n-sforms 'unbound)
	(setf n-sforms (neg-s-forms* (s-forms seq)))
	n-sforms)))

(defmethod neg-s-forms* (s-forms)
  (loop for sf in s-forms
	when (negation? (formula sf))
	collect sf))



(defmethod print-object ((sform s-formula) stream)
  (if *debugging-print-object*
      (call-next-method)
      (let ((*pp-print-parens* *show-parens-in-proof*))
	(format stream "~a" (formula sform)))))


(defun seq-formula (sform)
  (let ((fmla (formula sform)))
    (if (negation? fmla)(args1 fmla)
	fmla)))

(defun unparse-sform (sform)
  (unpindent (seq-formula sform)
	     (+ 6 *prover-indent*)
	     :string t
	     :length *prover-print-length*
	     :level *prover-print-depth*
	     :lines *prover-print-lines*
	     :width (or *prover-print-right-margin*
			*default-char-width*)))

(defun in-every-print-descendant? (sform)
  (every #'(lambda (ps)
	     (memq sform
		   (s-forms (current-goal ps))))
	 *print-descendants*))

(defun display-sform (sform sfnum stream)
  (let ((par-sforms (when *print-ancestor*
		      (s-forms (current-goal *print-ancestor*)))))
    (cond (*report-mode*
	   (unless (and (memq sform par-sforms)
			(every #'(lambda (ps)
				   (memq sform
					 (s-forms (current-goal ps))))
			       *print-descendants*))
	     (format stream "~%~V@T" *prover-indent*)
	     (format stream "{~a}   ~a" sfnum
		     (unparse-sform sform))))
	  (t
	   (format stream
	       "~%~V@T~6@<~:[{~a~@[, ~{~a~^, ~}~]}~;[~a~@[, ~{~a~^, ~}~]]~]~>~:[~;~%~6<~>~]"
	     *prover-indent* (memq sform par-sforms) sfnum
	     (label sform) (label sform))
	   (let* ((sform-str (unparse-sform sform))
		  ;; Start to adding tooltip info for sequents.
		  ;; Doesn't work if print-depth or print-length is set
		  ;; (*valid-id-check* nil)
		  ;; (oparse (parse :string sform-str :nt 'expr))
		  ;; (sparse (if (and (listp oparse) (null (cdr oparse)))
		  ;; 	      (car oparse)
		  ;; 	      oparse))
		  ;; (cc (copy-lex sform sparse nil sform-str))
		  ;; (info (collect-visible-decl-info cc))
		  )
	     (write sform-str :stream stream))))))

(defmethod print-object ((sequent sequent) stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*))))
	(*pp-print-parens* *show-parens-in-proof*))
    (cond (*debugging-print-object*
	   (call-next-method))
	  (t (let ((neg-s-forms (neg-s-forms sequent))
		   (pos-s-forms (pos-s-forms sequent)))
	       (loop for sf in neg-s-forms as
		     sfnum downfrom -1  
		     do
		     (display-sform sf sfnum stream))
	       (when (and *report-mode*
			  (loop for sf in  neg-s-forms
				thereis
				(and (memq sf par-sforms)
				     (in-every-print-descendant? sf))))
		 (format stream "~%~V@T ..." *prover-indent*))
	       (format stream "~%~V@T  |-------" *prover-indent*)
	       (loop for sf in pos-s-forms as sfnum from 1 
		     do
		     (display-sform sf sfnum stream))
	       (when (and *report-mode*
			  (loop for sf in  pos-s-forms
				thereis (and (memq sf par-sforms)
					     (in-every-print-descendant? sf))))
		 (format stream "~%~V@T ..." *prover-indent*))
	       (format stream "~%"))))))

(defun show-proof-skeleton (proofstate)
  (when (current-input proofstate)
    (commentary "~%~a:  ~a" (label proofstate)
	    (current-input proofstate))
    (when (eq (status-flag proofstate) '!)
      (commentary "  Done!") ))
  (cond ((eq (status-flag proofstate) '!)
	 (when (not (current-input proofstate))
	   (commentary "~%~a: Done!" (label proofstate)))
	 (dolist (x (done-subgoals proofstate)) (show-proof-skeleton x)))
	((or (done-subgoals proofstate)
	     (pending-subgoals proofstate)
	     (remaining-subgoals proofstate)
	     (current-subgoal proofstate))
	 (dolist (x (done-subgoals proofstate)) (show-proof-skeleton x))
	 (dolist (x (pending-subgoals proofstate)) (show-proof-skeleton x))
	 (dolist (x (remaining-subgoals proofstate)) (show-proof-skeleton x))
	 (show-proof-skeleton (current-subgoal proofstate)))
	((eq (status-flag proofstate) '*)
	 (commentary "~%~a : *pending*" (label proofstate)))
	((fresh? proofstate)
	 (commentary "~%~a: *remaining*" (label proofstate)))
	(t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Some useful display commands that can be invoked from emacs
;;(see pvs-prover.el).

(defun call-show-proof ()
  (if (and *in-checker* *ps*)
      (let ((*disable-gc-printout* t))
	(pvs-buffer "*Proof*"
	  (with-output-to-string (*standard-output*)
	    (write (editable-justification
		    (collect-justification *top-proofstate*))
		   :stream *standard-output* :pretty t :escape t
		   :level nil :length nil
		   :pprint-dispatch *proof-script-pprint-dispatch*))
	  t t))
      (pvs-message "No proof is currently running")))

(defun call-show-hidden ()
  (if (and *in-checker* *ps*)
      (let ((hforms (hidden-s-forms (current-goal *ps*)))
	    (*disable-gc-printout* t))
	(if (null hforms)
	    (pvs-message "No hidden formulas in current sequent.")
	    (pvs-buffer "Hidden"
	      (with-output-to-string (*standard-output*)
		(commentary "The hidden formulas in the current sequent are:")
		(let ((*print-ancestor* nil))
		  (commentary "~a" (make-instance 'sequent
				   :s-forms hforms))))
	      t t)))
      (pvs-message "No current proof")))
	
  
(defun call-ancestry ()
  (if (and *in-checker* *ps*)
      (let ((*disable-gc-printout* t))
	(pvs-buffer "Ancestry"
	  (with-output-to-string (*standard-output*)
	    (ancestry *ps*)) t t))
      (pvs-message "No current proof")))

(defmethod ancestry ((ps proofstate))
  (print-proofstate ps)
  (ancestry* (parent-proofstate ps)))

(defmethod ancestry* ((ps proofstate))
  (when (current-rule ps)
    (commentary "~%follows by ~s from:" (current-rule ps))
    (print-proofstate ps))
  (ancestry* (parent-proofstate ps)))

(defmethod ancestry* ((ps null))
  nil)

(defun call-siblings ()
  (if (and *in-checker* *ps*)
      (let ((*disable-gc-printout* t)
	    (siblings-str (with-output-to-string (*standard-output*)
			    (siblings *ps*))))
	(if (eq (length siblings-str) 0)
	    (pvs-message "Current goal has no siblings")
	    (pvs-buffer "Siblings" siblings-str t t)))
      (pvs-message "No current proof")))

(defmethod siblings ((ps proofstate))
  (let ((par-ps (parent-proofstate ps)))
    (when par-ps
      (when (remaining-subgoals par-ps)
	(commentary "~%The remaining siblings are: ")
	(loop for subgoal in (remaining-subgoals par-ps)
	      do (print-proofstate subgoal)))
      (when (pending-subgoals par-ps)
	(commentary "~%The postponed siblings are: ")
	(loop for subgoal in (pending-subgoals par-ps)
	      do (print-proofstate subgoal)))
      (when (done-subgoals par-ps)
	(commentary "~%The proved siblings are: ")
	(loop for subgoal in (done-subgoals par-ps)
	      do (print-proofstate subgoal))))))

(defun call-explain-tcc ()
  (if (and *in-checker* *ps*)
      (if  (tcc-proofstate? *ps*)
	   (let ((*disable-gc-printout* t))
	     (pvs-buffer "Explain TCC"
	       (with-output-to-string (*standard-output*)
		 (let* ((goal (current-goal *ps*))
			(expr (expr goal))
			(type (type goal))
			(kind (kind goal))
			(tcc (tcc goal)))
		   (case kind
		     ((subtype)(commentary "Subtype "))
		     ((termination) (commentary "Termination "))
		     ((cases) (commentary "Cases "))
		     ((existence) (commentary "Nonemptiness/Existence "))
		     ((actuals) (commentary "Actuals "))
		     ((assuming) (commentary "Assuming ")))
		   (commentary "TCC ~%~a ~%generated by typechecking~%~a : ~a"
		     tcc expr type)))
	       t t))
	   (pvs-message "Current goal is not a TCC goal"))
      (pvs-message "No current proof")))

(defun quant-to-lambda (form)
  (if (quant-expr? form)
      (make-lambda-expr (bindings form)(expression form))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(7.23.93) using gensubst for terms.

(defmethod termsubst-testfn ((expr expr) testfn)
  (funcall testfn expr))

(defmethod termsubst-testfn ((exprs list) testfn)
  (declare (ignore testfn))
  nil)

(defmethod termsubst-testfn ((expr assignment) testfn)
  (declare (ignore testfn))
  nil)

(defmethod termsubst-testfn ((exprs selection) testfn)
  (declare (ignore testfn))
  nil)

(defmethod termsubst-testfn ((expr t) testfn)
  (declare (ignore testfn))
  t)

(defmethod termsubst-substfn ((expr expr) substfn)
  (funcall substfn expr))

(defmethod termsubst-substfn ((expr t) substfn)
  (declare (ignore substfn))
  expr)

(defun termsubst (expr substfn testfn)
  (gensubst expr
    #'(lambda (ex) (termsubst-substfn ex substfn))
    #'(lambda (ex) (termsubst-testfn ex testfn))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;CRW: needed for flexible CMH prover help; adapted from undo1.
(defun prover-commands ()
  (apply #'append
	 '(try let note if)
	 (mapcar #'(lambda (ht)
		     (let ((keys nil))
		       (maphash #'(lambda (key val)
				    (declare (ignore val))
				    (push key keys))
				ht)
		       keys))
		 (list *rulebase* *steps* *rules*))))

(defun label-step (label fnums push?)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (fnums (extract-fnums-arg fnums)))
	(cond ((or (stringp label) (symbolp label))
	       (let ((strlbl (if (stringp label) label (string label)))
		     (symlbl (if (symbolp label) label (intern label :pvs))))
		 (cond ((memq symlbl '(quote nil * + -))
			(error-format-if
			 "~%Label cannot be nil, quote, *, +, or -.")
			(values 'X nil nil))
		       ((and (or (digit-char-p (char strlbl 0))
				 (char= (char strlbl 0) #\-))
			     (every #'digit-char-p (subseq strlbl 1)))
			(error-format-if
			 "~%Label cannot be an integer"))
		       (t
			(multiple-value-bind (signal subgoal)
			    (sequent-reduce
			     goalsequent
			     #'(lambda (sform)
				 (values
				  '?
				  (if (and push? label)
				      (lcopy sform
					     'label (union (list symlbl) (label sform)))
				      (lcopy sform
					'label (when label 
						 (list symlbl))))))
			     fnums)
			  (values signal (list subgoal) ;;(substitution ps)
				  ))))))
	      (t
	       (error-format-if "~%Label ~a is not a string." label)
	       (values 'X nil nil))))))

(defun unlabel-step (fnums label)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (nfnums (cond ((null fnums) '*)
			   ((consp fnums) fnums)
			   (t (list fnums))))
	     (sforms (find-all-sformnums (s-forms goalsequent) nfnums
					  #'always-true))
	     (nlabel (if (stringp label) (intern label :pvs) label)))
	(cond (sforms
	       (multiple-value-bind
		   (signal subgoal)
		   (sequent-reduce goalsequent
				   #'(lambda (sform)
				       (values '?
					       (if nlabel
						   (lcopy sform 'label
							  (loop for y in (label sform)
								unless (eq nlabel y)
								collect y))
						   (lcopy sform 'label nil))))
				   sforms)
		 (values signal (list subgoal))))
	      (t
	       (error-format-if "~%No formulas match ~a" (or fnums '*))
	       (values 'X nil nil))))))

(defun fnum-p (e)
  (or (numberp e)
      (and (symbolp e)
	   (or (memq e '(+ - *))
	       ;; Check for labels
	       (member e (collect-labels-of-current-sequent)
		       :test #'string=)))))
	
(defun collect-labels-of-current-sequent ()
  (reduce #'union (s-forms (current-goal *ps*)) :key #'label))

(defun just-install-proof-step (proof ps)
  (progn (setq *context-modified* t)
	 (values '! nil (list 'justification
			      (make-instance 'justification
				:label (label-suffix (label ps))
				:rule `(rerun ,proof))))))

(defun nth-or-last (n list)
  (if (< n (length list))
      (nth n list)
      (if (consp list)
	  (car (last list))
	  list)))

(defun semi-colonize (comment-string)
  (let ((newline-position
	 (position #\newline comment-string)))
    (if newline-position
	(let ((preline (subseq comment-string 0 newline-position))
	      (postline (subseq comment-string (1+ newline-position))))
	  (format nil ";;; ~a~%~a"
	    preline
	    (semi-colonize postline)))
	(format nil ";;; ~a" comment-string))))

(defun comment-step (string)
  #'(lambda (ps)
      (cond ((stringp string)
	     (let ((*print-length* nil)
		   (*print-level* nil))
	       (values '? (list (list (current-goal ps)
				      'comment
				      (semi-colonize string))))))
	    (t (error-format-if "~%Input ~a is not a string." string)
	       (values 'X nil nil)))))

(defun new-comment (proofstate)
  (let ((par-ps (parent-proofstate proofstate)))
    (and (or (not par-ps)
	     (not (eq (comment par-ps)(comment proofstate))))
	(comment proofstate))))

(defmethod comment ((list list)) (cadddr list))

(defun complete-checkpointed-proof (proof)
  (let ((*checkpointed-branches* nil))
    (complete-checkpointed-proof* proof)))

(defun complete-checkpointed-proof* (form)
  (cond ((and (consp form)
	      (stringp (car form)))
	 (cond ((member '(checkpoint) (cdr form) :test #'equal)
		(push form *checkpointed-branches*)
		form
		;;(ldiff form (cdr (member '(checkpoint) (cdr form) :test #'equal)))
		)
	       ((and (consp (car (last form)))
		     (every #'consp (car (last form))))
		(let ((nlast (mapcar #'complete-checkpointed-proof*
			       (car (last form)))))
		  (cond ((some #'(lambda (ff)
				   (memq ff *checkpointed-branches*))
			       (car (last form)))
			 (push form *checkpointed-branches*)
			 (append
			  (butlast form)
			  (list
			   (mapcar #'(lambda (ff nf)
				       (if (memq ff *checkpointed-branches*)
					   nf
					   (list (car ff)
						 (list 'just-install-proof
						       nf))))
			     (car (last form))
			     nlast))))
			(t form))))
	       (t form)))
	(t form)))

(defun strip-rerun (justif)
  (let ((label (label justif))
	(rule (rule justif)))
    (if (and (consp rule)
	     (eq (car rule) 'rerun)
	     (equal (label (cadr rule)) label))
	(cadr rule)
	justif)))

(defun seq-form-bindings (formula)
  (if (or (exists-expr? formula)
	  (forall-expr? formula))
      (bindings formula)
      (if (and (negation? formula)
	       (or (exists-expr? (args1 formula))
		   (forall-expr? (args1 formula))))
	  (bindings (args1 formula))
	  nil)))

(defun collect-prover-input-strings (input &optional alist)
  (cond ((null input)
	 alist)
	((stringp input)
	 (if (or (equal input "")
		 (assq input alist))
	     alist
	     (acons input nil alist)))
	((symbolp input) alist)
	((consp input)
	 (collect-prover-input-strings
	  (cdr input)
	  (collect-prover-input-strings (car input) alist)))
	(t alist)))
