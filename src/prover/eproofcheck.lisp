;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproofcheck.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Apr  2 21:12:58 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 22 18:56:13 1999
;; Update Count    : 10
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :pvs)

(defvar *subgoals* nil)

(defmethod prove (name &key  strategy)
  (let ((decl (get-formula *current-theory*
			   (if (stringp name)(intern name) name))))
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

(defun explain-errors ()
  (when *first-strategy-error*
    (when *last-strategy-error*
      (format t "~%The following errors occurred within the strategy:~%"))
    (format t "~a~%" *first-strategy-error*)
    (when *last-strategy-error*
      (format t "~a~%" *last-strategy-error*))
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

(defun simplify-expression (expr module-name strategy
				 &optional display? (id 'simplify-expr))
  (let* ((*suppress-printing* t)
	 (*printproofstate* nil)
	 (*proving-tcc* t)
	 (*generate-tccs* t)
	 (*subgoals* t)
	 (*current-theory* (get-theory module-name))
	 (*current-context* (context *current-theory*))
	 (expr (typecheck-uniquely (pc-parse expr 'expr)))
	 (sk-id 'F!1)
	 (bexpr (if (tc-eq (find-supertype (type expr)) *boolean*)
		    expr
		    (let* ((ftype (mk-funtype (type expr) *boolean*))
			   (skdecl (make-instance 'skolem-const-decl
				     'id sk-id
				     'type ftype
				     'module *current-theory*))
			   (*in-checker* t))
		      (copy-prover-context)
		      (setf (declarations-hash *current-context*)
			    (copy (current-declarations-hash)))
		      (put-decl skdecl (current-declarations-hash))
		      (make!-application (typecheck (pc-parse sk-id 'expr)
					   :expected ftype)
					 expr))))
	 (cexpr (universal-closure bexpr))
	 (expr-decl (make-instance 'formula-decl
		      'id (ref-to-id id)
		      'module *current-theory*
		      'spelling 'formula
		      'definition cexpr))
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
    (prove-decl expr-decl :strategy `(then (then ,strategy (postpone)) (quit))
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
			       (id (intern (subseq str 0 (position #\! str))))
			       (nid (make-new-variable id expr))
			       (nbd (mk-bind-decl nid (type ex))))
			  (push (cons ex nbd) alist)
			  nbd)))
		#'(lambda (ex)
		    (and (name-expr? ex)
			 (skolem-constant? (declaration ex)))))))
    (universal-closure nex)))

(defvar *subgoals* nil)

(defun simplify-expr (expr module-name strategy
			   &optional display? (id 'simplify-expr))
  ;;in the context of decl, simplify the boolean expr using strategy
  (let* ((*suppress-printing* t)
	 (*printproofstate* nil)
	 (*proving-tcc* t)
	 (*generate-tccs* t)
	 (*subgoals* t)
	 (*current-theory* (get-theory module-name))
	 (*current-context* (context *current-theory*))
	 (expr (typecheck (pc-parse expr 'expr)
		 :expected *boolean*))
	 (closed-expr (universal-closure expr))
	 (expr-decl (make-instance 'formula-decl
		      'id (ref-to-id id)
		      'module *current-theory*
		      'spelling 'formula
		      'definition closed-expr))
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
			 (make!-conjunction* antes)
			 (make!-implication
			  (make!-conjunction* antes)
			  (make!-disjunction* succs))))))
     (reverse subgoals))))

(defvar *force-dp* nil)

(defvar *dump-sequents-to-file* nil)
(defvar *show-parens-in-proof* nil)

(defmethod prove-decl :around ((decl formula-decl) &key strategy context)
  (if (or *proof-timeout*
	  (and *noninteractive*
	       *noninteractive-timeout*))
      (let ((timeout (or *proof-timeout* *noninteractive-timeout*)))
	(mp:with-timeout (timeout (pvs-message "Interrupted: ~a sec timeout"
				    timeout))
			 (call-next-method)))
      (call-next-method)))

(defmethod prove-decl ((decl formula-decl) &key strategy context)
  (ensure-default-proof decl)
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
	 (*pvs-bdd-hash* nil)
	 (*bdd-pvs-hash* nil)
	 (*bdd-counter* *bdd-counter*)
	 (*subtype-of-hash* (init-if-rec *subtype-of-hash*))
	 (*track-rewrites* nil)
	 (*context-modified* nil)
	 (*generate-tccs* 'none)
	 (*rewrite-msg-off* *rewrite-msg-off*)
	 (*ruletrace* nil)
	 (*ruletracedepth* 0)
	 ;; Hash tables
	 (*assert-if-arith-hash* (init-if-rec *assert-if-arith-hash*))
	 (*auto-rewrites* (init-if-rec *auto-rewrites*))
	 (*auto-rewrites-ops* (init-if-rec *auto-rewrites-ops*))
	 (*beta-cache* (init-if-rec *beta-cache*))
	 (*match-cache* (init-if-rec *match-cache*))
	 (*subtype-of-hash* (init-if-rec *subtype-of-hash*))
	 (*create-formulas-cache* (init-if-rec *create-formulas-cache*))
	 (*term-print-strings* (init-if-rec *term-print-strings*))
	 (*all-subst-mod-params-caches* (copy-subst-mod-params-cache))
	 (*pseudo-normalize-hash* (copy *pseudo-normalize-hash*))
	 (*pseudo-normalize-translate-id-hash*
	  (copy *pseudo-normalize-translate-id-hash*))
	 ;;
	 (*current-context* (or context (context decl)))
	 (*current-theory* (module decl))
	 (*in-checker* t) ; Make sure this follows setting of *current-context*
	 (*current-decision-procedure* (determine-decision-procedure decl))
	 (auto-rewrites-info (initialize-auto-rewrites))
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
    (newcounter *skovar-counter*)
    (newcounter *skofun-counter*)
    (newcounter *bind-counter*)
    (unless (closed-definition decl)
      (setf (closed-definition decl)
	    (universal-closure (definition decl))))
    (let* ((top-formula (closed-definition decl))
	   (s-form (make-instance 's-formula 'formula top-formula))
	   (sequent (make-instance 'sequent 's-forms (list s-form)))
	   (*init-dp-state* (dpi-empty-state))
	   (*dp-state* (dpi-push-state *init-dp-state*))
	   (*top-dp-state* (dpi-push-state *init-dp-state*))
	   (*top-proofstate*
	    (make-instance 'top-proofstate
	      'current-goal sequent
	      'label (string (id decl))
	      'strategy (if strategy
			    strategy
			    (query*-step))
	      'context *current-context*
	      'dp-state *dp-state*
	      'justification (justification decl)
	      'declaration decl
	      'current-auto-rewrites auto-rewrites-info)))
      (before-prove*)
      (unwind-protect
	   (dpi-start #'prove-decl-body)
	(after-prove*)
	(dpi-end *top-proofstate*)
	(unless *recursive-prove-decl-call*
	  (save-proof-info decl init-real-time init-run-time)))
      *top-proofstate*)))

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
      (pvs-error "Proof Error"
	(format nil "Can't find the ~a decision procedure"
	  (decision-procedure-used decl)))))

(defmethod prove-decl ((decl declaration) &key strategy)
  (declare (ignore strategy))
  (error-format-if "~%Couldn't find formula ~a in module ~a."
	     (id decl) (id *current-theory*)))

(defun before-prove* ()
  (when *start-proof-display*
    (let ((*ps* *top-proofstate*))
      (call-x-show-proof)))
  (when (or (not *proving-tcc*)
	    *noninteractive*)
    (pvs-emacs-eval "(setq pvs-in-checker t)")))

(defun initialize-auto-rewrites ()
  (let* ((rewrites+ (mapappend #'rewrite-names
			       (auto-rewrites *current-context*)))
	 (rewrites- (mapappend #'rewrite-names
			       (disabled-auto-rewrites *current-context*)))
	 (rewrites (set-difference rewrites+ rewrites- :test #'tc-eq)))
    (auto-rewrite rewrites *top-proofstate*))
  (make-instance 'auto-rewrites-info
    'rewrites *auto-rewrites*
    'auto-rewrites-names *auto-rewrites-names*
    'auto-rewrites!-names *auto-rewrites!-names*
    'macro-names *macro-names*
    'all-rewrites-names *all-rewrites-names*))

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
      (pvs-emacs-eval "(setq pvs-in-checker nil)")
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
  (clrhash *assert-if-arith-hash*)
  (clrhash *auto-rewrites*)
  (clrhash *auto-rewrites-ops*)
  (clrhash *beta-cache*)
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

(defun save-proof-info (decl init-real-time init-run-time)
  (let ((prinfo (default-proof decl))
	(script (extract-justification-sexp
		 (collect-justification *top-proofstate*))))
    (cond ((null (script prinfo))
	   (setf (script prinfo) script))
	  ((and (not *proving-tcc*)
		(not *noninteractive*)
		script
		(not (equal script '("" (postpone) nil nil)))
		(not (equal (script prinfo) script))
		(let ((ids (mapcar #'id
			     (remove-if-not #'(lambda (prinfo)
						(equal (script prinfo) script))
			       (proofs decl)))))
		  (pvs-yes-or-no-p
		   "~@[This proof is already associated with this formula ~
                       as ~{~a~^, ~}~%~]~
                    Would you like the proof to be saved~@[ anyway~]? "
		   ids ids)))
	   (cond ((pvs-yes-or-no-p
		   "Would you like to overwrite the current proof (named ~a)? "
		   (id prinfo))
		  (setf (script prinfo) script))
		 (t (let ((id (read-proof-id (next-proof-id decl)))
			  (description (read-proof-description)))
		      (setq prinfo
			    (make-default-proof decl script id
						description)))))))
    (setf (real-time prinfo) (- (get-internal-real-time) init-real-time))
    (setf (run-time prinfo) (- (get-run-time) init-run-time))
    (setf (run-date prinfo) (get-universal-time))
    (setf (proof-status decl)
	  (if (and (eq (status-flag *top-proofstate*) '!)
		   (not *context-modified*))
	      'proved
	      'unfinished))
    (format-if "~%~%Run time  = ~,2,-3F secs." (run-time prinfo))
    (format-if "~%Real time = ~,2,-3F secs.~%" (real-time prinfo))
    (when *context-modified*
      (setf (proof-status decl) 'unfinished)
      (when (and (not *proving-tcc*)
		 (pvs-yes-or-no-p
		  "~%Context was modified in mid-proof.  ~
                     Would you like to rerun the proof?~%"))
	(let ((*in-checker* nil))
	  (prove-decl decl :strategy '(then (rerun) (query*))))))))

(defun read-proof-id (default)
  (format t "Please enter an id (default ~a): " default)
  (let ((id (read-line)))
    (cond ((equal id "") default)
	  ((valid-proof-id id) (intern id))
	  (t (format t "~a is not a legal proof identifier:~%" id)
	     (read-proof-id default)))))

(defun valid-proof-id (str)
  (and (alpha-char-p (char str 0))
       (every #'(lambda (ch)
		  (or (alpha-char-p ch)
		      (digit-char-p ch)
		      (member ch '(#\_ #\? #\-) :test #'char=)))
	      (subseq str 1))))

(defun read-proof-description ()
  (format t "Please enter a description: ")
  (read-line))

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


(defmethod prove* ((proofstate proofstate))
  (let ((*current-context* (context proofstate)))
    (labels ((prove*-int (proofstate)
		(setf *current-context* (context proofstate))
		(when (and (null *printproofstate*)
			   (fresh? proofstate)
			   (not (typep (strategy proofstate) 'strategy))
			   ;;(not (typep proofstate 'strat-proofstate))
			   ;;(not (null (strategy proofstate)))
			   ;;(NSH:11.17.94): commented out
			   )
		  ;;(let ((*to-emacs* t))
		  ;;      (pvs-buffer "*goal*"
		  ;;		      proofstate
		  ;;		      t t))
		  ;;  (break)
		  (print-proofstate-if proofstate)
		  (unless *suppress-printing*
		    (clear-strategy-errors))
		  )
		(let ((nextstate (proofstepper proofstate)))
		  (cond  ((null (parent-proofstate proofstate))
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
				   (format t "Subgoal completed"))
				 (setf (status-flag proofstate) '*)
				 proofstate)
				(t (prove*-int nextstate))))
			 (t (prove*-int nextstate))))))
      (prove*-int proofstate))))

;;12/16: need to fix prove* to save the proof, etc.

(defun check-command-arguments (cmd keywords arguments has-rest? &optional expect-key?)
  (or (null arguments)
      (if (or expect-key?
	      (keywordp (car arguments)))
	  (cond ((not (keywordp (car arguments)))
		 (error-format-if "~%Found ~a when expecting a keyword in argument to ~a"
				  (car arguments) cmd)
		 nil)
		((not (memq (car arguments) keywords))
		 (error-format-if "~%~a is not a valid keyword for ~a"
				  (car arguments) cmd)
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
		 (check-command-arguments cmd keywords (cddr arguments) has-rest? t)))
	  (check-command-arguments cmd keywords (cdr arguments) has-rest?))))

(defun check-arguments (pcmd)
  (let* ((keylist (assq (car pcmd) *prover-keywords*))
	 (keywords (cddr keylist))
	 (has-rest? (cadr keylist)))
    (cond ((not keylist)
	   ;; The user may have used a $ form for a rule, remove it and check
	   ;; so we can give a more informative error message.
	   (let* ((dolform (string (car pcmd)))
		  (dolpos (1- (length dolform)))
		  (rawform (when (and (>= dolpos 0)
				      (char= (char dolform dolpos) #\$))
			     (subseq dolform 0 dolpos))))
	     (if (and rawform
		      (assq (intern rawform) *prover-keywords*)
		      (not (and (> dolpos 0)
				(char= (char rawform (1- dolpos)) #\$))))
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

(defun qread (prompt)
  (format t "~%~a"  prompt)
  (force-output)
  (let ((input (unwind-protect
		   (progn (excl:set-case-mode :case-insensitive-lower)
			  (ignore-errors (read)))
		 (excl:set-case-mode :case-sensitive-lower))))
    (cond ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
	       (throw 'quit nil)
	       (qread prompt)))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (throw 'abort nil)
	       (qread prompt)))
	  ((eq input :reset)         ;; from M-x reset-pvs 
	   (throw 'quit 'pvs-reset))
	  (t (auto-save-proof)
	     (if (consp input)
		 (if (check-arguments input)
		     input
		     (qread prompt))
	         input)))))

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
     (let ((post-proofstate  ;;check if current goal is prop-axiom.
	    (cond ((eq (check-prop-axiom (s-forms (current-goal proofstate)))
		       '!) ;;set flag to proved! and update fields. 
		   (setf (status-flag proofstate) '!      
			 (current-rule proofstate) '(propax)
			 (printout proofstate)
			 (format nil "~%which is trivially true.")
			 (justification proofstate)
			 (make-instance 'justification
			   'label (label-suffix (label proofstate))
			   'rule '(propax)))
		   proofstate)  ;;else display goal, 
		               ;;eval strategy, invoke rule-apply
		  (t (catch-restore  ;;in case of restore/enable interrupts
		      (progn
			(when ;;(not (strat-proofstate? proofstate))
			    ;;NSH(8.19.95): display-proofstate called only
			    ;;when current state is root, or has rule/input.
			    ;;in-apply taken care of in display-proofstate. 
			    (and (not (strat-proofstate? proofstate))
				 (or (null (parent-proofstate proofstate))
				     (current-rule (parent-proofstate proofstate))
				     (current-input (parent-proofstate proofstate))))
			  (display-proofstate proofstate))
			(let* ((*rule-args-alist* nil)
			       (strategy
				(strat-eval*
				 (if (strategy proofstate)
				     (strategy proofstate)
				     '(postpone t));;if no strat, move on.
				 proofstate)))
			  (setf (strategy proofstate) strategy)
			  (rule-apply strategy proofstate))))))))
		;;rule-apply returns a revised proofstate.
		(cond ((null post-proofstate);;hence aborted
		       (let ((nps ;;NSH(7.18.94) for proper restore
			      (nonstrat-parent-proofstate
			       proofstate)))
			 (setf (status-flag nps) nil
			       (remaining-subgoals nps) nil
			       (current-subgoal nps) nil
			       (pending-subgoals nps) nil
			       (done-subgoals nps) nil
			       (strategy nps)
			       (query*-step));;unevaluated
		       ;;(query*-step)
		       ;;is okay here.
			 nps))
		      ((eq (status-flag post-proofstate) '?) ;;subgoals created
		       (format-printout post-proofstate) ;;print commentary
		       (cond ((> (length (remaining-subgoals post-proofstate)) 1)
			      (when (and *rerunning-proof*
					 (integerp *rerunning-proof-message-time*)
					 (> (- (get-internal-real-time)
					       *rerunning-proof-message-time*)
					    3000))  ;;print mini-buffer msg
				(setq *rerunning-proof* (format nil "~a." *rerunning-proof*))
				(setq *rerunning-proof-message-time*
				      (get-internal-real-time))
				(pvs-message *rerunning-proof*))
			      (format-if "~%this yields  ~a subgoals: "
					 (length (remaining-subgoals post-proofstate))))
			     ((not (typep (car (remaining-subgoals post-proofstate))
					  'strat-proofstate))
			      (format-if "~%this simplifies to: "
					 )))
		       post-proofstate)
		      ((eq (status-flag post-proofstate) '!);;rule-apply proved
		       (format-printout post-proofstate)
		       (wish-done-proof post-proofstate)
		       (dpi-end post-proofstate)
;		       (when (printout post-proofstate)
;			 (format-if (printout post-proofstate)))
		       post-proofstate)
		      (t  post-proofstate))))
;;if incoming goal has subgoals
   ((eq (status-flag proofstate) '?)     ;;working on subgoals
    (cond ((null (remaining-subgoals proofstate))
	   (cond ((null (pending-subgoals proofstate))
		  (success-step proofstate));;no more subgoals,declare success
		 (t ;;pending subgoals
		  (post-processing-step proofstate))))
	  (t ;;subgoals remain
	   (let ((newps (pop (remaining-subgoals proofstate))))
	     (setf ;;(parent-proofstate newps) proofstate
		   (current-subgoal proofstate) newps)
;	           (substitution newps)
;		   (if (null (out-substitution proofstate))
;		       (substitution proofstate)
;		     (out-substitution proofstate))
;;		   (context newps)
;;		   (if (null (out-context proofstate))
;;		       (context proofstate)
;;		     (out-context proofstate))
		   
;	     (when (eq (status-flag newps) '*)
;	       (if (null (remaining-subgoals newps));;leaf node
;		   (setf (status-flag newps) nil;;make it fresh
;			 (strategy newps)
;			 (strategy proofstate))
;;			   (post-subgoal-strat (strategy proofstate))
;;			   nil
;			 (setf (status-flag newps) '?
;			       (strategy newps)
;			       (strategy proofstate))
;;			   (post-subgoal-strat (strategy proofstate))
;;			   nil
;			 )
;	       (setf (strategy proofstate);;zero out strategy
;		     (if (null (remaining-subgoals proofstate))
;			 nil
;			 (strategy proofstate))))


	     newps))))
   ((eq (status-flag proofstate) '*)  ;;pending goals, but proceeding
    (next-proofstate proofstate))
   ((memq (status-flag proofstate) '(X XX))  ;;failure
    (format-if "~%~%Attempted proof of ~a failed." (label proofstate))
    (next-proofstate proofstate))
   ((eq (status-flag proofstate) '!)  ;;success
    ;;(format t "~%~%Proved ~a." (label proofstate))
    (next-proofstate proofstate))
   (t (next-proofstate proofstate))))  ;;just in case

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
	(if quiet-flag pp (format-if "~a" pp))))))


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
	*in-apply*)))

(defun strat-eval* (strat ps)
  (let* ((*ps* ps)
	 (*par-ps* (get-parent-proofstate ps))
	 (* '*)
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
	 (*module-context* (copy-prover-context))
	 (*current-theory* *current-theory*))
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
	((if-form? strat);;(break "if")
	 (if (expr-eval (cadr strat))
	     (strat-eval (caddr strat))
	     (strat-eval (cadddr strat))))
	((try-form? strat)
	 (make-instance 'strategy
	   'topstep (strat-eval (cond-expr strat))
	   'subgoal-strategy (then-expr strat)
	   'failure-strategy (else-expr strat)))
	((let-form? strat)
	 (let ((let-value (let-eval (let-bindings strat))))
	   (strat-eval (subst-stratexpr
			     (let-body strat)
			     let-value
			     (reverse let-value)
			     ))))
	((eq (car strat) 'note)
	 ;;if this evaluates to a rule, then the input is noted with
	 ;;comment string which becomes part of the printout.
	 (let ((result
		(strat-eval (cadr strat))))
	   (when (typep result 'rule-instance)
	     (setf (rule-input result) strat
		   (rule-format result)
		   (if (cddr strat)
		       (cons (format nil "~%(~a)~a" (caddr strat)
				     (car (rule-format result)))
			     (cdr (rule-format result)))
		       (rule-format result))))
	   result))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (pair-formals-args (formals def)
					       (cdr strat)))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect
			    (if (consp x) ;;NHS(4.23.97)
				;;was ignoring args, otherwise.
				(cdr (assoc (car x) subalist))
				(cdr (assoc x subalist)))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr;;2/91:so that rules are APPLYed.
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
		(alist (pair-formals-args (formals def)
					       (cdr strat)))
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
		     (string (optional-name (cadr optionals)))))
	 (cadr optionals))
	((equal (string key)(string (optional-name (car optionals))))
	 (car optionals))
	(t (check-keyword key (cdr optionals)))))

;;NSH(2.28.95): check-keyword checks if key is a keyword in optionals.
;;get-keyword gets the value to be paired with the keyword. 

(defun get-keyword-arg (key optionals args)
  (cond ((null optionals) nil)
	((and (eq (car optionals) '&rest)
	      (consp (cdr optionals))
	      (equal (string key)
		     (string (optional-name (cadr optionals)))))
	 (when (consp args)
	   (if (listp (car args))
	       (cons (optional-name (cadr optionals)) (car args))
	       (cons (optional-name (cadr optionals))
		     (list (car args))))))
	((equal (string key)(string (optional-name (car optionals))))
	 (if (consp args)
	     (cons (optional-name (car optionals))(car args))
	     (cons (optional-name (car optionals))
		   (default (car optionals)))))
	(t (get-keyword-arg key (cdr optionals) args))))
	 

(defun pair-formals-args (formals args &optional opt-flag)
  (cond ((null formals) nil)
	((eq (car formals) '&rest)
	 (if (and (consp args)
		  (keywordp (car args)))
	     (let ((pair (get-keyword-arg (car args)
				formals
				(cdr args))))
	       (if (null pair)
		   (format t "~%Bad keyword in argument: ~a" (car args)))
	       (if pair
		   (list pair)
		   (if (consp (cdr formals))
		       (list (list (cadr formals)))
		       nil)))
	     (when (consp (cdr formals))
	       (list (cons (optional-name
			    (cadr formals))
			   (when (listp args) args))))))
	((eq (car formals) '&optional)
	 (pair-formals-args (cdr formals) args t))
	((null args) (if opt-flag
			 (collect-default-values formals)
			 (progn (error-format-if "~%Not enough actuals.")
				(restore))))
	((and opt-flag
	      (keywordp (car args)))
	 (let ((pair
		(get-keyword-arg (car args) formals (cdr args))))
	   (if (null pair)
	       (format t "~%Bad keyword in argument: ~a" (car args)))
	   (if  pair
	       (cons pair
		     (pair-formals-args (remove (car pair) formals
						:key #'optional-name)
					(cddr args)  opt-flag))

	       (cons (cons (optional-name (car formals))
			   (car args)) ;;NSH(3.16.95)
		     (pair-formals-args (cdr formals)(cdr args)
					opt-flag)))))
	(t (cons (cons (optional-name (car formals))  (car args))
		 (pair-formals-args (cdr formals)(cdr args) opt-flag)))))

(defun collect-default-values (formals)
  (when (consp formals)
    (if (eq (car formals) '&rest)
	(when (consp (cdr formals))
	  (list (list (optional-name (cadr formals)))))
	(cons (cons (optional-name (car formals))
		    (default (car formals)))
	      (collect-default-values (cdr formals))))))
(defun default (x)(when (and (consp x)(consp (cdr x)))(cadr x)))
(defun optional-name (x) (if  (consp x)(car x) x))
(defun quoted? (x) (and (consp x)(eq (car x) 'quote)))

(defun subst-stratexpr (expr alist reverse-alist)  
  (cond ((symbolp expr)
	 (let ((entry (assoc expr reverse-alist)))
	   (if (null entry) expr
	       (cdr entry))))
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
       (> (length x) 2)
       (eq (car x) 'let)
       (loop for y in (cadr x)
	     always (and (not (null y))(typep (car y) 'symbol)))))

(defun let-bindings (x) (when (let-form? x)(cadr x)))
(defun let-body (x) (when (let-form? x)(caddr x)))

(defun quote? (x)(and (consp x)(eq (car x) 'quote)))
(defvar *ps-globals* '(*label* *subgoalnum* *goal* *par-label* *par-goal*
		       *current-context* *module-context* *current-theory*))
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
     (*module-context* (copy-prover-context))
     (*current-theory* *current-theory*)))

(defun isfun? (sym)
  (and (symbolp sym)
       (or (fboundp sym)
	   ;;(special-form-p sym)
	   )))

(defun expr-eval (expr)
  (cond ((consp expr)
	 (if (or (gethash (car expr) *rulebase*)
		 (step-or-rule-defn (car expr))
		 (not (isfun? (car expr))))
	     expr
	     (eval expr)))
	(t (if (or (numberp expr)
		   (special-variable-p expr))
	       (eval expr)
	       expr))))

(defun label-suffix (label)
  (let ((pos (position #\. label :from-end t)))
    (if pos
	(let* ((suffix (subseq label (1+ pos)))
	       (check (every #'digit-char-p suffix)))
	  (if check suffix ""))
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
		 'label (label-suffix (label proofstate))
		 'rule  (sexp-unparse (current-rule proofstate))
		 'xrule (current-xrule proofstate)
		 'comment (new-comment proofstate)
		 'subgoals (mapcar #'(lambda (x) (justification x))
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
		 (intern (string-upcase name))
		 name))
	 (dp (car (member id *decision-procedures*))))
    (if dp
	(if (eq id *default-decision-procedure*)
	    (pvs-message "~a is already the default decision procedure" id)
	    (progn
	      (pvs-message "~a is now the default decision procedure" id)
	      (setq *default-decision-procedure* id)
	      (when *in-checker*
		(pvs-message
		    "You must restart the current proof to use the new default"))))
	(pvs-message "~a is not a known decision procedure" id))))

(defvar *report-mode* nil)
(defvar *print-ancestor* nil)

(defvar *print-descendants* nil)

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
      (format t "~%Q.E.D."))))

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
	       (format t  "~%~VTThis completes the proof of ~a.~%"
		 *prover-indent*
		 (label proofstate))
	       (format t "~%~VTPostponing the proof of ~a.~%"
		 *prover-indent*
		 (label proofstate)))
	   (cond ((> (length all-subgoals) 1)
		  (format t "~%~VTwe get ~a subgoals:"
		    *prover-indent*
		    (length all-subgoals))
		  (loop for x in  all-subgoals ;;(done-subgoals proofstate)
			do (report-proof* x 0)))
		 ((> (length (all-subgoals (car all-subgoals))) 1)
		  (format t "~%~VTthis simplifies to:" *prover-indent*)
		  (report-proof* (car all-subgoals) 0))
		 (t (when (and (or (null next-flag)(eql next-flag 0))
			       (printout proofstate))
		      (format t "~%~VTthis simplifies to:" *prover-indent*))
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
			   'label (label-suffix (label proofstate))
			   'comment (new-comment proofstate)
			   'rule '(postpone)))))))
	((eq (status-flag proofstate) '!)
	 (or (justification proofstate)
	     (make-instance 'justification
	       'label (label-suffix (label proofstate))
	       'rule (sexp-unparse (current-rule proofstate))
	       'xrule (current-xrule proofstate)
	       'comment (new-comment proofstate)
	       'subgoals
	       (sort 
		(mapcar #'collect-justification
		  (done-subgoals proofstate))
		#'<
		:key #'(lambda (x)(safe-parse-integer (label x)))))))
	((memq (status-flag proofstate) '(? *))
	 (make-instance 'justification
	   'label (label-suffix (label proofstate))
	   'rule (sexp-unparse (current-rule proofstate))
	   'xrule (current-xrule proofstate)
	   'comment (new-comment proofstate)
	   'subgoals
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
	     'label (label-suffix (label proofstate))
	     'rule '(postpone)
	     'comment (new-comment proofstate)))))



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
						       rule-args))
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
		 'rule (apply (rule-function entry)
			 args)
		 'rule-input rule
		 'rule-format (when (format-string entry)
				(cons (format-string entry)
				      args))))))))

(defun extract-optionals (optionals match)
  (cond ((null optionals) nil)
	((eq (car optionals) '&rest)
	 (when (cdr optionals)
	   (cdr (assoc (optional-name (cadr optionals)) match))))
	(t (cons (cdr (assoc (optional-name (car optionals))
			     match))
		 (extract-optionals (cdr optionals) match)))))

;	   (rulefun-entry (get-rule (funcall
;				     (apply (rulefun rulefun-entry)
;					    rule-args)
;				     ps)
;				    ps))
;	   (strategy-entry (make-instance
;			    'strategy-instance
;			    'strategy-fun
;			    (funcall (apply
;				      (strategy-fun strategy-entry)
;				      rule-args)
;				     ps)
;			    'strategy-input rule))
;	   (t )
	
(defun is-lambda (x)
  (and (functionp x)
       (not (symbolp x))))

(defun assert-tccforms (tccforms ps)
  (when tccforms
    (let* ((dp-state (dp-state ps)))
      (nprotecting-cong-state
       ((*dp-state* dp-state))
       ;;(break "atc")
       (let ((*rewrite-hash* (copy (rewrite-hash ps)))
	     (*subtype-hash* (copy (subtype-hash ps))))
	 (assert-tccforms* tccforms ps))))))

(defun assert-tccforms* (tccforms ps)
  (if (null tccforms) nil
      (multiple-value-bind (sig value)
	  (assert-if (tccinfo-formula (car tccforms)))
	(cond ((tc-eq value *true*)
	       (assert-tccforms* (cdr tccforms) ps))
	      ((eq sig 'X) (cons (car tccforms)
				 (assert-tccforms*
				  (cdr tccforms) ps)))
	      (t (when (tc-eq value *false*)
		   (error-format-if
		    "~%*WARNING*: TCC ~a ~%~11Tsimplifies to FALSE"
		    (tccinfo-formula (car tccforms))))
		 (setf (tccinfo-formula (car tccforms)) value)
		 (cons (car tccforms)
		       (assert-tccforms* (cdr tccforms) ps)))))))

(defun rule-apply (step ps)
  (let* ((*ps* ps)
	 (* '*)
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
	 (*current-theory* *current-theory*)
	 (*auto-rewrites* (rewrites ps))
	 (*all-rewrites-names* (all-rewrites-names ps))
	 (*auto-rewrites-names* (auto-rewrites-names ps))
	 (*auto-rewrites!-names* (auto-rewrites!-names ps))
 	 (*macro-names* (macro-names ps))	 
	 (*rewrite-hash* (rewrite-hash ps))
	 (*dp-state* (dp-state ps)))
    ;;(break)
    (cond ((typep step 'rule-instance);;if step is a rule, then
	   ;;reinvoke rule-apply with corresponding strategy. 
	   (rule-apply (make-instance 'strategy
			 'topstep step)
		       ps));;else step is a strategy
	  ((typep (topstep step) 'rule-instance)
	   (let* ((*tccforms* nil)
		  (topstep (topstep step))
		  (name (if (consp (rule-input topstep))
			    (car (rule-input topstep))
			    topstep)))
	     (when (memq name *ruletrace*)
	       (format t "~%~vTEnter: ~a" *ruletracedepth*
		       (rule-input topstep))
	       (incf *ruletracedepth*))
	     (multiple-value-bind (signal subgoals updates)
		 (funcall (rule topstep) ps);;(break "rule-ap")
	       (cond ((eq signal '!);;success
		      (when (memq name *ruletrace*)
			(decf *ruletracedepth*)
			(format t "~%~vT Exit: ~a -- Proved subgoal"
			  *ruletracedepth* name ))
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
						 'label (label-suffix (label ps))
						 'rule (rule-input topstep)
						 'comment (new-comment ps)))
		      (make-updates updates ps)
		      ps)
		     ((eq signal '?);;subgoals generated
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
				 (format t "~%No change. False TCCs: ~%~{  ~a, ~%~}"
				   (mapcar #'tccinfo-formula false-tccforms)))
			       (setf (status-flag ps) nil;;start afresh
				     (strategy ps)
				     (failure-strategy step))
			       ps)
			      (t (let* ((tcc-subgoals
				    (mapcar
					#'(lambda (x)
					    (let ((y 
						   (change-class
						    (copy (current-goal ps)
						      's-forms
						      (cons
						       (make-instance
							   's-formula
							 'formula
							 (tccinfo-formula x))
						       (s-forms
							(current-goal ps))))
						    'tcc-sequent)))
					      (setf (tcc y)
						    (tccinfo-formula x)
						    (reason y)
						    (tccinfo-reason x)
						    (expr y)
						    (tccinfo-expr x)
						    (kind y)
						    (tccinfo-kind x)
						    (type y)
						    (tccinfo-type x))
					      y))
				      tccforms))
				   (subgoal-proofstates
				    (make-subgoal-proofstates
				     ps
				     (subgoal-strategy step)
				     subgoals
				     tcc-subgoals
					;updates;must be attached to subgoals.
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
				(format t "~%~vT Exit: ~a -- ~a subgoal(s) generated."
				  *ruletracedepth* name (length subgoal-proofstates)))
			      (push-references *tccforms* ps)
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
			(format t "~%~vT Exit: ~a -- No change."
			  *ruletracedepth* name))
		      (unless *suppress-printing*
			(explain-errors)
			(format t "~%No change on: ~s" (rule-input topstep)))
		      (setf (status-flag ps) nil;;start afresh
			    (strategy ps)
			    (failure-strategy step))
		      (make-updates updates ps)
		      ps)
		     ((eq signal 'XX);;marks the current goal a failure
		      (setf (status-flag ps) 'XX)
		      ps)
		     ((eq signal '*)
		      (setf (status-flag ps) '*)
		      ps)
		     (t  (undo-proof signal ps))))))
	  ((typep (topstep step) 'strategy)
	   (setf (status-flag ps) '?
		 ;;		 (current-rule ps) (strategy-input rule)
		 (remaining-subgoals ps)
		 (make-subgoal-proofstates ps
					   (topstep step)
					   (list (current-goal ps))))
					;nil
	   ps)
	  (t (format t "~%Bad rule: ~a~%" step);;treated as skip
					;(break)
	     (setf (status-flag ps) nil;;start afresh
		   (strategy ps)
		   (failure-strategy (strategy ps)))
	     ps))))

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


(defvar *record-undone-proofstate* nil)

(defun undo-proof (info ps)
  (if (eq info 'undo)
      (cond ((and *record-undone-proofstate*
		  (eq ps (car *record-undone-proofstate*)))
	     (let ((oldps (cadr *record-undone-proofstate*))
		   (newps (caddr *record-undone-proofstate*)))
	       (format t "~%Restoring the proof to state prior to UNDO, ")
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
		   (format t "~%Undo operations must be immediately undone.")
		   (format t "~%No undo to undo."))
	       (setf (strategy ps) (query*-step));;(NSH:5/8/99)
	       ps))
      (let ((newps (findps info ps)))
	(cond ((null newps)
	       (format t "~%Sorry. Couldn't find such a proof state.")
	       (setf (strategy ps) (query*-step))
	       ps)
	      ((eq ps newps)
	       (format t "~%No change.")
	       (setf (strategy ps)(query*-step))
	       ps)
	      (t (format t "~%This will undo the proof to: ~a" newps)
		 (let ((response (pvs-y-or-n-p "Sure?")))
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
	(t (findps info (parent-proofstate ps)))))


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
	 (append (dependent-decls proofstate)
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
		    'current-goal
		    (let* (
			   (sequent
			    (if tcc-to-sequent? ;;NSH(10.3.95)
				;;added copy since sequents are shared
				;;so destructive change-class is bad.
				(change-class (copy sequent) 'sequent)
				sequent)))
		      (clean-goal sequent))
		    'context (copy-prover-context (context proofstate))
		    'strategy  strategy
		    'label
		    (if (= (length allsubgoals) 1)
			(label proofstate)
			(format nil "~a.~a" (label proofstate)
				goalnum))
		    'subgoalnum (1- goalnum)
		    'proof-dependent-decls proof-dependent-decls
		    'dependent-decls (dependent-decls proofstate)
		    ;;d-d can be nil but inheriting from parent is okay.
		    'dp-state (dpi-copy-state (dp-state proofstate))
		    'current-auto-rewrites
		    (current-auto-rewrites proofstate)
		    'rewrite-hash (rewrite-hash proofstate)
		    'subtype-hash (subtype-hash proofstate)
		    'tcc-hash (tcc-hash proofstate) ;;NSH(10.30.01)
		    'parent-proofstate proofstate
		    'comment (comment proofstate))))
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
    (cond ((or (null par-ps)(null pos)
	       (equal (label par-ps)(label ps)))
	   1)
	  (t (string-to-number (subseq label (1+ pos)))))))



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
;			       :context (context *current-theory*))))
;    (format-if "~%Adding declaration to current module.
;Other than that,")
;    (when (consp tc-decl)
;      (loop for decl in tc-decl
;	    do (add-decl decl (context *current-theory*))))
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

(defun retypecheck-sexp (sexp ps)
  (if (consp sexp)
      (if (eq (car sexp) 'typechecked)
	  (typecheck (parse :string (cadr sexp) :nt 'expr)
	    :expected
	    (typecheck (parse :string (caddr sexp)
			      :nt 'type-expr)
	      :context *current-context*)
	    :tccs 'none;;NSH(10.20.94) tccs generated
	    ;;at rule-application       
	    :context *current-context*)
	  (cons (retypecheck-sexp (car sexp) ps)
		(retypecheck-sexp (cdr sexp) ps)))
      sexp))
					       


(defun mystring<= (st1 st2)
  (or (< (length st1)(length st2))
      (and (eql (length st1)(length st2))
	   (string<= st1 st2))))
      

(defun rerun-step (justif  recheck? &optional break?)
  (cond ((or (typep justif 'justification)
	     (consp justif))
	 (let* ((top-rule-in (if (and recheck?
				      (typep justif 'justification)
				      (xrule justif))
				 (xrule justif)
				 (rule justif))))
	   (format-if "~%Rerunning step: ~s" (format-rule top-rule-in t))
	   (if nil ;(and (consp top-rule-in)
		    ;(not (step-or-rule-defn (car top-rule-in))))
	       (rerun-step (when (subgoals justif)
			       (car (subgoals justif)))
			   recheck? break?)
	       (let ((top-rule `(let ((x (retypecheck-sexp (quote ,top-rule-in)
							   *ps*)))
				 x))
		      (subgoal-strategy
		       (loop for subjustif in
			     (sort (subgoals justif) #'mystring<=
				   :key #'label)
			     collect `(let ((strat (rerun-step (quote
								,subjustif) ,recheck? ,break?)))
				       strat)))
		       )
		 (if break?
		     `(spread! ,top-rule ,subgoal-strategy)
		     `(spread@ ,top-rule ,subgoal-strategy))))))
	(t (query*-step))))
	
;(defun rerun-step (justif)
;  (cond ((or (typep justif 'justification)
;	     (consp justif))
;	 (let* ((top-rule (rule justif))
;		(rule-name (car top-rule))
;		(rule-entry (gethash rule-name *rulebase*))
;		(strategy-entry (gethash rule-name *strategies*)))
;	   (if rule-entry
;	       (make-instance 'strategy
;		 'top-rule-function
;		 #'(lambda (ps)
;		     (declare (ignore ps))
;		     (format-if "~%~a~%" (rule justif))
;		     (retypecheck-sexp (rule justif) ps))
;		 'subgoal-strategy-function
;		 #'(lambda (goal)
;		     (rerun-step (find (label goal)
;				       (subgoals justif)
;				       :test #'(lambda (x y)
;						 (equal x (label y))))))
;		 'strat-if-a-subgoal-fails
;		 (postpone-strat)
;		 )
;	       (rerun-step (car (subgoals justif))))))
;	(t (query*-step))))

;(add-strategy-fun 'rerun nil nil (rerun)
;		  "(rerun): redoes a proof")

(defmethod extract-justification-sexp ((justification justification))
  (list (label justification)
	(sexp-unparse (rule  justification))
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
	     (check-edited-justification (cdr ejustif)
					 (car ejustif))
	     (values "Label must be a string of numbers." (car ejustif))))
	(t
	 (if (and (consp (car ejustif))
		  (consp (caar ejustif)))
	     (some #'check-edited-justification (car ejustif))
	     (if (consp (car ejustif))
		 (check-edited-justification (cdr ejustif) label)
		 (if (stringp (car ejustif))
		     (if (valid-comment-string? (car ejustif))
			 (check-edited-justification (cdr ejustif) label)
			 (values "Invalid comment string" (car ejustif)))
		     (values "A rule application must be a list."
			     (car ejustif))))))))

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

(defun revert-justification (ejustif &optional label)
  (unless (null ejustif)
    (if  (null label)
	 (if (consp (cadr ejustif)) 
  	     (list (car ejustif)
	       (unformat-rule (cadr ejustif))
	       (revert-justification (cddr ejustif)(car ejustif)))
	     (revert-justification (cddr ejustif)(car ejustif)))
	 (if (and (consp (car ejustif))(consp (caar ejustif)))
	     (mapcar #'revert-justification (car ejustif))
	     (if (consp (car ejustif))
  	         (list (list label
			 (unformat-rule (car ejustif))
			 (revert-justification (cdr ejustif) label)))
		 (revert-justification (cdr ejustif) label))))))
  

(defmethod pp-justification ((justification justification))
  (pp-justification* (extract-justification-sexp justification) nil))

(defmethod pp-justification (justification)
  (pp-justification* justification nil))


(defun pp-justification* (justification label)
  (cond ((null justification)
	 nil)
	(t (when (comment justification)
	     (format t "~%~a" (comment justification)))
	   (cond ((equal (label justification) label)
		  (format t "~%")
		  (format t "~V@T" (+ 3 (length (string label)))))
		 (t (format t "~%~a : " (label justification))))
	   (write (format-rule (rule justification)) :pretty t)
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
	     *pvs-modules*)
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
		   (module (gethash mod *pvs-modules*)))
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
		       (if (equal (string x)(string (optional-name y)))
			   (optional-name y)
			   nil)))))

(defun match-formals-with-actuals (required optional  actuals)
  (pair-formals-args (append required (cons '&optional optional))
		     actuals))
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
;		     (cons (cons (optional-name entry) (cadr actuals))
;			   result)
;		     (cons (cons (optional-name entry) (default entry))
;			   result)))
;	     (if (consp optional)
;		 (cons (cons (optional-name (car optional))(car actuals))
;		       (match-formals-with-actuals required (cdr optional)
;						   (cdr actuals)))
;		 nil))))
;	(t (loop for x in optional when (not (eq x '&rest))
;		 collect (cons (optional-name x) (default x)))))
				 
	
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
					;		      'rule-part #'simp
					;		      'rule-input '(simp)))


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
  (list form))

(defmethod and++ ((form conjunction) depth)
  (nconc (and++ (args1 form) (when depth (1- depth)))
	 (and++ (args2 form) (when depth (1- depth)))))

(defmethod and++ ((form iff) depth)
  (list (make!-implication (args1 form) (args2 form))
	(make!-implication (args2 form) (args1 form))))

(defmethod and++ ((form cases-expr) depth)
  (and++ (translate-cases-to-if form) (when depth (1- depth))))

(defmethod and++ ((form branch) depth)
  (list (make!-implication (condition form) (then-part form))
	(make!-implication (negate! (condition form)) (else-part form))))

(defmethod and++ ((form negation) depth)
  (or (and+-negation (argument form) (when depth (1- depth)))
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
  (list (make!-negation (make!-conjunction (condition form) (then-part form)))
	(make!-negation (make!-conjunction (make!-negation (condition form))
					   (else-part form)))))

(defmethod and+-negation (form depth)
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
    ;;    (format t "~%conjunct = ~a" (formula selected-sform))
    (if (or (null selected-sform)
	    (not (and+form? (formula (car selected-sform))))
	    (eql depth 0))
	(values 'X nil nil)
	(let* ((sel-sform (car selected-sform))
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
	  (values '? new-sequents;;(substitution ps)
		  )))))

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
  (cond ((consp fnums)
	 (if (eq (car fnums) 'quote)
	     (cleanup-fnums (cadr fnums))
	     (append (cleanup-fnums (car fnums)) (cleanup-fnums (cdr fnums)))))
	((null fnums) nil)
	((stringp fnums) (list (intern fnums)))
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
(defun find-all-sformnums (sforms sformnums pred &optional (pos 1) (neg -1))
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
			  (memq (intern sformnum)
				(label (car sforms)))))
		 (funcall pred (car sforms)))
	    neg
	    (find-sform* (cdr sforms) sformnum pred pos (1- neg)))
	(if (and (or (memq sformnum '(* +))
		     (equal sformnum pos)
		     (and (label (car sforms))
			  (or (symbolp sformnum)
			      (stringp sformnum))
			  (memq (intern sformnum)
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
	     :lines *prover-print-lines*))

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
	       "~%~V@T~6@<~:[{~a~@[,~a~]}~;[~a~@[,~a~]]~]~>~:[~;~%~6<~>~]"
	     *prover-indent* (memq sform par-sforms) sfnum
	     (label sform) (label sform))
	   (write (unparse-sform sform) :stream stream)))))

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
    (format t "~%~a:  ~a" (label proofstate)
	    (current-input proofstate))
    (when (eq (status-flag proofstate) '!)
      (format t "  Done!") ))
  (cond ((eq (status-flag proofstate) '!)
	 (when (not (current-input proofstate))
	   (format t "~%~a: Done!" (label proofstate)))
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
	 (format t "~%~a : *pending*" (label proofstate)))
	((fresh? proofstate)
	 (format t "~%~a: *remaining*" (label proofstate)))
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
		(format t "The hidden formulas in the current sequent are:")
		(let ((*print-ancestor* nil))
		  (format t "~a" (make-instance 'sequent
				   's-forms hforms))))
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
    (format t "~%follows by ~s from:" (current-rule ps))
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
	(format t "~%The remaining siblings are: ")
	(loop for subgoal in (remaining-subgoals par-ps)
	      do (print-proofstate subgoal)))
      (when (pending-subgoals par-ps)
	(format t "~%The postponed siblings are: ")
	(loop for subgoal in (pending-subgoals par-ps)
	      do (print-proofstate subgoal)))
      (when (done-subgoals par-ps)
	(format t "~%The proved siblings are: ")
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
		     ((subtype)(format t "Subtype "))
		     ((termination) (format t "Termination "))
		     ((cases) (format t "Cases "))
		     ((existence) (format t "Nonemptiness/Existence "))
		     ((actuals) (format t "Actuals "))
		     ((assuming) (format t "Assuming ")))
		   (format t "TCC ~%~a ~%generated by typechecking~%~a : ~a"
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
	     (fnums (if (consp fnums) fnums (list fnums))))
	(cond ((or (stringp label)(symbolp label))
	       (cond ((memq (intern label) '(quote nil * + -))
		      (error-format-if
		       "~%Label cannot be NIL, QUOTE, *, +, or -.")
		      (values 'X nil nil))
		     ((and (or (digit-char-p (char (string label) 0))
			       (char= (char (string label) 0) #\-))
			   (every #'digit-char-p (subseq (string label) 1)))
		      (error-format-if
		       "~%Label cannot be an integer"))
		     (t
		      (multiple-value-bind
			  (signal subgoal)
			  (sequent-reduce
			   goalsequent
			   #'(lambda (sform)
			       (values
				'?
				(if (and push? label)
				    (lcopy sform
				      'label (cons (intern label)
						   (label sform)))
				    (lcopy sform
				      'label (when label 
					       (list (intern label)))))))
			   fnums)
			(values signal (list subgoal) ;;(substitution ps)
				)))))
	      (t (error-format-if "~%Label ~a is not a string." label)
		 (values 'X nil nil))))))

(defun unlabel-step (fnums label)
  #'(lambda (ps)
      (let* ((goalsequent (current-goal ps))
	     (nfnums (cond ((null fnums) '*)
			   ((consp fnums) fnums)
			   (t (list fnums))))
	     (sforms (find-all-sformnums (s-forms goalsequent) nfnums
					  #'always-true))
	     (nlabel (if (stringp label) (intern label) label)))
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
	
	
	 
			    

(defun just-install-proof-step (proof ps)
  (progn (setq *context-modified* t)
	 (values '! nil (list 'justification
			      (make-instance 'justification
				'label (label-suffix (label ps))
				'rule `(rerun ,proof))))))

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
	  (format nil ";;;~a~%~a"
	    preline
	    (semi-colonize postline)))
	(format nil ";;;~a" comment-string))))

(defun comment-step (string)
  #'(lambda (ps)
      (cond ((stringp string)
	     (let ((*print-length* nil)
		   (*print-level* nil))
	       (values '? (list (list (current-goal ps)
				      'comment
				      (semi-colonize string))))))
	    (t (error-format-if "~%Input ~a is not a string.")
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

(defun protect-emacs-output* (string pos &optional result)
  (if (< pos (length string))
      (protect-emacs-output*
       string
       (1+ pos)
       (case (char string pos)
	 (#\& (append '(#\& #\\) result))
	 (#\\ (append '(#\\ #\\) result))
	 (#\" (append '(#\" #\\) result))
	 (#\newline (append '(#\n #\\) result))
	 (t   (cons (char string pos) result))))
      (coerce (nreverse result) 'string)))

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
