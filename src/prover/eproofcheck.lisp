;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eproofcheck.lisp -- 
;; Author          : N. Shankar
;; Created On      : Thu Apr  2 21:12:58 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 02:05:25 1998
;; Update Count    : 7
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

(defun prove (name &key  strategy)
  (let ((decl (get-formula *current-theory*
			   (if (stringp name)(intern name) name))))
    (if decl
	(timeprover
	 (prove-decl decl
		     :strategy
		     (when strategy `(then ,strategy (query*)))))
	(format-if "~%No such formula.  Try again."))))

(defvar *steps* (init-symbol-table))
(defvar *rules* (init-symbol-table))
(defvar *dependent-decls* nil)

(defvar *ops* (init-symbol-table))

(defvar *subgoals* nil)

(defun simplify-expr (expr module-name strategy
			   &optional display? (id 'simplify-expr))
  ;;in the context of decl, simplify the boolean expr using strategy
  (let* ((*in-checker* t)
	 (*suppress-printing* T)
	 (*printproofstate* nil)
	 (*proving-tcc* T)
	 (*generate-tccs* T)
	 (*subgoals* T)
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
    (prove-decl expr-decl :strategy `(then (then ,strategy (postpone)) (quit)))
    *subgoals*))

(defmethod prove-decl ((decl formula-decl) &key strategy)
  (let ((init-real-time (get-internal-real-time))
	(init-run-time (get-internal-run-time))
	(*skovar-counter* nil)
	(*skofun-counter* nil)
	(*bind-counter* nil)
	(*recursive-prove-decl-call* *in-checker*)
	(*in-checker* t)
	(*displaying-proof* nil)
	(*current-displayed* nil)
	(*flush-displayed* nil)
	;;(*auto-rewrites-names* nil)
	(*auto-rewrites-off* nil)
	(*subtype-names* nil)
	(*named-exprs* nil)
	(*rec-type-dummies* nil)
	(*assert-typepreds* nil)
	(*pvs-bdd-hash* nil)
	(*bdd-pvs-hash* nil)
	(*bdd-counter* *bdd-counter*)
	(*track-rewrites* nil)
	(*context-modified* nil)
	(*generate-tccs* 'NONE)
	(*rewrite-msg-off* *rewrite-msg-off*)
	(*ruletrace* NIL)
	(*ruletracedepth* 0)
	(*translate-id-counter* nil)
	;; Hash tables
	(*assert-if-arith-hash* (init-if-rec *assert-if-arith-hash*))
	(*auto-rewrites* (init-if-rec *auto-rewrites*))
	(*auto-rewrites-ops* (init-if-rec *auto-rewrites-ops*))
	(*beta-cache* (init-if-rec *beta-cache*))
	(*match-cache* (init-if-rec *match-cache*))
	(*subtype-of-hash* (init-if-rec *subtype-of-hash*))
	(*create-formulas-cache* (init-if-rec *create-formulas-cache*))
	(*translate-id-hash* (init-if-rec *translate-id-hash*))
	(*translate-to-prove-hash* (init-if-rec *translate-to-prove-hash*))
	(*all-subst-mod-params-caches* (copy-subst-mod-params-cache))
	(*pseudo-normalize-hash* #+allegro-v4.3
				 (copy *pseudo-normalize-hash*)
				 #-allegro-v4.3
				 (pvs-copyhash *pseudo-normalize-hash*))
	(*pseudo-normalize-translate-id-hash*
	 #+allegro-v4.3 (copy *pseudo-normalize-translate-id-hash*)
	 #-allegro-v4.3 (pvs-copyhash *pseudo-normalize-translate-id-hash*))
	;;
	(typealist primtypealist)
	(*local-typealist* *local-typealist*)
	(applysymlist nil)
	(sigalist sigalist)
	(usealist usealist)
	(findalist findalist)
	(auto-rewrites-info
	 (make-instance 'auto-rewrites-info
	   'rewrites (init-symbol-table)
	   'auto-rewrites-names nil
	   'auto-rewrites!-names nil
	   'macro-names nil))
	(*current-context* (context decl)))
    (initprover)			;initialize prover
    (newcounter  *skovar-counter*)
    (newcounter  *skofun-counter*)
    (newcounter  *bind-counter*)
    (newcounter *translate-id-counter*)
    (unless (closed-definition decl)
      (setf (closed-definition decl)
	    (universal-closure (definition decl))))
    (let* ((top-formula (closed-definition decl))
	   (s-form (make-instance 's-formula 'formula top-formula))
	   (sequent (make-instance 'sequent 's-forms (list s-form)))
	   (*top-proofstate*
	    (make-instance 'top-proofstate
	      'current-goal sequent
	      'label (string (id decl))
	      'strategy (if strategy
			    strategy
			    (query*-step))
	      'context *current-context*
	      'alists (make-dpinfo sigalist findalist usealist)
	      'justification (justification decl)
	      'declaration decl
	      'current-auto-rewrites auto-rewrites-info)))
      (before-prove*)
      (unwind-protect
	  (catch 'quit			;to quit proofs
	    (if *please-interrupt*
		(prove* *top-proofstate*)
		(with-interrupts-deferred
		 (prove* *top-proofstate*))))
	(after-prove*)
	(unless *recursive-prove-decl-call*
	  (save-proof-info decl init-real-time init-run-time)))
      *top-proofstate*)))

(defmethod prove-decl ((decl declaration) &key strategy)
  (format-if "~%Couldn't find formula ~a in module ~a."
	     (id decl) (id *current-theory*)))

(defun before-prove* ()
  (when *start-proof-display*
    (let ((*ps* *top-proofstate*))
      (call-x-show-proof)))
  (pvs-emacs-eval "(setq pvs-in-checker t)"))

(defun after-prove* ()
  (pvs-emacs-eval "(setq pvs-in-checker nil)")
  (display-proofstate nil)
  (when *subgoals*
    (setq *subgoals*
	  (mapcar #'current-goal
	    (collect-all-remaining-subgoals *top-proofstate*))))
  (unless *recursive-prove-decl-call*
    (clear-proof-hashes)))

(defun clear-proof-hashes ()
  (clrhash *assert-if-arith-hash*)
  (clrhash *auto-rewrites*)
  (clrhash *auto-rewrites-ops*)
  (clrhash *beta-cache*)
  (clrhash *match-cache*)
  (clrhash *subtype-of-hash*)
  #+allegro-v4.3 (clrhash *create-formulas-cache*)
  #-allegro-v4.3 (pvs-clrhash *create-formulas-cache*)
  #+allegro-v4.3 (clrhash *translate-id-hash*)
  #-allegro-v4.3 (pvs-clrhash *translate-id-hash*)
  #+allegro-v4.3 (clrhash *translate-to-prove-hash*)
  #-allegro-v4.3 (pvs-clrhash *translate-to-prove-hash*))

(defun make-dpinfo (sigalist findalist usealist)
  (make-instance 'dpinfo
    'dpinfo-sigalist sigalist
    'dpinfo-findalist findalist
    'dpinfo-usealist usealist))

(defun save-proof-info (decl init-real-time init-run-time)
  (setf (proof-time decl)
	(list (- (get-internal-run-time) init-run-time)
	      (- (get-internal-real-time) init-real-time)
	      nil))
  (cond ((eq (status-flag *top-proofstate*) '!)
	 ;;(not *proving-tcc*)
	 (setf (justification2 decl)(justification decl)
	       (justification decl)
	       (justification *top-proofstate*))
	 (when *context-modified*
	   (setf (proof-status decl) 'unfinished)
	   (let ((ans (pvs-yes-or-no-p
		       "~%Context was modified in mid-proof.
Would you like to rerun the proof?~%")))
	     (if ans (prove-decl decl
				 :strategy
				 '(then (rerun)
					(query*)))))))
	((not (eq (status-flag *top-proofstate*) `X))
	 (let ((ans (or (null (justification decl))
			(and (not *proving-tcc*)
			     (pvs-yes-or-no-p
			      "~%Would you like the partial proof to be saved? ~%(***Old proof will be overwritten.***)~%")))))
	   (when ans
	     (when (justification decl)
	       (format t "~%Use M-x revert-proof to revert to previous proof."))
	     (setf (justification2 decl)(justification decl)
		   (justification decl)
		   (collect-justification *top-proofstate*)
		   (proof-status decl) 'unfinished)
	     )))))

(defun rerun-prove (decl)
  (if (and *noninteractive* *pvs-verbose*)
      (let ((*suppress-printing* nil)
	    (*proving-tcc* t))
	(prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))
      (let ((*suppress-printing* T)
	    (*printproofstate* nil)
	    (*proving-tcc* T))
	(prove-decl decl :strategy `(then (then (rerun) (postpone)) (quit))))))

(defun recheck-prove (decl)
    (let ((*suppress-printing* T)
	  (*printproofstate* nil)
	  (*proving-tcc* T)
	  (justif (justification decl))
	  )
      (cond ((typep justif 'justification)
	     (prove-decl decl
			 :strategy
			 `(then (then (rerun :recheck? T)
				 (postpone))
			   (quit))))
	    (t (let ((*proving-tcc* 'TCC));;to save new justification.
		 (rerun-prove decl))
	       (prove-decl decl
			 :strategy
			 `(then (then (rerun :recheck? T)
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
		  ;;(let ((*to-emacs* T))
		  ;;      (pvs-buffer "*goal*"
		  ;;		      proofstate
		  ;;		      T T))
		  ;;  (break)
		  (print-proofstate-if proofstate)
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
				 (format-if "Subgoal completed")
				 (setf (status-flag proofstate) '*)
				 proofstate)
				(t (prove*-int nextstate))))
			 (t (prove*-int nextstate))))))
      (prove*-int proofstate))))

;;12/16: need to fix prove* to save the proof, etc.

(defun qread (prompt)
  (format t "~%~a"  prompt)
  (force-output)
  (let ((input (ignore-errors (read))))
    (cond ((member input '(quit q exit (quit)(exit)(q))
		   :test #'equal)
	   (if (pvs-y-or-n-p "~%Do you really want to quit?  ")
	       (throw 'quit nil)
	       (qread prompt)))
	  ((eq input 'abort)
	   (if (pvs-y-or-n-p "~%Do you really want to abort?  ")
	       (throw 'abort nil)
	       (qread prompt)))
	  (t (auto-save-proof)
	     input))))

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
			(let ((strategy
			       (strat-eval*
				(if (strategy proofstate)
				    (strategy proofstate)
				    ;;(if *proving-tcc*
				    ;;  '(quit))
				    '(postpone T));;if no strat, move on.
				proofstate)))
			  (setf (strategy proofstate) strategy)
			  (rule-apply strategy proofstate))))))))
		;;rule-apply returns a
		;;revised proofstate.
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
			      (when *rerunning-proof*  ;;print mini-buffer msg
				(setq *rerunning-proof* (format nil "~a." *rerunning-proof*))
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
;		   (setf (status-flag newps) NIL;;make it fresh
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
	(if quiet-flag pp (format-if pp))))))


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

;(defun formals-part (def)
;  (cadr def))
;
;(defun defn-part (def)
;  (caddr def))

(defun new-formula-nums (sforms par-sforms &optional (pos 1) (neg -1))
  (when sforms
    (let* ((sign (not (not-expr? (formula (car sforms)))))
	   (result
	    (if sign
		(new-formula-nums (cdr sforms) par-sforms (1+ pos) neg)
		(new-formula-nums (cdr sforms) par-sforms pos (1- neg)))))
      (if (memq (car sforms) par-sforms)
	  result
	  (if sign (cons pos result)
	      (cons neg result))))))
	  
(defun strat-eval* (strat ps)
  (let* ((*ps* ps)
	 (*par-ps* (if (parent-proofstate ps)
		       (parent-proofstate ps)
		       *in-apply*))
	 (* '*)
	 (*goal* (current-goal ps))
	 (*label* (label (current-goal ps)))
					;	 (*subgoalnum* (subgoalnum ps))
	 (*+* (mapcar #'formula (pos-s-forms (s-forms (current-goal ps)))))
	 (*-* (mapcar #'(lambda (x) (args1 (formula x)))
		      (neg-s-forms (s-forms (current-goal ps)))))
	 (*par-label* (when *par-ps* (label *par-ps*)))
	 ;(label (parent-proofstate ps))
	 (*par-goal* (when *par-ps*
		       (current-goal *par-ps*)))
	 (*new-fmla-nums* (new-formula-nums (s-forms *goal*)
					    (when *par-goal* (s-forms *par-goal*))))
	 (*current-context* *current-context*)
	 (*module-context* (context *current-theory*))
	 (*current-theory* *current-theory*))
    (strat-eval strat)))

(defun step-or-rule-defn (name)
  (or (step-definition name)(rule-definition name)))


(defun strat-eval (strat)
  (cond ((typep strat 'strategy) strat)
	((null strat) (get-rule '(skip) *ps*))
	((not (consp strat))
	 (format-if "~%Ill-formed rule/strategy: ~s " strat)
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
	 ;;if this evaluates to a rule, then the input is noted.
	 (let ((result
		(strat-eval (cadr strat))))
	   (when (typep result 'rule-instance)
	     (setf (rule-input result) strat))
	   result))
	((rule-definition (car strat))
	 (let* ((def (rule-definition (car strat)))
		(subalist (pair-formals-args (formals def)
					       (cdr strat)))
		(args (loop for x in (formals def)
			    when (not (memq x '(&optional &rest)))
			    collect (cdr (assoc x subalist))))
		(def-expr  (subst-stratexpr
			    (defn def)
			    subalist
			    (reverse subalist)))
		(new-def-expr;;2/91:so that rules are APPLYed.
		 `(apply ,def-expr))
		(result (strat-eval new-def-expr)))
	   (setf (rule-input result)
		 strat
		 (rule-format result)
		 (when (format-string (rule-definition (car strat)))
		   (cons (format-string (rule-definition (car strat))) args)
;		   #'(lambda ()
;			  (apply #'format nil
;				 (format-string (rule-definition (car strat)))
;				 args))
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
	 (get-rule strat
		   *ps*))
	(t (format-if "~%Ill-formed rule/strategy: ~s " strat)
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
	 (pair-formals-args (cdr formals) args T))
	((null args) (if opt-flag
			 (collect-default-values formals)
			 (progn (format-if "~%Not enough actuals.")
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
     (*+* (mapcar #'formula (pos-s-forms (current-goal ps))))
     (*-* (mapcar #'formula (neg-s-forms (current-goal ps))))
     (*par-label* (when (parent-proofstate ps)
		    (label (parent-proofstate ps))))
     (*par-goal* (when (parent-proofstate ps)
		    (current-goal (parent-proofstate ps))))
     (*current-context* *current-context*)
     (*module-context* (context *current-theory*))
     (*current-theory* *current-theory*)))

(defun isfun? (sym)
  (and (symbolp sym)
       (or (fboundp sym)
	   (special-form-p sym))))

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

(defun groovy-cakes (expr)
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

;(defun expr-eval (expr ps &optional alist)
;  (cond ((null expr) nil)
;	((global? expr) (global-value expr ps))
;	((typep expr 'symbol)
;	 (let ((preval (assoc expr alist)))
;	   (if (null preval) nil (cdr preval))
;	   )))
;	((quote? expr) (cadr expr))
;	((if-form? expr)
;	 (if (expr-eval (cadr expr) ps alist)
;	     (expr-eval (caddr expr) ps alist)
;	     (expr-eval (cadddr expr) ps alist)))
;	((let-form? expr)
;	 (expr-eval (body expr) ps
;		     (loop for x in (let-bindings expr)
;			   collect (cons (car x)
;					 (expr-eval (cadr x) ps alist)))))
;	((and (eq (car expr 'equal)
;		  (> (length expr) 2)))
;	     (tc-eq (expr-eval (cadr expr) ps alist)
;		    (expr-eval (caddr expr) ps alist)))
;	((expr-primitive? (car expr))
;	 (eval `(,(car expr) @,(loop for x in (cdr expr)
;				collect (expr-eval x ps alist)))))
;	(t (let ((def (expr-definition (car expr))))
;	     (if (null def) (break "in-expr-eval")
;		 (expr-eval (definition def) ps
;			    (pair-formals-args (formals def)
;					       (loop for x in (cdr expr)
;						     collect (expr-eval
;							      x ps alist))))))))


;(defun expr-special (expr)
;  (and (not (null expr))
;       (memq (car expr) '(equal parse unparse typecheck))))
;
;(defun special-eval (expr ps alist)
;  (case (car expr)
;    (equal (tc-eq (expr-eval (cadr expr) ps alist)
;		  (expr-eval (caddr expr) ps alist)))
;    (parse (parse :string (expr-eval (cadr expr) ps alist)
;		  :nt (expr-eval (caddr expr) ps alist)))
;    (unparse (unparse (expr-eval (cadr expr) ps alist)))
;    (typecheck (typecheck (expr-eval (cadr expr) ps alist)
;			  :expected (expr-eval (caddr expr) ps alist)
;			  :context  (expr-eval (cadddr expr) ps alist)
;			  :kind  (expr-eval (caddddr expr) ps alist)))))
;
;(defun expr-primitive? (symb)
;  (memq symb '(null car cdr cons equal and or not + -
;	       symbolp stringp typep
;	       variable? constant? application? binding-expr?
;	       forall-expr? exists-expr? lambda-expr?
;	       record-expr? tuple-expr? neg-expr?
;	       disjunction? conjunction? implication? iff?
;	       make-application make-bind-expr make-record-expr
;	       make-tuple-expr funtype? tuple-type? record-type?
;	       make-funtype make-tuple-type make-record-type
;	       subst match find-match)))


(defun label-suffix (label)
  (let ((pos (position #\. label :from-end T)))
    (if pos
	(let* ((suffix (subseq label (1+ pos)))
	       (check (every #'digit-char-p suffix)))
	  (if check suffix ""))
	"")))
	

(defun success-step (proofstate)
  (wish-done-proof proofstate)
  (setf (status-flag proofstate) '!
	(done-subgoals proofstate)
	(sort (done-subgoals proofstate)
	      #'mystring<= :key #'label)
	(current-subgoal proofstate) nil
	(justification proofstate)
	(cond ((current-rule proofstate)

	       (make-instance 'justification
		 'label (label-suffix (label proofstate))
		 'rule  (sexp-unparse (current-rule proofstate))
		 'xrule (current-xrule proofstate)
		 'subgoals
		 (mapcar #'(lambda (x) (justification x))
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

(defvar *report-mode* nil)
(defvar *print-ancestor* nil)

(defvar *print-descendants* nil)

(defun non-strat-subgoal-proofstate (ps)
  (if (or (not (singleton? (done-subgoals ps)))
	  (not (strat-proofstate? (car (done-subgoals ps)))))
      ps
      (non-strat-subgoal-proofstate (car (done-subgoals ps)))))

(defun report-proof (proofstate)
  (let ((*report-mode* T)
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
  (let*	((done-subgoals (done-subgoals proofstate))
	 (all-subgoals (all-subgoals-sorted proofstate)))
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
      (format t "~VT~a" *prover-indent* (format-printout proofstate T)))
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
			   'rule '(postpone)))))))
	((eq (status-flag proofstate) '!)
	 (make-instance 'justification
	   'label (label-suffix (label proofstate))
	   'rule (sexp-unparse (current-rule proofstate))
	   'xrule (current-xrule proofstate)
	   'subgoals
	   (sort 
	    (mapcar #'collect-justification
		    (done-subgoals proofstate))
	    #'<
	    :key #'(lambda (x)(safe-parse-integer (label x))))))
	((memq (status-flag proofstate) '(? *))
	 (make-instance 'justification
	   'label (label-suffix (label proofstate))
	   'rule (sexp-unparse (current-rule proofstate))
	   'xrule (current-xrule proofstate)
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
			   'rule '(postpone)))))



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

(defmethod collect-dependents ((x T)  accum)
  accum)

(defun collect-dependent-decls (proofstate)
  (collect-dependents proofstate nil))

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
(defmethod topstep (x) nil)
(defmethod subgoal-strategy (x) nil)
(defmethod failure-strategy (x) nil)

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
;    (out-context (setf (out-context ps) value))
;    (out-substitution (setf (out-substitution ps) value))
    (alists (setf (alists ps) value))
    (current-auto-rewrites (setf (current-auto-rewrites ps) value))
    (rewrite-hash (setf (rewrite-hash ps) value))
    (subtype-hash (setf (subtype-hash ps) value))
    (dependent-decls (setf (dependent-decls ps) ;;NSH(4.21.95):special
			   (union value (dependent-decls ps))))
    (current-xrule (setf (current-xrule ps) value))))

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
		    (make-instance 'rule-instance
			   'rule (apply (rule-function entry)
					args)
			   'rule-input rule
			   'rule-format (when (format-string entry)
					  (cons (format-string entry)
						args)
;					  #'(lambda ()
;					    (apply #'format nil
;						  (format-string entry)
;						  args))
			   )))))))

(defun extract-optionals (optionals match)
  (cond ((null optionals) NIL)
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
  (let* ((alists (alists ps))
	 (sigalist (dpinfo-sigalist alists))
	 (findalist (dpinfo-findalist alists))
	 (usealist (dpinfo-usealist alists))
	 ;;(typealist (typealist alists)) ;;NSH(2.16.94) global to proof.
	 (*rewrite-hash* (copy (rewrite-hash ps)))
	 (*subtype-hash* (copy (subtype-hash ps))))
    (assert-tccforms* tccforms ps)))

(defun assert-tccforms* (tccforms ps)
  (if (null tccforms) nil
      (multiple-value-bind (sig value)
	      (assert-if (tccinfo-formula (car tccforms)))
	(cond ((tc-eq value *true*)
	       (assert-tccforms* (cdr tccforms) ps))
	      ((eq sig 'X) (cons (car tccforms)
				 (assert-tccforms*
				  (cdr tccforms) ps)))
	      (t (setf (tccinfo-formula (car tccforms)) value)
		 (cons (car tccforms)
		       (assert-tccforms* (cdr tccforms) ps)))))))

(defun rule-apply (step ps)
  (let* ((*ps* ps)
	 (* '*)
	 (*goal* (current-goal ps))
	 (*label* (label  ps))
					;	 (*subgoalnum* (subgoalnum ps))
	 (*+* (mapcar #'formula (pos-s-forms (s-forms (current-goal ps)))))
	 (*-* (mapcar #'formula (neg-s-forms (s-forms (current-goal ps)))))
	 (*par-label* (when (parent-proofstate ps)
			(label (parent-proofstate ps))))
	 (*par-goal* (when (parent-proofstate ps)
		       (current-goal (parent-proofstate ps))))
	 (*current-context* *current-context*)
	 (*module-context* (context *current-theory*))
	 (*current-theory* *current-theory*)
	 (*auto-rewrites* (rewrites ps))
	 (*all-rewrites-names* (all-rewrites-names ps))
	 (*auto-rewrites-names* (auto-rewrites-names ps))
	 (*auto-rewrites!-names* (auto-rewrites!-names ps))
	 (*macro-names* (macro-names ps))	 
	 (*rewrite-hash* (rewrite-hash ps)))
    ;;(break)
    (cond ((typep step 'rule-instance);;if step is a rule, then
	   ;;reinvoke rule-apply with corresponding strategy. 
	   (rule-apply (make-instance 'strategy
			 'topstep step)
		       ps));;else step is a strategy
	  ((typep (topstep step) 'rule-instance)
	  (let* ((*tccforms* NIL)
		 ;;(*generate-tccs* NIL) ;;NSH(10.20.94)
		 (topstep (topstep step))
		 (name (if (consp (rule-input topstep))
			   (car (rule-input topstep))
			   topstep)))
	    (when (memq name *ruletrace*)
	      (format t "~%~vTEnter: ~a" *ruletracedepth*
		      (rule-input topstep))
	      (incf *ruletracedepth*))
	    (multiple-value-bind (signal subgoals
					 updates)
		(funcall (rule topstep) ps) ;;(break "rule-ap")
	      (cond ((eq signal '!);;success
		     (when (memq name *ruletrace*)
		       (decf *ruletracedepth*)
		       (format t "~%~vT Exit: ~a -- Proved subgoal"
			   *ruletracedepth* name ))
		     (setf (status-flag ps) '!      
			   (current-rule ps)(rule-input topstep)
			   (printout ps)
			   (rule-format topstep)
			   (justification ps)
			   (make-instance 'justification
			     'label (label-suffix (label ps))
			     'rule (rule-input topstep)))
		     (make-updates updates ps)
		     ps)
		    ((eq signal '?);;subgoals generated
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
			    (tccforms (assert-tccforms *tccforms*
						       ps))
			    (tcc-subgoals
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
		       ;;cleaning up (NSH(7.27.94)
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
			     (setf (gethash (tccinfo-formula tcc) new-tcc-hash)
				   T))
		       (assert
			(every #'(lambda (sps)
				   (every #'(lambda (sfmla)
					      (null (freevars (formula sfmla))))
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
			     (printout ps)
			     (rule-format topstep)
			     (remaining-subgoals ps)
			     subgoal-proofstates)
		       (unless (typep ps 'top-proofstate)
			 (setf (strategy ps) nil))
		       (make-updates updates ps)
		       ps))
		    ((eq signal 'X)
		     (when (memq name *ruletrace*)
		       (decf *ruletracedepth*)
		       (format t "~%~vT Exit: ~a -- No change."
			   *ruletracedepth* name))
		     (format-if "~%No change on: ~s" (rule-input topstep))
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
		    (t  (undo-proof signal ps)))
	      )))
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
  (let* ((alists (alists ps))
	 (sigalist (dpinfo-sigalist alists))
	 (findalist (dpinfo-findalist alists))
	 (usealist (dpinfo-usealist alists))
	 ;;(typealist (typealist alists)) ;;NSH(2.16.94) global over proof.
	 (*rewrite-hash* (copy (rewrite-hash ps)))
	 (*subtype-hash* (copy (subtype-hash ps))))
    (loop for fmla in fmla-list
	  nconc
	  (multiple-value-bind (sig value)
	      (assert-if fmla)
	    (cond ((tc-eq value *true*) nil)
		  ((eq sig 'X) (list fmla))
		  (t (list value)))))))
		   

(defun undo-proof (info ps)
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
		      (setf (justification newps)
			    (collect-justification newps)
			    (status-flag newps) NIL
			    (remaining-subgoals newps) NIL
			    (pending-subgoals newps) NIL
			    (done-subgoals newps) NIL
			    (current-subgoal newps) NIL
			    ;(dependent-decls newps) NIL ;;NSH(12.14.94)
			    (current-rule newps) NIL
			    (current-xrule newps) NIL
			    (printout newps) NIL
			    (strategy newps) (query*-step))
		      newps)
		     (t (setf (strategy ps) (query*-step))
			ps)))))))

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
;;current proofstates.  Returns NIL, if target isn't found.
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
(defun remove-dble-negation (expr)
  (if (negation? expr)
      (if (negation? (args1 expr))
	  (remove-dble-negation (args1 (args1 expr)))
	expr)
    expr))
  
(defun clean-goal (goal)
  (let ((new-s-forms (loop for sf in (s-forms goal)
			   when (not (or (and (not-expr? (formula sf))
					      (tc-eq (args1 (formula sf))
						     *true*))
					 (tc-eq (formula sf) *false*)))
			   collect
			   (lcopy sf 'formula
				  (remove-dble-negation (formula sf))))))
    (if (every #'eq (s-forms goal) new-s-forms)
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
  (let* ((allsubgoals (append subgoals tcc-subgoals))
	 (numsubgoals (length allsubgoals))
	 (colwidth (length (format nil "~a" numsubgoals))))
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
		    'context (copy (context proofstate))
		    'strategy  strategy
		    'label
		    (if (= (length allsubgoals) 1)
			(label proofstate)
			(format nil "~a.~a" (label proofstate)
				goalnum))
		    'subgoalnum (1- goalnum)
		    'dependent-decls (dependent-decls proofstate)
		    'alists (copy (alists proofstate))
		    'current-auto-rewrites
		    (current-auto-rewrites proofstate)
		    'rewrite-hash (rewrite-hash proofstate)
		    'subtype-hash (subtype-hash proofstate)
		    'parent-proofstate proofstate))
					;			(updated-goalstate
					;			 (make-updates updates goalstate))
		 )
;	    (when (and (tcc-proofstate? goalstate)
;		       (not (tcc-sequent? (current-goal goalstate))))
;	    		   (break "in make-sub"))
	    (if (consp goal)
		(make-updates (cdr goal)
			      goalstate)
		goalstate)))))
;;	  (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun help-proves ()
  (format t
"~%   This is Release -5.0 of SRI Computer Science Laboratory's
ProVeS specification/verification/proof checking system.
The system comes with no guarantees.  Currently, there is no
way to save your work.  Please direct complaints and suggestions
to shankar@csl.sri.com.~%
~a parses a file containing a module.
   The modules are in *pvs-modules*.  The premodules are in *premodules*.
~a typechecks the module.
~a sets *current-theory*.
~a invokes the proof checker on a formula in
    the current module.
The rules in *rulebase* are: ~%"
  '(pvs-parse ":file" "<file-name>")
  '(tc (get-module "<module-name>"))
  '(set-current-module "<module-name>")
  '(prove "<formula-name>"))
  (funcall (help-rule-fun '*) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-current-module (modname)
  (let ((module (get-module modname)))
    (if (null module)
	(format t "~%No such module as ~a." modname)
	(setq *current-theory* module))
    *current-theory*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   
;;How 'bout some rules now?


;(defun help-rule (rule-name)
;  (make-instance 'rule
;		 'rule-part (help-rule-fun rule-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun change-strategy-rulefun (new-strategy)
;  #'(lambda (ps) (values 'X nil 
;			 (list 'strategy (funcall (symbol-function
;						   new-strategy) ps)))))
;
;;(defun change-strategy-rule (new-strategy)
;;  (make-instance 'rule
;;    'rule-part (change-strategy-rulefun new-strategy)))
;
;(addrule 'change-strategy (new-strategy) nil
;	 (change-strategy-rulefun new-strategy)
;	 "(change-strategy <new-strategy>):  Changes the current strategy.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun suppress ()
;  #'(lambda (ps)
;      (declare (ignore ps))
;      (setq *suppress-printing* t)
;      (values 'X nil nil)))
;
;(addrule 'suppress nil nil (suppress)
;	 "(suppress):  stops printing of proof output.")
	       





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
  (parse :string input :nt nt))

(defmethod pc-parse (input nt)
  (parse :string (format nil "~a" input) :nt nt))

(defmethod pc-parse ((input integer) nt)
  (parse :string (format nil "~a" input) :nt nt))

(defmethod pc-parse ((input syntax) nt)
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
	    :tccs 'NONE;;NSH(10.20.94) tccs generated
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
	   (format-if "~%Rerunning step: ~s" (format-rule top-rule-in))
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
	(extract-justification-sexp (subgoals justification))))

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
				     :string T)
	       (unparse (raise-actuals
			 (or ;;(type-value (type form))
			     (and (slot-exists-p form 'declared-type)
				  (declared-type form))
			     (type form)))
			:string T)))
	(t (unparse form :string T))))

(defmethod extract-justification-sexp ((list list))
  (cond ((null list) nil)
	(t (cons (extract-justification-sexp (car list))
		 (extract-justification-sexp (cdr list))))))

(defmethod extract-justification-sexp (x)
  x)

(defun editable-justification (justif &optional label xflag)
  (unless (null justif)
    (cond ((equal (label justif) label)
	   (cons (if (and xflag (xrule justif))
		     (format-rule (xrule justif))
		     (format-rule (rule justif)))
		 (editable-justification* (subgoals justif)
					  label xflag)))
	  (t (cons (label justif)
		   (cons (if (and xflag (xrule justif))
			     (format-rule (xrule justif))
			     (format-rule (rule justif)))
			 (editable-justification* (subgoals justif)
						  (label justif)
						  xflag)))))))

(defun editable-justification* (justifs &optional label xflag)
  (unless (null justifs)
    (if (singleton? justifs)
	(editable-justification (car justifs) label xflag)
	(list (loop for justif in justifs collect
	      (editable-justification justif nil xflag))))))

(defun check-edited-justification (ejustif &optional label)
  (if (null ejustif) nil
      (if (null label)
	  (if (and (stringp (car ejustif))
		   (every #'digit-char-p (car ejustif)))
	      (check-edited-justification (cdr ejustif)
					  (car ejustif))
	      (values "Label must be a string of numbers." (car ejustif)))
	  (if (and (consp (car ejustif))
		   (consp (caar ejustif)))
	      (some #'check-edited-justification (car ejustif))
	      (if (consp (car ejustif))
		  (check-edited-justification (cdr ejustif)
					      label)
		  (values "A rule application must be a list."
			  (car ejustif)))))))


(defun revert-justification (ejustif &optional label)
  (unless (null ejustif)
    (if  (null label)
	 (list (car ejustif)
	       (unformat-rule (cadr ejustif))
	       (revert-justification (cddr ejustif)(car ejustif)))
	 (if (consp (caar ejustif))
	     (mapcar #'revert-justification (car ejustif))
	     (list (list label
			 (unformat-rule (car ejustif))
			 (revert-justification (cdr ejustif) label)))))))
  

(defmethod pp-justification ((justification justification))
  (pp-justification* (extract-justification-sexp justification) nil))

(defmethod pp-justification (justification)
  (pp-justification* justification nil))


(defun pp-justification* (justification label)
  (cond ((null justification)
	 nil)
	(t (cond ((equal (car justification) label)
		  (format T "~%")
		  (format T "~V@T" (+ 3 (length (string label)))))
		 (t (format T "~%~a : " (car justification))))
	   (write (format-rule (cadr justification)) :pretty t)
	   (loop for entry in (reverse (caddr justification))
		 do (pp-justification* entry (car justification))))))

(defun format-rule (sexp)
  (cond ((null sexp) nil)
	((not (consp sexp)) sexp)
	((eq (car sexp) 'TYPECHECKED)
	 (list (cadr sexp) '!TYPE (caddr sexp)))
	(t (cons (format-rule (car sexp))
		 (format-rule (cdr sexp))))))

(defun unformat-rule (sexp)
  (when sexp
    (cond ((not (consp sexp)) sexp)
	  ((and (consp (cdr sexp))
		(consp (cddr sexp))
		(stringp (car sexp))
		(memq (cadr sexp) '(:TYPE !TYPE))
		(null (cdddr sexp)))
	   (list 'TYPECHECKED (car sexp)(caddr sexp)))
	  (t (cons (unformat-rule (car sexp))
		   (unformat-rule (cdr sexp)))))))
  

(defmethod label ((list list)) (car list))
(defmethod rule ((list list)) (cadr list))
(defmethod subgoals ((list list)) (caddr list))

;;catchall methods for label, rule, subgoals
(defmethod label ((x T)) "9999")
(defmethod rule ((x T)) '(skip))
(defmethod subgoals ((x T)) NIL)

		 
(defun collect-module-proofs (module)
  (loop for decl in (theory module)
	when (and (typep decl 'formula-decl)
		  (justification decl))
	 collect
	 (cons (id decl) (extract-justification-sexp
			  (justification decl)))))

(defun collect-proofs (modnames)
  (let ((proofs NIL))
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
	    (write x  :length nil :level nil :escape T :stream output)))
    ;(format t "~%Saved proofs in ~a" filestring)
    ))

(defun load-proofs (filestring)
  (with-open-file (input filestring :direction :input)
    (load-module-proofs input filestring)))

(defun load-module-proofs (input filestring)
  (let ((modproofs (read input NIL NIL)))
    (cond ((null modproofs) ;(format t "~%Read proofs from ~a"
				;	    filestring)
	   NIL)
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
			 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defstep 'split$ nil
;  '(let ((index (find-and-form (s-forms *goal*))))
;    (if (null index) (skip) (split index))))
;(addrulefun 'split$ nil nil (split$-rulefun)
;	    "(split$), split$: Searches for a conjunction and splits into subgoals.")
;
;(defun split$-rulefun ()
;  #'(lambda (ps)
;      (let ((index (find-and-form
;		    (s-forms  (current-goal ps)))))
;	(cond ((null index)
;	       `(skip))
;	      (t `(split ,index))))))
;
;					;(defun split$-rule ()
;					;  (make-instance 'rule
;					;    'rule-part
;					;    #'split$-rule-part
;					;    'rule-input '(split$)))

(defun find-and-form (s-forms)
  (let ((ans (find-pos-and-form s-forms 1)))
    (if ans ans
	(find-neg-and-form s-forms -1))))

(defun find-pos-and-form (s-forms index)
  (if (consp s-forms)
      (if  (not (not-expr? (formula (car s-forms))))
	   (if (and+form? (formula (car s-forms)))
	       index
	       (find-pos-and-form (cdr s-forms) (1+ index)))
	   (find-pos-and-form (cdr s-forms) index))
      NIL))

(defun find-neg-and-form (s-forms index)
  (if (consp s-forms)
      (if  (not-expr? (formula (car s-forms)))
	   (if (and+form? (formula (car s-forms)))
	       index
	       (find-neg-and-form (cdr s-forms) (1- index)))
	   (find-neg-and-form (cdr s-forms) index))
      NIL))
	  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun opt-default (optional)
;  (if (and (consp optional)(consp (cdr optional)))
;    (cadr optional)
;    nil))
;
;(defun opt-name (optional)
;  (if  (consp optional)
;    (car optional)
;    optional))

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
		       ((ifff? (args1 disjunct))
			(or+ (list (negate (make-implication (car args2)
							     (cadr args2)))
				   (negate (make-implication
					    (cadr args2)
					    (car args2))))))
		       (t (list disjunct))))
		   (if (and (typep (car (arguments disjunct)) 'name-expr)
			    (equal (args1 disjunct) *true*))
		       NIL
		       (list disjunct))))
	      (t (list disjunct)))
	    (if (and (typep disjunct 'name-expr)
		     (equal  disjunct *false*))
		NIL
		(list disjunct)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cases? (expr) (typep expr 'cases-expr))

;;;and+: takes a single formula and splits it into a list of conjuncts.
;;;EG: (and+ "NOT (A AND B IMPLIES C) AND NOT (E OR F)") is
;;;(NOT C #<Name-Expr A> #<Name-Expr B> NOT E NOT F).
(defun and+ (form)
  (if (and (typep form 'application)
	   (typep (operator form) 'name-expr))
      (let ((args (arguments form)))
	(cond  
	  ((conjunction? form)
	   (loop for conjunct in args
		 append (and+ conjunct)))
	  ((ifff? form)
	   (list (make-implication (car args)(cadr args))
		 (make-implication (cadr args)(car args))))
	  ((negation? form)
	   (if (and (typep (car args) 'application)
		    (typep (operator (car args)) 'name-expr))
	       (let ((args2 (arguments (car args))))
		 (cond  
		   ((implication? (car args))
		    (append (and+ (negate
				   (cadr args2)))
			    (and+ (car args2))))
		   ((disjunction? (car args))
		    (append (and+ (negate
				   (car args2)))
			    (and+ (negate
				   (cadr args2)))))
		   ((negation? (car args))
		    (and+ (car args2)))
		   ((cases? (car args))
		    (and+ (negate (translate-cases-to-if (car args)))))
		   ((branch? (car args))
		   (list (negate (make-conjunction
				  (list (condition (car args))
					(then-part (car args)))))
			 (negate
			  (make-conjunction
			   (list (negate (condition (car args)))
				 (else-part (car args)))))))
		   (t (list form))))
	       (list form)))
	  ((cases? form)
	   (and+ (tranlate-cases-to-if form)))
	  ((branch? form)
	   (list (make-implication (condition form)(then-part form))
		 (make-implication (negate (condition form))(else-part form))))
	  (t (list form))))
  (list form)))

					;(defun split-rule (sformnum)
					;  (make-instance 'rule
					;	   'rule-part (split-rule-fun sformnum)
					;	   'rule-input `(split ,sformnum)))


(defun and+form? (exp)
  (or (branch?  exp)
      (and (typep exp 'application)
	   (typep (operator exp) 'name-expr)
	   (or (conjunction? exp)(ifff? exp)
	       (and (negation? exp)
		    (let ((arg (car (arguments exp))))
		      (or (branch? arg)
			  (and (typep arg  'application)
			       (typep (operator arg) 'name-expr)
			       (or (negation? arg)
				   (disjunction? arg)
				   (implication? arg))))))))))

(defun split-rule-fun (sformnum &optional labels)
  #'(lambda (ps) (split-step sformnum ps labels)))

(defun split-step (sformnum ps labels)
  (let* ((goalsequent (current-goal ps))
	 (sformnum (find-sform (s-forms goalsequent) sformnum
			       #'(lambda (sf)(and+form? (formula sf)))))
	 (selected-sform (select-seq (s-forms goalsequent) (list sformnum))))
    ;;    (format t "~%conjunct = ~a" (formula selected-sform))
    (if (or (null selected-sform)
	    (not (and+form? (formula (car selected-sform)))))
	(values 'X nil nil)
	(let* ((sel-sform (car selected-sform))
	       (new-sforms (delete-seq (s-forms goalsequent) (list sformnum)))
	       (conjuncts (and+ (formula sel-sform)))
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

(defun select-seq1 (seq nums pos neg)
       (if (consp seq)
	   (if (not-expr? (formula (car seq)))
	       (if (memq neg nums)
		   (cons (car seq)
			 (select-seq1 (cdr seq) nums pos (1- neg)))
		   (select-seq1 (cdr seq) nums pos (1- neg)))
	       (if (memq pos nums)
		   (cons (car seq)
			 (select-seq1 (cdr seq) nums (1+ pos) neg))
		   (select-seq1 (cdr seq) nums (1+ pos) neg)))
	   nil))

(defun delete-seq1 (seq nums pos neg)
       (if (consp seq)
	   (if (not-expr? (formula (car seq)))
	       (if (memq neg nums)
		   (delete-seq1 (cdr seq) nums pos (1- neg))
		   (cons (car seq)
			 (delete-seq1 (cdr seq) nums pos (1- neg))))
	       (if (memq pos nums)
		   (delete-seq1 (cdr seq) nums (1+ pos) neg)
		   (cons (car seq)
			 (delete-seq1 (cdr seq) nums (1+ pos) neg))))
	   nil))

(defun select-seq (seq nums)
  (cond ((eq nums '*) seq)
	((eq nums '+) (loop for sform in seq
			    when (not (not-expr? (formula sform)))
			    collect sform))
	((eq nums '-) (loop for sform in seq
			    when (not-expr? (formula sform))
			    collect sform))
	((integerp nums)(select-seq1 seq (list nums) +1 -1))
	((consp nums)(select-seq1 seq nums +1 -1))
	(t NIL)))
(defun delete-seq (seq nums)
  (cond ((eq nums '*) NIL)
	((eq nums '-) (loop for sform in seq
			    when (not (not-expr? (formula sform)))
			    collect sform))
	((eq nums '+) (loop for sform in seq
			    when (not-expr? (formula sform))
			    collect sform))
	((integerp nums) (delete-seq1 seq (list nums) +1 -1))
	((consp nums) (delete-seq1 seq nums +1 -1))
	(t NIL)))

(defun gather-seq (seq yesnums nonums
		       &optional (pred #'(lambda (x) T))
		       (pos 1) (neg -1))
   (cond ((null seq) nil)
	 ((not-expr? (formula (car seq)))
	  (if (and (in-sformnums? (car seq) pos neg yesnums)
		   (not (in-sformnums? (car seq) pos neg nonums))
		   (funcall pred (car seq)))
	      (cons (car seq)
		    (gather-seq (cdr seq) yesnums nonums pred
				pos (1- neg)))
	      (gather-seq (cdr seq) yesnums nonums pred pos (1- neg))))
	 (t (if (and (in-sformnums? (car seq) pos neg yesnums)
		     (not (in-sformnums? (car seq) pos neg nonums))
		     (funcall pred (car seq)))
	      (cons (car seq)
		    (gather-seq (cdr seq) yesnums nonums
				pred (1+ pos) neg))
	      (gather-seq (cdr seq) yesnums nonums pred
			  (1+ pos)  neg)))))


					       
					  




(defmethod nth-arg ((expr application) num)
       (nth num (arguments expr)))

;;;(NSH: 3-20-91): redefined from makes.lisp
;commented out on 1/19/91 since Sam says this is already
;defined this way.
;(defmethod print-object ((expr expr) stream)
;  (if *debugging-print-object*
;      (call-next-method)
;      (progn (unparse expr :stream stream)
;	     (when (and (type expr)(not *in-checker*))
;	       (format stream "  :~a" (type expr))))))


(defmethod print-object ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*))))
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~%~a :  ~%~a"  (label ps) 
	      (current-goal ps)))))

(defmethod print-object ((ps tcc-proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*))))
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~%~a (TCC):   ~%~a"  (label ps)
	      (current-goal ps)))))

(defun pos-s-forms (s-forms)
  (loop for sf in s-forms when (not (not-expr? (formula sf))) collect
	sf))
(defun neg-s-forms (s-forms)
  (loop for sf in s-forms when  (not-expr? (formula sf)) collect
	sf))

(defmethod print-object ((sform s-formula) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~a" (formula sform))))


(defun seq-formula (sform)
  (let ((fmla (formula sform)))
    (if (not-expr? fmla)(args1 fmla)
	fmla)))
;;NSH(5.11.94): unparse-form caches the printout in the sform along with
;;the *default-char-width.
(defun unparse-sform (sform)
  (with-slots (printout) sform
  (cond ((and printout
	      (eql (car printout) *default-char-width*))
	 (cdr printout))
	(t (let ((string (unpindent (seq-formula sform)
				(+ 6 *prover-indent*)
				:string T)))
	     (setf (printout sform)
		   (cons *default-char-width* string))
	     string)))))

(defun in-every-print-descendant? (sform)
  (every #'(lambda (ps)
	     (memq sform
		   (s-forms (current-goal ps))))
	 *print-descendants*))

;;NSH(5.11.94): display-sform has been modified to use unparse-sform.
(defun display-sform (sform sfnum stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (cond (*report-mode*
	   (unless (memq sform par-sforms)
	     (format stream "~%~V@T" *prover-indent*)
	     (format stream "{~a}   ~a" sfnum
		     (unparse-sform sform))))
	  (t 
	   (format stream "~%~V@T" *prover-indent*)
	   (let ((old (memq sform par-sforms)))
	     (format stream "~a~a" (if old "[" "{") sfnum)
	     (when (label sform) (format stream ",~a"
				(label sform))) 
	     (format stream "~a   ~a" (if old "]" "}")
		     (unparse-sform sform)))))))

;(defun display-sform (sform sfnum stream)
;  (let ((par-sforms
;	 (when *print-ancestor*
;	   (s-forms (current-goal *print-ancestor*)))))
;    (cond (*report-mode*
;	   (unless (memq sform par-sforms)
;	     (format stream "~%~V@T" *prover-indent*)
;	     (format stream "{~a}   ~a" sfnum
;		     (unpindent (seq-formula sform)
;				(+ 6 *prover-indent*)
;				:string T))))
;	  (t 
;	   (format stream "~%~V@T" *prover-indent*)
;	   (let ((old (memq sform par-sforms)))
;	     (format stream "~a~a" (if old "[" "{") sfnum)
;	     (when (label sform) (format stream ",~a"
;				(label sform))) 
;	     (format stream "~a   ~a" (if old "]" "}")
;		     (unpindent (seq-formula sform)
;				(+ 6 *prover-indent*)
;				:string T)))))))

(defmethod print-object ((sequent sequent) stream);;ignoring printing
  ;;skovars for
  ;;now.[11/90]
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
  (cond (*debugging-print-object*
	 (call-next-method))
	(t (let ((neg-s-forms (neg-s-forms (s-forms sequent)))
		 (pos-s-forms (pos-s-forms (s-forms sequent))))
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
      (pvs-buffer "*Proof*"
	(with-output-to-string (*standard-output*)
	  (write (editable-justification
		  (collect-justification *top-proofstate*))
		 :stream *standard-output* :pretty t :escape t
		 :level nil :length nil))
	t t)
      (pvs-message "No proof is currently running")))

(defun call-show-hidden ()
  (if (and *in-checker* *ps*)
      (let ((hforms (hidden-s-forms (current-goal *ps*))))
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
      (pvs-buffer "Ancestry"
	(with-output-to-string (*standard-output*)
	  (ancestry *ps*)) t t)
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
      (pvs-buffer "Siblings"
	(with-output-to-string (*standard-output*)
	  (siblings *ps*)) t t)
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
	   (pvs-buffer "Explain TCC"
	     (with-output-to-string (*standard-output*)
	       (let* ((goal (current-goal *ps*))
		      (expr (expr goal))
		      (type (type goal))
		      (kind (kind goal))
		      (tcc (tcc goal))
		      (reason (reason goal)))
		 (case kind
		   ((subtype)(format t "Subtype "))
		   ((termination) (format t "Termination "))
		   ((cases) (format t "Cases "))
		   ((existence) (format t "Nonemptiness/Existence "))
		   ((actuals) (format t "Actuals "))
		   ((assuming) (format t "Assuming ")))
		 (format t "TCC ~%~a ~%generated by typechecking~%~a : ~a"
		   tcc expr type)))
	     t t)
	   (pvs-message "Current goal is not a TCC goal"))
      (pvs-message "No current proof")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(7/23/93) lambda-closure modelled on universal closure.

(defun lambda-closure (form)
   (let ((freevars-form (sort-freevars (freevars form))))
     (multiple-value-bind (newbindings new?)
	 (var-to-binding freevars-form)
       (let ((lform (make-lambda-expr newbindings form)))
	 (if new?
	     (reset-bindings lform freevars-form)
	     lform)))))

(defun quant-to-lambda (form)
  (if (quant-expr? form)
      (make-lambda-expr (bindings form)(expression form))
      form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;NSH(7.23.93) using gensubst for terms.

(defmethod termsubst-testfn ((expr expr) testfn)
  (funcall testfn expr))

(defmethod termsubst-testfn ((exprs list) testfn)
  NIL)

(defmethod termsubst-testfn ((expr assignment) testfn)
  NIL)

(defmethod termsubst-testfn ((exprs selection) testfn)
  NIL)

(defmethod termsubst-testfn ((expr T) testfn)
  T)

(defmethod termsubst-substfn ((expr expr) substfn)
  (funcall substfn expr))

(defmethod termsubst-substfn ((expr T) substfn)
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
				    (push key keys))
				ht)
		       keys))
		 (list *rulebase* *steps* *rules*))))
