;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checker-macros.lisp -- 
;; Author          : N. Shankar
;; Created On      : Fri Apr  3 12:35:13 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 01:18:28 1998
;; Update Count    : 6
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;
; *prover-keywords* is an alist of the form
; (command has-rest? arg1 arg2 ...) 
;
(defvar *prover-keywords* nil)

(defun make-prover-keywords (formals &optional result)
  (if (null formals)
      result
      (let* ((formal (car formals))
	     (nresult (if (memq formal '(&optional &rest))
			  result
		          (cons (intern (string (if (consp formal)
						    (car formal)
						    formal))
					'keyword)
				result))))
	(make-prover-keywords (cdr formals) nresult))))

(defvar *skovar-counter* (let ((x 0)) #'(lambda ()  (incf x))))
(defvar *skofun-counter* (let ((x 0)) #'(lambda ()  (incf x))))
(defvar *bind-counter* (let ((x 0)) #'(lambda ()  (incf x))))
(defvar *voss-counter* (let ((x 0)) #'(lambda ()  (incf x))))
(defvar *printproofstate* nil)
(defvar *in-checker* nil)
(defvar *in-apply* nil)
(defvar *please-interrupt* nil)
(defvar *assert-bindings* nil)
(defvar *modsubst* nil)
(defvar *proving-tcc* nil)
(defvar *assert-typepreds-off* nil)
(defvar *assert-typepreds* nil)
(defvar *prover-indent* 0)
(defvar *ignore-prover-output?* nil)
(defvar *rerunning-proof* nil)
(defvar *top-label* nil)
(defvar *max-occurrence* 0)
(defvar *rechecking-proof* nil)
(defvar *hash-rewrites?* nil)
(defvar *rewrite-hits* 0)
(defvar *rewrite-misses* 0)
(defvar *rewrite-hash* nil)
(defvar *pvs-voss-hash* nil)
(defvar *voss-pvs-hash* nil)
(defvar *subtype-hash* nil)
(defvar *track-rewrites* nil)
(defvar *ruletrace* nil)
(defvar *ruletracedepth* 0)
(defvar *template-num* 0)
(defvar *translate-to-prove-hash*
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
(defvar *prover-print-depth* nil)
(defvar *prover-print-length* nil)
(defvar *translate-id-hash*
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
(defvar *translate-id-counter* nil)
(defvar *process-output* nil)
(defvar *top-rewrite-hash* nil)
(defvar *top-findalist* nil)
(defvar *top-usealist* nil)
(defvar *top-sigalist* nil)
(defvar *pvs-bdd-hash* nil)
(defvar *bdd-pvs-hash* nil)
(defvar *pvs-bdd-inclusivity-formulas* nil)
(defvar *bdd-counter* (let ((x 0)) #'(lambda ()  (incf x))))
(defvar *bdd-initialized* nil)
(defvar *recognizer-forms-alist* nil)
(defvar *local-typealist* nil)
(defvar *recording-type-constraints* nil)
(defvar *top-simplify-ifs-hash* nil)
(defvar *local-simplify-ifs-hash* nil)
(defvar *dont-beta-reduce-let-exprs* nil)
(defvar *no-bound-variables-in-match* nil)
(defvar *dynamic-ordering?* nil)
(defvar *smash?* nil)
(defvar *beta-cache* (make-hash-table :test #'eq))
(defvar *match-cache* (make-hash-table :test #'eq))
(defvar *dont-cache-match?* nil)
(defvar *lift-if-updates* nil)
(defvar *last-recursive-rewrite-res* nil)
(defvar *cases-rewrite* nil)
(defvar *rewrite-print-depth* nil)
(defvar *rewrite-print-length* nil)
(defvar *auto-rewrite-depth* 0)
(defvar *mu-subtype-list* nil)
(defvar *remaining-actuals-matches* nil)
(defvar *all-boundvars* nil)
(defvar *assert-if-arith-hash* (make-hash-table :test #'eq))
(defvar *create-formulas-cache*
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
(defvar *replace-in-actuals?* nil)
(defvar *top-assert-flag* nil)
(defvar *constant-names* nil)
(defvar *collect-subterms* nil)
(defvar *detuple-singletons?* nil)
(defvar *replace-cache* nil)
(defvar *recursive-prove-decl-call* nil)
(defparameter *init-usealist*
  '((FALSE ((TRUE . FALSE) FALSE)) (TRUE ((TRUE . FALSE) TRUE))))
(defvar *init-alists*
  (make-instance 'dpinfo
    'dpinfo-sigalist nil
    'dpinfo-findalist nil
    'dpinfo-usealist *init-usealist*))
(defvar *goal*)
(defvar *label*)
(defvar *subgoalnum*)
(defvar *+*)
(defvar *-*)
(defvar *par-label*)
(defvar *par-goal*)
;;(defvar *current-context*)
(defvar *module-context*)
;;(defvar *current-theory*)
(defvar *ps* nil)
(defvar * '*)
(defvar + '+)
(defvar - '-)
(defvar *macro-names* nil)
(defvar *subst-type-hash* ;;used in assert-sformnums
  (make-hash-table :hash-function 'pvs-sxhash :test 'tc-eq))
(defvar *checkpointed-branches* nil)
(defvar *dp-print-incompatible-warning* t)
(defvar *print-expanded-dpinfo* t)
(defvar *dp-state* nil)
(defvar *new-ground?* nil)
(defvar *old-ground?* t)
(defvar *newdc* nil) ;;needs to be set to T to get new ground prover
(defvar *dp-changed*)
(defvar *alists*)
(defvar *dp-state*)
(defvar *top-alists*)
(defvar *top-dp-state*)
(defvar *break-on-ground-diff* t)

(defun new-ground ()
  (setq *newdc* t
	*new-ground?* t
	*old-ground?* nil))

(defun old-ground ()
  (setq *newdc* nil
	*new-ground?* nil
	*old-ground?* t))

(defun both-ground ()
  (setq *newdc* nil
	*new-ground?* t
	*old-ground?* t))

#+lucid
(defmacro pvs-format (stream format-string &rest args)
  `(lisp::format ,stream ,format-string ,@args))

#-lucid
(defmacro pvs-format (stream format-string &rest args)
  `(format ,stream ,format-string ,@args))

#+allegro
(defmacro with-interrupts-allowed (&body form)
  `(let ((excl::*without-interrupts* nil)) ,@form))

#+allegro
(defmacro with-interrupts-deferred (&body form)
  `(let ((excl::*without-interrupts* t)) ,@form))


;;; KCL does not really have the equivalent to the following - punt for now.
#+kcl
(defun with-interrupts-allowed (form)
  form)

#+kcl
(defun with-interrupts-deferred (form)
  form)

(defmacro catch-restore (form)  ;;NSH(8.22.94)
  `(if *in-apply* ,form
       (with-interrupts-allowed
	(catch 'abort ,form))))


(defmacro make-bool-expr (op &rest args)
  `(make-application (mk-name-expr ,op) ,@args))

;(defmacro op-id (id)
;  `(eq (id (operator ,name-expr)) id))

(defmacro eq-op-id (applic-expr given-id)
  `(and (name-expr? (operator ,applic-expr))
       (eq (id  (operator ,applic-expr))
	   ,given-id)))

(defmacro memq-op-id (applic-expr given-ids)
  `(and (name-expr? (operator ,applic-expr))
       (memq (id  (operator ,applic-expr))
	   ,given-ids)))


(defmacro newcounter (name)
   `(setq ,name (let ((x 0))
		 #'(lambda () (incf x)))))

(defmacro fstarg (application)
  `(car (arguments ,application)))

;;; Used in prove-decl to decide whether to simply use the existing hash
;;; table or to create a new one if this is a recursive call.
(defmacro init-if-rec (gvar)
  (let ((val (gentemp)))
    `(let ((,val ,gvar))
       (assert (hash-table-p ,val))
       (if *recursive-prove-decl-call*
	   (let ((test (hash-table-test ,val)))
	     (if (memq test '(eq eql equal equalp))
		 (make-hash-table :test test)
		 (make-hash-table :test test :hash-function 'pvs-sxhash)))
	   ,val))))

;;The rulebase is represented as a hash-table.

(defmacro init-symbol-table () `(make-hash-table :test #'eq))
(defmacro add-symbol-entry (symbol entry hash-table)
  `(setf (gethash ,symbol ,hash-table) ,entry))

(defvar *rulebase* (init-symbol-table))
(defvar *rulenames* nil)
(defvar *rulefunbase* (init-symbol-table))
(defvar *rulefunnames* nil)
(defvar *strategies* (init-symbol-table))
(defvar *strategy-names* nil)
(defvar *suppress-printing* nil)
(defvar *noninteractivemode* nil)
(defvar *cache-formula*   (init-symbol-table))
(defvar *top-proofstate* nil)
(defvar *auto-rewrites* (init-symbol-table))
(defvar *all-rewrites-names* nil)
(defvar *auto-rewrites-names* nil)
(defvar *auto-rewrites!-names* nil)
(defvar *auto-rewrites-ops* (init-symbol-table))
(defvar *auto-rewrites-off* nil)
(defvar *new-fmla-nums* nil)
(defvar *rewrite-msg-off* nil)

(defmacro format-list-of-items (list)
  `(format nil "~{~#[ none~;~s~; ~s and ~s~:;~@{~#[~; and~] ~s~^,~}~].~}"
    ,list))

(defmacro error-format-if (string &rest args)
  `(if *suppress-printing*
    (set-strategy-errors (format nil ,string ,@args))
    (format t ,string ,@args)))

(defmacro format-if (string &rest args)
  `(unless *suppress-printing*
    (format t ,string ,@args)))

(defmacro format-if-nontcc (string &rest args)
  `(unless *proving-tcc*
    (format t ,string ,@args)))

(defmacro format-rewrite-msg (id lhs rhs)
  `(unless (or *proving-tcc* *rewrite-msg-off*)
    (let* ((id-string (format nil "~a" ,id))
	   (*sb-print-depth* *rewrite-print-depth*)
	   (*sb-print-length* *rewrite-print-length*))
      (cond ((eq *rewrite-print-depth* 0)
	     (format t "~%Rewriting with ~a" id-string))
	    (t (format t "~%~a rewrites " id-string)
	       (let* ((lhs-string (format nil "~a" ,lhs))
		      (id-length (length id-string))
		      (lhs-length (length lhs-string)))
		 (if (< lhs-length (- *default-char-width* (+ id-length 12)))
		     (format t "~a" lhs-string)
		     (format t "~%~V@T~a" 2 (unpindent ,lhs 2 :string T)))
		 (format t "~%  to ~a" (unpindent ,rhs 6 :string T))))))))
	     
;    (format t "~%~a rewrites ~a to ~a"
;      ,id ,lhs ,rhs)))

;;; SO - this doesn't work
;(defmacro lambdign (args &rest body)
;  `(lambda (,@args) (declare (ignore ,@args)) ,@body))

(defmacro make-lambda-macro (args op-args body)
  (if op-args
      `(function (lambda (,@args &optional ,@op-args) ,body))
      `(function (lambda (,@args) ,body))))

;;addrule moved to rules.lisp.

;(defmacro addrulefun (name required-args optional-args body
;			      docstring)
;  `(let ((entry (gethash ,name *rulefunbase*))
;	)
;     (cond ((null entry)
;	    (add-symbol-entry ,name
;			      (make-instance 'rulefun-entry
;				'name ,name
;				'required-args (quote ,required-args)
;				'optional-args (quote ,optional-args)
;				'rulefun
;				,(macroexpand
;				  `(make-lambda-macro ,required-args
;						     ,optional-args
;						   ,body))
;				'docstring ,docstring)
;			      *rulefunbase*)
;	    (push ,name *rulefunnames*)
;	    (format t "~%Added rulefun ~a.~%" ,name))
;	   (t ;;(y-or-n-p "Do you really want to change rule ~a?" ,name)
;	    (setf (required-args entry) (quote ,required-args)
;		    (optional-args entry) (quote ,optional-args)
;		    (rulefun  entry)
;		    ,(macroexpand `(make-lambda-macro ,required-args ,optional-args
;				    ,body)))
;	      (format t "~%Changed rulefun ~a.~%" ,name))
;	   (t (format t "~%No change.~%")))))
;
;(defmacro add-strategy-fun (name required-args optional-args body
;			      docstring)
;  `(let ((entry (gethash ,name *strategies*))
;	)
;     (cond ((null entry)
;	    (add-symbol-entry ,name
;			      (make-instance 'strategy-entry
;				'name ,name
;				'required-args (quote ,required-args)
;				'optional-args (quote ,optional-args)
;				'strategy-fun 
;				,(macroexpand
;				  `(make-lambda-macro ,required-args
;						     ,optional-args
;						   ,body))
;				'docstring ,docstring)
;			      *strategies*)
;	    (push ,name *strategy-names*)
;	    (format t "~%Added rulefun ~a.~%" ,name))
;	   (t ;;(y-or-n-p "Do you really want to change rule ~a?" ,name)
;	    (setf (required-args entry) (quote ,required-args)
;		    (optional-args entry) (quote ,optional-args)
;		    (strategy-fun  entry)
;		    ,(macroexpand `(make-lambda-macro ,required-args ,optional-args
;				    ,body)))
;	      (format t "~%Changed strategy ~a.~%" ,name))
;	   (t (format t "~%No change.~%")))))

;;adds a synonym for a rule, rulefun, or strategy.
(defmacro synonymize (rulename1 rulename2 optable)
  `(let ((entry (gethash ,rulename1 ,optable)))
    (cond ((null entry)
	   (add-symbol-entry ,rulename1 (gethash ,rulename2 ,optable))
	   (format t "~%~a and ~a are now synonymous.~%" ,rulename1 ,rulename2))
	  ((y-or-n-p "Do you really want to change rule ~a?" ,rulename1)
	   (add-symbol-entry ,rulename1 (gethash ,rulename2 ,optable))
	   (format t "~%~a and ~a are synonymous.~%" ,rulename1 ,rulename2))
	  (t (format t "~%No Change. ~%")))))

(defmacro rule-synonym (rulename1 rulename2)
  `(synonymize ,rulename1 ,rulename2 *rulebase*))


(defun true-p (expr)
  (or (eq expr 'true)
      (tc-eq expr *true*)))

(defun false-p (expr)
  (or (eq expr 'false)
      (tc-eq expr *false*)))

(defmacro const (x) `#'(lambda (y) ,x))	 

(defmacro add-strategy (name args)
  `(push (cons ,name ,args) *strategies*))

(defmacro lambda? (exp)
  `(typep ,exp 'lambda-expr))


(defmacro forall? (exp)
  `(forall-expr? ,exp))


(defmacro exists? (exp)
  `(exists-expr? ,exp))

(defmacro parenthesize (exp cond)
  `(if (and ,cond
	(eq (type-of ,exp) 'infix-application)
	(eq (parens ,exp) 0))
    (copy ,exp 'parens 1)
    ,exp))

;;in-sformnums? checks if the current sformnum (pos or neg, 
;;  according to sign of sform) is in sformnums.
(defmacro in-sformnums? (sform pos neg sformnums)
  (let ((sign (gentemp))
	(sfnums (gentemp)))
    `(let ((,sign (not (negation? (formula ,sform))))
	   (,sfnums (cleanup-fnums ,sformnums)))
       (cond ((eql ,sfnums '*) T)
	     ((eql ,sfnums '+) ,sign)
	     ((eql ,sfnums '-) (not ,sign))
	     ((eql ,sfnums ,pos) ,sign)
	     ((eql ,sfnums ,neg) (not ,sign))
	     ((and (label ,sform) (memq ,sfnums (label ,sform))) T)
	     ;;NSH(12-23-91): Turning this off; restore if formula labels are used.
	     ;;NSH(4.3.97): restoring labels	
	     ((consp ,sfnums)
	      (cond ((memq '* ,sfnums) T)
		    ((memq '+ ,sfnums) ,sign)
		    ((memq '- ,sfnums) (not ,sign))
		    ((and (label ,sform)(intersection (label ,sform) ,sfnums)))
		    ((consp (car ,sfnums))
		     (if ,sign (member ,pos (car ,sfnums))
			 (member ,neg (car ,sfnums))))
		    (t (if ,sign (member ,pos ,sfnums)
			   (member ,neg ,sfnums)))))
	     (t nil)))))

(defmacro check-sformnums? (sformnums)
  `(or (integerp ,sformnums)
    (member ,sformnums '(+ * -))
    (and (consp ,sformnums)
     (every #'integerp ,sformnums))))

(defmacro restore ()
  `(throw 'abort *in-evaluator*))
;NSH(8.22.94): modified catch-restore to catch only outside apply.
;  `(if *in-apply*
;    (throw 'abort-in-apply nil)
;    (throw 'abort nil)))

(defmacro strategy-name? (rule-name)
  `(gethash ,rule-name *strategies*))

;;NSH(10.7.94) not used anywhere.
(defmacro signal-application (expr sigop newop sigargs newargs)
  `(if (and (eq ,sigop 'X)(eq ,sigargs 'X))
	  (values 'X ,expr)
	  (values '? (copy ,expr
			   'operator ,newop
			   'arguments ,newargs))))


(defmacro fresh? (proofstate)
  `(or (null (status-flag ,proofstate))
    (eql (status-flag ,proofstate) 0)))

(defmacro kwote (x)
  `(list 'quote ,x))

(defmacro mk-let (bindings expr)
  `(if (null ,bindings) ,expr
      (list 'let ,bindings ,expr)))

(defmacro mk-let* (bindings expr)
   `(if (null ,bindings) ,expr
      (list 'let* ,bindings ,expr)))

(defmacro timeprover (expr)
  `(let ((init-real-time (get-internal-real-time))
	 (init-run-time (get-internal-run-time))
	 (x ,expr))
    (format-if "~%~%Run time  = ~,2F secs."
      (/ (- (get-internal-run-time) init-run-time)
		internal-time-units-per-second))
    (format-if "~%Real time = ~,2F secs.~%"
      (/ (- (get-internal-real-time) init-real-time)
		internal-time-units-per-second))
    x))

;;NSH(1.31.94):modified to use tccinfo in *tccforms*
(defmacro make-assert-expr (expr)
  `(let* ((*tccforms* nil)
	  (*keep-unbound*  *bound-variables*)
	  (*generate-tccs* (if (and *generate-tccs*
				    (not (eq *generate-tccs* 'NONE)))
			       *generate-tccs*
			       'ALL))
	  (*false-tcc-error-flag* nil)
	  (expr ,expr)
	  (tcc-fmlas (when (not (eq *generate-tccs* 'NONE))
		       (loop for tccinfo in *tccforms*
			     when (not (forall-expr?;;NSH(8.19.98)
					;;prunes out TCCs w/freevars
					(tccinfo-formula tccinfo)))
			     nconc (and+ (tccinfo-formula tccinfo)))))
	  (test (loop for tcc-fmla in tcc-fmlas always
		      (let ((test (assert-test tcc-fmla)))
			(if (not (or (true-p test) (false-p test)))
			    (setq *dont-cache-match?* T)) ;NSH(6.12.95)
			(true-p test)))))
     (when test expr)))

(defmacro track-rewrite-format (res expr format-string &rest args)
  `(let ((id (if (consp ,res)(car ,res)(id ,res))))
     ;;NSH(9.19.97) removed (format nil "~a" ...) from above
     (when (member id *track-rewrites* :test #'same-id)
       (let ((expr-string (format nil "~a" ,expr)))
	 (format t "~%;;~a failed to rewrite " id)
	 (if (> (+ (length id)(length expr-string) 21) *default-char-width*)
	     (format t "~%;;~a" (unpindent ,expr 2 :string T))
	     (format t "~a" expr-string))
	 (format t "~%;;;;")
	 (format t ,format-string ,@args)))))


(defmacro replace-eq (lhs rhs)
  `(let ((*modsubst* T)
	 (subst (pairlis *bound-variables*
			 *bound-variables*))
	 (*bound-variables* nil))
    (not (eq (match ,lhs ,rhs nil subst) 'fail))))

(defmacro ignore-sig (expr)
  `(multiple-value-bind (sig value)
    ,expr
    value))

(defmacro print-proofstate (ps)
  `(let ((*sb-print-depth* *prover-print-depth*)
	 (*sb-print-length* *prover-print-length*))
    (format t "~a" ,ps)))


(defmacro print-proofstate-if (ps)
  `(let ((*sb-print-depth* *prover-print-depth*)
	 (*sb-print-length* *prover-print-length*))
    (format-if "~a" ,ps)))

(defmacro safe-parse-integer (string)
  `(if (every #'digit-char-p ,string) (parse-integer ,string) 0))

(defmacro no-tccs (expr)
  `(let ((*generate-tccs* 'NONE))
    ,expr))

(defmacro push-references (expr ps)
  `(mapc #'(lambda (x)
	     (unless (skolem-const-decl? x)
	       (pushnew x (dependent-decls ,ps))))
    (collect-references ,expr)))

(defmacro push-references-list (expr list)
  `(mapc #'(lambda (x)
	     (unless (skolem-const-decl? x)
	       (pushnew x ,list)))
	 (collect-references ,expr)))

(defmacro assoc-decl (x alist)
  `(assoc (declaration ,x) ,alist
	  :test #'(lambda (u v)(eq u (declaration v)))))

(defmacro tc-assoc (x alist)
  `(assoc ,x ,alist :test #'tc-eq))

(defmacro nprotecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (unwind-protect
	   (setq ,resultsym
		 (multiple-value-list (progn ,@body)))
	 (restore-old-cs ,new-cong-state))
       (values-list ,resultsym))))

(defmacro copying-cong-state (((new-cong-state old-cong-state)
			       (new-alists old-alists))
			      &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (copy-cs ,old-cong-state))
	   (,resultsym nil))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defmacro protecting-cong-state (((new-cong-state old-cong-state)
				  (new-alists old-alists))
				 &body body)
  (let ((resultsym (gensym)))
    `(let ((,new-alists (copy ,old-alists))
	   (,new-cong-state (new-cs ,old-cong-state))
	   (,resultsym nil))
       (setq ,resultsym
	     (multiple-value-list (progn ,@body)))
       (values-list ,resultsym))))

(defmacro call-process (expr dp-state alists)
  (let ((g-expr (gentemp))
	(g-dp-state (gentemp))
	(g-alists (gentemp)))
    `(let* ((,g-expr ,expr)
	    (,g-dp-state ,dp-state)
	    (,g-alists ,alists)
	    (sigalist (dpinfo-sigalist ,g-alists))
	    (findalist (dpinfo-findalist ,g-alists))
	    (usealist (dpinfo-usealist ,g-alists))
	    (new-expr (when *new-ground?* (top-translate-to-dc ,g-expr)))
	    ;; put in (typep expr 'syntax) check
	    ;; in case call-process is called from process-assert
	    ;; which already has translated exprs
	    (old-expr (when *old-ground?*
			(if (typep ,g-expr 'syntax)
			    (top-translate-to-old-prove ,g-expr)
			    ,g-expr)))
	    (new-result nil)
	    (old-result nil))
       (let ((typealist (append *local-typealist* typealist)))
	 (when *new-ground?*
	   (setq new-result (translate-from-dc
			     (dp::invoke-process new-expr ,g-dp-state))))
	 (when *old-ground?*
	   (setq old-result (translate-from-prove-list
			       (invoke-process old-expr)))
	   (setf (dpinfo-sigalist ,g-alists) sigalist
		 (dpinfo-findalist ,g-alists) findalist
		 (dpinfo-usealist ,g-alists) usealist))
	 (let ((not-incompatible
		(or (not (and *new-ground?* *old-ground?*))
		    (and (compatible-dp-results new-result old-result)))))
	   (when (and *dp-print-incompatible-warning*
		      (not not-incompatible))
	     (format t "~%***IncompatibleWarning*** expr: ~A,~%new-result: ~A, ~%old-result:~A"
	       new-expr new-result old-result))
	   (assert (or (not *break-on-ground-diff*)
		       not-incompatible)
		   (*break-on-ground-diff*)))
	 (setq *break-on-ground-diff* t)
	 (if *new-ground?*
	     new-result
	     old-result)))))

(defmacro translate-to-ground (expr)
  `(if *newdc*
      (translate-to-dc ,expr)
      (translate-to-prove ,expr)))

(defmacro translate-with-new-hash (&rest body)
  `(if *newdc*
       (let ((*translate-to-dc-hash*
	      (make-hash-table  ;;NSH(2.5.95)
	       :hash-function 'pvs-sxhash ;;hash to pvs-hash
	       :test 'tc-eq)))
	 ,@body)
      (let ((*translate-to-prove-hash*
	      (make-hash-table  ;;NSH(2.5.95)
	       :hash-function 'pvs-sxhash ;;hash to pvs-hash
	       :test 'tc-eq)))
	 ,@body)))

(defmacro inc-rewrite-depth (res)
  `(let ((id (if (consp ,res) (car ,res)(id ,res))))
     (incf *auto-rewrite-depth*)
     (when (zerop (mod *auto-rewrite-depth* 50))
       (format t "~%Warning: Rewriting depth = ~d; Rewriting with ~a"
	 *auto-rewrite-depth* id))))
  
(defmacro match! (pattern expr bind-alist subst)
  `(let ((*modsubst* T))
    (match ,pattern ,expr ,bind-alist ,subst)))

(defmacro with-zero-context (lisp-expr)
  `(nprotecting-cong-state
    ((*dp-state* *init-dp-state*)
     (*alists* *init-alists*))
    ,lisp-expr))

(defmacro inc-fnum (fnum)
  `(if (< ,fnum 0)
      (1- ,fnum)
      (1+ ,fnum)))

(defmacro dec-fnum (fnum)
  `(if (< ,fnum -1)
      (1+ ,fnum)
      (if (> ,fnum 1)
	  (1- ,fnum)
	  ,fnum)))

(defmacro check-for-connectives? (fmla)
;;  `(unless *assert-connectives?*) ;;NSH(11.24.98)not going 
                                   ;;to worry about asserting connectives
     `(connective-occurs? ,fmla))
