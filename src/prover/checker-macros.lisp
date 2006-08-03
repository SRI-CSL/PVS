;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checker-macros.lisp -- 
;; Author          : N. Shankar
;; Created On      : Fri Apr  3 12:35:13 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Sat Oct 31 01:18:28 1998
;; Update Count    : 6
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

;
; *prover-keywords* is an alist of the form
; (command has-rest? arg1 arg2 ...) 
;
(defvar *prover-keywords* nil)

(defun make-prover-keywords (formals &optional result)
  (if (null formals)
      (nreverse result)
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
(defvar *rerunning-proof-message-time* nil)
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
(defvar *translate-to-prove-hash*)
(defvar *prover-print-depth* nil)
(defvar *prover-print-length* nil)
(defvar *translate-id-hash*)
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
(defvar *sequent-typealist*)   ; Put e.g., integer_pred(x) in typealist
(defvar *recording-type-constraints* nil)
(defvar *top-simplify-ifs-hash* nil)
(defvar *local-simplify-ifs-hash* nil)
(defvar *let-reduce?* t)
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
(defvar *assert-if-arith-hash*)
(defvar *create-formulas-cache*)
(defvar *replace-in-actuals?* nil)
(defvar *top-assert-flag* nil)
(defvar *constant-names* nil)
(defvar *collect-subterms* nil)
(defvar *detuple-singletons?* nil)
(defvar *replace-cache* nil)
(defvar *recursive-prove-decl-call* nil)
(defparameter *init-usealist*
  '((false ((true . false) false)) (true ((true . false) true))))
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
  ;;(make-pvs-hash-table)
  nil) ;; Make this nil till we can analyze this further.
(defvar *checkpointed-branches* nil)
(defvar *dp-print-incompatible-warning* t)
(defvar *print-expanded-dpinfo* t)
(defvar *init-dp-state*)
(defvar *top-dp-state*)
(defvar *dp-state*)
(defvar *dp-changed*)
(defvar *infinity* (gensym "*-infinity"))
(defvar *quant-simp?* nil)
(defvar *implicit-typepreds?* nil)

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

#+(or cmu sbcl)
(defmacro with-interrupts-allowed (&body form)
  `(system:with-interrupts ,@form))

#+(or cmu sbcl)
(defmacro with-interrupts-deferred (&body form)
  `(system:without-interrupts ,@form))



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
		  (declare (fixnum x))
		  #'(lambda () (incf x)))))

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
		 (make-pvs-hash-table)))
	   (clrhash ,val)))))

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
		     (format t "~%~V@T~a" 2 (unpindent ,lhs 2 :string t)))
		 (format t "~%  to ~a" (unpindent ,rhs 6 :string t))))))))
	     
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

;;in-sformnums? checks if the current sform (pos or neg, 
;;  according to sign of sform) is in sformnums.
(defmacro in-sformnums? (sform pos neg sformnums)
  (let ((sign (gentemp))
	(sfnums (gentemp)))
    `(let ((,sign (not (negation? (formula ,sform))))
	   (,sfnums ,sformnums))
       ;; Assumes sformnums has been through cleanup-fnums
       (cond ((memq '* ,sfnums) t)
	     ((and (memq '+ ,sfnums) ,sign))
	     ((and (memq '- ,sfnums) (not ,sign)))
	     ((and (label ,sform)
		   (some #'(lambda (lbl) (memq lbl ,sfnums))
			 (label ,sform))))
	     (t (if ,sign
		    (member ,pos ,sfnums)
		    (member ,neg ,sfnums)))))))

(defmacro restore ()
  `(progn (when *in-evaluator*
	    (clear-dependent-theories *current-theory*))
	  (throw 'abort *in-evaluator*)))
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
	  (*keep-unbound* *bound-variables*)
	  (*generate-tccs* (if (and *generate-tccs*
				    (not (eq *generate-tccs* 'none)))
			       *generate-tccs*
			       'all))
	  (*false-tcc-error-flag* nil)
	  (expr ,expr)
	  (tcc-fmlas (when (not (eq *generate-tccs* 'none))
		       (loop for tccinfo in *tccforms*
			     when (not (forall-expr? ;;NSH(8.19.98)
					;;prunes out TCCs w/freevars
					(tccinfo-formula tccinfo)))
			     nconc (and+ (tccinfo-formula tccinfo)))))
	  (test (loop for tcc-fmla in tcc-fmlas always
		      (let ((test (assert-test tcc-fmla)))
			(if (not (or (true-p test) (false-p test)))
			    (setq *dont-cache-match?* t)) ;NSH(6.12.95)
			(true-p test)))))
     (when test expr)))

(defmacro track-rewrite-format (res expr format-string &rest args)
  `(let ((id (if (consp ,res)(car ,res)(id ,res))))
     ;;NSH(9.19.97) removed (format nil "~a" ...) from above
     (when (member id *track-rewrites* :test #'same-id)
       (let ((expr-string (format nil "~a" ,expr)))
	 (format t "~%;;~a failed to rewrite " id)
	 (if (> (+ (length (string id))(length expr-string) 21) *default-char-width*)
	     (format t "~%;;~a" (unpindent ,expr 2 :string t))
	     (format t "~a" expr-string))
	 (format t "~%;;;;")
	 (format t ,format-string ,@args)))))


(defmacro replace-eq (lhs rhs)
  `(let ((*modsubst* t)
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
     (unless (and (consp (current-input ,ps))
		  (eq (car (current-input ,ps)) 'lisp))
       (format-if "~a" ,ps))))

(defmacro safe-parse-integer (string)
  `(if (every #'digit-char-p ,string) (parse-integer ,string) 0))

(defmacro no-tccs (expr)
  `(let ((*generate-tccs* 'none))
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

(defmacro nprotecting-cong-state (((new-state old-state)) &body body)
  `(let ((,new-state (dpi-push-state ,old-state)))
     (unwind-protect
	 (progn ,@body)
       (dpi-pop-state ,new-state))))

(defmacro protecting-cong-state (((new-state old-state)) &body body)
  `(let ((,new-state (dpi-push-state ,old-state)))
     ,@body))

(defmacro call-process (expr dp-state)
  (let ((result (gentemp)))
    `(let ((,result (multiple-value-list (dpi-process ,expr ,dp-state))))
       (setq ,dp-state (cadr ,result))
       (car ,result))))

(defmacro translate-to-ground (expr)
  `(translate-to-prove ,expr))

(defmacro translate-with-new-hash (&rest body)
  `(let ((*translate-to-prove-hash*
	  (make-pvs-hash-table)))
     ,@body))

(defmacro inc-rewrite-depth (res)
  `(let ((id (if (consp ,res) (car ,res)(id ,res))))
     (incf *auto-rewrite-depth*)
     (when (zerop (mod *auto-rewrite-depth* 50))
       (format t "~%Warning: Rewriting depth = ~d; Rewriting with ~a"
	 *auto-rewrite-depth* id))))
  
(defmacro match! (pattern expr bind-alist subst)
  `(let ((*modsubst* t))
    (match ,pattern ,expr ,bind-alist ,subst)))

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
