;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs-mu.lisp -- Interface to the Mu-calculus model-checker
;; Author          : Sree, Shankar and Saidi
;; Created On      : Wed May  3 19:51:22 1995
;; Last Modified By: Hassen Saidi
;; Last Modified On: Fri Apr  3 14:22:30 1998
;; Update Count    : 16
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;; Formula mu_mk_false_formula (void)
;;; Formula mu_mk_false_formula (void)
;;; Formula mu_mk_true_formula (void)
;;; Formula mu_mk_bool_var (char *name)
;;; Formula mu_mk_unary_formula (FormulaType type, Formula f1)
;;; Formula mu_mk_binary_formula (FormulaType type, Formula f1, Formula f2)
;;; Formula mu_mk_ite_formula (Formula cond, Formula then_part, Formula else_part)
;;; Formula mu_mk_quantified_formula (FormulaType type, LIST vars,Formula f1)
;;; Formula mu_mk_subst (Formula f, int var, Formula g)
;;; Formula mu_mk_application (Term R, LIST subs, int curried)

(addrule 'musimp () ((fnums *)
		     (dynamic-ordering? nil))
	 (musimp-fun fnums dynamic-ordering?)
	 "MU Calculus Simplification .
  Dynamic ordering means the BDD package can reorder literals
  to reduce BDD size."
	 "~%Applying musimp,")

(defun musimp-fun (&optional fnums dynamic-ordering?)
  #'(lambda (ps)(run-musimp ps fnums dynamic-ordering?)))

(defun run-musimp (ps fnums dynamic-ordering?)
  (let ((*pvs-bdd-hash* (make-pvs-hash-table :hashfn #'pvs-sxhash
					     :test #'tc-eq))
	(*bdd-pvs-hash* (make-hash-table :test #'eq))
	(*pvs-bdd-inclusivity-formulas* nil)
	(*bdd-counter* *bdd-counter*)
	(*recognizer-forms-alist* nil)
	(*dynamic-ordering?* dynamic-ordering?)
	(*mu-subtype-list* nil))
    (let* ((sforms (s-forms (current-goal ps)))
	   (selected-sforms (select-seq sforms fnums))
	   (remaining-sforms (delete-seq sforms fnums))
	   (mu-string
	    (format nil "~a"
	      (make-bdd-restriction
	       (convert-pvs-to-mu
		(make-conjunction
		 (mapcar #'(lambda (sf) (negate (formula sf)))
		   selected-sforms)))
	       (make-bdd-conjunction
		(loop for x in  *pvs-bdd-inclusivity-formulas*
		      when (null (freevars (car x)));;NSH(8.17.95): subtypes
		      collect
		      (cdr x))))))
	   (tmp-file
	    (write-to-temp-file mu-string))
	   (mu-out-tmp-file
	    (write-to-temp-file ""))
	   (mu-output
	    ;; this is just to get the
	    ;; name of the muoutfile
	    ;; this style of programming was necessary for run-program
	    ;; call
	    (unwind-protect
		(progn
		  (if *dynamic-ordering?*
		      (run-program "pvsmu"
			:arguments
			(list
			 "-I";; output irredundant form
			 "-w";; inhibits warnings
			 "-s";; frontier set
			 "-v";; verbose
			 (format nil "-o~a" mu-out-tmp-file)
			 (format nil "~a" tmp-file)))
		      (run-program "pvsmu"
			:arguments
			(list
			 "-I";; output irredundant form
			 "-E";; cancel dynamic ordering
			 "-w";; inhibits warnings
			 "-s";; frontier set
			 "-v";; verbose
			 (format nil "-o~a" mu-out-tmp-file)
			 (format nil "~a" tmp-file))))
		  (get-bdd-output-string mu-out-tmp-file)
		  )
	      (delete-file mu-out-tmp-file)
	      (delete-file tmp-file)
	      )))
      ;;	  (format t "~%~Tin = ~a" mu-string)
      ;;	  (format t "~%~Tout = ~a" mu-output)
      ;;	  	  (break)
      (if (string= mu-output "")
	  (values 'X nil nil)
	  (let ((pvs-output (convert-mu-to-pvs mu-output)))
	    (if (null pvs-output)
		(values 'X nil nil)
		(let* ((conjuncts (and+ (negate pvs-output)))
		       (subgoals (mapcar
				     #'(lambda (disj-fmla)
					 (copy (current-goal ps)
					   's-forms
					   (nconc
					    (mapcar
						#'(lambda (fmla)
						    (let ((mem
							   (member fmla sforms
								   :key #'formula
								   :test #'tc-eq)))
						      (if mem (car mem)
							  (make-instance 's-formula
							    'formula fmla))))
					      (simplify-disjunct disj-fmla))
					    remaining-sforms)))
				   conjuncts)))
		  (if (and (singleton? subgoals)
			   (subsetp (s-forms (car subgoals)) sforms)
			   (subsetp sforms (s-forms (car subgoals))))
		      (values 'X nil nil)
		      (values '? ;;;something related to pvs-output
			      subgoals)))))))))

;;
;; Saidi(4.6.98)
;; Temporary comments
;;
;; New run-musimp function. Calls run-pvsmu to run the model-checker.
;; Calls add-mu-subgoals to build new subgoals.
;;


(defun run-musimp (ps fnums dynamic-ordering?)
(let* ((sforms (s-forms (current-goal ps)))
	   (selected-sforms (select-seq sforms fnums))
	   (remaining-sforms (delete-seq sforms fnums))
	   (mu-expression
	    (make-bdd-restriction-expr ;; to change
	       (convert-pvs-to-mu-expr  ;; to change
		(make-conjunction
		 (mapcar #'(lambda (sf) (negate (formula sf)))
		   selected-sforms)))
	       (make-bdd-conjunction-expr ;;  to change BDD-AND ???
		(loop for x in  *pvs-bdd-inclusivity-formulas*
		      when (null (freevars (car x)));;NSH(8.17.95): subtypes
		      collect
		      (cdr x)))))
           (mu-output (run-pvsmu mu-expression dynamic-ordering?))
        )
     (add-mu-subgoals ps sforms mu-output remaining-sforms)
  )
)


;;
;;
;; run-pvsmu
;;

(defun run-pvsmu (mu-expression dynamic-ordering?)
 (let ((*pvs-bdd-hash* (make-pvs-hash-table :hashfn #'pvs-sxhash
					     :test #'tc-eq))
	(*bdd-pvs-hash* (make-hash-table :test #'eq))
	(*pvs-bdd-inclusivity-formulas* nil)
	(*bdd-counter* *bdd-counter*)
	(*recognizer-forms-alist* nil)
	(*dynamic-ordering?* dynamic-ordering?)
	(*mu-subtype-list* nil))
   (real-run-pvsmu mu-expression *dynamic-ordering?*)
 )
)

;;
;;
;; add-mu-subgoals 
;;

(defun add-mu-subgoals (ps sforms mu-output remaining-sforms)
      (if (string= mu-output "") ;; NO should find out what is the
                                 ;; mu-expression for empty result
	  (values 'X nil nil)
	  (let ((pvs-output (convert-mu-expr-to-pvs mu-output)))
	    (if (null pvs-output)
		(values 'X nil nil)
		(let* ((conjuncts (and+ (negate pvs-output)))
		       (subgoals (mapcar
				    #'(lambda (disj-fmla)
				        (copy (current-goal ps)
				          's-forms
					  (nconc
					   (mapcar
					       #'(lambda (fmla)
					           (let ((mem
							 (member fmla sforms
							       :key #'formula
							       :test #'tc-eq)))
						    (if mem (car mem)
						       (make-instance 's-formula
							    'formula fmla))))
					      (simplify-disjunct disj-fmla))
					    remaining-sforms)))
				   conjuncts)))
		  (if (and (singleton? subgoals)
			   (subsetp (s-forms (car subgoals)) sforms)
			   (subsetp sforms (s-forms (car subgoals))))
		      (values 'X nil nil)
		      (values '? ;;;something related to pvs-output
			      subgoals))))))
)


;;
;;
;; convert-pvs-to-mu-expr
;; calls convert-pvs-to-mu-expr*
;;

(defun convert-pvs-to-mu-expr (expr) ;;expr must be of boolean type
  (let* ((*bound-variables* nil)
	 (*mu-nu-lambda-bindings-list* nil)
	 (mu-expr (convert-pvs-to-mu-expr* expr)))
 mu-expr)
)


;;
;; convert-pvs-to-mu-expr* :around ((expr expr));; same as the original version
;; convert-pvs-to-mu-expr* ((expr list))        ;; same as the original version
;; convert-pvs-to-mu-expr* ((expr cases-expr))  ;; same as the original version 
;; convert-pvs-to-mu-expr* ((expr application)) ;; NOT the same 
;; convert-pvs-to-mu-expr* ((expr expr))        ;; NOT the same 
;; convert-pvs-to-mu-expr* ((expr number-expr)) ;; NOT the same 
;;

(defmethod convert-pvs-to-mu-expr* :around ((expr expr))
  (let ((type (type expr)))
    (when (and (subtype? type)
	       (not (member expr *mu-subtype-list* :test #'tc-eq))
	       (not (assoc expr *pvs-bdd-inclusivity-formulas*
			   :test #'tc-eq)))
      (let ((constraints (collect-type-constraints-step expr))
	    (*mu-subtype-list* (cons expr *mu-subtype-list*)))
	(loop for x in constraints do
	      (push (cons expr (convert-pvs-to-mu* x))
		    *pvs-bdd-inclusivity-formulas*))
	(format t "~%Added constraints: ~a" constraints)))
    (call-next-method)))

;;
;;
;;

(defmethod convert-pvs-to-mu-expr* ((expr list))
  (loop for x in expr
	append (let ((result (convert-pvs-to-mu-expr* x)))
		(if (listp result)
		    result
		    (list result)))))

;;
;;
;;

(defmethod convert-pvs-to-mu-expr* ((expr cases-expr))
  (convert-pvs-to-mu-expr* (translate-cases-to-if expr)))

;;
;;
;;

(defmethod convert-pvs-to-mu-expr* ((expr number-expr))
  (convert-number-expr (number expr)))

(defmethod convert-number-expr (number) ;; ????
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                          ;;;
;;                                                                          ;;;
;;                                                                          ;;;
;;                                                                          ;;; 
;;                                                                          ;;;
;;                                                                          ;;;
;;                                                                          ;;;
;;                                                                          ;;;
;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;NSH(4.7.95): A brief explanation of the convert-pvs-to-mu.
;;The musimp rule invokes musimp-fun which sets up *pvs-bdd-hash*
;;and *bdd-pvs-hash* and applies convert-pvs-to-mu to the selected
;;s-forms.  The result is written to a file and run-program of
;;pvsmu is invoked on this file.  The result file is then read and
;;convert-mu-to-pvs is applied to it to get the result subgoal.

;;convert-pvs-to-mu just invokes convert-pvs-mu* after initializing
;;*mu-nu-lambda-bindings-list*.
;;convert-pvs-to-mu* is the real workhorse.
;; [P conn Q] ==> [P] conn [Q]
;; [NOT P] ==> NOT [P]
;; [IF P THEN Q ELSE R] ==> ([P]?[Q]:[R])
;; [a/=b] ==> NOT [a = b]
;; [(if A THEN b ELSE c) = d] ==> [if A THEN b = d ELSE c = d]
;; [d  = (if A THEN b ELSE c)] ==> [if A THEN d = b ELSE d = c]
;; a scalar constant, [a = b] ==> [a?(b)]
;;                    [b = a] ==> [a?(b)]
;; a, b of scalar type [a = b] ==> a1 = b1 and ...and an=bn
;; a, b of record type [a = b] ==> [l1(a)=l1(b)] and...and [ln(a)=ln(b)]
;; a of type [A->B], A is subrange[lo,hi], B is mu-translateable?, then
;;   [a = b] ==> [a(lo) = b(lo)] and ... and [a(hi) = b(hi)]
;;

(defvar *mu-nu-lambda-bindings-list* nil
  "lookup list containing vars that are bound by lambda operated on mu/nu")

;; given a pvs expr, it forms a basic expr-string that's bdd-isable and
;; a correspondng *recognizer-forms-list*: which is an assoc list such as:
;; ((x!1 (R . BVAR$1) (B . BVAR$2) (G . BVAR$3)) (y!1 (...)))
;; Then form excl/inclusives-strings:
;; enum-exclusives-string = ((NOT BVAR$1 + NOT (BVAR$3+BVAR$2)) & (NOT BVAR$2 +
;; NOT BVAR$3) & (corresponding to y!1))
;; enum-inclusives-string = find constructors of adt supertype of x!1,y!1
;;                          check if allof them occurr in recognizer-list and
;;                          then make a proper string if necessary.
;; then, the final bdd fmla to be sent is (expr-string |(exclusives&inclusives))
;; Boundary test cases considered: when there are no enums and when the
;;                                 enum type is a singleton - in which case
;;                                 enum-exclusives = nil, but 
;;                                 enum-inclusives /= nil

(defun make-enum-excl-incl-string ()
  (let* ((enum-exclusives
	  (loop for entry in *recognizer-forms-alist*
		for enum-exclusive-forms = (make-enum-exclusive-forms
					    (mapcar #'cdr (cdr entry)))
		unless (null enum-exclusive-forms)   ; throw out nils
		collect enum-exclusive-forms))
	 (enum-inclusives
	  (loop for entry in *recognizer-forms-alist*
		for enum-constructors =
		(length (constructors (adt (find-supertype (type (car entry))))))
		when (equal enum-constructors (length (cdr entry)))
		collect (make-bdd-disjunction (mapcar #'cdr (cdr entry)))
		;else
		;return nil
		))
	 (enum-exclusives-string
	  (make-bdd-conjunction enum-exclusives))
	 (enum-inclusives-string
	  (make-bdd-conjunction enum-inclusives))
	 (enum-excl-incl-string
	  (cond ((null enum-exclusives-string) enum-inclusives-string)
		((null enum-inclusives-string) enum-exclusives-string)
		(t (format nil "((~a)&(~a))" enum-exclusives-string
			   enum-inclusives-string)))))
    enum-excl-incl-string))

;; NB: exclusiv/inclusive context for encoded scalars is just the
;; disjunction of the encodings
;; discard entries which are mu/nu-lambda-bound
(defun make-incl-excl-string-for-encoded-scalars-in-recognizer-forms-alist
  ()
;;  (break)
  (let ((enum-inclusives
	 (loop for entry in *recognizer-forms-alist*
	       for enum-constructors =
	       (and (not (member (car entry) *mu-nu-lambda-bindings-list*
			    :test #'tc-eq))
		    (length (constructors (adt (find-supertype (type (car entry)))))))
	       when (equal enum-constructors (length (cdr entry)))
	       collect (make-bdd-disjunction (mapcar #'cdr (cdr entry)))
					;else
					;return nil
	       )))
    (make-bdd-conjunction enum-inclusives)))

(defun make-incl-excl-string-for-encoded-scalar (scalarvar)
;;  (break)
  (if (scalar? scalarvar)
      (let* ((recogzrs (recognizers (type scalarvar)))
	     (scalarvar (if ;(variable? scalarvar)
			    ;(declaration scalarvar)
			    (bind-decl? scalarvar)
			    (make-variable-expr scalarvar)
			   scalarvar))
	     (recapps (mapcar #'(lambda (x) (make-application x scalarvar))
			      recogzrs))
	     
	     (bddname0 (pvs-gethash scalarvar *pvs-bdd-hash*))
	     (scalar-encoding-list
	      (if (null bddname0)
		  (let ((newname
			 (intern (format nil "BVAR$~a"
				   (funcall *bdd-counter*)))))
		    (cadr
		     (setf (pvs-gethash scalarvar *pvs-bdd-hash*)
			   (list newname
				 (loop for i from 0 to
				       (- (length recogzrs) 1)
				       collect
				       (format nil "~a$$~a"
					 newname i))))))
		  (cadr bddname0)))
	     (encodings-list
	      (mapcar #'(lambda(x)
			  (enter-encoding-into-recognizer-form-alist x
								     scalar-encoding-list))
		      recapps)))
	(make-bdd-disjunction encodings-list))
      (if (recordtype? (type scalarvar))
	  (let* ((expr (if ;(variable? scalarvar)
			   ;(declaration scalarvar)
			 (bind-decl? scalarvar)
			 (make-variable-expr scalarvar)
			 scalarvar))
		 (fieldids (mapcar #'id (fields (type expr))))
		 (fieldapps (mapcar #'(lambda (x)
					(make-field-application x expr))
				    fieldids))
		 (fieldencodings
		  (loop for entry in fieldapps
			for entryencoding =
			(make-incl-excl-string-for-encoded-scalar entry)
			when entryencoding collect entryencoding)))
	    (make-bdd-conjunction fieldencodings))
		 
	  nil)))

(defun make-scalar-encoding-list (scalarvar)
  (if (scalar? scalarvar)
      (let* ((recogzrs (recognizers (type scalarvar)))
	     (scalarvar (if ;(variable? scalarvar)
			    ;(declaration scalarvar)
			    (bind-decl? scalarvar)
			    (make-variable-expr scalarvar)
			   scalarvar))
	     (recapps (mapcar #'(lambda (x) (make-application x scalarvar))
			      recogzrs))
	     
	     (bddname0 (pvs-gethash scalarvar *pvs-bdd-hash*))
	     (scalar-encoding-list
	      (if (null bddname0)
		  (let ((newname
			 (intern (format nil "BVAR$~a"
				   (funcall *bdd-counter*)))))
		    (cadr
		     (setf (pvs-gethash scalarvar *pvs-bdd-hash*)
			   (list newname
				 (loop for i from 0 to
				       (- (length recogzrs) 1)
				       collect
				       (format nil "~a$$~a"
					 newname i))))))
		  (cadr bddname0)))
	     (encodings-list
	      (mapcar #'(lambda(x)
			  (enter-encoding-into-recognizer-form-alist x
								     scalar-encoding-list))
		      recapps)))
	scalar-encoding-list)))
      
(defun convert-pvs-to-mu (expr) ;;expr must be of boolean type
  (let* ((*bound-variables* nil)
	 (*mu-nu-lambda-bindings-list* nil)
	 (expr-string (convert-pvs-to-mu* expr))
;        (enum-exclusives
;;;	 (break)
;         (loop for entry in *recognizer-forms-alist*
;	       for enum-exclusive-forms = (make-enum-exclusive-forms
;				     (mapcar #'cdr (cdr entry)))
;	       unless (null enum-exclusive-forms)   ; throw out nils
;	       collect enum-exclusive-forms))
;	(enum-inclusives
;	 (loop for entry in *recognizer-forms-alist*
;	       for enum-constructors =
;	       (length (constructors (adt (find-supertype (type (car entry))))))
;	       when (equal enum-constructors (length (cdr entry)))
;	       collect (make-bdd-disjunction (mapcar #'cdr (cdr entry)))
;	       ;else
;	       ;return nil
;	       ))
;	(enum-exclusives-string
;	 (make-bdd-conjunction enum-exclusives))
;	(enum-inclusives-string
;	 (make-bdd-conjunction enum-inclusives))
;	(enum-excl-incl-string
;	 (cond ((null enum-exclusives-string) enum-inclusives-string)
;	       ((null enum-inclusives-string) enum-exclusives-string)
;	       (t (format nil "((~a)&(~a))" enum-exclusives-string
;			  enum-inclusives-string))))
;        (enum-bdd-fmla
;	 (make-bdd-restriction expr-string
;			       enum-excl-incl-string))
	 )
    expr-string))

;; determine if expr is of the form "mu(P,PP)(Q)" or "nu(P,PP)(Q)"
;; determine if expr is of the form "mu(P,PP)(Q)" or "nu(P,PP)(Q)"
(defun mu-nu-expr-application? (expr)
  (if (application? expr)
      (let ((op (operator expr)))
	(and (application? op)
	     (typep (operator op) 'name-expr)
	     (find (string (id (operator op))) '("mu" "nu")
		   :test #'string=)
	     (eq (id (module
		      (declaration 
		       (operator op)))) '|mucalculus|)
	     (let
		 ((mu-actuals (actuals (module-instance
					(resolution (operator op))))))
	       (and (singleton? mu-actuals)
		    (or (mu-translateable?
			 (type-value (car mu-actuals)))
			(format t "~%Theory mucalculus with type ~a is not model-checkable."
			  (type-value (car mu-actuals))))))))
      nil))

;; determine if expr is of the form "mu(P,PP)" or "nu(P,PP)"
(defun mu-nu-expr? (expr)
  (if (application? expr)
      (let ((op (operator expr)))
	(and (typep op 'name-expr)
	     (find (string (id op)) '("mu" "nu") :test #'string=)
	     (eq (id (module (declaration op))) '|mucalculus|)
	     (let
		 ((mu-actuals (actuals (module-instance
					(resolution op)))))
	       (and (singleton? mu-actuals)
		    (or (mu-translateable?
			 (type-value (car mu-actuals)))
			(format t "~%Theory mucalculus with type ~a is not model-checkable."
			  (type-value (car mu-actuals))))))))
      nil))


(defun reachable? (expr)
  (and (name-expr? expr)
	      (eq (id expr) '|Reachable|)
	      (eq (id (module (declaration expr)))
		  '|Reachable|)))

(defun reachable-expr? (expr)
  (and (application? expr)
       (let ((op* (operator* expr)))
	 (reachable? op*))))
	 

;; enhanced with assignment of same bvars for name-expr and bind-decl
;(defun make-mu-variable (expr)
;    (let* ((expr (if (variable? expr)                ;; enhancement
;                     (declaration (resolution expr))  ;; to get same bvars
;                     expr))                           ;; for x in ALL x: P(x)
;	   (array? (boolean-array? (type expr)))
;	   (bddname (pvs-gethash expr *pvs-bdd-hash*)))
;      (cond ((null bddname)
;             (let ((newname
;                    (intern (format nil "BVAR$~a"
;                              (funcall *bdd-counter*))))
;		   )
;               (setf (pvs-gethash expr *pvs-bdd-hash*)
;                     newname)
;               (setf (gethash newname *bdd-pvs-hash*)
;                     expr)
;               (enter-into-recognizer-form-alist expr newname)
;	       (if array? 
;		   (loop for i from 0 to array?
;			 collect (format nil "~a$~a" newname i))
;		   (format nil "~a" newname))))
;            (t (enter-into-recognizer-form-alist expr bddname)
;	       (if array?
;		   (loop for i from 0 to array?
;			 collect (format nil "~a$~a" bddname i))
;		   (format nil "~a" bddname))))))

;;NSH(11.27.94) moved to assert.
;(defun scalar? (expr)
;  (and (enum-adt? (find-supertype (type expr))) expr))

;;asserts len-bit bitvect bvar to be less than or equal to number num
;;this is added as a restriction to the BDD.
(defun make-scalar-inclusive-formula (bvar num index)
  (if (zerop index)
      (if (logbitp 0 num)
	  (format nil "TRUE")
	  (format nil "(NOT ~a$$0)" bvar))
      (let ((bvarbit (format nil "~a$$~a" bvar index))
	    (bit (logbitp index num))
	    (rest-fmla
	     (make-scalar-inclusive-formula bvar num (1- index))))
	(if bit
	    (format nil "((NOT ~a) OR ~a)"
	      bvarbit rest-fmla)
	    (format nil "((NOT ~a) AND ~a)"
	      bvarbit rest-fmla)))))

(defun pad-false (list n)
  (if (zerop n)
      list
      (cons 'FALSE (pad-false list (1- n)))))
  

(defun make-geq-bdd (bddname-list number-rep)
  (let* ((bddname-list-len (length bddname-list))
	 (len (length number-rep))
	 (max-length (max bddname-list-len len))
	 (padded-reversed-bddname-list
	  (pad-false (reverse bddname-list)
		     (- max-length bddname-list-len)))
	 (padded-reversed-number-rep
	  (pad-false (reverse number-rep)
		     (- max-length len))))
    (make-geq-bdd* padded-reversed-bddname-list
		   padded-reversed-number-rep)))

(defun make-geq-bdd* (bdd-list num-rep)
  (if (consp num-rep)
      (if (consp (cdr num-rep))
	  (if (eq (car num-rep) 'TRUE)
	      (format nil "(~a AND ~a)" (car bdd-list)
		      (make-geq-bdd* (cdr bdd-list)(cdr num-rep)))
	      (format nil "(~a OR ~a)" (car bdd-list)
		      (make-geq-bdd* (cdr bdd-list)(cdr num-rep))))
	  (if (eq (car num-rep) 'TRUE)
	      (car bdd-list)
	      'TRUE))
      'TRUE))

(defun make-leq-bdd (bddname-list number-rep)
  (let* ((bddname-list-len (length bddname-list))
	 (len (length number-rep))
	 (max-length (max bddname-list-len len))
	 (padded-reversed-bddname-list
	  (pad-false (reverse bddname-list)
		     (- max-length bddname-list-len)))
	 (padded-reversed-number-rep
	  (pad-false (reverse number-rep)
		     (- max-length len))))
    (make-leq-bdd* padded-reversed-bddname-list
		   padded-reversed-number-rep)))

(defun make-leq-bdd* (bdd-list num-rep)
  (if (consp num-rep)
      (if (consp (cdr num-rep))
	  (if (eq (car num-rep) 'TRUE)
	      (format nil "(NOT ~a OR ~a)" (car bdd-list)
		      (make-leq-bdd* (cdr bdd-list)(cdr num-rep)))
	      (format nil "(NOT ~a AND ~a)" (car bdd-list)
		      (make-leq-bdd* (cdr bdd-list)(cdr num-rep))))
	  (if (eq (car num-rep) 'TRUE)
	      'TRUE
	      (format nil "NOT ~a" (car bdd-list))))
      'TRUE))

(defun make-subrange-inclusive-formula (bddname-list lo hi)
  (let* ((lo-rep (convert-number lo))
	(hi-rep (convert-number hi))
	(lower-rep (make-geq-bdd bddname-list lo-rep))
	(higher-rep (make-leq-bdd bddname-list hi-rep)))
    (make-bdd-conjunction (list lower-rep higher-rep))))
	

(defmethod convert-number (number)
  (let* ((len (max 1 (ceiling (log (1+ number) 2))))
	(format-bitstring (format nil "~~~d,'0b" len))
	(bit-string (format nil format-bitstring number)))
    (nreverse  ;lsb first: little-endian
     (loop for i from 0 to (1- len)
	   collect (if (eql (digit-char-p
			     (elt bit-string i)) 1)
		       'TRUE 'FALSE)))))


(defun make-subrange-names (expr &optional type)
  (let* ((type (if type type (type expr)))
	 (range (sub-range? type))
	 (lo (car range))
	 (hi (cdr range))
	 (len (max 1 (ceiling (log (1+ hi) 2))))
	 (bddname-hash
	  (pvs-gethash expr *pvs-bdd-hash*))
	 (bddname
	  (if bddname-hash
	      (car bddname-hash)
	      (intern (format nil "BVAR$~a"
			(funcall *bdd-counter*)))))
	 (bddname-list
	  (if bddname-hash
	      (cadr bddname-hash)
	      (loop for index from 0 to (1- len)
		    collect
		    (intern
		     (format nil "~a$$~a"
		       bddname index))))))
    (unless bddname-hash
      (setf (pvs-gethash expr *pvs-bdd-hash*)
	    (list bddname bddname-list))
      (pushnew (cons expr (make-subrange-inclusive-formula
			   bddname-list lo hi))
	       *pvs-bdd-inclusivity-formulas*
	       :test #'(lambda (x y)(tc-eq (car x)(car y)))))
	bddname-list))
	

(defun make-scalar-names (expr)
  (let* ((recs (recognizers
		(find-supertype (type expr))))
	 (len (max 1   ;;NSH(9.29.95) needed since log 1 2 = 0
		   (ceiling  ;;noticed by Nick Graham (York)
		    (log (length recs) 2))))
	 (bddname-hash
	  (pvs-gethash expr *pvs-bdd-hash*))
	 (bddname
	  (if bddname-hash
	      (car bddname-hash)
	      (intern (format nil "BVAR$~a"
			(funcall *bdd-counter*)))))
	 (bddname-list
	  (if bddname-hash
	      (cadr bddname-hash)
	      (loop for index from 0 to (1- len)
		    collect
		    (intern
		     (format nil "~a$$~a"
		       bddname index)))))
	 (format-bitstring (format nil "~~~d,'0b" len)))
    (unless bddname-hash
      (setf (pvs-gethash expr *pvs-bdd-hash*)
	    (list bddname bddname-list))
      (when (null (freevars expr))
	(loop for bddnm in (reverse bddname-list)
	      as i from 0
	      do
	      (let* ((irecs
		      (loop for rec in recs as j from 0
			    when
			    (eql (digit-char-p
				  (elt (format nil format-bitstring j)
				       i))
				 1)
			    collect
			    (make-application rec expr)))
		     (bdd-hashname
		      (make-disjunction irecs)))
		;;(format t "~%setting ~a to ~a"
		;;  bddnm bdd-hashname)
		(setf (gethash bddnm *bdd-pvs-hash*)
		      bdd-hashname))))
;      (if (member expr *pvs-bdd-inclusivity-formulas*
;		  :test #'tc-eq
;		  :key #'car)
;	  (break "~%Found duplicate inclusivity."))
      (pushnew (cons expr (make-scalar-inclusive-formula
			   bddname (1- (length recs)) (1- len)))
	       *pvs-bdd-inclusivity-formulas*
	       :test #'(lambda (x y)(tc-eq (car x)(car y)))))
    bddname-list))

(defun make-scalar-constant-bits (constant) ;;given a scalar constant
  (let* ((recog (recognizer constant))
	 (recs (recognizers
		(find-supertype (type constant))))
	 (len (ceiling
	       (log (length recs) 2)))
	 (bitlist (map 'list
		       #'(lambda (bit)
			   (if (eql (digit-char-p bit) 1)
			       "TRUE"
			       "FALSE"))
		       (format nil
			   (format nil "~~~d,'0b"
			     len)
			 (position recog recs :test #'tc-eq)))))
    (format nil "~{~a~^,~}" bitlist)))
		   

(defun make-mu-variable-recognizer-application (expr)
  (let* ((arg (args1 expr))
	 (op (operator expr))
	 (recs (recognizers
		(find-supertype (type arg))))
	 (len (ceiling
	       (log (length recs) 2)))
	 (bddname-list (reverse (convert-pvs-to-mu* arg)))
	 (format-bitstring (format nil "~~~d,'0b" len)))
    ;;NSH(6.1.96): moved reverse here from below.
    
    (make-bdd-conjunction
     (map 'list
	  #'(lambda (bit bddnm)
	      (if (eql (digit-char-p bit) 1)
	       (format nil "~a" bddnm)
	       (format nil "NOT ~a" bddnm)))
	  (format nil
	       format-bitstring
	     (position op recs :test #'tc-eq))
	   bddname-list))));;NSH(2.1.96); added reverse
;; Klaus noticed this unsoundness since the lsb was being set to the
;;msb of the position bit string.  


(defun make-mu-variable (expr)
  (let ((bddname (pvs-gethash expr *pvs-bdd-hash*)))
    (if bddname (if (consp bddname)(cadr bddname) bddname)
	(if (sub-range? (type expr))
	    (make-subrange-names expr)
	    (if (scalar? expr)
		(make-scalar-names expr)
		(let ((bddhash-name
		       (if (recognizer-application? expr)
			   (make-mu-variable-recognizer-application expr)
			   (intern (format nil "BVAR$~a"
				     (funcall *bdd-counter*))))))
		  ;;(format T "~%Translating ~s to ~s" expr bddhash-name)
		  (setf (pvs-gethash expr *pvs-bdd-hash*)
			bddhash-name)
		  (when (null (freevars expr))
		    ;;(format t "~% bdd-pvs-hashing ~a to ~a" bddhash-name expr)
		    (setf (gethash bddhash-name *bdd-pvs-hash*)
			  expr))
		  bddhash-name))))))

;;NSH(11.27.94): commenting out in favor of above defn.
;(defun make-mu-variable (expr)
;;  (when (not (tc-eq (type expr) *boolean*))
;;    (break "make-mu-var"))
;;  (when (and (or (application? expr)
;;		 (field-application? expr))
;;	     (or (update-expr? (argument expr))
;;		 (application? (argument expr))))
;;    (break "beta-reducible"))
;  (let ((bddname (pvs-gethash expr *pvs-bdd-hash*)))
;    (if bddname bddname
;	(let ((bddhash-name
;	       (if (recognizer-application? expr)
;		   (let* ((arg (args1 expr))
;			  (op (operator expr)))
;		     (make-bdd-conjunction
;		      (loop for rec in (recognizers
;				       (find-supertype (type arg)))
;			   as bddnm in
;			   (make-scalar-encoding-list arg)
;			   collect
;			   (if (tc-eq rec (operator expr))
;			       (format nil "NOT ~a" bddnm)
;			       bddnm))))
;		   (intern (format nil "BVAR$~a"
;					    (funcall *bdd-counter*))))))
;	  (setf (pvs-gethash expr *pvs-bdd-hash*)
;			   bddhash-name)
;	  (setf (gethash bddhash-name *bdd-pvs-hash*)
;		expr)
;	  bddhash-name))))


;; enhanced with assignment of same bvars for name-expr and bind-decl
;; enhanced to handle scalars
;(defun make-mu-variable (expr);;(break "mu-var")
;;  (when (and (variable? expr)
;;	     (not (member expr *bound-variables*
;;			  :test #'same-declaration)))
;;    (break "mu-var"))
;    (let* ((expr (cond ;((variable? expr) (declaration expr)) ;; enhancement
;			((bind-decl? expr) (make-variable-expr expr)) ;;to get same bvars
;			((and (field-application? expr) 
;			      (bind-decl? (argument expr))) ;; for x in
;			      (make-field-application
;			       (id expr) (argument expr)))  ;; ALL x: P(x)
;			((recognizer-application? expr)
;			 (let ((exprarg (args1 expr))
;			       (recop (operator
;				       (recognizer-application?
;					expr))))
;			   (if (and (field-application? exprarg)
;				    (bind-decl? (argument exprarg)))
;			       (make-application recop
;				 (make-field-application
;				  (id exprarg) (make-variable-expr
;						(argument exprarg))))
;			       (recognizer-application? expr))))
;			(t expr)))
;	   (array? (and (funtype? (type expr))
;			(mu-translateable? (type expr))))
;	   (recogzrs (cond
;		      ((scalar? expr) (recognizers (type expr)))
;		      ((recognizer-application? expr)
;		       (recognizers (find-supertype (type (args1 expr)))))
;		      (t nil)))
;	   (scalarbddname (and (recognizer-application? expr)
;			        (pvs-gethash (args1 expr) *pvs-bdd-hash*)))
;	   (bddname0 (pvs-gethash expr *pvs-bdd-hash*))
;	   (bddname (if (and bddname0 recogzrs)
;			(cadr bddname0)
;			bddname0))
;	   )
;;;      (break "in make-mu-var") 
;      (cond ((and (null bddname)
;		  (scalar-constant? expr))
;	     (let ((bit-list
;		    (loop for rec in
;			  (recognizers (find-supertype (type expr)))
;		   collect
;		   (if (tc-eq rec (recognizer expr))
;		       "FALSE"
;		       "TRUE"))))
;	       bit-list))
;	    ((null bddname)
;	     (let ((newname
;		    (intern (format nil "BVAR$~a"
;			      (funcall *bdd-counter*))))
;		   )
;	       (if (not recogzrs)
;		   (if (recordtype? (type expr))
;		       (let* ((fieldids (mapcar #'id
;						(fields (type expr))))
;			      (fieldapps (mapcar
;					  #'(lambda (x)
;					      (make-field-application x
;								      expr))
;					 fieldids))
;			      
;			      (fieldbvars
;			       ;(mapcar #'make-mu-variable fieldapps)
;			       (make-mu-variable* fieldapps)
;			       )
;			      )
;;;			 (break "recordtype")
;			 (setf (pvs-gethash expr *pvs-bdd-hash*)
;				 fieldbvars))
;		       (progn
;			 (setf (pvs-gethash expr *pvs-bdd-hash*)
;			       newname)
;			 (setf (gethash newname *bdd-pvs-hash*)
;			       expr)
;			 (enter-encoding-into-recognizer-form-alist expr
;								    newname)
;			 (if array?
;			     (loop for i from  0 to array? collect
;				   (format nil "~a$~a" newname i))
;			     (format nil "~a" newname))))
;
;		   (let ((scalar-encoding-list
;			  (if scalarbddname
;			      (cadr scalarbddname) ;; this is the root
;						   ;; name for scalar var
;			      (let ((expr (if (recognizer-application?
;					       expr)
;					      (args1 expr)
;					      expr))
;				    (temp (list newname
;					      (loop for i from 0 to
;						    (- (length recogzrs) 1)
;						    collect
;						    (format nil "~a$$~a"
;						      newname i)))))
;				(setf (pvs-gethash expr *pvs-bdd-hash*)
;				      temp)
;				  (cadr temp)))
;			      ))
;		     (if (scalar? expr)
;			  scalar-encoding-list
;			 (enter-encoding-into-recognizer-form-alist expr
;								    scalar-encoding-list))))))
;					;(format nil "~a" newname))))
;            (t (enter-encoding-into-recognizer-form-alist expr bddname)
;;;	       (break "here")
;;;	       (if (scalar? expr)
;;;		   (cadr (pvs-gethash expr *pvs-bdd-hash*))
;		   (if array?
;		       (loop for i from 0 to array?
;			     collect (format nil "~a$~a" bddname i))
;		       ;(format nil "~a" bddname)
;		       bddname)))))

;; negates a particular element in a list of boolean lits (symbols)
;; and forms a conjunction of the elements
(defun make-negate-pos-conjunction (pos litlist)
  (let ((string-list
	 (loop for str in litlist as i from 0
	       collect
	       (if (eq i pos)
		   (concatenate 'string "NOT " (string str))
		   str))))
    (make-bdd-conjunction string-list)))
		    
(defun enter-encoding-into-recognizer-form-alist (expr
						  scalar-encoding-list)
;;  (break)
  (let ((recexpr (recognizer-application? expr)))
    (when (not (null recexpr))
      (let* ((op (operator recexpr))
	     (arg (args1 recexpr))
	     (recogzrs (recognizers (type (args1 recexpr))))
	     (pos (position op recogzrs :test #'tc-eq))
	     (name (make-negate-pos-conjunction pos scalar-encoding-list))
	     (entry (assoc  arg *recognizer-forms-alist*
			    :test #'tc-eq)))
	(cond ((null entry)
	       (push (cons arg (list (cons op name)))
		     *recognizer-forms-alist*)
	       name
	       )
	      (t (pushnew (cons op name)
			  (cdr entry)
			  :test #'(lambda(x y) (tc-eq (car x) (car y))))
		 name
		 )))))
;;  (print *recognizer-forms-alist*)
  )

(defun make-mu-variable* (list &optional accum)
  (cond ((null list)
	 (nreverse accum))
	(t (let ((answer (make-mu-variable (car list))))
	     (make-mu-variable* (cdr list)
				(if (consp answer)
				    (append (reverse answer) accum)
				    (cons answer accum)))))))

(defun subrange!? (type)
  (let* ((subrange (type-value (declaration (subrange-res))))
	 (*modsubst* T)
	 (subst (match subrange type nil nil))) ;(break)
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons (number (cdr (assoc '|i| subst :test #'same-id)))
	    (number (cdr (assoc '|j| subst :test #'same-id)))))))
	

(defun below!? (type)
  (let* ((below (type-value (declaration (below-res))))
	 (*modsubst* T)
	 (subst (match below type nil nil)))
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons 0 (1- (number
		   (cdr (assoc '|i| subst :test #'same-id))))))))

(defun upto!? (type)
  (let* ((upto (type-value (declaration (upto-res))))
	 (*modsubst* T)
	 (subst (match upto type nil nil)))
    (when (and (not (eq subst 'fail))
	       (every #'(lambda (x)(number-expr? (cdr x))) subst))
      (cons 0 (number (cdr (assoc '|i| subst :test #'same-id)))))))

(defun sub-range? (type)
  (or (subrange!? type)
      (below!? type)
      (upto!? type)))
    
    
;  (with-slots ((ptype print-type)) type
;    (or (and (type-application? ptype)
;	     (memq (id (type ptype)) '(|upto| |below| |subrange|))
;	     (every #'number-expr? (parameters ptype))
;	     (id (type ptype)))
;	(and (type-name? ptype)
;	     (memq (id ptype) '(|upto| |below| |subrange|))
;	     (actuals ptype)
;	     (every #'(lambda (x) (number-expr? (expr x)))
;		     (actuals ptype))
;	     (id ptype)))))
;
;(defun range-subrange (type)
;  
;  (with-slots ((ptype print-type)) type
;    (let ((subrange-kind (subrange? type)))
;      (if (type-application? ptype)
;	  (if (eq subrange-kind '|upto|)
;	      (cons 0 (number (car (parameters ptype))))
;	      (if (eq subrange-kind '|below|)
;		  (cons 0 (1- (number (car (parameters ptype)))))
;		  (cons (number (car (parameters ptype)))
;			(number (cadr (parameters ptype))))))
;	  (if (eq subrange-kind '|upto|)
;	      (cons 0 (number (expr (car (actuals ptype)))))
;	      (if (eq subrange-kind '|below|)
;		  (cons 0 (1- (number (expr (car (actuals ptype))))))
;		  (cons (number (expr (car (actuals ptype))))
;			(number (expr (cadr (parameters ptype)))))))))))

(defmethod mu-translateable? ((type type-name))
  (or (tc-eq type *boolean*)
      (and (adt type)
	   (mu-translateable? (adt type)))))

(defmethod mu-translateable? ((type enumtype))
  T)

(defmethod mu-translateable? ((type recordtype))
  (every #'(lambda (x) (mu-translateable? (type x)))
	 (fields type)))

(defmethod mu-translateable? ((type funtype))
  (and (sub-range? (domain type))
       (mu-translateable? (range type))))

(defmethod mu-translateable? ((type subtype))
  (or (sub-range? type)
      (mu-translateable? (supertype type))))

(defmethod mu-translateable? ((type T))
  NIL)


(defun boolean-array? (type)
;;  (break)
  (and (funtype? type)
       (let ((ptype
	      ;(and
	       ;(consp (domain type))
			  (print-type
			   (domain type))
			   ;(car (domain type))
			   ;))
	     ))
	 (and (type-name? ptype)
	      (tc-eq (range type) *boolean*)
	      (memq (id ptype) '(|upto| |below|))
	      (number-expr? (expr (car (actuals (module-instance ptype)))))
	      (if (eq (id ptype) '|upto|)
		  (number (expr (car (actuals (module-instance ptype)))))
		  (1- (number (expr (car (actuals (module-instance ptype)))))))))))
		 
		 

;; makes a bdd-comma-separated-arguments string from a list - creates new bvars
;; Example on how to deal with a non-commutative operator
;; (a b c) ==> "BVAR$?, BVAR$??, BVAR$???"
;; () ==> nil
(defun make-bdd-comma-separated-arguments (list)
;;  (break)
  (let ((bvarlist (convert-pvs-to-mu* list)))
    (cond ((null bvarlist) nil)
	(t (format nil "~{~a~^,~}" bvarlist)))))

(defun boolean? (term)
  (tc-eq (type term) *boolean*))

(defun all-not-boolean? (term-list)
  (find-if-not #'(lambda (x) (or (tc-eq (type x) *boolean*)
				 (boolean-array? (type x))))
    term-list))

(defun all-boolean? (term-list)
  (not (all-not-boolean? term-list)))

;; methods to produce uncurried forms from curried forms
(defmethod get-op ((expr application))
  (let ((op (operator expr)))
    (get-op op)))

(defmethod get-op ((expr expr))
  expr)

(defmethod get-args ((expr application))
  (let* ((args (arguments expr))
	 (op (operator expr))
	 (opargs (get-args op)))
    (append opargs args)))

(defmethod get-args ((expr expr))
  nil)

(defun uncurry-with-string-output (expr)
  (let* ((*lift-if-updates* T)
	 (old-expr expr)
	 (expr (lift-if-expr expr)))
    (if (eq expr old-expr)
	(if (recognizer-application? expr)
	    (make-mu-variable expr)
	    (if (application? expr)
		(let ((op* (get-op expr));;NSH(4.5.95) extensions to handle
		      ;;more complex expressions.
		      (args* (get-args expr)))
		  (if (and (every #'(lambda (x) (mu-translateable? (type x)))
				  args*)
			   (or (lambda-expr? op*)
			       (and (variable? op*)
				    (member op* *mu-nu-lambda-bindings-list*
					    :test #'same-declaration))))
		      (let ((op-str (convert-pvs-to-mu* op*))
			    (args-str (format nil "~{~a~^,~}"
					(convert-pvs-to-mu* args*))))
			(format nil "~a(~a)" op-str args-str))
		      (if (and (mu-translateable? (type (operator expr)))
			       (not (number-expr? (argument expr))))
			  (let* ((arg-str (convert-pvs-to-mu* (argument expr)))
				 (arg-str (if (listp arg-str) arg-str
					      (list arg-str))))
			    (decode-array (operator expr) (nreverse arg-str)))
			  (make-mu-variable expr))))
		(make-mu-variable expr)))
	(convert-pvs-to-mu* expr))))


(defun decode-array (op args)
  (let* ((format-string (decode-array-format args))
	 (oplist
	  (loop for i from 0 to (1- (expt 2 (length args)))
		collect
		(convert-pvs-to-mu*
		 (make-application op
		   (make-number-expr i)))))
	 (len (if (consp oplist)
		  (if (consp (car oplist))
		      (length (car oplist))
		      1)
		  0)))
    (loop for j from 0 to (1- len)
	  collect
	  (apply #'format (cons nil
				(cons format-string
				      (mapcar #'(lambda (x)(nth j x))
					oplist)))))
    ))
					  
    

(defun decode-array-format (args)
  (if (consp args)
      (format nil "(~a ? ~a : ~a)" (car args)
	      (decode-array-format (cdr args))
	      (decode-array-format (cdr args) ))
      "~a"))

(defun decode-function (op args &optional (count 0) )
  (if (consp args)
      (if (eq (car args) 'TRUE)
	  (decode-function op (cdr args) (1+ (* 2 count)))
	  (if (eq (car args) 'FALSE)
	      (decode-function op (cdr args) (* 2 count))
	      (format nil "~a ? ~a : ~a" (car args)
		      (decode-function op (cdr args) (1+ (* 2 count)))
		      (decode-function op (cdr args) (* 2 count)))))
      (make-mu-variable (make-application op
			    (make-number-expr count)))))

(defmethod convert-pvs-to-mu* :around ((expr expr))
  (let ((type (type expr)))
    (when (and (subtype? type)
	       (not (member expr *mu-subtype-list* :test #'tc-eq))
	       (not (assoc expr *pvs-bdd-inclusivity-formulas*
			   :test #'tc-eq)))
      (let ((constraints (collect-type-constraints-step expr))
	    (*mu-subtype-list* (cons expr *mu-subtype-list*)))
	(loop for x in constraints do
	      (push (cons expr (convert-pvs-to-mu* x))
		    *pvs-bdd-inclusivity-formulas*))
	(format t "~%Added constraints: ~a" constraints)))
    (call-next-method)))
		 

(defmethod convert-pvs-to-mu* ((expr list))
  (loop for x in expr
	append (let ((result (convert-pvs-to-mu* x)))
		(if (listp result)
		    result
		    (list result)))))

(defmethod convert-pvs-to-mu* ((expr cases-expr))
  (convert-pvs-to-mu* (translate-cases-to-if expr)))

(defun bool-ineq? (expr);;assuming expr is application
  (let ((op (operator expr)))
    (and (name-expr? op)
	 (memq (id op) '(< <= > >=))
	 (eq (mod-id (module-instance (resolution op)))
	     '|bv_arith_nat|)
	 (number-expr? (expr (car (actuals (module-instance (resolution op))))))
	 (id op))))


(defmethod convert-pvs-to-mu* ((expr application))
  ;    (when (sub-range? (domain (type (operator expr))))
;	   (break "sub-range-domain"))
  (let* ((old-expr expr)
	 (*lift-if-updates* T)
	 (expr (if (and (boolean? expr)
			(equality? expr))
		   (lift-if-expr expr)
		   expr)))
    (cond
     ((or (disjunction? expr)(conjunction? expr)
	  (iff? expr)(implication? expr)) ; includes  WHEN 
      (let ((str1 (convert-pvs-to-mu* (args1 expr)))
	    (str2 (convert-pvs-to-mu* (args2 expr))))
	(cond ((disjunction? expr)
	       (format nil "(~a+~a)" str1 str2))
	      ((implication? expr)
	       (format nil "(~a implies ~a)" str1 str2))
	      ((conjunction? expr)
	       (format nil "(~a&~a)" str1 str2))
	      ((iff? expr)
	       (format nil "(~a equiv ~a)" str1 str2)))));; should this be = or xnor?
     ((not-expr? expr)
      (format nil "NOT (~a)" (convert-pvs-to-mu* (args1 expr))))
     ((branch? expr) 
      (let ((result (assert-test0 (condition expr))))
	(cond ((eq result 'TRUE)
	       (convert-pvs-to-mu* (then-part expr)))
	      ((eq result 'FALSE)
	       (convert-pvs-to-mu* (else-part expr)))
	      (t 
	       (let ((str1 (convert-pvs-to-mu* (condition expr)))
		     (str2 (convert-pvs-to-mu* (then-part expr)))
		     (str3 (convert-pvs-to-mu* (else-part expr))))
		 (format nil "(~a?~a:~a)"
		   str1 str2 str3))))))
     ((inequality? expr)
      (format nil "NOT (~a)"
	(convert-pvs-to-mu*
	 (make-equality (args1 expr)(args2 expr)))))
     ((and (equality? expr)
	   (funtype? (find-supertype (type (args1 expr))))
	   (mu-translateable? (type (args1 expr))))
      (let* ((dtype (domain (find-supertype (type (args1 expr)))))
	     (x (sub-range? dtype))
	     (lo (car x))
	     (hi (cdr x))
	     (arg1 (args1 expr))
	     (arg2 (args2 expr)))
      (make-bdd-conjunction
       (loop for i from lo to hi
	     collect (convert-pvs-to-mu*
		      (make-equality
		       (make-application arg1 (make-number-expr i))
		       (make-application arg2 (make-number-expr i))))))))
	   
     ((and (equality? expr)
	   (or (branch? (args1 expr));;NSH(2.2.96): added cases?
	       (cases-expr? (args1 expr))));;to this and next case, else
      (let ((arg1 (if (cases-expr? (args1 expr));;(cases ..) = e ignored.
		      (translate-cases-to-if (args1 expr))
		      (args1 expr))))
	(convert-pvs-to-mu*
	 (make-if-expr (condition arg1)
		       (make-equality (then-part arg1)
				      (args2 expr))
		       (make-equality (else-part arg1)
				      (args2 expr))))))
     ((and (equality? expr)
	   (or (branch? (args2 expr))
	       (cases-expr? (args2 expr))))
      (let ((arg2 (if (cases-expr? (args2 expr))
		      (translate-cases-to-if (args2 expr))
		      (args2 expr))))
	(convert-pvs-to-mu*
	 (make-if-expr (condition arg2)
		       (make-equality (args1 expr)
				      (then-part arg2))
		       (make-equality (args1 expr)
				      (else-part arg2))))))
     ((and (equality? expr)
	   (or (scalar-constant? (args1 expr))
	       (scalar-constant? (args2 expr))))
      (convert-pvs-to-mu*
       (if (scalar-constant? (args1 expr))
	   (make-application
	       (recognizer (args1 expr))
	     (args2 expr))
	   (make-application
	       (recognizer (args2 expr))
	     (args1 expr)))))
     ((and
       ;;(not (application? (operator expr)))
       (equality? expr)
       ;;(eq (id (operator expr)) '|=|) ;; equality on scalars
       (scalar? (args1 expr)))
      (let* ((exprarg1 (args1 expr))
	     (exprarg2 (args2 expr))
	     (scalar1
	      (if (bind-decl? exprarg1)
		  (make-variable-expr exprarg1)
		  exprarg1))
	     (scalar2
	      (if (bind-decl? exprarg2)
		  (make-variable-expr exprarg2)
		  exprarg2))
	     (muvarlist1 (make-scalar-names scalar1))
					;(make-scalar-encoding-list scalar1)
	     (muvarlist2 (make-scalar-names scalar2))
					;(make-scalar-encoding-list scalar2)
	     );;(break "scalar")
	(make-bdd-conjunction
	 (mapcar #'(lambda (x y) (format nil "(~a equiv ~a)" x y))
	   muvarlist1 muvarlist2))))
     ((and (equality? expr);;NSH(8.17.95)find-supertypes added.
	   (recordtype? (find-supertype (type (args1 expr))))
	   ;;(not (branch? (args1 expr)))
	   ;;(not (branch? (args2 expr)))
	   )
      (let* ((type (find-supertype (type (args1 expr))))
	     (fields (fields type))
	     (args1-list
	      (loop for decl in fields
		    collect (beta-reduce
			     (make-field-application
			      (id decl) (args1 expr)))))
	     (args2-list
	      (loop for decl in fields
		    collect (beta-reduce
			     (make-field-application
			      (id decl) (args2 expr)))))
	     (equality-bdds
	      (loop for x in args1-list
		    as y in args2-list
		    collect
		    (convert-pvs-to-mu*
		     (make-equality x y)))))
	(make-bdd-conjunction equality-bdds)))
;;;;;;;
     ((and (equality? expr)
	   (funtype? (find-supertype (type (args1 expr))))
	   (mu-translateable? (find-supertype (type (args1 expr)))))
      (let* ((type (find-supertype (type (args1 expr))))
	     (range (sub-range? (domain type)))
	     (lo (car range))
	     (hi (cdr range))
	     (args1-list
	      (convert-pvs-to-mu* (args1 expr)))
	     ;;		 (loop for i from lo to hi
	     ;;		       collect (beta-reduce
	     ;;				(make-application
	     ;;				    (args1 expr)
	     ;;				  (make-number-expr i))))
	     (args2-list
	      (convert-pvs-to-mu* (args2 expr)))
	     ;;		 (loop for i from lo to hi
	     ;;		       collect (beta-reduce
	     ;;				(make-application
	     ;;				    (args2 expr)
	     ;;				  (make-number-expr i))))
	     (equality-bdds
	      (loop for x in args1-list
		    as y in args2-list
		    collect
		    (format nil "~a equiv ~a" x y))))
	(make-bdd-conjunction equality-bdds)))
     ((and (equality? expr)
	   (sub-range? (type (args1 expr))))
      (let* ((args1-atoms (convert-pvs-to-mu* (args1 expr)))
	     (args2-atoms (convert-pvs-to-mu* (args2 expr)))
	     (equalities (loop for x in args1-atoms
			       as y in args2-atoms
			       collect
			       (format nil "~a equiv ~a" x y))))
	(make-bdd-conjunction equalities)))
     ;;	((bool-ineq? expr);NSH(11.18.95) needs to be implemented.
     ;;	 ())
     ((mu-nu-expr-application? expr)
      ;;(break)
      (let* ((exprargs (arguments expr))
	     (argsstr (make-bdd-comma-separated-arguments exprargs))
	     (exprargs-enum-excl-incl-str
	      (make-bdd-conjunction
	       (loop for entry in exprargs
		     for entryencoding =
		     (make-incl-excl-string-for-encoded-scalar entry)
		     when entryencoding collect entryencoding)))
	     (muop (operator expr))
	     (mu-or-nu (string (id (operator muop))))
	     (muargs1bindgs (bindings (args1 muop)));;args1muop islambda-expr
	     (*mu-nu-lambda-bindings-list*
	      (append (bindings (args1 muop))
		      *mu-nu-lambda-bindings-list*))
	     (muarg1str (make-bdd-comma-separated-arguments
			 (mapcar #'make-variable-expr muargs1bindgs)) )
	     (muarg2expr (expression (args1 muop)))
	     (muarg2exprbindgs (bindings muarg2expr));; islambda expr
	     (muarg2str (convert-pvs-to-mu* muarg2expr))
	     (muexprstr
	      (format nil "[~a ~a. ~a](~a)" mu-or-nu muarg1str muarg2str
		      argsstr)))
	;;(make-bdd-restriction
	muexprstr
	;;exprargs-enum-excl-incl-str)
	))
     ((mu-nu-expr? expr)
      ;;(break)
      (let* ((muop (operator expr))
	     (mu-or-nu (string (id muop)))
	     (muargs1bindgs (bindings (args1 expr)));;args1muop islambda-expr
	     (muarg1str (make-bdd-comma-separated-arguments
			 (mapcar #'make-variable-expr muargs1bindgs)))
	     (*mu-nu-lambda-bindings-list*;;NSH(6.23.95)
	      (append muargs1bindgs
		      *mu-nu-lambda-bindings-list*))
	     (muarg2expr (expression (args1 expr)))
	     (muarg2exprbindgs (bindings muarg2expr));; islambda expr
	     (muarg2str (convert-pvs-to-mu* muarg2expr))
	     (muexprstr
	      (format nil "[~a ~a. ~a]" mu-or-nu muarg1str muarg2str
		      )))
	muexprstr))
     ((reachable-expr? expr)
      (format nil "~a(~a)"
	(convert-pvs-to-mu* (operator expr))
	(make-bdd-comma-separated-arguments (arguments expr))))
					;	((and (variable? (operator expr))
					;	      (member (operator expr) *bound-variables*
					;		      :test #'same-declaration)
					;	      (boolean-array? (type (operator expr)))
					;	      (number-expr? (args1 expr)))
					;	 (format nil "~a$~a"
					;	   (pvs-gethash (car (member (operator expr) *bound-variables*
					;				     :test #'same-declaration))
					;			*pvs-bdd-hash*)
					;	   (number (args1 expr))))
     ((NOT (tc-eq (find-supertype (type expr)) *boolean*))
      (call-next-method));;invoke expr case.
     (t;; Other application like R(x,y)
	 
      (uncurry-with-string-output expr)
					; (make-mu-variable expr)
      ))))




	   ;(if (all-boolean? (get-args expr)) ;; decompose only for boolean binds
	   ;  (let ((argsstr (make-bdd-comma-separated-arguments (arguments expr)))
	   ;	   (opstr (string
	   ;		   (make-mu-variable (operator expr))
	   ; might be enhanced later
	   ; (format nil "~a(~a)" opstr argsstr)

(defmethod convert-pvs-to-mu* ((expr expr))  ;;NSH(8.17.95) added find-supertype
;;  (break)  
  (cond ((tc-eq expr *true*) "TRUE")
	((tc-eq expr *false*) "FALSE")
	((reachable? expr) "Reachable")
	((and (binding-expr? expr)
	      (every #'(lambda (x)(mu-translateable? (find-supertype (type x))))
		     (bindings expr)) ;;NSH(4.24.95)
	      ;(all-boolean? (bindings expr))
	      )
	 (let* ((expr-bindings (bindings expr))
		(expr-bindings
		 (mapcar #'make-variable-expr expr-bindings))
		(old-inclusivities *pvs-bdd-inclusivity-formulas*)
		(boundvarsstr (make-bdd-comma-separated-arguments
				 expr-bindings))
		(*bound-variables* (append expr-bindings
					   *bound-variables*))
		(new-inclusivities
		 (ldiff *pvs-bdd-inclusivity-formulas*
			old-inclusivities))
		(boundexprstr
		 ;(make-bdd-restriction)
		  (convert-pvs-to-mu* (expression expr))
		  ;(make-bdd-conjunction
		;;   (mapcar #'cdr new-inclusivities));
		))
	   (setq *pvs-bdd-inclusivity-formulas*  ;;NSH(8.17.95): to collect
		                            ;;subtypes from inside binding expr.
		 (set-difference *pvs-bdd-inclusivity-formulas*
				 new-inclusivities
				 :key #'car
				 :test #'tc-eq))
	   (cond ((forall-expr? expr)
		  (if new-inclusivities
		      (format nil "~a(A ~a. ~a -> ~a)~a" #\newline boundvarsstr
			  (make-bdd-conjunction
			   (mapcar #'cdr new-inclusivities))
			  boundexprstr #\newline)
		    (format nil "~a(A ~a. ~a)~a" #\newline boundvarsstr
			    boundexprstr #\newline)))
		 ((exists-expr? expr)
		  (if new-inclusivities
		      (format nil "~a(E ~a. ~a AND ~a)~a" #\newline boundvarsstr
			      (make-bdd-conjunction
			       (mapcar #'cdr new-inclusivities))
			      boundexprstr #\newline)
		  (format nil "~a(E ~a. ~a)~a" #\newline boundvarsstr
			  boundexprstr #\newline)))
		 ((lambda-expr? expr)
		  (format nil "~a[L ~a. ~a]~a" #\newline boundvarsstr
			  boundexprstr #\newline)))))
	((scalar-constant? expr) ;;NSH(4.24.95)
	 (make-scalar-constant-bits expr)) ;;NSH(6.11.95)
	((scalar?  expr)
	 (uncurry-with-string-output expr))
	((recordtype? (find-supertype (type expr))) ;;(break)
	 (convert-pvs-to-mu*
	  (loop for decl in (fields (find-supertype (type expr)))
		       collect (beta-reduce
				(make-field-application
				 (id decl) expr)))))
	((and (funtype? (find-supertype (type expr)))
	      (mu-translateable? (find-supertype (type expr))))
	 (let* ((range (sub-range? (domain (type expr))))
		(lo (car range))
		(hi (cdr range))
		(args-list
		 (loop for i from lo to hi
		       collect (beta-reduce
				(make-application
				    expr
				  (make-number-expr i))))))
	   (convert-pvs-to-mu* args-list)))
	(t ;;(when (not (tc-eq (type expr) *boolean*))
;;	     (break)) ;;NSH(11.22.94): should only false for
	             ;; quantified variable Q.
	   (make-mu-variable expr))))

(defmethod convert-pvs-to-mu* ((expr number-expr))
  (convert-number (number expr)))

(defun convert-mu-to-pvs (bdd-string)
  (let* ((pvs-string (bdd2pvs bdd-string))
	 (parsed-string (pc-parse pvs-string 'expr))
	 (pvs-form (termsubst parsed-string
			     #'(lambda (ex)
				 (gethash (id ex) *bdd-pvs-hash*))
			     #'(lambda (ex)
				 (and (name-expr? ex)
				      (gethash (id ex) *bdd-pvs-hash*)))))
	 (tc-pvs-form (with-no-type-errors
		       (typecheck pvs-form :expected *boolean*))))
    (or tc-pvs-form
	(format t "~%Failed to Model check:
    could not decode binary encodings of scalars."))))
