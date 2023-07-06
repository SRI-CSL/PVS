;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; translate-to-smt2.lisp -- 
;; Author          : K. Nukala and N. Shankar
;; Created On      : June 2023
;; Last Modified By: K. Nukala
;; Last Modified On: 07/03/2023
;; Update Count    : 4
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

;;NSH(12/5/10): Adapting translate-to-prove-yices to yices2 output. 

(defvar *smt2bindings* nil)
(defvar *smt2embeddedf* nil)
(defvar *smt2defns* nil)
;;(defvar *smt2datatype-warning* nil)
(defvar *smt2name-hash* (make-pvs-hash-table))
(defvar *translate-to-smt2-hash* (make-pvs-hash-table))
(defvar *smt2-executable* nil)
(defvar *smt2-flags* "--mode=one-shot")
(defvar *smt2-id-counter*)  ;;needs to be initialized in eproofcheck
(defvar *smt2-conditions* nil)
(defvar *smt2-subtype-constraints* nil)

(newcounter *smt2-id-counter*)

(defun clear-smt2 ()
  (setq *smt2defns* nil)
  (clrhash *smt2name-hash*)
  (clrhash *translate-to-smt2-hash*)
  (newcounter *smt2-id-counter*))

(defun smt2-name (expr &optional id) ;;expr must have id field,
  ;;or be given one
  (if (typep expr '(or dep-binding field-decl)) (or id (id expr))
      (let ((entry (gethash expr *y2name-hash*)))
	(or entry
	    (let ((name (smt2-id-name (or id (id expr)))))
	      (setf (gethash expr *y2name-hash*) name)
	      name)))))

(defun smt2-id-name (id)
  (intern
   (concatenate 'string
		(string (if (integerp id)
			    (format nil "~r"
				    id)
			    id))
		"_"
		(princ-to-string
		 (funcall
		  *smt2-id-counter*))) :pvs))

(defun smt2-type-name (expr)
  (smt2-name expr))

(defmethod translate-to-smt* :around ((obj type-expr) bindings)
  (declare (ignore bindings))
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let ((result (call-next-method)))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      result)))))

(defmethod translate-to-smt2* ((ty type-name) bindings)
  (declare (ignore bindings))
  (let ((smt2name-hash (gethash ty *smt2name-hash*)))
    (or smt2name-hash 
	(cond ((enum-adt? ty)
	       ;; (let ((constructors (constructors ty))
	       ;; 	     (yname (yices2-name ty)))	;;bindings can be ignored
	       (error "yices2 tranlation does not support scalars")
	       ;;(translate-to-yices2-scalar yname constructors))
	       )
	      ((tc-eq (find-supertype ty) *boolean*)
	       (format nil "Bool"))
	      ((tc-eq ty *number*) "Real")
	      ;;else uninterpreted type
	      (t (let ((smt2name (smt2-name ty)))
		   (push (format nil "(declare-sort ~a)" smt2name) *smt2defns*)
		   smt2name))))))

(defmethod translate-to-smt2* ((ty subtype) bindings)
  (with-slots (supertype predicate) ty
    (cond ;;((tc-eq ty *naturalnumber*) 'nat) ;;there is no nat
      ((tc-eq ty *integer*) "Int")
      ((tc-eq ty *real*) "Real")
      (t (translate-to-smt2* supertype bindings)))))

(defmethod translate-to-smt* ((ty tupletype) bindings)
  (with-slots (types) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* types bindings))))

(defmethod translate-to-smt2* ((ty recordtype) bindings)
  (with-slots (fields) ty
    (format nil "(Prod ~{~a ~})"
	    (translate-to-smt2* fields bindings))))

(defmethod translate-to-smt2* ((ty field-decl) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty dep-binding) bindings)
  (translate-to-smt2* (type ty) bindings))

(defmethod translate-to-smt2* ((ty funtype) bindings)
  (with-slots (domain range) ty
    (format nil "(Array ~a ~a)"
	    (translate-to-smt2* domain bindings)
	    (translate-to-smt2* range bindings))))


(defmethod translate-to-smt2* ((list list) bindings)
  (cond ((consp list)
	 (cons (translate-to-smt2* (car list) bindings)
	       (translate-to-smt2* (cdr list) bindings)))
	(t nil)))


(defmethod translate-to-smt2* :around ((obj expr) bindings)
  (if (or *bound-variables* *bindings*)
      (call-next-method)
      (let ((hashed-value (gethash obj *translate-to-smt2-hash*)))
	(or hashed-value
	    (let* ((result (call-next-method))
		   (type-constraints (type-constraints obj :none))
		   (rtype-constraints (loop for fmla in type-constraints
					    nconc (and+ fmla))))
	      (setf (gethash obj *translate-to-smt2-hash*)
		    result)
	      (loop for tc in rtype-constraints
		    do (let* ((ytc (translate-to-smt2* tc bindings))
			      (yclause (if *smt2-conditions*
					   (format nil "(assert (implies (and ~{ ~a~}) ~a))"
						   *smt2-conditions*
						   ytc)
					   (format nil "(assert ~a)" ytc))))
			 (push yclause *smt2-subtype-constraints*)))
	      result)))))

(defun smt2-recognizer (name bindings)
  (when (recognizer? name)
    (translate-to-smt2* (type name) bindings)
    (format nil "~a?" (translate-to-smt2* (constructor name) bindings) )))

(defmethod translate-to-smt2* ((expr name-expr) bindings)
  (let ((bpos (assoc expr bindings
		     :test #'same-declaration)))
    (if bpos (if (consp (cdr bpos))
		 (format nil "(Prod ~{ ~a~})"  (cdr bpos))
		 (cdr bpos))
	(let* ((smt2name-hashentry (gethash expr *smt2name-hash*)))
	  (or smt2name-hashentry
	      (smt2-interpretation expr)
					;(eta-expanded-smt2-interpretation expr)
	      (smt2-recognizer expr bindings)
	      (let* ((smt2type (translate-to-smt2* (type expr)
						   bindings))
		     (smt2name-hashentry (gethash expr *smt2name-hash*)))
		(or smt2name-hashentry
		    (let* ((smt2name (smt2-name expr))
			   (defn (format nil "(declare-const ~a ~a)"
					 smt2name
					 smt2type)))
		      (push defn
			    *smt2defns*)
		      (format-if "~%Adding definition: ~a" defn)
		      smt2name))))))))


(defmethod translate-to-smt2* ((expr constructor-name-expr) bindings)
  (call-next-method (lift-adt expr) bindings))

(defmethod translate-to-smt2* ((expr rational-expr) bindings)
  (declare (ignore bindings))
  (number expr))

(defmethod translate-to-smt2* ((ex string-expr) bindings)
  (declare (ignore bindings))
  (string->int (string-value ex)))

(defmethod translate-to-smt2* ((expr record-expr) bindings)
  (with-slots (assignments) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (sort-assignments (assignments expr)) bindings))))

(defmethod translate-to-smt2* ((expr tuple-expr) bindings)
  (with-slots (exprs) expr
    (format nil "(tuple ~{ ~a~})"
	    (translate-to-smt2* (exprs expr) bindings))))

(defmethod translate-to-smt2* ((expr branch) bindings)
  (let ((smt2condition (translate-to-smt2* (condition expr) bindings)))
    (format nil "(ite ~a ~a ~a)"
	    smt2condition
	    (let ((*smt2-conditions* (push smt2condition *smt2-conditions*)))
	      (translate-to-smt2* (then-part expr) bindings))
	    (let ((*smt2-conditions* (push `(not ,smt2condition) *smt2-conditions*)))
	      (translate-to-smt2* (else-part expr) bindings)))))

(defmethod translate-to-smt2* ((expr projection-expr) bindings)
  (break "Can't translate standalone projections"))

(defmethod translate-to-smt2* ((expr projection-application) bindings)
  (with-slots (argument index) expr
    (if (variable? argument)
	(let ((bnd (assoc argument bindings
			  :test #'same-declaration)))
	  (if (and bnd (consp (cdr bnd)))
	      (nth (1- index) (cdr bnd))
	      (format nil "((_ project ~a) ~a)"
		      (1+ (index expr))
		      (translate-to-yices2* argument bindings))))
	(format nil "((_ project ~a) ~a)"
		(1+ (index expr))
		(translate-to-yices2* argument bindings)))))


;; why do we let-bind fields to simply sfields?
(defmethod translate-to-smt2* ((expr field-application) bindings)
  (with-slots (id argument type) expr
    (let* ((fields (fields (find-supertype (type argument))))
	   ;; (sfields   fields)
	   (pos (position id fields ;; sfields
			  :key #'id)))
      (format nil "((_ project ~a) ~a)"
	      (1+ pos)
	      (translate-to-smt2* argument bindings)))))

(defmethod translate-to-smt2* ((expr application) bindings)
  (with-slots (operator argument) expr
    (let* ((op* (operator* expr))
	   (op-id (when (name-expr? op*) (id op*))))
      (cond ((and (eq op-id 'rem)
		  (eq (id (module-instance (resolution op*)))
		      'modulo_arithmetic))
	     (let ((denom (translate-to-smt2* (args1 (operator expr))
					      bindings))
		   (numer (translate-to-smt2* (args1 expr)
					      bindings)))
	       `(mod ,numer ,denom)))
	    ((and (eq op-id '-)  ;;NSH(4-19-10)
		  (unary-application? expr)
		  (eq (id (module-instance (resolution op*)))
		      '|number_fields|))
	     (format nil "(- 0 ~a)" (translate-to-smt2* (argument expr) bindings)))
	    ((and (eq op-id 'nat2bv)
		  (number-expr? (expr (car (actuals (module-instance op*))))))
	     (let ((size (translate-to-smt2*
			  (expr (car (actuals (module-instance op*))))
			  bindings))
		   (num (translate-to-smt2*
			 (args1 expr) bindings)))
	       `(mk-bv ,size ,num)))
	    ((and (eq op-id '-)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_arithmetic_defs|)
		  (not (tupletype? (domain (type op*)))))
	     (format nil "(bvneg ~a)"
		     (translate-to-smt2* (argument expr) bindings)))
	    ((and (eq op-id '^)
		  (eq (id (module-instance (resolution op*)))
		      '|bv_caret|)
		  (tuple-expr? argument)
		  (tuple-expr? (cadr (exprs argument)))
		  (number-expr? (car (exprs (cadr (exprs argument)))))
		  (number-expr? (cadr (exprs (cadr (exprs argument))))))
	     (format nil "((_ extract ~a ~a) ~a)"
		     (number (car (exprs (cadr (exprs argument)))))
		     (number (cadr (exprs (cadr (exprs argument)))))
		     (translate-to-smt2* (car (exprs argument)) bindings)))
	    ((and (enum-adt? (find-supertype (type argument)))
		  (recognizer? operator))
	     (format nil "(= ~a ~a)"
		     (translate-to-smt2* argument bindings)
		     (translate-to-smt2* (constructor operator) bindings)))
	    ((constructor? operator)
	     (format nil "(~a ~{ ~a~})"
		     (translate-to-smt2* operator bindings)
		     (translate-to-smt2* (arguments expr) bindings)))
	    (t
	     (let ((smt2-interpretation
		    (smt2-interpretation operator)))
	       (if smt2-interpretation
		   (format nil "(~a ~{ ~a~})"
			   smt2-interpretation
			   (translate-to-smt2* (arguments expr) bindings))
		   (let* ((smt-op (translate-to-smt2* operator bindings))
			  (arg (argument expr))
			  (args (if (tuple-expr? arg)
				    (arguments expr)
				    (let ((stype (find-supertype (type arg))))
				      (if (tupletype? stype)
					  (if (and (variable? arg)
						   (assoc arg bindings
							  :test
							  #'same-declaration))
					      arg
					      (loop for index from 1 to (length (types stype))
						    collect (make-projection-application index arg)))
					  (list arg)))))
			  (yargs (if (and (variable? args)
					  (tupletype? (find-supertype (type arg))))
				     (let ((bnd (assoc arg bindings
						       :test
						       #'same-declaration)))
				       (cdr bnd))
				     (translate-to-smt2* args
							 bindings))))
		     (format nil "(~a ~{ ~a~})" smt-op yargs)))))))))


(defun translate-smt2-bindings (bind-decls bindings prefix-string)
  (cond ((consp bind-decls)
	 (let ((smt2name (smt2-name (car bind-decls)))
	       (smt2type (translate-to-smt2* (type (car bind-decls)) bindings)))
	   (translate-smt2-bindings (cdr bind-decls)
				    (cons (cons (car bind-decls)
						smt2name)
					  bindings)
				    (format nil "~a ~a-~a"
					    prefix-string smt2name ytype))))
	(t (values bindings prefix-string))))



;; TODO: lambda/function case
;; (defmethod translate-to-smt2* ((expr binding-expr) bindings)
;;   (with-slots ((expr-bindings bindings) expression) expr
;;     (let ((stype (find-supertype (type (car expr-bindings)))))
;;       (multiple-value-bind (newbindings bindstring)
;; 	  (translate-smt2-bindings  expr-bindings bindings "")
;; 	(let ((smt2expression (translate-to-smt2* expression newbindings)))
;; 	  (cond ((lambda-expr? expr)
;; 		 (format nil "(define-fun ~a (~a) ~a)"
;; 			 (symbol-name (gensym))
;; 			 bindstring
;; 			 smt2expression))
;; 		((forall-expr? expr)
;; 		 (format nil "(forall (~a) ~a)"
;; 			 bindstring smt2expression))
;; 		((exists-expr? expr)
;; 		 (format nil "(exists (~a) ~a)"
;; 			 bindstring smt2expression))))))))


(defmethod translate-to-smt2* ((expr update-expr) bindings)
  (translate-smt2-assignments (assignments expr)
			      (translate-to-smt2* (expression expr) bindings)
			      (type expr)
			      bindings))



(defun translate-smt2-assignments (assigns 
				   trbasis type bindings)
  (if assigns
      (translate-smt2-assignments (cdr assigns)
				  (translate-smt2-assignment (car assigns)
							     trbasis
							     type
							     bindings)
				  type
				  bindings)
      trbasis))


(defun translate-smt2-assignment (assign trbasis type bindings)
  (translate-smt2-assign-args (arguments assign)
			      (expression assign)
			      trbasis
			      (find-supertype type)
			      bindings))


(defmethod translate-smt2-assign-args (args value trbasis (type recordtype)
				       bindings)
  (if args
      (let* ((label (id (caar args)))
	     (newtrbasis (format nil "(select ~a ~a)" trbasis label)))
	(format nil "(store ~a ~a ~a)" trbasis label
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (type (find (id (caar args)) (fields type)
			     :test #'eq :key #'id))
		 bindings)))
      (translate-to-smt2* value bindings)))



(defmethod translate-smt2-assign-args (args value trbasis (type tupletype)
				       bindings)
  (if args
      (let* ((index (number (caar args)))
	     (newtrbasis (format nil "((_ project ~a) ~a~)" index trbasis)))
	(format nil "(store ~a ~a ~a)" trbasis index
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (type (nth (1- index) (types type))) bindings)))
      (translate-to-smt2* value bindings)))



(defmethod translate-smt2-assign-args (args value trbasis (type funtype)
				       bindings)
  (if args
      (let* ((smt2args1 (translate-to-smt2* (car args) bindings))
	     (newtrbasis (format nil "(select ~a ~a)" trbasis smt2args1)) )
	(format nil "(store ~a ~a ~a)" trbasis smt2args1
		(translate-smt2-assign-args
		 (cdr args) value newtrbasis
		 (range type) bindings)))
      (translate-to-smt2* value bindings)))



(defmethod translate-smt2-assign-args (args value trbasis (type t) bindings)
  (declare (ignore args trbasis))
  (translate-to-smt2* value bindings))


(defun smt2-interpretation (name-expr)
  (when (name-expr? name-expr)
    (let* ((id-assoc (cdr (assoc (id name-expr) *smt2-interpreted-names*)))
	   (mod-assoc (cdr (assoc (id (module-instance
				       (resolution name-expr)))
				  id-assoc))))
      mod-assoc)))



;; TODO: update this to arbitrary solver executables
;; (defun find-yices2-executable ()
;;   (or *yices2-executable*
;;       (cond ((and (pvs-context-yices2-executable)
;; 		  (program-version (pvs-context-yices2-executable) "1"))
;; 	     (setq *yices2-executable* "(pvs-context-yices2-executable)"))
;; 	    ((program-version "yices2 --version" "Yices 2")
;; 	     (setq *yices2-executable* "yices2"))
;; 	    ((program-version "yices --version" "Yices 2")
;; 	     (setq *yices2-executable* "yices"))
;; 	    (t (format t "~%Yices 1 cannot be found in your path")
;; 	       (when (program-version "yices --version" "1")
;; 		 (format t "~%\"yices\" is in your path, but it is version 1"))
;; 	       (unless (pvs-yes-or-no-p
;; 			"~%Use yices (i.e., \"yices\" or \"yices-with-rewrites\") instead? ")
;; 		 (format t "~%If necessary, download and install Yices 2 from http://yices.csl.sri.com")
;; 		 (let ((path (pvs-dialog "~%Please enter the path to Yices 2: ")))
;; 		   (get-yices2-executable-path path)))))))


;; (defun get-yices2-executable-path (path)
;;   (cond ((program-version (concatenate 'string path " --version") "Yices 2")
;; 	 (setq *yices-executable* path))
;; 	(t (format t "~%Invalid path to Yices 2 executable")
;; 	   (let ((npath (pvs-dialog "~%Please enter the path to Yices 2: ")))
;; 	     (get-yices2-executable-path npath)))))


;; (defun yices2 (sformnums nonlinear?);;NSH(8-25-10) Added nonlinear? flag to use nlyices
;;   #'(lambda (ps)                   ;;this handles only arithmetic and uninterpreted
;;                                    ;;functions
;;       (let* ((goalsequent (current-goal ps))
;; 	     (s-forms (select-seq (s-forms goalsequent) sformnums))
;; 	     (*y2defns* nil)
;; 	     (*y2datatype-warning* nil)
;; 	     (*yices2-conditions* nil)
;; 	     (*yices2-subtype-constraints* nil))
;; 	(find-yices2-executable)
;; 	(assert *yices2-executable*)
;; 	(clear-yices2)
;; 	(let ((yices-forms
;; 	       (loop for sf in s-forms
;; 		     collect
;; 		     (let ((fmla (formula sf)))
;; 		       (if (negation? fmla)
;; 			   (format nil "(assert ~a)"
;; 			     (translate-to-yices2* (args1 fmla) nil))
;; 			   (format nil "(assert (not ~a))"
;; 			     (translate-to-yices2* fmla  nil))))))
;; 	      (revdefns (nreverse *y2defns*))
;; 	      (file (make-pathname :defaults (working-directory)
;; 				   :name (label ps) :type "yices")))
;; 	  (format-if "~%ydefns = ~% ~{~a~%~}" revdefns)
;; 	  (format-if "~%ysubtypes = ~% ~{~a~%~}" *yices2-subtype-constraints*)
;; 	  (format-if "~%yforms = ~% ~{~a~%~}" yices-forms)
;; 	  (with-open-file (stream  file :direction :output
;; 				   :if-exists :supersede)
;; 	    (format stream "~{~a ~%~}" revdefns)
;; 	    (unless nil ;;nonlinear?
;; 	      (format stream "~{~a ~%~}" *yices2-subtype-constraints*))
;; 	    (format stream "~{~a ~%~}" yices-forms)
;; 	    (format stream "(check)~%")
;; 	    ;(unless nonlinear? (format stream "(status)"))
;; 	    )
;; 	  (let ((*yices2-flags*
;; 		 (if nonlinear?
;; 		     (concatenate 'string
;; 		       *yices2-flags* " --logic=QF_UFNIRA")
;; 		     *yices2-flags*)))
;; 	    (multiple-value-bind (output err-output status)
;; 		(uiop:run-program
;; 		    (format nil "~a ~a ~a"
;; 		      *yices2-executable*
;; 		      *yices2-flags*
;; 		      (namestring file))
;; 		  :input "//dev//null"
;; 		  :output '(:string :stripped t)
;; 		  :ignore-error-status t)
;; 	      (when *y2datatype-warning*
;; 		(format t "~70,,,'*A" "")
;; 		(format t "~%Warning: The Yices datatype theory is not currently trustworthy.
;; Please check your results with a proof that does not rely on Yices. ~%")
;; 		(format t "~70,,,'*A" ""))
;; 	      (cond ((zerop status)
;; 		     ;;(break "yices result")
;; 		     (format-if "~%Result = ~a" output)
;; 		     (cond ((search "unsat"  output :from-end t)
;; 			    (format-if "~%Yices translation of negation is unsatisfiable")
;; 			    (values '! nil nil))
;; 			   (t (format-if "~%Yices translation of negation is not known to be satisfiable or unsatisfiable")
;; 			      (values 'X nil nil))))
;; 		    (t (format t
;; 			   "~%Error running yices - you may need to do one or more of:~
;;                           ~% 1. Download yices from http://yices.csl.sri.com~
;;                           ~% 2. add yices to your path and restart PVS.
;;                           ~%The error message is:~% ~a"
;; 			 err-output)
;; 		       (values 'X nil)))))))))


	
(addrule 'smt2 () ((fnums *) nonlinear?)
  (smt2 fnums nonlinear?)
  "Invokes an external endgame SMT solver to prove that the conjunction
of the negations of the selected formulas is unsatisfiable. "
  "~%Simplifying with SMT,")


  
  
(defstep smt2-with-rewrites
  (&optional (fnums *) defs theories rewrites exclude-theories exclude)
  (then (simplify-with-rewrites fnums defs theories rewrites exclude-theories exclude)
	(smt2 fnums))
  "Installs rewrites from statement (DEFS is either NIL, T, !, explicit,
or explicit!), from THEORIES, and REWRITES, then applies (assert fnums) followed
by (smt2 fnums), then turns off all the installed rewrites.  Examples:
 (smt2-with-rewrites  + ! (\"real_props\" (\"sets[nat]\"))
                         (\"assoc\"))
 (smt2-with-rewrites * nil :rewrites (\"assoc\" \"distrib\"))."
  "Installing rewrites, simplifying, applying external SMT solver, and disabling installed rewrites")


(defstep smt2simp (&optional (fnums *) nonlinear?)
  (then (skosimp*)(smt2 :fnums fnums :nonlinear? nonlinear?))
  "Repeatedly skolemizes and flattens, and then applies an external SMT solver"
  "Repeatedly skolemizing and flattening, and then invoking an external SMT solver")

  

(defstep smt2grind (&optional (defs !); nil, t, !, explicit, or explicit!
			  theories
			  rewrites
			  exclude
			  (if-match t)
			  (updates? t)
			  polarity?
			  (instantiator inst?)
			  (let-reduce? t)
			  cases-rewrite?
			  quant-simp?
			  no-replace?
			  implicit-typepreds?
			  nonlinear?)
  (then (install-rewrites$ :defs defs :theories theories
		      :rewrites rewrites :exclude exclude)
	(repeat* (bash$ :if-match if-match :updates? updates?
			:polarity? polarity? :instantiator instantiator
			:let-reduce? let-reduce?
			:quant-simp? quant-simp?
			:implicit-typepreds? implicit-typepreds?
			:cases-rewrite? cases-rewrite?))
	(smt2 :nonlinear? nonlinear?))
  "Core of GRIND: Installs rewrites, repeatedly applies BASH, and then
   invokes an external SMT solver.  See BASH for more explanation."
"Repeatedly simplifying with decision procedures, rewriting,
  propositional reasoning, quantifier instantiation, skolemization, dispatch to external SMT solver")

