;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;PVS to Clean Translator (version 0, Jan 20, 2006)
;;Authors: Ronny Wichers Schreur and Natarajan Shankar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Globals: *clean-record-defns* records Clean record type definitions
;;         *clean-primitives* is a list of Clean names for PVS primitives
;;         *pvsclean-primitives-map* is an alist mapping PVS primitives to
;;                   Clean versions
;;         *clean-nondestructive-hash* records translations hashed by PVS decl
;;         *clean-destructive-hash* records destructive translation
;;         *livevars-table* (shadowed) maintains update analysis
;;Top level function is pvs2clean(expr, context) which initializes globals and
;; calls pvs2clean*.  The main cases are applications, which lead to
;;pvs-defn-application and update-expr which branches according to destructive and
;;non-destructive updates.  Unfinished work includes modules and datatypes. 

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

(defvar *livevars-table* nil)

(defparameter *clean-primitives* '((==) (<>) True False pvsImplies pvsImplies
				   pvsIff (&&) (&&) (||)
 not pvsWhen pvsIff (+) (-) (*) (/) pvsNumberFieldPred (<) (<=) (>)
 (>=) pvsRealPred pvsIntegerPred pvsIntegerPred pvsRationalsPred pvsFloor pvsCeiling
 rem / pvsEven? pvsOdd? pvsCons hd tl isCons [!!] isNull pvsRestrict
 length isMember (!!) (++) reverse))

(defparameter *pvsclean-primitives-map*
  (pairlis *pvs2cl-primitives* *clean-primitives*))

(defvar *clean-record-defns* nil)

(defun pvs2clean-primitive-op (name)
  (let ((entry (assoc name *pvsclean-primitives-map* :test #'same-primitive?)))
    (when entry (cdr entry))))

(defmacro pvs2clean-error (msg &rest args)
  `(format t ,msg ,@args))

(defmacro pvsclean_update (array index value)
  `(let ((update-op (if (and *destructive?* *livevars-table*)
			(format nil "pvsDestructiveUpdate")
			(format nil "pvsNonDestructiveUpdate"))))
       (format nil  "~a ~a ~a ~a" update-op ,array ,index ,value)))

(defvar *clean-nondestructive-hash* (make-hash-table :test #'eq))
(defvar *clean-destructive-hash* (make-hash-table :test #'eq))

(defstruct clean-info
  id type definition analysis)

(defmacro clean-hashtable ()
  `(if *destructive?* *clean-destructive-hash* *clean-nondestructive-hash*))

(defun clean_id (op)
  (let ((hashentry (gethash (declaration op) (clean-hashtable))))
    (when hashentry (clean-info-id hashentry))))

(defun clean_nondestructive_id (op)
  (let ((hashentry (gethash (declaration op) *clean-nondestructive-hash*)))
    (when hashentry (clean-info-id hashentry))))

(defun clean_type (op)
  (let ((hashentry (gethash (declaration op) (clean-hashtable))))
    (when hashentry (clean-info-type hashentry))))

(defun clean_definition (op)
  (let ((hashentry (gethash (declaration op) (clean-hashtable))))
    (when hashentry (clean-info-definition hashentry))))

(defun clean_analysis (op)
  (let ((hashentry (gethash (declaration op) (clean-hashtable))))
    (when hashentry (clean-info-analysis hashentry))))

(defun mk-clean-funcall (fun args)
  (format nil "(~a ~{~a ~})" fun args))

(defun pvs2clean (expr &optional context)
  (let* ((*current-context*
	 (if context context *current-context*))
	(*current-theory* (theory *current-context*))
	(*generate-tccs* 'none))
    (pvs2clean* expr nil nil)))


(defmethod pvs2clean* ((expr number-expr) bindings livevars)
  (declare (ignore bindings livevars))
  (number expr))

(defmacro pvs2clean_tuple (args)
  `(format nil "(~{~a~^, ~})" ,args))

(defmethod pvs2clean* ((expr tuple-expr) bindings livevars)
  (let ((args (pvs2clean* (exprs expr) bindings livevars)))
    (pvs2clean_tuple args)))

(defmethod pvs2clean* ((expr record-expr) bindings livevars)
  (let* ((sorted-assignments (sort-assignments (assignments expr)))
	 (formatted-fields
	  (loop for entry in sorted-assignments
		collect (format nil "~a = ~a"
			  (caar (arguments entry))
			  (pvs2clean* (expression entry)
			  bindings livevars)))))
    (format nil "{~{~a~^, ~}}" formatted-fields)))
    
(defun matchlist (index length dummy)
  (if (eql index 0)
	(if (eql length 0)
	    (list dummy)
	    (cons dummy (enlist (1- length))))
      (cons '_ (matchlist (1- index)(1- length) dummy))))

(defun enlist (n)
  (if (eql n 0)
      nil
      (cons '_ (enlist (1- n)))))

(defmethod pvs2clean* ((expr projection-application) bindings livevars)
  (let* ((ll (length (exprs expr)))
	 (dummy (gentemp 'ddd))
	 (match-list (pvs2clean_tuple (matchlist (index expr) ll dummy)))
	 (expr-list (pvs2clean* expr bindings livevars)))
    `(let ,match-list = ,expr-list in ,dummy)))
	


(defmethod pvs2clean*  ((expr field-application) bindings livevars)
  (let* ((clarg (pvs2clean* (argument expr) bindings livevars))
	 (id (id expr)))
    (format nil "~a.~a" clarg id)))

(defmethod pvs2clean* ((expr list) bindings livevars)
  (if (consp expr)
      (cons (pvs2clean* (car expr) bindings
			(append (updateable-vars (cdr expr)) livevars))
	    (pvs2clean* (cdr expr) bindings  ;;need car's freevars
			(append (updateable-vars (car expr)) ;;f(A, A WITH ..)
				livevars)))
      nil))

(defmethod pvs2clean* ((expr application) bindings livevars)
  (with-slots (operator argument) expr
    (if (constant? operator)
	(if (pvs2cl-primitive? operator)
	    (pvs2clean-primitive-app expr bindings livevars)
	    (if (datatype-constant? operator)
		(mk-funapp (pvs2clean-resolution operator)
			   (pvs2clean* (arguments expr) bindings livevars))
		(pvs2clean-defn-application  expr bindings livevars)))
	(let ((clean-op (pvs2clean* operator bindings
				    (append (updateable-vars
					     argument)
					    livevars)))
	      (clean-arg (pvs2clean* argument bindings
				     (append
				      (updateable-free-formal-vars operator)
				      livevars))))
	  (if (clean-updateable? (type operator))
	      (format nil "(pvsSelect ~a ~a)"
		clean-op clean-arg
		(mk-clean-funcall clean-op
				  (list clean-arg))))))))

(defun pvs2clean-primitive-app (expr bindings livevars)
  (format nil "~a ~{ ~a~}"
    (pvs2clean-primitive-op (operator expr))
    (pvs2clean* (arguments expr) bindings livevars)))

(defun constant-formals (module)
  (loop for x in (formals module)
			 when (formal-const-decl? x)
			 collect (make-constant-from-decl x)))

(defun pvs2clean-defn-application (expr bindings livevars)
  (with-slots (operator argument) expr
    (pvs2clean-resolution operator)
    (let* ((actuals (expr-actuals (module-instance operator)))
	   (op-decl (declaration operator))
	   (args (arguments expr))
	   (clean-args (pvs2clean* (append actuals args) bindings livevars)))
      (if *destructive?*
	(let* ((defns (def-axiom op-decl))
	   (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	   (def-formals (when (lambda-expr? defn)
			  (bindings defn)))
	   (module-formals (constant-formals (module op-decl)))
	   (alist (append (pairlis module-formals actuals)
			  (pairlis def-formals args)))
	   (analysis (clean_analysis operator))
	   (check (check-output-vars analysis alist livevars)))
	  (format nil "(~a ~{ ~a~})" (if check (clean_id operator)
				       (clean_nondestructive_id operator))
			    clean-args)
	      )
	(format nil "(~a ~{ ~a~})" (clean_id operator) clean-args)))))

(defun pvs2clean-resolution (op)
  (let* ((op-decl (declaration op)))
    (pvs2clean-declaration op-decl)))

(defun pvs2clean-declaration (op-decl)
  (let ((nd-hashentry (gethash op-decl *clean-nondestructive-hash*))
	;(d-hashentry (gethash op-decl *clean-destructive-hash*))
	;enough to check one hash-table. 
	)
    (when (null nd-hashentry)
      (let ((op-id (gentemp (format nil "pvs_~a" (id op-decl))))
	    (op-d-id (gentemp (format nil "pvs_d_~a" (id op-decl)))))
      (setf (gethash op-decl *clean-nondestructive-hash*)
	    (make-clean-info :id op-id))
      (setf (gethash op-decl *clean-destructive-hash*)
	    (make-clean-info :id op-d-id))
      (let* ((defns (def-axiom op-decl))
	     (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	     (def-formals (when (lambda-expr? defn)
			    (bindings defn)))
	     (def-body (if (lambda-expr? defn) (expression defn) defn))
	     (module-formals (constant-formals (module op-decl)))
	     (range-type (if def-formals (range (type op-decl))
			     (type op-decl))))
	(pvs2clean-resolution-nondestructive op-decl (append module-formals def-formals)
					     def-body range-type)
	(pvs2clean-resolution-destructive op-decl (append module-formals def-formals)
					  def-body range-type))))))

(defun pvs2clean-resolution-nondestructive (op-decl formals body range-type)
  (let* ((*destructive?* nil)
	 (bind-ids (pvs2cl-make-bindings formals nil))
	 (cl-body (pvs2clean* body
			   (pairlis formals bind-ids)
			   nil))
	 (cl-type (format nil "~{~a ~} -> ~a"
		  (loop for var in formals
			collect (format nil "!~a" (pvs2clean-type (type var))))
		  (pvs2clean-type range-type)))
	 (cl-defn (format nil "~a ~{~a ~} -> ~a" "\\ " bind-ids cl-body))
	 (hash-entry (gethash op-decl *clean-nondestructive-hash*)))
    (format t "~%Defining (nondestructively) ~a with ~%type ~a ~%as ~a" (id op-decl) cl-type cl-defn)
    (setf (clean-info-type hash-entry)
	  cl-type
	  (clean-info-definition hash-entry)
	  cl-defn
	  )))

(defun pvs2clean-resolution-destructive (op-decl formals body range-type)
  (let* ((*destructive?* t)
	 (*output-vars* nil)
	 (bind-ids (pvs2cl-make-bindings formals nil))
	 (cl-body (pvs2clean* body
			   (pairlis formals bind-ids)
			   nil))
	 (cl-type (format nil "~{~a ~} -> ~a"
		  (loop for var in formals
			collect (if (assoc (declaration var) *output-vars*
					   :key #'declaration)
				    (format nil "!*~a" (pvs2clean-type (type var)))
				    (format nil "!~a" (pvs2clean-type (type var)))))
		  (pvs2clean-type range-type)))
	 (cl-defn  (format nil "~a ~{~a ~} -> ~a" "\\ " bind-ids cl-body))
	 (hash-entry (gethash op-decl *clean-destructive-hash*))
	 (old-output-vars (clean-info-analysis hash-entry)))
        (format t "~%Defining (destructively) ~a with ~%type ~a ~%as ~a" (id op-decl) cl-type cl-defn)
    (setf (clean-info-type hash-entry)
	  cl-type
	  (clean-info-definition hash-entry)
	  cl-defn
	  (clean-info-analysis hash-entry)
	  *output-vars*)
    (unless (equalp old-output-vars *output-vars*)
      (pvs2clean-resolution-destructive op-decl formals body range-type))))



	  
(defmethod pvs2clean* ((expr name-expr) bindings livevars)
  (let* ((decl (declaration expr))
	 (bnd (assoc  decl bindings :key #'declaration)))
    (assert (not (and bnd (const-decl? decl))))
    (if bnd
	(cdr bnd)
	(if (const-decl? decl)
	    (pvs2clean-constant expr decl bindings livevars)
	    (let ((undef (undefined expr "Hit untranslateable expression ~a")))
	      `(funcall ',undef))))))

(defun pvs2clean-constant (expr op-decl bindings livevars)
  (let* ((defns (def-axiom op-decl))
	 (defn (when defns (args2 (car (last (def-axiom op-decl))))))
	 (def-formals (when (lambda-expr? defn)
			(bindings defn))))
    (pvs2clean-resolution expr)
    (if def-formals 
	(let ((eta-expansion
		   (mk-lambda-expr def-formals
		     (mk-application expr
		       (loop for bd in def-formals
			     collect (change-class (copy bd) 'name-expr))))))
	  (pvs2clean* eta-expansion bindings livevars))
	(let* ((actuals (expr-actuals (module-instance expr)))
	       (clean-actuals (pvs2clean* actuals bindings livevars)))
	(format nil "(~a ~{ ~a~})" (clean_nondestructive_id expr)
		clean-actuals)))))



(defun pvs2clean-lambda (bind-decls expr bindings) ;;removed livevars
  (let* ((*destructive?* nil)
	 (bind-ids (pvs2cl-make-bindings bind-decls bindings))
	 (cl-body (pvs2clean* expr
			   (append (pairlis bind-decls bind-ids)
				   bindings)
			   nil)))
    (format nil "~a ~{~a ~} -> ~a" "\\ " bind-ids cl-body)))

(defmethod pvs2clean* ((expr lambda-expr) bindings livevars)
  (declare (ignore livevars))
  (let ((type (type expr))
	(clean-expr (pvs2clean-lambda (bindings expr) (expression expr) bindings)))
    (if (and (clean-updateable? type)
	     (funtype? type))
	(format nil "(Function ~a ~a)" (array-bound type) clean-expr)
	clean-expr)))


(defmethod pvs2clean* ((expr if-expr) bindings livevars)
  (cond ((branch? expr)
	 (let ((condition (condition expr))
	       (then-part (then-part expr))
	       (else-part (else-part expr)))
	 `(if ,(pvs2clean* condition bindings
			   (append (updateable-vars then-part)
				   (append (updateable-vars else-part)
					   livevars)))
	      ,(pvs2clean* (then-part expr) bindings livevars)
	      ,(pvs2clean* (else-part expr) bindings livevars))))
	(t (call-next-method))))

(defmethod pvs2clean* ((expr cases-expr) bindings livevars)
  (format nil "case ~a of ~{~%~a~}"
    (pvs2clean* (expression expr) bindings livevars)
    (pvs2clean-cases (selections expr)(else-part expr) bindings livevars)))

(defun pvs2clean-cases (selections else-part bindings livevars)
  (let ((selections-clean
	 (loop for entry in selections
	       collect
	       (let* ((bind-decls (args entry))
		      (bind-ids (pvs2cl-make-bindings bind-decls bindings)))
		 (format nil "~a ~{~a ~} -> ~a"
			 (pvs2clean* (constructor entry) bindings livevars)
			 bind-ids
			 (pvs2clean* (expression entry)
				     (append (pairlis bind-decls bind-ids) bindings)
				     livevars))))))
    (if else-part
	(format nil "~a ~% _ -> ~a"
	  (pvs2clean* (expression else-part) bindings livevars))
	selections-clean)))

(defmethod pvs2clean* ((expr update-expr) bindings livevars)
  (if (clean-updateable? (type (expression expr)))
      (if (and *destructive?*
	       (not (some #'maplet? (assignments expr))))
	  (let* ((expression (expression expr))
		 (assignments (assignments expr))
		 (*livevars-table* 
		  (no-livevars? expression livevars assignments))
		 )
	    ;;very unrefined: uses all
	    ;;freevars of eventually updated expression.
	    (cond (*livevars-table* ;; check-assign-types
		   (push-output-vars (car *livevars-table*)
				     (cdr *livevars-table*))
		   (pvs2clean-update expr
				  bindings livevars))
		  (t
		   (when (and *eval-verbose* (not *livevars-table*))
		     (format t "~%Update ~s translated nondestructively.
 Live variables ~s present" expr livevars))
		   (pvs2clean-update  expr
						    bindings livevars))))
	  (pvs2clean-update expr bindings livevars))
      (pvs2clean* (translate-update-to-if! expr)
		  bindings livevars)))

(defun pvs2clean-update
    (expr bindings livevars)
  (with-slots (type expression assignments) expr
    (let* ((assign-exprs (mapcar #'expression assignments))
	   (exprvar (gentemp "E"))
	   (clean-expr (pvs2clean* expression bindings
				(append (updateable-free-formal-vars
					 assign-exprs)
					;;assign-args can be ignored
					livevars))))
      (format nil "#! ~a"
    (pvs2clean-update* (type expression)
				   clean-expr exprvar
				   (mapcar #'arguments assignments)
				   assign-exprs
				   bindings
				   (append (updateable-vars expression)
					   livevars)
				   (list (list exprvar clean-expr)))))))

(defun pvs2clean-assign-rhs (assignments bindings livevars)
  (when (consp assignments)
      (let ((clean-assign-expr (pvs2clean* (expression (car assignments))
					   bindings
					   (append (updateable-vars
						    (arguments (car assignments)))
						   (append (updateable-vars (cdr assignments))
					   livevars))))
	    (*lhs-args* nil))
	(cons clean-assign-expr
	      (pvs2clean-assign-rhs (cdr assignments) bindings
				    (append (updateable-free-formal-vars
					     (expression (car assignments)))
					    livevars))))))

				   

;;recursion over updates in an update expression
(defun pvs2clean-update*
    (type expr exprvar
	  assign-args assign-exprs bindings livevars accum)
  (if (consp assign-args)
      (let* ((*lhs-args* nil)
	     (assign-exprvar (gentemp "R"))
	     (clean-assign-expr
	      (pvs2clean* (car assign-exprs)
			  bindings
			  (append (updateable-vars (cdr assign-exprs))
				  (append (updateable-vars (cdr assign-args))
					  livevars))))
	     (newexprvar (gentemp "N"))
	     (new-accum (pvs2clean-update-nd-type
		       type exprvar newexprvar
		       (car assign-args)
		       assign-exprvar
		       bindings
		       (append (updateable-free-formal-vars (car assign-exprs))
			       (append (updateable-vars (cdr assign-exprs))
				       (append (updateable-vars (cdr assign-args))
					       livevars)))
		       accum))
	     (lhs-bindings (nreverse *lhs-args*))
	     (cdr-clean-output
	      (pvs2clean-update*
	       type expr
	       newexprvar
	       (cdr assign-args)(cdr assign-exprs) bindings
	       (append (updateable-free-formal-vars (car assign-exprs))
		       livevars) 
		       new-accum )))
	(format nil "~a = ~a ~%~:{~a = ~a~%~} ~a"
	  assign-exprvar clean-assign-expr
		lhs-bindings
		 cdr-clean-output))
      (format nil "~:{~a = ~a~%~} = ~a" (nreverse accum) exprvar)))
     	  


;;recursion over nested update arguments in a single update.
(defun pvs2clean-update-nd-type (type expr newexprvar args assign-expr
					   bindings livevars accum)
  (if (consp args)
      (pvs2clean-update-nd-type* type expr newexprvar (car args) (cdr args) assign-expr
			      bindings livevars accum)
      (cons (list newexprvar assign-expr) accum)))

(defmethod pvs2clean-update-nd-type* ((type funtype) expr newexprvar arg1 restargs
				   assign-expr bindings livevars accum)
  (let* ((arg1var (gentemp "L"))
	 (clean-arg1 (pvs2clean*  (car arg1) bindings
				 (append (updateable-vars restargs)
					 livevars))))
    (push (list arg1var clean-arg1) *lhs-args*)
    (if (consp restargs)
	(let* (
	       (exprvar (gentemp "E"))
	       (exprval (format nil "pvsSelect ~a ~a" expr arg1var))
	       (newexprvar2 (gentemp "N"))
	       (newaccum
		(pvs2clean-update-nd-type 
		 (range type) exprvar newexprvar2
		 restargs assign-expr bindings livevars
		 (cons (list exprvar exprval) accum))))
	  (cons (list newexprvar (pvsclean_update expr arg1var newexprvar2))
		newaccum)
	  )
	(cons (list newexprvar (pvsclean_update expr arg1var assign-expr))
	      accum))))



(defmethod pvs2clean-update-nd-type* ((type recordtype) expr newexprvar arg1 restargs
				   assign-expr bindings livevars accum)
  (let ((id (id (car arg1))))
	(if (consp restargs)
	    (let* (
		   (exprvar (gentemp "E"))
		   (new-expr (format nil "~a.~a" expr id))
		   (field-type (type (find id (fields type) :key #'id) ))
		   (newexprvar2 (gentemp "N"))
		   (newaccum (pvs2clean-update-nd-type field-type exprvar newexprvar2
						       restargs assign-expr bindings
						       livevars
						       (cons (list exprvar new-expr) accum))))
	      (cons (list newexprvar (format nil "{~a & ~a = ~a}" expr id newexprvar2)) newaccum))
	    (cons (list newexprvar (format nil "{~a & ~a = ~a}" expr id assign-expr))
		  accum))))

(defmethod pvs2clean-update-nd-type* ((type subtype)  expr newexprvar arg1 restargs
				   assign-expr bindings livevars accum)
  (pvs2clean-update-nd-type* (find-supertype type) expr newexprvar arg1 restargs
			  assign-expr bindings livevars accum))

(defmethod pvs2clean-type ((type recordtype))
  (with-slots (print-type) type
    (if (type-name? print-type)
	(let ((entry (assoc (declaration print-type) *clean-record-defns*)))
	  (if entry (cadr entry) ;return the clean-rectype-name
	      (let* ((formatted-fields (loop for fld in (fields type)
				  collect
				  (format nil "~a :: !~a" (id fld)
						(pvs2clean-type (type fld)))))
		    (clean-rectype (format nil "{ ~{~a~^, ~} }" formatted-fields))
		    (clean-rectype-name (gentemp (format nil "pvs~a" (id print-type)))))
		(push (list (declaration print-type) clean-rectype-name clean-rectype)
		      *clean-record-defns*)
		clean-rectype-name)))
	(pvs2clean-error "~%Record type ~a must be declared." type))))

(defmethod pvs2clean-type ((type tupletype))
  (format nil "(~{!~a~^, ~})" (loop for elemtype in (types type)
				   collect (pvs2clean-type elemtype))))

(defmethod pvs2clean-type ((type funtype))
  (if (clean-updateable? type)
      (format nil "(PvsArray ~a)" (pvs2clean-type (range type)))
      (format nil "(~a -> ~a)"
	(pvs2clean-type (domain type))
	(pvs2clean-type (range type)))))

(defmethod pvs2clean-type ((type subtype))
  (let ((fs (find-supertype type)))
    (if (and (eq fs *number*)
	     (subtype-of? type *integer*))
	(format nil "BigInt");;Generates nonsense if type is not subtype of int.
	(pvs2clean-type (find-supertype type)))))


;;clean-updateable? is used to check if the type of an updated expression
;;is possibly destructively. 

(defmethod clean-updateable? ((texpr tupletype))
  (clean-updateable? (types texpr)))

;;this is the only case where clean-updateable? can be false, because
;;the given function type is not an array.  
(defmethod clean-updateable? ((texpr funtype)) ;;add enum types, subrange.
  (and (or (simple-below? (domain texpr))(simple-upto? (domain texpr)))
       (clean-updateable? (range texpr))))

(defmethod clean-updateable? ((texpr recordtype))
  (clean-updateable? (mapcar #'type (fields texpr))))

(defmethod clean-updateable? ((texpr subtype))
  (clean-updateable? (find-supertype texpr)))

(defmethod clean-updateable? ((texpr list))
  (or (null texpr)
      (and (clean-updateable? (car texpr))
	   (clean-updateable? (cdr texpr)))))

;;This is subsumed by fall-through case.
;(defmethod clean-updateable? ((texpr type-name))
;  (not (or (eq texpr *boolean*)
;	   (eq texpr *number*))))



;(defmethod clean-updateable? ((texpr actual))
;  (clean-updateable? (type-value texpr)))

(defmethod clean-updateable? ((texpr t))
  t) ;;It is okay to say  clean-updateable? for uninterpreted
;;or actuals since these will not be updated destructively or otherwise.
  
(defun pvs2clean-theory (theory)
  (let* ((theory (get-theory theory))
	 (*current-theory* theory)
	 (*current-context* (context theory)))
    (cond ((datatype? theory)
	   (let ((adt (adt-type-name theory)))
	     (pvs2clean-constructors (constructors adt) adt))
	   (pvs2clean-theory (adt-theory theory))
	   (let ((map-theory (adt-map-theory theory))
		 (reduce-theory (adt-reduce-theory theory)))
	     (when map-theory (pvs2clean-theory (adt-map-theory theory)))
	     (when reduce-theory (pvs2clean-theory (adt-reduce-theory theory))))) 
	  (t (loop for decl in (theory theory)
		   do (cond ((type-eq-decl? decl)
			     (let ((dt (find-supertype (type-value decl))))
			       (when (adt-type-name? dt)
				 (pvs2clean-constructors (constructors dt) dt))))
			    ((datatype? decl)
			     (let ((adt (adt-type-name decl)))
			       (pvs2clean-constructors (constructors adt) adt)))
			    ((const-decl? decl)
			     (unless (eval-info decl)
			       (progn
				 (pvs2clean-declaration decl))))
			    (t nil)))))))

(defun clear-clean-hash ()
  (clrhash *clean-nondestructive-hash*)
  (clrhash *clean-destructive-hash*))
