;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mlpp.lisp -- The PVS prettyprinter
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 23:19:42 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 18 20:56:23 2012
;; Update Count    : 10
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

;;PVS pretty-printer for machine learning

(in-package :pvs)

(defvar *mlpp-type-hash* nil "maintains the cached extraction for types")
(defvar *mlpp-term-hash* nil "maintains the cached extraction for terms")


;;We can encode Booleans as B and numbers as N, and then subtypes of a type T as T/,
;;tuple types as Uk....., where k is the arity, and record types as Rk where k is the arity.
;;These types can then be encoded by a vector consisting of the supertype structure and the
;;first weights of the first k terms.   Let's initially use vectors of size 5, with possible expansion to 20. 
;;This way all subtypes at the same level will be identical.
;;For terms, we generate a sequence of tokens of operators and their arities.
;;For variables, we generate the type/id pair.  

;;;Declarations

(defun mlpp-decl (decl)
  (let ((*current-context* (decl-context decl))
	(*mlpp-type-hash* (or *mlpp-type-hash* (make-pvs-hash-table))))
    (mlpp-decl* decl)))

(defmethod mlpp-decl* ((decls list))
  (when decls
    (cons (mlpp-decl* (car decls))
	  (mlpp-decl* (cdr decls)))))

(defmethod mlpp-decl* ((imp importing))
  `((:tag . :importing)
    (:theory-name . (mlpp* (theory-name imp)))))

(defmethod mlpp-decl* ((decl type-eq-decl))
  ;;name, kind, parameters, definition
  `((:tag . :type-eq-decl)
    (:name . ,(pvs2ir-unique-decl-id decl))
    ,(mlpp* (type-value decl))))

(defmethod mlpp-decl* ((decl enumtype))
  (mlpp* decl))

(defmethod mlpp-decl* ((decl type-decl))
  ;;name, kind, parameters, definition
  `((:tag . :type-decl)
    (:name . ,(pvs2ir-unique-decl-id decl))))

(defmethod mlpp-decl* ((decl formal-type-decl))
  ;;name, kind, parameters, definition
  `((:tag . :formal-type-decl)
    (:name . ,(pvs2ir-unique-decl-id decl))))

(defmethod mlpp-decl* ((decl formal-const-decl))
  ;;name, kind, parameters, definition
  `((:tag . :formal-const-decl)
    (:name . ,(pvs2ir-unique-decl-id decl))
    ,(mlpp* (type decl))))

(defmethod mlpp-decl* ((decl theory-abbreviation-decl))
  `((:tag . :theory-abbreviation-decl)
    (:id . ,(pvs2ir-unique-decl-id decl))
    (:theory-name . ,(mlpp* (theory-name decl)))))

(defmethod mlpp-decl* ((decl const-decl))
  ;;name, kind, parameters, definition
  `((:tag . :const-decl)
    (:name . ,(pvs2ir-unique-decl-id decl))
    ,(mlpp* (type decl))
    (:const-def . ,(mlpp* (decl-defn decl)))))

(defmethod mlpp-decl* ((decl var-decl))
  (with-slots (id declared-type type) decl
    `((:tag . :var-decl)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      ,(mlpp* type))))

(defmethod mlpp-decl* ((decl formula-decl))
  (with-slots (id spelling closed-definition default-proof) decl ;just the default proof for now
    (let ((prf (mlpp* default-proof)))
      `((:tag . :formula-decl)
	(:id . ,id)
	(:label . ,spelling)
	(:definition ,(mlpp* closed-definition))
	(:proof . ,prf)))))

(defmethod mlpp-decl* ((decl subtype-judgement))
  (with-slots (id declared-type type declared-subtype subtype) decl
    `((:tag . :subtype-judgement)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      (:type ,(list (mlpp* type)))
      (:declared-subtype ,(list (mlpp* declared-subtype)))
      (:subtype ,(list (mlpp* subtype))))))

(defmethod mlpp-decl* ((decl name-judgement))
  (with-slots (id declared-type type name) decl
    `((:tag . :name-judgement)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      (:type ,(list (mlpp* type)))
      (:name . ,(mlpp* name)))))

(defmethod mlpp-decl* ((decl application-judgement))
  (with-slots (id declared-type type name formals judgement-type) decl
    `((:tag . :application-judgement)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      (:type ,(list (mlpp* type)))
      (:name . ,(mlpp* name))
      (:formals . ,(mlpp* formals))
      (:judgement-type ,(list (mlpp* judgement-type))))))

(defmethod mlpp-decl* ((decl expr-judgement))
  (with-slots (id declared-type type expr formals judgement-type) decl
    `((:tag . :expr-judgement)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      (:type ,(list (mlpp* type)))
      (:expr . ,(mlpp* expr))
      (:formals . ,(mlpp* formals))
      ,@(when judgement-type
	  (list `(:judgement-type ,(list (mlpp* judgement-type))))))))

(defmethod mlpp-decl* ((decl conversion-decl))
  (with-slots (id expr) decl
    `((:tag . :conversion-decl)
      (:id . ,id)
      (:expr . ,(mlpp* expr)))))

(defmethod mlpp-decl* ((decl auto-rewrite-decl))
  (with-slots (id rewrite-names) decl
    `((:tag . :auto-rewrite-decl)
      (:id . ,id)
      (:rewrite-names . ,(mapcar #'mlpp* rewrite-names)))))

(defmethod mlpp* ((pinfo proof-info))
  `((:tag . :proof-info)
    (:script . ,(format nil "~s" (script pinfo)))
    (:status . ,(status pinfo))
    ;;(:refers-to . ,(refers-to pinfo))
    ))

(defmethod print-type-hash ()
  (let ((entries nil))
    (when (hash-table-p *mlpp-type-hash*)
      (maphash #'(lambda (key val)
		   (declare (ignore key))
		   (with-slots (typehash texpression) val
		     (push `(,(cdr typehash) . ,texpression) entries)))
	       *mlpp-type-hash*)
      `((:tag :typelist)
	(:entries . ,entries)))))

;;;Expressions
(defun mlpp (expr)
  (let* ((*mlpp-type-hash* (or *mlpp-type-hash* (make-pvs-hash-table)))
	 (result (mlpp* expr)))
    (values result *mlpp-type-hash*)))

(defmethod mlpp* ((expr int-expr))
  (with-slots (number) expr
    `((:tag . :integer)
      (:integer-value . ,number))))

(defmethod mlpp* ((expr number-expr))
  (with-slots (number) expr
    `((:tag . :integer)
      (:integer-value . ,number))))

(defmethod mlpp* ((expr string-expr))
  `((:tag . :string)
    (:string-value . ,(string-value expr))))

(defmethod mlpp* ((expr name-expr))
  (let ((decl (declaration expr)))
    (typecase decl
      (null (call-next-method))
      (const-decl
       `((:tag . :constant)
	 (:constant-name . ,(string (pvs2ir-unique-decl-id decl)))
	 (:actuals . ,(mlpp* (actuals (module-instance expr))))
	 ,(mlpp* (type expr))))
      (formal-const-decl
       `((:tag . :formal-constant)
	 (:constant-name . ,(pvs2ir-unique-decl-id decl))))
      (t `((:tag :variable)
	   ,@(when (type decl) (list (mlpp* (type decl))))
	   (:variable-name . ,(id expr)))))))

(defmethod mlpp* ((expr actual))
  (with-slots (expr type-value) expr
    (if type-value
	`((:tag . :type-actual)
	  ,(mlpp* type-value))
	`((:tag . :const-actual)
	  (:expr . ,(mlpp* expr))))))

(defmethod mlpp* ((expr lambda-expr))
  (with-slots (bindings expression) expr
    `((:tag . :lambda)
      (:bindings . ,(mlpp* bindings))
      (:expression . ,(mlpp* expression)))))

(defmethod mlpp* ((expr forall-expr))
  (with-slots (bindings expression) expr
    `((:tag . :forall)
      (:bindings . ,(mlpp* bindings))
      (:expression . ,(mlpp* expression)))))

(defmethod mlpp* ((expr exists-expr))
  (with-slots (bindings expression) expr
    `((:tag . :exists)
      (:bindings . ,(mlpp* bindings))
      (:expression . ,(mlpp* expression)))))

(defmethod mlpp* ((expr binding-expr))
  (with-slots (op bindings expression) expr
    `((:tag . :bind)
      (:operator . ,op)
      (:bindings . ,(mlpp* bindings))
      (:expression . ,(mlpp* expression)))))

(defmethod mlpp* ((expr application))
  (with-slots (operator argument) expr
    `((:tag . :apply)
      (:operator . ,(mlpp* operator))
      (:argument . ,(mlpp* argument)))))

(defmethod mlpp* ((expr list))
  ;; mapcar won't work for cons pairs
  (when expr
    (cons (mlpp* (car expr))
	  (mlpp* (cdr expr)))))

(defmethod mlpp* ((expr cases-expr))
  (with-slots (expression selections else-part) expr
    `((:tag . :cases)
      (:expr . ,(mlpp* expression))
      (:selections . ,(mlpp-selections selections))
      (:else-part . ,(mlpp* else-part)))))

(defun mlpp-selections (selections)
  (loop for sel in selections
	collect `((:tag . :selection)
		  (:pattern . `(apply ,(mlpp* (constructor sel)) ,(mlpp* (args sel))))
		  (:expr . ,(mlpp* (expression sel))))))

(defmethod mlpp* ((expr if-expr))
  (cond ((branch? expr)
	 (if (last-cond-expr? expr)
	     (mlpp* (then-part expr))
	     `((:tag . :if)
	       (:test . ,(mlpp* (condition expr)))
	       (:then . ,(mlpp* (then-part expr)))
	       (:else . ,(mlpp* (else-part expr))))))
	(t (call-next-method))))

(defmethod mlpp* ((ex let-expr))
  (with-slots (operator) ex
    (let ((let-bindings (bindings operator))
	  (arguments (arguments ex))
	  (expr (expression operator)))
      `((:tag . :let)
	(:bindings . ,(mapcar #'mlpp-let-binding let-bindings arguments))
	(:body . ,(mlpp* expr))))))

(defun mlpp-let-binding (binding arg)
  `((:tag . :let-binding)
    (:id . ,(id binding))
    (:declared-type ,(list (mlpp* (declared-type binding))))
    (:type ,(list (mlpp* (type binding))))
    (:expr . ,(mlpp* arg))))

(defmethod mlpp* ((expr record-expr))
  (with-slots (assignments) expr
    `((:tag . :record)
      (:assignments ,(mapcar #'mlpp* (sort-assignments assignments))))))

(defmethod mlpp* ((assn assignment))
  `((:arguments . ,(mlpp* (arguments assn)))
    (:expr . ,(mlpp* (expression assn)))))

(defmethod mlpp* ((expr tuple-expr))
  (with-slots (exprs) expr
    `((:tag :tuple)
      ,@(loop for tupentry in exprs
	      collect (mlpp* tupentry)))))

(defmethod mlpp* ((expr projection-application))
  (with-slots (index argument) expr
    `((:tag . :project)
      (:argument . ,(mlpp* argument))
      (:index . ,index))))

(defmethod mlpp* ((expr field-application))
  (with-slots (id argument) expr
    `((:tag . :getfield)
      (:field . ,id)
      (:argument . ,(mlpp* argument)))))

(defmethod mlpp* ((expr update-expr))
  (with-slots (type expression assignments) expr
    `((:tag . :update)
      (:expression . ,(mlpp* expression))
      (:assignments . ,(mlpp* assignments)))))

(defmethod mlpp* ((expr field-assignment-arg))
  `((:field . ,(id expr))))

(defmethod mlpp* ((expr proj-assign))
  `((:projection . ,(number expr))))

(defmethod mlpp* ((expr accessor-assignment-arg))
  `((:accessor . ,(id expr))))


;;;Types

(defmethod mlpp*-binding (bdecl)
  `((:tag . :bind-decl)
    (:variable-name . ,(pvs2ir-unique-decl-id bdecl))
    (:type . ,(mlpp* (type bdecl)))))

(defcl mlpp-type-entry ()
  typehash texpression)

(defun mk-mlpp-type-entry (typehash texpression)
  (make-instance 'mlpp-type-entry
    :typehash typehash
    :texpression texpression))


(defmethod mlpp* :around ((texpr type-expr))
  (let ((entry (gethash texpr *mlpp-type-hash*)))
    (or (and entry (typehash entry))
 	(let ((texpression (call-next-method))
	      (typehash `(:type . ,(pvs-sxhash texpr))))
	  ;;(json:encode-json texpression)
	  (setf (gethash texpr *mlpp-type-hash*)
		(mk-mlpp-type-entry typehash texpression))
	  typehash))))

(defmethod mlpp* :around ((texpr dep-binding))
  (let ((entry (gethash texpr *mlpp-type-hash*)))
    (or (and entry (typehash entry))
 	(let ((texpression (call-next-method))
	      (typehash `(:type . ,(pvs-sxhash texpr))))
	  ;;(json:encode-json texpression)
	  (setf (gethash texpr *mlpp-type-hash*)
		(mk-mlpp-type-entry typehash texpression))
	  typehash))))

(defmethod mlpp* ((texpr arraytype))
  (with-slots (domain range) texpr
    `((:tag . :arraytype)
      (:domain ,(mlpp* domain))
      (:range ,(mlpp* range)))))

(defmethod mlpp* ((texpr funtype))
  (with-slots (domain range) texpr
    (let ((talist `((:tag . :functiontype)
		    (:domain ,(mlpp* domain))
		    (:range ,(mlpp* range)))))
      talist)))

(defmethod mlpp* ((dep dep-binding))
  (with-slots (id type) dep
    (let ((talist `((:tag . :dep-binding)
		    (:id . ,id)
		    ,(mlpp* type))))
      talist)))

(defmethod mlpp* ((texpr recordtype))
  (with-slots (fields) texpr
    (let* ((sfields (sort-fields fields (dependent-fields? fields)))
	   (rtype `((:tag . :recordtype)
		    (:fields ,@(mapcar #'mlpp* sfields)))))
      ;;(json:encode-json rtype)
      rtype)))

(defmethod mlpp* ((fld field-decl))
  (with-slots (id type) fld
    `((:tag . :field)
      (:id . ,id)
      ,(mlpp* type))))

(defmethod mlpp* ((texpr tupletype))
  (with-slots (types) texpr
    (let ((ttype `((:tag . :tupletype)
		   (:types ,@(loop for typ in types collect (mlpp* typ))))))
      ;;(json:encode-json ttype)
      ttype)))

(defmethod mlpp* ((texpr enumtype))
  (with-slots (constructors) texpr
    `((:tag . :enumtype)
      (:elements . ,(mapcar #'id constructors)))))

(defmethod mlpp* ((texpr subtype))
  (with-slots (supertype predicate) texpr
    (let ((stype `((:tag . :subtype)
		   (:supertype ,(mlpp* supertype))
		   (:predicate . ,(mlpp* predicate)))))
      ;;(json:encode-json stype)
      stype)))

(defmethod mlpp* ((texpr type-name))
  (let ((tname `((:tag . :typename)
		 (:id . ,(pvs2ir-unique-decl-id (declaration texpr))))))
    ;;(json:encode-json tname)
    tname))

;;;Sequents

(defmethod mlpp* ((sform s-formula))
  (with-slots (formula label new? asserted?) sform
    `((:tag . :s-formula)
      (:label . ,label)
      (:new? . ,new?)
      (:formula . ,(mlpp* formula))
      (:asserted? . ,asserted?))))

(defmethod mlpp* ((seq sequent))
  (with-slots (p-sforms n-sforms hidden-s-forms) seq
    `((:tag . :sequent)
      (:antecedents . ,(mlpp* n-sforms))
      (:consequents . ,(mlpp* p-sforms))
      (:hidden . ,(mlpp* hidden-s-forms)))))

(defmethod mlpp* ((ps proofstate))
  (with-slots (label current-goal current-rule current-input) ps
    (let* ((mlpp-rule (mlpp-input current-rule))
	   (mlpp-inp (mlpp-input current-input))
	   (mlpp-ps `((:tag . :proofstate)
		      (:label . ,label)
		      (:current-goal . ,(mlpp* current-goal))
		      (:current-rule . ,mlpp-rule)
		      (:current-input . ,mlpp-inp))))
      (with-output-to-string (*standard-output*)
	(json:encode-json mlpp-ps))
      mlpp-ps)))

(defmethod mlpp* ((expr t))
  (format nil "~a" expr))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;declarations

;; (defmethod mlpp* ((decl type-decl))
;;   (with-slots (id type-value) decl
;;     `(type-decl :id ,id
;; 		:type-value ,(mlpp* type-value))))

;; (defmethod mlpp* ((decl type-eq-decl))
;;   (with-slots (id type-expr) decl
;;     `(type-eq-decl :id ,id
;; 		   :type-expr ,(mlpp* type-expr))))

;; (defmethod mlpp* ((decl const-decl))
;;   (with-slots (id type definition) decl
;;       `(const-decl :id ,id
;; 		   :type ,(mlpp* type)
;; 		   :definition ,(mlpp* definition))))

;; (defmethod mlpp* ((decl def-decl))
;;   (with-slots (id type definition measure) decl
;;       `(def-decl :id ,id
;; 	 :type ,(mlpp* type)
;; 	 :definition ,(mlpp* definition)
;; 	 :measure ,(mlpp* measure))))

(defmethod mlpp* ((module module))
  (with-slots (id formals assuming theory) module
    `((:tag . :theory)
      (:id . ,id)
      (:formals . ,(mlpp-decl* formals))
      (:assuming . ,(mlpp-decl* assuming))
      (:declarations . ,(mlpp-decl* theory)))))

(defmethod mlpp* ((adt datatype))
  (with-slots (id formals assuming constructors) adt
    `((:tag . :datatype)
      (:id . ,id)
      (:formals . ,(mlpp-decl* formals))
      (:assuming . ,(mlpp-decl* assuming))
      (:constructors . ,(mlpp-decl* constructors)))))

(defmethod mlpp-decl* ((decl inline-datatype))
  (with-slots (id constructors) decl
    `((:tag . :datatype)
      (:id . ,id)
      (:constructors . ,(mlpp-decl* constructors)))))

(defmethod mlpp-decl* ((c simple-constructor))
  (with-slots (id arguments recognizer) c
    `((:tag . :constructor)
      (:id . ,id)
      (:accessors . ,(mlpp-decl* arguments))
      (:recognizer . ,recognizer))))

(defmethod mlpp-decl* ((decl adtdecl))
  (with-slots (id declared-type type) decl
    `((:tag . :accessor)
      (:id . ,id)
      (:declared-type ,(list (mlpp* declared-type)))
      (:type ,(list (mlpp* type))))))
  
(defun mlpp-theory (theory)
  (mlpp* theory))

(defun mlpp-prelude ()
  (with-workspace (format nil "~a/lib/" *pvs-path*)
    (uiop:ensure-all-directories-exist (list "json/"))
    (dolist (th *prelude-theories*)
      (mlpp-theory-to-file th))))

(defun mlpp-all-libraries ()
  (dolist (lpair (pvs-library-alist))
    (mlpp-library (cdr lpair))))

(defun mlpp-library (lib &optional (theory-names '("top")))
  (with-workspace lib
    (unless nil ;(uiop:directory-exists-p "json")
      (uiop:ensure-all-directories-exist (list "json/"))
      (dolist (thname theory-names)
	(typecheck-file thname))
      (maphash #'(lambda (thid th)
		   (mlpp-theory-to-file th)
		   (let ((formulas (provable-formulas th)))
		     (dolist (formula formulas)
		       (collect-json-proof formula))))
	       (current-pvs-theories)))))

(defun mlpp-theory-to-file (th)
  (with-context th
    (let* ((*mlpp-type-hash* (make-pvs-hash-table))
	   (mlth (mlpp-theory th))
	   (thash (print-type-hash))
	   (mlmod `((:tag . :module-with-hash)
		    (:module ,mlth)
		    (:type-hash . ,thash))))
      (with-open-file (jdmp (format nil "json/~a.json" (id th))
			    :direction :output :if-exists :supersede
			    :if-does-not-exist :create)
	(json:encode-json mlmod jdmp)))))

(defvar *prover-input-list*)

(defun collect-prelude-json-proofs ()
  (dolist (th *prelude-theories*)
    (let ((formulas (provable-formulas th)))
      (dolist (formula formulas)
	(collect-json-proof formula)))))

(defun collect-json-proof (formula)
  (let* ((thdir (format nil "~a/json/~a-proofs/"
		  (context-path (module formula)) (id (module formula))))
	 (*dump-proof-data-to-file* (format nil "~a/~a.json" thdir (id formula)))
	 (*suppress-printing* t)
	 (*rewrite-msg-off* t) ;; *suppress-printing* isn't enough to shut off these messages
	 (*multiple-proof-default-behavior* :noquestions)
	 (def-prf (default-proof formula))
	 (*prover-input-list* (when def-prf
				(flatten-proof-script
				 (editable-justification (script def-prf))))))
    (uiop:ensure-all-directories-exist (list thdir))
    (with-prover-input-hook #'prover-list-input
      (prove-formula formula))))

(defun prover-list-input ()
  (if *prover-input-list*
      (pop *prover-input-list*)
      '(quit)))
