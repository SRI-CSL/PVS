;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-xml.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 29 23:19:42 1998
;; Lxml Modified By: Sam Owre
;; Lxml Modified On: Tue Jan 26 18:34:54 1999
;; Update Count    : 8
;; Status          : Alpha test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Copyright (c) 2002 SRI International, Menlo Park, CA 94025, USA.

(in-package :pvs)

(defmacro xsal-elt (stream eltname attrs &rest args)
  (let ((strm (gentemp)))
    `(let ((,strm ,stream))
       (pprint-indent :block 2 ,strm)
       (pprint-newline :linear ,strm)
       (pprint-logical-block (,strm nil)
	 (format ,strm "<~(~A~{ ~A=\"~A\"~}~)>" ',eltname ,attrs)
	 (format ,strm "~{~/pvs:pp-xml*/~}" (list ,@args))
	 (pprint-indent :block 0 ,strm)
	 (format ,strm "~_</~(~A~)>" ',eltname)))))

;;; Create structures so that we can get elements associated

(defstruct xml-formals list)

(defstruct xml-assuming list)

(defstruct xml-constr-args list)

(defstruct xml-recognizer id)

(defstruct xml-theory-id id)

(defstruct xml-library-id id)

(defstruct xml-actuals list)

(defstruct xml-mappings list)

(defstruct xml-bindings list)

(defstruct xml-index index)

(defstruct xml-ass-args list)

(defstruct xml-contains expr)

(defstruct xml-table-heading-expr expr)

(defstruct xml-table-headings headings)

(defstruct xml-table-entries entries)

(defstruct xml-row-entries entries)

(defun print-xml-prelude ()
  (dolist (theory *prelude-theories*)
    (format t "~%Generating ~a.xml" (id theory))
    (print-xml theory
	       :file (concatenate 'string (string (id theory)) ".xml"))))

(defun print-xml-pvs-file (filename)
  (let ((theories (typecheck-file filename nil nil nil t)))
    (with-open-file (str (format nil "~a.xml" filename)
			 :direction :output :if-exists :supersede)
      (dolist (theory theories)
	(print-xml theory :stream str)
	(format str "~2%")))))

(defun print-xml (obj &key string stream file char-width
		      length level lines (pretty t))
  (let ((*print-length* length)
	(*print-level* level)
	(*print-lines* lines)
	(*print-pretty* pretty)
	(*print-escape* nil)
	(*print-right-margin* (or char-width *default-char-width*)))
    (cond (string (with-output-to-string (stream)
		    (pp-xml obj stream)))
	  (stream (pp-xml obj stream))
	  (file (with-open-file (stream file :direction :output
					:if-exists :supersede)
		  (pp-xml obj stream)))
	  (t (pp-xml obj)))))

(defvar *pp-xml-bindings*)

(defun pp-xml (obj &optional (stream *standard-output*))
  (let ((*pp-xml-bindings* nil))
    (pprint-logical-block (nil nil)
      (pp-xml* stream obj))))

;;; Module level

(defmethod pp-xml* (stream (theories modules) &optional colon? atsign?)
  (pp-xml* stream (modules theories) colon? atsign?))

(defmethod pp-xml* (stream (mod module) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id formals exporting assuming theory) mod
    (xsal-elt stream theory (xml-attributes mod)
	      id
	      (when formals 
		(make-xml-formals :list formals))
	      (when assuming
		(make-xml-assuming :list assuming))
	      theory
	      exporting)))

(defmethod pp-xml* (stream (rtype recursive-type) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id formals importings constructors) rtype
    (if (datatype? rtype)
	(xsal-elt stream datatype (xml-attributes rtype)
		  id
		  (when formals 
		    (make-xml-formals :list formals))
		  importings
		  constructors)
	(xsal-elt stream codatatype (xml-attributes rtype)
		  id
		  (when formals 
		    (make-xml-formals :list formals))
		  importings
		  constructors))))

(defmethod pp-xml* (stream (constr adt-constructor) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id arguments recognizer) constr
    (xsal-elt stream constructor (xml-attributes constr)
	      id
	      (when arguments
		(make-xml-constr-args :list arguments))
	      (make-xml-recognizer :id (string recognizer)))))

(defmethod pp-xml* (stream (args xml-constr-args)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream accessors (xml-attributes args)
	    (xml-constr-args-list args)))

(defmethod pp-xml* (stream (rec xml-recognizer)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream recognizer (xml-attributes rec)
	    (xml-recognizer-id rec)))

(defmethod pp-xml* (stream (decl adtdecl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) decl
    (xsal-elt stream accessor (xml-attributes decl)
	      id type)))

(defmethod pp-xml* (stream (list list) &optional colon? atsign?)
  (dolist (elt list)
    (pp-xml* stream elt colon? atsign?)))

(defmethod pp-xml* (stream (list null) &optional colon? atsign?)
  (declare (ignore stream colon? atsign?))
  nil)

(defmethod pp-xml* (stream (formals xml-formals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream formals (xml-attributes formals)
	    (xml-formals-list formals)))

(defmethod pp-xml* (stream (assuming xml-assuming) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream assuming (xml-attributes assuming)
	    (xml-assuming-list assuming)))

(defmethod pp-xml* (stream (exp exporting) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (kind names but-names modules) exp
    (xsal-elt stream exporting (xml-attributes exp)
	      (pp-xml-exporting-names stream names)
	      (pp-xml-exporting-but-names stream but-names)
	      (pp-xml-exporting-kind stream kind)
	      (pp-xml-exporting-modules stream modules))))

(defun pp-xml-exporting-names (stream names)
  (when names
    (xsal-elt stream exporting-names
	      (pp-xml* stream names nil nil))))

(defun pp-xml-exporting-but-names (stream names)
  (when names
    (xsal-elt stream exporting-but-names
	      (pp-xml* stream names nil nil))))

(defun pp-xml-exporting-kind (stream kind)
  (xsal-elt stream exporting-kind nil
	    (format nil "~(~a~)" kind)))

(defun pp-xml-exporting-modules (stream names)
  (when names
    (xsal-elt stream exporting-theory-names
	      (pp-xml* stream names nil nil))))
	    

(defmethod pp-xml* (stream (name expname) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id kind type) name
    (xsal-elt stream export-name (xml-attributes name) id kind type)))


;;; Declarations

(defmethod pp-xml* (stream (imp importing) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (theory-name) imp
    (xsal-elt stream importing (xml-attributes imp) theory-name)))

(defmethod pp-xml* (stream (decl type-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id) decl
    (xsal-elt stream type-decl (xml-attributes decl) id)))

(defmethod pp-xml* (stream (decl nonempty-type-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id) decl
    (xsal-elt stream type-decl (xml-attributes decl) id)))

(defmethod pp-xml* (stream (decl type-def-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id formals type-value type-expr contains) decl
    (let ((*pp-xml-bindings* (append (car formals) *pp-xml-bindings*)))
      (xsal-elt stream type-decl (xml-attributes decl)
		id type-value type-expr
		(when contains
		  (make-xml-contains :expr contains))))))

(defmethod pp-xml* (stream (cont xml-contains) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream contains (xml-attributes cont)
	    (xml-contains-expr cont)))

(defmethod pp-xml* (stream (decl type-from-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type-expr contains) decl
    (xsal-elt stream type-from-decl (xml-attributes decl)
      id type-expr contains)))

(defmethod pp-xml* (stream (decl formal-subtype-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type-expr) decl
    (xsal-elt stream formal-subtype-decl (xml-attributes decl) id type-expr)))

(defmethod pp-xml* (stream (decl formal-nonempty-subtype-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type-expr) decl
    (xsal-elt stream formal-nonempty-subtype-decl (xml-attributes decl) id type-expr)))

(defmethod pp-xml* (stream (decl formal-const-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) decl
    (xsal-elt stream formal-const-decl (xml-attributes decl) id type)))

(defmethod pp-xml* (stream (decl lib-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id lib-string) decl
    (xsal-elt stream lib-decl (xml-attributes decl) id lib-string)))

(defmethod pp-xml* (stream (decl mod-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id modname) decl
    (xsal-elt stream mod-decl (xml-attributes decl) id modname)))

(defmethod pp-xml* (stream (decl var-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) decl
    (xsal-elt stream var-decl (xml-attributes decl) id type)))

(defmethod pp-xml* (stream (decl const-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type definition) decl
    (xsal-elt stream const-decl (xml-attributes decl) id type definition)))

(defmethod pp-xml* (stream (decl def-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type definition measure ordering) decl
    (xsal-elt stream def-decl (xml-attributes decl)
	      id type definition measure ordering)))

(defmethod pp-xml* (stream (decl formula-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id spelling definition default-proof) decl
    (case spelling
      (ASSUMPTION
       (xsal-elt stream assumption (xml-attributes decl)
		 id definition))
      ((AXIOM POSTULATE)
       (xsal-elt stream axiom-decl (xml-attributes decl)
		 id definition))
      (t
       (xsal-elt stream formula-decl (xml-attributes decl)
		 id definition default-proof)))))

(defmethod pp-xml* (stream (decl tcc-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id spelling definition) decl
    (xsal-elt stream tcc-decl (xml-attributes decl)
	      id definition)))

(defmethod xml-attributes ((decl formula-decl))
  (nconc (list 'kind (spelling decl)) (call-next-method)))

(defmethod pp-xml* (stream (decl name-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id name type) decl
    (xsal-elt stream name-judgement (xml-attributes decl) id name type)))

(defmethod pp-xml* (stream (decl application-judgement)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id name formals type) decl
    (let ((*pp-xml-bindings*
	   (append (apply #'append (formals decl)) *pp-xml-bindings*)))
      (xsal-elt stream application-judgement (xml-attributes decl)
		id type name (make-xml-bindings :list formals)))))

(defmethod pp-xml* (stream (decl number-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id number-expr type) decl
    (xsal-elt stream number-judgement (xml-attributes decl) id number-expr type)))

(defmethod pp-xml* (stream (decl subtype-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id subtype type declared-subtype) decl
    (let* ((bindings (when (type-application? declared-subtype)
		       (parameters declared-subtype)))
	   (*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (xsal-elt stream subtype-judgement (xml-attributes decl) id subtype type))))

(defmethod pp-xml* (stream (decl conversion-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expr) decl
    (xsal-elt stream conversion-decl (xml-attributes decl) expr)))


;;; Type expressions

(defmethod pp-xml* :around (stream (te type-expr) &optional colon? atsign?)
  (if (print-type te)
      (pp-xml* stream (print-type te) colon? atsign?)
      (call-next-method)))

(defmethod pp-xml* (stream (ex type-name) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (library mod-id actuals id mappings resolutions) ex
    (let ((mi (module-instance (car resolutions))))
      (xsal-elt stream type-name (xml-attributes ex)
		id
		(make-xml-theory-id :id (id mi))
		(unless (null (actuals mi))
		  (make-xml-actuals :list (actuals mi)))
		(unless (null (mappings mi))
		  (make-xml-mappings :list (mappings mi)))
		(unless (not (library mi))
		  (make-xml-library-id (library mi)))))))

(defmethod pp-xml* (stream (te type-application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (type parameters) te
    (xsal-elt stream type-application (xml-attributes te) type parameters)))

(defmethod pp-xml* (stream (ex field-assignment-arg) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id) ex
    (xsal-elt stream field-assign (xml-attributes ex) id)))

(defmethod pp-xml* (stream (ex proj-assign) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (number) ex
    (xsal-elt stream proj-assign (xml-attributes ex) number)))

(defmethod pp-xml* (stream (te subtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (supertype predicate) te
    (xsal-elt stream subtype (xml-attributes te) supertype predicate)))

(defmethod pp-xml* (stream (te expr-as-type) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expr supertype) te
    (xsal-elt stream expr-as-type (xml-attributes te) expr supertype)))

(defmethod pp-xml* (stream (te recordtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (fields) te
    (xsal-elt stream record-type (xml-attributes te) fields)))

(defun pp-xml-fields (stream fields &optional colon? atsign?)
  (when fields
    (let ((*pp-xml-bindings* (append (car fields) *pp-xml-bindings*)))
      (pp-xml* stream (car fields) colon? atsign?)
      (pp-xml-fields stream (cdr fields) colon? atsign?))))

(defmethod pp-xml* (stream (ex field-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) ex
    (xsal-elt stream field-decl (xml-attributes ex) id type)))

(defmethod pp-xml* (stream (te funtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (domain range) te
    (let ((*pp-xml-bindings*
	   (append (when (dep-binding? domain) (list domain))
		   *pp-xml-bindings*)))
      (xsal-elt stream function-type (xml-attributes te) domain range))))

(defmethod pp-xml* (stream (te tupletype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (types) te
    (xsal-elt stream tuple-type (xml-attributes te) types)))

;; (defun pp-xml-tupletype (stream types &optional colon? atsign?)
;;   (when types
;;     (let ((*pp-xml-bindings*
;; 	   (nconc (new-pp-xml-bindings
;; 		   (when (dep-binding? (car types)) (car types)))
;; 		  *pp-xml-bindings*)))
;;       (pp-xml* stream (car types) colon? atsign?)
;;       (pp-xml-tupletype stream (cdr types) colon? atsign?))))

(defmethod pp-xml* (stream (ex dep-binding) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) ex
    (xsal-elt stream binding (xml-attributes ex) id type)))

;;; Expressions

(defmethod pp-xml* (stream (ex number-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream number-expr (xml-attributes ex) (number ex)))

(defmethod pp-xml* (stream (ex string-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (unless (string-value ex)
    (setf (string-value ex) (pp-string-expr (argument ex))))
  (xsal-elt stream string-expr (xml-attributes ex) (string-value ex)))

(defmethod pp-xml* (stream (ex record-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (assignments) ex
    (xsal-elt stream record-expr (xml-attributes ex) assignments)))

(defmethod pp-xml* (stream (ex tuple-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (exprs) ex
    (xsal-elt stream tuple-expr (xml-attributes ex) exprs)))

(defmethod pp-xml* (stream (ex projection-application)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (index argument) ex
    (xsal-elt stream proj-appl-expr (xml-attributes ex)
	      argument
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex xml-index) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream index nil (xml-index-index ex)))
  
(defmethod pp-xml* (stream (ex field-application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id argument) ex
    (xsal-elt stream field-appl-expr (xml-attributes ex) argument id)))

(defmethod pp-xml* (stream (ex application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (operator argument) ex
    (xsal-elt stream application (xml-attributes ex) operator argument)))

(defmethod pp-xml* (stream (ex binding-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (xsal-elt stream binding-expr (xml-attributes ex) (operator ex) bindings expression))))

(defmethod pp-xml* (stream (ex lambda-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (xsal-elt stream lambda-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (ex forall-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (xsal-elt stream forall-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (ex exists-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (xsal-elt stream exists-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (bindings xml-bindings) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream bindings (xml-attributes bindings)
	    (xml-bindings-list bindings)))

(defmethod pp-xml* (stream (bd bind-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) bd
    (assert (memq bd *pp-xml-bindings*))
;;     (assert (not (member bd *pp-xml-bindings*
;; 			 :test #'(lambda (x y) (and (not (eq x y))
;; 						    (eq (id x) (id y))))))
;; 	    () "Different bindings with same identifier")
    (xsal-elt stream binding (xml-attributes bd) id type)))

(defmethod pp-xml* (stream (ex update-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expression assignments) ex
    (xsal-elt stream update-expr (xml-attributes ex) expression assignments)))

(defmethod pp-xml* (stream (ex cond-expr) &optional colon? atsign?)
  (pp-xml-cond-expr stream ex colon? atsign?))

(defmethod pp-xml* (stream (ex first-cond-expr) &optional colon? atsign?)
  (pp-xml-cond-expr stream ex colon? atsign?))

(defmethod pp-xml* (stream (ex single-cond-expr) &optional colon? atsign?)
  (pp-xml-cond-expr stream ex colon? atsign?))

(defmethod pp-xml* (stream (ex last-cond-expr) &optional colon? atsign?)
  (pp-xml-cond-expr stream ex colon? atsign?))

(defun pp-xml-cond-expr (stream ex colon? atsign?)
  (declare (ignore colon? atsign?))
  (let ((pairs (collect-cond-expr-pairs ex nil)))
    (xsal-elt stream cond-expr (xml-attributes ex) pairs)))

(defmethod pp-xml* (stream (ex cases-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expression selections else-part) ex
    (xsal-elt stream cases-expr (xml-attributes ex) expression selections else-part)))
      
(defmethod pp-xml* (stream (sel selection) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (constructor args expression) sel
    (let ((*pp-xml-bindings* (append args *pp-xml-bindings*)))
      (xsal-elt stream selection (xml-attributes sel)
		constructor
		(when args
		  (make-xml-bindings :list args))
		expression))))

(defmethod pp-xml* (stream (ass assignment) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (arguments expression) ass
    (xsal-elt stream assignment (xml-attributes ass)
	      (make-xml-ass-args :list arguments)
	      expression)))

(defmethod pp-xml* (stream (args xml-ass-args) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream assignment-args (xml-attributes args)
	    (xml-ass-args-list args)))

(defmethod pp-xml* (stream (ass maplet) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (arguments expression) ass
    (xsal-elt stream maplet (xml-attributes ass) arguments expression)))

(defmethod pp-xml* (stream (ex table-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-xml-table-expr stream ex row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp-xml* (stream (ex cond-table-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-xml-table-expr stream ex row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp-xml* (stream (ex cases-table-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-xml-table-expr stream ex row-expr col-expr row-headings col-headings table-entries)))

(defmethod pp-xml* (stream (ex let-table-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-xml-table-expr stream ex row-expr col-expr row-headings col-headings table-entries)))

(defun pp-xml-table-expr (stream table-expr row-expr col-expr
				 row-headings col-headings table-entries)
  (xsal-elt stream table-expr (xml-attributes table-expr)
	    (make-xml-table-heading-expr :expr row-expr)
	    (make-xml-table-heading-expr :expr col-expr)
	    (make-xml-table-headings :headings row-headings)
	    (make-xml-table-headings :headings col-headings)
	    (make-xml-table-entries
	     :entries (mapcar #'(lambda (re)
				  (make-xml-row-entries :entries re))
			table-entries))))

(defmethod pp-xml* (stream (thex xml-table-heading-expr)
			   &optional colon? atsign?)
  (xsal-elt stream table-heading-expr nil (xml-table-heading-expr-expr thex)))

(defmethod pp-xml* (stream (th xml-table-headings) &optional colon? atsign?)
  (xsal-elt stream table-headings nil (xml-table-headings-headings th)))

(defmethod pp-xml* (stream (te xml-table-entries) &optional colon? atsign?)
  (xsal-elt stream table-entries nil (xml-table-entries-entries te) nil nil))

(defmethod pp-xml* (stream (te xml-row-entries) &optional colon? atsign?)
  (xsal-elt stream row-entries nil (xml-row-entries-entries te) nil nil))

(defmethod pp-xml* (stream (ex name-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (library mod-id actuals id mappings resolutions type) ex
    (let* ((decl (declaration (car resolutions)))
	   (binding? (memq decl *pp-xml-bindings*))
	   (mi (unless binding? (module-instance (car resolutions)))))
      (assert (or (not binding?) (binding? decl)))
;;       (assert (or (not binding?)
;; 		  (not (member decl *pp-xml-bindings*
;; 			       :test #'(lambda (x y)
;; 					 (and (not (eq x y))
;; 					      (eq (id x) (id y)))))))
;; 	      () "Different bindings with same identifier")
      (xsal-elt stream name-expr (xml-attributes ex)
		id
		(unless binding?
		  (make-xml-theory-id :id (id mi)))
		(unless (or binding?
			    (null (actuals mi)))
		  (make-xml-actuals :list (actuals mi)))
		(unless (or binding?
			    (null (mappings mi)))
		  (make-xml-mappings :list (mappings mi)))
		(unless (or binding? (not (library mi)))
		  (make-xml-library-id (library mi)))
		type))))

(defmethod pp-xml* (stream (tid xml-theory-id) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream theory-id nil (string (xml-theory-id-id tid))))

(defmethod pp-xml* (stream (tid xml-library-id) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream library-id nil (string (xml-library-id-id tid))))

(defmethod pp-xml* (stream (actuals xml-actuals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xsal-elt stream actuals (xml-attributes actuals)
	    (xml-actuals-list actuals)))

(defmethod pp-xml* (stream (map mapping) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (lhs rhs) map
    (xsal-elt stream mapping (xml-attributes map) lhs rhs)))

(defmethod pp-xml* (stream (act actual) &optional colon? atsign?)
  (with-slots (expr type-value) act
    (pp-xml* stream (or type-value expr) colon? atsign?)))

(defmethod pp-xml* (stream (ex modname) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals) ex
    (xsal-elt stream theory-name (xml-attributes ex) id actuals)))

(defmethod pp-xml* (stream (ex symbol) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~2I~_~@<<~A~@[ ~A~]>~A</~A>~:>"
    "id" nil (translate-characters-to-xml-string (string ex)) "id")
  )

(defmethod pp-xml* (stream (ex number) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~d" ex))

(defmethod pp-xml* (stream (ex string) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~a" ex))

;;; Prootrees

(defmethod pp-xml* (stream (ex top-proofstate) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (declaration) ex
    (xsal-elt stream proofstate (xml-attributes ex)
	      (xml-declref declaration) (call-next-method))))

(defun xml-declref (declaration)
  (format nil "<declref>~a</declref>" (id declaration)))

(defmethod pp-xml* (stream (ex proofstate) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (label status-flag current-goal current-rule current-xrule
		     comment current-subgoal done-subgoals pending-subgoals
		     remaining-subgoals) ex
    (xsal-elt stream justification (xml-attributes ex)
	      (collect-justification ex))))

(defmethod pp-xml* (stream (ex justification) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xml-sexp (editable-justification ex nil nil nil) stream)
  (xml-sexp (clean-xrule (editable-justification ex nil t nil)) stream))

(defun xml-sexp (form stream)
  (when form
    (xsal-elt stream label nil (car form))
    (dolist (elt (cdr form))
      (xml-strat elt stream))))

(defun xml-strat (form stream)
  (if (consp form)
      (if (symbolp (car form))
	  (xsal-elt stream strategy nil (string (car form))
		    (xml-strat-args (cdr form) stream))
	  (break))
      (break)))

(defun xml-strat-args (args stream)
  (when args
    (cond ((keywordp (car args))
	   (xsal-elt stream arg (list 'key (car args)) (cadr args))
	   (xml-strat-args (cddr args) stream))
	  (t (xsal-elt stream arg nil (car args))
	     (xml-strat-args (cdr args) stream)))))

(defun clean-xrule (xrule)
  (if (consp xrule)
      (if (stringp (car xrule))
	  (cons (car xrule)
		(mapcan #'clean-xrule* (cdr xrule)))
	  (cons (clean-xrule (car xrule))
		(clean-xrule (cdr xrule))))
      xrule))

(defun clean-xrule* (xrule)
  (if (and (consp xrule)
	   (eq (car xrule) 'apply)
	   (consp (cadr xrule))
	   (eq (caadr xrule) 'rerun)
	   (consp (cadadr xrule))
	   (stringp (car (cadadr xrule))))
      (mapcan #'clean-xrule* (cdr (cadadr xrule)))
      (unless (equal xrule '(postpone))
	(list (clean-xrule xrule)))))

(defmethod pp-xml* (stream (ex sequent) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (n-sforms p-sforms) ex
    (xsal-elt stream sequent (xml-attributes ex) n-sforms p-sforms)))

(defmethod pp-xml* (stream (ex s-formula) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (formula) ex
    (xsal-elt stream formula (xml-attributes ex) formula)))

(defmethod pp-xml* (stream (ex proof-info) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id script create-date status) ex
    (break)
    (xsal-elt stream proof-info (xml-attributes ex)
	      id script create-date status)))

(defmethod xml-attributes :around ((ex syntax))
  (let ((attrs (call-next-method)))
    (if (place ex)
	(nconc (list 'place (format nil "~{~a~^ ~}" (place-list (place ex))))
	       attrs)
	attrs)))

(defmethod xml-attributes :around ((ex infix-application))
  (nconc (list 'infix "true") (call-next-method)))

(defmethod xml-attributes :around ((ex nonempty-type-decl))
  (nconc (list 'nonempty-type "true") (call-next-method)))

(defmethod xml-attributes (ex)
  (declare (ignore ex))
  nil)

(defun translate-characters-to-xml-string (string)
  (let ((chars nil))
    (dotimes (i (length string))
      (case (char string i)
	(#\" (push "&quot;" chars))
	(#\& (push "&amp;" chars))
	(#\' (push "&apos;" chars))
	(#\< (push "&lt;" chars))
	(#\> (push "&gt;" chars))
	(t   (push (string (char string i)) chars))))
    (apply #'concatenate 'string (nreverse chars))))
