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

(defmacro xpvs-elt (stream eltname attrs &rest args)
  (let ((strm (gentemp)))
    `(let ((,strm ,stream))
       (format ,strm "<~(~A~)~{ ~A=\"~A\"~}>" ',eltname ,attrs)
       (format ,strm "~{~/pvs:pp-xml*/~}" (list ,@args))
       (format ,strm "</~(~A~)>" ',eltname))
    ;; `(let ((,strm ,stream))
    ;;    (pprint-indent :block 2 ,strm)
    ;;    (pprint-newline :linear ,strm)
    ;;    (pprint-logical-block (,strm nil)
    ;; 	 (format ,strm "<~(~A~)~{~_ ~A=\"~A\"~}>" ',eltname ,attrs)
    ;; 	 (format ,strm "~{~/pvs:pp-xml*/~}" (list ,@args))
    ;; 	 (pprint-indent :block 0 ,strm)
    ;; 	 (format ,strm "~_</~(~A~)>" ',eltname)))
    ))

;;; Create structures so that we can get elements associated

(defstruct xml-theory-formals list)

(defstruct xml-decl-formals list)

(defstruct xml-arg-formals list)

(defstruct xml-assuming list)

(defstruct xml-constr-args list)

(defstruct xml-recognizer id)

(defstruct xml-constr-subtype id)

(defstruct xml-enum-elts list)

(defstruct xml-theory-id id)

(defstruct xml-library-id id)

(defstruct xml-actuals list)

(defstruct xml-dactuals list)

(defstruct xml-mappings list)

(defstruct xml-target name)

(defstruct xml-bindings list)

(defstruct xml-index index)

(defstruct xml-ass-args list)

(defstruct xml-contains expr)

(defstruct xml-table-heading-expr expr)

(defstruct xml-table-headings headings)

(defstruct xml-table-entries entries)

(defstruct xml-row-entries entries)

(defstruct xml-decl-index index)

(defstruct xml-subgoal subgoal)

(defstruct xml-done-subgoals goals)

(defstruct xml-pending-subgoals goals)

(defstruct xml-remaining-subgoals goals)

(defstruct xml-rule rule)

(defstruct xml-xrule xrule)

(defstruct xml-rule-args args)

(defstruct xml-kind kind)

(defstruct xml-rewrite-name-spec fmla-or-type)

(defstruct xml-rewrite-name-kind kind)

(defstruct xml-nonempty-type nonempty?)

(defstruct xml-assignments list)

(defstruct xml-varname arg)

(defstruct xml-path path)

(defstruct xml-exporting-names list)

;; xml-proofstate and xml-top-proofstate are for proofstates, but
;; ignoring all slots which are only needed during a proof

(defstruct (xml-proofstate)
  label status-flag current-goal current-rule current-xrule comment current-subgoal
  done-subgoals pending-subgoals remaining-subgoals)

(defstruct (xml-top-proofstate (:include xml-proofstate))
  declaration)

(defun print-xml-prelude ()
  (dolist (theory *prelude-theories*)
    (let ((*default-pathname-defaults*
	   (make-pathname :directory (format nil "~a/lib" *pvs-path*)))
	  (*current-context* (context theory)))
      (print-xml-theory theory))))

(defun print-xml-importchain (theoryname)
  (let ((theory (get-typechecked-theory theoryname)))
    (when theory
      (let ((*current-context* (context theory)))
	(dolist (th (remove-if #'(lambda (th)
				   (or (from-prelude? th)
				       (lib-datatype-or-theory? th)))
		      (collect-theory-usings theoryname nil)))
	  (print-xml-theory th))))))

(defun print-xml-pvs-file (filename)
  (let ((theories (typecheck-file filename nil nil nil t)))
    (dolist (theory theories)
      (print-xml-theory theory))))

(defun print-xml-theory (theory)
  (ensure-xml-subdirectory)
  (print-xml theory :file (make-xmlpath (string (id theory))))
  ;;(print-xml (xml-collect-proofs theory)
  ;;	     :file (concatenate 'string (string (id theory)) "-proofs.xml"))
  )

(defun ensure-xml-subdirectory ()
  (let ((subdir (make-pathname
		 :defaults *default-pathname-defaults*
		 :name #+case-sensitive "pvsxml" #-case-sensitive "PVSXML")))
    (if (file-exists-p subdir)
	(if (directory-p subdir)
	    t
	    (pvs-message "~a is a regular file,~%  and can't be used as a~
                          subdirectory for xml files unless it is moved."
	      subdir))
	(multiple-value-bind (result err)
	    (ignore-lisp-errors #+allegro (excl:make-directory subdir)
				#+cmu (unix:unix-mkdir (namestring subdir) #o777)
				#+sbcl
				(sb-unix:unix-mkdir (namestring subdir) #o777))
	  (cond (result (pvs-message "Created directory ~a" subdir)
			t)
		(t (pvs-message "Error creating ~a: ~a" subdir err)
		   nil))))))

(defmethod make-xmlpath ((name string))
  (make-pathname :defaults *default-pathname-defaults*
		 :directory (append (pathname-directory *default-pathname-defaults*)
				    (list #+case-sensitive "pvsxml"
					  #-case-sensitive "PVSXML"))
		 :name name
		 :type "xml"))

(defun xml-collect-proofs (theory)
  ;; For now, we simply rerun each proof, and save an intermediate
  ;; representation in order to save space, as the entire proof tree can get
  ;; quite large.  The internal representation easily generates the XML
  ;; proof.
  (read-strategies-files)
  (mapcar #'(lambda (d)
	      (let ((*last-proof* nil))
		(pvs-prove-decl d t)
		(assert *last-proof*)
		(xml-collect-proof *last-proof*)))
    (provable-formulas theory)))

(defmethod xml-collect-proof ((ex top-proofstate))
  (with-slots (declaration label status-flag current-goal current-rule current-xrule
			   comment current-subgoal done-subgoals pending-subgoals
			   remaining-subgoals) ex
    (make-xml-top-proofstate
     :declaration declaration
     :label label
     :status-flag status-flag
     :current-goal current-goal
     :current-rule current-rule
     :current-xrule current-xrule
     :comment comment
     :current-subgoal (when current-subgoal
			(make-xml-subgoal
			 :subgoal (xml-collect-proof current-subgoal)))
     :done-subgoals (when done-subgoals
		      (make-xml-done-subgoals
		       :goals (mapcar #'xml-collect-proof done-subgoals)))
     :pending-subgoals (when pending-subgoals
			 (make-xml-pending-subgoals
			  :goals (mapcar #'xml-collect-proof pending-subgoals)))
     :remaining-subgoals (make-xml-remaining-subgoals
			  :goals (mapcar #'xml-collect-proof remaining-subgoals)))))

(defmethod xml-collect-proof ((ex proofstate))
  (with-slots (label status-flag current-goal current-rule current-xrule
		     comment current-subgoal done-subgoals pending-subgoals
		     remaining-subgoals) ex
    (when current-xrule (break "current-xrule"))
    (make-xml-proofstate
     :label label
     :status-flag status-flag
     :current-goal current-goal
     :current-rule current-rule
     :current-xrule current-xrule
     :comment comment
     :current-subgoal (xml-collect-proof current-subgoal)
     :done-subgoals (mapcar #'xml-collect-proof done-subgoals)
     :pending-subgoals (mapcar #'xml-collect-proof pending-subgoals)
     :remaining-subgoals (mapcar #'xml-collect-proof remaining-subgoals))))

(defmethod xml-collect-proof ((ex null))
  nil)
  
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
(defvar *pp-xml-declaration*)
(defvar *pp-xml-decl-bindings-ctr*)

(defun pp-xml (obj &optional (stream *standard-output*))
  (let ((*pp-xml-bindings* nil))
    ;;(pprint-logical-block (nil nil)
      (pp-xml* stream obj)
      ;;)
    ))

;;; Module level

(defmethod pp-xml* (stream (theories modules) &optional colon? atsign?)
  (pp-xml* stream (modules theories) colon? atsign?))

(defmethod pp-xml* (stream (mod module) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id formals exporting assuming theory) mod
    (write "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
	   :stream stream :escape nil)
    (terpri stream)
    (let ((*current-context* (context mod)))
      (xpvs-elt stream theory (xml-attributes mod)
		;; TBD source-file
		id
		(when formals 
		  (make-xml-theory-formals :list formals))
		;; TBD theory-formal-decls
		(when assuming
		  (make-xml-assuming :list assuming))
		(remove-if #'(lambda (d)
			       (and (judgement? d)
				    (judgement? (generated-by d))))
		  theory)
		exporting
		;; TBD generated-by
		;; TBD messages (info warnings conversion-messages)
		))))


(defmethod pp-xml* (stream (formals xml-theory-formals) &optional colon? atsign?)
  (xpvs-elt stream theory-formals (xml-attributes formals)
	    (pp-xml* stream (xml-theory-formals-list formals) colon? atsign?)))

(defmethod pp-xml* (stream (formals xml-decl-formals) &optional colon? atsign?)
  (xpvs-elt stream decl-formals (xml-attributes formals)
	    (pp-xml* stream (xml-decl-formals-list formals) colon? atsign?)))

(defmethod pp-xml* (stream (formals xml-arg-formals) &optional colon? atsign?)
  (xpvs-elt stream arg-formals (xml-attributes formals)
	    (pp-xml* stream (xml-arg-formals-list formals) colon? atsign?)))


(defmethod xml-attributes ((ex module))
  (list '|xmlns:xlink| "http://www.w3.org/1999/xlink"))

(defmethod pp-xml* :around (stream (rtype inline-recursive-type)
				   &optional colon? atsign?)
  (declare (ignore stream colon? atsign?))
  (let ((*pp-xml-declaration* rtype)
	(*pp-xml-decl-bindings-ctr* nil))
    (call-next-method)))

;;; TBD recursive-type-with-subtypes, or simply add subtypes to recursive-type

(defmethod pp-xml* (stream (rtype recursive-type) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id formals importings constructors) rtype
    (if (enumtype? rtype)
	(xpvs-elt stream enumtype-decl (xml-attributes rtype)
		  id
		  (make-xml-enum-elts
		   :list (mapcar #'id constructors)))
	(if (datatype? rtype)
	    (cond ((inline-datatype? rtype)
		   (xpvs-elt stream inline-datatype
			     (xml-attributes rtype)
			     id
			     ;; Only one of formals or decl-formals should be there
			     (when formals (make-xml-theory-formals :list formals))
			     (when (and (inline-recursive-type? rtype)
					(decl-formals rtype))
			       (make-xml-decl-formals :list (decl-formals rtype)))
			     importings
			     constructors))
		  (t
		   (xpvs-elt stream datatype
			     (xml-attributes rtype)
			     id
			     ;; Only one of formals or decl-formals should be there
			     (when formals (make-xml-theory-formals :list formals))
			     (when (and (inline-recursive-type? rtype)
					(decl-formals rtype))
			       (make-xml-decl-formals :list (decl-formals rtype)))
			     importings
			     constructors)
		   (when (adt-theory rtype)
		     (print-xml-theory (adt-theory rtype)))
		   (when (adt-map-theory rtype)
		     (print-xml-theory (adt-map-theory rtype)))
		   (when (adt-reduce-theory rtype)
		     (print-xml-theory (adt-reduce-theory rtype)))))
	    (cond ((inline-codatatype? rtype)
		   (xpvs-elt stream inline-codatatype (xml-attributes rtype)
			     id
			     (when formals (make-xml-theory-formals :list formals))
			     (when (and (inline-recursive-type? rtype)
					(decl-formals rtype))
			       (make-xml-decl-formals :list (decl-formals rtype)))
			     importings
			     constructors))
		  (t
		   (xpvs-elt stream codatatype (xml-attributes rtype)
			     id
			     (when formals (make-xml-theory-formals :list formals))
			     (when (and (inline-recursive-type? rtype)
					(decl-formals rtype))
			       (make-xml-decl-formals :list (decl-formals rtype)))
			     importings
			     constructors)
		   (when (adt-theory rtype)
		     (print-xml-theory (adt-theory rtype)))
		   (when (adt-map-theory rtype)
		     (print-xml-theory (adt-map-theory rtype)))
		   (when (adt-reduce-theory rtype)
		     (print-xml-theory (adt-reduce-theory rtype)))))))))

(defmethod pp-xml* (stream (elts xml-enum-elts) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream enum-elts nil
	    (xml-enum-elts-list elts)))

(defmethod xml-attributes ((ex recursive-type))
  (unless (inline-recursive-type? ex)
    (list '|xmlns:xlink| "http://www.w3.org/1999/xlink")))

(defmethod pp-xml* (stream (constr simple-constructor) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id arguments recognizer) constr
    (xpvs-elt stream constructor (xml-attributes constr)
	      id
	      (when arguments
		(make-xml-constr-args :list arguments))
	      (make-xml-recognizer :id (string recognizer))
	      (when (constructor-with-subtype? constr)
		(make-xml-constr-subtype :id (string (id (subtype constr))))))))

(defmethod xml-attributes ((constr simple-constructor))
  (with-slots (ordnum) constr
    (nconc (list 'ordnum ordnum) (call-next-method))))
    

(defmethod pp-xml* (stream (args xml-constr-args)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream accessors (xml-attributes args)
	    (xml-constr-args-list args)))

(defmethod pp-xml* (stream (rec xml-recognizer)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream recognizer (xml-attributes rec)
	    (xml-recognizer-id rec)))

(defmethod pp-xml* (stream (subtype xml-constr-subtype)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream subtype-id (xml-attributes subtype)
	    (xml-constr-subtype-id subtype)))

(defmethod pp-xml* (stream (decl adtdecl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) decl
    (xpvs-elt stream accessor (xml-attributes decl)
	      id type)))

(defmethod pp-xml* (stream (list list) &optional colon? atsign?)
  (pp-xml* stream (car list) colon? atsign?)
  (let ((*pp-xml-bindings* (if (binding? (car list))
			       (cons (car list) *pp-xml-bindings*)
			       *pp-xml-bindings*)))
    (when (binding? (car list))
      (increment-binding-count (car list)))
    (pp-xml* stream (cdr list) colon? atsign?)))

(defmethod pp-xml* (stream (list null) &optional colon? atsign?)
  (declare (ignore stream colon? atsign?))
  nil)

(defmethod pp-xml* (stream (assuming xml-assuming) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream assuming (xml-attributes assuming)
	    (xml-assuming-list assuming)))

(defmethod pp-xml* (stream (exp exporting) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (kind names but-names modules) exp
    (xpvs-elt stream exporting (xml-attributes exp)
	      (pp-xml-exporting-names stream names)
	      (pp-xml-exporting-but-names stream but-names)
	      (pp-xml-exporting-kind stream kind)
	      (pp-xml-exporting-modules stream modules))))

(defun pp-xml-exporting-names (stream names)
  (when (consp names) ; Don't generate for 'all, only need but-names
    (xpvs-elt stream exporting-names nil
	      (make-xml-exporting-names :list names))))

(defun pp-xml-exporting-but-names (stream names)
  (when names
    (xpvs-elt stream exporting-but-names nil
	      (make-xml-exporting-names :list names))))

(defun pp-xml-exporting-kind (stream kind)
  (xpvs-elt stream exporting-kind nil
	    (format nil "~(~a~)" kind)))

(defun pp-xml-exporting-modules (stream names)
  (when names
    (xpvs-elt stream exporting-theory-names nil
	      (make-xml-exporting-names :list names))))

(defmethod pp-xml* (stream (names xml-exporting-names) &optional colon? atsign?)
  (pp-xml* stream (xml-exporting-names-list names) colon? atsign?))
	    

(defmethod pp-xml* (stream (name expname) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id kind type) name
    (xpvs-elt stream export-name (xml-attributes name) id (or kind type))))


;;; Declarations

(defmethod pp-xml* (stream (imp importing-entity) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (theory-name) imp
    (let ((*pp-xml-declaration* imp)
	  (*pp-xml-decl-bindings-ctr* nil))
      (if (importing? imp)
	  (xpvs-elt stream importing (xml-attributes imp) theory-name)
	  (xpvs-elt stream theory-abbreviation (xml-attributes imp)
		    (id imp) theory-name)))))

(defmethod xml-attributes ((imp importing))
  (with-slots (semi chain?) imp
    (nconc (when semi (list 'semi-colon-p "true"))
	   (when chain? (list 'chain-p "true"))
	   (call-next-method))))

(defmethod pp-xml* :around (stream (decl declaration) &optional colon? atsign?)
  (declare (ignore stream colon? atsign?))
  (let ((*pp-xml-declaration* decl)
	(*pp-xml-decl-bindings-ctr*
	 (when (boundp '*pp-xml-decl-bindings-ctr*)
	   *pp-xml-decl-bindings-ctr*)))
    (call-next-method)))

(defmethod xml-attributes :around ((d declaration))
  (let ((attrs (call-next-method)))
    (nconc (when (chain? d) (list 'chain-p "true"))
	   (when (semi d) (list 'semi-colon-p "true"))
	   attrs)))

(defmethod pp-xml* (stream (decl type-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals) decl
    (xpvs-elt stream type-decl (xml-attributes decl)
	      id
	      (when decl-formals (make-xml-decl-formals :list decl-formals))
	      (make-xml-nonempty-type :nonempty? (nonempty-type-decl? decl))
	      (when formals (make-xml-arg-formals
			     :list (mapcar #'(lambda (fm)
					       (make-xml-bindings :list fm))
				     formals))))))

(defmethod pp-xml* (stream (ne xml-nonempty-type) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream nonempty-p nil
	    (if (xml-nonempty-type-nonempty? ne) "true" "false") nil nil))

;; (defmethod pp-xml* (stream (decl nonempty-type-decl) &optional colon? atsign?)
;;   (declare (ignore colon? atsign?))
;;   (with-slots (id decl-formals) decl
;;     (xpvs-elt stream type-decl (xml-attributes decl) id decl-formals)))

;; (defmethod xml-attributes :around ((ex nonempty-type-decl))
;;   (nconc (list 'nonempty-type-p "true") (call-next-method)))

(defmethod pp-xml* (stream (decl type-def-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals type-value type-expr contains) decl
    (let ((*pp-xml-bindings* (append (car formals) *pp-xml-bindings*)))
      (dolist (fml (car formals))
	(increment-binding-count fml))
      (typecase decl
	(struct-subtype-decl
	 (xpvs-elt stream struct-subtype-decl (xml-attributes decl)
		   id
		   (when decl-formals (make-xml-decl-formals :list decl-formals))
		   (when formals (make-xml-arg-formals
				  :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		   type-expr
		   (supertype decl)
		   (when contains (make-xml-contains :expr contains))))
	(type-from-decl
	 (xpvs-elt stream type-from-decl (xml-attributes decl)
		   id
		   (when decl-formals (make-xml-decl-formals :list decl-formals))
		   (when formals (make-xml-arg-formals
				  :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		   type-expr
		   (supertype decl)))
	(t
	 (xpvs-elt stream type-def-decl (xml-attributes decl)
		   id
		   (when decl-formals (make-xml-decl-formals :list decl-formals))
		   (when formals (make-xml-arg-formals
				  :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		   type-expr
		   type-value
		   (when contains (make-xml-contains :expr contains))))))))

(defmethod pp-xml* (stream (cont xml-contains) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream contains (xml-attributes cont)
	    (xml-contains-expr cont)))

(defmethod pp-xml* (stream (decl formal-type-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id) decl
    (xpvs-elt stream formal-type-decl (xml-attributes decl)
	      id
	      (make-xml-nonempty-type :nonempty? (nonempty-type-decl? decl)))))

(defmethod pp-xml* (stream (decl formal-subtype-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type-expr type-value predicate) decl
    (xpvs-elt stream formal-subtype-decl (xml-attributes decl)
	      id
	      (make-xml-nonempty-type :nonempty? (nonempty-type-decl? decl))
	      type-expr
	      type-value
	      predicate)))

(defmethod pp-xml* (stream (decl formal-struct-subtype-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type-expr projection) decl
    (xpvs-elt stream formal-struct-subtype-decl (xml-attributes decl)
	      id
	      (make-xml-nonempty-type :nonempty? (nonempty-type-decl? decl))
	      type-expr
	      projection)))

;; (defmethod pp-xml* (stream (decl formal-nonempty-subtype-decl) &optional colon? atsign?)
;;   (declare (ignore colon? atsign?))
;;   (with-slots (id type-expr) decl
;;     (xpvs-elt stream formal-nonempty-subtype-decl (xml-attributes decl) id type-expr)))

(defmethod pp-xml* (stream (decl formal-const-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id declared-type type) decl
    (xpvs-elt stream formal-const-decl (xml-attributes decl)
	      id declared-type type)))

(defmethod pp-xml* (stream (decl lib-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id lib-string) decl
    (xpvs-elt stream lib-decl (xml-attributes decl)
	      id
	      (make-xml-path :path lib-string))))

(defmethod pp-xml* (stream (path xml-path) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream path nil (xml-path-path path)))

(defmethod pp-xml* (stream (decl mod-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id modname) decl
    (xpvs-elt stream theory-decl (xml-attributes decl) id modname)))

(defmethod pp-xml* (stream (decl var-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id declared-type type) decl
    (xpvs-elt stream var-decl (xml-attributes decl) id declared-type type)))

(defmethod pp-xml* (stream (decl const-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals declared-type type definition) decl
    (let ((*pp-xml-bindings* (apply #'append formals)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream const-decl (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(when formals (make-xml-arg-formals
			       :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		declared-type type definition))))

(defmethod pp-xml* (stream (decl macro-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals declared-type type definition) decl
    (let ((*pp-xml-bindings* (apply #'append formals)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream macro-decl (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(when formals (make-xml-arg-formals
			       :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		declared-type type definition))))

(defmethod pp-xml* (stream (decl def-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals declared-type type definition measure ordering) decl
    (let ((*pp-xml-bindings* (apply #'append formals)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream def-decl (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(when formals (make-xml-arg-formals
			       :list (mapcar #'(lambda (fm)
						 (make-xml-bindings :list fm))
				       formals)))
		declared-type type definition measure ordering))))

(defmethod pp-xml* (stream (decl inductive-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals declared-type type definition) decl
    (let ((*pp-xml-bindings* (apply #'append formals)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream ind-decl (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(when formals (make-xml-arg-formals
			       :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		declared-type type definition))))

(defmethod pp-xml* (stream (decl coinductive-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals declared-type type definition) decl
    (let ((*pp-xml-bindings* (apply #'append formals)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream coind-decl (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(when formals (make-xml-arg-formals
			       :list (mapcar #'(lambda (fm)
						    (make-xml-bindings :list fm))
					  formals)))
		declared-type type definition))))

(defmethod pp-xml* (stream (decl formula-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id spelling definition default-proof) decl
    (case spelling
      (ASSUMPTION
       (xpvs-elt stream assumption (xml-attributes decl)
		 id definition))
      ((AXIOM POSTULATE)
       (xpvs-elt stream axiom-decl (xml-attributes decl)
		 id definition))
      (t
       (xpvs-elt stream formula-decl (xml-attributes decl)
		 id definition (xml-proof-reference default-proof))))))

(defun xml-proof-reference (proof)
  )

(defmethod pp-xml* (stream (decl tcc-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id spelling definition) decl
    (xpvs-elt stream tcc-decl (xml-attributes decl)
	      id definition)))

(defmethod xml-attributes ((decl formula-decl))
  (nconc (list 'kind (string-downcase (spelling decl))) (call-next-method)))

(defmethod pp-xml* (stream (decl name-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals name declared-type type) decl
    (xpvs-elt stream name-judgement (xml-attributes decl)
	      id
	      (when decl-formals (make-xml-decl-formals :list decl-formals))
	      name declared-type type)))

(defmethod pp-xml* (stream (decl number-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals number-expr declared-type type) decl
    (xpvs-elt stream number-judgement (xml-attributes decl)
	      id
	      (when decl-formals (make-xml-decl-formals :list decl-formals))
	      number-expr declared-type type)))

(defmethod pp-xml* (stream (decl application-judgement)
			   &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals name formals declared-type type judgement-type) decl
    (let ((*pp-xml-bindings*
	   (append (apply #'append formals) *pp-xml-bindings*)))
      (dolist (fmls formals)
	(dolist (fml fmls)
	  (increment-binding-count fml)))
      (xpvs-elt stream application-judgement (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		name
		(when formals
		  (make-xml-arg-formals
		   :list (mapcar #'(lambda (fm)
				     (make-xml-bindings :list fm))
			   formals)))
		declared-type
		type))))

(defmethod pp-xml* (stream (decl subtype-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals declared-subtype subtype declared-type type) decl
    (let* ((bindings (when (type-application? declared-subtype)
		       (parameters declared-subtype)))
	   (*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      ;; (dolist (binding bindings)
      ;; 	(increment-binding-count binding))
      (xpvs-elt stream subtype-judgement (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		declared-subtype subtype
		declared-type type))))

(defmethod pp-xml* (stream (decl expr-judgement) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id decl-formals formals expr declared-type type) decl
    (let* ((*pp-xml-bindings* (append formals *pp-xml-bindings*)))
      (dolist (binding formals)
	(increment-binding-count binding))
      (xpvs-elt stream expr-judgement (xml-attributes decl)
		id
		(when decl-formals (make-xml-decl-formals :list decl-formals))
		(make-xml-bindings :list formals)
		expr
		declared-type type))))

(defmethod pp-xml* (stream (fml xml-varname) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (let ((fm (xml-varname-arg fml)))
    (xpvs-elt stream varname-expr (xml-attributes fml)
	      (id fm)
	      (type fm))))

(defmethod xml-attributes ((fml xml-varname))
  (let ((fm (xml-varname-arg fml)))
    (when (place fm)
      (list 'place (format nil "~{~a~^ ~}" (place-list (place fm)))))))


(defmethod pp-xml* (stream (decl conversion-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expr) decl
    (xpvs-elt stream conversion-decl (xml-attributes decl)
	      (make-xml-kind
	       :kind (typecase decl
		       (conversionminus-decl "remove")
		       (t "add")))
	      expr)))

(defmethod pp-xml* (stream (kind xml-kind) &optional colon? atsign?)
  (xpvs-elt stream kind nil
	    (pp-xml* stream (xml-kind-kind kind) colon? atsign?)))

(defmethod xml-attributes ((decl conversion-decl))
  (nconc (list 'key (typecase decl
		      (conversionplus-decl "CONVERSION+")
		      (conversionminus-decl "CONVERSION-")
		      (t "CONVERSION")))))

(defmethod pp-xml* (stream (decl auto-rewrite-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (rewrite-names) decl
    (xpvs-elt stream auto-rewrite (xml-attributes decl)
	      (make-xml-kind
	       :kind (typecase decl
		       (auto-rewrite-minus-decl "remove")
		       (t "add")))
	      rewrite-names)))

(defmethod xml-attributes ((decl auto-rewrite-decl))
  (nconc (list 'key (typecase decl
		      (auto-rewrite-plus-decl "AUTO_REWRITE+")
		      (auto-rewrite-minus-decl "AUTO_REWRITE-")
		      (t "AUTO_REWRITE")))))

(defmethod pp-xml* (stream (name rewrite-name) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream rewrite-name (xml-attributes name)
	    (id name)
	    (when (mod-id name)
	      (make-xml-theory-id :id (mod-id name)))
	    (when (or (acts-there? name) (actuals name))
	      (make-xml-actuals :list (actuals name)))
	    (when (or (dacts-there? name) (dactuals name))
	      (make-xml-dactuals :list (dactuals name)))
	    (when (mappings name)
	      (make-xml-mappings :list (mappings name)))
	    (when (target name)
	      (make-xml-target :name (target name)))
	    (when (library name)
	      (make-xml-library-id :id (library name)))
	    (typecase name
	      (formula-rewrite-name (make-xml-rewrite-name-spec
				     :fmla-or-type (spelling name)))
	      (constant-rewrite-name (make-xml-rewrite-name-spec
				      :fmla-or-type (declared-type name))))
	    (typecase name
	      (lazy-rewrite (make-xml-rewrite-name-kind
			     :kind "lazy"))
	      (eager-rewrite (make-xml-rewrite-name-kind
			     :kind "eager"))
	      (macro-rewrite (make-xml-rewrite-name-kind
			     :kind "macro")))
	    (car (resolutions name))))

(defmethod pp-xml* (stream (spec xml-rewrite-name-spec) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream spec nil (xml-rewrite-name-spec-fmla-or-type spec)))

(defmethod pp-xml* (stream (kind xml-rewrite-name-kind) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream kind nil (xml-rewrite-name-kind-kind kind)))

(defmethod pp-xml* (stream (name formula-rewrite-name) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream rewrite-name (xml-attributes name)
	    (id name)
	    (when (mod-id name)
	      (make-xml-theory-id :id (mod-id name)))
	    (when (or (acts-there? name) (actuals name))
	      (make-xml-actuals :list (actuals name)))
	    (when (or (dacts-there? name) (dactuals name))
	      (make-xml-dactuals :list (dactuals name)))
	    (when (mappings name)
	      (make-xml-mappings :list (mappings name)))
	    (when (target name)
	      (make-xml-target :name (target name)))
	    (when (library name)
	      (make-xml-library-id :id (library name)))
	    (car (resolutions name))
	    (string-downcase (spelling name))))

(defmethod pp-xml* (stream (name constant-rewrite-name) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream rewrite-name (xml-attributes name)
	    (id name)
	    (when (mod-id name)
	      (make-xml-theory-id :id (mod-id name)))
	    (when (or (acts-there? name) (actuals name))
	      (make-xml-actuals :list (actuals name)))
	    (when (or (dacts-there? name) (dactuals name))
	      (make-xml-dactuals :list (dactuals name)))
	    (when (mappings name)
	      (make-xml-mappings :list (mappings name)))
	    (when (target name)
	      (make-xml-target :name (target name)))
	    (when (library name)
	      (make-xml-library-id :id (library name)))
	    (car (resolutions name))
	    (declared-type name)))

(defmethod pp-xml* (stream (name fnum-rewrite) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream rewrite-name (xml-attributes name)
	    (id name)
	    (when (mod-id name)
	      (make-xml-theory-id :id (mod-id name)))
	    (when (or (acts-there? name) (actuals name))
	      (make-xml-actuals :list (actuals name)))
	    (when (or (dacts-there? name) (dactuals name))
	      (make-xml-dactuals :list (dactuals name)))
	    (when (mappings name)
	      (make-xml-mappings :list (mappings name)))
	    (when (target name)
	      (make-xml-target :name (target name)))
	    (when (library name)
	      (make-xml-library-id :id (library name)))
	    (car (resolutions name))
	    (fnum name)))


;;; Type expressions

;;; We only keep the print type - not the canonical type.  Thus to do the
;;; equivalent of tc-eq in XML one must follow resolutions and do something
;;; similar to subst-mod-params.  This must be kept in mind when reading
;;; XML into PVS.
;;; TBD - is this the right way to do this?
(defmethod pp-xml* :around (stream (te type-expr) &optional colon? atsign?)
  (if (print-type te)
      (pp-xml* stream (print-type te) colon? atsign?)
      (call-next-method)))

 (defmethod pp-xml* (stream (ex type-name) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (library mod-id acts-there? actuals dacts-there? dactuals
		       id mappings target resolutions) ex
    (xpvs-elt stream type-name (xml-attributes ex)
	      id
	      (when mod-id
		(make-xml-theory-id :id mod-id))
	      (when (or acts-there? actuals)
		(make-xml-actuals :list actuals))
	      (when (or dacts-there? dactuals)
		(make-xml-dactuals :list dactuals))
	      (when mappings
		(make-xml-mappings :list mappings))
	      (when target
		(make-xml-target :name target))
	      (when library
		(make-xml-library-id :id library))
	      (car resolutions))))

(defmethod pp-xml* (stream (te type-application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (type parameters) te
    (xpvs-elt stream type-application (xml-attributes te)
	      type
	      (mapcar #'(lambda (p)
			  (if (binding? p)
			      (make-xml-varname :arg p)
			      p))
		parameters))))

(defmethod pp-xml* (stream (ex field-assignment-arg) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id) ex
    (xpvs-elt stream field-assign (xml-attributes ex) id)))

(defmethod pp-xml* (stream (ex proj-assign) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (number) ex
    (xpvs-elt stream proj-assign (xml-attributes ex) number)))

(defmethod pp-xml* (stream (te subtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (supertype predicate) te
    (xpvs-elt stream setsubtype (xml-attributes te) supertype predicate)))

;; (defmethod pp-xml* (stream (te setsubtype) &optional colon? atsign?)
;;   (declare (ignore colon? atsign?))
;;   (call-next-method))

;;; TBD nsetsubtype needed?

(defmethod pp-xml* (stream (te expr-as-type) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expr supertype) te
    (xpvs-elt stream expr-as-type (xml-attributes te)
	      expr (or supertype (type expr)))))

(defmethod pp-xml* (stream (te funtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (domain range) te
    (let ((*pp-xml-bindings*
	   (append (when (dep-binding? domain) (list domain))
		   *pp-xml-bindings*)))
      (when (dep-binding? domain)
	(increment-binding-count domain))
      (xpvs-elt stream function-type (xml-attributes te) domain range))))

(defmethod xml-attributes ((te functiontype))
  (nconc (list 'keyword "FUNCTION") (call-next-method)))

(defmethod xml-attributes ((te arraytype))
  (nconc (list 'keyword "ARRAY") (call-next-method)))

(defmethod pp-xml* (stream (te tupletype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (types) te
    (xpvs-elt stream tuple-type (xml-attributes te) types)))

;; TBD deal with dependent types
;; (defun pp-xml-tupletype (stream types &optional colon? atsign?)
;;   (when types
;;     (let ((*pp-xml-bindings*
;; 	   (nconc (new-pp-xml-bindings
;; 		   (when (dep-binding? (car types)) (car types)))
;; 		  *pp-xml-bindings*)))
;;       (pp-xml* stream (car types) colon? atsign?)
;;       (pp-xml-tupletype stream (cdr types) colon? atsign?))))

(defmethod pp-xml* (stream (te cotupletype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (types) te
    (xpvs-elt stream tuple-type (xml-attributes te) types)))

(defmethod pp-xml* (stream (te struct-sub-tupletype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (types) te
    (xpvs-elt stream structural-sub-tuple-type (xml-attributes te) types)))

(defmethod pp-xml* (stream (te recordtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (fields) te
    (xpvs-elt stream record-type (xml-attributes te) fields)))

(defmethod pp-xml* (stream (te struct-sub-recordtype) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (fields) te
    (xpvs-elt stream structural-sub-record-type (xml-attributes te) fields)))

(defun pp-xml-fields (stream fields &optional colon? atsign?)
  (when fields
    (let ((*pp-xml-bindings* (cons (car fields) *pp-xml-bindings*)))
      (increment-binding-count (car fields))
      (pp-xml* stream (car fields) colon? atsign?)
      (pp-xml-fields stream (cdr fields) colon? atsign?))))

(defmethod pp-xml* (stream (ex field-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) ex
    (xpvs-elt stream field-decl (xml-attributes ex) id type)))

(defmethod pp-xml* (stream (ex dep-binding) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) ex
    (xpvs-elt stream binding (xml-attributes ex) id type)))

(defmethod pp-xml* (stream (te type-extension) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (type extension) te
    (xpvs-elt stream type-extension (xml-attributes te) type extension)))


;;; Expressions

;;; Types are not included, except for names - should they be?
;;; and what about judgement types?

(defmethod xml-attributes :around ((ex expr))
  (if (and (integerp (parens ex))
	   (> (parens ex) 0))
      (nconc (list 'parens (parens ex)) (call-next-method))
      (call-next-method)))

(defmethod pp-xml* (stream (ex rational-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream rational-expr (xml-attributes ex)
	    (format nil "~d ~d" (numerator (number ex)) (denominator (number ex)))))

(defmethod pp-xml* (stream (ex number-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream number-expr (xml-attributes ex) (number ex)))

(defmethod pp-xml* (stream (ex string-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (unless (string-value ex)
    (setf (string-value ex) (pp-string-expr (argument ex))))
  (xpvs-elt stream string-expr (xml-attributes ex)
	    (translate-characters-to-xml-string (string-value ex))))

(defmethod pp-xml* (stream (ex record-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (assignments) ex
    (xpvs-elt stream record-expr (xml-attributes ex) assignments)))

(defmethod pp-xml* (stream (ex tuple-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (exprs) ex
    (xpvs-elt stream tuple-expr (xml-attributes ex) exprs)))

(defmethod pp-xml* (stream (ex projection-application)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (index argument) ex
    (xpvs-elt stream proj-appl-expr (xml-attributes ex)
	      argument
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex projection-expr)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals index) ex
    (xpvs-elt stream proj (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex injection-expr)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals index) ex
    (xpvs-elt stream inj (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex injection?-expr)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals index) ex
    (xpvs-elt stream inj? (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex extraction-expr)
		    &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals index) ex
    (xpvs-elt stream extraction (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      (make-xml-index :index index))))

(defmethod pp-xml* (stream (ex xml-index) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream index nil (xml-index-index ex)))
  
(defmethod pp-xml* (stream (ex field-application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals argument) ex
    (xpvs-elt stream field-appl-expr (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      argument)))

(defmethod pp-xml* (stream (ex application) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (operator argument) ex
    (xpvs-elt stream application (xml-attributes ex) operator argument)))

(defmethod pp-xml* (stream (ex binding-expr) &optional colon? atsign?)
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (dolist (bd bindings)
	(increment-binding-count bd))
      (when (lambda-expr-with-type? ex)
	(pp-xml* stream (declared-ret-type ex) colon? atsign?))
      (xpvs-elt stream binding-expr (xml-attributes ex) (operator ex)
		bindings expression))))

(defmethod pp-xml* (stream (ex lambda-expr) &optional colon? atsign?)
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (dolist (bd bindings)
	(increment-binding-count bd))
      (when (lambda-expr-with-type? ex)
	(pp-xml* stream (declared-ret-type ex) colon? atsign?))
      (xpvs-elt stream lambda-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (ex forall-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (dolist (bd bindings)
	(increment-binding-count bd))
      (xpvs-elt stream forall-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (ex exists-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (bindings expression) ex
    (let ((*pp-xml-bindings* (append bindings *pp-xml-bindings*)))
      (dolist (bd bindings)
	(increment-binding-count bd))
      (xpvs-elt stream exists-expr (xml-attributes ex)
		(make-xml-bindings :list bindings)
		expression))))

(defmethod pp-xml* (stream (bindings xml-bindings) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream bindings (xml-attributes bindings)
	    (xml-bindings-list bindings)))

(defmethod pp-xml* (stream (bd bind-decl) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id type) bd
    (assert (memq bd *pp-xml-bindings*))
;;     (assert (not (member bd *pp-xml-bindings*
;; 			 :test #'(lambda (x y) (and (not (eq x y))
;; 						    (eq (id x) (id y))))))
;; 	    () "Different bindings with same identifier")
    (xpvs-elt stream binding (xml-attributes bd) id type)))

;; (defmethod xml-attributes ((bd bind-decl))
;;   (nconc (list '|id| (xml-declaration-id bd)) (call-next-method)))

(defmethod pp-xml* (stream (ex update-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expression assignments) ex
    (xpvs-elt stream update-expr (xml-attributes ex)
	      expression
	      (make-xml-assignments :list assignments))))

(defmethod pp-xml* (stream (ass xml-assignments) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream assignments nil
	    (xml-assignments-list ass)))

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
    (xpvs-elt stream cond-expr (xml-attributes ex) pairs)))

(defmethod pp-xml* (stream (ex cases-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (expression selections else-part) ex
    (xpvs-elt stream cases-expr (xml-attributes ex) expression selections else-part)))
      
(defmethod pp-xml* (stream (sel selection) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (constructor args expression) sel
    (let ((*pp-xml-bindings* (append args *pp-xml-bindings*)))
      (dolist (bd args)
	(increment-binding-count bd))
      (xpvs-elt stream selection (xml-attributes sel)
		constructor
		(when args
		  (make-xml-bindings :list args))
		expression))))

(defmethod pp-xml* (stream (ass assignment) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (arguments expression) ass
    (xpvs-elt stream assignment (xml-attributes ass)
	      (make-xml-ass-args :list arguments)
	      expression)))

(defmethod pp-xml* (stream (args xml-ass-args) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream assignment-args (xml-attributes args)
	    (xml-ass-args-list args)))

(defmethod pp-xml* (stream (ass maplet) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (arguments expression) ass
    (xpvs-elt stream maplet (xml-attributes ass)
	      (make-xml-ass-args :list arguments) expression)))

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
  (xpvs-elt stream table-expr (xml-attributes table-expr)
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
  (declare (ignore colon? atsign?))
  (xpvs-elt stream table-heading-expr nil (xml-table-heading-expr-expr thex)))

(defmethod pp-xml* (stream (th xml-table-headings) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream table-headings nil (xml-table-headings-headings th)))

(defmethod pp-xml* (stream (te xml-table-entries) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream table-entries nil (xml-table-entries-entries te) nil nil))

(defmethod pp-xml* (stream (te xml-row-entries) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream row-entries nil (xml-row-entries-entries te) nil nil))

(defmethod pp-xml* (stream (ex name-expr) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (library mod-id acts-there? actuals dacts-there? dactuals
		       id mappings target resolutions type) ex
    (let* ((decl (declaration (car resolutions)))
	   (binding? (memq decl *pp-xml-bindings*))
	   ;;(mi (unless binding? (module-instance (car resolutions))))
	   )
      (assert (or (not binding?) (binding? decl)))
;;       (assert (or (not binding?)
;; 		  (not (member decl *pp-xml-bindings*
;; 			       :test #'(lambda (x y)
;; 					 (and (not (eq x y))
;; 					      (eq (id x) (id y)))))))
;; 	      () "Different bindings with same identifier")
      (if (variable? ex)
	  (xpvs-elt stream varname-expr (xml-attributes ex)
		    id
		    type)
	  (xpvs-elt stream name-expr (xml-attributes ex)
		    id
		    (when mod-id
		      (make-xml-theory-id :id mod-id))
		    (when (or acts-there? actuals)
		      (make-xml-actuals :list actuals))
		    (when (or dacts-there? dactuals)
		      (make-xml-dactuals :list dactuals))
		    (when mappings
		      (make-xml-mappings :list mappings))
		    (when target
		      (make-xml-target :name target))
		    (when library
		      (make-xml-library-id :id library))
		    type
		    (car resolutions))))))

(defmethod pp-xml* (stream (res resolution) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (module-instance declaration) res
    (xpvs-elt stream resolution nil
	      module-instance (pp-xml-decl-index declaration))))

(defmethod pp-xml-decl-index (decl)
  (make-xml-decl-index :index (xml-declaration-index decl)))

(defmethod pp-xml-decl-index ((bd binding))
  (break "Should not get here"))

(defmethod pp-xml* (stream (dref xml-decl-index) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream index nil (xml-decl-index-index dref)))

;; (defmethod xml-attributes ((dref xml-declref))
;;   (list '|xlink:href| (xml-declref-ref dref)))


(defmethod xml-declaration-index (decl)
  (assert (and (module decl) (memq decl (all-decls (module decl)))))
  (let* ((id (or (id decl) (ref-to-id decl)))
	 (pos (position decl (remove-if #'(lambda (x)
					    (or (importing? x)
						(not (eq (ref-to-id x) id))))
			       (all-decls (module decl))))))
    pos))

(defmethod xml-declaration-index ((decl decl-formal))
  (assert (and (associated-decl decl)
	       (memq decl (decl-formals (associated-decl decl)))))
  (let* ((id (or (id decl) (ref-to-id decl)))
	 (pos (position decl (remove-if #'(lambda (x)
					    (or (importing? x)
						(not (eq (ref-to-id x) id))))
			       (decl-formals (associated-decl decl))))))
    pos))

(defmethod xml-declaration-index ((decl skolem-const-decl))
  (let* ((id (id decl))
	 (opt (assq id *pvs-operators*)))
    (break "Rethink this")
    (translate-characters-to-xml-string
     (if opt
	 (format nil "~a." (cdr opt))
	 (string id)))))

(defun increment-binding-count (binding)
  (assert (binding? binding))
  (let ((entry (assq (id binding) *pp-xml-decl-bindings-ctr*)))
    (if entry
	(incf (cdr entry))
	(setq *pp-xml-decl-bindings-ctr*
	      (acons (id binding) 0 *pp-xml-decl-bindings-ctr*)))))

;; (defmethod xml-declaration-index ((bd binding))
;;   (break "Should not get here")
;;   (assert *pp-xml-declaration*)
;;   (assert (memq bd *pp-xml-bindings*))
;;   (let ((pos (position bd (remove bd *pp-xml-bindings* :test-not #'same-id)))
;; 	(cnt (cdr (assq (id bd) *pp-xml-decl-bindings-ctr*))))
;;     (assert pos)
;;     (assert cnt)
;;     (translate-characters-to-xml-id 
;;      (format nil "~a-~a~@[-~d~]~@[-~d~]"
;;        (xml-declaration-id *pp-xml-declaration*)
;;        (id bd)
;;        (unless (zerop pos) pos) (unless (zerop cnt) cnt)))))

(defmethod xml-declaration-index ((imp importing))
  (break "What?")
  (let* ((theory (current-theory))
	 (tdecls (all-decls theory))
	 (decls (memq imp tdecls))
	 (dimp (find-if
		   #'(lambda (d)
		       (and (typep d '(or importing
					  theory-abbreviation-decl))
			    (not (chain? d))))
		 decls))
	 (remimps (remove-if-not
		      #'(lambda (d)
			  (and (typep d '(or importing
					     theory-abbreviation-decl))
			       (not (chain? d))))
		    tdecls)))
    (makesym "IMPORTING~d" (1+ (position dimp remimps)))))

(defmethod pp-xml* (stream (tid xml-theory-id) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream theory-id nil (string (xml-theory-id-id tid))))

(defmethod pp-xml* (stream (tid xml-library-id) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream library-id nil (string (xml-library-id-id tid))))

(defmethod pp-xml* (stream (actuals xml-actuals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream actuals (xml-attributes actuals)
	    (xml-actuals-list actuals)))

(defmethod pp-xml* (stream (actuals xml-dactuals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream dactuals (xml-attributes actuals)
	    (xml-dactuals-list actuals)))

(defmethod pp-xml* (stream (mappings xml-mappings) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream mappings (xml-attributes mappings)
	    (xml-mappings-list mappings)))

(defmethod pp-xml* (stream (map mapping) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (lhs rhs) map
    (xpvs-elt stream mapping (xml-attributes map) lhs rhs)))

(defmethod pp-xml* (stream (act actual) &optional colon? atsign?)
  (with-slots (expr type-value) act
    (pp-xml* stream (or type-value expr) colon? atsign?)))

(defmethod pp-xml* (stream (ex modname) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id actuals dactuals library mappings) ex
    (xpvs-elt stream theory-name (xml-attributes ex)
	      id
	      (when actuals (make-xml-actuals :list actuals))
	      (when dactuals (make-xml-dactuals :list dactuals))
	      (when library (make-xml-library-id :id library))
	      (when mappings (make-xml-mappings :list mappings)))))

(defmethod pp-xml* (stream (ex symbol) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~@<<~A~@[ ~A~]>~A</~A>~:>"
    "id" nil (translate-characters-to-xml-string (string ex)) "id")
  ;; (format stream "~2I~_~@<<~A~@[ ~A~]>~A</~A>~:>"
  ;;   "id" nil (translate-characters-to-xml-string (string ex)) "id")
  )

(defmethod pp-xml* (stream (ex number) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~d" ex))

(defmethod pp-xml* (stream (ex string) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (format stream "~a" ex))

;;; Prootrees

(defmethod pp-xml* (stream (ex xml-top-proofstate) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (declaration (goal current-goal) (rule current-rule)
			   (xrule current-xrule) (subgoal current-subgoal)
			   done-subgoals pending-subgoals remaining-subgoals) ex
    (let ((*pp-xml-decl-bindings-ctr* nil)
	  (*pp-xml-declaration* declaration))
      (xpvs-elt stream top-proofstate (xml-attributes ex)
		(pp-xml-decl-index declaration)
		(when rule (make-xml-rule :rule rule))
		(when xrule (make-xml-xrule :xrule xrule))
		goal subgoal done-subgoals pending-subgoals remaining-subgoals))))

(defmethod pp-xml* (stream (ex xml-proofstate) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots ((goal current-goal) (rule current-rule) (xrule current-xrule)
	       (subgoal current-subgoal) done-subgoals pending-subgoals
	       remaining-subgoals) ex
    (xpvs-elt stream proofstate (xml-attributes ex)
	      (when rule (make-xml-rule :rule rule))
	      (when xrule (make-xml-xrule :xrule xrule))
	      goal subgoal done-subgoals pending-subgoals remaining-subgoals)))

(defmethod xml-attributes ((ex xml-proofstate))
  (with-slots (label comment status-flag) ex
    (nconc (list 'label label 'status-flag (string status-flag))
	   (when comment (list 'comment comment))
	   (call-next-method))))

(defmethod pp-xml* (stream (ex xml-rule) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (rule) ex
    (xpvs-elt stream rule nil
	      (car rule)
	      (make-xml-rule-args :args (cdr rule)))))

(defmethod pp-xml* (stream (ex xml-rule-args) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (args) ex
    (xpvs-elt stream rule-args nil args)))

(defmethod pp-xml* (stream (ex xml-xrule) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (xrule) ex
    (xpvs-elt stream xrule nil
	      (car xrule)
	      (make-xml-rule-args :args (cdr xrule)))))

(defmethod pp-xml* (stream (ex xml-subgoal) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream subgoal nil (xml-subgoal-subgoal ex)))

(defmethod pp-xml* (stream (ex xml-done-subgoals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream done-subgoals nil (xml-done-subgoals-goals ex)))

(defmethod pp-xml* (stream (ex xml-pending-subgoals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream pending-subgoals nil (xml-pending-subgoals-goals ex)))

(defmethod pp-xml* (stream (ex xml-remaining-subgoals) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (xpvs-elt stream remaining-subgoals nil (xml-remaining-subgoals-goals ex)))

(defun xml-sexp (form stream)
  (when form
    (xpvs-elt stream label nil (car form))
    (dolist (elt (cdr form))
      (xml-strat elt stream))))

(defun xml-strat (form stream)
  (if (consp form)
      (if (symbolp (car form))
	  (xpvs-elt stream strategy nil (string (car form))
		    (xml-strat-args (cdr form) stream))
	  (break))
      (break)))

(defun xml-strat-args (args stream)
  (when args
    (cond ((keywordp (car args))
	   (xpvs-elt stream arg (list 'key (car args)) (cadr args))
	   (xml-strat-args (cddr args) stream))
	  (t (xpvs-elt stream arg nil (car args))
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
    (xpvs-elt stream sequent (xml-attributes ex) n-sforms p-sforms)))

(defmethod pp-xml* (stream (ex s-formula) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (formula) ex
    (xpvs-elt stream formula (xml-attributes ex) formula)))

(defmethod pp-xml* (stream (ex proof-info) &optional colon? atsign?)
  (declare (ignore colon? atsign?))
  (with-slots (id script create-date status) ex
    (break)
    (xpvs-elt stream proof-info (xml-attributes ex)
	      id script create-date status)))

(defmethod xml-attributes :around ((ex syntax))
  (let ((attrs (call-next-method)))
    (if (place ex)
	(nconc (list 'place (format nil "~{~a~^ ~}" (place-list (place ex))))
	       attrs)
	attrs)))

(defmethod xml-attributes :around ((b bind-decl))
  (let ((attrs (call-next-method)))
    (if (chain? b)
	(nconc (list 'chain-p "true") attrs)
	attrs)))

(defmethod xml-attributes :around ((ex infix-application))
  (nconc (list 'infix "true") (call-next-method)))

(defmethod xml-attributes :around ((ex xml-top-proofstate))
  (nconc (list '|xmlns:xlink| "http://www.w3.org/1999/xlink")
	 (call-next-method)))

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

;;; xsd:ID characters consist of letters, digits, underscores, hyphens, and
;;; periods.
(defun translate-characters-to-xml-id (string)
  (if (every #'xml-name-char-p string)
      string
      (let ((chars nil))
	(dotimes (i (length string))
	  (let ((ch (char string i)))
	    (if (xml-name-char-p ch)
		(push (string ch) chars)
		;; The other chars are all below 0, between 9 and A, between Z
		;; and a, or above z.
		(push 
		 (case ch
		   (#\! "-excl")
		   (#\# "-shrp")
		   (#\$ "-dlr")
		   (#\% "-pct")
		   (#\& "-amp")
		   (#\* "-star")
		   (#\+ "-plus")
		   (#\/ "-slsh")
		   (#\< "-lt")
		   (#\= "-eql")
		   (#\> "-gt")
		   (#\? "-p")
		   (#\@ "-ats")
		   (#\^ "-crt")
		   (#\~ "-tlda")
		   (t (break "something's wrong")))
		 chars))))
	(apply #'concatenate 'string (nreverse chars)))))

(defun xml-name-char-p (ch)
  (or (alpha-char-p ch)
      (digit-char-p ch)
      (member ch '(#\_ #\- #\.) :test #'char=)))

;;; Proof collection

(defun xml-collect-rule (ps)
  (and (or (children ps)
	   (eq (status-flag ps) '!))
       (or (current-rule ps)
	   (unless
	       ;; This can happen with checkpoints
	       (and (consp (current-input ps))
		    (eq (car (current-input ps)) 'lisp))
	     (current-input ps)))))

;;; Parsing XML

;; #+allegro
;; (defun parse-xml (file)
;;   (with-open-file (fxml file)
;;     (let ((lxml (net.xml.parser:parse-xml fxml :content-only t)))
;;       (remove-empty-strings lxml))))

(defun empty-string? (str)
  (and (stringp str)
       (every #'(lambda (c) (member c '(#\Space #\Tab #\Newline #\Return))) str)))

(defun remove-empty-strings (lxml)
  (cond ((null lxml) nil)
	((consp lxml)
	 (mapcan #'(lambda (elt)
		     (unless (empty-string? elt)
		       (list (remove-empty-strings elt))))
	   lxml))
	((symbolp lxml)
	 (intern (string lxml) :pvs))
	(t lxml)))
