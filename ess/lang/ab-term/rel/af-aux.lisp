;;; -*- Mode: Lisp; Package: ANALYSIS-FACILITY -*-
;;;
;;; Sccs Id @(#)af-aux.lisp	1.4 9/28/89");
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; AF-AUX.LISP - Ergo Analysis Facility
;;;
;;; ABSTRACT
;;; 
;;; Auxilliary routines called by the attribute grammar
;;;
;;; AUTHOR
;;;
;;;	R Nord
;;;
;;; HISTORY
;;;
;;;     07-22-87	rln	Initial development release.

(in-package "ANALYSIS-FACILITY")

(defconstant undefined-sym '_)

;;; Langauge interface

(defun get-lang-struct (lang-name)
  "Return the language structure for LANG-NAME and load associated sorts file."
  (let* ((lang-struct (lang:coerce-find-lang lang-name)))
    (unless (boundp (lang:lang-sort-table-name lang-struct))
      (unless (load (lang:lang-sorts-file lang-struct) :if-does-not-exist nil)
	(abox-error "Unable to load sorts file for (~A)" lang-name)))
    lang-struct))

;;; Error context support

(defvar *rulename* nil		"name of rule currently being analyzed.")
(defvar *operatorname* nil	"name of operator currently being analyzed.")

(defstruct errcon
  "Error context includes the sort and operator."
  sort
  oper)

(defun set-errcon-sort (sort errcon)
  "Set the sort field in the error context."
  (let ((copy (copy-errcon errcon)))
    (setf (errcon-sort copy) sort)
    copy))

(defun set-errcon-oper (oper errcon)
  "Set the operator filed in the error context."
  (let ((copy (copy-errcon errcon)))
    (setf (errcon-oper copy) oper)
    copy))

(defmacro in-error-context (errcon &body body)
  "Execute body in the error context."
  `(let ((*rulename* (if (errcon-p ,errcon) (errcon-sort ,errcon)))
	 (*operatorname* (if (errcon-p ,errcon) (errcon-oper ,errcon))))
     ,@body))


;;; External Grammars

(defvar *package-spec*)

(defun mk-ext-gram (gram pas package)
  "External grammar structure includes the gammar name, pass, and package."
  (if package (abox-warning "Packages for external grammars not implemented yet."))
  (list gram pas package))  ;(if package (symbol-name package) *package-spec*)))

(defun get-ext-gram (item extgram &key key)
  (ecase key
    (:gram (find item extgram :key #'first))
    (:pass (find item extgram :key #'second))))

(defun ext-gram-gram (extgram)
  (first extgram))

(defun ext-gram-pass (extgram)
  (second extgram))

(defun find-extgram (gram extgram)
  "Return extgram info if gram is defined in the external grammar list."
  (get-ext-gram gram extgram :key :gram))


;;; Signatures.  A signature structure consists of an
;;; alist having a nonterminal key and a list of attributes as data
;;; and a list of nonterminals.
     
(defun mk-sig (sig nont)
  (cons sig nont))

(defun ds-sig (sig)
  (values (car sig) (cdr sig)))

(defun mk-sig-null ()
  "Create an empty signature."
  (mk-sig nil nil))

(defun def-term-sig (idlist)
  "Define a signature given a list of terminal symbols."
  (let ((result (mk-sig-null)))
    (dolist (id idlist result)
      (cond ((equal "ID" (symbol-name id))
	     (setf result
		   (define-sort id (list (mk-attribute 'id-val nil 'syn)) result)))
	    ((equal "NUMBER" (symbol-name id))
	     (setf result
		   (define-sort id (list (mk-attribute 'num-val nil 'syn)) result)))
	    ((equal "STRING" (symbol-name id))
	     (setf result
		   (define-sort id (list (mk-attribute 'str-val nil 'syn)) result)))
	    (t (abox-warning "Invalid terminal ~A" id))))))

(defun define-sort (name attrs sig)
  "Add entry to the list  of attribute signatures and list of nonterminals."
  (multiple-value-bind (signatures nonterminals) (ds-sig sig)
    (define-signature name attrs (mk-sig signatures (cons name nonterminals)))))

(defun define-ext-signature (name attrs sig)
  "Add entry to the list  of attribute signatures and list of nonterminals."
  (multiple-value-bind (signatures nonterminals) (ds-sig sig)
    (define-signature
      name attrs (mk-sig
		  signatures (if (find name nonterminals) nonterminals
				 (cons name nonterminals))))))

(defun define-signature (name attrs sig)
  "Add entry to list of attribute signatures."
  (multiple-value-bind (signatures nonterminals) (ds-sig sig)
    (let ((entry (assoc name signatures)))
      (if entry
	  sig
	  (mk-sig (acons name attrs signatures) nonterminals)))))

;;; Verify that a nonterminal definition is consistent in signature
;;; with its previous occurrences.  It is an error to redefine a nonterminal.

(defun check-sort (name sig)
  "return true iff NAME is has not been defined in the signature SIG."
  (multiple-value-bind (signatures nonterminals) (ds-sig sig)
    (declare (ignore signatures))
    (let ((noerror t))
      (when (find name nonterminals)
	(setf noerror nil))
      noerror)))
    
;;; Verify that a nonterminal instance is consistent in signature
;;; with its previous occurrences.  The first time CHECK-SIGNATURE is called
;;; with a given name, an entry is made in the list *SIGNATURES*.  Subsequent
;;; calls check their arguments against the signature list.  Two attribute
;;; signatures match when they contain the same number of attributes and
;;; corresponding attributes have the same direction of propagation.

(defun check-signature (name attrs sig)
  "Return true iff a nonterminal NAME with attributes ATTRS is 
   consistent with its previous occurrences."
  (multiple-value-bind (signatures nonterminals) (ds-sig sig)
    (declare (ignore nonterminals))
    (let ((entry (assoc name signatures))
	  (noerror t))
      (if entry
	  (do ((a attrs (cdr a))
	       (b (cdr entry) (cdr b)))
	      ((and (null a) (null b)))
	    (unless (and a b (eq (attribute-direction (car a))
				 (attribute-direction (car b))))
	      ;; should unparse ATTRS below
	      (setf noerror nil))))
      noerror)))

(defun check-all-nonterminals-defined  (sign)
  "Return true iff all nonterminals referenced have been defined."
  (multiple-value-bind (signatures nonterminals) (ds-sig sign)
  (let ((noerror t))
    (dolist (sig signatures)
      (unless (find (car sig) nonterminals)
	(abox-warning "Sort ~A referenced but not defined" (car sig))
	(setf noerror nil)))
    noerror)))

(defun root-sort (sorts)
  "Return the first sort defined."
  (car sorts))


;;; Attribute definitions.

(defun enter-attrdef (attrdefs name value)
  "Enter an attribute definition in the ATTRDEFS environment
   checking that it is not already there."
  (if (lookp attrdefs name)
      (abox-error "Duplicate definiton of attribute ~A" name))
  (acons name (make-attrdef :definition value) attrdefs))

(defun enterinit (attrdefs ilist errcon)
  "Enter a list of attributes defined as themselves in the ATTRDEFS env."
  (in-error-context errcon
    (do ((ad attrdefs (enter-attrdef ad (car il) (car il)))
	 (il ilist (cdr il)))
	((null il) ad))))

(defun enterlist (attrdefs ilist value errcon)
  "Enter a list of attributes defined as VALUE u]in the ATTRDEFS env."
  (in-error-context errcon
    (do ((ad attrdefs (enter-attrdef ad (car il) value))
	 (il ilist (cdr il)))
	((null il) ad))))

(defun change-attrdef (attrdefs name value)
  "Change the definition of NAME to VALUE."
  (acons name (make-attrdef :definition value) attrdefs))

(defun changeinit (attrdefs ilist)
  "Change the definition of the list of attributes ILIST to themselves."
  (do ((ad attrdefs (change-attrdef ad (car il) (car il)))
       (il ilist (cdr il)))
      ((null il) ad)))

(defun lookup (env item)
  "Look up ITEM in ENV signalling an error if not found."
  (let ((entry (assoc item env)))
    (if entry (cdr entry) (abox-error "No defining occurrence for ~A" item))))

(defun lookp (env item)
  "Look up ITEM in ENV returning NIL if not found."
  (let ((entry (assoc item env)))
    (if entry (attrdef-definition (cdr entry)) nil)))

(defun bbp (attrs)
  "returns true iff attrs contains a bucket brigade attribute."
  (or (find 'last attrs :key #'attribute-kind)
      (find 'first attrs :key #'attribute-kind)))

(defun addtext (env itemlist)
  "Push each item in a list of items onto env."
  (dolist (item itemlist env)
    (unless (find item env) (push item env))))


;;; Tags for optional nonterminals

(defconstant dtag :default-tag)
(defvar *gentag-table* nil)

(defun gentag (tagname)
  "Return a unique symbol for tagname."
  (if (assoc tagname *gentag-table*)
      (cdr (assoc tagname *gentag-table*))
      (let ((tagvalue (gentemp)))
	(setf *gentag-table* (acons tagname tagvalue *gentag-table*))
	tagvalue)))

(defun entertag (tags tagname tagvalue)
  "Enter a tag and its associated value in the TAGS list."
  (unless (assoc tagname tags)
    (acons tagname tagvalue tags)))

(defun lookuptag (tags tag)
  "Return the tagvalue given a TAGname."
  (cdr (assoc tag tags)))

(defun lookuptagvalue (tags tagvalue)
  "Return the tagname geven a TAGVALUE."
  (car (find tagvalue tags :key #'cdr)))

(defun enteroptcon (optcon opt)
  "Add OPT to the optional context OPTCON."
  (cons opt optcon))

;;; Returns true iff attrname has not been defined within an optional
;;;  or if attrname has been defined within an optional 
;;;   appears within the optional context optcon.

(defun checkopt (attrdefs tags optcon attrname)
  (let ((attr (lookp attrdefs attrname)))
    (if (and (dp-visit-p attr) (dp-visit-optsort attr))
	(if (find (lookuptagvalue tags (dp-visit-optsort attr)) optcon)
	    t nil)
	t)))

;;; Returns true iff all attributes in ATTRS have not been defined within
;;; an optional in ATTRDEFS.

(defun check-opt-lhs (attrs attrdefs)
  (let ((noerror t))
    (dolist (attr attrs noerror)
      (let ((attrdef (lookp attrdefs (attribute-name attr))))
	(when (and (dp-visit-p attrdef) (dp-visit-optsort attrdefs))
	    (setf noerror nil)
	    (abox-warning "Illegal reference to optional attribute ~A"
			  (attribute-name attr)))))))

(defun check-opt-rhs (attrslist attrdefs)
  (let ((noerror t))
    (dolist (attrs attrslist noerror)
      (dolist (attr attrs noerror)
      (let ((attrdef (lookp attrdefs (attribute-name attr))))
	(when (and (inhp attr) (dp-visit-p attrdef) (dp-visit-optsort attrdef))
	    (setf noerror nil)
	    (abox-warning "Illegal reference to optional attribute ~A"
			  (attribute-name attr))))))))

	

;;; List checking

;;; Returns true iff all synthesized attributes in ATTRS that are referenced
;;; as a list are defined as a list in ATTRDEFS and all attributes that
;;; are not referenced as a list are not defined as a list in ATTRDEFS.

;;; Always returns true, this will only be a warning. Needs work.

(defun check-list (attrs attrdefs)
  (declare (ignore attrs attrdefs))
  t)

#|
(defun check-list (attrs attrdefs)
  (let ((noerror t))
    (dolist (attr attrs noerror)
      (let ((attrdef (lookp attrdefs (attribute-name attr))))
	(if (and (synp attr) (dp-visit-p attrdef))
	    (cond ((and (attribute-tlist attr) (not (dp-visit-tlist attrdef)))
		   (setf noerror nil)
		   (abox-warning "Attribute not defined as a list ~A"
				 (attribute-name attr)))
		  ((and (not (attribute-tlist attr)) (dp-visit-tlist attrdef))
		   (setf noerror nil)
		   (abox-warning "List attribute reference without * ~A"
				 (attribute-name attr)))))))
    t))
|#

;;; Always returns true, this will only be a warning.

(defun check-list-rhs (attrslist attrdefs)
  (let ((noerror t))
    (dolist (attrs attrslist noerror)
      (dolist (attr attrs noerror)
	(let ((attrdef (lookp attrdefs (attribute-name attr))))
	  (if (and (inhp attr) (dp-visit-p attrdef))
	      (cond ((and (attribute-tlist attr) (not (dp-visit-tlist attrdef)))
		     (setf noerror nil)
		     (abox-warning "Attribute not defined as a list ~A"
				   (attribute-name attr)))
		    ((and (not (attribute-tlist attr)) (dp-visit-tlist attrdef))
		     (setf noerror nil)
		     (abox-warning "List attribute reference without * ~A"
				   (attribute-name attr))))))))
    t))
  
;;; Returns true iff all attibutes in ATTRNAMES are not found in ITER-ATTRS.

(defun check-iter (attrnames iter-attrs)
  (let ((noerror t))
    (dolist (attrname attrnames noerror)
      (when (find attrname iter-attrs :key #'car)
	  (setf noerror nil)
	  (abox-warning "Attribute should not be declared `iterate' ~A" attrname)))))

(defun check-iter-rhs (attrs iter-attrs)
  (declare (ignore attrs iter-attrs))
  t)

;;; Returns true iff attrname has been defined as a list attribute

(defun attrlistp (attrdefs attrname)
  (let* ((attrdef (lookp attrdefs attrname))
	 (attr (if (dp-visit-p attrdef) (find attrname (dp-visit-tlist attrdef) :key #'attribute-name) nil)))
    (if attr (eq (attribute-kind attr) 'list))))
	 

(defun tlistp (tlist &aux sym)
  (setf sym  nil)
  (dolist (x tlist sym)
    (when x (setf sym x))))

(defun checkchild (tlist pos)
  (or (not tlist) (eq pos 1)))



;;; Given a list of left-hand-side attributes and a list of declarations,
;;; bash the FINAL, ITERATE, INITIAL, and TEST slots accordingly.  Return
;;; the modified attribute list.
;;; Aux are local attributes declared FINAL

(defun process-decls (attrs decls)
  (dolist (decl decls)
    (let ((declkind  (car decl))
	  (declattr  (cadr decl)))
      (let ((entry (find-if #'(lambda (x)
				(eq (attribute-name x) declattr)) attrs)))
	(unless entry
	  (setf entry (make-attribute :name declattr :direction 'aux))
	  (push entry attrs))
	(case declkind
	  ((fkind) (setf (attribute-final entry) t))
	  ((ikind) (setf (attribute-iterate entry) t)
	           (setf (attribute-initial entry) (caddr decl))
	           (setf (attribute-test    entry) (cadddr decl)))
	  (t       (abox-internal-error "Illegal declaration ~S" decl))))))
  attrs)


(defun get-mate (attr-name attrs)
  (attribute-mate (find attr-name attrs :key #'attribute-name)))

(defun mapmates (args results attrs)
  (mapcar #'(lambda (x) (if (find x results) (get-mate x attrs) undefined-sym))
	  args))

;;; Preprocess attributes of iterated nonterminals.  The MATE slots of
;;; bucket-brigade attributes are set to the name of the attribute's mate,
;;; i.e. the other attribute with the same tag.  An error is signalled if
;;; bucket-brigade attributes are not properly paired.

(defun find-mates (attrs errcon)
  (in-error-context errcon
  (dolist (item attrs attrs) 
    (when (member (attribute-kind item) '(first last))
      (let ((tag (attribute-tag item)))
	(do ((x attrs (cdr x)))
	    ((and (not (eq item (car x)))
		  (eq tag (attribute-tag (car x))))
	     (do ((y (cdr x) (cdr y)))
		 ((null y)
		  (setf (attribute-mate item) (attribute-name (car x))))
	       (when (and (not (eq item (car y)))
			  (eq (attribute-tag (car y)) tag))
		 (abox-error "Duplicate bucket-brigade tag ~A" tag))))
	  (when (null x)
	    (abox-error "No matching bucket-brigade tag ~A for ~A"
		   tag (attribute-name item)))))))))


(defun mk-family (confam synfam)
  "Family structure consisting of context and syntext."
  (cons confam synfam))

(defun family-confam (family)
  (car family))

(defun family-synfam (family)
  (cdr family))

(defun make-visit (family name attrs pos opt &optional (tlist nil))
  (multiple-value-bind (con syn) (split-attrs attrs)
    (make-dp-visit
     :nonterminal name
     :inherited (attr-names con)
     :synthesized (attr-names syn)
     :optsort opt
     :position pos
     :family family
     :tlist tlist)))

(defun mk-iterate (family name attrs pos opt initial test)
  (multiple-value-bind (con syn) (split-attrs attrs)
    (make-dp-iterate
     :nonterminal name
     :inherited (attr-names con)
     :synthesized (attr-names syn)
     :optsort opt
     :position pos
     :family family
     :initial initial
     :test test)))

(defun make-exp (exp &optional (au nil))
  (make-dp-eval :expression exp :attrsused au))

(defun make-exp-iterate (exp au initial test)
  (make-dp-eval-iterate :expression exp
			:attrsused au
			:initial initial
			:test test))

(defun mk-attribute (name kind direction &optional (tag nil) (lst nil))
  (make-attribute :name name :kind kind :direction direction
		  :tlist lst :tag tag))

(defun find-iter-eval (attr iter-attrs)
  (find attr iter-attrs :key #'first))

(defun find-iter (attrs iter-attrs)
  (let ((syns (get-syn-names attrs))
	(result nil))
    (dolist (iter iter-attrs (values-list result))
      (if (find (first iter) syns)
	  (push iter result)))))

(defun iter-init (iter)
  (second iter))

(defun iter-test (iter)
  (third iter))




;;; Return lists of inherited, synthesized and auxilliary attributes, 
;;; from a combined list of attribute specifiers.

(defun split-attrs (attrs)
  (split-attr-aux attrs nil nil nil))

(defun split-attr-aux (attrs inh syn aux)
  (cond ((null attrs)
	 (values (nreverse inh) (nreverse syn) (nreverse aux)))
	((eq (attribute-direction (car attrs)) 'inh)
	 (split-attr-aux (cdr attrs) (cons (car attrs) inh) syn aux))
	((eq (attribute-direction (car attrs)) 'syn)
	 (split-attr-aux (cdr attrs) inh (cons (car attrs) syn) aux))
	((eq (attribute-direction (car attrs)) 'aux)
	 (split-attr-aux (cdr attrs) inh syn (cons (car attrs) aux)))
	;; should unparse attribute
	(t (abox-internal-error "Bad attribute specifier - ~S" (car attrs)))))


(defun get-inh-names (attrs)
  (attr-names (split-attrs attrs)))

(defun get-syn-names (attrs)
  (multiple-value-bind (inh syn) (split-attrs attrs)
     (declare (ignore inh))
     (attr-names syn)))

(defun inhp (attr)
  (eq (attribute-direction attr) 'inh))

(defun synp (attr)
  (eq (attribute-direction attr) 'syn))

;;; Return lists of bucket brigade (first and last), list and distributed 
;;; attributes,
;;; from a combined list of attribute specifiers.

(defun list-attrs (attrs)
  (list-attr-aux attrs nil nil nil nil))

(defun list-attr-aux (attrs first last lst dst)
  (cond ((null attrs)
	 (values (nreverse first) (nreverse last) (nreverse lst) (nreverse dst)))
	((eq (attribute-kind (car attrs)) 'first)
	 (list-attr-aux (cdr attrs) (cons (car attrs) first) last lst dst))
	((eq (attribute-kind (car attrs)) 'last)
	 (list-attr-aux (cdr attrs) first (cons (car attrs) last) lst dst))
	((eq (attribute-kind (car attrs)) 'list)
	 (list-attr-aux (cdr attrs) first last (cons (car attrs) lst) dst))
	((eq (attribute-kind (car attrs)) 'dist)
	 (list-attr-aux (cdr attrs) first last lst (cons (car attrs) dst)))
	;; should unparse attribute
	(t (abox-internal-error "Bad attribute specifier - ~S" (car attrs)))))

(defmacro attr-bbf (attrs)
  `(values (list-attrs ,attrs)))

(defmacro attr-bbl (attrs)
  `(multiple-value-bind (bbf bbl) (list-attrs ,attrs)
     (declare (ignore bbf)) (values bbl)))

;;; Return list of attribute names, given a list of ATTRIBUTE structures.

(defun attr-names (attrs)
  (mapcar #'attribute-name attrs))

(defun unionlist (l)
  (do* ((ul l (cdr ul))
	(u (car ul) (union u (car ul))))
      ((null ul) u)))

(defun mapconstr (ec au)
  (mapcar #'make-exp ec au))

(defun addcode (env name code)
  (let ((entry (assoc name env)))
    (if entry (substitute (cons name (cons code (cdr entry))) name env :key #'car)
	(acons name (list code) env))))


;;; Error Handling

(defconstant error-type 0)
(defconstant warning-type 1)
(defconstant ierror-type 2)

(defun abox-message (mess-type mess)
  (let ((err-mess (ecase mess-type
		    (0 "Error")
		    (1 "Warning")
		    (2 "Internal Error"))))
    (if *rulename*
	(if *operatorname*
	    (format nil "ABox ~A, rule (~A), operator (~A): ~A~%"
		    err-mess *rulename* *operatorname* mess)
	    (format nil "ABox ~A, rule (~A): ~A~%" err-mess *rulename* mess))
	(format nil "Abox ~A: ~A~%" err-mess mess))))

(defun emessage (errcon message &rest args)
  "Constraint error message."
  (in-error-context errcon
    (abox-message error-type (apply #'format nil message args))))

(defun abox-warning (msg &rest args)
  (format t "~A" (abox-message warning-type (apply #'format nil msg args))))

;;; Signal a user-level error in the analysis facility.  Internal errors
;;; call ERROR to allow debugging in the break loop.

(defun abox-error (msg &rest args)
  "Signal error with throw 'abox-error-tag."
  (throw 'abox-error-tag
	 (abox-message error-type (apply #'format nil msg args))))
  ;;(error (abox-message error-type (apply #'format nil msg args))))   ; for debugging purposes

(defun abox-internal-error (msg &rest args)
  "Internal error."
  (error (abox-message ierror-type (apply #'format nil msg args))))

(defun symbol-catenate (&rest symbols)
  (do ((symnames (mapcar #'symbol-name symbols) (cdr symnames))
       (namelist nil (if (null (cdr symnames))
			 (cons (car symnames) namelist)
			 (cons "-" (cons (car symnames) namelist)))))
      ((null symnames)
       (intern (apply #'concatenate 'string (nreverse namelist))))))



(defun cons-undef (args)
  (cons undefined-sym args))

(defun mapargs (args results)
  "Return ARGS with any members appearing in results replaced with '_'."
  (mapcar #'(lambda (x) (if (find x results) x undefined-sym)) args))


(defun mapsort (oper pos nchild sort lang)
  (if (> nchild 1)
      (nthsort oper pos lang)
      sort))

(defun embed-sort (oper pos lang)
  (nthsort oper pos lang))

(defun embed-con (attrs attrdefs)
  "Context interface in an implicit sort are those attributes that are not
   defined locally in a definition clause, or in a visit."
  (let ((inhs (get-inh-names attrs)))
    (mapcan #'(lambda (x)
		(let ((attrdef (lookp attrdefs x)))
		  (if (or (dp-visit-p attrdef) (dp-eval-p attrdef))
		      nil
		      x)))
	    inhs)))

(defun embed-lhs (attrs attrdefs)
  "Context, syntext interface of an implicit sort are those contexts that are
   not defined locally and all syntexts."
  (multiple-value-bind (inh syn) (split-attrs attrs)
    (append
     (mapcan #'(lambda (x)
		 (let ((attrdef (lookp attrdefs (attribute-name x))))
		   (if (or (dp-visit-p attrdef) (dp-eval-p attrdef))
		       nil
		       (list x))))
	     inh)
     syn)))


(defun mergelist (l)
  (if (null l) l
      (append (car l) (mergelist (cdr l)))))

(defun is-attr (au ec)
  (eq ec (first au)))

(defun extract-attr (au ec)
  (declare (ignore au))
  ec)


;;; Langauge interface
;;; This will do for now until sb interface is set up
#+ergo-term
(defun nchildfun (tterm)
  (length (term:term-args (term:term-argn tterm 1))))

#+ergo-term
(defun nthsort (oper pos)
  (declare (ignore oper pos))
  :default-map-sort)

;;; SB language interface

(defun find-sym (sym)
  (when (symbolp sym)
    (setq sym (symbol-name sym)))
  (find-symbol sym (find-package *package-spec*)))


(defun find-opsig (oper opsig-table-name)
  (let ((opsig (sort:opsig-table-lookup
		(oper:mk-sim-op (find-sym oper)) (symbol-value opsig-table-name))))
    (unless opsig (abox-error "Unable to locate operator (~A) in opsig table."
			      oper))
    opsig))
  
#-ergo-term
(defun nthsort (oper pos lang)
  (let* ((opsig (find-opsig oper (lang:lang-opsig-table-name lang)))
	 (child-sort (nth pos (sort:opsig-inputs opsig))))
    (cond ((sort:is-sort-ttype child-sort)
	   (let ((sort-ttype (sort:ds-sort-ttype child-sort)))
	     (if (oper:is-sim-op sort-ttype)
		 (oper:ds-sim-op sort-ttype)
		 sort-ttype)))
	  ((sort:is-op-ttype child-sort)
	   (oper:ds-sim-op (sort:ds-op-ttype child-sort)))
	  (t (abox-internal-error "Cannot get sort from opsig ~S" child-sort)))))

#-ergo-term
(defun nchildfun (oper lang)
  (let ((opsig (find-opsig oper (lang:lang-opsig-table-name lang))))
    (length (sort:opsig-inputs opsig))))



