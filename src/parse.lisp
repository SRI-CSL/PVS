;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; parse.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Oct 21 19:36:29 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Nov  4 18:13:31 1998
;; Update Count    : 54
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

(defvar *allowed-ids* nil)

(defvar *allowed-typed-names* nil)

(defvar *newline-comments* nil)

;;; Makes extensive use of the following functions from ERGO:
;;;	sim-term-op - returns the symbol which is the operator of a term
;;;	is-sop	    - checks whether a given symbol is the operator of a term
;;;	term-args   - returns the list of arguments of a term
;;;	ds-id       - returns the symbol of an id term

;;; Parsing

(defun parse (&rest keys)
  (let* ((*current-file* (or (cadr (member :file keys))
			     *current-file*))
	 (start-time (when *current-file* (get-internal-real-time)))
	 (*newline-comments* nil))
    (init-parser)
    (multiple-value-bind (term error? place msg args)
	(handler-case 
	    (apply #'pvs-parse :return-errors t keys)
	  #+sbcl
	  (sb-int:stream-decoding-error (err)
	    (let ((msg
		   (format nil
		       "Error: ~a~%This error usually is due to a character that is not UTF-8~
                       ~%Run 'file ~a', and if it is not ASCII or UTF-8,~
                       ~%change it using something like~
                       ~%  iconv -f latin1 -t UTF-8 < ~a > ~a.copy~
                       ~%and move the .copy file after checking it with 'file'"
		     err *current-file* *current-file* *current-file*)))
	      (parse-error nil msg))))
      (when error?
	(let ((pargs (mapcar #'(lambda (arg)
				 (if (stringp arg)
				     (protect-format-string arg)
				     arg))
		       args)))
	  (parse-error place (parse-error-msg msg pargs))))
      (let* ((nt (cadr (member :nt keys)))
	     (fn (if nt
		     (makesym #+(and allegro (version>= 6)) "xt-~(~a~)"
			      #-(and allegro (version>= 6)) "XT-~a"
			      (sim-term-op term))
		     #'xt-adt-or-modules))
	     (*parsing-or-unparsing* t)
	     (parsed-object (funcall fn term)))
	;;(assert-places-set)
	(values parsed-object
		(if *current-file*
		    (realtime-since start-time)
		    0)
		(nreverse *newline-comments*))))))

(defun init-parser ()
  (setq *table-bracket-counter* 0)
  (setq *double-braces-counter* 0)
  (setq *elsif-places* nil)
  (setq *operator-places* nil))

(defun parse-error-msg (message args)
  (multiple-value-bind (found expected)
      (cond ((initial-error? message)
	     (values (car args) (cadr args)))
	    ((lam-error? message)
	     (values (cadr args) (car args)))
	    ((medial-error? message)
	     (values (car args) (cadr args))))
    (if found
	(format nil "Found '~A' when expecting '~A'~
                     ~:[~;~%  Note: May need a ';' before declaring an infix operator~]~
                     ~@[~%  Note: '~a' is now a keyword~]~
                     ~@[~%  Note: '~a' is only allowed in theory declarations (not importings)~]"
	  found expected
	  (string= found ":")
	  (when (string= found "AS") found)
	  (when (and (member found '("::=" "=") :test #'string=)
		     ;; Check that ':=' is expected
		     (>= (length (string (cadr args))) 2)
		     (string= (cadr args) ":=" :end1 2))
	    found))
	(format nil "~?" message args))))

(defun initial-error? (message)
  (and (stringp message)
       (>= (length message) 16)
       (string= message "Initial error." :start1 2 :end1 16)))

(defun lam-error? (message)
  (and (stringp message)
       (>= (length message) 27)
       (string= message "Look ahead set match error." :start1 0 :end1 27)))

(defun medial-error? (message)
  (and (stringp message)
       (>= (length message) 13)
       (string= message "Medial error." :start1 0 :end1 13)))

(defun parse-expr (string)
  (init-parser)
  (pvs-parse :string string :nt 'expr))

(defun psa (string &optional (nt 'adt-or-modules))
  (init-parser)
  (pvs-parse :string string :nt nt))

(defun ps (string &optional (nt 'adt-or-modules))
  (init-parser)
  (let* ((term (pvs-parse :string string :nt nt))
	 (fn (makesym "XT-~a" (sim-term-op term))))
    (funcall fn term)))

(defvar *escaped-operators-used* nil)

(defun xt-adt-or-modules (adt-or-modules)
  (mapcar #'(lambda (arg) (funcall #'xt-adt-or-module arg))
	  (term-args adt-or-modules)))

(defun xt-adt-or-module (adt-or-module)
  (let* ((*escaped-operators-used* nil)
	 (id (ds-vid (term-arg0 adt-or-module)))
	 (formals (xt-theory-formals (term-arg1 adt-or-module)))
	 (adt-or-mod (term-arg2 adt-or-module))
	 (aorm (case (sim-term-op adt-or-mod)
		 (MODULE (funcall #'xt-module adt-or-mod id))
		 ((DATATYPE CODATATYPE)
		  (funcall #'xt-datatype (sim-term-op adt-or-mod)
			   adt-or-mod id))
		 ((DATATYPES CODATATYPES)
		  (funcall #'xt-datatypes (sim-term-op adt-or-mod)
			   adt-or-mod id)))))
    (setf (id aorm) id
	  (formals aorm) formals
	  (place aorm) (term-place adt-or-module))
    ;;(assert (every #'place (formals aorm)))
    (when *escaped-operators-used*
      (let ((*current-context* (make-new-context aorm)))
	(pvs-info "This theory uses the operator~p ~:*~{~a~^ ~}~
                 ~%Remember that the backslash character must be doubled ~
                 in strings~%E.g., (expand \"/\\\\\")"
	  *escaped-operators-used*)))
    (when *current-file*
      (setf (filename aorm) (pathname-name *current-file*)))
    (setf (context-path aorm) *default-pathname-defaults*)
    aorm))

(defun xt-datatypes (class datatype &optional id inline)
  (let* ((subtypes-term (term-arg0 datatype))
	 (subtypes (mapcar #'(lambda (st)
			       (make-instance 'type-name
				 :id (xt-idop (xt-pidop st))
				 :place (term-place st)))
			   (term-args subtypes-term)))
	 (adtbody (term-arg1 datatype))
	 (endidop (xt-pidop (term-arg2 datatype)))
	 (endid (ds-vid (term-arg0 endidop))))
    (unless (or (null id)
		(eq id endid))
      (parse-error endidop
	"End id ~a does not match datatype id ~a" endid id))
    (multiple-value-bind (imps assuming adtcases)
	(xt-adtbody (term-args adtbody))
      (check-subtypes-constructors-consistency id subtypes adtcases)
	(make-instance (if (or (null id) inline)
			   (if (eq class 'DATATYPES)
			       'inline-datatype-with-subtypes
			       'inline-codatatype-with-subtypes)
			   (if (eq class 'DATATYPES)
			       'datatype-with-subtypes
			       'codatatype-with-subtypes))
	  :subtypes subtypes
	  :assuming assuming
	  :importings imps
	  :constructors adtcases
	  :place (term-place datatype)))))

(defun check-subtypes-constructors-consistency (id subtypes adtcases)
  (let ((badsubtype (find id subtypes :key #'id)))
    (when badsubtype
      (parse-error badsubtype "May not use datatype name as a subtype")))
  (let ((dup (duplicates? subtypes :test #'same-id)))
    (when dup
      (parse-error dup "Duplicate subtypes are not allowed")))
  (let ((badconstr (find-if #'(lambda (x)
				(or (not (typep x 'constructor-with-subtype))
				    (not (member (subtype x) subtypes
						 :test #'same-id))))
		     adtcases)))
    (when badconstr
      (if (typep badconstr 'constructor-with-subtype)
	  (parse-error (subtype badconstr)
	    "The subtype for this constructor was not declared above")
	  (parse-error badconstr
	    "Must specify a subtype for this constructor"))))
  (let ((badsubtype (find-if-not #'(lambda (s)
				     (some #'(lambda (a)
					       (same-id s (subtype a)))
					   adtcases))
		      subtypes)))
    (when badsubtype
      (parse-error badsubtype
	"This subtype has no associated constructors"))))

(defun xt-theory-formals (theory-formals &optional decl-formal?)
  (mapcan #'(lambda (ta) (xt-theory-formal ta decl-formal?))
    (term-args theory-formals)))

(defun change-to-decl-formal (decl)
  (change-class decl
      (typecase decl
	(formal-nonempty-struct-subtype-decl 'decl-formal-nonempty-struct-subtype)
	(formal-struct-subtype-decl 'decl-formal-struct-subtype)
	(formal-nonempty-subtype-decl 'decl-formal-nonempty-subtype)
	(formal-subtype-decl 'decl-formal-subtype)
	(formal-nonempty-type-decl 'decl-formal-nonempty-type)
	(formal-type-decl 'decl-formal-type)
	(formal-nonempty-type-appl-decl 'decl-formal-nonempty-type-appl)
	(formal-type-appl-decl 'decl-formal-type-appl)
	(formal-const-decl 'decl-formal-const-decl)
	(formal-theory-decl 'decl-formal-theory-decl))))

(defun xt-theory-formal (theory-formal &optional decl-formal?)
  (let* ((importing (term-arg0 theory-formal))
	 (idops (xt-check-periods (term-arg1 theory-formal)))
	 (decl-body (term-arg2 theory-formal)))
    (multiple-value-bind (decl dtype)
	(xt-declaration-body decl-body)
      (when decl-formal?
	;; (unless (formal-type-decl? decl)
	;;   (parse-error theory-formal
	;;     "Only type declarations allowed for declaration parameters"))
	(change-to-decl-formal decl))
      (append (unless (is-sop 'THEORY-FORMAL-NULL-1 importing)
		(xt-formal-importing-elt importing))
	      (xt-chained-decls (term-args idops) nil dtype nil decl
				theory-formal)))))

(defun xt-datatype (class datatype &optional id inline)
  (let* ((adtbody (term-arg0 datatype))
	 (endidop (xt-pidop (term-arg1 datatype)))
	 (endid (ds-vid (term-arg0 endidop))))
    (unless (or (null id)
		(eq id endid))
      (parse-error (term-arg1 datatype)
	"End id ~a does not match datatype id ~a" endid id))
    (multiple-value-bind (imps assuming adtcases)
	(xt-adtbody (term-args adtbody))
      (make-instance (if (or (null id) inline)
			 (if (eq class 'DATATYPE)
			     'inline-datatype
			     'inline-codatatype)
			 (if (eq class 'DATATYPE)
			     'datatype
			     'codatatype))
	:assuming assuming
	:importings imps
	:constructors adtcases
	:place (term-place datatype)))))

(defun xt-adtbody (adtbody &optional imps assuming adtcases)
  (if (null adtbody)
      (values imps assuming (nreverse adtcases))
      (case (sim-term-op (car adtbody))
	(ID-THEORY-DECL
	 (if (or assuming adtcases)
	     (parse-error (car adtbody) "Cannot have assuming here")
	     (let ((id (xt-check-periods (term-arg0 (car adtbody))))
		   (tdecl (xt-theory-decl (term-arg1 (car adtbody)))))
	       (setf (id tdecl) (xt-idop (xt-pidop id)))
	       (xt-adtbody (cdr adtbody) (cons tdecl imps) assuming adtcases))))
	(IMPORTING-ELT
	 (if (or assuming adtcases)
	     (parse-error (car adtbody) "Cannot have assuming here")
	     (let ((imp (xt-importing-elt (car adtbody))))
	       (xt-adtbody (cdr adtbody) (append imp imps) assuming adtcases))))
	(ASSUMING
	 (if (or assuming adtcases)
	     (parse-error (car adtbody) "Cannot have assuming here")
	     (let ((ass (xt-assumings (car adtbody))))
	       (xt-adtbody (cdr adtbody) imps ass adtcases))))
	(ID-ADTCASE
	 (let ((adtcase (xt-id-adtcase (car adtbody))))
	   (xt-adtbody (cdr adtbody) imps assuming (cons adtcase adtcases))))
	(t (error "Huh?")))))

(defun xt-id-adtcase (id-adtcase)
  (let* ((constrid (term-arg0 id-adtcase))
	 (dformals (xt-theory-formals (term-arg1 id-adtcase)))
	 (adtcase (term-arg2 id-adtcase))
	 (constructor (term-arg0 adtcase))
	 (recognizer (term-arg1 adtcase))
	 (constr (xt-constructor constrid constructor))
	 (subtype (when (is-sop 'ADTCASE-SUBTYPE adtcase)
		    (xt-check-periods (term-arg2 adtcase))
		    (make-instance 'type-name
		      :id (xt-idop (xt-pidop (term-arg2 adtcase)))
		      :place (term-place (term-arg2 adtcase))))))
    (xt-check-periods recognizer)
    (when subtype
      (change-class constr 'constructor-with-subtype)
      (setf (subtype constr) subtype))
    (setf (decl-formals constr) dformals)
    (setf (recognizer constr) (xt-idop (xt-pidop recognizer)))
    (setf (place constr) (term-place constructor))
    constr))

(defun xt-constructor (id adtdecls)
  (let ((idop (xt-check-periods id)))
    (make-instance 'simple-constructor
      :id (xt-idop (xt-pidop idop))
      :arguments (unless (is-sop 'CONSTRUCTOR-NULL-1 adtdecls)
		   (xt-adtdecls adtdecls))
      :place (term-place-interval id adtdecls))))

(defun xt-adtdecls (decls)
  (mapcan #'xt-adtdecl (term-args decls)))

(defun xt-adtdecl (adtdecl)
  (let* ((idops (xt-check-periods (term-arg0 adtdecl)))
	 (type (term-arg1 adtdecl))
	 (dtype (xt-not-enum-type-expr type)))
    (xt-chained-decls (term-args idops)
		      nil
		      type
		      nil
		      (make-instance 'adtdecl
			:declared-type dtype
			:place (term-place type))
		      adtdecl)))

(defun xt-module (theory &optional id)
  (let* ((exp-t (term-arg0 theory))
	 (exp (if (is-sop 'NOEXP exp-t)
		  (make-instance 'exporting
		    :kind 'default)
		  (xt-exporting exp-t)))
	 (assum-t (term-arg1 theory))
	 (assum (unless (is-sop 'NOASS assum-t)
		  (xt-assumings assum-t)))
	 (tpart-t (term-arg2 theory))
	 (tpart (unless (is-sop 'NOTHEORY tpart-t)
		  (xt-theory-part tpart-t)))
	 (endid (ds-vid (term-arg3 theory))))
    (unless (or (null id) (eq id endid))
      (parse-error (term-arg3 theory)
	"End id ~a does not match theory id ~a" endid id))
    ;; (assert (every #'place (modules exp)))
    ;; (assert (every #'place assum))
    ;; (assert (every #'place tpart))
    (let ((th (make-instance 'module
		:exporting exp
		:assuming assum
		:theory tpart
		:place (term-place theory))))
      (dolist (decl assum)
	(when (declaration? decl)
	  (setf (module decl) th)))
      (dolist (decl tpart)
	(when (declaration? decl)
	  (setf (module decl) th)))
      th)))
      

(defun xt-exporting (exporting)
  (let ((names (term-arg0 exporting))
	(modnames (term-arg1 exporting)))
    (make-instance 'exporting
      :names (case (sim-term-op names)
	       (EXPORTING-ALL 'all)
	       (EXPNAMES (mapcar #'xt-expname (term-args names)))
	       (t (break "Something's wrong with the parser")))
      :but-names (when (and (is-sop 'EXPORTING-ALL names)
			    (not (is-sop 'NOEXPBUTS (term-arg0 names))))
		   (mapcar #'xt-expname
			   (term-args (term-arg0 names))))
      :kind (case (sim-term-op modnames)
	      (EXPMODALL 'all)
	      (EXPMODCLOSURE 'closure)
	      (t nil))
      :modules (case (sim-term-op modnames)
		 (NOEXPORTINGMODS nil)
		 (EXPMODALL nil)
		 (EXPMODCLOSURE nil)
		 (t (mapcar #'xt-modname (term-args modnames))))
      :place (term-place exporting))))

(defun xt-expname (expname)
  (let* ((pid (xt-pidop (term-arg0 expname)))
	 (id (xt-idop pid))
	 (kind (term-arg1 expname)))
    (make-instance 'expname
      :id id
      :kind (case (sim-term-op kind)
	      (NOEXPKIND nil)
	      (EXPTYPE 'type)
	      (EXPFORMULA 'formula)
	      (t (xt-not-enum-type-expr kind)))
      :place (term-place expname))))

(defun xt-assuming-part (assumings)
  (mapcan #'(lambda (ass)
	      (if (is-sop 'IMPORTING-ELT ass)
		  (xt-importing-elt ass)
		  (xt-assuming ass)))
	  (term-args assumings)))

(defun xt-assumings (assumings)
  (mapcan #'(lambda (ass)
	      (if (is-sop 'IMPORTING-ELT ass)
		  (xt-importing-elt ass)
		  (xt-assuming ass)))
	  (term-args assumings)))

(defun xt-assuming (assuming)
  (let* ((_decl (term-arg0 assuming))
	 (idops (xt-check-periods (term-arg1 assuming)))
	 (decl-params (unless (is-sop 'NO-THEORY-FORMALS (term-arg2 assuming))
			(xt-theory-formals (term-arg2 assuming) t)))
	 (formals (term-arg3 assuming))
	 (pdformals (unless (is-sop 'NOFORMALS formals)
		      (xt-pdf formals)))
	 (decl (term-arg4 assuming))
	 (semi (term-arg5 assuming)))
    (when (and (cdr (term-args idops)) pdformals)
      (parse-error formals ": expected here"))
    (case (sim-term-op decl)
      ((LIB-DECL THEORY-DECL TYPE-DECL DATATYPE)
       (let ((badid (find-if #'(lambda (tid)
				 (assq (ds-id (term-arg0 tid))
				       *pvs-operators*))
		      (term-args idops))))
	 (when badid
	   (parse-error badid "Id expected here")))))
    (multiple-value-bind (pdecl tdecl)
	(xt-declaration-body decl idops)
      (let ((decls (xt-chained-decls (term-args idops)
				     decl-params
				     tdecl
				     pdformals
				     pdecl
				     assuming)))
	(setf (semi pdecl)
	      (if (is-sop 'SEMIC semi)
		  (if (is-sop 'ATDECL _decl)
		      'both t)
		  (if (is-sop 'ATDECL _decl)
		      '_decl nil)))
	decls))))

(defun xt-theory-part (theory)
  (mapcan #'(lambda (decl)
	      (case (sim-term-op decl)
		(IMPORTING-ELT (xt-importing-elt decl))
		(JUDGEMENT-ELT (xt-judgement-elt decl))
		(CONVERSION-ELT (xt-conversion-elt decl))
		(AUTO-REWRITE-ELT (xt-auto-rewrite-elt decl))
		(t (xt-theory decl))))
	  (term-args theory)))

(defun xt-theory (tdecl)
  (let* ((_decl (term-arg0 tdecl))
	 (idops (xt-check-periods (term-arg1 tdecl)))
	 (decl-params (unless (is-sop 'NO-THEORY-FORMALS (term-arg2 tdecl))
			  (xt-theory-formals (term-arg2 tdecl) t)))
	 (formals (term-arg3 tdecl))
	 (pformals (unless (is-sop 'NOFORMALS formals)
		     (xt-pdf formals)))
	 (decl (term-arg4 tdecl))
	 (semi (term-arg5 tdecl)))
    (when (and (cdr (term-args idops)) pformals)
      (parse-error formals ": expected here"))
    (when (and decl-params
	       (memq (sim-term-op decl) '(LIB-DECL VAR-DECL)))
      (parse-error decl-params "Declaration formals not allowed here"))
    (case (sim-term-op decl)
      ((LIB-DECL THEORY-DECL TYPE-DECL DATATYPE CODATATYPE)
       (let ((badid (find-if #'(lambda (tid)
				 (assq (ds-id (term-arg0 tid))
				       *pvs-operators*))
		      (term-args idops))))
	 (when badid
	   (parse-error badid "Id expected here")))))
    (case (sim-term-op decl)
      ((DATATYPE CODATATYPE)
       (xt-theory-datatype idops decl-params formals decl semi))
      ((DATATYPES CODATATYPES) ;; with subtypes
       (xt-theory-datatypes idops decl-params formals decl semi))
      (t (multiple-value-bind (pdecl type-decl)
	     (xt-declaration-body decl idops)
	   (let ((decls (xt-chained-decls (term-args idops)
					  decl-params
					  type-decl
					  pformals
					  pdecl
					  tdecl)))
	     #+pvsdebug			; Careful! nil is a valid PVS id
	     (assert (every #'(lambda (d)
				(or (not (const-decl? d))
				    (id d)))
			    decls))
	     (assert (every #'place decls))
	     (setf (semi pdecl)
		   (if (is-sop 'SEMIC semi)
		       (if (is-sop 'ATDECL _decl)
			   'both t)
		       (if (is-sop 'ATDECL _decl)
			   '_decl nil)))
	     decls))))))

(defun xt-pidop (pidop)
  (if (cdr (term-args pidop))
      (let ((idop (mk-ergo-term 'IDOP
		    (list (mk-id (makesym "~{~a~^.~}"
					  (mapcar #'xt-idop (term-args pidop))))))))
	(setf (term-place idop) (term-place pidop))
	idop)
      (term-arg0 pidop)))

(defun xt-check-periods (pidops)
  (let ((nterm (mk-ergo-term (sim-term-op pidops)
		 (mapcar
		     #'(lambda (pidop)
			 (if (is-sop 'IDOPAPPL pidop)
			     (parse-error pidops "parens not allowed here")
			     (if (cdr (term-args pidop)) ;; have periods
				 (if *xt-periods-allowed*
				     (xt-pidop pidop)
				     (parse-error pidops
				       "periods not allowed here"))
				 (if (is-sop 'PIDOP pidop)
				     (term-arg0 pidop)
				     pidop))))
		   (term-args pidops)))))
    (setf (term-place nterm) (term-place pidops))
    nterm))

(defun xt-theory-datatypes (idops decl-params formals decl semi)
  (cond ((is-sop 'PDF formals)
	 (parse-error (term-arg0 formals)
	   "Inline datatypes may not have parameters"))
	((cdr (term-args idops))
	 (parse-error (term-arg0 (term-arg1 idops))
	   "Only one id allowed for datatypes"))
	((not (is-id (term-arg0 (term-arg0 idops))))
	 (parse-error (term-arg0 idops)
	   "Datatype identifier must be an id"))
	(t (let* ((id (ds-vid (term-arg0 (term-arg0 idops))))
		  (adt (funcall #'xt-datatypes (sim-term-op decl)
				decl id t)))
	     (setf (id adt) id
		   (formals adt) (xt-theory-formals formals t)
		   (decl-formals adt) decl-params
		   (semi adt) (when (is-sop 'SEMIC semi) t))
	     (list adt)))))

(defun xt-theory-datatype (idops decl-params formals decl semi)
  (cond ((is-sop 'PDF formals)
	 (parse-error (term-arg0 formals)
	   "Inline (co)datatypes may not have theory parameters"))
	((cdr (term-args idops))
	 (parse-error (term-arg0 (term-arg1 idops))
	   "Only one id allowed for datatypes"))
	((not (is-id (term-arg0 (term-arg0 idops))))
	 (parse-error (term-arg0 idops)
	   "Datatype identifier must be an id"))
	(t (let* ((id (ds-vid (term-arg0 (term-arg0 idops))))
		  (adt (funcall #'xt-datatype (sim-term-op decl)
				decl id t)))
	     (setf (id adt) id
		   (formals adt) (xt-theory-formals formals t)
		   (decl-formals adt) decl-params
		   (semi adt) (when (is-sop 'SEMIC semi) t))
	     (list adt)))))

(defun xt-importing-elt (importing-elt)
  (let* ((importing (term-arg0 importing-elt))
	 (imp-place (term-place importing))
	 (importings
	  (mapcar #'(lambda (item)
		      (let* ((it-place (term-place item))
			     (place (vector (starting-row imp-place)
					    (starting-col imp-place)
					    (ending-row it-place)
					    (ending-col it-place))))
			(if (is-sop 'THEORY-ABBREVIATION-DECL item)
			    (make-instance 'theory-abbreviation-decl
			      :id (xt-idop (xt-pidop (term-arg1 item)))
			      :theory-name (xt-modname (term-arg0 item))
			      :place place
			      :semi (when (is-sop 'SEMIC
						  (term-arg1 importing-elt))
				      t)
			      :chain? t)
			    (make-instance 'importing
			      :theory-name (xt-modname item)
			      :place place
			      :semi (when (is-sop 'SEMIC
						  (term-arg1 importing-elt))
				      t)
			      :chain? t))))
	    (term-args importing))))
    (setf (chain? (car (last importings))) nil)
    importings))

(defun xt-formal-importing-elt (importing)
  (let ((lib-decl
	 (unless (is-sop 'NOLIB (term-arg0 importing))
	   (break "formal importing with lib: needs fixing")))
	(importings
	 (mapcar #'(lambda (item)
		     (let* ((imp-place (term-place importing))
			    (it-place (term-place item))
			    (place (vector (starting-row imp-place)
					   (starting-col imp-place)
					   (ending-row it-place)
					   (ending-col it-place))))
		       (if (is-sop 'THEORY-ABBREVIATION-DECL item)
			   (make-instance 'theory-abbreviation-decl
			     :id (ds-id (term-arg1 item))
			     :theory-name (xt-modname (term-arg0 item))
			     :place place
			     :chain? t)
			   (make-instance 'importing
			     :theory-name (xt-modname item)
			     :place place
			     :chain? t))))
	   (term-args (term-arg1 importing)))))
    (setf (chain? (car (last importings))) nil)
    (if lib-decl
	(cons lib-decl importings)
	importings)))

(defun xt-judgement-elt (decl)
  (let ((jdecls (xt-judgement (term-arg0 decl))))
    (dolist (jdecl jdecls)
      (setf (semi jdecl) (when (is-sop 'SEMIC (term-arg1 decl)) t)))
    jdecls))

(defun xt-judgement-decl (decl idops)
  (multiple-value-bind (jdecls dtype)
      (xt-judgement decl)
    (cond ((cdr (term-args idops))
	   (parse-error idops
	     "May not declare multiple ids for a judgement"))
	  ((cdr jdecls)
	   (parse-error decl
	     "May not declare multiple judgements when an id is given"))
	  (t (values (car jdecls) dtype)))))

(defun xt-judgement (decl)
  (let* ((jdecls (term-args (term-arg0 decl)))
	 (class (ds-id (term-arg1 decl)))
	 (type (term-arg2 decl))
	 (dtype (xt-not-enum-type-expr type))
	 (place (term-place decl)))
    (values (case class
	      (HAS-TYPE
	       (xt-const-judgement jdecls dtype place))
	      (SUBTYPE-OF
	       (xt-subtype-judgement jdecls dtype place))
	      (REC-HAS-TYPE
	       (xt-rec-judgement jdecls dtype place))
	      (REC-SUBTYPE-OF
	       (parse-error decl
		 "RECURSIVE not allowed for subtype judgements")))
	    type)))

(defun xt-subtype-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-subtype-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))

(defun appl-judgement-form? (expr-term)
  (cond ((is-sop 'APPLICATION expr-term)
	 (and (cond ((is-sop 'NAME-EXPR (term-arg0 expr-term))
		     (and (is-sop 'NOTYPE (term-arg1 (term-arg0 expr-term)))
			  (is-sop 'NOPRED (term-arg2 (term-arg0 expr-term)))))
		    ((is-sop 'APPLICATION (term-arg0 expr-term))
		     (appl-judgement-form? (term-arg0 expr-term)))
		    (t nil))
	      (is-sop 'FUN-ARGUMENTS (term-arg1 expr-term))
	      (term-args (term-arg1 expr-term))
	      (every #'(lambda (fa)
			 (or (is-sop 'NAME-EXPR fa)
			     (and (is-sop 'TUPLE-EXPR fa)
				  (and (term-args fa)
				       (every #'(lambda (tu) (is-sop 'NAME-EXPR tu))
					      (term-args fa))))))
		     (term-args (term-arg1 expr-term)))))
	((is-sop 'UNARY-TERM-EXPR expr-term)
	 (and (cond ((is-sop 'TUPLE-EXPR (term-arg1 expr-term))
		     (every #'(lambda (fa)
				(or (is-sop 'NAME-EXPR fa)
				    (and (is-sop 'TUPLE-EXPR fa)
					 (and (term-args fa)
					      (every #'(lambda (tu) (is-sop 'NAME-EXPR tu))
						     (term-args fa))))))
			    (term-args (term-arg1 expr-term))))
		    (t nil))))
	((is-sop 'TERM-EXPR expr-term)
	 )))
	 

(defun xt-subtype-judgement* (jdecl dtype place)
  (cond ((is-sop 'JDECL-EXPR jdecl)
	 (let* ((*allowed-typed-names*
		 (appl-judgement-form? (term-arg0 jdecl)))
		(ex (xt-expr (term-arg0 jdecl)))
		(type (xt-convert-expr-to-type-expr ex)))
	   (assert (place type))
	   (make-instance 'subtype-judgement
	     :declared-subtype type
	     :declared-type dtype
	     :chain? t
	     :place place)))
	(t
	 (assert (is-sop 'JDECL-TYPE jdecl))
	 (let ((type (xt-type-expr (term-arg0 jdecl))))
	   (make-instance 'subtype-judgement
	     :declared-subtype type
	     :declared-type dtype
	     :chain? t
	     :place place)))))

(defun xt-convert-expr-to-type-expr (ex)
  (let ((etype (xt-convert-expr-to-type-expr* ex)))
    (unless (place etype)
      (setf (place etype) (place ex)))
    etype))


(defmethod xt-convert-expr-to-type-expr* :around ((ex expr))
  (if (plusp (parens ex))
      (make-instance 'expr-as-type :expr ex)
      (call-next-method)))

(defmethod xt-convert-expr-to-type-expr* ((ex name-expr))
  (change-class ex 'type-name))

(defmethod xt-convert-expr-to-type-expr* ((ex set-expr))
  (change-class ex 'subtype
    :predicate (copy ex)))

(defmethod xt-convert-expr-to-type-expr* ((ex application))
  (if (name-expr? (operator ex))
      (let* ((tn (change-class (operator ex) 'type-name))
	     (args (arguments ex)))
	(make-instance 'type-application
	  :type tn
	  :parameters args
	  :place (place ex)))
      (parse-error (place ex) "Type application must be of the form T(a, b, ...)")))

(defmethod xt-convert-expr-to-type-expr* ((ex expr))
  (parse-error ex "Type expected here"))
      

(defun xt-const-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-const-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))

(defun xt-const-judgement* (jdecl dtype place)
  (case (sim-term-op jdecl)
    (JDECL-EXPR
     (let* ((*allowed-typed-names*
	     (appl-judgement-form? (term-arg0 jdecl)))
	    (ex (xt-expr (term-arg0 jdecl))))
       (typecase ex
	 (number-expr
	  (make-instance 'number-judgement
	    :number-expr ex
	    :declared-type dtype
	    :chain? t
	    :place place))
	 (bind-decl
	  (make-instance 'expr-judgement
	    :declared-type dtype
	    :chain? t
	    :expr ex
	    :place place))
	 (name-expr
	  (make-instance 'name-judgement
	    :name ex
	    :declared-type dtype
	    :chain? t
	    :place place))
	 (application
	  (if (and *allowed-typed-names*
		   ;; Have an application, possibly curried, check if every
		   ;; argument is a name-expr that is not a binding
		   (some #'(lambda (args)
			     (some #'bind-decl? args))
			 (arguments* ex)))
	      ;; Found a typed-name need to convert other name-exprs to
	      ;; untyped-bind-decls
	      (let* ((invalid-arg (find-if #'(lambda (args) (find-if-not #'name? args))
				    (arguments* ex)))
		     (formals (unless invalid-arg
				(create-formals-from-arguments (arguments* ex)))))
		(if invalid-arg
		    (parse-error invalid-arg "Must use bindings for application judgements")
		    (make-instance 'application-judgement
		      :name (operator* ex)
		      :formals formals
		      :declared-type dtype
		      :chain? t
		      :place place)))
	      (make-instance 'expr-judgement
		:declared-type dtype
		:chain? t
		:expr ex
		:place place)))
	 (t (make-instance 'expr-judgement
	      :declared-type dtype
	      :chain? t
	      :expr ex
	      :place place)))))))

;; Arguments come from parsing decl as an expr.  Now want to convert to
;; pdformals+

;; pdformals ::= '(' adformals ')'
;; adformals ::= adformal++','
;; adformal ::= typed-id | '(' typed-ids ')'
;; typed-id ::= idop [':' type-expr] ['|' expr]
;; typed-ids ::= idops [':' type-expr] ['|' expr]

;; Problems:
;;  - allowing "idop | expr" causes problems, so must include the type
;;  - 


(defun create-formals-from-arguments (args-list &optional formals)
  ;; args-list should be a list of lists
  (if (null args-list)
      (nreverse formals)
      (let ((args (create-formals-from-arguments* (car args-list))))
	(assert (consp args))
	(create-formals-from-arguments (cdr args-list) (cons args formals)))))

(defmethod create-formals-from-arguments* ((args cons) &optional formals)
  (let ((fml (create-formal-from-argument (car args))))
    (assert (bind-decl? fml))
    (create-formals-from-arguments* (cdr args) (cons fml formals))))

(defmethod create-formals-from-arguments* ((args null) &optional formals)
  (let ((fmls (nreverse formals)))
    ;; chain? is t by default - reset to nil if there is no following arg
    ;; or it is in parens
    (xt-set-declared-types fmls)
    fmls))

(defmethod create-formals-from-arguments* ((args tuple-expr) &optional formals)
  ;; Not sure this is possible - but break just in case
  (declare (ignore formals))
  (break "create-formals-from-arguments* (tuple-expr)"))

(defmethod create-formals-from-arguments* ((args t) &optional formals)
  (declare (ignore formals))
  (parse-error args "Illegal formal parameter"))
  

(defmethod create-formal-from-argument ((arg name-expr))
  (change-class arg 'untyped-bind-decl :chain? t))

(defmethod create-formal-from-argument ((arg bind-decl))
  arg)

(defmethod create-formal-from-argument ((arg t))
  (break "Bad arg in create-formal-from-argument"))
  

(defun xt-jexprdecl (jdecl)
  (when (and (> (length (term-args (term-arg0 jdecl))) 1)
	     (xt-lambda-formals-check (term-arg0 jdecl)))
    (parse-error (term-arg0 jdecl) "commas not allowed"))
  (let ((bindings (xt-lambda-formals (term-arg0 jdecl) nil)))
    (make-instance 'expr-judgement
      :formals (xt-flatten-bindings (car bindings) 0)
      :expr (xt-expr (term-arg1 jdecl)))))

(defun xt-rec-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-rec-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))


(defun xt-rec-judgement* (jdecl dtype place)
  (if (eq (sim-term-op jdecl) 'JDECL-EXPR)
      (let ((*allowed-typed-names*
	     (appl-judgement-form? (term-arg0 jdecl))))
	(unless *allowed-typed-names*
	  (parse-error jdecl "Recursive judgements are only for applications"))
	(let ((ex (xt-expr (term-arg0 jdecl))))
	  (cond ((not (application? ex))
		 (parse-error jdecl "Recursive judgements are only for applications"))
		((not (name-expr? (operator* ex)))
		 (parse-error jdecl "Recursive judgements are only for named applications"))
		(t
		 (let* ((invalid-arg (find-if #'(lambda (args) (find-if-not #'name? args))
				       (arguments* ex)))
			(formals (unless invalid-arg
				   (create-formals-from-arguments (arguments* ex)))))
		   (if invalid-arg
		       (parse-error invalid-arg
			 "Must use variables or bindings for recursive judgements")
		       (make-instance 'rec-application-judgement
			 :name (operator* ex)
			 :formals formals
			 :declared-type dtype
			 :chain? t
			 :place place)))))))
      (parse-error jdecl "Recursive judgements are not for types")))

;;; Conversions

(defun xt-conversion-elt (decl)
  (let ((cdecls (xt-conversion (term-arg0 decl))))
    (dolist (cdecl cdecls)
      (setf (semi cdecl) (when (is-sop 'SEMIC (term-arg1 decl)) t)))
    cdecls))

(defun xt-conversion (decl)
  (xt-conversion* (term-args decl) (sim-term-op decl)))

(defun xt-conversion* (exprs convkey &optional result)
  (cond ((null exprs)
	 (setf (chain? (car result)) nil)
	 (nreverse result))
	(t (let ((cdecl (xt-conversion** (car exprs)
					 (null (cdr exprs))
					 convkey)))
	     (xt-conversion* (cdr exprs) convkey
			     (cons cdecl result))))))

(defun xt-conversion** (texpr last? convkey)
  (let ((expr (xt-expr texpr)))
    (case convkey
      (CONVERSIONPLUS
       (make-instance 'conversionplus-decl
	 :id (when (name-expr? expr) (id expr))
	 :expr expr
	 :chain? (not last?)
	 :place (term-place texpr)))
      (CONVERSIONMINUS
       (make-instance 'conversionminus-decl
	 :id (when (name-expr? expr) (id expr))
	 :expr expr
	 :chain? (not last?)
	 :place (term-place texpr)))
      (t
       (make-instance 'conversion-decl
	 :id (when (name-expr? expr) (id expr))
	 :expr expr
	 :chain? (not last?)
	 :place (term-place texpr))))))


;;; auto-rewrites

(defun xt-auto-rewrite-elt (decl)
  (let ((adecl (xt-auto-rewrite (term-arg0 decl))))
    (setf (semi adecl) (when (is-sop 'SEMIC (term-arg1 decl)) t)
	  (place adecl) (term-place decl))
    (list adecl)))

(defun xt-auto-rewrite (decl)
  (let ((rnames (xt-rewrite-names (term-args decl))))
    (case (sim-term-op decl)
      (AUTO-REWRITE (make-instance 'auto-rewrite-decl
		      :rewrite-names rnames))
      (AUTO-REWRITEPLUS (make-instance 'auto-rewrite-plus-decl
			  :rewrite-names rnames))
      (AUTO-REWRITEMINUS (make-instance 'auto-rewrite-minus-decl
			   :rewrite-names rnames)))))

(defun xt-rewrite-names (rnames)
  (mapcar #'xt-rewrite-name rnames))

(defun xt-rewrite-name (rname)
  (let ((name (xt-name (term-arg0 rname) nil))
	(kind (term-arg1 rname))
	(qual (term-arg2 rname)))
    (case (sim-term-op kind)
      (LAZY (case (sim-term-op qual)
	      (TYPE (change-class name 'lazy-constant-rewrite-name
				  :declared-type (xt-not-enum-type-expr
						  (term-arg0 qual))))
	      (FORMULA (change-class name 'lazy-formula-rewrite-name
				     :spelling (sim-term-op (term-arg0 qual))))
	      (t (change-class name 'lazy-rewrite-name))))
      (EAGER (case (sim-term-op qual)
	       (TYPE (change-class name 'eager-constant-rewrite-name
		       :declared-type (xt-not-enum-type-expr
				       (term-arg0 qual))))
	       (FORMULA (change-class name 'eager-formula-rewrite-name
			  :spelling (sim-term-op (term-arg0 qual))))
	       (t (change-class name 'eager-rewrite-name))))
      (MACRO (case (sim-term-op qual)
	       (TYPE (change-class name 'macro-constant-rewrite-name
		       :declared-type (xt-not-enum-type-expr
				       (term-arg0 qual))))
	       (FORMULA (change-class name 'macro-formula-rewrite-name
			  :spelling (sim-term-op (term-arg0 qual))))
	       (t (change-class name 'macro-rewrite-name)))))))

(defun xt-rewrite-fnum (fnum)
  (let* ((num (ds-number (term-arg0 (term-arg0 fnum))))
	 (number (case (sim-term-op (term-arg1 (term-arg0 fnum)))
		   (- (- num))
		   (+ num))))
    (case (sim-term-op (term-arg1 fnum))
      (LAZY (make-instance 'lazy-fnum-rewrite
	      :fnum number))
      (EAGER (make-instance 'eager-fnum-rewrite
	       :fnum number))
      (MACRO (make-instance 'macro-fnum-rewrite
	       :fnum number)))))

(defun term-place-interval (strm etrm)
  (let ((splace (term-place strm))
	(eplace (term-place etrm)))
    (if (and splace eplace)
	(vector (starting-row splace) (starting-col splace)
		(ending-row eplace) (ending-col eplace))
	(or splace eplace))))

(defun xt-chained-decls (idops decl-params dtype formals decl absyn &optional result)
  (assert (listp formals))
  (if (null idops)
      (nreverse result)
      (let ((ndecl (if (cdr idops)
		       (case (sim-term-op (car idops))
			 (IDOPAPPL
			  (let ((tid (xt-idop (term-arg0 (car idops))))
				(types (term-args (term-arg1 (car idops)))))
			    (change-class (copy decl) 'formal-type-appl-decl
			      :id tid
			      :parameters (mapcar #'xt-type-expr types))))
			 (t
			  (copy decl
			    :id (if (is-id (car idops))
				    (ds-vid (car idops))
				    (xt-idop (car idops)))
			    :chain? t)))
		       (progn (case (sim-term-op (car idops))
				(IDOPAPPL
				 (let ((tid (xt-idop (term-arg0 (car idops))))
				       (types (term-args (term-arg1 (car idops)))))
				   (change-class decl 'formal-type-appl-decl
				     :id tid
				     :parameters (mapcar #'xt-type-expr types))))
				(t
				 (setf (id decl)
				       (if (is-id (car idops))
					   (ds-vid (car idops))
					   (xt-idop (car idops))))))
			      decl))))
	;;(assert (id ndecl))
	(when (and (slot-exists-p ndecl :declared-type)
		   (declared-type ndecl)
		   (not (eq (sim-term-op dtype) 'NO-TYPE-EXPR))
		   (not (typep decl 'pred-bind-decl)))
	  (assert dtype)
	  (assert (declared-type ndecl))
	  ;; Make sure a copy is created unless it's the last (or only) one
	  (setf (declared-type ndecl)
		(xt-not-enum-type-expr dtype)))
	(let* ((idpos (position-if #'(lambda (tm)
				       (memq (ds-sim-op (term-op tm))
					     '(IDOPS PIDOPS)))
				   (term-args absyn))))
	  (when idpos
	    (setf (place ndecl)
		  (term-place-interval (car idops) absyn))))
	(when decl-params
	  (when (typep ndecl '(or lib-decl))
	    (parse-error (place decl-params)
	      "Library declarations may not have declaration parameters"))
	  (setf (decl-formals ndecl) decl-params))
	(when formals (setf (formals ndecl) formals))
	(xt-chained-decls (cdr idops) decl-params dtype formals decl absyn
			  (cons ndecl result)))))

;;; pdformals ::= {'(' adformals ')'} <adformals>;

(defun xt-pdf (pdf)
  (mapcar #'xt-adformals (term-args pdf)))

;;; adformals ::= {adformal++','} <adformals(adformal+)>;

(defun xt-adformals (adformals)
  (xt-adformals* (term-args adformals)))

;;; adformal ::= typed-id | {'(' typed-ids ')'} <typed-ids>;

(defun xt-adformals* (adformals &optional untyped formals)
  (if (null adformals)
      (nconc formals untyped)
      (let ((df (car adformals)))
	(if (is-sop 'TYPED-IDS df)
	    (let ((tids (xt-typed-ids df)))
	      (dolist (x tids)
		(setf (parens x) 1))
	      (xt-adformals* (cdr adformals) nil (nconc formals untyped tids)))
	    (let ((formal (xt-typed-id df)))
	      (cond ((declared-type formal)
		     (mapc #'(lambda (un)
			       (setf (declared-type un)
				     (declared-type formal))
			       (setf (chain? un) t))
			   untyped)
		     (xt-adformals* (cdr adformals)
				    nil
				    (nconc formals untyped (list formal))))
		    (t (xt-adformals* (cdr adformals)
				      (nconc untyped (list formal))
				      formals))))))))

;;; typed-ids ::= {idops [':' type-expr]^t ['|' expr]^e}
;;; 	      <{typed-ids(idops,[no-type-expr()|type-expr]^t,
;;; 				[no-pred()|expr]^e)>;

(defun xt-typed-ids (typed-ids)
  (let* ((idops (term-arg0 typed-ids))
	 (type-expr (term-arg1 typed-ids))
	 (expr (term-arg2 typed-ids))
	 (no-pred (is-sop 'NO-PRED expr))
	 (type (unless (is-sop 'NO-TYPE-EXPR type-expr)
		 (xt-not-enum-type-expr type-expr))))
    (unless (or no-pred (not (cdr (term-args idops))))
      (parse-error (term-arg0 (term-arg1 (term-arg0 typed-ids)))
	"May not have multiple ids with '|'"))
    (xt-chained-decls
     (term-args idops)
     nil
     type-expr
     nil
     (if no-pred
	 (xt-typed-id-bind-decl nil type (term-place typed-ids))
	 (let* ((id (ds-vid (term-arg0 (car (term-args idops)))))
		(place (term-place (term-arg0 (car (term-args idops)))))
		(formals (xt-typed-id-bind-decl id type place))
		(dtype (make-instance (if type 'setsubtype 'nsetsubtype)
			 :supertype type
			 :formals formals
			 :formula (xt-expr expr)
			 :place (term-place type-expr))))
	   (make-instance 'pred-bind-decl
	     :declared-type dtype
	     :place (term-place typed-ids))))
     typed-ids)))

(defun xt-typed-id (typed-id)
  (let* ((idop (term-arg0 typed-id))
	 (type-expr (term-arg1 typed-id))
	 (expr (term-arg2 typed-id))
	 (no-pred (is-sop 'NO-PRED expr))
	 (type (unless (is-sop 'NO-TYPE-EXPR type-expr)
		 (xt-not-enum-type-expr type-expr))))
    (if no-pred
	(xt-typed-id-bind-decl (xt-idop idop) type (term-place typed-id))
	(let* ((id (xt-idop idop))
	       (place (term-place idop))
	       (formals (xt-typed-id-bind-decl id type place))
	       (dtype (make-instance (if type 'setsubtype 'nsetsubtype)
			:supertype type
			:formals formals
			:formula (xt-expr expr)
			:place (term-place type-expr))))
	  (make-instance 'pred-bind-decl
	    :id id
	    :declared-type dtype
	    :place (term-place typed-id))))))

(defun xt-typed-id-bind-decl (id type place)
  (if type
      (make-instance 'bind-decl
	:id id
	:declared-type type
	:place place)
      (make-instance 'untyped-bind-decl
	:id id
	:place place)))

;;; Declarations

(defun xt-declaration-body (body &optional idops)
  (case (sim-term-op body)
    (FTYPE-DECL (xt-ftype-decl body))
    (FCONST-DECL (xt-fconst-decl body))
    (FTHEORY-DECL (xt-ftheory-decl body))
    (LIB-DECL (xt-lib-decl body))
    (THEORY-DECL (xt-theory-decl body))
    (UNINTERP-TYPE-DECL (xt-uninterp-type-decl body))
    (TYPE-DECL (xt-type-decl body))
    (UNITS-DECL (xt-units-decl body))
    (JUDGEMENT (xt-judgement-decl body idops))
    (VAR-DECL (xt-var-decl body))
    (UNINTERP-CONST-DECL (xt-const-decl body))
    (CONST-DECL (xt-const-decl body))
    (MACRO-DECL (xt-macro-decl body))
    (DEF-DECL (xt-def-decl body))
    (IND-DECL (xt-ind-decl body))
    (COREC-DECL (xt-corec-decl body))
    (COIND-DECL (xt-coind-decl body))
    (ASSUMPTION (xt-assumption body))
    (FORMULA-DECL (xt-formula-decl body))
    (t (error "Decl not recognized - ~a" body))))

(defun xt-ftype-decl (ftype-decl)
  (let ((tkey (sim-term-op (term-arg0 ftype-decl)))
	(type-expr (term-arg1 ftype-decl)))
    (if (eq tkey 'TYPE)
	(case (sim-term-op type-expr)
	  (NOTYPE (make-instance 'formal-type-decl
		    :place (term-place ftype-decl)))
	  (STRUCT-SUBTYPE (make-instance 'formal-struct-subtype-decl
			    :type-expr (xt-not-enum-type-expr type-expr)
			    :place (term-place ftype-decl)))
	  (t (make-instance 'formal-subtype-decl
	       :type-expr (xt-not-enum-type-expr type-expr)
	       :place (term-place ftype-decl))))
	(case (sim-term-op type-expr)
	  (NOTYPE (make-instance 'formal-nonempty-type-decl
		    :keyword tkey
		    :place (term-place ftype-decl)))
	  (STRUCT-SUBTYPE (make-instance 'formal-nonempty-struct-subtype-decl
			    :type-expr (xt-not-enum-type-expr type-expr)
			    :keyword tkey
			    :place (term-place ftype-decl)))
	  (t (make-instance 'formal-nonempty-subtype-decl
	       :type-expr (xt-not-enum-type-expr type-expr)
	       :keyword tkey
	       :place (term-place ftype-decl)))))))

(defun xt-fconst-decl (fconst-decl)
  (let ((dtype (term-arg0 fconst-decl)))
    (values (make-instance 'formal-const-decl
	      :declared-type (xt-not-enum-type-expr dtype)
	      :place (term-place fconst-decl))
	    dtype)))

(defun xt-ftheory-decl (ftheory-decl)
  (let ((dmodname (term-arg0 ftheory-decl)))
    (values (make-instance 'formal-theory-decl
	      :theory-name (xt-modname dmodname)
	      :place (term-place ftheory-decl))
	    dmodname)))

(defun xt-lib-decl (lib-decl)
  (let ((libstr (coerce (ds-string (term-arg1 lib-decl)) 'string)))
    (make-instance (if (is-sop 'NOEQ (term-arg0 lib-decl))
		       'lib-decl
		       'lib-eq-decl)
      :lib-string libstr
      :place (term-place lib-decl))))

(defun xt-theory-decl (theory-decl)
  (make-instance 'mod-decl
    :modname (xt-modname (term-arg0 theory-decl))
    :place (term-place theory-decl)))

(defun xt-uninterp-type-decl (utd)
  (let ((tkey (sim-term-op (term-arg0 utd)))
	(place (term-place utd)))
    (if (eq tkey 'TYPE)
	(make-instance 'type-decl
	  :place place)
	(make-instance 'nonempty-type-decl
	  :keyword tkey
	  :place place))))

(defun xt-type-decl (type-decl)
  (let* ((tkey (sim-term-op (term-arg0 type-decl)))
	 (tdef (term-arg1 type-decl))
	 (eq? (is-sop 'EQUAL (term-arg0 tdef)))
	 (texpr (xt-type-expr (term-arg1 tdef)))
	 (tcont (term-arg2 type-decl))
	 (contains (unless (is-sop 'NOCONTAINS tcont)
		     (xt-expr tcont)))
	 (place (term-place type-decl)))
    (when (and contains (enumtype? texpr))
      (parse-error tcont "CONTAINING not expected here"))
    (cond ((enumtype? texpr)
	   (unless eq?
	     (parse-error (term-arg0 tdef)
	       "Cannot use ~a on an enumeration type declaration"
	       (sim-term-op (term-arg0 tdef))))
	   texpr)
	  ((eq tkey 'TYPE)
	   (case (sim-term-op (term-arg0 tdef))
	     (EQUAL (make-instance 'type-eq-decl
		      :type-expr texpr
		      :contains contains
		      :place place))
	     (FROM (make-instance 'type-from-decl
		     :type-expr texpr
		     :contains contains
		     :place place))
	     (t (make-instance 'struct-subtype-decl
		  :type-expr texpr
		  :contains contains
		  :place place))))
	  (t
	   (case (sim-term-op (term-arg0 tdef))
	     (EQUAL (make-instance 'nonempty-type-eq-decl
		      :keyword tkey
		      :type-expr texpr
		      :contains contains
		      :place place))
	     (FROM (make-instance 'nonempty-type-from-decl
		     :keyword tkey
		     :type-expr texpr
		     :contains contains
		     :place place))
	     (t (make-instance 'nonempty-struct-subtype-decl
		  :keyword tkey
		  :type-expr texpr
		  :contains contains
		  :place place)))))))

(defun xt-units-decl (units-decl)
  (let ((units-expr (term-arg0 units-decl))
	(offset (term-arg1 units-decl)))
    (values (make-instance 'units-decl
	      :declared-units (xt-units-term units-expr)
	      :offset (xt-rational offset))
	    units-expr)))

(defun xt-units-term (units-expr)
  (case (sim-term-op units-expr)
    (NAME (let ((nm (xt-name units-expr nil)))
	    (typecase nm
	      (decimal (/ (number (args1 nm)) (number (args2 nm))))
	      (number-expr (number nm))
	      (t (change-class nm 'units-name)
		 (setf (place nm) (term-place units-expr))
		 nm))))
    (UNITS-TERM (let ((op (sim-term-op (term-arg0 units-expr)))
		      (lhs (xt-units-term (term-arg1 units-expr)))
		      (rhs (xt-units-term (term-arg2 units-expr))))
		  (values (make-instance 'units-appl
			    :operator op
			    :arguments (list lhs rhs)
			    :place (term-place units-expr))
			  units-expr)))
    (RATIONAL (xt-rational units-expr))
    (t (break))))

(defun xt-rational (rat-term)
  (let ((pos? (is-sop 'POS (term-arg0 rat-term)))
	(num (ds-number (term-arg1 rat-term))))
    (when (zerop num)
      (parse-error 0 "Ratio with 0 numerator is just 0"))
    (if (is-sop 'NODEN (term-arg2 rat-term))
	(if pos? num (- num))
	(let ((den (ds-number (term-arg2 rat-term))))
	  (when (zerop den)
	    (parse-error 0 "Cannot have a ratio with 0 denominator"))
	  (if pos? (/ num den) (- (/ num den)))))))

(defun xt-var-decl (var-decl)
  (let ((dtype (term-arg0 var-decl)))
    (values (make-instance 'var-decl
	      :declared-type (xt-not-enum-type-expr dtype)
	      :place (term-place var-decl))
	    dtype)))

(defun xt-const-decl (const-decl)
  (let* ((dtype (term-arg0 const-decl))
	 (type-expr (xt-not-enum-type-expr dtype))
	 (value (cadr (term-args const-decl))))
    (values (make-instance 'const-decl
	      :declared-type type-expr
	      :definition (when value (xt-expr value))
	      :place (term-place const-decl))
	    dtype)))

(defun xt-macro-decl (macro-decl)
  (let* ((dtype (term-arg0 macro-decl))
	 (type-expr (xt-not-enum-type-expr dtype))
	 (value (term-arg1 macro-decl)))
    (values (make-instance 'macro-decl
	      :declared-type type-expr
	      :definition (xt-expr value)
	      :place (term-place macro-decl))
	    dtype)))

(defun xt-def-decl (def-decl)
  (let ((type-expr (term-arg0 def-decl))
	(def (term-arg1 def-decl))
	(meas (term-arg2 def-decl))
	(ordering (term-arg3 def-decl)))
    (values (make-instance 'def-decl
	      :declared-type (xt-not-enum-type-expr type-expr)
	      :definition (xt-expr def)
	      :declared-measure (xt-expr meas)
	      :ordering (unless (is-sop 'NOEXPR ordering)
			  (xt-expr ordering))
	      :place (term-place def-decl))
	    type-expr)))

(defun xt-ind-decl (ind-decl)
  (let ((type-expr (term-arg0 ind-decl))
	(def (term-arg1 ind-decl)))
    (values (make-instance 'inductive-decl
	      :declared-type (xt-not-enum-type-expr type-expr)
	      :definition (xt-expr def)
	      :place (term-place ind-decl))
	    type-expr)))

(defun xt-corec-decl (corec-decl)
  (let ((type-expr (term-arg0 corec-decl))
	(def (term-arg1 corec-decl)))
    (values (make-instance 'corecursive-decl
	      :declared-type (xt-not-enum-type-expr type-expr)
	      :definition (xt-expr def)
	      :place (term-place corec-decl))
	    type-expr)))

(defun xt-coind-decl (ind-decl)
  (let ((type-expr (term-arg0 ind-decl))
	(def (term-arg1 ind-decl)))
    (values (make-instance 'coinductive-decl
	      :declared-type (xt-not-enum-type-expr type-expr)
	      :definition (xt-expr def)
	      :place (term-place ind-decl))
	    type-expr)))

(defun xt-assumption (assumption)
  (make-instance 'formula-decl
    :spelling 'ASSUMPTION
    :definition (xt-expr (term-arg0 assumption))
    :place (term-place assumption)))

(defun xt-formula-decl (formula-decl)
  (let ((fname (term-arg0 formula-decl))
	(expr (term-arg1 formula-decl)))
    (when (and *no-obligations-allowed*
	       (is-sop 'OBLIGATION fname))
      (parse-error fname
	"The OBLIGATION keyword is not allowed here"))
    (if (is-sop 'TEST fname)
	(make-instance 'test-formula
	  :spelling (sim-term-op fname)
	  :definition (xt-expr expr)
	  :place (term-place formula-decl))
	(make-instance 'formula-decl
	  :spelling (sim-term-op fname)
	  :definition (xt-expr expr)
	  :place (term-place formula-decl)))))


;;; Type Expressions

(defun xt-not-enum-type-expr (type-expr)
  (let ((te (xt-type-expr type-expr)))
    (if (enumtype? te)
	(parse-error te
	  "Enumeration types are only allowed at the top level")
	te)))

(defun xt-type-expr (type-expr)
  (case (sim-term-op type-expr)
    ((TYPE-NAME NAME-EXPR)		; Allow name-exprs from actuals
     (xt-type-name type-expr))
    (TYPE-APPL (xt-type-appl type-expr))
    ;;(quotienttype (xt-quotienttype type-expr))
    (SUBTYPE (xt-subtype type-expr))
    (EXPR-AS-TYPE (xt-expr-as-type type-expr))
    ;;(enum-or-subtype (xt-enum-or-subtype type-expr))
    (ENUMTYPE (xt-enumtype type-expr))
    (FUNTYPE (xt-funtype type-expr))
    (RECORDTYPE (xt-recordtype type-expr))
    (VARIANTTYPE (parse-error type-expr
		   "Variant record types are not yet supported"))
    (EXTENDED-TYPE-NAME
     (xt-extended-type-name type-expr))
    (EXTENDED-TYPE
     (xt-extended-type type-expr))
    (STRUCT-SUBTYPE (xt-struct-subtype type-expr))
    (QUANT-TYPE (xt-quant-type type-expr))
    (GHOST (xt-ghost type-expr))
    (t (error "type-expr not recognized - ~a" type-expr))))

(defun xt-type-name (type-name)
  (let ((name (xt-name (term-arg0 type-name) nil)))
    (change-class name 'type-name)
    (setf (place name) (term-place type-name))
    name))

(defun xt-extended-type-name (type-expr)
  (let ((name (xt-type-name type-expr))
	(extension (xt-type-extension (term-arg1 type-expr))))
    (make-instance 'type-extension
      :type name
      :extension extension
      :place (term-place type-expr))))

(defun xt-extended-type (type-expr)
  (let ((type (xt-type-expr (term-arg0 type-expr)))
	(extension (xt-type-extension (term-arg1 type-expr))))
    (make-instance 'type-extension
      :type type
      :extension extension
      :place (term-place type-expr))))

(defun xt-struct-subtype (type-expr)
  (xt-type-expr (term-arg0 type-expr)))

(defun xt-type-extension (ext)
  (xt-type-expr ext))

(defun xt-record-extension (ext)
  (xt-fields (term-arg0 ext)))

(defun xt-tuple-extension (ext)
  (xt-dep-type-exprs ext))

(defun xt-type-appl (type-expr)
  (let ((type (term-arg0 type-expr))
	(args (term-arg1 type-expr)))
    (make-instance 'type-application
      :type (change-class (xt-name type nil) 'type-name)
      :parameters (mapcar #'xt-expr (term-args args))
      :place (term-place type-expr))))

(defun xt-enumtype (type-expr)
  (let ((args (xt-enumtype-args (term-arg0 type-expr))))
    (xt-enum args type-expr)))

(defun xt-enum (args enum)
  (let ((constrs
	 (mapcar #'(lambda (e)
		     (let* ((id (xt-idop e))
			    (constr (make-instance 'simple-constructor
				     :id id
				     :recognizer
				     (makesym "~a?" (if (memq id '(O |o|))
							'O
							(op-to-id id))))))
		       ;;(unparse constr :string t)
		       (setf (place constr) (term-place e))
		       constr))
		 args)))
    (make-instance 'enumtype
      :constructors constrs
      :place (term-place enum))))

(defun xt-enumtype-args (args &optional result)
  (if (is-sop 'SET-FORMALS args)
      (if (is-sop 'COMMA (term-arg1 args))
	  (xt-enumtype-args (term-arg2 args)
			    (cons (xt-enumtype-arg (term-arg0 args))
				  result))
	  (parse-error (term-arg1 args) "Comma expected here"))
      (nreverse (cons (xt-enumtype-arg args) result))))

(defun xt-enumtype-arg (arg)
  (unless (is-sop 'SET-ID arg)
    (parse-error arg "( not expected here"))
  (unless (is-sop 'NOTYPE (term-arg1 arg))
    (parse-error (term-arg1 arg) ": not expected here"))
  (term-arg0 arg))

(defun xt-subtype (type-expr)
  (let* ((args (term-arg0 type-expr))
	 (expr (term-arg1 type-expr))
	 (set-expr (mk-ergo-term* 'SET-EXPR args
				  (mk-ergo-term 'NO-TYPE-EXPR nil) expr))
	 (pred (make-xt-bind-expr 'set-expr set-expr set-expr)))
    (setf (place pred) (term-place type-expr))
    (make-instance 'subtype
      :predicate pred
      :place (term-place type-expr))))
	  
(defun xt-expr-as-type (expr-as-type)
  (make-instance 'expr-as-type
    :expr (xt-expr (term-arg0 expr-as-type))
    :place (term-place expr-as-type)))

(defun xt-idops (idops)
  (mapcar #'xt-idop (term-args idops)))

(defun xt-funtype (funtype)
  (let ((kind (term-arg0 funtype))
	(dep-type-exprs (term-arg1 funtype))
	(range (term-arg2 funtype)))
    (if (is-sop 'COMP-TYPE-EXPR-NULL-2 range)
	(if (is-sop 'COMP-TYPE-EXPR-NULL-1 kind)
	    (let ((dep-types (unless (is-sop 'emptytuple dep-type-exprs)
			       (xt-dep-type-exprs dep-type-exprs))))
	      (if (is-sop 'COTUPLETYPE dep-type-exprs)
		  (if (some #'dep-binding? dep-types)
		      (parse-error dep-type-exprs
			"Dependent types not allowed in cotuple types")
		      (make-instance 'cotupletype
			:types dep-types
			:place (term-place funtype)))
		  (if (singleton? dep-types)
		      (progn
			(incf (parens (car dep-types)))
			(car dep-types))
		      (make-instance 'tupletype
			:types dep-types
			:place (term-place funtype)))))
	    (parse-error funtype "Function type must have a range"))
	(let* ((ran (xt-not-enum-type-expr range))
	       (dom (xt-dep-type-exprs dep-type-exprs))
	       (tvar (make-new-variable '|t| (cons ran dom)))
	       (domain (xt-funtype-domain dom ran tvar)))
	  (make-instance (case (sim-term-op kind)
			   (COMP-TYPE-EXPR-NULL-1 'funtype)
			   (FUNCTION 'functiontype)
			   (ARRAY 'arraytype))
	    :domain domain
	    :range (xt-subst-new-domain-dep domain ran)
	    :place (term-place funtype))))))

(defmethod xt-subst-new-domain-dep (domain range)
  (declare (ignore domain))
  range)

(defmethod xt-subst-new-domain-dep ((domain dep-binding) range)
  (if (typep (declared-type domain) 'dep-domain-tupletype)
      (let ((tvar (id domain))
	    (bindings (var-bindings (declared-type domain))))
	(gensubst range
	  #'(lambda (ex)
	      (let ((index (cdr (assq (id ex) bindings))))
		(make-instance 'projappl
		  :id (makesym "PROJ_~d" index)
		  :index index
		  :argument (make-instance 'name-expr :id tvar))))
	  #'(lambda (ex)
	      (and (typep ex 'name-expr)
		   (assq (id ex) bindings)))))
      range))

(defun xt-funtype-domain (type-exprs ran tvar)
  (if (cdr type-exprs)
      (if (some #'dep-binding? type-exprs)
	  (let ((need-dep? (some #'(lambda (te)
				     (and (dep-binding? te)
					  (id-occurs-in (id te) ran)))
				 type-exprs)))
	    (xt-funtype-dep-domain type-exprs tvar need-dep?))
	  (make-instance 'domain-tupletype
	    :types type-exprs))
      (car type-exprs)))

(defun xt-funtype-dep-domain (type-exprs tvar need-dep? &optional (index 0)
					 types var-bindings)
  (if (null type-exprs)
      (if need-dep?
	  (make-instance 'dep-binding
	    :id tvar
	    :declared-type (make-instance 'dep-domain-tupletype
			     :types (nreverse types)
			     :var-bindings (nreverse var-bindings)))
	  (make-instance 'domain-tupletype
	    :types (nreverse types)))
      (if (typep (car type-exprs) 'dep-binding)
	  (let* ((id (id (car type-exprs)))
		 (var-binding (cons id (1+ index)))
		 (occurs? (id-occurs-in id (cdr type-exprs))))
	    (xt-funtype-dep-domain
	     (cdr type-exprs) tvar need-dep? (1+ index)
	     (cons (if occurs?
		       (car type-exprs)
		       (declared-type (car type-exprs)))
		   types)
	     (cons var-binding var-bindings)))
	  (xt-funtype-dep-domain
	   (cdr type-exprs) tvar need-dep? (1+ index)
	   (cons (car type-exprs) types)
	   var-bindings))))
	   
				   
      
(defun xt-dep-type-exprs (dep-type-exprs)
  (mapcar #'xt-dep-type-expr (term-args dep-type-exprs)))

(defun xt-dep-type-expr (dep-type-expr)
  (if (is-sop 'DEP-BINDING dep-type-expr)
      (xt-dep-binding dep-type-expr)
      (xt-not-enum-type-expr dep-type-expr)))

(defun xt-dep-binding (dep-binding)
  (let ((idop (term-arg0 dep-binding))
	(type (term-arg1 dep-binding)))
    (make-instance 'dep-binding
      :id (xt-idop idop)
      :declared-type (xt-not-enum-type-expr type)
      :place (term-place dep-binding))))

(defun xt-recordtype (recordtype)
  (make-instance 'recordtype
    :fields (xt-fields (term-arg0 recordtype))
    :place (term-place recordtype)))

(defun xt-fields (fields)
  (mapcan #'xt-field-decls (term-args fields)))

(defun xt-field-decls (field-decls)
  (let ((ids (term-arg0 field-decls))
	(type-expr (term-arg1 field-decls)))
    (xt-chained-decls (term-args ids)
		      nil
		      type-expr
		      nil
		      (make-instance 'field-decl
			:declared-type (xt-not-enum-type-expr type-expr)
			:place (term-place field-decls))
		      field-decls)))

;; <quant-type({forall()|exists()},lambda-formals, type-expr)>
(defun xt-quant-type (te)
  (let* ((quant (term-arg0 te))
	 (commas? (xt-lambda-formals-check (term-arg1 te)))
	 (bindings (xt-lambda-formals (term-arg1 te) commas?))
	 (texpr (xt-type-expr (term-arg2 te))))
    (case (sim-term-op quant)
      (FORALL (make-instance 'forall-type
		:bindings (caar bindings)
		:type texpr
		:place (term-place te)))
      (EXISTS (make-instance 'exists-type
		:bindings (caar bindings)
		:type texpr
		:place (term-place te))))))

(defun xt-ghost (te)
  (let ((texpr (xt-type-expr (term-arg0 te))))
    (setf (ghost? texpr) t)
    texpr))


;;; Expressions

(defun xt-expr (expr)
  (case (sim-term-op expr)
    (NUMBER-EXPR (xt-number-expr expr))
    (STRING-EXPR (xt-string-expr expr))
    (NAME-EXPR (xt-name-expr expr))
    (IFAPPL (xt-ifappl expr))
    ;;(TYPED-NAME (xt-typed-name expr))
    (LIST-EXPR (xt-list-expr expr))
    ;;(true (xt-true expr))
    ;;(false (xt-false expr))
    (REC-EXPR (xt-rec-expr expr))
    (TUPLE-EXPR (xt-tuple-expr expr))
    (TERM-EXPR (xt-term-expr expr))
    (UNARY-TERM-EXPR (xt-unary-term-expr expr))
    (FIELDEX (xt-fieldex expr))
    (PROJEX (xt-projex expr))
    (FIELDAPPL (xt-fieldappl expr))
    (PROJAPPL (xt-projappl expr))
    ;;(intype (xt-intype expr))
    (COERCION (xt-coercion expr))
    (IF-EXPR (xt-if-expr expr))
    (APPLICATION (xt-application expr))
    (BIND-EXPR (xt-bind-expr expr))
    (NAME-BIND-EXPR (xt-name-bind-expr expr))
    (SET-EXPR (xt-set-expr expr))
    (SET-LIST-EXPR (xt-set-list-expr expr))
    (ARRAY-EXPR (xt-array-expr expr))
    (LET-EXPR (xt-let-expr expr))
    (WHERE-EXPR (xt-where-expr expr))
    (UPDATE-EXPR (xt-update-expr expr))
    ;;(override-expr (xt-override-expr expr))
    (CASES-EXPR (xt-cases-expr expr))
    (COND-EXPR (xt-cond-expr expr))
    (TABLE-EXPR (xt-table-expr expr))
    (SKOVAR (xt-skovar expr))
    (BRACKET-EXPR (xt-bracket-expr expr))
    (CHAR-EXPR (xt-char-expr expr))
    (t (error "xt-expr: unrecognized expr - ~a" expr))))

(defun xt-number-expr (expr)
  (let ((num (ds-number (term-arg0 expr))))
    (assert (integerp num))
    (make-instance 'number-expr
      :number num
      :place (term-place expr))))

(defun xt-string-expr (expr)
  (let ((string (ds-string (term-arg0 expr)))
	(ne (mk-name-expr '|char|)))
    (make-instance 'string-expr
      :string-value string
      :operator (mk-name-expr '|list2finseq| (list (mk-actual ne)))
      :argument (xt-string-to-charlist string (term-place expr))
      :place (term-place expr))))

(defun xt-string-to-charlist (string place)
  (let ((codes (xt-string-to-codes string 0 (length string) place nil)))
    (xt-string-to-charlist* codes place)))

(defun xt-string-to-charlist* (codes place)
  (if (null codes)
      (add-place (make-instance 'null-expr
		   :id '|null|)
		 (vector (ending-row place) (ending-col place)
			 (ending-row place) (ending-col place)))
      (let* ((code (caar codes))
	     (cplace (cdar codes))
	     (aplace (vector (starting-row cplace) (starting-col cplace)
			     (ending-row place) (ending-col place)))
	     (scar (add-place
		    (mk-application (add-place (mk-name-expr '|char|) cplace)
		      (add-place (mk-number-expr code) cplace))
		    cplace))
	     (scdr (xt-string-to-charlist* (cdr codes) place)))
	 (make-instance 'list-expr
	   :operator (add-place (mk-name-expr '|cons|) cplace)
	   :argument (make-instance 'arg-tuple-expr
		       :exprs (list scar scdr)
		       :place aplace)
	   :place aplace))))

(defun mk-char-expr (code place)
  (let ((ex (add-place
	     (mk-application (add-place (mk-name-expr '|char|) place)
	       (add-place (mk-number-expr code) place))
	     place)))
    (change-class ex 'char-expr :code code)))

(defun xt-string-to-codes (string pos len place codes)
  (if (>= pos len)
      (nreverse codes)
      (multiple-value-bind (code npos bkslash?)
	  (next-string-code string pos len place)
	(let* ((strow (starting-row place))
	       (stcol (starting-col place))
	       (nplace (vector strow (+ stcol pos) strow (+ stcol npos)))
	       (acodes (acons code nplace codes))
	       (ncodes (if bkslash? (acons code nplace acodes) acodes)))
	  (xt-string-to-codes string npos len place ncodes)))))

(defun next-string-code (string pos len place)
  "Returns the next char and new string position. If the char is a
backslash, interpret the usual \n, \', etc. as well as \x (hex), \0 (octal),
\# (decimal). If not followed by one of these, it's returned as a backslash."
  (let ((char (char string pos)))
    (cond ((or *tex-mode*
	       (char/= char #\\))
	   (values (char-code char) (1+ pos)))
	  ;; have a backslash below
	  ((>= (1+ pos) len)
	   (let ((cplace (vector (svref place 0) (+ (svref place 1) pos)
				 (svref place 0) (+ (svref place 1) pos))))
	     (parse-error cplace "String ends with a '\\'")))
	  (t (let ((echar (char string (1+ pos))))
	       (case echar
		 (#\a (values (char-code #-gcl #\Bell #+gcl #\^G) (+ pos 2)))
		 (#\b (values (char-code #\Backspace) (+ pos 2)))
		 (#\f (values (char-code #\Page) (+ pos 2)))
		 (#\n (values (char-code #\Newline) (+ pos 2)))
		 (#\r (values (char-code #\Return) (+ pos 2)))
		 (#\t (values (char-code #\Tab) (+ pos 2)))
		 (#\v (values (char-code #-gcl #\VT #+gcl #\^K) (+ pos 2)))
		 (#\' (values (char-code #\') (+ pos 2)))
		 (#\" (values (char-code #\") (+ pos 2)))
		 (#\? (values (char-code #\?) (+ pos 2)))
		 (#\\ (values (char-code #\\) (+ pos 2) t))
		 ((#\x #\X)
		  (if (digit-char-p (char string (+ pos 2)) 16)
		      (parse-integer string :radix 16 :start (+ pos 2)
				     :end (min (+ pos 4) len) :junk-allowed t)
		      (let ((cplace (vector (svref place 0)
					    (+ (svref place 1) pos 2)
					    (svref place 0)
					    (+ (svref place 1) pos 2))))
			(parse-error cplace
			  "Illegal character after '\\X': must be a hex digit"
			  ))))
		 (#\0
		  (if (digit-char-p (char string (+ pos 2)) 8)
		      (multiple-value-bind (chcode npos)
			  (parse-integer string :radix 8 :start (+ pos 2)
					 :end (min (+ pos 5) len)
					 :junk-allowed t)
			(if (< chcode 256)
			    (values chcode npos)
			    (let ((cplace (vector (svref place 0)
						  (+ (svref place 1) pos 2)
						  (svref place 0)
						  (+ (svref place 1) pos 2))))
			      (parse-error cplace
				"Octal code must be less than 0400"))))
		      (let ((cplace (vector (svref place 0)
					    (+ (svref place 1) pos 2)
					    (svref place 0)
					    (+ (svref place 1) pos 2))))
			(parse-error cplace
			  "Illegal character after '\\0': must be a octal digit"
			  ))))
		 ((digit-char-p echar)
		  (multiple-value-bind (chcode npos)
		      (parse-integer string :radix 10 :start (1+ pos)
				     :end (min (+ pos 4) len)
				     :junk-allowed t)
		    (if (< chcode 256)
			(values chcode npos)
			(let ((cplace (vector (svref place 0)
					      (+ (svref place 1) pos 1)
					      (svref place 0)
					      (+ (svref place 1) pos 1))))
			  (parse-error cplace
			    "Decimal code must be less than 256")))))
		 (t (values (char-code char) (1+ pos)))))))))

(defun xt-list-expr (expr)
  (let* ((place (term-place expr))
	 (last-place (when place
		       (vector (ending-row place) (ending-col place)
			       (ending-row place) (ending-col place))))
	 (nullex (make-instance 'null-expr
		   :id '|null|
		   :place last-place))
	 (lexpr (xt-list-expr* (reverse (term-args expr)) nullex place)))
    (setf (place lexpr) place)
    lexpr))

(defun xt-list-expr* (exprs listex list-place)
  (if (null exprs)
      listex
      (let* ((ex (xt-expr (car exprs)))
	     (cplace (concat-places (place ex) list-place))
	     (consex
	      (make-instance 'list-expr
		:operator (make-instance 'name-expr
			    :id '|cons|
			    :place (place ex))
		:argument (make-instance 'arg-tuple-expr
			    :exprs (list ex listex)
			    :place cplace)
		:place cplace)))
	(xt-list-expr* (cdr exprs) consex list-place))))

(defun xt-bracket-expr (expr)
  (let ((op (ds-id (term-arg0 expr)))
	(args (term-args (term-arg1 expr))))
    (if args
	(make-instance 'bracket-expr
	  :operator (make-instance 'name-expr
		      :id op
		      :place (term-place expr))
	  :argument (xt-arg-expr args)
	  :place (term-place expr))
	(make-instance 'name-expr
	  :id op
	  :place (term-place expr)))))

(defvar *escaped-chars*
  '((#\0 . 0)
    (#\a . 7)
    (#\b . 8)
    (#\t . 9)
    (#\n . 10)
    (#\v . 11)
    (#\f . 12)
    (#\r . 13)
    (#\\ . 92)))

(defun xt-char-expr (expr)
  ;; keyword is any character between single quotes
  ;; this is was unused before PVS 7.2, but is now used for char
  (let ((char-str (ds-keyword (term-arg0 expr))))
    (when (= (length char-str) 0)
      (parse-error expr "Character not provided."))
    (let ((char (char char-str 0))
	  (place (term-place expr)))
      (if (= (length char-str) 1)
	  (mk-char-expr (char-code char) place)
	  (let* ((char2 (char char-str 1))
		 (ecode (cdr (assoc char2 *escaped-chars* :test #'char=))))
	    (if ecode
		(if (= (length char-str) 2)
		    (mk-char-expr ecode place)
		    (parse-error expr "Only one char allowed in char literal"))
		(if (char= char2 #\u)
		    (multiple-value-bind (code err)
			(ignore-errors (parse-integer char-str :start 2 :radix 16))
		      (cond ((null code)
			     (parse-error expr "char literal error: ~a" err))
			    ((> code #x10FFFF)
			     (parse-error expr "char literal is too large, must be <= 10FFFF"))
			    (t (mk-char-expr code place)))))))))))

(defun xt-arg-expr (args)
  (if (cdr args)
      (let ((exprs (mapcar #'xt-expr args)))
	(make-instance 'arg-tuple-expr
	  :exprs exprs
	  :place (merge-places (place (car exprs))
			       (place (car (last exprs))))))
      (xt-expr (car args))))

(defun xt-name-expr (expr)
  (let ((name (xt-name (term-arg0 expr) nil))
	(type (unless (is-sop 'NOTYPE (term-arg1 expr))
		(xt-type-expr (term-arg1 expr))))
	(pred (unless (is-sop 'NOPRED (term-arg2 expr))
		(xt-expr (term-arg2 expr)))))
    (cond ((or type pred)
	   (xt-typed-name expr name type pred))
	  ((typep name '(or number-expr decimal))
	   name)
	  (t (let ((upid (intern (string-upcase (format nil "~a" (id name))) :pvs)))
	       (multiple-value-bind (prindex prkind)
		   (projection? upid)
		 (if prindex
		     (cond ;;((actuals name)
		       ;;(parse-error expr "Projection may not have actuals"))
		       ((mod-id name)
			(parse-error expr "Projection may not have a theory id"))
		       ((library name)
			(parse-error expr "Projection may not have a library id"))
		       ((zerop prindex)
			(parse-error expr "Projection index may not be zero"))
		       (t (case prkind
			    (proj (make-instance 'projection-expr
				    :id upid
				    :actuals (actuals name)
				    :index prindex
				    :place (term-place expr)))
			    (in (make-instance 'injection-expr
				  :id upid
				  :actuals (actuals name)
				  :index prindex
				  :place (term-place expr)))
			    (in? (make-instance 'injection?-expr
				   :id upid
				   :actuals (actuals name)
				   :index prindex
				   :place (term-place expr)))
			    (out (make-instance 'extraction-expr
				   :id upid
				   :actuals (actuals name)
				   :index prindex
				   :place (term-place expr)))
			    (t (error "problem in xt-name-expr")))))
		     (progn (change-class name 'name-expr)
			    (setf (place name) (term-place expr))
			    name))))))))

(defun xt-typed-name (expr name type pred)
  (unless *allowed-typed-names*
    (parse-error expr "Binding not allowed here"))
  (when (eq *allowed-typed-names* t)
    (setq *allowed-typed-names* expr))
  (when (or (mod-id name)
	    (library name)
	    (actuals name)
	    (dactuals name)
	    (mappings name)
	    (target name))
    (parse-error expr "Binding id must be a simple id"))
  (assert (id name))
  (if pred
      (let* ((id (id name))
	     (id-place (term-place (term-arg0 (term-arg0 expr))))
	     (formals (xt-typed-id-bind-decl id type id-place))
	     (dtype (make-instance 'setsubtype
		      :supertype type
		      :formals formals
		      :formula pred
		      :place (place type))))
	(make-instance 'pred-bind-decl
	  :id id
	  :declared-type dtype
	  :place (term-place expr)))
      (xt-typed-id-bind-decl (id name) type (term-place expr))))

(defun xt-ifappl (expr)
  (let ((if-name (mk-name-expr 'IF))
	(args (term-arg0 expr))
	(place (term-place expr)))
    (setf (place if-name)
	  (vector (svref place 0) (svref place 1)
		  (+ (svref place 0) 2) (svref place 1)))
    (unless (is-sop 'TUPLE-EXPR args)
      (parse-error expr "Argument to IF without THEN must have parens"))
    (make-instance 'application
      :operator if-name
      :argument (xt-arg-expr (term-args args))
      :place place)))

(defun projection? (id)
  (when (symbolp id)
    (let ((str (string-upcase (string id))))
      (cond ((and (> (length str) 5)
		  (string= str "PROJ_" :end1 5)
		  (every #'digit-char-p (subseq str 5)))
	     (values (parse-integer str :start 5) 'proj))
	    ((and (> (length str) 3)
		  (string= str "IN_" :end1 3)
		  (every #'digit-char-p (subseq str 3)))
	     (values (parse-integer str :start 3) 'in))
	    ((and (> (length str) 4)
		  (string= str "IN?_" :end1 4)
		  (every #'digit-char-p (subseq str 4)))
	     (values (parse-integer str :start 4) 'in?))
	    ((and (> (length str) 4)
		  (string= str "OUT_" :end1 4)
		  (every #'digit-char-p (subseq str 4)))
	     (values (parse-integer str :start 4) 'out))))))

(defun xt-rec-expr (rec-expr)
  (make-instance 'record-expr
    :assignments (mapcar #'xt-assignment
			 (term-args (term-arg0 rec-expr)))
    :place (term-place rec-expr)))

(defun xt-tuple-expr (expr)
  (let ((exprs (mapcar #'xt-expr (term-args expr))))
    (cond ((null exprs)
	   ;;(parse-error expr "Expression expected here")
	   (make-instance 'tuple-expr
	     :exprs nil
	     :place (term-place expr)))
	  ((cdr exprs)
	   (make-instance 'tuple-expr
	     :exprs exprs
	     :place (term-place expr)))
	  (t (incf (parens (car exprs)))
	     (car exprs)))))

(defun xt-fieldex (expr)
  (multiple-value-bind (acts) ;; May need to deal with actuals, dactuals
      (unless (is-sop 'NOACTUALS (term-arg1 expr))
	(xt-actuals (term-arg1 expr) t))
    (make-instance 'fieldex
      :id (ds-id (term-arg0 expr))
      :actuals acts
      :place (term-place expr))))

(defun xt-fieldappl (expr)
  (make-instance 'fieldappl
    :id (ds-id (term-arg1 expr))
    :argument (xt-expr (term-arg0 expr))
    :place (term-place expr)))

(defun xt-projex (expr)
  (let ((acts (unless (is-sop 'NOACTUALS (term-arg1 expr))
		(xt-actuals (term-arg1 expr) t))))
    (make-instance 'projex
      :index (ds-number (term-arg0 expr))
      :actuals acts)))

(defun xt-projappl (expr)
  (let ((index (ds-number (term-arg1 expr))))
    (if (zerop index)
	(parse-error (term-arg1 expr) "Projection index may not be zero")
	(make-instance 'projappl
	  :index index
	  :argument (xt-expr (term-arg0 expr))
	  :place (term-place expr)))))

(defun xt-term-expr (expr)
  (let* ((op (term-arg0 expr))
	 (opid (sim-term-op op))
	 (args (term-args (term-arg1 expr)))
	 (arg (if (cdr args)
		  (let ((exprs (mapcar #'xt-expr args)))
		    (make-instance 'arg-tuple-expr
		      :exprs exprs
		      :place (merge-places (place (car exprs))
					   (place (car (last exprs))))))
		  (xt-expr (car args))))
	 (ne (make-instance 'name-expr
	       :id opid
	       :place (term-place op))))
    (if (and (memq opid *pvs-relational-operators*)
	     (arg-tuple-expr? arg)
	     (= (length (exprs arg)) 2))
	(let* ((lhs (rightmost-relation-term (car (exprs arg)) opid))
	       (rhs (leftmost-relation-term (cadr (exprs arg)) opid))
	       (app (mk-application ne lhs rhs)))
	  (setf (place app)
		(merge-places (place lhs) (place rhs)))
	  (if (eq lhs (car (exprs arg))) ; cannot be conjoined
	      (if (eq rhs (cadr (exprs arg)))
		  app
		  (mk-chained-relation app rhs))
	      (let ((conj1 (mk-chained-relation (car (exprs arg)) app)))
		(if (eq rhs (cadr (exprs arg)))
		    conj1
		    (mk-chained-relation conj1 (cadr (exprs arg)))))))
	(make-instance 'infix-application
	  :operator ne
	  :argument arg
	  :place (term-place expr)))))

(defun mk-chained-relation (lhs rhs)
  (make-instance 'chained-relation
    :operator (make-instance 'name-expr :id 'AND)
    :argument (make-instance 'arg-tuple-expr
		:exprs (list lhs rhs)
		:place (merge-places (place lhs) (place rhs)))))


;; Get the lhs arg, which is the rightmost element
;; idea is that with a infix rel expr 'lex R rex',
;; where R is a binary infix relational operator,
;; we want the right-most arg of lex and the left-most arg of rex
(defmethod rightmost-relation-term ((arg infix-application) op)
  (if (and (zerop (parens arg))
	   (name-expr? (operator arg))
	   (memq (id (operator arg)) *pvs-relational-operators*)
	   (or (and (not (memq op *pvs-equality-operators*))
		    (not (memq (operator arg) *pvs-equality-operators*)))
	       (eq op (id (operator arg)))))
      (args2 arg)
      arg))

(defmethod rightmost-relation-term ((arg chained-relation) op)
  (if (or (and (not (memq op *pvs-equality-operators*))
	       (not (memq (operator (args2 arg)) *pvs-equality-operators*)))
	  (eq op (id (operator (args2 arg)))))
      (rightmost-relation-term (args2 arg) op)
      arg))

(defmethod rightmost-relation-term ((arg t) op)
  (declare (ignore op))
  arg)

(defmethod leftmost-relation-term ((arg infix-application) op)
  (if (and (zerop (parens arg))
	   (name-expr? (operator arg))
	   (memq (id (operator arg)) *pvs-relational-operators*)
	   (or (and (not (memq op *pvs-equality-operators*))
		    (not (memq (operator arg) *pvs-equality-operators*)))
	       (eq op (id (operator arg)))))
      (args1 arg)
      arg))

(defmethod leftmost-relation-term ((arg chained-relation) op)
  (if (or (and (not (memq op *pvs-equality-operators*))
	       (not (memq (operator (args2 arg)) *pvs-equality-operators*)))
	  (eq op (id (operator (args2 arg)))))
      (leftmost-relation-term (args1 arg) op)
      arg))

(defmethod leftmost-relation-term ((arg t) op)
  (declare (ignore op))
  arg)


(defun xt-unary-term-expr (uexpr)
  (let* ((op (term-arg0 uexpr))
	 (expr (term-arg1 uexpr))
	 (opex (mk-name-expr (sim-term-op op))))
    (setf (place opex) (term-place op))
    (make-instance 'unary-application
      :operator opex
      :argument (xt-expr expr)
      :place (term-place uexpr))))

(defun xt-coercion (coercion)
  (let* ((expr (term-arg0 coercion))
	 (type (term-arg0 (term-arg1 coercion)))
	 (var (makesym "x¢~a" (funcall *coercion-var-counter*))))
    (make-instance 'coercion
      :operator (mk-lambda-expr (list (mk-bind-decl var (xt-type-expr type)))
		  (mk-name-expr var))
      :argument (xt-expr expr)
      :place (term-place coercion))))

(defun xt-if-expr (expr)
  (let* ((cond (term-arg0 expr))
	 (then (term-arg1 expr))
	 (elsif (term-arg2 expr))
	 (else (term-arg3 expr))
	 (place (term-place expr))
	 (if-place (when place
		     (vector (starting-row place) (starting-col place)
			     (starting-row place) (+ (starting-col place) 2))))
	 (if-name (make-instance 'name-expr
		    :id 'IF
		    :place if-place)))
    (make-instance 'if-expr
      :operator if-name
      :argument (make-instance 'arg-tuple-expr
		  :exprs (list (xt-expr cond)
			       (xt-expr then)
			       (xt-elsif-expr (term-args elsif) else))
		  :place (merge-places (term-place cond)
				       (term-place else)))
      :place (term-place expr))))

(defun xt-elsif-expr (elsifs else)
  (if (null elsifs)
      (xt-expr else)
      (let* ((tplace (term-place (car elsifs)))
	     (place (when tplace
		      (vector (starting-row tplace) (starting-col tplace)
			      (starting-row tplace) (+ (starting-col tplace) 5)))))
	(make-instance 'chained-if-expr
	  :operator (make-instance 'name-expr
		      :id 'IF
		      :place place)
	  :argument (make-instance 'arg-tuple-expr
		      :exprs (list (xt-expr (term-arg0 (car elsifs)))
				   (xt-expr (term-arg1 (car elsifs)))
				   (xt-elsif-expr (cdr elsifs) else)))))))

(defun xt-application (expr)
  (let ((op (xt-expr (term-arg0 expr)))
	(args (term-args (term-arg1 expr))))
    (unless args
      (parse-error (term-arg1 expr) "argument expected here"))
    (cond ((typep op 'projection-expr)
	   (make-instance 'projection-application
	     :id (id op)
	     :actuals (actuals op)
	     :index (index op)
	     :argument (xt-arg-expr args)
	     :place (term-place expr)))
	  ((typep op 'injection-expr)
	   (make-instance 'injection-application
	     :id (id op)
	     :actuals (actuals op)
	     :index (index op)
	     :argument (xt-arg-expr args)
	     :place (term-place expr)))
	  ((typep op 'injection?-expr)
	   (make-instance 'injection?-application
	     :id (id op)
	     :actuals (actuals op)
	     :index (index op)
	     :argument (xt-arg-expr args)
	     :place (term-place expr)))
	  ((typep op 'extraction-expr)
	   (make-instance 'extraction-application
	     :id (id op)
	     :actuals (actuals op)
	     :index (index op)
	     :argument (xt-arg-expr args)
	     :place (term-place expr)))
	  (t (make-instance 'application
	       :operator op
	       :argument (xt-arg-expr args)
	       :place (term-place expr))))))


(defun xt-bind-expr (bexpr)
  (let ((op (term-arg0 bexpr))
	(body (term-arg1 bexpr)))
    (make-xt-bind-expr
     #+(and allegro (version>= 6))
     (intern (string-downcase (symbol-name (sim-term-op op))) :pvs)
     #-(and allegro (version>= 6)) (sim-term-op op)
     body bexpr)))

(defun xt-name-bind-expr (bexpr)
  (let* ((op (term-arg0 bexpr))
	 (nop (make-instance 'name-expr
		:id (ds-id op)
		:place (term-place op)))
	 (body (term-arg1 bexpr))
	 (arg (make-xt-bind-expr 'LAMBDA body nil)))
    (assert (term-place body))
    (setf (place arg)
	  (concatenate 'vector
	    (term-place body)
	    (list (format nil "parsing a named binding expression for '~a'" nop))))
    (assert (place arg))
    (make-instance 'binding-application
      :operator nop
      :argument arg
      :place (term-place bexpr))))

(defun xt-skovar (expr)
  (make-instance 'name-expr
    :id (makesym "~a!~d"
		 (ds-id (term-arg0 expr))
		 (ds-number (term-arg1 expr)))
    :place (term-place expr)))

(defun make-xt-bind-expr (op body save-as)
  (let* (;;(set-expr? (is-sop 'SET-EXPR body))
	 (commas? (xt-lambda-formals-check (term-arg0 body)))
	 (bindings (xt-lambda-formals (term-arg0 body) commas?))
	 (expr (xt-expr (term-arg2 body)))
	 (lambda-type (unless (is-sop 'NO-TYPE-EXPR (term-arg1 body))
			(xt-not-enum-type-expr (term-arg1 body))))
	 (class (bind-expr-class op)))
    (if lambda-type
	(if (memq class '(lambda-expr set-expr))
	    (make-instance (if (eq class 'lambda-expr)
			       'lambda-expr-with-type
			       'set-expr-with-type)
	      :op op
	      :declared-ret-type lambda-type
	      :bindings (xt-flatten-bindings (car bindings) 0)
	      :expression (make-xt-bind-expr* (cdr bindings) class expr)
	      :commas? commas?
	      :place (term-place save-as))
	    (parse-error (term-arg1 body)
	      "Return type only allowed for lambda expressions"))
	(make-instance class
	  :op op
	  :bindings (xt-flatten-bindings (car bindings) 0)
	  :expression (make-xt-bind-expr* (cdr bindings) class expr)
	  :commas? commas?
	  :place (term-place save-as)))))

(defun bind-expr-class (op)
  (or (cdr (assoc op '((λ . lambda-expr)) :test #'string=))
      (case (intern (string-upcase op) :pvs)
	((LAMBDA) 'lambda-expr)
	((FORALL ∀) 'forall-expr)
	((EXISTS ∃) 'exists-expr)
	((SET-EXPR) 'set-expr)
	(t (break "bind-expr for ~a not set" op)))))


;;; Checks whether the formals all make sense, and returns whether the
;;; formals are separated by commas or whitespace.  Note that even with
;;; one formal, the separator is important; this is used to distinguish
;;; between (LAMBDA x: x) and (LAMBDA (x): x) --- returns T and NIL, resp.

(defun xt-lambda-formals-check (lambda-formals &optional comma-needed?)
  (if (or (is-sop 'LAMBDA-FORMALS lambda-formals)
	  (is-sop 'SET-FORMALS lambda-formals))
      (let ((lambda-formal (term-arg0 lambda-formals))
	    (comma (term-arg1 lambda-formals)))
	(xt-lambda-formal-check lambda-formal comma comma-needed?)
	(xt-lambda-formals-check (term-arg2 lambda-formals)
				 (if (is-sop 'COMMA comma) 'yes 'no)))
      (xt-lambda-formal-check lambda-formals nil comma-needed?)))

(defun xt-lambda-formal-check (lambda-formal comma comma-needed?)
  (when (or (and (eq comma-needed? 'yes)
		 comma
		 (is-sop 'NOCOMMA comma))
	    (and (eq comma-needed? 'no)
		 comma
		 (is-sop 'IDOP lambda-formal)))
    (parse-error comma ", expected here"))
  (when (and (eq comma-needed? 'no)
	     comma
	     (is-sop 'COMMA comma))
    (parse-error comma ", not allowed here"))
  (when (and (eq comma-needed? 'no)
	     (is-sop 'IDOP lambda-formal))
    (parse-error lambda-formal "( expected here"))
  (when (is-sop 'ADFORMALS lambda-formal)
    (let ((notype-or-pred (find-if #'(lambda (adf)
				       (and (is-sop 'TYPED-IDS adf)
					    (is-sop 'NO-TYPE-EXPR (term-arg1 adf))
					    (is-sop 'NO-PRED (term-arg2 adf))))
			    (term-args lambda-formal))))
      (when notype-or-pred
	(parse-error (car (term-args lambda-formal))
	  "Remove parentheses or give a type or predicate"))))
  (when (and (eq comma-needed? 'yes)
	     (is-sop 'ADFORMALS lambda-formal))
    (let* ((adformals (term-args lambda-formal))
	   (too-many-parens (find-if #'(lambda (adf)
					 (is-sop 'TYPED-IDS adf))
			      adformals))
	   (has-pred (find-if-not #'(lambda (adf)
				      (is-sop 'NO-PRED (term-arg2 adf)))
		       adformals))
	   (has-type (find-if-not #'(lambda (adf)
				      (is-sop 'NO-TYPE-EXPR (term-arg1 adf)))
		       adformals)))
      (when too-many-parens
	(parse-error too-many-parens "id expected here"))
      (when (and has-pred
		 (cdr adformals))
	(parse-error has-pred
	  "Only a single id allowed when a predicate is given"))
      (when (and (null has-type) (null has-pred))
	(parse-error (car adformals)
	  "Remove parentheses or give a type or predicate"))
      (when (and has-type
		 (not (eq has-type (car (last adformals)))))
	(parse-error has-pred "Only the last identifier may specify a type"))))
  ;; If comma is nil, then we are at the last binding.  If
  ;; comma-needed? is also nil, then there is only one, and we return
  ;; t if it is an idop or a parenthesized typed-ids.  This is needed
  ;; in order to correctly reconstruct the bindings list (see
  ;; xf-lambda-formal in unparse.lisp.
  (if comma-needed?
      (eq comma-needed? 'yes)
      (xt-formal-at-level-one? lambda-formal)))

(defun xt-formal-at-level-one? (lambda-formal)
  (or (and (is-sop 'IDOP lambda-formal) t)
      (let ((adformals (term-args lambda-formal)))
	(and (every #'(lambda (adf) (is-sop 'TYPED-ID adf))
		    adformals)
	     (every #'(lambda (adf)
			(and (is-sop 'NO-TYPE-EXPR (term-arg1 adf))
			     (is-sop 'NO-PRED (term-arg2 adf))))
		    (butlast adformals))
	     (not (and (is-sop 'NO-TYPE-EXPR
			       (term-arg1 (car (last adformals))))
		       (is-sop 'NO-PRED
			       (term-arg2 (car (last adformals))))))))))

(defun xt-lambda-formals (lambda-formals &optional commas? bindings lbindings)
  (if (or (is-sop 'LAMBDA-FORMALS lambda-formals)
	  (is-sop 'SET-FORMALS lambda-formals))
      (let* ((formals (xt-lambda-formal (term-arg0 lambda-formals)))
	     (lambda-formals-rest (term-arg2 lambda-formals))
	     (nbindings (cons formals bindings)))
	(xt-lambda-formals lambda-formals-rest
			   commas?
			   (when commas?
			     nbindings)
			   (if commas?
			       lbindings
			       (cons (nreverse nbindings) lbindings))))
      (let* ((formals (xt-lambda-formal lambda-formals))
	     (bindings (if commas?
			   (nreverse (cons (nreverse (cons formals bindings))
					   lbindings))
			   (nreverse (cons (list formals)
					   (if bindings
					       (cons bindings lbindings)
					       lbindings))))))
	bindings)))

(defun xt-lambda-formal (formal)
  (cond ((is-sop 'IDOP formal)
	 (make-instance 'bind-decl
	   :id (xt-idop formal)
	   :place (term-place formal)))
	((is-sop 'SET-ID formal)
	 (if (is-sop 'NOTYPE (term-arg1 formal))
	     (make-instance 'untyped-bind-decl
	       :id (xt-idop (term-arg0 formal))
	       :place (term-place formal))
	     (make-instance 'bind-decl
	       :id (xt-idop (term-arg0 formal))
	       :declared-type (xt-type-expr (term-arg1 formal))
	       :place (term-place formal))))
	(t (xt-lpdformals (term-args formal)))))

(defun xt-lpdformals (adformals)
  (xt-ladformals adformals))

(defun xt-ladformals (formals &optional bindings)
  (if (null formals)
      (nreverse bindings)
      (xt-ladformals
       (cdr formals)
       (cons (if (is-sop 'TYPED-ID (car formals))
		 (xt-typed-id (car formals))
		 (xt-typed-ids (car formals)))
	     bindings))))


;;; The primary thing that flatten-bindings does is to flatten the
;;; bindings-structure into a flat list of bindings.  On the way, it looks
;;; ahead for default types of bindings, if necessary.  Thus in
;;; (LAMBDA (x,y,z: int): y), x and y get declared-type int associated
;;; with them, while in (LAMBDA (x,(y),z: int): y), only z gets int.
;;; Basically, for any bindings list, the declared type is set to whatever
;;; can be found before a parentheses is found, unless aleady set.

(defun xt-flatten-bindings (bindings-structure level)
  (xt-set-declared-types bindings-structure)
  (mapc #'(lambda (x) (xt-set-paren-level x level))
	bindings-structure)
  (xt-flatten-bindings* bindings-structure))

(defun xt-set-declared-types (bindings-structure)
  (when (and bindings-structure
	     (listp bindings-structure))
    (cond ((listp (car bindings-structure))
	   (xt-set-declared-types (car bindings-structure))
	   (xt-set-declared-types (cdr bindings-structure)))
	  ((typep (car bindings-structure) 'untyped-bind-decl)
	   (dolist (b (cdr bindings-structure))
	     (cond ((or (listp b) (plusp (parens b)))
		    (setf (chain? (car bindings-structure)) nil)
		    (return))
		   ((declared-type b)
		    (setf (declared-type (car bindings-structure))
			  (declared-type b))
		    (setf (chain? (car bindings-structure)) t)
		    (return))))
	   (xt-set-declared-types (cdr bindings-structure)))
	  (t (xt-set-declared-types (cdr bindings-structure))))))

(defun xt-set-paren-level (bindings-structure &optional (level 0))
  (cond ((null bindings-structure) nil)
	((listp bindings-structure)
	 (xt-set-paren-level (car bindings-structure) (1+ level))
	 (xt-set-paren-level (cdr bindings-structure) level))
	(t (setf (parens bindings-structure) (max 0 (1- level))))))

(defun xt-flatten-bindings* (bindings-structure)
  (when bindings-structure
    (if (listp bindings-structure)
	(nconc (xt-flatten-bindings* (car bindings-structure))
	       (xt-flatten-bindings* (cdr bindings-structure)))
	(list bindings-structure))))
	      
  

(defun make-xt-bind-expr* (bindings class expr)
  (if bindings
      (make-instance class
	:bindings (xt-flatten-bindings (car bindings) 0)
	:expression (make-xt-bind-expr* (cdr bindings) class expr)
	:chain? t)
      expr))

(defun xt-set-expr (set-expr)
  (make-xt-bind-expr 'set-expr set-expr set-expr))

(defun xt-set-list-expr (exs)
  (let* ((exprs (mapcar #'xt-expr (term-args exs)))
	 ;;(tvar (make-new-variable 'x exprs))
	 )
    (make-instance 'set-list-expr
      :exprs exprs)))

(defun xt-array-expr (exs)
  (let* ((exprs (mapcar #'xt-expr (term-args exs)))
	 ;;(tvar (make-new-variable 'x exprs))
	 )
    (make-instance 'array-expr
      :exprs exprs
      :place (term-place exs))))

;; Note that xt-let-expr works as a lisp let* - this is so that later
;; bindings may refer to earlier ones.  Thus
;; LET x = e, y = x + 1 IN f(x, y)    becomes
;; (LAMBDA x: (LAMBDA y: f(x, y))(x + 1))(e)   and *NOT*
;; (LAMBDA (x, y): f(x, y))(e, x + 1), which will not typecheck.

(defun xt-let-expr (lexpr)
  (let* ((let-bindings (term-arg0 lexpr))
	 (expr (xt-expr (term-arg1 lexpr)))
	 (bindings (xt-let-bindings let-bindings))
	 (bformals (xt-let-formals let-bindings))
	 (args (apply #'xt-let-arguments (term-args let-bindings))))
    (xt-let-expr* expr bindings bformals args lexpr t)))

(defun xt-let-expr* (expr bindings bformals args &optional lexpr first?)
  (if (null bindings)
      expr
      (make-instance (if first? 'let-expr 'chained-let-expr)
	:operator (make-instance 'lambda-expr
		    :bindings (if (listp (car bindings))
				  (car bindings)
				  (list (car bindings)))
		    :expression (xt-let-expr*
				 expr (cdr bindings) (cdr bformals) (cdr args)
				 lexpr)
		    :place (term-place lexpr))
	:argument (xt-let-expr-argument (reverse (car bformals)) (car args))
	:place (term-place lexpr))))

(defun xt-let-expr-argument (formals arg)
  (if (null formals)
      arg
      (xt-let-expr-argument
       (cdr formals)
       (make-instance 'let-lambda-expr
	 :bindings (car formals)
	 :expression arg))))

(defun xt-where-expr (lexpr)
  (let* ((let-bindings (term-arg0 lexpr))
	 (expr (xt-expr (term-arg1 lexpr)))
	 (bindings (xt-let-bindings let-bindings))
	 (bformals (xt-let-formals let-bindings))
	 (args (apply #'xt-let-arguments (term-args let-bindings))))
    (xt-where-expr* expr bindings bformals args lexpr)))

(defun xt-where-expr* (expr bindings bformals args &optional lexpr)
  (if (null bindings)
      expr
      (make-instance (if lexpr 'where-expr 'chained-where-expr)
	:operator (make-instance 'lambda-expr
		    :bindings (if (listp (car bindings))
				  (car bindings)
				  (list (car bindings)))
		    :expression (xt-where-expr*
				 expr (cdr bindings) (cdr bformals) (cdr args))
		    :place (term-place lexpr))
	:argument (xt-let-expr-argument (reverse (car bformals)) (car args))
	:place (term-place lexpr))))

(defun xt-let-bindings (let-bindings)
  (mapcar #'xt-let-bind (term-args let-bindings)))

(defun xt-let-bind (lbind)
  (let ((let-bind (term-arg0 lbind)))
    (if (is-sop 'SIMPLEBIND let-bind)
	(xt-simplebind let-bind)
	(mapcar #'xt-simplebind (term-args let-bind)))))

(defun xt-simplebind (bind)
  (let ((formals (xt-pdf (term-arg1 bind))))
    (if (or (is-sop 'NOTYPE (term-arg2 bind))
	    (some #'(lambda (fmls)
		      (some #'(lambda (fml)
				(not (declared-type fml)))
			    fmls))
		  formals))
	(make-instance 'untyped-bind-decl
	  :id (xt-idop (term-arg0 bind))
	  :place (term-place bind))
	(make-instance 'arg-bind-decl
	  :id (xt-idop (term-arg0 bind))
	  :declared-type (if formals
			     (xt-simplebind-funtype
			      (reverse formals)
			      (xt-type-expr (term-arg2 bind)))
			     (xt-type-expr (term-arg2 bind)))
	  :place (term-place bind)))))

(defun xt-simplebind-funtype (formals range)
  (if (null formals)
      range
      (xt-simplebind-funtype
       (cdr formals)
       (let* ((dom (maplist #'(lambda (x)
				(xt-simplebind-domain-type x range))
			    (car formals)))
	      (tvar (make-new-variable '|t| (cons range dom)))
	      (domain (xt-funtype-domain dom range tvar)))
	 (make-instance 'funtype
	   :domain domain
	   :range (xt-subst-new-domain-dep domain range))))))

(defun xt-simplebind-domain-type (fmls range)
  (if (or (id-occurs-in (id (car fmls)) (cdr fmls))
	  (id-occurs-in (id (car fmls)) range))
      (mk-dep-binding (id (car fmls)) (declared-type (car fmls)))
      (declared-type (car fmls))))

(defun xt-let-formals (let-bindings)
  (mapcar #'xt-let-formals* (term-args let-bindings)))

(defun xt-let-formals* (lbind)
  (let ((let-bind (term-arg0 lbind)))
    (if (is-sop 'SIMPLEBIND let-bind)
	(xt-let-formals** let-bind)
	(progn (mapc #'xt-check-let-formals (term-args let-bind))
	       nil))))

(defun xt-let-formals** (bind)
  (xt-pdf (term-arg1 bind)))

(defun xt-check-let-formals (bind)
  (when (xt-pdf (term-arg1 bind))
    (parse-error (term-arg1 bind) "May not include parameters here")))

(defun xt-let-arguments (&rest let-binds)
  (mapcar #'(lambda (b) (xt-expr (term-arg1 b))) let-binds))

(defun xt-update-expr (uexpr)
  (let ((expr (term-arg0 uexpr))
	(assignments (term-arg1 uexpr)))
    (make-instance 'update-expr
      :expression (xt-expr expr)
      :assignments (mapcar #'xt-assignment (term-args assignments))
      :place (term-place uexpr))))

(defun xt-override-expr (oexpr)
  (make-instance 'override-expr
    :left (xt-expr (term-arg0 oexpr))
    :right (xt-expr (term-arg1 oexpr))
    :place (term-place oexpr)))

(defun xt-cases-expr (cexpr)
  (let ((expr (term-arg0 cexpr))
	(selections (xt-selections (term-arg1 cexpr)))
	(else (term-arg2 cexpr)))
    (if (some #'in-selection? selections)
	(if (every #'in-selection? selections)
	    (make-instance 'unpack-expr
	      :expression (xt-expr expr)
	      :selections selections
	      :else-part (unless (is-sop 'EXPR-NULL-1 else)
			   (xt-expr else))
	      :place (term-place cexpr))
	    (parse-error cexpr
	      "Can't mix constructors and IN selections"))
	(make-instance 'cases-expr
	  :expression (xt-expr expr)
	  :selections selections
	  :else-part (unless (is-sop 'EXPR-NULL-1 else)
		       (xt-expr else))
	  :place (term-place cexpr)))))

(defun xt-selections (sels)
  (mapcar #'xt-selection (term-args sels)))

(defun xt-selection (sel)
  (let* ((selector (term-arg0 sel))
	 (args (term-arg1 sel))
	 (expr (term-arg2 sel))
	 (id (xt-idop (xt-pidop selector))))
    (multiple-value-bind (index kind)
	(projection? id)
      (let ((constr (case kind
		      (in (make-instance 'injection-expr
			       :id (intern (string-upcase id) :pvs)
			       :index index
			       :place (term-place expr)))
		      (proj (parse-error sel "Projection illegal here"))
		      (t (mk-name-expr id)))))
	(setf (place constr) (term-place selector))
	(if (eq kind 'in)
	    (make-instance 'in-selection
	      :constructor constr
	      :args (if (is-sop 'SELECTION-NULL-1 args)
			(parse-error constr "Must provide an argument")
			(let* ((*allowed-ids* '(_)))
			  (xt-typed-ids args)))
	      :index index
	      :expression (xt-expr expr)
	      :place (term-place sel))
	    (make-instance 'selection
	      :constructor constr
	      :args (unless (is-sop 'SELECTION-NULL-1 args)
		      (let* ((*allowed-ids* '(_)))
			(xt-typed-ids args)))
	      :expression (xt-expr expr)
	      :place (term-place sel)))))))

(defun xt-cond-expr (expr)
  (let* ((cases (term-args (term-arg0 expr)))
	 (else (unless (is-sop 'NOELSE (term-arg1 expr))
		 (xt-expr (term-arg1 expr))))
	 (cond-expr (xt-cond-expr* cases else nil)))
    (if (typep cond-expr 'last-cond-expr)
	(change-class cond-expr 'single-cond-expr)
	(change-class cond-expr 'first-cond-expr))
    (setf (place cond-expr) (term-place expr))
    cond-expr))

(defun xt-cond-expr* (cases else conditions)
  (if cases
      (let* ((condition (xt-expr (term-arg0 (car cases))))
	     (then-part (xt-expr (term-arg1 (car cases))))
	     (else-part (xt-cond-expr* (cdr cases) else
				       (cons condition conditions)))
	     (ifname (mk-name-expr 'IF)))
	(setf (place ifname) (term-place (car cases)))
	(if else-part
	    (make-instance 'cond-expr
	      :operator ifname
	      :argument (make-instance 'arg-tuple-expr
			  :exprs (list condition
				       then-part
				       else-part)
			  :place (place condition))
	      :place (term-place (car cases)))
	    (make-instance 'last-cond-expr
	      :operator ifname
	      :argument (make-instance 'arg-tuple-expr
			  :exprs (list condition
				       then-part
				       then-part)
			  :place (place condition))
	      :place (term-place (car cases)))))
      (when else
	(make-instance 'last-cond-expr
	  :operator (mk-name-expr 'IF)
	  :argument (make-instance 'arg-tuple-expr
		      :exprs (list (mk-else-condition nil conditions else)
				   else
				   else)
		      :place (place else))))))

(defun xt-table-expr (expr)
  (let ((row-expr (unless (is-sop 'NOROWVAR (term-arg0 expr))
		    (xt-expr (term-arg0 expr))))
	(col-expr (unless (is-sop 'NOCOLVAR (term-arg1 expr))
		    (xt-expr (term-arg1 expr))))
	(col-headings (unless (is-sop 'NOCOLHEADING (term-arg2 expr))
		       (xt-col-headings (term-arg2 expr)))))
    (multiple-value-bind (row-headings rows)
	(xt-table-entries* (term-args (term-arg3 expr)) col-headings)
      (unless (or (null col-expr)
		  col-headings)
	(parse-error (term-arg1 expr)
	  "Column headings must be given if the column expression is"))
      (unless (or (null row-expr)
		  row-headings)
	(parse-error (term-arg0 expr) "Row headings must be given"))
      (make-instance 'table-expr
	:row-expr row-expr
	:col-expr col-expr
	:row-headings row-headings
	:col-headings col-headings
	:table-entries rows
	:place (term-place expr)))))

(defun xt-col-headings (col-heading)
  (let ((expr (xt-expr (term-arg0 col-heading)))
	(exprs (mapcar #'xt-table-entry* (term-args (term-arg1 col-heading)))))
    (when (some #'(lambda (ch) (eq ch 'else)) (butlast exprs))
      (let ((term (find-if #'(lambda (te)
			       (is-sop 'ELSE te))
		    (term-args (term-arg1 col-heading)))))
	(parse-error term "ELSE not allowed here")))
    (cons expr exprs)))

(defun xt-table-entries (table-entries)
  (xt-table-entries* (term-args table-entries) nil))

(defun xt-table-entries* (table-entries col-headings)
  (let* ((row-headings nil)
	 (row-entries (xt-row-entries table-entries))
	 (row-length (length (car row-entries)))
	 (col-heading-length (when col-headings (length col-headings))))
    ;; Can't have more than one row without having row-headings
    (when (and col-headings
	       (= col-heading-length row-length)
	       (cdr row-entries))
      (parse-error (car table-entries) "Row has the wrong length"))
    (unless (and col-headings
		 (= (length col-headings) (length (car row-entries))))
      (setq row-headings (mapcar #'car row-entries))
      (setq row-entries (mapcar #'cdr row-entries)))
    (when row-headings
      (xt-check-row-headings row-headings table-entries))
    (xt-check-table-entries row-entries table-entries)
    (values row-headings row-entries)))

(defun xt-check-row-headings (headings entries)
  (when (some #'null headings)
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'NOTABLEENTRY te))
		  entries)))
      (parse-error term "Empty entry not allowed in row headings")))
  (when (some #'(lambda (ch) (eq ch 'else)) (butlast headings))
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'ELSE te))
		  entries)))
      (parse-error term "ELSE not allowed here"))))

(defun xt-check-table-entries (row-entries table-entries)
  (mapc #'xt-check-table-row row-entries table-entries))

(defun xt-check-table-row (row entries)
  (when (some #'(lambda (ch) (eq ch 'else)) row)
    (let ((term (find-if #'(lambda (te)
			     (is-sop 'ELSE te))
		  (term-args entries))))
      (parse-error term "ELSE not allowed here"))))
  
(defun xt-table-entry* (table-entry)
  (case (sim-term-op table-entry)
    (NOTABLEENTRY nil)
    (ELSE 'else)
    (t (xt-expr table-entry))))

(defun xt-table-entry (table-entry)
  (mapcar #'xt-table-entry* (term-args table-entry)))

(defun xt-row-entries (row-entries &optional length rows)
  (if (null row-entries)
      (nreverse rows)
      (let* ((row-entry (mapcar #'xt-table-entry*
				(term-args (car row-entries))))
	     (row-length (length row-entry)))
	(unless (or (null length)
		    (= length row-length))
	  (parse-error (car row-entries)
	    "Length of this row is different from those above"))
	(xt-row-entries (cdr row-entries)
			row-length
			(cons row-entry rows)))))

(defun xt-assignment (ass)
  (let ((assign (term-arg0 ass))
	(expr (term-arg1 ass))
	(sep (ds-id (term-arg2 ass))))
    (when (cdr (term-args assign))
      (let ((bad-arg (find-if #'(lambda (x)
				  (memq (sim-term-op x)
					'(ASSIGN-ID ASSIGN-NUM)))
		       (term-args assign))))
	(when bad-arg
	  (parse-error bad-arg "( or ` expected here"))))
    (if (and (null (cdr (term-args assign)))
	     (memq (sim-term-op (term-arg0 assign))
		   '(ASSIGN-ID ASSIGN-SKONAME ASSIGN-NUM)))
	(if (eq sep 'CEQ)
	    (make-instance 'uni-assignment
	      :arguments (xt-assign assign)
	      :expression (xt-expr expr)
	      :place (term-place ass))
	    (make-instance 'uni-maplet
	      :arguments (xt-assign assign)
	      :expression (xt-expr expr)
	      :place (term-place ass)))
	(if (eq sep 'CEQ)
	    (make-instance 'assignment
	      :arguments (xt-assign assign)
	      :expression (xt-expr expr)
	      :place (term-place ass))
	    (make-instance 'maplet
	      :arguments (xt-assign assign)
	      :expression (xt-expr expr)
	      :place (term-place ass))))))

(defun xt-assign (ass-args)
  (mapcar #'xt-assign* (term-args ass-args)))

(defun xt-assign* (ass-arg)
  (case (sim-term-op ass-arg)
    (ID-ASSIGN (let ((id (ds-id (term-arg0 ass-arg))))
		 (if (every #'digit-char-p (string id))
		     (list (make-instance 'proj-assign
			 :number (parse-integer (string id))
			 :place (term-place ass-arg)))
		     (list (make-instance 'id-assign
			     :id id
			     :place (term-place ass-arg))))))
    (PROJ-ASSIGN (list (make-instance 'proj-assign
			 :number (ds-number (term-arg0 ass-arg))
			 :place (term-place ass-arg))))
    (ASSIGN-ID (let ((id (ds-id (term-arg0 ass-arg))))
		 (if (every #'digit-char-p (string id))
		     (list (make-instance 'number-expr
			     :number (parse-integer (string id))
			     :place (term-place ass-arg)))
		     (list (make-instance 'name-expr
			     :id id
			     :place (term-place ass-arg))))))
    (ASSIGN-SKONAME (list (make-instance 'name-expr
			    :id (makesym "~a!~d"
					 (ds-id (term-arg0 ass-arg))
					 (ds-number (term-arg1 ass-arg)))
			    :place (term-place ass-arg))))
    (ASSIGN-NUM (list (make-instance 'number-expr
			:number (ds-number (term-arg0 ass-arg))
			:place (term-place ass-arg))))
    (ASSIGN-TUPLE (mapcar #'xt-expr (term-args ass-arg)))))

(defun xt-assign-application (ass &optional args)
  (case (sim-term-op ass)
    (APPLICATION (xt-assign-application
		  (term-arg0 ass)
		  (cons (mapcar #'xt-expr (term-args (term-arg1 ass)))
			args)))
    (TUPLE-EXPR (cons (mapcar #'xt-expr (term-args ass)) args))
    (t (parse-error ass "( expected here"))))

(defmethod separator ((ass assignment))
  'ceq)

(defmethod separator ((ass maplet))
  'arr)


;;; Names

(defun xt-modname (modname)
  (let* ((id (term-arg0 modname))
	 (lib (term-arg1 modname))
	 (actuals (term-arg2 modname))
	 (mappings (term-arg3 modname))
	 (target (term-arg4 modname)))
    (multiple-value-bind (acts acts-there? dacts dacts-there?)
	(unless (is-sop 'NOACTUALS actuals)
	  (xt-actuals actuals t))
      (declare (ignore dacts))
      (when dacts-there?
	(parse-error (term-place modname)
	  "Declaration actuals not allowed for theory names"))
      (make-instance (if (is-sop 'NOACTUALS actuals)
			 'modname 'full-modname)
	:id (ds-vid id)
	:library (unless (is-sop 'NOLIB lib)
		   (ds-vid lib))
	:actuals acts
	:acts-there? acts-there?
	:mappings (unless (is-sop 'NOMAP mappings)
		    (xt-mappings mappings))
	:target (unless (is-sop 'NOTGT target)
		  (xt-modname target))
	:place (term-place modname)))))

;;; name! means create a name no matter what; otherwise numbers create number-exprs
;;; lib@idop[actuals]{{mappings}}:->target.pidop[dactuals]
(defun xt-name (name &optional name!)
  (let* ((idop (term-arg0 name))
	 (lib (term-arg1 name))
	 (actuals (term-arg2 name))
	 (pidop (case (sim-term-op (term-arg3 name))
		  (NOMOD nil)
		  (PIDACTS (term-arg0 (term-arg3 name)))
		  (t (term-arg3 name))))
	 (dactuals (when (is-sop 'PIDACTS (term-arg3 name))
		     (term-arg1 (term-arg3 name))))
	 (mappings (term-arg4 name))
	 (target (term-arg5 name))
	 (maybe-num? (and (not name!)
			  (is-sop 'NOLIB lib)
			  (is-sop 'NOACTUALS actuals)
			  (is-sop 'NOMAP mappings)
			  (is-sop 'NOTGT target)
			  (or (is-number (term-arg0 idop))
			      (valid-number? (string (ds-id (term-arg0 idop))))))))
    (multiple-value-bind (idops length)
	(xt-name-idops pidop maybe-num?)
      ;; At this point, idops is a number or a symbol (possibly with
      ;; periods), and length is the length of it.  If a number, then we
      ;; know maybe-num? is true, and we can create a rational.  The nil
      ;; symbol will return a length of either nil or 3, depending on whether
      ;; idops is actually empty.
      (assert (or (symbolp idops) (integerp idops)))
      (cond ((integerp idops)
	     (let* ((int-part (if (is-number (term-arg0 idop))
				  (ds-number (term-arg0 idop))
				  (parse-integer (string (ds-id (term-arg0 idop))))))
		    (frac-value idops)
		    (frac-length length))
	       (if (zerop frac-value)
		   (make-instance 'decimal-integer
		     :number int-part
		     :fractional-length frac-length
		     :place (term-place name))
		   (let* ((denom (expt 10 frac-length))
			  (num (+ (* denom int-part) frac-value)))
		     (make-instance 'decimal
		       :operator (mk-name-expr '/)
		       :argument (mk-arg-tuple-expr
				  (make-instance 'number-expr
				    :number num)
				  (make-instance 'number-expr
				    :number denom))
		       :place (term-place name))))))
	    ((and maybe-num? (null length))
	     (multiple-value-bind (num radix)
		 (if (is-number (term-arg0 idop))
		     (ds-number (term-arg0 idop))
		     (parse-number (string (ds-id (term-arg0 idop)))))
	       (assert (integerp num))
	       (if radix
		   (make-instance 'number-expr-with-radix
		     :number num
		     :radix radix
		     :place (term-place name))
		   (make-instance 'number-expr
		     :number num
		     :place (term-place name)))))
	    (t (multiple-value-bind (acts acts-there? dacts dacts-there?)
		   (unless (is-sop 'NOACTUALS actuals)
		     (xt-actuals actuals pidop))
		 (make-instance 'name
		   :id (if (null length)
			   (if (is-number (term-arg0 idop))
			       (makesym "~d" (ds-number (term-arg0 idop)))
			       (let ((id (ds-vid (term-arg0 idop))))
				 (when (memq id '(|/\\| |\\/|))
				   (pushnew id *escaped-operators-used*))
				 id))
			   idops)
		   :library (unless (is-sop 'NOLIB lib)
			      (ds-vid lib))
		   :actuals acts
		   :acts-there? acts-there?
		   :dactuals (or dacts
				 (when dactuals
				   (xt-actuals dactuals t)))
		   :dacts-there? dacts-there?
		   :mappings (unless (is-sop 'NOMAP mappings)
			       (xt-mappings mappings))
		   :mod-id (unless (null length)
			     (if (is-number (term-arg0 idop))
				 (makesym "~d" (ds-number (term-arg0 idop)))
				 (ds-id (term-arg0 idop))))
		   :target (unless (is-sop 'NOTGT target)
			     (xt-modname target))
		   :place (term-place name))))))))

(defun valid-number? (str)
  (or (every #'digit-char-p str)
      (and (> (length str) 2)
	   (char= (char str 0) #\0)
	   (let ((radix (case (char str 1)
			  ((#\x #\X) 16)
			  ((#\o #\O) 8)
			  ((#\b #\B) 2))))
	     (and radix
		  (every #'(lambda (ch) (digit-char-p ch radix))
			 (subseq str 2)))))))

(defun parse-number (str)
  (if (every #'digit-char-p str)
      (values (parse-integer str))
      (if (and (> (length str) 2)
	       (char= (char str 0) #\0))
	  (let ((radix (case (char str 1)
			 ((#\x #\X) 16)
			 ((#\o #\O) 8)
			 ((#\b #\B) 2))))
	    (if radix
		(values (parse-integer (subseq str 2) :radix radix) radix)
		(error "~a is not a valid number" str)))
	  (error "~a is not a valid number" str))))

(defun xt-name-idops (term maybe-num?)
  (when term
    (if (and maybe-num?
	     (null (cdr (term-args term)))
	     (or (is-number (term-arg0 (term-arg0 term)))
		 (every #'digit-char-p
			(string (ds-id (term-arg0 (term-arg0 term)))))))
	(if (is-number (term-arg0 (term-arg0 term)))
	    (values (ds-number (term-arg0 (term-arg0 term)))
		    (length (format nil "~d"
			      (ds-number (term-arg0 (term-arg0 term))))))
	    (values (parse-integer (string (ds-id (term-arg0 (term-arg0 term)))))
		    (length (string (ds-id (term-arg0 (term-arg0 term)))))))
	(let ((sym (makesym "~{~a~^.~}"
			    (mapcar #'(lambda (tm)
					(if (is-number tm)
					    (makesym (ds-number (term-arg0 tm)))
					    (ds-id (term-arg0 tm))))
			      (term-args term)))))
	  (values sym (length (string sym)))))))

(defun xt-actuals (actuals single?)
  (if (eq (sim-term-op actuals) 'ACTUALS)
      (let* ((acts (mapcar #'xt-actual (term-args actuals)))
	     (there (null acts)))
	(values acts there))
      (if single?
	  (parse-error (term-arg1 actuals)
	    "Cannot have two sets of actuals here")
	  ;; Possibilities: f[], f[][], f[a], f[a][], f[][b], f[a][b]
	  (let* ((acts1 (mapcar #'xt-actual (term-args (term-arg0 actuals))))
		 (acts2 (mapcar #'xt-actual (term-args (term-arg1 actuals))))
		 (there1 (and (null acts1)
			      (eq (sim-term-op (term-arg0 actuals)) 'ACTUALS)))
		 (there2 (and (null acts2)
			      (eq (sim-term-op (term-arg1 actuals)) 'ACTUALS))))
	    (values acts1 there1 acts2 there2)))))

(defun xt-actual (act)
  (if (eq (sim-term-op act) 'MAPPINGS)
      (parse-error act "Mappings must be preceded by a theory name")
      (make-instance 'actual
	:expr (if (member (sim-term-op act)
			  '(SUBTYPE EXPR-AS-TYPE ENUM-OR-SUBTYPE
				    FUNTYPE ;; PREDTYPE
				    RECORDTYPE))
		  (xt-not-enum-type-expr act)
		  (xt-expr act))
	:place (term-place act))))

(defun xt-mappings (mappings)
  (mapcar #'xt-mapping (term-args mappings)))

(defun xt-mapping (mapping)
  (let* ((lhs (term-arg0 mapping))
	 (rhs (term-arg1 mapping))
	 (kind (unless (is-sop 'NOQUAL (term-arg3 lhs))
		 (case (sim-term-op (term-arg3 lhs))
		   (TYPE 'type)
		   (THEORY 'theory)
		   (t 'expr))))
	 (dtype (when (is-sop 'TYPED (term-arg3 lhs))
		  (xt-not-enum-type-expr (term-arg0 (term-arg3 lhs)))))
	 (expr (cond ((is-sop 'MAPPING-RENAME mapping)
		      ;; Only allow idops or numbers at this point
		      (let ((ex (xt-expr rhs)))
			(cond ((name-expr? ex)
			       (if (or (library ex)
				       (actuals ex)
				       (mappings ex)
				       (mod-id ex))
				   (parse-error rhs
				     "RHS must be an id or number")
				   ex))
			       ((number-expr? ex)
				(make-instance 'name-expr
				  :id (number ex)
				  :place (place ex)))
			       (t (parse-error rhs
				    "RHS must be an id or number")))))
		     ((memq (sim-term-op rhs)
			    '(SUBTYPE EXPR-AS-TYPE ENUM-OR-SUBTYPE
				      FUNTYPE RECORDTYPE))
		      (xt-not-enum-type-expr rhs))
		     (t (xt-expr rhs)))))
    (assert (place expr))
    (if (is-sop 'NOFORMALS (term-arg2 lhs))
	(case (sim-term-op mapping)
	  (MAPPING-SUBST
	   (make-instance 'mapping-subst
	     :lhs (xt-mapping-lhs (term-arg0 mapping))
	     :rhs (make-instance 'mapping-rhs :expr expr :place (place expr))
	     :kind kind
	     :declared-type dtype))
	  (MAPPING-RENAME
	   (make-instance 'mapping-rename
	     :lhs (xt-mapping-lhs (term-arg0 mapping))
	     :rhs (make-instance 'mapping-rhs-rename
		    :expr expr :place (place expr))
	     :kind kind
	     :declared-type dtype)))
	(let ((formals (xt-pdf (term-arg2 lhs))))
	  (case (sim-term-op mapping)
	  (MAPPING-SUBST
	   (make-instance 'mapping-subst-with-formals
	     :lhs (xt-mapping-lhs (term-arg0 mapping))
	     :rhs (make-instance 'mapping-rhs
		    :expr (mk-lambda-exprs formals expr))
	     :formals formals
	     :kind kind
	     :declared-type dtype))
	  (MAPPING-RENAME
	   (make-instance 'mapping-rename-with-formals
	     :lhs (xt-mapping-lhs (term-arg0 mapping))
	     :rhs (make-instance 'mapping-rhs-rename
		    :expr (mk-lambda-exprs formals expr))
	     :formals formals
	     :kind kind
	     :declared-type dtype)))))))

(defun xt-mapping-lhs (lhs)
  (let ((decl-formals (xt-theory-formals (term-arg1 lhs))))
    (mapc #'change-to-decl-formal decl-formals)
    (if decl-formals
	(make-instance 'mapping-lhs
	  :id (xt-lhs-idops (term-arg0 lhs))
	  :decl-formals decl-formals
	  :place (term-place lhs))
	(make-instance 'name
	  :id (xt-lhs-idops (term-arg0 lhs))
	  :place (term-place lhs)))))

(defun xt-lhs-idops (idops)
  (if (cdr (term-args idops))
      (makesym "~{~a~^.~}"
	       (mapcar #'(lambda (idop) (ds-id (term-arg0 idop)))
		 (term-args idops)))
      (xt-idop (term-arg0 idops))))

(defun xt-unique-name (name)
  (let ((uname (xt-name (term-arg0 name) t))
	(qual (term-arg1 name)))
    (case (sim-term-op qual)
      (TYPED (change-class uname 'unique-typed-name)
	     (setf (declared-type uname) (xt-type-expr (term-arg0 qual))))
      (FORMULA (change-class uname 'unique-formula-name)
	       (setf (spelling uname) (sim-term-op (term-arg0 qual)))))
    uname))

(defun xt-idop (idop)
  (ds-vid (term-arg0 idop)))

(defun xt-bname (bname)
  (let ((name (xt-name (term-arg0 bname) nil))
	(number (ds-number (term-arg1 bname))))
    (when (or (mod-id name)
	      (library name)
	      (actuals name))
      (parse-error name "~a!~d is not a valid name" name number))
    (make-instance 'name
      :id (makesym "~a!~d" (id name) number))))

(defun ds-vid (term)
  (let* ((tid (ds-id term))
	 (id (if (integerp tid)
		 (makesym "~d" tid)
		 tid)))
    (when (memq id '(|/\\| |\\/|))
      (pushnew id *escaped-operators-used*))
    (if (or (numberp id)
	    (memq id *allowed-ids*)
	    (valid-pvs-id id))
	id
	(parse-error term "Invalid id"))))

(defun valid-theory-id (symbol)
  (every #'(lambda (ch)
	     (or (unialpha-char-p ch)
		 (digit-char-p ch)
		 ;; Note that periods are allowed in identifiers
		 ;; in general, but not in declarations - see
		 ;; xt-check-periods
		 (member ch '(#\_ #\?) :test #'char=)))
	 (string symbol)))

(defun valid-pvs-id (symbol)
  (or (not *valid-id-check*)
      *in-checker*
      (or (assq symbol *pvs-operators*)
	  (valid-pvs-id* (string symbol)))))

(defun valid-pvs-id* (sym &optional (start 0))
  (let* ((idstr (string sym))
	 (dpos (position #\. idstr :start start)))
    (or (and (valid-pvs-id** idstr start dpos)
	     (or (null dpos)
		 (valid-pvs-id* sym (1+ dpos))))
	;;(break "not valid?")
	)))

(defun unialpha-char-p (char)
  (or (alpha-char-p char)
      (> (char-code char) 127)))
  
(defun valid-pvs-id** (idstr start end)
  (and (plusp (length idstr))
       (or (and (unialpha-char-p (char idstr start))
		(every #'(lambda (ch)
			   (or (unialpha-char-p ch)
			       (digit-char-p ch)
			       (and *in-checker*
				    (char= ch #\!))
			       ;; Note that periods are allowed in identifiers
			       ;; in general, but not in declarations - see
			       ;; xt-check-periods
			       (member ch '(#\_ #\?) :test #'char=)))
		       (subseq idstr (1+ start) end)))
	   (and (null end)
		(or (assq (intern (subseq idstr start) :pvs) *pvs-operators*)
		    (if (and (> (length idstr) (1+ start))
			     (char= (char idstr start) #\0)
			     (memq (char idstr (1+ start))
				   '(#\x #\X #\o #\O #\b #\B)))
			(let ((radix (case (char idstr (1+ start))
				       ((#\x #\X) 16)
				       ((#\o #\O) 8)
				       ((#\b #\B) 2))))
			  (every #'(lambda (ch) (digit-char-p ch radix))
				 (subseq idstr (+ start 2))))
			(every #'digit-char-p (subseq idstr start))))))))
