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

;;; Makes extensive use of the following functions from ERGO:
;;;	sim-term-op - returns the symbol which is the operator of a term
;;;	is-sop	    - checks whether a given symbol is the operator of a term
;;;	term-args   - returns the list of arguments of a term
;;;	ds-id       - returns the symbol of an id term

(defun term-place (absyn)
  (getf (term:term-attr absyn) :place))

(defsetf term-place (absyn) (place)
  `(setf (getf (term:term-attr ,absyn) :place) ,place))


;;; Parsing

(defun parse (&rest keys)
  (let* ((*current-file* (or (cadr (member :file keys))
			     *current-file*))
	 (start-time (when *current-file* (get-internal-real-time))))
    (init-parser)
    (multiple-value-bind (term error? place msg args)
	(apply #'pvs-parse :return-errors t keys)
      (when error?
	(parse-error place (parse-error-msg msg args)))
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
		    0))))))

(defun init-parser ()
  (setq sbrt:*last-syntax* nil)
  (setq sbrt:*last-newline-comment* nil)
  (setq sbrt:*num-keywords-skipped* -1)
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
                     ~@[~%  Note: '~a' is now a keyword~]~
                     ~@[~%  Note: '~a' is only allowed in theory declarations (not importings)~]"
	  found expected
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
    aorm))

(defun xt-datatypes (class datatype &optional id inline)
  (let* ((subtypes-term (term-arg0 datatype))
	 (subtypes (mapcar #'(lambda (st)
			       (make-instance 'type-name
				 :id (ds-vid st)
				 :place (term-place st)))
			   (term-args subtypes-term)))
	 (assuming-term (term-arg1 datatype))
	 (assuming (unless (is-sop 'DATATYPE-NULL-2 assuming-term)
		     (xt-assumings assuming-term)))
	 (importing-term (term-arg2 datatype))
	 (importings (unless (is-sop 'DATATYPE-NULL-1 importing-term)
		  (xt-importing-elt importing-term)))
	 (adtcases-term (term-arg3 datatype))
	 (adtcases (xt-adtcases adtcases-term))
	 (endid (ds-vid (term-arg4 datatype))))
    (check-subtypes-constructors-consistency id subtypes adtcases)
    (unless (or (null id)
		(eq id endid))
      (parse-error (term-arg4 datatype)
	"End id ~a does not match datatype id ~a" endid id))
    (if (or (null id) inline)
	(make-instance (if (eq class 'DATATYPES)
			   'inline-datatype-with-subtypes
			   'inline-codatatype-with-subtypes)
	  :subtypes subtypes
	  :assuming assuming
	  :importings importings
	  :constructors adtcases
	  :place (term-place datatype))
	(make-instance (if (eq class 'DATATYPES)
			   'datatype-with-subtypes
			   'codatatype-with-subtypes)
	  :subtypes subtypes
	  :assuming assuming
	  :importings importings
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

(defun xt-theory-formals (theory-formals)
  (mapcan #'xt-theory-formal (term-args theory-formals)))

(defun xt-theory-formal (theory-formal)
  (let* ((importing (term-arg0 theory-formal))
	 (idops (xt-check-periods (term-arg1 theory-formal)))
	 (decl-body (term-arg2 theory-formal)))
    (multiple-value-bind (decl dtype)
	(xt-declaration-body decl-body)
      (append (unless (is-sop 'THEORY-FORMAL-NULL-1 importing)
		(xt-formal-importing-elt importing))
	      (xt-chained-decls (term-args idops) dtype nil decl
				theory-formal)))))

(defun xt-datatype (class datatype &optional id inline)
  (let ((assuming (term-arg0 datatype))
	(importing (term-arg1 datatype))
	(adtcases (term-arg2 datatype))
	(endid (ds-vid (term-arg3 datatype))))
    (unless (or (null id)
		(eq id endid))
      (parse-error (term-arg3 datatype)
	"End id ~a does not match datatype id ~a" endid id))
    (make-instance (if (or (null id) inline)
		       (if (eq class 'DATATYPE)
			   'inline-datatype
			   'inline-codatatype)
		       (if (eq class 'DATATYPE)
			   'datatype
			   'codatatype))
      :assuming (unless (is-sop 'DATATYPE-NULL-2 assuming)
		  (xt-assumings assuming))
      :importings (unless (is-sop 'DATATYPE-NULL-1 importing)
		   (xt-importing-elt importing))
      :constructors (xt-adtcases adtcases)
      :place (term-place datatype))))

(defun xt-adtcases (adtcases)
  (mapcar #'xt-adtcase (term-args adtcases)))

(defun xt-adtcase (adtcase)
  (let* ((constructor (term-arg0 adtcase))
	 (recognizer (term-arg1 adtcase))
	 (constr (xt-constructor constructor))
	 (subtype (when (is-sop 'ADTCASE-SUBTYPE adtcase)
		    (make-instance 'type-name
		      :id (ds-id (term-arg2 adtcase))
		      :place (term-place (term-arg2 adtcase))))))
    (when subtype
      (change-class constr 'constructor-with-subtype)
      (setf (subtype constr) subtype))
    (setf (recognizer constr) (xt-idop recognizer))
    (setf (place constr) (term-place constructor))
    constr))

(defun xt-constructor (constructor)
  (let ((idop (term-arg0 constructor))
	(adtdecls (term-arg1 constructor)))
    (make-instance 'simple-constructor
      :id (xt-idop idop)
      :arguments (unless (is-sop 'CONSTRUCTOR-NULL-1 adtdecls)
		   (xt-adtdecls adtdecls))
      :place (term-place constructor))))

(defun xt-adtdecls (decls)
  (mapcan #'xt-adtdecl (term-args decls)))

(defun xt-adtdecl (adtdecl)
  (let* ((idops (term-arg0 adtdecl))
	 (type (term-arg1 adtdecl))
	 (dtype (xt-not-enum-type-expr type)))
    (xt-chained-decls (term-args idops)
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
;    (assert (every #'place (modules exp)))
;    (assert (every #'place assum))
;    (assert (every #'place tpart))
    (make-instance 'module
      :exporting exp
      :assuming assum
      :theory tpart
      :place (term-place theory))))

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
  (let* ((idops (xt-check-periods (term-arg0 assuming)))
	 (formals (unless (is-sop 'NOFORMALS (term-arg1 assuming))
		    (xt-pdf (term-arg1 assuming))))
	 (decl (term-arg2 assuming))
	 (semi (term-arg3 assuming))
	 (@decl (term-arg4 assuming)))
    (when (and (cdr (term-args idops)) formals)
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
				     tdecl
				     formals
				     pdecl
				     assuming)))
	(setf (semi pdecl)
	      (if (is-sop 'SEMIC semi)
		  (if (is-sop 'ATDECL @decl)
		      'both t)
		  (if (is-sop 'ATDECL @decl)
		      '@decl nil)))
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
  (let* ((idops (xt-check-periods (term-arg0 tdecl)))
	 (formals (term-arg1 tdecl))
	 (pformals (unless (is-sop 'NOFORMALS formals)
		     (xt-pdf formals)))
	 (decl (term-arg2 tdecl))
	 (semi (term-arg3 tdecl))
	 (@decl (term-arg4 tdecl)))
    (when (and (cdr (term-args idops)) pformals)
      (parse-error formals ": expected here"))
    (case (sim-term-op decl)
      ((LIB-DECL THEORY-DECL TYPE-DECL DATATYPE)
       (let ((badid (find-if #'(lambda (tid)
				 (assq (ds-id (term-arg0 tid))
				       *pvs-operators*))
		      (term-args idops))))
	 (when badid
	   (parse-error badid "Id expected here")))))
    (cond ((is-sop 'DATATYPE decl)
	   (xt-theory-datatype idops formals decl semi))
	  ((is-sop 'DATATYPES decl)
	   (xt-theory-datatypes idops formals decl semi))
	  (t (multiple-value-bind (pdecl type-decl)
		 (xt-declaration-body decl idops)
	       (let ((decls (xt-chained-decls (term-args idops)
					      type-decl
					      pformals
					      pdecl
					      tdecl)))
		 #+pvsdebug ; Careful! nil is a valid PVS id
		 (assert (every #'(lambda (d)
				    (or (not (const-decl? d))
					(id d)))
				decls))
		 (assert (every #'place decls))
		 (setf (semi pdecl)
		       (if (is-sop 'SEMIC semi)
			   (if (is-sop 'ATDECL @decl)
			       'both t)
			   (if (is-sop 'ATDECL @decl)
			       '@decl nil)))
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
			 (if (cdr (term-args pidop)) ;; have periods
			     (if *xt-periods-allowed*
				 (xt-pidop pidop)
				 (parse-error pidops
				   "periods not allowed here"))
			     (if (is-sop 'PIDOP pidop)
				 (term-arg0 pidop)
				 pidop)))
		   (term-args pidops)))))
    (setf (term-place nterm) (term-place pidops))
    nterm))

(defun xt-theory-datatypes (idops formals decl semi)
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
		   (formals adt) (xt-theory-formals formals)
		   (semi adt) (when (is-sop 'SEMIC semi) t))
	     (list adt)))))

(defun xt-theory-datatype (idops formals decl semi)
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
		  (adt (funcall #'xt-datatype (sim-term-op decl)
				decl id t)))
	     (setf (id adt) id
		   (formals adt) (xt-theory-formals formals)
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
			      :id (ds-id (term-arg1 item))
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
  (let ((lib-decl (unless (is-sop 'NOLIB (term-arg0 importing))
		    (break)))
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
			     :place place)
			   (make-instance 'importing
			     :theory-name (xt-modname item)
			     :place place))))
	   (term-args (term-arg1 importing)))))
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

(defun xt-subtype-judgement* (jdecl dtype place)
  (cond ((is-sop 'JSUBTYPEDECL jdecl)
	 (let ((type (xt-not-enum-type-expr (term-arg0 jdecl))))
	   (make-instance 'subtype-judgement
	     :declared-subtype type
	     :declared-type dtype
	     :chain? t
	     :place place)))
	((is-sop 'JNAMEDECL jdecl)
	 (let ((name (xt-name (term-arg0 jdecl) nil)))
	   (make-instance 'subtype-judgement
	     :declared-subtype (change-class name 'type-name)
	     :declared-type dtype
	     :chain? t
	     :place place)))
	((is-sop 'JAPPLDECL jdecl)
	 (let ((name (xt-name (term-arg0 jdecl) nil))
	       (formals (xt-pdf (term-arg1 jdecl))))
	   (when (cdr formals)
	     (parse-error formals "Type applications may not be Curried"))
	   (make-instance 'subtype-judgement
	     :declared-subtype (make-instance 'type-application
				 :type (change-class name 'type-name)
				 :parameters (car formals)
				 :place (place name))
	     :declared-type dtype
	     :chain? t
	     :place place)))
	((is-number jdecl)
	 (parse-error jdecl "type expression expected here"))
	(t (make-instance 'subtype-judgement
	     :declared-subtype (xt-not-enum-type-expr jdecl)
	     :declared-type dtype
	     :chain? t
	     :place place))))

(defun xt-const-judgement (jdecls dtype place)
  (let ((decls (mapcar #'(lambda (jdecl)
			    (xt-const-judgement* jdecl dtype place))
			jdecls)))
    (setf (chain? (car (last decls))) nil)
    decls))

(defun xt-const-judgement* (jdecl dtype place)
  (case (sim-term-op jdecl)
    (JNAMEDECL
     (let ((ex (xt-name (term-arg0 jdecl) nil)))
       (if (number-expr? ex)
	   (make-instance 'number-judgement
	     :number-expr ex
	     :declared-type dtype
	     :chain? t
	     :place place)
	   (make-instance 'name-judgement
	     :name (change-class ex 'name-expr)
	     :declared-type dtype
	     :chain? t
	     :place place))))
    (JAPPLDECL
     (make-instance 'application-judgement
       :name (change-class (xt-name (term-arg0 jdecl) nil) 'name-expr)
       :formals (xt-pdf (term-arg1 jdecl))
       :declared-type dtype
       :chain? t
       :place place))
    (JEXPRDECL
     (let ((jd (xt-jexprdecl jdecl)))
       (setf (declared-type jd) dtype
	     (chain? jd) t
	     (place jd) place)
       jd))
    (t (parse-error jdecl "Types may not have HAS_TYPE judgements."))))

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
  (if (eq (sim-term-op jdecl) 'JAPPLDECL)
      (make-instance 'rec-application-judgement
	:name (change-class (xt-name (term-arg0 jdecl) nil) 'name-expr)
	:formals (xt-pdf (term-arg1 jdecl))
	:declared-type dtype
	:chain? t
	:place place)
      (parse-error jdecl "Recursive judgements are only for applications")))

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

(defun xt-chained-decls (idops dtype formals decl absyn &optional result)
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
		  (let ((splace (term-place (car idops)))
			(eplace (term-place absyn)))
		    (when (and splace eplace)
		      (vector (starting-row splace) (starting-col splace)
			      (ending-row eplace) (ending-col eplace)))))))
	(when formals (setf (formals ndecl) formals))
	(xt-chained-decls (cdr idops) dtype formals decl absyn
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
	    :id (xt-idop idop)
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
	      :offset (unless (is-sop 'NO-OFFSET offset)
			(xt-rational offset)))
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
  (let* ((pos? (is-sop 'POS (term-arg0 rat-term)))
	 (nbr (ds-number (term-arg1 rat-term)))
	 (num (if pos? nbr (- nbr))))
    (if (is-sop 'NODEN (term-arg2 rat-term))
	num
	(let* ((onbr (term-arg0 (term-arg2 rat-term)))
	       (onum (xt-idop onbr)))
	  (if (is-sop 'FLOAT (term-arg2 rat-term))
	      (if (zerop onum)
		  num
		  (let* ((len (length (format nil "~d" (ds-number (term-arg0 onbr)))))
			 (den (expt 10 len))
			 (nnum (+ (* num den) onum)))
		    ;; Note that this will not print correctly
		    (/ nnum den)))
	      (if (zerop onum)
		  (parse-error 0 "Cannot have a ratio with 0 denominator")
		  (/ num onum)))))))

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
    (make-instance 'formula-decl
      :spelling (sim-term-op fname)
      :definition (xt-expr expr)
      :place (term-place formula-decl))))


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
    (TYPE-UNITS (xt-type-units type-expr))
    (QUANT-TYPE (xt-quant-type type-expr))
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

(defun xt-type-units (type-expr)
  (xt-units-term (term-arg0 type-expr)))

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
	 (set-expr (mk-ergo-term* 'set-expr args expr))
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


;;; Expressions

(defun xt-expr (expr)
  (case (sim-term-op expr)
    (NUMBER-EXPR (xt-number-expr expr))
    (STRING-EXPR (xt-string-expr expr))
    (NAME-EXPR (xt-name-expr expr))
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
    (LET-EXPR (xt-let-expr expr))
    (WHERE-EXPR (xt-where-expr expr))
    (UPDATE-EXPR (xt-update-expr expr))
    ;;(override-expr (xt-override-expr expr))
    (CASES-EXPR (xt-cases-expr expr))
    (COND-EXPR (xt-cond-expr expr))
    (TABLE-EXPR (xt-table-expr expr))
    (SKOVAR (xt-skovar expr))
    (BRACK-EXPR (xt-brack-expr expr))
    (PAREN-VBAR-EXPR (xt-paren-vbar-expr expr))
    (BRACE-VBAR-EXPR (xt-brace-vbar-expr expr))
    (t (error "Unrecognized expr - ~a" expr))))

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
      (add-place (mk-name-expr '|null|)
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
	 (make-instance 'application
	   :operator (add-place (mk-name-expr '|cons|) cplace)
	   :argument (make-instance 'arg-tuple-expr
		       :exprs (list scar scdr)
		       :place aplace)
	   :place aplace))))

(defun xt-string-to-codes (string pos len place codes)
  (if (>= pos len)
      (nreverse codes)
      (multiple-value-bind (code npos)
	  (next-string-code string pos len place)
	(xt-string-to-codes string npos len place
			    (acons code
				   (vector (starting-row place)
					   (+ (starting-col place) pos)
					   (starting-row place)
					   (+ (starting-col place) npos))
				   codes)))))

(defun next-string-code (string pos len place)
  (let ((char (char string pos)))
    (if (or *tex-mode*
	    (char/= char #\\))
	(values (char-code char) (1+ pos))
	(if (>= (1+ pos) len)
	    (let ((cplace (vector (svref place 0) (+ (svref place 1) pos)
				  (svref place 0) (+ (svref place 1) pos))))
	      (parse-error cplace "String ends with a '\\'"))
	    (let ((echar (char string (1+ pos))))
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
		(#\\ (values (char-code #\\) (+ pos 2)))
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
		(t (if (digit-char-p echar)
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
				 "Decimal code must be less than 256"))))
		       (let ((cplace (vector (svref place 0)
					     (+ (svref place 1) pos 1)
					     (svref place 0)
					     (+ (svref place 1) pos 1))))
			 (parse-error cplace
			   "Illegal character after '\\'"))))))))))
		    

(defun xt-list-expr (expr)
  (let* ((place (term-place expr))
	 (last-place (when place
		       (vector (ending-row place) (ending-col place)
			       (ending-row place) (ending-col place))))
	 (lexpr (xt-list-expr* (term-args expr) last-place)))
    (setf (place lexpr) place)
    lexpr))

(defun xt-list-expr* (exprs last-place)
  (if exprs
      (let* ((ex (xt-expr (car exprs)))
	     (list (xt-list-expr* (cdr exprs) last-place)))
	(make-instance 'list-expr
	  :operator (make-instance 'name-expr
		      :id '|cons|
		      :place (place ex))
	  :argument (make-instance 'arg-tuple-expr
		      :exprs (list ex list)
		      :place (place ex))
	  :place (place ex)))
      (make-instance 'null-expr
	:id '|null|
	:place last-place)))

(defun xt-brack-expr (expr)
  (let ((args (term-args expr)))
    (if args
	(make-instance 'bracket-expr
	  :operator (make-instance 'name-expr
		      :id '[\|\|]
		      :place (term-place expr))
	  :argument (xt-arg-expr args)
	  :place (term-place expr))
	(make-instance 'name-expr
	  :id '[\|\|]
	  :place (term-place expr)))))

(defun xt-paren-vbar-expr (expr)
  (let ((args (term-args expr)))
    (if args
	(make-instance 'paren-vbar-expr
	  :operator (make-instance 'name-expr
		      :id '\(\|\|\)
		      :place (term-place expr))
	  :argument (xt-arg-expr args)
	  :place (term-place expr))
	(make-instance 'name-expr
	  :id '\(\|\|\)
	  :place (term-place expr)))))

(defun xt-brace-vbar-expr (expr)
  (let ((args (term-args expr)))
    (if args
	(make-instance 'brace-vbar-expr
	  :operator (make-instance 'name-expr
		      :id '{\|\|}
		      :place (term-place expr))
	  :argument (xt-arg-expr args)
	  :place (term-place expr))
	(make-instance 'name-expr
	  :id '{\|\|}
	  :place (term-place expr)))))

(defun xt-arg-expr (args)
  (if (cdr args)
      (let ((exprs (mapcar #'xt-expr args)))
	(make-instance 'arg-tuple-expr
	  :exprs exprs
	  :place (merge-places (place (car exprs))
			       (place (car (last exprs))))))
      (xt-expr (car args))))

(defun xt-name-expr (expr)
  (let ((name (xt-name (term-arg0 expr) nil)))
    (if (typep name '(or number-expr decimal))
	name
	(let ((upid (intern (string-upcase (format nil "~a" (id name))))))
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
		       name)))))))

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
	   (parse-error expr "Expression expected here"))
	  ((cdr exprs)
	   (make-instance 'tuple-expr
	     :exprs exprs
	     :place (term-place expr)))
	  (t (incf (parens (car exprs)))
	     (car exprs)))))

(defun xt-fieldex (expr)
  (make-instance 'fieldex
    :id (ds-id (term-arg0 expr))
    :actuals (unless (is-sop 'NOACTUALS (term-arg1 expr))
	       (xt-actuals (term-arg1 expr)))
    :place (term-place expr)))

(defun xt-fieldappl (expr)
  (make-instance 'fieldappl
    :id (ds-id (term-arg1 expr))
    :argument (xt-expr (term-arg0 expr))
    :place (term-place expr)))

(defun xt-projex (expr)
  (make-instance 'projex
    :index (ds-number (term-arg0 expr))
    :actuals (unless (is-sop 'NOACTUALS (term-arg1 expr))
	       (xt-actuals (term-arg1 expr)))))

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
	 (ne (make-instance 'name-expr
	       :id opid
	       :place (term-place op))))
    (make-instance 'infix-application
      :operator ne
      :argument (if (cdr args)
		    (let ((exprs (mapcar #'xt-expr args)))
		      (make-instance 'arg-tuple-expr
			:exprs exprs
			:place (merge-places (place (car exprs))
					     (place (car (last exprs))))))
		    (xt-expr (car args)))
      :place (term-place expr))))

(defun xt-unary-term-expr (uexpr)
  (let* ((op (term-arg0 uexpr))
	 (expr (term-arg1 uexpr))
	 (opex (mk-name-expr (sim-term-op op))))
    (setf (place opex) (term-place op))
    (make-instance 'unary-application
      :operator opex
      :argument (xt-expr expr)
      :place (term-place uexpr))))

(defvar *coercion-var-counter* (let ((x 0)) #'(lambda ()  (incf x))))

(defun xt-coercion (coercion)
  (let* ((expr (term-arg0 coercion))
	 (type (term-arg0 (term-arg1 coercion)))
	 (var (makesym "x%~a" (funcall *coercion-var-counter*))))
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
  (let ((op (term-arg0 bexpr))
	(body (term-arg1 bexpr)))
    (make-instance 'binding-application
      :operator (make-instance 'name-expr
		  :id (ds-id op)
		  :place (term-place op))
      :argument (make-xt-bind-expr 'lambda-expr body nil)
      :place (term-place bexpr))))

(defun xt-skovar (expr)
  (make-instance 'name-expr
    :id (makesym "~a!~d"
		 (ds-id (term-arg0 expr))
		 (ds-number (term-arg1 expr)))
    :place (term-place expr)))

(defun make-xt-bind-expr (class body save-as)
  (let* ((set-expr? (is-sop 'SET-EXPR body))
	 (commas? (xt-lambda-formals-check (term-arg0 body)))
	 (bindings (xt-lambda-formals (term-arg0 body) commas?))
	 (expr (xt-expr (term-arg1 body))))
    (make-instance class
      :bindings (xt-flatten-bindings (car bindings) (if set-expr? 1 0))
      :expression (make-xt-bind-expr* (cdr bindings) class expr)
      :commas? commas?
      :place (term-place save-as))))


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
	     (cond ((listp b) (return))
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
			       :id (intern (string-upcase id))
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
			(mapcar #'(lambda (a) (make-instance 'bind-decl
						:id (xt-idop a)
						:place (term-place a)))
			  (term-args args)))
	      :index index
	      :expression (xt-expr expr)
	      :place (term-place sel))
	    (make-instance 'selection
	      :constructor constr
	      :args (unless (is-sop 'SELECTION-NULL-1 args)
		      (mapcar #'(lambda (a) (make-instance 'bind-decl
					      :id (xt-idop a)
					      :place (term-place a)))
			(term-args args)))
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
		      :exprs (list (mk-else-condition nil conditions)
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
    (make-instance (if (is-sop 'NOACTUALS actuals)
		       'modname 'full-modname)
      :id (ds-vid id)
      :library (unless (is-sop 'NOLIB lib)
		 (ds-vid lib))
      :actuals (unless (is-sop 'NOACTUALS actuals)
		 (xt-actuals actuals))
      :mappings (unless (is-sop 'NOMAP mappings)
		  (xt-mappings mappings))
      :target (unless (is-sop 'NOTGT target)
		(xt-modname target))
      :place (term-place modname))))

;;; name! means create a name no matter what; otherwise numbers create
;;; number-exprs
(defun xt-name (name &optional name!)
  (let* ((idop (term-arg0 name))
	 (lib (term-arg1 name))
	 (actuals (term-arg2 name))
	 (mappings (term-arg4 name))
	 (target (term-arg5 name))
	 (maybe-num? (and (not name!)
			  (is-sop 'NOLIB lib)
			  (is-sop 'NOACTS actuals)
			  (is-sop 'NOMAP mappings)
			  (is-sop 'NOTGT target)
			  (or (is-number (term-arg0 idop))
			      (every #'digit-char-p
				     (string (ds-id (term-arg0 idop))))))))
    (multiple-value-bind (idops length)
	(xt-name-idops (term-arg3 name) maybe-num?)
      ;; At this point, idops is a number or a symbol, and length is the length of it.
      ;; If a number, then we know maybe-num? is true, and we can create a rational.
      ;; The nil symbol will return a length of either 0 or 3, depending on if whether
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
		     :fractional-length frac-length)
		   (let* ((denom (expt 10 frac-length))
			  (num (+ (* denom int-part) frac-value)))
		     (make-instance 'decimal
		       :operator (mk-name-expr '/)
		       :argument (mk-arg-tuple-expr
				  (make-instance 'number-expr
				    :number num)
				  (make-instance 'number-expr
				    :number denom)))))))
	    ((and maybe-num? (zerop length))
	     (let ((num (if (is-number (term-arg0 idop))
			    (ds-number (term-arg0 idop))
			    (parse-integer (string (ds-id (term-arg0 idop)))))))
	       (assert (integerp num))
	       (make-instance 'number-expr
		 :number num
		 :place (term-place name))))
	    (t (make-instance 'name
		 :id (if (zerop length)
			 (if (is-number (term-arg0 idop))
			     (makesym "~d" (ds-number (term-arg0 idop)))
			     (let ((id (ds-id (term-arg0 idop))))
			       (when (memq id '(|/\\| |\\/|))
				 (pushnew id *escaped-operators-used*))
			       id))
			 idops)
		 :library (unless (is-sop 'NOLIB lib)
			    (ds-vid lib))
		 :actuals (unless (is-sop 'NOACTS actuals)
			    (xt-actuals actuals))
		 :mappings (unless (is-sop 'NOMAP mappings)
			     (xt-mappings mappings))
		 :mod-id (unless (zerop length)
			   (if (is-number (term-arg0 idop))
			     (makesym "~d" (ds-number (term-arg0 idop)))
			     (ds-id (term-arg0 idop))))
		 :target (unless (is-sop 'NOTGT target)
			   (xt-modname target))
		 :place (term-place name)))))))

(defun xt-name-idops (term maybe-num?)
  (cond ((eq (sim-term-op term) 'NOMOD)
	 (values nil 0))
	((and maybe-num?
	      (null (cdr (term-args term)))
	      (or (is-number (term-arg0 (term-arg0 term)))
		  (every #'digit-char-p
			 (string (ds-id (term-arg0 (term-arg0 term)))))))
	 (if (is-number (term-arg0 (term-arg0 term)))
	     (values (ds-number (term-arg0 (term-arg0 term)))
		     (length (format nil "~d"
			       (ds-number (term-arg0 (term-arg0 term))))))
	     (values (parse-integer (string (ds-id (term-arg0 (term-arg0 term)))))
		     (length (string (ds-id (term-arg0 (term-arg0 term))))))))
	(t (let ((sym (makesym "~{~a~^.~}"
			       (mapcar #'(lambda (tm)
					   (if (is-number tm)
					       (makesym (ds-number (term-arg0 tm)))
					       (ds-id (term-arg0 tm))))
				 (term-args term)))))
	     (values sym (length (string sym)))))))

(defun xt-actuals (actuals)
  (mapcar #'xt-actual (term-args actuals)))

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
	 (kind (unless (is-sop 'NOQUAL (term-arg2 lhs))
		 (case (sim-term-op (term-arg2 lhs))
		   (TYPE 'type)
		   (THEORY 'theory)
		   (t 'expr))))
	 (dtype (when (is-sop 'TYPED (term-arg2 lhs))
		  (xt-not-enum-type-expr (term-arg0 (term-arg2 lhs)))))
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
    (if (is-sop 'NOFORMALS (term-arg1 lhs))
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
	(let ((formals (xt-pdf (term-arg1 lhs))))
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
  (make-instance 'name
    :id (xt-lhs-idops (term-arg0 lhs))
    :place (term-place lhs)))

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
	 (id (if (and (symbolp tid)
		      (every #'digit-char-p (string tid)))
		 (parse-integer (string tid))
		 tid)))
    (when (memq id '(|/\\| |\\/|))
      (pushnew id *escaped-operators-used*))
    (if (or (numberp id)
	    (valid-pvs-id id))
	id
	(parse-error term "Invalid id"))))

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
	(break "not valid?"))))
  
(defun valid-pvs-id** (idstr start end)
  (or (and (alpha-char-p (char idstr start))
	   (every #'(lambda (ch)
		      (or (alpha-char-p ch)
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
	       (every #'digit-char-p (subseq idstr start))))))
