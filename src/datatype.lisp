;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; datatype.lisp -- Generation and typechecking of datatypes
;; Author          : Sam Owre
;; Created On      : Sun Dec 12 01:18:46 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jul  2 18:51:55 1999
;; Update Count    : 125
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

;;; Generates the theory corresponding to the datatype.  The declarations
;;; generated are:
;;;   the uninterpreted datatype declaration
;;;   the recognizer declarations
;;;   the accessor declarations
;;;   the ord declaration (Not generated for now)
;;;   extensionality and eta axioms
;;;   accessor/constructor axioms
;;;   inclusive axiom
;;;   induction axiom
;;;   every definition - positive type parameters
;;;   some definition - positive type parameters
;;;   subterm definition
;;;   << definition
;;;   << well-founded axiom
;;;   reduce_nat definition
;;;   REDUCE_nat definition
;;;   reduce_ordinal definition
;;;   REDUCE_ordinal definition

(defvar *adt* nil
  "The datatype declaration being processed")

(defvar *adt-vars* nil
  "A hashtable that holds the variable names used for the adt, constructors,
and accessors")

(defvar *last-adt-decl* nil)

(defvar *generate-all-adt-axioms* t
  "Indicates whether all ADT axioms should be generated")

(defvar *ord-definition-cutoff* most-positive-fixnum
  "If the number of constructors is larger than this, its definition won't be
generated")

(defvar *negative-occurrence* nil)


;;; Three new theories are built and typechecked; this macro wraps around
;;; the generation and typechecking of the declarations.

(defmacro build-adt-theory (tid adt &rest forms)
  (let ((vtid (gensym))
	(vadt (gensym)))
    `(let* ((,vtid ,tid)
	    (,vadt ,adt)
	    (*generating-adt* ,vadt)
	    (*current-theory* (make-instance 'rectype-theory
				:id ,vtid
				:exporting (make-instance 'exporting
					     :kind 'default)))
	    (*current-context* (make-new-context *current-theory*))
	    (*typechecking-module* t)
	    (*tccs* nil)
	    (*tccdecls* nil)
	    (*exprs-generating-actual-tccs* nil))
       (setf (gethash ,vtid (if (from-prelude? ,vadt)
				*prelude*
				*pvs-modules*))
	     *current-theory*)
       ,@forms
       (setf (generated-by *current-theory*) (id ,vadt))
       (push 'typechecked (status *current-theory*))
       (generate-xref *current-theory*)
       (maphash #'(lambda (id decls)
		    (let ((ndecls (remove-if #'formal-decl? decls)))
		      (when ndecls
			(mapc #'set-visibility ndecls)
			(setf (gethash id (lhash-table
					   (current-declarations-hash)))
			      ndecls))))
		(lhash-table (current-declarations-hash)))
       (setf (all-usings *current-theory*)
	     (let ((imps nil))
	       (maphash #'(lambda (th thinsts)
			    (unless (and (from-prelude? th)
					 (singleton? thinsts))
			      (push (cons th thinsts) imps)))
			(lhash-table (current-using-hash)))
	       imps))
       (check-exporting *current-theory*)
       (setf (all-usings *current-theory*)
	     (let ((imps nil))
	       (map-lhash #'(lambda (th thinsts)
			      (unless (from-prelude? th)
				(push (cons th thinsts) imps)))
			  (current-using-hash))
	       imps))
       (setf (saved-context *current-theory*) *current-context*)
       *current-theory*)))

(defmethod typecheck ((adt recursive-type) &key expected context)
  (declare (ignore context))
  (let ((*subtype-of-hash* (make-hash-table :test #'equal)))
    (unwind-protect (typecheck* adt expected nil nil)
      (unless (typechecked? adt)
	(untypecheck-theory adt)))))

(defmethod typecheck* ((adt recursive-type) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*adt* adt)
	(*adt-vars* (make-hash-table :test #'eq))
	(*generate-tccs* 'all))
    (setf (adt-type-name adt) nil)
    (mapc #'(lambda (c)
	      (setf (con-decl c) nil
		    (rec-decl c) nil
		    (acc-decls c) nil))
	   (constructors adt))
    (if *typechecking-module*
	(typecheck-inline-adt adt)
	(typecheck-top-level-adt adt))))

(defun typecheck-inline-adt (adt)
  ;; (when (assuming adt)
  ;;   (type-error (car (assuming adt))
  ;;     "Assumings are not allowed for in-line (co)datatypes"))
  (let ((*generating-adt* adt)
	(*last-adt-decl* (current-declaration)))
    (generate-inline-adt adt))
  (put-decl adt)
  (setf (typechecked? adt) t))

(defun generate-inline-adt (adt)
  (cond ((decl-formals adt)
	  (typecheck* (decl-formals adt) nil nil nil)
	  (dolist (fdecl (decl-formals adt))
	    (set-visibility fdecl)
	    (setf (module fdecl) (current-theory))
	    (setf (associated-decl fdecl) adt))
	  (generate-adt-sections adt))
	 (t (generate-adt-sections adt))))

(defun typecheck-top-level-adt (adt)
  (tcdebug "~%Typecheck (CO)DATATYPE ~a" (id adt))
  (setf (formals-sans-usings adt)
	(remove-if #'importing-param? (formals adt)))
  (let ((adt-theory (generate-adt-theory adt)))
    (setf (adt-theory adt) adt-theory))
  (let ((map-theory (generate-adt-map-theory adt)))
    (setf (adt-map-theory adt) map-theory))
  (let ((reduce-theory (if (datatype? adt)
			   (generate-adt-reduce-theory adt)
			   (generate-codt-coreduce-theory adt))))
    (setf (adt-reduce-theory adt) reduce-theory))
  (save-adt-file adt)
  (let* ((adt-file (concatenate 'string (string (id adt))
				(if (datatype? adt) "_adt" "_codt")))
	 (proofs (read-pvs-file-proofs adt-file)))
    (restore-from-context adt-file (adt-theory adt) proofs)
    (when (adt-map-theory adt)
      (restore-from-context adt-file (adt-map-theory adt) proofs))
    (when (adt-reduce-theory adt)
      (restore-from-context adt-file (adt-reduce-theory adt) proofs)))
  adt)


;;; We don't want to create a file unless we have to.  Occasionally a
;;; forced typecheck comes through, and then we want to save the file only
;;; if it is different from what is out there.  Thus we compare the old
;;; forms with the new.  Note that the file is never read from, it is
;;; simply created so that the generated theories may be viewed
;;; conveniently.  We want to generate a file whenever there is a
;;; difference, which can happen either because the DATATYPE that
;;; generated it has changed, or a new patch causes the old one to be
;;; invalid.

(defun save-adt-file (adt)
  (let* ((adt-file (concatenate 'string (string (id adt))
				(if (datatype? adt) "_adt" "_codt")))
	 (adt-path (make-specpath adt-file))
	 (file-exists? #+allegro (file-exists-p adt-path)
		       #-allegro (probe-file adt-path))
	 (adt-theories (adt-generated-theories adt))
	 (mods (make-instance 'modules :modules adt-theories))
	 (ce (unless *loading-prelude*
	       (get-context-file-entry (filename adt))))
	 (adt-ce (get-context-file-entry adt-file))
	 (checksum-ok? (and adt-ce
			    file-exists?
			    (equal #+allegro (excl:md5-file adt-path)
				   #+sbcl (sb-md5:md5sum-file adt-path)
				   #-(or allegro sbcl) (md5:md5sum-file adt-path) 
				   (ce-md5sum adt-ce)))))
    (unless (and file-exists?
		 ce
		 checksum-ok?
		 (equal (ce-write-date ce)
			(file-write-time (make-specpath (filename adt)))))
      (when file-exists?
	(ignore-errors
	  (chmod "a+w" (namestring adt-path)))
	(ignore-file-errors
	 (delete-file (namestring adt-path))))
      (let* ((adtmod (let ((*unparse-expanded* t))
		       (unparse mods :string t)))
	     (condition (nth-value 1
			  (ignore-file-errors
			   (with-open-file (str adt-path :direction :output
						:if-exists :supersede)
			     (format str "~a~a~2%"
			       *adt-generated-string*
			       *current-file*)
			     (princ adtmod str))))))
	(if condition
	    (pvs-message "Error in writing out ~a: ~a"
	      (namestring adt-path) condition)
	    (pvs-message "Wrote pvs file ~a" adt-file)))
      (chmod "a-w" (namestring adt-path)))
    (let ((ntheories (parse :file adt-path)))
      (copy-lex adt-theories ntheories t)
      (assert (every #'place (theory (car adt-theories)))))
    (dolist (th adt-theories)
      (let* ((tot (car (tcc-info th)))
	     (prv (cadr (tcc-info th)))
	     (mat (caddr (tcc-info th)))
	     (obl (- tot prv mat)))
	(if (zerop tot)
	    (pvs-message "In ~aDATATYPE theory ~a: No TCCs generated~
                          ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
	      (if (codatatype? adt) "CO" "")
	      (id th) (length (warnings th)) (length (info th)))
	    (pvs-message
		    "In ~aDATATYPE theory ~a: ~d TCC~:p, ~d proved, ~
                     ~d subsumed, ~d unproved~
                     ~[~:;; ~:*~d warning~:p~]~[~:;; ~:*~d msg~:p~]"
	      (if (codatatype? adt) "CO" "")
	      (id th) tot prv mat obl
	      (length (warnings th)) (length (info th))))))
    (let ((fdate (file-write-time adt-path))
	  (ce2 (get-context-file-entry adt-file)))
      (setf (generated-file-date adt) fdate)
      (push 'typechecked (status adt))
      (dolist (th adt-theories)
	(setf (filename th) adt-file))
      (setf (gethash adt-file *pvs-files*)
	    (cons fdate (adt-generated-theories adt)))
      (cond (ce2
	     (setf (ce-write-date ce2) fdate)
	     (setq *pvs-context-changed* t))
	    (t (update-context adt-file))))))


(defun adt-generated-theories (adt)
  (when (adt-theory adt)
    (delete-if #'null
      (list (adt-theory adt) (adt-map-theory adt) (adt-reduce-theory adt)))))

;;; The adt-theory

(defmethod generate-adt-theory ((adt datatype))
  (build-adt-theory (makesym "~a_adt" (id adt)) adt
    (generate-adt-sections adt)))

(defmethod generate-adt-theory ((adt codatatype))
  (build-adt-theory (makesym "~a_codt" (id adt)) adt
    (generate-adt-sections adt)))

(defun generate-adt-sections (adt)
  (generate-adt-vars adt)
  (set-constructor-ord-numbers (constructors adt))
  (check-adt-name-uniqueness adt)
  (generate-adt-type adt)
  (when (formals adt)
    (generate-adt-formals adt))
  (when (assuming adt)
    (generate-adt-assuming adt))
  (generate-adt-importing (importings adt))
  (generate-adt-decls adt)
  (check-adt-types adt)
  (set-adt-nonemptiness adt)
  (set-adt-positive-formal-types adt)
  (generate-adt-uninterpreted-ord-function-and-axiom adt)
  (generate-adt-ord-function adt)
  (if (or (enumtype? adt)
	  (every #'(lambda (c) (null (arguments c))) (constructors adt)))
      (progn (generate-inclusive-axiom adt)
	     (generate-disjoint-axiom adt))
      (generate-adt-axioms adt))
  (generate-remaining-adt-sections adt)
  (setf (adt-theory adt) (current-theory)))

(defmethod generate-remaining-adt-sections ((adt datatype))
  (generate-adt-induction adt)
  (unless (or (enumtype? adt)
	      (every #'(lambda (c) (null (arguments c))) (constructors adt)))
    (when (and (positive-types adt)
	       (not (every #'null (positive-types adt))))
      (generate-adt-every adt)
      (generate-adt-some adt)))
  (generate-adt-subterm-decls adt)
  (unless (or (enumtype? adt)
	      (every #'(lambda (c) (null (arguments c))) (constructors adt)))
    (generate-adt-reduce adt '|nat|)
    (when (and (not (eq (id adt) '|ordstruct|))
	       (get-theory "ordstruct_adt"))
      (generate-adt-reduce adt '|ordinal|))))

;;; (remove-method #'positive-types
;;;    (find-method #'positive-types '() (list (find-class 'inline-recursive-type))))

(defmethod generate-remaining-adt-sections ((codt codatatype))
  (generate-bisimulation codt)
  (unless (or (enumtype? codt)
	      (every #'(lambda (c) (null (arguments c))) (constructors codt)))
    (when (and (positive-types codt)
	       (not (every #'null (positive-types codt))))
      (generate-adt-every codt)
      (generate-adt-some codt))
    ;;(generate-codt-subterm codt)
    ))

(defun generate-bisimulation (adt)
  (let* ((rvid (make-new-variable 'R adt))
	 (rtype (typecheck (mk-type-name 'PRED
			     (list (make-instance 'actual
				     :expr
				     (mk-tupletype
				      (list (mk-type-name (id adt))
					    (mk-type-name (id adt)))))))))
	 (rbd (make-bind-decl rvid rtype))
	 (rvar (make-variable-expr rbd))
	 (avid (make-new-variable '|x| adt))
	 (bvid (make-new-variable '|y| adt))
	 (atype (typecheck (mk-type-name (id adt))))
	 (btype (typecheck (mk-type-name (id adt))))
	 (abd (make-bind-decl avid atype))
	 (bbd (make-bind-decl bvid btype))
	 (avar (make-variable-expr abd))
	 (bvar (make-variable-expr bbd))
	 (*bound-variables* (cons rbd (cons abd (cons bbd *bound-variables*))))
	 (cform (mk-forall-expr (list abd bbd)
		  (mk-implication
		   (mk-application* rvar (list avar bvar))
		   (mk-disjunction
		    (mapcar #'(lambda (c)
				(bisimulation-case c avar bvar rvar adt))
		      (constructors adt))))))
	 (cdecl (mk-adt-def-decl '|bisimulation?|
		  *boolean*
		  (pc-parse (unparse cform :string t) 'expr)
		  (list (list rbd))))
	 (bivid (make-new-variable 'B adt))
	 (bitype (mk-expr-as-type (mk-name-expr '|bisimulation?|)))
	 (bibd (mk-bind-decl bivid bitype))
	 (bivar (mk-name-expr bivid))
	 (fdecl (mk-formula-decl '|coinduction|
		  (mk-forall-expr (list bibd abd bbd)
		    (mk-application '=>
		      (mk-application bivar avar bvar)
		      (mk-application '= avar bvar)))
		  'AXIOM)))
    (typecheck-adt-decl cdecl)
    (typecheck-adt-decl fdecl)))

(defun bisimulation-case (c avar bvar rvar adt)
  (if (arguments c)
      (let ((avals
	     (mapcar #'(lambda (a)
			 (bisimulation-arg-value
			  (type a)
			  (if (some #'(lambda (cc)
					(and (not (eq cc c))
					     (some #'(lambda (aa)
						       (same-id aa a))
						   (arguments cc))))
				    (constructors adt))
			      (mk-application (id a)
				(mk-coercion avar
					     (mk-expr-as-type
					      (mk-name-expr (recognizer c)))))
			      (mk-application (id a) avar))
			  (mk-application (id a) bvar)
			  rvar adt))
	       (arguments c))))
	(mk-conjunction (cons (mk-application (mk-name-expr (recognizer c))
				avar)
			      (cons (mk-application (mk-name-expr (recognizer c))
				      bvar)
				    avals))))
      (mk-conjunction (list (mk-application (mk-name-expr (recognizer c))
			      avar)
			    (mk-application (mk-name-expr (recognizer c))
			      bvar)))))

(defmethod bisimulation-arg-value ((te type-name) avar bvar rvar adt)
  (cond ((same-declaration te (adt-type-name adt))
	 (mk-application rvar (copy avar) (copy bvar)))
	((adt? te)
	 (let ((rels (mapcar #'(lambda (act)
				 (bisimulation-rel (type-value act)
						   rvar adt))
		       (positive-actuals te))))
	   (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=)))
		      rels)
	       (mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	       (mk-application (mk-simple-every-application te adt rels)
		 (copy avar) (copy bvar)))))
	(t (mk-application (mk-name-expr '=) (copy avar) (copy bvar)))))

(defmethod bisimulation-arg-value ((te datatype-subtype) avar bvar rvar adt)
  (bisimulation-arg-value (declared-type te) avar bvar rvar adt))

(defmethod bisimulation-arg-value ((te subtype) avar bvar rvar adt)
  (if (finite-set-type? te)
      (let ((rel (bisimulation-rel (supertype te) rvar adt)))
	(if (and (name-expr? rel) (eq (id rel) '=))
	    (mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	    (mk-application (let* ((name (mk-name-expr '|every|))
				   (pname (pc-parse (unparse name :string t)
					    'expr)))
			      (mk-application pname rel))
	      (copy avar) (copy bvar))))
      (bisimulation-arg-value (supertype te) avar bvar rvar adt)))

(defmethod bisimulation-arg-value ((te funtype) avar bvar rvar adt)
  (let* ((fid (make-new-variable '|x| (list adt avar bvar rvar)))
	 (fbd (make-bind-decl fid (domain te)))
	 (*bound-variables* (cons fbd *bound-variables*))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (rel (bisimulation-rel
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       rvar adt)))
    (if (and (name-expr? rel) (eq (id rel) '=))
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-forall-expr (list fbd)
	  (mk-application rel
	    (mk-application (copy avar) fvar)
	    (mk-application (copy bvar) fvar))))))

(defmethod bisimulation-arg-value ((te recordtype) avar bvar rvar adt)
  (let ((rels (bisimulation-selection-fields
	       (fields te) avar bvar rvar adt (dependent? te))))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-conjunction
	 (mapcar #'(lambda (fd rel)
		     (mk-application rel
		       (mk-application (id fd) (copy avar))
		       (mk-application (id fd) (copy bvar))))
	   (fields te) rels)))))

(defun bisimulation-selection-fields (fields avar bvar rvar adt dep?
					     &optional result)
  (if (null fields)
      (nreverse result)
      (acc-map-selection-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       avar bvar rvar adt dep?
       (cons (bisimulation-rel (type (car fields)) rvar adt)
	     result))))

(defmethod bisimulation-arg-value ((te tupletype) avar bvar rvar adt)
  (let ((rels (bisimulation-selection-types (types te) avar bvar rvar adt)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-application (mk-name-expr '=) (copy avar) (copy bvar))
	(mk-conjunction
	 (let ((num 0))
	   (mapcar #'(lambda (rel)
		       (incf num)
		       (let ((aproj (make-instance 'projappl
				      :id (makesym "PROJ_~d" num)
				      :index num
				      :argument (copy avar)))
			     (bproj (make-instance 'projappl
				      :id (makesym "PROJ_~d" num)
				      :index num
				      :argument (copy bvar))))
			 (mk-application rel aproj bproj)))
	     rels))))))

(defun bisimulation-selection-types (types avar bvar rvar adt &optional result)
  (if (null types)
      (nreverse result)
      (bisimulation-selection-types
       (cdr types)
       avar bvar rvar adt
       (cons (bisimulation-rel (car types) rvar adt) result))))

(defmethod bisimulation-arg-value ((te type-expr) avar bvar rvar adt)
  (declare (ignore rvar adt))
  (mk-application (mk-name-expr '=) (copy avar) (copy bvar)))

(defmethod bisimulation-rel ((te type-name) rvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (copy rvar))
	((adt? te)
	 (let ((rels (mapcar #'(lambda (act)
				   (bisimulation-rel (type-value act)
						     rvar adt))
			 (positive-actuals te))))
	   (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	       (mk-name-expr '=)
	       (mk-simple-every-application te adt rels))))
	(t (mk-name-expr '=))))

(defmethod bisimulation-rel ((te datatype-subtype) rvar adt)
  (bisimulation-rel (declared-type te) rvar adt))

(defmethod bisimulation-rel ((te subtype) rvar adt)
  (if (finite-set-type? te)
      (let ((rel (bisimulation-rel (supertype te) rvar adt)))
	(if (and (name-expr? rel) (eq (id rel) '=))
	    (mk-name-expr '=)
	    (let* ((name (mk-name-expr '|every|))
		   (pname (pc-parse (unparse name :string t)
			    'expr)))
	      (mk-application pname rel))))
      (bisimulation-rel (supertype te) rvar adt)))

(defmethod bisimulation-rel ((te funtype) rvar adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (*bound-variables* (cons fbd *bound-variables*))
	 (rel (bisimulation-rel
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) fvar nil))
		   (range te))
	       rvar adt)))
    (if (and (name-expr? rel) (eq (id rel) '=))
	rel
	(let* ((lid (make-new-variable '|f| (list te rel)))
	       (lbd (make-bind-decl lid te))
	       (lvar (mk-name-expr lid nil nil
				   (make-resolution lbd
				     (current-theory-name) te))))
	  (mk-lambda-expr (list lbd)
	    (mk-forall-expr (list fbd)
	      (mk-application rel
		(mk-application lvar fvar))))))))

(defmethod bisimulation-rel ((te recordtype) rvar adt)
  (let* ((rxid (make-new-variable '|rx| te))
	 (rxbd (make-bind-decl rxid te))
	 (rxvar (make-variable-expr rxbd))
	 (ryid (make-new-variable '|ry| te))
	 (rybd (make-bind-decl ryid te))
	 (ryvar (make-variable-expr rybd))
	 (*bound-variables* (cons rxbd (cons rybd *bound-variables*)))
	 (rels (bisimulation-rel-fields (fields te) rxvar ryvar rvar adt)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-name-expr '=)
	(mk-lambda-expr (list rxbd rybd)
	  (mk-conjunction rels)))))

(defun bisimulation-rel-fields (fields rxvar ryvar rvar adt
				       &optional rels)
  (if (null fields)
      (nreverse rels)
      (let ((rel (bisimulation-arg-value
		  (type (car fields))
		  (make-field-application (car fields) rxvar)
		  (make-field-application (car fields) ryvar)
		  rvar adt)))
	(bisimulation-rel-fields (cdr fields) rxvar ryvar rvar adt
				 (cons rel rels)))))

(defmethod bisimulation-rel ((te tupletype) rvar adt)
  (let* ((txid (make-new-variable '|tx| te))
	 (txbd (make-bind-decl txid te))
	 (txvar (make-variable-expr txbd))
	 (tyid (make-new-variable '|ty| te))
	 (tybd (make-bind-decl tyid te))
	 (tyvar (make-variable-expr tybd))
	 (*bound-variables* (cons txbd (cons tybd *bound-variables*)))
	 (rels (bisimulation-rel-types (types te) txvar tyvar rvar adt 1)))
    (if (every #'(lambda (x) (and (name-expr? x) (eq (id x) '=))) rels)
	(mk-name-expr '=)
	(mk-lambda-expr (list txbd tybd)
	  (mk-conjunction rels)))))

(defun bisimulation-rel-types (types rxvar ryvar rvar adt num
				     &optional rels)
  (if (null types)
      (nreverse rels)
      (let ((rel (bisimulation-arg-value
		  (car types)
		  (make-projection-application num rxvar)
		  (make-projection-application num ryvar)
		  rvar adt)))
	(bisimulation-rel-types (cdr types) rxvar ryvar rvar adt (1+ num)
				(cons rel rels)))))

(defmethod bisimulation-rel ((te dep-binding) rvar adt)
  (bisimulation-rel (type te) rvar adt))

(defmethod bisimulation-rel ((te type-expr) rvar adt)
  (declare (ignore rvar adt))
  (mk-name-expr '=))

;;;;;;;;;;;;;;

(defun set-adt-nonemptiness (adt)
  (if (some #'(lambda (c)
		(every #'(lambda (a) (not (possibly-empty-type? (type a))))
		       (arguments c)))
	    (constructors adt))
      (setf (nonempty? (adt-type-name adt)) t)
      (pvs-warning "Datatype ~a may be empty" (id adt)))
  (set-recognizer-nonemptiness adt))

(defun set-recognizer-nonemptiness (adt)
  (mapc #'set-recognizer-nonemptiness* (constructors adt)))

(defun set-recognizer-nonemptiness* (constructor)
  (if (arguments constructor)
      (unless (some #'(lambda (a) (possibly-empty-type? (type a)))
		    (arguments constructor))
	(set-nonempty-type (range (type (con-decl constructor))) (con-decl constructor)))
      (set-nonempty-type (type (con-decl constructor)) (con-decl constructor))))

(defmethod set-adt-positive-formal-types ((adt recursive-type))
  (when (formals adt)
    (setf (positive-types adt)
	  (mapcar #'type-value
	    (remove-if-not #'(lambda (ff)
			       (and (typep ff 'formal-type-decl)
				    (occurs-positively? (type ff) adt)))
	      (formals-sans-usings (current-theory)))))))

(defmethod set-adt-positive-formal-types ((adt inline-recursive-type))
  ;; In this case, the positive types are only for the adt-type-name
  ;; perhaps should do this also for the top-level recursive types as well
  (when (decl-formals adt)
    (setf (positive-types adt)
	  (mapcar #'type-value
	    (remove-if-not #'(lambda (ff)
			       (and (typep ff 'formal-type-decl)
				    (occurs-positively? (type ff) adt)))
	      (decl-formals (declaration (adt-type-name adt))))))))

(defun check-adt-name-uniqueness (adt)
  (let ((constrids (mapcar #'id (remove-if-not #'simple-constructor?
				  (constructors adt))))
	(recognids (mapcar #'recognizer (constructors adt)))
	(*current-theory* (if (typep adt 'inline-recursive-type)
			      (current-theory)
			      adt)))
    (when (duplicates? constrids)
      (type-error (duplicates? (constructors adt) :test #'same-id)
	"Duplicate ~a names are not allowed"
	(if (typep adt 'enumtype) "enumeration" "constructor")))
    (when (duplicates? recognids)
      (type-error (duplicates? (constructors adt) :key #'recognizer)
	"Duplicate recognizer names are not allowed"))
    (when (duplicates? (append constrids recognids))
      (type-error (find-if #'(lambda (c) (memq (id c) recognids))
		    (constructors adt))
	"~a may not be used as both a constructor and recognizer name"
	(duplicates? (append constrids recognids))))
    (when (duplicates? (cons (id adt) recognids))
      (type-error adt "Datatype name may not be used as a recognizer name"))
    (dolist (c (constructors adt))
      (let ((accessids (mapcar #'id (arguments c))))
	(when (duplicates? accessids)
	  (type-error (duplicates? (arguments c) :test #'same-id)
	    "May not have duplicate accessor names for a single constructor"))
	(when (duplicates? (cons (id adt) accessids))
	  (type-error (find-if #'(lambda (x) (eq (id x) (id adt)))
			      (arguments c))
	    "Datatype name should not be used as an accessor name"))))))

(defun check-adt-positive-types (adt)
  (setf (positive-types (current-theory))
	(mapcar #'type-value
		(remove-if-not #'(lambda (ff)
				   (and (typep ff 'formal-type-decl)
					(occurs-positively? (type ff) adt)))
		  (formals-sans-usings (current-theory))))))

(defun non-recursive-constructor (c type)
  (not (some #'(lambda (a) (occurs-in type (type a))) (arguments c))))

(defun generate-adt-formals (adt)
  (setf (formals (current-theory)) (mapcar #'copy (formals adt)))
  (setf (formals-sans-usings (current-theory))
	(remove-if #'importing-param? (formals (current-theory))))
  (mapc #'(lambda (afm cfm)
	    (let ((*adt-decl* afm))
	      ;;(setf (abstract-syntax cfm) nil)
	      (typecheck-adt-decl cfm nil)))
	(formals adt) (formals (current-theory)))
  (set-dependent-formals (formals-sans-usings (current-theory))))

(defun generate-adt-assuming (adt)
  (setf (assuming (current-theory)) (mapcar #'copy (assuming adt)))
  (mapc #'(lambda (ad cd)
	    (let ((*adt-decl* ad))
	      (typecheck-adt-decl cd nil)))
	(assuming adt) (assuming (current-theory))))

(defun generate-adt-importing (importings)
  (when importings
    (dolist (imp importings)
      (let* ((cimp (copy-importing imp))
	     (*adt-decl* nil))
	(typecheck-adt-decl cimp)))))

(defun copy-importing (imp)
  (let ((thname (pc-parse (unparse (theory-name imp) :string t) 'modname)))
    (setf (place thname) (place (theory-name imp)))
    (make-instance 'importing
      :theory-name thname)))

(defun generate-adt-type (adt)
  (let* ((*adt-decl* adt)
	 (dparams (new-decl-formals* adt))
	 (tdecl (mk-type-decl (id adt) 'type-decl nil dparams)))
    (typecheck-adt-decl tdecl)
    (setf (generated-by tdecl) (id adt))
    (setf (adt-type-name adt) (type-value tdecl))))

(defun generate-adt-decls (adt)
  (let* ((ptype (mk-predtype (adt-type-name adt)))
	 (last (car (last (constructors adt)))))
    (unless (inline-datatype? adt)
      (dolist (fm (formals adt))
	(unless (typep fm 'importing)
	  (setf (module fm) adt)))
      (dolist (c (constructors adt))
	(dolist (arg (arguments c))
	  (setf (module arg) adt))))
    (dolist (c (constructors adt))
      (let ((rec (generate-adt-recognizer (recognizer c) c ptype)))
	(setf (chain? rec) (not (eq c last)))))
    (generate-adt-subtypes adt)
    (generate-accessors adt)
    (dolist (c (constructors adt))
      (generate-adt-constructor c))))


;;; Generate the subtype type declarations

(defmethod generate-adt-subtypes ((adt recursive-type))
  nil)

(defmethod generate-adt-subtypes ((adt recursive-type-with-subtypes))
  (dolist (subtype (subtypes adt))
    ;; First generate the predicate
    (let* ((pred-decl (mk-adt-subtype-pred subtype (constructors adt)))
	   (*adt-decl* pred-decl))
      (typecheck-adt-decl pred-decl)
      (put-decl pred-decl)
      ;; Then the (sub)type declaration
      (let* ((sdecl (mk-adt-subtype-decl
		     subtype (mk-adt-subtype subtype (constructors adt))))
	     (*adt-decl* sdecl))
	(typecheck-adt-decl sdecl)
	(put-decl sdecl)
	(typecheck* subtype nil nil nil))))
  (let ((*generate-tccs* 'none))
    ;; Now make sure the subtype field of each constructor is typechecked
    (dolist (c (constructors adt))
      (typecheck* (subtype c) nil nil nil))
    ;; Finally set up judgements for subtypes with more than one constructor
    (dolist (c (constructors adt))
      (when (multiple-recognizer-subtypes? c (constructors adt))
	(let ((jdecl (make-instance 'subtype-judgement
		       :declared-subtype (mk-expr-as-type
					  (mk-name-expr (recognizer c)))
		       :declared-type (subtype c))))
	  (typecheck-adt-decl jdecl)
	  (put-decl jdecl))))))

(defun multiple-recognizer-subtypes? (c constructors)
  (some #'(lambda (cc)
	    (and (not (eq cc c))
		 (eq (id (subtype cc)) (id (subtype c)))))
	constructors))

(defun mk-adt-subtype-pred (subtype constructors &optional preds)
  (if (null constructors)
      (let* ((var (make-new-variable '|x| preds))
	     (bd (mk-bind-decl var (adt-type-name *adt*)))
	     (*bound-variables* (cons bd *bound-variables*))
	     (appreds (mapcar #'(lambda (p)
				  (mk-application p (mk-name-expr var)))
			      (nreverse preds))))
	(mk-adt-def-decl (id subtype) (copy *boolean*)
			 (mk-disjunction appreds) (list bd) nil
			 (place subtype)))
      (mk-adt-subtype-pred subtype (cdr constructors)
			   (if (same-id subtype (subtype (car constructors)))
			       (cons (recognizer (car constructors)) preds)
			       preds))))

(defun mk-adt-subtype (subtype constructors &optional preds)
  (if (null constructors)
      (if (cdr preds)
	  (let* ((var (make-new-variable '|x| preds))
		 (bd (mk-bind-decl var (adt-type-name *adt*)))
		 (*bound-variables* (cons bd *bound-variables*))
		 (appreds (mapcar #'(lambda (p)
				      (mk-application p (mk-name-expr var)))
			    (nreverse preds))))
	    (mk-setsubtype (adt-type-name *adt*)
			   (mk-lambda-expr (list bd)
			     (mk-disjunction appreds))))
	  (mk-expr-as-type (mk-name-expr (car preds))))
      (mk-adt-subtype subtype (cdr constructors)
		      (if (same-id subtype (subtype (car constructors)))
				 (cons (recognizer (car constructors)) preds)
				 preds))))

(defun mk-adt-subtype-decl (subtype-name subtype)
  (let ((tdecl (mk-type-decl (id subtype-name) 'type-eq-decl subtype)))
    (setf (place tdecl) (place subtype-name))
    tdecl))


;;; Generate the recognizer type declarations

(defun generate-adt-recognizers (adt)
  (let* ((ptype (mk-predtype (adt-type-name adt)))
	 (recs (mapcar #'(lambda (c)
			   (generate-adt-recognizer (recognizer c) c ptype))
		       (constructors adt))))
    (mapc #'(lambda (r) (setf (chain? r) t)) recs)
    (setf (chain? (car (last recs))) nil)
    recs))

(defun generate-adt-recognizer (rec constr ptype)
  (let* ((dparams (new-decl-formals* *adt*))
	 (rdecl (mk-adt-recognizer-decl
		 rec (parse-unparse ptype 'type-expr)
		 (ordnum constr) dparams))
	 (*adt-decl* rdecl))
    (typecheck-adt-decl rdecl)
    (put-decl rdecl)
    (setf (rec-decl constr) rdecl)))


;;; Generate the constructor constant declarations.

(defun generate-adt-constructors (adt)
  (mapc #'(lambda (c)
	    (generate-adt-constructor c))
	(constructors adt)))

(defun new-decl-formals (adt)
  (assert (adt-type-name adt))
  (if (decl-formals adt)
      (let* ((fdecls (new-decl-formals* adt))
	     (dacts (mk-dactuals fdecls))
	     (thinst (mk-modname (id (current-theory)) nil nil nil dacts))
	     (atype (subst-adt-type (adt-type-name adt) thinst adt)))
	(values fdecls dacts thinst atype))
      (values nil nil (current-theory-name) (adt-type-name adt))))

(defun new-decl-formals* (adt)
  (when (decl-formals adt)
    (let* ((fdecls (mapcar #'(lambda (fp)
			       (copy fp
				 ;;'id (makesym "~a_1" (id fp))
				 'typechecked? nil
				 'type nil
				 'type-value nil
				 'associated-decl nil))
		     (decl-formals adt))))
      (typecheck* fdecls nil nil nil)
      fdecls)))

(defun generate-adt-constructor (constr)
  (let* ((dparams (new-decl-formals* *adt*))
	 (dacts (mk-dactuals dparams))
	 (cdecl (mk-adt-constructor-decl (id constr) nil (ordnum constr) dparams)))
    ;;(typecheck-adt-decl cdecl t nil nil)
    (let ((ftype (with-current-decl cdecl
		   (generate-adt-constructor-type constr dacts))))
      (setf (declared-type cdecl) ftype)
      (let ((*adt-decl* constr)
	    (*generate-tccs* 'none))
	(typecheck-adt-decl cdecl)
	(setf (con-decl constr) cdecl)))))

(defun mk-dactuals (dparams)
  (mapcar #'(lambda (dp) (mk-actual (type dp))) dparams))

(defun generate-adt-constructor-type (constr dacts)
  (if (arguments constr)
      (let ((type (mk-funtype (generate-adt-constructor-domain
			       (arguments constr) dacts)
			      (mk-expr-as-type
			       (mk-unique-name-expr
				(recognizer constr)
				(type (rec-decl constr))
				dacts)))))
	;;(typecheck type)
	type)
      (mk-expr-as-type (mk-unique-name-expr (recognizer constr)
					    (type (rec-decl constr))
					    dacts))))

(defun mk-unique-name-expr (id type dacts)
  (let ((ex (mk-name-expr id)))
    (setf (dactuals ex) dacts)
    (if (> (count-if #'(lambda (d) (eq (module d) (current-theory)))
		     (get-declarations id))
	   1)
	(make-instance 'coercion
	  :operator (mk-lambda-expr (list (mk-bind-decl '|x| type)) (mk-name-expr '|x|))
	  :argument ex)
	ex)))

(defun generate-adt-constructor-domain (accessors dacts &optional result)
  (if (null accessors)
      (nreverse result)
      (let* ((acc (car accessors))
	     (occ? (id-occurs-in (id acc) (cdr accessors)))
	     (dtype (parse-unparse (subst-adt-decl-actuals (declared-type acc) dacts)
				   'type-expr))
	     (atype (if occ?
			(mk-dep-binding (id acc) dtype)
			dtype))
	     (*adt-decl* dtype))
	(assert atype)
	(let* ((ty (typecheck atype))
	       (*bound-variables* (if occ?
				      (cons ty *bound-variables*)
				      *bound-variables*)))
	  (generate-adt-constructor-domain (cdr accessors)
					   dacts
					   (cons atype result))))))

(defun subst-adt-decl-actuals (type dacts)
  (gensubst type
    #'(lambda (x) (copy x 'dactuals dacts))
    #'adt-component-name))

(defun adt-component-name (x &optional (adt *adt*))
  (and (name? x)
       (or (eq (id x) (id adt))
	   (member (id x) (constructors adt)
		   :test (lambda (id y)
			   (or (eq id (id y))
			       (eq id (recognizer y))
			       (member id (arguments y) :key #'id)))))))
  


;;; Generate the accessor constant declarations.

(defun generate-accessors (adt)
  (let* ((common-accessors (collect-common-accessors adt))
	 (acc-decls (mapcar #'(lambda (entry)
				(generate-accessor entry adt))
		      common-accessors)))
    (dolist (c (constructors adt))
      ;;(dolist (a (arguments c))
	;;(setf (type a) (typecheck* (declared-type a) nil nil nil)))
      (setf (acc-decls c)
	    (find-corresponding-acc-decls
	     (arguments c) common-accessors acc-decls)))
    (mapc #'(lambda (entry acc-decl)
	      (when (cddr (car entry))
		(make-common-accessor-subtype-judgements
		 (cdar entry) (domain (type acc-decl)) adt)))
	  common-accessors acc-decls)
    ;;(generate-common-accessor-evel-defns acc-decls adt)
    acc-decls))

;; This doesn't work - the ground evaluator will call it recursively
;; (defun generate-common-accessor-evel-defns (acc-decls adt)
;;   (when acc-decls
;;     (generate-common-accessor-evel-defn (car acc-decls) adt)
;;     (generate-common-accessor-evel-defns (cdr acc-decls) adt)))

;; (defmethod generate-common-accessor-evel-defn ((d shared-adt-accessor-decl)
;; 					       adt)
;;   (let* ((vid (make-new-variable 'x adt))
;; 	 (bd (make-bind-decl vid (domain (type d))))
;; 	 (var (make-variable-expr bd))
;; 	 (body (mk-lambda-expr (list bd)
;; 		 (mk-cases-expr var
;; 		   (mapcar #'(lambda (c)
;; 			       (mk-selection (mk-name-expr (id c))
;; 				 (mapcar #'(lambda (a)
;; 					     (mk-bind-decl (id a) nil))
;; 				   (arguments c))
;; 				 (mk-name-expr (id d))))
;; 		     (constructors d))
;; 		   nil))))
;;     (typecheck* body (type d) nil nil)
;;     (setf (eval-fun d) body)))
  
;; (defmethod generate-common-accessor-evel-defn ((d adt-accessor-decl) adt)
;;   nil)

(defun find-corresponding-acc-decls (accs common-accessors acc-decls
					  &optional result)
  (if (null accs)
      (nreverse result)
      (let ((corr-act (find-corresponding-acc-decl
		       (car accs) common-accessors acc-decls)))
	(assert corr-act)
	(setf (accessor-decl (car accs)) corr-act)
	(find-corresponding-acc-decls
	 (cdr accs)
	 common-accessors
	 acc-decls
	 (cons corr-act result)))))

(defun find-corresponding-acc-decl (acc common-accessors acc-decls)
  (assert common-accessors)
  (if (find-corresponding-acc-decl* acc (car common-accessors))
      (car acc-decls)
      (find-corresponding-acc-decl
       acc (cdr common-accessors) (cdr acc-decls))))

(defun find-corresponding-acc-decl* (acc common-accessors)
  (when common-accessors
    (or (memq acc (cdar common-accessors))
	(find-corresponding-acc-decl* acc (cdr common-accessors)))))

(defun generate-accessor (entry adt)
  ;; entry is of the form (((adtdecl range-type deps acc-decl) adtdecl ...)
  ;;                       ((adtdecl range-type deps acc-decl) adtdecl ...) ...)
  ;; where each element reflects a different, but common, range type.
  (let* ((acc-decl (cadddr (caar entry)))
	 (dparams (decl-formals acc-decl))
	 (domain (get-accessor-domain-type entry adt))
	 (range (get-accessor-range-type entry domain adt))
	 (acc-type (with-added-decls dparams
		     (make-accessor-funtype domain range nil)))
	 (acc-decls (cdar entry)))
    (setf (declared-type acc-decl) acc-type)
    (when (cdr acc-decls)
      (change-class 'shared-adt-accessor-decl acc-decl
	:constructors (mapcar #'(lambda (d)
				  (id (find d (constructors adt)
					    :key #'arguments :test #'memq)))
			acc-decls)))
    (typecheck-adt-decl acc-decl)
    (when (cddr (car entry))
      (make-common-accessor-subtype-judgements (cdar entry) domain adt))
    (assert (or (null (decl-formals *adt*)) (decl-formals acc-decl)))
    acc-decl))

(defun make-common-accessor-subtype-judgements (adtdecls domain adt)
  (dolist (adtdecl adtdecls)
    (make-common-accessor-subtype-judgement adtdecl domain adt)))

(defun make-common-accessor-subtype-judgement (adtdecl domain adt)
  (let* ((*generate-tccs* 'none)
	 (constr (find adtdecl (constructors adt)
		      :key #'arguments :test #'memq))
	 (subtype (mk-expr-as-type (mk-name-expr (recognizer constr))))
	 (tsubtype (typecheck* subtype nil nil nil)))
    (unless (subtype-of? tsubtype domain)
      (let ((jdecl (make-instance 'subtype-judgement
		     :declared-subtype subtype
		     :declared-type (dep-binding-type domain))))
	(typecheck-adt-decl jdecl)
	(put-decl jdecl)))))

(defun make-accessor-funtype (domain range deps)
  (if deps
      (let* ((tdom (typecheck* (copy domain) nil nil nil))
	     (dtype (if (dep-binding? tdom)
			tdom
			(mk-dep-binding (make-new-variable '|d|
					  (list tdom range))
					tdom)))
	     (dep-name (make-variable-expr dtype))
	     (*bound-variables* (cons dtype *bound-variables*))
	     (subst-range (substit range
			    (if (dep-binding? domain)
				(pairlis
				 (cons domain (mapcar #'declaration deps))
				 (cons dtype
				       (mapcar #'(lambda (dep)
						   (typecheck* (mk-application (id dep)
								 dep-name)
							       (type dep) nil nil))
					 deps)))
				(pairlis
				 (mapcar #'declaration deps)
				 (mapcar #'(lambda (dep)
					     (typecheck* (mk-application (id dep)
							   dep-name)
							 (type dep) nil nil))
				   deps))))))
	(mk-funtype dtype subst-range))
      (mk-funtype domain range)))

(defun get-accessor-domain-type (entry adt)
  (let ((domain (get-accessor-domain-basic-type entry adt)))
    (if (cdr entry)
	(let* ((dep-id (make-new-variable '|d| adt))
	       (dtype (typecheck* domain nil nil nil)))
	  (mk-dep-binding dep-id dtype))
	domain)))
  
(defun get-accessor-domain-basic-type (entry adt)
  (let ((acc-decls (mapappend #'cdr entry))
	(dparams (decl-formals (cadddr (caar entry)))))
    (if (= (length (mapappend #'cdr entry)) (length (constructors adt)))
	(mk-type-name (id adt))
	(if (subtypes adt)
	    (multiple-value-bind (subtypes recognizers)
		(get-accessor-covered-subtypes (copy-list acc-decls) adt)
	      (get-accessor-domain-type* subtypes recognizers dparams))
	    (get-accessor-domain-type*
	     nil
	     (mapcan #'(lambda (c)
			 (when (some #'(lambda (acc) (memq acc acc-decls))
				     (arguments c))
			   (list (recognizer c))))
	       (constructors adt))
	     dparams)))))

(defun get-accessor-domain-type* (subtypes recognizers dparams)
  (cond ((and (singleton? subtypes)
	      (null recognizers))
	 (car subtypes))
	((and (singleton? recognizers)
	      (null subtypes))
	 (let ((*generate-tccs* 'none)
	       (dacts (mk-dactuals dparams))
	       (recname (mk-name-expr (car recognizers))))
	   (setf (dactuals recname) dacts)
	   (with-bound-declparams dparams
	     (typecheck* (mk-expr-as-type recname) nil nil nil))))
	(t
	 (let* ((*generate-tccs* 'none)
		(preds (append subtypes recognizers))
		(var (make-new-variable '|x| preds))
		(bd (mk-bind-decl var (adt-type-name *adt*)))
		(*bound-variables* (cons bd *bound-variables*))
		(appreds (mapcar #'(lambda (p)
				     (if (subtype? p)
					 (mk-application (id (print-type p))
					   (mk-name-expr var))
					 (mk-application p
					   (mk-name-expr var))))
			   preds))
		(pred (mk-lambda-expr (list bd) (mk-disjunction appreds)))
		(subtype (mk-setsubtype (adt-type-name *adt*) pred)))
	   (typecheck* subtype nil nil nil)))))

(defun get-accessor-covered-subtypes (accs adt &optional subtypes recs)
  (if (null accs)
      (values (nreverse subtypes) (nreverse recs))
      (let ((constr (find (car accs) (constructors adt)
			  :key #'arguments :test #'memq)))
	(assert constr)
	(multiple-value-bind (subtype-accs subtype-recs covers?)
	    (get-accessors-for-subtype
	     (car accs) (subtype constr) (constructors adt))
	  (declare (ignore subtype-recs))
	  ;; subtype-accs has all accessors of that name for the given subtype
	  ;; covers? is true if every constructor of that subtype has an
	  ;; accessor of that name
	  (multiple-value-bind (in-accs out-accs)
	      (split-on #'(lambda (acc) (memq acc subtype-accs)) accs)
	    (if covers?
		;; We got it covered - can use the subtype
		(get-accessor-covered-subtypes
		 out-accs
		 adt
		 (cons (type-value (declaration (subtype constr))) subtypes)
		 recs)
		(get-accessor-covered-subtypes
		 out-accs
		 adt
		 subtypes
		 (append recs
			 (mapcan #'(lambda (c)
				     (when (some #'(lambda (acc)
						     (memq acc in-accs))
						 (arguments c))
				       (list (recognizer c))))
			   (constructors adt))))))))))

(defun get-accessors-for-subtype (acc subtype constructors
				      &optional accs recs (covered? t))
  (if (null constructors)
      (values (nreverse accs) (nreverse recs) covered?)
      (if (same-id (subtype (car constructors)) subtype)
	  (let ((sim-acc (find acc (arguments (car constructors))
			       :test #'same-id)))
	    (if sim-acc
		(get-accessors-for-subtype
		 acc subtype (cdr constructors)
		 (cons sim-acc accs)
		 (cons (recognizer (car constructors)) recs)
		 covered?)
		(get-accessors-for-subtype acc subtype (cdr constructors)
					   accs recs nil)))
	  (get-accessors-for-subtype acc subtype (cdr constructors)
				     accs recs covered?))))

(defun get-accessor-range-type (entry domain adt)
  (assert (or (null (cdr entry)) (dep-binding? domain)))
  (if (cdr entry)
      (let* (;;(dtype (if (dep-binding? domain) (type domain) domain))
	     (dvar (when (dep-binding? domain)
		     (make-variable-expr domain)))
	     (suptype (reduce #'compatible-type (mapcar #'cadar entry)))
	     (tid (make-new-variable '|x| adt))
	     (tbd (make-bind-decl tid suptype))
	     (tvar (make-variable-expr tbd))
	     (*bound-variables* (cons tbd *bound-variables*))
	     (pred (get-accessor-range-type-pred entry dvar tvar adt)))
	(mk-subtype suptype
	  (make-instance 'set-expr
	    :bindings (list tbd)
	    :expression pred)))
      (cadaar entry)))

(defun get-accessor-range-type-pred (entry dvar tvar adt)
  (let* ((vars (caddr (caar entry)))
	 (subst-alist
	      (pairlis
	       (mapcar #'declaration vars)
	       (mapcar #'(lambda (dep)
			   (typecheck* (mk-application (id dep) dvar)
				       (type dep) nil nil))
		 vars)))
	 (epred (subtype-pred (substit (cadaar entry) subst-alist)
			      (type tvar)))
	 (tpred (make!-application epred tvar)))
    (if (null (cdr entry))
	tpred
	(let* ((adtype (get-accessor-domain-type (list (car entry)) adt))
	       (dtype (if (dep-binding? adtype) (type adtype) adtype))
	       (pred (predicate dtype))
	       (cond (make!-application pred dvar))
	       (else (get-accessor-range-type-pred
		      (cdr entry) dvar tvar adt)))
	  (make!-if-expr cond tpred else)))))
  

;;; The common-accessors list is built from entries of the form
;;; ((acc-decl type freevars) acc-decl ...)
;;; which reflects accessors with the same identifier and type as the caar
;;; Lists of these are made from accessors with the same id and different,
;;; but compatible, types.
;;; Finally, lists of these are made for each accessor id, resulting in
;;;  ( (((acc-decl type freevars) acc-decl ...)
;;;     ((acc-decl type freevars) acc-decl ...))
;;;    (((acc-decl type freevars) acc-decl ...)) ... )
(defun collect-common-accessors (adt)
  (let ((common-accessors nil))
    (dolist (c (constructors adt))
      (let ((*bound-variables* nil))
	(dolist (a (arguments c))
	  (let* ((dparams (new-decl-formals adt))
		 (dacts (mk-dactuals dparams))
		 ;; Create the incomplete decl, to set (current-declaration)
		 ;; so that dparams can be found properly
		 (adecl (mk-adt-accessor-decl (id a) nil adt nil dparams))
		 (atype (subst-adt-decl-actuals (declared-type a) dacts)))
	    (with-added-decls dparams
	      (init-adt-decl adecl)
	      (assert (current-declaration))
	      (let ((type (typecheck atype)))
		(assert type)
		(assert (fully-instantiated? type))
		(when (and (type-name? type)
			   (not (fully-instantiated? (dactuals type))))
		  (break "Not insted"))
		;;(copy-lex type-copy (declared-type a))
		;;(copy-lex type (declared-type a))
		(when (and (type-name? type)
			   (not (fully-instantiated? (dactuals type))))
		  (break "Not insted 2"))
		(push (typecheck* (mk-dep-binding (id a) type) nil nil nil)
		      *bound-variables*)
		(let* ((accinfo (list a type (sort-freevars (freevars type)) adecl))
		       (entry (assoc accinfo common-accessors
				     :test #'compatible-accinfo
				     :key #'car)))
		  (if entry
		      (let ((subentry (assoc accinfo entry
					     :test #'same-accinfo)))
			(if subentry
			    (nconc subentry (list a))
			    (nconc entry (list (list accinfo a)))))
		      (setq common-accessors
			    (nconc common-accessors
				   (list (list (list accinfo a)))))))))))))
    common-accessors))

;;; accinfo is (accessor type freevars declparams)
(defun compatible-accinfo (ai1 ai2)
  (and (same-id (car ai1) (car ai2))
       (progn (when (decl-formals *adt*) (break "compatible-accinfo")) t)
       (multiple-value-bind (bindings mismatch?)
	   (collect-same-accinfo-bindings (caddr ai1) (caddr ai2))
	 (unless mismatch?
	   (strict-compatible?* (cadr ai1) (cadr ai2) bindings)))))

(defun same-accinfo (ai1 ai2)
  (and (same-id (car ai1) (car ai2))
       (progn (when (decl-formals *adt*) (break "same-accinfo")) t)
       (let ((bindings (collect-same-accinfo-bindings
			(caddr ai1) (caddr ai2))))
	 (tc-eq-with-bindings (cadr ai1) (cadr ai2) bindings))))

(defun collect-same-accinfo-bindings (deps1 deps2 &optional bindings)
  ;; bindings are sorted already
  (if (null deps1)
      (if (null deps2)
	  bindings
	  (values nil t))
      (if (and deps2
	       (same-id (car deps1) (car deps2))
	       (tc-eq-with-bindings (type (car deps1)) (type (car deps2))
				    bindings))
	  (collect-same-accinfo-bindings
	   (cdr deps1) (cdr deps2)
	   (acons (declaration (car deps1)) (declaration (car deps2))
		  bindings))
	  (values nil t))))


(defun mk-recognizer-type (rec-id dacts)
  (let ((rpred (mk-name-expr rec-id)))
    (setf (dactuals rpred) dacts)
    (mk-expr-as-type rpred)))

;;; Ord axiom

(defun generate-adt-uninterpreted-ord-function-and-axiom (adt)
  (generate-adt-uninterpreted-ord-function adt)
  (generate-adt-uninterpreted-ord-axiom adt))

(defun generate-adt-uninterpreted-ord-function (adt)
  (multiple-value-bind (dparams dacts thinst tn)
      (new-decl-formals adt)
    (declare (ignore dacts thinst))
    (let* ((len (length (constructors adt)))
	   (fname (makesym "~a_ord" (id adt)))
	   (ftype (mk-funtype tn
			      (make-instance 'type-application
				:type (mk-type-name '|upto|)
				:parameters (list (mk-number-expr (1- len))))))
	   (cdecl (mk-const-decl fname ftype nil nil nil dparams)))
      (setf (type cdecl) nil)
      (typecheck-adt-decl cdecl))))

(defun generate-adt-uninterpreted-ord-axiom (adt)
  (let* ((dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (fname (mk-name-expr (makesym "~a_ord" (id adt)))))
    (setf (dactuals fname) dacts)
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_ord_defaxiom" (id adt))
       (parse-unparse
	(mk-conjunction
	 (generate-adt-uninterpreted-ord-axiom-conjuncts
	  (constructors adt) fname 0 dacts)))
       'AXIOM nil dparams))))

(defun generate-adt-uninterpreted-ord-axiom-conjuncts (constrs fname num dacts
							       &optional conjs)
  (if (null constrs)
      (nreverse conjs)
      (generate-adt-uninterpreted-ord-axiom-conjuncts
       (cdr constrs) fname (1+ num) dacts
       (let ((conj (generate-adt-uninterpreted-ord-conjunct
		    (car constrs) fname num dacts)))
	 (cons conj conjs)))))

(defun generate-adt-uninterpreted-ord-conjunct (c fname num dacts)
  (if (arguments c)
      (let* ((bindings (mapcar #'bind-decl (arguments c)))
	     (vars (mapcar #'mk-name-expr bindings))
	     (cname (mk-name-expr c)))
	(setf (dactuals cname) dacts)
	(mk-forall-expr (mapcar #'bind-decl (arguments c))
	  (mk-application '=
	    (mk-application fname
	      (mk-application* cname vars))
	    (mk-number-expr num))))
      (let ((cname (mk-name-expr c)))
	(setf (dactuals cname) dacts)
	(mk-application '=
	  (mk-application fname cname)
	  (mk-number-expr num)))))

;;; Ord function

(defun generate-adt-ord-function (adt)
  (let* ((var (make-new-variable '|x| adt))
	 (len (length (constructors adt)))
	 (dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (*generate-tccs* 'none))
    (typecheck-adt-decl
     (mk-const-decl '|ord|
       (make-instance 'type-application
	 :type (mk-type-name '|upto|)
	 :parameters (list (mk-number-expr (1- len))))
       (when (and *generate-all-adt-axioms*
		  (<= len *ord-definition-cutoff*))
	 (parse-unparse (mk-cases-expr (mk-name-expr var)
			  (generate-adt-ord-selections (constructors adt))
			  nil)))
       (list (mk-arg-bind-decl var
			       (mk-type-name (id adt)
				 nil nil nil nil nil nil dacts)))
       nil dparams))))

(defun set-constructor-ord-numbers (constructors &optional (num 0))
  (when constructors
    (setf (ordnum (car constructors)) num)
    (set-constructor-ord-numbers (cdr constructors) (1+ num))))

(defun generate-adt-ord-selections (constructors &optional result)
  (if (null constructors)
      (nreverse result)
      (let ((sel (generate-adt-ord-selection (car constructors))))
	(generate-adt-ord-selections (cdr constructors)
				     (cons sel result)))))

(defun generate-adt-ord-selection (c)
  (if (arguments c)
      (mk-selection (mk-name-expr (id c))
	(mapcar #'(lambda (a) (change-class (copy (get-adt-var a)) 'bind-decl))
	  (arguments c))
	(mk-number-expr (ordnum c)))
      (mk-selection (mk-name-expr (id c)) nil
		      (mk-number-expr (ordnum c)))))


;;; Done generating the recognizer, constructor, and accessor declarations,
;;; now check the types

(defun check-adt-types (adt)
  (let ((adt-type (adt-type-name adt)))
    (check-adt-constructor-types (constructors adt))
    (dolist (c (constructors adt))
      (check-adt-type-occurrences c adt-type))))


;;; This actually typechecks the accessor types, creating bind-decls and
;;; storing them in the accessor-bindings slot of each constructor.

(defun check-adt-constructor-types (constructors)
  (when constructors
    (check-adt-accessor-types (arguments (car constructors)))
    (check-adt-constructor-types (cdr constructors))))

(defun check-adt-accessor-types (accessors &optional result)
  (if (null accessors)
      (nreverse result)
      (let* ((a (car accessors))
	     (dparams (decl-formals (accessor-decl a)))
	     (dacts (mk-dactuals dparams))
	     (dtype (subst-adt-decl-actuals (declared-type a) dacts))
	     (bd (mk-bind-decl (id a) (parse-unparse dtype 'type-expr)))
	     (*adt-decl* (declared-type a)))
	(copy-lex (declared-type bd) (declared-type a))
	(with-added-decls (decl-formals (accessor-decl a))
	  (let ((*decl-bound-parameters* dparams))
	    (typecheck* bd nil nil nil)))
	(setf (type a) (type bd)
	      (bind-decl a) bd)
	(let ((*bound-variables* (cons bd *bound-variables*)))
	  (check-adt-accessor-types (cdr accessors) (cons bd result))))))

(defun check-adt-type-occurrences (c adt-type)
  (mapc #'(lambda (a)
	    (check-adt-type-occurrences* a adt-type))
	(arguments c)))

(defun check-adt-type-occurrences* (arg adt-type)
  (let ((atype (type arg)))
    (multiple-value-bind (pos? negocc)
	(occurs-positively? adt-type atype)
      (unless pos?
	(when (and negocc
		   (not (place negocc)))
	  (multiple-value-bind (pos2? negocc2)
	      (occurs-positively? adt-type (declared-type arg))
	    (declare (ignore pos2?))
	    (when (and negocc2 (place negocc2))
	      (setq negocc negocc2))))
	(let ((*current-theory* *adt*))
	  (setf (filename *current-theory*) *current-file*)
	  (type-error (or negocc (declared-type arg))
	    "Recursive uses of the datatype ~a may not appear in:~%  ~
             the domain of a function type,~%  ~
             as a non-positive parameter to another datatype,~%  ~
             or in the predicate of a subtype."
	    (id adt-type)))))))

(defun sequence-adt? (adt-type arg-type)
  (let ((atype (find-supertype arg-type)))
    (and (typep atype 'funtype)
	 (tc-eq (domain atype) *naturalnumber*)
	 (tc-eq (range atype) adt-type))))

(defun datatype-adt? (adt-type arg-type &optional poscheck?)
  (let ((atype (find-supertype arg-type)))
    (and (typep atype 'type-name)
	 (let* ((mi (module-instance (resolution atype)))
		(th (if (same-id mi (current-theory))
			(current-theory)
			(get-theory mi))))
	   (assert th)
	   (and (not (eq th (current-theory)))
		(generated-by th)
		(typep (get-theory (generated-by th)) 'recursive-type)
		(if poscheck?
		    (pos-datatype-adt? adt-type (actuals mi)
				       (get-theory (generated-by th)))
		    (member adt-type (actuals mi)
			    :test #'(lambda (aty act)
				      (and (type-value act)
					   (tc-eq (type-value act)
						  aty))))))))))

(defun pos-datatype-adt? (adt-type actuals adt)
  (pos-datype-adt?* adt-type actuals (formals-sans-usings adt) adt))

(defun pos-datype-adt?* (adt-type acts formals adt)
  (or (null acts)
      (and (or (not (occurs-in adt-type (car acts)))
	       (and (tc-eq (type-value (car acts)) adt-type)
		    (member (car formals) (positive-types adt)
			    :test #'(lambda (x y)
				      (let ((fdecl (if (name? y)
						       (declaration y)
						       (declaration (print-type y)))))
					(and (typep fdecl 'formal-type-decl)
					     (same-id x fdecl))))))
	       (and (typep (find-supertype (type-value (car acts))) 'type-name)
		    (pos-datatype-adt?
		     adt-type
		     (actuals (find-supertype (type-value (car acts))))
		     adt)))
	   (pos-datype-adt?* adt-type (cdr acts) (cdr formals) adt))))

(defun all-adt-types-are-positive (adt)
  (every #'(lambda (fml)
	     (or (not (typep fml 'type-decl))
		 (member fml (positive-types adt)
			 :test #'(lambda (x y)
				   (tc-eq (type-value x) y)))))
	 (formals (adt-theory adt))))

(defun some-adt-type-is-positive (adt)
  (some #'(lambda (fml)
	    (and (typep fml 'type-decl)
		 (member fml (positive-types adt)
			 :test #'(lambda (x y)
				   (tc-eq (type-value x) y)))))
	 (formals (adt-theory adt))))

(defun generate-adt-axioms (adt)
  (generate-adt-extensionalities (constructors adt) adt)
  (generate-adt-accessor-axioms (constructors adt) adt)
  (generate-inclusive-axiom adt)
  (generate-disjoint-axiom adt))

(defun generate-adt-extensionalities (constructors adt)
  (when constructors
    (generate-adt-extensionality-axioms (car constructors) adt)
    (generate-adt-extensionalities (cdr constructors) adt)))

(defun generate-adt-extensionality-axioms (c adt)
  (generate-adt-extensionality c adt)
  (when (arguments c)
    (generate-adt-eta c adt)))

(defun generate-adt-extensionality (c adt)
  (let* ((var (mk-name-expr (makesym "~a_var" (op-to-id (recognizer c)))))
	 (var2 (copy var 'id (makesym "~a2" (id var))))
	 (dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (*generate-tccs* 'none))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_~a_extensionality"
			       (id adt) (op-to-id (id c)))
       (mk-forall-expr (list (mk-bind-decl (id var)
			       (mk-recognizer-type (recognizer c) dacts))
			     (mk-bind-decl (id var2)
			       (mk-recognizer-type (recognizer c) dacts)))
	 (if (arguments c)
	     (mk-application 'IMPLIES
	       (mk-conjunction
		(mapcar #'(lambda (a)
			    (mk-application '=
			      (if (some #'(lambda (cc)
					    (eq (recognizer cc) (id a)))
					(constructors adt))
				  (mk-coercion (mk-application (id a) var)
					       (type a))
				  (mk-application (id a) var))
			      (mk-application (id a) var2)))
		  (arguments c)))
	       (mk-application '= var var2))
	     (mk-application '= var var2)))
       'AXIOM nil dparams))))

(defun generate-adt-eta (c adt)
  (let* ((var (mk-name-expr (makesym "~a_var" (op-to-id (recognizer c)))))
	 (dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (*generate-tccs* 'none))
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_~a_eta" (id adt) (op-to-id (id c)))
       (mk-forall-expr (list (mk-bind-decl (id var)
			       (mk-recognizer-type (recognizer c) dacts)))
	 (mk-application '=
	   (apply #'mk-application (id c)
		  (mapcar #'(lambda (a)
			      (mk-application (id a) var))
			  (arguments c)))
	   var))
       'AXIOM nil dparams))))

(defun generate-adt-accessor-axioms (constructors adt)
  (when constructors
    (when (arguments (car constructors))
      (generate-accessor-axioms (car constructors) adt))
    (generate-adt-accessor-axioms (cdr constructors) adt)))

(defun generate-accessor-axioms (c adt)
  (let ((bds (mapcar #'bind-decl (arguments c))))
    (dolist (a (arguments c))
      (let* ((vars (mapcar #'get-adt-var (arguments c)))
	     (cappl (mk-application* (id c) vars))
	     (consappl (if (> (count-if #'(lambda (d)
					    (eq (module d) (current-theory)))
					(get-declarations (id c)))
			      1)
			   (make-instance 'coercion
			     :operator (mk-lambda-expr
					   (list (mk-bind-decl '|x|
						   (mk-expr-as-type
						    (mk-name-expr
							(recognizer c)))))
					 (mk-name-expr '|x|))
			     :argument cappl)
			   cappl))
	     (dparams (new-decl-formals* adt))
	     (*generate-tccs* 'none))
	(typecheck-adt-decl
	 (mk-formula-decl (makesym "~a_~a_~a"
				   (id adt)
				   (op-to-id (id a))
				   (op-to-id (id c)))
	   (parse-unparse
	    (mk-forall-expr (adt-forall-bindings vars bds)
	      (mk-application '=
		(mk-application (id a) consappl)
		(get-adt-var a))))
	   'AXIOM nil dparams)
	 t nil)))))

(defun adt-forall-bindings (vars bds &optional result)
  (if (null vars)
      (nreverse result)
      (let* ((nbd (mk-bind-decl (id (car vars))
		   (declared-type (car bds)) (type (car bds))))
	     (var (mk-name-expr (id (car vars)) nil nil
				(make-resolution nbd
				  (current-theory-name) (type nbd)))))
	(adt-forall-bindings (cdr vars)
			     (substit (cdr bds)
			       (acons (car bds) var nil))
			     (cons nbd result)))))

(defun generate-disjoint-axiom (adt)
  (declare (ignore adt))
  ;; We no longer generate the disjointness axiom because it can get large
  ;; and significantly slow down typechecking.  For a datatype with n
  ;; constructors, it would need to generate choose(n, 2) = n!/(n-2)!2!
  ;; disequalities.  The ord function axiom uses the fact that natural
  ;; numbers are distinct, and hence provides this.
  )
  
;;(defparameter *disjoint-axiom-expansion-maxsize* 100)

;; (defun generate-disjoint-axiom (adt)
;;   (let* ((varid (makesym "~a_var" (id adt)))
;; 	 (bd (make-bind-decl varid (adt-type-name adt)))
;; 	 (var (make-variable-expr bd))
;; 	 (*generate-tccs* 'none)
;; 	 (appls (mapcar #'(lambda (c)
;; 			    (let ((recname
;; 				   (mk-name-expr (recognizer c)
;; 				     nil nil
;; 				     (make-resolution (rec-decl c)
;; 				       (current-theory-name)
;; 				       (type (rec-decl c))))))
;; 			      (make!-application recname var)))
;; 		  (constructors adt))))
;;     (typecheck-adt-decl
;;      (mk-formula-decl (makesym "~a_disjoint" (id adt))
;;        (make!-forall-expr (list bd)
;; 	 (if (<= (length appls) *disjoint-axiom-expansion-maxsize*)
;; 	     (make!-conjunction* (make-disjoint-pairs appls))
;; 	     (let ((list-type (typecheck* (pc-parse "list[bool]" 'type-expr)
;; 					  nil nil nil)))
;; 	       (make-application (mk-name-expr '|pairwise_disjoint?|)
;; 		 (make-list-expr appls list-type)))))
;;        'AXIOM))))

(defun make-disjoint-pairs (appls &optional result)
  (if (null (cdr appls))
      result
      (make-disjoint-pairs
       (cdr appls)
       (nconc result
	      (mapcar #'(lambda (a)
			   (make!-negation
			    (make!-conjunction (car appls) a)))
		       (cdr appls))))))

(defun generate-inclusive-axiom (adt)
  (let* ((tvar (mk-name-expr (makesym "~a_var" (id adt))))
	 (dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (tname (mk-type-name (id adt))))
    (setf (dactuals tname) dacts)
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_inclusive" (id adt))
       (mk-forall-expr (list (mk-bind-decl (id tvar) tname))
	 (mk-disjunction
	  (mapcar #'(lambda (c) (mk-application (recognizer c) tvar))
		  (constructors adt))))
       'AXIOM nil dparams))))

(defun generate-adt-induction (adt)
  (let* ((indvar (make-new-variable '|p| adt))
	 (dparams (new-decl-formals* adt))
	 (dacts (mk-dactuals dparams))
	 (tname (mk-type-name (id adt))))
    (setf (dactuals tname) dacts)
    (typecheck-adt-decl
     (mk-formula-decl (makesym "~a_induction" (id adt))
       (mk-forall-expr (list (mk-bind-decl indvar (mk-predtype tname)))
	 (mk-implication (gen-induction-hypothesis adt indvar)
			 (gen-induction-conclusion adt indvar tname)))
       'AXIOM nil dparams))))

(defmethod gen-induction-hypothesis ((adt datatype) indvar &optional ign)
  (declare (ignore ign))
  (mk-conjunction
   (mapcar #'(lambda (c) (gen-induction-hypothesis c indvar adt))
	   (constructors adt))))

(defmethod gen-induction-hypothesis ((c simple-constructor) indvar
				     &optional adt)
  (if (arguments c)
      (let* ((subst-alist (adt-subst-alist (arguments c)))
	     (arghyps (acc-induction-hypotheses (arguments c) indvar adt
						subst-alist))
	     (ppappl (mk-application indvar
		       (mk-application* (id c)
			 (mapcar #'get-adt-var-name (arguments c)))))
	     (indh (mk-forall-expr (adt-forall-bindings
				    (mapcar #'get-adt-var-name (arguments c))
				    (mapcar #'bind-decl (arguments c)))
		     (if arghyps
			 (mk-implication (mk-conjunction arghyps) ppappl)
			 ppappl))))
	(pc-parse (unparse indh :string t) 'expr))
      (mk-application indvar (mk-name-expr (id c)))))

(defun adt-subst-alist (args)
  (adt-subst-alist* (mapcar #'get-adt-var-name args)
		    (mapcar #'bind-decl args)
		    (mapcar #'bind-decl args)))

(defun adt-subst-alist* (vars bds sbds &optional result)
  (if (null vars)
      (nreverse result)
      (let* ((nbd (mk-bind-decl (id (car vars))
		   (declared-type (car sbds)) (type (car sbds))))
	     (var (mk-name-expr (id (car vars)) nil nil
				(make-resolution nbd
				  (current-theory-name) (type nbd)))))
	(adt-subst-alist* (cdr vars)
			  (cdr bds)
			  (substit (cdr sbds)
			    (acons (car sbds) var nil))
			  (acons (car bds) var result)))))

(defun get-adt-var-name (arg)
  (make-instance 'name-expr :id (id (get-adt-var arg))))

(defun acc-induction-hypotheses (args indvar adt subst-alist &optional result)
  (if (null args)
      (nreverse result)
      (let ((hyp (acc-induction-hypothesis (car args) indvar adt subst-alist)))
	(acc-induction-hypotheses (cdr args) indvar adt subst-alist
				  (if hyp (cons hyp result) result)))))


;;; Generate the induction hypothesis according to the type of the argument.
;;; It is either the datatype itself, a sequence of the datatype, or occurs as
;;; a positive actual parameter to a different datatype.  In every other case,
;;; return NIL.

(defun acc-induction-hypothesis (arg indvar adt subst-alist)
  (let ((pred (acc-induction-hypothesis* (substit (type arg) subst-alist)
					 indvar adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (mk-application pred (get-adt-var-name arg)))))

(defmethod acc-induction-hypothesis* ((te type-name) indvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (mk-name-expr indvar))
	((adt? te)
	 (let* ((acts (actuals-corresponding-to-positive-types
		       (actuals (module-instance te))
		       (adt? te)))
		(preds (mapcar #'(lambda (act)
				   (acc-induction-hypothesis*
				    (type-value act) indvar adt))
			       acts)))
	   (if (every #'(lambda (p) (or (everywhere-true? p) (null p)))
		      preds)
	       (mk-everywhere-true-function te)
	       (mk-application* '|every|
		 (mapcar #'(lambda (p a)
			     (or p
				 (mk-everywhere-true-function (type-value a))))
		   preds acts)))))
	(t ;;(mk-everywhere-true-function te)
	 nil)))

(defun actuals-corresponding-to-positive-types (acts adt)
  (actuals-corresponding-to-positive-types*
   acts (formals-sans-usings adt) (positive-types adt)))

(defun actuals-corresponding-to-positive-types* (acts formals postypes
						      &optional posacts)
  (if (null acts)
      (nreverse posacts)
      (actuals-corresponding-to-positive-types*
       (cdr acts)
       (cdr formals)
       postypes
       (if (member (car formals) postypes :test #'same-id)
	   (cons (car acts) posacts)
	   posacts))))

(defmethod acc-induction-hypothesis* ((te datatype-subtype) indvar adt)
  (acc-induction-hypothesis* (declared-type te)  indvar adt))

(defmethod acc-induction-hypothesis* ((te subtype) indvar adt)
  (if (finite-set-type? te)
      (let* ((dom (domain (supertype te)))
	     (pred (acc-induction-hypothesis* dom indvar adt)))
	(if (or (everywhere-true? pred) (null pred))
	    (mk-everywhere-true-function te)
	    (mk-application '|every| pred)))
      (acc-induction-hypothesis* (supertype te) indvar adt)))

(defmethod acc-induction-hypothesis* ((te funtype) indvar adt)
  (let* ((fid (make-new-variable '|x| te))
	 (fbd (make-bind-decl fid (domain te)))
	 (fvar (mk-name-expr fid nil nil
			     (make-resolution fbd
			       (current-theory-name) (domain te))))
	 (*bound-variables* (cons fbd *bound-variables*))
	 (pred (acc-induction-hypothesis*
		(if (typep (domain te) 'dep-binding)
		    (substit (range te) (acons (domain te) fvar nil))
		    (range te))
		indvar adt)))
    (when (and pred
	       (not (everywhere-true? pred)))
      (if (tc-eq (domain te) *naturalnumber*)
	  (mk-application '|every| pred)
	  (let* ((lid (make-new-variable '|f| (list te pred)))
		 (lbd (make-bind-decl lid te))
		 (lvar (mk-name-expr lid nil nil
				     (make-resolution lbd
				       (current-theory-name) te))))
	    (mk-lambda-expr (list lbd)
	      (mk-forall-expr (list fbd)
		(mk-application pred
		  (mk-application lvar fvar)))))))))

(defmethod acc-induction-hypothesis* ((te recordtype) indvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons rbd *bound-variables*))
	 (preds (acc-induction-fields (fields te) rvar indvar adt
				      (dependent? te))))
    (unless (every #'(lambda (p) (or (null p) (everywhere-true? p))) preds)
      (mk-lambda-expr (list rbd)
	(mk-conjunction
	 (mapcan #'(lambda (fd pred)
		     (when pred
		       (list (mk-application pred
			       (make-field-application (id fd) rvar)))))
	   (fields te) preds))))))

(defun acc-induction-fields (fields rvar indvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-induction-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar indvar adt dep?
       (cons (acc-induction-hypothesis* (type (car fields))
					indvar adt)
	     result))))

(defmethod acc-induction-hypothesis* ((te tupletype) indvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (preds (acc-induction-tuples (types te) tvar indvar adt)))
    (unless (every #'null preds)
      (mk-lambda-expr (list tbd)
	(mk-conjunction
	 (let ((num 0))
	   (mapcan #'(lambda (pred)
		       (incf num)
		       (when pred
			 (list (mk-application pred
				 (make-instance 'projappl
				   :id (makesym "PROJ_~d" num)
				   :index num
				   :argument tvar)))))
	     preds)))))))

(defun acc-induction-tuples (types tvar indvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-induction-tuples
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       tvar indvar adt (1+ index)
       (cons (acc-induction-hypothesis* (car types) indvar adt) result))))

(defmethod acc-induction-hypothesis* ((te cotupletype) indvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (preds (acc-induction-tuples (types te) tvar indvar adt)))
    (unless (every #'null preds)
      (mk-lambda-expr (list tbd)
	(let ((num 0)
	      (varid (make-new-variable '|x| adt)))
	  (mk-cases-expr tvar
	    (mapcar #'(lambda (pred type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  :id (makesym "IN_~d" num)
					  :index num))
			       (sel-expr (if pred
					     (mk-application pred var)
					     *true*)))
			  (mk-selection in-expr (list bd) sel-expr)))
	      preds (types te))
	    nil))))))

(defmethod acc-induction-hypothesis* ((te dep-binding) indvar adt)
  (acc-induction-hypothesis* (type te) indvar adt))

(defmethod acc-induction-hypothesis* ((te type-expr) indvar adt)
  (declare (ignore indvar adt))
  ;;(mk-everywhere-true-function te)
  )

(defun mk-everywhere-true-function (type)
  (make!-lambda-expr
      (list (mk-bind-decl (make-new-variable '|x| type) type type))
    (copy *true*)))

(defun mk-everywhere-false-function (type)
  (make!-lambda-expr
      (list (mk-bind-decl (make-new-variable '|x| type) type type))
    (copy *false*)))

(defmethod gen-induction-conclusion ((adt datatype) indvar tname)
  (let ((tvar (mk-name-expr (makesym "~a_var" (id adt)))))
    (mk-forall-expr (list (mk-bind-decl (id tvar) tname))
      (mk-application indvar tvar))))


;;; Generate the every and some functions - only when some formal
;;; parameter occurs positively.  This means either no occurrence, direct
;;; occurrence, or a subtype of one of these two cases.

(defun generate-adt-every (adt)
  (generate-adt-predicate adt '|every|))

(defun generate-adt-some (adt)
  (generate-adt-predicate adt '|some|))

(defun generate-adt-predicate (adt function-id)
  (multiple-value-bind (dparams1 dacts1 thinst1 atype1)
      (new-decl-formals adt)
    (declare (ignore dacts1))
    (multiple-value-bind (dparams2 dacts2 thinst2 atype2)
	(new-decl-formals adt)
      (declare (ignore dacts2))
      (let* ((avar (mk-name-expr (make-new-variable '|a| adt)))
	     (pvars (generate-adt-predicate-variables adt)) ;; Purely syntactic
	     (ptypes1 (subst-adt-type (positive-types adt) thinst1 adt))
	     (cases1 (mk-cases-expr avar
		       (mapcar #'(lambda (c)
				   (generate-adt-predicate-selection
				    c pvars ptypes1 adt function-id thinst1 t))
			 (constructors adt))
		       nil))
	     (ptypes2 (subst-adt-type (positive-types adt) thinst2 adt))
	     (cases2 (when (datatype? adt)
		       (mk-cases-expr avar
			 (mapcar #'(lambda (c)
				     (generate-adt-predicate-selection
				      c pvars ptypes2 adt function-id thinst2 nil))
			   (constructors adt))
			 nil)))
	     (fargs1 (list (mapcar #'mk-arg-bind-decl
			     (mapcar #'id pvars)
			     (mapcar #'(lambda (pty)
					 (mk-type-name 'PRED
					   (list (mk-actual pty))))
			       ptypes1))
			   (list (mk-arg-bind-decl (id avar) atype1))))
	     (fargs2 (when (datatype? adt)
		       (list (mapcar #'mk-arg-bind-decl
			       (mapcar #'id pvars)
			       (mapcar #'(lambda (pty)
					   (mk-type-name 'PRED
					     (list (mk-actual pty))))
				 ptypes2))
			     (list (mk-arg-bind-decl (id avar) atype2))))))
	;; Curried form
	(typecheck-adt-decl
	 (if (datatype? adt)
	     (mk-adt-def-decl function-id (copy *boolean*)
			      (parse-unparse cases1) fargs1 nil nil dparams1)
	     (if (eq function-id '|every|)
		 (mk-coinductive-decl function-id (copy *boolean*)
				      (pc-parse (unparse cases1 :string t) 'expr)
				      fargs1)
		 (mk-inductive-decl function-id (copy *boolean*)
				    (pc-parse (unparse cases1 :string t) 'expr)
				    fargs1))))
	;; Uncurried form
	(when (datatype? adt)
	  (typecheck-adt-decl
	   (mk-adt-def-decl function-id (copy *boolean*)
			    (parse-unparse cases2)
			    (append (apply #'append (butlast fargs2))
				    (car (last fargs2)))
			    nil nil dparams2)))))))

(defun generate-adt-predicate-variables (adt)
  (let ((ptypes (positive-types adt)))
    (if (singleton? ptypes)
	(list (mk-name-expr (make-new-variable '|p| adt)))
	(let ((ctr 0))
	  (mapcar #'(lambda (p)
		      (declare (ignore p))
		      (mk-name-expr (make-new-variable '|p| adt (incf ctr))))
		  ptypes)))))

(defun make-accessor-bind-decls (accs thinst &optional bdecls)
  (if (null accs)
      (nreverse bdecls)
      (make-accessor-bind-decls
       (cdr accs) thinst
       (if thinst
	   (let* ((adecl (accessor-decl (car accs)))
		  (atype (subst-mod-params (type (car accs))
					   thinst (current-theory) adecl)))
	     (make-bind-decl (id (get-adt-var-name (car accs))) atype))
	   (make-bind-decl (id (get-adt-var-name (car accs))) (type (car accs)))))))
  

(defun generate-adt-predicate-selection (c pvars ptypes adt function-id
					   thinst curried?)
  (if (arguments c)
      (let* ((bindings (mapcar #'(lambda (a)
				   (make-bind-decl (id (get-adt-var-name a))
				     (subst-mod-params (type a)
					 thinst (current-theory) (accessor-decl a))))
			 (arguments c)))
	     (vars (mapcar #'(lambda (b)
			       (mk-name-expr (id b) nil nil
					     (make-resolution b
					       (current-theory-name)
					       (type b))))
		     bindings)))
	(mk-selection (mk-name-expr (id c))
	  bindings
	  (let ((exprs (acc-predicate-selections
			(arguments c) vars pvars ptypes thinst adt
			function-id curried?)))
	    (if (eq function-id '|every|)
		(mk-conjunction exprs)
		(mk-disjunction exprs)))))
      (mk-selection (mk-name-expr (id c)) nil
		    (copy (if (eq function-id '|every|) *true* *false*)))))

(defun acc-predicate-selections (args vars pvars ptypes thinst adt function-id
				      curried? &optional selections)
  (if (null vars)
      (nreverse selections)
      (let ((sel (acc-predicate-selection (car vars) (type (car vars))
					  pvars ptypes thinst adt
					  function-id curried?))
	    (nvars (substit (cdr vars)
		     (acons (bind-decl (car args)) (car vars) nil))))
	(acc-predicate-selections (cdr args) nvars pvars ptypes thinst adt
				  function-id curried?
				  (cons sel selections)))))


(defmethod acc-predicate-selection (arg (te type-name) pvars ptypes thinst adt
					funid curried?)
  (cond ((member te ptypes :test #'tc-eq)
	 (let* ((pos (position te ptypes :test #'tc-eq))
		(pvar (nth pos pvars)))
	   (mk-application pvar (copy arg))))
	((tc-eq te (subst-adt-type (adt-type-name adt) thinst adt))
	 (if curried?
	     (mk-application (mk-application* funid pvars) arg)
	     (mk-application* funid (append pvars (list (copy arg))))))
	((adt? te)
	 (let ((funs (mapcar #'(lambda (act)
				 (acc-predicate-selection*
				  (type-value act) pvars ptypes thinst adt funid))
			     (positive-actuals te))))
	   (if (if (eq funid '|every|)
		   (every #'everywhere-true? funs)
		   (every #'everywhere-false? funs))
	       (call-next-method)
	       (if curried?
		   (mk-application (mk-predicate-application funid te adt funs)
		     arg)
		   (mk-predicate-application
		    funid te adt (append funs (list (copy arg))))))))
	(t (call-next-method))))

(defmethod acc-predicate-selection (arg (te datatype-subtype) pvars
					ptypes thinst adt funid curried?)
  (acc-predicate-selection arg (declared-type te) pvars
			   ptypes thinst adt funid curried?))

(defmethod acc-predicate-selection (arg (te subtype) pvars ptypes thinst adt
					funid curried?)
  (if (finite-set-type? te)
      (let ((fun (acc-predicate-selection*
		  (domain (supertype te)) pvars ptypes thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (if curried?
		(mk-application (mk-predicate-application funid te adt (list fun))
		  arg)
		(mk-predicate-application
		 funid te adt (cons fun (list (copy arg)))))))
      (acc-predicate-selection arg (supertype te) pvars ptypes thinst adt
			       funid curried?)))

(defmethod acc-predicate-selection (arg (te funtype) pvars ptypes thinst adt
					funid curried?)
  (declare (ignore curried?))
  (if (sequence? te)
      (let ((fun (acc-predicate-selection* (range te) pvars ptypes thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (mk-application (mk-application funid fun) (copy arg))))
      (let* ((fid (make-new-variable '|x| te))
	     (fbd (make-bind-decl fid (domain te)))
	     (fvar (mk-name-expr fid nil nil
				 (make-resolution fbd
				   (current-theory-name) (domain te))))
	     (*bound-variables* (cons fbd *bound-variables*))
	     (fun (acc-predicate-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) fvar nil))
		       (range te))
		   pvars ptypes thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (if (eq funid '|every|)
		(mk-forall-expr (list fbd)
		  (mk-application fun
		    (mk-application (copy arg) (copy fvar))))
		(mk-exists-expr (list fbd)
		  (mk-application fun
		    (mk-application (copy arg) (copy fvar)))))))))

(defmethod acc-predicate-selection (arg (te recordtype) pvars ptypes thinst adt
					funid curried?)
  (declare (ignore curried?))
  (let* ((rbd (make-bind-decl (id arg) te))
	 (rvar (mk-name-expr (id arg) nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (preds (acc-predicate-fields (fields te) rvar pvars ptypes thinst adt funid
				      (dependent? te))))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-conjunction
	 (mapcan #'(lambda (fd pred)
		     (unless (if (eq funid '|every|)
				 (everywhere-true? pred)
				 (everywhere-false? pred))
		       (list (mk-application pred
			       (mk-application (id fd) (copy arg))))))
	   (fields te) preds)))))

(defun acc-predicate-fields (fields rvar pvars ptypes thinst adt funid dep?
				    &optional result)
  (if (null fields)
      (nreverse result)
      (acc-predicate-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       rvar pvars ptypes adt funid dep?
       (cons (acc-predicate-selection*
	      (copy-untyped (type (car fields))) pvars ptypes thinst adt funid)
	     result))))

(defmethod acc-predicate-selection (arg (te tupletype) pvars ptypes thinst adt
					funid curried?)
  (declare (ignore curried?))
  (let ((preds (acc-predicate-types (types te) arg pvars ptypes thinst adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-conjunction
	 (let ((num 0))
	   (mapcan #'(lambda (pred)
		       (incf num)
		       (unless (if (eq funid '|every|)
				   (everywhere-true? pred)
				   (everywhere-false? pred))
			 (list (mk-application pred
				 (make-instance 'projappl
				   :id (makesym "PROJ_~d" num)
				   :index num
				   :argument (copy arg))))))
	     preds))))))

(defmethod acc-predicate-selection (arg (te cotupletype) pvars ptypes thinst adt
					funid curried?)
  (declare (ignore curried?))
  (let ((preds (acc-predicate-types (types te) arg pvars ptypes thinst adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(let ((num 0)
	      (varid (make-new-variable '|x| adt)))
	  (mk-cases-expr arg
	    (mapcar #'(lambda (pred type)
			(incf num)
			(let* ((bd (make-bind-decl varid type))
			       (var (make-variable-expr bd))
			       (in-expr (make-instance 'injection-expr
					  :id (makesym "IN_~d" num)
					  :index num))
			       (sel-expr (if pred
					     (mk-application pred var)
					     (if (eq funid '|every|)
						 *true* *false*))))
			  (mk-selection in-expr (list bd) sel-expr)))
	      preds (types te))
	    nil)))))

(defun acc-predicate-types (types arg pvars ptypes thinst adt funid
				  &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-predicate-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index arg)
		    nil))
	   (cdr types))
       arg pvars ptypes adt funid (1+ index)
       (cons (acc-predicate-selection* (car types) pvars ptypes thinst adt funid)
	     result))))

(defmethod acc-predicate-selection (arg (te type-expr) pvars ptypes thinst adt
					funid curried?)
  (declare (ignore arg pvars ptypes thinst adt curried?))
  (copy (if (eq funid '|every|) *true* *false*)))

(defmethod acc-predicate-selection* ((te type-name) pvars ptypes thinst adt funid)
  (cond ((member te ptypes :test #'tc-eq)
	 (let ((pos (position te ptypes :test #'tc-eq)))
	   (nth pos pvars)))
	((tc-eq te (subst-adt-type (adt-type-name adt) thinst adt))
	 (mk-application* funid pvars))
	((adt? te)
	 (let ((funs (mapcar #'(lambda (act)
				 (acc-predicate-selection* (type-value act)
							   pvars ptypes
							   thinst adt funid))
			     (positive-actuals te))))
	   (if (if (eq funid '|every|)
		   (every #'everywhere-true? funs)
		   (every #'everywhere-false? funs))
	       (call-next-method)
	       (mk-predicate-application funid te adt funs))))
	(t (call-next-method))))

(defmethod acc-predicate-selection* ((te funtype) pvars ptypes thinst adt funid)
  (if (sequence? te)
      (let ((fun (acc-predicate-selection* (range te) pvars ptypes
					   thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (mk-application funid fun)))
      (let* ((fid (make-new-variable '|x| te))
	     (fbd (make-bind-decl fid (domain te)))
	     (fvar (mk-name-expr fid nil nil
				 (make-resolution fbd
				   (current-theory-name) (domain te))))
	     (*bound-variables* (cons fbd *bound-variables*))
	     (fun (acc-predicate-selection*
		   (if (typep (domain te) 'dep-binding)
		       (substit (range te) (acons (domain te) fvar nil))
		       (range te))
		   pvars ptypes thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (let* ((lid (make-new-variable '|f| (list te fun)))
		   (lbd (make-bind-decl lid te))
		   (lvar (mk-name-expr lid nil nil
				       (make-resolution lbd
					 (current-theory-name) te))))
	      (mk-lambda-expr (list lbd)
		(if (eq funid '|every|)
		    (mk-forall-expr (list fbd)
		      (mk-application fun
			(mk-application lvar (copy fvar))))
		    (mk-exists-expr (list fbd)
		      (mk-application fun
			(mk-application lvar (copy fvar)))))))))))

(defmethod acc-predicate-selection* ((te recordtype) pvars ptypes thinst adt funid)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons rbd *bound-variables*))
	 (preds (acc-predicate-fields (fields te) rvar pvars ptypes thinst adt funid
				      (dependent? te))))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-conjunction
	   (mapcan #'(lambda (fd pred)
		       (unless (if (eq funid '|every|)
				   (everywhere-true? pred)
				   (everywhere-false? pred))
			 (list (mk-application pred
				 (mk-application (id fd) rvar)))))
	     (fields te) preds))))))

(defmethod acc-predicate-selection* ((te tupletype) pvars ptypes thinst adt funid)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (preds (acc-predicate-types (types te) tvar pvars ptypes thinst adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (mk-conjunction
	   (let ((num 0))
	     (mapcan #'(lambda (pred)
			 (incf num)
			 (unless (if (eq funid '|every|)
				     (everywhere-true? pred)
				     (everywhere-false? pred))
			   (list (mk-application pred
				   (make-instance 'projappl
				     :id (makesym "PROJ_~d" num)
				     :index num
				     :argument tvar)))))
	       preds)))))))

(defmethod acc-predicate-selection* ((te cotupletype) pvars ptypes thinst adt funid)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (preds (acc-predicate-types (types te) tvar pvars ptypes thinst adt funid)))
    (if (if (eq funid '|every|)
	    (every #'everywhere-true? preds)
	    (every #'everywhere-false? preds))
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable '|x| adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (pred type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    :id (makesym "IN_~d" num)
					    :index num))
				 (sel-expr (if pred
					       (mk-application pred var)
					       (if (eq funid '|every|)
						   *true* *false*))))
			    (mk-selection in-expr (list bd) sel-expr)))
		preds (types te))
	      nil))))))

(defmethod acc-predicate-selection* ((te dep-binding) pvars ptypes thinst adt funid)
  (acc-predicate-selection* (type te) pvars ptypes thinst adt funid))

(defmethod acc-predicate-selection* ((te datatype-subtype) pvars ptypes
				     thinst adt funid)
  (acc-predicate-selection* (declared-type te) pvars ptypes thinst adt funid))

(defmethod acc-predicate-selection* ((te subtype) pvars ptypes
				     thinst adt funid)
  (if (finite-set-type? te)
      (let ((fun (acc-predicate-selection*
		  (domain (supertype te)) pvars ptypes thinst adt funid)))
	(if (if (eq funid '|every|)
		(everywhere-true? fun)
		(everywhere-false? fun))
	    (call-next-method)
	    (mk-predicate-application funid te adt (list fun))))
      (acc-predicate-selection* (find-adt-supertype te) pvars ptypes thinst
				adt funid)))

;;; subterm definition

(defun generate-adt-subterm-decls (adt)
  (generate-adt-subterm adt)
  (generate-adt-<< adt)
  (generate-adt-<<-wf adt))

(defun generate-adt-subterm (adt)
  (multiple-value-bind (dparams dacts thinst atype)
      (new-decl-formals adt)
    (declare (ignore dacts thinst))
    (let* ((xid (make-new-variable '|x| adt))
	   (yid (make-new-variable '|y| adt))
	   (subterm-decl
	    (mk-adt-def-decl '|subterm|
	      *boolean*
	      (parse-unparse (gen-adt-subterm-definition
			      adt (mk-name-expr xid) (mk-name-expr yid)))
	      (list (mk-arg-bind-decl xid atype)
		    (mk-arg-bind-decl yid atype))
	      nil nil dparams)))
      (typecheck-adt-decl subterm-decl))))
	   
(defun generate-adt-<< (adt)
  (multiple-value-bind (dparams dacts thinst atype)
      (new-decl-formals adt)
    (declare (ignore dacts thinst))
    (let* ((xid (make-new-variable '|x| adt))
	   (yid (make-new-variable '|y| adt))
	   (wf-type (with-bound-declparams dparams
		      (typecheck* (mk-expr-as-type
				   (mk-name-expr '|strict_well_founded?|
				     (list (mk-actual atype))))
				  nil nil nil)))
	   (<<-decl
	    (mk-adt-def-decl '<<
	      (supertype wf-type)
	      (mk-lambda-expr (list (mk-bind-decl xid atype)
				    (mk-bind-decl yid atype))
		(parse-unparse (gen-adt-<<-definition
				adt (mk-name-expr xid) (mk-name-expr yid))))
	      nil
	      (print-type (supertype wf-type))
	      nil
	      dparams)))
      (typecheck-adt-decl <<-decl)
      (setf (type <<-decl) wf-type
	    (declared-type <<-decl) (print-type wf-type)))))

(defun generate-adt-<<-wf (adt)      
  (multiple-value-bind (dparams dacts thinst atype)
      (new-decl-formals adt)
    (declare (ignore dacts thinst))
    (let* ((<<-wf-decl (mk-formula-decl (makesym "~a_well_founded" (id adt))
			 (mk-application
			     (mk-name-expr '|strict_well_founded?|
			       (list (mk-actual atype)))
			   (mk-name-expr '<<))
			 'AXIOM nil dparams)))
      (typecheck-adt-decl <<-wf-decl t nil))))

(defun gen-adt-subterm-definition (adt xvar yvar)
  (if (every #'(lambda (c)
		 (non-recursive-constructor c (adt-type-name adt)))
	     (constructors adt))
      (mk-application '= xvar yvar)
      (mk-application 'OR
	(mk-application '= xvar yvar)
	(mk-cases-expr yvar
	  (mapcar #'(lambda (c) (gen-adt-subterm-selection c adt xvar))
		  (constructors adt))
	  nil))))

(defun gen-adt-subterm-selection (c adt xvar)
  (if (arguments c)
      (let ((subst (adt-subst-alist (arguments c))))
	(mk-selection (mk-name-expr (id c))
	  (mapcar #'(lambda (b) (declaration (cdr b))) subst)
	  (mk-disjunction
	   (mapcar #'(lambda (a)
		       (acc-subterm-selection (make-instance 'name-expr
						:id (id (get-adt-var a)))
					      (substit (type a) subst)
					      xvar adt))
	     (arguments c)))))
      (mk-selection (mk-name-expr (id c)) nil *false*)))

(defun acc-subterm-selection (arg type xvar adt)
  (let ((fun (acc-subterm-selection* type xvar adt)))
    (if (everywhere-false? fun)
	(copy *false*)
	(mk-application fun arg))))

(defmethod acc-subterm-selection* ((te type-name) xvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (let* ((bd (mk-bind-decl (make-new-variable '|z| te) te))
		(le (mk-lambda-expr (list bd)
		      (mk-application '|subterm| (copy xvar)
				      (mk-name-expr (id bd))))))
	   (setf (parens le) 1)
	   le))
	((adt? te)
	 (let ((subs (mapcar #'(lambda (act)
				 (acc-subterm-selection* (type-value act)
						     xvar adt))
			     (positive-actuals te))))
	   (if (every #'everywhere-false? subs)
	       (call-next-method)
	       (mk-predicate-application '|some| te adt subs))))
	(t (call-next-method))))

(defmethod acc-subterm-selection* ((te datatype-subtype) xvar adt)
  (acc-subterm-selection* (declared-type te) xvar adt))

(defmethod acc-subterm-selection* ((te subtype) xvar adt)
  (if (finite-set-type? te)
      (let ((sub (acc-subterm-selection* (domain (supertype te)) xvar adt)))
	(if (everywhere-false? sub)
	    (call-next-method)
	    (mk-predicate-application '|some| te adt (list sub))))
      (acc-subterm-selection* (supertype te) xvar adt)))

(defmethod acc-subterm-selection* ((te funtype) xvar adt)
  (let* ((zid (make-new-variable '|z| (list te xvar)))
	 (zbd (make-bind-decl zid (domain te)))
	 (zvar (mk-name-expr zid nil nil
			     (make-resolution zbd
			       (current-theory-name) (domain te))))
	 (*bound-variables* (cons zbd *bound-variables*))
	 (sub (acc-subterm-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) zvar nil))
		   (range te))
	       xvar adt)))
    (if (everywhere-false? sub)
	(call-next-method)
	(let* ((fid (make-new-variable '|f| (list te sub)))
	       (fbd (make-bind-decl fid te))
	       (fvar (mk-name-expr fid nil nil
				   (make-resolution fbd
				     (current-theory-name) te))))
	  (mk-lambda-expr (list fbd)
	    (mk-exists-expr (list zbd)
	      (mk-application sub
		(mk-application fvar zvar))))))))

(defmethod acc-subterm-selection* ((te recordtype) xvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons rbd *bound-variables*))
	 (subs (acc-subterm-fields (fields te) xvar rvar adt (dependent? te))))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-disjunction
	   (mapcan #'(lambda (fd sub)
		       (unless (everywhere-false? sub)
			 (list (mk-application sub
				 (mk-application (id fd) rvar)))))
	     (fields te) subs))))))

(defun acc-subterm-fields (fields xvar rvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-subterm-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       xvar rvar adt dep?
       (cons (acc-subterm-selection* (type (car fields)) xvar adt)
	     result))))

(defmethod acc-subterm-selection* ((te tupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (subs (acc-subterm-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(let ((le (mk-lambda-expr (list tbd)
		    (let ((num 0))
		      (mk-disjunction
		       (mapcan #'(lambda (sub)
				   (incf num)
				   (unless (everywhere-false? sub)
				     (list (mk-application sub
					     (make-instance 'projappl
					       :id (makesym "PROJ_~d" num)
					       :index num
					       :argument tvar)))))
			 subs))))))
	  (setf (parens le) 1)
	  le))))

(defmethod acc-subterm-selection* ((te cotupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (subs (acc-subterm-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable '|w| adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (sub type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    :id (makesym "IN_~d" num)
					    :index num))
				 (sel-expr (mk-application sub var)))
			    (mk-selection in-expr (list bd) sel-expr)))
		subs (types te))
	      nil))))))

(defun acc-subterm-types (types xvar tvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-subterm-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       xvar tvar adt (1+ index)
       (cons (acc-subterm-selection* (car types) xvar adt) result))))

(defmethod acc-subterm-selection* ((te dep-binding) xvar adt)
  (acc-subterm-selection* (type te) xvar adt))

(defmethod acc-subterm-selection* ((te type-expr) xvar adt)
  (declare (ignore xvar adt))
  (mk-everywhere-false-function te))

(defun make-some-subterm-application (arg xvar adt)
  (mk-application
      (mk-application '|some|
	(let ((zvar (mk-name-expr (make-new-variable '|z| adt))))
	  (mk-lambda-expr (list (mk-bind-decl (id zvar) (adt-type-name adt)))
	    (mk-application '|subterm| (copy xvar) zvar))))
    (copy arg)))


(defun gen-adt-<<-definition (adt xvar yvar)
  (if (every #'(lambda (c)
		 (non-recursive-constructor c (adt-type-name adt)))
	     (constructors adt))
      (copy *false*)
      (mk-cases-expr yvar
	(mapcar #'(lambda (c) (gen-adt-<<-selection c adt xvar))
		(constructors adt))
	nil)))

(defun gen-adt-<<-selection (c adt xvar)
  (if (arguments c)
      (let ((subst (adt-subst-alist (arguments c))))
	(mk-selection (mk-name-expr (id c))
	  (mapcar #'(lambda (b) (declaration (cdr b))) subst)
	  (mk-disjunction
	   (mapcar #'(lambda (a)
		       (acc-<<-selection (make-instance 'name-expr
					   :id (id (get-adt-var a)))
					 (substit (type a) subst)
					 xvar adt))
	     (arguments c)))))
      (mk-selection (mk-name-expr (id c)) nil *false*)))

(defun acc-<<-selection (arg type xvar adt)
  (let ((fun (acc-<<-selection* type xvar adt)))
    (if (everywhere-false? fun)
	(copy *false*)
	(mk-application fun arg))))

(defmethod acc-<<-selection* ((te type-name) xvar adt)
  (cond ((tc-eq te (adt-type-name adt))
	 (let ((bd (mk-bind-decl (make-new-variable '|z| te) te)))
	   (mk-lambda-expr (list bd)
	     (mk-application 'OR
	       (mk-application '= (copy xvar) (mk-name-expr (id bd)))
	       (mk-application '<< (copy xvar) (mk-name-expr (id bd)))))))
	((adt? te)
	 (let ((subs (mapcar #'(lambda (act)
				 (acc-<<-selection* (type-value act)
						     xvar adt))
			     (positive-actuals te))))
	   (if (every #'everywhere-false? subs)
	       (call-next-method)
	       (mk-predicate-application '|some| te adt subs))))
	(t (call-next-method))))

(defmethod acc-<<-selection* ((te datatype-subtype) xvar adt)
  (acc-<<-selection* (declared-type te) xvar adt))

(defmethod acc-<<-selection* ((te subtype) xvar adt)
  (if (finite-set-type? te)
      (let ((sub (acc-<<-selection* (domain (supertype te)) xvar adt)))
	(if (everywhere-false? sub)
	    (call-next-method)
	    (mk-predicate-application '|some| te adt (list sub))))
      (acc-<<-selection* (supertype te) xvar adt)))

(defmethod acc-<<-selection* ((te funtype) xvar adt)
  (let* ((zid (make-new-variable '|z| (list te xvar)))
	 (zbd (make-bind-decl zid (domain te)))
	 (zvar (mk-name-expr zid nil nil
			     (make-resolution zbd
			       (current-theory-name) (domain te))))
	 (*bound-variables* (cons zbd *bound-variables*))
	 (sub (acc-subterm-selection*
	       (if (typep (domain te) 'dep-binding)
		   (substit (range te) (acons (domain te) zvar nil))
		   (range te))
	       xvar adt)))
    (if (everywhere-false? sub)
	(call-next-method)
	(let* ((fid (make-new-variable '|f| (list te sub)))
	       (fbd (make-bind-decl fid te))
	       (fvar (mk-name-expr fid nil nil
				   (make-resolution fbd
				     (current-theory-name) te))))
	  (mk-lambda-expr (list fbd)
	    (mk-exists-expr (list zbd)
	      (mk-application sub
		(mk-application fvar zvar))))))))

(defmethod acc-<<-selection* ((te recordtype) xvar adt)
  (let* ((rid (make-new-variable '|r| te))
	 (rbd (make-bind-decl rid te))
	 (rvar (mk-name-expr rid nil nil
			     (make-resolution rbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons rbd *bound-variables*))
	 (subs (acc-<<-fields (fields te) xvar rvar adt (dependent? te))))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list rbd)
	  (mk-disjunction
	   (mapcan #'(lambda (fd sub)
		       (unless (everywhere-false? sub)
			 (list (mk-application sub
				 (mk-application (id fd) rvar)))))
	     (fields te) subs))))))

(defun acc-<<-fields (fields xvar rvar adt dep? &optional result)
  (if (null fields)
      (nreverse result)
      (acc-<<-fields
       (if dep?
	   (substit (cdr fields)
	     (acons (car fields)
		    (make-field-application (car fields) rvar)
		    nil))
	   (cdr fields))
       xvar rvar adt dep?
       (cons (acc-<<-selection* (type (car fields)) xvar adt)
	     result))))

(defmethod acc-<<-selection* ((te tupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (subs (acc-<<-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(let ((le (mk-lambda-expr (list tbd)
		    (let ((num 0))
		      (mk-disjunction
		       (mapcan #'(lambda (sub)
				   (incf num)
				   (unless (everywhere-false? sub)
				     (list (mk-application sub
					     (make-instance 'projappl
					       :id (makesym "PROJ_~d" num)
					       :index num
					       :argument tvar)))))
			 subs))))))
	  (setf (parens le) 1)
	  le))))

(defmethod acc-<<-selection* ((te cotupletype) xvar adt)
  (let* ((tid (make-new-variable '|t| te))
	 (tbd (make-bind-decl tid te))
	 (tvar (mk-name-expr tid nil nil
			     (make-resolution tbd
			       (current-theory-name) te)))
	 (*bound-variables* (cons tbd *bound-variables*))
	 (subs (acc-<<-types (types te) xvar tvar adt)))
    (if (every #'everywhere-false? subs)
	(call-next-method)
	(mk-lambda-expr (list tbd)
	  (let ((num 0)
		(varid (make-new-variable '|w| adt)))
	    (mk-cases-expr tvar
	      (mapcar #'(lambda (sub type)
			  (incf num)
			  (let* ((bd (make-bind-decl varid type))
				 (var (make-variable-expr bd))
				 (in-expr (make-instance 'injection-expr
					    :id (makesym "IN_~d" num)
					    :index num))
				 (sel-expr (mk-application sub var)))
			    (mk-selection in-expr (list bd) sel-expr)))
		subs (types te))
	      nil))))))

(defun acc-<<-types (types xvar tvar adt &optional (index 1) result)
  (if (null types)
      (nreverse result)
      (acc-<<-types
       (if (typep (car types) 'dep-binding)
	   (substit (cdr types)
	     (acons (car types)
		    (make-projection-application index tvar)
		    nil))
	   (cdr types))
       xvar tvar adt (1+ index)
       (cons (acc-<<-selection* (car types) xvar adt) result))))

(defmethod acc-<<-selection* ((te dep-binding) xvar adt)
  (acc-<<-selection* (type te) xvar adt))

(defmethod acc-<<-selection* ((te type-expr) xvar adt)
  (declare (ignore xvar adt))
  (mk-everywhere-false-function te))

(defun make-some-<<-application (arg xvar adt)
  (mk-application
      (mk-application '|some|
	(let ((zvar (mk-name-expr (make-new-variable '|z| adt))))
	  (mk-lambda-expr (list (mk-bind-decl (id zvar) (adt-type-name adt)))
	    (mk-application '<< (copy xvar) zvar))))
    (copy arg)))


; (defmethod declarations ((adt datatype))
;   nil)

(defun init-adt-decl (decl)
  (setf (current-declaration) decl)
  (when (typep decl '(and declaration (not formal-decl)))
    (setf (generated-by decl) (id *adt*)))
  (when (typep *adt* 'inline-recursive-type)
    (push decl (generated *adt*)))
  decl)

(defun typecheck-adt-decl (decl &optional (add? t) (reduce? t) (init? t))
  ;;(assert (null (current-declaration)))
  (when init? (init-adt-decl decl))
  (when (eq add? t)
    (setf (theory (current-theory))
	  (if *last-adt-decl*
	      (let* ((theory-part (theory (current-theory)))
		     (rest (cdr (memq *last-adt-decl* theory-part))))
		(nconc (ldiff theory-part rest) (cons decl rest)))
	      (nconc (theory (current-theory)) (list decl))))
    (when *last-adt-decl*
      (setq *last-adt-decl* decl)))
  (typecase decl
    (declaration (setf (module decl) (current-theory))
		 (assert (or (null (decl-formals *adt*))
			     (decl-formals decl)))
		 (typecheck* decl nil nil nil)
		 (setf (typechecked? decl) t)
		 (unless (or (eq add? 'no)
			     (and (typep decl 'type-def-decl)
				  (typep (type-expr decl) 'enumtype)))
		   (put-decl decl))
		 (when (and reduce?
			    (typep decl '(or const-decl formula-decl)))
		   (setf (definition decl)
			 (beta-reduce (definition decl)))))
    (importing (typecheck-using (theory-name decl)))
    (datatype (unwind-protect
		  (typecheck* decl nil nil nil)
		(cleanup-datatype decl))))
  ;;(setf (current-declaration) nil)
  )

(defun add-adt-decl (decl)
  (unless (and (typep decl 'type-def-decl)
	       (typep (type-expr decl) 'enumtype))
    (put-decl decl))
  (setf (theory (current-theory))
	  (if *last-adt-decl*
	      (let* ((theory-part (theory (current-theory)))
		     (rest (cdr (memq *last-adt-decl* theory-part))))
		(nconc (ldiff theory-part rest) (cons decl rest)))
	      (nconc (theory (current-theory)) (list decl))))
  (when *last-adt-decl*
    (setq *last-adt-decl* decl)))


;;; Returns true if the given type occurs positively in the datatype.  All
;;; of the accessor types are checked, and if any occurrence of the type is
;;; direct, in the range of a function type (not recursively), or as a
;;; subtype of one of these two cases.

(defvar *simple-pos* nil
  "If set to T, then occurs-positively? will return nil if the type occurs in a
function, tuple, or record type")

(defvar *is-finite-pos* nil
  "If t, indicates we are in a finite set, in which case we no longer care
  about positivity.")

(defun occurs-positively? (type1 type2)
  (let* ((*negative-occurrence* nil)
	 (pos? (occurs-positively?* type1 type2 nil)))
    (values pos? *negative-occurrence*)))

(defmethod occurs-positively?* (type (adt recursive-type) none)
  (and (occurs-positively?* type (formals-sans-usings adt) none)
       (occurs-positively?* type (constructors adt) none)))

(defmethod occurs-positively?* (type (list list) none)
  (or (null list)
      (and (occurs-positively?* type (car list) none)
	   (occurs-positively?* type (cdr list) none))))

(defmethod occurs-positively?* (type (con simple-constructor) none)
  (occurs-positively?* type
		       (mapcar #'(lambda (ac) (range (type ac)))
			       (acc-decls con))
		       none))

(defmethod occurs-positively?* (type (fm formal-type-decl) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (fm formal-const-decl) none)
  (occurs-positively?* type (declared-type fm) none))

(defmethod occurs-positively?* (type (decl const-decl) none)
  (occurs-positively?* type (type decl) none))

(defmethod occurs-positively?* :around (type (te type-expr) none)
  (if (tc-eq type te)
      (cond ((and none (not *is-finite-pos*))
	     (setq *negative-occurrence* te)
	     nil)
	    (t t))
      (call-next-method)))

(defmethod occurs-positively?* (type (te type-name) none)
  (if (adt? te)
      (let ((adt (adt te)))
	(if (typep adt 'inline-recursive-type)
	    (every #'(lambda (a)
		       (if (type-value a)
			   (occurs-positively?* type (type-value a) t)
			   (occurs-positively?* type (type (expr a)) t)))
		   (actuals te))
	    (every #'(lambda (a pos?)
		       (if (type-value a)
			   (occurs-positively?* type (type-value a)
						(or none
						    (unless pos? t)))
			   (occurs-positively?* type (type (expr a)) t)))
		   (actuals te)
		   (mapcar #'(lambda (ff)
			       (member ff
				       (positive-types adt)
				       :test #'same-id))
			   (formals-sans-usings adt)))))
      (occurs-positively?* type (actuals te) t)))

(defmethod occurs-positively?* (type (te datatype-subtype) none)
  (occurs-positively?* type (declared-type te) none))

(defmethod occurs-positively?* (type (te subtype) none)
  (let ((*is-finite-pos* (or *is-finite-pos* (finite-set-type? te))))
    (and (occurs-positively?* type (supertype te) none)
	 (occurs-positively?* type (predicate te) none))))

(defmethod finite-set-type? ((te subtype))
  (and (set-type? (supertype te))
       (is-finite-predicate? (predicate te))))

(defmethod finite-set-type? ((te type-expr))
  nil)

(defmethod set-type? ((te funtype))
  (tc-eq (range te) *boolean*))

(defmethod set-type? ((te type-expr))
  nil)

(defmethod is-finite-predicate? ((ex name-expr))
  (and (eq (id (declaration ex)) '|is_finite|)
       (eq (id (module-instance ex)) '|finite_sets|)))

(defmethod is-finite-predicate? ((ex expr))
  nil)

(defmethod occurs-positively?* (type (te funtype) none)
  (and (occurs-positively?* type (range te)
			    (or (and (not *is-finite-pos*) *simple-pos*) none))
       (occurs-positively?* type (domain te) t)))

(defmethod occurs-positively?* (type (te tupletype) none)
  (occurs-positively?* type (types te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te cotupletype) none)
  (occurs-positively?* type (types te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te recordtype) none)
  (occurs-positively?* type (fields te) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (fd field-decl) none)
  (occurs-positively?* type (type fd) (or *simple-pos* none)))

(defmethod occurs-positively?* (type (te dep-binding) none)
  (occurs-positively?* type (type te) none))

(defvar *adt-recursive-names* nil)

(defmethod occurs-positively?* (type (ex name-expr) none)
  (or *is-finite-pos*
      (member ex *adt-recursive-names* :test #'tc-eq)
      (not (occurs-in type (actuals (module-instance ex))))
      (let* ((res (resolution ex))
	     (decl (declaration res))
	     (*adt-recursive-names* (cons ex *adt-recursive-names*)))
	(when (and (typep decl '(or const-decl def-decl))
		   (definition decl))
	  (let* ((def (subst-mod-params (car (def-axiom decl))
					(module-instance res)
					(module decl)))
		 (*adt-recursive-names*
		  (if (typep def 'binding-expr)
		      (append (mapcar #'make-variable-expr (bindings def))
			      *adt-recursive-names*)
		      *adt-recursive-names*)))
	    (or
	    (occurs-positively?*
	     type
	     (if (typep def 'binding-expr)
		 (args2 (expression def))
		 (args2 def))
	     none)
	    (break "2")))))))

(defmethod occurs-positively?* (type (ex adt-name-expr) none)
  (let ((adt (adt ex)))
    (occurs-positively-in-adt?
     type
     (actuals (module-instance (resolution (or (print-type adt) adt))))
     (formals-sans-usings (adt adt))
     (positive-types (adt adt))
     none)))

;;(defmethod positive-types ((dt recursive-type))
;;  (positive-types (or (adt-theory dt) (current-theory))))
;;; (remove-method #'positive-types
;;;    (find-method #'positive-types '() (list (find-class 'inline-recursive-type))))

(defun occurs-positively-in-adt? (type acts formals postypes none)
  (or (null acts)
      (and (if (member (car formals) postypes
		       :test #'same-id
		       :key #'(lambda (x) (or (print-type x) x)))
	       (occurs-positively?* type (type-value (car acts)) none)
	       (occurs-positively?* type (expr (car acts)) t))
	   (occurs-positively-in-adt? type (cdr acts) (cdr formals)
				      postypes none))))

(defmethod occurs-positively?* (type (ex number-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex record-expr) none)
  (occurs-positively?* type (assignments ex) none))

(defmethod occurs-positively?* (type (ex tuple-expr) none)
  (occurs-positively?* type (exprs ex) none))

(defmethod occurs-positively?* (type (ex projection-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex injection-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex injection?-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex extraction-expr) none)
  (declare (ignore type none))
  t)

(defmethod occurs-positively?* (type (ex projection-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex injection-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex injection?-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex extraction-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex field-application) none)
  (occurs-positively?* type (argument ex) none))

(defmethod occurs-positively?* (type (ex application) none)
  (and (occurs-positively?* type (operator ex) none)
       (occurs-positively?* type (argument ex) none)))

(defmethod occurs-positively?* (type (ex binding-expr) none)
  (and (occurs-positively?* type (bindings ex) none)
       (occurs-positively?* type (expression ex) none)))

(defmethod occurs-positively?* (type (ex cases-expr) none)
  (and (occurs-positively?* type (expression ex) none)
       (occurs-positively?* type (selections ex) none)
       (occurs-positively?* type (else-part ex) none)))

(defmethod occurs-positively?* (type (ex selection) none)
  (occurs-positively?* type (expression ex) none))

(defmethod occurs-positively?* (type (ex update-expr) none)
  (and (occurs-positively?* type (expression ex) none)
       (occurs-positively?* type (assignments ex) none)))

(defmethod occurs-positively?* (type (ex assignment) none)
  (and (occurs-positively?* type (arguments ex) none)
       (occurs-positively?* type (expression ex) none)))

(defmethod occurs-positively?* (type (ex bind-decl) none)
  (occurs-positively?* type (type ex) none))

(defmethod occurs-positively?* (type (ex actual) none)
  (if (type-value ex)
      (occurs-positively?* type (type-value ex) none)
      (occurs-positively?* type (expr ex) none)))

(defun generate-adt-vars (adt)
  (clrhash *adt-vars*)
  (setf (gethash adt *adt-vars*) (make-new-variable '|v| adt))
  (dolist (c (constructors adt))
    (let ((num 0))
      (dolist (a (arguments c))
	(setf (gethash a *adt-vars*)
	      (mk-name-expr (makesym "~a~d_var"
				     (op-to-id (id c))
				     (incf num))))))))

(defun get-adt-var (obj)
  (let ((var (gethash obj *adt-vars*)))
    (if (syntax? var)
	(copy-all var)
	var)))

(defmethod sequence? ((te funtype))
  (tc-eq (domain te) *naturalnumber*))

(defmethod sequence? ((te type-expr))
  nil)

(defmethod sequence? ((te dep-binding))
  (sequence? (type te)))

(defmethod everywhere-true? (expr)
  (declare (ignore expr))
  nil)

(defmethod everywhere-true? ((expr lambda-expr))
  (tc-eq (expression expr) *true*))

(defmethod everywhere-false? (expr)
  (declare (ignore expr))
  nil)

(defmethod everywhere-false? ((expr lambda-expr))
  (tc-eq (expression expr) *false*))

(defun mk-identity-fun (te)
  (let ((act (mk-actual te)))
    (pc-parse (unparse (mk-name-expr '|id| (list act)) :string t)
	      'expr)))

(defmethod identity-fun? (expr)
  (declare (ignore expr))
  nil)

(defmethod identity-fun? ((expr name-expr))
  (eq (id expr) '|id|))

(defmethod mk-predicate-application (funid (te type-name) adt funs)
  (if (eq (adt te) adt)
      (mk-application* funid funs)
      (let* ((acts (actuals te))
	     (name (mk-name-expr funid acts))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname funs))))

(defmethod mk-predicate-application (funid (te type-expr) adt funs)
  (declare (ignore adt))
  (let ((name (mk-name-expr funid)))
    (mk-application* name funs)))

(defmethod mk-map-application ((te type-name) fpairs adt maps)
  (if (eq (adt te) adt)
      (mk-application* '|map| maps)
      (let* ((acts (raise-actuals (actuals (module-instance te))))
	     (macts (raise-actuals (subst-map-actuals (positive-actuals te) fpairs)))
	     (name (mk-name-expr '|map| (append acts macts)))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname maps))))

(defmethod mk-map-application ((te funtype) fpairs adt maps)
  (declare (ignore fpairs adt))
  (mk-application* '|map| maps))

(defmethod mk-map-application ((te datatype-subtype) fpairs adt maps)
  (mk-map-application (declared-type te) fpairs adt maps))

(defmethod mk-map-application ((te subtype) fpairs adt maps)
  (mk-map-application (supertype te) fpairs adt maps))

(defun map-application? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'name-expr)
       (eq (id (operator expr)) '|map|)))

(defun every-application? (expr)
  (and (typep expr 'application)
       (typep (operator expr) 'name-expr)
       (eq (id (operator expr)) '|every|)))

(defun subst-map-actuals (te fpairs)
  (let ((rpairs (remove-if #'(lambda (pr) (eq (car pr) (cdr pr))) fpairs)))
    (gensubst te
      #'(lambda (x) (subst-map-actuals! x rpairs))
      #'(lambda (x) (subst-map-actuals? x rpairs)))))

(defmethod subst-map-actuals? ((name name) fpairs)
  (and (resolution name)
       (assoc (declaration name) fpairs :test #'same-id)))

(defmethod subst-map-actuals? (obj fpairs)
  (declare (ignore obj fpairs)))

(defmethod subst-map-actuals! ((name name) fpairs)
  (let ((nres (subst-map-actuals! (resolution name) fpairs)))
    (lcopy name
      'id (id (declaration nres))
      'resolutions (list nres))))

(defmethod subst-map-actuals! ((res resolution) fpairs)
  (let ((ndecl (cdr (assoc (declaration res) fpairs :test #'same-id))))
    (make-resolution ndecl (current-theory-name))))

(defmethod mk-every-application ((te type-name) fpairs adt everys)
  (if (eq (adt te) adt)
      (mk-application* '|every| everys)
      (let* ((acts (actuals (module-instance te)))
	     (macts (subst-map-actuals acts fpairs))
	     (name (mk-name-expr '|every| (append acts macts)))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname everys))))

(defmethod mk-simple-every-application ((te type-name) adt everys)
  (if (eq (adt te) adt)
      (mk-application* '|every| everys)
      (let* ((name (mk-name-expr '|every|))
	     (pname (pc-parse (unparse name :string t) 'expr)))
	(mk-application* pname everys))))

(defmethod mk-every-application ((te funtype) fpairs adt everys)
  (declare (ignore fpairs adt))
  (mk-application* '|every| everys))

(defun positive-actuals (type-name)
  (let ((adt (adt? type-name)))
    (positive-actuals* (actuals (module-instance type-name))
		       (formals-sans-usings adt)
		       (positive-types adt))))

(defun positive-actuals* (acts formals pos-formals &optional result)
  (if (null acts)
      (nreverse result)
      (positive-actuals* (cdr acts) (cdr formals) pos-formals
			 (if (member (car formals) pos-formals
				     :test #'same-id)
			     (cons (car acts) result)
			     result))))

(defmethod subtypes ((adt recursive-type))
  nil)


(defun generate-codt-coreduce-theory (codt)
  (build-adt-theory (makesym "~a_codt_coreduce" (id codt)) codt
    (let ((dom (make-new-variable '|domain| codt)))
      (generate-adt-reduce-formals codt dom)
      (let* ((codtinst (generate-adt-reduce-using codt dom))
	     (struct (generate-codt-structure-datatype codt dom)))
	(typecheck-adt-decl struct)
	(generate-codt-coreduce codt dom codtinst struct)))))

(defmethod generate-codt-structure-datatype ((codt codatatype-with-subtypes) dom)
  (make-instance 'inline-datatype-with-subtypes
    :id (makesym "~a_struct" (id codt))
    :constructors (generate-codt-structure-constructors codt dom)
    :subtypes (generate-codt-structure-subtypes codt)))

(defmethod generate-codt-structure-datatype ((codt codatatype) dom)
  (make-instance 'inline-datatype
    :id (makesym "~a_struct" (id codt))
    :constructors (generate-codt-structure-constructors codt dom)))

(defun generate-codt-structure-constructors (codt dom)
  (generate-codt-structure-constructors* (constructors codt) codt dom))

(defun generate-codt-structure-constructors* (constructors codt dom
							   &optional result)
  (if (null constructors)
      (nreverse result)
      (generate-codt-structure-constructors*
       (cdr constructors)
       codt
       dom
       (cons (generate-codt-structure-constructor (car constructors) codt dom)
	     result))))

(defmethod generate-codt-structure-constructor (constructor codt dom)
  (make-instance 'simple-constructor
    :id (makesym "inj_~a" (id constructor))
    :arguments (mapcar #'(lambda (arg)
			   (generate-codt-structure-argument arg codt dom))
		 (arguments constructor))
    :recognizer (makesym "inj_~a" (recognizer constructor))))

(defmethod generate-codt-structure-constructor
    ((constructor constructor-with-subtype) codt dom)
  (make-instance 'constructor-with-subtype
    :id (makesym "inj_~a" (id constructor))
    :arguments (mapcar #'(lambda (arg)
			   (generate-codt-structure-argument arg codt dom))
		 (arguments constructor))
    :recognizer (makesym "inj_~a" (recognizer constructor))
    :subtype (make-instance 'type-name
	       :id (makesym "inj_~a" (id (subtype constructor))))))

(defun generate-codt-structure-argument (arg codt dom)
  (make-instance 'adtdecl
    :id (makesym "inj_~a" (id arg))
    :declared-type (generate-codt-structure-accessor-type
		    (pc-parse (unparse (declared-type arg) :string t)
		      'type-expr)
		    (typecheck (mk-type-name (id (adt-type-name codt))))
		    codt dom)))

(defun generate-codt-structure-accessor-type (type codtype codt dom)
  (let ((*dont-expand-adt-subtypes* t))
    (gensubst type
      #'(lambda (te) (if (subtype-of? (typecheck te) codtype)
			 (typecheck (mk-type-name dom))
			 (generate-codt-structure-accessor-type
			  (find-supertype (typecheck te)) codtype codt dom)))
      #'(lambda (te) (and (type-expr? te)
			  (not (expr-as-type? te))
			  (let ((tval (typecheck te)))
			    (or (subtype-of? tval codtype)
				(and (subtype? te)
				     (adt-type-name? (find-supertype tval))))))))))

(defun generate-codt-structure-subtypes (codt)
  (mapcar #'(lambda (st)
	      (make-instance 'type-name
		:id (makesym "inj_~a" (id st))))
    (subtypes codt)))


(defun generate-codt-coreduce (codt dom codtinst struct)
  (let* ((*generate-tccs* 'none)
	 (dtype (typecheck* (mk-type-name dom) nil nil nil))
	 (coreduce-type (generate-coreduce-function-type dtype codt struct))
	 (opid (make-new-variable '|op| codt))
	 (opbd (make-bind-decl opid coreduce-type))
	 (opvar (make-variable-expr opbd))
	 (xvid (make-new-variable '|x| codt))
	 (xbd (make-bind-decl xvid dtype))
	 (xvar (make-variable-expr xbd))
	 (coreduce-op (mk-application '|coreduce| opvar))
	 (op-xvar (mk-application opvar xvar))
	 (dvid (make-new-variable '|c| codt))
	 (dbd (make-bind-decl dvid codtinst))
	 (dvar (make-variable-expr dbd))
	 (*bound-variables*
	  (cons opbd (cons xbd (cons dbd *bound-variables*))))
	 (coreduce-range
	  (mk-subtype codtinst
	    (mk-lambda-expr (list dbd)
	      (mk-disjunction
	       (mapcar #'(lambda (c1 c2)
			   (mk-conjunction
			    (list (mk-application (recognizer c1)
				    op-xvar)
				  (mk-application (recognizer c2)
				    dvar))))
		 (constructors struct) (constructors codt))))))
	 (*adt-vars* (make-hash-table :test #'eq)))
    (generate-adt-vars struct)
    (let* ((fpairs (acons (declaration dtype) (declaration codtinst) nil))
	   (precases (mapcar #'(lambda (c)
				 (generate-adt-map-selection
				  c (list coreduce-op) (list dtype)
				  fpairs struct
				  (adt-type-name struct) t))
		       (constructors struct)))
	   (cases (mk-cases-expr op-xvar
		    (mapcar #'(lambda (sel c)
				(copy sel
				  'expression (if (application?
						   (expression sel))
						  (mk-application (id c)
						    (argument
						     (expression sel)))
						  (mk-name-expr (id c)))))
		      precases (constructors codt))
		    nil))
	   (decl (mk-adt-def-decl
		     '|coreduce| coreduce-range
		     (pc-parse (unparse cases :string t) 'expr)
		     (list (list opbd) (list xbd)))))
      (typecheck-adt-decl decl))))

(defun generate-coreduce-function-type (dtype codt struct)
  (let* ((ftype (mk-funtype (list dtype) (adt-type-name struct)))
	 (opname (make-new-variable '|op| codt))
	 (opbd (make-bind-decl opname ftype))
	 (opvar (make-variable-expr opbd))
	 (dname (make-new-variable '|x| codt))
	 (dbd (make-bind-decl dname dtype))
	 (*bound-variables* (cons opbd (cons dbd *bound-variables*)))
	 (type-alist (append (mapcar #'(lambda (x y)
					 (cons (typecheck x) (typecheck y)))
			       (subtypes codt) (subtypes struct))
			     (mapcar #'(lambda (x y)
					 (cons (typecheck
						   (mk-name-expr
						       (recognizer x)))
					       (typecheck
						   (mk-name-expr
						       (recognizer y)))))
			       (constructors codt) (constructors struct))))
	 (cases (generate-coreduce-funtype-cases opvar codt struct type-alist)))
    (if (every #'null cases)
	ftype
	(mk-subtype ftype
	  (mk-lambda-expr (list opbd)
	    (make-forall-expr (list dbd)
	      (typecheck
		  (mk-cases-expr (make-application opvar
				   (make-variable-expr dbd))
		    (remove-if #'null cases)
		    (when (some #'null cases)
		      *true*))
		:expected *boolean*)))))))

(defun generate-coreduce-funtype-cases (op codt struct type-alist)
  (mapcar #'(lambda (c cs)
	      (generate-coreduce-funtype-case c cs op codt struct type-alist))
    (constructors codt) (constructors struct)))

(defun generate-coreduce-funtype-case (c cs op codt struct type-alist)
  (when (arguments c)
    (let* ((bindings (mapcar #'(lambda (a as)
				   (make-bind-decl (id (get-adt-var a))
				     (type as)))
			 (arguments c) (arguments cs)))
	   (vars (mapcar #'(lambda (b)
			     (mk-name-expr (id b) nil nil
					   (make-resolution b
					     (current-theory-name)
					     (type b))))
		   bindings))
	   (value (generate-coreduce-funtype-selection-value
		    (arguments c) (arguments cs) vars op codt struct
		    type-alist)))
      (when value
	(mk-selection (mk-name-expr (id cs))
	  bindings
	  value)))))

(defun generate-coreduce-funtype-selection-value (args sargs vars op codt
						       struct type-alist
						       &optional result)
  (if (null args)
      result
      (let ((val (generate-coreduce-funtype-selection-value1
		  (car args) (car sargs) (car vars) op codt struct
		  type-alist)))
	(generate-coreduce-funtype-selection-value
	 (cdr args) (cdr sargs) (cdr vars) op codt struct type-alist
	 (if val
	     (if result
		 (make-conjunction (list result val))
		 val)
	     result)))))

(defun generate-coreduce-funtype-selection-value1 (arg sarg var op codt struct
						       type-alist)
  (generate-coreduce-funtype-selection-value*
   (type arg) (type sarg) var op codt struct type-alist))

(defmethod generate-coreduce-funtype-selection-value* ((te type-name) ste var op codt struct type-alist)
  (declare (ignore ste var op codt struct type-alist))
  nil)

(defmethod generate-coreduce-funtype-selection-value* ((te subtype) (ste type-name) var op codt struct type-alist)
  (cond ((subtype-of? te (adt-type-name codt))
	 (let ((struct-subtype (cdr (assoc te type-alist :test #'tc-eq))))
	   (assert struct-subtype)
	   (make-application (predicate struct-subtype)
	     (make-application op var))))
	(t (assert (adt ste))
	   (let* ((formals (formals-sans-usings (adt ste)))
		  (ptypes (positive-types (adt ste)))
		  (supte (find-supertype te))
		  (acts (actuals (module-instance
				  (resolution (or (print-type supte) supte)))))
		  (sacts (actuals (module-instance
				   (resolution (or (print-type ste) ste)))))
		  (vals (mapcar
			    #'(lambda (a sa fm)
				(when (member fm ptypes
					      :test #'same-id
					      :key #'(lambda (x)
						       (or (print-type x) x)))
				  (let* ((did (make-new-variable '|x| codt))
					 (dbd (make-bind-decl did (type-value sa)))
					 (dvar (make-variable-expr dbd))
					 (*bound-variables*
					  (cons dbd *bound-variables*))
					 (dval
					  (generate-coreduce-funtype-selection-value*
					   (type-value a) (type-value sa)
					   dvar op codt struct type-alist)))
				    (mk-lambda-expr (list dbd)
				      (or dval *true*)))))
			  acts sacts formals)))
	     (unless (every #'everywhere-true? vals)
	       (mk-application (mk-application* (mk-name-expr '|every|)
				 vals)
		 var))))))
					 
	     

(defmethod generate-coreduce-funtype-selection-value* ((te subtype) (ste subtype) var op codt struct type-alist)
  (declare (ignore op struct type-alist))
  (assert (not (subtype-of? te (adt-type-name codt))))
  nil)

(defmethod generate-coreduce-funtype-selection-value* ((te funtype) (ste funtype) var op codt struct type-alist)
  ;;(assert (tc-eq (domain te) (domain ste)))
  (let* ((dvarid (make-new-variable '|d| codt))
	 (dbd (make-bind-decl dvarid (if (dep-binding? (domain te))
					 (type (domain te))
					 (domain te))))
	 (dvar (make-variable-expr dbd))
	 (*bound-variables* (cons dbd *bound-variables*))
	 (value (generate-coreduce-funtype-selection-value* (range te) (range ste) (make-application var dvar) op codt struct type-alist)))
    (when value
      (mk-forall-expr (list dbd)
	(if (dep-binding? (domain ste))
	    (substit value (acons (domain ste) dbd nil))
	    value)))))

(defmethod generate-coreduce-funtype-selection-value*
    ((te tupletype) (ste tupletype) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-tup-value*
   (types te) (types ste) var op codt struct type-alist))

(defun generate-coreduce-funtype-selection-tup-value* (types stypes var op codt struct type-alist &optional (num 1) result)
  (if (null types)
      result
      (let* ((proj (make!-projection-application num var))
	     (aval (generate-coreduce-funtype-selection-value*
		    (car types) (car stypes) proj op codt struct type-alist)))
	(generate-coreduce-funtype-selection-tup-value*
	 (cdr types)
	 (if (dep-binding? (car stypes))
	     (substit (cdr stypes)
	       (acons (car stypes) proj nil))
	     (cdr stypes))
	 var op codt struct type-alist (1+ num)
	 (if aval
	     (if result
		 (make-conjunction (list result aval))
		 aval)
	     result)))))

(defmethod generate-coreduce-funtype-selection-value*
    ((te cotupletype) (ste cotupletype) var op codt struct type-alist)
  (let ((sel-var-id (make-new-variable '|x| codt)))
    (generate-coreduce-funtype-selection-cotup-value*
     (types te) (types ste) te var op codt struct sel-var-id type-alist)))

(defun generate-coreduce-funtype-selection-cotup-value*
  (types stypes cotup var op codt struct sel-var-id type-alist
	 &optional (num 1) result)
  (if (null types)
      (unless (every #'(lambda (sel) (tc-eq (expression sel) *true*)) result)
	(make!-unpack-expr var (nreverse result)))
      (let* ((bd (make-bind-decl sel-var-id (car types)))
	     (sel-var (make-variable-expr bd))
	     (in-ex (make!-injection-application num sel-var cotup))
	     (aval (generate-coreduce-funtype-selection-value*
		    (car types) (car stypes) sel-var op codt struct type-alist))
	     (sel (mk-selection in-ex (list bd) (or aval *true*))))
	(generate-coreduce-funtype-selection-cotup-value*
	 (cdr types)
	 (cdr stypes)
	 cotup var op codt struct sel-var-id type-alist (1+ num)
	 (cons sel result)))))

(defmethod generate-coreduce-funtype-selection-value* ((te dep-binding) (ste dep-binding) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-value* (type te) (type ste) var op codt struct type-alist))

(defmethod generate-coreduce-funtype-selection-value* ((te recordtype) (ste recordtype) var op codt struct type-alist)
  (generate-coreduce-funtype-selection-rec-value*
   (fields te) (fields ste) var op codt struct type-alist))

(defun generate-coreduce-funtype-selection-rec-value* (fields sfields var op codt struct type-alist &optional result)
  (if (null fields)
      result
      (let* ((avar (make!-field-application (car sfields) var))
	     (aval (generate-coreduce-funtype-selection-value*
		   (type (car fields)) (type (car sfields)) avar op codt struct type-alist)))
	(generate-coreduce-funtype-selection-rec-value*
	 (cdr fields)
	 (substit (cdr sfields)
	   (acons (car fields) avar nil))
	 var op codt struct type-alist
	 (if aval
	     (if result
		 (make-conjunction (list result aval))
		 aval)
	     result)))))

(defvar *adt-subtypes* nil)

(defun find-adt-subtypes (adt)
  (let ((adt-type (adt-type-name adt))
	(*adt-subtypes* nil))
    (find-adt-subtypes* (constructors adt) adt-type)
    *adt-subtypes*))

(defmethod find-adt-subtypes* ((list list) adt-type)
  (dolist (x list)
    (find-adt-subtypes* x adt-type)))

(defmethod find-adt-subtypes* ((c adt-constructor) adt-type)
  (find-adt-subtypes* (arguments c) adt-type))

(defmethod find-adt-subtypes* ((a adtdecl) adt-type)
  (find-adt-subtypes* (type a) adt-type))

(defmethod find-adt-subtypes* (type adt-type)
  (mapobject #'(lambda (ex)
		 (cond ((and (subtype? ex)
			     (not (expr-as-type? ex))
			     (tc-eq adt-type (find-supertype ex)))
			(pushnew ex *adt-subtypes* :test #'tc-eq)
			t)
		       ((datatype-subtype? ex)
			(find-adt-subtypes* (declared-type ex) adt-type)
			nil)))
	     type))

(defmethod decl-formals ((adt recursive-type))
  nil)

(defun subst-adt-type (te thinst adt)
  (if thinst
      (subst-mod-params te thinst
	(current-theory) (declaration (adt-type-name adt)))
      te))
