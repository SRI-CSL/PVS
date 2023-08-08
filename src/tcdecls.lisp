;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tcdecls.lisp -- 
;; Author          : Sam Owre
;; Created On      : Mon Oct 18 22:45:21 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Wed Nov  4 17:04:21 1998
;; Update Count    : 90
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

(export '(typecheck-decls typecheck-decl set-visibility add-judgement-decl
	  make-formals-funtype))

;;; Typecheck* methods for declarations - all of these methods have a
;;; declaration instance and a context for input, and they all check for
;;; duplication, set the type or type-value of the decl as appropriate,
;;; and return the decl.  Any other behavior is specific to the decl
;;; class, and described with the corresponding method below.

(defun typecheck-decls (decls)
  (when decls
    (let ((decl (car decls)))
      (setf (current-declaration) decl)
      (typecase decl
	(declaration (typecheck-decl decl))
	(importing (tcdebug "~%    Processing ~a" decl)
		   (let ((stime (get-internal-real-time)))
		     (dolist (d (generated decl))
		       (when (tcc-decl? d)
			 (if (memq d (assuming (current-theory)))
			     (setf (assuming (current-theory))
				   (delete d (assuming (current-theory))))
			     (setf (theory (current-theory))
				   (delete d (theory (current-theory)))))))
		     (setf (generated decl) nil)
		     (let ((*generating-adt* nil))
		       (unwind-protect
			    (typecheck* decl nil nil nil)
			 (unless (saved-context decl)
			   (cleanup-typecheck-decls decl))))
		     (tcdebug " - ~d msec" (realtime-since stime))))
	(recursive-type (cond ((typechecked? decl)
			       (mapc #'(lambda (d) (add-decl d nil))
				     (generated decl)))
			      (t
			       (tcdebug "~%    Processing (Co)Datatype ~a"
					(id decl))
			       (unwind-protect
				   (typecheck* decl nil nil nil)
				 (cleanup-datatype decl))))))
      ;;(sort-and-rename-tccs decl)
      (typecheck-decls (cdr decls)))))

;; (defun sort-and-rename-tccs (decl)
;;   (let ((dtccs (reverse (remove-if (complement #'tcc?) (generated decl)))))
;;     (when (cdr dtccs)
;;       (break)))
;;   )


(defun typecheck-decl (decl)
  ;; This check is needed because typechecking an importing may remove
  ;; declarations from the current theory, but the decls list to
  ;; typecheck-decls will not be affected, and TCCs may be accidentally
  ;; added.
  (when (or (symbolp (generated-by decl))
	    (and (declaration? (generated-by decl))
		 (typechecked? (generated-by decl))))
    (cond ((typechecked? decl)
	   (typecase decl
	     (mod-decl
	      (dolist (d (generated decl))
		(if (importing? d)
		    (typecheck-using* (get-theory (theory-name d)) (theory-name d))
		    (let ((dhash (current-declarations-hash)))
		      (dolist (id (id-suffixes (id d)))
			(pushnew d (get-lhash id dhash) :test #'eq)))))
	      (put-decl decl))
	     (theory-abbreviation-decl
	      (let* ((modinst (theory-name decl))
		     (mod (get-theory modinst)))
		(add-exporting-with-theories mod modinst)
		(add-to-using modinst)))
	     (conversion-decl
	      (push decl (current-conversions)))
	     (judgement
	      (add-judgement-decl decl))
	     (auto-rewrite-decl
	      (add-auto-rewrite-to-context decl)))
	   (unless (mod-decl? decl)
	     (put-decl-and-generated decl)))
	  (t (unwind-protect
		  (progn
		    (reset-beta-cache)
		    (assert (typep (current-theory) '(or module recursive-type)))
		    (setf (module decl) (current-theory))
		    (assert (module decl))
		    (tcdebug "~%    Typechecking ~a" decl)
		    (let ((stime (get-internal-real-time)))
		      (typecheck* decl nil nil nil)
		      (setf (typecheck-time decl) (realtime-since stime))
		      (tcdebug " - ~d msec" (typecheck-time decl)))
		    (setf (typechecked? decl) t)
		    (unless (decl-formal? decl)
		      (put-decl decl))
		    (remove-pseudo-normalize-freevar-entries))
	       (unless (or *loading-prelude*
			   (typechecked? decl))
		 (cleanup-typecheck-decls decl)))))))

;;; When a declaration is not changed by retypechecking, the generated
;;; declarations still need to be added to the symbol-table, i.e.,
;;; (current-declarations-hash)
(defun put-decl-and-generated (decl)
  (dolist (d (generated decl))
    #+pvsdebug
    (assert (memq d (all-decls (current-theory))))
    (let ((*insert-add-decl* t))
      (add-decl d nil)
      (when (tcc? d) (push d *tccdecls*))))
  (put-decl decl)
  (regenerate-xref decl))

(defun cleanup-datatype (adt)
  (unless (typechecked? adt)
    (dolist (decl (generated adt))
      (remove-generated-decl decl))
    (setf (generated adt) nil)
    (untypecheck-theory adt)))

(defun remove-generated-decl (decl)
  (mapc #'remove-generated-decl (generated decl))
  (cond ((member decl (theory (current-theory)))
	 (setf (theory (current-theory))
	       (remove decl (theory (current-theory)))))
	((member decl (assuming (current-theory)))
	 (setf (assuming (current-theory))
	       (remove decl (assuming (current-theory))))))
;   (setf (gethash (id decl) (declarations (current-theory)))
; 	(remove decl
; 		(gethash (id decl) (declarations (current-theory)))))
  )

(defun cleanup-typecheck-decls (decl)
  (cond ((and (type-def-decl? decl)
	      (enumtype? (type-expr decl)))
	 (cleanup-datatype decl)
	 (setf (generated decl) nil))
	((generated-by decl)
	 (remove-generated-decl decl))
	((not (typechecked? decl))
	 (untypecheck-theory decl)
	 (reset-typecheck-caches))))

(defmethod typecheck* :around ((decl declaration) expected kind args)
   (declare (ignore expected kind args))
   (cond ((decl-formals decl)
	  (assert (every #'decl-formal? (decl-formals decl)))
	  (typecheck-decl-formals (decl-formals decl) decl)
	  ;;(typecheck* (decl-formals decl) nil nil nil)
	  (dolist (fdecl (decl-formals decl))
	    (set-visibility fdecl)
	    (setf (module fdecl) (current-theory))
	    (assert (eq (associated-decl fdecl) decl)))
	  (with-added-decls (decl-formals decl)
	    (let ((*tcc-conditions*
		   (add-decl-formal-existence-assumptions (decl-formals decl))))
	      (call-next-method)))
	  (assert (every #'(lambda (fml)
			     (not (memq fml (get-declarations (id fml)))))
			 (decl-formals decl))))
	 (t (call-next-method)))
   (setf (typechecked? decl) t)
   decl)

(defun add-decl-formal-existence-assumptions (dfmls &optional (conditions *tcc-conditions*))
  (if (null dfmls)
      conditions
      (add-decl-formal-existence-assumptions
       (cdr dfmls)
       (let ((dass (decl-formal-existence-assumption (car dfmls))))
	 (if dass
	     (cons dass conditions)
	     conditions)))))

(defun decl-formal-existence-assumption (dfml)
  (when (nonempty-type-decl? dfml)
    (let* ((type (type-value dfml))
	   (var (make-new-variable '|x| type))
	   (*generate-tccs* 'none))
      (make!-exists-expr (list (make-bind-decl var type)) *true*))))
  

(defun typecheck-decl-formals (dfmls decl)
  (when dfmls
    (let ((dfml (car dfmls)))
      (with-added-decls (list dfml)
	(setf (associated-decl dfml) decl)
	(typecheck-decl dfml)
	(typecheck-decl-formals (cdr dfmls) decl)))))


(defmethod new-decl-formals ((decl declaration))
  (when (decl-formals decl)
    (let* ((dfmls (new-decl-formals* decl))
	   (dacts (mk-dactuals dfmls))
	   (th (or (module decl) (current-theory)))
	   (thinst (mk-modname (id th) nil nil nil dacts decl))
	   (res (mk-resolution th thinst nil)))
      (setf (resolutions thinst) (list res))
      (values dfmls dacts thinst))))

(defmethod new-decl-formals ((decl mapping-lhs))
  (when (decl-formals decl)
    (let* ((dfmls (new-decl-formals* decl))
	   (dacts (mk-dactuals dfmls))
	   (th (or (module decl) (current-theory)))
	   (thinst (mk-modname (id th) nil nil nil dacts))
	   (res (mk-resolution th thinst nil)))
      (setf (resolutions thinst) (list res))
      (values dfmls dacts thinst))))

(defmethod new-decl-formals ((imp importing))
  nil)

(defun new-decl-formals* (decl)
  (new-decl-formals** (decl-formals decl)))

(defun new-decl-formals** (dfmls &optional nfmls)
  (if (null dfmls)
      (nreverse nfmls)
      (new-decl-formals**
       (cdr dfmls)
       (with-added-decls nfmls
	 (let ((nfml (new-decl-formal (car dfmls))))
	   (cons nfml nfmls))))))

(defmethod new-decl-formal ((fml decl-formal-type) &optional id)
  (let ((nfml (copy fml
		'id (or id (id fml))
		'module (current-theory)
		'typechecked? nil
		'type nil
		'type-value nil
		'associated-decl nil)))
    ;; (typecheck-decl nfml)
    (typecheck* nfml nil nil nil)
    nfml))

;;; If just an id, create a formal-type-decl - no need for subtype at the
;;; moment (this is just for reduce with "range" parameter)
(defmethod new-decl-formal ((fml symbol) &optional id)
  (declare (ignore id))
  (let ((nfml (make-instance 'decl-formal-type
		:id fml
		:module (current-theory))))
    (typecheck* nfml nil nil nil)
    nfml))

;;; Typechecking formal declarations - if it is a type, a type-name
;;; instance is created, otherwise the declared type is typechecked.
;;; Note that the type slot is set to the result in either case (maybe
;;; there should be a type-value slot to handle TYPE parameters).

(defmethod typecheck* ((decl formal-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (with-current-decl decl
    (check-duplication decl)
    (let ((tn (mk-type-name (id decl))))
      (setf (resolutions tn)
	    (list (mk-resolution decl (current-theory-name) tn)))
      (setf (type decl) tn)
      (setf (type-value decl) tn))
    decl))

(defmethod typecheck* ((decl formal-nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl)
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl) decl)
  decl)

(defmethod typecheck* ((decl formal-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (let* ((tn (mk-print-type-name (id decl)))
	 (res (mk-resolution decl (current-theory-name) nil)))
    (setf (resolutions tn) (list res))
    (type-def-decl-value decl tn))
  (assert (type-value decl))
  decl)

(defmethod typecheck* ((decl formal-nonempty-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl)
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl) decl)
  decl)

(defmethod typecheck* ((decl formal-struct-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (let* ((tn (mk-print-type-name (id decl)))
	 (res (mk-resolution decl (current-theory-name) nil)))
    (setf (resolutions tn) (list res))
    (type-def-decl-value decl tn))
  decl)

(defmethod typecheck* ((decl formal-nonempty-struct-subtype-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl)
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl) decl)
  decl)

(defmethod typecheck* ((decl formal-type-appl-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  ;;(check-type-application-formals decl)
  (dolist (te (parameters decl))
    (typecheck* te nil nil nil)
    (let ((reses (remove-if (complement
			     #'(lambda (r)
				 (formal-type-decl? (declaration r))))
		   (resolutions te))))
      (cond ((cdr reses)
	     (type-error te "Type error"))
	    ((null reses)
	     (type-error te "Must resolve to a formal dependent type"))
	    (t (set-type te (type (car reses)))))))
  (let ((tn (make-self-resolved-type-name decl)))
    (setf (type-value decl) tn))
  decl)

(defmethod typecheck* ((decl formal-nonempty-type-appl-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (put-decl decl)
  (make-nonempty-assumption (type-value decl))
  (set-nonempty-type (type-value decl) decl)
  decl)


(defmethod typecheck* ((decl formal-const-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl)
	  (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (assert (fully-instantiated? (type decl)))
  (assert (or (null (print-type (type decl)))
	      (tc-eq (print-type (type decl)) (declared-type decl))))
  (if (free-params (type decl))
      (set-nonempty-type (type decl) decl)
      ;; Don't check here
      ;;(check-nonempty-type (type decl) (id decl))
      )
  (check-duplication decl)
  decl)

(defmethod typecheck* ((decl formal-theory-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; If we have an existing formal-theory-decl or mod-decl of the same
  ;; theory, and it has uninterpreted types, constants, or theories, then
  ;; we must create a separate copy.
  (let ((th (get-theory (id decl))))
    (when (and th (get-importings th))
      (type-error decl
	"Identifier ~a is already in use as a theory" (id decl))))
  (typecheck-inlined-theory decl)
  (unless (fully-instantiated? (theory-name decl))
    (type-error (theory-name decl)
      "Actual parameters must be provided for theory declarations"))
  (put-decl decl)
  (setf (saved-context decl) (copy-context *current-context*))
  ;; Mistake - do not do this to satisfy save-theories
  ;;(add-to-using (theory-name decl))
  decl)

;;; typecheck-named-theory is used for mod-decls and formal-theory-decls

;; (defun typecheck-named-theory (decl)
;;   (let ((theory-name (theory-name decl)))
;;     (when (and (null (library (theory-name decl)))
;; 	       (eq (id (theory-name decl)) (id (current-theory))))
;;       (type-error (theory-name decl)
;; 	"Formal theory declarations may not refer to the containing theory"))
;;     (let* ((theory (get-typechecked-theory theory-name))
;; 	   (tgt-name (target theory-name))
;; 	   (tgt-theory (when tgt-name (get-typechecked-theory tgt-name)))
;; 	   (abbr-info (if (or (formal-theory-decl? decl)
;; 			      (mappings theory-name)
;; 			      (target theory-name))
;; 			  ""
;; 			  (format nil
;; 			      "~%No mappings given in theory declaration ~a~%~
;;                               Perhaps this should be given the abbreviation form~
;;                               ~%  ~a"
;; 			    (id decl)
;; 			    (unparse (make-instance 'theory-abbreviation-decl
;; 				       :id (id decl)
;; 				       :theory-name theory-name)
;; 			      :string t)))))
;;       (unless (interpretable? theory)
;; 	(pvs-info
;; 	    "Theory ~a has no interpretable types, constants, or theories"
;; 	  theory-name))
;;       (unless (equal abbr-info "")
;; 	(pvs-info abbr-info))
;;       (let ((mappings (determine-implicit-mappings
;; 		       theory theory-name tgt-name tgt-theory)))
;; 	(when tgt-theory
;; 	  (typecheck-using (target theory-name)))
;; 	(typecheck-named-theory* theory
;; 				 (lcopy theory-name
;; 				   :mappings mappings
;; 				   :target nil)
;; 				 decl
;; 				 tgt-theory
;; 				 tgt-name)))))

(defun determine-implicit-mappings (theory theory-name tgt-name tgt-theory)
  (if tgt-theory
      (let ((*current-context* (copy-context *current-context*)))
	(typecheck-mappings (mappings theory-name) theory-name)
	(unless (resolution tgt-name)
	  (setf (resolutions tgt-name)
		(list (make-resolution tgt-theory tgt-name))))
	(add-to-using tgt-name tgt-theory)
	(determine-implicit-mappings*
	 (interpretable-declarations theory)
	 tgt-theory (target theory-name) (mappings theory-name)))
      (progn
	(typecheck-mappings (mappings theory-name) theory-name)
	(mappings theory-name))))

(defun determine-implicit-mappings* (decls tgt-theory target mappings
					   &optional imappings)
  (if (null decls)
      (nreverse imappings)
      (let ((mapping (or (find (id (car decls)) mappings
			       :key #'(lambda (m) (id (lhs m))))
			 (determine-implicit-mapping
			  (car decls) target tgt-theory))))
	(determine-implicit-mappings*
	 (cdr decls)
	 tgt-theory
	 target
	 mappings
	 (if mapping
	     (cons mapping imappings)
	     imappings)))))

(defun determine-implicit-mapping (decl target tgt-theory)
  (let* ((tgt-decls (remove-if (complement
				#'(lambda (d)
				    (and (declaration? d)
					 (eq (id d) (id decl))
					 (visible? d)
					 (eq (kind-of d) (kind-of decl)))))
		      (all-decls tgt-theory)))
	 (reses (unless tgt-decls
		  (let ((name (resolve (id decl) (kind-of decl) nil
				       (context tgt-theory))))
		    (when name (resolutions name))))))
    (cond ((and (null tgt-decls)
		(null reses))
	   (pvs-info "No implicit mapping found for ~a" (id decl)))
	  ((or (cdr tgt-decls)
	       (cdr reses))
	   (break "Need to refine"))
	  (t (mk-mapping
	      (mk-name (id decl) nil (id (module decl))
		       (make-resolution decl
			 (make-theoryname (module decl))))
	      (let ((tdecl (or (car tgt-decls) (declaration (car reses)))))
		(typecase decl
		  (type-decl (typecheck*
			      (if tgt-decls
				  (mk-type-name (id decl)
				    (actuals target) (id target)
				    (make-resolution (or (car tgt-decls)
							 (declaration (car reses)))
				      target)
				    :mappings (mappings target)
				    :library (library target))
				  (mk-type-name (id tdecl)
				    (actuals (module-instance (car reses)))
				    (id (module-instance (car reses)))
				    (car reses)
				    :mappings (mappings (module-instance (car reses)))
				    :library (library (module-instance (car reses)))))
			      nil nil nil))
		  (module (if tgt-decls
			      (mk-modname (id decl) (actuals target)
					  (library target)
					  (mappings target))
			      (module-instance (car reses))))
		  (t (if tgt-decls
			 (mk-name-expr (id decl) (actuals target) (id target)
				       (make-resolution (car tgt-decls) target)
				       :mappings (mappings target)
				       :library (library target))
			 (mk-name-expr (id tdecl)
			   (actuals (module-instance (car reses)))
			   (id (module-instance (car reses)))
			   (car reses)
			   :mappings (mappings (module-instance (car reses)))
			   :library (library (module-instance (car reses)))))))))))))


(defun cleanup-mapped-axiom-tccs (thdecl cur-th src-th int-th)
  (let ((mapped-tccs (remove-if
			 (complement #'(lambda (d)
					 (and (mapped-axiom-tcc? d)
					      (eq (generated-by d) thdecl))))
		       (all-decls cur-th))))
    (mapobject #'(lambda (x)
		   (when (and (name? x)
			      (resolution x)
			      (eq (module (declaration x)) src-th))
		     (setf (module-instance (resolution x))
			   (mk-modname (id int-th)))
		     (setf (declaration (resolution x))
			   (find-associated-interpreted-declaration
			    (id x) int-th))
		     nil))
	       mapped-tccs)))

(defun find-associated-interpreted-declaration (id int-theory)
  (let ((pdecls (remove-if (complement
			    #'(lambda (d)
				(and (declaration? d)
				     (eq (id d) id))))
		  (all-decls int-theory))))
    (cond ((null pdecls)
	   (break "Something's wrong, notify pvs-bugs"))
	  ((cdr pdecls)
	   (break))
	  (t (car pdecls)))))
	   

(defmethod typecheck-named-theory* ((theory datatype) theory-name decl
				    tgt-theory tgt-name)
  (typecheck-named-theory* (adt-theory theory)
			   (copy theory-name :id (id (adt-theory theory)))
			   decl
			   tgt-theory tgt-name))

(defun find-local-theory-reference (expr &optional allowed-decls)
  (let ((local-ref nil)
	(place-ex nil))
    (mapobject #'(lambda (ex)
		   (or local-ref
		       (if (name? ex)
			   (setq place-ex nil)
			   (when (place ex)
			     (setq place-ex ex)
			     nil))
		       (when (and (name? ex)
				  (resolution ex)
				  (not (binding? (declaration ex)))
				  (not (var-decl? (declaration ex)))
				  (not (or (memq (declaration ex) allowed-decls)
					   (and (generated-by (declaration ex))
						(memq (generated-by (declaration ex))
						      allowed-decls))))
				  (eq (module (declaration ex))
				      (current-theory)))
			 (setq local-ref (or place-ex ex)))))
	       expr)
    local-ref))

(defmethod module ((th module))
  th)

(defvar *theory-mapping-renames* nil)

;;; Library Declarations

(defmethod typecheck* ((decl lib-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;;;(check-duplication decl)
  (let ((lib-path (get-library-path (lib-string decl))))
    (unless lib-path
      (type-error decl "Directory ~a does not exist" (lib-string decl)))
    (setf (lib-ref decl) (merge-pathnames lib-path))
    (dolist (d (get-declarations (id decl)))
      (when (and (lib-decl? d)
		 (not (file-equal (lib-ref decl) (lib-ref d))))
	(if (eq (module d) (current-theory))
	    (type-error decl
	      "Library id ~a declared earlier in this theory (~a) ~
               with a different path."
	      (id decl) (id (current-theory)))
	    (pvs-warning
		"Library id ~a declared in imported theory ~a ~
               with a different path."
	      (id decl) (id (module d))))))))


;;; Module declarations

(defmethod typecheck* ((decl mod-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (decl-formals decl)
    (type-error decl
      "Declaration formals are not allowed in theory declarations"))
  (let ((th (get-theory (id decl))))
    (when (and th (get-importings th))
      (type-error decl
	"Identifier ~a is already in use as a theory" (id decl))))
  (let ((*generate-tccs* 'none))
    (typecheck* (modname decl) nil nil nil))
  ;; Note that this updates the declarations-hash, before creating the
  ;; inlined theory.
  (typecheck-using (modname decl))
  (typecheck-inlined-theory decl)
  (unless (fully-instantiated? (modname decl))
    (type-error (modname decl)
      "Actual parameters must be provided for theory declarations"))
  (assert (resolution (modname decl)))
  (put-decl decl)
  ;;(add-to-using (expanded-theory-name (modname decl)))
  (setf (saved-context decl) (copy-context *current-context*))
  ;; Need to allow id to be used as abbreviation
  )

(defun typecheck-inlined-theory (thdecl)
  (assert (typep thdecl '(or mod-decl formal-theory-decl modname)))
  (let ((theory-name (if (modname? thdecl)
			 thdecl
			 (theory-name thdecl))))
    (when (and (null (library (theory-name thdecl)))
	       (eq (id (theory-name thdecl)) (id (current-theory))))
      (type-error (theory-name thdecl)
	"Formal theory declarations may not refer to the containing theory"))
    (let* ((thname (expanded-theory-name theory-name)) ; walks down theory references
	   (theory (declaration thname))
	   (tgt-name (target thname))
	   (tgt-theory (when tgt-name (get-typechecked-theory tgt-name))))
      (inlined-theory-info thdecl theory thname) ; info messages, e.g., no mappings given
      (let* ((mappings (determine-implicit-mappings
			theory thname tgt-name tgt-theory))
	     (full-thname (lcopy thname :mappings mappings :target nil)))
	(when tgt-theory
	  (typecheck-using tgt-name))
	(set-type-actuals-and-maps full-thname theory)
	;; (when (mappings full-thname)
	;;   (unless (fully-instantiated? full-thname)
	;;     (type-error theory-name
	;; 	"Theory name ~a must be fully instantiated when mappings are given"
	;; 	theory-name)))
	(unless (fully-instantiated? full-thname)
	  (type-error theory-name
	    "Theory name ~a must be fully instantiated" theory-name))
	(typecheck-inlined-theory* theory full-thname thdecl)))))

(defun inlined-theory-info (thdecl theory thname)
  (let ((abbr-info
	 (if (or (formal-theory-decl? thdecl)
		 (mappings thname)
		 (target thname))
	     ""
	     (format nil "~%No mappings given in theory declaration ~a~%~
                          Perhaps this should be given the abbreviation form~
                          ~%  ~a"
	       (id thdecl)
	       (unparse (make-instance 'theory-abbreviation-decl
			  :id (id thdecl) :theory-name thname)
		 :string t)))))
    (unless (interpretable? theory)
      (pvs-info
	  "Theory ~a has no interpretable types, constants, or theories"
	(theory-name thdecl)))
    (unless (equal abbr-info "")
      (pvs-info abbr-info))))

;;; Given a theory name, get the fully expanded name
;;; For example,
;;;  A[T: TYPE]: THEORY ...
;;;  B[T: TYPE]: THEORY ... A1: THEORY = A[T]{{assn1}}
;;;  C: THEORY ... IMPORTING B; A2: THEORY = A1[int]{{assn2}}
;;; Then (expanded-theory-name (theory-name A2)) returns values
;;;   A[int]{{assn}} and theory A
;;; where assn is a merge of assn2 and assn1 and all substitutions have been made.
;;; This can obviously go recursively as deep as desired, with
;;; formal-theory-decls involved as well as theory decls.
(defun expanded-theory-name (thname)
  (assert *current-context*)
  (with-no-type-errors (get-typechecked-theory thname nil t))
  (typecheck* thname nil 'module nil)
  ;; If no error from typecheck*, then res should be a singleton
  ;; In the above, it would be B[int].A1[int]{{assn2}}
  (assert (resolution thname))
  (expanded-theory-name* (declaration thname) thname))

(defmethod expanded-theory-name* ((thref datatype-or-module) thname)
  (assert (eq (declaration thname) thref))
  thname)

(defmethod expanded-theory-name* ((thref theory-reference) thname)
  (let* ((next-thname (theory-name thref))
	 (rmappings (create-new-lhs-decls (mappings thname) next-thname))
	 (nmappings (append (mappings next-thname) rmappings)))
    (expanded-theory-name
     (lcopy next-thname :mappings nmappings))))

;;; From expanded-theory-name*; given th{{...}}, thname is the declaration
(defun create-new-lhs-decls (mappings thname)
  (let* ((thref (declaration thname))
	 (thinst (lcopy thname :mappings nil))
	 (decls (interpretable-declarations thref)))
    (mapcan #'(lambda (map)
		(let* ((decl (find (lhs map) decls :test #'same-id))
		       (nres (when decl (make-resolution decl thinst))))
		  (unless decl (break "No match for ~a" map))
		  (when decl
		    (list (copy map :lhs (copy (lhs map)
					   :resolutions (list nres)))))))
      mappings)))

;; (defun get-named-theory (theory-name)
;;   (or (get-theory theory-name)
;;       (let ((nm (resolve (id theory-name) 'module nil)))
;; 	(when nm
;; 	  (get-named-theory (modname (declaration nm)))))))

;; ;;; Completes the theory name.  Note that the initial mappings are probably
;; ;;; not typechecked, and merge assumes they are.  We can't actually
;; ;;; typecheck them until we know what theory intance this is, so we pull
;; ;;; them out, figure out the name, typecheck them, and merge them in.
;; (defun get-complete-theory-name (theory-name)
;;   (if (get-theory theory-name)
;;       theory-name
;;       (let ((nm (resolve (id theory-name) 'module nil)))
;; 	(when nm
;; 	  (let ((modname (modname (declaration nm))))
;; 	    (typecheck-mappings (mappings theory-name) modname)
;; 	    theory-name)))))

(defmethod typecheck-inlined-theory* ((theory module) theory-name decl)
  ;; (assert (typep thdecl '(or mod-decl formal-theory-decl)))
  (let ((*typecheck-using* theory-name)) ;; Provide information to tcc-gen
    (when (some #'(lambda (m) (mod-decl? (declaration (lhs m))))
		(mappings theory-name))
      (add-theory-mappings-importings theory theory-name))
    (when (some #'formal-theory-decl? (formals theory))
      (add-theory-parameters-importings theory theory-name))
    (when (mappings theory-name)
      (generate-mapped-axiom-tccs theory-name))
    ;; Inline the generated decls, but with nested identifiers
    (make-inlined-theory theory theory-name decl)))

(defun make-inlined-theory (theory theory-name thdecl)
  (assert (typep thdecl '(or mod-decl formal-theory-decl)))
  (let ((stheory (subst-mod-params-inlined-theory theory theory-name thdecl)))
    (assert (or (null (all-decls theory)) (not (eq stheory theory))))
    ;;(setf (theory-mappings thdecl) bindings)
    (setf (theory-mappings thdecl)
	  (mapcar #'(lambda (elt)
	    (if (typep (cdr elt) '(or type-def-decl const-decl theory-reference))
		(let* ((res (make-resolution (cdr elt) (current-theory-name)))
		  (nm (mk-name-expr (id (cdr elt)) nil nil res))
		  (act (make-instance 'actual :expr nm)))
		  (cons (car elt) act))
		elt))
	    (theory-mappings stheory)))
    (let ((fml-part (memq thdecl (formals (current-theory))))
	  (ass-part (memq thdecl (assuming (current-theory))))
	  (th-part (memq thdecl (theory (current-theory)))))
      (assert (or fml-part ass-part th-part))
      ;; Splice in the declarations in the right place
      (cond (fml-part
	     ;; Goes to the front of assuming or theory part
	     (if (assuming (current-theory))
		 (setf (assuming (current-theory))
		       (append (all-decls stheory) (assuming (current-theory))))
		 (setf (theory (current-theory))
		       (append (all-decls stheory) (theory (current-theory))))))
	    (ass-part
	     (let* ((rest (cdr ass-part))
		    (prev (ldiff (assuming (current-theory)) rest)))
	       (setf (assuming (current-theory))
		     (append prev (all-decls stheory) rest))))
	    (th-part
	     (let* ((rest (cdr th-part))
		    (prev (ldiff (theory (current-theory)) rest)))
	       (setf (theory (current-theory))
		     (append prev (all-decls stheory) rest)))))
      (dolist (decl (all-decls stheory))
	;; overwrite stheory
	(when (declaration? decl)
	  (setf (module decl) (current-theory)))
	(when (and fml-part (declaration? decl))
	  (setf (visible? decl) nil))
	(setf (generated-by decl) thdecl)
	(push decl (generated thdecl))
	;;(add-new-inlined-decl decl lastdecl part)
	;;(make-inlined-theory-decl decl)
	(when (declaration? decl)
	  (put-decl decl)
	  (assert (or (importing? decl)
		      (memq decl (get-declarations (id decl))))))))))

(defun subst-mod-params-inlined-theory (theory theory-name thdecl)
  "Does subst-mod-params for the whole theory, but with a fresh
all-subst-mod-params-caches.  It does this by resetting this in
*workspace-session*, then restoring it after the subst-mod-params call."
  ;; (assert (typep thdecl '(or mod-decl formal-theory-decl)))
  (let ((cur-all-subst-mod-params-caches
  	 (all-subst-mod-params-caches *workspace-session*)))
    ;; Need to ignore earlier substitutions, but restore after
    (setf (all-subst-mod-params-caches *workspace-session*)
  	  (make-pvs-hash-table :strong-eq? t))
    (unwind-protect (subst-mod-params theory theory-name theory thdecl)
      (setf (all-subst-mod-params-caches *workspace-session*)
  	    cur-all-subst-mod-params-caches))))

(defun apply-to-bindings (fn bindings-form)
  "Applies function fn to the elements of bindings that maintains the
dependencies.  Returns the new bindings plus an alist that can
be used for substit.  Every element of bindings is either a binding, or a
list of bindings; this is to allow the general form of definition and lamda
bindings."
  (typecase bindings-form
    (list (apply-to-bindings* fn bindings-form nil nil))
    (binding
     (let ((nb (funcall fn bindings-form)))
       (values nb (unless (eq nb bindings-form) (acons bindings-form nb nil)))))
    (t (funcall fn bindings-form))))

(defun apply-to-bindings* (fn bindings-form nbindings-form alist)
  (if (null bindings-form)
      (values nbindings-form alist)
      (typecase (car bindings-form)
	(binding
	 ;; Better to use fn, then substit, or the other way around?
	 (let* ((fbinding (funcall fn (car bindings-form)))
		(nbinding (substit fbinding alist)))
	   (apply-to-bindings*
	    fn (cdr bindings-form)
	    (nconc nbindings-form (list nbinding))
	    (if (eq nbinding (car bindings-form))
		alist
		(acons (car bindings-form) nbinding alist)))))
	(list
	 (multiple-value-bind (nnbindings-form nalist)
	     (apply-to-bindings* fn (car bindings-form) nil alist)
	   (apply-to-bindings*
	    fn (cdr bindings-form)
	    (nconc nbindings-form (list nnbindings-form))
	    nalist)))
	(t (let ((nex (substit (funcall fn (car bindings-form)) alist)))
	     (apply-to-bindings*
	      fn (cdr bindings-form) (nconc nbindings-form (list nex)) alist))))))

(defmethod typecheck-inlined-theory* ((theory datatype) theory-name decl)
  ;; Need to inline the datatype as well
  (let* (;;(*all-subst-mod-params-caches* nil)
	 (nadt (subst-mod-params theory theory-name theory)))
    (break "Need to deal with ~a" nadt)
    (typecheck-inlined-theory* (adt-theory theory)
			     (copy theory-name :id (id (adt-theory theory)))
			     decl)))


(defmethod typecheck* ((decl theory-abbreviation-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (check-duplication decl)
  (typecheck-using (theory-name decl))
  (assert (resolution (theory-name decl)))
  (put-decl decl)
  (setf (saved-context decl) (copy-context *current-context*))
  ;; Need to allow id to be used as abbreviation
  )


;;; Type-decl - there are three possibilities.  If there is no
;;; type-expression, then it is an uninterpreted type; the type-value
;;; slot is set to a newly created type-name.  If there is a type-expr,
;;; then it is typechecked.  If the defined? flag is set, then the
;;; type-value is set to the result, otherwise it is an uninterpreted
;;; subtype and a new subtype type-expression is created.  For example,
;;; if the decl is "t: type from int", then the subtype created is
;;; "lambda (x:int): x intype t".  See the type-expression methods for
;;; details on typechecking the type-expr slot of decl.

(defmethod typecheck* ((decl type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; (when (formals decl)
  ;;   (type-error decl "Uninterpreted types may not have parameters"))
  (check-duplication decl)
  (setf (type-value decl)
	(let* ((dacts (mk-dactuals (decl-formals decl)))
	       (tn (if *generating-adt*
		       (mk-adt-type-name (id decl) nil nil nil
					 *generating-adt* dacts)
		       (mk-type-name (id decl) nil nil nil :dactuals dacts))))
	  ;; ;; If there are decl-formals, the dactuals need to be typechecked
	  ;; (when (dactuals tn)
	  ;;   (typecheck* (dactuals tn) nil nil nil))
	  (let ((thinst (if dacts ; Slight optimization
			    (copy (current-theory-name))
			    (current-theory-name))))
	    (when dacts
	      (change-class thinst 'declparam-modname
		:dactuals (dactuals tn)
		:declaration decl))
	    (setf (resolutions tn) (list (mk-resolution decl thinst tn))))
	  tn))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defmethod typecheck* ((decl nonempty-type-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (call-next-method)
  (set-nonempty-type (type-value decl) decl)
  (unless (eq (id (current-theory)) '|booleans|)
    (put-decl decl)
    (generate-existence-axiom decl))
  decl)

(defun generate-existence-axiom (decl)
  (multiple-value-bind (dfmls dacts thinst)
      (new-decl-formals decl)
    (declare (ignore dacts))
    (let* ((id (makesym "~a_nonempty" (id decl)))
	   (edecl (mk-formula-decl id nil 'AXIOM nil dfmls))
	   (type (if thinst
		     (with-current-decl edecl
		       (subst-mod-params (type-value decl)
			   thinst (current-theory) decl))
		     (type-value decl)))
	   (var (make-new-variable '|x| type))
	   (form (mk-exists-expr (list (mk-bind-decl var type)) *true*))
	   (tform (with-current-decl edecl
		    (typecheck* form *boolean* nil nil))))
      (setf (definition edecl) tform)
      (typecheck* edecl nil nil nil)
      (pvs-info "Added existence AXIOM for ~a:~%~a"
	(id decl) (unparse edecl :string t))
      (add-decl edecl))))
    

(defmethod typecheck* ((decl type-def-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; If there are formals, they become parameters in a type-application
  (check-type-application-formals decl)
  (check-duplication decl)
  (let* ((tn (mk-print-type-name (id decl)))
	 (prntype (if (formals decl)
		      (make-instance 'print-type-application
			:type tn
			:parameters (mapcar #'mk-name-expr
				      (car (formals decl))))
		      tn))
	 (*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (tval (type-def-decl-value decl prntype)))
    (assert tval)
    #+pvsdebug
    (assert (fully-instantiated? tval))
    (setf (type-value decl) tval)
    (setf (resolutions tn) (list (mk-resolution decl (current-theory-name) nil)))
    (typecase (type-expr decl)
      (enumtype (typecheck* (type-expr decl) nil nil nil))
      (subtype (when (typep (predicate (type-expr decl)) 'expr)
		 (put-decl decl))))
    (when (and (nonempty-type-def-decl? decl)
	       (not (contains decl)))
      (let ((*bound-variables* (apply #'append (formals decl))))
	(check-nonempty-type-of decl)))
    (when (contains decl)
      (typecheck* (contains decl) tval nil nil)
      (set-nonempty-type (type-expr decl) decl)
      (set-nonempty-type tval decl)))
  (when *loading-prelude*
    (set-prelude-types (id decl) (type-value decl)))
  decl)

(defun check-type-application-formals (decl)
  (when (formals decl)
    (when (type-from-decl? decl)
      (type-error decl
	"Uninterpreted types may not have parameters"))
    (when (cdr (formals decl))
      (type-error (cdar (formals decl))
	"Type applications may not be curried"))
    (typecheck* (formals decl) nil nil nil)
    ;;(set-formals-types (apply #'append (formals decl)))
    ))

(defmethod check-nonempty-type-of ((decl nonempty-type-def-decl))
  (assert (type-value decl))
  (let ((ctype (copy (type-value decl) :print-type nil)))
    (check-nonempty-type ctype decl)
    (setf (nonempty? (type-value decl)) (nonempty? ctype))))

(defmethod check-nonempty-type-of ((decl nonempty-type-from-decl))
  (assert (type-value decl))
  (check-nonempty-type (supertype (type-value decl)) decl)
  (set-nonempty-type (type-value decl) decl)
  (put-decl decl)
  (generate-existence-axiom decl))

(defmethod type-def-decl-value (decl tn)
  (assert (type-decl? decl))
  (assert (typep tn '(or type-name type-application)))
  (assert (print-type-expr? tn))
  (cond ((enumtype? (type-expr decl))
	 ;; Note that the class of tn has changed, but still a type-name
	 (change-class tn 'adt-type-name
	   :adt (type-expr decl)
	   :single-constructor? (singleton? (constructors (type-expr decl))))
	 (set-nonempty-type tn decl)
	 tn)
	(t (let ((tval (typecheck* (type-expr decl) nil nil nil)))
	     (assert tval)
	     ;;(unless (print-type-expr? (type-expr decl)) ; could have been set above
	     (set-type (type-expr decl) nil)
	     ;;)
	     (when (has-type-vars? (type-expr decl))
	       (type-error (type-expr decl)
		 "Cannot determine the type associated with ~a:~%  Please provide more ~
                  information, i.e., actual parameters or a coercion."
		 (type-expr decl)))
	     (copy tval :print-type tn)))))

(defmethod type-def-decl-value ((decl type-from-decl) tn)
  (assert (type-decl? decl))
  (assert (typep tn '(or print-type-name print-type-application)))
  (when (enumtype? (type-expr decl))
    (type-error decl
      "Enumtype must be declared at top level"))
  (check-type-application-formals decl)
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (stype (typecheck* (type-expr decl) nil nil nil))
	 (utype (generate-uninterpreted-subtype decl stype)))
    (set-type (type-expr decl) nil)
    #+pvsdebug
    (assert (fully-instantiated? tn))
    (setf (supertype decl) stype)
    (setf (type-value decl) utype)
    (setf (print-type utype) tn)
    ;; (typecase tn
    ;;   (print-type-expr nil)		; nothing to do
    ;;   (type-name (change-class tn 'print-type-name
    ;; 		   :resolutions (list (copy (resolution tn) :type nil))))
    ;;   (expr-as-type (change-class tn 'print-expr-as-type))
    ;;   (type-application (change-class tn 'print-type-application))
    ;;   (t (break "huhh?")))
    utype))

(defmethod type-def-decl-value ((decl struct-subtype-decl) prtype)
  (assert (type-decl? decl))
  (assert (print-type-expr? prtype))
  (assert (null (type (resolution prtype))))
  (when (enumtype? (type-expr decl))
    (type-error decl
      "Enumtype must be declared at top level"))
  (check-type-application-formals decl)
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (stype (typecheck* (type-expr decl) nil nil nil))
	 (utype (generate-uninterpreted-projtype decl stype prtype)))
    (set-type (type-expr decl) nil)
    (setf (supertype decl) stype)
    (setf (print-type utype) prtype)
    (setf (type-value decl) utype)
    utype))


;;; Units-decl

(defparameter *fundamental-unit-ids*
  '(metre kilogram second coulomb candle degree_kelvin radian))

(defmethod typecheck* ((decl units-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl
      "Unit declarations may not have parameters"))
  (check-duplication decl)
  (let ((pos (position (id decl) *fundamental-unit-ids*)))
    (cond (pos
	   (unless (eq (id (current-theory)) 'units)
	     (type-error decl "May not redefine fundamental units"))
	   (setf (scale (declared-units decl)) 1)
	   (setf (dimensionality (declared-units decl))
		 (make-array (length *fundamental-unit-ids*) :initial-element 0))
	   (setf (svref (dimensionality (declared-units decl)) pos) 1))
	  (t (typecheck* (declared-units decl) nil nil nil))))
  (setf (type-value decl) *real*))

(defmethod typecheck* ((ue units-appl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (case (operator ue)
    (* (let ((lhs (if (numberp (args1 ue))
		      (args1 ue)
		      (typecheck* (args1 ue) nil nil nil)))
	     (rhs (if (numberp (args2 ue))
		      (args2 ue)
		      (typecheck* (args2 ue) nil nil nil))))
	 (if (numberp lhs)
	     (if (numberp rhs)
		 (type-error ue "Units must include a name")
		 (setf (scale ue) (* lhs (scale rhs))
		       (dimensionality ue) (dimensionality rhs)))
	     (if (numberp rhs)
		 (setf (scale ue) (* rhs (scale lhs))
		       (dimensionality ue) (dimensionality lhs))
		 (setf (scale ue) (* (scale lhs) (scale rhs))
		       (dimensionality ue) (map 'vector #'+
						(dimensionality lhs)
						(dimensionality rhs)))))
	 ue))
    (/ (let ((lhs (if (numberp (args1 ue))
		      (args1 ue)
		      (typecheck* (args1 ue) nil nil nil)))
	     (rhs (if (numberp (args2 ue))
		      (args2 ue)
		      (typecheck* (args2 ue) nil nil nil))))
	 (if (numberp lhs)
	     (if (numberp rhs)
		 (type-error ue "Units must include a name")
		 (setf (scale ue) (/ lhs (scale rhs))
		       (dimensionality ue) (map 'vector #'- (dimensionality rhs))))
	     (if (numberp rhs)
		 (setf (scale ue) (/ (scale lhs) rhs)
		       (dimensionality ue) (dimensionality lhs))
		 (setf (scale ue) (/ (scale lhs) (scale rhs))
		       (dimensionality ue) (map 'vector #'-
						(dimensionality lhs)
						(dimensionality rhs)))))
	 ue))
    (t (break))))

(defmethod args1 ((ue units-appl))
  (car (arguments ue)))

(defmethod args2 ((ue units-appl))
  (cadr (arguments ue)))

(defmethod typecheck* ((ue units-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((res (remove-if-not #'(lambda (r)
				(units-decl? (declaration r)))
	       (resolve ue 'type nil))))
    (when (null res)
      (type-error ue "No resolution for unit name ~a" ue))
    (when (cdr res)
      (type-error ue "Ambiguous: "))
    (setf (resolutions ue) res)
    (let ((dunits (declared-units (declaration ue))))
      (setf (scale ue) (scale dunits)
	    (dimensionality ue) (dimensionality dunits)))
    ue))

;;; Var-decl

(defmethod typecheck* ((decl var-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  (let* ((*generate-tccs* 'none)
	 (dtype (typecheck* (declared-type decl) nil nil nil)))
    #+pvsdebug
    (assert (fully-instantiated? dtype))
    (setf (type decl) dtype))
  (set-type (declared-type decl) nil)
  (assert (null (freevars (type decl))))
  (unless (fully-instantiated? (type decl))
    (type-error (declared-type decl)
      "Could not determine the full theory instance"))
  (check-duplication decl)
  decl)


;;; Const-decl - if the constant has a definition, then it must be
;;; typechecked, and a formula must be generated which allows the
;;; definition to be referenced in proof declarations.

(defmethod typecheck* ((decl const-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (typecheck* (formals decl) nil nil nil)
    ;;(set-formals-types (apply #'append (formals decl)))
    )
  (assert (fully-instantiated? (formals decl)))
  (assert (fully-typed? (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil))))
    (set-type (declared-type decl) nil)
    #+badassert
    (assert (or (null (print-type rtype))
		(tc-eq (print-type rtype) (declared-type decl))))
    (assert (fully-instantiated? (declared-type decl)))
    (assert (fully-instantiated? rtype))
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (assert (fully-instantiated? (type decl)))
    (assert (null (freevars (type decl))))
    (unless (typep decl 'adt-constructor-decl)
      (if (definition decl)
	  (set-nonempty-type (type decl) decl)
	  (check-nonempty-type (type decl) decl)))
    (check-duplication decl)
    (if (definition decl)
	(let ((*tcc-conditions* (add-formals-to-tcc-conditions (formals decl))))
	  (typecheck* (definition decl) rtype nil nil)
	  (check-positive-types decl)
	  #+pvsdebug (assert (fully-instantiated? (definition decl)))
	  #+pvsdebug
	  (assert (every #'(lambda (fv)
			     (let ((fdecl (declaration fv)))
			       (or (field-decl? fdecl)
				   (memq fdecl *bound-variables*))))
			 (freevars (definition decl))))
	  (put-decl decl)
	  (make-def-axiom decl))
	(setf (positive-types decl) nil)))
  (assert (listp (positive-types decl)))
  decl)

(defun check-positive-types (decl)
  (if (or (formals-sans-usings (current-theory))
	  (decl-formals decl))
      ;; check-positive-types* essentially filters out those postypes
      ;; that are found to not be positive
      (let* ((aformals (append (remove-if-not #'formal-type-decl?
				 (formals-sans-usings (current-theory)))
			       (decl-formals decl)))
	     (dformals (apply #'append (formals decl)))
	     ;; Check the arg types
	     (pformals (check-positive-types* (mapcar #'type dformals)
					      dformals
					      decl nil aformals))
	     ;; Then the definition
	     (ptypes (mapcar #'type-value
		       (check-positive-types* (definition decl)
					      dformals
					      decl nil pformals))))
	(setf (positive-types decl) ptypes))
      (setf (positive-types decl) nil)))

(defmethod check-positive-types* ((te type-name) fargs decl bindings postypes)
  (declare (ignore fargs decl bindings))
  postypes)

(defmethod check-positive-types* ((te dep-binding) fargs decl bindings postypes)
  (check-positive-types* (type te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te subtype) fargs decl bindings postypes)
  (check-positive-types*
   (predicate te) fargs decl bindings
   (check-positive-types* (supertype te) fargs decl bindings postypes)))

(defmethod check-positive-types* ((te funtype) fargs decl bindings postypes)
  (check-positive-types*
   (range te) fargs decl
   (if (dep-binding? (domain te))
       (cons (domain te) bindings)
       bindings)
   (remove-if #'(lambda (pt)
		  (occurs-in pt (domain te)))
     postypes)))

(defmethod check-positive-types* ((te tupletype) fargs decl bindings postypes)
  (check-positive-types* (types te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te cotupletype) fargs decl bindings postypes)
  (check-positive-types* (types te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te recordtype) fargs decl bindings postypes)
  (check-positive-types* (fields te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te field-decl) fargs decl bindings postypes)
  (check-positive-types* (type te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te struct-sub-recordtype) fargs decl bindings postypes)
  (check-positive-types* (fields te) fargs decl bindings postypes))

(defmethod check-positive-types* ((te struct-sub-tupletype) fargs decl bindings postypes)
  (check-positive-types* (types te) fargs decl bindings postypes))


(defmethod check-positive-types* ((ex application) fargs decl bindings postypes)
  (check-positive-types* (operator ex) fargs decl bindings
			 (check-positive-types* (argument ex) fargs decl bindings
						postypes)))

(defmethod check-positive-types* ((ex tuple-expr) fargs decl bindings postypes)
  (check-positive-types* (exprs ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex binding-expr) fargs decl bindings postypes)
  (let ((bps (free-params (bindings ex))))
    (check-positive-types*
     (expression ex) fargs decl
     (append (bindings ex) bindings)
     (remove-if #'(lambda (pt) (memq pt bps)) postypes))))

(defmethod check-positive-types* ((ex list) fargs decl bindings postypes)
  (if (null ex)
      postypes
      (check-positive-types*
       (car ex) fargs decl bindings
       (check-positive-types* (cdr ex) fargs decl
			      (if (binding? (car ex))
				  (cons (car ex) bindings)
				  bindings)
			      postypes))))

(defmethod check-positive-types* ((ex cases-expr) fargs decl bindings postypes)
  (check-positive-types*
   (expression ex) fargs decl bindings
   (check-positive-types*
    (selections ex) fargs decl bindings
    (check-positive-types*
     (else-part ex) fargs decl bindings postypes))))

(defvar *ignore-bindings* nil)

(defmethod check-positive-types* ((ex selection) fargs decl bindings postypes)
  (check-positive-types*
   (constructor ex) fargs decl bindings
   (let ((*ignore-bindings* (append (args ex) *ignore-bindings*)))
     (check-positive-types*
      (expression ex) fargs decl (append (args ex) bindings) postypes))))

(defmethod check-positive-types* ((ex field-application) fargs decl bindings postypes)
  (check-positive-types* (argument ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex projection-application) fargs decl bindings postypes)
  (check-positive-types* (argument ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex injection-application) fargs decl bindings postypes)
  (check-positive-types* (argument ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex injection?-application) fargs decl bindings postypes)
  (check-positive-types* (argument ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex extraction-application) fargs decl bindings postypes)
  (check-positive-types* (argument ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex record-expr) fargs decl bindings postypes)
  (check-positive-types* (assignments ex) fargs decl bindings postypes))

(defmethod check-positive-types* ((ex update-expr) fargs decl bindings postypes)
  (check-positive-types*
   (expression ex) fargs decl bindings
   (check-positive-types*
    (assignments ex) fargs decl bindings postypes)))

(defmethod check-positive-types* ((ex assignment) fargs decl bindings postypes)
  (check-positive-types*
   (expression ex) fargs decl bindings
   (check-positive-types*
    (arguments ex) fargs decl bindings postypes)))
   


(defmethod check-positive-types* ((ex rational-expr) fargs decl bindings postypes)
  (declare (ignore fargs decl bindings))
  postypes)

(defmethod check-positive-types* ((ex field-assignment-arg) fargs decl bindings postypes)
  (declare (ignore fargs decl bindings))
  postypes)

(defmethod check-positive-types* ((ex projection-expr) fargs decl bindings postypes)
  (declare (ignore fargs decl bindings))
  postypes)

(defmethod check-positive-types* ((ex injection-expr) fargs decl bindings postypes)
  (declare (ignore fargs decl bindings))
  postypes)

(defmethod check-positive-types* ((ex name-expr) fargs decl bindings postypes)
  ;; if interpreted, check interpretation postypes
  ;; else any types appearing in actuals are not postypes
  ;; =, /=, if-then-else, are treated as interpreted
  (let ((ndecl (declaration ex)))
    (if (or (eq ndecl decl)
	    (memq ndecl fargs)
	    (memq ndecl *ignore-bindings*)
	    (and (eq (id ex) '=) (eq (id (module ndecl)) '|equalities|))
	    (and (memq (id ex) '(/= ≠)) (eq (id (module ndecl)) '|notequal|))
	    (and (eq (id ex) 'IF) (eq (id (module ndecl)) '|if_def|))
	    (and (null (formals-sans-usings (module decl)))
		 (null (decl-formals decl))))
	postypes
	(typecase ndecl
	  (const-decl
	   (let ((cpostypes (mapcar #'(lambda (pt)
					(cond ((type-name? pt) (declaration pt))
					      ((type-name? (print-type pt))
					       (declaration (print-type pt)))
					      (t (break "check"))))
			      (positive-types ndecl))))
	     (unless (listp cpostypes) (break "Not a list?"))
	     ;; Find actuals corresponding to cpostypes, and check positivity
	     ;; relative to that.  For example, T may be positive, but the
	     ;; corresponding actual is [D -> R], hence D is not positive.
	     (check-positive-types-actuals (module-instance ex)
					   ndecl
					   cpostypes
					   postypes)))
	  (formal-const-decl
	   (assert (eq (module ndecl) (current-theory)))
	   postypes)
	  (field-decl postypes)
	  (bind-decl
	   (assert (memq ndecl bindings))
	   postypes)
	  (dep-binding
	   (assert (memq ndecl bindings))
	   postypes)
	  (t (break "Need more here"))))))

(defun all-type-params (decl)
  (append (remove-if-not #'type-decl? (formals (module decl)))
	  (remove-if-not #'type-decl? (decl-formals decl))))

(defun check-positive-types-actuals (modinst ndecl cpostypes postypes)
  ;; modinst has actuals and dactuals, these relate cpostypes to postypes
  (let ((actuals (append (unless (eq (module ndecl) (current-theory))
			   (actuals modinst))
			 (dactuals modinst)))
	(formals (append (unless (eq (module ndecl) (current-theory))
			   (formals-sans-usings (module ndecl)))
			 (decl-formals ndecl))))
    (assert (= (length actuals) (length formals)))
    (check-positive-types-actuals* actuals formals cpostypes
				   (if (eq (module ndecl) (current-theory))
				       (intersection postypes cpostypes)
				       postypes))))

(defun check-positive-types-actuals* (actuals formals cpostypes postypes)
  (if (or (null actuals)
	  (null postypes))
      postypes
      (check-positive-types-actuals*
       (cdr actuals) (cdr formals) cpostypes
       (let ((afps (remove-if-not #'formal-type-decl?
		     (free-params (car actuals)))))
	 ;; In this case, we need to find those types in the actuals that
	 ;; appear in postypes for concreteness, suppose the formal is T,
	 ;; the actual is [D -> R], T is in cpostypes, and D and R are in
	 ;; postypes - then D is removed from postypes because it has a
	 ;; negative occurrence.
	 (remove-if #'(lambda (afp)
			(and (member (type afp) postypes :test #'tc-eq)
			     (or (not (memq (car formals) cpostypes))
				 (not (occurs-positively?
				       (type afp) (car actuals))))))
	   afps)))))

(defun positive-type-in-decl? (fml decl)
  (member fml (positive-types decl)
	  :test #'positive-type?))

(defun positive-type? (fml postype)
  (and (formal-type-decl? fml)
       (tc-eq (type-value fml) postype)))

(defun add-formals-to-tcc-conditions (formals &optional (conditions *tcc-conditions*))
  (if (null formals)
      conditions
      (add-formals-to-tcc-conditions
       (cdr formals)
       (add-bindings-to-tcc-conditions (car formals) conditions))))


;;; formals are generally of the form ((d11 ... d1n)...(dm1 ... dmk)),
;;; which generates a function type of the form
;;; [t11,...,t1n -> [... -> [tm1,...,tmk -> rtype] ...]]
;;; where the tij are the types of the dij.  Of course, dependencies
;;; have to be handled properly.  If a tij depends on an earlier dkl, then the
;;; dkl corresponds to a dep-binding, not just a type.

(defun make-formals-funtype (formals range)
  ;; formals is a list of lists, range is a type, possibly dependent on the
  ;; formals
  (let* ((*generate-tccs* 'none)
	 (domtypes (mapcar #'make-formals-domtype formals))
	 (bound-formals (remove-if #'(lambda (fv)
				       (not (some #'(lambda (fmlist)
						      (memq (declaration fv) fmlist))
						  formals)))
			  (mapcar #'declaration (freevars (cons range domtypes)))))
	 (dtypes (if bound-formals
		     (mapcar #'(lambda (dt fmls)
				 (if (some #'(lambda (fm)
					       (member fm bound-formals :key #'declaration))
					   fmls)
				     (let* ((base (if (cdr fmls) '|d| (id (car fmls))))
					    (newid (make-new-variable base
						     (cons range domtypes) nil
						     :exceptions bound-formals)))
				       (mk-dep-binding newid dt (print-type dt)))
				     dt))
		       domtypes formals)
		     domtypes))
	 (ftype (make-formals-ftype dtypes range formals bound-formals)))
    ftype))

(defun make-formals-domtype (formals &optional substs types)
  ;; Now we just have a simple list of formals, i.e. bind-decls
  (if (null formals)
      (if (cdr types)
	  (mk-tupletype (nreverse types))
	  (car types))
      (let* ((domtype (substit (type (car formals)) substs))
	     (in-rest? (member (car formals) (freevars (cdr formals))
			       :key #'declaration))
	     (newid (when in-rest? 
		      (make-new-variable (id (car formals)) (cdr formals) nil
					 :exceptions (list (car formals)))))
	     (dtype (if in-rest?
			(mk-dep-binding newid domtype)
			domtype))
	     (nsubsts (if in-rest?
			  (let ((dep-name (mk-name-expr newid nil nil
							(make-resolution dtype
							  (current-theory-name)
							  domtype))))
			    (acons (car formals) dep-name substs))
			  substs)))
	(assert (type-expr? domtype))
	(make-formals-domtype (cdr formals) nsubsts (cons dtype types)))))

(defun make-formals-ftype (dtypes range formals bound-formals &optional domains substs)
  "Given a declaration of the form
  f(x:X, y:Y(x))(z: Z(x, y)): R(x, y, z)
make-formal-ftype creates the funtype
  [d: [x: X, Y(x)] -> [d1: Z(d`1, d`2) -> R(d`1, d`2, d1)]]
The dependent types are created only when needed."
  (if (null dtypes)
      (let* ((*pseudo-normalizing* t)
	     (srange (substit range substs)))
	(mk-funtype* domains srange))
      (let ((dtype (let ((*pseudo-normalizing* t))
		     (substit (car dtypes) substs))))
	(if (dep-binding? dtype)
	    (let* ((fmls (car formals))
		   (dname (mk-dep-binding-name dtype)))
	      ;; update substs
	      (if (cdr fmls)
		  (dotimes (i (length fmls))
		    (when (memq (nth i fmls) bound-formals)
		      (let ((proj (make-projection-application (1+ i) dname)))
			(push (cons (nth i fmls) proj) substs))))
		  (push (cons (car fmls) dname) substs))
	      (make-formals-ftype (cdr dtypes) range (cdr formals) bound-formals
				  (cons dtype domains) substs))
	    (make-formals-ftype (cdr dtypes) range (cdr formals) bound-formals
				(cons dtype domains) substs)))))
			       

;;; Def-decl - this is the class for recursive definitions.  Checks that
;;; the type is a function type, typechecks the measure and checks that
;;; it is also a function and has the same domain and its range is nat.
;;; The decl is then added to the context prior to typechecking the
;;; definition, so that references to the definition can be resolved.
;;; Finally, a formula is generated for the definition.

(defmethod typecheck* ((decl def-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  ;;(set-formals-types (apply #'append (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil)))
	 (*recursive-subtype-term* nil))
    (set-type rtype nil) ;;NSH(12/17/19): was (defined-type decl)
    ;;same fix as const-decl
    #+badassert ;; print-type may have been pseudo-normalized
    (assert (or (null (print-type rtype))
		(tc-eq (print-type rtype) (declared-type decl))))
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (assert (fully-instantiated? (type decl)))
    #+pvsdebug (assert (null (freevars (type decl))))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "Recursive definition must be a function type"))
    (unless (typep decl 'adt-def-decl)
      (typecheck-measure decl)
      (assert (fully-instantiated? (measure decl)))
      (setf (recursive-signature decl) (compute-recursive-signature decl)))
    ;;(assert (null (freevars (recursive-signature decl))))
    (set-nonempty-type rtype decl)
    (put-decl decl)
    (let ((*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	  (*added-recursive-defn-conversions* nil))
      ;; See check-set-type-recursive-operator for how recursive conversions
      ;; are generated.
      (typecheck* (definition decl) rtype nil nil)
      (when *added-recursive-defn-conversions*
	(remove-recursive-defn-conversions *added-recursive-defn-conversions*)
	(mapobject #'(lambda (ex) (progn (when (typep ex '(or expr type-expr))
					   (setf (free-variables ex) 'unbound))
					 nil))
		   (definition decl))))
    (assert (fully-instantiated? (definition decl)))
    (check-positive-types decl)
    (make-def-axiom decl))
  decl)

(defun remove-recursive-defn-conversions (added-conversions)
  (when added-conversions
    (let* ((convex (car added-conversions))
	   (ex (from-expr convex))
	   (cex (copy ex)))
      (assert (or (name-expr? ex) (application? ex)) ()
	      "recursive-defn-conversion not on name-expr?")
      (typecase ex
	(name-expr
	 (change-class convex (class-of ex)
	   :type (type cex)
	   :mod-id (mod-id cex)
	   :library (library cex)
	   :actuals (actuals cex)
	   :dactuals (dactuals cex)
	   :acts-there? (acts-there? cex)
	   :dacts-there? (dacts-there? cex)
	   :id (id cex)
	   :mappings (mappings cex)
	   :target (target cex)
	   :resolutions (resolutions cex)))
	(application
	 (change-class convex (class-of ex)
	   :type (type cex)
	   :operator (operator cex)
	   :argument (argument cex)))
	(t (break "need a case here")))
      (assert (tc-eq convex cex) ()
	      "Error in remove-recursive-defn-conversions"))
    (remove-recursive-defn-conversions (cdr added-conversions))))

(defun set-formals-types (formals)
  (when formals
    (when (declared-type (car formals))
      (set-type (declared-type (car formals)) nil))
    (let ((*bound-variables* (cons (car formals) *bound-variables*)))
      (set-formals-types (cdr formals)))))


;;; Given a recursive function of the form
;;; f(x1:X1,..,xk:Xk)(y1:Y1,..,ym:Ym)(z1:Z1,..,zn:Zn): RECURSIVE R =
;;;    ...
;;;  MEASURE M BY <
;;; Computes
;;;  [a:[X1,..,Xk] -> [b:[Y1,..,Ym] ->
;;;     [{c:[Z1,..,Zn] | M(a)(b)(c) < M(x1,..,xk)(y1,..,ym)(z1,..,zn)} -> R]]]
;;; In general, of course, there may be dependencies that we need to take
;;; care of.
(defun compute-recursive-signature (decl)
  (assert (measure-depth decl))
  (compute-recursive-signature* (type decl) (measure-depth decl) decl))

(defun compute-recursive-signature* (type depth decl &optional domtypes)
  (if (= depth 0)
      (let* ((dom (if (dep-binding? (domain type))
		      (type (domain type))
		      (domain type)))
	     (vid (make-new-variable '|z| (cons decl domtypes)))
	     (bd (make!-bind-decl vid dom))
	     (avar (let ((var (make-variable-expr bd)))
		     (set-extended-place var (measure decl)
					 "creating var ~a for domain ~a"
					 var dom)
		     var))
	     (arg1 (make-lhs-measure-application
		    (measure decl) (reverse domtypes) avar decl))
	     (arg2 (make-rhs-measure-application decl))
	     (appl (if (ordering decl)
		       (make!-application (copy (ordering decl)) arg1 arg2)
		       (typecheck* (mk-application '< arg1 arg2)
				   *boolean* nil nil)))
	     (pred (make!-lambda-expr (list bd) appl))
	     (subtype (mk-setsubtype dom pred))
	     (fvars (mapcar #'declaration
		      (freevars (cons subtype (cons (range type) domtypes)))))
	     (clean-domtypes (mapcar #'(lambda (dtype)
					 (if (memq dtype fvars)
					     dtype
					     (type dtype)))
			       domtypes)))
	(setq *recursive-subtype-term* arg2)
	(if (dep-binding? (domain type))
	    (let ((dsubtype (mk-dep-binding
			     (make-new-variable (id (domain type))
			       (freevars (range type))) subtype)))
	      (mk-funtype* (cons dsubtype clean-domtypes)
			   (substit (range type)
			     (acons (domain type) dsubtype nil))))
	    (mk-funtype* (cons subtype clean-domtypes) (range type))))
      (if (dep-binding? (domain type))
	  (let ((ndep (mk-dep-binding (make-new-variable (id (domain type))
					(freevars (range type)))
				      (type (domain type)))))
	    (compute-recursive-signature*
	     (substit (range type) (acons (domain type) ndep nil))
	     (1- depth) decl
	     (cons ndep domtypes)))
	  (compute-recursive-signature*
	   (range type) (1- depth) decl
	   (cons (mk-dep-binding (make-new-variable '|z| (cons decl domtypes))
				 (domain type))
		 domtypes)))))

(defun make-lhs-measure-application (meas domtypes var decl)
  (assert (place meas))
  (if (null domtypes)
      (let ((mapp (make!-application meas var)))
	(set-extended-place mapp meas
			    "creating measure application ~a" mapp)
	mapp)
      (make-lhs-measure-application
       (let* ((dvar (make-variable-expr (car domtypes)))
	      (mapp (make!-application meas dvar)))
	 (set-extended-place dvar meas
			     "creating measure variable ~a" dvar)
	 (set-extended-place mapp meas
			     "creating measure application ~a" mapp)
	 mapp)
       (cdr domtypes) var decl)))

(defun make-rhs-measure-application (decl)
  (let ((abindings (append (formals decl)
			    (bindings* (definition decl))))
	(oargs (outer-arguments decl)))
    (mapc #'(lambda (args bindings)
	      (mapc #'(lambda (arg binding)
			(assert (place binding))
			(set-extended-place arg binding
					    "making arg ~a from binding ~a"
					    arg binding))
		    args bindings))
	  oargs abindings)
    (make!-recursive-application (measure decl) oargs)))

(defun recursive-calls-without-enough-args (decl)
  (let ((depth (measure-depth decl))
	(processed-names nil)
	(impoverished-names nil))
    (mapobject #'(lambda (ex)
		   (typecase ex
		     (name-expr
		      (when (and (eq (id ex) (id decl))
				 (not (memq ex processed-names)))
			(push ex impoverished-names)))
		     (application
		      (let ((op (operator* ex))
			    (args (argument* ex)))
			(when (and (name-expr? op)
				   (eq (id op) (id decl))
				   (not (memq op processed-names)))
			  (push op processed-names)
			  (unless (> (length args) depth)
			    (push op impoverished-names))))))
		   nil)
	       (definition decl))
    impoverished-names))

(defun typecheck-measure (decl)
  (let ((*bound-variables* (apply #'append (formals decl)))
	(*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	(stype (find-supertype (type decl)))
	(*generate-tccs* 'none))
    (typecheck* (declared-measure decl) nil nil nil)
    (if (some #'(lambda (pty)
		  (let ((sty (find-supertype pty)))
		    (and (typep sty 'funtype)
			 (compatible? (domain sty) (domain stype)))))
	      (ptypes (declared-measure decl)))
	(setf (measure decl) (declared-measure decl))
	(let ((nmeas (mk-measure-lambda-exprs
		      (formals decl) (declared-measure decl))))
	  (typecheck* nmeas nil nil nil)
	  (setf (place nmeas) (place (declared-measure decl)))
	  (setf (measure decl) nmeas)))
    (when (ordering decl)
      (typecheck* (ordering decl) nil nil nil)
      (let ((otypes (remove-if-not #'relation-type? (ptypes (ordering decl)))))
	(if otypes
	    (setf (types (ordering decl)) otypes)
	    (type-error (ordering decl)
	      "Order must be a relation (e.g. of type [T, T -> bool])")))))
  (let* ((stype (find-supertype (type decl)))
	 (ctypes (remove-if-not
		     #'(lambda (pty)
			 (let ((sty (find-supertype pty)))
			   (and (typep sty 'funtype)
				(compatible? (domain sty) (domain stype)))))
		   (ptypes (measure decl))))
	 (ftypes (or (remove-if-not
			 #'(lambda (pty)
			     (let ((sty (find-supertype pty)))
			       (tc-eq (domain sty) (domain stype))))
		       ctypes)
		     ctypes)))
    (if ftypes
	(set-measure-types (measure decl) ftypes (type decl))
	(if (types (measure decl))
	    (type-error (declared-measure decl) "Measure must be a function.")
	    (type-error (declared-measure decl)
	      "Could not determine the type of the measure function."))))
  (let* ((*bound-variables* nil)
	 (stype (find-supertype (type decl)))
	 (mtypes (mapcar #'find-supertype (ptypes (measure decl)))))
    (if (ordering decl)
	(typecheck-measure-with-ordering decl stype (measure decl) mtypes
					 (ordering decl))
	(typecheck-measure* decl stype (measure decl) mtypes))
    (assert (fully-instantiated? (measure decl)))))

(defun mk-measure-lambda-exprs (formals dmeasure)
  (let ((mformals (measure-lambda-formals (reverse formals) dmeasure)))
    (mk-lambda-exprs mformals dmeasure)))

(defun measure-lambda-formals (formals dmeasure)
  (if (or (null (cdr formals))
	  (some #'(lambda (fm) (id-occurs-in (id fm) dmeasure))
		(car formals)))
      (reverse formals)
      (measure-lambda-formals (cdr formals) dmeasure)))

(defun set-measure-types (meas types dtype)
  (let ((mtypes (get-measure-types meas types (domain dtype)))
	(mreses (when (typep meas 'name-expr)
		  (get-measure-reses meas (resolutions meas) (domain dtype)))))
    (setf (types meas) mtypes)
    (when mreses
      (setf (resolutions meas) mreses))))

(defun get-measure-types (meas types dtype &optional result)
  (if (null types)
      (if (null result)
	  (type-error meas "Could not determine the full theory instance")
	  result)
      (get-measure-types
       meas
       (cdr types)
       dtype
       (if (fully-instantiated? (car types))
	   (cons (car types) result)
	   (let ((bindings (mapcar #'(lambda (ff)
				       (list (declaration ff)))
				   (free-formals meas))))
	     (tc-match dtype (domain (car types)) bindings)
	     (if (and bindings (every #'cdr bindings))
		 (cons (subst-for-formals (car types) bindings) result)
		 result))))))

(defun get-measure-reses (meas reses dtype &optional result)
  (if (null reses)
      (if (null result)
	  (type-error meas "Could not determine the full theory instance")
	  result)
      (get-measure-reses
       meas
       (cdr reses)
       dtype
       (if (fully-instantiated? (car reses))
	   (cons (car reses) result)
	   (let ((bindings (mapcar #'(lambda (ff) (list (declaration ff)))
				   (free-formals meas))))
	     (tc-match dtype (domain (find-supertype (type (car reses))))
		       bindings)
	     (if (and bindings (every #'cdr bindings))
		 (cons (subst-for-formals (car reses) bindings) result)
		 result))))))


;;; The recursion here is to handle the situation where the ordering is a
;;; higher-order function; e.g.,
;;; foo: RECURSIVE [int -> [int -> nat]] = (LAMBDA i: (LAMBDA j: j - i))
;;;   MEASURE (LAMBDA i: f(i)) BY <<
;;; We want to go down the measure until we hit the domain type of the
;;; ordering.

;;; decl is the def-decl
;;; type is the type of the decl (recurses on the range)
;;; meas is the measure of the decl (recurses on the expression if lambda-expr)
;;; mtypes is the possible types of the meas (recurses on the ranges)
;;; ordering is the ordering of the decl
;;; doms is the domains collected as we recurse on the ranges

(defun typecheck-measure-with-ordering (decl type meas mtypes ordering
					     &optional doms)
  (let ((ftype (find-supertype type)))
    (unless (funtype? ftype)
      (type-error (measure decl)
	"Wrong number of arguments in measure"))
    (let* ((pranges
	    (mapcar #'(lambda (pty)
			(let ((sty (find-supertype pty)))
			  (copy ftype 'range (car (types (domain sty))))))
	      (ptypes ordering)))
	   (ctypes
	    (remove-if-not #'(lambda (rty)
			       (some #'(lambda (ty) (compatible? ty rty))
				     pranges))
	      mtypes)))
      (cond ((singleton? ctypes)
	     (let ((eftype (mk-funtype* doms (car ctypes))))
	       (assert (null (freevars eftype)))
	       (assert (fully-instantiated? eftype))
	       (setf (measure-depth decl) (length doms))
	       (typecheck (measure decl)
		 :expected eftype
		 :tccs 'all))
	     (assert (fully-instantiated? (measure decl)))
	     (typecheck-ordering decl))
	    ((funtype? ftype)
	     (let ((ptypes
		    (remove-if-not
			#'(lambda (mty)
			    (and (typep mty 'funtype)
				 (compatible? (domain ftype) (domain mty))))
		      mtypes)))
	       (if ptypes
		   (typecheck-measure-with-ordering
		    decl (range ftype)
		    (when (lambda-expr? meas) (expression meas))
		    (mapcar #'(lambda (mty)
				(if (and (dep-binding? (domain mty))
					 (dep-binding? (domain ftype)))
				    (substit (range mty)
				      (acons (domain mty) (domain ftype) nil))
				    (range mty)))
		      mtypes)
		    ordering
		    (cons (domain ftype) doms))
		   (measure-incompatible decl type meas mtypes))))
	    (t (type-error (measure decl)
		 "Measure must have range compatible with the domain of the ordering"))))))

(defun typecheck-ordering (decl)
  (let* ((ordering (ordering decl))
	 (measure (measure decl))
	 (mtype (range (find-supertype (type measure))))
	 (*bound-variables* (apply #'append (formals decl))))
    ;; The measure has already been typechecked.
    (typecheck-ordering* decl ordering mtype)))

(defun typecheck-ordering* (decl ordering mtype)
  (let ((expected (mk-funtype (list mtype mtype) *boolean*)))
    (cond ((some #'(lambda (ty) (compatible? ty expected)) (types ordering))
	   (let* ((otype (lift-measure-type-for-ordering mtype ordering nil))
		  (exp (mk-funtype (list otype otype) *boolean*)))
	     (typecheck ordering :expected exp :tccs 'all)
	     (generate-well-founded-tcc decl otype)))
	  ((typep (find-supertype mtype) 'funtype)
	   (typecheck-ordering* decl ordering (range (find-supertype mtype))))
	  (t (type-error ordering
	       "Ordering is incompatible with the measure.")))))

(defmethod lift-measure-type-for-ordering :around ((mtype type-expr) ordering
						   bindings)
  (if (freevars mtype)
      (let ((otype (call-next-method)))
	(check-for-ordering-freevars otype ordering bindings)
	otype)
      mtype))

(defun check-for-ordering-freevars (otype ordering bindings)
  (when (freevars otype)
    (let ((fvs (remove-if #'(lambda (fv)
			      (assq (declaration fv) bindings))
		 (freevars otype))))
      (when fvs
	(type-error ordering
	  "Order type has free variables ~{~a~^,~}: ~%  ~a"
	  fvs otype)))))

(defmethod lift-measure-type-for-ordering ((mtype type-expr) ordering bindings)
  (declare (ignore ordering bindings))
  mtype)

(defmethod lift-measure-type-for-ordering ((mtype funtype) ordering bindings)
  (let ((dom (domain mtype)))
    (check-for-ordering-freevars (if (dep-binding? dom) (type dom) dom)
				 ordering bindings)
    (let ((ran (lift-measure-type-for-ordering
		(range mtype) ordering
		(if (dep-binding? dom)
		    (acons dom dom bindings)))))
      (mk-funtype dom ran))))

(defmethod lift-measure-type-for-ordering ((mtype tupletype) ordering bindings)
  (let ((ntypes (lift-measure-type-for-ordering (types mtype) ordering
						bindings)))
    (mk-tupletype ntypes)))

(defmethod lift-measure-type-for-ordering ((list list) ordering bindings)
  (when list
    (let ((cartype (lift-measure-type-for-ordering
		    (dep-binding-type (car list)) ordering bindings)))
      (cons cartype
	    (lift-measure-type-for-ordering
	     (if (dep-binding? cartype)
		 (substit (cdr list) (acons (car list) cartype nil))
		 (cdr list))
	     ordering
	     (acons cartype (car list) bindings))))))
      
(defmethod lift-measure-type-for-ordering ((mtype subtype) ordering bindings)
  (lift-measure-type-for-ordering (supertype mtype) ordering bindings))


(defun typecheck-measure* (decl type meas mtypes &optional doms)
  (unless (funtype? type)
    (type-error (measure decl)
      "Wrong number of arguments in measure"))
  (let ((natrange (mk-funtype* doms (copy type :range *naturalnumber*)))
	(ordrange (when *ordinal*
		    (mk-funtype* doms (copy type 'range *ordinal*)))))
    (cond ((some #'(lambda (ty) (compatible? ty natrange))
		 mtypes)
	   (setf (measure-depth decl) (length doms))
	   (typecheck (measure decl) :expected natrange :tccs 'all))
	  ((and *ordinal*
		(some #'(lambda (ty) (compatible? ty ordrange))
		      mtypes))
	   (setf (measure-depth decl) (length doms))
	   (typecheck (measure decl) :expected ordrange :tccs 'all))
	  ((funtype? type)
	   (let ((ptypes (remove-if-not
			     #'(lambda (mty)
				 (and (typep mty 'funtype)
				      (nth-domain mty doms)
				      (compatible? (domain type)
						   (nth-domain mty doms))))
			   mtypes)))
	     (if ptypes
		 (typecheck-measure*
		  decl (range type)
		  (when (lambda-expr? meas) (expression meas))
		  ptypes
		  (cons (domain type) doms))
		 (measure-incompatible decl type meas mtypes))))
	  (t (type-error (measure decl)
	       "Measure must have range a naturalnumber or an ordinal")))))

(defmethod nth-domain ((ftype funtype) doms)
  (if (null doms)
      (domain ftype)
      (nth-domain (range ftype) (cdr doms))))

(defmethod nth-domain (ftype doms)
  (declare (ignore ftype doms))
  nil)

(defmethod measure-incompatible (decl type (meas lambda-expr) mtypes)
  (let ((ftypes (remove-if-not #'(lambda (mty) (typep mty 'funtype))
		  mtypes)))
    (if ftypes
	(measure-incompatible* (domain-types type)
			       (mapcar #'domain-types ftypes)
			       (bindings meas))
	(type-error (measure decl)
	  "Wrong number of arguments in measure"))))

(defun measure-incompatible* (dom mdoms bindings)
  (if (and (cdr dom)
	   (some #'(lambda (mdom)
		     (compatible? (car dom) (car mdom)))
		 mdoms))
      (measure-incompatible* (cdr dom) (mapcar #'cdr mdoms) (cdr bindings))
      (type-incompatible (car bindings)
			 (mapcar #'car mdoms)
			 (car dom))))

(defmethod measure-incompatible (decl type meas mtypes)
  (declare (ignore meas))
  (let ((ftypes (remove-if-not #'(lambda (mty) (typep mty 'funtype))
		  mtypes)))
    (if ftypes
	(type-error (measure decl)
	  "Incompatible domain types for measure~
           ~%     Found: ~{~a~^, ~}~%  Expected: ~a"
	  (mapcar #'domain mtypes) (domain type))
	(type-error (measure decl)
	  "Wrong number of arguments in measure"))))

(defun mk-funtype* (doms range)
  (if (null doms)
      range
      (mk-funtype* (cdr doms) (mk-funtype (car doms) range))))


;;; inductive-decl

(defmethod typecheck* ((decl fixpoint-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  ;;(set-formals-types (apply #'append (formals decl)))
  (let* ((*bound-variables* (apply #'append (formals decl)))
	 (*tcc-conditions* (add-formals-to-tcc-conditions (formals decl)))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil))))
    (set-type (declared-type decl) nil)
    (assert (or (null (print-type rtype))
		(tc-eq (print-type rtype) (declared-type decl))))
    (setf (type decl)
	  (make-formals-funtype (formals decl) rtype))
    (check-duplication decl)
    (unless (funtype? (find-supertype (type decl)))
      (type-error decl "(Co)Inductive definition must be a function type"))
    (unless (tc-eq (find-supertype (range* (type decl))) *boolean*)
      (type-error decl
	"(Co)Inductive definitions must have (eventual) range type boolean"))
    (set-nonempty-type rtype decl)
    (put-decl decl)
    (typecheck* (definition decl) rtype nil nil)
    (let* ((all-vars (ind-def-formals decl))
	   (fixed-vars (fixed-inductive-variables decl all-vars))
	   (rem-vars (remove-if #'(lambda (avar) (memq avar fixed-vars))
		       all-vars))
	   (pred-type (inductive-pred-type decl fixed-vars)))
      (check-inductive-occurrences decl pred-type fixed-vars rem-vars)
      (make-def-axiom decl)
      (make-inductive-def-inductions decl pred-type fixed-vars rem-vars))
    (check-positive-types decl))
  decl)

(defun inductive-pred-type (decl fixed-vars)
  (inductive-pred-type* (formals decl)
			(beta-reduce (definition decl))
			fixed-vars))

(defmethod inductive-pred-type* ((formals cons) ex fixed-vars)
  (let ((rtype (inductive-pred-type* (cdr formals) ex fixed-vars)))
    (ind-pred-type (car formals) fixed-vars rtype)))

;; (defun ind-pred-types (formals fixed-vars rtype)
;;   (if (null formals)
;;       rtype
;;       (let ((nrtype (ind-pred-types (cdr formals) fixed-vars rtype)))
;; 	(ind-pred-type (car formals) fixed-vars nrtype))))

(defun ind-pred-type (formals fixed-vars rtype)
  (let ((bds (remove-if #'(lambda (fm) (memq fm fixed-vars))
	       formals)))
    (if bds
	(make-formals-funtype (list bds) rtype)
	rtype)))

(defmethod inductive-pred-type* ((formals null) (ex lambda-expr) fixed-vars)
  (let ((rtype (inductive-pred-type* nil (expression ex) fixed-vars)))
    (ind-pred-type (bindings ex) fixed-vars rtype)))

(defmethod inductive-pred-type* ((formals null) ex fixed-vars)
  (declare (ignore fixed-vars))
  (assert (tc-eq (range* (type ex)) *boolean*))
  (type ex))


(defun check-inductive-occurrences (decl pred-type fixed-vars rem-vars)
  (unless (check-inductive-occurrences* (definition-body decl) decl 1)
    (let* ((pid (make-new-variable 'P (cons (definition decl) (formals decl))))
	   (bd (make-bind-decl pid pred-type))
	   (var (make-variable-expr bd))
	   (body (definition-body decl))
	   (pbody (subst-pred-for-ind var fixed-vars body decl))
	   (lbody (make-lambda-expr (list bd)
		    (if rem-vars
			(make-lambda-expr rem-vars pbody)
			pbody)))
	   (mono (make-monotonic-application lbody))
	   (ubody (if fixed-vars
		      (make-forall-expr fixed-vars mono)
		      mono))
	   (ndecl (typecheck* (mk-monotonicity-tcc (make-tcc-name) ubody)
			      nil nil nil)))
      (insert-tcc-decl 'monotonicity (mk-name-expr (id decl)) nil ndecl))))

(defun make-monotonic-application (lbody)
  ;; When we have monotonic? in the prelude, we will use this
  ;;  (typecheck* (mk-application '|monotonic?| lbody) *boolean* nil nil)
  
  ;; Until then, need to expand monotonic? directly, i.e., given lbody of 
  ;; the form "LAMBDA (p:pred[T]): expr", we generate
  ;; FORALL (p1, p2: pred[T]):
  ;;   (FORALL (x:T): p1(x) => p2(x))
  ;;       => (FORALL (x:T): lbody(p1)(x) => lbody(p2)(x))
  (let* ((pid1 (make-new-variable 'P lbody 1))
	 (pid2 (make-new-variable 'P lbody 2))
	 (ptype (type (car (bindings lbody))))
	 (bd1 (make-bind-decl pid1 ptype))
	 (bd2 (make-bind-decl pid2 ptype))
	 (var1 (make-variable-expr bd1))
	 (var2 (make-variable-expr bd2)))
    (if (tc-eq (find-supertype ptype) *boolean*)
	(make-forall-expr (list bd1 bd2)
	  (make-implication (make-implication var1 var2)
			    (make-implication
			     (beta-reduce (make-application lbody var1))
			     (beta-reduce (make-application lbody var2)))))
	(let* ((ante (make-monotonic-ante ptype var1 var2 lbody))
	       (succ (make-monotonic-succ
		      (beta-reduce (make-application lbody var1))
		      (beta-reduce (make-application lbody var2)))))
	  (make-forall-expr (list bd1 bd2)
	    (make-implication ante succ))))))

(defun make-monotonic-ante (ptype var1 var2 lbody &optional bds)
  (if (tc-eq (find-supertype ptype) *boolean*)
      (make-forall-expr (nreverse bds)
	(make-implication var1 var2))
      (let* ((xid (make-new-variable '|x| (cons lbody bds)))
	     (xbd (make-bind-decl xid (domain ptype)))
	     (xvar (make-variable-expr xbd))
	     (app1 (make-application var1 xvar))
	     (app2 (make-application var2 xvar)))
	(make-monotonic-ante
	 (range ptype) app1 app2 lbody (cons xbd bds)))))

(defmethod make-monotonic-succ ((body1 expr) (body2 expr) &optional bds)
  (make-forall-expr bds
    (make-ind-implication body1 body2)))

(defmethod make-monotonic-succ ((body1 lambda-expr) (body2 lambda-expr)
				&optional bds)
  (let* ((xbds (mapcar #'(lambda (bd) (make-bind-decl (id bd) (type bd)))
		 (bindings body1)))
	 (xvars (mapcar #'make-variable-expr xbds))
	 (app1 (beta-reduce (make-application* body1 xvars)))
	 (app2 (beta-reduce (make-application* body2 xvars))))
    (make-monotonic-succ app1 app2 (append bds xbds))))

;;; parity is 1 for positive context, -1 for negative, and 0 for unknown
;;; Returns T if all occurrences are known to be positive.

(defmethod check-inductive-occurrences* ((ex name-expr) decl parity)
  (declare (ignore parity))
  (not (eq (declaration ex) decl)))

(defmethod check-inductive-occurrences* ((ex application) decl parity)
  (cond
    ((or (conjunction? ex)
	 (disjunction? ex))
     (and (check-inductive-occurrences* (args1 ex) decl parity)
	  (check-inductive-occurrences* (args2 ex) decl parity)))
    ((implication? ex)
     (and (check-inductive-occurrences* (args1 ex) decl (- parity))
	  (check-inductive-occurrences* (args2 ex) decl parity)))
    ((negation? ex)
     (check-inductive-occurrences* (args1 ex) decl (- parity)))
    ((inductive-occurrence? ex (type decl) decl)
     (or (= parity 1)
	 (when (= parity -1)
	   (type-error ex
	     "Negative occurrences of inductive definitions are not allowed."))))
    (t (and (check-inductive-occurrences* (operator ex) decl 0)
	    (check-inductive-occurrences* (arguments ex) decl 0)))))

(defmethod check-inductive-occurrences* ((ex branch) decl parity)
  (and (check-inductive-occurrences* (condition ex) decl 0)
       (check-inductive-occurrences* (then-part ex) decl parity)
       (check-inductive-occurrences* (else-part ex) decl parity)))

(defmethod check-inductive-occurrences* ((ex cases-expr) decl parity)
  (and (check-inductive-occurrences* (expression ex) decl parity)
       (check-inductive-occurrences* (selections ex) decl parity)
       (or (null (else-part ex))
	   (check-inductive-occurrences* (else-part ex) decl parity))))

(defmethod check-inductive-occurrences* ((ex selection) decl parity)
  (check-inductive-occurrences* (expression ex) decl parity))


;;; inductive-occurrence? checks that the expression is a fully applied
;;; form of the inductive constant.

(defmethod inductive-occurrence? (ex (type subtype) decl)
  (inductive-occurrence? ex (find-supertype type) decl))

(defmethod inductive-occurrence? ((ex application) (type funtype) decl)
  (inductive-occurrence? (operator ex) (find-supertype (range type)) decl))

(defmethod inductive-occurrence? ((ex name-expr) (type type-name) decl)
  (eq (declaration ex) decl))

(defmethod inductive-occurrence? (ex type decl)
  (declare (ignore ex type decl))
  nil)

;;; partial-inductive-occurrence? is like inductive-occurrence?, but does
;;; not require the occurrence to be fully applied.

(defmethod partial-inductive-occurrence? (ex (type subtype) decl)
  (partial-inductive-occurrence? ex (find-supertype type) decl))

(defmethod partial-inductive-occurrence? ((ex application) (type funtype) decl)
  (partial-inductive-occurrence? (operator ex) (find-supertype (range type)) decl))

(defmethod partial-inductive-occurrence? ((ex name-expr) type decl)
  (declare (ignore type))
  (eq (declaration ex) decl))

(defmethod partial-inductive-occurrence? (ex type decl)
  (declare (ignore ex type decl))
  nil)

(defmethod check-inductive-occurrences* ((ex quant-expr) decl parity)
  (and (not (occurs-in decl (bindings ex)))
       (check-inductive-occurrences* (expression ex) decl parity)))

(defmethod check-inductive-occurrences* ((list list) decl parity)
  (or (null list)
      (and (check-inductive-occurrences* (car list) decl parity)
	   (check-inductive-occurrences* (cdr list) decl parity))))

(defmethod check-inductive-occurrences* (ex decl parity)
  (declare (ignore parity))
  (not (occurs-in decl ex)))


(defun make-inductive-def-inductions (decl pred-type fixed-vars rem-vars)
  (let* ((*generate-tccs* 'none)
	 (pid (make-new-variable 'P (definition decl)))
	 (wbd (make-bind-decl pid pred-type))
	 (sbd (make-bind-decl pid pred-type))
	 (wvar (make-variable-expr wbd))
	 (svar (make-variable-expr sbd))
	 (body (definition-body decl))
	 (wprem (weak-induction-hypothesis wvar fixed-vars rem-vars body decl))
	 (sprem (strong-induction-hypothesis svar fixed-vars rem-vars body decl))
	 (wconc (make-inductive-conclusion wvar rem-vars decl))
	 (sconc (make-inductive-conclusion svar rem-vars decl))
	 (wform (make!-forall-expr (append fixed-vars (list wbd))
		  (make!-implication wprem wconc)))
	 (sform (make!-forall-expr (append fixed-vars (list sbd))
		  (make!-implication sprem sconc)))
	 (wid (makesym "~a_weak_~ainduction" (op-to-id (id decl))
		       (if (inductive-decl? decl) "" "co")))
	 (uwid (unique-formula-id wid))
	 (sid (makesym "~a_~ainduction" (op-to-id (id decl))
		       (if (inductive-decl? decl) "" "co")))
	 (usid (unique-formula-id sid))
	 (wdecl (typecheck* (mk-formula-decl uwid wform 'AXIOM) nil nil nil))
	 (sdecl (typecheck* (mk-formula-decl usid sform 'AXIOM) nil nil nil)))
    (add-decl wdecl)
    (add-decl sdecl)))

(defun unique-formula-id (fid)
  (let ((decls (all-decls (current-theory))))
    (if (member fid decls
		:test #'(lambda (x y)
			  (and (formula-decl? y)
			       (eq x (id y)))))
	(unique-formula-id* fid 1 decls)
	fid)))

(defun unique-formula-id* (fid num decls)
  (let ((nfid (makesym "~a_~d" fid num)))
    (if (member nfid decls
		:test #'(lambda (x y)
			  (and (formula-decl? y)
			       (eq x (id y)))))
	(unique-formula-id* fid (1+ num) decls)
	nfid)))

(defmethod definition-body ((decl const-decl))
  (definition-body (beta-reduce (definition decl))))

(defmethod definition-body ((ex lambda-expr))
  (definition-body (expression ex)))

(defmethod definition-body ((ex expr))
  ex)

(defmethod ind-def-pred-type (pred-type (decl const-decl))
  (ind-def-pred-type pred-type (beta-reduce (definition decl))))

(defmethod ind-def-pred-type (pred-type (ex lambda-expr))
  (ind-def-pred-type (range pred-type) (expression ex)))

(defmethod ind-def-pred-type (pred-type (ex expr))
  pred-type)

;;; Walks down the body of the inductive definition looking for variables
;;; that never change, e.g., the R in
;;;   TC(R)(x, y): INDUCTIVE bool
;;;     = R(x,y) OR (EXISTS z: TC(R)(x,z) AND TC(R)(z,y))
;;; The inductive predicate P then replaces TC(R), rather than TC.
;;; This function copies the list of all variables, then with each recursive
;;; occurrence of the inductive defn, removes those whose arguments are not
;;; the same as the corresponding var.

(defun fixed-inductive-variables (decl vars)
  (let ((fvars (copy-list vars)))
    (mapobject #'(lambda (ex)
		   (when (partial-inductive-occurrence?
			  ex (find-supertype (type decl)) decl)
		     (let ((indargs (ind-def-arguments ex)))
		       (mapc #'(lambda (v a)
				 (unless (and (typep a 'name-expr)
					      (eq (declaration a) v))
				   (setq fvars (delete v fvars))))
			     vars indargs)
		       (dolist (v (nthcdr (length indargs) vars))
			 (setq fvars (delete v fvars))))
		     t))
	       (definition decl))
    fvars))

(defun ind-def-formals (decl)
  (apply #'append
	 (append (formals decl)
		 (ind-def-formals* (beta-reduce (definition decl))))))

(defmethod ind-def-formals* ((ex lambda-expr))
  (append (list (bindings ex))
	  (ind-def-formals* (expression ex))))

(defmethod ind-def-formals* (ex)
  (declare (ignore ex))
  nil)

(defmethod ind-def-arguments (ex)
  (declare (ignore ex))
  nil)

(defmethod ind-def-arguments ((ex application))
  (append (ind-def-arguments (operator ex)) (arguments ex)))

(defun weak-induction-hypothesis (nvar fixed-vars rem-vars expr decl)
  (if (null rem-vars)
      (if (inductive-decl? decl)
	  (make-ind-implication (subst-pred-for-ind nvar fixed-vars expr decl) nvar)
	  (make-ind-implication nvar (subst-pred-for-ind nvar fixed-vars expr decl)))
      (make-forall-expr rem-vars
	(if (inductive-decl? decl)
	    (make-ind-implication
	     (subst-pred-for-ind nvar fixed-vars expr decl)
	     (make-ind-appl nvar rem-vars))
	    (make-ind-implication
	     (make-ind-appl nvar rem-vars)
	     (subst-pred-for-ind nvar fixed-vars expr decl))))))

(defun make-ind-appl (pvar avars)
  (make-ind-appl* (type pvar) pvar
		  (if (binding? (car avars))
		      (mapcar #'make-variable-expr avars)
		      avars)))

(defun make-ind-appl* (ptype pappl avars)
  (if (null avars)
      pappl
      (cond ((compatible? (domain ptype) (type (car avars)))
	     (make-ind-appl* (range ptype)
			     (make-application pappl (car avars))
			     (cdr avars)))
	    ((tupletype? (domain ptype))
	     (let ((dvars (subseq avars 0 (length (types (domain ptype))))))
	       (assert (every #'(lambda (ty a) (compatible? ty (type a)))
			      (types (domain ptype)) dvars))
	       (make-ind-appl* (range ptype)
			       (make-application* pappl dvars)
			       (subseq avars (length (types (domain ptype)))))))
	    (t (error "Somthing's wrong - please send your spec to pvs-bugs@csl.sri.com")))))

(defun make-ind-implication (x y)
  (make-ind-implication* (type x) x y))

(defmethod make-ind-implication* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-forall-expr (list nbd)
      (make-ind-implication*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-implication* ((te subtype) x y)
  (make-ind-implication* (supertype te) x y))

(defmethod make-ind-implication* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make-implication x y))

(defun subst-pred-for-ind (nvar fixed-vars expr decl)
  (let ((*generate-tccs* 'none))
    (gensubst expr
      #'(lambda (ex)
	  (typecase ex
	    (application
	     (let ((rem-vars
		    (remove-if #'null
		      (mapcar #'(lambda (a)
				  (remove-if #'(lambda (x)
						 (and (typep x 'name-expr)
						      (memq (declaration x)
							    fixed-vars)))
				    a))
			(arguments* ex)))))
	       (if (null rem-vars)
		   (copy nvar)
		   (make!-recursive-application (copy nvar) rem-vars))))
	    (name-expr (copy nvar))))
      #'(lambda (ex)
	  (typecase ex
	    (application (let ((op (operator* ex)))
			   (and (typep op 'name-expr)
				(eq (declaration op) decl))))
	    (name-expr (eq (declaration ex) decl)))))))

(defun strong-induction-hypothesis (nvar fixed-vars rem-vars expr decl)
  (let ((*generate-tccs* 'none))
    (if (null rem-vars)
	(if (inductive-decl? decl)
	    (make-ind-implication
	     (subst-strong-pred-for-ind nvar fixed-vars expr decl)
	     nvar)
	    (make-ind-implication
	     nvar
	     (subst-strong-pred-for-ind nvar fixed-vars expr decl)))
	(make-forall-expr rem-vars
	  (if (inductive-decl? decl)
	      (make-ind-implication
	       (subst-strong-pred-for-ind nvar fixed-vars expr decl)
	       (make-ind-appl nvar rem-vars))
	      (make-ind-implication
	       (make-ind-appl nvar rem-vars)
	       (subst-strong-pred-for-ind nvar fixed-vars expr decl)))))))

(defun subst-strong-pred-for-ind (nvar fixed-vars expr decl)
  (let ((stype (find-supertype (type decl))))
    (gensubst expr
      #'(lambda (ex)
	  (let ((fmls (collect-decl-formals decl)))
	    (typecase ex
	      (application
	       (make-inductive-conjunction
		(copy ex)
		(make-ind-pred-application nvar ex fixed-vars fmls)
		(argument* ex)
		fmls
		(inductive-decl? decl)))
	      (name-expr
	       (make-inductive-conjunction ex nvar nil fmls
					   (inductive-decl? decl))))))
      #'(lambda (ex)
	  (typecase ex
	    (application
	     (partial-inductive-occurrence? ex stype decl))
	    (name-expr
	     (eq (declaration ex) decl)))))))

(defun make-ind-pred-application (pred ex fixed-vars fmls)
  (make-ind-pred-application* pred (argument* ex) fixed-vars fmls))

(defun make-ind-pred-application* (pred args fixed-vars formals
					&optional pargs)
  (if (null args)
      (if pargs
	  (make!-recursive-application pred (nreverse pargs))
	  pred)
      (make-ind-pred-application*
       pred (cdr args) fixed-vars (cdr formals)
       (let ((cargs (mapcan #'(lambda (arg fml)
				(unless (memq fml fixed-vars)
				  (list arg)))
		      (if (and (cdr (car formals))
			       (tuple-expr? (car args)))
			  (exprs (car args))
			  (list (car args)))
		      (car formals))))
	 (if cargs
	     (cons cargs pargs)
	     pargs)))))

(defun collect-decl-formals (decl)
  (append (formals decl)
	  (lambda-bindings* (definition decl))))

(defmethod lambda-bindings* ((ex lambda-expr))
  (cons (bindings ex)
	(lambda-bindings* (expression ex))))

(defmethod lambda-bindings* (ex)
  (declare (ignore ex))
  nil)

(defun make-inductive-conjunction (inddef pred args fmls ind?)
  (cond ((null fmls)
	 (if ind?
	     (make-ind-conjunction inddef pred)
	     (make-ind-disjunction inddef pred)))
	(args
	 (make-inductive-conjunction inddef pred (cdr args) (cdr fmls) ind?))
	(t (let* ((bds (mapcar #'(lambda (x)
				   (typecheck* (mk-bind-decl (id x)
						 (or (declared-type x)
						     (type x)))
					       nil nil nil))
			 (car fmls)))
		  (bvars (mapcar #'make-variable-expr bds)))
	     (make!-set-expr bds
	       (make-inductive-conjunction
		(make!-application* inddef bvars)
		(make!-application* pred bvars)
		(cdr args)
		(cdr fmls)
		ind?))))))

(defun make-ind-conjunction (x y)
  (make-ind-conjunction* (type x) x y))

(defmethod make-ind-conjunction* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-lambda-expr (list nbd)
      (make-ind-conjunction*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-conjunction* ((te subtype) x y)
  (make-ind-conjunction* (supertype te) x y))

(defmethod make-ind-conjunction* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make!-conjunction x y))

(defun make-ind-disjunction (x y)
  (make-ind-disjunction* (type x) x y))

(defmethod make-ind-disjunction* ((te funtype) x y)
  (let* ((nvid (make-new-variable '|x| (list x y)))
	 (nbd (make-bind-decl nvid (if (dep-binding? (domain te))
				       (type (domain te))
				       (domain te))))
	 (nvar (make-variable-expr nbd)))
    (make-lambda-expr (list nbd)
      (make-ind-disjunction*
       (if (dep-binding? (domain te))
	   (substit (range te) (acons (domain te) nvar nil))
	   (range te))
       (make-application x nvar)
       (make-application y nvar)))))

(defmethod make-ind-disjunction* ((te subtype) x y)
  (make-ind-disjunction* (supertype te) x y))

(defmethod make-ind-disjunction* ((te type-name) x y)
  (assert (tc-eq (find-supertype te) *boolean*))
  (make!-disjunction x y))

(defun make-inductive-conclusion (var rem-vars decl)
  (let* ((dname (mk-name-expr (id decl) nil nil
			      (make-resolution decl
				(theory-name *current-context*)
				(type decl))))
	 (dargs (mapcar #'(lambda (blist)
			    (mapcar #'mk-name-expr blist))
			(append (formals decl)
				(all-lambda-bindings
				 (beta-reduce (definition decl))))))
	 (pargs (mapcar #'make-variable-expr rem-vars))
	 (impl (labels ((make-appl (op args)
		          (if (null args)
			      op
			      (make-appl (make-application* op (car args))
					 (cdr args)))))
		 (if (inductive-decl? decl)
		     (make-ind-implication
		      (make-appl dname dargs)
		      (if (null pargs)
			  (copy var)
			  (make-ind-appl (copy var) pargs)))
		     (make-ind-implication
		      (if (null pargs)
			  (copy var)
			  (make-ind-appl (copy var) pargs))
		      (make-appl dname dargs))))))
    (if (null rem-vars)
	impl
	(make-forall-expr rem-vars impl))))

(defmethod all-lambda-bindings (expr)
  (declare (ignore expr))
  nil)

(defmethod all-lambda-bindings ((ex lambda-expr))
  (cons (bindings ex) (all-lambda-bindings (expression ex))))


;;; Formula-decl - the expected type is always boolean

(defmethod typecheck* ((decl formula-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (when (formals decl)
    (type-error decl "Formals are not allowed in ~(~a~)s" (type-of decl)))
  (check-duplication decl)
  (let ((cdecl (current-declaration)))
    (setf (current-declaration) decl)
    (typecheck* (definition decl) nil nil nil)
    ;; Could be "GHOST bool"
    (set-type (definition decl) *boolean*)
    (setf (current-declaration) cdecl))
  (let ((*generate-tccs* 'none))
    (setf (closed-definition decl)
	  (universal-closure (definition decl))))
  (when (eq (spelling decl) 'ASSUMPTION)
    (handle-existence-assuming-on-formals decl))
  decl)

(defun remove-defined-type-names (decl &optional (theory (current-theory)))
  (let ((refs (collect-references (definition decl))))
    (when (some #'(lambda (ref)
		    (and (type-def-decl? ref)
			 (eq (module ref) theory)))
		refs)
      ;;(change-class decl 'assuming-decl)
      ;;(setf (original-definition decl) (definition decl))
      (setf (definition decl)
	    (remove-defined-type-names* (definition decl) theory))
      (setf (closed-definition decl)
	    (remove-defined-type-names* (closed-definition decl) theory)))))

(defun remove-defined-type-names* (ex theory)
  (let ((*in-theory* theory))
    (declare (special *in-theory*))
    (gensubst ex
      #'remove-defined-type-name!
      #'remove-defined-type-name?)))

(defmethod remove-defined-type-name? ((ex type-expr))
  (declare (special *in-theory*))
  (and (type-name? (print-type ex))
       (let ((tdecl (declaration (print-type ex))))
	 (and (type-def-decl? tdecl)
	      (eq (module tdecl) *in-theory*)))))

(defmethod remove-defined-type-name! ((ex type-expr))
  (copy ex 'print-type nil))

(defmethod remove-defined-type-name? ((ex type-name))
  (declare (special *in-theory*))
  (let ((tdecl (declaration ex)))
    (and (type-def-decl? tdecl)
	 (eq (module tdecl) *in-theory*))))

(defmethod remove-defined-type-name! ((ex type-name))
  (copy-all (type-expr (declaration ex))))

(defmethod remove-defined-type-name? ((ex simple-decl))
  (declare (special *in-theory*))
  (and (type-name? (declared-type ex))
       (let ((tdecl (declaration (declared-type ex))))
	 (and (type-def-decl? tdecl)
	      (eq (module tdecl) *in-theory*)))))

(defmethod remove-defined-type-name! ((ex simple-decl))
  (declare (special *in-theory*))
  (copy ex
    'declared-type (remove-defined-type-names* (declared-type ex) *in-theory*)
    'type (remove-defined-type-names* (declared-type ex) *in-theory*)))

(defmethod remove-defined-type-name? (ex)
  (declare (ignore ex))
  nil)

(defun handle-existence-assuming-on-formals (decl)
  (when (and (typep (definition decl) 'exists-expr)
	     (tc-eq (expression (definition decl)) *true*)
	     (singleton? (bindings (definition decl)))
	     (typep (type (car (bindings (definition decl)))) 'type-name)
	     (memq (declaration (type (car (bindings (definition decl)))))
		   (formals (current-theory))))
    (set-nonempty-type (type (car (bindings (definition decl)))) decl)))


;;;  TYPE EXPRESSIONS  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These typecheck* methods verify that the
;;; type-expr makes sense, and return the canonical type expression;
;;; which is essentially the same type expression  with all of the type-names
;;; replaced by their definitions.  In each case, an attempt is made to
;;; return the original type-expr if it is already in canonical form.


(defmethod typecheck* ((type enumtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (id type) (id (declaration *current-context*)))
  (setf (module type) (current-theory))
  (call-next-method))


;;; TYPE-NAME - the name is resolved to a type-decl, and its type-value
;;; is returned.  If we are processing the prelude, then call
;;; set-prelude-types to set *boolean*, etc.

(defmethod typecheck* ((type type-name) expected kind arguments)
  (declare (ignore expected kind))
  (assert (or (null (resolution type)) (type (resolution type))))
  (unless (and (resolution type)
	       (null arguments))
    (call-next-method type nil 'type arguments)
    (set-type type nil)
    (let ((tval (type (resolution type))))
      (when (and (formals (declaration (resolution type)))
		 (null arguments))
	(type-error type "Type name must have arguments"))
      (assert tval)))
  (type (resolution type)))


(defmethod typecheck* ((te print-type-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (assert (resolution te))
  (resolution-type (resolution te)))

(defun resolution-type (res)
  (or (type res)
      (resolution-type* (declaration res) (module-instance res))))

(defun resolution-type* (decl mi)
  (let ((theory (module decl)))
    (typecase decl
      ((or type-decl formal-type-decl)
       ;;(let ((*free-bindings* (append (apply #'append (formals decl)) *free-bindings*)))
	 (if (fully-instantiated? mi)
	     (subst-mod-params (type-value decl) mi theory decl)
	     (type-value decl)))
       ;;)
      ((or expname typed-declaration simple-decl)
       (if (fully-instantiated? mi)
	   (subst-mod-params (type decl) mi theory decl)
	   (type decl))))))

(defmethod typecheck* ((te print-expr-as-type) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((etype (type (expr te)))
	 (stype (find-supertype etype))
	 (tval (mk-subtype (domain stype) (expr te))))
    (setf (print-type tval) te)
    tval))

(defmethod typecheck* ((type type-application) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (parameters type) nil nil nil)
  (let ((te (let ((*dont-worry-about-full-instantiations* t))
	      (typecheck* (type type) nil 'type (parameters type)))))
    (cond ((formal-type-appl-decl? (declaration (type type)))
	   (assert (eq (current-theory) (module (declaration (type type)))))
	   (let ((*generate-tccs* 'none)
		 (typeslist (subst-mod-params
			     (parameters (declaration (type type)))
			     (module-instance
			      (resolution (type type)))
			     (module (declaration (type type))))))
	     (set-type-for-application-parameters
	      (parameters type) typeslist))
	   (let ((tval (substit te
			 (pairlis (parameters (declaration (type type)))
				  (parameters type)))))
	     ;; tval now has the parameters
	     (unless (print-type tval)
	       (setf (print-type tval) type))
	     (when (or (nonempty-type-decl? (declaration (type type)))
		       (nonempty? type))
	       (setf (nonempty? tval) t)
	       (setf (nonempty? type) t))
	     tval))
	  (t
	   ;; Note that te is just the subtype associated with the
	   ;; type-application; the parameters have not yet been substituted
	   (unless (formals (declaration (type type)))
	     (type-error type "~a may not be used in a type application" te))
	   (unless (length= (car (formals (declaration (type type))))
			    (parameters type))
	     (type-error type "Wrong number of actuals in ~a" type))
	   (let ((*generate-tccs* 'none)
		 (typeslist (make-formals-type-app
			     (subst-mod-params
			      (formals (declaration (type type)))
			      (module-instance
			       (resolution (type type)))
			      (module (declaration (type type)))
			      (declaration (type type))))))
	     (set-type-for-application-parameters
	      (parameters type) (car typeslist)))
	   (let ((tval (substit te (pairlis (car (formals
						  (declaration (type type))))
					    (parameters type)))))
	     ;; tval now has the parameters
	     (unless (print-type tval)
	       (setf (print-type tval) type))
	     (when (or (nonempty-type-decl? (declaration (type type)))
		       (nonempty? type))
	       (setf (nonempty? tval) t)
	       (setf (nonempty? type) t))
	     tval)))))

(defun set-type-for-application-parameters (parameters types)
  (when parameters
    (let ((type (if (fully-instantiated? (car types))
		    (car types)
		    (instantiate-type-from-application-parameter
		     (car types) (car parameters)))))
      (set-type (car parameters) type)
      (set-type-for-application-parameters
       (cdr parameters)
       (if (dep-binding? (car types))
	   (substit (cdr types)
	     (acons (car types) (car parameters) nil))
	   (cdr types))))))

(defun instantiate-type-from-application-parameter (type param)
  (instantiate-type-from-param-types type (ptypes param) param))

(defun instantiate-type-from-param-types (type ptypes param &optional itypes)
  (if (null ptypes)
      (cond ((null itypes)
	     (type-error param
	       "Could not determine the theory instance for ~a" param))
	    ((cdr itypes)
	     (type-ambiguity param))
	    (t (car itypes)))
      (if (fully-instantiated? (car ptypes))
	  (let ((ity (find-parameter-instantiation type (car ptypes))))
	    (instantiate-type-from-param-types
	     type (cdr ptypes) param
	     (if (and (fully-instantiated? ity)
		      (not (member ity itypes :test #'tc-eq)))
		 (cons ity itypes)
		 itypes))))))

(defun make-formals-type-app (formals)
  (let ((typeslist (mapcar #'(lambda (fm)
			       (mapcar #'type fm)) formals)))
    (make-formals-type-app* (car formals) (cdr formals)
			    (car typeslist) (cdr typeslist)
			    nil nil)))

(defun make-formals-type-app* (formals formalslist types typeslist
				       tup tuples)
  (cond ((and (null formals) (null formalslist))
	 (nreverse (cons (nreverse tup) tuples)))
	((null formals)
	 (make-formals-type-app* (car formalslist) (cdr formalslist)
				 (car typeslist) (cdr typeslist)
				 nil (cons (nreverse tup) tuples)))
	(t (let* ((type (if (or (occurs-in (car formals) (cdr formals))
				(occurs-in (car formals) formalslist))
			    (mk-dep-binding (id (car formals))
					    (car types))
			    (car types)))
		  (alist (when (dep-binding? type)
			   (list (cons (car formals) type))))
		  (cdrtypes (when (cdr types) (substit (cdr types) alist)))
		  (ntypeslist (substit typeslist alist)))
	     (make-formals-type-app* (cdr formals) formalslist
				     cdrtypes ntypeslist
				     (cons type tup) tuples)))))


(defmethod typecheck* ((type expr-as-type) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (expr type) nil nil nil)
  (let* ((used-a-conversion? nil)
	 (ptypes (or (remove-if-not #'(lambda (ty)
					(and (not (from-conversion ty))
					     (let ((sty (find-supertype ty)))
					       (and (funtype? sty)
						    (tc-eq (find-supertype
							    (range sty))
							   *boolean*)))))
		       (types (expr type)))
		     (prog1 (look-for-expected-from-conversion (expr type))
		       (setq used-a-conversion? t))))
	 (ftypes (remove-if-not #'fully-instantiated? ptypes))
	 (types (if (typep (expr type) 'name-expr)
		    (let* ((reses (resolutions (expr type)))
			   (bres (bound-variable-resolution reses))
			   (breses (if bres (list bres) reses))
			   (nreses (filter-local-resolutions
				   (remove-if-not
				       #'(lambda (r)
					   (some #'(lambda (fty)
						     (tc-eq fty (type r)))
						 ftypes))
				     breses))))
		      (or (mapcar #'type nreses)
			  ftypes))
		    ftypes)))
    (unless (or (type (expr type))
		(singleton? types))
      (cond ((cdr types)
	     (type-ambiguity (expr type)))
	    (ptypes
	     (type-error type
	       "Could not determine the full theory instance"))
	    (t (type-error (expr type)
		 "Could not determine the predicate for this type: ~a"
		 type))))
    (unless used-a-conversion?
      (setf (types (expr type)) types))
    (let ((ftype (find-supertype (or (type (expr type))
				     (car types)))))
      (unless (and (funtype? ftype)
		   (tc-eq (find-supertype (range ftype)) *boolean*))
	(type-error (expr type) "Does not resolve to a predicate"))
      (set-type (expr type) ftype)
      (assert (fully-typed? (expr type)))
      (let* ((psexpr (pseudo-normalize (expr type)))
	     (tval (mk-subtype (domain ftype) psexpr))
	     (ptype (change-class (copy type) 'print-expr-as-type)))
	(setf (print-type tval) ptype)
	(setf (subtype-conjuncts tval) (collect-subtype-conjuncts tval))
	tval))))

(defun collect-subtype-conjuncts (subtype)
  (let ((conjs (collect-subtype-conjuncts* subtype)))
    (assert (or (null conjs)
		(and (bind-decl? (car conjs)) (every #'expr? (cdr conjs)))))
    (let ((exp-conjs (collect-expanded-conjuncts (cdr conjs))))
      (cons (car conjs) exp-conjs))))

(defun collect-expanded-conjuncts (conjs &optional exp-conjs)
  (if (null conjs)
      (nreverse exp-conjs)
      (let* ((ex (car conjs))
	     (op (when (application? ex) (operator ex)))
	     (decl (when (name-expr? op) (declaration op)))
	     (defn (when (and (typep decl '(and const-decl (not def-decl)))
			      (def-axiom decl))
		     (args2 (car (last (def-axiom decl))))))
	     (sdefn (when (and (lambda-expr? defn)
			       (conjunction? (expression defn)))
		      (subst-mod-params defn
			  (module-instance op) (module decl) decl)))
	     (nconjs (when sdefn
		       (collect-expanded-conjuncts
			(collect-conjuncts (make!-application sdefn (argument ex)))))))
	(collect-expanded-conjuncts
	 (cdr conjs)
	 (if nconjs
	     (nconc (nreverse nconjs) exp-conjs)
	     (cons ex exp-conjs))))))

(defmethod collect-subtype-conjuncts* ((te subtype))
  (let ((tconj (collect-subtype-conjuncts* (supertype te)))
	(pconj (collect-subtype-conjuncts* (predicate te))))
    (assert (or (null tconj) (and (bind-decl? (car tconj)) (every #'expr? (cdr tconj)))))
    (assert (or (null pconj) (and (bind-decl? (car pconj)) (every #'expr? (cdr pconj)))))
    (if tconj
	(if pconj
	    (append tconj (substit (cdr pconj)
			    (acons (car pconj) (car tconj) nil)))
	    tconj)
	pconj)))

(defmethod collect-subtype-conjuncts* ((te type-expr))
  nil)

;;; Lambda exprs are the interesting ones, as they are the only exprs with conjuncts.
;;; First we change to a single variable, e.g. λ (x, y, z): p(x, y, z) is changed to
;;; λ w: p(w`1, w`2, w`3).
(defmethod collect-subtype-conjuncts* ((ex lambda-expr))
  (if (cdr (bindings ex))
      (unless (fully-instantiated? ex)
	(when (singleton? (remove-duplicates (mapcar #'module (free-params ex))))
	  (let ((*current-context* (context (module (car (free-params ex))))))
	    (unless (fully-instantiated? ex) (break "Not fi"))
	    ;; create single-binding equivalent
	    (let* ((dtype (make-formals-domtype (bindings ex)))
		   (vid (make-new-variable '|t| dtype))
		   (bd (typecheck* (mk-bind-decl vid dtype) nil nil nil))
		   (var (make-variable-expr bd))
		   (prex (make!-projections var))
		   (nex (make!-lambda-expr (list bd)
			  (substit (expression ex) (mapcar #'cons (bindings ex) prex)))))
	      (if (conjunction? (expression nex))
		  (cons (car (bindings nex)) (collect-conjuncts (expression nex)))
		  (list (car (bindings nex)) (expression nex)))))))
      (if (conjunction? (expression ex))
	  (cons (car (bindings ex)) (collect-conjuncts (expression ex)))
	  (list (car (bindings ex)) (expression ex)))))

(defmethod collect-subtype-conjuncts* ((ex expr))
  ;; Not a lambda-expr - but still a predicate, e.g., p - returns (x: T p(x))
  (let ((*current-context* (if (fully-instantiated? ex)
			       *current-context*
			       (context (module (car (free-params ex)))))))
    (when (fully-instantiated? ex)
      (let* ((dtype (domain (type ex)))
	     (vid (make-new-variable '|x| dtype))
	     (bd (typecheck* (mk-bind-decl vid dtype) nil nil nil))
	     (var (make-variable-expr bd))
	     (app (make!-application ex var)))
	;; Could expand app defn, and collect conjuncts.
	(list bd app)))))

(defmethod look-for-expected-from-conversion ((expr application))
  (and (typep (operator expr) 'name-expr)
       (typep (resolution (operator expr)) 'lambda-conversion-resolution)
       (list (k-conv-type (resolution (operator expr))))))

(defmethod look-for-expected-from-conversion (expr)
  (declare (ignore expr))
  nil)


;;; SUBTYPE - typecheck* the supertype and the predicate, and return a
;;; subtype type-expr; either the original if typechecking the supertype
;;; returns the same, or a newly built one if it doesn't.  Note that the
;;; expected type for the predicate is canonical.  This is only for
;;; interpreted subtypes; uninterpreted subtypes are handled in
;;; typecheck* for type-decls.

(defmethod typecheck* ((type subtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (if (and (fully-typed? type)
	   (fully-instantiated? type))
      type
      (cond ((predicate type)
	     (unless (type (predicate type))
	       (typecheck* (predicate type) nil nil nil))
	     (let ((types (or (types (predicate type))
			      (list (type (predicate type))))))
	       (cond ((cdr types)
		      (type-ambiguity (predicate type)))
		     ((typep (find-supertype (car types)) 'funtype)
		      (let ((etype (mk-funtype (list (domain (find-supertype
							      (car types))))
					       *boolean*)))
			(unless (type (predicate type))
			  (set-type (predicate type) etype))))
		     (t (type-error type "Predicate expected here"))))
	     (let* ((stype (domain (find-supertype (type (predicate type)))))
		    (tval (mk-subtype stype (pseudo-normalize (predicate type)))))
	       (setf (supertype type) stype)
	       (setf (print-type tval) (print-type type))
	       (setf (subtype-conjuncts tval) (collect-subtype-conjuncts tval))
	       (setf (nonempty? tval) (nonempty? type))
	       tval))
	    (t (let ((stype (typecheck* (supertype type) nil nil nil)))
		 (if (named-type-expr? type)
		     (let ((tval (generate-uninterpreted-subtype
				  (declaration *current-context*)
				  stype)))
		       tval)
		     (type-error type
		       "Uninterpreted subtypes are only allowed at the top level")
		     ))))))

(defmethod typecheck* ((type setsubtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((stype (typecheck* (supertype type) nil nil nil))
	 (bindings (if (consp (formals type))
		       (formals type)
		       (list (formals type)))))
    (typecheck* bindings nil nil nil)
    (unless (supertype type)
      (setf (supertype type)
	    (if (singleton? bindings)
		(type (car bindings))
		(make-tupletype-from-bindings bindings))))
    (if (predicate type)
	(typecheck* (predicate type)
		    (mk-funtype (mapcar #'type bindings) (copy *boolean*))
		    nil nil)
	(setf (predicate type)
	      (let* ((*bound-variables* (append bindings *bound-variables*))
		     (form (typecheck* (formula type) *boolean* nil nil)))
		(make!-lambda-expr bindings form))))
    (let ((tval (if (and stype
			 (eq stype (supertype type)))
		    type
		    (mk-subtype (or stype (supertype type))
		      (predicate type)))))
      tval)))


;;; Checks whether the subtype is the current declaration.

(defun named-type-expr? (type)
  (and (typep (declaration *current-context*) 'type-decl)
       (eq type (type-expr (declaration *current-context*)))))


;;; Generates a new predicate and a new subtype based on that predicate.
;;; Given the type-decl "T: type from T1", the predicate generated is of
;;; the form "T_pred: [T1->bool]" and the subtype is of the form
;;; "from T1 with T_pred".

(defun generate-uninterpreted-subtype (decl stype)
  (multiple-value-bind (dfmls dacts thinst)
      (new-decl-formals decl)
    (let* ((pname (makesym "~a_pred" (id decl)))
	   (pdecl (mk-const-decl pname nil nil nil nil dfmls))
	   (ptype (if thinst
		      (with-current-decl pdecl
			(subst-mod-params stype thinst (current-theory) decl))
		      stype))
	   (ftype (with-current-decl pdecl
		    (typecheck* (mk-funtype (list ptype) *boolean*)
				nil nil nil)))
	   (pexpr (mk-name-expr pname))
	   (cdecl (current-declaration)))
      (setf (declared-type pdecl) ftype)
      (typecheck* pdecl nil nil nil) ;; This will set current-declaration
      (setf (current-declaration) cdecl)
      (setf (predicate decl) pdecl)
      (add-decl pdecl (not (formal-subtype-decl? decl)))
      (setf (dactuals pexpr) dacts)
      (typecheck* pexpr ftype nil nil)
      (mk-subtype stype pexpr))))

(defun formal-subtype? (type)
  (and (subtype? type)
       (type-name? (print-type type))
       (formal-subtype-decl? (declaration (print-type type)))))

;;; Generates a new uninterpreted type and an uninterpreted projection
;;; function from that type to the given stype.
(defun generate-uninterpreted-projtype (decl stype prtype)
  (multiple-value-bind (dfmls dacts thinst)
      (new-decl-formals decl)
    (declare (ignore dacts))
    (let* ((pname (makesym "~a_proj" (id decl)))
	   (pdecl (mk-const-decl pname nil nil nil nil dfmls))
	   (ptype (if thinst
		      (with-current-decl pdecl
			(subst-mod-params stype thinst (current-theory) decl))
		      stype))
	   (struct-subtype (generate-struct-subtype ptype prtype))
	   (surjname (mk-name-expr '|surjective?|
		       (list (mk-actual struct-subtype) (mk-actual stype))))
	   (surjtype (typecheck* (mk-expr-as-type surjname) nil nil nil))
	   (cdecl (declaration *current-context*))
	   (ndecl (let ((*generate-tccs* 'none))
		    (setf (type pdecl) surjtype
			  (declared-type pdecl) surjtype)
		    (typecheck* pdecl nil nil nil))))
      (setf (generated-by struct-subtype) decl)
      (setf (current-declaration) cdecl)
      (setf (projection decl) ndecl)
      (add-decl ndecl (not (formal-struct-subtype-decl? decl)))
      struct-subtype)))

;; (defun generate-dep-type-binding (decl tname)
;;   (let ((bd (mk-dep-binding (binding-id decl) tname)))
;;     (pushnew bd (generated decl))
;;     tname))

(defmethod make-self-resolved-type-name ((decl type-decl))
  (let* ((tname (mk-type-name (id decl)))
	 (tres (mk-resolution decl (current-theory-name) tname)))
    (setf (resolutions tname) (list tres))
    tname))

(defmethod generate-struct-subtype ((te recordtype) prtype)
  (make-instance 'struct-sub-recordtype
    :type te
    :print-type prtype
    :fields (fields te)
    :dependent? (dependent? te)))

(defmethod generate-struct-subtype ((te tupletype) prtype)
  (make-instance 'struct-sub-tupletype
    :type te
    :print-type prtype
    :types (types te)
    :dependent? (dependent? te)))

(defmethod generate-struct-subtype ((te struct-sub-recordtype) prtype)
  (setf (print-type te) prtype)
  te)


;;; Tuple Types - in the following, the f_i are optional; any that are
;;; missing will not be used in typechecking the following types.

;;;  C  |-  T1 type,  ... ,  C,f_1:T1,...,f_n-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [ f_1:T1, ... f_n-1:Tn, Tn ] type

(defmethod typecheck* ((type tupletype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((types (typecheck-tuples (types type) nil))
	 (tuptype (if (equal types (types type))
		      type
		      (mk-tupletype types))))
    tuptype))

(defmethod typecheck* ((type struct-sub-tupletype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (type type) nil nil nil)
  (let* ((types (typecheck-tuples (types type) nil))
	 (tuptype (if (equal types (types type))
		      type
		      (mk-struct-sub-tupletype (type type) types))))
    tuptype))

(defun typecheck-tuples (types result)
  (if (null types)
      (nreverse result)
      (let* ((ty (car types))
	     (ntype (typecheck* ty nil nil nil))
	     (*bound-variables* (if (dep-binding? ntype)
				    (cons ntype *bound-variables*)
				    *bound-variables*)))
	(typecheck-tuples (cdr types) (cons ntype result)))))

(defmethod typecheck* ((type cotupletype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((types (typecheck-tuples (types type) nil))
	 (cotuptype (if (equal types (types type))
			type
			(mk-cotupletype types))))
    cotuptype))


;;;  C  |-  T1 type,  ... ,  C,f1:T1,...,fn-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [# f1:T1, ... , fn:Tn #] type

(defmethod typecheck* ((type recordtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (cond ((every #'type (fields type))
	 type)
	(t (when (duplicates? (fields type) :test #'same-id)
	     (type-error type "Duplicate field names not allowed"))
	   (typecheck-fields (fields type))
	   (let* ((dependent? (dependent-fields? (fields type)))
		  (sorted-fields (sort-fields (fields type) dependent?))
		  (ntype (mk-recordtype sorted-fields dependent?)))
	     (setf (dependent? type) dependent?)
	     (tcdebug "~%~6tTypechecking Recordtype:~%~8t~a" type)
	     ntype))))

(defun sort-fields (fields &optional dependent?)
  (if dependent?
      (sort-dependent-fields fields nil)
      (sort (copy-list fields) #'string-lessp :key #'id)))

(defun sort-dependent-fields (fields sorted-fields)
  (if (null fields)
      (nreverse sorted-fields)
      (let ((nfield (next-best-field fields)))
	(sort-dependent-fields (remove nfield fields)
			       (cons nfield sorted-fields)))))

(defun next-best-field (fields)
  (let ((candidates (remove-if #'(lambda (fd)
				   (dependent-on-some? fd fields))
		      fields)))
    (lexical-min-field candidates)))

(defun lexical-min-field (fields &optional min)
  (cond ((null fields)
	 min)
	((null min)
	 (lexical-min-field (cdr fields) (car fields)))
	(t (lexical-min-field (cdr fields)
			      (if (string-lessp (id (car fields)) (id min))
				  (car fields)
				  min)))))
      

(defun dependent-on-some? (field fields)
  (and fields
       (or (and (not (eq field (car fields)))
		(dependent-on? field (car fields)))
	   (dependent-on-some? field (cdr fields)))))

(defun dependent-on? (fld1 fld2)
  (member fld2 (freevars (type fld1)) :test #'same-declaration))

(defmethod dependencies ((fld field-decl) (rtype recordtype))
  "Return the fields in rtype that depend on fld, or that fld depends on
  E.g., if we have R = [# a, b(a), c(b) #], then (dependencies a R) = {b},
  (dependencies b R) = {a, c}, and (dependencies c R) = {b}"
  (assert (memq fld (fields rtype)))
  (remove-if #'(lambda (fd)
		 (or (eq fd fld)
		     (and (not (dependent-on? fd fld))
			  (not (dependent-on? fld fd)))))
    (fields rtype)))

(defmethod dependencies ((db dep-binding) (ttype tupletype))
  (assert (memq db (types ttype)))
  (let ((idx 0)
	(indices nil))
    (dolist (ty (types ttype))
      (incf idx)
      (when (and (not (eq ty db))
		 (or (member db (freevars ty)
			     :test #'same-declaration)
		     (and (dep-binding? ty)
			  (member ty (freevars db)
				  :test #'same-declaration))))
	(push idx indices)))
    (nreverse indices)))

(defmethod dependencies ((type type-expr) (ttype tupletype))
  (assert (memq type (types ttype)))
  (let ((idx 0)
	(indices nil))
    (dolist (ty (types ttype))
      (incf idx)
      (when (and (dep-binding? ty)
		 (member ty (freevars type)
			 :test #'same-declaration))
	(push idx indices)))
    (nreverse indices)))

;; (defmethod dependencies ((constructor constructor-name-expr) constructor-list)
;;   "Return the constructors in constructor-list that depend on constructor, or that constructor depends on"
;;   (assert (memq constructor constructor-list))
;;   (remove-if #'(lambda (constr)
;; 		 (or (eq constr constructor-list)
;; 		     (and (not (dependent-on? constr constructor))
;; 			  (not (dependent-on? constructor constr)))))
;;     constructor-list))

;; (defmethod dependencies ((constructor constructor-name-expr) (rt recursive-type))
;;   "Return the constructors in constructor-list that depend on constructor, or that constructor depends on"
;;   (dependencies constructor (constructors rt)))

;; (defmethod dependencies ((constructor constructor-name-expr) (type adt-type-name))
;;   "Return the constructors in constructor-list that depend on constructor, or that constructor depends on"
;;   (dependencies constructor (constructors type)))

(defmethod dependencies ((accessor accessor-name-expr) (constructor constructor-name-expr))
  (dependencies accessor (accessor-names constructor)))

(defmethod dependencies ((accessor accessor-name-expr) accessor-names)
  "Return the accessors in accessor-names that depend on accessor, or that accessor depends on"
  (assert (member accessor accessor-names :test #'tc-eq))
  (remove-if #'(lambda (acc)
		 (or (tc-eq acc accessor)
		     (and (not (acc-dependent-on? acc accessor))
			  (not (acc-dependent-on? accessor acc)))))
    accessor-names))

(defun acc-dependent-on? (accname1 accname2)
  (occurs-in accname2 (type accname1)))

(defun dependent-fields? (fields)
  (some #'(lambda (fld)
	    (some #'(lambda (fv)
		      (memq (declaration fv) fields))
		  (freevars (type fld))))
	fields))


;;; The tricky part here is that a field has two identities:
;;;  1. Within a dependent record type, it appears as a simple name-expr,
;;;     whose type is the declared type of the field.
;;;  2. Outside the recordtype, references to the field should expect a
;;;     function type whose domain is the recordtype and range is the
;;;     declared-type.

(defun typecheck-fields (fields)
  (when fields
    (typecheck* (car fields) nil nil nil)
    (let* ((*bound-variables* (cons (car fields) *bound-variables*)))
      (typecheck-fields (cdr fields)))))

(defmethod typecheck* ((type struct-sub-recordtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (type type) nil nil nil)
  (cond ((every #'type (fields type))
	 type)
	(t (when (duplicates? (fields type) :test #'same-id)
	     (type-error type "Duplicate field names not allowed"))
	   (typecheck-fields (fields type))
	   (let* ((dependent? (dependent-fields? (fields type)))
		  (sorted-fields (sort-fields (fields type) dependent?))
		  (ntype (mk-struct-sub-recordtype
			  (type type) sorted-fields dependent?)))
	     (setf (dependent? type) dependent?)
	     (tcdebug "~%~6tTypechecking Struct-sub-Recordtype:~%~8t~a" type)
	     ntype))))


;;; Funtype - three rules are actually used here.  As in the tupletype
;;; case, the f_i are optional for the first two rules.

;;;  C  |-  T1 type,  ... ,  C,f1:T1,...,fn-1:Tn-1 |- Tn type
;;;  --------------------------------------------------------
;;;          C  |-  [ f1:T1, ...  -> Tn #] type
;;;          C  |-  [ [f1:T1, ... ] -> Tn #] type
;;;          C  |-  [ [# f1:T1, ... #] -> Tn #] type

(defmethod typecheck* ((type funtype) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let* ((dom (typecheck* (domain type) nil nil nil))
	 (*bound-variables* (if (typep (domain type) 'dep-binding)
				(cons (domain type) *bound-variables*)
				*bound-variables*))
	 (rng (typecheck* (range type) nil nil nil)))
    (if (and (eq dom (domain type))
	     (eq rng (range type)))
	type
	(let ((tval (mk-funtype dom rng (class-of type))))
	  tval))))

(defun get-funtype-dependencies (funtype types)
  (if (cdr types)
      (remove-if-not #'dep-binding? types)
      (typecase (domain funtype)
	(recordtype (fields (car types)))
	(tupletype (mapcar #'(lambda (db)
			       (mk-bind-decl (id db) (declared-type db)))
			   (remove-if-not #'dep-binding? (types (car types)))))
	(t (remove-if-not #'dep-binding? types)))))

(defmethod typecheck* ((decl field-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((type (let ((*generate-tccs* 'none))
		(typecheck* (declared-type decl) nil nil nil))))
    (set-type (declared-type decl) nil)
    #+badassert
    (assert (or (null (print-type type))
		(tc-eq (print-type type) (declared-type decl))))
    (setf (type decl) type))
  decl)


(defmethod typecheck* ((db dep-binding) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (type db)
    (let ((*generate-tccs* 'none))
      (setf (type db) (typecheck* (declared-type db) nil nil nil)))
    (set-type (declared-type db) nil)
    (assert (or (null (print-type (type db)))
		(tc-eq (print-type (type db)) (declared-type db)))))
  db)

(defmethod typecheck* ((te type-extension) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((lhstype (typecheck* (type te) nil nil nil))
	(rhstype (typecheck* (extension te) nil nil nil)))
    (set-type (type te) nil)
    (set-type (extension te) nil)
    (let ((slhstype (find-supertype lhstype))
	  (srhstype (find-supertype rhstype)))
      (typecase slhstype
	((or recordtype struct-sub-recordtype)
	 (unless (typep srhstype '(or recordtype struct-sub-recordtype))
	   (type-error te
	     "Type ~a is a recordtype, ~a is not" (type te) (extension te)))
	 (unless (and (eq slhstype lhstype)
		      (eq srhstype rhstype))
	   (pvs-warning "The extended type ~a will not be a subtype" te))
	 (let ((extended-type (recordtype-union slhstype srhstype te)))
	   (setf (print-type extended-type)
		 (change-class (copy te) 'print-type-extension))
	   extended-type))
	((or tupletype struct-sub-tupletype)
	 (unless (typep srhstype '(or tupletype struct-sub-tupletype))
	   (type-error te
	     "Type ~a is a tupletype, ~a is not" (type te) (extension te)))
	 (let* ((extended-type
		 (typecheck* (copy slhstype
			       :types (append (types slhstype)
					      (types srhstype)))
			     nil nil nil)))
	   (setf (print-type extended-type) te)
	   extended-type))
	(t (type-error (type te)
	     "Type extensions may only be applied to record and tuple types"
	     ))))))

(defun recordtype-union (rt1 rt2 te)
  (let ((nfields (recordtype-union* (fields rt1) (fields rt2) te)))
    (typecheck* (if (or (struct-sub-recordtype? rt1)
			(struct-sub-recordtype? rt2))
		    (make-instance 'struct-sub-recordtype
		      :type (or (type rt1) (type rt2))
		      :generated-by te
		      :fields nfields)
		    (make-instance 'recordtype
		      :fields nfields))
		nil nil nil)))

(defun recordtype-union* (flds1 flds2 te &optional bindings nfields)
  (if (null flds1)
      (cond ((null flds2)
	     (pvs-warning "~-100I~_~<The record extension ~_~w ~_adds no new fields~:>"
	       (list te))
	     (sort-fields nfields))
	    (t (sort-fields
		(append (nreverse nfields)
		       (substit flds2
			 (mapcar #'(lambda (x) (cons (cdr x) (car x)))
			   bindings))))))
      (let* ((fld1 (car flds1))
	     (fld2 (find (id fld1) flds2 :key #'id)))
	(when (and fld2
		   (not (tc-eq-with-bindings (type fld1) (type fld2)
					     bindings)))
	  (type-error fld1
	    "~-100I~_~<Field ~a appears with different types: ~_~a~:_ and ~:_~a~:>"
	    (list (id fld1) (type fld1) (type fld2))))
	(recordtype-union* (cdr flds1)
			   (if fld2 (remove fld2 flds2) flds2)
			   te
			   (if fld2 (acons fld1 fld2 bindings) bindings)
			   (cons fld1 nfields)))))
  

;; (defmethod typecheck* ((te quant-type) expected kind args)
;;   (declare (ignore expected kind arguments))
;;   (break "Not yet finished"))

;;; Judgements

(defmethod typecheck* ((decl subtype-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (setf (subtype decl) (typecheck* (declared-subtype decl) nil nil nil))
  ;; Note that the declared-subtype is now a print-type
  (set-type (subtype decl) nil)
  ;; (when (print-type (subtype decl))
  ;;   (setf (print-type (subtype decl))
  ;; 	  (copy-judgement-subtype-without-types (declared-subtype decl))))
  (let ((*bound-variables*
	 (if (typep (declared-subtype decl) 'type-application)
	     (append (mapcan #'(lambda (p)
				 (when (variable? p)
				   (list (declaration p))))
		       (parameters (declared-subtype decl)))
		     *bound-variables*)
	     *bound-variables*))
	(*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
    (set-type (type decl) nil)
    (assert (or (null (print-type (type decl)))
		(tc-eq (print-type (type decl)) (declared-type decl)))))
  (let ((*checking-conversions* t)) ;; Disable optimization using subtype-conjuncts
    (if (subtype-of? (subtype decl) (type decl))
	(pvs-warning
	    "In judgement ~:[at~;~:*~a,~] Line ~d:~_ ~a~_ is already known to be a subtype of~%  ~a"
	  (id decl) (line-begin (place decl))
	  (declared-subtype decl) (declared-type decl))
	(let* ((bd (make-new-bind-decl (subtype decl)))
	       (bvar (make-variable-expr bd)))
	  (setf (place bvar) (place (declared-subtype decl)))
	  (unless (compatible? (subtype decl) (type decl))
	    (type-error decl
	      "~@[In judgement ~a: ~]subtype ~a is incompatible with ~a"
	      (id decl) (declared-subtype decl) (declared-type decl)))
	  (let* ((*compatible-pred-reason*
		  (acons bvar "judgement" *compatible-pred-reason*))
		 (incs (compatible-preds (subtype decl) (type decl) bvar)))
	    (cond (incs
		   (unless (eq *generate-tccs* 'none)
		     (generate-subtype-tcc bvar (type decl) incs))
		   (add-judgement-decl decl))
		  (t (pvs-warning
			 "Subtype judgement is superfluous~@[ (on line ~d)~]:~
                        ~%  ~a"
		       (line-begin (place decl)) (unparse-decl decl)))))))))

(defmethod copy-judgement-subtype-without-types ((te type-application))
  (lcopy te
    :parameters (mapcar #'(lambda (p)
			    (if (and (bind-decl? p)
				     (not (untyped-bind-decl? p)))
				(change-class (copy p) 'untyped-bind-decl)
				p))
		  (parameters te))))

(defmethod copy-judgement-subtype-without-types (te)
  te)
  

(defmethod typecheck* ((decl number-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (set-type (declared-type decl) nil)
  (assert (or (null (print-type (type decl)))
	      (tc-eq (print-type (type decl)) (declared-type decl))))
  (let ((*compatible-pred-reason*
	 (acons (number-expr decl) "judgement" *compatible-pred-reason*)))
    (typecheck* (number-expr decl) (type decl) 'expr nil))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod typecheck* ((decl name-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
  (when (funtype? (find-supertype (type decl)))
    (pvs-warning "Judgement at line ~d may lead to unprovable TCCs~
                  ~%See language document for details."
      (starting-row (place decl))))
  (set-type (declared-type decl) nil)
  (assert (or (null (print-type (type decl)))
	      (tc-eq (print-type (type decl)) (declared-type decl))))
  (let ((*no-conversions-allowed* t)
	(*compatible-pred-reason*
	 (acons (name decl) "judgement" *compatible-pred-reason*)))
    (typecheck* (name decl) (type decl) nil nil))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod typecheck* ((decl application-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (formals decl) nil nil nil)
  (let ((fmlist (apply #'append (formals decl))))
    (let ((dup (duplicates? fmlist :key #'id)))
      (when dup
	(type-error dup
	  "May not use duplicate arguments in judgements")))
    ;;(set-formals-types fmlist)
    )
  (let* ((*bound-variables* (reverse (apply #'append (formals decl)))))
    (let ((*generate-tccs* 'none))
      (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
    (set-type (type decl) nil)
    (assert (or (null (print-type (type decl)))
		(tc-eq (print-type (type decl)) (declared-type decl))))
    (let* ((*no-conversions-allowed* t)
	   (expr (mk-application-for-formals (name decl) (formals decl)))
	   (*compatible-pred-reason*
	    (acons expr "judgement" *compatible-pred-reason*)))
      (unless (place expr)
	(setf (place expr) (place (name decl))))
      ;; We typecheck without tccs first, to find out whether this is
      ;; a recursive judgement
      (let ((*generate-tccs* 'none))
	(typecheck* expr (type decl) nil nil))
      (when (def-decl? (declaration (name decl)))
	(pvs-warning
	    "The generated TCCs for judgement ~a~%~
             might be easier to prove using a RECURSIVE judgement.~%~
             (for backward compatibility, this is not generated by default)."
	  (ref-to-id decl)))
      (set-type expr (type decl))))
  (setf (judgement-type decl)
	(make-formals-funtype (formals decl) (type decl)))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl))

(defmethod typecheck* ((decl expr-judgement) expected kind arguments)
  (declare (ignore expected kind arguments))
  ;; We typecheck copies of the declared-type and expr - enough to tell whether we
  ;; need to change decl to an application-judgement.
  (let* ((ctype (unless (forall-expr? (expr decl))
		  (let ((*generate-tccs* 'none)
			(*no-conversions-allowed* t)
			(dtype (copy-all (declared-type decl))))
		    (copy-lex (declared-type decl) dtype)
		    (typecheck* dtype nil nil nil))))
	 (cexpr (when ctype
		  (let ((*generate-tccs* 'none)
			(*no-conversions-allowed* t)
			(cex (copy-all (expr decl))))
		    (copy-lex (expr decl) cex)
		    (typecheck* cex ctype nil nil))))
	 (mexpr (and cexpr (or (from-macro cexpr) cexpr))))
    ;; expr-judgement basically just has an expr and declared-type may want
    ;; to treat as appl-judgement, with name and formals, in the special
    ;; case of application form, e.g., f(x, y)(z), where all the arguments
    ;; are distinct variables.
    (if (and (typep mexpr '(and application (not infix-application)))
	     (let ((args-lists (arguments* mexpr)))
	       (and (every #'(lambda (args) (every #'variable? args))
			   args-lists)
		    (not (duplicates? (apply #'append args-lists)
				      :test #'same-declaration)))))
	(change-expr-judgement-to-application-judgement decl)
	(typecheck-expr-judgement decl))))

(defun typecheck-expr-judgement (decl)
  "Typechecks the expr-judgement decl, determined not to be an application-judgement.
Note that if it is a forall-expr, it is treated specially; e.g.,
  FORALL (x: real | x > 1): x * x HAS_TYPE {y : real | y > x}
in a way, a HAS_TYPE b is boolean, but it's not a valid expr."
  (let ((*generate-tccs* 'none))
    (cond ((forall-expr? (expr decl))
	   ;; Note that it is not really a forall expr, as it is not boolean
	   (typecheck* (bindings (expr decl)) nil nil nil)
	   (let ((*bound-variables* (append (bindings (expr decl)) *bound-variables*)))
	     (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
	     (typecheck* (expression (expr decl)) (type decl) nil nil)))
	  (t (setf (type decl) (typecheck* (declared-type decl) nil nil nil))
	     (typecheck* (expr decl) (type decl) nil nil))))
  ;; Not an application-judgement, but has freevars
  ;; Get the freevars list, and create untyped-bind-decls
  ;; Append to the beginning of bindings if expr is a forall-expr
  ;; Set (formals decl) to this list
  ;; Then retypecheck expr under the new bindings
  (let* ((*no-expected* t)
	 ;; uform is not a valid forall expr, but this gets the
	 ;; expr and type under the same bindings
	 (lform (if (forall-expr? (expr decl))
		    (copy (expr decl)
		      :expression (list (expression (expr decl)) (type decl)))
		    (list (expr decl) (type decl))))
	 (uform (universal-closure lform))
	 (*no-conversions-allowed* t)
	 (*compatible-pred-reason*
	  (acons (if (forall-expr? uform)
		     (car (expression uform))
		     (car uform))
		 "judgement" *compatible-pred-reason*))
	 (*bound-variables* (when (forall-expr? uform) (bindings uform))))
    (if (forall-expr? uform)
	(let* ((*bound-variables* (bindings uform)))
	  (assert (listp (expression uform)))
	  (set-type (car (expression uform)) (cadr (expression uform))))
	(set-type (car uform) (cadr uform)))
    (setf (closed-form decl) uform)
    (cond ((and (expr-judgement? decl)
		(expr-judgement-useless? (closed-form decl)))
	   (useless-judgement-warning decl))
	  (t (when (formals-sans-usings (current-theory))
	       (generic-judgement-warning decl))
	     (add-judgement-decl decl)))))

(defun expr-judgement-useless? (form)
  (if (forall-expr? form)
      (let ((eform (expression form)))
	(subtype-of? (type (car eform)) (cadr eform)))
      (subtype-of? (type (car form)) (cadr form))))

(defun useless-judgement-warning (decl)
  (let ((form (if (forall-expr? (closed-form decl))
		  (expression (closed-form decl))
		  (closed-form decl))))
    (pvs-warning
	"Expression judgement ~@[~a ~]at line ~d the expr type:~%  ~a~
       ~% is already known to be a subtype of~%  ~a~%~
       This judgement will not be used"
      (id decl) (starting-row (place decl)) (type (car form)) (cadr form))))

;;; The expr-judgement expression is of the form, e.g., f(x, y)(z), where x,
;;; y, and z are determined to be (distinct) variables.  In this case, we
;;; change it to an application-judgement
(defun change-expr-judgement-to-application-judgement (decl)
  (assert (application? (expr decl)))
  (assert (name-expr? (operator* (expr decl))))
  (let* ((name (operator* (expr decl)))
	 (formals (mapcar #'(lambda (args)
			      (mapcar #'(lambda (arg)
					  (if (bind-decl? arg)
					      arg
					      (make-instance 'untyped-bind-decl
						:id (id arg)
						:place (place arg))))
				args))
		    (arguments* (expr decl))))
	 (dtype (declared-type decl)))
    (change-class decl 'application-judgement
      :name name
      :formals formals
      :declared-type dtype
      :type nil)
    (typecheck* decl nil nil nil)))

(defmethod typecheck* ((decl rec-application-judgement) expected kind args)
  (declare (ignore expected kind args))
  (typecheck-rec-application-judgement decl))

;; Split this from the above, as somehow the judgement-type slot is not set
;; in CMULisp otherwise.  I can't say I understand this - Owre
(defun typecheck-rec-application-judgement (decl)
  (typecheck* (formals decl) nil nil nil)
  (let ((fmlist (apply #'append (formals decl))))
    (let ((dup (duplicates? fmlist :key #'id)))
      (when dup
	(type-error dup
	  "May not use duplicate arguments in judgements")))
    ;;(set-formals-types fmlist)
    )
  (let* ((*bound-variables* (reverse (apply #'append (formals decl)))))
    (let ((*generate-tccs* 'none))
      (setf (type decl) (typecheck* (declared-type decl) nil nil nil)))
    (set-type (declared-type decl) nil)
    (assert (or (null (print-type (type decl)))
		(tc-eq (print-type (type decl)) (declared-type decl))))
    (let* ((*no-conversions-allowed* t)
	   (expr (mk-application-for-formals (name decl) (formals decl))))
      (unless (place expr)
	(setf (place expr) (place (name decl))))
      (let ((*generate-tccs* 'none))
	(typecheck* expr (type decl) nil nil))
      (unless (and (name-expr? (operator* expr))
		   (def-decl? (declaration (operator* expr))))
	(type-error expr "~a does not resolve to a recursive function"
		    (operator* expr)))
      (let ((*collecting-tccs* t)
	    (*tccforms* nil)
	    (*compatible-pred-reason*
	     (acons expr "judgement" *compatible-pred-reason*)))
	(set-type expr (type decl))
	(let ((tcc (find-if #'(lambda (tcc)
				(equalp (tccinfo-reason tcc) "judgement"))
		     *tccforms*)))
	  ;;(assert tcc)
	  (when tcc
	    (setf (rewrite-formula decl) (tccinfo-formula tcc)))))
      (typecheck-rec-judgement decl expr)))
  (setf (judgement-type decl)
	(make-formals-funtype (formals decl) (type decl)))
  (assert (judgement-type decl))
  (when (formals-sans-usings (current-theory))
    (generic-judgement-warning decl))
  (add-judgement-decl decl)
  (when (id decl)
    ;; Generate the corresponding formula - this is generally not one of the
    ;; TCCs, as they are obtained by walking down the recursive declaration
    ;; body.  Instead, we use the rewrite-formula obtained above
    (let ((jtcc (mk-recursive-judgement-axiom (id decl) (rewrite-formula decl))))
      (setf (newline-comment jtcc)
	    (list (format nil
		      "% Recursive judgement axiom generated for judgement~%% ~a"
		    (id decl))))
      (add-decl jtcc))))
    
(defun typecheck-rec-judgement (decl expr)
  (change-class decl 'rec-application-judgement)
  (let* ((op (operator* expr))
	 (recdecl (declaration op))
	 (def (rec-judgement-definition decl recdecl))
	 (arg-bds (typecheck* (copy-untyped (formals decl)) nil nil nil))
	 (*bound-variables* (apply #'append arg-bds))
	 (rtype (let ((*generate-tccs* 'none))
		  (typecheck* (declared-type decl) nil nil nil)))
	 (jtype (make-formals-funtype (formals decl) rtype))
	 (vid (make-new-variable '|v| (list expr def)))
	 (vbd (make-bind-decl vid jtype))
	 (vname (mk-name-expr vbd))
	 (vterm (make!-applications vname
				    (mapcar #'(lambda (x)
						(mapcar #'mk-name-expr x))
				      arg-bds)))
	 (vbody (make!-lambda-exprs
		 arg-bds
		 (make!-applications op
				     (mapcar #'(lambda (x)
						 (mapcar #'mk-name-expr x))
				       arg-bds))))
	 (vdef (make!-equation vname vbody))
	 (jdecl-arg-alist (pairlis-rec-formals (formals decl) arg-bds))
	 (rdecl-arg-alist (pairlis-rec-formals (formals recdecl) arg-bds))
	 (subst-type (substit (type decl) jdecl-arg-alist))
	 (precond (make!-forall-expr (mapcan #'copy-list arg-bds)
		    (make!-conjunction*
		     (compatible-predicates (judgement-types+ vterm)
					    subst-type vterm))))
	 (*bound-variables* (cons vbd *bound-variables*))
	 (rec-alist (acons recdecl vname rdecl-arg-alist))
	 ;; Can't use substit here, as the def is also being substituted
	 (sdef (gensubst def
		 #'(lambda (x)
		     (mk-name-expr (cdr (assq (declaration x) rec-alist))))
		 #'(lambda (x) (and (name-expr? x)
				    (assq (declaration x) rec-alist)))))
	 (jsig (rec-judgement-signature decl (type op)))
	 (nrange (typecheck* (copy-untyped (rec-judgement-range jsig decl))
			     nil nil nil))
	 (*tcc-conditions* (cons vdef (cons vbd (add-formals-to-tcc-conditions arg-bds))))
	 ;; Don't add directly to *tcc-conditions* as they may not be
	 ;; needed, and make subsumption impossible (and really ugly TCCs)
	 ;; See add-tcc-conditions in tcc-gen.lisp
	 (*rec-judgement-extra-conditions* (list precond vbd))
	 (*compatible-pred-reason*
	  (acons (name decl) "recursive-judgement"
		 *compatible-pred-reason*)))
    (assert (null (freevars (reverse *tcc-conditions*))))
    (typecheck* (pc-parse (unparse sdef :string t) 'expr) nrange nil nil)))

(defun rec-judgement-range (jsig jdecl)
  (rec-judgement-range* jsig (formals jdecl)))

(defun rec-judgement-range* (jsig formals)
  (if (null formals)
      jsig
      (rec-judgement-range*
       (if (dep-binding? (domain jsig))
	   (substit (range jsig) (acons (domain jsig)
					(if (singleton? (car formals))
					    (caar formals)
					    (make-tuple-expr
					     (mapcar #'make-variable-expr
					       (car formals))))
					nil))
	   (range jsig))
       (cdr formals))))

(defun rec-judgement-definition (jdecl rdecl)
  (rec-judgement-definition*
   (formals jdecl)
   (formals rdecl)
   (definition rdecl)))

(defun rec-judgement-definition* (jformals rformals def)
  (if (null jformals)
      (if (null rformals)
	  def
	  (make!-lambda-exprs rformals def))
      (if (null rformals)
	  (rec-judgement-definition*
	   (cdr jformals)
	   nil
	   (let ((names (mapcar #'make-variable-expr (car jformals))))
	     (make!-application* def names)))
	  (rec-judgement-definition*
	   (cdr jformals)
	   (cdr rformals)
	   def))))

(defun pairlis-rec-formals (oldformals newformals &optional alist)
  (if (or (null oldformals)
	  (null newformals))
      (nreverse alist)
      (pairlis-rec-formals
       (cdr oldformals) (cdr newformals)
       (pairlis-rec-formals* (car oldformals) (car newformals) alist))))

(defun pairlis-rec-formals* (oldformals newformals alist)
  (if (singleton? oldformals)
      (if (singleton? newformals)
	  (pairlis oldformals newformals alist)
	  (acons (car oldformals)
		 (make!-tuple-expr* (mapcar #'mk-name-expr newformals))
		 alist))
      (if (singleton? newformals)
	  (pairlis oldformals
		   (make!-projections (mk-name-expr (car newformals)))
		   alist)
	  (pairlis oldformals newformals alist))))


;;; Given a recursive judgement declaration jdecl and the associated
;;; recursive function declaration recdecl, this function computes the
;;; judgement signature.  The original recursive function is provided to
;;; take the intersection types of the domain and range types, thus yielding
;;; more accurate TCCs.  The tricky parts here are currying and dependent
;;; types, in that the curry depth may not match, and either the original
;;; recursive function type or the judgement type (or both) may be dependent
;;; types.

(defun rec-judgement-signature (jdecl optype)
  (rec-judgement-signature* (make-formals-funtype (formals jdecl) (type jdecl))
			    optype
			    jdecl))

(defmethod rec-judgement-signature* ((jtype funtype) (rtype funtype)
				     jdecl &optional domtypes)
  (let* ((jdom (domain jtype))
	 (rdom (domain rtype))
	 (idom (intersection-type jdom rdom))
	 ;; idom is never a dep-binding, ndom is one if either jdom or rdom is
	 (ndom (if (dep-binding? jdom)
		   (if (tc-eq idom (type jdom))
		       jdom
		       (mk-dep-binding (id jdom) idom))
		   (if (dep-binding? rdom)
		       (mk-dep-binding (id rdom) idom)
		       idom))))
    (rec-judgement-signature*
     (if (or (eq ndom jdom)
	     (not (dep-binding? jdom)))
	 (range jtype)
	 (substit (range jtype) (acons jdom ndom nil)))
     (if (and (dep-binding? ndom)
	      (dep-binding? rdom))
	 (substit (range rtype) (acons rdom ndom nil))
	 (range rtype))
     jdecl
     (cons ndom domtypes))))

(defmethod rec-judgement-signature* ((jtype funtype) rtype jdecl
				     &optional domtypes)
  (declare (ignore rtype domtypes))
  (type-error jdecl "Recursive judgement has too many arguments"))

(defmethod rec-judgement-signature* (jtype rtype jdecl &optional domtypes)
  (declare (ignore rtype jdecl))
  (let ((nrng ;;(intersection-type jtype rtype)
	 jtype))
    (mk-funtype* domtypes nrng)))

(defmethod generic-judgement-warning ((decl number-judgement))
  (when (free-params (type decl))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the number."
      (id decl) (starting-row (place decl)) (id (module decl)))))
  

(defmethod generic-judgement-warning ((decl name-judgement))
  (unless (subsetp (free-params (type decl))
		   (free-params (name decl)))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the name ~a."
      (id decl) (starting-row (place decl)) (id (module decl)) (name decl))))

(defmethod generic-judgement-warning ((decl application-judgement))
  (unless (subsetp (free-params (type decl))
		   (union (free-params (name decl))
			  (free-params (formals decl))))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the name ~a."
      (id decl) (starting-row (place decl)) (id (module decl)) (name decl))))

(defmethod generic-judgement-warning ((decl expr-judgement))
  (unless (subsetp (free-params (type decl))
		   (union (free-params (expr decl))
			  (free-params (formals decl))))
    (pvs-warning
	"Judgement ~@[~a ~]at line ~d will not be used if theory ~a~%  ~
         is only imported generically, as the theory instance cannot be~%  ~
         determined from the expr ~a."
      (id decl) (starting-row (place decl)) (id (module decl)) (expr decl))))


(defun mk-application-for-formals (expr formals)
  (if (null formals)
      expr
      (let ((names (mapcar #'mk-name-expr (car formals))))
	(mapc #'(lambda (nm fm)
		  (setf (place nm) (place fm)))
	      names (car formals))
	(mk-application-for-formals
	 (mk-application* expr names)
	 (cdr formals)))))

(defun make!-application-for-formals (expr formals)
  (if (null formals)
      expr
      (let ((names (mapcar #'mk-name-expr (car formals))))
	(mapc #'(lambda (nm fm)
		  (setf (place nm) (place fm)))
	      names (car formals))
	(make!-application-for-formals
	 (make!-application* expr names)
	 (cdr formals)))))

;;; Conversions

(defmethod typecheck* ((decl conversion-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (expr decl) nil nil nil)
    (resolve-conversion-expr (expr decl)))
  ;;(set-type (expr decl) (type (expr decl)))
  (check-conversion-applicability decl)
  (setf (k-combinator? decl) (k-combinator? (expr decl)))
  (push decl (conversions *current-context*))
  decl)

(defmethod resolve-conversion-expr ((ex name-expr))
  ;; ex has been typechecked, but may have ambiguities
  (let* ((reses (resolutions ex))
	 (valid-reses (remove-if #'has-type-vars? reses :key #'type)))
    (unless valid-reses
      (type-error ex
	"Cannot determine the type associated with ~a:~%  Please provide more ~
           information, i.e., actual parameters or a coercion." ex))
    (if (singleton? valid-reses)
	(setf (resolutions ex) valid-reses
	      (type ex) (type (car valid-reses)))
	(let* ((same-id-convs (remove-if-not #'(lambda (conv)
						 (and (name-expr? (expr conv))
						      (eq (id (expr conv)) (id ex))))
				(current-conversions)))
	       (new-conv-reses (remove-if #'(lambda (res)
						  (member res same-id-convs
							  :key #'(lambda (conv)
								   (resolution (expr conv)))
							  :test #'tc-eq))
				     valid-reses))
	       (finst-reses (if (cdr new-conv-reses)
				(or (remove-if-not #'fully-instantiated? new-conv-reses)
				    new-conv-reses)
				new-conv-reses)))
	  (when (null finst-reses)
	    (pvs-warning "Conversion ~a ~a is already aavailable"
	      ex (format nil "(at line ~d, column ~d) "
		   (starting-row (place ex)) (starting-col (place ex)))))
	  (cond ((cdr finst-reses)
		 (type-ambiguity ex))
		(t (setf (resolutions ex) finst-reses
			 (type ex) (type (car finst-reses)))))))))

(defmethod resolve-conversion-expr ((ex expr))
  ;; ex has been typechecked, but may have ambiguities
  (when (and (expr? ex)
	     (not (type ex)))
    (let ((valid-types (remove-if #'has-type-vars? (ptypes ex))))
      (unless valid-types
	(type-error ex
	  "Cannot determine the type associated with ~a:~%  Please provide more ~
           information, i.e., actual parameters or a coercion." ex))
      (if (singleton? valid-types)
	  (if (and (fully-instantiated? (car valid-types))
		   (fully-typed? (car valid-types)))
	      (set-type ex (car valid-types))
	      (progn ;; (break "Check resolve-conversion-expr")
		     (setf (type ex) (car valid-types))))
	  (let ((gtype (find-if-not #'fully-instantiated? valid-types)))
	    (when gtype
	      (setf (type ex) gtype)))))))

(defun check-conversion-applicability (decl)
  (let* ((ctype (type (expr decl)))
	 (stype (find-supertype ctype)))
    (unless (typep stype 'funtype)
      (type-error (expr decl)
	"Conversion is not a function:~%  ~a: ~a" (expr decl) ctype))
    (when (has-type-vars? ctype)
      (type-error (expr decl)
	"Cannot determine the conversion type for ~a:~%  Please provide more ~
         information, i.e., actual parameters or a coercion." (expr decl)))
    (when (and (fully-instantiated? stype)
	       (strict-compatible? (domain stype) (range stype)))
      (type-error (expr decl)
	"The domain and range of this conversion are compatible;~%~
         the conversion will never be used:~%  ~a: ~a" (expr decl) ctype))
    ;; Note that conversions are similar to auto-rewrites, where the
    ;; free-params of a conversion correspond to the free variables of an
    ;; auto-rewrite.  This is a test that the LHS covers all the free
    ;; variables.
    (unless (fully-instantiated? (expr decl))
      (let* ((frees (remove-if #'(lambda (fp)
				   (memq fp (formals-sans-usings
					     (current-theory))))
		      (free-params (expr decl))))
	     (ftheory (module (car frees)))
	     (fdecl (declaration (expr decl))))
	(unless (every #'(lambda (fp) (eq (module fp) ftheory))
		       (cdr frees))
	  (type-error (expr decl)
	    "Conversion expression has free parameters from different theories"
	    ))
	(unless (= (length frees)
		   (+ (length (formals-sans-usings ftheory))
		      (length (decl-formals fdecl))))
	  (type-error (expr decl)
	    "conversion does not determine all free parameters"))))))

(defmethod type ((decl conversion-decl))
  (type (expr decl)))

;;; Disabling conversions

(defmethod typecheck* ((decl conversionminus-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (expr decl) nil nil nil)
    (resolve-conversion-expr (expr decl)))
  (disable-conversion decl)
  decl)

(defun disable-conversion (decl)
  (multiple-value-bind (dconvs new-convs)
      (split-on #'(lambda (cd) (tc-eq (expr decl) (expr cd)))
		(conversions *current-context*))
    (cond ((equal new-convs (conversions *current-context*))
	   ;; Three possibilities in this case
	   (let ((similar-convs
		  (remove-if-not #'(lambda (cd)
				     (same-declaration (expr decl)
						       (expr cd)))
		    (conversions *current-context*))))
	     ;; Don't need to do anything if the conversion doesn't
	     ;; appear.  Won't even complain since it may have been
	     ;; removed earlier by a similar declaration in a
	     ;; different theory.
	     (when similar-convs
	       ;; If we are removing the generic version, and
	       ;; instances are in the conversions list, simply
	       ;; remove them.
	       (if (not (fully-instantiated? (expr decl)))
		   (setf (conversions *current-context*)
			 (remove-if #'(lambda (cd)
					(memq cd similar-convs))
			   (conversions *current-context*)))
		   ;; Otherwise, add it to the disabled-conversions list
		   (pushnew decl (disabled-conversions *current-context*)
			    :test #'tc-eq :key #'expr)))))
	  (t (setf (disabled-conversions *current-context*)
		   (cons decl
			 (append dconvs
				 (disabled-conversions *current-context*))))
	     (if (fully-instantiated? (expr decl))
		 (setf (conversions *current-context*) new-convs)
		 (setf (conversions *current-context*)
		       (if (name-expr? (expr decl))
			   (remove-if #'(lambda (cd)
					  (and (name-expr? (expr cd))
					       (same-declaration (expr decl)
								 (expr cd))))
			     (conversions *current-context*))
			   (break "TODO: Finish disabling conversions for non-names")
			   )))))))
    

;;; auto-rewrite-decls

(defmethod typecheck* ((decl auto-rewrite-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (typecheck* (rewrite-names decl) nil nil nil)
  (add-auto-rewrite-to-context decl)
  decl)

(defun add-auto-rewrite-to-context (decl)
  (assert (auto-rewrite-decl? decl))
  (pushnew decl (auto-rewrites *current-context*))
  (let ((new-disabled
	 (mapcar #'(lambda (disabled)
		     (remove-enabled-rewrites disabled (rewrite-names decl)))
	   (disabled-auto-rewrites *current-context*))))
    (unless (equal new-disabled (disabled-auto-rewrites *current-context*))
      (setf (disabled-auto-rewrites *current-context*) new-disabled))))

(defun remove-enabled-rewrites (disabled names)
  (let ((diff (remove-if #'(lambda (rname)
			     (member rname names :test #'tc-eq))
		  (rewrite-names disabled))))
    (if (equal diff (rewrite-names disabled))
	disabled
	(let ((edecl (find-if #'(lambda (d)
				  (and (auto-rewrite-minus-decl? d)
				       (eq (module d) (current-theory))
				       (tc-eq (rewrite-names d) diff)))
		       (get-declarations (id disabled)))))
	  (or edecl
	      (let ((ndisabled (copy disabled
				 :rewrite-names diff
				 :module (current-theory)
				 :generated-by disabled)))
		(assert (not (member ndisabled
				     (remove-if-not #'(lambda (d)
							(eq (module d) (current-theory)))
				       (get-declarations (id ndisabled)))
				     :test #'add-decl-test)))
		(add-decl ndisabled)
		(assert (memq ndisabled (all-decls (module ndisabled))))
		ndisabled))))))


(defmethod typecheck* ((decl auto-rewrite-minus-decl) expected kind arguments)
  (declare (ignore expected kind arguments))
  (unless (typechecked? decl)
    (typecheck* (rewrite-names decl) nil nil nil))
  (pushnew decl (disabled-auto-rewrites *current-context*))
  decl)

(defmethod typecheck* ((rname rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((reses (formula-or-definition-resolutions rname)))
    (unless reses
      (let ((*resolve-error-info* nil))
	(resolution-error rname 'rewrite-name nil (not *in-checker*))))
    (setf (resolutions rname) reses))
  rname)

(defmethod typecheck* ((rname constant-rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((*generate-tccs* 'none))
    (setf (type rname) (typecheck* (declared-type rname) nil nil nil)))
  (set-type (declared-type rname) nil)
  (let ((reses (definition-resolutions rname)))
    (unless reses
      (resolution-error rname 'rewrite-name nil (not *in-checker*)))
    (setf (resolutions rname) reses))
  rname)

(defmethod typecheck* ((rname formula-rewrite-name) expected kind arguments)
  (declare (ignore expected kind arguments))
  (let ((reses (formula-resolutions rname)))
    (unless reses
      (resolution-error rname 'rewrite-name nil (not *in-checker*)))
    (setf (resolutions rname) reses))
  rname)

(defun check-duplication (decl)
  (when (projection? (id decl))
    (type-error decl "May not overload projection names"))
  (let* ((decls (get-declarations (id decl))))
    (dolist (d decls)
      (when (and (not (eq d decl))
		 (declaration? d)
		 (eq (id d) (id decl))
		 (eq (module d) (current-theory)))
	(duplicate-decls decl d)))))

(defun duplicate-decls (x y)
  (when (eq (kind-of x) (kind-of y))
    (if (eq (kind-of x) 'expr)
	(let ((tyx (type x))
	      (tyy (type y)))
	  (cond ((strict-compatible? tyx tyy)
		 (type-error x
		   "~a has previously been declared~a with type ~a, which is ambiguous"
		   (id x)
		   (if (place y)
		       (format nil " (at line ~d, column ~d)"
			 (starting-row (place y)) (starting-col (place y)))
		       "")
		   (unparse tyx :string t)))
		((tc-unifiable? x y)
		 (type-error x
		   "~a has previously been declared~a with type ~a, which may be ambiguous"
		   (id x)
		   (if (place y)
		       (format nil " (at line ~d, column ~d)"
			 (starting-row (place y)) (starting-col (place y)))
		       "")
		   (unparse (type y) :string t)))))
	(unless (or (and (typep y 'type-def-decl)
			 (typep (type-expr y) 'recursive-type))
		    (and (typep y 'type-decl)
			 (not (types-with-same-signatures x y)))
		    (and (typep y 'decl-formal-type)
			 (typep x 'decl-formal-type)
			 (or (null (associated-decl x))
			     (null (associated-decl y))
			     (not (eq (associated-decl x) (associated-decl y))))))
	  (type-error x
	    "Name ~a already in use~a as a ~a"
	    (id x)
	    (if (place y)
		(format nil " (at line ~d, column ~d)"
		  (starting-row (place y)) (starting-col (place y)))
		"")
	    (kind-of x))))))

(defmethod types-with-same-signatures ((x type-decl) (y type-decl))
  (if (formals x)
      (and (formals y)
	   (= (length (car (formals x))) (length (car (formals y))))
	   (every #'(lambda (a b) (compatible? (type a) (type b)))
		  (car (formals x)) (car (formals y))))
      (null (formals y))))

(defun tc-unifiable? (x y)
  (declare (ignore x y))
  nil)
	

(defun set-prelude-types (id type)
  (case id
    (|boolean| (setq *boolean* type))
    (|number| (setq *number* type))
    (|number_field| (setq *number_field* type))
    (|real| (setq *real* type)
     (push-ignored-type-constraints *real*))
    (|rational| (setq *rational* type)
     (push-ignored-type-constraints *rational*))
    (|integer| (setq *integer* type)
     (push-ignored-type-constraints *integer*))
    (|naturalnumber| (setq *naturalnumber* type)
     (push-ignored-type-constraints *naturalnumber*))
    (|posint| (setq *posint* type))
    (|negint| (setq *negint* type))
    (|posrat| (setq *posrat* type))
    (|negrat| (setq *negrat* type))
    (|even_int| (setq *even_int* type))
    (|odd_int| (setq *odd_int* type))
    (|even_nat| (setq *even_nat* type))
    (|even_posnat| (setq *even_posnat* type))
    (|odd_posnat| (setq *odd_posnat* type))
    (|even_negint| (setq *even_negint* type))
    (|odd_negint| (setq *odd_negint* type))
    (|ordinal| (setq *ordinal* type))
    (|string| (setq *string-type* type))
    (|character| (setq *character* type)
		 (setq *char* (tc-type "below[0x110000]")))
    (|uint8| (setq *uint8* type))
    (|uint16| (setq *uint16* type))
    (|uint32| (setq *uint32* type))
    (|uint64| (setq *uint64* type))
    (|int8| (setq *int8* type))
    (|int16| (setq *int16* type))
    (|int32| (setq *int32* type))
    (|int64| (setq *int64* type))
    ;; Pos and neg versions of uint8, etc.
    (|even_uint8| (setq *even_uint8* type))
    (|pos_uint8| (setq *pos_uint8* type))
    (|pos_uint16| (setq *pos_uint16* type))
    (|pos_uint32| (setq *pos_uint32* type))
    (|pos_uint64| (setq *pos_uint64* type))
    (|neg_int8| (setq *neg_int8* type))
    (|neg_int16| (setq *neg_int16* type))
    (|neg_int32| (setq *neg_int32* type))
    (|neg_int64| (setq *neg_int64* type))
    ;; Even and odd versions of the above
    (|even_pos_uint8| (setq *even_pos_uint8* type))
    (|even_pos_uint16| (setq *even_pos_uint16* type))
    (|even_pos_uint32| (setq *even_pos_uint32* type))
    (|even_pos_uint64| (setq *even_pos_uint64* type))
    (|even_neg_int8| (setq *even_neg_int8* type))
    (|even_neg_int16| (setq *even_neg_int16* type))
    (|even_neg_int32| (setq *even_neg_int32* type))
    (|even_neg_int64| (setq *even_neg_int64* type))
    (|odd_pos_uint8| (setq *odd_pos_uint8* type))
    (|odd_pos_uint16| (setq *odd_pos_uint16* type))
    (|odd_pos_uint32| (setq *odd_pos_uint32* type))
    (|odd_pos_uint64| (setq *odd_pos_uint64* type))
    (|odd_neg_int8| (setq *odd_neg_int8* type))
    (|odd_neg_int16| (setq *odd_neg_int16* type))
    (|odd_neg_int32| (setq *odd_neg_int32* type))
    (|odd_neg_int64| (setq *odd_neg_int64* type))
    ))

;; (defun subst-new-map-decls* (obj)
;;   (subst-new-map-decl* obj))

;; (defmethod subst-new-map-decl* (obj)
;;   (break "need more subst-new-map-decl* methods - ~a : ~a" obj (type-of obj)))

;; (defmethod subst-new-map-decl* ((ex name-expr))
;;   (let ((nres (subst-new-map-decl* (resolution ex))))
;;     (if (eq nres (resolution ex))
;; 	ex
;; 	(copy ex
;; 	  :type (type res)
;; 	  :actuals (subst-new-map-decl* (actuals ex))
;; 	  :dactuals (subst-new-map-decl* (dactuals ex))
;; 	  :resolutions (list nres)))))

;; (defmethod subst-new-map-decl* ((ex adt-name-expr))
;;   (let ((nex (call-next-method)))
;;     (if (eq ex nex)
;; 	nex
;; 	(copy nex :adt-type (subst-new-map-decl* (adt-type ex))))))

;; (defmethod subst-new-map-decl* ((ex projection-expr))
;;   (with-slots (actuals dactuals type) ex
;;     (lcopy ex
;;       :actuals (subst-new-map-decl* actuals)
;;       :dactuals (subst-new-map-decl* dactuals)
;;       :type (subst-new-map-decl* type))))

;; (defmethod subst-new-map-decl* ((ex injection-expr))
;;   (with-slots (actuals dactuals type) ex
;;     (lcopy ex
;;       :actuals (subst-new-map-decl* actuals)
;;       :dactuals (subst-new-map-decl* dactuals)
;;       :type (subst-new-map-decl* type))))

;; (defmethod subst-new-map-decl* ((ex injection?-expr))
;;   (with-slots (actuals dactuals type) ex
;;     (lcopy ex
;;       :actuals (subst-new-map-decl* actuals)
;;       :dactuals (subst-new-map-decl* dactuals)
;;       :type (subst-new-map-decl* type))))

;; (defmethod subst-new-map-decl* ((ex extraction-expr))
;;   (with-slots (actuals dactuals type) ex
;;     (lcopy ex
;;       :actuals (subst-new-map-decl* actuals)
;;       :dactuals (subst-new-map-decl* dactuals)
;;       :type (subst-new-map-decl* type))))

;; (defmethod subst-new-map-decl* ((ex projection-application))
;;   (with-slots (argument actuals dactuals type index) ex
;;     (let ((narg (subst-new-map-decl* argument)))
;;       (if (eq argument narg)
;; 	  ex
;; 	  (let ((ntype (subst-new-map-decl* (type ex))))
;; 	    (lcopy ex
;; 	      :argument narg
;; 	      :actuals (subst-new-map-decl* actuals)
;; 	      :dactuals (subst-new-map-decl* dactuals)
;; 	      :type ntype))))))

;; (defmethod subst-new-map-decl* ((ex injection-application))
;;   (with-slots (argument actuals dactuals type index) ex
;;     (let ((narg (subst-new-map-decl* argument))
;; 	  (ntype (subst-new-map-decl* type)))
;;       (lcopy ex
;; 	:argument narg
;; 	:actuals (subst-new-map-decl* actuals)
;; 	:dactuals (subst-new-map-decl* dactuals)
;; 	:type ntype))))

;; (defmethod subst-new-map-decl* ((ex injection?-application))
;;   (with-slots (argument actuals dactuals type index) ex
;;     (let ((narg (subst-new-map-decl* argument))
;; 	  (ntype (subst-new-map-decl* type)))
;;       (lcopy ex
;; 	:argument narg
;; 	:actuals (subst-new-map-decl* actuals)
;; 	:dactuals (subst-new-map-decl* dactuals)
;; 	:type ntype))))

;; (defmethod subst-new-map-decl* ((ex extraction-application))
;;   (with-slots (argument actuals dactuals type index) ex
;;     (let ((narg (subst-new-map-decl* argument))
;; 	  (ntype (subst-new-map-decl* type)))
;;       (lcopy ex
;; 	:argument narg
;; 	:actuals (subst-new-map-decl* actuals)
;; 	:dactuals (subst-new-map-decl* dactuals)
;; 	:type ntype))))

;; (defmethod subst-new-map-decl* ((ex field-application))
;;   (with-slots (id argument type) ex
;;     (let ((narg (subst-new-map-decl* argument))
;; 	  (ntype (subst-new-map-decl* type)))
;;       (lcopy ex :argument narg :type ntype))))

;; (defmethod subst-new-map-decl* ((res resolution))
;;   (with-slots (module-instance declaration type) res
;;     (let* ((smodinst (subst-new-map-decl* module-instance))
;; 	   (stype (subst-new-map-decl* type))
;; 	   (nres (if (and (eq smodinst module-instance)
;; 			  (eq stype type))
;; 		     res
;; 		     (mk-resolution declaration smodinst stype))))
;;       nres)))

;; (defmethod subst-new-map-decl* ((ex modname))
;;   (lcopy ex
;;     :actuals (subst-new-map-decl* (actuals expr))
;;     :dactuals (subst-new-map-decl* (dactuals expr))))

;; (defmethod subst-new-map-decl* ((act actual))
;;   (with-slots (expr type-value) act
;;     (let ((ntype (when type-value (subst-new-map-decl* type-value))))
;;       (lcopy act
;; 	:expr (cond ((and ntype (eq ntype type-value))
;; 		     expr)
;; 		    (type-value ntype)
;; 		    (t (let ((nexpr (subst-new-map-decl* expr)))
;; 			 (pseudo-normalize nexpr))))
;; 	:type-value ntype))))

;; (defmethod subst-new-map-decl* ((ex application))
;;   (with-slots (operator argument) ex
;;     (let ((op (subst-new-map-decl* operator))
;; 	  (arg (subst-new-map-decl* argument)))
;;       (if (and (eq op operator)
;; 	       (eq arg argument))
;; 	  ex
;; 	  (copy ex
;; 	    :operator op
;; 	    :argument arg
;; 	    :type (subst-new-map-decl* (type ex)))))))

;; (defmethod subst-new-map-decl* ((ex record-expr))
;;   (let ((ntype (subst-new-map-decl* (type ex))))
;;     (lcopy ex
;;       :assignments (subst-new-map-decl* (assignments expr))
;;       :type ntype)))

;; (defmethod subst-new-map-decl* ((ex tuple-expr))
;;   (let ((nexprs (subst-new-map-decl* (exprs expr))))
;;     (if (eq nexprs (exprs ex))
;; 	ex
;; 	(let ((ntype (if (every #'(lambda (nex oex) (eq (type nex) (type oex)))
;; 				nexprs (exprs ex))
;; 			 (type ex)
;; 			 (mk-tupletype (mapcar #'type nexprs)))))
;; 	  (copy ex
;; 	    :exprs nexprs
;; 	    :type ntype)))))

;; (defmethod subst-new-map-decl* ((list list))
;;   (let ((slist (subst-new-map-decl-list list nil)))
;;     (if (equal list slist) list slist)))

;; (defun subst-new-map-decl-list (list result)
;;   (if (null list)
;;       (nreverse result)
;;       (subst-new-map-decl-list
;;        (cdr list) (cons (subst-new-map-decl* (car list)) result))))

;; (defmethod subst-new-map-decl* ((ex update-expr))
;;   (let ((ntype (subst-new-map-decl* (type expr))))
;;     (lcopy ex
;;       :expression (subst-new-map-decl* (expression ex))
;;       :assignments (subst-new-map-decl* (assignments ex))
;;       :type ntype)))

;; (defmethod subst-new-map-decl* ((ex assignment))
;;   (lcopy ex
;;     :arguments (subst-new-map-decl* (arguments ex))
;;     :expression (subst-new-map-decl* (expression ex))))

;; (defmethod subst-new-map-decl* ((ex binding-expr))
;;   (break "Need subst-new-map-decl* binding-expr"))
