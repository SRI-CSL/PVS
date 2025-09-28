;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-decl.lisp -- add or modify a declaration, even in the middle of a
;;                  proof.
;; Author          : Sam Owre
;; Created On      : Mon Dec 13 22:47:45 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr 30 13:38:19 1995
;; Update Count    : 6
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

;;; Add declaration

;; (prevdecl decl theory fileref date line)
(defvar *add-declaration-info* nil)

(defun add-declaration-at (fileref line)
  (with-pvs-file (filename) fileref
    (typecheck-file filename nil nil nil t)
    (let ((theory (find-theory-at filename line)))
      (if theory
	  (let* ((decl (get-decl-at line t (list theory))) ; may be nil if line is after last decl
		 (pdecl (previous-decl decl theory)) ; may be nil if no previous decl
		 (date (file-write-time (make-specpath filename))))
	    (when *add-declaration-info*
	      (pvs-message "Discarding previous add-declaration"))
	    (setq *add-declaration-info*
		  (list pdecl decl theory fileref date line))
	    t)
	  (pvs-message "Cursor must be within a theory")))))

(defun previous-decl (decl theory)
  (if decl
      (let ((decls (delete-if #'(lambda (d)
				  (or (typep d 'field-decl)
				      (generated-by d)))
		     (reverse (append (assuming theory)
				      (theory theory))))))
	(assert (memq decl decls))
	(cadr (memq decl decls)))
      (if (theory theory)
	  (car (last (theory theory)))
	  (car (last (assuming theory))))))

(defun typecheck-add-declaration (declfile &optional update-theory?)
  "Called from Emacs install-add-declaration, which creates the temporary
declfile containing the declaration(s) to be inserted.  update-theory? is
always t from there."
  (unless *add-declaration-info*
    (pvs-error "add-declaration error" "Not adding declaration; call add-declaration first"))
  (unless (add-declaration-info-current?)
    (pvs-error "add-declaration error" "File has been modified since add-declaration called"))
  (multiple-value-bind (pdecl odecl theory fileref filedate line)
      (values-list *add-declaration-info*)
    (declare (ignore filedate))
    (let ((cur-ctx *current-context*))
      (with-pvs-file (filename) fileref
	(let* ((*current-context* (cond (pdecl (decl-context pdecl))
					(odecl (decl-context odecl t))
					(t (context theory))))
	       (*from-buffer* "Add Declaration")
	       (*tc-add-decl* t)
	       (declplace (cond (pdecl (place-list (place pdecl)))
				(odecl (place-list (place odecl)))
				(t (list line 2 line 3))))
	       (assuming? (cond (pdecl (memq pdecl (assuming theory)))
				(odecl (memq odecl (assuming theory)))
				(t nil)))
	       (new-decls (parse :file declfile
				 :nt (if assuming?
					 'assumings
					 'theory-part)))
	       (typechecked? (typechecked-file? filename)))
	  (when (or typechecked? update-theory?)
	    ;; calls typecheck-decl on new-decls but shouldn't add them
	    ;; to either the theory or the declarations-hash
	    (typecheck-new-decls new-decls))
	  (when update-theory?
	    (when *in-checker*
	      (setq *context-modified* t))
	    (add-declarations-to-theory new-decls typechecked? assuming? pdecl odecl)
	    (assert cur-ctx)
	    (add-new-decls-to-contexts pdecl odecl new-decls theory cur-ctx)
	    ;;(reset-add-decl-places odecl new-decls theory filename)
	    (pushnew 'modified (status theory))
	    (let ((fe (get-context-file-entry filename)))
	      (when fe
		(setf (ce-object-date fe) nil)
		(setf (current-pvs-context-changed) t))))
	  ;; We've made all the changes, clear out the info as it isn't valid anymore.
	  (setq *add-declaration-info* nil)
	  (list fileref declplace))))))

(defun add-new-decls-to-contexts (pdecl odecl new-decls theory cur-ctx)
  ;; First add to the contexts of the local theory - must update the
  ;; saved-contexts of any importing following pdecl, as well as the
  ;; saved-context of the theory.
  (dolist (elt (cdr (memq (or pdecl odecl) (all-decls theory))))
    (when (importing? elt)
      (add-new-decls-to-context new-decls (saved-context elt))))
  (add-new-decls-to-context new-decls (saved-context theory))
  ;; Now add to all other theory contexts that import theory
  (maphash #'(lambda (id th)
	       (declare (ignore id))
	       (unless (eq th theory)
		 (dolist (elt (all-decls th))
		   (when (and (importing? elt)
			      (saved-context elt)
			      (get-importings
			       theory
			       (using-hash (saved-context elt))))
		     (add-new-decls-to-context new-decls (saved-context elt))))
		 (when (and (saved-context th)
			    (get-importings theory
					    (using-hash (saved-context th))))
		   (add-new-decls-to-context new-decls (saved-context th)))))
	   (current-pvs-theories))
  ;; Now add to the current prover/evaluator context
  (assert cur-ctx)
  (when cur-ctx
    (let ((*current-context* cur-ctx))
      (when (if (eq theory (current-theory))
		(memq (current-declaration)
		      (memq (or pdecl odecl) (all-decls theory)))
		(get-importings theory (current-using-hash))))
      (cond (*in-checker*
	     (add-new-decls-to-prover-contexts new-decls *top-proofstate*))
	    (*in-evaluator*
	     (add-new-decls-to-context new-decls *current-context*)))
      (assert (every #'(lambda (d)
			 (or (not (declaration? d))
			     (memq d (get-declarations (id d)))))
		     new-decls))))
  ;; If there is an importing, we need to reset the all-importings
  (when (some #'importing? new-decls)
    ;; need to reset things up the importing chain
    (reset-importing-chains theory)
    (setf (current-pvs-context-changed) t)))

(defun reset-importing-chains (theory)
  (reset-importing-chains* theory)
  (dolist (tid (find-all-usedbys theory))
    (let ((th (get-theory tid)))
      (when th
	(reset-importing-chains* th)))))

(defun reset-importing-chains* (theory)
  (setf (all-imported-theories theory) 'unbound)
  (setf (all-imported-names theory) 'unbound)
  (setf (immediate-usings theory) 'unbound)
  (setf (all-usings theory)
	(let ((imps nil))
	  (map-lhash #'(lambda (th thinsts)
			 (unless (from-prelude? th)
			   (push (cons th thinsts) imps)))
		     (using-hash (saved-context theory)))
	  imps)))


(defun add-new-decls-to-prover-contexts (decls proofstate)
  (let ((*symbol-tables* nil))
    (declare (special *symbol-tables*))
    (add-new-decls-to-prover-contexts* decls proofstate)))

(defun add-new-decls-to-prover-contexts* (decls proofstate)
  (declare (special *symbol-tables*))
  (unless (memq (declarations-hash (context proofstate)) *symbol-tables*)
    (add-new-decls-to-context decls (context proofstate)))
  (dolist (ps (children proofstate))
    (add-new-decls-to-prover-contexts* decls ps)))

(defun add-new-decls-to-context (decls context)
  (let ((*current-context* context))
    (dolist (d decls)
      (typecase d
	(importing (add-to-using (theory-name d)))
	(declaration (put-decl d (declarations-hash context)))))))

(defun typecheck-new-decls (decls)
  (let ((*insert-add-decl* nil)
	(*generate-tccs* 'all))
    (typecheck-decls decls)
    (dolist (d decls)
      (let ((*generate-xref-declaration* d))
	(setf (refers-to d) nil)
	(generate-xref d)))))

(defun reset-add-decl-places (odecl decls theory filename)
  (let ((remdecls (if odecl
		       (memq odecl (all-decls theory))
		       (all-decls theory)))
	(line-diff (+ (- (ending-row (place (car (last decls))))
			 (starting-row (place (car decls))))
		      2))
	(remtheories (cdr (memq theory (gethash filename (current-pvs-files))))))
    (set-add-decl-places* remdecls line-diff)
    (incf (ending-row (place theory)) line-diff)
    (set-add-decl-places* remtheories line-diff)))

(defun add-declaration-info-current? ()
  (and *add-declaration-info*
       (let ((fileref (fourth *add-declaration-info*)))
	 (with-pvs-file (filename) fileref
	   (parsed-file? filename)
	   (eql (fifth *add-declaration-info*)
		(parsed-date (make-specpath filename)))))))

(defun add-declarations-to-theory (decls typechecked? assuming? pdecl odecl)
  (set-add-decl-places decls)
  (cond (typechecked?
	 ;; Check that declarations are unique
	 (dolist (d (remove-if #'importing? decls))
	   (let ((tdecls (remove-if-not #'(lambda (td)
					    (and (typep td 'declaration)
						 (eq (id td) (id d))))
			   (all-decls (module d)))))
	     (mapc #'(lambda (td) (duplicate-decls d td)) tdecls)))
	 (let ((adecl pdecl))
	   (dolist (d decls)
	     (add-declaration-to-theory d assuming? adecl odecl)
	     (setq adecl d))))
	(t (let* ((odecl (cadr *add-declaration-info*))
		  (theory (caddr *add-declaration-info*))
		  (atail (memq odecl (assuming theory)))
		  (ttail (memq odecl (theory theory))))
	     (if atail
		 (setf (assuming theory)
		       (append (ldiff (assuming theory) atail)
			       (append decls atail)))
		 (setf (theory theory)
		       (append (ldiff (theory theory) ttail)
			       (append decls ttail))))))))

(defun add-declaration-to-theory (decl assuming? pdecl odecl)
  ;; insert? t, generated? nil, after? t, pdecl is the one to add after
  (assert (or (null pdecl)
	      (memq pdecl (if assuming? (assuming (current-theory)) (theory (current-theory))))))
  (assert (or (null odecl)
	      (memq odecl (if assuming? (assuming (current-theory)) (theory (current-theory))))))
  (add-decl decl t nil assuming? nil pdecl odecl)
  (when (declaration? decl)
    (with-current-decl decl
      (set-visibility decl)
      (assert (memq decl (get-declarations (id decl))))
      (mapc #'(lambda (d) (add-declaration-to-theory d assuming? pdecl odecl))
	    (generated decl)))))


;;; Add-decl is used to incorporate newly generated declarations.  It
;;; inserts the new declaration after the current declaration found in
;;; the context.

;;; Side effects:
;;;   (theory decl)
;;;   (generated-by decl)
;;;   (generated (declaration *current-context*))
;;;   (assuming (theory *current-context*))
;;;   (theory (theory *current-context*))
;;;   (local-decls *current-context*)
;;;   (declarations (theory *current-context*))

(defun add-decl (decl &optional (insert? t) (generated? t) assuming? after? after-decl before-decl)
  (when (or (adt-def-decl? decl)
	    (importing? decl)
	    (mapped-decl? decl)
	    (not (member decl
			 (remove-if-not #'(lambda (d)
					    (eq (module d) (current-theory)))
			   (get-declarations (id decl)))
			 :test #'add-decl-test)))
    (let* ((thry (current-theory))
	   (curdecl (or after-decl
			before-decl
			*current-top-declaration*
			(current-declaration)))
	   (cdecl (when curdecl
		    (if (or (tcc? decl)
			    (importing? decl)) 
			curdecl
			(or (find-if-not #'(lambda (d) (tcc? d))
			      (generated curdecl))
			    curdecl))))
	   (atail0 (unless (and cdecl
				(typep cdecl 'formal-decl)
				(tcc? decl))
		     (if cdecl
			 (if (memq cdecl (formals thry))
			     (assuming thry)
			     (memq cdecl (assuming thry)))
			 (when assuming?
			   (assuming thry)))))
	   (atail (if (or (null cdecl)
			  (and (null after-decl) before-decl)
			  (tcc? decl)
			  (judgement? decl)
                          (and (formula-decl? decl)
                               (eq (spelling decl) 'ASSUMPTION)))
                       atail0 (cdr atail0)))
	   (ttail0 (if cdecl
		       (if (formal-decl? cdecl)
			   (unless assuming?
			     (theory thry))
			   (member cdecl (theory thry)))
		       (unless assuming?
			 (theory thry))))
	   (ttail (if (or (null cdecl)
			  (and (null after-decl) before-decl)
			  (tcc? decl)
			  (judgement? decl)
                          (and (formula-decl? decl)
                               (eq (spelling decl) 'ASSUMPTION)))
                      ttail0 (cdr ttail0))))
      #+pvsdebug (assert (or cdecl (not generated?)))
      #+pvsdebug (assert (or atail0 ttail0 (not insert?)
			     (not *insert-add-decl*)
			     (formal-decl? curdecl)))
      (unless (importing? decl)
	(unless (binding? decl)
	  (setf (module decl) thry)
	  (when generated?
	    (setf (generated-by decl) (or (generated-by cdecl) cdecl))))
	(when (and curdecl generated?)
	  (pushnew decl (generated curdecl))))
      (when (tcc? decl)
	(setq atail (remove-previous-formal-tccs decl atail))
	(setq ttail (remove-previous-formal-tccs decl ttail)))
      (when (and insert? *insert-add-decl*)
	(cond (atail0
	       (setf (assuming thry)
		     (if cdecl
			 (if after?
			     (append (ldiff (assuming thry) (cdr atail))
				     (cons decl (cdr atail)))
			     (append (ldiff (assuming thry) atail)
				     (cons decl atail)))
			 (cons decl atail))))
	      (t ;;ttail
	       (setf (theory thry)
		     (if cdecl
			 (if after?
			     (append (ldiff (theory thry) (cdr ttail))
				     (cons decl (cdr ttail)))
			     (append (ldiff (theory thry) ttail)
				     (cons decl ttail)))
			 (cons decl ttail)))))
	(setf (all-declarations thry) nil))
      (assert (or (importing? decl) (eq thry (module decl))))
      (assert (or (null *insert-add-decl*) (memq decl (all-decls thry))))
      (unless (or (importing? decl)
		  (null *insert-add-decl*))
	(put-decl decl (current-declarations-hash)))
      decl)))

(defun remove-previous-formal-tccs (decl decls)
  (if (and (car decls)
	   (tcc? (car decls))
	   (typep (generated-by decl) 'formal-decl)
	   (typep (generated-by (car decls)) 'formal-decl)
	   (not (eq (generated-by decl) (generated-by (car decls)))))
      (remove-previous-formal-tccs decl (cdr decls))
      decls))

(defun set-add-decl-places (added-decls)
  "Sets the places for the added-decls and the ones that follow those."
  (let* ((theory (third *add-declaration-info*))
	 (line (sixth *add-declaration-info*))
	 (all-decls (append (assuming theory) (theory theory)))
	 (rem-decls (cdr (memq (car (last added-decls)) all-decls)))
	 (remplace (if rem-decls
		       (place (car rem-decls))
		       (vector line 4)))
	 (added-line-diff (1- (line-begin remplace)))
	 (rem-line-diff (+ (line-end (place (car (last added-decls)))) 1)))
    (set-add-decl-places* added-decls added-line-diff (col-begin remplace))
    (set-add-decl-places* rem-decls rem-line-diff)))

(defun set-add-decl-places* (decls line-diff &optional (col 0))
  (mapobject #'(lambda (x)
		 (when (and (syntax? x)
			    (place x))
		   (incf (starting-row (place x)) line-diff)
		   (incf (ending-row (place x)) line-diff)
		   (unless (zerop col)
		     (incf (starting-col (place x)) col)
		     (incf (ending-col (place x)) col))
		   nil))
	     decls))


;;; modify-declaration

(defvar *mod-declaration-info* nil)

(defun modify-declaration-at (fileref line)
  (with-pvs-file (filename) fileref
    (if (parsed-file? filename)
	(let ((decl (get-decl-at line t (get-theories filename))))
	  (cond ((null decl)
		 (pvs-message "Could not find associated declaration"))
		((not (typep decl '(or const-decl formula-decl)))
		 (pvs-message "May only modify formulas and constants"))
		((chain? decl)
		 (pvs-message "May not modify multiple declaration"))
		(t (let ((date (file-write-date (make-specpath filename))))
		     ;; (when *mod-declaration-info*
		     ;;   (pvs-message "Discarding previous modify-declaration"))
		     (setq *mod-declaration-info* (list decl fileref line date))
		     (pvs-buffer "Modify Declaration"
		       (unparse-decl decl))
		     t))))
	(pvs-message "~a has not been parsed" filename))))

(defun typecheck-mod-declaration (declfile &optional update-theory?)
  (if *mod-declaration-info*
      (if (mod-declaration-info-current?)
	  (with-pvs-file (filename) (second *mod-declaration-info*)
	    (let* ((*from-buffer* "Modify Declaration")
		   (*tc-add-decl* t)
		   (decl (first *mod-declaration-info*))
		   (theory (module decl))
		   (oplace (place-list (place decl)))
		   (decls (parse :file declfile
				 :nt (if (member decl (assuming theory))
					 'assumings 'theory-part)))
		   (typechecked? (typechecked-file? filename)))
	      (verify-mod-decl decl decls)
	      (when (or typechecked? update-theory?)
		(typecheck-mod-decls decls decl)
		(when (and (typed-declaration? decl)
			   (not (tc-eq (type decl) (type (car decls)))))
		  (type-error (car decls)
		    "Modified declaration must have the same type - ~a"
		    (type decl))))
	      (when update-theory?
		(when *in-checker*
		  (setq *context-modified* t)
		  (when (memq decl (collect-dependent-decls *top-proofstate*))
		    (pvs-message
			"Declaration ~a was used earlier in the current proof"
		      (id decl))))
		(when *create-formulas-cache*
		  (maphash #'(lambda (res body)
			       (declare (ignore body))
			       (when (eq (declaration res) decl)
				 (remhash res *create-formulas-cache*)))
			   *create-formulas-cache*))
		(set-add-decl-places* decls (1- (car oplace)) (cadr oplace))
		(change-old-decl-to-new decl (car decls))
		(unless (= (ending-row (place decl)) (caddr oplace))
		  (reset-mod-decl-places decl theory filename
					 (- (ending-row (place decl))
					    (caddr oplace))))
		(setf (generated decl) (generated (car decls)))
		(dolist (d (generated decl))
		  ;; 		(setf (gethash (id d) (declarations theory))
		  ;; 		      (remove d (gethash (id d)
		  ;; 					 (declarations theory))))
		  (when (and *in-checker*
			     *current-context*)
		    (setf (get-lhash (id d) (current-declarations-hash))
			  (remove d (get-lhash (id d)
					       (current-declarations-hash))))))
		(dolist (d (generated decl))
		  ;;		(put-decl d (declarations theory))
		  (when (and *in-checker*
			     *current-context*
			     (eq theory (current-theory))
			     (memq (current-declaration)
				   (memq decl (append (assuming theory)
						      (theory theory)))))
		    (put-decl d (current-declarations-hash))))
		(when (and (typep decl 'formula-decl)
			   (eq (proof-status decl) 'proved))
		  (setf (proof-status decl) 'unchecked))
		(let ((fe (get-context-file-entry filename)))
		  (ignore-errors (delete-file (make-binpath filename)))
		  (when fe
		    (setf (ce-object-date fe) nil)
		    (setf (current-pvs-context-changed) t)))
		(list filename oplace))))
	  (pvs-message "File has been modified"))
      (pvs-message "Not adding declaration")))

(defun typecheck-mod-decls (decls decl)
  (let ((*insert-add-decl* nil)
	(*current-context* (decl-context decl nil))
	(*generate-tccs* 'all))
    (typecheck-decls decls)
    (dolist (d decls)
      (let ((*generate-xref-declaration* d))
	(setf (refers-to d) nil)
	(generate-xref d)))))

(defun reset-mod-decl-places (decl theory filename line-diff)
  (let ((remdecls (cdr (memq decl (all-decls theory))))
	(remtheories (cdr (memq theory (gethash filename (current-pvs-files))))))
    (set-add-decl-places* remdecls line-diff)
    (incf (ending-row (place theory)) line-diff)
    (set-add-decl-places* remtheories line-diff)))

(defmethod change-old-decl-to-new (old new)
  (setf (place old) (place new))
  (unless (eq (class-of old) (class-of new))
    (change-class old (class-of new))))

(defmethod change-old-decl-to-new ((old const-decl) (new const-decl))
  (call-next-method)
  (mapobject #'(lambda (ex)
		 (when (and (typep ex '(and name-expr
					(not field-assignment-arg)
					(not binding)))
			    (eq (declaration (resolution ex)) new))
		   (setf (declaration (resolution ex)) old)))
	     (definition new))
  (setf (definition old) (definition new))
  (setf (def-axiom old) (def-axiom new)))

(defmethod change-old-decl-to-new ((old const-decl) (new def-decl))
  (call-next-method)
  (setf (declared-measure old) (declared-measure new))
  (setf (ordering old) (ordering new))
  (setf (measure old) (measure new))
  (setf (measure old) (measure new)))

(defmethod change-old-decl-to-new ((old formula-decl) (new formula-decl))
  (call-next-method)
  (setf (spelling old) (spelling new))
  (setf (definition old) (definition new))
  (setf (closed-definition old) (closed-definition new))
  (setf (kind old) (kind new)))
  

(defun verify-mod-decl (decl decls)
  (unless (singleton? decls)
    (type-error (cadr decls)
      "Only one declaration is allowed"))
  (unless (same-id decl (car decls))
    (type-error (car decls)
      "Modified declaration must keep the same id - ~a" (id decl)))
  (unless (same-decl-types decl (car decls))
    (type-error (car decls)
      "Modified declaration must be the same kind - ~a" (type-of decl))))

(defmethod same-decl-types ((d1 const-decl) (d2 const-decl))
  t)

(defmethod same-decl-types ((d1 formula-decl) (d2 formula-decl))
  t)

(defmethod same-decl-types (d1 d2)
  (declare (ignore d1 d2))
  nil)

;;; *mod-declaration-info* == (decl fileref line date)
(defun mod-declaration-info-current? ()
  (and *mod-declaration-info*
       (let ((fileref (second *mod-declaration-info*)))
	 (with-pvs-file (filename) fileref
	   (and (parsed-file? filename)
		(eql (fourth *mod-declaration-info*)
		     (parsed-date (make-specpath filename))))))))
