;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add-decl.lisp -- add or modify a declaration, even in the middle of a
;;                  proof.
;; Author          : Sam Owre
;; Created On      : Mon Dec 13 22:47:45 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Sun Apr 30 13:38:19 1995
;; Update Count    : 6
;; Status          : Alpha test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package 'pvs)

;;; Add declaration

(defvar *add-declaration-info* nil)

(defun add-declaration-at (filename line)
  (typecheck-file filename nil t)
  (let ((theory (find-theory-at filename line)))
    (if theory
	(let* ((decl (get-decl-at line t (list theory)))
	       (pdecl (previous-decl decl theory))
	       (date (file-write-date (make-specpath filename))))
	  (cond ((or decl pdecl)
		 (when *add-declaration-info*
		   (pvs-message "Discarding previous add-declaration"))
		 (setq *add-declaration-info*
		       (list pdecl decl theory filename date line))
		 t)
		(t (pvs-message
		       "Theory must have at least one declaration"))))
	(pvs-message "Cursor must be within a theory"))))

(defun previous-decl (decl theory)
  (if decl
      (cadr (memq decl
		  (delete-if #'(lambda (d)
				 (or (typep d '(or using field-decl))
				     (generated-by d)))
			     (reverse (append (assuming theory)
					      (theory theory))))))
      (if (theory theory)
	  (car (last (theory theory)))
	  (car (last (assuming theory))))))

(defun typecheck-add-declaration (declfile &optional update-theory?)
  (if *add-declaration-info*
      (if (add-declaration-info-current?)
	  (let* ((*from-buffer* "Add Declaration")
		 (*tc-add-decl* t)
		 (pdecl (first *add-declaration-info*))
		 (odecl (second *add-declaration-info*))
		 (theory (third *add-declaration-info*))
		 (filename (fourth *add-declaration-info*))
		 (oplace (if odecl
			     (place-list (place odecl))
			     (list (sixth *add-declaration-info*)
				   (starting-col (place pdecl)))))
		 (assuming? (member odecl (assuming theory)))
		 (decls (parse :file declfile
			       :nt (if assuming?
				       'assumings
				       'theory-part)))
		 (typechecked? (typechecked-file? filename))
		 (*current-theory* theory))
	    (when (or typechecked? update-theory?)
	      (typecheck-new-decls decls pdecl))
	    (when update-theory?
	      (when *in-checker*
		(setq *context-modified* t))
	      (let ((*current-context* (when typechecked? (context pdecl))))
		(add-declarations-to-theory decls typechecked? assuming?))
	      (add-new-decls-to-context pdecl decls theory)
	      (reset-add-decl-places odecl decls theory filename)
	      (let ((fe (get-context-file-entry filename)))
		(ignore-errors (delete-file (make-binpath filename)))
		(when fe
		  (setf (ce-object-date fe) nil)
		  (setq *pvs-context-changed* t))))
	    (list filename oplace))
	  (pvs-message "File has been modified"))
      (pvs-message "Not adding declaration")))

(defun add-new-decls-to-context (pdecl decls theory)
  (let ((context (saved-context theory)))
    (dolist (d decls)
      (put-decl d (local-decls context))))
  (when (and *in-checker*
	     *current-context*
	     (eq theory (module *current-context*))
	     (memq (declaration *current-context*)
		   (cdr (memq pdecl (append (assuming theory)
					    (theory theory))))))
    (dolist (d decls)
      (put-decl d (local-decls *current-context*)))))

(defun typecheck-new-decls (decls pdecl)
  (let ((*insert-add-decl* nil)
	(*current-context* (decl-context pdecl t))
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
	(remtheories (cdr (memq theory (gethash filename *pvs-files*)))))
    (reset-places* remdecls line-diff)
    (incf (ending-row (place theory)) line-diff)
    (reset-places* remtheories line-diff)))

(defun add-declaration-info-current? ()
  (let ((filename (cadddr *add-declaration-info*)))
    (and *add-declaration-info*
	 (parsed-file? filename)
	 (eql (fifth *add-declaration-info*)
	      (parsed-date (make-specpath filename))))))

(defun add-declarations-to-theory (decls typechecked? assuming?)
  (reset-places decls)
  (cond (typechecked?
	 ;; Check that declarations are unique
	 (dolist (d (remove-if #'using? decls))
	   (let ((tdecls (gethash (id d) (declarations (module d)))))
	     (mapc #'(lambda (td) (duplicate-decls d td)) tdecls)))
	 (dolist (d decls)
	   (add-declaration-to-theory d assuming?)))
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

(defun add-declaration-to-theory (decl assuming?)
  (add-decl decl t nil assuming?)
  (setf (declaration *current-context*) decl)
  (when (declaration? decl)
    (set-visibility decl)
    (mapc #'(lambda (d) (add-declaration-to-theory d assuming?))
	  (generated decl))))

(defun add-decl (decl &optional (insert? t) (generated? t) (assuming? nil))
  (when (or (adt-def-decl? decl)
	    (using? decl)
	    (not (member decl
			 (gethash (id decl) (local-decls *current-context*))
			 :test #'add-decl-test)))
    (let* ((thry (module *current-context*))
	   (curdecl (declaration *current-context*))
	   (cdecl (when curdecl
		    (if (tcc? decl)
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
			  (tcc? decl)
                          (and (formula-decl? decl)
                               (eq (spelling decl) 'assumption)))
                       atail0 (cdr atail0)))
	   (ttail0 (if cdecl
		       (if (formal-decl? cdecl)
			   (theory thry)
			   (member cdecl (theory thry)))
		       (unless assuming?
			 (theory thry))))
	   (ttail (if (or (null cdecl)
			  (tcc? decl)
                          (and (formula-decl? decl)
                               (eq (spelling decl) 'assumption)))
                      ttail0 (cdr ttail0))))
      #+pvsdebug (assert (or cdecl (not generated?)))
      #+pvsdebug (assert (or atail0 ttail0 (not insert?)
			     (not *insert-add-decl*)
			     (formal-decl? curdecl)))
      (unless (using? decl)
	(unless (binding? decl)
	  (setf (module decl) thry)
	  (when generated?
	    (setf (generated-by decl) (or (generated-by cdecl) cdecl))))
	(when generated?
	  (pushnew decl (generated curdecl))))
      (when (tcc? decl)
	(setq atail (remove-previous-formal-tccs decl atail))
	(setq ttail (remove-previous-formal-tccs decl ttail)))
      (when (and insert? *insert-add-decl*)
	(cond (atail0
	       (setf (assuming thry)
		     (if cdecl
			 (append (ldiff (assuming thry) atail)
				 (cons decl atail))
			 (cons decl atail))))
	      (t ;;ttail
	       (setf (theory thry)
		     (if cdecl
			 (append (ldiff (theory thry) ttail)
				 (cons decl ttail))
			 (cons decl ttail))))))
      (unless (or (using? decl)
		  (null *insert-add-decl*))
	(put-decl decl (local-decls *current-context*))
	(put-decl decl (declarations thry)))
      decl)))

(defun remove-previous-formal-tccs (decl decls)
  (if (and (car decls)
	   (tcc? (car decls))
	   (typep (generated-by decl) 'formal-decl)
	   (typep (generated-by (car decls)) 'formal-decl)
	   (not (eq (generated-by decl) (generated-by (car decls)))))
      (remove-previous-formal-tccs decl (cdr decls))
      decls))

(defun reset-places (added-decls)
  (let* ((theory (caddr *add-declaration-info*))
	 (all-decls (append (assuming theory) (theory theory)))
	 (rem-decls (cdr (memq (car (last added-decls)) all-decls)))
	 (remplace (if rem-decls
		       (place (car rem-decls))
		       (vector (sixth *add-declaration-info*) 2)))
	 (added-line-diff (1- (line-begin remplace)))
	 (rem-line-diff (+ (line-end (place (car (last added-decls)))) 1)))
    (reset-places* added-decls added-line-diff
		   (col-begin remplace))
    (reset-places* rem-decls rem-line-diff)))

(defun reset-places* (decls line-diff &optional (col 0))
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

(defun modify-declaration-at (filename line)
  (if (parsed-file? filename)
      (multiple-value-bind (decl theory)
	  (get-decl-at line t (get-theories filename))
	(cond ((null decl)
	       (pvs-message "Could not find associated declaration"))
	      ((not (typep decl '(or const-decl formula-decl)))
	       (pvs-message "May only modify formulas and constants"))
	      ((chain? decl)
	       (pvs-message "May not modify multiple declaration"))
	      (t (let ((date (file-write-date (make-specpath filename))))
;		   (when *mod-declaration-info*
;		     (pvs-message "Discarding previous modify-declaration"))
		   (setq *mod-declaration-info*
			 (list decl theory filename date))
		   (pvs-buffer "Modify Declaration"
		     (unparse-decl decl))
		   t))))
      (pvs-message "~a has not been parsed" filename)))

(defun typecheck-mod-declaration (declfile &optional update-theory?)
  (if *mod-declaration-info*
      (if (mod-declaration-info-current?)
	  (let* ((*from-buffer* "Modify Declaration")
		 (*tc-add-decl* t)
		 (decl (first *mod-declaration-info*))
		 (theory (second *mod-declaration-info*))
		 (filename (third *mod-declaration-info*))
		 (pdecl (previous-decl decl theory))
		 (oplace (place-list (place decl)))
		 (decls (parse :file declfile
			       :nt (if (member decl (assuming theory))
				       'assumings 'theory-part)))
		 (typechecked? (typechecked-file? filename))
		 (*current-theory* theory))
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
		#+allegro-v4.3
		(maphash #'(lambda (res body)
			     (when (eq (declaration res) decl)
			       (remhash res *create-formulas-cache*)))
			 *create-formulas-cache*)
		#-allegro-v4.3
		(pvs-maphash #'(lambda (res body)
				 (when (eq (declaration res) decl)
				   (pvs-remhash res *create-formulas-cache*)))
			     *create-formulas-cache*))
	      (reset-places* decls (1- (car oplace)) (cadr oplace))
	      (change-old-decl-to-new decl (car decls))
	      (unless (= (ending-row (place decl)) (caddr oplace))
		(reset-mod-decl-places decl theory filename
				       (- (ending-row (place decl))
					  (caddr oplace))))
	      (setf (generated decl) (generated (car decls)))
	      (dolist (d (generated decl))
		(setf (gethash (id d) (declarations theory))
		      (remove d (gethash (id d)
					 (declarations theory))))
		(when (and *in-checker*
			   *current-context*)
		  (setf (gethash (id d)
				 (local-decls *current-context*))
			(remove d (gethash (id d)
					   (local-decls
					    *current-context*))))))
	      (dolist (d (generated decl))
		(put-decl d (declarations theory))
		(when (and *in-checker*
			   *current-context*
			   (eq theory (module *current-context*))
			   (memq (declaration *current-context*)
				 (memq decl (append (assuming theory)
						    (theory theory)))))
		  (mapc #'(lambda (d)
			    (put-decl d (local-decls *current-context*)))
			(generated decl))))
	      (when (and (typep decl 'formula-decl)
			 (eq (proof-status decl) 'proved))
		(setf (proof-status decl) 'unchecked))
	      (let ((fe (get-context-file-entry filename)))
		(ignore-errors (delete-file (make-binpath filename)))
		(when fe
		  (setf (ce-object-date fe) nil)
		  (setq *pvs-context-changed* t)))
	      (list filename oplace)))
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
	(remtheories (cdr (memq theory (gethash filename *pvs-files*)))))
    (reset-places* remdecls line-diff)
    (incf (ending-row (place theory)) line-diff)
    (reset-places* remtheories line-diff)))

(defmethod change-old-decl-to-new (old new)
  (setf (place old) (place new))
  (unless (eq (class-of old) (class-of new))
    (change-class old (class-of new))))

(defmethod change-old-decl-to-new ((old const-decl) (new const-decl))
  (call-next-method)
  (mapobject #'(lambda (ex)
		 (when (and (typep ex '(and name-expr
					    (not field-assignment-arg)))
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
  (setf (kind old) (kind new))
  (setf (tcc-disjuncts old) (tcc-disjuncts new)))
  

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
  nil)

(defun mod-declaration-info-current? ()
  (let ((filename (third *mod-declaration-info*)))
    (and *mod-declaration-info*
	 (parsed-file? filename)
	 (eql (fourth *mod-declaration-info*)
	      (parsed-date (make-specpath filename))))))
