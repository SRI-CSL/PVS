;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compare.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sat Feb 26 21:41:39 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Nov  5 15:07:41 1998
;; Update Count    : 15
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The entry point is compare, which compares two theories and returns two
;;; values: an indication of whether they compare and an association list
;;; which indicates the differences found.  The possible association list
;;; entries are:
;;;    (oexporting nexporting difference)
;;;    (ousing nusing difference)
;;;    (oconstructor nconstructor difference)
;;;    (odecl ndecl difference)
;;; where
;;;   difference - added, deleted, reordered, changed, signature, body

;;; compare compares two theories, generally calling compare-decl-lists on the
;;; formals, etc.  These set the variable *differences* according to the
;;; differences found.  Within compare-decl-lists, compare-sig and compare-bod
;;;; are calle, and if any differences are found either 'signature or 'body is
;;; pushed onto *decl-diffs*, and is later added to *differences*.

(in-package 'pvs)

(defvar *differences* nil)
(defvar *decl-diffs* nil)
(defvar *needs-retypechecking* nil)
(defvar *compare-selections* nil)

(defun compare (old new)
  (assert (and (typep old 'datatype-or-module)
	       (typep new 'datatype-or-module)))
  (compare* old new))


;;; No need to check syntax class, everything will be copied over anyway.

(defmethod compare* :around ((old datatype-or-module) (new datatype-or-module))
  (if (typep old 'inline-datatype)
      (call-next-method)
      (let ((*differences* nil))
	(call-next-method)
	;; id
	(compare-decl-lists (formals old) (formals new))
	(compare-decl-lists (assuming old) (assuming new))
	;; end-id
	;; filename
	;; status
	;; generated
	;; generated-by
	*differences*)))

(defmethod compare* ((old module) (new module))
  (assert (and (exporting old) (exporting new)))
  ;; declarations
  ;; types
  ;; implementing
  (compare* (exporting old) (exporting new))
  ;; all-usings
  ;; used-by
  (compare-decl-lists (theory old) (theory new)))

(defmethod compare* ((old datatype) (new datatype))
  (cond ((and (importings old) (importings new))
	 (compare* (importings old) (importings new)))
	((importings old)
	 (push (list (importings old) 'del) *differences*))
	((importings new)
	 (push (list (importings new) 'add) *differences*)))
  (compare-decl-lists (constructors old) (constructors new)))

(defmethod compare* ((old module) (new datatype))
  (push (list old new 'changed) *differences*))

(defmethod compare* ((old datatype) (new module))
  (push (list old new 'changed) *differences*))

(defmethod compare* ((old inline-datatype) (new inline-datatype))
  (when (eq (id old) (id new))
    (cond ((and (importings old) (importings new))
	   (compare* (importings old) (importings new)))
	  ((importings old)
	   (push (list (importings old) 'del) *differences*))
	  ((importings new)
	   (push (list (importings new) 'add) *differences*)))
    (compare-decl-lists (constructors old) (constructors new))
    (values t *decl-diffs*)))

(defmethod compare* :around ((old enumtype) (new enumtype))
  (compare-decl-lists (constructors old) (constructors new))
  (values t *decl-diffs*))

(defmethod compare* ((old simple-constructor) (new simple-constructor))
  (compare-decl-lists (arguments old) (arguments new))
  (and (same-id old new)
       (compare* (recognizer old) (recognizer new))))

(defmethod compare* ((old exporting) (new exporting))
  (let* (;;(*decl-diffs* nil)
	 (same? (and (eq (kind old) (kind new))
		     (if (and (listp (names old)) (listp (names new)))
			 (compare* (names old) (names new))
			 (eq (names old) (names new)))
		     (compare* (but-names old) (but-names new))
		     (compare* (modules old) (modules new)))))
    (unless same?
      (push (list old new 'changed) *differences*))))

(defmethod compare* ((old importing) (new importing))
  (let ((*decl-diffs* nil))
    (compare-sig (theory-name old) (theory-name new))
    (values t *decl-diffs*)))


;;; compare-decl-lists is called on the top-level declarations lists.
;;; Given declaration lists A = (a1 ... am), B = (b1 ... bn), it returns
;;; a list of the form 
;;;

(defvar *compare-saved* nil)

(defun compare-decl-lists (olist nlist)
  (let* ((rolist (if *compare-saved*
		     olist
		     (remove-if #'(lambda (d)
				    (and (typep d 'declaration)
					 (generated-by d)))
		       olist)))
	 (diffs (compare-decls rolist nlist nil)))
    ;; Here we order the differences, checking whether the ADDs and DELs
    ;; affect other declarations
    (when diffs
      (push diffs *differences*))))


;;; compare-decls returns a list of the form
;;;  ((odecl ndecl 'BODY) ... (odecl ndecl 'SIG) ... (ndecl 'ADD) (odecl 'DEL))

(defun compare-decls (olist nlist result)
  (if (null olist)
      (if (null nlist)
	  (nreverse result)
	  (nconc (nreverse result)
		 (mapcar #'(lambda (ne) (list ne 'ADD)) nlist)))
      (if (null nlist)
	  (nconc (nreverse result)
		 (mapcar #'(lambda (oe) (list oe 'DEL)) olist))
	  (compare-decls* olist nlist result))))

(defun compare-decls* (olist nlist result)
  (let ((nelt (car nlist)))
    (multiple-value-bind (oelt compare)
	(find-comparison olist nelt)
      (cond ((eq oelt (car olist))
	     (compare-decls (cdr olist) (cdr nlist)
			    (if compare
				(cons (list oelt nelt compare) result)
				result)))
	    ((null oelt)
	     (compare-decls olist (cdr nlist)
			    (cons (list 'ADD nelt) result)))
	    (t (compare-decls (remove oelt olist) (cdr nlist)
			      (if compare
				  (cons (list oelt nelt compare) result)
				  result)))))))


(defun find-comparison (olist nelt &optional prevs)
  (when olist
    (multiple-value-bind (match? diff)
	(compare* (car olist) nelt)
      (cond (match?
	     (let ((oelt (car olist)))
	       (dolist (p prevs)
		 (when (and (typep oelt 'declaration)
			    (member (id oelt) (refers-to p)
				    :test #'(lambda (x y) (eq x (id y)))))
		   (pushnew p *needs-retypechecking* :test #'eq)))
	       (if (or (member oelt *needs-retypechecking* :test #'eq)
		       (and prevs
			    (declaration? oelt)
			    (intersection prevs (refers-to oelt))))
		   (values oelt (if (eq diff 'SIGNATURE) diff 'BODY))
		   (values oelt diff))))
	    ((and (typep (car olist) 'declaration)
		  (generated-by (car olist)))
	     (find-comparison (cdr olist) nelt (cons (car olist) prevs)))))))


;;; Compare* on declarations returns NIL if the ids or classes don't
;;; match, otherwise returns a list of the form (odecl ndecl level)
;;; where level is BODY or SIGNATURE.  LEXICAL differences will be
;;; handled during update, since it simply copies the new stuff over.

;;; Declarations

(defmethod compare* :around ((old declaration) (new declaration))
  (when (and (eq (id old) (id new))
	     (eq (type-of old) (type-of new)))
    (let ((*decl-diffs* nil))
      (compare-sig (formals old) (formals new))
      (call-next-method)
      (values t *decl-diffs*))))

(defmethod compare* ((old typed-declaration) (new typed-declaration))
  (compare-sig (declared-type old) (declared-type new)))

;(defmethod compare* ((old formal-decl) (new formal-decl))
;  (call-next-method))

(defmethod compare* ((old mod-decl) (new mod-decl))
  (and (call-next-method)
       (compare-sig (modname old) (modname new))))

;(defmethod compare* ((old type-decl) (new type-decl))
;  (call-next-method))

(defmethod compare* ((old type-def-decl) (new type-def-decl))
  (and (call-next-method)
       (compare-bod (contains old) (contains new))
       (compare-sig (type-expr old) (type-expr new))))

;(defmethod compare* ((old var-decl) (new var-decl))
;  (call-next-method))

(defmethod compare* ((old const-decl) (new const-decl))
  (and (call-next-method)
       (compare-bod (definition old) (definition new))))

(defmethod compare* ((old def-decl) (new def-decl))
  (and (call-next-method)
       (compare-bod (definition old) (definition new))
       (compare-bod (declared-measure old) (declared-measure new))))

(defmethod compare* ((old formula-decl) (new formula-decl))
  (and (call-next-method)
       (or (eq (spelling old) (spelling new))
	   (if (and (or (not (memq (spelling old) '(axiom postulate)))
			(not (memq (spelling new) '(axiom postulate))))
		    (or (memq (spelling old) '(assumption axiom postulate))
			(memq (spelling new) '(assumption axiom postulate))))
	       (setq *decl-diffs* 'SIGNATURE)
	       (unless *decl-diffs*
		 (setq *decl-diffs* 'SIGNATURE))))
       (compare-bod (kind old) (kind new))
       (compare-bod (definition old) (definition new))))

(defmethod compare* ((old subtype-judgement) (new subtype-judgement))
  (and (call-next-method)
       (compare-sig (declared-subtype old) (declared-subtype new))))

(defmethod compare* ((old number-judgement) (new number-judgement))
  (and (call-next-method)
       (compare-sig (number-expr old) (number-expr new))))

(defmethod compare* ((old name-judgement) (new name-judgement))
  (and (call-next-method)
       (compare-sig (name old) (name new))))

(defmethod compare* ((old application-judgement) (new application-judgement))
  (and (call-next-method)
       (compare-sig (name old) (name new))
       (compare-sig (formals old) (formals new))))

(defmethod compare* ((old conversion-decl) (new conversion-decl))
  (and (call-next-method)
       (compare-sig (name old) (name new))))

(defmethod compare* ((old typed-conversion-decl) (new typed-conversion-decl))
  (and (call-next-method)
       (compare-sig (declared-type old) (declared-type new))))

(defmethod compare* ((old field-decl) (new field-decl))
  (call-next-method))


;;; Below the declaration level, compare* simply returns t or nil
;;; according to whether the entities match

;;; Type expressions

;(defmethod compare* ((old type-expr) (new type-expr))
;  nil)

;;; This isn't handled by the name method, since type-expr comes first in
;;; the class hierarchy.

(defmethod compare* ((old type-name) (new type-name))
  (and (compare* (mod-id old) (mod-id new))
       (compare* (library old) (library new))
       (and (or (and (null (actuals old)) (null (actuals new)))
		(and (actuals old) (actuals new)))
	    (compare* (actuals old) (actuals new)))
       (compare* (id old) (id new))))

(defmethod compare* ((old type-application) (new type-application))
  (and (compare* (type old) (type new))
       (compare* (parameters old) (parameters new))))

(defmethod compare* ((old subtype) (new subtype))
  (and ;;(compare* (contains old) (contains new))
       ;;(compare* (supertype old) (supertype new))
       (compare* (predicate old) (predicate new))))

(defmethod compare* ((old setsubtype) (new setsubtype))
  (and ;;(compare* (contains old) (contains new))
       (compare* (supertype old) (supertype new))
       (compare* (formals old) (formals new))
       (compare* (formula old) (formula new))))

(defmethod compare* ((old nsetsubtype) (new nsetsubtype))
  (and ;;(compare* (contains old) (contains new))
       (compare* (formals old) (formals new))
       (compare* (formula old) (formula new))))

(defmethod compare* ((old expr-as-type) (new expr-as-type))
  (and (call-next-method)
       (compare* (expr old) (expr new))))

(defmethod compare* ((old funtype) (new funtype))
  (and (compare* (domain old) (domain new))
       (compare* (range old) (range new))))

(defmethod compare* ((old tupletype) (new tupletype))
  (compare* (types old) (types new)))

(defmethod compare* ((old recordtype) (new recordtype))
  (compare* (fields old) (fields new)))


;;; Expressions

;;; Handled by name method
;(defmethod compare* ((old name-expr) (new name-expr))
;  (call-next-method))

(defmethod compare* ((old number-expr) (new number-expr))
  (compare* (number old) (number new)))

(defmethod compare* ((old tuple-expr) (new tuple-expr))
  (compare* (exprs old) (exprs new)))

(defmethod compare* ((old record-expr) (new record-expr))
  (compare* (assignments old) (assignments new)))

(defmethod compare* ((old cases-expr) (new cases-expr))
  (and (compare* (expression old) (expression new))
       (compare* (selections old) (selections new))
       (compare* (else-part old) (else-part new))))

(defmethod compare* ((old selection) (new selection))
  (and (compare* (constructor old) (constructor new))
       (let ((*compare-selections* t))
	 (compare* (args old) (args new)))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old projection-application) (new projection-application))
  (and (compare* (id old) (id new))
       (compare* (index old) (index new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old field-application) (new field-application))
  (and (compare* (id old) (id new))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old field-application) (new application))
  (and (typep (operator new) 'name-expr)
       (eq (id old) (id (operator new)))
       (compare* (argument old) (argument new))))

(defmethod compare* ((old application) (new application))
  (and (compare* (operator old) (operator new))
       (compare* (arguments old) (arguments new))))

(defmethod compare* ((old when-expr) (new application))
  (and (typep (argument new) 'arg-tuple-expr)
       (= (length (exprs (argument new))) 2)
       (compare* (reverse (exprs (argument old))) (exprs (argument new)))))

(defmethod compare* ((old table-expr) (new table-expr))
  (and (compare* (row-expr old) (row-expr new))
       (compare* (col-expr old) (col-expr new))
       (compare* (row-headings old) (row-headings new))
       (compare* (col-headings old) (col-headings new))
       (compare* (table-entries old) (table-entries new))))

(defmethod compare* ((old implicit-conversion) (new expr))
  (compare* (args1 old) new))

(defmethod compare* ((old argument-conversion) (new expr))
  (compare* (operator old) new))

(defmethod compare* ((old lambda-conversion) (new expr))
  (compare* (expression old) new))

(defmethod compare* ((old binding-expr) (new binding-expr))
  (and (same-binding-op? old new)
       (compare* (bindings old) (bindings new))
       (compare* (expression old) (expression new))))

;(defmethod compare* ((old quant-expr) (new quant-expr))
;  (call-next-method))

(defmethod compare* ((old update-expr) (new update-expr))
  (and (compare* (expression old) (expression new))
       (compare* (assignments old) (assignments new))))

(defmethod compare* ((old assignment) (new assignment))
  (and (compare* (arguments old) (arguments new))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old maplet) (new maplet))
  (and (compare* (arguments old) (arguments new))
       (compare* (expression old) (expression new))))

(defmethod compare* ((old assignment) (new maplet))
  nil)

(defmethod compare* ((old maplet) (new assignment))
  nil)


;;; Names and simple-decls

(defmethod compare* ((old simple-decl) (new simple-decl))
  (and (compare* (id old) (id new))
       (or (compare* (declared-type old) (declared-type new))
	   (and *compare-selections*
		(null (declared-type new))))))

;(defmethod compare* ((old modname) (new modname))
;  (and (call-next-method)
;       (compare (renamings old) (renamings new))))

(defmethod compare* ((old name) (new name))
  (and (compare* (mod-id old) (mod-id new))
       (compare* (library old) (library new))
       (and (or (and (null (actuals old)) (null (actuals new)))
		(and (actuals old) (actuals new)))
	    (compare* (actuals old) (actuals new)))
       (compare* (id old) (id new))))

(defmethod compare* ((old expr) (new expr))
  nil)

(defmethod compare* ((old type-expr) (new type-expr))
  nil)

(defmethod compare* ((old actual) (new actual))
  (and (or (and (zerop (parens (expr old)))
		(zerop (parens (expr new))))
	   (and (not (zerop (parens (expr old))))
		(not (zerop (parens (expr new))))))
       (compare* (expr old) (expr new))))

(defmethod compare* ((old list) (new list))
  (and (length= old new)
       (every #'(lambda (oe ne) (compare* oe ne))
	      old new)))


(defmethod compare-sig (old new)
  (unless (or (eq *decl-diffs* 'SIGNATURE)
	      (compare* old new))
    (setq *decl-diffs* 'SIGNATURE))
  t)

(defmethod compare-bod (old new)
  (unless (or *decl-diffs*
	      (compare* old new))
    (setq *decl-diffs* 'BODY))
  t)

#-gcl
(defmethod compare* ((old term::default-term) (new term::default-term))
  (equal (term-to-sexp old) (term-to-sexp new)))

(defmethod compare* (old new)
  (equal old new))

(defmethod compare* ((old syntax) (new syntax))
  (eq (class-of old) (class-of new)))

(defun add-decl-diffs (old new)
  (when *decl-diffs*
    (push (list old new
		(or (find 'signature *decl-diffs*)
		    (find 'body *decl-diffs*)))
	  *differences*)))
