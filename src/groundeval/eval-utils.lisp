(in-package 'pvs)

(defmacro load-file (str)
  `(compile-file ,str)
  `(load ,str))

(defun fully-instantiated? (obj)
  (let ((formals (when *current-theory*
		   (formals-sans-usings *current-theory*))))
    (every #'(lambda (fp) (memq fp formals))
	   (free-params obj))))

(defun get-branch-decl ()
  (let ((if-theory (get-theory "if_def")))
    (find 'IF (theory if-theory) :key #'id) ))

(defun make-branch-if (type)
  (let* ((actuals (list (mk-actual type)))
	 (res
	  (make-instance 'resolution
	    'declaration (get-branch-decl)
	    'module-instance (make-instance 'modname
			       'id '|if_def|
			       'actuals actuals)
	    'type (mk-funtype (list type) *boolean*))))
    (mk-name-expr 'if actuals '|if_def| res 'CONSTANT)))
    

(defun mk-branch-expr (cond then else  type)
  (mk-branch-expr* 'if-expr cond then else type))

(defun mk-chained-branch-expr (cond then else type)
  (mk-branch-expr* 'chained-if-expr cond then else type))

(defun mk-branch-expr* (class cond then else type)
  (let ((if-name (make-branch-if type)))
    (if (eq class 'if-expr)
	(make-instance 'if-expr
	  'operator if-name
	  'argument (make-instance 'arg-tuple-expr
		      'exprs (list cond then else)))
	(make-instance 'chained-if-expr
	  'operator if-name
	  'argument (make-instance 'arg-tuple-expr
		      'exprs (list cond then else))))))

(defun mk-translate-cases-to-if (cases-expr)
  (mk-translate-cases-to-if* (expression cases-expr)
			  (selections cases-expr)
			  (else-part cases-expr)))

(defun mk-translate-cases-to-if* (expr selections else-part &optional chained?)
  (cond ((and (null (cdr selections))
	      (null else-part))
	 (mk-subst-accessors-in-selection expr (car selections)))
	((null selections)
	 else-part)
	(t (let* ((sel (car selections))
		  (thinst (module-instance (find-supertype (type expr))))
		  (rec (subst-mod-params (recognizer (constructor sel))
					 thinst))
		  (cond (mk-application rec expr))
		  (then ;(subst-mod-params
			 (mk-subst-accessors-in-selection expr sel)
			 ;thinst)
		    )
		  (else (mk-translate-cases-to-if* expr (cdr selections)
						else-part t)))
	     (if chained?
		 (mk-chained-branch-expr cond then else (type expr))
		 (mk-branch-expr cond then else (type expr)))))))

(defun mk-subst-accessors-in-selection (expr sel)
  (let* ((thinst (module-instance (find-supertype (type expr))))
	 (accs (subst-mod-params (accessors (constructor sel)) thinst))
	 (vars (args sel))
	(selexpr (expression sel)))
    (substit selexpr
      (pairlis vars
	       (mapcar #'(lambda (acc) (mk-application acc expr))
		       accs)))))
