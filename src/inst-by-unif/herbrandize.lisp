(in-package :pvs)

;; Herbrandize

(defun herbrandize (fmlas vars renamings)
  (herbrandize* fmlas vars renamings))

(defun skolemize (fmlas vars renamings)
  (skolemize* fmlas vars renamings))

(defmethod herbrandize* ((fmlas null) lvars renamings)
  (values nil lvars renamings))

(defmethod herbrandize* ((fmlas cons) lvars renamings)
  (multiple-value-bind (hfmla lvars1 renamings1)
      (herbrandize* (car fmlas) lvars renamings)
    (multiple-value-bind (hfmlas lvars2 renamings2)
	(herbrandize* (cdr fmlas) lvars1 renamings1)
      (values (cons hfmla hfmlas) lvars2 renamings2))))

(defmethod herbrandize* ((fmla negation) lvars renamings)
  (multiple-value-bind (new-fmla new-lvars new-renamings)
      (skolemize* (args1 fmla) lvars renamings)
    (values (lcopy-negation fmla new-fmla) new-lvars new-renamings)))

(defmethod herbrandize* ((fmla disjunction) lvars renamings)
  (multiple-value-bind (hfmla1 lvars1 renamings1)
      (herbrandize* (args1 fmla) lvars renamings)
    (multiple-value-bind (hfmla2 lvars2 renamings2)
	(herbrandize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-disjunction fmla hfmla1 hfmla2) lvars2 renamings2))))

(defmethod herbrandize* ((fmla conjunction) lvars renamings)
  (multiple-value-bind (hfmla1 lvars1 renamings1)
      (herbrandize* (args1 fmla) lvars renamings)
    (multiple-value-bind (hfmla2 lvars2 renamings2)
	(herbrandize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-conjunction fmla hfmla1 hfmla2) lvars2 renamings2))))

(defmethod herbrandize* ((fmla iff-or-boolean-equation) lvars renamings)
 (multiple-value-bind (hfmla1 lvars1 renamings1)
      (herbrandize* (args1 fmla) lvars renamings)
    (multiple-value-bind (hfmla2 lvars2 renamings2)
	(herbrandize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-iff fmla hfmla1 hfmla2) lvars2 renamings2))))

(defmethod herbrandize* ((fmla implication) lvars renamings)
 (multiple-value-bind (sfmla1 lvars1 renamings1)
      (skolemize* (args1 fmla) lvars renamings)
    (multiple-value-bind (hfmla2 lvars2 renamings2)
	(herbrandize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-implication fmla sfmla1 hfmla2) lvars2 renamings2))))

(defmethod herbrandize* ((fmla expr) lvars renamings)
  (values (substit fmla renamings) lvars renamings))

(defmethod herbrandize* ((fmla forall-expr) lvars renamings)
  (multiple-value-bind (bndngs body)
      (destructure-universal fmla)
    (if (safe-to-skolemize? bndngs)
	(herbrandize* body
		      lvars
		      (add-to-subst bndngs
			     (mk-new-skofuns bndngs lvars)
			     renamings))
      (values (substit fmla renamings) lvars renamings))))

(defmethod herbrandize* ((fmla exists-expr) lvars renamings)
  (multiple-value-bind (bndngs body)
      (destructure-existential fmla)
    (let ((new-lvars (mk-new-lvars bndngs)))
      (herbrandize* body
		    (union new-lvars lvars :test #'tc-eq)
		    (add-to-subst bndngs new-lvars renamings)))))

(defmethod skolemize* ((fmlas null) lvars renamings)
  (values nil lvars renamings))

(defmethod skolemize* ((fmlas cons) lvars renamings)
  (multiple-value-bind (sfmla lvars1 renamings1)
      (skolemize* (car fmlas) lvars renamings)
    (multiple-value-bind (sfmlas lvars2 renamings2)
	(skolemize* (cdr fmlas) lvars1 renamings1)
      (values (cons sfmla sfmlas) lvars2 renamings2))))
		
(defmethod skolemize* ((fmla expr) lvars renamings)
  (values (substit fmla renamings) lvars renamings))

(defmethod skolemize* ((fmla negation) lvars renamings)
  (multiple-value-bind (new-fmla new-lvars new-renamings)
      (herbrandize* (args1 fmla) lvars renamings)
    (values (lcopy-negation fmla new-fmla) new-lvars new-renamings)))

(defmethod skolemize* ((fmla disjunction) lvars renamings)
  (multiple-value-bind (sfmla1 lvars1 renamings1)
      (skolemize* (args1 fmla) lvars renamings)
    (multiple-value-bind (sfmla2 lvars2 renamings2)
	(skolemize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-disjunction fmla sfmla1 sfmla2) lvars2 renamings2))))

(defmethod skolemize* ((fmla conjunction) lvars renamings)
  (multiple-value-bind (sfmla1 lvars1 renamings1)
      (skolemize* (args1 fmla) lvars renamings)
    (multiple-value-bind (sfmla2 lvars2 renamings2)
	(skolemize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-conjunction fmla sfmla1 sfmla2) lvars2 renamings2))))

(defmethod skolemize* ((fmla iff-or-boolean-equation) lvars renamings)
  (multiple-value-bind (sfmla1 lvars1 renamings1)
      (skolemize* (args1 fmla) lvars renamings)
    (multiple-value-bind (sfmla2 lvars2 renamings2)
	(skolemize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-iff fmla sfmla1 sfmla2) lvars2 renamings2))))

(defmethod skolemize* ((fmla implication) lvars renamings)
  (multiple-value-bind (hfmla1 lvars1 renamings1)
      (herbrandize* (args1 fmla) lvars renamings)
    (multiple-value-bind (sfmla2 lvars2 renamings2)
	(skolemize* (args2 fmla) lvars1 renamings1)
      (values (lcopy-implication fmla hfmla1 sfmla2) lvars2 renamings2))))

(defmethod skolemize* ((fmla branch) lvars renamings)
  (multiple-value-bind (scond lvars1 renamings1)
      (skolemize* (condition fmla) lvars renamings)
    (multiple-value-bind (sthen lvars2 renamings2)
	(skolemize* (then-part fmla) lvars1 renamings1)
      (multiple-value-bind (hcond lvars3 renamings3)
	  (herbrandize* (condition fmla) lvars2 renamings2)
	(multiple-value-bind (selse lvars4 renamings4)
	    (skolemize (else-part fmla) lvars3 renamings3)
	  (values (make!-disjunction (make!-conjunction scond sthen)
				     (make!-conjunction hcond selse))
		  lvars4 renamings4))))))

(defmethod skolemize* ((fmla forall-expr) lvars renamings)
  (multiple-value-bind (bndngs body)
      (destructure-universal fmla)
    (let ((new-lvars (mk-new-lvars bndngs)))
      (skolemize* body
		  (union new-lvars lvars :test #'tc-eq)
		  (add-to-subst bndngs new-lvars renamings)))))
		      
(defmethod skolemize* ((fmla exists-expr) lvars renamings)
  (multiple-value-bind (bndngs body)
      (destructure-existential fmla)
    (let ((new-skofuns (mk-new-skofuns bndngs lvars)))
      (if (safe-to-skolemize? bndngs)
	  (skolemize* body
		      lvars
		      (add-to-subst bndngs new-skofuns renamings))
	  (values (substit fmla renamings) lvars renamings)))))

(defun safe-to-skolemize? (bndngs)
  (every #'(lambda (bndng)
		   (nonempty? (type bndng)))
	 bndngs))


;; Extend a substitution

(defmethod add-to-subst ((l1 cons) (l2 cons) subst)
  (add-to-subst (car l1) (car l2) (add-to-subst (cdr l1) (cdr l2) subst)))

(defmethod add-to-subst ((l1 null) (l2 null) subst)
  subst)

(defmethod add-to-subst ((x name-expr) (e expr) subst)
  (acons x e subst))
