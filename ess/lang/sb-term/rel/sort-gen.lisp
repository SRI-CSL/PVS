;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)sort-gen.lisp	1.33 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; Ergo Syntax Box

;;; Scott Dietzen, Sat Jun 13 15:43:19 1987


(in-package :syntax-box)   (use-package :ergolisp)

(use-package '(:sb-runtime :sort :oper))



;;; Global Variables 

(defvar *suppress-sort-errors* nil)

(defvar *error-occurred-sort?* nil)

(defvar *local-error-occurred-sort?* nil)

(defvar *sort-table* nil)

(defvar *opsig-table* nil)

(defvar *union-to-sort-assoc* ())


(defvar *current-nt-name* nil
  "Name of the current nonterminal used in error reporting.")


(defvar *pattern-augment-checked* (make-hash-table :test #'eq))


(defvar *nt-opt-count* -1)
(defvar *nt-alt-count* -1)
(defvar *nt-list-count* -1)



(defun lang-intern (x)
  (intern (if (stringp x)
	      #+(and allegro (version>= 6)) (string-downcase x)
	      #-(and allegro (version>= 6)) (string-upcase x)
	      (symbol-name x))
	  (lang:lang-abs-syn-package *language*)))



;;; Control the processing of each nonterminal pattern. This is the main
;;; exported routine.

(defun sort-grammar (grammar &optional (suppress-errors nil))
  (setq *suppress-sort-errors* suppress-errors)
  (setq *sort-table* (sort:make-sort-table))
  (setq *opsig-table* (sort:make-opsig-table))
  (setq *error-occurred-sort?* nil)
  (setq *union-to-sort-assoc* ())
  (do ((nt-runner (grammar-nonterminal-definitions grammar)
		  (cdr nt-runner)))
      ((null nt-runner)
       (if *error-occurred-sort?*
	   'error-occurred))
    (setq *current-nt-name* (nt-name (car nt-runner)))
    (format t "   ~A ... ~%" *current-nt-name*)	; progress report
    (sort-nt (car nt-runner))))


(defun sort-nt (nt)
  (setq *local-error-occurred-sort?* nil)
  (setq *nt-opt-count* -1)
  (setq *nt-alt-count* -1)
  (setq *nt-list-count* -1)
  (clrhash *pattern-augment-checked*)
  (determine-pat-ttype (nt-pattern nt))
  (cond ((not *local-error-occurred-sort?*)
	 (assign-sorts-and-opsigs-pattern
	  (nt-pattern nt)
	  (mk-nt-sort)))))


(defun mk-nt-sort ()
  (sort:mk-sort-ttype (lang-intern *current-nt-name*)))





(defun determine-pat-ttype (pat &key (upat? nil))
  (if (not (pattern-leaf-p pat))
      (mapc #'(lambda (pat)
		(determine-pat-ttype pat :upat? upat?))
            (pattern-sons pat)))
  (set-pattern-ttype
   pat
   (cond ((get-pattern-ttype pat))	; bound by lower augment.
         ((memq (pattern-kind pat) '(plus doubleplus star doublestar))
          (sort:mk-list-ttype
           (get-pattern-ttype (pattern-son0 pat))))
         ((and (eq (pattern-kind pat) 'nonterminal)
               (is-lexical-terminal (pattern-leaf-ds pat) *grammar-term*))
          (sort:mk-sort-ttype (intern (symbol-name (pattern-leaf-ds pat))
				      (find-package :sb-runtime))))
         ((eq (pattern-kind pat) 'nonterminal)
          (sort:mk-sort-ttype (lang-intern (pattern-leaf-ds pat))))
         ((eq (pattern-kind pat) 'ext-nonterminal)
	  (multiple-value-bind (nt-name lang-name)
	      (pattern-leaf-ds pat)
	    (sort:mk-sort-ttype
	     (intern (symbol-name nt-name)
		     (get-lang-abs-syn-package-name lang-name)))))
         (t
          ())))
  (cond ((pattern-upats pat)		; New for upatterns
	 (map-upat-patterns #'(lambda (pat)
				(determine-pat-ttype pat :upat? t))
			    (pattern-upats pat))))
  (cond ((get-pattern-augment pat)
	 (determine-aug-ttype (get-pattern-augment pat)
			      :upat? upat?))))




(defun determine-aug-ttype (aug &key (upat? nil))
  (if (not (augment-leaf-p aug))
      (mapc #'(lambda (aug)
		(determine-aug-ttype aug :upat? upat?))
            (augment-args aug)))
  (set-augment-ttype
   aug 
   (ecase (augment-kind aug)
     (string
      (sort:mk-sort-ttype 'sb-runtime::string))
     (literal
      (sort:mk-sort-ttype 'sb-runtime::id))
     (number
      (sort:mk-sort-ttype 'sb-runtime::number))
     (id
      (sort:mk-sort-ttype 'sb-runtime::id))

     ((name ext-name)
      (if (augment-path aug)
	  (find-ttype (car (augment-path aug)))
	  (if upat?
	      nil			; OK
	      (error "Internal Error: ~%~
                 Augment has no path to reference: ~S~%" aug))))

     ((alt opt)
      (ck-no-list-or-null-ttypes (augment-args aug))
      (sort:mk-union-ttype (mapcar #'(lambda (x)
				       (get-augment-ttype x))
				   (augment-args aug))))

     (null
      (sort:mk-null-ttype))

     (term-const
      (let* ((op (augment-leaf-ds (augment-arg0 aug)))
	     (op (oper:mk-sim-op
		  (if (symbolp op)
		      (lang-intern op)
		      op)))		; string.
	     (op-ttype
	      (sort:mk-op-ttype op)))
	op-ttype))

     ((bind internal-bind)
      (cond ((pattern-p (car (augment-path aug)))
	     (set-pattern-ttype
	      (car (augment-path aug))
	      (get-augment-ttype (car (augment-args aug)))))
	    (t
	     ;; No need to set here, we will set the path when we reach the
	     ;; name which references it. 
	     (get-augment-ttype (car (augment-args aug))))))

     ((star plus)
      (if (not upat?)
	  (error "Internal Error:  There should not be any plus's or star's ~%~
	          in the augments at this point. ~%~S~%"
		 aug)))
     

     (list
      (cond ((augment-args aug)
	     (ck-no-list-or-null-ttypes (augment-args aug))
	     (sort:mk-list-ttype
	      (sort:mk-union-ttype
	       (mapcar #'(lambda (x)
			   (get-augment-ttype x))
		       (augment-args aug)))))
	    (t
	     (sort:mk-elist-ttype))))

     (cons
      (let ((son0-ttype (get-augment-ttype (augment-arg0 aug)))
	    (son1-ttype (get-augment-ttype (augment-arg1 aug))))
	(if (not (sort:is-list-ttype son1-ttype))
	    (sort-gen-error
	     (format nil
		 "The second argument of cons is not of list type : ~%~S~%"
	       (augment-arg1 aug))))
	(sort:mk-list-ttype 
	 (sort:mk-union-ttype
	  (list son0-ttype
		(sort:ds-list-ttype son1-ttype))))))


     (bcons
      (let ((son0-ttype (get-augment-ttype (augment-arg0 aug)))
	    (son1-ttype (get-augment-ttype (augment-arg1 aug))))
	(if (not (sort:is-list-ttype son0-ttype))
	    (sort-gen-error
	     (format nil
		 "The first argument of bcons is not of list type : ~%~S~%"
	       (augment-arg0 aug))))
	(sort:mk-list-ttype
	 (sort:mk-union-ttype
	  (list (sort:ds-list-ttype son0-ttype)
		son1-ttype)))))

     (append
      (let ((son0-ttype (get-augment-ttype (augment-arg0 aug)))
	    (son1-ttype (get-augment-ttype (augment-arg1 aug))))
	(if (not (sort:is-list-ttype son0-ttype))
	    (sort-gen-error
	     (format nil
		 "The first argument of append is not of list type : ~%~S~%"
	       (augment-arg0 aug))))
	(if (not (sort:is-list-ttype son1-ttype))
	    (sort-gen-error
	     (format nil
		 "The second argument of append is not of list type : ~%~S~%"
	       (augment-arg1 aug))))
	(sort:mk-list-ttype 
	 (sort:mk-union-ttype
	  (list (sort:ds-list-ttype son0-ttype)
		(sort:ds-list-ttype son1-ttype))))))


     ((and)
      (get-augment-ttype (car (last (augment-args aug))))))))




(defun find-ttype (x)
  (cond ((augment-p x)
         (get-augment-ttype x))
        ((pattern-p x)
         (get-pattern-ttype x))
        (t
         (error
	  "Internal Error: ~%~
          The following object is neither a pattern or an augment:~%~S~%"
	  x))))



(defun ck-no-list-or-null-ttypes (aug-list)
  (do ((tt-runner (mapcar #'(lambda (x)
			      (get-augment-ttype x))
			  aug-list)
		  (cdr tt-runner)))
      ((or (null tt-runner)
	   (sort:is-list-ttype (car tt-runner))
	   (sort:is-null-ttype (car tt-runner))
	   (sort:is-elist-ttype (car tt-runner)))
       (if tt-runner
	   (sort-gen-error
	    (format nil
		"One of these augment sons is a list or null without an ~
                 operator:~%~S~%"
	      aug-list))))))



(defun lookup-union-to-sort (union-ttype)
  (cdr (assoc union-ttype *union-to-sort-assoc* :test #'sort:ttype-equal)))

(defun insert-union-to-sort (union-ttype sort)
  (cond ((lookup-union-to-sort union-ttype))
	(t
	 (setq *union-to-sort-assoc*
	       (acons union-ttype sort *union-to-sort-assoc*)))))




(defun introduce-sort (aug def-ttype)
  (cond ((lookup-union-to-sort def-ttype))
	((sort:is-op-ttype def-ttype) ; singular
	 (let ((opsort
		(sort:mk-sort-ttype (oper:ds-sim-op
				     (sort:ds-op-ttype def-ttype)))))
	   (insert-union-to-sort def-ttype opsort)
	   opsort))
	(t
	 (let ((new-sort
		(mk-sort-ttype
		 (lang-intern
		  (concatenate 'string
		    (symbol-name *current-nt-name*)
		    "-"
		    (ecase (augment-kind aug)
		      ((opt alt)
		       (symbol-name (augment-kind aug)))
		      ((cons bcons list append)
		       "LIST"))
		    "-"
		    (ecase (augment-kind aug)
		      (opt
		       (princ-to-string (incf *nt-opt-count*)))
		      (alt
		       (princ-to-string (incf *nt-alt-count*)))
		      ((cons bcons list append)
		       (princ-to-string (incf *nt-list-count*)))))))))
	   (insert-union-to-sort def-ttype new-sort)
	   new-sort))))





(defun assign-sorts-and-opsigs-pattern (pat result-sort)
  (cond ((and (get-pattern-binding-aug pat)
	      (not (gethash pat *pattern-augment-checked*)))
	 (setf (gethash pat *pattern-augment-checked*) t)
					; The above hack eliminates infinite
					; recursion from cirularity in the
					; grammars (An augment may bind to and
					; reference the pattern it is
					; associated with.).
	 (assign-sorts-and-opsigs-augment (get-pattern-binding-aug pat)
					  result-sort)
	 (setf (gethash pat *pattern-augment-checked*) nil))
					; (see comment above).  Here we want to
					; reset this so that later ferences to
					; the same pattern will follow the same
					; path. 
	(t
	 (ecase (pattern-kind pat)
	   ((ext-nonterminal nonterminal)
	    (cond ((and result-sort
			(is-sort-ttype (get-pattern-ttype pat))
			(not (sort:ttype-equal result-sort
					       (get-pattern-ttype pat))))
		   (sort-gen-error
		    (format nil
		      "Attempting to include a nonterminal or lexical terminal ~
                       in another sort.~%~
	               NT:~S~%Sort:~S~%"
		      pat result-sort)
		    nil)
		   (if (sort:is-sort-ttype result-sort)
		       (sort:add-ttype-to-sort (get-pattern-ttype pat)
					       result-sort *sort-table*)))))

	   ((plus star doubleplus doublestar)
	    (cond ((and (is-sort-ttype result-sort)
			(not (sort:ttype-equal result-sort
					       (get-pattern-ttype pat))))
		   (sort:add-ttype-to-sort (get-pattern-ttype pat)
					   result-sort
					   *sort-table*))
		  ((and result-sort
			(not (sort:ttype-equal result-sort
					       (get-pattern-ttype pat))))
		   (sort-gen-error
		    (format nil
		      "Attempting to include a list pattern directly within a ~
                       sort.~%NT:~S~%Sort:~S~%"
		      pat result-sort)
		    t)))
	    (assign-sorts-and-opsigs-pattern (pattern-son0 pat)
					     result-sort))

	   ((ukeyword alt opt jux seq)
	    (sort-gen-error
	     (format nil
		 "Attempting to reference pattern with no associated augment:~%~
                  ~S" pat))))))

; (cond ((pattern-upats pat)		; New for upatterns.
;	 (map-upat-patterns #'(lambda (pat)
;				(assign-sorts-and-opsigs-pattern
;				 pat result-sort))
;			    (pattern-upats pat))))
  )




(defun assign-sorts-and-opsigs-augment (aug result-sort)
  (ecase (augment-kind aug)

    ((term-const)
     (assign-sorts-and-opsigs-term-const aug result-sort))

    ((string number literal id null)
     (cond ((and result-sort
		 (not (sort:ttype-equal result-sort
					(get-augment-ttype aug))))
	    (sort-gen-error
	     (format nil
		 "Attempting to assign a sort to a abstract syntax leaf value.~%~
	          Leaf:~S~%Sort:~S~%"
	          aug result-sort)
	     nil)
	    (if (sort:is-sort-ttype result-sort)
		(sort:add-ttype-to-sort (get-augment-ttype aug)
					result-sort *sort-table*)))))
    
    ((bind internal-bind)
     (assign-sorts-and-opsigs-augment (augment-arg0 aug)
				      result-sort))

    ((name ext-name)
     (cond ((augment-p (car (augment-path aug)))
	    (assign-sorts-and-opsigs-augment (car (augment-path aug))
					     result-sort))
	   ((pattern-p (car (augment-path aug)))
	    (assign-sorts-and-opsigs-pattern (car (augment-path aug))
					     result-sort))
	   (t
	    (error "Internal error: ~% The object in question is neither ~
                    a pattern nor an augment:~%~S~%" 
		   (car (augment-path aug))))))

    ((alt opt)
     (let ((new-result-sort 
	    (cond ((is-sort-ttype result-sort)
		   result-sort)
		  ((sort:is-union-ttype (get-augment-ttype aug))
		   (introduce-sort aug (get-augment-ttype aug)))
		  (t
		   (get-augment-ttype aug)))))
					; No sort needed in last case. 
       (mapc #'(lambda (a)
		 (assign-sorts-and-opsigs-augment
		  a
		  new-result-sort))
	     (augment-args aug))))

    ((list cons bcons append)
     (mapc #'(lambda (a)
	       (assign-sorts-and-opsigs-augment
		a
		result-sort))
	   (augment-args aug)))

    (and
     (mapc #'(lambda (a)
	       (assign-sorts-and-opsigs-augment a nil))
	   (butlast (augment-args aug)))
     (assign-sorts-and-opsigs-augment (car (last (augment-args aug)))
				      result-sort))

    ((star plus)
     (error "Internal Error:  There should not be any plus's or star's ~%~
	     in the augments at this point. ~%~S~%"
	    aug))))
  



(defun assign-sorts-and-opsigs-term-const (aug result-sort)
  (let* ((op (augment-leaf-ds (augment-arg0 aug)))
	 (op (oper:mk-sim-op
	      (if (symbolp op)
		  (lang-intern op)
		  op)))		; string.
	 (op-ttype (sort:mk-op-ttype op))
	 (new-result-sort (cond ((is-sort-ttype result-sort)
				 result-sort)
				(t
				 (introduce-sort aug op-ttype)))))

    (cond ((has-null-arg? aug)
	   (let ((opsig
		  (sort:mk-opsig (lang:lang-name *language*)
				 (list (sort:mk-null-ttype))
				 new-result-sort)))
	     (sort:opsig-table-insert op opsig
				      *opsig-table*
				      *suppress-sort-errors*)))

	  ((has-elist-arg? aug)
	   (let ((opsig
		  (sort:mk-opsig (lang:lang-name *language*)
				 (list (mk-elist-ttype))
				 new-result-sort))
		 (old-opsig (opsig-table-lookup op *opsig-table*)))
	     (if (not (and old-opsig
			   (is-list-ttype (car (opsig-inputs old-opsig)))))
		 (sort:opsig-table-insert op opsig
					  *opsig-table*
					  *suppress-sort-errors*))))


	  ((has-list-arg? aug)
	   (let* ((son-sort
		   (if (sort:is-union-ttype (get-augment-ttype
					     (augment-arg1 aug)))
		       (introduce-sort (augment-arg1 aug)
				       (get-augment-ttype
					(augment-arg1 aug)))
		       (get-augment-ttype (augment-arg1 aug))))
					; No sort needed in last case. 
		  (opsig (sort:mk-opsig
			  (lang:lang-name *language*)
			  (list son-sort)
			  new-result-sort))
		  (old-opsig (opsig-table-lookup op *opsig-table*)))
	     (if (and old-opsig
		      (is-elist-ttype (car (opsig-inputs old-opsig))))
		 (sort:opsig-table-insert op opsig *opsig-table* :no-warnings)
		 (sort:opsig-table-insert op opsig
					  *opsig-table* *suppress-sort-errors*))
	     (assign-sorts-and-opsigs-augment (augment-arg1 aug)
					      (if (is-sort-ttype son-sort)
						  son-sort))))

	  (t
	   (ck-no-list-or-null-ttypes (cdr (augment-args aug)))
	   (mapc #'(lambda (a)
		     (assign-sorts-and-opsigs-augment a nil))
		 (cdr (augment-args aug)))
	   (let ((opsig (sort:mk-opsig
			 (lang:lang-name *language*)
			 (mapcar #'find-augment-ttype
				 (cdr (augment-args aug)))
			 new-result-sort)))
	     (sort:opsig-table-insert op opsig
				      *opsig-table* *suppress-sort-errors*))))

    (sort:add-ttype-to-sort op-ttype 
			    new-result-sort
			    *sort-table*)))





(defun has-elist-arg? (aug)
  (and (= (term-arity aug) 2)
       (sort:is-elist-ttype
	(get-augment-ttype (augment-arg1 aug)))
       (not (eq 'term-const (augment-kind (augment-arg1 aug))))))

(defun has-list-arg? (aug)
  (and (= (term-arity aug) 2)
       (sort:is-list-ttype
	(get-augment-ttype (augment-arg1 aug)))
       (not (eq 'term-const (augment-kind (augment-arg1 aug))))))

(defun has-null-arg? (aug)
  (and (= (term-arity aug) 2)
       (sort:is-null-ttype
	(get-augment-ttype (augment-arg1 aug)))
       (not (eq 'term-const (augment-kind (augment-arg1 aug))))))




(defun find-augment-ttype (aug)
  (let ((aug-ttype (get-augment-ttype aug)))
    (cond ((lookup-union-to-sort aug-ttype))
	  (t
	   aug-ttype))))

     

(defun construct-sort-table-function ()
  `(defparameter ,(lang-sort-table-name *language*)
     (sort:make-sort-table ',(sort:sort-table-contents *sort-table*))))


(defun construct-opsig-table-function ()
  `(defparameter ,(lang-opsig-table-name *language*)  
     (sort:make-opsig-table ',(sort:opsig-table-contents *opsig-table*))))





;;; Error Reporting

(defun sort-gen-error (error-string &optional (error? t))
  (if (not *local-error-occurred-sort?*)
      (setq *local-error-occurred-sort?* error?))
  (if (not *error-occurred-sort?*)
      (setq *error-occurred-sort?* error?))
  (cond ((not *suppress-sort-errors*)
	 (if error?
	     (format t
		 "~%  Error in nonterminal definition: ~A~%"
	       *current-nt-name*)
	     (format t
		 "~%  Warning in nonterminal definition: ~A~%"
	       *current-nt-name*))
	 (format t
	     error-string))))

