;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)inter-phase.lisp	1.10 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Scott Dietzen
;;; Revised, Scott Dietzen, Mon Oct 13 16:05:43 1986

;;; Intermediate Phase.

(in-package 'syntax-box)  (use-package :ergolisp)
(use-package '(sb-runtime))

;;; Understanding of the internal grammar term structure is essential to
;;; understanding this code (see documentation in access.lisp). @@@

;;; @@@ Document use of set-pattern-augment, get-pattern-augment.

;;;   The intermediate pass performs several functions on a nonterminal's
;;; pattern and its associated abstract syntax constructors: 
;;;
;;; PASS I -- BIND-AUGMENT's:
;;;         The abstract syntax contructors (augment's) are implicitly bound 
;;;     to the patterns at which they are defined.  This bind is made 
;;;     explicit to avoid confusion with relocated augment's (see RELOCATION).
;;;     This is also a good time to do some other random checking. 
;;;
;;; RESOLUTION: 
;;;	    RESOLVE all name occurances, that is match each name to the 
;;;     unique subpattern or augment result to which it refers.
;;;	Any errors which arise should be reported to the user
;;;     and the remainder of the intermediate phase may be aborted.  
;;;
;;; REFERENCES:
;;;         Analyze augment REFERENCES to patterns to make sure they are valid.
;;;     This may involve generating internal augment's to handel alts, opts,
;;;     stars and sequences of length 1.  When internal augment's are
;;;     generated, it is necessary to check discrimination information.  Again,
;;;     errors are reported and the remaining code aborted. (such poetry!)
;;;
;;; CHECK REPETITIVE PATTERNS:
;;;  	    @@@ <fill in>
;;;
;;; RELOCATION:
;;;         RELOCATE the augment's to the best location for their execution
;;;     during parsing.  
;;;
;;; ALLOCATION:
;;;	    ALLOCATE slots (array positions) for each piece 
;;;     of concrete and abstract syntax which must be generated
;;;	and store the appropriate indices in the pattern or augment's.
;;;

  
;;;   Each of the above functions is given a module within the 
;;; intermediate phase.





;;; Global Variables


(defvar *error-occurred-inter?* nil)     

;;; This is a global flag utilized to avoid unnessary work after an error was
;;; found during name resolution.  It is reset in the function
;;; crunch-pattern.


(defvar *local-error-occurred-inter?* nil)

;;; This is a local flag used to halt extra checking after an error has been
;;; found locally, that is within the current pattern.  Hopefully this will
;;; avoid duplication of error messages.


(defvar *max-slot-count* 0)     

;;; During slot allocation *max-slot-count* maintains the maximum slot index
;;; allocated so far.  This value is returned to specify the space required for
;;; parsing.  It is reset in the
;;; do-slot-allocation.


(defvar *current-nt-name* nil)

;;; Name of the current nonterminal used in error reporting.

(defvar *nt-opt-default-op-count* 0)

;;; So we can generate names for null optional operators we must introduce. 

(defvar *top-level-alts* ())

;;; @Hack!  For the parser generator Roger's required the assumption that the
;;; first slot always contain the result of parsing the nonterminal.  This is
;;; different from our normal convention in the case of alternations where
;;; usually the first son is index of the branch.   To get around this, a
;;; specific bind is added to store the result of a top-level alt in the 0
;;; slot.  Unfortunately certain references must know when they are dealing
;;; with a top-level alt, so they are added to the above list.  Granted, this
;;; is an ugly fix.  But I believe the elegant way to do it is to relax Anne's
;;; restriction concerning the 0th slot, but I'm not willing to make these
;;; modifications to her code.  By the way, the reason the same problem does
;;; not come up for opt, star, and doublestar, is that none of these may occur
;;; at top level (no nullable nonterminals). 



;;;  Exported Routines



;;; Control the processing of each nonterminal pattern. This is the main
;;; exported routine.

(defun crunch-grammar (grammar)
  (setq *error-occurred-inter?* nil)
  (setq *top-level-alts* nil)
  (do ((nt-runner (grammar-nonterminal-definitions grammar)
		  (cdr nt-runner)))
      ((null nt-runner)
       (if *error-occurred-inter?*
	   'error-occurred))
    (setq *current-nt-name* (nt-name (car nt-runner)))
    (format t "   ~A ... ~%" *current-nt-name*) ; progress report
    (crunch-nt (car nt-runner))))



;;; Applies the intermediate phase code to each nonterminal's pattern.  The
;;; number of slots required for parsing is associated with the nt-information
;;; record.

(defun crunch-nt (nt)
  (setq *local-error-occurred-inter?* nil)
  (setq *nt-opt-default-op-count* 0)
  (crunch-pattern (nt-pattern nt))
  (set-nt-slot-total nt *max-slot-count*)
  (cond (*local-error-occurred-inter?* 'error-occurred)))



;;; This function controls the application of the  separate phases (described
;;; in the initial comments) to the nonterminal pattern.  () is returned when
;;; errors occur.

(defun crunch-pattern (nt-pat)
  (if (eq (pattern-kind nt-pat)
	  'alt)
      (setq *top-level-alts*
	    (cons nt-pat *top-level-alts*))) ;@hack, see above
  (set-pattern-referenced nt-pat t)
  ;; The top level pattern is implicitly refenced: it is the result of the
  ;; nonterminal parse.

  (block inter-error-exit-block         ; Forgive me father, for I have
                                        ; sinned...
    (bind-augment nt-pat)
    (resolve-pattern nt-pat)
    (when *local-error-occurred-inter?* (return-from inter-error-exit-block))
    (check-pattern-references nt-pat)
    (when *local-error-occurred-inter?* (return-from inter-error-exit-block))
    (relocate-augment-of-pattern nt-pat)
    (when *local-error-occurred-inter?* (return-from inter-error-exit-block))
    (check-plus-star-augment-of-pattern nt-pat)
    (when *local-error-occurred-inter?* (return-from inter-error-exit-block))
    (do-slot-allocation nt-pat)))



;;; Pass 1 -- Bind Augment's Module


;;; When a pattern has an associated augment, it is our convention that naming
;;; that piece of concrete syntax yields the abstract syntax built by the
;;; augment.  Since in the relocation module we will be moving augment's, we
;;; must be careful not to bind augments to each peice of concrete syntax where
;;; has one.  This is accomplished by explicitly binding the augment's to the
;;; name associated with the pattern at which they are initially defined. (That
;;; is, we are making our implicit binding conventions, explicit here rather
;;; than hiding them in later code.)  Augments are then copied and made
;;; attributes of the associated patterns -- This way we may SIDE EFFECT them
;;; without messing with the original tree. 

(defun bind-augment (pat)
  (assert-sb (pattern-p pat))

					; $upattern revision 
  (cond ((pattern-upats pat)
	 (set-augment-of-upats pat)
	 (map-upat-patterns #'bind-augment
			    (pattern-upats pat))))

  (cond ((pattern-augment pat)	
	 (let ((new-aug
		(make-augment
		 :kind 'bind
		 :name (make-bind-name pat)
					; just for clarity.
		 :path (list pat) 
					; Path is set to point directly to the
					; name it references (see slot
					; allocation).
		 :args
		 (list (copy-augment
			(pattern-augment pat))))))

	   (set-pattern-augment pat new-aug)

	   (cond ((not (and (is-pat-iterator pat)
			    (is-aug-iterator (pattern-augment pat))))
		  ;; There is an implicit bind between the plus
		  ;; and star patterns and augment's.  It is handled
		  ;; in the referrences module.
		  (set-pattern-binding-aug pat new-aug)
		  (set-pattern-bound pat t))))))
    
  (if (not (pattern-leaf-p pat))
      (mapc (function bind-augment)
	    (pattern-sons pat))))




;;; Resolution Module

;;; It is necessary to check the augment's (abstract syntax constructors) to
;;; make sure all references to concrete syntax or abstract syntax have an
;;; unambiguous object.  It simplifies the relocation and slot allocation
;;; modules to store a path of the tree nodes (perhaps containing both pattern
;;; and augment nodes) visited while going from the pattern node associated
;;; with the current augment to the definition of the name in question.  Hence
;;; with each augment which references something, the entire path is stored in
;;; the path field.  These paths are kept in reverse order to simplify
;;; operations. Note --  The addition of these paths make our data structure
;;; (with attributes) circular.







;;; Resolve-pattern simply controls the application of resolve-augment. 

(defun resolve-pattern (pat &key (upattern? nil))
  (assert-sb (pattern-p pat))
  (if (not (pattern-leaf-p pat))
      (mapc #'(lambda (pat)
		(resolve-pattern pat :upattern? upattern?))
	    (pattern-sons pat)))
  (cond ((get-pattern-augment pat)
	 (resolve-augment (get-pattern-augment pat)
			  (list (list pat))
			  :upattern? upattern?)))

					; $upattern revision 
  (if (pattern-upats pat)
      (map-upat-patterns #'(lambda (pat)
			     (resolve-pattern pat :upattern? t))
			 (pattern-upats pat))))




;;; Resolution Module

;;; Compare-paths is a utility routine used to compare two possible paths to
;;; the object referenced by an augment.  If both are valid paths, then we have
;;; an ambiguity.  In certain instances (see below) the paths may be the same,
;;; in which case no error should be reported.

(defun compare-paths (path1 path2)
  (cond ((and path1
	      path2
	      (listp path1)
	      (listp path2)
	      (eq (car path1)
		  (car path2)))		; paths are identical because they
					; reach the same object.
	 path1)
	((and path1 path2)
	 'ambiguous)
	((or path1 path2))))




;;; Resolution Module

;;; Resolve-augment takes a list of all the paths which are available to be
;;; searched, and recursively resolves the abstract syntax for a particular
;;; pattern node.  It side effects the associated path attribute of the current
;;; peice of abstract syntax (actually storing the entire path to the reference
;;; in that location).  This information is required in the relocation module.
;;; Path-list contains the list of paths which we may search further down to
;;; locate the names references in the current abstract syntax augment.

(defun resolve-augment (aug path-list &key (upattern? nil))
  (assert-sb (augment-p aug))
  ;; The following group must have associated syntax.
  (cond ((augment-base-p aug))  ; nothing to do.
	((memq (augment-kind aug)
	       '(plus star alt opt name ext-name))
	 (let ((path (search-pattern-list aug path-list)))
	   (cond ((or (null path)
		      (eq path 'ambiguous))
		   (if (eq path 'ambiguous)
		       (inter-phase-error 
			(format nil
			    "Augment name reference ambiguous: ~%~S~%"
			  aug)
			t)
		       (if (not upattern?)
			   (inter-phase-error
			    (format nil
				"Augment name reference not found: ~%~S~%"
			      aug)
			    t))))
		 (t
		  (case (augment-kind aug)
		    ((plus star)
		     ;; The recursive call only allows searching below the
		     ;; pattern associated with the plus, star, etc.
		     ;; because no reference may cross this barrier.
		     (mapc
		      #'(lambda (abs)
			  (resolve-augment
			   abs
			   `((,(pattern-son0 (car path)) ,(car path)))
			   :upattern? upattern?))
		      (augment-args aug)))

		    (opt
		     ;; Search below pattern opt for second son of augment opt.
		     (resolve-augment (augment-arg0 aug)
				      path-list
				      :upattern? upattern?)
		     (resolve-augment (augment-arg1 aug)
				      `((,(pattern-son0 (car path)) ,@path)
					,@path-list)
				      :upattern? upattern?))
		    (alt
		     ;; Search pattern alt son for corresponding son of augment
		     ;; alt.
		     (mapc
		      #'(lambda (abs alt-pat-son)
			  (resolve-augment
			   abs
			   `((,alt-pat-son ,@path)
			     ,@path-list)
			   :upattern? upattern?))
		      (augment-args aug)
		      (pattern-sons (car path))))

		    ((name ext-name)))
		  ;; Note that no recursive call for name is necessary
		  ;; because it is a leaf of the abstract syntax tree.
		  (set-augment-path aug path)))))
	;; no associated syntax, there is nothing below augment to search for.
	(t
	 (mapc #'(lambda (abs)
		   (resolve-augment abs path-list :upattern? upattern?))
	       (augment-args aug)))))



;;; Resolution Module

;;; Search-pattern-list simply controls the application of search-pattern to
;;; each of the paths to be searched.

(defun search-pattern-list (aug path-list)
  (do ((paths path-list (cdr paths))
       (search-result nil
		      (compare-paths (search-pattern aug (car paths) t)
				     search-result)))
      ((or (eq search-result 'ambiguous)
	   (null paths))
       search-result)))



(defun search-pattern (aug path is-top-level-pattern?)
  (let ((pat (car path))
	(check-current-pattern nil)
	(check-below nil)
	(search-augment nil))
    (cond ((augment-matches-pattern? pat aug)
	   (setq check-current-pattern path)))
					; The above contains the path to the
					; current pattern if it matches.
    
    (cond     
     ((pattern-leaf-p pat))		; bottom of recursion. do nothing.
     ((not (memq (pattern-kind pat)
		 '(opt alt plus doubleplus star doublestar)))
      ;; We may not search below these special patterns.
      (setq 
       check-below
       (do ((son-list (pattern-sons pat)
		      (cdr son-list))
	    (search-result nil
			   (compare-paths (search-pattern aug
							  (cons (car son-list)
								path)
							  ())
					  search-result)))
	   ((or (eq search-result 'ambiguous)
		(null son-list))
	    search-result)))))
    
    ;; We now must search the abstract syntax for the name we need.
    (cond ((and (not is-top-level-pattern?)
		(eq 'name (augment-kind aug))
		(get-pattern-augment pat))		       
	   (let ((search-result
		  (name-search-augment (augment-leaf-ds aug)
				       (get-pattern-augment pat)
				       pat)))
	     (setq search-augment
		   (cond ((eq search-result 'ambiguous) 'ambiguous)
			 (search-result (append search-result path)))))))
    		
		  
    (compare-paths check-current-pattern
		   (compare-paths check-below
				  search-augment))))



;;; Resolution Module

;;; Augment-matches-pattern? is a side effecting boolean routine.  It yields
;;; true if the given augment references the given pattern.  The side effects
;;; allow checking in the next module to be sure patterns referenced have
;;; abstract syntax bound to them and all abstract syntax which is built is
;;; utilized.

(defun augment-matches-pattern? (pat aug)
  (let ((pat-name (pattern-name pat)))

    (or (cond ((and (or (eq (augment-kind aug) (pattern-kind pat))
			(and (eq (pattern-kind pat) 'doubleplus)
			     (eq (augment-kind aug) 'plus))
			(and (eq (pattern-kind pat) 'doublestar)
			     (eq (augment-kind aug) 'star)))
		    (eq (augment-name aug) pat-name))
					; By definition repetitive augment's
					; bind to their associated patterns.
					; The following checks for multiple
					; references to the son of a repetitive
					; pattern as in
					; { A+ ['a'] }  < [ {mk-x(A)}+  |
					;                   {mk-y(A)}+ ] > 
					; This is an error, which should be
					; built?
	       (cond ((is-pat-iterator pat)
		      (cond ((get-pattern-bound pat)
			     (if (not (is-nested-in?
				       aug
				       (get-pattern-augment pat)))
				    ;; @@HACK, this should really check if it
				    ;; is only nested in once, so that an error
				    ;; IS produced for above.  The
				    ;; is-nested-in? call is to avoid errors
				    ;; for E+ <s(E+)>.
				 (inter-phase-error
				  (format nil
				    "There may not be more than one reference ~
                                     to the argument of a repetitive~%~
  				     (plus, star, etc.) pattern:~%~S~%"
				    pat)
				  t))
			     (set-pattern-referenced pat t)))))
					; aug does match
	       T))			

	(cond ((or
		(and (eq (augment-kind aug) 'name)
		     (eq (augment-name aug) pat-name)
		     (or (and (eq (pattern-kind pat) 'nonterminal)
			      (eq (augment-leaf-ds aug) ; e.g.{ E ';'}<E>
				  (pattern-leaf-ds pat)))
			 (and (memq (pattern-kind pat) ; e.g. {[ A ] ';'} <opt>
				    '(plus doubleplus star doublestar
					   opt alt seq jux))
			      (or (eq (pattern-kind pat)
				      (augment-leaf-ds aug))))))

		(and (eq (augment-kind aug) 'ext-name) ; e.g. { E@ada ';'}
					;         <E@ada>
		     (eq (pattern-kind pat) 'ext-nonterminal)
		     (eq (augment-name aug) pat-name)
		     (multiple-value-bind (p1 p2)
			 (pattern-leaf-ds pat)
		       (multiple-value-bind (a1 a2)
			   (augment-leaf-ds aug)
			 (and (eq p1 a1)
			      (eq p2 a2))))))

	       (cond ((is-nested-in? aug (get-pattern-augment pat))
		      (set-pattern-referenced pat t)
		      (set-pattern-bound pat nil))
					; In this case the pattern is not
					; really defined by the augment since
					; the augment uses the pattern in it's
					; definition.
		     (t
		      (set-pattern-referenced pat t)))
	       T)))))
					; augment does match!







;;;  Resolution Module


;;;     Utility routine.  Is aug1 a descendant of aug2?


(defun is-nested-in? (aug1 aug2)
  (cond ((not (augment-p aug2))
	 ())
	((eq aug1 aug2)
	 t)
	((augment-leaf-p aug2)
	 ())
	(t
	 (do ((aug2-args (augment-args aug2) (cdr aug2-args)))
	     ((or (null aug2-args)
		  (is-nested-in? aug1 (car aug2-args)))
	      (cond ((null aug2-args) ())
		    (t)))))))


;;; Utility

(defun make-bind-name (pat)
  (sb-intern-upcase
   (concatenate 'string
     (symbol-name (pattern-kind pat))
     "_"
     (cond ((and (pattern-name pat)
		 (symbolp (pattern-name pat)))
	    (symbol-name (pattern-name pat)))
	   ((numberp (pattern-name pat))
	    (princ-to-string (pattern-name pat)))
	   (t
	    "")))))




;;; Resolution Module


;;; The following routine recursively searches abstract syntax looking for a
;;; bind to the name in question.  If multiple such names are found an ambigous
;;; result is returned.

(defun name-search-augment (name aug pat)
  (assert-sb (augment-p aug))
  (if (not (augment-base-p aug))
      (let ((check-current
	     (cond ((and (eq 'bind (augment-kind aug))
			 (eq name (augment-name aug)))
		    (cond ((and (eq pat
				    (car (augment-path
					  (get-pattern-augment pat))))
				(eq aug
				    (car (augment-args
					  (get-pattern-augment pat)))))
					; result of pat is referenced by bind
					; to augment name and reference to that
					; name.
			   (set-pattern-referenced pat t)))
		    (list aug))))
	    (check-sons
	     (cond ((augment-leaf-p aug)
		    ())			; bottom of recursion, do nothing. 
		   ((not (memq (augment-kind aug)
			       '(star plus opt alt)))
		    (do ((args (augment-args aug)
			       (cdr args))
			 (search-result 
			  nil
			  (let ((search
				 (name-search-augment name (car args) pat)))
			    (cond ((and search-result search)
				   'ambiguous)
				  ((or search-result search))))))
			((or (eq search-result 'ambiguous)
			     (null args))
			 (cond ((eq search-result 'ambiguous) 'ambiguous)
			       (search-result (append search-result (list aug)))
			       (t ()))))))))
	(cond ((and check-current check-sons)
	       'ambiguous)
	      ((or check-current check-sons))))))



;;; References Module

;;; Analyze REFERENCES to patterns to make sure they are valid.  




;;; References Module

;;; Check-pattern-refereneces checks each pattern 
;;; --- If that pattern has AS bound to it, but the pattern result is not used,
;;; then a warning is signalled.
;;; --- If patterns with no AS are referenced, default augment's are generated
;;; if possible (opts and alts for example).  Otherwise an error is reorted.

(defun check-pattern-references (pat &key (upattern? nil))
  (assert-sb (pattern-p pat))
  (cond ((and (not (get-pattern-referenced pat))
	      (not (get-pattern-bound pat))))
	((and (get-pattern-referenced pat)
	      (get-pattern-bound pat)))
	((and (not (get-pattern-referenced pat))
	      (get-pattern-bound pat))
	 (if (not upattern?)
	     (inter-phase-error
	      (format nil
		  "Augment bound to pattern but never used:~%~S~%"
		pat)
	      nil)))
	((and (get-pattern-referenced pat)
	      (not (get-pattern-bound pat)))
	 (ecase (pattern-kind pat)
	   ((nonterminal ext-nonterminal)) 
					; should be referenced.
	   ((seq ukeyword jux)
	    (inter-phase-error
	     (format nil
		 "Pattern referenced which has no augment:~%~S~%"
	       pat)
	     t))
			   
	   (alt		
	    (let ((new-aug (make-alt-augment pat)))
	      (set-pattern-binding-aug pat new-aug)
	      (set-pattern-bound pat t)
	      (cond ((and (get-pattern-augment pat)
			  (eq (augment-kind (get-pattern-augment pat))
			      'and))
		     (set-pattern-augment
		      pat
		      (make-augment :kind 'and
				    :args
				    (cons new-aug
					  (augment-args
					   (get-pattern-augment pat))))))
		    ((get-pattern-augment pat)
		     (set-pattern-augment
		      pat
		      (make-augment :kind 'and
				    :args
				    (list new-aug
					  (get-pattern-augment pat)))))
		    (t
		     (set-pattern-augment pat new-aug)))))
			   
	   ((plus doubleplus star doublestar)
	    (let ((son-pat (car (pattern-sons pat))))
	      (set-pattern-referenced son-pat t)))

	   (opt
	    (let ((son-pat (car (pattern-sons pat)))
		  (new-aug (make-opt-augment pat)))
	      (set-pattern-binding-aug pat new-aug)
	      (set-pattern-bound pat t)
	      (cond ((and (get-pattern-augment pat)
			  (eq (augment-kind (get-pattern-augment pat))
			      'and))
		     (set-pattern-augment
		      pat
		      (make-augment :kind 'and
				    :args 
				    (cons new-aug
					  (augment-args
					   (get-pattern-augment pat))))))
		    ((get-pattern-augment pat)
		     (set-pattern-augment
		      pat
		      (make-augment :kind 'and
				    :args
				    (list new-aug
					  (get-pattern-augment pat)))))
		    (t
		     (set-pattern-augment pat new-aug)))
	      (cond ((not (eq (pattern-kind pat) 'opt))
		     (set-pattern-referenced son-pat t))))))))

  (if (not (pattern-leaf-p pat))
      (mapc #'(lambda (pat)
		(check-pattern-references pat :upattern? upattern?))
	    (pattern-sons pat)))

					; $upattern revision 
  (if (pattern-upats pat)
      (map-upat-patterns #'(lambda (pat)
			     (check-pattern-references pat :upattern? t))
			 (pattern-upats pat))))




;;; References Module

;;; Build default augment for alt which is referenced but has no augment.  The
;;; default augment simply yields whichever son was chosen.

(defun make-alt-augment (pat)
  (make-augment
   :kind 'bind
   :name (make-bind-name pat)
	; not used, just for clarity
   :path (list pat)
   :args
   (list
    (make-augment
     :kind 'alt
     :name (pattern-name pat)
     :path (list pat)
     :args
     (do ((sons (pattern-sons pat) (cdr sons))
	  (AUG-list ()
		    (cons
		     (make-augment
		      :kind 'name
		      :name (pattern-name (car sons))
		      :path (list (car sons) pat)
		      :args
		      (cond ((eq (pattern-kind (car sons)) 'nonterminal)
			     (list (mk-id (pattern-leaf-ds (car sons)))))
			    ((eq (pattern-kind (car sons)) 'ext-nonterminal)
			     (multiple-value-bind (nt ext-name)
				 (pattern-leaf-ds (car sons))
			       (list nt ext-name)))
			    (t 
			     (pattern-kind (car sons)))))
		     AUG-list)))
	 ((null sons)
	  (reverse AUG-list))
       (set-pattern-referenced (car sons) t))))))



;;; References Module


;;; Build default augment for opt which is referenced but has no augment.  The
;;; default augment simply yields the son or nil.

(defun make-opt-augment (pat)
  (let* ((son-pat (car (pattern-sons pat)))
	 (new-aug 
	  (make-augment
	   :kind 'opt
	   :name (pattern-name pat)
	   :path (list pat)
	   :args
	   (list (make-augment
		  :kind 'term-const
		  :args (list (make-augment :kind 'literal-aug
					    :args (list
						   (mk-literal
						    (gen-null-opt-op))))
			      (make-augment :kind 'null
					    :args ())))
		 (make-augment
		  :kind 'name
		  :name (pattern-name son-pat)
		  :path (list son-pat pat)
		  :args
		  (cond ((eq (pattern-kind son-pat) 'nonterminal)
			 (list (mk-id (pattern-leaf-ds son-pat))))
			((eq (pattern-kind son-pat) 'ext-nonterminal)
			 (multiple-value-bind (nt ext-name)
			     (pattern-leaf-ds son-pat)
			   (list nt ext-name)))
			(t 
			 (pattern-kind son-pat))))))))
    (set-pattern-referenced son-pat t)
    (make-augment :kind 'bind
		  :name (make-bind-name pat)  ; above not used, for clarity
		  :path (list pat)
		  :args (list new-aug))))



(defun gen-null-opt-op ()
  (intern (concatenate 'string
	    (symbol-name *current-nt-name*)
	    "-NULL-"
	    (princ-to-string (incf *nt-opt-default-op-count*)))))




;;; Relocation Module

;;; The idea behind abstract syntax relocation is that abstract syntax can be
;;; best constructed in certain locations.  That is, it may be better
;;; constructed at lower position in the pattern tree.  Otherwise the parser
;;; would be required to grovel over large portions of the pattern and abstract
;;; syntax tree and maintain values it no longer needs. When abstract syntax is
;;; moved down, it is bound to a unique name which is then referenced from the
;;; orinial position.

;;; The reader may want to familiarize himself with the concept of a pattern
;;; path, described in the resolution module.  

;;; Please note that throughout this module, we rely on the fact that no errors
;;; were found during name resolution.




;;; Relocation	Module



;;; The following routine simply controls the application of relocate-augment.
;;; Note that a top-down organization is necessary because as peices of
;;; abstract syntax are moved down, it may become necessary to move subpeices
;;; of that abstract syntax further down.  With a top down approach, each
;;; pattern node need only be visited once and relocated abstract syntax will
;;; be revisited.

(defun relocate-augment-of-pattern (pat)
  (assert-sb (pattern-p pat))
  (cond ((get-pattern-augment pat)
	 (relocate-augment (get-pattern-augment pat)
			   pat)))

  (if (not (pattern-leaf-p pat))
      (mapc (function relocate-augment-of-pattern)
	    (pattern-sons pat)))
					; $upattern revision 
  (if (pattern-upats pat)
      (map-upat-patterns #'relocate-augment-of-pattern
			 (pattern-upats pat))))



;;; Relocation Module


;;; Relocate-augment determines the new location for a peice of abstract syntax
;;; (by calling deter-new-augment-location).  If it can move the abstract
;;; syntax down, it does so.  Otherwise it attempts to move the arguments (tree
;;; sons) of that abstract syntax down.

(defun relocate-augment (aug pat)
  (assert-sb (augment-p aug))
  (cond ((not (augment-leaf-p aug))
	 (let (path-to-new-location)
	   (cond ((and (or (is-pat-iterator pat)
			   (is-aug-iterator aug))
		       (setf path-to-new-location
			     (deter-new-augment-location aug))
		       (not (eq path-to-new-location t)) 
					; if it can go anywhere (such as
					; constructors with no args) then leave
					; it where it is.
		       (not (eq (car path-to-new-location) pat)))
		  (move-down aug (car path-to-new-location) pat))
		 (t
		  (mapc (function
			 (lambda (abs) (relocate-augment abs pat)))
			(augment-args aug))))))))


;;; This routine checks to see if there is a reason to move the augment down;
;;; nil is returned in this case.  For example, moving  list(`A,23) down, would
;;; only require passing it back up.


(defun should-move-down (aug)
  (assert-sb (augment-p aug))
  (and (not (augment-base-p aug))
       (or (not (eq (augment-kind aug) 'term-const))
	   (and (eq (augment-kind aug) 'term-const)
		(do ((L (mapcar (function should-move-down)
				(augment-args aug))
			(cdr L))
		     (result nil
			     (or result (car L))))
		    ((null L) result))))))




;;; Relocation Module


;;; Deter-new-augment-location determines how far down the tree the given peice
;;; of abstract syntax may be moved.  It actually returns a path whose lowest
;;; pattern of the tree is the location to which the abstract syntax should be
;;; moved.  Note that this may be the pattern node to which the abstract syntax
;;; is already associated (the path contains only one element).

(defun deter-new-augment-location (aug)
  (assert-sb (augment-p aug))
  (if (augment-base-p aug)
      t				; location arbitrary
      (case (augment-kind aug)
	((plus star)			; abstract syntax which must be moved
					; down to a certain location, their
					; associated patterns.
	 (augment-path aug))
	(t
	 (let ((initial-restriction
		(cond ((memq (augment-kind aug) '(opt alt name ext-name))
		       (augment-path aug))
					; abstract syntax which may not be
					; moved below a certain location, their
					; associated patterns.
		      (t
		       t))))		; abstract syntax which may be moved
					; down to arbitrary locations depending
					; on there sons.  t is a wild card
					; meaning there are no initial
					; restrictions.
	   (if (augment-leaf-p aug)
	       initial-restriction
	       (do ((aug-arg-list (augment-args aug)
				  (cdr aug-arg-list))
		    (current-match initial-restriction
				   (match-lists current-match
						(deter-new-augment-location
						 (car aug-arg-list)))))
		   ((or (null current-match)
			(null aug-arg-list))
		    (cond ((eq current-match t))
			  ((null current-match) 
			   ())
			  (t		; cleans off abstract syntax
			   (do ((result current-match (cdr result)))
			       ((or (pattern-p (car result))
				    (null result))
				result))))))))))))

			     



;;; Relocation Module

;;; Performs relocation.  Adds name references so values may be retrieved after
;;; moving down.

(defun move-down (aug new-pat old-pat)
  (let* ((new-name (gensym))
	 (new-aug (cond ((memq (augment-kind aug) '(bind plus star))
			 aug)
			(t
			 (make-augment :kind 'bind
				       :name new-name
				       :args (list aug))))))


    (cond ((eq aug (get-pattern-augment old-pat))
					; @hack, so discriminators may be
					; found.
	   (set-pattern-augment
	    old-pat
	    (make-augment :kind 'internal-bind
			  :name (make-bind-name old-pat)
					; above not used, just for clarity
			  :path (list old-pat)
			  :args
			  (list
			   (make-augment :kind 'name
					 :path (list new-aug)
					 :args (list (mk-id new-name)))))))
					; @hack!
	  (t
	   (let ((repl-aug
		  (make-augment :name
				(cond ((not (eq 'bind (augment-kind aug)))
				       new-name)
				      (t
				       (augment-name aug)))
				:kind 'name
				:args ()
				:path
				(cond ((is-aug-iterator aug)
				       (list new-pat)) ; reference used in slot
						       ; allocation
				      (t
				       (list new-aug)))))) ; reference used in
							   ; slot allocation
	     (repl-aug-in-aug (get-pattern-augment old-pat) aug repl-aug))))

    (cond ((null (get-pattern-augment new-pat))
	   (set-pattern-augment new-pat new-aug))
	  ((not (eq 'and
		    (augment-kind (get-pattern-augment new-pat))))
	   (set-pattern-augment new-pat
				(make-augment :kind 'and
					      :args
					      (list
					       (get-pattern-augment new-pat)
					       new-aug))))
	  (t				; (eq 'and (augment-kind ...))
	   (set-pattern-augment
	    new-pat
	    (make-augment :kind 'and
			  :args 
			  (append (augment-args
				   (get-pattern-augment new-pat))
				  (list new-aug))))))))




;;; Relocation Module

;;; Utility


;;; Match-lists takes two lists and returns the greatest sublist which is
;;; included in each list and incluedes the last element of each list.  For
;;; example, (match-lists '(a b c d e) '(x b a c d e))   -->   '(c d e) The
;;; value t is a wild card which matches any list.

(defun match-lists (list1 list2)
  (cond ((eq list1 t) list2)
	((eq list2 t) list1)
	(t
       	 (do ((runner1 (reverse list1) (cdr runner1))
	      (runner2 (reverse list2) (cdr runner2))
	      (result  nil
		       (cond ((eq (car runner1)
				  (car runner2))
			      (cons (car runner1) result))
			     (t result))))
	     ((or (null runner1)
		  (null runner2)
		  (not (eq (car runner1)
			   (car runner2))))
	      result)))))



(defun repl-aug-in-aug (aug target-aug repl-aug)    ;; @ Side effecting
						    ;; augments
  (assert-sb (augment-p aug))
  (if (not (augment-leaf-p aug))
      (cond ((memq target-aug (augment-args aug))
	     (do ((son-runner (augment-args aug) (cdr son-runner))
		  (i 0 (1+ i)))
		 ((eq target-aug (car son-runner))
		  (replace-augment-son aug repl-aug i))))

	    (t
	     (mapc #'(lambda (x)
		       (repl-aug-in-aug x
					target-aug repl-aug))
		   (augment-args aug))))))




;;; Module

;;; Repetitive patterns have special requirements.  The following routine makes
;;; sure the son of a repetitive pattern has AS bound to it and that AS is
;;; bound to the son of the patten.

(defun check-plus-star-augment-of-pattern (pat)
  (assert-sb (pattern-p pat))
  (cond ((and (is-pat-iterator pat)
	      (get-pattern-augment pat))
	 (check-plus-star-augment (get-pattern-augment pat)
				  pat)))
  (if (not (pattern-leaf-p pat))
      (mapc (function check-plus-star-augment-of-pattern)
	    (pattern-sons pat)))
					; $upattern revision 
  (if (pattern-upats pat)
      (map-upat-patterns #'check-plus-star-augment-of-pattern
			 (pattern-upats pat))))




(defun check-plus-star-augment (aug pat)
  (assert-sb (augment-p aug))
  (cond ((not (augment-leaf-p aug))
	 (cond ((is-aug-iterator aug)
		(process-plus-star aug pat)))
	 (mapc (function
		(lambda (abs) (check-plus-star-augment abs pat)))
	       (augment-args aug)))))



(defun process-plus-star (aug pat)
  (let ((name-augment-to-modify
	 (car (augment-args aug))))
    (cond ((not (eq 'name (augment-kind name-augment-to-modify)))
	   (error "SB Internal Error (see maintainter)"))
	  (t
	   (let* ((new-pat (car (pattern-sons pat)))
		  (new-aug (make-augment
			    :kind 'bind
			    :name (make-bind-name new-pat)
					; just for clarity, not used
			    :path (list new-pat)
			    :args (list name-augment-to-modify))))
	     (set-pattern-referenced new-pat nil)
	     (set-pattern-bound new-pat t)
	     (set-pattern-binding-aug new-pat new-aug)
	     (cond ((null (get-pattern-augment new-pat))
		    (set-pattern-augment new-pat
					 new-aug))
		   ((not (eq 'and
			     (augment-kind (get-pattern-augment new-pat))))
		    (set-pattern-augment
		     new-pat
		     (make-augment :kind 'and
				   :args (list
					  (get-pattern-augment new-pat)
					  new-aug))))
		   (t
		    (set-pattern-augment
		     new-pat
		     (make-augment :kind 'and
				   :args
				   (append (augment-args
					    (get-pattern-augment new-pat))
					   (list new-aug))))))
	      
	     (cond ((eq aug (get-pattern-augment pat))
		    (set-pattern-augment pat ())) ; No longer needed.
		   (t
		    (repl-aug-in-aug (get-pattern-augment pat)
				     aug
				     (make-augment :kind 'name
						   :name (pattern-name pat)
						   :args ()
						   :path (list pat))))))))))


;;; Slot Allocation Module


;;; Slots are the means by which information is passed between various portions
;;; of the tree during parsing and unparsing. Rather then remember all the
;;; interdependencies and data paths at parse and unparse time, we generate the
;;; slots now to simplify the later phases.  Some optimization occurs during
;;; this slot allocation so that our arrays don't become unduely large.

;;; The majority of patterns and augments are assigned a single slot.
;;; Alternations, optionals, and the star patterns are exceptions; they get two
;;; slots.  Their second slot has their particular value while the first slot
;;; contains the branch index of the alternation or (t or nil) for the optional
;;; or star.  Sequence patterns which have no referenced results are not given
;;; slots because no values will be generated for them.




;;; Top level

(defun do-slot-allocation (pat)
  (setq *max-slot-count* 0)
  (do-pattern-slot-allocation pat 0))


;;; General Utility

(defun incr-slot-count (slot-count incr)
  (let ((new-slot-count (+ slot-count incr)))
    (setq *max-slot-count* (max *max-slot-count* new-slot-count))
    new-slot-count))



;;; Slot Allocation Module

(defun do-pattern-slot-allocation (pat slot-count &key (upats-done nil))
  
  				; $upattern revision 
  (cond ((and (not upats-done)
	      (pattern-upats pat))
	 (max (do-pattern-slot-allocation pat
					  slot-count
					  :upats-done t)
	      (reduce #'max
		      (map-upat-patterns #'(lambda (pat)
					     (do-pattern-slot-allocation
					      pat slot-count))
					 (pattern-upats pat)
					 :result t)
		      :initial-value 0)))

;;; --->   indentation

 (t  
  (ecase (pattern-kind pat)
    ((nonterminal ext-nonterminal)
      (set-pattern-slot pat slot-count)
      (cond ((get-pattern-augment pat)
	     (do-augment-slot-allocation (get-pattern-augment pat)
					 (incr-slot-count slot-count 1)))
	    (t
	     (incr-slot-count slot-count 1))))

     (ukeyword
     (let ((current-slot-count
	    (incr-slot-count slot-count 
			     (cond ((or (get-pattern-referenced pat)
					(get-pattern-bound pat))
				    1)
				   (t 0)))))
       (cond ((or (get-pattern-referenced pat)
		  (get-pattern-bound pat))
	      (set-pattern-slot pat slot-count)))
       (cond ((get-pattern-augment pat)
	      (do-augment-slot-allocation (get-pattern-augment pat)
					  (incr-slot-count slot-count 1)))
	     (t
	      current-slot-count))))

     (alt
      (let ((start-slot
	     (incr-slot-count slot-count 
			      (cond ((and (or (get-pattern-referenced pat)
					      (get-pattern-bound pat))
					  (not (memq pat *top-level-alts*)))
				     2)	; @hack, see above doc.
				    (t 1))))
	    (local-max-slot-count 0)
	    (temp-max-slot-count 0))

	(set-pattern-slot pat slot-count)
	(do ((son-list (pattern-sons pat) (cdr son-list)))
	    ((null son-list) 
	     (cond ((get-pattern-augment pat)
		    (do-augment-slot-allocation (get-pattern-augment pat)
						local-max-slot-count))
		   (t
		    local-max-slot-count)))

	  (setq temp-max-slot-count
		(do-pattern-slot-allocation (car son-list)
					    start-slot))
	  (setq local-max-slot-count (max local-max-slot-count
					  temp-max-slot-count)))))
     
     ;; do-pattern-slot-allocation is continued on the next page. 
         
     ;; Slot Allocation Module
     
     ;; do-pattern-slot-allocation (continued from previous page).

     ((seq jux)
      (let ((current-slot-count
	     (incr-slot-count slot-count 
			      (cond ((or (get-pattern-referenced pat)
					 (get-pattern-bound pat))
				     1)
				    (t 0)))))
	(cond ((or (get-pattern-referenced pat)
		   (get-pattern-bound pat))
	       (set-pattern-slot pat slot-count)))
	(do ((son-list (pattern-sons pat) (cdr son-list)))
	    ((null son-list)
	     (cond ((get-pattern-augment pat)
		    (do-augment-slot-allocation (get-pattern-augment pat)
						current-slot-count))
		   (t
		    current-slot-count)))
	  (setq current-slot-count
		(do-pattern-slot-allocation (car son-list)
					    current-slot-count)))))


     ((opt star doublestar plus doubleplus)
      (let ((start-slot
	     (incr-slot-count slot-count 
			      (case (pattern-kind pat)
				((star doublestar) 1)
				((plus doubleplus) 1)
				((opt) 
				 (cond ((or (get-pattern-referenced pat)
					    (get-pattern-bound pat))
					2)
				       (t 1))))))
					; @Worried about bugs from above.
	    temp-start-slot)
	(set-pattern-slot pat slot-count)
	(setq temp-start-slot
	      (do-pattern-slot-allocation (car (pattern-sons pat))
					  start-slot))
	(cond ((get-pattern-augment pat)
	       (setq temp-start-slot
		     (do-augment-slot-allocation (get-pattern-augment pat)
						 temp-start-slot))))
	temp-start-slot))))))

  





;;;  Slot Allocation Module

(defun do-augment-slot-allocation (aug slot-count)
  (let ((result				; used to hold the result until we can
					; clobber the augment-path field.
	 (ecase (augment-kind aug)

	   ((null string number literal id)
	    slot-count)
    
	   ((name ext-name)
	    (set-augment-key-slot
	     aug
	     (cond ((pattern-p (car (augment-path aug)))
		    (cond ((and (memq (pattern-kind (car (augment-path aug)))
				      '(opt alt))
				(not (memq (car (augment-path aug))
					   *top-level-alts*))) ; @hack (see
							       ; above)
			   (+ 1 (get-pattern-slot (car (augment-path aug)))))
			  (t
			   (get-pattern-slot (car (augment-path aug))))))
		   (t
		    (augment-result-slot (car (augment-path aug))))))
	    slot-count)
    
	   ((bind internal-bind)
	    (let ((new-slot-count
		   (incr-slot-count slot-count 
				    (cond ((pattern-p (car (augment-path aug)))
					   0)
					  (t 1)))))
	      (set-augment-result-slot
	       aug
	       (cond ((pattern-p (car (augment-path aug)))
		      ;; we are binding to a pattern
		      (cond ((and (memq (pattern-kind
					 (car (augment-path aug)))
					'(opt alt))
				  (not (memq (car (augment-path aug))
					     *top-level-alts*))) 
			     ;; @hack (see above)
			     (+ 1 (get-pattern-slot
				   (car (augment-path aug)))))
			    (t
			     (get-pattern-slot (car (augment-path aug))))))
		     (t
		      slot-count)))	; binding for abstract syntax. 
	      (do-augment-slot-allocation (car (augment-args aug))
					  new-slot-count)))
	   

	   
	   ;; do-augment-slot-allocation is continued on the next page. 
	       
	   ;; Slot Allocation Module
	   
	   ;; do-augment-slot-allocation (continued from previous page).
	   

	   ((alt opt)
	    (set-augment-key-slot aug
				  (get-pattern-slot (car (augment-path aug))))
	    (do ((son-list (augment-args aug) (cdr son-list)))
		((null son-list)
		 slot-count)
	      (do-augment-slot-allocation (car son-list)
					  slot-count)))
     

 
	   ((plus star)			; actually no plus or star in aug
					; (removed)
	    slot-count)
	    
	       ;; Error removed as this is not necessarily true for upatterns.
	       ;; See no need to leave the warning in. 
               ;; (error "SB Internal Error (see maintainter)~%~ 
	       ;;     No plus's or star's in augments at this point.")
	    
	   ((term-const list cons bcons append and)
	    (let ((start-slot slot-count))
	      (do ((son-list (augment-args aug) (cdr son-list)))
		  ((null son-list)
		   start-slot)
		(setq start-slot
		      (do-augment-slot-allocation (car son-list)
						  start-slot))))))))

    result))


								      
;;; Error Reporting

(defun inter-phase-error (string error?)
  (if (not *error-occurred-inter?*)
      (setq *error-occurred-inter?* error?))
  (if (not *local-error-occurred-inter?*)
      (setq *local-error-occurred-inter?* error?))
  (terpri)
  (if error?
      (format t "Inter-phase Error:~%")
      (format t "Inter-phase Warning:~%"))
  (format t "Problem in nonterminal definition: ~A~%" *current-nt-name*)
  (format t string))
