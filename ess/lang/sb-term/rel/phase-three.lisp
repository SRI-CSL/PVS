;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)phase-three.lisp	1.7 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Anne Rogers 
;;; Revised, Scott Dietzen, Mon Oct 13 16:05:43 1986



(in-package :syntax-box)   (use-package :ergolisp)

(use-package '(:sb-runtime))


; process-grammar


; Build fragment, the next intermediate representation, from the patterns.
; Fragments have the format

;	Number x Look ahead set x Code
;
;    Number -- a unique tag 
;    Look ahead set -- the look ahead set for the pattern
;    code -- the code for parsing the pattern (including the as)


(defun make-fragment (fs-list code)
    (list (gentemp) fs-list code))



;;; Following moved to aux-funs so it would be loaded earlier. -srd.

;;; This function is used in collapse.l and in this file.  It is called at
;;; runtime, so it interns symbols into the wrong package, causing generated
;;; parser files to be written with var-name's qualified for some bogus 
;;; package.  These files use to use the function in runtime-macros.l, but it
;;; is not solely used in places that are only called at the time of compiling
;;; a generated parser.
;;;(defun slot-code (x)
;;;   (intern (format nil "V~D" x) *sb-package*))

;;;(eval-when (load compile eval)
;;;  (defmacro get-number (fragment)
;;;    `(car ,fragment))
;;;  (defmacro get-fs (fragment)
;;;    `(cadr ,fragment))
;;;  (defmacro get-code (fragment)
;;;    `(nth 2 ,fragment)))




(defvar *need-brackets* nil)


; Process-grammar: Grammar Structure
;   

(defun process-grammar (gram-struct)

      ; to produce the correct information for keywords, we need another
      ; dependency-graph.    A ::= ... B ... will result in an edge from
      ; A to B.
  (setq *keyword-graph* (list nil))
  (init-graph *keyword-graph* (mapcar #'(lambda (x) ; srd revised
					  (nt-name x))
				      (get-nt-list gram-struct)))
  
      ; Nt-list has the format (Nt-name Nt-information)* -- There is an
      ; entry for each nonterminal in the grammar.
  (do ((nts (get-nt-list gram-struct) (cdr nts)))
      ((null nts))

    (setq *current-nt* (nt-name (car nts)))
    (process-nt (nt-name (car nts))	; srd revised
		(car nts)
		gram-struct)))



; Process-nt: Nt-name x Nt-information x Grammar structure
;    Given a nonterminal definition build the code fragments for it.
;    Once all the fragments are built call the appropriate collapse 
;    routine to build the correct function.

(defun process-nt (nt nt-def gram-struct)
    (let* ((aux-list nil) (initials nil) (medials nil)
	   (pat-as nil) (initial-la-check nil) (jux-branches nil)
	   (medial-la-check nil) (s-elist nil)
           (pattern (nt-pattern nt-def))
	   (temp-brackets (get-nt-brackets nt gram-struct))
	   (brackets (if temp-brackets (list temp-brackets) nil))
	   (number-of-slots (get-nt-slot-total nt-def)))


         ; this flag will be true if we have overlapping brackets
      (setq *need-brackets* nil)

          ; If the nonterminal defintion is an alternation call
          ; process-son on each of the branches.
      (cond ((eq (get-kind pattern) 'alt)

	     (if (get-as pattern)
		 (setq pat-as (as-parse-gen (get-as pattern) gram-struct)))

	         ; For each branch of the alternation
	     (do ((sons (get-sons pattern) (cdr sons)))
		 ((null sons))
		      
	           ; When we flattened the nonterminal definition pattern
	           ; we only saved the new patterns for productions that were
	           ; left recursive.  We need to flatten again,
	           ; But this time to only one symbol. We can not 
	           ; save the two symbol versions for each pattern because 
	           ; overlapping productions can be generated.  For example,
	           ; B* ==> Epsilon, B, BBB*.  We can not choose between the
	           ; last two productions within two symbols.  If we only 
	           ; flatten to one symbol we get Epsilon, BB* which is ok.

	       (do ((flat-sons (flatten-first (car sons)) (cdr flat-sons)))
		   ((null flat-sons))

		     ; generate fragments -- process son returns a list of
		     ; fragments.  The first of which is the main fragment
		     ; for the production.  
		 (multiple-value-bind (init-flag ps-result jux-value)
		   (process-son (car flat-sons) nt gram-struct)


		       ; Epsilon production are illegal.
		   (cond ((leading-epsilon-p (get-fs (car ps-result)))
			  (my-error 1))

			     ; If init-flag is true, the production is not
			     ; left recursive.
			 (init-flag
			      ; save the look ahead sets -- we check for two symbol
			      ; disambiguation later.
			  (push (get-fs (car ps-result)) initial-la-check)

			      ; Productions that have epsilon as a possible 
			      ; second symbol get tacked onto the
			      ; end of the section for initial operators.  This is
			      ; so that we can parse both id and id : foo.  For the
			      ; moment save the fragment number in s-elist.
			  (if (check-second-epsilon (get-fs (car ps-result)))
			      (push (get-number (car ps-result)) s-elist)
			      (push (get-number (car ps-result)) initials)))

			     ; Otherwise it is a medial production.  Save the
		             ; look ahead set with the medial look-ahead-sets
		             ; and save the fragment number in the medial fragment
		             ; list.
			 (t (if jux-value (push jux-value jux-branches))
			    (push (get-number (car ps-result)) medials)
			    (push (get-fs (car ps-result)) medial-la-check)))

		       ; Save all the fragments.
		   (setq aux-list (append aux-list ps-result)))))


	        ; If this nonterminal has brackets then we need to 
	        ; add a look ahead set for the implicit bracket production to
	        ; the initial look ahead list so that we can make sure that there 
	        ; is not a look ahead set clash.
	     (if brackets
		 (push (build-bracket-fs-entry nt brackets) initial-la-check))

	        ; check the initial look ahead set and make sure that we can
	        ; disambiguate between productions within two symbols.
	     (setq initial-la-check (compare-fs-list initial-la-check))

	        ; check the medial look ahead set and make sure that we can
	        ; disambiguate within one symbol.  Note: Compare-medial-fs-list
	        ; produces an error list.
	     (setq medial-la-check (compare-medial-fs-list medial-la-check)))


	    (t
	        ; Do the same thing but for singleton nonterminal definitions.
	        ; Must flatten to one symbol.
	     (do ((flat-sons (flatten-first pattern) (cdr flat-sons)))
		 ((null flat-sons))
	       
	           ; generate fragment list for pattern.  Leading fragment is the
	           ; main one. 
	       (multiple-value-bind (init-flag ps-result jux-value)
		 (process-son (car flat-sons) nt gram-struct)

		 (declare (ignore jux-value))
		     ; epsilon productions are illegal.
		 (cond ((leading-epsilon-p (get-fs (car ps-result)))
			(my-error 1))

		           ; is it an initial?
		       (init-flag
			    ; save the look ahead set for comparison later.
			(push (get-fs (car ps-result)) initial-la-check)
			    ; save all the fragments.
			(setq aux-list (append aux-list ps-result))
			    ; if epsilon is a possible second then store
		            ; fragment number in s-elist.
			(if (check-second-epsilon (get-fs (car ps-result)))
			    (push (get-number (car ps-result)) s-elist)
			    (push (get-number (car ps-result)) initials)))

		           ; can not have medials for a singleton nonterminal
		           ; definition.
		       (t (my-error 31)))))

	         ; again deal with brackets if there are any.
	     (if brackets
		 (push (build-bracket-fs-entry nt brackets) initial-la-check))

	         ; check for two symbol disambiguation.
	     (setq initial-la-check (compare-fs-list initial-la-check))))


          ; Call the appropriate routine for function generation.  If
          ; medials is empty then call one of the initials only routines.
      (if (null medials)
	      ; If this nonterminal has brackets then we need to call the 
	      ; routine that will generate code to eat them.  Ow.  call the
	      ; standard initials-only routine.
	  (if brackets
	      (collapse-bracket-i-only (append initials s-elist)
				       aux-list
				       number-of-slots
				       nt
				       pat-as
				       initial-la-check
				       gram-struct)

	      (collapse-i-only (append initials s-elist)
			       aux-list
			       number-of-slots
			       nt
			       pat-as
			       initial-la-check
			       *need-brackets*))

	      ; Ow.  We need to call one of the routines that handles
	      ; left recursive patterns.  Again, if there are brackets then
	      ; call the routine that generates code todeal with brackets.
	  (if brackets
	      (collapse-bracket-im (append initials s-elist)
				   medials
				   number-of-slots
				   aux-list
				   nt
				   pat-as
				   gram-struct
				   initial-la-check
				   medial-la-check
				   jux-branches)

	      (collapse-im (append initials s-elist)
			   medials
			   number-of-slots
			   aux-list
			   nt
			   pat-as
			   gram-struct
			   initial-la-check
			   medial-la-check
			   *need-brackets*
	   		   jux-branches)))))

	    


; flatten-first
;         Given a pattern signal an error if it can be empty (ie. it is an
;         opt, star, or doublestar).  If not call flatten.

(defun flatten-first (pat)
  (let ((results nil))

    (case (get-kind pat)

      ((seq plus doubleplus)
           ; flatten the pattern and check all the results.
       (do ((new-alts (flatten-pattern pat) (cdr new-alts))
	    (temp-result))
	   ((null new-alts))
			
	     ; if the first guy in the pattern
	     ; is not atomic then check will return
	     ; some new patterns.  Add these to the end
	     ; of the list.
	 (setq temp-result (check-first (car new-alts)))
	 (if temp-result
	     (setq new-alts (append new-alts temp-result))
	     (push (car new-alts) results))))

          ; This is only here for completeness.  This case will be
	  ; detected by the first flatten routine.
      ((opt star doublestar) (my-error 1))

          ; base cases -- already ready flattened
      ((ukeyword nonterminal ext-nonterminal jux)
       (push pat results))

      (t (my-error 15)))

    results))
			
; Check-first:
;      Check to make sure that the pattern is finished being flattened.
;      The first son must be atomic.

(defun check-first (pat)
  (if (not (or (atomic pat) (atomic (get-first-son pat))))
      (flatten-pattern pat)))
			       


; Build-bracket-fs-entry: builds the lookahead set entry for the implicit
; production associated with the brackets. {Open-bracket NT Close-bracket}

(defun build-bracket-fs-entry (nt brackets)
  (let ((seconds (make-initial-list (get-look-ahead (list 'nonterminal nt))))
	(result nil))

    (do ((guys brackets (cdr guys)))
	((null guys) result)

      (push (cons (caar guys) seconds) result))))


; Process-son: Pattern x Nt-name x Grammar structure --> fragment*
;    Generate fragment list for a pattern.  We know that the pattern with have
;    one of three types.  It will be either a singleton nonterminal, singleton
;    keyword or a sequence where the first symbol is atomic.
;
;    The function returns three values.  First an initial flag which is true when
;    the leading symbol is not left recursive.  The second value is the list of
;    fragments.  The first fragment in the list is the main fragment for the
;    production.  The third value is the juxtaposition list
;    (fragment-number jux-name LAS LBP).

(defun process-son (pat nt-name gram-struct)
  (let ((result nil) (init-flag t) (jux-value nil))

    (case (get-kind pat)
      
          ; Singleton nonterminal, keyword, or external nonterminal.
          ; Must be an initial (singleton recursive instances are illegal
          ; and have already been checked for.)
          ; Pattern-initial is True for any symbol that starts a production.
      ((nonterminal ukeyword ext-nonterminal)
       (set-initial pat t)
       (setq result (process-pattern nt-name pat 0 gram-struct)))

          ; singleton jux -- if pattern is left recursive 
          ; Build code in-line.
          ;    Move current-result to left-son's result slot
          ;    If left-son has abstract syntax, process it.
          ;    Parse right-son with jux's rbp.
          ;    If pattern has abstract syntax, handle it here.
      (jux
       (set-initial pat t)
       (if (recursive-p (get-first-son pat) nt-name)
	   (let ((left (get-first-son pat)) (code nil))

	     (setq init-flag nil)
	     (setq result
		   (process-pattern nt-name (get-second-son pat)
				    (get-jux-bp (pattern-name pat) 'right
						 nt-name gram-struct)
				    gram-struct))

	     (if (get-as pat)
		 (push (as-parse-gen (get-as pat) gram-struct) code))
	     (push `(parse ,(get-number (car result))) code)
	     (if (get-as left) (push (as-parse-gen left gram-struct) code))
	     (push `(slot-to-slot ,(slot-code (get-result left))
				  ,(slot-code 0))
		   code)
	     (push (make-fragment (get-fs (car result)) (add-let code))
		   result)
	     (setq jux-value (list (get-number (car result))
				   (intern (string (gensym)) *sb-package*)
				   (get-fs (car result))
				   (get-jux-bp (pattern-name pat) 'left nt-name
					       gram-struct))))

	      ; ow. let process-jux take care of it.
	   (setq result (process-pattern nt-name pat 0 gram-struct))))


          ; The leading symbol in the sequence must be atomic because of the
          ; flattening.  
      ((seq internal-seq)
       (let ((first (get-first-son pat)))
		
	    ; First is the leading symbol for this production.
	 (set-initial first t)

	 (case (get-kind first)

	       ; If first is a keyword then this production is an initial.
	   ((ukeyword ext-nonterminal)
	    (setq result (process-pattern nt-name pat 0 gram-struct)))

	       ;  leading juxtaposition
	   (jux
	        ; left recursive, again build code here.  remember to parse
	        ; build code in-line.
	        ;    move current result to result slot for left-son of jux.
	        ;    if left-son of jux has abstract syntax, process it.
	        ;    parse right son of jux.
	        ;    process abstract syntax for jux if there is any.
	        ;    parse rest of sequence.
	    (if (recursive-p (get-first-son first) nt-name)
		(let ((left (get-first-son first))
		      (code nil)
		      (r1 (process-pattern nt-name (get-second-son first)
			       (get-jux-bp (pattern-name first) 'right nt-name
					   gram-struct)
			       gram-struct))
		      (r2 (process-pattern nt-name (get-second-son pat)
					   0 gram-struct)))
		  (setq init-flag nil)
		  (if (get-as pat)
		      (push (as-parse-gen (get-as pat) gram-struct) code))
		  (push `(parse ,(get-number (car r2))) code)
		  (if (get-as first)
		      (push (as-parse-gen (get-as first) gram-struct) code))
		  (push `(parse ,(get-number (car r1))) code)
		  (if (get-as left)
		      (push (as-parse-gen (get-as left) gram-struct) code))
		  (push `(slot-to-slot ,(slot-code (get-result left))
				       ,(slot-code 0))
			code)
		  (setq result
			(cons (make-fragment (make-seq-fs (get-fs (car r1))
							  (get-fs (car r2)))
					     (add-let code))
			      (append r1 r2)))
		  (setq jux-value (list (get-number (car result))
				       (intern (string (gensym)) *sb-package*)
				       (get-fs (car result))
				       (get-jux-bp (pattern-name first) 'left
						   nt-name gram-struct))))

		    ; ow let process-seq and process-jux deal with the pattern
		(setq result (process-pattern nt-name pat 0 gram-struct))))

	   (nonterminal
	    (let* ((temp (get-second-son pat))
		   (second (if (atomic temp) temp (get-first-son temp))))

	                ; if the pattern is not left-recursive, process-seq
	                ; can handle it.
	      (cond ((not (recursive-p first nt-name))
		     (setq result (process-pattern nt-name pat 0 gram-struct)))

		        ; if it is left recursive and the second symbol is
		        ; not a keyword, then we have an error.  Should
		        ; be using jux.  A ::= A B .... => A ::= A jux B ...
		    ((not (eq (get-kind second) 'ukeyword))
		     (my-error 37))

		    (t  ; left recursive -- we have a medial.
		     (let ((code nil))

		       (setq init-flag nil)
		           ; make sure we get the right RBP for the medial.
		       (set-medial second t)
		       (setq result
			     (process-pattern nt-name temp 0 gram-struct))

		       (if (get-as pat)
			   (push (as-parse-gen (get-as pat) gram-struct) code))
		       (push `(parse ,(get-number (car result))) code)
		       (if (get-as first)
			   (push (as-parse-gen (get-as first) gram-struct) code))
		       (push `(slot-to-slot ,(slot-code (get-result first))
					    ,(slot-code 0))
			     code)

		       (push (make-fragment (get-fs (car result)) (add-let code))
			     result))))))
	       
	   (t (my-error 32)))))

      (t (my-error 32)))
				 
  (values init-flag result jux-value)))




; one random function
(defun add-let (code)
    (if (= (length code) 1) (car code) (cons 'progn code)))


; process-pattern:  Nt-name x Pattern x Number x Grammar Structure.
;    deal with pattern.  Return a list of code fragments.

(defun process-pattern (nt-name pat rbp gram-struct)

  (case (get-kind pat)
    (nonterminal (process-nonterminal nt-name pat rbp gram-struct))

    (ext-nonterminal (process-ext-nonterminal nt-name pat gram-struct))

    (ukeyword (process-keyword pat gram-struct))

    (jux (process-jux nt-name pat rbp gram-struct))
    
    (seq (process-sequence nt-name pat rbp gram-struct))

    (internal-seq (process-sequence nt-name pat rbp gram-struct))

    (alt (process-alternation  nt-name pat rbp gram-struct))

    (opt (process-optional nt-name pat rbp gram-struct))

    ((star gen-star) (process-star nt-name pat rbp gram-struct))

    (plus (process-plus nt-name pat rbp gram-struct))

    (doublestar (process-doublestar nt-name pat rbp gram-struct))

    (doubleplus (process-doubleplus nt-name pat rbp gram-struct))

    (just-as (list (make-fragment nil
				  (as-parse-gen (get-as pat) gram-struct))))

    (t (my-error 11))))



; Process-nonterminal: Nt-name x Pattern x Number x Grammar Structure --> fragment*
;    Handle nonterminals.

(defun process-nonterminal (nt-name pat rbp gram-struct)
  (let* ((name (get-first-son pat))
	 (full-name (list 'nonterminal name))
	 (slot (slot-code (get-result pat)))
	 (fun-name (make-function-name name))
	 (as (get-as pat))
	 (code nil))

    (if as (push (as-parse-gen as gram-struct) code))

        ; If the nonterminal is a lexical-terminal
        ; we just have to check to make sure we have
        ; the right type of token.
    (cond ((is-lexical-terminal name gram-struct)
	   (if slot
	       (push `(lam ,(make-singleton-lt name gram-struct)
			   (gobble-to-slot ,slot)) code)
	       (push `(lam ,(make-singleton-lt name gram-struct)
			   (gobble-token)) code))
	   (list (make-fragment (make-singleton-lt name gram-struct)
				(add-let code))))

	      ; if this is a recursive call then we need generate function call
	      ; code that uses the rbp.
	  ((eq name nt-name)

	   (if slot
	       (push `(code-to-slot ,slot (,fun-name ,rbp)) code)
	       (push `(toss-value (,fun-name rbp)) code))

	   (list (make-fragment (get-look-ahead full-name) (add-let code))))

	      ; If this is a leading symbol (but not recursive) then we
	      ; need to check for bracket overlap.  If the brackets for this
	      ; nonterminal overlap with the brackets for the current nonterminal
	      ; then they need to be removed from the look-ahead set for
	      ; the fragment.  Also, bracket-list needs to be added to the 
	      ; arguments of the generated function call.
          ((get-initial pat)

	       ; the keywords of this nonterminal can be seen from the current
               ; nonterminal.
	   (addedge (list 'nonterminal nt-name) full-name *keyword-graph*)

	   (cond ((bracket-overlap *current-nt* full-name gram-struct)
		  (if slot
		      (push `(code-to-slot ,slot (,fun-name 0 bracket-list)) code)
		      (push `(toss-value (,fun-name 0 bracket-list)) code))

		  (list (make-fragment
			 (remove-brackets full-name *current-nt* gram-struct)
			 (add-let code))))
		 (t
		  (if slot
		      (push `(code-to-slot ,slot (,fun-name 0)) code)
		      (push `(toss-value (,fun-name 0)) code))

		  (list (make-fragment (get-look-ahead full-name)
				       (add-let code))))))

	     ; ow.  just generate a normal function call with an rbp of 0.
	  (t
	       ; the keywords of this nonterminal can be seen from the current
               ; nonterminal.
	   (addedge (list 'nonterminal nt-name) full-name *keyword-graph*)
	   
	   (if slot
		 (push `(code-to-slot ,slot (,fun-name 0)) code)
		 (push `(toss-value (,fun-name 0)) code))

	     (list (make-fragment (get-look-ahead full-name) (add-let code)))))))




; Bracket-overlap : Nt-name x Nt-name x Grammar Structure -> brackets*
;
;   Check for overlap between the brackets of NT1 and the brackets and
;   inherited brackets of NT2.  NT2 will be either (nonterminal name) or
;   (ext-nonterminal name).
;

(defun bracket-overlap (nt1 nt2 gram-struct)
            ; get the full info -- including inherited.
  (let* ((b2 (cadr (get-bracketing-information nt2)))
 	 (b1 (get-nt-brackets nt1 gram-struct))
	 (result nil))

    (if (or (null b1) (null b2))
	nil
	(do ((guys b2 (cdr guys)))
	    ((null guys) result)
	  
	  (setq result (or result (eq (caar guys) (car b1))))))

        ; we need to keep the brackting information argument the current-
        ; nonterminal.
    (if result (setq *need-brackets* t))
    result))


; Remove-brackets: Nt-name x Nt-Name x Grammar Structure -> Look ahead set
;
;   Remove entry for the overlapping bracket from Nt's look ahead set
;   and return the new LAS.  Nt will be either (nonterminal name) or
;   (ext-nonterminal name)

(defun remove-brackets (nt cnt gram-struct)
  (let ((las (get-look-ahead nt))
	(b1 (get-nt-brackets cnt gram-struct)))

        ; check for an overlap.  This function should not be called if there
        ; is not an overlap but better to be safe.
    (cond ((bracket-overlap cnt nt gram-struct)

	   (do ((entries las (cdr entries)) (new-las))
	       ((null entries) new-las)

	         ; does this entry start with the offending bracket.
	     (cond ((eq (caar entries) (car b1)))
	
		       ; because we are taking away the opeing bracket
		       ; token we have to add the closing bracket as
		       ; as a possible second wherever we see an
		       ; epsilon.
		   ((member 'epsilon (cdar entries))
		    (push (append (list (caar entries) (cadr b1)) (cdar entries))
			  new-las))
		   
		       ; implicit epsilon case.
		   ((= (length (car entries)) 1)
		    (push (list (caar entries) (cadr b1)) new-las))
		   
		       ; ow.  just keep the entry.
		   (t (push (car entries) new-las)))))
	  (t las))))



; Process-ext-nonterminal: Name x Pattern x Grammar Structure --> fragment*
;    Handle ext-nonterminals.

(defun process-ext-nonterminal (current-nt-name pat gram-struct)
  (let* ((gram-name (get-second-son pat))
	 (nt-name (get-first-son pat))
	 (fun-name (make-function-name gram-name nt-name))
	 (full-name (list 'ext-nonterminal gram-name nt-name))
	 (slot (slot-code (get-result pat)))
	 (as (get-as pat))
	 (code nil))

        ; the keywords of this nonterminal can be seen from the current
        ; nonterminal.
    (addedge (list 'nonterminal current-nt-name) full-name *keyword-graph*)

    (if as (push (as-parse-gen as gram-struct) code))

        ; If this is a leading symbol, we
        ; need to check for bracket overlap.  If the brackets for this
        ; nonterminal overlap with the brackets for the current nonterminal
        ; then they need to be removed from the look-ahead set for
        ; the fragment.  Also, bracket-list needs to be added to the
        ; arguments of the generated function call.
    (cond ((get-initial pat)

	       ; if the brackets overlap then we may have picked up a
	       ; bracket belonging to this nonterminal.  Pass the
	       ; bracket list, in case.
	   (cond ((bracket-overlap *current-nt* full-name gram-struct)
		  (if slot
		      (push `(code-to-slot ,slot (,fun-name 0 bracket-list)) code)
		      (push `(toss-value (,fun-name 0 bracket-list)) code))

		  (list (make-fragment
			 (remove-brackets full-name *current-nt* gram-struct)
			 (add-let code))))
		 (t
		  (if slot
		      (push `(code-to-slot ,slot (,fun-name 0)) code)
		      (push `(toss-value (,fun-name 0)) code))
		  (list (make-fragment (get-look-ahead full-name)
				       (add-let code))))))



	     ; ow.  just generate a normal function call with an rbp of 0.
	  (t (if slot
		 (push `(code-to-slot ,slot (,fun-name 0)) code)
		 (push `(toss-value (,fun-name 0)) code))

	     (list (make-fragment (get-look-ahead full-name) (add-let code)))))))



; Process-keyword: Pattern --> fragment*
;    Handle keywords.

(defun process-keyword (pat gram-struct)
  (let ((code nil)
	(as (get-as pat))
	(keyword (get-first-son pat)))
	  
    (if as (push (as-parse-gen as gram-struct) code))

        ; check for keyword an eat it.
    (list (make-fragment (make-singleton-keyword keyword)
	       (add-let (cons `(lam ,(make-singleton-keyword keyword)
				    (gobble-token))
			      code))))))




; Process-Jux: Name x Pattern x Number x Grammar -> fragment*
;  handle internal juxtapositions.  Make sure that the jux is correct --
;  one of the operands must be recursive.
(defun process-jux (nt-name pat rbp gram-struct)
  (if (or (recursive-p (get-first-son pat) nt-name)
	  (recursive-p (get-second-son pat) nt-name))
      (let* ((new-rbp (get-jux-bp (pattern-name pat) 'right nt-name gram-struct))
	     (r1 (process-pattern nt-name (get-first-son pat) rbp gram-struct))
	     (r2 (process-pattern nt-name (get-second-son pat)
				  new-rbp gram-struct))
	     (code nil))

	(if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))
	(push `(parse ,(get-number (car r2))) code)
	(push `(parse ,(get-number (car r1))) code)
	(cons (make-fragment (make-seq-fs (get-fs (car r1)) (get-fs (car r2)))
			     (add-let code))
	      (append r1 r2)))

      (my-error 36)))

; Process-sequence: Nt-name x Pattern x Number x Grammar Structure.
;   handle sequence.

(defun process-sequence (nt-name pat rbp gram-struct)
  (let* ((first (get-first-son pat))
	 (second (get-second-son pat))
	 (first-kind (get-kind first))
	 temp1 temp2 code)
		      
        ; generate code for the front.
    (setq temp1 (process-pattern nt-name first rbp gram-struct))

        ; If the leading symbol is a keyword then the rbp may change.  If the
        ; keyword is a leading symbol then get the initial rbp.  If it is a
        ; medial, get the medial rbp.  OW, get the aggregate rbp.
    (if (eq first-kind 'ukeyword)
	(let ((temp
	                 ; if keyword is the first symbol in a production,
	                 ; we must have an initial
	       (cond ((get-initial first)
		      (get-nt-op-prec (get-first-son first)
				      nt-name
				      'initial
				      gram-struct))

		         ; if keyword is a medial, get medial rbp.
		     ((get-medial first)
		      (get-nt-op-prec (get-first-son first)
				      nt-name
				      'medial-right
				      gram-struct))

		         ; ow. we must have an aggregate.
		     (t (get-nt-op-prec (get-first-son first)
					nt-name
					'aggregate
					gram-struct)))))
	  
	     (if temp (setq rbp temp) (setq rbp 0)))

	(setq rbp 0))
    
        ; generate code for the back.
    (setq temp2 (process-pattern nt-name second rbp gram-struct))

        ; If first is a star, doublestar, plus or opt then
        ; the fs-list of the back must be distinct from the
        ; fs-list of the front.
    (if (and (member first-kind '(star doublestar plus opt gen-star))
	     (compare-fs (get-fs (car temp1)) (get-fs (car temp2))))
	(my-error 4))

				

        ; If the front is a doubleplus or a doublestar then
        ; the separator can not be in the initial list of
        ; the back.
    (if (member first-kind '(doublestar doubleplus))
	(let ((sep (get-separator first)))
	  (if (eq (get-kind sep) 'ukeyword)
	      (if (member `(keyword ,(get-first-son sep))
			  (make-initial-list (get-fs (car temp2)))
			  :test #'equal)
		  (my-error 5))
	      (my-error 22))))
			     
    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

        ; parse back.
    (push `(parse ,(get-number (car temp2))) code)
        ; parse front.
    (push `(parse ,(get-number (car temp1))) code)

    (cons (make-fragment
	   (make-seq-fs (get-fs (car temp1)) (get-fs (car temp2)))
	   (add-let code))
	  (append temp1 temp2))))
 


; Process-alternation: Nt-name x Pattern x Grammar Structure. --> fragment*
;    deal with alts.
   
(defun process-alternation (nt-name pat rbp gram-struct)
  (let ((sons-results nil) (fs-temp nil) (pp-temp nil)
	(alt-parse-list nil) (code nil) (se-list nil) )

       ; for each branch.
    (do ((alts (get-sons pat) (cdr alts))
	 (count 0 (+ count 1)))
	((null alts))

      (setq pp-temp (process-pattern nt-name (car alts) rbp gram-struct))

        ; can not have epsilon branches.
      (if (leading-epsilon-p (get-fs (car pp-temp))) (my-error 2))

          ; save fragments.
      (setq sons-results (append sons-results pp-temp))

         ; Save main fragment number.  If the look-ahead-set has
         ; a possible epsilon second, we want to push this branch
         ; to the bottom of the set of alternatives -- save in a special
         ; list.
      (if (check-second-epsilon (get-fs (car pp-temp)))
	  (push (list (get-number (car pp-temp)) count) se-list)
	  (push (list (get-number (car pp-temp)) count) alt-parse-list))

          ; Keep list of the sons fs-lists, so we can 
          ; compare them later.
      (push (get-fs (car pp-temp)) fs-temp))
		    
    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

        ; alt-parse, list of branch fragment numbers, result field.
    (push `(alt-parse ,(append (reverse alt-parse-list) (reverse se-list))
		      ,(get-result pat)) code)
       ; the compare-fs-list compares the look ahead sets for the branches
       ; check for two symbol disambiguation.  and builds the look ahead set
       ; for the alternation.
    (cons (make-fragment (compare-fs-list fs-temp) (add-let code))
	  sons-results)))






; Process-optional: Nt-name x Pattern x Number x Grammar structure -> fragment*
;    handle optionals.

(defun process-optional (nt-name pat rbp gram-struct)
  (let (code
	(pp-temp (process-pattern nt-name (get-first-son pat) rbp gram-struct)))

       ; epsilon guts illegal,
    (if (leading-epsilon-p (get-fs (car pp-temp))) (my-error 3))

    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

        ; opt-parse, inside fragment number, result field.
    (push `(opt-parse ,(get-result pat) ,(get-number (car pp-temp))) code)

        ; look-ahead set must have an extra epsilon for the empty case
    (cons (make-fragment (cons '(epsilon) (get-fs (car pp-temp)))
			 (add-let code))
	  pp-temp)))

			    
; Process-star: Nt-name x Pattern x Number x Grammar Structure --> fragment*
;    handle stars

(defun process-star (nt-name pat rbp gram-struct)
  (let* ((code nil)
	 (pat-son (get-first-son pat))
	 (parse-macro-name
	  (if (eq (get-kind pat) 'star) 'star-parse 'gen-star-parse))
	 (pp-temp (process-pattern nt-name pat-son rbp gram-struct)))

    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

    (push (list parse-macro-name
		(get-number (car pp-temp))
		    ; result field
		(get-result pat)
		    ; partial result field -- alternations use two slots.
		    ; The first is a flag, which is used to determine which
		    ; branch was matched, and the second contains the result
		    ; of parsing that branch.
		(if (eq (get-kind pat-son) 'alt)
		    (+ (get-result pat-son) 1)
		    (get-result pat-son)))
	  code)

        ; remember stars can go to epsilon.
    (cons (make-fragment
	   (cons '(epsilon) (handle-loop-look-ahead
			     (get-fs (car pp-temp))
			     (make-initial-list (get-fs (car pp-temp))) 6))
	   (add-let code))
	  pp-temp)))



; Process-plus: Nt-name x Pattern x Number x Grammar Structure --> fragment*
;   handle pluses

(defun process-plus (nt-name pat rbp gram-struct)
  (let* ((code nil)
	 (pat-son (get-first-son pat))
	 (pp-temp (process-pattern nt-name pat-son rbp gram-struct)))

    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

    (push (list 'plus-parse
		(get-number (car pp-temp))
		    ; result field
		(get-result pat)
		    ; partial result field -- alternations use two slots.
		    ; The first is a flag, which is used to determine which
		    ; branch was matched, and the second contains the result
		    ; of parsing that branch.
		(if (eq (get-kind pat-son) 'alt)
		    (+ (get-result pat-son) 1)
		    (get-result pat-son)))
	  code)

    (cons (make-fragment
	   (handle-loop-look-ahead (get-fs (car pp-temp))
				   (make-initial-list (get-fs (car pp-temp)))
				   7)
	   (add-let code))
	  pp-temp)))

	    
; Process-doublestar: Nt-name x Pattern x Number x Grammar Structure --> fragment*
; handle doublestars

(defun process-doublestar (nt-name pat rbp gram-struct)

  (let ((pat-son (get-subpat pat))
	(sep (get-separator pat))
	sep-name pp-temp code)

	    
		      
        ; Get the separator.  Make sure that it is
        ; a user keyword.
    (if (eq (get-kind sep) 'ukeyword)
	(setq sep-name (get-first-son sep))
	(my-error 22))

    (setq pp-temp (process-pattern nt-name pat-son rbp gram-struct))

    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

    (push (list 'doublestar-parse
		(get-number (car pp-temp))
		    ; result field
		(get-result pat)
		    ; partial result field -- alternations use two slots.
		    ; The first is a flag, which is used to determine which
		    ; branch was matched, and the second contains the result
		    ; of parsing that branch.
		(if (eq (get-kind pat-son) 'alt)
		    (+ (get-result pat-son) 1)
		    (get-result pat-son))

		    ; seperator.
		(list 'quote (make-keyword sep-name)))
	  code)

        ; doublestar ==> epsilon.
    (cons (make-fragment
	   (cons '(epsilon)
		 (handle-loop-look-ahead (get-fs (car pp-temp)) `(,sep-name) 8))
	   (add-let code))
	  pp-temp)))





; Process-doubleplus: Nt-name x Pattern x Number x Grammar Structure --> fragment*
;    handle doublepluses

(defun process-doubleplus (nt-name pat rbp gram-struct)

  (let ((pat-son (get-subpat pat))
	(sep (get-separator pat))
	(sep-name nil) (code nil) (pp-temp nil))
		      
    ; Separator must be a user keyword.
    (if (eq (get-kind sep) 'ukeyword)
	(setq sep-name (get-first-son sep)) 
	(my-error 22))

    (setq pp-temp (process-pattern nt-name pat-son rbp gram-struct))

    (if (get-as pat) (push (as-parse-gen (get-as pat) gram-struct) code))

    (push (list 'doubleplus-parse
		(get-number (car pp-temp))
		    ; result field
		(get-result pat)
		    ; partial result field -- alternations use two slots.
		    ; The first is a flag, which is used to determine which
		    ; branch was matched, and the second contains the result
		    ; of parsing that branch.
		(if (eq (get-kind pat-son) 'alt)
		    (+ (get-result pat-son) 1)
		    (get-result pat-son))
		    ; seperator name
		(list 'quote (make-keyword sep-name)))
	  code)

    (cons (make-fragment
	   (handle-loop-look-ahead (get-fs (car pp-temp)) `(,sep-name) 9)
	   (add-let code))
	  pp-temp)))



; Handle-loop-epsilon: Look ahead set x initial symbols x Number --> look ahead set
;   Build the look ahead set for looping constructs.

(defun handle-loop-look-ahead (fs-list initials error-number)

      ; the guts of a loop can not go to epsilon.
  (cond ((leading-epsilon-p fs-list) (my-error error-number))

	    ; Everywhere we see an epsilon as a possible second we have to
	    ; add the initials as possible seconds too.  
	(t (do ((fs-temp fs-list (cdr fs-temp)) (new-fs))
	       ((null fs-temp) new-fs)
			  
	         ; implicit epsilon.
	     (cond ((= (length (car fs-temp)) 1)
		    (push (append (list (caar fs-temp) 'epsilon) initials) new-fs))

		   ((second-epsilon-p (car fs-temp))
		    (if (intersection (cdr fs-temp) initials)
			(my-error 30)
			(push (append (car fs-temp) initials) new-fs)))

		   (t (push (car fs-temp) new-fs)))))))




;  As-Parse-Gen: As-Type x Grammar -> Code; 
;     Given a piece of abstract syntax build the code for it.

(defun as-parse-gen (as-pat gram-struct)

  (case (get-as-kind as-pat)
    (number
     `(mk-number ,(get-aug-leaf-value as-pat)))
    (string
     `(mk-string ,(get-aug-leaf-value as-pat)))
    (id
     `(mk-id ',(get-aug-leaf-value as-pat)))
    (literal
     `(mk-id ',(get-aug-leaf-value as-pat)))

    (null
     (error "Internal error: Attempting to generate code for a null." ))

    (list
     `(mk-temp-rept (list ,@(mapcar #'(lambda (x)
					(as-parse-gen x gram-struct))
				    (get-as-args as-pat)))))
    (cons
     `(mk-temp-rept
       (cons ,(as-parse-gen (car (get-as-args as-pat))
			    gram-struct)
	     (ds-temp-rept ,(as-parse-gen (cadr (get-as-args as-pat))
					  gram-struct)))))

    (bcons
     `(mk-temp-rept
       (append (ds-temp-rept ,(as-parse-gen (car (get-as-args as-pat))
					    gram-struct))
	       (list ,(as-parse-gen (cadr (get-as-args as-pat))
				    gram-struct)))))
			     

    (append
     `(mk-temp-rept
       (append (ds-temp-rept ,(as-parse-gen (car (get-as-args as-pat))
					    gram-struct))
	       (ds-temp-rept ,(as-parse-gen (cadr (get-as-args as-pat))
					    gram-struct)))))
     
     

    ;; term constructor call. 
    (term-const
     (cond ((has-list-arg? as-pat)	;  routine imported from sort-gen.
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   (ds-temp-rept
			    ,(as-parse-gen (cadr (get-as-args as-pat))
					   gram-struct))))
	   ((or (has-null-arg? as-pat)	; routines imported from sort-gen.
		(has-elist-arg? as-pat))
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   nil))
	   (t
	    `(make-sb-term ',(augment-term-const-op as-pat)
			   (list ,@(mapcar #'(lambda (x)
					       (as-parse-gen  x gram-struct))
					   (cdr (get-as-args as-pat))))))))

    
    ;; or it is a constructor call.  Deal with sons and slap function name onto
    ;; the front of the resulting term.


        ; If the type is name then we just need to return the
        ; value from that slot.
    ((name ext-name)
     (cond ((and (pattern-p (car (augment-path as-pat)))
		 (memq (pattern-kind (car (augment-path as-pat)))
		       '(star doublestar plus doubleplus)))
	    `(ck-rept-ref ,(slot-code (get-as-key as-pat))))
	   ;; srd revised.  This is a hack. It would be better if all these
	   ;; values were initialed properly, but this is hard because of the
	   ;; way the parser generator flattens things.  This is a much easier
	   ;; and not too expensive fix.
	   (t
	    (slot-code (get-as-key as-pat)))))


	       
        ; If the type is bind then evaluate the son and store
        ; the result in the appropriate slot (the slot specified in the bind)
    (bind 
     (let ((as-code (as-parse-gen (get-as-first-son as-pat) gram-struct)))
       (cond ((and (atom as-code)
		   (eq as-code (slot-code (get-as-result as-pat))))
	      nil)  ; statement has no effect, we are moving a slot to itself.
	     ((atom as-code)
	      `(value-to-slot ,(slot-code (get-as-result as-pat)) ,as-code))
	     (t
	      `(as-to-slot ,(slot-code (get-as-result as-pat)) ,as-code)))))
	       
	       
        ; internal-bind's only needed for unparser generation (finding
	; discriminators).  They do not really do anything for parser
	; generation. 
    (internal-bind nil)

    
    
    ;; internal as-type for unrolled loops.  Cons partial result into correct
    ;; slot for the loop.
    (piece-of-loop
     `(cons-to-slot ,(slot-code (get-as-second-son as-pat))
		    ,(slot-code (get-as-first-son as-pat))))
    ;; See documentation for gen-star-parse to understand why result will be a
    ;; term rather than a list.  (srd term revision, 12/86).
    
    ;; internal as-type -- we have unrolled a construct out of existence.
    ;; known-branch is used to set its flag slot.
    (known-branch
     `(value-to-slot ,(slot-code (get-as-second-son as-pat))
		     ,(get-as-first-son as-pat)))

        ; If the type is and --just deal with each son.
    (and
     (add-let (mapcar #'(lambda (x)
			  (as-parse-gen x gram-struct))
		      (get-as-args as-pat))))
       
        ; If the type is opt we have to build code that
        ; checks its key field to see which branch to
        ; call.
    (opt
     `(if (= ,(slot-code (get-as-key as-pat)) 0)
	  ,(as-parse-gen (get-as-first-son as-pat) gram-struct)
	  ,(as-parse-gen (get-as-second-son as-pat) gram-struct)))


        ; For an alt we have to check the key field and
        ; choose the right branch based in that value.   Some optimization
        ; here.
    (alt
     (let (branches as-code temp optimize-results)

       (do ((sons (get-as-args as-pat) (cdr sons)) (count 0 (+ count 1)))
	   ((null sons))

	 (setq as-code (as-parse-gen (car sons) gram-struct))

	     ; if as-code is an atom then we are just getting the value of some
	     ; slot.  Other branches may be doing the same thing so we want
	     ; to save the slot-name and the count. SO we can collapse all of
	     ; these branches into one.
	 (cond ((atom as-code)
		(setq temp (assoc as-code optimize-results))

		    ; if the slot-name has already been seen then just add 
		    ; the current branch number to the list keys.
		 (if temp (setf (cdr temp) (cons count (cdr temp)))
		          (push (list as-code count) optimize-results)))

		(t (push (list count as-code) branches))))

           ; special case -- if branches is empty then we only have slot name
	   ; references and if we only have one entry in optimize results
	   ; then everyone is refering to the same slot -- no need to build
	   ; a case.

       (cond ((and (null branches) (= (length optimize-results) 1))
	      (caar optimize-results))

	     (t (do ((entries optimize-results (cdr entries)))
		    ((null entries))

		  (if (= (length (car entries)) 2)
		      (push (reverse (car entries)) branches)
		      (push (list (cdar entries) (caar entries)) branches)))

		(append `(case ,(slot-code (get-as-key as-pat))) branches)))))

        ; The list operator says to build a default sequence 
        ; using the abstract syntax results from each son.

    ;; @@ Eventually this bracnh should go away
      
    (flatten
     `(let ((first ,(as-parse-gen (get-as-second-son as-pat) gram-struct))
	    (second ,(as-parse-gen (nth 2 (get-as-args as-pat)) gram-struct)))

	(make-term 
	 ,(get-term-const-op as-pat)
	 (append
	  (if (funcall ,(get-discriminator as-pat gram-struct) first)
	      (car (funcall ,(get-destructor as-pat gram-struct) first))
	      (list first))
	  (if (funcall ,(get-discriminator as-pat gram-struct) second)
	      (car (funcall ,(get-destructor as-pat gram-struct) second))
	      (list second))))))

    (t
     (sb-system-error))))
     
     ;; (cons (get-as-kind as-pat) (mapcar #'(lambda (x) (as-parse-gen  x
     ;; gram-struct)) (get-as-args as-pat))))))
     
    

