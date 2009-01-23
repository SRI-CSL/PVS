;;; -*- Mode: Lisp; Package: syntax-box; Log: sb-changes.log -*-
;;; Sccs Id @(#)look-ahead.lisp	1.4 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


(in-package :syntax-box)  (use-package :ergolisp)

(use-package :sb-runtime)




;  This module produces the look ahead sets (LAS) for all the noterminals in the
;  grammar.  Our look ahead sets are very similiar to LL sets except that we 
;  allow left recursion.
;
;  FORMAT OF LOOK AHEAD SETS:
;   Two kinds of symbols can be in an LAS, keywords which have the form 
;   (keyword <string>), and lexical terminals which look like
;   (lexical-terminal <string>).
;
;   Each entry in an LAS is a list in which the first element is a
;   first symbol and all subsequent elements are possible second symbols.
;              (<first-symbol . second-symbol>*).
;
;  HOW GRAMMAR CONSTRUCTS MAP INTO LAS:
;  Look-ahead(A), where A is an member of L(G) (where G is the grammar grammar),
;   is the look ahead set for A.  First-symbols(L) where L is a look ahead set
;   is just a list of the first symbols in that set.
;   NB: we use + to mean set union.
;
;   A-LAS == look-ahead(A)     k is a keyword
;   B-LAS == look-ahead(B)
;
;   look-ahead(keyword) = '(keyword <name-of-keyword>)
;
;   look-ahead(lexical-terminal) == '(lexical-terminal <name-of-lt>)
;
;   look-ahead(nt@grammar) -- Get the look-ahead sets for grammar out
;                             of *ext-look-ahead*, find NT in that.
;
;   look-ahead( {A B} ) -- The look ahead set for a sequence is made by 
;			   combining the two look ahead sets.  A-LAS forms the
;			   beginning of the new set.
;                          If any of the members of the new set
;			   have epsilon as a possible second symbols then 
;			   the epsilon is replaced with First-symbols(B-LAS).
;                          If epsilon is a member of A-LAS then B-LAS is 
;			   appended to the new set.  
;
;
;   look-ahead( A jux B ) -- same as above sequence case except that 
;                            A and B are known to be either nonterminals
;                            or lexical terminals.
;
;   look-ahead( [A] ) == lookahead(A) + {epsilon}
;
;   look-ahead( {A | B | C} ) == look-ahead(A) + look-ahead(B) + look-ahead(C)
;
;   look-ahead( {A}* ) = {epsilon} + look-ahead( {A}+ )
;
;   look-ahead( {A}+ ) == look-ahead( {A {A}*})
;
;   look-ahead( {A **  k} ) == look-ahead( {A ++ k} ) + {epsilon}
;
;   look-ahead( {A ++ k} ) == look-ahead( {A k {A ++ k}} ) + look-ahead(A)
;
;
;  ALGORITHM FOR FINDING LAS:
;   For each nonterminal
;	flatten the definition (see flatten.l)
;	build a list of first/second grammar symbols pairs 
;   	add an entry to the dependency GRAPH for each production of the
;            (the code for the graph functions is in aux-functions.slip)
;  	form NT -> NT1 w, where NT, NT1 are nonterminals, NT <> NT1 and
;	    w is a member of L(G).
;
;   find the strongly connected components in GRAPH -- Note: All of the 
;   nonterminals in a strongly connected component have the same look-ahead-set.
;
;   for each component build the list of first symbols (get the info from the
;	first/second grammar symbol pairs)
;
;   for each componet build the look ahead set using the first symbols and the
;	f/s pairs.
;
 

; Macros for getting into the structure of *look-ahead-sets*


(defun make-epsilon ()
    'epsilon)
    
(defun make-lt-pair (name gram-struct)
  (list (is-lexical-terminal name gram-struct) 'epsilon))

(defun make-lt (name gram-struct)
  (is-lexical-terminal name gram-struct))

(defun make-nt-pair (name)
   `((nonterminal ,name) epsilon))

(defun make-nt (name) 
  `(nonterminal ,name))

(defun is-nt (x)
  (and (listp x)
       (eq (car x) 'nonterminal)))

(defun make-ext-nt (nt-name grammar-name)
  `(ext-nonterminal ,grammar-name ,nt-name))

(defun make-ext-nt-pair (nt-name grammar-name)
   `( (ext-nonterminal ,grammar-name ,nt-name) epsilon))

(defun is-ext-nt (x)
  (and (listp x)
       (eq (car x) 'ext-nonterminal)))

(defun make-keyword-pair (name)
  (list (sbst-intern-case name) 'epsilon))	; srd revised. 

(defun make-keyword (name)
  (sbst-intern-case name))			; srd revised. 

(defun make-pair (x y)
   (list x y))

(defun make-singleton-lt (name gram-struct)
    `((,(is-lexical-terminal name gram-struct) epsilon)))

(defun make-singleton-keyword (name)
    `((,(sbst-intern-case name) epsilon)))


(defun leading-epsilon-p (fs-list)
    (assoc 'epsilon fs-list))

(defun second-epsilon-p (fs-entry)
  (or (member 'epsilon (cdr fs-entry))
      (= (length fs-entry) 1)))


(defun check-second-epsilon (fs-list)
  (let (result)
    (do ((entries fs-list (cdr entries)))
	((null entries) result)

      (setq result (or result (second-epsilon-p (car entries)))))))



(defun recursive-p (pat name)
  (and (eq (get-kind pat) 'nonterminal)
       (eq (get-first-son pat) name)))


(defun get-look-ahead (nt)
  (if (eq (car nt) 'nonterminal)
      (cadr (assoc (cadr nt) *look-ahead-sets*))
          ; find grammar in ext-look-ahead.  find nt in that.
      (let ((temp (cadr (assoc (nth 2 nt)
			       (cadr (assoc (cadr nt) *ext-look-ahead*))))))
	(if (null temp) (my-error 35 nt) temp))))



; (defvar *first-results* nil "Intermediate list where first sets are stored")

(defun get-first-list (nt)
  (if (eq (car nt) 'nonterminal)
      (cadr (assoc (cadr nt) *first-results*))
          ; find grammar in ext-look-ahead.  find nt in that.
      (let ((temp (cadr (assoc (nth 2 nt)
			       (cadr (assoc (cadr nt) *ext-first-sets*))))))
	(if (null temp) (my-error 35 nt) temp))))




(defun equal-union (x y)
   (union x y :test #'equal))


; (defvar *nt-lst* nil "Intermediate list where look-nti structures are stored")

;;; Generate Look Ahead

(defun generate-look-ahead (gram-struct)
  (let ((nt-list (get-nt-list gram-struct))
	(component-list nil))
    
    ;; initialize graph - add each nonterminal to the graph.
    (setq *dependency-graph* (list nil))
    (init-graph *dependency-graph* (mapcar #'(lambda (x) (nt-name x))
					   nt-list))
    
    ;; for each nonterminal find first symbol information.  *Nt-Lst* is the
    ;; resulting list -- it has the form (<NT-Name> <Look-nti structure>)*
    (setq *nt-lst* nil)
    (mapcar #'(lambda (x) (first-symbols x gram-struct)) nt-list)
    
    ;; find the strongly connected components
    (setq component-list (scf *dependency-graph*))
    
    ;; Build the first list for each nonterminal.  Initialize *first-results* -
    ;; the resulting list of (<nt-name> <first list>)*.  Note:  we only build
    ;; the first-sets for local nonterminals.  External nonterminal first-sets
    ;; come from the aux-files.
    (setq *first-results* nil)
    (mapcar #'(lambda (x) (if (eq (caar x) 'nonterminal)
			      (generate-firsts-for-components x)))
	    component-list)
    
    ;; Build look ahead sets for each nonterminal.  Results are in
    ;; *look-ahead-sets* -- (<nt-name> look-ahead-set)*.  Again, only build
    ;; look-ahead for local nonterminals.
    (setq *look-ahead-sets* nil)
    (mapcar #'(lambda (x) (if (eq (caar x) 'nonterminal)
			      (generate-component-las x)))
	    component-list)))

    



; First-symbols:
;   nt-def -- Nt-Name x NT-information -> NT-name x Look-NTI
;
;   If a nonterminal has more than one production, i.e. it is an
;   alternation, then call alt-first-symbols.  If it is a singleton then
;   call single-first-symbols.
;
;   Each of the first symbol routines produces an entry for *NT-LST*.  An entry
;   in *NT-LST* looks like <Name Look-NTi structure>.  A look-nti structure 
;   is composed of
;	pairs -- a list of first/second grammar symbols
;	fsymbols -- a list of leading token (lexical terminals and keywords)
;	fnt -- a list leading nonterminals not including recursive
;	   instances of the current nonterminal
;	msymbols -- list of medial token.  Medials are the guys that 
;	   follow leading recursive nts.
;	mnt -- list of medial nonterminals.

(defun first-symbols (nt-def gram-struct)
   (setq *current-nt* (nt-name nt-def))
   (cond ((eq (get-kind (nt-pattern nt-def)) 'alt)
	  (alt-first-symbols nt-def gram-struct))
	 (t (single-first-symbols nt-def gram-struct))))



; Alt-First-Symbols:
;   Nt-Name x NT-inforamtion -> NT-Name x Look-NTI
;
;   find the first symbols for a nonterminal with more than one production.
;
(defun alt-first-symbols (nt-def gram-struct)
  (let* ((name (nt-name nt-def))	; srd revised
	 (name-vertex (make-nt name))
	 (def-pat (nt-pattern nt-def))	; srd revised
	 (result (get-result def-pat))
	 (pat-as (get-as def-pat))
	 (pairs nil) (fsymbols nil) (first-nt nil) (msymbols nil)
	 (medial-nt nil) (new-def nil))
    ;; loop for each production
    (do ((alts (get-sons def-pat) (cdr alts)) (count 0 (+ count 1))
	 (alt) (flattened-patterns) (recursive))
	((null alts))

      (setq alt (car alts))
	  ; Flatten the production.  NB: we know that a flattened production
	  ; will either be a singleton keyword, nonterminal, ext-nonterminal,
          ; or juxtaposition or it will be
	  ; a sequence with atomic first and second symbols (atomic means
	  ; keyword, nonterminal, juxtaposition, or ext-nonterminal)

      (setq flattened-patterns (flatten-alternative alt))

	  ; for each new production
      (do ((fpats flattened-patterns (cdr fpats))
	   (current-pattern) (first-son) (cpat-kind))
	  ((null fpats))
	(setq current-pattern (car fpats)
	      cpat-kind (get-kind current-pattern)
	      first-son (get-first-son current-pattern))
	(ecase cpat-kind
	 (nonterminal ; singleton nonterminal
	      ; if it is a lexical terminal add (lt epsilon) to f/s pairs
	      ; and add lt to the first symbols
	  (cond ((is-lexical-terminal first-son gram-struct)
		 (push (make-lt-pair first-son gram-struct) pairs)
		 (push (make-lt first-son gram-struct) fsymbols))
		
		    ; we can not have a singleton recursive instance
		    ; of the nonterminal
		((eq first-son name)
		 (my-error 19))
	    
		    ; for a regular nonterminal add (NT epsilon) to f/s pairs
		    ; Add Nt to the leading nts.  And add an edge from
		    ; current-NT -> NT to the dependency graph
		(t (let ((entry (make-nt first-son)))
		     (push (make-pair entry 'epsilon) pairs)
		     (push entry first-nt)
		     (addedge name-vertex entry *dependency-graph*)))))

	     ; singleton external nonterminal.  Add (ENT epsilon)
	     ; to fs-pairs.  Add ENT to leading nonterminal list.
	     ; and add an edge from Current-NT -> ENT.
	 (ext-nonterminal
	  (let ((entry (make-ext-nt first-son
				    (get-second-son current-pattern))))
	    (push (make-pair entry 'epsilon) pairs)
	    (push entry first-nt)
	    (addedge name-vertex entry *dependency-graph*)))
	 ;; singleton keywords.  Add (keyword epsilon) to f/s pairs and
	 ;; add keyword to the first symbols
	 (ukeyword
	  (push (make-keyword-pair first-son) pairs)
	  (push (make-keyword first-son) fsymbols))	 
	     ; singleton juxtaposition or leading jux
	 ((jux seq internal-seq)
	  (if (or (eq cpat-kind 'jux)
		  (and (member cpat-kind '(seq internal-seq))
		       (eq (get-kind first-son) 'jux)))
	  (let* ((left (if (eq cpat-kind 'jux)
			   first-son
			   (get-first-son first-son)))
		 (left-fs (get-first-son left))

		 (right (if (eq cpat-kind 'jux)
			    (get-second-son current-pattern)
			    (get-second-son first-son)))
		 (right-fs (get-first-son right)))


	    (cond
	         ; right side of jux must be recursive
	     ((eq (get-kind left) 'ext-nonterminal)
	      (if (recursive-p right name)
		  (let ((entry (make-ext-nt left-fs (get-second-son left))))
		    (push (make-pair entry name-vertex) pairs)
		    (push entry first-nt)
		    (addedge name-vertex entry *dependency-graph*))

		  (my-error 36)))

	         ; right side of jux must be recursive.
	     ((lt-p left gram-struct)
	      (if (recursive-p right name)
		  (let ((entry (make-lt left-fs gram-struct)))
		    (push (make-pair entry name-vertex) pairs)
		    (push entry fsymbols))

		  (my-error 36)))


	         ; if left side is recursive, then the right side need not be.
	     ((recursive-p left name)
	      (cond ((eq (get-kind right) 'ext-nonterminal)
		     (push (make-ext-nt right-fs (get-second-son right))
			   medial-nt))
		    ((lt-p right gram-struct)
		     (push (make-lt right-fs gram-struct) msymbols))
		    (t (push (make-nt right-fs) medial-nt))))
	         ; if left side is not recursive, the right side must be.
	     (t (if (recursive-p right name)
		    (let ((entry (make-nt left-fs)))
		      (push (make-pair entry name-vertex) pairs)
		      (push entry first-nt)
		      (addedge name-vertex entry *dependency-graph*))
		    (my-error 36)))))
	  (let* ((fs-first-son (get-first-son first-son))
		 (second (get-second-son current-pattern))
		 ;; If second son is a sequence we need its first
		 ;; son...which is the second grammar symbol.
		 (second-son (if (member (get-kind second) '(seq internal-seq))
				 (get-first-son second)
				 second))
		 (ss-kind (get-kind second-son))
		 ;; the second sons entry for pairs, etc.
		 (sse
		  (cond ((lt-p second-son gram-struct)
			 (make-lt (get-first-son second-son)
				  gram-struct))
			((eq ss-kind 'nonterminal)
			 (make-nt (get-first-son second-son)))
			((eq ss-kind 'ext-nonterminal)
			 (make-ext-nt (get-first-son second-son)
				      (get-second-son second-son)))
			((eq ss-kind 'jux)
			 (let* ((left (get-first-son second-son))
				(left-fs (get-first-son left)))
			   (cond ((eq (get-kind left) 'external-nonterminal)
				  (make-ext-nt left-fs (get-second-son left)))
				 
				 ((lt-p left gram-struct)
				  (make-lt left-fs gram-struct))

				 (t (make-nt left-fs)))))
			((eq ss-kind 'just-as) (make-epsilon))
			(t (make-keyword (get-first-son second-son))))))
	    ;; if the first son is a nonterminal
	    (cond ((eq (get-kind first-son) 'nonterminal)
		       ; it could be a lexical terminal then add
		       ; (lt sse) to f/s pairs and add lt to first
		       ; symbols.
		   (cond ((is-lexical-terminal fs-first-son gram-struct)
			  (push (make-pair (make-lt fs-first-son gram-struct)
					   sse)
				pairs)
			  (push (make-lt fs-first-son gram-struct) fsymbols))

			    ; if it is a recursive instance then save sse
			    ; in the correct medial list
			 ((eq fs-first-son name)
			  (setq recursive t)
			  (cond ((or (lt-p second-son gram-struct)
				     (eq (get-kind second-son) 'just-as)
				     (eq (get-kind second-son) 'ukeyword))
				 (push sse msymbols))
				;; error -- A :: A B .... should be
				;; using a jux.  Otherwise there is no op
				;; for the parser to test.
				(t (my-error 37)
				   (push sse medial-nt))))
			 ;; If it is a normal nonterminal add (NT sse)
			 ;; to f/s pairs, and NT to leading nts, and
			 ;; add an edge from NT -> fs-first-son to G
			 (t (let ((entry (make-nt fs-first-son)))
			      (push (make-pair entry sse) pairs)
			      (push entry first-nt)
			      (addedge name-vertex entry *dependency-graph*)))))

		      ; leading external nonterminal -- same as singleton
		      ; case except that the pair has an entry for the
                      ; second son of the sequence.
		  ((eq (get-kind first-son) 'ext-nonterminal)
		   (let ((entry (make-ext-nt fs-first-son
					     (get-second-son first-son))))
		     (push (make-pair entry sse) pairs)
		     (push entry first-nt)
		     (addedge name-vertex entry *dependency-graph*)))

		      ; or it is a keyword.  Just add (keyword sse) to f/s
		      ; pairs and add keyword to the first symbols
		   
		  (t (push (make-pair (make-keyword fs-first-son) sse) pairs)
		     (push (make-keyword fs-first-son) fsymbols))))))))

      ;; If the nt definition pattern has abstract syntax associated with
      ;; then we need to tag each branch with an abstract syntax
      ;; constructor that tells us which production we are looking at.
      ;; If the production is recursive then we need to keep the 
      ;; flattened version of it.  
      (cond (pat-as
	     (cond (recursive
		    (mapcar #'(lambda (x) (add-known-branch count result x))
			    flattened-patterns)
		    (setq new-def (append new-def flattened-patterns)))
		   (t (add-known-branch count result alt)
		      (push alt new-def))))

	    (recursive
	     (setq new-def (append new-def flattened-patterns)))
	    (t (push alt new-def))))
        ; Save the new productions in this NT's entry in NT-list.
    (set-nt-pattern nt-def		; srd revised
		    (make-pattern :kind 'alt
				  :augment (get-as def-pat)
				  :slot (get-result def-pat)
				  :sons new-def))


        ; Handle brackets -- Save the opening bracket in first symbols and
        ; add a pair for the implicit production.  open NT close
					; srd revised, new grammar structure. 
    (let ((left-bracket
	   (get-nt-bracket name *grammar-term* 'left-bracket)))
      (cond (left-bracket
	     (push (make-keyword left-bracket)
		   fsymbols)
	     (push (make-pair (make-keyword left-bracket) (make-nt name))
		   pairs))))
					; end revision. 

       ; Save (NT-name look-NTI) to *nt-lst*
    (push (list
	   name
	   (make-look-nti :pairs (remove-duplicates pairs :test #'equal)
			  :fsymbols (remove-duplicates fsymbols :test #'equal)
			  :fnt (remove-duplicates first-nt :test #'equal)
			  :msymbols (remove-duplicates msymbols :test #'equal)
			  :mnt (remove-duplicates medial-nt :test #'equal)))
	  *nt-lst*)))


; Single-first-symbols
;    Nt-name x Nt-information -> NT-name x Look-NTI
;
;    This is the singleton production version.  It is identical to the 
;    alt version except that we flatten def-pat instead of the sons of def-pat
;    and we do not need to add known-branch constructors to the branches.

(defun single-first-symbols (nt-def gram-struct)
  (let* ((name (nt-name nt-def))	; srd revised
	 (name-vertex (make-nt name))
	 (def-pat (nt-pattern nt-def))	; srd revised
	 (pairs nil) (fsymbols nil) (first-nt nil) (msymbols nil)
	 (medial-nt nil) (new-def nil) (recursive nil) (flattened-patterns nil))

    (setq flattened-patterns (flatten-alternative def-pat))

    (do ((fpats flattened-patterns (cdr fpats)) (current-pattern) (first-son))
	((null fpats))

      (setq current-pattern (car fpats))
      (setq first-son (get-first-son current-pattern))

      (case (get-kind current-pattern)

	(nonterminal
	 (cond ((is-lexical-terminal (get-first-son current-pattern)
				     gram-struct)
		(push (make-lt-pair first-son gram-struct) pairs)
		(push (make-lt first-son gram-struct) fsymbols))

	       ((eq first-son name)
		(my-error 19))
	    
	       (t (let ((entry (make-nt first-son)))
		    (push (make-pair entry 'epsilon) pairs)
		    (push entry first-nt)
		    (addedge name-vertex entry *dependency-graph*)))))

	(ext-nonterminal
	 (let ((entry (make-ext-nt first-son
				   (get-second-son current-pattern))))
	   (push (make-pair entry 'epsilon) pairs)
	   (push entry first-nt)
	   (addedge name-vertex entry *dependency-graph*)))

	(ukeyword
	 (push (make-keyword-pair first-son) pairs)
	 (push (make-keyword first-son) fsymbols))

	(jux (my-error 38))

	((seq internal-seq)
	 (let* ((fs-first-son (get-first-son first-son))
		(second (get-second-son current-pattern))
					; second son
		(second-son (cond ((memq (get-kind second)
					 '(seq internal-seq))
				   (get-first-son second))
				  (t second)))
		(ss-kind (get-kind second-son))

		(sse
		 (cond ((lt-p second-son gram-struct)
			(make-lt (get-first-son second-son) gram-struct))

		       ((eq ss-kind 'nonterminal)
			(make-nt (get-first-son second-son)))

		       ((eq ss-kind 'ext-nonterminal)
			(make-ext-nt (get-first-son second-son)
				     (get-second-son second-son)))

		       ((eq ss-kind 'just-as) (make-epsilon))

		       ((eq ss-kind 'ukeyword)
			(make-keyword (get-first-son second-son)))

		       (t (my-error 38)))))

	   (cond ((eq (get-kind first-son) 'nonterminal)
		  (cond ((is-lexical-terminal fs-first-son
					      gram-struct)
			 (push (make-pair (make-lt fs-first-son gram-struct)
					  sse)
			       pairs)
			 (push (make-lt fs-first-son gram-struct) fsymbols))

			((eq fs-first-son name)
			 (setq recursive t)
			 (cond ((or (lt-p second-son gram-struct)
				    (eq (get-kind second-son) 'just-as)
				    (eq (get-kind second-son) 'ukeyword))
				(push sse msymbols))
			       (t (push sse medial-nt))))

			(t (let ((entry (make-nt fs-first-son)))
			     (push (make-pair entry sse) pairs)
			     (push entry first-nt)
			     (addedge name-vertex entry *dependency-graph*)))))

		 ((eq (get-kind first-son) 'ext-nonterminal)
		  (let ((entry (make-ext-nt fs-first-son
					    (get-second-son first-son))))
		    (push (make-pair entry sse) pairs)
		    (push entry first-nt)
		    (addedge name-vertex entry *dependency-graph*)))

		 (t (push (make-pair (make-keyword fs-first-son) sse) pairs)
		    (push (make-keyword fs-first-son) fsymbols)))))

	(t (my-error 23))))

	(if recursive (setq new-def flattened-patterns))


    (cond ((> (length new-def) 1)
	   (set-nt-pattern nt-def	; srd revised
			   (make-pattern :kind 'alt
					 :augment (get-as def-pat)
					 :slot (get-result def-pat)
					 :sons new-def))))


        ; Handle brackets -- Save the opening bracket in first symbols and
        ; add a pair for the implicit production.  open NT close
					; srd revised, new grammar structure. 
    (let ((left-bracket
	   (get-nt-bracket name *grammar-term* 'left-bracket)))
      (cond (left-bracket
	     (push (make-keyword left-bracket)
		   fsymbols)
	     (push (make-pair (make-keyword left-bracket) (make-nt name))
		   pairs))))
					; end revision. 


       ; Save results

    (push (list
	   name
	   (make-look-nti :pairs (remove-duplicates pairs :test #'equal)
			  :fsymbols (remove-duplicates fsymbols :test #'equal)
			  :fnt (remove-duplicates first-nt :test #'equal)
			  :msymbols (remove-duplicates msymbols :test #'equal)
			  :mnt (remove-duplicates medial-nt :test #'equal)))
	  *nt-lst*)))



; Add-known-branch:
;  Number x Number x Pattern -> Pattern
;
;  Add a known branch constructor to the abstract syntax of the pattern. 

(defun add-known-branch (count result pat)
  (let (temp-as)
    (cond ((get-as pat) (push (get-as pat) temp-as)))
    (push (known-branch  count result) temp-as)
    
    (set-pattern-augment pat (make-and temp-as))))


; Generate-firsts-for-components
;   Component -> {NT-Name x first-symbols}*

(defun generate-firsts-for-components (c)
  (let (component-fsymbols)

    ; For each nonterminal in the component
    (do ((nodes c (cdr nodes)) (node))
	((null nodes))

          ; Get the look-nti structure for this nonterminal
      (setq node (cadr (assoc (cadr (car nodes)) *nt-lst*)))
      (if (null node)
	  (my-error 24 (cadr (car nodes)))

	  (prog ()
	        ; add this nonterminals first symbols to the component's list
	    (setq component-fsymbols
		  (union component-fsymbols (look-nti-fsymbols node)))

	        ; For each NT in the leading nonterminal list add its first symbols
	        ; to the component's first symbol list.
	    (mapcar
	     #'(lambda (x)
		 (cond ((not (member x c))
			(setq component-fsymbols
			      (union component-fsymbols (get-first-list x))))))
	     (look-nti-fnt node)))))

       ; for each nonterminal in the component add (Name x component-first-symbols)
       ; to *first-results*.
    (mapcar
     #'(lambda (x) (push (list (cadr x) component-fsymbols)
			 *first-results*))
     c)))



; Generate-Component-LAS
;  Component -> {Nt-name x first-symbols}*

(defun generate-component-las (c)
  (let (component-las epsilon-list epsilon-symbols new-las)

        ; For each nonterminal in the component
    (do ((nodes c (cdr nodes)) (node))
	((null nodes))

          ; get the look-nti structure for this nonterminal.
      (setq node (cadr (assoc (cadr (car nodes)) *nt-lst*)))
      (if (null node)
	  (my-error 24 (cadr (car nodes)))

	  (prog ()

	        ; add entry generated by each pair to the component look-ahead-set
	        ; epsilon-symbols is a list of symbols that can replace epsilon
	        ; as a possible second.  (medial symbols)
	    (multiple-value-setq (epsilon-symbols component-las)
	      (deal-with-pairs (look-nti-pairs node) c component-las))

	        ; save the appropriate symbols in epsilon-list -- save all of the
	        ; symbols that follow recursive instances of the nonterminal.
	    (setq epsilon-list
		  (reduce #'union
			  (list  epsilon-list
				 epsilon-symbols
				 (look-nti-msymbols node)
				 (deal-with-medial-nts (look-nti-mnt node))))))))
      

       ; Because we have recursive nonterminal definitions anywhere we see
       ; an epsilon as a possible second symbol we can add all of the
       ; symbols in epsilon-list as possible seconds too.
 
       ; For each entry in the component look ahead set
    (do ((entries component-las (cdr entries)))
	((null entries))
	   
          ; if epsilon is a possible second then add the members of epsilon-list
          ; to the seconds list as well.
      (cond ((member 'epsilon (car entries))
	     (setq new-las
		   (add-entry new-las
		      (cons (caar entries)
			    (union (cdar entries) epsilon-list)))))

	        ; an entry of length one has an implicit epsilon as a second
	        ; make epsilon-list its second list.  Make sure to add the epsilon
	        ; explicitly.
	    ((= (length (car entries)) 1)
	     (setq new-las
		   (add-entry new-las
			      (append `(,(caar entries) epsilon) epsilon-list))))

	       ; otherwise just save the entry.
	    (t (setq new-las (add-entry new-las (car entries))))))

        ; Save (Name x Look-ahead-set) for each nonterminal
        ; in the component.
      	    (mapcar #'(lambda (x) (push (list (cadr x) new-las) *look-ahead-sets*))
	    c)))


; Deal-with-medial-nts. 
;  NT-Name* -> first-symbols

(defun deal-with-medial-nts (mnt)
      ; make a list of all the first symbols.
  (cond (mnt (reduce #'equal-union (mapcar #'get-first-list mnt)))))


; Deal-with-pairs:
;   Pair* x component x LAS -> LAS

(defun deal-with-pairs (pair-list c component-las)
  (let (epsilon-list)

       ; For each pair
    (do ((pairs pair-list (cdr pairs)) (x) (y))
	((null pairs))

      (setq x (caar pairs))
      (setq y (cadar pairs))
         ; y is a list of possible seconds.
      (setq y (cond ((eq y 'epsilon) '(epsilon))
		    ((or (is-nt y) (is-ext-nt y))
		     (get-first-list y))
		    (t (list y))))

         ; IF x is a member of C then the members of y belong to epsilon-list.

      (cond ((and (is-nt x)
		  (member x c :test #'equal))
	     (setq epsilon-list (union epsilon-list y)))
	        ; otherwise deal with the pair.
	    (t (setq component-las
		     (deal-with-single-pairs x y component-las)))))

    (values epsilon-list component-las)))


; Deal-with-single-pair:  Symbol x Symbol* x LAS -> LAS
; Add a new entry to LAS for <x y>

(defun deal-with-single-pairs (x y component-las)

      ; If x is a nonterminal or an external nonterminal then y gets filled
      ; in for all epsilon that appear as second symbols.

  (cond ((or (is-nt x) (is-ext-nt x))
	 (do ((entries (get-look-ahead x) (cdr entries)))
	     ((null entries))
	   
	   (cond ((member 'epsilon (car entries))
		  (setq component-las
		     (add-entry component-las
		      (cons (caar entries)
			    (union (cdr (remove 'epsilon (car entries))) y)))))
	
		     ; implicit epsilon
		 ((= (length (car entries)) 1)
		  (setq component-las
			(add-entry component-las (cons (caar entries) y))))

		 (t (setq component-las
			  (add-entry component-las (car entries)))))))

	    ; otherwise just add the pair entry to the LAS.
	(t (setq component-las (add-entry component-las (cons x y)))))
  component-las)


; Add-entry:
;   LAS x <x y> -> LAS
;
(defun add-entry (las entry)
  (let (temp)

        ; If x already has an entry then add y to the list of possible
        ; seconds.  If not add a new entry to LAS.
    (cond ((setq temp (assoc (car entry) las))
           (setq las
		 (cons (append temp (cdr entry))
		       (remove temp las))))
	  (t (push entry las)))
    las))

