;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)aux-funs.lisp	1.10 2/8/90
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; File containing all syntax box global varaibles which are shared between
;;; files. 

;;; Scott Dietzen, Mon Oct 13 16:05:43 1986


(in-package 'syntax-box)  (use-package :ergolisp)


(defparameter *sb-package* (find-package :sb))

(use-package '(sb-runtime))
(import '(*sbst-package*))








;;; Global variable set to the grammar structure. 
(defvar *grammar-term* nil)


;;; Global variable whether target language is case-sensitive. 
(defvar *grammar-case-sensitive?* nil)


;;; *parser-gen-error* is True when there has been an error found
;;; while generating the parser.
(defvar *parser-gen-error* nil)

;;; The name of the current nonterminal.
(defvar *current-nt* nil)

(defvar *global-function-list* nil "going away soon")

;;; Code that deals with these may be found in look-ahead.slisp, but they
;;; are referenced and initialized in this file.
(defvar *dependency-graph* nil "Dependency graph")

(defvar *look-ahead-sets* nil)

;;; Code that deals with this may be found in phase-three.slisp, but it
;;; is referenced in this file.
(defvar *keyword-graph* nil)

(defvar *ext-brackets* nil)
(defvar *ext-package-names* nil)
(defvar *ext-look-ahead* nil)
(defvar *ext-first-sets* nil)
(defvar *ext-lexer-information* nil)
(defvar *ext-operators* nil)
(defvar *ext-used-operators* nil)

;;; This is the package name for the IN-PACKAGE call in the generated files.
(defvar *code-package-spec* nil)

;;; This is the package name for the abstract syntax produced by the client.
(defvar *abs-syn-package-spec* nil)

;;; The lisp package of name *abs-syn-package-spec*.
(defvar *abs-syn-lisp-package* nil)


;;; This is the sequence of package names for the USE-PACKAGE call in the
;;;  generated files.
(defvar *use-packages-spec* nil)

;;; The following are set to symbols that are the names of constants
;;; and specials in the generated lexer and parser files.  These are computed
;;; by taking the value *sb-conc-name-upcased*, concatenating it with strings
;;; like "-KEYWORD-LIST", "-LEXICAL-READTABLE", etc, and interning them in
;;; the SB's package, so they will be written out unqualified with the use
;;; of SB-WRITE.  These are used in this file and lexer-gen.slisp.
(defvar *conc-keyword-list*)
(defvar *conc-single-char-op-list*)
(defvar *conc-multi-char-op-list*)
(defvar *conc-all-operators-list*)
(defvar *conc-new-line-comment-char*)
(defvar *conc-open-comment-char*)
(defvar *conc-close-comment-char*)
(defvar *conc-escape-char*)
(defvar *conc-case-sensitive*)
(defvar *conc-lexical-readtable*)
(defvar *conc-keyword-table*)
(defvar *conc-abs-syn-package*)
(defvar *conc-lexer-init-p*)
(defvar *conc-restricted-chars*)

(defvar *conc-string-char*)
(defvar *conc-literal-char*)
(defvar *conc-keyword-char*)



(defvar *global-brackets* ())

(defvar *global-lexer-information* nil)

(defvar *global-operator-list* nil)


(defvar *first-results* nil "Intermediate list where first sets are stored")
(defvar *nt-lst* nil "Intermediate list where look-nti structures are stored")


(defvar *top-level-alts* nil)


(defvar *unparser-gen-error* nil)




(eval-when (compile eval load)
(defmacro sb-write (&rest write-calls)
  `(let ((*package* *sb-package*)
	 (*print-level* nil)
	 (*print-length* nil)
	 (*print-circle* nil)
	 (*print-case* :downcase))	; Accomodates a LISP compilation bug in
					; version 2.1.1 (remove when fixed.)
     ,@write-calls)))


(defun save-function (function)
    (push function *global-function-list*))

(defun make-function-name (name &optional name1)
  (if name1
      (let ((package (cadr (assoc name *ext-package-names*)))
	    (sym (sb-intern-ncase
		  (concatenate 'simple-string "!" (symbol-name name1) "!"))))
	(if package
	    (intern (symbol-name sym)
		    (or (find-package package) (make-package package)))
	    (my-error 34 name)))
      (sb-intern-ncase
       (concatenate 'simple-string "!" (symbol-name name) "!"))))




;;; NT information for lookahead set generation

(defstruct (look-nti (:print-function print-look-nti))
  (pairs ())
  (fsymbols ())
  (fnt ())
  (msymbols ())
  (mnt ()))

(defun print-look-nti (obj str i)
  (declare (ignore i obj))
  (format str "#<Look-Nti>"))



; Dependency Graph Stuff
;
;   During look-ahead set generation we need a graph that represents
;   the dependencies between the nonterminals.  For our purposes a nonterminal A
;   is dependent on a nonterminal B if B is a leading symbols in one of
;   A's productions.  (for example, A ::= B ...). 

;   The graph structure is a list containing one element which is a list
;   of the form (NT-name . NT-name) where the leading nonterminal is the node
;   and the list of nt-names is the list of nonterminals that the current node
;   depends on.  So for our example above the graph would look like --
;
;                      ( ( (A B) ) ).
;
;   vertices have the form (nonterminal <name>) or (ext-nonterminal <name>).
;   all the code had to be changed to incorporate this.


; Init-graph -- make each nonterminal in nodes a node in the graph G.

(defun init-graph (g nodes)
  (do ((guys nodes (cdr guys))
       temp)
      ((null guys) (setf (car g) temp))
      (push (list (list 'nonterminal (car guys))) temp)))



; Addedge: Nt-name x Nt-name x G -> G  -- add an edge from NT1 to NT2 in G

(defun addedge (nt1 nt2 g)
  (let ((temp (assoc nt1 (car g) :test #'equal)))

    (if (not (member nt2 temp :test #'equal))
	(setf (cdr temp) (cons nt2 (cdr temp))))))

; Adj: Nt-name x G --> Nt-name*  -- Return the list of nodes that node points to.

(defun adj (node g)
  (cdr (assoc node (car g) :test #'equal)))


; Strongly Connected Component Stuff
;  ref: Depth First Search and Linear Graph Algorithms
;       Robert Tarjan, Siam J. Comput. V1, N2, june 1972 (pg 146)
;
;

; Globals used by SCC stuff.

(defvar *num-list* nil)
(defvar *ll-list* nil)
(defvar *count* nil)
(defvar *scstack* nil)
(defvar *final-result* nil)

(defun scf (g)
 (let ((nodes (mapcar #'car (car g))))

   (setq *num-list* nil)
   (setq *ll-list* nil)
   (setq *scstack* nil)
   (setq *final-result* nil)
   (setq *count* 1)

   (do ((vertices nodes (cdr vertices)))
       ((null vertices))
     (if (null (num (car vertices))) (sc (car vertices) g)))
   (reverse *final-result*)))



(defun sc (v g)
  (num v *count*)
  (ll v *count*)
  (setq *count* (+ *count* 1))
  (push v *scstack*)
  (do ((avert (adj v g) (cdr avert)) (w))
      ((null avert))

    (setq w (car avert))
    (cond ((null (num w))
	   (sc w g)
	   (ll v (min (ll v) (ll w))))
	  ((and (< (num w) (num v)) (member w *scstack* :test #'equal))
	   (ll v (min (num w) (ll v))))))

  (cond ((= (num v) (ll v))
	 (let (result)
	   (do ()
	       ((equal (car *scstack*) v))
	     (push (car *scstack*) result)
	     (setq *scstack* (cdr *scstack*)))
	   (push v result)
	   (setq *scstack* (cdr *scstack*))
	   (push result *final-result*)))))

(defun num (node &optional number)
  (cond (number (push `(,node ,number) *num-list*))
	(t (cadr (assoc node *num-list* :test #'equal)))))

(defun ll (node &optional number)
  (cond (number (push `(,node ,number) *ll-list*))
	(t (cadr (assoc node *ll-list* :test #'equal)))))


; Reduce-graph -- takes a dependency graph and reduces each strongly
;  connected component to a single vertex.  New graph has the structure
;  (Label list-of-vertices in the component edge-list)
;  The vertex list contain the names of the vertices from the dep. graph.

(defun reduce-graph (g)
            ; find strongly connected components (scf returns them in
            ; topologoical order)
  (let ((scc (scf g))
	(numbers nil)
	(reduced-graph nil))

        ; Number each component and add an entry for each of its
        ; vertices to numbers  (vertex component-number)
    (do ((vertices scc (cdr vertices))
	 (count 0 (+ count 1)))
	((null vertices))

      (mapcar #'(lambda (x) (push (list x count) numbers)) (car vertices)))

        ; For each component find the new names of its adjacent vertices.
        ; Remember to remove all of the members of the component from
        ; the list.
    (do ((vertices scc (cdr vertices)))
	((null vertices))


      (push (list (cadr (assoc (caar vertices) numbers :test #'equal))
		  (car vertices)
		  (mapcar 
		   #'(lambda (x) (cadr (assoc x numbers :test #'equal)))
		   (set-difference 
		    (reduce #'union
			    (mapcar #'(lambda (x) (adj x g)) (car vertices)))
		    (car vertices) :test #'equal)))
	    reduced-graph))
    reduced-graph))


;;; Moved here from phase-three because used elsewhere.  Documented in
;;; phase-three. 


;;; SLOT-CODE is used in collapse and in phase-three.  It is called at runtime,
;;; so it interns symbols into the wrong package, causing generated parser
;;; files to be written with var-name's qualified for some bogus package.
;;; These files use to use the function in runtime-macros.l, but it is not
;;; solely used in places that are only called at the time of compiling a
;;; generated parser.

(defun slot-code (x)
  (sb-intern-nupcase (format nil "V~D" x)))


(eval-when (load compile eval)

(defmacro get-number (fragment)
  `(car ,fragment))
    
(defmacro get-fs (fragment)
  `(cadr ,fragment))
  
(defmacro get-code (fragment)
  `(nth 2 ,fragment)))





;;; Moved from top-parse.lisp because used elsewhere. 

(defun get-bracketing-information (nt)
  (if (eq (car nt) 'nonterminal)
      (assoc (cadr nt) *global-brackets*)
      (assoc (nth 2 nt) (cadr (assoc (cadr nt) *ext-brackets*)))))



;;; Error reporting routines. 

;;; This guy handles an internal error.  
(defun interror (formatstr &rest args)
  (error "Internal error: ~?" formatstr args))

;;; This routine is destined to replace my-error because it doesn't use error 
;;; numbers.
(defun my-better-error (formatstr &rest args)
  (format t "Current nonterminal is ~A.~%" *current-nt*)
  (setq *parser-gen-error* t)
  (format t "Error: ~?~%" formatstr args))

(defun my-warning (formatstr &rest args)
  (format t "Current nonterminal is ~S.~%" *current-nt*)
  (format t "Warning: ~?~%" formatstr args))

;;; Handling errors by number is bogus, so this routine should vanish someday.
(defun my-error (number &optional extra-value)
  (format t "Error number ~S.  " number)
  (ecase number
    (0 (interror "Could not find nonterminal definition"))
    (1 (my-better-error "Null productions in nonterminal"))
    (2 (my-better-error "Alternation branch => epsilon."))
    (3 (my-better-error "Inside of optional => epsilon."))
    (4 (my-better-error "Front pattern and back pattern overlap."))
    (5 (my-better-error "Separator and back pattern overlap."))
    (6 (my-better-error "Subpattern of star can not produce an epsilon."))
    (7 (my-better-error "Subpattern of plus can not produce an epsilon."))
    (8 (my-better-error "First son of doublestar can not produce an epsilon."))
    (9 (my-better-error "First son of doubleplus can not produce an epsilon."))
    (10 (my-better-error "Separator must be a keyword."))
    (11 (interror "Undefined pattern type"))
    (12 (my-warning "Front and Back of sequence overlap on symbols: ~A"
		    extra-value))
    (13 (my-better-error "Overlapping productions.  Overlapping symbols are: ~A"
			 extra-value))
    (15 (interror "Wrong type of pattern to flatten-alternative."))
    (17 (my-better-error "Overlap of second symbols."))
    (18 (interror "Wrong type of pattern to flatten-pattern."))
    (19 (my-better-error "Infinite-loop in grammar NT -> NT."))
    (20 (interror "Wrong type of pattern to new-fs-list."))
    (21 (interror "First son has wrong type in combine-seq."))
    (22 (my-better-error "Seperator for ++/** must be a keyword."))
    (23 (my-better-error
	 "Wrong type of pattern from flatten (alt/single).~%~
          May be that nonterminal ~a is nullable." *current-nt*)
	(interror ""))	
    (24 (my-better-error "Can not find definition for nonterminal ~S."
			 extra-value))
    (26 (interror "Null init-flag in process-nt."))
    (27 (interror "Fell off the end of asp."))
    (28 (my-better-error "Could not find fragment."))
    (29 (my-better-error "Medial symbols overlap on symbol ~S." extra-value))
    (30 (my-better-error "Looping construct Look-ahead set overlap."))
    (31 (interror "Medials found in singleton nonterminal."))
    (32 (interror "Bad pattern returned from flatten-first."))
    (33 (my-better-error "Can not find auxiliary file for grammar ~A."
			 extra-value))
    (34 (my-better-error "The package name of the grammar ~A is nil."
			 extra-value))
    (35 (my-better-error "Nonterminal ~A does not exist in grammar ~A."
			 (third extra-value)
			 (second extra-value)))
    (36 (my-better-error
	 "One of the operands of a juxtaposition must be a recursive reference."))
    (37 (my-better-error
	 "NT0 ::= NT1 NT2 ... should use juxtaposition, i.e., ~
	 NT0 ::= NT1 jux NT2."))
    (38 (interror "Can't use jux in singleton nonterminal definitions."))))
