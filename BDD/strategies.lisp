(in-package 'pvs)

(defstep expand* (&rest names)
  (expand1* names)
  "Expands all the given names and simplifies. "
  "Expanding the definition(s) of ~a")


;(defstep expand1* (names)
;  (if (null names)
;      (skip)
;      (let ((name1 (car names))
;	    (rest-names (cdr names)))
;	(then (expand name1) (expand1* rest-names))))
;  "Expands all the given names and simplifies. "
;  "Expanding the definition(s) of ~a")
;
;(defstep induct-and-bddrewrite (var &optional (fnum 1)  &rest rewrites)
;  (then (let ((x `(auto-rewrite ,@rewrites)))
;          x)
;    (try (induct var fnum)
;     (then*
;      (skosimp*)
;      (assert)
;      (repeat* (apply (then* (inst?)(lift-if)(bddsimp)(skosimp*))
;              "Applying instantiation, if-lifting, propositional
;simplification, and skolemization,"))
;      (assert)
;      (assert))
;     (skip)))
;  "Performs induction according to the type of VAR using FNUM
;to formulate induction predicate, and then simplifies using the given
;REWRITES. "
;  "By induction on ~a and rewriting")
;
;
;(defstep name-induct-and-bddrewrite (var &optional (fnum 1) name
;				     &rest rewrites)
;  (then (let ((x `(auto-rewrite ,@rewrites)))
;	  x)
;    (try (induct var fnum name)
;     (then*
;      (skosimp*)
;      (repeat* (apply (then* (inst?)
;			     (assert :hash-rewrites? t)
;			     (repeat (lift-if))
;			     (assert :hash-rewrites? t)
;			     (bddsimp)
;			     (assert :hash-rewrites? t))
;		      "Applying instantiation, if-lifting, propositional
;simplification and skolemization,"))
;      (assert))
;     (skip)))
;  "Performs induction according to the type of VAR or the induction scheme
;named NAME, using FNUM to formulate induction predicate, and then simplifies
;using given REWRITES. "
;  "By induction on ~a and rewriting")


(defhelper auto-rewrite-always-imported-theory-instances (imported-theory)
  (let ((theoryimportlist (assuming-instances (declaration *top-proofstate*)))
	(ctlops_instantiations
	 (delete-if-not #'(lambda (x)
			    (string= imported-theory (string (id x))))
	   theoryimportlist))
	(actualslist (mapcar #'(lambda (x) (id (expr (car (actuals x)))))
			     ctlops_instantiations))
	(ctlopstheoryimportlist
	 (mapcar #'(lambda (x) (format nil "~a[~a]" imported-theory x))
		 actualslist)))
    (auto-rewrite-theory-always ctlopstheoryimportlist))
  "Applies (auto-rewrite-theory :always? T) for all instances of the
     imported theory in the current theory."
  "Auto-rewriting all instances of imported theory ~a with
    :always? T option")    

(defstep auto-rewrite-theory-always (thlist)
  (if (null thlist)
      (skip)
      (let ((hd (car thlist))
	    (tl (cdr thlist)))
	(then
	 (auto-rewrite-theory hd :always? T)
	 (auto-rewrite-theory-always tl))))
  "Applies (auto-rewrite-theory :always? T) on a given list of theories."
  "Auto-rewriting given theories ~a with :always? T option")

(defstep model-check (&optional (dynamic-ordering? T)(cases-rewrite? T))
  (let ((cuth *current-theory*)
	(cuthstr (string (id cuth)))
	;(theoryimportlist (assuming-instances (declaration *top-proofstate*)))
	;(ctlops_instantiations
	 ;(mapcar #'(lambda (x) (eq '|ctlops| (id x)))
		; theoryimportlist)
	)
;    (if (null ctlops_instantiations)
;	(skip)
	(then* (skolem!)
	       (auto-rewrite-theory cuthstr :always? T)
	       (auto-rewrite-theory "ctlops" :defs T :always? T)
	       (auto-rewrite-theory "connectives" :defs T :always? T)
	       ;; (auto-rewrite-theory "ctlops[staterec]" :always? T)
	       (auto-rewrite "/=")
	       (rewrite-msg-off)
	       (assert :cases-rewrite?  cases-rewrite?)
	       (expand "EX")
	       (expand "NOT")
	       (expand "AND")
	       (expand "OR")
	       (expand "IMPLIES")
	       (expand "IFF")
	       (expand "TRUE")
	       (expand "FALSE")
	       (assert :cases-rewrite? cases-rewrite?)
	       (musimp :dynamic-ordering? dynamic-ordering?)))
  "Rewrites temporal operators into mu/nu expressions, and
simplifies using mu-calculus checker.  If DYNAMIC-ORDERING? is T,
the BDD package uses dynamic ordering to minimize the BDD size.
If CASES-REWRITE is NIL, this turns off rewriting within the
selections of unsimplified CASES expressions."
  "By rewriting and mu-simplifying")
	   

