;;
;; extrategies.lisp
;; Release: Extrategies-6.0 (12/12/12)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/Extrategies
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; List of strategies in Extrategies:
(defparameter *extra-tegies* "
%  Printing and commenting: printf, commentf
%  Defining tactics, i.e., local strategies: deftactic
%  Labeling and naming: unlabel*, delabel, relabel, name-label,
%    name-label*, name-replace*, discriminate
%  Copying formulas: copy*, protect
%  Programming: mapstep, mapstep@, with-fnums, with-fnums@
%  Control flow: finalize, touch, for, for@, when, when@, unless,
%    unless@, when-label, unless-label, if-label, for, for@
%  Skolem, let-in, let-def: skeep, skeep*, skoletin, skoletin*,
%    redlet, redlet*, skodef, skodef*
%  TCCs: tccs-expr, tccs-formula, tccs-formula*, tccs-step
%  Miscellaneous: splash, replaces, rewrites, rewrite*")

(defparameter *extra-version* "Extrategies-6.0 (12/12/12)")

(defparameter *extra-trusted-sources* nil)

(setq *extra-trusted-sources* (make-hash-table))

(defun record-trusted-source (src comment &optional quiet?)
  (setf (gethash src *extra-trusted-sources*) (cons comment quiet?)))

(defun is-trusted-source (src)
  (gethash src *extra-trusted-sources*))

(record-trusted-source '*PVSTypechecker* "PVS Typechecker" t)
(record-trusted-source '*PVSGroundEvaluator* "PVS Ground Evaluator" t)

;;;;;;;;;; Utility function to load library patches

(defun extra-load-library-patches ()
  (get-pvs-library-path)
  (dolist (path *pvs-library-path*)
    (dolist (lib (directory path))
      (when (and (directory-p lib) (string= "pvs-patches" 
					    (file-namestring lib)))
	(loop for i from 0 
	      for patch = (make-pathname
			   :defaults (format nil "~a/" lib)
			   :name (format nil "patch-~a" i)
			   :type "lisp")
	      while (file-exists-p patch)
	      do (progn (pvs-message "Loading PVS library patch:")
			(load patch)))))))

;;;;;;;;;; Utility functions and additional strategies

;; a <=> b
(defun iffequiv (a b)
  (equal (null a) (null b)))

;; Get all keys of a hash table
(defun get-hash-keys (hash)
  (loop for k being the hash-key of hash
	collect k))

;; Generates a label with given prefix that is fresh in the current context
(defun freshlabel (prefix)
  (when *current-context*
    (let ((alllabels (union (extra-get-labels '*)
			    (extra-get-labels '* t))))
      (loop for i from 0
	    for nn = (intern (format nil "~a~a:" prefix i))
	    when (not (member nn alllabels :test #'string=))
	    return nn))))

;; Generates a label with given prefix that is fresh in the current context
(defun freshlabels (prefix n)
  (when *current-context*
    (let ((nlabels 0)
	  (alllabels (union (extra-get-labels '*)
			    (extra-get-labels '* t))))
      (loop for i from 0
	    for nn = (intern (format nil "~a~a:" prefix i))
	    while (< nlabels n) 
	    when (not (member nn alllabels :test #'string=))
	    collect (progn (incf nlabels) nn)))))

;; Check if name has been defined in the proof context
(defun check-name (name)
  (let* ((name (if (stringp name) (intern name) name))
	 (pc-name (pc-parse name 'expr)))
    (resolve pc-name 'expr nil)))

;; Check if an identifier is a free variable (and not in but list)
;; The nonstrict option includes unbounded declared variables
(defun is-freevar (name &optional nonstrict but)
  (and (not (member name but :test #'string=))
       (let ((rs (check-name name)))
	 (if nonstrict 
	   (every #'(lambda (r) (and (resolution? r) (var-decl? (declaration r))))
		  rs)
	   (null rs)))))

;; Generates a name with given prefix that is fresh in the current context (and not in but list)
(defun freshname (prefix &optional nonstrict (postfix "_") but)
  (when *current-context*
    (if (is-freevar prefix nonstrict but)
	prefix
      (loop for i from 0
	    for nn = (format nil "~a~a~a" prefix i postfix)
	    when (is-freevar nn nonstrict but)
	    return nn))))

(defun names2freshnames (names &optional nonstrict (postfix "_") but)
  (when names
    (let ((nn (freshname (car names) nonstrict postfix but)))
      (cons nn  (names2freshnames (cdr names) nonstrict postfix (cons nn but))))))
      
;; Generates a name with given prefix that is fresh in the current context
(defun freshnames (prefix n &optional nonstrict (postfix "_") but)
  (when *current-context*
    (if (= n 1)
	(list (freshname prefix nonstrict postfix but))
      (let ((nnames 0))
	(loop for i from 0
	      for nn = (format nil "~a~a~a" prefix i postfix)
	      while (< nnames n)
	      when (is-freevar nn nonstrict but)
	      collect (progn (incf nnames) nn))))))
  
;; Get a list of formula numbers from fnums
(defun extra-get-fnums (fnums)
  (when fnums
    (if (numberp fnums)
	(and (or (and (> fnums 0) (<= fnums (length *+*)))
		 (and (< fnums 0) (<= (- fnums) (length *-*))))
	     (list fnums))
      (map-fnums-arg fnums))))

;; Get a formula number from fnum
(defun extra-get-fnum (fnum)
  (car (extra-get-fnums fnum)))

;; Get a PVS object from expr, where expr can be speficied as a formula or a string
;; or using Manip's location
(defun extra-get-expr (expr)
  (cond ((expr? expr) expr)
	((or (numberp expr) (symbolp expr))
	 (extra-get-formula expr))
	((stringp expr)
	 (pc-typecheck (pc-parse expr 'expr)))
	((and (listp expr) (equal (car expr) '!))
	 (ee-pvs-obj (car (eval-ext-expr expr))))))

(defun extra-get-expstr (expr)
  (expr2str (extra-get-expr expr)))

;; Returns list of formula numbers not in fnums
(defun extra-get-but-fnums (fnums &key (all '*))
  (when all
    (let ((forms (extra-get-fnums all))
	  (but   (extra-get-fnums fnums)))
      (set-difference forms but))))

;; Get sequent formula from an *actual* formula number in the sequent.
(defun extra-get-seqf-from-fnum (fn)
  (when fn
    (let* ((fs    (if (> fn 0) (p-sforms *goal*) (n-sforms *goal*)))
	   (index (- (abs fn) 1)))
      (nth index fs))))
  
;; Get list of sequent formulas in fnums
;; If hidden? is t fnums should be a list of numbers or one of the symbols *,-,+
(defun extra-get-seqfs (fnums &optional hidden?)
  (when fnums
    (if hidden?
	(select-seq (hidden-s-forms *goal*) fnums)
      (loop for fn in (extra-get-fnums fnums)
	    collect (extra-get-seqf-from-fnum fn)))))

;; Get sequent formula in fnum
(defun extra-get-seqf (fnum &optional hidden?)
  (when fnum
    (car (extra-get-seqfs fnum hidden?))))

;; Get formula from an *actual* formula number in the sequent.
(defun extra-get-formula-from-fnum (fn)
  (when fn 
    (let* ((seqf (extra-get-seqf-from-fnum fn)))
      (when seqf
	(if (> fn 0)
	    (formula seqf)
	  (argument (formula seqf)))))))

;; Get a formula given a FNUM, which can be a label.
(defun extra-get-formula (fnum)
  (when fnum
    (extra-get-formula-from-fnum (extra-get-fnum fnum))))

;; Get list of labels of an *actual* formula number
(defun extra-get-labels-from-fnum (fn)
  (when fn
    (label (extra-get-seqf-from-fnum fn))))

;; Generalized union
(defun union-list (l)
  (when l (union (car l) (union-list (cdr l)))))

;; Get list of labels of formulas in fnums
;; If hidden? is t fnums should be a list of numbers or one of the symbols *,-,+
(defun extra-get-labels (fnums &optional hidden?)
  (when fnums
    (union-list (loop for seq in (extra-get-seqfs fnums hidden?)
		      collect (label seq)))))

;; Expression with an infix operator?
(defun is-infix-operator (term-obj op)
  (and (infix-application? term-obj)
       (name-expr? (operator term-obj))
       (eq (id (operator term-obj)) op)))

;; Expression with a prefix operator?
(defun is-prefix-operator (term-obj op)
  (and (unary-application? term-obj)
       (name-expr? (operator term-obj))
       (eq (id (operator term-obj)) op)))

;; If l is not a list put it into a list
(defun enlist-it (l)
  (if (and (listp l) (not (equal (car l) '!)))
      l
    (list l)))

;; Pairs lists ls1 and ls2. Unless cut? is t, lists are completed with the last
;; elements if they have different length.
(defun pair-lists (ls1 ls2 &optional cut? prevl1 prevl2)
  (when (if cut? (and ls1 ls2) (or ls1 ls2))
    (let ((l1 (if ls1 (car ls1) prevl1))
	  (l2 (if ls2 (car ls2) prevl2)))
    (cons (cons l1 l2)
	  (pair-lists (cdr ls1) (cdr ls2) cut? l1 l2)))))

;; Returns relation if expr is an order relation 
(defun is-order-relation (expr)
  (let ((rel (car (is-relation expr))))
    (when (and (not (equal rel '=))
	       (not (equal rel '/=))) rel)))

;; Returns symbol that is the negation of the order relation rel
(defun neg-relation (rel)
  (cond ((equal rel '<)  '>=)
	((equal rel '<=) '>)
	((equal rel '>)  '<=)
	((equal rel '>=) '<)
	((equal rel '=) '/=)
	((equal rel '/=) '=)))

;; Returns the identifier of expression as a string
(defun variable-id (expr)
  (if (name-expr? expr) (id expr)
    (format nil "~a" expr)))

;; Returns true if expr is a variable (when vars is not null it checks if the variable
;; expr is included in vars)
(defun is-variable-expr (expr &optional vars)
  (and expr
       (or (name-expr? expr)
	   (fieldappl? expr))
       (or (null vars)
	   (member (variable-id expr) (enlist-it vars) :test #'string=))))

;; Returns true if expr is a function application of function name
(defun is-function-expr (expr &optional names)
  (and expr
       (application? expr)
       (name-expr? (operator expr))
       (or (null names)
	   (member (id (operator expr)) (enlist-it names) :test #'string=))))

;; Returns a numerical constant whene expr is a ground number expression
(defun number-from-expr (expr)
  (cond ((rational-expr? expr)
	 (number expr))
	((decimal? expr)
	 (/ (number (args1 expr)) (number (args2 expr))))
	((is-prefix-operator expr '-)
	 (let ((num (number-from-expr (args1 expr))))
	   (when num (- num))))
	((and (infix-application? expr)
	      (name-expr? (operator expr)))
	  (let* ((op   (car (member (id (operator expr)) '(+ - * /))))
		 (num1 (and op (number-from-expr (args1 expr))))
		 (num2 (and op num1 (number-from-expr (args2 expr)))))
	    (when num2 (cond ((equal op '+) (+ num1 num2))
			     ((equal op '-) (- num1 num2))
			     ((equal op '*) (* num1 num2))
			     ((equal op '/) (/ num1 num2))))))))

;; Returns true if type is a number type
(defun is-number-type (type)
  (or (and (type-name? type)
           (equal (id type) (id *number*)))
      (and (subtype? type)
	   (is-number-type (supertype type)))))

;; Returns rrue if expr is a number expression
(defun is-number-expr (expr)
  (is-number-type (type expr)))

;; Merges two lists in one string using 
;; :empty as the empty-string
;; :conc as the string-concatenator
;; :sep as the string-separator
;; For instance (merge2str '("a" "b" "c") '("A" "B" "C") :conc "-" :sep ",")
;; returns "a-A,b-B,c-C"

(defun merge2str (l1 l2 &key (empty "") (conc "") (sep ""))
  (let ((l (loop for a in l1
		 for b in l2
		 collect (format nil "~a~a~a" a conc b))))
    (if l (format nil "~a~{~a~}" (car l) (loop for ab in (cdr l) append (list sep ab)))
      empty)))
  
;; Sign of n (note that 0 returns 1).
(defun sign (n)
  (if (>= n 0) 1 -1))
  
;; String to int.
(defun str2int (str)
  (multiple-value-bind (n l) (parse-integer str :junk-allowed t)
    (when (and n (= (length str) l)) n)))

;; Expression to string (tries to minimize parentheses)
(defun expr2str (expr)
  (when expr
    (cond ((stringp expr) expr)
	  ((numberp expr) (format nil "~a" expr))
	  ((and (infix-application? expr)
		(= (parens expr) 0)
		(not (is-relation expr)))
	   (format nil "(~a)" expr))
	  ((and (or (name-expr? expr)
		    (rational-expr? expr))
		(> (parens expr) 0))
	   (format nil "~a" (copy expr 'parens 0)))
	  (t (format nil "~a" expr)))))
  
;; Creates a list of numbers in the range from..to.
(defun fromto (from to) 
  (cond 
   ((< from to) (cons from (fromto (+ from 1) to)))
   ((> from to) (cons from (fromto (- from 1) to)))
   (t (list to))))

;; Returns the minimum of a list of numbers.
(defun minlist (l)
  (eval (cons 'min l)))

;; Returns the maximum of a list of numbers.
(defun maxlist (l)
  (eval (cons 'max l)))

;; Removes numbers in list fnums that appear before from.
(defun remove-before (from fnums)
  (when fnums
    (if (= from (car fnums))
	fnums
        (remove-before from (cdr fnums)))))
	       
;; Removes numbers in list fnums that appear after to.
(defun remove-after (to fnums)
  (when fnums
    (let ((a (car fnums)))
      (if (= to a)
	  (list a)
          (cons a (remove-after to (cdr fnums)))))))

;; Let l=(l0..ln), if flag then returns (l0,l2,..),
;; otherwise returns (l1,l3,..).
(defun each-other (l flag)
  (when l
    (if flag
	(cons (car l)(each-other (cdr l) (not flag)))
      (each-other (cdr l) (not flag)))))

;; Removes position p of list l.
(defun removepos (p l)
  (when l
    (if (= p 0) (cdr l)
      (cons (car l) (removepos (- p 1) (cdr l))))))

;; Find the first formula that satisfies test. Test is a function with two arguments
;; the first of which is a formula number and the second is the corresponding expression.
;; Returns the first arguments that make the test hold
(defun first-formula (fnums &key test)
  (loop for fn in (extra-get-fnums fnums)
	for form = (extra-get-formula-from-fnum fn)
	when (or (null test) (funcall test fn form))
	return (list fn form)))

;;;;;;;;;; Extrategies

;;; Printing and commenting

(defstrat printf (msg &rest args)
  (let ((msg (format nil "~%~a" msg))
	(xxx (apply 'format (cons t (cons msg args)))))
    (skip))
  "[Extrategies] Prints the Lisp formatted string MSG using the format arguments
ARGS. ARGS must be constant values.")

(defstrat commentf (msg &rest args)
  (let ((msg (apply 'format (cons nil (cons msg args)))))
    (comment msg))
  "[Extrategies] Adds the formatted comment MSG to the sequent using the format
arguments ARGS. ARGS can only have constant values.")

;;; Labeling and naming

(defstep unlabel* (&optional (fnums *) label)
  (let ((fs (extra-get-fnums fnums)))
    (if (and label (listp label))
	(let ((qfs (list 'quote fs)))
	  (mapstep #'(lambda(x)`(unlabel ,qfs ,x)) label))
      (unlabel fs label)))
  "[Extrategies] Removes specified LABEL(s) (or all labels if LABEL is nil) from FNUMS."
  "Removing ~1@*~:[all labels~;label(s) ~:*~a~] from ~@*~a")

(defstep delabel (label &optional hide? (hidden? t))
  (let ((fnums (extra-get-fnums label))
	(seqfs  (when hidden? (extra-get-seqfs label t))))
    (then (when fnums
	    (unlabel* fnums label)
	    (when hide? (hide fnums)))
	  (when seqfs
	    (let ((lbs (enlist-it label)))
	      (mapstep #'(lambda(x)`(unlabel :label ,x :hidden? t)) lbs)))))
  "[Extrategies] Removes LABEL(s). If HIDE? is t, the delabeled formulas are hidden.
If hidden? is t, LABEL(s) are also removed from hidden formulas."
  "Removing label(s) ~a")

(defstep relabel (label fnums &optional pairing? (push? t))
  (let ((lbs  (enlist-it label))
	(lfs  (mapcar #'extra-get-fnums (if pairing? fnums (list fnums))))
	(lbfs (if pairing? (pair-lists lbs lfs)
		(mapcar #'(lambda (x) (cons x lfs)) lbs))))
    (then
     (unless push? (unlabel* fnums))
     (mapstep #'(lambda(x)`(label ,(car x) ,(cdr x) :push? t)) lbfs)))
  "[Extrategies] Labels FNUMS as LABEL(s), keeping the old ones. If PAIRING? is t, both LABEL and PAIRING?
must be lists of the form (LAB1 ... LABn) and (FNUM1 ... FNUMn). In this case, LABELi is paired to FNUMi.
If the lists have different length, they are completed using their last element. If PUSH? is t, then the
new labels are added to any existing ones.  Otherwise, the new labels replace all existing ones."
  "Labeling formula(s) ~1@*~a as ~@*~a")

(defstep name-label (name expr &optional label (fnums *) (dir lr) hide? (step (subtype-tcc)))
  (let ((labnl   (unless (equal label 'none)
		   (or label (format nil "~a:" name))))
	(estr    (extra-get-expstr expr)))
    (when estr
      (if (not (check-name name))
	  (with-fnums@
	   ((!nml fnums)
	    (!nlt)
	    (!nlx))
	   (tccs-expr estr :label !nlt :step step)
	   (branch (discriminate (name name estr) (labnl !nlx))
		   ((then (when fnums (replace !nlx !nml))
			  (let ((flagdir (equal dir 'rl)))
			    (when@ flagdir
				   (claim "%1 = %2" t name estr)
				   (delete !nlx)
				   (label labnl -1)))
			  (when hide?
			    (printf "Reveal new hidden formulas with (reveal \"~a\")" labnl)
			    (hide labnl))
			  (delete !nlt))
		    (finalize step))))
	(printf "Name ~a already exists" name))))
  "[Extrategies] Adds formmula EXPR=NAME, where NAME is a new name, as a hypothesis. The added formula is
labeled according to the value of LABEL. If LABEL is none, the formula is not labeled; if LABEL is nil,
the formula is labeled as NAME:; otherwise, it is labeled as LABEL. If FNUMS is not nil, the name is
replaced in those formulas. DIR indicates the direction of the name definition, e.g., EXPR=NAME,
if DIR is lr, or NAME=EXPR, if DIR is rl. The added formula is hidden when HIDE? is t.
TCCs generated during the execution of the command are discharged with the proof command STEP."
  "Naming ~1@*~a as ~@*~a")

(defstep name-label* (names-and-exprs &optional label (fnums *) (dir lr) hide? (step (subtype-tcc)))
  (let ((qlabel (list 'quote label))
	(qdir   (list 'quote dir))
	(qstep  (list 'quote step)))
    (with-fnums
     (!nls fnums)
     (let ((qnls (list 'quote !nls)))
       (mapstep@ #'(lambda(x)`(when ,(= (mod (length x) 2) 0)
				(name-label ,(car x) ,(cadr x) ,qlabel ,qnls ,qdir ,hide? ,qstep)))
		 names-and-exprs :list? t))))
  "[Extrategies] Iterates name-label on NAMES-AND-EXPRS, which is assumed to be a list of the form
(NAME1 EXPR1 NAME2 EXPR2 ... NAMEn EXPRn). Options are as in name-label.
TCCs generated during the execution of the command are discharged with the proof command STEP."
  "Interating name-label on a list of names and expressions")

(defstrat name-replace* (names-and-exprs &optional (fnums *) (dir lr) hide?  (step (subtype-tcc)))
  (name-label* names-and-exprs none fnums dir hide? step)
  "[Extrategies] Same as name-label* without labeling the formulas."
  "Iterating name-replace")

(defstep discriminate (step &optional label strict?)
  (if label
      (with-fnums
       (!dsc)
         (if strict?
	     (then (with-labels step !dsc)
		   (relabel label !dsc))
	   (then (relabel !dsc *)
		 step
		 (relabel label (^ !dsc)))))
    step)
  "[Extrategies] Labels formulas generated by STEP as LABEL(s). When STRICT? is set to t,
all formulas that are considered new by PVS are also labeled."
  "Labeling new formulas~*~@[ as ~a~]")

;;; Strategy programming

(defstrat mapstep (funstep &optional list list?)
  (when list
    (let ((step (funcall (eval funstep) (if list? list (car list))))
	  (rest (cdr list)))
      (then step
	    (mapstep funstep rest list?))))
  "[Extrategies] Sequentially applies FUNSTEP to each element of LIST. FUNSTEP is a function
in Lisp that takes one argument and returns a proof command. After each application of FUNSTEP,
the resulting proof command is applied to all branches. When LIST? is nil, the argument of
FUNSTEP represents the head of the list at each iteration. For example,

(mapstep #'(lambda(x)`(name ,x \"1\")) (\"One\" \"Uno\"))

behaves as (then (name \"One\" \"1\") (name \"Uno\" \"1\")).

If LIST? is t, the argument of FUNSTEP represents the complete list at each iteration.
For example,

(mapstep #'(lambda(x)`(name ,(car x) ,(length x))) (\"Two\" \"One\") :list? t)

behaves as (then (name \"Two\" 2) (name \"One\" 1)).

Technicality: Note the use of quotation and anti-quotation in the examples. Variables in FUNSTEP
other than FUNSTEP's parameter can be unquoted only if they were defined outside FUNSTEP
as (list 'quote <something>), e.g.,

(let ((lbs (list 'quote '(\"a\" \"b\" \"c\"))))
  (mapstep #'(lambda(x)`(relabel ,lbs ,x)) (-1 1)))")

(defstrat mapstep@ (funstep &optional list list?)
  (when list
    (let ((step (funcall (eval funstep) (if list? list (car list))))
	  (rest (cdr list)))
      (then@ step
	     (mapstep@ funstep rest list?))))
  "[Extrategies] Sequentially applies FUNSTEP to each element of LIST. FUNSTEP is a function
in Lisp that takes one argument and returns a proof command. After each application of FUNSTEP,
the resulting proof command is applied to the main branch. When LIST? is nil, the argument of
FUNSTEP represents the head of the list at each iteration. If LIST? is t, the argument of FUNSTEP
represents the complete list at each iteration. See (help mapstep) for examples of use.")

(defun withfnums-bindings (bindings)
  (let ((bndgs (enlist-it bindings)))
    (if (listp (car bndgs)) bndgs (list bndgs))))

(defhelper with-fnums-tccs__ (ftccs)
  (mapstep #'(lambda(x)`(tccs-formula* ,(car x) :label ,(cdr x))) ftccs)
  "[Extrategies] Internal strategy." "")

(defhelper with-fnums__ (bindings thn steps)
  (when steps
    (let ((bindgs (withfnums-bindings bindings))
	  (vrslbs (mapcar #'(lambda(x)(list (car x) (list 'quote (freshlabel (string (car x))))))
			  bindgs))
	  (ftccs  (loop for b in bindgs
			when (equal (caddr b) ':tccs)
			collect (list (car b) (freshlabel (format nil "~a-tccs" (string (car b)))))))
	  (ltccs  (mapcar #'cadr ftccs))
	  (labs   (mapcar #'car bindgs))
	  (fnums  (mapcar #'cadr bindgs))
	  (thenstep (cons thn steps))
	  (step `(let ,vrslbs (then (relabel ,labs ,fnums :pairing? t)
				    (with-fnums-tccs__$ ,ftccs)
				    (try ,thenstep (skip) (fail))
				    (delabel ,labs)
				    (delete ,ltccs)))))
      step))
  "[Extrategies] Internal strategy." "")

(defstrat with-fnums (bindings &rest steps)
  (else (with-fnums__$ bindings then steps) (skip))
  "[Extrategies] Allows variables in body to be bound to formulas in the current sequent.
STEPS are sequentially applied to all branches. BINDINGS has either the form (!VAR FNUMS) or
((!VAR1 FNUMS1) ... (!VARN FNUMSn)). For example,

(with-fnums (!l -2) (flatten) (inst? !l))

flattens the current sequent and then instantatiates the formula that used to be -2 in the
original sequent.")

(defstrat with-fnums@ (bindings &rest steps)
  (else (with-fnums__$ bindings then@ steps) (skip))
  "[Extrategies] Allows variables in body to be bound to formulas in the current sequent.
STEPS are sequentially applied to the main branch. BINDINGS has either the form (!VAR FNUMS) or
((!VAR1 FNUMS1) ... (!VARN FNUMSn)). For example,

(with-fnums@ (!l -2) (split) (inst? !l))

splits the current sequent and then instantatiates, on the main branch, the formula that
used to be -2 in the original sequent.")

;;; Copying formulas

(defhelper copy__ (fn label labels?)
  (let ((labs (extra-get-labels-from-fnum fn)))
    (then (discriminate (copy fn) label)
	  (when labels?
	    (relabel labs label))
	  (hide label)))
  "[Extrategies] Internal strategy." "")

(defstep copy* (fnums &optional label hide? labels?)
  (let ((fs      (extra-get-fnums fnums))
	(labcpy  (or label (freshlabel "CPY")))
	(qlabcpy (list 'quote labcpy)))
    (then
     (mapstep #'(lambda (x)`(copy__$ ,x ,qlabcpy ,labels?)) fs)
     (unless hide? (reveal labcpy))
     (unless label (delabel labcpy))))
  "[Extrategies] Copies formulas in FNUMS. The copied formulas are labeled as LABEL(s), if
LABEL is not nil. When HIDE? is t, the copied formulas are hidden. If LABELS? is t,
labels are also copied."
  "Copying formulas ~a")

(defstep protect (fnums step &optional label hide?)
  (if fnums
      (let ((labprc (or label (freshlabel "PRC"))))
	(with-fnums
	 (!pro fnums)
	 (copy* !pro labprc :hide? t :labels? t)
	 step
	 (unless hide? (reveal labprc))
	 (unless label (delabel labprc))))
    step)
  "[Extrategies] Protects formulas FNUMS so that they are not afected by STEP. The protected formulas
 are labeled as LABEL(s), if LABEL is not nil."
  "Protecting formulas in ~a")

;;; Defining tactics

(defhelper localtactic__ (name stratn step)
  (if (check-name stratn)
      step
    (printf "Local strategy ~a is not defined in this proof context" name))
  "[Extrategies] Internal strategy." "")

(defrule deftactic (name arg_or_step &optional step)
  (let ((stratn  (format nil "local_tactic_~a__" name))
	(arg     (when step arg_or_step))
	(stp     (list 'localtactic__ name stratn (or step arg_or_step)))
	(doc1    (format nil "Local tactic ~a defined in the proof context: ~a"
			 name (label *ps*)))
	(doc2    (format nil "Applying local tactic ~a" name)))
    (then (lisp (defstep name arg stp doc1 doc2)) 
	  (if (check-name stratn)
	      (printf "Redefining local tactic ~a" name)
	    (then (name stratn "TRUE")
		  (delete -1)))))
  "[Extrategies] Defines a tactic named NAME. A tactic is a strategy that is local to the current branch
of the proof. NAME needs to be a valid identifier in PVS. A tactic definition can be either
(deftactic NAME STEP) or (deftactic NAME (ARGUMENTS) STEP). For example,

(deftactic grind_when_a (when-label \"a\" (grind)))

defines a tactic (grind_when_a) that applies (grind) only when a formula labeled \"a\"
appears in the sequent. The definition

(deftactic grind_when_lab (label) (when-label label (grind)))

defines a tactic (grind_when_lab LABEL) that applies (grind) when a given LABEL appears in the
sequent. Tactic parameters can be optional and they can have default values. For example,

(deftactic grind_if_lab_else (label &optional (step (skip)))
  (if-label label (grind) step))

defines a tactic that can be used either as (grind_if_lab_else LABEL), in which case it
behaves as (grind_when_lab LABEL), or (grind_if_lab_else LABEL STEP). In the latter case,
the tactic behaves as  (grind_when_lab LABEL) except when LABEL doesn't appear in the
sequent, in which case it applies STEP."
  "Defining local tactic ~a")

;;; This strategy enables the addition of trusted formulas into the current sequent.
;;; Examples of such additions are type-checking information (TCCs), ground evaluations,
;;; and external trusted oracles. The strategy MUST only be used in proof rules.

(defstrat trust (src step &optional steps)
  (let ((tsrc (is-trusted-source src)))
    (if tsrc
	(let ((trstme (make-instance 's-formula 'formula *true* 'label nil 'new? nil 'asserted? nil))
	      (mrcl   `(let ((xxx (setf (s-forms *goal*) (cons ,trstme (s-forms *goal*)))))
			 (unless ,(cdr tsrc) (printf "Source: ~a" ,(car tsrc)))))
	      (stps   (mapcar #'(lambda (x) (or (and (equal x '!) mrcl) x)) steps)))
	  (try-branch step stps (skip)))
      (printf "~a is not a trusted source" src)))
  "This strategy enables the addition of trusted formulas into the current sequent.
Examples of such additions are type-checking information (TCCs), ground evaluations,
and external *trusted* oracles. The strategy *must* only be used in proof rules.")

;;; TCCs -- The following rules extend the internal proving capabilities of PVS.
;;; They cannot be written as a combination of the basic proof rules

(defhelper relabel-hide__ (step lab1 lab2 hide?)
  (then step
	(relabel lab1 lab2)
	(when hide?
	  (hide lab2)))
  "[Extrategies] Internal strategy." "")

(defun get-tccs-expr (expr)
  (when expr
    (let ((*tccforms* nil)
	  (*generate-tccs* 'all))
      (pc-typecheck (pc-parse (format nil "~a" expr) 'expr))
      (reverse (mapcar #'tccinfo-formula *tccforms*)))))
  
(defhelper tccs-expr__ (expr label hide? step)
  (let ((e    (extra-get-expr expr))
	(estr (expr2str e)))
    (when e
      (with-fnums
       (!tce)
       (relabel-hide__ (discriminate (typepred! estr :all? t :implicit? t) !tce)
		       label !tce hide?)
       (let ((tccs  (get-tccs-expr e))
	     (tcc   (when tccs (expr2str (mk-conjunction tccs)))))
	 (when tccs
	   (trust *PVSTypechecker*
		  (discriminate (case tcc) !tce)
		  ((relabel-hide__ (flatten -1) label !tce hide?)
		   (finalize step) !)))))))
  "[Extrategies] Internal strategy." "")

(defrule tccs-expr (expr &optional label hide? (step (subtype-tcc)))
  (tccs-expr__$ expr label hide? step)
  "[Extrategies] Adds TCCs of expression EXPR as hypotheses to the current sequent. Added hypotheses
are labeled as LABEL(s), if LABEL is not nil. They are hidden when HIDE? is t. TCCs generated during
the execution of the command are discharged with the proof command STEP."
  "Adding TCCs of expression ~a as hypotheses")

(defhelper tccs-formula__ (fn)
  (let ((tccs  (get-tccs-expr (extra-get-formula-from-fnum fn)))
	(tcc   (when tccs (expr2str (mk-conjunction tccs)))))
    (when tccs
      (trust *PVSTypechecker*
	     (case tcc)
	     ((flatten -1) !))))
  "[Extrategies] Internal strategy." "")

(defhelper tccs-formula*__ (fnums)
  (with-fnums
   (!tcf fnums)
   (let ((fs1 (extra-get-fnums !tcf)))
     (when fs1
       (all-implicit-typepreds !tcf)
       (let ((fs2 (extra-get-fnums !tcf)))
	 (mapstep #'(lambda(x)`(tccs-formula__$ ,x)) fs2)))))
  "[Extrategies] Internal strategy." "")

(defrule tccs-formula* (&optional (fnums *) label hide?)
  (with-fnums
   (!tcfs)
   (then (discriminate (tccs-formula*__$ fnums label hide?) !tcfs)
	 (relabel-hide__ (skip) label !tcfs hide?)))
  "[Extrategies] Adds TCCs of formulas FNUMS as hypotheses to the current sequent. Added hypotheses
are labeled as LABEL(s), if LABEL is not nil. They are hidden when HIDE? is t."
  "Adding TCCs of formulas ~a as hypotheses")

(defstep tccs-formula (&optional (fnum 1) label hide?)
  (tccs-formula* fnum label hide?)
  "[Extrategies] Adds TCCs of formula FNUM as hypotheses to the current sequent. Added hypotheses
are labeled as LABEL(s), if LABEL is not nil. They are hidden when HIDE? is t."
  "Adding TCCs of formula ~a as hypotheses")

(defhelper tccs-step__ (step label hide?)
  (with-fnums
   ((!tcs)
    (!tcl))
   (trust
    *PVSTypechecker*
    (with-labels step !tcs t)
    ((let ((parent (parent-proofstate *ps*))
	   (tccs   (loop for goal in (remaining-subgoals parent)
			 append (select-seq (s-forms (current-goal goal)) !tcs)))
	   (fms    (mapcar #'formula tccs))
	   (expr   (when fms (expr2str (mk-conjunction fms)))))
       (when expr
	 (trust *PVSTypechecker*
		(discriminate (case expr) !tcl)
		((relabel-hide__ (flatten !tcl) label !tcl hide?)
		 (delete !tcs) !)))) !)))
  "[Extrategies] Internal strategy." "")

(defrule tccs-step (step &optional label hide?)
  (tccs-step__$ step label hide?)
  "[Extrategies] If STEP generates subgoals, e.g., TCCs, these subgoals are added as hypotheses to the first
subgoal. Added hypotheses are labeled as LABEL(s), if LABEL is not nil. They are hidden when HIDE? is t."
 "Adding TCCs of step ~a as hypotheses")

;;; Control flow

(defhelper finalize__ (step)
  (try step (fail) (skip))
  "[Extrategies] Internal strategy." "")

(defstrat finalize (step)
  (else (finalize__$ step) (skip))
  "[Extrategies] Either finishes the current goal with STEP or does nothing.")

(defstep touch (&optional (step (skip)))
  (else step (case "TRUE"))
  "[Extrategies] Does step and touches the proof context so that try and else consider that step
does something, even when it doesn't." "Doing ~a and touching the proof context")

(defstrat when (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then steps)))
	step)
    (skip))
  "[Extrategies] Behaves as (if FLAG (then STEP1 ... STEPn) (skip)). Due to the execution model
of strategies in PVS, FLAG must be a variable.")

(defstrat when@ (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then@ steps)))
	step)
    (skip))
  "[Extrategies] Behaves as (if FLAG (then@ STEP1 ... STEPn) (skip)). Due to the execution model
of strategies in PVS, FLAG must be a simple variable.")

(defstrat unless (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then steps)))
	   step)
      (skip))
  "[Extrategies] Behaves as (if (not FLAG) (then STEP1 ... STEPn) (skip)). Due to the
execution model of strategies in PVS, FLAG must be a simple variable.")

(defstrat unless@ (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then@ steps)))
	   step)
      (skip))
  "[Extrategies] Behaves as (if (not FLAG) (then@ STEP1 ... STEPn) (skip)). Due to
the execution model of strategies in PVS, FLAG must be a simple variable.")

(defhelper when-label__ (label step)
  (let ((fs (extra-get-fnums label)))
    (when fs step))
  "[Extrategies] Internal strategy." "")

(defstrat when-label (label &rest steps)
  (let ((qlabl (list 'quote label)))
    (mapstep #'(lambda(x)`(when-label__$ ,qlabl ,x)) steps))
  "[Extrategies]  Sequentially applies STEPS to all branches as long as at least one formula
is labeled as LABEL in the sequent.")

(defhelper unless-label__ (label step)
  (let ((fs (extra-get-fnums label)))
    (unless fs step))
  "[Extrategies] Internal strategy." "")

(defstrat unless-label (label &rest steps)
  (let ((qlabl (list 'quote label)))
    (mapstep #'(lambda(x)`(unless-label__$ ,qlabl ,x)) steps))
  "[Extrategies]  Sequentially applies STEPS to all branches as long as no formula is labeled
as LABEL in the sequent.")

(defstrat if-label (label then-step &optional (else-step (skip)))
  (if (extra-get-fnums label)
      then-step else-step)
  "[Extrategies]  Applies THEN-STEP if at least one formula is labeled as LABEL in the current
sequent; otherwise, applies ELSE-STEP.")

(defhelper for__ (n step)
  (if (numberp n)
      (if (<= n 0)
	  (skip)
	(let ((m (- n 1)))
	  (then step
		(for__$ m step))))
    (unless n
     (repeat* step)))
  "[Extrategies] Internal strategy." "")

(defstep for (n &rest steps)
  (when steps
    (let ((step (cons 'then steps)))
      (for__$ n step)))
  "[Extrategies] Iterates N times STEP1 ... STEPn, or until it does nothing if N is nil,
along all the branches."
  "Iterating ~1@*~a ~@*~a times along all the branches")

(defhelper for@__ (n step)
  (if (numberp n)
      (if (<= n 0)
	  (skip)
	(let ((m (- n 1)))
	  (then@
	   step
	   (for@__$ m step))))
    (unless@ n
     (repeat step)))
  "[Extrategies] Internal strategy." "")

(defstep for@ (n &rest steps)
  (when steps
    (let ((step (cons 'then@ steps)))
      (for@__$ n step)))
  "[Extrategies] Iterates N times STEP1 ... STEPn, or until it does nothing if N is nil,
along the main branch."
  "Iterating ~1@*~a ~@*~a times along the main branch")

;; Skolem, let-in, let-def

(defun skeep-formula (fn expr)
  (or (and (< fn 0) (exists-expr? expr))
      (and (> fn 0) (forall-expr? expr))))

(defstep skeep (&optional (fnum (+ -)) preds? (postfix ""))
  (let ((fnexpr (first-formula fnum :test #'skeep-formula))
    	(fn     (car fnexpr))
	(expr   (cadr fnexpr)))
    (when fnexpr
        (let ((names (names2freshnames
		      (mapcar #'(lambda(x) (format nil "~a~a" (id x) postfix)) (bindings expr))
		      t)))
	  (then (skolem fn names preds?)
		(flatten)))))
   "[Extrategies] Skolemizes the first universally quantified formula in FNUM, using the names
of the bound variables as the names of the skolem constants. If POSTFIX is provided, it is appended
to the variable names. Names that clash with with other names in the current sequent are replaced by
fresh names. Type predicates are introduced as hypothesis when PREDS? is t."
   "Skolemizing and keeping names of universal formula in ~a")

(defstep skeep* (&optional (fnum '*) preds? (postfix "") n)
  (with-fnums (!skp fnum)
	      (for@ n (skeep !skp :preds? preds? :postfix postfix)))
  "[Extrategies] Iterates N times skeep (or until it does nothing if N is nil) in the first
universally quantified formula in FNUM.  If POSTFIX is provided, it is appended to the variable
names. Names that clash with with other names in the current sequent are replaced by fresh names.
Type predicates are introduced as hypothesis when PREDS? is t."
  "Iterating skeep in ~a")

(defhelper skoletin__ (fn expr name nth var postfix hide? step)
  (let ((flabels (extra-get-labels-from-fnum fn))
	(consq   (> fn 0))
	(ret     (make-ret))
	(nexpr   (sigmared expr name nth 
			   :newnames (enlist-it var)
			   :postfix postfix :ret ret))
	(retexpr (ret-expr ret))
	(lv      (mapcar #'(lambda(x)(format nil "~a:" x))
			 (each-other retexpr t)))
	(flag    (not (numberp nexpr)))
	(casestr (when flag (format nil "~a" nexpr))))
    (when flag
      (with-fnums
       ((!skl fn :tccs)
	(!skd))
       (try-branch
	(name-label* retexpr :fnums nil :dir rl :hide? nil)
	((then (branch (discriminate (case casestr) !skd)
		       ((if consq (then (replaces lv :hide? nil) (beta (!skl !skd)))
			  (beta !skd :let-reduce? nil))
			(if consq (beta !skd :let-reduce? nil)
			  (then (replaces lv :hide? nil) (beta (!skl !skd))))
			(then (replaces lv :hide? nil) (finalize step))))
	       (relabel flabels !skd)
	       (delete !skl)
	       (if hide?
		   (hide lv)
		 (delabel lv)))
	 (then (replaces lv :hide? nil)
	       (delete !skl)
	       (finalize step)))
	(skip)))))
  "[Extrategies] Internal strategy." "")

(defun skoletin-formula (fn expr)
  (let-expr? expr))

(defstep skoletin (&optional (fnum (+ -)) name (nth 1) var (postfix "") hide? (step (subtype-tcc)))
  (let ((fnexpr (or (first-formula fnum :test #'skoletin-formula)
		    (first-formula fnum)))
    	(fn     (car fnexpr))
	(expr   (cadr fnexpr)))
    (when fnexpr
      (skoletin__$ fn expr name nth var postfix hide? step)))
  "[Extrategies] Names the NTH occurrence (left-right, depth-first) of
NAME in a let-binding of the form
   LET ...,NAME = <expr>,... IN <e>
in FNUM and introduces the equality <var>=expr as hypothesis. All occurrences
of NAME in <e> are replaced by <var>. By default, <var> is NAME, with a POSTFIX
if provided. An explicit name can be provided using the option VAR. Names that
clash with other names in the current sequent are replaced by fresh names.

If NAME is nil, the name of the NTH let-in binding is chosen by the strategy.
In this case, if the NTH let-in binding is an expression (<x1>,...,<xn>) = <expr>,
the equalities <var1>=<expr>`1,...,<varn>=<expr>`n are introduced to the sequent.
By default, <vari> is <xi>. Explicit names can be provided using the
option VAR as a list (<var1> ... <varm>), where m <= n. In this list,
if <vari> is _, <xi> is used instead of <vari>.

Name definitions are hidden when HIDE? is t; they can be recalled at any time with
the command (reveal \"<name>:\"), where <name> is one of the names introduced by the
strategy. TCCs generated during the execution of the command are discharged with the
proof command STEP.

NOTE: This command works better when all let-in variables are explicitly typed as in
LET x:posreal = 2 IN 1/x."
  "Naming let-in binding in ~a")

(defstep skoletin* (&optional (fnum *) (postfix "") hide? (step (subtype-tcc)) n)
  (with-fnums
   (!sks fnum)
   (for@ n (skoletin !sks :postfix postfix :hide? hide? :step step)))
  "[Extrategies] Iterates N times skoletin (or until it does nothing if N is nil) in FNUM."
  "Naming let-in bindings in ~a")

(defhelper redlet__ (fn expr name nth step)
  (let ((flabels (extra-get-labels-from-fnum fn))
	(nexpr   (sigmared expr name nth))
	(flag    (not (numberp nexpr)))
	(casestr (when flag (format nil "id(~a)" nexpr))))
    (when flag
      (with-fnums
       ((!rdl fn :tccs)
	(!rdd))
       (branch (discriminate (case casestr) !rdd)
	       ((then
		 (expand "id" !rdd :assert? none)
		 (if (> fn 0)
		    (beta (!rdl !rdd))
		  (beta !rdd :let-reduce? nil)))
		(then
		 (expand "id" !rdd :assert? none)
		 (if (< fn 0)
		     (beta (!rdl !rdd))
		   (beta !rdd :let-reduce? nil)))
		(finalize step)))
       (relabel flabels !rdd)
       (delete !rdl))))
  "[Extrategies] Internal strategy." "")

(defstep redlet (&optional (fnum (+ -)) name (nth 1) (step (subtype-tcc)))
  (let ((fnexpr (or (first-formula fnum :test #'skoletin-formula)
		    (first-formula fnum)))
    	(fn     (car fnexpr))
	(expr   (cadr fnexpr)))
    (when fnexpr
      (redlet__$ fn expr name nth step)))
  "[Extrategies] Reduces the NTH occurrence of NAME (left-right, depth-first) in a let-in expression
in FNUM. If NAME is nil, the NTH name is reduced instead. TCCs generated during the execution of the
command are discharged with the proof command STEP.

NOTE: This command works better when all let-in variables are explicitly typed as in LET x:posreal = 2 IN 1/x."
  "Reducing let-in expression in ~a")

(defstep redlet* (&optional (fnum *) (step (subtype-tcc)) (n 1))
  (with-fnums
   (!rds fnum)
   (for@ n (redlet !rds :step step)))
  "[Extrategies] Iterates N times redlet (or until it does nothing if N is nil). TCCs generated during
the execution of the command are discharged with the proof command STEP."
  "Reducing let-in expressions in ~a")

(defhelper skodef__ (fnum expr name var postfix hide? step)
  (let ((names (mapcar #'(lambda(x) (format nil "~a" (id x)))
		       (bindings expr)))
	(flag  (or (not name)
		   (member name names :test #'string=)))
	(nve   (when flag (carands (if (< fnum 0)
				       (args1 (expression expr))
				     (expression expr))
				   names name var postfix)))
	(n     (nth 0 nve))
	(v     (nth 1 nve))
	(e     (nth 2 nve))
	(lv    (format nil "~a:" v))
	(cases (format nil "(~a=~a) IFF TRUE" v e))
	(ivr   (instvar (cons v n) (length names))))
    (when nve
      (with-fnums
       (!skk fnum :tccs)
       (try-branch
	(name-label v e :fnums nil :dir rl :hide? t)
	((branch (let ((stp (cons 'inst (cons !skk ivr))))
		   stp)
		 ((branch (case-replace cases)
			  ((then
			    (delete -1)
			    (unless hide? (reveal lv) (delabel lv)))
			   (then (reveal lv) (finalize (assert (lv 1))))
			   (finalize step)))
		  (finalize step)))
	 (finalize step))
	(skip)))))
  "[Extrategies] Internal strategy." "")

(defun skodef-formula (fnum expr)
  (or (and (< fnum 0)
	   (forall-expr? expr)
	   (implication? (expression expr)))
      (and (> fnum 0)
	   (exists-expr? expr))))

(defstep skodef (&optional (fnum (+ -)) name var (postfix "") hide? (step (subtype-tcc)))
  (let ((fnexpr (first-formula fnum :test #'skodef-formula))
    	(fn     (car fnexpr))
	(expr   (cadr fnexpr)))
    (when fnexpr
      (skodef__$ fn expr name var postfix hide? step)))
  "[Extrategies] Given a antecedent formula FNUM of the form
   FORALL(..,NAME:<type>,..) : NAME=<expr> AND ... IMPLIES ...
or a consequent formula FNUM of the form
   EXISTS (..,NAME:<type>,..) : NAME=<expr> AND ...,
this strategy introduces a name definition <var>=expr as hypothesis and
instantiates NAME in FNUM with <var>. By default, <var> is NAME, with
a POSTFIX if provided. An explicit name can be provided using the option VAR.
Names that clash with other names in the current sequent are replaced by
fresh names. 

Name definitions are hidden when HIDE? is t; they can be recalled at any
time with the command (reveal \"<name>:\"), where <name> is the name
introduced by the strategy. TCCs generated during the execution of the command
are discharged with the proof command STEP."
  "Instantiating a quantifier in ~a with a name definition")

(defstep skodef* (&optional (fnum *) hide? (postfix "") (step (subtype-tcc)) n)
  (with-fnums
   (!skk fnum)
   (for@ n (skodef !skk :postfix postfix :hide? hide? :step step)))
  "[Extrategies] Iterates N times skodef (or until it does nothing if N is nil) in FNUM.
TCCs generated during the execution of the command are discharged with the proof command STEP."
  "Iterating skodef in ~a")

;;; Splitting and splashing

(defhelper splash__ (fnum formula reverse step)
  (let ((ands    (get-ands-expr formula (> fnum 0)))
	(rands   (if reverse (reverse ands) ands))
	(docases (> (length rands) 1)))
    (with-fnums
     ((!spl fnum :tccs)
      (!spd))
     (when docases
       (let ((cases (expr2str (mk-conjunction (butlast rands)))))
	 (branch (discriminate (case cases) !spd)
		 ((then (replace !spd) (flatten !spd) (assert (!spl !spd) :let-reduce? nil))
		  (then (delete !spl) (splash$ !spd :reverse? nil))
		  (then (finalize step) (delete !spl)))))
       (flatten !spl))))
  "[Extrategies] Internal strategy." "")

(defun splash-formula (fn expr)
  (or (and (< fn 0) (or (implication? expr) (disjunction? expr)))
      (and (> fn 0) (conjunction? expr))))

(defstep splash (&optional (fnum *) reverse? (step (subtype-tcc)))
  (let ((fnexpr (first-formula fnum :test #'splash-formula))
	(fn     (car fnexpr))
	(expr   (cadr fnexpr)))
    (when fnexpr
      (splash__$ fn expr reverse? step)))
  "[Extrategies] Asymmetrically splits this first (-)-disjunctive or (+)-conjunctive formula
in FNUM. That formula is reversed when REVERSE? is t (this may generate unprovable TCCs since
Boolean operators in PVS are non-strict.) TCCs generated during the execution of the command
are discharged with the proof command STEP."
  "Splashing formula in ~a")

;;; Miscellaneous

(defstep replaces (&optional (fnums -) (in *) but from to
			     (hide? t) (dir lr))
  (let ((flist (extra-get-fnums fnums))
	(nfrom (extra-get-fnum from))
	(nto   (extra-get-fnum to))
	(feqs  (remove-if #'(lambda (x) (= x 0))
			  (cond ((and from to) (fromto nfrom nto))
				(from (remove-before nfrom flist))
				(to   (remove-after nto flist))
				(t    flist)))))
    (when feqs
      (let ((labreq (freshlabels "REQ" (length feqs)))
	    (qdir   (list 'quote dir))
	    (forms  (extra-get-but-fnums but :all in)))
	(with-fnums
	 (!rep forms)
	 (let ((qrep (list 'quote !rep)))
	   (then
	    (relabel labreq feqs :pairing? t)
	    (mapstep #'(lambda(x)`(try (replace ,x ,qrep :dir ,qdir)
				       (when ,hide?
					 (unlabel* ,x ,qrep)
					 (delabel ,x :hide? t))
				       (skip)))
		     labreq)
	    (delabel labreq)))))))
  "[Extrategies] Iterates the proof command replace to rewrite with the formulas in FNUMS,
respecting the order, the formulas in IN but not in BUT. The keys DIR and HIDE? are like
in REPLACE. Notice that in contrast to REPLACE, the default value of HIDE? is T. Instead
of using FNUMS, rewriting formulas can be addressed via FROM and TO."
  "Iterating replace")

(defstep rewrites (lemmas-or-fnums &optional (fnums *) (target-fnums *) (dir lr) (order in) dont-delete?)
   (let ((lms    (enlist-it lemmas-or-fnums))
	 (qdir   (list 'quote dir))
	 (qorder (list 'quote order))
	 (qdont  (list 'quote dont-delete?)))
     (with-fnums ((!rew fnums)
		  (!ret target-fnums))
		 (let ((qrew (list 'quote !rew))
		       (qret (list 'quote !ret)))
		   (mapstep@ #'(lambda (x)`(rewrite ,x :fnums ,qrew :target-fnums ,qret
						    :dir ,qdir :order ,qorder :dont-delete? ,qdont))
			     lms))))
   "[Extrategies] Rewrites with a list of lemmas or fnums. LEMMAS-OR-FNUMS has the form
(LEMMAS-OR-FNUMS1 ... LEMMAS-OR-FNUMS). Options are as in rewrite."
  "Rewriting with ~a")

(defstep rewrite* (lemmas-or-fnums &optional (fnums *) (target-fnums *) (dir lr) (order in) dont-delete?)
   (let ((lms    (enlist-it lemmas-or-fnums))
	 (qdir   (list 'quote dir))
	 (qorder (list 'quote order))
	 (qdont  (list 'quote dont-delete?)))
     (with-fnums ((!rws fnums)
		  (!rwt target-fnums))
		  (let ((qrws (list 'quote !rws))
			(qrwt (list 'quote !rwt)))
		    (repeat
		     (mapstep@ #'(lambda (x)`(rewrite ,x :fnums ,qrws :target-fnums ,qrwt
						      :dir ,qdir :order ,qorder :dont-delete? ,qdont))
			       lms)))))
   "[Extrategies] Recursively rewrites LEMMAS-OR-FNUMS on the main branch. Options are as in rewrites."
   "Rewriting recursively with ~a")

(defstrat extrategies-about ()
  (let ((version *extra-version*)
	(strategies *extra-tegies*)) 
    (printf "%--
% ~a
% http://shemesh.larc.nasa.gov/people/cam/Extrategies
% Strategies in Extrategies:~a
%--~%" version strategies))
  "[Extrategies] Prints Extrategies's about information.")

;;; EXPERIMENTAL EXTRATEGIES

;;; Induction

(defhelper inductionfree__ (recvar &optional first)
  (let ((name (freshname "V"))
	(pre  (car (eval-ext-expr `(! * (-> ,recvar)))))
	(term (when pre (format nil "~a" (ee-pvs-obj pre)))))
    (if term
	(branch (name-replace name term :hide? t)
		((inductionfree__$ name)
		 (skip)))
      (unless
       first
       (typepred recvar))))
  "[Extrategies] Internal strategy." "")

(defstrat inductionfree (&optional (recvar "v"))
  (if (forall-expr? (extra-get-formula 1))
      (let ((recvar (format nil "~a!1" recvar)))
	(then (skosimp* :preds? t)
	      (repeat (inductionfree__$ recvar t))
	      (assert)))
    (then
     (repeat (inductionfree__$ recvar t))
     (assert)))
  "[Extrategies] Extracts induction free principle from definition of recursive function. RECVAR is the
name of the quantified variable that encodes the recursive call.")

;;;;; SPECIFIC FUNCTIONS ;;;;; 

;; Used in splash for extracting conjuctive expressions in the consequent

(defun get-ands-expr (expr is-and)
  (cond ((or (and is-and (conjunction? expr))
             (and (not is-and) (disjunction? expr)))
	 (append (get-ands-expr (args1 expr) is-and)
		 (get-ands-expr (args2 expr) is-and)))
	((and (not is-and) (implication? expr))
	 (append (get-ands-expr (args1 expr) t)
		 (get-ands-expr (args2 expr) nil)))
	(is-and (list expr))
	(t      (list (mk-negation expr)))))

(defun undef-rel (f1 f2 o1 o2)
  (and (= (* o1 o2) 0)
       (or (and (> f1 0) (> f2 0))
	   (and (/= o2 0) (> f1 0))
	   (and (/= o1 0) (> f2 0)))))

(defun abs-symm-rel (op)
  (let ((aop (abs op)))
    (cond ((= aop 0) op)
          ((= aop 1) 2)
	  ((= aop 2) 1))))

(defun new-relation (f1 f2 o1 o2)
  (cond ((= o1 0) o2)
	((= o2 0) o1)
	(t 
	 (let ((op (cond ((and (< f1 0) (< f2 0))
			  (max (abs o1) (abs o2)))
			 ((and (> f1 0) (> f2 0))
			  (min (abs o1) (abs o2)))
			 ((< f1 0) 
		   (max (abs o1) (abs-symm-rel o2)))
			 (t 
			  (min (abs o1) (abs-symm-rel o2))))))
	      (* (sign o1) op)))))
	
(defun merge-names-exprs (names exprs)
  (when names (cons (car names) (cons (car exprs)
				      (merge-names-exprs (cdr names)
							 (cdr exprs))))))

(defun make-exprs (names expr n)
  (when names (cons (format nil "(~a)`~a" expr n)
		    (make-exprs (cdr names) expr (+ n 1)))))

(defun carands (ands names name var postfix)
  (cond ((conjunction? ands)
	 (or (carands (args1 ands) names name var postfix)
	     (carands (args2 ands) names name var postfix)))
	((and (equation? ands) (name-expr? (args1 ands)))
	 (let ((nm (format nil "~a" (id (args1 ands)))))
	   (when (or (not name) (string= nm name))
	     (let ((p (position nm names :test #'string=)))
	       (when p
		 (list (+ p 1)
		       (freshname (or var (format nil "~a~a" nm postfix)))
		       (format nil "~a" (args2 ands))))))))))

(defun fillunder (n)
  (when (> n 0) (cons '_ (fillunder (- n 1)))))

(defun instvar_aux (vn n i)
  (cond ((> i n) nil)
	((eq (cdr vn) i) (cons (car vn) (fillunder (- n i))))
	(t (cons '_ (instvar_aux vn n (+ i 1))))))

(defun instvar (vn n)
  (instvar_aux vn n 1))
  
(defun merge-let-names (names newnames postfix)
  (when names
    (cond ((and newnames (string= (car newnames) '_))
	   (cons (format nil "~a~a" (car names) postfix)
		 (merge-let-names (cdr names) (cdr newnames) postfix)))
	  (newnames 
	   (cons (car newnames) 
		 (merge-let-names (cdr names) (cdr newnames) postfix)))
	  (t (mapcar #'(lambda (x) (format nil "~a~a" x postfix))
		     names)))))

;; Used in skoletin to return an expression
(defstruct ret expr)

(defun sigmared-list (exprs name n l &key newnames postfix ret)
  (if exprs
      (let ((e (sigmared (car exprs) name n :newnames newnames
			 :postfix postfix :ret ret)))
	(if (numberp e)
	    (sigmared-list (cdr exprs) name n (append l (list (car exprs)))
			   :newnames newnames :postfix postfix :ret ret)
	  (append l (cons e (cdr exprs)))))
    n))
  
(defun sigmared (expr name n &key newnames postfix ret)
  (cond
   ((<= n 0) n)
   ((let-expr? expr)
    (let* ((namexpr (argument expr))
	   (e       (sigmared namexpr name n :newnames newnames
			      :postfix postfix :ret ret)))
      (if (numberp e)
	  (let* ((names   (mapcar  #'(lambda(x) (format nil "~a" (id x)))
				   (bindings (operator expr))))
		 (onevar  (not (cdr names)))
		 (types   (mapcar #'(lambda(x) (if (dep-binding? x) (type x) x))
				      (if onevar
					  (list (domain (type (operator expr))))
					(types (domain (type (operator expr)))))))
		 (letexpr (expression (operator expr)))
		 (typelet (range (type (operator expr))))
		 (m       (if (or (not name) (member name names
						     :test #'string=))
			      (- e 1)
			    e)))
	    (cond ((and (= m 0) (or onevar (not name)))
		   ;; Let-in single variable or
		   ;; Let-in any name
		   (let*
		       ((mergenames (when ret (names2freshnames
					       (merge-let-names names
								newnames  
								postfix))))
			(newnamexpr (if ret (format nil "~{~a~^,~}" mergenames)
				      namexpr))
			(newexprs   (when ret
				      (cond (onevar (list namexpr))
					    ((tuple-expr? namexpr)
					     (exprs namexpr))
					    (t (make-exprs names namexpr 1)))))
			(strexprs   (mapcar #'(lambda(x)(format nil "~a" x))
					    newexprs))
			(newtypes   (merge2str names types :conc ":" :sep ","))
			(lbdapp     (format nil "(LAMBDA (~a):~a)(~a)"
					    newtypes letexpr newnamexpr))
			(pcexpr     (pc-parse lbdapp 'expr)))
		     (progn 
		       (when ret (setf (ret-expr ret)
				       (merge-names-exprs mergenames
							  strexprs)))
		       (setf (type pcexpr) (type expr))
		       pcexpr)))
		  ((= m 0)
		   ;; Let-in multiple variable, reducing one variable
		   (let*
		       ((p     (position name names :test #'string=))
			(prj   (if (tuple-expr? namexpr)
				   (format nil "~a" (nth p (exprs namexpr)))
				   (format nil "~a`~a" namexpr (+ p 1))))
			(mergename (when ret
				     (freshname (if newnames (car newnames)
						  (format nil "~a~a"
							  name postfix)))))
			(newnamexpr (if ret mergename prj))
			(lbdapp (format nil "(LAMBDA (~a:~a):~a)(~a)"
					name (nth p types)
					letexpr newnamexpr))
			(nexpr (pc-parse lbdapp 'expr))
			(dummy (setf (type nexpr) typelet))
			(nargs (if (tuple-expr? namexpr)
				   (format nil "~{~a~^,~}" (removepos p (exprs namexpr)))
				 (merge2str (list namexpr) 
					    (removepos
					     p (fromto 1 (length names)))
					    :conc "`" :sep ",")))
			(argexpr (pc-parse (format nil "(~a)" nargs) 'expr))
			(typetup (copy (type namexpr)
				   'types
				   (removepos p (types (type namexpr))))))
		     (progn
		       (when ret (setf (ret-expr ret) (list mergename prj)))
		       (setf (type argexpr) typetup)
		       (copy expr
			     'argument
			     argexpr
			     'operator
			     (copy (operator expr)
				   'expression nexpr
				   'type
				   (copy (type (operator expr))
					 'domain
					 (copy (domain (type
							(operator expr)))
					       'types
					       (removepos p types)))
				   'bindings
				   (removepos p (bindings
						 (operator expr))))))))
		  (t ;; Recursion
		   (let ((f (sigmared letexpr name m :newnames newnames
				      :postfix postfix :ret ret)))
		     (if (numberp f) f
		       (copy expr 'operator
			     (copy (operator expr)
				   'expression f)))))))
	(copy expr 'argument e))))
   ((infix-application? expr)
    (let ((e (sigmared (args1 expr) name n :newnames newnames
		       :postfix postfix :ret ret)))
      (if (numberp e)
	  (let ((f (sigmared (args2 expr) name e :newnames newnames
			     :postfix postfix :ret ret)))
	    (if (numberp f) f
	      (copy expr 'argument
		    (copy (argument expr)
			  'exprs
			  (list (args1 expr) f)))))
	(copy expr 'argument
	      (copy (argument expr)
		    'exprs
		    (list e (args2 expr)))))))
   ((unary-application? expr)
    (let ((e (sigmared (argument expr) name n :newnames newnames
		       :postfix postfix :ret ret)))
      (if (numberp e) e
	(copy expr 'argument e))))
   ((tuple-expr? expr)
    (let ((e (sigmared-list (exprs expr) name n nil :newnames newnames
			    :postfix postfix :ret ret)))
      (if (numberp e) e
	(copy expr 'exprs e))))
   (t n)))

    
