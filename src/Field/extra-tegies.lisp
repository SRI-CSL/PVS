;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXTRA-TEGIES
;; Cesar A Munoz (munoz@nianet.org)
;; National Institute of Aerospace, February 2003
;; ICASE-NASA LaRC
;; Version: Field.2b (for PVS 3.1 and Manip 1.1)
;; $Id: extra-tegies,v 1.2 2007/09/14 19:34:32 owre Exp $
;;
(defvar field_ver__ "2b")

;; Global variable to generate new names.
(defvar field_var__ 0)

;;;;; GENERAL PURPOSE FUNCTIONS ;;;;; 

;; Generates a new name with given prefix.
(defun newname (prefix)
  (progn (setq field_var__ (+ 1 field_var__))
	 (format nil "~A~A__" prefix field_var__)))

;; Translates a symbol (*,+,-) or label into a list of formula numbers,
(defun extra-map-fnums (fnums)
  (cond ((numberp fnums) (list fnums))
	((or (stringp fnums)(symbolp fnums))
	 (gather-fnums (s-forms *goal*)
		       fnums nil true-predicate))
	((listp fnums) (when fnums (append (extra-map-fnums (car fnums))
					   (extra-map-fnums (cdr fnums)))))
	(t nil)))

;; Returns list of formula numbers not in fnums
(defun extra-map-but-fnums (fnums &key (all '*))
  (let ((forms (extra-map-fnums all))
	(but   (extra-map-fnums fnums)))
    (remove-if #'(lambda(x)(member x but)) forms)))
    
;; Gets a formula number given a FNUM, which can be a label.
(defun extra-get-fnum (fnum)
  (when fnum
    (cond ((and (numberp fnum) (= fnum 0)) nil)
	  ((numberp fnum) fnum)
	  ((listp fnum) (extra-get-fnum (car fnum)))
	  (t (car (extra-map-fnums fnum))))))

;; Get a sequent given a FNUM, which can be a label.
(defun extra-get-seq (fnum)
  (let ((f (extra-get-fnum fnum)))
    (when f
      (let* ((fs    (if (> f 0)	(p-sforms *goal*) (n-sforms *goal*)))
	     (index (- (abs f) 1)))
	(nth index fs)))))

;; Get a formula given a FNUM, which can be a label.
(defun extra-get-formula (fnum)
  (let* ((f   (extra-get-fnum fnum))
	 (seq (extra-get-seq f)))
    (when seq
      (if (> f 0)
	  (formula seq)
	(argument (formula seq))))))

;; Get list of labels of a given FNUM
(defun get-labels (fnum)
  (let ((seq (extra-get-seq fnum)))
    (when seq (mapcar #'(lambda(x)(format nil "~A" x)) (label seq)))))

;; Antecedent?
(defun is-antecedent (fnum)
  (< (extra-get-fnum fnum) 0))

;; Consequent?
(defun is-consequent (fnum)
  (> (extra-get-fnum fnum) 0))

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

;; Creates a list of n new names with given prefix. 
(defun newnames (prefix n)
  (cond ((> n 0) (cons (newname prefix) (newnames prefix (- n 1))))
	(t nil))
)
;; Creates a list of names with given prefix and postfix. Midfixes are taken
;; from l.
(defun listnames (prefix l postfix)
  (mapcar #'(lambda(x)(format nil "~A~A~A" prefix x postfix)) l))

;; Sign of n (note that 0 returns 1).
(defun sign (n)
  (if (>= n 0) 1 -1))
  
;; String to int.
(defun str2int (str)
  (multiple-value-bind (n l) (parse-integer str :junk-allowed t)
    (when (and n (= (length str) l)) n)))

;; Expression to string (tries to minimize parentheses)
(defun expr2str (expr)
  (cond ((stringp expr) expr)
	((numberp expr) (format nil "~A" expr))
	((and (infix-application? expr)
	      (= (parens expr) 0)
	      (not (is-relation expr)))
	 (format nil "(~A)" expr))
	((and (or (name-expr? expr)
		  (number-expr? expr))
	      (> (parens expr) 0))
	 (format nil "~A" (copy expr 'parens 0)))
	(t (format nil "~A" expr))))

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

;; Merges two lists in one string using 
;; :empty as the empty-string
;; :conc as the string-concatenator
;; :sep as the string-separator
;; For instance (merge2str '("a" "b" "c") '("A" "B" "C") :conc "-" :sep ",")
;; returns "a-A,b-B,c-C"

(defun merge2str (l1 l2 &key (empty "") (conc "") (sep ""))
  (cond ((and (cdr l1) (cdr l2))
	 (format nil "~A~A~A~A~A" (car l1) conc (car l2) sep
		 (merge2str (cdr l1) (cdr l2) :conc conc :sep sep)))
	((and (cdr l1) l2)
	 (format nil "~A~A~A~A~A" (car l1) conc (car l2) sep
		 (merge2str (cdr l1) l2 :conc conc :sep sep)))
	((cdr l1)
	 (format nil "~A~A~A~A" (car l1) conc sep
		 (merge2str (cdr l1) l2 :conc conc :sep sep)))
	((and l1 (cdr l2))
	 (format nil "~A~A~A~A~A" (car l1) conc (car l2) sep
		 (merge2str l1 (cdr l2) :conc conc :sep sep)))
	((cdr l2)	
	 (format nil "~A~A~A~A" conc (car l2) sep
		 (merge2str l1 (cdr l2) :conc conc :sep sep)))
	((and l1 l2) 
	 (format nil "~A~A~A" (car l1) conc (car l2)))
	(l1
	 (format nil "~A~A" (car l1) conc))
	(l2
	 (format nil "~A~A" conc (car l2)))
	(t (format nil "~A" empty))))

;; Field's standard message for an invalid formula number.
(defun no-formula (fnum)
  (format nil "No sequent formula(s) corresponding to ~A" fnum))

;; Field's standard message for an invalid arithmetic relational formula.
(defun no-relation (fnum)
  (format nil "No arithmetic relational formula(s) in ~A" fnum))

;; Field's standard message for no suitable formula.
(defun no-suitable (fnum)
  (format nil "No suitable formula(s) in ~A" fnum))

;; Field's standard message for no suitable expression.
(defun no-suitable-expr (expr)
  (format nil "No suitable expression(s) in ~A" expr))

;; Structure for returning expressions
(defstruct ret expr)

;;;;; SPECIFIC FUNCTIONS ;;;;; 

(defun else-rewrite (names)
  (let ((step (if (not names)
                  '(skip)
                  (let ((rewcar (if (atom (car names))
				    `(rewrite ,(car names))
			  	    `(rewrite ,(caar names)
					      :target-fnums ,(cadr names)))))
		       `(else ,rewcar
			      ,(else-rewrite (cdr names)))))))
       step)
)

(defun get-distrib-plus (l expr)
  (cond ((or (is-infix-operator expr '+)
	     (is-infix-operator expr '-))
	 (adjoin (expr2str expr) l :test #'string=))
	((is-infix-operator expr '/)
	 (get-distrib-expr (get-distrib-expr l (args1 expr))
			   (args2 expr)))
	((is-infix-operator expr '*)
	 (get-distrib-plus (get-distrib-plus l (args1 expr))
			   (args2 expr)))
	(t l)))

(defun get-distrib-expr (l expr)
  (cond ((or (is-infix-operator expr '+)
	     (is-infix-operator expr '-)
	     (is-infix-operator expr '/)
	     (is-relation expr))
	 (get-distrib-expr (get-distrib-expr l (args1 expr))
			   (args2 expr)))
	((is-prefix-operator expr '-)
	 (get-distrib-expr l (args1 expr)))
	((is-infix-operator expr '*)
	 (get-distrib-plus (get-distrib-plus l (args1 expr))
			   (args2 expr)))
	(t l)))

(defun get-distrib-formulas (l fnums)
  (when fnums
    (let ((formula (extra-get-formula (car fnums)))
	  (nl      (get-distrib-formulas l (cdr fnums))))
      (if formula
	  (get-distrib-expr nl formula)
	nl))))

(defun get-ands-expr (l expr is-and)
  (cond ((or (and is-and (conjunction? expr))
             (and (not is-and) (disjunction? expr)))
	 (get-ands-expr
	  (get-ands-expr l (args1 expr) is-and) (args2 expr) is-and))
	((and (not is-and) (implication? expr))
	 (append (get-ands-expr l (args2 expr) is-and) 
		 (cons (expr2str (args1 expr)) nil)))
	(is-and (cons (expr2str expr) l))
	(t      (append l (cons (format nil "NOT(~A)" expr) nil)))))

(defun ord2num (expr)
  (cond ((is-infix-operator expr '<)  -2)
        ((is-infix-operator expr '<=) -1)
        ((is-infix-operator expr '=)   0)
        ((is-infix-operator expr '>=)  1)
        ((is-infix-operator expr '>)   2)
        (t                nil)))

(defun num2ord (num)
  (cond ((= num -2) "<")
        ((= num -1) "<=")
        ((= num 0) "=")
        ((= num 1) ">=")
        ((= num 2) ">")
        (t nil)))

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
  (when names (cons (format nil "(~A)`~A" expr n)
		    (make-exprs (cdr names) expr (+ n 1)))))

(defun carands (ands names name var postfix)
  (cond ((conjunction? ands)
	 (or (carands (args1 ands) names name var postfix)
	     (carands (args2 ands) names name var postfix)))
	((and (equation? ands) (name-expr? (args1 ands)))
	 (let ((nm (format nil "~A" (id (args1 ands)))))
	   (when (or (not name) (string= nm name))
	     (let ((p (position nm names :test #'string=)))
	       (when p
		 (cons (+ p 1)
		       (cons (or var (format nil "~A~A" nm postfix))
			     (format nil "~A" (args2 ands)))))))))
	(t nil)))

(defun fillunder (n)
  (when (> n 0) (cons "_" (fillunder (- n 1)))))

(defun instvar_aux (vn n i)
  (cond ((> i n) nil)
	((eq (cdr vn) i) (cons (car vn) (fillunder (- n i))))
	(T (cons "_" (instvar_aux vn n (+ i 1))))))

(defun instvar (vn n)
  (instvar_aux vn n 1))
  
(defun merge-let-names (names newnames postfix)
  (when names
    (cond ((and newnames (string= (car newnames) "_"))
	   (cons (format nil "~A~A" (car names) postfix)
		 (merge-let-names (cdr names) (cdr newnames) postfix)))
	  (newnames 
	   (cons (car newnames) 
		 (merge-let-names (cdr names) (cdr newnames) postfix)))
	  (t (mapcar #'(lambda (x) (format nil "~A~A" x postfix))
		     names)))))

(defun sigmared (expr name n &key newnames postfix ret)
  (cond
   ((<= n 0) n)
   ((let-expr? expr)
    (let* ((namexpr (argument expr))
	   (e       (sigmared namexpr name n :newnames newnames
			      :postfix postfix :ret ret)))
      (if (numberp e)
	  (let* ((names   (mapcar  #'(lambda(x) (format nil "~A" (id x)))
				   (bindings (operator expr))))
		 (onevar  (not (cdr names)))
		 (types   (if onevar
			      (list (domain (type (operator expr))))
			    (types (domain (type (operator expr))))))
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
		       ((mergenames (when ret (merge-let-names names
							       newnames  
							       postfix)))
			(newnamexpr (if ret (merge2str mergenames nil :sep ",")
				      namexpr))
			(newexprs   (when ret
				      (cond (onevar (list namexpr))
					    ((tuple-expr? namexpr)
					     (exprs namexpr))
					    (t (make-exprs names namexpr 1)))))
			(strexprs   (mapcar #'(lambda(x)(format nil "~A" x))
					    newexprs))
			(newtypes   (merge2str names types :conc ":" :sep ","))
			(pcexpr     (pc-parse (format
					       nil "(LAMBDA (~A):~A)(~A)"
					       newtypes letexpr newnamexpr)
					      'expr)))
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
				   (format nil "~A" (nth p (exprs namexpr)))
				   (format nil "~A`~A" namexpr (+ p 1))))
			(mergename (when ret
				     (if newnames (car newnames)
					 (format nil "~A~A"
					   name postfix)))) 
			(newnamexpr (if ret mergename prj))
			(nexpr (pc-parse (format nil "(LAMBDA (~A:~A):~A)(~A)"
					   name (nth p types)
					   letexpr newnamexpr)
				 'expr))
			(dummy (setf (type nexpr) typelet))
			(nargs (if (tuple-expr? namexpr)
				   (merge2str (removepos p
							 (exprs namexpr))
					      nil :sep ",")
				   (merge2str (list namexpr) 
					      (removepos
					       p (fromto 1 (length names)))
					      :conc "`" :sep ",")))
			(argexpr (pc-parse (format nil "(~A)" nargs) 'expr))
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

(defun sigmared-list (exprs name n l &key newnames postfix ret)
  (if exprs
      (let ((e (sigmared (car exprs) name n :newnames newnames
			 :postfix postfix :ret ret)))
	(if (numberp e)
	    (sigmared-list (cdr exprs) name n (append l (list (car exprs)))
			   :newnames newnames :postfix postfix :ret ret)
	  (append l (cons e (cdr exprs)))))
    n))
  
;;;;; GENERAL PURPOSE STRATEGIES ;;;;; 

(defstep relabel* (flabels fnums &optional (pairing? t))
  (let ((flag (and flabels fnums)))
       (when flag
	     (let ((label      (car flabels))
		   (labelrest  (cdr flabels))
		   (fn         (extra-map-fnums fnums))
		   (fnum       (if pairing? (car fn) fnums))
		   (rest       (if pairing? (cdr fn) fnums)))
		  (if labelrest
		      (then (relabel label fnum)
			    (relabel* labelrest rest))
		      (relabel label fnums)))))
  "[Field Package] Labels a list of formulas FNUMS using a list of labels FLABELS. If PAIRING? is t, pairs FLABELS with FNUMS, otherwise labels all FNUMS with FLABELS."
  "Labeling with labels ~A formulas ~A"
)

(defstep unlabel* (fnums flabels)
  (if (listp flabels)
      (let ((label (when flabels (car flabels)))
	    (rest  (when flabels (cdr flabels))))
	   (when flabels
		 (unlabel fnums label)
		 (unlabel* fnums rest)))
      (unlabel fnums flabels))
  "[Field Package] Iterates UNLABEL with FNUMS and the list of labels FLABELS."
  "~%Unlabeling in ~A labels ~A"
)

(defstep delabel (flabels &optional hide?)
  (let ((fnums (extra-map-fnums flabels)))
       (when fnums
	     (unlabel* fnums flabels)
	     (when hide? (hide fnums))))
  "[Field Package] Removes specified list of labels FLABELS. Hides the formulas if hide? is T."
  "~%Delabeling ~A"
)

(defstep relabel (label fnums)
  (label label fnums :push? t)
  "[Field Package] Pushes label LABEL in FNUMS."
  "~%Pushing label ~A in formula ~A"
)

(defstrat for (n step)
  (if (or (not (numberp n)) (= n 0))
      (skip)
      (if (< n 0) (repeat step)
	  (let ((m (- n 1)))
	       (then step
		     (for m step)))))
"[Field Package] Repeats N times STEP, or until it does nothing if N < 0, in all the branches."
)

(defstrat for@ (n step)
  (if (or (not (numberp n)) (= n 0))
      (skip)
      (if (< n 0) (repeat@ step)
	  (let ((m (- n 1)))
	       (then@ step
		      (for m step)))))
  "[Field Package] Repeats N times STEP, or until it does nothing if N < 0, in the main branch."
)

(defstrat loop (step &optional n)
  (if (numberp n)
      (for n step)
      (repeat step))
  "[Field package] Repeats N times STEP (or until it does nothing if N is nil) in all the branches."
)

(defstrat loop@ (step &optional n)
  (IF (numberp n)
      (for@ n step)
      (repeat@ step))
  "[Field package] Repeats N times STEP (or until it does nothing if N is nil) in the main branch."
)

(defstrat when (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then steps)))
	   step)
      (skip))
  "[Field Package] An abbreviation for (IF flag (THEN step1 .. step n) (SKIP)). Because of the execution model of strategies in PVS, FLAG *must* be a simple variable."
)

(defstrat when@ (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then@ steps)))
	   step)
      (skip))
  "[Field Package] An abbreviation for (IF flag (THEN@ step1 .. step n) (SKIP)). Because of the execution model of strategies in PVS, FLAG *must* be a simple variable."
)

(defstrat when-not (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then steps)))
	   step)
      (skip))
  "[Field Package] An abbreviation for (IF (not flag) (THEN step1 .. step n) (SKIP)). Because of the execution model of strategies in PVS, FLAG *must* be a simple variable."
)

(defstrat when-not@ (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then@ steps)))
	   step)
      (skip))
  "[Field Package] An abbreviation for (IF (not flag) (THEN@ step1 .. step n) (SKIP)). Because of the execution model of strategies in PVS, FLAG *must* be a simple variable."
)

;;;;;  EXTRA-TEGIES ;;;;; 

;; REWRITE-NAMES, REWRITE*, and SQ-SIMP are not official extra-tegies.
;; They are here because I don't know where else to put them.  They may
;; disappear from this package in the future, and re-appear in a better
;; location.

(defstep rewrite-names (names)
  (let ((step (else-rewrite names)))
       step)
  "[Field Package] Rewrites with a list of NAMES. A NAME can be name-lema OR (name-lema FNUMS)."
  "~%Rewriting with ~A"
)

(defstep rewrite* (names)
  (repeat (rewrite-names names))
  "[Field Package] Rewrites recursively REWRITE-NAMES. NAMES as in REWRITE-NAMES."
  "~%Rewriting recursively with ~A"
)

(defstep sq-simp ()  
  (rewrite* ("sq_neg" "sq_times" "sq_plus" "sq_minus" "sq_0"
  	     "sq_div" "sqrt_0" "sqrt_1" "sqrt_def" "sqrt_square"
             "sqrt_sq" "sq_sqrt" "sqrt_times" "sqrt_div"))
  "[Field Package] Simplifies sq and sqrt."
  "~%Simplifying sq and sqrt"
) 

(defstep cut (cases)
  (when cases
	(let ((nc (car cases))
	      (nl (cdr cases)))
	     (branch (case nc)
		     ((cut nl)
		      (skip)))))
  "[Field Package] Cuts one by one the formulas in CASES."
  "~%Cutting ~A"
)

(defstep splash (fnum)
  (let ((formula (extra-get-formula fnum))
	(msg     (no-formula fnum)))
       (if formula 
	   (let ((cases1 (get-ands-expr nil formula (is-consequent fnum)))
		 (cases2 (when cases1 (reverse (cdr cases1)))))
		(when cases2
		      (relabel "SP:" fnum)
		      (branch (cut cases2)
			      ((then (split "SP:")
				     (delabel "SP:"))
			       (delabel "SP:" :hide? T)))))
	   (skip-msg msg)))
  "[Field Package] Asymmetrically splits a conjunction FNUM in the consequent (or a disjunction in the antecedent)."
  "~%Splashing formula ~A"
)

(defstep real-props (&optional (fnums '*) (theories "extra_tegies")
			       (let-reduce? t) (distrib? t))
  (let ((th    (cons "real_props"
		     (if (listp theories) theories
		       (list theories))))
	(lfnums (if distrib? fnums "RP:"))
	(step   (list 'then 
		      (cons 'auto-rewrite-theories th)
		      (list 'assert lfnums :let-reduce? let-reduce?)
		      (cons 'stop-rewrite-theory th))))
       (if distrib?
	   step
	   (then  (relabel "RP:" fnums)
		  (name-distrib fnums :label "ND:")
		  step
		  (replaces "ND:" :dir rl :in "RP:" :but "ND:" :hide? nil)
		  (delabel "ND:" :hide? t)
		  (delabel "RP:"))))
  "[Field Package] Autorewrites with \"real_props\" and THEORIES in FNUMS. If LET-REDUCE? is nil, let-in expressions will not be reduced. If DISTRIB? is nil, distribution laws will be blocked."
  "~%Applying REAL-PROPS"
)

(defstep mult-by__ (flabel expr &optional (sign +))
  (let ((fnum    (extra-get-fnum flabel))
	(flabels (get-labels fnum))
        (f       (when fnum (sign fnum))))
       (when fnum
	     (branch (mult-by fnum expr sign)
		     ((let ((flag    (not (extra-map-fnums flabel))))
			   (when flag (relabel* flabels f :pairing? nil)))
		      (assert)))))
  "[Field Package] Multiplies both sides of formula FLABEL by EXPR. In contrast to MULT-BY, this strategy preserves the label."
  "~%Multiplying in ~A by ~A"
)

(defhelper replaces__ (flabels inlabels hide? dir)
  (when flabels
	(let ((fnum   (when flabels (car flabels)))
	      (frest  (when flabels (cdr flabels))))
	     (then  (replace fnum inlabels :hide? hide? :dir dir)
		    (replaces__ frest inlabels hide? dir))))
  "[Field Package: Internal strategy]. Use REPLACES instead."
  ""
)

(defstep replaces (&optional (fnums '*) (in '*) but from to
				    (hide? t) (dir lr))
  (let ((flist (extra-map-fnums fnums))
	(nfrom (extra-get-fnum from))
	(nto   (extra-get-fnum to))
	(feqs  (remove-if-not #'(lambda (x) (< x 0))
	         (cond ((and from to) (fromto nfrom nto))
		       (from (remove-before nfrom flist))
		       (to   (remove-after nto flist))
		       (t    flist)))))
       (when feqs
	     (let ((flabels (listnames "EQ" feqs ":"))
		   (forms   (extra-map-but-fnums but :all in)))
		  (then (relabel* flabels feqs)
			(relabel "RE:" forms)
			(replaces__ flabels "RE:" nil dir)
			(delabel flabels :hide? hide?)
			(delabel "RE:")))))
  "[Field Package] Iterates the PVS command REPLACE to rewrite with the formulas in FNUMS, respecting the order, the formulas in IN but not in BUT. The keys DIR and HIDE? are like in REPLACE. Notice that in contrast to REPLACE, the default value of HIDE? is T. Instead of using FNUMS, rewriting formulas can be addressed via FROM and TO. The key STEP specifies the command to be executed after all the replaces have taken place."
  "~%Iterating REPLACE"
)

(defstrat name-replace*__ (names-and-exprs flabels dir hide?)
  (when names-and-exprs
	(let ((name (car  names-and-exprs))
	      (expr (cadr names-and-exprs))
	      (rest (cddr names-and-exprs)))
	     (try-branch
	       (name name expr)
	       ((then (replace -1 flabels)
		      (let ((flag (string-equal dir "rl")))
			   (when flag (swap-rel -1)))
		      (when hide? (hide -1))
		      (name-replace*__ rest flabels dir hide?))
		(assert))
	       (let ((msg (format nil "Error: ~A is already declared." name)))
		    (then (skip-msg msg :force-printing? t)
			  (fail))))))
  "[Field Package: Internal strategy]. Use NAME-REPLACE* instead."
)

(defstep name-replace* (names-and-exprs &optional (fnums '*) (dir lr) hide?)
  (let ((forms   (extra-map-fnums fnums))
	(flabels (listnames "NR" forms ":")))
       (when forms
	     (relabel* flabels forms)
	     (try (name-replace*__ names-and-exprs flabels dir hide?)
		  (delabel flabels)
		  (fail))))
  "[Field Package] Iterates NAME-REPLACE with names-and-exprs in FNUMS. DIR indicates the direction of the name definition. Use HIDE? to hide the name definition."
  "~%Iterating NAME-REPLACE with ~A"
)

(defstrat name-replace-label__ (name expr label flabels dir hide?)
  (let
   ((nlabel (or label (format nil "~A:" name))))
   (try-branch (name name expr)
	       ((then (replace -1 flabels)
		      (let ((flag (string-equal dir 'rl)))
			   (when flag (swap-rel -1)))
		      (relabel nlabel -1)
		      (when hide? (hide -1)))
		(assert))
	       (let
		((msg (format nil "Error: ~A is already declared." name)))
		(then (skip-msg msg :force-printing? t)
		      (fail)))))
  "[Field Package: Internal strategy]. Use NAME-REPLACE-LABEL instead."
)

(defstep name-replace-label (name expr &optional label (fnums '*) (dir lr)
				  hide?)
  (let ((forms   (extra-map-fnums fnums))
	(flabels (listnames "NRL" forms ":")))
       (when (or forms (not fnums))
	     (relabel* flabels forms)
	     (try (name-replace-label__ name expr label flabels dir hide?)
		  (delabel flabels)
		  (fail))))
  "[Field Package] Uses NAME, REPLACE, and LABEL to abbreviate an expression in FNUMS with a newly chosen name and label. If LABEL is nil, NAME: is used as label. DIR indicates the direction of the name definition. Use HIDE? to hide the name definition."
  "~%Replacing with name ~A expression ~A"
)
  
(defstrat name-replace-label*__ (names-and-exprs label flabels dir hide?)
  (when names-and-exprs
	(let ((name (car  names-and-exprs))
	      (expr (cadr names-and-exprs))
	      (rest (cddr names-and-exprs)))
	     (try-branch
	      (name-replace-label__ name expr label flabels dir hide?)
	      ((name-replace-label*__ rest label flabels dir hide?)
	       (skip))
	      (fail))))
  "[Field Package: Internal strategy]. Use NAME-REPLACE-LABEL* instead."
)

(defstep name-replace-label* (names-and-exprs
			      &optional label (fnums '*) (dir lr) hide?)
  (let ((forms   (extra-map-fnums fnums))
	(flabels (listnames "NRLS" forms ":")))
       (when (or forms (not fnums))
	     (relabel* flabels forms)
	     (try
	      (name-replace-label*__ names-and-exprs label flabels dir hide?)
	      (delabel flabels)
	      (fail))))
  "[Field Package] Iterates NAME-REPLACE-LABEL with names-and-exprs."
  "~%Iterating NAME-REPLACE-LABEL with ~A"
)

(defstrat name-distrib (&optional (fnums '*) (prefix "ND") label hide?)
  (let ((dist (get-distrib-formulas nil (extra-map-fnums fnums))))
       (when dist
	     (let ((names   (newnames prefix (length dist)))
		   (nameseq (merge-names-exprs names dist)))
		  (if label 
		      (name-replace-label* nameseq :label label
					   :fnums fnums :hide? hide?)
		      (name-replace* nameseq :fnums fnums :hide? hide?)))))
  "[Field Package] Introduces new names, which are based on PREFIX, to block the automatic application of distributive laws in formulas FNUMS. Labels with LABEL the formulas where new names are defined. These formulas are hidden if HIDE? is t."
)

(defstep grind-reals (&optional defs (theories "extra_tegies") rewrites
				exclude (if-match t) (updates? t)
				polarity? (instantiator inst?)
				(let-reduce? t)
				nodistrib)
  (let ((th    (cons "real_props"
		     (if (listp theories) theories
		       (list theories))))
	(step `(grind :defs ,defs :theories ,th :rewrites ,rewrites :exclude ,exclude :if-match ,if-match :updates? ,updates? :polarity? ,polarity? :instantiator ,instantiator :let-reduce? let-reduce?)))
       (then
	(when nodistrib (name-distrib nodistrib :label "ND:"))
	(try (assert) (fail) (skip))
	step
	(when nodistrib
	      (replaces "ND:" :dir rl :but "ND:" :hide? nil)
	      (delabel "ND:" :hide? t))))
  "[Field Package] Applies GRIND with \"real_props\". Supports the same options as GRIND. Additionally, GRIND-REALS blocks distribution laws in main level expressions in the list of formulas NODISTRIB."
  "~%Applying GRIND-REALS"
)

(defstep add-formulas (fnum1 &optional fnum2 (hide? t) label)
  (let ((fn2      (or fnum2 fnum1))
	(f1       (extra-get-fnum fnum1))
	(f2       (extra-get-fnum fn2))
	(formula1 (when f1 (extra-get-formula f1)))
	(formula2 (when f2 (extra-get-formula f2)))
	(msg1     (no-formula  (format nil "either ~A, ~A" fnum1 fn2)))
	(rel1     (when f1 (is-relation formula1)))
	(rel2     (when f2 (is-relation formula2)))
	(msg2     (no-relation (format nil "either ~A, ~A" fnum1 fn2)))
	(msg3     (no-suitable (format nil "either ~A, ~A" fnum1 fn2))))
       (if (and f1 f2 rel1 rel2)
	   (let ((o1   (ord2num formula1))
		 (o2   (ord2num formula2)))
		(if (undef-rel f1 f2 o1 o2)
		    (skip-msg msg3)
		    (let ((flag (>= (* f1 f2 o1 o2) 0))
			  (f11 (format nil "~A" (args1 formula1)))
			  (f12 (format nil "~A" (if flag 
						    (args1 formula2)
						  (args2 formula2))))
			  (f21 (format nil "~A" (args2 formula1)))
			  (f22 (format nil "~A" (if flag 
						    (args2 formula2)
						  (args1 formula2))))
			  (op  (num2ord (new-relation f1 f2 o1 o2)))
			  (af11 (newname "af"))
			  (af12 (newname "af"))
			  (af21 (newname "af"))
			  (af22 (newname "af"))
			  (str (when op (format nil "~A+~A ~A ~A+~A"
						af11 af12
						op
						af21 af22))))
			 (then@
			  (name-replace af11 f11)
			  (name-replace af12 f12)
			  (name-replace af21 f21)
			  (name-replace af22 f22)
			  (when hide?
				(relabel "AD1:" fnum1)
				(relabel "AD2:" fn2))
			  (then (if label
				    (branch (case str)
					    ((relabel label -1)
					     (relabel label 1)
					     (skip)))
				    (case str))
				(assert)
				(assert)
				(rewrite af11)
				(rewrite af12)
				(rewrite af21)
				(rewrite af22)
				(assert)
				(when hide?
				      (delabel ("AD1:" "AD2:") :hide? t)))))))
	   (if (not (and formula1 formula2))
	       (skip-msg msg1)
	       (skip-msg msg2))))
  "[Field Package] Adds relational formulas FNUM1 and FNUM2. If FNUM2 is nil, adds FNUM with itself. Hides the formulas when HIDE? is t. Labels the result with LABEL (if specified)."
  "~%Adding relational formulas ~A and ~A"
)
       
(defstep neg-formula (fnum)
  (let ((formula (extra-get-formula fnum))
	(msg1    (no-formula fnum))
	(rel     (when formula (is-relation formula)))
	(msg2    (no-relation fnum))) 
       (if rel
	   (then (relabel "NF:" fnum)
		 (mult-by__ "NF:" "-1" :sign -)
		 (real-props "NF:")
		 (delabel "NF:"))
	   (if (not formula)
	       (skip-msg msg1)
	       (skip-msg msg2))))
  "[Field Package] Negates both sides of the relational formula FNUM."
  "~%Negating relational formula ~A"
)

(defstep sub-formulas (fnum1 fnum2 &optional (hide? t) label)
  (let ((f1       (extra-get-fnum fnum1))
	(f2       (extra-get-fnum fnum2))
	(formula1 (when f1 (extra-get-formula f1)))
	(formula2 (when f2 (extra-get-formula f2)))
	(msg1     (no-formula (format nil "either ~A, ~A" fnum1 fnum2)))
	(rel1     (when f1 (is-relation formula1)))
	(rel2     (when f2 (is-relation formula2)))
	(msg2     (no-relation (format nil "either ~A, ~A" fnum1 fnum2)))
	(msg3     (format nil "No suitable formulas in either ~A, ~A"
			  fnum1 fnum2)))
       (if (and f1 f2 rel1 rel2)
	   (let ((o1   (ord2num formula1))
		 (o2   (ord2num formula2)))
		(if (or (undef-rel f1 f2 o1 o2) (= f1 f2))
		    (skip-msg msg3)
		    (then (relabel "SF1:" f1)
			  (relabel "SF2:" f2)
			  (copy "SF2:")
			  (if (> f2 0)
			      (relabel "NSF2:" 1)
			      (relabel "NSF2:" -1))
			  (neg-formula "NSF2:")
			  (add-formulas "SF1:" "NSF2:" :hide? nil
					:label "SF3:")
			  (real-props "SF3:")
			  (when label (relabel label "SF3:"))
			  (delabel "NSF2:" :hide? t)
			  (delabel "SF3:")
			  (delabel ("SF1:" "SF2:") :hide? hide?))))
	   (if (not (and formula1 formula2))
	       (skip-msg msg1)
	       (skip-msg msg2))))
  "[Field Package] Subtracts relational formulas FNUM1 and FNUM2. Hides the formulas when HIDE? is t. Labels the result with LABEL (if specified)."
  "~%Substracting relational formulas ~A and ~A"
)

(defstep wrap-formula (fnum str &optional rev?)
  (let ((formula (extra-get-formula fnum))
	(f       (when formula (sign (extra-get-fnum fnum))))
	(msg1    (no-formula fnum))
	(rel     (when formula (is-relation formula)))
	(msg2    (no-relation fnum)))
       (if rel
	   (let
	    ((op      (ord2num formula))
	     (flabels (get-labels fnum))
	     (str1    (when op (format nil "~A(~A) ~A ~A(~A)"
				       str
				       (args1 formula)
				       (num2ord (if rev? (* -1 op) op))
				       str
				       (args2 formula))))
	     (str2    (when str1 (if (> f 0) (format nil "NOT(~A)" str1)
				   str1))))
	    (when op
		  (relabel "WF:" fnum)
		  (branch (case str2)
			  ((then (delabel "WF:" :hide? t)
				 (relabel* flabels f :pairing? nil))
			   (assert)))
		  (delabel "WF:")))
	   (if (not formula)
	       (skip-msg msg1)
	       (skip-msg msg2))))
  "[Field Package] Wraps both sides of the relational formula in FNUM with STR(...). If rev? is nil, the original relation is preserved, otherwise the relation is reversed."
  "~%Wrapping relational formula ~A with ~A"
)

(defstep skeep (&optional (fnum 1) preds?)
   (let ((expr (extra-get-formula fnum))
	 (msg  (no-formula fnum)))
	(if expr
	    (let ((names (when (or (forall-expr? expr) (exists-expr? expr))
			   (mapcar  #'(lambda(x) (id x)) 
				    (bindings expr)))))
		 (when names
		       (skolem fnum names preds?)(flatten)))
	    (skip-msg msg)))
   "[Field Package] SKEEP works like SKOSIMP but keeps the names of the bound variables. In case of name clashes, use SKOSIMP instead."
   "~%Skolemizing with the names of the bound variables"
)

(defstep skodef (fnum &optional name var (postfix "") hide?)
  (let
   ((expr     (extra-get-formula fnum))
    (foralleq (when expr (or (and (is-antecedent fnum)
				  (forall-expr? expr)
				  (implication? (expression expr)))
			     (and (is-consequent fnum)
				  (exists-expr? expr)))))
    (names    (when foralleq (mapcar #'(lambda(x) (format nil "~A" (id x)))
				     (bindings expr))))
    (flag     (and foralleq (or (not name)
				(member name names :test #'string=))))
    (nve      (when flag (carands (if (is-antecedent fnum)
				      (args1 (expression expr))
				    (expression expr))
				  names name var postfix)))
    (mssg     (no-suitable fnum)))
   (if nve
       (let
	((n       (car nve))
	 (v       (cadr nve))
	 (e       (cddr nve))
	 (lv      (format nil "~A:" v))
	 (casestr (format nil "(~A=~A) IFF TRUE" v e))
	 (step    (cons 'INST (cons "SKD:" (instvar (cons v n)
						    (length names))))))
	(then
	 (relabel "SKD:" fnum)
	 (try-branch
	  (name-replace-label v e :fnums nil :dir rl :hide? t)
	  ((branch step
		   ((branch (case-replace casestr)
			    ((then
			      (hide -1)
			      (when-not hide? (reveal lv) (delabel lv)))
			     (then (reveal lv)(assert))
			     (assert)))
		    (assert)))
	   (skip))
	  (fail))
	 (delabel "SKD:")))
       (if foralleq
	   (skip)
	   (skip-msg mssg))))
  "[Field Package]  Introduces a name definition <xp>=expr, where where <xp> is VAR concatenated with POSTFIX; and then, instantiates NAME in FNUM with <xp>. If VAR is nil, <xp> is NAME concatenated with POSTFIX. The wild-card for any NAME is nil. FNUM must have the form (FORALL(..,NAME,..) : NAME=expr AND ... IMPLIES ...), if it is in the antecedent, or (EXISTS (..,NAME,..):NAME=expr AND ...), if it is in the consequent.  The name definition is hidden when HIDE? is nil; it may be recalled at any time using (REVEAL \"<xp>:\")."
  "~%Instantiating a quantifier in ~A with a name definition"
)

(defstep skodef* (fnum &optional hide? (postfix "") (n -1))
  (let
   ((expr (extra-get-formula fnum))
    (msg1 (no-formula fnum))
    (msg2 (no-suitable fnum)))
   (if expr 
       (let
	((foralleq (or (and (is-antecedent fnum)
			    (forall-expr? expr)
			    (implication? (expression expr)))
		       (and (is-consequent fnum)
			    (exists-expr? expr)))))
	(if foralleq
	    (then (relabel "SKDS:" fnum)
		  (for n (skodef "SKDS:" :hide? hide? :postfix postfix))
		  (delabel "SKDS:"))
	    (skip-msg msg2)))
       (skip-msg msg1)))
  "[Field Package] Iterates N times SKODEF (or until it does nothing if N < 0) in FNUM."
  "~%Iterating SKODEF in ~A"
)

(defstep redlet (fnum &optional name (nth 1))
  (let
   ((f       (extra-get-fnum fnum))
    (expr    (extra-get-formula fnum))
    (flabels (get-labels fnum))
    (msg     (no-formula fnum)))
   (if expr
       (then (relabel "LR:" fnum)
	     (let ((nexpr   (sigmared expr name nth))
		   (flag    (not (numberp nexpr)))
		   (casestr (when flag (format nil "id(~A)" nexpr))))
		  (if flag
		      (then (branch (case casestr)
				    ((then (relabel* flabels -1 :pairing? nil)
					   (expand "id" -1 :assert? none)
					   (if (> f 0)
					       (beta (-1 "LR:"))
					       (beta -1 :let-reduce? nil)))
				     (then (relabel* flabels 1 :pairing? nil)
					   (expand "id" 1 :assert? none)
					   (if (< f 0)
					       (beta (1 "LR:"))
					       (beta 1 :let-reduce? nil)))
				     (assert)))
			    (delabel "LR:" :hide? t))
		      (fail)))
	     (delabel "LR:"))
       (skip-msg msg)))
  "[Field Package] Reduces the NTH occurrence left-right, depth-first, of a sub-expression LET NAME = ... in FNUM. If NAME is nil, reduces the NTH occurrence of a let-in expression in FNUM."
  "Reducing a let-in expression in ~A"
)

(defstep redlet* (fnum &optional (n 1))
  (let ((expr (extra-get-formula fnum))
	(msg  (no-formula fnum)))
       (if expr 
	   (then (relabel "LRS:" fnum)
		 (for n (redlet "LRS:"))
		 (delabel "LRS:"))
	   (skip-msg msg)))
  "[Field Package] Iterates N times REDLET (or until it does nothing if N < 0)."
  "~%Reducing let-in expressions in ~A"
)

(defstep skoletin (fnum &optional name (nth 1) var (postfix "")	hide?)
  (let
   ((f       (extra-get-fnum fnum))
    (expr    (extra-get-formula fnum))
    (flabels (get-labels fnum))
    (msg     (no-formula fnum)))
   (if expr
       (then (relabel "SKL:" fnum)
	     (let ((ret     (make-ret))
		   (nexpr   (sigmared expr name nth 
				      :newnames (if (listp var) var
						  (list var))
				      :postfix postfix :ret ret))
		   (retexpr (ret-expr ret))
		   (lv      (mapcar #'(lambda(x)(format nil "~A:" x))
				    (each-other retexpr t)))
		   (flag    (not (numberp nexpr)))
		   (casestr (when flag (format nil "id(~A)" nexpr))))
		  (if flag
		      (try-branch
		       (name-replace-label* retexpr
					    :fnums nil :dir rl :hide? t)
		       ((then (branch (case casestr)
				      ((then (relabel* flabels -1
						       :pairing? nil)
					     (expand "id" -1 :assert? none)
					     (if (> f 0)
						 (then (reveal lv)
						       (replaces lv
								 :but lv
								 :hide? nil
								 :dir lr)
						       (beta)
						       (assert))
						 (beta -1 :let-reduce? nil)))
				       (then (relabel* flabels 1 :pairing? nil)
					     (expand "id" 1 :assert? none)
					     (if (< f 0)
						 (then (reveal lv)
						       (replaces lv
								 :but lv
								 :hide? nil
								 :dir lr)
						       (beta)
						       (assert))
						 (beta 1 :let-reduce? nil)))
				       (assert)))
			      (delabel "SKL:" :hide? t)
			      (when-not hide? (reveal lv) (delabel lv)))
			(skip))
		       (fail))
		      (fail)))
	     (delabel "SKL:"))
       (skip-msg msg)))
  "[Field Package] Introduces a name definition <xp>=expr, where where <xp> is VAR concatenated with POSTFIX, to reduce the NTH occurrence left-right, depth-first, of a sub-expression LET NAME = ... in FNUM. If VAR is nil, <xp> is NAME concatenated with POSTFIX. The wild-card for any NAME is nil. If the NTH occurrence of NAME is a let-in expression having the form LET (x1,..,NAME,..,xn) = ... expr, VAR must be list of variable names (v1 ... vm), where m < n. In this case, name definitions v1=expr`1,...,vm=expr`m,..<xnp>=expr`n are introduced. If vi = \"_\", <xip>=expr`i is introduced. Name definitions are hidden when HIDE? is t; they may be recalled at any time with the command (REVEAL \"<xp>:\")."
  "Reducing a let-in expression in ~A"
)

(defstep skoletin* (fnum &optional (postfix "") hide? (n -1))
  (let ((expr (extra-get-formula fnum))
	(msg  (no-formula fnum)))
       (if expr 
	   (then (relabel "SKLS:" fnum)
		 (for n (skoletin "SKLS:" :postfix postfix :hide? hide?))
		 (delabel "SKLS:"))
	   (skip-msg msg)))
  "[Field Package] Iterates N times SKOLETIN (or until it does nothing if N < 0) in FNUM."
  "~%Iterating SKOLETIN in ~A"
)

(defstrat field-about ()
  (let ((msg (format nil "%%
%% Version: Field.~A. $Revision: 1.2 $
%% http://research.nianet.org/~Amunoz/Field
%% Contact and Bugs: Cesar A. Munoz (munoz@nianet.org)
%% NIA (Formerly ICASE) - NASA LaRC
%%" field_ver__ "~")))
       (skip-msg msg))
  "[Field Package] Prints FIELD's about information."
)

;; (defstrat reload-field ()
;;   (let ((a (progn (libload "Field/extra-tegies")
;; 		  (libload "Field/field-strategies")))
;; 	(msg (format nil "Loading: Field.~A" field_ver__)))
;;        (skip-msg msg))
;;   "[Field Package] Reloads the Field Package."
;; )
