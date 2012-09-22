;; extrategies.lisp

(defparameter *extra-version* "Extrategies-5.0 (10/11/12)")

(defparameter *extra-vars* 0) ;; Global variable to generate new names.

;;;;;;;;;; Utility functions and additional strategies

;; a <=> b
(defun iffequiv (a b)
  (equal (null a) (null b)))

;; Get all keys of a hash table
(defun get-hash-keys (hash)
  (loop for k being the hash-key of hash
	collect k))

;; Generates a new name with given prefix.
(defun newname (prefix)
  (incf *extra-vars*)
  (format nil "~a~a__" prefix *extra-vars*))

;; Creates a list of n new names with given prefix. 
(defun newnames (prefix n)
  (loop for i from 1 to n
	collect (newname prefix)))

;; Creates a list of names with given prefix and postfix. Midfixes are taken
;; from l.
(defun listnames (prefix l postfix)
  (mapcar #'(lambda(x)(format nil "~A~A~A" prefix x postfix)) l))

(defun newlabel (prefix)
  (format nil "~a:" (newname prefix)))

;; Generates a name with given prefix that is free in the current context
(defun freename (prefix)
  (when *current-context*
    (if (check-name prefix)
	(loop for i from 0
	      for nn = (format nil "~a~a" prefix i)
	      when (not (check-name nn))
	      return nn)
      prefix)))

;; Check if NAME has been defined in the proof context
(defun check-name (name)
  (let* ((name (if (stringp name) (intern name) name))
	 (pc-name (pc-parse name 'expr)))
    (resolve pc-name 'expr nil)))

;; Get a list of formula numbers from fnums
(defun extra-map-fnums (fnums)
  (map-fnums-arg fnums))

;; Get a formula number from fnum
(defun extra-map-fnum (fnum)
  (car (extra-map-fnums fnum)))

;; Get a PVS object from expr, where expr can be speficied as a formula or a string
;; or using Manip's location
(defun extra-get-expr (expr)
  (cond ((expr? expr) expr)
	((numberp expr) (extra-get-formula expr))
	((stringp expr)
	 (let ((fnum (extra-map-fnum expr)))
	   (if fnum (extra-get-formula fnum)
	     (pc-typecheck (pc-parse expr 'expr)))))
	((and (listp expr) (equal (car expr) '!))
	 (ee-pvs-obj (car (eval-ext-expr expr))))))

;; If l is not a list put it into a list
(defun enlist-it (l)
  (if (listp l) l (list l)))

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
  (cond ((or (number-expr? expr) (rational-expr? expr))
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

;; Make numerical expression
(defun mk-numerical (n &optional inv)
  (let ((fp (exact-fp n)))
    (cond ((and (stringp n) inv) (pc-typecheck (pc-parse (format nil "1/~a" n) 'expr)))
	  ((stringp n) (pc-typecheck (pc-parse (format nil "~a" n) 'expr)))
	  (t (let ((num (if inv (denominator fp) (numerator fp)))
		   (den (if inv (numerator fp) (denominator fp))))
	       (if (= den 1) (make-number-expr num)
		 (mk-division (make-number-expr num) 
			      (make-number-expr den))))))))

;; Returns true if type is a number type
(defun is-number-type (type)
  (or (and (type-name? type)
           (equal type *number*))
      (and (subtype? type)
	   (is-number-type (supertype type)))))

;; Returns rrue if expr is a number expression
(defun is-number-expr (expr)
  (is-number-type (type expr)))

;;;;;;;;;; Extrategies

;;; BIG MIRACLE

(defhelper __miracle__ ()
  (let ((thetrue (make-instance 
		  's-formula 
		  'formula (pc-typecheck (pc-parse "TRUE" 'expr))
		  'label nil 
		  'new? nil 
		  'asserted? nil))
	(thefalse (setf (s-forms *goal*) (cons thetrue (s-forms *goal*)))))
    (propax))
  "[Extrategies] Internal strategy. WARNING: DO NOT USE."
  "")

;;; Printing and commenting

(defstrat printf (msg &rest args)
  (let ((msg (format nil "~%~a" msg))
	(xxx (apply 'format (cons t (cons msg args)))))
    (skip))
  "[Extrategies] Prints the the Lisp formatted string MSG using the format arguments
ARGS, but behaves like skip. ARGS can only have constant values.")

(defstrat commentf (msg &rest args)
  (let ((msg (apply 'format (cons nil (cons msg args)))))
    (comment msg))
  "[Extrategies] Adds the formatted comment MSG to the sequent using the format
arguments ARGS. ARGS can only have constant values.")

;;; Labeling

(defstrat unlabel*__ (fnums label)
  (when label
    (let ((la  (car label))
	  (bel (cdr label)))
      (then (unlabel fnums la)
	    (unlabel*__ fnums bel))))
  "[Extrategies] Internal strategy.")

(defstep unlabel* (&optional (fnums *) label)
  (let ((fs (extra-map-fnums fnums)))
    (if (and label (listp label))
	(unlabel*__ fs label)
      (unlabel fs label)))
  "[Extrategies]  Removes specified label(s) LABEL (or all labels if none specified)
from the formulas FNUMS."
  "Removing ~1@*~:[all labels~;label(s) ~:*~a~] from ~@*~a")

(defstep delabel (label &optional hide?)
  (let ((fnums (extra-map-fnums label)))
       (when fnums
	     (unlabel* fnums label)
	     (when hide? (hide fnums))))
  "[Extrategies] Removes label(s) LABEL. Hides formulas if HIDE? is t."
  "Removing label(s) ~a")

(defstrat relabel1__ (lbs fs)
  (when lbs
    (let ((l  (car lbs))
	  (bs (cdr lbs)))
      (then (label l fs :push? t)
	    (relabel1__ bs fs))))
  "[Extrategies] Internal strategy.")

(defstrat relabel2__ (lbs fs)
  (when fs
    (let ((l  (car lbs))
	  (bs (or (cdr lbs) lbs))
	  (f  (car fs))
	  (s  (cdr fs)))
      (then (label l f :push? t)
	    (relabel2__ bs s))))
  "[Extrategies] Internal strategy.")

(defstep relabel (label fnums &optional (pairing? t))
  (let ((fs  (extra-map-fnums fnums))
	(lbs (enlist-it label)))
    (if pairing?
	(when lbs (relabel2__ lbs fs))
      (relabel1__ lbs fs)))
  "[Extrategies] Pushes label(s) LABEL into FNUMS. If PAIRING? is t and
LABEL is a list of labels, each label is paired to a formula in FNUMS"
  "Pushing label(s) ~a into formula(s) ~a")

(defstep name-label (name expr &optional hide?)
  (let ((label (format nil "~a:" name))
	(expr  (extra-get-expr expr)))
    (when expr
      (try-branch
       (name name expr)
       ((then
	 (label label -1)
	 (when hide? (hide label)))
	(skip))
       (skip))))
  "[Extrategies] Name an expression and label it"
  "Naming ~1@*~a with ~@*~a")

(defstep name-label* (nameexprs &optional hide?)
  (when nameexprs
    (let ((name     (caar nameexprs))
	  (expr     (cdar nameexprs)) 
	  (rest     (cdr  nameexprs)))
      (then 
       (name-label name expr hide?)
       (name-label* rest hide?))))
  "[Extrategies] Repeat name-label to a list of associations (name.expression)."
  "Repeating name-label to the list ~a")

(defstep discriminate (step &optional label)
  (if label
      (let ((lb (newlabel "DISCR")))
	(then (relabel lb *)
	      step
	      (relabel label (^ lb))
	      (delabel lb)))
    step)
  "[Extrategies] Apply STEP and label any new formula with LABEL."
  "Discriminating generated formulas~*~@[ with label ~a~]")

(defstrat mapstep@ (funstep &optional list)
  (when list
    (let ((step (funcall (eval funstep) (car list)))
	  (rest (cdr list)))
      (then@ step
	     (mapstep@ funstep rest))))
  "[Extrategies] Apply FUNSTEP to each element of LIST. The resulting steps are
sequentially applied to all branches.")

(defstrat mapstep (funstep &optional list)
  (when list
    (let ((step (funcall (eval funstep) (car list)))
	  (rest (cdr list)))
      (then step
	    (mapstep funstep rest))))
  "[Extrategies] Apply FUNSTEP to each element of LIST. The resulting steps are
sequentially applied to the main branch.")

(defstep copy* (fnums &optional label hide?)
  (let ((fs (extra-map-fnums fnums))
	(lb (or label (format nil "~a:" (newname "CP")))))
    (then
     (mapstep #'(lambda (x)`(then (discriminate (copy ,x) ,lb)
				  (hide ,lb))) fs)
     (when-not hide?
	       (reveal lb)
	       (when-not label
			 (delabel lb)))))
  "[Extrategies] Copy the formulas in FNUMS and label the new formulas with LABEL,
if LABEL is not nil. When HIDE? is t, hide the new formulas."
  "Copying formulas ~a")

(defstep protect (fnums step)
  (if fnums
      (let ((lbo (newlabel "PRTO"))
	    (lbc (newlabel "PRTC")))
	(then
	 (relabel lbo fnums)
	 (copy* lbo :label lbc)
	 (hide lbo)
	 step
	 (delete lbc)
	 (reveal lbo)
	 (delabel lbo)))
    step)
  "[Extrategies] Protect formulas in FNUMS when applying STEP."
  "Protecting formulas in ~a")

;;; Local tactics

(defstrat localtactic__ (name stratn step)
  (if (check-name stratn)
      step
    (printf "Local strategy ~a is not defined in this proof context" name))
  "[Extrategies] Internal strategy.")

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
  "[Extrategies] Defines local tactic (NAME ARGUMENTS) as STEP"
  "Defining local tactic ~a")

;;; Cases

(defstep caserel (expr1 expr2 &optional (rel "="))
  (let ((e1 (extra-get-expr expr1))
	(e2 (extra-get-expr expr2))
	(name1 (newname "CR"))
	(name2 (newname "CR"))
	(nes (list (cons name1 e1) (cons name2 e2)))
	(cases (format nil "~a ~a ~a" name1 rel name2)))
    (then (name-label* nes :hide? t)
	  (case cases)
	  (expand name1 :assert? none)
	  (expand name2 :assert? none)))
  "[Extrategies] Case a relation holds for two expressions"
  "Casing ~a ~2@*~a ~1@*~a")

;;; Induction

(defstrat inductionfree__ (recvar &optional first)
  (let ((name (freename "V"))
	(pre  (car (eval-ext-expr `(! * (-> ,recvar)))))
	(term (when pre (format nil "~a" (ee-pvs-obj pre)))))
    (if term
	(branch (name-replace name term :hide? t)
		((inductionfree__ name)
		 (skip)))
      (when-not
       first
       (typepred recvar))))
  "[Extrategies] Internal strategy.")

(defstrat inductionfree (&optional (recvar "v"))
  (if (forall-expr? (extra-get-formula 1))
      (let ((recvar (format nil "~a!1" recvar)))
	(then (skosimp* :preds? t)
	      (repeat (inductionfree__ recvar t))
	      (assert)))
    (then
     (repeat (inductionfree__ recvar t))
     (assert)))
  "[Extrategies] Extracts induction free principle from definition of recursive function. RECVAR is the
name of the quantified variable that encodes the recursive call.")

;;; TCCs

(defstep tccs-expr (expr &optional all? label)
  (let ((e (extra-get-expr expr)))
    (discriminate
     (then
      (typepred! e :implicit? t)
      (when all? (typepred e)))
     label))
  "[Extrategies] Behaves as (typepred! EXPR :implicit? t). If ALL? is t, then it
also does (typepred EXPR). Added hypotheses are labeled with LABEL, if provided."
  "Adding type constraints for ~a")

(defstep tccs-formulas (&optional (fnums *) all? label)
  (let ((fs (extra-map-fnums fnums)))
    (discriminate
     (then 
      (all-implicit-typepreds fs)
      (when all? (all-typepreds fs))) label))
  "[Extrategies] Behaves as (all-implicit-typepreds FNUMS). If ALL? is t, then it
also does (all-typepreds FNUMS). Added hypotheses are labeled with LABEL, if provided."
  "Adding type constraints for formulas ~a")

;;; Control flow
(defstrat when (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then steps)))
	step)
    (skip))
  "[Extrategies] Behaves as (if FLAG (then STEP1 ... STEPn) (skip)). Due to the execution model
of strategies in PVS, FLAG *must* be a simple variable.")

(defstrat when@ (flag &rest steps)
  (if (and flag steps)
      (let ((step (cons 'then@ steps)))
	step)
    (skip))
  "[Extrategies] Behaves as (if FLAG (then@ STEP1 ... STEPn) (skip)). Due to the execution model
of strategies in PVS, FLAG *must* be a simple variable.")

(defstrat when-not (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then steps)))
	   step)
      (skip))
  "[Extrategies] An abbreviation for (if (not FLAG) (then STEP1 ... STEPn) (skip)).Due to the
execution model of strategies in PVS, FLAG *must* be a simple variable.")

(defstrat when-not@ (flag &rest steps)
  (if (and (not flag) steps)
      (let ((step (cons 'then@ steps)))
	   step)
      (skip))
  "[Extrategies] An abbreviation for (if (not FLAG) (then@ STEP1 ... STEPn) (skip)). Due to
the execution model of strategies in PVS, FLAG *must* be a simple variable.")

(defstrat for__ (n step)
  (if (numberp n)
      (if (<= n 0)
	  (skip)
	(let ((m (- n 1)))
	  (then step
		(for__ m step))))
    (when-not n
     (repeat* step)))
  "[Extrategies] Internal strategy.")

(defstep for (n &rest steps)
  (when steps
    (let ((step (cons 'then steps)))
      (for__ n step)))
  "[Extrategies] Iterates N times STEP1 ... STEPn, or until it does nothing if N is nil,
along all the branches."
  "Iterating ~1@*~a ~@*~a times along all the branches")

(defstrat for@__ (n step)
  (if (numberp n)
      (if (<= n 0)
	  (skip)
	(let ((m (- n 1)))
	  (then@
	   step
	   (for@__ m step))))
    (when-not@ n
     (repeat step)))
  "[Extrategies] Internal strategy.")

(defstep for@ (n &rest steps)
  (when steps
    (let ((step (cons 'then@ steps)))
      (for@__ n step)))
  "[Extrategies] Iterates N times STEP1 ... STEPn, or until it does nothing if N is nil,
along the main branch."
  "Iterating ~1@*~a ~@*~a times along the main branch")

;; Skolem, lets, definitions

(defrule skoletin (&optional (fnum 1) name (nth 1) var (postfix "") hide?)
  (let
   ((f       (extra-map-fnum fnum))
    (expr    (extra-get-formula fnum))
    (flabels (get-labels fnum))
    (msg     (no-formula fnum)))
   (if expr
       (then (relabel "SKL:" fnum)
	     (let ((ret     (make-ret))
		   (nexpr   (sigmared expr name nth 
				      :newnames (enlist-it var)
				      :postfix postfix :ret ret))
		   (retexpr (ret-expr ret))
		   (lv      (mapcar #'(lambda(x)(format nil "~a:" x))
				    (each-other retexpr t)))
		   (flag    (not (numberp nexpr)))
		   (casestr (when flag (format nil "~a" nexpr))))
	       (if flag
		   (try-branch
		    (name-replace-label* retexpr
					 :fnums nil :dir rl :hide? t)
		    ((then (branch (case casestr)
				   ((then (relabel flabels -1
						       :pairing? nil)
					  (if (> f 0)
					      (__miracle__)
					    (beta -1 :let-reduce? nil)))
				    (then (relabel flabels 1 :pairing? nil)
					  (if (< f 0)
					      (__miracle__)
					    (beta 1 :let-reduce? nil)))
				    (__miracle__)))
			   (delabel "SKL:" :hide? t)
			   (when-not hide? (reveal lv) (delabel lv)))
		     (__miracle__))
		    (fail))
		 (fail)))
	     (delabel "SKL:"))
     (skip-msg msg)))
  "[Extrategies] Introduces a name definition <var>=expr, where where <var> is VAR concatenated with POSTFIX,
and reduces the NTH occurrence left-right, depth-first, of a sub-expression LET .... NAME = <expr>... in FNUM.
If VAR is nil, <var> is NAME concatenated with POSTFIX. The wild-card for any NAME is nil. If the NTH occurrence
of NAME is an expression (x1,..,NAME,..,xn), VAR must be list of variable names (v1 ... vm), where m < n.
In this case, name definitions v1=expr`1,...,vm=expr`m,..<xnp>=expr`n are introduced. If vi = \"_\",
<xip>=expr`i is introduced. Name definitions are hidden when HIDE? is t; they may be recalled at any time
with the command (reveal \"<xp>:\")."
  "Reducing let-in expression in formula ~a")

(defstep skoletin* (fnum &optional (postfix "") hide? n)
  (let ((expr (extra-get-formula fnum))
	(msg  (no-formula fnum)))
    (if expr 
	(then (relabel "SKLS:" fnum)
	      (for n (skoletin "SKLS:" :postfix postfix :hide? hide?))
	      (delabel "SKLS:"))
      (skip-msg msg)))
  "[Extrategies] Iterates N times skoletin (or until it does nothing if N is nil) in FNUM."
  "~%Iterating skoletin in ~a")

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
		       ((mergenames (when ret (merge-let-names names
							       newnames  
							       postfix)))
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
				     (if newnames (car newnames)
					 (format nil "~a~a"
					   name postfix)))) 
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

;;;;;; TO BE WORKED

;; Returns list of formula numbers not in fnums
(defun extra-map-but-fnums (fnums &key (all '*))
  (let ((forms (extra-map-fnums all))
	(but   (extra-map-fnums fnums)))
    (remove-if #'(lambda(x)(member x but)) forms)))
    
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

;; Sign of n (note that 0 returns 1).
(defun sign (n)
  (if (>= n 0) 1 -1))
  
;; String to int.
(defun str2int (str)
  (multiple-value-bind (n l) (parse-integer str :junk-allowed t)
    (when (and n (= (length str) l)) n)))

(defun str2decimal (str)
  (let ((a (pc-parse str 'expr)))
    (cond ((number-expr? a) (number a))
	  ((decimal? a) (/ (number (args1 a)) (number (args2 a)))))))

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

;; Field's standard message for an invalid formula number.
(defun no-formula (fnum)
  (format nil "No formula ~a found in the current sequent." fnum))

;; Field's standard message for an invalid arithmetic relational formula.
(defun no-relation (fnum)
  (let ((fnums (extra-map-fnums fnum)))
  (format nil "No arithmetic relational formula~p found in ~{~a~^, ~}." 
	  (length fnums) fnums)))

(defun check-no-relation (fnum)
  (let* ((f   (extra-get-formula fnum))
	 (rel (is-relation f)))
    (cond ((not f) (no-formula fnum))
	  ((not rel) (no-relation fnum)))))

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



;;;;;  EXTRA-TEGIES ;;;;; 

;; REWRITE-NAMES, REWRITE*, and SQ-SIMP are not official extra-tegies.
;; They are here because I don't know where else to put them.  They may
;; disappear from this package in the future, and re-appear in a better
;; location.

(defstep rewrite-names (names)
  (let ((step (else-rewrite names)))
       step)
  "[Field] Rewrites with a list of NAMES. A NAME can be name-lema OR (name-lema FNUMS)."
  "~%Rewriting with ~A"
)

(defstep rewrite* (names)
  (repeat (rewrite-names names))
  "[Field] Rewrites recursively REWRITE-NAMES. NAMES as in REWRITE-NAMES."
  "~%Rewriting recursively with ~A"
)

(defstep sq-simp ()  
  (rewrite* ("sq_neg" "sq_times" "sq_plus" "sq_minus" "sq_0"
  	     "sq_div" "sqrt_0" "sqrt_1" "sqrt_def" "sqrt_square"
             "sqrt_sq" "sq_sqrt" "sqrt_times" "sqrt_div"))
  "[Field] Simplifies sq and sqrt."
  "~%Simplifying sq and sqrt"
) 

(defstep cut (cases)
  (when cases
	(let ((nc (car cases))
	      (nl (cdr cases)))
	     (branch (case nc)
		     ((cut nl)
		      (skip)))))
  "[Field] Cuts one by one the formulas in CASES."
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
  "[Field] Asymmetrically splits a conjunction FNUM in the consequent (or a disjunction in the antecedent)."
  "~%Splashing formula ~A"
)

(defstep real-props (&optional (fnums '*) (theories "extra_tegies")
			       (let-reduce? t) (distrib? t))
  (let ((lb (newlabel "ND"))
	(exclude (when (not distrib?)
		   (list "times_div1" "times_div2" "div_times"
			 "add_div" "minus_div1" "minus_div2"
			 "div_distributes" "div_distributes_minus")))
	(th     (enlist-it theories))
	(lfnums (if distrib? fnums "RP:"))
	(step   (list 'then
		      (list 'auto-rewrite-theory "real_props" :exclude exclude)
		      (cons 'auto-rewrite-theories th)
		      (list 'assert lfnums :let-reduce? let-reduce?)
		      (cons 'stop-rewrite-theory (cons "real_props" th)))))
       (if distrib?
	   step
	   (then  (relabel "RP:" fnums)
		  (name-distrib fnums :label lb)
		  step
		  (replaces lb :dir rl :in "RP:" :but lb :hide? nil)
		  (delabel lb :hide? t)
		  (delabel "RP:"))))
  "[Field] Autorewrites with \"real_props\" and THEORIES in FNUMS. If LET-REDUCE? is nil, let-in expressions will not be reduced. If DISTRIB? is nil, distribution laws will be blocked."
  "~%Applying REAL-PROPS")

(defstep mult-by__ (flabel expr &optional (sign +))
  (let ((fnum    (extra-get-fnum flabel))
	(flabels (get-labels fnum))
        (f       (when fnum (sign fnum))))
       (when fnum
	     (branch (mult-by fnum expr sign)
		     ((let ((flag    (not (extra-map-fnums flabel))))
			   (when flag (relabel* flabels f :pairing? nil)))
		      (assert)))))
  "[Field] Multiplies both sides of formula FLABEL by EXPR. In contrast to MULT-BY, this strategy preserves the label."
  "~%Multiplying in ~A by ~A"
)

(defhelper replaces__ (flabels inlabels hide? dir)
  (when flabels
	(let ((fnum   (when flabels (car flabels)))
	      (frest  (when flabels (cdr flabels))))
	     (then  (replace fnum inlabels :hide? hide? :dir dir)
		    (replaces__ frest inlabels hide? dir))))
  "[Field] Internal strategy."
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
  "[Field] Iterates the PVS command REPLACE to rewrite with the formulas in FNUMS, respecting the order, the formulas in IN but not in BUT. The keys DIR and HIDE? are like in REPLACE. Notice that in contrast to REPLACE, the default value of HIDE? is T. Instead of using FNUMS, rewriting formulas can be addressed via FROM and TO. The key STEP specifies the command to be executed after all the replaces have taken place."
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
		      (let ((flag (string= dir 'rl)))
			   (when flag (swap-rel -1)))
		      (when hide? (hide -1))
		      (name-replace*__ rest flabels dir hide?))
		(assert))
	       (let ((msg (format nil "Error: ~A is already declared." name)))
		    (then (skip-msg msg :force-printing? t)
			  (fail))))))
  "[Field] Internal strategy."
)

(defstep name-replace* (names-and-exprs &optional (fnums '*) (dir lr) hide?)
  (let ((forms   (extra-map-fnums fnums))
	(flabels (listnames "NR" forms ":")))
       (when forms
	     (relabel* flabels forms)
	     (try (name-replace*__ names-and-exprs flabels dir hide?)
		  (delabel flabels)
		  (fail))))
  "[Field] Iterates NAME-REPLACE with names-and-exprs in FNUMS. DIR indicates the direction of the name definition. Use HIDE? to hide the name definition."
  "~%Iterating NAME-REPLACE with ~A"
)

(defstrat name-replace-label__ (name expr label flabels dir hide?)
  (let
   ((nlabel (or label (format nil "~A:" name))))
   (try-branch (name name expr)
	       ((then (replace -1 flabels)
		      (let ((flag (string= dir 'rl)))
			   (when flag (swap-rel -1)))
		      (relabel nlabel -1)
		      (when hide? (hide -1)))
		(assert))
	       (let
		((msg (format nil "Error: ~A is already declared." name)))
		(then (skip-msg msg :force-printing? t)
		      (fail)))))
  "[Field] Internal strategy."
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
  "[Field] Uses NAME, REPLACE, and LABEL to abbreviate an expression in FNUMS with a newly chosen name and label. If LABEL is nil, NAME: is used as label. DIR indicates the direction of the name definition. Use HIDE? to hide the name definition."
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
  "[Field] Internal strategy."
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
  "[Field] Iterates NAME-REPLACE-LABEL with names-and-exprs."
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
  "[Field] Introduces new names, which are based on PREFIX, to block the automatic
application of distributive laws in formulas FNUMS. Labels with LABEL the formulas
where new names are defined. These formulas are hidden if HIDE? is t.")

(defstep grind-reals (&optional defs (theories "extra_tegies") rewrites
				exclude (if-match t) (updates? t)
				polarity? (instantiator inst?)
				(let-reduce? t)
				nodistrib protect)
  (let ((lb    (newlabel "ND"))
	(th    (cons "real_props" (enlist-it theories)))
	(pro   (cons lb (enlist-it protect)))
	(step `(grind :defs ,defs :theories ,th :rewrites
		      ,rewrites :exclude ,exclude :if-match
		      ,if-match :updates? ,updates? :polarity?
		      ,polarity? :instantiator
		      ,instantiator :let-reduce? let-reduce?)))
       (then
	(when nodistrib
	  (name-distrib nodistrib :label lb))
	(try (assert) (fail) (skip))
	(protect pro step)
	(when nodistrib
	  (replaces lb :dir rl :but lb :hide? nil)
	  (delabel lb :hide? t))))
  "[Field] Apply GRIND with \"real_props\". This strategy supports the same
options as GRIND. Additionally, GRIND-REALS blocks distribution laws in main level
expressions in the list of formulas NODISTRIB and protects formulas in PROTECT."
  "~%Applying GRIND-REALS")

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
  "[Field] Adds relational formulas FNUM1 and FNUM2. If FNUM2 is nil, adds FNUM with itself. Hides the formulas when HIDE? is t. Labels the result with LABEL (if specified)."
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
  "[Field] Negates both sides of the relational formula FNUM."
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
  "[Field] Subtracts relational formulas FNUM1 and FNUM2. Hides the formulas when HIDE? is t. Labels the result with LABEL (if specified)."
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
  "[Field] Wraps both sides of the relational formula in FNUM with STR(...). If rev? is nil, the original relation is preserved, otherwise the relation is reversed."
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
   "[Field] SKEEP works like SKOSIMP but keeps the names of the bound variables. In case of name clashes, use SKOSIMP instead."
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
  "[Field]  Introduces a name definition <xp>=expr, where where <xp> is VAR concatenated with POSTFIX; and then, instantiates NAME in FNUM with <xp>. If VAR is nil, <xp> is NAME concatenated with POSTFIX. The wild-card for any NAME is nil. FNUM must have the form (FORALL(..,NAME,..) : NAME=expr AND ... IMPLIES ...), if it is in the antecedent, or (EXISTS (..,NAME,..):NAME=expr AND ...), if it is in the consequent.  The name definition is hidden when HIDE? is nil; it may be recalled at any time using (REVEAL \"<xp>:\")."
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
  "[Field] Iterates N times SKODEF (or until it does nothing if N < 0) in FNUM."
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
  "[Field] Reduces the NTH occurrence left-right, depth-first, of a sub-expression LET NAME = ... in FNUM. If NAME is nil, reduces the NTH occurrence of a let-in expression in FNUM."
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
  "[Field] Iterates N times REDLET (or until it does nothing if N < 0)."
  "~%Reducing let-in expressions in ~A"
)

(defstrat extrategies-about ()
  (let ((version *extra-version*)) 
    (printf "%--
% ~a
% http://shemesh.larc.nasa.gov/people/cam/Field
%--~%" version))
  "[Extrategies] Prints Extrategies's about information.")

