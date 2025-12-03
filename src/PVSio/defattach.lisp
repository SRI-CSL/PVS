;;
;; defattach.lisp
;; Release: PVSio-8.0 (10/13/2023)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/PVSio
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; Functions and macros for defining PVSio semantic attachments
;;

(in-package :pvs)

(defun pvsio-version ()
  (pvs-message *pvsio-version*))

(defvar *pvsio-attachments* (make-hash-table :test #'equal)
  "Hash table of PVSio attachments indexed by theory.decl-id.")

(defstruct attachment
  theory     ;; PVS theory name
  name       ;; PVS definition name. Due to PVS overloading, there may be more than one
             ;; attachement with the same name for the same theory
  formals    ;; Formal arguments of the PVSio definition
  proto      ;; Prototype of the PVSio definition
  decl-id    ;; Unique id of the PVS declaration
  decl-proto ;; Prototype of the PVS declaration (proto should match decl-proto)
  primitive  ;; NIL unless this attachment a primitive, i.e., trusted by the PVS theorem prover
  fsymbol)   ;; Symbol of the PVSio function defining this attachment

(defvar *pvsio-type-hash* (make-hash-table)
  "Hash table of dynamic types of PVSio functions, indexed by a unically
generated symbol. The index is passed to PVSio functions using the key
parameter :the-pvs-type-key_.")

(defun attach-lt (att1 att2)
  "Lexicographic (THEORY,NAME) less-than order for attachments."
  (or (string< (attachment-theory att1) (attachment-theory att2))
      (and (string= (attachment-theory att1) (attachment-theory att2))
	   (string< (attachment-name att1) (attachment-name att2)))))

(defun list-of-attachments (&key theory name)
  "Lists attachments in a given THEORY with a given NAME, sorted by THEORY and NAME.
The value NIL represents any NAME or any THEORY."
  (loop for attach being the hash-value of *pvsio-attachments*
	with attachments = nil
	when (and (or (null name) (string= name (attachment-name attach)))
		  (or (null theory) (string= theory (attachment-theory attach))))
	do (setq attachments (insert-into-sorted-list attach attachments #'attach-lt))
	finally (return attachments)))

(defun pvsio-attachment (expr)
  "If EXPR is a name-expr?, returns its PVSio attachment (if any)."
  (when (and (name-expr? expr) (= (length (resolutions expr)) 1))
    (let* ((decl (declaration (car (resolutions expr))))
	   (key  (pvsio-attachment-key decl)))
      (gethash key *pvsio-attachments*))))

(defun remove-attachment-theory (theory)
  "Removes attachments of THEORY."
  (maphash #'(lambda (key attach)
               (when (string= (attachment-theory attach) theory)
                 (remhash key *pvsio-attachments*)))
           *pvsio-attachments*))

(defun attachment-names ()
  "Returns a sorted list of attachment names."
  (loop for attach being the hash-value of *pvsio-attachments*
	with names = nil
	do (setq names (insert-into-sorted-list (attachment-name attach) names #'string<
						:duplicates nil))
	finally (return names)))

(defun attachment-theories ()
  "Returns the sorted list of attachment theories."
  (loop for attach being the hash-value of *pvsio-attachments*
	with theories = nil
	do (setq theories (insert-into-sorted-list (attachment-theory attach) theories #'string<
						   :duplicates nil))
	finally (return theories)))

(defun defattach-long-name (attach)
  "Returns the the long name of an attachment including name,
pretty-printed prototype, and whether is primitive or not."
  (format nil "~a::~a~:[~; [primitive]~]"
	  (attachment-name attach)
	  (pp-prototype (attachment-decl-proto attach))
	  (attachment-primitive attach)))

(defun list-pvs-attachments-str ()
  (let ((l (loop for theory in (attachment-theories)
		 for atts = (mapcar #'defattach-long-name (list-of-attachments :theory theory))
		 collect (format nil "Theory ~a: ~{~a~^, ~}.~2%" theory atts))))
    (if l (format nil "~{~a~}" l)
      (format nil "No semantics attachments loaded in current context.~2%"))))

(defun list-pvs-attachments ()
  (format t "~a" (list-pvs-attachments-str)))

(defun help-attachment-doc (attach)
  (let ((fsymbol (attachment-fsymbol attach)))
    (documentation fsymbol 'function)))

(defun help-theory-attachments-str (theory)
  (let ((l (mapcar #'help-attachment-doc (list-of-attachments :theory theory))))
    (if l (format nil "~{~a~2%~}" l)
      (format nil "No semantic attachments in theory ~a.~2%" theory))))

(defun help-pvs-theory-attachments (theory)
  (format t "~a" (help-theory-attachments-str theory)))

(defun help_pvs_theory_attachments (theory)
  (help-pvs-theory-attachments theory))

(defun help-attachment-str (name)
  (let ((l (mapcar #'help-attachment-doc (list-of-attachments :name name))))
    (if l (format nil "~{~a~2%~}" l)
      (format nil "No semantic attachments named ~a.~2%" name))))

(defun help-pvs-attachment (name)
  (format t "~a" (help-attachment-str name)))

(defun help_pvs_attachment (name) 
  (help-pvs-attachment name))

(defun split_thnm (thnm)
  "If THNM is a string of the form TH.NM, returns a tuple of strings TH and NM.
Otherwise, returns a tuple of NIL and the string THNM"
  (let ((pos (position #\. thnm)))
    (if pos (cons (subseq thnm 0 pos)
		  (subseq thnm (+ pos 1)))
      (cons nil thnm))))

(defun find-atomic-prototype (type &optional (rec t))
  "An atomic prototype is either a string representing the type-name or print-type
of the type at the declared level, or NIL in case of a more complex type.
For example, the atomic prototypes of the following constants are provided
in the comments below.
a: nat             % nat
b: T               % T
c: boolean         % boolean
e: {n:nat | n > 3} % nat
f: [nat,bool]      % NIL
g: (odd?)          % NIL
h: [nat->bool]     % NIL"
  (cond ((print-type type)
	 (find-atomic-prototype (print-type type) nil))
	((type-name? type)
	 (symbol-name (id type)))
	((and rec (subtype? type) (supertype type))
	 (find-atomic-prototype  (supertype type) nil))))

(defun find-prototype (declaration)
  "Finds the prototype of a declaration. The prototype is a representation
of the shape of the type of the declaration that only uses atomic protypes
-- see definition above. This shape is used to statically disambiguate semantic
attachments with the same name, but differen types. A prototype  is a
list (T0 T1 .. TN) representing a type of the form [[T1,..,TN]->T0], where N=0 in
case of a constant. Each element Ti is an atomic prototype."
  (let ((decltype (type declaration)))
    (if (funtype? decltype)
	(cons (find-atomic-prototype (range decltype))
	      (mapcar (lambda (formal) (find-atomic-prototype (type formal)))
		      (car (formals declaration))))
      (list (find-atomic-prototype decltype)))))

(defun get-attach-formals (declformals &optional return-type prototype formals)
  "DELFORMALS is a list of the form (V0 .. VN), where each Vi is either a
variable name (symbol) or a list of the form (Vi Ti), where Ti is a
type name (string). The symbol Vi may be :return representing the return
variable. Return a list where the car is the prototype of the attachment
declaration and  the cdr are the formals of the declaration."
  (if declformals
      (let ((arg (car declformals)))
	(cond ((eq arg :return)
	       (get-attach-formals (cdr declformals)
				   return-type
				   prototype
				   formals))
	      ((and (consp arg) (eq (car arg) :return))
	       (get-attach-formals (cdr declformals)
				   (cadr arg)
				   prototype
				   formals))
	      ((consp arg)
	       (get-attach-formals (cdr declformals)
				   return-type
				   (append prototype (list (cadr arg)))
				   (append formals (list (car arg)))))
	      (t
	       (get-attach-formals (cdr declformals)
				   return-type
				   (append prototype (list nil))
				   (append formals (list arg))))))
    (cons (cons return-type prototype) formals)))

(defun prototype-match (proto1 proto2)
  "PROTO1 = (T0 T1 .. Tn) matches PROTO2 = (S0 S1 .. Sn) if their
length are the same and for all i, Ti = Si OR Ti = NIL."
  (and (= (length proto1) (length proto2))
       (loop for ti in proto1
	     for si in proto2
	     always (or (null ti) (string= ti si)))))

(defun find-matching-prototypes (proto declarations &optional best-declaration matched-protos)
  "Returns a list where the car is the best declaration, possibly NIL if none or more than one,
and the cdr is the list of matched prototypes."
  (if declarations
    (let* ((declaration (car declarations))
	   (proto2      (find-prototype declaration)))
      (if (prototype-match proto proto2)
	  (find-matching-prototypes proto (cdr declarations)
				    (unless matched-protos declaration)
				    (append matched-protos (list proto2)))
	(find-matching-prototypes proto (cdr declarations)
				  best-declaration
				  matched-protos)))
    (cons best-declaration matched-protos)))

(defun pp-prototype (prototype)
  "Pretty-prints prototype"
  (flet ((pp-proto (arg) (or arg "_")))
    (if (cdr prototype)
	(let ((sqb (cddr prototype))) ;; Is domain a cartesian product?
	  (format nil "[~:[~;[~]~{~a~^,~}~:[~;]~] -> ~a]"
		  sqb
		  (mapcar #'pp-proto (cdr prototype))
		  sqb
		  (pp-proto (car prototype))))
      (format nil "~a" (pp-proto (car prototype))))))

(defun defattach-theory-name (theory name declformals body &key primitive)
  (let* ((quiet  theory)
	 (th_nm  (split_thnm name))
	 (theory (if (car th_nm) (get-theory (car th_nm)) theory))
	 (name   (cdr th_nm)))
    (cond ((and (null theory) (null (car th_nm)))
	   (pvs-message "Error (pvs-attachments): Theory qualification of attachment ~a is missing."
			name))
	  ((null theory)
	   (pvs-message "Error (pvs-attachments): Theory ~a not found in current context." (car th_nm)))
	  (t
	   (let* ((thnm           (format nil "~a.~a" (id theory) name))
		  (declarations   (const-decls-of-theory theory name))
		  (proto-formals  (when declarations (get-attach-formals declformals)))
		  (proto          (car proto-formals))
		  (best-matched   (find-matching-prototypes proto declarations))
		  (best-decl      (car best-matched))
		  (matched-protos (cdr best-matched))
		  (best-proto     (car matched-protos)))
	     (cond ((null declarations)
		    (pvs-message "Error (pvs-attachments): Declaration ~a not found."
				 thnm))
		   ((null matched-protos)
		    (pvs-message "Error (pvs-attachments): Declaration ~a::~a not found."
				 thnm (pp-prototype proto)))
		   ((null best-decl)
		    (pvs-message "Error (pvs-attachments): Declaration ~a::~a could be resolved to:~%~
                        ~{~a::~a~%~}~
                        To resolve ambiguity, annotate attchment variables with type names."
				 thnm (pp-prototype proto)
				 (mapcan (lambda (p) (list thnm (pp-prototype p))) matched-protos)))
		   (t (unless quiet
			(pvs-message "Loading semantic ~:[attachment~;primitive~] of ~a::~a~%"
				     primitive thnm (pp-prototype best-proto)))
		      (defattach-pvsio best-decl name declformals body :primitive primitive))))))))

(defun pvsio-attachment-key (decl)
  (when (const-decl? decl)
    (format nil "~a.~a"
	    (id (module decl))
	    (get-unique-id decl))))

(defun defattach-pvsio (decl name declformals body &key primitive)
  (let* ((theory        (id (module decl)))
	 (proto-formals (get-attach-formals declformals))
	 (proto         (car proto-formals))
	 (formals       (cdr proto-formals))
	 (uid           (get-unique-id decl))
	 (key 		(pvsio-attachment-key decl))
	 (decl-proto    (find-prototype decl))
	 (fpvsio        (makesym "~a-~a.~a" "pvsio" theory uid))
	 (attach        (make-attachment :theory theory
					 :name name
					 :formals formals
					 :proto proto
					 :decl-id uid
					 :decl-proto decl-proto
					 :primitive primitive
					 :fsymbol fpvsio)))
    (setf (gethash key *pvsio-attachments*) attach)
    (defattach-body attach body)))

(defmacro pvsio-not-in-prover (name)
  `(when (and *in-checker* (not *in-evaluator*))
     (error 'pvsio-inprover :format-control ,(format nil "~
~a is defined as a semantic attachment.
It cannot be evaluated in a formal proof." name))))

;; decls is a list of declarations for defun, e.g., (declare (ignore x))
(defun defattach-body (attachment body &optional decls)
  (if (and body (listp (car body)) (equal (caar body) 'declare)) ;; Car of body is a declaration
      (defattach-body attachment (cdr body) (append decls (list (car body))))
    (let* ((theory
	    (attachment-theory attachment))
	   (name
	    (attachment-name attachment))
	   (formals
	    (attachment-formals attachment))
	   (proto
	    (attachment-proto attachment))
	   (fsymbol
	    (attachment-fsymbol attachment))
	   (formalstr
	    (format nil "~@[(~{~a~@[::~a~]~^,~})~]~@[::~a~]"
		    (merge-lists formals (cdr proto)) (car proto)))
	   (dobo
	    (if (and body (cdr body) (stringp (car body)))
		body
	      (cons nil body)))
	   (letbody
	    `((let ((the-pvs-type_ (gethash the-pvs-type-key_ *pvsio-type-hash*)))
		(declare (ignorable the-pvs-type_))
		,@(cdr dobo))))
	   (thnmpro
	    (format nil "~a.~a" theory (defattach-long-name attachment)))
	   (newdoc
	    (format nil "~
PVSio Declaration: ~a.~a~a
PVS Resolution: ~a
Lisp function symbol: ~a~
~@[~%~a~]"
		    theory name formalstr
		    thnmpro
		    fsymbol
		    (car dobo)))
	   (newformals
	    (append formals (list '&key 'the-pvs-type-key_ 'the-theory-actuals_)))
	   (newbody
	    (if (attachment-primitive attachment)
		letbody
	      (cons `(pvsio-not-in-prover ,thnmpro) letbody))))
      (append `(defun ,fsymbol ,newformals)
	      (cons '(declare (ignorable the-theory-actuals_)) decls)
	      (cons newdoc newbody)))))

(defmacro defattach (thnm formals &rest body)
  (defattach-theory-name nil (symbol-name thnm) formals body))

(defmacro defprimitive (thnm formals &rest body)
  (defattach-theory-name nil (symbol-name thnm) formals body :primitive t))

(defun defattach-theory (theory attachments)
  (mapcar #'(lambda (defattach)
	      (if (and (consp defattach) (memq (car defattach) '(defattach defprimitive)))
		  (let ((primitive (eq (car defattach) 'defprimitive))
			(name      (symbol-name (cadr defattach)))
			(formals   (caddr defattach))
			(body      (cdddr defattach)))
		    (defattach-theory-name theory name formals body :primitive primitive))
		defattach))
	  attachments))

(defmacro attachments (theory-id &rest attachments)
  (let ((theory (get-theory theory-id)))
    (if theory
	(progn
	  (remove-attachment-theory theory-id)
	  (pvs-message "Loading semantic attachments of theory ~a." theory-id)
	  `(progn
	     ,@(defattach-theory theory attachments)))
      (pvs-message "Error (pvs-attachments): Theory ~a not found in current context." theory-id))))

;; Reports running-time errors in attachments
(defmacro attach-error (fmtstr &rest args)
  "Reports an error in the execution of a semantic attachment. FMTSTR
is a format string."
  `(error 'pvsio-error :message (format nil ,fmtstr ,@args)))

;; Global variables are represented by a list (name val1 val2 ... valn). Name is
;; non-empty for mutable variables of type Global.

(defun pvsio_new_gvar (&optional name)
  "Creates a new mutable variable with an undefined value"
  (list name))

(defun pvsio_ref_gvar (value &optional name)
  "Creates a new mutable variable and sets it to given value"
  (list name value))

(defun pvsio_def_gvar (gvar value)
  "Sets mutable variable gvar to given value"
  (setf (cdr gvar) (list value)))

(defun pvsio_undef_gvar (gvar)
  "Returns TRUE if mutable variable is undefined"
  (unless (cdr gvar) t))

(defun pvsio_val_gvar (gvar)
  "Returns value of mutable variable. Issue an error if GVAR is undefined"
  (cond ((cdr gvar)
	 (cadr gvar))
	((car gvar)
	 (attach-error "Global variable is undefined (~a)" (car gvar)))
	(t
	 (attach-error "Mutable variable is undefined"))))

(defun pvsio_reset_gvar (gvar)
  "Sets mutable variable to undefined"
  (setf (cdr gvar) nil))

(defun pvsio_push_gvar (gvar value)
  "Pushes value to the top of the mutable variable and skips"
  (and (push value (cdr gvar)) t))

(defun pvsio_pop_gvar (gvar)
  "Pops value of the mutable variable and fails silently when mutable variable is undefined"
  (and (pop (cdr gvar)) t))

(defun pvsio-eval-lisp (expr)
  "Returns 2 values. The main value is the Lisp representation of EXPR.
The second value is it's PVS type."
  (let* ((expr (extra-get-expr expr))
	 (type (type expr)))
    (values (eval (pvs2cl expr))
	    type)))

(defun pvsio_get_gvar_by_name (name)
  "Returns the current value of a PVSio Global variable. It assumes the variable is defined."
  (let ((gvar (pvsio-eval-lisp name)))
    (pvsio_val_gvar gvar)))

(defun pvsio-funcall (funstr &rest args)
  "Apply attachment function of FUNSTR (string), which maybe fully qualified,
to the list of arguments, e.g., (pvsio-funcall f0 arg1 arg2 ... argn).
Closure of attachment functions (including those of PVSio attachment) have
exactly 1 tuple argument."
  (multiple-value-bind (fun type)
      (pvsio-eval-lisp funstr)
    (let ((arity (if (funtype? type) (arity type) 0))
	  (nargs (length args)))
      (cond ((and (functionp fun) (> arity 0) (> nargs 0))
	     (cond ((or (and (= arity 1) (= nargs 1))
			(and (= nargs 1) (vectorp (car args))
			     (= arity (length (car args)))))
		    (funcall fun (car args)))
		   ((= arity nargs)
		    (funcall fun (eval `(pvs2cl_tuple ,@args))))
		   (t (attach-error "~a expects ~a arguments, ~a were provided."
				    funstr arity nargs))))
	    ((null args) fun)
	    (t (attach-error "~a expects no arguments." funstr))))))
