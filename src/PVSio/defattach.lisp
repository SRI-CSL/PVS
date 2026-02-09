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

(defvar *pvsio-attachments* (make-hash-table :test #'equal)
  "Hash table of PVSio attachments indexed by pvsio-attachment-key.")

(defstruct attachment
  theo-qid    ;; PVS theory's qualified identifier (string of the form "[<lib>@]<id>")
  name        ;; PVS definition name. Due to PVS overloading, there may be more than one
              ;; attachement with the same name for the same theory
  formals     ;; Formal arguments of the PVSio definition
  xformals    ;; Extra-formals (theory parameters and decl-type)
  proto       ;; Prototype of the PVSio definition
  tc-time     ;; UTC type-check time (used in the the computation of pvsio-attachment-key)
  declaration ;; PVS declaration whose prototype best matches proto
  primitive   ;; T when this attachment a primitive, i.e., trusted by the PVS theorem prover
  source      ;; Path to pvs-attachment where this attachment is defined
  fsymbol)    ;; Symbol of the PVSio function defining this attachment

(defvar *pvsio-type-hash* (make-hash-table)
  "Hash table of resolved types used by PVSio functions, indexed by a unically
generated symbol.")

(defun attach-lt (att1 att2)
  "Lexicographic (THEORY,NAME) less-than order for attachments."
  (or (string< (attachment-theo-qid att1) (attachment-theo-qid att2))
      (and (string= (attachment-theo-qid att1) (attachment-theo-qid att2))
	   (string< (attachment-name att1) (attachment-name att2)))))

(defun list-of-attachments (&key theory name)
  "List attachments in a given THEORY (theory's qualified id) with a given NAME,
sorted by THEORY and NAME. The value NIL represents any NAME or any THEORY."
  (loop for attach being the hash-value of *pvsio-attachments*
	with attachments = nil
	when (and (or (null name) (string= name (attachment-name attach)))
		  (or (null theory) (string= theory (attachment-theo-qid attach))))
	do (setq attachments (insert-into-sorted-list attach attachments #'attach-lt))
	finally (return attachments)))

(defun pvsio-attachment (expr)
  "If EXPR is a name-expr?, return its PVSio attachment (if any)."
  (when (and (name-expr? expr) (= (length (resolutions expr)) 1))
    (let* ((decl (declaration (car (resolutions expr))))
	   (key  (pvsio-attachment-key decl)))
      (gethash key *pvsio-attachments*))))

(defun remove-attachment-theory (theory)
  "Remove attachments of THEORY (theory's qualified id)."
  (maphash #'(lambda (key attach)
               (when (string= (attachment-theo-qid attach) theory)
                 (remhash key *pvsio-attachments*)))
           *pvsio-attachments*))

(defun attachment-names ()
  "Return a sorted list of attachment names."
  (loop for attach being the hash-value of *pvsio-attachments*
	with names = nil
	do (setq names (insert-into-sorted-list (attachment-name attach) names #'string<
						:duplicates nil))
	finally (return names)))

(defun attachment-theories ()
  "Return the sorted list of attachment theories."
  (loop for attach being the hash-value of *pvsio-attachments*
	with theories = nil
	do (setq theories (insert-into-sorted-list
			   (attachment-theo-qid attach) theories #'string<
			   :duplicates nil))
	finally (return theories)))

(defun defattach-long-name (attach)
  "Return the the long name of an attachment including name,
pretty-printed prototype, and whether is primitive or not."
  (format nil "~a::~a~:[~; [primitive]~]"
	  (attachment-name attach)
	  (pp-prototype (get-prototype-decl (attachment-declaration attach)))
	  (attachment-primitive attach)))

(defun list-pvs-attachments-str ()
  (let ((l (loop for theory in (attachment-theories)
		 for atts = (mapcar #'defattach-long-name (list-of-attachments :theory theory))
		 collect (format nil "Theory ~a:~{~%  ~a~}~%" theory atts))))
    (if l (format nil "~{~a~}" l)
      (format nil "No semantics attachments loaded in current context.~%"))))

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

(defun get-atomic-prototype (type &optional (rec t))
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
	 (get-atomic-prototype (print-type type) nil))
	((type-name? type)
	 (symbol-name (id type)))
	((and rec (subtype? type) (supertype type))
	 (get-atomic-prototype  (supertype type) nil))))

(defun get-prototype-decl (declaration)
  "Finds the prototype of a declaration. The prototype is a representation
of the shape of the type of the declaration that only uses atomic protypes
-- see definition above. This shape is used to statically disambiguate semantic
attachments with the same name, but differen types. A prototype  is a
list (T0 T1 .. TN) representing a type of the form [[T1,..,TN]->T0], where N=0 in
case of a constant. Each element Ti is an atomic prototype."
  (let ((decltype (type declaration)))
    (if (funtype? decltype)
	(cons (get-atomic-prototype (range decltype))
	      (mapcar (lambda (formal) (get-atomic-prototype (type formal)))
		      (car (formals declaration))))
      (list (get-atomic-prototype decltype)))))

(defstruct extra-formal
  varid ;; Variable identifier (symbol)
  kind  ;; :type (formal is a type), :const (formal is a const)
  param ;; Parameter name
  (where nil) ;; :decl (formal is a declaration), :theo (formal is a theory parameter)
  (pos nil)   ;; Position of parameter in either theory parameters or declaration formals
  )

(defun parse-attach-formals (declformals &optional return-type prototype formals xformals)
  "DECLFORMALS is a list of the form (V0 .. VN), where each Vi is either a
parameter name (symbol) or one of the special list forms:
* (V T),  where T is a PVS type name (symbol), indicating the parameter
  V has PVS type T
* (:return T), where T is a PVS type name, indicating that returned value
  has type T
* (V :decl-formal P), where V is a variable name (symbol) and P is a theory parameter,
  indicating the value of P should be bound to variable V. If P is ommitted,
  it is assumed that the name of the parameter is also V.
* (V :decl-type), where V is a varible name (symbol), indicating that the
  type of the PVS declaration should be bound to variable V
Return muliple values in the following order.
* declared prototype, i.e., shape of the declaration type
* list of formals (variable names)
* list of extra-formal"
  (if declformals
      (let ((arg (car declformals)))
	(cond ((and (consp arg) (eq (car arg) :return))
	       (parse-attach-formals (cdr declformals)
				     (cadr arg)
				     prototype
				     formals
				     xformals))
	      ((and (consp arg) (member (cadr arg) '(:decl-formal :decl-type)))
	       (let* ((varid   (nth 0 arg))
		      (kind    (when (eq (cadr arg) :decl-type)
				 :type))
		      (param   (when (eq (cadr arg) :decl-formal)
				 (or (nth 2 arg) varid)))
		      (xformal (make-extra-formal
				:varid varid
				:kind  kind
				:param param)))
		 (parse-attach-formals (cdr declformals)
				       return-type
				       prototype
				       formals
				       (append
					xformals
					(list xformal)))))
	      ((consp arg)
	       (parse-attach-formals (cdr declformals)
				     return-type
				     (append prototype (list (cadr arg)))
				     (append formals (list (car arg)))
				     xformals))
	      (t
	       (parse-attach-formals (cdr declformals)
				     return-type
				     (append prototype (list nil))
				     (append formals (list arg))
				     xformals))))
    (values (cons return-type prototype) formals xformals)))

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
	   (proto2      (get-prototype-decl declaration)))
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
  (when prototype
    (flet ((pp-proto (arg) (or arg "_")))
      (if (cdr prototype)
	  (let ((sqb (cddr prototype))) ;; Is domain a cartesian product?
	    (format nil "[~:[~;[~]~{~a~^,~}~:[~;]~] -> ~a]"
		    sqb
		    (mapcar #'pp-proto (cdr prototype))
		    sqb
		    (pp-proto (car prototype))))
      (format nil "~a" (pp-proto (car prototype)))))))

(defun defattach-thnm (thnm declformals body &key primitive)
  (let* ((th_nm      (split_thnm thnm))
	 (name       (cdr th_nm))
	 (source     (car *pvs-attachment-source-file*))
         (theories   (cdr *pvs-attachment-source-file*))
	 (theory     (when (car th_nm) (extra-get-theory (car th_nm) :imported source)))
	 (loadedfile (when source (gethash source *pvsio-loaded-files*))))
    (cond ((null (car th_nm))
	   (pvs-message "Error (pvs-attachments): Theory qualification of attachment ~a is missing."
			name))
	  ((and theory (outdated-theory theory theories))
	   (add-updated-theory theory loadedfile)
	   (attach-theory-name theory name declformals body :primitive primitive :verbose t))
	  (theory
	   (or (add-updated-theory theory loadedfile)) t)
	  (loadedfile
	   (or (add-dangling-theory (car th_nm) loadedfile)) t)
	  (t
	   (pvs-message "Error (pvs-attachments): Theory ~a not found in current context."
			(car th_nm))))))

(defun check-xformals (xformals formals decl)
  "Return string message if xformals isn't disjoint with formals or xformal doesn't appear
as a formal in decl."
  (when xformals
    (let* ((xformal      (car xformals))
	   (varid        (extra-formal-varid xformal))
	   (param        (extra-formal-param xformal))
	   (theo-params  (formals (module decl)))
	   (decl-formals (decl-formals decl))
	   (ntheo        (length theo-params))
	   (ndecl        (length decl-formals))
	   (find-theo    (when param (member param theo-params :test #'string= :key #'id)))
	   (find-decl    (when param (member param decl-formals :test #'string= :key #'id))))
      (cond ((member varid formals)
	     (format nil "Formal parameter ~a appears more than once" varid))
	    ((or (eq varid 't) (member 't formals))
	     (format nil "Symbol ~a cannot be used as variable name" t))
	    ((and param (not find-theo) (not find-decl))
	     (format nil "~a is neither a theory parameter nor a declaration formal" param))
	    (t
	     (when find-theo
	       (setf (extra-formal-kind xformal) (if (formal-type-decl? (car find-theo)) :type :const))
	       (setf (extra-formal-where xformal) :theo)
	       (setf (extra-formal-pos xformal) (- ntheo (length find-theo))))
	     (when find-decl
	       (setf (extra-formal-kind xformal) (if (decl-formal-type? (car find-decl)) :type :const))
	       (setf (extra-formal-where xformal) :decl)
	       (setf (extra-formal-pos xformal) (- ndecl (length find-decl))))
	     (let ((rest (check-xformals (cdr xformals) formals decl)))
	       (if (listp rest)
		   (cons xformal rest)
		 rest)))))))

(defun proto-xformals (xformals atheo adecl)
  (when xformals
    (let* ((xformal (car xformals))
	   (varid   (extra-formal-varid xformal))
	   (param   (extra-formal-param xformal)))
      (when param
	(if (eq (extra-formal-where xformal) :theo)
	    (setf (aref atheo (extra-formal-pos xformal)) varid)
	  (setf (aref adecl (extra-formal-pos xformal)) varid)))
      (proto-xformals (cdr xformals) atheo adecl))))

(defun pp-xformals (attach)
  (let* ((decl     (attachment-declaration attach))
	 (atheo    (make-array (length (formals (module decl))) :initial-element '_))
	 (adecl    (make-array (length (decl-formals decl)) :initial-element '_))
	 (xformals (attachment-xformals attach)))
    (when xformals
      (proto-xformals xformals atheo adecl)
      (cons
       (when (and (> (length atheo) 0) (find-if-not (lambda (x) (eq x '_)) atheo))
	 (format nil "[~{~a~^,~}]" (coerce atheo 'list)))
       (when (and (> (length adecl) 0) (find-if-not (lambda (x) (eq x '_)) adecl))
	 (format nil "[~{~a~^,~}]" (coerce adecl 'list)))))))

(defun attach-theory-name (theory name declformals body &key primitive verbose)
  (let ((thnm         (format nil "~a.~a" (extra-qid-theory theory) name))
	(declarations (const-decls-of-theory theory name)))
    (if declarations
	(multiple-value-bind
	    (proto formals xformals)
	    (parse-attach-formals declformals)
	  (let* ((best-matched   (find-matching-prototypes proto declarations))
		 (best-decl      (car best-matched))
		 (matched-protos (cdr best-matched))
		 (best-proto     (pp-prototype (car matched-protos)))
		 (chk-xfmls      (when best-decl (check-xformals xformals formals best-decl))))
	    (cond ((null matched-protos)
		   (pvs-message "Error (pvs-attachments): Declaration ~a::~a not found."
				thnm (pp-prototype proto)))
		  ((null best-decl)
		   (pvs-message "Error (pvs-attachments): Declaration ~a::~a could be resolved to:~%~
                        ~{~a::~a~%~}~
                        To resolve ambiguity, annotate attachment paramters with type names."
				thnm (pp-prototype proto)
				(mapcan (lambda (p) (list thnm (pp-prototype p))) matched-protos)))
		  ((stringp chk-xfmls)
		   (pvs-message "Error (pvs-attachments): ~a in attachment ~a::~a" chk-xfmls thnm best-proto))
		  (t (when verbose
		       (pvs-message "Loading semantic ~:[attachment~;primitive~] of ~a::~a"
				    primitive thnm best-proto))
		     (defattach-pvsio best-decl name proto formals chk-xfmls body primitive)))))
      (pvs-message "Error (pvs-attachments): Declaration ~a not found."
		   thnm))))

(defun pvsio-attachment-key (decl)
  "Compute unique id based on internal decl id and time stamp of the declaraton's theory."
  (when (and (const-decl? decl)
	     (not (skolem-const-decl? decl)))
    (let ((theory (module decl)))
      (format nil "~a.~a-~a"
	      (extra-qid-theory theory)
	      (get-unique-id decl)
	      (typecheck-time theory)))))

(defun defattach-pvsio (decl name proto formals xformals body primitive)
  (let* ((theory     (module decl))
	 (key 	     (pvsio-attachment-key decl))
	 (fpvsio     (makesym "pvsio-~a.~a" (extra-qid-theory theory) (get-unique-id decl)))
	 (attach     (make-attachment :theo-qid (extra-qid-theory theory)
				      :name name
				      :formals formals
				      :xformals xformals
				      :proto proto
				      :tc-time (typecheck-time theory)
				      :declaration decl
				      :primitive primitive
				      :source (car *pvs-attachment-source-file*)
				      :fsymbol fpvsio)))
    (setf (gethash key *pvsio-attachments*) attach)
    (defattach-body attach body)))

(defmacro pvsio-not-in-prover (name)
  `(when (and *in-checker* (not *in-evaluator*))
     (error 'pvsio-inprover :format-control ,(format nil "~
~a is defined as a semantic attachment.
It cannot be evaluated in a formal proof." name))))

(defun let-xformals (xformals n)
  (when xformals
    (let ((xformal (car xformals)))
      (cons
       (list (extra-formal-varid xformal)
	     (if (eq (extra-formal-kind xformal) :type)
		 `(gethash (nth ,n decl-actuals_) *pvsio-type-hash*)
	       `(nth ,n decl-actuals_)))
       (let-xformals (cdr xformals) (1+ n))))))

;; decls is a list of declarations for defun, e.g., (declare (ignore x))
(defun defattach-body (attachment body &optional decls)
  (if (and body (listp (car body)) (equal (caar body) 'declare)) ;; Car of body is a declaration
      (defattach-body attachment (cdr body) (append decls (list (car body))))
    (let* ((theory
	    (attachment-theo-qid attachment))
	   (name
	    (attachment-name attachment))
	   (formals
	    (attachment-formals attachment))
	   (proto
	    (attachment-proto attachment))
	   (fsymbol
	    (attachment-fsymbol attachment))
	   (source
	    (attachment-source attachment))
	   (formalstr
	    (format nil "~@[(~{~a~@[::~a~]~^,~})~]~@[::~a~]"
		    (merge-lists formals (cdr proto)) (car proto)))
	   (dobo
	    (if (and body (cdr body) (stringp (car body)))
		body
	      (cons nil body)))
	   (let-xformals
	    (let-xformals (attachment-xformals attachment) 0))
	   (let-body
	    (if let-xformals
		`((let (,@let-xformals) ,@(cdr dobo)))
		(cdr dobo)))
	   (theo-decl (pp-xformals attachment))
	   (thnmpro
	    (format nil "~a.~a" theory (defattach-long-name attachment)))
	   (newdoc
	    (format nil "~
PVSio Declaration: ~a~@[~a~].~a~@[~a~]~a
PVS Resolution: ~a~
~@[~%Source File: ~a~]
Lisp Function Symbol: ~a
Lisp Code: ~{~a~}~
~@[~%~a~]"
		    theory (car theo-decl) name (cdr theo-decl) formalstr
		    thnmpro
		    source
		    fsymbol
		    let-body
		    (car dobo)))
	   (newformals
	    (append formals (list '&key 'decl-actuals_)))
	   (newbody
	    (if (attachment-primitive attachment)
		let-body
	      (cons `(pvsio-not-in-prover ,thnmpro) let-body))))
      (append `(defun ,fsymbol ,newformals)
	      (cons '(declare (ignorable decl-actuals_)) decls)
	      (cons newdoc newbody)))))

(defmacro defattach (thnm formals &rest body)
  (defattach-thnm (symbol-name thnm) formals body))

(defmacro defprimitive (thnm formals &rest body)
  (defattach-thnm (symbol-name thnm) formals body :primitive t))

(defun attach-theory (theory attachments)
  (mapcar #'(lambda (defattach)
	      (if (and (consp defattach) (memq (car defattach) '(defattach defprimitive)))
		  (let ((primitive (eq (car defattach) 'defprimitive))
			(name      (symbol-name (cadr defattach)))
			(formals   (caddr defattach))
			(body      (cdddr defattach)))
		    (attach-theory-name theory name formals body
					:primitive primitive))
		  defattach))
	  attachments))

(defmacro attachments (theory-id &rest attachments)
  (let* ((source     (car *pvs-attachment-source-file*))
         (theories   (cdr *pvs-attachment-source-file*))
	 (loadedfile (when source (gethash source *pvsio-loaded-files*)))
	 (theory     (extra-get-theory theory-id :imported source))
	 (theoqid    (extra-qid-theory theory)))
    (cond ((and theory (outdated-theory theory theories))
	   (add-updated-theory theory loadedfile)
	   (remove-attachment-theory theoqid)
	   (pvs-message "Loading semantic attachments of theory ~a" theoqid)
	   `(progn
	      ,@(attach-theory theory attachments)))
	  (theory
	   (or (add-updated-theory theory loadedfile)) t)
	  (loadedfile
	   (or (add-dangling-theory theory-id loadedfile)) t)
	  (t
	   (pvs-message "Error (pvs-attachments): Theory ~a not found in current context."
			theory-id)))))

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
