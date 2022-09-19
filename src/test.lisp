(in-package :pvs)

;;; (xref:get-relation rel name1 name2)
;;;  rel is one of:
;;;   :direct-calls: true if name1 directly calls name2.
;;;   :indirect-calls: true if name1 indirectly calls name2 (i.e. refs #'bar for function bar).
;;;   :macro-calls: true if name1 calls the macro identified by name2.
;;;   :calls: true if any of the three previous relations is true.
;;;   :binds: true if name1 binds the global variable identified by name2.
;;;   :references: true if name1 references the global variable identified by name2.
;;;   :sets: true if name1 sets the global variable identified by name2.
;;;   :uses: true if any of the three previous relations are true.

;;; Example usage: find all files which have functions that call a given macro
;;; Useful when a macro is modified.
(defun source-files-using-macro (mname)
  (mapcar #'pathname-name
    (remove-if-not #'pathnamep
      (remove-duplicates
	  (mapcar #'excl:source-file
	    (xref:get-relation :macro-calls :wild mname))))))

;;; To run the Allegro profiler:
;;;   (prof:with-profiling () <body>)
;;;   (prof:show-flat-profile) or (prof:show-call-graph) give the information
;;; prof:*hidden-packages* and prof:*hidden-functions* control what is displayed
;;;   note that these affect the display, not the collection
;;; prof:reset-profiler-display-defaults 

(defparameter *hidden-functions*
  '("start_reborn_lisp" "first_lisp_thread" excl::lisp-thread-start
    excl::run-initial-thread excl::thread-reset-catcher excl::thread-bind-and-call
    (:internal (:effective-method 1 t t t t) 0)
    (:internal (:effective-method 4 nil nil t t) 0)
    (:internal (:effective-method 2 nil nil t t) 0)
    (:internal (:effective-method 1 nil nil t t) 0)
    every some
    (:internal excl::effective-method-eql 0)
    (:internal (:effective-method 2 t nil t t) 0)
    (:internal (:effective-method 3 nil nil t t) 0)
    (:internal (:effective-method 1 t nil t t) 0)
    (method initialize-instance (standard-object))
    "smp_request_gc_op" "sem_getvalue" "count_call" clrhash excl::instance-read-1
    (method shared-initialize (standard-object t))
    "sem_wait" excl::sort-list excl::merge-lists* reduce "lisp_cons"
    excl::typep-class "(lisp-trampolines)" (:discriminator (:caching (class) nil))
    (:internal excl::compute-default-constructor 0)
    excl::gethash_2op_1ret "new_simple_vector" "bind_one_spec"
    excl::allocate-object-of-standard-class "restify2"
    (:discriminator (:n-n-reader)) excl::.inv-si-slot-value
    (:discriminator (:caching (class class t) nil))
    intern excl::package-name-cvt-1 "q_wrapper"
    (:internal (:effective-method 1 t t nil nil) 0)
    pvs-sxhash-lists-a
    (:internal (:effective-method 2 t nil nil t) 0)
    excl::validate-make-instance-initargs
    "qsymeval" "start" "function_lisp_apply"
    excl::fast-list-position-eq excl::list-remove-if format
    "unbind_one_spec"))

(defun hide-from-profile ()
  (pushnew (find-package :prof) prof:*hidden-packages*)
  (dolist (fun *hidden-functions*)
    (pushnew fun prof:*hidden-functions*)))

(defun hide-methods-from-profile (gfun)
  (dolist (mthd (collect-method-forms gfun))
    (pushnew mthd prof:*hidden-functions*)))

(defun get-current-backtrace ()
  (handler-case (error "foo")
    (error (cond)
      (with-output-to-string (stream)
	(top-level.debug:zoom stream)))))

;;; To get more information from Allegro compilation
;;;  (proclaim '(:explain :types :calls :boxing :variables :tailmerging :inlining))

;;; If the tracer seems to not work (especially in the debugger),
;;;  (setq lucid::*trace-suppress* nil)
;;;  (setq excl::*inhibit-trace* nil)     for allegro

;;; To see where a slot is set incorrectly, you need to use two methods:
;;;  (defmethod initialize-instance :around ((expr expr) &rest initargs)
;;;    (call-next-method)
;;;    (unless (and (integerp (parens expr)) (>= (parens expr) 0))
;;;      (break))
;;;    expr)
;;;
;;;  (defmethod (setf parens) :around (val (expr expr))
;;;    (unless (>= val 0) (break)) (call-next-method))

;;; (remove-method #'initialize-instance (find-method #'initialize-instance '(:around) (list (find-class 'resolution))))

;;; To make it so that require, load, etc. work from an allegro image,
;; (setf (logical-pathname-translations "sys")
;;       `((";**;*.*" "/csl/allegro/allegro8.0/")
;; 	   ("**;*.*" "/csl/allegro/allegro8.0/")))
(setf (excl:bundle-pathname) #P"/home/owre/acl/files.bu")

(setf (logical-pathname-translations "sys")
       `((";**;*.*" "/home/owre/acl/")
 	   ("**;*.*" "/home/owre/acl/")))

;; #+allegro
;; (defun start-ide ()
;;   (setf (logical-pathname-translations "sys")
;;        `((";**;*.*" "/csl/allegro/allegro8.1/")
;; 	 ("**;*.*" "/csl/allegro/allegro8.1/")))
;;   (require :ide)
;;   ;;(setq excl:*mozilla-library-path* "/homes/owre/lib/firefox-1.0.7/")
;;   (ide:start-ide))

#+allegro
(defun show-unreferenced-functions ()
  (let ((pvs-package (find-package :pvs)))
    (do-symbols (x pvs-package)
      (when (and (eq (symbol-package x) pvs-package)
		 (fboundp x))
	(unless (xref:get-relation :calls :wild x)
	  (format t "~%~a is unreferenced" x))))))

(defun all-external-symbols (pkg)
  (let ((ext-symbs nil))
    (do-symbols (x (find-package pkg))
      (multiple-value-bind (sym stat) 
	  (find-symbol (string x) pkg)
	(when (eq stat :external)
	  (push sym ext-symbs))))
    ext-symbs))

(defun doc (symbol)
  (dolist (doctype '(variable function structure type setf)) ; cltl2, p. 752
    (let ((doc (documentation symbol doctype)))
      (when doc
	(format t "~%~a: ~a ~a~%~a~%"
	  symbol doctype
	  (case doctype
	    (variable (symbol-value symbol))
	    (function
	     #+allegro (excl::func_formals (symbol-function symbol))
	     #-allegro "unknown args")
	    (structure
	     (break "check"))
	    (type
	     (break "type"))
	    (setf
	     (break "setf")))
	  doc)))))

(defparameter *ignored-special-variables*
  '(*integer* *real* *posint* *naturalnumber* *number_field* *even_int*
    *number* *odd_int* *boolean* *false* *true* *pvs-directories*
    *rational-pred* +boolops+ *integer-pred* *pvs-operators*
    *infix-operators* pr-outfile *strat-file-dates*
    *fast-make-instance-makers* PVS-ALL-OPERATORS-LIST
    *slot-info* *last-end-value*
    *expr-prec-info* PVS-KEYWORD-LIST
    whitespace-chars *load-prelude-hook*
    PVS-RESTRICTED-CHARS))
	      

(defun check-special-variables ()
  (do-symbols (x)
    (when (and (special-variable-p x)
	       (boundp x)
	       (eq (symbol-package x) (find-package :pvs))
	       (typep (symbol-value x) '(not (or number symbol package stream
						 readtable token)))
	       (not (memq x *ignored-special-variables*)))
      (format t "~%~a - ~a" x
	      (typecase (symbol-value x)
		(hash-table (symbol-value x))
		(list (format nil "list - length ~d"
			(if (consp (cdr (symbol-value x)))
			    (length (symbol-value x))
			    -1)))
		(t (type-of (symbol-value x))))))))

(defmacro trace* (&rest trace-forms)
  `(trace ,@(expand-trace-form-methods trace-forms)))

(defun expand-trace-form-methods (trace-forms &optional nforms)
  (if (null trace-forms)
      (nreverse nforms)
      (let ((nform (expand-trace-form-method (car trace-forms))))
	(expand-trace-form-methods
	 (cdr trace-forms)
	 (if (consp nform)
	     (append nform nforms)
	     (cons nform nforms))))))

(defun expand-trace-form-method (trace-form)
  (if (consp trace-form)
      (if (or (eq (car trace-form) 'method)
	      (consp (car trace-form))
	      (not (typep (symbol-function (car trace-form))
			  'generic-function)))
	  trace-form
	  (mapcar #'(lambda (method-form)
		      (cons method-form (cdr trace-form)))
	    (collect-method-forms (car trace-form))))
      (if (typep (symbol-function trace-form) 'generic-function)
	  (mapcar #'list (collect-method-forms trace-form))
	  trace-form)))

(defun trace-methods (funsym)
  (dolist (frm (collect-method-forms funsym))
    (eval `(trace (,frm)))))

(defun collect-method-forms (funsym)
  (mapcar #'(lambda (m)
	      (let ((q (method-qualifiers m))
		    (a (mapcar #'class-name (method-specializers m))))
		`(method ,funsym ,@q ,a)))
    (generic-function-methods (symbol-function funsym))))


;; (in-package :monitor)
;; ;;; change to metering.lisp to monitor methods

;; (defun PLACE-FUNCTION (function-place)
;;   "Return the function found at FUNCTION-PLACE. Evals FUNCTION-PLACE
;; if it isn't a symbol, to allow monitoring of closures located in
;; variables/arrays/structures."
;;   (if (symbolp function-place)
;;       (symbol-function function-place)
;;       (if (listp function-place)
;; 	  (fboundp function-place)
;; 	  (eval function-place))))

;; (defun required-arguments (name)
;;   (let* ((function (fboundp name))
;;          (args #+:excl(excl::arglist function)
;; 	       #-:excl(arglist function))
;;          (pos (position-if #'(lambda (x)
;;                                (and (symbolp x)
;;                                     (let ((name (symbol-name x)))
;;                                       (and (>= (length name) 1)
;;                                            (char= (schar name 0)
;;                                                   #\&)))))
;;                            args)))
;;     (if pos
;;         (values pos t)
;;         (values (length args) nil))))

;; (defun monitoring-unencapsulate (name &optional warn)
;;   "Removes monitoring encapsulation code from around Name."
;;   (let ((finfo (get-monitor-info name)))
;;     (when finfo				; monitored
;;       (when (symbolp name)
;; 	(remprop name 'metering-functions))
;;       (setq *monitored-functions* 
;; 	    (remove name *monitored-functions* :test #'equal))
;;       (if (eq (place-function name)
;; 	      (metering-functions-new-definition finfo))
;; 	  (setf (place-function name)
;; 		(metering-functions-old-definition finfo))
;; 	  (when warn
;; 	    (warn "Preserving current definition of redefined function ~S."
;; 		  name))))))

;; (defun collect-pvs-methods-on-class (class)
;;   (do-symbols (x)
;;     (when (and (fboundp x)
;; 	       (clos::generic-function-p (symbol-function x)))
;;       (dolist (m (generic-function-methods (symbol-function x)))
;; 	(when (member class (method-specializers m) :key
;; 		      #'(lambda (y) (unless (typep y 'eql-specializer)
;; 				      (class-name y))))
;; 	  (let ((q (method-qualifiers m))
;; 		(a (mapcar #'(lambda (y)
;; 			       (if (typep y 'eql-specializer)
;; 				   `(eql ,(eql-specializer-object y))
;; 				   (class-name y)))
;; 		     (method-specializers m))))
;; 	    (format t "~%~a~%   is in ~a" `(method ,x ,@q ,a)
;; 		    (excl:source-file m))))))))

;; (in-package :pvs)

;; ;; (defun write-method-template-to-file (fun-name args file &optional slots)
;; ;;   (let ((mfile (format nil "~a/src/file" *pvs-path*)))
;; ;;     (with-open-file (out mfile :direction :output :if-exists :supersede)
;; ;;       (format out
;; ;; 	  "(in-package :pvs)~2%~
;; ;;            (defun ~a (obj ~a)
;; ;;              (~a* obj ~a))"
;; ;; 	fun-name args fun-name args)
;; ;;       (dolist (si *slot-info*)
;; ;; 	(write-method-template fun-name args (car si) out)))))

;; (defun write-restore-methods-to-file ()
;;   (let ((mfile (format nil "/tmp/restore.lisp")))
;;     (with-open-file (out mfile :direction :output :if-exists :supersede)
;;       (format out "(in-package :pvs)~2%")
;;       (dolist (si *slot-info*)
;; 	(write-restore-methods (car si) out)))))

;; (defun write-restore-methods (name out)
;;   (let* ((slots (get-all-slots-of (list name)))
;; 	 (saved-slots (saved-slots% slots))
;; 	 (restored-slots (restored-slots% saved-slots)))
;;     (format out "~2%")
;;     (write `(defmethod restore-object* ((obj ,name))
;; 	      (let ((*restore-object-parent* obj))
;; 		(with-slots ,(mapcar #'car restored-slots) obj
;; 		  ,@(mapcar #'(lambda (a)
;; 				`(let ((*restore-object-parent-slot*
;; 					',(car a)))
;; 				   (restore-object* (,(car a) obj))))
;; 		      restored-slots)
;; 		  obj)))
;; 	   :stream out :level nil :length nil :pretty t)))

;; (defun restored-slots% (args)
;;   (remove-if #'(lambda (a) (memq :restore-as a)) args))

;; (defmacro monitor-form (form 
;; 			&optional exclude (nested :exclusive) (threshold 0.01)
;; 			(key :percent-time))
;;   "Monitor the execution of all functions in the current package
;; during the execution of FORM.  All functions that are executed above
;; THRESHOLD % will be reported."
;;   `(unwind-protect
;;        (progn
;; 	 (mon:monitor-all)
;; 	 (mon:unmonitor ,@exclude)
;; 	 (mon:reset-all-monitoring)
;; 	 (time ,form)
;; 	 (mon:report-monitoring :all ,nested ,threshold ,key :ignore-no-calls))
;;      (mon:unmonitor))))

;; (excl:def-fwrapper gethash-wrap (key hash-table)
;; 		   (when (> (hash-table-count hash-table) 2000)
;; 		     (break "Big table: ~a" hash-table))
;; 		   (excl:call-next-fwrapper))

;; (excl:fwrap 'gethash 'gethashwrap1 'gethash-wrap)

;;; Like show, but only gives the "interesting" slots
(defun show-ps (ps)
  (format t "~a (class ~a): ~{~% ~{~25a~a~}~}"
    ps (class-of ps)
    (mapcar #'(lambda (x) (list x (slot-value ps x)))
      '(current-rule current-xrule current-input status-flag current-subgoal pending-subgoals remaining-subgoals done-subgoals))))

(defun strategy-summary ()
  (let ((strats nil))
    (do-all-strategies
     #'(lambda (st)
	  (if (rule-entry? st)
	      (push
	       (cons (name st)
		     (if (optional-args st)
			 (append (required-args st)
				 (cons '&optional (optional-args st)))
			 (required-args st)))
	       strats)
	      (unless (char= (char (string (name st))
				   (1- (length (string (name st)))))
			     #\$)
		(push (cons (name st) (formals st)) strats)))))
    (format t "~{~%~a~}" (sort strats #'string< :key #'car))))

(defun class-hierarchy (name)
  (class-hierarchy* (list (find-class name)) nil))

(defun class-hierarchy* (classes all-classes)
  (if (null classes)
      (mapcar #'class-name all-classes)
      (if (memq (car classes) all-classes)
	  (class-hierarchy* (cdr classes) all-classes)
	  (class-hierarchy* (append (class-direct-subclasses (car classes)) (cdr classes))
			    (cons (car classes) all-classes)))))

;;; From https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/Levenshtein_distance#Common_Lisp
(defun levenshtein-distance (str1 str2)
  "Calculates the Levenshtein distance between str1 and str2, returns an editing distance (int)."
  (let ((n (length str1))
	(m (length str2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance m))
	  ((= 0 m) (return-from levenshtein-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all chars of each string
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (char-equal (schar str1 i) (schar str2 j)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))
