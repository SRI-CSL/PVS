(in-package 'pvs)

(defun cl2pvs (sexpr type &optional context)
  (let ((pexpr (cl2pvs* sexpr type context)))
    (typecheck pexpr :expected type :context context
	       :tccs 'none)))

(defmethod cl2pvs* (sexpr (type type-name) context)
  (if (tc-eq type *number*)
      (mk-number-expr sexpr)
      (if (tc-eq type *boolean*)
	  (if sexpr *true* *false*)
	  (break "Not translateable: ~a" sexpr))))

(defmethod cl2pvs* (sexpr (type subtype) context)
  (cl2pvs* sexpr (find-supertype type) context))

(defmethod cl2pvs* (sexpr (type tupletype) context)
  (mk-tuple-expr (loop for typ in (types type)
		       as i from 0
		       collect (cl2pvs* (svref sexpr i)
					typ
					context))))

(defmethod cl2pvs* (sexpr (type recordtype) context)
  (mk-record-expr
   (loop for fld in (sorted-fields type)
	 as i from 0
	 collect (mk-assignment 'uni
		   (list (list (mk-name-expr (id fld))))
		   (cl2pvs* (svref sexpr i) (type fld) context)))))

(defun list-type? (type)
  (eq (id (module-instance (resolution type))) '|list_adt|))

(defmethod cl2pvs* (sexpr (type adt-type-name) context)
  (let* ((recognizers (recognizers type))
	 (recognizer-funs
	  (if (list-type? type)
	      (list #'null #'consp)
	      (loop for rec in recognizers
		    collect (lisp-function (declaration rec)))))
	 (recognizer (loop for recfun in recognizer-funs
			   as rec in recognizers
			   thereis
			   (and recfun
				(funcall recfun
					 sexpr)
				rec))))
    (if recognizer
	(let* ((constructor (constructor recognizer))
	       (accessors (accessors constructor))
	       (accessor-funs
		(if (list-type? type)
		    (if (eq (id recognizer) '|cons?|)
			(list #'car #'cdr)
			nil)
		    (loop for acc in accessors
			  collect (lisp-function (declaration acc)))))
	       (args (loop for accfn in accessor-funs
			   as acc in accessors
			   collect 
			   (cl2pvs*
			    (funcall accfn
				     sexpr)
			    (range (type acc))
			    context))))
	  (if accessors
	      (mk-application* constructor args)
	      constructor))
	(break "No recognizer for datatype expression ~a" sexpr))))


