;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; manip-vectors.lisp -- Manip extensions for vectors.
;; Author          : Ben Di Vito <b.divito@nasa.gov>
;; Created On      : 17 Oct 2008
;; Last Modified By: 
;; Last Modified On: 17 Mar 2009 (v1.3-beta)
;; Last Modified On: 17 Mar 2009 (v1.3)
;; Status          : Development
;; Version         : 1.3
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Requires Manip 1.3 or later.
;;;
;;;===== add macro to collect names of vector lemmas used


(defclass pvs-type-vector () () )   ; for vectors of arbitrary dimension

(defclass pvs-type-vect2 (pvs-type-vector) () )
(defclass pvs-type-vect3 (pvs-type-vector) () )
(defclass pvs-type-vectn (pvs-type-vector) () )

(defun refresh-vector-types ()
  (loop for name in '(|Vect2| |Vect3| |VectN|)
        for class in '(pvs-type-vect2 pvs-type-vect3 pvs-type-vectn)
        for res = (resolve name 'type nil)
        when res
          do (register-manip-type (type (car (resolutions res))) class)))

(register-manip-lib
 "vectors"
 #'refresh-vector-types
 "Extends several Manip strategies to work with vectors.
Invoke (help manip-vectors) to see full documentation.")

(defhelper manip-vectors ()
  (skip)
  "[Manip] The following strategies provide support for vectors:

 - swap:  commutes vector sums and scalar/dot products
 - group: associates vector sums and scalar/dot products
 - swap-group: combination of these two actions
 - mult-by: create scalar products or dot products according to types
            in formula and new term.
 - div-by: dividing by real x creates scalar product (1/x)*v. 
           dividing by vector u creates scalar product (1/norm(u))*v. 
 - move-terms: behaves with vector sums exactly as for reals.
 - cross-mult: acts on divisors in the real part of scalar products.
 - factor: factors the real and vector parts of sums of scalar products.

Several new strategies for vectors were introduced whose actions aren't
normally needed for reals, but will work for reals as well as vectors:

 - permute-terms: reorder additive terms on one side of a relation
 - cancel-add: find and cancel additive terms within a formula
 - distrib: distribute multiplication over additive terms

In addition, the following strategies work for vectors because they
invoke vector-aware strategies internally:

 - isolate, isolate-replace, cross-add (using move-terms)

Several strategies work the same on scalar products as they do for real
products, although their actions are limited to the real part of a
scalar product:

 - permute-mult, name-mult
"
  "")


;;; ===================

;; For swap

(defmethod manip-commutativity-lemmas ((term-class pvs-type-vector))
  '("add_comm" "dot_comm"))

;; For group, swap-group

;; (defparameter vector-add-assoc-lemmas '("add_assoc"))

(defmethod manip-associativity-lemmas
    ((cl-x pvs-type-vector) (cl-y pvs-type-vector) (cl-z pvs-type-vector))
  '("add_assoc"))   ;; only addition is possible (ignoring 3D cross products)

(defmethod manip-associativity-lemmas
    ((cl-x pvs-type-real) (cl-y pvs-type-real) (cl-z pvs-type-vector))
  '("scal_assoc"))  ;; a * (b * u)

(defmethod manip-associativity-lemmas
    ((cl-x pvs-type-real) (cl-y pvs-type-vector) (cl-z pvs-type-vector))
  '("dot_assoc"))   ;; a * (u * v)


(defmethod manip-comm-assoc-lemmas
    ((cl-x pvs-type-vector) (cl-y pvs-type-vector) (cl-z pvs-type-vector) dir)
  ;; only addition is possible (ignoring 3D cross products)
  (if (eq dir 'l) '("add_comm_assoc_right") '("add_comm_assoc_left")))

(defmethod manip-comm-assoc-lemmas
    ((cl-x pvs-type-real) (cl-y pvs-type-real) (cl-z pvs-type-vector) dir)
  (if (eq dir 'l) '("scal_comm_assoc") nil))

(defmethod manip-comm-assoc-lemmas
    ((cl-x pvs-type-real) (cl-y pvs-type-vector) (cl-z pvs-type-vector) dir)
  (if (eq dir 'l) '("dot_scal_right") '("dot_scal_comm_assoc")))

(defmethod manip-comm-assoc-lemmas
    ((cl-x pvs-type-vector) (cl-y pvs-type-real) (cl-z pvs-type-vector) dir)
  (if (eq dir 'l) '("dot_scal_right") nil))



;; For mult-by

;;;; div by real: mult by 1/a
;;;; div by vect: mult by 1/norm(v)

(defmethod mult/div-by-steps ((rel-cl pvs-type-vector) (term-cl pvs-type-real)
			      op term fnum sign formula relation)
  (when (eq op '/)
    (setq term (format nil "(1/~A)" (safety-parens term))))
  (cond ((not (eq relation '=))
	 (manip-unsuitable-fnum 'mult/div-by fnum))
	((< fnum 0)
	 (mult-by-scal/dot term fnum formula))
	(t (mult-by-scal term fnum formula))))

(defmethod mult/div-by-steps ((rel-cl pvs-type-real) (term-cl pvs-type-vector)
			      op term fnum sign formula relation)
  (when (eq op '/)
    (setq term (format nil "(1/norm(~A))" term)))
  (cond ((not (eq relation '=))
	 (manip-unsuitable-fnum 'mult/div-by fnum))
	((< fnum 0)
	 (mult-by-scal/dot term fnum formula t))
	(t (mult-by-scal term fnum formula t))))

;; The following three methods have the same definition.  They have been
;; duplicated so that the argument specializers will allow only cases
;; of matching vector types.

(defmethod mult/div-by-steps ((rel-cl pvs-type-vect2) (term-cl pvs-type-vect2)
			      op term fnum sign formula relation)
  (mult/div-by-dot/norm op term fnum sign formula relation))

(defmethod mult/div-by-steps ((rel-cl pvs-type-vect3) (term-cl pvs-type-vect3)
			      op term fnum sign formula relation)
  (mult/div-by-dot/norm op term fnum sign formula relation))

(defmethod mult/div-by-steps ((rel-cl pvs-type-vectn) (term-cl pvs-type-vectn)
			      op term fnum sign formula relation)
  (mult/div-by-dot/norm op term fnum sign formula relation))

(defun mult/div-by-dot/norm (op term fnum sign formula relation)
  (cond ((and (eq relation '=) (eq op '*) (< fnum 0))  ;; dot product
	 (mult-by-scal/dot term fnum formula))
	((and (eq relation '=) (eq op '/) (< fnum 0))  ;; div by norm
	 (mult-by-scal/dot (format nil "(1/norm(~A))" term) fnum formula))
	((and (eq relation '=) (eq op '/))             ;; div by norm
	 (mult-by-scal (format nil "(1/norm(~A))" term) fnum formula))
	(t (manip-unsuitable-fnum 'mult/div-by fnum))))

;; If multiplication of an antecedent equality involves vectors,
;; we don't need the lemmas in the vectors library.
;; Both scalar products and dot products are supported.

(defun mult-by-scal/dot (term fnum formula &optional swap?)
  (let* ((lhs-text
	  (if swap?
	      (format nil "~A * ~A" (safety-parens (args1 formula)) term)
	      (format nil "~A * ~A" term (safety-parens (args1 formula)))))
	 (rhs-text
	  (if swap?
	      (format nil "~A * ~A" (safety-parens (args2 formula)) term)
	      (format nil "~A * ~A" term (safety-parens (args2 formula)))))
	 (new-eq (format nil "~A = ~A" lhs-text rhs-text))
	 (both-sides-step `(claim$ ,new-eq (replace ,fnum)))
	 (target (if (< fnum 0) (- fnum 1) fnum))  ;;; protect
;	 (cancel-steps `((rewrite "div_cancel2" ,fnum)))  ;; useful still?
	 )
    `(then ,both-sides-step (hide ,target) (assert))))

;; Multiplication of equalities involving scalars and vectors
;; (not valid for dot products).
;; Requires lemmas in the vectors library.
;;;;; can work for mult-by nonzero vector???

(defun mult-by-scal (term fnum formula &optional rel-real?)
  (let* ((lhs-text (textify (args1 formula)))
	 (rhs-text (textify (args2 formula)))
	 (both-sides-lemma
	  (if rel-real? "scal_cancel" "dot_divby"))   ;;; name changes?
	 (inst-step (if rel-real?
			`(inst -1 ,lhs-text ,rhs-text ,term)
			`(inst -1 ,term ,lhs-text ,rhs-text)))
	 (target (if (< fnum 0) (- fnum 1) (+ fnum 1)))
	 (step-list `(then (lemma ,both-sides-lemma)
			   ,inst-step
			   (branch (split -1 1)
				   ((assert)
				    (then (hide ,target)  ;;;  ,@cancel-steps)
					  (assert)))))))
    step-list))

;;; ===================

;;; For move-terms

(defmethod manip-additive-zero ((vect pvs-type-vector)) "zero")

;; Vector term movement is handled mostly like reals.  Grind-with-ext
;; can prove the main equivalence, although it could be slow or go down
;; unproductive paths.  Name-replace on all of the terms is performed
;; first to reduce the equivalence to its simplest form.

(defmethod move-terms-just-step ((vect pvs-type-vector)
				 side from-list to-list tnums)
  (let* (;;;(num-terms (length from-list))
	 (zero (manip-additive-zero vect))
	 (name-steps
	  (loop for expr in (append from-list to-list)
	        unless (equal (textify (cadr expr)) zero)
  	          collect `(name-replace ,(name-gensym "move_vect")
					 ,(cadr expr) :hide? t))))
    `(then (hide-all-but 1) ,@name-steps (expand "zero") (rewrite-msg-off)
	   (unwind-protect$ (grind-with-ext) (rewrite-msg-on)))))

;;; For permute-terms

;; Justification for vector term permutation is nearly the same as
;; for term movement.

(defmethod permute-terms-just-step ((vect pvs-type-vector) terms tnums)
  (let* (;;;(num-terms (length terms))
	 (zero (manip-additive-zero vect))
	 (name-steps
	  (loop for expr in terms
	        unless (equal (textify (cadr expr)) zero)
  	          collect `(name-replace ,(name-gensym "perm_vect")
					 ,(cadr expr) :hide? t))))
    `(then (hide-all-but 1) ,@name-steps (rewrite-msg-off)
	   (unwind-protect$ (grind-with-ext) (rewrite-msg-on)))))


;;; ===================

;;; For cross-mult

(defmethod cross-mult-cond ((num pvs-type-vector) formula left-side)
  (and (is-relation formula)
       (is-term-operator
	(if left-side (args1 formula) (args2 formula)) '*)
       (is-term-operator
	(args1 (if left-side (args1 formula) (args2 formula))) '/)))

(defmethod cross-mult-step ((vect pvs-type-vector)
			    fnum formula relation left-side side-obj other-obj)
  ;; a/b * u = v IFF a * u = b * v
  (if (eq relation '=)
      (let* ((vector-lemma
	      (if left-side "scal_div_mult_left" "scal_div_mult_right"))
	     (adj-fnum (if (< fnum 0) (- fnum 1) fnum)))
	;; rewrite has issues with deleting target formula
	`(then (lemma ,vector-lemma)
	       (inst? -1 :where ,adj-fnum)
	       (replace -1 ,adj-fnum :hide? t)
	       (assert ,fnum)))
      (manip-unsuitable-fnum 'cross-mult fnum)))


;;; ===================

;; For scalar products of the form a_1*...*a_n * u, collect the real
;; factors a_1,...,a_n and return as a left-mult list.  The right-mult
;; list will contain u.  An infix application of '*' will be included if
;; the left operand (at least) is real.  Returns (left-mult right-mult).

(defun collect-scalar-terms (mult-expr)
  (collect-multiplicative-terms
   mult-expr
   #'(lambda (expr) (subtype-of? (type expr) *number_field*))
   #'(lambda (a b)  (subtype-of? (type a) *number_field*))))


;;; For factor

(defmethod collect-factor-terms ((val-cl pvs-type-vector) add-terms)
  (mapcar #'(lambda (expr) (collect-scalar-terms expr))
	  add-terms))

;; The justification proof for equating the original and factored
;; expressions needs to apply scalar product distribution over
;; additive vector terms.  Care needs to be taken because the
;; prover's arithmetic simplification can reorder terms.  An
;; intermediate equality is introduced to avoid the reordering.

(defmethod factor-steps ((val-cl pvs-type-vector) fnum id?)
   (let* ((adj-fnum (if (< fnum 0) (- fnum 1) fnum))
	  (replace-one `(replace -1 ,adj-fnum :hide? t))
	  (expand-id (if id? '((expand "id")) nil))
	  (rewrites-1
	   `(else*$ ,@(targeted-rewrites fnum '("scal_add_left"
						"scal_sub_left"))))
	  (rewrites-2
	   `(else*$ ,@(targeted-rewrites fnum '("scal_add_right"
						"scal_sub_right")))))
     `(,replace-one
       (then (hide-all-but 1) ,@expand-id (assert)
	     (repeat ,rewrites-1) (repeat ,rewrites-2)
	     (assert) (cancel-add$ ,fnum)))))


;;; For distrib

;; Scalar product distribution

(defmethod distrib-step ((left-cl pvs-type-real) (right-cl pvs-type-vector)
			 expr-obj fnum)
  (distrib-name-steps expr-obj fnum '("scal_add_left" "scal_sub_left"
				      "scal_add_right" "scal_sub_right")))

;; Dot product distribution

(defmethod distrib-step ((left-cl pvs-type-vector) (right-cl pvs-type-vector)
			 expr-obj fnum)
  (distrib-name-steps expr-obj fnum '("dot_add_left" "dot_sub_left"
				      "dot_add_right" "dot_sub_right")))

(defun distrib-name-steps (expr-obj fnum rewrite-lemmas)
  (let* ((name-step `(name ,(name-gensym "distrib") ,expr-obj))
	 (adj-fnum (if (< fnum 0) (- fnum 1) fnum))
	 (replace-1 `(replace -1 ,adj-fnum))
	 (rewrites `(repeat (else*$ ,@(targeted-rewrites -1 rewrite-lemmas))))
	 (replace-2 `(replace -1 ,adj-fnum :dir rl :hide? t)))
    `(then ,name-step ,replace-1 (assert -1) ,rewrites ,replace-2)))


;;; For permute-mult

(defmethod collect-mult-factors ((val-cl pvs-type-vector) expr)
  (collect-scalar-terms expr))


;;;;;;;;;;

;;; For cancel-add

(defmethod cancel-add-same-side-step ((val-cl pvs-type-vector) fnum side)
  (let* ((formula (manip-get-formula fnum))
	 (terms (collect-additive-terms
		 '+ (if (eq side 'l) (args1 formula) (args2 formula))))
	 (matches (find-matching-terms terms)))
    (if (every #'null matches)
;	     (gen-manip-response 'cancel-add
;				 "No canceling terms found (same side).")
	'(skip)
	(let* ((moves (loop for j from 1 to (length terms)
			    for m in matches
			    when m append (list j m)))
	       (rewrite-rules '("add_cancel_neg2" "add_cancel2"
				"sub_eq_args" "neg_add_left"))
	       (rewrite-step `(else*$ ,@(targeted-rewrites fnum rewrite-rules))))
	  `(then (permute-terms$ ,fnum ,side ,moves r)
		 (repeat ,rewrite-step))))))

;;; Following seems to create problems of startup race conditions.

;(pvs-emacs-eval
; (format nil "(pvs-log-message \"MSG\" ~S)"
;	 "Finished loading manip-vectors.lisp."))
