(in-package :pvs)

(defvar *ws1s-signature* nil)

;; Signature

(def-pvs-term fset-of-nats "finite_set[nat]" "record_prelude_example" :nt type-expr)

(def-pvs-term empty-fset-of-nats "emptyset[nat]" "record_prelude_example"
                                 :expected "finite_set[nat]")

(def-pvs-term add-to-fset "add[nat]" "record_prelude_example"
                          :expected "[nat, finite_set[nat] -> finite_set[nat]]")



(defun ws1s-types ()
  (list *boolean*
	*naturalnumber*
	(fset-of-nats)))
  
(defun ws1s-type? (type)
  (member type (ws1s-types) :test #'tc-eq))
  
(defun 0th-order? (expr)
  (subtype-of? (type expr) *boolean*))

(defun 1st-order? (expr)
  (or (subtype-of? (type expr) *naturalnumber*)
      (some #'subtype-of-nat? (judgement-types+ expr))
      (some #'subtype-of-nat? (gethash expr *known-judgements*))))

(defun subtype-of-nat? (type)
  (subtype-of? type *naturalnumber*))
  
(defun 2nd-order? (expr)
  (or (subtype-of? (type expr) (fset-of-nats))
      (finite-set-of-nat? expr)
      (some #'finite-set-of-nat? (gethash expr *known-judgements*))))
 
(defun level (expr)
  (cond ((boolean? expr) 0)
	((1st-order? expr) 1)
	((2nd-order? expr) 2)))

;; Recognizers

(defun var? (expr) 
  (and (name-expr? expr)
       (member (kind expr) '(variable constant))
    ;   (typep (declaration expr) '(and const-decl (not def-decl)))
       (not (def-axiom (declaration expr)))))

(defun var0? (expr) (and (var? expr) (boolean? expr)))
(defun var1? (expr) (and (var? expr) (1st-order? expr)))
(defun var2? (expr) (and (var? expr) (2nd-order? expr)))
