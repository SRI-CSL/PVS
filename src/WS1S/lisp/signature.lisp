(in-package :pvs)

(defvar *ws1s-signature* nil)

;; Signature

(defvar *fset-of-nats* 
  (let* ((*current-context* (context (get-theory "finite_sets_def[nat]")))
         (*current-theory* (module *current-context*))
         (*generate-tccs* 'none))
    (pc-typecheck (pc-parse "finite_set[nat]" 'type-expr))))

(defun 0th-order? (expr)
  (tc-eq (type expr) *boolean*))

(defun 1st-order? (expr)
  (let ((types (judgement-types+ expr)))
    (some #'(lambda (ty)
	      (subtype-of? ty *naturalnumber*))
	  types)))

(defun 2nd-order? (expr)
  (or (subtype-of? (type expr) *fset-of-nats*)
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

(defun ge1? (trm)
  (and (application? trm)
       (number-ge-op? (operator trm))
       (1st-order? (args1 trm))
       (1st-order? (args2 trm))))

(defun gt1? (trm)
  (and (application? trm)
       (number-gt-op? (operator trm))
       (1st-order? (args1 trm))
       (1st-order? (args2 trm))))

(defun lt1? (trm)
  (and (application? trm)
       (number-lt-op? (operator trm))
       (1st-order? (args1 trm))
       (1st-order? (args2 trm))))

(defun le1? (trm)
  (and (application? trm)
       (number-le-op? (operator trm))
       (1st-order? (args1 trm))
       (1st-order? (args2 trm))))
