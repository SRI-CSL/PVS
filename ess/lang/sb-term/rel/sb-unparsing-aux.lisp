;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package :syntax-box)  (use-package :ergolisp)

;;; The following hacks revise the precedences in the unparser for grammars
;;; since there is some wierd anomaly with the parsing of |. 

(defvar sb-bracket-info
        (make-hash-table :test #'eq))
;;(clrhash sb-bracket-info)
(mapc
  #'(lambda (entry)
     (set-bracket-info (car entry) (cadr entry) (caddr entry) sb-bracket-info))
  '((pattern sbst::{ sbst::})))


(defvar sb-prec-info
        (make-hash-table :test #'eq))
;;(clrhash sb-prec-info)
;;(mapc #'(lambda (nt)
;;          (init-prec-info nt sb-prec-info))
;;       '(pattern augment))
(mapc
  #'(lambda (entry)
     (set-prec-info (car entry) (cadr entry) (caddr entry) (cadddr entry)
      sb-prec-info))
  '((pattern sbst::\| 1 0)
    (pattern sbst::\| 2 0)))
