;;; -*- Mode: Lisp; Package: sb-runtime -*-
(in-package :sb-runtime)  (use-package :ergolisp)

(use-package '(:oper :occ :term :sort :lang))


;; #+(and allegro-version>= (version>= 8 2))
;; (eval-when (:execute :compile-toplevel :load-toplevel)
;;   (setq *readtable* cl::*pvs-readtable*))


(defparameter *sbrt-sort-table*
              (make-sort-table
                '((#t(:sort id) . #t(:union))
		  (#t(:sort cid) . #t(:union))
		  (#t(:sort number) . #t(:union))
		  (#t(:sort string) . #t(:union))
		  (#t(:sort literal) . #t(:union))
		  (#t(:sort keyword) . #t(:union)))))
