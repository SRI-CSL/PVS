;;; -*- Mode: Lisp; Package: SB-RUNTIME -*-
(in-package "SB-RUNTIME")  (use-package :ergolisp)

(use-package '("OPER" "OCC" "TERM" "SORT" "LANG"))



(defparameter *sbrt-sort-table*
              (make-sort-table
                '((#t(:sort id) . #t(:union))
		  (#t(:sort cid) . #t(:union))
		  (#t(:sort number) . #t(:union))
		  (#t(:sort string) . #t(:union))
		  (#t(:sort literal) . #t(:union))
		  (#t(:sort keyword) . #t(:union)))))
