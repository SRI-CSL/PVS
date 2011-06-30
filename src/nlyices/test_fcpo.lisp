;;========================================
;; Test: use fcpo via foreign functions
;; this requires test_fcpo.so
;;========================================

(require :foreign)

(load-nl "polyrep-totdeglex.lisp")
(load-nl "decide3_2.lisp")
(load-nl "nlsolver.lisp")
(load-nl "named-callbacks")
(load-nl "fcpo")

(load-nl "test_fcpo.so")

;;-----------------------------------------------------
;; Foreign functions defined in test_fcpo.so
;;-----------------------------------------------------

(ff:def-foreign-call init_callbacks ((n :int) (d (:array ncb:callback-desc)))  :returning :void)

(ff:def-foreign-call test1 (:void) :returning :void)

(ff:def-foreign-call test2 (:void) :returning :void)

(ff:def-foreign-call test3 (:void) :returning :void)


;;---------------------------
;; Initialize the callbacks
;;---------------------------

(defun init ()
  (let ((desc (ncb:get-callbacks)))
    (unwind-protect
	(init_callbacks (ncb:num-callbacks) desc)
      (ncb:free-callbacks desc))))



