;; This loads some files on startup.

(in-package :cl-user)

(defvar *lisp-initialized* nil
  "Have we loaded the standard lisp environment?  Useful for disksaving.")

(unless *lisp-initialized*
  ;;; Another bug workaround, this for LUCID 2.1.
  #+allegro
  (progn
    (unless (find-package :ergolisp)
      #-sbcl (make-package :ergolisp)
      #+sbcl (make-package :ergolisp :use '(:common-lisp)))
    )
  #+cmu
  (progn
    (unless (find-package :ergolisp)
      #-sbcl (make-package :ergolisp)
      #+sbcl (make-package :ergolisp :use '(:common-lisp)))
    )
  #+sbcl
  (unless (find-package :ergolisp) (make-package :ergolisp :use '(:common-lisp)))
  (load (format nil "~a/sys/ergolisp/rel/ergolisp.lisp" *ess-path*))
  (load (format nil "~a/sys/ergolisp/rel/ergolisp-exports.lisp" *ess-path*))
  (load (format nil "~a/sys/ergolisp/rel/ergo-system.lisp" *ess-path*))
  (load (format nil "~a/sys/tools/rel/retry.lisp" *ess-path*))
  #+(or clisp cmu sbcl)
  (unless (find-package :tools)
    #-sbcl (make-package :tools)
    #+sbcl (make-package :tools :use '(:common-lisp :ergolisp)))
  (load (format nil "~a/sys/tools/rel/box-system.lisp" *ess-path*))
  (load (format nil "~a/sys/tools/rel/box" *ess-path*))
  (load (format nil "~a/box-defs" *ess-path*))
  (use-package :tools)

  )

(when *lisp-initialized*
    (boxload tools))

(setq *lisp-initialized* t)

(use-package :tools)
(use-package :ergolisp)
(use-package :ergolisp :tools)

;; ;; Workaround in Allegro / Dec3100 compiler bug before version 3.1
;; #+(and allegro dec3100 allegro-v3.0)
;; (push2 :source t (tools::efile-local-opts '#>e"lp-special.lisp"))
