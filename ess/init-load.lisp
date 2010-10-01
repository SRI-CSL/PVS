;; This loads some files on startup.

(in-package :cl-user)

(defvar *lisp-initialized* nil
  "Have we loaded the standard lisp environment?  Useful for disksaving.")

;;; Bug work around.  See also below.
#+(and lucid lcl3.0)
(unless (find-package :constr)
  #-sbcl (make-package :constr)
  #+sbcl (make-package :constr :use '(:common-lisp)))

(unless *lisp-initialized*
  ;;; Another bug workaround, this for LUCID 2.1.
  #+(and lucid (not lcl3.0))
  (progn
    (unless (find-package :tools)
      #-sbcl (make-package :tools)
      #+sbcl (make-package :tools :use '(:common-lisp)))
    (import '(system::cd) :tools)
    (unless (find-package :ergolisp)
      #-sbcl (make-package :ergolisp)
      #+sbcl (make-package :ergolisp :use '(:common-lisp)))
    (import '(system::memq) :ergolisp))

  #+(and lucid lcl3.0)
  (progn
    ;; in order to avoid conflicts with 2.1
    ;; (setq lcl:*load-binary-pathname-types* '("3bin")) ; default is '("lbin")
    ;; Some reasonable settings for compiler.  Default does not
    ;; tail-merge (speed = 2)!!!
    (proclaim '(optimize (speed 3) (safety 1) (space 1)))
    ;; These are not exported from SYSTEM: in 3.0
    ;; Next form is a bug-workaround for Lucid 3.0.  See above.
    (setq constr::*proclaim-constrs-inline* t)
    )
;;#+allegro (unintern 'excl::retry :excl) ; take out when retry is moved to el.
  #+allegro
  (progn
    (unless (find-package :ergolisp)
      #-sbcl (make-package :ergolisp)
      #+sbcl (make-package :ergolisp :use '(:common-lisp)))
    (import '(excl::memq) :ergolisp))
  #+cmu
  (progn
    (unless (find-package :ergolisp)
      #-sbcl (make-package :ergolisp)
      #+sbcl (make-package :ergolisp :use '(:common-lisp)))
    (import '(extensions::memq) :ergolisp)
    (export '(extensions::memq) :ergolisp)
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
