;; This loads some files on startup.

(in-package "USER")

(defvar *lisp-initialized* nil
  "Have we loaded the standard lisp environment?  Useful for disksaving.")

;;; Bug work around.  See also below.
#+(and lucid lcl3.0)
(unless (find-package :constr) (make-package :constr))

(unless *lisp-initialized*
  ;;; Another bug workaround, this for LUCID 2.1.
  #+(and lucid (not lcl3.0))
  (progn
    (unless (find-package :tools) (make-package :tools))
    (import '(system::cd) :tools)
    (unless (find-package :ergolisp) (make-package :ergolisp))
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
;;  #+allegro (unintern 'excl::retry :excl) ; take out when retry is moved to el.
  (load "/homes/EHDM/systems/ess/sys/ergolisp/rel/ergolisp.lisp")
  (load "/homes/EHDM/systems/ess/sys/ergolisp/rel/ergolisp-exports.lisp")
  (load "/homes/EHDM/systems/ess/sys/ergolisp/rel/ergo-system.lisp")
  (load "/homes/EHDM/systems/ess/sys/tools/rel/retry.lisp")
  (load "/homes/EHDM/systems/ess/sys/tools/rel/box-system.lisp")
  (load "/homes/EHDM/systems/ess/sys/tools/rel/box")
  (load "/homes/EHDM/systems/ess/box-defs")
  (use-package :tools)

  )

(when *lisp-initialized*
    (boxload tools))

(setq *lisp-initialized* t)

(use-package :tools)
(use-package :ergolisp)
(use-package :ergolisp :tools)

;; Workaround in Allegro / Dec3100 compiler bug before version 3.1
#+(and allegro dec3100 allegro-v3.0)
(push2 :source t (tools::efile-local-opts '#>e"lp-special.lisp"))
