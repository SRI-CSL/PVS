;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-methods.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Dec 31 19:16:33 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Dec 31 21:13:07 1998
;; Update Count    : 11
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "USER")

#+allegro
(eval-when (eval load)
  (setq *record-source-file-info* t
	*load-source-file-info* t
	*record-xref-info* t
	*ignore-package-name-case* t))

(eval-when (eval load)
  (defvar *pvs-binary-type*
    #+(and allegro sparc) "fasl"	; Sun4
    #+(and allegro rios) "rfasl"	; PowerPC/RS6000
    #+(and allegro hpux) "hfasl"	; HP 9000
    #+(and allegro x86) "lfasl"         ; Intel x86
    #+(and lucid lcl4.1 sparc) "sbin"	; Sun4 new Lucid
    #+(and lucid (not lcl4.1) sparc) "obin" ; Sun4 old Lucid
    #+(and lucid rios) "rbin"		; PowerPC/RS6000
    #+(and lucid mips) "mbin"		; DEC
    ;;; These are experimental
    #+gcl "o"
    #+cmu "sparcf"
    #+harlequin-common-lisp "wfasl"
    ))

#+allegro
(eval-when (eval load)
  (setq excl:*fasl-default-type* *pvs-binary-type*)
  (setq system:*load-search-list*
	(list #p"" (make-pathname :type *pvs-binary-type*)
	      #p(:type "cl") #p(:type "lisp"))))

(defpackage pvs (:use #+lucid "LUCID-COMMON-LISP" "LISP"
		      #-gcl "CLOS" #+gcl "PCL"))

(setq *cltl1-in-package-compatibility-p* t)

(in-package "PVS")
(import '(user::*pvs-path*))
(let ((excl:*enable-package-locked-errors* nil))
  (load "src/defcl.lisp")
  (load "src/store-object.lisp")
  (load "src/classes-expr.lisp")
  (load "src/classes-decl.lisp")
  (load "src/prover/estructures.lisp"))

(write-deferred-methods-to-file t)
(excl:exit)
