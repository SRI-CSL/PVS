;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-pvs-parser.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Dec 29 02:55:05 1998
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Dec 29 03:28:07 1998
;; Update Count    : 1
;; Status          : Unknown, Use with caution!
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)

(eval-when (eval load)
  (unless (boundp '*pvs-path*)
    (defvar *pvs-path* (sys:getenv "PVSPATH"))))

#+allegro
(eval-when (eval load)
  (setq *ignore-package-name-case* t))

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

(load (format nil "~a/ess/dist-ess.lisp" (or *pvs-path* ".")))
(generate-ess ergolisp sb)

(compile-file-if-needed (format nil "~a/src/ergo-gen-fixes" (or *pvs-path* ".")))
(load (format nil "~a/src/ergo-gen-fixes" (or *pvs-path* ".")))
;;(compile-file-if-needed "src/ergo-runtime-fixes")
;;(load "src/ergo-runtime-fixes")
(let ((sbmake (intern (string :sb-make) :sb)))
  (funcall sbmake
	   :language "pvs"
	   :working-dir (format nil "~a/src/" (or *pvs-path* "."))
	   :unparser? nil))
(excl:exit)
