;;;
;;; RAHD: Real Algebra in High Dimensions v0.5
;;; A feasible decision method for the existential theory of real closed fields.
;;;
;;; ** Top level system compiler/loader **
;;;
;;; Written by Grant Olney Passmore
;;; Ph.D. Student, University of Edinburgh
;;; Visiting Fellow, SRI International
;;; Contact: g.passmore@ed.ac.uk, http://homepages.inf.ed.ac.uk/s0793114/
;;; 
;;; This file: began on         22-Sept-2008,
;;;            last updated on  23-Nov-2009.
;;;

;;;
;;; To Compile/Load:
;;;
;;;  :cload rahd
;;;  (rahd-reboot)
;;;

;;;
;;; To get an idea of how the system works, begin with PROVER.LISP.
;;;

;;;
;;; Create the RAHD package and make it our current home.
;;;

(defpackage :rahd (:use :common-lisp #+allegro :excl))
(in-package :rahd)

(export '(g go! rahd-reset-state extract-non-refuted-cases
	    canon-tms contra-eqs demod-lin demod-num fert-tsos
	    gen-ex-cad int-dom-zpb open-ex-inf-cad open-frag-ex-inf-cad
	    rcr-ineqs rcr-svars residue-class-ring-ineqs simp-arith
	    simp-gls simp-real-null simp-tvs simp-zrhs triv-ideals
	    univ-sturm-ineqs))

;;;
;;; Current version of RAHD.
;;;

(defparameter *rahd-version* "v0.5")

;;;
;;; Declare our global compiler optimization setting.
;;;

(declaim (OPTIMIZE (SAFETY 0) (SPACE 1) (SPEED 3) (DEBUG 0)))

;;;
;;; Clozure Common Lisp requires us to enable the pretty printer.
;;;

#+ccl (setf *print-pretty* t)

;;;
;;; Declare the work path for external tools (add trailing `/').
;;;

; (defparameter *rahd-work-path* "/.automount/gr1/export/u5/homes/passmore/Code/NLA-Procedure0/")

(defparameter *rahd-work-path* "") 

;;;
;;; COMPILE-FILE-AND-LOAD: Given a list of filename strings, compile
;;;  and load them.  (We assume the trailing ".lisp" is omitted.)
;;;

(defun compile-file-and-load (&rest fnames)
  (mapcar #'(lambda (fname) 
	      (let ((fname-full (format nil "~D.lisp" fname)))
		(compile-file fname-full #+allegro :load-after-compile #+ccl :load #+ccl t #+ccl :verbose #+ccl t)
		#+sbcl (load fname)
		(format t "~%[RAHD-REBOOT]: ~D compiled and loaded successfully." fname-full)))
	      fnames))

;;;
;;; RAHD-REBOOT: Compile and reload all files in our system.
;;;

(defun rahd-reboot (&key hands-off-state)
  (compile-file-and-load 
   "polyalg"
   "polyeval"
   "polyconv"
   "sturm"
   "sturmineq"
   "strings"
   "ineqfert"
   "canonizer"
   "ideals"
   "opencad"
   "cocoa"
   "cases"
   "realnull"
   "cauchyeval"
   "intgrldom"
   "demodlin"
   "demodnl"
   "plinsolver"
   "intsplit"
   "interval"
   "intvlcp"
   "gbrnull"
   "cnf"
   "division"
   "quicksat"
   "prover"
   "abbrevs"
   "rahd"
   "regression"
   "debug"
   "prfanal")
  (if (not hands-off-state) (rahd-reset-state))
  (format t "~%[RAHD-REBOOT]: RAHD ~D successfully rebooted." *rahd-version*)
  t)

;;;
;;; RAHD-BUILD-STAND-ALONE: Build a stand-alone version of RAHD.
;;;
;;; IMG-NAME: A string without a leading ".DXL" -- We will then generate the
;;;  following files:
;;;
;;;   <img-name>.dxl  -- the RAHD Allegro image,
;;;   <img-name>.exec -- the RAHD bash file that will invoke ACL with <img-name>.dxl.
;;;
;;; * Note: This functionality is currently only available for RAHD images compiled
;;;    with Allegro Common Lisp.
;;;

#+allegro 
(defun rahd-build-stand-alone (&optional build-name)
  (when (not (member ':ALLEGRO *features*))
    (break "RAHD-BUILD-STAND-ALONE can only be used on Allegro Common Lisp.  Sorry."))
  (when (or (search ".dxl" build-name) 
	    (and build-name (not (stringp build-name))))
    (break "The NAME of the build should be a string that does not contain the substring \".DXL\"."))
  (let ((build-name (or build-name "saved_rahd")))
    (fmt 0 " ~%~% >> [RAHD-BUILD-STAND-ALONE]: Building stand-alone Allegro (alisp) -based RAHD executable.~%")
    (fmt 0 " ~% Build settings: ~%          Build name: ~A~%" build-name)
    (fmt 0 "          Image name: ~A.dxl~%" build-name)
    (fmt 0 "          Executable: ~A.exec~%" build-name)
    (fmt 0 " ~% Build status: ~%          Dumping image ...............")
    (dumplisp :name (format nil "~A.dxl" build-name))
    (fmt 0 "..... DONE.~%")
    (fmt 0 "          Dumping executable ..........")
    (with-open-file (exec-out (format nil "~A.exec" build-name) :direction :output :if-exists :supersede)
		    (format exec-out "#!/bin/bash~%export PATH=$PATH:./~%alisp -I ./~A.dxl -e \"(in-package :rahd)\""
			    build-name))
    (fmt 0 "..... DONE.~%")
    (fmt 0 "          Marking executable +x .......")
    (excl:run-shell-command (format nil "chmod +x ~A.exec" build-name))
    (fmt 0 "..... DONE.~%~% >> [RAHD-BUILD-STAND-ALONE]: Process complete.~%"))
  t)

#+allegro 
(defun rahd-save-session (session-name)
  (fmt 0 "~%~% >> [RAHD-SAVE-SESSION]: Building stand-alone RAHD executable image with a snap-shot of current session.")
  (rahd-build-stand-alone session-name)
  (fmt 0 "~% >> [RAHD-SAVE-SESSION]: Current session saved and available as ~A.exec.~%~%" session-name)
 t)

#+ccl
(defun make-ccl-binary (name)
  (rahd-reboot)
  (ccl:save-application name :toplevel-function #'cl-check :prepend-kernel t))

#+sbcl
(defun make-sbcl-binary (name)
  (rahd-reboot)
  (sb-ext:save-lisp-and-die name :toplevel #'cl-check :executable t))

(defun quit ()
  #+sbcl (sb-ext:quit))
