;;;
;;; RAHD: Real Algebra in High Dimensions v0.0
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
;;;            last updated on  14-Nov-2008.
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

(defpackage :RAHD (:use :common-lisp #+allegro :excl))
(in-package RAHD)

(export '(rahd-reset-state g go! extract-non-refuted-cases))

;;;
;;; Current version of RAHD.
;;;

(defparameter *rahd-version* "v0.0")

;;;
;;; Declare our global compiler optimization setting.
;;;

(declaim (optimize (safety 0) (space 1) (speed 3) (debug 0)))

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
		(compile-file fname-full :load-after-compile t :verbose t)
		(format t "~%[RAHD-REBOOT]: ~D compiled and loaded successfully." fname-full)))
	      fnames))

;;;
;;; RAHD-REBOOT: Compile and reload all files in our system.
;;;

(defun rahd-reboot (&optional &key hands-off-state)
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
   "integrald"
   "demodlin"
   "cnf"
   "division"
   "prover"
   "rahd"
   "regression"
   "debug")
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
		    (format exec-out "#!/bin/bash~%export PATH=$PATH:./~%alisp -I /afs/inf.ed.ac.uk/user/s07/s0793114/Code/RAHD/~A.dxl -e \"(in-package RAHD)\""
			    build-name))
    (fmt 0 "..... DONE.~%")
    (fmt 0 "          Marking executable +x .......")
    (#+allegro excl:run-shell-command #+cmu extensions:run-program
	       (format nil "chmod +x ~A.exec" build-name))
    (fmt 0 "..... DONE.~%~% >> [RAHD-BUILD-STAND-ALONE]: Process complete.~%"))
  t)

(defun rahd-save-session (session-name)
  (fmt 0 "~%~% >> [RAHD-SAVE-SESSION]: Building stand-alone RAHD executable image with a snap-shot of current session.")
  (rahd-build-stand-alone session-name)
  (fmt 0 "~% >> [RAHD-SAVE-SESSION]: Current session saved and available as ~A.exec.~%~%" session-name)
 t)
