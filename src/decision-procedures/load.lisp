(in-package dp)

(defvar *dp-directory* #-linux86 "/project/pvs/src/prover/dc-prototypes/"
                       #+linux86 "/home/pvs/src/prover/dc-prototypes/")

(defmethod ppr (obj)
  (write obj :pretty t :level nil :length nil)
  nil)

(defmethod ppr ((obj hash-table))
  (maphash #'(lambda (x y) (format t "~%~a - ~a" x y))
	   obj))

#+allegro (defun string-append (&rest args)
	    (apply #'clos::string-append args))

;;loads all the new (July 1996) decision procedure code.
(defun load-dp ()
  (load (string-append *dp-directory* "nelson-oppen")) ;;term ADTs, cong. closure
  (load (string-append *dp-directory* "arithmetic")) ;;Basic arithmetic
  (load (string-append *dp-directory* "no-cong"))
                                 ;;interface between N-O cong.closure & rest
  (load (string-append *dp-directory* "no-transclosure"))
                                ;;N-O arithmetic decision procedure
                                ;;based on Gaussian elim (equalities)
                                ;; and Fourier-Motzkin (inequalities)
  (load (string-append *dp-directory* "no-interp"))
                           ;;top-level N-O combination of DPs
  (load (string-append *dp-directory* "shostak"))
                          ;;Top-level Shostak (CC, canon, etc.)
  (load (string-append *dp-directory* "shostak-interp")))
                        ;;solve and sigma for Shostak (incl. arith ineq solver).

(defun load-shostak ()
  ;(load "/project/pvs/src/utils/hash")
  (load (string-append *dp-directory* "types"))
  (load (string-append *dp-directory* "node-structures"))
  (load (string-append *dp-directory* "integer"))
  (load (string-append *dp-directory* "arithmetic"))
  (load (string-append *dp-directory* "shostak"))
  (load (string-append *dp-directory* "shostak-interp"))
  (load (string-append *dp-directory* "arith-solve"))
  (load (string-append *dp-directory* "polylib/" "foreign"))
  (load (string-append *dp-directory* "polyhedron")))

(defun compile-shostak ()
  ;(load "/project/pvs/src/utils/hash")
  (compile-file (string-append *dp-directory* "types"))
  (load (string-append *dp-directory* "types"))
  (compile-file (string-append *dp-directory* "node-structures"))
  (load (string-append *dp-directory* "node-structures"))
  (compile-file (string-append *dp-directory* "integer"))
  (load (string-append *dp-directory* "integer"))
  (compile-file (string-append *dp-directory* "arithmetic"))
  (load (string-append *dp-directory* "arithmetic"))
  (compile-file (string-append *dp-directory* "shostak"))
  (load (string-append *dp-directory* "shostak"))
  (compile-file (string-append *dp-directory* "shostak-interp"))
  (load (string-append *dp-directory* "shostak-interp"))
  (compile-file (string-append *dp-directory* "arith-solve"))
  (load (string-append *dp-directory* "arith-solve"))
  (compile-file (string-append *dp-directory* "polylib/" "foreign"))
  (load (string-append *dp-directory* "polylib/" "foreign"))
  (compile-file (string-append *dp-directory* "polyhedron"))
  (load (string-append *dp-directory* "polyhedron")))

(defun proclaim-speed ()
  (proclaim '(optimize (compilation-speed 0) (space 1)
		       (safety 1) (speed 3)
		       #+allegro (debug 0))))

(defun proclaim-dev ()
  (proclaim '(optimize (compilation-speed 0) (space 1)
		       (safety 1) (speed 1)
		       #+allegro (debug 2))))
  
