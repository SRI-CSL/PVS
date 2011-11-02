;;============================================
;; TOP-LEVEL READ-EVAL LOOP FOR NLYICES
;; - load libraries
;; - then call the top-level yices function
;;============================================

#-mk-defsystem ; This is set in PVS/pvs.system (by defsystem)
(eval-when (compile eval load)
  (require :foreign)
  (require :defftype))

;;--------------------------------------------------------------------------
;; *nlyices-path* should be set to the 'nlyices' top directory.
;; All files are loaded from <nlyices-path>/lisp.
;;
;; BD: try to guess a reasonable default for *nlyices-path*
;; This was (defparameter *nlyices-path* #p"/homes/owre/nlyices")
;;
;; 1) read environment variable "NLYICES_DIR". If it is set use that
;;    as the defaylt *nlyices-path* (as a string).
;; 2) if NLYICES_DIR is not set, use the current directory
;;    but remove lisp if the current directory is of the form /../lisp
;;--------------------------------------------------------------------------

#-mk-defsystem
(defun filter-current-dir ()
   (let* ((l1 (reverse (pathname-directory (excl:current-directory))))
 	 (l2 (if (string-equal (car l1) "lisp") (cdr l1) l1)))
     (if (cdr l2)
 	(make-pathname :directory (reverse (cdr l2)) :name (car l2))
       (make-pathname :directory l2))))

#-mk-defsystem
(defun guess-nlyices-dir ()
  (or (sys:getenv "NLYICES_DIR") (filter-current-dir)))


;;----------------------------------
;; Base nlyices path + load macros
;;----------------------------------

#-mk-defsystem
(defparameter *nlyices-path* (guess-nlyices-dir))

#-mk-defsystem
(defmacro cfile-nl (filenm)
  `(compile-file (format nil "~a/lisp/~a" *nlyices-path* ,filenm) :load-after-compile t))

#-mk-defsystem
(defmacro load-nl (filenm)
  `(load (format nil "~a/lisp/~a" *nlyices-path* ,filenm)))

#-mk-defsystem
(cfile-nl "polyrep-totdeglex")
#-mk-defsystem
(cfile-nl "vts")
#-mk-defsystem
(cfile-nl "decide3_2a")
#-mk-defsystem
(cfile-nl "nlsolver")
#-mk-defsystem
(cfile-nl "named-callbacks")
#-mk-defsystem
(cfile-nl "fcpo")

#+(and (not mk-defsystem) macosx)
(load-nl "libyices.dylib")

#+(and (not mk-defsystem) linux x86-64)
(load-nl "libyices64.so")

#+(and (not mk-defsystem) linux (not x86-64))
(load-nl "libyices32.so")


;;---------------------------------------------------
;; Foreign function for initializing LISP callbacks
;;---------------------------------------------------

#+allegro
(ff:def-foreign-call init_callbacks ((n :int) (d (:array ncb:callback-desc)))  :returning :void)


;;---------------------------
;; Initialize the callbacks
;;---------------------------

(defun nlyices-init ()
  (let ((desc (ncb:get-callbacks)))
    (unwind-protect
	(init_callbacks (ncb:num-callbacks) desc)
      (ncb:free-callbacks desc))))




;;------------------------------------------------
;; Foreign functions declared in yices_reval.h:
;;
;; extern int yices_main(int argc, char *argv[])
;; extern int run_yices(void);
;;------------------------------------------------

#+allegro
(ff:def-foreign-call yices_main ((argc :int) (argv (:array (* char)))) :returning (:int fixnum))

#+allegro
(ff:def-foreign-call run_yices (:void) :returning (:int fixnum))


;;--------------------------------------------------------------------------------
;; Convert a list of strings to a foreign object (array of (*char)) 
;; that can be passed to yices_main
;; - args = list of strings to convert
;; - the returned object is allocated in the lisp heap (:foreign allocation type)
;;--------------------------------------------------------------------------------

#+allegro
(defun strings-to-argv (args)
  (let* ((n (length args))
	 (fobj (ff:allocate-fobject `(:array (* char) ,n))))
    (do ((i 0 (+ i 1))
	 (l args (cdr l)))
	((= i n) fobj)
	(setf (ff:fslot-value fobj i) (ff::string-to-native (car l))))))


;;------------------------------------------------------
;; Cleanup the object constructed by stringc2args
;; - we need this to prevent memory leaks since
;;   string-to-native allocate memory in non-gc'd area
;;------------------------------------------------------

#+allegro
(defun cleanup-argv (fobj n)
  (dotimes (i n) (excl:aclfree (ff:fslot-value fobj i))))


;;--------------------------------------------------
;; Call yices_main with the given string arguments
;;--------------------------------------------------

(defun call-yices (&rest args)
  (let ((fobj (strings-to-argv (cons "nlyices" args))))
    (unwind-protect 
	(yices_main (1+ (length args)) fobj)
      (cleanup-argv fobj (1+ (length args))))))



;;------------------------------------------------------------------
;; Call yices_main with a single argument (normally an input file)
;; and with (check) results copied into a temporary file
;; - return a string that indicates the status produced by yices
;;
;; - 16 to 22 are error codes defined in yices_exit_codes.h
;; - code 0 means no error. Then we can open the temporary file to
;;   check whether the result is sat, unsat, or unknown.
;;
;; NOTE: this needs to be fixed. Right now, Yices calls 'exit' on
;; error and this kills the process (including the LISP caller).
;;------------------------------------------------------------------

#+allegro
(defun check-with-yices (filename &optional with-egraph)
  (let* ((tmp (system:make-temp-file-name))
	 (args (if with-egraph 
		   (list "nlyices" "--egraph" "-o" tmp  filename)
		   (list "nlyices" "-o" tmp  filename)))
	 (fobj (strings-to-argv args))
	 (nobj (length args)))
    (unwind-protect 
	(let ((code (yices_main nobj fobj)))
	  (case code
		(16 "yices-out-of-memory")
		(17 "yices-syntax-error")
		(18 "yices-file-not-found")
		(19 "yices-incorrect-usage")
		(20 "yices-error")
		(21 "yices-interrupted")
		(22 "yices-internal-error")
		(0 (get-yices-answer tmp))
		(t "invalid-exit-code-from-yices")))
      (cleanup-argv fobj nobj))))




;; read the last line of file tmp
;; and convert that to 'sat, 'unsat, or 'unknown
(defun get-yices-answer (filename)
  (with-open-file
   (answer-file filename :direction :input :if-does-not-exist nil)
   (if answer-file 
       (nth-value 0 (read-line answer-file nil "yices-output-file-empty"))
     "yices-output-file-not-found")))
  
		  




;;------------------------------------------------------
;; Collect all the example files and run yices on them
;;------------------------------------------------------

;; run yices on file: print file name, answer from yices, and run-time
(defun test (file)
  (format t "~%TEST: ~A~%" (file-namestring file))
  (let ((real-t0 (get-internal-real-time))
	(run-t0 (get-internal-run-time))
	(answer (check-with-yices (namestring file))))
    (let ((real-t1 (get-internal-real-time))
	  (run-t1 (get-internal-run-time)))
      (format t "END TEST~%Answer: ~A, real-time = ~A, run-time = ~A~%~%" answer (- real-t1 real-t0) (- run-t1 run-t0)))))



(defun sort-files (l)
  (sort l #'string-lessp :key #'file-namestring))

(defun all-examples () 
  (append (sort-files (directory "../examples/*.ys"))
	  (sort-files (directory "../examples/radh-examples/*.ys"))))
  

(defun all-tests ()
  (mapc #'test (all-examples))
  nil)


;;-------------------------------------------------------------
;; Collect all the example files and benchmark yice+rahd(s) on them
;; for all rahd strageties s
;;-------------------------------------------------------------
(defstruct BenchmarkResult file answer strategy realTime runTime)

(defun benchmark (file strategy)
  (format t "~%TEST: ~A~%" (file-namestring file))
  (nlsolver:strategize strategy)
  (let ((real-t0 (get-internal-real-time))
	(run-t0 (get-internal-run-time))
	(answer (check-with-yices (namestring file))))
    (let ((real-t1 (get-internal-real-time))
	  (run-t1 (get-internal-run-time)))
      (let ((realDelta (- real-t1 real-t0))
	    (runDelta (- run-t1 run-t0)))
	(let ((result (make-BenchmarkResult :file file :answer answer :strategy strategy :realTime realDelta :runTime runDelta)))
	  (format t "END TEST~%Answer:~% ~A~%" result)
	  result)))))

(defun benchmarkFiles (files strategy)
  (mapcar (lambda (x) (benchmark x strategy)) files))

(defun benchmarkProduct (files strategies)
  (reduce #'append (mapcar (lambda (y) (benchmarkFiles files y)) strategies) ))

;;(defvar small (list "../examples/john1.ys" "../examples/john2.ys"))
(defun black-list ()
  (list "examples/angle.ys"
	"examples/nl3.ys"
	"examples/nl3a.ys"
	"examples/pedos_inequality.ys"
	;;"sdp_ternary4.ys" ;; No GB?
	"steiner-lehmus-theorem.ys"
	"examples/train.ys" ;; Higher cutoff for projection?
	))

;; (benchmark "../examples/train.ys" "calculemus-0")

(defun passes-black-list (example)
  (notany (lambda (x) (search x (namestring example))) (black-list)))

(defun practical-examples ()
  (remove-if-not 'passes-black-list (all-examples)))

(defun practical-benchmarks ()
  (benchmarkProduct (practical-examples) (list "calculemus-0")))

(defun all-benchmarks ()
  (benchmarkProduct (all-examples) nlsolver::*rahd-strategies*))
  
(defun benchmarkResultToCSV (bm)
  (format t "'~a', ~a, ~a, ~a, ~a ~%"
	  (BenchmarkResult-file bm)
	  (BenchmarkResult-answer bm)
	  (BenchmarkResult-strategy bm)
	  (BenchmarkResult-realTime bm)
	  (BenchmarkResult-runTime bm)))

(defun benchmarkResultListToCSV (bms)
  (format t "file, answer, strategy, realTime, runTime~%")
  (mapcar #'benchmarkResultToCSV bms) nil)

(defun go-benchmark ()
  (benchmarkResultListToCSV (all-benchmarks)))

;;-------------------------------------------------------------
;; Same thing but for examples that require egraph + nlsolver
;;-------------------------------------------------------------

(defun eg-test (file)
  (format t "~%TEST: ~A~%" (file-namestring file))
  (let ((real-t0 (get-internal-real-time))
	(run-t0 (get-internal-run-time))
	(answer (check-with-yices (namestring file) t)))
    (let ((real-t1 (get-internal-real-time))
	  (run-t1 (get-internal-run-time)))
      (format t "END TEST~%Answer: ~A, real-time = ~A, run-time = ~A~%~%" answer (- real-t1 real-t0) (- run-t1 run-t0)))))


(defun all-eg-examples ()
  (sort-files (directory "../examples/QF_UFNRA/cas/*.ys")))


(defun all-eg-tests ()
  (mapc #'eg-test (all-eg-examples))
  nil)



;;-------------------------------
;; Make sure we call this first 
;;-------------------------------

#-mk-defsystem ;; Done in pvs-init
(nlyices-init)

;; get some trace

;; (gb:set-debug-level 3)
