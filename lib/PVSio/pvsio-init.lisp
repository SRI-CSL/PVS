(in-package :pvs)

(let ((file (environment-variable "PVSIOFILE"))
      (time (read-from-string (environment-variable "PVSIOTIME")))
      (theory (environment-variable "PVSIOTHEORY"))
      (packlist (read-from-string (environment-variable "PVSIOPACK")))
      (verb (read-from-string (environment-variable "PVSIOVERB")))
      (tccs (read-from-string (environment-variable "PVSIOTCCS")))
      (main (read-from-string (environment-variable "PVSIOMAIN")))
      (*pvsio-version* (environment-variable "PVSIOVERSION")))
  (unless (probe-file (format nil "~a.pvs" file))
    (format t "File not found: ~a.pvs~%" file)
    (bye 1))
  (when time (setq *pvs-eval-do-timing* t))
  (multiple-value-bind 
   (val err)
   (progn 
     (with-open-file 
      (*standard-output*
       (format nil "~a~a.log" (directory-namestring file) theory)
       :direction :output
       :if-does-not-exist :create
       :if-exists :supersede)
      (pvs-init)
      (change-context (directory-namestring file))
      (dolist (pack packlist) (load-prelude-library pack))
      (load-pvs-attachments)
      (when (null verb)
	(format t "~%Typechecking")
	(unwind-protect
	    (typecheck-file (file-namestring file) nil nil nil t)
	  (fresh-line)
	  (finish-output))))
     (when verb
       (typecheck-file (file-namestring file) nil nil nil t))
     (evaluation-mode-pvsio theory main tccs t (null main))
     t)
   (when err (format t "~%~a (~a.pvs). See ~a~a.log~%"
		     err file (directory-namestring file) theory)))
  (fresh-line)
  (bye 0))

