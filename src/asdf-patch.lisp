;;; ASDF is the Common Lisp package system.
;;; Unfortunately, it embeds absolute pathnames into any built images.

;;; These then lead to errors when asdf or quicklisp are used by anyone
;;; other than "owre".  The following fixes that.  This is loaded in
;;; pvs-init, as it substitutes the new user for "owre", and it can't know
;;; that at build time.

(in-package :asdf)

(defmethod perform :around ((o operation) (c component))
  (dolist (slot '(relative-pathname absolute-pathname))
    (when (slot-boundp c slot)
      (let ((spath (slot-value c slot)))
	(when (pathnamep spath)
	  (handler-case (ensure-directories-exist spath)
	    (file-error ()
	      (let ((npath (fix-user-name-in-directory spath "owre")))
		(when npath
		  (setf (slot-value c slot) npath)))))))))
  (call-next-method))

(defun fix-output-translations ()
  (when (boundp '*output-translations*)
    (labels ((fix-ot (list)
	       (when list
		 (cond ((pathnamep (car list))
			(let ((fpath (fix-user-name-in-directory (car list))))
			  (when fpath (setf (car list) fpath))))
		       ((consp (car list))
			(fix-ot (car list))))
		 (cond ((pathnamep (cdr list))
			(let ((fpath (fix-user-name-in-directory (cdr list))))
			  (when fpath (setf (cdr list) fpath))))
		       ((consp (cdr list))
			(fix-ot (cdr list)))))))
      (fix-ot *output-translations*))))

(defun fix-user-name-in-directory (path &optional (uname "owre"))
  (let ((pdirs (pathname-directory path)))
    (when (member uname pdirs :test #'string=)
      (let* ((udirs (pathname-directory (user-homedir-pathname)))
	     (sdirs (substitute (car (last udirs)) uname pdirs :test #'string=)))
	(make-pathname :directory sdirs :defaults path)))))

(eval-when (:execute :load-toplevel)
  (fix-output-translations)
  (when (and (boundp '*user-cache*)
	     *user-cache*)
    (setq *user-cache*
	  (fix-user-name-in-directory *user-cache*))))
