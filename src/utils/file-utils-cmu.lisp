;;   Copyright (C) 2004 Cees-Bart Breunesse and Joseph Kiniry, SoS Group,
;;                      Computing Sciences Department, University of Nijmegen

(in-package :PVS)

(export '(file-exists-p directory-p read-permission? write-permission?
			file-write-time get-file-info))


(defun file-exists-p (file)
  (UNIX:unix-access (remove-backslashes (namestring (merge-pathnames file)))
		    UNIX:f_ok)))

(defun remove-backslashes (string)
  (lisp::remove-backslashes string 0 (length string)))

(defun directory-p (dir)
  (let* ((filestring (namestring (merge-pathnames dir)))
	 (stat (nth-value 3 (UNIX:unix-stat filestring))))
    (when (and stat
	       (= (logand UNIX:S-IFMT stat) UNIX:S-IFDIR))
      ;; Needs to end with a slash!!!
      (pathname (if (char= (char filestring (1- (length filestring))) #\/)
		    filestring
		    (concatenate 'string filestring "/"))))))

(defun read-permission? (file)
  (values (UNIX:unix-access (namestring (merge-pathnames file)) UNIX:r_ok)))

(defun write-permission? (file)
  (values (UNIX:unix-access (namestring (merge-pathnames file)) UNIX:w_ok)))

(defconstant u1970 (encode-universal-time 0 0 0 1 1 1970 0))

(defun file-write-time (file)
  (let ((acc (multiple-value-list
	      (UNIX:unix-stat (namestring (merge-pathnames file))))))
    (if (nth 0 acc)
	(+ u1970 (nth 10 acc))
	0)))

(defun get-file-info (file)
  (let ((acc (multiple-value-list (UNIX:unix-stat (namestring (merge-pathnames file))))))
    (if (nth 0 acc)
	(list (nth 1 acc) (nth 2 acc))
	nil)))
