(in-package :pvs)
(require :foreign)
(export '(file-exists-p directory-p read-permission? write-permission?
			file-write-time get-file-info))

(ff:def-foreign-call fileutils___file_exists_p
    ((filename (* :char) simple-string))
  :returning :int
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :call-direct t)

(ff:def-foreign-call fileutils___directory_p
    ((filename (* :char) simple-string))
  :returning :int
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :call-direct t)

(ff:def-foreign-call fileutils___read_permission_p
    ((filename (* :char) simple-string))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :call-direct t)

(ff:def-foreign-call fileutils___write_permission_p
    ((filename (* :char) simple-string))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :call-direct t)

(ff:def-foreign-call fileutils___file_write_time
    ((filename (* :char) simple-string))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :returning :int
  :call-direct t)

(ff:def-foreign-call fileutils___getfileinfo
    ((filename (* :char) simple-string)
     (stat (* :int) (simple-array fixnum)))
  #+(version>= 6) :strings-convert #+(version>= 6) nil
  :call-direct t)

;;;

(defmacro expanded-tilde-namestring (filename)
  `(ignore-errors (excl::tilde-expand-unix-namestring (namestring ,filename))))

(defun file-exists-p (filename)
  (let ((exp-file (expanded-tilde-namestring filename)))
    (and exp-file
	 (excl:with-native-string
	  (cstr exp-file)
	  (zerop (fileutils___file_exists_p cstr))))))

(defun directory-p (filename)
  (when filename
    (let ((filestring (expanded-tilde-namestring filename)))
      (excl:with-native-string
       (cstr filestring)
       (unless (zerop (fileutils___directory_p cstr))
	 (pathname (if (char= (char filestring (1- (length filestring))) #\/)
		       filestring
		       (concatenate 'string filestring "/"))))))))

(defun read-permission? (filename)
  (excl:with-native-string
   (cstr (expanded-tilde-namestring filename))
   (let ((errno (fileutils___read_permission_p cstr)))
     (or (zerop errno)
	 (values nil (ff:char*-to-string errno))))))

(defun write-permission? (filename)
  (excl:with-native-string
   (cstr (expanded-tilde-namestring filename))
   (let ((errno (fileutils___write_permission_p cstr)))
     (or (zerop errno)
	 (values nil (ff:char*-to-string errno))))))

(defconstant u1970 (encode-universal-time 0 0 0 1 1 1970 0))

(defun file-write-time (filename)
  (excl:with-native-string
   (cstr (expanded-tilde-namestring filename))
   (let ((date (fileutils___file_write_time cstr)))
     (unless (zerop date)
       (+ date u1970)))))

;;; #(dev inode mtime isdir mode)
(let ((fstat-array
       (make-array 2 :initial-element 0 :element-type 'fixnum)))
  (defun get-file-info (filename)
    (excl:with-native-string
     (cstr (expanded-tilde-namestring filename))
     (when (zerop (fileutils___getfileinfo cstr fstat-array))
       (list (aref fstat-array 0)
	     (aref fstat-array 1))))))
