(let ((system:*load-search-list*
       '(:first
	 (:newest-do-compile
	  #.(make-pathname :directory "/homes/owre/src/cl-glade-0.23"
			   :type "fasl"))
	  #.(make-pathname :directory "/homes/owre/src/cl-glade-0.23"
			   :type "lisp"))))
  (load "gtk")
  (load "gtk-gdk-keysyms")
  (load "gtk-cdbind-acl")
  (load "gtk-bindings")
  (load "gtk-allegro")
  (load "glade")
  (load "glade-runtime")
  (load "glade-utilities")
  (load "read-xml")
  (load "glade")
  (load "glade-transform")
  (load "glade-spec"))

(defun on_foo1_activate (&key signal widget info)
  (declare (ignore signal widget info))
  (format t "Foo clicked")
  (break))

(defun on_quit1_activate (&key signal widget info)
  (declare (ignore signal widget info))
  (format t "Quit clicked")
  (break))

(defun gtk_true (&key signal widget info)
  (declare (ignore signal widget info))
  (format t "GTK thing clicked")
  (break))

(glade-transform:defun-make-widget make-window1
    "/homes/rushby/Projects/project1/project1.glade"
  "window1")

(defvar *glade-thing*)

(defun test-glade-thing ()
  (unwind-protect
      (progn
	(setf *glade-thing* (glade:show (make-window1)))
	(glade:main))
    (when *glade-thing*
      (glade:destroy *glade-thing*))
    (glade:flush-events)))
