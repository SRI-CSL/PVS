;;; -*- Mode: Lisp -*-

;;; sbcl.lisp --
;;;
;;; This init file was last tested with SBCL 0.6.13 and
;;; SBCL 0.7pre.71

;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: sbcl.lisp,v 1.8 2002/03/26 09:41:04 anisotropy9 Exp $


(in-package :ilisp)

;; ILISP-specifics for SBCL. Since version 0.7 introduced lots of changes,
;; e.g.(bytecode-)interpreter goes away, and lots of other 'renaming'-changes,
;; take care of that, by testing via the 'magic'-macros:
;; THE-SYMBOL-IF-DEFINED, and THE-FUNCTION-IF-DEFINED.
;;
;; MNA: 2001-10-20
;; Some annotations:
;; <1> - interpreter related changes (interpreter missing in sbcl-0.7.x)
;; <2> - byte-compiler related changes (sbcl-0.7.x)
;; <3> - renamings in sbcl-0.7.x., where in general this is accounted for
;;       using THE-SYMBOL-IF-DEFINED and THE-FUNCTION-IF-DEFINED macros.
;;       In general, the "new" symbol comes before the "old" symbol.

;;;% CMU CL does not define defun as a macro
(defun ilisp-compile (form package filename)
  "Compile FORM in PACKAGE recording FILENAME as the source file."
  (ilisp-errors
   (ilisp-eval
    (format nil "(funcall (compile nil '(lambda () ~A)))" form)
    package filename)))

;;;% Stream settings, when running connected to pipes.
;;;
;;; When SBCL is running as a piped process, it still manages to open
;;; /dev/tty to use it for the *terminal-io* stream.  This means that an
;;; error will cause lisp to stop and wait for input from /dev/tty, which is
;;; probably not available and certainly not what you were expecting.
;;;
;;; We want it to use the same input that the user is typing at, ie,
;;; the pipe (stdin).

(defvar *Fix-pipe-streams* T
  "Set to Nil if you want them left alone.  And tell me you don't get stuck.")

(when (and *Fix-pipe-streams*
	   (sb-impl::synonym-stream-p *terminal-io*)
	   (eq (sb-impl::synonym-stream-symbol *terminal-io*)
	       'sb-impl::*tty*))
  (setf *terminal-io* (make-two-way-stream sb-impl::*stdin* sb-impl::*stdout*))
  ;; *query-io* and *debug-io* are synonym streams to this, so this fixes
  ;; everything.
  )

;;; Normally, errors which occur while in the debugger are just ignored, unless
;;; the user issues the "flush" command, which toggles this behavior.

(setq sb-debug:*flush-debug-errors* nil)  ; allow multiple error levels.


;;;%% arglist/source-file utils.

(defun get-correct-fn-object (sym)
  "Deduce how to get the \"right\" function object and return it."
  (let ((fun (or (macro-function sym)
		 (and (fboundp sym) (symbol-function sym)))))
    (cond (fun
            (if (and (= (the-function-if-defined ((#:widetag-of :sb-impl)
                                                  (#:get-type :sb-impl)) fun)
                        ;; <3>
                        #.(the-symbol-if-defined
                           ((#:closure-header-widetag :sb-vm)
                            (#:closure-header-type :sb-vm) :eval-p t)))
                     (not (the-function-if-defined
                           ((#:interpreted-function-p :sb-eval) ()) fun)))
              ;; <3>
              (the-function-if-defined ((#:%closure-fun :sb-impl)
                                        (#:closure-function :sb-impl))
                                       fun)
              ;; else just return the old function-object
              fun))
	  (t
            (error "Unknown function ~a.  Check package." sym)
            nil))))

;;; 2000-04-02: Martin Atzmueller
;;; better (more bulletproof) arglist code adapted from cmulisp.lisp:

(defun extract-function-info-from-name (sym)
  (let ((mf (macro-function sym)))
    (if mf
	(values mf :macro)
	(if (fboundp sym)
	    (values (symbol-function sym) :function)
	    (values nil nil)))))

(defun arglist (symbol package)
  (ilisp-errors
   (let* ((package-name (if (packagep package)
                          (package-name package)
                          package))
	  (x (ilisp-find-symbol symbol package-name)))
     (flet ((massage-arglist (args)
	      (typecase args
		(string (if (or (null args) (string= args "()"))
                          ""
                          args))
		(list (if args
                        (let ((*print-pretty* t)
                              (*print-escape* t)
                              (*print-base* 10)
                              (*print-radix* nil))
                          (format nil "~A" args))
                        "()"))
		(t ""))))

       (multiple-value-bind (func kind)
	   (extract-function-info-from-name x)
	 (if (and func kind)
           (case (the-function-if-defined ((#:widetag-of :sb-impl)
                                           (#:get-type :sb-impl)) func)
             ;; <3>
             ((#.(the-symbol-if-defined ((#:closure-header-widetag :sb-vm)
                                         (#:closure-header-type :sb-vm)
                                         :eval-p t))
                 #.(the-symbol-if-defined ((#:simple-fun-header-widetag :sb-vm)
                                           (#:function-header-type :sb-vm)
                                           :eval-p t))
		 #.(the-symbol-if-defined ((#:closure-fun-header-widetag
                                            :sb-vm)
                                           (#:closure-function-header-type
                                            :sb-vm)
                                           :eval-p t)))
               (massage-arglist
                (the-function-if-defined ((#:%simple-fun-arglist :sb-impl)
                                          (#:%function-arglist :sb-impl))
                                         func)))
             (#.(the-symbol-if-defined
                 ((#:funcallable-instance-header-widetag :sb-vm)
                  (#:funcallable-instance-header-type :sb-vm)
                  :eval-p t))
               (typecase func
                 ;; <2>
                 (#.(the-symbol-if-defined ((#:byte-function :sb-kernel) ()))
                   "Byte compiled function or macro, no arglist available.")
                 (#.(the-symbol-if-defined ((#:byte-closure :sb-kernel) ()))
                   "Byte compiled closure, no arglist available.")
                 ((or generic-function sb-pcl::generic-function)
                   (sb-pcl::generic-function-pretty-arglist func))
                 ;; <1>
                 (#.(the-symbol-if-defined ((#:interpreted-function :sb-eval) ()))
                   (the-function-if-defined
                    ((#:interpreted-function-arglist :sb-eval) ()
                     :function-binding-p t)
                    (massage-arglist (funcall the-function func))))
                 (t (print 99 *trace-output*) "No arglist available.")
                 ))			; typecase
             (t "No arglist available.")) ; case
           "Unknown function - no arglist available." ; For the time
					; being I just
					; return this
					; value. Maybe
					; an error would
					; be better.
           ))))))

;;; source-file symbol package type --
;;; New version provided by Richard Harris <rharris@chestnut.com> with
;;; suggestions by Larry Hunter <hunter@work.nlm.nih.gov>.

(defun source-file (symbol package type)
  (declare (ignore type))
  (ilisp-errors
   (let* ((x (ilisp-find-symbol symbol package))
	  (fun (get-correct-fn-object x)))
     (when (and fun
                ;; <1>
                (not (the-function-if-defined
                      ((#:interpreted-function-p :sb-eval) ()) fun)))
	   ;; The hack above is necessary because CMUCL does not
	   ;; correctly record source file information when 'loading'
	   ;; a non compiled file.
	   ;; In this case we fall back on the TAGS machinery.
	   ;; (At least as I underestand the code).
	   ;; Marco Antoniotti 11/22/94.
	   (cond ((sb-pcl::generic-function-p fun)
		  (dolist (method (sb-pcl::generic-function-methods fun))
		    (print-simple-source-info
		     (or (sb-pcl::method-fast-function method)
			 (sb-pcl::method-function method))))
		  t)
		 (t (print-simple-source-info fun)))))))

;;; Patch suggested by Richard Harris <rharris@chestnut.com>

;;; FUN-DEFINED-FROM-PATHNAME takes a symbol or function object.  It
;;; returns a pathname for the file the function was defined in.  If it was
;;; not defined in some file, then nil is returned.
;;;
;;; FUN-DEFINED-FROM-PATHNAME is from hemlock/rompsite.lisp (cmucl17f), 
;;; with added read-time conditionalization to work in older versions
;;; of cmucl.  It may need a little bit more conditionalization for
;;; some older versions of cmucl.

(defun fun-defined-from-pathname (function)
  "Returns the file where FUNCTION is defined in (if the file can be found).
Takes a symbol or function and returns the pathname for the file the
function was defined in.  If it was not defined in some file, nil is
returned."
  (flet ((frob (code)
	   (let ((info (sb-kernel:%code-debug-info code)))
	     (when info
	       (let ((sources (sb-c::debug-info-source info)))
		 (when sources
		   (let ((source (car sources)))
		     (when (eq (sb-c::debug-source-from source) :file)
		       (sb-c::debug-source-name source)))))))))
    (typecase function
      (symbol (fun-defined-from-pathname (fdefinition function)))
      ;; <2>
      (#.(the-symbol-if-defined ((#:byte-function :sb-kernel) ()))
        "Byte compiled function or macro, no arglist available.")
      (#.(the-symbol-if-defined ((#:byte-closure :sb-kernel) ()))
        "Byte compiled closure, no arglist available.")
      (#.(the-symbol-if-defined ((#:byte-closure :sb-kernel) ()))
        (fun-defined-from-pathname
         (the-function-if-defined ((#:byte-closure-function :sb-kernel) ()
                                   :function-binding-p t)
                                  (funcall the-function function))))
      (#.(the-symbol-if-defined ((#:byte-function :sb-kernel) ()))
        (the-function-if-defined ((#:byte-function-component :sb-c) ()
                                  :function-binding-p t)
                                 (frob (funcall the-function function))))
      (function
        ;; <3>
        (frob (the-function-if-defined ((#:fun-code-header :sb-kernel)
                                        (#:function-code-header :sb-kernel))
                                       (the-function-if-defined
                                        ((#:%simple-fun-self :sb-kernel)
                                         (#:%function-self :sb-kernel))
                                        function))))
      (t nil))))


;;; print-simple-source-info --
;;; Patches suggested by Larry Hunter <hunter@work.nlm.nih.gov> and
;;; Richard Harris <rharris@chestnut.com>
;;; Nov 21, 1994.

(defun print-simple-source-info (fun)
  (let ((path (fun-defined-from-pathname fun)))
    (when (and path (probe-file path))
      (print (namestring (truename path)))
      t)))


(defun sbcl-trace (symbol package breakp)
  "Trace SYMBOL in PACKAGE."
  (ilisp-errors
   (let ((real-symbol (ilisp-find-symbol symbol package)))
     (setq breakp (read-from-string breakp))
     (when real-symbol (eval `(trace ,real-symbol :break ,breakp))))))

;;; end of file -- sbcl.lisp --

