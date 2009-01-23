;;; -*- Mode: Lisp; Package: Analysis-Facility -*-
;;;
;;; Sccs Id @(#)af-top.lisp	1.4 9/28/89");
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

;;; AF-TOP.LISP - Ergo Analysis Facility (Grammar Preprocessor)
;;;
;;; ABSTRACT
;;; 
;;; This program compiles an attribute grammar written in the analysis
;;; facility metalanguage into a collection of Lisp procedures which walk
;;; the parse tree of an object language program using the attribute family
;;; functions of NEWATTR.  This is a reimplementation of the ABox designed
;;; and implemented by William Maddox.  
;;; The code in particular af-dependency is based on the previous design.
;;;
;;; AUTHOR
;;;
;;;     R Nord
;;;
;;; HISTORY
;;;
;;;     07-22-87     rln     Initial development release.


(in-package :analysis-facility)

(export '(ab ab-make help))


(defconstant *version* "1.0 (September 29, 1989)")

(defconstant *maintainer* "rln@cs.cmu.edu")



(defconstant gen-src-file-ext #+spice "slisp" #+lucid "lisp")

(defconstant gen-src-file-ext-len (length gen-src-file-ext))

(defconstant ab-generic-package '("AF-RUNTIME-LIB"))

;;; Interface specials

(defvar *package-spec*)

(defvar *use-packages-spec*)

(defvar *ab-working-dir*)

(defvar *working-dir-source-ext*)

(defvar *ab-conc-name*)

(defvar *ab-conc-name-upcased*)

(defvar *analyzer-file-spec*)

;;; Attribute grammar specials

(defvar *gentag-table*)



;;; Analysis box top level.

;;; Handle abox errors by printing error message and aborting the body form.
;;; Value is true if an error occurred, else false.

(defmacro trap-abox-errors (&body body)
  (let ((status (gentemp)))
    `(let ((,status (catch 'abox-error-tag
		      ,@body
		      nil)))
       (if ,status
	   (progn
	     (format t "~A~%" ,status)
	     t)
	   nil))))


(eval-when (compile eval)

(defmacro ab-read-line ()
  `(string-trim '(#\space #\tab) (read-line)))

(defmacro merged-concat-name (prefix suffix merger)
  `(merge-pathnames (concatenate `simple-string ,prefix ,suffix) ,merger))

(defmacro expand-given-file-spec (file merger ending)
  `(if (stringp ,file)
       (merge-pathnames ,file ,merger)
     (merged-concat-name *ab-conc-name* ,ending ,merger)))
); eval-when

;;; Analysis box main program.  This constitutes the entire interface to this
;;; program. It was taken from the routines in "TOP.SLISP" from the synatx box.

(defun ab (&key conc-name grammar directory
		  (package nil) (use-packages nil use-packages-sp)
		  (analyzer nil))
  "Compiles an attribute grammar into an attribute evaluator.
  All arguments are keywords.
  :CONC-NAME is a string that controls some default names.
  :GRAMMAR is the name of the file containing the grammar.
   If it is not nil and not a string, then it
   defaults to <conc-name>-agr.txt in the working directory.
  :DIRECTORY is the directory the user is building the analyzer in.
   If it is not nil and not a string, then it defaults to the
   directory named conc-name in the current directory.
  :PACKAGE is a string that names the desired package for the generated
   analyzer. If it is not nil and not a string, then
   it defaults to the uppercased conc-name.  If a string is supplied, it is
   uppercased, and if any other value is supplied, the user is asked for a
   package. 
  :USE-PACKAGES is a string of package names separated by spaces
   that the generated files' package should use.  The generated files
   always use the \"ANALYSIS-RUNTIME\" package, and the :use-packages are all
   uppercased. 
  :ANALYZER contains the Lisp code for the generated analyzer.
   If it is not nil and not a string, then it
   defaults to <conc-name>-analyzer.slisp in the working directory."
  
    (format t "ERGO Analysis box -- Version ~A~%" *version*)
    (format t "Comments and Bug Reports to ~A~%" *maintainer*)
    (trap-abox-errors
     (init-interface-specials)
     (init-attribute-grammar-specials)
     (cond ((stringp conc-name) (setf *ab-conc-name* conc-name))
	   (t (format t "Conc name for files in working directory: ")
	      (setf *ab-conc-name* (ab-read-line))))
     (setf *ab-conc-name-upcased* (string-upcase *ab-conc-name*))
     (deal-with-directory-stuff directory)
     (setf grammar
	  (get-file-spec grammar "Grammar file:"
			 (concatenate 'simple-string *ab-conc-name* "-agr.txt")
			 *ab-working-dir*))
     (deal-with-package package use-packages use-packages-sp)
     (setf *analyzer-file-spec*
	  (get-file-spec analyzer "File name for generated analyzer:"
			 (concatenate 'simple-string *ab-conc-name* "-analyzer")
			 *working-dir-source-ext*))
     (deal-with-analyzer grammar)
     (format t "Thank you for using Ergo Software.")))


(defun ab-make (&key grammar 
		  (package nil) (use-packages nil)
		  (analyzer nil)
		  &allow-other-keys)
    (format t "ERGO Analysis box -- Version ~A~%" *version*)
    (format t "Comments and Bug Reports to ~A~%" *maintainer*)
    (trap-abox-errors
     (init-interface-specials)
     (init-attribute-grammar-specials)
     (deal-with-package-batch package use-packages)
     (setf *analyzer-file-spec* analyzer)
     (deal-with-analyzer grammar)
     (format t "Thank you for using Ergo Software.")))



(defun init-interface-specials ()
  (setf *ab-conc-name* "")
  (setf *ab-conc-name-upcased* "")
  (setf *ab-working-dir* nil)
  (setf *working-dir-source-ext* nil)
  (setf *package-spec* nil)
  (setf *use-packages-spec* ab-generic-package)
  (setf *analyzer-file-spec* nil))

(defun init-attribute-grammar-specials ()
  (setf *gentag-table* nil))


;;; DEAL-WITH-DIRECTORY-STUFF implements the semantics of the directory
;;; argument to ABOX  If it is supplied, and it is a string, then it is
;;; expanded to an absolute name and used as the directory spec.  If it is
;;; supplied as non-nil, and it is not a string, then the default name is
;;; used (see the doc string for ABOX).  We have to make sure a slash is at
;;; the end of the name because of how it is used, and the interpretation
;;; of the name by Spice Lisp pathname routines.

(defun deal-with-directory-stuff (directory)
  (let ((default (concatenate 'simple-string
			      "~/" *ab-conc-name* "/")))
    (if directory
	(unless (stringp directory)
		(setf directory default))
      (setf directory (get-expanded-file-spec "Working directory: " default))
    )
  (deal-with-directory-batch directory)))

(defun deal-with-directory-batch (directory)
  (let ((default (concatenate 'simple-string
			      "~/" *ab-conc-name* "/")))
    (unless (stringp directory)
	    (setf directory default)))
  (if (char/= (schar directory (1- (length (the simple-string directory))))
	      #\/)
      (setf directory (concatenate 'simple-string directory "/")))
  (setf *ab-working-dir* directory)
  (setf *working-dir-source-ext*
	(concatenate 'simple-string
		     *ab-working-dir* "." gen-src-file-ext)))



;;; DEAL-WITH-PACKAGE-STUFF implements the semantics of the package and
;;; use-packages arguments to ABOX.  If the package is supplied, and it is a
;;; string, then it is copied and uppercased.  It is copied because someone
;;; could write a function to call ABOX, and supply the same string for
;;; :conc-name and :package, which generates file names with uppercased
;;; prefixes.  

(defun deal-with-package (package use-packages use-packages-sp)
  (cond (package
	 (if (stringp package)
	     (setf *package-spec* (string-upcase package)) 
	     (setf *package-spec* *ab-conc-name-upcased*)))
	(t (let ((default *ab-conc-name-upcased*))
	     (format t "Package specification for generated files: [~S] "
		     default)
	     (setf package (ab-read-line))
	     (if (string= (the simple-string package) "")
		 (setf *package-spec* default)
		 (setf *package-spec* (nstring-upcase package))))))
  (cond (use-packages-sp
	 (setf *use-packages-spec*
	       (append *use-packages-spec*
		       (if (and (stringp use-packages)
				(string/= (the simple-string use-packages) ""))
			   (listify-use-packages use-packages)
			   use-packages))))
	(t (format t "Please enter packages to be used by ~S~
		      ~% on one line separated by spaces: [none] "
		   *package-spec*)
	   (setf use-packages (ab-read-line))
	   (unless (string= (the simple-string use-packages) "")
	     (setf *use-packages-spec*
		   (append *use-packages-spec*
			   (listify-use-packages use-packages)))))))

(defun deal-with-package-batch (package use-packages)
  (if (stringp package)
      (setf *package-spec* (string-upcase package)) 
      (setf *package-spec* *ab-conc-name-upcased*))
  (setf *use-packages-spec*
	(append *use-packages-spec*
		(if (and (stringp use-packages)
			 (string/= (the simple-string use-packages) ""))
		    (listify-use-packages use-packages)
		    use-packages))))

;;; Lisp Bug Fix
(defmacro read-from-string-fx (string eof-error-p eof-value &key start)
  `(if (>= ,start (length ,string))
       (values ,eof-value ,start)
     (read-from-string ,string ,eof-error-p ,eof-value :start ,start)))
  
;;; LISTIFY-USE-PACKAGES coerces the string of use-packages names to
;;; a list of symbols whose names are uppercases of the names in the string.
(defun listify-use-packages (use-packages)
  (do ((index 0)
       (result nil)
       sym)
      (nil)
    (multiple-value-setq (sym index)
      (read-from-string-fx use-packages nil :eof :start index))
    (if (eq sym :eof)
	(return result)
	(push (symbol-name sym) result))))


;;; DEAL-WITH-ANALYZER implements the semantics of the analyzer argument
;;; to ABOX.

(defun deal-with-analyzer (grammar)
  (let ((*package* (find-package 'analysis-facility)))
    (format t "   reading grammar file ~S ... ~%" grammar)
    (multiple-value-bind (abstract-syntax sb-error)
#+ergo-term	(parse :nonterminal 'agg :file grammar)
#-ergo-term     (af-parse :nt 'agg :file grammar)
      (if sb-error (abox-error "Syntax error in attribute grammar"))
      (format t "   executing analysis phase ...~%")
      (multiple-value-bind (declarations context syntext attributes)
#+ergo-term	(analyze abstract-syntax 'agg)
#-ergo-term	(agg-af-analyze abstract-syntax)
	(if (eq declarations :error) (abox-error "Constraint error in attribute grammar"))
	(format t "   writing code for package ~S~%" *package-spec*)
	(format t "~&   using packages ~S~%" *use-packages-spec*)
	(format t "~&   into ~S~%" *analyzer-file-spec*)
	(with-open-file (outstream *analyzer-file-spec*
				   :direction :output :if-exists :supersede)
          (format outstream ";;; -*- Mode: Lisp; Package: ~A -*-~2%" *package-spec*)
	  (format outstream ";;; Analyzer for grammar ~A~%" grammar)
	  (format outstream ";;; Generated by ERGO Analysis box -- Version ~A~%"
		  *version*)
          (terpri outstream)
	  (format outstream "(in-package ~S)~%~
              (use-package '(:ergolisp))~%~
	      (use-package '~S)~3%"
	      *package-spec*
	      *use-packages-spec*)
	  (terpri outstream)
	  (dolist (x declarations) (pprint x outstream))
	  (terpri outstream)
	  (dolist (x context) (pprint x outstream))
	  (terpri outstream)
	  (dolist (x syntext) (pprint x outstream))
	  (terpri outstream)
	  (dolist (x attributes) (pprint x outstream))
	  (terpri outstream))))))



(defun help ()
  "Prints this stuff."
  (format t "The following functions are available:~
	     ~2%ab:ab~%~A~
	     ~2%ab:help~%~A~
	     ~2%"
	  (documentation 'ab 'function)
	  (documentation 'help 'function))
  (values))


(defun get-file-spec (file prompt default merger)
  (cond (file (unless (stringp file)
		      (setf file default))
	      (merge-pathnames file merger))
	 (t (get-expanded-file-spec prompt default merger))))


(defun get-expanded-file-spec (prompt default &optional (merger ""))
  (let ((default (namestring (merge-pathnames default merger))))
    (write-string prompt)
    (format t " [~A] " default)
    (let ((response (ab-read-line)))
      (declare (simple-string response))
      (if (string= response "")
	  default
	  (namestring (merge-pathnames response merger))))))


;;; Print the question, solicit a response from the user, and return 
;;; t if the answer was yes or nothing at all, otherwise return nil.
(defun yesp (question &aux answer)
  (format t question)
  (setq answer
	(string-downcase (ab-read-line)))
    (or (string= answer "")
	(string= answer "y")
	(string= answer "ye")
	(string= answer "yes")))
