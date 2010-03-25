;;; -*- Mode: Lisp;  Package: lang; Log: lang-changes.log  -*-
;;; Sccs Id @(#)languages.lisp	1.46 10/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Generic language support for SB.

;;; Scott Dietzen, Thu Jun 18 03:15:27 1987


#-gcl
(defpackage :lang #+sbcl (:use :common-lisp :ergolisp))
(in-package :lang)
#-sbcl (use-package :ergolisp)


(export '(lang langp ck-lang-type
	  find-lang coerce-find-lang
	  lang-define

	  lang-name lang-conc-name lang-working-dir
	  lang-abs-syn-package-name lang-code-package-name
	  lang-use-packages lang-grammar-file lang-lexer-file lang-parser-file
	  lang-unparse-nts lang-unparser-file lang-info-file lang-sorts-file
	  lang-parse-routine-name
	  lang-unparse-routine-name lang-win-unparse-routine-name
	  lang-sort-table-name lang-opsig-table-name
	  lang-sub-languages 

	  get-lang-name get-lang-conc-name get-lang-working-dir
          get-lang-abs-syn-package-name get-lang-code-package-name 
          get-lang-use-packages get-lang-grammar-file
	  get-lang-lexer-file get-lang-parser-file get-lang-unparse-nts
	  get-lang-unparser-file get-lang-info-file get-lang-sorts-file 
	  get-lang-parse-routine-name
	  get-lang-unparse-routine-name get-lang-win-unparse-routine-name
	  get-lang-sort-table-name get-lang-opsig-table-name
	  get-lang-sub-languages 

	  interactively-define-lang read-lang
	  write-lang write-reduced-lang
	  lang-code-package get-lang-code-package 
	  lang-abs-syn-package get-lang-abs-syn-package

	  load-lang load-lang-source
	  compile-lang rec-compile-lang
	  compile-load-lang 

	  *porting-system*
	  ))


;;; Static Exported Macros -- This code is subject to the policy restriction
;;; that no exported macros will ever be changed, so a client need never
;;; recompile his code because of revisons herein.  Should any such change ever
;;; be necessary, the maintainer must accept the burdon of notifying all
;;; clients. This policy is necessary so that unnecessary recompilation does
;;; not insue every time this code is revised.  Particularly since so many
;;; clients depend on it. 



;;; Adding new langauage fields the programmer must
;;;	- change the language structure.
;;;	- change define-lang.
;;;	- change interactively-define-lang.
;;;	- add a lang-X routine.
;;;	- add a get-lang-X routine.
;;;     - change write-lang




(defvar *porting-system* nil
  "A non-nil value indicates that the system is running on a non-Sun.  Sun
   dependent features should not be used.  This seems to be good place to 
   put this variable because it makes it easier for other packages to refer to
   it .")



(defconstant-if-unbound standard-use-packages
  '("ERGOLISP" "OPER" "OCC" "TERM" "SORT" "SB-RUNTIME" "LANG" "NEWATTR")
  "The standard packages used by SB output files.")

(defconstant-if-unbound standard-use-languages
  '("LEXICAL-TERMINALS")
  "The standard languages used by SB output files.")


(defconstant-if-unbound gen-src-file-ext "lisp")

(defconstant-if-unbound per-gen-src-file-ext
  (concatenate 'string "." gen-src-file-ext))






(defstruct (lang-struct (:predicate langp))
  "Information about a language."
  (name "" 			:type string)
  (conc-name ""			:type string)		     
  (working-dir ""		:type string)		     
  (code-package ""		:type string)				
  (abs-syn-package ""		:type string)
  (use-packages ()		:type list)
  (grammar-file ""		:type string)
  (lexer-file ""		:type string)
  (parser-file ""		:type string)
  (unparse-nts t			    )
  (unparser-file ""		:type string)
  (info-file ""			:type string)
  (sorts-file ""		:type string)
  (parse-routine-name ()    	:type symbol)
  (unparse-routine-name ()	:type symbol)
  (win-unparse-routine-name ()	:type symbol)
  (sort-table-name ()		:type symbol)
  (opsig-table-name ()		:type symbol)
  (sub-languages ()		:type list)
)

(deftype lang ()
  "The type of languages"
  'lang-struct)

(defun ck-lang-type (lang)
  "Make sure LANG is a language, otherwise give an error message."
  (check-type lang lang "an operator"))



(eval-when (:compile-toplevel :execute :load-toplevel)
  (defmacro not-empty-str? (s)
    `(not (string= ,s ""))))


(defun do-intern (string package-name)
  (intern (string-upcase string)
	  (cond ((find-package package-name))
		(t
		 (make-package package-name)))))



(defvar *lang-table* (make-hash-table :test #'eq))

(defun find-lang (lang-name)
  "Look up LANG-NAME in the global language table.  If not found, return nil."
  (let ((lang-name (if (stringp lang-name)
		       (do-intern lang-name "LANG")
		       (do-intern (symbol-name lang-name) "LANG"))))
    (gethash lang-name *lang-table*)))


(defun coerce-find-lang (lang-name)
  "Look up LANG-NAME in the global language table.  If not found, signal an
   error." 
  (let ((lang (find-lang lang-name)))
    (cond (lang)
	  (t
	   (cerror
	    (format nil
		"Define ~S (read-lang or interactively-define-lang) ~
		 before continuing." lang-name)
	    (format nil
		"Language ~S has not been defined." lang-name))
	   (coerce-find-lang lang-name)))))



(eval-when (:compile-toplevel :execute :load-toplevel)

  (defmacro lang-name (lang)
    `(lang-struct-name ,lang))
  (defmacro lang-conc-name (lang)
    `(lang-struct-conc-name ,lang))
  (defmacro lang-working-dir (lang)
    `(lang-struct-working-dir ,lang))
  (defmacro lang-code-package-name (lang)
    `(lang-struct-code-package ,lang))
  (defmacro lang-abs-syn-package-name (lang)
    `(lang-struct-abs-syn-package ,lang))
  (defmacro lang-use-packages (lang)
    `(lang-struct-use-packages ,lang))
  (defmacro lang-grammar-file (lang)
    `(lang-struct-grammar-file ,lang))
  (defmacro lang-lexer-file (lang)
    `(lang-struct-lexer-file ,lang))
  (defmacro lang-parser-file (lang)
    `(lang-struct-parser-file ,lang))
  (defmacro lang-unparser-file (lang)
    `(lang-struct-unparser-file ,lang))
  (defmacro lang-unparse-nts (lang)
    `(lang-struct-unparse-nts ,lang))
  (defmacro lang-info-file (lang)
    `(lang-struct-info-file ,lang))
  (defmacro lang-sorts-file (lang)
    `(lang-struct-sorts-file ,lang))
  (defmacro lang-parse-routine-name (lang)
    `(lang-struct-parse-routine-name ,lang))
  (defmacro lang-unparse-routine-name (lang)
    `(lang-struct-unparse-routine-name ,lang))
  (defmacro lang-win-unparse-routine-name (lang)
    `(lang-struct-win-unparse-routine-name ,lang))
  (defmacro lang-sort-table-name (lang)
    `(lang-struct-sort-table-name ,lang))
  (defmacro lang-opsig-table-name (lang)
    `(lang-struct-opsig-table-name ,lang))
  (defmacro lang-sub-languages (lang)
    `(lang-struct-sub-languages ,lang))

  )					;eval-when



(defvar *name-last-lang-read* nil 
  "To recover language name after loading.")

(defun lang-define (&key (name "")
			 (conc-name "")
			 (working-dir "")
			 (code-package "")
			 (abs-syn-package "")
			 (use-packages ())
			 (grammar-file "")
			 (sub-languages ())
			 (lexer-file "")
			 (parser-file "")
			 (unparse-nts ())
			 (unparser-file "")
			 (info-file "")
			 (sorts-file "")
			 (parse-routine-name ())
			 (unparse-routine-name ())
			 (win-unparse-routine-name ())
			 (sort-table-name ())
			 (opsig-table-name ())
			 (package "")
			 &allow-other-keys)
  "This routine defines a language structure given the keyword arguments which
   make it up.  For an interactive definition, use INTERACTIVELY-DEFINE-LANG."
  (setq *name-last-lang-read* name)
  (let* ((hash-name (do-intern name "LANG"))
	 (old-lang (find-lang hash-name))
	 (lang
	  (make-lang-struct
	   :name name
	   :conc-name (setf conc-name	; used in other defaults.
			    (if (not-empty-str? conc-name)
				conc-name
				(if old-lang
				    (lang-conc-name old-lang)
				    name)))
	   :working-dir (setf working-dir
			      (if (not-empty-str? working-dir)
				  working-dir
				  (if old-lang
				      (lang-working-dir old-lang)
				      "")))
	   :code-package (setf code-package ; used in other defaults.
			       (if (not-empty-str? code-package)
				   code-package
				   (if old-lang
				       (lang-code-package-name old-lang)
				       (if (not-empty-str? package)
					   package
					   (string-upcase conc-name)))))
	   :abs-syn-package (if (not-empty-str? abs-syn-package)
				abs-syn-package
				(if old-lang
				    (lang-abs-syn-package-name old-lang)
				    (if (not-empty-str? code-package)
					code-package
					(string-upcase conc-name))))
	   :use-packages (union standard-use-packages
				(cond (use-packages)
				      (old-lang
				       (lang-use-packages old-lang)))
				:test #'string=)
	   :sub-languages (union standard-use-languages
				 (cond (sub-languages)
				       (old-lang
					(lang-sub-languages  old-lang)))
				 :test #'string=)
	   :grammar-file (namestring
			  (merge-pathnames
			   (if (not-empty-str? grammar-file)
			       grammar-file
			       (if old-lang
				   (lang-grammar-file old-lang)
				   (concatenate 'string conc-name "-gr.txt")))
			   working-dir))
	   :lexer-file (namestring
			(merge-pathnames
			 (if (not-empty-str? lexer-file)
			     lexer-file
			     (if old-lang
				 (lang-lexer-file old-lang)
				 (concatenate 'string
				   conc-name "-lexer." gen-src-file-ext)))
			 working-dir))
	   :parser-file (namestring
			 (merge-pathnames
			  (if (not-empty-str? parser-file)
			      parser-file
			      (if old-lang
				  (lang-parser-file old-lang)
				  (concatenate 'string
				    conc-name "-parser." gen-src-file-ext)))
			  working-dir))
	   :unparse-nts (or unparse-nts
			    (and old-lang
				 (lang-unparse-nts old-lang))
			    t)
	   :unparser-file (namestring
			   (merge-pathnames
			    (if (not-empty-str? unparser-file)
				unparser-file
				(if old-lang
				    (lang-unparser-file old-lang)
				    (concatenate 'string
				      conc-name "-unparser." gen-src-file-ext)))
			    working-dir))
	   :info-file (namestring
		       (merge-pathnames
			(if (not-empty-str? info-file)
			    info-file
			    (if old-lang
				(lang-info-file old-lang)
				(concatenate 'string
				  conc-name "-info." gen-src-file-ext)))
			working-dir))
	   :sorts-file (namestring
			(merge-pathnames
			 (if (not-empty-str? sorts-file)
			     sorts-file
			     (if old-lang
				 (lang-sorts-file old-lang)
				 (concatenate 'string
				   conc-name "-sorts." gen-src-file-ext)))
			 working-dir))
	  :parse-routine-name (or parse-routine-name
				  (and old-lang
				       (lang-parse-routine-name old-lang))
				  (do-intern (concatenate 'string
					       conc-name "-parse")
					     code-package))
	  :unparse-routine-name (or unparse-routine-name
				    (and old-lang
					 (lang-unparse-routine-name old-lang))
				    (do-intern (concatenate 'string
						 conc-name "-unparse")
					       code-package))
	  :win-unparse-routine-name (or win-unparse-routine-name
					(and old-lang
					     (lang-win-unparse-routine-name
					      old-lang))
					(do-intern (concatenate 'string
						     conc-name "-win-unparse")
						   code-package))
	  :sort-table-name (or sort-table-name
			       (and old-lang
				    (lang-sort-table-name old-lang))
			       (do-intern (concatenate 'string
					    "*" conc-name "-sort-table*")
					  code-package))
	  :opsig-table-name (or opsig-table-name
				(and old-lang 
				     (lang-opsig-table-name old-lang))
				(do-intern (concatenate 'string
					     "*" conc-name "-opsig-table*")
					   code-package))
	  )))

    ;; The following is not really worht it.
    ;; (if (and old-lang
    ;;          (not (equal old-lang lang)))
    ;;	   (warn "Redefining language ~S.~%~%" name))
    (setf (gethash hash-name
		   *lang-table*)
	  lang)

    ;;(let ((*package* (get-lang-lisp-code-package name)))
    ;;   (use-package use-packages))
    ;; Above does not compile properly in lucid, unless the language spec is
    ;; in package 'user.  Instead use package spec has been moved to the
    ;; front of the written file. 
    ))




(defun get-expanded-file-spec (prompt default &optional (merger ""))
  (setq default (namestring (merge-pathnames default merger)))
  (namestring (merge-pathnames
	       (read-trim-and-default prompt default default)
	       merger)))

(defun get0-expanded-file-spec (prompt default &optional (merger ""))
  (setq default (namestring (merge-pathnames default merger)))
  (let ((answer 
	 (read-trim-and-default
	     (format nil
		 "~? (0 for none) " prompt nil)
	     default default)))
    (if (string= answer "0")
	""
	(namestring (merge-pathnames answer
				     merger)))))

;;; Ask the user the question quest, and tell him pdefault is the default
;;; answer.  Read a one line response from the user, trim white space
;;; from both ends of the line, and return ldefault if the result is empty,
;;; or the line if the result was not.
(defun read-trim-and-default (quest pdefault ldefault &aux response)
  (format t "~A [~A] " quest pdefault)
  (setq response (string-trim '(#\space #\tab) (read-line)))
  (if (string= response "")
      ldefault
      response))


;;; Removes the standard packages form the use package list. 

(defun remove-standard-packages (L)
  (set-difference L
		  standard-use-packages
		  :test #'(lambda (x y)
			    (or (and (stringp x) (stringp y)
				     (string= x y))
				(and (symbolp x) (symbolp y)
				     (eq x y))))))

(defun remove-standard-languages (L)
  (set-difference L
		  standard-use-languages
		  :test #'(lambda (x y)
			    (or (and (stringp x) (stringp y)
				     (string= x y))
				(and (symbolp x) (symbolp y)
				     (eq x y))))))


;;; LISTIFY-USE-PACKAGES coerces the string of use-packages names to
;;; a list of strings whose names are uppercases of the names in the string.
(defun listify-use-packages (use-packages)
  (mapcar #'string-upcase (string-to-list use-packages)))


;;; Convert a string with a bunch of words separated by spaces into
;;; a list of words.  Each word is a string.
(defun string-to-list (string &aux (result '()) index)
  (setq string (concatenate 'string string " "))
  (loop
   (setq string (string-left-trim " " string))
   (when (equal string "") (return))
   (setq index (search " " string))
   (push (subseq string 0 index) result)
   (setq string (subseq string index)))
  result)




(defun interactively-define-lang (&optional (lang-name "") &aux lang)
  "This routine interactively defines a language by prompting for it's various
   components.  It may also be used to redefine an existing language."
  (setq lang (cond ((find-lang lang-name))
		   (t (make-lang-struct))))
  (let (name code-package abs-syn-package
        conc-name use-packages working-dir mergeable-default
	grammar-file lexer-file parser-file unparser-file unparse-nts
	info-file sorts-file parse-routine-name unparse-routine-name
	sort-table-name opsig-table-name win-unparse-routine-name
	sub-languages)


    (setq name
	  (let ((default (cond ((not-empty-str? (lang-name lang))
				(lang-name lang))
			       (t
				(cond ((stringp lang-name)
				       lang-name)
				      (t
				       (string-downcase
					(symbol-name lang-name))))))))
	    (read-trim-and-default "Language name:"
				   default default)))
    (setq code-package
	  (let ((default
		  (string-upcase (cond ((not-empty-str?
					 (lang-code-package-name lang))
					(lang-code-package-name lang))
				       (t name)))))
	    (string-upcase
	     (read-trim-and-default "Package specification for generated files:"
		 default default))))
    (setq abs-syn-package
	  (let ((default
		  (string-upcase
		   (cond ((not-empty-str? (lang-abs-syn-package-name lang))
			  (lang-abs-syn-package-name lang))
			 ((not-empty-str? code-package)
			  code-package)
			 (t name)))))
	    (string-upcase
	     (read-trim-and-default
		 "Package specification for produced abstract syntax:"
		 default default))))
    (setq conc-name
	  (let ((default (cond ((not-empty-str? (lang-conc-name lang))
				(lang-conc-name lang))
			       (t name))))
	  (read-trim-and-default "Conc name for files in working directory:"
				 default default)))

    (setq use-packages
	  (let ((default (cond ((lang-use-packages lang)
				(remove-standard-packages
				 (lang-use-packages lang)))
			       (t ""))))
	    (append
	     (remove-standard-packages
	      (listify-use-packages
	       (read-trim-and-default
		   (format nil
		       "Enter packages to be used by ~S ~
                       on one line separated by spaces (The~% ~
		       following standard packages required by SB runtime ~
		       support are assumed:~%  ~S )~%"
		     code-package standard-use-packages)
		   default default)))
	     standard-use-packages)))

    (setq working-dir
	  (let ((default (cond ((not-empty-str? (lang-working-dir lang))
				(lang-working-dir lang))
			       (t
				(concatenate 'string "~/" conc-name "/")))))
	    (get-expanded-file-spec "Working directory:" default)))
				  
    ;; Ensure the working dir really looks like a directory.
    (unless (equal (elt working-dir (- (length working-dir) 1)) #\/)
	    (setq working-dir (concatenate 'string working-dir "/")))
    
    (setq mergeable-default
	  (concatenate 'string working-dir "." gen-src-file-ext))

    (setq grammar-file
	  (let ((default (concatenate 'string conc-name "-gr.txt")))
	    (get-expanded-file-spec "Grammar file:"
	       default working-dir)))

    (setq sub-languages
	  (let ((default (cond ((lang-sub-languages lang)
				(remove-standard-languages
				 (lang-sub-languages lang)))

			       (t ""))))
	    (append
	     (remove-standard-languages
	      (listify-use-packages
	       (read-trim-and-default
		   (format nil
		       "Enter sub-languages to be used by ~S ~
                       on one line separated by spaces~%~
		       (The following standard sub-languages required by ~%~
                       SB runtime support are assumed:  ~S )~%"
		     name standard-use-languages)
		   default default)))
	     standard-use-languages)))

    (setq lexer-file
	  (let ((default (concatenate 'string
			   conc-name "-lexer." gen-src-file-ext)))
	    (get0-expanded-file-spec "Lexer file:" default mergeable-default)))

    (setq parser-file 
	  (let ((default (concatenate 'string
			   conc-name "-parser." gen-src-file-ext)))
	    (get0-expanded-file-spec "Parser file:" default mergeable-default)))

    (setq unparser-file
	  (let ((default (concatenate 'string
			   conc-name "-unparser." gen-src-file-ext)))
	    (get0-expanded-file-spec "Unparser file:" default
	      mergeable-default)))

    (setq unparse-nts
	  (multiple-value-bind
	      (default1 default2)
	      (cond ((eq (lang-unparse-nts lang) t)
		     (values "all" t))
		    (t
		     (values (lang-unparse-nts lang) (lang-unparse-nts lang))))
	    (read-trim-and-default
	     (format nil
		 "Names of nonterminals to unparse separated by spaces:~%")
	     default1 default2)))
    (when (stringp unparse-nts)
      (setq unparse-nts (string-to-list unparse-nts)))

    (setq info-file
	  (let ((default (concatenate 'string
			   conc-name "-info." gen-src-file-ext)))
	    (get0-expanded-file-spec "Info file:" default mergeable-default)))

    (setq sorts-file
	  (let ((default (concatenate 'string
			   conc-name "-sorts." gen-src-file-ext)))
	    (get0-expanded-file-spec "Sorts file:" default mergeable-default)))

    (let ((answer
	   (string-downcase
	    (read-trim-and-default "Default the remaining names? "
		"yes" "yes"))))
      (cond
       ((not (or (string= answer "")
		 (string= answer "y")
		 (string= answer "ye")
		 (string= answer "yes")))

	(if (not-empty-str? parser-file)
	    (setq parse-routine-name
		  (let ((def-name
			  (cond ((lang-parse-routine-name lang))
				(t (concatenate 'string conc-name "-parse")))))
		    (do-intern 
		     (read-trim-and-default "Name of parsing routine:"
			 def-name def-name)
		     code-package))))

	(if (not-empty-str? unparser-file)
	    (setq unparse-routine-name
		  (let ((def-name
			  (cond ((lang-unparse-routine-name lang))
				(t
				 (concatenate 'string conc-name "-unparse")))))
		    (do-intern 
		     (read-trim-and-default "Name of unparsing routine:"
			 def-name def-name)
		     code-package))))

	(if (not-empty-str? unparser-file)
	    (setq win-unparse-routine-name
		  (let ((def-name
			  (cond ((lang-win-unparse-routine-name lang))
				(t (concatenate 'string
				     conc-name "-win-unparse")))))
		    (do-intern 
		     (read-trim-and-default "Name of window unparsing routine:"
			 def-name def-name)
		     code-package))))

	(if (not-empty-str? sorts-file)
	    (setq sort-table-name
		  (let ((def-name (cond ((lang-sort-table-name lang))
					(t (concatenate 'string
					     "*" conc-name "-sort-table*")))))
		    (do-intern 
		     (read-trim-and-default "Name of sort table:"
			 def-name def-name)
		     code-package))))

	(if (not-empty-str? sorts-file)
	    (setq opsig-table-name
		  (let ((def-name (cond ((lang-opsig-table-name lang))
					(t (concatenate 'string
					     "*" conc-name "-opsig-table*")))))
		    (do-intern 
		     (read-trim-and-default "Name of operator signature table:"
			 def-name def-name)
		     code-package)))))))

    (format t "~%~%")

    (lang-define
      :name name
      :conc-name conc-name
      :code-package code-package
      :abs-syn-package abs-syn-package
      :use-packages use-packages
      :working-dir working-dir
      :grammar-file grammar-file
      :lexer-file lexer-file
      :parser-file parser-file
      :unparser-file unparser-file
      :unparse-nts unparse-nts
      :info-file info-file
      :sorts-file sorts-file
      :parse-routine-name parse-routine-name
      :unparse-routine-name unparse-routine-name
      :win-unparse-routine-name win-unparse-routine-name
      :sort-table-name sort-table-name
      :opsig-table-name opsig-table-name
      :sub-languages sub-languages)

    (let ((answer
	   (string-downcase
	    (read-trim-and-default
		(format nil
		    "Write out the language definition to ~S?" name)
		"yes" "yes"))))
      (cond ((or (string= answer "")
		 (string= answer "y")
		 (string= answer "ye")
		 (string= answer "yes"))
	     (write-lang name))))

    (coerce-find-lang name)))





(defun read-lang (lang-name &optional (file-name nil))
  "Read the language defintion LANG-NAME from a file.  If LANG-NAME is nil
   it will be read from the file and returned.  If the optional file is not
   input, it is prompted for." 
  (let ((lang-name (if (stringp lang-name)
		       lang-name
		       (if lang-name
			   (string-downcase (symbol-name lang-name))))))
    (if (not file-name)
	(setq file-name
	      (let ((def-dir
		      (concatenate 'string "~/" lang-name "/")))
		(get-expanded-file-spec "Input file:"
   		    (if lang-name
			(concatenate 'string
			  lang-name "-lang-def." gen-src-file-ext)
			"")
		  (if lang-name 
		      def-dir
		      "")))))
    (load file-name)
    lang::*name-last-lang-read*))
    



(defun write-lang (lang-name &optional (file-name nil))
  "Write language LANG-NAME out to a file."
  (let* ((lang (coerce-find-lang lang-name))
	 (*print-level* nil)
	 (*print-length* nil)
	 (pack-name (lang-code-package-name lang)))
    (if (not file-name)
	(setq file-name
	      (let ((default (concatenate 'string
			       (lang-conc-name lang)
			       "-lang-def." gen-src-file-ext)))
		(get-expanded-file-spec "File:"
		    default
		  (lang-working-dir lang)))))
    (with-open-file (s file-name :direction :output
		       :if-exists :supersede)
      (format s 
	  "(in-package ~S)  ;; package for abstract syntax. ~%~%~
           (in-package ~S)  ;; package for generated code.  ~%~%~
           (use-package '~S)~%~%~
	   (lang:lang-define ~%~
	    :name ~S~%~
	    :conc-name ~S~%~
	    :working-dir ~S~%~
	    :code-package ~S~%~
            :abs-syn-package ~S~%~
	    :use-packages '~S~%~
	    :grammar-file ~S~%~
	    :sub-languages '~S~%~
	    :lexer-file ~S~%~
	    :parser-file ~S~%~
	    :unparse-nts '~S~%~
	    :unparser-file ~S~%~
	    :info-file ~S~%~
	    :sorts-file ~S~%~
	    :parse-routine-name '~A::~A~%~
	    :unparse-routine-name '~A::~A~%~
	    :win-unparse-routine-name '~A::~A~%~
	    :sort-table-name '~A::~A~%~
	    :opsig-table-name '~A::~A~%~
            )~%"
	(lang-abs-syn-package-name lang)
	(lang-code-package-name lang)
	(lang-use-packages lang)
	(lang-name lang)
	(lang-conc-name lang)
	(lang-working-dir lang)
	(lang-code-package-name lang)
	(lang-abs-syn-package-name lang)
	(lang-use-packages lang)
	(lang-grammar-file lang)
	(lang-sub-languages lang)
	(lang-lexer-file lang)
	(lang-parser-file lang)
	(lang-unparse-nts lang)
	(lang-unparser-file lang)
	(lang-info-file lang)
	(lang-sorts-file lang)
	;; IF we don't use this fancy way of writing the symbols, exported
	;; symbols may be written with only one ':' which does not work if this
	;; file is loaded before the language files (as it probably would be). 
	pack-name (symbol-name (lang-parse-routine-name lang))
	pack-name (symbol-name (lang-unparse-routine-name lang))
	pack-name (symbol-name (lang-win-unparse-routine-name lang))
	pack-name (symbol-name (lang-sort-table-name lang))
	pack-name (symbol-name (lang-opsig-table-name lang))
	))))



(defun write-reduced-lang (lang-name s)				     
  "Write language LANG-NAME without any file names to stream S.  Used in the
   sorts file." 
  (let* ((lang (coerce-find-lang lang-name))
	 (*print-level* nil)
	 (*print-length* nil)
	 (pack-name (lang-code-package-name lang)))
    (format s 
	"(lang:lang-define ~%~
	    :name ~S~%~
	    :conc-name ~S~%~
	    :code-package ~S~%~
            :abs-syn-package ~S~%~
	    :use-packages '~S~%~
	    :sub-languages '~S~%~
	    :unparse-nts '~S~%~
	    :parse-routine-name '~A::~A~%~
	    :unparse-routine-name '~A::~A~%~
	    :win-unparse-routine-name '~A::~A~%~
	    :sort-table-name '~A::~A~%~
	    :opsig-table-name '~A::~A~%~
            )~%"
      (lang-name lang)
      (lang-conc-name lang)
      (lang-code-package-name lang)
      (lang-abs-syn-package-name lang)
      (lang-use-packages lang)
      (lang-sub-languages lang)
      (lang-unparse-nts lang)
      ;; IF we don't use this fancy way of writing the symbols, exported
      ;; symbols may be written with only one ':' which does not work if this
      ;; file is loaded before the language files (as it probably would be). 
      pack-name (symbol-name (lang-parse-routine-name lang))
      pack-name (symbol-name (lang-unparse-routine-name lang))
      pack-name (symbol-name (lang-win-unparse-routine-name lang))
      pack-name (symbol-name (lang-sort-table-name lang))
      pack-name (symbol-name (lang-opsig-table-name lang))
      )))


(defun get-lang-name (lang-name)
  (lang-name (coerce-find-lang lang-name)))
(defun get-lang-conc-name (lang-name)
  (lang-conc-name (coerce-find-lang lang-name)))
(defun get-lang-working-dir (lang-name)
  (lang-working-dir (coerce-find-lang lang-name)))
(defun get-lang-code-package-name (lang-name)
  (lang-code-package-name (coerce-find-lang lang-name)))
(defun get-lang-abs-syn-package-name (lang-name)
  (lang-abs-syn-package-name (coerce-find-lang lang-name)))
(defun get-lang-use-packages (lang-name)
  (lang-use-packages (coerce-find-lang lang-name)))
(defun get-lang-grammar-file (lang-name)
  (lang-grammar-file (coerce-find-lang lang-name)))
(defun get-lang-sub-languages (lang-name)
  (lang-sub-languages (coerce-find-lang lang-name)))
(defun get-lang-lexer-file (lang-name)
  (lang-lexer-file (coerce-find-lang lang-name)))
(defun get-lang-parser-file (lang-name)
  (lang-parser-file (coerce-find-lang lang-name)))
(defun get-lang-unparser-file (lang-name)
  (lang-unparser-file (coerce-find-lang lang-name)))
(defun get-lang-unparse-nts (lang-name)
  (lang-unparse-nts (coerce-find-lang lang-name)))
(defun get-lang-info-file (lang-name)
  (lang-info-file (coerce-find-lang lang-name)))
(defun get-lang-sorts-file (lang-name)
  (lang-sorts-file (coerce-find-lang lang-name)))
(defun get-lang-parse-routine-name (lang-name)
  (lang-parse-routine-name (coerce-find-lang lang-name)))
(defun get-lang-unparse-routine-name (lang-name)
  (lang-unparse-routine-name (coerce-find-lang lang-name)))
(defun get-lang-win-unparse-routine-name (lang-name)
  (lang-win-unparse-routine-name (coerce-find-lang lang-name)))
(defun get-lang-sort-table-name (lang-name)
  (lang-sort-table-name (coerce-find-lang lang-name)))
(defun get-lang-opsig-table-name (lang-name)
  (lang-opsig-table-name (coerce-find-lang lang-name)))





(defun lang-code-package (lang)
  "Return the lisp package defined for a language structure."
  (cond ((find-package (lang-code-package-name lang)))
	(t
	 (make-package (lang-code-package-name lang)))))

(defun get-lang-code-package (lang-name)
  "Return the lisp package defined for a language given the language NAME."
  (lang-code-package (coerce-find-lang lang-name)))


(defun lang-abs-syn-package (lang)
  "Return the lisp package defined for a language structure."
  (cond ((find-package (lang-abs-syn-package-name lang)))
	(t
	 (make-package (lang-abs-syn-package-name lang)))))

(defun get-lang-abs-syn-package (lang-name)
  "Return the lisp package defined for a language given the language NAME."
  (lang-abs-syn-package (coerce-find-lang lang-name)))




(defun load-lang (lang-name)
  "Loads the SB generated files associated with LANG-NAME."
  (let ((lang (coerce-find-lang lang-name)))
    ;; (tools:boxload 'sb-support)
    (mapc #'load-lang
	  (lang-sub-languages lang))
    (if (not-empty-str? (lang-lexer-file lang))
	(load (if (search per-gen-src-file-ext (lang-lexer-file lang))
		  (subseq (lang-lexer-file lang)
			  0
			  (search per-gen-src-file-ext
				  (lang-lexer-file lang)))
		  (lang-lexer-file lang))
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-parser-file lang))
	(load (if (search per-gen-src-file-ext (lang-parser-file lang))
		  (subseq (lang-parser-file lang)
			  0
			  (search per-gen-src-file-ext
				  (lang-parser-file lang)))
		  (lang-parser-file lang))
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-unparser-file lang))
	(load (if (search per-gen-src-file-ext (lang-unparser-file lang))
		  (subseq (lang-unparser-file lang)
			  0
			  (search per-gen-src-file-ext
				  (lang-unparser-file lang)))
		  (lang-unparser-file lang))
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-sorts-file lang))
	(load (if (search per-gen-src-file-ext (lang-sorts-file lang))
		  (subseq (lang-sorts-file lang)
			  0
			  (search per-gen-src-file-ext
				  (lang-sorts-file lang)))
		  (lang-sorts-file lang))
	      :verbose t
	      :if-does-not-exist nil))
    nil))


(defun load-lang-source (lang-name)
  "Loads the SB generated source (.lisp) files associated with LANG-NAME."
  (let ((lang (coerce-find-lang lang-name)))
    ;; (tools:boxload 'sb-support)
    (mapc #'load-lang-source
	  (lang-sub-languages lang))
    (if (not-empty-str? (lang-lexer-file lang))
	(load (lang-lexer-file lang)
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-parser-file lang))
	(load (lang-parser-file lang)
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-unparser-file lang))
	(load (lang-unparser-file lang)
	      :verbose t
	      :if-does-not-exist nil))
    (if (not-empty-str? (lang-sorts-file lang))
	(load (lang-sorts-file lang)
	      :verbose t
	      :if-does-not-exist nil))
    nil))


(defun compile-lang (lang-name)
  "Compiles the SB generated files associated with LANG-NAME."
  ;; (tools:boxload 'sb-support)
  (let ((lang (coerce-find-lang lang-name))
	(*print-case* :upcase))		; LUCID BUG
    (if (and (not-empty-str? (lang-lexer-file lang))
	     (probe-file (lang-lexer-file lang)))
	(compile-file (lang-lexer-file lang)))
    (if (and (not-empty-str? (lang-parser-file lang))
	     (probe-file (lang-parser-file lang)))
	(compile-file (lang-parser-file lang)))
    (if (and (not-empty-str? (lang-unparser-file lang))
	     (probe-file (lang-unparser-file lang)))
	(compile-file (lang-unparser-file lang)))
					; no need to compile sorts.
    nil))


(defun rec-compile-lang (lang-name)
  "Compiles the SB generated files associated with LANG-NAME and all associated
   sublanguages."
  ;; (tools:boxload 'sb-support)
  (let ((lang (coerce-find-lang lang-name)))
    (mapc #'rec-compile-lang
	  (lang-sub-languages lang))
    (compile-lang lang-name)
    nil))


(defun compile-load-lang (lang-name)
  "Compiles and loads the SB generated files associated with LANG-NAME."
  (rec-compile-lang lang-name)
  (load-lang lang-name))
