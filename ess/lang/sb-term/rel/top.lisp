;;; -*- Mode: Lisp;  Package: syntax-box; Log: sb-changes.log  -*-
;;; Sccs Id @(#)top.lisp	1.43 10/1/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; Ergo Syntax Box

;;; Written (initially) by Bill Chiles.

;;; User interface changed completely by Tim Freeman because Bill Chiles' 
;;; code was inherently unportable.

;;; Functions reorganized so that clients may better access the individual 
;;; components of the syntax box.  Hook provided for client specific grammar 
;;; processing. -- Scott Dietzen (7/17/86)

;;; Revised, Scott Dietzen, Mon Oct 13 18:16:46 1986
;;;    Added calls to preprocessing phase. 


(in-package :syntax-box)  (use-package :ergolisp)

(export '(sb sb-make))

(defconstant-if-unbound sb-version "1 Feb 88")

(defconstant-if-unbound gen-src-file-ext "lisp")


(defmacro string-empty? (s)
  `(string= "" ,s))



(defun sb (&optional (lang-name "") &key
		     (processing-hook nil)
		     (suppress-sort-errors nil)
		     (no-sort-phase nil))
  "SB interactively gathers the necessary information to generate a parser
    or unparser (If you want to do things from a program, call sb-make.).
    SB then performs the necessary syntax box functions.
    Optional arguments are 
	lang-name
   		The name of language for which you are running the SB.
 		If not previously defined, it is interactively defined. 
    Keyword arguments are 
      suppress-sort-errors (boolean)
		Execute the sort phase, but do not print warnings or error
		messages. (Parser, lexer, and info file may still be
		generated even when errors occur.).
      no-sort-phase  (boolean)
		The sort analysis phase is not executed.  (Neither an unparser
                nor sort file may be generated.)
      processing-hook
		Function to be called after parsing the grammar which
		is given the grammar abstract syntax as an argument.
		Only necessary for special purpose applications in which 
		client wishes to modify his grammar structure before 
		processing.  The hook function should return the grammar
		structure and whether an error occurred (as multiple 
		values). Should a non nil error value be returned, no
		further processing will be done.  Argument defaults to 
                nil (no function)."

      
  (let* ((lang-name (if (or (null lang-name)
			    (and (stringp lang-name)
				 (string= lang-name "")))
			(read-trim-and-default "Language name:"
			    "" "")
			lang-name))
	 (lang (if (yesp
		    (format nil "Do you wish to procede without defining or ~
                                 redefining language ~S? " lang-name))
		  (lang:coerce-find-lang lang-name)
		  (cond ((yesp
			  (format nil "Do you want to read the language ~
		                       definition from a file? "))
			 (lang:read-lang lang-name)
			 (lang:coerce-find-lang lang-name))
			(t
			 (lang:interactively-define-lang lang-name)
			 (lang:coerce-find-lang lang-name))))))

    (multiple-value-bind
	(info? lexer? parser? unparser? sorts?)
	(if (yesp
	     (format nil "~%~%Generate all SB produced files for language ~S? "
	       lang-name))
	    (values t t t t t)
	    (sb-input lang-name))
      (sb-make :language (lang:lang-name lang)
	       :lang-struct (lang:coerce-find-lang lang-name)
	       :info? info?
	       :info-file (lang:get-lang-info-file lang-name)
	       :lexer? lexer?
	       :lexer-file (lang:get-lang-lexer-file lang-name)
	       :parser? parser?
	       :parser-file (lang:get-lang-parser-file lang-name)
	       :unparser? unparser?
	       :unparser-file (lang:get-lang-unparser-file lang-name)
	       :sorts? sorts?
	       :sorts-file (lang:get-lang-sorts-file lang-name)	       
	       :suppress-sort-errors suppress-sort-errors
	       :no-sort-phase no-sort-phase
	       :processing-hook processing-hook))

    (values)))
      




(defun sb-input (lang-name)
  "SB-INPUT interactively gathers the necessary information to run the 
    syntax box."
  (let (info? lexer? parser? unparser? sorts?)
    (setq unparser? (if (string= "" (lang:get-lang-unparser-file lang-name))
			nil
			(yesp "Generate an unparser?")))
    (setq parser? (if (string= "" (lang:get-lang-parser-file lang-name))
		      nil
		      (yesp "Generate a parser?")))
    (setq lexer?
	  (if parser?
	      (if (string= "" (lang:get-lang-lexer-file lang-name))
		  nil
		  (yesp "Generate a lexer?"))
	      nil))
    (setq info?
	  (if (and parser? lexer?)
	      (if (string= "" (lang:get-lang-info-file lang-name))
		  nil
		  (yesp "Generate an info file?"))))
    (setq sorts? (if (string= "" (lang:get-lang-sorts-file lang-name))
		     nil
		     (yesp "Generate a sorts file?")))
    (values info? lexer? parser? unparser? sorts?)))




(defun default-output-filename (working-dir conc-name suffix)
  "Generates the default output filename (as a string) from WORKING-DIR
and SUFFIX (without the file type)."
  (namestring
   (merge-pathnames
    (concatenate 'string conc-name suffix "." gen-src-file-ext)
    working-dir)))

(defun sb-make 
       (&key (language ())
	     
	     (lang-struct ())
	     
	     (conc-name "")
	     (working-dir "")
	     (abs-syn-package "")
	     (code-package "")
	     (use-packages ())
	     (sub-languages ())
             (grammar-file "")
	     (lexer-file "")
	     (parser-file "")
	     (unparse-nts ())
	     (unparser-file "")
	     (info-file "")
             (sorts-file "")
             (parse-routine-name ())
	     (unparse-routine-name ())
	     (win-unparse-routine-name ())

	     (info? t)
	     (lexer? t)
	     (unparser? t)
	     (parser? t)
	     (sorts? t)

	     (suppress-sort-errors nil)
	     (no-sort-phase nil)
	     (processing-hook nil)
	     &allow-other-keys
	     &aux error-occurred? lang)
    "Process a grammar, without any interactive input.  This routine does 
    not compile things.  We return t if we succeeded or nil if we did not.

    Keyword arguments are
      language     The name of the language
 
      lang-struct  Structure containing information about the language.
		   This overrides other input, and is generally passed from the
		   interactive SB.

          The following arguments are the parameters for the SB.  Most are
          defaulted.  The minimum you need define is working-dir or the
          explicit file names.  If lang-struct is provided, then the following
          are unused.  For explanations of these, use the interactive version
	  (SB:SB) first.  The defaults follow the names.
      conc-name 			language
      working-dir 
      grammar-file 			working-dir/*-gr.txt
      lexer-file 			working-dir/*-lexer.lisp
      parser-file 			working-dir/*-parser.lisp
      unparser-file 			working-dir/*-unparser.lisp
      info-file				working-dir/*-info.lisp
      sorts-file 			working-dir/*-sorts.lisp
      abs-syn-package 			conc-name
      code-package 			conc-name
      use-packages 			lang:standard-use-packages
      sub-languages			lang:standard-use-languages 
      unparse-nts 			t
      parse-routine-name 		conc-name'-PARSE'
      unparse-routine-name 		conc-name'-UNPARSE'
      win-unparse-routine-name          conc-name'-WIN-UNPARSE'

      parser?      boolean (generate a parser?)
      unparser?    boolean (generate an unparser?)
      lexer?       boolean (generate a lexer?)
      info?        boolean (generate an info file?)
      sorts?       boolean (generate a sort file?)

      suppress-sort-errors (boolean)
		Execute the sort phase, but do not print warnings or error
		messages. (Parser, lexer, and info file may still be
		generated even when errors occur.).
      no-sort-phase  (boolean)
		The sort analysis phase is not executed.  (Neither an unparser
                nor sort file may be generated.)
      processing-hook
		   Function to be called after parsing the grammar 
		   which is given the grammar abstract syntax as an 
	           argument.  Only necessary for special applications in 
                   which client wishes to modify his grammar before 
		   processing.  The hook function should return the grammar
 		   structure and whether an error occurred (as multiple 
                   values).  Should a non nil error value be returned, 
                   no further processsing will be done.  Argument defaults 
                   to nil (no function)."

    (format t "~&ERGO Project -- Syntactic Box Generator ~A~%" sb-version)

    (setq lang
	  (or lang-struct
	      (let ()			;does defaulting.
		(lang:lang-define
		 :name 			(if (stringp language)
					    language
					    (symbol-name language))
		 :conc-name 		conc-name
		 :working-dir 		working-dir
		 :abs-syn-package   	abs-syn-package
		 :code-package 		code-package
		 :use-packages 		use-packages
		 :sub-languages		sub-languages
		 :grammar-file 		grammar-file
		 :lexer-file 		lexer-file
		 :parser-file 		parser-file
		 :unparse-nts	 	unparse-nts
		 :unparser-file		unparser-file
		 :info-file 		info-file
		 :sorts-file 		sorts-file
		 :parse-routine-name 	parse-routine-name
		 :unparse-routine-name 	unparse-routine-name
		 :win-unparse-routine-name  win-unparse-routine-name)
		(lang:coerce-find-lang language))))
    (setq *language* lang)		; Further nested references.
    
    (if (string-empty? grammar-file)
	(setq grammar-file (lang:lang-grammar-file lang)))

    (if (and info? (string-empty? info-file))
	(setq info-file (default-output-filename working-dir
			  (lang-conc-name lang) "-info")))
    (if (and sorts? (string-empty? sorts-file))
	(setq sorts-file (default-output-filename working-dir
			   (lang-conc-name lang) "-sorts")))
    (if (and lexer? (string-empty? lexer-file))
	(setq lexer-file (default-output-filename working-dir
			   (lang-conc-name lang) "-lexer")))
    (if (and parser? (string-empty? parser-file))
	(setq parser-file (default-output-filename working-dir
			    (lang-conc-name lang) "-parser")))
    (if (and unparser? (string-empty? unparser-file))
	(setq unparser-file (default-output-filename working-dir
			      (lang-conc-name lang) "-unparser")))


    (let* ((*package* *sb-package*)
	   (*abs-syn-package-spec*
	    (lang:lang-abs-syn-package-name lang))
	   (*abs-syn-lisp-package*
	    (cond ((find-package *abs-syn-package-spec*))
		  (t (make-package *abs-syn-package-spec*))))
	   (*code-package-spec*
	    (lang:lang-code-package-name lang))	   
	   (*code-lisp-package*
	    (cond ((find-package *code-package-spec*))
		  (t (make-package *code-package-spec*))))
	   (*use-packages-spec* (lang:lang-use-packages lang)))
      
      (use-package *use-packages-spec* *abs-syn-lisp-package*)
      (use-package *use-packages-spec* *code-lisp-package*)

      (block error-exit-block		; Forgive me father, for I have
					; sinned...
        (format t "Reading grammar file ~S.~%" grammar-file)
        (multiple-value-setq (*grammar-term* error-occurred?)
          (sb-frontend grammar-file))
        (when error-occurred? (return-from error-exit-block))

	(let ((*grammar-case-sensitive?*
	       (grammar-case-sensitive? *grammar-term*)))
	
	  (when processing-hook
	    (format t "Executing client specific processing.~%")
	    (multiple-value-setq (*grammar-term* error-occurred?)
	      (funcall processing-hook *grammar-term*))
	    (when error-occurred?
	      (format t "Client processing yielded an error.~%")
	      (format t "Aborting syntax box ... ~%")
	      (return-from error-exit-block)))

	  (format t "Executing grammar preprocessing phase.~%")
	  (multiple-value-setq (*grammar-term* error-occurred?)
	    (pre-process *grammar-term* lang))
	  (when error-occurred? (return-from error-exit-block))
		  
	  (format t "Executing intermediate phase.~%")
	  (multiple-value-setq (*grammar-term* error-occurred?)
	    (sb-intermediate *grammar-term*))
	  (when error-occurred? (return-from error-exit-block))

	  (when (not no-sort-phase)
	    (format t "Executing sort analysis.~%")
	    (setq error-occurred?
		  (sort-grammar *grammar-term* suppress-sort-errors))
	    (when (and error-occurred?
		       (not suppress-sort-errors))
	      (return-from error-exit-block)))

	  (format t "Executing code generation phase.~%")
	  (sb-backend lang
		      :grammar *grammar-term*
		      :info-file (and info? info-file)
		      :sorts-file (if (or no-sort-phase
					  error-occurred?) ; when
					; suppress-sort-errors
				      nil
				      (and sorts? sorts-file))
		      :parser-file (and parser? parser-file)
		      :lexer-file (and lexer? lexer-file)
		      :unparser-file (if (or no-sort-phase
					     error-occurred?)
					 nil
					 (and unparser? unparser-file))
		      :unparse-nts unparse-nts)))

      (init-sb-attr-table)		; @@@ HACK! (see access.lisp)
      (parser-clean-up)
      (values error-occurred?)))



(defun sb-frontend (grammar-file)
  "SB-FRONTEND co-ordinates parsing the grammar file (opened)."
  (multiple-value-bind
      (grammar error-occurred?)
      (funcall (lang:get-lang-parse-routine-name "meta-grammar")
	       :nt 'meta-grammar :file grammar-file)
    (values grammar error-occurred?)))


(defun sb-intermediate (grammar)
  "SB-INTERMEDIATE coordinates the intermediate phase of the syntax box, 
   which includes reference disambiguation, relocation, and other 
   prerequisites to code generation"
  (let ((error?
	 (equal (crunch-grammar grammar) 'error-occurred)))
    (values grammar error?)))


(defun sb-backend (lang &key grammar (info-file nil)
			 	     (sorts-file nil)
				     (parser-file nil)
				     (lexer-file nil)
				     (unparser-file nil)
				     (unparse-nts nil))
  "SB-BACKEND coordinates code generation for the syntax box." 
  (block error-exit-block        ;; Forgive me father, for I have sinned...
    (get-external-grammars-info lang)
    (format t "   writing code for package ~A.~%" *code-package-spec*)
    (format t "   using packages ~S~%" *use-packages-spec*)
    (gen-var/const-names (lang:lang-conc-name lang))

    (when sorts-file
      (generate-sorts-file sorts-file))

    (setq *unparser-gen-error* nil)
    (when unparser-file
      (format t "   generating unparser ...~%")
      (setq unparse-nts
	    (cond ((or (eq unparse-nts t)
		       (and (null unparse-nts)
			    (equal (lang:lang-unparse-nts lang) t)))
		   (grammar-nonterminal-definitions grammar))
		  ((or (not (listp (lang:lang-unparse-nts lang)))
		       (not (listp unparse-nts)))
		   (error "The unparse-nts argument should be a list ~
			              or T."))
		  (t 
		   (select-nonterms-from-list (or unparse-nts
						  (lang:lang-unparse-nts lang))
					      *grammar-term*))))
      (generate-unparser grammar unparse-nts unparser-file))
			 
    (when *unparser-gen-error* (return-from error-exit-block))

    (setq *parser-error* nil)
    (when parser-file
      (format t "   generating parser ...~%")
      (generate-parser grammar parser-file))
    (when *parser-error* (return-from error-exit-block))

    (setq *global-operator-list* nil)
    (when lexer-file
      (cond (parser-file
	     (format t "   generating lexer ...~%")
	     (generate-custom-lexer grammar lexer-file))
	    (t
	     (warn "Can't make a lexer if you didn't make a parser."))))

    (when info-file
      (if (and lexer-file parser-file)
	  (generate-info-file info-file)
	  (warn "Can only write an info file when a parser and a ~
                            lexer were both generated.")))
    (values)))
  
    
;;; Generate an info file and give it the name info.
(defun generate-info-file (info)
  (with-open-file (s info
		     :direction :output :if-exists :new-version)
    (format t "   writing file ~S.~%" info)
    (sb-write
     (format s "~S~%~S~%~S~%~S~%~S~%~S~%"
	     *code-package-spec* *look-ahead-sets*
	     *first-results* *global-brackets*
	     *global-lexer-information*
	     *global-operator-list*))))


;;; Generate a sorts file and give it the name sorts.
(defun generate-sorts-file (sorts)
  (with-open-file (s sorts
		     :direction :output :if-exists :new-version)
    (format t "   writing file ~S.~%" sorts)
    (initialize-file s :sorts)
    (sb-write
     (let ((*package* (lang:lang-code-package *language*)))
       (format s "#+(and allegro-version>= (version>= 8 2))~
                    (eval-when (:execute :compile-toplevel :load-toplevel)~
                      (setq *readtable* cl::*pvs-readtable*))")
       (pprint (construct-opsig-table-function) s)
       (format s "~%~%")
       (pprint (construct-sort-table-function) s)
       (format s "~%~%")
       (lang:write-reduced-lang (lang:lang-name *language*)
				s)
       (format s "~%")))))



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
    
;;; LISTIFY-USE-PACKAGES coerces the string of use-packages names to
;;; a list of strings whose names are uppercases of the names in the string.
(defun listify-use-packages (use-packages)
  (mapcar #'string-upcase (string-to-list use-packages)))

;;; SELECT-NONTERMS-FROM-LIST looks up nonterminal names in the given
;;; alist of nonterminal information (nonterms) and returns a list of
;;; the corresponding nonterminal records.
(defun select-nonterms-from-list (nonterm-names grammar &aux nt)
  (mapcar #'(lambda (nt-name)
	      (loop 
	       (setf nt-name (intern (string-upcase nt-name) *sb-package*))
	       (do ((nt-runner (grammar-nonterminal-definitions grammar)
			       (cdr nt-runner)))
		   ((or (null nt-runner)
			(eq nt-name (nt-name (car nt-runner))))
		    (setf nt (car nt-runner))))
	       (cond (nt
		      (return nt))
		     (t 
		      (format t "Unidentifiable nonterminal name: ~A.  ~
	                         Please type it in correctly: " nt-name)
		      (setf nt-name
			    (read-line *standard-input*))))))
	  nonterm-names))

;;; Print the question, solicit a response from the user, and return 
;;; t if the answer was yes or nothing at all, otherwise return nil.
(defun yesp (question &aux answer)
  (setq answer
	(string-downcase
	 (read-trim-and-default question "yes" "yes")))
  (or (string= answer "")
      (string= answer "y")
      (string= answer "ye")
      (string= answer "yes")))




(defun get-expanded-file-spec (prompt default &optional (merger ""))
  (setq default (namestring (merge-pathnames default merger)))
  (read-trim-and-default prompt default default))


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


;;; Ask the user whether he wants to generate the prompt2 part of the
;;; output of the syntax box.  prompt1 should be either "a" or "an" 
;;; depending upon whether prompt2 starts with a vowel.  Conc is the conc 
;;; name the user gave.  default is a default file name to merge with the 
;;; result.  pprompt2 is a prettier version of prompt2.  If pprompt2 and 
;;; prompt2 are different, pprompt2 goes in the question and prompt2 goes 
;;; in the file name.
(defun prompt-for-sb-output-p (prompt1 prompt2 conc default
				       &optional (pprompt2 prompt2))
  (if (yesp (format nil "Construct ~A ~A?" prompt1 pprompt2))
      (get-expanded-file-spec
       (format nil "File name for generated ~A:" pprompt2)
       (format nil "~A-~A" conc prompt2)
       default)
      nil))



