;;
;; prooflite.lisp
;; Release: ProofLite-7.1.0 (11/05/20)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/ProofLite
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;

(in-package :pvs)

(defparameter *prooflite-version* "7.1.0 (Nov 05, 2020)")

(defun associate-proof-with-formulas (theory-name formula-name strategy force
						  &optional
						  (overwrite-default-proof? t)
						  (save-prf-file? t))
  ;; overwrite-default-proof? is meaninless if force is nil
  (let ((theory (get-typechecked-theory theory-name)))
    (if theory
	(let* ((fname  (match-formula-name formula-name))
	       (name   (car fname))
	       (args   (cdr fname)))
	  (if fname
	      (let* ((regexp (expand-rex name))
		     (str    (if force "" "untried "))
		     (fdecls (remove-if-not
			      #'(lambda (d)
				  (and (formula-decl? d)
				       (let ((idstr (format nil "~a" (id d))))
					 (and (or (not (proofs d)) force)
					      (pregexp-match regexp idstr)))))
			      (all-decls theory))))
		(if fdecls
		    (associate-proof-with-formula 
		     theory regexp (instantiate-script strategy args 1 "#") 
		     fdecls
		     overwrite-default-proof?
		     save-prf-file?)
		  (pvs-message 
		   "\"~a\" does not match any ~aformula" name str)))
	    (pvs-message 
	     "\"~a\" is not a valid proof script header" formula-name)))
      (pvs-message "Theory ~a not found" theory-name))))

(defun associate-proof-with-formula (theory regexp strategy fdecls
					    &optional
					    (overwrite-default-proof? t)
					    (save-prf-file? t))
  "This function overwrites the default proof in FDECLS if OVERWRITE-DEFAULT-PROOF? is t (default)."
  (when fdecls
    (let* ((fdecl  (car fdecls))
           (match  (pregexp-match regexp (format nil "~a" (id fdecl))))
           (script (instantiate-script strategy match 0 "\\$")))
      (multiple-value-bind (strat err)
	  #+(and allegro (not pvs6) (not pvs7))
	  (unwind-protect
	      (progn (excl:set-case-mode :case-insensitive-lower)
		     (ignore-errors (values (read-from-string script))))
	    (excl:set-case-mode :case-sensitive-lower))
	  #-(and allegro (not pvs6) (not pvs7))
	  (ignore-errors (values (read-from-string script)))
	(let ((just (unless err
		      (or (revert-justification strat)
			  (revert-justification (list "" strat))
			  strat))))
	  (unless just
	    (type-error script 
			"Bad form for script~%  ~s" script))

	  (if overwrite-default-proof?
	      (setf (justification fdecl) just)
	    (let ((nextid (next-proof-id fdecl)))
	      (format t "~%~a~%" nextid)
	      (make-default-proof fdecl (extract-justification-sexp just) nextid)))
	  ;; Save proof to prf file only if requested
	  (when save-prf-file?
	    (save-all-proofs theory))
	  (pvs-message "Proof script ~a was installed" (id fdecl)))))
    (associate-proof-with-formula theory regexp strategy (cdr fdecls))))

;; Returns a list of theory names defined in a PVS file
(defun theories-in-file (file)
  ;; gethash returns a list of form (date th1 th2 ...), where date is the
  ;; time the file was parsed.
  (cdr (gethash file (current-pvs-files))))

;; Returns a list of theories imported in theory-names  
(defun imported-theories-in-theories (theory-names)
  (remove-duplicates 
   (mapcan #'(lambda (theo) (imported-theories-in-theory theo))
	   theory-names)))

(defun my-collect-theory-usings (theory)
  (unless (or (memq theory *modules-visited*)
	      (from-prelude? theory)
	      (lib-datatype-or-theory? theory))
    (push theory *modules-visited*)
    (dolist (use (get-immediate-usings theory))
      (let ((th (get-theory use)))
	(when th
	  (my-collect-theory-usings 
	   (if (typep th 'rectype-theory)
	       (get-typechecked-theory (generated-by th))
	       th)))))))

;; Returns a list of immediately imported theories in the theory-name
(defun immediate-theories-in-theory (theory-name)
  (let ((theory (get-typechecked-theory theory-name)))
    (when theory
      (remove-duplicates 
       (loop for use in (get-immediate-usings theory)
	     for th = (get-theory use)
	     when th
	     unless (from-prelude? th)
	     collect (if (typep th 'rectype-theory)
			 (get-typechecked-theory (generated-by th))
		       th))))))

;; Returns a list of theories imported in the theory-name
(defun imported-theories-in-theory (theory-name)
  (let ((theory (get-typechecked-theory theory-name)))
    (when theory
      (let* ((*modules-visited* nil))
	(my-collect-theory-usings theory)
	(nreverse *modules-visited*)))))

(defun trim-left (str)
  (string-left-trim '(#\Space #\Tab) str))

(defun trim-right (str)
  (string-right-trim '(#\Space #\Tab) str))

(defun trim (str)
  (string-trim '(#\Space #\Tab) str))

(defun ident () "([\\w\\*?]+)")

(defun spaces () "[\\s\\t]*")

(defun greedyspaces () "(?<!\\s\\t)[\\s\\t]*(?!\\s\\t)")

(defun replace-all-str (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun expand-rex (str)
  (format nil "^~a$" (replace-all-str (pregexp-replace* "*" str "(.+)") "?" "\\?")))

(defun read-one-line (file)
  (let* ((str   (read-line file nil nil))
	 (strim (when str (trim str))))
    (cond ((and (> (length strim) 3) (string= strim "%|-" :end1 3)) 
	   strim)
	  ((and (> (length strim) 0) (char= (elt strim 0) #\%))
	   "%")
	  (strim ""))))

(defun match-formula-name (str)
  (let ((match (cdr (pregexp-match 
		     (format nil "^~a(?:$|~a\\[(.*)\\]$)"
			     (ident) (greedyspaces))
		     str))))
    (when match
      (let* ((name   (car match))
	     (argstr (cadr match))
	     (args   (when argstr (pregexp-split ";" (trim argstr))))
	     (nargs  (when args (mapcar #'(lambda (x) (trim x)) args))))
	(cons name nargs)))))

(defun is-proof-comment (str)
  (when (> (length str) 3) (trim-left (subseq str 3))))

(defun is-comment (str) (> (length str) 0))

(defun match-proof (str)
  (let ((match (pregexp-match-positions
		(format nil "^(.+):~a(?i:proof)\\b" (spaces))
		str)))
    (when match 
      (cons (trim-right (subseq str (caadr match) (cdadr match)))
	    (trim (subseq str (cdar match) (length str)))))))

(defun match-qed (str)
  (let ((match (pregexp-match "^(.*)\\b(?i:qed).*$" str)))
    (when match
      (trim (cadr match)))))

(defun install-script (theory script formulas force &optional (overwrite-default-proof? t) (save-prf-file? t))
  (let ((strategy (format nil "(\"\" ~a)" script)))
    (multiple-value-bind (strat err)
	(ignore-errors (values (read-from-string strategy)))
      (if err
	  (pvs-error "Prooflite script error"
		     (format nil "Error in script for formula ~{~a~^, ~}: ~a." formulas err))
	(multiple-value-bind (msg subjust)
	    (check-edited-justification strat)
	  (if msg
	      (pvs-error
	       "Proof syntax error"
	       (format nil
		       "Error in script for formula ~{~a~^, ~}: ~a ~@[(Offending proof part: ~s)~]"
		       formulas
		       (or msg "Proof syntax error")
		       subjust))
	    (when formulas 
	      (associate-proof-with-formulas
	       theory
	       (car formulas) 
	       strategy
	       force
	       overwrite-default-proof?
	       save-prf-file?)
	      (install-script theory script (cdr formulas) force overwrite-default-proof? save-prf-file?))))))))

(defun new-script (script new)
  (let ((notempty (> (length new) 0)))
    (cond ((and script notempty)
	   (format nil "~a ~a" script new))
	  (script script)
	  (notempty new))))

(defun instantiate-script (script args n str)
  (if args
      (let ((new (pregexp-replace* (format nil "~a~a" str n) script
				   (car args))))
	(instantiate-script new (cdr args) (+ n 1) str))
    script))

(defun install-prooflite-scripts (filename theory line force)
  (with-open-file 
   (file (make-specpath filename) :direction :input)
   (let* ((loc        (place (get-theory theory)))
	  (lfrom      (aref loc 0))
	  (lto        (aref loc 2)))
     (loop repeat (- lfrom 1) do (read-line file nil nil))
     (if (= line 0)
	 (pvs-message "Installing inlined proof scripts into theory ~a." 
		      theory)
       (pvs-message "Installing proof script at line ~a of file ~a." 
		    line filename))
     (do ((str (read-one-line file))
	  (n   lfrom)
	  (formulas)
	  (script))
	 ((or (null str) (> n lto)))
	 (let ((proofcomment (is-proof-comment str)))
	   (cond (proofcomment
		  (let* ((proof     (match-proof proofcomment))
			 (formula   (car proof))
			 (qed       (match-qed proofcomment)))
		    (cond 
		     (formula
		      (when (and script (< line n))
			(pvs-message 
			 "QED is missing in proof script(s) ~a [Theory: ~a]"
			 formulas theory))
		      (setq str (format nil "%|-~a" (cdr proof)))
		      (setq formulas (cons formula 
					   (when (not script) formulas)))
		      (setq script nil))
		     (qed
		      (let ((newscript (new-script script qed))) 
			(when (and newscript formulas (<= line n))
			  (install-script theory newscript 
					  (reverse formulas) 
					  force))
			(cond ((or (= line 0) (< n line))
			       (setq str (read-one-line file))
			       (setq n   (+ n 1))
			       (setq formulas nil)
			       (setq script nil))
			      ((not formulas) 
			       (pvs-message 
				"No script was installed [Theory: ~a]" 
				theory)
			       (setq str nil))
			      (t (setq str nil)))))
		     (t (setq str (read-one-line file))
			(setq n   (+ n 1))
			(setq script (new-script script proofcomment))))))
		 ((is-comment str)
		  (setq str (read-one-line file))
		  (setq n   (+ n 1)))
		 (t 
		  (when (and formulas (< line n))
		    (pvs-error
		     "Prooflite script error"
		     (format
		      nil
		      "QED is missing in proof script~:[~;s~] ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} [Theory: ~a]"
		      (cadr formulas)
		      formulas theory)))
		  (cond ((or (= line 0) (< n line))
			 (setq str (read-one-line file))
			 (setq n   (+ n 1))
			 (setq formulas nil)
			 (setq script nil))
			((and (< 0 line) (or (not formulas) (= line n)))
			 (pvs-message "No script was installed [Theory: ~a]" 
				      theory)
			 (setq str nil))
			(t (setq str nil))))))))))

(defun install-prooflite-scripts-from-prl-file (theory prl-filename force &optional (plain-script? t))
  "Installs all the prooflite scripts from a file called PRL-FILENAME into the theory THEORY.
  It assumes that the prl file exists and that theory is not nil."
  (let((at-least-one-script-saved? nil))
    (with-open-file 
     (file prl-filename :direction :input)
     (do ((str (read-line file nil))
	  (formulas)
	  (script))
	 ((null str))
       (let ((proofcomment (if plain-script?
			       (pregexp-replace "\%.*" (trim-left str) "")
			     (is-proof-comment str))))
	 (cond (proofcomment
		(let* ((proof     (match-proof proofcomment))
		       (formula   (car proof))
		       (qed       (match-qed proofcomment)))
		  (cond 
		   (formula
		    (when script
		      (pvs-error
		       "Prooflite script error"
		       (format
			nil
			"QED is missing in proof script~:[~;s~] ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} [Theory: ~a]"
			(cadr formulas)
			formulas theory)))
		    (setq str (format nil "~a" (cdr proof)))
		    (setq formulas (cons formula 
					 (when (not script) formulas)))
		    (setq script nil))
		   (qed
		    (let ((newscript (new-script script qed))) 
		      (when (and newscript formulas)
			(install-script theory newscript 
					(reverse formulas) 
					force
					t
					nil)
			(setq at-least-one-script-saved? t))
		      (cond ((not formulas) 
			     (pvs-message 
			      "No script was installed [Theory: ~a]" 
			      theory)
			     (setq str nil))
			    (t
			     (setq str (read-line file nil))
			     (setq formulas nil)
			     (setq script nil)))))
		   (t (setq str (read-line file nil))
		      (setq script (new-script script proofcomment))))))
	       ((is-comment str)
		(setq str (read-line file nil)))
	       (t 
		(when formulas
		  (pvs-error
		   "Prooflite script error"
		   (format
		    nil
		    "QED is missing in proof script~:[~;s~] ~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~} [Theory: ~a]"
		    (cadr formulas)
		    formulas theory)))
		(setq str (read-line file nil))
		(setq formulas nil)
		(setq script nil)))))
     (when at-least-one-script-saved? (with-context theory (save-all-proofs))))))

(defun then-prooflite (script)
  (cond ((and
	  (cdr script)
	  (null (cddr script))
	  (listp (caadr script)))
	 (list (list 'spread
		     (car script)
		     (mapcar #'(lambda (s) (to-prooflite (cdr s)))
			     (cadr script)))))
	(script
	 (cons (car script) (then-prooflite (cdr script))))))

(defun to-prooflite (script)
  (cond ((null (cdr script))
	 (car script))
	((and (null (cddr script))
	      (listp (caadr script)))
	 (car (then-prooflite script)))
	(t
	 (cons 'then (then-prooflite script)))))

(defun prooflite-script (fdecl)
  (when fdecl
    (if (justification fdecl)
	(to-prooflite (cdr (editable-justification
				    (justification fdecl))))
      (list 'postpone))))
			
(defun proof-to-prooflite-script (name line)
  (let* ((fdecl (formula-decl-to-prove name nil line "pvs")))
    (when fdecl
      (pvs-buffer
       "ProofLite"
       (with-output-to-string
	 (out)
	 (write-prooflite-script fdecl out)))
      (list (aref (place fdecl) 2)))))

(defun find-formula (theory-name formula)
  (let ((theory (get-typechecked-theory theory-name)))
    (when theory
      (find-if #'(lambda (d) (and (formula-decl? d)
				  (string= (format nil "~a" (id d))
					   formula)))
	       (all-decls theory)))))

(defun all-decl-names (theory-name)
  (let ((theory (get-typechecked-theory theory-name)))
    (when theory
      (mapcar #'(lambda (d) (format nil "~a" (id d)))
	      (remove-if-not #'(lambda (d) (formula-decl? d))
			     (all-decls theory))))))
  
(defun display-prooflite-script (theory formula)
  (let* ((fdecl (find-formula theory formula)))
    (when fdecl
      (pvs-buffer
       "ProofLite"
       (with-output-to-string
	 (out)
	 (write-prooflite-script fdecl out))))))

(defun write-prooflite-script (fdecl &optional (outstream t))
  (format outstream "~a : PROOF~%" (id fdecl))
  (write (prooflite-script fdecl)
	 :stream outstream :pretty t :escape t
	 :level nil :length nil
	 :pprint-dispatch *proof-script-pprint-dispatch*)
  (format outstream "~%QED ~a~%" (id fdecl)))

(defun get-prooflite-file-name (theory)
  (format nil "~a.prl" (id theory)))

(defun write-all-prooflite-scripts-to-file (theoryname)
  "Writes the proofscripts of all declarations in the theory (including TCCs)  into a file called \"<filename>__<theoryname>.prl\", where <filename> is the name of the file where the theory is defined. This function overwrites the \"prl\" file if it already exists. It returns the filename of the produced file on success and nil on error."
  (let((theory (get-typechecked-theory theoryname)))
    (if theory
	(let ((prl-filename (get-prooflite-file-name theory)))
	  (pvs-message "Storing proofs from theory \"~a\" into file \"~a\"." (id theory) prl-filename)
	  (handler-case
	   (with-open-file
	    (outs prl-filename
		  :direction :output
		  :if-exists :supersede
		  :if-does-not-exist :create)
	    (loop for d in (all-decls theory)
		  when (and (formula-decl? d) (not (or (axiom? d) (assumption? d))))
		  do (write-prooflite-script d outs)
		  finally (return prl-filename)))
	   (file-error
	    (e)
	    (pvs-error "File error" (format nil "Error: Could not write into ~a.~%" (file-error-pathname e)))
	    nil)))
      (pvs-error "Theory not found" (format nil "Error: Theory ~a not found in context.~%" theoryname)))))

(defun get-default-proof-script (theory-name formula)
  (let ((formula-declaration (find-formula theory-name formula)))
    (when (and formula-declaration (justification formula-declaration))
      (format t "~a" (get-proof-script-output-string formula-declaration nil))))
  (values))
