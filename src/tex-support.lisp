;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tex-support.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Nov 11 17:52:20 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 22 12:52:46 1999
;; Update Count    : 10
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;; --------------------------------------------------------------------

(in-package :pvs)

;#+allegro
;(import '(excl::string-output-stream excl::string-input-stream))

;;; The rudiments of a table lookup for mapping of ids and functions.

(defvar *latex-program* "lualatex")

;;; Must be filled in by user.  
(defvar *latex-id-length-list* nil 
  "records length of translated ids, including function names when
without args, for instance in proof instances or exporting clauses")

(defvar *latex-id-macro-list* nil
  "if want id mapped to something other than \id which is the default,
put the macro here.")

;;;   If fooooo(a, b) -> foo^a(b), then ('|fooooo| . 3) would be on this list.
(defvar *latex-fun-sym-length-list* nil
  "records length of translated function applications and identifiers.")

(defvar *latex-fun-sym-macro-list* nil
  "by default, id(arg, arg2) maps to \idfn{arg}{arg2}.  If want id to
map to macro instead of \idfn, put ((id . arity) . macro) here.")

(defvar *latex-newcommands-list* nil
  "Goes at head of latex ouptut, contains defintions of \idfn for
translated applications etc.  Need to double up backslashes to prevent
lisp from doing escapes")

(defvar *insert-newcommands-into-output* t
  "Can suppress insertion of newcommands into each tex file generated,
useful if more than one specification is to be included in one document")

(defvar *latex-keyword-list* nil
  "records latex translations of pvs keywords")

(defvar *latex-keyword-length-list* nil
  "records length of translated keywords applications")

(defvar *latex-files* nil "The list of files printed during this run.")

(defvar *latex-substitutions* nil
  "The list of substitution files generated during this run.")


;;; Called by Emacs

(defun latex-prelude (&optional dir)
  (let ((pdir (if dir
		  (pathname dir)
		  (make-pathname :defaults *default-pathname-defaults* :name "prelude-tex")))
	(mkdir-err nil))
    (unless (file-exists-p pdir)
      (multiple-value-bind (success? err)
	  (ignore-file-errors
	   #+allegro (excl:make-directory pdir)
	   #+cmu (unix:unix-mkdir (namestring pdir) #o777)
	   #+sbcl (sb-unix:unix-mkdir (namestring pdir) #o777))
	(declare (ignore success?))
	(setq mkdir-err err)))
    (cond (mkdir-err
	   (pvs-message "~a could not be created: ~a" mkdir-err))
	  ((not (directory-p pdir))
	   (pvs-message "~a is not a directory" pdir))
	  ((not (write-permission? pdir))
	   (pvs-message "You do not have write permission in ~a" dir))
	  (t (let ((orig-dir (working-directory))
		   ;;(*default-pathname-defaults* dir)
		   )
	       (unwind-protect
		    (progn (set-working-directory dir)
			   (latex-print-theories *prelude-theories*
						 dir)
			   (pvs-message
			       "Created LaTeX files for prelude in ~a ~
                         ~%pvsfiles.tex can be run with lualatex or xelatex in that directory"
			     dir))
		 (set-working-directory orig-dir)))))))

(defun latex-theories (theories &optional show-subst main-file-name (target-dir (namestring (working-directory))))
  (if (not (write-permission?))
      (pvs-error "LatexFile error" "You do not have write permission")
      (when theories
        (ensure-directories-exist target-dir)
	(let ((main-pathname (latex-print-theories theories show-subst main-file-name target-dir)))
	  (pvs-message "Created LaTeX files - they are included in ~a" main-pathname)
	  main-pathname))))

(defun latex-theory (theoryref &optional show-subst)
  (if (typechecked? theoryref)
      (with-theory (theory) theoryref
    	(latex-theories (list theory)
			show-subst
			(format nil "~a#~a" (filename theory) (id theory))
		        (format nil "~apvstex/" (namestring (path *workspace-session*)))))
      (pvs-error "LatexFile error" (format nil "Theory ~a is not typechecked" theoryref))))

(defun latex-pvs-file (fileref &optional show-subst)
  (with-pvs-file
      (filename) fileref
      (if (typechecked-file? filename)
	  (latex-theories (get-theories filename)
			  show-subst
			  (format nil "~a#" (pathname-name filename))
			  (format nil "~apvstex/" (namestring (path *workspace-session*))))
	  (pvs-error "LatexFile error" (format nil "~a is not typechecked" filename)))))

(defun latex-usingchain (theoryref &optional show-subst)
  (if (typechecked? theoryref)
      (with-theory (theory) theoryref
	 (latex-theories (collect-theory-usings theory nil)
			 show-subst
			 (format nil "~a#~a.importchain" (filename theory) (id theory))
			 (format nil "~apvstex/" (namestring (path *workspace-session*)))))
      (pvs-error "LatexFile error" (format nil "Theory ~a is not typecheked" theoryref))))

(defun latex-print-theories (theories &optional show-subst main-file-name target-dir)
  (get-tex-substitutions show-subst)
  (maplist #'(lambda (thlist)
	       (latex-print-theory (car thlist) show-subst (cadr thlist) target-dir))
	   theories)
  (latex-print-finish main-file-name target-dir))

(defvar *latex-linelength* 100)

(defun latex-print-theory (theory &optional gen-subst next target-dir)
  (let ((file (make-pathname :directory target-dir
			     :name (string (id theory)) :type "tex"))
	(*latex-id-length-list* *latex-id-length-list*)
	(*latex-id-macro-list* *latex-id-macro-list*)
	(*latex-fun-sym-length-list* *latex-fun-sym-length-list*)
	(*latex-fun-sym-macro-list* *latex-fun-sym-macro-list*)
	(*latex-keyword-list* *latex-keyword-list*)
	(*latex-keyword-length-list* *latex-keyword-length-list*)
	(*latex-newcommands-list* *latex-newcommands-list*)
	(*show-conversions* nil)
	(old-ids *latex-id-macro-list*))
    (when (and (filename theory)
	       (not (eq (intern (filename theory) :pvs) (id theory))))
      (read-tex-substitutions (working-directory) (intern (filename theory) :pvs)))
    (read-tex-substitutions (working-directory) (id theory))
    (read-tex-substitutions (working-directory) (id theory))
    (push (string (id theory)) *latex-files*)
    (multiple-value-bind (v condition)
	(ignore-errors
	  (with-open-file (stream file :direction :output :if-exists :supersede)
	    (when *insert-newcommands-into-output*
	      (dolist (nc *latex-newcommands-list*)
		(princ nc stream)
		(terpri stream)))
	    (format stream "\\begin{alltt}~%")
	    (pp-tex theory stream)
	    (format stream "\\end{alltt}~%")))
      (declare (ignore v))
      (when condition
	(pvs-error "LaTeX generation error"
	  (format nil "~a" condition))))
    (when gen-subst
      (generate-latex-chart (string (id theory))
			    (format nil "~a-sub" (id theory))
			    (ldiff *latex-id-macro-list* old-ids)))
    (pvs-message
	"Generated ~a~:[~;~:* - generating ~a~]"
      (shortname file) (when next (id next)))))


(defun latex-print-finish (&optional (top-filename "pvs-files") target-dir (supersede? t))
  (let ((top-pathname (make-pathname :directory target-dir
				     :name top-filename :type "tex")))
    (if (and (not supersede?) (probe-file top-pathname))
	(let ((lines (with-open-file (stream top-pathname)
		       (loop for line = (read-line stream nil)
			     while line
			     collect line))))
	  (with-open-file (stream top-pathname :direction :output :if-exists :supersede)
	    (loop for line in (butlast lines)
		  do (write-line line stream))
	    (format stream "~:{\\markright{~a}\\label{~a}~:*\\input{~a}\\newpage
~}
\\end{document}" (mapcar #'(lambda (name)
					  (let ((fname (pathname-name name)))
					    (list (latex-protect fname) fname)))
				      (reverse *latex-files*)))))
	(with-open-file (mods top-pathname
			      :direction :output
			      :if-exists :supersede
			      :if-does-not-exist :create)
	  
	  (format mods "~
% Automatically generated by PVS ~a
\\documentclass[12pt]{article}
\\usepackage{alltt}
\\usepackage{fullpage}
\\usepackage{supertabular}
\\usepackage{~a/pvs}

\\usepackage{amssymb}
\\usepackage{fontspec}
\\usepackage[math-style=ISO]{unicode-math}
\\setmainfont[Numbers=OldStyle, Ligatures=TeX]{TeX Gyre Termes}
\\setmathfont[Numbers=Lining]{TeX Gyre Termes Math}
\\setmathfont[range=\\bigstar]{XITS Math}
%\\setmathfont[range=\\leadsto]{XITS Math}
%\\setmathfont[range=\\int]{TeX Gyre Termes Math} % just to get parameters from this font
\\begin{document}
~{\\input{~a}~%~}
~:{\\markright{~a}\\label{~a}~:*\\input{~a}\\newpage
~}
\\end{document}
"
		  *pvs-version*
		  (environment-variable "PVSPATH")
		  *latex-substitutions*
		  (mapcar #'(lambda (name)
			      (let ((fname (pathname-name name)))
				(list (latex-protect fname) fname)))
			  (reverse *latex-files*)))))
    top-pathname))

(defun generate-latex-chart (name fname ids)
  (when ids
    (push fname *latex-substitutions*)
    (with-open-file (chart (format nil "~a.tex" fname)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (format chart "~
% Automatically generated by PVS.

~{~a~%~}

\\tablehead{\\hline Identifier&Translation\\\\ \\hline}
\\tabletail{\\hline}
\\bottomcaption{Translations for Identifiers Used in the Specification\\label{~a-trans}}
\\begin{supertabular}{|l|l|}
~:{\\verb|~a| & $~a$\\\\~%~}\\end{supertabular}
"
	      *latex-newcommands-list*
	      name
	      (mapcar #'(lambda (c) (list (car c) (cdr c))) ids)))))

(defun latex-protect (name)
  (let* ((nstring (cond ((stringp name)
			 name)
			((symbolp name)
			 (string name))
			(t (coerce name 'string))))
	 (len (length nstring)))
    (labels ((latex-protect* (string pos result)
	       (if (< pos len)
		   (latex-protect*
		    string (1+ pos)
		    (let ((ch (char string pos)))
		      (case ch
			((#\# #\$ #\% #\& #\_ #\{ #\})
			 (cons ch (cons #\\ result)))
			;; \ ==> \char92
			(#\\ (append '(#\2 #\9 #\r #\a #\h #\c #\\) result))
			;; ^ ==> \char94
			(#\^ (append '(#\4 #\9 #\r #\a #\h #\c #\\) result))
			;; ~ ==> \char126
			(#\~ (append '(#\6 #\2 #\1 #\r #\a #\h #\c #\\) result))
			(t (cons ch result)))))
		   (coerce (nreverse result) 'string))))
      (latex-protect* nstring 0 nil))))

(defun get-tex-substitutions (gen-subst)
  (setq *latex-newcommands-list* nil
	*latex-keyword-list* nil
	*latex-keyword-length-list* nil
	*latex-id-macro-list* nil
	*latex-id-length-list* nil
	*latex-fun-sym-macro-list* nil
	*latex-fun-sym-length-list* nil)
  (read-tex-substitutions (directory-p (environment-variable "PVSPATH")))
  (read-tex-substitutions "~/")
  (setq *latex-substitutions* nil)
  (let ((old-ids *latex-id-macro-list*))
    (read-tex-substitutions (working-directory))
    (when gen-subst
      (generate-latex-chart "pvs-tex"
			    "pvs-tex"
			    (ldiff *latex-id-macro-list* old-ids))))
  (setq *latex-files* nil))

(defun read-tex-substitutions (path &optional (name "pvs-tex"))
  (let ((file (merge-pathnames path (format nil "~a.sub" name))))
    (when (file-exists-p file)
      (let ((newsubs nil))
	(let ((*latex-newcommands-list* nil))
	  ;;(push "\\setlength{\\fboxsep}{1pt} " *latex-newcommands-list*)
	  (with-open-file (str file)
	    (read-tex-subs str))
	  (push (format nil "%   ~a" (namestring file))
		*latex-newcommands-list*)
	  (push "% The following substitutions are from the file:"
		*latex-newcommands-list*)
	  (setq newsubs *latex-newcommands-list*))
	(setq *latex-newcommands-list*
	      (nconc *latex-newcommands-list* newsubs))))))

(defun read-tex-subs (str)
  (let ((rline (read-line str nil 'eof)))
    (unless (eq rline 'eof)
      (let ((line (string-left-trim '(#\space #\tab) rline)))
	(if (string= line "---")
	    (read-inline-subst str)
	    (progn
	      (unless (or (string= line "")
			  (char= (char line 0) #\%))
		(parse-tex-subs line))
	      (read-tex-subs str)))))))

(defun read-inline-subst (str &optional result)
  (let ((line (read-line str nil 'eof)))
    (if (eq line 'eof)
	(setq *latex-newcommands-list*
	      (revappend result *latex-newcommands-list*))
	(read-inline-subst str (cons line result)))))

(defun parse-tex-subs (line)
  (multiple-value-bind (id type width tex)
      (parse-tex-line line)
    (when id
      (unless (and type width)
	(pvs-error "TeX substitution error"
		    (format nil
			    "Incomplete substitution defined for ~a" id)))
      (let ((nwidth (if (string= width "-")
			(length id)
			(parse-integer width :junk-allowed t))))
	(unless nwidth
	  (pvs-error "TeX substitution error"
		      (format nil
			      "Illegal length ~a for ~a" width id)))
	(unless (plusp nwidth)
	  (pvs-error "TeX substitution error"
		      (format nil
			      "Illegal length ~a for ~a:~% must be a positive integer" width id)))
	(cond
	  ((string-equal type "key")
	   (let ((sym (intern (string-upcase id) :pvs)))
	     (when tex (push (cons sym tex) *latex-keyword-list*))
	     (push (cons sym nwidth) *latex-keyword-length-list*)))
	  ((or (string-equal type "id")
	       (string= type "0"))
	   (let ((sym (intern id :pvs)))
	     (when tex (push (cons sym tex) *latex-id-macro-list*))
	     (push (cons sym nwidth) *latex-id-length-list*)))
	  ((or (string-equal type "id[" :end1 (min (length type) 3))
	       (string= type "[" :end1 1))
	   (let* ((sym (intern id :pvs))
		  (arity (list (parse-integer type :junk-allowed t
					      :start (1+ (position #\[ type)))))
		  (fn (cons sym arity)))
	     (when tex
	       (if (some #'(lambda (c) (char= c #\#)) tex)
		   (let ((name (generate-tex-name id arity "id")))
		     (push (cons fn name) *latex-id-macro-list*)
		     (push (format nil
			       "\\def~a~{#~a~}{~a}% How to print name ~a with ~a actuals"
				   name
				   (loop for x from 1 to (apply #'+ arity)
					 collect x)
				   tex ;;(string-trim '(#\{ #\}) tex)
				   id arity)
			   *latex-newcommands-list*))
		   (push (cons fn tex) *latex-id-macro-list*))
	       (push (cons fn nwidth) *latex-id-length-list*))))
	  (t
	   (let ((arity (read-from-string type)))
	     (when (integerp arity) (setq arity (list arity)))
	     (unless (and (consp arity)
			  (null (cdr (last arity)))
			  (every #'(lambda (e)
				     (and (integerp e) (plusp e)))
				 arity))
	       (pvs-error "TeX substitution error"
		 (format nil
		   "Illegal type or arity ~a for ~a: ~
                    must be KEY, ID, number, or list of numbers" arity id)))
	     (let* ((sym (intern id :pvs))
		    (fn (cons sym arity)))
	       (when tex
		 (if (some #'(lambda (c) (char= c #\#)) tex)
		     (let ((name (generate-tex-name id arity)))
		       (push (cons fn name) *latex-fun-sym-macro-list*)
		       (push (format nil
			       "\\def~a~{#~a~}{~a}% How to print function ~a with arity ~a"
			       name (loop for x from 1 to (apply #'+ arity)
					  collect x) tex id arity)
			     *latex-newcommands-list*))
		     (push (cons fn tex) *latex-fun-sym-macro-list*)))
	       (push (cons fn nwidth) *latex-fun-sym-length-list*)))))))))

(defun generate-tex-name (str arity &optional (end "fn"))
  (let ((op (cdr (assq (ref-to-id str) *pvs-operators*))))
    (if op
	(format nil "\\op~a~r~a" op (apply #'+ arity) end)
	(format nil "\\~a~a"
	  (apply #'concatenate 'string
		 (map 'list
		      #'(lambda (c)
			  (if (alpha-char-p c)
			      (string c)
			      (case c
				(#\0 "zero")
				(#\1 "one")
				(#\2 "two")
				(#\3 "three")
				(#\4 "four")
				(#\5 "five")
				(#\6 "six")
				(#\7 "seven")
				(#\8 "eight")
				(#\9 "nine")
				(#\_ "underscore")
				(otherwise "other"))))
		      (format nil "~a~a" str (apply #'+ arity))))
	  end))))
		       

(defun split-tex-line (line &optional (pos 0) all-the-rest)
  (let ((spos (position-if #'(lambda (c) (not (white-space c)))
			   line :start pos)))
    (when spos
      (cond (all-the-rest
	     (subseq line spos))
	    ((char= (char line spos) #\()
	     (let* ((pos (position #\) line :start spos))
		    (epos (when pos (1+ pos))))
	       (values (subseq line spos epos) epos)))
	    (t (let ((epos (position-if #'white-space line :start spos)))
		 (values (subseq line spos epos) epos)))))))

(defun parse-tex-line (line)
  (let ((id nil)
	(type nil)
	(width nil)
	(tex nil)
	(pos nil))
    (multiple-value-setq (id pos) (split-tex-line line))
    (when pos
      (multiple-value-setq (type pos) (split-tex-line line pos))
      (when pos
	(multiple-value-setq (width pos) (split-tex-line line pos))
	(when pos
	  (setq tex (split-tex-line line pos t)))))
    (when (string= id "o") (setq id "O"))
    (values id type width tex)))


;;; Latex for proofs, built from code of Shankar's

(defun latex-proof (texfile &optional terse? show-subst (main-filename "last-proof") (overwrite-main? t))
  (if *last-proof*
      (let ((latex-output-directory
	     (format nil "~apvstex/" (namestring (context-path (context *last-proof*))))))
	(ensure-directories-exist latex-output-directory)
	(let ((*report-mode* terse?)
	      (*current-context* (context *last-proof*))
	      (*tex-mode* t)
	      (texpath (when (or (stringp texfile) (pathnamep texfile))
			 (make-pathname :directory latex-output-directory
					:name (pathname-name texfile)
					:type "tex"))))
	  (get-tex-substitutions show-subst)
	  (read-tex-substitutions (working-directory) (id (current-theory)))
	  (cond ((stringp texfile)
		 (with-open-file (stream texpath :direction :output
					 :if-exists :supersede)
		   (latex-proof1 stream terse?)))
		(t (latex-proof1 t terse?)))
	  (let ((*latex-files* (list (subseq texfile 0
					     (search ".tex" texfile)))))
	    (let ((generated-file (latex-print-finish main-filename latex-output-directory overwrite-main?)))
	      (pvs-message "Created LaTeX proof file ~a - it is included in ~a.tex" texfile main-filename)
	      generated-file))))
      (pvs-message "Need to rerun the proof")))

(defun latex-proof1 (stream terse?)
  (mapc #'(lambda (newc) (format stream "~a~%" newc))
	*latex-newcommands-list*)
  (setq *latex-newcommands-list* nil)
  (format stream "~a proof for {\\tt ~a}.~2%"
    (if terse? "Terse" "Verbose")
    (latex-protect (label *last-proof*)))
  (latex-proof* *last-proof* stream terse? nil)
  (when (and (typep *last-proof* 'top-proofstate)
	     (eq (status-flag *last-proof*) '!))
    (format stream "~%~%Q.E.D.")))
  
;;; See  report-proof* (eproofcheck.lisp)
(defun latex-proof* (proofstate stream terse? &optional flag)
  (when (or (null terse?) (null flag))
    (latex-print proofstate stream))
  (when (current-rule proofstate)
    (format stream "~%Rule? ~VT~a~%"
      *prover-indent* (latex-protect (format nil "~s" (current-rule proofstate)))))
  (latex-proof-printout (printout proofstate) stream)
  (when (null (done-subgoals proofstate))
    (format stream "~%This completes the proof of {\\tt ~a}.~%"
      (latex-protect (label proofstate))))
  (let ((*print-ancestor* proofstate))
    (when (done-subgoals proofstate)
      (cond ((> (length (done-subgoals proofstate)) 1)
	     (format stream "~%~VTwe get ~a subgoals:~%"
	       *prover-indent* (length (done-subgoals proofstate)))
	     (loop for x in  (done-subgoals proofstate)
		   do (latex-proof* x stream terse?)))
	    (t (latex-proof* (car (done-subgoals proofstate)) stream
			     terse? t))))))

(defun latex-proof-printout (printout stream)
  (when printout
    (let ((string (if (consp printout)
		      (apply #'format nil (car printout)
			     (latex-proof-printout-exprs (cdr printout)))
		      (latex-protect printout))))
      (format stream "~a~%" string))))

(defmethod latex-proof-printout-exprs ((args list))
  (mapcar #'latex-proof-printout-exprs args))

(defmethod latex-proof-printout-exprs ((arg string))
  (if (string= arg "_")
      "\\rule{.1in}{.01in}"
      (latex-print (pc-parse arg 'expr) nil)))

(defmethod latex-proof-printout-exprs (arg)
  (if (eq arg '_)
      "\\rule{.1in}{.01in}"
      (latex-protect (format nil "~a" arg))))

(defun alltt-proof (file terse?)
  (with-open-file (*standard-output* file :direction :output
				     :if-exists :supersede)
    (let ((*report-mode* terse?)
	  (*top-proofstate* *last-proof*)
	  (*prover-indent* *prover-indent*)
	  (*disable-gc-printout* t))
      (report-proof* (non-strat-subgoal-proofstate *last-proof*))
      (when (and (typep *last-proof* 'top-proofstate)
		 (eq (status-flag *last-proof*) '!))
	(format t "~%Q.E.D.")))))
  

;;; The following methods were derived from the various print-objects in
;;; eproofcheck.lisp

(defmethod latex-print ((ps proofstate) stream)
  (let* ((*ps* ps)
	 (*print-ancestor* (if *print-ancestor*
			       *print-ancestor*
			       (parent-proofstate *ps*))))
    (assert (or (top-proofstate? ps) *print-ancestor*))
    (format stream "~%{\\tt ~a:}~2%\\vspace*{0.1in}\\hspace*{0.2in}~%~
                     \\begin{tabular}{|cl}~%"
      (latex-protect (label ps)))
    (latex-print (current-goal ps) stream)
    (format stream "~%\\end{tabular}~2%\\vspace{0.1in}~%")))

(defmethod latex-print ((sequent sequent) stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (let ((neg-s-forms (neg-s-forms sequent))
	  (pos-s-forms (pos-s-forms sequent)))
      (cond (neg-s-forms
	     (loop for sf in neg-s-forms as sfnum downfrom -1  
		   do (latex-sform sf sfnum stream))
	     (when (and *report-mode*
			(loop for sf in  neg-s-forms
			      thereis (memq sf par-sforms)))
	       (format stream "~%~VT & $\\vdots$ \\\\" *prover-indent*)))
	    (t (format stream "\\strut\\\\")))
      (format stream "\\hline~%")
      (cond (pos-s-forms
	     (loop for sf in pos-s-forms as sfnum from 1 
		   do (latex-sform sf sfnum stream))
	     (when (and *report-mode*
			(loop for sf in  pos-s-forms
			      thereis (memq sf par-sforms)))
	       (format stream "~% & $\\vdots$ \\\\")))
	    (t (format stream "\\strut\\\\"))))))

;;; See display-sform (eproofcheck.lisp)
(defun latex-sform (sform sfnum stream)
  (let ((par-sforms (when *print-ancestor*
		      (s-forms (current-goal *print-ancestor*)))))
    (if *report-mode*
	(unless (memq sform par-sforms)
	  (format stream "$\\{\\mbox{\\rm ~a}\\}$ & " sfnum)
	  (latex-print (seq-formula sform) stream))
	(let ((old (memq sform par-sforms)))
	  (format stream "$~a\\mbox{\\rm ~a}" (if old "[" "\\{") sfnum)
	  (when (label sform)
	    (format stream " (~{~a~^, ~})"
	      (mapcar #'latex-protect (label sform))))
	  (format stream "~a$ &" (if old "]" "\\}"))
	  (latex-print (seq-formula sform) stream)))))

(defmethod latex-print ((expr expr) stream)
  (cond (stream
	 (format stream "\\begin{minipage}[t]{5.5in}{\\begin{alltt}")
	 (pp-tex expr stream)
	 (format stream "\\end{alltt}}\\end{minipage}")
	 (format stream "\\\\"))
	(t (with-output-to-string (str)
	     (pp-tex expr str)))))

;;; 

(defun latex-theory-view (theoryname filename viewer &optional show-subst)
  (declare (ignore filename))
  (let ((main-pathname (latex-theory theoryname show-subst)))
    (let ((pdf-pathname (run-latex main-pathname)))
      (run-latex-viewer viewer pdf-pathname))))
  
(defun latex-proof-view (texfile viewer &optional terse? show-subst)
  (declare (ignore show-subst))
  (if *last-proof*
      (run-latex-viewer viewer (run-latex (latex-proof texfile terse?)))
      (pvs-message "No proof has been run yet")))

(defun file-contents (filename)
  (with-output-to-string (out)
    (with-open-file (in filename)
      (labels ((in2out ()
		 (let ((ch (read-char in nil 'eof)))
		   (unless (eq ch 'eof)
		     (write-char ch out)
		     (in2out)))))
	(in2out)))))

(defun latex-viewer ()
  #+(or macosx os-macosx) "open"
  #-(or macosx os-macosx) "evince")

(defun run-latex (tex-file-path)
  (unless *pvs-tmp-file* (set-pvs-tmp-file))
  (let ((tmp-file (funcall *pvs-tmp-file*))
	(tex-file-name (file-namestring tex-file-path))
	(tex-file-dir (directory-namestring tex-file-path)))
      (multiple-value-bind (out err status)
	  (uiop:run-program
	   (format nil "pushd ~a && ~a ~a ; popd"
		   tex-file-dir
		   *latex-program*
		   tex-file-name)
	   :input "/dev/null"
	   :output tmp-file
	   :error-output :output
	   :ignore-error-status t)
	(declare (ignore out err))
	(if (zerop status)
	    (make-pathname :name (pathname-name (pathname tex-file-name)) :type "pdf"
			   :directory tex-file-dir)
	    (let ((result (file-contents tmp-file)))
	      (delete-file tmp-file)
	      (pvs-error "LaTeX error" result))))))

(defun run-latex-viewer (latex-viewer pdf-pathname)
  (unless *pvs-tmp-file* (set-pvs-tmp-file))
  (let ((tmp-file (funcall *pvs-tmp-file*)))
    (multiple-value-bind (out err status)
	(uiop:run-program
	 (format nil "~a ~a" latex-viewer pdf-pathname)
	 :input "/dev/null"
	 :output tmp-file
	 :error-output :output
	 :ignore-error-status t)
      (declare (ignore out err))
      (unless (zerop status)
	(let ((result (file-contents tmp-file)))
	  (delete-file tmp-file)
	  (pvs-error "LaTeX error" result))))))
