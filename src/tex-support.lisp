;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tex-support.lisp -- 
;; Author          : Sam Owre
;; Created On      : Thu Nov 11 17:52:20 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Jan 22 12:52:46 1999
;; Update Count    : 10
;; Status          : Beta test
;; 
;; HISTORY
;; 13-Mar-1994		Sam Owre	
;;    Last Modified: Sun Mar 13 15:23:22 1994 #5 (Sam Owre)
;;    Fixed handling of actuals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package 'pvs)

;#+allegro
;(import '(excl::string-output-stream excl::string-input-stream))

;;; The rudiments of a table lookup for mapping of ids and functions.

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

(defun latex-theory (theoryname filename &optional show-subst)
  (let ((theory (get-theory theoryname)))
    (cond ((not *pvs-context-writable*)
	   (pvs-message "You do not have write permission in this context"))
	  ((and theory (typechecked? theory))
	   (latex-print-theories (list theory) show-subst))
	  (t (pvs-message "Theory ~a has not been typechecked" theoryname)))))

(defun latex-pvs-file (filename &optional show-subst)
  (cond ((not *pvs-context-writable*)
	 (pvs-message "You do not have write permission in this context"))
	((typechecked-file? filename)
	 (latex-print-theories (get-theories filename) show-subst))
	(t (pvs-message "File ~a has not been typechecked" filename))))

(defun latex-usingchain (theoryname filename &optional show-subst)
  (let ((theory (get-theory theoryname)))
    (cond ((not *pvs-context-writable*)
	   (pvs-message "You do not have write permission in this context"))
	  ((and theory (typechecked? theory))
	   (update-pvs-context)
	   (latex-print-theories (mapcar #'get-theory
					   (context-usingchain theoryname))
				 show-subst))
	  (t (pvs-message "Theory ~a has not been typechecked" theoryname)))))

(defun latex-print-theories (theories &optional show-subst)
  (get-tex-substitutions show-subst)
  (maplist #'(lambda (thlist)
	       (latex-print-theory (car thlist) show-subst (cadr thlist)))
	   theories)
  (latex-print-finish))

(defvar *latex-linelength* 100)

(defun latex-print-theory (theory &optional gen-subst next)
  (let ((file (make-pathname :defaults (working-directory)
			     :name (id theory) :type "tex"))
	(*latex-id-length-list* *latex-id-length-list*)
	(*latex-id-macro-list* *latex-id-macro-list*)
	(*latex-fun-sym-length-list* *latex-fun-sym-length-list*)
	(*latex-fun-sym-macro-list* *latex-fun-sym-macro-list*)
	(*latex-keyword-list* *latex-keyword-list*)
	(*latex-keyword-length-list* *latex-keyword-length-list*)
	(*latex-newcommands-list* *latex-newcommands-list*)
	(old-ids *latex-id-macro-list*))
    (read-tex-substitutions (working-directory) (id theory))
    (push (string (id theory)) *latex-files*)
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (when *insert-newcommands-into-output*
	(dolist (nc *latex-newcommands-list*)
	  (princ nc stream)
	  (terpri stream)))
      (format stream "\\begin{alltt}~%")
      (pp-tex theory stream)
      (format stream "\\end{alltt}~%"))
    (when gen-subst
      (generate-latex-chart (string (id theory))
			    (format nil "~a-sub" (id theory))
			    (ldiff *latex-id-macro-list* old-ids)))
    (pvs-message
	"Generated ~a~:[~;~:* - generating ~a~]"
      (shortname file) (when next (id next)))))


(defun latex-print-finish (&optional file)
  (with-open-file (mods (make-pathname :defaults *pvs-context-path*
				       :name "pvs-files" :type "tex")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create)
    (format mods "~
% Automatically generated by PVS ~a

\\documentclass[fleqn,12pt]{article}
\\usepackage{fullpage,supertabular,alltt,latexsym,~a/pvs}
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
  (let* ((nstring (if (stringp name)
		      name
		      (coerce name 'string)))
	 (len (length nstring)))
    (labels ((latex-protect* (string pos result)
	       (if (< pos len)
		   (latex-protect* string (1+ pos)
				   (case (char string pos)
				     (#\_ (append '(#\_ #\\) result))
				     (#\$ (append '(#\$ #\\) result))
				     (t (cons (char string pos)
					      result))))
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
  (let ((line (read-line str nil 'eof)))
    (unless (eq line 'eof)
      (if (string= line "---")
	  (read-inline-subst str)
	  (progn
	    (parse-tex-subs line)
	    (read-tex-subs str))))))

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
	(cond
	  ((string-equal type "key")
	   (let ((sym (intern (string-upcase id))))
	     (when tex (push (cons sym tex) *latex-keyword-list*))
	     (push (cons sym nwidth) *latex-keyword-length-list*)))
	  ((or (string-equal type "id")
	       (string= type "0"))
	   (let ((sym (intern id)))
	     (when tex (push (cons sym tex) *latex-id-macro-list*))
	     (push (cons sym nwidth) *latex-id-length-list*)))
	  ((or (string-equal type "id[" :end1 (min (length type) 3))
	       (string= type "[" :end1 1))
	   (let* ((sym (intern id))
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
	     (let* ((sym (intern id))
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
		       

(defun split-tex-line (line &optional (pos 0) &key all-the-rest)
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

(defun white-space (c)
  (member c '(#\Space #\Tab)))

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
	  (setq tex (split-tex-line line pos :all-the-rest t)))))
    (when (string= id "o") (setq id "O"))
    (values id type width tex)))


;;; Latex for proofs, built from code of Shankar's

(defun latex-proof (texfile &optional terse? show-subst)
  (if *last-proof*
      (let ((*report-mode* terse?)
	    (*current-context* (context *last-proof*))
	    (*current-theory* (module (declaration *last-proof*)))
	    (*tex-mode* t)
	    (texpath (make-pathname :defaults (working-directory)
				    :name (pathname-name texfile)
				    :type "tex")))
	(get-tex-substitutions show-subst)
	(read-tex-substitutions (working-directory) (id *current-theory*))
	(cond ((stringp texfile)
	       (with-open-file (stream texpath :direction :output
				       :if-exists :supersede)
		 (latex-proof1 stream terse?)))
	      (t (latex-proof1 t terse?)))
	(let ((*latex-files* (list (subseq texfile 0
					   (search ".tex" texfile)))))
	  (latex-print-finish texfile))
	(pvs-message "Created LaTeX proof file ~a" texfile))
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
  

(defun latex-proof* (proofstate stream terse? &optional flag)
  (when (or (null terse?) (null flag))
    (latex-print proofstate stream))
  ;;  (when (current-rule proofstate)
  ;;    (format stream "~%~VT~a" *prover-indent* (current-rule
  ;;    proofstate)))
  (latex-proof-printout (printout proofstate) stream)
  (when (null (done-subgoals proofstate))
    (format stream "~%This completes the proof of {\\tt ~a}.~%"
      (latex-protect (label proofstate))))
  (let ((*print-ancestor*
	 (if (null flag) proofstate *print-ancestor*)))
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
  (latex-print (pc-parse arg 'expr) nil))

(defmethod latex-proof-printout-exprs (arg)
  (if (eq arg '_)
      "\\rule{.1in}{.01in}"
      (latex-protect (format nil "~a" arg))))

(defun alltt-proof (file terse?)
  (with-open-file (*standard-output* file :direction :output
				     :if-exists :supersede)
    (let ((*report-mode* terse?)
	  (*top-proofstate* *last-proof*)
	  (*prover-indent* *prover-indent*))
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
    (format stream "~%{\\tt ~a:}~2%\\vspace*{0.1in}\\hspace*{0.2in}~%~
                     \\begin{tabular}{|cl}~%"
      (latex-protect (label ps)))
    (latex-print (current-goal ps) stream)
    (format stream "~%\\end{tabular}~2%\\vspace{0.1in}~%")))

(defmethod latex-print ((sequent sequent) stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (let ((neg-s-forms (neg-s-forms (s-forms sequent)))
	  (pos-s-forms (pos-s-forms (s-forms sequent))))
      (cond (neg-s-forms
	     (loop for sf in neg-s-forms as sfnum downfrom -1  
		   do (latex-sform sf sfnum stream))
	     (when (and *report-mode*
			(loop for sf in  neg-s-forms
			      thereis (memq sf par-sforms)))
	       (format stream "~% & $\\vdots$ \\\\" *prover-indent*)))
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

(defun latex-sform (sform sfnum stream)
  (let ((par-sforms
	 (when *print-ancestor*
	   (s-forms (current-goal *print-ancestor*)))))
    (if *report-mode*
	(unless (memq sform par-sforms)
	  (format stream "$\\{\\mbox{\\rm ~a}\\}$ & " sfnum)
	  (latex-print (seq-formula sform) stream))
	(let ((old (memq sform par-sforms)))
	  (format stream "$~a\\mbox{\\rm ~a}" (if old "[" "\\{") sfnum)
	  (when (label sform)
	    (format stream "{\\tt ~a}" (latex-protect (label sform))))
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
  (let ((theory (get-theory theoryname)))
    (cond ((not *pvs-context-writable*)
	   (pvs-message "You do not have write permission in this context"))
	  ((and theory (typechecked? theory))
	   (latex-theory theoryname show-subst)
	   (let ((errstr (run-latex "pvs-files")))
	     (if errstr
		 (pvs-error "LaTeX error" errstr)
		 (run-latex-viewer viewer))))
	  (t (pvs-message "Theory ~a has not been typechecked" theoryname)))))

(defun latex-proof-view (texfile viewer &optional terse? show-subst)
  (cond (*last-proof*
	 (latex-proof texfile terse?)
	 (let ((errstr (run-latex "pvs-files")))
	   (if errstr
	       (pvs-error "LaTeX error" errstr)
	       (run-latex-viewer viewer))))
	(t (pvs-message "No proof has been run yet"))))

#+lucid
(defun run-latex-viewer (latex-viewer)
  (if latex-viewer
      (if (lucid::find-file-in-path latex-viewer)
	  (let ((status nil)
		(tmp-file (funcall *pvs-tmp-file*)))
	    (with-open-file (out tmp-file
				 :direction :output :if-exists :supersede)
	      (multiple-value-bind (stream errstream stat process-id)
		  (run-program latex-viewer
			       :arguments '("pvs-files")
			       :output out
			       :error-output out
			       :wait nil
			       )
		(declare (ignore stream errstream process-id))
		(setq status stat)))
	    (delete-file tmp-file))
	  (pvs-message "Cannot find ~a in your path." latex-viewer))
      (pvs-message "No viewer provided")))

#+lucid
(defun run-latex (filename)
  (unless *pvs-tmp-file* (set-pvs-tmp-file))
  (let ((status nil)
	(tmp-file (funcall *pvs-tmp-file*)))
    (with-open-file (out tmp-file
			 :direction :output :if-exists :supersede)
      (multiple-value-bind (stream errstream stat process-id)
	  (run-program "latex"
		       :arguments (list filename)
		       :input "//dev//null"
		       :output out)
	(declare (ignore stream errstream process-id))
	(setq status stat)))
    (let ((result (unless (zerop status)
		    (file-contents tmp-file))))
      (delete-file tmp-file)
      result)))

#+allegro
(defun run-latex-viewer (latex-viewer)
  (if latex-viewer
      (let ((status nil)
	    (tmp-file (funcall *pvs-tmp-file*)))
	(with-open-file (out tmp-file
			     :direction :output :if-exists :supersede)
	  (setq status
		(excl:run-shell-command
		 (format nil "~a pvs-files" latex-viewer)
		 :output out
		 :error-output :output
		 :wait nil)))
	(delete-file tmp-file))
      (pvs-message "No viewer provided")))

#+allegro
(defun run-latex (filename)
  (unless *pvs-tmp-file* (set-pvs-tmp-file))
  (let ((status nil)
	(tmp-file (funcall *pvs-tmp-file*)))
    (with-open-file (out tmp-file
			 :direction :output :if-exists :supersede)
      (setq status (excl:run-shell-command
		    (format nil "latex ~a" filename)
		    :input "//dev//null"
		    :output out
		    :error-output :output)))
    (let ((result (unless (zerop status)
		    (file-contents tmp-file))))
      (delete-file tmp-file)
      result)))

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
  "xdvi")
