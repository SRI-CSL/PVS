;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ergo-tex-fixes.lisp -- 
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 01:31:08 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Thu Mar 24 01:20:35 1994
;; Update Count    : 14
;; Status          : Beta test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fixes to allow latex pretty printing of applications.  Works for
;;; applications of type f(args), where f is an id, or for plain ids.
;;; Files changed are rt-format, rt-unp-top, 

(in-package 'sbrt)

;; Change if keyword font size changes.
(defvar *sb-keyword-expansion-factor* 1.5) 

(defvar *sb-tex-mode* nil
  "Used by various functions to check for tex-mode.")

;;; Defined as 0.6 in rt-unp-tex.lisp
(defparameter spaces-to-en 0.2
  "Conversion factor from spaces to TeX en units.")

(defvar *translating* nil
  "true if translation of a function application is in progress, so no
spaces are output (they mess up formatting).")

(defvar *translating-fn* nil
  "equal to the arity of id if next id(args) is to be translated")

(defvar *translation-stack* nil
  "top is true if innermost ( ) is being translated")

(defvar *untab-count* 0
  "Temporary count to collect untabs to place after the next cr.")  

(defvar *ignored-tabs* 0
  "records number of tabs ignored while translating function applications")

(defvar *holding-keyword* nil
  "keeps a keyword while waiting to see if it is followed by a space")

(defvar *holding-id* nil
  "keeps an id while waiting to see if it is followed by a space")

(defvar *pre-space* nil
  "t if ergo thinks a space should appear before the next keyword or
identifier.")

(defvar *post-space-used* nil
  "records whether ergo space has been included in a keyword or id, so
that it isn't included at the end of one token and the beginning of the
next.")

(defvar *ignore-next-space* nil
  "t if next space shouldn't be included in an id or keyword, for
instance when mapping an application")

(defvar *modnames* nil
  "A list of modnames, used to ensure that modnames don't get translated")

(defvar *translate-id* nil)

(defvar *pvs-major-version* nil)

(defvar *unparsing-tables* nil
  "The (nested) tables we are inside")

(defun map-aw-to-tex (aw)
  (let ((*escape-character* #\\)
	(*print-pretty* nil)		; To avoid weirdness.
	(*s* (make-string-output-stream))
	(*indent-sequence* nil)
	(*untab-count* 0)
	(*indent-count* 0)
	(*sb-tex-mode* t))
    (when pvs::*insert-newcommands-into-output*
      (mapc #'(lambda (newc) (format *s* "~A~%" newc)) 
	    pvs::*latex-newcommands-list*))
    (format *s* " \\begin{program} ~%")
    (let ((*translating* nil)
	  (*ignored-tabs* 0)
	  (*pre-space* nil)
	  (*holding-keyword* nil)
	  (*holding-id* nil)
	  (*translating-fn* nil)
	  (*translation-stack* nil)
	  (*num-keywords-skipped* -1)
	  (*modnames* nil)
	  (*unparsing-tables* nil)
	  (*pvs-major-version* (pvs::major-version))
	  (tex-keywords (make-hash-table :test #'eq))
	  (tex-ids (make-hash-table :test #'eq))
	  (tex-strings (make-hash-table :test #'equal)))
      (map-aw-to-tex-aux aw)
      (commit-held-tokens))
    ;;(format *s* " \\pvsnewline{}~%")
    (write-untabs) (write-indents)
    (unless (zerop *ignored-tabs*) (break))
    (format *s* " \\end{program}")
    (get-output-stream-string *s*)))


;;; Changed so that a mapped operator is prepended by a backslash,
;;; and so that ( or , or ) is mapped to braces in argument lists.
;;; Comes from rt-unp-tex.  Also fixed so that untabbing done right.
(defun map-aw-to-tex-aux (aw)
  (let* ((*escape-character* #\\)	; for TeX.
	 (pvs-term (pvs-term aw))
	 (translating-fn? (translating-fn? pvs-term))
	 (translating-acts? (translating-acts? pvs-term))
	 ;; when *translating* is true, no spaces are output.
	 (*translating* (unless (typep pvs-term 'pvs::declaration)
			  (or *translating*
			      translating-fn?
			      translating-acts?)))
	 ;;(*translating-fn* (if translating-acts?
		;;	       (cdar translating-acts?)
			;;       (or *translating-fn*
				;;   (cdar translating-fn?))))
	 )
    (when (and (is-sim-term (aw-term aw))
	       (is-sop 'pvs::theory (aw-term aw)))
      (format *s* "%~%%----- Start of declaration ~a~%"
	(ds-id (term-arg0 (term-arg0 (term-arg0 (aw-term aw))))))
      ;;(format t "~%% Start of declaration ~a~%"
	;;(ds-id (term-arg0 (term-arg0 (term-arg0 (aw-term aw))))))
      )
    (setq *translating-fn* (if translating-acts?
			       (cdar translating-acts?)
			       (or *translating-fn*
				   (cdar translating-fn?))))
    (let ((cmt (getf (term:term-attr (aw-term aw)) :comment)))
      (setq *newline-comments-to-output*
	    (append *newline-comments-to-output* cmt)))
    (dolist (son (aw-sons-form aw))
      (when (and (token-p son)
		 (member (token-kind son) '(:keyword :lt))
		 (member (token-subkind son) '(:id :identifier)))
	(collect-comments (aw-term aw))))
    (tex-buffered-whitespace-and-comments)
    (do ((sons (aw-sons-form aw) (cdr sons)))
	((null sons) aw)
      ;; first son is the function, needs to know to map id.
      ;; Later sons need to know to map '(' ',' ')' to braces.
      (if (aw-p (car sons))
	  (let ((*translate-id* (translate-id? (car sons))))
	    (map-aw-to-tex-aux (car sons)))
	  (let ((token (car sons)))
	    (assert (token-p token))
	    ;;(format t "~%Token-value = ~a" (token-value token))
	    (ecase (token-kind token)
	      (:keyword
	       (commit-held-tokens)
	       (case (token-value token)
		 ((sbst::|(| sbst::|[| sbst::|(#| sbst::|(:| sbst::|[\||)
		  (push *translating-fn* *translation-stack*)
		  (format *s* (cond (*translating-fn*
				     " {")
				    ((eq (token-value token) 'sbst::|(#|)
				     "(\\#")
				    (t (string (token-value token)))))
		  (setq *pre-space* nil)
		  (setq *translating-fn* nil))
		 ((sbst::|)| sbst::|]| sbst::|#)| sbst::|:)| sbst::|\|]|)
		  (format *s* (cond ((car *translation-stack*)
				     " }")
				    ((eq (token-value token) 'sbst::|#)|)
				     "\\#)")
				    (t (string (token-value token)))))
		  (setq *pre-space* nil)
		  (setq *translating-fn* (cdr (pop *translation-stack*))))
		 ((sbst::|,|)
		  (setq *pre-space* nil)
		  (when (car *translation-stack*)
		    (setq *ignore-next-space* t)) ; ignore spaces in
						  ; mapped applications
		  (format *s* (if (car *translation-stack*) "}{" " ,")))
		 ((sbst::TABLE)
		  (push pvs-term *unparsing-tables*)
		  (incf *num-keywords-skipped*)
		  (setq *holding-keyword* token))
		 ((sbst::ENDTABLE)
		  (pop *unparsing-tables*)
		  (incf *num-keywords-skipped*)
		  (setq *holding-keyword* token))
		 (t ;;(format t "~%Token-value = ~a" (token-value token))
		    (ecase (token-subkind token)
;		      ((:id :identifier)
;		       (setq *num-keywords-skipped* 0)
;		       (tex-buffered-whitespace-and-comments))
;		      ((:string :number)
;		       (incf *num-keywords-skipped*))
		      (:jux)
		      ((nil)
		       (when (or (member (intern (string (token-value token))
						 'pvs)
					 pvs::*infix-operators*)
				 (member (token-value token)
					 '(sbst::OF)))
			 (setq *pre-space* t))
		       (incf *num-keywords-skipped*)
		       (setq *holding-keyword* token))))))
	      (:lt
	       (commit-held-tokens)
	       (tex-buffered-whitespace-and-comments)
	       (ecase (token-subkind token)
		 ((:id :identifier)
;		  (format *s* "~A"
;			  (tex-id token))
		  (setq *holding-id* (list token *translate-id*))
		  (setq *num-keywords-skipped* 0))
		 ((:number)
		  (setq *pre-space* nil)
		  (format *s* " ~A"
			  (if (token-str-value token)
			      (token-str-value token)
			      (token-value token)))
		  (incf *num-keywords-skipped*))
		 ((:string)
		  (setq *pre-space* nil)
		  (format *s* " ~A"
			  (tex-string token))
		  (incf *num-keywords-skipped*))))
	      (:whitespace
	       (ecase (token-subkind token)
		 (:sp
		  (let* ((spaces1 (sb-pixels-to-chars (token-value token)))
			 (spaces (* spaces-to-en spaces1)))
		    (when (and (> spaces 0) (not *ignore-next-space*))
		      (setq *post-space-used* nil)
		      (commit-held-tokens t)
		      (unless *post-space-used* (setq *pre-space* t)))
		    (when *ignore-next-space* (setq *ignore-next-space* nil))))
;		    (if (and (> spaces 0) (not *translating*))
;			(format *s* " \\hspace{~Aem}" spaces))
		 (:cr
		  ;; changed here too -- SJP  We don't want to put
		  ;; formatting commands inside a mapped function application.
		  (commit-held-tokens)
		  (setq *pre-space* nil)
		  (unless (or *unparsing-tables*
			      *translating-fn*)
		    (dolist (fn *translation-stack*)
		      (when fn
			(format *s* "\\egroup")))
		    (format *s* " \\pvsnewline{}")
		    (commit-indents)
		    ;; changed here too -- SJP (indents must come after cr)
		    (write-untabs) (write-indents)
		    (dolist (fn *translation-stack*)
		      (when fn
			(format *s* "\\bgroup")))
		    (format *s* "~%")))
		 (:indent
		  (incf *indent-count*))
		 (:unindent
		  (decf *indent-count*))    
		 (:tab-left
		  (cond (*translating*
			 (incf *ignored-tabs*))
			(t (commit-indents)
			   (unless *unparsing-tables*
			     (format *s* "\\ii")))))
		 (:tab-right
		  (cond (*translating*
			 (incf *ignored-tabs*))
			(t (commit-indents)
			   (unless *unparsing-tables*
			     (format *s* "\\ii")))))
		 (:untab
		  (cond ((> *ignored-tabs* 0)
			 (decf *ignored-tabs*))
			(t (commit-indents)
			   (unless *unparsing-tables*
			     (incf *untab-count*)))))))))))))

(defun translate-id? (aw)
  (let ((term (aw-term aw)))
    (not (and (is-id term)
	      (getf (term:term-attr term) :no-translation)))))

(defun tex-buffered-whitespace-and-comments ()
  (when (and ;;*newline-seen-since-last-output*
	 *newline-comments-to-output*)
    (unless pvs::*no-comments*
      ;;(format t "~%num = ~a, cmts = ~a"
	;;*num-keywords-skipped* *newline-comments-to-output*)
      (dolist (cmt *newline-comments-to-output*)
	(when (= (second cmt) *num-keywords-skipped*)
	  (let ((indent (floor (or (car *indent*) 0))))
	    (format *s* "~%~:[~;\\pvsnewline{}~%~]\\pvscmt{~a}\\pvsnewline{}~%"
	      (third cmt)
	      (tex-protect (first cmt))))
	  (setq *newline-comments-to-output*
		(remove cmt *newline-comments-to-output*)))))
    ;;(setq *newline-comments-to-output* nil)
    ))

(defun tex-protect (string &optional (pos 0) result)
  (if (< pos (length string))
      (tex-protect string (1+ pos)
		   (let ((ch (char string pos)))
		     (case ch
		       ((#\# #\$ #\% #\& #\_ #\{ #\})
			(append `(,ch #\\) result))
		       ((#\~ #\^ #\\)
			(append `(#\| ,ch #\| #\b #\r #\e #\v #\\) result))
		       (t (cons ch result)))))
      (coerce (nreverse result) 'string)))

(defun pvs-term (aw)
  (when *sb-tex-mode*
    (when (is-sim-term (aw-term aw))
      (let* ((xt-fun (intern (format nil "XT-~:@(~a~)"
			       (sim-term-op (aw-term aw)))
			     'pvs)))
	(when (fboundp xt-fun)
	  (funcall xt-fun (aw-term aw)))))))

(defun translating-fn? (pvs-term &optional arity)
  (cond ((and (listp pvs-term)
	      (null (cdr pvs-term))
	      (typep (car pvs-term) '(or pvs::const-decl pvs::def-decl))
	      (pvs::formals (car pvs-term)))
	 (assoc (cons (pvs::id (car pvs-term))
		      (mapcar #'length (pvs::formals (car pvs-term))))
		pvs::*latex-fun-sym-length-list* :test #'equal))
	((and (typep pvs-term '(and pvs::application
				    (not (or pvs::if-expr
					     pvs::infix-application
					     pvs::unary-application))))
	      (zerop (pvs::parens pvs-term)))
	 (translating-fn? (pvs::operator pvs-term)
			   (cons (length (pvs::arguments pvs-term))
				 arity)))
	((and arity
	      (typep pvs-term 'pvs::name-expr))
	 (assoc (cons (pvs::id pvs-term) arity)
		pvs::*latex-fun-sym-length-list* :test #'equal))))

(defun translating-acts? (pvs-term)
  (when (and (typep pvs-term 'pvs::name)
	     (pvs::actuals pvs-term))
    (assoc (list (pvs::id pvs-term) (length (pvs::actuals pvs-term)))
	   pvs::*latex-id-macro-list* :test #'equal)))

;(defun no-translation? (pvs-term)
;  (typep pvs-term 'pvs::modname))      

(defun commit-held-tokens (&optional post-space)
  (tex-buffered-whitespace-and-comments)
  (when *holding-keyword*
    (format *s* "~A" (tex-keyword *pre-space* *holding-keyword* post-space))
    (setq *pre-space* nil)
    (setq *post-space-used* post-space)
    (setq *holding-keyword* nil))
  (when *holding-id*
    (format *s* "~A" (tex-id *pre-space* (car *holding-id*) post-space
			     (cadr *holding-id*)))
    (setq *pre-space* nil)
    (setq *post-space-used* post-space)
    (setq *holding-id* nil)))

(defun write-untabs ()
  (unless (or (zerop *untab-count*)
	      *unparsing-tables*)
    (write-pvsnewline-if-needed)
    (do ((i 0 (1+ i)))
	((>= i *untab-count*))
      (format *s* "\\oo"))
    (setq *untab-count* 0)))

(defun write-indents ()
  (unless *unparsing-tables*
    (let ((num (reduce #'+ *indent-sequence*)))
      (if (>= num 0)
	  (do ((i 0 (1+ i)))
	      ((>= i num))
	    (format *s* "\\zi"))
	  (do ((i 0 (1- i)))
	      ((<= i num))
	    (format *s* "\\zo")))
      (setq *indent-sequence* nil))))

(defun write-pvsnewline-if-needed ()
  (let* ((str (get-output-stream-string *s*))
	 (ln (length str)))
    (write-string str *s*)
    (unless (or *unparsing-tables*
		(and (>= ln 12)
		     (string= (subseq str (- ln 12) (1- ln))
			      "\\pvsnewline{}")))
      (format *s* "\\\\[-\\baselineskip]"))))

;;; print the id as a command if the id is on the list of mapped ids.
(defun tex-id (pre-space token post-space translate-id)
  ;;(format t "~%token = ~A, trans = ~a" (token-value token) translate-id)
  (or (and translate-id
	   (pvs-special-id-value token))
      (let ((id (or (gethash (token-value token) tex-ids)
		    (setf (gethash (token-value token) tex-ids)
			  (let* ((str1 (if (token-str-value token)
					   (token-str-value token)
					   (symbol-name (token-value token))))
				 (str (if (and (not *case-sensitive*)
					       (eq :downcase *print-case*))
					  (string-downcase str1)
					  str1))
				 (*restricted-chars* TeX-restricted-chars))
			    (insert-escapes str))))))
	;; I have made ids print as variables in program mode, so that their
	;; font can be changed.
	(setq *post-space-used* t)
	(format nil " \\~A{~A~A~A}"
	  (if (member id '("TRUE" "FALSE") :test #'equal)
	      "pvskey" "pvsid")
	  (if pre-space " " "")
	  id
	  (if post-space " " "")))))
  
(defun pvs-special-id-value (token)	;returns nil if token isn't special.
  (when *sb-tex-mode*
    (if *translating-fn*
	(let* ((fn (cons (token-value token) *translating-fn*))
	       (expansion (pvs-function-translation fn token)))
	  ;; add space before macro expansion
	  ;;(setq *translating-fn* (cdr (pop *translation-stack*)))
	  (when expansion (format nil " ~A" expansion)))
	(let* ((tv (token-value token))
	       (tvs (string tv)))
	  (cond
	   ((assoc tvs pvs::*latex-id-length-list*
		   :test  #'(lambda (x y)
			      (and (symbolp y) (string= x (string y)))))
	    (format nil "{~A}"
	      (cdr (assoc tvs pvs::*latex-id-macro-list*
			  :test #'(lambda (x y)
				    (and (symbolp y)
					 (string= x (string y))))))))
	   ((assoc tv pvs::*latex-id-length-list*)
	    (format nil " ~A"		; add space before macro expansion
	      (or (cdr (assoc tv pvs::*latex-id-macro-list*))
		  ;; no macro, add backslash to id to make default.
		  (format nil "\\~A" (symbol-name tv)))))
	   (t (multiple-value-bind (name sub bang dollar)
		  (break-pvs-name tvs)
		(let* ((*restricted-chars* TeX-restricted-chars)
		       (ename (insert-escapes name)))
		  (if (= (length name) 1)
		      (format nil " ~a~@[_{~a}~]~@[^{~a}~]~@[^{~a}~]"
			ename sub
			(when bang (let ((string ""))
				     (dotimes (n bang)
				       (setq string
					     (concatenate 'string string
							  "\\prime")))
				     string))
			(when dollar
			  (format nil "\\fbox{$\\scriptscriptstyle ~d$}"
			    dollar)))
		      (format nil "\\pvsid{~a~@[\\\\_{~a}~]~@[^{~a}~]}~@[^{~a}~]"
			ename
			sub
			(when dollar
			  (format nil "\\fbox{$\\scriptscriptstyle ~d$}"
			    dollar))
			(when bang (let ((string ""))
				     (dotimes (n bang)
				       (setq string
					     (concatenate 'string string
							  "\\prime")))
				     string))))))))))))

(defun pvs-function-translation (fn token)
  (or (cdr (assoc fn pvs::*latex-fun-sym-macro-list* :test #'equal))
      (cdr (assoc fn pvs::*latex-id-macro-list* :test #'equal))
      ;; otherwise no macro, so add backslash, append
      ;; with 'fn' for default 
      (and (assoc fn pvs::*latex-fun-sym-length-list* :test #'equal)
	   (format nil "\\~Afn" (symbol-name (token-value token))))))

(defun break-pvs-name (string)
  (let* ((bang-pos (position #\! string))
	 (dollar-pos (position #\$ string))
	 (namestr (cond (bang-pos (subseq string 0 bang-pos))
			(dollar-pos (subseq string 0 dollar-pos))
			(t string)))
	 (subscr? (and (> (length namestr) 1)
		       (alpha-char-p (char namestr 0))
		       (every #'digit-char-p (subseq namestr 1))))
	 (name1 (if subscr?
		    (coerce (list (char namestr 0)) 'string)
		    namestr))
	 (sub (when subscr? (subseq namestr 1))))
    (values name1 sub
	    (when bang-pos (parse-integer (subseq string (1+ bang-pos))))
	    (when dollar-pos (parse-integer (subseq string (1+ dollar-pos)))))))

	  
;;; The following two functions are from rt-format.lisp.  The first
;;; notices when an application is seen, and corrects the width of the
;;; previous identifier if it is on *latex-fun-sym-length-list*.  The
;;; second does mapping of ids that aren't in function applications.

(defvar *last-id-or-opsym-tinfo* nil "keep the tinfo for the last
id-or-opsym in case we discover it is the operator in an application and
is to be mapped during TeXifying")

(defun set-width (tinfo)
  (when (and *sb-tex-mode*
	     (typep (uterm-term (tinfo-uterm tinfo)) 'pvs::application))
    ;; The last id-or-opsym was really the operator in an application.
    ;; We are TeXifying, so we now go back and check if it had a length
    ;; on the *latex-fun-sym-length-list*, and update its tinfo.  It
    ;; should be the token right before this one, so width information
    ;; will be calculated correctly.
    (let ((newlength
	   (cdr (assoc (cons (token-value (tinfo-son *last-id-or-opsym-tinfo*))
			     (length (term-args
				      (uterm-term (tinfo-uterm tinfo)))))
		       pvs::*latex-fun-sym-length-list* :test #'equal))))
      (when newlength
	  (setf (tinfo-width *last-id-or-opsym-tinfo*)
		(+ (if (>= (tinfo-index *last-id-or-opsym-tinfo*) 1)
		       (tinfo-width
			(aref *tinfo* (1- (tinfo-index
					   *last-id-or-opsym-tinfo*))))
		       0)
		   (cond ((tinfo-bp-spaces *last-id-or-opsym-tinfo*))
			 (t 0))			; last bp has no spaces.
		   newlength)))))

  (setf (tinfo-width tinfo)
	(+ (if (>= (tinfo-index tinfo) 1)
	       (tinfo-width (aref *tinfo* (1- (tinfo-index tinfo))))
	       0)
	   (cond ((tinfo-bp-spaces tinfo))
		 (t 0))			; last bp has no spaces.
	   (token-width (tinfo-son tinfo))))
  (when (and *sb-tex-mode*
	     (is-id (uterm-term (tinfo-uterm tinfo))))
    (setq *last-id-or-opsym-tinfo* tinfo)))

;;; The change here is to look up the length of mapped ids on the
;;; *latex-id-length-list*, and to include an expansion factor in
;;; keyword lengths.
(defun token-width (token)
  (declare (type token token))
  (ecase (token-kind token)
    (:keyword 
     (if (eq (token-subkind token) :jux)
	 0
	 (sb-chars-to-pixels
	  (or (pvs-special-token-width token)
;;; makes multi-symbol keywords seem longer if their typeface is bigger
	      (1+ (* (if *sb-tex-mode* *sb-keyword-expansion-factor* 1)
		     (1- (length (if (token-str-value token)
				     (token-str-value token)
				     (symbol-name (token-value token)))))))))))
    (:lt (ecase (token-subkind token)
	   ((:id :identifier)
	    (sb-chars-to-pixels
	     (or (pvs-special-id-width token)
		 (length (if (token-str-value token)
			     (token-str-value token)
			     (symbol-name (token-value token)))))))
	   (:string
	    (sb-chars-to-pixels (length (token-str-value token))))
	   (:number 
	    (sb-chars-to-pixels (length (format nil "~A"
						(token-value token)))))))
    (:whitespace
     (case (token-subkind token)
       (:sp (token-value token))
       (t 0)))))

(defun pvs-special-token-width (token) ;returns nil if token is not special
  (if *sb-tex-mode* 
      (cdr (assoc (token-value token)
		  pvs::*latex-keyword-length-list*))))

(defun pvs-special-id-width (token)	;returns nil if token is not special
  (if *sb-tex-mode* 
      (cdr (assoc (token-value token) 
		  pvs::*latex-id-length-list*))))


;;; Changed so that pvs keywords can be output nicely.  Produces
;;; \. foo . or \.foo. or \. foo. or \.foo . depending on whether ergo
;;; thinks there should be spaces before or after the keyword.

(defun tex-keyword (pre-space token post-space)
  (cond ((eq (token-value token) 'sbst::|&TABLE&|)
	 (let ((rowlen (length (car (pvs::table-entries
				     (car *unparsing-tables*))))))
	   (format nil "\\pvsnewline{}~%\\begin{array}{|~:[~;c||~]*{~d}{c|}}~
                        \\hline"
	     (pvs::row-headings (car *unparsing-tables*)) rowlen)))
	((eq (token-value token) 'sbst::|&STARTCOL&|)
	 (if (pvs::row-headings (car *unparsing-tables*))
	     "&" ""))
	((eq (token-value token) 'sbst::|&ENDCOL&|)
	 "\\\\ \\hline\\hline")
	((eq (token-value token) 'sbst::|&VBAR&|)
	 "&")
	((eq (token-value token) 'sbst::|&ENDROW&|)
	 "\\\\ \\hline")
	((eq (token-value token) 'sbst::|&ENDTABLE&|)
	 "\\end{array}\\pvsnewline{}")
	((assq (token-value token) pvs::*latex-keyword-list*)
	 (format nil " ~a~A~a"
	   (if pre-space "\\mbox{ }" "")
	   (cdr (assoc (token-value token) pvs::*latex-keyword-list*))
	   (if (and post-space
		    (not (memq (token-value token)
			       '(sbst::|&&&| sbst::|&&&&|))))
	       "\\mbox{ }" "")))
	(t (let ((keyword
		  (or (gethash (token-value token) tex-keywords)
		      (setf (gethash (token-value token) tex-keywords)
			    (let* ((str1 (or (token-str-value token)
					     (symbol-name (token-value token))))
				   (str (cond ((eq :downcase *print-case*)
					       (string-downcase str1))
					      ((eq :upcase *print-case*)
					       (string-upcase str1))
					      ((eq :capitalize *print-case*)
					       (string-capitalize str1)))))
			      (if (alpha-char-p (char str 0))
				  (let ((*restricted-chars*
					 (cons #\. TeX-restricted-chars)))
				    (insert-escapes str))
				  (let ((*restricted-chars* TeX-restricted-chars))
				    (special-fix (insert-escapes str)))))))))
	     (setq *post-space-used* t)
	     (format nil (if (alpha-char-p (elt keyword 0))
			     " \\pvskey{~A~A~A}"
			     " ~A~A~A")
	       (if (or pre-space
		       (member keyword '("THEN" "ELSE" "ELSIF" "ENDIF" "BY")
			       :test #'equal))
		   "\\mbox{ }" "")
	       keyword
	       (if post-space "\\mbox{ }" ""))))))

;;; reduce the spacing between tokens, ergo had this at 0.6.
(setq spaces-to-em 0.4)

(defun tex-string (token)
  (let* ((str (if (token-str-value token)
		  (token-str-value token)
		  (symbol-name (token-value token)))))
    (format nil " ~A" 
      (let ((*restricted-chars* nil))
	(insert-escapes str)))))
