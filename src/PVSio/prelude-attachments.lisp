;;
;; prelude-attachments.lisp
;; Release: PVSio-8.0 (08/04/2023)
;;
;; Contact: Cesar Munoz (cesar.a.munoz@nasa.gov)
;; NASA Langley Research Center
;; http://shemesh.larc.nasa.gov/people/cam/PVSio
;;
;; Copyright (c) 2011-2012 United States Government as represented by
;; the National Aeronautics and Space Administration.  No copyright
;; is claimed in the United States under Title 17, U.S.Code. All Other
;; Rights Reserved.
;;
;; Semantic attachments for PVSio standard library
;;

(in-package :pvs)

(decimals:define-decimal-formatter c (:round-magnitude -6) (:show-trailing-zeros t))
(decimals:define-decimal-formatter d (:round-magnitude -6))

(defun replace-string (string part replacement &key (test #'char=))
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

(defparameter *ol-digits* '("0̅" "1̅" "2̅" "3̅" "4̅" "5̅" "6̅" "7̅" "8̅" "9̅"))

(defun replace-ol-digits (acc &optional (n 0) (ol *ol-digits*))
  (if (null ol) acc
    (replace-ol-digits (replace-string acc (format nil "~a" n) (car ol))
		       (1+ n) (cdr ol))))

(defun pp-decimal (str finp)
  (let ((pos (position #\. str)))
    (if pos
	(let* ((fixp (+ pos finp 1))
	       (pre  (subseq str 0 fixp))
	       (pos  (subseq str fixp (length str))))
	  (format nil "~a~a" pre (replace-ol-digits pos)))
      str)))

(defun pp-rational (r)
  "Pretty prints rational numbers using overline to indicate repeating digits"
  (multiple-value-bind (finp infp)
      (decimal-precision-of-rat r)
    (let ((str (ratio2decimal-with-rounding-mode r 0 (+ finp infp) t)))
      (pp-decimal str finp))))

(defun stdstr-attachments ()

(eval '(attachments |stdstr|

(defattach |charcode| (n)
  "Char whose code is N"
  (format nil "~a" (code-char n)))

(defattach |chartable| ()
  "Standard char table"
  (or 
   (do ((i 0 (+ i 1)))
       ((>= i 128))
       (let ((c (code-char i)))
	 (when (and (graphic-char-p c)
		    (standard-char-p c))
	   (format t "~3d : ~a " i c))))
   t))
   
(defattach |newline| ()
  "New line"
  (format nil "~a" #\Newline))

(defattach |tab| ()
  "Tabular"
  (format nil "~a" #\Tab))

(defattach |spaces| (n)
  "N spaces"
  (make-string n :initial-element #\Space))

(defattach |upcase| (s)
  "Converts string S to uppercase"
  (string-upcase s))

(defattach |downcase| (s)
  "Converts string S to downcase"
  (string-downcase s))

(defattach |capitalize| (s)
  "Capitalizes string S"
  (string-capitalize s))

(defattach |strsub| (s i j)
  "If i <= j returns substring S[i..j]. Otherwise, returns substring
S[j..i].
NOTE: Name changed in PVS-8.0 from substr to substring to avoid clash
with charstring.substr"
  (cond ((and (<= 0 i) (<= i j) (< j (length s)))
	 (subseq s i (+ j 1)))
	((and (<= 0 j) (<= j i) (< i (length s)))
	 (reverse (subseq s j (+ i 1)))	 )
	(t "")))

(defattach |strreplace| (s part replacement)
  "Returns a new string in which all the occurences of part in s 
is replaced with replacement."
  (replace-string s part replacement))

(defattach |strfind| (s1 s2)
  "Index of leftmost occurrence of S1 in S2, starting from 0, or -1 if none"
  (or (search s1 s2 :test #'char=) -1))

(defprimitive |concat| (s1 s2)
  "Concatenates S1 and S2"
  (format nil "~a~a" s1 s2))

(defattach |rat2decstr_with_zeros| (r precision rounding zeros)
  "Converts rational number to string decimal representation using given precision, i.e., natural number n
denoting 10^(-n), and rounding mode, i.e, TowardsZero, TowardsInfnty, TowardsNegInfnty, TowardsPosInfnty.
Displays trailing zeroes when zeros is set to TRUE"
  (ratio2decimal-with-rounding-mode r rounding precision zeros))

(defattach |pp_rat| (r)
  "Pretty prints rational numbers using overline to indicate repeating digits"
  (pp-rational r))

(defattach |decstr2rat| (s)
  "Converts string representing a decimal number to rational number"
  (handler-case
      (decimals:parse-decimal-number s)
    (decimals:decimal-parse-error
     (condition)
     (declare (ignore condition))
     (throw-pvsio-exc "NotARealNumber" s))))

(defattach |str2real| (s)
  "Rational denoted by S"
  (handler-case
      (decimals:parse-decimal-number s)
    (decimals:decimal-parse-error
     (condition)
     (declare (ignore condition))
     (let ((n (read-from-string s)))
       (cond ((rationalp n) n)
	     ((floatp n) (rationalize n))
	     (t (throw-pvsio-exc "NotARealNumber" s)))))))

(defattach |str2int| (s)
  "Integer denoted by S"
  (let ((i (read-from-string s)))
    (if (integerp i) i 
      (throw-pvsio-exc "NotAnInteger" s))))

(defattach |number?| (s)
  "Tests if S denotes a number"
  (numberp (read-from-string s)))

(defattach |int?| (s)
  "Tests if S denotes an integer"
  (integerp (read-from-string s)))

(defattach |strcmp| (s1 s2 sensitive)
  "Returns 0 if s1 = s2, < 0 if s1 < s2, > 0 if s1 > s2. ~
   Comparison is case sensitive according to sensitive"
  (if sensitive
      (cond ((string= s1 s2) 0)
	    ((string< s1 s2) -1)
	    (t 1))
    (cond ((string-equal s1 s2) 0)
	  ((string-lessp s1 s2) -1)
	  (t 1))))
  
(defattach |strtrim| (s1 s2)
   "A substring of s2, with all the characters in s1 stripped of the beginning and end"
   (string-trim s1 s2))

(defattach |strtrim_left| (s1 s2)
   "A substring of s2, with all the characters in s1 stripped of the beginning"
   (string-left-trim s1 s2))

(defattach |strtrim_right| (s1 s2)
   "A substring of s2, with all the characters in s1 stripped of the end"
   (string-right-trim s1 s2))

(defattach |trim| (s)
   "A substring of s, with all the space characters stripped of the beginning and end"
   (string-trim '(#\Space #\Tab #\Newline) s))

(defattach |trim_left| (s)
   "A substring of s, with all the space characters stripped of the beginning"
   (string-left-trim '(#\Space #\Tab #\Newline) s))

(defattach |trim_right| (s)
   "A substring of s, with all the space characters stripped of the end"
   (string-right-trim '(#\Space #\Tab #\Newline) s))

)))

(defun prompt (s)
  (when (string/= s "")
    (format t "~a~%" s)
    (finish-output))
  (clear-input))

(defparameter *pvsio_length_str_stream* (make-hash-table))

(defun read-token (f s)
  (let* ((c (read-char f nil nil))
	 (stream
	  (when c
	    (do* ((str   (make-string-output-stream))
		  (start t))
		((not c) str)
	      (let ((p (when c (position c s)))
		    (m (when c (member c '(#\Space #\Tab #\Newline)))))
		(cond ((and start p)(write-char c str)
		       (return str))
		      (p (unread-char c f)
			 (return str))
		      ((and start m) (setq c (read-char f nil nil)))
		      (m (return str))
		      (t (setq start nil)
			 (write-char c str)
			 (setq c (read-char f nil nil)))))))))
    (when stream (get-output-stream-string stream))))

(defun stdio-attachments ()

(eval '(attachments |stdio|

(defprimitive |printstr| (s) 
  "Prints lisp format of string S"
  (not (format t "~a" s)))

(defattach |query_token| (mssg s)
  "Queries a token separated by characters in S from standard input with prompt MSSG"
  (prompt mssg)
  (format nil "~a" (read-token *standard-input* s)))

(defattach |query_line| (mssg) 
  "Queries a line from standard input with prompt MSSG"
  (prompt mssg)
  (format nil "~a" (read-line)))

(defattach |query_real| (mssg) 
  "Queries a real number from standard input with prompt MSSG"
  (prompt mssg)
  (let ((i (read)))
    (if (numberp i) (rational i)
      (throw-pvsio-exc "NotARealNumber" (format nil "~a" i)))))

(defattach |query_int| (mssg) 
  "Queries an integer from standard input with prompt MSSG"
  (prompt mssg)       
  (let ((i (read)))
    (if (integerp i) i 
      (throw-pvsio-exc "NotAnInteger" (format nil "~a" i)))))

(defattach |stdin| () 
  "Standard input stream"
  *standard-input*)

(defattach |stdout| ()
  "Standard output stream"
  *standard-output*)

(defattach |stderr| ()
  "Error output stream"
  *error-output*)

(defattach |fopenin_lisp| (s) 
  "Opens file input stream named S"
  (let ((f (open s :direction :input :if-does-not-exist nil)))
    (or f (throw-pvsio-exc "FileNotFound" s))))

(defattach |sopenin| (s) 
  "Opens string S as an input stream"
  (let ((str (make-string-input-stream s)))
    (setf (gethash str *pvsio_length_str_stream*) (length s))
    str))

(defattach |fopenout_lisp| (s i) 
  "Opens file output stream named S"
  (let ((f (cond ((= i 0) (open s :direction :output :if-exists nil))
		 ((= i 1) (open s :direction :output :if-exists :supersede))
		 ((= i 2) (open s :direction :output :if-exists :append))
		 ((= i 3) (open s :direction :output :if-exists :overwrite))
		 ((= i 4) (open s :direction :output :if-exists :rename)))))
    (or f (throw-pvsio-exc "FileAlreadyExists" s))))

(defattach |sopenout| (s) 
  "Opens string output stream"
  (let ((f (make-string-output-stream)))
    (format f s)
    f))

(defattach |fclose| (f) 
  "Closes stream F"
  (or (when (typep f 'file-stream)
	(close f))
      t))

(defattach |fexists| (s) 
  "Tests if file named S already exists"
  (and (probe-file s) t))

(defattach |fopen?| (f)
  "Tests if stream F is open"
  (open-stream-p f))

(defattach |strstream?| (f)
  "Tests if F is a string stream"
  (typep f 'string-stream))

(defattach |filestream?| (f)
  "Tests if F is a file stream"
  (typep f 'file-stream))

(defattach |finput?| (f)
  "Tests if F is an input string"
  (input-stream-p f))

(defattach |foutput?| (f)
  "Tests if F is an output string"
  (output-stream-p f))

(defattach |fname_lisp| (f)
  "Gets the name of a file stream"
  (namestring f))
  
(defattach |fgetstr_lisp| (f) 
  "Gets string from string output stream F"
  (if (typep f 'string-stream)
      (get-output-stream-string f)
    ""))

(defattach |eof_lisp| (f) 
  "Tests end of input stream F"
  (not (peek-char nil f nil nil)))

(defattach |flength_lisp| (f) 
  "Length of stream F"
  (cond ((typep f 'file-stream)   (file-length f))
	((typep f 'string-stream) 
	 (nth-value 0 (gethash f *pvsio_length_str_stream*)))
	(t 0)))

(defattach |fgetpos_lisp| (f) 
  "Gets current position of file stream F"
  (if (typep f 'file-stream) 
      (file-position f)
    0))

(defattach |fsetpos_lisp| (f n) 
  "Set current position of file stream F"
  (when (typep f 'file-stream)
    (cond ((<= n 0) (file-position f :start))
	  ((>= n (file-length f)) (file-position f :end))
	  (t (file-position f n)))))

(defattach |fprint_lisp| (f s) 
  "Prints S in stream F"
  (not (format f "~a" s)))

(defattach |fread_token_lisp| (f s) 
  "Reads a token from stream F separated by characters in S"
  (format nil "~a" (read-token f s)))

(defattach |fread_line_lisp| (f) 
  "Reads a line from stream F"
  (let ((s (read-line f nil nil)))
    (format nil "~a" s)))

(defattach |fread_real_lisp| (f)
  "Reads a real number from stream F"
  (let ((i (read f nil nil)))
    (when i 
      (if (numberp i) (rational i)
        (throw-pvsio-exc "NotARealNumber" (format nil "~a" i))))))

(defattach |fread_int_lisp| (f)
  "Reads an integer from stream F"
  (let ((i (read f nil nil)))
    (when i 
      (if (integerp i) i
        (throw-pvsio-exc "NotAnInteger" (format nil "~a" i))))))

(defattach |filename| (s)
  "Returns the name part of a file name"
  (file-namestring s))

(defattach |directory| (s)
  "Returns the directory part of a file name"
  (directory-namestring s))

)))

(defun rat2double (x) 
  (float x 1.0d0))

(defun stdmath-attachments ()

(eval '(attachments |stdmath|
	     
(defattach |RANDOM| ()
  "Real random number in the interval [0..1]"
  (rational (random (rat2double 1))))

(defattach |NRANDOM| (x)
  "Natural random number in the interval [0..X)"
  (random x))

(defattach |rational| (x)
  "Returns a rational number that is close to the real number (identity when input is rational)"
  (rational x))

(defprimitive |rat2numden| (r)
  "Returns numerator and denominator of rational number"
  (pvs2cl_tuple (numerator r) (denominator r)))

(defattach |decimal_precision| (r)
  "Compute the decimal precision of a rational number. Return 2 values the first one is number of
non-repeating decimals. The second one is the period of the decimal. If the second value is 0,
the rational has a finite decimal representation."
  (multiple-value-bind (finp infp)
      (decimal-precision-of-rat r)
    (pvs2cl_tuple finp infp)))

(defattach |finite_precision| (r)
  "Compute the finite decimal precision of a rational number. Return -1, it the rational number
doesn't have a finite representation. This function is the equivalent to the first projection of
decimal_precision, but it doesn't compute the period of the decimal, which is expensive."
  (finite-precision-of-rat r))

)))

(defstruct indent stack n prefix)

(defun stdindent-attachments ()

(eval '(attachments |stdindent|

(defattach |create_indent| (n s) 
  "Creates an ident structure with indentation N and prefix S"
  (make-indent :stack nil :n n :prefix s))

(defattach |pop_indent| (i)
  "Pops one element of indent I"
  (or (setf (indent-stack i) (cdr (indent-stack i))) t))

(defattach |top_indent| (i)
  "Top of I"
  (or (car (indent-stack i)) 0))

(defattach |push_indent| (i n) 
  "Pushes a N-indentation in indent I"
  (setf (indent-stack i)
	(cons (+ n (or (car (indent-stack i)) 0))
	      (indent-stack i))))

(defattach |get_indent| (i) 
  "Gets current indentation value of indent I"
  (indent-n i))
  
(defattach |set_indent| (i n) 
  "Sets a new indentation value to indent I"
  (setf (indent-n i) n))

(defattach |get_prefix| (i)
  "Gets prefix of indent I"
  (indent-prefix i))

(defattach |set_prefix| (i s) 
  "Sets prefix S to indent I"
  (setf (indent-prefix i) s))

)))

(defun formatargs (e type)
  (cond
   ((and (type-name? type)(equal (id type) '|Lisp|))
    (let ((ptype (find-supertype (type-value (car (actuals type))))))
      (cond ((and (listp e) (type-name? ptype) (equal (id ptype) '|list|))
	     (list (loop for ei in e
			 append (formatargs ei (type-value (car (actuals ptype)))))))
	    ((and (arrayp e) (tupletype? ptype))
	     (loop for ei across e
		   for ti in (types ptype)
		   append (formatargs ei ti)))
	    ((numberp e) (list (rat2double e)))
	    (t (list e)))))
   ((or (stringp e) (numberp e)) (list e))
   (t (list (cl2pvs e type)))))

(defun stdprog-attachments ()

(eval '(attachments |stdprog|

(defattach |exit| ()
  "Exits the current evaluation and returns to the ground evaluator"
  (error ""))

(defattach |error| (mssg)
  "Signals the error message MSSG to the ground evaluator"
  (error "~a" mssg))

(defattach |new| ()
  "Creates a new mutable variable without any current value"
  (cons nil t))

(defattach |ref| (e)
  "Creates a mutable variable with a current value E"
  (list e))

(defattach |def| (v e)
  "Sets to E the value of a mutable variable V"
  (pvsio_set_gvar v e))

(defattach |undef| (v)
  "Tests if a mutable variable V is undefined"
  (cdr v))

(defattach |val_lisp| (v)
  "Gets the current value of a mutable variable V"
  (pvsio_get_gvar v))

(defattach |loop_lift| (f)
   "Applies F in an infinite loop"
   (handler-case 
       (loop (pvs-funcall f nil))
     (pvsio-loop
      (condition)
      (val condition))))

(defattach |return| (e)
  "Returns E from an infinite loop"
  (error 'pvsio-loop :val e))

 (defattach |to_lisp| (e)
   "Returns internal Lisp expression representing e. To be used exclusively in format function"
   e)

(defattach |format| (s e)
   "Formats expression E using Common Lisp format string S"
   (let ((the-type (pc-typecheck (cadr (types (domain (pc-parse the-pvs-type_ 'type)))))))
     (apply #'format (cons nil (cons s (formatargs e the-type))))))

)))

(define-condition pvsio-exception (simple-error)
  ((str-tag :accessor str-tag  :initarg :str-tag)
   (val :accessor val :initarg :val))
  (:report
   (lambda (condition stream)
     (format stream "PVSio Exception (~a): ~a" (str-tag condition) (val condition)))))

(define-condition pvsio-loop (simple-error)
  ((val :accessor val :initarg :val)))

(defun throw-pvsio-exc (str-tag val)
  (error 'pvsio-exception :str-tag str-tag :val val))

(defun stdcatch-attachments ()

(eval '(attachments |stdcatch|
	     
(defattach |catch_lift| (tag f1 f2)
  "If F1 throws the exception e tagged tag, then evaluates F2(e). Otherwise, returns F1"
  (handler-case
      (pvs-funcall f1 nil)
    (pvsio-exception
     (condition)
     (if (equal tag (str-tag condition))
	 (let* ((exc (pvs2cl_record tag (val condition))))
	   (pvs-funcall f2 exc))
       (error condition)))))

(defattach |throw| (exc)
  "Throws the exception EXC"
  (let ((str-tag (elt exc 0))
	(val     (elt exc 1)))
    (throw-pvsio-exc str-tag val)))

)))

(defun stdpvs-attachments ()

(eval '(attachments |stdpvs|

(defattach |typeof| (e)
  (declare (ignore e))		      
  "Returns the string value of the type of E"
  (let* ((the-domain (domain the-pvs-type_)))
    (format nil "~a" (or (print-type the-domain) the-domain))))

(defattach |str2pvs| (s)
  "Translates string S to PVS format"
  (eval (pvs2cl (pc-typecheck (pc-parse s 'expr)))))

(defattach |pvs2str_lisp| (e)
  "Translates PVS expresion E to a string"
  (let* ((the-domain (domain the-pvs-type_)))
    (handler-case 
	(str (cl2pvs e (pc-typecheck the-domain)))
      (pvseval-error
       (condition)
       (throw-pvsio-exc "CantTranslateBack"
			(format nil "~a" condition))))))

)))

(defun stdpvsio-attachments ()
 
(eval '(attachments |stdpvsio|

(defattach |help_pvs_attachment| (s)
  "Displays help for semantic attachment S"
  (or (help-pvs-attachment s) t))

(defattach |help_pvs_theory_attachments| (s)
  "Displays help for semantic attachments in theory S"
  (or (help-pvs-theory-attachments s) t))

(defattach |pvsio_version| ()
  "Shows current version of PVSio"
  *pvsio-version*)

(defattach |set_promptin| (s)
  "Change current PVSio input prompt to S"
  (when (setq *pvsio-promptin* s) t))

(defattach |set_promptout| (s)
  "Change current PVSio output prompt to S"
  (when (setq *pvsio-promptout* s) t))

)))

(defun stdsys-attachments ()

(eval '(attachments |stdsys|

(defattach |get_time| ()
  "Gets current system time"
  (multiple-value-bind (s mi h d mo y dow dst tz) (get-decoded-time)
    (the (simple-array *) (pvs2cl_record d dow dst h mi mo s tz y))))

(defattach |sleep| (n)
  "Sleeps n seconds"
  (or (sleep n) t))

(defattach |get_env| (name default)
  "Gets environment variable NAME. Returns DEFAULT if undefined"
  (or (environment-variable (string name)) default))

)))

(defun initialize-prelude-attachments ()
  (stdstr-attachments)
  (stdio-attachments)
  (stdmath-attachments)
  (stdindent-attachments)
  (stdprog-attachments)
  (stdcatch-attachments)
  (stdpvs-attachments)
  (stdpvsio-attachments)
  (stdsys-attachments))
