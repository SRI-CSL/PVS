;;
;; prelude-attachments.lisp
;; Release: PVSio-8.0 (10/13/2023)
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

(defun stdpvs-attachments ()

(eval '(attachments |stdpvs|

(defattach |format_lisp| (s e)
   "Formats expression E using Common Lisp format string S"
   (let ((the-type (pc-typecheck (cadr (types (domain the-pvs-type_)))))
	 (pprat    (pvsio-eval-lisp "stdmath.PP_RATIONALS")))
     (unwind-protect
	 (progn
	   (pvsio_push_gvar pprat nil)
	   (apply #'format (cons nil (cons s (formatargs e the-type)))))
       (pvsio_pop_gvar pprat))))

(defattach |error| (mssg)
  "Signals the error message MSSG to the ground evaluator"
  (error 'pvsio-error :message mssg))

(defattach |exit| ()
  "Exits the current evaluation and returns to the ground evaluator"
  (error 'pvsio-exit))

(defattach |last_iteration| (e)
  "Breaks a loop with the value E"
  (let ((the-type (domain the-pvs-type_)))
    (error 'pvsio-break :val e :type the-type)))

(defattach |return| (e)
  "Returns E as the value of a function call"
  (let ((the-type (domain the-pvs-type_)))
    (error 'pvsio-return :val e :type the-type)))

(defattach |loop_lift| (f)
   "Applies F in an infinite loop"
   (handler-case 
       (loop (pvs-funcall f 0))
     (pvsio-break
      (condition)
      (val condition))))

(defattach |to_lisp| (pvs)
  "Translates PVS object to Lisp"
  pvs)

(defattach |to_lisp_| (pvs)
  "Translates PVS object to Lisp"
  pvs)

(defattach |unwind_protect_lift| (ft fcu)
  "Evaluate ft, returning its value. The cleanup code fcu will be evaluated if control leaves ft."
  (unwind-protect
      (pvs-funcall ft 0)
    (pvs-funcall fcu 0)))

(defattach |type_of_domain_lisp| (e)
  (declare (ignore e))		      
  "Returns the string value of the type of E"
  (let* ((the-domain (domain (domain the-pvs-type_))))
    (format nil "~a" (or (print-type the-domain) the-domain))))

)))

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

;; Oveline: <char> Ctrl-x 8 305 
(defparameter *ol-digits* '("0̅" "1̅" "2̅" "3̅" "4̅" "5̅" "6̅" "7̅" "8̅" "9̅"))
(defparameter *ellipsis* '("¨" . "¨̅"))

(defun replace-ol-digits (acc &optional (n 0) (ol *ol-digits*))
  (if (null ol) acc
    (replace-ol-digits (replace-string acc (format nil "~a" n) (car ol))
		       (1+ n) (cdr ol))))

;; Pretty prints decimal using overline to indicate repeating digits. Finp is the precision of the
;; non-repeating digits. Truncated indicates that the infinite representation was truncated.
(defun pp-decstr (str finp truncated)
  (let ((pos (position #\. str)))
    (if pos
	(let* ((fixp (+ pos finp 1))
	       (pre  (subseq str 0 (min fixp (length str))))
	       (pos  (when (< fixp (length str)) (subseq str fixp (length str))))
	       (npos (when pos (replace-ol-digits pos)))
	       (ell  (when truncated (if (>= fixp (length str)) (car *ellipsis*) (cdr *ellipsis*)))))
	  (format nil "~a~@[~a~]~@[~a~]" pre npos ell))
      str)))

(defun pp-rat (r &optional (precision 6))
  "Pretty prints rational numbers using overline to indicate repeating digits and ellipsis when decimal
expansion is truncated. If precision is negative, the rational is printed in decimal representation if the
representation is finite. Otherwise, it prints its rational form."
  (multiple-value-bind (finp infp)
      (decimal-precision-of-rat r (max 0 precision))
    (cond ((and (< precision 0) (= infp 0)) (ratio2decimal-with-rounding-mode r 0 finp))
	  ((< precision 0) (format nil "~a" r))
	  (t (let ((str (ratio2decimal-with-rounding-mode r 0 (min precision (+ finp infp)) t)))
	       (pp-decstr str finp (> (+ finp infp) precision)))))))

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

(defattach |substring| (s i j)
  "If i <= j returns substring S[i..j]. Otherwise, returns substring
S[j..i]. Empty if indices are out of range  
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

(defattach |strsplit| (str separator)
  "Splits the string STR using SEPARATOR as separator."
  (split str (char separator 0)))

(defattach |strfind| (s1 s2)
  "Index of leftmost occurrence of S1 in S2 or -1 if S1 doesn't occur in S2"
  (or (search s1 s2 :test #'char=) -1))

(defattach |strfind_from_end| (s1 s2)
  "Index of rightmost occurrence of S1 in S2 or -1 if S1 doesn't occur in S2"
  (or (search s1 s2 :from-end t :test #'char=) -1))

(defprimitive |strconcat| (s1 s2)
  "Concatenates S1 and S2"
  (format nil "~a~a" s1 s2))

(defattach |rat2decstr_with_zeros| (r precision rounding zeros)
  "Converts rational number to string decimal representation using given precision, i.e., natural number n
denoting 10^(-n), and rounding mode, i.e, TowardsZero, TowardsInfnty, TowardsNegInfnty, TowardsPosInfnty.
Displays trailing zeroes when zeros is set to TRUE"
  (ratio2decimal-with-rounding-mode r rounding precision zeros))

(defattach |pp_decstr| (str finp truncated)
  "Pretty prints decimal using overline to indicate repeating digits. Finp is the precision of the
non-repeating digits. Truncated indicates that the infinite representation was truncated."
  (pp-decstr str finp truncated))

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
  "Returns 0 if s1 = s2, -1 if s1 < s2, 1 if s1 > s2. If sensitive is TRUE, comparise is case sensitive."
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

(defattach |subtypeof?| (t1 t2)
  "Returns TRUE if T1 is a subtype of T2 (types are represented using strings). Uses subtype judgements, but otherwise it's essentially syntactic"
  (let ((the-type1 (pc-typecheck (pc-parse t1 'type-expr)))
	(the-type2 (pc-typecheck (pc-parse t2 'type-expr))))
    (subtype-of? the-type1 the-type2)))

(defattach |str2pvs| (s)
  "Translates string S to PVS format"
  (eval (pvs2cl (pc-typecheck (pc-parse s 'expr)))))

(defattach |pvs2str_lisp| (e)
  "Translates PVS expresion E to a string"
  (let ((the-domain (domain the-pvs-type_))
	(pprat      (pvsio-eval-lisp "stdmath.PP_RATIONALS")))
    (handler-case
	(unwind-protect
	     (progn
	       (pvsio_push_gvar pprat nil)
	       (str (cl2pvs e (pc-typecheck the-domain))))
	  (pvsio_pop_gvar pprat))
      (groundeval-error
	  (condition)
	(declare (ignore condition))
	(throw-pvsio-exc "PVS2String"
			 (format nil "PVS Object doesn't have literal representation"))))))
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
 (cond ((= i 0) (let ((f (open s :direction :output :if-exists nil)))
		  (or f (throw-pvsio-exc "FileAlreadyExists" s))))
       ((= i 1) (open s :direction :output :if-exists :supersede))
       ((= i 2) (open s :direction :output :if-exists :append))
       ((= i 3) (open s :direction :output :if-exists :overwrite))))

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
  "Get the name of a file stream"
  (namestring f))

(defattach |frename| (oldname newname)
  "Rename a file stream named oldname with newname. Return long newname"
  (handler-case
      (namestring (rename-file oldname newname))
    (error (condition)
      (declare (ignore condition))
      (throw-pvsio-exc "FileNotFound" oldname))))
  
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
  (or (file-namestring s) ""))

(defattach |directory| (s)
  "Returns the directory part of a file name"
  (directory-namestring s))

(defattach |pathname_path| (name)
   "Path (list of directories) of pathname"
   (let ((dirs (pathname-directory name)))
     (if (equal (car dirs) ':absolute)
         (cons "/" (cdr dirs))
       (cdr dirs))))

 (defattach |fwrite_lisp| (f typ pvs)
   "Writes a PVS object to output stream, so that it can be retrieved afterwards by fread"
   (or (format f "~s~%" (cons typ pvs)) t))

 (defattach |fread_lisp| (f typ)
   "Reads an PVS object of type T from an input stream written by fwrite"
   (let* ((type-pvs (read f))
	  (the-type1 (pc-typecheck (pc-parse (car type-pvs) 'type-expr)))
	  (the-type2 (pc-typecheck (pc-parse typ 'type-expr))))
     (if (subtype-of? the-type1 the-type2)
	 (cdr type-pvs)
       (throw-pvsio-exc
	"ReadPVS" (format nil "Type ~a is not of a sub-type of ~a" the-type1 the-type2)))))

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
  (if (floatp x) (rationalize x) x))

(defprimitive |rat2numden| (r)
  "Returns numerator and denominator of rational number"
  (pvs2cl_tuple (numerator r) (denominator r)))

(defattach |decimal_precision| (r maxperiod)
  "Compute the decimal precision of a rational number. Return 2 values. The first one is the number of
non-repeating digits. If maxperiod is negative, the second value is the period of the repeating
digits. Computing the period is expensive for rationals with large denominators. Therefore, if
maxperiod is non-negative, the second value is the minimum between the period and maxperiod+1.
In either case, if the second value is 0, the rational has a finite decimal representation."
  (multiple-value-bind (finp infp)
      (decimal-precision-of-rat r maxperiod)
    (pvs2cl_tuple finp infp)))

)))

(defun formatargs (e type)
  (cond
   ((and (type-name? type)(equal (id type) '|Lisp|))
    (let ((ptype (find-supertype (type-value (car (dactuals type))))))
      (cond ((and (listp e) (type-name? ptype) (equal (id ptype) '|list|))
	     (list (loop for ei in e
			 append (formatargs ei (type-value (car (actuals ptype)))))))
	    ((and (arrayp e) (tupletype? ptype))
	     (loop for ei across e
		   for ti in (types ptype)
		   append (formatargs ei ti)))
	    ((numberp e) (list (rat2double e)))
	    (t (list e)))))
   ((or (tc-eq (find-supertype type) *number*)
	(tc-eq (find-supertype type) *string-type*))
    (list e))
   (t (list (cl2pvs e type)))))

(defun stdprog-attachments ()

(eval '(attachments |stdprog|

(defattach |new| ()
  "Creates a new mutable variable with an undefined value"
  (pvsio_new_gvar))

(defattach |ref| (value)
  "Creates a mutable variable and sets it to given value"
  (pvsio_ref_gvar value))

(defattach |def| (gvar value)
  "Sets mutable variable gvar to given value"
  (pvsio_def_gvar gvar value))

(defattach |val_lisp| (gvar)
  "Returns value of mutable variable. Throws exception UndefinedMutableVariable when undefined?(gvar)"
  (pvsio_val_gvar gvar))

(defattach |undef| (gvar)
  "Returns TRUE if mutable variable is undefined"
  (pvsio_undef_gvar gvar))

(defattach |reset| (gvar)
  "Sets mutable variable to undefined"
  (pvsio_reset_gvar gvar))

(defattach |push_lisp| (gvar value)
  "Pushes value to the top of the mutable variable and skips"
  (pvsio_push_gvar gvar value))

(defattach |pop_lisp| (gvar)
  "Pops value of the mutable variable and fails silently when mutable variable is undefined"
  (pvsio_pop_gvar gvar))
 
)))

(define-condition pvsio-exception (simple-error)
  ((tag :accessor tag :initarg :tag)
   (val :accessor val :initarg :val)
   (mssg :accessor mssg :initarg :mssg))
  (:report
   (lambda (condition stream)
     (format stream "[~a]~@[ ~a~]"
	     (tag condition) (mssg condition)))))

(defun throw-pvsio-exctag (tag val mssg)
  (error 'pvsio-exception :tag tag :val val :mssg  mssg))

(defun throw-pvsio-exc (exc val &optional mssg)
  (let ((tag (pvsio-eval-lisp (format nil "~a`tag" exc)))
	(msg (or mssg (pvsio-funcall (format nil "~a`formatter" exc) val))))
    (throw-pvsio-exctag tag val msg)))

(define-condition pvsio-error (simple-error)
  ((message :accessor message  :initarg :message))
  (:report
   (lambda (condition stream)
     (format stream "~@[~a~]" (message condition)))))

(define-condition pvsio-exit (simple-error) ())

(define-condition pvsio-return (simple-error)
  ((val :accessor val :initarg :val)
   (type :accessor type :initarg :type))
  (:report
   (lambda (condition stream)
     (let ((val-str (handler-case
			(str (cl2pvs (val condition) (pc-typecheck (type condition))))
		      (error (condition) (declare (ignore condition))))))
       (format stream "Value~@[ ~a~] was returned outside the scope of a function" val-str)))))

(define-condition pvsio-break (pvsio-return) ()
  (:report
   (lambda (condition stream)
     (let ((val-str (handler-case
			(str (cl2pvs (val condition) (pc-typecheck (type condition))))
		      (error (condition) (declare (ignore condition))))))
       (format stream "Value~@[ ~a~] was returned outside the scope of a loop" val-str)))))

(defun stdcatch-attachments ()

(eval '(attachments |stdcatch|

(defun starts-with-tag (exctag string)
  (let ((lstr (length string))
	(lexc (length exctag)))
    (and (>= lstr lexc)
	 (string= exctag string :end2 lexc)
	 (or (= lstr lexc)
	     (and (> lstr (+ 2 lexc))
		  (string= (subseq string lexc (+ 2 lexc)) "::"))))))

(defattach |catch_lift| (exctag f1 f2)
  "If F1 throws the exception tagged exctag, then evaluates f2(val). Otherwise, returns F1"
  (handler-case
      (pvs-funcall f1 0)
    (pvsio-exception
     (condition)
     (if (starts-with-tag exctag (tag condition))
	 (let* ((exc (val condition)))
	   (pvs-funcall f2 exc))
       (error condition)))))

(defattach |throw_lisp| (exctag val mssg)
  "Throws the exception tagged EXCTAG using VAL and MSSG"
 (throw-pvsio-exctag exctag val mssg))

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

(defattach |real_time| ()
   "Real time"
   (get-internal-real-time))

(defattach |run_time| ()
  "Run time"
  (get-internal-run-time))

(defattach |internal_time_units| ()
  "Units of internal time"
  internal-time-units-per-second)

(defattach |sleep| (n)
  "Sleeps n seconds"
  (or (sleep n) t))

(defattach |get_env| (name default)
  "Gets environment variable NAME. Returns DEFAULT if undefined"
  (or (environment-variable (string name)) default))

(defattach |system_call| (call)
  "Make a system call and return status and output string"
  (let ((output (extra-system-call call)))
    (pvs2cl_record (car output) (cdr output))))

)))

(defun initialize-prelude-attachments ()
  (stdpvs-attachments)
  (stdstr-attachments)
  (stdio-attachments)
  (stdmath-attachments)
  (stdprog-attachments)
  (stdcatch-attachments)
  (stdpvsio-attachments)
  (stdsys-attachments))
