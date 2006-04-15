; defattach.lisp
;; Functions and macros for semantic attachments
;; Release : PVSio-2.c (09/16/05)

(in-package :pvs)

(defparameter *pvsio-attachments* (make-hash-table :test #'equal))

(defun pvsio-version ()
  (pvs-message *pvsio-version*))

(defun attachment-list ()
  (sort (loop for x being each hash-key in *pvsio-attachments*
	      using (hash-value y)
	      collect (cons x y))
	#'string< :key #'car))

(defun find-attachment (name l r)
  (if l
      (let ((f (find name (cdar l)
		     :test #'(lambda (x y) (string= x (car y))))))
	(if f 
	    (find-attachment name (cdr l) (append r (list (cons (caar l) f))))
	  (find-attachment name (cdr l) r)))
    r))

(defun find-attachment-name (name)
  (find-attachment name (attachment-list) nil))

(defun find-nameargs (theory nameargs)
  (find nameargs (gethash theory *pvsio-attachments*) 
	:test #'equal))

(defun attachment-names ()
  (sort (remove-duplicates 
	 (mapcar #'car (reduce #'append (mapcar #'cdr (attachment-list))))
	 :test #'string=)
	#'string<))

(defun attachment-theories ()
  (mapcar #'car (attachment-list)))

(defun help-attachments-str (l a s)
  (if (or l a)
      (cond ((cdr a) 
	     (help-attachments-str l (cdr a) (format nil "~a~a, " s (caar a))))
	    (a 
	     (help-attachments-str l nil (format nil "~a~a~%" s (caar a))))
	    (t (help-attachments-str
		(cdr l) (cdar l)
		(format nil "~a~%Theory ~a: " s (caar l)))))
    s))

(defun list-attachments-str ()
  (let ((l (attachment-list)))
    (if l (help-attachments-str 
	   l nil
	   (format nil "Semantic attachments loaded in current context~%~%"))
      (format nil "No semantics attachments loaded in current context"))))

(defun list-attachments ()
  (format t "~a" (list-attachments-str)))

(defun help-theory-attachments-str (theory)
  (multiple-value-bind 
   (obj find) 
   (gethash theory *pvsio-attachments*)
   (if find
       (format
	nil "Semantic attachments in theory ~a~%~%~a" 
	theory 
	(reduce #'(lambda (x y)
		    (concatenate 'string x y))
		(mapcar #'(lambda(x)
			    (help-attachment-theory-nameargs-str theory x))
			obj)))
     (format nil "No semantic attachments in theory ~a" theory))))

(defun help-pvs-theory-attachments (theory)
  (format t "~a" (help-theory-attachments-str theory)))

(defun help_pvs_theory_attachments (theory)
  (help-pvs-theory-attachments theory))

(defun help-attachment-str (name)
  (let ((f (find-attachment-name name)))
    (if f (format 
	   nil "Semantic attachments named ~a ~%~%~a"
	   name
	   (reduce #'(lambda (x y)
		       (concatenate 'string x y))
		   (mapcar #'(lambda(x)
			       (help-attachment-theory-nameargs-str 
				(car x) (cdr x))) f)))
      (format nil "No semantic attachments named ~a" name))))

(defun help-pvs-attachment (name)
  (format t "~a" (help-attachment-str name)))

(defun help_pvs_attachment (name) 
  (help-pvs-attachment name))

(defun help-attachment-theory-nameargs-str (theory nameargs)
  (let ((fun (makesym "pvsio_~a_~a_~a" theory (car nameargs) (cdr nameargs))))
    (documentation fun 'function)))

(defun less-name-args (p1 p2)
  (or (string< (car p1) (car p2))
      (and (string= (car p1) (car p2)) (< (cdr p1) (cdr p2)))))

(defun add-sorted (nameargs list)
  (cond
   ((not list) (list nameargs))
   ((equal nameargs (car list)) list)
   ((less-name-args nameargs (car list))
    (cons nameargs list))
   (t (cons (car list)(add-sorted nameargs (cdr list))))))

(defun check-defattach (name body)
  (cond ((not (symbolp name))
	 (pvs-message
	  "Error: ~a is not a valid name of a semantic attachment"
	  name)
	 nil)
	((null body) 
	 (pvs-message "Error: Body of semantic attachment ~a is missing"
		      name))
	(t t)))

(defmacro defattach  (thnm args &rest body)
  (when (check-defattach thnm body)
    (let* ((thnms  (symbol-name thnm))
	   (pos    (position #\. thnms)))
      (cond ((not pos) 
	     (pvs-message  
	      "Error: ~a is not a valid name of a semantic attachment"
	      thnms))
	    (t 
	     (let ((theory (subseq thnms 0 pos))
		   (name   (subseq thnms (+ pos 1))))
	       (pvs-message "Loading semantic attachment: ~a.~a" theory name)
	       (defattach-aux theory name args body)))))))

(defmacro defattach-th-nm  (theory nm args &rest body)
  (when (check-defattach nm body)
    (defattach-aux theory (symbol-name nm) args body)))

(defun defattach-aux  (theory name args body)
  (let* ((nargs    (length args))
	 (newargs  (if (> nargs 0) 
		       (append args (list '&optional '*the-pvs-type*))
		     args))
	 (nameargs (cons name nargs))
	 (nm       (makesym "~a" name))
	 (fnm      (makesym "pvsio_~a_~a_~a" theory name nargs))
	 (th       (makesym "~a" theory))
	 (argstr   (if args
		       (format nil "(~a~{,~a~})" (car args) (cdr args))
		     ""))
	 (dobo     (if (and body (cdr body) (stringp (car body))) 
		       body
		     (cons "" body)))
	 (doc      (format nil "
Attachment: ~a.~a~% 
Usage: ~a~a~%
Documentation: ~a~%
Lisp name : ~a~%
Lisp definition: ~%
~s~%~%" 
			   theory name name argstr
			   (car dobo) fnm (cons 'progn (cdr dobo))))
	 (mssg   
	  (format 
	   nil 
	   "Function ~a.~a is defined as a semantic attachment. 
It cannot be evaluated in a formal proof.
" 
	   theory name)))
    (if (find-nameargs theory nameargs)
	(pvs-message "Redefining ~a.~a" theory name)
      (setf (gethash theory *pvsio-attachments*) 
	    (add-sorted nameargs (gethash theory *pvsio-attachments*))))
    (when (> nargs 0) (push (mk-name nm nil th) *pvsio2cl-primitives*))
    `(defun ,fnm ,newargs ,doc 
       (if *in-evaluator* 
	   ,(cons 'progn (cdr dobo))
	 (throw '*pvsio-inprover* ,mssg)))))

(defun reattach (theory macros)
  (mapcar #'(lambda (x) 
	      (if (and (consp x) (equal (car x) '|defattach|))
		  (cons 'defattach-th-nm (cons theory (cdr x)))
		x)) 
	  macros))

(defmacro attachments (th &rest macros)
  (cond ((not (symbolp th))
	 (pvs-message "Error: ~a is not a valid theory name" th))
	(t 
	 (let ((theory (symbol-name th))) 
	   (pvs-message "Loading semantic attachments theory: ~a" theory)
	   (cons 'progn (reattach theory macros))))))

