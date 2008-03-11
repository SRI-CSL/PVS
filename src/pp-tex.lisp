;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pp-tex.lisp -- 
;; Author          : Sam Owre
;; Created On      : Tue Jan 12 03:25:25 1999
;; Last Modified By: Sam Owre
;; Last Modified On: Tue Jan 26 18:28:57 1999
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

(defvar *latex-id-strings* (make-hash-table :test 'eq))
(defvar *latex-keyword-strings* (make-hash-table :test 'eq))
(defvar *latex-funsym-strings* (make-hash-table :test 'eq))
(defvar *pvs-tex-substitution-hash* (make-hash-table :test 'equal))
(defvar *tex-symbol-counters* nil)
(defvar *in-tex-math-mode* nil)
(defvar *pp-tex-column* nil)
(defvar *pp-tex-spaces-to-delete* nil)
(defvar *pp-tex-newline-list* nil)
(defvar *pp-tex-newline-element* nil)

(defun pp-tex (obj stream)
  (let ((*print-pretty* t)
	(*print-readably* nil)
	(*print-escape* nil)
	(*print-right-margin* *latex-linelength*)
	(*tex-symbol-counters* nil)
	(*disable-gc-printout* t))
    ;;(setf (slot-value *standard-output* 'excl::charpos) 0)
    (unwind-protect
	(let* ((str (with-output-to-string (*standard-output*)
		      (pp-tex* obj)))
	       (*in-tex-math-mode* nil)
	       (*pp-tex-column* 0)
	       (*pp-tex-spaces-to-delete* 0)
	       (len (length str))
	       (*pp-tex-newline-list* (tex-get-math-newline-info str 0 len))
	       (*pp-tex-newline-element* nil))
	  (write-string-with-tex-substitutions str 0 len stream))
      (clrhash *latex-id-strings*)
      (clrhash *latex-keyword-strings*)
      (clrhash *latex-funsym-strings*)
      (clrhash *pvs-tex-substitution-hash*))))

(defun tex-get-math-newline-info (str pos len &optional cur-info info)
  (if (< pos len)
      (let ((char (char str pos)))
	(case char
	  (#\( (unless (zerop pos)
		 (if (char= (char str (1- pos)) #\\)
		     (tex-get-math-newline-info str (1+ pos) len
						(list pos) info)
		     (tex-get-math-newline-info str (1+ pos) len
						cur-info info))))
	  (#\) (unless (zerop pos)
		 (if (char= (char str (1- pos)) #\\)
		     (if (cdr cur-info)
			 (tex-get-math-newline-info
			  str (1+ pos) len
			  nil (cons (nreverse (cons pos cur-info)) info))
			 (tex-get-math-newline-info
			  str (1+ pos) len nil info))
		     (tex-get-math-newline-info str (1+ pos) len
						cur-info info))))
	  (#\newline (if cur-info
			 (tex-get-math-newline-info str (1+ pos) len
						    (cons pos cur-info) info)
			 (tex-get-math-newline-info str (1+ pos) len
						    cur-info info)))
	  (t (tex-get-math-newline-info str (1+ pos) len cur-info info))))
      (nreverse info)))

(defun write-string-with-tex-substitutions (str pos len stream)
  (if (< pos len)
      (let ((char (char str pos))
	    (wrote-char nil))
	(case char
	  (#\( (let ((elt (assoc pos *pp-tex-newline-list*)))
		 (when elt
		   (setq *pp-tex-newline-element* (cdr elt)))))
	  (#\) (when (and *pp-tex-newline-element*
			  (= pos (car *pp-tex-newline-element*)))
		 (setq *pp-tex-newline-element* nil)))
	  (#\newline (if *pp-tex-newline-element*
			 (progn
			   (pop *pp-tex-newline-element*)
			   (if (cdr *pp-tex-newline-element*)
			       (write "}\\)} \\zbox{\\({" :stream stream)
			       (write "}\\)} \\hbox{\\({" :stream stream))
			   (setq wrote-char t)
			   (setq *pp-tex-spaces-to-delete*
				 (+ *pp-tex-column* 100)))
			 (setq *pp-tex-column* 0)))
	  (#\{ (when *pp-tex-newline-element*
		 (if (cdr *pp-tex-newline-element*)
		     (write "{{\\zbox{\\(" :stream stream)
		     (write "{{\\hbox{\\(" :stream stream))))
	  (#\} (when *pp-tex-newline-element*
		 (write "}\\)}}" :stream stream)))
	  (#\space (unless (zerop *pp-tex-spaces-to-delete*)
		     (decf *pp-tex-spaces-to-delete*)
		     (setq wrote-char t))
	   (unless *pp-tex-newline-element*
	     (incf *pp-tex-column*)))
	  (t (unless *pp-tex-newline-element*
	       (incf *pp-tex-column*))
	     (setq *pp-tex-spaces-to-delete* 0)))
	(cond ((< (char-code char) 127)
	       (unless wrote-char
		 (write-char char stream))
	       (write-string-with-tex-substitutions str (1+ pos) len stream))
	      (t (assert (= (char-code char) 127) ()
			 (format nil "char-code = ~d, pos = ~d, len = ~d"
			   (char-code char) pos len))
		 (let* ((midpos (position-if #'(lambda (ch)
						 (not (= (char-code ch)
							 127)))
					     str
					     :start pos :end len))
			(endpos (or (position-if #'(lambda (ch)
						     (<= (char-code ch) 127))
						 str
						 :start midpos :end len)
				   len)))
		   (multiple-value-bind (trans npos)
		       (get-latex-substitution-translation str pos endpos)
		     (write trans :stream stream)
		     (write-string-with-tex-substitutions
		      str npos len stream))))))))
  
(defun get-latex-substitution-translation (str pos epos)
  (assert (< pos epos))
  (let ((trans (gethash (subseq str pos epos) *pvs-tex-substitution-hash*)))
    (assert trans ()
	    "trans of ~a not found" (subseq str pos epos))
    (if trans
	(values trans epos)
	(get-latex-substitution-translation str pos (1- epos)))))


;(defmethod pp-tex* :around ((syn syntax))
;  (call-next-method)
;  ;; need to deal with comments here
;  )

;;; Module level

(defmethod pp-tex* ((theories modules))
  (pprint-logical-block (nil (modules theories))
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (terpri) (terpri))))

;(defmethod pp-tex* ((mod datatype-or-module))
;  (call-next-method))

(defmethod pp-tex* ((mod module))
  (with-slots (id formals exporting assuming theory) mod
    (let ((*current-context* (saved-context mod)))
      (pprint-logical-block (nil nil)
	(pp-tex-id id)
	(pp-tex-theory-formals formals)
	(write ": ")
	(pprint-indent :block 2)
	(pp-tex-keyword 'THEORY)
	(pprint-indent :block 1)
	(pprint-newline :mandatory)
	(pp-tex* exporting)
	(pp-tex-keyword 'BEGIN)
	(pprint-indent :block 2)
	(pprint-newline :mandatory)
	(pp-tex-assuming (if *unparse-expanded*
			     assuming
			     (remove-if #'generated-by assuming)))
	(pp-tex-theory (if *unparse-expanded*
			   theory
			   (remove-if #'generated-by theory)))
	(pprint-indent :block 1)
	(pprint-newline :mandatory)
	(pprint-newline :mandatory)
	(pp-tex-keyword 'END)
	(write-char #\space)
	(pp-tex-id id)))))

(defun pp-tex-theory-formals (formals)
  (when formals
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block
	  (nil (check-chained-syntax formals)
	       :prefix (get-pp-tex-id '\[)
	       :suffix (get-pp-tex-id '\]))
	(loop (let ((*pretty-printed-prefix* nil)
		    (elt (pprint-pop)))
		(when (typep elt 'importing)
		  (let ((imps (list elt)))
		    (loop while (chain? (car imps))
			  do (setq elt (pprint-pop))
			  do (push elt imps))
		    (pprint-logical-block
			(nil (nreverse imps)
			     :prefix (get-pp-tex-id '\()
			     :suffix (get-pp-tex-id '\)))
		      (pp-tex-keyword 'IMPORTING)
		      (write #\space)
		      (pprint-indent :current 0)
		      (loop (pp-tex* (pprint-pop))
			    (pprint-exit-if-list-exhausted)
			    (write-char #\,)
			    (write-char #\space)
			    (pprint-newline :fill))))
		  (write-char #\space)
		  (setq elt (pprint-pop)))
		(pp-tex* elt)
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill)))))))

(defmethod pp-tex* ((exp exporting))
  (with-slots (kind names but-names modules) exp
    (unless (eq kind 'default)
      (pprint-logical-block (nil nil)
	(pp-tex-keyword 'EXPORTING)
	(write-char #\space)
	(pprint-indent :current 0)
	(pp-tex-exportings names but-names)
	(when (or kind modules)
	  (pprint-indent :block 4)
	  (pprint-newline :fill)
	  (pp-tex-exportingmods kind modules)))
      (pprint-newline :mandatory))))

(defun pp-tex-exportings (names but-names)
  (pprint-logical-block (nil names)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill)))
  (when but-names
    (write-char #\space)
    (pp-tex-keyword 'BUT)
    (write-char #\space)
    (pprint-logical-block (nil but-names)
      (loop (pprint-indent :current 2)
	    (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)))))

(defun pp-tex-exportingmods (kind modules)
  (pprint-logical-block (nil modules)
    (write-char #\space)
    (pp-tex-keyword 'WITH)
    (write-char #\space)
    (if kind
	(pp-tex-keyword kind)
	(loop (pprint-indent :current 2)
	      (pp-tex* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)))))

(defmethod pp-tex* ((name expname))
  (with-slots (id kind type) name
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when kind
	(write-char #\:)
	(write-char #\space)
	(if (symbolp kind)
	    (pp-tex-keyword kind)
	    (pp-tex* type))))))

(defun pp-tex-assuming (assuming)
  (when assuming
    (let ((*pretty-printing-decl-list* t)
	  (last-one (car (last assuming))))
      (pprint-newline :mandatory)
      (pprint-logical-block (nil nil)
	(pp-tex-keyword 'ASSUMING)
	(pprint-indent :block 1)
	(pprint-newline :mandatory)
	(pprint-logical-block (nil (check-chained-syntax assuming))
	  (loop (let ((*pretty-printed-prefix* nil)
		      (decl (pprint-pop)))
		  (if (typep decl 'importing)
		      (let ((imps (list decl)))
			(loop while (chain? (car imps))
			      do (setq decl (pprint-pop))
			      do (push decl imps))
			(pprint-logical-block (nil (nreverse imps))
			  (pp-tex-keyword 'IMPORTING)
			  (write #\space)
			  (pprint-indent :current 0)
			  (loop (pp-tex* (pprint-pop))
				(pprint-exit-if-list-exhausted)
				(write-char #\,)
				(write-char #\space)
				(pprint-newline :fill)))
			(unless *pp-compact*
			  (pprint-newline :mandatory)))
		      (pp-tex* decl))
		  (unless (or (chain? decl)
			      (eq decl last-one))
		    (pprint-newline :mandatory)
		    (unless *pp-compact*
		      (pprint-newline :mandatory))))
		(pprint-exit-if-list-exhausted)))
	(pprint-indent :block 0)
	(pprint-newline :mandatory)
	(pp-tex-keyword 'ENDASSUMING)
	(pprint-newline :mandatory)))))

(defun pp-tex-theory (theory)
  (when theory
    (let ((*pretty-printing-decl-list* t)
	  (last-one (car (last theory))))
      (pprint-logical-block (nil (check-chained-syntax theory))
	(pprint-indent :block 0)
	(pprint-newline :mandatory)
	(loop (let ((*pretty-printed-prefix* nil)
		    (decl (pprint-pop)))
		(if (typep decl '(or importing theory-abbreviation-decl))
		    (let ((imps (list decl)))
		      (loop while (chain? (car imps))
			    do (setq decl (pprint-pop))
			    do (push decl imps))
		      (pprint-logical-block (nil (nreverse imps))
			(pp-tex-keyword 'IMPORTING)
			(write #\space)
			(pprint-indent :current 0)
			(loop (pp-tex* (pprint-pop))
			      (pprint-exit-if-list-exhausted)
			      (write-char #\,)
			      (write-char #\space)
			      (pprint-newline :fill))))
		    (pp-tex* decl))
		(unless (or (chain? decl)
			    (eq decl last-one))
		  (pprint-newline :mandatory)
		  (unless *pp-compact*
		    (pprint-newline :mandatory))))
	      (pprint-exit-if-list-exhausted))))))

(defmethod pp-tex* ((dt recursive-type))
  (with-slots (id formals importings assuming constructors) dt
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (pp-tex-theory-formals formals)
      (write-char #\:)
      (write-char #\space)
      (pprint-indent :block 2)
      (if (codatatype? dt)
	  (pp-tex-keyword 'CODATATYPE)
	  (pp-tex-keyword 'DATATYPE))
      (pprint-indent :block 1)
      (when (typep dt 'datatype-with-subtypes)
	(write-char #\space)
	(pp-tex-keyword 'WITH)
	(write-char #\space)
	(pp-tex-keyword 'SUBTYPES)
	(write-char #\space)
	(pprint-logical-block (nil (subtypes dt))
	  (loop (pp-tex* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space))))
      (pprint-newline :mandatory)
      (pp-tex-keyword 'BEGIN)
      (pprint-indent :block 2)
      (pprint-newline :mandatory)
      (when importings
	(pprint-logical-block (nil importings)
	  (pp-tex-keyword 'IMPORTING)
	  (write-char #\space)
	  (pprint-indent :current 0)
	  (loop (pp-tex* (theory-name (pprint-pop)))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill)))
	(pprint-newline :mandatory)
	(pprint-newline :mandatory))
      (pp-tex-assuming assuming)
      (pp-tex-constructors constructors)
      (pprint-indent :block 1)
      (pprint-newline :mandatory)
      (pp-tex-keyword 'END)
      (write-char #\space)
      (pp-tex-id id))))

;;; (defmethod pp-tex* ((dt datatype-with-subtype)))

;;; (defmethod pp-tex* ((dt inline-datatype)))

;;; (defmethod pp-tex* ((dt inline-datatype-with-subtype)))

(defun pp-tex-constructors (constructors)
  (let ((*pretty-printing-decl-list* t))
    (pprint-logical-block (nil constructors)
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory)))))

(defmethod pp-tex* ((constr adt-constructor))
  (with-slots (id arguments recognizer) constr
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (pprint-indent :current 0)
      (when arguments
	(pprint-logical-block
	    (nil arguments
	       :prefix (get-pp-tex-id '\()
	       :suffix (get-pp-tex-id '\)))
	  (loop (pp-tex* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :fill))))
      (pprint-newline :fill)
      (write-char #\:)
      (write-char #\space)
      (pp-tex-id recognizer)
      (when (typep constr 'constructor-with-subtype)
	(write-char #\:)
	(write-char #\space)
	(pp-tex* (subtype constr))))))

(defmethod pp-tex* ((ad adtdecl))
  ;; The around method for declarations would have already printed the id:
  (with-slots (declared-type) ad
    (pp-tex* declared-type)))

;;; (defmethod pp-tex* ((constr simple-constructor)))

;;; (defmethod pp-tex* ((constr constructor-with-subtype)))

(defmethod pp-tex* ((te enumtype))
  (pp-tex-id (id te))
  (write ": ")
  (pprint-newline :fill)
  (pp-tex-keyword 'TYPE)
  (write-char #\space)
  (pprint-newline :miser)
  (write-char #\=)
  (write-char #\space)
  (pprint-newline :fill)
  (pprint-logical-block (nil (constructors te) :prefix "\\{" :suffix "\\}")
    (pprint-indent :block 0)
    (loop (pp-tex-id (id (pprint-pop)))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
	  (pprint-newline :fill))))

;;; Declarations

(defmethod pp-tex* ((imp importing))
  (with-slots (theory-name chain? semi) imp
    (unless *pretty-printing-decl-list*
      (pp-tex-keyword 'IMPORTING)
      (write-char #\space))
    (pp-tex* theory-name)))

(defmethod pp-tex* :around ((decl declaration))
  (with-slots (id module formals chain? semi) decl
    (when (or *unparse-expanded*
	      *adt*
	      (not (generated-by decl)))
      (cond ((theory-abbreviation-decl? decl)
	     (call-next-method))
	    ((and chain?
		  *pretty-printing-decl-list*)
	     (pp-tex-id id)
	     (unless (typep decl '(or formal-decl adtdecl))
	       (write-char #\,)
	       (write-char #\space)
	       (pprint-newline :fill)))
	    (t (when (newline-comment decl)
		 (write (car (newline-comment decl)))
		 (write-char #\space)
		 (pprint-newline :mandatory))
	       (when (and *comment-on-proof-status*
			  (tcc? decl))
		 (format t "  % ~a~%" (proof-status-string decl)))
	       (pp-tex-const-decl id formals (if module
						 (id module)
						 (id (current-theory))))
	       (pprint-indent :block 6)
	       (write-char #\:)
	       (write-char #\space)
	       ;;(pprint-newline :fill)
	       (call-next-method)
	       (when semi (write-char #\;))
	       (pprint-indent :block 0)
	       (unless (typep decl 'formal-decl)
		 (write "\\vspace*{\\pvsdeclspacing}")))))))

(defun pp-tex-const-decl (id pre-formals theory-id &optional post-formals)
  (cond (pre-formals
	 (let ((funsym (get-pp-tex-funsym id (mapcar #'length pre-formals)
					  theory-id)))
	   (cond (funsym
		  (unless *in-tex-math-mode*
		    (write "\\("))
		  (let ((*in-tex-math-mode* t)
			(*print-right-margin* most-positive-fixnum))
		    (write funsym)
		    (dolist (args pre-formals)
		      (dolist (a args)
			(write "{")
			(pp-tex* a)
			(write "}"))))
		  (unless *in-tex-math-mode*
		    (write "\\)"))
		  (pp-tex-decl-formals post-formals))
		 (t (pp-tex-const-decl id (butlast pre-formals) theory-id
				       (cons (car (last pre-formals))
					     post-formals))))))
	(t (pp-tex-id id)
	   (pp-tex-decl-formals post-formals))))

(defun pp-tex-decl-formals (formals)
  (when formals
    (let ((*pretty-printing-decl-list* t))
      (pprint-logical-block (nil formals)
	(loop (pp-tex-lambda-formal (pp-tex-chained-decls (pprint-pop)) nil)
	      (pprint-exit-if-list-exhausted)
	      (pprint-newline :fill))))))



;;; Need this as a primary method
(defmethod pp-tex* ((decl declaration))
  nil)

;; (defmethod pp-tex* ((decl nonempty-type-decl)))

(defmethod pp-tex* ((decl type-decl))
  (with-slots (type-expr chain?) decl
    (if (typep decl 'nonempty-type-decl)
	(pp-tex-keyword (case (keyword decl)
			  (nonempty-type 'NONEMPTY_TYPE)
			  (t 'TYPE+)))
	(pp-tex-keyword 'TYPE))))

(defmethod pp-tex* ((decl type-def-decl))
  (with-slots (type-expr contains chain?) decl
    (if (typep decl 'nonempty-type-decl)
	(pp-tex-keyword (case (keyword decl)
			  (nonempty-type 'NONEMPTY_TYPE)
			  (t 'TYPE+)))
	(pp-tex-keyword 'TYPE))
    (write-char #\space)
    (pprint-newline :miser)
    (if (typep decl 'type-eq-decl)
	(write-char #\=)
	(pp-tex-keyword 'FROM))
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* type-expr)
    (when contains
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword 'CONTAINING)
      (write-char #\space)
      (pprint-newline :miser)
      (pp-tex* contains))))
      

;; (defmethod pp-tex* ((decl nonempty-type-def-decl)) )

;; (defmethod pp-tex* ((decl type-eq-decl)) )

;; (defmethod pp-tex* ((decl nonempty-type-eq-decl)) )

;; (defmethod pp-tex* ((decl type-from-decl)) )

;; (defmethod pp-tex* ((decl nonempty-type-from-decl)) )

;; (defmethod pp-tex* ((decl formal-decl)) )

(defmethod pp-tex* ((decl formal-type-decl))
  (with-slots (type-expr) decl
    (if (typep decl 'formal-nonempty-type-decl)
	(pp-tex-keyword (case (keyword decl)
			  (nonempty-type 'NONEMPTY_TYPE)
			  (t 'TYPE+)))
	(pp-tex-keyword 'TYPE))
    (when (typep decl 'formal-subtype-decl)
      (write-char #\space)
      (pp-tex-keyword 'FROM)
      (write-char #\space)
      (pp-tex* type-expr))))

;; (defmethod pp-tex* ((decl formal-nonempty-type-decl)) )

;; (defmethod pp-tex* ((decl formal-subtype-decl)) )

;; (defmethod pp-tex* ((decl formal-nonempty-subtype-decl)) )

(defmethod pp-tex* ((decl formal-const-decl))
  (with-slots (declared-type) decl
    (pp-tex* declared-type)))

(defmethod pp-tex* ((decl formal-theory-decl))
  (with-slots (theory-name) decl
    (pp-tex-keyword 'THEORY)
    (write-char #\space)
    (pp-tex* theory-name)))

(defmethod pp-tex* ((decl lib-decl))
  (with-slots (lib-string) decl
    (pp-tex-keyword 'LIBRARY)
    (when (typep decl 'lib-eq-decl)
      (write-char #\space)
      (write-char #\=))
    (write-char #\space)
    (pprint-newline :fill)
    (let* ((str (lib-string decl))
	   (ch (find-verb-char str)))
      (format t "\\symbol{34}$\\verb~c~a~c$\\symbol{34}" ch str ch))))

(defun find-verb-char (str)
  (dotimes (i 127)
    (let ((ch (character i)))
      (when (and (graphic-char-p ch)
		 (not (alphanumericp ch))
		 (not (char= ch #\space))
		 (not (find ch str)))
	(return ch)))))

;; (defmethod pp-tex* ((decl lib-eq-decl)) )

(defmethod pp-tex* ((decl mod-decl))
  (with-slots (modname) decl
    (pp-tex-keyword 'THEORY)
    (write-char #\space)
    (write-char #\=)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* modname)))

(defmethod pp-tex* ((decl theory-abbreviation-decl))
  (with-slots (id theory-name) decl
    (unless *pretty-printing-decl-list*
      (pp-tex-keyword 'IMPORTING)
      (write-char #\space))
    (pp-tex* theory-name)
    (write-char #\space)
    (pp-tex-keyword 'AS)
    (write-char #\space)
    (pp-tex-id id)))

(defmethod pp-tex* ((decl var-decl))
  (with-slots (declared-type) decl
    (pp-tex-keyword 'VAR)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* declared-type)))

(defmethod pp-tex* ((decl const-decl))
  (with-slots (declared-type definition) decl
    (pprint-newline :fill)
    (pp-tex* declared-type)
    (when definition
      (write-char #\space)
      (pp-tex-keyword '=)
      (pprint-indent :block 4)
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex* definition)
      (pprint-indent :block 0))))

(defmethod pp-tex* ((decl macro-decl))
  (with-slots (declared-type definition) decl
    (pp-tex-keyword 'MACRO)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* declared-type)
    (write-char #\space)
    (pp-tex-keyword #\=)
    (pprint-indent :block 4)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* definition)
    (pprint-indent :block 0)))

(defmethod pp-tex* ((decl def-decl))
  (with-slots (declared-type definition declared-measure ordering) decl
    (pp-tex-keyword 'RECURSIVE)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* declared-type)
    (write-char #\space)
    (pp-tex-keyword '=)
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* definition)
    (pprint-indent :block 3)
    (pprint-newline :mandatory)
    (pp-tex-keyword 'MEASURE)
    (write-char #\space)
    (pp-tex* declared-measure)
    (when ordering
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword 'BY)
      (write-char #\space)
      (pp-tex* ordering))
    (pprint-indent :block 0)))

(defmethod pp-tex* ((decl adt-def-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* declared-type)
    (when definition
      (write-char #\space)
      (pp-tex-keyword '=)
      (pprint-indent :block 4)
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex* definition)
      (pprint-indent :block 0))))

(defmethod pp-tex* ((decl fixpoint-decl))
  (with-slots (declared-type definition) decl
    (write-char #\space)
    (if (inductive-decl? decl)
	(pp-tex-keyword 'INDUCTIVE)
	(pp-tex-keyword 'COINDUCTIVE))
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* declared-type)
    (write-char #\space)
    (pp-tex-keyword '=)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* definition)))

(defmethod pp-tex* ((decl formula-decl))
  (with-slots (spelling definition) decl
    (pp-tex-keyword spelling)
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* definition)
    (pprint-indent :block 0)))

(defmethod pp-tex* :around ((decl name-judgement))
  (with-slots (id name chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(pp-tex-id id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (pp-tex-keyword 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex* name)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex-keyword 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex* declared-type)
	     (when semi (write-char #\;))
	     (write "\\vspace*{\\pvsdeclspacing}")
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp-tex* :around ((decl application-judgement))
  (with-slots (id name formals chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(pp-tex-id id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (pp-tex-keyword 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex* name)
    (pp-tex-decl-formals formals)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex-keyword 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex* declared-type)
	     (when semi (write-char #\;))
	     (write "\\vspace*{\\pvsdeclspacing}")
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp-tex* :around ((decl number-judgement))
  (with-slots (id number-expr chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(pp-tex-id id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (pp-tex-keyword 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex* number-expr)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pp-tex-keyword 'HAS_TYPE)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex* declared-type)
	     (when semi (write-char #\;))
	     (write "\\vspace*{\\pvsdeclspacing}")
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp-tex* :around ((decl subtype-judgement))
  (with-slots (id declared-subtype chain? declared-type semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (when id
	(pp-tex-id id)
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :miser))
      (pp-tex-keyword 'JUDGEMENT)
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex* declared-subtype)
    (cond ((and chain?
		*pretty-printing-decl-list*)
	   (write-char #\,)
	   (write-char #\space))
	  (t (pprint-indent :block 4)
	     (write-char #\space)
	     (pp-tex-keyword 'SUBTYPE_OF)
	     (write-char #\space)
	     (pprint-newline :fill)
	     (pp-tex* declared-type)
	     (when semi (write-char #\;))
	     (write "\\vspace*{\\pvsdeclspacing}")
	     (pprint-indent :block 0)
	     (setq *pretty-printed-prefix* nil)))))

(defmethod pp-tex* :around ((decl conversion-decl))
  (with-slots (expr chain?) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (typecase decl
	(conversionplus-decl (pp-tex-keyword 'CONVERSION+))
	(conversionminus-decl (pp-tex-keyword 'CONVERSION-))
	(t (pp-tex-keyword 'CONVERSION)))
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex* expr)
    (when (and chain?
	       *pretty-printing-decl-list*)
      (write-char #\,)
      (write-char #\space))
    (pprint-newline :mandatory)))

(defmethod pp-tex* :around ((decl auto-rewrite-decl))
  (with-slots (rewrite-names semi) decl
    (when (or (not *pretty-printing-decl-list*)
	      (not *pretty-printed-prefix*))
      (when *pretty-printing-decl-list*
	(setq *pretty-printed-prefix* t))
      (typecase decl
	(auto-rewrite-plus-decl (pp-tex-keyword 'AUTO_REWRITE+))
	(auto-rewrite-minus-decl (pp-tex-keyword 'AUTO_REWRITE-))
	(t (pp-tex-keyword 'AUTO_REWRITE)))
      (write-char #\space)
      (pprint-newline :miser))
    (pp-tex-rewrite-names rewrite-names)
    (when semi
      (write-char #\;))
    (pprint-newline :mandatory)))

(defun pp-tex-rewrite-names (names)
  (pprint-logical-block (nil names)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

;;; Type expressions

(defmethod pp-tex* :around ((te type-expr))
  (if (print-type te)
      (pp-tex* (print-type te))
      (progn (dotimes (p (parens te))
	       (write-char #\[))
	     (call-next-method)
	     (dotimes (p (parens te))
	       (write-char #\])))))

(defmethod pp-tex* ((te type-application))
  (with-slots (type parameters) te
    (pprint-logical-block (nil nil)
      (pp-tex* type)
      (pp-tex-arguments parameters))))

(defun pp-tex-arguments* (args)
  (pprint-logical-block (nil args)
    (loop (pprint-indent :current 2)
	  (let ((nextargs (pprint-pop)))
	    (if (and (singleton? nextargs)
		     (typep (car nextargs) 'quoted-assign))
		(pp-tex* (car nextargs))
		(pp-tex-arguments nextargs)))
	  (pprint-exit-if-list-exhausted)
	  (unless *in-tex-math-mode*
	    (pprint-newline :fill)))))

(defmethod pp-tex* ((ex id-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (pp-tex-id (id ex))))

(defmethod pp-tex* ((ex proj-assign))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (pp-tex-number (number ex))))

(defun pp-tex-arguments (args)
  (pprint-logical-block
      (nil args
	       :prefix (get-pp-tex-id '\()
	       :suffix (get-pp-tex-id '\)))
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (unless *in-tex-math-mode*
	    (pprint-newline :fill)))))

(defmethod pp-tex* ((te subtype))
  (with-slots (supertype predicate) te
    (let* ((bindings (if (typep (predicate te) 'binding-expr)
			 (bindings (predicate te))
			 (let* ((id (make-new-variable '|x| te))
				(bd (mk-bind-decl id
				      (or (and (supertype te)
					       (print-type (supertype te)))
					  (supertype te)))))
			   (list bd))))
	   (expr (if (null (predicate te))
		     (formula te)
		     (if (typep (predicate te) 'binding-expr)
			 (expression (predicate te))
			 (let ((var (mk-name-expr (id (car bindings)))))
			   (mk-application (predicate te) var))))))
      (pprint-logical-block (nil nil :prefix "\\{" :suffix "\\}")
	(pp-tex-bindings bindings)
	(pprint-indent :block 8)
	(write-char #\space)
	(write-char #\|)
	(write-char #\space)
	(pprint-newline :fill)
	(pp-tex* expr)))))

(defmethod pp-tex* ((te expr-as-type))
  (with-slots (expr) te
    (pp-tex-id '\()
    (pp-tex* expr)
    (pp-tex-id '\))))

(defmethod pp-tex* ((te recordtype))
  (with-slots (fields) te
    (pprint-logical-block
	(nil fields
	       :prefix (get-pp-tex-id '\[\#)
	       :suffix (get-pp-tex-id '\#\]))
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :linear)))))

(defmethod pp-tex* ((te struct-sub-recordtype))
  (with-slots (fields) te
    (if fields
	(pprint-logical-block (nil fields
	       :prefix (get-pp-tex-id '\[\#)
	       :suffix (get-pp-tex-id '\#\]))
	  (loop (pp-tex* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :linear)))
	(progn (write (get-pp-tex-id '\[\#))
	       (write #\space)
	       (write (get-pp-tex-id '\#\]))))))

(defmethod pp* ((te struct-sub-tupletype))
  (with-slots (types) te
    (if types
	(pprint-logical-block
	    (nil types
		 :prefix (get-pp-tex-id '\[)
		 :suffix (get-pp-tex-id '\]))
	  (loop (pp-tex* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :linear)))
	(progn (write (get-pp-tex-id '\[))
	       (write #\space)
	       (write (get-pp-tex-id '\]))))))

(defmethod pp-tex* ((te funtype))
  (with-slots (domain range) te
    (pprint-logical-block
	(nil nil
	     :prefix (get-pp-tex-id '\[)
	     :suffix (get-pp-tex-id '\]))
      (pprint-indent :current 2)
      (pp-tex-funtype domain range)
      (pprint-indent :block 0))))

(defmethod pp-tex-funtype (domain range)
  (pp-tex* domain)
  (write-char #\space)
  (pp-tex-keyword '->)
  (write-char #\space)
  (pprint-newline :fill)
  (pp-tex* range))

(defmethod pp-tex-funtype ((domain domain-tupletype) range)
  (with-slots (types) domain
    (pprint-logical-block (nil types)
      (pprint-indent :current 0)
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))
    (write-char #\space)
    (pp-tex-keyword '->)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* range)))

(defmethod pp-tex-funtype ((domain dep-binding) range)
  (with-slots (id declared-type) domain
    (if (typep declared-type 'dep-domain-tupletype)
	(let* ((types (types declared-type))
	       (bindings (var-bindings declared-type))
	       (ctr 0)
	       (*parsing-or-unparsing* t))
	  (pprint-logical-block (nil types)
	    (pprint-indent :current 0)
	    (loop (let ((nty (pprint-pop)))
		    (incf ctr)
		    (if (typep nty 'dep-binding)
			(pp-tex* nty)
			(let ((var (car (rassoc ctr bindings))))
			  (if var
			      (pprint-logical-block (nil nil)
				(pp-tex* var)
				(write-char #\:)
				(write-char #\space)
				(pprint-newline :fill)
				(pp-tex* nty))
			      (pp-tex* nty)))))
		  (pprint-exit-if-list-exhausted)
		  (write-char #\,)
		  (write-char #\space)
		  (pprint-newline :fill)))
	  (write-char #\space)
	  (pp-tex-keyword '->)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp-tex* (gensubst range
		     #'(lambda (ex)
			 (let ((var (car (rassoc (index ex) bindings))))
			   (make-instance 'name-expr
			     :id var)))
		     #'(lambda (ex)
			 (and (typep ex 'projection-application)
			      (typep (argument ex) 'name-expr)
			      (eq (id (argument ex)) (id domain)))))))
	(call-next-method))))



(defmethod pp-tex* ((te tupletype))
  (with-slots (types) te
    (pprint-logical-block
	(nil types
	     :prefix (get-pp-tex-id '\[)
	     :suffix (get-pp-tex-id '\]))
      (pprint-indent :current 0)
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod pp-tex* ((te cotupletype))
  (with-slots (types) te
    (pprint-logical-block
	(nil types
	     :prefix (get-pp-tex-id '\[)
	     :suffix (get-pp-tex-id '\]))
      (pprint-indent :current 0)
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\space)
	    (write-char #\+)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defmethod pp-tex* ((te quant-type))
  (pprint-logical-block (nil nil)
    (write (if (forall-type? te)
	       (pp-tex-keyword 'FORALL)
	       (pp-tex-keyword 'EXISTS)))
    (pprint-indent :current 1)
    (write-char #\space)
    (pprint-newline :miser)
    (pp-tex-lambda-formal (pp-chained-decls (bindings te)) nil nil)
    (pprint-indent :block 2)
    (write-char #\:)
    (write-char #\space)
    (pprint-newline :fill)
    (pp-tex* (type te))))


;;; Expressions

(defmethod pp-tex* :around ((ex expr))
  (if (and *ppmacros*
	   (from-macro ex))
      (pp-tex* (from-macro ex))
      (if (typep ex 'binding)
	  (call-next-method)
	  (progn (dotimes (p (parens ex))
		   (pp-tex-id '\())
		 (call-next-method)
		 (dotimes (p (parens ex))
		   (pp-tex-id '\)))))))

(defmethod pp-tex* :around ((ex infix-application))
  (cond ((and *pp-print-parens*
	      (zerop (parens ex)))
	 (pp-tex-id '\()
	 (call-next-method)
	 (pp-tex-id '\)))
	(t (call-next-method))))

(defmethod pp-tex* ((ex number-expr))
  (pp-tex-number (number ex)))

(defun pp-tex-number (number)
  (if *in-tex-math-mode*
      (write number)
      (let* ((len (length (format nil "~d" number)))
	     (str (make-new-tex-string len)))
	(setf (gethash str *pvs-tex-substitution-hash*)
	      (format nil "\\(~d\\)" number))
	(write str))))

(defmethod pp-tex* ((ex decimal-integer))
  (cond (*in-tex-math-mode*
	 (write (number ex))
	 (write-char #\.)
	 (dotimes (i (fractional-length ex))
	   (write-char #\0)))
	(t (let* ((len (length (format nil "~,vf"
				 (fractional-length ex) (number ex))))
		  (str (make-new-tex-string len)))
	     (setf (gethash str *pvs-tex-substitution-hash*)
		   (format nil "\\(~,vf\\)"
		     (fractional-length ex) (number ex)))
	     (write str)))))

(defmethod pp-tex* ((ex string-expr))
  (unless (string-value ex)
    (setf (string-value ex) (pp-string-expr (argument ex))))
  (write (string-value ex) :escape t))

(defmethod pp-tex* ((ex list-expr))
  (if (valid-list-expr? ex)
      (pprint-logical-block
	  (nil (list-arguments ex)
	       :prefix (get-pp-tex-id '\(\:)
	       :suffix (get-pp-tex-id '\:\)))
	(pprint-indent :current 0)
	(loop (pp-tex* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :fill)))
      (call-next-method)))

(defmethod pp-tex* ((ex null-expr))
  (pp-tex-keyword '|(:|)
  (pp-tex-keyword '|:)|))

(defmethod pp-tex* ((ex bracket-expr))
  (pprint-logical-block
      (nil (arguments ex)
	   :prefix (get-pp-tex-id '\[\|)
	   :suffix (get-pp-tex-id '\|\]))
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp-tex* ((ex paren-vbar-expr))
  (pprint-logical-block
      (nil (arguments ex)
	   :prefix (get-pp-tex-id '\(\|)
	   :suffix (get-pp-tex-id '\|\)))
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp-tex* ((ex brace-vbar-expr))
  (pprint-logical-block
      (nil (arguments ex)
	   :prefix (get-pp-tex-id '\{\|)
	   :suffix (get-pp-tex-id '\|\}))
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp-tex* ((ex record-expr))
  (with-slots (assignments) ex
    (pprint-logical-block
	(nil assignments
	   :prefix (get-pp-tex-id '\(\#)
	   :suffix (get-pp-tex-id '\#\)))
      (pprint-indent :current 0)
      (loop (pp-tex* (pprint-pop))
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :linear)))))

(defmethod pp-tex* ((ex tuple-expr))
  (with-slots (exprs) ex
    (pp-tex-arguments exprs)))

(defmethod pp-tex* ((ex projection-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals)))))

(defmethod pp-tex* ((ex injection-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals)))))

(defmethod pp-tex* ((ex injection?-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals)))))

(defmethod pp-tex* ((ex extraction-expr))
  (with-slots (id actuals) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals)))))

(defmethod pp-tex* ((ex projection-application))
  (with-slots (id actuals argument) ex
    (if (and *pp-new-projection-forms*
	     (null actuals))
	(pprint-logical-block (nil nil)
	  (if (and (zerop (parens (argument ex)))
		   (< (precedence (argument ex) 'left)
		      (precedence 'sbst::|`| 'right)))
	      (progn (pp-tex-id '\()
		     (pp-tex* (argument ex))
		     (pp-tex-id '\)))
	      (pp-tex* (argument ex)))
	  (write '|`|)
	  (pp-tex-number (index ex)))
	(pprint-logical-block (nil nil)
	  (pp-tex-id id)
	  (when actuals
	    (pp-tex-actuals actuals))
	  (pp-tex-arguments (argument-list argument))))))

(defmethod pp-tex* ((ex projappl))
  (if (actuals ex)
      (call-next-method)
      (pprint-logical-block (nil nil)
	(if (and (zerop (parens (argument ex)))
		 (< (precedence (argument ex) 'left)
		    (precedence 'sbst::|`| 'right)))
	    (progn (pp-tex-id '\()
		   (pp-tex* (argument ex))
		   (pp-tex-id '\)))
	    (pp-tex* (argument ex)))
	(write '|`|)
	(pp-tex-number (index ex)))))

(defmethod pp-tex* ((ex injection-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals))
      (pp-tex-arguments (argument-list argument)))))

(defmethod pp-tex* ((ex injection?-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals))
      (pp-tex-arguments (argument-list argument)))))

(defmethod pp-tex* ((ex extraction-application))
  (with-slots (id actuals argument) ex
    (pprint-logical-block (nil nil)
      (pp-tex-id id)
      (when actuals
	(pp-tex-actuals actuals))
      (pp-tex-arguments (argument-list argument)))))

(defmethod pp-tex* ((ex field-application))
  (with-slots (id argument) ex
    (if *pp-new-projection-forms*
	(pprint-logical-block (nil nil)
	  (if (and (zerop (parens (argument ex)))
		   (< (precedence (argument ex) 'left)
		      (precedence 'sbst::|`| 'right)))
	      (progn (pp-tex-id '\()
		     (pp-tex* (argument ex))
		     (pp-tex-id '\)))
	      (pp-tex* (argument ex)))
	  (write '|`|)
	  (pp-tex-id (id ex)))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp-tex-id id)
	  (unless *in-tex-math-mode*
	    (pprint-newline :fill))
	  (pp-tex-arguments (argument-list argument))))))

(defmethod pp-tex* ((ex fieldappl))
  (pprint-logical-block (nil nil)
    (if (and (zerop (parens (argument ex)))
	     (< (precedence (argument ex) 'left)
		(precedence 'sbst::|`| 'right)))
	(progn (pp-tex-id '\()
	       (pp-tex* (argument ex))
	       (pp-tex-id '\)))
	(pp-tex* (argument ex)))
    (write '|`|)
    (pp-tex-id (id ex))))

(defmethod pp-tex* ((ex fieldex))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (pp-tex-id (id ex))
    (when (actuals ex)
      (pprint-newline :fill)
      (pp-tex-actuals (actuals ex)))))

(defmethod pp-tex* ((ex projex))
  (pprint-logical-block (nil nil)
    (write-char #\`)
    (pp-tex-number (index ex))
    (when (actuals ex)
      (pprint-newline :fill)
      (pp-tex-actuals (actuals ex)))))

(defmethod pp-tex* ((ex implicit-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp-tex* (argument ex))))

(defmethod pp-tex* ((ex argument-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp-tex* (operator ex))))

(defmethod pp-tex* ((ex lambda-conversion))
  (if *show-conversions*
      (call-next-method)
      (pp-tex* (expression ex))))

(defmethod pp-tex* :around ((ex application))
  (with-slots (operator argument) ex
    (if (typep ex '(or bracket-expr paren-vbar-expr brace-vbar-expr))
	(call-next-method)
	(let* ((args (argument* ex))
	       (arglengths (mapcar #'(lambda (arg)
				       (if (typep arg 'tuple-expr)
					   (length (exprs arg))
					   1))
			     args))
	       (appltrans (get-pp-tex-funsym operator arglengths)))
	  (cond (appltrans
		 (unless *in-tex-math-mode*
		   (write "\\("))
		 (let ((*in-tex-math-mode* t))
		   (dotimes (p (parens ex))
		     (pp-tex-id '\())
		   (write appltrans)
		   (dolist (arg args)
		     (cond ((typep arg 'tuple-expr)
			    (dolist (a (exprs arg))
			      (write "{")
			      (pp-tex* a)
			      (write "}")))
			   (t (write "{")
			      (pp-tex* arg)
			      (write "}"))))
		   (dotimes (p (parens ex))
		     (pp-tex-id '\))))
		 (unless *in-tex-math-mode*
		   (write "\\)")))
		(t (call-next-method)))))))

(defmethod pp-tex* ((ex application))
  (let ((operator (get-pp-operator* (operator ex)))
	(args (get-pp-argument* (operator ex) (list (argument ex)))))
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (and (not *in-tex-math-mode*)
	       (zerop (parens operator))
	       (< (precedence operator 'left)
		  (precedence (car args) 'right)))
	  (let ((*in-tex-math-mode* t))
	    (write "\\(")
	    (pp-tex* operator)
	    (write "\\)"))
	  (pp-tex* operator))
      (pprint-indent :block 4)
      (unless *in-tex-math-mode*
	(if (simple-name? operator)
	    (pprint-newline :miser)
	    (pprint-newline :fill)))
      (pp-tex-arguments-list args))))

(defun pp-tex-arguments-list (args)
  (pprint-logical-block (nil args)
    (loop (pp-tex-arguments (pp-argument-list (pprint-pop)))
	  (pprint-exit-if-list-exhausted)
	  (unless *in-tex-math-mode*
	    (pprint-newline :fill)))))

(defmethod pp-tex* ((ex infix-application))
  (with-slots (operator argument) ex
    (if (and (typep operator 'name-expr)
	     (memq (id operator) *infix-operators*)
	     (typep argument 'tuple-expr)
	     (= (length (exprs argument)) 2)
	     (not (or (mod-id operator)
		      (library operator)
		      (actuals operator)
		      (mappings operator))))
	(let ((lhs (car (exprs argument)))
	      (rhs (cadr (exprs argument)))
	      (oper (sbst-symbol (id operator))))
	  (pprint-logical-block (nil nil)
	    (pprint-indent :block 1)
	    (if (and (zerop (parens lhs))
		     (< (precedence lhs 'left)
			(gethash oper (second *expr-prec-info*))))
		(progn (pp-tex-id '\()
		       (pp-tex* lhs)
		       (pp-tex-id '\)))
		(pp-tex* lhs))
	    (write-char #\space)
	    (unless *in-tex-math-mode*
	      (pprint-newline :fill))
	    (pp-tex-id (if (eq (id operator) 'O) '|o| (id operator))
		       (when (resolution operator)
			 (id (module-instance operator))))
	    (write-char #\space)
	    (unless *in-tex-math-mode*
	      (pprint-newline :fill))
	    (if (and (zerop (parens rhs))
		     (< (precedence rhs 'right)
			(gethash oper (third *expr-prec-info*))))
		(progn (pp-tex-id '\()
		       (pp-tex* rhs)
		       (pp-tex-id '\)))
		(pp-tex* rhs))))
	(call-next-method))))

;; pp-tex* ((ex infix-conjunction)) 

;; pp-tex* ((ex infix-disjunction)) 

(defmethod pp-tex* ((ex unary-application))
  (with-slots (operator argument) ex
    (if (and (typep operator 'name-expr)
	     (not (mod-id operator))
	     (not (library operator))
	     (memq (id operator) *unary-operators*))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp-tex-id (id operator))
	  (when (valid-pvs-id* (id operator))
	    (write-char #\space)
	    (pprint-newline :miser))
	  (if (>= (precedence argument 'right)
		      (gethash (sbst-symbol (id operator))
			       (first *expr-prec-info*)))
	      (pp-tex* argument)
	      (pprint-logical-block
		  (nil nil
		       :prefix (get-pp-tex-id '\()
		       :suffix (get-pp-tex-id '\)))
		(pp-tex* argument))))
	(call-next-method))))

(defmethod pp-tex* ((ex binding-application))
  (with-slots (operator argument) ex
    (if (lambda-expr? argument)
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp-tex-id (id operator))
	  (write-char #\!)
	  (write-char #\space)
	  (pprint-newline :miser)
	  (pp-tex-lambda-formals argument)
	  (write-char #\:)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp-tex* (expression argument)))
	(call-next-method))))

(defmethod pp-tex* ((ex when-expr))
  (with-slots (operator argument) ex
    (if (real-projected-arg-tuple-expr? argument)
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp-tex-keyword 'WHEN)
	  (pp-tex-id '\()
	  (pp-tex* (argument (car (exprs argument))))
	  (pp-tex-id '\)))
	(pprint-logical-block (nil nil)
	  (pprint-indent :current 2)
	  (pp-tex* (args2 ex))
	  (write-char #\space)
	  (pprint-newline :miser)
	  (pp-tex-keyword 'WHEN)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp-tex* (args1 ex))))))

(defmethod pp-tex* ((ex if-expr))
  (pprint-logical-block (nil nil)
    (pprint-indent :current 2)
    (pp-tex-keyword 'IF)
    (write-char #\space)
    (pp-tex* (condition ex))
    (write-char #\space)
    (pprint-newline :linear)
    (pp-tex-keyword 'THEN)
    (write-char #\space)
    (pp-tex* (then-part ex))
    (write-char #\space)
    (pprint-indent :block 0)
    (pprint-newline :linear)
    (cond ((chained-else? (else-part ex))
	   (pp-tex-keyword 'ELSIF)
	   (pp-tex-chained-if-expr (else-part ex) nil))
	  (t (pp-tex-keyword 'ELSE)
	     (write-char #\space)
	     (pp-tex* (else-part ex))))
    (write-char #\space)
    (pprint-newline :linear)
    (pp-tex-keyword 'ENDIF)))

(defmethod pp-tex* ((ex chained-if-expr))
  (pprint-logical-block (nil nil)
    (pp-tex-chained-if-expr ex t)))

(defun pp-tex-chained-if-expr (ex print-if?)
  (when print-if?
    (pp-tex-keyword 'IF))
  (write-char #\space)
  (pp-tex* (condition ex))
  (write-char #\space)
  (pprint-indent :block 2)
  (pprint-newline :fill)
  (pp-tex-keyword 'THEN)
  (write-char #\space)
  (pp-tex* (then-part ex))
  (write-char #\space)
  (pprint-indent :block 0)
  (pprint-newline :linear)
  (cond ((typep (else-part ex) 'chained-if-expr)
	 (pp-tex-keyword 'ELSIF)
	 (pp-tex-chained-if-expr (else-part ex) nil))
	(t (pp-tex-keyword 'ELSE)
	   (write-char #\space)
	   (pp-tex* (else-part ex)))))

(defmethod pp-tex* ((ex coercion))
  (with-slots (argument operator) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp-tex* argument)
      (write "::")
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex* (coercion-declared-type operator)))))

(defmethod pp-tex* ((ex binding-expr))
  (let ((*pretty-printing-decl-list* t))
    (multiple-value-bind (bindings-list expr)
	(pp-unchain-binding-expr (expression ex) (list (bindings ex))
				 (operator ex))
      (pprint-logical-block (nil nil)
	(pp-tex-keyword (operator ex))
	(pprint-indent :current 1)
	(write-char #\space)
	(pprint-newline :miser)
	(pprint-logical-block (nil bindings-list)
	  (loop (let ((bindings (pprint-pop)))
		  (pp-tex-lambda-formal (pp-chained-decls bindings)
					(commas? ex)
					nil))
		(pprint-exit-if-list-exhausted)
		(pprint-newline :fill)))
	(pprint-indent :block 2)
	(write ": ")
	(pprint-newline :fill)
	(pp-tex* expr)))))

(defmethod pp-tex* ((ex set-expr))
  (with-slots (bindings expression) ex
    (pprint-logical-block (nil nil :prefix "\\{" :suffix "\\}")
      (pprint-indent :current 2)
      (pp-tex-bindings bindings)
      (write-char #\space)
      (pprint-newline :fill)
      (write-char #\|)
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex* expression))))

(defmethod pp-tex* ((ex set-list-expr))
  (with-slots (exprs) ex
    (pprint-logical-block
	(nil exprs :prefix "\\{:" :suffix " :\\}")
      (when exprs
	(write-char #\space)
	(loop (pp-tex* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :fill))))))

(defmethod pp-tex* ((ex let-expr))
  (if (lambda-expr? (operator ex))
      (multiple-value-bind (let-bindings expr)
	  (get-let-bindings ex)
	(pprint-logical-block (nil nil)
	  (pp-tex-keyword 'LET)
	  (write-char #\space)
	  (pp-tex-let-bindings let-bindings)
	  (write-char #\space)
	  (pprint-indent :block 2)
	  (pprint-newline :fill)
	  (pp-tex-keyword 'IN)
	  (write-char #\space)
	  (pprint-newline :fill)
	  (pp-tex* expr)))
      (call-next-method)))

(defun pp-tex-let-bindings (let-bindings)
  (pprint-logical-block (nil let-bindings)
    (loop (let ((lb (pprint-pop)))
	    (if (cdr (car lb))
		(pprint-logical-block
		    (nil nil
			 :prefix (get-pp-tex-id '\()
			 :suffix (get-pp-tex-id '\)))
		  (pp-tex-bindings (car lb)))
		(if (cadr lb)
		    (pp-tex-id (id (caar lb)))
		    (pp-tex-bindings (car lb))))
	    (when (cadr lb)
	      (pp-tex-decl-formals (cadr lb))
	      (when (declared-type (caar lb))
		(write ": ")
		(pp-tex* (get-let-binding-range (declared-type (caar lb))
						(cadr lb)))))
	    (write-char #\space)
	    (pprint-indent :block 3)
	    (pprint-newline :fill)
	    (pp-tex-keyword '=)
	    (write-char #\space)
	    (pprint-newline :fill)
	    (pp-tex* (caddr lb))
	    (pprint-indent :block 0))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp-tex* ((ex where-expr))
  (multiple-value-bind (where-bindings expr)
      (get-where-bindings ex)
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp-tex* expr)
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword 'WHERE)
      (write-char #\space)
      (pp-tex-let-bindings where-bindings))))

(defmethod pp-tex* ((ex update-expr))
  (with-slots (expression assignments) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (and (zerop (parens expression))
	       (< (precedence expression 'left)
		  (precedence 'WITH 'right)))
	  (progn (pp-tex-id '\()
		 (pp-tex* expression)
		 (pp-tex-id '\)))
	  (pp-tex* expression))
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword 'WITH)
      (write-char #\space)
      (pprint-logical-block
	  (nil assignments
	       :prefix (get-pp-tex-id '\[)
	       :suffix (get-pp-tex-id '\]))
	(pprint-indent :current 0)
	(loop (pp-tex* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :linear))))))

(defmethod pp-tex* ((ex cond-expr))
  (pp-tex-cond-expr ex))

(defmethod pp-tex* ((ex first-cond-expr))
  (pp-tex-cond-expr ex))

(defmethod pp-tex* ((ex single-cond-expr))
  (pp-tex-cond-expr ex))

(defmethod pp-tex* ((ex last-cond-expr))
  (pp-tex-cond-expr ex))

(defun pp-tex-cond-expr (ex)
  (let* ((pairs (collect-cond-expr-pairs ex nil))
	 (*pp-else-cond-exprs* (if (else-condition? (caar (last pairs)))
				   (cons (caar (last pairs))
					 *pp-else-cond-exprs*)
				   *pp-else-cond-exprs*)))
    (pprint-logical-block (nil nil)
      (pp-tex-keyword 'COND)
      (write-char #\space)
      (pprint-newline :miser)
      (pprint-logical-block (nil pairs)
	(loop (let ((pair (pprint-pop)))
		(pp-tex* (car pair))
		(pprint-indent :block 2)
		(write-char #\space)
		(pprint-newline :fill)
		(pp-tex-keyword '->)
		(write-char #\space)
		(pprint-newline :fill)
		(pp-tex* (cdr pair))
		(pprint-indent :block 0)
		(pprint-exit-if-list-exhausted)
		(write-char #\,)
		(write-char #\space)
		(pprint-newline :linear))))
      (write-char #\space)
      (pprint-newline :linear)
      (pp-tex-keyword 'ENDCOND))))

(defmethod pp-tex* ((ex else-condition))
  (if (memq ex *pp-else-cond-exprs*)
      (pp-tex-keyword 'ELSE)
      (call-next-method)))

(defmethod pp-tex* ((ex cases-expr))
  (with-slots (expression selections else-part) ex
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp-tex-keyword 'CASES)
      (write-char #\space)
      (pp-tex* expression)
      (write-char #\space)
      (pp-tex-keyword 'OF)
      (write-char #\space)
      (pprint-newline :fill)
      (pprint-logical-block (nil selections)
	(loop (pp-tex* (pprint-pop))
	      (pprint-exit-if-list-exhausted)
	      (write-char #\,)
	      (write-char #\space)
	      (pprint-newline :linear)))
      (when else-part
	(write-char #\space)
	(pprint-newline :linear)
	(pp-tex-keyword 'ELSE)
	(write-char #\space)
	(pp-tex* else-part))
      (write-char #\space)
      (pprint-newline :linear)
      (pp-tex-keyword 'ENDCASES))))

(defmethod pp-tex* ((sel selection))
  (with-slots (constructor args expression) sel
    (let ((appltrans (get-pp-tex-funsym constructor (list (length args)))))
      (pprint-logical-block (nil nil)
	(pprint-indent :current 2)
	(cond (appltrans
	       (unless *in-tex-math-mode*
		 (write "\\("))
	       (let ((*in-tex-math-mode* t))
		 (write appltrans)
		 (dolist (arg args)
		   (write "{")
		   (pp-tex-id (id arg))
		   (write "}")))
	       (unless *in-tex-math-mode*
		 (write "\\)")))
	    (t (pp-tex-id (id constructor))
	       (when args
		 (pprint-logical-block
		     (nil args
			  :prefix (get-pp-tex-id '\()
			  :suffix (get-pp-tex-id '\)))
		   (loop (pp-tex-id (id (pprint-pop)))
			 (pprint-exit-if-list-exhausted)
			 (write-char #\,)
			 (write-char #\space)
			 (pprint-newline :fill))))))
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :fill)
	(pp-tex* expression)))))

(defmethod pp-tex* ((ass assignment))
  (with-slots (arguments expression) ass
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (typep ass 'uni-assignment)
	  (pp-tex* (caar arguments))
	  (pp-tex-arguments* arguments))
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword '|:=|)
      (write-char #\space)
      (pp-tex* expression))))

(defmethod pp-tex* ((ass maplet))
  (with-slots (arguments expression) ass
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (if (typep ass 'uni-assignment)
	  (pp-tex* (caar arguments))
	  (pp-tex-arguments* arguments))
      (write-char #\space)
      (pprint-newline :fill)
      (pp-tex-keyword '|\|->|)
      (write-char #\space)
      (pp-tex* expression))))

(defmethod pp-tex* ((ex table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-tex-table-expr row-expr col-expr row-headings col-headings
		       table-entries)))

(defmethod pp-tex* ((ex cond-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-tex-table-expr row-expr col-expr row-headings col-headings
		       table-entries)))

(defmethod pp-tex* ((ex cases-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-tex-table-expr row-expr col-expr row-headings col-headings
		       table-entries)))

(defmethod pp-tex* ((ex let-table-expr))
  (with-slots (row-expr col-expr row-headings col-headings table-entries) ex
    (pp-tex-table-expr row-expr col-expr row-headings col-headings
		       table-entries)))

(defun pp-tex-table-expr (row-expr col-expr row-headings col-headings
				   table-entries)
  (pprint-logical-block (nil nil)
    (pp-tex-keyword 'TABLE)
    (write-char #\space)
    (pprint-newline :miser)
    (when row-expr
      (pp-tex* row-expr))
    (when col-expr
      (write-char #\,)
      (write-char #\space)
      (pprint-newline :miser)
      (pp-tex* col-expr))
    (pprint-indent :block 2)
    (pprint-newline :mandatory)
    (format t "\\end{alltt}~%\\begin{tabular}{|*{~d}{c|}}\\hline~%"
      (1+ (length (car table-entries))))
    (let ((rows (if row-headings
		    (mapcar #'cons row-headings table-entries)
		    table-entries)))
      (when col-headings
	(write-char #\&)
	(pprint-logical-block (nil col-headings)
	  (loop (pp-tex* (pprint-pop))
		(pprint-exit-if-list-exhausted)
		(write-char #\&))))
      (format t "\\\\\\hline~%")
      (pprint-newline :mandatory)
      (pprint-logical-block (nil rows)
	(loop (let ((row (pprint-pop)))
		(pprint-logical-block (nil row)
		  (loop (pp-tex* (pprint-pop))
			(pprint-exit-if-list-exhausted)
			(write-char #\&)))
		(pprint-exit-if-list-exhausted)
		(format t "\\\\\\hline~%")
		(pprint-newline :mandatory))))
      (pprint-indent :block 0)
      (format t "\\\\\\hline~%\\end{tabular}~%\\begin{alltt}~%")
      (pprint-newline :mandatory)
      (pp-tex-keyword 'ENDTABLE))))

(defun pp-tex-bindings (bindings)
  (pprint-logical-block (nil bindings)
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :linear))))

(defmethod pp-tex* ((d simple-decl))
  (with-slots (id declared-type) d
    (pprint-logical-block (nil nil)
      (pprint-indent :current 2)
      (pp-tex-id id)
      (when declared-type
	(write-char #\:)
	(write-char #\space)
	(pprint-newline :fill)
	(pp-tex* declared-type)))))

(defmethod pp-tex* ((ex name))
  (with-slots (library mod-id actuals id mappings target) ex
    (pprint-logical-block (nil (list ex))
      (pprint-indent :block 4)
      (when library
	(pp-tex-id library)
	(write-char #\@))
      (cond (mod-id
	     (pp-tex-id mod-id)
	     (when actuals
	       (pprint-newline :fill)
	       (pp-tex-actuals actuals))
	     (when mappings
	       (pprint-newline :fill)
	       (pp-tex-mappings mappings))
	     (when target
	       (write " :-> ")
	       (pprint-newline :fill)
	       (pp-tex* target))
	     (write-char #\.)
	     (pp-tex-id id))
	    (t
	     (pp-tex-id id)
	     (when actuals
	       (pprint-newline :fill)
	       (pp-tex-actuals actuals))
	     (when mappings
	       (pprint-newline :fill)
	       (pp-tex-mappings mappings))
	     (when target
	       (write " :-> ")
	       (pprint-newline :fill)
	       (pp-tex* target)))))))

(defun pp-tex-mappings (mappings)
  (pprint-logical-block
      (nil mappings
	   :prefix (get-pp-tex-id '\{\{)
	   :suffix (get-pp-tex-id '\}\}))
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
	  (pprint-newline :fill))))

(defmethod pp-tex* ((map mapping))
  (pprint-logical-block (nil nil)
    (pp-tex* (lhs map))
    (when (kind map)
      (write ": ")
      (if (eq (kind map) 'expr)
	  (pp-tex* (declared-type map))
	  (pp-tex-keyword (kind map))))
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (typecase map
      (mapping-def (write "="))
      (mapping-subst (write ":="))
      (mapping-rename (write "::=")))
    (write-char #\space)
    (if (rhs map)
	(pp-tex* (expr (rhs map)))
	(pp-tex-keyword 'NORHS))))

(defmethod pp-tex* ((map mapping-with-formals))
  (pprint-logical-block (nil nil)
    (pp-tex* (lhs map))
    (pp-tex-decl-formals (formals map))
    (when (kind map)
      (write ": ")
      (if (eq (kind map) 'expr)
	  (pp-tex* (declared-type map))
	  (pp-tex-keyword (kind map))))
    (pprint-indent :block 2)
    (write-char #\space)
    (pprint-newline :fill)
    (typecase map
      (mapping-def (write "="))
      (mapping-subst (write ":="))
      (mapping-rename (write "::=")))
    (write-char #\space)
    (if (rhs map)
	(pp-tex* (remove-map-formals (formals map) (expr (rhs map))))
	(pp-tex-keyword 'NORHS))))

(defmethod pp-tex* ((ex eager-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write "!")))

(defmethod pp-tex* ((ex macro-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write "!!")))

(defmethod pp-tex* ((ex constant-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write ": ")
    (pprint-newline :fill)
    (pp-tex* (declared-type ex))))

(defmethod pp-tex* ((ex formula-rewrite-name))
  (pprint-logical-block (nil nil)
    (call-next-method)
    (write ": ")
    (pprint-newline :fill)
    (write (spelling ex))))

(defmethod pp-tex* ((ex fnum-rewrite))
  (write (fnum ex)))

(defmethod pp-tex* ((list list))
  (if (and list
	   (or (and (every #'declaration? list)
		    (every #'module list))
	       (every #'importing? list)))
      (pp-tex-theory list)
      (let ((*pretty-printing-decl-list* t)
	    (*pretty-printed-prefix* nil))
	(dolist (elt list)
	  (pp-tex* elt)))))

(defun pp-tex-actuals (actuals)
  (pprint-logical-block
      (nil actuals
	   :prefix (get-pp-tex-id '\[)
	   :suffix (get-pp-tex-id '\]))
    (pprint-indent :current 0)
    (loop (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write-char #\,)
	  (write-char #\space)
	  (pprint-newline :fill))))

(defmethod pp-tex* ((act actual))
  (with-slots (expr) act
    (pp-tex* expr)))

(defun pp-tex-lambda-formals (expr)
  (pp-tex-lambda-formal (pp-tex-chained-decls (bindings expr))
		    (commas? expr)
		    (set-expr? (typep expr 'set-expr)))
  (when (and (typep (expression expr) 'lambda-expr)
	     (chain? (expression expr)))
    (pp-tex-lambda-formals (expression expr))))

(defun pp-tex-lambda-formal (bindings commas? &optional set-expr?)
  (if commas?
      (if (cdr bindings)
	  (pprint-logical-block (nil nil)
	    (if (or (cdr (car bindings))
		    (and (declared-type (caar bindings))
			 (not (typep (caar bindings) 'untyped-bind-decl))))
		(pp-tex-lambda-adformals (car bindings))
		(if set-expr?
		    (pprint-logical-block (nil nil)
		      (pp-tex-id (id (caar bindings)))
		      (when (and (declared-type (caar bindings))
				 (not (typep (caar bindings)
					     'untyped-bind-decl)))
			(write-char #\:)
			(write-char #\space)
			(pp (declared-type (caar bindings)))))
			(pp-tex-id (id (caar bindings)))))
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)
	    (pp-tex-lambda-formal (cdr bindings) commas? set-expr?))
	  (if (and set-expr?
		   (zerop (parens (caar bindings))))
	      (if (cdar bindings)
		  (pp-tex-lambda-formal (mapcar #'list (car bindings))
				    commas? set-expr?)
		  (pprint-logical-block (nil nil)
		    (pp-tex-id (id (caar bindings)))
		    (when (declared-type (caar bindings))
		      (write-char #\:)
		      (write-char #\space)
		      (pp (declared-type (caar bindings))))))
	      (if (or (cdr (car bindings))
		      (declared-type (caar bindings)))
		  (pprint-logical-block
		      (nil nil
			   :prefix (get-pp-tex-id '\()
			   :suffix (get-pp-tex-id '\)))
		    (pp-tex-paren-adformals* (car bindings)))
		  (pp-tex-id (id (caar bindings))))))
      (if (and set-expr?
	       (zerop (parens (caar bindings))))
	  (pprint-logical-block (nil nil)
	    (pp-tex-id (id (caar bindings)))
	    (when (declared-type (caar bindings))
	      (write-char #\:)
	      (write-char #\space)
	      (pp (declared-type (caar bindings)))))
	  (pp-tex-paren-adformals bindings))))

(defun pp-tex-paren-adformals (bindings)
  (pprint-logical-block
      (nil bindings
	   :prefix (get-pp-tex-id '\()
	   :suffix (get-pp-tex-id '\)))
    (loop (let* ((next (pprint-pop))
		 (parens (if (zerop (parens (car next))) 0 1)))
	    (pp-tex-paren-adformals* next parens)
	    (pprint-exit-if-list-exhausted)
	    (write-char #\,)
	    (write-char #\space)
	    (pprint-newline :fill)))))

(defun pp-tex-paren-adformals* (b &optional (parens (parens (car b))))
  (if (zerop parens)
      (pprint-logical-block (nil nil)
	(mapl #'(lambda (bb)
		  (pp-tex-bind-decl (car bb) nil (cdr bb))
		  (when (cdr bb)
		    (write-char #\,)
		    (write-char #\space)
		    (pprint-newline :fill)))
	      b))
      (pprint-logical-block
	  (nil nil
	       :prefix (get-pp-tex-id '\()
	       :suffix (get-pp-tex-id '\)))
	(pp-tex-paren-adformals* b 0))))

(defun pp-tex-lambda-adformals (bindings)
  (pprint-logical-block
      (nil bindings
	   :prefix (get-pp-tex-id '\()
	   :suffix (get-pp-tex-id '\)))
    (loop (pprint-indent :current 2)
	  (pp-tex* (pprint-pop))
	  (pprint-exit-if-list-exhausted)
	  (write ", ")
	  (pprint-newline :fill))))

;(defun pp-typed-ids (bindings)
;  (pprint-logical-block (nil bindings)
;    (loop (pprint-indent :current 2)
;	  (write (id (pprint-pop)))
;	  (pprint-exit-if-list-exhausted)
;	  (write-char #\,)
;	  (write-char #\space)
;	  (pprint-newline :linear)))
;  (write-char #\:)
;  (write-char #\space)
;  (pp-tex* (declared-type (car (last bindings)))))

(defmethod pp-tex* ((bd untyped-bind-decl))
  (pp-tex-id (id bd)))

(defmethod pp-tex* ((bd bind-decl))
  (pp-tex-bind-decl bd))

(defun pp-tex-bind-decl (bd &optional (parens? t) next-bds)
  (let* ((pnum (if (and parens? (integerp (parens bd)))
		   (if (and (pred-bind-decl? bd)
			    (setsubtype? (declared-type bd)))
		       (max (parens bd) 1)
		       (parens bd))
		   0))
	 (pl (make-string pnum :initial-element #\())
	 (pr (make-string pnum :initial-element #\))))
    (pprint-logical-block (nil nil :prefix pl :suffix pr)
      (pp-tex-id (id bd))
      (when (and (declared-type bd)
		 (not (and (chain? bd) next-bds)))
	(cond ((and (pred-bind-decl? bd)
		    (setsubtype? (declared-type bd)))
	       (when (supertype (declared-type bd))
		 (pprint-indent :block 2)
		 (write-char #\:)
		 (write-char #\space)
		 (pprint-newline :fill)
		 (pp-tex* (supertype (declared-type bd))))
	       (pprint-indent :block 4)
	       (write-char #\space)
	       (pprint-newline :fill)
	       (write-char #\|)
	       (write-char #\space)
	       (pp-tex* (formula (declared-type bd))))
	      (t (pprint-indent :block 2)
		 (write-char #\:)
		 (write-char #\space)
		 (pprint-newline :fill)
		 (pp-tex* (declared-type bd))))))))

(defun pp-tex-chained-decls (decls &optional cdecls part)
  (if (null decls)
      (nreverse (if cdecls (cons (nreverse cdecls) part) part))
      (let ((chainp (and (typep (car decls) '(and (not arg-bind-decl)
					      (or declaration bind-decl)))
			 (chain? (car decls))
			 (or (not (typep (car decls)
					 '(or typed-declaration bind-decl)))
			     (declared-type (car decls))))))
	(pp-tex-chained-decls
	 (cdr decls)
	 (when chainp
	   (cons (car decls) cdecls))
	 (cond (chainp
		part)
	       ((typep (car decls) 'importing)
		(cons (car decls)
		      (if cdecls (cons (nreverse cdecls) part) part)))
	       ((and cdecls
		     (or (and (typep (car cdecls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car cdecls))
			      (typep (car decls)
				     '(or typed-declaration bind-decl))
			      (declared-type (car decls))
			      (ps-eq (declared-type (car cdecls))
				     (declared-type (car decls))))
			 (and (typep (car cdecls) 'type-decl)
			      (typep (car decls) 'type-decl))))
		(cons (nreverse (cons (car decls) cdecls)) part))
	       (t (cons (list (car decls))
			(if cdecls (cons (nreverse cdecls) part) part))))))))
  
(defmethod pp-tex* ((ex symbol))
  (when ex (write ex)))


;;;;; Substitutions

; Identifier  Type  Length  Substitution
; ----------  ----  ------  ------------
; THEORY      key     9     {\large\bf Theory}
; f1          id      3     {\rm bar}
; f2          id[2]   2     {#2_{#1}^{f}}
; f3          2       2     {#1^#2}
; f4          (1 2)   3     {\sum_{i=#2}^{#3}#1(i,#2)}

(defun pp-tex-id (id &optional theory-id)
  (write (get-pp-tex-id id theory-id)))

;; Generates a string of the given length, so that the prettyprinter will
;; insert the right whitespace, and associates this string with the
;; substitution for the second pass in write-string-with-tex-substitutions
;; The string must start with char 128 (DEL), then any number of characters
;; >= 128, though the last one(s) must be > 128.  Note that this means any
;; mapping said to be of length 1 will actually be treated as length 2, or
;; more if there are more than 128 of length 1 or 2.
(defun get-pp-tex-id (symbol &optional theory-id)
  (let* ((msymbol (makesym "~a%" symbol))
	 (thsym (when theory-id (makesym "~a.~a" theory-id symbol)))
	 (mthsym (when theory-id (makesym "~a%" thsym)))
	 (tr (if thsym
		 (if *in-tex-math-mode*
		     (gethash thsym *latex-id-strings*)
		     (gethash mthsym *latex-id-strings*))
		 (if *in-tex-math-mode*
		     (gethash symbol *latex-id-strings*)
		     (gethash msymbol *latex-id-strings*)))))
    (or tr
	(let* ((itrans (or (when thsym
			     (cdr (assq thsym *latex-id-macro-list*)))
			   (cdr (assq symbol *latex-id-macro-list*))))
	       (needs-math (and (not *in-tex-math-mode*) itrans))
	       (trans (or itrans
			  (get-pp-tex-id* symbol)))
	       (len (or (when thsym
			  (cdr (assq thsym *latex-id-length-list*)))
			(cdr (assq symbol *latex-id-length-list*))
			(length (if (symbolp symbol)
				    (string symbol)
				    (format nil "~a" symbol)))))
	       (str (make-new-tex-string len))
	       (strans (if needs-math
			   (format nil "\\(~a\\)" trans)
			   trans)))
	  (setf (gethash (if *in-tex-math-mode*
			     (or thsym symbol)
			     (or mthsym msymbol))
			 *latex-id-strings*)
		str)
	  (setf (gethash str *pvs-tex-substitution-hash*) strans)
	  str))))

;;; Should do something useful with $ in names.

(defun get-pp-tex-id* (symbol)
  (let ((str (if (symbolp symbol)
		 (string symbol)
		 (format nil "~a" symbol))))
    (cond ((and (alpha-char-p (char str 0))
		(every #'digit-char-p (subseq str 1)))
	   (if (= (length str) 1)
	       (if *in-tex-math-mode*
		   str
		   (format nil "\\(~a\\)" str))
	       (format nil "~:[\\(~;~]~c\\sb{~a}~:[\\)~;~]"
		 *in-tex-math-mode* (char str 0) (subseq str 1)
		 *in-tex-math-mode*)))
	  ((find #\! str)
	   (let ((pos (position #\! str)))
	     (if (and (= (- (length str) pos) 2)
		      (digit-char-p (elt str (1+ pos))))
		 (format nil "~:[\\(~;~]~,,v,''a~:[\\)~;~]"
		   *in-tex-math-mode*
		   (parse-integer (subseq str (1+ pos)))
		   (let ((*in-tex-math-mode* t))
		     (get-pp-tex-id* (subseq str 0 pos)))
		   *in-tex-math-mode*)
		 (format nil "\\pvsid{~a}" (latex-protect str)))))
	  (t (format nil "\\pvsid{~a}" (latex-protect str))))))

(defmethod get-pp-tex-funsym ((op name-expr) arglengths &optional theory-id)
  (get-pp-tex-funsym (id op) arglengths
		     (or theory-id
			 (when (resolution op)
			   (id (module-instance op))))))

(defmethod get-pp-tex-funsym (ex arglengths &optional theory-id)
  (declare (ignore ex arglengths theory-id))
  nil)

(defmethod get-pp-tex-funsym ((symbol symbol) arglengths &optional theory-id)
  (let* ((symargs (cons symbol arglengths))
	 (thsym (when theory-id (makesym "~a.~a" theory-id symbol)))
	 (thsymargs (when theory-id (cons thsym arglengths))))
    (multiple-value-bind (texstr there?)
	(if thsym
	    (gethash thsymargs *latex-funsym-strings*)
	    (gethash symargs *latex-funsym-strings*))
      (if there?
	  texstr
	  (let* ((trans (or (when thsym
			     (cdr (assoc thsymargs *latex-fun-sym-macro-list*
					 :test #'equal)))
			   (cdr (assoc symargs *latex-fun-sym-macro-list*
				       :test #'equal))))
		 (len (when trans
			(or (when thsym
			      (cdr (assoc thsymargs *latex-fun-sym-length-list*
					  :test #'equal)))
			    (cdr (assoc symargs *latex-fun-sym-length-list*
					:test #'equal)))))
		 (str (when trans (make-new-tex-string len))))
	    (setf (gethash (or thsymargs symargs) *latex-funsym-strings*)
		  trans)
	    (when trans
	      (setf (gethash str *pvs-tex-substitution-hash*) trans)
	      str))))))

(defun pp-tex-keyword (symbol)
  (write (get-pp-tex-keyword symbol)))

(defun get-pp-tex-keyword (symbol)
  (let* ((sym (if *in-tex-math-mode* (makesym "~a%" symbol) symbol))
	 (texstr (gethash sym *latex-keyword-strings*)))
    (or texstr
	(setf (gethash sym *latex-keyword-strings*)
	      (let* ((ktrans (cdr (assq symbol *latex-keyword-list*)))
		     (trans (if ktrans
				(if *in-tex-math-mode*
				    ktrans
				    (format nil "\\(~a\\)" ktrans))
				(format nil "\\pvskey{~a}"
				  (latex-protect (string symbol)))))
		     (len (or (cdr (assq symbol *latex-keyword-length-list*))
			      (length (string symbol))))
		     (str (make-new-tex-string len)))
		(setf (gethash str *pvs-tex-substitution-hash*) trans)
		str)))))

;; Creates a string starting with char 128, ending with chars > 128.  Hence
;; is always at least two chars.  This is needed in order to recognize when
;; two strings run together.  Since lengths are approximate anyway, it
;; shouldn't matter much.
(defun make-new-tex-string (length)
  (when (< length 2) (setq length 2))
  (let ((cnt (get-next-tex-symbol-counter length))
	(str (make-string length :initial-element (code-char 127))))
    (if (and (= length 2) (> cnt 126))
	(make-new-tex-string 3)
	;; Technically we could need to check whether we used up the length
	;; 3 strings as well, but that would mean we have over 16384
	;; identifiers of length 0, 1, 2, and 3.  Even if someone generated
	;; a spec like this, would they really want to latex-print it?
	(make-new-tex-string* str (1- length) cnt))))

(defun make-new-tex-string* (str pos cnt)
  (if (zerop cnt)
      (progn (assert (char= (char str 0) #\Rubout))
	     (assert (not (char= (char str (1- (length str))) #\Rubout)))
	     str)
      (multiple-value-bind (q r)
	  (floor cnt 127)
	(setf (char str pos) (code-char (+ r 128)))
	(assert (not (char= (char str pos) #\Rubout)))
	(make-new-tex-string* str (1- pos) q))))

(defun get-next-tex-symbol-counter (length)
  (let ((counter (assoc length *tex-symbol-counters* :test #'=)))
    (cond (counter (incf (cdr counter)))
	  (t (push (cons length 1) *tex-symbol-counters*)
	     1))))

; (defun break-pvs-name (string)
;   (let* ((bang-pos (position #\! string))
; 	 (dollar-pos (position #\$ string))
; 	 (namestr (cond (bang-pos (subseq string 0 bang-pos))
; 			(dollar-pos (subseq string 0 dollar-pos))
; 			(t string)))
; 	 (subscr? (and (> (length namestr) 1)
; 		       (alpha-char-p (char namestr 0))
; 		       (every #'digit-char-p (subseq namestr 1))))
; 	 (name1 (if subscr?
; 		    (coerce (list (char namestr 0)) 'string)
; 		    namestr))
; 	 (sub (when subscr? (subseq namestr 1))))
;     (values name1 sub
; 	    (when bang-pos (parse-integer (subseq string (1+ bang-pos))))
; 	    (when dollar-pos (parse-integer (subseq string (1+ dollar-pos)))))))
