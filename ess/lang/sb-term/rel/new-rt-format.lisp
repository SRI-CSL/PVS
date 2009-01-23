;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-format.lisp	1.41 11/15/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Wed Aug 26 17:16:29 1987

(in-package :sb-runtime)  (use-package :ergolisp)

(export '(
	  format-uterm 
	  *unparser-op-list* *spacing-info* *formatting-off*
	  *sb-format-warnings?* *zero-space-bp*
	  sb-deffontheight sb-deffontwidth ;; do no need to export.
	  oct-points
	 ))

(defvar *formatting-off* nil
  "Disables formatting line breaks.")

(defvar *unparser-op-list* nil
  "Bound by unparsers and used to recognize those keywords distinguished as
   operators.")

(defvar *spacing-info* nil
  "Bound by unparsers and used to override default between token spacing.")

(defparameter *zero-space-bp* 20
  "This factor inhibits breaks between tokens normally separted by zero space
  (e.g., operators like `(' or `,').  This is set to the number of nonterminal
  nesting levels that should be encountered before violating this parameter. 
  (As this has been a farily good heuristic, the initial value is high.)")



(defconstant sb-deffontheight 1)
(defconstant sb-deffontwidth 1)

(defvar *sb-fontwidth* sb-deffontwidth)
(defvar *sb-fontheight* sb-deffontheight)


(eval-when (compile eval load)

(defmacro sb-chars-to-pixels (chars)
  `(* (the fixnum *sb-fontwidth*) ,chars))
(defmacro sb-pixels-to-chars (pixels)
  `(floor (/ (the fixnum ,pixels) (the fixnum *sb-fontwidth*))))
(defmacro sb-lines-to-pixels (lines)
  `(* (the fixnum *sb-fontheight*) (the fixnum ,lines)))
(defmacro sb-pixels-to-lines (pixels)
  `(floor (/ (the fixnum ,pixels) (the fixnum *sb-fontheight*))))
(defmacro null-sb-chars-to-pixels (chars)
  `(let ((chars ,chars))
     (cond (chars
	    (the fixnum (sb-chars-to-pixels chars))))))

)  ;eval-when



;;; Twiddle these for better spacing
;;; (Pixel figures for fonts already allow for the surrounding space.  This
;;; would be a possilbe future inhancement.)

;; (defparameter *char-above* 0)

;; (defparameter *char-below* 0)

;; (defparameter *char-left* 2)

;; (defparameter *char-right* 0)




(defparameter indent-unit 1)
					; default number of chars to indent per
					; nesting level (may be revised if
 					; formatting demands).

(defparameter def-indent-unit-width
  (the fixnum (sb-chars-to-pixels (the fixnum indent-unit))))
(proclaim '(fixnum def-indent-unit-width))

(defvar *indent-unit-width* nil)
					; Changes if original nesting will not
					; work. 

(eval-when (compile eval load)
(defmacro indent-width (units)
  `(* *indent-unit-width* ,units)))


(defparameter indent-unit-token		; This avoids repeatedly consing this 
  (make-token :kind :whitespace		; token.
	      :subkind :indent
	      :value (the fixnum indent-unit)))


(defparameter *rept-indent-units* (* 1 (the fixnum indent-unit)))
(defparameter *uterm-indent-units* (the fixnum (* 2 (the fixnum indent-unit))))


(defvar *allowed-width* nil)



;;; Spacing Between Tokens

(defun look-up-default-spaces (token-class1 token-class2)
  (ecase token-class1
    (:generic
     (ecase token-class2
       (:generic 1)
       (:op 1)
       (:jux 0)))
    (:op
     (ecase token-class2
       (:generic 1)
       (:op 1)
       (:jux 0)))
    (:jux
     (ecase token-class2
       (:generic 1)
       (:op 1)
       (:jux 0)))))
		 


(defun get-token-space-kind (token)
  (case (token-kind token)
    (:keyword
     (cond ((memq (token-value token) *unparser-op-list*)
	    :op)
	   ((eq (token-subkind token) :jux)
	    :jux)
	   (t
	    :generic)))
    (t
     :generic)))



(defun base-default-spaces (token1 token2)
  (null-sb-chars-to-pixels
   (the fixnum
	(look-up-default-spaces (get-token-space-kind token1)
				(get-token-space-kind token2)))))



;;; Spacing Between Tokens  (continued)

;;; HACK to allow special spacing requirements to be set until this is 
;;;    added to the formatting language.

(defun special-default-spaces (token1 token2)
  (null-sb-chars-to-pixels
   (cond ((and (eq (token-kind token2) :keyword)
	       (memq (token-value token2) *unparser-op-list*)
	       (memq (token-value token2)
		     '(SBST::|,|
		       SBST::|;|
		       SBST::|:|
		       SBST::|.|
		       SBST::|)|    
		       SBST::|]|
		       SBST::|}| )))
	  0)
	 ((and (eq (token-kind token1) :keyword)
	       (memq (token-value token1) *unparser-op-list*)
	       (memq (token-value token1)
		     '(SBST::|(|
		       SBST::|[|
		       SBST::|{| )))
	  0)
	 ((and (not (eq (token-kind token1) :keyword))
	       (eq (token-kind token2) :keyword)
	       (memq (token-value token2) *unparser-op-list*)
	       (memq (token-value token2)
		     '(SBST::|(| 
		       SBST::|[| )))
	  0))))





;;; Spacing Between Tokens  (continued)


(defun lang-special-spaces (token1 token2 name1 name2)
  (let* ((op2? (memq (token-value token2) *unparser-op-list*))
	 (jux2? (eq (token-subkind token2) :jux))
	 (lt1? (if (not (eq (token-kind token1) :keyword))
		   name1))
	 (lt2? (if (not (eq (token-kind token2) :keyword))
		   name2))
	 (keyhash1 (gethash (cond (lt1?)
				  (t (token-value token1)))
			    *spacing-info*)))
    (null-sb-chars-to-pixels
     (cadr
      (cond (keyhash1
	     (cond ((assoc (token-value token2) keyhash1))
		   ((and op2?
			 (assoc :op keyhash1)))
		   ((and jux2?
			 (assoc :jux keyhash1)))
		   ((and lt2?
			 (assoc lt2? keyhash1)))
		   ((and lt2?
			 (assoc :lt keyhash1)))
		   ((assoc :arb keyhash1)))))))))




(defconstant nesting-constant 100
  "Increment given to break point values to insure no conflict accross
   levels.") 
(defconstant rept-nesting-constant 20
  "Increment given to break point values within repts (not quite a full
   nesting).") 
(defconstant starting-bp-value (* 20 nesting-constant)
  "Starting value for bp's. Arbitrary, but wanted non-negitive so that
   reductions in bp value (i.e., preferred breaks) do not lower it below zero
   and cause inadvertent breaks.")
(defconstant bp-rel-constant (/ nesting-constant 2)
  "Indicates preferred bp's.  Should be less than nesting-constant.")





(defun token-width (token)
  (declare (type token token))
  (ecase (token-kind token)
    (:keyword 
     (if (eq (token-subkind token) :jux)
	 0
	 (the fixnum
	      (sb-chars-to-pixels
	       (length (or (the simple-string (token-str-value token))
			   (symbol-name (token-value token))))))))
    (:lt (ecase (token-subkind token)
	   ((:id :identifier)
	    (the fixnum
		 (sb-chars-to-pixels
		  (length (or (the simple-string (token-str-value token))
			      (symbol-name (token-value token)))))))
	   (:string
	    (the fixnum
		 (sb-chars-to-pixels (length (the simple-string
						  (token-str-value token))))))
	   (:number 
	    (the fixnum
		 (sb-chars-to-pixels (length (the simple-string
						  (format nil "~A"
						    (token-value token)))))))))
    (:whitespace
     (case (token-subkind token)
       (:sp (token-value token))
       (t 0)))))



(defun son-list-width (son-list)
  (do ((sons son-list (cdr sons))
       (result 0 (+ result
		    (let ()
		      (assert (token-p (car sons)))
		      (the fixnum (token-width (car sons)))))))
      ((null sons)
       result)
    (declare (fixnum result))))




(defstruct tinfo			; for token-info
  (son nil)
  (bp nil)
  (bp-break? nil)	
  (index nil)
  (lson nil)				; also indents field (at diff. time)
  (rson nil)				; also unindents field (at diff. time)
  (parent nil)
  (uterm nil)
  (width nil)
  (indent-stack nil)
  )


(eval-when (compile eval load)
(defmacro tinfo-bp-value (tinfo)
  `(bp-value (tinfo-bp ,tinfo)))
(defmacro tinfo-bp-united-flag (tinfo)
  `(bp-united-flag (tinfo-bp ,tinfo)))
(defmacro tinfo-bp-crs (tinfo)
  `(bp-crs (tinfo-bp ,tinfo)))
(defmacro tinfo-bp-spaces (tinfo)
  `(bp-spaces (tinfo-bp ,tinfo)))
(defmacro tinfo-bp-format (tinfo)
  `(bp-format (tinfo-bp ,tinfo)))

(defmacro tinfo-indents (tinfo)
  `(tinfo-lson ,tinfo))
(defmacro tinfo-unindents (tinfo)
  `(tinfo-rson ,tinfo))
  )


(defvar *tinfo*
  (make-array 0
	      :element-type 'tinfo
	      :adjustable t
	      :fill-pointer t))

(proclaim '(vector *tinfo*))

(defun tinfo-bump (index)
  (declare (fixnum index))
  (let ((total (1+ index)))
    (do ()
	((> (fill-pointer *tinfo*)
	    total))
      (if (< (fill-pointer *tinfo*)
	     (array-total-size *tinfo*))
	  (incf (fill-pointer *tinfo*))
	  (vector-push-extend nil *tinfo* 100))
      ;; Below check is becase vector-push-extend does not necessarily make
      ;; them all nil, for now it is putting 0's in there. 
      (if (or (null  (aref *tinfo* (1- (fill-pointer *tinfo*))))
	      (not (tinfo-p (aref *tinfo* (1- (fill-pointer *tinfo*))))))
	  (setf (aref *tinfo* (1- (fill-pointer *tinfo*)))
		(make-tinfo :index (1- (fill-pointer *tinfo*))))))
    total))



(defun flush-tinfo ()			; Zero's fields rather than cons new.
  (do ((i 0 (1+ i)))
      ((or (= i (length (the vector *tinfo*)))
	   (null (tinfo-son (aref *tinfo* i)))))
    (declare (fixnum i))
    (let ((tinfo (aref *tinfo* i)))
      (setf (tinfo-son tinfo) nil)
      (setf (tinfo-bp tinfo) nil)       ; @@Consider reusing rather than consing.
      (setf (tinfo-bp-break? tinfo) nil)
      ;;(setf (tinfo-index tinfo) i)    ; not side effected.
      (setf (tinfo-indents tinfo) nil)
      ;;(setf (tinfo-lson tinfo) nil)   ; same field as indents
      (setf (tinfo-unindents tinfo) nil)
      ;;(setf (tinfo-rson tinfo) nil)   ; same field as unindents
      (setf (tinfo-parent tinfo) nil)
      (setf (tinfo-uterm tinfo) nil)
      (setf (tinfo-width tinfo) nil)
      (setf (tinfo-indent-stack tinfo) nil)
      ))
  (setf (fill-pointer *tinfo*) 0))




(defvar *i* 0
  "Global conter used to init. *tinfo*, easier than passing around.")

(proclaim '(fixnum *i*))

(defun init-tinfo (uterm)
  (let ((*i* -1))
    (init-tinfo-aux uterm
		    starting-bp-value)
    (if (and (>= *i* 0)
	     (null (tinfo-bp (aref *tinfo* *i*))))
					; explicit add an ending bp if
					; none 
	(setf (tinfo-bp (aref *tinfo* *i*))
	      (make-bp :value 0
		       :spaces 0
		       :crs 0)))))
  



(defun init-tinfo-aux (uterm nesting-level)
  (declare (fixnum nesting-level))
  (let ((hold-i *i*)
	(new-tab-count 0))
    (declare (fixnum new-tab-count hold-i))
    (do ((sons (uterm-sons uterm) (cdr sons))
	 (bps (uterm-bps uterm) (cdr bps)))
	((null sons))
      (let ((son (car sons))
	    (bp (if bps (copy-bp (car bps))))) ; copy because we side effect
					       ; bps!
	(mapc #'(lambda (ws)
		  (if (or (eq (token-subkind ws) :tab-right)
			  (eq (token-subkind ws) :tab-left))
		      (incf new-tab-count))
		  (if (eq (token-subkind ws) :untab)
		      (decf new-tab-count)))
	    (if bp
		(bp-format bp)))
	(cond ((uterm-p son)
	       (init-tinfo-aux son
			       (+ nesting-level (if (eq (uterm-kind son) :rept)
						    rept-nesting-constant
						    nesting-constant)))
	       (if (>= *i* 0)		; above could be empty rept., still
					; empty tinfo array.
		   (let ((curr-tinfo (aref *tinfo* *i*))) 
					; note: prob. hold-i < *i*
		     (if bp
			 (setf (tinfo-bp curr-tinfo)
			       (merge-bps (tinfo-bp curr-tinfo)	; side effected
					  (let ()
					    (setf (bp-value bp)
						  (+ nesting-level
						     (the fixnum (bp-value bp))))
					    bp)))))))
	      ((token-p son)
	       (setf *i* (the fixnum (tinfo-bump *i*)))
	       (let ((curr-tinfo (aref *tinfo* *i*)))
		 (setf (tinfo-son curr-tinfo) son)
		 (setf (tinfo-uterm curr-tinfo) uterm)
		 (if bp
		     (setf (tinfo-bp curr-tinfo)
			   (merge-bps (tinfo-bp curr-tinfo) ; side effected
				      (let ()
					(setf (bp-value bp)
					      (+ nesting-level
						 (the fixnum (bp-value bp))))
					bp)))))))))
       ;; The following does default indentation if there is none present.
       ;; whenever the uterm nesting level increases, so does the indentation.
    (cond ((and (< (the fixnum (1+ hold-i)) *i*)
		(not (eq (uterm-kind uterm) :rept)))
					; different indent for rept.
	   ;;(format t "(~A .. ~A)~%" (1+ hold-i) *i*)	   
	   (setf (tinfo-indents (aref *tinfo* (1+ hold-i)))
		 (cons `(,*i* ,*uterm-indent-units*)
		       (tinfo-indents
			(aref *tinfo* (1+ hold-i))))))
	  ((and (>= hold-i 0)
		(< hold-i *i*)
		(eq (uterm-kind uterm) :rept))
	   ;;(format t "(~A .. ~A  :rept)~%" hold-i *i*)	   
	   (setf (tinfo-indents (aref *tinfo* hold-i))
		 (cons `(,*i* ,*rept-indent-units*)
		       (tinfo-indents
			(aref *tinfo* hold-i))))))))
 

(defun merge-bps (bp new-bp)
  (cond ((and bp new-bp)
	 (setf (bp-value bp)
	       (cond ((and (bp-value bp) (bp-value new-bp))
		      (min (the fixnum (bp-value bp))
			   (the fixnum (bp-value new-bp))))
		     (t
		      (or (bp-value bp) (bp-value new-bp)))))
	 (setf (bp-united-flag bp)	; @@ consider merge!
	       (append (bp-united-flag bp)
		       (bp-united-flag new-bp)))
	 (setf (bp-crs bp)
	       (cond ((and (bp-crs bp)
			   (bp-crs new-bp))
		      (max (the fixnum (bp-crs bp))
			   (the fixnum (bp-crs new-bp))))
		     ((or  (bp-crs bp)
			   (bp-crs new-bp)))))
	 (setf (bp-spaces bp)
	       (cond ((and (bp-spaces bp)
			   (bp-spaces new-bp))
		      (max (the fixnum (bp-spaces bp))
			   (the fixnum (bp-spaces new-bp))))
		     ((or  (bp-spaces bp)
			   (bp-spaces new-bp)))))

	 (setf (bp-format bp)
	       (append (bp-format bp) (bp-format new-bp)))

         bp)
	((or bp new-bp))))





(defun determine-between-token-spaces (token1 token2 name1 name2)
  (cond ((lang-special-spaces token1 token2 name1 name2))
	((special-default-spaces token1 token2))
	(t
	 (base-default-spaces token1 token2))))







(defun replicate (number ptr)
  (do ((count number (1- count))
       (result nil (cons ptr result)))
      ((= count 0) result)
    (declare (fixnum count))))


(defun no-tabs-or-indents (formats)
  (null
   (do ((tokens formats (cdr tokens))
	(hit? nil
	      (and (eq (token-kind (car tokens)) :whitespace)
		   (memq (token-subkind (car tokens))
			 '(:indent :unindent :tab-left :tab-right :untab)))))
       ((or hit? (null tokens)) 
	hit?)
     (declare (boolean hit?)))))
   



(defun insert-default-ws (tinfo)
  (if (no-tabs-or-indents (tinfo-bp-format tinfo))
      (let* ((new-indents 
	      (do ((indents (tinfo-indents tinfo) (cdr indents))
		   (result 0
			   (let ((index (caar indents))
				 (units (cadar indents)))
			     (declare (fixnum units))
			     (cond ((no-tabs-or-indents
				     (tinfo-bp-format
				      (aref *tinfo* index)))
				    (setf (tinfo-unindents
					   (aref *tinfo* index))
					  (the fixnum
					       (+ units
						  (the fixnum
						       (cond ((tinfo-unindents
							       (aref *tinfo*
								     index)))
							     (t 0))))))
				    (+ result units))
				   (t
				    result)))))
		  ((null indents)
		   result)
		(declare (fixnum result))))
	     (new-unindents (cond ((tinfo-unindents tinfo))
				  (t 0)))
	     (indents-to-add (- (the fixnum new-indents)
				(the fixnum new-unindents))))
	(declare (fixnum indents-to-add))
	;;(format t "~A: ~A~%" 
	;;  (tinfo-index tinfo)
	;;  (let ((token (tinfo-son tinfo)))
	;;    (if (token-p token)
	;;        (token-value token))))
	(cond ((> indents-to-add 0)
	       ;;(format t "~A >> ~A ~%"
	       ;;  (tinfo-index tinfo) indents-to-add)
	       (setf (tinfo-bp-format tinfo)
		     (nconc (replicate indents-to-add
				       indent-unit-token)
			    (tinfo-bp-format tinfo))))
	      ((< indents-to-add 0)
	       ;;(format t "~A << ~A ~%"
	       ;;  (tinfo-index tinfo) (abs indents-to-add))
	       (setf (tinfo-bp-format tinfo)
		     (nconc (replicate (abs indents-to-add)
				       unindent-token)
			    (tinfo-bp-format tinfo)))))))
  (setf (tinfo-indents tinfo) nil)
  (setf (tinfo-unindents tinfo) nil))
			




(defun assign-between-token-whitespace ()
  (do ((i 0 (1+ i)))
      ((= i (1- (fill-pointer *tinfo*))))
    (declare (fixnum i))
    (let* ((tinfo (aref *tinfo* i))
	   (son (tinfo-son tinfo))
	   (bp (tinfo-bp tinfo))
	   (next-tinfo (aref *tinfo* (1+ i)))
	   (next-son (tinfo-son next-tinfo)))
      (assert (and (token-p son)
		   (token-p next-son)))
      (if (null (bp-spaces bp))
	  (let ((spaces (determine-between-token-spaces
			 son
			 next-son
			 (uterm-name (tinfo-uterm tinfo))
			 (uterm-name (tinfo-uterm next-tinfo)))))
	    (declare (fixnum spaces))
	    (cond ((= spaces 0)
		   (setf (bp-spaces bp) 0))
		  ((< spaces 0)		     ; represent cr's from
		   (setf (bp-spaces bp) 0)   ; lang-special-spaces
		   (setf (bp-crs bp) (abs spaces))
		   (setf (bp-value bp) most-negative-fixnum))
		  ((> spaces 0)
		   (setf (bp-spaces bp) spaces))))
	  (setf (bp-spaces bp)
		(the fixnum (sb-chars-to-pixels (the fixnum (bp-spaces bp))))))
					; Up till now, has been char spaces.

      (if (= 0 (the fixnum (bp-spaces bp)))
	  (setf (bp-value bp)
		(+ (bp-value bp)
		   (the fixnum
			(* (the fixnum *zero-space-bp*) nesting-constant)))))
					; above is experiment, don't want to
					; break where no white space is
					; required . 

      (if (null (bp-crs bp))
	  (setf (bp-crs bp) 1))
      (insert-default-ws tinfo)))

  (let* ((tinfo (aref *tinfo* (1- (fill-pointer *tinfo*))))
	 (bp (tinfo-bp tinfo)))
    (if (null (bp-spaces bp))
	(setf (bp-spaces bp) 0))
    (if (null (bp-crs bp))
	(setf (bp-crs bp) 0))
    (insert-default-ws tinfo)))




;;; Because of the constraints placed upon the constructed tree, setup-p-tree
;;; is a linear algorithm.  (Visits any tree node at most twice.) 

(defun setup-bp-tree ()
  (let ((root (aref *tinfo* 0))
	(curr-att (aref *tinfo* 0)))
    (set-width root)
    (do ((i 1 (1+ i)))
	((= i (fill-pointer *tinfo*)))	; last item may have bp.
      (declare (fixnum i))
      (let* ((new (aref *tinfo* i)))
	(set-width new)
	(cond ((>= (the fixnum (tinfo-bp-value new))
		   (the fixnum (tinfo-bp-value curr-att)))
	       (setf (tinfo-rson curr-att)
		     new)
	       (setf (tinfo-parent new)
		     curr-att)
	       (setf curr-att new))
	      (t
	       (do ((parent (tinfo-parent curr-att)
			    (tinfo-parent parent)))
		   ((or (null parent)
			(>= (the fixnum (tinfo-bp-value new))
			    (the fixnum
				 (tinfo-bp-value parent)))
					; optimize above walk over long
					; sequences.
		    (cond (parent
			   (setf (tinfo-parent (tinfo-rson parent))
				 new)
			   (setf (tinfo-lson new)
				 (tinfo-rson parent))
			   (setf (tinfo-rson parent)
				 new)
			   (setf (tinfo-parent new)
				 parent)
			   (setf curr-att new))
			  (t
			   (setf (tinfo-parent new)
				 nil)
			   (setf (tinfo-lson new)
				 root)
			   (setf (tinfo-parent root)
				 new)
			   (setf root
				 new)
			   (setf curr-att new))))))))))
    root))



(defun set-width (tinfo)
  (setf (tinfo-width tinfo)
	(+ (the fixnum
		(if (>= (the fixnum (tinfo-index tinfo)) 1)
		    (tinfo-width (aref *tinfo* (1- (the fixnum
							(tinfo-index tinfo)))))
		    0))
	   (the fixnum
		(cond ((tinfo-bp-spaces tinfo))
		      (t 0)))		; last bp has no spaces.
	   (the fixnum (token-width (tinfo-son tinfo))))))



(defun balance-bp-tree (root start end)
  (declare (fixnum start end))
  (if root
      (let ((new-root root)
	    (mid-point (the rational
			    (/ (the fixnum
				    (+ (the fixnum
					    (if (>= start 1)
						(the fixnum
						     (tinfo-width
						      (aref *tinfo*
							    (1- start))))
						0))
				       (the fixnum
					    (tinfo-width (aref *tinfo* end)))))
			       2))))

	(cond ((tinfo-rson root)
	       (cond ((= (the fixnum (tinfo-bp-value root))
			 (the fixnum (tinfo-bp-value (tinfo-rson root))))
		      (do ((i 1 (1+ i))
			   (rson (tinfo-rson root) (tinfo-rson rson))
			   (best-rson
			    root
			    (if (< (abs (- (the fixnum
						(tinfo-width rson))
					   (the rational mid-point)))
				   (abs (- (the fixnum
						(tinfo-width best-rson))
					   (the rational mid-point))))
				rson
				best-rson)))
			  ((or (null (tinfo-rson rson))
			       (< (the fixnum (tinfo-bp-value root))
				  (the fixnum (tinfo-bp-value rson))))
			   (cond ((and (>= i 2) ; nodes should be flattened
				       (not (eq root best-rson)))
				  (setq new-root best-rson)
				  (promote-mid-son root new-root))))
			(declare (fixnum i)))))))

	(balance-bp-tree (tinfo-lson new-root)
			 start (tinfo-index new-root))
	(balance-bp-tree (tinfo-rson new-root)
			 (1+ (the fixnum (tinfo-index new-root))) end)
	new-root)))






(defun promote-mid-son (old-root new-root)
  (let ((old-root-parent (tinfo-parent old-root))
	(new-root-old-parent (tinfo-parent new-root))
	(new-root-old-lson (tinfo-lson new-root)))

    (setf (tinfo-lson new-root)
	  old-root)
    (setf (tinfo-parent old-root)
	  new-root)

    (if old-root-parent
	(if (eq (tinfo-lson old-root-parent) old-root)
	    (setf (tinfo-lson old-root-parent)
		  new-root)
	    (setf (tinfo-rson old-root-parent)
		  new-root)))
    (setf (tinfo-parent new-root)
	  old-root-parent)

    (setf (tinfo-rson new-root-old-parent)
	  new-root-old-lson)
    (if new-root-old-lson
	(setf (tinfo-parent new-root-old-lson)
	      new-root-old-parent))

    new-root))




(defun should-break-intv? (start end)
  (declare (fixnum start end))
  (or (> (+ (the fixnum
		 (- (the fixnum (tinfo-width (aref *tinfo* end)))
		    (the fixnum
			 (if (>= start 1)
			     (the fixnum
				  (tinfo-width (aref *tinfo* (1- start))))
			     0))))
	    (the fixnum
		 (if (>= start 1)
		     (cond ((car (tinfo-indent-stack (aref *tinfo*
							   (1- start)))))
			   (t 0))
		     0)))			; indentation!
	 (the fixnum *allowed-width*))))


(defun break-bp-tree (root start end united-flags)
  (declare (fixnum start end))
  (cond ((and root
	      (< start end)
	      (or (< (the fixnum (tinfo-bp-value root)) 0)
		  (do ((flags (tinfo-bp-united-flag root) (cdr flags)))
		      ((or (null flags)
			   (member (car flags)
				   united-flags
				   :test #'eql))
		       (car flags)))
		  (should-break-intv? start end)))
	 (let* ((new-united-flags
		 (union (tinfo-bp-united-flag root)
			united-flags
			:test #'eql)))
	   (setf (tinfo-bp-break? root) t)
	   (break-bp-tree (tinfo-lson root)
			  start
			  (tinfo-index root)
			  new-united-flags)
	   (break-bp-tree (tinfo-rson root)
			  (1+ (the fixnum (tinfo-index root)))
			  end
			  new-united-flags)))
	(t
	 (set-tinfo-indent-stack start end))))



(defun set-tinfo-indent-stack (start end)
  (declare (fixnum start end))
  (let ((ns (if (>= start 1)
		(tinfo-indent-stack (aref *tinfo* (1- start)))
		'(0))))
    (do ((i start (1+ i)))
	((> i end))
      (declare (fixnum i))
      (let* ((tinfo (aref *tinfo* i))
	     (bp (tinfo-bp tinfo)))
	(do ((formats (bp-format bp) (cdr formats)))
	    ((null formats))
	  (ecase (token-kind (car formats))
	    (:whitespace
	     (ecase (token-subkind (car formats))
	       (:cr
		())
	       (:indent
		(push (+ (the fixnum (car ns))
			 (the fixnum
			      (indent-width (the fixnum
						 (token-value
						  (car formats))))))
		      ns))
	       ((:unindent :untab)
		(pop ns))
	       (:tab-left
		(push (+ (the fixnum (car ns))
			 (- (the fixnum (tinfo-width tinfo))
			    (the fixnum
				 (if (>= start 1)
				     (the fixnum
					  (tinfo-width (aref *tinfo*
							     (1- start))))
				     0))))
		      ns))
	       (:tab-right
		(push (+ (the fixnum (car ns))
			 (the fixnum (tinfo-bp-spaces tinfo))
			 (the fixnum
			      (- (the fixnum (tinfo-width tinfo))
				 (the fixnum
				      (if (>= start 1)
					  (the fixnum
					       (tinfo-width (aref *tinfo*
								  (1- start))))
					  0)))))
		      ns))))))))

    (setf (tinfo-indent-stack (aref *tinfo* end))
	  ns)))








(defun queue-aw-son (aw son)
  (cond ((null (aw-sons-form aw))
	 (setf (aw-sons-form-tl aw)
	       (setf (aw-sons-form aw)
		     (list son))))
	(t
	 (setf (cdr (aw-sons-form-tl aw))
	       (list son))
	 (setf (aw-sons-form-tl aw)
	       (cdr (aw-sons-form-tl aw))))))



(defun build-aw-structure-&-insert-whitespace (uterm aw)
  (let ((*i* -1))
    (build-aw-structure-aux uterm aw 0)
    (if (>= *i* 0)
	(let ((tinfo (aref *tinfo* *i*)))
	  (cond ((tinfo-bp tinfo)
		 (output-bp-tokens (tinfo-bp tinfo)
				   (tinfo-bp-break? tinfo)
				   aw
				   t)))))))


(defun build-aw-structure-aux (uterm aw depth)
  (declare (fixnum depth))
  (do ((sons (uterm-sons uterm) (cdr sons))
       (count 0 (1+ count)))
      ((null sons)
       aw)
    (declare (fixnum count))
    (cond ((token-p (car sons))
	   (incf *i*)
	   (let* ((tinfo (aref *tinfo* *i*))
		  (son (tinfo-son tinfo))
		  (bp (tinfo-bp tinfo))
		  (bp-break? (tinfo-bp-break? tinfo)))
	     ;;(assert (eq son (car sons)))
	     ;; No longer holds due to ellipsis. 
	     (queue-aw-son aw son)
	     (cond ((and bp
			 (cdr sons))	; otherwise put formatting in above aw. 
		    (output-bp-tokens bp bp-break? aw nil)))))
	  ((uterm-p (car sons))
	   (let ((new-aw (make-aw :uterm (car sons)
				  :indent-unit-width
				  (aw-indent-unit-width aw))))
	     (build-aw-structure-aux (car sons) new-aw (1+ depth))
	     (queue-aw-son aw new-aw)
	     (if (>= *i* 0)
		 (let* ((tinfo (aref *tinfo* *i*))
			(bp (tinfo-bp tinfo))
			(bp-break? (tinfo-bp-break? tinfo)))
		   (if (and bp
			    (uterm-sons (car sons)) ; empty rept.
			    (cdr sons))	; otherwise put formatting in
					; above aw.  
		       (output-bp-tokens bp bp-break? aw nil)))))))))



(defun output-bp-tokens (bp bp-break? aw last-token?)
  (mapc #'(lambda (ws)
	    (assert (eq (token-kind ws) :whitespace))
	    (if (not (eq (token-subkind ws) :tab-right))
		(queue-aw-son aw ws)))
	(bp-format bp))
  (cond (bp-break? 
	 (do ((crs (bp-crs bp) (1- crs)))
	     ((= 0 crs))
	   (declare (fixnum crs))
	   (queue-aw-son aw cr-token)))
	((not last-token?)
	 (assert (bp-spaces bp))
	 (queue-aw-son aw
		       (make-token :kind :whitespace
				   :subkind :sp
				   :value (bp-spaces bp)))))
  (mapc #'(lambda (ws)
	    ;;(assert (eq (token-kind ws) :whitespace))   ; covered by above.
	    (if (eq (token-subkind ws) :tab-right)
		(queue-aw-son aw ws)))
	(bp-format bp)))





(defun format-aw (uterm aw width)
  (let ((*allowed-width* width))
    (flush-tinfo)
    
    (init-tinfo uterm)

    (assign-between-token-whitespace)

    (if (not *formatting-off*)
	(let* ((root1 (setup-bp-tree))
	       (root (balance-bp-tree root1 0 (1- (fill-pointer *tinfo*)))))
	  (break-bp-tree root 0 (1- (fill-pointer *tinfo*)) nil)))

    (build-aw-structure-&-insert-whitespace uterm aw)

    (compute-octs-and-string aw)

    (define-children-siblings-for-win aw)

    aw))



(defvar *sb-format-warnings?* nil)


(defun format-uterm (uterm width
			   &key
			   (top-width width)
			   (bottom-width width)
			   (indent-unit-width def-indent-unit-width)
			   (do-warning t)
			   (fontwidth sb-deffontwidth)
			   (fontheight sb-deffontheight))
  (declare (fixnum width) (ignore top-width bottom-width))
  (let* ((*sb-fontwidth* fontwidth)
	 (*sb-fontheight* fontheight)
	 (*indent-unit-width* indent-unit-width)
	 (aw (memo-aw uterm
		      width
		      *indent-unit-width*
		      fontwidth
		      fontheight)))
    (let ((aw
	   (cond ((and (> (the fixnum (aw-maxwidth aw)) width)
		       (= (the fixnum (aw-indent-unit-width aw)) 0))
		  (if *sb-format-warnings?*
		      (warn "Can not format abstract syntax within given ~%~
			 width even with identation for nesting removed. ~%~
		         Char-width: ~S~%"
			(sb-pixels-to-chars width)))
		  aw)
		 ((and (> (the fixnum (aw-maxwidth aw)) width)
		       (> (the fixnum (aw-indent-unit-width aw)) 0))
		  (if (and do-warning
			   *sb-format-warnings?*)
		      (warn "Can not format abstract syntax within given ~
                             width. ~%~
                             Attempting to decrease nesting indentation. ~%~
		             Char-width: ~S~%"
			    (sb-pixels-to-chars width)))
		  (format-uterm uterm width :do-warning nil
				:indent-unit-width
				(floor (/ *indent-unit-width* 2))
				:fontwidth fontwidth
				:fontheight fontheight))
		 (t
		  aw))))

      aw)))




(defun define-children-siblings-for-win (aw &optional (parent-aw nil))
  (let ((children (mapcan #'(lambda (son)
			      (if (aw-p son)
				  (list son)))
			  (aw-sons-form aw))))
    (mapc #'(lambda (child)
	      (define-children-siblings-for-win child aw))
	  children)
    (setf (aw-children aw) children)
    (setf (aw-parent aw) parent-aw)))



(defvar *indent* nil
  "Indentation stack local to compute-octs-and-string.")

(defvar *s* nil
  "Outstring for accumulated tokens")



(defun compute-octs-and-string (aw)
  (let ((*print-pretty* nil)		; To avoid weirdness.
	(*indent* '(0))
	(*s* (make-string-output-stream)))
    (compute-octs-and-string-aux aw 0 0 0 nil nil)
    (setf (aw-outstring aw) (get-output-stream-string *s*))))


  
(defun compute-octs-and-string-aux (aw leftx topy topdent
				    more-on-topline? more-on-botline?)
  (declare (fixnum topdent leftx))
  (assert (>= topdent leftx))
  (let ((x topdent)  
	(y topy)
	;;(s (make-string-output-stream))
	(minx topdent)
	(maxx 0)
	(height 1))
    (declare (fixnum x y minx maxx height))
    (do ((sons (aw-sons-form aw) (cdr sons))
	 (rev-sons nil (cons (car sons) rev-sons)))
	((null sons) aw)
      (if (token-p (car sons))
	  (let ((token (car sons)))
	    (assert (token-p token))
	    ;;(format t " ~A ~A ~A ~%" (token-kind token)
	    ;;	      (token-subkind token) *indent*)
	    (ecase (token-kind token)
	      ((:keyword :lt)
	       (setf x (+ x (the fixnum (token-width token))))
	       (ecase (token-subkind token)
		 ((:id :identifier nil)	; nil for keyword
		  (let* ((str (if (token-str-value token)
				  (token-str-value token)
				  (symbol-name (token-value token)))))
		    (princ (if (and (not *case-sensitive*)
				    (eq :downcase *print-case*))
			       (string-downcase str)
			       str)
			   *s*)))
		 ((:string :number)
		  (princ (if (token-str-value token)
			     (token-str-value token)
			     (token-value token))
			 *s*))
		 (:jux ())))

	      (:whitespace
	       (ecase (token-subkind token)
		 (:sp (let* ((spaces (the fixnum
					  (sb-pixels-to-chars
					   (token-value token))))
			     (space (the fixnum
					 (sb-chars-to-pixels spaces)))) ; exact
			(setf x (+ x space))
			(do ((i spaces (1- i)))
			    ((<= i 0))
			  (declare (fixnum i))
			  (write-char #\space *s*))))
		 (:cr (let ((act-indent (the fixnum
					     (sb-chars-to-pixels
					      (the fixnum
						   (sb-pixels-to-chars
						    (car *indent*)))))))
			(setf maxx (max maxx x))
			(setf y (+ y (sb-lines-to-pixels 1)))
			(setf x act-indent)
			(setf minx (min minx x))
			(incf height)
			(terpri *s*)
			(do ((i (sb-pixels-to-chars act-indent) (1- i)))
			    ((<= i 0))
			  (declare (fixnum i))
			  (write-char #\space *s*))))
		 (:indent (push (+ (the fixnum (car *indent*))
				   (the fixnum (indent-width
						(the fixnum
						     (token-value token)))))
				*indent*))
		 (:unindent (if (cdr *indent*)
				(pop *indent*)))
		 (:tab-left (push x *indent*))
		 (:tab-right (push x *indent*))	; already ordered properly
					; with whitespace
		 (:untab (if (cdr *indent*)
			     (pop *indent*)))))))
			
	  ;; continued
	  
	  ;; continued
	    
	  ;;  (aw-p (car sons))
	  (let ((aw (car sons))
		(new-leftx (car *indent*)))
	    (compute-octs-and-string-aux aw (min x new-leftx) y x
					 (is-more-on-line? more-on-topline?
							   rev-sons)
					 (is-more-on-line? more-on-botline?
							   (cdr sons)))
	    (setf maxx (max maxx
			    (rightx (aw-oct aw))))
	    ;;(write-string (aw-outstring aw) s)  ; for old 1 string per aw
	    (setf y (- (the fixnum (boty (aw-oct aw)))
		       (the fixnum (sb-lines-to-pixels 1))))
	    (setf x (botdent (aw-oct aw))))))


    (setf maxx (max maxx x))
    ;;(setf (aw-outstring aw) (get-output-stream-string s))

    (setf (aw-oct aw)
	  (make-oct :leftx 	 minx
		    :topy 	 topy
		    :rightx	 maxx
		    :boty	 (+ y (sb-lines-to-pixels 1))
		    :topdent     (if (not more-on-topline?) minx topdent)
		    :botdent     (if (not more-on-botline?) maxx x)
		    ))))

;;		  (if (and (< (car *indent*) aw-indent)
;;			   nil)		; @@@@ Temp. HACK!!!
;;		      (warn "Attempting to write a token with less ~%~
;;          		 indentation than the level with which the abstract ~%~
;; 			 window was started.  You probably attempt to pop ~%~
;;		         too many levels of tabs or indents in your format ~%~
;;			 specification for ~A~%~%" (aw-name aw)))




(defun is-more-on-line? (more-on-line? remaining-sons)
  (do ((sons remaining-sons (cdr sons)))
      ((or (null sons)
	   (aw-p (car sons))
	   (not (eq (token-kind (car sons)) :whitespace))
	   (eq (token-subkind (car sons)) :cr))
       (and (or sons
		more-on-line?)
	    (not (and sons
		      (token-p (car sons))
		      (eq (token-subkind (car sons)) :cr)))))))




(defun oct-points (oct y-offset height)
  (declare (fixnum y-offset height))
  ; points suitable for outlining a term in the octoid, y-offset is
  ; added to y-coordinates.
    (let ((x1 (- (the fixnum (leftx oct)) 1))
	  (x2 (- (the fixnum (topdent oct)) 1))
	  (x3 (rightx oct))
	  (x4 (botdent oct))
	  (y1 (+ (the fixnum (topy oct)) height y-offset))
	  (y2 (+ (the fixnum (topy oct)) y-offset))
	  (y3 (+ (- (the fixnum (boty oct)) height) y-offset))
	  (y4 (+ (the fixnum (boty oct)) y-offset)))
      (list (list x1 y1)
	    (list x2 y1)
	    (list x2 y2)
	    (list x3 y2)
	    (list x3 y3)
	    (list x4 y3)
	    (list x4 y4)
	    (list x1 y4)
	    (list x1 y1))))

