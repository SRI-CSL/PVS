;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-unp-structs.lisp	1.11 11/3/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;


;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Wed Aug 26 17:16:29 1987

(in-package 'sb-runtime)  (use-package :ergolisp)

(export '(token-p make-token token-kind token-subkind 
	  token-value token-str-value 
	  bp-p make-bp bp-united-flag bp-value
	  bp-format bp-crs bp-spaces 
	  aw-parent aw-oct aw-children aw-outstring
	  aw-term aw-maxwidth 
	  leftx rightx topy boty topdent botdent topliney botliney
	  width height 

	  print-token print-bp print-oct print-uterm print-aw
	 ))


(defconstant mult-line-threshold 60)

(defvar *sbrt-debug* nil
  "Causes more detailed printing of some unparsing structures.")





(defstruct (token (:print-function print-token))
  ;; kind is one of {:keyword, :lt, :whitespace}
  (kind ())			
  (subkind ())
  ;; subkinds for kind :lt are {:id, :number, :string} (others are made
  ;;    strings). 
  ;; subkinds for kind :whitespace are {:cr -- cariage-return, 
  ;;  				       :sp -- spaces, 
  ;;				       : (others inserted by formatter).
  ;;					
  (value ())
  (str-value ())			; includes escape chars etc. 
  )


(defun print-token (self stream depth)
  (declare (ignore depth))
  (if *sbrt-debug*
      (print-struct self stream token :kind :subkind :value :str-value)
      (format stream "#<unp-token>")))




(defstruct (bp (:print-function print-bp))
  ;; breakpoint representation
  (value ())
  (united-flag ())
  (crs nil)				; number of cr's if broken.
  (spaces nil)				; number of space's if unbroken (pixels)
  (format nil)				; list of tokens controlling whitespace
  )


(defun print-bp (self stream depth)
  (declare (ignore depth))
  (if *sbrt-debug*
      (print-struct self stream bp :value :united-flag :crs :spaces :format)
      (format stream "#<bp>")))




(defstruct (oct (:conc-name nil)
		(:print-function print-oct))
  (leftx 0)				; all dimensions in pixels
  (topy 0)
  (rightx 0)
  (boty 0)
  (topdent 0)
  (botdent 0)
  ;;(topliney 0)
  ;;(botliney 0)
  )


(defun print-oct (self stream depth)
  (declare (ignore depth))
  (if *sbrt-debug*
      (format stream "#<oct :leftx ~A :topy ~A :rightx ~A ~
                            :boty ~A :topdent ~A :botdent ~A>"
	(leftx self)
	(topy self)
	(rightx self)
	(boty self)
	(topdent self)
	(botdent self))
      (format stream "#<oct>")))


(eval-when (compile load eval)

(defmacro width (oct)
  `(- (rightx ,oct) (leftx ,oct)))

(defmacro height (oct)
  `(- (boty ,oct) (topy ,oct)))

)



(defstruct (uterm (:print-function print-uterm))
  (name nil :type symbol)
  (kind nil)				; :rept only (for now)
  (term nil)				; term for which generated 
  (sons nil)				; contains tokens, nested aws;
					; before bracketing, whitespace
  (sons-tl nil)				; so things may be easily added.
  (bps nil)				; break-points, same length as sons.
  (bps-tl nil)				; so things may be added quickly
  (lbp nil)				; for bracketing (min of aw)
  (rbp nil)				; for bracketing (min of aw)
  )


(defun print-uterm (self stream depth)
  (declare (ignore depth))
  (let ((*print-circle* nil)
	(term-str (format nil "~A" (uterm-term self))))
    (if (< (length term-str) mult-line-threshold)
	(format stream "#<uterm :term ~A>" term-str)
	(format stream "#<uterm :term ~%~A~%" term-str))))

					;(format stream "#<uterm>")



(defstruct (aw (:print-function print-aw))
  (uterm nil)				; uterm associated with aw.
  (sons nil)				; parallel to uterm-sons but aws.
  (sons-form nil)			; contains tokens, nested aws
					; after formatting (extra whitespace)
  (sons-form-tl nil)			; same as above
  (indent-unit-width nil)		; width of indent unit.
  (outstring nil)			; output string.
  (oct nil)
  (parent nil)				; aw for IF, nil for root.
  (children nil)			; subset of sons that are aw's
 )


(defun print-aw (aw stream depth)
  (declare (ignore depth))
  (if (aw-outstring aw)
      (let ((*print-circle* nil))
	(if (< (length (aw-outstring aw)) mult-line-threshold)
	    (format stream "#<aw :string ~A>" (aw-outstring aw))
	    (format stream "#<aw :string ~%~A~%" (aw-outstring aw))))
      (let ((*print-circle* nil)
	    (term-str (format nil "~A" (aw-term aw))))
	(if (< (length term-str) mult-line-threshold)
	    (format stream "#<aw :term ~A>" term-str)
	    (format stream "#<aw :term ~%~A~%" term-str)))))

					;(format stream "#<aw>")


(eval-when (compile eval load)

(defmacro aw-term (aw)
  `(uterm-term (aw-uterm ,aw)))
(defmacro aw-maxwidth (aw)		; see below.
  `(let ((oct (aw-oct ,aw)))
     (width oct)))
(defmacro aw-bps (aw)
  `(uterm-bps (aw-uterm ,aw)))
(defmacro aw-name (aw)
  `(uterm-name (aw-uterm ,aw)))
(defmacro aw-kind (aw)
  `(uterm-kind (aw-uterm ,aw)))
)
   ;; eval-when 


