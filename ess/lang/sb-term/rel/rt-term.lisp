;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-term.lisp	1.6 9/21/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.


;;; Macro definitions for SB abstract syntax (SB generated parser
;;;   output).   
;;; Scott Dietzen, Fri Oct 10 14:16:59 1986



(in-package "SB-RUNTIME" :nicknames '("RT-SB" "RTSB" "SB-RT" "SBRT"))
(use-package :ergolisp)

(use-package '("TERM" "OCC" "OPER")) 




;;; The ":use '()" below is important because different common lisps will 
;;; export different symbols from the LISP package, so it is important that
;;; the SBST package does not use the LISP package, so we can get repeatable
;;; behavior on different machines.

(eval-when (compile load eval)
  (defvar *sbst-package* 
    (cond ((find-package "SBST"))
	  (t
	   (make-package "SBST"
			 :nicknames '("SB-ST" "SB-SYMBOL-TABLE") :use '())))))


(defvar *no-sb-system-graphics* nil)
;;; See rt-format.lisp for explanation. 



(export '(*sbst-package* 
          apply-lexical-terminal-constructor
          apply-lexical-terminal-discriminator
          apply-lexical-terminal-destructor))






(defun apply-lexical-terminal-constructor (type token)
  (ecase type
    (sbst::!id! (mk-id token))
    (sbst::!string! (mk-string token))
    (sbst::!number! (mk-number token))
    (sbst::!literal! (mk-literal token))     
    (sbst::!keyword! (mk-keyword token))))   

(defun apply-lexical-terminal-discriminator (type arg)
  (ecase type
    (sbst::id (is-id arg))
    (sbst::string (is-string arg))
    (sbst::number (is-number arg))
    (sbst::literal (is-literal arg))
    (sbst::keyword (is-keyword arg))))

(defun apply-lexical-terminal-destructor (type arg)
  (ecase type
    (sbst::id (ds-id arg))
    (sbst::string (ds-string arg))
    (sbst::number (ds-number arg))
    (sbst::literal (ds-literal arg))
    (sbst::keyword (ds-keyword arg))))





