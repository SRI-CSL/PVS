;;;-*- Mode: lisp; Package: retry -*-
;;;
;;; Author: fp
;;; Macros for establishing tags to retry certain operations. 
;;;
;;; Sccs Id @(#)retry.lisp	1.6 9/26/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;

#-gcl
(defpackage "RETRY")
(in-package "RETRY") (use-package :ergolisp)

;;; The next three forms should be replaced by eexport, once this is moved
;;; to the ergolisp box.

;;; First a lucid 2.1 bug workaround.
#+(or (and lucid (not lcl3.0)) :harlequin-common-lisp)
(eval-when (load compile eval)
  (dolist (s '("*CATCHERS*" "RETRY" "RETRY-CATCH" "REINIT-RETRY-CATCH"))
    (export (list (intern s :lisp)) :lisp)))

(export '(lisp::*catchers* lisp::retry
			   lisp::retry-catch lisp::reinit-retry-catch)
	:lisp)
(export '(*catchers* retry retry-catch reinit-retry-catch))

;; #-harlequin-common-lisp ;; apparent bug in re-defvaring *catchers*
(defvar *catchers* nil
  "List of retry-catchers in the current environment.")

;; #+harlequin-common-lisp ;; work around for the above
;; (setq *catchers* nil)

(defmacro retry-catch (label &rest forms)
  `(reinit-retry-catch ,label nil . ,forms))

;;; This is like retry-catch except that retryform is executed if
;;; the user decides to retry with this tag.  The intent here is
;;; that retryform will prompt for some better input than the input
;;; that didn't work the first time.
;;; The block and tagbody here are meant to get us an effect similar
;;; to that of the loop macro, but with a block name other than nil.
;;; This way, if one of the forms in the body calls return, we can
;;; return to the right place. 
(defmacro reinit-retry-catch (label retryform &body forms &aux
				    (blockname (gensym))
				    (topname (gensym)))
  `(block ,blockname
     (tagbody
      ,topname
      (catch ,label
	(return-from ,blockname
	  (let ((*catchers* (cons ,label *catchers*)))
	    . ,forms)))
      ;; next line prevents compiler warnings about Label NIL not referenced. 
      ,@(if (null retryform) '() `(,retryform))
      (go ,topname))))

(defun retry (&optional (label nil))
  (when (not label)
    (format *query-io* "Choose one of ~S-> " *catchers*)
    (setq label (read *query-io*)))
  (if (eq label :ignore)
      :ignore
      (throw label nil)))

