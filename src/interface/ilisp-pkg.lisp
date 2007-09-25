;;; -*- Mode: Lisp -*-

;;; ilisp-pkg.lisp --
;;; ANSI CL DEFPACKAGE definition for ILISP.
;;;
;;; Common Lisp initializations
;;;
;;; Author: Marco Antoniotti, marcoxa@cs.nyu.edu
;;;
;;; This file is part of ILISP.
;;; Please refer to the file COPYING for copyrights and licensing
;;; information.
;;; Please refer to the file ACKNOWLEGDEMENTS for an (incomplete) list
;;; of present and past contributors.
;;;
;;; $Id: ilisp-pkg.lisp,v 1.6 2002/03/27 20:48:11 anisotropy9 Exp $

;;;----------------------------------------------------------------------------
;;; Prologue

#+(or allegro-v4.0 allegro-v4.1)
(eval-when (compile load eval)
  (setq excl:*cltl1-in-package-compatibility-p* t))


;;;----------------------------------------------------------------------------
;;; Definitions

;;; ILISP package --

;;;
;;; GCL 2.2 doesn't have defpackage (yet) so we need to put the export
;;; here. (toy@rtp.ericsson.se)
;;;
;;; Please note that while the comment and the fix posted by Richard
;;; Toy are correct, they are deprecated by at least one of the ILISP
;;; maintainers. :) By removing the 'nil' in the following #+, you
;;; will fix the problem but will not do a good service to the CL
;;; community.  The right thing to do is to install DEFPACKAGE in your
;;; GCL and to write the GCL maintainers and to ask them to
;;; incorporate DEFPACKAGE in their standard builds.
;;; Marco Antoniotti <marcoxa@icsi.berkeley.edu> 19960715
;;;
;;; "The use of keyword and uninterned symbol names in the package
;;; definition is a result of internecine wars during the ANSI
;;; definition process. The solution to make CL case insensitive and
;;; have the reader use uppercase appears, with the power of
;;; hindsight, short-sighted. However, the backwardly incompatible
;;; solution provided by Franz Inc seems a sub-optimal fix."
;;; 27 March 2002 Will Deakin

#-(and nil gcl)
(defpackage :ilisp (:nicknames :ILISP) (:use :common-lisp #+:CMU :conditions)
  ;; The following symbols should properly 'shadow' the inherited
  ;; ones.
  (:export #:ilisp-errors
           #:ilisp-save
           #:ilisp-restore
           #:ilisp-symbol-name
           #:ilisp-find-symbol
           #:ilisp-find-package
           #:ilisp-eval
           #:ilisp-compile
           #:ilisp-describe
           #:ilisp-inspect
           #:ilisp-arglist
           #:ilisp-documentation
           #:ilisp-macroexpand
           #:ilisp-macroexpand-1
           #:ilisp-trace
           #:ilisp-untrace
           #:ilisp-compile-file-extension
           #:ilisp-compile-file
           #:ilisp-casify
           #:ilisp-matching-symbols
           #:ilisp-callers
           #:ilisp-source-files
           #:ilisp-print-info-message
           #+:SBCL #:sbcl-trace
           #+:CMU #:cmulisp-trace
           #+(or :SBCL :CMU) #:source-file
	   )
  )
;;; ILISP --

;;; end of file -- ilisp-pkg.lisp --
