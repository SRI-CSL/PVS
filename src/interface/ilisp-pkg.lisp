;;; -*- Mode: Lisp -*-

;;; ilisp-pkg.lisp --

;;; This file is part of ILISP.
;;; Version: 5.8
;;;
;;; Copyright (C) 1990, 1991, 1992, 1993 Chris McConnell
;;;               1993, 1994 Ivan Vasquez
;;;               1994, 1995, 1996 Marco Antoniotti and Rick Busdiecker
;;;               1996 Marco Antoniotti and Rick Campbell
;;;
;;; Other authors' names for which this Copyright notice also holds
;;; may appear later in this file.
;;;
;;; Send mail to 'ilisp-request@naggum.no' to be included in the
;;; ILISP mailing list. 'ilisp@naggum.no' is the general ILISP
;;; mailing list were bugs and improvements are discussed.
;;;
;;; ILISP is freely redistributable under the terms found in the file
;;; COPYING.

(in-package "PVS")

;;; CLtL2 defpackage definition for ILISP.
;;;
;;; Common Lisp initializations
;;;
;;; Author: Marco Antoniotti, marcoxa@cs.nyu.edu

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

#-(and nil gcl)
(defpackage :ilisp (:use :lisp #+:CMU "CONDITIONS")
  ;; The following symbols should properly 'shadow' the inherited
  ;; ones.
  (:export :ilisp-errors
	   :ilisp-save
	   :ilisp-restore
	   :ilisp-symbol-name
	   :ilisp-find-symbol
	   :ilisp-find-package
	   :ilisp-eval
	   :ilisp-compile
	   :ilisp-describe
	   :ilisp-inspect
	   :ilisp-arglist
	   :ilisp-documentation
	   :ilisp-macroexpand
	   :ilisp-macroexpand-1
	   :ilisp-trace
	   :ilisp-untrace
	   :ilisp-compile-file
	   :ilisp-casify
	   :ilisp-matching-symbols
	   :ilisp-callers
	   :ilisp-source-files)
  )
;;; ILISP --

;;; end of file -- ilisp-pkg.lisp --
