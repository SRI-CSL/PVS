;;; -*- Mode: Lisp;  Package: sb-runtime; Log: sb-changes.log  -*-
;;; Sccs Id @(#)rt-unp-top.lisp	1.16 10/2/89
;;; ******************************************************************* ;;;
;;;          (c) Copyright 1989 by Carnegie Mellon University.          ;;;
;;;                        All rights reserved.                         ;;;
;;;         This code was written as part of the Ergo project.          ;;;
;;;  If you want to use this code or any Ergo software, please contact  ;;;
;;;			Frank Pfenning (fp@cs.cmu.edu)			;;;
;;; ******************************************************************* ;;;



;;; ERGO Project, Syntax Box.

;;; Scott Dietzen, Mon Sep 21 16:08:08 1987

(in-package "SB-RUNTIME")  (use-package :ergolisp)


(export '(*default-char-width*
	  is-nt-opsig?
	  unparse-term
	  term-to-uterm uterm-to-aw term-to-aw
	  read-parse-term
	  unparse-runtime-error
	  *unparse-style* *formatting-off* *parens-off*
	  ))

(defvar *default-char-width* 80
  "Number of characters (fix-width) that fit accross the screen. (This value
   may be superseded with the :char-width parameter to the unparsing routines.)")

(defvar *formatting-off* nil
  "Disables formatting line breaks.")

(defvar *parens-off* nil
  "Disables paren insertion.")

(defvar *unparse-style* nil
  "Global variable containing keyword symbol or list of keyword symbols ~
   describing the desired unparsing style.")


(defun is-nt-opsig? (nt-name term)
  "Determines whether NT-NAME(0th arg) is the sort of term TERM (1st arg)."
  (and (term:is-sim-term term)
       (let ((op-result
	      (sort:opsig-output
	       (sort:opsig-table-lookup (term:term-op term)))))
	 (and op-result
	      (sort:is-sort-ttype op-result)
	      (eq nt-name
		  (sort:ds-sort-ttype op-result))))))
		       


(defun term-to-uterm (term &key
			   (nt nil)
			   (lang nil)
			   (no-warn nil)
			   (style *unparse-style*)
			   &allow-other-keys)
  "Unparses a TERM (0th arg) into a UTERM (unparsed-term).
   The remaining arguments are keyword:
     :NT -- the nonterminal name of the term (optional, normally determined by
            sort of term.)
     :LANG -- the language name of the term (optional, normally determined by
            sort of term.)
     :NO-WARN -- Suppresses a warning if TERM is not unparseable.
     :STYLE -- Symbol or list of symbols to select appropriate unparsing
            patterns ( << >> ) for unparsing."
  (let* ((opsig-entry (sort:opsig-table-lookup (term:term-op term)))
	 (lang-name (cond (lang)
			  (t
			   (if opsig-entry
			       (sort:opsig-lang-name opsig-entry)))))
	 (lang-unparse-routine
	  (if lang-name
	      (lang:get-lang-unparse-routine-name lang-name)))
	 (sort (if opsig-entry
		   (sort:opsig-output opsig-entry)))
	 (sort-name (cond (nt)
			  (t
			   (if (is-sort-ttype sort)
			       (ds-sort-ttype sort))))))
    (cond ((and sort-name
		(fboundp lang-unparse-routine)
		)
					; @hack, unparse-gen depends on the
					; following format as well.
		;(fboundp (intern (concatenate 'string
	        ;                  (string-upcase
		;		    (get-lang-conc-name lang-name))
		;		   "-UNP-"
		;		   (string-upcase
		;		    (symbol-name sort-name)))
		;		 (find-package
                ;                 (lang:get-lang-code-package-name
		;		   lang-name)))
	   (funcall (lang:get-lang-unparse-routine-name lang-name)
		    sort-name
		    term
		    :style style))
	  (t
	   (cond (no-warn
		  nil)
		 (t
		  (warn "Term not unparseable. ~%~
                     Either sort ~S is not a nonterminal,~%~
                     sort ~S is not an unparseable nonterminal, ~%~
                     or The proper unparser is not loaded for ~S."
		     sort-name sort-name lang-name)
		  nil))))))





(defun uterm-to-aw (uterm &key
			  (char-width *default-char-width*)
			  (width nil)
			  (lang nil)
			  (no-warn nil)
			  (fontwidth sb-deffontwidth)
			  (fontheight sb-deffontheight)
			  &allow-other-keys)
  "Converts a UTERM (unparsed-term, 0th arg) into a AW (abstract window)
   given the width
   The remaining arguments are keyword:
     :NT -- the nonterminal name of the term (optional, normally determined by
            sort of term.)
     :CHAR-WIDTH -- width of result in characters, defaults to
            *default-char-width* .
     :WIDTH -- width of result in pixels, defaults to conversion of CHAR-WIDTH.
     :LANG -- the language name of the term (optional, normally determined by
            sort of term.)
     :NO-WARN -- Suppresses a warning if TERM is not unparseable."
  (let* ((term (uterm-term uterm))
	 (opsig-entry (sort:opsig-table-lookup
		       (term:term-op term)))
	 (lang-name (cond (lang)
			  (t
			   (if opsig-entry
			       (sort:opsig-lang-name opsig-entry)))))
	 (lang-win-unparse-routine
	  (if lang-name
	      (lang:get-lang-win-unparse-routine-name lang-name)))
	 (width (cond (*formatting-off* most-positive-fixnum)
		      (width)
		      (t (setq width
			       (let ((*sb-fontwidth* fontwidth))
				 (sb-chars-to-pixels char-width)))))))

    (cond ((fboundp lang-win-unparse-routine)
	   (funcall lang-win-unparse-routine
		    uterm
		    width
		    :fontwidth fontwidth
		    :fontheight fontheight))
	  (t
	   (if (not no-warn)
	       (warn "UTerm formatting routine not available for unparsing.
 		      (Unparser probably not loaded for ~S).~%~
                      Formatting without language knowledge.~%"
		     lang-name))
	   (let* ((*unparser-op-list* nil))
	     (format-uterm uterm width :fontwidth fontwidth
			               :fontheight fontheight))))))



(defun term-to-aw (term &key
			(nt nil)
			(lang nil)
			(no-warn nil)
			(char-width *default-char-width*)
			(width nil)
			(fontwidth sb-deffontwidth)
			(fontheight sb-deffontheight)
			(style *unparse-style*)
			&allow-other-keys)
  (let ((uterm (term-to-uterm term
			      :nt nt
			      :lang lang
			      :no-warn no-warn
			      :style style)))
    (if uterm 
	(uterm-to-aw uterm
		     :char-width char-width
		     :width width
		     :lang lang
		     :no-warn no-warn
		     :fontwidth fontwidth
		     :fontheight fontheight))))




(defun unparse-term (term &key
			  (stream *terminal-io* streamp)
			  (file nil filep)
			  (string nil stringp)
			  (nt nil)
			  (lang nil)
			  (no-form *formatting-off*)
			  (no-parens *parens-off*)
			  (char-width *default-char-width*)
			  (width nil)
			  (no-warn nil)
			  (style *unparse-style*)
			  (sb-tex nil)
			  &allow-other-keys)
  "Unparses a TERM (0th arg).  The remaining arguments are keyword:
     :STREAM -- output stream.
     :FILE -- output file.
     :STRING -- return result as a string.
     (Only one of the previous three should be specified.)
     :NT -- the nonterminal name of the term (optional, normally determined by
            sort of term.)
     :LANG -- the language name of the term (optional, normally determined by
            sort of term.)
     :CHAR-WIDTH -- width of result in characters, defaults to
            *default-char-width* .
     :WIDTH -- width of result in pixels, defaults to conversion of CHAR-WIDTH.
     :NO-WARN -- Suppresses a warning if TERM is not unparseable.
     :NO-FORM -- Disables formatting line breaks.
     :STYLE -- Symbol or list of symbols to select appropriate unparsing
            patterns ( << >> ) for unparsing.
     :SB-TEX -- Maps unparsing result into specialized TeX output.  Also turned
            on if STYLE includes :SB-TEX.  "
  (declare (ignore string streamp))
  (let* ((*parens-off* no-parens)
	 (*formatting-off* no-form)
	 (sb-tex (or sb-tex
		     (eq style :sb-tex)
		     (and (listp style)
			  (member :sb-tex style :test #'eq))))
	 (*print-escape* (if sb-tex nil *print-escape*))
	 (width (cond (no-form most-positive-fixnum)
		      (width)
		      (t (setq width 
			       (let ((*sb-fontwidth* sb-deffontwidth))
				 (sb-chars-to-pixels char-width))))))
					; do not want escapes in TeX.
         (uterm (term-to-uterm term :nt nt
			            :lang lang
				    :no-warn no-warn
				    :style style))
	 (aw (if uterm
		 (uterm-to-aw uterm :lang lang
			      :width width
			      :no-warn no-warn
			      :fontwidth sb-deffontwidth
			      :fontheight sb-deffontheight))))
    (cond (aw
	   (let* ((unp-str (if sb-tex
			       (map-aw-to-tex aw)
			       (aw-outstring aw)))
		  (*print-pretty* nil))	; otherwise things are messed up.
	     (cond (stringp
		    unp-str)
		   (filep
		    (with-open-file (s file
				       :direction :output
				       :if-exists :new-version)
		      (write-string unp-str s)
		      t))
		   (t
		    (write-string unp-str stream)
		    t)))))))





(defun term:sbrt-print-term-hook (term stream depth)
  (cond ((and (not term:*disable-auto-unparsing*)
	      (null *print-escape*)
	      (unparse-term term :stream stream :no-warn t))
	 nil)
	(t
	 (term:standard-print-term term stream depth))))

