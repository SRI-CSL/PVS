;;; -*- Mode: LISP; Syntax: Common-lisp; Package: VERSE -*-

; global variables and declarations for the prover subsystem
;
; -- Allen Van Gelder, Sep 4, 1984.
;
; 10-Oct-85  FvH - macros have been moved to file prmacros
;
; ------------------------------------------------------------------------
; global variables/constants with initializations
;
; CONVENTION:
; Use "defconst" for something that should "never" change.
; Use "defvar" with an initialization (2nd) argument for something that
;     does not change during the course of execution, but may evolve over
;     time.  Initialization to nil may be a placeholder to allow the
;     third (documentation) argument to be specified.
; Use "defvar" with an initialization (2nd) argument for symbols that may
;     be changed by the user to control execution, but are not changed
;     by normal program execution.  Flags, switches, etc. are in this
;     category.
; Use "setq" in an initialization function to initialize things that
;     are expected to vary during execution.  This allows the execution to
;     be completely re-initialized, which is desirable in itself for
;     debugging, and also serves as valuable documentation.

(defconstant  true 'true  )
(defconstant  false 'false )

(defconstant  primtypealist
    '((true .  bool)
      (false . bool)
      (and . bool)
      (or . bool)
      (not . bool)        ;; 6-May-88 FvH  added - was it ever there before ??
      (implies . bool)
      (iff . bool)
      (equal . bool)
      (lessp . bool)
      (lesseqp . bool)
      (greaterp . bool)
      (greatereqp . bool)
      (QUO . number)
      (PLUS . number)
      (MINUS . number)
      (DIFFERENCE . number)
      (TIMES . number)
      (DIVIDE . number)
      (tupcons . tuple)
      (update . array)
      (arrayrest . array)
      (TUPSEL-integer . integer)
      (TUPSEL-number . number)
      (TUPSEL-bool . bool)
     )) ;NSH from prglobals.lisp

(defvar interpsyms
     '(PLUS MINUS DIFFERENCE TIMES DIVIDE
       lessp lesseqp greaterp greatereqp
       tupcons TUPSEL
       update arrayrest arraysel
       if*
      )); NSH from prglobals.lisp

(defvar  pr-infile "prx-.m"    " input file - a dummy used to fool maclisp " )
(defvar  infilealt  "pr-.m"    " input file name, alternate. still needed? ")
(defvar  pr-outfile "pr-.m"    " output file - r, w, thawed used for all i/o")
(defvar  pr-errorfile "err-.m" " error file - dummy later set to sys errfile")

(defvar *info-file*      "info-.m")

(defvar  pr-infofile *info-file*
                " info file - dummy later set to sys infofile")

(defvar  justsko nil)  ; t iff skolemization only, no proof
(defvar  pr-pdafile "pr-.d"    " file used by pda for modified prover input")
(defvar  debugfilename "pr-.d"  " debug file name (see pda) ")
(defvar  implication nil             " bind for error return ")
(defvar  *write-implication-flag* nil "controls printing of implication file")
(defvar  impfilename "implication"   " implication file name ")
(defvar  verboseflg nil              " enables messages to TTY if T ")
(defvar  standalone-prover nil " t if prover run standalone, default is nil ")
(defvar  prover-debug-feature nil)  ; set to t is certain debugging features
                                    ; are present
(defvar  local-info-file nil)  ; default info file for local dumping
(defvar  shortnamelist nil)    ; mapping of short/long names
(defvar  typealist nil         " primtypealist plus user types " )
(defvar  prappendtrace nil)    ; t if trace is to append to file, not overwrite
(defvar  justskolemize nil)    ; t if main prover is NOT to be invoked
(defvar  +vc-mode+ nil)        ; t if generating vc's (HSP only) - 10/86 added
(defvar  proofname nil)
(defvar  sigalist nil)
(defvar  findalist nil)
(defvar  usealist nil)

;;; 10/86 FvH - the following stuff should all be obsolete:
; To see if "prover" returned the following error message, do this
; test, assuming the return was stored in "result":
; 	(eq result (symbolconc error-001-badpremises))
;
; To get result from file err-file-in, do:
;	(setq result (symbolconc (readline err-file-in)))
;
;; (defvar error-001-badpremises "Premises of PROVE command are inconsistent")

(defvar needed-if*  nil  "t if canonization inserted an if* to lift" )

; ------------------------------------------------------------------------
; more compiler declarations

(proclaim '(ftype (function (&rest t) t) prerr neatprint))	

(proclaim  
 '(special fnfalist applysymlist standvarlist prcommand formalist
	  saved-prcommand saved-formalist eqformalist addformerrmsg
	  usertypealist funcalist ))


(defvar res-info nil) ; holds the result of the proof for use by the trace

;;; the following special declarations are rather offensive:

(proclaim '(special const sum u s eq lit var coef product ineqpot  ))


;;; the following (til end-of-file) are taken from prpp:

(defconstant *infixlist*	;temporary list of infixes	
   '(equal nequal lessp greaterp lesseq lesseqp greatereq greatereqp
	   PLUS MINUS TIMES DIVIDE DIFFERENCE) )

(defconstant precedence-alist 					
  '((iff 1)
    (implies 2)
    (or 3)
    (and 4)
    (not 5)
    (equal 6)
    (nequal 6)
    (lessp 6)
    (greaterp 6)
    (lesseq 6)
    (lesseqp 6)
    (greatereq 6)
    (greatereqp 6)
    (PLUS 7)
    (MINUS 7)
    (DIFFERENCE 7)
    (TIMES 8)
    (DIVIDE 8)))

(defvar precedence-stack nil)

(defvar outfile nil)

(defvar p-info nil)

;;; variables added for hlp
(defvar trace-sname-p nil)    ; whether to use trace format for pp name
(defvar disable-sname-p nil)  ; whether to strip prover name in strip-name
(defvar trace-snamealist nil) ; alist of prover names to trace pp names
(defvar tnamealist nil)		; alist of prover names to printed names
				;(e.g. with @CF)
(defvar snamealist nil)		;alist of prover names to pp names
				; (i.e. no sko *)
(defvar skofnappalist nil)    ; alist of skolem fn applns used to suppress
                              ; excessive printing
(defvar *remembered-exprs* nil); list of prover expressions 
(defvar *remembered-prs* nil) ; list of substitutions (structures)
(defvar addformerrmsg nil)

(proclaim '(special trace-sname-p disable-sname-p
		    trace-snamealist tnamealist snamealist
		    *remembered-exprs* *remembered-prs*))
