;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals.lisp -- Global variables of PVS.  These are generally those shared
;;                 across multiple files.
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 00:21:19 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Apr  3 12:47:44 1998
;; Update Count    : 30
;; Status          : Alpha test
;; 
;; HISTORY
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; provide

(in-package :pvs)

;; shadow

(export '(*current-context* *current-theory* *typechecking-module* *pvs-directories*))

;; require

;; use-package

;; import

(defvar *pvs-path* nil
  "Set by Emacs")

(defvar *pvs-emacs-interface* nil
  "Set to t by Emacs in pvs-load - affects how pvs-emacs functions work")

(defparameter *pvs-directories*
  '("" "src" "src/prover" "src/decision-procedures" "src/interface"
    "src/utils" "BDD" "src/interface" "src/ics-interface"
    "src/WS1S/lisp" "src/abstraction"
    "src/ground-prover" "src/groundeval" "src/inst-by-unif" ))

(defparameter *pvs-version* "3.0 Beta")
(defparameter *binfile-version* 18)

(defparameter *context-name* ".pvscontext")

(defvar *pvs-global-tables* nil)

(defvar *bin-theories-set* nil)

(defvar *pvs-context* nil)

(defvar *pvs-context-path* nil
  "The current context path - this can change when loading libraries")

(defvar *pvs-current-context-path* nil
  "The current context path - this one only changes with change-context")

(defvar *pvs-initialized* nil)
(defvar *pvs-files* nil)

(defvar *pvs-tmp-file* nil
  "Used to send large amounts of information to/from emacs")

(defvar *pvs-verbose* nil
  "Flag indicating level of messages to print when noninteractive")

(defvar *suppress-msg* nil
  "Flag indicating whether to suppress messages output with pvs-message")

(defvar *pp-new-projection-forms* nil
  "Flag indicating to prettyprint new projection forms, e.g., foo`1 rather
than proj_1(foo).")

(defvar *comment-on-proof-status* nil
  "Flag indicating whether to include the proof status when unparsing a
formula declaration.")

(defvar *show-conversions* t
  "Flag indicating whether conversions are to be displayed when unparsing.")

(defvar *no-conversions-allowed* nil)

(defvar *from-buffer* nil
  "Set to the buffer from which a temporary file was generated for
parsing or typechecking - used by pvs-error.")

(defvar *load-prelude-hook* nil
  "Functions (with no args) to be called when the prelude is (re)loaded")

(defvar *untypecheck-hook* nil
  "Functions (with no args) to be called whenever untypecheck is called")

(defvar *prelude-context* nil
  "Provides the context associated with the prelude")

(defvar *prelude* (make-hash-table :test #'eq :rehash-size 1)
  "The hash-table of prelude")

(defvar *prelude-theories* nil
  "A list of the prelude theories; more useful than *prelude* when the
order is important")
  
(defvar *prelude-library-context* nil
  "Provides the context associated with the current prelude libraries")

(defvar *prelude-libraries* nil
  "The pathnames of the prelude libraries that have been loaded.
Given a pathname, returns a hash-table which can then be put into
*visible-libraries*")

(defvar *imported-libraries* nil)

(defvar *visible-libraries* nil
  "Those libraries that are visible - PVS context dependent")

(defvar *prelude-libraries-uselist* nil)

(defvar *visible-libraries-uselist* nil
  "Used to be able to quickly build a context - keeps track of the visible
prelude libraries")

(defvar *loaded-libraries* nil)

(defvar *library-alist* nil)

(defvar *pvs-modules* nil
  "The hash-table of modules known to the system in this session")

(defvar *current-system* 'pvs
  "Used to indicate which system is being parsed, etc. e.g., PVS, DC, ...")

(defvar *current-library* nil)
(defvar *current-file* nil)
(defvar *current-theory* nil
  "The module which is currently being focused on")
(defvar *typechecking-module* nil
  "A flag indicating whether we are typechecking a module - used to control
   TCC generation")

;;; *circular-file-dependencies* is an alist associating each filename
;;; with a list of circularities.  It is computed after typechecking a
;;; file, and bin files are not generated for any files involved in a
;;; circularity.  It has the form ((file th1 th2 ...) ...)
(defvar *circular-file-dependencies* nil)

(defvar *in-typechecker* nil
  "A flag indicating that we are in the typechecker")

(defvar *in-evaluator* nil)

(defvar *pvs-eval-do-timing* nil)

(defvar *in-coercion* nil)

(defvar *noninteractive* nil)
(defvar *noninteractive-timeout* nil)
(defvar *proof-timeout* nil)

(defvar *recursive-calls-without-enough-args* nil)

(defvar *recursive-subtype-term* nil)

(defvar *expression-types* (make-hash-table :test 'eq))

(defvar *set-type-formal* nil)

(defvar *set-type-actuals-name* nil)

(defvar *loading-prelude* nil "A flag indicating the obvious")

(defvar *loading-library* nil
  "A flag indicating that a library is being loaded.")

(defvar *tc-theories* nil "Used to check for IMPORT circularities.")
(defvar *current-context* nil
  "The default context used when creating expressions")
(defvar *tex-mode* nil)
;(defvar *id-to-modinsts* nil)

(defvar *type-error-catch* nil
  "Set to a value to throw to when trying to control typechecking.")

(defvar *skip-k-conversion-check* nil)

(defvar *tc-match-exact* nil)

(defvar *term-print-strings* nil)

(defvar *place-error-flag* nil)

(defvar *disable-gc-printout* nil)

;;; An association list of operators and their places.  The problem is
;;; that operators are thrown away, and later make-sb-term is called with
;;; just an id.  We thus keep all possible places associated with an id,
;;; and in make-sb-term set the place attribute if there is a unique one,
;;; otherwise we set a places attribute, and wait for parse to determine
;;; the right place from the argument places.

(defvar *operator-places* nil)

;;; These variables are types, used for many built-in functions and constants.
(defvar *boolean* nil)
(defvar *true* nil)
(defvar *false* nil)
(defvar *number* nil)
(defvar *real* nil)
(defvar *rational* nil)
(defvar *integer* nil)
(defvar *naturalnumber* nil)
(defvar *posint* nil)
(defvar *even_int* nil)
(defvar *odd_int* nil)
(defvar *ordinal* nil)

(defvar *tcdebug* nil)
(defvar *evaluator-debug* nil)
(defvar *evaluator-debug-undefined* nil)

;;(defvar *generating-tcc* nil)

(defvar *generate-tccs* nil
  "Flag indicating whether TCCs need to be generated, has one of the values
NIL (the default), TOP, ALL, or NONE.")

(defvar *false-tcc-error-flag* t
  "Flag indicating whether false TCCs should lead to a type error")

(defvar *tccs* nil "The TCC declarations generated while typechecking a file")
(defvar *tccforms* nil
  "The TCCs generated while typechecking an expression when in the prover,
evaluator, or when *collecting-tccs* is true.")
(defvar *collecting-tccs* nil
  "Controls whether TCCs are inserted in the current theory or simply
collected in *tccforms*")
;(defvar *suppress-proved-tccs* nil
;  "Whether to suppress proved tccs when prettyprinting")
(defvar *recursive-tcc-names* nil
  "Keeps track of recursive names for which TCCs have been generated.")

;;; Associate old tcc names with new tccs, so that proofs may be restored.
(defvar *old-tcc-names* nil)

(defvar *compatible-pred-reason* nil)

(defvar *expressions-seen-for-tccs* nil)

(defvar *full-conversion-type* nil)
(defvar *allow-free-variables* nil)
(defvar *bound-variables* nil)
(defvar *typecheck-args* nil)
(defvar *tcc-conditions* nil)

(defvar *generating-mapped-axiom-tcc* nil)

(defvar *valid-id-check* t
  "Indicates whether the parser should check for valid ids.")

(defvar *no-obligations-allowed* nil)

(defvar *copy-print-type* nil
  "Controls the setting of print-type in gensubst")

(defvar *record-and-tuple-types* nil
  "Contains all of the user-declared record and tuple types, used to
generate unique accessors for these types")
(defvar *generating-adt* nil)

(defvar *adt-generated-string*
  "%%% ADT file generated from ")

(defvar *adt-decl* nil
  "Set to the declarations of a datatype while typechecking the generated
declarations.  Used by pvs-error to put the cursor on the datatype declaration
rather than the generated declaration.")

(defun pprint-comment-strings (stream string)
  (let ((lines (mk::split-string string :item #\newline))
	(ccol 1 ;(1+ (excl:stream-line-column stream))
	      ))
    (when (and (cdr lines) (integerp ccol) (> ccol 0)
	       (every #'(lambda (line)
			  (and (> (length line) ccol)
			       (= (count #\space line :end ccol) ccol)))
		      (cdr lines)))
      (setq lines
	    (cons (car lines)
		  (mapcar #'(lambda (line)
			      (subseq line ccol))
		    (cdr lines)))))
    (pprint-logical-block (stream lines :prefix "\"" :suffix "\"")
      (pprint-indent :block 0)
      (loop (pprint-exit-if-list-exhausted)
	    (write (pprint-pop) :stream stream :escape nil :pretty nil
		   :pprint-dispatch nil)
	    (pprint-exit-if-list-exhausted)
	    (pprint-newline :mandatory stream)))))

(defvar *proof-script-pprint-dispatch*
  #-gcl
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons string)
			 #'(lambda (s list)
			     (let ((*print-escape* t))
			       (pprint-linear s list)))
			 1
			 table)
    (set-pprint-dispatch 'string
			 #'pprint-comment-strings
			 1
			 table)
    table)
  #+gcl nil)

(defvar *visible-only* nil)

(defvar *parsing-or-unparsing* nil
  "Indicates whether parsing/unparsing is going on, primarily used so
that gensubst does not try to pseudo-normalize inappropriately.")

(defvar *unparse-all* nil)
(defvar *no-comments* t
  "Controls the printing of comments; see ergo-runtime-fixes.lisp")

(defvar *andusingctl* "~#[...nothing~;~a~;~a and ~a~:;~@{~#[~;and ~]~a~^, ~}~]"
  "Used to print 1, 2, and 3 or more items nicely.")

(defvar *modules-visited* nil)

(defvar *in-pvs-batch* nil)

;;; Prover-related variables

(defvar *use-default-dp?* nil)
(defvar *prover-print-lines* nil)
(defvar *print-lines* nil)

(defvar *substit-dont-simplify* nil)

(defvar *context-modified* nil
  "Set by add-declaration and modify-declaration to indicate that the
current proof is suspect.")

;;; Set when typechecking add- and mod-decls
(defvar *tc-add-decl* nil)

;;; A list of (type . gensym) pairs, where type is a subtype.  Needed in
;;; order to ensure soundness of deBruijnized expressions.
(defvar *subtype-names* nil)

;;; Used to speed up subtype-of? calculations
(defvar *subtype-of-hash* (make-hash-table :test #'eq))

(defvar *subtypes-matched* nil)

(defvar *named-exprs* nil
  "A list of (expr . gensym) pairs, where expr is a binding-expr in which
a connective occurs.")

(defvar *rec-type-dummies* nil
  "A list of (rectype . gensym) pairs, where type is a recordtype.
Needed to generate the same dummy name for record literals.")

(defvar *keep-unbound* nil
  "Used by universal-closure to control the quantification.")

(defvar *last-proof* nil
  "The last proof attempted - an instance of proofstate")


;;; Variables used in the X proof display code (wish.lisp)

(defvar *start-proof-display* nil)

(defvar *displaying-proof* nil)

(defvar *current-displayed* nil)

(defvar *flush-displayed* nil)


(defvar *assert-flag* nil)

(defvar *showprogess* 'yes)
(defvar *quietsuspend* 'yes)
(defvar *quietmodulesave* 'no)
(defvar *autoimport* 'yes)
(defvar *autotypecheck* 'yes)
;(defvar *tctimeout* 1)
(defvar *tcc-messages* 'no)
(defvar *ppcase* 'upper)
(defvar *ppcomments* 'append)
(defvar *ppstyle* ())
(defvar *pplinelength* 78)
(defvar *ppmorespace* 0)
(defvar *ppnewlinecomments* 'indent)
(defvar *ppmacros* nil)
;(defvar *prbell* 'yes)
;(defvar *prbetareduce* 'yes)
;(defvar *prchain* 'terse)
;(defvar *printerpmult* 'normal)
;(defvar *printerpdivide* 'no)
;(defvar *prhalt* 'error)
;(defvar *prtimeout* 60)
;(defvar *prtrace* 'mixed)
;(defvar *prtried* 'ask)
;(defvar *prmode* 'checking)

(defvar applysymlist nil)

(defparameter *pvs-operators*
  '((= . |equal|)
    (< . |lessp|)
    (<= . |lesseqp|)
    (> . |greaterp|)
    (>= . |greatereqp|)
    (+ . |plus|)
    (- . |difference|)
    (* . |times|)
    (/ . |divide|)
    (/= . |notequal|)
    (== . |equiv|)
    (& . |and|)
    (/\\ . |wedge|)
    (\\/ . |vee|)
    (^ . |caret|)
    ([] . |box|)
    (<> . |diamond|)
    (~ . |tilde|)
    (=> . |implies|)
    (<=> . |iff|)
    (++ . |doubleplus|)
    (-- . |doubleminus|)
    (** . |doubletimes|)
    (// . |doubledivide|)
    (^^ . |doublecaret|)
    (<< . |doublelessp|)
    (>> . |doublegreaterp|)
    (<<= . |doublelesseqp|)
    (>>= . |doublegreatereqp|)
    (\# . |sharp|)
    (@@ . |doubleat|) 
    (\#\# . |doublesharp|)
    (\|- . |turnstile|)
    (\|= . |models|)
    (<\| . |triangleleft|)
    (\|> . |triangleright|)
    ([\|\|] . |specialbrackets|)
    (\(\|\|\) . |specialparens|)
    ({\|\|} . |specialbraces|)
    (O . |oh|)
    (|o| . |oh|)
    ))
