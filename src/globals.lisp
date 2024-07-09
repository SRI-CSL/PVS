;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; globals.lisp -- Global variables of PVS.  These are generally those shared
;;                 across multiple files.
;; Author          : Sam Owre
;; Created On      : Wed Nov  3 00:21:19 1993
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Apr  3 12:47:44 1998
;; Update Count    : 30
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006-2013, SRI International.  All Rights Reserved.

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

(defvar *files-loaded* nil
  "Keeps track of files that have been loaded using defwrapper/defadvice
in util.lisp")

(defvar *loading-files* :other)

(defvar *pvs-path* nil "Set by make process")

(defvar *pvs-library-path* nil
  "Set from the PVS_LIBRARY_PATH environment variable + *pvs-path*")

(defvar *pvs-websocket-interface* nil
  "Set to t by Websocket when a message is received")

(defparameter *pvs-directories*
  '("" "src" "src/prover" "src/decision-procedures" "src/interface"
    "src/utils" "src/BDD" "src/interface" "src/ics-interface"
    "src/WS1S/lisp" "src/abstraction" "src/ground-prover" "src/groundeval"
    "src/PVSio" "src/inst-by-unif" "src/Field" "src/Manip" "src/ProofLite"
    "src/rahd" "src/cl-json/src"))

#+allegro
(defvar *pvs-lisp-process* nil
  "Set to mp:*current-process* in pvs-server, in order to use
  mp:symeval-in-process and get the dynamic values of *ps* and
  *top-proofstate*")

#+allegro
(defun pvs-build-date ()
  (let ((end (position #\space excl::cl-release-date :from-end t)))
    (subseq excl::cl-release-date 0 end)))

(defvar *pvs-version* "8.0")

(eval-when (:execute :compile-toplevel :load-toplevel)
  ;; Not used in PVS sources, but may be useful for patches, strategies, etc.
  ;;(pushnew (intern (format nil "pvs~a" *pvs-version*) :keyword) *features*)
  (pushnew :pvs8.0 *features*)
  (pushnew :pvs8 *features*)
  (pushnew :pvs *features*)
  )

(defvar *pvs-git-describe*)

(defparameter *binfile-version* 38)

(defvar *ignore-binfile-errors* t)

(defvar *pvsbin-string* "pvsbin")

(defvar *pvs-build-time* (get-universal-time))

(defparameter *context-name* ".pvscontext")

(defvar *pvs-global-tables* nil)

(defvar *adt-type-name-pending*)

(defvar *pvs-initialized* nil)

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

(defvar *checking-conversions* nil
  "Only needed when lisp assertions are being checked")

(defvar *no-conversions-allowed* nil)

(defvar *conversions-allowed* nil)

(defvar *allow-var-decl-comparison* nil
  "Used in tc-eq test for assuming-instances in tcc-gen.lisp")

(defvar *from-buffer* nil
  "Set to the buffer from which a temporary file was generated for
parsing or typechecking - used by pvs-error.")

(defvar *load-prelude-hook* nil
  "Functions (with no args) to be called when the prelude is (re)loaded")

(defvar *untypecheck-hook* nil
  "Functions (with no args) to be called whenever untypecheck is called")

(defvar *dont-untypecheck-recdef-conversions* nil
  "Flag to control untypechecking of recursive-defn-conversions - see typecheck* (def-decl)")

(defvar *added-recursive-defn-conversions*) ;; bound in typecheck* (def-decl)

(defvar *prelude-core-context* nil
  "Provides the context associated with the prelude")

(defvar *prelude-context* nil
  "Provides the context associated with the prelude")

(defvar *prelude* (make-hash-table :test #'eq :rehash-size 1)
  "The hash-table of prelude")

(defvar *prelude-theories* nil
  "A list of the prelude theories; more useful than *prelude* when the
order is important")

(defvar *global-prelude-libraries* nil
  "A list of libraries to be treated as extensions to the prelude.
These are not associated with a workspace, and usually done through
.pvs.lisp files, either in the home directory or in the PVS_LIBRARY_PATH.")

(defvar *all-workspace-sessions* nil
  "List of all workspace-session instances, see classes-decl.lisp for details.")

(defvar *current-system* 'pvs
  "Used to indicate which system is being parsed, etc. e.g., PVS, DC, ...")

(defvar *current-library* nil)
(defvar *current-file* nil)
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

;; This is set by the PVSTIMEOUT environment variable,
;; which in turn comes from the -timeout arg
(defvar *noninteractive-timeout* nil)

;;; This timeout is used when running typecheck-prove
;;; the proof is not a tcc strategy
(defvar *tcp-timeout* 300)

;;; This timeout is used when proving a TCC using the default TCC strategies
;;; as these can lead to infinite loops. Note that this has no effect on
;;; TCCs with proofs other than the default TCC strategies (e.g., tcc,
;;; subtype-tcc, etc.)
(defvar *tcc-timeout* 30)

(defvar *use-rationals* nil)

(defvar *recursive-calls-without-enough-args* nil)

(defvar *recursive-subtype-term* nil)

(defvar *added-recursive-def-conversion* nil)

(defvar *expression-types* (make-hash-table :test 'eq))

(defvar *resolve-name* nil)

(defvar *set-type-formal* nil)

(defvar *set-type-actuals-name* nil)
(defvar *set-type-subtype* nil)
(defvar *set-type-expr* nil)

(defvar *loading-prelude* nil "A flag indicating the obvious")

(defvar *loading-library* nil
  "A flag indicating that a library is being loaded.")

(defvar *tc-theories* nil "Used to check for IMPORT circularities.")

(defvar *workspace-session* nil)

(defvar *current-context* nil
  "The default context used when creating expressions")

(defvar *current-top-declaration* nil
  "Keeps track of the *real* current-declaration")

(defvar *tex-mode* nil)
;(defvar *id-to-modinsts* nil)

(defvar *parse-error-catch* nil
  "Set to a value to throw to when trying to control parsing.")

(defvar *coercion-var-counter* (let ((x 0)) #'(lambda ()  (incf x))))

(defvar *type-error-catch* nil
  "Set to a value to throw to when trying to control typechecking.")

(defvar *skip-k-conversion-check* nil)

(defvar *tc-match-exact* nil)

(defvar *tc-match-type-names* nil)

(defvar *term-print-strings* nil)

(defvar *place-error-flag* nil)

(defvar *disable-gc-printout* nil)

;; Controls when periods are allowed in declaration ids
;; Essentially allowed anywhere except user-introduced declarations.
(defvar *xt-periods-allowed* nil)

;;; An association list of operators and their places.  The problem is
;;; that operators are thrown away, and later make-sb-term is called with
;;; just an id.  We thus keep all possible places associated with an id,
;;; and in make-sb-term set the place attribute if there is a unique one,
;;; otherwise we set a places attribute, and wait for parse to determine
;;; the right place from the argument places.

(defvar *operator-places* nil)

(defparameter *infix-operators*
  '(O ∘ IFF <=> ⇔ IMPLIES => ⇒ WHEN OR ∨ \\/ AND /\\ ∧ & XOR ANDTHEN ORELSE
	^ + - * / ++ ~ ** // ^^ \|- ⊢ \|= ⊨ <\| \|> = /= ≠ == < <= > >=
	<< >> <<= >>= |#| @@ |##| ∘ ∨ ∧ ⊕ ⊘ ⊗ ⊖ ⊙ ⊛ ⨁ ⨂ ⨀
	⊢ ⊨ ± ∓ ∔ × ÷ ⊞ ⊟ ⊠ ≁ ∼ ≃ ≅ ≇ ≈ ≉ ≍ ≎ ≏ ≐ ≗ ≙ ≡ ⋈ ≤ ≥ ≦ ≧
	≨ ≩ ≪ ≫ ≮ ≯ ≰ ≱ ≺ ≻ ◁ ▷ ∈ ∉ ∋ ∩ ∪ ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊎ ⊊ ⊋
	⊏ ⊐ ⊓ ⊔ ⋀ ⋁ ⋂ ⋃ • ← ↑ → ↓ ↝ ↦ ⇐ ⇒ ⇑ ⇓ ⇔ ∇ ⊣ ⊥ ⊩ ◯ ★ ✠ √))

(defparameter *unary-operators* '(NOT ¬ + - ~ □ ◇ <> ◯ √))

;;; These variables are types, used for many built-in functions and constants.
(defvar *boolean* nil)
(defvar *true* nil)
(defvar *false* nil)
(defvar *number* nil)
(defvar *number_field* nil)
(defvar *real* nil)
(defvar *rational* nil)
(defvar *integer* nil)
(defvar *naturalnumber* nil)
(defvar *posint* nil)
(defvar *negint* nil)
(defvar *posrat* nil)
(defvar *negrat* nil)
(defvar *even_int* nil)
(defvar *odd_int* nil)
(defvar *even_nat* nil)
(defvar *even_posnat* nil)
(defvar *odd_posnat* nil)
(defvar *even_negint* nil)
(defvar *odd_negint* nil)
(defvar *ordinal* nil)
(defvar *string-type* nil)
(defvar *character* nil)
(defvar *char* nil)
(defvar *uint8* nil)
(defvar *uint16* nil)
(defvar *uint32* nil)
(defvar *uint64* nil)
(defvar *int8* nil)
(defvar *int16* nil)
(defvar *int32* nil)
(defvar *int64* nil)
(defvar *pos_uint8* nil)
(defvar *pos_uint16* nil)
(defvar *pos_uint32* nil)
(defvar *pos_uint64* nil)
(defvar *neg_int8* nil)
(defvar *neg_int16* nil)
(defvar *neg_int32* nil)
(defvar *neg_int64* nil)

(defvar *even_uint8* nil)
(defvar *even_pos_uint8* nil)
(defvar *even_pos_uint16* nil)
(defvar *even_pos_uint32* nil)
(defvar *even_pos_uint64* nil)
(defvar *even_neg_int8* nil)
(defvar *even_neg_int16* nil)
(defvar *even_neg_int32* nil)
(defvar *even_neg_int64* nil)
(defvar *odd_pos_uint8* nil)
(defvar *odd_pos_uint16* nil)
(defvar *odd_pos_uint32* nil)
(defvar *odd_pos_uint64* nil)
(defvar *odd_neg_int8* nil)
(defvar *odd_neg_int16* nil)
(defvar *odd_neg_int32* nil)
(defvar *odd_neg_int64* nil)


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

;; Used to keep track of which expression have already gone through
;; check-type-actuals processing
(defvar *exprs-generating-actual-tccs* nil)

(defvar *tccs-generated-for*)

;;; Associate old tcc names with new tccs, so that proofs may be restored.
(defvar *old-tcc-names* nil)

(defvar *compatible-pred-reason* nil)

(defvar *expressions-seen-for-tccs* nil)

(defvar *full-conversion-type* nil)
(defvar *decl-params* nil)
(defvar *allow-free-variables* nil)
(defvar *bound-variables* nil)

;;(defvar *set-type-exprs*)

;;; *decl-bound-parameters* is used in fully-instantiated?
;;; Normally the current-declaration is used, but, e.g., in generating
;;; datatype accessor declarations the formals are needed before the
;;; declaration is available.
;;; (defvar *decl-bound-parameters* nil)
(defvar *typecheck-args* nil)
(defvar *tcc-conditions* nil)
;;; We need to have extra conditions on the recursive function calls, but
;;; only if they actually occur.  E.g., in
;;; f(n: nat): RECURSIVE nat = if n=0 then 0 ELSE f(f(n-1)-1) endif
;;; the TCC for f(n-1)-1 replaces f with v, and in the recursive judgement,
;;; v and a precondition on v are added to *rec-judgement-extra-conditions*,
;;; Whaich are then added to the TCC conditions if needed.
(defvar *rec-judgement-extra-conditions* nil)

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

(defvar *ignore-exportings* nil)

(defvar *typecheck-using* nil)

(defvar *importing-axioms* nil)

;; (defun pprint-comment-strings (stream string)
;;   (let ((lines (uiop:split-string string :separator "#\newline"))
;; 	(ccol 1 ;(1+ (excl:stream-line-column stream))
;; 	      ))
;;     (when (and (cdr lines) (integerp ccol) (> ccol 0)
;; 	       (every #'(lambda (line)
;; 			  (and (> (length line) ccol)
;; 			       (= (count #\space line :end ccol) ccol)))
;; 		      (cdr lines)))
;;       (setq lines
;; 	    (cons (car lines)
;; 		  (mapcar #'(lambda (line)
;; 			      (subseq line ccol))
;; 		    (cdr lines)))))
;;     (pprint-logical-block (stream lines :prefix "\"" :suffix "\"")
;;       (pprint-indent :block 0)
;;       (loop (pprint-exit-if-list-exhausted)
;; 	    (write (pprint-pop) :stream stream :escape nil :pretty nil
;; 		   :pprint-dispatch
;; 		   #-sbcl nil
;; 		   #+sbcl (sb-pretty::make-pprint-dispatch-table #() nil nil)
;; 		   )
;; 	    (pprint-exit-if-list-exhausted)
;; 	    (pprint-newline :mandatory stream)))))

(defvar *proof-script-pprint-dispatch*
  (let ((table (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons string)
			 #'(lambda (s list)
			     (let ((*print-escape* t))
			       (pprint-linear s list)))
			 1
			 table)
    ;; (set-pprint-dispatch 'string
    ;; 			 #'pprint-comment-strings
    ;; 			 1
    ;; 			 table)
    table))

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

;;; Prover-related variables

(defvar *use-default-dp?* nil)
(defvar *prover-print-lines* nil)

(defvar *report-mode* nil)

(defvar *report-tccs* nil
  "If not nil, prover prints result of each TCC simplification")

(defvar *print-descendants* nil)

(defvar *print-ancestor* nil)


#-sbcl
(defvar *print-lines* nil)

(defvar *substit-dont-simplify* nil)

(defvar *subst-params-decl* nil)

(defvar *context-modified* nil
  "Set by add-declaration and modify-declaration to indicate that the
current proof is suspect.")

(defvar *insert-add-decl* t
  "Flag used for the add-declaration and modify-declaration commands to
   allow typechecking without side effects.")


;;; Set when typechecking for add- and mod- decl commands
(defvar *tc-add-decl* nil)

;;; A list of (type . gensym) pairs, where type is a subtype.  Needed in
;;; order to ensure soundness of deBruijnized expressions.
(defvar *subtype-names* nil)

;;; Used to speed up subtype-of? calculations
(defvar *subtype-of-hash* (make-hash-table :test #'equal))

(defvar *subtypes-matched* nil)

(defvar *subst-fields-hash*)

(defvar *type-predicates-recordtype-hash*)

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

;;(defvar *dump-proof-data-to-file* nil)

(defvar *log-proofs* :ask)

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
(defvar *tcc-messages* 'no)
(defvar *ppcase* nil
  "Indicates the prefered prettyprinting for PVS terms.
Allowed values are:
 :unicode - try to use the unicode symbol, if available
 :short - try to use short ascii forms, e.g. &, => rather than AND, IMPLIES
 :upper - use uppercase keywords
 :lower - use lowercase keywords
 nil - use the form given in the term originally, but uppercase")
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
    (≠ . |notequal|)
    (== . |equiv|)
    (& . |and|)
    (/\\ . |wedge|)
    (∧ . |wedge|)
    (\\/ . |vee|)
    (∨ . |vee|)
    (^ . |caret|)
    ;;([] . |box|)
    (¬ . |not|)
    (<> . |diamond|)
    (◇ . |diamond|)
    (□ . |box|)
    (~ . |tilde|)
    (=> . |implies|)
    (⇒ . |arimplies|)
    (<=> . |iff|)
    (⇔ . |ariff|)
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
    (⊢ . |turnstile|)
    (\|= . |models|)
    (⊨ . |models|)
    (<\| . |triangleleft|)
    (\|> . |triangleright|)
    ([\|\|] . |specialbrackets|)
    (\(\|\|\) . |specialparens|)
    ({\|\|} . |specialbraces|)
    (O . |oh|)
    (|o| . |oh|)
    (∘ . |oh|)
    (⊕ . |oplus|)
    (≁ . |nsim|)
    (≃ . |simeq|)
    (≅ . |cong|)
    (∼ . |sim|)
    (≇ . |ncong|                        ) (≍ . |asymp|)
    (≈ . |approx|                       ) (≉ . |napprox|)
    (≎ . |Bumpeq|                       ) (≏ . |bumpeq|)
    (≐ . |doteq|                        ) (≗ . |circeq|)
    (≙ . |defs|                         ) (≡ . |equiv|)
    (≤ . |leq|                          ) (≥ . |geq|)
    (≦ . |leqq|                         ) (≧ . |geqq|)
    (≨ . |lneq|                         ) (≩ . |gneq|)
    (≪ . |ll|                           ) (≫ . |gg|)
    (≮ . |nless|                        ) (≯ . |ngtr|)
    (≰ . |nleq|                         ) (≱ . |ngeq|)
    (≺ . |prec|                         ) (≻ . |succ|)
    (▷ . |rhd|                          ) (◁ . |lhd|)
    (∈ . |in|                           ) (∉ . |notin|)
    (⊂ . |subset|                       ) (⊃ . |supset|)
    (⊄ . |nsubset|                      ) (⊅ . |nsupset|)
    (⊆ . |subseteq|                     ) (⊇ . |supseteq|)
    (⊊ . |subsetneq|                    ) (⊋ . |supsetneq|)
    (⊏ . |sqsubset|                     ) (⊐ . |sqsupset|)
    (• . |bullet|                       ) (∋ . |ni|)
    (← . |leftarrow|                    ) (↑ . |uparrow|)
    (→ . |rightarrow|                   ) (↓ . |downarrow|)
    (↝ . |leadsto|                      ) (↦ . |mapsto|)
    (⇐ . |Leftarrow|                    ) (⋂ . |bigcap|)
    (⇑ . |Uparrow|                      ) (⇓ . |Downarrow|)
    (∇ . |nabla|                        ) (⊣ . |dashv|)
    (⊥ . |perp|                         ) (⊩ . |Vdash|)
    (◯ . |bigcirc|                      ) (★ . |bigstar|)
    (✠ . |maltese|                      ) (⋈ . |Join|)
    (⊖ . |ominus|                       ) (⨁ . |n-ary_circled_plus_operator|)
    (± . |pm|                           ) (∓ . |mp|)
    (∔ . |dotplus|                      ) (⊞ . |boxplus|)
    (⊟ . |boxminus|                     ) (⊎ . |uplus|)
    (∪ . |cup|                          ) (⊔ . |sqcup|)
    (⋁ . |bigvee|                       ) (⋃ . |bigcup|)
    (⊘ . |oslash|                       ) (⊗ . |otimes|)
    (⊙ . |odot|                         ) (⊛ . |circledast|)
    (⨂ . |n-ary_circled_times_operator| ) (⨀ . |n-ary_circled_dot_operator|)
    (× . |times|                        ) (÷ . |div|)
    (⊠ . |boxtimes|                     ) (∩ . |cap|)
    (⊓ . |sqcap|                        ) (⋀ . |bigwedge|)
    ))

(defparameter *pvs-relational-operators*
  ;; Operators that can be chained, e.g., "1 < x <= 2",
  ;; which is translated as "1 < x and x <= 2"
  ;; This is basically all relational operators
  '(= /= < <= > >= == << >> <| |>
    ≁ ∼ ≃ ≅ ≇ ≈ ≉ ≍ ≎ ≏ ≐ ≗ ≙ ≡ ⋈ ≤ ≥ ≦ ≧
    ≨ ≩ ≪ ≫ ≮ ≯ ≰ ≱ ≺ ≻ ◁ ▷
    ⊂ ⊃ ⊄ ⊅ ⊆ ⊇ ⊊ ⊋ ⊏ ⊐ ⊑ ⊒))

(defparameter *pvs-equality-operators*
  ;; These are operators that are allowed between boolean terms
  ;; The idea is that it's OK to chain, e.g., "a < b > c", but not
  ;; "a < b = c", which should not be treated as "a < b AND b = c"
  ;; The only exception is if the equality-operators are the same, e.g.,
  ;; "a = b = c" is "a = b AND b = c".  Remember adding parentheses
  ;; means no chaining.
  '(= /= ≠))
