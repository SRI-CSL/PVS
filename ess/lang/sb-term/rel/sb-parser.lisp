;;; -*- Mode: Lisp; Package: SYNTAX-BOX -*-
(in-package :syntax-box)  ;; creates package for abstract syntax. 

(in-package :syntax-box)  ;; enters package for generated code.  

(use-package '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr))


(export '( sb-parse ))

(defparameter sb-abs-syn-package (find-package "syntax-box")) 

(defun sb-parse (&key (nt 'meta-grammar) error-threshold ask-about-bad-tokens
  (return-errors nil) (stream nil streamp) string file (exhaust-stream nil))
  (cond (stream)
        (string
         (setf stream (make-string-input-stream string)))
        (file
         (setf stream (open file)))
        (t
         (error
          "must provide an input means -- either :stream, ~
			:string, or :file.")))
  (setq *sb-keyword-table* (init-keyword-table sb-keyword-list))
  (let ((*parser-error* nil)
        (*parser-error-count* 0)
        (*parser-return-errors* return-errors)
        (*parser-abort-threshold* error-threshold)
        (*ask-about-bad-tokens* ask-about-bad-tokens)
        (*lexical-stream* (open-lexical-stream (open-placestream stream)))
        (*hold-a1* nil)
        (*hold-a2* nil)
        (*hold-b1* nil)
        (*hold-b2* nil)
        (*abs-syn-package* sb-abs-syn-package)
        (*reader-fun* #'reader)
        (*apply-lex-term-constr-fun* #'apply-lexical-terminal-constructor)
        (*keyword-table* *sb-keyword-table*)
        (*close-comment-char* sb-close-comment-char)
        (*case-sensitive* sb-case-sensitive))
    (init-lexer *lexical-stream*)
    (multiple-value-bind (abs-syntax error-string error-args error-place) (unwind-protect (catch
                      'parser-abort-catch
                      (prog1
                        (case nt
                          (meta-grammar (!meta-grammar! 0))
                          (external-grammars (!external-grammars! 0))
                          (comment-character (!comment-character! 0))
                          (escape-character (!escape-character! 0))
                          (operator-information (!operator-information! 0))
                          (lexical-terminals (!lexical-terminals! 0))
                          (bracketing-information (!bracketing-information! 0))
                          (spacing-information (!spacing-information! 0))
                          (precedence-information (!precedence-information! 0))
                          (multiple-levels (!multiple-levels! 0))
                          (single-level (!single-level! 0))
                          (single-op-precedence (!single-op-precedence! 0))
                          (nonterminal-definition (!nonterminal-definition! 0))
                          (pattern (!pattern! 0))
                          (augment (!augment! 0))
                          (format-command (!format-command! 0))
                          (t (error "unknown nonterminal ~a." nt)))
                        (when (or file string exhaust-stream)
                          (clet* (((token ignore place) (peek-first)))
                                 (unless (or (eq token :eof) (eq token 'eof))
                                   (do-syntax-error
                                    "there is garbage at the end of your ~
			      file or string:~%~a"
                                    place))))))
                     (unless
                      streamp
                      (close stream)))
      (if return-errors
          (values abs-syntax *parser-error* error-place error-string error-args)
          (values abs-syntax *parser-error*))))) 


(defun !format-command! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4)
    (initials-only
     ((la-match ((sbst::/+)))
      (progn
        (lam ((sbst::/+ epsilon)) (gobble-token))
        (lam ((sbst::!number! epsilon)) (gobble-to-slot v2))
        (value-to-slot v0 2)
        (as-to-slot v1 (make-sb-term 'incr-bp (list v2)))))
     ((la-match ((sbst::/-)))
      (progn
        (lam ((sbst::/- epsilon)) (gobble-token))
        (lam ((sbst::!number! epsilon)) (gobble-to-slot v2))
        (value-to-slot v0 3)
        (as-to-slot v1 (make-sb-term 'decr-bp (list v2)))))
     ((la-match ((sbst::?)))
      (progn
        (lam ((sbst::? epsilon)) (gobble-token))
        (alt-parse
         ((la-match ((sbst::!id!))) (value-to-slot v2 0)
          (lam ((sbst::!id! epsilon)) (gobble-to-slot v4)))
         ((la-match ((sbst::!number!))) (value-to-slot v2 1)
          (lam ((sbst::!number! epsilon)) (gobble-to-slot v4)))
         (t (initial-error '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
        (value-to-slot v3 v4)
        (value-to-slot v0 4)
        (as-to-slot v1 (make-sb-term 'unite (list v3)))))
     ((la-match ((sbst::_)))
      (progn
        (lam ((sbst::_ epsilon)) (gobble-token))
        (opt-parse ((sbst::!number! epsilon))
                   (lam ((sbst::!number! epsilon)) (gobble-to-slot v3)) v2)
        (value-to-slot v0 0)
        (as-to-slot v1
                    (if (= v2 0) (make-sb-term 'sp (list (mk-number 1)))
                        (make-sb-term 'sp (list v3))))))
     ((la-match ((sbst::|#|)))
      (progn
        (lam ((sbst::|#| epsilon)) (gobble-token))
        (value-to-slot v0 1)
        (as-to-slot v1 (make-sb-term 'cr (list)))))
     ((la-match ((sbst::!+)))
      (progn
        (lam ((sbst::!+ epsilon)) (gobble-token))
        (opt-parse ((sbst::!number! epsilon))
                   (lam ((sbst::!number! epsilon)) (gobble-to-slot v3)) v2)
        (value-to-slot v0 5)
        (as-to-slot v1
                    (if (= v2 0)
                        (make-sb-term 'push-indent (list (mk-number 2)))
                        (make-sb-term 'push-indent (list v3))))))
     ((la-match ((sbst::!-)))
      (progn
        (lam ((sbst::!- epsilon)) (gobble-token))
        (value-to-slot v0 6)
        (as-to-slot v1 (make-sb-term 'pop-indent (list)))))
     ((la-match ((sbst::@<)))
      (progn
        (lam ((sbst::@< epsilon)) (gobble-token))
        (value-to-slot v0 7)
        (as-to-slot v1 (make-sb-term 'push-tab-left (list)))))
     ((la-match ((sbst::@>)))
      (progn
        (lam ((sbst::@> epsilon)) (gobble-token))
        (value-to-slot v0 8)
        (as-to-slot v1 (make-sb-term 'push-tab-right (list)))))
     ((la-match ((sbst::@^)))
      (progn
        (lam ((sbst::@^ epsilon)) (gobble-token))
        (value-to-slot v0 9)
        (as-to-slot v1 (make-sb-term 'pop-tab (list)))))
     (t
      (initial-error
       '((sbst::@^ epsilon) (sbst::@> epsilon) (sbst::@< epsilon)
        (sbst::!- epsilon) (sbst::!+ epsilon sbst::!number!)
        (sbst::? sbst::!id! sbst::!number!) (sbst::/- sbst::!number!)
        (sbst::/+ sbst::!number!) (sbst::|#| epsilon)
        (sbst::_ epsilon sbst::!number!)))))
    (value-to-slot v0 v1)
    v0)) 


(defun !augment! (rbp &optional (bracket-list (empty-bracket-list)))
  (let (mtemp
        v0
        v1
        v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5)
    (stack-brackets sbst::{ augment)
    (initials
     ((la-match ((sbst::[)))
      (progn
        (lam ((sbst::[ epsilon)) (gobble-token))
        (code-to-slot v2 (!augment! 10))
        (lam ((sbst::] epsilon)) (gobble-token))
        (value-to-slot v0 21)
        (as-to-slot v1 (make-sb-term 'opt-aug (list v2)))))
     ((la-match ((sbst::!id! sbst::@)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v2))
        (lam ((sbst::@ epsilon)) (gobble-token))
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
        (value-to-slot v0 11)
        (as-to-slot v1 (make-sb-term 'ext-name (list v2 v3)))))
     ((la-match ((sbst::list)))
      (progn
        (lam ((sbst::list epsilon)) (gobble-token))
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (doublestar-parse
         ((sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::[ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::append sbst::|(|) (sbst::bcons sbst::|(|)
          (sbst::cons sbst::|(|) (sbst::list sbst::|(|)
          (sbst::null epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::seq epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doublestar epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::star epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doubleplus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::plus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::alt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::opt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!id! epsilon sbst::@ sbst::|(| sbst::* sbst::+ sbst::\|
           sbst::^)
          (sbst::!literal! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!number! epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!string! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^))
         (code-to-slot v3 (!augment! 0)) v2 v3 'sbst::|,|)
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 13)
        (as-to-slot v1 (make-sb-term 'list (ds-temp-rept (ck-rept-ref v2))))))
     ((la-match ((sbst::cons)))
      (progn
        (lam ((sbst::cons epsilon)) (gobble-token))
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (code-to-slot v2 (!augment! 0))
        (lam ((sbst::|,| epsilon)) (gobble-token))
        (code-to-slot v3 (!augment! 0))
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 14)
        (as-to-slot v1 (make-sb-term 'cons (list v2 v3)))))
     ((la-match ((sbst::bcons)))
      (progn
        (lam ((sbst::bcons epsilon)) (gobble-token))
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (code-to-slot v2 (!augment! 0))
        (lam ((sbst::|,| epsilon)) (gobble-token))
        (code-to-slot v3 (!augment! 0))
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 15)
        (as-to-slot v1 (make-sb-term 'bcons (list v2 v3)))))
     ((la-match ((sbst::append)))
      (progn
        (lam ((sbst::append epsilon)) (gobble-token))
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (code-to-slot v2 (!augment! 0))
        (lam ((sbst::|,| epsilon)) (gobble-token))
        (code-to-slot v3 (!augment! 0))
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 16)
        (as-to-slot v1 (make-sb-term 'append (list v2 v3)))))
     ((la-match ((sbst::!string! sbst::|(|)))
      (progn
        (lam ((sbst::!string! epsilon)) (gobble-to-slot v3))
        (value-to-slot v2 2)
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (doublestar-parse
         ((sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::[ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::append sbst::|(|) (sbst::bcons sbst::|(|)
          (sbst::cons sbst::|(|) (sbst::list sbst::|(|)
          (sbst::null epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::seq epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doublestar epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::star epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doubleplus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::plus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::alt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::opt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!id! epsilon sbst::@ sbst::|(| sbst::* sbst::+ sbst::\|
           sbst::^)
          (sbst::!literal! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!number! epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!string! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^))
         (code-to-slot v5 (!augment! 0)) v4 v5 'sbst::|,|)
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 17)
        (as-to-slot v1
                    (make-sb-term 'term-const
                                  (ds-temp-rept
                                   (mk-temp-rept
                                    (cons
                                     (case v2
                                       (2 (make-sb-term 'string-aug (list v3)))
                                       (1 (make-sb-term 'literal-aug (list v3)))
                                       (0 (make-sb-term 'id-aug (list v3))))
                                     (ds-temp-rept (ck-rept-ref v4)))))))))
     ((la-match ((sbst::!literal! sbst::|(|)))
      (progn
        (lam ((sbst::!literal! epsilon)) (gobble-to-slot v3))
        (value-to-slot v2 1)
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (doublestar-parse
         ((sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::[ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::append sbst::|(|) (sbst::bcons sbst::|(|)
          (sbst::cons sbst::|(|) (sbst::list sbst::|(|)
          (sbst::null epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::seq epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doublestar epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::star epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doubleplus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::plus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::alt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::opt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!id! epsilon sbst::@ sbst::|(| sbst::* sbst::+ sbst::\|
           sbst::^)
          (sbst::!literal! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!number! epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!string! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^))
         (code-to-slot v5 (!augment! 0)) v4 v5 'sbst::|,|)
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 17)
        (as-to-slot v1
                    (make-sb-term 'term-const
                                  (ds-temp-rept
                                   (mk-temp-rept
                                    (cons
                                     (case v2
                                       (2 (make-sb-term 'string-aug (list v3)))
                                       (1 (make-sb-term 'literal-aug (list v3)))
                                       (0 (make-sb-term 'id-aug (list v3))))
                                     (ds-temp-rept (ck-rept-ref v4)))))))))
     ((la-match ((sbst::!id! sbst::|(|)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
        (value-to-slot v2 0)
        (lam ((sbst::|(| epsilon)) (gobble-token))
        (doublestar-parse
         ((sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::[ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons
           sbst::list sbst::null sbst::seq sbst::doublestar sbst::star
           sbst::doubleplus sbst::plus sbst::alt sbst::opt sbst::!id!
           sbst::!literal! sbst::!number! sbst::!string!)
          (sbst::append sbst::|(|) (sbst::bcons sbst::|(|)
          (sbst::cons sbst::|(|) (sbst::list sbst::|(|)
          (sbst::null epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::seq epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doublestar epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::star epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::doubleplus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::plus epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::alt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::opt epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!id! epsilon sbst::@ sbst::|(| sbst::* sbst::+ sbst::\|
           sbst::^)
          (sbst::!literal! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!number! epsilon sbst::* sbst::+ sbst::\| sbst::^)
          (sbst::!string! epsilon sbst::|(| sbst::* sbst::+ sbst::\| sbst::^))
         (code-to-slot v5 (!augment! 0)) v4 v5 'sbst::|,|)
        (lam ((sbst::|)| epsilon)) (gobble-token))
        (value-to-slot v0 17)
        (as-to-slot v1
                    (make-sb-term 'term-const
                                  (ds-temp-rept
                                   (mk-temp-rept
                                    (cons
                                     (case v2
                                       (2 (make-sb-term 'string-aug (list v3)))
                                       (1 (make-sb-term 'literal-aug (list v3)))
                                       (0 (make-sb-term 'id-aug (list v3))))
                                     (ds-temp-rept (ck-rept-ref v4)))))))))
     ((la-match ((sbst::!string! epsilon)))
      (progn
        (lam ((sbst::!string! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 0)
        (as-to-slot v1 (make-sb-term 'string-aug (list v1)))))
     ((la-match ((sbst::!number!)))
      (progn
        (lam ((sbst::!number! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 1)
        (as-to-slot v1 (make-sb-term 'number-aug (list v1)))))
     ((la-match ((sbst::!literal! epsilon)))
      (progn
        (lam ((sbst::!literal! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 2)
        (as-to-slot v1 (make-sb-term 'literal-aug (list v1)))))
     ((la-match ((sbst::!id! epsilon)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 3)
        (as-to-slot v1 (make-sb-term 'name (list v1)))))
     ((la-match ((sbst::opt)))
      (progn
        (lam ((sbst::opt epsilon)) (gobble-token))
        (value-to-slot v0 4)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'opt))))))
     ((la-match ((sbst::alt)))
      (progn
        (lam ((sbst::alt epsilon)) (gobble-token))
        (value-to-slot v0 5)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'alt))))))
     ((la-match ((sbst::plus)))
      (progn
        (lam ((sbst::plus epsilon)) (gobble-token))
        (value-to-slot v0 6)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'plus))))))
     ((la-match ((sbst::doubleplus)))
      (progn
        (lam ((sbst::doubleplus epsilon)) (gobble-token))
        (value-to-slot v0 7)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'doubleplus))))))
     ((la-match ((sbst::star)))
      (progn
        (lam ((sbst::star epsilon)) (gobble-token))
        (value-to-slot v0 8)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'star))))))
     ((la-match ((sbst::doublestar)))
      (progn
        (lam ((sbst::doublestar epsilon)) (gobble-token))
        (value-to-slot v0 9)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'doublestar))))))
     ((la-match ((sbst::seq)))
      (progn
        (lam ((sbst::seq epsilon)) (gobble-token))
        (value-to-slot v0 10)
        (as-to-slot v1 (make-sb-term 'name (list (mk-id 'seq))))))
     ((la-match ((sbst::null)))
      (progn
        (lam ((sbst::null epsilon)) (gobble-token))
        (value-to-slot v0 12)
        (as-to-slot v1 (make-sb-term 'null (list)))))
     (t
      (initial-error
       '((sbst::!id! sbst::|(| sbst::@ epsilon)
        (sbst::!literal! sbst::|(| epsilon) (sbst::!string! sbst::|(| epsilon)
        (sbst::append sbst::|(|) (sbst::bcons sbst::|(|) (sbst::cons sbst::|(|)
        (sbst::list sbst::|(|) (sbst::null epsilon) (sbst::seq epsilon)
        (sbst::doublestar epsilon) (sbst::star epsilon)
        (sbst::doubleplus epsilon) (sbst::plus epsilon) (sbst::alt epsilon)
        (sbst::opt epsilon) (sbst::!number! epsilon)
        (sbst::[ sbst::!string! sbst::!number! sbst::!literal! sbst::!id!
         sbst::opt sbst::alt sbst::plus sbst::doubleplus sbst::star
         sbst::doublestar sbst::seq sbst::null sbst::list sbst::cons
         sbst::bcons sbst::append sbst::[ sbst::{)
        (sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons sbst::cons sbst::list
         sbst::null sbst::seq sbst::doublestar sbst::star sbst::doubleplus
         sbst::plus sbst::alt sbst::opt sbst::!id! sbst::!literal!
         sbst::!number! sbst::!string!)))))
    (value-to-slot v0 v1)
    (eat-brackets (sbst::} sbst::{) augment)
    (cond ((>
            (cond ((member (peek-first) '(sbst::* sbst::+ sbst::\| sbst::^))
                   (setq mtemp (peek-first))
                   (cadr
                    (assoc (peek-first)
                           '((sbst::^ 40) (sbst::+ 30) (sbst::* 30)
                            (sbst::\| 20)))))
                  (t
                   0))
            rbp)
           (loop (clean-variables (v1 v2 v3 v4 v5))
                 (case mtemp
                   (sbst::^
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::^ epsilon)) (gobble-token))
                      (alt-parse
                       ((la-match ((sbst::!id!))) (value-to-slot v3 0)
                        (lam ((sbst::!id! epsilon)) (gobble-to-slot v5)))
                       ((la-match ((sbst::!number!))) (value-to-slot v3 1)
                        (lam ((sbst::!number! epsilon)) (gobble-to-slot v5)))
                       (t
                        (initial-error
                         '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
                      (value-to-slot v4 v5)
                      (value-to-slot v0 22)
                      (as-to-slot v1 (make-sb-term 'tag-aug (list v2 v4)))))
                   (sbst::\|
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::\| epsilon)) (gobble-token))
                      (doubleplus-parse
                       ((sbst::{ sbst::{ sbst::[ sbst::append sbst::bcons
                         sbst::cons sbst::list sbst::null sbst::seq
                         sbst::doublestar sbst::star sbst::doubleplus
                         sbst::plus sbst::alt sbst::opt sbst::!id!
                         sbst::!literal! sbst::!number! sbst::!string!)
                        (sbst::[ sbst::{ sbst::[ sbst::append sbst::bcons
                         sbst::cons sbst::list sbst::null sbst::seq
                         sbst::doublestar sbst::star sbst::doubleplus
                         sbst::plus sbst::alt sbst::opt sbst::!id!
                         sbst::!literal! sbst::!number! sbst::!string!)
                        (sbst::append sbst::|(|) (sbst::bcons sbst::|(|)
                        (sbst::cons sbst::|(|) (sbst::list sbst::|(|)
                        (sbst::null epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::seq epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::doublestar epsilon sbst::* sbst::+ sbst::\|
                         sbst::^)
                        (sbst::star epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::doubleplus epsilon sbst::* sbst::+ sbst::\|
                         sbst::^)
                        (sbst::plus epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::alt epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::opt epsilon sbst::* sbst::+ sbst::\| sbst::^)
                        (sbst::!id! epsilon sbst::@ sbst::|(| sbst::* sbst::+
                         sbst::\| sbst::^)
                        (sbst::!literal! epsilon sbst::|(| sbst::* sbst::+
                         sbst::\| sbst::^)
                        (sbst::!number! epsilon sbst::* sbst::+ sbst::\|
                         sbst::^)
                        (sbst::!string! epsilon sbst::|(| sbst::* sbst::+
                         sbst::\| sbst::^))
                       (code-to-slot v4 (!augment! 21)) v3 v4 'sbst::\|)
                      (value-to-slot v0 20)
                      (as-to-slot v1
                                  (make-sb-term 'alt-aug
                                                (ds-temp-rept
                                                 (mk-temp-rept
                                                  (cons v2
                                                        (ds-temp-rept
                                                         (ck-rept-ref v3)))))))))
                   (sbst::+
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::+ epsilon)) (gobble-token))
                      (value-to-slot v0 19)
                      (as-to-slot v1 (make-sb-term 'plus-aug (list v2)))))
                   (sbst::*
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::* epsilon)) (gobble-token))
                      (value-to-slot v0 18)
                      (as-to-slot v1 (make-sb-term 'star-aug (list v2)))))
                   (t (medial-error '(sbst::* sbst::+ sbst::\| sbst::^))))
                 (value-to-slot v0 v1) (eat-brackets (sbst::} sbst::{) augment)
                 (when (<=
                   (cond ((member (peek-first)
                                  '(sbst::* sbst::+ sbst::\| sbst::^))
                          (setq mtemp (peek-first))
                          (cadr
                           (assoc (peek-first)
                                  '((sbst::^ 40) (sbst::+ 30) (sbst::* 30)
                                   (sbst::\| 20)))))
                         (t
                          0))
                   rbp)
                   (return nil)))))
    v0)) 


(defun !pattern! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore bracket-list))
  (let (mtemp
        v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (initials
     ((la-match ((sbst::{)))
      (progn
        (lam ((sbst::{ epsilon)) (gobble-token))
        (code-to-slot v3 (!pattern! 10))
        (cons-to-slot v2 v3)
        (gen-star-parse
         ((sbst::{ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
          (sbst::[ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
          (sbst::!keyword! epsilon sbst::jux sbst::\| sbst::* sbst::+ sbst::++
           sbst::** sbst::^ sbst::< sbst::<<)
          (sbst::!id! epsilon sbst::@ sbst::jux sbst::\| sbst::* sbst::+
           sbst::++ sbst::** sbst::^ sbst::< sbst::<<))
         (code-to-slot v3 (!pattern! 0)) v2 v3)
        (lam ((sbst::} epsilon)) (gobble-token))
        (value-to-slot v0 5)
        (as-to-slot v1 (make-sb-term 'seq (ds-temp-rept (ck-rept-ref v2))))))
     ((la-match ((sbst::[)))
      (progn
        (lam ((sbst::[ epsilon)) (gobble-token))
        (code-to-slot v3 (!pattern! 10))
        (cons-to-slot v2 v3)
        (gen-star-parse
         ((sbst::{ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
          (sbst::[ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
          (sbst::!keyword! epsilon sbst::jux sbst::\| sbst::* sbst::+ sbst::++
           sbst::** sbst::^ sbst::< sbst::<<)
          (sbst::!id! epsilon sbst::@ sbst::jux sbst::\| sbst::* sbst::+
           sbst::++ sbst::** sbst::^ sbst::< sbst::<<))
         (code-to-slot v3 (!pattern! 0)) v2 v3)
        (lam ((sbst::] epsilon)) (gobble-token))
        (value-to-slot v0 4)
        (as-to-slot v1
                    (make-sb-term 'opt
                                  (list
                                   (make-sb-term 'seq
                                                 (ds-temp-rept (ck-rept-ref v2))))))))
     ((la-match ((sbst::!id! sbst::@)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v2))
        (lam ((sbst::@ epsilon)) (gobble-token))
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
        (value-to-slot v0 1)
        (as-to-slot v1 (make-sb-term 'ext-nonterminal (list v2 v3)))))
     ((la-match ((sbst::!id! epsilon)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 0)
        (as-to-slot v1 (make-sb-term 'nonterminal (list v1)))))
     ((la-match ((sbst::!keyword!)))
      (progn
        (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v1))
        (value-to-slot v0 2)
        (as-to-slot v1 (make-sb-term 'ukeyword (list v1)))))
     (t
      (initial-error
       '((sbst::!keyword! epsilon) (sbst::!id! sbst::@ epsilon)
        (sbst::[ sbst::!id! sbst::!keyword! sbst::[ sbst::{)
        (sbst::{ sbst::!id! sbst::!keyword! sbst::[ sbst::{)))))
    (value-to-slot v0 v1)
    (cond ((>
            (cond ((member (peek-first)
                           '(sbst::jux sbst::\| sbst::* sbst::+ sbst::++
                            sbst::** sbst::^ sbst::< sbst::<<))
                   (setq mtemp (peek-first))
                   (cadr
                    (assoc (peek-first)
                           '((sbst::^ 80) (sbst::jux 70) (sbst::++ 60)
                            (sbst::** 60) (sbst::+ 60) (sbst::* 60)
                            (sbst::< 50) (sbst::<< 40) (sbst::jux 30)
                            (sbst::\| 20)))))
                  ((member (peek-first)
                           '(sbst::_ sbst::|#| sbst::/+ sbst::/- sbst::?
                            sbst::!+ sbst::!- sbst::@< sbst::@> sbst::@^))
                   (setq mtemp 'g24129)
                   70)
                  (t
                   0))
            rbp)
           (loop (clean-variables (v1 v2 v3 v4 v5 v6 v7 v8 v9 v10))
                 (case mtemp
                   (sbst::<<
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::<< epsilon)) (gobble-token))
                      (plus-parse
                       ((sbst::{ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                        (sbst::[ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                        (sbst::!keyword! epsilon sbst::jux sbst::\| sbst::*
                         sbst::+ sbst::++ sbst::** sbst::^ sbst::< sbst::<<)
                        (sbst::!id! epsilon sbst::@ sbst::jux sbst::\| sbst::*
                         sbst::+ sbst::++ sbst::** sbst::^ sbst::< sbst::<<))
                       (progn
                         (code-to-slot v6 (!pattern! 41))
                         nil) v5 v6)
                      (lam ((sbst::> epsilon)) (gobble-token))
                      (opt-parse ((sbst::|:| sbst::!id!))
                                 (progn
                                   (lam ((sbst::|:| epsilon)) (gobble-token))
                                   (doubleplus-parse ((sbst::!id! epsilon))
                                                     (progn
                                                       (lam
                                                        ((sbst::!id! epsilon))
                                                        (gobble-to-slot v9))
                                                       nil)
                                                     v8 v9 'sbst::|,|))
                                 v7)
                      (lam ((sbst::> epsilon)) (gobble-token))
                      (as-to-slot v10
                                  (make-sb-term 'upat
                                                (list
                                                 (if (= v7 0)
                                                     (make-sb-term 'uids nil)
                                                     (make-sb-term 'uids
                                                                   (ds-temp-rept
                                                                    (ck-rept-ref
                                                                     v8))))
                                                 (make-sb-term 'seq
                                                               (ds-temp-rept
                                                                (ck-rept-ref v5))))))
                      (value-to-slot v4 v10)
                      (cons-to-slot v3 v4)
                      (gen-star-parse
                       ((sbst::<< sbst::{ sbst::[ sbst::!keyword! sbst::!id!))
                       (progn
                         (lam ((sbst::<< epsilon)) (gobble-token))
                         (plus-parse
                          ((sbst::{ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                           (sbst::[ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                           (sbst::!keyword! epsilon sbst::jux sbst::\| sbst::*
                            sbst::+ sbst::++ sbst::** sbst::^ sbst::< sbst::<<)
                           (sbst::!id! epsilon sbst::@ sbst::jux sbst::\|
                            sbst::* sbst::+ sbst::++ sbst::** sbst::^ sbst::<
                            sbst::<<))
                          (progn
                            (code-to-slot v6 (!pattern! 41))
                            nil) v5 v6)
                         (lam ((sbst::> epsilon)) (gobble-token))
                         (opt-parse ((sbst::|:| sbst::!id!))
                                    (progn
                                      (lam ((sbst::|:| epsilon)) (gobble-token))
                                      (doubleplus-parse ((sbst::!id! epsilon))
                                                        (progn
                                                          (lam
                                                           ((sbst::!id! epsilon))
                                                           (gobble-to-slot v9))
                                                          nil)
                                                        v8 v9 'sbst::|,|))
                                    v7)
                         (lam ((sbst::> epsilon)) (gobble-token))
                         (as-to-slot v10
                                     (make-sb-term 'upat
                                                   (list
                                                    (if (= v7 0)
                                                        (make-sb-term 'uids nil)
                                                        (make-sb-term 'uids
                                                                      (ds-temp-rept
                                                                      (ck-rept-ref
                                                                      v8))))
                                                    (make-sb-term 'seq
                                                                  (ds-temp-rept
                                                                   (ck-rept-ref
                                                                    v5))))))
                         (value-to-slot v4 v10))
                       v3 v4)
                      (value-to-slot v0 14)
                      (as-to-slot v1
                                  (make-sb-term 'upattern
                                                (list v2
                                                      (make-sb-term 'upats
                                                                    (ds-temp-rept
                                                                     (ck-rept-ref
                                                                      v3))))))))
                   (sbst::<
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::< epsilon)) (gobble-token))
                      (code-to-slot v3 (!augment! 0))
                      (lam ((sbst::> epsilon)) (gobble-token))
                      (value-to-slot v0 13)
                      (as-to-slot v1 (make-sb-term 'augment (list v2 v3)))))
                   (g24129
                    (progn
                      (slot-to-slot v2 v0)
                      (plus-parse
                       ((sbst::@^ epsilon) (sbst::@> epsilon)
                        (sbst::@< epsilon) (sbst::!- epsilon)
                        (sbst::!+ sbst::!number! epsilon)
                        (sbst::? sbst::!id! sbst::!number!)
                        (sbst::/- sbst::!number!) (sbst::/+ sbst::!number!)
                        (sbst::|#| epsilon) (sbst::_ sbst::!number! epsilon))
                       (code-to-slot v4 (!format-command! 0)) v3 v4)
                      (value-to-slot v0 12)
                      (as-to-slot v1
                                  (make-sb-term 'format
                                                (list v2
                                                      (make-sb-term 'ws-specs
                                                                    (ds-temp-rept
                                                                     (ck-rept-ref
                                                                      v3))))))))
                   (sbst::^
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::^ epsilon)) (gobble-token))
                      (alt-parse
                       ((la-match ((sbst::!id!))) (value-to-slot v3 0)
                        (lam ((sbst::!id! epsilon)) (gobble-to-slot v5)))
                       ((la-match ((sbst::!number!))) (value-to-slot v3 1)
                        (lam ((sbst::!number! epsilon)) (gobble-to-slot v5)))
                       (t
                        (initial-error
                         '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
                      (value-to-slot v4 v5)
                      (value-to-slot v0 11)
                      (as-to-slot v1 (make-sb-term 'tag (list v2 v4)))))
                   (sbst::**
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::** epsilon)) (gobble-token))
                      (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v3))
                      (opt-parse
                       ((sbst::_ sbst::!number! epsilon sbst::@^ sbst::@>
                         sbst::@< sbst::!- sbst::!+ sbst::? sbst::/- sbst::/+
                         sbst::|#| sbst::_)
                        (sbst::|#| epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::/+ sbst::!number!) (sbst::/- sbst::!number!)
                        (sbst::? sbst::!id! sbst::!number!)
                        (sbst::!+ sbst::!number! epsilon sbst::@^ sbst::@>
                         sbst::@< sbst::!- sbst::!+ sbst::? sbst::/- sbst::/+
                         sbst::|#| sbst::_)
                        (sbst::!- epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@< epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@> epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@^ epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_))
                       (plus-parse
                        ((sbst::@^ epsilon) (sbst::@> epsilon)
                         (sbst::@< epsilon) (sbst::!- epsilon)
                         (sbst::!+ sbst::!number! epsilon)
                         (sbst::? sbst::!id! sbst::!number!)
                         (sbst::/- sbst::!number!) (sbst::/+ sbst::!number!)
                         (sbst::|#| epsilon) (sbst::_ sbst::!number! epsilon))
                        (code-to-slot v6 (!format-command! 0)) v5 v6)
                       v4)
                      (value-to-slot v0 10)
                      (as-to-slot v1
                                  (if (= v4 0)
                                      (make-sb-term 'doublestar
                                                    (list v2
                                                          (make-sb-term
                                                           'ukeyword (list v3))))
                                      (make-sb-term 'doublestar
                                                    (list v2
                                                          (make-sb-term 'format
                                                                      (list
                                                                      (make-sb-term
                                                                      'ukeyword
                                                                      (list
                                                                      v3))
                                                                      (make-sb-term
                                                                      'ws-specs
                                                                      (ds-temp-rept
                                                                      (ck-rept-ref
                                                                      v5)))))))))))
                   (sbst::++
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::++ epsilon)) (gobble-token))
                      (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v3))
                      (opt-parse
                       ((sbst::_ sbst::!number! epsilon sbst::@^ sbst::@>
                         sbst::@< sbst::!- sbst::!+ sbst::? sbst::/- sbst::/+
                         sbst::|#| sbst::_)
                        (sbst::|#| epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::/+ sbst::!number!) (sbst::/- sbst::!number!)
                        (sbst::? sbst::!id! sbst::!number!)
                        (sbst::!+ sbst::!number! epsilon sbst::@^ sbst::@>
                         sbst::@< sbst::!- sbst::!+ sbst::? sbst::/- sbst::/+
                         sbst::|#| sbst::_)
                        (sbst::!- epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@< epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@> epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_)
                        (sbst::@^ epsilon sbst::@^ sbst::@> sbst::@< sbst::!-
                         sbst::!+ sbst::? sbst::/- sbst::/+ sbst::|#| sbst::_))
                       (plus-parse
                        ((sbst::@^ epsilon) (sbst::@> epsilon)
                         (sbst::@< epsilon) (sbst::!- epsilon)
                         (sbst::!+ sbst::!number! epsilon)
                         (sbst::? sbst::!id! sbst::!number!)
                         (sbst::/- sbst::!number!) (sbst::/+ sbst::!number!)
                         (sbst::|#| epsilon) (sbst::_ sbst::!number! epsilon))
                        (code-to-slot v6 (!format-command! 0)) v5 v6)
                       v4)
                      (value-to-slot v0 9)
                      (as-to-slot v1
                                  (if (= v4 0)
                                      (make-sb-term 'doubleplus
                                                    (list v2
                                                          (make-sb-term
                                                           'ukeyword (list v3))))
                                      (make-sb-term 'doubleplus
                                                    (list v2
                                                          (make-sb-term 'format
                                                                      (list
                                                                      (make-sb-term
                                                                      'ukeyword
                                                                      (list
                                                                      v3))
                                                                      (make-sb-term
                                                                      'ws-specs
                                                                      (ds-temp-rept
                                                                      (ck-rept-ref
                                                                      v5)))))))))))
                   (sbst::+
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::+ epsilon)) (gobble-token))
                      (value-to-slot v0 8)
                      (as-to-slot v1 (make-sb-term 'plus (list v2)))))
                   (sbst::*
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::* epsilon)) (gobble-token))
                      (value-to-slot v0 7)
                      (as-to-slot v1 (make-sb-term 'star (list v2)))))
                   (sbst::\|
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::\| epsilon)) (gobble-token))
                      (doubleplus-parse
                       ((sbst::{ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                        (sbst::[ sbst::{ sbst::[ sbst::!keyword! sbst::!id!)
                        (sbst::!keyword! epsilon sbst::jux sbst::\| sbst::*
                         sbst::+ sbst::++ sbst::** sbst::^ sbst::< sbst::<<)
                        (sbst::!id! epsilon sbst::@ sbst::jux sbst::\| sbst::*
                         sbst::+ sbst::++ sbst::** sbst::^ sbst::< sbst::<<))
                       (code-to-slot v4 (!pattern! 21)) v3 v4 'sbst::\|)
                      (value-to-slot v0 6)
                      (as-to-slot v1
                                  (make-sb-term 'alt
                                                (ds-temp-rept
                                                 (mk-temp-rept
                                                  (cons v2
                                                        (ds-temp-rept
                                                         (ck-rept-ref v3)))))))))
                   (sbst::jux
                    (progn
                      (slot-to-slot v2 v0)
                      (lam ((sbst::jux epsilon)) (gobble-token))
                      (code-to-slot v3 (!pattern! 31))
                      (opt-parse
                       ((sbst::juxform sbst::_ sbst::|#| sbst::/+ sbst::/-
                         sbst::? sbst::!+ sbst::!- sbst::@< sbst::@> sbst::@^))
                       (progn
                         (lam ((sbst::juxform epsilon)) (gobble-token))
                         (plus-parse
                          ((sbst::@^ epsilon) (sbst::@> epsilon)
                           (sbst::@< epsilon) (sbst::!- epsilon)
                           (sbst::!+ sbst::!number! epsilon)
                           (sbst::? sbst::!id! sbst::!number!)
                           (sbst::/- sbst::!number!) (sbst::/+ sbst::!number!)
                           (sbst::|#| epsilon) (sbst::_ sbst::!number! epsilon))
                          (code-to-slot v6 (!format-command! 0)) v5 v6))
                       v4)
                      (value-to-slot v0 3)
                      (as-to-slot v1
                                  (make-sb-term 'jux
                                                (list v2 v3
                                                      (if (= v4 0)
                                                          (make-sb-term
                                                           'ws-specs nil)
                                                          (make-sb-term
                                                           'ws-specs
                                                           (ds-temp-rept
                                                            (ck-rept-ref v5)))))))))
                   (t
                    (medial-error
                     '(sbst::jux sbst::\| sbst::* sbst::+ sbst::++ sbst::**
                      sbst::^ sbst::_ sbst::|#| sbst::/+ sbst::/- sbst::?
                      sbst::!+ sbst::!- sbst::@< sbst::@> sbst::@^ sbst::<
                      sbst::<<))))
                 (value-to-slot v0 v1)
                 (when (<=
                   (cond ((member (peek-first)
                                  '(sbst::jux sbst::\| sbst::* sbst::+ sbst::++
                                   sbst::** sbst::^ sbst::< sbst::<<))
                          (setq mtemp (peek-first))
                          (cadr
                           (assoc (peek-first)
                                  '((sbst::^ 80) (sbst::jux 70) (sbst::++ 60)
                                   (sbst::** 60) (sbst::+ 60) (sbst::* 60)
                                   (sbst::< 50) (sbst::<< 40) (sbst::jux 30)
                                   (sbst::\| 20)))))
                         ((member (peek-first)
                                  '(sbst::_ sbst::|#| sbst::/+ sbst::/- sbst::?
                                   sbst::!+ sbst::!- sbst::@< sbst::@> sbst::@^))
                          (setq mtemp 'g24129)
                          70)
                         (t
                          0))
                   rbp)
                   (return nil)))))
    v0)) 


(defun !nonterminal-definition! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (initials-only
     ((la-match ((sbst::!id!)))
      (progn
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
        (lam ((sbst::|::=| epsilon)) (gobble-token))
        (code-to-slot v2 (!pattern! 0))
        (as-to-slot v0 (make-sb-term 'nt-def (list v1 v2)))))
     (t (initial-error '((sbst::!id! sbst::|::=|)))))
    nil
    v0)) 


(defun !single-op-precedence! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7
        v8)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7 v8)
    (initials-only
     ((la-match ((sbst::jux)))
      (progn
        (lam ((sbst::jux epsilon)) (gobble-token))
        (opt-parse ((sbst::^ sbst::!id! sbst::!number!))
                   (progn
                     (lam ((sbst::^ epsilon)) (gobble-token))
                     (alt-parse
                      ((la-match ((sbst::!id!))) (value-to-slot v4 0)
                       (lam ((sbst::!id! epsilon)) (gobble-to-slot v6)))
                      ((la-match ((sbst::!number!))) (value-to-slot v4 1)
                       (lam ((sbst::!number! epsilon)) (gobble-to-slot v6)))
                      (t
                       (initial-error
                        '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
                     (value-to-slot v5 v6))
                   v3)
        (value-to-slot v1 1)
        (as-to-slot v2
                    (case v1
                      (1
                       (make-sb-term 'jux-op
                                     (list
                                      (if (= v3 0) (make-sb-term 'null (list))
                                          v5))))
                      (0 (make-sb-term 'keyword-op (list v3)))))
        (alt-parse
         ((la-match ((sbst::medial))) (value-to-slot v7 1)
          (progn
            (lam ((sbst::medial epsilon)) (gobble-token))
            (alt-parse
             ((la-match ((sbst::left))) (value-to-slot v8 0)
              (lam ((sbst::left epsilon)) (gobble-token)))
             ((la-match ((sbst::right))) (value-to-slot v8 1)
              (lam ((sbst::right epsilon)) (gobble-token)))
             ((la-match ((sbst::lbind))) (value-to-slot v8 2)
              (lam ((sbst::lbind epsilon)) (gobble-token)))
             ((la-match ((sbst::rbind))) (value-to-slot v8 3)
              (lam ((sbst::rbind epsilon)) (gobble-token)))
             (t
              (initial-error
               '((sbst::left epsilon) (sbst::right epsilon)
                (sbst::lbind epsilon) (sbst::rbind epsilon)))))))
         ((la-match ((sbst::initial))) (value-to-slot v7 0)
          (lam ((sbst::initial epsilon)) (gobble-token)))
         ((la-match ((sbst::aggregate))) (value-to-slot v7 2)
          (lam ((sbst::aggregate epsilon)) (gobble-token)))
         (t
          (initial-error
           '((sbst::medial sbst::left sbst::right sbst::lbind sbst::rbind)
            (sbst::initial epsilon) (sbst::aggregate epsilon)))))
        (as-to-slot v0
                    (case v7
                      (2 (make-sb-term 'aggregate (list v2)))
                      (1
                       (make-sb-term 'medial
                                     (list v2
                                           (case v8
                                             (3 (mk-id 'rbind))
                                             (2 (mk-id 'lbind))
                                             (1 (mk-id 'right))
                                             (0 (mk-id 'left))))))
                      (0 (make-sb-term 'initial (list v2)))))))
     ((la-match ((sbst::!keyword!)))
      (progn
        (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v3))
        (value-to-slot v1 0)
        (as-to-slot v2
                    (case v1
                      (1
                       (make-sb-term 'jux-op
                                     (list
                                      (if (= v3 0) (make-sb-term 'null (list))
                                          v5))))
                      (0 (make-sb-term 'keyword-op (list v3)))))
        (alt-parse
         ((la-match ((sbst::medial))) (value-to-slot v7 1)
          (progn
            (lam ((sbst::medial epsilon)) (gobble-token))
            (alt-parse
             ((la-match ((sbst::left))) (value-to-slot v8 0)
              (lam ((sbst::left epsilon)) (gobble-token)))
             ((la-match ((sbst::right))) (value-to-slot v8 1)
              (lam ((sbst::right epsilon)) (gobble-token)))
             ((la-match ((sbst::lbind))) (value-to-slot v8 2)
              (lam ((sbst::lbind epsilon)) (gobble-token)))
             ((la-match ((sbst::rbind))) (value-to-slot v8 3)
              (lam ((sbst::rbind epsilon)) (gobble-token)))
             (t
              (initial-error
               '((sbst::left epsilon) (sbst::right epsilon)
                (sbst::lbind epsilon) (sbst::rbind epsilon)))))))
         ((la-match ((sbst::initial))) (value-to-slot v7 0)
          (lam ((sbst::initial epsilon)) (gobble-token)))
         ((la-match ((sbst::aggregate))) (value-to-slot v7 2)
          (lam ((sbst::aggregate epsilon)) (gobble-token)))
         (t
          (initial-error
           '((sbst::medial sbst::left sbst::right sbst::lbind sbst::rbind)
            (sbst::initial epsilon) (sbst::aggregate epsilon)))))
        (as-to-slot v0
                    (case v7
                      (2 (make-sb-term 'aggregate (list v2)))
                      (1
                       (make-sb-term 'medial
                                     (list v2
                                           (case v8
                                             (3 (mk-id 'rbind))
                                             (2 (mk-id 'lbind))
                                             (1 (mk-id 'right))
                                             (0 (mk-id 'left))))))
                      (0 (make-sb-term 'initial (list v2)))))))
     (t
      (initial-error
       '((sbst::!keyword! sbst::initial sbst::medial sbst::aggregate)
        (sbst::jux sbst::aggregate sbst::medial sbst::initial sbst::^)))))
    nil
    v0)) 


(defun !single-level! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (initials-only
     ((la-match ((sbst::!keyword!) (sbst::jux)))
      (progn
        (code-to-slot v1 (!single-op-precedence! 0))
        (cons-to-slot v0 v1)
        (gen-star-parse ((sbst::|,| sbst::!keyword! sbst::jux))
                        (progn
                          (lam ((sbst::|,| epsilon)) (gobble-token))
                          (code-to-slot v1 (!single-op-precedence! 0)))
                        v0 v1)
        (as-to-slot v0
                    (make-sb-term 'prec-level (ds-temp-rept (ck-rept-ref v0))))))
     (t
      (initial-error
       '((sbst::jux sbst::^ sbst::initial sbst::medial sbst::aggregate)
        (sbst::!keyword! sbst::initial sbst::medial sbst::aggregate)))))
    nil
    v0)) 


(defun !multiple-levels! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (initials-only
     ((la-match ((sbst::!keyword!) (sbst::jux)))
      (progn
        (code-to-slot v1 (!single-level! 0))
        (cons-to-slot v0 v1)
        (gen-star-parse
         ((sbst::!keyword! sbst::initial sbst::medial sbst::aggregate
           sbst::initial sbst::medial sbst::aggregate)
          (sbst::jux sbst::^ sbst::initial sbst::medial sbst::aggregate sbst::^
           sbst::initial sbst::medial sbst::aggregate))
         (code-to-slot v1 (!single-level! 0)) v0 v1)
        (as-to-slot v0
                    (make-sb-term 'prec-levels (ds-temp-rept (ck-rept-ref v0))))))
     (t
      (initial-error
       '((sbst::jux sbst::^ sbst::initial sbst::medial sbst::aggregate sbst::^
          sbst::initial sbst::medial sbst::aggregate)
        (sbst::!keyword! sbst::initial sbst::medial sbst::aggregate
         sbst::initial sbst::medial sbst::aggregate)))))
    nil
    v0)) 


(defun !precedence-information! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6)
    (initials-only
     ((la-match ((sbst::precedence)))
      (progn
        (lam ((sbst::precedence epsilon)) (gobble-token))
        (opt-parse ((sbst::information epsilon))
                   (lam ((sbst::information epsilon)) (gobble-token)) v1)
        (plus-parse ((sbst::!id! sbst::!keyword! sbst::jux))
                    (progn
                      (lam ((sbst::!id! epsilon)) (gobble-to-slot v4))
                      (code-to-slot v5 (!multiple-levels! 0))
                      (as-to-slot v6 (make-sb-term 'prec-entry (list v4 v5)))
                      (value-to-slot v3 v6))
                    v2 v3)
        (as-to-slot v0
                    (make-sb-term 'prec-entries (ds-temp-rept (ck-rept-ref v2))))))
     (t (initial-error '((sbst::precedence sbst::!id! sbst::information)))))
    nil
    v0)) 


(defun !spacing-information! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10
        v11
        v12
        v13
        v14
        v15
        v16)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
                           v15 v16)
    (initials-only
     ((la-match ((sbst::spacing)))
      (progn
        (lam ((sbst::spacing epsilon)) (gobble-token))
        (opt-parse ((sbst::information epsilon))
                   (lam ((sbst::information epsilon)) (gobble-token)) v1)
        (plus-parse
         ((sbst::jux sbst::^ sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_)
          (sbst::arb sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_)
          (sbst::lt sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_)
          (sbst::op sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_)
          (sbst::!id! sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_)
          (sbst::!keyword! sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
           sbst::!keyword! sbst::@^ sbst::@> sbst::@< sbst::!- sbst::!+ sbst::?
           sbst::/- sbst::/+ sbst::|#| sbst::_))
         (progn
           (alt-parse
            ((la-match ((sbst::!keyword!))) (value-to-slot v4 0)
             (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v5)))
            ((la-match ((sbst::!id!))) (value-to-slot v4 1)
             (lam ((sbst::!id! epsilon)) (gobble-to-slot v5)))
            ((la-match ((sbst::op))) (value-to-slot v4 2)
             (lam ((sbst::op epsilon)) (gobble-token)))
            ((la-match ((sbst::lt))) (value-to-slot v4 3)
             (lam ((sbst::lt epsilon)) (gobble-token)))
            ((la-match ((sbst::arb))) (value-to-slot v4 4)
             (lam ((sbst::arb epsilon)) (gobble-token)))
            ((la-match ((sbst::jux))) (value-to-slot v4 5)
             (progn
               (lam ((sbst::jux epsilon)) (gobble-token))
               (opt-parse ((sbst::^ sbst::!id! sbst::!number!))
                          (progn
                            (lam ((sbst::^ epsilon)) (gobble-token))
                            (alt-parse
                             ((la-match ((sbst::!id!))) (value-to-slot v6 0)
                              (lam ((sbst::!id! epsilon)) (gobble-to-slot v8)))
                             ((la-match ((sbst::!number!)))
                              (value-to-slot v6 1)
                              (lam ((sbst::!number! epsilon))
                                   (gobble-to-slot v8)))
                             (t
                              (initial-error
                               '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
                            (value-to-slot v7 v8))
                          v5)))
            (t
             (initial-error
              '((sbst::!keyword! epsilon) (sbst::!id! epsilon)
               (sbst::op epsilon) (sbst::lt epsilon) (sbst::arb epsilon)
               (sbst::jux epsilon sbst::^)))))
           (star-parse
            ((sbst::@^ epsilon) (sbst::@> epsilon) (sbst::@< epsilon)
             (sbst::!- epsilon) (sbst::!+ sbst::!number! epsilon)
             (sbst::? sbst::!id! sbst::!number!) (sbst::/- sbst::!number!)
             (sbst::/+ sbst::!number!) (sbst::|#| epsilon)
             (sbst::_ sbst::!number! epsilon))
            (code-to-slot v10 (!format-command! 0)) v9 v10)
           (alt-parse
            ((la-match ((sbst::!keyword!))) (value-to-slot v11 0)
             (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v12)))
            ((la-match ((sbst::!id!))) (value-to-slot v11 1)
             (lam ((sbst::!id! epsilon)) (gobble-to-slot v12)))
            ((la-match ((sbst::op))) (value-to-slot v11 2)
             (lam ((sbst::op epsilon)) (gobble-token)))
            ((la-match ((sbst::lt))) (value-to-slot v11 3)
             (lam ((sbst::lt epsilon)) (gobble-token)))
            ((la-match ((sbst::arb))) (value-to-slot v11 4)
             (lam ((sbst::arb epsilon)) (gobble-token)))
            ((la-match ((sbst::jux))) (value-to-slot v11 5)
             (progn
               (lam ((sbst::jux epsilon)) (gobble-token))
               (opt-parse ((sbst::^ sbst::!id! sbst::!number!))
                          (progn
                            (lam ((sbst::^ epsilon)) (gobble-token))
                            (alt-parse
                             ((la-match ((sbst::!id!))) (value-to-slot v13 0)
                              (lam ((sbst::!id! epsilon)) (gobble-to-slot v15)))
                             ((la-match ((sbst::!number!)))
                              (value-to-slot v13 1)
                              (lam ((sbst::!number! epsilon))
                                   (gobble-to-slot v15)))
                             (t
                              (initial-error
                               '((sbst::!id! epsilon) (sbst::!number! epsilon)))))
                            (value-to-slot v14 v15))
                          v12)))
            (t
             (initial-error
              '((sbst::!keyword! epsilon) (sbst::!id! epsilon)
               (sbst::op epsilon) (sbst::lt epsilon) (sbst::arb epsilon)
               (sbst::jux epsilon sbst::^)))))
           (as-to-slot v16
                       (make-sb-term 'space-com
                                     (list
                                      (case v4
                                        ((1 0) v5)
                                        (5
                                         (make-sb-term 'jux-op
                                                       (list
                                                        (if (= v5 0)
                                                            (make-sb-term 'null
                                                                      (list))
                                                            v7))))
                                        (4 (mk-id 'arb))
                                        (3 (mk-id 'lt))
                                        (2 (mk-id 'op)))
                                      (make-sb-term 'ws-specs
                                                    (ds-temp-rept
                                                     (ck-rept-ref v9)))
                                      (case v11
                                        ((1 0) v12)
                                        (5
                                         (make-sb-term 'jux-op
                                                       (list
                                                        (if (= v12 0)
                                                            (make-sb-term 'null
                                                                      (list))
                                                            v14))))
                                        (4 (mk-id 'arb))
                                        (3 (mk-id 'lt))
                                        (2 (mk-id 'op))))))
           (value-to-slot v3 v16))
         v2 v3)
        (as-to-slot v0 (make-sb-term 'spacing (ds-temp-rept (ck-rept-ref v2))))))
     (t
      (initial-error
       '((sbst::spacing sbst::jux sbst::arb sbst::lt sbst::op sbst::!id!
          sbst::!keyword! sbst::information)))))
    nil
    v0)) 


(defun !bracketing-information! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7)
    (initials-only
     ((la-match ((sbst::bracketing)))
      (progn
        (lam ((sbst::bracketing epsilon)) (gobble-token))
        (opt-parse ((sbst::information epsilon))
                   (lam ((sbst::information epsilon)) (gobble-token)) v1)
        (plus-parse ((sbst::!id! sbst::!keyword!))
                    (progn
                      (lam ((sbst::!id! epsilon)) (gobble-to-slot v4))
                      (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v5))
                      (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v6))
                      (as-to-slot v7
                                  (make-sb-term 'bracket-entry (list v4 v5 v6)))
                      (value-to-slot v3 v7))
                    v2 v3)
        (as-to-slot v0
                    (make-sb-term 'bracket-entries
                                  (ds-temp-rept (ck-rept-ref v2))))))
     (t (initial-error '((sbst::bracketing sbst::!id! sbst::information)))))
    nil
    v0)) 


(defun !lexical-terminals! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6)
    (initials-only
     ((la-match ((sbst::lexical)))
      (progn
        (lam ((sbst::lexical epsilon)) (gobble-token))
        (lam ((sbst::terminals epsilon)) (gobble-token))
        (doubleplus-parse ((sbst::!id! epsilon sbst::!keyword!))
                          (progn
                            (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
                            (opt-parse ((sbst::!keyword! epsilon))
                                       (lam ((sbst::!keyword! epsilon))
                                            (gobble-to-slot v5))
                                       v4)
                            (as-to-slot v6
                                        (if (= v4 0) v3
                                            (make-sb-term 'delimiter
                                                          (list v3 v5))))
                            (value-to-slot v2 v6))
                          v1 v2 'sbst::|,|)
        (as-to-slot v0
                    (make-sb-term 'lex-terms (ds-temp-rept (ck-rept-ref v1))))))
     (t (initial-error '((sbst::lexical sbst::terminals)))))
    nil
    v0)) 


(defun !operator-information! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (initials-only
     ((la-match ((sbst::operators)))
      (progn
        (lam ((sbst::operators epsilon)) (gobble-token))
        (star-parse ((sbst::!keyword! epsilon))
                    (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v2)) v1 v2)
        (as-to-slot v0 (make-sb-term 'op-info (ds-temp-rept (ck-rept-ref v1))))))
     (t (initial-error '((sbst::operators epsilon sbst::!keyword!)))))
    nil
    v0)) 


(defun !escape-character! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (initials-only
     ((la-match ((sbst::escape)))
      (progn
        (lam ((sbst::escape epsilon)) (gobble-token))
        (lam ((sbst::character epsilon)) (gobble-token))
        (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v1))
        (as-to-slot v0 (make-sb-term 'escape-character (list v1)))))
     (t (initial-error '((sbst::escape sbst::character)))))
    nil
    v0)) 


(defun !comment-character! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5)
    (initials-only
     ((la-match ((sbst::comment)))
      (progn
        (lam ((sbst::comment epsilon)) (gobble-token))
        (lam ((sbst::character epsilon)) (gobble-token))
        (opt-parse ((sbst::newline sbst::!keyword!))
                   (progn
                     (lam ((sbst::newline epsilon)) (gobble-token))
                     (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v2)))
                   v1)
        (opt-parse ((sbst::delimited sbst::!keyword!))
                   (progn
                     (lam ((sbst::delimited epsilon)) (gobble-token))
                     (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v4))
                     (lam ((sbst::!keyword! epsilon)) (gobble-to-slot v5)))
                   v3)
        (as-to-slot v0
                    (make-sb-term 'comment-character
                                  (list
                                   (if (= v1 0) (make-sb-term 'null (list)) v2)
                                   (if (= v3 0) (make-sb-term 'null (list)) v4)
                                   (if (= v3 0) (make-sb-term 'null (list)) v5))))))
     (t (initial-error '((sbst::comment sbst::character)))))
    nil
    v0)) 


(defun !external-grammars! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (initials-only
     ((la-match ((sbst::external)))
      (progn
        (lam ((sbst::external epsilon)) (gobble-token))
        (lam ((sbst::grammars epsilon)) (gobble-token))
        (doubleplus-parse ((sbst::!id! epsilon))
                          (lam ((sbst::!id! epsilon)) (gobble-to-slot v2)) v1
                          v2 'sbst::|,|)
        (as-to-slot v0 (make-sb-term 'ext-gram (ds-temp-rept (ck-rept-ref v1))))))
     (t (initial-error '((sbst::external sbst::grammars)))))
    nil
    v0)) 


(defun !meta-grammar! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10
        v11
        v12
        v13
        v14
        v15
        v16
        v17
        v18
        v19
        v20
        v21
        v22
        v23
        v24
        v25
        v26
        v27
        v28
        v29
        v30
        v31
        v32)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14
                           v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27
                           v28 v29 v30 v31 v32)
    (initials-only
     ((la-match ((sbst::grammar)))
      (progn
        (lam ((sbst::grammar epsilon)) (gobble-token))
        (lam ((sbst::!id! epsilon)) (gobble-to-slot v4))
        (value-to-slot v1 1)
        (value-to-slot v3 v4)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (opt-parse ((sbst::external sbst::grammars))
                   (code-to-slot v7 (!external-grammars! 0)) v5)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (opt-parse ((sbst::case sbst::sensitive))
                   (progn
                     (lam ((sbst::case epsilon)) (gobble-token))
                     (lam ((sbst::sensitive epsilon)) (gobble-token)))
                   v8)
        (opt-parse ((sbst::comment sbst::character))
                   (code-to-slot v11 (!comment-character! 0)) v9)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (opt-parse ((sbst::escape sbst::character))
                   (code-to-slot v14 (!escape-character! 0)) v12)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (opt-parse ((sbst::operators sbst::!keyword!))
                   (code-to-slot v17 (!operator-information! 0)) v15)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::external)))
      (progn
        (code-to-slot v7 (!external-grammars! 0))
        (value-to-slot v5 1)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (opt-parse ((sbst::case sbst::sensitive))
                   (progn
                     (lam ((sbst::case epsilon)) (gobble-token))
                     (lam ((sbst::sensitive epsilon)) (gobble-token)))
                   v8)
        (opt-parse ((sbst::comment sbst::character))
                   (code-to-slot v11 (!comment-character! 0)) v9)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (opt-parse ((sbst::escape sbst::character))
                   (code-to-slot v14 (!escape-character! 0)) v12)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (opt-parse ((sbst::operators sbst::!keyword!))
                   (code-to-slot v17 (!operator-information! 0)) v15)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::case)))
      (progn
        (lam ((sbst::case epsilon)) (gobble-token))
        (lam ((sbst::sensitive epsilon)) (gobble-token))
        (value-to-slot v8 1)
        (opt-parse ((sbst::comment sbst::character))
                   (code-to-slot v11 (!comment-character! 0)) v9)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (opt-parse ((sbst::escape sbst::character))
                   (code-to-slot v14 (!escape-character! 0)) v12)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (opt-parse ((sbst::operators sbst::!keyword!))
                   (code-to-slot v17 (!operator-information! 0)) v15)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::comment)))
      (progn
        (code-to-slot v11 (!comment-character! 0))
        (value-to-slot v9 1)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (opt-parse ((sbst::escape sbst::character))
                   (code-to-slot v14 (!escape-character! 0)) v12)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (opt-parse ((sbst::operators sbst::!keyword!))
                   (code-to-slot v17 (!operator-information! 0)) v15)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::escape)))
      (progn
        (code-to-slot v14 (!escape-character! 0))
        (value-to-slot v12 1)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (opt-parse ((sbst::operators sbst::!keyword!))
                   (code-to-slot v17 (!operator-information! 0)) v15)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::operators)))
      (progn
        (code-to-slot v17 (!operator-information! 0))
        (value-to-slot v15 1)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (opt-parse ((sbst::lexical sbst::terminals))
                   (code-to-slot v20 (!lexical-terminals! 0)) v18)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::lexical)))
      (progn
        (code-to-slot v20 (!lexical-terminals! 0))
        (value-to-slot v18 1)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (opt-parse ((sbst::bracketing sbst::information sbst::!id!))
                   (code-to-slot v23 (!bracketing-information! 0)) v21)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v15 0)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::bracketing)))
      (progn
        (code-to-slot v23 (!bracketing-information! 0))
        (value-to-slot v21 1)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (opt-parse ((sbst::precedence sbst::information sbst::!id!))
                   (code-to-slot v26 (!precedence-information! 0)) v24)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v18 0)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (value-to-slot v15 0)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::precedence)))
      (progn
        (code-to-slot v26 (!precedence-information! 0))
        (value-to-slot v24 1)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (opt-parse
         ((sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
           sbst::lt sbst::arb sbst::jux))
         (code-to-slot v29 (!spacing-information! 0)) v27)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v21 0)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (value-to-slot v18 0)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (value-to-slot v15 0)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::spacing)))
      (progn
        (code-to-slot v29 (!spacing-information! 0))
        (value-to-slot v27 1)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (plus-parse ((sbst::!id! sbst::|::=|))
                    (progn
                      (code-to-slot v32 (!nonterminal-definition! 0))
                      (lam ((sbst::|;| epsilon)) (gobble-token))
                      (value-to-slot v31 v32))
                    v30 v31)
        (value-to-slot v24 0)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (value-to-slot v21 0)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (value-to-slot v18 0)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (value-to-slot v15 0)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     ((la-match ((sbst::!id!)))
      (progn
        (code-to-slot v32 (!nonterminal-definition! 0))
        (lam ((sbst::|;| epsilon)) (gobble-token))
        (value-to-slot v31 v32)
        (cons-to-slot v30 v31)
        (gen-star-parse ((sbst::!id! sbst::|::=|))
                        (progn
                          (code-to-slot v32 (!nonterminal-definition! 0))
                          (lam ((sbst::|;| epsilon)) (gobble-token))
                          (value-to-slot v31 v32))
                        v30 v31)
        (value-to-slot v27 0)
        (as-to-slot v28
                    (if (= v27 0) (make-sb-term 'meta-grammar-null-9 nil) v29))
        (value-to-slot v24 0)
        (as-to-slot v25
                    (if (= v24 0) (make-sb-term 'meta-grammar-null-8 nil) v26))
        (value-to-slot v21 0)
        (as-to-slot v22
                    (if (= v21 0) (make-sb-term 'meta-grammar-null-7 nil) v23))
        (value-to-slot v18 0)
        (as-to-slot v19
                    (if (= v18 0) (make-sb-term 'meta-grammar-null-6 nil) v20))
        (value-to-slot v15 0)
        (as-to-slot v16
                    (if (= v15 0) (make-sb-term 'meta-grammar-null-5 nil) v17))
        (value-to-slot v12 0)
        (as-to-slot v13
                    (if (= v12 0) (make-sb-term 'meta-grammar-null-4 nil) v14))
        (value-to-slot v9 0)
        (as-to-slot v10
                    (if (= v9 0) (make-sb-term 'meta-grammar-null-3 nil) v11))
        (value-to-slot v8 0)
        (value-to-slot v5 0)
        (as-to-slot v6 (if (= v5 0) (make-sb-term 'meta-grammar-null-2 nil) v7))
        (value-to-slot v1 0)
        (as-to-slot v2 (if (= v1 0) (make-sb-term 'meta-grammar-null-1 nil) v3))
        (as-to-slot v0
                    (make-sb-term 'grammar
                                  (list v2 v6 v10 v16 v19 v22 v25 v28
                                        (make-sb-term 'nts
                                                      (ds-temp-rept
                                                       (ck-rept-ref v30)))
                                        (if (= v8 0) (mk-id 'nocase)
                                            (mk-id 'case))
                                        v13)))))
     (t
      (initial-error
       '((sbst::!id! sbst::|::=|)
        (sbst::spacing sbst::information sbst::!keyword! sbst::!id! sbst::op
         sbst::lt sbst::arb sbst::jux)
        (sbst::precedence sbst::information sbst::!id!)
        (sbst::bracketing sbst::information sbst::!id!)
        (sbst::lexical sbst::terminals) (sbst::operators sbst::!keyword!)
        (sbst::escape sbst::character) (sbst::comment sbst::character)
        (sbst::case sbst::sensitive) (sbst::external sbst::grammars)
        (sbst::grammar sbst::!id!)))))
    nil
    v0)) 

