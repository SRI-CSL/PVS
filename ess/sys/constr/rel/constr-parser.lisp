;;; -*- Mode: Lisp; Package: :constrg -*-
(in-package :constr-term-rep)  ;; creates package for abstract syntax. 

(in-package :constrg)  ;; enters package for generated code.  

(use-package '(:newattr :lang :sb-runtime :sort :term :occ :oper :ergolisp))


(export '( constr-parse ))

(defparameter constr-abs-syn-package (find-package :constr-term-rep))

(defun constr-parse
    (&key (nt 'constr-term-rep::items) error-threshold ask-about-bad-tokens
     (return-errors nil) (stream nil streamp) string file
     (exhaust-stream nil))
  (cond (stream)
        (string (setf stream (make-string-input-stream string)))
        (file (setf stream (open file)))
        (t
         (error "Must provide an input means -- either :stream, ~
			:string, or :file.")))
  (setq *constr-keyword-table* (init-keyword-table constr-keyword-list))
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
        (*abs-syn-package* constr-abs-syn-package)
        (*reader-fun* #'reader)
        (*apply-lex-term-constr-fun* #'apply-lexical-terminal-constructor)
        (*keyword-table* *constr-keyword-table*)
        (*close-comment-char* constr-close-comment-char)
        (*case-sensitive* constr-case-sensitive))
    (init-lexer *lexical-stream*)
    (multiple-value-bind (abs-syntax error-string error-args error-place)
        (unwind-protect
            (catch 'parser-abort-catch
              (prog1 (case nt
                       (constr-term-rep::items (!items! 0))
                       (constr-term-rep::item (!item! 0))
                       (constr-term-rep::abbrev (!abbrev! 0))
                       (constr-term-rep::datatype (!datatype! 0))
                       (constr-term-rep::dtarm (!dtarm! 0))
                       (constr-term-rep::part (!part! 0))
                       (constr-term-rep::ptype (!ptype! 0))
                       (t (error "Unknown nonterminal ~A." nt)))
                     (when (or file string exhaust-stream)
                       (clet* (((token ignore place) (peek-first)))
                              (unless (or (eq token :eof) (eq token 'eof))
                                (do-syntax-error "There is garbage at the end of your ~
			      file or string:~%~A" place))))))
          (unless streamp (close stream)))
      (if return-errors
          (values abs-syntax *parser-error* error-place error-string
                  error-args)
        (values abs-syntax *parser-error*)))))


(defun !ptype! (rbp &optional (bracket-list (empty-bracket-list)))
  (let (mtemp v0 v1 v2 v3)
    (ergo-ignore-if-unused v0 v1 v2 v3)
    (stack-brackets sbst::|(| ptype)
    (initials ((la-match ((sbst::|'|)))
               (progn (lam ((sbst::|'| epsilon)) (gobble-token))
                      (lam ((sbst::!id! epsilon)) (gobble-to-slot v2))
                      (value-to-slot v0 11)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-tv
                                                (list v2)))))
              ((la-match ((sbst::{ sbst::})))
               (progn (lam ((sbst::{ epsilon)) (gobble-token))
                      (lam ((sbst::} epsilon)) (gobble-token))
                      (value-to-slot v0 9)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-members
                                                (ds-temp-rept
                                                 (ck-rept-ref v2))))))
              ((la-match ((sbst::{ sbst::not sbst::{ sbst::unit sbst::|'|
                           sbst::!id! sbst::|(|)))
               (progn (lam ((sbst::{ epsilon)) (gobble-token))
                      (code-to-slot v3 (!ptype! 0)) (cons-to-slot v2 v3)
                      (gen-star-parse ((sbst::|,|
                                        sbst::|(|
                                        sbst::!id!
                                        sbst::|'|
                                        sbst::unit
                                        sbst::{
                                        sbst::not))
                                      (progn
                                       (lam
                                        ((sbst::|,| epsilon))
                                        (gobble-token))
                                       (code-to-slot v3 (!ptype! 0)))
                                      v2 v3)
                      (lam ((sbst::} epsilon)) (gobble-token))
                      (value-to-slot v0 9)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-members
                                                (ds-temp-rept
                                                 (ck-rept-ref v2))))))
              ((la-match ((sbst::not)))
               (progn (lam ((sbst::not epsilon)) (gobble-token))
                      (code-to-slot v2 (!ptype! 50)) (value-to-slot v0 6)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-appl
                                                (list
                                                 (make-sb-term
                                                  'constr-term-rep::ptype-con
                                                  (list (mk-id 'not)))
                                                 v2)))))
              ((la-match ((sbst::!id!)))
               (progn (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
                      (value-to-slot v0 12)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-con
                                                (list v1)))))
              ((la-match ((sbst::unit)))
               (progn (lam ((sbst::unit epsilon)) (gobble-token))
                      (value-to-slot v0 10)
                      (as-to-slot v1
                                  (make-sb-term 'constr-term-rep::ptype-con
                                                (list
                                                 (mk-id
                                                  'constr-term-rep::unit))))))
              (t
               (initial-error '((sbst::not sbst::|(| sbst::!id! sbst::|'|
                                 sbst::unit sbst::{ sbst::not)
                                (sbst::{ sbst::not sbst::{ sbst::unit
                                 sbst::|'| sbst::!id! sbst::|(| sbst::})
                                (sbst::unit epsilon) (sbst::|'| sbst::!id!)
                                (sbst::!id! epsilon)
                                (sbst::|(| sbst::|(| sbst::!id! sbst::|'|
                                 sbst::unit sbst::{ sbst::not)))))
    (value-to-slot v0 v1)
    (eat-brackets (sbst::|)| sbst::|(|) ptype)
    (cond ((> (cond ((member (peek-first)
                             '(sbst::* sbst::? sbst::-> sbst::|#| sbst::and
                               sbst::or sbst::sats sbst::not-sats))
                     (setq mtemp (peek-first))
                     (cadr (assoc (peek-first)
                                  '((sbst::not-sats 80) (sbst::sats 80)
                                    (sbst::? 70) (sbst::* 70) (sbst::jux 60)
                                    (sbst::and 41) (sbst::or 31)
                                    (sbst::|#| 21) (sbst::-> 11)))))
                    ((member (peek-first)
                             '(sbst::|(| sbst::!id! sbst::|'| sbst::unit
                               sbst::{ sbst::not))
                     (setq mtemp 'g45180)
                     60)
                    (t 0))
              rbp)
           (loop (clean-variables (v1 v2 v3))
                 (case mtemp
                   (g45180
                    (progn (slot-to-slot v2 v0) (code-to-slot v3 (!ptype! 61))
                           (value-to-slot v0 13)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list v2 v3)))))
                   (sbst::not-sats
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::not-sats epsilon)) (gobble-token))
                           (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
                           (value-to-slot v0 8)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'and)))
                                           v2))
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'not)))
                                           (make-sb-term
                                            'constr-term-rep::ptype-appl
                                            (list
                                             (make-sb-term
                                              'constr-term-rep::ptype-con
                                              (list (mk-id 'satisfies)))
                                             (make-sb-term
                                              'constr-term-rep::ptype-con
                                              (list v3)))))))))))
                   (sbst::sats
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::sats epsilon)) (gobble-token))
                           (lam ((sbst::!id! epsilon)) (gobble-to-slot v3))
                           (value-to-slot v0 7)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'and)))
                                           v2))
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'satisfies)))
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list v3)))))))))
                   (sbst::or
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::or epsilon)) (gobble-token))
                           (code-to-slot v3 (!ptype! 30)) (value-to-slot v0 5)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'or)))
                                           v2))
                                         v3)))))
                   (sbst::and
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::and epsilon)) (gobble-token))
                           (code-to-slot v3 (!ptype! 40)) (value-to-slot v0 4)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list (mk-id 'and)))
                                           v2))
                                         v3)))))
                   (sbst::|#|
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::|#| epsilon)) (gobble-token))
                           (code-to-slot v3 (!ptype! 20)) (value-to-slot v0 3)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list
                                             (mk-id 'constr-term-rep::prod)))
                                           v2))
                                         v3)))))
                   (sbst::->
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::-> epsilon)) (gobble-token))
                           (code-to-slot v3 (!ptype! 10)) (value-to-slot v0 2)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-appl
                                          (list
                                           (make-sb-term
                                            'constr-term-rep::ptype-con
                                            (list
                                             (mk-id 'constr-term-rep::arrow)))
                                           v2))
                                         v3)))))
                   (sbst::?
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::? epsilon)) (gobble-token))
                           (value-to-slot v0 1)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-con
                                          (list (mk-id 'optional)))
                                         v2)))))
                   (sbst::*
                    (progn (slot-to-slot v2 v0)
                           (lam ((sbst::* epsilon)) (gobble-token))
                           (value-to-slot v0 0)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::ptype-appl
                                        (list
                                         (make-sb-term
                                          'constr-term-rep::ptype-con
                                          (list (mk-id 'list-of)))
                                         v2)))))
                   (t
                    (medial-error '(sbst::* sbst::? sbst::-> sbst::|#|
                                    sbst::and sbst::or sbst::sats
                                    sbst::not-sats sbst::|(| sbst::!id!
                                    sbst::|'| sbst::unit sbst::{ sbst::not))))
                 (value-to-slot v0 v1)
                 (eat-brackets (sbst::|)| sbst::|(|) ptype)
                 (when (<= (cond ((member (peek-first)
                                          '(sbst::*
                                            sbst::?
                                            sbst::->
                                            sbst::|#|
                                            sbst::and
                                            sbst::or
                                            sbst::sats
                                            sbst::not-sats))
                                  (setq mtemp (peek-first))
                                  (cadr (assoc

                                         (peek-first)
                                         '((sbst::not-sats 80)
                                           (sbst::sats 80)
                                           (sbst::? 70)
                                           (sbst::* 70)
                                           (sbst::jux 60)
                                           (sbst::and 41)
                                           (sbst::or 31)
                                           (sbst::|#| 21)
                                           (sbst::-> 11)))))
                                 ((member (peek-first)
                                          '(sbst::|(|
                                            sbst::!id!
                                            sbst::|'|
                                            sbst::unit
                                            sbst::{
                                            sbst::not))
                                  (setq mtemp 'g45180)
                                  60)
                                 (t 0))
                           rbp)
                   (return nil)))))
    v0))


(defun !part! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1 v2 v3)
    (ergo-ignore-if-unused v0 v1 v2 v3)
    (initials-only ((la-match ((sbst::!id! sbst::|:|)))
                    (progn (lam ((sbst::!id! epsilon)) (gobble-to-slot v2))
                           (lam ((sbst::|:| epsilon)) (gobble-token))
                           (value-to-slot v1 1) (code-to-slot v3 (!ptype! 0))
                           (as-to-slot v0
                                       (make-sb-term
                                        'constr-term-rep::part
                                        (list
                                         (if (= v1 0) (mk-id 'nil) v2)
                                         v3)))))
                   ((la-match ((sbst::not) (sbst::{) (sbst::unit) (sbst::|'|)
                               (sbst::!id! epsilon sbst::* sbst::? sbst::->
                                sbst::|#| sbst::and sbst::or sbst::sats
                                sbst::not-sats sbst::|(| sbst::!id! sbst::|'|
                                sbst::unit sbst::{ sbst::not)
                               (sbst::|(|)))
                    (progn (code-to-slot v3 (!ptype! 0)) (value-to-slot v1 0)
                           (as-to-slot v0
                                       (make-sb-term
                                        'constr-term-rep::part
                                        (list
                                         (if (= v1 0) (mk-id 'nil) v2)
                                         v3)))))
                   (t
                    (initial-error '((sbst::!id! sbst::|:| epsilon sbst::*
                                      sbst::? sbst::-> sbst::|#| sbst::and
                                      sbst::or sbst::sats sbst::not-sats
                                      sbst::|(| sbst::!id! sbst::|'|
                                      sbst::unit sbst::{ sbst::not)
                                     (sbst::|(| sbst::|(| sbst::!id! sbst::|'|
                                      sbst::unit sbst::{ sbst::not)
                                     (sbst::|'| sbst::!id!)
                                     (sbst::unit epsilon sbst::* sbst::?
                                      sbst::-> sbst::|#| sbst::and sbst::or
                                      sbst::sats sbst::not-sats sbst::|(|
                                      sbst::!id! sbst::|'| sbst::unit sbst::{
                                      sbst::not)
                                     (sbst::{ sbst::} sbst::|(| sbst::!id!
                                      sbst::|'| sbst::unit sbst::{ sbst::not)
                                     (sbst::not sbst::|(| sbst::!id! sbst::|'|
                                      sbst::unit sbst::{ sbst::not)))))
    nil
    v0))


(defun !dtarm! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1 v2 v3 v4)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4)
    (initials-only ((la-match ((sbst::!id!)))
                    (progn (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
                           (opt-parse ((sbst::of
                                        epsilon
                                        sbst::!id!
                                        sbst::not
                                        sbst::{
                                        sbst::unit
                                        sbst::|'|
                                        sbst::|(|))
                                      (progn
                                       (lam
                                        ((sbst::of epsilon))
                                        (gobble-token))
                                       (doublestar-parse
                                        ((sbst::|(|
                                          sbst::|(|
                                          sbst::!id!
                                          sbst::|'|
                                          sbst::unit
                                          sbst::{
                                          sbst::not)
                                         (sbst::|'| sbst::!id!)
                                         (sbst::unit
                                          epsilon
                                          sbst::*
                                          sbst::?
                                          sbst::->
                                          sbst::|#|
                                          sbst::and
                                          sbst::or
                                          sbst::sats
                                          sbst::not-sats
                                          sbst::|(|
                                          sbst::!id!
                                          sbst::|'|
                                          sbst::unit
                                          sbst::{
                                          sbst::not)
                                         (sbst::{
                                          sbst::}
                                          sbst::|(|
                                          sbst::!id!
                                          sbst::|'|
                                          sbst::unit
                                          sbst::{
                                          sbst::not)
                                         (sbst::not
                                          sbst::|(|
                                          sbst::!id!
                                          sbst::|'|
                                          sbst::unit
                                          sbst::{
                                          sbst::not)
                                         (sbst::!id!
                                          sbst::|:|
                                          epsilon
                                          sbst::*
                                          sbst::?
                                          sbst::->
                                          sbst::|#|
                                          sbst::and
                                          sbst::or
                                          sbst::sats
                                          sbst::not-sats
                                          sbst::|(|
                                          sbst::!id!
                                          sbst::|'|
                                          sbst::unit
                                          sbst::{
                                          sbst::not))
                                        (progn
                                         (code-to-slot v4 (!part! 0))
                                         nil)
                                        v3
                                        v4
                                        'sbst::|,|))
                                      v2)
                           (as-to-slot v0
                                       (make-sb-term
                                        'constr-term-rep::dtarm
                                        (list
                                         v1
                                         (if
                                          (= v2 0)
                                          (make-sb-term
                                           'constr-term-rep::parts
                                           nil)
                                          (make-sb-term
                                           'constr-term-rep::parts
                                           (ds-temp-rept
                                            (ck-rept-ref v3)))))))))
                   (t (initial-error '((sbst::!id! epsilon sbst::of)))))
    nil
    v0))


(defun !datatype! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1 v2 v3 v4)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4)
    (initials-only ((la-match ((sbst::datatype)))
                    (progn (lam ((sbst::datatype epsilon)) (gobble-token))
                           (alt-parse ((la-match ((sbst::!id! sbst::=)))
                                       (value-to-slot v1 0)
                                       (progn
                                        (lam
                                         ((sbst::!id! epsilon))
                                         (gobble-to-slot v2))
                                        (lam
                                         ((sbst::= epsilon))
                                         (gobble-token))
                                        (doublestar-parse
                                         ((sbst::!id! sbst::of epsilon))
                                         (progn
                                          (code-to-slot v4 (!dtarm! 0))
                                          nil)
                                         v3
                                         v4
                                         'sbst::|\||)))
                                      ((la-match
                                        ((sbst::!id! sbst::of epsilon)))
                                       (value-to-slot v1 1)
                                       (code-to-slot v2 (!dtarm! 0)))
                                      (t
                                       (initial-error
                                        '((sbst::!id!
                                           sbst::=
                                           sbst::of
                                           epsilon)))))
                           (as-to-slot v0
                                       (case
                                        v1
                                        (1
                                         (make-sb-term
                                          'constr-term-rep::constr
                                          (list v2)))
                                        (0
                                         (make-sb-term
                                          'constr-term-rep::mconstr
                                          (list
                                           v2
                                           (make-sb-term
                                            'constr-term-rep::constrs
                                            (ds-temp-rept
                                             (ck-rept-ref v3))))))))))
                   (t (initial-error '((sbst::datatype sbst::!id!)))))
    nil
    v0))


(defun !abbrev! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1 v2 v3)
    (ergo-ignore-if-unused v0 v1 v2 v3)
    (initials-only ((la-match ((sbst::abbrev)))
                    (progn (lam ((sbst::abbrev epsilon)) (gobble-token))
                           (lam ((sbst::!id! epsilon)) (gobble-to-slot v1))
                           (alt-parse ((la-match ((sbst::==)))
                                       (value-to-slot v2 0)
                                       (lam
                                        ((sbst::== epsilon))
                                        (gobble-token)))
                                      ((la-match ((sbst::=)))
                                       (value-to-slot v2 1)
                                       (lam
                                        ((sbst::= epsilon))
                                        (gobble-token)))
                                      (t
                                       (initial-error
                                        '((sbst::== epsilon)
                                          (sbst::= epsilon)))))
                           (code-to-slot v3 (!ptype! 0))
                           (as-to-slot v0
                                       (make-sb-term
                                        'constr-term-rep::abbrev
                                        (list v1 v3)))))
                   (t (initial-error '((sbst::abbrev sbst::!id!)))))
    nil
    v0))


(defun !item! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1)
    (ergo-ignore-if-unused v0 v1)
    (initials-only ((la-match ((sbst::abbrev)))
                    (progn (code-to-slot v1 (!abbrev! 0)) (value-to-slot v0 0)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::abbrev-item
                                        (list v1)))))
                   ((la-match ((sbst::datatype)))
                    (progn (code-to-slot v1 (!datatype! 0))
                           (value-to-slot v0 1)
                           (as-to-slot v1
                                       (make-sb-term
                                        'constr-term-rep::mconstr-item
                                        (list v1)))))
                   (t
                    (initial-error '((sbst::datatype sbst::!id! sbst::!id!)
                                     (sbst::abbrev sbst::!id!)))))
    (value-to-slot v0 v1)
    v0))


(defun !items! (rbp &optional (bracket-list (empty-bracket-list)))
  (declare (ignore rbp))
  (declare (ignore bracket-list))
  (let (v0 v1)
    (ergo-ignore-if-unused v0 v1)
    (initials-only ((la-match ((sbst::datatype) (sbst::abbrev)))
                    (progn (code-to-slot v1 (!item! 0)) nil
                           (cons-to-slot v0 v1)
                           (gen-star-parse ((sbst::datatype
                                             sbst::!id!
                                             sbst::!id!)
                                            (sbst::abbrev sbst::!id!))
                                           (progn
                                            (code-to-slot v1 (!item! 0))
                                            nil)
                                           v0 v1)
                           (as-to-slot v0
                                       (make-sb-term
                                        'constr-term-rep::items
                                        (ds-temp-rept (ck-rept-ref v0))))))
                   (t
                    (initial-error '((sbst::abbrev sbst::!id!)
                                     (sbst::datatype sbst::!id!
                                      sbst::!id!)))))
    nil
    v0))

