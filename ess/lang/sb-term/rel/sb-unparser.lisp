;;; -*- mode: lisp; package: syntax-box -*-
(in-package :syntax-box)  ;; creates package for abstract syntax. 

(in-package :syntax-box)  ;; enters package for generated code.  

(use-package '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr))


(export '( sb-unparse  sb-win-unparse ))

(defvar sb-bracket-info (make-hash-table :test #'eq)) 
(clrhash sb-bracket-info) 
(mapc
 #'(lambda (entry)
     (set-bracket-info (car entry) (cadr entry) (caddr entry) sb-bracket-info))
 '((augment sbst::{ sbst::}))) 

(defvar sb-prec-info (make-hash-table :test #'eq)) 
(clrhash sb-prec-info) 
(mapc #'(lambda (nt)
          (init-prec-info nt sb-prec-info)) '(pattern augment)) 
(mapc
 #'(lambda (entry)
     (set-prec-info (car entry) (cadr entry) (caddr entry) (cadddr entry)
                    sb-prec-info))
 '((pattern sbst::{ 0 10) (pattern sbst::[ 0 10) (pattern sbst::^ 1 80)
  (pattern jux 1 70) (pattern sbst::++ 1 60) (pattern sbst::** 1 60)
  (pattern sbst::+ 1 60) (pattern sbst::* 1 60) (pattern sbst::< 1 50)
  (pattern sbst::<< 1 40) (pattern sbst::jux 1 30) (pattern sbst::\| 1 20)
  (pattern sbst::^ 2 81) (pattern jux 2 71) (pattern sbst::++ 2 61)
  (pattern sbst::** 2 61) (pattern sbst::+ 2 61) (pattern sbst::* 2 61)
  (pattern sbst::< 2 51) (pattern sbst::<< 2 41) (pattern sbst::jux 2 31)
  (pattern sbst::\| 2 21) (augment sbst::[ 0 10) (augment sbst::^ 1 40)
  (augment sbst::+ 1 30) (augment sbst::* 1 30) (augment sbst::\| 1 20)
  (augment sbst::^ 2 41) (augment sbst::+ 2 31) (augment sbst::* 2 31)
  (augment sbst::\| 2 21))) 

(defvar sb-spacing-info (make-hash-table :test #'eq)) 
(clrhash sb-spacing-info) 
(mapc
 #'(lambda (t1
            t2
            bp)
     (cond ((assoc t2 (gethash t1 sb-spacing-info)))
           (t
            (setf (gethash t1 sb-spacing-info)
                  (cons (list t2 bp) (gethash t1 sb-spacing-info))))))
 '(sbst::|(| sbst::|)| sbst::[ sbst::] sbst::{ sbst::} sbst::< sbst::> sbst::<<
  sbst::|,| sbst::|;| sbst::\| sbst::^ sbst::@ sbst::|:| sbst::|::=| sbst::+
  sbst::++ sbst::* sbst::** sbst::!- sbst::!+ sbst::_ sbst::|#| sbst::?
  sbst::/+ sbst::/- sbst::@> sbst::@< sbst::@^ sbst::id sbst::number
  sbst::string sbst::literal sbst::keyword meta-grammar external-grammars
  comment-character escape-character operator-information lexical-terminals
  bracketing-information spacing-information precedence-information
  multiple-levels single-level single-op-precedence nonterminal-definition
  pattern augment format-command sbst::|(| sbst::|)| sbst::[ sbst::] sbst::{
  sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;| sbst::\| sbst::^ sbst::@
  sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::* sbst::** sbst::!- sbst::!+
  sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/- sbst::@> sbst::@< sbst::@^
  sbst::id sbst::number sbst::string sbst::literal sbst::keyword meta-grammar
  external-grammars comment-character escape-character operator-information
  lexical-terminals bracketing-information spacing-information
  precedence-information multiple-levels single-level single-op-precedence
  nonterminal-definition pattern augment format-command sbst::|(| sbst::|)|
  sbst::[ sbst::] sbst::{ sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;|
  sbst::\| sbst::^ sbst::@ sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::*
  sbst::** sbst::!- sbst::!+ sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/-
  sbst::@> sbst::@< sbst::@^ sbst::id sbst::number sbst::string sbst::literal
  sbst::keyword meta-grammar external-grammars comment-character
  escape-character operator-information lexical-terminals
  bracketing-information spacing-information precedence-information
  multiple-levels single-level single-op-precedence nonterminal-definition
  pattern augment format-command sbst::|(| sbst::|)| sbst::[ sbst::] sbst::{
  sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;| sbst::\| sbst::^ sbst::@
  sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::* sbst::** sbst::!- sbst::!+
  sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/- sbst::@> sbst::@< sbst::@^
  sbst::id sbst::number sbst::string sbst::literal sbst::keyword meta-grammar
  external-grammars comment-character escape-character operator-information
  lexical-terminals bracketing-information spacing-information
  precedence-information multiple-levels single-level single-op-precedence
  nonterminal-definition pattern augment format-command sbst::^ sbst::|(|
  sbst::|)| sbst::[ sbst::] sbst::{ sbst::} sbst::< sbst::> sbst::<< sbst::|,|
  sbst::|;| sbst::\| sbst::^ sbst::@ sbst::|:| sbst::|::=| sbst::+ sbst::++
  sbst::* sbst::** sbst::!- sbst::!+ sbst::_ sbst::|#| sbst::? sbst::/+
  sbst::/- sbst::@> sbst::@< sbst::@^ sbst::id sbst::number sbst::string
  sbst::literal sbst::keyword meta-grammar external-grammars comment-character
  escape-character operator-information lexical-terminals
  bracketing-information spacing-information precedence-information
  multiple-levels single-level single-op-precedence nonterminal-definition
  pattern augment format-command sbst::< sbst::<< sbst::|(| sbst::|)| sbst::[
  sbst::] sbst::{ sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;| sbst::\|
  sbst::^ sbst::@ sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::* sbst::**
  sbst::!- sbst::!+ sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/- sbst::@>
  sbst::@< sbst::@^ sbst::id sbst::number sbst::string sbst::literal
  sbst::keyword meta-grammar external-grammars comment-character
  escape-character operator-information lexical-terminals
  bracketing-information spacing-information precedence-information
  multiple-levels single-level single-op-precedence nonterminal-definition
  pattern augment format-command sbst::|(| sbst::|)| sbst::[ sbst::] sbst::{
  sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;| sbst::\| sbst::^ sbst::@
  sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::* sbst::** sbst::!- sbst::!+
  sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/- sbst::@> sbst::@< sbst::@^
  sbst::id sbst::number sbst::string sbst::literal sbst::keyword meta-grammar
  external-grammars comment-character escape-character operator-information
  lexical-terminals bracketing-information spacing-information
  precedence-information multiple-levels single-level single-op-precedence
  nonterminal-definition pattern augment format-command sbst::|(| sbst::|)|
  sbst::[ sbst::] sbst::{ sbst::} sbst::< sbst::> sbst::<< sbst::|,| sbst::|;|
  sbst::\| sbst::^ sbst::@ sbst::|:| sbst::|::=| sbst::+ sbst::++ sbst::*
  sbst::** sbst::!- sbst::!+ sbst::_ sbst::|#| sbst::? sbst::/+ sbst::/-
  sbst::@> sbst::@< sbst::@^ sbst::id sbst::number sbst::string sbst::literal
  sbst::keyword meta-grammar external-grammars comment-character
  escape-character operator-information lexical-terminals
  bracketing-information spacing-information precedence-information
  multiple-levels single-level single-op-precedence nonterminal-definition
  pattern augment format-command sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::|#| sbst::_ sbst::/+ sbst::/- sbst::? sbst::!+)
 '(sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+
  sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+
  sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+
  sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+
  sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+
  sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::+ sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::* sbst::*
  sbst::* sbst::* sbst::* sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::++
  sbst::++ sbst::++ sbst::++ sbst::++ sbst::++ sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::**
  sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** sbst::** :arb
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^
  sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ sbst::^ :arb :arb sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::> sbst::>
  sbst::> sbst::> sbst::> sbst::> sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::< sbst::<
  sbst::< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<< sbst::<<
  sbst::<< sbst::<< sbst::<< sbst::<< sbst::> sbst::} sbst::|;| sbst::|:| :arb
  sbst::|#| sbst::number sbst::number sbst::number :arb sbst::number)
 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 0 0 1 0 0 0 0 0 0)) 

(defvar sb-key-token-map (make-hash-table :test #'eq)) 
(defvar sb-key-esc-token-map (if sb-escape-char (make-hash-table :test #'eq))) 
(defvar sb-lt-token-map (make-hash-table :test #'equal)) 
(defvar sb-lt-esc-token-map (if sb-escape-char (make-hash-table :test #'equal))) 


(defun sb-unparse (nt as &key (style *unparse-style*))
  (let* ((*unparser-op-list* sb-all-operators-list)
         (*key-token-map* sb-key-token-map)
         (*key-esc-token-map* sb-key-esc-token-map)
         (*lt-token-map* sb-lt-token-map)
         (*lt-esc-token-map* sb-lt-esc-token-map)
         (*apply-lt-dis-fun* #'apply-lexical-terminal-discriminator)
         (*apply-lt-des-fun* #'apply-lexical-terminal-destructor)
         (*apply-lt-token-cons-fun* #'apply-lt-token-constructor)
         (*bracket-info* sb-bracket-info)
         (*prec-info* sb-prec-info)
         (sort (opsig-output (opsig-table-lookup (term-op as))))
         (nt-name
          (cond (nt)
                ((is-sort-ttype sort)
                 (ds-sort-ttype sort))
                (t
                 'meta-grammar)))
         (*case-sensitive* sb-case-sensitive)
         (*escape-character* sb-escape-char)
         (*restricted-chars* sb-restricted-chars)
         (*string-char* sb-string-char)
         (*literal-char* sb-literal-char)
         (*keyword-char* sb-keyword-char)
         (*unparse-style*
          (if (and style (not (consp style))) (list style) style))
         (*no-escapes* (or *no-escapes* (null (insert-escapes?))))
         (*current-print-depth* 1))
    (let ((uterm
           (case nt-name
             (meta-grammar (sb-unp-meta-grammar as :top-level? t))
             (external-grammars (sb-unp-external-grammars as :top-level? t))
             (comment-character (sb-unp-comment-character as :top-level? t))
             (escape-character (sb-unp-escape-character as :top-level? t))
             (operator-information
              (sb-unp-operator-information as :top-level? t))
             (lexical-terminals (sb-unp-lexical-terminals as :top-level? t))
             (bracketing-information
              (sb-unp-bracketing-information as :top-level? t))
             (spacing-information (sb-unp-spacing-information as :top-level? t))
             (precedence-information
              (sb-unp-precedence-information as :top-level? t))
             (multiple-levels (sb-unp-multiple-levels as :top-level? t))
             (single-level (sb-unp-single-level as :top-level? t))
             (single-op-precedence
              (sb-unp-single-op-precedence as :top-level? t))
             (nonterminal-definition
              (sb-unp-nonterminal-definition as :top-level? t))
             (pattern (sb-unp-pattern as :top-level? t))
             (augment (sb-unp-augment as :top-level? t))
             (format-command (sb-unp-format-command as :top-level? t))
             (t (unparse-runtime-error
		 "nonterminal not unparsable:" nt-name nil)
		nil))))
      uterm))) 

(defun sb-win-unparse (uterm width &key (fontwidth sb-deffontwidth) (fontheight sb-deffontheight))
  (let* ((*case-sensitive* sb-case-sensitive)
         (*unparser-op-list* sb-all-operators-list)
         (*spacing-info* sb-spacing-info))
    (format-uterm uterm width :fontwidth fontwidth :fontheight fontheight))) 



(defun sb-unp-meta-grammar (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-meta-grammar-aux :top-level? top-level?)) 



(defun sb-unp-external-grammars (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-external-grammars-aux :top-level? top-level?)) 



(defun sb-unp-comment-character (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-comment-character-aux :top-level? top-level?)) 



(defun sb-unp-escape-character (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-escape-character-aux :top-level? top-level?)) 



(defun sb-unp-operator-information (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-operator-information-aux :top-level? top-level?)) 



(defun sb-unp-lexical-terminals (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-lexical-terminals-aux :top-level? top-level?)) 



(defun sb-unp-bracketing-information (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-bracketing-information-aux :top-level? top-level?)) 



(defun sb-unp-spacing-information (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-spacing-information-aux :top-level? top-level?)) 



(defun sb-unp-precedence-information (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-precedence-information-aux :top-level? top-level?)) 



(defun sb-unp-multiple-levels (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-multiple-levels-aux :top-level? top-level?)) 



(defun sb-unp-single-level (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-single-level-aux :top-level? top-level?)) 



(defun sb-unp-single-op-precedence (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-single-op-precedence-aux :top-level? top-level?)) 



(defun sb-unp-nonterminal-definition (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-nonterminal-definition-aux :top-level? top-level?)) 



(defun sb-unp-pattern (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-pattern-aux :top-level? top-level?)) 



(defun sb-unp-augment (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-augment-aux :top-level? top-level?)) 



(defun sb-unp-format-command (as &key (top-level? nil))
  (memo-uterm as #'sb-unp-format-command-aux :top-level? top-level?)) 



(defun sb-unp-meta-grammar-aux (as)
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
    (setf v0 as)
    (nt-unp 'meta-grammar as
            ((upats
              ((:elided :pats :upats :augs)
               (unp-uterm v0
                          ((unp-bind 'seq_ v0) (unp-term-const 'grammar)
                           (unp-name 'opt v2) (toss-name 'opt) (toss-name 'opt)
                           (toss-name 'opt) (toss-name 'opt) (toss-name 'opt)
                           (toss-name 'opt) (toss-name 'opt) (unp-list-const)
                           (unp-name (quote nil) v4)
                           (dis-opt vnil (lambda (x)
                                           (match-id 'nocase x))
                                    (lambda (x)
                                      (match-id 'case x)))
                           (unp-opt-aug vnil ((unp-base-lit)) ((unp-base-lit)))
                           (toss-name 'opt)
                           (unp-seq
                            (((unp-keyword 'sbst::grammar))
                             ((unp-bind 'opt_0 v2)
                              (dis-opt v1
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-10 x)))
                                       (lambda (x)
                                         (dis-lt 'sbst::id x)))
                              (unp-opt-aug v1 ((unp-null-const))
                                           ((unp-name (quote nil) v3)))
                              (unp-opt v1 ((unp-lt 'sbst::id v3))))
                             ((unp-keyword 'sbst::|...|))
                             ((unp-uterm v4
                                         ((unp-rept v4
                                                    ((unp-uterm v5
                                                                ((unp-bind
                                                                  'seq_ v5)
                                                                 (unp-name
                                                                  'nonterminal-definition
                                                                  v6)
                                                                 (unp-seq
                                                                  (((unp-nt
                                                                     sb-unp-nonterminal-definition
                                                                     v6))
                                                                   ((unp-keyword
                                                                     'sbst::|;|)))
                                                                  ((make-bp
                                                                    :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                   (make-bp
                                                                    :value
                                                                    -268435456
                                                                    :united-flag
                                                                    nil :spaces
                                                                      nil
                                                                      :crs
                                                                      0
                                                                      :format
                                                                      (list
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1)
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1))))))))
                                                    v5
                                                    (make-bp :value 0
                                                             :united-flag nil
                                                             :spaces nil :crs
                                                             nil :format (list)))))))
                            ((make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value -268435456 :united-flag nil
                                      :spaces nil :crs 0 :format
                                      (list
                                       (make-token :kind :whitespace :subkind
                                                   :cr :value 1)
                                       (make-token :kind :whitespace :subkind
                                                   :cr :value 1)))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list)))))))
              (t
               (unp-uterm v0
                          ((unp-bind 'seq_ v0) (unp-term-const 'grammar)
                           (unp-name 'opt v2) (unp-name 'opt v6)
                           (unp-name 'opt v10) (unp-name 'opt v16)
                           (unp-name 'opt v19) (unp-name 'opt v22)
                           (unp-name 'opt v25) (unp-name 'opt v28)
                           (unp-list-const) (unp-name (quote nil) v30)
                           (dis-opt v8 (lambda (x)
                                         (match-id 'nocase x))
                                    (lambda (x)
                                      (match-id 'case x)))
                           (unp-opt-aug v8 ((unp-base-lit)) ((unp-base-lit)))
                           (unp-name 'opt v13)
                           (unp-seq
                            (((unp-bind 'opt_0 v2)
                              (dis-opt v1
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-1 x)))
                                       (lambda (x)
                                         (dis-lt 'sbst::id x)))
                              (unp-opt-aug v1 ((unp-null-const))
                                           ((unp-name (quote nil) v3)))
                              (unp-opt v1
                                       ((unp-bind 'seq_ v3) (unp-name 'id v4)
                                        (unp-seq
                                         (((unp-keyword 'sbst::grammar))
                                          ((unp-lt 'sbst::id v4)))
                                         ((make-bp :value 0 :united-flag nil
                                                   :spaces nil :crs nil :format
                                                   (list))
                                          (make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_1 v6)
                              (dis-opt v5
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-2 x)))
                                       (lambda (x)
                                         (sb-dis-external-grammars x)))
                              (unp-opt-aug v5 ((unp-null-const))
                                           ((unp-name (quote nil) v7)))
                              (unp-opt v5
                                       ((unp-seq
                                         (((unp-nt sb-unp-external-grammars v7)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-opt v8
                                       ((unp-seq
                                         (((unp-keyword 'sbst::case))
                                          ((unp-keyword 'sbst::sensitive)))
                                         ((make-bp :value 0 :united-flag nil
                                                   :spaces nil :crs nil :format
                                                   (list))
                                          (make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_2 v10)
                              (dis-opt v9
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-3 x)))
                                       (lambda (x)
                                         (sb-dis-comment-character x)))
                              (unp-opt-aug v9 ((unp-null-const))
                                           ((unp-name (quote nil) v11)))
                              (unp-opt v9
                                       ((unp-seq
                                         (((unp-nt sb-unp-comment-character v11)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_9 v13)
                              (dis-opt v12
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-4 x)))
                                       (lambda (x)
                                         (sb-dis-escape-character x)))
                              (unp-opt-aug v12 ((unp-null-const))
                                           ((unp-name (quote nil) v14)))
                              (unp-opt v12
                                       ((unp-seq
                                         (((unp-nt sb-unp-escape-character v14)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_3 v16)
                              (dis-opt v15
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-5 x)))
                                       (lambda (x)
                                         (sb-dis-operator-information x)))
                              (unp-opt-aug v15 ((unp-null-const))
                                           ((unp-name (quote nil) v17)))
                              (unp-opt v15
                                       ((unp-seq
                                         (((unp-nt sb-unp-operator-information
                                                   v17)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_4 v19)
                              (dis-opt v18
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-6 x)))
                                       (lambda (x)
                                         (sb-dis-lexical-terminals x)))
                              (unp-opt-aug v18 ((unp-null-const))
                                           ((unp-name (quote nil) v20)))
                              (unp-opt v18
                                       ((unp-seq
                                         (((unp-nt sb-unp-lexical-terminals v20)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_5 v22)
                              (dis-opt v21
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-7 x)))
                                       (lambda (x)
                                         (sb-dis-bracketing-information x)))
                              (unp-opt-aug v21 ((unp-null-const))
                                           ((unp-name (quote nil) v23)))
                              (unp-opt v21
                                       ((unp-seq
                                         (((unp-nt
                                            sb-unp-bracketing-information v23)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_6 v25)
                              (dis-opt v24
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-8 x)))
                                       (lambda (x)
                                         (sb-dis-precedence-information x)))
                              (unp-opt-aug v24 ((unp-null-const))
                                           ((unp-name (quote nil) v26)))
                              (unp-opt v24
                                       ((unp-seq
                                         (((unp-nt
                                            sb-unp-precedence-information v26)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-bind 'opt_7 v28)
                              (dis-opt v27
                                       (lambda (x)
                                         (and (dis-op 'meta-grammar-null-9 x)))
                                       (lambda (x)
                                         (sb-dis-spacing-information x)))
                              (unp-opt-aug v27 ((unp-null-const))
                                           ((unp-name (quote nil) v29)))
                              (unp-opt v27
                                       ((unp-seq
                                         (((unp-nt sb-unp-spacing-information
                                                   v29)))
                                         ((make-bp :value -268435456
                                                   :united-flag nil :spaces nil
                                                   :crs 0 :format
                                                   (list
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1)
                                                    (make-token :kind
                                                                :whitespace
                                                                :subkind :cr
                                                                :value 1))))))))
                             ((unp-uterm v30
                                         ((unp-rept v30
                                                    ((unp-uterm v31
                                                                ((unp-bind
                                                                  'seq_ v31)
                                                                 (unp-name
                                                                  'nonterminal-definition
                                                                  v32)
                                                                 (unp-seq
                                                                  (((unp-nt
                                                                     sb-unp-nonterminal-definition
                                                                     v32))
                                                                   ((unp-keyword
                                                                     'sbst::|;|)))
                                                                  ((make-bp
                                                                    :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                   (make-bp
                                                                    :value
                                                                    -268435456
                                                                    :united-flag
                                                                    nil :spaces
                                                                      nil
                                                                      :crs
                                                                      0
                                                                      :format
                                                                      (list
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1)
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1))))))))
                                                    v31
                                                    (make-bp :value 0
                                                             :united-flag nil
                                                             :spaces nil :crs
                                                             nil :format (list)))))))
                            ((make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list))
                             (make-bp :value -268435456 :united-flag nil
                                      :spaces nil :crs 0 :format
                                      (list
                                       (make-token :kind :whitespace :subkind
                                                   :cr :value 1)))
                             (make-bp :value 0 :united-flag nil :spaces nil
                                      :crs nil :format (list)))))))))))) 



(defun sb-unp-external-grammars-aux (as)
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (setf v0 as)
    (nt-unp 'external-grammars as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name 'doubleplus v1)
                         (unp-seq
                          (((unp-keyword 'sbst::external))
                           ((unp-keyword 'sbst::grammars))
                           ((unp-uterm v1
                                       ((unp-double-rept v1
                                                         ((unp-uterm v2
                                                                     ((unp-lt
                                                                      'sbst::id
                                                                      v2))))
                                                         ((unp-keyword
                                                           'sbst::|,|))
                                                         v2
                                                         (make-bp :value 2
                                                                  :united-flag
                                                                  nil :spaces
                                                                  nil :crs nil
                                                                  :format
                                                                  (list))
                                                         (make-bp :value 0
                                                                  :united-flag
                                                                  nil :spaces
                                                                  nil :crs nil
                                                                  :format
                                                                  (list)))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-comment-character-aux (as)
  (let (v0
        v1
        v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5)
    (setf v0 as)
    (nt-unp 'comment-character as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0)
                         (unp-term-const 'comment-character)
                         (dis-opt v1 (lambda (x)
                                       (and (dis-op 'null x)))
                                  (lambda (x)
                                    (dis-lt 'sbst::keyword x)))
                         (unp-opt-aug v1 ((unp-term-const 'null))
                                      ((unp-name 'keyword v2)))
                         (dis-opt v3 (lambda (x)
                                       (and (dis-op 'null x)))
                                  (lambda (x)
                                    (dis-lt 'sbst::keyword x)))
                         (unp-opt-aug v3 ((unp-term-const 'null))
                                      ((unp-name 'keyword v4)))
                         (unp-opt-aug v3 ((unp-term-const 'null))
                                      ((unp-name 'keyword v5)))
                         (unp-seq
                          (((unp-keyword 'sbst::comment))
                           ((unp-keyword 'sbst::character))
                           ((unp-opt v1
                                     ((unp-seq
                                       (((unp-keyword 'sbst::newline))
                                        ((unp-lt 'sbst::keyword v2)))
                                       ((make-bp :value 0 :united-flag nil
                                                 :spaces nil :crs nil :format
                                                 (list))
                                        (make-bp :value 0 :united-flag nil
                                                 :spaces nil :crs nil :format
                                                 (list)))))))
                           ((unp-opt v3
                                     ((unp-seq
                                       (((unp-keyword 'sbst::delimited))
                                        ((unp-lt 'sbst::keyword v4))
                                        ((unp-lt 'sbst::keyword v5)))
                                       ((make-bp :value 0 :united-flag nil
                                                 :spaces nil :crs nil :format
                                                 (list))
                                        (make-bp :value 0 :united-flag nil
                                                 :spaces nil :crs nil :format
                                                 (list))
                                        (make-bp :value 0 :united-flag nil
                                                 :spaces nil :crs nil :format
                                                 (list))))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-escape-character-aux (as)
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (setf v0 as)
    (nt-unp 'escape-character as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-term-const 'escape-character)
                         (unp-name 'keyword v1)
                         (unp-seq
                          (((unp-keyword 'sbst::escape))
                           ((unp-keyword 'sbst::character))
                           ((unp-lt 'sbst::keyword v1)))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-operator-information-aux (as)
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (setf v0 as)
    (nt-unp 'operator-information as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name 'star v1)
                         (unp-seq
                          (((unp-keyword 'sbst::operators))
                           ((unp-uterm v1
                                       ((unp-rept v1
                                                  ((unp-uterm v2
                                                              ((unp-lt
                                                                'sbst::keyword
                                                                v2))))
                                                  v2
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                          ((make-bp :value -268435456 :united-flag nil :spaces
                                    nil :crs 0 :format
                                    (list
                                     (make-token :kind :whitespace :subkind :cr
                                                 :value 1)))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-lexical-terminals-aux (as)
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6)
    (setf v0 as)
    (nt-unp 'lexical-terminals as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name (quote nil) v1)
                         (unp-seq
                          (((unp-keyword 'sbst::lexical))
                           ((unp-keyword 'sbst::terminals))
                           ((unp-uterm v1
                                       ((unp-double-rept v1
                                                         ((unp-uterm v2
                                                                     ((unp-bind
                                                                      'seq_ v2)
                                                                      (unp-name
                                                                      (quote
                                                                      nil)
                                                                      v6)
                                                                      (unp-bind
                                                                      '#:g23695
                                                                      v6)
                                                                      (dis-opt
                                                                      v4
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (and
                                                                      (dis-op
                                                                      'delimiter
                                                                      x))))
                                                                      (unp-opt-aug
                                                                      v4
                                                                      ((unp-name
                                                                      'id v3))
                                                                      ((unp-term-const
                                                                      'delimiter)
                                                                      (unp-name
                                                                      'id v3)
                                                                      (unp-name
                                                                      'keyword
                                                                      v5)))
                                                                      (unp-seq
                                                                      (((unp-lt
                                                                      'sbst::id
                                                                      v3))
                                                                      ((unp-opt
                                                                      v4
                                                                      ((unp-lt
                                                                      'sbst::keyword
                                                                      v5)))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))
                                                         ((unp-keyword
                                                           'sbst::|,|))
                                                         v2
                                                         (make-bp :value 2
                                                                  :united-flag
                                                                  nil :spaces
                                                                  nil :crs nil
                                                                  :format
                                                                  (list))
                                                         (make-bp :value 0
                                                                  :united-flag
                                                                  nil :spaces
                                                                  nil :crs nil
                                                                  :format
                                                                  (list)))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value -268435456 :united-flag nil :spaces
                                    nil :crs 0 :format
                                    (list
                                     (make-token :kind :whitespace :subkind :cr
                                                 :value 1)))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-bracketing-information-aux (as)
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6
        v7)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7)
    (setf v0 as)
    (nt-unp 'bracketing-information as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name (quote nil) v2)
                         (unp-seq
                          (((unp-keyword 'sbst::bracketing))
                           ((unp-opt v1 ((unp-keyword 'sbst::information))))
                           ((unp-uterm v2
                                       ((unp-rept v2
                                                  ((unp-uterm v3
                                                              ((unp-bind 'seq_
                                                                      v3)
                                                               (unp-name
                                                                (quote nil) v7)
                                                               (unp-bind
                                                                '#:g23697 v7)
                                                               (unp-term-const
                                                                'bracket-entry)
                                                               (unp-name 'id v4)
                                                               (unp-name
                                                                'keyword v5)
                                                               (unp-name
                                                                'keyword v6)
                                                               (unp-seq
                                                                (((unp-lt
                                                                   'sbst::id v4))
                                                                 ((unp-lt
                                                                   'sbst::keyword
                                                                   v5))
                                                                 ((unp-lt
                                                                   'sbst::keyword
                                                                   v6)))
                                                                ((make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                 (make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                 (make-bp
                                                                  :value
                                                                  -268435456
                                                                  :united-flag
                                                                  nil :spaces
                                                                      nil
                                                                      :crs
                                                                      0
                                                                      :format
                                                                      (list
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1))))))))
                                                  v3
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value -268435456 :united-flag nil :spaces
                                    nil :crs 0 :format
                                    (list
                                     (make-token :kind :whitespace :subkind :cr
                                                 :value 1)))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-spacing-information-aux (as)
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
    (setf v0 as)
    (nt-unp 'spacing-information as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name (quote nil) v2)
                         (unp-seq
                          (((unp-keyword 'sbst::spacing))
                           ((unp-opt v1 ((unp-keyword 'sbst::information))))
                           ((unp-uterm v2
                                       ((unp-rept v2
                                                  ((unp-uterm v3
                                                              ((unp-bind 'seq_
                                                                      v3)
                                                               (unp-name
                                                                (quote nil) v16)
                                                               (unp-bind
                                                                '#:g23699 v16)
                                                               (unp-term-const
                                                                'space-com)
                                                               (dis-alt v4
                                                                      ((lambda (x)
                                                                      (dis-lt
                                                                      'sbst::keyword
                                                                      x))
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'op
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'lt
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'arb
                                                                      x))
                                                                      (lambda (x)
                                                                      (and
                                                                      (dis-op
                                                                      'jux-op
                                                                      x)))))
                                                               (unp-alt-aug v4
                                                                      (((unp-name
                                                                      'keyword
                                                                      v5))
                                                                      ((unp-name
                                                                      'id
                                                                      v5))
                                                                      ((unp-base-lit))
                                                                      ((unp-base-lit))
                                                                      ((unp-base-lit))
                                                                      ((unp-term-const
                                                                      'jux-op)
                                                                      (dis-opt
                                                                      v5
                                                                      (lambda (x)
                                                                      (and
                                                                      (dis-op
                                                                      'null
                                                                      x)))
                                                                      (lambda (x)
                                                                      (or
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x)
                                                                      (dis-lt
                                                                      'sbst::number
                                                                      x))))
                                                                      (unp-opt-aug
                                                                      v5
                                                                      ((unp-term-const
                                                                      'null))
                                                                      ((unp-name
                                                                      'alt
                                                                      v7))))))
                                                               (unp-list-const)
                                                               (unp-name 'star
                                                                      v9)
                                                               (dis-alt v11
                                                                      ((lambda (x)
                                                                      (dis-lt
                                                                      'sbst::keyword
                                                                      x))
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'op
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'lt
                                                                      x))
                                                                      (lambda (x)
                                                                      (match-id
                                                                      'arb
                                                                      x))
                                                                      (lambda (x)
                                                                      (and
                                                                      (dis-op
                                                                      'jux-op
                                                                      x)))))
                                                               (unp-alt-aug v11
                                                                      (((unp-name
                                                                      'keyword
                                                                      v12))
                                                                      ((unp-name
                                                                      'id
                                                                      v12))
                                                                      ((unp-base-lit))
                                                                      ((unp-base-lit))
                                                                      ((unp-base-lit))
                                                                      ((unp-term-const
                                                                      'jux-op)
                                                                      (dis-opt
                                                                      v12
                                                                      (lambda (x)
                                                                      (and
                                                                      (dis-op
                                                                      'null
                                                                      x)))
                                                                      (lambda (x)
                                                                      (or
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x)
                                                                      (dis-lt
                                                                      'sbst::number
                                                                      x))))
                                                                      (unp-opt-aug
                                                                      v12
                                                                      ((unp-term-const
                                                                      'null))
                                                                      ((unp-name
                                                                      'alt
                                                                      v14))))))
                                                               (unp-seq
                                                                (((unp-alt v4
                                                                      (((unp-lt
                                                                      'sbst::keyword
                                                                      v5))
                                                                      ((unp-lt
                                                                      'sbst::id
                                                                      v5))
                                                                      ((unp-keyword
                                                                      'sbst::op))
                                                                      ((unp-keyword
                                                                      'sbst::lt))
                                                                      ((unp-keyword
                                                                      'sbst::arb))
                                                                      ((unp-seq
                                                                      (((unp-keyword
                                                                      'sbst::jux))
                                                                      ((unp-opt
                                                                      v5
                                                                      ((unp-seq
                                                                      (((unp-keyword
                                                                      'sbst::^))
                                                                      ((unp-bind
                                                                      'alt_tag
                                                                      v7)
                                                                      (dis-alt
                                                                      v6
                                                                      ((lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::number
                                                                      x))))
                                                                      (unp-alt-aug
                                                                      v6
                                                                      (((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v8))
                                                                      ((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v8))))
                                                                      (unp-alt
                                                                      v6
                                                                      (((unp-lt
                                                                      'sbst::id
                                                                      v8))
                                                                      ((unp-lt
                                                                      'sbst::number
                                                                      v8))))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))))
                                                                 ((unp-uterm v9
                                                                      ((unp-rept
                                                                      v9
                                                                      ((unp-uterm
                                                                      v10
                                                                      ((unp-nt
                                                                      sb-unp-format-command
                                                                      v10))))
                                                                      v10
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))
                                                                 ((unp-alt v11
                                                                      (((unp-lt
                                                                      'sbst::keyword
                                                                      v12))
                                                                      ((unp-lt
                                                                      'sbst::id
                                                                      v12))
                                                                      ((unp-keyword
                                                                      'sbst::op))
                                                                      ((unp-keyword
                                                                      'sbst::lt))
                                                                      ((unp-keyword
                                                                      'sbst::arb))
                                                                      ((unp-seq
                                                                      (((unp-keyword
                                                                      'sbst::jux))
                                                                      ((unp-opt
                                                                      v12
                                                                      ((unp-seq
                                                                      (((unp-keyword
                                                                      'sbst::^))
                                                                      ((unp-bind
                                                                      'alt_tag
                                                                      v14)
                                                                      (dis-alt
                                                                      v13
                                                                      ((lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::number
                                                                      x))))
                                                                      (unp-alt-aug
                                                                      v13
                                                                      (((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v15))
                                                                      ((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v15))))
                                                                      (unp-alt
                                                                      v13
                                                                      (((unp-lt
                                                                      'sbst::id
                                                                      v15))
                                                                      ((unp-lt
                                                                      'sbst::number
                                                                      v15))))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))))
                                                                ((make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                 (make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                 (make-bp
                                                                  :value
                                                                  -268435456
                                                                  :united-flag
                                                                  nil :spaces
                                                                      nil
                                                                      :crs
                                                                      0
                                                                      :format
                                                                      (list
                                                                      (make-token
                                                                      :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1))))))))
                                                  v3
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value -268435456 :united-flag nil :spaces
                                    nil :crs 0 :format
                                    (list
                                     (make-token :kind :whitespace :subkind :cr
                                                 :value 1)))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-precedence-information-aux (as)
  (let (v0
        v1
        v2
        v3
        v4
        v5
        v6)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6)
    (setf v0 as)
    (nt-unp 'precedence-information as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-list-const)
                         (unp-name (quote nil) v2)
                         (unp-seq
                          (((unp-keyword 'sbst::precedence))
                           ((unp-opt v1 ((unp-keyword 'sbst::information))))
                           ((unp-uterm v2
                                       ((unp-rept v2
                                                  ((unp-uterm v3
                                                              ((unp-bind 'seq_
                                                                      v3)
                                                               (unp-name
                                                                (quote nil) v6)
                                                               (unp-bind
                                                                '#:g23701 v6)
                                                               (unp-term-const
                                                                'prec-entry)
                                                               (unp-name 'id v4)
                                                               (unp-name
                                                                'multiple-levels
                                                                v5)
                                                               (unp-seq
                                                                (((unp-lt
                                                                   'sbst::id v4))
                                                                 ((unp-nt
                                                                   sb-unp-multiple-levels
                                                                   v5)))
                                                                ((make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                 (make-bp
                                                                  :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))
                                                  v3
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value -268435456 :united-flag nil :spaces
                                    nil :crs 0 :format
                                    (list
                                     (make-token :kind :whitespace :subkind :cr
                                                 :value 1)))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-multiple-levels-aux (as)
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (setf v0 as)
    (nt-unp 'multiple-levels as
            ((unp-uterm v0
                        ((unp-bind 'plus_ v0) (unp-list-const)
                         (unp-name 'plus v0)
                         (unp-rept v0
                                   ((unp-uterm v1
                                               ((unp-seq
                                                 (((unp-nt sb-unp-single-level
                                                           v1)))
                                                 ((make-bp :value -268435456
                                                           :united-flag nil
                                                           :spaces nil :crs 0
                                                           :format
                                                           (list
                                                            (make-token :kind
                                                                      :whitespace
                                                                      :subkind
                                                                      :cr
                                                                      :value
                                                                      1))))))))
                                   v1
                                   (make-bp :value -268435456 :united-flag nil
                                            :spaces nil :crs 0 :format
                                            (list
                                             (make-token :kind :whitespace
                                                         :subkind :cr :value 1)))))))))) 



(defun sb-unp-single-level-aux (as)
  (let (v0
        v1)
    (ergo-ignore-if-unused v0 v1)
    (setf v0 as)
    (nt-unp 'single-level as
            ((unp-uterm v0
                        ((unp-bind 'doubleplus_ v0) (unp-list-const)
                         (unp-name 'doubleplus v0)
                         (unp-double-rept v0
                                          ((unp-uterm v1
                                                      ((unp-nt
                                                        sb-unp-single-op-precedence
                                                        v1))))
                                          ((unp-keyword 'sbst::|,|)) v1
                                          (make-bp :value 2 :united-flag nil
                                                   :spaces nil :crs nil :format
                                                   (list))
                                          (make-bp :value 0 :united-flag nil
                                                   :spaces nil :crs nil :format
                                                   (list))))))))) 



(defun sb-unp-single-op-precedence-aux (as)
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
    (setf v0 as)
    (nt-unp 'single-op-precedence as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0)
                         (dis-alt v7
                                  ((lambda (x)
                                     (and (dis-op 'initial x)))
                                   (lambda (x)
                                     (and (dis-op 'medial x)))
                                   (lambda (x)
                                     (and (dis-op 'aggregate x)))))
                         (unp-alt-aug v7
                                      (((unp-term-const 'initial)
                                        (unp-name 'alt v2))
                                       ((unp-term-const 'medial)
                                        (unp-name 'alt v2)
                                        (dis-alt v8
                                                 ((lambda (x)
                                                    (match-id 'left x))
                                                  (lambda (x)
                                                    (match-id 'right x))
                                                  (lambda (x)
                                                    (match-id 'lbind x))
                                                  (lambda (x)
                                                    (match-id 'rbind x))))
                                        (unp-alt-aug v8
                                                     (((unp-base-lit))
                                                      ((unp-base-lit))
                                                      ((unp-base-lit))
                                                      ((unp-base-lit)))))
                                       ((unp-term-const 'aggregate)
                                        (unp-name 'alt v2))))
                         (unp-seq
                          (((unp-uterm v2
                                       ((unp-bind 'alt_key v2)
                                        (dis-alt v1
                                                 ((lambda (x)
                                                    (and (dis-op 'keyword-op x)))
                                                  (lambda (x)
                                                    (and (dis-op 'jux-op x)))))
                                        (unp-alt-aug v1
                                                     (((unp-term-const
                                                        'keyword-op)
                                                       (unp-name 'keyword v3))
                                                      ((unp-term-const 'jux-op)
                                                       (dis-opt v3
                                                                (lambda (x)
                                                                  (and
                                                                   (dis-op
                                                                    'null x)))
                                                                (lambda (x)
                                                                  (or
                                                                   (dis-lt
                                                                    'sbst::id x)
                                                                   (dis-lt
                                                                    'sbst::number
                                                                    x))))
                                                       (unp-opt-aug v3
                                                                    ((unp-term-const
                                                                      'null))
                                                                    ((unp-name
                                                                      'alt v5))))))
                                        (unp-alt v1
                                                 (((unp-lt 'sbst::keyword v3))
                                                  ((unp-seq
                                                    (((unp-keyword 'sbst::jux))
                                                     ((unp-opt v3
                                                               ((unp-seq
                                                                 (((unp-keyword
                                                                    'sbst::^))
                                                                  ((unp-bind
                                                                    'alt_tag v5)
                                                                   (dis-alt v4
                                                                      ((lambda (x)
                                                                      (dis-lt
                                                                      'sbst::id
                                                                      x))
                                                                      (lambda (x)
                                                                      (dis-lt
                                                                      'sbst::number
                                                                      x))))
                                                                   (unp-alt-aug
                                                                    v4
                                                                    (((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v6))
                                                                     ((unp-name
                                                                      (quote
                                                                      nil)
                                                                      v6))))
                                                                   (unp-alt v4
                                                                      (((unp-lt
                                                                      'sbst::id
                                                                      v6))
                                                                      ((unp-lt
                                                                      'sbst::number
                                                                      v6))))))
                                                                 ((make-bp
                                                                   :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                  (make-bp
                                                                   :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))))
                                                    ((make-bp :value 0
                                                              :united-flag nil
                                                              :spaces nil :crs
                                                              nil :format
                                                              (list))
                                                     (make-bp :value 0
                                                              :united-flag nil
                                                              :spaces nil :crs
                                                              nil :format
                                                              (list))))))))))
                           ((unp-alt v7
                                     (((unp-keyword 'sbst::initial))
                                      ((unp-seq
                                        (((unp-keyword 'sbst::medial))
                                         ((unp-alt v8
                                                   (((unp-keyword 'sbst::left))
                                                    ((unp-keyword 'sbst::right))
                                                    ((unp-keyword 'sbst::lbind))
                                                    ((unp-keyword 'sbst::rbind))))))
                                        ((make-bp :value 0 :united-flag nil
                                                  :spaces nil :crs nil :format
                                                  (list))
                                         (make-bp :value 0 :united-flag nil
                                                  :spaces nil :crs nil :format
                                                  (list)))))
                                      ((unp-keyword 'sbst::aggregate))))))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-nonterminal-definition-aux (as)
  (let (v0
        v1
        v2)
    (ergo-ignore-if-unused v0 v1 v2)
    (setf v0 as)
    (nt-unp 'nonterminal-definition as
            ((unp-uterm v0
                        ((unp-bind 'seq_ v0) (unp-term-const 'nt-def)
                         (unp-name 'id v1) (unp-name 'pattern v2)
                         (unp-seq
                          (((unp-lt 'sbst::id v1)) ((unp-keyword 'sbst::|::=|))
                           ((unp-nt sb-unp-pattern v2)))
                          ((make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list)))))))))) 



(defun sb-unp-pattern-aux (as)
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
        v10)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (setf v0 as)
    (nt-unp 'pattern as
            ((unp-bind 'alt_ v0)
             (dis-alt v0
                      ((lambda (x)
                         (and (dis-op 'nonterminal x)))
                       (lambda (x)
                         (and (dis-op 'ext-nonterminal x)))
                       (lambda (x)
                         (and (dis-op 'ukeyword x)))
                       (lambda (x)
                         (and (dis-op 'jux x)))
                       (lambda (x)
                         (and (dis-op 'opt x)
                              (and (dis-op 'seq (term-argn x 0)))))
                       (lambda (x)
                         (and (dis-op 'seq x)))
                       (lambda (x)
                         (and (dis-op 'alt x)))
                       (lambda (x)
                         (and (dis-op 'star x)))
                       (lambda (x)
                         (and (dis-op 'plus x)))
                       (lambda (x)
                         (or
                          (and (dis-op 'doubleplus x)
                               (and (dis-op 'ukeyword (term-argn x 1))))
                          (and (dis-op 'doubleplus x)
                               (and (dis-op 'format (term-argn x 1))
                                    (and
                                     (dis-op 'ukeyword
                                             (term-argn (term-argn x 1) 0)))
                                    (and
                                     (dis-op 'ws-specs
                                             (term-argn (term-argn x 1) 1)))))))
                       (lambda (x)
                         (or
                          (and (dis-op 'doublestar x)
                               (and (dis-op 'ukeyword (term-argn x 1))))
                          (and (dis-op 'doublestar x)
                               (and (dis-op 'format (term-argn x 1))
                                    (and
                                     (dis-op 'ukeyword
                                             (term-argn (term-argn x 1) 0)))
                                    (and
                                     (dis-op 'ws-specs
                                             (term-argn (term-argn x 1) 1)))))))
                       (lambda (x)
                         (and (dis-op 'tag x)))
                       (lambda (x)
                         (and (dis-op 'format x)
                              (and (dis-op 'ws-specs (term-argn x 1)))))
                       (lambda (x)
                         (and (dis-op 'augment x)))
                       (lambda (x)
                         (and (dis-op 'upattern x)
                              (and (dis-op 'upats (term-argn x 1)))))))
             (unp-name (quote nil) v1)
             (unp-alt v0
                      (((sb-unp-pattern-aux-alt-1 v0 v1))
                       ((sb-unp-pattern-aux-alt-2 v0 v1))
                       ((sb-unp-pattern-aux-alt-3 v0 v1))
                       ((sb-unp-pattern-aux-alt-4 v0 v1))
                       ((sb-unp-pattern-aux-alt-5 v0 v1))
                       ((sb-unp-pattern-aux-alt-6 v0 v1))
                       ((sb-unp-pattern-aux-alt-7 v0 v1))
                       ((sb-unp-pattern-aux-alt-8 v0 v1))
                       ((sb-unp-pattern-aux-alt-9 v0 v1))
                       ((sb-unp-pattern-aux-alt-10 v0 v1))
                       ((sb-unp-pattern-aux-alt-11 v0 v1))
                       ((sb-unp-pattern-aux-alt-12 v0 v1))
                       ((sb-unp-pattern-aux-alt-13 v0 v1))
                       ((sb-unp-pattern-aux-alt-14 v0 v1))
                       ((sb-unp-pattern-aux-alt-15 v0 v1)))))))) 



(defun sb-unp-pattern-aux-alt-1 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'nonterminal)
                (unp-name 'id v1) (unp-lt 'sbst::id v1))))) 



(defun sb-unp-pattern-aux-alt-2 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'ext-nonterminal)
                (unp-name 'id v2) (unp-name 'id v3)
                (unp-seq
                 (((unp-lt 'sbst::id v2)) ((unp-keyword 'sbst::@))
                  ((unp-lt 'sbst::id v3)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-3 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'ukeyword)
                (unp-name 'keyword v1) (unp-lt 'sbst::keyword v1))))) 



(defun sb-unp-pattern-aux-alt-4 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'jux)
                (unp-name 'pattern v2) (unp-name 'pattern v3)
                (dis-opt v4
                         (lambda (x)
                           (and (dis-op 'ws-specs x) (null (get-term-args x))))
                         (lambda (x)
                           (and (dis-op 'ws-specs x))))
                (unp-opt-aug v4 ((unp-elist-const))
                             ((unp-list-const) (unp-name 'plus v5)))
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::jux))
                  ((unp-nt sb-unp-pattern v3))
                  ((unp-opt v4
                            ((unp-seq
                              (((unp-keyword 'sbst::juxform))
                               ((unp-uterm v5
                                           ((unp-rept v5
                                                      ((unp-uterm v6
                                                                  ((unp-nt
                                                                    sb-unp-format-command
                                                                    v6))))
                                                      v6
                                                      (make-bp :value 0
                                                               :united-flag nil
                                                               :spaces nil :crs
                                                               nil :format
                                                               (list)))))))
                              ((make-bp :value 0 :united-flag nil :spaces nil
                                        :crs nil :format (list))
                               (make-bp :value 0 :united-flag nil :spaces nil
                                        :crs nil :format (list))))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-5 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'opt) (unp-list-const)
                (unp-name 'plus v2)
                (unp-seq
                 (((unp-keyword 'sbst::[))
                  ((unp-uterm v2
                              ((unp-rept v2
                                         ((unp-uterm v3
                                                     ((unp-nt sb-unp-pattern v3))))
                                         v3
                                         (make-bp :value 0 :united-flag nil
                                                  :spaces nil :crs nil :format
                                                  (list))))))
                  ((unp-keyword 'sbst::])))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-6 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-list-const) (unp-name 'plus v2)
                (unp-seq
                 (((unp-keyword 'sbst::{))
                  ((unp-uterm v2
                              ((unp-rept v2
                                         ((unp-uterm v3
                                                     ((unp-nt sb-unp-pattern v3))))
                                         v3
                                         (make-bp :value 0 :united-flag nil
                                                  :spaces nil :crs nil :format
                                                  (list))))))
                  ((unp-keyword 'sbst::})))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-7 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (upats
     ((:parse)
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-list-const) (unp-cons)
                  (unp-name 'pattern v2) (unp-name 'doubleplus v3)
                  (unp-seq
                   (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::\|))
                    ((unp-uterm v3
                                ((unp-double-rept v3
                                                  ((unp-uterm v4
                                                              ((unp-nt
                                                                sb-unp-pattern
                                                                v4))))
                                                  ((unp-keyword 'sbst::\|)) v4
                                                  (make-bp :value 2
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list))
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                   ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))
     (t
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-list-const) (unp-cons)
                  (unp-name 'pattern v2) (unp-name 'doubleplus v3)
                  (unp-seq
                   (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::\|))
                    ((unp-uterm v3
                                ((unp-double-rept v3
                                                  ((unp-uterm v4
                                                              ((unp-nt
                                                                sb-unp-pattern
                                                                v4))))
                                                  ((unp-keyword 'sbst::\|)) v4
                                                  (make-bp :value 2
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list))
                                                  (make-bp :value -500
                                                           :united-flag
                                                           (list 1) :spaces nil
                                                           :crs nil :format
                                                           (list)))))))
                   ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value -500 :united-flag (list 1) :spaces nil :crs
                             nil :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))))) 



(defun sb-unp-pattern-aux-alt-8 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'star)
                (unp-name 'pattern v2)
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::*)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-9 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'plus)
                (unp-name 'pattern v2)
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::+)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-10 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1)
                (dis-opt v4
                         (lambda (x)
                           (and (dis-op 'doubleplus x)
                                (and (dis-op 'ukeyword (term-argn x 1)))))
                         (lambda (x)
                           (and (dis-op 'doubleplus x)
                                (and (dis-op 'format (term-argn x 1))
                                     (and
                                      (dis-op 'ukeyword
                                              (term-argn (term-argn x 1) 0)))
                                     (and
                                      (dis-op 'ws-specs
                                              (term-argn (term-argn x 1) 1)))))))
                (unp-opt-aug v4
                             ((unp-term-const 'doubleplus)
                              (unp-name 'pattern v2) (unp-term-const 'ukeyword)
                              (unp-name 'keyword v3))
                             ((unp-term-const 'doubleplus)
                              (unp-name 'pattern v2) (unp-term-const 'format)
                              (unp-term-const 'ukeyword) (unp-name 'keyword v3)
                              (unp-list-const) (unp-name 'plus v5)))
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::++))
                  ((unp-lt 'sbst::keyword v3))
                  ((unp-opt v4
                            ((unp-uterm v5
                                        ((unp-rept v5
                                                   ((unp-uterm v6
                                                               ((unp-nt
                                                                 sb-unp-format-command
                                                                 v6))))
                                                   v6
                                                   (make-bp :value 0
                                                            :united-flag nil
                                                            :spaces nil :crs
                                                            nil :format (list)))))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-11 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1)
                (dis-opt v4
                         (lambda (x)
                           (and (dis-op 'doublestar x)
                                (and (dis-op 'ukeyword (term-argn x 1)))))
                         (lambda (x)
                           (and (dis-op 'doublestar x)
                                (and (dis-op 'format (term-argn x 1))
                                     (and
                                      (dis-op 'ukeyword
                                              (term-argn (term-argn x 1) 0)))
                                     (and
                                      (dis-op 'ws-specs
                                              (term-argn (term-argn x 1) 1)))))))
                (unp-opt-aug v4
                             ((unp-term-const 'doublestar)
                              (unp-name 'pattern v2) (unp-term-const 'ukeyword)
                              (unp-name 'keyword v3))
                             ((unp-term-const 'doublestar)
                              (unp-name 'pattern v2) (unp-term-const 'format)
                              (unp-term-const 'ukeyword) (unp-name 'keyword v3)
                              (unp-list-const) (unp-name 'plus v5)))
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::**))
                  ((unp-lt 'sbst::keyword v3))
                  ((unp-opt v4
                            ((unp-uterm v5
                                        ((unp-rept v5
                                                   ((unp-uterm v6
                                                               ((unp-nt
                                                                 sb-unp-format-command
                                                                 v6))))
                                                   v6
                                                   (make-bp :value 0
                                                            :united-flag nil
                                                            :spaces nil :crs
                                                            nil :format (list)))))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-12 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'tag)
                (unp-name 'pattern v2) (unp-name 'alt v4)
                (unp-seq
                 (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::^))
                  ((unp-bind 'alt_ v4)
                   (dis-alt v3
                            ((lambda (x)
                               (dis-lt 'sbst::id x))
                             (lambda (x)
                               (dis-lt 'sbst::number x))))
                   (unp-alt-aug v3
                                (((unp-name (quote nil) v5))
                                 ((unp-name (quote nil) v5))))
                   (unp-alt v3
                            (((unp-lt 'sbst::id v5))
                             ((unp-lt 'sbst::number v5))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-pattern-aux-alt-13 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (upats
     ((:pats :augs :noform :elided)
      (unp-uterm v1
                 ((unp-bind 'nonterminal_ v1) (unp-term-const 'format)
                  (unp-name 'pattern v1) (unp-term-const 'ws-specs)
                  (toss-name 'plus) (unp-nt sb-unp-pattern v1))))
     (t
      (unp-uterm v1
                 ((unp-bind 'jux_ v1) (unp-term-const 'format)
                  (unp-name 'pattern v2) (unp-list-const) (unp-name 'plus v3)
                  (unp-jux ((unp-nt sb-unp-pattern v2)) 'jux
                           ((unp-uterm v3
                                       ((unp-rept v3
                                                  ((unp-uterm v4
                                                              ((unp-nt
                                                                sb-unp-format-command
                                                                v4))))
                                                  v4
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list))))))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))
                           (make-bp :value 0 :united-flag nil :spaces nil :crs
                                    nil :format (list))))))))) 



(defun sb-unp-pattern-aux-alt-14 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (upats
     ((:pats :upats :noaugs)
      (unp-uterm v1
                 ((unp-bind 'nonterminal_ v1) (unp-term-const 'augment)
                  (unp-name 'pattern v1) (toss-name 'augment)
                  (unp-nt sb-unp-pattern v1))))
     ((:augs)
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-term-const 'augment)
                  (toss-name 'pattern) (unp-name 'augment v2)
                  (unp-seq
                   (((unp-keyword 'sbst::<)) ((unp-nt sb-unp-augment v2))
                    ((unp-keyword 'sbst::>)))
                   ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))
     (t
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-term-const 'augment)
                  (unp-name 'pattern v2) (unp-name 'augment v3)
                  (unp-seq
                   (((unp-nt sb-unp-pattern v2)) ((unp-keyword 'sbst::<))
                    ((unp-nt sb-unp-augment v3)) ((unp-keyword 'sbst::>)))
                   ((make-bp :value -200 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))))) 



(defun sb-unp-pattern-aux-alt-15 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5
        v6
        v7
        v8
        v9
        v10)
    (ergo-ignore-if-unused v2 v3 v4 v5 v6 v7 v8 v9 v10)
    (upats
     ((:pats :augs :noupats :elided :noform)
      (unp-uterm v1
                 ((unp-bind 'nonterminal_ v1) (unp-term-const 'upattern)
                  (unp-name 'pattern v1) (unp-term-const 'upats) (toss-rept)
                  (unp-nt sb-unp-pattern v1))))
     ((:upats)
      (unp-uterm v1
                 ((unp-bind 'plus_ v1) (unp-term-const 'upattern)
                  (toss-name 'pattern) (unp-list-const)
                  (unp-name (quote nil) v1)
                  (unp-rept v1
                            ((unp-uterm v2
                                        ((unp-bind 'seq_ v2)
                                         (unp-name (quote nil) v8)
                                         (unp-bind '#:g23706 v8)
                                         (unp-term-const 'upat)
                                         (dis-opt v5
                                                  (lambda (x)
                                                    (and (dis-op 'uids x)
                                                         (null
                                                          (get-term-args x))))
                                                  (lambda (x)
                                                    (and (dis-op 'uids x))))
                                         (unp-opt-aug v5 ((unp-elist-const))
                                                      ((unp-list-const)
                                                       (unp-name (quote nil) v6)))
                                         (unp-list-const)
                                         (unp-name (quote nil) v3)
                                         (unp-seq
                                          (((unp-keyword 'sbst::<<))
                                           ((unp-uterm v3
                                                       ((unp-rept v3
                                                                  ((unp-uterm
                                                                    v4
                                                                    ((unp-bind
                                                                      'nonterminal_
                                                                      v4)
                                                                     (unp-name
                                                                      'pattern
                                                                      v4)
                                                                     (unp-nt
                                                                      sb-unp-pattern
                                                                      v4))))
                                                                  v4
                                                                  (make-bp
                                                                   :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))
                                           ((unp-keyword 'sbst::>))
                                           ((unp-opt v5
                                                     ((unp-seq
                                                       (((unp-keyword
                                                          'sbst::|:|))
                                                        ((unp-uterm v6
                                                                    ((unp-double-rept
                                                                      v6
                                                                      ((unp-uterm
                                                                      v7
                                                                      ((unp-bind
                                                                      'nonterminal_
                                                                      v7)
                                                                      (unp-name
                                                                      'id
                                                                      v7)
                                                                      (unp-lt
                                                                      'sbst::id
                                                                      v7))))
                                                                      ((unp-keyword
                                                                      'sbst::|,|))
                                                                      v7
                                                                      (make-bp
                                                                      :value 2
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value 0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))
                                                       ((make-bp :value 0
                                                                 :united-flag
                                                                 nil :spaces
                                                                 nil :crs nil
                                                                 :format (list))
                                                        (make-bp :value 0
                                                                 :united-flag
                                                                 nil :spaces
                                                                 nil :crs nil
                                                                 :format (list)))))))
                                           ((unp-keyword 'sbst::>)))
                                          ((make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list))
                                           (make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list))
                                           (make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list))
                                           (make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list))
                                           (make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list)))))))
                            v2
                            (make-bp :value 0 :united-flag nil :spaces nil :crs
                                     nil :format (list))))))
     (t
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-term-const 'upattern)
                  (unp-name 'pattern v2) (unp-list-const)
                  (unp-name (quote nil) v3)
                  (unp-seq
                   (((unp-nt sb-unp-pattern v2))
                    ((unp-uterm v3
                                ((unp-rept v3
                                           ((unp-uterm v4
                                                       ((unp-bind 'seq_ v4)
                                                        (unp-name (quote nil)
                                                                  v10)
                                                        (unp-bind '#:g23703 v10)
                                                        (unp-term-const 'upat)
                                                        (dis-opt v7
                                                                 (lambda (x)
                                                                   (and
                                                                    (dis-op
                                                                     'uids x)
                                                                    (null
                                                                     (get-term-args
                                                                      x))))
                                                                 (lambda (x)
                                                                   (and
                                                                    (dis-op
                                                                     'uids x))))
                                                        (unp-opt-aug v7
                                                                     ((unp-elist-const))
                                                                     ((unp-list-const)
                                                                      (unp-name
                                                                      (quote
                                                                      nil)
                                                                      v8)))
                                                        (unp-list-const)
                                                        (unp-name (quote nil)
                                                                  v5)
                                                        (unp-seq
                                                         (((unp-keyword
                                                            'sbst::<<))
                                                          ((unp-uterm v5
                                                                      ((unp-rept
                                                                      v5
                                                                      ((unp-uterm
                                                                      v6
                                                                      ((unp-bind
                                                                      'nonterminal_
                                                                      v6)
                                                                      (unp-name
                                                                      'pattern
                                                                      v6)
                                                                      (unp-nt
                                                                      sb-unp-pattern
                                                                      v6))))
                                                                      v6
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))))))
                                                          ((unp-keyword
                                                            'sbst::>))
                                                          ((unp-opt v7
                                                                    ((unp-seq
                                                                      (((unp-keyword
                                                                      'sbst::|:|))
                                                                      ((unp-uterm
                                                                      v8
                                                                      ((unp-double-rept
                                                                      v8
                                                                      ((unp-uterm
                                                                      v9
                                                                      ((unp-bind
                                                                      'nonterminal_
                                                                      v9)
                                                                      (unp-name
                                                                      'id
                                                                      v9)
                                                                      (unp-lt
                                                                      'sbst::id
                                                                      v9))))
                                                                      ((unp-keyword
                                                                      'sbst::|,|))
                                                                      v9
                                                                      (make-bp
                                                                      :value
                                                                      2
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))
                                                                      ((make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list))
                                                                      (make-bp
                                                                      :value
                                                                      0
                                                                      :united-flag
                                                                      nil
                                                                      :spaces
                                                                      nil
                                                                      :crs
                                                                      nil
                                                                      :format
                                                                      (list)))))))
                                                          ((unp-keyword
                                                            'sbst::>)))
                                                         ((make-bp :value 0
                                                                   :united-flag
                                                                   nil :spaces
                                                                   nil :crs nil
                                                                   :format
                                                                   (list))
                                                          (make-bp :value 0
                                                                   :united-flag
                                                                   nil :spaces
                                                                   nil :crs nil
                                                                   :format
                                                                   (list))
                                                          (make-bp :value 0
                                                                   :united-flag
                                                                   nil :spaces
                                                                   nil :crs nil
                                                                   :format
                                                                   (list))
                                                          (make-bp :value 0
                                                                   :united-flag
                                                                   nil :spaces
                                                                   nil :crs nil
                                                                   :format
                                                                   (list))
                                                          (make-bp :value 0
                                                                   :united-flag
                                                                   nil :spaces
                                                                   nil :crs nil
                                                                   :format
                                                                   (list)))))))
                                           v4
                                           (make-bp :value 0 :united-flag nil
                                                    :spaces nil :crs nil
                                                    :format (list)))))))
                   ((make-bp :value -200 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))))) 



(defun sb-unp-augment-aux (as)
  (let (v0
        v1
        v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4 v5)
    (setf v0 as)
    (nt-unp 'augment as
            ((unp-bind 'alt_ v0)
             (dis-alt v0
                      ((lambda (x)
                         (and (dis-op 'string-aug x)))
                       (lambda (x)
                         (and (dis-op 'number-aug x)))
                       (lambda (x)
                         (and (dis-op 'literal-aug x)))
                       (lambda (x)
                         (and (dis-op 'name x)))
                       (lambda (x)
                         (and (dis-op 'name x) (match-id 'opt (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x) (match-id 'alt (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x) (match-id 'plus (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x)
                              (match-id 'doubleplus (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x) (match-id 'star (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x)
                              (match-id 'doublestar (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'name x) (match-id 'seq (term-argn x 0))))
                       (lambda (x)
                         (and (dis-op 'ext-name x)))
                       (lambda (x)
                         (and (dis-op 'null x)))
                       (lambda (x)
                         (and (dis-op 'list x)))
                       (lambda (x)
                         (and (dis-op 'cons x)))
                       (lambda (x)
                         (and (dis-op 'bcons x)))
                       (lambda (x)
                         (and (dis-op 'append x)))
                       (lambda (x)
                         (and (dis-op 'term-const x)))
                       (lambda (x)
                         (and (dis-op 'star-aug x)))
                       (lambda (x)
                         (and (dis-op 'plus-aug x)))
                       (lambda (x)
                         (and (dis-op 'alt-aug x)))
                       (lambda (x)
                         (and (dis-op 'opt-aug x)))
                       (lambda (x)
                         (and (dis-op 'tag-aug x)))))
             (unp-name (quote nil) v1)
             (unp-alt v0
                      (((sb-unp-augment-aux-alt-1 v0 v1))
                       ((sb-unp-augment-aux-alt-2 v0 v1))
                       ((sb-unp-augment-aux-alt-3 v0 v1))
                       ((sb-unp-augment-aux-alt-4 v0 v1))
                       ((sb-unp-augment-aux-alt-5 v0 v1))
                       ((sb-unp-augment-aux-alt-6 v0 v1))
                       ((sb-unp-augment-aux-alt-7 v0 v1))
                       ((sb-unp-augment-aux-alt-8 v0 v1))
                       ((sb-unp-augment-aux-alt-9 v0 v1))
                       ((sb-unp-augment-aux-alt-10 v0 v1))
                       ((sb-unp-augment-aux-alt-11 v0 v1))
                       ((sb-unp-augment-aux-alt-12 v0 v1))
                       ((sb-unp-augment-aux-alt-13 v0 v1))
                       ((sb-unp-augment-aux-alt-14 v0 v1))
                       ((sb-unp-augment-aux-alt-15 v0 v1))
                       ((sb-unp-augment-aux-alt-16 v0 v1))
                       ((sb-unp-augment-aux-alt-17 v0 v1))
                       ((sb-unp-augment-aux-alt-18 v0 v1))
                       ((sb-unp-augment-aux-alt-19 v0 v1))
                       ((sb-unp-augment-aux-alt-20 v0 v1))
                       ((sb-unp-augment-aux-alt-21 v0 v1))
                       ((sb-unp-augment-aux-alt-22 v0 v1))
                       ((sb-unp-augment-aux-alt-23 v0 v1)))))))) 



(defun sb-unp-augment-aux-alt-1 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'string-aug)
                (unp-name 'string v1) (unp-lt 'sbst::string v1))))) 



(defun sb-unp-augment-aux-alt-2 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'number-aug)
                (unp-name 'number v1) (unp-lt 'sbst::number v1))))) 



(defun sb-unp-augment-aux-alt-3 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'literal-aug)
                (unp-name 'literal v1) (unp-lt 'sbst::literal v1))))) 



(defun sb-unp-augment-aux-alt-4 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'nonterminal_ v1) (unp-term-const 'name)
                (unp-name 'id v1) (unp-lt 'sbst::id v1))))) 



(defun sb-unp-augment-aux-alt-5 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::opt))))) 



(defun sb-unp-augment-aux-alt-6 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::alt))))) 



(defun sb-unp-augment-aux-alt-7 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::plus))))) 



(defun sb-unp-augment-aux-alt-8 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::doubleplus))))) 



(defun sb-unp-augment-aux-alt-9 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::star))))) 



(defun sb-unp-augment-aux-alt-10 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::doublestar))))) 



(defun sb-unp-augment-aux-alt-11 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'name) (unp-base-lit)
                (unp-keyword 'sbst::seq))))) 



(defun sb-unp-augment-aux-alt-12 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'ext-name)
                (unp-name 'id v2) (unp-name 'id v3)
                (unp-seq
                 (((unp-lt 'sbst::id v2)) ((unp-keyword 'sbst::@))
                  ((unp-lt 'sbst::id v3)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-13 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'null)
                (unp-keyword 'sbst::null))))) 



(defun sb-unp-augment-aux-alt-14 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-list-const) (unp-name 'doublestar v2)
                (unp-seq
                 (((unp-keyword 'sbst::list)) ((unp-keyword 'sbst::|(|))
                  ((unp-uterm v2
                              ((unp-double-rept v2
                                                ((unp-uterm v3
                                                            ((unp-nt
                                                              sb-unp-augment v3))))
                                                ((unp-keyword 'sbst::|,|)) v3
                                                (make-bp :value 2 :united-flag
                                                         nil :spaces nil :crs
                                                         nil :format (list))
                                                (make-bp :value 0 :united-flag
                                                         nil :spaces nil :crs
                                                         nil :format (list))))))
                  ((unp-keyword 'sbst::|)|)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-15 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'cons)
                (unp-name 'augment v2) (unp-name 'augment v3)
                (unp-seq
                 (((unp-keyword 'sbst::cons)) ((unp-keyword 'sbst::|(|))
                  ((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::|,|))
                  ((unp-nt sb-unp-augment v3)) ((unp-keyword 'sbst::|)|)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-16 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'bcons)
                (unp-name 'augment v2) (unp-name 'augment v3)
                (unp-seq
                 (((unp-keyword 'sbst::bcons)) ((unp-keyword 'sbst::|(|))
                  ((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::|,|))
                  ((unp-nt sb-unp-augment v3)) ((unp-keyword 'sbst::|)|)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-17 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'append)
                (unp-name 'augment v2) (unp-name 'augment v3)
                (unp-seq
                 (((unp-keyword 'sbst::append)) ((unp-keyword 'sbst::|(|))
                  ((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::|,|))
                  ((unp-nt sb-unp-augment v3)) ((unp-keyword 'sbst::|)|)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-18 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-list-const) (unp-cons)
                (dis-alt v2
                         ((lambda (x)
                            (and (dis-op 'id-aug x)))
                          (lambda (x)
                            (and (dis-op 'literal-aug x)))
                          (lambda (x)
                            (and (dis-op 'string-aug x)))))
                (unp-alt-aug v2
                             (((unp-term-const 'id-aug) (unp-name 'id v3))
                              ((unp-term-const 'literal-aug)
                               (unp-name 'literal v3))
                              ((unp-term-const 'string-aug)
                               (unp-name 'string v3))))
                (unp-name 'doublestar v4)
                (unp-seq
                 (((unp-alt v2
                            (((unp-lt 'sbst::id v3))
                             ((unp-lt 'sbst::literal v3))
                             ((unp-lt 'sbst::string v3)))))
                  ((unp-keyword 'sbst::|(|))
                  ((unp-uterm v4
                              ((unp-double-rept v4
                                                ((unp-uterm v5
                                                            ((unp-nt
                                                              sb-unp-augment v5))))
                                                ((unp-keyword 'sbst::|,|)) v5
                                                (make-bp :value 2 :united-flag
                                                         nil :spaces nil :crs
                                                         nil :format (list))
                                                (make-bp :value 0 :united-flag
                                                         nil :spaces nil :crs
                                                         nil :format (list))))))
                  ((unp-keyword 'sbst::|)|)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-19 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'star-aug)
                (unp-name 'augment v2)
                (unp-seq
                 (((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::*)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-20 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'plus-aug)
                (unp-name 'augment v2)
                (unp-seq
                 (((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::+)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-21 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (upats
     ((:parse)
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-list-const) (unp-cons)
                  (unp-name 'augment v2) (unp-name 'doubleplus v3)
                  (unp-seq
                   (((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::\|))
                    ((unp-uterm v3
                                ((unp-double-rept v3
                                                  ((unp-uterm v4
                                                              ((unp-nt
                                                                sb-unp-augment
                                                                v4))))
                                                  ((unp-keyword 'sbst::\|)) v4
                                                  (make-bp :value 2
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list))
                                                  (make-bp :value 0
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list)))))))
                   ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))
     (t
      (unp-uterm v1
                 ((unp-bind 'seq_ v1) (unp-list-const) (unp-cons)
                  (unp-name 'augment v2) (unp-name 'doubleplus v3)
                  (unp-seq
                   (((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::\|))
                    ((unp-uterm v3
                                ((unp-double-rept v3
                                                  ((unp-uterm v4
                                                              ((unp-nt
                                                                sb-unp-augment
                                                                v4))))
                                                  ((unp-keyword 'sbst::\|)) v4
                                                  (make-bp :value 2
                                                           :united-flag nil
                                                           :spaces nil :crs nil
                                                           :format (list))
                                                  (make-bp :value -500
                                                           :united-flag
                                                           (list 1) :spaces nil
                                                           :crs nil :format
                                                           (list)))))))
                   ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list))
                    (make-bp :value -500 :united-flag (list 1) :spaces nil :crs
                             nil :format (list))
                    (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                             :format (list)))))))))) 



(defun sb-unp-augment-aux-alt-22 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'opt-aug)
                (unp-name 'augment v2)
                (unp-seq
                 (((unp-keyword 'sbst::[)) ((unp-nt sb-unp-augment v2))
                  ((unp-keyword 'sbst::])))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-augment-aux-alt-23 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4
        v5)
    (ergo-ignore-if-unused v2 v3 v4 v5)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'tag-aug)
                (unp-name 'augment v2) (unp-name 'alt v4)
                (unp-seq
                 (((unp-nt sb-unp-augment v2)) ((unp-keyword 'sbst::^))
                  ((unp-bind 'alt_ v4)
                   (dis-alt v3
                            ((lambda (x)
                               (dis-lt 'sbst::id x))
                             (lambda (x)
                               (dis-lt 'sbst::number x))))
                   (unp-alt-aug v3
                                (((unp-name (quote nil) v5))
                                 ((unp-name (quote nil) v5))))
                   (unp-alt v3
                            (((unp-lt 'sbst::id v5))
                             ((unp-lt 'sbst::number v5))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux (as)
  (let (v0
        v1
        v2
        v3
        v4)
    (ergo-ignore-if-unused v0 v1 v2 v3 v4)
    (setf v0 as)
    (nt-unp 'format-command as
            ((unp-bind 'alt_ v0)
             (dis-alt v0
                      ((lambda (x)
                         (or (and (dis-op 'sp x) (match-num 1 (term-argn x 0)))
                             (and (dis-op 'sp x))))
                       (lambda (x)
                         (and (dis-op 'cr x)))
                       (lambda (x)
                         (and (dis-op 'incr-bp x)))
                       (lambda (x)
                         (and (dis-op 'decr-bp x)))
                       (lambda (x)
                         (and (dis-op 'unite x)))
                       (lambda (x)
                         (or
                          (and (dis-op 'push-indent x)
                               (match-num 2 (term-argn x 0)))
                          (and (dis-op 'push-indent x))))
                       (lambda (x)
                         (and (dis-op 'pop-indent x)))
                       (lambda (x)
                         (and (dis-op 'push-tab-left x)))
                       (lambda (x)
                         (and (dis-op 'push-tab-right x)))
                       (lambda (x)
                         (and (dis-op 'pop-tab x)))))
             (unp-name (quote nil) v1)
             (unp-alt v0
                      (((sb-unp-format-command-aux-alt-1 v0 v1))
                       ((sb-unp-format-command-aux-alt-2 v0 v1))
                       ((sb-unp-format-command-aux-alt-3 v0 v1))
                       ((sb-unp-format-command-aux-alt-4 v0 v1))
                       ((sb-unp-format-command-aux-alt-5 v0 v1))
                       ((sb-unp-format-command-aux-alt-6 v0 v1))
                       ((sb-unp-format-command-aux-alt-7 v0 v1))
                       ((sb-unp-format-command-aux-alt-8 v0 v1))
                       ((sb-unp-format-command-aux-alt-9 v0 v1))
                       ((sb-unp-format-command-aux-alt-10 v0 v1)))))))) 



(defun sb-unp-format-command-aux-alt-1 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'seq_ v1)
                (dis-opt v2
                         (lambda (x)
                           (and (dis-op 'sp x) (match-num 1 (term-argn x 0))))
                         (lambda (x)
                           (and (dis-op 'sp x))))
                (unp-opt-aug v2 ((unp-term-const 'sp) (unp-base-num))
                             ((unp-term-const 'sp) (unp-name 'number v3)))
                (unp-seq
                 (((unp-keyword 'sbst::_))
                  ((unp-opt v2 ((unp-lt 'sbst::number v3)))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux-alt-2 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'cr)
                (unp-keyword 'sbst::|#|))))) 



(defun sb-unp-format-command-aux-alt-3 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'incr-bp)
                (unp-name 'number v2)
                (unp-seq
                 (((unp-keyword 'sbst::/+)) ((unp-lt 'sbst::number v2)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux-alt-4 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'decr-bp)
                (unp-name 'number v2)
                (unp-seq
                 (((unp-keyword 'sbst::/-)) ((unp-lt 'sbst::number v2)))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux-alt-5 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'seq_ v1) (unp-term-const 'unite) (unp-name 'alt v3)
                (unp-seq
                 (((unp-keyword 'sbst::?))
                  ((unp-bind 'alt_ v3)
                   (dis-alt v2
                            ((lambda (x)
                               (dis-lt 'sbst::id x))
                             (lambda (x)
                               (dis-lt 'sbst::number x))))
                   (unp-alt-aug v2
                                (((unp-name (quote nil) v4))
                                 ((unp-name (quote nil) v4))))
                   (unp-alt v2
                            (((unp-lt 'sbst::id v4))
                             ((unp-lt 'sbst::number v4))))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux-alt-6 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'seq_ v1)
                (dis-opt v2
                         (lambda (x)
                           (and (dis-op 'push-indent x)
                                (match-num 2 (term-argn x 0))))
                         (lambda (x)
                           (and (dis-op 'push-indent x))))
                (unp-opt-aug v2 ((unp-term-const 'push-indent) (unp-base-num))
                             ((unp-term-const 'push-indent)
                              (unp-name 'number v3)))
                (unp-seq
                 (((unp-keyword 'sbst::!+))
                  ((unp-opt v2 ((unp-lt 'sbst::number v3)))))
                 ((make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list))
                  (make-bp :value 0 :united-flag nil :spaces nil :crs nil
                           :format (list)))))))) 



(defun sb-unp-format-command-aux-alt-7 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'pop-indent)
                (unp-keyword 'sbst::!-))))) 



(defun sb-unp-format-command-aux-alt-8 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'push-tab-left)
                (unp-keyword 'sbst::@<))))) 



(defun sb-unp-format-command-aux-alt-9 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'push-tab-right)
                (unp-keyword 'sbst::@>))))) 



(defun sb-unp-format-command-aux-alt-10 (v0 v1)
  (ergo-ignore-if-unused v0 v1)
  (let (v2
        v3
        v4)
    (ergo-ignore-if-unused v2 v3 v4)
    (unp-uterm v1
               ((unp-bind 'ukeyword_ v1) (unp-term-const 'pop-tab)
                (unp-keyword 'sbst::@^))))) 



(defun sb-dis-meta-grammar (as)
  (or (is-nt-opsig? 'meta-grammar as) ((lambda (x)
                                         (dis-op 'grammar x)) as))) 



(defun sb-dis-external-grammars (as)
  (or (is-nt-opsig? 'external-grammars as)
      ((lambda (x)
         (dis-op 'ext-gram x)) as))) 



(defun sb-dis-comment-character (as)
  (or (is-nt-opsig? 'comment-character as)
      ((lambda (x)
         (dis-op 'comment-character x)) as))) 



(defun sb-dis-escape-character (as)
  (or (is-nt-opsig? 'escape-character as)
      ((lambda (x)
         (dis-op 'escape-character x)) as))) 



(defun sb-dis-operator-information (as)
  (or (is-nt-opsig? 'operator-information as)
      ((lambda (x)
         (dis-op 'op-info x)) as))) 



(defun sb-dis-lexical-terminals (as)
  (or (is-nt-opsig? 'lexical-terminals as)
      ((lambda (x)
         (dis-op 'lex-terms x)) as))) 



(defun sb-dis-bracketing-information (as)
  (or (is-nt-opsig? 'bracketing-information as)
      ((lambda (x)
         (dis-op 'bracket-entries x)) as))) 



(defun sb-dis-spacing-information (as)
  (or (is-nt-opsig? 'spacing-information as)
      ((lambda (x)
         (dis-op 'spacing x)) as))) 



(defun sb-dis-precedence-information (as)
  (or (is-nt-opsig? 'precedence-information as)
      ((lambda (x)
         (dis-op 'prec-entries x)) as))) 



(defun sb-dis-multiple-levels (as)
  (or (is-nt-opsig? 'multiple-levels as)
      ((lambda (x)
         (dis-op 'prec-levels x)) as))) 



(defun sb-dis-single-level (as)
  (or (is-nt-opsig? 'single-level as) ((lambda (x)
                                         (dis-op 'prec-level x)) as))) 



(defun sb-dis-single-op-precedence (as)
  (or (is-nt-opsig? 'single-op-precedence as)
      ((lambda (x)
         (or ((lambda (x)
                (dis-op 'aggregate x)) x)
             ((lambda (x)
                (dis-op 'medial x)) x)
             ((lambda (x)
                (dis-op 'initial x)) x)))
       as))) 



(defun sb-dis-nonterminal-definition (as)
  (or (is-nt-opsig? 'nonterminal-definition as)
      ((lambda (x)
         (dis-op 'nt-def x)) as))) 



(defun sb-dis-pattern (as)
  (or (is-nt-opsig? 'pattern as)
      ((lambda (x)
         (or ((lambda (x)
                (dis-op 'upattern x)) x)
             ((lambda (x)
                (dis-op 'augment x)) x)
             ((lambda (x)
                (dis-op 'format x)) x)
             ((lambda (x)
                (dis-op 'tag x)) x)
             ((lambda (x)
                (dis-op 'doublestar x)) x)
             ((lambda (x)
                (dis-op 'doubleplus x)) x)
             ((lambda (x)
                (dis-op 'plus x)) x)
             ((lambda (x)
                (dis-op 'star x)) x) ((lambda (x)
                                                  (dis-op 'alt x)) x)
             ((lambda (x)
                (dis-op 'seq x)) x) ((lambda (x)
                                                 (dis-op 'opt x)) x)
             ((lambda (x)
                (dis-op 'jux x)) x)
             ((lambda (x)
                (dis-op 'ukeyword x)) x)
             ((lambda (x)
                (dis-op 'ext-nonterminal x)) x)
             ((lambda (x)
                (dis-op 'nonterminal x)) x)))
       as))) 



(defun sb-dis-augment (as)
  (or (is-nt-opsig? 'augment as)
      ((lambda (x)
         (or ((lambda (x)
                (dis-op 'tag-aug x)) x)
             ((lambda (x)
                (dis-op 'opt-aug x)) x)
             ((lambda (x)
                (dis-op 'alt-aug x)) x)
             ((lambda (x)
                (dis-op 'plus-aug x)) x)
             ((lambda (x)
                (dis-op 'star-aug x)) x)
             ((lambda (x)
                (dis-op 'term-const x)) x)
             ((lambda (x)
                (dis-op 'append x)) x)
             ((lambda (x)
                (dis-op 'bcons x)) x)
             ((lambda (x)
                (dis-op 'cons x)) x)
             ((lambda (x)
                (dis-op 'list x)) x)
             ((lambda (x)
                (dis-op 'null x)) x)
             ((lambda (x)
                (dis-op 'ext-name x)) x)
             ((lambda (x)
                (dis-op 'name x)) x)
             ((lambda (x)
                (dis-op 'literal-aug x)) x)
             ((lambda (x)
                (dis-op 'number-aug x)) x)
             ((lambda (x)
                (dis-op 'string-aug x)) x)))
       as))) 



(defun sb-dis-format-command (as)
  (or (is-nt-opsig? 'format-command as)
      ((lambda (x)
         (or ((lambda (x)
                (dis-op 'pop-tab x)) x)
             ((lambda (x)
                (dis-op 'push-tab-right x)) x)
             ((lambda (x)
                (dis-op 'push-tab-left x)) x)
             ((lambda (x)
                (dis-op 'pop-indent x)) x)
             ((lambda (x)
                (dis-op 'push-indent x)) x)
             ((lambda (x)
                (dis-op 'unite x)) x)
             ((lambda (x)
                (dis-op 'decr-bp x)) x)
             ((lambda (x)
                (dis-op 'incr-bp x)) x)
             ((lambda (x)
                (dis-op 'cr x)) x) ((lambda (x)
                                                (dis-op 'sp x)) x)))
       as))) 

