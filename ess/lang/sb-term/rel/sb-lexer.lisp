;;; -*- mode: lisp; package: syntax-box -*-
(in-package :syntax-box)  ;; creates package for abstract syntax. 

(in-package :syntax-box)  ;; enters package for generated code.  

(use-package '(:ergolisp :oper :occ :term :sort :sb-runtime :lang :newattr))


(export '())

(defparameter sb-keyword-list
              '(sbst::} sbst::{ sbst::|(| sbst::|,| sbst::|)| sbst::append
               sbst::|(| sbst::|,| sbst::|)| sbst::bcons sbst::|(| sbst::|,|
               sbst::|)| sbst::cons sbst::|(| sbst::|,| sbst::|)| sbst::list
               sbst::|(| sbst::|,| sbst::|)| sbst::null sbst::@ sbst::seq
               sbst::doublestar sbst::star sbst::doubleplus sbst::plus
               sbst::alt sbst::opt sbst::* sbst::+ sbst::\| sbst::\| sbst::[
               sbst::] sbst::^ sbst::|::=| sbst::initial sbst::medial
               sbst::left sbst::right sbst::lbind sbst::rbind sbst::aggregate
               sbst::bracketing sbst::operators sbst::comment sbst::newline
               sbst::delimited sbst::|;| sbst::sensitive sbst::case
               sbst::grammar sbst::grammars sbst::external sbst::character
               sbst::escape sbst::terminals sbst::lexical sbst::precedence
               sbst::spacing sbst::information sbst::op sbst::lt sbst::arb
               sbst::op sbst::lt sbst::arb sbst::> sbst::|:| sbst::> sbst::<<
               sbst::> sbst::|:| sbst::> sbst::<< sbst::> sbst::< sbst::**
               sbst::++ sbst::juxform sbst::jux sbst::_ sbst::|#| sbst::/+
               sbst::/- sbst::? sbst::!+ sbst::!- sbst::@< sbst::@> sbst::@^)) 
(defparameter sb-single-char-op-list
              '(#\? #\# #\_ #\^ #\| #\; #\, #\> #\} #\{ #\] #\[ #\) #\()) 
(defparameter sb-multi-char-op-list
              '((#\< . lex-<) (#\: . |lex-:|) (#\+ . lex-+) (#\* . lex-*)
               (#\! . lex-!) (#\/ . lex-/) (#\@ . lex-@))) 
(defparameter sb-all-operators-list
              '(sbst::@^ sbst::@< sbst::@> sbst::/- sbst::/+ sbst::? sbst::|#|
               sbst::_ sbst::!+ sbst::!- sbst::** sbst::* sbst::++ sbst::+
               sbst::|::=| sbst::|:| sbst::@ sbst::^ sbst::\| sbst::|;|
               sbst::|,| sbst::<< sbst::> sbst::< sbst::} sbst::{ sbst::]
               sbst::[ sbst::|)| sbst::|(|)) 
(defparameter sb-new-line-comment-char #\%) 
(defparameter sb-open-comment-char nil) 
(defparameter sb-close-comment-char nil) 
(defparameter sb-escape-char #\\) 
(defparameter sb-case-sensitive nil) 
(defparameter sb-string-char #\") 
(defparameter sb-keyword-char #\') 
(defparameter sb-literal-char #\`) 
(defparameter sb-restricted-chars
              (reduce #'(lambda (r
                                 s)
                          (union r s :test #'char=))
                      (list sb-single-char-op-list
                            (if sb-new-line-comment-char
                                (list sb-new-line-comment-char))
                            (if sb-open-comment-char
                                (list sb-open-comment-char))
                            (if sb-close-comment-char
                                (list sb-close-comment-char))
                            (if sb-escape-char (list sb-escape-char))
                            (if sb-string-char (list sb-string-char))
                            (if sb-keyword-char (list sb-keyword-char))
                            (if sb-literal-char (list sb-literal-char))))) 
(defvar *sb-keyword-table* nil) 
(defun init-lexer-aux (lexstream)
  (init-lexical-readtable lexstream :single-char-op-list sb-single-char-op-list
                          :new-line-comment-char sb-new-line-comment-char
                          :open-comment-char sb-open-comment-char :escape-char
                          sb-escape-char :multi-char-op-list
                          sb-multi-char-op-list)) 

(defun init-lexer (lexstream)
  (init-lexer-aux lexstream)
  (if sb-string-char
      (lexical-make-macro lexstream sb-string-char #'read-sb-string))
  (if sb-keyword-char
      (lexical-make-macro lexstream sb-keyword-char #'read-keyword-string))
  (if sb-literal-char
      (lexical-make-macro lexstream sb-literal-char #'read-literal))) 

(defun lex-@ (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\^)
           'sbst::@^)
          ((char= holdchar #\<)
           'sbst::@<)
          ((char= holdchar #\>)
           'sbst::@>)
          (t
           (lexical-unread-char stream)
           'sbst::@)))) 
(defun lex-/ (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\-)
           'sbst::/-)
          ((char= holdchar #\+)
           'sbst::/+)
          (t
           (lexical-unread-char stream)
           (illegal-token-error "/")
           :illegal-token)))) 
(defun lex-! (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\+)
           'sbst::!+)
          ((char= holdchar #\-)
           'sbst::!-)
          (t
           (lexical-unread-char stream)
           (illegal-token-error "!")
           :illegal-token)))) 
(defun lex-* (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\*)
           'sbst::**)
          (t
           (lexical-unread-char stream)
           'sbst::*)))) 
(defun lex-+ (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\+)
           'sbst::++)
          (t
           (lexical-unread-char stream)
           'sbst::+)))) 
(defun |lex-:| (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\:)
           (setf holdchar (lexical-read-char stream :eof))
           (if (and sb-escape-char (char= holdchar sb-escape-char))
               (setf holdchar (lexical-read-char stream :eof)))
           (cond ((char= holdchar #\=)
                  'sbst::|::=|)
                 (t
                  (lexical-unread-char stream)
                  (illegal-token-error "::")
                  :illegal-token)))
          (t
           (lexical-unread-char stream)
           'sbst::|:|)))) 
(defun lex-< (stream symbol)
  (declare (ignore symbol))
  (let (holdchar)
    (setf holdchar (lexical-read-char stream :eof))
    (if (and sb-escape-char (char= holdchar sb-escape-char))
        (setf holdchar (lexical-read-char stream :eof)))
    (cond ((char= holdchar #\<)
           'sbst::<<)
          (t
           (lexical-unread-char stream)
           'sbst::<)))) 
