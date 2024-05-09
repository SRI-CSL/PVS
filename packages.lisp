(in-package :cl-user)
(export '(*ess-path*))

#+:allegro
(require :loop)

#+:allegro
(require :asdf)

#+:sbcl
(require :sb-md5)
#+:sbcl
(require :sb-cltl2)
#+:sbcl
(require :sb-aclrepl)

(eval-when (:execute :load-toplevel)
  (unless (find-package :cl-json)
    (ql:quickload :cl-json))
  (unless (find-package :babel)
    (ql:quickload :babel))
  (unless (find-package :fast-websocket)
    (ql:quickload :fast-websocket))
  (unless (find-package :clack)
    (ql:quickload :clack))
  (unless (find-package :hunchentoot)
    (ql:quickload :hunchentoot))
  (unless (find-package :websocket-driver)
    (ql:quickload :websocket-driver))
  (unless (find-package :lparallel)
    (ql:quickload :lparallel))
  (unless (find-package :anaphora)
    (ql:quickload :anaphora))
  (unless (find-package :cffi)
    (ql:quickload :cffi))
  ;; Causes problems with closer-mop-packages
  #-sbcl
  (unless (find-package :trivial-timer)
    (ql:quickload :trivial-timer))
  )

;; #+:sbcl
;; (require :sb-rotate-byte)
;; (require :sb-cltl2)

;;; From ergolisp (ess/) parser generator
(defpackage :ergolisp
  (:use :common-lisp)
  (:export #:*type-check-exceptions*
	   #:Alist-of
	   #:Boolean
	   #:Function-of
	   #:Hash-table-of
	   #:List-of
	   #:Nat
	   #:Optional
	   #:Ref
	   #:Sequence-of
	   #:all-constructors
	   #:boolean-equal
	   #:clet*
	   #:dcase
	   #:ddefun
	   #:declare-constructor
	   #:declare-ftype
	   #:def-disksave-hook
	   #:defconstr
	   #:defmulticonstr
	   #:defreconstr
	   #:defsconstr
	   #:defstype
	   #:dlambda
	   #:dlet
	   #:dlet*
	   #:eexport
	   #:ergo-disksave
	   #:fdlambda
	   #:mover
	   ;; #:memq
	   #:pop2
	   #:proclaim-ftype
	   #:push2
	   #:re-acons
	   #:re-cons
	   #:re-mapcar
	   #:tdefun
	   #:tdefvar
	   #:type-check
	   #:type-check-functions
	   #:type-uncheck
	   #:undeclare-ftype
	   #:|#>|
	   )
  )

(defpackage :oper
  (:use :common-lisp :ergolisp)
  (:export #:check-oper-type
	   #:ds-cid-op
	   #:ds-id-op
	   #:ds-key-op
	   #:ds-leaf-op
	   #:ds-lit-op
	   #:ds-num-op
	   #:ds-sim-op
	   #:ds-str-op
	   #:is-cid-op
	   #:is-id-op
	   #:is-key-op
	   #:is-leaf-op
	   #:is-lit-op
	   #:is-num-op
	   #:is-sim-op
	   #:is-str-op
	   #:leaf-op-kind
	   #:leaf-op-value
	   #:mk-cid-op
	   #:mk-id-op
	   #:mk-key-op
	   #:mk-leaf-op
	   #:mk-lit-op
	   #:mk-num-op
	   #:mk-sim-op
	   #:mk-str-op
	   #:oper
	   #:oper-equal
	   #:oper-to-sexp
	   #:operp
	   #:sexp-to-oper
	   )
  )

(defpackage :occ
  (:use :common-lisp :ergolisp)
  (:export #:check-occ-type
	   #:is-empty-occ
	   #:list-to-occ
	   #:make-empty-occ
	   #:nil-occ
	   #:null-occ
	   #:occ
	   #:occ-diff
	   #:occ-equal
	   #:occ-parent
	   #:occ-path
	   #:occ-push
	   #:occ-push0
	   #:occ-push1
	   #:occ-push2
	   #:occ-push3
	   #:occ-push4
	   #:occ-push5
	   #:occ-push6
	   #:occ-push7
	   #:occ-push8
	   #:occ-push9
	   #:occ-rest
	   #:occ-to-list
	   #:occ-top
	   #:occp
	   )
  )

(defpackage :ergo-system 
  (:use :common-lisp :ergolisp)
  (:export :ergo-disksave :def-disksave-hook :mover))

(defpackage :ergo-types
  (:use :common-lisp :ergolisp)
  (:export :Nat :Optional :Ref :boolean-equal :List-of
	   :Sequence-of :Function-of :Alist-of :Hash-table-of))

(defpackage :sbst
  (:nicknames :sb-st :sb-symbol-table))

(defpackage :tdefun
  (:use :common-lisp :ergolisp)
  (:export :tdefun :tdefvar :defstype))

(defpackage :type-check
  (:use :common-lisp :ergolisp)
  (:export :declare-ftype :proclaim-ftype :undeclare-ftype
	   :type-check :type-check-functions :type-uncheck
	   :*type-check-exceptions*))

(defpackage :print-utils
  (:use :common-lisp :ergolisp)
  (:export :print-struct :writing-readably))

(defpackage :language
  (:use :common-lisp :ergolisp)
  (:export :op-sort :check-op :subsort-p :language-sortlist :sublanguage-p))

(defpackage :dlambda-lib
  (:use :common-lisp :ergolisp)
  (:export :re-cons :re-acons :re-mapcar))

(defpackage :dlambda
  (:use :common-lisp :ergolisp)
  (:export :dlambda :fdlambda :ddefun :dlet :dlet* :dcase
	   :declare-constructor :defreconstr :all-constructors))

(defpackage :defsconstr
  (:use :common-lisp :ergolisp)
  (:export #:defsconstr))

(defpackage :constr-term-rep
  (:use :common-lisp :ergolisp))

(defpackage :constr
  (:use :common-lisp :ergolisp)
  (:export #:defconstr
	   #:defmulticonstr
	   #:proclaim-constrs-inline
	   )
  )

(defpackage :term
  (:nicknames :gterm)
  (:use :common-lisp :ergolisp :oper :occ)
  (:export #:*cache-ds-term-p*
	   #:*default-ds-term-impl*
	   #:*default-get-term-attr-impl*
	   #:*default-mk-term-impl*
	   #:*default-set-term-attr-impl*
	   #:*disable-auto-unparsing*
	   #:attr-clear-all
	   #:attr-clear-one
	   #:ck-term
	   #:ck-term-op
	   #:ck-term-sop
	   #:clear-ds-term
	   #:defrep
	   #:defrep-case
	   #:defrep-case-oneway
	   #:defrep-cases
	   #:defrep-cases-oneway
	   #:defrep-oneway
	   #:ds-cid
	   #:ds-id
	   #:ds-keyword
	   #:ds-literal
	   #:ds-number
	   #:ds-string
	   #:ds-term
	   #:ds-term-impl
	   #:expand-constr-cases
	   #:get-subterm
	   #:get-term-attr-impl
	   #:is-cid
	   #:is-id
	   #:is-keyword
	   #:is-leaf-term
	   #:is-literal
	   #:is-number
	   #:is-op
	   #:is-sim-term
	   #:is-sop
	   #:is-string
	   #:leaf-term-kind
	   #:leaf-term-value
	   #:memq-sop
	   #:mk-cid
	   #:mk-id
	   #:mk-keyword
	   #:mk-leaf-term
	   #:mk-literal
	   #:mk-number
	   #:mk-sim-term
	   #:mk-string
	   #:mk-term
	   #:mk-term-impl
	   #:nil-get-term-attr-impl
	   #:nil-set-term-attr-impl
	   #:op-arglist
	   #:op-args
	   #:oper-sort
	   #:print-term-hook
	   #:replace-argn
	   #:replace-subterm
	   #:sbrt-print-term-hook
	   #:search-list-for-term
	   #:search-list-for-term-with-subterm
	   #:set-term-attr
	   #:set-term-attr-impl
	   #:sexp-to-term
	   #:sim-term-op
	   #:standard-print-term
	   #:term
	   #:term-arg0
	   #:term-arg1
	   #:term-arg2
	   #:term-arg3
	   #:term-arg4
	   #:term-arg5
	   #:term-arg6
	   #:term-arg7
	   #:term-arg8
	   #:term-arg9
	   #:term-argn
	   #:term-args
	   #:term-arity
	   #:term-arity0?
	   #:term-arity1?
	   #:term-arity2?
	   #:term-arity3?
	   #:term-arity4?
	   #:term-arity5?
	   #:term-arity6?
	   #:term-arity7?
	   #:term-arity8?
	   #:term-arity9?
	   #:term-attr
	   #:term-equal
	   #:term-op
	   #:term-to-sexp
	   #:termp
	   #:trace-rep-functions
	   #:untrace-rep-functions
	   ))

(defpackage :sort
  (:use :common-lisp :ergolisp :oper)
  (:export #:add-ttype-to-sort
	   #:check-opsig-type
	   #:check-ttype-type
	   #:ds-id-ttype
	   #:ds-list-ttype
	   #:ds-lit-ttype
	   #:ds-num-ttype
	   #:ds-op-ttype
	   #:ds-sort-ttype
	   #:ds-str-ttype
	   #:ds-union-ttype
	   #:is-elist-ttype
	   #:is-id-ttype
	   #:is-list-ttype
	   #:is-lit-ttype
	   #:is-null-ttype
	   #:is-num-ttype
	   #:is-op-ttype
	   #:is-sort-ttype
	   #:is-str-ttype
	   #:is-union-ttype
	   #:make-opsig-table
	   #:make-sort-table
	   #:mk-elist-ttype
	   #:mk-id-ttype
	   #:mk-list-ttype
	   #:mk-lit-ttype
	   #:mk-null-ttype
	   #:mk-num-ttype
	   #:mk-op-ttype
	   #:mk-opsig
	   #:mk-sort-ttype
	   #:mk-str-ttype
	   #:mk-union-ttype
	   #:opsig
	   #:opsig-arity
	   #:opsig-equal
	   #:opsig-inputs
	   #:opsig-lang-name
	   #:opsig-output
	   #:opsig-table-contents
	   #:opsig-table-delete
	   #:opsig-table-insert
	   #:opsig-table-lookup
	   #:opsig-to-sexp
	   #:opsigp
	   #:sexp-to-opsig
	   #:sexp-to-ttype
	   #:sort-table-contents
	   #:sort-table-delete
	   #:sort-table-insert
	   #:sort-table-lookup
	   #:ttype
	   #:ttype-equal
	   #:ttype-overlap?
	   #:ttype-to-sexp
	   #:ttypep
	   ))

(defpackage :lang
  (:use :common-lisp :ergolisp)
  (:export #:*porting-system*
	   #:ck-lang-type
	   #:coerce-find-lang
	   #:compile-lang
	   #:compile-load-lang
	   #:find-lang
	   #:get-lang-abs-syn-package
	   #:get-lang-abs-syn-package-name
	   #:get-lang-code-package
	   #:get-lang-code-package-name
	   #:get-lang-conc-name
	   #:get-lang-grammar-file
	   #:get-lang-info-file
	   #:get-lang-lexer-file
	   #:get-lang-name
	   #:get-lang-opsig-table-name
	   #:get-lang-parse-routine-name
	   #:get-lang-parser-file
	   #:get-lang-sort-table-name
	   #:get-lang-sorts-file
	   #:get-lang-sub-languages
	   #:get-lang-unparse-nts
	   #:get-lang-unparse-routine-name
	   #:get-lang-unparser-file
	   #:get-lang-use-packages
	   #:get-lang-win-unparse-routine-name
	   #:get-lang-working-dir
	   #:interactively-define-lang
	   #:lang
	   #:lang-abs-syn-package
	   #:lang-abs-syn-package-name
	   #:lang-code-package
	   #:lang-code-package-name
	   #:lang-conc-name
	   #:lang-define
	   #:lang-grammar-file
	   #:lang-info-file
	   #:lang-lexer-file
	   #:lang-name
	   #:lang-opsig-table-name
	   #:lang-parse-routine-name
	   #:lang-parser-file
	   #:lang-sort-table-name
	   #:lang-sorts-file
	   #:lang-sub-languages
	   #:lang-unparse-nts
	   #:lang-unparse-routine-name
	   #:lang-unparser-file
	   #:lang-use-packages
	   #:lang-win-unparse-routine-name
	   #:lang-working-dir
	   #:langp
	   #:load-lang
	   #:load-lang-source
	   #:read-lang
	   #:rec-compile-lang
	   #:write-lang
	   #:write-reduced-lang
	   ))

(defpackage :retry
  (:use :common-lisp :ergolisp)
  (:export #:*catchers*
	   #:retry #:retry-catch
	   #:reinit-retry-catch))

(defpackage :tools
  (:use :common-lisp :ergolisp :retry)
  (:import-from :cl-user :*ess-path*)
  (:export #:*box-debug*
	   #:*box-edit-query*
	   #:*box-messages*
	   #:*box-warn*
	   #:*compiler-messages*
	   #:*ess-feature-alist*
	   #:*ess-name*
	   #:*ess-temp-name*
	   #:*ess-version*
	   #:*ess-version-date*
	   #:*load-messages*
	   #:*null-output*
	   #:*plain-readtable*
	   #:*time-function*
	   #:all-boxes
	   #:all-crates
	   #:all-efiles
	   #:all-suffixes
	   #:box-local-opts
	   #:box-maintainers
	   #:box-needs
	   #:box-path
	   #:boxclear
	   #:boxflag
	   #:boxforce
	   #:boxgen
	   #:boxinit
	   #:boxload
	   #:boxload-force
	   #:build-ess-lisp
	   #:cd
	   #:crategen
	   #:crateload
	   #:crateload-force
	   #:defbox
	   #:eflag
	   #:eforce
	   #:egen
	   #:eload
	   #:eload-force
	   #:ergolisp
	   #:generate-ess-lisp
	   #:getenv
	   #:list-all-crates
	   #:list-box-files
	   #:list-boxes-in-crates
	   #:list-tar-crates
	   #:load-ess-lisp
	   #:locate-efile
	   #:nice-time-string
	   #:pop2
	   #:print-all-boxes
	   #:print-all-crates
	   #:print-all-efiles
	   #:print-all-suffixes
	   #:push2
	   #:recompile-ess-lisp
	   #:regenerate-with
	   #:save-ess-lisp
	   #:sb
	   #:sys-parameters-string
	   #:user-name
	   #:wdir
	   #:while-connected-to
	   #:windowsystem
	   #:|#>|
	   ))

(defpackage :sb-runtime
  (:nicknames :rt-sb :rtsb :sb-rt :sbrt)
  (:use :common-lisp :ergolisp :oper :occ :term :sort :lang :print-utils)
  (:export
   #:*abs-syn-package*
   #:*apply-lex-term-constr-fun*
   #:*apply-lex-term-destr-fun*
   #:*apply-lex-term-discr-fun*
   #:*apply-lt-des-fun*
   #:*apply-lt-dis-fun*
   #:*apply-lt-token-cons-fun*
   #:*as-stack*
   #:*ask-about-bad-tokens*
   #:*bracket-info*
   #:*case-sensitive*
   #:*close-comment-char*
   #:*current-print-depth*
   #:*current-print-length*
   #:*default-char-width*
   #:*delexical-stream*
   #:*disable-aw-caching*
   #:*disable-caching*
   #:*disable-uterm-caching*
   #:*escape-character*
   #:*formatting-off*
   #:*hold-a1*
   #:*hold-a2*
   #:*hold-a3*
   #:*hold-a4*
   #:*hold-b1*
   #:*hold-b2*
   #:*hold-b3*
   #:*hold-b4*
   #:*key-esc-token-map*
   #:*key-token-map*
   #:*keyword-char*
   #:*keyword-list*
   #:*keyword-table*
   #:*lexical-stream*
   #:*literal-char*
   #:*lt-esc-token-map*
   #:*lt-token-map*
   #:*multi-char-op-list*
   #:*new-line-comment-char*
   #:*no-escapes*
   #:*open-comment-char*
   #:*parens-off*
   #:*parser-abort-threshold*
   #:*parser-error*
   #:*parser-error-count*
   #:*parser-return-errors*
   #:*pat-nesting*
   #:*prec-info*
   #:*reader-fun*
   #:*restricted-chars*
   #:*sb-format-warnings?*
   #:*sb-print-depth*
   #:*sb-print-length*
   #:*sbst-package*
   #:*single-char-op-list*
   #:*spacing-info*
   #:*string-char*
   #:*unparse-style*
   #:*unparser-op-list*
   #:*uterm*
   #:*uterm-bp-count*
   #:*uterm-nt-name*
   #:*uterm-son-count*
   #:*zero-space-bp*
   #:alt-parse
   #:apply-lexical-terminal-constructor
   #:apply-lexical-terminal-destructor
   #:apply-lexical-terminal-discriminator
   #:apply-lt-token-constructor
   #:as-peek
   #:as-pop
   #:as-push
   #:as-push-args
   #:as-to-slot
   #:aw-children
   #:aw-maxwidth
   #:aw-oct
   #:aw-outstring
   #:aw-parent
   #:aw-term
   #:botdent
   #:botliney
   #:boty
   #:bp-crs
   #:bp-format
   #:bp-p
   #:bp-spaces
   #:bp-united-flag
   #:bp-value
   #:bracket-list
   #:bracket-nesting-const
   #:bracket-uterm #:dis-lt
   #:ck-rept-ref
   #:clean-variables
   #:close-delexer
   #:code-to-slot
   #:cons-to-slot
   #:constructor-to-slot
   #:cr-token
   #:dis-alt
   #:dis-op
   #:dis-opt
   #:do-syntax-error
   #:doubleplus-parse
   #:doublestar-parse
   #:ds-temp-rept
   #:eat-brackets
   #:ellipsis-token
   #:empty-bracket-list
   #:epsilon
   #:error-peek-token
   #:flush-lexer
   #:format-uterm
   #:full-la-match
   #:gen-star-parse
   #:get-term-args
   #:gettoken
   #:gobble-to-slot
   #:gobble-token
   #:height
   #:illegal-token-error
   #:init-as-stack
   #:init-delexer
   #:init-keyword-table
   #:init-lexical-readtable
   #:init-nt-uterm
   #:init-prec-info
   #:initial-error
   #:initials
   #:initials-only
   #:insert-escapes?
   #:is-lexical-escape?
   #:is-nt-opsig?
   #:la-match
   #:lam
   #:lam-error
   #:lbp
   #:leading-epsilon-p
   #:leftx
   #:lexical-clear-input
   #:lexical-make-escape
   #:lexical-make-macro
   #:lexical-read
   #:lexical-read-char
   #:lexical-unread-char
   #:look-ahead-match
   #:make-bp
   #:make-sb-term
   #:make-slot-array
   #:make-spec-bp
   #:make-token
   #:make-uterm-lt
   #:make-var-name
   #:match-id
   #:match-lit
   #:match-num
   #:match-str
   #:medial-error
   #:medials
   #:memo-aw
   #:memo-uterm
   #:mk-keyword-token
   #:mk-null
   #:mk-seq-bp
   #:mk-temp-plus
   #:mk-temp-rept
   #:mk-temp-star
   #:mk-unp-rept
   #:not-empty-bracket-list
   #:nt-unp
   #:oct-points
   #:open-lexical-stream
   #:open-placestream
   #:opt-parse
   #:parser-abort-catch
   #:parser-error
   #:peek-first
   #:peek-second
   #:placestream
   #:plus-parse
   #:pop-bracket
   #:print-aw
   #:print-bp
   #:print-oct
   #:print-token
   #:print-uterm
   #:push-bracket
   #:queue-uterm-bp
   #:queue-uterm-son
   #:rbp
   #:read-keyword-string
   #:read-literal
   #:read-parse-term
   #:read-sb-string
   #:reader
   #:rightx
   #:sb-deffontheight
   #:sb-deffontwidth
   #:set-bracket-info
   #:set-prec-info
   #:slot-to-slot
   #:stack-brackets
   #:star-parse
   #:string-lexer
   #:tab-left-token
   #:tab-right-token
   #:term-to-aw
   #:term-to-uterm
   #:theaw
   #:theuterm
   #:token
   #:token-kind
   #:token-p
   #:token-str-value
   #:token-subkind
   #:token-value
   #:top-bracket
   #:topdent
   #:topliney
   #:topy
   #:toss-name
   #:toss-rept
   #:toss-value
   #:unindent-token
   #:unp-alt
   #:unp-alt-aug
   #:unp-base-id
   #:unp-base-lit
   #:unp-base-num
   #:unp-base-str
   #:unp-bcons
   #:unp-bind
   #:unp-cons
   #:unp-double-rept
   #:unp-elist-const
   #:unp-ext-nt
   #:unp-jux
   #:unp-jux-keyword
   #:unp-keyword
   #:unp-list
   #:unp-list-const
   #:unp-lt
   #:unp-name
   #:unp-nt
   #:unp-null-const
   #:unp-opt
   #:unp-opt-aug
   #:unp-rept
   #:unp-seq
   #:unp-term-const
   #:unp-uterm
   #:unparse-runtime-error
   #:unparse-term
   #:untab-token
   #:upats
   #:uterm
   #:uterm-to-aw
   #:value-to-slot
   #:vnil
   #:width
   ))

(defpackage :syntax-box
  (:nicknames :sb)
  (:use :common-lisp :ergolisp :oper :term :sort :sb-runtime :lang :ergo-system)
  (:export :sb :sb-make))

(defpackage #:newattr
  (:use #:common-lisp #:ergolisp)
  (:export #:defattr
	   #:defattr-attrfun
	   #:defattrfam
	   #:defattrfam-attrfun
	   #:defcon
	   #:defcon-deltafun
	   #:defconfam
	   #:defconfam-deltafun
	   #:defemptysyn
	   #:defgattr
	   #:defgattr-attrfun
	   #:defgcon
	   #:defgcon-deltafun
	   #:defgsyn
	   #:defgsyn-compfun
	   #:defsyn
	   #:defsyn-compfun
	   #:defsynfam
	   #:defsynfam-compfun
	   #:delta-con
	   #:delta-confam
	   #:delta-gcon
	   #:empty-gcon
	   #:empty-gsyn
	   #:get-attr
	   #:get-attr-at
	   #:get-attrfam
	   #:get-gattr
	   #:get-gsyn
	   #:get-gsyn-argn
	   #:get-syn
	   #:get-syn-argn
	   #:get-syn-at
	   #:get-synfam
	   #:get-synfam-argn
	   #:idsym
	   #:mvb
	   #:mvs
	   #:numval
	   #:revocc
	   #:sort
	   #:strval
	   #:termself
	   #:tocc
	   #:undefined
	   #:vls
	   ))

(defpackage :constrg
  (:use :common-lisp :ergolisp :newattr :lang :sb-runtime :sort :term :occ :oper
	:ergo-system)
  (:export :constr-parse))

(defpackage #:file-utils
  (:use #:common-lisp #:cffi)
  (:export #:file-exists-p #:directory-p #:read-permission?
	   #:write-permission? #:file-write-time #:get-file-info))

(defpackage :ergo-lisp
  (:use :common-lisp)
  (:export #:regression-test #:regression-test-only #:regression-test-opt
	   #:next-key #:next-value #:*regression-testing-p*
	   #:move-script #:regressible-error #:regressible-warn))

(defpackage :af-runtime-lib
  (:nicknames :abrt :afrt)
  (:use :common-lisp)
  (:export :opcase :argcase :rt-delta-error :rt-term-argn :rt-term-args
	   :rt-symbol :rt-ite :rt-opt :rt-function :rt-ast
	   #+termocc :rt-termocc #+termocc :rt-mk-termocc #+termocc :rt-termocc-argn
	   :rt-trap-constraint-errors :rt-constraint-check
	   :rt-first-child :rt-last-child :rt-nchild
	   :rt-defconfam :rt-defsynfam :rt-defattrfam
	   :rt-defconfam-deltafun :rt-defsynfam-compfun :rt-defattrfam-attrfun
	   :rt-delta-confam
	   :rt-get-synfam :rt-get-synfam-argn
	   :rt-opt-present :rt-get-synfam-list
	   :rt-get-synfam-opt :rt-get-synfam-argn-opt
	   :rt-get-iterate :rt-get-iterate-opt
	   :rt-get-attrfam
	   :rt-get-attrfam-at
	   :rt-mvb :rt-vls
	   ))


;;; PVS packages
(defpackage #:pvs
  ;;(:nicknames :PVS)
  (:use :cl-user :cl :ergolisp :oper :occ :term :sort
	:sb-runtime :lang :newattr :file-utils :anaphora
	#+:sbcl :sb-pcl)
  (:shadow :class :condition :debug :declaration :keyword :name :number
	   :parse-error :type :type-error :memq :var)
  (:import-from :file-utils)
  (:import-from :lparallel.queue
		:make-queue
		:peek-queue
		:peek-queue/no-lock
		:pop-queue
		:pop-queue/no-lock
		:push-queue
		:push-queue/no-lock
		:queue
		:queue-count
		:queue-count/no-lock
		:queue-empty-p
		:queue-empty-p/no-lock
		:queue-full-p
		:queue-full-p/no-lock
		:try-pop-queue
		:try-pop-queue/no-lock
		:with-locked-queue
		)
  (:export
   #:*boolean*
   #:*bound-variables*
   #:*current-context*
   #:*debugging-print-object*
   #:*even_int*
   #:*even_nat*
   #:*even_negint*
   #:*even_posnat*
   #:*false*
   #:*finish-proofstate-hooks*
   #:*generate-all-adt-axioms*
   #:*generate-tccs*
   #:*in-apply*
   #:*in-checker*
   #:*in-evaluator*
   #:*integer*
   #:*last-attempted-proof*
   #:*last-proof*
   #:*multiple-proof-default-behavior*
   #:*naturalnumber*
   #:*negint*
   #:*negrat*
   #:*newline-comments*
   #:*number*
   #:*number_field*
   #:*odd_int*
   #:*odd_negint*
   #:*odd_posnat*
   #:*posint*
   #:*posrat*
   #:*prelude*
   #:*prelude-context*
   #:*proofstate-hooks*
   #:*proofstate-indent*
   #:*proofstate-width*
   #:*prover-commentary*
   #:*ps*
   #:*pvs-fasl-type*
   #:*pvs-buffer-hook*
   #:*pvs-dialog-hook*
   #:*pvs-directories*
   #:*pvs-emacs-interface*
   #:*pvs-error-hook*
   #:*pvs-library-path*
   #:*pvs-lisp-process*
   #:*pvs-message-hook*
   #:*pvs-path*
   #:*pvs-query-hook*
   #:*pvs-tmp-file*
   #:*pvs-version*
   #:*pvs-warning-hook*
   #:*pvs-websocket-interface*
   #:*pvs-y-or-n-hook*
   #:*real*
   #:*rewrite-msg-off*
   #:*show-conversions*
   #:*success-proofstate-hooks*
   #:*tcc-conditions*
   #:*top-proofstate*
   #:*true*
   #:*typechecking-module*
   #:actual
   #:actuals
   #:add-judgement-decl
   #:add-psinfo
   #:add-psinfo
   #:add-to-alist
   #:adt-constructor
   #:adt-or-theory
   #:adtdecl
   #:all
   #:all-decls
   #:application
   #:args1
   #:args2
   #:argument
   #:argument*
   #:argument-conversions
   #:arguments
   #:assignment
   #:assignments
   #:assq
   #:assq
   #:assuming
   #:beta-reduce
   #:bind-decl
   #:binding-expr
   #:bindings
   #:boolean-equation
   #:branch
   #:bye
   #:bytestring-to-string
   #:cam
   #:chain?
   #:change-workspace
   #:check-arguments
   #:check-chained-syntax
   #:check-for-tccs*
   #:children
   #:clear-theories
   #:clear-workspace
   #:collect-justification
   #:collect-pvs-file-decls-info
   #:comment
   #:commentary
   #:compatible-preds
   #:compatible-type
   #:compatible?
   #:conjunction
   #:const-decl
   #:const-decl?
   #:constructors
   #:context
   #:context-entry-of
   #:context-file-of
   #:context-files-and-theories
   #:context-usingchain
   #:conversion-decl
   #:conversionplus-decl?
   #:conversions
   #:copy
   #:copy-all
   #:copy-context
   #:copy-lhash-table
   #:copy-theory-proofs-to-orphan-file
   #:current-context-path
   #:current-declarations-hash
   #:current-goal
   #:current-rule
   #:current-session
   #:current-theory
   #:current-theory-name
   #:current-using-hash
   #:datatype-or-module
   #:declaration
   #:declarations-hash
   #:declared-type
   #:default-proof
   #:defcl
   #:definition
   #:description
   #:directory-p
   #:disequation
   #:disjunction   
   #:do-all-theories
   #:domain
   #:enumtype
   #:equation
   #:error-string
   #:exit-pvs
   #:expr
   #:expression
   #:exprs
   #:extract-justification-sexp
   #:fe-id
   #:fe-status
   #:field-application
   #:field-decl
   #:fields
   #:file-exists-p
   #:file-name
   #:file-older
   #:file-write-time
   #:filename
   #:find-declaration
   #:find-supertype
   #:formal-const-decl
   #:formal-type-decl
   #:formals
   #:formals-sans-usings
   #:format-printout
   #:formula
   #:formula-decl
   #:formula-decl-to-prove
   #:free-formals
   #:free-params
   #:free-params*
   #:from-prelude?
   #:funtype
   #:generate-actuals-tcc
   #:generate-assuming-tccs
   #:generate-existence-tcc
   #:generate-recursive-tcc
   #:generate-subtype-tcc
   #:generated-by
   #:gensubst
   #:gensubst*
   #:get-context-theory-entry
   #:get-context-theory-names
   #:get-decl-resolutions
   #:get-declarations
   #:get-file-info
   #:get-formula-decl
   #:get-importings
   #:get-lhash
   #:get-proof-script
   #:get-proof-scripts
   #:get-proof-status
   #:get-pvs-file-dependencies
   #:get-pvs-version-information
   #:get-tccs
   #:get-term-at
   #:get-theory
   #:get-theory-dependencies
   #:get-typechecked-theory
   #:goto-declaration
   #:handle-deleted-theories
   #:hidden-s-forms
   #:id
   #:iff
   #:iff-or-boolean-equation
   #:implication
   #:importing
   #:index
   #:infix-application
   #:info
   #:inline-datatype
   #:inline-recursive-type
   #:interrupt-session
   #:intersection-type
   #:iso8601-date
   #:json-decl-list
   #:judgement?
   #:justification
   #:kind-of
   #:kind-of-name-expr
   #:label
   #:lambda-expr
   #:length=
   #:let-expr
   #:lf
   #:lhash-next
   #:lhash-table
   #:lisp
   #:lisp
   #:list-declarations
   #:make!-application
   #:make!-conjunction*
   #:make!-disjunction*
   #:make!-equation
   #:make!-field-application
   #:make-bind-decl
   #:make-default-proof
   #:make-formals-funtype
   #:make-lhash-table
   #:make-new-bindings
   #:make-new-context
   #:make-new-variable
   #:make-resolution
   #:make-variable-expr
   #:makesym
   #:map-lhash
   #:mapappend
   #:mapobject
   #:mapobject*
   #:memq
   #:message
   #:mk-application
   #:mk-bind-decl
   #:mk-const-decl
   #:mk-conversionplus-decl
   #:mk-field-decl
   #:mk-funtype
   #:mk-lambda-expr
   #:mk-modname
   #:mk-name-expr
   #:mk-number-expr
   #:mk-resolution
   #:mk-subtype
   #:mk-type-name
   #:modname
   #:module
   #:name
   #:name-expr
   #:names-info
   #:names-info-proof-formula
   #:neg-s-forms
   #:neg-s-forms*
   #:negation
   #:negation?
   #:next-proof-id
   #:none
   #:nonempty-types
   #:nonempty?
   #:number
   #:number-expr
   #:occurs-in
   #:operator
   #:operator*
   #:output-proofstate
   #:output-proofstate
   #:parens
   #:parent-proofstate
   #:parse-error
   #:parse-error
   #:parse-file
   #:path-from-top
   #:pc-complete
   #:pc-parse
   #:pc-typecheck
   #:place
   #:place
   #:place-list
   #:place-list
   #:pos-s-forms
   #:pos-s-forms*
   #:pp*
   #:pp-chained-decls
   #:pp-with-view
   #:predicate
   #:prettyprint-dedukti
   #:print-type
   #:projection-application
   #:proofs
   #:proofstate
   #:proofstate?
   #:proofstate-num-subgoals
   #:propax
   #:propositional-application
   #:protect-emacs-output
   #:protect-emacs-output
   #:prove-formula
   #:prove-tccs
   #:proved?
   #:prover-help
   #:prover-init
   #:prover-status
   #:prover-step
   #:ps-eq
   #:psinfo-cmd-gate
   #:psinfo-command
   #:psinfo-command
   #:psinfo-json-result
   #:psinfo-json-result
   #:psinfo-lock
   #:psinfo-res-gate
   #:ptype-of
   #:put-decl
   #:put-decl
   #:pvs-abort
   #:pvs-abort
   #:pvs-abort
   #:pvs-abort
   #:pvs-buffer
   #:pvs-buffer
   #:pvs-current-directory
   #:pvs-delete-proof
   #:pvs-display
   #:pvs-display
   #:pvs-emacs-eval
   #:pvs-emacs-eval
   #:pvs-error
   #:pvs-file-of
   #:pvs-git-description
   #:pvs-locate
   #:pvs-log
   #:pvs-message
   #:pvs-meta-info
   #:pvs-parse
   #:pvs-query
   #:pvs-rename-file
   #:pvs-select-proof
   #:pvs-view-proof
   #:pvs-warning
   #:pvs-yn
   #:pvs2json
   #:quant-expr
   #:quit-all-proof-sessions
   #:quit-command-p
   #:range
   #:read-permission?
   #:read-strategies-files
   #:recognizer
   #:record-expr
   #:recordtype
   #:recursive-type
   #:ref-to-id
   #:resolution
   #:resolutions
   #:resolve
   #:restore
   #:restore-context
   #:restore-from-context
   #:rpc-mode-debugger
   #:s-forms
   #:save-all-proofs
   #:save-context
   #:save-proofs
   #:saved-context
   #:script
   #:session-output
   #:set-dependent-formals
   #:set-lambda-dep-types
   #:set-pvs-tmp-file
   #:set-pvs-tmp-file
   #:set-type
   #:set-type*
   #:set-type-actuals
   #:set-visibility
   #:show
   #:show-context-path
   #:show-declaration
   #:show-expanded-form
   #:show-orphaned-proofs
   #:show-proof-file
   #:show-proofs-pvs-file
   #:show-tccs
   #:simple-constructor
   #:simple-name?
   #:singleton?
   #:status
   #:status-flag
   #:str
   #:strict-compatible?
   #:strict-subtype-of?
   #:strim
   #:subst-actuals-in-next-formal
   #:subst-for-formals
   #:substit #:tupletype
   #:substit-binding
   #:subtype
   #:subtype-of?
   #:subtype-pred
   #:subtype-preds
   #:supertype
   #:tc-eq
   #:tc-eq-with-bindings
   #:tc-term
   #:te-formula-info
   #:te-id
   #:theory
   #:theory-elt
   #:theory-name
   #:top-proofstate
   #:tuple-expr
   #:type
   #:type-ambiguity
   #:type-ambiguity
   #:type-canon
   #:type-decl
   #:type-def-decl
   #:type-error
   #:type-error
   #:type-expr
   #:type-incompatible
   #:type-incompatible
   #:type-name
   #:type-name?
   #:type-value
   #:typecheck
   #:typecheck*
   #:typecheck-decl
   #:typecheck-decls
   #:typecheck-file
   #:typecheck-uniquely
   #:typed-declaration
   #:types
   #:unique-ps-id
   #:unparse
   #:untypecheck-theory
   #:update-context
   #:update-expr
   #:using-hash
   #:valid-proofs-file
   #:var-decl
   #:view
   #:visible?
   #:whereis-declaration-used
   #:wish-current-rule
   #:wish-parent-proofstate
   #:with-context
   #:with-pvs-file
   #:with-workspace
   #:write-deferred-methods
   #:write-deferred-methods-to-file
   #:write-permission?
   #:write-pvs-version-file
   #:write-json-to-temp-file
   #:write-to-temp-file
   #:x-subgoals
   #:xmlrpc-output-proofstate
   ;;sb-runtime:*abs-syn-package*
   ))

;; From interface/pvs-json-rpc.lisp
(defpackage #:pvs-jsonrpc
  (:use #:cl-user #:json #:common-lisp)
  (:export #:*interrupted-rpc*
	   #:finish-proofstate-rpc-hook
	   #:process-jsonrpc
	   #:pvs-message-hook
	   #:pvs-y-or-n-hook
	   #:pvs2json-response
	   #:rpc-output-notify-proof-success
	   #:send-response
	   ))

(defpackage #:pvs-websocket
  (:use #:cl-user #:common-lisp)
  (:nicknames #:pvs-ws)
  (:export #:start-pvs-server
	   ;;#:start-proof-server
	   #:stop-pvs-server
	   #:ws-current-connection
	   ))

