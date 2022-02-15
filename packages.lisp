(in-package :cl-user)

#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :loop))

;; #+:sbcl
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :sb-rotate-byte)
;;   (require :sb-cltl2))

;; ;; These come from quicklisp/asdf
;; (require :babel)
;; (require :clack)
;; (require :websocket-driver)

(defpackage #:pvs
  (:use :cl-user :cl :ergolisp :oper :occ :term :sort
	:sb-runtime :lang :newattr :anaphora
	:make #+:sbcl :sb-pcl)
  #+:sbcl
  (:shadow :type :declaration :number :debug :class :keyword
	   :condition :parse-error :type-error)
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
  (:export #:*newline-comments*
	   #:*proofstate-indent*
	   #:*proofstate-width*
	   #:*ps-control-info*
	   #:*pvs-buffer-hook*
	   #:*pvs-dialog-hook*
	   #:*pvs-error-hook*
	   #:*pvs-message-hook*
	   #:*pvs-query-hook*
	   #:*pvs-warning-hook*
	   #:*pvs-y-or-n-hook*
	   #:add-psinfo
	   #:args1
	   #:args2
	   #:assq
	   #:comment
	   #:current-goal
	   #:expr
	   #:format-printout
	   #:formula
	   #:get-proof-scripts
	   #:hidden-s-forms
	   #:lisp
	   #:memq
	   #:names-info-proof-formula
	   #:neg-s-forms
	   #:neg-s-forms*
	   #:negation?
	   #:output-proofstate
	   #:parent-proofstate
	   #:parse-error
	   #:path-from-top
	   #:pc-complete
	   #:place
	   #:place-list
	   #:pos-s-forms
	   #:pos-s-forms*
	   #:pp-with-view
	   #:proofstate-num-subgoals
	   #:protect-emacs-output
	   #:psinfo-cmd-gate
	   #:psinfo-command
	   #:psinfo-json-result
	   #:psinfo-lock
	   #:psinfo-res-gate
	   #:pvs-abort
	   #:pvs-abort
	   #:pvs-buffer
	   #:pvs-display
	   #:pvs-emacs-eval
	   #:pvs-error
	   #:pvs-locate
	   #:pvs-message
	   #:pvs-query
	   #:pvs-warning
	   #:pvs-yn
	   #:pvs2json
	   #:s-forms
	   #:set-pvs-tmp-file
	   #:strim
	   #:theory-elt
	   #:type-ambiguity
	   #:type-error
	   #:type-expr
	   #:type-incompatible
	   #:view
	   #:write-to-temp-file
	   #:xmlrpc-output-proofstate
	   ;; macros.lisp
	   #:add-to-alist
	   #:cam
	   #:do-all-theories
	   #:get-declarations
	   #:get-importings
	   #:length=
	   #:makesym
	   #:put-decl
	   #:singleton?
	   #:with-context
	   #:with-pvs-file
	   #:with-workspace
	   ;; pvs-emacs.lisp
	   #:*pvs-buffer-hook*
	   #:*pvs-message-hook*
	   #:*pvs-warning-hook*
	   #:*pvs-error-hook*
	   #:*pvs-y-or-n-hook*
	   #:*pvs-query-hook*
	   #:*pvs-dialog-hook*
	   #:pvs-buffer
	   #:pvs-error
	   #:pvs-warning
	   #:pvs-message
	   #:pvs-display
	   #:pvs-abort
	   #:pvs-yn
	   #:pvs-query
	   #:pvs-emacs-eval
	   #:output-proofstate
	   #:pvs2json
	   #:protect-emacs-output
	   #:parse-error
	   #:type-error
	   #:set-pvs-tmp-file
	   #:place
	   #:place-list
	   #:type-ambiguity
	   #:type-incompatible
	   #:pvs-locate
	   #:write-to-temp-file
	   #:psinfo-json-result
	   #:psinfo-command
	   #:pvs-abort
	   #:lisp
	   #:add-psinfo
	   #:pvs-error
	   #:message
	   #:error-string
	   #:file-name
	   ;; utils.lisp
	   #:argument*
	   #:assq
	   #:bytestring-to-string
	   #:copy-all
	   #:copy-context
	   #:current-declarations-hash
	   #:current-theory
	   #:current-theory-name
	   #:current-using-hash
	   #:file-older
	   #:find-supertype
	   #:formals
	   #:get-theory
	   #:iso8601-date
	   #:lf
	   #:make-default-proof
	   #:make-new-context
	   #:mapappend
	   #:next-proof-id
	   #:operator*
	   #:prover-status
	   #:put-decl
	   #:pvs-current-directory
	   #:pvs-git-description
	   #:pvs-meta-info
	   #:ref-to-id
	   #:resolution
	   #:show
	   #:tc-term
	   ;; pvs.lisp
	   #:prover-init
	   #:prover-step
	   ;; gensubst.lisp
	   #:gensubst
	   #:gensubst*
	   #:mapobject
	   #:mapobject*
	   ))

;; From interface/pvs-json-rpc.lisp
(defpackage #:pvs-jsonrpc
  (:use :cl-user :json :common-lisp)
  (:export #:*interrupted-rpc*
	   #:finish-proofstate-rpc-hook
	   #:process-jsonrpc
	   #:pvs2json-response
	   #:rpc-output-notify-proof-success
	   #:update-ps-control-info-result
	   #:send-response
	   ))

(defpackage :pvs-websocket
  (:use :cl-user)
  (:nicknames :pvs-ws)
  (:export #:start-pvs-server
	   #:start-proof-server
	   #:ws-current-connection
	   ))
