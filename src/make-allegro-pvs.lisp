;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-allegro-pvs.lisp -- Invoked by build-pvs within make-pvs.lisp
;;                          used for Allegro build only
;; Author          : Sam Owre
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2006, SRI International.  All Rights Reserved.

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

(in-package :cl-user)
(defvar *pvs-path* (or (sys:getenv "PVSPATH") "."))
(eval-when (:load-toplevel :execute)
  (require :tpl-debug)
  #+(or runtime-standard runtime-dynamic)
  (pushnew :runtime *features*)
  #-(or runtime-standard runtime-dynamic)
  (require :prof)
  (require :ffcompat)
  (setq excl:*global-gc-behavior* :auto)
  ;;(load "~/quicklisp/setup.lisp")
  ;;(load (sys:getenv "QUICKLISP"))
  ;;(load (format nil "~a/pvs.system" *pvs-path*))
  (require :uri)
  (require :pxml-sax)
  (require :sock-s)
  ;;(require :smputil)
  (let ((excl:*enable-package-locked-errors* nil))
    ;; These are autoloaded functions - ed was accidentally used in a proof
    ;; and caused an unexpected error
    ;; (fmakunbound 'add-breakpoint)
    ;; (fmakunbound 'arrest-command)
    ;; (fmakunbound 'breakpoint-1)
    ;; (fmakunbound 'build-lisp-image)
    ;; (fmakunbound 'configure-dns)
    ;; (fmakunbound 'def-autoload-function)
    ;; (fmakunbound 'def-autoload-generic-function)
    ;; (fmakunbound 'def-autoload-macro)
    ;; (fmakunbound 'delete-breakpoint)
    ;; (fmakunbound 'disassemble)
    ;; (fmakunbound 'discard-local-name-info)
    ;; (fmakunbound 'dns-ipaddr-to-hostname)
    ;; (fmakunbound 'dns-lookup-hostname)
    ;; (fmakunbound 'dns-query)
    ;; (fmakunbound 'echo-stream-input-stream)
    ;; (fmakunbound 'echo-stream-output-stream)
    (fmakunbound 'ed)
    ;; (fmakunbound 'eol-convention)
    ;; (fmakunbound 'euc-to-string)
    ;; (fmakunbound 'find-composed-external-format)
    ;; (fmakunbound 'focus-command)
    ;; (fmakunbound 'ftp-transfer-file)
    ;; (fmakunbound 'ftrace)
    ;; (fmakunbound 'funtrace)
    ;; (fmakunbound 'funwrap)
    ;; (fmakunbound 'generate-application)
    ;; (fmakunbound 'generate-executable)
    ;; (fmakunbound 'generate-filled-ef-templates)
    ;; (fmakunbound 'generate-rsa-keys)
    ;; (fmakunbound 'get-autoload-function)
    ;; (fmakunbound 'get-autoload-macro)
    ;; (fmakunbound 'hmac-md5-final)
    ;; (fmakunbound 'hmac-md5-init)
    ;; (fmakunbound 'hmac-md5-string)
    ;; (fmakunbound 'hmac-md5-update)
    ;; (fmakunbound 'hmac-sha1-final)
    ;; (fmakunbound 'hmac-sha1-init)
    ;; (fmakunbound 'hmac-sha1-string)
    ;; (fmakunbound 'hmac-sha1-update)
    ;; (fmakunbound 'initialize-sigio-handling)
    ;; (fmakunbound 'inspect)
    ;; (fmakunbound 'install-breakpoints)
    ;; (fmakunbound 'kill-command)
    ;; (fmakunbound 'list-all-completions)
    ;; (fmakunbound 'list-all-completions-abbrev-search)
    ;; (fmakunbound 'list-all-completions-search)
    ;; (fmakunbound 'list-undefined-functions)
    ;; (fmakunbound 'list-unused-functions)
    ;; (fmakunbound 'locale-time-ut-to-format-control-args)
    ;; (fmakunbound 'make-echo-stream)
    ;; (fmakunbound 'make-function-input-stream)
    ;; (fmakunbound 'make-pipe-stream)
    ;; (fmakunbound 'make-ssl-client-stream)
    ;; (fmakunbound 'make-ssl-server-stream)
    ;; (fmakunbound 'make-two-way-stream)
    ;; (fmakunbound 'old-compiler-walk)
    ;; (fmakunbound 'open-ftp-stream)
    ;; (fmakunbound 'parse-ucet)
    ;; (fmakunbound 'prebuild-all-efs)
    ;; (fmakunbound 'process-code)
    ;; (fmakunbound 'processes-command)
    ;; (fmakunbound 'rc4)
    ;; (fmakunbound 'rc4-set-key)
    ;; (fmakunbound 'remove-sigio-handler)
    ;; (fmakunbound 'set-sigio-handler)
    ;; (fmakunbound 'sha1-file)
    ;; (fmakunbound 'sha1-final)
    ;; (fmakunbound 'sha1-init)
    ;; (fmakunbound 'sha1-string)
    ;; (fmakunbound 'sha1-update)
    ;; (fmakunbound 'sigio-supported-p)
    ;; (fmakunbound 'start-emacs-lisp-interface)
    ;; (fmakunbound 'step-1)
    ;; (fmakunbound 'string-sort-key)
    ;; (fmakunbound 'string-to-euc)
    ;; (fmakunbound 'two-way-stream-input-stream)
    ;; (fmakunbound 'two-way-stream-output-stream)
    ;; (fmakunbound 'unarrest-command)
    ;; (fmakunbound 'unicode-combining-class)
    ;; (fmakunbound 'unicode-nfd)
    ;; (fmakunbound 'update-allegro)
    ;; (fmakunbound 'use-background-streams)
    ;; (fmakunbound 'walk)
    ;; (fmakunbound 'who-references)
    ))
;;(mk:operate-on-system :pvs :compile)
(require :asdf)
(load "~/quicklisp/setup.lisp")
(push *default-pathname-defaults* asdf:*central-registry*)
(asdf:load-system :pvs)
(when (sys:getenv "PVSMAKELOADAFTER")
  (load (sys:getenv "PVSMAKELOADAFTER")))
