;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eval-utils.lisp -- 
;; Author          : N. Shankar and Sam Owre
;; Created On      : Thu May 20 16:21:00 2004
;; Last Modified By: Sam Owre
;; Last Modified On: Thu May 20 16:21:44 2004
;; Update Count    : 1
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

(in-package :pvs) 

(defun mk-translate-cases-to-if (cases-expr)
  (mk-translate-cases-to-if* (expression cases-expr)
			     (selections cases-expr)
			     (else-part  cases-expr)))

(defun mk-translate-cases-to-if* (expr selections else-part &optional chained?)
  (cond ((and (null (cdr selections))
	      (null else-part))
	 (mk-subst-accessors-in-selection expr (car selections)))
	((null selections)
	 else-part)
	(t (let* ((sel (car selections))
		  (thinst (module-instance (find-supertype (type expr))))
		  (theory (module (declaration (find-supertype (type expr)))))
		  (rec (subst-mod-params (recognizer (constructor sel)) thinst theory))
		  (cond (make!-application rec expr))
		  (then (mk-subst-accessors-in-selection expr sel))
		  (else (mk-translate-cases-to-if* expr (cdr selections)
						   else-part t)))
	     (if chained?
		 (make!-chained-if-expr cond then else)
		 (make!-if-expr cond then else))))))

(defun mk-subst-accessors-in-selection (expr sel)
  (let* ((thinst (module-instance (find-supertype (type expr))))
	 (theory (module (declaration (find-supertype (type expr)))))
	 (accs (subst-mod-params (accessors (constructor sel)) thinst theory))
	 (vars (args sel))
	(selexpr (expression sel)))
    (substit selexpr
      (pairlis vars
	       (mapcar #'(lambda (acc) (make!-application acc expr))
		       accs)))))

(defmethod print-object ((obj eval-info) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~@<#<eval-info ~2I~_~0:Iir: ~a~:@_cdefn: ~a~:@_c-type-info-table: ~a~:@_internal: ~W~:@_external: ~W>~:>"
	(ir obj) (cdefn obj) (c-type-info-table obj)
	(internal obj) (external obj))))

(defmethod print-object ((obj eval-defn-info) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream
	  "~@<#<eval-defn-info ~2I~_~0:Iunary:       ~W~:@_multiary:    ~W~:@_destructive: ~W>~:>"
	(unary obj) (multiary obj) (destructive obj))))

(defmethod print-object ((obj eval-defn) stream)
  (if *debugging-print-object*
      (call-next-method)
      (format stream "~@<#<eval-defn ~W>~:>" (name obj))))

;; These should only be necessary for debugging

(defun clear-all-eval-infos ()
  "Walks down all theories in the current workspace session, including the prelude
and libraries that have been loaded, and calls clear-theory-eval-infos."
  (clrhash *c-primitive-type-attachments-hash*)
  (do-all-theories #'clear-theory-eval-infos))

(defun clear-theory-eval-infos (th)
  "Clears the eval-info slots of all const-decls of the theory th."
  (when (module? th)
    (setf (ht-instance-clone th) nil))
  (dolist (decl (all-decls th))
    (when (typep decl '(or formal-const-decl const-decl))
      (setf (eval-info decl) nil))))
