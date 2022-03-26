;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; save-theories.lisp -- provides the facilities for saving PVS theories.
;; Author          : Carl Witty (with mods by Sam Owre)
;; Created On      : Sun Aug 14 14:44:02 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Oct 30 11:36:10 1998
;; Update Count    : 5
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

(defvar *restore-objects-seen*)

(defvar *restore-object-hash*)

(defvar *restoring-type-application* nil)

(defvar *needs-pseudonormalizing* nil)

(defvar *bin-theories-set*)

(defvar *restoring-theory* nil)

(defvar *restoring-declaration*)

(defun save-theory (theory)
  (pvs-log "~%Saving ~a" (binpath-id theory))
  (store-object-to-file (cons *binfile-version* theory)
			(make-binpath (binpath-id theory))))

(defmethod get-theories-to-save (file)
  (mapcan #'get-theories-to-save (sort-theories (get-theories file))))

(defmethod get-theories-to-save ((th module))
  (list th))

(defmethod get-theories-to-save ((adt datatype))
  (append (adt-generated-theories adt) (list adt)))

(defvar *ignore-binfile-errors* t)

;;; Called from restore-theory in context.lisp
(defun get-theory-from-binfile (filename)
  "Gets a theory from the binfile, by fetching, then restoring.  Fetching
basically creates the instances, and fills in some slots.  Restoring fills
in the remaining slots, in particular, those that may reference other
instances, e.g., other declarations within the theory, or self-references."
  (let ((file (make-binpath filename))
	(start-time (get-internal-real-time))
	(*bin-theories-set* nil))
    (multiple-value-bind (vtheory fetch-error)
	(if *ignore-binfile-errors*
	    (ignore-lisp-errors (fetch-object-from-file file))
	    (fetch-object-from-file file))
      (let ((load-time (get-internal-real-time)))
	(cond ((and (consp vtheory)
		    (integerp (car vtheory))
		    (= (car vtheory) *binfile-version*))
	       (let* ((theory (cdr vtheory))
		      (*restore-object-hash* (make-hash-table :test #'eq))
		      (*restore-objects-seen* nil)
		      (*assert-if-arith-hash* (make-hash-table :test #'eq))
		      (*subtype-of-hash* (make-hash-table :test #'equal))
		      (*current-context* (make-new-context
					  (if (recursive-type? theory)
					      (adt-theory theory)
					      theory))))
		 (assert (current-theory))
		 (assert (module? (current-theory)))
		 (assert (judgement-types-hash (current-judgements)))
		 (restore-object theory)
		 (setf (context-path theory) (working-directory))
		 ;;(assert (eq (current-theory) theory))
		 ;;(assert (datatype-or-module? theory))
		 (restore-saved-context (saved-context theory))
		 (pvs-message
		     "Restored theory from ~a in ~,2,-3fs ~
                      (load part took ~,2,-3fs)"
		   file (realtime-since start-time)
		   (floor (- load-time start-time) millisecond-factor))
		 theory))
	      (fetch-error
	       ;;(break "Error in fetching ~a - ~a" filename fetch-error)
	       (pvs-message "~I~<Error in fetching ~a -~_ ~a~:>" filename fetch-error)
	       (ignore-lisp-errors (delete-file file))
	       (dolist (thid *bin-theories-set*)
		 (remhash thid (current-pvs-theories)))
	       nil)
	      (t (pvs-message "Bin file version for ~a is out of date"
		   filename)
		 (ignore-lisp-errors (delete-file file))
		 (dolist (thid *bin-theories-set*)
		   (remhash thid (current-pvs-theories)))
		 nil))))))

(defun typechecked-modules ()
  (let ((theory-list nil))
    (maphash #'(lambda (id mod)
		 (declare (ignore id))
		 (when (typechecked? mod)
		   (push mod theory-list)))
	     (current-pvs-theories))
    theory-list))


;; While we're saving a particular theory, we don't want to save any
;; other modules it might refer to, or any declarations that belong to
;; other modules.  *saving-theory* is bound to the theory being saved.
(defvar *saving-theory* nil)

(defvar *saving-declaration* nil)

(defmethod store-object* :around ((obj datatype-or-module))
  (if (typep obj '(not (or inline-recursive-type enumtype)))
      (if *saving-theory*
	  (if (external-library-reference? obj)
	      (reserve-space 4
		(push-word (store-obj 'modulelibref))
		(push-word (store-obj (id obj)))
		;; Take out the car/cdr, because fetch postpones conses
		(push-word (store-obj (get-library-id (context-path obj))))
		(push-word (store-obj (context-path obj))))
	      (reserve-space 2
		(push-word (store-obj 'moduleref))
		(push-word (store-obj (id obj)))))
	  (let ((*saving-theory* obj))
	    (call-next-method)))
      (call-next-method)))

(defmethod store-object* :around ((obj adt-type-name))
  (if (inline-recursive-type? (adt obj))
      (call-next-method)
      (progn
	(unless (recursive-type? (adt obj))
	  ;; May happen after restoring from bin files
	  (restore-adt-slot obj))
	(call-next-method (copy obj
			    'adt (if (external-library-reference? (adt obj))
				     (cons (context-path (adt obj))
					   (id (adt obj)))
				     (id (adt obj))))))))

(defmethod external-library-reference? ((obj datatype-or-module))
  (and (not (file-equal (context-path obj) *default-pathname-defaults*))
       (not (from-prelude? obj))))

(defmethod external-library-reference? (obj)
  (declare (ignore obj))
  nil)

;; (defmethod store-object* :around ((obj resolution))
;;   (assert (not (datatype? (get-theory (module-instance obj)))))
;;   (call-next-method))

;; (defmethod store-object* :around ((obj resolution))
;;   (assert (not (and (null (library (module-instance obj)))
;; 		    (lib-datatype-or-theory?
;; 		     (module (declaration obj))))))
;;   (call-next-method))

(defmethod store-object* :around ((obj list-expr))
  (with-slots (place parens type) obj
    (let* ((len (list-expr-length obj))
	   (list-ex obj))
      ;;(format t "~%Saving ~d list elts" len)
      (reserve-space (+ len 5)
	(push-word (store-obj 'list-expr-array))
	(push-word (store-obj len))
	(push-word (store-obj place))
	(push-word (store-obj parens))
	(push-word (store-obj type))
	(loop while (list-expr? list-ex)
	   do (progn (push-word (store-obj (args1 list-ex)))
		     (setq list-ex (args2 list-ex))))))))

(setf (get 'list-expr-array 'fetcher) 'fetch-list-expr-array)
(defun fetch-list-expr-array ()
  (let* ((len (fetch-obj (stored-word 1)))
	 (place (fetch-obj (stored-word 2)))
	 (parens (fetch-obj (stored-word 3)))
	 (type (fetch-obj (stored-word 4))))
    ;;(format t "~%fetching ~d elts" len)
    (let ((list-ex (mk-null-expr)))
      (when place
	(setf (place list-ex)
	      (vector (ending-row place) (ending-col place)
		      (ending-row place) (ending-col place))))
      (dotimes (i len)
	;; work backwards
	(let ((elt (fetch-obj (stored-word (+ len 4 (- i))))))
	  (setq list-ex (mk-list-expr elt list-ex))))
      (setf (place list-ex) place)
      (setf (parens list-ex) parens)
      (setf (type list-ex) type)
      list-ex)))

(defvar *restoring-module* nil
  "used to keep from recursing through the following")

(defmethod restore-object* :around ((obj module))
  (unless (eq obj *restoring-module*)
    (let ((*restoring-module* obj))
      (call-next-method))
    (dolist (pair (nonempty-types obj))
      (let ((*restore-object-parent* pair)
	    (*restore-object-parent-slot* 'car)
	    (type (car pair))
	    (*restoring-declaration* (cdr pair)))
	(with-current-decl *restoring-declaration*
	  (restore-object* type))))
    (dolist (ass-inst (assuming-instances obj))
      (let ((*restore-object-parent* ass-inst)
	    (thinst (car ass-inst))
	    (expr (cadr ass-inst))
	    (*restoring-declaration* (caddr ass-inst)))
	(when *restoring-declaration*
	  (let ((*restore-object-parent-slot* 'car))
	    (restore-object* thinst))
	  (let ((*restore-object-parent-slot* 'cadr))
	    (restore-object* expr)))))))


(defmethod restore-object* :around ((obj list-expr))
  (let ((*restore-object-parent* obj))
    (when (type obj)
      (let ((*restore-object-parent-slot* 'type))
	(restore-object* (type obj))))
    (let* ((*generate-tccs* 'none)
	   (list-ex obj)
	   (elt-type (type-value
		      (car (actuals (find-adt-supertype (type obj))))))
	   (cons-ex (when elt-type (make!-cons-name-expr elt-type)))
	   (null-ex (when elt-type (make!-null-name-expr elt-type))))
      (loop while (list-expr? list-ex)
	 do (progn (let ((*restore-object-parent-slot* 'argument))
		     (restore-object* (args1 list-ex)))
		   (setf (type list-ex) (range (type cons-ex)))
		   (let ((op (operator list-ex)))
		     (unless (constructor-name-expr? op)
		       (change-class op 'constructor-name-expr))
		     (unless (type op)
		       (setf (type op) (type cons-ex))
		       (setf (resolutions op) (resolutions cons-ex))))
		   (setf (type (argument list-ex))
			 (mk-tupletype (list elt-type
					     (if (null-expr? (args2 list-ex))
						 (type null-ex)
						 (range (type cons-ex))))))
		   (setq list-ex (args2 list-ex))))
      (setf (type list-ex) (type null-ex))
      (setf (resolutions list-ex) (resolutions null-ex)))
    obj))

(defun list-expr-length (ex)
  (assert (or (list-expr? ex) (null-expr? ex)))
  (let ((len 0)
	(list-ex ex))
    (loop while (list-expr? list-ex)
       do (setq len (1+ len)
		list-ex (args2 list-ex)))
    (assert (null-expr? list-ex))
    len))

(defun list-expr-to-list (ex)    
  (assert (or (list-expr? ex) (null-expr? ex)))
  (let* ((len (list-expr-length ex))
	 (list (make-list len))
	 (list-ex ex)
	 (i 0))
    (loop while (list-expr? list-ex)
       do (progn (setf (nth i list) (args1 list-ex))
		 (incf i)
		 (setq list-ex (args2 list-ex))))
    (assert (null-expr? list-ex))
    list))

(defun list-expr-to-array (ex)
  (assert (or (list-expr? ex) (null-expr? ex)))
  (let* ((len (list-expr-length ex))
	 (arr (make-array len))
	 (list-ex ex)
	 (i 0))
    (loop while (list-expr? list-ex)
       do (progn (setf (aref arr i) (args1 list-ex))
		 (incf i)
		 (setq list-ex (args2 list-ex))))
    arr))

(defvar *mappings-formals* nil)

(defmethod store-object* :around ((obj mapping-with-formals))
  (with-slots (place lhs rhs kind declared-type type mapped-decl formals)
      obj
    (reserve-space 9
      (push-word (store-obj 'mapping-with-formals))
      (push-word (store-obj formals))
      (let ((*mappings-formals* (append formals *mappings-formals*)))
	(push-word (store-obj place))
	(push-word (store-obj lhs))
	(push-word (store-obj rhs))
	(push-word (store-obj kind))
	(push-word (store-obj declared-type))
	(push-word (store-obj type))
	(push-word (store-obj mapped-decl))))))

(defmethod update-fetched ((obj mapping-with-formals))
  (with-slots (place lhs rhs kind declared-type type mapped-decl
                     formals pvs-sxhash-value)
    obj
    (setf formals (fetch-obj (stored-word 1)))
    (let ((*mappings-formals* (append formals *mappings-formals*)))
      (setf place (fetch-obj (stored-word 2)))
      (setf lhs (fetch-obj (stored-word 3)))
      (setf rhs (fetch-obj (stored-word 4)))
      (setf kind (fetch-obj (stored-word 5)))
      (setf declared-type (fetch-obj (stored-word 6)))
      (setf type (fetch-obj (stored-word 7)))
      (setf mapped-decl (fetch-obj (stored-word 8)))
      (setf pvs-sxhash-value nil))))

(defmethod store-object* :around ((obj resolution))
  (with-slots (declaration module-instance type) obj
    (reserve-space 4
      (push-word (store-obj 'resolution))
      (push-word (store-obj declaration))
      (push-word (store-obj (lcopy module-instance :resolutions nil)))
      (push-word (store-obj type)))))

(setf (get 'moduleref 'fetcher) 'fetch-moduleref)
(defun fetch-moduleref ()
  (let* ((mod-name (fetch-obj (stored-word 1)))
	 (theory (get-theory mod-name)))
    (unless theory
      (error "Attempt to fetch unknown theory ~s" mod-name))
    theory))

(setf (get 'modulelibref 'fetcher) 'fetch-modulelibref)
(defun fetch-modulelibref ()
  (let* ((mod-name (fetch-obj (stored-word 1)))
	 (lib-id (fetch-obj (stored-word 2)))
	 (lib-path (fetch-obj (stored-word 3)))
	 (theory (or (get-theory* mod-name lib-id)
		     (get-theory* mod-name nil))))
    (unless theory
      (error "Attempt to fetch unknown library theory ~s from lib-id ~s~%~
              Was associated with ~:[non~;~]existing path ~a"
	     mod-name lib-id
	     (file-exists-p lib-path) lib-path))
    theory))

(defmethod update-fetched :around ((obj datatype-or-module))
  (call-next-method)
  (typecase obj
    (inline-recursive-type nil)
    (t (assert (filename obj))
       (when (recursive-type? obj)
	 (let ((atns (assq (id obj) *adt-type-name-pending*)))
	   (dolist (atn (cdr atns))
	     (setf (adt atn) obj))
	   (setf *adt-type-name-pending*
		 (delete atns *adt-type-name-pending*))))
       ;;(assert (listp *bin-theories-set*))
       (pushnew (id obj) *bin-theories-set*)
       (setf (gethash (id obj) (current-pvs-theories)) obj))))

(defmethod store-object* :around ((obj formula-decl))
  (dolist (proof (proofs obj))
    (setf (refers-to proof)
	  (delete-if (complement
		      #'(lambda (ref)
			  (and (declaration? ref)
			       (assq (module ref)
				     (all-usings *saving-theory*)))))
	    (refers-to proof))))
  (call-next-method))

(defmethod store-object* :around ((obj declaration))
  (with-slots (module) obj
    (assert *saving-theory*)
    (if (and module
	     (not (eq module *saving-theory*))
	     (not (typep obj '(or skolem-const-decl decl-formal)))
	     (not (and (judgement? obj) (generated-by obj))))
	(if (external-library-reference? module)
	    (reserve-space 5
	      (push-word (store-obj 'decllibref))
	      (push-word (store-obj (get-library-id (context-path module))))
	      (push-word (store-obj (context-path module)))
	      (push-word (store-obj (id module)))
	      (push-word (position obj (all-decls module))))
	    (store-declref obj))
	(let ((*saving-declaration* obj))
	  (call-next-method)))))

(defmethod store-object* :around ((obj decl-formal))
  (let ((pos (position obj *mappings-formals*)))
    (if pos
	(reserve-space 4
	  (push-word (store-obj 'declformal-declref))
	  (push-word (store-obj pos)))
	(call-next-method))))

(defun store-declref (obj)
  (let ((module (module obj)))
    #+pvsdebug
    (assert (or (not *saving-theory*)
		(from-prelude? module)
		(memq module (all-imported-theories *saving-theory*))
		(assq module (all-usings *saving-theory*))
		;; This shouldn't happen - but I haven't had time to chase
		;; down exactly what is happening in untypecheck-usedbys
		;; See ~owre/pvs-specs/Shankar/2007-06-12/
		;; tpecheck dpll3_2, modify resolution, retypecheck, and sc.
		;; Something is keeping a pointer to an old resolution theory
		(memq (get-theory (id module))
		      (all-imported-theories *saving-theory*))
		(assq (get-theory (id module))
		      (all-usings *saving-theory*)))
	    () "Attempt to store declaration in illegal theory")
    (typecase obj
      (number-declaration
       (reserve-space 2
	 (push-word (store-obj 'number-declref))
	 (push-word (store-obj (id obj)))))
      (t (let ((dpos (position obj (all-decls module))))
	   (assert dpos () "no dpos?")
	   (reserve-space 3
	     (push-word (store-obj 'declref))
	     (push-word (store-obj (id module)))
	     (push-word dpos)))))))

(defmethod store-object* :around ((obj inline-recursive-type))
  (with-slots ((theory adt-theory)) obj
    #+pvsdebug
    (assert (position obj (all-decls theory)))
    (if (not (eq theory *saving-theory*))
	(if (external-library-reference? theory)
	    (reserve-space 5
	      (push-word (store-obj 'decllibref))
	      (push-word (store-obj (get-library-id (context-path theory))))
	      (push-word (store-obj (context-path theory)))
	      (push-word (store-obj (id theory)))
	      (push-word (position obj (all-decls theory))))
	    (reserve-space 3
	      ;; (assert (or (from-prelude? theory)
	      ;; 		  (assq theory (all-usings *saving-theory*)))
	      ;; 	      () "Storing inline-recursive-type from unimported theory")
	      (push-word (store-obj 'declref))
	      (push-word (store-obj (id theory)))
	      (push-word (position obj (all-decls theory)))))
	(let ((*saving-declaration* obj))
	  (call-next-method)))))

;; nonempty-type-decl
;; type-def-decl
;; nonempty-type-def-decl
;; type-eq-decl
;; nonempty-type-eq-decl
;; type-from-decl
;; nonempty-type-from-decl
;; (defmethod store-object* ((obj type-decl))
;;   (reserve-space 14
;;     (with-slots (newline-comment id formals module
;; 				 refers-to chain?
;; 				 typechecked? visible?
;; 				 generated generated-by
;; 				 semi typecheck-time
;; 				 type-value)
;; 	obj (push-word (store-obj 'type-decl))
;; 	(push-word (store-obj newline-comment))
;; 	(push-word (store-obj id))
;; 	(push-word (store-obj formals))
;; 	(push-word (store-obj module))
;; 	(push-word (store-obj refers-to))
;; 	(push-word (store-obj chain?))
;; 	(push-word (store-obj typechecked?))
;; 	(push-word (store-obj visible?))
;; 	(push-word (store-obj generated))
;; 	(push-word (store-obj generated-by))
;; 	(push-word (store-obj semi))
;; 	(push-word (store-obj typecheck-time))
;; 	(push-word (store-obj (unless (and (type-name? type-value)
;; 					   (eq (declaration type-value) obj))
;; 				type-value))))))

(defun ignore-self-reference-type-values (type-decl)
  (if (and (type-value type-decl)
	   (or (and (type-name? (type-value type-decl))
		    (eq (declaration (type-value type-decl)) type-decl))
	       (and (subtype? (type-value type-decl))
		    (type-name? (print-type (type-value type-decl)))
		    (eq (declaration
			 (resolution (print-type (type-value type-decl))))
			type-decl))
	       (and (type-expr? (type-value type-decl))
		    (type-application? (print-type (type-value type-decl)))
		    (eq (declaration
			 (type (print-type (type-value type-decl))))
			type-decl))))
      (cons (if (type-name? (type-value type-decl))
		(class-name (class-of (type-value type-decl)))
		'type-name)
	    (typecase (type-value type-decl)
	      (adt-type-name
	       (list 'adt (id (adt (type-value type-decl)))
		     'single-constructor? (single-constructor?
					   (type-value type-decl))))))
      (type-value type-decl)))
  
(defmethod all-usings ((obj recursive-type))
  (when (adt-theory obj)
    (cons (list (adt-theory obj) (mk-modname (id (adt-theory obj))))
	  (all-usings (adt-theory obj)))))

(defmethod store-object* :around ((obj context))
  (with-slots (theory theory-name declaration
		      declarations-hash using-hash
		      judgements known-subtypes
		      conversions disabled-conversions
		      auto-rewrites disabled-auto-rewrites)
      obj
    (reserve-space 13
      (push-word (store-obj 'context))
      (push-word (store-obj theory))
      (push-word (store-obj theory-name))
      (push-word (store-obj declaration))
      (let ((decl-hash (create-store-declarations-hash
			declarations-hash)))
	(push-word (store-obj decl-hash)))
      (let ((use-hash (create-store-using-hash using-hash)))
	(push-word (store-obj use-hash)))
      (let ((sjudgements (create-store-judgements judgements)))
	(push-word (store-obj sjudgements)))
      (let ((ksubtypes (create-store-known-subtypes known-subtypes)))
	(push-word (store-obj ksubtypes)))
      (let ((convs (create-store-conversions conversions)))
	(push-word (store-obj convs)))
      (push-word (store-obj
		  disabled-conversions))
      (push-word (store-obj auto-rewrites))
      (push-word (store-obj
		  disabled-auto-rewrites)))))

;;; If only new conversions were added, then at some point we will hit
;;; the prelude conversions list.  Otherwise, we at least make sure we
;;; just use an index for conversions that are in the prelude list.
;;; At the moment, there are no desabled-conversions in the prelude
;;; context, so we can ignore them.
(defun create-store-conversions (conversions &optional store-convs)
  (cond ((null conversions)
	 (nreverse store-convs))
	((eq conversions (conversions *prelude-context*))
	 (nreverse (cons 'prelude-conversions store-convs)))
	(t (let ((pos (position (car conversions)
				(conversions *prelude-context*)
				:test #'eq)))
	     (create-store-conversions
	      (cdr conversions)
	      (if pos
		  (cons pos store-convs)
		  (cons (car conversions) store-convs)))))))

(defun create-store-declarations-hash (declarations-hash)
  (if (or (eq declarations-hash 'prelude-declarations-hash)
	  (eq declarations-hash (declarations-hash *prelude-context*)))
      'prelude-declarations-hash
      (make-linked-hash-table
       :table (lhash-table declarations-hash)
       :next (create-store-declarations-hash (lhash-next declarations-hash)))))

(defmethod judgement-types-hash ((sym symbol))
  (judgement-types-hash (judgements *prelude-context*)))

(defmethod judgement-declarations ((sym symbol))
  (judgement-declarations (judgements *prelude-context*)))

(defmethod number-judgements-alist ((sym symbol))
  (number-judgements-alist (judgements *prelude-context*)))
  
(defmethod name-judgements-alist ((sym symbol))
  (name-judgements-alist (judgements *prelude-context*)))

(defmethod application-judgements-alist ((sym symbol))
  (application-judgements-alist (judgements *prelude-context*)))

(defun create-store-judgements (judgements)
  (if (symbolp judgements)
      (judgements *prelude-context*)
      (let* ((pjudgements (judgements *prelude-context*))
	     (eqnum? (eq (number-judgements-alist pjudgements)
			 (number-judgements-alist judgements)))
	     (eqname? (eq (name-judgements-alist pjudgements)
			  (name-judgements-alist judgements)))
	     (eqapp? (eq (application-judgements-alist pjudgements)
			 (application-judgements-alist judgements)))
	     (eqexpr? (eq (expr-judgements-alist pjudgements)
			  (expr-judgements-alist judgements))))
	(if (and eqnum? eqname? eqapp? eqexpr?)
	    'prelude-judgements
	    (let ((num-judgements (create-store-number-judgements
				   (number-judgements-alist judgements)
				   (number-judgements-alist pjudgements)))
		  (name-judgements (create-store-name-judgements
				    (name-judgements-alist judgements)
				    (name-judgements-alist pjudgements)))
		  (appl-judgements (create-store-application-judgements
				    (application-judgements-alist judgements)
				    (application-judgements-alist pjudgements)))
		  (expr-judgements (create-store-expr-judgements
				    (expr-judgements-alist judgements)
				    (expr-judgements-alist pjudgements))))
	      (make-instance 'judgements
		:judgement-types-hash nil
		:number-judgements-alist num-judgements
		:name-judgements-alist name-judgements
		:application-judgements-alist appl-judgements
		:expr-judgements-alist expr-judgements))))))

(defun create-store-number-judgements (num-judgements pnum-judgements
						      &optional numjs)
  (cond ((null num-judgements)
	 (nreverse numjs))
	((eq num-judgements pnum-judgements)
	 (nreverse (cons 'prelude-number-judgements numjs)))
	(t (let ((pos (position (car num-judgements) pnum-judgements
				:test #'eq)))
	     (create-store-number-judgements
	      (cdr num-judgements)
	      pnum-judgements
	      (cons (or pos (car num-judgements)) numjs))))))

(defun create-store-name-judgements (name-judgements pname-judgements
						     &optional namejs)
  (cond ((null name-judgements)
	 (nreverse namejs))
	((eq name-judgements pname-judgements)
	 (nreverse (cons 'prelude-name-judgements namejs)))
	(t (let ((pos (position (car name-judgements) pname-judgements
				:test #'eq)))
	     (create-store-name-judgements
	      (cdr name-judgements)
	      pname-judgements
	      (cons (or pos (car name-judgements)) namejs))))))

(defun create-store-application-judgements (appl-judgements pappl-judgements
						      &optional appljs)
  (cond ((null appl-judgements)
	 (nreverse appljs))
	((eq appl-judgements pappl-judgements)
	 (nreverse (cons 'prelude-application-judgements appljs)))
	(t (let ((pos (position (car appl-judgements) pappl-judgements
				:test #'eq)))
	     (create-store-application-judgements
	      (cdr appl-judgements)
	      pappl-judgements
	      (cons (or pos (car appl-judgements)) appljs))))))

(defun create-store-expr-judgements (expr-judgements pexpr-judgements
				     &optional exprjs)
  (cond ((null expr-judgements)
	 (nreverse exprjs))
	((eq expr-judgements pexpr-judgements)
	 (nreverse (cons 'prelude-expr-judgements exprjs)))
	(t (let ((pos (position (car expr-judgements) pexpr-judgements
				:test #'eq)))
	     (create-store-expr-judgements
	      (cdr expr-judgements)
	      pexpr-judgements
	      (cons (or pos (car expr-judgements)) exprjs))))))
	     

(defun create-store-known-subtypes (known-subtypes &optional store-subtypes)
  (cond ((null known-subtypes)
	 (nreverse store-subtypes))
	((equal known-subtypes (known-subtypes *prelude-context*))
	 (nreverse (cons 'prelude-known-subtypes store-subtypes)))
	(t (let ((pos (position (car known-subtypes)
				(known-subtypes *prelude-context*)
				:test #'eq)))
	     (create-store-known-subtypes
	      (cdr known-subtypes)
	      (if pos
		  (cons pos store-subtypes)
		  (cons (car known-subtypes) store-subtypes)))))))

(defun create-store-using-hash (using-hash)
  (if (or (eq using-hash 'prelude-using-hash)
	  (eq using-hash (using-hash *prelude-context*)))
      'prelude-using-hash
      (make-linked-hash-table
       :table (lhash-table using-hash)
       :next (create-store-using-hash (lhash-next using-hash)))))

(setf (get 'declref 'fetcher) 'fetch-declref)
(defun fetch-declref ()
  (let* ((mod-name (fetch-obj (stored-word 1)))
	 (theory (get-theory mod-name)))
    (unless theory
      (error "Attempt to fetch declaration from unknown theory ~s" mod-name))
    (let* ((decl-pos (stored-word 2))
	   (decl (nth decl-pos (all-decls theory))))
      (unless decl
	;;(break "No decl?")
	(error "Declaration was not found"))
      decl)))

(setf (get 'declformal-declref 'fetcher) 'fetch-declformal-declref)
(defun fetch-declformal-declref ()
  (let* ((fdecl-pos (fetch-obj (stored-word 1)))
	 (fdecl (nth fdecl-pos *mappings-formals*)))
    (unless fdecl
      (error "Decl formal was not found"))
    fdecl))

(setf (get 'decllibref 'fetcher) 'fetch-decllibref)
(defun fetch-decllibref ()
  (let* ((lib-id (fetch-obj (stored-word 1)))
	 (mod-name (fetch-obj (stored-word 3)))
	 (theory (or (get-theory* mod-name lib-id)
		     (get-theory* mod-name nil))))
    (unless theory
      (error "Attempt to fetch declaration from unknown theory ~s" mod-name))
    (let* ((decl-pos (stored-word 4))
	   (decl (nth decl-pos (all-decls theory))))
      (assert decl () "Declaration was not found")
      decl)))

(setf (get 'number-declref 'fetcher) 'fetch-number-declref)
(defun fetch-number-declref ()
  (let* ((number (fetch-obj (stored-word 1))))
    (number-declaration number)))

(defmethod update-fetched :around ((obj adt-type-name))
  (call-next-method)
  (when (or (symbolp (adt obj))
	    (stringp (adt obj)))
    (let ((rec-type (when (symbolp (adt obj))
		      (get-theory (adt obj)))))
      (if rec-type
	  (let ((atns (assq (adt obj) *adt-type-name-pending*)))
	    (dolist (atn (cdr atns))
	      (setf (adt atn) rec-type))
	    (setf (adt obj) rec-type)
	    (setf *adt-type-name-pending*
		  (delete atns *adt-type-name-pending*)))
	  (add-to-alist (adt obj) obj *adt-type-name-pending*)))))

;; This is the only place that store-print-types are created.
;; Whenever the print-type of a type-expr is not null or a type-name
;; pointing back to the current declaration, the store-print-type replaces
;; the full type, optimizing on space and time.
(defmethod store-object* :around ((obj type-expr))
   (if (and (print-type obj)
	    (not (and (type-name? (print-type obj))
		      (eq (declaration (print-type obj))
			  *saving-declaration*))))
       (let* ((spt (make-instance 'store-print-type
		     :print-type (print-type obj)))
	      (sop *store-object-ptr*))
	 #+pvsdebug (assert (type-expr? (print-type obj)))
	 #+pvsdebug (assert (print-type-correct? obj))
	 (let ((val (store-obj spt)))
	   #+pvsdebug (assert (gethash spt *store-object-hash*))
	   (setf (gethash sop *store-object-substs*)
		 (gethash spt *store-object-hash*))
	   val))
       (call-next-method)))

;;; Note that at this point all we have is the framework, the slots of
;;; the store-print-type have not yet been filled in.
;; (defmethod update-fetched :around ((obj store-print-type))
;;   (pushnew obj *restore-objects*)
;;   (call-next-method))

(defmethod store-object* ((obj vector))
  (let ((len (length obj)))
    (reserve-space (+ len 2)
      (push-word (store-obj 'vector))
      (push-word len)
      (dotimes (i len)
	(push-word (store-obj (aref obj i)))))))

(setf (get 'vector 'fetcher) 'fetch-vector)
(defun fetch-vector ()
  (let* ((len (stored-word 1))
	 (vec (make-array len)))
    (dotimes (i len)
      (setf (aref vec i) (fetch-obj (stored-word (+ 2 i)))))
    vec))

(defmethod store-object* ((obj linked-hash-table))
  (reserve-space 3
    (push-word (store-obj 'linked-hash-table))
    (push-word (store-obj (lhash-table obj)))
    (push-word (store-obj (lhash-next obj)))))

(setf (get 'linked-hash-table 'fetcher) 'fetch-linked-hash-table)
(defun fetch-linked-hash-table ()
  (make-linked-hash-table
   :table (fetch-obj (stored-word 1))
   :next (fetch-obj (stored-word 2))))

(defmethod store-object* ((obj context-entry))
  (reserve-space 8
    (push-word (store-obj 'context-entry))
    (push-word (store-obj (ce-file obj)))
    (push-word (store-obj (ce-write-date obj)))
    (push-word (store-obj (ce-proofs-date obj)))
    (push-word (store-obj (ce-object-date obj)))
    (push-word (store-obj (ce-dependencies obj)))
    (push-word (store-obj (ce-theories obj)))
    (push-word (store-obj (ce-extension obj)))))

(setf (get 'context-entry 'fetcher) 'fetch-context-entry)
(defun fetch-context-entry ()
  (make-context-entry
   :file (fetch-obj (stored-word 1))
   :write-date (fetch-obj (stored-word 2))
   :proofs-date (fetch-obj (stored-word 3))
   :object-date (fetch-obj (stored-word 4))
   :dependencies (fetch-obj (stored-word 5))
   :theories (fetch-obj (stored-word 6))
   :extension (fetch-obj (stored-word 7))))

(defmethod store-object* ((obj theory-entry))
  (reserve-space 5
    (push-word (store-obj 'theory-entry))
    (push-word (store-obj (te-id obj)))
    (push-word (store-obj (te-status obj)))
    (push-word (store-obj (te-dependencies obj)))
    (push-word (store-obj (te-formula-info obj)))))

(setf (get 'theory-entry 'fetcher) 'fetch-theory-entry)
(defun fetch-theory-entry ()
  (make-theory-entry
   :id (fetch-obj (stored-word 1))
   :status (fetch-obj (stored-word 2))
   :dependencies (fetch-obj (stored-word 3))
   :formula-info (fetch-obj (stored-word 4))))

(defmethod store-object* ((obj formula-entry))
  (reserve-space 4
    (push-word (store-obj 'formula-entry))
    (push-word (store-obj (fe-id obj)))
    (push-word (store-obj (fe-status obj)))
    (push-word (store-obj (fe-proof-refers-to obj)))))

(setf (get 'formula-entry 'fetcher) 'fetch-formula-entry)
(defun fetch-formula-entry ()
  (make-formula-entry
   :id (fetch-obj (stored-word 1))
   :status (fetch-obj (stored-word 2))
   :proof-refers-to (fetch-obj (stored-word 3))))

(defmethod store-object* ((obj declaration-entry))
  (reserve-space 5
    (push-word (store-obj 'declaration-entry))
    (push-word (store-obj (de-id obj)))
    (push-word (store-obj (de-class obj)))
    (push-word (store-obj (de-type obj)))
    (push-word (store-obj (de-theory-id obj)))))

(setf (get 'declaration-entry 'fetcher) 'fetch-declaration-entry)
(defun fetch-declaration-entry ()
  (make-declaration-entry
   :id (fetch-obj (stored-word 1))
   :class (fetch-obj (stored-word 2))
   :type (fetch-obj (stored-word 3))
   :theory-id (fetch-obj (stored-word 4))))




(defun restore-object (obj)
  (restore-object* obj))

(defmethod restore-object* :around ((obj datatype-or-module))
  (if (inline-recursive-type? obj)
      (call-next-method)
      (if *restoring-theory*
	  obj
	  (let ((*restoring-theory* obj))
	    (setf (formals-sans-usings obj)
		  (remove-if #'(lambda (ff) (typep ff 'importing)) (formals obj)))
	    (call-next-method)))))

(defmethod restore-object* :around ((obj recursive-type))
  (call-next-method)
  (setf (type-value (declaration (adt-type-name obj)))
	(adt-type-name obj)))

(defmethod restore-object* :around ((obj adt-type-name))
  (call-next-method)
  (when (consp (adt obj))
    ;; (lib-path . adt-id)
    (if (get-prelude-library-theory (car (adt obj)))
	(let ((adt (get-theory (cdr (adt obj)))))
	  (assert adt)
	  (setf (adt obj) adt))
	(let ((adt (get-theory* (cdr (adt obj)) (car (adt obj)))))
	  (if adt
	      (setf (adt obj) adt)
	      (let ((lib-id (get-library-id (car (adt obj)))))
		(assert lib-id)
		(let ((adt (get-theory* (cdr (adt obj)) lib-id)))
		  (assert adt)
		  (setf (adt obj) adt))))))))

(defun get-prelude-library-theory (id)
  (find-if #'(lambda (path)
	       (let ((ws (get-workspace-session path)))
		 (gethash id (pvs-theories ws))))
    (current-prelude-libraries)))


(defmethod restore-object* :around ((obj importing-entity))
  (let ((*restoring-declaration* obj))
    (call-next-method)
    (when (saved-context obj)
      (setq *current-context* (saved-context obj))
      (restore-saved-context *current-context*)
      (setq *current-context* (copy-context *current-context*))
      #+pvsdebug
      (assert (every #'conversion-decl? (conversions (saved-context obj)))))))

(defun restore-saved-context (obj)
  (when obj
    (let ((*restoring-declaration* nil)
	  (*needs-pseudonormalizing* nil)
	  (*current-context* obj))
      (assert (module? (theory obj)))
      (assert (module? (current-theory)))
      (setf (declarations-hash obj)
	    (restore-declarations-hash (declarations-hash obj)))
      ;; Restoring known-subtypes requires judgements, and vice-versa
      ;; So we partially restore the judgements first
      (prerestore-context-judgements (judgements obj))
      (setf (using-hash obj)
	    (restore-using-hash (using-hash obj)))
      (setf (known-subtypes obj)
	    (restore-context-known-subtypes (known-subtypes obj)))
      (setf (judgements obj)
	    (restore-context-judgements (judgements obj)))
      (setf (conversions obj)
	    (restore-context-conversions (conversions obj)))
      (dolist (pair *needs-pseudonormalizing*)
      	(let ((ex (car pair))
      	      (decl (cdr pair)))
      	  (with-current-decl decl
      	    (typecase ex
      	      (type-expr (pseudo-norm! ex))
      	      (expr (break "expr case") (pseudo-normalize ex))
      	      (t (break "what else?"))))))
      #+pvsdebug
      (assert (every #'conversion-decl? (conversions obj)))
      obj)))

(defmethod pseudo-norm! ((act actual))
  (cond ((type-value act)
	 (pseudo-norm! (type-value act))
	 (setf (expr act) (type-value act)))
	(t (let ((nex (pseudo-normalize (expr act))))
	     (unless (eq nex (expr act))
	       (setf (expr act) nex))))))

(defmethod pseudo-norm! ((te type-name))
  (mapc #'pseudo-norm! (actuals te))
  (mapc #'pseudo-norm! (dactuals te)))

(defmethod pseudo-norm! ((db dep-binding))
  (pseudo-norm! (type db)))

(defmethod pseudo-norm! ((te funtype))
  (pseudo-norm! (domain te))
  (pseudo-norm! (range te)))

(defmethod pseudo-norm! ((te tupletype))
  (mapc #'pseudo-norm! (types te)))

(defmethod pseudo-norm! ((te cotupletype))
  (mapc #'pseudo-norm! (types te)))

(defmethod pseudo-norm! ((te recordtype))
  (mapc #'pseudo-norm! (fields te)))

(defmethod pseudo-norm! ((fld field-decl))
  (pseudo-norm! (type fld)))

(defmethod pseudo-norm! ((te subtype))
  (let ((pte (pseudo-normalize (predicate te))))
    (unless (eq pte te)
      (setf (predicate te) pte))))

(defmethod pseudo-norm! ((te type-application))
  (pseudo-norm! (type te))
  (let ((nparms (mapcar #'pseudo-normalize (parameters te))))
    (unless (equal nparms (parameters te))
      (setf (parameters te) nparms))))

(defmethod pseudo-norm! (te)
  (break "Need pseudo-norm! method for ~a" (type te)))

(defun restore-declarations-hash (lhash)
  (cond ((null lhash)
	 nil)
	((eq lhash 'prelude-declarations-hash)
	 (prelude-declarations-hash))
	((eq (lhash-next lhash) 'prelude-declarations-hash)
	 (setf (lhash-next lhash) (prelude-declarations-hash))
	 lhash)
	(t (restore-declarations-hash (lhash-next lhash))
	   lhash)))

(defun restore-using-hash (lhash)
  (cond ((null lhash)
	 nil)
	((eq lhash 'prelude-using-hash)
	 (prelude-using-hash))
	(t (maphash #'(lambda (th ulist)
			(declare (ignore th))
			(restore-object* ulist))
		    (lhash-table lhash))
	   (if (eq (lhash-next lhash) 'prelude-using-hash)
	       (setf (lhash-next lhash) (prelude-using-hash))
	       (restore-using-hash (lhash-next lhash)))
	   lhash)))

(defun restore-context-conversions (conversions &optional convs)
  (cond ((null conversions)
	 #+pvsdebug
	 (assert (every #'conversion-decl? convs))
	 (nreverse convs))
	((eq (car conversions) 'prelude-conversions)
	 (let ((pconvs (conversions *prelude-context*)))
	   (assert pconvs)
	   (push pconvs *restore-objects-seen*)
	   #+pvsdebug
	   (assert (every #'conversion-decl? convs))
	   (nconc (nreverse convs) pconvs)))
	(t (let ((*restoring-declaration* (car conversions)))
	     (restore-context-conversions
	      (cdr conversions)
	      (cons (if (numberp (car conversions))
			(let ((pconv (nth (car conversions)
					  (conversions *prelude-context*))))
			  (assert (conversion-decl? pconv))
			  (push pconv *restore-objects-seen*)
			  pconv)
			(progn (restore-object* (car conversions))
			       (assert (conversion-decl? (car conversions)))
			       (car conversions)))
		    convs))))))

(defun prerestore-context-judgements (judgements)
  (if (eq (current-judgements) 'prelude-judgements)
      (setf (current-judgements)
	    (copy (judgements *prelude-context*)
	      'judgement-types-hash
	      (make-pvs-hash-table #-cmu :weak-keys? #-cmu t)))
      (let ((pjudgements (judgements *prelude-context*)))
	(unless (judgement-types-hash (current-judgements))
	  (setf (judgement-types-hash (current-judgements))
		(make-pvs-hash-table #-cmu :weak-keys? #-cmu t)))
	(unless (eq judgements 'prelude-judgements)
	  ;; (prerestore-number-judgements-alist
	  ;;  (number-judgements-alist judgements)
	  ;;  (number-judgements-alist pjudgements))
	  (setf (name-judgements-alist judgements)
		(prerestore-name-judgements-alist
		 (name-judgements-alist judgements)
		 (name-judgements-alist pjudgements)))
	  (assert (listp (application-judgements-alist judgements)))
	  (when (memq 'prelude-application-judgements
		      (application-judgements-alist judgements))
	    #+pvsdebug
	    (assert
	     (null (cdr (memq 'prelude-application-judgements
			      (application-judgements-alist judgements)))))
	    (nconc (nbutlast (application-judgements-alist judgements))
		   (application-judgements-alist pjudgements)))
	  ;;(prerestore-application-judgements-alist
	  ;; (application-judgements-alist judgements)
	  ;; (application-judgements-alist pjudgements))
	  (assert (listp (expr-judgements-alist judgements)))
	  (when (memq 'prelude-expr-judgements
		      (expr-judgements-alist judgements))
	    #+pvsdebug
	    (assert
	     (null (cdr (memq 'prelude-expr-judgements
			      (expr-judgements-alist judgements)))))
	    (nconc (nbutlast (expr-judgements-alist judgements))
		   (expr-judgements-alist pjudgements)))))))

(defun prerestore-nunmer-judgements-alist (number-judgements pnumber-judgements
					   &optional numjs)
  (cond ((null number-judgements)
	 (nreverse numjs))
	((eq (car number-judgements) 'prelude-number-judgements)
	 (assert pnumber-judgements)
	 (push pnumber-judgements *restore-objects-seen*)
	 (nconc (nreverse numjs) pnumber-judgements))
	(t (prerestore-number-judgements-alist
	    (cdr number-judgements)
	    pnumber-judgements
	    (cons (if (numberp (car number-judgements))
		      (let ((pnumj (nth (car number-judgements)
					pnumber-judgements)))
			(assert pnumj)
			(push pnumj *restore-objects-seen*)
			pnumj)
		      (car number-judgements))
		  numjs)))))

(defun prerestore-name-judgements-alist (name-judgements pname-judgements
							 &optional namejs)
  (cond ((null name-judgements)
	 (nreverse namejs))
	((eq (car name-judgements) 'prelude-name-judgements)
	 (assert pname-judgements)
	 (push pname-judgements *restore-objects-seen*)
	 (nconc (nreverse namejs) pname-judgements))
	(t (prerestore-name-judgements-alist
	    (cdr name-judgements)
	    pname-judgements
	    (cons (if (numberp (car name-judgements))
		      (let ((pnamej (nth (car name-judgements)
					 pname-judgements)))
			(assert pnamej)
			(push pnamej *restore-objects-seen*)
			pnamej)
		      (car name-judgements))
		  namejs)))))

(defun restore-context-judgements (judgements)
  (let ((pjudgements (judgements *prelude-context*)))
    (cond ((eq judgements 'prelude-judgements)
	   (when (number-judgements-alist pjudgements)
	     (push (number-judgements-alist pjudgements)
		   *restore-objects-seen*))
	   (push (name-judgements-alist pjudgements)
		 *restore-objects-seen*)
	   (push (application-judgements-alist pjudgements)
		 *restore-objects-seen*)
	   (push (expr-judgements-alist pjudgements)
		 *restore-objects-seen*)
	   (copy pjudgements
	     'judgement-types-hash
	     (make-pvs-hash-table #-cmu :weak-keys? #-cmu t)))
	  (t
	   (setf (judgement-types-hash judgements) (make-pvs-hash-table
						    #-cmu :weak-keys? #-cmu t))
	   (setf (number-judgements-alist judgements)
		 (restore-number-judgements-alist
		  (number-judgements-alist judgements)
		  (number-judgements-alist pjudgements)))
	   (setf (name-judgements-alist judgements)
		 (restore-name-judgements-alist
		  (name-judgements-alist judgements)
		  (name-judgements-alist pjudgements)))
	   (setf (application-judgements-alist judgements)
		 (restore-application-judgements-alist
		  (application-judgements-alist judgements)
		  (application-judgements-alist pjudgements)))
	   (setf (expr-judgements-alist judgements)
		 (restore-expr-judgements-alist
		  (expr-judgements-alist judgements)
		  (expr-judgements-alist pjudgements)))
	   judgements))))

(defun restore-number-judgements-alist (num-judgements pnum-judgements
						       &optional numjs)
  (cond ((null num-judgements)
	 (nreverse numjs))
	((eq (car num-judgements) 'prelude-number-judgements)
	 (when pnum-judgements
	   (push pnum-judgements *restore-objects-seen*))
	 (nconc (nreverse numjs) pnum-judgements))
	(t (restore-number-judgements-alist
	    (cdr num-judgements)
	    pnum-judgements
	    (cons (if (numberp (car num-judgements))
		      (let ((pnumj (nth (car num-judgements) pnum-judgements)))
			(assert pnumj)
			(push pnumj *restore-objects-seen*)
			pnumj)
		      (restore-object* (car num-judgements)))
		  numjs)))))

(defun restore-name-judgements-alist (name-judgements pname-judgements
						       &optional namejs)
  (cond ((null name-judgements)
	 (nreverse namejs))
	((eq (car name-judgements) 'prelude-name-judgements)
	 (assert pname-judgements)
	 (push pname-judgements *restore-objects-seen*)
	 (nconc (nreverse namejs) pname-judgements))
	(t (restore-name-judgements-alist
	    (cdr name-judgements)
	    pname-judgements
	    (cons (if (numberp (car name-judgements))
		      (let ((pnamej (nth (car name-judgements)
					 pname-judgements)))
			(assert pnamej)
			(push pnamej *restore-objects-seen*)
			pnamej)
		      (restore-object* (car name-judgements)))
		  namejs)))))

(defun restore-application-judgements-alist (appl-judgements pappl-judgements
						       &optional appljs)
  (cond ((null appl-judgements)
	 (nreverse appljs))
	((eq (car appl-judgements) 'prelude-application-judgements)
	 (assert pappl-judgements)
	 (push pappl-judgements *restore-objects-seen*)
	 (nconc (nreverse appljs) pappl-judgements))
	(t (restore-application-judgements-alist
	    (cdr appl-judgements)
	    pappl-judgements
	    (cons (if (numberp (car appl-judgements))
		      (let ((papplj (nth (car appl-judgements)
					 pappl-judgements)))
			(assert papplj)
			(push papplj *restore-objects-seen*)
			papplj)
		      (restore-object* (car appl-judgements)))
		  appljs)))))

(defun restore-expr-judgements-alist (expr-judgements pexpr-judgements
				      &optional exprjs)
  (cond ((null expr-judgements)
	 (nreverse exprjs))
	((eq (car expr-judgements) 'prelude-expr-judgements)
	 (assert pexpr-judgements)
	 (push pexpr-judgements *restore-objects-seen*)
	 (nconc (nreverse exprjs) pexpr-judgements))
	(t (restore-expr-judgements-alist
	    (cdr expr-judgements)
	    pexpr-judgements
	    (cons (if (numberp (car expr-judgements))
		      (let ((pexprj (nth (car expr-judgements)
					 pexpr-judgements)))
			(assert pexprj)
			(push pexprj *restore-objects-seen*)
			pexprj)
		      (restore-object* (car expr-judgements)))
		  exprjs)))))

(defun restore-context-known-subtypes (known-subtypes &optional ksubtypes)
  (cond ((null known-subtypes)
	 (nreverse ksubtypes))
	((eq (car known-subtypes) 'prelude-known-subtypes)
	 (let ((pknown-subtypes (known-subtypes *prelude-context*)))
	   (assert pknown-subtypes)
	   (push pknown-subtypes *restore-objects-seen*)
	   (nconc (nreverse ksubtypes) pknown-subtypes)))
	((listp (car known-subtypes))
	 (restore-object (car known-subtypes))
	 (mapcar #'(lambda (fv)
		     (let ((*restore-object-parent* (declaration fv))
			   (*restore-object-parent-slot* 'type))
		       (restore-object* (type (declaration fv)))))
	   (freevars (car known-subtypes)))
	 (assert (every #'type-expr? (car known-subtypes)))
	 #+pvsdebug
	 (assert (every #'type-expr?
			(mapcar #'(lambda (x)
				    (type (declaration x)))
			  (freevars (car known-subtypes)))))
	 (restore-context-known-subtypes (cdr known-subtypes)
					 (cons (car known-subtypes) ksubtypes)))
	(t (restore-context-known-subtypes
	    (cdr known-subtypes)
	    (cons (if (numberp (car known-subtypes))
		      (let ((pksub (nth (car known-subtypes)
					(known-subtypes *prelude-context*))))
			(assert pksub)
			(push pksub *restore-objects-seen*)
			pksub)
		      (let ((car-subtypes (restore-object* (car known-subtypes))))
			(mapcar #'(lambda (fv)
				    (let ((*restore-object-parent*
					   (declaration fv))
					  (*restore-object-parent-slot* 'type))
				      (restore-object*
				       (type (declaration fv)))))
			  (freevars car-subtypes))
			(assert (every #'type-expr? car-subtypes))
			#+pvsdebug
			(assert (every #'type-expr?
				       (mapcar #'(lambda (x)
						   (type (declaration x)))
					 (freevars car-subtypes))))
			car-subtypes))
		  ksubtypes)))))

(defmethod restore-object* :around ((obj declaration))
  #+badassert
  (assert (every #'(lambda (fdecl)
		     (not (memq fdecl (get-declarations (id fdecl)))))
		 (decl-formals obj)) () "recursive declaration restore?")
  (with-current-decl obj
    (if (gethash obj *restore-object-hash*)
	(let ((*restoring-declaration* obj))
	  (call-next-method))
	(if (and (module obj)
		 (eq (module obj) *restoring-theory*))
	    (unless (and (boundp '*restoring-declaration*) ;; bound to nil in context
			 (not (or (eq (generated-by obj) *restoring-declaration*)
				  (typep obj '(or adtdecl judgement)))))
	      (let ((*restoring-declaration* obj)
		    (*bound-variables* (apply #'append (formals obj))))
		(call-next-method)
		(when (and (eq *restoring-theory* (current-theory))
			   (not (typep obj 'adtdecl)))
		  (put-decl obj))))
	    (let ((*restoring-declaration* obj))
	      (call-next-method)))))
  #+badassert
  (assert (every #'(lambda (fdcl)
		     (not (memq fdcl (get-declarations (id fdcl)))))
		 (decl-formals obj))))

(defmethod restore-object* :around ((obj existence-tcc))
  (call-next-method)
  (setf (nonempty? (type obj)) t))

(defmethod restore-object* :around ((obj binding-expr))
  (let ((*bound-variables* (append (bindings obj) *bound-variables*)))
    (call-next-method)))

(defmethod restore-object* :around ((obj resolution))
  (call-next-method)
  (unless (or (not (typed-declaration? (declaration obj)))
	      (null (type (declaration obj)))
	      (eq (type (declaration obj)) (type obj)))
    (let ((*restore-object-parent* (declaration obj))
	  (*restore-object-parent-slot* 'type))
      (restore-object* (type (declaration obj)))
      (assert (type-expr? (type (declaration obj)))))))

(defmethod restore-object* :around ((obj nonempty-type-decl))
  (call-next-method)
  (set-nonempty-type (type-value obj) obj))

;; (defmethod set-nonempty-type ((te store-print-type) decl)
;;   (declare (ignore decl))
;;   (setf (nonempty? te) t))

(defmethod restore-object* :around ((obj type-decl))
  (call-next-method)
  (assert (not (store-print-type? (type-value obj))))
;;   (assert (or (not (subtype? (type-value obj)))
  ;; 	      (not (store-print-type? (supertype (type-value obj))))))
  (restore-type-value obj)
  obj)

(defun restore-type-value (obj)
  (when (consp (type-value obj))
    (let* ((tn (apply #'make-instance (or (car (type-value obj))
					  'type-name)
		      :id (id obj) (cdr (type-value obj))))
	   (res (mk-resolution obj (mk-modname (id (module obj))) tn)))
      (setf (resolutions tn) (list res))
      (let ((ptype (if (formals obj)
		       (make-instance 'type-application
			 :type tn
			 :parameters (mapcar #'mk-name-expr
				       (car (formals obj))))
		       tn)))
	(when (adt-type-name? tn)
	  (let ((rec-type (when (symbolp (adt tn))
			    (get-theory (adt tn)))))
	    (if rec-type
		(let ((atns (assq (adt tn) *adt-type-name-pending*)))
		  (dolist (atn (cdr atns))
		    (setf (adt atn) rec-type))
		  (setf (adt tn) rec-type)
		  (setf *adt-type-name-pending*
			(delete atns *adt-type-name-pending*)))
		(add-to-alist (adt tn) tn *adt-type-name-pending*))))
	(if (type-def-decl? obj)
	    (setf (type-value obj) (type-def-decl-saved-value obj ptype))
	    (setf (type-value obj) tn))
	#+pvsdebug (assert (true-type-expr? (type-value obj)))
	)))
  (type-value obj))


(defmethod restore-object* :around ((obj conversionminus-decl))
  (call-next-method)
  (disable-conversion obj))

(defmethod restore-object* :around ((obj type-name))
  (let* ((res (resolution obj))
	 (type (when res
		 (if (store-print-type? (type res))
		     (print-type (type res))
		     (type res)))))
    (cond ((eq obj type)
	   (let ((*restore-object-parent* obj)
		 (*restore-object-parent-slot* 'actuals))
	     (restore-object* (actuals obj)))
	   (let ((*restore-object-parent* obj)
		 (*restore-object-parent-slot* 'dactuals))
	     (restore-object* (dactuals obj)))
	   (let ((*restore-object-parent* res)
		 (*restore-object-parent-slot* 'module-instance))
	     (restore-object* (module-instance res)))
	   (let ((*restore-object-parent* res)
		 (*restore-object-parent-slot* 'declaration))
	     (restore-object* (declaration res)))
	   (setf (type res) type)
	   obj)
	  (t (call-next-method)))))

(defmethod restore-object* :around (obj)
  (let ((gobj (gethash obj *restore-object-hash*)))
    (cond (gobj
	   (unless (eq gobj obj)
	     (setf-restored-object gobj))
	   (assert (not (store-print-type? gobj)))
	   gobj)
	  ((null obj) nil)
	  ((memq obj *restore-objects-seen*)
;; 	   (unless (or (listp obj)
;; 		       (def-decl? obj)
;; 		       (and (const-decl? obj) (def-axiom obj))
;; 		       (eq obj (known-subtypes *prelude-context*)))
;; 	     (break "Working hard on this one"))
	   (unless (eq obj *restoring-type-application*)
	     (assert (not (store-print-type? obj)) ()
		     "Should have been restored already"))
	   obj)
	  (t (let* ((*restore-objects-seen* (cons obj *restore-objects-seen*))
		    (nobj (call-next-method)))
	       (when (subtype? nobj)
		 (assert (not (store-print-type? (supertype nobj))) ()
			 "nobj: call-next-method should have restored supertype"))
	       (when (subtype? obj)
		 (assert (not (store-print-type? (supertype obj))) ()
			 "obj: call-next-method should have restored supertype"))
	       (unless (eq obj nobj)
		 (setf-restored-object nobj))
	       (assert (not (store-print-type? nobj)) ()
		       "call-next-method should have restored this")
	       (setf (gethash obj *restore-object-hash*) nobj))))))

(defmethod setf-restored-object (nobj)
  (setf-restored-object*
   nobj *restore-object-parent* *restore-object-parent-slot*))

(defmethod setf-restored-object* (nobj parent slot)
  #+pvsdebug (assert (eq (type-of (class-of parent)) 'standard-class))
  #+pvsdebug (assert (slot-exists-p parent slot))
  (assert (not (eq slot 'declarations-hash)) () "setting declarations-hash")
  (setf (slot-value parent slot) nobj))

(defmethod restore-object* ((obj cons))
  (let ((*restore-object-parent* obj))
    (let ((*restore-object-parent-slot* 'car))
      (restore-object* (car obj)))
    (let ((*restore-object-parent-slot* 'cdr))
      (restore-object* (cdr obj))))
  obj)

(defmethod setf-restored-object* (nobj (parent cons) slot)
  (assert (memq slot '(car cdr)))
  (if (eq slot 'car)
      (setf (car parent) nobj)
      (setf (cdr parent) nobj)))

(defmethod restore-object* ((obj symbol))
  obj)

(defmethod restore-object* ((obj string))
  obj)

(defmethod restore-object* ((obj pathname))
  obj)

(defmethod restore-object* ((obj cl:number))
  obj)

(defmethod restore-object* ((obj hash-table))
  (let ((*restore-object-parent* obj))
    (maphash #'(lambda (x y)
		 (let* ((pair (cons x y))
			(*restore-object-parent-slot* pair))
		   (restore-object* pair)))
	     obj))
  obj)

(defmethod setf-restored-object* (nobj (parent hash-table) slot)
  (unless (eq (car slot) (car nobj))
    (remhash parent (car slot)))
  (setf (gethash (car nobj) parent) (cdr nobj)))

(defmethod restore-object* ((obj linked-hash-table))
  (unless (eq *restore-object-parent-slot* 'declarations-hash)
    (restore-object* (lhash-table obj)))
  (if (and (not (null (lhash-next obj)))
	   (symbolp (lhash-next obj)))
      (setf (lhash-next obj)
	    (funcall (lhash-next obj)))
      (restore-object* (lhash-next obj)))
  obj)

(defun prelude-declarations-hash ()
  (declarations-hash *prelude-context*))

(defun prelude-using-hash ()
  (using-hash *prelude-context*))

(defmethod restore-object* ((obj array))
  (let ((*restore-object-parent* obj))
    (dotimes (i (length obj))
      (let ((*restore-object-parent-slot* i))
	(restore-object* (aref obj i))))
    obj))

(defmethod setf-restored-object* (nobj (parent array) slot)
  (setf (aref parent slot) nobj))

(defmethod restore-object* :around ((obj recordtype))
  (prog1 (call-next-method)
    (unless (memq *restore-object-parent-slot* '(print-type type-expr))
      (setf (fields obj)
	    (sort-fields (fields obj) (dependent? obj))))))


;;; Do not make this an :around method - causes problems I don't fully understand

(defmethod restore-object* ((obj store-print-type))
  (or (type obj)
      (let ((pt (print-type obj))
	    ;;(*pseudo-normalizing* t)
	    )
	(cond ((and (type-name? pt)
		    (store-print-type? (type-value (declaration pt))))
	       ;;(assert (eq (type-value (declaration pt)) obj))
	       (let* ((decl (declaration pt))
		      (tn (mk-type-name (id decl)))
		      (res (mk-resolution decl (mk-modname (id (module decl))) tn))
		      (ptype (if (formals decl)
				 (make-instance 'type-application
				   :type tn
				   :parameters (mapcar #'mk-name-expr
						 (car (formals decl))))
				 tn))
		      (tval (type-def-decl-saved-value decl ptype)))
		 (setf (resolutions tn) (list res))
		 #+pvsdebug (assert (true-type-expr? tval) ()
				    "Not a true type - type-name")
		 (unless (nonempty? tval)
		   (setf (nonempty? tval)
			 (or (nonempty? obj)
			     (nonempty? pt))))
		 (setf (type-value decl) tval)))
	      ((type-application? pt)
	       (restore-object* (parameters pt))
	       (let ((*restoring-type-application* obj))
		 (restore-object* (type pt)))
	       (let ((texpr (type-expr-from-print-type pt)))
		 #+pvsdebug (assert (true-type-expr? texpr) ()
				    "Not a true type - type-application")
		 (unless (nonempty? texpr)
		   (setf (nonempty? texpr)
			 (or (nonempty? obj)
			     (nonempty? pt))))
		 (setf (type obj) texpr)))
	      ((store-print-type? pt)
	       (let ((stype (restore-object* pt)))
	       	 #+pvsdebug (assert (true-type-expr? stype) ()
				    "Not a true type - store-print-type case")
	       	 (setf (type obj) stype)
		 (unless (nonempty? stype)
		   (setf (nonempty? stype)
			 (or (nonempty? obj)
			     (nonempty? pt))))
	       	 stype))
	      (t (restore-object* pt)
		 (let ((texpr (type-expr-from-print-type pt)))
		   #+pvsdebug (assert (true-type-expr? texpr) ()
				      "Not a true type - default case")
		   (unless (nonempty? texpr)
		     (setf (nonempty? texpr)
			   (or (nonempty? obj)
			       (nonempty? pt))))
		   (setf (type obj) texpr)))))))

(defun type-def-decl-saved-value (decl tn)
  (with-current-decl decl
    (cond ((type-from-decl? decl)
	   (let* ((*bound-variables* (apply #'append (formals decl)))
		  (stype (typecheck* (type-expr decl) nil 'type nil))
		  (pname (makesym "~a_pred" (id decl)))
		  (pdecl (find-if #'(lambda (d)
				      (and (const-decl? d)
					   (eq (id d) pname)))
			   (generated decl)))
		  (pexpr (mk-name-expr (id pdecl) nil nil
				       (mk-resolution pdecl (current-theory-name)
						      (type pdecl))))
		  (utype (mk-subtype stype pexpr)))
	     (set-type (type-expr decl) nil)
	     (setf (type-value decl) utype)
	     (assert (type-expr? tn))
	     (setf (print-type utype) tn)
	     utype))
	  ((enumtype? (type-expr decl))
	   (change-class tn 'adt-type-name 'adt (type-expr decl))
	   tn)
	  (t (restore-object* (type-expr decl))
	     (let* ((*generate-tccs* 'none)
		    (*bound-variables* (apply #'append (formals decl)))
		    (tval (typecheck* (type-expr decl) nil nil nil)))
	       (assert (type-expr? tn))
	       #+pvsdebug (assert (true-type-expr? tval))
	       (copy tval 'print-type tn))))))

(defmethod true-type-expr? (obj)
  (declare (ignore obj))
  nil)

(defmethod true-type-expr? ((obj type-expr))
  t)

(defmethod true-type-expr? ((obj expr-as-type))
  nil)

(defmethod true-type-expr? ((obj type-application))
  nil)

;; (defmethod restore-object-print-type ((obj store-print-type))
;;   (or (type obj)
;;       (setf (type obj)
;; 	    (restore-object-print-type (print-type obj)))))

;; (defmethod restore-object-print-type ((ptype type-name))
;;   (type-expr-from-print-type ptype))

;; (defmethod restore-object-print-type ((ptype expr-as-type))
;;   (type-expr-from-print-type ptype))

;; (defmethod restore-object-print-type ((ptype type-application))
;;   (restore-object-print-types* (parameters ptype))
;;   (type-expr-from-print-type ptype))

;; (defmethod restore-object-print-types* ((list list))
;;   (dolist (elt list)
;;     (restore-object-print-types* elt)))

;; (defmethod restore-object-print-types* ((ex binding))
;;   (setf (type ex) (restore-object-print-types* (type ex))))

;; (defmethod restore-object-print-types* ((ex name-expr))
;;   (setf (type ex) (restore-object-print-types* (type ex)))
;;   (setf (type (resolution ex))
;; 	(restore-object-print-types* (type (resolution ex)))))

;; (defmethod restore-object-print-types* ((ex store-print-type))
;;   (restore-object ex))

(defmethod type-expr-from-print-type ((te type-name))
  (let* ((decl (declaration (resolution te)))
	 (tval (if (consp (type-value decl))
		   (restore-type-value decl)
		   (type-value decl)))
	 (thinst (module-instance (resolution te))))
    (assert (eq (id (module decl)) (id thinst))
	    () "Strange type-name resolution")
    (restore-object* tval)
    (restore-object* (actuals thinst))
    (restore-object* (dactuals thinst))
    (assert (not (store-print-type? tval)))
    (let* ((type-expr (if (or (actuals thinst) (dactuals thinst))
			  (let* ((*pseudo-normalizing* t) ;; disallow pseudo-normalize
				 (nte (subst-mod-params tval thinst (module decl) decl)))
			    (when (theory-element? *restoring-declaration*)
			      (push (cons nte *restoring-declaration*)
				    *needs-pseudonormalizing*))
			    nte)
			  (copy tval 'print-type te))))
      #+pvsdebug (assert (or (print-type type-expr) (tc-eq te type-expr)))
      #+pvsdebug (assert (true-type-expr? type-expr))
      type-expr)))

(defmethod type-expr-from-print-type ((te expr-as-type))
  (let* ((suptype (find-supertype (type (expr te))))
	 (subtype (mk-subtype (domain suptype) (expr te))))
    ;; Make sure to set the slot, not call any methods
    (with-slots (print-type) subtype
      (setf print-type te))
    subtype))

(defmethod type-expr-from-print-type ((te type-application))
  (let* ((res (resolution (type te)))
	 (decl (declaration res))
	 (thinst (module-instance res)))
    (restore-object* (actuals thinst))
    (restore-object* (dactuals thinst))
    (let* ((mtype-expr (if (or (actuals thinst) (dactuals thinst))
			   (let* ((*pseudo-normalizing* t) ;; disallow pseudo-normalize
				  (nte (subst-mod-params (type-value decl) thinst
					 (module decl) decl))
				  (decl (or (and (boundp '*restoring-declaration*)
						 (theory-element? *restoring-declaration*)
						 *restoring-declaration*)
					    (current-declaration))))
			     (assert decl)
			     (push (cons nte decl) *needs-pseudonormalizing*)
			     nte)
			   (type-value decl)))
	   (type-expr (if (every #'(lambda (x y)
				     (and (name-expr? y)
					  (eq (declaration y) x)))
				 (car (formals decl)) (parameters te))
			  mtype-expr
			  (let ((*pseudo-normalizing* t) ;; disallow pseudo-normalize
				(nte (substit mtype-expr
				       (pairlis (car (formals decl)) (parameters te))))
				(decl (or (and (boundp '*restoring-declaration*)
					       (theory-element? *restoring-declaration*)
					       *restoring-declaration*)
					  (current-declaration))))
			    (assert (theory-element? decl))
			    (push (cons nte decl) *needs-pseudonormalizing*)
			    nte))))
      #+pvsdebug (assert (true-type-expr? type-expr))
      (unless (eq type-expr (type-value decl))
	;; Make sure to set the slot, not call any methods
	(with-slots (print-type) type-expr
	  (setf print-type te)))
      type-expr)))

(defmethod print-type-correct? ((te type-expr))
  (or (null (print-type te))
      (let ((*current-context* (or *current-context*
				   (and *saving-theory*
					(saved-context *saving-theory*)))))
	(tc-eq te (type-expr-from-print-type (print-type te))))))
