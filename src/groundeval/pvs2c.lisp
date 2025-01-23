;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pvs2c.lisp -- 
;; Author          : Shankar, Gaspard Ferey, and Sam Owre
;; Status          : Stable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; --------------------------------------------------------------------
;; PVS
;; Copyright (C) 2024, SRI International.  All Rights Reserved.

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

(defvar *pvs2c-current-decl* nil)
(defvar *pvs2c-defn-actuals* nil)
(defvar *current-pvs2c-theory* nil)
;;(defvar *pvs2c-preceding-theories* nil)
(defvar *pvs2c-library-path* nil)
(defvar *pvs2c-theory-importings* nil)
(defvar *pvs2c-theory-decls* nil)

;;; SO The following are from the pvs-strings branch of my clib/src,
;;; but I'm not certain of the source.
;;;    GC.c - not generated, supports GC in PVS2C
;;; The rest are from prelude theories:
;;;    bytestrings euclidean_division exp2 gen_strings identity integertypes lex2 min_nat
;;;    modulo_arithmetic ordinals ordstruct_adt pvslib.c real_defs strings

;;commented theories are the ones that are supported by C code generation
;;The non-parametric ones that are supported are in *pvs2c-prelude-theories$
;;Others like |restrict|, |K_conversion|, |K_props|, |identity| have code generated
;;during partial monomorphization.
(defparameter *primitive-prelude-theories*
  '(|booleans| |equalities| |notequal| |if_def| |boolean_props| ;; |xor_def|
    |quantifier_props| |defined_types| |exists1| |equality_props| |if_props| |functions|
    |functions_alt| ;; |transpose| |restrict|
    |restrict_props| |extend| ;; |extend_bool|
    |extend_props| |extend_func_props| ;; |K_conversion| |K_props| ;; |identity|
    |identity_props| |relations| |orders| |orders_alt| |restrict_order_props|
    |extend_order_props| |wf_induction| |measure_induction| |epsilons| |decl_params| |sets|
    |sets_lemmas| |function_inverse_def| |function_inverse| |function_inverse_alt|
    |function_image| |function_props| |function_props_alt| |function_props2| |relation_defs|
    |relation_props| |relation_props2| |relation_converse_props| |indexed_sets|
    |operator_defs| |numbers| |number_fields| |reals| |real_axioms| |bounded_real_defs|
    |bounded_real_defs_alt| |real_types| |rationals| |integers| |naturalnumbers|
    ;; |min_nat| |real_defs|
    |real_props| |extra_real_props| |extra_tegies| |rational_props| |integer_props|
    |floor_ceil| |exponentiation| ;; |euclidean_division|
    |divides| ;; |modulo_arithmetic|
    |subrange_inductions| |bounded_int_inductions| |bounded_nat_inductions|
    |subrange_type| |int_types| |nat_types| ;; |exp2| |integertypes|
    |nat_fun_props| |finite_sets| |restrict_set_props| |extend_set_props|
    |function_image_aux| |function_iterate| ;|sequences|
    |seq_functions| ;|finite_sequences| |more_finseq| |ordstruct|
    ;; |ordstruct_adt| |ordinals| |lex2|
    |lex3| |lex4| |list| |list_adt| |list_adt_map| |list_props| |map_props|
    |more_map_props| |filters| |list2finseq| |list2set| |disjointness| |character|
    |character_adt| ;; |strings| |gen_strings|
    |charstrings| ;; |bytestrings|
    |lift| |lift_adt| |lift_adt_map| |union| |tostring| |file|
    |mucalculus| |ctlops| |fairctlops| |Fairctlops| |bit| |bv| |bv_concat_def|
    |bv_bitwise| |bv_nat| ;|empty_bv|
    |bv_caret| ;|integer_bv_ops|
    |mod| |bv_arith_nat_defs| |bv_int_defs| |bv_arithmetic_defs| |bv_extend_defs|
    |infinite_sets_def| |finite_sets_of_sets|
    |EquivalenceClosure| |QuotientDefinition| |KernelDefinition| |QuotientKernelProperties|
    |QuotientSubDefinition| |QuotientExtensionProperties| |QuotientDistributive|
    |QuotientIteration| |PartialFunctionDefinitions| |PartialFunctionComposition|
    |stdpvs| |stdexc| |stdexc_tags| |stdcatch| |stdprog| |stdglobal| |stdmutable|
    |stdmath| |stdstr| |stdio| |stdpvsio| |stdsys|))

;; Leaving out
;;   |euclidean_division| - no functions
;;   |function_iterate| - iterate not translatable
;;   |modulo_arithmetic| - no functions
;;   |array_sequences| - needs work

(defparameter *pvs2c-prelude-theories*
  ;; Sorted alphabetically
  '(|bytestrings| |empty_bv| |exp2| |extend_bool|
    |gen_strings| |identity| |integer_bv_ops| |integertypes| |lex2|
    |min_nat| |modulo_arithmetic| |more_finseq|
    |ordinals| |ordstruct| |real_defs| |sequences| |strings|
    |transpose| |xor_def|))

(defun ensure-c-directories-exist ()
  (when *pvs2c-library-path*
    (let* ((c-dir *pvs2c-library-path*)
	   (src-dir (format nil "~a/src/" c-dir))
	   (include-dir (format nil "~a/include/" c-dir))
	   (lib-dir (format nil "~a/lib/" c-dir)))
      (uiop:ensure-all-directories-exist (list src-dir))
      (dolist (file (uiop:directory-files src-dir "*.c"))
	(delete-file file))
      (uiop:ensure-all-directories-exist (list include-dir))
      (dolist (file (uiop:directory-files src-dir "*.h"))
	(delete-file file))
      (uiop:ensure-all-directories-exist (list lib-dir))
      (dolist (file (uiop:directory-files src-dir "*.a"))
	(delete-file file))
      (dolist (file (uiop:directory-files src-dir "*.so"))
	(delete-file file))
      (dolist (file (uiop:directory-files src-dir "*.dylib"))
	(delete-file file)))))

(defun pvs2c-prelude ()
  (let ((*pvs2c-library-path* (format nil "~a/lib/pvs2c/" *pvs-path*))
	(*suppress-output* t)
	(pvs2c-translated-theories nil))
    (ensure-c-directories-exist)
    (uiop:copy-file (format nil "~a/src/groundeval/pvslib.c" *pvs-path*)
		    (format nil "~a/lib/pvs2c/src/pvslib.c" *pvs-path*))
    (uiop:copy-file (format nil "~a/src/groundeval/pvslib.h" *pvs-path*)
		    (format nil "~a/lib/pvs2c/include/pvslib.h" *pvs-path*))
    (dolist (theory *prelude-theories*)
      (when (memq (id theory) *pvs2c-prelude-theories*)
	(let ((main-theory (if (datatype? theory) (adt-theory theory) theory)))
	  (let ((c-file (pvs2c-theory* main-theory t)))
	    (when c-file
	      (push (file-namestring c-file) pvs2c-translated-theories))))))
    (create-config-make (cons "pvslib.c" ;NSH(1/21/25)(cons "GC.c"
					       (reverse pvs2c-translated-theories)))))

(defun create-config-make (c-files)
  (let ((makefile (merge-pathnames "src/Makefile" *pvs2c-library-path*)))
    (with-open-file (mf makefile
			:direction :output :if-exists :supersede :if-does-not-exist :create)
      (case (intern (pvs-platform) :pvs)
	(|ix86_64-Linux|
	 (format mf "CFLAGS := -I ../include -fPIC -Wall -Winline -O -ggdb~%")
	 (format mf "LDFLAGS = -Bsymbolic -shared -L./ -lc -lm -lgmp~%"))
	(|ix86-MacOSX|
	 (format mf "CFLAGS := -dynamic -DNDEBUG -arch x86_64 -I ../include~%")
	 (format mf "LDFLAGS = -bundle -flat_namespace -undefined suppress -L./ -lc -lm -lgmp~%"))
	(|arm-MacOSX|
	 (format mf "SDK=$(shell xcrun --show-sdk-path)")
	 (format mf "CFLAGS := -g -O2 -Wall -pedantic -std=gnu99 -mtune=native -mcpu=apple-a14 -I ../include")
	 (format mf "LDFLAGS = -dylib -flat_namespace -undefined suppress -arch arm64 -platform_version macos 11.0.0 12.0 -L $(SDK)/usr/lib -L./ -lc -lm -lgmp~%")))
      (format mf "~%src := ~{~a~^ ~}~%" c-files)
      (format mf "~%obj := $(src:.c=.o)~%")
      (format mf "~%.c.o : ; $(CC) ${CFLAGS} -c $< -o $@~%")
      (format mf "~%all : ../lib/libpvs-prelude.so ../lib/libpvs-prelude.a~%")
      (format mf "~%../lib/libpvs-prelude.so : ${obj}~%")
      (format mf "~a$(LD) $(LDFLAGS) -o $@ ${obj}~%" #\tab)
      (format mf "~%../lib/libpvs-prelude.a : $(obj)~%")
      (format mf "~a$(AR) r $@ ${obj}~%" #\tab)
      (format mf "~%clean :~%")
      (format mf "~a-rm $(obj)~%" #\tab)
      (format mf "~a-rm ../lib/libpvs-prelude.so ../lib/libpvs-prelude.a~%" #\tab))
    (format t "~%Generated ~a" makefile)))
  

;; (defun used-prelude-theory-names (theory &optional prelude-theory-names)
;;   (let ((th (get-theory theory)))
;;     (unless th
;;       (error "Theory ~a not found - may need to typecheck first" theory))
;;     (dolist (decl (all-decls th))
;;       (when (typep decl '(or type-decl const-decl))
;; 	(dolist (d (refers-to decl))
;; 	  (when (and (typep d '(or type-decl const-decl))
;; 		     (from-prelude? d))
;; 	    (unless (member (module d) prelude-theory-names :test #'same-id)
;; 	      (setq prelude-theory-names
;; 		    (used-prelude-theory-names (module d)
;; 					       (cons (id (module d)) ;(mk-modname (id (module d)))
;; 						     prelude-theory-names))))))))
;;     prelude-theory-names))

;;from-prelude? is a better function
;; (defun prelude-theory? (theory)
;;   (memq theory *prelude-theories*))

;; (defun pvs2c-preceding-prelude-theories (theory);theory should not be a prelude theory
;;   (let ((*preceding-prelude-theories* nil)
;; 	(theory-defn (get-theory theory)))
;;     (unless (memq (id theory-defn) *primitive-prelude-theories*)
;;       (pvs2c-preceding-prelude-theories* theory-defn));(break "precedprelude")
;;     *preceding-prelude-theories*))

;; (defun pvs2c-preceding-prelude-theories-root (theory)
;;   (loop for thy in (used-prelude-theories theory)
;; 	when (not (memq (id thy) *primitive-prelude-theories*))
;; 	do (pvs2c-preceding-prelude-theories* thy))
;;   (when (prelude-theory? theory)
;;     (pushnew theory *preceding-prelude-theories* :test #'same-id)))

;; (defun pvs2c-preceding-prelude-theories* (theory)
;;   (let ((imported-prelude-theories
;; 	 (if (from-prelude? theory)
;; 	     (implicit-prelude-importings theory)
;; 	   (used-prelude-theories theory))))
;;   (loop for thy in imported-prelude-theories
;; 	when (not (memq (id thy) *primitive-prelude-theories*))
;; 	do (pvs2c-preceding-prelude-theories* thy))
;;   (unless (from-prelude? theory)
;;     (let ((all-imported-theories (all-imported-theories theory)))
;;       (when (listp all-imported-theories);i.e., not 'unbound
;; 	(loop for thy in all-imported-theories
;; 	      when (not (from-prelude? thy))
;; 	      do (pvs2c-preceding-prelude-theories* thy)))))
;;   (when (from-prelude? theory)
;;     (pushnew theory *preceding-prelude-theories* :test #'same-id))))

;; (defun pvs2c-preceding-theories (theory)
;;   (let ((*pvs2c-preceding-theories* nil)
;; 	(theory-defn (get-theory theory)))
;;     (pvs2c-preceding-theories* theory-defn)
;;       *pvs2c-preceding-theories*))

;; (defmethod pvs2c-preceding-theories* ((theory module))
;;   (unless (eq (all-imported-theories theory) 'unbound)
;;     (loop for thy in (all-imported-theories theory)
;; 	  do (pvs2c-preceding-theories* thy)))
;;   (unless (from-prelude? theory)
;;     (pushnew theory *pvs2c-preceding-theories* :test #'same-id)))

;; (defmethod pvs2c-preceding-theories* ((theory datatype))
;;   (with-slots (adt-theory adt-map-theory adt-reduce-theory all-imported-theories) theory
;;     (unless (eq (all-imported-theories theory) 'unbound)
;;       (loop for thy in all-imported-theories
;; 	    do (pvs2c-preceding-theories* thy)))
;;     ;;pushing is enough since the importings are already pushed in above, otherwise we have a circularity
;;     (when adt-theory (pushnew adt-theory *pvs2c-preceding-theories* :test #'same-id))
;;     (when adt-map-theory (pushnew adt-map-theory *pvs2c-preceding-theories* :test #'same-id))
;;     (when adt-reduce-theory (pushnew adt-reduce-theory *pvs2c-preceding-theories* :test #'same-id))))

(defun pvs2c-library (&rest top-theory-refs)
  (unless top-theory-refs (error "pvs2-library: takes at least one theory-ref"))
  (let* ((top-theories (mapcar #'get-typechecked-theory top-theory-refs))
	 (*pvs2c-library-path* (context-path (car top-theories))))
    (unless (every #'(lambda (th) (equalp (context-path th) *pvs2c-library-path*))
		   (cdr top-theories))
      (error "All top theories must be from the same library"))
    (ensure-c-directories-exist)
    (dolist (top-theory top-theories)
      (dolist (theory (all-importings top-theory))
	(pvs2c-theory theory))
      (pvs2c-theory top-theory))))

(defun pvs2c-pvs-file (fileref)
  (let ((theories (typecheck-file fileref nil nil nil t))
	;; (*pvs2c-preceding-theories* nil)
	)
    ;; (dolist (theory theories)
    ;;   (pvs2c-preceding-theories* theory))
    ;;(pvs2c-theories *pvs2c-preceding-theories* t)
    (pvs2c-theories theories t)))

(defun pvs2c-theories (theories force?)
  (mapcar #'(lambda (th) (pvs2c-theory* th force?)) (reverse theories)))

(defun pvs2c-theory (theoryref &optional force?)
  (let* ((theory (get-typechecked-theory theoryref nil t)))
    (with-context theory
      (pvs2c-theory* theory force?))))

;; Null dir means not making a library (e.g., source file is in pvs2c subdirectory
(defmethod pvs2c-theory* ((rectype recursive-type) &optional force?)
  (let ((th (adt-theory rectype)))
    (with-workspace th
      (pvs2c-theory-body th force?))))

(defmethod pvs2c-theory* ((th module) &optional force?)
  (with-workspace th
    (pvs2c-theory-body th force?)))

(defun pvs2c-theory-body (theory &optional force? indecl);;*theory-id* needs to be bound by caller
  (let* ((*theory-id* (simple-id (id theory)))
	 ;; (*preceding-mono-theories* nil) ; monomorphised theory instances used in this theory
	 )
    (pvs2c-theory-body-step theory force? indecl)))

(defun pvs2cl-theory-formals (formals force?
			      &optional theory-formals ir-theory-formals ir-theory-tbindings)
  (cond ((null formals)
	 (values theory-formals ir-theory-formals ir-theory-tbindings))
	(t (let ((formal (car formals)))
	     (let ((*current-context* (decl-context formal))
		   (*var-counter* nil))
	       (newcounter *var-counter*)
	       (pvs2c-decl formal force?))
	     (let ((irformal
		    (cond ((formal-const-decl? formal)
			   (ir-defn (ir (eval-info formal))))
			  ((formal-type-decl? formal)
			   (let ((ir-type-value (ir-type-value formal)))
			     (or (ir-const-formal (ir-type-name ir-type-value))
				 (setf (ir-const-formal (ir-type-name ir-type-value))
				       (mk-ir-const-formal (ir-type-id (ir-type-name (ir-type-value formal)))
							   *type-actual-ir-name*))))))))
	       ;;(setq *theory-formals* (nconc *theory-formals* (list formal)))
	       ;;(setq *ir-theory-formals* (nconc *ir-theory-formals* (list irformal)))
	       ;; (format t "~%pushing binding")
	       ;;(setq *ir-theory-tbindings*
	       ;;  (nconc *ir-theory-tbindings* (list (cons formal irformal))))
	       (pvs2cl-theory-formals
		(cdr formals) force?
		(nconc theory-formals (list formal))
		(nconc ir-theory-formals (list irformal))
		(nconc ir-theory-tbindings (list (cons formal irformal)))))))))

(defun pvs2c-theory-body-step (theory force? indecl)
  (let* ((*current-pvs2c-theory* theory)
	 (formals (formals-sans-usings theory))
	 (*theory-formals* nil) ;;using the special vars as accumulators in pvs2cl-theory-formals for processed parameters
	 (*ir-type-info-table* nil)
	 (*c-type-info-table* nil)
	 ;;(*closure-info-table* nil) ;;not currently used
	 ;;(formal-ids (loop for decl in *theory-formals* do (pvs2ir-decl decl)))
	 (*theory-type-formals* (loop for formal in *theory-formals* when  (formal-type-decl? formal) collect formal))
	 (*ir-theory-formals* nil)
	 (*ir-theory-tbindings* nil)
	 (*pvs2c-theory-importings* nil)
	 (*pvs2c-theory-decls* nil)
	 ;; (*c-theory-formals*
	 ;;  (ir2c-theory-formals *ir-theory-formals* *theory-formals*))
	 )				;(break "body-step")
    (multiple-value-bind (*theory-formals* *ir-theory-formals* *ir-theory-tbindings*)
	(pvs2cl-theory-formals formals force?)
      (when force? (setf (ht-instance-clone theory) nil))
      (if (eq *theory-id* '|modulo_arithmetic|)
	  (let ((decl (find-if #'(lambda (d) (and (const-decl? d) (eq (id d) '|rem|)))
			(theory theory))))
	    (assert decl () "Couldn't find rem in modulo_arithmetic")
	    (pvs2c-decls (list decl) indecl force?))
	  (pvs2c-decls (theory theory) indecl force?)
	  ;; (loop for decl in (theory theory)
	  ;;       when (if (null indecl)
	  ;; 	       (or (const-decl? decl) ;(or (adt-operation? decl)(def-axiom decl))
	  ;; 		   (type-decl? decl)
	  ;; 		   (adt-type-decl? decl)
	  ;; 		   (test-formula? decl))
	  ;; 	       (eq decl indecl))
	  ;;       do (pvs2c-decl decl force?))
	  )
      (cond (*pvs2c-theory-decls*
	     (print-header-file *theory-id* theory)
	     (print-body-file *theory-id* theory))
	    (t (format t "~%No C code generated for theory ~a, since no declarations were translatable"
		 (id theory)))))))

(defun pvs2c-decls (decls indecl force?)
  (when decls
    (let ((decl (car decls)))
      (when (or (eq decl indecl)
		(typep decl '(or const-decl type-decl test-formula))
		(adt-type-decl? decl))
	(handler-case (progn (pvs2c-decl decl force?)
			     (push decl *pvs2c-theory-decls*)
			     (dolist (ref (refers-to decl))
			       (unless (or (eq (module decl) (module ref))
					   (and (from-prelude? (module ref))
						(let ((mod-id (or (generated-by (module ref))
								  (id (module ref)))))
					    	  (not (member mod-id
							       *pvs2c-prelude-theories*)))))
				 (pushnew (module ref) *pvs2c-theory-importings*))))
	  (pvs2c-error (c)
	    (let ((warning (format nil "~%No C code generated for ~a~%  ~a" (id decl) c)))
	      (push (cons decl warning) (pvs2c-warnings (module decl)))
	      (warn warning))))))
    (pvs2c-decls (cdr decls) indecl force?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pvs2c-decl (decl force?)
  (let (;;(saved-c-type-info-table *c-type-info-table*)
	(*pvs2c-current-decl* decl) ;;used to save auxilliary type defns in c-type-info-table
	(*current-context* (decl-context decl))
	(*var-counter* nil)
	(*pvs2c-defn-actuals* nil))
    (when force? (clear-decl decl))
    (newcounter *var-counter*)
    (pvs2c-decl* decl)))

;;NSH(8/6/2016): Closures are handled now. 
;; (handler-case
;;  (pvs2c-decl* decl)
;;  (pvs2c-error (condition) (format t "~%closures not handled")
;; 		  (setq *c-type-info-table* saved-c-type-info-table)))))
  
(defmethod pvs2c-decl* ((decl type-eq-decl))
  (let ((typename (pvs2ir-decl decl)))
    ;(break "type-eq-decl")
    (when  (and (ir-typename? typename)
		(ir-type-value decl));some type definitions translate to no-ops
      (add-c-type-definition typename)))); (ir2c-type (ir-type-defn typename))(ir-type-id typename)))))

(defmethod pvs2c-decl* ((decl type-decl)) ;;has to be an adt-type-decl
  (let* (;(thid (simple-id (id (module decl))))
	 (declid (simple-id (id decl)))
	 (thname (intern (format nil "~a__~a" *theory-id* declid)))
	 (hashentry (gethash  thname *c-primitive-type-attachments-hash*)))
    (cond (hashentry
	   (unless *to-emacs*
	     (format t "~% attaching definition for type ~a" declid))
	   (push-type-info-to-decl hashentry decl)
	   thname)
	  (t (let ((typename (pvs2ir-decl* decl)))
	       (if (adt (type-value decl))
		   (add-c-type-definition (ir2c-type (ir-type-defn typename))(ir-type-id typename))
		 (let* ((ir-type-id (ir-type-id typename))
			(c-type-info (mk-simple-c-type-info nil ir-type-id
							    (format nil "//uninterpreted type~%typedef void * ~a_t;" ir-type-id) nil (format nil "~s" (id *pvs2c-current-decl*)))))
		   (push-type-info-to-decl c-type-info decl)
		   ir-type-id)))))))

(defmethod pvs2c-decl* ((decl formal-type-decl))
   (pvs2ir-decl decl))

(defmethod pvs2c-decl* ((decl formal-const-decl))
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl))))
    (ir2c-decl* ir decl)))
  
;;conversion for a definition
(defmethod pvs2c-decl* ((decl const-decl))
  (let* (;(thid (simple-id (id (module decl))))
	 (declid (simple-id (id decl)))
	 (ir-function-id (intern (format nil "~a__~a" *theory-id* declid)))
	 (hashentry (gethash ir-function-id *c-primitive-attachments-hash*)))
    (cond (hashentry
	   (let* ((einfo (or (eval-info decl)
			     (let* ((new-einfo (make-instance 'eval-info))
				    )	;all nil for now
			       (setf (eval-info decl) new-einfo)
			       new-einfo)))
		  (new-ir-function (mk-ir-function ir-function-id decl))
		  (new-ir (mk-ir-defn new-ir-function nil nil nil)))
	     (setf (ir einfo) new-ir)
	     (setf (cdefn einfo) hashentry)
	     (op-name hashentry)))
	  (t (unless (or (adt-constructor-decl? decl) ;;these are automatically generated from the adt type decl
			 (adt-recognizer-decl? decl)
			 (adt-accessor-decl? decl))
	       (pvs2ir-decl decl))
	     (let ((ir (ir (eval-info decl)))) ;(break "pvs2c-decl*/const-decl")
	       (ir2c-decl* ir decl))))))

(defmethod pvs2c-decl* ((decl test-formula))
  (pvs2ir-decl decl)
  (let ((ir (ir (eval-info decl)))) ;(break "pvs2c-decl*/const-decl")
    (ir2c-decl* ir decl)))

(defun c-args-string (ir-vars)
  (let* ((c-arg-types (loop for arg in ir-vars
			    collect (add-c-type-definition (ir2c-type (ir-vtype arg)))))
	 (c-args (loop for arg in ir-vars
		       as c-arg-type in c-arg-types
		       collect (format nil "~a_t ~a"
				       (mppointer-type c-arg-type)
				       (ir-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void"))))
    c-args-string))


(defun def-c-attach-primitive-type (theory name header-defn)
  (let* ((thname (make-c-name (simple-id theory) (simple-id name)))
	 (header (format nil "typedef ~a ~a_t;" header-defn thname))
	 (c-type-info (mk-simple-c-type-info nil thname header nil nil) ))
    (setf (gethash thname *c-primitive-type-attachments-hash*) c-type-info)
    thname))
    
	 

(defun def-c-attach-primitive (theory name return-type args arg-types header-defn &optional definition)
  (let* ((thname (make-c-name (simple-id theory)(simple-id name)))
	 (arg-type-pairs (loop for arg in args as
			       arg-type in arg-types
			       collect (format nil "~a_t ~a" arg-type arg)))
	 (arg-string (if (null arg-type-pairs)
			 (format nil "(void)")
			 (format nil "(~{~a~^, ~})" arg-type-pairs)))
	 (header (if definition (format nil "~a_t ~a~a;" return-type thname arg-string)
		     (format nil "static inline ~a_t ~a~a~a" return-type thname arg-string header-defn)))
	 (c-definition (when definition (format nil "~a_t ~a~a~a" return-type thname arg-string definition)))
	 (c-defn-info (mk-c-noextern-defn-info thname header c-definition arg-types return-type)))
    (setf (gethash thname *c-primitive-attachments-hash* ) c-defn-info)
    thname))


(defun make-c-defn-info (ir pvs-return-type) ;(break "make-c-defn-info")
  (with-slots
	(ir-function-name ir-return-type ir-args ir-defn) ir
    ;; (when (eq (id decl) 'coef)(break "coef"))
    ;;   (format t "~%ir-defn~% =~a" (print-ir ir-defn))
    (if (null ir-defn)
	(let* ((ir-function-name (ir-fname ir-function-name))
	       ;;(ir-result-type ir-return-type) ;(pvs2ir-type (type decl))
	       ;; (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	       ;; (dummy (when (null c-result-type) (break "make-c-defn-info")))
	       ;;might need to adjust c-header when result type is gmp
	       ;; (c-header (format nil "~a_t ~a(~a)"
	       ;; 		   (mppointer-type c-result-type) ir-function-name
	       ;; 		   (c-args-string ir-args)))
	       )
	  (unless *to-emacs* ;; causes problems
	    (format t "~%No definition for ~a" ir-function-name))
	  nil)
	  ;; (mk-c-defn-info ir-function-name (format nil "~a;" c-header) nil nil
	  ;; 		  c-result-type)
	(let* ((ir-function-name (ir-fname ir-function-name))
	       (pre-ir (if ir-args
			   (mk-ir-lambda ir-args ir-return-type ir-defn)
			   ir-defn))
	       (post-ir (preprocess-ir pre-ir))
	       (ir-args (when (ir-lambda? post-ir)
			  (ir-vartypes post-ir)))
					;	     (ir-decl-type (add-c-type-definition (ir2c-type (pvs2ir-type (declared-type decl)))))
	       (ir-result-type (if (ir-lambda? post-ir)
				   (ir-rangetype post-ir) ;(pvs2ir-type (range (find-supertype (type decl))))
				   (pvs2ir-type pvs-return-type)))
	       (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	       (c-arg-types (loop for arg in ir-args
			       collect (mppointer-type (add-c-type-definition (ir2c-type (ir-vtype arg))))))
	       (*mpvar-parameters*  (loop for arg in ir-args
				       as c-arg-type in c-arg-types
				       when (mpnumber-type? c-arg-type)
				       collect arg))
	       (c-args-string (c-args-string ir-args))
	       (c-header (format nil "~a_t ~a(~a)" (mppointer-type c-result-type) ir-function-name c-args-string))
	       ;; (case c-result-type
	       ;; 		 ((|mpz| |mpq|)
	       ;; 		  (if (consp c-args)
	       ;; 		      (format nil void ~a(~a_t ~a, ~a)"
	       ;; 			      ir-function-name c-result-type '|result| c-args-string)
	       ;; 		    (format nil void ~a(~a_t ~a)"
	       ;; 			      ir-function-name c-result-type '|result|)))
	       ;; 		 (t (format nil ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	       (ir-body (if (ir-lambda? post-ir)
			    (ir-body post-ir)
			    post-ir))
	       (c-body (print2c (ir2c ir-body ir-result-type)))
	       (c-body (if ir-args c-body
			 (format nil "~%~8Tstatic bool_t defined = false;~%~8Tif (!defined){~%~12T~a~%~8Tdefined = true;};" c-body)))
	       (static? (if ir-args "" " static "))
	       (c-result-decl (if (mpnumber-type? c-result-type)
				  (let ((mptype (mppointer-type c-result-type)))
				    (format nil "~a_t ~a result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
				      mptype static?; c-result-type c-result-type
				      ))
				  (format nil "~a_t ~a result;" c-result-type static?)))
	       (c-defn  (format nil "~a{~%~8T~a~%~a~%~8T~%~8Treturn result;~%}"
			  c-header 
			  c-result-decl
			  c-body
			  ;; (if (ir-reference-type? ir-result-type)
			  ;;     "result->count++;"
			  ;;   "")
			  ))
	       ;; (case c-result-type
	       ;; 	       ((|mpq| |mpz|)
	       ;; 		(format nil "~a{~%~a~%}"
	       ;; 		     c-header 
	       ;; 		     c-body))
	       ;; 	       (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	       ;; 		     c-header 
	       ;; 		     (mppointer-type c-result-type)
	       ;; 		     c-body)))
	       )
	  
	  (unless *suppress-output* ;*to-emacs* ;; causes problems
	    (format t "~%Function ~a"  ir-function-name)
	    ;(format t "~%MPvars ~{~a, ~}" (print-ir *mpvar-parameters*))
	    (format t "~%Before  preprocessing = ~%~a" (print-ir pre-ir))	   
	    (format t "~%After preprocessing = ~%~a" (print-ir post-ir))
	    (format t "~%Generates C definition = ~%~a" c-defn))
	  (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn c-arg-types
			  c-result-type)))))

(defun make-c-closure-defn-info (ir-lambda-expr ir-function-name c-param-decl-string)
  (declare (ignore c-param-decl-string))
  (let* ((ir-args (ir-vartypes ir-lambda-expr))
	 (ir-result-type (ir-rangetype ir-lambda-expr)) ;(pvs2ir-type (range (find-supertype (type decl))))
	 (c-result-type (add-c-type-definition (ir2c-type ir-result-type)))
	 (fvars (pvs2ir-freevars* ir-lambda-expr))
	 (args-fvarargs (append ir-args fvars))
	 (c-arg-types (loop for arg in args-fvarargs
		       collect (add-c-type-definition (ir2c-type (freevar-type arg)))))
	 (c-args (loop for arg in args-fvarargs
		       collect (format nil "~a_t ~a"
				       (mppointer-type (add-c-type-definition (ir2c-type (freevar-type arg))))
				       (freevar-name arg))))
	 (c-args-string (if (consp c-args)
			    (format nil "~{~a~^, ~}" c-args)
			  (format nil "void")))
	 (c-header (format nil "~a_t ~a(~a)" (mppointer-type c-result-type)
			   ir-function-name c-args-string))
			   ;;ignoring params since they are in the free vars;;c-param-decl-string
	 ;; (case c-result-type
	 ;; 	     ((|mpz| |mpq|)
	 ;; 	      (if (consp c-args)
	 ;; 		  (format nil void ~a(~a_t ~a, ~a)"
	 ;; 		      ir-function-name c-result-type '|result| c-args-string)
	 ;; 		  (format nil void ~a(~a_t ~a)"
	 ;; 		      ir-function-name c-result-type '|result|)))
	 ;; 	     (t (format nil ~a_t ~a(~a)" c-result-type ir-function-name c-args-string)))
	 (c-defn-arg-types c-arg-types)
	 ;; (case c-result-type
	 ;; 		     ((|mpz| |mpq|)(cons c-result-type c-arg-types))
	 ;; 		     (t c-arg-types))
	 (c-defn-result-type (mppointer-type c-result-type))
	     ;; (case c-result-type
	     ;; 		       ((|mpz| |mpq|) 'void)
	     ;; 		       (t c-result-type))
	 (ir-body (ir-body ir-lambda-expr))
	 (*mpvar-parameters*  (loop for arg in ir-args
				    as c-arg-type in c-arg-types 
				    when (mpnumber-type? c-arg-type)
				    collect arg))
	 (c-body (print2c (ir2c ir-body ir-result-type)))
	 (c-result-decl (case c-result-type
			  ((|mpq| |mpz|)
			   (format nil "~a_t result;" ; = safe_malloc(sizeof(~a_t)); ~a_init(result);"
				   (mppointer-type c-result-type); c-result-type c-result-type
				   ))
			  (t (format nil "~a_t result;" c-result-type))))
	 (c-defn  (format nil "~a{~%~8T~a~%~a~%~8Treturn result;~%}"
			  c-header 
			  c-result-decl
			  c-body))
	 ;; (case c-result-type
	 ;; 	   ((|mpq| |mpz|)
	 ;; 	    (format nil "~a{~%~a~%}" c-header c-body))
	 ;; 	   (t (format nil "~a{~%~8T~a_t result;~%~a~%~8Treturn result;~%}"
	 ;; 		      c-header
	 ;; 		      c-result-type
	 ;; 		      c-body)))
	 )
    (unless *suppress-output* ;;*to-emacs*
      (format t "~%Closure After preprocessing = ~%~a" (print-ir ir-lambda-expr))
      (format t "~%Generates C definition = ~%~a" c-defn))
    ;(break "make-c-closure-defn")
    (mk-c-defn-info ir-function-name (format nil "~a;" c-header) c-defn
		    c-defn-arg-types c-defn-result-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;print the header file from the information in *c-type-info-table*
;;If the type has a PVS definition, the type-info is saved with this declaration.
;;Otherwise, the type information is generated for the type expression in the global.

(defun print-header-file (theory-id theory)
  (let* ((file-string (if *pvs2c-library-path*
			  (format nil "~a/include/~a_c.h" *pvs2c-library-path* theory-id)
			  (format nil "~a_c.h" theory-id)))
	 ;; (preceding-theories (pvs2c-preceding-theories theory))
	 ;; (preceding-prelude-theories (pvs2c-preceding-prelude-theories theory))
	 ;; (theory-instances (when (ht-instance-clone theory)
	 ;; 		     (maphash #'(lambda (x y) x) (ht-instance-clone theory))))
	 )				;(break "print-header-file")
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede :if-does-not-exist :create)
      (format output "//Code generated using pvs2ir")
      (format output "~%#ifndef _~a_h ~%#define _~a_h" theory-id theory-id)
      (format output "~%~%#include <stdio.h>")
      (format output "~%~%#include <stdlib.h>")
      (format output "~%~%#include <inttypes.h>")
      (format output "~%~%#include <stdbool.h>")
      (format output "~%~%#include <stdarg.h>")		    
      (format output "~%~%#include <string.h>")
      (format output "~%~%#include <fcntl.h>")		    
      (format output "~%~%#include <math.h>")
      (format output "~%~%#include <sys/mman.h>")
      (format output "~%~%#include <sys/stat.h>")
      (format output "~%~%#include <sys/types.h>")
      (format output "~%~%#include <gmp.h>")
      (format output "~%~%#include \"pvslib.h\"")
      (dolist (th *pvs2c-theory-importings*)
	(format output "~%~%#include \"~a_c.h\"" (id th)))
      ;; (format output "~%~%#include \"~a_c.h\"" (id theory))
      ;; (format t "~%mono-theories: ~{ ~a~^,~}" *preceding-mono-theories*)
      ;; (loop for thy in  *preceding-mono-theories*
      ;; 	  do (format output "~%~%#include \"~a_c.h\"" (id thy)))
      (format output "~%~%//cc -O3 -Wall -o ~a" theory-id )
      (format output " -I ~a/include" *pvs-path*)
      (format output " ~a/lib/pvslib.c " *pvs-path*)
      (format output " -I ~alib" *pvs-path*)
      ;; (loop for thy in preceding-prelude-theories
      ;; 	    do (format output " ~alib/~a_c.c" *pvs-path* (id thy)))
      ;; (loop for thy in preceding-prelude-theories
      ;; 	    do (when (ht-instance-clone thy)
      ;; 		 (maphash #'(lambda (x y)
      ;; 			      (declare (ignore y))
      ;; 			      (format output  " ~a_c.c" x))
      ;; 			  (ht-instance-clone thy))))
      ;; (loop for thy in preceding-theories
      ;; 	    do (format output " ~a_c.c" (id thy))
      ;; 	    (when (ht-instance-clone thy)
      ;; 	      (maphash #'(lambda (x y)
      ;; 			   (declare (ignore y))
      ;; 			   (format output  " ~a_c.c" x))
      ;; 		       (ht-instance-clone thy))))
      ;; (loop for thy in  *preceding-mono-theories*
      ;; 	  do (format output " ~a_c.c" (id thy)))
      (format output " -lgmp ")
      ;; (when (formals theory) (format output "~%typedef pointer_t"))
      (loop for formal in (formals theory)
	    when (formal-type-decl? formal)
	    do (format output "~%~%typedef pointer_t ~a_t;" (ir-type-id (ir-type-name (ir-type-value formal)))))
      (print-type-info-headers-to-file output *c-type-info-table*)
      (loop for decl in (theory theory)
	    when (and (slot-exists-p decl 'eval-info)
		      (eval-info decl)
		      (cdefn (eval-info decl)))
	    do (print-header-decl decl output))
      (format output "~%#endif")
      (unless *to-emacs*
	(format t "~%Wrote ~a" file-string))
      (id theory))))

(defun print-header-decl (decl output)
  (let ((einfo (eval-info decl)))
    (when (accessor-eval-info? einfo)
      (format output "~%~%~a" (op-header (update-cdefn (eval-info decl)))))
    (format output "~%~%~a" (op-header (cdefn (eval-info decl))))))

(defun print-type-info-headers-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-headers-to-file output (cdr type-info-stack))
	 (print-type-defn-headers (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defn-headers ((type-info simple-c-type-info) output)
  (when (comment-string type-info) (format output "~%//~a" (comment-string type-info)))
  (format output "~%~%~a~%~%"
	  (tdefn type-info)))

(defmethod print-type-defn-headers ((type-info closure-c-type-info) output)
  (with-slots (tdecl tdefn ftable-type-defn release-info hash-entry-type-defn hash-type-defn
		     copy-info lookup-info dupdate-info update-info equal-info json-info comment-string)
      type-info
    (when comment-string (format output "~%//~a" comment-string))
    (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	    tdecl ftable-type-defn hash-entry-type-defn hash-type-defn tdefn)
    (format output "~a~%~%" (op-header release-info))
    (format output "~a~%~%" (op-header copy-info)) ;copy is required, but functions below are optional
    (when lookup-info (format output "~a~%~%" (op-header lookup-info)))
    (when dupdate-info (format output "~a~%~%" (op-header dupdate-info)))
    (when update-info (format output "~a~%~%" (op-header update-info)))
    (when equal-info (format output "~a~%~%" (op-header equal-info)))
    (when json-info (format output  "~a~%~%" (op-header json-info)))
    ))

(defmethod print-type-defn-headers ((type-info c-type-info) output)
  (when (comment-string type-info) (format output "~%//~a" (comment-string type-info)))
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~a~%~%~a~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdefn type-info)
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  (op-header (equal-info type-info))
	  (if (json-info type-info)(op-header (json-info type-info))(format nil " "))
	  (if (act-defn type-info)(ir-actual-type-defn (act-defn type-info))(format nil " "))
	  (if (release-ptr-info type-info)(op-header (release-ptr-info type-info)) (format nil " "))
	  (if (equal-ptr-info type-info)(op-header (equal-ptr-info type-info)) (format nil " "))
	  (if (json-ptr-info type-info)(op-header (json-ptr-info type-info)) (format nil " "))	  
	  (if (act-defn type-info)(ir-actual-fun-header (act-defn type-info)))(format nil " ")) 
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-header t-info)))
  (loop for t-info in (upgrade-info type-info)
	do (format output "~a~%~%" (op-header t-info))))

(defmethod print-type-defn-headers ((type-info c-closure-info) output)
  (format output "~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (tdecl type-info)
	  (tdefn type-info)
	  (op-header (fdefn type-info))
	  (op-header (mdefn type-info))
	  (op-header (hdefn type-info))	  
	  (op-header (new-info type-info))
	  (op-header (release-info type-info))
	  (op-header (copy-info type-info))
	  ))

(defun print-type-info-defns-to-file (output type-info-stack)
  (cond ((consp type-info-stack)
	 (print-type-info-defns-to-file output (cdr type-info-stack))
	 (print-type-defns (car type-info-stack) output))
	(t nil)))

(defmethod print-type-defns ((type-info simple-c-type-info) output)
  (declare (ignore output))
  nil); do nothing

(defmethod print-type-defns ((type-info closure-c-type-info) output)
  (with-slots (release-info copy-info lookup-info dupdate-info update-info equal-info json-info) type-info
	      (format output "~%~%~a" (op-defn release-info))
	      (format output "~%~%~a" (op-defn copy-info))
	      (when lookup-info (format output "~%~%~a" (op-defn lookup-info)))
	      (when dupdate-info (format output "~%~%~a" (op-defn dupdate-info)))
	      (when update-info (format output "~%~%~a" (op-defn update-info)))
	      (when equal-info (format output "~%~%~a" (op-defn equal-info)))
	      (when json-info (format output "~%~%~a" (op-defn json-info)))))


(defmethod print-type-defns ((type-info c-type-info) output)
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%"
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (if (release-ptr-info type-info)(op-defn (release-ptr-info type-info))(format nil " "))
	  (op-defn (copy-info type-info))
	  (op-defn (equal-info type-info))
	  (if (json-info type-info)(op-defn (json-info type-info))(format nil " "))
	  (if (equal-ptr-info type-info)(op-defn (equal-ptr-info type-info))(format nil " "))
	  (if (json-ptr-info type-info)(op-defn (json-ptr-info type-info))(format nil " "))
	  (if (act-defn type-info)(ir-actual-fun-defn (act-defn type-info))(format nil " ")))
  (loop for t-info in (update-info type-info)
	do (format output "~a~%~%" (op-defn t-info)))
  (loop for t-info in (upgrade-info type-info)
	do (format output "~a~%~%" (op-defn t-info))))

(defmethod print-type-defns ((type-info c-closure-info) output);(break "closure-info")
  (format output "~%~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a~%~%~a"
	  (op-defn (fdefn type-info))
	  (op-defn (mdefn type-info))	  
	  (op-defn (hdefn type-info))
	  (op-defn (new-info type-info))
	  (op-defn (release-info type-info))
	  (op-defn (copy-info type-info))
))

(defun print-body-file (theory-id theory)
  "Generates the .c file for the given theory, returning the C file name if
successful."
  (let* ((file-string (if *pvs2c-library-path*
			  (format nil "~a/src/~a_c.c" *pvs2c-library-path* theory-id)
			  (format nil "~a_c.c" theory-id)))
	 (file-path (format nil "~a" (working-directory))))
    (with-open-file (output file-string :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (format output "//Code generated using pvs2ir2c")
      (format output "~%#include \"~a_c.h\""  theory-id)
      (print-type-info-defns-to-file output *c-type-info-table*)
      (loop for decl in (theory theory)
	 do (let ((einfo (and (slot-exists-p decl 'eval-info) (eval-info decl))))
	      (when (and einfo (cdefn einfo))
		(let ((op-defn (op-defn (cdefn einfo))))
		  (when op-defn
		    (when (accessor-eval-info? einfo)
		      (format output "~%~%~a" (op-defn (update-cdefn (eval-info decl)))))
		    (format output "~%~%~a" (op-defn (cdefn (eval-info decl)))))))))
      (unless *to-emacs*
	(format t "~%Wrote ~a" file-string))
      (concatenate 'string file-path file-string))))

(defparameter *usable-prelude-theories*
  '(|xor_def| |real_defs| |exp2| |integertypes| |sequences| |more_finseq| |ordstruct_adt|
    |ordinals| |lex2| |strings| |gen_strings| |bytestrings| |empty_bv|
    |integer_bv_ops|))

(defun make-c-tests-main (name &optional (path *default-pathname-defaults*))
  (with-workspace path
    (let ((filename (format nil "~a.c" name))
	  (test-formulas (collect-test-formulas)))
      (unless test-formulas
	(error "No test-formulas found in typechecked theories of ~a" path))
      (with-open-file (main filename :direction :output :if-exists :supersede)
	(format main "// test main ~a generated by make-c-tests-main" name)
	(format main "~2%// Includes")
	(dolist (th (remove-duplicates (mapcar #'module test-formulas)))
	  (format main "~%#include \"~a_c.h\"" (id th)))
	(format main "~2%int main () {")
	(dolist (tform test-formulas)
	  (format main "~2%~Tprintf(\"~a.~a ==> %s\\n\", ~a() ? \"true\" : \"false\");"
	    (id (module tform)) (id tform) (op-name (cdefn (eval-info tform)))))
	(format main "~%}~%")))))

(defun collect-test-formulas ()
  (let ((th-hash (current-pvs-theories))
	(tforms nil))
    (dolist (k (sort (get-hash-keys th-hash) #'string-lessp))
      (let ((th (gethash k (current-pvs-theories))))
	(setf tforms
	      (append tforms (remove-if-not #'test-formula? (all-decls th))))))
    tforms))
