;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros.lisp -- 
;; Author          : Sam Owre
;; Created On      : Sun Jan  9 18:44:56 1994
;; Last Modified By: Sam Owre
;; Last Modified On: Fri Dec 14 13:20:02 2012
;; Update Count    : 16
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

(export '(length= singleton? add-to-alist makesym get-declarations put-decl
	  get-importings))

(defmacro tcdebug (ctl &rest args)
  `(when *tcdebug*
     (if *to-emacs*
	 (pvs-message ,ctl ,@args)
	 (format t ,ctl ,@args))))

(defmacro makesym (ctl &rest args)
  `(intern (format nil ,ctl ,@args) :pvs))

(defmacro makenewsym (ctl &rest args)
  (let ((str (gentemp)))
    `(let ((,str (format nil ,ctl ,@args)))
       (make-new-symbol ,str))))

(defmacro msgtime (ctl &rest args)
  `(progn (format t ,ctl ,@(butlast args)) (time ,(car (last args)))))

(defmacro length= (l1 l2)
  `(= (length ,l1) (length ,l2)))

(defmacro singleton? (obj)
  `(and (consp ,obj) (null (cdr ,obj))))

;;; Like typep, but a little faster.
;;; Can only be used on instances of classes.

(defmacro typec (inst class-name)
  `(eq (class-name (class-of ,inst)) ,class-name))

(defmacro add-comment (decl ctl &rest args)
  (let ((cdecl (gensym)))
    `(when *typechecking-module*
       (let ((,cdecl (or (car (generated ,decl))
			 ,decl)))
	 (setf (newline-comment ,cdecl)
	       (append (newline-comment ,cdecl)
		       (list (format nil "% ~@?" ,ctl ,@args))))))))

;;; Courtesy of Tim Winkler

;#+gcl
;(defmacro dotimes-fixnum (&rest body)
;  (let ((var (car (car body)))
;	(lim (cadr (car body)))
;	(res (cddr (car body)))
;	(acts (cdr body))
;	(limvar (gensym))
;	(lab (gensym)))
;    `(block ()
;	   (let* ((,limvar ,lim) (,var 0))
;	     (declare (fixnum ,var ,limvar))
;	     (tagbody
;	      ,lab
;	      (if (>= ,var ,limvar) (return (progn ,@res)))
;	      (tagbody ,@acts)
;	      (setq ,var (1+ ,var))
;	      (go ,lab)))))
;  )


;;; Redefine dotimes for GCL - it is WAY too slow otherwise.

#+gcl
(defmacro dotimes ((var form &optional (val nil)) &rest body &environment env)
  (multiple-value-bind (doc decls bod)
      (pcl::extract-declarations body env)
    (declare (ignore doc))
    (let ((limit (gensym))
          (label (gensym)))
      `(let ((,limit ,form)
             (,var 0))
         (declare (fixnum ,limit ,var))
         ,@decls
         (block nil
           (tagbody
	      ,label
              (when (>= ,var ,limit) (return-from nil ,val))
              ,@bod
              (setq ,var (the fixnum (1+ ,var)))
              (go ,label)))))))


;;; These macros speed up the making of terms, by going directly to the
;;; makes on the underlying ERGO structures.

(defmacro mk-ergo-term (op args)
  `(term::mk-default-term (oper::mk-oper :op ,op) ,args))

(defmacro mk-ergo-term* (op &rest args)
  `(term::mk-default-term (oper::mk-oper :op ,op) (list ,@args)))

;;; mk-sim-term* is like ERGO's mk-sim-term, but allows &rest args

(defmacro mk-sim-term* (s &rest args)
  `(mk-sim-term ,s (list ,@args)))

#+lucid
(defmacro ignore-file-errors (&rest body)
  `(ignore-errors
     (handler-bind ((lucid::file-protection-error
		     #'(lambda (x)
			 (declare (ignore x))
			 (invoke-restart (car (compute-restarts))))))
       ,@body)))

#+(not lucid)
(defmacro ignore-file-errors (&rest body)
  `(handler-case (progn ,@body) (file-error (condition) (values nil condition))))

(defvar *ignore-lisp-errors* t)

(defmacro ignore-lisp-errors (&rest body)
  `(if *ignore-lisp-errors*
       (ignore-errors ,@body)
       (progn ,@body)))
  

(unless (fboundp 'nth-value)
  (defmacro nth-value (n form)
    `(nth ,n (multiple-value-list ,form))))

(defmacro gen-lambda-expr (vsym vtype operator)
  (let ((type (gensym))
	(id (gensym))
	(bd (gensym))
	(nvar (gensym))
	(op (gensym))
	(lexpr (gensym)))
    `(let* ((,type ,vtype)
	    (,op ,operator)
	    (,id (make-new-variable ,vsym (cons ,type ,op)))
	    (,bd (make-bind-decl ,id ,type))
	    (,nvar (make-variable-expr ,bd))
	    (,lexpr (make!-lambda-expr (list ,bd)
		      (if (listp ,op)
			  (make!-conjunction*
			   (mapcar #'(lambda (o)
				       (make!-application o ,nvar))
			     ,op))
			  (make!-application ,op ,nvar)))))
       ;;        (unless (tc-eq ,lexpr (beta-reduce ,lexpr))
       ;; 	 (break "Why?"))
       ,lexpr)))

(defmacro gen-forall-expr (vsym vtype operator)
  (let ((type (gensym))
	(id (gensym))
	(bd (gensym))
	(nvar (gensym))
	(op (gensym)))
    `(let* ((,type ,vtype)
	    (,op ,operator)
	    (,id (make-new-variable ,vsym (cons ,type ,op)))
	    (,bd (typecheck* (mk-bind-decl ,id ,type ,type) nil nil nil))
	    (,nvar (mk-name-expr ,id nil nil (make-resolution ,bd nil ,type))))
       (beta-reduce (make-forall-expr (list ,bd)
		      (if (listp ,op)
			  (mk-conjunction
			   (mapcar #'(lambda (o)
				       (mk-application o ,nvar))
			     ,op))
			  (mk-application ,op ,nvar)))))))


(defmacro with-no-type-errors (&rest forms)
  `(let ((*type-error-catch* 'type-error))
    (catch 'type-error
      ,@forms)))

(defmacro with-no-parse-errors (&rest forms)
  `(let ((*parse-error-catch* 'parse-error))
     (catch 'parse-error
       ,@forms)))

;;; with-pvs-context is a macro that temporarily changes the context,
;;; restoring everything to the previous state on exiting.

(defmacro with-workspace (lib-ref &rest forms)
  (let ((lref (gentemp))
	(lib-path (gentemp))
	(ws (gentemp))
	(shortdir (gentemp))
	(orig-dir (gentemp)))
    `(let* ((,lref ,lib-ref)
	    (,lib-path (if (workspace-session? ,lref)
			   (path ,lref)
			   (get-library-path ,lref))))
       (if (directory-p ,lib-path)
	   (let* ((,orig-dir (working-directory))
		  (,shortdir (shortpath ,lib-path))
		  (*default-pathname-defaults* ,shortdir)
		  (*current-context* nil)
		  (*workspace-session*
		   (or (when (workspace-session? ,lref)
			 ,lref)
		       (get-workspace-session ,lib-path)
		       (let ((,ws (make-instance 'workspace-session
				    :path ,lib-path)))
			 (push ,ws *all-workspace-sessions*)
			 ,ws))))
	     (unwind-protect 
		  (prog1 (progn (set-working-directory ,shortdir)
				(unless (pvs-context *workspace-session*)
				  (restore-context))
				,@forms)
		    (when (pvs-context-changed *workspace-session*)
		      (save-context)))
	       (set-working-directory ,orig-dir)))
	   (error "Library ~a does not exist" ,lib-path)))))

(defmacro with-directory (dirstr &rest forms)
  "This has the effect of cd, by creating a context in which
*default-pathname-defaults* and (working-directory) are set to dir, making
it the default."
  (let ((dir (gentemp))
	(shortdir (gentemp))
	(orig-dir (gentemp)))
    `(let ((,dir (directory-p ,dirstr)))
       (if ,dir
	   (let* ((,orig-dir (working-directory))
		  (,shortdir (shortpath ,dir))
		  (*default-pathname-defaults* ,shortdir))
	     (unwind-protect
		  (progn (set-working-directory ,shortdir)
			 ,@forms)
	       (set-working-directory ,orig-dir)))
	   (error "Directory ~a does not exist" ,dirstr)))))

(defmacro with-pvs-file (vars pvsfileref &rest body)
  "Given a pvsfile string or pathname, gets the directory and file parts,
invokes with-workspace on the directory, and applies pvs-fn to the basename,
which is the pvsfile without directory.
Example:
  (with-pvs-file (fname thname) (show-tccs fname thname))
expands to:
  (let ((t2440 (split pvsfileref #\#))
        (fname (nth 0 t2440))
        (thname (nth 1 t2440)))
    (show-tccs fname thname))"
  (let ((args (gentemp))
	(path (gentemp))
	(dir (gentemp))
	(name (gentemp))
	(ext (gentemp))
	(msg (gentemp)))
    `(let* ((,args (split ,pvsfileref #\#))
	    (,path (car ,args))
	    (,dir (uiop:pathname-directory-pathname ,path))
	    (,name (pathname-name ,path))
	    (,ext (pathname-type ,path))
	    (,(car vars) ,name)
	    ,@(let ((cnt 0))
		(mapcar #'(lambda (v) (list v `(nth ,(incf cnt) ,args))) (cdr vars))))
       (unless (or (uiop:pathname-equal ,dir #p"")
		   (directory-p ,dir))
	 (let ((,msg (format nil "with-pvs-file error: bad directory ~a" ,dir)))
	   (pvs-error ,msg ,msg)))
       (unless (or (null ,ext) (string-equal ,ext "pvs"))
	 ;; The name could be, e.g., foo.bar, and is intended to refer to
	 ;; foo.bar.pvs.  So rather than give an error, we just treat it as
	 ;; the name, with nil type
	 (setq ,name (uiop:strcat ,name "." ,ext)))
       (cond ((or (uiop:pathname-equal ,dir #p"")
		  (uiop:pathname-equal ,dir *default-pathname-defaults*))
	      ,@body)
	     (t (with-workspace ,dir
		  ,@body))))))
	 

(defmacro add-to-alist (key entry alist)
  (let ((vkey (gentemp))
	(ventry (gentemp))
	(valist (gentemp))
	(centry (gentemp)))
    `(let* ((,vkey ,key)
	    (,ventry ,entry)
	    (,valist ,alist)
	    (,centry (assoc ,vkey ,valist)))
       (if ,centry
	   (unless (memq ,ventry ,centry)
	     (setf (cdr ,centry) (cons ,ventry (cdr ,centry))))
	   (push (list ,vkey ,ventry) ,alist)))))

(defmacro starting-row (place)
  `(svref ,place 0))

(defmacro starting-col (place)
  `(svref ,place 1))

(defmacro ending-row (place)
  `(svref ,place 2))

(defmacro ending-col (place)
  `(svref ,place 3))

(defmacro line-begin (place)
  `(when ,place (svref ,place 0)))

(defmacro col-begin (place)
  `(when ,place (svref ,place 1)))

(defmacro line-end (place)
  `(when ,place (svref ,place 2)))

(defmacro col-end (place)
  `(when ,place (svref ,place 3)))

(defmacro start-place (place)
  `(list (starting-row ,place) (starting-col ,place)))

(defmacro end-place (place)
  `(list (ending-row ,place) (ending-col ,place)))

#+gcl
(Clines
"#include <signal.h>"
"extern int interrupt_flag;"
"extern int interrupt_enable;"
"extern void (*sigint)();"
)

#+gcl
(defCfun "object enable_interrupts()" 0
" interrupt_enable = TRUE;"
" if (interrupt_flag) agcl_signal(SIGINT, sigint);"
" Creturn(Cnil);"
)

#+gcl
(defentry enable-interrupts () (object enable_interrupts))

#+gcl
(defCfun "object disable_interrupts()" 0
" interrupt_enable = FALSE;"
" Creturn(Cnil);"
)

#+gcl
(defentry disable-interrupts () (object disable_interrupts))

#+gcl
(defmacro with-interrupts-disabled (&rest body)
  `(unwind-protect
	(progn
	  (disable-interrupts)
	  ,@body)
     (enable-interrupts)))

(defmacro add-place (form place)
  (let ((obj (gentemp)))
    `(let ((,obj ,form))
       (setf (place ,obj) ,place)
       ,obj)))

(defmacro def-pvs-term (name term theory &key (nt 'expr) expected)
  (assert (symbolp name) () "NAME should be a symbol")
  (assert (stringp term) () "TERM should be a string")
  (assert (stringp theory) () "THEORY should be a string")
  (eval-when (:execute :load-toplevel)
    (let ((var (gensym))
	  (reset-name (intern (format nil "%RESET-~a" name) :pvs))
	  (hook (if (gethash (intern theory :pvs) *prelude*)
		    '*load-prelude-hook*
		    '*untypecheck-hook*)))
      `(let ((,var nil))
	 (pushnew ',reset-name ,hook)
	 (defun ,name ()
	   (or ,var
	       (let* ((ctheory (get-typechecked-theory ,theory))
		      (*current-context* (saved-context ctheory))
		      (*generate-tccs* 'none)
		      ,@(when expected
			      `((expected-type
				 (pc-typecheck (pc-parse ,expected 'type-expr))))))
		 (assert *current-context*)
		 (setq ,var (pc-typecheck (pc-parse ,term ',nt)
			      ,@(when expected '(:expected expected-type)))))))
	 (defun ,reset-name ()
	   (setq ,var nil))))))

(defun maphash-sorted (fn hashtable &optional (predicate #'string<))
  (dolist (k (sort (get-hash-keys hashtable) predicate))
    (funcall fn k (gethash k hashtable))))

(defun do-theories (fn &optional (pvs-theories (current-pvs-theories)))
  (dolist (k (sort (get-hash-keys pvs-theories) #'string-lessp))
    (funcall fn (gethash k pvs-theories))))

(defun do-all-theories (fn &optional no-prelude?)
  "Goes through all known (e.g., parsed) theories of the current context,
and all *all-workspace-sessions* applying fn to each theory."
  (do-theories fn)
  (dolist (ws *all-workspace-sessions*)
    (unless (eq ws *workspace-session*) ; Did this above
      (do-theories fn (pvs-theories ws))))
  (unless no-prelude?
    (do-theories fn *prelude*)))

(defmacro protect-types-hash (obj &rest forms)
  `(unwind-protect
	(let ((*expression-types* (if *in-typechecker*
				      *expression-types*
				      *empty-expression-types*))
	      (*in-typechecker* (or *in-typechecker*
				    (if (or *in-checker* *in-evaluator*)
					(if (syntax? ,obj)
					    (str ,obj)
					    ,obj)
					t))))
	  ,@forms)
     (unless *in-typechecker*
       (setq *empty-expression-types* (make-hash-table :test 'eq)))))

;; (defmacro with-case-insensitive-lower (&rest forms)
;;   #+(and allegro (version>= 6))
;;   `(unwind-protect
;;        (progn (excl:set-case-mode :case-insensitive-lower)
;; 	      ,@forms)
;;      (excl:set-case-mode :case-sensitive-lower))
;;   #-(and allegro (version>= 6))
;;   `(progn ,@forms))

(defmacro get-declarations (id &optional decl-hash)
  (if decl-hash
      `(get-lhash ,id ,decl-hash)
      `(get-lhash ,id (current-declarations-hash))))

(defsetf get-declarations (id &optional decl-hash) (decl)
  (let ((ghash (gensym))
	(gdecl (gensym))
	(gid (gensym)))
    `(let ((,gid ,id)
	   (,gdecl ,decl)
	   (,ghash (or ,decl-hash (current-declarations-hash))))
       (assert (typep ,gdecl '(or declaration inline-recursive-type))
	       () "setf get-declarations: not a declaration")
       (assert (eq (id ,gdecl) ,gid) () "setf get-declarations: id mismatch")
       (pushnew ,decl (get-lhash ,id ,ghash) :test #'eq))))

(defmacro put-decl (decl &optional decl-hash)
  (let ((gdecl (gensym)))
    (if decl-hash
	`(let ((,gdecl ,decl))
	   (setf (get-declarations (id ,gdecl) ,decl-hash) ,gdecl))
	`(let ((,gdecl ,decl))
	   (setf (get-declarations (id ,gdecl) (current-declarations-hash))
		 ,gdecl)))))

;;; This is mostly used for decl-formals needed while building declarations,
;;; e.g., for TCCs and datatypes involving decl-formals.  It temporarily
;;; adds the new formals to the declarations hash table, removing any
;;; conflicting ones, and restoring the declarations hash to its former
;;; state at the end.

(defmacro with-added-decls (decls &rest body)
  (let ((gdecls (gensym))
	;;(gdecl (gensym))
	;;(assoc-decls (gensym))
	;;(dfmls (gensym))
	)
    `(let ((,gdecls ,decls))
       ;; We want to push new decl-formals into the current-declarations-hash
       ;; But there may already be some there (recursively)
       ;; We remove any clashing ones, and restore after executing body
       (if (null ,gdecls)
	   (progn ,@body)
	   (unwind-protect
		;; Might have a problem here, if there are decl-formals
		;; already in the declarations hash
		(progn (add-decl-formals-to-declarations-hash ,gdecls)
		       ,@body)
	     (remove-decl-formals-from-declarations-hash ,gdecls))))))

(defun remove-decl-formals-from-declarations-hash (dfmls)
  "Tries to remove the dfmls from the declarations-hash, returining the list
of those actually removed."
  (let ((remdecls nil))
    (dolist (dfml dfmls)
      ;; Don't use get-declarations - it appends the lhash values
      ;; Just take the lhash-table
      (let* ((ht (lhash-table (current-declarations-hash)))
	     (assoc-decls (gethash (id dfml) ht)))
	(when (memq dfml assoc-decls)
	  (setf (gethash (id dfml) ht) (remove dfml assoc-decls))
	  (push dfml remdecls))
	(assert (not (memq dfml (get-declarations (id dfml)))))))
    remdecls))

(defun add-decl-formals-to-declarations-hash (dfmls)
  (dolist (dfml dfmls)
    (let* ((ht (lhash-table (current-declarations-hash)))
	   (assoc-decls (gethash (id dfml) ht)))
      #+badassert (assert (not (memq dfml assoc-decls)))
      (unless (memq dfml assoc-decls)
	(setf (gethash (id dfml) ht) (cons dfml assoc-decls))))))

(defvar *default-context*)

(defmacro with-default-context (&rest body)
  (let ((theory (gensym))
	(id (gensym)))
    `(progn
       (unless (boundp '*default-context*)
	 (let* ((,id (gentemp "default_theory"))
		(,theory (pc-parse "default_theory: theory begin default_t: type end default_theory"
			   'adt-or-theory)))
	   (typecheck ,theory)
	   (setq *default-context* (make-new-context ,theory))
	   (setf (declaration *default-context*) (car (theory ,theory)))))
       (let ((*current-context* *default-context*)
	     (*generate-tccs* 'all))
	 ,@body))))

(defmacro with-context (obj &rest body)
  "The macro with-context executes the body in the specified context.
obj may be of type:
  declaration: must have :module set to a theory, in which it appears
               the resuting context is just before the declaration
  string: parsed as a name, which see below
          e.g., \"finite_sets[int].is_finite\"
  name: tries to interpret the name as a unique declaration or theory
        setting context accordingly
  theory: (aka module) last declaration (inclusive) of the theory is
          the context
  recursive-type: treated as a declaration if inline
                  otherwise uses the main generated theory
  importing: same as a declaration
  :prelude uses the prelude context."
  (let ((eobj (gentemp)))
    `(let* ((,eobj ,obj)
	    (*current-context* (if (eq ,eobj :prelude)
				  *prelude-context* (context ,eobj)))
	    (*generate-tccs* 'all))
       ,@body)))

(defmacro with-current-theory (theory &rest body)
  (let ((cth (gensym)))
    `(let ((,cth (current-theory)))
       (unwind-protect
	    (progn (setf (current-theory) ,theory)
		   ,@body)
	 (setf (current-theory) ,cth)))))

(defvar *current-declaration-stack* nil)

(defmacro with-current-decl (decl &rest body)
  (let ((cdecl (gensym))
	(gdecl (gensym))
	(remdecls (gensym)))
    `(let* ((,cdecl (current-declaration))
	    (*current-declaration-stack* (cons ,cdecl *current-declaration-stack*))
	    (*current-top-declaration* (or *current-top-declaration* ,cdecl))
	    (,gdecl ,decl)
	    (,remdecls nil))
       (unwind-protect
	    (progn (setf (current-declaration) ,gdecl)
		   (when ,cdecl
		     (setq ,remdecls
			   (remove-decl-formals-from-declarations-hash (decl-formals ,cdecl))))
		   (add-decl-formals-to-declarations-hash (decl-formals ,gdecl))
		   ,@body)
	 (remove-decl-formals-from-declarations-hash (decl-formals ,gdecl))
	 (setf (current-declaration) ,cdecl)
	 (when ,cdecl
	   (add-decl-formals-to-declarations-hash ,remdecls))))))

(defmacro with-bound-declparams (decls &rest body)
  (let ((gdecls (gensym)))
    `(let ((,gdecls ,decls))
       (with-added-decls ,gdecls
	 (let ((*decl-bound-parameters* ,gdecls))
	   ,@body)))))

;; Only used by add-decl, destructively modifies the context
(defun delete-declaration (decl &optional decl-hash)
  (assert (or decl-hash *current-context*))
  (let* ((lht (or decl-hash (current-declarations-hash)))
	 (ht (lhash-table lht)))
    (when (hash-table-p ht)
      (let* ((decls (gethash (id decl) ht)))
	(when (memq decl decls)
	  (if (cdr decls)
	      (setf (gethash (id decl) ht) (delete decl decls))
	      (remhash (id decl) ht)))))
    (when (lhash-next lht)
      (delete-declaration decl (lhash-next lht)))))

(defun do-all-declarations (function &optional decl-hash)
  (assert (or decl-hash *current-context*))
  (let ((lht (or decl-hash (current-declarations-hash))))
    (map-lhash #'(lambda (id decls)
		   (declare (ignore id))
		   (mapc function decls))
	       lht)))

(defmacro get-importings (theory &optional using-hash)
  (if using-hash
      `(get-lhash ,theory ,using-hash)
      `(get-lhash ,theory (current-using-hash))))

(defsetf get-importings (theory &optional using-hash) (using)
  (if using-hash
      `(setf (get-lhash ,theory ,using-hash) ,using)
      `(setf (get-lhash ,theory (current-using-hash)) ,using)))

(defmacro put-importing (inst theory &optional using-hash)
  (if using-hash
      `(pushnew ,inst (get-importings ,theory ,using-hash))
      `(pushnew ,inst (get-importings ,theory (current-using-hash)))))

(defmacro ensure-trailing-slash (dirstring)
  (let ((dstring (gentemp)))
    `(let ((,dstring ,dirstring))
       (assert (stringp ,dstring))
       (if (or (string= ,dstring "")
	       (char= (char ,dstring (1- (length ,dstring))) #\/))
	   ,dstring
	   (concatenate 'string ,dirstring "/")))))

;;; Taken from http://dunsmor.com/lisp/onlisp/onlisp_22.html
(defmacro dbind (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(dbind-ex (destruc pat gseq #'atom) body))))

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (when pat
    (let ((rest (cond ((funcall atom? pat) pat)
		      ((eq (car pat) '&rest) (cadr pat))
		      ((eq (car pat) '&body) (cadr pat))
		      (t nil))))
      (if rest
	  `((,rest (subseq ,seq ,n)))
	  (let ((p (car pat))
		(rec (destruc (cdr pat) seq atom? (1+ n))))
	    (if (funcall atom? p)
		(cons `(,p (elt ,seq ,n))
		      rec)
		(let ((var (gensym)))
		  (cons (cons `(,var (elt ,seq ,n))
			      (destruc p var atom?))
			rec))))))))

(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
			 (if (consp (car b))
			     (car b)
			     b))
		     binds)
	 ,(dbind-ex (mapcan #'(lambda (b)
				(if (consp (car b))
				    (cdr b)))
		      binds)
		    body))))

(defmacro update-alist (akey value alist &key overwrite test test-not key)
  "A common pattern implemented by this is:
    (let ((entry (assoc akey alist)))
      (if entry
          (setf (cdr entry) value)
          (acons akey value alist)))"
  (let ((%elt (gentemp))
	(%akey (gentemp))
	(%alist (gentemp)))
    (format t "~%test: ~a" test)
    `(let* ((,%alist ,alist)
	    (,%akey ,akey)
	    (,%elt (assoc ,%akey ,%alist
			  ,@(when test (list :test test))
			  ,@(when test-not (list :test-not test-not))
			  ,@(when key (list :key key)))))
       (if ,%elt
	   (progn (when ,overwrite
		    (setf (cdr ,%elt) ,value))
		  ,%alist)
	   (acons ,%akey ,value ,%alist)))))

;;; Finds the item in list using test, applies fn to item and the found
;;; element.  If it is not nil, returns the item and the fn
;;; result. Otherwise it goes on to the next item.  Returns nil if no item
;;; satisfies both.  Just returning the fn result would not say which item
;;; led to it, and returning just the item would mean needing to call fn
;;; twice.
(defmacro find-and-apply (item fn list &key (test #'eq) key)
  (let ((item_ (gentemp))
	(key_ (gentemp)))
    `(let ((,item_ ,item)
	   (,key_ ,key))
       (dolist (elt ,list)
	 (when (funcall ,test ,item_ (if ,key_ (funcall ,key_ elt) elt))
	   (let ((result (funcall ,fn ,item_ elt)))
	     (when result
	       (return (values elt result)))))))))

(defmacro real-timer (str &rest body)
  (let ((%start (gentemp))
	(%result (gentemp)))
    `(let ((,%start (get-internal-real-time))
	   (,%result (progn ,@body)))
       (format t "~%~a: ~d"
	 ,str (- (get-internal-real-time) ,%start))
       ,%result)))
	
(defmacro cam (form)
  `(compute-applicable-methods (function ,(car form)) (list ,@(cdr form))))
