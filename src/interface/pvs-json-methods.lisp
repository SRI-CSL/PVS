(in-package :pvs-json-rpc)

;;; The requests

(defrequest list-methods ()
  "List all available methods"
  (sort (mapcar #'car *pvs-request-methods*) #'string-lessp))

(defrequest help (methodname)
  "Get help for the specified methodname -
   provides the docstring and the argument spec"
  (multiple-value-bind (reqfun argspec docstring)
      (get-json-request-function methodname)
    (if entry
	`((:argspec ,argspec)
	  (:docstring . ,docstring))
	(error "~a not found" methodname))))

(defrequest lisp (string)
  "Just evaluate the string in lisp"
  (let ((*package* (find-package :pvs)))
    (eval (read-from-string string))))

(defrequest change-context (dir)
  "Change PVS context"
  (pvs:change-context dir))

(defrequest typecheck (filename &optional forced? prove-tccs? importchain? nomsg?)
  "Typecheck a file"
  (let ((theories (pvs:typecheck-file filename)))
    (xmlrpc-theories theories)))

(defun jsonrpc-pvs-typecheck-file (filename &optional optargs)
  ;; filename is a string
  ;; optargs is a struct of form
  ;; {"forced?" :bool "prove-tccs?" :bool "importchain?" :bool "nomsg?" :bool}
  (declare (ignore optargs))
  (let ((theories (pvs:typecheck-file filename)))
    (xmlrpc-theories theories)))

(defun xmlrpc-theories (theories &optional thdecls)
  (if (null theories)
      (nreverse thdecls)
      (let ((thdecl (xmlrpc-theory (car theories))))
	(xmlrpc-theories (cdr theories) (cons thdecl thdecls)))))

(defun xmlrpc-theory (theory)
  `(:theory . ((:id . ,(pvs:id theory))
	       (:decls . ,(xmlrpc-theory-decls (pvs:all-decls theory))))))

(defun xmlrpc-theory-decls (decls &optional thdecls)
  (if (null decls)
      (nreverse thdecls)
      (let ((thdecl (xmlrpc-theory-decl (car decls))))
	(xmlrpc-theory-decls
	 (cdr decls)
	 (if thdecl
	     (cons thdecl thdecls)
	     thdecls)))))

(defun xmlrpc-theory-decl (decl)
  (xmlrpc-theory-decl* decl))

(defmethod xmlrpc-theory-decl* ((decl pvs:var-decl))
  nil)

(defmethod xmlrpc-theory-decl* ((imp pvs:importing))
  `((:importing . ,(pvs:str (pvs:theory-name imp)))
    (:kind . :importing)
    (:place . ,(pvs:place-list imp))))

(defmethod xmlrpc-theory-decl* ((decl pvs:typed-declaration))
  `((:id . ,(pvs:id decl))
    (:kind . ,(pvs:kind-of decl))
    (:type . ,(pvs:str (pvs:type decl)))
    (:place . ,(pvs:place-list decl))))

(defmethod xmlrpc-theory-decl* ((decl pvs:formula-decl))
  `((:id . ,(pvs:id decl))
    (:kind . :formula)
    (:place . ,(pvs:place-list decl))))

;;; Prover interface

(defrequest prove-formula (formula theory)
  "Starts interactive proof of a formula from a given theory"
  (let* ((gate (mp:make-gate nil))
	 (lock (mp:make-process-lock)))
    (setq pvs:*proofstate-info*
	  (pvs:make-psinfo
	   :lock lock
	   :gate gate
	   :process (mp:process-run-function
		     "PVS Prover"
		     #'pvs:prove-formula theory formula nil)))
    (format t "~%prove-formula: Waiting...~%")
    (mp:process-wait "Waiting for initial Proofstate" #'mp:gate-open-p gate)
    (format t "~%prove-formula: Done waiting...~%")
    (mp:with-process-lock (lock)
      (let ((psjson (psinfo-psjson *proofstate-info*)))
	(setf (psinfo-psjson *proofstate-info*) nil)
	psjson))))

(defrequest proof-command (form)
  "Sends a command to the prover"
  (format t "~a" form)
  (mp:process-wait "Waiting for next Proofstate"
		   #'mp:gate-open-p (psinfo-gate *proofstate-info*))
  (mp:with-process-lock ((psinfo-lock *proofstate-info*))
      (let ((psjson (psinfo-psjson *proofstate-info*)))
	(setf (psinfo-psjson *proofstate-info*) nil)
	psjson)))
  
